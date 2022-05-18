
{$IFDEF UNIX}
type
  hostent = record
    h_name: PChar;
    h_aliases: PPChar;
    h_addrtype: cInt;
    h_length: cInt;
    h_addr_list: PPChar;
  end;
  THostEnt = hostent;
  PHostEnt = ^THostEnt;

  function gethostbyname2(name: PChar; af: integer): PHostEnt; cdecl; external 'c' name 'gethostbyname2';
{$ENDIF}

{$IFNDEF POSIX}
type
  PAddrInfo = ^addrinfo;
  PPAddrInfo = ^PAddrInfo;
  addrinfo = record
    ai_flags: Integer;
    ai_family: Integer;
    ai_socktype: Integer;
    ai_protocol: Integer;
    ai_addrlen: NativeUInt;
    ai_canonname: PAnsiChar;
    ai_addr: PSockAddr;
    ai_next: PAddrInfo;
  end;
  {$EXTERNALSYM addrinfo}
{$ENDIF}

{$IFDEF MSWINDOWS}
const
  SIO_KEEPALIVE_VALS = $98000004;
  SIO_GET_INTERFACE_LIST = $4004747F;
  AF_INET6 = 23;

type
  in6_addr = record
    case Integer of
      0: (s6_addr: array[0..15] of u_char);
      1: (s6_addr16: array[0..7] of u_short);
      2: (s6_addr32: array[0..3] of UInt32);
  end;
  {$EXTERNALSYM IN6_ADDR}
  TIn6Addr = in6_addr;
  {$EXTERNALSYM TIn6Addr}
  PIn6Addr = ^in6_addr;

  sockaddr_in6 = record
    sin6_family   : Smallint;         // AF_INET6
    sin6_port     : u_short;          // Transport level port number
    sin6_flowinfo : u_long;           // IPv6 flow information
    sin6_addr     : TIn6Addr;         // IPv6 address
    sin6_scope_id : u_long;           // set of interfaces for a scope
  end;
  {$EXTERNALSYM SOCKADDR_IN6}
  TSockAddrIn6 = sockaddr_in6;
  {$EXTERNALSYM TSockAddrIn6}
  PSockAddrIn6 = ^sockaddr_in6;
  PSockAddr_In6 = ^sockaddr_in6;

  sockaddr_gen = packed record
    AddressIn: sockaddr_in;
    filler1: Cardinal;
    filler2: Cardinal;
  end;
  {$EXTERNALSYM sockaddr_gen}

  INTERFACE_INFO = packed record
    iiFlags: Cardinal;
    iiAddress: sockaddr_gen;
    iiBroadcastAddress: sockaddr_gen;
    iiNetmask: sockaddr_gen;
  end;
  {$EXTERNALSYM INTERFACE_INFO}
  PINTERFACE_INFO = ^INTERFACE_INFO;

  TTcpKeepAlive = record
    OnOff: Cardinal;
    KeepAliveTime: Cardinal;
    KeepAliveInterval: Cardinal;
  end;

function Startup: integer;
begin
  hLockWsaData.Enter;
  try
    Result := 0;
    if WsaData.wVersion = 0 then // not initialized
      Result := WSAStartup($0202, WsaData);
  finally
    hLockWsaData.Leave;
  end;
end;

procedure Cleanup;
begin
  hLockWsaData.Enter;
  try
    if WsaData.wVersion <> 0 then
      WSACleanup;
  finally
    hLockWsaData.Leave;
  end;
end;

type
  TWSAIoctl = function(s: Integer; cmd: Cardinal; lpInBuffer: IntPtr; dwInBufferLen: Cardinal;
    lpOutBuffer: IntPtr; dwOutBufferLen: Cardinal; var lpdwOutBytesReturned: Cardinal;
    lpOverLapped: IntPtr; lpOverLappedRoutine: IntPtr): Integer; stdcall;
  {$EXTERNALSYM TWSAIoctl}
  TGetAddrInfo = function(NodeName: PAnsiChar; ServiceName: PAnsiChar; Hints: PAddrInfo;
    pResult: PPAddrInfo): Integer; stdcall;
  {$EXTERNALSYM TGetAddrInfo}
  TFreeAddrInfo = procedure(iAI: PAddrInfo); stdcall;
  {$EXTERNALSYM TFreeAddrInfo}

var
  WSAIoctl: TWSAIoctl;
  {$EXTERNALSYM WSAIoctl}
  GetAddrInfo: TGetAddrInfo;
  {$EXTERNALSYM GetAddrInfo}
  FreeAddrInfo: TFreeAddrInfo;
  {$EXTERNALSYM FreeAddrInfo}

const
  WS2_32 = 'ws2_32.dll';
var
  hWS2_32Lib: HMODULE = 0;

function NotLink: integer;
begin
  raise Exception.Create('Function is not linked');
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
end;

function IsAddrInfoSupported: Boolean;
begin
  Result := @GetAddrInfo <> @NotLink;
end;

function ApiGetAddrInfo(NodeName: PAnsiChar; Hints: AddrInfo; out pResult: PAddrInfo): Integer;
var
  ip_addr: Cardinal;
  hp: PHostEnt;
begin
  ip_addr := Cardinal(inet_addr(NodeName));
  if ip_addr = Cardinal(INADDR_NONE) then begin
    hp := gethostbyname(NodeName);
    if hp <> nil then
      ip_addr := PCardinal(hp.h_addr^)^;
  end;
  if ip_addr = Cardinal(INADDR_NONE) then begin
    Result := WSAGetLastError;
    pResult := nil;
  end
  else begin
    Result := 0;
    pResult := Marshal.AllocHGlobal(sizeof(AddrInfo));
    System.FillChar(pResult^, sizeof(AddrInfo), 0);
    pResult.ai_socktype := Hints.ai_socktype;
    pResult.ai_protocol := Hints.ai_protocol;
    pResult.ai_family := Hints.ai_family;
    pResult.ai_addrlen := sizeof(TSockAddrIn);
    pResult.ai_addr := Marshal.AllocHGlobal(pResult.ai_addrlen);
    PSockAddrIn(pResult.ai_addr).sin_family := Hints.ai_family;
    PSockAddrIn(pResult.ai_addr).sin_addr.s_addr := Integer(ip_addr);
  end;
end;

procedure ApiFreeAddrInfo(iAI: PAddrInfo);
begin
  Marshal.FreeHGlobal(iAI.ai_addr);
  Marshal.FreeHGlobal(iAI);
end;

procedure LoadWS2_32Lib;

  function GetProc(hLib: HMODULE; Name: string): FARPROC;
  begin
    Result := GetProcAddress(hLib, PChar(Name));
    if Result = nil then
      Result := @NotLink;
  end;

begin
  hLockWsaData.Enter;
  try
    if hWS2_32Lib > 0 then
      Exit;

    hWS2_32Lib := LoadLibrary(PChar(WS2_32));
    if hWS2_32Lib > 0 then begin
      WSAIoctl := GetProc(hWS2_32Lib, 'WSAIoctl');
      GetAddrInfo := GetProc(hWS2_32Lib, 'getaddrinfo');
      FreeAddrInfo := GetProc(hWS2_32Lib, 'freeaddrinfo');
    end
    else
      raise Exception.Create('Cannot load ' + WS2_32);
  finally
    hLockWsaData.Leave;
  end;
end;

procedure FreeWS2_32Lib;
begin
  hLockWsaData.Enter;
  try
    if hWS2_32Lib > 0 then begin
      FreeLibrary(hWS2_32Lib);
      hWS2_32Lib := 0;
    end;
  finally
    hLockWsaData.Leave;
  end;
end;
{$ENDIF MSWINDOWS}

function GetSocketError: integer;
begin
{$IFDEF MSWINDOWS}
  Result := WSAGetLastError;
{$ELSE}
  Result := errno;
{$ENDIF}
end;

{$IFDEF POSIX}
// for calling "setsockopt" with the same parameters for Windows and POSIX
function setsockopt(socket, level, option_name: Integer; option_value: IntPtr; option_len: socklen_t): Integer;
var
  temp: integer;
begin
  if option_value <> nil then
    Result := Posix.SysSocket.setsockopt(socket, level, option_name, option_value^, option_len)
  else begin
    temp := 0;
    Result := Posix.SysSocket.setsockopt(socket, level, option_name, temp, 0);
  end;
end;

function getsockopt(socket, level, option_name: Integer; option_value: IntPtr; var option_len: socklen_t): Integer;
var
  temp: integer;
begin
  if option_value <> nil then
    Result := Posix.SysSocket.getsockopt(socket, level, option_name, option_value^, option_len)
  else begin
    temp := 0;
    option_len := 0;
    Result := Posix.SysSocket.getsockopt(socket, level, option_name, temp, option_len);
  end;
end;
{$ENDIF POSIX}

{$IFDEF UNIX}
{$IFDEF DARWIN}
const
  SO_NOSIGPIPE = $1022;
  {$EXTERNALSYM SO_NOSIGPIPE}
{$ENDIF}

function inet_addr(cp: PAnsiChar): Cardinal;
  function StrToHostAddr(IP: AnsiString): in_addr;
  type
    array4int = array[1..4] of byte;
  var
    Dummy: AnsiString;
    I,j,k: Integer;
    Temp: in_addr;
  begin
    Result.s_addr := Cardinal(INADDR_NONE);
    for I := 1 to 4 do begin
      if I < 4 then begin
        J := Pos('.',IP);
        if J = 0 then
          exit;
        Dummy := Copy(IP,1,J-1);
        Delete(IP,1,J);
      end
      else
        Dummy := IP;
      Val(Dummy,k,J);
      array4int(Temp.s_addr)[i] := k;
      if J <> 0 then Exit;
    end;
    Result.s_addr := Temp.s_addr; //ntohl(Temp.s_addr);
  end;

begin
  Result := StrToHostAddr(cp).s_addr;
end;

function inet_ntoa(Entry: in_addr): AnsiString;
begin
  Result := NetAddrToStr(Entry);
end;

function getsockopt(socket, level, option_name: Integer; option_value: IntPtr; var option_len: socklen_t): Integer;
begin
  Result := fpgetsockopt(socket, level, option_name, option_value, @option_len);
end;

function setsockopt(socket, level, option_name: Integer; option_value: IntPtr; option_len: socklen_t): Integer;
begin
  Result := fpsetsockopt(socket, level, option_name, option_value, option_len);
end;

function fcntl(socket: Integer; cmd: Integer): Integer;
begin
  Result := fpfcntl(socket, cmd);
end;

function shutdown(s: TSocket; how: Integer): Integer;
begin
  Result := fpshutdown(s, how);
end;

function recv(s: TSocket; var Buf; len, flags: Integer): Integer;
begin
  Result := fprecv(s, @Buf, len, flags);
end;

function send(s: TSocket; const Buf; len, flags: Integer): Integer;
begin
  Result := fpsend(s, @Buf, len, flags {$IFNDEF DARWIN} or MSG_NOSIGNAL{$ENDIF});
end;

function select(N: cint; readfds, writefds, exceptfds: pfdset; TimeOut: PTimeVal): cint;
begin
  Result := fpselect(N, readfds, writefds, exceptfds, TimeOut);
end;

function getaddrinfo(Hostname, Servname: PAnsiChar; const Hints: AddrInfo; out pResult: PAddrInfo): Integer;
var
  phe: PHostEnt;
  in6: Tin6_addr;
  in4: TIn_addr;
begin
  Result := 0;

  if Hints.ai_family <> AF_INET then begin // = AF_INET6 or AF_UNSPEC
    in6 := StrToHostAddr6(Hostname);
    if CompareMem(@in6, @NoAddress6, SizeOf(in6)) then begin
      phe := gethostbyname2(Hostname, AF_INET6);
      if not Assigned(phe) then
        Result := -1
      else
        Move(phe.h_addr_list[0]^, in6.u6_addr16[0], phe.h_length);
    end;
    if Result = 0 then begin
      GetMem(pResult, sizeof(AddrInfo));
      System.FillChar(pResult^, sizeof(AddrInfo), 0);
      pResult.ai_socktype := Hints.ai_socktype;
      pResult.ai_protocol := Hints.ai_protocol;
      pResult.ai_family := AF_INET6;
      pResult.ai_addrlen := sizeof(TInetSockAddr6);
      GetMem(pResult.ai_addr, pResult.ai_addrlen);
      PInetSockAddr6(pResult.ai_addr).sin6_family := AF_INET6;
      PInetSockAddr6(pResult.ai_addr).sin6_addr.u6_addr16 := in6.u6_addr16;
      Exit;
    end;
  end;

  if Hints.ai_family <> AF_INET6 then begin  // = AF_INET or AF_UNSPEC
    Result := 0;
    in4.s_addr := inet_addr(Hostname);
    if DWORD(in4.s_addr) = DWORD(INADDR_NONE) then begin
      phe := gethostbyname2(Hostname, AF_INET);
      if not Assigned(phe) then
        Result := -1
      else
        in4.s_addr := PCardinal(phe^.h_addr_list[0])^;
    end;
    if Result = 0 then begin
      GetMem(pResult, sizeof(AddrInfo));
      System.FillChar(pResult^, sizeof(AddrInfo), 0);
      pResult.ai_socktype := Hints.ai_socktype;
      pResult.ai_protocol := Hints.ai_protocol;
      pResult.ai_family := AF_INET;
      pResult.ai_addrlen := sizeof(TInetSockAddr);
      GetMem(pResult.ai_addr, pResult.ai_addrlen);
      PInetSockAddr(pResult.ai_addr).sin_family := AF_INET;
      PInetSockAddr(pResult.ai_addr).sin_addr.s_addr := in4.s_addr;
      Exit;
    end;
  end;
end;

procedure freeaddrinfo(iAI: PAddrInfo);
begin
  if iAI.ai_addr <> nil then
    FreeMem(iAI.ai_addr);
  FreeMem(iAI);
end;
{$ENDIF UNIX}

function IsAddressIPv6(const HostName: string): boolean;
var
  i: Integer;
  ch: Char;
  HasSeparator: boolean;
begin
  Result := False;

  if HostName = '' then
    Exit;

  HasSeparator := False;
  for i := 1 to Length(HostName) do begin
    ch := HostName[i];
    if CharInSet(ch, ['0'..'9']) or
       CharInSet(ch, ['a'..'z']) or
       CharInSet(ch, ['A'..'Z']) or
       (ch = '%') or (ch = '/')
    then
      continue
    else if ch = ':' then
      HasSeparator := True
    else
      Exit;
  end;

  Result := HasSeparator;
end;

function GetHostAddrInfo(const HostName: string; const IPVersion: TIPVersion; out HostAddrInfo: PAddrInfo): integer;
var
  Hints: AddrInfo;
begin
  HostAddrInfo := nil;

  System.FillChar(Hints, SizeOf(Hints), 0);
  if IPVersion = ivIPv4 then
    Hints.ai_family := AF_INET
  else if IPVersion = ivIPv6 then
  {$IFDEF MSWINDOWS}
    // windows cannot resolve DNS name in IPv6 network with family = AF_INET6
    if not IsAddressIPv6(HostName) then
      Hints.ai_family := AF_UNSPEC
    else
  {$ENDIF}
      Hints.ai_family := AF_INET6
  else
    Hints.ai_family := AF_UNSPEC;
  Hints.ai_socktype := SOCK_STREAM;
  Hints.ai_protocol := IPPROTO_TCP;

{$IFDEF MSWINDOWS}
  LoadWS2_32Lib; // load GetAddrInfo function
  if not IsAddrInfoSupported then
    Result := ApiGetAddrInfo(PAnsiChar(AnsiString(Hostname)), Hints, HostAddrInfo)
  else
{$ENDIF}
    Result := getaddrinfo(PAnsiChar(AnsiString(Hostname)), nil, {$IFDEF MSWINDOWS}@{$ENDIF}Hints, {$IFDEF MSWINDOWS}@{$ENDIF}HostAddrInfo);
end;

procedure Set_SendTimeout(Sd: NativeInt; Value: integer);
var
{$IFDEF MSWINDOWS}
  i: Int64;
{$ELSE}
  tv: timeval;
{$ENDIF}
begin
  if (Sd <> SOCKET_ERROR) and (Value > 0) then begin
  {$IFDEF MSWINDOWS}
    i := Int64(Value) * 1000;
    Win32Check(setsockopt(Sd, SOL_SOCKET, SO_SNDTIMEO, @i, sizeof(i)) = 0);
  {$ELSE}
    tv.tv_sec := Value;
    tv.tv_usec := 0;
    setsockopt(Sd, SOL_SOCKET, SO_SNDTIMEO, @tv, sizeof(tv));
  {$ENDIF}
  end;
end;

procedure Set_ReceiveTimeout(Sd: NativeInt; Value: integer);
var
{$IFDEF MSWINDOWS}
  i: Int64;
{$ELSE}
  tv: timeval;
{$ENDIF}
begin
  if Sd <> SOCKET_ERROR then begin
  {$IFDEF MSWINDOWS}
    i := Int64(Value) * 1000;
    //ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options_for_windows_nt_and_windows_95_98_me_2.htm
    Win32Check(setsockopt(Sd, SOL_SOCKET, SO_RCVTIMEO, @i, sizeof(i)) = 0);
  {$ELSE}
    tv.tv_sec  := Value;
    tv.tv_usec := 0;
    setsockopt(Sd, SOL_SOCKET, SO_RCVTIMEO, @tv, sizeof(tv));
  {$ENDIF}
  end;
end;

procedure Set_SendBuffer(Sd: NativeInt; Value: integer);
begin
  if (Sd <> SOCKET_ERROR) and (Value > 0) then begin
  {$IFDEF MSWINDOWS}
    Win32Check(setsockopt(Sd, SOL_SOCKET, SO_SNDBUF, @Value, sizeof(Value)) = 0);
  {$ELSE}
    setsockopt(Sd, SOL_SOCKET, SO_SNDBUF, @Value, sizeof(Value));
  {$ENDIF}
  end;
end;

procedure Set_ReceiveBuffer(Sd: NativeInt; Value: integer);
begin
  if Sd <> SOCKET_ERROR then begin
  {$IFDEF MSWINDOWS}
    Win32Check(setsockopt(Sd, SOL_SOCKET, SO_RCVBUF, @Value, sizeof(Value)) = 0);
  {$ELSE}
    setsockopt(Sd, SOL_SOCKET, SO_RCVBUF, @Value, sizeof(Value));
  {$ENDIF}
  end;
end;

function SocketFastsend(Sd: NativeInt): Integer;
var
  nodelay: Integer;
begin
  Result := 0;
  if setsockopt(Sd, IPPROTO_IP, IP_TOS, nil, 0) <> 0 then begin
  {$IFDEF POSIX}
    SetLastError(0);
  {$ENDIF}
    nodelay := 1;
    Result := setsockopt(Sd, IPPROTO_TCP, TCP_NODELAY, @nodelay, sizeof(nodelay));
  end;
end;

function SocketKeepalive(Sd: NativeInt; onoff: boolean): Integer;
var
  opt: Integer;
{$IFDEF MSWINDOWS}
  TcpKeepAlive: TTcpKeepAlive;
  UnUsed: Cardinal;
{$ENDIF}
begin
  if onoff then
    opt := 1
  else
    opt := 0;

{$IFDEF MSWINDOWS}
  TcpKeepAlive.OnOff := opt;
  TcpKeepAlive.KeepAliveTime := 15000;
  TcpKeepAlive.KeepAliveInterval := 1000;

  LoadWS2_32Lib; // load WSAIoctl function
  Result := WSAIoctl(Sd, SIO_KEEPALIVE_VALS, @TcpKeepAlive, sizeof(TcpKeepAlive), nil, 0, UnUsed, nil, nil);
{$ELSE}
  Result := setsockopt(Sd, SOL_SOCKET, SO_KEEPALIVE, @opt, sizeof(opt));
{$ENDIF}
end;

function SocketBroadcast(Sd: NativeInt; onoff: boolean): Integer;
var
  opt: Integer;
begin
  if onoff then
    opt := 1
  else
    opt := 0;

  Result := setsockopt(Sd, SOL_SOCKET, SO_BROADCAST, @opt, sizeof(opt));
end;

function SocketCreate(family, socktype, protocol: Integer): NativeInt;
{$IFDEF MACOS}
var
  opt: Integer;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result := fpsocket(family, socktype, protocol);
{$ELSE}
  Result := {$IFDEF POSIX}Posix.SysSocket.{$ELSE}WinSock.{$ENDIF}socket(family, socktype, protocol);
{$ENDIF}
  if Result = SOCKET_ERROR then
    exit;

{$IFDEF MSWINDOWS}
  // arg:=0;
  // ioctlsocket(Result, FIONBIO, Integer(arg));
{$ELSE}
  fcntl(Result, F_GETFL);
{$IFDEF MACOS}
  opt := 1;
  setsockopt(Result, SOL_SOCKET, SO_NOSIGPIPE, @opt, sizeof(opt));
{$ENDIF}
{$ENDIF}
end;

procedure SocketShutdown(Sd: NativeInt; how: Integer);
begin
  if Sd <> SOCKET_ERROR then
    shutdown(Sd, how);
end;

procedure SocketClose(Sd: NativeInt);
begin
  if Sd <> SOCKET_ERROR then begin
  {$IFDEF MSWINDOWS}
    WinSock.closesocket(Sd);
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.UniStd.__close(Sd);
  {$ENDIF}
  {$IFDEF UNIX}
    Sockets.closesocket(Sd);
  {$ENDIF}
  end;
end;

function SocketWaitForData(Sd: NativeInt; MicroSeconds: integer; out ErrorCode: integer): boolean;
var
{$IFDEF UNIX}
  fdSet: TFDSet;
{$ELSE}
  {$IFDEF POSIX}
  fdSet: fd_set;
  {$ELSE}
  fdSet: TFDSet;
  {$ENDIF}
{$ENDIF}
{$IFDEF UNIX}
  Time: timeval;
  pTime: PTimeVal;
{$ELSE}
  Time: timeval;
  pTime: PTimeVal;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  FD_ZERO(fdSet);
  FD_SET(Sd, fdSet);
{$ENDIF}
{$IFDEF POSIX}
  __FD_ZERO(fdSet);
  __FD_SET(Sd, fdSet);
{$ENDIF}
{$IFDEF UNIX}
  fpFD_ZERO(fdSet);
  fpFD_SET(Sd, fdSet);
{$ENDIF}

{$IFDEF UNIX}
  if MicroSeconds < 0 then
    pTime := nil
  else begin
    Time.tv_sec := MicroSeconds div 1000;
    Time.tv_usec := (MicroSeconds mod 1000) * 1000;
    pTime := @Time;
  end;
{$ELSE}
  if MicroSeconds < 0 then
    pTime := nil
  else begin
    Time.tv_sec := MicroSeconds div 1000;
    Time.tv_usec := (MicroSeconds mod 1000) * 1000;
    pTime := @Time;
  end;
{$ENDIF}

{$IFDEF UNIX}
  if fpselect(Sd + 1, @fdSet, nil, nil, pTime) <= 0 then
{$ELSE}
  if {$IFDEF POSIX}Posix.SysSelect.{$ELSE}WinSock.{$ENDIF}select(Sd + 1, @fdSet, nil, nil, pTime) < 0 then
{$ENDIF}
  begin
    ErrorCode := GetSocketError;
    Result := False;
    Exit;
  end
  else
    ErrorCode := 0;

{$IFDEF MSWINDOWS}
  Result := FD_ISSET(Sd, fdSet);
{$ENDIF}
{$IFDEF POSIX}
  Result := __FD_ISSET(Sd, fdSet);
{$ENDIF}
{$IFDEF UNIX}
  Result := fpFD_ISSET(Sd, fdSet) > 0;
{$ENDIF}
end;

procedure GetIPInterfaces(List: TStrings);
{$IFDEF MSWINDOWS}
var
  Sd: NativeInt;
  Info: INTERFACE_INFO;
  pBuffer: IntPtr;
  BufferLen: Cardinal;
  BytesReturned: Cardinal;
  NumInterfaces, i: Cardinal;
  sa: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Startup;
  Sd := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
  try
    BufferLen := SizeOf(INTERFACE_INFO) * 20;
    pBuffer := Marshal.AllocHGlobal(BufferLen);

    LoadWS2_32Lib; // load WSAIoctl function
    if WSAIoCtl(Sd, SIO_GET_INTERFACE_LIST, nil, 0, pBuffer, BufferLen, BytesReturned, nil, nil) = SOCKET_ERROR then
      raise Exception.CreateFmt('Socket Error Code: %d($%X)', [GetSocketError, GetSocketError]);

    List.BeginUpdate;
    try
      List.Clear;
      NumInterfaces := BytesReturned div SizeOf(INTERFACE_INFO);

      for i := 0 to NumInterfaces - 1 do begin
        Info := PINTERFACE_INFO(PtrOffset(pBuffer, i * SizeOf(INTERFACE_INFO)))^;
        sa := inet_ntoa(WinSock.in_addr(Info.iiAddress.addressIn.sin_addr.S_addr));
        List.Add(string(sa));
      end;
    finally
      List.EndUpdate;
    end;
  finally
    SocketClose(Sd);
  end;
{$ELSE}
  List.Clear;
{$ENDIF}
end;

