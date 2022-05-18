//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRVioTcp;

interface

uses
  Classes, SysUtils,
{$IFDEF POSIX}
  Posix.SysTypes, Posix.Pthread, Posix.SysSocket,
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  {$IFNDEF BCB}
  {$NOINCLUDE winsock}
  {$HPPEMIT '#include <winsock2.h>'}
  {$ENDIF}
  {$ENDIF}
  WinSock,
{$ENDIF}
{$IFDEF UNIX}
  sockets, baseunix, unix, netdb,
{$ENDIF}
  SyncObjs,
  CLRClasses, CRTypes, CRVio, CRVioSocket;

resourcestring
  SUnknownSocketError = 'Unknown socket error';
  SWSAStartupFailed = 'WSAStartup failed.'#$D#$A'Socket Error Code: %d';
  SUnknownHost = 'Unknown host ''%s''.'#$D#$A'Socket Error Code: %d($%X)';
  SCannotCreateSocket = 'Cannot create TCP/IP socket:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SSocketAborted = 'Socket was aborted';
  SCannotBind = 'Cannot bind to address ''%s'':'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SCannotConnect = 'Cannot connect to server on host ''%s'':'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SConnectionTimeout = 'Cannot connect to server on host ''%s'': Connection timed out';
  SErrorOnListening = 'Error on listening for incoming connection:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SErrorOnAccepting = 'Error on accepting of incoming connection attempt:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SErrorOnSettingOptions = 'Error on setting socket options:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SErrorOnRetrievingOptions = 'Error on retrieving socket options:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SErrorOnDataReading = 'Error on data reading from the connection:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SErrorOnDataWriting = 'Error on data writing to the connection:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SInvalidDataWriting = 'Error on data writing to the connection: Invalid socket';
  SSocketErrWithErr = 'Socket error.'#13#10'Socket Error Code = %d($%X)';
  SHttpResponseTooLarge = 'HTTP response is too large';
  SUnknownHttpVersion = 'Unknown HTTP version: ';
  SHttpError = 'HTTP error: ';
  SSocksAccessDenided = 'Socks access denided';
  SSocksResponseTooLarge = 'Socks response is too large';
  SUnknownSocksAddressType = 'Unknown Socks address type';
  SUnsupportedCommandType = 'Unsupported Socks command type';
  SUnsupportedAuthType = 'Unsupported Socks auth type';
  SUnknownSocksVersion = 'Unknown Socks version';

{$IFNDEF MSWINDOWS}
{$IFNDEF MOBILE}
const
  ESysEINTR = 4;
{$IFDEF LINUX}
  EAgain = 11;
{$ELSE}
  EAgain = 35;
{$ENDIF}
{$ENDIF}
{$ENDIF}

type
  TCRConnectMode = (cmBind, cmConnect);

  TConnector = class;

  TCRVioTcp = class(TCRVioSocket)
  private
    FProviderName: string;
    FProxyOptions: TProxyOptions;

    FLocalSockAddr: PSockAddr;
    FRemoteSockAddr: PSockAddr;
    FConnectMode: TCRConnectMode;
    FSocksVersion: TCRSocksVersion;

    function InternalConnect(const Host: string; Port: integer): boolean;

    procedure HttpNegotiate;
    procedure Socks4Negotiate;
    procedure Socks5Negotiate;
    procedure WriteSocks5Address(const Buffer: TBytes; var Offset: integer; const AHostname: string; const APort: integer);

    function GetLocalSockAddr: PSockAddr;
    procedure SetLocalSockAddr(Value: PSockAddr);
    function GetRemoteSockAddr: PSockAddr;
    procedure SetRemoteSockAddr(Value: PSockAddr);
    function GetSockAddrIP(const SockAddr: PSockAddr): string;
    function GetSockAddrPort(const SockAddr: PSockAddr): integer;

  protected
    procedure Init; override;
    procedure CloseSocket; override;
    function GetAvailable: Integer; override;
    function ReadLine: string;
    procedure ProcessWebResponse(const Response: string);

  public
    constructor Create(ProxyOptions: TProxyOptions; const ProviderName: string;
      const Hostname: string; Port: integer; IPVersion: TIPVersion = ivIPv4); overload;
    destructor Destroy; override;

    function CreateNew(NewSd: NativeInt; From: PSockAddr): TCRVioTcp;

    function WaitForConnect(AConnector: TConnector; AConnectionTimeoutMSec: cardinal;
      var AErrorDesc: string): boolean; virtual; // user's functionality

    function TryConnect: Boolean; override;
    procedure Connect; override;
    function TryBind: Boolean;
    procedure Bind;
    procedure ShutDown(how: Integer);

    class function GetSockAddrSize(SockAddr: PSockAddr): {$IFDEF POSIX}socklen_t{$ELSE}Integer{$ENDIF};
    function GetLocalIP: string;
    function GetLocalPort: integer;
    function GetRemoteIP: string;
    function GetRemotePort: integer;

    procedure Listen(Backlog: Integer);
    procedure Accept(out NewSd: NativeInt; out From: PSockAddr);
    function GetSocketOption(OptionLevel, OptionName: integer): integer;
    procedure SetSocketOption(OptionLevel, OptionName, OptionValue: integer);

    function ReadNoWait(const buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const buffer: TValueArr; offset, count: integer): integer; override;
    function Write(const buffer: TValueArr; offset, count: integer): integer; override;

    procedure ReadSocksRequest(out AHostname: string; out APort: integer);
    procedure WriteSocksResponse(const AHostname: string; const APort: integer);

    property LocalSockAddr: PSockAddr read GetLocalSockAddr;
    property RemoteSockAddr: PSockAddr read GetRemoteSockAddr;

    property ProviderName: string read FProviderName;
    property ProxyOptions: TProxyOptions read FProxyOptions;
  end;

  TConnector = class(TThread)
  private
  {$IFNDEF MSWINDOWS}
    FEndEvent: TEvent;
  {$ENDIF}

    FVio: TCRVioTcp;
    FHostname: string;
    FPort: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const Hostname: string; Port: integer; Vio: TCRVioTcp);
    destructor Destroy; override;
    function Wait(Timeout: cardinal): boolean;
  end;

function GetHostName: string;

const
  {$EXTERNALSYM SD_SEND}
  SD_SEND = 1;
  {$EXTERNALSYM SD_BOTH}
  SD_BOTH = 2;
{$IFNDEF MSWINDOWS}
  SOCKET_ERROR = -1;
  {$EXTERNALSYM SOCKET_ERROR}
{$ENDIF}

{$IFDEF LINUX64}
  {$DEFINE SC_USE_REUSEPORT}
{$ENDIF}
{$IFDEF ANDROID}
  {$DEFINE SC_USE_REUSEPORT}
{$ENDIF}
{$IFDEF SC_USE_REUSEPORT}
  // SO_REUSEPORT has different values on different platforms,
  // but we are only interested in it on Android (it is 512 on BSD)
  SC_SO_REUSEPORT = 15;
  {$EXTERNALSYM SC_SO_REUSEPORT}
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.ArpaInet, Posix.Errno, Posix.Fcntl,
  Posix.NetDB, Posix.NetinetIn, Posix.NetinetTCP,
  Posix.UniStd, Posix.SysTime, Posix.SysSelect,
{$ENDIF}
  CRFunctions, CRBase64;

{$I CRSocketImpl.inc}

{$IFDEF POSIX}
  {$IFNDEF ANDROID}
  {$IFNDEF LINUX}
    {$DEFINE __ADDR_NAME}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

var
  IsFinalizated: boolean = False;

function GetHostName: string;
{$IFNDEF UNIX}
var
  str: array [0..63] of AnsiChar;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Res: integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Res := Startup;
  if Res <> 0 then
    raise SocketException.CreateFmt(SWSAStartupFailed, [Res], Res);
{$ENDIF}

{$IFDEF UNIX}
  Result := Unix.GetHostName;
{$ELSE}
  Result := '';
  if {$IFDEF POSIX}Posix.Unistd.{$ELSE}WinSock.{$ENDIF}gethostname({$IFNDEF NEXTGEN}str{$ELSE}PAnsiChar(@str[0]){$ENDIF}, 64) = 0 then
    Result := string(AnsiString({$IFNDEF NEXTGEN}str{$ELSE}PAnsiChar(@str[0]){$ENDIF}));
{$ENDIF}
end;

{ TCRVioTcp }

constructor TCRVioTcp.Create(ProxyOptions: TProxyOptions; const ProviderName: string;
  const Hostname: string; Port: integer; IPVersion: TIPVersion = ivIPv4);
begin
  inherited Create(Hostname, Port, IPVersion);

  FProviderName := ProviderName;
  Assert(FProxyOptions <> nil);
  if Assigned(ProxyOptions) then
    FProxyOptions.Assign(ProxyOptions);
end;

destructor TCRVioTcp.Destroy;
begin
  FProxyOptions.Free;

  inherited;
end;

procedure TCRVioTcp.Init;
begin
  inherited;

  FConnectMode := cmConnect;
  FSocksVersion := svNoSocks;
  FProxyOptions := TProxyOptions.Create;
end;

procedure TCRVioTcp.CloseSocket;
begin
  inherited;

  SetLocalSockAddr(nil);
  SetRemoteSockAddr(nil);
end;

class function TCRVioTcp.GetSockAddrSize(SockAddr: PSockAddr): {$IFDEF POSIX}socklen_t{$ELSE}Integer{$ENDIF};
begin
  if (SockAddr <> nil) and ({$IFDEF POSIX}Psockaddr_in{$ENDIF}(SockAddr).sin_family = AF_INET6) then
    Result := sizeof(sockaddr_in6)
  else
    Result := sizeof(sockaddr_in);
end;

function TCRVioTcp.GetLocalSockAddr: PSockAddr;
var
  len: {$IFDEF POSIX}socklen_t{$ELSE}Integer{$ENDIF};
begin
  if (FLocalSockAddr = nil) and (FRemoteSockAddr <> nil) then begin
    len := GetSockAddrSize(FRemoteSockAddr);
    GetMem(FLocalSockAddr, len);

  {$IFDEF UNIX}
    if fpgetsockname(FSd, FLocalSockAddr, @len) < 0 then
  {$ELSE}
    if getsockname(FSd, FLocalSockAddr^, len) < 0 then
  {$ENDIF}
    begin
      FreeMem(FLocalSockAddr);
      FLocalSockAddr := nil;
      raise SocketException.Create(Format(SSocketErrWithErr, [GetSocketError, GetSocketError]), GetSocketError);
    end;
  end;

  if FLocalSockAddr = nil then
    raise SocketException.Create(SSocketNotConnected);

  Result := FLocalSockAddr;
end;

procedure TCRVioTcp.SetLocalSockAddr(Value: PSockAddr);
begin
  if FLocalSockAddr <> nil then
    FreeMem(FLocalSockAddr);

  FLocalSockAddr := Value;
end;

function TCRVioTcp.GetRemoteSockAddr: PSockAddr;
begin
  if FRemoteSockAddr = nil then
    raise SocketException.Create(SSocketNotConnected);

  Result := FRemoteSockAddr;
end;

procedure TCRVioTcp.SetRemoteSockAddr(Value: PSockAddr);
begin
  if FRemoteSockAddr <> nil then
    FreeMem(FRemoteSockAddr);

  FRemoteSockAddr := Value;
end;

function TCRVioTcp.InternalConnect(const Host: string; Port: integer): boolean;
var
  HostAddrInfo: PAddrInfo;
  RetVal: integer;
  Opt: integer;
begin
  Result := False;

  RetVal := GetHostAddrInfo(Host, FIPVersion, HostAddrInfo);
  if (RetVal <> 0) or (HostAddrInfo = nil) then begin
    FLastErrorCode := GetSocketError;
    FLastError := Format(SUnknownHost, [Host, FLastErrorCode, FLastErrorCode]);
    Exit;
  end;

  try
    if FSd = SOCKET_ERROR then begin
      hLockSd.Acquire;
      try
        if FAborted then begin
          FLastError := SSocketAborted;
          Exit;
        end;

        FSd := SocketCreate({$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_family, SOCK_STREAM, IPPROTO_TCP);
        if FSd = SOCKET_ERROR then begin
          FLastErrorCode := GetSocketError;
          FLastError := Format(SCannotCreateSocket, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
          Exit;
        end;
      finally
        hLockSd.Release;
      end;

      SocketKeepalive(FSd, True);
    end;

    {$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_port := htons(Port);

    try
      if FConnectMode = cmBind then begin
        Opt := 1;
        setsockopt(FSd, SOL_SOCKET, SO_REUSEADDR, @Opt, sizeof(Integer));
      {$IFDEF SC_USE_REUSEPORT}
        setsockopt(FSd, SOL_SOCKET, SC_SO_REUSEPORT, @Opt, sizeof(Integer));
      {$ENDIF}

        // Bind the Socket to the desired port
      {$IFDEF UNIX}
        if (fpbind(FSd, HostAddrInfo.ai_addr, HostAddrInfo.ai_addrlen) < 0)
      {$ENDIF}
      {$IFDEF POSIX}
        if (Posix.SysSocket.bind(FSd, HostAddrInfo.ai_addr^, HostAddrInfo.ai_addrlen) < 0)
      {$ENDIF}
      {$IFDEF MSWINDOWS}
        if (WinSock.bind(FSd, HostAddrInfo.ai_addr^, HostAddrInfo.ai_addrlen) < 0)
      {$ENDIF}
        then begin
          FLastErrorCode := GetSocketError;
          FLastError := Format(SCannotBind, [Host, SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
          Exit;
        end;
      end
      else begin
      {$IFDEF UNIX}
        if (fpconnect(FSd, HostAddrInfo.ai_addr, HostAddrInfo.ai_addrlen) < 0)
      {$ENDIF}
      {$IFDEF POSIX}
        if (Posix.SysSocket.connect(FSd, HostAddrInfo.ai_addr^, HostAddrInfo.ai_addrlen) < 0)
      {$ENDIF}
      {$IFDEF MSWINDOWS}
        if (WinSock.connect(FSd, HostAddrInfo.ai_addr^, HostAddrInfo.ai_addrlen) < 0)
          and (WSAGetLastError <> WSAEWOULDBLOCK)
      {$ELSE}
          {and ??? errno = EWOUDLBLOCK}
      {$ENDIF}
        then begin
          FLastErrorCode := GetSocketError;
          FLastError := Format(SCannotConnect, [Host, SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
          Exit;
        end;
      end;

      Result := True;
    finally
      if not Result then
        CloseSocket;
    end;

    Assert(FRemoteSockAddr = nil);
    GetMem(FRemoteSockAddr, HostAddrInfo.ai_addrlen);
    Move(HostAddrInfo.ai_addr^, FRemoteSockAddr^, HostAddrInfo.ai_addrlen);

  finally
  {$IFDEF MSWINDOWS}
    if not IsAddrInfoSupported then
      ApiFreeAddrInfo(HostAddrInfo)
    else
  {$ENDIF}
      FreeAddrInfo(HostAddrInfo{$IFDEF POSIX}^{$ENDIF});
  end;
end;

function TCRVioTcp.WaitForConnect(AConnector: TConnector; AConnectionTimeoutMSec: cardinal;
  var AErrorDesc: string): boolean;
begin
  AErrorDesc := '';
  Result := not AConnector.Wait(AConnectionTimeoutMSec);
end;

function TCRVioTcp.TryConnect: Boolean;
var
{$IFDEF MSWINDOWS}
  Res: integer;
{$ENDIF}
  Connector: TConnector;
  OldValue: integer;
  SocketHost: string;
  SocketPort: integer;
  ConnectionTimeoutMSec: cardinal;
  AError: string;
begin
  Result := inherited TryConnect;

  FAborted := False;
  SetLocalSockAddr(nil);
  SetRemoteSockAddr(nil);
  FLastError := '';
  FLastErrorCode := 0;
  if FHostname = '' then
    FHostname := LOCAL_HOST;

  try
  {$IFDEF MSWINDOWS}
    Res := Startup;
    if Res <> 0 then begin
      FLastErrorCode := Res;
      FLastError := Format(SWSAStartupFailed, [Res]);
      Exit;
    end;
    LoadWS2_32Lib; // load WSAIoctl function
  {$ENDIF}

    if FConnectMode = cmBind then begin
      SocketHost := FBindAddress;
      SocketPort := FPort;
    end
    else
    if FProxyOptions.Hostname <> '' then begin
      SocketHost := FProxyOptions.Hostname;
      SocketPort := FProxyOptions.Port;
    end
    else begin
      SocketHost := FHostName;
      SocketPort := FPort;
    end;

    if FConnectionTimeout > 0 then begin
      Connector := TConnector.Create(SocketHost, SocketPort, Self);
      try
        if (Int64(FConnectionTimeout) * 1000) > MaxInt then
          ConnectionTimeoutMSec := MaxInt
        else
          ConnectionTimeoutMSec := FConnectionTimeout * 1000;

        AError := '';
        if not WaitForConnect(Connector, ConnectionTimeoutMSec, AError) then begin
        {$IFDEF MSWINDOWS}
          TerminateThread(Connector.Handle, 0);
        {$ENDIF}
        {$IFDEF POSIX}
          pthread_detach(pthread_t(Connector.ThreadID));
          Connector.FreeOnTerminate := True;
        {$ENDIF POSIX}

          if AError <> '' then
            FLastError := AError
          else
            FLastError := Format(SConnectionTimeout, [SocketHost]);
        end
        else begin
          Result := FLastError = '';
        end;
      finally
        if not Connector.FreeOnTerminate then
          Connector.Free;
      end;
    end
    else
      Result := InternalConnect(SocketHost, SocketPort);

    if not Result then
      Exit;

    try
      if FConnectMode = cmConnect then
        SocketFastsend(FSd);

      // Force to set timeout
      if FSendTimeout <> 0 then begin
        OldValue := FSendTimeout;
        try
          FSendTimeout := -1;
          SetSendTimeout(OldValue);
        finally
          FSendTimeout := OldValue;
        end;
      end;

      OldValue := FReceiveTimeout;
      try
        FReceiveTimeout := -1;
        SetReceiveTimeout(OldValue);
      finally
        FReceiveTimeout := OldValue;
      end;

      if FSendBuffer <> DefaultBuffer then begin
        OldValue := FSendBuffer;
        try
          FSendBuffer := -1;
          SetSendBuffer(OldValue);
        finally
          FSendBuffer := OldValue;
        end;
      end;

      if FReceiveBuffer <> DefaultBuffer then begin
        OldValue := FReceiveBuffer;
        try
          FReceiveBuffer := -1;
          SetReceiveBuffer(OldValue);
        finally
          FReceiveBuffer := OldValue;
        end;
      end;

    except
      Result := False;
      raise;
    end;

  finally
    if not Result then
      CloseSocket;
  end;
end;

procedure TCRVioTcp.Connect;
begin
  FConnectMode := cmConnect;

  if not TryConnect then
    if FLastError <> '' then
      raise SocketException.Create(FLastError, FLastErrorCode)
    else
      raise SocketException.Create(SUnknownSocketError);

  if FProxyOptions.Hostname <> '' then begin
    try
      case FProxyOptions.SocksVersion of
        svNoSocks:
          HttpNegotiate;
        svSocks4:
          Socks4Negotiate;
        svSocks5:
          Socks5Negotiate;
      end;
    except
      CloseSocket;
      raise;
    end;
  end;
end;

procedure TCRVioTcp.Socks4Negotiate;
var
  ip_addr: cardinal;
  HostAddrInfo: PAddrInfo;
  RetVal: integer;
  Buf, IDBuf: TBytes;
  Count, IDLen: integer;
begin
  SetLength(IDBuf, 0);
  IDBuf := Encoding.UTF8.GetBytes(FProxyOptions.Username);
  IDLen := Length(IDBuf);

  SetLength(Buf, 9 + IDLen);
  Buf[0] := 4; // version
  Buf[1] := 1; // command type
  Buf[2] := byte(FPort shr 8);
  Buf[3] := byte(FPort);

  ip_addr := cardinal(inet_addr(PAnsiChar(AnsiString(FHostName))));
  if ip_addr = cardinal(INADDR_NONE) then begin
    RetVal := GetHostAddrInfo(FHostName, ivIPv4, HostAddrInfo);
    if (RetVal <> 0) or (HostAddrInfo = nil) then begin
      FLastErrorCode := GetSocketError;
      FLastError := Format(SUnknownHost, [FHostName, FLastErrorCode, FLastErrorCode]);
      raise SocketException.Create(FLastError, FLastErrorCode)
    end;
    ip_addr := {$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_addr.S_addr;
  end;

  Move(ip_addr, Buf[4], 4);

  // User ID
  if IDLen > 0 then
    Move(IDBuf[0], Buf[8], IDLen);
  Buf[8 + IDLen] := 0;

  WriteNoWait(TValueArr(Buf), 0, 9 + IDLen);
  Count := ReadNoWait(TValueArr(Buf), 0, 8);
  if Count < 8 then
    raise SocketException.Create(FLastError, FLastErrorCode);

  if Buf[1] <> $5A then
    raise SocketException.Create(SSocksAccessDenided);
end;

procedure TCRVioTcp.Socks5Negotiate;
var
  Buf, UserBuf, PassBuf: TBytes;
  UserLen, PassLen: integer;
  Offset, Count, NeedLen: integer;
begin
  SetLength(UserBuf, 0);
  SetLength(PassBuf, 0);
  UserBuf := Encoding.UTF8.GetBytes(FProxyOptions.Username);
  UserLen := byte(Length(UserBuf));

  SetLength(Buf, 1024);
  Buf[0] := 5; // version

  if UserLen > 0 then begin
    Buf[1] := 2; // auth count
    Buf[2] := 0; // no auth
    Buf[3] := 2; // user/pass
    Count := 4;
  end
  else begin
    Buf[1] := 1; // auth count
    Buf[2] := 0; // no auth
    Count := 3;
  end;

  WriteNoWait(TValueArr(Buf), 0, Count);
  Count := ReadNoWait(TValueArr(Buf), 0, 2);
  if Count < 2 then
    raise SocketException.Create(FLastError, FLastErrorCode);

  if (Buf[0] <> 5) or (Buf[1] = $FF) then
    raise SocketException.Create(SSocksAccessDenided);

  if Buf[1] = 2 then begin
    Buf[0] := 1;
    Buf[1] := UserLen;
    if UserLen > 0 then
      Move(UserBuf[0], Buf[2], UserLen);

    PassBuf := Encoding.UTF8.GetBytes(FProxyOptions.Password);
    PassLen := byte(Length(PassBuf));
    Buf[2 + UserLen] := PassLen;
    if PassLen > 0 then
      Move(PassBuf[0], Buf[3 + UserLen], PassLen);

    WriteNoWait(TValueArr(Buf), 0, 3 + UserLen + PassLen);
    Count := ReadNoWait(TValueArr(Buf), 0, 2);
    if Count < 2 then
      raise SocketException.Create(FLastError, FLastErrorCode);

    if Buf[1] <> 0 then
      raise SocketException.Create(SSocksAccessDenided);
  end;

  Buf[0] := 5; // version
  Buf[1] := 1; // command type
  Buf[2] := 0; // reserved

  Offset := 3;
  WriteSocks5Address(Buf, Offset, FHostname, FPort);
  WriteNoWait(TValueArr(Buf), 0, Offset);

  Count := ReadNoWait(TValueArr(Buf), 0, Length(Buf));
  if Count < 7 then
    raise SocketException.Create(FLastError, FLastErrorCode);

  if Buf[1] <> 0 then
    raise SocketException.Create(SSocksAccessDenided + ' [' + IntToStr(Buf[1]) + ']');

  case Buf[3] of
    1:
      NeedLen := 4 + 4 + 2;
    3:
      NeedLen := 4 + 1 + Buf[4] + 2;
    4:
      NeedLen := 4 + 16 + 2;
  else
    raise SocketException.Create(SUnknownSocksAddressType + ' [' + IntToStr(Buf[3]) + ']');
  end;

  if NeedLen > Count then begin
    Dec(NeedLen, Count);
    Count := ReadNoWait(TValueArr(Buf), Count, NeedLen);
    if Count < NeedLen then
      raise SocketException.Create(FLastError, FLastErrorCode);
  end
  else
  if NeedLen < Count then
    raise SocketException.Create(SSocksResponseTooLarge);
end;

procedure TCRVioTcp.WriteSocks5Address(const Buffer: TBytes; var Offset: integer;
  const AHostname: string; const APort: integer);
var
  ip_addr: cardinal;
  HostAddrInfo: PAddrInfo;
  RetVal: integer;
  DNSBuf: TBytes;
  DNSLen: integer;
begin
  ip_addr := cardinal(inet_addr(PAnsiChar(AnsiString(AHostName))));
  if ip_addr = cardinal(INADDR_NONE) then begin
    if FProxyOptions.ResolveDNS then begin
      RetVal := GetHostAddrInfo(AHostName, FIPVersion, HostAddrInfo);
      if (RetVal <> 0) or (HostAddrInfo = nil) then begin
        FLastErrorCode := GetSocketError;
        FLastError := Format(SUnknownHost, [AHostName, FLastErrorCode, FLastErrorCode]);
        raise SocketException.Create(FLastError, FLastErrorCode)
      end;

      if {$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_family = AF_INET6 then begin
        Buffer[Offset] := 4; // Address type = IPv6
        Move(PSockAddr_In6(HostAddrInfo.ai_addr).sin6_addr.{$IFDEF __ADDR_NAME}__s6_addr16{$ELSE}s6_addr16{$ENDIF}, Buffer[Offset + 1], 16);
        Inc(Offset, 17);
      end
      else begin
        Buffer[Offset] := 1; // Address type = IPv4
        Move({$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_addr.s_addr, Buffer[Offset + 1], 4);
        Inc(Offset, 5);
      end;
    end
    else begin
      Buffer[Offset] := 3; // Address type = DNS
      SetLength(DNSBuf, 0);
      DNSBuf := Encoding.Default.GetBytes(AHostName);
      DNSLen := byte(Length(DNSBuf));
      Buffer[Offset + 1] := DNSLen;
      Move(DNSBuf[0], Buffer[Offset + 2], DNSLen);
      Inc(Offset, 2 + DNSLen);
    end;
  end
  else begin
    Buffer[Offset] := 1; // Address type = IPv4
    Move(ip_addr, Buffer[Offset + 1], 4);
    Inc(Offset, 5);
  end;

  Buffer[Offset] := byte(APort shr 8);
  Buffer[Offset + 1] := byte(APort);
  Inc(Offset, 2);
end;

procedure TCRVioTcp.HttpNegotiate;
var
  Credentials: TBytes;
  Base64Credentials: string;
  UserAgent: string;
  HttpRequest: TBytes;
begin
  if Trim(FProviderName) <> '' then
    UserAgent := #13#10'User-Agent: ' + FProviderName
  else
    UserAgent := '';

  SetLength(HttpRequest, 0);
  if FProxyOptions.Username <> '' then begin
    SetLength(Credentials, 0);
    Credentials := Encoding.UTF8.GetBytes(FProxyOptions.Username + ':' + FProxyOptions.Password);
    Base64Credentials := Encoding.Default.GetString(TBase64.Encode(Credentials));
    HttpRequest := Encoding.Default.GetBytes(Format('CONNECT %s:%d HTTP/1.1%s'#13#10'Proxy-Authorization: Basic %s'#13#10#13#10, [FHostName, FPort, UserAgent, Base64Credentials]));
  end
  else
    HttpRequest := Encoding.Default.GetBytes(Format('CONNECT %s:%d HTTP/1.0%s'#13#10#13#10, [FHostName, FPort, UserAgent]));

  WriteNoWait(TValueArr(HttpRequest), 0, Length(HttpRequest));
  ProcessWebResponse(ReadLine);
end;

function TCRVioTcp.ReadLine: string;
var
  Response: TBytes;
  WritePos, ReadPos: integer;
  ReadCount: integer;
  SeparatorNo: integer;
  i: integer;
begin
  SetLength(Response, 1024);
  ReadCount := ReadNoWait(TValueArr(Response), 0, 16); // 'HTTP/1.1 200'#13#10#13#10
  if ReadCount <= 0 then
    raise SocketException.Create(FLastError, FLastErrorCode);

  SeparatorNo := 0;
  ReadPos := 0;
  WritePos := ReadCount;

  while True do begin
    for i := ReadPos to WritePos - 1 do begin
      if Response[i] = 13 then begin
        if (SeparatorNo = 0) or (SeparatorNo = 2) then
          Inc(SeparatorNo)
        else
          SeparatorNo := 1;
      end
      else
      if Response[i] = 10 then begin
        if (SeparatorNo = 1) or (SeparatorNo = 3) then
          Inc(SeparatorNo)
        else
          SeparatorNo := 0;
      end
      else
        SeparatorNo := 0;

      if SeparatorNo = 4 then
        Break;
    end;

    if SeparatorNo = 4 then
      Break;

    ReadPos := WritePos;

    if (WritePos + 4) >= Length(Response) then
      raise SocketException.Create(SHttpResponseTooLarge);

    ReadCount := ReadNoWait(TValueArr(Response), WritePos, 4 - SeparatorNo);
    if ReadCount > 0 then
      Inc(WritePos, ReadCount)
    else
      raise SocketException.Create(FLastError, FLastErrorCode);
  end;

  Result := Encoding.Default.GetString(Response, 0, WritePos - 4{CRLFCRLF});
end;

procedure TCRVioTcp.ProcessWebResponse(const Response: string);
var
  HttpVersion, HttpCode: string;
begin
  // 'HTTP/1.1 200 Connection established'
  HttpVersion := Copy(Response, 1, 9);
  HttpCode := Copy(Response, 10, 3);
  if (HttpVersion <> 'HTTP/1.1 ') and (HttpVersion <> 'HTTP/1.0 ') then
    raise SocketException.Create(SUnknownHttpVersion + HttpVersion);
  if HttpCode <> '200' then
    raise SocketException.Create(SHttpError + Copy(Response, 10, Length(Response)));
end;

function TCRVioTcp.TryBind: Boolean;
begin
  if FSd <> SOCKET_ERROR then
    raise SocketException.Create(SSocketError);

  if FBindAddress = '' then begin
    if FIPVersion in [ivIPv6, ivIPBoth] then
      FBindAddress := '::'
    else
      FBindAddress := '0.0.0.0';
  end;

  FConnectMode := cmBind;
  try
    Result := TryConnect;
  finally
    FConnectMode := cmConnect;
  end;
end;

procedure TCRVioTcp.Bind;
begin
  if not TryBind then
    if FLastError <> '' then
      raise SocketException.Create(FLastError, FLastErrorCode)
    else
      raise SocketException.Create(SUnknownSocketError);
end;

procedure TCRVioTcp.ShutDown(how: Integer);
begin
  SocketShutdown(FSd, how);
end;

procedure TCRVioTcp.Listen(Backlog: Integer);
var
  opt: linger;
begin
  if FSd = SOCKET_ERROR then
    raise SocketException.Create(SSocketNotConnected);

  try
    // We try to make the port reusable and have it Close as fast as possible
    // without waiting in unnecessary wait states on Close.
    //opt := 1;
    //setsockopt(FSd, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@opt), sizeof(opt));
    opt.l_onoff := 1;
    opt.l_Linger := 5;
    setsockopt(FSd, SOL_SOCKET, SO_LINGER, PAnsiChar(@opt), sizeof(opt));

  {$IFDEF UNIX}
    if fplisten(FSd, Backlog) < 0 then
  {$ELSE}
    if {$IFDEF POSIX}Posix.SysSocket.{$ELSE}WinSock.{$ENDIF}listen(FSd, Backlog) < 0 then
  {$ENDIF}
    begin
      FLastErrorCode := GetSocketError;
      FLastError := Format(SErrorOnListening, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
      raise SocketException.Create(FLastError, FLastErrorCode);
    end;
  except
    CloseSocket;
    raise;
  end;
end;

procedure TCRVioTcp.Accept(out NewSd: NativeInt; out From: PSockAddr);
var
  FromLen: {$IFDEF POSIX}socklen_t{$ELSE}Integer{$ENDIF};
begin
  if FSd = SOCKET_ERROR then
    raise SocketException.Create(SSocketNotConnected);

  FromLen := GetSockAddrSize(FRemoteSockAddr);
  GetMem(From, FromLen);
  try
    NewSd :=
    {$IFDEF UNIX}
      fpaccept(FSd, From, @FromLen);
    {$ENDIF}
    {$IFDEF POSIX}
      Posix.SysSocket.accept(FSd, From^, FromLen);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      WinSock.accept(FSd, From, @FromLen);
    {$ENDIF}
    if NewSd < 0 then begin
      FLastErrorCode := GetSocketError;
      FLastError := Format(SErrorOnAccepting, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
      raise SocketException.Create(FLastError, FLastErrorCode);
    end;
  except
    FreeMem(From);
    raise;
  end;
end;

function TCRVioTcp.CreateNew(NewSd: NativeInt; From: PSockAddr): TCRVioTcp;
var
  opt: integer;
begin
  Result := TCRVioTcp.Create;
  try
    Result.FSd := NewSd;
    Result.FIPVersion := IPVersion;
    Result.SetLocalSockAddr(nil);
    Result.SetRemoteSockAddr(From);
    SocketFastsend(NewSd);

    opt := GetSocketOption(SOL_SOCKET, SO_KEEPALIVE);
    Result.SetSocketOption(SOL_SOCKET, SO_KEEPALIVE, opt);
  except
    Result.Free;
    raise;
  end;
end;

function TCRVioTcp.GetSocketOption(OptionLevel, OptionName: integer): integer;
var
  Size: {$IFDEF MSWINDOWS}integer{$ELSE}socklen_t{$ENDIF};
begin
  if FSd = SOCKET_ERROR then
    raise SocketException.Create(SSocketNotConnected);

  Size := sizeof(Result);
  if getsockopt(FSd, OptionLevel, OptionName, PAnsiChar(@Result), Size) <> 0 then begin
    FLastErrorCode := GetSocketError;
    FLastError := Format(SErrorOnRetrievingOptions, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
    raise SocketException.Create(FLastError, FLastErrorCode);
  end;
end;

procedure TCRVioTcp.SetSocketOption(OptionLevel, OptionName, OptionValue: integer);
begin
  if FSd = SOCKET_ERROR then
    raise SocketException.Create(SSocketNotConnected);

  if setsockopt(FSd, OptionLevel, OptionName, PAnsiChar(@OptionValue), sizeof(OptionValue)) <> 0 then
  {$IFDEF MSWINDOWS}
  begin
    FLastErrorCode := GetSocketError;
    FLastError := Format(SErrorOnSettingOptions, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
    raise SocketException.Create(FLastError, FLastErrorCode);
  end;
  {$ENDIF}
end;

function TCRVioTcp.GetSockAddrIP(const SockAddr: PSockAddr): string;
var
  i: integer;
begin
  Assert(SockAddr <> nil);

  if ({$IFDEF POSIX}Psockaddr_in{$ENDIF}(SockAddr).sin_family = AF_INET6) then begin
    Result := '';
    for i := 0 to 7 do
      Result := Result + IntToHex(ntohs(PSockAddr_In6(SockAddr).sin6_addr.{$IFDEF __ADDR_NAME}__s6_addr16{$ELSE}s6_addr16{$ENDIF}[i]), 4) + ':';
    SetLength(Result, Length(Result) - 1);
  end
  else
  if ({$IFDEF POSIX}Psockaddr_in{$ENDIF}(SockAddr).sin_family = AF_INET) then
    Result := string(inet_ntoa({$IFDEF POSIX}Psockaddr_in{$ENDIF}(SockAddr).sin_addr))
  else
    raise SocketException.Create(SUnknownSocketError);
end;

function TCRVioTcp.GetSockAddrPort(const SockAddr: PSockAddr): integer;
begin
  Assert(SockAddr <> nil);

  if ({$IFDEF POSIX}Psockaddr_in{$ENDIF}(SockAddr).sin_family = AF_INET6) then
    Result := ntohs(PSockAddr_In6(SockAddr).sin6_port)
  else
  if ({$IFDEF POSIX}Psockaddr_in{$ENDIF}(SockAddr).sin_family = AF_INET) then
    Result := ntohs({$IFDEF POSIX}Psockaddr_in{$ENDIF}(SockAddr).sin_port)
  else
    raise SocketException.Create(SUnknownSocketError);
end;

function TCRVioTcp.GetLocalIP: string;
begin
  Result := GetSockAddrIP(GetLocalSockAddr);
end;

function TCRVioTcp.GetLocalPort: integer;
begin
  Result := GetSockAddrPort(GetLocalSockAddr);
end;

function TCRVioTcp.GetRemoteIP: string;
begin
  Result := GetSockAddrIP(GetRemoteSockAddr);
end;

function TCRVioTcp.GetRemotePort: integer;
begin
  Result := GetSockAddrPort(GetRemoteSockAddr);
end;

function TCRVioTcp.ReadNoWait(const buffer: TValueArr; offset, count: integer): integer;

  function InernalReceive(Loop: integer): integer;
  begin
    Result := recv(FSd, buffer[offset], count, 0);

    if Result = SOCKET_ERROR then begin
    {$IFNDEF MSWINDOWS}
    {$IFNDEF MOBILE}
      if (GetSocketError in [ESysEINTR, EAgain]) and (Loop > 0) then begin
        sleep(10);
        Result := InernalReceive(Loop - 1);
      end
      else
    {$ENDIF}
    {$ENDIF}
      begin
        if not IsFinalizated then begin
          FLastErrorCode := GetSocketError;
          FLastError := Format(SErrorOnDataReading, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
        end;
        Result := 0; // silent error handling
      end;
    end;
  end;

begin
  if FSd = SOCKET_ERROR then
    raise SocketException.Create(SSocketNotConnected);

  FLastError := '';
  FLastErrorCode := 0;
  Result := InernalReceive(5);
end;

function TCRVioTcp.WriteNoWait(const buffer: TValueArr; offset, count: integer): integer;
begin
  if FSd = SOCKET_ERROR then
    raise SocketException.Create(SSocketNotConnected);

  FLastError := '';
  FLastErrorCode := 0;
  Result := send(FSd, buffer[offset], count, {$IFDEF LINUX}MSG_NOSIGNAL{$ELSE}0{$ENDIF});
  if Result = SOCKET_ERROR then begin
    if not IsFinalizated then begin
      FLastErrorCode := GetSocketError;
      FLastError := Format(SErrorOnDataWriting, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
    end;
    Result := 0; // silent error handling
  end;
end;

function TCRVioTcp.Write(const buffer: TValueArr; offset, count: integer): integer;
begin
  if FSd = SOCKET_ERROR then begin
    FLastErrorCode := 0;
    FLastError := SInvalidDataWriting;
    Result := 0;
  end
  else
    Result := inherited Write(buffer, offset, count);
end;

function TCRVioTcp.GetAvailable: Integer;
begin
  if FSd = SOCKET_ERROR then
    raise SocketException.Create(SSocketNotConnected);

{$IFDEF MSWINDOWS}
  Result := 0;
  if ioctlsocket(FSd, FIONREAD, Result) <> 0 then begin
    FLastErrorCode := GetSocketError;
    FLastError := Format(SErrorOnRetrievingOptions, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
    raise SocketException.Create(FLastError, FLastErrorCode);
  end;
{$ELSE}
  if WaitForData(0) then
    Result := 1
  else
    Result := 0;
{$ENDIF}
end;

procedure TCRVioTcp.ReadSocksRequest(out AHostname: string; out APort: integer);
var
  Buf, DNSBuf: TBytes;
  Count, Offset, DNSLen, NeedLen: integer;
  AuthSupported: boolean;
  i: integer;
begin
  SetLength(Buf, 1024);
  Count := ReadNoWait(TValueArr(Buf), 0, Length(Buf));
  if Count < 2 then
    raise SocketException.Create(FLastError, FLastErrorCode);
  Offset := Count;

  if Buf[0] = 4 then begin
    FSocksVersion := svSocks4;

    if Count < 9 then begin
      Count := ReadNoWait(TValueArr(Buf), Offset, Length(Buf) - Offset);
      Inc(Count, Offset);
      if Count < 9 then
        raise SocketException.Create(FLastError, FLastErrorCode);
    end;

    Offset := 8;
    while Buf[Offset] <> 0 do begin
      Inc(Offset);
      if Offset >= Count then begin
        Count := ReadNoWait(TValueArr(Buf), Offset, Length(Buf) - Offset);
        if Count <= 0 then
          raise SocketException.Create(FLastError, FLastErrorCode);
        Inc(Count, Offset);
      end;
    end;

    if Buf[1] <> 1 then
      raise SocketException.Create(SUnsupportedCommandType);

    APort := (Buf[2] shl 8) + Buf[3];
    AHostname := IntToStr(Buf[4]) + '.' + IntToStr(Buf[5]) + '.' + IntToStr(Buf[6]) + '.' + IntToStr(Buf[7]);
  end
  else
  if Buf[0] = 5 then begin
    FSocksVersion := svSocks5;

    if Count < (Buf[1]{Auth count} + 2) then begin
      Count := ReadNoWait(TValueArr(Buf), Offset, Length(Buf) - Offset);
      Inc(Count, Offset);
      if Count < (Buf[1] + 2) then
        raise SocketException.Create(FLastError, FLastErrorCode);
    end;

    AuthSupported := False;
    for i := 0 to Buf[1]{Auth count} - 1 do begin
      if Buf[2 + i] in [0, 2] then begin
        AuthSupported := True;
        break;
      end;
    end;

    if not AuthSupported then
      raise SocketException.Create(SUnsupportedAuthType);

    Buf[0] := 5;
    Buf[1] := 0;
    WriteNoWait(TValueArr(Buf), 0, 2);

    Count := ReadNoWait(TValueArr(Buf), 0, Length(Buf));
    if Count < 5 then
      raise SocketException.Create(FLastError, FLastErrorCode);
    Offset := Count;

    if Buf[0] <> 5 then
      raise SocketException.Create(SUnknownSocksVersion);

    if Buf[1] <> 1 then
      raise SocketException.Create(SUnsupportedCommandType);

    case Buf[3] of
      1:
        NeedLen := 4 + 4 + 2;
      3:
        NeedLen := 4 + 1 + Buf[4]{DNSLen} + 2;
      4:
        NeedLen := 4 + 16 + 2;
    else
      raise SocketException.Create(SUnknownSocksAddressType);
    end;

    if Count < NeedLen then begin
      Count := ReadNoWait(TValueArr(Buf), Offset, NeedLen - Count);
      Inc(Count, Offset);
      if Count < NeedLen then
        raise SocketException.Create(FLastError, FLastErrorCode);
    end;

    case Buf[3] of
      1: begin
        AHostname := IntToStr(Buf[4]) + '.' + IntToStr(Buf[5]) + '.' + IntToStr(Buf[6]) + '.' + IntToStr(Buf[7]);
        Offset := 8;
      end;
      3: begin
        DNSLen := Buf[4];
        SetLength(DNSBuf, DNSLen);
        if DNSLen > 0 then
          Move(Buf[5], DNSBuf[0], DNSLen);

        AHostname := Encoding.Default.GetString(DNSBuf);
        Offset := 5 + DNSLen;
      end;
      4: begin
        AHostname := '';
        for i := 0 to 7 do
          AHostname := AHostname + IntToHex((Buf[4 + i * 2] shl 8) + Buf[4 + i * 2 + 1], 4) + ':';
        SetLength(AHostname, Length(AHostname) - 1);
        Offset := 20;
      end;
    end;

    APort := (Buf[Offset] shl 8) + Buf[Offset + 1];
  end
  else
    raise SocketException.Create(SUnknownSocksVersion);
end;

procedure TCRVioTcp.WriteSocksResponse(const AHostname: string; const APort: integer);
var
  Buf: TBytes;
  Offset, Count: integer;
begin
  SetLength(Buf, 1024);
  case FSocksVersion of
    svSocks4: begin
      Buf[0] := 0;   // NUL-byte
      Buf[1] := $5A; // result code
      Offset := 8;
    end;
    svSocks5: begin
      Buf[0] := 5; // version
      Buf[1] := 0; // result code
      Buf[2] := 0; // reserved
      Offset := 3;
      WriteSocks5Address(Buf, Offset, AHostname, APort);
    end
  else
    raise SocketException.Create(SUnknownSocksVersion);
  end;

  Count := WriteNoWait(TValueArr(Buf), 0, Offset);
  if Count < Offset then
    raise SocketException.Create(FLastError, FLastErrorCode);
end;

{ TConnector }

constructor TConnector.Create(const Hostname: string; Port: integer; Vio: TCRVioTcp);
begin
  FVio := Vio;
  FHostname := HostName;
  FPort := Port;

{$IFNDEF MSWINDOWS}
  FEndEvent := CreateEvent;
{$ENDIF}

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TConnector.Destroy;
begin
  inherited;

{$IFNDEF MSWINDOWS}
  FEndEvent.Free;
{$ENDIF}
end;

function TConnector.Wait(Timeout: cardinal): boolean;
begin
{$IFDEF MSWINDOWS}
  Result := WaitForSingleObject(Handle, Timeout) <> WAIT_OBJECT_0;
{$ELSE}
  Result := FEndEvent.WaitFor(Timeout) <> wrSignaled;
{$ENDIF}
end;

procedure TConnector.Execute;
begin
  FVio.InternalConnect(FHostname, FPort);
{$IFNDEF MSWINDOWS}
  FEndEvent.SetEvent;
{$ENDIF}
end;

initialization

finalization
  IsFinalizated := True;
{$IFDEF MSWINDOWS}
  FreeWS2_32Lib;
{$ENDIF}

end.
