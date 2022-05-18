//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRVioUdp;

interface

uses
  Classes, SysUtils,
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
{$IFDEF POSIX}
  Posix.ArpaInet, Posix.Errno, Posix.Fcntl,
  Posix.NetDB, Posix.NetinetIn, Posix.NetinetTCP,
  Posix.SysSocket, Posix.UniStd,
  Posix.SysTime, Posix.SysSelect,
{$ENDIF}
  SyncObjs, CLRClasses, CRTypes, CRVio, CRVioSocket;

type
  TCRVioUdp = class(TCRVioSocket)
  private
    FIsBroadcast: boolean;
    FBroadcastHostAddrInfo: IntPtr;
    FHostAddrInfo: IntPtr;

    function CheckHostAddrInfo(IsBroadcast: boolean): IntPtr;
    function InternalReadFrom(const Buffer: TValueArr; Offset, Count: integer): integer;

  protected
    procedure InternalClose; override;

  public
    procedure CreateSocket;
    procedure Connect; override;

    function Read(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function ReadFromHost(const Buffer: TValueArr; Offset, Count: integer; out FromHost: boolean): integer;
    function ReadNoWait(const buffer: TValueArr; offset, count: integer): integer; override;
    function Write(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function WriteNoWait(const buffer: TValueArr; offset, count: integer): integer; override;

    property IsBroadcast: boolean read FIsBroadcast write FIsBroadcast;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  CRFunctions;

{$I CRSocketImpl.inc}

{ TCRVioUdp }

procedure TCRVioUdp.CreateSocket;
var
{$IFDEF MSWINDOWS}
  Res: integer;
{$ENDIF}
  ai_family: integer;
  HostAddrInfo: PAddrInfo;
begin
  Assert(FSd = SOCKET_ERROR);
  FLastError := '';
  FLastErrorCode := 0;

{$IFDEF MSWINDOWS}
  Res := Startup;
  if Res <> 0 then begin
    FLastErrorCode := Res;
    FLastError := Format('WSAStartup failed.'#$D#$A'Socket Error Code: %d', [Res]);
    raise SocketException.Create(FLastError);
  end;
{$ENDIF}

  HostAddrInfo := CheckHostAddrInfo(FHostName = '');
  if HostAddrInfo <> nil then
    ai_family := {$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_family
  else
  if FIPVersion = ivIPv6 then
    ai_family := AF_INET6
  else
    ai_family := AF_INET;

  FSd := SocketCreate(ai_family, SOCK_DGRAM, 0);
  if FSd = SOCKET_ERROR then begin
    FLastErrorCode := GetSocketError;
    FLastError := Format('Cannot create UDP socket:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)', [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
    raise SocketException.Create(FLastError);
  end;

  if FIsBroadcast then
    SocketBroadcast(FSd, FIsBroadcast);
end;

procedure TCRVioUdp.Connect;
begin
  raise SocketException.Create('A datagram socket is a type of connectionless network socket');
end;

procedure TCRVioUdp.InternalClose;
begin
  if FBroadcastHostAddrInfo <> nil then
  {$IFDEF MSWINDOWS}
    if not IsAddrInfoSupported then
      ApiFreeAddrInfo(PAddrInfo(FBroadcastHostAddrInfo))
    else
  {$ENDIF}
    freeaddrinfo(PAddrInfo(FBroadcastHostAddrInfo){$IFDEF POSIX}^{$ENDIF});

  if FHostAddrInfo <> nil then
  {$IFDEF MSWINDOWS}
    if not IsAddrInfoSupported then
      ApiFreeAddrInfo(PAddrInfo(FHostAddrInfo))
    else
  {$ENDIF}
    freeaddrinfo(PAddrInfo(FHostAddrInfo){$IFDEF POSIX}^{$ENDIF});

  inherited;
end;

function TCRVioUdp.CheckHostAddrInfo(IsBroadcast: boolean): IntPtr;
var
  HostAddrInfo: PAddrInfo;
  RetVal: integer;
begin
  if IsBroadcast then
    HostAddrInfo := FBroadcastHostAddrInfo
  else
    HostAddrInfo := FHostAddrInfo;

  if HostAddrInfo = nil then begin
    if IsBroadcast then
      RetVal := GetHostAddrInfo(LOCAL_HOST, FIPVersion, HostAddrInfo)
    else
      RetVal := GetHostAddrInfo(FHostName, FIPVersion, HostAddrInfo);

    if (RetVal <> 0) or (HostAddrInfo = nil) then begin
      FLastErrorCode := GetSocketError;
      FLastError := Format('Unknown host ''%s''.'#$D#$A'Socket Error Code: %d($%X)', [Host, LastErrorCode, LastErrorCode]);
      Result := nil;
      Exit;
    end;

    if IsBroadcast then
      FBroadcastHostAddrInfo := HostAddrInfo
    else
      FHostAddrInfo := HostAddrInfo;
  end;

  Result := HostAddrInfo;
end;

function TCRVioUdp.InternalReadFrom(const Buffer: TValueArr; Offset, Count: integer): integer;
var
  HostAddrInfo: PAddrInfo;
  FromLen: {$IFDEF POSIX}socklen_t{$ELSE}Integer{$ENDIF};
begin
  FLastError := '';
  FLastErrorCode := 0;
  Result := 0;

  if SocketWaitForData(FSd, FReceiveTimeout * 1000, FLastErrorCode) then begin
    HostAddrInfo := CheckHostAddrInfo(True);
    Assert(HostAddrInfo <> nil);

    System.FillChar(HostAddrInfo.ai_addr^, HostAddrInfo.ai_addrlen, 0);
    FromLen := HostAddrInfo.ai_addrlen;

  {$IFDEF UNIX}
    Result := fprecvfrom(FSd, @Buffer[Offset], Count, 0, HostAddrInfo.ai_addr, @FromLen);
  {$ELSE}
    Result := recvfrom(FSd, Buffer[Offset], Count, 0, HostAddrInfo.ai_addr^, FromLen);
  {$ENDIF}
    HostAddrInfo.ai_addrlen := FromLen;

    if Result = SOCKET_ERROR then begin
      FLastErrorCode := GetSocketError;
      FLastError := Format('Error on data reading from the connection:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)', [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
    end;
  end;
end;

function TCRVioUdp.Read(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  Result := InternalReadFrom(Buffer, Offset, Count);
end;

function TCRVioUdp.ReadFromHost(const Buffer: TValueArr; Offset, Count: integer; out FromHost: boolean): integer;
var
  HostAddrInfo: PAddrInfo;
begin
  FromHost := False;
  Result := InternalReadFrom(Buffer, Offset, Count);

  if Result <= 0 then
    Exit;

  if FHostName <> '' then begin
    Assert(FBroadcastHostAddrInfo <> nil);
    HostAddrInfo := CheckHostAddrInfo(False);

    if HostAddrInfo <> nil then begin
      if HostAddrInfo.ai_family = AF_INET6 then
        FromHost := (PAddrInfo(FBroadcastHostAddrInfo).ai_addr.sa_family = HostAddrInfo.ai_addr.sa_family) and
          CompareMem(@PSockAddr_In6(PAddrInfo(FBroadcastHostAddrInfo).ai_addr).sin6_addr.s6_addr,
            @PSockAddr_In6(HostAddrInfo.ai_addr).sin6_addr.s6_addr, SizeOf(in6_addr))
      else
        FromHost := (PAddrInfo(FBroadcastHostAddrInfo).ai_addr.sa_family = HostAddrInfo.ai_addr.sa_family) and
          ({$IFDEF POSIX}Psockaddr_in{$ENDIF}(PAddrInfo(FBroadcastHostAddrInfo).ai_addr).sin_addr.s_addr = {$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_addr.s_addr);
    end;
  end;
end;

function TCRVioUdp.ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  Result := 0;
  Assert(False, 'Use ReadFrom instead');
end;

function TCRVioUdp.Write(const Buffer: TValueArr; Offset, Count: integer): integer;
var
  HostAddrInfo: PAddrInfo;
begin
  FLastError := '';
  FLastErrorCode := 0;
  Result := -1;

  HostAddrInfo := CheckHostAddrInfo(FIsBroadcast);
  if HostAddrInfo = nil then
    Exit;

  if FIsBroadcast then
    if HostAddrInfo.ai_family = AF_INET6 then
      System.FillChar(PSockAddr_In6(HostAddrInfo.ai_addr).sin6_addr, sizeof(in6_addr), $FF)
    else
      System.FillChar({$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_addr, sizeof(in_addr), $FF);
  {$IFDEF POSIX}Psockaddr_in{$ENDIF}(HostAddrInfo.ai_addr).sin_port := htons(FPort);

{$IFDEF UNIX}
  Result := fpsendto(FSd, @Buffer[Offset], Count, 0, HostAddrInfo.ai_addr, HostAddrInfo.ai_addrlen);
{$ELSE}
  Result := sendto(FSd, Buffer[Offset], Count, 0, HostAddrInfo.ai_addr^, HostAddrInfo.ai_addrlen);
{$ENDIF}
  if Result = SOCKET_ERROR then begin
    FLastErrorCode := GetSocketError;
    FLastError := Format('Error on data writing to the connection:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)', [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
  end;
end;

function TCRVioUdp.WriteNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  Result := 0;
  Assert(False, 'Use WriteTo instead');
end;

{$IFDEF MSWINDOWS}
initialization

finalization
  FreeWS2_32Lib;
{$ENDIF}

end.

