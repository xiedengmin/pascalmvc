//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRVioSocket;

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
  WinSock, Windows,
{$ENDIF}
{$IFDEF UNIX}
  sockets, baseunix, unix, netdb,
{$ENDIF}
  SyncObjs,
  CLRClasses, CRTypes, CRVio;

resourcestring
  SSocketError = 'Socket error';
  SSocketNotConnected = 'Socket not connected';
  SErrorOnDataWaiting = 'Error on data waiting from the connection:'#$D#$A'%s.'#$D#$A'Socket Error Code: %d($%X)';
  SCannotChangeHost = 'Cannot change host for the active connection';
  SCannotChangePort = 'Cannot change port for the active connection';
  SCannotChangeTcpVersion = 'Cannot change TCP/IP version for the active connection';

const
  LOCAL_HOST = 'localhost';
  DefaultTimeOut = 30;
  DefaultBuffer = 8192;

type
  TCRVioSocket = class(TCRVio)
  protected
    FBindAddress: string;
    FHostName: string;
    FPort: integer;
    FConnectionTimeout: integer;
    FSendTimeout: integer;
    FReceiveTimeout: integer;
    FSendBuffer: integer;
    FReceiveBuffer: integer;
    FIPVersion: TIPVersion;

    hLockSd: TCriticalSection;
    FSd: NativeInt;
    FAborted: boolean;

    procedure SetBindAddress(const Value: string);
    procedure SetHost(const Value: string);
  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(const Value: integer);
    procedure SetIPVersion(const Value: TIPVersion);

    function GetConnectionTimeout: integer; override;
    procedure SetConnectionTimeout(Value: integer); override;
    function GetSendTimeout: integer; override;
    procedure SetSendTimeout(Value: integer); override;
    function GetReceiveTimeout: integer; override;
    procedure SetReceiveTimeout(Value: integer); override;

    procedure SetSendBuffer(const Value: Integer);
    procedure SetReceiveBuffer(const Value: Integer);

    function GetConnected: boolean; override;
    procedure BeforeClosing; override;
    procedure InternalClose; override;

    procedure Init; virtual;
    procedure CloseSocket; virtual;

  public
    constructor Create; overload; virtual;
    constructor Create(const Hostname: string; Port: integer; IPVersion: TIPVersion = ivIPv4); overload; virtual;
    destructor Destroy; override;

    function WaitForData(MillisecondsTimeout: integer = -1): boolean; override;

    function GetSocket: NativeInt; virtual;
    property SendBuffer: integer read FSendBuffer write SetSendBuffer;
    property ReceiveBuffer: integer read FReceiveBuffer write SetReceiveBuffer;
    property BindAddress: string read FBindAddress write SetBindAddress;
    property Host: string read FHostName write SetHost;
    property Port: integer read FPort write SetPort;
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion;
  end;

procedure GetIPInterfaces(List: TStrings);

{$IFDEF MSWINDOWS}
var
  WsaData: TWSAData; // on windows winsock
  hLockWsaData: TCriticalSection;
{$ENDIF}

const
  {$EXTERNALSYM SD_SEND}
  SD_SEND = 1;
  {$EXTERNALSYM SD_BOTH}
  SD_BOTH = 2;
{$IFNDEF MSWINDOWS}
  SOCKET_ERROR = -1;
  {$EXTERNALSYM SOCKET_ERROR}
{$ENDIF}

implementation

uses
{$IFDEF POSIX}
  Posix.ArpaInet, Posix.Errno, Posix.Fcntl,
  Posix.NetDB, Posix.NetinetIn, Posix.NetinetTCP,
  Posix.SysSocket, Posix.UniStd,
  Posix.SysTime, Posix.SysSelect,
{$ENDIF}
  CRFunctions;

{$I CRSocketImpl.inc}

{ TCRVioSocket }

constructor TCRVioSocket.Create;
begin
  inherited Create;

  FHostName := '';
  FPort := 0;
  FIPVersion := ivIPv4;

  Init;
end;

constructor TCRVioSocket.Create(const Hostname: string; Port: integer; IPVersion: TIPVersion = ivIPv4);
begin
  inherited Create;

  FHostName := Hostname;
  FPort := Port;
  FIPVersion := IPVersion;

  Init;
end;

destructor TCRVioSocket.Destroy;
begin
  inherited;

  hLockSd.Free;
end;

procedure TCRVioSocket.Init;
begin
  FSd := SOCKET_ERROR;
  FBindAddress := '';
  FConnectionTimeout := 0;
  FReceiveTimeout := DefaultTimeOut;
  FSendTimeout := 0;
  FSendBuffer := DefaultBuffer;
  FReceiveBuffer := DefaultBuffer;
  FAborted := False;

  hLockSd := TCriticalSection.Create;
end;

function TCRVioSocket.GetConnected: boolean;
begin
  Result := FSd <> SOCKET_ERROR;
end;

procedure TCRVioSocket.SetBindAddress(const Value: string);
begin
  if GetConnected then begin
    FLastErrorCode := 0;
    FLastError := SCannotChangeHost;
    raise SocketException.Create(FLastError);
  end
  else
    FBindAddress := Value;
end;

procedure TCRVioSocket.SetHost(const Value: string);
begin
  if GetConnected then begin
    FLastErrorCode := 0;
    FLastError := SCannotChangeHost;
    raise SocketException.Create(FLastError);
  end
  else
    FHostName := Value;
end;

procedure TCRVioSocket.SetPort(const Value: integer);
begin
  if GetConnected then begin
    FLastErrorCode := 0;
    FLastError := SCannotChangePort;
    raise SocketException.Create(FLastError);
  end
  else
    FPort := Value;
end;

procedure TCRVioSocket.SetIPVersion(const Value: TIPVersion);
begin
  if GetConnected then begin
    FLastErrorCode := 0;
    FLastError := SCannotChangeTcpVersion;
    raise SocketException.Create(FLastError);
  end
  else
    FIPVersion := Value;
end;

function TCRVioSocket.GetConnectionTimeout: integer;
begin
  Result := FConnectionTimeout;
end;

procedure TCRVioSocket.SetConnectionTimeout(Value: integer);
begin
  FConnectionTimeout := Value;
end;

function TCRVioSocket.GetSendTimeout: integer;
begin
  Result := FSendTimeout;
end;

procedure TCRVioSocket.SetSendTimeout(Value: integer);
begin
  if Value <> FSendTimeout then begin
    FSendTimeout := Value;
    Set_SendTimeout(FSd, FSendTimeout);
  end;
end;

function TCRVioSocket.GetReceiveTimeout: integer;
begin
  Result := FReceiveTimeout;
end;

procedure TCRVioSocket.SetReceiveTimeout(Value: integer);
begin
  if Value <> FReceiveTimeout then begin
    FReceiveTimeout := Value;
    Set_ReceiveTimeout(FSd, FReceiveTimeout);
  end;
end;

procedure TCRVioSocket.SetSendBuffer(const Value: Integer);
begin
  if Value <> FSendBuffer then begin
    FSendBuffer := Value;
    Set_SendBuffer(FSd, FSendBuffer);
  end;
end;

procedure TCRVioSocket.SetReceiveBuffer(const Value: Integer);
begin
  if Value <> FReceiveBuffer then begin
    FReceiveBuffer := Value;
    Set_ReceiveBuffer(FSd, FReceiveBuffer);
  end;
end;

procedure TCRVioSocket.BeforeClosing;
begin
  SocketShutdown(FSd, {$IFNDEF LINUX}SD_SEND{$ELSE}SD_BOTH{$ENDIF});
end;

procedure TCRVioSocket.InternalClose;
begin
  CloseSocket;
end;

procedure TCRVioSocket.CloseSocket;
var
  Sd: NativeInt;
begin
  hLockSd.Acquire;
  try
    FAborted := True;

    if FSd <> SOCKET_ERROR then begin
      Sd := FSd;
      FSd := SOCKET_ERROR; // do not raise exception in the WaitForData method
      SocketClose(Sd);
    end;
  finally
    hLockSd.Release;
  end;
end;

function TCRVioSocket.WaitForData(MillisecondsTimeout: integer = -1): boolean;
begin
  if FSd = SOCKET_ERROR then
    raise SocketException.Create(SSocketNotConnected);

  FLastError := '';
  Result := SocketWaitForData(FSd, MillisecondsTimeout, FLastErrorCode);

  if FLastErrorCode <> 0 then begin
    if FSd <> SOCKET_ERROR then begin
      FLastError := Format(SErrorOnDataWaiting, [SysErrorMessage(FLastErrorCode), FLastErrorCode, FLastErrorCode]);
      raise SocketException.Create(FLastError, FLastErrorCode);
    end
    else
      Result := False;
  end;
end;

function TCRVioSocket.GetSocket: NativeInt;
begin
  Result := FSd;
end;

{$IFDEF MSWINDOWS}
initialization
  hLockWsaData := TCriticalSection.Create;
  ZeroMemory(@WsaData, SizeOf(WsaData));

finalization
  Cleanup;
  FreeWS2_32Lib;
  hLockWsaData.Free;
{$ENDIF}

end.
