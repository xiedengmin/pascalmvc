/////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRSecureConnection;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs, Types,
  CLRClasses, CRTypes, CRFunctions, CRVio, CRVioTcp;

type
  TScVersion = class(TPersistent)
  private
    FMajor: integer;
    FMinor: integer;
    FBuild: integer;
    FRevision: integer;

  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload;
    constructor Create(AMajor, AMinor: integer); overload;
    constructor Create(AMajor, AMinor, ABuild: integer); overload;
    constructor Create(AMajor, AMinor, ABuild, ARevision: integer); overload;

    function IsEqual(Obj: TScVersion): boolean;
    procedure Parse(const Value: string);
    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

  published
    property Major: integer read FMajor write FMajor default 0;
    property Minor: integer read FMinor write FMinor default 0;
    property Build: integer read FBuild write FBuild default -1;
    property Revision: integer read FRevision write FRevision default -1;
  end;

  TScNetworkCredential = class(TPersistent)
  private
    FDomain: string;
    FPassword: string;
    FUserName: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
  published
    property Domain: string read FDomain write FDomain;
    property Password: string read FPassword write FPassword;
    property UserName: string read FUserName write FUserName;
  end;

  TScWebProxy = class(TPersistent)
  private
    FAddress: string;
    FPort: integer;
    FCredentials: TScNetworkCredential;
    procedure SetCredentials(Value: TScNetworkCredential);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

  published
    property Address: string read FAddress write FAddress;
    property Port: integer read FPort write FPort default 8080;
    property Credentials: TScNetworkCredential read FCredentials write SetCredentials;
  end;

  { Pool }
const
  DefValMaxPoolSize = 100;
  StatisticsCount = 8;

type
  TScSecureConnectionParameters = class;
  TScSecureConnection = class;
  TScConnectionPool = class;
  TScConnectionPoolManager = class;
  TScConnectionsArray = array of TScSecureConnection;
  TIntegerArray = array of integer;
  TStatisticsArray = array [0..StatisticsCount-1] of integer;

  TScConnectionPool = class
  private
    FConnectionParameters: TScSecureConnectionParameters;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FManager: TScConnectionPoolManager;

    FPooledConnections: TScConnectionsArray;
    FPooledConnectionsCount, FHead, FTail: integer;
    FVersions: TIntegerArray;
    FVersion: integer;
    FStatistics: TStatisticsArray;
    FDoomedConnectionsCount: integer;
    FInvalidateVersion: integer;

    hBusy: TEvent;
    FLockPooled, FLockTaken, FLockVersion: TCriticalSection;

    function IsLive(Connection: TScSecureConnection): boolean;
    function CheckIsConnected(Connection: TScSecureConnection): boolean;
    procedure ReserveConnection;
    function InternalGetConnection(out Connection: TScSecureConnection; out Version: integer;
      Reserve: boolean = True): boolean;
    procedure InternalFreeConnection(var Connection: TScSecureConnection; Reserved: boolean = False);

  protected
    FTakenConnectionsCount: integer;

    procedure Validate;
    procedure Clear;
    function GetTotalConnectionsCount: integer;
    function InternalPutConnection(Connection: TScSecureConnection): boolean;
    procedure InternalReturnConnection(Connection: TScSecureConnection; Version: integer);

    function CreateNewConnector: TScSecureConnection;

  public
    constructor Create(Manager: TScConnectionPoolManager; ConnectionParameters: TScSecureConnectionParameters);
    destructor Destroy; override;

    function GetConnection: TScSecureConnection;
    function PutConnection(Connection: TScSecureConnection): boolean;
    procedure Invalidate;

    property TotalConnectionsCount: integer read GetTotalConnectionsCount;
    property Manager: TScConnectionPoolManager read FManager;
    property ConnectionParameters: TScSecureConnectionParameters read FConnectionParameters;
    property PooledConnectionsCount: integer read FPooledConnectionsCount;
  end;

  TScValidateThread = class(TThread)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FManager: TScConnectionPoolManager;
    FEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TScConnectionPoolManager);
    destructor Destroy; override;
    procedure Terminate;
  end;

  TScConnectionPoolManager = class
  private
    FPools: TCRObjectList;
    FValidateThread: TScValidateThread;

  protected
    FLockGet: TCriticalSection;
    FLockList: TCriticalSection;

  protected
    class function GetPoolManagerIndex: Integer;
    class procedure SetPoolManagerIndex(Value: Integer);
    function CreateConnectionPool(ConnectionParameters: TScSecureConnectionParameters): TScConnectionPool;
    function GetConnectionPool(ConnectionParameters: TScSecureConnectionParameters): TScConnectionPool;

    procedure InternalClear;
    function InternalGetConnection(ConnectionParameters: TScSecureConnectionParameters): TScSecureConnection;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetConnection(ConnectionParameters: TScSecureConnectionParameters): TScSecureConnection;
    class procedure Clear;
  end;

  { TScSecureConnectionParameters }

  TScSecureConnectionParameters = class(TPersistent)
  private
    FProxyOptions: TProxyOptions;
    FSSLOptions: TSSLOptions;

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    MinPoolSize: integer;
    MaxPoolSize: integer;
    ConnectionLifeTime: integer;

    ConnectionGroupName: string;
    ProviderName: string;
    BindAddress: string;
    Hostname: string;
    Port: integer;
    IPVersion: TIPVersion;
    Timeout: integer;
    IsSecure: boolean;
    IOHandler: TCRIOHandler;

    constructor Create;
    destructor Destroy; override;
    function Equals(ConnectionParameters: TScSecureConnectionParameters): boolean; reintroduce;

    property ProxyOptions: TProxyOptions read FProxyOptions;
    property SSLOptions: TSSLOptions read FSSLOptions;
  end;

  { TScSecureConnection }

  TScSecureConnection = class
  private
    FRefCount: integer;

    FTmpBuffer: TBytes;

    FReadBuffer: TBytes;
    FReadPos: integer;
    FWritePos: integer;

    FLock: TCriticalSection;
    FVio: TCRVio;
    FIsSecure: boolean;
    FConnectionTime: cardinal;
    FPoolVersion: integer;
    FPool: TScConnectionPool;
    FIsConnected: boolean;
    FIsDisconnected: boolean;

    FOnAsyncReceive: TNotifyEvent;

    procedure SetOnAsyncReceive(Value: TNotifyEvent);
    function GetAfterDisconnect: TNotifyEvent;
    procedure SetAfterDisconnect(Value: TNotifyEvent);
    function GetIsSecure: boolean;
    procedure SetIsSecure(Value: boolean);

    procedure DefragBuffer;
    function CopyData(const Buffer: TValueArr; Offset, Count: integer): integer;

    procedure CreateSSLIOHandler(ConnectionParameters: TScSecureConnectionParameters);

  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRef;
    procedure Release;


    procedure CreateVio(ConnectionParameters: TScSecureConnectionParameters);
    procedure InitVio(ConnectionParameters: TScSecureConnectionParameters; AVio: TCRVioTcp);
    procedure Bind(const BindAddress: string);
    procedure Connect;
    procedure Abort;
    procedure Disconnect;
    function CheckIsConnected: boolean;
    function IsValid: boolean;
    procedure TryReturnToPool;

    function Write(const Buffer: TValueArr; Offset, Count: integer): integer; overload;
    procedure Write(const Buffer: TBytes); overload;
    procedure WriteLine(const Str: string; AEncoding: Encoding = nil);

    function ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
    function Read(const Buffer: TValueArr; Offset, Count: integer): integer;
    function WaitForData(MillisecondsTimeout: integer): boolean;

    procedure ClearBuffer;
    function ReadLine(CancellationToken: TScCancellationToken): string; overload;
    function ReadLine(AEncoding: Encoding = nil; CancellationToken: TScCancellationToken = nil): string; overload;
    function CheckForDataOnSource(NeedCount: integer; TimeoutMSec: integer = -1): integer;

    procedure SetReadWriteTimeout(Value: integer);
    procedure SetSocketOption(OptionLevel, OptionName, OptionValue: integer);
    procedure RaiseLastError;

    procedure Renegotiate;

    function GetLocalIP: string;
    function GetLocalPort: integer;
    function GetRemoteIP: string;
    function GetRemotePort: integer;

    property IsConnected: boolean read FIsConnected;
    property IsSecure: boolean read GetIsSecure write SetIsSecure;

    property OnAsyncReceive: TNotifyEvent read FOnAsyncReceive write SetOnAsyncReceive;
    property AfterDisconnect: TNotifyEvent read GetAfterDisconnect write SetAfterDisconnect;

  end;

  TScSecureConnectionStream = class(TStream)
  private
    FConnection: TScSecureConnection;
  protected
    function GetSize: Int64; {$IFDEF VER7P}override;{$ENDIF}{$IFDEF FPC}override;{$ENDIF}
  public
    constructor Create(AOwner: TScSecureConnection);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TScHttpParser = class
  private
    class function ParseFragment(var ParseString: string): string;
    class function ParseNetworkLocation(var ParseString: string): string;
    class function ParseQuery(var ParseString: string): string;
    class function ParseParameters(var ParseString: string): string;
    class function ParseResource(var ParseString: string): string;
    class function ParsePath(var ParseString: string): string;
    class function ParsePassword(var ParseString: string): string;
    class function ParseUserPassword(var ParseString: string): string;
    class function ParsePort(var ParseString: string): string;
    class function GetToEnd(const FindChar: Char; var ParseString: string; const KeepFirst: Boolean): string;

  public
    class function ParseScheme(var ParseString: string): string;
    class procedure ParseURL(const URL: string; out Scheme, User, Password,
      NetworkLocation, Port, Path, Resource, Parameters, Query, Fragment: string);
    class function NthWord(const InputString: string; const Delimiter: Char; Number: integer): string;
    class function WordsCount(const InputString: string; const Delimiter: Char): integer;
    class procedure ParseKeyValue(const Token: string; out Key, Value: string);
    class function ParseTokens(const Tokens: string; const Delimiter: Char): TStringArray;
    class function FindToken(const Token: string; const Tokens: string; const Delimiter: Char): integer;
  end;

const
  SEND_BLOCK_SIZE = 32 * 1024;
  READ_BUFFER_SIZE = 32 * 1024;
  UNBUFFERED_READ_MIN_SIZE = 2048;

implementation

resourcestring
  SConnectionActive = 'Secure connection is already created';
  SInvalidInputArgs = 'Invalid input arguments';
  SInvalidVersion = 'Invalid version format';
  SMaxConnectionsReached  = 'Maximum connections reached in pool';
  SNotEnoughData = 'There is not enough data to read';
  SSecureConnectionNotAvailable = 'Secure connection is not available';
  SConnectionClosed = 'Error on data reading from the connection: '#$D#$A'An existing connection was forcibly closed by the remote host';
  SReadNoWaitInternalError = 'ReadNoWait internal error';

var
  CreateLock: TCriticalSection;
  PoolManagerList: TCRObjectList;
  PoolManagerIndex: integer = -1;

{ TScVersion }

constructor TScVersion.Create;
begin
  inherited Create;

  FMajor := 0;
  FMinor := 0;
  FBuild := -1;
  FRevision := -1;
end;

constructor TScVersion.Create(AMajor, AMinor: integer);
begin
  inherited Create;

  FMajor := AMajor;
  FMinor := AMinor;
  FBuild := -1;
  FRevision := -1;
end;

constructor TScVersion.Create(AMajor, AMinor, ABuild: integer);
begin
  inherited Create;

  FMajor := AMajor;
  FMinor := AMinor;
  FBuild := ABuild;
  FRevision := -1;
end;

constructor TScVersion.Create(AMajor, AMinor, ABuild, ARevision: integer);
begin
  inherited Create;

  FMajor := AMajor;
  FMinor := AMinor;
  FBuild := ABuild;
  FRevision := ARevision;
end;

procedure TScVersion.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScVersion) then begin
    TScVersion(Dest).FMajor := FMajor;
    TScVersion(Dest).FMinor := FMinor;
    TScVersion(Dest).FBuild := FBuild;
    TScVersion(Dest).FRevision := FRevision;
  end
  else
    inherited;
end;

function TScVersion.IsEqual(Obj: TScVersion): boolean;
begin
  Result := (Obj.FMajor = FMajor) and (Obj.FMinor = FMinor) and
    (Obj.FBuild = FBuild) and (Obj.FRevision = FRevision);
end;

function TScVersion.ToString: string;
begin
  Result := IntToStr(FMajor) + '.' + IntToStr(FMinor);
  if FBuild > -1 then
    Result := Result + '.' + IntToStr(FBuild);
  if FRevision > -1 then
    Result := Result + '.' + IntToStr(FRevision);
end;

procedure TScVersion.Parse(const Value: string);
var
  Pos: array[1..4] of integer;
  DotCount: integer;
  i: integer;
begin
  DotCount := 0;
  for i := 1 to Length(Value) do begin
    if Value[i] = '.' then begin
      Inc(DotCount);
      if DotCount > 3 then
        raise EConvertError.CreateRes(@SInvalidVersion);
      Pos[DotCount] := i;
      Pos[DotCount + 1] := Length(Value) + 1;
    end
    else
    if not CharInSet(Value[i], ['0'..'9']) then
      raise EConvertError.CreateRes(@SInvalidVersion);
  end;

  if DotCount = 0 then
    raise EConvertError.CreateRes(@SInvalidVersion);

  if not TryStrToInt(Copy(Value, 1, Pos[1] - 1), FMajor) then
    raise EConvertError.CreateRes(@SInvalidVersion);
  if not TryStrToInt(Copy(Value, Pos[1] + 1, Pos[2] - Pos[1] - 1), FMinor) then
    raise EConvertError.CreateRes(@SInvalidVersion);

  if DotCount >= 2 then begin
    if not TryStrToInt(Copy(Value, Pos[2] + 1, Pos[3] - Pos[2] - 1), FBuild) then
      raise EConvertError.CreateRes(@SInvalidVersion);
  end
  else
    FBuild := -1;

  if DotCount = 3 then begin
    if not TryStrToInt(Copy(Value, Pos[3] + 1, Length(Value) - Pos[3]), FRevision) then
      raise EConvertError.CreateRes(@SInvalidVersion);
  end
  else
    FRevision := -1;
end;

{ TScNetworkCredential }

procedure TScNetworkCredential.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScNetworkCredential) then begin
    TScNetworkCredential(Dest).FDomain := FDomain;
    TScNetworkCredential(Dest).FPassword := FPassword;
    TScNetworkCredential(Dest).FUserName := FUserName;
  end
  else
    inherited;
end;

{ TScWebProxy }

constructor TScWebProxy.Create;
begin
  inherited;

  FCredentials := TScNetworkCredential.Create;
  FPort := 8080;
end;

destructor TScWebProxy.Destroy;
begin
  FCredentials.Free;
  inherited;
end;

procedure TScWebProxy.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScWebProxy) then begin
    TScWebProxy(Dest).FAddress := FAddress;
    TScWebProxy(Dest).FPort := FPort;
    TScWebProxy(Dest).FCredentials.Assign(FCredentials);
  end
  else
    inherited;
end;

procedure TScWebProxy.SetCredentials(Value: TScNetworkCredential);
begin
  FCredentials.Assign(Value);
end;

{ TScConnectionPool }

constructor TScConnectionPool.Create(Manager: TScConnectionPoolManager; ConnectionParameters: TScSecureConnectionParameters);
begin
  inherited Create;

  FConnectionParameters := TScSecureConnectionParameters.Create;
  FConnectionParameters.Assign(ConnectionParameters);
  FManager := Manager;

  SetLength(FPooledConnections, Self.ConnectionParameters.MaxPoolSize);
  SetLength(FVersions, Self.ConnectionParameters.MaxPoolSize);
  hBusy := CreateEvent(True);
  FLockPooled := TCriticalSection.Create;
  FLockTaken := TCriticalSection.Create;
  FLockVersion := TCriticalSection.Create;
end;

destructor TScConnectionPool.Destroy;
begin
  Clear;

  FLockVersion.Free;
  FLockTaken.Free;
  FLockPooled.Free;
  hBusy.Free;
  FConnectionParameters.Free;

  inherited;
end;

function TScConnectionPool.GetTotalConnectionsCount: integer;
begin
  FLockPooled.Enter;
  try
    FLockTaken.Enter;
    try
      Result := FTakenConnectionsCount + FPooledConnectionsCount;
    finally
      FLockTaken.Leave;
    end;
  finally
    FLockPooled.Leave;
  end;
end;

procedure TScConnectionPool.Invalidate;
begin
  Inc(FInvalidateVersion);
end;

procedure TScConnectionPool.Validate;
var
  Connection: TScSecureConnection;
  i, FirstVersion, LastVersion, Doomed, Removed, Version: integer;
begin
  FirstVersion := FStatistics[0];
  LastVersion := FStatistics[StatisticsCount - 1];
  for i := StatisticsCount - 1 downto 1 do
    FStatistics[i] := FStatistics[i - 1];
  FStatistics[0] := FVersion;
  Doomed := (FDoomedConnectionsCount + StatisticsCount - 2) div StatisticsCount;
  FDoomedConnectionsCount := FPooledConnectionsCount - ConnectionParameters.MinPoolSize - Doomed;

  i := FTail;
  Removed := 0;
  while (FHead <> i) and InternalGetConnection(Connection, Version) do begin
    if (Version <= LastVersion) or not IsLive(Connection) or
      ((Version <= FirstVersion) or (Connection.FPoolVersion < FInvalidateVersion))
      and not CheckIsConnected(Connection)
    then begin
      InternalFreeConnection(Connection);
      Inc(Removed);
    end
    else
      InternalReturnConnection(Connection, Version);
  end;

  if Removed < Doomed then begin
    Doomed := Doomed - Removed;
    for i := 0 to Doomed - 1 do
      if InternalGetConnection(Connection, Version) then
        InternalFreeConnection(Connection)
      else
        break;
  end;
end;

procedure TScConnectionPool.Clear;
var
  Connection: TScSecureConnection;
  Version: integer;
begin
  while InternalGetConnection(Connection, Version) do
    InternalFreeConnection(Connection);
end;

function TScConnectionPool.PutConnection(Connection: TScSecureConnection): boolean;
begin
  Result := InternalPutConnection(Connection);
end;

function TScConnectionPool.IsLive(Connection: TScSecureConnection): boolean;
var
  LifeTime: Cardinal;
begin
  Result := FConnectionParameters.ConnectionLifeTime = 0;
  if Result then // If connector life time is zero then does not remove connector
    Exit;

  LifeTime := GetTickInterval(Connection.FConnectionTime, GetTickCount);
  Result := LifeTime <= Cardinal(FConnectionParameters.ConnectionLifeTime);
end;

function TScConnectionPool.CheckIsConnected(Connection: TScSecureConnection): boolean;
begin
  Result := Connection.CheckIsConnected;
  Connection.FPoolVersion := FInvalidateVersion;
end;

procedure TScConnectionPool.ReserveConnection;
begin
  Inc(FTakenConnectionsCount);
  if FTakenConnectionsCount >= ConnectionParameters.MaxPoolSize then
    hBusy.ResetEvent;
end;

function TScConnectionPool.InternalGetConnection(out Connection: TScSecureConnection;
  out Version: integer; Reserve: boolean = True): boolean;
begin
  FLockPooled.Enter;
  try
    if Reserve then
      FLockTaken.Enter;
    try
      Result := False;
      Connection := nil;
      if not Reserve or (FTakenConnectionsCount < ConnectionParameters.MaxPoolSize) then
        if FPooledConnectionsCount > 0 then begin
          Connection := FPooledConnections[FHead];
          FPooledConnections[FHead] := nil;

          Version := FVersions[FHead];
          Inc(FHead);
          if FHead = ConnectionParameters.MaxPoolSize then
            FHead := 0;
          Dec(FPooledConnectionsCount);
          if Reserve then
            ReserveConnection;
          Result := True;
        end;
    finally
      if Reserve then
        FLockTaken.Leave;
    end;
  finally
    FLockPooled.Leave;
  end;
end;

procedure TScConnectionPool.InternalFreeConnection(var Connection: TScSecureConnection; Reserved: boolean = False);
begin
  try
    try
      Connection.Disconnect; // for NEXTGEN
    finally
      Connection.Release;
      Connection := nil;
    end;
  except
  end;

  if not Reserved then begin
    FLockTaken.Enter;
    try
      Dec(FTakenConnectionsCount);
      hBusy.SetEvent;
    finally
      FLockTaken.Leave;
    end;
  end;
end;

function TScConnectionPool.CreateNewConnector: TScSecureConnection;
begin
  Result := TScSecureConnection.Create;
  try
    Result.CreateVio(ConnectionParameters);
  except
    Result.Free;
    raise;
  end;
end;

function TScConnectionPool.GetConnection: TScSecureConnection;
const
{$IFDEF UNIX}
  Timeout: Cardinal = $FFFFFFFF;
{$ELSE}
  Timeout: Cardinal = 30000;
{$ENDIF}
var
  Version: integer;
begin
  if hBusy.WaitFor(Timeout) = wrTimeout then
    raise Exception.Create(SMaxConnectionsReached);

  FLockTaken.Enter;
  try
    if FTakenConnectionsCount < ConnectionParameters.MaxPoolSize then
      ReserveConnection
    else
      raise Exception.Create(SMaxConnectionsReached);
  finally
    FLockTaken.Leave;
  end;

  if InternalGetConnection(Result, Version, False) then
    if (Result.FPoolVersion < FInvalidateVersion) and not CheckIsConnected(Result) then
      InternalFreeConnection(Result, True);

  if Result = nil then
    Result := CreateNewConnector;

  Result.FPool := Self;
  Result.FPoolVersion := FInvalidateVersion;
end;

function TScConnectionPool.InternalPutConnection(Connection: TScSecureConnection): boolean;
var
  Version: integer;
begin
  Assert(Connection.FPool = Self);
  Connection.FPool := nil; // protection from PutConnection call on already pooled connection
  Result := False;

  if not IsLive(Connection) or not Connection.IsConnected or
    (Connection.FPoolVersion < FInvalidateVersion) and not CheckIsConnected(Connection)
  then
    InternalFreeConnection(Connection)
  else begin
    FLockVersion.Enter;
    try
      Inc(FVersion);
      Version := FVersion;
    finally
      FLockVersion.Leave;
    end;
    InternalReturnConnection(Connection, Version);
    Result := True;
  end;
end;

procedure TScConnectionPool.InternalReturnConnection(Connection: TScSecureConnection; Version: integer);
begin
  FLockPooled.Enter;
  try
    FPooledConnections[FTail] := Connection;
    FVersions[FTail] := Version;
    Inc(FTail);
    if FTail = ConnectionParameters.MaxPoolSize then
      FTail := 0;
    Inc(FPooledConnectionsCount);

    FLockTaken.Enter;
    try
      Dec(FTakenConnectionsCount);
      hBusy.SetEvent;
    finally
      FLockTaken.Leave;
    end;
  finally
    FLockPooled.Leave;
  end;
end;

{ TScConnectionPoolManager }

constructor TScConnectionPoolManager.Create;
begin
  inherited;

  FPools := TCRObjectList.Create;
  FLockGet := TCriticalSection.Create;
  FLockList := TCriticalSection.Create;
  FValidateThread := TScValidateThread.Create(Self);

  SetPoolManagerIndex(PoolManagerList.Add(Self));
end;

destructor TScConnectionPoolManager.Destroy;
begin
  SetPoolManagerIndex(-1);
  PoolManagerList.Remove(Self);

  if FValidateThread <> nil then begin
    FValidateThread.Terminate;
    FValidateThread.WaitFor;
    FValidateThread.Free;
  end;

  if (FPools <> nil) and (FLockGet <> nil) and (FLockList <> nil) then
    InternalClear;
  FPools.Free;
  FLockGet.Free;
  FLockList.Free;

  inherited;
end;

class function TScConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TScConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

// Conn parameters used for creating new pool with initial parameters
function TScConnectionPoolManager.GetConnectionPool(ConnectionParameters: TScSecureConnectionParameters): TScConnectionPool;
var
  i: integer;
  Pool: TScConnectionPool;
begin
  Result := nil;

  // Search if pool with same connection string exist
  for i := 0 to FPools.Count - 1 do begin
    Pool := TScConnectionPool(FPools.Items[i]);
    if Pool.FConnectionParameters.Equals(ConnectionParameters) then begin
      Result := Pool;
      break;
    end;
  end;

  // Create new pool object if existing not found
  if Result = nil then begin
    Result := CreateConnectionPool(ConnectionParameters);
    FPools.Add(Result);
  end;
end;

procedure TScConnectionPoolManager.InternalClear;
begin
  FLockList.Enter;
  try
    FPools.Clear;
  finally
    FLockList.Leave;
  end;
end;

function TScConnectionPoolManager.InternalGetConnection(ConnectionParameters: TScSecureConnectionParameters): TScSecureConnection;
begin
  FLockGet.Enter;
  try
    Result := GetConnectionPool(ConnectionParameters).GetConnection;
  finally
    FLockGet.Leave;
  end;
end;

class function TScConnectionPoolManager.GetConnection(ConnectionParameters: TScSecureConnectionParameters): TScSecureConnection;
var
  Index: Integer;
  PoolManager: TScConnectionPoolManager;
begin
  CreateLock.Enter;
  try
    Index := GetPoolManagerIndex;
    if Index = -1 then
      PoolManager := Self.Create
    else
      PoolManager := TScConnectionPoolManager(PoolManagerList[Index]);
  finally
    CreateLock.Leave;
  end;

  Result := PoolManager.InternalGetConnection(ConnectionParameters);
end;

class procedure TScConnectionPoolManager.Clear;
var
  Index: Integer;
  PoolManager: TScConnectionPoolManager;
begin
  Index := GetPoolManagerIndex;
  if Index <> -1 then begin
    PoolManager := TScConnectionPoolManager(PoolManagerList[Index]);
    PoolManager.InternalClear;
  end;
end;

function TScConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TScSecureConnectionParameters): TScConnectionPool;
begin
  Result := TScConnectionPool.Create(Self, ConnectionParameters);
end;

{ TScValidateThread }

constructor TScValidateThread.Create(Manager: TScConnectionPoolManager);
begin
  FManager := Manager;
  FEvent := CreateEvent;

  inherited Create(False);
end;

destructor TScValidateThread.Destroy;
begin
  inherited;
  FEvent.Free;
end;

procedure TScValidateThread.Terminate;
begin
  inherited;
  FEvent.SetEvent;
end;

procedure TScValidateThread.Execute;
const
  Timeout = 30000;
var
  i, Count: integer;
  Pool: TScConnectionPool;
  Ticks, BeginTickCount: cardinal;
begin
  try
    Ticks := 0;
    while not Terminated do begin
      if (Ticks < Timeout) and (FEvent.WaitFor(Timeout - Ticks) = wrSignaled) then
        Exit;

      BeginTickCount := GetTickCount;

      FManager.FLockList.Enter;
      try
        for i := FManager.FPools.Count - 1 downto 0 do begin
          if Terminated then
            Exit;
          Pool := TScConnectionPool(FManager.FPools[i]);
          Pool.Validate;

          FManager.FLockGet.Enter;
          try
            Count := Pool.TotalConnectionsCount;
            if Count = 0 then
              FManager.FPools.Delete(i);
          finally
            FManager.FLockGet.Leave;
          end;
        end;
      finally
        FManager.FLockList.Leave;
      end;

      Ticks := GetTickInterval(BeginTickCount, GetTickCount);
    end;
  except
  end;
end;

{ TScSecureConnectionParameters }

constructor TScSecureConnectionParameters.Create;
begin
  inherited Create;

  MaxPoolSize := DefValMaxPoolSize;
  FProxyOptions := TProxyOptions.Create;
  FSSLOptions := TSSLOptions.Create;
end;

destructor TScSecureConnectionParameters.Destroy;
begin
  FProxyOptions.Free;
  FSSLOptions.Free;
  inherited;
end;

function TScSecureConnectionParameters.Equals(ConnectionParameters: TScSecureConnectionParameters): boolean;
begin
  Result := False;
  if ConnectionParameters <> nil then
    Result :=
      (MinPoolSize = ConnectionParameters.MinPoolSize) and
      (MaxPoolSize = ConnectionParameters.MaxPoolSize) and
      (ConnectionLifeTime = ConnectionParameters.ConnectionLifeTime) and
      AnsiSameText(ConnectionGroupName, ConnectionParameters.ConnectionGroupName) and
      AnsiSameText(BindAddress, ConnectionParameters.BindAddress) and
      AnsiSameText(Hostname, ConnectionParameters.Hostname) and
      (Port = ConnectionParameters.Port) and
      (IPVersion = ConnectionParameters.IPVersion) and
      AnsiSameText(ProxyOptions.Hostname, ConnectionParameters.ProxyOptions.Hostname) and
      (ProxyOptions.Port = ConnectionParameters.ProxyOptions.Port) and
      AnsiSameText(ProxyOptions.Username, ConnectionParameters.ProxyOptions.Username) and
      AnsiSameText(ProxyOptions.Password, ConnectionParameters.ProxyOptions.Password) and
      AnsiSameText(ProviderName, ConnectionParameters.ProviderName) and
      (IsSecure = ConnectionParameters.IsSecure);
end;

procedure TScSecureConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TScSecureConnectionParameters then begin
    TScSecureConnectionParameters(Dest).MinPoolSize := MinPoolSize;
    TScSecureConnectionParameters(Dest).MaxPoolSize := MaxPoolSize;
    TScSecureConnectionParameters(Dest).ConnectionLifeTime := ConnectionLifeTime;

    TScSecureConnectionParameters(Dest).ConnectionGroupName := ConnectionGroupName;
    TScSecureConnectionParameters(Dest).ProviderName := ProviderName;
    TScSecureConnectionParameters(Dest).BindAddress := BindAddress;
    TScSecureConnectionParameters(Dest).Hostname := Hostname;
    TScSecureConnectionParameters(Dest).Port := Port;
    TScSecureConnectionParameters(Dest).IPVersion := IPVersion;
    TScSecureConnectionParameters(Dest).Timeout := Timeout;
    TScSecureConnectionParameters(Dest).IsSecure := IsSecure;
    TScSecureConnectionParameters(Dest).ProxyOptions.Assign(ProxyOptions);
    TScSecureConnectionParameters(Dest).SSLOptions.Assign(SSLOptions);
    TScSecureConnectionParameters(Dest).IOHandler := IOHandler;
  end
  else
    inherited;
end;

{ TScSecureConnection }

constructor TScSecureConnection.Create;
begin
  inherited;

  FLock := TCriticalSection.Create;
  SetLength(FReadBuffer, READ_BUFFER_SIZE);
  SetLength(FTmpBuffer, READ_BUFFER_SIZE);

  AddRef;
end;

destructor TScSecureConnection.Destroy;
begin
  try
    Disconnect;
  except
  end;

  FVio.Free;
  FLock.Free;

  inherited;
end;

procedure TScSecureConnection.AddRef;
begin
{$IFDEF AUTOREFCOUNT}
  __ObjAddRef;
{$ENDIF}

  Inc(FRefCount);
end;

procedure TScSecureConnection.Release;
begin
  if Assigned(Self) then begin
    Assert(FRefCount > 0, ClassName + '.Free RefCount = ' + IntToStr(FRefCount));

    if FRefCount = 1 then
    {$IFNDEF AUTOREFCOUNT}
      Free
    {$ENDIF}
    else
      Dec(FRefCount);

  {$IFDEF AUTOREFCOUNT}
    __ObjRelease;
  {$ENDIF}
  end;
end;

procedure TScSecureConnection.SetOnAsyncReceive(Value: TNotifyEvent);
begin
  if @Value <> @FOnAsyncReceive then begin
    FOnAsyncReceive := Value;

    if FVio <> nil then begin
      FVio.OnAsyncReceive := FOnAsyncReceive;
      FVio.NonBlocking := Assigned(FOnAsyncReceive);
    end;
  end;
end;

function TScSecureConnection.GetIsSecure: boolean;
begin
  if FIsSecure then
    Result := (FVio <> nil) and (FVio as TCRVioHandler).IsSecure
  else
    Result := False;
end;

procedure TScSecureConnection.SetIsSecure(Value: boolean);
begin
  if FIsSecure then begin
    Assert(FVio <> nil);
    (FVio as TCRVioHandler).IsSecure := Value;
  end;
end;

procedure TScSecureConnection.CreateSSLIOHandler(ConnectionParameters: TScSecureConnectionParameters);
begin
  if ConnectionParameters.IOHandler = nil then
    raise Exception.Create(SSecureConnectionNotAvailable);

  FVio := TCRVioHandler.Create(ConnectionParameters.Hostname, ConnectionParameters.Port,
    ConnectionParameters.IOHandler, nil, nil, ConnectionParameters.SSLOptions, nil, ConnectionParameters.IPVersion);
end;

procedure TScSecureConnection.CreateVio(ConnectionParameters: TScSecureConnectionParameters);
begin
  if FVio <> nil then
    raise Exception.Create(SConnectionActive);

  FIsDisconnected := False;

  FLock.Acquire;
  try
    FIsSecure := ConnectionParameters.IsSecure;

    if FIsSecure then begin
      CreateSSLIOHandler(ConnectionParameters);
    end
    else
      FVio := TCRVioTcp.Create(ConnectionParameters.ProxyOptions, ConnectionParameters.ProviderName,
        ConnectionParameters.Hostname, ConnectionParameters.Port, ConnectionParameters.IPVersion);

    SetReadWriteTimeout(ConnectionParameters.Timeout);
    Bind(ConnectionParameters.BindAddress);
  finally
    FLock.Release;
  end;
end;

procedure TScSecureConnection.InitVio(ConnectionParameters: TScSecureConnectionParameters; AVio: TCRVioTcp);
begin
  if AVio = nil then
    raise Exception.Create(SInvalidInputArgs);

  if FVio <> nil then
    raise Exception.Create(SConnectionActive);

  FIsDisconnected := False;

  FLock.Acquire;
  try
    FIsSecure := ConnectionParameters.IsSecure;

    if FIsSecure then begin
      CreateSSLIOHandler(ConnectionParameters);
    end
    else
      FVio := AVio;

    SetReadWriteTimeout(ConnectionParameters.Timeout);
  finally
    FLock.Release;
  end;
end;

procedure TScSecureConnection.Bind(const BindAddress: string);
begin
  if BindAddress = '' then
    Exit;

  if (FVio <> nil) and (FVio is TCRVioTcp) then begin
    TCRVioTcp(FVio).BindAddress := BindAddress;
    TCRVioTcp(FVio).Bind;
  end;
end;

procedure TScSecureConnection.Connect;
begin
  FConnectionTime := GetTickCount;

  if FVio <> nil then
    FVio.Connect;

  FIsDisconnected := False;
  FIsConnected := True;
end;

procedure TScSecureConnection.Disconnect;
begin
  FLock.Acquire;
  try
    if FIsDisconnected then
      Exit;

    FIsDisconnected := True;
    FIsConnected := False;
  finally
    FLock.Release;
  end;

  if (FPool <> nil) and (PoolManagerList <> nil) then begin
    FPool.Invalidate;
    FPool.PutConnection(Self);
  end
  else
  if FVio <> nil then
    FVio.Close;
end;

procedure TScSecureConnection.Abort;
begin
  FLock.Acquire;
  try
    if FIsDisconnected then
      Exit;

    FIsDisconnected := True;
    FIsConnected := False;
  finally
    FLock.Release;
  end;

  if FVio <> nil then
    FVio.Close;
end;

function TScSecureConnection.IsValid: boolean;
begin
    Result := not FIsDisconnected and (FVio <> nil);
end;

function TScSecureConnection.CheckIsConnected: boolean;
begin
    FIsConnected := not FIsDisconnected and (FVio <> nil) and FVio.Connected;
  Result := FIsConnected;
end;

function TScSecureConnection.Write(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  begin
    Assert(FVio <> nil);
    Result := FVio.Write(Buffer, Offset, Count);
    if FVio.LastErrorCode <> 0 then
      raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
  end;
end;

procedure TScSecureConnection.Write(const Buffer: TBytes);
begin
  begin
    Assert(FVio <> nil);
    FVio.Write(TValueArr(Buffer), 0, Length(Buffer));
    if FVio.LastErrorCode <> 0 then
      raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
  end;
end;

procedure TScSecureConnection.WriteLine(const Str: string; AEncoding: Encoding = nil);
var
  StrLen, Count: integer;
begin
  StrLen := Length(Str) + 2{CRLF};
  if StrLen * 4 > Length(FTmpBuffer) then
    SetLength(FTmpBuffer, StrLen * 4);

  if AEncoding = nil then
    AEncoding := Encoding.Default;

  Count := AEncoding.GetBytes(Str + #13#10, 1, StrLen, FTmpBuffer, 0);
  Write(TValueArr(FTmpBuffer), 0, Count);
end;

function TScSecureConnection.ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  begin
    Assert(FVio <> nil);
    Result := FVio.ReadNoWait(Buffer, Offset, Count);
  end;
end;

function TScSecureConnection.WaitForData(MillisecondsTimeout: integer): boolean;
begin
    Result := (FVio <> nil) and FVio.Connected and FVio.WaitForData(MillisecondsTimeout);
end;

procedure TScSecureConnection.ClearBuffer;
begin
  SetLength(FReadBuffer, READ_BUFFER_SIZE);
  FReadPos := 0;
  FWritePos := 0;
end;

procedure TScSecureConnection.DefragBuffer;
begin
  if FWritePos = FReadPos then begin
    FReadPos := 0;
    FWritePos := 0;
  end
  else
  if FReadPos >= READ_BUFFER_SIZE then begin // performance increase
    Move(FReadBuffer[FReadPos], FReadBuffer[0], FWritePos - FReadPos);
    Dec(FWritePos, FReadPos);
    FReadPos := 0;
  end;
end;

function TScSecureConnection.ReadLine(CancellationToken: TScCancellationToken): string;
begin
  Result := ReadLine(nil, CancellationToken);
end;

function TScSecureConnection.ReadLine(AEncoding: Encoding = nil;
  CancellationToken: TScCancellationToken = nil): string;
var
  FindPos, TmpPos: integer;
  ReadCount, Count: integer;
  Prev: byte;
  i: integer;
begin
  Result := '';
  if not IsValid then
    Exit;

  Prev := 0;
  TmpPos := FReadPos;
  FindPos := -1;

  repeat
    for i := TmpPos to FWritePos - 1 do begin
      if (Prev = 13) and (FReadBuffer[i] = 10) then begin
        FindPos := i;
        Break;
      end;
      Prev := FReadBuffer[i];
    end;
    TmpPos := FWritePos;

    if FindPos < 0 then begin
      if FWritePos >= Length(FReadBuffer) then
        SetLength(FReadBuffer, Length(FReadBuffer) + READ_BUFFER_SIZE);

      ReadCount := ReadNoWait(@FReadBuffer[0], FWritePos, Length(FReadBuffer) - FWritePos);
      if ReadCount > 0 then
        Inc(FWritePos, ReadCount)
      else
        RaiseLastError;

      if CancellationToken <> nil then
        CancellationToken.ThrowIfCancellationRequested;
    end;
  until FindPos >= 0;

  Count := FindPos - FReadPos + 1;

  if AEncoding = nil then
    AEncoding := Encoding.Default;
  Result := AEncoding.GetString(FReadBuffer, FReadPos, Count - 2{CRLF});

  Inc(FReadPos, Count);
  DefragBuffer;
end;

function TScSecureConnection.CheckForDataOnSource(NeedCount: integer;
  TimeoutMSec: integer = -1{don't wait}): integer;
var
  NeedRead: boolean;
  ReadCount: integer;
begin
  if (NeedCount > 0) and ((FWritePos - FReadPos) < NeedCount) then begin
    if (FWritePos + NeedCount) > Length(FReadBuffer) then
      SetLength(FReadBuffer, Length(FReadBuffer) + READ_BUFFER_SIZE);

    if TimeoutMSec >= 0 then
      NeedRead := WaitForData(TimeoutMSec)
    else
      NeedRead := True;

    if NeedRead then begin
      ReadCount := ReadNoWait(@FReadBuffer[0], FWritePos, Length(FReadBuffer) - FWritePos);
      if ReadCount > 0 then
        Inc(FWritePos, ReadCount);
    end;
  end;

  Result := FWritePos - FReadPos;
end;

function TScSecureConnection.CopyData(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if Count > 0 then begin
    Result := FWritePos - FReadPos;
    if Result = 0 then
      Exit;

    if Result > Count then
      Result := Count;

    if Buffer <> nil then
      Move(FReadBuffer[FReadPos], Buffer[Offset], Result);
    Inc(FReadPos, Result);
    DefragBuffer;
  end
  else
    Result := 0;
end;

function TScSecureConnection.Read(const Buffer: TValueArr; Offset, Count: integer): integer;
var
  ReadCount: integer;
begin
  Result := CopyData(Buffer, Offset, Count);
  Dec(Count, Result);

  if Count <= 0 then
    Exit;

  if FWritePos <> FReadPos then
    raise Exception.Create(SReadNoWaitInternalError);

  if (Buffer = nil) or (Count < UNBUFFERED_READ_MIN_SIZE) then begin
    Assert(FWritePos = 0);
    ReadCount := ReadNoWait(@FReadBuffer[0], 0, Length(FReadBuffer));
    if ReadCount > 0 then
      Inc(FWritePos, ReadCount)
    else
      Exit;

    ReadCount := CopyData(Buffer, Offset, Count);
  end
  else
    ReadCount := ReadNoWait(Buffer, Offset, Count);

  if ReadCount > 0 then
    Inc(Result, ReadCount);
end;

procedure TScSecureConnection.RaiseLastError;
begin
  if FVio <> nil then begin
    if FVio.LastError <> '' then
      raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
  end;

  raise SocketException.Create(SConnectionClosed);
end;

procedure TScSecureConnection.Renegotiate;
begin
end;

procedure TScSecureConnection.TryReturnToPool;
begin
  if (FPool <> nil) and (PoolManagerList <> nil) then
    FPool.PutConnection(Self);
end;

procedure TScSecureConnection.SetReadWriteTimeout(Value: Integer);
begin
  if FVio <> nil then
    FVio.Timeout := Value;
end;

procedure TScSecureConnection.SetSocketOption(OptionLevel, OptionName, OptionValue: integer);
begin
  if (FVio <> nil) and (FVio is TCRVioTcp) then
    TCRVioTcp(FVio).SetSocketOption(OptionLevel, OptionName, OptionValue);
end;

function TScSecureConnection.GetLocalIP: string;
begin
  if (FVio <> nil) and (FVio is TCRVioTcp) then
    Result := TCRVioTcp(FVio).GetLocalIP
  else
    Result := '';
end;

function TScSecureConnection.GetLocalPort: integer;
begin
  if (FVio <> nil) and (FVio is TCRVioTcp) then
    Result := TCRVioTcp(FVio).GetLocalPort
  else
    Result := 0;
end;

function TScSecureConnection.GetRemoteIP: string;
begin
  if (FVio <> nil) and (FVio is TCRVioTcp) then
    Result := TCRVioTcp(FVio).GetRemoteIP
  else
    Result := '';
end;

function TScSecureConnection.GetRemotePort: integer;
begin
  if (FVio <> nil) and (FVio is TCRVioTcp) then
    Result := TCRVioTcp(FVio).GetRemotePort
  else
    Result := 0;
end;

function TScSecureConnection.GetAfterDisconnect: TNotifyEvent;
begin
  if FVio <> nil then
    Result := FVio.OnClose
  else
    Result := nil;
end;

procedure TScSecureConnection.SetAfterDisconnect(Value: TNotifyEvent);
begin
  if FVio <> nil then
    FVio.OnClose := Value;
end;


{ TScSecureConnectionStream }

constructor TScSecureConnectionStream.Create(AOwner: TScSecureConnection);
begin
  inherited Create;

  Assert(AOwner <> nil);
  FConnection := AOwner;
end;

function TScSecureConnectionStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FConnection.Read(@Buffer, 0, Count);
end;

function TScSecureConnectionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FConnection.Write(@Buffer, 0, Count);
  Result := Count;
end;

function TScSecureConnectionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

function TScSecureConnectionStream.GetSize: Int64;
begin
  Result := 0;
end;

{ TScHttpParser }

class procedure TScHttpParser.ParseURL(const URL: string; out Scheme, User, Password,
  NetworkLocation, Port, Path, Resource, Parameters, Query, Fragment: string);
var
  ParseString: string;
begin
  ParseString := URL;

  Fragment := ParseFragment(ParseString);
  Scheme := ParseScheme(ParseString);
  NetworkLocation := ParseNetworkLocation(ParseString);
  Query := ParseQuery(ParseString);
  Parameters := ParseParameters(ParseString);
  Resource := ParseResource(ParseString);
  Path := ParsePath(ParseString);
  if Path = '' then
    Path := '/';

  if NetworkLocation <> '' then begin
    User := ParseUserPassword(NetworkLocation);
    Password := ParsePassword(User);
    Port := ParsePort(NetworkLocation);
  end
  else begin
    User := '';
    Password := '';
    Port := '';
  end;
end;

class function TScHttpParser.ParseFragment(var ParseString: string): string;
begin
  Result := GetToEnd('#', ParseString, True);
end;

class function TScHttpParser.ParseScheme(var ParseString: string): string;
var
  i, len: Integer;
begin
  Result := '';

  len := Length(ParseString);
  if len > 2 then begin
    i := 1;
    while (i < len - 1) and CharInSet(ParseString[i], ['0'..'9', 'A'..'Z', 'a'..'z', '+', '.', '-']) do
      Inc(i);

    if (ParseString[i] = ':') and (ParseString[i + 1] = '/') then begin
      Result := Copy(ParseString, 1, i);
      Delete(ParseString, 1, i);
    end;
  end;
end;

class function TScHttpParser.ParseNetworkLocation(var ParseString: string): string;
var
  i, len: Integer;
begin
  Result := '';

  if ParseString <> '' then begin
    len := Length(ParseString);
    if (len >= 2) and (ParseString[1] = '/') and (ParseString[2] = '/') then begin
      i := 3;
      while (i <= len) and (ParseString[i] <> '/') do
        Inc(i);

      Result := Copy(ParseString, 3, i - 3);
      Delete(ParseString, 1, i - 1);
    end
    else begin
      i := Pos('/', ParseString);
      if i > 1 then begin
        Result := Copy(ParseString, 1, i - 1);
        Delete(ParseString, 1, i - 1);
      end
      else
        if i = 0 then begin
          Result := ParseString;
          ParseString := '';
        end
    end;
  end;
end;

class function TScHttpParser.ParseQuery(var ParseString: string): string;
begin
  Result := GetToEnd('?', ParseString, True);
end;

class function TScHttpParser.ParseParameters(var ParseString: string): string;
begin
  Result := GetToEnd(';', ParseString, True);
end;

class function TScHttpParser.ParseResource(var ParseString: string): string;
var
  i: Integer;
begin
  if Pos('.', ParseString) > 0 then begin
    i := Length(ParseString);
    while (i > 0) and (ParseString[i] <> '/') do
      Dec(i);

    if i > 0 then begin
      Result := Copy(ParseString, i + 1, Length(ParseString));
      ParseString := Copy(ParseString, 1, i);
    end
    else
      Result := '';
  end
  else
    Result := '';
end;

class function TScHttpParser.ParsePath(var ParseString: string): string;
begin
  Result := ParseString;
end;

class function TScHttpParser.ParsePassword(var ParseString: string): string;
begin
  Result := GetToEnd(':', ParseString, False);
end;

class function TScHttpParser.ParseUserPassword(var ParseString: string): string;
var
  i: Integer;
begin
  Result := '';

  if ParseString <> '' then begin
    i := Pos('@', ParseString);
    if i > 0 then begin
      Result := Copy(ParseString, 1, i - 1);
      Delete(ParseString, 1, i);
    end;
  end;
end;

class function TScHttpParser.ParsePort(var ParseString: string): string;
begin
  Result := GetToEnd(':', ParseString, True);
end;

class function TScHttpParser.GetToEnd(const FindChar: Char; var ParseString: string; const KeepFirst: Boolean): string;
var
  i, len: Integer;
begin
  Result := '';

  if ParseString <> '' then begin
    i := Pos(FindChar, ParseString);
    if i > 0 then begin
      len := Length(ParseString) - i + 1;
      Result := Copy(ParseString, i, len);
      Delete(ParseString, i, len);
    end;

    if not KeepFirst then
      Delete(Result, 1, 1);
  end;
end;

class function TScHttpParser.NthWord(const InputString: string; const Delimiter: Char; Number: integer): string;
var
  i, n: integer;
  StartPos: integer;
begin
  if InputString <> '' then begin
    n := 0;
    StartPos := 1;

    i := 1;
    while i <= Length(InputString) do begin
      if InputString[i] = Delimiter then begin
        Inc(n);
        if n = Number - 1 then
          StartPos := i + 1
        else
        if n = Number then
          Break;
      end;
      Inc(i);
    end;

    if (StartPos > 1) or (Number = 1) then
      Result := Copy(InputString, StartPos, i - StartPos)
    else
      Result := '';
  end
  else
    Result := '';
end;

class function TScHttpParser.WordsCount(const InputString: string; const Delimiter: Char): integer;
var
  i: integer;
begin
  if Length(InputString) > 0 then
    Result := 1
  else
    Result := 0;

  for i := 1 to Length(InputString) do
    if InputString[i] = Delimiter then
      Inc(Result);
end;

class procedure TScHttpParser.ParseKeyValue(const Token: string; out Key, Value: string);
var
  p: integer;
begin
  p := Pos(':', Token);
  if p > 0 then begin
    Key := Copy(Token, 1, p - 1);
    Value := Trim(Copy(Token, p + 1, Length(Token)));
  end
  else begin
    Key := Token;
    Value := '';
  end;
end;

class function TScHttpParser.ParseTokens(const Tokens: string; const Delimiter: Char): TStringArray;
var
  i, n: integer;
  StartPos: integer;
begin
  SetLength(Result, 0);
  n := 0;
  StartPos := 1;

  for i := 1 to Length(Tokens) do begin
    if Tokens[i] = Delimiter then begin
      Inc(n);
      SetLength(Result, n);
      Result[n - 1] := Trim(Copy(Tokens, StartPos, i - StartPos));
      StartPos := i + 1;
    end;
  end;

  if StartPos <= Length(Tokens) then begin
    Inc(n);
    SetLength(Result, n);
    Result[n - 1] := Trim(Copy(Tokens, StartPos, Length(Tokens) - StartPos + 1));
  end;
end;

class function TScHttpParser.FindToken(const Token: string; const Tokens: string; const Delimiter: Char): integer;
var
  TokensArr: TStringArray;
begin
  TokensArr := ParseTokens(Tokens, Delimiter);

  for Result := 0 to Length(TokensArr) - 1 do begin
    if AnsiSameText(TokensArr[Result], Token) then
      Exit;
  end;

  Result := -1;
end;

{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
var
  OldDLLProc: TDLLProc;

procedure LibraryProc(Reason: integer);
begin
  if Reason = DLL_PROCESS_DETACH then begin
    PoolManagerList.Free;
    PoolManagerList := nil;
  end;

  if Assigned(OldDLLProc) then
    OldDLLProc(Reason);
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
  OldDLLProc := DLLProc;
  DLLProc := @LibraryProc;
{$ENDIF}
{$ENDIF}
  CreateLock := TCriticalSection.Create;
  PoolManagerList := TCRObjectList.Create;

finalization
  PoolManagerList.Free;
  CreateLock.Free;

end.
