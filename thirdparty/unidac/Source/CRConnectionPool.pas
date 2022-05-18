
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Connection Pooling supports
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRConnectionPool;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs, Types, Variants,
  CRTypes, CLRClasses, CRVio, CRAccess, MemUtils;

const
  StatisticsCount = 8;

type
  TCRConnectionParametersClass = class of TCRConnectionParameters;
  TCRConnectionPoolManagerClass = class of TCRConnectionPoolManager;

  TCRConnectionPool = class;
  TCRConnectionPoolManager = class;

  { TCRConnectionParameters }

  TCRConnectionParameters = class(TPersistent)
  protected
    FSslOptions: TSSLOptions;
    FHttpOptions: THttpOptions;
    FProxyOptions: TProxyOptions;

    procedure CreateSecureOptions;
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; virtual;
    function PoolParamsToString: string; virtual;
  public
    MinPoolSize: integer;
    MaxPoolSize: integer;
    Username: string;
    Server: string;
    Password: string;
    ConnectionLifeTime: integer;
    Validate: boolean;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    IOHandler: TCRIOHandler;
    OnError: TErrorProc;

    constructor Create; virtual;
    destructor Destroy; override;

    function Equals(Parameters: TCRConnectionParameters): boolean; reintroduce; virtual;
    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function AsString: string; virtual;
  end;

  { TCRConnectionPool }

  TCRConnectionPool = class
  private
    FConnectionParameters: TCRConnectionParameters;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FManager: TCRConnectionPoolManager;
  protected
    FTakenConnectionsCount: integer;

    procedure Validate; virtual;
    procedure Clear; virtual;
    procedure AsyncClear; virtual;
    function GetTotalConnectionsCount: integer; virtual;
    function InternalPutConnection(CRConnection: TCRConnection): boolean; virtual; abstract;

  public
    constructor Create(Manager: TCRConnectionPoolManager; ConnectionParameters: TCRConnectionParameters); virtual;
    destructor Destroy; override;

    function GetConnection: TCRConnection; virtual; abstract;
    function PutConnection(CRConnection: TCRConnection): boolean;
    procedure Invalidate; virtual;

    property TotalConnectionsCount: integer read GetTotalConnectionsCount;
    property Manager: TCRConnectionPoolManager read FManager;
    property ConnectionParameters: TCRConnectionParameters read FConnectionParameters;
  end;

  TCRConnectionsArray = array of TCRConnection;
  TIntegerArray = array of integer;
  TStatisticsArray = array [0..StatisticsCount-1] of integer;

  { TCRLocalConnectionPool }

  TCRLocalConnectionPool = class(TCRConnectionPool)
  private
    //private ConnectMode connectMode = ConnectMode.Default;
    FPooledConnections: TCRConnectionsArray;
    FPooledConnectionsCount, FHead, FTail: integer;
    FVersions: TIntegerArray;
    FVersion: integer;
    FStatistics: TStatisticsArray;
    FDoomedConnectionsCount: integer;
    FInvalidateVersion, FClearVersion: integer;

    hBusy: TEvent;
    FLockPooled, FLockTaken, FLockVersion: TCriticalSection;

    function IsLive(CRConnection: TCRConnection): boolean;
    function CheckIsValid(Connection: TCRConnection): boolean;
    procedure ReserveConnection;
    function InternalGetConnection(out Connection: TCRConnection; out Version: integer;
      Reserve: boolean = True): boolean;
    procedure InternalFreeConnection(var Connection: TCRConnection; Reserved: boolean = False);
  protected
    class function GetConnectorClass: TCRConnectionClass; virtual;
    function CreateNewConnector: TCRConnection;
    procedure OpenConnector(Connector: TCRConnection); virtual;
    procedure InitConnectorParams(Connector: TCRConnection); virtual;
    procedure InitConnectorSecureParams(Connector: TCRConnection); virtual;

    procedure Validate; override;
    procedure Clear; override;
    procedure AsyncClear; override;
    function GetTotalConnectionsCount: integer; override;
    function InternalPutConnection(CRConnection: TCRConnection): boolean; override;
    procedure InternalReturnConnection(Connection: TCRConnection; Version: integer); virtual;
  public
    // TODO: Add transaction context parameter
    constructor Create(Manager: TCRConnectionPoolManager; ConnectionParameters: TCRConnectionParameters); override;
    destructor Destroy; override;

    function GetConnection: TCRConnection; override;
    procedure Invalidate; override;

    property PooledConnectionsCount: integer read FPooledConnectionsCount;
  end;

  { TValidateThread }

  TValidateThread = class(TThread)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FManager: TCRConnectionPoolManager;
  {$IFDEF MSWINDOWS}
    FEvent: TEvent;
  {$ENDIF}
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TCRConnectionPoolManager);
  {$IFDEF MSWINDOWS}
    destructor Destroy; override;
    procedure Terminate;
  {$ENDIF}
  end;

  { TCRConnectionPoolManager }

  TCRConnectionPoolManager = class
  private
    FPools: TCRObjectList;
    FValidateThread: TValidateThread;

  protected
    FLockGet: TCriticalSection;
    FLockList: TCriticalSection;
    FSQLMonitorClass: TClass;

  protected
    class function GetPoolManagerIndex: Integer; virtual;
    class procedure SetPoolManagerIndex(Value: Integer); virtual;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; virtual; abstract;
    function GetConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;

    procedure InternalClear;
    procedure InternalAsyncClear;
    function InternalGetConnection(ConnectionParameters: TCRConnectionParameters): TCRConnection; virtual;
    function InternalCheckConnection(var Connection: TCRConnection): TCRConnection; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TClass): TCRConnection;
    class procedure Clear;
    class procedure AsyncClear;

    property SQLMonitorClass: TClass read FSQLMonitorClass write FSQLMonitorClass;
  end;

  procedure ClearPoolManager;

implementation

uses
  TypInfo,
  CRFunctions, CRProps, MemData,
  DAConsts, DASQLMonitor;

var
  CreateLock: TCriticalSection;
  PoolManagerList: TCRObjectList;

{ TCRConnectionParameters }

constructor TCRConnectionParameters.Create;
begin
  inherited Create;

  MaxPoolSize := DefValMaxPoolSize;
end;

destructor TCRConnectionParameters.Destroy;
begin
  FSslOptions.Free;
  FHttpOptions.Free;
  FProxyOptions.Free;

  inherited;
end;

function TCRConnectionParameters.Equals(Parameters: TCRConnectionParameters): boolean;
begin
  Result := False;
  if Parameters <> nil then begin
    Result :=
      (MinPoolSize = Parameters.MinPoolSize) and
      (MaxPoolSize = Parameters.MaxPoolSize) and
      (ConnectionLifeTime = Parameters.ConnectionLifeTime) and
      SameText(Username, Parameters.Username) and
      SameText(Server, Parameters.Server) and
      (Password = Parameters.Password) and
      (Validate = Parameters.Validate) and
      (IOHandler = Parameters.IOHandler);

    if (FSslOptions <> nil) and (Parameters.FSslOptions <> nil) then
      Result := Result and FSslOptions.Equals(Parameters.FSslOptions)
    else if (FSslOptions <> nil) or (Parameters.FSslOptions <> nil) then
      Result := False;

    if (FHttpOptions <> nil) and (Parameters.FHttpOptions <> nil) then
      Result := Result and FHttpOptions.Equals(Parameters.FHttpOptions)
    else if (FHttpOptions <> nil) or (Parameters.FHttpOptions <> nil) then
      Result := False;

    if (FProxyOptions <> nil) and (Parameters.FProxyOptions <> nil) then
      Result := Result and FProxyOptions.Equals(Parameters.FProxyOptions)
    else if (FProxyOptions <> nil) or (Parameters.FProxyOptions <> nil) then
      Result := False;
  end;
end;

procedure TCRConnectionParameters.CreateSecureOptions;
begin
  // Call for MyDAC, ODAC and PgDAC only
  FSslOptions := TSSLOptions.Create;
  FHttpOptions := THttpOptions.Create;
  FProxyOptions := TProxyOptions.Create;
end;

procedure TCRConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TCRConnectionParameters then begin
    TCRConnectionParameters(Dest).MinPoolSize        := MinPoolSize;
    TCRConnectionParameters(Dest).MaxPoolSize        := MaxPoolSize;
    TCRConnectionParameters(Dest).Username           := Username;
    TCRConnectionParameters(Dest).Password           := Password;
    TCRConnectionParameters(Dest).Server             := Server;
    TCRConnectionParameters(Dest).ConnectionLifeTime := ConnectionLifeTime;
    TCRConnectionParameters(Dest).Validate           := Validate;
    TCRConnectionParameters(Dest).IOHandler          := IOHandler;
    TCRConnectionParameters(Dest).OnError            := OnError;

    if TCRConnectionParameters(Dest).FSslOptions <> nil then
      TCRConnectionParameters(Dest).FSslOptions.Assign(FSslOptions);
    if TCRConnectionParameters(Dest).FHttpOptions <> nil then
      TCRConnectionParameters(Dest).FHttpOptions.Assign(FHttpOptions);
    if TCRConnectionParameters(Dest).FProxyOptions <> nil then
      TCRConnectionParameters(Dest).FProxyOptions.Assign(FProxyOptions);
  end
  else
    inherited;
end;

function TCRConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prSSLCipher:
      FSslOptions.Cipher := Value;
    prSSLCA:
      FSslOptions.CA := Value;
    prSSLKey:
      FSslOptions.Key := Value;
    prSSLCert:
      FSslOptions.Cert := Value;

    prUseHttp:
      FHttpOptions.Enabled := Value;
    prHttpUrl:
      FHttpOptions.Url := Value;
    prHttpUsername:
      FHttpOptions.Username := Value;
    prHttpPassword:
      FHttpOptions.Password := Value;
    prHttpTrustServerCertificate:
      FHttpOptions.TrustServerCertificate := Value;

    prProxyHostname:
      FProxyOptions.Hostname := Value;
    prProxyPort:
      FProxyOptions.Port := integer(Value);
    prProxyUsername:
      FProxyOptions.Username := Value;
    prProxyPassword:
      FProxyOptions.Password := Value;
    prProxySocksVersion:
      FProxyOptions.SocksVersion := TCRSocksVersion(integer(Value));
    prProxyResolveDNS:
      FProxyOptions.ResolveDNS := boolean(Value);

    else begin
      Assert(False, IntToStr(Prop));
      Result := False;
    end;
  end;
end;

function TCRConnectionParameters.ConnectParamsToString: string;
begin
  Result := Format('Username=%s'#13'Server=%s'#13,
    [Username, Server]);
end;

function TCRConnectionParameters.PoolParamsToString: string;
begin
  Result := Format(
    'MinPoolSize=%d'#13'MaxPoolSize=%d'#13'ConnectionLifeTime=%d'#13 +
    'Validate=' + BoolToStr(Validate, True) + #13,
    [MinPoolSize, MaxPoolSize, ConnectionLifeTime]);
end;

function TCRConnectionParameters.AsString: string;
begin
  Result := ConnectParamsToString + PoolParamsToString;
end;

{ TCRConnectionPool }

constructor TCRConnectionPool.Create(Manager: TCRConnectionPoolManager; ConnectionParameters: TCRConnectionParameters);
begin
  inherited Create;

  FConnectionParameters := TCRConnectionParametersClass(ConnectionParameters.ClassType).Create;
  FConnectionParameters.Assign(ConnectionParameters);
  FManager := Manager;

  TDASQLMonitorClass(FManager.SQLMonitorClass).PoolMessage(Self, 'Connection pool created', False, True);
end;

destructor TCRConnectionPool.Destroy;
begin
  TDASQLMonitorClass(FManager.SQLMonitorClass).PoolMessage(Self, 'Connection pool destroyed', False);

  FConnectionParameters.Free;

  inherited;
end;

function TCRConnectionPool.GetTotalConnectionsCount: integer;
begin
  Result := FTakenConnectionsCount;
end;

procedure TCRConnectionPool.Invalidate;
begin
end;

procedure TCRConnectionPool.Validate;
begin
end;

procedure TCRConnectionPool.Clear;
begin
end;

procedure TCRConnectionPool.AsyncClear;
begin
end;

function TCRConnectionPool.PutConnection(CRConnection: TCRConnection): boolean;
begin
  Result := InternalPutConnection(FManager.InternalCheckConnection(CRConnection));
end;

{ TCRLocalConnectionPool }

constructor TCRLocalConnectionPool.Create(Manager: TCRConnectionPoolManager; ConnectionParameters: TCRConnectionParameters);
begin
  inherited;

  SetLength(FPooledConnections, Self.ConnectionParameters.MaxPoolSize);
  SetLength(FVersions, Self.ConnectionParameters.MaxPoolSize);
  hBusy := TEvent.Create(nil, True, True, '');
  FLockPooled := TCriticalSection.Create;
  FLockTaken := TCriticalSection.Create;
  FLockVersion := TCriticalSection.Create;
end;

destructor TCRLocalConnectionPool.Destroy;
begin
  Clear;
  FLockVersion.Free;
  FLockTaken.Free;
  FLockPooled.Free;
  hBusy.Free;

  inherited;
end;

function TCRLocalConnectionPool.IsLive(CRConnection: TCRConnection): boolean;
var
  LifeTime: Cardinal;
begin
  Result := FConnectionParameters.ConnectionLifeTime = 0;
  if Result then // If connector life time is zero then does not remove connector
    Exit;

  LifeTime := GetTickInterval(CRConnection.ConnectionTime, {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF});
  Result := LifeTime <= Cardinal(FConnectionParameters.ConnectionLifeTime);
end;

function TCRLocalConnectionPool.CheckIsValid(Connection: TCRConnection): boolean;
begin
  Result := Connection.CheckIsValid;
  Connection.PoolVersion := FInvalidateVersion;
end;

procedure TCRLocalConnectionPool.ReserveConnection;
begin
  Inc(FTakenConnectionsCount);
  if FTakenConnectionsCount >= ConnectionParameters.MaxPoolSize then
    hBusy.ResetEvent;
end;

function TCRLocalConnectionPool.InternalGetConnection(out Connection: TCRConnection;
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

procedure TCRLocalConnectionPool.InternalFreeConnection(var Connection: TCRConnection;
  Reserved: boolean = False);
begin
  // TODO: May be this try-except unnecessary
  try
    try
      Connection.Disconnect; // for NEXTGEN
    finally
      Connection.Free;
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

  TDASQLMonitorClass(FManager.SQLMonitorClass).PoolMessage(Self, 'Connection destroyed in pool', True);
end;

function TCRLocalConnectionPool.GetConnection: TCRConnection;
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

  if InternalGetConnection(Result, Version, False) then begin
    if (Result.PoolVersion < FClearVersion) or
      (ConnectionParameters.Validate or (Result.PoolVersion < FInvalidateVersion))
      and not CheckIsValid(Result)
    then
      InternalFreeConnection(Result, True);
  end;

  if Result = nil then begin
    Result := CreateNewConnector;
    TDASQLMonitorClass(FManager.SQLMonitorClass).PoolMessage(Self, 'Connection created in pool', False);
    OpenConnector(Result);
  end;
  Result.Pool := Self;
  Result.PoolVersion := FInvalidateVersion;

  TDASQLMonitorClass(FManager.SQLMonitorClass).PoolMessage(Self, 'Connection taken from pool', True);
end;

function TCRLocalConnectionPool.InternalPutConnection(CRConnection: TCRConnection): boolean;
var
  Version: integer;
begin
  Assert(CRConnection.Pool = Self);
  CRConnection.Pool := nil; // protection from PutConnection call on already pooled connection
  Result := False;

  if not IsLive(CRConnection) or not CRConnection.IsValid or
    (CRConnection.PoolVersion < FClearVersion) or
    (CRConnection.PoolVersion < FInvalidateVersion) and not CheckIsValid(CRConnection)
  then
    InternalFreeConnection(CRConnection)
  else begin
    FLockVersion.Enter;
    try
      Inc(FVersion);
      Version := FVersion;
    finally
      FLockVersion.Leave;
    end;
    InternalReturnConnection(CRConnection, Version);

    TDASQLMonitorClass(FManager.SQLMonitorClass).PoolMessage(Self, 'Connection returned to pool', True);
    Result := True;
  end;
end;

procedure TCRLocalConnectionPool.InternalReturnConnection(Connection: TCRConnection;
  Version: integer);
begin
  FLockPooled.Enter;
  try
    FPooledConnections[FTail] := Connection;
    FVersions[FTail] := Version;
    Inc(FTail);
    if FTail = ConnectionParameters.MaxPoolSize then
      FTail := 0;
    Inc(FPooledConnectionsCount);
    {if FDoomedConnectionsCount > FPooledConnectionsCount - ConnectionParameters.MinPoolSize then
      FDoomedConnectionsCount := FPooledConnectionsCount - ConnectionParameters.MinPoolSize;}

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

class function TCRLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Assert(False, 'Abstract method');
  Result := nil;
end;

function TCRLocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := GetConnectorClass.Create;
  try
    InitConnectorParams(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCRLocalConnectionPool.OpenConnector(Connector: TCRConnection);
begin
  StartWait;
  try
    try
      Connector.Connect('');
    except
      InternalFreeConnection(Connector);
      raise;
    end;
  finally
    StopWait;
  end;
end;

procedure TCRLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetServer(ConnectionParameters.Server);
  Connector.SetUsername(ConnectionParameters.UserName);
  Connector.SetPassword(ConnectionParameters.Password);

  InitConnectorSecureParams(Connector);
end;

procedure TCRLocalConnectionPool.InitConnectorSecureParams(Connector: TCRConnection);
begin
  Connector.IOHandler := ConnectionParameters.IOHandler;

  if ConnectionParameters.FSslOptions <> nil then begin
    Connector.SetProp(prSSLCipher, ConnectionParameters.FSslOptions.Cipher);
    Connector.SetProp(prSSLCA, ConnectionParameters.FSslOptions.CA);
    Connector.SetProp(prSSLKey, ConnectionParameters.FSslOptions.Key);
    Connector.SetProp(prSSLCert, ConnectionParameters.FSslOptions.Cert);
  end;

  if ConnectionParameters.FHttpOptions <> nil then begin
    Connector.SetProp(prUseHttp, ConnectionParameters.FHttpOptions.Enabled);
    Connector.SetProp(prHttpUrl, ConnectionParameters.FHttpOptions.Url);
    Connector.SetProp(prHttpUsername, ConnectionParameters.FHttpOptions.Username);
    Connector.SetProp(prHttpPassword, ConnectionParameters.FHttpOptions.Password);
    Connector.SetProp(prHttpTrustServerCertificate, ConnectionParameters.FHttpOptions.TrustServerCertificate);
  end;

  if ConnectionParameters.FProxyOptions <> nil then begin
    Connector.SetProp(prProxyHostname, ConnectionParameters.FProxyOptions.Hostname);
    Connector.SetProp(prProxyPort, ConnectionParameters.FProxyOptions.Port);
    Connector.SetProp(prProxyUsername, ConnectionParameters.FProxyOptions.Username);
    Connector.SetProp(prProxyPassword, ConnectionParameters.FProxyOptions.Password);
    Connector.SetProp(prProxySocksVersion, ConnectionParameters.FProxyOptions.SocksVersion);
    Connector.SetProp(prProxyResolveDNS, ConnectionParameters.FProxyOptions.ResolveDNS);
  end;
end;

procedure TCRLocalConnectionPool.Validate;
var
  Connection: TCRConnection;
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
      (Connection.PoolVersion < FClearVersion) or
      ((Version <= FirstVersion) or (Connection.PoolVersion < FInvalidateVersion))
      and not CheckIsValid(Connection)
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

procedure TCRLocalConnectionPool.Invalidate;
begin
  Inc(FInvalidateVersion);
end;

procedure TCRLocalConnectionPool.Clear;
var
  Connection: TCRConnection;
  Version: integer;
begin
  while InternalGetConnection(Connection, Version) do
    InternalFreeConnection(Connection);
end;

procedure TCRLocalConnectionPool.AsyncClear;
begin
  Inc(FInvalidateVersion);
  Inc(FClearVersion);
end;

function TCRLocalConnectionPool.GetTotalConnectionsCount: integer;
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

{ TCRConnectionPoolManager }

constructor TCRConnectionPoolManager.Create;
begin
  inherited;

  FPools := TCRObjectList.Create;
  FLockGet := TCriticalSection.Create;
  FLockList := TCriticalSection.Create;
  FValidateThread := TValidateThread.Create(Self);

  SetPoolManagerIndex(PoolManagerList.Add(Self));
end;

destructor TCRConnectionPoolManager.Destroy;
begin
  SetPoolManagerIndex(-1);
  PoolManagerList.Remove(Self);

  if FValidateThread <> nil then begin
    FValidateThread.Terminate;
  {$IFDEF MSWINDOWS}
    if IsLibrary then
      TerminateThread(FValidateThread.Handle, 0);
  {$ENDIF}
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

class function TCRConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Assert(False, 'Abstract method');
  Result := -1;
end;

class procedure TCRConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  Assert(False, 'Abstract method');
end;

// Conn parameters used for creating new pool with initial parameters
function TCRConnectionPoolManager.GetConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
var
  i: integer;
  Pool: TCRConnectionPool;
begin
  Result := nil;

  // Search if pool with same connection string exist
  for i := 0 to FPools.Count - 1 do begin
    Pool := TCRConnectionPool(FPools.Items[i]);
    if Pool.FConnectionParameters.Equals(ConnectionParameters) then begin
      Pool.FConnectionParameters.OnError := ConnectionParameters.OnError;
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

procedure TCRConnectionPoolManager.InternalClear;
begin
  FLockList.Enter;
  try
    FPools.Clear;
  finally
    FLockList.Leave;
  end;
end;

procedure TCRConnectionPoolManager.InternalAsyncClear;
var
  i: integer;
begin
  FLockList.Enter;
  try
    for i := 0 to FPools.Count - 1 do
      TCRConnectionPool(FPools[i]).AsyncClear;
  finally
    FLockList.Leave;
  end;
end;

function TCRConnectionPoolManager.InternalGetConnection(ConnectionParameters: TCRConnectionParameters): TCRConnection;
begin
  FLockGet.Enter;
  try
    Result := GetConnectionPool(ConnectionParameters).GetConnection;
  finally
    FLockGet.Leave;
  end;
end;

function TCRConnectionPoolManager.InternalCheckConnection(var Connection: TCRConnection): TCRConnection;
begin
  Result := Connection;
end;

class function TCRConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TClass): TCRConnection;
var
  Index: Integer;
  PoolManager: TCRConnectionPoolManager;
begin
  CreateLock.Enter;
  try
    Index := GetPoolManagerIndex;
    if Index = -1 then begin
      PoolManager := Self.Create;
      PoolManager.SQLMonitorClass := SQLMonitorClass;
    end
    else
      PoolManager := TCRConnectionPoolManager(PoolManagerList[Index]);
  finally
    CreateLock.Leave;
  end;

  Result := PoolManager.InternalGetConnection(ConnectionParameters);
end;

class procedure TCRConnectionPoolManager.Clear;
var
  Index: Integer;
  PoolManager: TCRConnectionPoolManager;
begin
  Index := GetPoolManagerIndex;
  if Index <> -1 then begin
    PoolManager := TCRConnectionPoolManager(PoolManagerList[Index]);
    PoolManager.InternalClear;
  end;
end;

class procedure TCRConnectionPoolManager.AsyncClear;
var
  Index: Integer;
  PoolManager: TCRConnectionPoolManager;
begin
  Index := GetPoolManagerIndex;
  if Index <> -1 then begin
    PoolManager := TCRConnectionPoolManager(PoolManagerList[Index]);
    PoolManager.InternalAsyncClear;
  end;
end;

{ TValidateThread }

constructor TValidateThread.Create(Manager: TCRConnectionPoolManager);
begin
  FManager := Manager;
{$IFDEF MSWINDOWS}
  FEvent := TEvent.Create(nil, True, False, '');
{$ENDIF}

  inherited Create(False);
end;

{$IFDEF MSWINDOWS}
destructor TValidateThread.Destroy;
begin
  FEvent.Free;
  inherited;
end;

procedure TValidateThread.Terminate;
begin
  inherited;
  FEvent.SetEvent;
end;
{$ENDIF}

procedure TValidateThread.Execute;
const
  Timeout = 30000;
var
  i, Count: integer;
  Pool: TCRConnectionPool;
  Ticks, BeginTickCount: cardinal;
begin
  Ticks := 0;
  while True do begin
    if Terminated then
      Exit;
  {$IFDEF MSWINDOWS}
    if (Ticks < Timeout) and (FEvent.WaitFor(Timeout - Ticks) = wrSignaled) then
      Exit;
  {$ELSE}
    while Ticks < Timeout do begin
      Sleep(200);
      if Terminated then
        Exit;
      Ticks := Ticks + 200;
    end;
  {$ENDIF}

    BeginTickCount := GetTickCount;

    FManager.FLockList.Enter;
    try
      for i := FManager.FPools.Count - 1 downto 0 do begin
        if Terminated then
          Exit;
        Pool := TCRConnectionPool(FManager.FPools[i]);
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

procedure ClearPoolManager;
begin
  PoolManagerList.Free;
  PoolManagerList := nil;
end;

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
  ClearPoolManager;
  CreateLock.Free;

end.
