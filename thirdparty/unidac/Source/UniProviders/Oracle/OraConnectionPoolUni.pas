
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright (c) 1998-2021 Devart. All right reserved.
//  Connection Pool
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraConnectionPoolUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SyncObjs, {$IFDEF VER6P}Variants,{$ENDIF}
  CRTypes, CLRClasses, CRConnectionPool, CRVio, CRAccess,
{$IFNDEF UNIDACPRO}
  OraCall, OraClasses;
{$ELSE}
  OraCallUni, OraClassesUni;
{$ENDIF}

const
  prPoolingType = 101;

type
  TOraPoolingType = (optLocal, optOCI{$IFNDEF LITE}{$IFDEF MSWINDOWS}, optMTS{$ENDIF}{$ENDIF});

  TOraConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    Direct: boolean;
    HomeName: string;
    IPVersion: TIPVersion;
    Charset: string;
    CharLength: word;
    UseUnicode: boolean;
    UnicodeEnvironment: boolean;
    SubscriptionPort: Integer;
    ConnectMode: TConnectMode;
    UseOCI7: boolean;
    PoolingType: TOraPoolingType;
    StatementCache: boolean;
    StatementCacheSize: integer;
    ConnectionTimeout: integer;
    OptimizerMode: TOptimizerMode;
    ClientIdentifier: string;
    Schema: string;
    ProxyUserName: string;
    ProxyPassword: string;

    constructor Create; override;

    function Equals(Parameters: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TOraLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    class procedure InitConnectorOraParams(Connector: TCRConnection; Parameters: TCRConnectionParameters);
    procedure InitConnectorParams(Connector: TCRConnection); override;
    procedure InitConnectorSecureParams(Connector: TCRConnection); override;
  end;

  TOraOCIConnectionPool = class(TCRConnectionPool)
  private
    FEnvironment: TOCIEnvironment;
    FhOCIError: pOCIError;
    FhOCISPool: pOCISPool;
    FPoolName: string;

    function GetOCI8: TOCI8API;
    function GetHome: TOracleHome;
    function AllocPoolHandle: pOCISPool;
    procedure FreePoolHandle(hOCISPool: pOCISPool);
    procedure CreateOCIPool;
    procedure FreeOCIPool;
  protected
    function InternalPutConnection(CRConnection: TCRConnection): boolean; override;

    property OCI8: TOCI8API read GetOCI8;
  public
    destructor Destroy; override;

    function GetConnection: TCRConnection; override;
  end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  TOraMTSConnectionPool = class(TCRConnectionPool)
  protected
    function InternalPutConnection(CRConnection: TCRConnection): boolean; override;
  public
    function GetConnection: TCRConnection; override;
  end;
{$ENDIF}
{$ENDIF}

  TOraConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;

    function InternalGetConnection(ConnectionParameters: TCRConnectionParameters): TCRConnection; override;
    function InternalCheckConnection(var Connection: TCRConnection): TCRConnection; override;
  end;

implementation

uses
  SysUtils,
  CRProps, CRFunctions, MemData, DAConsts,
{$IFNDEF UNIDACPRO}
  OraConsts, OraProps, OraError;
{$ELSE}
  OraConstsUni, OraPropsUni, OraErrorUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TOraConnectionParameters}

constructor TOraConnectionParameters.Create;
begin
  inherited;

  CreateSecureOptions;
end;

procedure TOraConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TOraConnectionParameters then begin
    TOraConnectionParameters(Dest).PoolingType := PoolingType;
  {$IFDEF NET}
    TOraConnectionParameters(Dest).Direct := Direct;
  {$ENDIF}
    TOraConnectionParameters(Dest).HomeName := HomeName;
    TOraConnectionParameters(Dest).IPVersion := IPVersion;
    TOraConnectionParameters(Dest).Charset := Charset;
    TOraConnectionParameters(Dest).CharLength := CharLength;
    TOraConnectionParameters(Dest).UseUnicode := UseUnicode;
    TOraConnectionParameters(Dest).UnicodeEnvironment := UnicodeEnvironment;
    TOraConnectionParameters(Dest).SubscriptionPort := SubscriptionPort;
    TOraConnectionParameters(Dest).ConnectMode := ConnectMode;
    TOraConnectionParameters(Dest).UseOCI7 := UseOCI7;
    TOraConnectionParameters(Dest).StatementCache := StatementCache;
    TOraConnectionParameters(Dest).StatementCacheSize := StatementCacheSize;
    TOraConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
    TOraConnectionParameters(Dest).OptimizerMode := OptimizerMode;
    TOraConnectionParameters(Dest).ClientIdentifier := ClientIdentifier;
    TOraConnectionParameters(Dest).Schema := Schema;
  end;

  inherited;
end;

function TOraConnectionParameters.Equals(Parameters: TCRConnectionParameters): boolean;
var
  OraParameters: TOraConnectionParameters;
begin
  Result := False;
  if Parameters <> nil then begin
    OraParameters := TOraConnectionParameters(Parameters);
    Result :=
      (PoolingType = OraParameters.PoolingType) and
      (MinPoolSize = OraParameters.MinPoolSize) and
      (MaxPoolSize = OraParameters.MaxPoolSize) and
      (ConnectionLifeTime = OraParameters.ConnectionLifeTime) and
      (Validate = OraParameters.Validate) and

    {$IFDEF NET}
      Direct = OraParameters.Direct and
    {$ENDIF}
      SameText(HomeName, OraParameters.HomeName) and
      (IPVersion = OraParameters.IPVersion) and
      SameText(Charset, OraParameters.Charset) and
      (CharLength = OraParameters.CharLength) and
      (UseUnicode = OraParameters.UseUnicode) and
      (UnicodeEnvironment = OraParameters.UnicodeEnvironment) and
      (SubscriptionPort = OraParameters.SubscriptionPort) and
      (ConnectMode = OraParameters.ConnectMode) and
      (UseOCI7 = OraParameters.UseOCI7) and
      (StatementCache = OraParameters.StatementCache) and
      (StatementCacheSize = OraParameters.StatementCacheSize)and
      (ConnectionTimeout = OraParameters.ConnectionTimeout) and
      (OptimizerMode = OraParameters.OptimizerMode) and
      (ClientIdentifier = OraParameters.ClientIdentifier) and
      SameText(Schema, OraParameters.Schema) and
      SameText(Server, OraParameters.Server) and
      ((SameText(Username, OraParameters.Username) and (Password = OraParameters.Password)) or (PoolingType = optOCI)) and

      FSslOptions.Equals(OraParameters.FSslOptions) and
      FHttpOptions.Equals(OraParameters.FHttpOptions) and
      FProxyOptions.Equals(OraParameters.FProxyOptions);
  end;
end;

function TOraConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
  {$IFDEF NET}
    prDirect: begin
    {$IFDEF NET}
      Direct := Value;
    {$ELSE}
      if Value = True then
        raise Exception.Create(SDirectModeNotSupported);
    {$ENDIF}
    end;
  {$ENDIF}
    prHomeName:
      HomeName := Value;
    prIPVersion:
      IPVersion := Value;
    prCharset:
      Charset := Value;
    prCharLength:
      CharLength := Value;
    prUseUnicode:
      UseUnicode := Value;
    prUnicodeEnvironment:
      UnicodeEnvironment:= Value;
    prSubscriptionPort:
      SubscriptionPort := Value;
    prConnectMode:
      ConnectMode := TConnectMode(Value);
    prUseOCI7:
      UseOCI7 := Value;
    prPoolingType:
      PoolingType := TOraPoolingType(Value);
    prStatementCache:
      StatementCache := Value;
    prStatementCacheSize:
      StatementCacheSize := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prOptimizerMode:
      OptimizerMode := TOptimizerMode(Value);
    prClientIdentifier:
      ClientIdentifier := Value;
    prSchema:
      Schema := Value;

    prSSLServerCertDN:
      FSslOptions.ServerCertDN := Value;

  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TOraLocalConnectionPool }

class function TOraLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TOCIConnection;
end;

class procedure TOraLocalConnectionPool.InitConnectorOraParams(Connector: TCRConnection; Parameters: TCRConnectionParameters);
begin
{$IFDEF NET}
  Connector.SetProp(prDirect, TOraConnectionParameters(Parameters).Direct);
{$ENDIF}
  Connector.SetProp(prHomeName, TOraConnectionParameters(Parameters).HomeName);
  Connector.SetProp(prIPVersion, TOraConnectionParameters(Parameters).IPVersion);
  Connector.SetProp(prCharset, TOraConnectionParameters(Parameters).Charset);
  Connector.SetProp(prCharLength, TOraConnectionParameters(Parameters).CharLength);
  Connector.SetProp(prUseUnicode, TOraConnectionParameters(Parameters).UseUnicode);
  Connector.SetProp(prStatementCache, TOraConnectionParameters(Parameters).StatementCache);
  Connector.SetProp(prSubscriptionPort, TOraConnectionParameters(Parameters).SubscriptionPort);
  Connector.SetProp(prConnectMode, Variant(TOraConnectionParameters(Parameters).ConnectMode));
  Connector.SetProp(prUseOCI7, TOraConnectionParameters(Parameters).UseOCI7);
  Connector.SetProp(prStatementCache, TOraConnectionParameters(Parameters).StatementCache);
  Connector.SetProp(prStatementCacheSize, TOraConnectionParameters(Parameters).StatementCacheSize);
  Connector.SetProp(prConnectionTimeOut, TOraConnectionParameters(Parameters).ConnectionTimeout);
  Connector.SetProp(prOptimizerMode, Variant(TOraConnectionParameters(Parameters).OptimizerMode));
  Connector.SetProp(prClientIdentifier, TOraConnectionParameters(Parameters).ClientIdentifier);
  Connector.SetProp(prSchema, TOraConnectionParameters(Parameters).Schema);

  Connector.OnError := TOraConnectionParameters(Parameters).OnError;
end;

procedure TOraLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited;

  InitConnectorOraParams(Connector, ConnectionParameters);
  InitConnectorSecureParams(Connector);
end;

procedure TOraLocalConnectionPool.InitConnectorSecureParams(Connector: TCRConnection);
begin
  inherited;

  Connector.SetProp(prSSLServerCertDN, TOraConnectionParameters(ConnectionParameters).FSslOptions.ServerCertDN);
end;

{ TOraOciConnectionPool }

destructor TOraOCIConnectionPool.Destroy;
begin
  FreeOCIPool;

  inherited;
end;

function TOraOCIConnectionPool.GetOCI8: TOCI8API;
begin
  Result := FEnvironment.Home.OCI8;
end;

function TOraOCIConnectionPool.GetHome: TOracleHome;
begin
{$IFDEF NET}
  if TOraConnectionParameters(ConnectionParameters).Direct then
    RaiseError(SOCIPoolNotSupportedWithDirect);
{$ENDIF}

  Result := OracleHomes.GetHome(TOraConnectionParameters(ConnectionParameters).HomeName);
  Result.Init;
  if Result.OCIVersion < 9200 then
    RaiseError(SOCIPoolNotSupported);
end;

function TOraOCIConnectionPool.AllocPoolHandle: pOCISPool;
var
  Res: Integer;
begin
  Res := OCI8.OCIHandleAlloc(FEnvironment.hOCIEnv, Result, OCI_HTYPE_SPOOL, 0, nil);
  TOraError.Check(
    @OCI8.OCIErrorGet,
    Res,
    FEnvironment.UnicodeEnv,
    FhOCIError
  );
end;

procedure TOraOCIConnectionPool.FreePoolHandle(hOCISPool: pOCISPool);
begin
  OCI8.OCIHandleFree(hOCISPool, OCI_HTYPE_SPOOL);
end;

procedure TOraOCIConnectionPool.CreateOCIPool;
var
  PoolName, p: IntPtr;
  PoolNameLen: Cardinal;
  Mode: Cardinal;
  Res, Size: integer;
begin
  FEnvironment := GetHome.AllocEnvironment(False, TOraConnectionParameters(ConnectionParameters).UnicodeEnvironment, SubscriptionPort);
{$IFNDEF AUTOREFCOUNT}
  FEnvironment.AddRef;
{$ENDIF}
  try
    FEnvironment.Init;

    FhOCIError := FEnvironment.AllocErrorHandle;
    try
      FhOCISPool := AllocPoolHandle;
      try
        Mode := OCI_DEFAULT;
        if TOraConnectionParameters(ConnectionParameters).StatementCache then
          Mode := OCI_SPC_STMTCACHE;

        with TOraConnectionParameters(ConnectionParameters) do begin
          p := StringToHGlobalOCI(Server, Size, FEnvironment.UnicodeEnv);
          Res := OCI8.OCISessionPoolCreate(FEnvironment.hOCIEnv, FhOCIError, FhOCISPool, PoolName, PoolNameLen,
            p, Size, MinPoolSize, MaxPoolSize, 0, nil, 0, nil, 0, Mode);
          Marshal.FreeCoTaskMem(p);
          TOraError.Check(
            @OCI8.OCIErrorGet,
            Res,
            FEnvironment.UnicodeEnv,
            FhOCIError
          );
        end;

        FPoolName := PtrToStringOCI(PoolName, FEnvironment.UnicodeEnv);
      except
        FreePoolHandle(FhOCISPool);
        FhOCISPool := nil;
        raise;
      end;
    except
      FEnvironment.FreeErrorHandle(FhOCIError);
      FhOCIError := nil;
      raise;
    end;
  except
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.ReleaseRef;
  {$ENDIF}
    FEnvironment := nil;
    raise;
  end;
end;

procedure TOraOCIConnectionPool.FreeOCIPool;
begin
  if FPoolName <> '' then begin
    TOraError.Check(
      @OCI8.OCIErrorGet,
      OCI8.OCISessionPoolDestroy(FhOCISPool, FhOCIError, OCI_DEFAULT),
      FEnvironment.UnicodeEnv,
      FhOCIError
    );
    FPoolName := '';
  end;

  if FhOCISPool <> nil then begin
    OCI8.OCIHandleFree(FhOCISPool, OCI_HTYPE_SPOOL);
    FhOCISPool := nil;
  end;

  if FhOCIError <> nil then begin
    FEnvironment.FreeErrorHandle(FhOCIError);
    FhOCIError := nil;

  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.ReleaseRef;
  {$ENDIF}
    FEnvironment := nil;
  end;
end;

function TOraOCIConnectionPool.GetConnection: TCRConnection;
begin
  if FPoolName = '' then
    CreateOCIPool;

  Result := TOCIConnection.Create;
  try
    Result.SetUsername(ConnectionParameters.Username);
    Result.SetPassword(ConnectionParameters.Password);
    TOraLocalConnectionPool.InitConnectorOraParams(Result, ConnectionParameters);

    TOCIConnection(Result).SetConnectionType(ctOCIPooled);
    TOCIConnection(Result).Connect(FPoolName);
    Result.Pool := Self;
    InterlockedIncrement(FTakenConnectionsCount);
  except
    Result.Free;
    raise;
  end;
end;

function TOraOCIConnectionPool.InternalPutConnection(CRConnection: TCRConnection): boolean;
begin
  CRConnection.Disconnect;
  CRConnection.Free;
  InterlockedDecrement(FTakenConnectionsCount);
  Result := False;
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}

{ TOraMTSConnectionPool }

function TOraMTSConnectionPool.GetConnection: TCRConnection;
begin
{$IFDEF NET}
  if TOraConnectionParameters(ConnectionParameters).Direct then
    RaiseError(SMTSPoolNotSupportedWithDirect);
{$ENDIF}

  Result := TOCIConnection.Create;
  try
    Result.SetUsername(ConnectionParameters.Username);
    Result.SetPassword(ConnectionParameters.Password);
    Result.SetServer(ConnectionParameters.Server);
    TOraLocalConnectionPool.InitConnectorOraParams(Result, ConnectionParameters);
    TOCIConnection(Result).SetConnectionType(ctMTSPooled);
    TOCIConnection(Result).Connect('');
    Result.Pool := Self;
    InterlockedIncrement(FTakenConnectionsCount);
  except
    Result.Free;
    raise;
  end;
end;

function TOraMTSConnectionPool.InternalPutConnection(CRConnection: TCRConnection): boolean;
begin
  CRConnection.Disconnect;
  CRConnection.Free;
  InterlockedDecrement(FTakenConnectionsCount);
  Result := False;
end;
{$ENDIF}
{$ENDIF}

{ TOraConnectionPoolManager }

class function TOraConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TOraConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TOraConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  case TOraConnectionParameters(ConnectionParameters).PoolingType of
    optLocal : Result := TOraLocalConnectionPool.Create(Self, ConnectionParameters);
    optOCI   : Result := TOraOciConnectionPool.Create(Self, ConnectionParameters);
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    optMTS   : Result := TOraMTSConnectionPool.Create(Self, ConnectionParameters);
  {$ENDIF}
  {$ENDIF}
  else
    Result := nil;
    Assert(False);
  end;
end;

function TOraConnectionPoolManager.InternalGetConnection(
  ConnectionParameters: TCRConnectionParameters): TCRConnection;
var
  ProxyConnection: TCRConnection;
  OraConnectionParams: TOraConnectionParameters;
begin
  FLockGet.Enter;
  try
    Result := inherited InternalGetConnection(ConnectionParameters);
    OraConnectionParams := TOraConnectionParameters(ConnectionParameters);

    if (OraConnectionParams.ProxyUserName <> '') or (OraConnectionParams.ProxyPassword <> '') then begin
      ProxyConnection := Result;
      Result := TOCIConnection.Create;
      Result.SetUsername(OraConnectionParams.ProxyUserName);
      Result.SetPassword(OraConnectionParams.ProxyPassword);
      TOraLocalConnectionPool.InitConnectorOraParams(Result, ConnectionParameters);
      TOCIConnection(Result).ProxyConnection := TOCIConnection(ProxyConnection);
      Result.Pool := ProxyConnection.Pool;
      Result.Connect('');
    end;
  finally
    FLockGet.Leave;
  end;
end;

function TOraConnectionPoolManager.InternalCheckConnection(var Connection: TCRConnection): TCRConnection;
begin
  if TOCIConnection(Connection).ProxyConnection <> nil then begin
    Result := TOCIConnection(Connection).ProxyConnection;
    Connection.Free;
  end
  else
    Result := inherited InternalCheckConnection(Connection);
end;

initialization
  PoolManagerIndex := -1;

end.
