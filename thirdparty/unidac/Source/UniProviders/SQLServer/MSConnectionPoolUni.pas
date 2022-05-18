
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Sdac.inc}
unit MSConnectionPoolUni;

interface

uses
  Classes, Variants,
  CRConnectionPool, CRAccess, CRTypes, CRVio,
{$IFNDEF UNIDACPRO}
  MSConsts, MSClasses;
{$ELSE}
  MSConstsUni, MSClassesUni;
{$ENDIF}

type
  TMSConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;

  public
    Database: string;
    Port: integer;
    IsolationLevel: TCRIsolationLevel;
    Provider: TMSProvider;
    IPVersion: TIPVersion;
  {$IFDEF MSWINDOWS}
    Authentication: TMSAuthentication;
    CompactVersion: TCompactVersion;
    NativeClientVersion: TNativeClientVersion;
  {$ENDIF}

    QuotedIdentifier: boolean;
    Language: string;
    Encrypt: boolean;
    PersistSecurityInfo: boolean;
    AutoTranslate: boolean;
    MultipleConnections: boolean;
    NetworkLibrary: string;
    ApplicationName: string;
    WorkstationID: string;
    PacketSize: integer;
    TrustServerCertificate: boolean;
    ForceCreateDatabase: boolean;
    ApplicationIntent: TApplicationIntent;
    MultiSubnetFailover: boolean;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TMSLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TMSConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, SyncObjs, CRProps, CRFunctions, MemData, 
{$IFNDEF UNIDACPRO}
  MSProps;
{$ELSE}
  MSPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TMSConnectionParameters}

procedure TMSConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TMSConnectionParameters then begin
    TMSConnectionParameters(Dest).Database := Database;
    TMSConnectionParameters(Dest).Port := Port;
    TMSConnectionParameters(Dest).IsolationLevel := IsolationLevel;
    TMSConnectionParameters(Dest).Provider := Provider;
    TMSConnectionParameters(Dest).IPVersion := IPVersion;
  {$IFDEF MSWINDOWS}
    TMSConnectionParameters(Dest).Authentication := Authentication;
    TMSConnectionParameters(Dest).CompactVersion := CompactVersion;
    TMSConnectionParameters(Dest).NativeClientVersion := NativeClientVersion;
  {$ENDIF}

    TMSConnectionParameters(Dest).QuotedIdentifier := QuotedIdentifier;
    TMSConnectionParameters(Dest).Language := Language;
    TMSConnectionParameters(Dest).Encrypt := Encrypt;
    TMSConnectionParameters(Dest).PersistSecurityInfo := PersistSecurityInfo;
    TMSConnectionParameters(Dest).AutoTranslate := AutoTranslate;
    TMSConnectionParameters(Dest).MultipleConnections := MultipleConnections;
    TMSConnectionParameters(Dest).NetworkLibrary := NetworkLibrary;
    TMSConnectionParameters(Dest).ApplicationName := ApplicationName;
    TMSConnectionParameters(Dest).WorkstationID := WorkstationID;
    TMSConnectionParameters(Dest).PacketSize := PacketSize;
    TMSConnectionParameters(Dest).TrustServerCertificate := TrustServerCertificate;
    TMSConnectionParameters(Dest).ForceCreateDatabase := ForceCreateDatabase;
    TMSConnectionParameters(Dest).ApplicationIntent := ApplicationIntent;
    TMSConnectionParameters(Dest).MultiSubnetFailover := MultiSubnetFailover;
  end;

  inherited;
end;

function TMSConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString + Format(
    'Database=%s'#13,
    [Database]);
end;

function TMSConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  O: TMSConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TMSConnectionParameters) then begin
    O := TMSConnectionParameters(Obj);
    Result :=
      (CompareText(Database, O.Database) = 0) and
      (O.Port = Port) and
      (O.IsolationLevel = IsolationLevel) and
      (O.Provider = Provider) and
      (O.IPVersion = IPVersion) and
      (O.QuotedIdentifier = QuotedIdentifier) and
      (CompareText(O.Language, Language) = 0)and
      (O.Encrypt = Encrypt) and
      (O.PersistSecurityInfo = PersistSecurityInfo) and
      (O.AutoTranslate = AutoTranslate) and
      (O.MultipleConnections = MultipleConnections) and
      (CompareText(O.NetworkLibrary, NetworkLibrary) = 0) and
      (CompareText(O.ApplicationName, ApplicationName) = 0) and
      (CompareText(O.WorkstationID, WorkstationID) = 0) and
      (O.PacketSize = PacketSize) and
      (O.TrustServerCertificate = TrustServerCertificate) and
      (O.ForceCreateDatabase = ForceCreateDatabase) and
      (O.ApplicationIntent = ApplicationIntent) and
      (O.MultiSubnetFailover = MultiSubnetFailover)
    {$IFDEF MSWINDOWS}
      and (O.Authentication = Authentication)
    {$ENDIF}
      ;
    {$IFDEF MSWINDOWS}
    if Result then begin
      if Provider = prCompact then
        Result := O.CompactVersion = CompactVersion
      else
      if Provider = prNativeClient then
        Result := O.NativeClientVersion = NativeClientVersion;
    end;
    {$ENDIF}
  end;
end;

function TMSConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prIsolationLevel:
      IsolationLevel := TCRIsolationLevel(Value);
    prProvider:
      Provider := TMSProvider(Value);
    prIPVersion:
      IPVersion := TIPVersion(Value);
  {$IFDEF MSWINDOWS}
    prAuthentication:
      Authentication := TMSAuthentication(Value);
    prCompactVersion:
      CompactVersion := TCompactVersion(CompactVersion);
    prNativeClientVersion:
      NativeClientVersion := TNativeClientVersion(NativeClientVersion);
  {$ENDIF}
    prApplicationIntent:
      ApplicationIntent := TApplicationIntent(Value);
    prQuotedIdentifier:
      QuotedIdentifier := Value;
    prLanguage:
      Language := Value;
    prEncrypt:
      Encrypt := Value;
    prPersistSecurityInfo:
      PersistSecurityInfo := Value;
    prAutoTranslate:
      AutoTranslate := Value;
    prMultipleConnections:
      MultipleConnections := Value;
    prNetworkLibrary:
      NetworkLibrary := Value;
    prApplicationName:
      ApplicationName := Value;
    prWorkstationID:
      WorkstationID := Value;
    prPacketSize:
      PacketSize := Value;
    prTrustServerCertificate:
      TrustServerCertificate := Value;
    prForceCreateDatabase:
      ForceCreateDatabase := Value;
    prMultiSubnetFailover:
      MultiSubnetFailover := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TMSLocalConnectionPool }

class function TMSLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TMSSQLConnection;
end;

procedure TMSLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDatabase, TMSConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prPort, TMSConnectionParameters(ConnectionParameters).Port);
  if TMSConnectionParameters(ConnectionParameters).Provider = prDirect then
    Connector.SetProp(prServer, ConnectionParameters.Server);
  Connector.SetProp(prProvider, Variant(TMSConnectionParameters(ConnectionParameters).Provider));
  Connector.SetProp(prIPVersion, Variant(TMSConnectionParameters(ConnectionParameters).IPVersion));
{$IFDEF MSWINDOWS}
  Connector.SetProp(prAuthentication, Variant(TMSConnectionParameters(ConnectionParameters).Authentication));
  Connector.SetProp(prCompactVersion, Variant(TMSConnectionParameters(ConnectionParameters).CompactVersion));
  Connector.SetProp(prNativeClientVersion, Variant(TMSConnectionParameters(ConnectionParameters).NativeClientVersion));
{$ENDIF}

  Connector.SetProp(prQuotedIdentifier, TMSConnectionParameters(ConnectionParameters).QuotedIdentifier);
  Connector.SetProp(prLanguage, TMSConnectionParameters(ConnectionParameters).Language);
  Connector.SetProp(prEncrypt, TMSConnectionParameters(ConnectionParameters).Encrypt);
  Connector.SetProp(prPersistSecurityInfo, TMSConnectionParameters(ConnectionParameters).PersistSecurityInfo);
  Connector.SetProp(prAutoTranslate, TMSConnectionParameters(ConnectionParameters).AutoTranslate);
  Connector.SetProp(prMultipleConnections, TMSConnectionParameters(ConnectionParameters).MultipleConnections);
  Connector.SetProp(prNetworkLibrary, TMSConnectionParameters(ConnectionParameters).NetworkLibrary);
  Connector.SetProp(prApplicationName, TMSConnectionParameters(ConnectionParameters).ApplicationName);
  Connector.SetProp(prWorkstationID, TMSConnectionParameters(ConnectionParameters).WorkstationID);
  Connector.SetProp(prPacketSize, TMSConnectionParameters(ConnectionParameters).PacketSize);
  Connector.SetProp(prTrustServerCertificate, TMSConnectionParameters(ConnectionParameters).TrustServerCertificate);
  Connector.SetProp(prForceCreateDatabase, TMSConnectionParameters(ConnectionParameters).ForceCreateDatabase);
  Connector.SetProp(prApplicationIntent, Variant(TMSConnectionParameters(ConnectionParameters).ApplicationIntent));
  Connector.SetProp(prIsolationLevel, TMSConnectionParameters(ConnectionParameters).IsolationLevel);
  Connector.SetProp(prMultiSubnetFailover, TMSConnectionParameters(ConnectionParameters).MultiSubnetFailover);
  Connector.OnError := ConnectionParameters.OnError;

  Connector.GetInternalTransaction.SetProp(prIsolationLevel, Variant(TMSConnectionParameters(ConnectionParameters).IsolationLevel));

  inherited;
end;

{ TMSConnectionPoolManager }

class function TMSConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TMSConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TMSConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TMSLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
