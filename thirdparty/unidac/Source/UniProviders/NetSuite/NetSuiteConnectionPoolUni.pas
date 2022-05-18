
//////////////////////////////////////////////////
//  NetSuite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I NetSuiteDac.inc}
unit NetSuiteConnectionPoolUni;


interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, SyncObjs,
  MemUtils, MemData, CRAccess, CRConnectionPool,
{$IFNDEF UNIDACPRO}
  ODBCConnectionPool,
  NetSuiteClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  NetSuiteClassesUni;
{$ENDIF}

type
  TNetSuiteConnectionParameters = class(TODBCConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AccountId: string;
    RoleId: string;
    ApplicationId: string;
    CustomTables: boolean;
    CustomFields: boolean;
    Sandbox: boolean;

    ProxyServer: string;
    ProxyPort: Integer;
    ProxyUser: string;
    ProxyPassword: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TNetSuiteLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TNetSuiteConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  NetSuiteProps;
{$ELSE}
  NetSuitePropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TNetSuiteConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  NetSuiteObj: TNetSuiteConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TNetSuiteConnectionParameters) then begin
    NetSuiteObj := TNetSuiteConnectionParameters(Obj);
    Result :=
      (AccountId = NetSuiteObj.AccountId) and
      (RoleId = NetSuiteObj.RoleId) and
      (ApplicationId = NetSuiteObj.ApplicationId) and
      (CustomTables = NetSuiteObj.CustomTables) and
      (CustomFields = NetSuiteObj.CustomFields) and
      (Sandbox = NetSuiteObj.Sandbox) and

      (ProxyServer = NetSuiteObj.ProxyServer) and
      (ProxyPort = NetSuiteObj.ProxyPort) and
      (ProxyUser = NetSuiteObj.ProxyUser) and
      (ProxyPassword = NetSuiteObj.ProxyPassword);
  end;
end;

function TNetSuiteConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAccountId:
      AccountId := Value;
    prRoleId:
      RoleId := Value;
    prApplicationId:
      ApplicationId := Value;
    prCustomTables:
      CustomTables := Value;
    prCustomFields:
      CustomFields := Value;
    prSandbox:
      Sandbox := Value;

    prProxyHostname:
      ProxyServer := Value;
    prProxyPort:
      ProxyPort := Value;
    prProxyUsername:
      ProxyUser := Value;
    prProxyPassword:
      ProxyPassword := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TNetSuiteConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TNetSuiteConnectionParameters then begin
    TNetSuiteConnectionParameters(Dest).AccountId := AccountId;
    TNetSuiteConnectionParameters(Dest).RoleId := RoleId;
    TNetSuiteConnectionParameters(Dest).ApplicationId := ApplicationId;
    TNetSuiteConnectionParameters(Dest).CustomTables := CustomTables;
    TNetSuiteConnectionParameters(Dest).CustomFields := CustomFields;
    TNetSuiteConnectionParameters(Dest).Sandbox := Sandbox;

    TNetSuiteConnectionParameters(Dest).ProxyServer := ProxyServer;
    TNetSuiteConnectionParameters(Dest).ProxyPort := ProxyPort;
    TNetSuiteConnectionParameters(Dest).ProxyUser := ProxyUser;
    TNetSuiteConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TNetSuiteLocalConnectionPool }

class function TNetSuiteLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TNetSuiteConnection;
end;

procedure TNetSuiteLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prAccountId, Variant(TNetSuiteConnectionParameters(ConnectionParameters).AccountId));
  Connector.SetProp(prRoleId, Variant(TNetSuiteConnectionParameters(ConnectionParameters).AccountId));
  Connector.SetProp(prApplicationId, Variant(TNetSuiteConnectionParameters(ConnectionParameters).ApplicationId));
  Connector.SetProp(prCustomTables, Variant(TNetSuiteConnectionParameters(ConnectionParameters).CustomTables));
  Connector.SetProp(prCustomFields, Variant(TNetSuiteConnectionParameters(ConnectionParameters).CustomFields));
  Connector.SetProp(prSandbox, Variant(TNetSuiteConnectionParameters(ConnectionParameters).Sandbox));

  Connector.SetProp(prProxyHostname, Variant(TNetSuiteConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TNetSuiteConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TNetSuiteConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TNetSuiteConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TNetSuiteConnectionPoolManager }

class function TNetSuiteConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TNetSuiteConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TNetSuiteConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TNetSuiteLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
