
//////////////////////////////////////////////////
//  Magento Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I MagentoDac.inc}
unit MagentoConnectionPoolUni;


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
  MagentoConsts, MagentoClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  MagentoConstsUni, MagentoClassesUni;
{$ENDIF}

type
  TMagentoConnectionParameters = class(TODBCConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    ApiVersion: TApiVersion;
    Domain: string;
    ApiKey: string;
    Username: string;
    Password: string;

    ProxyServer: string;
    ProxyPort: Integer;
    ProxyUser: string;
    ProxyPassword: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TMagentoLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TMagentoConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  MagentoProps;
{$ELSE}
  MagentoPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TMagentoConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  MagentoObj: TMagentoConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TMagentoConnectionParameters) then begin
    MagentoObj := TMagentoConnectionParameters(Obj);
    Result :=
      (ApiVersion = MagentoObj.ApiVersion) and
      (Domain = MagentoObj.Domain) and
      (ApiKey = MagentoObj.ApiKey) and
      (Username = MagentoObj.Username) and
      (Password = MagentoObj.Password) and

      (ProxyServer = MagentoObj.ProxyServer) and
      (ProxyPort = MagentoObj.ProxyPort) and
      (ProxyUser = MagentoObj.ProxyUser) and
      (ProxyPassword = MagentoObj.ProxyPassword);
  end;
end;

function TMagentoConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prApiVersion:
      ApiVersion := TApiVersion(Value);
    prServer:
      Domain := Value;
    prApiKey:
      ApiKey := Value;
    prUsername:
      Username := Value;
    prPassword:
      Password := Value;

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

procedure TMagentoConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TMagentoConnectionParameters then begin
    TMagentoConnectionParameters(Dest).ApiVersion := ApiVersion;
    TMagentoConnectionParameters(Dest).Domain := Domain;
    TMagentoConnectionParameters(Dest).ApiKey := ApiKey;
    TMagentoConnectionParameters(Dest).Username := Username;
    TMagentoConnectionParameters(Dest).Password := Password;

    TMagentoConnectionParameters(Dest).ProxyServer := ProxyServer;
    TMagentoConnectionParameters(Dest).ProxyPort := ProxyPort;
    TMagentoConnectionParameters(Dest).ProxyUser := ProxyUser;
    TMagentoConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TMagentoLocalConnectionPool }

class function TMagentoLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TMagentoConnection;
end;

procedure TMagentoLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prApiVersion, Variant(TMagentoConnectionParameters(ConnectionParameters).ApiVersion));
  Connector.SetProp(prApiKey, Variant(TMagentoConnectionParameters(ConnectionParameters).ApiKey));
  Connector.SetProp(prServer, Variant(TMagentoConnectionParameters(ConnectionParameters).Domain));
  Connector.SetProp(prUsername, Variant(TMagentoConnectionParameters(ConnectionParameters).Username));
  Connector.SetProp(prPassword, Variant(TMagentoConnectionParameters(ConnectionParameters).Password));

  Connector.SetProp(prProxyHostname, Variant(TMagentoConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TMagentoConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TMagentoConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TMagentoConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TMagentoConnectionPoolManager }

class function TMagentoConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TMagentoConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TMagentoConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TMagentoLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
