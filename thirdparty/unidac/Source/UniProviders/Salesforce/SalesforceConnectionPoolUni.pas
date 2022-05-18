
//////////////////////////////////////////////////
//  Salesforce Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SalesforceDac.inc}
unit SalesforceConnectionPoolUni;

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
  SalesforceClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  SalesforceClassesUni;
{$ENDIF}

type
  TSalesforceConnectionParameters = class(TODBCConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    SecurityToken: string;

    ProxyServer: string;
    ProxyPort: Integer;
    ProxyUser: string;
    ProxyPassword: string;
    IncludeDeleted: Boolean;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TSalesforceLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TSalesforceConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  SalesforceProps;
{$ELSE}
  SalesforcePropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TSalesforceConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  SalesforceObj: TSalesforceConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TSalesforceConnectionParameters) then begin
    SalesforceObj := TSalesforceConnectionParameters(Obj);
    Result :=
      (SecurityToken = SalesforceObj.SecurityToken) and
      (ProxyServer = SalesforceObj.ProxyServer) and
      (ProxyPort = SalesforceObj.ProxyPort) and
      (ProxyUser = SalesforceObj.ProxyUser) and
      (ProxyPassword = SalesforceObj.ProxyPassword);
  end;
end;

function TSalesforceConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prSecurityToken:
      SecurityToken := Value;
    prProxyHostname:
      ProxyServer := Value;
    prProxyPort:
      ProxyPort := Value;
    prProxyUsername:
      ProxyUser := Value;
    prProxyPassword:
      ProxyPassword := Value;
    prIncludeDeleted:
      IncludeDeleted := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TSalesforceConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TSalesforceConnectionParameters then begin
    TSalesforceConnectionParameters(Dest).SecurityToken := SecurityToken;
    TSalesforceConnectionParameters(Dest).ProxyServer := ProxyServer;
    TSalesforceConnectionParameters(Dest).ProxyPort := ProxyPort;
    TSalesforceConnectionParameters(Dest).ProxyUser := ProxyUser;
    TSalesforceConnectionParameters(Dest).ProxyPassword := ProxyPassword;
    TSalesforceConnectionParameters(Dest).IncludeDeleted := IncludeDeleted;
  end;
end;

{ TSalesforceLocalConnectionPool }

class function TSalesforceLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TSalesforceConnection;
end;

procedure TSalesforceLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prSecurityToken, Variant(TSalesforceConnectionParameters(ConnectionParameters).SecurityToken));
  Connector.SetProp(prProxyHostname, Variant(TSalesforceConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TSalesforceConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TSalesforceConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TSalesforceConnectionParameters(ConnectionParameters).ProxyPassword));
  Connector.SetProp(prIncludeDeleted, Variant(TSalesforceConnectionParameters(ConnectionParameters).IncludeDeleted));
end;

{ TSalesforceConnectionPoolManager }

class function TSalesforceConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TSalesforceConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TSalesforceConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TSalesforceLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
