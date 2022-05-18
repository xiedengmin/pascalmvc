
//////////////////////////////////////////////////
//  Zoho CRM Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I ZohoDac.inc}
unit ZohoConnectionPoolUni;


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
  ZohoClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  ZohoClassesUni;
{$ENDIF}

type
  TZohoConnectionParameters = class(TODBCConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AuthenticationToken: string;

    ProxyServer: string;
    ProxyPort: Integer;
    ProxyUser: string;
    ProxyPassword: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TZohoLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TZohoConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  ZohoProps;
{$ELSE}
  ZohoPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TZohoConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ZohoObj: TZohoConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TZohoConnectionParameters) then begin
    ZohoObj := TZohoConnectionParameters(Obj);
    Result :=
      (AuthenticationToken = ZohoObj.AuthenticationToken) and
      (ProxyServer = ZohoObj.ProxyServer) and
      (ProxyPort = ZohoObj.ProxyPort) and
      (ProxyUser = ZohoObj.ProxyUser) and
      (ProxyPassword = ZohoObj.ProxyPassword);
  end;
end;

function TZohoConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAuthenticationToken:
      AuthenticationToken := Value;
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

procedure TZohoConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TZohoConnectionParameters then begin
    TZohoConnectionParameters(Dest).AuthenticationToken := AuthenticationToken;
    TZohoConnectionParameters(Dest).ProxyServer := ProxyServer;
    TZohoConnectionParameters(Dest).ProxyPort := ProxyPort;
    TZohoConnectionParameters(Dest).ProxyUser := ProxyUser;
    TZohoConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TZohoLocalConnectionPool }

class function TZohoLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TZohoConnection;
end;

procedure TZohoLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prAuthenticationToken, Variant(TZohoConnectionParameters(ConnectionParameters).AuthenticationToken));
  Connector.SetProp(prProxyHostname, Variant(TZohoConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TZohoConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TZohoConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TZohoConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TZohoConnectionPoolManager }

class function TZohoConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TZohoConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TZohoConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TZohoLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
