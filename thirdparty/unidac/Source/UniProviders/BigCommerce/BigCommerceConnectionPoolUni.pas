
//////////////////////////////////////////////////
//  BigCommerce Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I BigCommerceDac.inc}
unit BigCommerceConnectionPoolUni;

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
  BigCommerceClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  BigCommerceClassesUni;
{$ENDIF}

type
  TBigCommerceConnectionParameters = class(TODBCConnectionParameters)
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

  TBigCommerceLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TBigCommerceConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  BigCommerceProps;
{$ELSE}
  BigCommercePropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TBigCommerceConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  BigCommerceObj: TBigCommerceConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TBigCommerceConnectionParameters) then begin
    BigCommerceObj := TBigCommerceConnectionParameters(Obj);
    Result :=
      (AuthenticationToken = BigCommerceObj.AuthenticationToken) and
      (ProxyServer = BigCommerceObj.ProxyServer) and
      (ProxyPort = BigCommerceObj.ProxyPort) and
      (ProxyUser = BigCommerceObj.ProxyUser) and
      (ProxyPassword = BigCommerceObj.ProxyPassword);
  end;
end;

function TBigCommerceConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
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

procedure TBigCommerceConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TBigCommerceConnectionParameters then begin
    TBigCommerceConnectionParameters(Dest).AuthenticationToken := AuthenticationToken;
    TBigCommerceConnectionParameters(Dest).ProxyServer := ProxyServer;
    TBigCommerceConnectionParameters(Dest).ProxyPort := ProxyPort;
    TBigCommerceConnectionParameters(Dest).ProxyUser := ProxyUser;
    TBigCommerceConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TBigCommerceLocalConnectionPool }

class function TBigCommerceLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TBigCommerceConnection;
end;

procedure TBigCommerceLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prAuthenticationToken, Variant(TBigCommerceConnectionParameters(ConnectionParameters).AuthenticationToken));
  Connector.SetProp(prProxyHostname, Variant(TBigCommerceConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TBigCommerceConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TBigCommerceConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TBigCommerceConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TBigCommerceConnectionPoolManager }

class function TBigCommerceConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TBigCommerceConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TBigCommerceConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TBigCommerceLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
