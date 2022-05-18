
//////////////////////////////////////////////////
//  FreshBooks Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I FreshBooksDac.inc}
unit FreshBooksConnectionPoolUni;


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
  FreshBooksConsts, FreshBooksClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  FreshBooksConstsUni, FreshBooksClassesUni;
{$ENDIF}

type
  TFreshBooksConnectionParameters = class(TODBCConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    ApiVersion: TApiVersion;
    AccessToken: string;
    AuthenticationToken: string;
    CompanyName: string;

    ProxyServer: string;
    ProxyPort: Integer;
    ProxyUser: string;
    ProxyPassword: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TFreshBooksLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TFreshBooksConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  FreshBooksProps;
{$ELSE}
  FreshBooksPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TFreshBooksConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  FreshBooksObj: TFreshBooksConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TFreshBooksConnectionParameters) then begin
    FreshBooksObj := TFreshBooksConnectionParameters(Obj);
    Result :=
      (ApiVersion = FreshBooksObj.ApiVersion) and
      (AccessToken = FreshBooksObj.AccessToken) and
      (AuthenticationToken = FreshBooksObj.AuthenticationToken) and
      (CompanyName = FreshBooksObj.CompanyName) and

      (ProxyServer = FreshBooksObj.ProxyServer) and
      (ProxyPort = FreshBooksObj.ProxyPort) and
      (ProxyUser = FreshBooksObj.ProxyUser) and
      (ProxyPassword = FreshBooksObj.ProxyPassword);
  end;
end;

function TFreshBooksConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prApiVersion:
      ApiVersion := TApiVersion(Value);
    prAccessToken:
      AccessToken := Value;
    prAuthenticationToken:
      AuthenticationToken := Value;
    prCompanyName:
      CompanyName := Value;

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

procedure TFreshBooksConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TFreshBooksConnectionParameters then begin
    TFreshBooksConnectionParameters(Dest).ApiVersion := ApiVersion;
    TFreshBooksConnectionParameters(Dest).AccessToken := AccessToken;
    TFreshBooksConnectionParameters(Dest).AuthenticationToken := AuthenticationToken;
    TFreshBooksConnectionParameters(Dest).CompanyName := CompanyName;

    TFreshBooksConnectionParameters(Dest).ProxyServer := ProxyServer;
    TFreshBooksConnectionParameters(Dest).ProxyPort := ProxyPort;
    TFreshBooksConnectionParameters(Dest).ProxyUser := ProxyUser;
    TFreshBooksConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TFreshBooksLocalConnectionPool }

class function TFreshBooksLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TFreshBooksConnection;
end;

procedure TFreshBooksLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prApiVersion, Variant(TFreshBooksConnectionParameters(ConnectionParameters).ApiVersion));
  Connector.SetProp(prAccessToken, Variant(TFreshBooksConnectionParameters(ConnectionParameters).AccessToken));
  Connector.SetProp(prAuthenticationToken, Variant(TFreshBooksConnectionParameters(ConnectionParameters).AuthenticationToken));
  Connector.SetProp(prCompanyName, Variant(TFreshBooksConnectionParameters(ConnectionParameters).CompanyName));

  Connector.SetProp(prProxyHostname, Variant(TFreshBooksConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TFreshBooksConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TFreshBooksConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TFreshBooksConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TFreshBooksConnectionPoolManager }

class function TFreshBooksConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TFreshBooksConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TFreshBooksConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TFreshBooksLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
