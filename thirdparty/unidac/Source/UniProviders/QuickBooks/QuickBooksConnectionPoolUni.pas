
//////////////////////////////////////////////////
//  QuickBooks Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I QuickBooksDac.inc}
unit QuickBooksConnectionPoolUni;


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
  QuickBooksClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  QuickBooksClassesUni;
{$ENDIF}

type
  TQuickBooksConnectionParameters = class(TODBCConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    CompanyId: string;
    AccessToken: string;
    AccessTokenSecret: string;
    ConsumerKey: string;
    ConsumerKeySecret: string;
    Sandbox: boolean;

    ProxyServer: string;
    ProxyPort: Integer;
    ProxyUser: string;
    ProxyPassword: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TQuickBooksLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TQuickBooksConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  QuickBooksProps;
{$ELSE}
  QuickBooksPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TQuickBooksConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  QuickBooksObj: TQuickBooksConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TQuickBooksConnectionParameters) then begin
    QuickBooksObj := TQuickBooksConnectionParameters(Obj);
    Result :=
      (CompanyId = QuickBooksObj.CompanyId) and
      (AccessToken = QuickBooksObj.AccessToken) and
      (AccessTokenSecret = QuickBooksObj.AccessTokenSecret) and
      (ConsumerKey = QuickBooksObj.ConsumerKey) and
      (ConsumerKeySecret = QuickBooksObj.ConsumerKeySecret) and
      (Sandbox = QuickBooksObj.Sandbox) and

      (ProxyServer = QuickBooksObj.ProxyServer) and
      (ProxyPort = QuickBooksObj.ProxyPort) and
      (ProxyUser = QuickBooksObj.ProxyUser) and
      (ProxyPassword = QuickBooksObj.ProxyPassword);
  end;
end;

function TQuickBooksConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCompanyId:
      CompanyId := Value;
    prAccessToken:
      AccessToken := Value;
    prAccessTokenSecret:
      AccessTokenSecret := Value;
    prConsumerKey:
      ConsumerKey := Value;
    prConsumerKeySecret:
      ConsumerKeySecret := Value;
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

procedure TQuickBooksConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TQuickBooksConnectionParameters then begin
    TQuickBooksConnectionParameters(Dest).CompanyId := CompanyId;
    TQuickBooksConnectionParameters(Dest).AccessToken := AccessToken;
    TQuickBooksConnectionParameters(Dest).AccessTokenSecret := AccessTokenSecret;
    TQuickBooksConnectionParameters(Dest).ConsumerKey := ConsumerKey;
    TQuickBooksConnectionParameters(Dest).ConsumerKeySecret := ConsumerKeySecret;
    TQuickBooksConnectionParameters(Dest).Sandbox := Sandbox;

    TQuickBooksConnectionParameters(Dest).ProxyServer := ProxyServer;
    TQuickBooksConnectionParameters(Dest).ProxyPort := ProxyPort;
    TQuickBooksConnectionParameters(Dest).ProxyUser := ProxyUser;
    TQuickBooksConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TQuickBooksLocalConnectionPool }

class function TQuickBooksLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TQuickBooksConnection;
end;

procedure TQuickBooksLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prCompanyId, Variant(TQuickBooksConnectionParameters(ConnectionParameters).CompanyId));
  Connector.SetProp(prAccessToken, Variant(TQuickBooksConnectionParameters(ConnectionParameters).AccessToken));
  Connector.SetProp(prAccessTokenSecret, Variant(TQuickBooksConnectionParameters(ConnectionParameters).AccessTokenSecret));
  Connector.SetProp(prConsumerKey, Variant(TQuickBooksConnectionParameters(ConnectionParameters).ConsumerKey));
  Connector.SetProp(prConsumerKeySecret, Variant(TQuickBooksConnectionParameters(ConnectionParameters).ConsumerKeySecret));
  Connector.SetProp(prSandbox, Variant(TQuickBooksConnectionParameters(ConnectionParameters).Sandbox));

  Connector.SetProp(prProxyHostname, Variant(TQuickBooksConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TQuickBooksConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TQuickBooksConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TQuickBooksConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TQuickBooksConnectionPoolManager }

class function TQuickBooksConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TQuickBooksConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TQuickBooksConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TQuickBooksLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
