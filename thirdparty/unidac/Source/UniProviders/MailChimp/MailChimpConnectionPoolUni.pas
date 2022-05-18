
//////////////////////////////////////////////////
//  MailChimp Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I MailChimpDac.inc}
unit MailChimpConnectionPoolUni;


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
  MailChimpClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  MailChimpClassesUni;
{$ENDIF}

type
  TMailChimpConnectionParameters = class(TODBCConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    ApiKey: string;

    ProxyServer: string;
    ProxyPort: Integer;
    ProxyUser: string;
    ProxyPassword: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TMailChimpLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TMailChimpConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  MailChimpProps;
{$ELSE}
  MailChimpPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TMailChimpConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  MailChimpObj: TMailChimpConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TMailChimpConnectionParameters) then begin
    MailChimpObj := TMailChimpConnectionParameters(Obj);
    Result :=
      (ApiKey = MailChimpObj.ApiKey) and
      (ProxyServer = MailChimpObj.ProxyServer) and
      (ProxyPort = MailChimpObj.ProxyPort) and
      (ProxyUser = MailChimpObj.ProxyUser) and
      (ProxyPassword = MailChimpObj.ProxyPassword);
  end;
end;

function TMailChimpConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prApiKey:
      ApiKey := Value;
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

procedure TMailChimpConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TMailChimpConnectionParameters then begin
    TMailChimpConnectionParameters(Dest).ApiKey := ApiKey;
    TMailChimpConnectionParameters(Dest).ProxyServer := ProxyServer;
    TMailChimpConnectionParameters(Dest).ProxyPort := ProxyPort;
    TMailChimpConnectionParameters(Dest).ProxyUser := ProxyUser;
    TMailChimpConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TMailChimpLocalConnectionPool }

class function TMailChimpLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TMailChimpConnection;
end;

procedure TMailChimpLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prApiKey, Variant(TMailChimpConnectionParameters(ConnectionParameters).ApiKey));
  Connector.SetProp(prProxyHostname, Variant(TMailChimpConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TMailChimpConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TMailChimpConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TMailChimpConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TMailChimpConnectionPoolManager }

class function TMailChimpConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TMailChimpConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TMailChimpConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TMailChimpLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
