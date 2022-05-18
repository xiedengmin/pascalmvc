
//////////////////////////////////////////////////
//  Salesforce Marketing Cloud Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I ExactTargetDac.inc}
unit ExactTargetConnectionPoolUni;


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
  ExactTargetClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  ExactTargetClassesUni;
{$ENDIF}

type
  TExactTargetConnectionParameters = class(TODBCConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    ProxyServer: string;
    ProxyPort: Integer;
    ProxyUser: string;
    ProxyPassword: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TExactTargetLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TExactTargetConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  ExactTargetProps;
{$ELSE}
  ExactTargetPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TExactTargetConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ExactTargetObj: TExactTargetConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TExactTargetConnectionParameters) then begin
    ExactTargetObj := TExactTargetConnectionParameters(Obj);
    Result :=
      (ProxyServer = ExactTargetObj.ProxyServer) and
      (ProxyPort = ExactTargetObj.ProxyPort) and
      (ProxyUser = ExactTargetObj.ProxyUser) and
      (ProxyPassword = ExactTargetObj.ProxyPassword);
  end;
end;

function TExactTargetConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
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

procedure TExactTargetConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TExactTargetConnectionParameters then begin
    TExactTargetConnectionParameters(Dest).ProxyServer := ProxyServer;
    TExactTargetConnectionParameters(Dest).ProxyPort := ProxyPort;
    TExactTargetConnectionParameters(Dest).ProxyUser := ProxyUser;
    TExactTargetConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TExactTargetLocalConnectionPool }

class function TExactTargetLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TExactTargetConnection;
end;

procedure TExactTargetLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prProxyHostname, Variant(TExactTargetConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TExactTargetConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TExactTargetConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TExactTargetConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TExactTargetConnectionPoolManager }

class function TExactTargetConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TExactTargetConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TExactTargetConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TExactTargetLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
