
//////////////////////////////////////////////////
//  Dynamics CRM Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I DynamicsDac.inc}
unit DynamicsConnectionPoolUni;


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
  DynamicsClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  DynamicsClassesUni;
{$ENDIF}

type
  TDynamicsConnectionParameters = class(TODBCConnectionParameters)
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

  TDynamicsLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TDynamicsConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  DynamicsProps;
{$ELSE}
  DynamicsPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TDynamicsConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  DynamicsObj: TDynamicsConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TDynamicsConnectionParameters) then begin
    DynamicsObj := TDynamicsConnectionParameters(Obj);
    Result :=
      (ProxyServer = DynamicsObj.ProxyServer) and
      (ProxyPort = DynamicsObj.ProxyPort) and
      (ProxyUser = DynamicsObj.ProxyUser) and
      (ProxyPassword = DynamicsObj.ProxyPassword);
  end;
end;

function TDynamicsConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
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

procedure TDynamicsConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TDynamicsConnectionParameters then begin
    TDynamicsConnectionParameters(Dest).ProxyServer := ProxyServer;
    TDynamicsConnectionParameters(Dest).ProxyPort := ProxyPort;
    TDynamicsConnectionParameters(Dest).ProxyUser := ProxyUser;
    TDynamicsConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TDynamicsLocalConnectionPool }

class function TDynamicsLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TDynamicsConnection;
end;

procedure TDynamicsLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prProxyHostname, Variant(TDynamicsConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TDynamicsConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TDynamicsConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TDynamicsConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TDynamicsConnectionPoolManager }

class function TDynamicsConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TDynamicsConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TDynamicsConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TDynamicsLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
