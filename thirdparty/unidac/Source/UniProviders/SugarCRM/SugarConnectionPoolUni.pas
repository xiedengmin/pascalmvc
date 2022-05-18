
//////////////////////////////////////////////////
//  SugarCRM Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I SugarDac.inc}
unit SugarConnectionPoolUni;


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
  SugarClasses;
{$ELSE}
  ODBCConnectionPoolUni,
  SugarClassesUni;
{$ENDIF}

type
  TSugarConnectionParameters = class(TODBCConnectionParameters)
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

  TSugarLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TSugarConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
  
implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  SugarProps;
{$ELSE}
  SugarPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TSugarConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  SugarObj: TSugarConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TSugarConnectionParameters) then begin
    SugarObj := TSugarConnectionParameters(Obj);
    Result :=
      (ProxyServer = SugarObj.ProxyServer) and
      (ProxyPort = SugarObj.ProxyPort) and
      (ProxyUser = SugarObj.ProxyUser) and
      (ProxyPassword = SugarObj.ProxyPassword);
  end;
end;

function TSugarConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
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

procedure TSugarConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TSugarConnectionParameters then begin
    TSugarConnectionParameters(Dest).ProxyServer := ProxyServer;
    TSugarConnectionParameters(Dest).ProxyPort := ProxyPort;
    TSugarConnectionParameters(Dest).ProxyUser := ProxyUser;
    TSugarConnectionParameters(Dest).ProxyPassword := ProxyPassword;
  end;
end;

{ TSugarLocalConnectionPool }

class function TSugarLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TSugarConnection;
end;

procedure TSugarLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  inherited InitConnectorParams(Connector);

  Connector.SetProp(prProxyHostname, Variant(TSugarConnectionParameters(ConnectionParameters).ProxyServer));
  Connector.SetProp(prProxyPort, Variant(TSugarConnectionParameters(ConnectionParameters).ProxyPort));
  Connector.SetProp(prProxyUsername, Variant(TSugarConnectionParameters(ConnectionParameters).ProxyUser));
  Connector.SetProp(prProxyPassword, Variant(TSugarConnectionParameters(ConnectionParameters).ProxyPassword));
end;

{ TSugarConnectionPoolManager }

class function TSugarConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TSugarConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TSugarConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TSugarLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
