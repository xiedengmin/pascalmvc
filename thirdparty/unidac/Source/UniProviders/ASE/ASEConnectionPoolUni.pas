
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASEConnectionPoolUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, SyncObjs,
  CRTypes, MemData, CRAccess, CRConnectionPool, DASQLMonitor,
{$IFNDEF UNIDACPRO}
  {$IFDEF ODBC_PROVIDER}ODBCClasses, ODBCConnectionPool,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ASEClasses,{$ENDIF}
  ASEConnection;
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCClassesUni, ODBCConnectionPoolUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ASEClassesUni,{$ENDIF}
  ASEConnectionUni;
{$ENDIF}

type
  TASEConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;
  public
    Database: string;
    Port: integer;
    ConnectionTimeout: integer;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TASELocalConnectionPool = class({$IFDEF ODBC_PROVIDER}TODBCLocalConnectionPool{$ELSE}TCRLocalConnectionPool{$ENDIF})
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TASEConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
  CRProps, CRFunctions;

var
  PoolManagerIndex: Integer;

{ TASEConnectionPoolParamters }

function TASEConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ASEObj: TASEConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TASEConnectionParameters) then begin
    ASEObj := TASEConnectionParameters(Obj);
    Result :=
      (Database = ASEObj.Database) and
      (Port = ASEObj.Port) and
      (ConnectionTimeout = ASEObj.ConnectionTimeout);
  end;
end;

function TASEConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TASEConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TASEConnectionParameters then begin
    TASEConnectionParameters(Dest).Database := Database;
    TASEConnectionParameters(Dest).Port := Port;
    TASEConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
  end;
end;

function TASEConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString + Format(
    'Port=%d'#13'Database=%s'#13,
    [Port, Database]);
end;

{ TASELocalConnectionPool }

class function TASELocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TASEConnection;
end;

procedure TASELocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDatabase, TASEConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prPort, TASEConnectionParameters(ConnectionParameters).Port);
  Connector.SetProp(prConnectionTimeout, TASEConnectionParameters(ConnectionParameters).ConnectionTimeout);
  Connector.OnError := ConnectionParameters.OnError;

  Connector.SetUsername(ConnectionParameters.UserName);
  Connector.SetPassword(ConnectionParameters.Password);
end;

{ TASEConnectionPoolManager }

class function TASEConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TASEConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TASEConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TASELocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
