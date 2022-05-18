
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DB2Dac.inc}
unit DB2ConnectionPoolUni;

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
  ODBCClasses, ODBCConnectionPool, DB2Classes;
{$ELSE}
  ODBCClassesUni, ODBCConnectionPoolUni, DB2ClassesUni;
{$ENDIF}

type
  TDB2ConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    Database: string;
    Port: integer;
    Schema: string;
    FunctionPath: string;
    ConnectionTimeout: integer;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TDB2LocalConnectionPool = class(TODBCLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TDB2ConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  DB2Props;
{$ELSE}
  DB2PropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TDB2ConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ODBCObj: TDB2ConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TDB2ConnectionParameters) then begin
    ODBCObj := TDB2ConnectionParameters(Obj);
    Result :=
      (Database = ODBCObj.Database) and
      (Port = ODBCObj.Port) and
      (Schema = ODBCObj.Schema) and
      (FunctionPath = ODBCObj.FunctionPath) and
      (ConnectionTimeout = ODBCObj.ConnectionTimeout);
  end;
end;

function TDB2ConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prSchema:
      Schema := Value;
    prFunctionPath:
      FunctionPath := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TDB2ConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TDB2ConnectionParameters then begin
    TDB2ConnectionParameters(Dest).Database := Database;
    TDB2ConnectionParameters(Dest).Port := Port;
    TDB2ConnectionParameters(Dest).Schema := Schema;
    TDB2ConnectionParameters(Dest).FunctionPath := FunctionPath;
    TDB2ConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
  end;
end;

{ TDB2LocalConnectionPool }

class function TDB2LocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TDB2Connection;
end;

procedure TDB2LocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDatabase, TDB2ConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prPort, TDB2ConnectionParameters(ConnectionParameters).Port);
  Connector.SetProp(prSchema, TDB2ConnectionParameters(ConnectionParameters).Schema);
  Connector.SetProp(prFunctionPath, TDB2ConnectionParameters(ConnectionParameters).FunctionPath);
  Connector.SetProp(prConnectionTimeout, TDB2ConnectionParameters(ConnectionParameters).ConnectionTimeout);
  Connector.OnError := ConnectionParameters.OnError;

  inherited;
end;

{ TDB2ConnectionPoolManager }

class function TDB2ConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TDB2ConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TDB2ConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TDB2LocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
