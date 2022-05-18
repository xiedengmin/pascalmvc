
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ODBCDac.inc}
unit ODBCConnectionPoolUni;

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
  ODBCClasses, ODBCCall;
{$ELSE}
  ODBCClassesUni, ODBCCallUni;
{$ENDIF}

type
  TODBCConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    DSNType: TDSNType;
    ConnectionTimeout: integer;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TODBCLocalConnectionPool = class(TCRLocalConnectionPool)
  private
    FODBCEnv: IODBCEnvironment;
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
    procedure InternalReturnConnection(Connection: TCRConnection; Version: integer); override;
  public
    destructor Destroy; override;
  end;

  TODBCConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  ODBCProps;
{$ELSE}
  ODBCPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionPoolParamters }

function TODBCConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ODBCObj: TODBCConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TODBCConnectionParameters) then begin
    ODBCObj := TODBCConnectionParameters(Obj);
    Result :=
      (DSNType = ODBCObj.DSNType) and
      (ConnectionTimeout = ODBCObj.ConnectionTimeout);
  end;
end;

function TODBCConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDSNType:
      DSNType := TDSNType(Value);
    prConnectionTimeout:
      ConnectionTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TODBCConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TODBCConnectionParameters then begin
    TODBCConnectionParameters(Dest).DSNType := DSNType;
    TODBCConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
  end;
end;

{ TODBCLocalConnectionPool }

class function TODBCLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TODBCConnection;
end;

procedure TODBCLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDSNType, Variant(TODBCConnectionParameters(ConnectionParameters).DSNType));
  Connector.SetProp(prConnectionTimeout, TODBCConnectionParameters(ConnectionParameters).ConnectionTimeout);
  Connector.OnError := ConnectionParameters.OnError;

  inherited;
end;

destructor TODBCLocalConnectionPool.Destroy;
begin
  FODBCEnv := nil;

  inherited;
end;

procedure TODBCLocalConnectionPool.InternalReturnConnection(Connection: TCRConnection; Version: integer);
begin
  inherited;

  FODBCEnv := TODBCConnection(Connection).ODBCEnv;
end;

{ TODBCConnectionPoolManager }

class function TODBCConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TODBCConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TODBCConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TODBCLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
