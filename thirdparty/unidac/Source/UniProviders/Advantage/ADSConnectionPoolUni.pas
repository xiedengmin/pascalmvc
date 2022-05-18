
//////////////////////////////////////////////////
//  Advantage Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ADSDac.inc}
unit ADSConnectionPoolUni;

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
  ODBCClasses, ODBCConnectionPool, ADSClasses;
{$ELSE}
  ODBCClassesUni, ODBCConnectionPoolUni, ADSClassesUni;
{$ENDIF}

type
  TADSConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;
  public
    Database: string;
    ConnectionTimeout: integer;
    ServerTypes: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TADSLocalConnectionPool = class(TODBCLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TADSConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
  CRProps, CRFunctions,
{$IFNDEF UNIDACPRO}
  ADSProps;
{$ELSE}
  ADSPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TADSConnectionPoolParamters }

function TADSConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ADSObj: TADSConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TADSConnectionParameters) then begin
    ADSObj := TADSConnectionParameters(Obj);
    Result :=
      (Database = ADSObj.Database) and
      (ConnectionTimeout = ADSObj.ConnectionTimeout) and
      SameText(ServerTypes, ADSObj.ServerTypes);
  end;
end;

function TADSConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prADSServerTypes:
      ServerTypes := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TADSConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TADSConnectionParameters then begin
    TADSConnectionParameters(Dest).Database := Database;
    TADSConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
    TADSConnectionParameters(Dest).ServerTypes := ServerTypes;
  end;
end;

function TADSConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString + Format(
    'Database=%s'#13,
    [Database]);
end;

{ TADSLocalConnectionPool }

class function TADSLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TADSConnection;
end;

procedure TADSLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDatabase, TADSConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prConnectionTimeout, TADSConnectionParameters(ConnectionParameters).ConnectionTimeout);
  Connector.SetProp(prADSServerTypes, TADSConnectionParameters(ConnectionParameters).ServerTypes);
  Connector.OnError := ConnectionParameters.OnError;

  Connector.SetUsername(ConnectionParameters.UserName);
  Connector.SetPassword(ConnectionParameters.Password);
end;

{ TADSConnectionPoolManager }

class function TADSConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TADSConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TADSConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TADSLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
