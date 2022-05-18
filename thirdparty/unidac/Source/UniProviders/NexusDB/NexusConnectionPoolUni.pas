//////////////////////////////////////////////////
//  NexusDB Data Access Components
//  Copyright © 2009-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I NexusDac.inc}
unit NexusConnectionPoolUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, SyncObjs, Variants,
  CRTypes, CRAccess, CRConnectionPool,
{$IFNDEF UNIDACPRO}
  NexusClasses;
{$ELSE}
  NexusClassesUni;
{$ENDIF}

{$IFNDEF DUMMY}
type
  TNexusConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;
  public
    Database: string;
    ConnectionTimeout: integer;
    HeartbeatInterval: integer;
    LostConnectionTimeout: integer;
    WatchdogInterval: integer;
    CommandTimeout: integer;
    DatabaseReadOnly: boolean;
    Port: string;
    Protocol: TNexusProtocol;
    SecretKey: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TNexusLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TNexusConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;
{$ENDIF} // DUMMY

implementation

uses
  CRProps, CRFunctions, MemData,
{$IFNDEF UNIDACPRO}
  NexusProps;
{$ELSE}
  NexusPropsUni;
{$ENDIF}

{$IFNDEF DUMMY}
var
  PoolManagerIndex: Integer;

{ TNexusConnectionPoolParamters }

function TNexusConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  NexusObj: TNexusConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TNexusConnectionParameters) then begin
    NexusObj := TNexusConnectionParameters(Obj);
    Result :=
      (AnsiCompareText(NexusObj.Database, Database) = 0) and
      (ConnectionTimeout = NexusObj.ConnectionTimeout) and
      (HeartbeatInterval = NexusObj.HeartbeatInterval) and
      (LostConnectionTimeout = NexusObj.LostConnectionTimeout) and
      (WatchdogInterval = NexusObj.WatchdogInterval) and
      (CommandTimeout = NexusObj.CommandTimeout) and
      (DatabaseReadOnly = NexusObj.DatabaseReadOnly) and
      (Port = NexusObj.Port) and
      (Protocol = NexusObj.Protocol) and
      (SecretKey = NexusObj.SecretKey);
  end;
end;

function TNexusConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prHeartbeatInterval:
      HeartbeatInterval := Value;
    prLostConnectionTimeout:
      LostConnectionTimeout := Value;
    prWatchdogInterval:
      WatchdogInterval := Value;
    prCommandTimeout:
      CommandTimeout := Value;
    prDatabaseReadOnly:
      DatabaseReadOnly := Value;
    prProtocol:
      Protocol := TNexusProtocol(Value);
    prSecretKey:
      SecretKey := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TNexusConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TNexusConnectionParameters then begin
    TNexusConnectionParameters(Dest).Database := Database;
    TNexusConnectionParameters(Dest).Port := Port;
    TNexusConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
    TNexusConnectionParameters(Dest).HeartbeatInterval := HeartbeatInterval;
    TNexusConnectionParameters(Dest).LostConnectionTimeout := LostConnectionTimeout;
    TNexusConnectionParameters(Dest).WatchdogInterval := WatchdogInterval;
    TNexusConnectionParameters(Dest).CommandTimeout := CommandTimeout;
    TNexusConnectionParameters(Dest).DatabaseReadOnly := DatabaseReadOnly;
    TNexusConnectionParameters(Dest).Protocol := Protocol;
    TNexusConnectionParameters(Dest).SecretKey := SecretKey;
  end;

  inherited;
end;

function TNexusConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString + Format(
    'Port=%d'#13'Database=%s'#13,
    [Port, Database]);
end;

{ TNexusLocalConnectionPool }

class function TNexusLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TNexusConnection;
end;

procedure TNexusLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDatabase, TNexusConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prPort, TNexusConnectionParameters(ConnectionParameters).Port);
  Connector.SetProp(prConnectionTimeout, TNexusConnectionParameters(ConnectionParameters).ConnectionTimeout);
  Connector.SetProp(prHeartbeatInterval, TNexusConnectionParameters(ConnectionParameters).HeartbeatInterval);
  Connector.SetProp(prLostConnectionTimeout, TNexusConnectionParameters(ConnectionParameters).LostConnectionTimeout);
  Connector.SetProp(prWatchdogInterval, TNexusConnectionParameters(ConnectionParameters).WatchdogInterval);
  Connector.SetProp(prCommandTimeout, TNexusConnectionParameters(ConnectionParameters).CommandTimeout);
  Connector.SetProp(prDatabaseReadOnly, TNexusConnectionParameters(ConnectionParameters).DatabaseReadOnly);
  Connector.SetProp(prProtocol, Integer(TNexusConnectionParameters(ConnectionParameters).Protocol));
  Connector.SetProp(prSecretKey, TNexusConnectionParameters(ConnectionParameters).SecretKey);

  Connector.OnError := ConnectionParameters.OnError;

  inherited;
end;

{ TNexusConnectionPoolManager }

class function TNexusConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TNexusConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TNexusConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TNexusLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

{$ENDIF} // DUMMY

end.
