
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I PgDac.inc}
unit PgConnectionPoolUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, SyncObjs, Variants,
  CRTypes, CRAccess, CRConnectionPool, CRVio,
{$IFNDEF UNIDACPRO}
  PgClasses;
{$ELSE}
  PgClassesUni;
{$ENDIF}

type
  TPgConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;
  public
    Database: string;
    Port: integer;
    ProtocolVersion: TProtocolVersion;
    ApplicationName: string;
    Charset,
    MessagesCharset: string;
    UseUnicode: boolean;
    Schema: string;
    ConnectionTimeout: integer;
    IPVersion: TIPVersion;
    RedshiftConnection: boolean;
    MultipleConnections: boolean;

    SSLMode: TSSLMode;

    constructor Create; override;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TPgLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TPgConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
  CRProps, CRFunctions, MemData,
{$IFNDEF UNIDACPRO}
  PgProps;
{$ELSE}
  PgPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TPgConnectionParameters }

constructor TPgConnectionParameters.Create;
begin
  inherited;

  CreateSecureOptions;
end;

function TPgConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  PgObj: TPgConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TPgConnectionParameters) then begin
    PgObj := TPgConnectionParameters(Obj);
    Result :=
      SameText(Database, PgObj.Database) and
      (Port = PgObj.Port) and
      (ProtocolVersion = PgObj.ProtocolVersion) and
      SameText(Charset, PgObj.Charset) and
      SameText(MessagesCharset, PgObj.MessagesCharset) and
      (UseUnicode = PgObj.UseUnicode) and
      SameText(Schema, PgObj.Schema) and
      (ConnectionTimeout = PgObj.ConnectionTimeout) and
      SameText(ApplicationName, PgObj.ApplicationName) and
      (IPVersion = PgObj.IPVersion) and
      (RedshiftConnection = PgObj.RedshiftConnection) and
      (MultipleConnections = PgObj.MultipleConnections) and
      (SSLMode = PgObj.SSLMode);
  end;
end;

function TPgConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prCharset:
      Charset := Value;
    prMessagesCharset:
      MessagesCharset := Value;
    prUseUnicode:
      UseUnicode := Value;
    prSchema:
      Schema := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prProtocolVersion:
      ProtocolVersion := TProtocolVersion(Value);
    prApplicationName:
      ApplicationName:= Value;
    prIPVersion:
      IPVersion := Value;
    prRedshiftConnection:
      RedshiftConnection := Value;
    prMultipleConnections:
      MultipleConnections := Value;

    prSSLMode:
      SSLMode := TSSLMode(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TPgConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TPgConnectionParameters then begin
    TPgConnectionParameters(Dest).Database := Database;
    TPgConnectionParameters(Dest).Port := Port;
    TPgConnectionParameters(Dest).ProtocolVersion := ProtocolVersion;
    TPgConnectionParameters(Dest).Charset := Charset;
    TPgConnectionParameters(Dest).MessagesCharset := MessagesCharset;
    TPgConnectionParameters(Dest).UseUnicode := UseUnicode;
    TPgConnectionParameters(Dest).Schema := Schema;
    TPgConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
    TPgConnectionParameters(Dest).ApplicationName := ApplicationName;
    TPgConnectionParameters(Dest).IPVersion := IPVersion;
    TPgConnectionParameters(Dest).RedshiftConnection := RedshiftConnection;
    TPgConnectionParameters(Dest).MultipleConnections := MultipleConnections;
    TPgConnectionParameters(Dest).SSLMode := SSLMode;
  end;
end;

function TPgConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString + Format(
    'Port=%d'#13'Database=%s'#13,
    [Port, Database]);
end;

{ TPgLocalConnectionPool }

class function TPgLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TPgSQLConnection;
end;

procedure TPgLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDatabase, TPgConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prPort, TPgConnectionParameters(ConnectionParameters).Port);
  Connector.SetProp(prProtocolVersion, Variant(TPgConnectionParameters(ConnectionParameters).ProtocolVersion));
  Connector.SetProp(prCharset, TPgConnectionParameters(ConnectionParameters).Charset);
  Connector.SetProp(prMessagesCharset, TPgConnectionParameters(ConnectionParameters).MessagesCharset);
  Connector.SetProp(prUseUnicode, TPgConnectionParameters(ConnectionParameters).UseUnicode);
  Connector.SetProp(prSchema, TPgConnectionParameters(ConnectionParameters).Schema);
  Connector.SetProp(prConnectionTimeout, TPgConnectionParameters(ConnectionParameters).ConnectionTimeout);
  Connector.SetProp(prApplicationName, TPgConnectionParameters(ConnectionParameters).ApplicationName);
  Connector.SetProp(prIPVersion, TPgConnectionParameters(ConnectionParameters).IPVersion);
  Connector.SetProp(prRedshiftConnection, TPgConnectionParameters(ConnectionParameters).RedshiftConnection);
  Connector.SetProp(prMultipleConnections, TPgConnectionParameters(ConnectionParameters).MultipleConnections);
  Connector.SetProp(prSSLMode, Integer(TPgConnectionParameters(ConnectionParameters).SSLMode));

  Connector.IOHandler := ConnectionParameters.IOHandler;
  Connector.OnError := ConnectionParameters.OnError;

  inherited;
end;

{ TPgConnectionPoolManager }

class function TPgConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TPgConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TPgConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TPgLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
