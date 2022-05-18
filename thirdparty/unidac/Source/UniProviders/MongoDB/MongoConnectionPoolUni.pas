
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoConnectionPoolUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, SyncObjs, Variants,
  CRTypes, CRAccess, CRConnectionPool, CRVio,
{$IFNDEF UNIDACPRO}
  MongoClasses;
{$ELSE}
  MongoClassesUni;
{$ENDIF}

type
  TMongoConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;
  public
    Database: string;
    Port: integer;
    UseUnicode: boolean;
    ClientLibrary,
    BSONLibrary,
    AdditionalServers,
    ConnectionOptions: string;
    DescribeAmount: integer;
    LowercaseObjectId,
    SQLEngine: boolean;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TMongoLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TMongoConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
  CRProps, CRFunctions, MemData,
{$IFNDEF UNIDACPRO}
  MongoProps;
{$ELSE}
  MongoPropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TMongoConnectionParameters }

function TMongoConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  Params: TMongoConnectionParameters;
begin
  Result := inherited Equals(Obj);

  if Result and (Obj is TMongoConnectionParameters) then begin
    Params := TMongoConnectionParameters(Obj);

    Result :=
      (Port = Params.Port) and
      SameText(Database, Params.Database) and
      (UseUnicode = Params.UseUnicode) and
      SameText(ClientLibrary, Params.ClientLibrary) and
      SameText(BSONLibrary, Params.BSONLibrary) and
      SameText(AdditionalServers, Params.AdditionalServers) and
      SameText(ConnectionOptions, Params.ConnectionOptions) and
      (DescribeAmount = Params.DescribeAmount) and
      (LowercaseObjectId = Params.LowercaseObjectId) and
      (SQLEngine = Params.SQLEngine);
  end;
end;

function TMongoConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prPort:
      Port := Value;
    prDatabase:
      Database := Value;
    prUseUnicode:
      UseUnicode := Value;
    prClientLibrary:
      ClientLibrary := Value;
    prBSONLibrary:
      BSONLibrary := Value;
    prAdditionalServers:
      AdditionalServers := Value;
    prConnectionOptions:
      ConnectionOptions := Value;
    prDescribeAmount:
      DescribeAmount := Value;
    prLowercaseObjectId:
      LowercaseObjectId := Value;
    prSQLEngine:
      SQLEngine := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TMongoConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TMongoConnectionParameters then begin
    TMongoConnectionParameters(Dest).Port := Port;
    TMongoConnectionParameters(Dest).Database := Database;
    TMongoConnectionParameters(Dest).UseUnicode := UseUnicode;
    TMongoConnectionParameters(Dest).ClientLibrary := ClientLibrary;
    TMongoConnectionParameters(Dest).BSONLibrary := BSONLibrary;
    TMongoConnectionParameters(Dest).AdditionalServers := AdditionalServers;
    TMongoConnectionParameters(Dest).ConnectionOptions := ConnectionOptions;
    TMongoConnectionParameters(Dest).DescribeAmount := DescribeAmount;
    TMongoConnectionParameters(Dest).LowercaseObjectId := LowercaseObjectId;
    TMongoConnectionParameters(Dest).SQLEngine := SQLEngine;
  end;
end;

function TMongoConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString + Format(
    'Port=%d'#13'Database=%s'#13,
    [Port, Database]);
end;

{ TMongoLocalConnectionPool }

class function TMongoLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TMongoConnection;
end;

procedure TMongoLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prPort, TMongoConnectionParameters(ConnectionParameters).Port);
  Connector.SetProp(prDatabase, TMongoConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prUseUnicode, TMongoConnectionParameters(ConnectionParameters).UseUnicode);
  Connector.SetProp(prClientLibrary, TMongoConnectionParameters(ConnectionParameters).ClientLibrary);
  Connector.SetProp(prBSONLibrary, TMongoConnectionParameters(ConnectionParameters).BSONLibrary);
  Connector.SetProp(prAdditionalServers, TMongoConnectionParameters(ConnectionParameters).AdditionalServers);
  Connector.SetProp(prConnectionOptions, TMongoConnectionParameters(ConnectionParameters).ConnectionOptions);
  Connector.SetProp(prDescribeAmount, TMongoConnectionParameters(ConnectionParameters).DescribeAmount);
  Connector.SetProp(prLowercaseObjectId, TMongoConnectionParameters(ConnectionParameters).LowercaseObjectId);

  Connector.IOHandler := ConnectionParameters.IOHandler;
  Connector.OnError := ConnectionParameters.OnError;

  inherited;
end;

{ TMongoConnectionPoolManager }

class function TMongoConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TMongoConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TMongoConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TMongoLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
