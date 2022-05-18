
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MyConnectionPoolUni;

interface

uses
  Classes, CRConnectionPool, CRAccess, CRTypes, MemData, CRVio,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  MyClasses;
{$ELSE}
  MyClassesUni;
{$ENDIF}

type
  TMyConnectionParameters = class(TCRConnectionParameters)
  protected
    FEmbParams: TStrings;

    procedure SetEmbParams(Value: TStrings);
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;
  public
    Database: string;
    Port: integer;
    ConnectionTimeout: integer;
    Compress: boolean;
    UseUnicode: boolean;
    Charset: string;
    Protocol: TMyProtocol;
    Embedded: boolean;
  {$IFDEF HAVE_DIRECT}
    Direct: boolean;
    IPVersion: TIPVersion;
  {$ENDIF}
    Interactive: boolean;
    IsolationLevel: TCRIsolationLevel;

    constructor Create; override;
    destructor Destroy; override;

    function Equals(Parameters: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    property EmbParams: TStrings read FEmbParams write SetEmbParams;
  end;

  TMyLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TMyConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF HAVE_DIRECT}
  {$IFNDEF UNIDACPRO}MySqlApiDirect,{$ELSE}MySqlApiDirectUni,{$ENDIF}
{$ENDIF}
  SysUtils, SyncObjs, CRProps, CRFunctions,
  {$IFNDEF UNIDACPRO}MyProps, MySqlApi;{$ELSE}MyPropsUni, MySqlApiUni;{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TMyConnectionParameters}

constructor TMyConnectionParameters.Create;
begin
  inherited;

  CreateSecureOptions;

  FEmbParams := TStringList.Create;
  Direct := True;
end;

destructor TMyConnectionParameters.Destroy;
begin
  FEmbParams.Free;
  inherited;
end;

procedure TMyConnectionParameters.SetEmbParams(Value: TStrings);
begin
  FEmbParams.Assign(Value);
end;

procedure TMyConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TMyConnectionParameters then begin
    TMyConnectionParameters(Dest).Database := Database;
    TMyConnectionParameters(Dest).Port := Port;
    TMyConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
    TMyConnectionParameters(Dest).Compress := Compress;
    TMyConnectionParameters(Dest).UseUnicode := UseUnicode;
    TMyConnectionParameters(Dest).Charset := Charset;
    TMyConnectionParameters(Dest).Protocol := Protocol;
    TMyConnectionParameters(Dest).Embedded := Embedded;

  {$IFDEF HAVE_DIRECT}
    TMyConnectionParameters(Dest).Direct := Direct;
    TMyConnectionParameters(Dest).IPVersion := IPVersion;
  {$ENDIF}
    TMyConnectionParameters(Dest).Interactive := Interactive;
    TMyConnectionParameters(Dest).IsolationLevel := IsolationLevel;
    TMyConnectionParameters(Dest).EmbParams.Assign(EmbParams);
  end;

  inherited;
end;

function TMyConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString + Format(
    'Port=%d'#13'Database=%s'#13,
    [Port, Database]);
end;

function TMyConnectionParameters.Equals(Parameters: TCRConnectionParameters): boolean;
var
  MyParameters: TMyConnectionParameters;
begin
  Result := inherited Equals(Parameters);
  if Result and (Parameters is TMyConnectionParameters) then begin
    MyParameters := TMyConnectionParameters(Parameters);
    Result :=
    {$IFDEF HAVE_DIRECT}
      (MyParameters.Direct = Direct) and
      (MyParameters.IPVersion = IPVersion) and
    {$ENDIF}
      (AnsiCompareText(MyParameters.Database, Database) = 0) and
      (MyParameters.Port = Port) and
      (MyParameters.ConnectionTimeout = ConnectionTimeout) and
      (MyParameters.Compress = Compress) and
      (MyParameters.UseUnicode = UseUnicode) and
      (MyParameters.Charset = Charset) and
      (MyParameters.Protocol = Protocol) and
      (MyParameters.Embedded = Embedded) and
      (MyParameters.Interactive = Interactive) and
      (MyParameters.IsolationLevel = IsolationLevel);
  end;
end;

function TMyConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prCharset:
      Charset := Value;
    prUseUnicode:
      UseUnicode := Value;
    prCompress:
      Compress := Value;
    prProtocol:
      Protocol := TMyProtocol(Value);
    prEmbedded:
      Embedded := Value;
    prEmbParams:
      FEmbParams.Text := Trim(string(Value));
  {$IFDEF HAVE_DIRECT}
    prDirect:
      Direct := Value;
    prIPVersion:
      IPVersion := Value;
  {$ENDIF}
    prInteractive:
      Interactive := Value;
    prIsolationLevel:
      IsolationLevel := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TMyLocalConnectionPool }

class function TMyLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TMySQLConnection;
end;

procedure TMyLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDatabase, TMyConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prConnectionTimeout, TMyConnectionParameters(ConnectionParameters).ConnectionTimeout);
  Connector.SetProp(prPort, TMyConnectionParameters(ConnectionParameters).Port);
  Connector.SetProp(prEmbedded, TMyConnectionParameters(ConnectionParameters).Embedded);
{$IFDEF HAVE_DIRECT}
  Connector.SetProp(prDirect, TMyConnectionParameters(ConnectionParameters).Direct);
  Connector.SetProp(prIPVersion, TMyConnectionParameters(ConnectionParameters).IPVersion);
{$ENDIF}
  Connector.SetProp(prCompress, TMyConnectionParameters(ConnectionParameters).Compress);
  Connector.SetProp(prUseUnicode, TMyConnectionParameters(ConnectionParameters).UseUnicode);
  Connector.SetProp(prCharset, TMyConnectionParameters(ConnectionParameters).Charset);
  Connector.SetProp(prProtocol, Integer(TMyConnectionParameters(ConnectionParameters).Protocol));
  Connector.SetProp(prInteractive, TMyConnectionParameters(ConnectionParameters).Interactive);
  Connector.SetProp(prIsolationLevel, TMyConnectionParameters(ConnectionParameters).IsolationLevel);

  Connector.SetProp(prEmbParams, TMyConnectionParameters(ConnectionParameters).EmbParams.Text);
  Connector.OnError := ConnectionParameters.OnError;

  inherited;
end;

{ TMyConnectionPoolManager }

class function TMyConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TMyConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TMyConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TMyLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
