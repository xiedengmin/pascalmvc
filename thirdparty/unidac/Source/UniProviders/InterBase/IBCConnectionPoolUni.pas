
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright (c) 2006-2021 Devart. All right reserved.
//  Connection Pool
//////////////////////////////////////////////////

{$I IbDac.inc}
unit IBCConnectionPoolUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, TypInfo, SyncObjs,
  CRTypes, CRConnectionPool, CRAccess, CRVio,
  {$IFNDEF UNIDACPRO}IBCClasses{$ELSE}IBCClassesUni{$ENDIF};

type
  TIBCConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;
  public
    Database: string;
    Port: string;
    Protocol: _TIBCProtocol;
    SSLOptions: TGDSSSLOptions;
    IPVersion: TIPVersion;
    DBParams: TStringList;
    Charset: string;
    SQLDialect: integer;
    UseUnicode: boolean;
    Role: string;
    TrustedAuthentication: boolean;
    NoDBTriggers: boolean;
    ClientLibrary: string;
    SimpleNumericMap: Boolean;

    constructor Create; override;
    destructor Destroy; override;
    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TIBCLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TIBCConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;

    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
  CRProps, CRFunctions, MemData,
{$IFNDEF UNIDACPRO}
  IBCProps, IBCCall;
{$ELSE}
  IBCPropsUni, IBCCallUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TIBCConnectionParameters}

constructor TIBCConnectionParameters.Create;
begin
  inherited;
  DBParams := TStringList.Create;
  SSLOptions := TGDSSSLOptions.Create;
end;

destructor TIBCConnectionParameters.Destroy;
begin
  SSLOptions.Free;
  DBParams.Free;

  inherited;
end;

procedure TIBCConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TIBCConnectionParameters then begin
    TIBCConnectionParameters(Dest).Database := Database;
    TIBCConnectionParameters(Dest).Protocol := Protocol;
    TIBCConnectionParameters(Dest).IPVersion := IPVersion;
    TIBCConnectionParameters(Dest).Port := Port;
    TIBCConnectionParameters(Dest).DBParams.Assign(DBParams);
    TIBCConnectionParameters(Dest).Charset := Charset;
    TIBCConnectionParameters(Dest).SQLDialect := SQLDialect;
    TIBCConnectionParameters(Dest).UseUnicode := UseUnicode;
    TIBCConnectionParameters(Dest).Role := Role;
    TIBCConnectionParameters(Dest).TrustedAuthentication := TrustedAuthentication;
    TIBCConnectionParameters(Dest).NoDBTriggers := NoDBTriggers;
    TIBCConnectionParameters(Dest).ClientLibrary := ClientLibrary;
    TIBCConnectionParameters(Dest).SimpleNumericMap := SimpleNumericMap;

    if TIBCConnectionParameters(Dest).SSLOptions <> nil then
      TIBCConnectionParameters(Dest).SSLOptions.Assign(SSLOptions);
  end;
  inherited;
end;

function ListCompareFunction(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List[Index1], List[Index2]);
end;

function CompareParamLists(List1, List2: TStringList): boolean;
var
  i: integer;

  function GetName(Str: string; Index: integer): string;
  begin
    Result := Str;
    if Index > 0 then
      Result := Trim(Copy(Str, 1, Index - 1));
  end;

  function GetValue(Str: string; Index: integer): string;
  begin
    Result := Str;
    if Index > 0 then
      Result := Trim(Copy(Str, Index + 1, Length(str) - Index));
  end;

  function CompareItems(Item1, Item2: string): boolean;
  var
    i1, i2: integer;
  begin
    i1 := pos('=', Item1);
    i2 := pos('=', Item2);
    Result := (AnsiCompareText(GetName(Item1, i1), GetName(Item2, i2)) = 0)
      and (AnsiCompareText(GetValue(Item1, i1), GetValue(Item2, i2)) = 0);
  end;
begin
  Result := True;
  if List1.Count <> List2.Count then
    Result := False
  else begin
    List1.CustomSort(ListCompareFunction);
    List2.CustomSort(ListCompareFunction);

    for i := 0 to List1.Count - 1 do
      if not CompareItems(List1[i], List2[i]) then begin
        Result := False;
        break;
      end;
  end;
end;

function TIBCConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  O: TIBCConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TIBCConnectionParameters) then begin
    O := TIBCConnectionParameters(obj);
    Result :=
      (AnsiCompareText(Database, O.Database) = 0) and
      (Protocol = O.Protocol) and
      (IPVersion = O.IPVersion) and
      (AnsiCompareText(Port, O.Port) = 0) and
      (AnsiCompareText(Charset, O.Charset) = 0) and
      (SQLDialect = O.SQLDialect) and
      (UseUnicode = O.UseUnicode) and
      (Role = O.Role) and
      (TrustedAuthentication = O.TrustedAuthentication) and
      (NoDBTriggers = O.NoDBTriggers) and
      (ClientLibrary = O.ClientLibrary) and
      (SimpleNumericMap = O.SimpleNumericMap) and
      (CompareParamLists(DBParams, O.DBParams));

      if (SSLOptions <> nil) and (O.SSLOptions <> nil) then
        Result := Result and SSLOptions.Equals(O.SSLOptions)
      else if (SSLOptions <> nil) or (O.SSLOptions <> nil) then
        Result := False;
  end;
end;

function TIBCConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prProtocol:
      Protocol := _TIBCProtocol(Value);
    prIPVersion:
      IPVersion := Value;
    prCharset:
      Charset := Value;
    prSQLDialect:
      SQLDialect := Value;
    prUseUnicode:
      UseUnicode := Value;
    prRole:
      Role := Value;
    prPort:
      Port := Value;
    prTrustedAuthentication:
      TrustedAuthentication := Value;
    prNoDBTriggers:
      NoDBTriggers := Value;
    prClientLibrary:
      ClientLibrary := Value;
    prSimpleNumericMap:
      SimpleNumericMap := Value;
    //SSL
    prUseSSL:
      SSLOptions.Enabled := Value;
    prServerPublicFile:
      SSLOptions.ServerPublicFile := Value;
    prServerPublicPath:
      SSLOptions.ServerPublicPath := Value;
    prClientCertFile:
      SSLOptions.ClientCertFile := Value;
    prClientPassPhraseFile:
      SSLOptions.ClientPassPhraseFile := Value;
    prClientPassPhrase:
      SSLOptions.ClientPassPhrase := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TIBCConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString + Format(
    'Port=%s'#13'Protocol=' + GetEnumName(TypeInfo(_TIBCProtocol), Integer(Protocol)) + #13 +
    'Database=%s'#13'SQLDialect=%d'#13,
    [Port, Database, SQLDialect]);
end;

{ TIBCLocalConnectionPool }

class function TIBCLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TGDSConnection;
end;

procedure TIBCLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prPort, TIBCConnectionParameters(ConnectionParameters).Port);
  Connector.SetProp(prDatabase, TIBCConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prProtocol, Variant(TIBCConnectionParameters(ConnectionParameters).Protocol));
  Connector.SetProp(prIPVersion, Variant(TIBCConnectionParameters(ConnectionParameters).IPVersion));
  Connector.SetProp(prCharset, TIBCConnectionParameters(ConnectionParameters).Charset);
  Connector.SetProp(prSQLDialect, TIBCConnectionParameters(ConnectionParameters).SQLDialect);
  Connector.SetProp(prUseUnicode, TIBCConnectionParameters(ConnectionParameters).UseUnicode);
  Connector.SetProp(prRole, TIBCConnectionParameters(ConnectionParameters).Role);
  Connector.SetProp(prTrustedAuthentication, TIBCConnectionParameters(ConnectionParameters).TrustedAuthentication);
  Connector.SetProp(prNoDBTriggers, TIBCConnectionParameters(ConnectionParameters).NoDBTriggers);
  Connector.SetProp(prClientLibrary, TIBCConnectionParameters(ConnectionParameters).ClientLibrary);
  Connector.SetProp(prSimpleNumericMap, TIBCConnectionParameters(ConnectionParameters).SimpleNumericMap);
  TGDSConnection(Connector).SetParams(TIBCConnectionParameters(ConnectionParameters).DBParams);
  Connector.OnError := ConnectionParameters.OnError;

  //SSL
  if TIBCConnectionParameters(ConnectionParameters).SSLOptions <> nil then begin
    Connector.SetProp(prUseSSL, TIBCConnectionParameters(ConnectionParameters).SSLOptions.Enabled);
    Connector.SetProp(prServerPublicFile, TIBCConnectionParameters(ConnectionParameters).SSLOptions.ServerPublicFile);
    Connector.SetProp(prServerPublicPath, TIBCConnectionParameters(ConnectionParameters).SSLOptions.ServerPublicPath);
    Connector.SetProp(prClientCertFile, TIBCConnectionParameters(ConnectionParameters).SSLOptions.ClientCertFile);
    Connector.SetProp(prClientPassPhraseFile, TIBCConnectionParameters(ConnectionParameters).SSLOptions.ClientPassPhraseFile);
    Connector.SetProp(prClientPassPhrase, TIBCConnectionParameters(ConnectionParameters).SSLOptions.ClientPassPhrase);
  end;

  inherited;
end;

{TIBCConnectionPoolManager}

class function TIBCConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TIBCConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TIBCConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TIBCLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
