
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFConnectionUni;

interface

uses
  SysUtils,
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  {$IFDEF ODBC_PROVIDER}ODBCClasses,{$ENDIF}
  DBFConsts, DBFProps, DBFParser,
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCClassesUni,{$ENDIF}
  DBFConstsUni, DBFPropsUni, DBFParserUni,
{$ENDIF}
  CRAccess;

type
  TDBFConnection = class;

  TDBFSQLInfo = class({$IFDEF ODBC_PROVIDER}TODBCSQLInfo{$ELSE}TSQLInfo{$ENDIF})
  public
    function IdentCase: TIdentCase; override;
    function QuotesNeeded(const Value: string): boolean; override;
  end;

  TCustomDBFDirectConnector = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TDBFConnection;
    FInternalConnection: TCRConnection;
    FUseUnicode: boolean;
  public
    constructor Create(Owner: TDBFConnection; const UseUnicode: boolean); virtual;
    destructor Destroy; override;

    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual; abstract;

    function GetInternalConnection: TCRConnection;
  end;

  TDBFConnection = class({$IFDEF ODBC_PROVIDER}TODBCConnection{$ELSE}TCRConnection{$ENDIF})
  private
    FConnector: TCustomDBFDirectConnector;
    FDatabase: string;
    FCollatingSequence: string;
    FDirect: boolean;
    FDBFFormat: TDBFFormat;
    FCodePage: TDBFCodePage;
    FConnectMode: TDBFConnectMode;
    FIndexOnReading: TDBFIndexKind;
    FIgnoreDataErrors,
    FIgnoreMetadataErrors,
    FIgnoreBrokenTables: boolean;
    FIdentifierCase: TDBFIdentifierCase;
  {$IFNDEF ODBC_PROVIDER}
    FUseUnicode: boolean;
  {$ENDIF}
    FAllFieldsAsNullable: boolean;
  protected
  {$IFDEF ODBC_PROVIDER}
    function GetConnectionString: string; override;
  {$ENDIF}
    procedure SetDirect(const Value: Boolean);
    procedure SetFormat(const Value: TDBFFormat);
    procedure SetCodePage(const Value: TDBFCodePage);
    procedure SetConnectMode(const Value: TDBFConnectMode);
    procedure SetIndexOnReading(const Value: TDBFIndexKind);
    procedure SetIgnoreDataErrors(const Value: boolean);
    procedure SetIgnoreMetadataErrors(const Value: boolean);
    procedure SetIgnoreBrokenTables(const Value: boolean);
    procedure SetIdentifierCase(const Value: TDBFIdentifierCase);

    procedure CreateConnector;

    function CheckCommand(Command: TCRCommand): boolean; override;
    function CheckRecordSet(RecordSet: TCRRecordSet): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
    function GetLoaderClass: TCRLoaderClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
    function GetServerVersionFull: string; override;
    function GetServerVersion: string; override;
    function GetClientVersion: string; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;

    function GetInternalTransaction: TCRTransaction; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;

    property Connector: TCustomDBFDirectConnector read FConnector;
    property Direct: boolean read FDirect;
    property DBFFormat: TDBFFormat read FDBFFormat;
    property CodePage: TDBFCodePage read FCodePage;
    property ConnectMode: TDBFConnectMode read FConnectMode;
    property IndexOnReading: TDBFIndexKind read FIndexOnReading;
    property IgnoreDataErrors: boolean read FIgnoreDataErrors;
    property IgnoreMetadataErrors: boolean read FIgnoreMetadataErrors;
    property IgnoreBrokenTables: boolean read FIgnoreBrokenTables;
    property IdentifierCase: TDBFIdentifierCase read FIdentifierCase;
    property AllFieldsAsNullable: boolean read FAllFieldsAsNullable;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  {$IFDEF DBFENGINE}DBFEngine,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}DBFClasses,{$ENDIF}
{$ELSE}
  {$IFDEF DBFENGINE}DBFEngineUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}DBFClassesUni,{$ENDIF}
{$ENDIF}
  CRFunctions, CRProps;

{ TDBFSQLInfo }

function TDBFSQLInfo.IdentCase: TIdentCase;
begin
  Result := icMixed;
end;

function TDBFSQLInfo.QuotesNeeded(const Value: string): boolean;
begin
  Result := inherited QuotesNeeded(Value);
  if not Result and ((DBFKeywordLexems.IndexOf(AnsiUpperCase(Value)) >= 0) or (Pos('.', Value) > 0)) then
    Result := True;
end;

{ TCustomDBFDirectConnector }

constructor TCustomDBFDirectConnector.Create(Owner: TDBFConnection; const UseUnicode: boolean);
begin
  inherited Create;

  FOwner := Owner;
  FInternalConnection := nil;
  FUseUnicode := UseUnicode;
end;

destructor TCustomDBFDirectConnector.Destroy;
begin
  if Assigned(FInternalConnection) then begin
    FInternalConnection.Disconnect;
    FInternalConnection.Free;
  end;

  inherited;
end;

function TCustomDBFDirectConnector.GetInternalConnection: TCRConnection;
begin
  Result := FInternalConnection;
end;

{ TDBFConnection }

constructor TDBFConnection.Create;
begin
  inherited;

  FConnector := nil;
  FDirect := False;
  FDBFFormat := dfdBaseVII;
  FCodePage := dpDefault;
  FConnectMode := cmShared;
  FIndexOnReading := ikNative;
  FIgnoreDataErrors := False;
  FIgnoreMetadataErrors := False;
  FIgnoreBrokenTables := False;
  FIdentifierCase := icOriginal;
  FAllFieldsAsNullable := False;
end;

destructor TDBFConnection.Destroy;
begin
  FConnector.Free;

  inherited;
end;

{$IFDEF ODBC_PROVIDER}
function TDBFConnection.GetConnectionString: string;
var
  DefaultDir: string;
begin
  DefaultDir := ExtractFilePath(FDatabase);
  Result := Format('DRIVER={Microsoft dBase Driver (*.dbf)};DefaultDir=%s;DBQ=%s;UID=%s;PWD=%s',
    [DefaultDir, FDatabase, FUsername, FPassword]);

  if FCollatingSequence <> '' then
    Result := Result + ';CollatingSequence=' + FCollatingSequence;
end;
{$ENDIF}

procedure TDBFConnection.SetDirect(const Value: Boolean);
begin
{$IFNDEF ODBC_PROVIDER}
  if not Value then
    raise Exception.Create(SNonDirectNotSupported);
{$ENDIF}

  if Value <> FDirect then
    FDirect := Value;
end;

procedure TDBFConnection.SetFormat(const Value: TDBFFormat);
begin
  if Value <> FDBFFormat then begin
    Disconnect;
    FDBFFormat := Value;
  end;
end;

procedure TDBFConnection.SetCodePage(const Value: TDBFCodePage);
begin
  FCodePage := Value;
end;

procedure TDBFConnection.SetConnectMode(const Value: TDBFConnectMode);
begin
  if Value <> FConnectMode then begin
    Disconnect;
    FConnectMode := Value;
  end;
end;

procedure TDBFConnection.SetIndexOnReading(const Value: TDBFIndexKind);
begin
  if Value <> FIndexOnReading then begin
    Disconnect;
    FIndexOnReading := Value;
  end;
end;

procedure TDBFConnection.SetIgnoreDataErrors(const Value: boolean);
begin
  if Value <> FIgnoreDataErrors then begin
    Disconnect;
    FIgnoreDataErrors := Value;
  end;
end;

procedure TDBFConnection.SetIgnoreMetadataErrors(const Value: boolean);
begin
  if Value <> FIgnoreMetadataErrors then begin
    Disconnect;
    FIgnoreMetadataErrors := Value;
  end;
end;

procedure TDBFConnection.SetIgnoreBrokenTables(const Value: boolean);
begin
  if Value <> FIgnoreBrokenTables then begin
    Disconnect;
    FIgnoreBrokenTables := Value;
  end;
end;

procedure TDBFConnection.SetIdentifierCase(const Value: TDBFIdentifierCase);
begin
  if Value <> FIdentifierCase then begin
    Disconnect;
    FIdentifierCase := Value;
  end;
end;

procedure TDBFConnection.CreateConnector;
{$IFDEF DBFENGINE}
var
  v: Variant;
  UseUnicode: boolean;
{$ENDIF}
begin
  Assert(FDirect);
{$IFDEF DBFENGINE}
  if GetProp(prUseUnicode, v) then
    UseUnicode := v
  else
    UseUnicode := False;
  FConnector := TDBFDirectConnector.Create(Self, UseUnicode);
{$ELSE}
  raise Exception.Create(SDirectNotSupported)
{$ENDIF}
end;

function TDBFConnection.CheckCommand(Command: TCRCommand): boolean;
begin
  Result := IsClass(Command, GetCommandClass);
end;

function TDBFConnection.CheckRecordSet(RecordSet: TCRRecordSet): boolean;
begin
  Result := IsClass(RecordSet, GetRecordSetClass);
end;

function TDBFConnection.GetCommandClass: TCRCommandClass;
begin
  if FDirect then
  {$IFDEF DBFENGINE}
    Result := TDBFDirectCommand
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TDBFCommand;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TDBFConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  if FDirect then
  {$IFDEF DBFENGINE}
    Result := TDBFDirectRecordSet
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TDBFRecordSet;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TDBFConnection.GetTransactionClass: TCRTransactionClass;
begin
  if FDirect then
  {$IFDEF DBFENGINE}
    Result := TDBFDirectTransaction
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TDBFTransaction;
  {$ELSE}
    Result := TCRTransaction;
  {$ENDIF}
end;

function TDBFConnection.GetLoaderClass: TCRLoaderClass;
begin
  if FDirect then
  {$IFDEF DBFENGINE}
    Result := TDBFDirectLoader
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TDBFLoader;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TDBFConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  if FDirect then
  {$IFDEF DBFENGINE}
    Result := TDBFMetaData
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TODBCMetaData;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TDBFConnection.GetServerVersionFull: string;
begin
  if FDirect then
    Result := 'Direct access'
  else
  {$IFDEF ODBC_PROVIDER}
    Result := inherited GetServerVersionFull;
  {$ELSE}
    Result := '';
  {$ENDIF}
end;

function TDBFConnection.GetServerVersion: string;
begin
  if FDirect then
    Result := 'Direct access'
  else
  {$IFDEF ODBC_PROVIDER}
    Result := inherited GetServerVersion;
  {$ELSE}
    Result := '';
  {$ENDIF}
end;

function TDBFConnection.GetClientVersion: string;
begin
  if FDirect then
    Result := 'Direct access'
  else
  {$IFDEF ODBC_PROVIDER}
    Result := inherited GetClientVersion;
  {$ELSE}
    Result := '';
  {$ENDIF}
end;

class function TDBFConnection.GetMapRulesClass: TCRMapRulesClass;
begin
{$IFDEF ODBC_PROVIDER}
  Result := TDBFCommand.GetMapRulesClass;
{$ELSE}
{$IFDEF DBFENGINE}
  Result := TDBFDirectCommand.GetMapRulesClass
{$ELSE}
  Result := TCRCommand.GetMapRulesClass;
{$ENDIF}
{$ENDIF}
end;

procedure TDBFConnection.Connect(const ConnectString: string);
begin
  if not FDirect then
    inherited
  else if not FConnected then begin
    try
      if FConnector <> nil then
        FConnector.Disconnect
      else
        CreateConnector;
      FConnector.SetProp(prDatabase, FDatabase);

      FConnector.Connect;

      FConnected := True;
      FNativeConnection := True;
    except
      on EFailOver do;
      else begin
        FConnected := False;
        raise;
      end;
    end;
  end;
end;

procedure TDBFConnection.Disconnect;
begin
  if FDirect and FConnected then begin
    FConnector.Disconnect;
    FConnected := False;
  end
  else
    inherited;
end;

function TDBFConnection.GetInternalTransaction: TCRTransaction;
var
  ClassType: TCRTransactionClass;
begin
  ClassType := GetTransactionClass;

  if not (FInternalTransaction is ClassType) then begin
    FInternalTransaction.CheckInactive;
    FInternalTransaction.Free;
    FInternalTransaction := ClassType.Create;
    FInternalTransaction.AddConnection(Self);
  end;

  Result := inherited GetInternalTransaction;
end;

function TDBFConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prCollatingSequence:
      FCollatingSequence := Value;
    prDirect:
      SetDirect(Value);
    prDBFFormat:
      SetFormat(Value);
    prCodePage:
      SetCodePage(TDBFCodePage(Value));
    prConnectMode:
      SetConnectMode(TDBFConnectMode(Value));
    prIndexOnReading:
      SetIndexOnReading(TDBFIndexKind(Value));
    prIgnoreDataErrors:
      SetIgnoreDataErrors(Value);
    prIgnoreMetadataErrors:
      SetIgnoreMetadataErrors(Value);
    prIgnoreBrokenTables:
      SetIgnoreBrokenTables(Value);
    prIgnoreIndexErrors:
      SuppressIndexOpenErrors := Value;
    prIdentifierCase:
      SetIdentifierCase(Value);
  {$IFNDEF ODBC_PROVIDER}
    prUseUnicode:
      FUseUnicode := Value;
  {$ENDIF}
    prAllFieldsAsNullable:
      FAllFieldsAsNullable := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TDBFConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    // max string parameter length
    prMaxStringSize:
      Value := 254;
    prDatabase:
      Value := ExcludeTrailingPathDelimiter(FDatabase);
    prCollatingSequence:
      Value := FCollatingSequence;
    prDirect:
      Value := FDirect;
    prDBFFormat:
      Value := FDBFFormat;
    prCodePage:
      Value := Cardinal(FCodePage);
    prConnectMode:
      Value := Cardinal(FConnectMode);
    prIndexOnReading:
      Value := Cardinal(FIndexOnReading);
    prIgnoreDataErrors:
      Value := FIgnoreDataErrors;
    prIgnoreMetadataErrors:
      Value := FIgnoreMetadataErrors;
    prIgnoreBrokenTables:
      Value := FIgnoreBrokenTables;
    prIgnoreIndexErrors:
      Value := SuppressIndexOpenErrors;
    prIdentifierCase:
      Value := FIdentifierCase;
  {$IFNDEF ODBC_PROVIDER}
    prUseUnicode:
      Value := FUseUnicode;
  {$ENDIF}
    prAllFieldsAsNullable:
      Value := FAllFieldsAsNullable;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TDBFConnection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TDBFConnection(Source).FDatabase;
end;

end.
