
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASEConnectionUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  CLRClasses,
  Classes, SysUtils, Variants, SyncObjs,
  CRTypes, MemData, CRAccess, CRParser, CRFunctions, CRVio,
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  {$IFDEF TDS}Tds5Consts,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ODBCDataTypeMap, ODBCClasses, ODBCProps,{$ENDIF}
  ASEConsts, ASEProps, ASEDataTypeMap, ASEParser;
{$ELSE}
  {$IFDEF TDS}Tds5ConstsUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ODBCDataTypeMapUni, ODBCClassesUni, ODBCPropsUni,{$ENDIF}
  ASEConstsUni, ASEPropsUni, ASEDataTypeMapUni, ASEParserUni;
{$ENDIF}

type
  TASESelectMethod = (smCursor, smDirect);
  TASEEncryptPassword = (epDisable, epRequire, epPrefer);
  TASEPrepareMethod = (pmNone, pmPartial, pmFull, pmFullatPrepare);

  TASESQLInfo = class({$IFDEF ODBC_PROVIDER}TODBCSQLInfo{$ELSE}TSQLInfo{$ENDIF})
  public
    function LeftQuote: Char; override;
    function RightQuote: Char; override;
    function IsQuoted(const Value: string): boolean; override;
    function IdentCase: TIdentCase; override;
    function ParamQuoteAllowed: boolean; override;

    procedure SplitObjectName(const Name: string; out DataBase: string; out Owner: string; out ObjName: string); reintroduce;

    function NamesFromList(List: TStrings; NormalizedName: boolean = True; Delimiter: string = ';'): string; override;
  end;

  TASEConnection = class({$IFDEF ODBC_PROVIDER}TODBCConnection{$ELSE}TCRConnection{$ENDIF})
  private
    FPort: integer;
    FAnsiNull: Boolean;
    FApplicationName: string;
    FSelectMethod: TASESelectMethod;
    FEncryptPassword: TASEEncryptPassword;
    FPreparedMethod: TASEPrepareMethod;
    FClientHostName: string;
    FCharSet: string;
    FConnector: TCRConnector;
    FDirect: boolean;
    FIPVersion: TIPVersion;
    FQuotedIdentifier: Boolean;
    FMultipleConnections: boolean;
    FTextSize: Integer;
  {$IFNDEF ODBC_PROVIDER}
    FUseUnicode: boolean;
    FConnectionTimeout: integer;
  {$ENDIF}
    FNCharSize: integer;
    FUniCharSize: integer;

    procedure SetAnsiNull(const Value: boolean);
    procedure SetQuotedIdentifier(const Value: boolean);
  protected
  {$IFNDEF ODBC_PROVIDER}
    FCachedCatalog: string;
    FCachedSchema: string;
  {$ENDIF}
    procedure CheckUniAndNCharSize;

    procedure SetDirect(const Value: Boolean);

  {$IFDEF ODBC_PROVIDER}
    function GetConnectionString: string; override;
    function OutParamIsInOut: boolean; override;
  {$ENDIF}
    procedure SetDatabase(const Value: string);
    procedure SetTextSize(const Value: string);
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
    class function GetMapRulesClass: TCRMapRulesClass; override;

    function GetConnector: TCRConnector; override;
    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;
    procedure Ping; override;
    function CheckIsValid: boolean; override;
    procedure SetIsolationLevel(const Value: TCRIsolationLevel); override;

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function CanChangeDatabase: boolean; override;

    function GetCurrentCatalog: string; {$IFDEF ODBC_PROVIDER}override;{$ENDIF}
    function GetCachedCatalog: string; {$IFDEF ODBC_PROVIDER}override;{$ENDIF}
    function GetCurrentSchema: string; {$IFDEF ODBC_PROVIDER}override;{$ENDIF}
    function GetCachedSchema: string; {$IFDEF ODBC_PROVIDER}override;{$ENDIF}

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;

    property EnableBCD;
    property EnableFMTBCD;
    property Direct: boolean read FDirect;
    property IPVersion: TIPVersion read FIPVersion;
    property Server: string read FServer;
    property ClientHostName: string read FClientHostName;
    property Port: integer read FPort write FPort;
    property ApplicationName: string read FApplicationName;
    property EncryptPassword: TASEEncryptPassword read FEncryptPassword;
    property NCharSize: integer read FNCharSize;
    property UniCharSize: integer read FUniCharSize;

  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  ASESQLGenerator,
  {$IFDEF TDS}Tds5Classes,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ASEClasses,{$ENDIF}
{$ELSE}
  ASESQLGeneratorUni,
  {$IFDEF TDS}Tds5ClassesUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ASEClassesUni,{$ENDIF}
{$ENDIF}
  DAConsts, CRProps;

{ TASESQLInfo }

function TASESQLInfo.LeftQuote: Char;
begin
  Result := '[';
end;

function TASESQLInfo.RightQuote: Char;
begin
  Result := ']';
end;

function TASESQLInfo.IsQuoted(const Value: string): boolean;
var
  len: integer;
begin
  len := Length(Value);
  if (len <= 1) then
    Result := False
  else
    Result := ((Value[1] = LeftQuote) and (Value[len] = RightQuote)) or  // [ ]
              ((Value[1] = '"') and (Value[len] = '"')) // " "
end;

function TASESQLInfo.IdentCase: TIdentCase;
begin
  Result := icMixedCaseSensitive;
end;

function TASESQLInfo.ParamQuoteAllowed: boolean;
begin
  Result := False;
end;

procedure TASESQLInfo.SplitObjectName(const Name: string; out DataBase: string; out Owner: string; out ObjName: string);
var
  Info: TSQLObjectInfo;
begin
  inherited SplitObjectName(Name, Info);

  DataBase := Info.Catalog;
  Owner := Info.Schema;
  ObjName := Info.Name;
end;

function TASESQLInfo.NamesFromList(List: TStrings; NormalizedName: boolean = True; Delimiter: string = ';'): string;
begin
  Result := inherited NamesFromList(List, NormalizedName, ',');
end;

{ TASEConnection }

constructor TASEConnection.Create;
begin
  inherited;

  FAnsiNull := True; // Set the AnsiNull option to On by default (old behavior)
  FDirect := False;
  FIPVersion := ivIPv4;
  FPort := DefaultASEPort;
  FDatabase := '';
  FNCharSize := 1;
  FUniCharSize := 2;
  FQuotedIdentifier := DefValQuotedIdentifier;
  FMultipleConnections := DefValMultipleConnections;

end;

destructor TASEConnection.Destroy;
begin
  inherited;

  if FNativeConnection then
    FConnector.Free;
end;

function TASEConnection.GetCommandClass: TCRCommandClass;
begin
  if FDirect then
  {$IFDEF TDS}
    Result := TTDS5Command
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TASECommand;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TASEConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  if FDirect then
  {$IFDEF TDS}
    Result := TTDS5RecordSet
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TASERecordSet;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TASEConnection.GetTransactionClass: TCRTransactionClass;
begin
  if FDirect then
  {$IFDEF TDS}
    Result := TTDS5Transaction
  {$ELSE}
    Result := TASETransaction
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TASETransaction;
  {$ELSE}
    Result := TTDS5Transaction;
  {$ENDIF}
end;

function TASEConnection.GetLoaderClass: TCRLoaderClass;
begin
  if FDirect then
  {$IFDEF TDS}
    Result := TTDS5Loader
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TASELoader;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TASEConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  if FDirect then
  {$IFDEF TDS}
    Result := TTDS5MetaData
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := TASEMetaData;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

class function TASEConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TASEMapRules;
end;

procedure TASEConnection.SetAnsiNull(const Value: boolean);
begin
  if FAnsiNull <> Value then
  begin
    FAnsiNull := Value;
    if FConnected then
      ExecuteSQL(TCustomASESQLGenerator.GenerateAnsiNullSQL(FAnsiNull));
  end;
end;

procedure TASEConnection.SetQuotedIdentifier(const Value: boolean);
begin
  if FQuotedIdentifier <> Value then
  begin
    FQuotedIdentifier := Value;
    if FConnected then
      ExecuteSQL(TCustomASESQLGenerator.GenerateQuotedIdentifierSQL(FQuotedIdentifier));
  end;
end;

procedure TASEConnection.CheckUniAndNCharSize;
var
  ResordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  ResordSet := OpenRecordSet('SELECT @@unicharsize, @@ncharsize');
  try
    ResordSet.AllocRecBuf(RecBuf);
    try
      ResordSet.GetNextRecord(RecBuf);
      if ResordSet.Eof then begin
        FUniCharSize := 2;
        FNCharSize := 1;
      end
      else begin
        ResordSet.GetFieldAsVariant(ResordSet.Fields[0], RecBuf, v);
        FUniCharSize := v;
        ResordSet.GetFieldAsVariant(ResordSet.Fields[1], RecBuf, v);
        FNCharSize := v;
      end;
    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
  finally
    ReleaseRecordSet(ResordSet);
  end;
end;

procedure TASEConnection.SetDirect(const Value: Boolean);
begin
{$IFNDEF TDS}
  if Value then
    raise Exception.Create(SDirectNotSupported);
{$ENDIF}

{$IFNDEF ODBC_PROVIDER}
  if not Value then
    raise Exception.Create(SNonDirectNotSupported);
{$ENDIF}

  if Value <> FDirect then begin
    FDirect := Value;

    if not (FInternalTransaction is GetTransactionClass) then begin
      FInternalTransaction.Free;
      FInternalTransaction := GetTransactionClass.Create;
      FInternalTransaction.AddConnection(Self);
    end;
  end;
end;

{$IFDEF ODBC_PROVIDER}

function TASEConnection.GetConnectionString: string;
var
  Port: integer;
  Server: string;
  Database: string;
  ApplicationName: string;
  SelectMethod: TASESelectMethod;
  EncryptPassword: TASEEncryptPassword;
  PrepareMethod: TASEPrepareMethod;
  ClientHostName: string;
  TextSize: Integer;
  CharSet: String;
begin
  Server := Trim(FServer);
  if Server = '' then
    Server := 'localhost';

  Port := FPort;
  if Port = 0 then
    Port := DefaultASEPort;

  Database := Trim(FDatabase);
  ApplicationName := Trim(FApplicationName);
  SelectMethod := FSelectMethod;
  EncryptPassword := FEncryptPassword;
  PrepareMethod := FPreparedMethod;
  ClientHostName := FClientHostName;
  TextSize := FTextSize;
  CharSet := Trim(FCharSet);

  if IsDriverPresent('Adaptive Server Enterprise') then begin
    Result := Format('DRIVER={Adaptive Server Enterprise};UID=%s;PWD=%s;server=%s;port=%d',
      [FUsername, FPassword, Server, Port]);
    if Database <> '' then
      Result := Result + ';database=' + Database;
    if EncryptPassword = epRequire then
      Result := Result + ';EncryptPassword=1'
    else if EncryptPassword = epPrefer then
      Result := Result + ';EncryptPassword=2'
    else
      Result := Result + ';EncryptPassword=0';
  end
  else begin
    Result := Format('DRIVER={Sybase ASE ODBC Driver};UID=%s;PWD=%s;NA=%s,%d',
      [FUsername, FPassword, Server, Port]);
    if Database <> '' then
      Result := Result + ';DB=' + Database;
    if SelectMethod = smCursor then
      Result := Result + ';SM=1'
    else
      Result := Result + ';SM=0';
    if EncryptPassword in [epRequire, epPrefer] then
      Result := Result + ';PasswordEncryption=1'
    else
      Result := Result + ';PasswordEncryption=0';
    Result := Result + ';OptimizePrepare=';
    case PrepareMethod of
      pmPartial: Result := Result + '1';
      pmFull: Result := Result + '2';
      pmFullatPrepare: Result := Result + '3';
      else
        Result := Result + '0';
    end;
  end;

  if ApplicationName <> '' then
    Result:= Result + ';App=' + ApplicationName;

  if ClientHostName <> '' then
    Result:= Result + ';WKID=' + ClientHostName;

  if CharSet <> '' then
    Result := Result + ';CharSet=' + CharSet;

  if TextSize > 0 then
    Result := Result + ';TextSize=' + IntToStr(TextSize);

  Result := Result + ';EnableDescribeParam=1';
end;

function TASEConnection.OutParamIsInOut: boolean;
begin
  Result := True;
end;

{$ENDIF}

procedure TASEConnection.SetDatabase(const Value: string);
const
  SWrongDatabaseName = 'Changing database name to default value is not allowed';
begin
  FDatabase := Value;

  if FDirect and FConnected then
    FConnector.SetDatabase(Value);
end;

procedure TASEConnection.SetTextSize(const Value: string);
begin
  ExecuteSQL(TCustomASESQLGenerator.GenerateTextSizeSQL(Value));
end;

procedure TASEConnection.CreateConnector;
begin
  Assert(FDirect);
{$IFDEF TDS}
  FConnector := TTDS5Connector.Create(Self);
{$ENDIF}
end;

function TASEConnection.CheckCommand(Command: TCRCommand): boolean;
begin
  Result := IsClass(Command, GetCommandClass);
  if Result then
    Command.SetCursorState(csInactive); // To prevent blocking execute after previous exec
end;

function TASEConnection.CheckRecordSet(RecordSet: TCRRecordSet): boolean;
begin
  Result := IsClass(RecordSet, GetRecordSetClass);
end;

function TASEConnection.GetConnector: TCRConnector;
begin
  Result := FConnector;
end;

procedure TASEConnection.Connect(const ConnectString: string);
var
  ClassOld: TClass;
  Params: string;
type
  PClass = ^TClass;
begin
  if FConnected then
    Exit;

  Params := '';
  if not FDirect then begin
    inherited;

    Params := Params + TCustomASESQLGenerator.GenerateAnsiNullSQL(FAnsiNull) + DALineSeparator;
    Params := Params + TCustomASESQLGenerator.GenerateQuotedIdentifierSQL(FQuotedIdentifier) + DALineSeparator;
    ExecuteSQL(Params);
  end
  else
    try
      if FNativeConnection then begin
        FreeAndNil(FConnector);
        CreateConnector;
      end;

      FConnector.Connect;

      // instead of inherited parent of parent here
      ClassOld := PClass(Self)^;
      PClass(Self)^ := TCRConnection;
      (Self as TCRConnection).Connect(ConnectString);
      PClass(Self)^ := ClassOld;

      FConnected := True;

      if FDatabase <> '' then
        Params := Params + TCustomASESQLGenerator.GenerateDatabaseSQL(FDatabase) + DALineSeparator;
      Params := Params + TCustomASESQLGenerator.GenerateTextSizeSQL('2147483647') + DALineSeparator;
      Params := Params + TCustomASESQLGenerator.GenerateQuotedIdentifierSQL(FQuotedIdentifier) + DALineSeparator;

      if FIsolationLevel <> ilReadCommitted then
        Params := Params + TCustomASESQLGenerator.GenerateIsolationLevelSQL(FIsolationLevel) + DALineSeparator;

      ExecuteSQL(Params);
      CheckUniAndNCharSize;
    except
      on EFailOver do;
      else begin
        FConnected := False;
        raise;
      end;
    end;
end;

procedure TASEConnection.Disconnect;
begin
  if not FConnected then
    Exit;

  try
    if FNativeConnection then
      if FDirect then
        FConnector.Disconnect
      else
        inherited;
  finally
    FConnected := False;
  end;
end;

procedure TASEConnection.Ping;
begin
  if FDirect then
    ExecuteSQL('SELECT 1')
  else
    inherited;
end;

function TASEConnection.CheckIsValid: boolean;
const
  SCheckConnection = '/* Check connection */';
begin
  if FDirect then begin
  {$IFDEF TDS}
    FIsValid := FConnected;
    if FIsValid then
      try
        ExecuteSQL(SCheckConnection);
      except
        FIsValid := False;
      end;
    Result := FIsValid;
  {$ELSE}
    raise Exception.Create(SDirectNotSupported);
  {$ENDIF}
  end
  else
  {$IFDEF ODBC_PROVIDER}
    Result := inherited CheckIsValid;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

procedure TASEConnection.SetIsolationLevel(const Value: TCRIsolationLevel);
begin
  if FConnected then
    ExecuteSQL(TCustomASESQLGenerator.GenerateIsolationLevelSQL(Value));

  FIsolationLevel := Value;
end;

function TASEConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prPort:
      Value := FPort;
    prAnsiNull:
      Value := FAnsiNull;
    prMaxStringSize:
      Value := 1960;
    prApplicationName:
      Value := FApplicationName;
    prSelectMethod:
      Value := Variant(FSelectMethod);
    prEncryptPassword:
      Value := Variant(FEncryptPassword);
    prPrepareMethod:
      Value := Variant(FPreparedMethod);
    prClientHostName:
      Value := FClientHostName;
    prCharSet:
      Value := FCharSet;
    prDirect:
      Value := FDirect;
    prIPVersion:
      Value := FIPVersion;
    prQuotedIdentifier:
      Value := FQuotedIdentifier;
    prMultipleConnections:
      Value := FMultipleConnections;
    prTextSize:
      Value := FTextSize;
  {$IFNDEF ODBC_PROVIDER}
    prUseUnicode:
      Value := FUseUnicode;
    prConnectionTimeout:
      Value := FConnectionTimeout;
  {$ENDIF}
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TASEConnection.SetProp(Prop: Integer; const Value: variant): Boolean;
begin
  Result := True;
  case Prop of
    prDatabase: begin
      if Value <> FDatabase then begin
        if GetConnected then
          SetDatabase(Value);
        FDatabase := Value;
      end;
    end;
    prPort:
      FPort := Value;
    prAnsiNull:
      SetAnsiNull(Value);
    prApplicationName:
      FApplicationName := Value;
    prSelectMethod:
      FSelectMethod := TASESelectMethod(Value);
    prEncryptPassword:
      FEncryptPassword := TASEEncryptPassword(Value);
    prPrepareMethod:
      FPreparedMethod := TASEPrepareMethod(Value);
    prClientHostName:
      FClientHostName := Value;
    prCharSet:
      FCharSet := Value;
    prDirect:
      SetDirect(Value);
    prIPVersion:
      FIPVersion := Value;
    prQuotedIdentifier:
      SetQuotedIdentifier(Value);
    prMultipleConnections:
      FMultipleConnections := Value;
    prTextSize:
      FTextSize := Value;
  {$IFNDEF ODBC_PROVIDER}
    prUseUnicode:
      FUseUnicode := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
  {$ENDIF}
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TASEConnection.Assign(Source: TCRConnection);
var
  Src: TASEConnection;
begin
  inherited;

  Src := TASEConnection(Source);
  FDatabase := Src.FDatabase;
  FPort := Src.FPort;
  FAnsiNull := Src.FAnsiNull;
  FApplicationName := Src.FApplicationName;
  FSelectMethod := Src.FSelectMethod;
  FEncryptPassword := Src.FEncryptPassword;
  FPreparedMethod := Src.FPreparedMethod;
  FClientHostName := Src.FClientHostName;
  FCharSet := Src.FCharSet;
  FQuotedIdentifier := Src.FQuotedIdentifier;

  FDatabase := Src.FDatabase;
  FPort := Src.FPort;
  SetDirect(Src.FDirect);
end;

procedure TASEConnection.AssignConnect(Source: TCRConnection);
begin
  {$IFDEF ODBC_PROVIDER}
  inherited;
  {$ENDIF}

  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Assign(Source);
      FInternalTransaction.AssignConnect(TASEConnection(Source).FInternalTransaction);
      FConnector := TASEConnection(Source).FConnector;
      FConnected := TASEConnection(Source).FConnected;
      FNativeConnection := False;
    end;
  end;
end;

function TASEConnection.CanChangeDatabase: boolean;
begin
  Result := True;
end;

function TASEConnection.GetCurrentCatalog: string;
var
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  RecordSet := OpenRecordSet('SELECT db_name()');
  try
    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then begin
        Result := '';
        exit;
      end;
      RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
      Result := Trim(VarToStr(v));
    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
  finally
    ReleaseRecordSet(RecordSet);
  end;
end;

function TASEConnection.GetCachedCatalog: string;
begin
  if FCachedCatalog = '' then
    if FDatabase <> '' then
      FCachedCatalog := SQLInfo.NormalizeName(FDatabase, False, True)
    else
      FCachedCatalog := GetCurrentCatalog;

  Result := FCachedCatalog;
end;

function TASEConnection.GetCachedSchema: string;
begin
  if FCachedSchema = '' then
    FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;

function TASEConnection.GetCurrentSchema: string;
var
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  RecordSet := OpenRecordSet('SELECT USER');
  try
    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then begin
        Result := '';
        Exit;
      end;
      RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
      Result := Trim(VarToStr(v));
    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
  finally
    ReleaseRecordSet(RecordSet);
  end;
end;

function TASEConnection.GetServerVersion: string;
begin
  if FDirect then
  {$IFDEF TDS}
    Result := FConnector.ServerVersion
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := inherited GetServerVersion;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TASEConnection.GetServerVersionFull: string;
begin
  if FDirect then
  {$IFDEF TDS}
    Result := 'Adaptive Server Enterprise ' + FConnector.ServerVersion
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := inherited GetServerVersionFull;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

function TASEConnection.GetClientVersion: string;
begin
  if FDirect then
  {$IFDEF TDS}
    Result := ''
  {$ELSE}
    raise Exception.Create(SDirectNotSupported)
  {$ENDIF}
  else
  {$IFDEF ODBC_PROVIDER}
    Result := inherited GetClientVersion;
  {$ELSE}
    raise Exception.Create(SNonDirectNotSupported);
  {$ENDIF}
end;

end.
