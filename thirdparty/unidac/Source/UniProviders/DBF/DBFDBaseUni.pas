
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFDBaseUni;

interface

{$IFDEF DBFENGINE}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, Variants, DateUtils, StrUtils, Types, TypInfo,
{$IFDEF LOG_PACKETS}
  LogHandler,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Unistd, Posix.Stdio,
{$ENDIF}
  FMTBcd,
  CRTypes, CRFunctions, CLRClasses, CRAccess, CRDataTypeMap, MemData,
  CRVirtualData,
  LiteClassesVirtual,
{$IFNDEF UNIDACPRO}
  DBFConsts, DBFStructs, DBFUtils, DBFIndexes, DBFMemos, DBFParser,
  {$IFDEF ODBC_PROVIDER}ODBCDataTypeMap,{$ENDIF}
  DBFDataTypeMap;
{$ELSE}
  DBFConstsUni, DBFStructsUni, DBFUtilsUni, DBFIndexesUni, DBFMemosUni, DBFParserUni,
  {$IFDEF ODBC_PROVIDER}ODBCDataTypeMapUni,{$ENDIF}
  DBFDataTypeMapUni;
{$ENDIF}

type
  TDBFTransactionAction = (trActionInsert, trActionEdit, trActionDelete);

  TDBFTransactionBuffer = record
    RecNo: integer;
    Action: TDBFTransactionAction;
    Buffer: TBytes;
  end;

  TTDBFTransactionData = array of TDBFTransactionBuffer;

  PColumnInfo = ^TColumnInfo;
  PConstraintInfo = ^TConstraintInfo;

  TAlterOperation = (aoNone, aoAddField, aoDropField, aoAlterField);

  TDBFDataConverter = class(TDataConverters)
  end;

  TNumericAlignment = (naRight, naLeft);

  TDBFFileClass = class of TDBFDBase;

  TDBFDBase = class
  protected
    FDBFFormat: TDBFFormat;
    FFileName: string;
    FStream: TStream;
    FMemo: TDBFMemoFile;
    FCalculator: TDBFExpressionCalculator;
    FFields: TBytes;
    FFieldCount: integer;
    FFieldSize: integer;
    FOffsets: array of integer;
    FLengths: array of integer;
    FAlignments: array of TNumericAlignment;
    FKeyFields: array of boolean;
    FHeaderFormat: TDBFFormat;
    FHeader: TDBFDBase7Header;
    FHeaderSize: integer;
    FCurrentRecNo: integer;
    FCurrentRecordBuffer: TBytes;
    FHasSqlTableFiles, FHasDbtMemoFiles: boolean;
  {$IFDEF USE_TFORMATSETTINGS}
    FDateTimeFormatSettings: TFormatSettings;
  {$ENDIF}
    FIndexFileType: TIndexFileType;
    FIndexFile: TDBFIndexFile;
    { dBaseVII }
    FConstraints: array of TDBFDBase7StandardDescriptor;
    FPropDataOffs: Int64;
    { VisualFoxPro }
    FFieldNoToFieldMaskIndex, FFieldNoToFieldHasLenIndex: array of Byte;
    FNullMaskFieldIndex: integer;
    { Transaction }
    FInTransaction: boolean;
    FDBFTransactionData: TTDBFTransactionData;
    FCodePage: TDBFCodePage;
    FEncoding: {$IFNDEF NEXTGEN}Encoding{$ELSE}TEncoding{$ENDIF};
    FConnectMode: TDBFConnectMode;
    FForceReadOnly,
    FForceExclusive: boolean;
    FIndexOnReading: TDBFIndexKind;
    FIgnoreDataErrors,
    FIgnoreMetadataErrors,
    FIgnoreBrokenTables,
    FHasDeletedFlag: boolean;
    FPrepared: boolean;
    FSavedPosition: Int64;
    FIdentifierCase: TDBFIdentifierCase;
    FAllFieldsAsNullable: boolean;

    procedure AddFieldToIndexFields(var IndexFields: TIndexFieldArray; FieldNo: integer; const IndexName: string);

    function InternalStrToDateTime(const S: string): TDateTime;
    function GetVirtualValueType(const V: variant): TVirtualType;
    function GetIndexFieldValue(const Value: TVirtualValue; const FieldNo: integer; const FieldName: string): variant;

    procedure SeekRecord;
    procedure WriteRecord;
    procedure WriteField(FieldNo: integer);
    procedure WriteHeader;
    procedure WriteMemoIndex(FieldNo, IdxOffset, Len: integer); virtual;
    function GetRecordDeleted: boolean;
    procedure SetRecordDeleted(Value: boolean);
    function IsNullMaskField(FieldNo: integer): boolean; virtual;
    function IsAllowedFieldType(FieldType: AnsiChar): boolean; virtual;
    function IsNullableFieldType(FieldType: AnsiChar; Flags: byte): boolean; virtual;
    function IsAllowedIndexType(FieldType: AnsiChar): boolean; virtual;
    function GetDbfType(const DataType, FieldName: string): AnsiChar;
    procedure SetFieldDef(FieldPtr: PDBFField; ColumnInfo: PColumnInfo; var Offset: integer); virtual;
    procedure SetFieldConstraint(FieldPtr: PDBFField; ConstraintInfo: PConstraintInfo); virtual;
    procedure SetHasMemo(Value: boolean); virtual;
    function GetMemoValue(FieldNo, MemoIdx, MemoLength: integer; IsInline, IsBinary: boolean): variant;
    function SetMemoValue(FieldNo: integer; IsInline, IsBinary, WriteLength: boolean; const Value: PVirtualValue): integer; virtual;
    function ContainsChars(FieldNo: integer; ExceptOf: byte; HasLengthMarker: boolean): boolean;{$IFDEF USE_INLINE} inline;{$ENDIF}
    procedure InternalOpenIndexFile; virtual;

    procedure DoDeleteFile(FName: string);
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); virtual;
    destructor Destroy; override;

    function GetStream: TStream;
    procedure SetStream(const Stream: TStream);

    function Active: boolean;
    procedure ReadHeader;
    function GetFileMode(ForceReadOnly, ForceExclusive: boolean): Word;
    procedure Prepare; virtual;
    function Open(ForceReadOnly: boolean = False; ForceExclusive: boolean = False): boolean; virtual;
    procedure OpenIndexFile;
    class function GetMemoClass: TDBFMemoClass; virtual;
    class function GetIndexClass: TDBFIndexClass; virtual;
    procedure OpenMemoFile;
    procedure Close;
    procedure ClearTransactionData;
    procedure RollbackTransaction;

    function EOF: boolean;
    procedure First;
    procedure Next;

    function VirtualContainsValue(const Value: TVirtualValue): boolean; virtual;
    function GetRecordCount: integer;
    procedure SetRecordCount(Value: integer);
    class function GetFileClass(DBFFormat: TDBFFormat): TDBFFileClass;
    class function GetMemoExt: string; virtual;
    class function GetIndexExt: string; virtual;
    function GetField(FieldNo: integer): PDBFField;
    function GetFieldIsKey(FieldNo: integer): boolean;
    function GetFieldType(FieldNo: integer): AnsiChar; virtual;
    function GetFieldDbfType(FieldNo: integer): Word;
    function GetFieldLength(FieldNo: integer): Byte; virtual;
    function GetFieldNumDecimal(FieldNo: integer): Byte; virtual;
    function GetFieldFlags(FieldNo: integer): Byte; virtual;
    function GetFieldNullable(FieldNo: integer): boolean; virtual;
    function GetFieldTypeIsMemo(FieldType: AnsiChar): boolean; virtual;
    function GetFieldName(FieldNo: integer): string; virtual;
    function GetFieldNo(FieldName: string): integer;
    function GetFieldCount: integer;
    function GetFieldNull(FieldNo: integer): boolean; virtual;
    function GetFieldValue(FieldNo: integer): variant; virtual;
    function GetFieldAsString(FieldNo: integer): string; overload; virtual;
    function GetFieldAsString(FieldNo: integer; Len: integer): string; overload;
    function GetFieldAsFloat(FieldNo: integer; ForceIgnoreErrors: boolean = False): double; virtual;

    procedure SetFieldValue(FieldNo: integer; var Value: TVirtualValue; GenerateAutoInc: boolean); virtual;

    function GetIndexKeyData(const Value: TVirtualValue; FieldNo, IdxNo: integer): TBytes; virtual;
    procedure DescribeField(FieldNo: integer; var Idx: integer; var FieldDesc: TVirtualFieldDesc); virtual;
    function GetNullableFieldCount: integer; virtual;
    function GetNullFlagsLength(NullableFieldCount: integer): integer; virtual;

    procedure SetRecno(Value: integer);
    function ReadRecord: boolean;
    procedure EditRecord(const Values: TVirtualValues);
    procedure InsertRecord(const Values: TVirtualValues);

    procedure CreateTable(const Database, TableName: string; const Members: TTableMembers);
    procedure DropTable;
    procedure DropMemoFile;
    procedure DropIndexFile;
    procedure PackTable(PackMemo, PackDbf: boolean); virtual;
    procedure AlterTableAddField(const Members: TTableMembers);
    procedure AlterTableDropField(const Members: TTableMembers);
    procedure AlterTableAlterField(const Members: TTableMembers);
    procedure AlterTableCreateIndex(const Members: TTableMembers);
    procedure AlterTableDropIndex(const Members: TTableMembers);
    procedure ZapTable; virtual;
    procedure ReindexTable; virtual;

    procedure RaiseDataError(FieldNo: integer);

    property Prepared: boolean read FPrepared;
    property DBFFormat: TDBFFormat read FDBFFormat;
    property HeaderFormat: TDBFFormat read FHeaderFormat;
    property CodePage: TDBFCodePage read FCodePage write FCodePage;
    property RecNo: integer read FCurrentRecNo write SetRecno;
    property RecordDeleted: boolean read GetRecordDeleted write SetRecordDeleted;
    property FieldCount: integer read FFieldCount;
    property IndexFileType: TIndexFileType read FIndexFileType;
    property IndexFile: TDBFIndexFile read FIndexFile;
    property InTransaction: boolean read FInTransaction write FInTransaction;
    property NullMaskFieldIndex: integer read FNullMaskFieldIndex;
    property ConnectMode: TDBFConnectMode read FConnectMode write FConnectMode;
    property ForceReadOnly: boolean read FForceReadOnly;
    property ForceExclusive: boolean read FForceExclusive;
    property IdentifierCase: TDBFIdentifierCase read FIdentifierCase write FIdentifierCase;
    property FileEncoding: {$IFNDEF NEXTGEN}Encoding{$ELSE}TEncoding{$ENDIF} read FEncoding;
    property IndexOnReading: TDBFIndexKind read FIndexOnReading write FIndexOnReading;
    property IgnoreDataErrors: boolean read FIgnoreDataErrors write FIgnoreDataErrors;
    property IgnoreMetadataErrors: boolean read FIgnoreMetadataErrors write FIgnoreMetadataErrors;
    property IgnoreBrokenTables: boolean read FIgnoreBrokenTables write FIgnoreBrokenTables;
    property AllFieldsAsNullable: boolean read FAllFieldsAsNullable write FAllFieldsAsNullable;
  end;

  TDBFClipper = class(TDBFDBase)
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); override;

    class function GetMemoClass: TDBFMemoClass; override;
  end;

  TDBFDBaseIV = class(TDBFDBase)
  protected
    procedure SetHasMemo(Value: boolean); override;
    procedure InternalOpenIndexFile; override;
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); override;

    class function GetMemoClass: TDBFMemoClass; override;
    class function GetIndexClass: TDBFIndexClass; override;
    class function GetIndexExt: string; override;

    function GetIndexKeyData(const Value: TVirtualValue; FieldNo, IdxNo: integer): TBytes; override;

    { test dBaseIV+ }
    function TestSearchDepth(FieldNo, First, Last: integer): integer;
    procedure TestDuplicateIndexPath(IdxName: string);
  end;

  TDBFDBaseV = class(TDBFDBaseIV) // dBase for Windows
  protected
    function IsAllowedFieldType(FieldType: AnsiChar): boolean; override;
    function IsAllowedIndexType(FieldType: AnsiChar): boolean; override;
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); override;
  end;

  TDBFDBaseVII = class(TDBFDBaseIV)
  protected
    function IsAllowedFieldType(FieldType: AnsiChar): boolean; override;
    function IsNullableFieldType(FieldType: AnsiChar; Flags: byte): boolean; override;
    function IsAllowedIndexType(FieldType: AnsiChar): boolean; override;
    procedure SetFieldDef(FieldPtr: PDBFField; ColumnInfo: PColumnInfo; var Offset: integer); override;
    procedure SetFieldConstraint(FieldPtr: PDBFField; ConstraintInfo: PConstraintInfo); override;
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); override;

    function Open(ForceReadOnly: boolean = False; ForceExclusive: boolean = False): boolean; override;

    function GetPropValue(PropIndex: integer): variant;
    function GetFieldType(FieldNo: integer): AnsiChar; override;
    function GetFieldTypeIsMemo(FieldType: AnsiChar): boolean; override;
    function GetFieldLength(FieldNo: integer): Byte; override;
    function GetFieldNumDecimal(FieldNo: integer): Byte; override;
    function GetFieldName(FieldNo: integer): string; override;
    function GetFieldNull(FieldNo: integer): boolean; override;
    function GetFieldValue(FieldNo: integer): variant; override;
    procedure SetFieldValue(FieldNo: integer; var Value: TVirtualValue; GenerateAutoInc: boolean); override;
    function GetIndexKeyData(const Value: TVirtualValue; FieldNo, IdxNo: integer): TBytes; override;
    procedure DescribeField(FieldNo: integer; var Idx: integer; var FieldDesc: TVirtualFieldDesc); override;
  end;

{$ENDIF}

implementation

{$IFDEF DBFENGINE}

uses
  Math,
{$IFNDEF UNIDACPRO}
  DBFFoxPro, DBFHiPerSix;
{$ELSE}
  DBFFoxProUni, DBFHiPerSixUni;
{$ENDIF}

{$IFDEF LOG_PACKETS}
var
  InsertRecordTickCount, InsertRecordStartTick: Cardinal;
{$ENDIF}

{ TDBFDBase }

constructor TDBFDBase.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited Create;

{$IFDEF USE_TFORMATSETTINGS}
  FDateTimeFormatSettings.DateSeparator := '-';
  FDateTimeFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  FDateTimeFormatSettings.ShortDateFormat := FDateTimeFormatSettings.LongDateFormat;
  FDateTimeFormatSettings.TimeSeparator := ':';
  FDateTimeFormatSettings.LongTimeFormat := 'hh:mm:ss';
  FDateTimeFormatSettings.ShortTimeFormat := FDateTimeFormatSettings.LongTimeFormat;
{$ENDIF}

  FDBFFormat := dfdBaseIII;
  FHeaderFormat := HeaderFormat;
  FCodePage := dpDefault;
  FConnectMode := cmShared;
  FIndexOnReading := ikNative;
  FIgnoreDataErrors := False;
  FIgnoreMetadataErrors := False;
  FIgnoreBrokenTables := False;
  FIdentifierCase := icOriginal;
  FAllFieldsAsNullable := False;
  FForceReadOnly := False;
  FForceExclusive := False;

  FFileName := IncludeTrailingPathDelimiter(Database) + TableName;
  if ExtractFileExt(FFileName) = '' then
    FFileName := FFileName + DBF_EXT;

  FHeaderSize := SizeOf(TDBFHeader);
  FFieldSize := DBaseFieldSize;
  FCurrentRecNo := -1;
  FEncoding := nil;
  FHasDeletedFlag := True;
  FPrepared := False;

  FCalculator := TDBFExpressionCalculator.Create;
end;

destructor TDBFDBase.Destroy;
begin
  FCalculator.Free;

  Close;

  inherited;
end;

procedure TDBFDBase.AddFieldToIndexFields(var IndexFields: TIndexFieldArray; FieldNo: integer; const IndexName: string);
var
  i: integer;
begin
  for i := 0 to Length(indexFields) - 1 do
    if indexFields[i].FieldNo = FieldNo then
      Exit;

  SetLength(indexFields, Length(indexFields) + 1);
  indexFields[High(indexFields)].FieldNo := FieldNo;
  indexFields[High(indexFields)].IndexName := UpperCase(IndexName);
end;

function TDBFDBase.InternalStrToDateTime(const S: string): TDateTime;
{$IFNDEF USE_TFORMATSETTINGS}
var
  OldDateSeparator, OldTimeSeparator: char;
  OldDateFormat, OldLongDateFormat, OldTimeFormat, OldLongTimeFormat: string;

  procedure SetSeparators;
  begin
    OldDateSeparator := DateSeparator;
    OldDateFormat := ShortDateFormat;
    OldLongDateFormat := LongDateFormat;
    OldTimeSeparator := TimeSeparator;
    OldTimeFormat := ShortTimeFormat;
    OldLongTimeFormat := LongTimeFormat;

    DateSeparator := '-';
    ShortDateFormat := 'yyyy-mm-dd';
    LongDateFormat := 'yyyy-mm-dd';
    TimeSeparator := ':';
    ShortTimeFormat := 'hh:mm:ss';
    LongTimeFormat := 'hh:mm:ss';
  end;

  procedure RestoreSeparators;
  begin
    DateSeparator := OldDateSeparator;
    ShortDateFormat := OldDateFormat;
    LongDateFormat := OldLongDateFormat;
    TimeSeparator := OldTimeSeparator;
    ShortTimeFormat := OldTimeFormat;
    LongTimeFormat := OldLongTimeFormat;
  end;
{$ENDIF}

begin
{$IFDEF USE_TFORMATSETTINGS}
{$IFNDEF FPC}
  Result := StrToDateTimeDef(S, 0, FDateTimeFormatSettings);
{$ELSE}
  if not TryStrToDateTime(S, Result, FDateTimeFormatSettings) then
    Result := 0;
{$ENDIF}
{$ELSE}
  SetSeparators;
  try
    Result := StrToDateTimeDef(S, 0);
  finally
    RestoreSeparators;
  end;
{$ENDIF}
end;

function TDBFDBase.GetVirtualValueType(const V: variant): TVirtualType;
begin
  case TVarData(V).VType of
    varEmpty,
    varNull:
      Result := vrNull;
    varSmallint,
    varInteger,
    varShortInt,
    varByte,
    varWord,
    varLongWord,
    varInt64{$IFDEF VER12P}, varUInt64{$ENDIF}:
      Result := vrInteger;
    varDate, // as string ?
    varSingle,
    varDouble{$IFDEF MSWINDOWS}, varCurrency{$ENDIF}:
      Result := vrFloat;
    varOleStr,
    varString{$IFDEF VER12P}{$IFDEF VER16P}, varUStrArg{$ENDIF}, varUString{$ENDIF}:
      Result := vrString;
    varSharedObject:
      Result := vrBlob;
  else
    Result := vrNull;
  end;
end;

function TDBFDBase.GetIndexFieldValue(const Value: TVirtualValue; const FieldNo: integer; const FieldName: string): variant;
var
  i: integer;
begin
  i := GetFieldNo(FieldName);
  if (i >= 0) and (i = FieldNo) then
    Result := Value.Value
  else if FieldNo >= 0 then
    Result := GetFieldValue(i)
  else
    Result := null;
end;

procedure TDBFDBase.SeekRecord;
begin
  if FStream = nil then
    Open(True, False);

  if (FCurrentRecNo <> -1) and (FStream.Seek(LongInt(FHeader.DB3.dataOffset + FCurrentRecNo * FHeader.DB3.recLen), soBeginning) <= 0) then
    raise Exception.Create('DBFData seek error');
end;

procedure TDBFDBase.WriteRecord;
var
  written: integer;
begin
  SeekRecord;
  written := FStream.Write(FCurrentRecordBuffer[0], FHeader.DB3.recLen);
  if written <> FHeader.DB3.recLen then
    raise Exception.CreateFmt('Written %d bytes instead of %d', [written, FHeader.DB3.recLen]);
end;

procedure TDBFDBase.WriteField(FieldNo: integer);
var
  written: integer;
begin
  if FStream.Seek(LongInt(FHeaderSize + FieldNo * FFieldSize), soBeginning) <= 0 then
    raise Exception.Create('DBFData seek error');

  written := FStream.Write(FFields[FieldNo * FFieldSize], FFieldSize);
  if written <> FFieldSize then
    raise Exception.CreateFmt('Written %d bytes instead of %d', [written, FFieldSize]);
end;

procedure TDBFDBase.WriteHeader;
var
  written: integer;
begin
  if FStream.Seek(LongInt(0), soBeginning) < 0 then
    raise Exception.Create('DBFData seek error');

  // todo date modified ?
  written := FStream.Write(FHeader, FHeaderSize);
  if written <> FHeaderSize then
    raise Exception.CreateFmt('Written %d bytes instead of %d', [written, FHeaderSize]);
end;

procedure TDBFDBase.WriteMemoIndex(FieldNo, IdxOffset, Len: integer);
var
  aStr: AnsiString;
begin
  FillChar(FCurrentRecordBuffer[FOffsets[FieldNo]], FLengths[FieldNo], ANSI_SPACE);
  if IdxOffset > 0 then begin
    aStr := AnsiString(IntToStr(IdxOffset));
    Move(PAnsiChar(aStr)^, FCurrentRecordBuffer[FOffsets[FieldNo] + FLengths[FieldNo] - Length(aStr)], Length(aStr));
  end;
end;

function TDBFDBase.GetRecordDeleted: boolean;
begin
  Result := FCurrentRecordBuffer[0] = DBF_RECORD_DELETED;
end;

procedure TDBFDBase.SetRecordDeleted(Value: boolean);
var
  i: integer;
begin
  Open(False, False);
  ReadRecord;

  if FInTransaction then begin
    i := Length(FDBFTransactionData);
    SetLength(FDBFTransactionData, Length(FDBFTransactionData) + 1);
    FDBFTransactionData[i].RecNo := RecNo;
    FDBFTransactionData[i].Action := trActionDelete;
  end;

  if Value then
    FCurrentRecordBuffer[0] := DBF_RECORD_DELETED
  else
    FCurrentRecordBuffer[0] := DBF_RECORD_BEGIN;
  WriteRecord;
end;

procedure TDBFDBase.ClearTransactionData;
var
  i: integer;
begin
  for i := 0 to Length(FDBFTransactionData) - 1 do
    FDBFTransactionData[i].Buffer := nil;
  FDBFTransactionData := nil;
end;

procedure TDBFDBase.RollbackTransaction;
var
  i: integer;
begin
  for i := Length(FDBFTransactionData) - 1 downto 0 do begin
    SetRecno(FDBFTransactionData[i].RecNo);
    ReadRecord;
    case FDBFTransactionData[i].Action of
      trActionInsert:
        FCurrentRecordBuffer[0] := DBF_RECORD_DELETED;
      trActionEdit:
        Move(FDBFTransactionData[i].Buffer[0], FCurrentRecordBuffer[0], FHeader.DB3.recLen);
      trActionDelete:
        FCurrentRecordBuffer[0] := DBF_RECORD_BEGIN;
    else
      raise Exception.CreateFmt('Unknown action %d', [Ord(FDBFTransactionData[i].Action)]);
    end;
    WriteRecord;
  end;
end;

function TDBFDBase.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TDBFDBase.SetStream(const Stream: TStream);
begin
  FStream := Stream;
end;

function TDBFDBase.Active: boolean;
begin
  Result := FStream <> nil;
end;

procedure TDBFDBase.ReadHeader;
var
  readed: integer;
  FileLocale: Cardinal;
  DriverName: AnsiString;
begin
  Assert(FStream <> nil);

  FStream.Seek(LongInt(0), soFromBeginning);
  readed := FStream.Read(FHeader, FHeaderSize);
  if readed < SizeOf(TDBFHeader) then
    raise Exception.CreateFmt('Error reading file header: %s'#13#10'File size: %d, readed: %d', [FFileName, FStream.Size, readed]);

  FEncoding := nil;
  if FCodePage <> dpDefault then
    FEncoding := Encoding.GetEncoding(CodePageLocale[FCodePage])
  else begin
    if FHeaderFormat = dfdBaseVII then begin
    {$IFNDEF NEXTGEN}
      DriverName := AnsiString(FHeader.langDrvName);
    {$ELSE}
      if Length(FHeader.langDrvName) > 0 then begin
        DriverName.SetLength(Length(FHeader.langDrvName));
        Move(FHeader.langDrvName[0], DriverName.Ptr^, DriverName.Length);
      end
      else
        DriverName := '';
    {$ENDIF}

      FileLocale := GetCodepage(string(DriverName));
    end
    else
      FileLocale := CodePageLocale[LanguageIdToCodePage(FHeader.DB3.langDrv)];
    if FileLocale <> 0 then
      FEncoding := Encoding.GetEncoding(FileLocale);
  end;

  FHasSqlTableFiles := (FHeader.DB3.version and DBF_HAS_SQLTABLE) <> 0;
  FHasDbtMemoFiles := ((FHeader.DB3.version and DBF_HAS_DBT) <> 0) or ((FDBFFormat in [dfFoxPro2, dfCodebase, dfVisualFoxPro]) and ((FHeader.DB3.flags and DBF_FLAG_HAS_MEMO) <> 0));
end;

procedure TDBFDBase.DoDeleteFile(FName: string);
{$IFNDEF UNIX}
var
  LastError: Cardinal;
{$ENDIF}
begin
{$IFNDEF UNIX}
  if not SysUtils.DeleteFile(FName) then begin
    LastError := GetLastError;
    if LastError <> 2 then // ERROR_FILE_NOT_FOUND
      raise Exception.CreateFmt('Cannot delete file, error "%s"', [SysErrorMessage(LastError)]);
  end;
{$ELSE}
  if FileExists(FName) then
    if not SysUtils.DeleteFile(FName) then
      raise Exception.Create('Cannot delete file');
{$ENDIF}
end;

class function TDBFDBase.GetFileClass(DBFFormat: TDBFFormat): TDBFFileClass;
begin
  case DBFFormat of
    dfdBaseIII:
      Result := TDBFDBase;
    dfClipper:
      Result := TDBFClipper;
    dfdBaseIV, dfdBaseV:
      Result := TDBFDBaseV;
    dfdBaseVII:
      Result := TDBFDBaseVII;
    dfFoxPro2:
      if DBFFormat = dfCodebase then
        Result := TDBFCodebase
      else
        Result := TDBFFoxPro;
    dfVisualFoxPro:
      Result := TDBFVisualFoxPro;
    dfHiPerSix:
      Result := TDBFHiPerSix;
    dfCodebase:
      Result := TDBFCodebase;
  else
    raise Exception.CreateFmt('Unknown DBFFormat %s', [GetEnumName(TypeInfo(TDBFFormat), Ord(DBFFormat))]);
  end;
end;

class function TDBFDBase.GetMemoExt: string;
begin
  Result := DBT_EXT;
end;

class function TDBFDBase.GetIndexExt: string;
begin
  Result := '';
end;

procedure TDBFDBase.SetRecno(Value: integer);
begin
  if FCurrentRecNo <> Value then
    FCurrentRecNo := Value;
end;

function TDBFDBase.ReadRecord: boolean;
var
  readed: integer;
begin
  Result := False;
  if FCurrentRecNo < 0 then
    Exit;
  SeekRecord;
  readed := FStream.Read(FCurrentRecordBuffer[0], FHeader.DB3.recLen);
  if readed < FHeader.DB3.recLen then
    if AnsiChar(FCurrentRecordBuffer[0]) = DBF_RECORD_EOF then begin
      SetRecordCount(FCurrentRecNo);
      WriteHeader;
    end
    else
      raise Exception.CreateFmt('Read %d bytes instead of %d', [readed, FHeader.DB3.recLen]);
  Result := True;
end;

function TDBFDBase.VirtualContainsValue(const Value: TVirtualValue): boolean;
begin
  Result := not (VarIsNull(Value.Value) or VarIsEmpty(Value.Value) or (Value.ValueType = vrNull));
end;

function TDBFDBase.EOF: boolean;
begin
  Result := FCurrentRecNo >= GetRecordCount;
end;

procedure TDBFDBase.First;
begin
  if GetRecordCount > 0 then
    SetRecno(0)
  else
    SetRecno(-1);
end;

procedure TDBFDBase.Next;
begin
  if not EOF then
    SetRecno(RecNo + 1);
end;

function TDBFDBase.GetRecordCount: integer;
begin
  Result := FHeader.DB3.numRecs;
end;

procedure TDBFDBase.SetRecordCount(Value: integer);
begin
  FHeader.DB3.numRecs := Value;
end;

function TDBFDBase.GetField(FieldNo: integer): PDBFField;
begin
  Result := @FFields[FieldNo * FFieldSize];
end;

function TDBFDBase.GetFieldIsKey(FieldNo: integer): boolean;
begin
  Result := FKeyFields[FieldNo];
end;

function TDBFDBase.GetFieldNo(FieldName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to GetFieldCount - 1 do
    if SameText(FieldName, GetFieldName(i)) then begin
      Result := i;
      Break;
    end;
end;

function TDBFDBase.GetFieldCount: integer;
begin
  Result := Length(FFields) div FFieldSize;
end;

function TDBFDBase.GetIndexKeyData(const Value: TVirtualValue; FieldNo, IdxNo: integer): TBytes;
begin
  Result := nil;
end;

function TDBFDBase.GetFieldType(FieldNo: integer): AnsiChar;
var
  fieldPtr: PDBFField;
begin
  fieldPtr := GetField(FieldNo);
  Result := fieldPtr.DB3FType;
end;

function TDBFDBase.GetFieldDbfType(FieldNo: integer): Word;
var
  fType: AnsiChar;
begin
  fType := GetFieldType(FieldNo);
  case fType of
    DBF_TYPE_CHAR:
      Result := dbfChar;
    DBF_TYPE_NUMERIC:
      Result := dbfNumeric;
    DBF_TYPE_DATE:
      Result := dbfDate;
    DBF_TYPE_LOGICAL:
      Result := dbfLogical;
    DBF_TYPE_MEMO:
      Result := dbfMemo;

  // TDBFDBaseV
    DBF_TYPE_FLOAT:
      Result := dbfNumeric;
    DBF_TYPE_BINARY: begin
      if FDBFFormat in [dfVisualFoxPro, dfAuto] then
        Result := dbfDouble
      else
        Result := dbfBlob;
    end;
    DBF_DB7_TYPE_DOUBLE:
      Result := dbfDouble;
    DBF_TYPE_GENERAL:
      Result := dbfBlob;

  // TDBFDBaseVII
    DBF_TYPE_INTEGER:
      Result := dbfInteger;
    DBF_TYPE_TIMESTAMP:
      Result := dbfTime;
  {$IFDEF DBFENGINE}
    DBF_TYPE_AUTOINC:
      Result := dbfAutoincrement;
  {$ENDIF}

  {$IFDEF DBFENGINE}
    DBF_TYPE_CURRENCY:
      Result := dbfCurrency;
  {$ENDIF}
    DBF_TYPE_TIME:
      Result := dbfTime;
    DBF_TYPE_PICTURE:
      Result := dbfBlob;
    DBF_TYPE_VARBINARY:
      Result := dbfBlob;
    DBF_TYPE_VARCHAR:
      Result := dbfVarChar;
    DBF_TYPE_BLOB:
      Result := dbfBlob;
    DBF_TYPE_NULLFLAGS:
      Result := dbfVarBinary;
  else
    raise Exception.CreateFmt('Unknown field type %s', [string(AnsiString(fType))]);
  end;
end;

function TDBFDBase.GetFieldLength(FieldNo: integer): Byte;
var
  fieldPtr: PDBFField;
begin
  fieldPtr := GetField(FieldNo);
  Result := fieldPtr.DB3Len;
end;

function TDBFDBase.GetFieldNumDecimal(FieldNo: integer): Byte;
var
  fieldPtr: PDBFField;
begin
  fieldPtr := GetField(FieldNo);
  Result := fieldPtr.DB3NumDecimal;
end;

function TDBFDBase.IsAllowedFieldType(FieldType: AnsiChar): boolean;
begin
  Result := FieldType in [DBF_TYPE_NUMERIC, DBF_TYPE_CHAR, DBF_TYPE_DATE, DBF_TYPE_LOGICAL, DBF_TYPE_MEMO];
end;

function TDBFDBase.IsNullableFieldType(FieldType: AnsiChar; Flags: byte): boolean;
begin
  Result := FAllFieldsAsNullable;
end;

function TDBFDBase.IsAllowedIndexType(FieldType: AnsiChar): boolean;
begin
  Result := FieldType in [DBF_TYPE_NUMERIC, DBF_TYPE_CHAR, DBF_TYPE_DATE];
end;

function TDBFDBase.GetFieldName(FieldNo: integer): string;
var
  fieldPtr: PDBFField;
begin
  fieldPtr := GetField(FieldNo);
  Result := DB3NameToString(fieldPtr);
end;

function TDBFDBase.IsNullMaskField(FieldNo: integer): boolean;
begin
  Result := False;
end;

function TDBFDBase.GetFieldFlags(FieldNo: integer): Byte;
begin
  Result := 0;
end;

function TDBFDBase.GetFieldNullable(FieldNo: integer): boolean;
begin
  Result := FAllFieldsAsNullable;
end;

function TDBFDBase.GetFieldTypeIsMemo(FieldType: AnsiChar): boolean;
begin
  Result := FieldType in [DBF_TYPE_MEMO, DBF_TYPE_GENERAL];
end;

function TDBFDBase.GetFieldNull(FieldNo: integer): boolean;
var
  fType: AnsiChar;
  aStr: AnsiString;
begin
  fType := GetFieldType(FieldNo);

  case fType of
    DBF_TYPE_CHAR:
      // FoxPro has both left to right and right to left (due to "binary" flag probably)
      // there can be leading spaces
      Result := ContainsChars(FieldNo, ANSI_SPACE, True);
    DBF_TYPE_NUMERIC, DBF_TYPE_FLOAT: begin
      if FAlignments[FieldNo] = naLeft then
        Result := FCurrentRecordBuffer[FOffsets[FieldNo]] = ANSI_SPACE
      else begin
        Result := FCurrentRecordBuffer[FOffsets[FieldNo] + FLengths[FieldNo] - 1] = ANSI_SPACE;
        if Result and (FCurrentRecordBuffer[FOffsets[FieldNo]] <> ANSI_SPACE) then begin
          FAlignments[FieldNo] := naLeft;
          Result := False;
        end;
      end;
    end;
    DBF_TYPE_DATE, DBF_TYPE_LOGICAL:
      Result := FCurrentRecordBuffer[FOffsets[FieldNo]] = ANSI_SPACE;
    DBF_TYPE_MEMO, DBF_TYPE_GENERAL, DBF_TYPE_BLOB: begin
      SetLengthA(aStr, FLengths[FieldNo]);
      Move(FCurrentRecordBuffer[FOffsets[FieldNo]], PAnsiChar(aStr)^, FLengths[FieldNo]);
      Result := (Trim(string(aStr)) = '') or ((FLengths[FieldNo] = 10) and (StrToIntDef(string(aStr), 0) <= 0));
    end;
  else
    raise Exception.CreateFmt('Unknown type %s', [fType]);
  end;
end;

procedure TDBFDBase.DescribeField(FieldNo: integer; var Idx: integer; var FieldDesc: TVirtualFieldDesc);
var
  flags: Byte;
  fType: AnsiChar;
  dbfType: Word;
begin
  flags := GetFieldFlags(FieldNo);
  fType := GetFieldType(FieldNo);
  dbfType := GetFieldDbfType(FieldNo);

  FieldDesc.Name := GetFieldName(FieldNo);
  case FIdentifierCase of
    icLower: FieldDesc.Name := LowerCase(FieldDesc.Name);
    icUpper: FieldDesc.Name := UpperCase(FieldDesc.Name);
  end;

  // FoxPro autoincrement integer with flag
  if (FDBFFormat = dfVisualFoxPro) and ((flags and DBF_FIELDFLAG_AUTOINC) <> 0) then
    dbfType := dbfAutoincrement;

  if fType in [DBF_TYPE_CHAR, DBF_TYPE_NUMERIC, DBF_TYPE_FLOAT, DBF_TYPE_VARBINARY, DBF_TYPE_VARCHAR] then
    FieldDesc.Length := GetFieldLength(FieldNo);
  FieldDesc.Scale := GetFieldNumDecimal(FieldNo);
  FieldDesc.DBType := dbfType;

  case dbfType of
    dbfChar:
      FieldDesc.DataType := dtString;
    dbfCurrency:
      FieldDesc.DataType := dtCurrency;
    dbfNumeric: begin
      if FieldDesc.Scale = 0 then
        FieldDesc.DataType := dtInt64
      else
        FieldDesc.DataType := dtFloat;
    end;
    dbfFloat:
      FieldDesc.DataType := dtSingle;
    dbfDate:
      FieldDesc.DataType := dtDate;
    dbfTime:
      FieldDesc.DataType := dtDateTime;
    dbfTimeStamp:
      FieldDesc.DataType := dtSQLTimeStamp;
    dbfDouble: begin
      FieldDesc.DataType := dtFloat;
    end;
    dbfInteger:
      FieldDesc.DataType := dtInteger;
    dbfLogical:
      FieldDesc.DataType := dtBoolean;
    dbfMemo:
      FieldDesc.DataType := dtMemo;
    dbfBlob:
      FieldDesc.DataType := dtBlob; // Picture
    dbfAutoincrement:
      FieldDesc.DataType := dtInteger;
    dbfVarBinary:
      FieldDesc.DataType := dtVarBytes;
    dbfVarChar:
      FieldDesc.DataType := dtString;
  else
    raise Exception.CreateFmt('Unknown dbfType %d', [dbfType]);
  end;

  FieldDesc.IsKey := GetFieldIsKey(FieldNo);
  FieldDesc.IsAutoIncrement := dbfType = dbfAutoincrement;
  FieldDesc.Required := not (IsNullableFieldType(fType, flags) or FAllFieldsAsNullable);
  FieldDesc.Hidden := (flags and DBF_FIELDFLAG_SYSTEM) <> 0;
  FieldDesc.ActualIndex := FieldNo;

  {TODO: ?}//FFields[i].ReadOnly := FMemData.Fields[i].ReadOnly;
end;

function TDBFDBase.GetFileMode(ForceReadOnly, ForceExclusive: boolean): Word;
begin
  if ForceReadOnly and (FConnectMode <> cmExclusive) then
    Result := fmOpenRead
  else
    Result := fmOpenReadWrite;

  if FConnectMode = cmShared then
    Result := Result or fmShareDenyWrite
  else if FConnectMode = cmUnsafe then
    Result := Result or fmShareDenyNone
  else
    Result := Result or fmShareExclusive;
end;

procedure TDBFDBase.Prepare;
const
  MISMATCH_SIZE_ERROR = 'Field "%s" of type "%s" has a wrong size %d (%s).';
  MISMATCH_TYPE_ERROR = 'Type "%s" of the field "%s" is not allowed for a table of type %s (%s).';
var
  i, readed, offs: integer;
  fieldPtr: PDBFField;
  fType: AnsiChar;
begin
  if FPrepared then
    Exit;

  FSavedPosition := 0;
  FStream := TFileStream.Create(FFileName, GetFileMode(True, False));
  try
    ReadHeader;
    if FHeader.DB3.recLen <= 0 then begin
      if FIgnoreBrokenTables then
        Exit
      else
        raise Exception.CreateFmt('Invalid record size (%d): %s', [FHeader.DB3.recLen, FFileName]);
    end;
    SetLength(FCurrentRecordBuffer, FHeader.DB3.recLen);

    FFieldCount := 0;
    FFields := nil;
    while (FStream.Position + FFieldSize) <= FHeader.DB3.dataOffset do begin
      SetLength(FFields, Length(FFields) + FFieldSize);
      fieldPtr := GetField(FFieldCount);
      offs := FStream.Position;
      readed := FStream.Read(fieldPtr^, FFieldSize);
      if (readed > 0) and (PByte(fieldPtr)^ = DBF_SECTION_END) then begin // terminator
        FStream.Position := offs + 1;
        SetLength(FFields, Length(FFields) - FFieldSize);
        Break;
      end;

      if readed < FFieldSize then
        raise Exception.CreateFmt('error: readed %d', [readed]);

    {$IFDEF LOG_PACKETS}
      if FHeaderFormat = dfdBaseVII then
        AddToLog(Format(
          '%d: name: %s, type: %s, len: %d, numDec: %d, flagMDX: %X',
          [FFieldCount, string(fieldPtr.DB7Name), string(fieldPtr.DB7FType), fieldPtr.DB7Len, fieldPtr.DB7NumDecimal, fieldPtr.DB7FlagMDX]))
      else
        AddToLog(Format(
          '%d: name: %s, type: %s, len: %d, numDec: %d, flagMDX: %X',
          [FFieldCount, string(fieldPtr.DB3Name), string(fieldPtr.DB3FType), fieldPtr.DB3Len, fieldPtr.DB3NumDecimal, fieldPtr.DB3FlagMDX]));
    {$ENDIF}
      Inc(FFieldCount);
    end;

    if FHasDeletedFlag then
      offs := 1 // zero offset reserved for "Deleted"
    else
      offs := 0;
    SetLength(FOffsets, FFieldCount);
    SetLength(FLengths, FFieldCount);
    SetLength(FAlignments, FFieldCount);
    SetLength(FKeyFields, FFieldCount);

    for i := 0 to FFieldCount - 1 do begin
      fieldPtr := GetField(i);
      FOffsets[i] := offs;
      FAlignments[i] := naRight;

      if FHeaderFormat = dfdBaseVII then begin
        FLengths[i] := fieldPtr.DB7Len;
        fType := fieldPtr.DB7FType;
      end
      else begin
        FLengths[i] := fieldPtr.DB3Len;
        fType := fieldPtr.DB3FType;
      end;

      if {$IFNDEF NEXTGEN}fType = #0{$ELSE}Ord(fType) = 0{$ENDIF} then
        raise Exception.CreateFmt(MISMATCH_TYPE_ERROR, ['0', GetFieldName(i), GetEnumName(TypeInfo(TDBFFormat), Ord(FDBFFormat)), FFileName])
      else
        if not IsAllowedFieldType(fType) and not FIgnoreMetadataErrors then
          raise Exception.CreateFmt(MISMATCH_TYPE_ERROR, [string(AnsiString(fType)), GetFieldName(i), GetEnumName(TypeInfo(TDBFFormat), Ord(FDBFFormat)), FFileName]);

      case fType of
        DBF_TYPE_CHAR,
        DBF_TYPE_VARBINARY,
        DBF_TYPE_VARCHAR:
          if (FLengths[i] < 0) or (((FLengths[i] = 0) or (FLengths[i] > 254)) and not FIgnoreMetadataErrors) then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_NUMERIC,
        DBF_TYPE_FLOAT:
          if (FLengths[i] < 0) or (((FLengths[i] = 0) or (FLengths[i] > 32)) and not FIgnoreMetadataErrors) then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_CURRENCY:
          if (FLengths[i] < SizeOf(Currency)) or ((FLengths[i] > SizeOf(Currency)) and not FIgnoreMetadataErrors) then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_DATE,
        DBF_TYPE_TIME,
        DBF_TYPE_TIMESTAMP:
          if (FLengths[i] < 8) or ((FLengths[i] > 8) and not FIgnoreMetadataErrors) then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_BINARY:
          // Binary: dBASE 5
          // Double: MS Visual FoxPro
          if FDBFFormat = dfVisualFoxPro then begin
            if (FLengths[i] < SizeOf(Double)) or ((FLengths[i] > SizeOf(Double)) and not FIgnoreMetadataErrors) then
              raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
          end
          else if FLengths[i] <= 0 then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_INTEGER:
          if (FLengths[i] < SizeOf(Integer)) or ((FLengths[i] > SizeOf(Integer)) and not FIgnoreMetadataErrors) then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_LOGICAL:
          if (FLengths[i] < SizeOf(Byte)) or ((FLengths[i] > SizeOf(Byte)) and not FIgnoreMetadataErrors) then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_MEMO,
        DBF_TYPE_BLOB,
        DBF_TYPE_OLEBLOB,
        DBF_TYPE_GENERAL:
          if FLengths[i] <= 0 then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_PICTURE:
          raise Exception.CreateFmt('Type %s not implemented yet'#13#10'%s', [string(AnsiString(fType)), FFileName]);
        DBF_TYPE_AUTOINC:
          if (FLengths[i] < SizeOf(Cardinal)) or ((FLengths[i] > SizeOf(Cardinal)) and not FIgnoreMetadataErrors) then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        DBF_TYPE_NULLFLAGS: begin
          FNullMaskFieldIndex := i;
          if FLengths[i] <= 0 then
            raise Exception.CreateFmt(MISMATCH_SIZE_ERROR, [GetFieldName(i), string(AnsiString(fType)), FLengths[i], FFileName]);
        end;
      else
        raise Exception.CreateFmt('Unknown type %s'#13#10'%s', [string(AnsiString(fType)), FFileName]);
      end;
      offs := offs + FLengths[i];
    end;
    FSavedPosition := FStream.Position;
  finally
    FreeAndNil(FStream);
  end;
end;

function TDBFDBase.Open(ForceReadOnly: boolean = False; ForceExclusive: boolean = False): boolean;
begin
  Result := True;

  if (FStream <> nil) and (FConnectMode <> cmExclusive) then begin
    if (FForceReadOnly = ForceReadOnly) and (FForceExclusive = ForceExclusive) then
      Exit
    else
      Close;
  end;

  FForceReadOnly := ForceReadOnly;
  FForceExclusive := ForceExclusive;

  if FStream = nil then begin
    FStream := TFileStream.Create(FFileName, GetFileMode(ForceReadOnly, ForceExclusive));
    ReadHeader;
  end;

  OpenMemoFile;
  OpenIndexFile;
end;

procedure TDBFDBase.OpenIndexFile;
begin
  SetLength(FKeyFields, FFieldCount);

  try
      if FIndexFile = nil then
        InternalOpenIndexFile;
  except
    on e: Exception do
      if SuppressIndexOpenErrors then
        FreeAndNil(FIndexFile)
      else
        raise;
  end;
end;

class function TDBFDBase.GetMemoClass: TDBFMemoClass;
begin
  Result := TDBFMemoFile;
end;

class function TDBFDBase.GetIndexClass: TDBFIndexClass;
begin
  Result := TDBFIndexFile;
end;

procedure TDBFDBase.OpenMemoFile;
begin
  if FHasDbtMemoFiles and (FMemo = nil) then begin
    FMemo := GetMemoClass.Create(Self, FindFileByName(ChangeFileExt(FFileName, GetMemoExt)));
    FMemo.Open;
  end;
end;

procedure TDBFDBase.Close;
begin
  FreeAndNil(FStream);
  FreeAndNil(FMemo);
  FreeAndNil(FIndexFile);
  ClearTransactionData;
end;

function TDBFDBase.GetDbfType(const DataType, FieldName: string): AnsiChar;
const
  UNKNOWN_TYPE_ERROR = 'Unknown field type "%s". %s';
  MISMATCH_TYPE_ERROR = 'Type "%s" of the field "%s" is not allowed for a table of type %s. %s';
var
  ftIdx: integer;
begin
  ftIdx := SqlTypeToDbfType.IndexOf(UpperCase(DataType));
  if ftIdx < 0 then
    raise Exception.CreateFmt(UNKNOWN_TYPE_ERROR, [DataType, FFileName]);

  Result := AnsiChar(SqlTypeToDbfType.Values[ftIdx]);
  if not IsAllowedFieldType(Result) then
    raise Exception.CreateFmt(MISMATCH_TYPE_ERROR, [string(AnsiString(Result)), FieldName, GetEnumName(TypeInfo(TDBFFormat), Ord(FDBFFormat)), FFileName]);
end;

procedure TDBFDBase.SetFieldDef(FieldPtr: PDBFField; ColumnInfo: PColumnInfo; var Offset: integer);
var
  fType: AnsiChar;
  aStr: AnsiString;
begin
  aStr := AnsiString(UpperCase(ColumnInfo.Name));
  if Length(aStr) > Length(FieldPtr.DB3Name) then
    SetLengthA(aStr, Length(FieldPtr.DB3Name));
  if LengthA(aStr) > 0 then
    Move(PAnsiChar(aStr)^, FieldPtr.DB3Name[0], LengthA(aStr));

  fType := GetDbfType(ColumnInfo.DataType, string(aStr));
  FieldPtr.DB3FType := fType;

  if ColumnInfo.Length > 0 then
    FieldPtr.DB3Len := Byte(ColumnInfo.Length)
  else if DbfFieldTypeToFieldLen[Byte(fType)] > 0 then begin
    if (FDBFFormat = dfHiPerSix) and (fType = DBF_TYPE_MEMO) then
      FieldPtr.DB3Len := SizeOf(TSMTMemoLink)
    else if (FDBFFormat in [dfFoxPro2, dfVisualFoxPro]) and (fType = DBF_TYPE_MEMO) then
      FieldPtr.DB3Len := 4
    else
      FieldPtr.DB3Len := DbfFieldTypeToFieldLen[Byte(fType)];
  end
  else
    raise Exception.CreateFmt('Unknown field length for field %s of type %s. %s', [ColumnInfo.Name, ColumnInfo.DataType, FFileName]);
  Inc(Offset, FieldPtr.DB3Len);

  if ColumnInfo.Scale < 0 then
    FieldPtr.DB3NumDecimal := 0
  else
    FieldPtr.DB3NumDecimal := ColumnInfo.Scale;
end;

procedure TDBFDBase.SetFieldConstraint(FieldPtr: PDBFField; ConstraintInfo: PConstraintInfo);
begin
  if (ConstraintInfo <> nil) and (ConstraintInfo.ConstraintType = ctPrimaryKey)
    and (FDBFFormat in [dfdBaseIV, dfdBaseV]) then
    FieldPtr.DB3FlagMDX := DBF_FLAG_HAS_MDX
  else
    FieldPtr.DB3FlagMDX := 0;
end;

procedure TDBFDBase.SetHasMemo(Value: boolean);
begin
  if Value then
    FHeader.DB3.version := FHeader.DB3.version or DBF_HAS_DBT
  else
    FHeader.DB3.version := FHeader.DB3.version and not DBF_HAS_DBT;
end;

function TDBFDBase.GetMemoValue(FieldNo, MemoIdx, MemoLength: integer; IsInline, IsBinary: boolean): variant;
begin
  TVarData(Result).VType := varSharedObject;
  TVarData(Result).VPointer := TBlob.Create;
  if IsInline then begin
    TBlob(TVarData(Result).VPointer).RollbackEnabled := False;
    try
      TBlob(TVarData(Result).VPointer).Write(0, MemoLength, @FCurrentRecordBuffer[FOffsets[FieldNo]]);
    finally
      TBlob(TVarData(Result).VPointer).RollbackEnabled := True;
    end;
  end
  else
    FMemo.Read(TBlob(TVarData(Result).VPointer), MemoIdx, 0, IsBinary);
end;

function TDBFDBase.SetMemoValue(FieldNo: integer; IsInline, IsBinary, WriteLength: boolean; const Value: PVirtualValue): integer;
var
  aStr: AnsiString;
  Buf, Buf1: TBytes;
  PBuf: pointer;
  Len, MemoIdx: integer;
  v: variant;
  Blob: TBlob;
  equal: boolean;
  VType: Word;

  procedure SetAsBlob(Blob: TBlob);
  var
    BlobData: TCRBlobData;
    bufPos, bufLen, count: integer;
  begin
    bufPos := 0;
    BlobData := Blob.GetData;
    count := Blob.Size;
    if count > FLengths[FieldNo] then
      count := FLengths[FieldNo];
    while bufPos < count do begin
      bufLen := BlobData.Read(bufPos, count, @FCurrentRecordBuffer[FOffsets[FieldNo] + bufPos]);
      Inc(bufPos, bufLen);
    end;
  end;

begin
  Result := FLengths[FieldNo];

  if IsInline then begin
    FillChar(FCurrentRecordBuffer[FOffsets[FieldNo]], Result, ANSI_SPACE);
    Result := 0;
    Len := 0;
    equal := False;
    PBuf := nil;
    case Value^.ValueType of
      vrNull:;
      vrString,
      vrAnsiString: begin
        aStr := AnsiString(Value.Value);
        Len := LengthA(aStr);
        if Len > FLengths[FieldNo] then
          SetLengthA(aStr, FLengths[FieldNo])
        else
          Result := Len;

        if IsBinary then
          Buf := FEncoding.ANSI.GetBytes(aStr)
        else if FEncoding = nil then
          Buf := Encoding.Default.GetBytes(aStr)
        else
          Buf := FEncoding.GetBytes(aStr);

        PBuf := @Buf[0];
      end;
      vrBlob: begin
        VType := TVarData(Value.Value).VType;
        if (VType and varByRef) <> 0 then begin
          Blob := TVarData(Value.Value).VPointer;
          SetAsBlob(Blob);
          Len := Blob.Size;
          Result := Len;
          equal := True;
        end
        else if VType = varArray + varByte then begin
          Len := TVarData(Value.Value).VArray.Bounds[0].ElementCount;
          if Len > FLengths[FieldNo] then
            Len :=  FLengths[FieldNo];
          PBuf := TVarData(Value.Value).VArray.Data;
        end
        else
          raise Exception.CreateFmt('Unknown VType %d', [VType]);
      end;
    else
      raise Exception.CreateFmt('Unknown ValueType %d', [integer(Value.ValueType)]);
    end;
    if not equal and (Len > 0) then
      Move(PBuf^, FCurrentRecordBuffer[FOffsets[FieldNo]], Len);
    if WriteLength and (Len < FLengths[FieldNo]) then
      FCurrentRecordBuffer[FOffsets[FieldNo] + FLengths[FieldNo] - 1] := Len;
  end
  else begin
    if not GetFieldNull(FieldNo) then begin
      v := GetFieldValue(FieldNo);
      Blob := TVarData(v).VPointer;
      Len := Blob.Size;
      Result := GetVirtualTypeSize(Value^);
      equal := Len = Result;
    {$IFNDEF VER9P}
      SetLength(Buf, 0);
      SetLength(Buf1, 0);
    {$ENDIF}
      if equal and (Len > 0) then begin
        Buf := Blob.AsBytes;
        Buf1 := GetVirtualTypeAsBytes(Value^);
        equal := CompareMem(@Buf[0], @Buf1[0], Length(Buf));
      end;
    end
    else begin
      equal := VarIsNull(Value^.Value) or VarIsEmpty(Value^.Value);
      if not equal then
        Result := GetVirtualTypeSize(Value^)
      else
        Result := 0;
    end;

    if not equal then begin
      MemoIdx := FMemo.Write(Value^, Result, IsBinary);
      WriteMemoIndex(FieldNo, MemoIdx, Result);
    end;
  end;
end;

function TDBFDBase.ContainsChars(FieldNo: integer; ExceptOf: byte; HasLengthMarker: boolean): boolean;
var
  PtrStart, PtrEnd: IntPtr;
begin
  Result := True;
  PtrStart := PtrOffset(@FCurrentRecordBuffer[0], FOffsets[FieldNo]);
  PtrEnd := PtrOffset(PtrStart, FLengths[FieldNo]);
  while NativeInt({$IFNDEF FPC}PtrStart{$ELSE}(@PtrStart)^{$ENDIF}) <= NativeInt({$IFNDEF FPC}PtrEnd{$ELSE}(@PtrEnd)^{$ENDIF}) do begin
    if (HasLengthMarker and (Byte(PtrStart^) = 0)) or (Byte(PtrStart^) <> ExceptOf) then begin
      Result := False;
      Exit;
    end;
    if (HasLengthMarker and (Byte(PtrEnd^) = 0)) or (Byte(PtrEnd^) <> ExceptOf) then begin
      Result := False;
      Exit;
    end;
    PtrStart := PtrOffset(PtrStart, 1);
    PtrEnd := PtrOffset(PtrEnd, -1);
  end;
end;

procedure TDBFDBase.InternalOpenIndexFile;
begin
  // do nothing
end;

function TDBFDBase.GetNullableFieldCount: integer;
begin
  Result := 0;
end;

function TDBFDBase.GetNullFlagsLength(NullableFieldCount: integer): integer;
begin
  Result := 0;
end;

procedure TDBFDBase.CreateTable(const Database, TableName: string; const Members: TTableMembers);
var
  TblName: string;
  indexFields: TIndexFieldArray;
  i, j, written, len, offset: integer;
  year, month, day: Word;
  fieldPtr: PDBFField;
  DriverName: AnsiString;
  buf: TBytes;
  a: AnsiChar;
  nullFlagsColumnInfo: TColumnInfo;
begin
  {CREATE TABLE | DBF TableName1 [NAME LongTableName] [FREE]
    [CODEPAGE = nCodePage]
    ( FieldName1 FieldType [( nFieldWidth [, nPrecision] )] [NULL | NOT NULL]
    [CHECK lExpression1 [ERROR cMessageText1]]
    [AUTOINC [NEXTVALUE NextValue [STEP StepValue]]] [DEFAULT eExpression1]
    [PRIMARY KEY | UNIQUE [COLLATE cCollateSequence]]
    [REFERENCES TableName2 [TAG TagName1]] [NOCPTRANS]
    [, FieldName2 ... ]
    [, PRIMARY KEY eExpression2 TAG TagName2 |, UNIQUE eExpression3 TAG TagName3
    [COLLATE cCollateSequence]]
    [, FOREIGN KEY eExpression4 TAG TagName4 [NODUP]
    [COLLATE cCollateSequence]
    REFERENCES TableName3 [TAG TagName5]] [, CHECK lExpression2 [ERROR cMessageText2]] )
    | FROM ARRAY ArrayName}

  // fill header
  FillChar(FHeader, FHeaderSize, 0);
  case FDBFFormat of
    dfdBaseIV, dfdBaseV:
      FHeader.DB3.version := 3 or DBF_DBASE4_DBT;
    dfdBaseVII:
      FHeader.DB3.version := 4;
    dfFoxPro2:
      FHeader.DB3.version := $75; // $F5 ? $80 is memo?
    dfVisualFoxPro:
      FHeader.DB3.version := $30;
    dfHiPerSix:
      FHeader.DB3.version := $65; // $E5 ? $80 is memo?
    dfClipper:
      FHeader.DB3.version := $0B;
  else
    FHeader.DB3.version := $3; // dfdBaseIII
  end;
  // todo if indexes etc update version
  DecodeDate(Now, year, month, day);
  if FDBFFormat = dfVisualFoxPro then
    FHeader.DB3.year := year - 2000
  else
    FHeader.DB3.year := year - 1900;
  FHeader.DB3.month := month;
  FHeader.DB3.day := day;
  FHeader.DB3.numRecs := 0;
  FHeader.DB3.dataOffset := $300; // todo after field's declarations
  FHeader.DB3.flags := 0;
  FHeader.DB3.langDrv := 0;
  FillChar(FHeader.langDrvName[0], Length(FHeader.langDrvName), 0);
  if FHeaderFormat = dfdBaseVII then begin
    DriverName := AnsiString(GetLanguageDriverName(FCodePage, FDBFFormat));
    Move(PAnsiChar(DriverName)^, FHeader.langDrvName[0], LengthA(DriverName));
  end
  else
    FHeader.DB3.langDrv := LanguageDriverID[FCodePage];

  TblName := TableName;
  case FIdentifierCase of
    icLower: TblName := LowerCase(TblName);
    icUpper: TblName := UpperCase(TblName);
  end;

  FFileName := IncludeTrailingPathDelimiter(Database) + TblName;
  if ExtractFileExt(FFileName) = '' then
    FFileName := FFileName + DBF_EXT;

  FFieldCount := Length(Members.Columns);
  FConstraints := nil;
  SetLength(FOffsets, FFieldCount);
  SetLength(FLengths, FFieldCount);
  SetLength(FAlignments, FFieldCount);
  SetLength(FFieldNoToFieldMaskIndex, FFieldCount);
  FillChar(FFieldNoToFieldMaskIndex[0], Length(FFieldNoToFieldMaskIndex), 0);
  // variant size fields has extra bit
  SetLength(FFieldNoToFieldHasLenIndex, FFieldCount);
  FillChar(FFieldNoToFieldHasLenIndex[0], Length(FFieldNoToFieldHasLenIndex), 0);
  FKeyFields := nil;

  // field defs
  Assert(FFields = nil);
  if FHeaderFormat = dfdBaseVII then
    FFieldSize := $30
  else
    FFieldSize := $20;
  SetLength(FFields, FFieldCount * FFieldSize);
  FHasDbtMemoFiles := False;
  FHeader.DB3.recLen := 1;
  indexFields := nil;
  SetLength(buf, FFieldSize * FFieldCount + 1); // plus 0D; properties(todo), if props + 1A at the end of props, plus 1A as data

  SetLength(FKeyFields, FFieldCount);
  for i := 0 to Length(Members.Constraints) - 1 do
    for j := 0 to Length(Members.Constraints[i].ColumnInfo) - 1 do begin
      fieldPtr := GetField(Members.Constraints[i].ColumnInfo[j].ColumnIndex);
      SetFieldConstraint(fieldPtr, @Members.Constraints[i]);
      if (Members.Constraints[i].ConstraintType = ctPrimaryKey)
        and (FDBFFormat in [dfdBaseIV, dfdBaseV, dfdBaseVII, dfFoxPro2, dfVisualFoxPro]) then begin
        FHeader.DB3.flags := FHeader.DB3.flags or DBF_FLAG_HAS_MDX;
        FKeyFields[Members.Constraints[i].ColumnInfo[j].ColumnIndex] := True;
        AddFieldToIndexFields(indexFields, Members.Constraints[i].ColumnInfo[j].ColumnIndex, Members.Constraints[i].Name);
      end;
    end;

  offset := 1; // 0 used for deleted sign
  for i := 0 to FFieldCount - 1 do begin
    FOffsets[i] := FHeader.DB3.recLen;
    FAlignments[i] := naRight;
    fieldPtr := GetField(i);
    //FillChar(fieldPtr^, FFieldSize, 0);
    SetFieldDef(fieldPtr, @Members.Columns[i], offset);

    if FHeaderFormat = dfdBaseVII then begin
      Inc(FHeader.DB3.recLen, fieldPtr.DB7Len);
      FHasDbtMemoFiles := FHasDbtMemoFiles or GetFieldTypeIsMemo(fieldPtr.DB7FType);
    end
    else begin
      Inc(FHeader.DB3.recLen, fieldPtr.DB3Len);
      FHasDbtMemoFiles := FHasDbtMemoFiles or GetFieldTypeIsMemo(fieldPtr.DB3FType);
    end;

    //if FDBFFormat = dfVisualFoxPro then
    //  hasNullFlags := hasNullFlags or ((fieldPtr.FPFlags and DBF_FIELDFLAG_NULLABLE) <> 0) or (fieldPtr.DB3FType in [DBF_TYPE_VARBINARY, DBF_TYPE_VARCHAR]);
    FLengths[i] := fieldPtr.DB3Len;
    Move(fieldPtr^, buf[i * FFieldSize], FFieldSize);
  end;

  // if VFP and has nullable or VarChar/VarByte add _NullFlags field with FPFlags = DBF_FIELDFLAG_SYSTEM
  if (FDBFFormat = dfVisualFoxPro) and (GetNullableFieldCount > 0) then begin
    Inc(FFieldCount);
    SetLength(FOffsets, FFieldCount);
    SetLength(FLengths, FFieldCount);
    SetLength(FAlignments, FFieldCount);
    SetLength(FFields, FFieldCount * FFieldSize);

    FOffsets[FFieldCount - 1] := FHeader.DB3.recLen;
    fieldPtr := GetField(FFieldCount - 1);
    //FillChar(fieldPtr^, FFieldSize, 0);
    //nullFlagsColumnInfo.Name := VFP_NULLFLAGS_NAME;
    nullFlagsColumnInfo.DataType := VFP_NULLFLAGS_TYPE;
    //nullFlagsColumnInfo.NotNull := True;
    SetFieldDef(fieldPtr, @nullFlagsColumnInfo, offset);
    Inc(FHeader.DB3.recLen, fieldPtr.DB3Len);
    FLengths[FFieldCount - 1] := fieldPtr.DB3Len;
    SetLength(buf, FFieldSize * FFieldCount + 1);
    Move(fieldPtr^, buf[(FFieldCount - 1) * FFieldSize], FFieldSize);
    FNullMaskFieldIndex := FFieldCount - 1;
  end;

  buf[Length(buf) - 1] := DBF_SECTION_END;
  if FDBFFormat in [dfFoxPro2, dfVisualFoxPro] then begin
    len := Length(buf) + $100;
    if (len mod $8) > 0 then
      len := (len div $8 + 1) * $8 + 1;
    SetLength(buf, len);
  end;
  FPropDataOffs := FHeaderSize + Length(buf);
  // todo add constraints TDBFDBase7FieldProperties, array of TDBFDBase7StandardDescriptor?
  //buf[Length(buf) - 1] := Byte(DBF_RECORD_EOF);
  // calculate and fix dataOffset
  FHeader.DB3.dataOffset := Word(FHeaderSize + Length(buf));

  SetHasMemo(FHasDbtMemoFiles);
  if FHasDbtMemoFiles then
    FMemo := GetMemoClass.Create(Self, ChangeFileExt(FFileName, GetMemoExt), True);

  case FDBFFormat of
    dfFoxPro2, dfVisualFoxPro:
      if (FHeader.DB3.flags and DBF_FLAG_HAS_MDX) <> 0 then
        FIndexFile := GetIndexClass.Create(Self, ChangeFileExt(FFileName, GetIndexExt), True, indexFields);
    dfdBaseIV, dfdBaseV, dfdBaseVII:
      if (FHeader.DB3.flags and DBF_FLAG_HAS_MDX) <> 0 then
        FIndexFile := GetIndexClass.Create(Self, ChangeFileExt(FFileName, GetIndexExt), True, indexFields);
  end;

  Assert(FStream = nil);
  FStream := TFileStream.Create(FFileName, fmCreate);
  WriteHeader;
  written := FStream.Write(buf[0], Length(buf));
  if written <> Length(buf) then
    raise Exception.CreateFmt('Written %d bytes instead of %d', [written, Length(buf)]);
  a := DBF_RECORD_EOF;
  FStream.Write(a, SizeOf(Byte));
end;

function TDBFDBase.GetFieldValue(FieldNo: integer): variant;
var
  fType: AnsiChar;
  MemoIdx: integer;
  b: Byte;
  str: string;
  y, m, d: Word;
begin
  Result := Unassigned;

  fType := GetFieldType(FieldNo);

  // if not Nullable then Result := SQLite defaults, same for all unassigned
  case fType of
    DBF_TYPE_CHAR:
      Result := GetFieldAsString(FieldNo);
    DBF_TYPE_NUMERIC, DBF_TYPE_FLOAT: begin
      str := Trim(Encoding.Default.GetString(FCurrentRecordBuffer, FOffsets[FieldNo], FLengths[FieldNo]));
      if Trim(str) <> '' then
        Result := GetFieldAsFloat(FieldNo)
      else if IsNullableFieldType(fType, GetFieldFlags(FieldNo)) then
        Result := Null
      else
        Result := 0;
    end;
    DBF_TYPE_DATE: begin // string YYYYMMDD like 20021201
      str := Trim(Encoding.Default.GetString(FCurrentRecordBuffer, FOffsets[FieldNo], 8));
      if Trim(str) <> '' then begin
        if (TDBFDataConverter.InternalStrToInt16(Copy(str, 1, 4), @y, False) = csSuccess) and
           (TDBFDataConverter.InternalStrToInt16(Copy(str, 5, 2), @m, False) = csSuccess) and
           (TDBFDataConverter.InternalStrToInt16(Copy(str, 7, 2), @d, False) = csSuccess) and
           (y > 0) and ((m > 0) and (m <= 12)) and ((d > 0) and (d <= 31))
        then
          Result := EncodeDate(y, m, d)
        else if FIgnoreDataErrors then
          Result := EncodeDate(1899, 12, 30)
        else
          RaiseDataError(FieldNo);
      end
      else if IsNullableFieldType(fType, GetFieldFlags(FieldNo)) then
        Result := Null
      else
        Result := EncodeDate(1899, 12, 30);
    end;
    DBF_TYPE_LOGICAL: begin
      b := FCurrentRecordBuffer[FOffsets[FieldNo]];
      if b = Byte(ANSI_SPACE) then begin
        if IsNullableFieldType(fType, GetFieldFlags(FieldNo)) then
          Result := Null
        else
          Result := False;
      end
      else if b = Byte(CHAR_TRUE) then
        Result := True
      else if b = Byte(CHAR_FALSE) then
        Result := False
      else if not FIgnoreDataErrors then
        RaiseDataError(FieldNo);
    end;
    DBF_TYPE_MEMO, DBF_TYPE_BINARY: begin
      str := Trim(Encoding.Default.GetString(FCurrentRecordBuffer, FOffsets[FieldNo], FLengths[FieldNo]));
      if str <> '' then begin
        if TDBFDataConverter.InternalStrToInt32(str, @MemoIdx, False) <> csSuccess then
          if not FIgnoreDataErrors then
            RaiseDataError(FieldNo);
      end
      else
        MemoIdx := 0;

      if MemoIdx > 0 then
        Result := GetMemoValue(FieldNo, MemoIdx, -1, False, fType = DBF_TYPE_BINARY);
    end;
  else
    raise Exception.CreateFmt('Unknown type %s', [fType]);
  end;
end;

function TDBFDBase.GetFieldAsString(FieldNo: integer): string;
begin
  Result := GetFieldAsString(FieldNo, FLengths[FieldNo]);
end;

function TDBFDBase.GetFieldAsString(FieldNo: integer; Len: integer): string;
begin
  if FEncoding = nil then
    Result := TrimRight(Encoding.Default.GetString(FCurrentRecordBuffer, FOffsets[FieldNo], Len))
  else
    Result := TrimRight(FEncoding.GetString(FCurrentRecordBuffer, FOffsets[FieldNo], Len));
end;

function TDBFDBase.GetFieldAsFloat(FieldNo: integer; ForceIgnoreErrors: boolean = False): double;
var
  str: string;
begin
  str := Trim(Encoding.Default.GetString(FCurrentRecordBuffer, FOffsets[FieldNo], FLengths[FieldNo]));
  if (str <> '') and (UpperCase(str) <> 'OUT') then begin
    TDBFDataConverter.ChangeDecimalSeparator(Str);
    if TDBFDataConverter.InternalStrToFloat(str, @Result, False) <> csSuccess then
      if FIgnoreDataErrors or ForceIgnoreErrors then
        Result := 0
      else
        RaiseDataError(FieldNo);
  end
  else
    Result := 0;
end;

procedure TDBFDBase.SetFieldValue(FieldNo: integer; var Value: TVirtualValue; GenerateAutoInc: boolean);
var
  len, prevLen, prec: integer;
  fType: AnsiChar;
  b: Byte;
  d: Double;
  str: string;
  aStr: AnsiString;
begin
  fType := GetFieldType(FieldNo);

{$IFDEF LOG_PACKETS}
  AddToLog(Format('SetFieldValue: %d, %s', [FieldNo, string(Value.Value)]));
{$ENDIF}

  case fType of
    DBF_TYPE_CHAR:
      SetMemoValue(FieldNo, True, False, False, @Value);
    DBF_TYPE_NUMERIC, DBF_TYPE_FLOAT: begin
      FillChar(FCurrentRecordBuffer[FOffsets[FieldNo]], FLengths[FieldNo], ANSI_SPACE);
      if VirtualContainsValue(Value) then begin
        str := Trim(string(Value.Value));
        d := StrToFloat(str);
        // with exponent string is up to 4 digits longer
        // 7 or less for values of type Single, 15 or less for values of type Double, and 18 or less for values of type Extended
        prec := FLengths[FieldNo];
        // it seems fixes abnormal FloatToStrF with precision 20
        if prec > 15 then
          prec := 15;
      {$IFNDEF VER24P}
      {$IFNDEF WIN64}
        len := 0;
      {$ENDIF}
      {$ENDIF}
        prevLen := 0;
        while True do begin
          str := FloatToStrF(d, ffGeneral, prec, GetFieldNumDecimal(FieldNo));
          len := Length(str);
          if len > FLengths[FieldNo] then begin
            if len = prevLen then begin
              // we can't get shorter string, stop trying
              len := FLengths[FieldNo];
              Break;
            end
            else
              prevLen := len;
            Dec(prec);
            if prec < 0 then
              Break;
          end
          else
            Break;
        end;
        if len > 0 then begin
          str := StringReplace(str, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.', []);
          aStr := AnsiString(str);
          if FAlignments[FieldNo] = naRight then
            Move(PAnsiChar(aStr)^, FCurrentRecordBuffer[FOffsets[FieldNo] + FLengths[FieldNo] - len], len)
          else
            Move(PAnsiChar(aStr)^, FCurrentRecordBuffer[FOffsets[FieldNo]], len);
        end;
      end;
    end;
    DBF_TYPE_DATE: begin // string YYYY-MM-DD to string YYYYMMDD like 20021201
      if VirtualContainsValue(Value) then begin
        aStr := AnsiString(StringReplace(string(Value.Value), '-', '', [rfReplaceAll]));
        Move(PAnsiChar(aStr)^, FCurrentRecordBuffer[FOffsets[FieldNo]], 8);
      end
      else
        FillChar(FCurrentRecordBuffer[FOffsets[FieldNo]], 8, ANSI_SPACE);
    end;
    DBF_TYPE_LOGICAL: begin
      if not VirtualContainsValue(Value) then
        b := ANSI_SPACE
      else if boolean(Value.Value) then
        b := Byte(CHAR_TRUE)
      else
        b := Byte(CHAR_FALSE);
      FCurrentRecordBuffer[FOffsets[FieldNo]] := b;
    end;
    DBF_TYPE_MEMO, DBF_TYPE_BINARY: begin
      if not VirtualContainsValue(Value) then
        WriteMemoIndex(FieldNo, 0, 0)
      else
        SetMemoValue(FieldNo, False, fType <> DBF_TYPE_MEMO, False, @Value);
    end;
  else
    raise Exception.CreateFmt('Unknown type %s', [fType]);
  end;
end;

procedure TDBFDBase.EditRecord(const Values: TVirtualValues);
var
  i, idxNo: integer;
  oldValue: TVirtualValue;
  keyBuffer: TBytes;
  Expression: TDBFExpression;
  FieldName: string;
begin
{$IFNDEF VER9P}
  keyBuffer := nil; // anti-warning
{$ENDIF}

  Open(False, False);
  ReadRecord;

  if FInTransaction then begin
    i := Length(FDBFTransactionData);
    SetLength(FDBFTransactionData, Length(FDBFTransactionData) + 1);
    FDBFTransactionData[i].RecNo := RecNo;
    FDBFTransactionData[i].Action := trActionEdit;
    SetLength(FDBFTransactionData[i].Buffer, FHeader.DB3.recLen);
    Move(FCurrentRecordBuffer[0], FDBFTransactionData[i].Buffer[0], FHeader.DB3.recLen);
  end;

  for i := 0 to Length(Values) - 1 do begin
    if IsNullMaskField(i) then
      Continue;

    oldValue.Value := GetFieldValue(i);
    try
      oldValue.ValueType := GetVirtualValueType(oldValue.Value);

      SetFieldValue(i, Values[i], False);
      FieldName := UpperCase(GetFieldName(i));

      case FIndexFileType of
        itMDX, itCDX: if FIndexFile <> nil then begin
          for idxNo := 0 to FIndexFile.GetIndexCount - 1 do begin
            Expression := FIndexFile.GetIndexExpression(IdxNo);
            if Expression.Fields.IndexOf(FieldName) >= 0 then begin
              keyBuffer := GetIndexKeyData(oldValue, i, idxNo);
              FIndexFile.Delete(idxNo, FCurrentRecNo + 1, @keyBuffer[0]);
              keyBuffer := GetIndexKeyData(Values[i], i, idxNo);
              FIndexFile.Add(idxNo, FCurrentRecNo + 1, @keyBuffer[0]);
            end;
          end;
        end;
      end;
    finally
      if TVarData(oldValue.Value).VType = varSharedObject then
        TBlob(TVarData(oldValue.Value).VPointer).Free;
    end;
  end;
  WriteRecord;
end;

procedure TDBFDBase.InsertRecord(const Values: TVirtualValues);
var
  i, idxNo: integer;
  keyBuffer: TBytes;
  Expression: TDBFExpression;
  FieldName: string;
begin
{$IFNDEF VER9P}
  SetLength(keyBuffer, 0); // anti-warning
{$ENDIF}

  Open(False, False);
  RecNo := GetRecordCount;

  if FInTransaction then begin
    i := Length(FDBFTransactionData);
    SetLength(FDBFTransactionData, Length(FDBFTransactionData) + 1);
    FDBFTransactionData[i].RecNo := RecNo;
    FDBFTransactionData[i].Action := trActionInsert;
  end;

  FCurrentRecordBuffer[0] := DBF_RECORD_BEGIN;
  for i := 0 to Length(Values) - 1 do begin
    SetFieldValue(i, Values[i], True);
    FieldName := UpperCase(GetFieldName(i));

    case FIndexFileType of
      itMDX, itCDX: begin
        for idxNo := 0 to FIndexFile.GetIndexCount - 1 do begin
          Expression := FIndexFile.GetIndexExpression(IdxNo);
          if Expression.Fields.IndexOf(FieldName) >= 0 then begin
            keyBuffer := GetIndexKeyData(Values[i], i, idxNo);
            FIndexFile.Add(idxNo, FCurrentRecNo + 1, @keyBuffer[0]);
          end;
        end;
      end;
    end;
  end;

  WriteRecord;
  FStream.Write(DBF_RECORD_EOF, 1);
  Inc(FHeader.DB3.numRecs);
  WriteHeader;
end;

procedure TDBFDBase.DropTable;
begin
  DropMemoFile;
  DropIndexFile;
  DoDeleteFile(ChangeFileExt(FFileName, DBF_EXT));
end;

procedure TDBFDBase.DropMemoFile;
begin
  if FStream <> nil then begin
    // set memo flag to 0
    FHeader.DB3.version := FHeader.DB3.version and not DBF_HAS_DBT;
    if FDBFFormat = dfVisualFoxPro then
      FHeader.DB3.flags := FHeader.DB3.flags and not DBF_FLAG_HAS_MEMO;
    WriteHeader;
  end;
  FreeAndNil(FMemo);
  DoDeleteFile(ChangeFileExt(FFileName, GetMemoExt));
end;

procedure TDBFDBase.DropIndexFile;
begin
  if FStream <> nil then begin
    // set index flag to 0
    FHeader.DB3.flags := FHeader.DB3.flags and not(DBF_FLAG_HAS_MDX);
    WriteHeader;
  end;
  FreeAndNil(FIndexFile);
  DoDeleteFile(ChangeFileExt(FFileName, GetIndexExt));
end;

procedure TDBFDBase.PackTable(PackMemo, PackDbf: boolean);
var
  buf: TBytes;
  recCount, readed, written: integer;
  a: AnsiChar;
  ms: TMemoryStream;
  m: TDBFMemoFile;
  i, fCount, MemoIdx: Integer;
  tempFName, memoFName: string;
  fType: AnsiChar;
  v: TVirtualValue;
  Blob: TBlob;
begin
  // Permanently removes all records marked for deletion in the current table and reduces the size of the table memo (.fpt) file associated with the table
  if FStream = nil then
    if not Open then
      raise Exception.CreateFmt('Cannot open %s', [FFileName]);

  if not PackMemo and not PackDbf then begin
    PackMemo := True;
    PackDbf := True;
  end;

  // copy header to TMemoryStream
  if PackDbf then begin
    ms := TMemoryStream.Create;
    try
      SetLength(buf, FHeader.DB3.dataOffset);
      FStream.Seek(LongInt(0), soFromBeginning);
      readed := FStream.Read(buf[0], Length(buf));
      if readed < Length(buf) then
        raise Exception.CreateFmt('error: read %d', [readed]);
      ms.Write(buf[0], Length(buf));

      recCount := 0;
      First;
      while True do
        try
          if EOF or not ReadRecord then
            Break;

          if RecordDeleted then
            Continue;

          // copy to TMemoryStream
          ms.Write(FCurrentRecordBuffer[0], FHeader.DB3.recLen);
          Inc(recCount);
        finally
          Next;
        end;

      a := DBF_RECORD_EOF;
      ms.Write(a, SizeOf(Byte));
      // fix record count
      FHeader.DB3.numRecs := recCount;
      ms.Seek(LongInt(0), soFromBeginning);
      written := ms.Write(FHeader, FHeaderSize);
      if written <> FHeaderSize then
        raise Exception.CreateFmt('Written %d bytes instead of %d', [written, FHeaderSize]);
      // save TMemoryStream
      Close;
      ms.SaveToFile(FFileName);
    finally
      ms.Free;
    end;

    // reindex
    if not Open then
      raise Exception.CreateFmt('Cannot open %s', [FFileName]);
    ReindexTable;
  end;

  if PackMemo and FHasDbtMemoFiles then begin
    // Open to reread in case we did PackDBF before
    if FStream = nil then
      if not Open then
        raise Exception.CreateFmt('Cannot open %s', [FFileName]);

    fCount := GetFieldCount;
    tempFName := ChangeFileExt(FFileName, '.TMP');
    memoFName := FMemo.FileName;
    case FDBFFormat of
      dfFoxPro2, dfVisualFoxPro:
        m := TDBFFoxProMemo.Create(Self, tempFName, True);
      dfdBaseIV, dfdBaseVII:
        m := TDBFIVMemo.Create(Self, tempFName, True);
      dfHiPerSix:
        m := TDBFHiPerSixMemo.Create(Self, tempFName, True);
    else
      m := TDBFMemoFile.Create(Self, tempFName, True);
    end;
    try
      First;
      while True do
        try
          if EOF or not ReadRecord then
            Break;

          if RecordDeleted then
            Continue;

          for i := 0 to fCount - 1 do begin
            fType := GetFieldType(i);
            if GetFieldTypeIsMemo(fType) and not GetFieldNull(i) then begin
              // read from FMemo
              v.Value := GetFieldValue(i);
              v.ValueType := vrBlob;
              Blob := TVarData(v.Value).VPointer;
              MemoIdx := m.Write(v, Blob.Size, True);
              WriteMemoIndex(i, MemoIdx, Blob.Size);
              WriteRecord;
              Blob.Free;
            end;
          end;
        finally
          Next;
        end;

      Close;
    finally
      m.Free;
    end;

    // delete current memo, replace
    if not SysUtils.DeleteFile(memoFName) then
    {$IFNDEF UNIX}
      raise Exception.CreateFmt('Cannot delete file, error "%s"', [SysErrorMessage(GetLastError)]);
    {$ELSE}
      raise Exception.Create('Cannot delete file');
    {$ENDIF}
    if not SysUtils.RenameFile(tempFName, memoFName) then
    {$IFNDEF UNIX}
      raise Exception.CreateFmt('Cannot rename file, error "%s"', [SysErrorMessage(GetLastError)]);
    {$ELSE}
      raise Exception.Create('Cannot delete file');
    {$ENDIF}
  end;
end;

procedure TDBFDBase.AlterTableAddField(const Members: TTableMembers);
var
  indexFields: TIndexFieldArray;
  i, j, offset, len, readed, written: integer;
  fieldPtr: PDBFField;
  buf, temp: TBytes;
  ms: TMemoryStream;
  a: AnsiChar;
  pHdr: PDBFDBase7Header;
  isMemo: boolean;
begin
  // recreate structure to new table with AlterOperation
  // copy fields to new table from old
  // todo if VFP and _NullFlags exists then add before _NullFlags?
  Assert(Length(Members.Columns) = 1, Format('Error: Members.Columns length %d isntead of 1', [Length(Members.Columns)]));

  ms := TMemoryStream.Create;
  try
    Open;
    len := FHeaderSize + FFieldSize * FFieldCount;
    SetLength(buf, len + FFieldSize + 1); // plus 0D; properties(todo), if props + 1A at the end of props, plus 1A as data
    pHdr := @buf[0];
    FStream.Seek(LongInt(0), soFromBeginning);
    readed := FStream.Read(buf[0], len);
    if readed < len then
      raise Exception.CreateFmt('error: read %d', [readed]);

    fieldPtr := @buf[len];

    indexFields := nil;
    for i := 0 to Length(Members.Constraints) - 1 do
      for j := 0 to Length(Members.Constraints[i].ColumnInfo) - 1 do begin
        //Assert(Members.Constraints[i].ColumnInfo[j].ColumnIndex = 0, Format('wrong column index %d', [Members.Constraints[i].ColumnInfo[j].ColumnIndex]));
        SetFieldConstraint(fieldPtr, @Members.Constraints[i]);
        AddFieldToIndexFields(indexFields, Members.Constraints[i].ColumnInfo[j].ColumnIndex, Members.Constraints[i].Name);
      end;

    offset := FHeader.DB3.recLen;
    SetFieldDef(fieldPtr, @Members.Columns[0], offset);

    if FHeaderFormat = dfdBaseVII then begin
      Inc(pHdr.DB3.recLen, fieldPtr.DB7Len);
      SetLength(temp, fieldPtr.DB7Len);
      isMemo := GetFieldTypeIsMemo(fieldPtr.DB7FType);
      case fieldPtr.DB7FType of
        DBF_TYPE_CHAR,
        DBF_TYPE_NUMERIC,
        DBF_TYPE_FLOAT,
        DBF_TYPE_DATE,
        DBF_TYPE_LOGICAL,
        DBF_TYPE_MEMO:
          FillChar(temp[0], Length(temp), $20);
      end;
    end
    else begin
      Inc(pHdr.DB3.recLen, fieldPtr.DB3Len);
      SetLength(temp, fieldPtr.DB3Len);
      isMemo := GetFieldTypeIsMemo(fieldPtr.DB3FType);
      case fieldPtr.DB3FType of
        DBF_TYPE_CHAR,
        DBF_TYPE_NUMERIC,
        DBF_TYPE_FLOAT,
        DBF_TYPE_DATE,
        DBF_TYPE_LOGICAL,
        DBF_TYPE_VARCHAR:
          FillChar(temp[0], Length(temp), $20);
        DBF_TYPE_MEMO,
        DBF_TYPE_GENERAL,
        DBF_TYPE_BLOB:
          if FDBFFormat in [dfFoxPro2, dfVisualFoxPro] then begin
            Assert(fieldPtr.DB3Len = 4, 'Memo field size in VFP must be 4');
            PInteger(@temp[0])^ := 0; // -1;
          end
          else
          if FDBFFormat = dfHiPerSix then begin
            Assert(fieldPtr.DB3Len = SizeOf(TSMTMemoLink), 'Memo field size in HiPerSix must be 10');
            PSMTMemoLink(@temp[0]).Signature := $2021
          end
          else
            FillChar(temp[0], Length(temp), $20);
      end;
    end;

    buf[Length(buf) - 1] := DBF_SECTION_END;
    if FDBFFormat in [dfFoxPro2, dfVisualFoxPro] then begin
      len := Length(buf) + $100;
      if (len mod $8) > 0 then
        len := (len div $8 + 1) * $8 + 1;
      SetLength(buf, len);
      pHdr := @buf[0]; // in case resized and ptr changed
    end;
    // todo add constraints TDBFDBase7FieldProperties, array of TDBFDBase7StandardDescriptor?
    //buf[Length(buf) - 1] := Byte(DBF_RECORD_EOF);
    // calculate and fix dataOffset
    pHdr.DB3.dataOffset := Word(Length(buf));

    ms.Seek(LongInt(0), soFromBeginning);
    written := ms.Write(buf[0], Length(buf));
    if written <> Length(buf) then
      raise Exception.CreateFmt('Written %d bytes instead of %d', [written, Length(buf)]);

    First;
    while True do
      try
        if EOF or not ReadRecord then
          Break;

        // copy to TMemoryStream
        ms.Write(FCurrentRecordBuffer[0], FHeader.DB3.recLen);
        ms.Write(temp[0], Length(temp));
      finally
        Next;
      end;

    a := DBF_RECORD_EOF;
    ms.Write(a, SizeOf(Byte));

    // create Memo
    if isMemo and (FMemo = nil) then begin
      pHdr.DB3.version := pHdr.DB3.version or DBF_HAS_DBT;
      FMemo := GetMemoClass.Create(Self, ChangeFileExt(FFileName, GetMemoExt), True);
    end;

    // if index added create index file if absent, add index tag
    if (indexFields <> nil) and (FIndexFile = nil) then
      case FDBFFormat of
        dfFoxPro2, dfVisualFoxPro: begin
          pHdr.DB3.flags := pHdr.DB3.flags or DBF_FLAG_HAS_MDX;
          FIndexFile := GetIndexClass.Create(Self, ChangeFileExt(FFileName, GetIndexExt), True, indexFields);
        end;
        dfdBaseIV, dfdBaseV, dfdBaseVII: begin
          pHdr.DB3.flags := pHdr.DB3.flags or DBF_FLAG_HAS_MDX;
          FIndexFile := GetIndexClass.Create(Self, ChangeFileExt(FFileName, GetIndexExt), True, indexFields);
        end;
      end;

    // save TMemoryStream
    Close;
    // write header
    ms.Seek(LongInt(0), soFromBeginning);
    written := ms.Write(buf[0], FHeaderSize);
    if written <> FHeaderSize then
      raise Exception.CreateFmt('Written %d bytes instead of %d', [written, FHeaderSize]);
    ms.SaveToFile(FFileName);
  finally
    ms.Free;
  end;

  Prepare;
  if not Open then
    raise Exception.CreateFmt('Cannot open %s', [FFileName]);

  if indexFields <> nil then begin
    for i := 0 to Length(indexFields) - 1 do
      FIndexFile.AddIndex(indexFields[i].FieldNo, indexFields[i].IndexName, GetFieldName(indexFields[i].FieldNo));
    // close and open index again to reread indexes, they will be sorted for FoxPro
    FIndexFile.Close;
    FIndexFile.Open;
    ReindexTable;
  end;
end;

procedure TDBFDBase.AlterTableDropField(const Members: TTableMembers);
var
  i, j, fieldNo, fieldLen, offset, recDataOffset, len, readed, written: integer;
  fieldPtr: PDBFField;
  buf: TBytes;
  ms: TMemoryStream;
  a: AnsiChar;
  pHdr: PDBFDBase7Header;
  hasMemo: boolean;
  Expression: TDBFExpression;
  FieldName: string;
begin
  Assert(Length(Members.Columns) = 1, Format('Error: Members.Columns length %d isntead of 1', [Length(Members.Columns)]));

  ms := TMemoryStream.Create;
  try
    Open;
    // recreate header
    len := FHeaderSize + FFieldSize * (FFieldCount - 1);
    SetLength(buf, len + 1); // plus 0D; properties(todo), if props + 1A at the end of props, plus 1A as data
    pHdr := @buf[0];
    FStream.Seek(LongInt(0), soFromBeginning);
    readed := FStream.Read(buf[0], FHeaderSize);
    if readed < FHeaderSize then
      raise Exception.CreateFmt('error: read %d', [readed]);

    fieldNo := GetFieldNo(Members.Columns[0].Name);
    if fieldNo < 0 then
      raise Exception.CreateFmt('Field %s not found', [Members.Columns[0].Name]);

    FieldName := UpperCase(GetFieldName(fieldNo));

    fieldPtr := GetField(fieldNo);
    if FHeaderFormat = dfdBaseVII then begin
      fieldLen := fieldPtr.DB7Len;
      Dec(pHdr.DB3.recLen, fieldLen);
    end
    else begin
      fieldLen := fieldPtr.DB3Len;
      Dec(pHdr.DB3.recLen, fieldLen);
    end;

    hasMemo := False;
    offset := 1; // offset 1 based (0 deleted sign)
    recDataOffset := 0;
    j := 0;
    for i := 0 to FFieldCount - 1 do
      if i <> fieldNo then begin
        fieldPtr := GetField(i);
        if FHeaderFormat = dfdBaseVII then
          hasMemo := hasMemo or GetFieldTypeIsMemo(fieldPtr.DB7FType)
        else
          hasMemo := hasMemo or GetFieldTypeIsMemo(fieldPtr.DB3FType);

        Move(fieldPtr^, buf[FHeaderSize + j * FFieldSize], FFieldSize);
        if FDBFFormat = dfVisualFoxPro then begin
          fieldPtr := @buf[FHeaderSize + j * FFieldSize];
          fieldPtr.DB3Offset := offset;
        end;
        if FHeaderFormat = dfdBaseVII then
          Inc(offset, FieldPtr.DB7Len)
        else
          Inc(offset, FieldPtr.DB3Len);
        Inc(j);
      end
      else
        recDataOffset := offset; // - 1

    buf[Length(buf) - 1] := DBF_SECTION_END;
    if FDBFFormat in [dfFoxPro2, dfVisualFoxPro] then begin
      len := Length(buf) + $100;
      if (len mod $8) > 0 then
        len := (len div $8 + 1) * $8 + 1;
      SetLength(buf, len);
      pHdr := @buf[0]; // in case resized and ptr changed
    end;
    // todo add constraints TDBFDBase7FieldProperties, array of TDBFDBase7StandardDescriptor?
    //buf[Length(buf) - 1] := Byte(DBF_RECORD_EOF);
    // calculate and fix dataOffset
    pHdr.DB3.dataOffset := Word(Length(buf));

    ms.Seek(LongInt(0), soFromBeginning);
    written := ms.Write(buf[0], Length(buf));
    if written <> Length(buf) then
      raise Exception.CreateFmt('Written %d bytes instead of %d', [written, Length(buf)]);

    // copy data
    First;
    while True do
      try
        if EOF or not ReadRecord then
          Break;

        // copy to TMemoryStream
        ms.Write(FCurrentRecordBuffer[0], recDataOffset);
        if (recDataOffset + fieldLen) < High(FCurrentRecordBuffer) then
          ms.Write(FCurrentRecordBuffer[recDataOffset + fieldLen], FHeader.DB3.recLen - recDataOffset - fieldLen);
      finally
        Next;
      end;

    a := DBF_RECORD_EOF;
    ms.Write(a, SizeOf(Byte));

    // if no more memo fields then drop memo file
    if not hasMemo and (FMemo <> nil) then
      DropMemoFile;

    // if indexed then delete index
    if FIndexFile <> nil then begin
      for i := FIndexFile.GetIndexCount - 1 downto 0 do begin
        Expression := FIndexFile.GetIndexExpression(i);
        if Expression.Fields.IndexOf(FieldName) >= 0 then
          FIndexFile.DeleteIndex(FIndexFile.GetIndexName(i));
      end;

      // if no more indexes then drop index file
      if FIndexFile.GetIndexCount <= 0 then
        DropIndexFile;
    end;

    // todo: VFP if no more nullable then remove nullable field

    // save TMemoryStream
    Close;
    ms.SaveToFile(FFileName);
  finally
    ms.Free;
  end;

  if not Open then
    raise Exception.CreateFmt('Cannot open %s', [FFileName]);
end;

procedure TDBFDBase.AlterTableAlterField(const Members: TTableMembers);
var
  indexFields: TIndexFieldArray;
  i, j, fieldNo, oldLen, fieldLen, offset, recDataOffset, len, readed, written: integer;
  fieldPtr: PDBFField;
  buf, temp: TBytes;
  ms, ms2: TMemoryStream;
  a: AnsiChar;
  pHdr: PDBFDBase7Header;
  isMemo: boolean;
  dbf: TDBFDBase;
  v: TVirtualValue;
{$IFNDEF UNIX}
  LastError: Cardinal;
{$ENDIF}
const
  TEMP_EXT = '.TMP';
begin
  // recreate structure to new table with AlterOperation
  // copy fields to new table from old
  Assert(Length(Members.Columns) = 1, Format('Error: Members.Columns length %d isntead of 1', [Length(Members.Columns)]));

{$IFNDEF VER25P}
  fieldNo := -1;
{$ENDIF}  
  ms := TMemoryStream.Create;
  try
    Open;
    len := FHeaderSize + FFieldSize * FFieldCount;
    SetLength(buf, len + 1); // plus 0D; properties(todo), if props + 1A at the end of props, plus 1A as data
    pHdr := @buf[0];
    FStream.Seek(LongInt(0), soFromBeginning);
    readed := FStream.Read(buf[0], len);
    if readed < len then
      raise Exception.CreateFmt('error: read %d', [readed]);

    fieldNo := GetFieldNo(Members.Columns[0].Name);
    if fieldNo < 0 then
      raise Exception.CreateFmt('Field %s not found', [Members.Columns[0].Name]);

    fieldPtr := GetField(fieldNo);

    if FHeaderFormat = dfdBaseVII then
      oldLen := fieldPtr.DB7Len
    else
      oldLen := fieldPtr.DB3Len;

    fieldPtr := @buf[FHeaderSize + fieldNo * FFieldSize];
    SetFieldDef(fieldPtr, @Members.Columns[0], offset);
    indexFields := nil;
    for i := 0 to Length(Members.Constraints) - 1 do
      for j := 0 to Length(Members.Constraints[i].ColumnInfo) - 1 do begin
        //Assert(Members.Constraints[i].ColumnInfo[j].ColumnIndex = 0, Format('wrong column index %d', [Members.Constraints[i].ColumnInfo[j].ColumnIndex]));
        SetFieldConstraint(fieldPtr, @Members.Constraints[i]);
        AddFieldToIndexFields(indexFields, Members.Constraints[i].ColumnInfo[j].ColumnIndex, Members.Constraints[i].Name);
      end;

    if FHeaderFormat = dfdBaseVII then begin
      fieldLen := fieldPtr.DB7Len;
      pHdr.DB3.recLen := pHdr.DB3.recLen + fieldPtr.DB7Len - oldLen;
      isMemo := GetFieldTypeIsMemo(fieldPtr.DB7FType);
    end
    else begin
      fieldLen := fieldPtr.DB3Len;
      pHdr.DB3.recLen := pHdr.DB3.recLen + fieldPtr.DB3Len - oldLen;
      isMemo := GetFieldTypeIsMemo(fieldPtr.DB3FType);
    end;
    SetLength(temp, fieldLen);

    offset := 1; // offset 1 based, 0 for "deleted" sign
    recDataOffset := 0;
    for i := 0 to FFieldCount - 1 do begin
      fieldPtr := GetField(i);

      if i = fieldNo then
        recDataOffset := offset;

      if FDBFFormat = dfVisualFoxPro then begin
        fieldPtr := @buf[FHeaderSize + i * FFieldSize];
        fieldPtr.DB3Offset := offset;
      end;

      if i = fieldNo then
        Inc(offset, fieldLen)
      else
      if FHeaderFormat = dfdBaseVII then
        Inc(offset, FieldPtr.DB7Len)
      else
        Inc(offset, FieldPtr.DB3Len);
    end;

    buf[Length(buf) - 1] := DBF_SECTION_END;
    // todo add constraints TDBFDBase7FieldProperties, array of TDBFDBase7StandardDescriptor?
    //buf[Length(buf) - 1] := Byte(DBF_RECORD_EOF);
    // calculate and fix dataOffset
    pHdr.DB3.dataOffset := Word(Length(buf));

    ms.Seek(LongInt(0), soFromBeginning);
    written := ms.Write(buf[0], Length(buf));
    if written <> Length(buf) then
      raise Exception.CreateFmt('Written %d bytes instead of %d', [written, Length(buf)]);

    // copy data
    First;
    while True do
      try
        if EOF or not ReadRecord then
          Break;

        // copy to TMemoryStream
        ms.Write(FCurrentRecordBuffer[0], recDataOffset);
        // write stub
        ms.Write(temp[0], Length(temp));
        if (recDataOffset + oldLen) < High(FCurrentRecordBuffer) then
          ms.Write(FCurrentRecordBuffer[recDataOffset + oldLen], FHeader.DB3.recLen - recDataOffset - oldLen);
      finally
        Next;
      end;

    a := DBF_RECORD_EOF;
    ms.Write(a, SizeOf(Byte));

    // create Memo
    if FMemo = nil then begin
      if isMemo then begin
        SetHasMemo(isMemo);
        FMemo := GetMemoClass.Create(Self, ChangeFileExt(FFileName, GetMemoExt), True);
      end;
    end
    else begin
      // close files to copy memo and indexes
      FreeAndNil(FMemo);
      ms2 := TMemoryStream.Create;
      try
        ms2.LoadFromFile(ChangeFileExt(FFileName, GetMemoExt));
        ms2.SaveToFile(ChangeFileExt(FFileName, TEMP_EXT + GetMemoExt));
      finally
        ms2.Free;
      end;
    end;

    // if index added create index file if absent, add index tag
    // flag in header?
    if FIndexFile = nil then begin
      if indexFields <> nil then
        case FDBFFormat of
          dfFoxPro2, dfVisualFoxPro: begin
            pHdr.DB3.flags := pHdr.DB3.flags or DBF_FLAG_HAS_MDX;
            FIndexFile := GetIndexClass.Create(Self, ChangeFileExt(FFileName, TEMP_EXT + GetIndexExt), True, indexFields);
          end;
          dfdBaseIV, dfdBaseV, dfdBaseVII: begin
            pHdr.DB3.flags := pHdr.DB3.flags or DBF_FLAG_HAS_MDX;
            FIndexFile := GetIndexClass.Create(Self, ChangeFileExt(FFileName, TEMP_EXT + GetIndexExt), True, indexFields);
          end;
        end;
    end
    else begin
      // close files to copy memo and indexes
      FreeAndNil(FIndexFile);
      ms2 := TMemoryStream.Create;
      try
        case FDBFFormat of
          dfFoxPro2, dfVisualFoxPro: begin
            ms2.LoadFromFile(ChangeFileExt(FFileName, GetIndexExt));
            ms2.SaveToFile(ChangeFileExt(FFileName, TEMP_EXT + GetIndexExt));
          end;
          dfdBaseIV, dfdBaseV, dfdBaseVII: begin
            ms2.LoadFromFile(ChangeFileExt(FFileName, GetIndexExt));
            ms2.SaveToFile(ChangeFileExt(FFileName, TEMP_EXT + GetIndexExt));
          end;
        end;
      finally
        ms2.Free;
      end;
    end;

    // save TMemoryStream as temp
    ms.SaveToFile(ChangeFileExt(FFileName, TEMP_EXT + DBF_EXT));
  finally
    ms.Free;
    Close;
  end;

  dbf := GetFileClass(FDBFFormat).Create(ExtractFileDir(FFileName), ChangeFileExt(ExtractFileName(FFileName), TEMP_EXT + DBF_EXT), FHeaderFormat);
  dbf.CodePage := FCodePage;
  dbf.ConnectMode := FConnectMode;
  dbf.IndexOnReading := FIndexOnReading;
  dbf.IgnoreDataErrors := FIgnoreDataErrors;
  dbf.IgnoreMetadataErrors := FIgnoreMetadataErrors;
  dbf.IdentifierCase := FIdentifierCase;

  try
    Open;
    First;
    dbf.Prepare;
    dbf.First;
    while True do
      try
        if EOF or not ReadRecord then
          Break;
        if dbf.EOF or not dbf.ReadRecord then
          Break;

        v.Value := GetFieldValue(fieldNo);
        v.ValueType := GetVirtualValueType(v.Value);
        dbf.SetFieldValue(fieldNo, v, True);
        dbf.WriteRecord;
      finally
        Next;
        dbf.Next;
      end;
  finally
    dbf.Free;
    Close;
  end;

  // replace DBF
  DoDeleteFile(ChangeFileExt(FFileName, DBF_EXT));
  DoDeleteFile(ChangeFileExt(FFileName, GetMemoExt));
  DoDeleteFile(ChangeFileExt(FFileName, GetIndexExt));

  if not SysUtils.RenameFile(ChangeFileExt(FFileName, TEMP_EXT + DBF_EXT), FFileName) then begin
  {$IFNDEF UNIX}
    LastError := GetLastError;
    if LastError <> 0 then
      raise Exception.CreateFmt('Cannot delete file, error "%s"', [SysErrorMessage(LastError)]);
  {$ELSE}
    raise Exception.Create('Cannot delete file');
  {$ENDIF}
  end;
  if FileExists(ChangeFileExt(FFileName, TEMP_EXT + GetMemoExt)) then
    if not SysUtils.RenameFile(ChangeFileExt(FFileName, TEMP_EXT + GetMemoExt), ChangeFileExt(FFileName, GetMemoExt)) then begin
    {$IFNDEF UNIX}
      LastError := GetLastError;
      if LastError <> 0 then
        raise Exception.CreateFmt('Cannot delete file, error "%s"', [SysErrorMessage(LastError)]);
    {$ELSE}
      raise Exception.Create('Cannot delete file');
    {$ENDIF}
  end;
  if FileExists(ChangeFileExt(FFileName, TEMP_EXT + GetIndexExt)) then
    if not SysUtils.RenameFile(ChangeFileExt(FFileName, TEMP_EXT + GetIndexExt), ChangeFileExt(FFileName, GetIndexExt)) then begin
    {$IFNDEF UNIX}
      LastError := GetLastError;
      if LastError <> 0 then
        raise Exception.CreateFmt('Cannot delete file, error "%s"', [SysErrorMessage(LastError)]);
    {$ELSE}
      raise Exception.Create('Cannot delete file');
    {$ENDIF}
    end;

  if not Open then
    raise Exception.CreateFmt('Cannot open %s', [FFileName]);

  if indexFields <> nil then
    ReindexTable;
end;

procedure TDBFDBase.AlterTableCreateIndex(const Members: TTableMembers);
var
  i, j, fieldNo: integer;
  fieldPtr: PDBFField;
  indexFields: TIndexFieldArray;
begin
  fieldNo := GetFieldNo(Members.Columns[0].Name);
  if fieldNo < 0 then
    raise Exception.CreateFmt('Field %s not found', [Members.Columns[0].Name]);

  fieldPtr := GetField(fieldNo);
  indexFields := nil;
  for i := 0 to Length(Members.Constraints) - 1 do
    for j := 0 to Length(Members.Constraints[i].ColumnInfo) - 1 do begin
      //Assert(Members.Constraints[i].ColumnInfo[j].ColumnIndex = 0, Format('wrong column index %d', [Members.Constraints[i].ColumnInfo[j].ColumnIndex]));
      SetFieldConstraint(fieldPtr, @Members.Constraints[i]);
      WriteField(Members.Constraints[i].ColumnInfo[j].ColumnIndex);
      AddFieldToIndexFields(indexFields, Members.Constraints[i].ColumnInfo[j].ColumnIndex, Members.Constraints[i].Name);
    end;

  if indexFields <> nil then begin
    // check index file exists or create
    if FIndexFile = nil then
      case FDBFFormat of
        dfFoxPro2, dfVisualFoxPro: begin
          FHeader.DB3.flags := FHeader.DB3.flags or DBF_FLAG_HAS_MDX;
          FIndexFile := GetIndexClass.Create(Self, ChangeFileExt(FFileName, GetIndexExt), True, indexFields);
          FIndexFileType := itCDX;
          WriteHeader;
        end;
        dfdBaseIV, dfdBaseV, dfdBaseVII: begin
          FHeader.DB3.flags := FHeader.DB3.flags or DBF_FLAG_HAS_MDX;
          FIndexFile := GetIndexClass.Create(Self, ChangeFileExt(FFileName, GetIndexExt), True, indexFields);
          FIndexFileType := itMDX;
          WriteHeader;
        end;
      else
        raise Exception.Create('CREAETE INDEX is not supported');  
      end
    else
      for i := 0 to Length(indexFields) - 1 do
        FIndexFile.AddIndex(indexFields[i].FieldNo, indexFields[i].IndexName, GetFieldName(indexFields[i].FieldNo));
    // close and open index again to reread indexes, they will be sorted for FoxPro
    FIndexFile.Close;
    FIndexFile.Open;
    ReindexTable;
  end;
end;

procedure TDBFDBase.AlterTableDropIndex(const Members: TTableMembers);
var
  i: integer;
begin
  if FIndexFile <> nil then begin
    for i := 0 to Length(Members.Constraints) - 1 do
      FIndexFile.DeleteIndex(Members.Constraints[i].Name);

    // close and open index again to reread indexes, they will be sorted for FoxPro
    FIndexFile.Close;
    FIndexFile.Open;
    // drop index file if 0 indexes left
    if FIndexFile.GetIndexCount <= 0 then
      DropIndexFile;
  end;
end;

procedure TDBFDBase.ZapTable;
var
  a: AnsiChar;
begin
  // Removes all records from a table, leaving just the table structure
  if FStream = nil then
    if not Open then
      raise Exception.CreateFmt('Cannot open %s', [FFileName]);

  FHeader.DB3.numRecs := 0;
  WriteHeader;
  FStream.Size := Int64(FHeader.DB3.dataOffset);
  if FDBFFormat <> dfVisualFoxPro then begin
    a := DBF_RECORD_EOF;
    FStream.Write(a, SizeOf(Byte));
  end;

  if FIndexFile <> nil then
    FIndexFile.ZapIndex;
  if FMemo <> nil then
    FMemo.ZapMemo;
end;

procedure TDBFDBase.RaiseDataError(FieldNo: integer);
const
  INVALID_DATA_ERROR = 'Invalid data for the field "%s".'#13#10'%s';
begin
  raise Exception.CreateFmt(INVALID_DATA_ERROR, [GetFieldName(FieldNo), FFileName]);
end;

procedure TDBFDBase.ReindexTable;
var
  i, fCount, idxNo: integer;
  keyBuffer: TBytes;
  Value: TVirtualValue;
  Expression: TDBFExpression;
  FieldName: string;
begin
{$IFNDEF VER9P}
  keyBuffer := nil; // anti-warning
{$ENDIF}

  // Rebuilds open index files
  if FStream = nil then
    if not Open then
      raise Exception.CreateFmt('Cannot open %s', [FFileName]);

  if (FIndexFile <> nil) and (FIndexFileType in [itMDX, itCDX]) then begin
    FIndexFile.ZapIndex;
    fCount := GetFieldCount;

    First;
    while True do
      try
        if EOF or not ReadRecord then
          Break;

        if RecordDeleted then
          Continue;

        for i := 0 to fCount - 1 do  begin
          FieldName := UpperCase(GetFieldName(i));
          for idxNo := 0 to FIndexFile.GetIndexCount - 1 do begin
            Expression := FIndexFile.GetIndexExpression(IdxNo);
            if Expression.Fields.IndexOf(FieldName) >= 0 then begin
              Value.Value := GetFieldValue(i);
              Value.ValueType := GetVirtualValueType(Value.Value);

              keyBuffer := GetIndexKeyData(Value, i, idxNo);
              FIndexFile.Add(idxNo, FCurrentRecNo + 1, @keyBuffer[0]);
            end;
          end;
        end;
      finally
        Next;
      end;
  end;
end;

{ TDBFClipper }

constructor TDBFClipper.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited;

  FDBFFormat := dfClipper;
end;

class function TDBFClipper.GetMemoClass: TDBFMemoClass;
begin
  Result := TDBFClipperMemo;
end;

{ TDBFDBaseIV }

constructor TDBFDBaseIV.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited;

  FDBFFormat := dfdBaseIV;
end;

class function TDBFDBaseIV.GetIndexExt: string;
begin
  Result := MDX_EXT;
end;

class function TDBFDBaseIV.GetMemoClass: TDBFMemoClass;
begin
  Result := TDBFIVMemo;
end;

class function TDBFDBaseIV.GetIndexClass: TDBFIndexClass;
begin
  Result := TMDXFile;
end;

procedure TDBFDBaseIV.SetHasMemo(Value: boolean);
begin
  if Value then
    FHeader.DB3.version := FHeader.DB3.version or DBF_HAS_DBT or DBF_DBASE4_DBT
  else
    FHeader.DB3.version := FHeader.DB3.version and not(DBF_HAS_DBT or DBF_DBASE4_DBT);
end;

procedure TDBFDBaseIV.InternalOpenIndexFile;
var
  i, j: integer;
  IndexNamesList: TStringList;
begin
  // dBase 3 ndx, used only for fast search
  // dBase 4 has same version info except presence of DBF_FLAG_HAS_MDX
  if (FHeader.DB3.flags and DBF_FLAG_HAS_MDX) <> 0 then begin
    FIndexFileType := itMDX;
    FIndexFile := GetIndexClass.Create(Self, FindFileByName(ChangeFileExt(FFileName, GetIndexExt)));
    FIndexFile.Open;
    IndexNamesList := TStringList.Create;
    try
      FIndexFile.GetIndexFields(IndexNamesList);
      for i := 0 to IndexNamesList.Count - 1 do begin
        for j := 0 to FFieldCount - 1 do
          FKeyFields[j] := FKeyFields[j] or (UpperCase(IndexNamesList[i]) = UpperCase(GetFieldName(j)));
      end;
    finally
      IndexNamesList.Free;
    end;
  end;
end;

function TDBFDBaseIV.GetIndexKeyData(const Value: TVirtualValue; FieldNo, IdxNo: integer): TBytes;
var
  fType: AnsiChar;
  Expression: TDBFExpression;
  ExprValue: variant;
  keyLen, len: integer;
  str: string;
  aStr: AnsiString;
  bcd: TBcd;
  n: TDBaseNumeric;
  dt: TDateTime;
  d: Double;
begin
  keyLen := FIndexFile.GetIndexKeyLength(IdxNo);
  Assert(keyLen > 0);
  SetLength(Result, keyLen);
  FillChar(Result[0], Length(Result), 0); // Result reusable, several filters can use same Result

  fType := FIndexFile.GetIndexKeyType(IdxNo);

  Expression := FIndexFile.GetIndexExpression(IdxNo);
  ExprValue := FCalculator.CalculateExpression(Self, Expression, Value, FieldNo, GetIndexFieldValue).Result;

  if fType = {$IFNDEF NEXTGEN}#0{$ELSE}0{$ENDIF} then begin
    fType := Expression.ResultType;
    FIndexFile.SetIndexKeyType(IdxNo, fType)
  end;

  case fType of
    DBF_TYPE_CHAR: begin
      FillChar(Result[0], Length(Result), $20);
      if not VarIsNull(ExprValue) then begin
        aStr := AnsiString(VarToStr(ExprValue));
        len := Min(LengthA(aStr), keyLen);
        if len > 0 then
          Move(PAnsiChar(aStr)^, Result[0], len);
      end;
    end;
    DBF_TYPE_NUMERIC: begin
      SetLength(Result, SizeOf(TDBaseNumeric)); // ???
      if not VarIsNull(ExprValue) then begin
        case Value.ValueType of
          vrNull:
            bcd := NullBcd;
          vrInteger:
            bcd := Int64ToBcd(integer(ExprValue)); // IntegerToBcd(Value.Value); - Range check error
          vrFloat:
            bcd := DoubleToBcd(double(ExprValue));
          vrString:
            bcd := StrToBcd(VarToStr(ExprValue));
        else
          raise Exception.CreateFmt('Wrong ValueType %d', [integer(Value.ValueType)]);
        end;
      end
      else
        bcd := NullBcd;
      n := TDBFIndexFile.BcdToDBaseNumeric(@bcd);
      Move(n, Result[0], SizeOf(TDBaseNumeric));
    end;
    DBF_TYPE_DATE: // string YYYY-MM-DD to string YYYYMMDD like 20021201
      if not VarIsNull(ExprValue) then begin
        if Value.ValueType in [vrString, vrAnsiString, vrWideString] then begin
          str := StringReplace(VarToStr(ExprValue), '-', '', [rfReplaceAll]);
          dt := EncodeDate(StrToInt(Copy(str, 1, 4)), StrToInt(Copy(str, 5, 2)), StrToInt(Copy(str, 7, 2)));
        end
        else
          dt := double(ExprValue);
        d := DateTimeToJulianDate(dt) + 0.5; // 12:00:00 of day must have!
        Move(d, Result[0], SizeOf(double));
      end;
  else
    raise Exception.CreateFmt('Unknown fType %s', [string(AnsiString(fType))]);
  end;
end;

function TDBFDBaseIV.TestSearchDepth(FieldNo, First, Last: integer): integer;
var
  i, idxNo: integer;
  checkingIdxName: string;
  checkingIdxType: AnsiChar;
  keyBuffer: TBytes;
  FirstEntry: PDBFLinkedListEntry;
  dt: TDateTime;
  Value: TVirtualValue;
  found: boolean;
{$IFDEF LOG_PACKETS}
  recNo: integer;
{$ENDIF}
begin
{$IFNDEF VER9P}
  keyBuffer := nil; // anti-warning
{$ENDIF}

  Result := 0;
  Assert(FIndexFileType = itMDX);

  checkingIdxName := GetFieldName(FieldNo);
  checkingIdxType := GetFieldType(FieldNo);

  idxNo := FIndexFile.GetIndexNo(checkingIdxName);

  for i := First to Last{16386} do begin
    case checkingIdxType of
      DBF_TYPE_AUTOINC, DBF_TYPE_INTEGER: begin
        Value.Value := i;
        Value.ValueType := vrInteger;
      end;
      DBF_TYPE_CHAR: begin
        Value.Value := 'C' + IntToStr(i);
        Value.ValueType := vrString;
      end;
      DBF_TYPE_NUMERIC: begin
        Value.Value := i;
        Value.ValueType := vrFloat;
      end;
      DBF_TYPE_DATE: begin
        dt := EncodeDateTime(2016, 5, {19} 23, 12, 0, 0, 0); // 12:00:00 of day must have!
        dt := IncDay(dt, i);
        Value.Value := dt;
        Value.ValueType := vrFloat;
      end;
      DBF_TYPE_TIMESTAMP: begin
        dt := EncodeDateTime(2016, 5, 24, 17, 34, 46, 0);
        dt := IncDay(dt, i);
        Value.Value := dt;
        Value.ValueType := vrFloat;
      end;
      DBF_TYPE_OLEBLOB: begin // Double field
        Value.Value := i;
        Value.ValueType := vrFloat;
      end;
    end;
    keyBuffer := GetIndexKeyData(Value, FieldNo, idxNo);

    case FIndexFileType of
      itMDX: begin
        FirstEntry := FIndexFile.Find(nil, 0, idxNo, @keyBuffer[0], found);
        Result := FIndexFile.LinkedListDepth(FirstEntry);
      {$IFDEF LOG_PACKETS}
        recNo := FirstEntry.EntryNo;
        AddToLog(Format('%d '#9'found: %d '#9'depth: %d '#9'recNo: %d', [i, integer(found), Result, recNo]));
      {$ENDIF}
        FIndexFile.FreeLinkedList(FirstEntry);
      end;
      itCDX:
        ;
    end;
  end;
end;

procedure TDBFDBaseIV.TestDuplicateIndexPath(IdxName: string);
var
  i, idxNo: integer;
  FirstEntry, LastEntry, temp: PDBFLinkedListEntry;
  list: TList;
{$IFDEF LOG_PACKETS}
  s: string;
{$ENDIF}
begin
  case FIndexFileType of
    itMDX, itCDX: begin
      idxNo := FIndexFile.GetIndexNo(IdxName);
      if idxNo >= 0 then begin
        list := TList.Create;
        FirstEntry := FIndexFile.GetLeftmostEntry(idxNo);
        LastEntry := FIndexFile.GetRightmostEntry(idxNo);
        FIndexFile.CheckPathByEntries(FirstEntry, LastEntry, idxNo, list);
        FIndexFile.FreeLinkedList(FirstEntry);
        FIndexFile.FreeLinkedList(LastEntry);
        Assert(list.Count = 0); // todo as check somehow?
        for i := 0 to list.Count - 1 do begin
          temp := list[i];
        {$IFDEF LOG_PACKETS}
          temp := FIndexFile.LinkedListRoot(temp);
          s := '';
          while True do begin
            s := s + Format('B: %.2X, E: %d', [temp.BlockNo, temp.EntryNo]) + #9;
            if temp.Child <> nil then
              temp := temp.Child
            else
              Break;
          end;
          AddToLog(s);
        {$ENDIF}
          FIndexFile.FreeLinkedList(PDBFLinkedListEntry(temp));
        end;
        list.Free;
      end;
    end;
  end;
end;

{ TDBFDBaseV }

constructor TDBFDBaseV.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited;

  FDBFFormat := dfdBaseV;
end;

function TDBFDBaseV.IsAllowedFieldType(FieldType: AnsiChar): boolean;
begin
  Result := inherited IsAllowedFieldType(FieldType) or (FieldType in [DBF_TYPE_FLOAT, DBF_DB7_TYPE_DOUBLE, DBF_TYPE_BINARY, DBF_TYPE_GENERAL]);
end;

function TDBFDBaseV.IsAllowedIndexType(FieldType: AnsiChar): boolean;
begin
  Result := inherited IsAllowedIndexType(FieldType) or (FieldType in [DBF_TYPE_FLOAT, DBF_DB7_TYPE_DOUBLE]);
end;

{ TDBFDBaseVII }

constructor TDBFDBaseVII.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited;

  FDBFFormat := dfdBaseVII;
  if HeaderFormat = dfdBaseVII then begin
    FHeaderSize := SizeOf(TDBFDBase7Header);
    FFieldSize := DBase7FieldSize;
  end;
end;

function TDBFDBaseVII.GetFieldType(FieldNo: integer): AnsiChar;
var
  fieldPtr: PDBFField;
begin
  if HeaderFormat = dfdBaseVII then begin
    fieldPtr := GetField(FieldNo);
    Result := fieldPtr.DB7FType;
  end
  else
    Result := inherited GetFieldType(FieldNo);
end;

function TDBFDBaseVII.GetFieldLength(FieldNo: integer): Byte;
var
  fieldPtr: PDBFField;
begin
  if HeaderFormat = dfdBaseVII then begin
    fieldPtr := GetField(FieldNo);
    Result := fieldPtr.DB7Len;
  end
  else
    Result := inherited GetFieldLength(FieldNo);
end;

function TDBFDBaseVII.GetFieldNumDecimal(FieldNo: integer): Byte;
var
  fieldPtr: PDBFField;
begin
  if HeaderFormat = dfdBaseVII then begin
    fieldPtr := GetField(FieldNo);
    Result := fieldPtr.DB7NumDecimal;
  end
  else
    Result := inherited GetFieldNumDecimal(FieldNo);
end;

function TDBFDBaseVII.GetFieldName(FieldNo: integer): string;
var
  fieldPtr: PDBFField;
begin
  if HeaderFormat = dfdBaseVII then begin
    fieldPtr := GetField(FieldNo);
    Result := DB7NameToString(fieldPtr);
  end
  else
    Result := inherited GetFieldName(FieldNo);
end;

function TDBFDBaseVII.GetFieldTypeIsMemo(FieldType: AnsiChar): boolean;
begin
  Result := FieldType in [DBF_TYPE_MEMO, DBF_TYPE_BINARY, DBF_TYPE_BLOB];
end;

function TDBFDBaseVII.IsAllowedFieldType(FieldType: AnsiChar): boolean;
begin
  Result := inherited IsAllowedFieldType(FieldType) or (FieldType in [DBF_TYPE_AUTOINC, DBF_TYPE_INTEGER, DBF_TYPE_TIMESTAMP, DBF_TYPE_FLOAT, DBF_DB7_TYPE_DOUBLE, DBF_TYPE_BINARY, DBF_TYPE_GENERAL]);
end;

function TDBFDBaseVII.IsNullableFieldType(FieldType: AnsiChar; Flags: byte): boolean;
begin
  if FieldType = DBF_TYPE_AUTOINC then
    Result := False
  else
    Result := inherited IsNullableFieldType(FieldType, Flags);
end;

function TDBFDBaseVII.IsAllowedIndexType(FieldType: AnsiChar): boolean;
begin
  Result := inherited IsAllowedIndexType(FieldType) or (FieldType in [DBF_TYPE_AUTOINC, DBF_TYPE_INTEGER, DBF_TYPE_TIMESTAMP]);
end;

procedure TDBFDBaseVII.SetFieldDef(FieldPtr: PDBFField; ColumnInfo: PColumnInfo; var Offset: integer);
var
  fType: AnsiChar;
  aStr: AnsiString;
begin
  aStr := AnsiString(ColumnInfo.Name);
  if Length(aStr) > Length(FieldPtr.DB7Name) then
    SetLengthA(aStr, Length(FieldPtr.DB7Name));
  if LengthA(aStr) > 0 then
    Move(PAnsiChar(aStr)^, FieldPtr.DB7Name[0], LengthA(aStr));

  fType := GetDbfType(ColumnInfo.DataType, string(aStr));
  FieldPtr.DB7FType := fType;

  if (ColumnInfo.Length > 0) and (fType <> DBF_DB7_TYPE_DOUBLE) then
    FieldPtr.DB7Len := Byte(ColumnInfo.Length)
  else if DbfFieldTypeToFieldLen[Byte(fType)] > 0 then
    FieldPtr.DB7Len := DbfFieldTypeToFieldLen[Byte(fType)]
  else
    raise Exception.CreateFmt('Unknown field length for field %s type %s', [ColumnInfo.Name, ColumnInfo.DataType]);
  //Inc(FHeader.DB3.recLen, FieldPtr.DB7Len);
  Inc(Offset, FieldPtr.DB7Len);

  if (ColumnInfo.Scale < 0) or (fType = DBF_DB7_TYPE_DOUBLE) then
    FieldPtr.DB7NumDecimal := 0
  else
    FieldPtr.DB7NumDecimal := ColumnInfo.Scale;

  if ColumnInfo.IsAutoincrement then // Next Autoincrement value, if the Field type is Autoincrement, 0x00 otherwise.
    FieldPtr.DB7AutoincNextValue := 1
  else
    FieldPtr.DB7AutoincNextValue := 0;
end;

procedure TDBFDBaseVII.SetFieldConstraint(FieldPtr: PDBFField; ConstraintInfo: PConstraintInfo);
begin
  if (ConstraintInfo <> nil) and (ConstraintInfo.ConstraintType = ctPrimaryKey) then begin
    //FHeader.DB3.flags := FHeader.DB3.flags or DBF_FLAG_HAS_MDX;
    FieldPtr.DB7FlagMDX := DBF_FLAG_HAS_MDX;
  end
  else
    FieldPtr.DB7FlagMDX := 0;
end;

procedure TDBFDBaseVII.DescribeField(FieldNo: integer; var Idx: integer; var FieldDesc: TVirtualFieldDesc);
var
  i: integer;
begin
  inherited;

  if Length(FConstraints) > 0 then
    for i := 0 to Length(FConstraints) - 1 do
      if FConstraints[i].Offset - 1 = FieldNo then begin
        Assert(FConstraints[i].IsConstraint = 2);
        case FConstraints[i].Prop of
          DBF7_PROP_REQUIRED:
            FieldDesc.Required := True;
          DBF7_PROP_MIN: begin
            //v := FFile.GetPropValue(i);
            // todo do something with v
          end;
          DBF7_PROP_MAX: begin
            //v := FFile.GetPropValue(i);
            // todo do something with v
          end;
          DBF7_PROP_DEFAULT:
            FieldDesc.Default := GetPropValue(i);
          DBF7_PROP_DATABASE: begin
            // ???
          end;
        else
          raise Exception.CreateFmt('Unknown prop type %d', [FConstraints[i].Prop]);
        end;
      end;
end;

function TDBFDBaseVII.GetFieldNull(FieldNo: integer): boolean;
var
  fType: AnsiChar;
  aStr: AnsiString;
begin
  fType := GetFieldType(FieldNo);
  case fType of
    DBF_TYPE_INTEGER, DBF_TYPE_AUTOINC:
      Result := False;
    DBF_DB7_TYPE_DOUBLE:
      Result := False;
    DBF_TYPE_TIMESTAMP:
      Result := PInt64(@FCurrentRecordBuffer[FOffsets[FieldNo]])^ = 0;
    DBF_TYPE_MEMO, DBF_TYPE_BINARY, DBF_TYPE_GENERAL: begin
      SetLengthA(aStr, FLengths[FieldNo]);
      Move(FCurrentRecordBuffer[FOffsets[FieldNo]], PAnsiChar(aStr)^, FLengths[FieldNo]);
      Result := Trim(string(aStr)) = '';
    end;
  else
    Result := inherited GetFieldNull(FieldNo);
  end;
end;

function TDBFDBaseVII.Open(ForceReadOnly: boolean = False; ForceExclusive: boolean = False): boolean;
var
  i, readed: integer;
  constr: TDBFDBase7FieldProperties;
begin
  Result := inherited Open(ForceReadOnly, ForceExclusive);

  FPropDataOffs := 0;
  FConstraints := nil;
  FStream.Position := FSavedPosition;
  if (HeaderFormat = dfdBaseVII) and (FStream.Position + SizeOf(TDBFDBase7FieldProperties) < FHeader.DB3.dataOffset) then begin
    //Assert(FHeader.DB3.dataOffset - FFileStream.Position >= $200);
    FPropDataOffs := FStream.Position;
    // constraints: min, max, default, required
    readed := FStream.Read(constr, SizeOf(constr));
    if readed < SizeOf(constr) then
      raise Exception.CreateFmt('error: readed %d', [readed]);

    FStream.Position := FPropDataOffs + constr.StartOfStandardDescr;
    SetLength(FConstraints, constr.NumOfProps);
    for i := 0 to constr.NumOfProps - 1 do begin
      readed := FStream.Read(FConstraints[i], SizeOf(TDBFDBase7StandardDescriptor));
      if readed < SizeOf(TDBFDBase7StandardDescriptor) then
        raise Exception.CreateFmt('error: readed %d', [readed]);
    end;
  end;
end;

function TDBFDBaseVII.GetPropValue(PropIndex: integer): variant;
var
  fType: AnsiChar;
  i, propLen, readed: integer;
  oldOffs: Int64;
  b: Byte;
  d: Double;
  i64: Int64;
  aStr: AnsiString;
  str: string;
  buf: TBytes;
begin
  Result := Unassigned;

  fType := DbfPropTypeToDbfType[FConstraints[PropIndex].FieldType];
  propLen := FConstraints[PropIndex].DataLength;
  Assert(propLen > 0);
  SetLength(buf, propLen);
  oldOffs := FStream.Position;
  try
    FStream.Position := FPropDataOffs + FConstraints[PropIndex].DataOffset;
    readed := FStream.Read(buf[0], propLen);
    if readed < propLen then
      raise Exception.CreateFmt('error: readed %d', [readed]);
  finally
    FStream.Position := oldOffs;
  end;

  case fType of
    DBF_TYPE_CHAR: begin
      SetLengthA(aStr, propLen);
      Move(buf[0], PAnsiChar(aStr)^, propLen);
      Result := TrimRight(string(aStr));
    end;
    DBF_TYPE_NUMERIC, DBF_TYPE_FLOAT: begin
      SetLengthA(aStr, propLen);
      Move(buf[0], PAnsiChar(aStr)^, propLen);
      str := Trim(StringReplace(string(aStr), '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []));
      if str <> '' then
        Result := str;
    end;
    DBF_TYPE_DATE: begin // string YYYYMMDD like 20021201
      Assert(propLen = 8);
      SetLengthA(aStr, 8);
      Move(buf[0], PAnsiChar(aStr)^, 8);
      str := string(aStr);
      if Trim(str) <> '' then
        Result := EncodeDate(StrToInt(Copy(str, 1, 4)), StrToInt(Copy(str, 5, 2)), StrToInt(Copy(str, 7, 2)));
    end;
    DBF_TYPE_TIMESTAMP: begin
      Assert(propLen = SizeOf(TTimeStamp));
      Move(buf[0], i64, SizeOf(TTimeStamp));
      Result := DBaseTimeStampToDateTime(i64);
    end;
    DBF_TYPE_INTEGER: begin
      Assert(propLen = SizeOf(integer));
      i := PInteger(@buf[0])^;
      Assert(FHeaderFormat = dfdBaseVII);
      Result := DBase7DataToInteger(i);
    end;
    DBF_TYPE_LOGICAL: begin
      b := buf[0];
      if b = Byte(CHAR_TRUE) then
        Result := True
      else
      if b = Byte(CHAR_FALSE) then
        Result := False;
    end;
    DBF_TYPE_MEMO, DBF_TYPE_BINARY, DBF_TYPE_GENERAL, DBF_TYPE_BLOB, DBF_TYPE_OLEBLOB: begin
      // Binary: dBASE 5,  Double: dBaseVII
      if fType = DBF_TYPE_OLEBLOB then begin
        Assert(propLen = 8);
        d := PDouble(@buf[0])^;
        Result := DBase7DataToDouble(d);
      end
      else
        raise Exception.CreateFmt('Not supported: types memo, binary, blob in props %s', [fType]);
    end;
    DBF_TYPE_AUTOINC: begin
      // big-endian
      Result := FastSwap(PCardinal(@buf[0])^) and $7FFFFFFF;
    end;
  else
    raise Exception.CreateFmt('Unknown type %s', [fType]);
  end;
end;

function TDBFDBaseVII.GetIndexKeyData(const Value: TVirtualValue; FieldNo, IdxNo: integer): TBytes;
var
  fType: AnsiChar;
  Expression: TDBFExpression;
  ExprValue: variant;
  i, keyLen: integer;
  dt: TDateTime;
  d: Double;
  i64: Int64;
begin
  keyLen := FIndexFile.GetIndexKeyLength(IdxNo);
  Assert(keyLen > 0);
  SetLength(Result, keyLen);
  FillChar(Result[0], Length(Result), 0); // Result reusable, several filters can use same Result

  fType := FIndexFile.GetIndexKeyType(IdxNo);

  Expression := FIndexFile.GetIndexExpression(IdxNo);
  ExprValue := FCalculator.CalculateExpression(Self, Expression, Value, FieldNo, GetIndexFieldValue).Result;

  if fType = {$IFNDEF NEXTGEN}#0{$ELSE}0{$ENDIF} then begin
    fType := Expression.ResultType;
    FIndexFile.SetIndexKeyType(IdxNo, fType)
  end;

  case fType of
    DBF_TYPE_AUTOINC, DBF_TYPE_INTEGER:
      if not VarIsNull(ExprValue) then begin
        i := IntegerToDBase7Data(integer(ExprValue));
        PInteger(@Result[0])^ := i;
      end;
    DBF_TYPE_TIMESTAMP:
      if not VarIsNull(ExprValue) then begin
        if Value.ValueType = vrString then
          dt := InternalStrToDateTime(VarToStr(ExprValue))
        else
          dt := TDateTime(ExprValue);
        i64 := DateTimeToDBaseTimeStamp(dt);
        Move(i64, Result[0], SizeOf(Int64));
      end;
    DBF_DB7_TYPE_DOUBLE:
      if not VarIsNull(ExprValue) then begin
        d := DoubleToDBase7Data(double(ExprValue));
        Move(d, Result[0], SizeOf(double));
      end;
  else
    Result := inherited GetIndexKeyData(Value, FieldNo, IdxNo);
  end;
end;

function TDBFDBaseVII.GetFieldValue(FieldNo: integer): variant;
var
  fType: AnsiChar;
  i, MemoIdx: integer;
  d: Double;
  i64: Int64;
  aStr: AnsiString;
  Blob: TBlob;
begin
  Result := Unassigned;

  fType := GetFieldType(FieldNo);

  case fType of
    DBF_TYPE_TIMESTAMP: begin
      Move(FCurrentRecordBuffer[FOffsets[FieldNo]], i64, SizeOf(TTimeStamp));
      Result := DBaseTimeStampToDateTime(i64);
    end;
    DBF_TYPE_INTEGER: begin
      i := PInteger(@FCurrentRecordBuffer[FOffsets[FieldNo]])^;
      Result := DBase7DataToInteger(i);
    end;
    DBF_DB7_TYPE_DOUBLE: begin
      Assert(FLengths[FieldNo] = 8);
      d := PDouble(@FCurrentRecordBuffer[FOffsets[FieldNo]])^;
      Result := DBase7DataToDouble(d);
    end;
    DBF_TYPE_MEMO, DBF_TYPE_BINARY, DBF_TYPE_GENERAL, DBF_TYPE_BLOB: begin
      SetLengthA(aStr, FLengths[FieldNo]);
      Move(FCurrentRecordBuffer[FOffsets[FieldNo]], PAnsiChar(aStr)^, FLengths[FieldNo]);
      if Trim(string(aStr)) <> '' then begin
        if TDBFDataConverter.InternalStrToInt32(string(aStr), @MemoIdx, False) <> csSuccess then
          if not FIgnoreDataErrors then
            RaiseDataError(FieldNo);
      end
      else
        MemoIdx := 0;

      if MemoIdx > 0 then begin
        Blob := TBlob.Create;
        TVarData(Result).VType := varSharedObject;
        TVarData(Result).VPointer := Blob;
        FMemo.Read(Blob, MemoIdx);
      end;
    end;
    DBF_TYPE_AUTOINC: // big-endian
      Result := FastSwap(PCardinal(@FCurrentRecordBuffer[FOffsets[FieldNo]])^) and $7FFFFFFF;
  else
    Result := inherited GetFieldValue(FieldNo);
  end;
end;

procedure TDBFDBaseVII.SetFieldValue(FieldNo: integer; var Value: TVirtualValue; GenerateAutoInc: boolean);
var
  fieldPtr: PDBFField;
  i: integer;
  fType: AnsiChar;
  d: Double;
  dt: TDateTime;
  i64: Int64;
begin
  fieldPtr := GetField(FieldNo);
  fType := GetFieldType(FieldNo);

{$IFDEF LOG_PACKETS}
  AddToLog(Format('DBaseVII.SetFieldValue: %d, %s', [FieldNo, string(Value.Value)]));
{$ENDIF}

  case fType of
    DBF_TYPE_TIMESTAMP: begin
      i64 := 0;
      if VirtualContainsValue(Value) then begin
        dt := InternalStrToDateTime(Value.Value);
        i64 := DateTimeToDBaseTimeStamp(dt);
      end;
      Move(i64, FCurrentRecordBuffer[FOffsets[FieldNo]], SizeOf(TTimeStamp));
    end;
    DBF_TYPE_INTEGER: begin
      if VirtualContainsValue(Value) then begin
        i := Value.Value;
        i := IntegerToDBase7Data(i);
      end
      else
        i := 0;
      Move(i, FCurrentRecordBuffer[FOffsets[FieldNo]], SizeOf(Integer));
    end;
    DBF_DB7_TYPE_DOUBLE: begin
      Assert(FLengths[FieldNo] = 8);
      if VirtualContainsValue(Value) then begin
        d := Value.Value;
        d := DoubleToDBase7Data(d);
      end
      else
        d := 0;
      PDouble(@FCurrentRecordBuffer[FOffsets[FieldNo]])^ := d;
    end;
    DBF_TYPE_MEMO, DBF_TYPE_BINARY, DBF_TYPE_GENERAL, DBF_TYPE_BLOB: begin
      if not VirtualContainsValue(Value) then
        WriteMemoIndex(FieldNo, 0, 0)
      else
        SetMemoValue(FieldNo, False, fType <> DBF_TYPE_MEMO, False, @Value);
    end;
    DBF_TYPE_AUTOINC: begin
      if GenerateAutoInc then begin
        i := fieldPtr.DB7AutoincNextValue;
        // little-endian
        fieldPtr.DB7AutoincNextValue := i + 1;
        // big-endian
        i := FastSwap(i) or $80;
        Move(i, FCurrentRecordBuffer[FOffsets[FieldNo]], SizeOf(Integer));
        Value.Value := i;
        Value.ValueType := vrInteger;
        WriteField(FieldNo);
      end;
    end;
  else
    inherited;
  end;
end;

{$ENDIF}

end.
