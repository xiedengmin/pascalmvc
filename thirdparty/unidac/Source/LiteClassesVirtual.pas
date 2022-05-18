
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I VirtualQuery.inc}
{$I LiteDac.inc}
unit LiteClassesVirtual;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, Variants, SyncObjs, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
{$IFDEF VER17P}
  Generics.Collections,
{$ENDIF}
  CLRClasses, CRTypes, CRAccess, CRParser, CRTimeStamp, MemData,
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFDEF VIRTUAL_QUERY}
  LiteFunctionVirtual, LiteCollationVirtual, LiteConstsVirtual, LiteCallVirtual, LiteErrorVirtual, LiteParserVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
  {$IFNDEF LITE}LiteFunction, LiteCollation,{$ENDIF} LiteConsts, LiteCall, LiteError, LiteParser;
{$ELSE}
  {$IFNDEF LITE}LiteFunctionUni, LiteCollationUni,{$ENDIF} LiteConstsUni, LiteCallUni, LiteErrorUni, LiteParserUni;
{$ENDIF}
{$ENDIF}

type
  TSQLiteCommand = class;
  TSQLiteTypes = class;

  TSQLiteConnection = class(TCRConnection)
  private
    FLiteTypes: TSQLiteTypes;
  {$IFNDEF LITE}
    FCollationManager: TSQLiteCollationManager;
    // to avoid compiler crash when building LiteStaticVirtual for CB6
    FFunctionManager: {$IFNDEF CB6}TSQLiteFunctionManager{$ELSE}TObject{$ENDIF};
  {$ENDIF}
    FDatabase: string;
    FClientLibrary: string;
    FUseUnicode: boolean;
    FASCIIDataBase: boolean;
    FCharLength: integer;
    FEncryptionKey: string;
    FDateFormat: string;
    FTimeFormat: string;
    FNativeDate: boolean;
  {$IFDEF LITE}
    FChangeEncryptionKey: boolean; // if need change encryption key
    FNewEncryptionKey: string; // for change password in dbExpres
  {$ENDIF}
    FEnableSharedCache: boolean;
    FEnableLoadExtension: boolean;
    FBusyTimeout: integer;
    FReadUncommitted: boolean;
    FDefaultCollations: boolean;
    FLastInsertId: int64;
    FForeignKeys : boolean;
    FStaticLibrary: boolean;
    FForceCreateDatabase: boolean;
    FEncryptionAlgorithm: TLiteEncryptionAlgorithm;
    FConnectMode: TConnectMode;
    FLockingMode: TLockingMode;
    FSynchronous: TSynchronous;
    FJournalMode: TJournalMode;
    FAPI: TSQLite3API;
    FCipherLicense: string;

    procedure SetASCIIDataBase(const Value: boolean);
    procedure SetBusyTimeout(const Value: integer);
    procedure SetReadUncommitted(const Value: boolean);
  {$IFNDEF LITE}
    procedure SetDefaultCollations(const Value: boolean);
  {$ENDIF}
    procedure SetForeignKeys(const Value: boolean);
    procedure SetUseStaticLibrary(const Value: boolean);
    procedure SetConnectMode(const Value: TConnectMode);
    procedure SetLockingMode(const Value: TLockingMode);
    procedure SetSynchronous(const Value: TSynchronous);
    procedure SetJournalMode(const Value: TJournalMode);
  protected
    procedure InternalSetBusyTimeout(const Value: integer);
    procedure InternalSetReadUncommitted(const Value: boolean);
  {$IFNDEF LITE}
    procedure InternalSetDefaultCollations(const Value: boolean);
  {$ENDIF}
    procedure InternalSetForeignKeys(const Value: boolean);
    procedure InternalSetLockingMode(const Value: TLockingMode);
    procedure InternalSetSynchronous(const Value: TSynchronous);
    procedure InternalSetJournalMode(const Value: TJournalMode);

  {$IFNDEF LITE}
    procedure InternalCos(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalSin(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalTan(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalCot(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalAcos(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalAsin(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalAtan(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalAtan2(InValues: array of Variant; var ResultValue: Variant);

    procedure InternalDegrees(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalRadians(InValues: array of Variant; var ResultValue: Variant);

    procedure InternalTruncate(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalRound(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalCeiling(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalFloor(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalMod(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalPower(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalSqrt(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalSign(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalRand(InValues: array of Variant; var ResultValue: Variant);

    procedure InternalExp(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalLog(InValues: array of Variant; var ResultValue: Variant);
    procedure InternalLog10(InValues: array of Variant; var ResultValue: Variant);

    procedure RegisterInternalFunctions;
  {$ENDIF}

    function GetInternalCommandClass: TCRCommandClass; override;

    property EnableBCD;
    property EnableFMTBCD;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
  {$IFNDEF LITE}
    function GetLoaderClass: TCRLoaderClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
    function GetFunctionManagerClass: TSQLiteFunctionManagerClass; virtual;
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure Check(ErrorCode: integer);
    function DetectError(ErrorCode: integer; out ErrorMsg: string): integer;
    procedure ProcessError(ErrorCode: integer; ErrorMsg: string; Component: TObject); virtual;

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;
    procedure Ping; override;
    function CheckIsValid: boolean; override;
    procedure BreakExec;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;
    function CanChangeDatabase: boolean; override;
    function GetTypes: TSQLiteTypes;
  {$IFNDEF LITE}
    function GetCollationManager: TSQLiteCollationManager;
    function GetFunctionManager: TSQLiteFunctionManager;
  {$ENDIF}

    { String encoding}
    function IsUnicodeDataBase: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function EncodeString(AString: AnsiString): AnsiString; overload;
    function EncodeString(AString: AnsiString; UseUTF8: boolean): AnsiString; overload;
    function EncodeString(WString: WideString): AnsiString; overload;
    function EncodeString(WString: WideString; UseUTF8: boolean): AnsiString; overload;
    function DecodeString(pUtf8String: PAnsiChar): string;
    function DecodeStringA(pUtf8String: PAnsiChar; UseUTF8: boolean): AnsiString;
    function DecodeStringW(pUtf8String: PAnsiChar; UseUTF8: boolean): WideString;

    { SQLite encryption }
    procedure EncryptDatabase(const NewKey: string);

    { misc functions }
    procedure ReleaseDatabaseMemory;
    function GetDatabaseReadOnly(const DatabaseName: string = ''): integer;
    function Limit(Id: integer; NewVal: integer): integer;
    function NormalizeTableName(const TableName: string): string;

    property ASCIIDataBase: boolean read FASCIIDataBase;
    property UseUnicode: boolean read FUseUnicode;
    property DateFormat: string read FDateFormat;
    property TimeFormat: string read FTimeFormat;
    property NativeDate: boolean read FNativeDate;
    property ForeignKeys: boolean read FForeignKeys;
    property UseStaticLibrary: Boolean read FStaticLibrary;
    property EncryptionAlgorithm: TLiteEncryptionAlgorithm read FEncryptionAlgorithm;
    property ConnectMode: TConnectMode read FConnectMode;
    property API: TSQLite3API read FAPI;
    property CipherLicense: string read FCipherLicense;
  end;

  TSQLiteTransaction = class(TCRTransaction)
  public
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: string); override;
    procedure ReleaseSavepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;
  end;

  TSQLiteTypes = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TSQLiteConnection;

  {$IFDEF LITE}
    procedure GetTypeAttr(DataType: Integer; TypeModifier: Integer; var Length, Scale: integer);
    function GetInternalType(TypeCode: integer; var ObjectType: TObjectType): word;
  {$ENDIF}
  protected
  public
    constructor Create(Connection: TSQLiteConnection);
    destructor Destroy; override;

  {$IFDEF LITE}
    procedure DetectDataType(TypeOID, TypeModifier: integer;
      var DataType, SubDataType: word; var Length, Scale, Size: integer;
      var Fixed: boolean; var ObjectType: TObjectType;
      LongStrings, FlatBuffers, EnableBCD, EnableFMTBCD,
      CursorAsString, FieldsAsText: boolean);
  {$ENDIF}

    function GetSQLiteType(DataType: word; var NeedConvertToText: boolean): word;
    function GetVarType(VarType: TVarType): word;
    function ConvertToText(DataType, Scale: word; Value: Variant): AnsiString;
    function ConvertBlob(Value: Variant; var DestrType: IntPtr): TBytes;
    function ConvertMemo(Value: Variant): AnsiString;
  end;

  TSQLiteParamDesc = class(TParamDesc)
  private
    FNormalizedName: string;
  end;

  TSQLiteCommand = class(TCRCommand)
  private
    FCommandSQL: AnsiString;
    FPCommandSQL,
    FCurrentCommand,
    FCommandTail: IntPtr;
    FCommandLength: integer;
    FIsCursorPragma: boolean;
    FCachedBufA: TBytes;
    FCachedBufW: TBytes;

    function FindParamByNormalizedName(const Name: string): TSQLiteParamDesc;
  protected
    procedure AllocCachedBuffers;
    procedure FreeCachedBuffers;

    procedure BindParams(Offset: integer);
    procedure BindBlob(Index: integer; const Value: variant);
    procedure BindMemo(Index: integer; const Value: variant);
    procedure BindAnsiText(Index: integer; pAStr: PAnsiChar; Len: Integer);
    procedure BindWideText(Index: integer; const WStr: WideString);

    procedure DetectCommandType;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TSQLiteConnection;
    FStmt: pSQLite3Stmt;
    FCursorState: TCursorState;
    FExecResult: integer;
    FRowsAffected: integer;
    FLockAfterExecute: boolean;
    FDateSeparator: Char;
    FShortDateFormat: string;
    FTimeSeparator: Char;
    FShortTimeFormat: string;
    FFormatSettingsInitialized: boolean;
  protected
    procedure Check(ErrorCode: integer);
    procedure CheckPrepareError(ErrorCode: integer); virtual;

    procedure InitFormatSettings;
    procedure ResetFormatSettings;

    procedure InternalPrepare; virtual;
    procedure InternalExecute; virtual;

    procedure CreateBatchCommand; override;
    function NeedBatchTransaction: boolean; override;
    function NeedBatchSavepoint: boolean; override;
    procedure InternalExecuteBatch(Iters, Offset: integer); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetTableInfoClass: TTableInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    class function GetParamDescClass: TParamDescClass; override;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure SetConnection(Value: TCRConnection); override;

    procedure BreakExec; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;
    procedure Execute; override;
    procedure SetSQL(const Value: string); override;
    function ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string; override;
    procedure ParseSQLType; override;
    procedure InitProcParams(const Name: string; Overload: integer);
    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    property EnableBCD;
    property EnableFMTBCD;
  end;

  TSQLiteRecordSet = class(TCRRecordSet)
  private
    FCommand: TSQLiteCommand;
    FDumpData,
    FUnknownAsString,
    FAdvancedTypeDetection,
    FHasUndefinedFields,
    FInExplicitInit: boolean;
    FFieldTypes: array of integer;

    procedure DescribeFieldDesc(Field: TCRFieldDesc; Index: integer);
    procedure ReadFieldValues(RecBuf: IntPtr; Row: integer);
    procedure ReadFieldValuesForDump(RecBuf: IntPtr);
  protected
    procedure Check(ErrorCode: integer);
    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
    procedure InternalPrepare; override;
    function ExtFieldsInfoIsInternal: boolean; override;
    procedure CreateFieldDescs; override;
  {$IFNDEF LITE}
    procedure RequestFieldsInfo(Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo); override;
  {$ENDIF}
    procedure InternalClose; override;
    function IdentityFieldIsData: boolean; override;

  { Fetch }
    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); override;
    function NeedUnPrepareAfterFetch: boolean; override;

    function GetDBType(const SQLTypeName: string; var Len, Scale: integer): word; overload; virtual;
    function GetDBType(SQLType: integer): word; overload; virtual;
    function GetDBUnknown: word; virtual;
    function IsFixedDBType(DBType: word): boolean; virtual;
    function GetDataType(DBType: word): word; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InternalPrefetch;
    procedure ExplicitInitFields; override;
    procedure GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType: Word; SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean); override;
    procedure PutDataAsVariant(DataBuf: IntPtr; DataLenPtr: PWord; DataType: Word; Len, Scale: Word; HasParent: boolean; const Value: variant; IsDatabaseValue: boolean); override;
    class procedure GetDateFromBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure DetectIdentityField; override;
    procedure DetectFieldType(const FieldName: string; DBType: Word; DBLength: Integer; DBScale: SmallInt;
      out DataType: Word; out Len, Scale: Integer; out Fixed: boolean);

    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override;
    procedure SetToEnd; override;
  end;

  TSQLiteTableInfo = class(TCRTableInfo)
  protected
    FTableNameFull: string;
    FKeyCount: integer;

    function GetTableNameFull: string; override;
    procedure SetTableNameFull(const Value: string); override;
  end;

{ TSQLiteMetaData }

  TColumnInfo = record
    Name: string;
    DataType: string;
    IsAutoincrement: boolean;
    Default: string;
    Length, Scale: integer;
    NotNull: boolean;
  end;

  TIndexColumnInfo = record
    ColumnIndex: integer;
    IsDesc: boolean;
  end;

  TIndexType = (itPrimaryKey, itUnique, itNonUnique);

  TIndexInfo = record
    IndexType: TIndexType;
    ColumnInfo: array of TIndexColumnInfo;
  end;

  TConstraintType = (ctPrimaryKey, ctUnique, ctCheck, ctForeignKey);

  TConstraintInfo = record
    ConstraintType: TConstraintType;
    Name: string;
    ColumnInfo: array of TIndexColumnInfo;
  end;

  TTableMembers = record
    Columns: array of TColumnInfo;
    Constraints: array of TConstraintInfo;
  end;

{$IFDEF LITE}
  {$DEFINE DBX_METADATA}
{$ENDIF}

  TSQLiteMetaData = class (TCRMetaData)
  protected
    function GetConnection: TSQLiteConnection;
    function GetTypesForSQL(const ObjectTypes: string; AllTypes: array of string): string;
    function CreateRecordSet: TCRRecordSet; override;

    procedure InternalGetMetaDataKindsList(List: TStringList); override;
    function GetTables(Restrictions: TStrings): TData; override;
    procedure CreateColumnsFields; override;
    function GetColumns(Restrictions: TStrings): TData; override;
    function GetProcedures(Restrictions: TStrings): TData; override;
    function GetProcedureParameters(Restrictions: TStrings): TData; override;
    function GetIndexes(Restrictions: TStrings): TData; override;
    function GetIndexColumns(Restrictions: TStrings): TData; override;
    function GetConstraints(Restrictions: TStrings): TData; override;
    function GetConstraintColumns(Restrictions: TStrings): TData; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class procedure ParseTableSQL(const SQL: string; var TableName: string; var Members: TTableMembers);
  end;

  TSQLiteInfo = class (TSQLInfo)
  public
    function LeftQuote: Char; override;
    function RightQuote: Char; override;
    function IdentCase: TIdentCase; override;
    function ParamQuoteAllowed: boolean; override;
    function IsQuoted(const Value: string): boolean; override;
    procedure SplitObjectName(const Name: string; out Info: TSQLObjectInfo); override;
    procedure ParseTablesInfo(const SQL: string; TablesInfo: TCRTablesInfo); override;
  end;

{$IFNDEF LITE}
  TSQLiteLoader = class(TCRSimpleLoader)
  private
    FAutoCommit,
    FAutoCommitIsUsed: boolean;
    FAutoCommitRowCount: integer;
  protected
    procedure CreateCommand; override;
    procedure DoPrepare; override;
    procedure DoLoadRow; override;
    function UsedConnection: TSQLiteConnection;
    function UsedTransaction: TSQLiteTransaction;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Finish; override;
  end;

  TSQLiteBackupProgressEvent = procedure(PagesTotal, PagesRemaining: integer) of object;

  TSQLiteBackup = class
  private
    FSrcConnection: TSQLiteConnection;
    FDestConnection: TSQLiteConnection;

    FBackupHandle: pSQLite3Backup;

    FSourceDatabaseName: string;
    FDestinationDatabaseName: string;
    FPagesPerStep: integer;
    FWaitWhenLocked: boolean;
    FWaitDelay: integer;
    FWaitTimeout: integer;
    FOnProgress: TSQLiteBackupProgressEvent;

    procedure Init;
    function Step: boolean;
    procedure Finish;

    function GetPageCount: integer;
    function GetRemaining: integer;
  public
    constructor Create(SourceConnection, DestinationConnection: TSQLiteConnection);

    procedure Backup;

    property SourceDatabaseName: string read FSourceDatabaseName write FSourceDatabaseName;
    property DestinationDatabaseName: string read FDestinationDatabaseName write FDestinationDatabaseName;
    property PagesPerStep: integer read FPagesPerStep write FPagesPerStep default -1;
    property WaitWhenLocked: boolean read FWaitWhenLocked write FWaitWhenLocked default False;
    property WaitDelay: integer read FWaitDelay write FWaitDelay default 250;
    property WaitTimeout: integer read FWaitTimeout write FWaitTimeout default 0;
    property OnProgress: TSQLiteBackupProgressEvent read FOnProgress write FOnProgress;
  end;
{$ENDIF}

var
  SQLiteInfo: TSQLiteInfo;

function ConvertStrToDateTime(DataType: word; Value: string;
  const ADateSeparator, ATimeSeparator: Char;
  const AShortDateFormat, AShortTimeFormat: string;
  NativeDate: boolean; out Fractions: string; out Error: boolean): TDateTime;
function ConvertStrToTimestampOffset(Value: string;
  const ADateSeparator, ATimeSeparator: Char;
  const AShortDateFormat, AShortTimeFormat: string): TSQLTimeStampOffset;
function FractionsToText(Fractions: Cardinal; Scale: Word): string;
function TimezoneToText(TimeZoneHour, TimeZoneMinute: SmallInt; TimeSeparator: Char): string;

implementation


uses
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
  StrUtils, DateUtils, Math,
  CRProps, CRFunctions, MemUtils, DAConsts,
{$IFDEF VIRTUAL_QUERY}
  VirtualDataTypeMap,
  LiteStaticCallVirtual, LiteDataTypeMapVirtual, LitePropsVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
  {$IFNDEF NOSTATIC}{$IFNDEF LITE}LiteStaticCall,{$ELSE}LiteStatic,{$ENDIF}{$ENDIF}
  LiteDataTypeMap, LiteProps;
{$ELSE}
  {$IFNDEF NOSTATIC}LiteStaticCallUni,{$ENDIF}
  LiteDataTypeMapUni, LitePropsUni;
{$ENDIF}
{$ENDIF}

const
  TempBufCharSize = 1024 * 64;
  TempBufWideSize = TempBufCharSize * 2; // size in bytes Wide chars buffer
  TempBufAnsiSize = TempBufCharSize * 4; // size in bytes for UTF8 chars buffer
  CursorPragmas = 'table_info index_list index_info foreign_key_list database_list ';

var
  hLockConnectCount: TCriticalSection;

{$IFDEF FPC}
{$IFNDEF UNIX}
function FileExists(const Filename: string): boolean;
var
  Attr: integer;
begin
  Attr := integer(Windows.GetFileAttributesW(PWideChar(CRFunctions.UTF8Decode(FileName))));
  if Attr <> -1 then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result:=False;
end;
{$ENDIF}
{$ENDIF}

procedure GetSQLTypeMeasures(var SQLTypeName: string; var SQLTypeLen, SQLTypeScale: integer);
var
  s: string;
  p1, p2: integer;
begin
  SQLTypeLen := -1;
  SQLTypeScale := -1;
  p1 := Pos('(', SQLTypeName);
  p2 := Pos(')', SQLTypeName);
  if (p1 > 0) and (p2 > 0) then begin
    s := Trim(Copy(SQLTypeName, p1 + 1, p2 - p1 - 1));
    SQLTypeName := Copy(SQLTypeName, 1, p1 - 1);
    p1 := Pos(',', s);
    if p1 > 0 then begin
      SQLTypeLen := StrToIntDef(Trim(Copy(s, 1, p1 - 1)), -1);
      SQLTypeScale := StrToIntDef(Trim(Copy(s, p1 + 1, Length(s) - p1)), -1);
    end
    else
      SQLTypeLen := StrToIntDef(Trim(s), -1);
  end;
end;

function ConvertStrToDateTime(DataType: word; Value: string;
  const ADateSeparator, ATimeSeparator: Char;
  const AShortDateFormat, AShortTimeFormat: string;
  NativeDate: boolean; out Fractions: string; out Error: boolean): TDateTime;
var
{$IFDEF USE_TFORMATSETTINGS}
  FmtSet: TFormatSettings;
{$ELSE}
  OldDateSeparator, OldTimeSeparator, OldDecimalSeparator: char;
  OldDateFormat, OldTimeFormat: string;
{$ENDIF}
  i, Len, Separators,
  LastDigitPos,
  LastSeparatorPos,
  PrevSeparatorPos: integer;

  procedure SetSeparators;
  begin
  {$IFDEF USE_TFORMATSETTINGS}
    FmtSet.DateSeparator := ADateSeparator;
    FmtSet.ShortDateFormat := AShortDateFormat;
    FmtSet.TimeSeparator := ATimeSeparator;
    FmtSet.ShortTimeFormat := AShortTimeFormat;
    FmtSet.DecimalSeparator := '.';
  {$ELSE}
    OldDateSeparator := DateSeparator;
    OldDateFormat := ShortDateFormat;
    OldTimeSeparator := TimeSeparator;
    OldTimeFormat := LongTimeFormat;
    OldDecimalSeparator := DecimalSeparator;
    DateSeparator := ADateSeparator;
    ShortDateFormat := AShortDateFormat;
    TimeSeparator := ATimeSeparator;
    ShortTimeFormat := AShortTimeFormat;
    DecimalSeparator := '.';
  {$ENDIF}
  end;

{$IFNDEF USE_TFORMATSETTINGS}
  procedure RestoreSeparators;
  begin
    DateSeparator := OldDateSeparator;
    ShortDateFormat := OldDateFormat;
    LongTimeFormat := OldTimeFormat;
    TimeSeparator := OldTimeSeparator;
    DecimalSeparator := OldDecimalSeparator;
  end;
{$ENDIF}

begin
  Result := 0;
  Error := False;
  Separators := 0;
  LastDigitPos := 0;
  LastSeparatorPos := 0;
  PrevSeparatorPos := 0;
  Len := Length(Value);
  for i := Len downto 2 do begin
    if CharInSet(Value[i], [':', '-', '.', ',', '/']) then begin
      Inc(Separators);
      if Separators = 1 then
        LastSeparatorPos := i
      else
        PrevSeparatorPos := i;
    end
    else if (LastDigitPos = 0) and CharInSet(Value[i], ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
      LastDigitPos := i;
    if Separators = 2 then begin
      SetSeparators;
      try
        if (Value[LastSeparatorPos] = '.') and (Value[PrevSeparatorPos] = {$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator) then begin
          if ((LastDigitPos - LastSeparatorPos) > 3) then
            Fractions := Copy(Value, LastSeparatorPos + 1, 3)
          else
            Fractions := Copy(Value, LastSeparatorPos + 1, LastDigitPos - LastSeparatorPos + 1);
          Value := Copy(Value, 1, LastSeparatorPos - 1) + Copy(Value, LastDigitPos + 1, Len);
        end
        else
          Fractions := '';

        if not TryStrToDateTime(Value, Result{$IFDEF USE_TFORMATSETTINGS}, FmtSet{$ENDIF}) then begin
          Result := 0;
          Error := True;
        end;
      finally
      {$IFNDEF USE_TFORMATSETTINGS}
        RestoreSeparators;
      {$ENDIF}
      end;
      Break;
    end
    else if i = 2 then
      if Separators > 0 then begin
        SetSeparators;
      {$IFDEF USE_TFORMATSETTINGS}
        Result := StrToFloat(Value, FmtSet);
      {$ELSE}
        try
          Result := StrToFloat(Value);
        finally
          RestoreSeparators;
        end;
      {$ENDIF}
        if NativeDate then
          Result := JulianDateToDateTime(Result);
      end
      else
        if NativeDate then
          Result := UnixToDateTime(StrToInt(Value))
        else
          Result := StrToInt(Value);
  end;

  case DataType of
    dtDate :
      Result := Trunc(Result);
    dtTime :
      Result := Abs(Frac(Result));
  end;
end;

function ConvertStrToTimestampOffset(Value: string;
  const ADateSeparator, ATimeSeparator: Char;
  const AShortDateFormat, AShortTimeFormat: string): TSQLTimeStampOffset;
var
  OldDateSeparator, OldTimeSeparator, OldDecimalSeparator: char;
  OldDateFormat, OldTimeFormat: string;

  procedure SetSeparators;
  begin
  {$IFDEF USE_FORMATSETTINGS}
    OldDateSeparator := FormatSettings.DateSeparator;
    OldDateFormat := FormatSettings.ShortDateFormat;
    OldTimeSeparator := FormatSettings.TimeSeparator;
    OldTimeFormat := FormatSettings.LongTimeFormat;
    OldDecimalSeparator := FormatSettings.DecimalSeparator;
    FormatSettings.DateSeparator := ADateSeparator;
    FormatSettings.ShortDateFormat := AShortDateFormat;
    FormatSettings.TimeSeparator := ATimeSeparator;
    FormatSettings.ShortTimeFormat := AShortTimeFormat;
    FormatSettings.DecimalSeparator := '.';
  {$ELSE}
    OldDateSeparator := DateSeparator;
    OldDateFormat := ShortDateFormat;
    OldTimeSeparator := TimeSeparator;
    OldTimeFormat := LongTimeFormat;
    OldDecimalSeparator := DecimalSeparator;
    DateSeparator := ADateSeparator;
    ShortDateFormat := AShortDateFormat;
    TimeSeparator := ATimeSeparator;
    ShortTimeFormat := AShortTimeFormat;
    DecimalSeparator := '.';
  {$ENDIF}
  end;

  procedure RestoreSeparators;
  begin
  {$IFDEF USE_FORMATSETTINGS}
    FormatSettings.DateSeparator := OldDateSeparator;
    FormatSettings.ShortDateFormat := OldDateFormat;
    FormatSettings.LongTimeFormat := OldTimeFormat;
    FormatSettings.TimeSeparator := OldTimeSeparator;
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  {$ELSE}
    DateSeparator := OldDateSeparator;
    ShortDateFormat := OldDateFormat;
    LongTimeFormat := OldTimeFormat;
    TimeSeparator := OldTimeSeparator;
    DecimalSeparator := OldDecimalSeparator;
  {$ENDIF}
  end;

begin
  SetSeparators;
  try
    Result := StrToSQLTimeStampOffset(Value);
  finally
    RestoreSeparators;
  end;
end;

function FractionsToText(Fractions: Cardinal; Scale: Word): string;
var
  i: Integer;
begin
  if Fractions > 0 then begin
    Result := IntToStr(Fractions);
    for i := 1 to Scale - Length(Result) do
      Result := '0' + Result;
    i := Length(Result);
    while (i > 0) and (Result[i] = '0') do
      Dec(i);
    Result := Copy(Result, 1, i);
    Result := '.' + Result;
  end
  else
    Result := '';
end;

function TimezoneToText(TimeZoneHour, TimeZoneMinute: SmallInt; TimeSeparator: Char): string;
var
  s: string;
begin
  if (TimeZoneHour <> 0) or (TimeZoneMinute <> 0) then begin
    Result := IntToStr(TimeZoneHour);
    Result := StringOfChar('0', 2 - Length(Result)) + Result;
    s := IntToStr(TimeZoneMinute);
    Result := Result + TimeSeparator + StringOfChar('0', 2 - Length(s)) + s;
    if TimeZoneHour >= 0 then
      Result := '+' + Result
    else
      Result := '-' + Result;
  end
  else
    Result := '';
end;

{ TSQLiteConnection }

constructor TSQLiteConnection.Create;
begin
  inherited;

  FBusyTimeout := DefValBusyTimeout;
  FEnableSharedCache := False;
  FEnableLoadExtension := False;
  FEncryptionKey := '';
{$IFDEF LITE}
  FNewEncryptionKey := '';
{$ENDIF}
  FReadUncommitted := False;
  FLiteTypes := nil;
  FForeignKeys := True;
  FDefaultCollations := True;
{$IFNDEF LITE}
  FCollationManager := nil;
  FFunctionManager := nil;
{$ENDIF}
  FStaticLibrary := DefValDirect;
  FForceCreateDatabase := False;
  FNativeDate := True;
  FConnectMode := DefValConnectMode;
  FLockingMode := DefValLockingMode;
  FSynchronous := DefValSynchronous;
  FJournalMode := DefValJournalMode;
  FAPI := TSQLite3API.Create;
  FCipherLicense := '';
end;

destructor TSQLiteConnection.Destroy;
begin
  Disconnect;
{$IFNDEF LITE}
  FCollationManager.Free;
  FFunctionManager.Free;
{$ENDIF}
  FLiteTypes.Free;
  FAPI.Free;

  inherited;
end;

procedure TSQLiteConnection.SetASCIIDataBase(const Value: boolean);
begin
  if FASCIIDataBase <> Value then
    Disconnect;

  FASCIIDataBase := Value;
end;

procedure TSQLiteConnection.SetBusyTimeout(const Value: integer);
begin
  FBusyTimeout := Value;
  if FConnected then
    InternalSetBusyTimeout(FBusyTimeout);
end;

procedure TSQLiteConnection.SetReadUncommitted(const Value: boolean);
begin
  FReadUncommitted := Value;
  if FConnected then
    InternalSetReadUncommitted(FReadUncommitted);
end;

procedure TSQLiteConnection.SetUseStaticLibrary(const Value: boolean);
begin
{$IFNDEF NOSTATIC}
  if Value <> FStaticLibrary then begin
    Disconnect;

    if FAPI.Direct <> Value then
      if FAPI.Initialized then
        FAPI.UnInitialize;

    FStaticLibrary := Value;

{$IFDEF CODEC}
    if not Value then
      if EncryptionAlgorithm <> DefaultEncryptionAlgorithm then
        SetProp(prEncryptionAlgorithm, DefaultEncryptionAlgorithm);
{$ENDIF}
  end;
{$ELSE}
  if Value then begin
    FStaticLibrary := False;
    raise Exception.Create(SDirectNotSupported);
  end;
{$ENDIF}
end;

procedure TSQLiteConnection.SetConnectMode(const Value: TConnectMode);
begin
  if FConnectMode <> Value then
    Disconnect;

  FConnectMode := Value;
end;

procedure TSQLiteConnection.SetLockingMode(const Value: TLockingMode);
begin
  FLockingMode := Value;
  if FConnected then
    InternalSetLockingMode(FLockingMode);
end;

procedure TSQLiteConnection.SetSynchronous(const Value: TSynchronous);
begin
  FSynchronous := Value;
  if FConnected then
    InternalSetSynchronous(FSynchronous);
end;

procedure TSQLiteConnection.SetJournalMode(const Value: TJournalMode);
begin
  FJournalMode := Value;
  if FConnected then
    InternalSetJournalMode(FJournalMode);
end;

{$IFNDEF LITE}
procedure TSQLiteConnection.SetDefaultCollations(const Value: boolean);
begin
  FDefaultCollations := Value;
  if FConnected then
    InternalSetDefaultCollations(Value);
end;
{$ENDIF}

procedure TSQLiteConnection.SetForeignKeys(const Value: boolean);
begin
  FForeignKeys := Value;
  if FConnected then
    InternalSetForeignKeys(FForeignKeys);
end;

procedure TSQLiteConnection.InternalSetBusyTimeout(const Value: integer);
begin
  Check(FAPI.sqlite3_busy_timeout(FAPI.SQLite, Value));
end;

procedure TSQLiteConnection.InternalSetReadUncommitted(const Value: boolean);
begin
  if Value then
    ExecuteSQL('PRAGMA read_uncommitted = true')
  else
    ExecuteSQL('PRAGMA read_uncommitted = false');
end;

{$IFNDEF LITE}
procedure TSQLiteConnection.InternalSetDefaultCollations(const Value: boolean);
begin
  if Value then
    GetCollationManager.RegisterDefaultCollations
  else
    GetCollationManager.UnRegisterDefaultCollations
end;
{$ENDIF}

procedure TSQLiteConnection.InternalSetForeignKeys(const Value: boolean);
begin
  if Value then
    ExecuteSQL('PRAGMA foreign_keys = ON')
  else
    ExecuteSQL('PRAGMA foreign_keys = OFF');
end;

procedure TSQLiteConnection.InternalSetLockingMode(const Value: TLockingMode);
var
  Mode: string;
begin
  case Value of
    lmNormal:
      Mode := 'NORMAL';
    lmExclusive:
      Mode := 'EXCLUSIVE';
  end;
  ExecuteSQL('PRAGMA locking_mode = ' + Mode);
end;

procedure TSQLiteConnection.InternalSetSynchronous(const Value: TSynchronous);
var
  Mode: string;
begin
  case Value of
    smOff:
      Mode := 'OFF';
    smNormal:
      Mode := 'NORMAL';
    smFull:
      Mode := 'FULL';
    smExtra:
      Mode := 'EXTRA';
  end;
  ExecuteSQL('PRAGMA synchronous = ' + Mode);
end;

procedure TSQLiteConnection.InternalSetJournalMode(const Value: TJournalMode);
var
  Mode: string;
begin
  case Value of
    jmDelete:
      Mode := 'DELETE';
    jmTruncate:
      Mode := 'TRUNCATE';
    jmPersist:
      Mode := 'PERSIST';
    jmMemory:
      Mode := 'MEMORY';
    jmWAL:
      Mode := 'WAL';
    jmOff:
      Mode := 'OFF';
  end;
  ExecuteSQL('PRAGMA journal_mode = ' + Mode);
end;


{$IFNDEF LITE}
procedure TSQLiteConnection.InternalCos(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Cos(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalSin(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Sin(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalTan(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Tan(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalCot(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Cot(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalAcos(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := ArcCos(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalAsin(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := ArcSin(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalAtan(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := ArcTan(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalAtan2(InValues: array of Variant; var ResultValue: Variant);
begin
  if not (VarIsNull(InValues[0]) or VarIsNull(InValues[1])) then
    ResultValue := ArcTan2(InValues[0], InValues[1])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalDegrees(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := RadToDeg(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalRadians(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := DegToRad(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalTruncate(InValues: array of Variant; var ResultValue: Variant);
begin
  if not (VarIsNull(InValues[0]) or VarIsNull(InValues[1])) then
    ResultValue := Trunc(double(InValues[0]) * Power(10, InValues[1])) / Power(10, InValues[1])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalRound(InValues: array of Variant; var ResultValue: Variant);
begin
  if not (VarIsNull(InValues[0]) or VarIsNull(InValues[1])) then
    ResultValue := Round(double(InValues[0]) * Power(10, InValues[1])) / Power(10, InValues[1])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalCeiling(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Ceil(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalFloor(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Floor(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalMod(InValues: array of Variant; var ResultValue: Variant);
begin
  if not (VarIsNull(InValues[0]) or VarIsNull(InValues[1])) then
    ResultValue := InValues[0] mod InValues[1]
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalPower(InValues: array of Variant; var ResultValue: Variant);
begin
  if not (VarIsNull(InValues[0]) or VarIsNull(InValues[1])) then
    ResultValue := Power(InValues[0], InValues[1])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalSqrt(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Sqrt(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalSign(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Sign(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalRand(InValues: array of Variant; var ResultValue: Variant);
begin
  ResultValue := Random;
end;

procedure TSQLiteConnection.InternalExp(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Exp(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalLog(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Ln(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.InternalLog10(InValues: array of Variant; var ResultValue: Variant);
begin
  if not VarIsNull(InValues[0]) then
    ResultValue := Log10(InValues[0])
  else
    ResultValue := null;
end;

procedure TSQLiteConnection.RegisterInternalFunctions;
var
  FunctionManager: {$IFNDEF CB6}TSQLiteFunctionManager{$ELSE}TObject{$ENDIF};
begin
  FunctionManager := GetFunctionManager;

  TSQLiteFunctionManager(FunctionManager).RegisterFunction('COS', 1, InternalCos);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('SIN', 1, InternalSin);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('TAN', 1, InternalTan);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('COT', 1, InternalCot);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('ACOS', 1, InternalAcos);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('ASIN', 1, InternalAsin);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('ATAN', 1, InternalAtan);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('ATAN2', 2, InternalAtan2);

  TSQLiteFunctionManager(FunctionManager).RegisterFunction('DEGREES', 1, InternalDegrees);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('RADIANS', 1, InternalRadians);

  TSQLiteFunctionManager(FunctionManager).RegisterFunction('TRUNCATE', 2, InternalTruncate);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('ROUND', 2, InternalRound);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('CEILING', 1, InternalCeiling);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('FLOOR', 1, InternalFloor);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('MOD', 2, InternalMod);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('POWER', 2, InternalPower);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('SQRT', 1, InternalSqrt);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('SIGN', 1, InternalSign);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('RAND', 0, InternalRand);

  TSQLiteFunctionManager(FunctionManager).RegisterFunction('EXP', 1, InternalExp);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('LOG', 1, InternalLog);
  TSQLiteFunctionManager(FunctionManager).RegisterFunction('LOG10', 1, InternalLog10);

  Randomize;
end;
{$ENDIF}

function TSQLiteConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prClientLibrary:
      FClientLibrary := Value;
    prUseUnicode:
      FUseUnicode := Value;
    prASCIIDataBase:
      SetASCIIDataBase(Value);
    prEncryptionKey:
      FEncryptionKey := Value;
{$IFDEF LITE}
    prNewEncryptionKey: begin
      FChangeEncryptionKey := True;
      FNewEncryptionKey := Value;
    end;
{$ENDIF}
    prEnableSharedCache:
      FEnableSharedCache:= Value;
    prEnableLoadExtension:
      FEnableLoadExtension := Value;
    prBusyTimeout:
      SetBusyTimeout(Value);
    prReadUncommitted:
      SetReadUncommitted(Value);
{$IFNDEF LITE}
    prDefaultCollations:
      SetDefaultCollations(Value);
{$ENDIF}
    prDateFormat:
      FDateFormat := Value;
    prTimeFormat:
      FTimeFormat := Value;
    prNativeDate:
      FNativeDate := Value;
    prForeignKeys:
      SetForeignKeys(Value);
    prStaticLibrary:
      SetUseStaticLibrary(Value);
    prForceCreateDatabase:
      FForceCreateDatabase := Value;
    prEncryptionAlgorithm:
  {$IFDEF CODEC}
      FEncryptionAlgorithm := Value;
  {$ELSE}
      FEncryptionAlgorithm := DefaultEncryptionAlgorithm;
  {$ENDIF}
    prConnectMode:
      SetConnectMode(Value);
    prCipherLicense:
      FCipherLicense := Value;
    prLockingMode:
      SetLockingMode(Value);
    prSynchronous:
      SetSynchronous(Value);
    prJournalMode:
      SetJournalMode(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TSQLiteConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prClientLibrary:
      Value := FClientLibrary;
    prUseUnicode:
      Value := FUseUnicode;
    prASCIIDataBase:
      Value := FASCIIDataBase;
    prLastInsertId:
      Value := FLastInsertId;
    prEnableSharedCache:
      Value := FEnableSharedCache;
    prEnableLoadExtension:
      Value := FEnableLoadExtension;
    prEncryptionKey:
      Value := FEncryptionKey;
{$IFDEF LITE}
    prNewEncryptionKey:
      Value := FNewEncryptionKey;
{$ENDIF}
    prBusyTimeout:
      Value := FBusyTimeout;
    prReadUncommitted:
      Value := FReadUncommitted;
    prDefaultCollations:
      Value := FDefaultCollations;
    prDateFormat:
      Value := FDateFormat;
    prTimeFormat:
      Value := FTimeFormat;
    prNativeDate:
      Value := FNativeDate;
    prForeignKeys:
      Value := FForeignKeys;
    prStaticLibrary:
      Value := FStaticLibrary;
    prForceCreateDatabase:
      Value := FForceCreateDatabase;
    prEncryptionAlgorithm:
      Value := FEncryptionAlgorithm;
    prConnectMode:
      Value := FConnectMode;
    prCipherLicense:
      Value := FCipherLicense;
    prLockingMode:
      Value := FLockingMode;
    prSynchronous:
      Value := FSynchronous;
    prJournalMode:
      Value := FJournalMode;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TSQLiteConnection.Check(ErrorCode: integer);
var
  liteErrorCode: integer;
  ErrorMsg: string;
begin
  if ErrorCode = 0 then
    Exit;

  liteErrorCode := DetectError(ErrorCode, ErrorMsg);
  ProcessError(liteErrorCode, ErrorMsg, Component);
end;

function TSQLiteConnection.DetectError(ErrorCode: integer; out ErrorMsg: string): integer;
begin
  // check if SQLite can return error
  FAPI.GetLiteErrorCode(Result);
  if Result <> 0 then
    // if SQLite can return error then use standard method
    FAPI.GetLiteErrorMsg(ErrorMsg)
  else begin
    // if SQLite cannot return error then try get internal error code and msg
    Result := ErrorCode;
    FAPI.GetPredefinedErrorMsg(Result, ErrorMsg);
  end;
end;

procedure TSQLiteConnection.ProcessError(ErrorCode: integer; ErrorMsg: string; Component: TObject);
var
  Error: ESQLiteError;
  Fail, NeedFreeError: boolean;
begin
  NeedFreeError := True;
  Error := ESQLiteError.Create(ErrorCode, ErrorMsg);
  try
  {$IFNDEF NODBACCESS}
    Error.Component := Component;
  {$ENDIF}
    Fail := True;
    DoError(Error, Fail);
    if Fail then
      NeedFreeError := False;
  finally
    if NeedFreeError then
      Error.Free;
  end;

  if Fail then
    raise Error
  else
    Abort;
end;

procedure TSQLiteConnection.Connect(const ConnectString: string);
var
  OpenMode: integer;
  pKey: PAnsiChar;

  function GetCharLength: integer;
  {$IFNDEF FPC}{$IFNDEF LINUX}
  var
    cp: word;
  {$ENDIF}{$ENDIF}
  begin
    Result := 1;

    if not FUseUnicode then begin
    {$IFDEF FPC}
      Result := 4;
    {$ELSE}
    {$IFDEF LINUX}
      Result := 4;
    {$ELSE}
      cp := GetACP;
      case cp of
        CP_BIG5,
        CP_GBK,
        CP_SJIS,
        CP_UHC: Result := 2;
        CP_EUC_JP,
        CP_EUC_JIS_2004,
        CP_EUC_CN,
        CP_EUC_KR,
        CP_EUC_TW,
        CP_JOHAB: Result := 3;
        CP_GB18030,
        CP_UTF_8: Result := 4;
      end;
    {$ENDIF}
    {$ENDIF}
    end;
  end;

begin
  if FConnected then
    Exit;

{$IFNDEF NOSTATIC}
{$IFDEF CODEC}
  if FStaticLibrary then begin
    if FEncryptionAlgorithm = DefaultEncryptionAlgorithm then
      if FEncryptionKey <> '' then
        raise Exception.Create(SEncryptionKeyMustBeEmpty);
  end;
{$ENDIF}
{$ENDIF}

  try
    if not FAPI.Initialized then begin
    {$IFNDEF NOSTATIC}
      FAPI.Direct := UseStaticLibrary;
    {$ELSE}
      FAPI.Direct := False;
    {$ENDIF}
      FAPI.ClientLibrary := FClientLibrary;
      FAPI.Initialize;
    end;

    FAPI.SQLite := nil;
    try
      case FConnectMode of
        cmReadWrite:
          OpenMode := SQLITE_OPEN_READWRITE;
        cmReadOnly:
          OpenMode := SQLITE_OPEN_READONLY;
      else
        OpenMode := SQLITE_OPEN_READWRITE
      end;

      if FForceCreateDatabase then
        OpenMode := OpenMode or SQLITE_OPEN_CREATE;

      if FEnableSharedCache then
        OpenMode := OpenMode or SQLITE_OPEN_SHAREDCACHE
      else
        OpenMode := OpenMode or SQLITE_OPEN_PRIVATECACHE;

      OpenMode := OpenMode or SQLITE_OPEN_URI;

      Check(FAPI.sqlite3_open_v2(PAnsiChar(CRFunctions.UTF8Encode(WideString(FDatabase))), FAPI.SQLite, OpenMode, nil));

    {$IFNDEF NOSTATIC}
      if UseStaticLibrary then
        Limit(SQLITE_LIMIT_EXPR_DEPTH, DEF_LIMIT_EXPR_DEPTH);
    {$ENDIF}

      if not FStaticLibrary and (FCipherLicense <> '') then
        ExecuteSQL('PRAGMA cipher_license=''' + FCipherLicense + '''')  ;

      if FEncryptionKey <> '' then begin
      {$IFNDEF NEXTGEN}
        pKey := Marshal.StringToHGlobalAnsi(AnsiString(FEncryptionKey));
      {$ELSE}
        pKey := AnsiString(FEncryptionKey).Ptr;
      {$ENDIF}

        try
        {$IFNDEF NOSTATIC}
          if FStaticLibrary then begin
          {$IFNDEF CODEC}
            NoStaticEncryption;
          {$ENDIF}
          end;
        {$ENDIF}
          if FStaticLibrary then
            ExecuteSQL('PRAGMA encryption=' + LiteEncryptionAlgorithm[FEncryptionAlgorithm]);
          Check(FAPI.sqlite3_key(FAPI.SQLite, pKey, Length(FEncryptionKey)));

        finally
        {$IFNDEF NEXTGEN}
          Marshal.FreeCoTaskMem(pKey);
        {$ENDIF}
        end;
      end;

      // extended error codes are supported since sqlite ver. 3.3.8
      Check(FAPI.sqlite3_extended_result_codes(FAPI.SQLite, 1));
      if FEnableLoadExtension then
        Check(FAPI.sqlite3_enable_load_extension(FAPI.SQLite, integer(FEnableLoadExtension)));

    {$IFNDEF LITE}
      try
        ExecuteSQL('select * from sqlite_master');
      except
        on e: ESQLiteError do begin
          if not ((e.ErrorCode = SQLITE_BUSY) or (e.ErrorCode = SQLITE_LOCKED) or (e.ErrorCode = SQLITE_LOCKED_SHAREDCACHE)) then
            raise;
        end
        else
          raise;
      end;
    {$ELSE}
    {$IFDEF DBX40}
      ExecuteSQL('select * from sqlite_master');
    {$ENDIF}
    {$ENDIF}

  {$IFDEF LITE}
    if FChangeEncryptionKey then
      try
        EncryptDatabase(FNewEncryptionKey);
        FEncryptionKey := FNewEncryptionKey;
      {$IFDEF CODEC}
        if FAPI.Direct then begin
          Sqlite3CodecSetKey(FAPI.SQLite, 0, PAnsiChar(AnsiString(FEncryptionKey)), True);
          Sqlite3CodecSetKey(FAPI.SQLite, 0, PAnsiChar(AnsiString(FEncryptionKey)), False);
        end;
      {$ENDIF}
      finally
        FChangeEncryptionKey := false;
        FNewEncryptionKey := '';
      end;
  {$ENDIF}

      FConnected := True;
      FCharLength := GetCharLength;

      InternalSetLockingMode(FLockingMode);
      InternalSetSynchronous(FSynchronous);
      InternalSetJournalMode(FJournalMode);
      InternalSetBusyTimeout(FBusyTimeout);
      InternalSetReadUncommitted(FReadUncommitted);
      InternalSetForeignKeys(FForeignKeys);
    {$IFNDEF LITE}
      if FDefaultCollations then
        InternalSetDefaultCollations(FDefaultCollations);
    {$ENDIF}
      FNativeConnection := True;

    {$IFNDEF LITE}
      RegisterInternalFunctions;
      GetFunctionManager.RegistrAllFunctions;
    {$ENDIF}

      inherited;

    except
      on EFailOver do;
      else begin
        if FAPI.SQLite <> nil then begin
          FAPI.sqlite3_close(FAPI.SQLite);
          FAPI.SQLite := nil;
        end;
        FConnected := False;
        raise;
      end;
    end;

  except
    raise;
  end;
end;

procedure TSQLiteConnection.Disconnect;
begin
  if not FConnected then
    Exit;

  FConnected := False;
  try
    if FNativeConnection then
      Check(FAPI.sqlite3_close(FAPI.SQLite));
  finally
    FAPI.SQLite := nil;
    FNativeConnection := True;
  end;

  inherited;
end;

procedure TSQLiteConnection.Ping;
begin
  ExecuteSQL('SELECT 1');
end;

function TSQLiteConnection.CheckIsValid: boolean;
begin
  Result := True;
end;

procedure TSQLiteConnection.BreakExec;
begin
  if FConnected then
    FAPI.sqlite3_interrupt(FAPI.SQLite);
end;

procedure TSQLiteConnection.Assign(Source: TCRConnection);
var
  Src: TSQLiteConnection;
begin
  inherited;

  Src := TSQLiteConnection(Source);
  FDatabase := Src.FDatabase;
  FClientLibrary := Src.FClientLibrary;
  FASCIIDataBase := Src.FASCIIDataBase;
  FBusyTimeout := Src.FBusyTimeout;
  FDefaultCollations := Src.FDefaultCollations;
  FEncryptionKey := Src.FEncryptionKey;
  FEnableSharedCache := Src.FEnableSharedCache;
  FEnableLoadExtension := Src.FEnableLoadExtension;;
  FReadUncommitted := Src.FReadUncommitted;
  FUseUnicode := Src.FUseUnicode;
  FForeignKeys := Src.FForeignKeys;
  FAPI.UnInitialize;
  FAPI.Assign(Src.FAPI);
  FAPI.SQLite := nil;
  FCipherLicense := Src.FCipherLicense;
  FLockingMode := Src.FLockingMode;
  FSynchronous := Src.FSynchronous;
  FJournalMode := Src.FJournalMode;
end;

procedure TSQLiteConnection.AssignConnect(Source: TCRConnection);
var
  Src: TSQLiteConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TSQLiteConnection(Source);
      Assign(Src);
      FAPI.Assign(Src.FAPI);
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := FAPI.SQLite <> nil;
      FNativeConnection := False;
    end;
  end;
end;

function TSQLiteConnection.GetServerVersion: string;
begin
  Result := GetClientVersion;
end;

function TSQLiteConnection.GetServerVersionFull: string;
begin
  Result := GetClientVersion;
{$IFNDEF NOSTATIC}
  if UseStaticLibrary then
    Result := Result + ' (direct access)';
{$ENDIF}
end;

function TSQLiteConnection.GetClientVersion: string;
var
  pVersion: PAnsiChar;
begin
  if not FAPI.Initialized then
    FAPI.Initialize;

  pVersion := FAPI.sqlite3_libversion;
  Result := string(AnsiString(pVersion));
end;

function TSQLiteConnection.CanChangeDatabase: boolean;
begin
  Result := False;
end;

function TSQLiteConnection.GetTypes: TSQLiteTypes;
begin
  if FLiteTypes = nil then
    FLiteTypes := TSQLiteTypes.Create(self);
  Result := FLiteTypes;
end;

{$IFNDEF LITE}
function TSQLiteConnection.GetCollationManager: TSQLiteCollationManager;
begin
  if FCollationManager = nil then
    FCollationManager := TSQLiteCollationManager.Create(self);

  Result := FCollationManager;
end;

function TSQLiteConnection.GetFunctionManager: TSQLiteFunctionManager;
begin
  if FFunctionManager = nil then
    FFunctionManager := GetFunctionManagerClass.Create(self);

  Result := TSQLiteFunctionManager(FFunctionManager);
end;
{$ENDIF}

function TSQLiteConnection.GetInternalCommandClass: TCRCommandClass;
begin
  Result := TSQLiteCommand;
end;

function TSQLiteConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TSQLiteCommand;
end;

function TSQLiteConnection.GetDatabaseReadOnly(const DatabaseName: string): integer;
var
  TempDatabaseName: string;
  pDatabaseName: PAnsiChar;
begin
  TempDatabaseName := DatabaseName;
  if TempDatabaseName = '' then
    TempDatabaseName := 'main';

{$IFNDEF NEXTGEN}
  pDatabaseName := Marshal.StringToHGlobalAnsi(AnsiString(TempDatabaseName));
{$ELSE}
  pDatabaseName := AnsiString(TempDatabaseName).Ptr;
{$ENDIF}
  try
    Result := API.sqlite3_db_readonly(API.SQLite, pDatabaseName);
  finally
  {$IFNDEF NEXTGEN}
    Marshal.FreeCoTaskMem(pDatabaseName);
  {$ENDIF}
  end;
end;

function TSQLiteConnection.Limit(Id: integer; NewVal: integer): integer;
begin
  Result := API.sqlite3_limit(API.SQLite, id, newVal);
end;

function TSQLiteConnection.NormalizeTableName(const TableName: string): string;
begin
  Result := TableName;
  if SQLInfo.IsQuoted(Result) then
    Result := SQLInfo.UnQuote(Result);
  if SQLInfo.QuotesNeeded(Result) then
    Result := SQLInfo.Quote(TableName);
  if not SQLInfo.IsQuoted(Result) and ((LiteKeywordLexems.IndexOf(AnsiUpperCase(Result)) >= 0) or (Pos('.', Result) > 0)) then
    Result := SQLInfo.Quote(Result);
end;

function TSQLiteConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TSQLiteRecordSet;
end;

{$IFNDEF LITE}

function TSQLiteConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TSQLiteLoader;
end;

function TSQLiteConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TSQLiteMetaData;
end;

function TSQLiteConnection.GetFunctionManagerClass: TSQLiteFunctionManagerClass;
begin
  Result := TSQLiteFunctionManager;
end;

class function TSQLiteConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TSQLiteCommand.GetMapRulesClass;
end;

{$ENDIF}

function TSQLiteConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TSQLiteTransaction;
end;

function TSQLiteConnection.IsUnicodeDataBase: boolean;
begin
  Result := FUseUnicode or not FASCIIDataBase;
end;

function TSQLiteConnection.EncodeString(AString: AnsiString): AnsiString;
begin
  Result := EncodeString(AString, IsUnicodeDataBase);
end;

function TSQLiteConnection.EncodeString(AString: AnsiString; UseUTF8: boolean): AnsiString;
begin
  if UseUTF8 then
    Result := CRFunctions.UTF8Encode(WideString(AString))
  else
    Result := AString;
end;

function TSQLiteConnection.EncodeString(WString: WideString): AnsiString;
begin
  Result := EncodeString(WString, IsUnicodeDataBase);
end;

function TSQLiteConnection.EncodeString(WString: WideString; UseUTF8: boolean): AnsiString;
begin
  if UseUTF8 then
    Result := CRFunctions.UTF8Encode(WString)
  else
    Result := AnsiString(WString);
end;

function TSQLiteConnection.DecodeString(pUtf8String: PAnsiChar): string;
begin
{$IFDEF VER12P}
  Result := DecodeStringW(pUtf8String, IsUnicodeDataBase);
{$ELSE}
  Result := DecodeStringA(pUtf8String, IsUnicodeDataBase);
{$ENDIF}
end;

function TSQLiteConnection.DecodeStringA(pUtf8String: PAnsiChar; UseUTF8: boolean): AnsiString;
{$IFNDEF IS_UTF8}
var
  Len1, Len2: Integer;
  TempWideStr: WideString;
{$ENDIF}
begin
{$IFNDEF IS_UTF8}
  if UseUTF8 then begin
    Len1 := Length(pUtf8String);
    if Len1 > 0 then begin
      SetLength(TempWideStr, Len1);
      Len2 := UnicodeFromLocaleChars(CP_UTF8, 0, pUtf8String, Len1, PWideChar(TempWideStr), Len1);
      SetLengthA(Result, Len1);
      Len2 := LocaleCharsFromUnicode({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, PWideChar(TempWideStr), Len2, PAnsiChar(Result), Len1, nil, nil);
      if Len2 <> Len1 then
        SetLengthA(Result, Len2);
    end
    else
      Result := '';
  end
  else
{$ENDIF}
    Result := AnsiString(pUtf8String);
end;

function TSQLiteConnection.DecodeStringW(pUtf8String: PAnsiChar; UseUTF8: boolean): WideString;
var
  Len1, Len2: Integer;
begin
  if UseUTF8 then begin
    Len1 := Length(pUtf8String);
    if Len1 > 0 then begin
      SetLength(Result, Len1);
      Len2 := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.Utf8ToUnicode(PWideChar(Result), Len1 + 1, pUtf8String, Len1) - 1;
      if Len2 <> Len1 then
        SetLength(Result, Len2);
    end
    else
      Result := '';
  end
  else
    Result := WideString(AnsiString(pUtf8String));
end;

procedure TSQLiteConnection.EncryptDatabase(const NewKey: string);
var
  pKey,
  pNewKey: PAnsiChar;
begin
{$IFDEF CODEC}
  if FStaticLibrary then
    if FEncryptionAlgorithm = leDefault then
      raise Exception.Create(SDefaultAlgorithm);
{$ENDIF}

{$IFNDEF NEXTGEN}
  pKey := Marshal.StringToHGlobalAnsi(AnsiString(FEncryptionKey));
  pNewKey := Marshal.StringToHGlobalAnsi(AnsiString(NewKey));
{$ELSE}
  pKey := AnsiString(FEncryptionKey).Ptr;
  pNewKey := AnsiString(NewKey).Ptr;
{$ENDIF}

  try
{$IFNDEF NOSTATIC}
  if FStaticLibrary then begin
  {$IFNDEF CODEC}
    NoStaticEncryption;
  {$ENDIF}
  end;
{$ENDIF}
  ExecuteSQL('PRAGMA encryption=' + LiteEncryptionAlgorithm[FEncryptionAlgorithm]);
  Check(FAPI.sqlite3_key(FAPI.SQLite, pKey, Length(FEncryptionKey)));
  Check(FAPI.sqlite3_rekey(FAPI.SQLite, pNewKey, Length(NewKey)));

  finally
  {$IFNDEF NEXTGEN}
    Marshal.FreeCoTaskMem(pKey);
    Marshal.FreeCoTaskMem(pNewKey);
  {$ENDIF}
  end;
end;

procedure TSQLiteConnection.ReleaseDatabaseMemory;
begin
  Check(FAPI.sqlite3_db_release_memory(FAPI.SQLite));
end;

procedure TSQLiteTransaction.StartTransaction;
var
  CommandText: string;
  Connection: TSQLiteConnection;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  Connection := TSQLiteConnection(FConnections[0]);

  if not Connection.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);

  CommandText := 'BEGIN ';

  case FIsolationLevel of
    ilReadUncommitted, ilReadCommitted, ilRepeatableRead:
      CommandText := CommandText + 'DEFERRED';
    ilIsolated, ilSnapshot:
      CommandText := CommandText + 'IMMEDIATE';
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;

  Connection.ExecuteSQL(CommandText);

  FActive := True;
  FNativeTransaction := True;
end;

procedure TSQLiteTransaction.Commit;
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  if FNativeTransaction then
    Connection.ExecuteSQL('COMMIT');

  FActive := False;
end;

procedure TSQLiteTransaction.Rollback;
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  if FNativeTransaction then
    Connection.ExecuteSQL('ROLLBACK');

  FActive := False;
end;

procedure TSQLiteTransaction.Savepoint(const Name: string);
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  Connection.ExecuteSQL('SAVEPOINT ' + Name);
end;

procedure TSQLiteTransaction.ReleaseSavepoint(const Name: string);
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  Connection.ExecuteSQL('RELEASE SAVEPOINT ' + Name);
end;

procedure TSQLiteTransaction.RollbackToSavepoint(const Name: string);
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  Connection.ExecuteSQL('ROLLBACK TO SAVEPOINT ' + Name);
end;

{ TSQLiteTypes }

constructor TSQLiteTypes.Create(Connection: TSQLiteConnection);
begin
  inherited Create;

  FConnection := Connection;
end;

destructor TSQLiteTypes.Destroy;
begin
  inherited;
end;

{$IFDEF LITE}

procedure TSQLiteTypes.GetTypeAttr(DataType: Integer; TypeModifier: Integer;
  var Length, Scale: integer);
begin
  Length := 0;
  Scale := 0;

  case DataType of
    dtString:
      if TypeModifier <> -1 then
        Length := TypeModifier - 4;
  end;
end;

function TSQLiteTypes.GetInternalType(TypeCode: integer; var ObjectType: TObjectType): word;
begin
  ObjectType := nil;
  case TypeCode of
    SQLITE_INTEGER:
      Result := dtInteger;
    SQLITE_FLOAT:
      Result := dtFloat;
    SQLITE_TEXT:
      Result := dtString;
    SQLITE_BLOB:
      Result := dtBlob;
  else
    Result := dtUnknown;
  end;
end;

procedure TSQLiteTypes.DetectDataType(TypeOID, TypeModifier: integer;
  var DataType, SubDataType: word; var Length, Scale, Size: integer;
  var Fixed: boolean; var ObjectType: TObjectType;
  LongStrings, FlatBuffers, EnableBCD, EnableFMTBCD,
  CursorAsString, FieldsAsText: boolean);
begin
  DataType := GetInternalType(TypeOID, ObjectType);
  SubDataType := 0;
  Fixed := False;

  GetTypeAttr(DataType, TypeModifier, Length, Scale);

  if FieldsAsText then begin
    SubDataType := DataType;
    DataType := dtString;
  end
  else begin
    case DataType of
      dtUnknown: begin
        SubDataType := DataType;
        DataType := dtString;
      end;
    end;
  end;

  case DataType of
    dtString: begin
      if Length = 0 then begin
        DataType := dtMemo;
      end;
      if DataType <> dtMemo then begin
        if (Length < 255) or (LongStrings and (Length <= High(Word))) then begin
          if (Length >= FlatBufferLimit) and not FlatBuffers then
            DataType := dtExtString;
        end
        else begin
          DataType := dtMemo;
          SubDataType := dtString;
        end;
      end;
      if FConnection.FUseUnicode then
        case DataType of
          dtString:
            DataType := dtWideString;
          dtExtString:
            DataType := dtExtWideString;
          dtMemo: begin
            DataType := dtWideMemo;
            if SubDataType = 0 then
              SubDataType := dtWideString;
          end;
        end;
    end;
    dtMemo:
      if FConnection.FUseUnicode then
        DataType := dtWideMemo;
  {$IFDEF LITE}
    dtLargeint:
      if not (EnableFMTBCD or FConnection.EnableFMTBCD) then begin
        DataType := dtFloat;
        SubDataType := dtLargeint;
      end;
  {$ENDIF}
  end;

  Size := TSQLiteRecordSet.GetBufferSize(DataType, Length);
end;

{$ENDIF}

function TSQLiteTypes.GetSQLiteType(DataType: word; var NeedConvertToText: boolean): word;
begin
  NeedConvertToText := False;

  case DataType of
    dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32, dtBoolean:
      Result := SQLITE_INTEGER;
    dtFloat, dtCurrency, dtBCD, dtFMTBCD:
      Result := SQLITE_FLOAT;
    dtDate: begin
      Result := SQLITE_TEXT;
      NeedConvertToText := True;
    end;
    dtTime: begin
      Result := SQLITE_TEXT;
      NeedConvertToText := True;
    end;
    dtDateTime, dtSQLTimeStamp: begin
      Result := SQLITE_TEXT;
      NeedConvertToText := True;
    end;
    dtBlob, dtBytes, dtVarBytes:
      Result := SQLITE_BLOB;
  else
    // Unknown types save as TEXT
    Result := SQLITE_TEXT;
  end;
end;

function TSQLiteTypes.GetVarType(VarType: TVarType): word;
begin
  case VarType of
    varShortInt, varByte:
      Result := dtInt8;
    varSmallInt:
      Result := dtInt16;
    varWord:
      Result := dtUInt16;
    varInteger:
      Result := dtInt32;
    varLongWord:
      Result := dtUInt32;
    varInt64:
      Result := dtInt64;
    varBoolean:
      Result := dtBoolean;
    varSingle, varDouble:
      Result := dtFloat;
    varCurrency:
      Result := dtCurrency;
    varDate:
      Result := dtDateTime;
    varArray + varByte:
      Result := dtVarBytes;
    varByRef:
      Result := dtVarBytes;
    varAny:
      Result := dtBytes;
  else
    Result := dtUnknown;
  end;
end;

function TSQLiteTypes.ConvertToText(DataType, Scale: word; Value: Variant): AnsiString;
var
  Hour, Min, Sec, FSec: word;
  val_date: TDateTime;
  val_ts: TSQLTimeStamp;
  val_tstz: TSQLTimeStampOffset;
  str_val: string;
begin
  case DataType of
    dtDate : begin
      val_date := Value;
      str_val := FormatDateTime('yyyy-mm-dd', val_date);
      Result := AnsiString(str_val);
    end;
    dtTime : begin
      val_date := Value;
      if Scale > 0 then begin
        DecodeTime(Value, Hour, Min, Sec, FSec);
        str_val := FractionsToText(FSec, Scale);
        str_val := FormatDateTime('hh'':''nn'':''ss', val_date) + str_val;
      end
      else
        str_val := FormatDateTime('hh'':''nn'':''ss', val_date);
      Result := AnsiString(str_val);
    end;
    dtDateTime: begin
      val_date := Value;
      if Scale > 0 then begin
        DecodeTime(Value, Hour, Min, Sec, FSec);
        str_val := FractionsToText(FSec, Scale);
        str_val := FormatDateTime('yyyy-mm-dd hh'':''nn'':''ss', val_date) + str_val;
      end
      else
        str_val := FormatDateTime('yyyy-mm-dd hh'':''nn'':''ss', val_date);
      Result := AnsiString(str_val);
    end;
    dtSQLTimeStamp: begin
      val_ts := VarToSQLTimeStamp(Value);
      val_date := EncodeDateTime(val_ts.Year, val_ts.Month, val_ts.Day, val_ts.Hour, val_ts.Minute, val_ts.Second, 0);
      str_val := FractionsToText(val_ts.Fractions, 3);
      str_val := FormatDateTime('yyyy-mm-dd hh'':''nn'':''ss', val_date) + str_val;
      Result := AnsiString(str_val);
    end;
    dtSQLTimeStampOffset: begin
      val_tstz := VarToSQLTimeStampOffset(Value);
      val_date := EncodeDateTime(val_tstz.Year, val_tstz.Month, val_tstz.Day, val_tstz.Hour, val_tstz.Minute, val_tstz.Second, 0);
      str_val := FractionsToText(val_tstz.Fractions, 3);
      str_val := FormatDateTime('yyyy-mm-dd hh'':''nn'':''ss', val_date) + str_val;
      str_val := str_val + ' ' + TimezoneToText(val_tstz.TimeZoneHour, val_tstz.TimeZoneMinute, ':');
      Result := AnsiString(str_val);
    end;
  else
    Result := AnsiString(VarToStr(Value));
  end;
end;

function TSQLiteTypes.ConvertBlob(Value: Variant; var DestrType: IntPtr): TBytes;
var
  Obj: TObject;
  Blob: TBlob;
  BlobData: TCRBlobData;
  SafeArray: PVarArray;
  bufPos, bufLen: Cardinal;
begin
  case VarType(Value) of
    varSharedObject: begin
      Obj := TObject(TVarData(Value).VPointer);
      if not (Obj is TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      Blob.Defrag;
      SetLength(Result, Blob.Size);
      bufPos := 0;
      BlobData := Blob.GetData;
      while bufPos < Blob.Size do begin
        bufLen := BlobData.Read(bufPos, Cardinal(Length(Result)) - bufPos, @Result[bufPos]);
        Inc(bufPos, bufLen);
      end;
//      Marshal.Copy(PtrOffset(Blob.FirstPiece, SizeOf(TPieceHeader)), Result, 0, Blob.FirstPiece.Used);
      DestrType := SQLITE_STATIC;
    end;
    varArray or varByte: begin
      SafeArray := VarArrayAsPSafeArray(Value);
      bufLen := SafeArray.Bounds[0].ElementCount;
      SetLength(Result, bufLen);
      if bufLen > 0 then
        Move(SafeArray.Data^, Result[0], bufLen);
      {
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      pBlob := VarArrayLock(Value);
      try
        Marshal.Copy(pBlob, Result, 0, hb - lb + 1);
      finally
        VarArrayUnlock(Value);
      end;
      }
      DestrType := SQLITE_TRANSIENT;
    end;
  else
    Result := Encoding.Default.GetBytes(VarToStr(Value));
    DestrType := SQLITE_TRANSIENT;
  end;
end;

function TSQLiteTypes.ConvertMemo(Value: Variant): AnsiString;
var
  ws: WideString;
  Obj: TObject;
  Blob: TBlob;
begin
  case VarType(Value) of
    varSharedObject: begin
      Obj := TObject(TVarData(Value).VPointer);
      if not (Obj is TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      ws := Blob.AsWideString;
    end;
  else
    ws := VarToWideStr(Value);
  end;

  // encode string: UTF8 or ASCII
  Result := FConnection.EncodeString(ws);
end;

{ TSQLiteCommand }

constructor TSQLiteCommand.Create;
begin
  inherited;

  FFormatSettingsInitialized := False;
  ResetFormatSettings;
end;

destructor TSQLiteCommand.Destroy;
begin

  inherited;
end;

class function TSQLiteCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TSQLiteInfo;
end;

function TSQLiteCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited SetProp(Prop, Value);
  //end;
end;

function TSQLiteCommand.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRowsProcessed:
      Value := FRowsAffected;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TSQLiteCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TSQLiteConnection(Value);
  end;
end;

class function TSQLiteCommand.GetTableInfoClass: TTableInfoClass;
begin
  Result := TSQLiteTableInfo;
end;

class function TSQLiteCommand.GetParserClass: TSQLParserClass;
begin
  Result := TLiteParser;
end;

class function TSQLiteCommand.GetParamDescClass: TParamDescClass;
begin
  Result := TSQLiteParamDesc;
end;

procedure TSQLiteCommand.BreakExec;
begin
  if GetCursorState <> csInactive then
    FConnection.BreakExec;
end;

procedure TSQLiteCommand.Prepare;
var
  NeedFreeSQL: boolean;
begin
  if FStmt <> nil then
    Exit;

  if FConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
  if Trim(FSQL) = '' then
    raise Exception.Create(SEmptySQLStatement);

  NeedFreeSQL := False;
  if FCommandLength = 0 then begin
    FCommandSQL := FConnection.EncodeString(FSQL);
    FCommandLength := LengthA(FCommandSQL);
    FPCommandSQL := Marshal.StringToHGlobalAnsi(FCommandSQL);
    FCurrentCommand := FPCommandSQL;
  end;

  try
    try
      InternalPrepare;
    except
      NeedFreeSQL := True;
      raise;
    end;
  finally
    if (FCommandLength = 0) or NeedFreeSQL then
      Marshal.FreeCoTaskMem(FPCommandSQL);
  end;

  FCursorState := csPrepared;
end;

procedure TSQLiteCommand.Unprepare;
begin
  if FStmt = nil then
    Exit;

  FConnection.FAPI.sqlite3_finalize(FStmt);
  FStmt := nil;
  FCursorState := csInactive;

  FreeCachedBuffers;
end;

function TSQLiteCommand.GetPrepared: boolean;
begin
  Result := FStmt <> nil;
end;

procedure TSQLiteCommand.Execute;
begin
  if GetCursorState > csPrepared then
    Exit;

  try
    InternalExecute;
  except
    if not FLockAfterExecute and Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;

  if not FLockAfterExecute and Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TSQLiteCommand.SetSQL(const Value: string);
begin
  inherited;

  FCommandTail := nil;
  FCommandLength := 0;
end;

function TSQLiteCommand.ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string;
var
  Param: TSQLiteParamDesc;
  Parser: TLiteParser;
  Code: integer;
  St, StNorm: string;
  AllParams: TStringList;
  InPragma: boolean;
  IsFirstLexem: boolean;
  ParamIndex: integer;
  Builder: StringBuilder;
  ParamIsQuoted: boolean;
  LastPos: Int64;
begin
  FIsCursorPragma := False;
  FParsedSQLType := qtUnknown;

  Result := SQL;

  FParamsInfo.Clear;

  Assert(Params <> nil);
  if FScanParams then
    Params.Clear;

  Parser := TLiteParser.Create(SQL);
  try
    Parser.DecSeparator := '.';
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    InPragma := False;
    IsFirstLexem := True;

    Builder := StringBuilder.Create(Length(SQL));
    try
      AllParams := TStringList.Create;
      try
        Parser.ToBegin;
        LastPos := 1;
        repeat
          Code := Parser.GetNext(St);
          if IsFirstLexem then begin
            case Code of
              lxPRAGMA:
                InPragma := True;
              lxSELECT, lxWITH:
                FParsedSQLType := qtSelect;
              lxINSERT:
                FParsedSQLType := qtInsert;
              lxUPDATE:
                FParsedSQLType := qtUpdate;
              lxDELETE:
                FParsedSQLType := qtDelete;
            end;

            if (Code <> lcBlank) and (Code <> lcComment) then
              IsFirstLexem := False;
          end;

          while (Code <> lcEnd) and (Code <> lxColon) do begin
            Code := Parser.GetNext(St);

            if InPragma and (Code <> lcEnd) then
              if pos(LowerCase(St) + ' ', CursorPragmas) > 0 then begin
                FParsedSQLType := qtCursor;
                FIsCursorPragma := True;
              end;

            if Code = lxPRAGMA then
              InPragma := True;
          end;

          if Code = lxColon then begin
            Code := Parser.GetNext(St);
            if (Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst) then begin
              ParamIsQuoted := SQLInfo.IsQuoted(St);
              if ParamIsQuoted then begin
                St := SQLInfo.UnQuote(St);
                StNorm := StringReplace(St, ' ', '_', [rfReplaceAll]);
              end;

              if FScanParams then begin
                ParamIndex := AllParams.IndexOf(St);
                if ParamIndex < 0 then begin
                  Param := TSQLiteParamDesc(AddParam);
                  Param.SetName(St);
                  AllParams.Add(St);
                end
                else
                  Param := TSQLiteParamDesc(Params[ParamIndex]);
              end
              else
                Param := TSQLiteParamDesc(Params.FindParam(St));

              AddParamPosition(St, Parser.CurrPos - Length(St), Parser.CurrPos, Param);
              if not ParamIsQuoted then begin
                Builder.Append(Copy(SQL, LastPos, Parser.CurrPos - LastPos + 1));
                if FScanParams then
                  Param.FNormalizedName := St;
              end
              else begin
                Builder.Append(Copy(SQL, LastPos, Parser.CurrPos - LastPos - Length(St) - 1));
                Builder.Append(StNorm);
                if FScanParams then
                  Param.FNormalizedName := StNorm;
              end;
              LastPos := Parser.CurrPos + 1;
            end;
          end;
        until Code = lcEnd;

        Builder.Append(Copy(SQL, LastPos, Parser.CurrPos - LastPos + 1));
      finally
        AllParams.Free;
      end;

      Result := Builder.ToString;
    finally
      Builder.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSQLiteCommand.ParseSQLType;
var
  Parser: TSQLParser;
  Code: integer;
  St: string;
begin
  FParsedSQLType := qtUnknown;

  Parser := TLiteParser.Create(FSQL);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;

    Code := Parser.ToLexem([lxCREATE, lxALTER, lxWITH, lxSELECT, lxPRAGMA, lxINSERT, lxUPDATE, lxDELETE, lxDROP]);

    case Code of
      lxSELECT, lxWITH:
        FParsedSQLType := qtSelect;
      lxINSERT:
        FParsedSQLType := qtInsert;
      lxUPDATE:
        FParsedSQLType := qtUpdate;
      lxDELETE:
        FParsedSQLType := qtDelete;
      lxCREATE:
        FParsedSQLType := qtCreate;
      lxDROP:
        FParsedSQLType := qtDrop;
      lxPRAGMA: begin
        while True do begin
          Code := Parser.GetNext(St);
          if Code = lcEnd then
            break
          else if pos(LowerCase(St) + ' ', CursorPragmas) > 0 then begin
            FParsedSQLType := qtCursor;
            break;
          end;
        end;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSQLiteCommand.InitProcParams(const Name: string; Overload: integer);
begin
  raise Exception.Create(SStoredProcNotSupported);
end;

function TSQLiteCommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;
begin
  Result := '';
  raise Exception.Create(SStoredProcNotSupported);
end;

function TSQLiteCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

{$IFNDEF LITE}
class function TSQLiteCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TLiteMapRules;
end;
{$ENDIF}

procedure TSQLiteCommand.SetCursorState(Value: TCursorState);
begin
  FCursorState := Value;
end;

procedure TSQLiteCommand.Check(ErrorCode: integer);
begin
  FConnection.Check(ErrorCode);
end;

procedure TSQLiteCommand.CheckPrepareError(ErrorCode: integer);
begin
  Check(ErrorCode);
end;

procedure TSQLiteCommand.InitFormatSettings;

  function DetectSeparator(var Format: string; DefaultSeparator: Char): Char;
  var
    i: integer;
    NewSeparator: Char;
  begin
    NewSeparator := #0;
    for i := 1 to Length(Format) do
      if CharInSet(Char(Format[i]), ['-', '.', ':', '/', '\']) then begin
        if NewSeparator = #0 then
          NewSeparator := Format[i];
        if Format[i] = NewSeparator then
          Format[i] := DefaultSeparator;
      end;

    if NewSeparator <> #0 then
      Result := NewSeparator
    else
      Result := DefaultSeparator;
  end;

begin
  ResetFormatSettings;

  if FConnection = nil then
    exit;

  if FConnection.FDateFormat <> '' then begin
    FShortDateFormat := FConnection.FDateFormat;
    FDateSeparator := DetectSeparator(FShortDateFormat, '-');
  end;

  if FConnection.FTimeFormat <> '' then begin
    FShortTimeFormat := FConnection.FTimeFormat;
    FTimeSeparator := DetectSeparator(FShortTimeFormat, ':');
  end;
end;

procedure TSQLiteCommand.ResetFormatSettings;
begin
  FDateSeparator := '-';
  FShortDateFormat := 'yyyy-mm-dd';
  FTimeSeparator := ':';
  FShortTimeFormat := 'hh:nn:ss';
end;

procedure TSQLiteCommand.InternalPrepare;
var
  Start: Cardinal;
  PrepareResult: integer;
begin
  Start := {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
  repeat
    PrepareResult := FConnection.FAPI.sqlite3_prepare_v2(FConnection.FAPI.SQLite,
      FCurrentCommand, FCommandLength, FStmt, FCommandTail);

    if (FConnection.FBusyTimeout = 0) or
       ((PrepareResult <> SQLITE_BUSY) and
        (PrepareResult <> SQLITE_LOCKED) and
        (PrepareResult <> SQLITE_LOCKED_SHAREDCACHE))
    then
      Break
    else
      Sleep(1);
  until {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF} - Start > Cardinal(FConnection.FBusyTimeout * 1000);

  if PrepareResult <> SQLITE_OK then
    CheckPrepareError(PrepareResult);

  Dec(FCommandLength, NativeUInt(FCommandTail) - NativeUInt(FCurrentCommand));
  if FCommandLength < 0 then
    FCommandLength := 0;
  while (FCommandLength > 0) and (Marshal.ReadByte(FCommandTail) <= byte(' ')) do begin
    FCommandTail := PtrOffset(FCommandTail, 1);
    Dec(FCommandLength);
  end;
end;

procedure TSQLiteCommand.InternalExecute;
var
  i: integer;
  NeedPrepare: boolean;
  HasNoSemicolon: boolean;
  Iter: integer;
  OldSQL: string;
  Start: Cardinal;
begin
  FRowsAffected := 0;
  FParamsProcessed := 0;
  Iter := 0;
  OldSQL := FSQL;

  while Iter < FBatchIters do begin
    FSQL := OldSQL;

    NeedPrepare := not GetPrepared;
    if NeedPrepare then
      Prepare;

    try
      FConnection.FAPI.sqlite3_reset(FStmt);

      BindParams(FBatchOffset + Iter);

      while True do begin
        Start := {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
        repeat
          FExecResult := FConnection.FAPI.sqlite3_step(FStmt);

          if (FConnection.FBusyTimeout = 0) or
             ((FExecResult <> SQLITE_BUSY) and
              (FExecResult <> SQLITE_LOCKED) and
              (FExecResult <> SQLITE_LOCKED_SHAREDCACHE))
          then
            Break
          else
            Sleep(1);
        until {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF} - Start > Cardinal(FConnection.FBusyTimeout * 1000);

        if not ((FExecResult = SQLITE_ROW) or (FExecResult = SQLITE_DONE)) and
           // internal error in sqlite3 when the last line of a multiline query is a comment
           not ((FExecResult = SQLITE_MISUSE) and (FCommandLength = 0))
        then
          Check(FExecResult);

        if CommandType <> ctCursor then begin
          Inc(FRowsAffected, FConnection.FAPI.sqlite3_changes(FConnection.FAPI.SQLite));
          if (FBatchIters > 1) or (FBatchOffset > 0) then
            Inc(FParamsProcessed);
        end;

        if (FCommandLength > 0) then begin
          HasNoSemicolon := False;
          for i := 0 to FCommandLength - 1 do
            if Marshal.ReadByte(FCommandTail, i) <> byte(';') then begin
              HasNoSemicolon := True;
              Break;
            end;
          if not HasNoSemicolon then
            Break;

          UnPrepare;
          FCurrentCommand := FCommandTail;
          Prepare;
          BindParams(FBatchOffset + Iter);

          DetectCommandType;
        end
        else
          Break;
      end;

      if CommandType <> ctCursor then
        FConnection.FLastInsertId := FConnection.FAPI.sqlite3_last_insert_rowid(FConnection.FAPI.SQLite)
      else
        FConnection.FLastInsertId := 0;

    finally
      if NeedPrepare then
        UnPrepare;
    end;

    Inc(Iter);
  end;
end;

procedure TSQLiteCommand.CreateBatchCommand;
begin
  // Empty
end;

function TSQLiteCommand.NeedBatchTransaction: boolean;
begin
  Result := True;
end;

function TSQLiteCommand.NeedBatchSavepoint: boolean;
begin
  Result := True;
end;

procedure TSQLiteCommand.InternalExecuteBatch(Iters, Offset: integer);
var
  NeedPrepare: boolean;
begin
  FBatchIters := Iters;
  FBatchOffset := Offset;

  NeedPrepare := not GetPrepared;
  if NeedPrepare then
    Prepare;
  try
    Execute;
  finally
    if NeedPrepare then
      UnPrepare;
  end;
end;

function TSQLiteCommand.FindParamByNormalizedName(const Name: string): TSQLiteParamDesc;
var
  i: integer;
  Param: TSQLiteParamDesc;
begin
  Result := nil;

  for i := 0 to FParams.Count - 1 do begin
    Param := TSQLiteParamDesc(FParams[i]);
    if (Param <> nil) and SameText(Param.FNormalizedName, Name) then begin
      Result := Param;
      Break;
    end;
  end;
end;

procedure TSQLiteCommand.AllocCachedBuffers;
begin
  // UTF8 can be 4 bytes per char
  if FCachedBufA = nil then
    SetLength(FCachedBufA, TempBufAnsiSize + 1); // with Ansi zero terminator
  if FCachedBufW = nil then
    SetLength(FCachedBufW, TempBufWideSize + 2); // with Wide zero terminator
end;

procedure TSQLiteCommand.FreeCachedBuffers;
begin
  SetLength(FCachedBufA, 0);
  SetLength(FCachedBufW, 0);
end;

procedure TSQLiteCommand.BindParams(Offset: integer);
var
  i, Count: integer;
  Param: TParamDesc;
  str_val: string;
  int_val: int64;
  float_val: double;
  bool_var: boolean;
  val_date: TDateTime;
  val_ts: TSQLTimeStamp;
  val_tstz: TSQLTimeStampOffset;
  ParamNull: boolean;
  ParamValuePtr: PVariant;
  ForceBindByName: boolean;
  ParamName: string;

  procedure InternalBindValue(Index: integer; DataType: Word; ParamIsNull: boolean; const Value: PVariant);
  begin
    if ParamIsNull then begin
      Check(FConnection.FAPI.sqlite3_bind_null(FStmt, Index));
      Exit;
    end;

    case DataType of
      dtInt8, dtUInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32: begin
        int_val := Value^;
        Check(FConnection.FAPI.sqlite3_bind_int64(FStmt, Index, int_val));
      end;
      dtBoolean: begin
        bool_var := Value^;
        Check(FConnection.FAPI.sqlite3_bind_int64(FStmt, Index, Int64(bool_var)));
      end;
      dtSingle, dtFloat, dtCurrency, dtBCD, dtFMTBCD: begin
        float_val := Value^;
        Check(FConnection.FAPI.sqlite3_bind_double(FStmt, Index, float_val));
      end;
      dtDate: begin
        str_val := FormatDateTime(StringReplace(FShortDateFormat, '-', FDateSeparator, [rfReplaceAll]), Value^);
      {$IFDEF VER12P}
        BindWideText(Index, str_val);
      {$ELSE}
        BindAnsiText(Index, PAnsiChar(str_val), Length(str_val));
      {$ENDIF}
      end;
      dtTime: begin
        str_val := FormatDateTime(StringReplace(FShortTimeFormat, ':', FTimeSeparator, [rfReplaceAll]), Value^);
      {$IFDEF VER12P}
        BindWideText(Index, str_val);
      {$ELSE}
        BindAnsiText(Index, PAnsiChar(str_val), Length(str_val));
      {$ENDIF}
      end;
      dtDateTime: begin
        val_date := Value^;
        str_val := FormatDateTime(StringReplace(FShortDateFormat, '-', FDateSeparator, [rfReplaceAll]) + ' ' +
                                  StringReplace(FShortTimeFormat, ':', FTimeSeparator, [rfReplaceAll]), val_date);
      {$IFDEF VER12P}
        BindWideText(Index, str_val);
      {$ELSE}
        BindAnsiText(Index, PAnsiChar(str_val), Length(str_val));
      {$ENDIF}
      end;
      dtSQLTimeStamp: begin
        val_ts := VarToSQLTimeStamp(Value^);
        val_date := EncodeDateTime(val_ts.Year, val_ts.Month, val_ts.Day, val_ts.Hour, val_ts.Minute, val_ts.Second, 0);
        str_val := FractionsToText(val_ts.Fractions, 3);
        str_val := FormatDateTime(StringReplace(FShortDateFormat, '-', FDateSeparator, [rfReplaceAll]) + ' ' +
                                  StringReplace(FShortTimeFormat, ':', FTimeSeparator, [rfReplaceAll]), val_date) + str_val;
      {$IFDEF VER12P}
        BindWideText(Index, str_val);
      {$ELSE}
        BindAnsiText(Index, PAnsiChar(str_val), Length(str_val));
      {$ENDIF}
      end;
      dtSQLTimeStampOffset: begin
        val_tstz := VarToSQLTimeStampOffset(Value^);
        val_date := EncodeDateTime(val_tstz.Year, val_tstz.Month, val_tstz.Day, val_tstz.Hour, val_tstz.Minute, val_tstz.Second, 0);
        str_val := FractionsToText(val_tstz.Fractions, 3);
        str_val := FormatDateTime(StringReplace(FShortDateFormat, '-', FDateSeparator, [rfReplaceAll]) + ' ' +
                                  StringReplace(FShortTimeFormat, ':', FTimeSeparator, [rfReplaceAll]), val_date) + str_val;
        str_val := str_val + ' ' + TimezoneToText(val_tstz.TimeZoneHour, val_tstz.TimeZoneMinute, FTimeSeparator);
      {$IFDEF VER12P}
        BindWideText(Index, str_val);
      {$ELSE}
        BindAnsiText(Index, PAnsiChar(str_val), Length(str_val));
      {$ENDIF}
      end;
      dtBlob, dtBytes, dtVarBytes:
        BindBlob(Index, Value^);
    else
      BindMemo(Index, Value^);
    end;
  end;

begin
  Count := FConnection.FAPI.sqlite3_bind_parameter_count(FStmt);
  ForceBindByName := (Count <> Params.Count) or ((ParamsInfo.Count > 0) and (Count <> ParamsInfo.Count));


  AllocCachedBuffers;

  for i := 0 to Count - 1 do begin
    if ForceBindByName then begin
      ParamName := FConnection.DecodeString(FConnection.FAPI.sqlite3_bind_parameter_name(FStmt, i + 1));
      if ParamName <> '' then begin
        Delete(ParamName, 1, 1);
        Param := FindParamByNormalizedName(ParamName);
        if Param = nil then
          raise Exception.Create(SIncorrectParameter);
      end
      else
        Param := FParams[i];
    end
    else
      Param := FParams[i];

    ParamValuePtr := Param.GetItemPtr(Offset);
    ParamNull := VarIsEmpty(ParamValuePtr^) or VarIsNull(ParamValuePtr^);

    InternalBindValue(i + 1, Param.GetDataType, ParamNull, ParamValuePtr);
  end;
end;

procedure TSQLiteCommand.BindBlob(Index: integer; const Value: variant);
var
  Obj: TObject;
  Blob: TBlob;
  BlobData: IntPtr;
  lb, hb: integer;
  ws: WideString;
{$IFNDEF VER12P}
  sa: AnsiString;
{$ENDIF}
begin
  case VarType(Value) of
    varEmpty, varNull:
      Check(FConnection.FAPI.sqlite3_bind_null(FStmt, Index));
    varSharedObject: begin
      Obj := TObject(TVarData(Value).VPointer);
      if not (Obj is TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      Blob.Defrag;
      if not Blob.IsNull then begin
        if not Blob.IsEmpty then begin
          BlobData := PtrOffset(Blob.FirstPiece, SizeOf(TPieceHeader));
          Check(FConnection.FAPI.sqlite3_bind_blob(FStmt, Index, BlobData, Blob.FirstPiece.Used, SQLITE_STATIC));
        end
        else
          Check(FConnection.FAPI.sqlite3_bind_zeroblob(FStmt, Index, 0));
      end
      else
        Check(FConnection.FAPI.sqlite3_bind_null(FStmt, Index));
    end;
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        Check(FConnection.FAPI.sqlite3_bind_blob(FStmt, Index, BlobData, hb - lb + 1,
          SQLITE_TRANSIENT));
      finally
        VarArrayUnlock(Value);
      end;
    end;
    varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: begin
      ws := VarToWideStr(Value);
      BindWideText(Index, ws);
    end;
    else begin
    {$IFDEF VER12P}
      ws := VarToStr(Value);
      BindWideText(Index, ws);
    {$ELSE}
      sa := VarToStr(Value);
      BindAnsiText(Index, PAnsiChar(sa), Length(sa));
    {$ENDIF}
    end;
  end;
end;

procedure TSQLiteCommand.BindMemo(Index: integer; const Value: variant);
var
  Obj: TObject;
  Blob: TBlob;
  ws: WideString;
{$IFNDEF NEXTGEN}
  sa: AnsiString;
{$ELSE}
  sa: TBytes;
  BlobSize: Integer;
{$ENDIF}
begin
  case VarType(Value) of
    varEmpty, varNull:
      Check(FConnection.FAPI.sqlite3_bind_null(FStmt, Index));
    varSharedObject: begin
      Obj := TObject(TVarData(Value).VPointer);
      if not (Obj is TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      if (Blob = nil) or (Blob.IsNull) then
        Check(FConnection.FAPI.sqlite3_bind_null(FStmt, Index))
      else if Blob.IsUnicode then begin
        ws := Blob.AsWideString;
        BindWideText(Index, ws);
      end
      else begin
      {$IFNDEF NEXTGEN}
        sa := Blob.AsAnsiString;
        BindAnsiText(Index, PAnsiChar(sa), Length(sa));
      {$ELSE}
        BlobSize := Blob.Size;
        SetLength(sa, BlobSize + 1);
        Blob.Read(0, BlobSize, @sa[0]);
        sa[BlobSize] := 0; // zero terminator
        BindAnsiText(Index, @sa[0], BlobSize)
      {$ENDIF}
      end;
    end;
    varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: begin
      ws := VarToWideStr(Value);
      BindWideText(Index, ws);
    end;
    else begin
    {$IFDEF VER12P}
      ws := VarToStr(Value);
      BindWideText(Index, ws);
    {$ELSE}
      sa := VarToStr(Value);
      BindAnsiText(Index, PAnsiChar(sa), Length(sa));
    {$ENDIF}
    end;
  end;
end;

procedure TSQLiteCommand.BindAnsiText(Index: integer; pAStr: PAnsiChar; Len: Integer);
{$IFNDEF IS_UTF8}
var
  n: Integer;
  TempBufW: TBytes;
  TempBufA: TBytes;
{$ENDIF}
begin
{$IFNDEF IS_UTF8}
  if FConnection.IsUnicodeDataBase then begin
    if Len <= TempBufCharSize then begin
      TempBufA := FCachedBufA;
      if Len > 0 then begin
        Len := UnicodeFromLocaleChars({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, pAStr, Len, @FCachedBufW[0], TempBufCharSize);
        Len := LocaleCharsFromUnicode(CP_UTF8, 0, @FCachedBufW[0], Len, @TempBufA[0], TempBufAnsiSize, nil, nil);
      end;
    end
    else begin
      n := UnicodeFromLocaleChars({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, pAStr, Len, nil, 0);
      SetLength(TempBufW, n * SizeOf(WideChar));
      Len := UnicodeFromLocaleChars({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, pAStr, Len, @TempBufW[0], n);
      n := LocaleCharsFromUnicode(CP_UTF8, 0, @TempBufW[0], Len, nil, 0, nil, nil);
      SetLength(TempBufA, n);
      Len := LocaleCharsFromUnicode(CP_UTF8, 0, @TempBufW[0], Len, @TempBufA[0], n, nil, nil);
    end;
    TempBufA[Len] := 0; // zero terminator

    Check(FConnection.FAPI.sqlite3_bind_text(FStmt, Index, @TempBufA[0], Len, SQLITE_TRANSIENT));
  end
  else
{$ENDIF}
    Check(FConnection.FAPI.sqlite3_bind_text(FStmt, Index, pAStr, Len, SQLITE_TRANSIENT));
end;

procedure TSQLiteCommand.BindWideText(Index: integer; const WStr: WideString);
var
  n: Integer;
  Len: Integer;
  TempBufA: TBytes;
{$IFNDEF IS_UTF8}
  sa: AnsiString;
{$ENDIF}
begin
{$IFNDEF IS_UTF8}
  if not FConnection.IsUnicodeDataBase then begin
    sa := FConnection.EncodeString(WStr);
    Len := LengthA(sa);
    Check(FConnection.FAPI.sqlite3_bind_text(FStmt, Index, PAnsiChar(sa), Len, SQLITE_TRANSIENT));
  end
  else
{$ENDIF}
  begin
    Len := Length(WStr);
    if Len <= TempBufCharSize then begin
      TempBufA := FCachedBufA; // use existing temp buffer
      if Len > 0 then
        Len := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.UnicodeToUtf8(@TempBufA[0], TempBufAnsiSize, PWideChar(WStr), Len) - 1
      else
        TempBufA[0] := 0; // zero terminator
    end
    else begin
      n := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.UnicodeToUtf8(nil, 0, PWideChar(WStr), Len) + 1;
      SetLength(TempBufA, n);
      Len := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.UnicodeToUtf8(@TempBufA[0], n, PWideChar(WStr), Len) - 1;
    end;

    Check(FConnection.FAPI.sqlite3_bind_text(FStmt, Index, @TempBufA[0], Len, SQLITE_TRANSIENT));
  end;
end;

procedure TSQLiteCommand.DetectCommandType;
begin
  if (FConnection.FAPI.sqlite3_column_count(FStmt) > 0) or FIsCursorPragma then
    CommandType := ctCursor
  else
    CommandType := ctStatement;
end;

{ TSQLiteRecordSet }

constructor TSQLiteRecordSet.Create;
begin
  inherited Create;

  FFetchAll := False;
  FFetchRows := 25;
end;

destructor TSQLiteRecordSet.Destroy;
begin
  Close;

  inherited;
end;

procedure TSQLiteRecordSet.ExplicitInitFields;
begin
  FInExplicitInit := True;
  try
    inherited;
  finally
    FInExplicitInit := False;
  end;
end;

procedure TSQLiteRecordSet.GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType: Word; SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean);
var
  tso: TSQLTimeStampOffset;
begin
  case DataType of
    dtSQLTimeStampOffset: begin
      tso := PSQLTimeStampOffset(DataBuf)^;
      Value := VarSQLTimeStampOffsetCreate(tso);
    end;
  else
    inherited;
  end;
end;

procedure TSQLiteRecordSet.PutDataAsVariant(DataBuf: IntPtr; DataLenPtr: PWord; DataType: Word; Len, Scale: Word; HasParent: boolean; const Value: variant; IsDatabaseValue: boolean);
begin
  case DataType of
    dtSQLTimeStampOffset:
      PSQLTimeStampOffset(DataBuf)^ := VarToSQLTimeStampOffset(Value);
  else
    inherited;
  end;
end;

class procedure TSQLiteRecordSet.GetDateFromBuf(Buf, Date: IntPtr; HasParent: boolean; Format: TDateFormat);
begin
  case Format of
    dfMSecs:
      try
        inherited;
      except
        inherited GetDateFromBuf(Buf, Date, HasParent, dfDateTime);
      end;
  else
    inherited;
  end;
end;

function TSQLiteRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDumpData:
      FDumpData := Value;
    prUnknownAsString:
      FUnknownAsString := Value;
    prAdvancedTypeDetection:
      FAdvancedTypeDetection := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TSQLiteRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
  {$IFDEF ODBC_PROVIDER}
    prCommandTimeout:
      Value := 0;
  {$ENDIF}
    prUnknownAsString:
      Value := FUnknownAsString;
    prAdvancedTypeDetection:
      Value := FAdvancedTypeDetection;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TSQLiteRecordSet.ExecCommand(Iters: integer = 1; Offset: integer = 0);
var
  NeedPrepare: boolean;
begin
  FCommand.InitFormatSettings;

  FCommand.FLockAfterExecute := True;
  try
    NeedPrepare := not Prepared;
    if NeedPrepare then
      InternalPrepare;

    try
      inherited;

      if FCommand.CommandType = ctCursor then begin
        if FCommand.FExecResult = SQLITE_DONE then
          FCommand.SetCursorState(csFetched)
        else
          FCommand.SetCursorState(csExecuted)
      end;

    except
      if NeedPrepare then
        InternalUnprepare;
      raise;
    end;

    if FCommand.CommandType <> ctCursor then
      if NeedPrepare then
        InternalUnprepare;

  except
    FCommand.FLockAfterExecute := False;
    if Assigned(FCommand.AfterExecute) then
      FCommand.AfterExecute(False);
    raise;
  end;
  FCommand.FLockAfterExecute := False;
  if Assigned(FCommand.AfterExecute) then
    FCommand.AfterExecute(True);
end;

procedure TSQLiteRecordSet.SetToEnd;
begin
  FetchAll;

  inherited;
end;

procedure TSQLiteRecordSet.Check(ErrorCode: integer);
begin
  FCommand.FConnection.Check(ErrorCode);
end;

procedure TSQLiteRecordSet.CreateCommand;
begin
  SetCommand(TSQLiteCommand.Create);
end;

procedure TSQLiteRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TSQLiteCommand(Value);
end;

procedure TSQLiteRecordSet.InternalPrepare;
begin
  inherited;

  FCommand.DetectCommandType;
end;

function TSQLiteRecordSet.ExtFieldsInfoIsInternal: boolean;
begin
  Result := FCommand.FConnection.FAPI.IsMetaDataAPIAvailable;
end;

procedure TSQLiteRecordSet.DetectIdentityField;
var
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  FIdentityField := nil;

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if FieldDesc.IsAutoincrement or (CompareText(FieldDesc.ActualName, 'rowid') = 0) then
      if FieldDesc.TableInfo = UpdatingTableInfo then begin
        FIdentityField := FieldDesc;
        Exit;
      end;
  end;
end;

procedure TSQLiteRecordSet.DetectFieldType(const FieldName: string; DBType: Word; DBLength: Integer; DBScale: SmallInt;
  out DataType: Word; out Len, Scale: Integer; out Fixed: boolean);
const
  MaxStringSize = 8192;
{$IFNDEF LITE}
var
  FetchConverter: TFetchConverter;
{$ENDIF}
begin
{$IFNDEF LITE}
  FetchConverter := GetMapFetchConverter(FieldName, DBType, DBLength, DBScale);
  if FetchConverter <> nil then
    DataType := FetchConverter.InternalDataType
  else
{$ENDIF}
  begin
    DataType := GetDataType(DBType);

    if DataType = dtUnknown then
      DataType := dtString;

    if DataType = dtVarBytes then
      if DBLength <= 0 then
        DataType := dtBlob;

    if DataType = dtString then
      if (DBLength > 8000) or
         (DBLength >= 255) and not FLongStrings or
         (DBLength < 0) and not FUnknownAsString
      then
        DataType := dtMemo;

    if FCommand.FConnection.FUseUnicode then
      case DataType of
        dtString:
          DataType := dtWideString;
        dtMemo:
          DataType := dtWideMemo;
      end;
  end;

  Fixed := IsFixedDBType(DBType);

  if DBLength >= 0 then
    Len := DBLength
  else
    Len := 0;

  if DBScale >= 0 then
    Scale := DBScale
  else
    Scale := 0;

  if (not FCommand.FConnection.FUseUnicode) and (DataType in [dtString, dtFixedChar, dtWideString, dtFixedWideChar]) then
    Len := Len * FCommand.FConnection.FCharLength;

  if DataType = dtVarBytes then begin
    if Len > 0 then
      if (Len >= FlatBufferLimit) and not FFlatBuffers then
        DataType := dtExtVarBytes;
  end
  else if DataType = dtFMTBCD then begin
    if (Len > MaxFMTBcdDigits) or (Len <= 0) then
      Len := MaxFMTBcdDigits;
    if Scale > Len then // if length was reduced
      Scale := Len;
  end
  else if DataType = dtBCD then begin
    if (Len > MaxBcdPrecision) or (Len <= 0) then
      Len := MaxBcdPrecision;
    if Scale > MaxBcdScale then
      Scale := MaxBcdScale;
  end
  else if DataType in [dtString, dtWideString] then begin
    if Len <= 0 then
      Len := MaxStringSize;

    if not FFlatBuffers and (Len >= FlatBufferLimit) then
      if DataType = dtString then
        DataType := dtExtString
      else if DataType = dtWideString then
        DataType := dtExtWideString;
  end;
end;

procedure TSQLiteRecordSet.DescribeFieldDesc(Field: TCRFieldDesc; Index: integer);

  function GetAnsiBytes(const chars: AnsiString): TBytes;
  begin
    SetLength(Result, Length(chars) + 1);
    Move(PAnsiChar(chars)^, Pointer(Result)^, Length(chars));
    Result[Length(chars)] := 0;
  end;

var
  TableName, DatabaseName, DataTypeName: string;
  TableInfo: TCRTableInfo;
  DataType: Word;
  Len, Scale: integer;
  Fixed: Boolean;
{$IFNDEF LITE}
  aDatabaseName,
  aTableName,
  aFieldName: TBytes;
  pDataType,
  pCollation: IntPtr;
  NotNull, PrimaryKey, AutoInc: integer;
{$ENDIF}
begin
  Field.FieldNo := Index + 1;
  Field.ActualFieldNo := Index;
  Field.Name := FCommand.FConnection.DecodeString(FCommand.FConnection.FAPI.sqlite3_column_name(FCommand.FStmt, Index));
  if FCommand.FConnection.FAPI.IsMetaDataAPIAvailable then
    Field.ActualName := FCommand.FConnection.DecodeString(FCommand.FConnection.FAPI.sqlite3_column_origin_name(FCommand.FStmt, Index))
  else
    Field.ActualName := '';

  Field.TableInfo := nil;
  if FCommand.FConnection.FAPI.IsMetaDataAPIAvailable then begin
    TableName := FCommand.FConnection.DecodeString(FCommand.FConnection.FAPI.sqlite3_column_table_name(FCommand.FStmt, Index));
    DatabaseName := FCommand.FConnection.DecodeString(FCommand.FConnection.FAPI.sqlite3_column_database_name(FCommand.FStmt, Index));
    if TableName <> '' then begin
      TableName := FCommand.FConnection.NormalizeTableName(TableName);
      TableInfo := FTablesInfo.FindByName(TableName);
      if TableInfo = nil then begin
        TableInfo := FTablesInfo.Add;
        TableInfo.TableName := TableName;
        TableInfo.TableAlias := '';
        if DatabaseName <> '' then
          TableName := FCommand.FConnection.NormalizeTableName(DatabaseName) + '.' + TableName;
        TableInfo.TableNameFull := TableName;
      end;
      Field.TableInfo := TableInfo;
    end;
  end;

{$IFNDEF LITE}
  DataTypeName := '';
{$IFNDEF VER9P}
  SetLength(aDatabaseName, 0);
  SetLength(aTableName, 0);
  SetLength(aFieldName, 0);
{$ENDIF}
  if Field.TableInfo <> nil then begin
    aDatabaseName := GetAnsiBytes(FCommand.FConnection.EncodeString(DatabaseName));
    aTableName := GetAnsiBytes(FCommand.FConnection.EncodeString(SQLiteInfo.UnQuote(Field.TableInfo.TableName)));
    aFieldName := GetAnsiBytes(FCommand.FConnection.EncodeString(Field.Name));
    if FCommand.FConnection.FAPI.sqlite3_table_column_metadata(FCommand.FConnection.FAPI.SQLite,
      @aDatabaseName[0], @aTableName[0], @aFieldName[0], pDataType, pCollation, NotNull, PrimaryKey, AutoInc) = 0 then
    begin
      DataTypeName := SQLiteInfo.UnQuote(UpperCase(FCommand.FConnection.DecodeString(pDataType)));
      Field.IsKey := boolean(PrimaryKey);
      if Field.IsKey then
        TSQLiteTableInfo(Field.TableInfo).FKeyCount := TSQLiteTableInfo(Field.TableInfo).FKeyCount + 1;
      Field.IsAutoIncrement := boolean(AutoInc) or ((UpperCase(DataTypeName) = 'INTEGER') and Field.IsKey);
      Field.Required := boolean(NotNull);
    end;
  end;
{$ENDIF}

  if FDumpData then begin
    DataType := dtWideMemo;
    Len := 0;
    Scale := 0;
    Fixed := False;
  end
  else begin
  {$IFNDEF LITE}
    if DataTypeName = '' then
  {$ENDIF}
      DataTypeName := UpperCase(FCommand.FConnection.DecodeString(FCommand.FConnection.FAPI.sqlite3_column_decltype(FCommand.FStmt, Index)));

    if DataTypeName <> '' then begin
      GetSQLTypeMeasures(DataTypeName, Len, Scale);

      Field.DBType := GetDBType(DataTypeName, Len, Scale);
      Field.DBLength := Len;
      Field.DBScale := Scale;
    end
    else if Field.ActualName = '' then begin
      // set DataType basing on type of values from first record
      DataType := FCommand.FConnection.FAPI.sqlite3_column_type(FCommand.FStmt, Index);
      Field.DBType := GetDBType(DataType);
      Field.DBLength := -1;
      Field.DBScale := -1;
    end
    else begin
      Field.DBType := GetDBUnknown;
      Field.DBLength := -1;
      Field.DBScale := -1;
    end;

    DetectFieldType(Field.Name, Field.DBType, Field.DBLength, Field.DBScale, DataType, Len, Scale, Fixed);
  end;

  Field.DataType := DataType;
  Field.Length := Len;
  Field.Scale := Scale;
  Field.Fixed := Fixed;

  Field.Size := GetBufferSize(Field.DataType, Field.Length);

  if Field.ActualName = '' then
    Field.ActualName := Field.Name;

  if FAdvancedTypeDetection then
    if ((Field.TableInfo = nil) or (DataTypeName = '')) and
       not (Field.DataType in [dtString, dtWideString, dtMemo, dtWideMemo, dtExtString, dtExtWideString])
    then begin
      FFieldTypes[Index] := 0;
      FHasUndefinedFields := True;
    end
    else
      FFieldTypes[Index] := -1;
end;

procedure TSQLiteRecordSet.CreateFieldDescs;
const
  MaxStringSize = 8192;
var
  OldCursorState: TCursorState;
  Field: TCRFieldDesc;
  FieldsCount: Integer;
  i: integer;
begin
  OldCursorState := FCommand.GetCursorState;
  if (OldCursorState = csInactive) and (FCommand.CommandType <> ctStatement) then begin
    InternalPrepare;
    if FInExplicitInit{$IFNDEF LITE} or (SmartFetchState = sfMetaInfo){$ENDIF} then
      ExecCommand;
  end;

  try
    if FCommand.CommandType <> ctCursor then
      raise Exception.Create(SNotRows);

    FTablesInfo.BeginUpdate;
    try
      if not FCommand.FConnection.FAPI.IsMetaDataAPIAvailable then begin
        FTablesInfo.CaseSensitive := False;
        FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, FTablesInfo);
      end;

      FieldsCount := FCommand.FConnection.FAPI.sqlite3_column_count(FCommand.FStmt);

      FHasUndefinedFields := False;
      if FAdvancedTypeDetection then
        SetLength(FFieldTypes, FieldsCount)
      else
        SetLength(FFieldTypes, 0);

      for i := 0 to FieldsCount - 1 do begin
        Field := TCRFieldDesc(CreateFieldDesc);
        DescribeFieldDesc(Field, i);

        FFields.Add(Field);
      end;

      if FAdvancedTypeDetection and FHasUndefinedFields then
        InternalPrefetch;

      for i := 0 to FieldsCount - 1 do begin
        Field := TCRFieldDesc(FFields[i]);

        if FAdvancedTypeDetection and (FFieldTypes[i] > 0) then begin
          Field.DataType := FFieldTypes[i];

          if FCommand.FConnection.FUseUnicode then
            case Field.DataType of
              dtString:
                Field.DataType := dtWideString;
              dtMemo:
                Field.DataType := dtWideMemo;
            end;

          if Field.DataType in [dtString, dtWideString] then begin
            Field.Length := MaxStringSize;
            Field.Scale := 0;

            if not FUnknownAsString then
              if Field.DataType = dtString then
                Field.DataType := dtMemo
              else
                Field.DataType := dtWideMemo;
          end;

          if not FFlatBuffers then
            if Field.DataType = dtString then
              Field.DataType := dtExtString
            else if Field.DataType = dtWideString then
              Field.DataType := dtExtWideString;

          Field.Size := GetBufferSize(Field.DataType, Field.Length);
        end;

        if (Field.TableInfo <> nil) and (TSQLiteTableInfo(Field.TableInfo).FKeyCount > 1) then
          Field.IsAutoIncrement := False;
      end;

    finally
      FTablesInfo.EndUpdate;
    end;
  finally
    if OldCursorState = csInactive then begin
      InternalUnPrepare;
      if FInExplicitInit{$IFNDEF LITE} or (SmartFetchState = sfMetaInfo){$ENDIF} then
        CloseCommand;
    end;
  end;
end;

{$IFNDEF LITE}

procedure TSQLiteRecordSet.RequestFieldsInfo(Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo);

  function Locate(MetaData: TData; RecBuf: IntPtr; Field: TFieldDesc; Value: string): boolean;
  var
    v: variant;
  begin
    MetaData.SetToBegin;
    while True do begin
      MetaData.GetNextRecord(RecBuf);
      if MetaData.Eof then
        break;
      MetaData.GetFieldAsVariant(Field, RecBuf, v);
      if SameText(VarToStr(v), Value) then begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;

var
  Restr: TStringList;
  SQLiteMetaData: TSQLiteMetaData;
  MetaData: TData;
  i, p: integer;
  ColumnInfo, NewColumnInfo: TCRColumnInfo;
  Located: boolean;
  RecBuf: IntPtr;
  v: variant;
  NameField: TFieldDesc;
  ExprField: TFieldDesc;
begin
  for i := 0 to High(Tables) do
    with Tables[i] do begin
      if (FFieldOrigins <> foNone) or (i = FUpdatingTableInfoIdx) then
        Flag := 1
      else
        Flag := 0; // don't query fields for this table
    end;

  SQLiteMetaData := TSQLiteMetaData.Create;
  try
    for i := 0 to High(Tables) do begin
      if Tables[i].Flag = 0 then
        continue;

      Restr := TStringList.Create;
      try
        Restr.Add('TABLE_NAME=' + Tables[i].Name);
        MetaData := SQLiteMetaData.GetMetaData(FCommand.FConnection, FCommand.FConnection.GetInternalTransaction, 'columns', Restr);
      finally
        Restr.Free;
      end;

      MetaData.AllocRecBuf(RecBuf);
      try
        p := 0;
        while p < Columns.Count do begin
          ColumnInfo := Columns[p];
          if (ColumnInfo.TableIndex <> -1) and (ColumnInfo.TableIndex <> i) then begin
            Inc(p);
            continue;
          end;

          NameField := MetaData.Fields[3];
          ExprField := MetaData.Fields[10];

          if ColumnInfo.Name = '*' then begin
            MetaData.SetToBegin;
            repeat
              MetaData.GetNextRecord(RecBuf);
              if MetaData.Eof then
                break;

              Inc(p);
              NewColumnInfo := TCRColumnInfo.Create;
              Columns.Insert(p, NewColumnInfo);
              NewColumnInfo.Table := ColumnInfo.Table;
              NewColumnInfo.TableIndex := i;

              MetaData.GetFieldAsVariant(NameField, RecBuf, v);
              NewColumnInfo.Name := VarToStr(v);
              NewColumnInfo.Alias := NewColumnInfo.Name;

              if FDefaultValues and (i = FUpdatingTableInfoIdx) then begin
                MetaData.GetFieldAsVariant(ExprField, RecBuf, v);
                NewColumnInfo.Expr := VarToStr(v);
              end;
            until False;
          end
          else
          if (ColumnInfo.Name <> '') then begin
            if ColumnInfo.TableIndex <> -1 then begin
              if FDefaultValues and (i = FUpdatingTableInfoIdx) then
                Located := Locate(MetaData, RecBuf, NameField, ColumnInfo.Name)
              else
                Located := False;
            end
            else
              Located := Locate(MetaData, RecBuf, NameField, ColumnInfo.Name);

            if Located then begin
              if FDefaultValues then begin
                MetaData.GetFieldAsVariant(ExprField, RecBuf, v);
                ColumnInfo.Expr := VarToStr(v);
              end;

              ColumnInfo.TableIndex := i;
            end;
          end;

          inc(p);
        end;
        MetaData.Close;
      finally
        MetaData.FreeRecBuf(RecBuf);
      end;
    end;
  finally
    SQLiteMetaData.Free;
  end;
end;

{$ENDIF}

function TSQLiteRecordSet.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

procedure TSQLiteRecordSet.InternalClose;
begin
  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  inherited;

  if not Prepared then
    InternalUnprepare;
end;

procedure TSQLiteRecordSet.InternalPrefetch;
var
  Res,
  FieldsCount,
  ValueType,
  FieldType,
  i, n: integer;
begin
  n := 0;
  FieldsCount := Length(FFieldTypes);

  while True do begin
    for i := n to FieldsCount - 1 do begin
      if FFieldTypes[i] >= 0 then begin
        ValueType := FCommand.FConnection.FAPI.sqlite3_column_type(FCommand.FStmt, i);
        case ValueType of
          SQLITE_INTEGER:
            FieldType := dtLargeint;
          SQLITE_FLOAT:
            FieldType := dtFloat;
          SQLITE_TEXT, SQLITE_NULL:
            FieldType := dtString;
          SQLITE_BLOB:
            FieldType := dtBlob;
        else
          FieldType := dtUnknown;
          Assert(False);
        end;

        if FFieldTypes[i] = 0 then
          FFieldTypes[i] := FieldType
        else if FieldType <> FFieldTypes[i] then begin
          FFieldTypes[i] := dtString;
          Inc(n);
        end;
      end;
    end;

    Res := FCommand.FConnection.FAPI.sqlite3_step(FCommand.FStmt);
    if Res = SQLITE_DONE then begin
      Res := FCommand.FConnection.FAPI.sqlite3_reset(FCommand.FStmt);
      if Res <> SQLITE_OK then
        Check(Res);
      Res := FCommand.FConnection.FAPI.sqlite3_step(FCommand.FStmt);
      if (Res <> SQLITE_ROW) and (Res <> SQLITE_DONE) then
        Check(Res);
      Break;
    end
    else if Res <> SQLITE_ROW then
      Check(Res);
  end;
end;

procedure TSQLiteRecordSet.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);
var
  Item: IntPtr;
  Res: integer;
  ItemSize: Integer;
begin
  Item := PtrOffset(Block, SizeOf(TBlockHeader));
  ItemSize := RecordSize + SizeOf(TItemHeader);
  RowsObtained := 1;
  while True do begin
    if FDumpData then
      ReadFieldValuesForDump(PtrOffset(Item, SizeOf(TItemHeader)))
    else
      ReadFieldValues(PtrOffset(Item, SizeOf(TItemHeader)), RowsObtained - 1);

    Res := FCommand.FConnection.FAPI.sqlite3_step(FCommand.FStmt);
    if Res = SQLITE_DONE then begin
      FCommand.SetCursorState(csFetched);
      Break;
    end
    else
      if Res <> SQLITE_ROW then
        Check(Res);

    if RowsObtained = FFetchRows then
      Break;
    Item := PtrOffset(Item, ItemSize);
    Inc(RowsObtained);
  end;
end;

function TSQLiteRecordSet.NeedUnPrepareAfterFetch: boolean;
begin
  Result := not Prepared; {AutoPrepare}
end;

function TSQLiteRecordSet.GetDBType(const SQLTypeName: string; var Len, Scale: integer): word;
begin
  Result := TLiteConverterManager.GetDBType(SQLTypeName);
end;

function TSQLiteRecordSet.GetDBType(SQLType: integer): word;
begin
  Result := TLiteConverterManager.GetDBType(SQLType);
end;

function TSQLiteRecordSet.GetDBUnknown: word;
begin
  Result := liteNull;
end;

function TSQLiteRecordSet.IsFixedDBType(DBType: word): boolean;
begin
  Result := DBType = liteChar;
end;

function TSQLiteRecordSet.GetDataType(DBType: word): word;
begin
  case DBType of
    liteTinyInt,
    liteSmallint:
      Result := dtSmallint; // Int16
    liteInt,
    liteInteger,
    liteMediumInt:
      Result := dtInteger;  // Int32
    liteBigInt,
    liteUBigInt:
      Result := dtLargeint; // Int64
    liteChar,
    liteVarChar:
      Result := dtString;
    liteText,
    liteClob:
      Result := dtMemo;
    liteBlob:
      Result := dtBlob;
    liteReal,
    liteFloat,
    liteDouble:
      Result := dtFloat;
    liteNumeric,
    liteDecimal,
    liteNumber:
      if FCommand.EnableFMTBCD or FCommand.FConnection.EnableFMTBCD then
        Result := dtFMTBCD
      else if FCommand.EnableBCD or FCommand.FConnection.EnableBCD then
        Result := dtBCD
      else
        Result := dtFloat;
    liteMoney:
      Result := dtCurrency;
    liteBool:
      Result := dtBoolean;
    liteBinary,
    liteVarBinary:
      Result := dtVarBytes;
    liteDate:
      Result := dtDate;
    liteTime:
      Result := dtTime;
    liteDateTime:
      Result := dtDateTime;
    liteTimestamp:
      Result := dtDateTime;
    liteTimestampTZ:
      Result := dtSQLTimeStampOffset;
    liteBit:
      Result := dtBoolean;
    else
      Result := dtUnknown;
  end;
end;

procedure TSQLiteRecordSet.ReadFieldValues(RecBuf: IntPtr; Row: integer);
var
  i, n, ValueType, Len,
  FieldIndex: integer;
  Field: TCRFieldDesc;
  HeapBuf, BlobData: IntPtr;
  Blob: TBlob;
  Piece: PPieceHeader;
  val_int: integer;
  val_int64: int64;
  val_float: double;
  val_single: single;
  val_cur: currency;
  val_bcd: TBcd;
  val_str, s: string;
  val_blob: IntPtr;
  val_date: TDateTime;
  val_ts: TSQLTimeStamp;
  val_tstz: TSQLTimeStampOffset;
  pUtf8Str: PAnsiChar;
  FieldDataType: word;
  DataPtr: IntPtr;
  IndPtr: IntPtr;
  DataLenPtr: PWord;
{$IFNDEF IS_UTF8}
  TempBufW: TBytes;
{$ENDIF}
  err: boolean;
begin
  FCommand.AllocCachedBuffers;

  CreateComplexFields(RecBuf, True);

  for i := 0 to FFields.Count - 1 do begin
    Field := TCRFieldDesc(FFields[i]);
    if Field.FieldDescKind <> fdkData then
      continue;

    IndPtr := PtrOffset(RecBuf, DataSize + i);

      DataPtr := PtrOffset(RecBuf, Field.DataOffset);
      if Field.HasValueLen then
        DataLenPtr := PtrOffset(RecBuf, Field.Offset)
      else
        DataLenPtr := nil;

    FieldIndex := Field.ActualFieldNo;
    if (Field.ActualFieldNo > -1) then
      ValueType := FCommand.FConnection.FAPI.sqlite3_column_type(FCommand.FStmt, FieldIndex)
    else
      ValueType := SQLITE_NULL;

    FieldDataType := Field.DataType;
    if ValueType = SQLITE_NULL then begin
      Marshal.WriteByte(IndPtr, 1);
      if FieldDataType in [dtMemo, dtWideMemo, dtBlob] then begin
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
        Blob.Clear;
      end;
    end
    else begin
      Marshal.WriteByte(IndPtr, 0);
      case FieldDataType of
        dtInt8, dtUInt8: begin
          val_int := FCommand.FConnection.FAPI.sqlite3_column_int(FCommand.FStmt, FieldIndex);
          Marshal.WriteByte(DataPtr, Byte(val_int));
        end;
        dtSmallint, dtWord: begin
          val_int := FCommand.FConnection.FAPI.sqlite3_column_int(FCommand.FStmt, FieldIndex);
          Marshal.WriteInt16(DataPtr, SmallInt(val_int));
        end;
        dtInteger: begin
          val_int := FCommand.FConnection.FAPI.sqlite3_column_int(FCommand.FStmt, FieldIndex);
          Marshal.WriteInt32(DataPtr, val_int);
        end;
        dtLongWord: begin
          val_int64 := FCommand.FConnection.FAPI.sqlite3_column_int64(FCommand.FStmt, FieldIndex);
          Marshal.WriteInt32(DataPtr, Integer(Cardinal(val_int64)));
        end;
        dtLargeint: begin
          val_int64 := FCommand.FConnection.FAPI.sqlite3_column_int64(FCommand.FStmt, FieldIndex);
          Marshal.WriteInt64(DataPtr, val_int64);
        end;
        dtSingle, dtFloat, dtCurrency: begin
          val_float := FCommand.FConnection.FAPI.sqlite3_column_double(FCommand.FStmt, FieldIndex);
          if FieldDataType <> dtSingle then
            Marshal.WriteInt64(DataPtr, BitConverter.DoubleToInt64Bits(val_float))
          else begin
            val_single := val_float;
            Marshal.WriteInt32(DataPtr, CRBitConverter.SingleToInt32Bits(val_single));
          end;
        end;
        dtExtended: begin
          if ValueType = SQLITE_INTEGER then
            val_float := FCommand.FConnection.FAPI.sqlite3_column_int64(FCommand.FStmt, FieldIndex)
          else
            val_float := FCommand.FConnection.FAPI.sqlite3_column_double(FCommand.FStmt, FieldIndex);
          val_int64 := PInt64(@val_float)^;
          Marshal.WriteInt64(DataPtr, val_int64);
        end;
        dtBCD: begin
          if ValueType = SQLITE_INTEGER then
            val_cur := FCommand.FConnection.FAPI.sqlite3_column_int64(FCommand.FStmt, FieldIndex)
          else
            val_cur := FCommand.FConnection.FAPI.sqlite3_column_double(FCommand.FStmt, FieldIndex);
          val_int64 := PInt64(@val_cur)^;
          Marshal.WriteInt64(DataPtr, val_int64);
        end;
        dtFMTBCD: begin
          if ValueType = SQLITE_INTEGER then
            val_str := IntToStr(FCommand.FConnection.FAPI.sqlite3_column_int64(FCommand.FStmt, FieldIndex))
          else if ValueType = SQLITE_TEXT then
            val_str := string(AnsiString(FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex)))
          else
            val_str := FloatToStr(FCommand.FConnection.FAPI.sqlite3_column_double(FCommand.FStmt, FieldIndex));
          val_bcd := StrToBcd(val_str);
          PBcd(DataPtr)^ := val_bcd;
        end;
        dtBoolean: begin
          val_int64 := FCommand.FConnection.FAPI.sqlite3_column_int64(FCommand.FStmt, FieldIndex);
          if val_int64 <> 0 then
            val_int64 := 1;
          Marshal.WriteInt16(DataPtr, val_int64);
        end;
        dtGuid: begin
          val_int := FCommand.FConnection.FAPI.sqlite3_column_type(FCommand.FStmt, FieldIndex);
          if val_int = SQLITE_TEXT then begin
            pUtf8Str := FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex);
            Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
            if Len > Field.Length then
              Len := Field.Length;
            Move(pUtf8Str^, DataPtr^, Len);
            Marshal.WriteByte(DataPtr, Len, 0);
              DataLenPtr^ := Word(Len);
          end;
        end;
        dtString: begin
          pUtf8Str := FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex);
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
          if Len > 0 then
          {$IFNDEF IS_UTF8}
            if FCommand.FConnection.IsUnicodeDataBase then begin
              Len := UnicodeFromLocaleChars(CP_UTF8, 0, pUtf8Str, Len, @FCommand.FCachedBufW[0], Field.Length);
              Len := LocaleCharsFromUnicode({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, @FCommand.FCachedBufW[0], Len, DataPtr, Field.Length, nil, nil);
            end
            else
          {$ENDIF}
              Move(pUtf8Str^, DataPtr^, Len);
          Marshal.WriteByte(DataPtr, Len, 0);
            DataLenPtr^ := Word(Len);
        end;
        dtWideString: begin
          pUtf8Str := FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex);
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
          if Len > 0 then
            {$IFNDEF IS_UTF8}
            if not FCommand.FConnection.IsUnicodeDataBase then
              Len := UnicodeFromLocaleChars({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, pUtf8Str, Len, DataPtr, Field.Length)
            else
            {$ENDIF}
              Len := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.Utf8ToUnicode(DataPtr, Field.Length + 1, pUtf8Str, Len) - 1;
          Marshal.WriteInt16(DataPtr, Len * 2, 0);
            DataLenPtr^ := Word(Len);
        end;
        dtExtString: begin
          pUtf8Str := FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex);
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
        {$IFNDEF IS_UTF8}
          if (Len > 0) and FCommand.FConnection.IsUnicodeDataBase then begin
            Len := UnicodeFromLocaleChars(CP_UTF8, 0, pUtf8Str, Len, @FCommand.FCachedBufW[0], Field.Length);
            Len := LocaleCharsFromUnicode({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, @FCommand.FCachedBufW[0], Len, @FCommand.FCachedBufA[0], Field.Length, nil, nil);
            HeapBuf := StringHeap.AllocStr(@FCommand.FCachedBufA[0], Len);
          end
          else
        {$ENDIF}
            HeapBuf := StringHeap.AllocStr(pUtf8Str, Len);
            DataLenPtr^ := Word(Len);
          Marshal.WriteIntPtr(DataPtr, HeapBuf);
        end;
        dtExtWideString: begin
          pUtf8Str := FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex);
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
          if Len > 0 then
          {$IFNDEF IS_UTF8}
            if not FCommand.FConnection.IsUnicodeDataBase then
              Len := UnicodeFromLocaleChars({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, pUtf8Str, Len, @FCommand.FCachedBufW[0], Field.Length)
            else
          {$ENDIF}
              Len := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.Utf8ToUnicode(@FCommand.FCachedBufW[0], Field.Length + 1, pUtf8Str, Len) - 1;
          HeapBuf := StringHeap.AllocWideStr(@FCommand.FCachedBufW[0], Len);
            DataLenPtr^ := Word(Len);
          Marshal.WriteIntPtr(DataPtr, HeapBuf);
        end;
        dtMemo: begin
          Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
          pUtf8Str := FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex);
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
          if Len > 0 then begin
          {$IFNDEF IS_UTF8}
            if FCommand.FConnection.IsUnicodeDataBase then begin
              if Len <= TempBufCharSize then begin
                TempBufW := FCommand.FCachedBufW; // use existing temp buffer
                Len := UnicodeFromLocaleChars(CP_UTF8, 0, pUtf8Str, Len, @TempBufW[0], TempBufCharSize);
              end
              else begin
                n := UnicodeFromLocaleChars(CP_UTF8, 0, pUtf8Str, Len, nil, 0);
                SetLength(TempBufW, n * SizeOf(WideChar));
                Len := UnicodeFromLocaleChars(CP_UTF8, 0, pUtf8Str, Len, @TempBufW[0], n);
              end;
              n := LocaleCharsFromUnicode({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, @TempBufW[0], Len, nil, 0, nil, nil);
              Blob.AllocPiece(Piece, n);
              BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
              Len := LocaleCharsFromUnicode({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, @TempBufW[0], Len, BlobData, n, nil, nil);
            end
            else
          {$ENDIF}
            begin
              Blob.AllocPiece(Piece, Len);
              BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
              Move(pUtf8Str^, BlobData^, Len);
            end;
            Piece.Used := Len;
            Blob.AppendPiece(Piece);
          end
          else
            Blob.AsString := '';
        end;
        dtWideMemo: begin
          Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
          pUtf8Str := FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex);
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
          if Len > 0 then begin
            {$IFNDEF IS_UTF8}
            if not FCommand.FConnection.IsUnicodeDataBase then begin
              n := UnicodeFromLocaleChars({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, pUtf8Str, Len, nil, 0);
              Blob.AllocPiece(Piece, n * SizeOf(WideChar));
              BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
              Len := UnicodeFromLocaleChars({$IFDEF VER12P}DefaultSystemCodePage{$ELSE}0{$ENDIF}, 0, pUtf8Str, Len, BlobData, n);
            end
            else
            {$ENDIF}
            begin
              n := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.Utf8ToUnicode(nil, 0, pUtf8Str, Len) + 1;
              Blob.AllocPiece(Piece, n * SizeOf(WideChar));
              BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
              Len := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.Utf8ToUnicode(BlobData, n, pUtf8Str, Len) - 1;
            end;
            Piece.Used := Len * SizeOf(WideChar);
            Blob.AppendPiece(Piece);
          end
          else
            Blob.AsWideString := '';
        end;
        dtBlob: begin
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
          Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
          if Len > 0 then begin
            val_blob := FCommand.FConnection.FAPI.sqlite3_column_blob(FCommand.FStmt, FieldIndex);
            Blob.AllocPiece(Piece, Len);
            BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
            Move(val_blob^, BlobData^, Len);
            Piece.Used := Len;
            Blob.AppendPiece(Piece);
          end
          else
            Blob.AsString := '';
        end;
        dtBytes, dtVarBytes: begin
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
          if Field.Size < Len then
            Len := Field.Size;
          if Len > 0 then begin
            val_blob := FCommand.FConnection.FAPI.sqlite3_column_blob(FCommand.FStmt, FieldIndex);
            CopyBuffer(val_blob, DataPtr, Len);
          end;
            DataLenPtr^ := Word(Len);
        end;
        dtExtVarBytes: begin
          Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, FieldIndex);
          HeapBuf := StringHeap.NewBuf(Len);
          if Len > 0 then begin
            val_blob := FCommand.FConnection.FAPI.sqlite3_column_blob(FCommand.FStmt, FieldIndex);
            CopyBuffer(val_blob, HeapBuf, Len);
          end;
            DataLenPtr^ := Word(Len);
          Marshal.WriteIntPtr(DataPtr, HeapBuf);
        end;
        dtDate, dtTime, dtDateTime, dtSQLTimeStamp: begin
          val_str := FCommand.FConnection.DecodeString(FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex));
          val_date := ConvertStrToDateTime(Field.DataType, val_str,
                                           FCommand.FDateSeparator, FCommand.FTimeSeparator,
                                           FCommand.FShortDateFormat, FCommand.FShortTimeFormat,
                                           FCommand.FConnection.FNativeDate, s, err);
          case Field.DataType of
            dtDate:
              Marshal.WriteInt64(DataPtr, BitConverter.DoubleToInt64Bits(val_date));
            dtTime, dtDateTime: begin
              if s <> '' then begin
                for n := 1 to 3 - Length(s) do
                  s := s + '0';
                if Length(s) > 3 then begin
                  s := Copy(s, 1, 4);
                  n := (StrToInt(s) + 5) div 10;
                end
                else
                  n := StrToInt(s);
                val_date := val_date + n / 86400000;
              end;
              Marshal.WriteInt64(DataPtr, BitConverter.DoubleToInt64Bits(val_date));
            end;
            dtSQLTimeStamp: begin
              val_ts := DateTimeToSQLTimeStamp(val_date);
              if s <> '' then begin
                for n := 1 to 3 - Length(s) do
                  s := s + '0';
                if Length(s) > 3 then begin
                  s := Copy(s, 1, 4);
                  val_ts.Fractions := (StrToInt(s) + 5) div 10;
                end
                else
                  val_ts.Fractions := StrToInt(s);
              end;
              PSQLTimeStamp(DataPtr)^ := val_ts;
            end;
          end;
        end;
        dtSQLTimeStampOffset: begin
          val_str := FCommand.FConnection.DecodeString(FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, FieldIndex));
          val_tstz := ConvertStrToTimestampOffset(val_str,
                                                  FCommand.FDateSeparator, FCommand.FTimeSeparator,
                                                  FCommand.FShortDateFormat, FCommand.FShortTimeFormat);
          PSQLTimeStampOffset(DataPtr)^ := val_tstz;
        end
      else
        Assert(False);
      end;
    end;
  end;
end;

procedure TSQLiteRecordSet.ReadFieldValuesForDump(RecBuf: IntPtr);

  procedure MoveToBuffer(const Str: WideString; Buf: IntPtr; Len: integer);
  begin
    Move(Pointer(Str)^, Buf^, Len * 2);
  end;

var
  i, ValueType, Len: integer;
  Field: TFieldDesc;
  DataBuf: IntPtr;
  BlobData: IntPtr;
  Blob: TBlob;
  Piece: PPieceHeader;
  val_str: WideString;
  val_blob: IntPtr;
  s: string;
begin
  CreateComplexFields(RecBuf, True);

  for i := 0 to FFields.Count - 1 do begin
    Field := FFields[i];
    Marshal.WriteByte(RecBuf, DataSize + i, 0);
    DataBuf := PtrOffset(RecBuf, Field.DataOffset);
    ValueType := FCommand.FConnection.FAPI.sqlite3_column_type(FCommand.FStmt, i);
    case ValueType of
      SQLITE_INTEGER:
        val_str := WideString(IntToStr(FCommand.FConnection.FAPI.sqlite3_column_int64(FCommand.FStmt, i)));
      SQLITE_FLOAT:
        val_str := WideString(StringReplace(FloatToStr(FCommand.FConnection.FAPI.sqlite3_column_double(FCommand.FStmt, i)),
          {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.', []));
      SQLITE_TEXT:
        val_str := WideString(QuotedStr(FCommand.FConnection.DecodeString(FCommand.FConnection.FAPI.sqlite3_column_text(FCommand.FStmt, i))));
      SQLITE_BLOB: begin
        Len := FCommand.FConnection.FAPI.sqlite3_column_bytes(FCommand.FStmt, i);
        val_blob := FCommand.FConnection.FAPI.sqlite3_column_blob(FCommand.FStmt, i);
        SetLength(s, Len * 2);
        BinToHex(val_blob, PChar(s), Len);
        val_str := WideString('X''' + s + '''');
      end;
      SQLITE_NULL:
        val_str := 'NULL';
    end;

    Len := Length(val_str);
    if Len > 0 then begin
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf)));
      Blob.AllocPiece(Piece, Len * 2);
      BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
      MoveToBuffer(val_str, BlobData, Len);
      Piece.Used := Len * 2;
      Blob.AppendPiece(Piece);
    end;
  end;
end;

{ TSQLiteTableInfo }

function TSQLiteTableInfo.GetTableNameFull: string;
begin
  Result := FTableNameFull;
end;

procedure TSQLiteTableInfo.SetTableNameFull(const Value: string);
begin
  FTableNameFull := Value;
end;

{ TSQLiteMetaData }

constructor TSQLiteMetaData.Create;
begin
  inherited;
end;

destructor TSQLiteMetaData.Destroy;
begin
  inherited;
end;

function TSQLiteMetaData.GetConnection: TSQLiteConnection;
begin
  Result := TSQLiteRecordSet(FRecordSet).FCommand.FConnection;
end;

function TSQLiteMetaData.GetTypesForSQL(const ObjectTypes: string; AllTypes: array of string): string;
var
  i: integer;
  Res: TBooleanArray;
begin
  Res := ParseTypes(ObjectTypes, AllTypes);
  Result := '';
  for i := 0 to High(AllTypes) do
    if Res[i] then begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + '''' + AllTypes[i] + '''';
    end;
  if Result = '' then
    Result := '''''';
end;

function TSQLiteMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TSQLiteRecordSet.Create;
end;

class procedure TSQLiteMetaData.ParseTableSQL(const SQL: string; var TableName: string; var Members: TTableMembers);
var
  Lexem, s: string;
  Parser: TLiteParser;
  BracketCount, Code, i, c, j: integer;
  IsLast,
  InDataType,
  InDefault,
  InConstraints: boolean;
  NullClause: string;
begin
  Members.Columns := nil;
  Members.Constraints := nil;

  Parser := TLiteParser.Create(SQL);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    Parser.Uppered := False;
    Parser.QuotedString := True;

    if TableName = '' then begin
      repeat
        Code := Parser.GetNext(Lexem);
        if SameText(Lexem, 'table') or SameText(Lexem, 'exists') then begin
          repeat
            Code := Parser.GetNext(Lexem);
          until (Code = lcEnd) or not (SameText(Lexem, 'exists') or SameText(Lexem, 'if'));
          if Code = lcEnd then
            Exit;
          TableName := Lexem;
          Break;
        end;
      until Code = lcEnd;
    end;

    if Parser.ToLexem(lxLeftBracket) <> lcEnd then begin
      BracketCount := 0;
      IsLast := False;
      InConstraints := False;
    {$IFNDEF VER24P}
      c := -1;
    {$ENDIF}
      repeat
        Code := Parser.GetNext(Lexem);

        if (Code = lcIdent) or (Code >= lxSQLFirst) then begin
          s := LowerCase(Lexem);
          if (s = 'constraint') or (s = 'primary') or (s = 'foreign')
            or (s = 'check') or (s = 'unique')
          then begin
            InConstraints := True;
            break;
          end;
          c := Length(Members.Columns);
          SetLength(Members.Columns, c + 1);
          Members.Columns[c].Name := SQLiteInfo.UnQuote(Lexem);
        end
        else
          break;

        InDataType := True;
        InDefault := False;
        NullClause := '';
        while True do begin
          Code := Parser.GetNext(Lexem);

          if {$IFNDEF VER24P}(c >= 0) and {$ENDIF}((Code = lcIdent) or (Code >= lxSQLFirst)) then begin
            s := LowerCase(Lexem);
            if (s = 'primary') or (s = 'unique') then begin
              i := Length(Members.Constraints);
              SetLength(Members.Constraints, i + 1);
              if s = 'primary' then
                Members.Constraints[i].ConstraintType := ctPrimaryKey
              else
                Members.Constraints[i].ConstraintType := ctUnique;
              SetLength(Members.Constraints[i].ColumnInfo, 1);
              Members.Constraints[i].ColumnInfo[0].ColumnIndex := c;
              InDataType := False;
              if s = 'primary' then begin
                if LowerCase(Members.Columns[c].DataType) = 'integer' then
                  Members.Columns[c].IsAutoincrement := True;
                Parser.GetNextToken; // key
                Code := Parser.GetNext(Lexem);
              end;
            end
            else if (s = 'not') or (s = 'null') then begin
              if s = 'not' then
                NullClause := s
              else begin
                {$IFNDEF VER24P}if (c >= 0) then{$ENDIF}
                Members.Columns[c].NotNull := NullClause = 'not';
              end;

              InDataType := False;
            end
            else
            if (s = 'foreign') or (s = 'check') or (s = 'autoincrement') then
              InDataType := False
            else
            if s = 'default' then begin
              InDefault := True;
              InDataType := False;
              continue;
            end
            else
            if InDataType then begin
              if Members.Columns[c].DataType <> '' then
                Members.Columns[c].DataType := Members.Columns[c].DataType + ' ';
              Members.Columns[c].DataType := Members.Columns[c].DataType + Lexem;
              if LowerCase(Members.Columns[c].DataType) = 'autoinc' then
                Members.Columns[c].IsAutoincrement := True;
              continue;
            end;
          end;

          {$IFNDEF VER24P}if (c >= 0) then{$ENDIF}
            if InDefault then begin
              Members.Columns[c].Default := Lexem;
              InDefault := False;
            end
            else if InDataType then begin
              if (Code = lxRightBracket) and (BracketCount = 0) then begin
                IsLast := True;
                break
              end
              else if (Code = lxComma) and (BracketCount = 0) then
                break;

              Members.Columns[c].DataType := Members.Columns[c].DataType + Lexem;
            end;

          if Code = lcEnd then begin
            IsLast := True;
            break;
          end;

          if (Code = lxComma) and (BracketCount = 0) then
            break
          else if Code = lxLeftBracket then
            Inc(BracketCount)
          else if Code = lxRightBracket then
            if BracketCount = 0 then begin
              IsLast := True;
              break
            end
            else
              Dec(BracketCount);
        end;
      until IsLast;

      if not InConstraints then
        exit;

      IsLast := False;
      repeat
        s := LowerCase(Lexem);
        if s = 'constraint' then
          repeat
            Code := Parser.GetNext(Lexem);
            s := LowerCase(Lexem);
          until (Code = lcEnd) or (s = 'primary') or (s = 'foreign')
            or (s = 'check') or (s = 'unique');

        if (Code <> lcEnd) then begin
          if Parser.ToLexem(lxLeftBracket) = lcEnd then
            break;
          i := Length(Members.Constraints);
          SetLength(Members.Constraints, i + 1);
          if s = 'primary' then
            Members.Constraints[i].ConstraintType := ctPrimaryKey
          else if s = 'unique' then
            Members.Constraints[i].ConstraintType := ctUnique
          else if s = 'foreign' then
            Members.Constraints[i].ConstraintType := ctForeignKey
          else
            Members.Constraints[i].ConstraintType := ctCheck;
          Members.Constraints[i].ColumnInfo := nil;
          repeat
            Code := Parser.GetNext(Lexem);
            if (Code = lcEnd) or (Code = lxRightBracket) then
              break;
            if (Code <> lxComma) then begin
              for c := 0 to Length(Members.Columns) - 1 do
                if LowerCase(SQLiteInfo.UnQuote(Lexem)) = LowerCase(Members.Columns[c].Name) then begin
                  j := Length(Members.Constraints[i].ColumnInfo);
                  SetLength(Members.Constraints[i].ColumnInfo, j + 1);
                  Members.Constraints[i].ColumnInfo[j].ColumnIndex := c;

                  Code := Parser.GetNext(Lexem); // asc, desc
                  if Code <> lcEnd then begin
                    Members.Constraints[i].ColumnInfo[j].IsDesc := LowerCase(Lexem) = 'desc';
                    if Code = lxRightBracket then
                      Parser.Back;
                  end;
                  break;
                end;
            end;
          until False;
        end;

        while True do begin
          Code := Parser.GetNext(Lexem);

          if Code = lcEnd then begin
            IsLast := True;
            break;
          end;

          if (Code = lxComma) and (BracketCount = 0) then
            break
          else if (Code = lxLeftBracket) then
            Inc(BracketCount)
          else if (Code = lxRightBracket) then
            if BracketCount = 0 then begin
              IsLast := True;
              break
            end
            else
              Dec(BracketCount);
        end;

        if IsLast then
          break;

        Code := Parser.GetNext(Lexem);
      until Code = lcEnd;
    end;

  finally
    for i := Low(Members.Columns) to High(Members.Columns) do
      GetSQLTypeMeasures(Members.Columns[i].DataType, Members.Columns[i].Length, Members.Columns[i].Scale);

    for i := Low(Members.Constraints) to High(Members.Constraints) do begin
      if Members.Constraints[i].ConstraintType = ctPrimaryKey then begin
        Members.Constraints[i].Name := 'pk_' + LowerCase(TableName);
        if length(Members.Constraints[i].ColumnInfo) = 1 then
          if Members.Columns[Members.Constraints[i].ColumnInfo[0].ColumnIndex].DataType = 'integer' then
            Members.Columns[Members.Constraints[i].ColumnInfo[0].ColumnIndex].IsAutoincrement := True;
      end
      else begin
        case Members.Constraints[i].ConstraintType of
          ctUnique: Members.Constraints[i].Name := 'u_' + LowerCase(TableName);
          ctForeignKey: Members.Constraints[i].Name := 'fk_' + LowerCase(TableName);
          ctCheck: Members.Constraints[i].Name := 'c_' + LowerCase(TableName);
        end;

        for j := 0 to Length(Members.Constraints[i].ColumnInfo) - 1 do
          Members.Constraints[i].Name := Members.Constraints[i].Name + '_' + LowerCase(Members.Columns[Members.Constraints[i].ColumnInfo[j].ColumnIndex].Name);
      end;
    end;

    Parser.Free;
  end;
end;

procedure TSQLiteMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  List.Clear;
  List.Sorted := False;
  List.Add('MetaDataKinds');
  List.Add('Restrictions');
  List.Add('Tables');
  List.Add('Columns');
  List.Add('Indexes');
  List.Add('IndexColumns');
  List.Add('Constraints');
  List.Add('ConstraintColumns');
  List.Sorted := True;
end;

function TSQLiteMetaData.GetTables(Restrictions: TStrings): TData;
const
  SQL = 'SELECT name, type ' +
    'FROM sqlite_master ' +
    'WHERE %s lower(type) IN (%s) ORDER BY name';

{$IFDEF DBX_METADATA}
  dnRECNO         = 1;
  dnCATALOG_NAME  = 2;
  dnSCHEMA_NAME   = 3;
  dnTABLE_NAME    = 4;
  dnTABLE_TYPE    = 5;
{$ELSE}
  dnCATALOG_NAME  = 1;
  dnSCHEMA_NAME   = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
{$ENDIF}
var
  WhereClause, TableName, TableTypes, QuotedTypes: string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);

  WhereClause := '';
  AddWhere(WhereClause, 'lower(name)', LowerCase(TableName));
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  QuotedTypes := GetTypesForSQL(LowerCase(TableTypes), ['table', 'view']);

  FRecordSet.SetSQL(Format(SQL, [WhereClause, QuotedTypes]));
  FRecordSet.Open;

  CreateTablesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
  {$IFDEF DBX_METADATA}
    CopyRecord([1], [dnTABLE_NAME]);
    if LowerCase(FRecordSetHelper.FieldValues[2]) = 'table' then
      FMemDataHelper.FieldValues[dnTABLE_TYPE] := 1
    else if LowerCase(FRecordSetHelper.FieldValues[2]) = 'view' then
      FMemDataHelper.FieldValues[dnTABLE_TYPE] := 2
    else
      FMemDataHelper.FieldValues[dnTABLE_TYPE] := 0; // unknown
  {$ELSE}
    CopyRecord([1, 2], [dnTABLE_NAME, dnTABLE_TYPE]);
  {$ENDIF}
    FMemDataHelper.AppendRecord;
  end;

  FRecordSet.Close;
  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TSQLiteMetaData.CreateColumnsFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
{$ELSE}
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
{$ENDIF}
  AddField('TABLE_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
{$IFNDEF DBX_METADATA}
  AddField('POSITION', dtInt32);
  AddField('DATA_TYPE', dtString, 50);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  AddField('NULLABLE', dtInt32);
  AddField('DEFAULT_VALUE', dtMemo);
  AddField('PRIMARY_KEY', dtInt32);
{$ELSE}
  AddField('COLUMN_POSITION', dtInt32);
  AddField('COLUMN_TYPE', dtInt32);
  AddField('COLUMN_DATATYPE', dtInt32);
  AddField('COLUMN_TYPENAME', dtString, 50);
  AddField('COLUMN_SUBTYPE', dtInt32);
  AddField('COLUMN_LENGTH', dtInt32);
  AddField('COLUMN_PRECISION', dtInt32);
  AddField('COLUMN_SCALE', dtInt32);
  AddField('COLUMN_NULLABLE', dtInt32);
{$ENDIF}
  FMemData.InitFields;
end;

function TSQLiteMetaData.GetColumns(Restrictions: TStrings): TData;
const
  SQL = 'PRAGMA table_info(%s)';

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnCOLUMN_NAME   = 4;
  dnPOSITION      = 5;
  dnDATA_TYPE     = 6;
  dnLENGTH        = 7;
  dnPRECISION     = 8;
  dnSCALE         = 9;
  dnNULLABLE      = 10;
  dnDEFAULT       = 11;
  dnPRIMARY       = 12;
var
  TableName, ColumnName, DataType: string;
  Len, Scale: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  CreateColumnsFields;
  FMemData.Open;

  FRecordSet.SetSQL(Format(SQL, [LowerCase(SQLiteInfo.NormalizeName(TableName, True))]));
  FRecordSet.Prepare;
  if FRecordSet.GetCommand.CommandType = ctCursor then begin
    FRecordSet.Open;
    FRecordSetHelper.AllocBuffer;
    while FRecordSetHelper.NextRecord do begin
      FMemDataHelper.AllocBuffer;
      if (ColumnName = '') or (LowerCase(FRecordSetHelper.FieldValues[2]) = LowerCase(ColumnName)) then begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
        FMemDataHelper.FieldValues[dnCOLUMN_NAME] := FRecordSetHelper.FieldValues[2];
        FMemDataHelper.FieldValues[dnPOSITION] := FRecordSetHelper.FieldValues[1] + 1;
        DataType := VarToStr(FRecordSetHelper.FieldValues[3]);
        GetSQLTypeMeasures(DataType, Len, Scale);
        FMemDataHelper.FieldValues[dnDATA_TYPE] := DataType;
        if Scale <> -1 then begin
          FMemDataHelper.FieldValues[dnPRECISION] := Len;
          FMemDataHelper.FieldValues[dnSCALE] := Scale;
        end
        else if Len <> -1 then
          FMemDataHelper.FieldValues[dnLENGTH] := Len;
        FMemDataHelper.FieldValues[dnNULLABLE] := 1 - FRecordSetHelper.FieldValues[4];
        FMemDataHelper.FieldValues[dnDEFAULT] := FRecordSetHelper.FieldValues[5];
      {$IFNDEF DBX_METADATA}
        FMemDataHelper.FieldValues[dnPRIMARY] := FRecordSetHelper.FieldValues[6];
      {$ENDIF}
        FMemDataHelper.AppendRecord;
      end;
    end;
    FRecordSet.Close;
  end;
  FRecordSet.UnPrepare;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TSQLiteMetaData.GetProcedures(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TSQLiteMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TSQLiteMetaData.GetIndexes(Restrictions: TStrings): TData;
const
  SQL = 'PRAGMA index_list(%s)';

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;

var
  TableName, IndexName, IndName: string;
  Unique: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  CreateIndexesFields;
  FMemData.Open;

  FRecordSet.SetSQL(Format(SQL, [LowerCase(SQLiteInfo.NormalizeName(TableName, True))]));
  FRecordSet.Prepare;
  if FRecordSet.GetCommand.CommandType = ctCursor then begin
    FRecordSet.Open;
    FRecordSetHelper.AllocBuffer;
    while FRecordSetHelper.NextRecord do begin
      IndName := VarToStr(FRecordSetHelper.FieldValues[2]);
      if (IndexName <> '') and (IndexName <> IndName) then
        continue;
      Unique := FRecordSetHelper.FieldValues[3];
      FMemDataHelper.AllocBuffer;
      FMemDataHelper.InitRecord;
      FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
      FMemDataHelper.FieldValues[dnINDEX_NAME] := IndName;
      FMemDataHelper.FieldValues[dnUNIQUE] := Unique;
      FMemDataHelper.AppendRecord;
    end;
    FRecordSet.Close;
    end;
  FRecordSet.UnPrepare;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TSQLiteMetaData.GetIndexColumns(Restrictions: TStrings): TData;
const
  SQL1 = 'PRAGMA index_list(%s)';
  SQL2 = 'PRAGMA index_info(%s)';

{$IFDEF DBX_METADATA}
  dnRECNO = 1;
  dnCATALOG_NAME = 2;
  dnSCHEMA_NAME = 3;
  dnTABLE_NAME = 4;
  dnINDEX_NAME = 5;
  dnCOLUMN_NAME = 6;
  dnPOSITION = 7;
  dnPKEY_NAME = 8;
  dnINDEX_TYPE = 9;
  dnSORT_ORDER = 10;
  dnFILTER = 11;
{$ELSE}
  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnINDEX_CATALOG   = 4;
  dnINDEX_SCHEMA    = 5;
  dnINDEX_NAME      = 6;
  dnCOLUMN_NAME     = 7;
  dnPOSITION        = 8;
  dnSORT_ORDER      = 9;
{$ENDIF}

var
  TableName, IndexName, Uniqueness: string;
  IndName, Unique: string;
  IndexInfo: TSQLiteRecordSet;
  IndexInfoHelper: TDataHelper;
{$IFDEF DBX_METADATA}
  IndType: Integer;
{$ENDIF}
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  Uniqueness := Trim(Restrictions.Values['UNIQUE']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  CreateIndexColumnsFields;
  FMemData.Open;

  FRecordSet.SetSQL(Format(SQL1, [SQLiteInfo.NormalizeName(TableName, True)]));
  FRecordSet.Prepare;
  if FRecordSet.GetCommand.CommandType = ctCursor then begin
    FRecordSet.Open;
    FRecordSetHelper.AllocBuffer;

    IndexInfo := CreateRecordSet as TSQLiteRecordSet;
    IndexInfoHelper := TDataHelper.Create(IndexInfo);
    try
    {$IFNDEF LITE}
      IndexInfo.DataTypeMap.Enabled := False;
    {$ENDIF}
      IndexInfo.SetProp(prFetchAll, True);
      IndexInfo.SetProp(prFlatBuffers, False);
      IndexInfo.SetConnection(FRecordSet.GetCommand.GetConnection);
      IndexInfo.SetTransaction(FRecordSet.GetCommand.GetTransaction);

      while FRecordSetHelper.NextRecord do begin
        IndName := VarToStr(FRecordSetHelper.FieldValues[2]);
        if (IndexName <> '') and (IndexName <> IndName) then
          continue;
        Unique := VarToStr(FRecordSetHelper.FieldValues[3]);
        if ((Uniqueness = '1') and ((Unique = '1') or (Pos('unique', LowerCase(Unique)) > 0))) or
          (((Uniqueness = '0') or (Uniqueness = '')) and ((Unique = '0') or (Pos('unique', LowerCase(Unique)) = 0)))
        then begin
          IndexInfo.SetSQL(Format(SQL2, [IndName]));
          IndexInfo.Prepare;
          if IndexInfo.GetCommand.CommandType = ctCursor then begin
            IndexInfo.Open;
            IndexInfoHelper.AllocBuffer;
            while IndexInfoHelper.NextRecord do begin
              FMemDataHelper.AllocBuffer;
              FMemDataHelper.InitRecord;
              FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
              FMemDataHelper.FieldValues[dnINDEX_NAME] := IndName;
              FMemDataHelper.FieldValues[dnCOLUMN_NAME] := IndexInfoHelper.FieldValues[3];
              FMemDataHelper.FieldValues[dnPOSITION] := IndexInfoHelper.FieldValues[1] + 1;
              FMemDataHelper.FieldValues[dnSORT_ORDER] := 'ASC';
            {$IFDEF DBX_METADATA}
              if Pos('unique', LowerCase(Unique)) > 0 then
                IndType := 2
              else
                IndType := 1;
              FMemDataHelper.FieldValues[dnINDEX_TYPE] := IndType;
            {$ENDIF}
              FMemDataHelper.AppendRecord;
            end;
            IndexInfo.Close;
          end;
          IndexInfo.UnPrepare;
        end;
      end;
      FRecordSet.Close;
    finally
      IndexInfoHelper.Free;
      IndexInfo.Free;
    end;
  end;
  FRecordSet.UnPrepare;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TSQLiteMetaData.GetConstraints(Restrictions: TStrings): TData;
const
  SQL = 'SELECT sql FROM sqlite_master ' +
    'WHERE Lower(name) = ''%s'' AND type = ''table''';

  dnTABLE_CATALOG      = 1;
  dnTABLE_SCHEMA       = 2;
  dnTABLE_NAME         = 3;
  dnCONSTRAINT_NAME    = 4;
  dnCONSTRAINT_TYPE    = 5;
  dnINDEX_CATALOG      = 6;
  dnINDEX_SCHEMA       = 7;
  dnINDEX_NAME         = 8;

var
  TableName: string;
  DDL: string;
  Members: TTableMembers;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  CreateConstraintsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  FRecordSet.SetSQL(Format(SQL, [LowerCase(TableName)]));
  FRecordSet.Open;

  FRecordSetHelper.AllocBuffer;
  if FRecordSetHelper.NextRecord then begin
    DDL := VarToStr(FRecordSetHelper.FieldValues[1]);
    if DDL <> '' then begin
      ParseTableSQL(DDL, TableName, Members);

      for i := 0 to Length(Members.Constraints) - 1 do begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
        FMemDataHelper.FieldValues[dnCONSTRAINT_NAME] := Members.Constraints[i].Name;
        case Members.Constraints[i].ConstraintType of
          ctPrimaryKey: FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := 'PRIMARY KEY';
          ctUnique: FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := 'UNIQUE';
          ctForeignKey: FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := 'FOREIGN KEY';
          ctCheck: FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := 'CHECK';
        end;
        FMemDataHelper.AppendRecord;
      end;
    end;
  end;

  FRecordSet.Close;
  FMemData.SetToBegin;
  Result := FMemData;
end;

function TSQLiteMetaData.GetConstraintColumns(Restrictions: TStrings): TData;
const
  SQL = 'SELECT sql FROM sqlite_master ' +
    'WHERE Lower(name) = ''%s'' AND type = ''table''';

  dnTABLE_CATALOG      = 1;
  dnTABLE_SCHEMA       = 2;
  dnTABLE_NAME         = 3;
  dnCONSTRAINT_NAME    = 4;
  dnCOLUMN_NAME        = 5;
  dnCOLUMN_POSITION    = 6;

var
  TableName, ConstraintName: string;
  DDL: string;
  Members: TTableMembers;
  i, j: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  CreateConstraintColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  FRecordSet.SetSQL(Format(SQL, [LowerCase(TableName)]));
  FRecordSet.Open;

  FRecordSetHelper.AllocBuffer;
  if FRecordSetHelper.NextRecord then begin
    DDL := VarToStr(FRecordSetHelper.FieldValues[1]);
    if DDL <> '' then begin
      ParseTableSQL(DDL, TableName, Members);

      for i := 0 to Length(Members.Constraints) - 1 do begin
        if (ConstraintName <> '') and (ConstraintName <> Members.Constraints[i].Name) then
          continue;

        for j := 0 to Length(Members.Constraints[i].ColumnInfo) - 1 do begin
          FMemDataHelper.InitRecord;
          FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
          FMemDataHelper.FieldValues[dnCONSTRAINT_NAME] := Members.Constraints[i].Name;
          FMemDataHelper.FieldValues[dnCOLUMN_NAME] := Members.Columns[Members.Constraints[i].ColumnInfo[j].ColumnIndex].Name;
          FMemDataHelper.FieldValues[dnCOLUMN_POSITION] := j + 1;
          FMemDataHelper.AppendRecord;
        end;
      end;
    end;
  end;

  FRecordSet.Close;
  FMemData.SetToBegin;
  Result := FMemData;
end;

{ TLiteSQLInfo }

function TSQLiteInfo.LeftQuote: Char;
begin
  Result := '[';
end;

function TSQLiteInfo.RightQuote: Char;
begin
  Result := ']';
end;

function TSQLiteInfo.IdentCase: TIdentCase;
begin
  Result := icMixed;
end;

function TSQLiteInfo.ParamQuoteAllowed: boolean;
begin
  Result := False;
end;

function TSQLiteInfo.IsQuoted(const Value: string): boolean;
var
  l: integer;
begin
  l := Length(Value);
  if (l <= 1) then
    Result := False
  else
    Result := (CharInSet(Value[1], ['`', '"', '['])) and (CharInSet(Value[l], ['`', '"', ']']));
end;

procedure TSQLiteInfo.SplitObjectName(const Name: string; out Info: TSQLObjectInfo);
begin
  inherited;

  Info.Catalog := Info.Schema;
  Info.Schema := '';
end;

procedure TSQLiteInfo.ParseTablesInfo(const SQL: string; TablesInfo: TCRTablesInfo);
var
  i: integer;
  ObjectInfo: TSQLObjectInfo;
begin
  inherited ParseTablesInfo(SQL, TablesInfo);

  for i := 0 to TablesInfo.Count - 1 do begin
    TSQLiteTableInfo(TablesInfo[i]).TableNameFull := TablesInfo[i].TableName;
    SplitObjectName(TablesInfo[i].TableNameFull, ObjectInfo);
    TablesInfo[i].TableName := ObjectInfo.Name;
  end;
end;

{$IFNDEF LITE}

{ TSQLiteLoader }

constructor TSQLiteLoader.Create;
begin
  inherited;
end;

destructor TSQLiteLoader.Destroy;
begin
  inherited;
end;

procedure TSQLiteLoader.CreateCommand;
begin
  FCommand := TSQLiteCommand.Create;
end;

procedure TSQLiteLoader.DoPrepare;
{$IFDEF UNIDACPRO}
var
  Value: variant;
{$ENDIF}
begin
  inherited;

  if UsedConnection <> nil then begin
  {$IFDEF UNIDACPRO}
    UsedConnection.GetProp(prAutoCommit, Value);
    FAutoCommitIsUsed := Value and FAutoCommit;
  {$ELSE}
    FAutoCommitIsUsed := FAutoCommit;
  {$ENDIF}
  end;

  if FAutoCommitIsUsed then begin
    if UsedTransaction.GetInTransaction then
      UsedTransaction.Commit;
    UsedTransaction.StartTransaction;
  end;
end;

procedure TSQLiteLoader.DoLoadRow;
begin
  inherited;

  if FAutoCommitIsUsed then
    if (FLoadedRows mod FAutoCommitRowCount) = 0 then begin
      if UsedTransaction.GetInTransaction then
        UsedTransaction.Commit;
      UsedTransaction.StartTransaction;
    end;
end;

function TSQLiteLoader.UsedConnection: TSQLiteConnection;
begin
  Result := nil;
  if FConnection <> nil then
    Result := TSQLiteConnection(FConnection);
end;

function TSQLiteLoader.UsedTransaction: TSQLiteTransaction;
begin
  if FTransaction <> nil then
    Result := TSQLiteTransaction(FTransaction)
  else
    Result := TSQLiteTransaction(UsedConnection.GetInternalTransaction);
end;

function TSQLiteLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      FAutoCommit := Value;
    prAutoCommitRowCount:
      FAutoCommitRowCount := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TSQLiteLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      Value := FAutoCommit;
    prAutoCommitRowCount:
      Value := FAutoCommitRowCount;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TSQLiteLoader.Finish;
begin
  if FAutoCommitIsUsed then
    if UsedTransaction.GetInTransaction then
      UsedTransaction.Commit;

  inherited;
end;

{ TSQLiteBackup }

constructor TSQLiteBackup.Create(SourceConnection, DestinationConnection: TSQLiteConnection);
begin
  inherited Create;

  if SourceConnection = nil then
    raise Exception.Create(SBackupSourceNotConnected);
  if DestinationConnection = nil then
    raise Exception.Create(SBackupDestinationNotConnected);
  if SourceConnection = DestinationConnection then
    raise Exception.Create(SBackupConnectionsEqual);

  FSourceDatabaseName := 'main';
  FDestinationDatabaseName := 'main';
  FPagesPerStep := -1;
  FWaitWhenLocked := False;
  FWaitDelay := 250;
  FWaitTimeout := 250;

  FSrcConnection := SourceConnection;
  FDestConnection := DestinationConnection;
end;

procedure TSQLiteBackup.Init;
begin
  FBackupHandle := FSrcConnection.FAPI.sqlite3_backup_init(
    FDestConnection.FAPI.SQLite, PAnsiChar(AnsiString(CRFunctions.UTF8Encode(WideString(FDestinationDatabaseName)))),
    FSrcConnection.FAPI.SQLite, PAnsiChar(AnsiString(CRFunctions.UTF8Encode(WideString(FSourceDatabaseName)))));

  if FBackupHandle = nil then
    FSrcConnection.Check((FSrcConnection.FAPI.sqlite3_errcode(FDestConnection.FAPI.SQLite)));
end;

function TSQLiteBackup.Step: boolean;
var
  Res, Code: integer;
  d, dd: Cardinal;
  ErrMsg: string;
begin
  Result := True;

  d := {$IFDEF VER17P}TThread.{$ENDIF}{$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
  repeat
    Code := FSrcConnection.FAPI.sqlite3_backup_step(FBackupHandle, FPagesPerStep);
    Res := Code and $FF;
    case Res of
      SQLITE_BUSY,
      SQLITE_LOCKED:
        if FWaitWhenLocked then begin
          dd := {$IFDEF VER17P}TThread.{$ENDIF}{$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
          if Abs(dd - d) <= FWaitTimeout then
            Sleep(FWaitDelay)
          else
            Break;
        end
        else
          Break;
      SQLITE_DONE:
        Result := False;
      SQLITE_OK:;
    end;
  until not (Res in [SQLITE_BUSY, SQLITE_LOCKED]);

  if not (Res in [SQLITE_DONE, SQLITE_OK]) then begin
    FDestConnection.FAPI.GetLiteErrorMsg(ErrMsg);
    if ErrMsg <> 'not an error' then
      FDestConnection.Check(Code)
    else
      Result := False;
  end;
end;

procedure TSQLiteBackup.Finish;
begin
  FSrcConnection.Check(FSrcConnection.FAPI.sqlite3_backup_finish(FBackupHandle));
end;

function TSQLiteBackup.GetPageCount: integer;
begin
  Result := FSrcConnection.FAPI.sqlite3_backup_pagecount(FBackupHandle);
end;

function TSQLiteBackup.GetRemaining: integer;
begin
  Result := FSrcConnection.FAPI.sqlite3_backup_remaining(FBackupHandle);
end;

procedure TSQLiteBackup.Backup;
begin
  Init;
  try
    while Step do
      if Assigned(FOnProgress) then
        FOnProgress(GetPageCount, GetRemaining);
  finally
    Finish;
  end;
end;
{$ENDIF}

initialization

  hLockConnectCount := TCriticalSection.Create;
  SQLiteInfo := TSQLiteInfo.Create(nil);

finalization
  SQLiteInfo.Free;
  hLockConnectCount.Free;

end.
