//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  MyClasses
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MyClassesUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs, Variants,
{$IFNDEF NODBACCESS}
  DB, DBAccess,
{$ENDIF}
  CLRClasses,
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
  MemData, CRTypes, CRAccess, CRParser, CRVio,
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  MyCall, MySqlApi;
{$ELSE}
  MyCallUni, MySqlApiUni;
{$ENDIF}

const
  SResultParamName = 'Result';
  MaxPlaceHoldersCount = 65535;

type
{ EMyError }
//  TMyErrorProc = procedure (E: {$IFDEF LITE}Exception{$ELSE}EDAError{$ENDIF}; var Fail: boolean) of object;

{$IFDEF NODBACCESS}
  EMyError = class(ECRError)
{$ELSE}
  EMyError = class(EDAError)
{$ENDIF}
  protected
  {$IFNDEF NODBACCESS}
    FLineNumber: integer;
  {$ENDIF}
    FErrorLevel: string;
  public
    constructor Create(ErrorCode: integer; const Msg: string); overload;
    constructor Create(ErrorCode: integer; const Msg: string; const ErrorLevel: string); overload;
  {$IFNDEF NODBACCESS}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}

  {$IFNDEF NODBACCESS}
    property LineNumber: integer read FLineNumber;
  {$ENDIF}
   property ErrorLevel: string read FErrorLevel;
  end;

{ TMyErrors }

  TMyErrors = class(TCRObjectList)
  private
    function GetError(Index: integer): EMyError;
  public
    property Errors[Index: integer]: EMyError read GetError; default;
  end;

{ TMySQLConnection }

  TMySQLRecordSet = class;

  TMyProtocol = (mpDefault, mpTCP, mpPipe, mpMemory, mpSSL, mpHttp, mpSSLHttp);

  TExecuteProc = procedure(const NativeSQL: string) of object;

  TMyWarningEvent = procedure(Errors: TMyErrors) of object;

  TParamInfo = record
    Name: string;
    Direction: TParamDirection;
    ParamType: string;
    Unsigned: boolean;
    Size: integer;
  end;
  TParamInfos = array of TParamInfo;

  TMySQLConnection = class (TCRConnection)
  private
    FLock: TCriticalSection;

  protected
    FOnDialogAuthPlugin: TMyDialogAuthPluginEvent;
    FOnWarning: TMyWarningEvent;

    FMySQLAPI: TMySQLAPI;
    FMySQL: PMYSQL_CON;

  { Session }
    FPort: integer;
    FCompress: boolean;
    FUseUnicode: boolean;
    FCharset: string;
    FUsedCharset: string;
    FIsUtf8: boolean;
    FConnectionTimeout: integer;
    FProtocol: TMyProtocol;
    FOptimizedBigInt: boolean;
    FNullForZeroDelphiDate: boolean;
    FEmbedded: boolean;
    FDirect: boolean;
    FIPVersion: TIPVersion;
    FCheckPrecision: boolean;
    FCheckBackslashes: boolean;
    FNeedBackslashes: boolean;
    FInteractive: boolean;
    FEmbParams: TStringList;
    FDBCharLength: word;
    //FDoError: TMyErrorProc;

  { Server info }
    FServerVer: string;
    FServerPrimaryVer, FServerMinorVer, FServerReleaseVer: integer;
    FIsMariaDB: boolean;

    function GetIsClient41: boolean;
    function GetIsServer41: boolean;
  {$IFDEF MSWINDOWS}
    function DetectSystemEncoding: string;
  {$ENDIF}
    procedure DetectUsedCharser;

    procedure SetDatabase(const Value: string);
    procedure TryToApplyCharset;
    procedure CheckNoBackslashEscapesMode;

    function RequestWarnings: TMyErrors;

    procedure SetMySQLAPI;
    procedure SetEmbParamsStr(const EmbParams: string);

    function EscapeAndQuoteVar(const Data: variant; UseUnicode: boolean{$IFDEF HAVE_COMPRESS}; SendBlobCompressed: boolean{$ENDIF}): AnsiString;

    property EnableBCD;
    property EnableFMTBCD;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Lock;
    procedure Release;

    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
  {$IFNDEF LITE}
    function GetLoaderClass: TCRLoaderClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    procedure Check(const Status: TIntPtrObj; Component: TObject); overload; virtual;
    procedure Check(const Status: integer; Component: TObject); overload; virtual;
    procedure Check(Component: TObject); overload; virtual;
    procedure MySQLError(Component: TObject);
    procedure DoError(E: Exception; var Fail: boolean); override;

    procedure CheckStmt(stmt: PMYSQL_STMT; const Status: integer; Component: TObject); virtual;
    procedure MySQLStmtError(stmt: PMYSQL_STMT; Component: TObject);

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;
    procedure Ping; override;
    procedure SetIsolationLevel(const Value: TCRIsolationLevel); override;

    procedure AppendValueToSQL(SQL: AnsiStringBuilder;
      const DataType: word; const Value: variant;
      const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}: boolean); overload;

    procedure AppendValueToSQL(SQL: WideStringBuilder;
      const DataType: word; const Value: variant;
      const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}: boolean); overload;

    procedure AppendValueToSQL(SQLA: AnsiStringBuilder; SQLW: WideStringBuilder;
      const DataType: word; const Value: variant;
      const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}: boolean); overload;

    function EscapeAndQuoteStr(const p: PAnsiChar; Len: integer): AnsiString; overload;
    function EscapeAndQuoteStr(const p: AnsiString; Len: integer): AnsiString; overload;
    function EscapeAndQuoteWideStr(const p: PWideChar; Len: integer{in characters}): WideString; overload;
    function EscapeAndQuoteWideStr(const p: WideString; Len: integer): WideString; overload;

    procedure InitCommandProp(Command: TCRCommand); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    function CheckIsValid: boolean; override;
  {$IFNDEF NODBACCESS}
    procedure ReturnToPool; override;
  {$ENDIF}

    procedure CheckMySQLLib;
    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;

    function GetThreadId: cardinal;

    function GetLastWarningCount: cardinal;

    function IsFractionSupported: boolean;

    property IsClient41: boolean read GetIsClient41;
    property IsServer41: boolean read GetIsServer41;
    property ServerPrimaryVer: integer read FServerPrimaryVer;
    property ServerMinorVer: integer read FServerMinorVer;
    property ServerReleaseVer: integer read FServerReleaseVer;
    property IsMariaDB: boolean read FIsMariaDB;
    property UseUnicode: boolean read FUseUnicode write FUseUnicode;
    property IsUtf8: boolean read FIsUtf8 write FIsUtf8;
    property UsedCharset: string read FUsedCharset;
    property DBCharLength: word read FDBCharLength;
    property Reconnected: boolean read FReconnected write FReconnected;

    property MySQLAPI: TMySQLAPI read FMySQLAPI;
    property MySQL: PMYSQL_CON read FMySQL;

    property OnDialogAuthPlugin: TMyDialogAuthPluginEvent write FOnDialogAuthPlugin;
    property OnWarning: TMyWarningEvent read FOnWarning write FOnWarning;
  end;

{ TMySQLCommand }

  TMySQLCommand = class (TCRCommand)
  private
    // Params processing (see PlaceParamValues)
    FCachedSQL: TStringList; // SQL without comments. One line for each parameter
    FCreateConnection: boolean;
    FUseCreateConnection: boolean;

    procedure FillCachedSQL;
    function GetParam(const ParamIdx: integer; var IOParamCnt: integer): TParamDesc;

  protected
    FConnection: TMySQLConnection;
    FConnectionSwap: TMySQLConnection; // Used to store main connection on FetchAll = False
    FCommandTimeout: integer;

    FCursorState: TCursorState;

    FEnableBoolean: boolean;

    FRowsAffected: integer;
    FRowsAffectedPrev: integer; // For SP with out param
    FRowsAffectedNext: integer;
    FSQLResult: PMYSQL_RES;
    FSQLResultNext: PMYSQL_RES;
    FOpenNext: boolean;

    FStoredProc: boolean;
    FCanReadParams: boolean;
    FIsSelectParams: boolean;
    FRequestResultset: boolean; // Indicate current command owner - MyCommand(False) or MyDataSet(True)

    FPrepared: boolean;
    Fstmt: PMYSQL_STMT;
    FOnExecute: TExecuteProc;
    FBreakExecCS: TCriticalSection;
    FWaitForBreak: boolean;

    procedure Check(const Status: TIntPtrObj); overload; virtual;
    procedure Check(const Status: integer); overload; virtual;
    procedure Check; overload; virtual;
    procedure CheckStmt(const Status: integer); virtual;

    procedure QuerySwapConnection;
    procedure ReleaseSwapConnection(ForbidQueries: boolean = False);

    function CalcRowsAffected: integer;
    procedure UseNextResult;

    procedure SetLimit0;

    procedure PlaceParamValues(SQL: AnsiStringBuilder);
    procedure GetSPParams(const Name: string; out IsFunc: boolean; out ParamList: string; out ReturnParam: string);
    procedure DescribeParams(const ParamList: string; const IsResult: boolean; var ParamsInfo: TParamInfos; ParamDescs: TParamDescs; const EnableBoolean: boolean);
    procedure InternalExecute;

    function GetBatchSQL(Iters, Offset: integer): string; override;
    procedure InternalExecuteBatch(Iters, Offset: integer); override;

    function IsLabelSyntax(Code: integer; PrevCode: integer): boolean; override;

    property Params;

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;

    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; override;
    procedure CreateProcCallByParams(const Name: string; IsFunc: boolean; const ParamsInfo: TParamInfos; out ASQL, AUserSQL: string);
    function ForceCreateSPParams: boolean; override;
    procedure Execute; override;
    procedure Close; override;

    procedure SetConnection(Value: TCRConnection); override;
    procedure SetSQL(const Value: string); override;

    class function GetTableInfoClass: TTableInfoClass; override;
    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    procedure BreakExec; override;

    property BatchOwner;
    property BatchOffset: Integer read FBatchOffset;

    property EnableBCD;
    property EnableFMTBCD;

    property OnExecute: TExecuteProc read FOnExecute write FOnExecute;
  end;

{ TMySQLRecordset }

  TMySQLFieldDesc = class (TCRFieldDesc)
  private
    FMySQLType: TMySqlFieldType; // Native MySQL datatype
    FIsUnsigned: boolean; // "Sign" flag for MySQL datatype
    FIsPrimaryKey: boolean;

    FIsBinary: boolean; // for string and blob fields
    FIsCurrentTimestamp: boolean;

  public
    property MySQLType: TMySqlFieldType read FMySQLType;
    property IsUnsigned: boolean read FIsUnsigned;
    property IsPrimaryKey: boolean read FIsPrimaryKey;

    property IsBinary: boolean read FIsBinary;
    property IsCurrentTimestamp: boolean read FIsCurrentTimestamp;
  end;

  TMyTableInfo = class(TCRTableInfo)
  protected
    FMaxTimestamp: TDateTime;
    FTableNameFull: string;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FTimestampField: TMySQLFieldDesc;
    function GetTableNameFull: string; override;
    procedure SetTableNameFull(const Value: string); override;

  public
    property MaxTimestamp: TDateTime read FMaxTimestamp write FMaxTimestamp;
    property TimestampField: TMySQLFieldDesc read FTimestampField;
  end;

  TMySQLInfo = class(TSQLInfo)
  public
    function LeftQuote: Char; override;
    function RightQuote: Char; override;
    function IsCursorSQLType(Code: Integer): boolean; override;

    function IsQuoted(const Value: string): boolean; override;
    function Quote(const Value: string; const LeftQ: Char; const RightQ: Char): string; override;
    function UnQuote(const Value: string): string; override;

    procedure SplitObjectName(const Name: string; out DataBase: string; out ObjName: string); reintroduce; overload;
    procedure SplitObjectName(const Name: string; out Info: TSQLObjectInfo);  overload; override;

    function NamesFromList(List: TStrings; NormalizedName: boolean = True; Delimiter: string = ';'): string; override;
  end;

  TMyOpenNextState = (
    osNotChecked,
    osMoreResults,     // mysql_next_result = 0   Successful and there are more results
    osNoMoreResults,   // mysql_next_result = -1  Successful and there are no more results
    osError            // mysql_next_result > 0   An error occurred
  );
  TMySQLFetchState = (fsBof, fsMiddle, fsEof);

  TMySQLRecordset = class (TCRRecordSet)
  private
    FIsClient41: boolean;
    FUseUnicode: boolean;
    FIsUtf8: boolean;
    FOptimizedBigInt: boolean;
    FNullForZeroDelphiDate: boolean;
    FServerPrimaryVer: integer;
    FServerMinorVer: integer;
    FServerReleaseVer: integer;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FMySQLAPI: TMySQLAPI;

    FFetchBnd: TMYSQL_BINDS; // PreparedClient

    // for fetch performance optimization
    FNulls: TBytes;
    FValueLens: TLenArr;
    FUtf8Buff: TBytes;
    FWsBuff: TBytes;

    function GetIsClient41: boolean;
    function GetUseUnicode: boolean;
    function GetIsUtf8: boolean;
    function GetOptimizedBigInt: boolean;
    function GetNullForZeroDelphiDate: boolean;
    function GetServerPrimaryVer: integer;
    function GetServerMinorVer: integer;
    function GetServerReleaseVer: integer;

    procedure ReadParams;
    function CreateBlob(Field: TFieldDesc): TBlob;
  {$IFDEF HAVE_COMPRESS}
    procedure SetCompressed(FieldNo: integer; Blob: TBlob);
  {$ENDIF}

    procedure ReadFieldValue(Field: TMySQLFieldDesc; pData: IntPtr; RecNo: integer; const row: PMYSQL_ROW);
    procedure GetDataFromRow(pData: IntPtr; RecNo: integer; const row: PMYSQL_ROW); overload;
    procedure GetDataFromRowPrepClient(const pData: IntPtr);
    procedure FillTablesAliases(Parser: TSQLParser);
    procedure InternalCreateFieldDescs(Parser: TSQLParser);

  protected
    FCommand: TMySQLCommand;
    FCreateConnection: boolean;

    FNullForZeroDate: boolean;
    FFieldsAsString: boolean;
    FNumberFieldsAsString: boolean;
    FEnableBoolean: boolean;
    FBinaryAsString: boolean;
    FUseHandler: boolean;

    FOpenNextState: TMyOpenNextState;
    FIsCanFastClose: boolean;

    procedure Check(const Status: IntPtr); overload; virtual;
    procedure Check(const Status: integer); overload; virtual;
    procedure Check; overload; virtual;

    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;

  { Open / Close }
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

  { Fetch }
    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); override;
    procedure ProcessFetchedBlock(Block: PBlockHeader; FetchBack: boolean); override;
    procedure IncFetchBlockOffset(var FetchBlockOffset: integer; FieldDesc: TFieldDesc);
    function IsNeedFetchBlock(const DataType: word): boolean;
    procedure AllocFetchBuffer; override;
    function NeedInitFieldsOnFetch: boolean; override;

    procedure FreeResult(const TryGetNextResult, TryToFinishFetch: boolean);
    procedure DrainResults;

  { Fields }
    procedure CreateFieldDescs; override;
    function CanUseAllKeyFields: boolean; override;
    function IdentityFieldIsData: boolean; override;
    procedure FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray;
      ForceUseAllKeyFields: boolean); override;
    procedure FillDataFieldDescs(out DataFieldDescs: TFieldDescArray;
      ForceUseAllKeyFields: boolean); override;
    function RequiredReInitFields: boolean; override;

  { Fetch }
    function GetCorrectedDataType(const FieldDesc: TFieldDesc): word;

    function IsFractionSupported: boolean;

    //PreCached FConection properties
    property IsClient41: boolean read GetIsClient41;
    property UseUnicode: boolean read GetUseUnicode;
    property IsUtf8: boolean read GetIsUtf8;
    property OptimizedBigInt: boolean read GetOptimizedBigInt;
    property NullForZeroDelphiDate: boolean read GetNullForZeroDelphiDate;
    property ServerPrimaryVer: integer read GetServerPrimaryVer;
    property ServerMinorVer: integer read GetServerMinorVer;
    property ServerReleaseVer: integer read GetServerReleaseVer;
  public
    constructor Create; override;

  { Fields }
    function GetFieldDescType: TFieldDescClass; override;
    procedure ExplicitInitFields; override;
    function GetTimestampField: TCRFieldDesc;
    procedure DetectIdentityField; override;
    procedure DetectFieldType({$IFNDEF LITE}const FieldName: string;{$ENDIF} DBType: Word; DBLength, DBScale: Integer;
      const LengthInBytes: cardinal; const TableName: AnsiString;
      out InternalType, SubDataType: word; out Length, Scale: Integer; out Fixed: boolean);

  { Open / Close }
    procedure Reopen; override;
    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override;
    procedure Disconnect; override;


  {$IFNDEF LITE}
  { Data Type Map }
    function GetMapOnDemandConverter(const FieldName: string; DataType: Word; DBType: Word; DBLength, DBScale: Integer; out MapRule: TCRMapRule): TOnDemandConverter; override;
  {$ENDIF}

  { Navigation }
    procedure SetToEnd; override;
    procedure BreakFetch; override;

    function CheckNextResult: boolean;
    function CanDisconnect: boolean; override;
    function RowsReturn: boolean; override;

    function GetSQLObjectIndex(const ObjName: string; out IsAlias: boolean): integer; overload; // MySQL specific - Search by name and by alias!!!
    function GetSQLObjectIndex(const ObjName: string): integer; overload; // MySQL specific - Search by name and by alias!!!
    function AddSQLObjectIfNeed(const SQLObjName: string; const IsView: boolean): integer; // Add new SQLObject, if need. If SQLObject already present then return its index
    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    function IsCaseSensitive: boolean; override;
  end;

{ TMySQLTransaction }

  TMySQLTransaction = class (TCRTransaction)
  protected
    function GetConnection: TMySQLConnection;

  public
    constructor Create; override;
    destructor Destroy; override;

  { Transaction control }
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure Savepoint(const Name: string); override;
    procedure ReleaseSavepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;
  end;

{$IFNDEF LITE}
{ TMySQLMetaData }

  TMySQLMetaData = class (TCRMetaData)
  protected
    FRecordSet: TMySQLRecordSet;

    function CreateRecordSet: TCRRecordSet; override;
    function GetTypesForSQL(const ObjectTypes: string): string;

    function GetTables(Restrictions: TStrings): TData; override;
    procedure CopyTablesData(Restrictions: TStrings); virtual;

    function GetColumns(Restrictions: TStrings): TData; override;
    procedure CopyColumnsData(Restrictions: TStrings); virtual;

    function GetProcedures(Restrictions: TStrings): TData; override;
    function GetProcedureParameters(Restrictions: TStrings): TData; override;

    function GetIndex(Restrictions: TStrings; Columns: boolean): TData;
    function GetIndexes(Restrictions: TStrings): TData; override;
    procedure CopyIndexesData(Restrictions: TStrings); virtual;

    function GetIndexColumns(Restrictions: TStrings): TData; override;
    procedure CopyIndexColumnsData(Restrictions: TStrings); virtual;

    function GetConstraints(Restrictions: TStrings): TData; override;

    function GetConstraintColumns(Restrictions: TStrings): TData; override;

    function GetDatabases(Restrictions: TStrings): TData; override;
    procedure CopyDatabasesData(Restrictions: TStrings);
  end;
{$ENDIF}

{ TMySQLLoader }

  _TMyDuplicateKeys = (_dkNone, _dkIgnore, _dkReplace);

  TMySQLLoader = class(TCRLoader)
  protected
    FConnection: TMySQLConnection;
    FInsHeader: string;
    FInsHeaderLen: Integer;
    FBuffer: AnsiStringBuilder;
    FRowBuffer: AnsiStringBuilder; // CurrentRow
    FValues: array of Variant;

    FLock: boolean;
    FDelayed: boolean;
    FRowsPerQuery: integer;
    FDebug: boolean;
    //FCommandTimeout: integer;
    FDuplicateKeys: _TMyDuplicateKeys;

    procedure Clear;
    procedure Flush;
    procedure ExecSQL(const SQL: AnsiString);

    procedure CalculateRowBuffer;
    procedure AppendRowBufferToBuffer;

    procedure SetConnection(Value: TCRConnection); override;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); override;
    function IsPutColumnDataAllowed: boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Prepare; override;
    procedure Reset; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;
  end;

  function IsLargeDataTypeUsed(const FieldDesc: TFieldDesc): boolean; overload;
  function IsLargeDataTypeUsed(const ParamDesc: TParamDesc): boolean; overload;
  procedure CheckDirParam(EmbParams: TStrings; const ParamName: string);

const
{$IFNDEF MSWINDOWS}
  MaxWord = $FFFF;
{$ENDIF}
{$IFNDEF MSWINDOWS}
  Base10: double = 10;
{$ELSE}
{$IFDEF WIN64}
  Base10: extended = 10;
{$ELSE}
  Base10 = 10;
{$ENDIF}
{$ENDIF}
  SizeForFieldsWithZeroLength: integer = MaxWord - 5; // For compatibility with TClientDataSet
  DefValProtocol = mpDefault;

var
  ZeroDate: TDateTime;
  __Strings65535ToMemo: boolean = True;  // remove at 15.08.07
  __UseNewBooleanParamType: boolean = False;
  MySQLInfo: TMySQLInfo;

var
  CurrentProjectOutputDir: string = '';

implementation


uses
{$IFDEF PERF_COUNTER}
  Debug,
{$ENDIF}
  FMTBcd, DateUtils,
{$IFNDEF UNIDACPRO}
  MySqlApiDirect, MySqlSession,
{$ELSE}
  MySqlApiDirectUni, MySqlSessionUni,
{$ENDIF}
  SysConst, Math, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CRProps, CRFunctions, CRTimeStamp, MemUtils, DAConsts,
{$IFNDEF UNIDACPRO}
  MyProps, MyParser, MyConsts, MySqlNet, MyDataTypeMap;
{$ELSE}
  MyPropsUni, MyParserUni, MyConstsUni, MySqlNetUni, MyDataTypeMapUni;
{$ENDIF}

// Convert Utf8 buffer to WideString buffer with or without null terminator.
function Utf8ToWs(
  const Dest: TValueArr; MaxDestBytes{w/wo #0}: Cardinal;
  const Source: TValueArr; SourceBytes: Cardinal;
  const AddNull: boolean): Cardinal{bytes w/wo #0};
begin
  Result := {$IFNDEF NEXTGEN}{$IFNDEF FPC}CRFunctions{$ELSE}System{$ENDIF}{$ELSE}System{$ENDIF}.Utf8ToUnicode(
    PWideChar(Dest), MaxDestBytes div sizeof(WideChar), Source, SourceBytes);

  if not AddNull then
    Dec(Result)
  else begin
  {$IFNDEF FPC}
    if Result = 0 then begin
      Marshal.WriteInt16(Dest, 0);
      Inc(Result);
    end;
  {$ELSE}
    Marshal.WriteInt16(Dest, (Result - 1) * sizeof(WideChar), 0);
  {$ENDIF}
  end;

  Result := Result * sizeof(WideChar);
end;

{$IFNDEF FPC}
function WsToAnsi(
  const Dest: TValueArr; MaxDestBytes{w/wo #0}: Cardinal;
  const Source: TValueArr; SourceBytes: Cardinal;
  const AddNull: boolean): Cardinal{bytes w/wo #0};
begin
  Result := LocaleCharsFromUnicode(CP_ACP, 0, PWideChar(Source), SourceBytes div sizeof(WideChar), Dest, MaxDestBytes, nil, nil);

  if AddNull then begin
    Marshal.WriteByte(Dest, Result, 0);
    Inc(Result);
  end;
end;

function AnsiToWs(
  const Dest: TValueArr; MaxDestBytes{w/wo #0}: Cardinal;
  const Source: TValueArr; SourceBytes: Cardinal;
  const AddNull: boolean): Cardinal{bytes w/wo #0};
begin
  Result := UnicodeFromLocaleChars(CP_ACP, 0, Source, SourceBytes, PWideChar(Dest), MaxDestBytes) * sizeof(WideChar);

  if AddNull then begin
    Marshal.WriteInt16(Dest, Result, 0);
    Result := Result + sizeof(WideChar);
  end;
end;
{$ENDIF}

procedure CheckDirParam(EmbParams: TStrings; const ParamName: string);
var
  s: string;
begin
  s := EmbParams.Values[ParamName];
  if s = '' then
    Exit;

  if (s <> '') and ((s[1] = '/') or (s[1] = '\')) then
    s := Copy(s, 2, MaxInt);

  if (s <> '') and (s[1] = '.') and // relative path
    (ParamName = '--basedir') and
    (CurrentProjectOutputDir <> '')
  then
    s := IncludeTrailingBackslash(CurrentProjectOutputDir) + s;

  s := StringReplace(s, '\', '/', [rfReplaceAll]);
  EmbParams.Values[ParamName] := s;
end;

function IsLargeDataTypeUsed(const FieldDesc: TFieldDesc): boolean;
begin
  Result := TMySQLFieldDesc(FieldDesc).FMySQLType in [
    FIELD_TYPE_GEOMETRY,
    FIELD_TYPE_TINY_BLOB, // TINYBLOB, TINYTEXT
    FIELD_TYPE_MEDIUM_BLOB, // MEDIUMBLOB, MEDIUMTEXT
    FIELD_TYPE_LONG_BLOB, // LONGBLOB, LONGTEXT
    FIELD_TYPE_JSON, // JSON
    FIELD_TYPE_BLOB // BLOB, TEXT
  ];
end;

function IsLargeDataTypeUsed(const ParamDesc: TParamDesc): boolean;
begin
  case ParamDesc.GetDataType of
    dtBlob, dtMemo, dtWideMemo:
      Result := True;
    else
      Result := False;
  end;
end;

function GenerateTableName(const CatalogName, TableName, DefaultCatalogName: string): string;
begin
  if (CatalogName <> '') and (CatalogName <> DefaultCatalogName) then
    Result := Format('%s.%s',
      [MySQLInfo.QuoteIfNeed(CatalogName), MySQLInfo.QuoteIfNeed(TableName)])
  else
    Result := Format('%s', [MySQLInfo.QuoteIfNeed(TableName)])
end;

procedure ConvertMySQLTypeToInternalFormat(DBType: Word; FieldLengthInBytes: cardinal; Decimals: Integer;
  EnableBoolean, OptimizedBigInt, BinaryAsString,
  UseUnicode, EnableBCD, EnableFMTBCD, CheckPrecision: boolean;
  out InternalType: word; out Fixed: boolean);
var
  Prec: cardinal;
begin
  Fixed := False;

  case DBType of // Must be sync with TCustomMyDataSet.SetNumberRange
    // Integer fields
    myDecimal: begin // DECIMAL
      if CheckPrecision then begin
        Prec := FieldLengthInBytes - 1;
        if Decimals > 0 then
          Dec(Prec);
        if EnableBCD and (Prec <= MaxBcdPrecision - MaxBcdScale) and
          (Decimals <= MaxBcdScale)
        then
          InternalType := dtBCD
        else
        if EnableFMTBCD then
          InternalType := dtFmtBCD
        else
          InternalType := dtFloat;
      end
      else begin
        if EnableFMTBCD then
          InternalType := dtFmtBCD
        else
        if EnableBCD then
          InternalType := dtBCD
        else
          InternalType := dtFloat;
      end;
    end;
    myBit: // MySQL 5.0
      if (FieldLengthInBytes < 32{bit}) and OptimizedBigInt then
        InternalType := dtInt32
      else
        InternalType := dtInt64;
    myTiny: // TINYINT
      if (FieldLengthInBytes = 1) and EnableBoolean then
        InternalType := dtBoolean
      else
        InternalType := dtInt8;
    myTinyUnsigned: // TINYINT
      if (FieldLengthInBytes = 1) and EnableBoolean then
        InternalType := dtBoolean
      else
        InternalType := dtUInt8;
    mySmall: // SMALLINT
      InternalType := dtInt16;
    mySmallUnsigned: // SMALLINT
      InternalType := dtUInt16;
    myInt: // INT
      InternalType := dtInt32;
    myIntUnsigned: // INT
      InternalType := dtUInt32;
    myMedium, myMediumUnsigned: // MEDIUMINT
      // 'Unsigned' flag is not used. dtInt32 may contain both signed or unsigned 24-bit values
      InternalType := dtInt32;
    myBigint:  // BIGINT
      if (FieldLengthInBytes <= 11) and OptimizedBigInt then
        InternalType := dtInt32
      else
        InternalType := dtInt64;
    myBigintUnsigned:  // BIGINT
      if (FieldLengthInBytes <= 11) and OptimizedBigInt then
        InternalType := dtInt32
      else
        InternalType := dtInt64;

    // Float fields
    myFloat: // FLOAT
      InternalType := dtFloat;
    myDouble: // DOUBLE
      InternalType := dtFloat;

    // DateTime fields
    myTimestamp: // TIMESTAMP14, TIMESTAMP12, TIMESTAMP8, TIMESTAMP6
      InternalType := dtDateTime;
    myDate: // DATE
      InternalType := dtDate;
    myTime: // TIME
      InternalType := dtTime;
    myDateTime: // DATETIME
      InternalType := dtDateTime;
    myYear: // YEAR4, YEAR2
      InternalType := dtUInt16;

    // String fields
    myChar: begin
      InternalType := dtString;
      Fixed := True;
    end;
    myVarchar:
      InternalType := dtString;

    myBinary: begin
      if not BinaryAsString then
        InternalType := dtBytes
      else
        InternalType := dtString;
      Fixed := True;
    end;
    myVarbinary:
      if not BinaryAsString then
        InternalType := dtVarBytes
      else
        InternalType := dtString;

    myEnum: begin
      InternalType := dtString;
      Fixed := True;
    end;
    mySet: begin
      InternalType := dtString;
      Fixed := True;
    end;
    myNull:
      InternalType := dtString;

    myTinyBlob, myMediumBlob, myLongBlob, myBlob:
      InternalType := dtBlob;
    myTinyText, myMediumText, myLongText, myText, myJSON:
      InternalType := dtMemo;
  else
    Assert(False, 'Unknown MySQL datatype (' + IntToStr(DBType) + ')');
  end;

  if UseUnicode {and not IsBinary }then
    case InternalType of
      dtString:
        InternalType := dtWideString;
      dtMemo:
        InternalType := dtWideMemo;
    end;
end;

{ EMyError }

{$IFDEF NODBACCESS}
procedure DatabaseError(const Message: string; Component: TComponent = nil);
begin
  if Assigned(Component) and (Component.Name <> '') then
    raise Exception.Create(Format('%s: %s', [Component.Name, Message])) else
    raise Exception.Create(Message);
end;

procedure DatabaseErrorFmt(const Message: string; const Args: array of const;
  Component: TComponent = nil);
begin
  DatabaseError(Format(Message, Args), Component);
end;

constructor EMyError.Create(ErrorCode: integer; const Msg: string);
begin
  inherited Create(ErrorCode, Msg);

  FErrorLevel  := 'Error';
end;

constructor EMyError.Create(ErrorCode: integer; const Msg: string; const ErrorLevel: string);
begin
  inherited Create(ErrorCode, Msg);

  FErrorLevel  := ErrorLevel;
end;

{$ELSE}
constructor EMyError.Create(ErrorCode: integer; const Msg: string);
var
  s: string;
  i, j: integer;
begin
  inherited Create(ErrorCode, Msg);

  FErrorLevel  := 'Error';

  case ErrorCode of
    ER_PARSE_ERROR: begin
      try
        i := Length(Msg);
        //j := 0;
        while i >= 1 do begin
          if (Msg[i] >= '0') and (Msg[i] <= '9') then begin
            // search Line
            j := i;
            Dec(i);
            while (i >= 1) and (Msg[i] >= '0') and (Msg[i] <= '9') do
              Dec(i);
            s := Copy(Msg, i + 1, j - i);
            FLineNumber := StrToInt(s);
            Break;
          end;
          Dec(i);
        end;
      except
        // Silent exception handling
      end;
    end;
  end;
end;

constructor EMyError.Create(ErrorCode: integer; const Msg: string; const ErrorLevel: string);
begin
  inherited Create(ErrorCode, Msg);

  FErrorLevel  := ErrorLevel;
end;
{$ENDIF}

{$IFNDEF NODBACCESS}
function EMyError.IsFatalError: boolean;
begin
  Result :=
    (ErrorCode = CR_CONN_HOST_ERROR) or
    (ErrorCode = CR_SERVER_LOST) or
    (ErrorCode = CR_SERVER_GONE_ERROR) or
    (ErrorCode = ER_SERVER_SHUTDOWN);
end;

function EMyError.IsKeyViolation: boolean;
begin
  Result := (ErrorCode = ER_DUP_ENTRY);
end;
{$ENDIF}

{ TMyErrors }

function TMyErrors.GetError(Index: integer): EMyError;
begin
  Result := EMyError(inherited Items[Index]);
end;

function ChangeDecimalSeparator(const Value: Variant): string;
var
  i: integer;
begin
  if TVarData(Value).VType = varCurrency then
    Result := CurrToStr(TVarData(Value).VCurrency)
  else
    Result := string(Value);

  if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
    for i := 1 to Length(Result) do
      if Result[i] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator then begin
        Result[i] := '.';
        Break;
      end;
end;

{ TMySQLConnection }

constructor TMySQLConnection.Create;
begin
  inherited;

  FDirect := True;
  FIPVersion := ivIPv4;
  FMySQLAPI := MyAPIDirect;
  FMySQL := nil;

  FConnectionTimeout := DefValConnectionTimeout;
  FProtocol := DefValProtocol;
  FEmbParams := TStringList.Create;
  FCheckBackslashes := False;
  FNeedBackslashes := True;
  FPort := MYSQL_PORT;
  FDBCharLength := 1;
  FLock := TCriticalSection.Create;
end;

destructor TMySQLConnection.Destroy;
begin
  FLock.Free;
  FEmbParams.Free;
  inherited;
end;

procedure TMySQLConnection.Lock;
begin
  FLock.Enter;
end;

procedure TMySQLConnection.Release;
begin
  FLock.Leave;
end;

function TMySQLConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TMySQLCommand;
end;

function TMySQLConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TMySQLRecordSet;
end;

function TMySQLConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TMySQLTransaction;
end;

{$IFNDEF LITE}

function TMySQLConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TMySQLLoader;
end;

function TMySQLConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TMySQLMetaData;
end;

class function TMySQLConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TMySQLCommand.GetMapRulesClass;
end;

{$ENDIF}

procedure TMySQLConnection.SetDatabase(const Value: string);
begin
  if FDatabase <> Value then begin
    FDatabase := Value;
    if FConnected and (Value <> '') then
      Check(FMySQLAPI.mysql_select_db(FMySQL, FDatabase), Component);
  end;
end;

procedure TMySQLConnection.Check(const Status: TIntPtrObj; Component: TObject);
begin
  if Status = nil then
    MySQLError(Component);
end;

procedure TMySQLConnection.Check(const Status: integer; Component: TObject);
begin
  if Status <> 0 then
    MySQLError(Component);
end;

procedure TMySQLConnection.Check(Component: TObject);
begin
  if FMySQLAPI.mysql_errno(FMySQL) <> 0 then
    MySQLError(Component);
end;

procedure TMySQLConnection.MySQLError(Component: TObject);
var
  Fail: boolean;
  Err: EMyError;
  Code: cardinal;
  Msg: string;
begin
  Code := FMySQLAPI.mysql_errno(FMySQL);
  Msg := FMySQLAPI.mysql_error(FMySQL);

  Err := EMyError.Create(Code, Msg);
  try
  {$IFNDEF NODBACCESS}
    Err.Component := Component;
  {$ENDIF}

    Fail := True;

    if Assigned(OnError) then
      DoError(Err, Fail);
    if Fail then
      raise Err
    else
      Abort;
  finally
    if not Fail then
      Err.Free;
  end;
end;

procedure TMySQLConnection.CheckStmt(stmt: PMYSQL_STMT; const Status: integer; Component: TObject);
begin
  if Status <> 0 then
    MySQLStmtError(stmt, Component);
end;

procedure TMySQLConnection.MySQLStmtError(stmt: PMYSQL_STMT; Component: TObject);
var
  Fail: boolean;
  Err: EMyError;
  Code: cardinal;
  Msg: string;
begin
  Code := FMySQLAPI.mysql_stmt_errno(stmt);
  Msg := FMySQLAPI.mysql_stmt_error(stmt);

  Err := EMyError.Create(Code, Msg);
{$IFNDEF NODBACCESS}
  Err.Component := Component;
{$ENDIF}
  Fail := True;
  if Assigned(OnError) then
    DoError(Err, Fail);
  if Fail then
    raise Err
  else
    Abort;
end;

{$IFDEF MSWINDOWS}
function TMySQLConnection.DetectSystemEncoding: string;
var
  cp: word;
begin
  cp := GetACP;
  case cp of
    437: Result := 'cp850';
    850: Result := 'cp850';
    852: Result := 'cp852';
    858: Result := 'cp850';
    866: Result := 'cp866';
    874: Result := 'tis620';
    932: Result := 'cp932';
    936: Result := 'gbk';
    949: Result := 'euckr';
    950: Result := 'big5';
    1250: Result := 'latin2';
    1251: Result := 'cp1251';
    1252: Result := 'latin1';
    1253: Result := 'greek';
    1254: Result := 'latin5';
    1255: Result := 'hebrew';
    1256: Result := 'cp1256';
    1257: Result := 'cp1257';
    10000: Result := 'macroman';
    10001: Result := 'sjis';
    10002: Result := 'big5';
    10008: Result := 'gb2312';
    10021: Result := 'tis620';
    10029: Result := 'macce';
    20107: Result := 'swe7';
    20127: Result := 'ascii';
    20866: Result := 'koi8r';
    20932: Result := 'ujis';
    20936: Result := 'gb2312';
    20949: Result := 'euckr';
    21866: Result := 'koi8u';
    28591: Result := 'latin1';
    28592: Result := 'latin2';
    28597: Result := 'greek';
    28598: Result := 'hebrew';
    28599: Result := 'latin5';
    28603: Result := 'latin7';
    38598: Result := 'hebrew';
    51932: Result := 'ujis';
    51936: Result := 'gb2312';
    51949: Result := 'euckr';
    51950: Result := 'big5';
    65001: Result := 'utf8';
  else
    Result := '';
  end;
end;
{$ENDIF}

procedure TMySQLConnection.DetectUsedCharser;
begin
  if FUseUnicode then begin
    if FCharset = 'utf8mb4' then
      FUsedCharset := 'utf8mb4'
    else
      FUsedCharset := 'utf8';
  end
  else begin
    FUsedCharset := FCharset;
  {$IFDEF IS_UTF8}
    if FCharset = '' then
      FUsedCharset := 'utf8';
  {$ELSE}
    {$IFDEF MSWINDOWS}
      if FCharset = '' then
        FUsedCharset := DetectSystemEncoding;
    {$ENDIF}
  {$ENDIF}
  end;
end;

procedure TMySQLConnection.TryToApplyCharset;
begin
  if (FServerPrimaryVer >= 4) and ((FServerPrimaryVer <> 4) or (FServerMinorVer <> 0)) then begin // Ignore 4.0 and below for ODAC compatibility
    DetectUsedCharser;
    if FUsedCharset <> '' then
      ExecuteSQL(Format('SET NAMES ''%s''', [FUsedCharset]));

    FDBCharLength := GetCharSetWidth(FUsedCharset);
  end
  else begin
    FUseUnicode := False; // To prevent errors on decode
    FIsUtf8 := False;
    FDBCharLength := 1;
  end;
end;

procedure TMySQLConnection.CheckNoBackslashEscapesMode;

  function GetSQLMode: string;
  var
    RecordSet: TCRRecordSet;
    RecBuf: IntPtr;
    v: variant;
  begin
    RecordSet := OpenRecordSet('SELECT @@session.sql_mode');
    try
      if RecordSet = nil then
        Exit;

      RecordSet.AllocRecBuf(RecBuf);
      try
        RecordSet.GetNextRecord(RecBuf);
        if not RecordSet.GetNull(RecordSet.Fields[0], RecBuf) then begin
          RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
          Result := v;
        end;
      finally
        RecordSet.FreeRecBuf(RecBuf);
      end;
    finally
      ReleaseRecordSet(RecordSet);
    end;
  end;

begin
  if FCheckBackslashes then
    FNeedBackslashes := Pos('NO_BACKSLASH_ESCAPES', UpperCase(GetSQLMode)) <= 0
  else
    FNeedBackslashes := True;
end;

function TMySQLConnection.RequestWarnings: TMyErrors;
var
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
  Warning: EMyError;
  level, Text, Code: string;
begin
  Result := nil;

  RecordSet := OpenRecordSet('SHOW WARNINGS');
  try
    if RecordSet = nil then
      Exit;

    RecordSet.AllocRecBuf(RecBuf);
    try
      repeat
        RecordSet.GetNextRecord(RecBuf);

        if RecordSet.Eof then
          Exit;

        if not RecordSet.GetNull(RecordSet.Fields[2], RecBuf) then begin
          RecordSet.GetFieldAsVariant(RecordSet.Fields[2], RecBuf, v);
          Text := v;

          if not RecordSet.GetNull(RecordSet.Fields[1], RecBuf) then begin
            RecordSet.GetFieldAsVariant(RecordSet.Fields[1], RecBuf, v);
            Code := v;
          end
          else
            Code := '0';

          if not RecordSet.GetNull(RecordSet.Fields[0], RecBuf) then begin
            RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
            Level := v;
          end
          else
            Level := 'Warning';

          Warning := EMyError.Create(StrToInt(Code), Text, Level);

          if Result = nil then
            Result := TMyErrors.Create;
          Result.Add(Warning);
        end;
      until False;
    finally
      RecordSet.FreeRecBuf(RecBuf);
    end;
  finally
    ReleaseRecordSet(RecordSet);
  end;
end;

procedure TMySQLConnection.DoError(E: Exception; var Fail: boolean);
var
  Reconnect: boolean;
{$IFNDEF LITE}
  Reexecute: boolean;
  ConnLostCause: TConnLostCause;
{$ENDIF}
begin
  if not Additional then
    inherited
  else begin
    Reconnect := False;
  {$IFNDEF LITE}
    Reexecute := False;
  {$ENDIF}
    if Assigned(OnError) then
      OnError(E, Fail, Reconnect{$IFNDEF LITE}, Reexecute{$ENDIF}, 0{$IFNDEF LITE}, ConnLostCause{$ENDIF});
  end;
end;

procedure TMySQLConnection.Connect(const ConnectString: string);
var
  MemoryName: string;

  procedure SetOptions;
  var
    Pr: MYSQL_PROTOCOL_TYPE;
    PrL: integer;
    p: Integer;
  begin
    // Setting options
    Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_OPT_CONNECT_TIMEOUT, FConnectionTimeout), Component);

    DetectUsedCharser;
    FIsUtf8 := (FUsedCharset = 'utf8') or (FUsedCharset = 'utf8mb4');
    if FIsUtf8 then
      Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_SET_CHARSET_NAME, 'utf8'), Component);

    if FMySQLAPI is TMySQLAPIDirect then begin
      if not FIsUtf8 and (FUsedCharset <> '') then
        Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_SET_CHARSET_NAME, FUsedCharset), Component);
      TMySQLAPIDirect(FMySQLAPI).SetOnDialogAuthPlugin(FMySQL, FOnDialogAuthPlugin);
      TMySQLAPIDirect(FMySQLAPI).SetIsUTF8(FMySQL, FIsUtf8);

    {$IFNDEF LITE}
      TMySQLAPIDirect(FMySQLAPI).SetIOHandler(FMySQL, FIOHandler);
      TMySQLAPIDirect(FMySQLAPI).SetOptions(FMySQL, FHttpOptions, FProxyOptions, FSSLOptions);
    {$ELSE}
      TMySQLAPIDirect(FMySQLAPI).SetOptions(FMySQL, nil, nil, FSSLOptions);
    {$ENDIF}
      TMySQLAPIDirect(FMySQLAPI).SetIPVersion(FMySQL, FIPVersion);
    end;

  {$IFNDEF LITE}
    FSSLOptions.Enabled := (FIOHandler <> nil) and (FProtocol <> mpHttp);
    FHttpOptions.Enabled := FProtocol in [mpHttp, mpSSLHttp];
  {$ENDIF}

    // Protocol
    if FProtocol in [mpSSL, mpSSLHttp] then begin
      FSSLOptions.Key := StringReplace(FSSLOptions.Key, '\', '/', [rfReplaceAll]);
      FSSLOptions.Cert := StringReplace(FSSLOptions.Cert, '\', '/', [rfReplaceAll]);
      FSSLOptions.CA := StringReplace(FSSLOptions.CA, '\', '/', [rfReplaceAll]);

      // Without checking as error in non-direct implementation
      FMySQLAPI.mysql_ssl_set(FMySQL, FSSLOptions.Key, FSSLOptions.Cert, FSSLOptions.CA, '', FSSLOptions.Cipher);
    end
    else
  {$IFNDEF LITE}
    if FProtocol in [mpHttp, mpSSLHttp] then begin
      if not (FMySQLAPI is TMySQLAPIDirect) then
        DatabaseError('Wrong protocol');
    end
    else
  {$ENDIF}
    if FProtocol <> mpDefault then begin
      if IsClient41 then begin
        Pr := MYSQL_PROTOCOL_DEFAULT;
        case FProtocol of
          mpTCP:
            Pr := MYSQL_PROTOCOL_TCP;
          mpPipe:
            Pr := MYSQL_PROTOCOL_PIPE;
          mpMemory:
            Pr := MYSQL_PROTOCOL_MEMORY;
        else
          DatabaseError('Wrong protocol');
        end;
        PrL := Integer(Pr);
        Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_OPT_PROTOCOL, PrL), Component);

        if FProtocol = mpMemory then begin
          p := Pos(',', FServer);
          if p > 0 then begin
            MemoryName := Copy(FServer, p + 1, Length(FServer));
            Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_SHARED_MEMORY_BASE_NAME, MemoryName), Component);
          end;
        end;
      end
      else
        if FProtocol = mpPipe then begin
          PrL := 0;
          Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_OPT_NAMED_PIPE, PrL), Component);
        end;
    end;

  end;

var
  Flags: cardinal;
  Server, Username, Password, Database: string;
  p: Integer;
begin
  if not FConnected then begin
    FServerVer := '';
    FServerPrimaryVer := 0;
    FServerMinorVer := 0;
    FIsMariaDB := False;
    CheckMySQLLib;
    try
      if FMySQL <> nil then
        Disconnect;

      Assert(FMySQL = nil);

      // Getting FMySQL
      FMySQL := FMySQLAPI.mysql_init(FMySQL);
      Check(FMySQL, Component);
      SetOptions;

      // Other opt
      Flags := CLIENT_FOUND_ROWS;
      if IsClient41 then
        Flags := Flags + CLIENT_MULTI_STATEMENTS;

      if FCompress then
        Flags := Flags + CLIENT_COMPRESS;

      if FInteractive then
        Flags := Flags + CLIENT_INTERACTIVE;

      if FServer = '' then
        Server := 'localhost'
      else begin
        p := Pos(',', FServer);
        if p > 0 then
          Server := Copy(FServer, 1, p - 1)
        else
          Server := FServer;
      end;

      if FIsUtf8 then begin
        Database := string({$IFNDEF NEXTGEN}CRFunctions.{$ELSE}System.{$ENDIF}Utf8Encode(WideString(FDatabase)));
        Username := string({$IFNDEF NEXTGEN}CRFunctions.{$ELSE}System.{$ENDIF}Utf8Encode(WideString(FUsername)));
        Password := string({$IFNDEF NEXTGEN}CRFunctions.{$ELSE}System.{$ENDIF}Utf8Encode(WideString(FPassword)));
      end
      else begin
        Database := FDatabase;
        Username := FUsername;
        Password := FPassword;
      end;

      Check(FMySQLAPI.mysql_real_connect(FMySQL, Server, Username, Password,
        Database, FPort, '', Flags), Component);
      FConnected := True;
      FNativeConnection := True;
      inherited;

      FServerVer := FMySQLAPI.mysql_get_server_info(FMySQL);

      DecodeVersion(FServerVer, FServerPrimaryVer, FServerMinorVer, FServerReleaseVer, FIsMariaDB);
      TryToApplyCharset;
      CheckNoBackslashEscapesMode;
      if FIsolationLevel <> ilReadCommitted then
        SetIsolationLevel(FIsolationLevel);
    except
      on EFailOver do;
      else begin
        try
          Disconnect;
        except
        end;
        raise;
      end;
    end;
  end;
end;

procedure TMySQLConnection.Disconnect;
begin
  if (FMySQL <> nil) and FNativeConnection then
    FMySQLAPI.mysql_close(FMySQL);
  FMySQL := nil;
  FConnected := False;
end;

procedure TMySQLConnection.Ping;
begin
  MySQLAPI.SetTimeout(MySQL, FConnectionTimeout);
  if MySQLAPI.mysql_ping(MySQL) <> 0 then // connection lost
    MySQLError(Self);
end;

procedure TMySQLConnection.SetIsolationLevel(const Value: TCRIsolationLevel);
begin
  if FConnected then
    case Value of
      ilReadCommitted:
        ExecuteSQL('SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED');
      ilReadUnCommitted:
        ExecuteSQL('SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED');
      ilRepeatableRead:
        ExecuteSQL('SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ');
      ilIsolated:
        ExecuteSQL('SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE');
    else
      raise Exception.Create(SUnsupportedIsolationLevel);
    end;

  FIsolationLevel := Value;
end;

procedure TMySQLConnection.AppendValueToSQL(SQL: AnsiStringBuilder;
  const DataType: word; const Value: variant;
  const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}: boolean);
begin
  AppendValueToSQL(SQL, nil, DataType, Value, IsNull, UseUnicode {$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF});
end;

procedure TMySQLConnection.AppendValueToSQL(SQL: WideStringBuilder;
  const DataType: word; const Value: variant;
  const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}: boolean);
begin
  AppendValueToSQL(nil, SQL, DataType, Value, IsNull, UseUnicode {$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF});
end;

procedure TMySQLConnection.AppendValueToSQL(SQLA: AnsiStringBuilder; SQLW: WideStringBuilder;
  const DataType: word; const Value: variant;
  const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}: boolean);

  procedure Append({$IFNDEF LITE}{$IFDEF VER7P}const{$ENDIF}{$ENDIF} Value: string);
  begin
    if SQLA <> nil then
      SQLA.Append(AnsiString(Value))
    else
      SQLW.Append(WideString(Value));
  end;

  procedure AppendA({$IFNDEF LITE}const{$ENDIF} Value: AnsiString);
  begin
    if SQLA <> nil then
      SQLA.Append(Value)
    else
      SQLW.Append(WideString(Value));
  end;

  procedure DivMod(Dividend: Int64; Divisor: integer; out Result, Remainder: integer);
  begin
    Result := Dividend div Divisor;
    Remainder := Dividend mod Divisor;
  end;

{$IFDEF VER6}
  function VarToDateTime(const V: Variant): TDateTime;
  begin
    if TVarData(V).VType = varDate then
      Result := TVarData(V).VDate
    else
      Result := Variants.VarToDateTime(V);
  end;
{$ENDIF}

var
  MSecs, Seconds: Int64;
  HourCount, MinCount, SecCount: integer;
  c: Currency;

begin
  if IsNull then
    Append('NULL')
  else
  if (DataType in [dtDate, dtTime, dtDateTime]) and
     (VarToDateTime(Value) = ZeroDate)
  then
    Append('''0000-00-00 00:00:00''')
  else begin
    case DataType of
      dtUnknown, dtBytes, dtVarBytes, dtExtVarBytes, dtBlob: // Unicode not used in any case
        AppendA(EscapeAndQuoteVar(Value, False {$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}));
      dtString, dtWideString, dtExtString, dtExtWideString, dtMemo, dtWideMemo:
        AppendA(EscapeAndQuoteVar(Value, UseUnicode {$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}));
      dtDateTime, dtSQLTimeStamp: begin
        Append('''');
          MSecs := Round(TDateTime(Value) * MSecsPerDay) mod 1000;

        if MSecs > 0 then begin
          Append(FormatDateTime('YYYY-MM-DD HH:NN:SS.ZZZ', Value));
        end
        else begin
          Append(FormatDateTime('YYYY-MM-DD HH:NN:SS', Value));
        end;
        Append('''');
      end;
      dtDate: begin
        Append('''');
        Append(FormatDateTime('YYYY-MM-DD', Value));
        Append('''');
      end;
      dtTime: begin
        Seconds := Round(TDateTime(Value) * SecsPerDay);
        if Seconds >= 0 then
          Append('''')
        else begin
          Append('''-');
          Seconds := - Seconds;
        end;

        DivMod(Seconds, 60{Min/Hour} * 60{Sec/Min}, HourCount, SecCount);
        DivMod(SecCount, 60{Sec/Min}, MinCount, SecCount);

        Append(IntToStr(HourCount));
        if MinCount >= 10 then
          Append(':')
        else
          Append(':0');
        Append(IntToStr(MinCount));
        if SecCount >= 10 then
          Append(':')
        else
          Append(':0');
        Append(IntToStr(SecCount));

        MSecs := Round(TDateTime(Value) * MSecsPerDay) mod 1000;
        if MSecs > 0 then begin
          Append('.');
          Append(IntToStr(MSecs));
        end;

        Append('''');
      end;
      dtCurrency, dtSingle, dtFloat, dtExtended:
        Append(ChangeDecimalSeparator(Value));
      dtBCD: begin
        c := Value;
        Append(ChangeDecimalSeparator(CurrToStr(c)));
      end;
      dtFmtBCD:
        Append(ChangeDecimalSeparator(Value));
      dtBoolean: begin// see MySQL reference manual 10.1.4 Boolean Values
        if (ServerPrimaryVer >= 5) and __UseNewBooleanParamType then begin
          if Boolean(Value) then
            Append('b''1''')
          else
            Append('b''0''');
        end
        else begin
          if Boolean(Value) then
            Append('1')
          else
            Append('0');
        end;
      end;
      else
        Append(Value);
    end;
  end;
end;

function TMySQLConnection.EscapeAndQuoteVar(const Data: variant; UseUnicode: boolean{$IFDEF HAVE_COMPRESS}; SendBlobCompressed: boolean{$ENDIF}): AnsiString;

  procedure SendBlobAsIs(Blob: TBlob);
  begin
    if Blob.Size = 0 then begin
      Result := '''''';
      Exit;
    end;

    Blob.Defrag;
    if not UseUnicode then
      if Blob.IsUnicode then
        Result := AnsiString(EscapeAndQuoteWideStr(PWideChar(PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader))), Blob.FirstPiece.Used shr 1))
      else
        Result := EscapeAndQuoteStr(PAnsiChar(Blob.FirstPiece) + sizeof(TPieceHeader), Blob.FirstPiece.Used)
    else
      if Blob.IsUnicode then
        Result := CRFunctions.UTF8Encode(EscapeAndQuoteWideStr(PWideChar(PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader))), Blob.FirstPiece.Used shr 1))
      else
        Result := CRFunctions.AnsiToUtf8(EscapeAndQuoteStr(PAnsiChar(Blob.FirstPiece) + sizeof(TPieceHeader), Blob.FirstPiece.Used));
  end;

var
  pData: PVarData;
  Blob: TBlob;
  s: AnsiString;
  ws: WideString;
{$IFDEF HAVE_COMPRESS}
  b: TBytes;
  SwapBlob: TBlob;
{$ENDIF}
begin
  pData := @Data;
  if pData.VType = varSharedObject then begin
    Assert(pData.VPointer <> nil);
    // Assert(TObject(pData.VPointer) is TBlob); // - trial
    Blob := pData.VPointer;
    Blob.Defrag;

  {$IFDEF HAVE_COMPRESS}
    if SendBlobCompressed then begin
      // (compr.a) Blob is not TCompressedBlob
      //   (compr.a.1) can't compress (Size <= MIN_COMPRESS_LENGTH) -> send "as is"
      //   (compr.a.2) can compress (Size > MIN_COMPRESS_LENGTH)    -> convert to TCompressedBlob(Compressed = False), then (compr.b.2)
      // (compr.b) Blob is TCompressedBlob, but not compressed
      //   (compr.b.1) can't compress (Size <= MIN_COMPRESS_LENGTH) -> send "as is"
      //   (compr.b.2) can compress (Size > MIN_COMPRESS_LENGTH)    -> compress, then send "as is"
      // (compr.c) Blob is TCompressedBlob and compressed           -> send "as is"
      SwapBlob := nil;
      try
        if not (Blob is TCompressedBlob) {compr.a} then
          if Blob.Size <= MIN_COMPRESS_LENGTH {compr.a.1} then begin
            SendBlobAsIs(Blob);
            Exit;
          end
          else begin {compr.a.2}
            SwapBlob := Blob;
            Blob := TCompressedBlob.Create(Blob.IsUnicode);
            Blob.Assign(SwapBlob);
          end;
        // {compr.b} or {compr.c}
        TCompressedBlob(Blob).Compressed := True; // Try to compress {compr.b.2 only}
        SendBlobAsIs(Blob);
      finally
        if SwapBlob <> nil then
          Blob.Free; // Free created object
      end;
    end
    else begin
      // (uncompr.a) Blob is not TCompressedBlob                    -> send "as is"
      // (uncompr.b) Blob is TCompressedBlob, but not compressed    -> send "as is"
      // (uncompr.c) Blob is TCompressedBlob and compressed         -> uncompress then send "as is"
      if not (Blob is TCompressedBlob) {uncompr.a} or
         not TCompressedBlob(Blob).Compressed {uncompr.b}
      then
        SendBlobAsIs(Blob)
      else begin {uncompr.c} // Does not use SendBlobAsIs for performance reason
      {$IFNDEF VER9P}
        SetLength(b, 0); // anti-warning
      {$ENDIF}
        if not UseUnicode then begin
          if Blob.IsUnicode then
            b := Encoding.Default.GetBytes(Blob.AsWideString)
          else
            b := Blob.AsBytes;
          Result := EscapeAndQuoteStr(PAChar(b), Blob.Size);
        end
        else
          if Blob.IsUnicode then
            Result := CRFunctions.UTF8Encode(EscapeAndQuoteWideStr(Blob.AsWideString, Blob.Size shr 1))
          else begin
            b := Blob.AsBytes;
            Result := CRFunctions.AnsiToUTF8(EscapeAndQuoteStr(PAChar(b), Blob.Size));
          end;
      end;
    end;
  {$ELSE}
    SendBlobAsIs(Blob);
  {$ENDIF}
  end
  else
  if pData.VType = varArray + varByte then
    Result := EscapeAndQuoteStr(pData.VArray.Data, pData.VArray.Bounds[0].ElementCount)
  else
  if TVarData(Data).VPointer = nil then
    Result := ''''''
  else
  if UseUnicode then begin
    if (pData.VType = varOleStr) {$IFDEF VER12P}or (pData.VType = varUString){$ENDIF} then begin
      ws := EscapeAndQuoteWideStr(TVarData(Data).VPointer, StrLenW(TVarData(Data).VPointer));
      Result := CRFunctions.UTF8Encode(ws);
    end
    else begin
      s := EscapeAndQuoteStr(TVarData(Data).VPointer,
        {$IFDEF VER18P}{$IFNDEF NEXTGEN}AnsiStrings.{$ENDIF}{$ENDIF}StrLen(PAChar(TVarData(Data).VPointer)));
      Result := CRFunctions.AnsiToUTF8(s);
    end;
  end
  else
    if (pData.VType = varOleStr) {$IFDEF VER12P}or (pData.VType = varUString){$ENDIF} then begin
      ws := EscapeAndQuoteWideStr(TVarData(Data).VPointer, StrLenW(TVarData(Data).VPointer));
      Result := AnsiString(ws);
    end
    else begin
      s := AnsiString(Data);
      Result := EscapeAndQuoteStr(s, LengthA(s));
    end;
end;

function TMySQLConnection.EscapeAndQuoteStr(const p: PAnsiChar; Len: integer): AnsiString;

  function mysql_real_escape_string(_to, from: PAnsiChar; length: cardinal): cardinal;

    procedure EscapeSpesialCharacter;
    begin
      case from^ of
        #0: begin
          _to^ := '\';
          Inc(_to);
          _to^ := '0';
          Inc(_to);
        end;
        #$A: begin
          _to^ := '\';
          Inc(_to);
          _to^ := 'n';
          Inc(_to);
        end;
        #$D: begin
          _to^ := '\';
          Inc(_to);
          _to^ := 'r';
          Inc(_to);
        end;
        '\': begin
          _to^ := '\';
          Inc(_to);
          _to^ := '\';
          Inc(_to);
        end;
        '''': begin
          _to^ := '\';
          Inc(_to);
          _to^ := '''';
          Inc(_to);
        end;
        '"': begin
          _to^ := '\';
          Inc(_to);
          _to^ := '"';
          Inc(_to);
        end;
        #$1A: begin
          _to^ := '\';
          Inc(_to);
          _to^ := 'Z';
          Inc(_to);
        end;
        else begin
          _to^ := from^;
          Inc(_to);
        end;
      end
    end;

  var
    to_start, _end: PAnsiChar;
    IsGbkOrEuckr, IsEuckr: boolean;
  begin
    IsEuckr := FUsedCharset = 'euckr';
    IsGbkOrEuckr := (FUsedCharset = 'gbk') or IsEuckr;

    to_start := _to;
    _end := from + length;
    while from < _end do begin
      if not FNeedBackslashes then
        case from^ of
          '''': begin
            _to^ := '''';
            Inc(_to);
            _to^ := '''';
            Inc(_to);
          end;
        else
          begin
            _to^ := from^;
            Inc(_to);
          end;
        end
      else begin
        if SysLocale.FarEast and IsGbkOrEuckr and ({$IFDEF NEXTGEN}byte{$ENDIF}(from^) in LeadBytes) then begin
          _to^ := '\';
          Inc(_to);
          _to^ := from^;
          Inc(_to);

          if IsEuckr then begin
            Inc(from);
            if from <> _end then
              EscapeSpesialCharacter;
          end;
        end
        else
          EscapeSpesialCharacter;

        Inc(from);
      end;
    end;

    _to^ := #0;
    Result := _to - to_start;
  end;
begin
  SetLengthA(Result, cardinal(Len * 2 + 2));
  Result[1] := '''';
  SetLengthA(Result, mysql_real_escape_string(PAnsiChar(Result) + 1, p, Len) + 2);
  Result[LengthA(Result)] := '''';
end;

function TMySQLConnection.EscapeAndQuoteStr(const p: AnsiString; Len: integer): AnsiString;
begin
  Result := EscapeAndQuoteStr(PAnsiChar(p), Len);
end;

function TMySQLConnection.EscapeAndQuoteWideStr(const p: PWideChar; Len: integer{in characters}): WideString;

  function mysql_real_escape_string(_to: PWideChar; from: PWideChar; length: cardinal): cardinal;
  var
    to_start, _end: PWideChar;
  begin
    to_start := _to;

    _end := from + length;
    while from <> _end do begin
      if not FNeedBackslashes then
        case from^ of
          '''': begin
            _to^ := '''';
            Inc(_to);
            _to^ := '''';
            Inc(_to);
          end;
          else begin
            _to^ := from^;
            Inc(_to);
          end;
        end
      else
        case from^ of
          #0: begin
            _to^ := '\';
            Inc(_to);
            _to^ := '0';
            Inc(_to);
          end;
          #$A: begin
            _to^ := '\';
            Inc(_to);
            _to^ := 'n';
            Inc(_to);
          end;
          #$D: begin
            _to^ := '\';
            Inc(_to);
            _to^ := 'r';
            Inc(_to);
          end;
          '\': begin
            _to^ := '\';
            Inc(_to);
            _to^ := '\';
            Inc(_to);
          end;
          '''': begin
            _to^ := '\';
            Inc(_to);
            _to^ := '''';
            Inc(_to);
          end;
          '"': begin
            _to^ := '\';
            Inc(_to);
            _to^ := '"';
            Inc(_to);
          end;
          #$1A: begin
            _to^ := '\';
            Inc(_to);
            _to^ := 'Z';
            Inc(_to);
          end;
          else begin
            _to^ := from^;
            Inc(_to);
          end;
        end;

      Inc(from);
    end;

    _to^ := #0;
    Result := _to - to_start;
  end;
begin
  SetLength(Result, cardinal(Len * 2 + 2));
  Result[1] := '''';
  SetLength(Result, mysql_real_escape_string(PWideChar(Result) + 1, p, Len) + 2);
  Result[Length(Result)] := '''';
end;

function TMySQLConnection.EscapeAndQuoteWideStr(const p: WideString; Len: integer): WideString;
begin
  Result := EscapeAndQuoteWideStr(PWideChar(p), Len);
end;

procedure TMySQLConnection.Assign(Source: TCRConnection);
var
  Src: TMySQLConnection;
begin
  inherited;

  Src := TMySQLConnection(Source);
  FDatabase := Src.FDatabase;
  FPort := Src.FPort;
  FCompress := Src.FCompress;
  FUseUnicode := Src.FUseUnicode;
  FCharset := Src.FCharset;
  FUsedCharset := Src.FUsedCharset;
  FIsUtf8 := Src.FIsUtf8;
  FConnectionTimeout := Src.FConnectionTimeout;
  FProtocol := Src.FProtocol;
  FOptimizedBigInt := Src.FOptimizedBigInt;
  FNullForZeroDelphiDate := Src.FNullForZeroDelphiDate;
  FEmbedded := Src.FEmbedded;
  FDirect := Src.FDirect;
  FIPVersion := Src.FIPVersion;
  FCheckPrecision := Src.FCheckPrecision;
  FCheckBackslashes := Src.FCheckBackslashes;
  FInteractive := Src.FInteractive;
  FEmbParams.Text := Src.FEmbParams.Text;
  FMySQLAPI := Src.FMySQLAPI;
end;

procedure TMySQLConnection.AssignConnect(Source: TCRConnection);
var
  Src: TMySQLConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TMySQLConnection(Source);
      Assign(Src);
      FMySQL := Src.FMySQL;
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := FMySQL <> nil;
      FNativeConnection := False;
    end;
  end;
end;

procedure TMySQLConnection.SetMySQLAPI;
begin
  if FEmbedded then
    FMySQLAPI := MyAPIEmbedded
  else
    if FDirect then
      FMySQLAPI := MyAPIDirect
    else
      FMySQLAPI := MyAPIClient;
end;

procedure TMySQLConnection.SetEmbParamsStr(const EmbParams: string);
begin
  FEmbParams.Text := Trim(EmbParams);

  CheckDirParam(FEmbParams, '--basedir');
  CheckDirParam(FEmbParams, '--datadir');
  CheckDirParam(FEmbParams, '--character-sets-dir');
  CheckDirParam(FEmbParams, '--tmpdir');
  CheckDirParam(FEmbParams, '--log-bin');
  CheckDirParam(FEmbParams, '--log-bin-index');
end;

function TMySQLConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCharset: begin
      if FCharset <> LowerCase(Value) then begin
        FCharset := LowerCase(Value);
        if FConnected and not FUseUnicode then
          TryToApplyCharset;
      end;
    end;
    prCheckBackslashes: begin
      if FCheckBackslashes <> Value then begin
        FCheckBackslashes := Value;
        if FConnected then
          CheckNoBackslashEscapesMode;
      end;
    end;
    prCheckPrecision:
      FCheckPrecision := Value;
    prCompress:
      FCompress := Value;
    prConnectionTimeout: begin
      FConnectionTimeout := Value;
      if FConnected then
        MySQLAPI.SetTimeout(MySQL, FConnectionTimeout);
    end;
    prDatabase:
      SetDatabase(Value);
    prDirect: begin
      FDirect := Value;
      SetMySQLAPI;
    end;
    prEmbedded: begin
      FEmbedded := Value;
      SetMySQLAPI;
    end;
    prEmbParams:
      SetEmbParamsStr(Value);
    prInteractive:
      FInteractive := Value;
    prIPVersion:
      FIPVersion := Value;
    prNullForZeroDelphiDate:
      FNullForZeroDelphiDate := Value;
    prOptimizedBigInt:
      FOptimizedBigInt := Value;
    prPort:
      FPort := Value;
    prProtocol:
      FProtocol := TMyProtocol(Integer(Value));
    prUseUnicode: begin
      if FUseUnicode <> Value then begin
        FUseUnicode := Value;
        if FConnected then
          TryToApplyCharset;
      end;
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMySQLConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prCharset:
      Value := FCharset;
    prCheckBackslashes:
      Value := FCheckBackslashes;
    prCompress:
      Value := FCompress;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prDatabase:
      Value := FDatabase;
    prDirect:
      Value := FDirect;
    prEmbedded:
      Value := FEmbedded;
    prInteractive:
      Value := FInteractive;
    prIPVersion:
      Value := FIPVersion;
    prLastInsertId:
      Value := FMySQLAPI.mysql_insert_id(FMySQL);
    prNeedBackslashes:
      Value := FNeedBackslashes;
    prPort:
      Value := FPort;
    prProtocol:
      Value := integer(FProtocol);
    prUseUnicode:
      Value := FUseUnicode;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TMySQLConnection.CheckMySQLLib;
begin
  if IsClass(FMySQLAPI, TMySQLAPIEmbedded) then
    TMySQLAPIEmbedded(FMySQLAPI).Params := FEmbParams;
  FMySQLAPI.CheckMySQLLib;
end;

function TMySQLConnection.GetClientVersion: string;
begin
  if FMySQLAPI.LoadedMySQLLib then
    Result := FMySQLAPI.ClientVer
  else begin
    CheckMySQLLib;
    Result := FMySQLAPI.ClientVer;
    FMySQLAPI.FreeMySQLLib;
  end;
end;

procedure TMySQLConnection.InitCommandProp(Command: TCRCommand);
begin
  TMySQLCommand(Command).FCommandTimeout := FConnectionTimeout;
end;

function TMySQLConnection.GetIsClient41: boolean;
begin
  Assert(FMySQLAPI <> nil);
  Result := (FMySQLAPI.ClientStructVer = cv410)
    or (FMySQLAPI.ClientStructVer = cv411)
    or (FMySQLAPI.ClientStructVer = cvDirect)
  ;
end;

function TMySQLConnection.GetIsServer41: boolean;
begin
  Assert(FMySQLAPI <> nil);
  Result := (FServerPrimaryVer >= 5) or
    ((FServerPrimaryVer = 4) and (FServerMinorVer >= 1));
end;

function TMySQLConnection.GetServerVersion: string;
begin
  Result := IntToStr(FServerPrimaryVer) + '.' + IntToStr(FServerMinorVer) +
    '.' + IntToStr(FServerReleaseVer);
end;

function TMySQLConnection.GetServerVersionFull: string;
begin
  Result := FServerVer;
end;

function TMySQLConnection.GetThreadId: cardinal;
begin
  Assert(FMySQLAPI <> nil);
  Result := MySQLAPI.mysql_thread_id(FMySQL);
end;

function TMySQLConnection.GetLastWarningCount: cardinal;
begin
  Assert(FMySQLAPI <> nil);
  Result := MySQLAPI.mysql_warning_count(FMySQL);
end;

function TMySQLConnection.CheckIsValid: boolean;
begin
  FIsValid := (FMySQLAPI <> nil) and (FMySQL <> nil) and (FMySQLAPI.mysql_ping(FMySQL) = 0)
    and (FMySQLAPI.mysql_ping(FMySQL) = 0); // on some server versions two pings is needed to detect that connection is killed
  Result := FIsValid;
end;

{$IFNDEF NODBACCESS}
procedure TMySQLConnection.ReturnToPool;
begin
  inherited;

  FOnWarning := nil;
end;
{$ENDIF}


function TMySQLConnection.IsFractionSupported: boolean;
begin
  Assert(FMySQLAPI <> nil);
  Result := (ServerPrimaryVer >= 6) or
    (IsMariaDB and (ServerPrimaryVer = 5) and (ServerMinorVer >= 3) or
    (ServerPrimaryVer = 5) and (ServerMinorVer >= 6));
end;

{ TMySQLCommand }

procedure TMySQLCommand.Check(const Status: TIntPtrObj);
begin
  Assert(FConnection <> nil);
  if Status = nil then
    FConnection.MySQLError(Component);
end;

procedure TMySQLCommand.Check(const Status: integer);
begin
  Assert(FConnection <> nil);
  if Status <> 0 then
    FConnection.MySQLError(Component);
end;

procedure TMySQLCommand.Check;
begin
  Assert(FConnection <> nil);
  FConnection.Check(Component);
end;

procedure TMySQLCommand.CheckStmt(const Status: integer);
begin
  Assert(FConnection <> nil);
  if Status <> 0 then
    FConnection.MySQLStmtError(Fstmt, Component);
end;

constructor TMySQLCommand.Create;
begin
  inherited;
  FCommandTimeout := DefaultCommandTimeout;
  FRequestResultset := False;
  FRowsAffected := -1;
  FCursorState := csInactive;
  FCachedSQL := TStringList.Create;
  FBreakExecCS := TCriticalSection.Create;
  FEnableBoolean := True;
  FCreateConnection := False;
  FUseCreateConnection := False;
end;

destructor TMySQLCommand.Destroy;
begin
  if (GetBatchCommand <> nil) and TMySQLCommand(GetBatchCommand).FPrepared then
    TMySQLCommand(GetBatchCommand).Unprepare;

  FCachedSQL.Free;
  FBreakExecCS.Free;
  inherited;
end;


procedure TMySQLCommand.QuerySwapConnection;
begin
  if FCursorState = csExecuted then
    Exit; // This is a duplicate call from Execute (1. - InternalExecute; 2. - Open)

  if FConnectionSwap <> nil then
    Exit;

  Assert(FConnection <> nil);
  FConnection.FMySQLAPI.SetTimeout(FConnection.FMySQL, FCommandTimeout);
  if FSQLResult = nil then
    Check(FConnection.FMySQLAPI.mysql_ping(FConnection.FMySQL));

  FConnectionSwap := FConnection;
  FConnection := nil;

  if Assigned(FConnectionSwap.GetPooledConnection) then
    FConnection := TMySQLConnection(FConnectionSwap.GetPooledConnection)
  else begin
    FConnection := TMySQLConnection.Create;
  {$IFDEF AUTOTEST}
    Inc(__SwapConnectionCount);
  {$ENDIF}
    FConnection.Assign(FConnectionSwap);
  end;

  FConnection.Connect('');
  if FConnectionSwap.GetInternalTransaction.GetInTransaction then
    FConnection.GetInternalTransaction.StartTransaction;
  FConnection.Additional := True;
end;

procedure TMySQLCommand.ReleaseSwapConnection(ForbidQueries: boolean = False);
begin
  if FConnectionSwap = nil then
    Exit;

  if FConnection <> nil then begin
    if FConnection.GetInternalTransaction.GetInTransaction and not ForbidQueries then // CR-DbxMda12849
      FConnection.GetInternalTransaction.Commit;
    FConnection.Additional := True;
  {$IFNDEF NODBACCESS}
    if Assigned(FConnectionSwap.GetPooledConnection) then begin
      FConnection.IsValid := not ForbidQueries; // CR15836

      if FConnection.Pool <> nil then
        FConnection.ReturnToPool
      else
        FConnection.Free;
    end
    else
  {$ENDIF}
    begin
      FConnection.Disconnect; // Quick destroy FSQLResult without fetching all. WAR: if fetched all records then not optimal
      FreeAndNil(FConnection);
    {$IFDEF AUTOTEST}
      Dec(__SwapConnectionCount);
    {$ENDIF}
    end;
  end;

  FConnection := FConnectionSwap;
  FConnectionSwap := nil;
end;

procedure TMySQLCommand.Prepare;
var
  u: AnsiString;
  Connection, UsedConnection: TMySQLConnection;
begin
  if GetPrepared then
    Exit;

  if not FConnection.IsClient41 then
    DatabaseError(SPrepareNotSupportedC);

  if not FConnection.IsServer41 then
    DatabaseError(SPrepareNotSupportedS);

  Connection := FConnection;
  Connection.Lock;
  try
    if FCreateConnection and FUseCreateConnection then
      QuerySwapConnection;
    UsedConnection := FConnection;
    UsedConnection.Lock;
    try
      Connection.Release;
      Connection := nil;

      Fstmt := FConnection.FMySQLAPI.mysql_stmt_init(FConnection.FMySQL);
      Check(Fstmt);
      try
        FConnection.FMySQLAPI.SetTimeout(FConnection.FMySQL, FCommandTimeout);
        if FConnection.FIsUtf8 then
          u := CRFunctions.UTF8Encode(WideString(FSQL))
        else
          u := AnsiString(FSQL);
        CheckStmt(FConnection.FMySQLAPI.mysql_stmt_prepare(Fstmt, PAnsiChar(u), LengthA(u)));
      {$IFDEF AUTOTEST}
        Inc(__ServerPrepareCount);
      {$ENDIF}
      except
        if Fstmt <> nil then begin
          FConnection.FMySQLAPI.mysql_stmt_close(Fstmt);
          Fstmt := nil;
        end;
        raise;
      end;
    finally
      UsedConnection.Release;
    end;
    FPrepared := True;
  finally
    if Connection <> nil then
      Connection.Release;
  end;

  inherited;
end;

procedure TMySQLCommand.Unprepare;
begin
  if GetPrepared and (Fstmt <> nil) then begin
    FConnection.Lock;
    try
      FConnection.FMySQLAPI.mysql_stmt_close(Fstmt);
    finally
      FConnection.Release;
      Fstmt := nil;
      inherited;
      FPrepared := False;
    end;
  end;
end;

function TMySQLCommand.GetPrepared: boolean;
begin
  Result := FPrepared;
end;

function TMySQLCommand.CalcRowsAffected: integer;
begin
  if not GetPrepared then begin
    Result := FConnection.FMySQLAPI.mysql_affected_rows(FConnection.FMySQL);
    if FConnection.FMySQLAPI.mysql_field_count(FConnection.FMySQL) = 0 then
      SetCursorState(csInactive)
    else
      SetCursorState(csExecuted);
  end
  else begin
    Result := FConnection.FMySQLAPI.mysql_stmt_affected_rows(Fstmt);
    if FConnection.FMySQLAPI.mysql_stmt_field_count(Fstmt) = 0 then
      SetCursorState(csInactive)
    else
      SetCursorState(csExecuted);
  end
end;

procedure TMySQLCommand.GetSPParams(const Name: string; out IsFunc: boolean;
  out ParamList: string; out ReturnParam: string);

  type
    TMyParamBlockResult = record
      Params: string;
      Returns: string;
    end;

  procedure _GetSPParams(out IsFunc: boolean; out ParamList: string; out ReturnParam: string);
  var
    RecordSet: TCRRecordSet;

  var
    RecBuf: IntPtr;
    Blob: TBlob;
    v: variant;
    Database, ProcName: string;
  begin
    IsFunc := False;
    ParamList := '';
    ReturnParam := '';

    if FConnection.FServerPrimaryVer < 5 then
      Exit;

    MySQLInfo.SplitObjectName(Name, Database, ProcName);
    if Database <> '' then
      Database := SQLInfo.NormalizeName(Database, False, True)
    else
      Database := FConnection.FDatabase;
    ProcName := SQLInfo.NormalizeName(ProcName, False, True);

    // Can't use LOWER (5.0.9 bug) RecordSet.FCommand.SQL := 'SELECT type, returns, param_list FROM mysql.proc WHERE (name = ''' + Name + ''') AND (LOWER(db) = LOWER(''' + FDatabase + '''))';
    RecordSet := FConnection.OpenRecordSet('SELECT type, returns, param_list FROM mysql.proc WHERE (name = ''' +
      ProcName + ''') AND (db = ''' + Database + ''')');
    try
      if RecordSet = nil then
        RecordSet := FConnection.OpenRecordSet('SELECT type, returns, param_list FROM mysql.proc WHERE (name = ''' +
          ProcName + ''') AND (LOWER(db) = ''' + AnsiLowerCase(Database) + ''')');

      if RecordSet = nil then
        Exit;

      RecordSet.AllocRecBuf(RecBuf);
      try
        RecordSet.GetNextRecord(RecBuf);
        if not RecordSet.Eof then begin
          RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
          IsFunc := UpperCase(v)= 'FUNCTION';

          if not RecordSet.Fields[1].IsBlob then begin
            RecordSet.GetFieldAsVariant(RecordSet.Fields[1], RecBuf, v);
            ReturnParam := v;
          end
          else begin
            if not RecordSet.GetNull(RecordSet.Fields[1], RecBuf) then begin
              Blob := RecordSet.GetBlob(RecordSet.Fields[1], RecBuf);
              if Blob.Size > 0 then
                ReturnParam := Blob.{$IFDEF IS_UNICODE}AsWideString{$ELSE}AsString{$ENDIF};
            end;
          end;

          if not RecordSet.GetNull(RecordSet.Fields[2], RecBuf) then begin
            Blob := RecordSet.GetBlob(RecordSet.Fields[2], RecBuf) as TBlob;
            if Blob.Size > 0 then
              ParamList := Blob.{$IFDEF IS_UNICODE}AsWideString{$ELSE}AsString{$ENDIF};
          end;
        end;
      finally
        RecordSet.FreeRecBuf(RecBuf);
      end;

    finally
      FConnection.ReleaseRecordSet(RecordSet);
    end;
  end;

  function GetCreateSQL: string;
  var
    RecordSet: TCRRecordSet;
    RecBuf: IntPtr;
    v: variant;
    Database, ProcName: string;
    Blob: TBlob;
  begin
    MySQLInfo.SplitObjectName(Name, Database, ProcName);
    if Database <> '' then
      Database := SQLInfo.NormalizeName(Database, False, True)
    else
      Database := FConnection.FDatabase;
    ProcName := SQLInfo.NormalizeName(ProcName, False, True);

    IsFunc := False;
    Result := 'SHOW CREATE PROCEDURE ' + Database + '.' + SQLInfo.QuoteIfNeed(ProcName);
    try
      RecordSet := FConnection.OpenRecordSet(Result);
    except
      on E: EMyError do
        if E.ErrorCode = ER_SP_DOES_NOT_EXIST then begin
          Result := 'SHOW CREATE FUNCTION ' + Database + '.' + SQLInfo.QuoteIfNeed(ProcName);
          RecordSet := FConnection.OpenRecordSet(Result);
          IsFunc := True;
        end
        else
          raise;
      else
        raise;
    end;

    try
      if RecordSet = nil then
        Exit;

      RecordSet.AllocRecBuf(RecBuf);
      try
        RecordSet.GetNextRecord(RecBuf);
        if not RecordSet.Eof then begin
          if not RecordSet.GetNull(RecordSet.Fields[2], RecBuf) then begin
            if RecordSet.Fields[2].IsBlob then begin
              Blob := RecordSet.GetBlob(RecordSet.Fields[2], RecBuf);
              if Blob <> nil then
                Result := Blob.AsString
              else
                Result := '';
            end
            else begin
              RecordSet.GetFieldAsVariant(RecordSet.Fields[2], RecBuf, v);
              Result := v;
            end;
          end;
        end;
      finally
        RecordSet.FreeRecBuf(RecBuf);
      end;
    finally
      FConnection.ReleaseRecordSet(RecordSet);
    end;
  end;

  function GetParamBlock(aProcSrc: string): TMyParamBlockResult;
  var
    aReturnsBeginPos: integer;

    function GetParamBlockParams: string;
    var
      i, aBeginPos, aEndPos, aColonCount: integer;
    begin
      Result := '';
      aBeginPos := Pos('(', aProcSrc);
      if aBeginPos > aReturnsBeginPos then
        Exit;
      if aBeginPos = 0 then
        Exit;
      aColonCount := 1;
      aEndPos := 0;
      for i := aBeginPos + 1 to Length(aProcSrc) - 1 do begin
        if aProcSrc[i] = '(' then
          Inc(aColonCount)
        else
          if aProcSrc[i] = ')' then
            Dec(aColonCount);
        if aColonCount = 0 then begin
          aEndPos := i;
          Break;
        end;
      end;
      if aColonCount <> 0 then Exit;
      Result := Copy(aProcSrc, aBeginPos + 1, (aEndPos - aBeginPos - 1));
    end;

    function GetParamBlockReturns: string;
    var
      Parser: TMyParser;
      aLexem: string;
      aCurrToken: Integer;
      aTypeSize: string;

      function ParserNext(out Lexem: string): Integer;
      begin
        Result := Parser.GetNext(Lexem);
        aCurrToken := Result;
      end;
    begin
      Result := '';
      Parser := TMyParser.Create(aProcSrc);
      try
        Parser.OmitBlank := True;
        Parser.OmitComment := True;
        Parser.Uppered := True;
        if not Parser.ToLexem('RETURNS') then
          exit;

        aReturnsBeginPos := Parser.CurrPos;

        if Parser.GetNext(aLexem) <> lcIdent then
          Exit;

        Result := aLexem;

        if (ParserNext(aLexem) = lcEnd) or (aCurrToken <> lxLeftBracket) then
          Exit;

        Result := Result + aLexem;

        Parser.OmitBlank := False;

        aTypeSize := '';
        while (ParserNext(aLexem) <> lcEnd) and (aCurrToken <> lxRightBracket) do begin
          if aCurrToken = lxLeftBracket then
            Exit;

          aTypeSize := aTypeSize + aLexem;
        end;

        if aCurrToken = lxRightBracket then
          Result := Result + aTypeSize + ')';

      finally
        Parser.Free;
      end;
    end;

  begin
    aReturnsBeginPos := MaxInt;
    Result.Returns := GetParamBlockReturns;
    Result.Params := GetParamBlockParams;
  end;

begin
  if ((FConnection.FServerPrimaryVer = 5) and (FConnection.FServerMinorVer = 0) and
    (FConnection.FServerReleaseVer < 4)) or FConnection.FEmbedded
  then
    _GetSPParams(IsFunc, ParamList, ReturnParam)
  else
    with GetParamBlock(GetCreateSQL) do begin
      ParamList := Params;
      ReturnParam := Returns;
    end;
end;

procedure TMySQLCommand.DescribeParams(const ParamList: string; const IsResult: boolean;
  var ParamsInfo: TParamInfos; ParamDescs: TParamDescs; const EnableBoolean: boolean);
var
  Parser: TMyParser;
  StLex: string;
  CodeLexem: integer;
  s, Name: string;

  Param: TParamDesc;
  ParamType: TParamDirection;
  pii: integer;
  sz, code: Integer;
begin
  Parser := TMyParser.Create(ParamList);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
  {$IFNDEF MSWINDOWS}
    Parser.DecSeparator := ',';
  {$ENDIF}
    while True do begin
      CodeLexem := Parser.GetNext(StLex);
      if CodeLexem = lcEnd then
        Break;

      pii := Length(ParamsInfo);
      SetLength(ParamsInfo, pii + 1);

      if ParamDescs <> nil then begin
        Param := TParamDesc.Create;//(Params, ParamType);
        ParamDescs.Add(Param);
      end
      else
        Param := nil;

      // Parameter direction
      if not IsResult then begin
        s := UpperCase(StLex);
        ParamType := pdUnknown;
        if s = 'IN' then
          ParamType := pdInput
        else
        if s = 'OUT' then
          ParamType := pdOutput
        else
        if s = 'INOUT' then
          ParamType := pdInputOutput;

        if ParamType = pdUnknown then
          ParamType := pdInput // as default
        else
          {$IFOPT C+}CodeLexem := {$ENDIF}Parser.GetNext(StLex);

        if (CodeLexem = lcSymbol) and (StLex = '$') then begin
          {$IFOPT C+}CodeLexem := {$ENDIF}Parser.GetNext(StLex);
          StLex := '$' + StLex;
        end;

        Name := StLex;

        // Parameter name
        Assert((CodeLexem = lcIdent) or (CodeLexem = lxSET) or (CodeLexem = lxSTATUS), StLex + ' in ' + ParamList + ', Name = ' + Name);

        // Parameter type
        {$IFOPT C+}CodeLexem := {$ENDIF}Parser.GetNext(StLex);
      end
      else begin
        ParamType := pdResult;
        Name := SResultParamName;
      end;

      Name := SQLInfo.NormalizeName(Name, False, True);
      if not IsResult then
        ParamsInfo[pii].Name := Name;
      if Param <> nil then
        Param.SetName(Name);

      ParamsInfo[pii].Direction := ParamType;
      if Param <> nil then
        Param.SetParamType(ParamType);

      // Parameter type
      Assert((CodeLexem = lcIdent) or (CodeLexem = lxSET), StLex + ' in ' + ParamList + ', Name = ' + Name);  // SET may be both identifier and datatype
      s := UpperCase(StLex);
      ParamsInfo[pii].ParamType := s;
      ParamsInfo[pii].Unsigned := False;

      if Param <> nil then begin
        if s = 'BIT' then
          Param.SetDataType(dtBoolean)
        else
        if s = 'TINYINT' then
          Param.SetDataType(dtInt8)
        else
        if s = 'SMALLINT' then
          Param.SetDataType(dtInt16)
        else
        if s = 'MEDIUMINT' then
          Param.SetDataType(dtInt32)
        else
        if (s = 'INT') or (s = 'INTEGER') then
          Param.SetDataType(dtInt32)
        else
        if s = 'BIGINT' then
          Param.SetDataType(dtInt64)
        else
        if s = 'DOUBLE' then
          Param.SetDataType(dtFloat)
        else
        if s = 'FLOAT' then
          Param.SetDataType(dtFloat)
        else
        if (s = 'DECIMAL') or (s = 'NUMERIC') then // DECIMAL(10, 2)?
          Param.SetDataType(dtFloat)
        else
        if (s = 'CHAR') or (s = 'NCHAR') then // (SIZE)?, BINARY?
          Param.SetDataType(dtString)
        else
        if (s = 'VARCHAR') or (s = 'NVARCHAR') then // (SIZE)?, BINARY?
          Param.SetDataType(dtString)
        else
        if s = 'DATE' then
          Param.SetDataType(dtDate)
        else
        if s = 'TIME' then
          Param.SetDataType(dtTime)
        else
        if s = 'TIMESTAMP' then // (14)?
          Param.SetDataType(dtDateTime)
        else
        if s = 'DATETIME' then
          Param.SetDataType(dtDateTime)
        else
        if s = 'YEAR' then begin // (4)?
          Param.SetDataType(dtUInt16);
          ParamsInfo[pii].Unsigned := True;
        end
        else
        if (s = 'BOOL') or (s = 'BOOLEAN') then
          Param.SetDataType(dtInt8)
        else
        if s = 'BINARY' then
          Param.SetDataType(dtBytes)
        else
        if s = 'VARBINARY' then
          Param.SetDataType(dtVarBytes)
        else
        if s = 'TINYBLOB' then
          Param.SetDataType(dtBlob)
        else
        if s = 'BLOB' then
          Param.SetDataType(dtBlob)
        else
        if s = 'MEDIUMBLOB' then
          Param.SetDataType(dtBlob)
        else
        if s = 'LONGBLOB' then
          Param.SetDataType(dtBlob)
        else
        if s = 'TINYTEXT' then
          Param.SetDataType(dtMemo)
        else
        if s = 'TEXT' then
          Param.SetDataType(dtMemo)
        else
        if s = 'MEDIUMTEXT' then
          Param.SetDataType(dtMemo)
        else
        if s = 'LONGTEXT' then
          Param.SetDataType(dtMemo)
        else
        if s = 'ENUM' then // ('value1','value2','value3')?
          Param.SetDataType(dtString)
        else
        if s = 'SET' then // ('value1','value2','value3')
          Param.SetDataType(dtString)
        else
        if s = 'GEOMETRY' then
          Param.SetDataType(dtBlob)
        else
        if s = 'POINT' then
          Param.SetDataType(dtBlob)
        else
        if s = 'LINESTRING' then
          Param.SetDataType(dtBlob)
        else
        if s = 'POLYGON' then
          Param.SetDataType(dtBlob)
        else
        if s = 'GEOMETRYCOLLECTION' then
          Param.SetDataType(dtBlob)
        else
        if s = 'MULTIPOINT' then
          Param.SetDataType(dtBlob)
        else
        if s = 'MULTILINESTRING' then
          Param.SetDataType(dtBlob)
        else
        if s = 'MULTIPOLYGON' then
          Param.SetDataType(dtBlob)
        else
        if s = 'JSON' then
          Param.SetDataType(dtMemo)
        else
          raise Exception.Create('Unknown column type "' + s + '" in "' + ParamList + '"');
      end;

      ParamsInfo[pii].Size := -1;
      repeat
        CodeLexem := Parser.GetNext(StLex);
        if CodeLexem = lxLeftBracket then begin
          CodeLexem := Parser.GetNext(StLex);
          while (CodeLexem <> lcEnd) and (CodeLexem <> lxRightBracket) do begin
            if (ParamsInfo[pii].Size = -1) and (CodeLexem = lcNumber) then begin
              Val(StLex, sz, code);
              if code = 0 then begin
                ParamsInfo[pii].Size := sz;

                if Param <> nil then begin
                  if Param.GetDataType = dtString then
                    Param.SetSize(ParamsInfo[pii].Size);
                  if (Param.GetDataType = dtInt8) and FEnableBoolean and (CodeLexem = lcNumber) and (StLex = '1')  then
                    Param.SetDataType(dtBoolean);
                end;
              end;
            end;

            if (s = 'DECIMAL') or (s = 'NUMERIC') then begin
              ParamsInfo[pii].ParamType := '(';
              repeat
                ParamsInfo[pii].ParamType := ParamsInfo[pii].ParamType + StLex;
                CodeLexem := Parser.GetNext(StLex);
              until (CodeLexem = lcEnd) or (CodeLexem = lxRightBracket);
              ParamsInfo[pii].ParamType := ParamsInfo[pii].ParamType + ')';
            end
            else
              CodeLexem := Parser.GetNext(StLex);
          end;
        end;
        if UpperCase(StLex) = 'UNSIGNED' then
          ParamsInfo[pii].Unsigned := True;
      until (CodeLexem = lcEnd) or (CodeLexem = lxComma);

      if Param <> nil then
        if FConnection.FUseUnicode then
          case Param.GetDataType of
            dtString:
              Param.SetDataType(dtWideString);
            dtMemo:
              Param.SetDataType(dtWideMemo);
          end;
    end;
  finally
    Parser.Free;
  end;
end;

function TMySQLCommand.GetBatchSQL(Iters, Offset: integer): string;
var
  OriginalSQL: string;
  BatchSQL: StringBuilder;
  i, OpenBracket, CloseBracket, SQLLength: integer;
  LocalParamIndex: integer;
{$IFDEF HAVE_COMPRESS}
  SendBlobCompressed: boolean;
{$ENDIF}
  HasParams: boolean;

  procedure AppendParamValue(ParamIndex, ItemIndex: integer);
  var
    ParamDesc: TParamDesc;
    ParamValuePtr: PVariant;
    Blob: TBlob;
    IsNull: boolean;
  begin
    if (FParsedSQLType = qtInsert) and FConnection.IsServer41 then begin
      Inc(LocalParamIndex);
      BatchSQL.Append(':p' + IntToStr(LocalParamIndex));
    end
    else begin
      ParamDesc := FParamsInfo[ParamIndex].ParamRef;
      ParamValuePtr := ParamDesc.GetItemPtr(ItemIndex);
      if (ParamDesc.GetDataType in [dtBlob, dtMemo, dtWideMemo]) and (PVarData(ParamValuePtr).VType = varSharedObject) then begin
        Blob := PVarData(ParamValuePtr).VPointer;
        IsNull := Blob.IsNull;
      end
      else
        IsNull := VarIsEmpty(ParamValuePtr^) or VarIsNull(ParamValuePtr^);

      FConnection.AppendValueToSQL(BatchSQL, ParamDesc.GetDataType, ParamValuePtr^, IsNull,
        FConnection.FUseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF});
    end;
  end;

  procedure AppendParams(Iteration, StartIndex: integer);
  var
    i, n: integer;
  begin
    n := StartIndex;
    for i := 0 to FParamsInfo.Count - 1 do begin
      if i = 0 then
        BatchSQL.Append(OriginalSQL, OpenBracket - 1, StartIndex - OpenBracket + 1)
      else
        BatchSQL.Append(OriginalSQL, n, FParamsInfo[i].StartPosition - n - 1);

      AppendParamValue(i, Offset + Iteration);

      n := FParamsInfo[i].EndPosition - 1;

      if i = (FParamsInfo.Count - 1) then
        BatchSQL.Append(OriginalSQL, n, CloseBracket - n);
    end;
  end;

begin
  OriginalSQL := Trim(FSQL);
  Result := OriginalSQL;
  LocalParamIndex := 0;

{$IFDEF HAVE_COMPRESS}
  case FCompressBlob of
    cbServer, cbClientServer:
      SendBlobCompressed := True;
    else
      SendBlobCompressed := False;
  end;
{$ENDIF}

  SQLLength := Length(OriginalSQL);
  BatchSQL := StringBuilder.Create(SQLLength * Iters);
  HasParams := FParamsInfo.Count > 0;
  try
    case FParsedSQLType of
      qtInsert:
        if HasParams then begin
          OpenBracket := FParamsInfo[0].StartPosition;
          while OpenBracket >= 1 do begin
            if OriginalSQL[OpenBracket] = '(' then
              Break;
            Dec(OpenBracket);
          end;

          CloseBracket := FParamsInfo[FParamsInfo.Count - 1].EndPosition;
          while CloseBracket <= SQLLength do begin
            if OriginalSQL[CloseBracket] = ')' then
              Break;
            Inc(CloseBracket);
          end;

          BatchSQL.Append(OriginalSQL, 0, OpenBracket - 1);

          for i := 0 to Iters - 1 do begin
            if i > 0 then
              BatchSQL.Append(',');

            AppendParams(i, FParamsInfo[0].StartPosition - 1);
          end;

          if CloseBracket < SQLLength then
            BatchSQL.Append(OriginalSQL, CloseBracket, SQLLength - CloseBracket);
        end
        else
          for i := 0 to Iters - 1 do begin
            BatchSQL.Append(OriginalSQL);
            if OriginalSQL[SQLLength] <> ';' then
              BatchSQL.Append(';');
          end;
      qtUpdate,
      qtDelete: begin
        OpenBracket := 1;
        CloseBracket := SQLLength;
        for i := 0 to Iters - 1 do begin
          if HasParams then
            AppendParams(i, FParamsInfo[0].StartPosition - 1)
          else
            BatchSQL.Append(OriginalSQL);

          if OriginalSQL[SQLLength] <> ';' then
            BatchSQL.Append(';');
        end;
      end;
    end;

    Result := BatchSQL.ToString;
  finally
    BatchSQL.Free;
  end;
end;

procedure TMySQLCommand.InternalExecuteBatch(Iters, Offset: integer);
var
  BatchSize,
  LastBatchSize: integer;
  i, n,
  RowsAffected,
  ParamsProcessed: integer;
  BatchCommand: TMySQLCommand;
  OldDisableParamScan: boolean;
begin
  BatchCommand := TMySQLCommand(GetBatchCommand);

  if (FParsedSQLType = qtInsert) or FConnection.IsServer41 then begin
    if (FParsedSQLType = qtInsert) and FConnection.IsServer41 then begin
      Assert(BatchCommand <> nil);

      BatchSize := MaxPlaceHoldersCount div FParamsInfo.Count;

      n := 0;
      LastBatchSize := 0;
      RowsAffected := 0;
      ParamsProcessed := 0;
      BatchCommand.FRowsAffected := 0;
      BatchCommand.FParamsProcessed := 0;

      while n < Iters do begin
        if n + BatchSize > Iters then
          BatchSize := Iters - n;

        if LastBatchSize <> BatchSize then begin
          OldDisableParamScan := BatchCommand.FScanParams;
          BatchCommand.FDisableParamScan := False;
          try
            BatchCommand.SetSQL(GetBatchSQL(BatchSize, Offset));
          finally
            BatchCommand.FDisableParamScan := OldDisableParamScan;
          end;
          if BatchCommand.FPrepared then
            BatchCommand.Unprepare;
        end;

        BatchCommand.FBatchIters := BatchSize;
        BatchCommand.FBatchOffset := Offset + n;
        BatchCommand.FParamsProcessed := 0;
        BatchCommand.FLastBatchIters := BatchSize;

        if not BatchCommand.FPrepared then
          BatchCommand.Prepare;

        BatchCommand.Execute;

        Inc(RowsAffected, BatchCommand.FRowsAffected);
        Inc(ParamsProcessed, BatchSize);
        Inc(n, BatchSize);
        LastBatchSize := BatchSize;
      end;
    end
    else begin
      inherited InternalExecuteBatch(Iters, Offset);
      RowsAffected := BatchCommand.FRowsAffected;
      ParamsProcessed := Iters;
    end;

    FRowsAffected := RowsAffected;
  end
  else begin
    FRowsAffected := 0;
    ParamsProcessed := Iters;
    for i := 0 to Iters - 1 do begin
      inherited InternalExecuteBatch(1, Offset + i);
      FRowsAffected := FRowsAffected + BatchCommand.FRowsAffected;
    end;
  end;

  FParamsProcessed := ParamsProcessed;
end;

function TMySQLCommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;
var
  IsFunc: boolean;
  ParamList, ReturnParam: string;
  ParamsInfo: TParamInfos;
  pd: TParamDescs;
begin
  if NeedDescribe or (Params.Count > 0) then
    GetSPParams(Name, IsFunc, ParamList, ReturnParam)
  else
    IsFunc := False;

  if NeedDescribe then begin
    Params.Clear;
    pd := Params;
  end
  else
    pd := nil;

  ParamsInfo := nil;
  if NeedDescribe or (Params.Count > 0) then begin
    if IsFunc then
      DescribeParams(ReturnParam, True, ParamsInfo, pd, FEnableBoolean);
    DescribeParams(ParamList, False, ParamsInfo, pd, FEnableBoolean);
    Assert(Params.Count <= Length(ParamsInfo));
  end;

  CreateProcCallByParams(Name, IsFunc, ParamsInfo, FSQL, FUserSQL);
  Result := FUserSQL;
end;

procedure TMySQLCommand.CreateProcCallByParams(const Name: string; IsFunc: boolean;
  const ParamsInfo: TParamInfos; out ASQL, AUserSQL: string);

  function GetSelectForParam(const ParamName: string; const QuotedParamName: string; const ParamInfo: TParamInfo): string;
  begin
    if (ParamInfo.ParamType = 'TINYINT') or (ParamInfo.ParamType = 'SMALLINT') or
      (ParamInfo.ParamType = 'MEDIUMINT') or (ParamInfo.ParamType = 'INT') or
      (ParamInfo.ParamType = 'INTEGER') or (ParamInfo.ParamType = 'BIGINT') then begin
      Result := 'CAST(' + QuotedParamName + ' AS';
      if ParamInfo.Unsigned then
        Result := Result + ' UNSIGNED)'
      else
        Result := Result + ' SIGNED)';
    end
    else
    if ParamInfo.ParamType = 'YEAR' then
      Result := 'CAST(' + QuotedParamName + ' AS UNSIGNED)'
    else
    if ParamInfo.ParamType = 'DATE' then
      Result := 'CAST(' + QuotedParamName + ' AS DATE)'
    else
    if ParamInfo.ParamType = 'TIME' then
      Result := 'CAST(' + QuotedParamName + ' AS TIME)'
    else
    if ParamInfo.ParamType = 'DATETIME' then
      Result := 'CAST(' + QuotedParamName + ' AS DATETIME)'
    else
    if (ParamInfo.ParamType = 'TINYBLOB') or (ParamInfo.ParamType = 'BLOB') or
      (ParamInfo.ParamType = 'MEDIUMBLOB') or (ParamInfo.ParamType = 'LONGBLOB') then
      Result := 'CAST(' + QuotedParamName + ' AS BINARY)'
    else
    if (Copy(ParamInfo.ParamType, 1, 7) = 'DECIMAL') or (Copy(ParamInfo.ParamType, 1, 7) = 'NUMERIC') then
      Result := 'CAST(' + QuotedParamName + ' AS ' + ParamInfo.ParamType + ')'
    else
      Result := QuotedParamName;

    Result := Result + ' AS ' + AnsiQuotedStr(ParamName, '''');
  end;

var
  i, iMin: integer;
  sParams, sUserParams, sSet, sSelect, sCastSelect, sUserSet: string;
  ParamName, OriginalParamName: string;
  Param: TParamDesc;
begin
  if IsFunc then begin
    ASQL := 'SELECT ' + SQLInfo.NormalizeName(Name, FQuoteNames) + '(';
    iMin := 1;
  end
  else begin
    iMin := 0;
    ASQL := 'CALL ' + SQLInfo.NormalizeName(Name, FQuoteNames) + '(';
  end;

  sParams := '';
  sUserParams := '';
  sSet := '';
  sUserSet := '';
  sSelect := '';
  sCastSelect := '';
  for i := iMin to Params.Count - 1 do begin
    Param := Params[i];

    if i > iMin then begin
      sParams := sParams + ', ';
      sUserParams := sUserParams + ', ';
    end;

    ParamName := Param.GetName;
  {$IFDEF LITE}
    if ParamName = '' then begin
      Param.SetName(ParamsInfo[i].Name);
      ParamName := ParamsInfo[i].Name;
    end;
  {$ENDIF}
    case Param.GetParamType of
      pdUnknown, pdInput: begin
        sParams := sParams + '?';
        sUserParams := sUserParams + ':' + ParamName;
      end;
      pdInputOutput, pdOutput: begin
        OriginalParamName := '@' + ParamName;
        ParamName := '@' + SQLInfo.NormalizeName(ParamName, FQuoteNames);

        if Param.GetParamType = pdInputOutput then begin
          if sSet <> '' then begin
            sSet := sSet + ', ';
            sUserSet := sUserSet + ', ';
          end;
          sSet := sSet + ParamName + ' = ?';
          sUserSet := sUserSet + ParamName + ' = :' + Param.GetName;
        end;

        if sSelect = '' then
          sSelect := ParamName
        else
          sSelect := sSelect + ', ' + ParamName;

        if sCastSelect = '' then
          sCastSelect := GetSelectForParam(OriginalParamName, ParamName, ParamsInfo[i])
        else
          sCastSelect := sCastSelect + ', ' + GetSelectForParam(OriginalParamName, ParamName, ParamsInfo[i]);

        sParams := sParams + ParamName;
        sUserParams := sUserParams + ParamName;
      end;
    end;
  end;
  AUserSQL := ASQL + sUserParams + ')';
  ASQL := ASQL + sParams + ')';

  if sSet <> '' then begin
    ASQL := 'SET ' + sSet + '; ' + ASQL;
    AUserSQL := 'SET ' + sUserSet + '; ' + AUserSQL;
  end;
  if sSelect <> '' then begin
    ASQL := ASQL + '; SELECT ' + sCastSelect;
    AUserSQL := AUserSQL + '; SELECT ' + sCastSelect;
  end;
end;

function TMySQLCommand.ForceCreateSPParams: boolean;
begin
  Result := True;
end;

procedure TMySQLCommand.SetLimit0;
var
  Parser: TMyParser;
  Code: integer;
begin
  Parser := TMyParser.Create(FSQL);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  try
    Code := Parser.ToLexem([lxSELECT, lxSET, lxSemicolon]);
    if Code <> lxSELECT then
      Exit;

    Code := Parser.ToLexem([lxLIMIT, lxPROCEDURE, lxFOR, lxLOCK, lxSemicolon]);
    if Code = lxLIMIT then begin
      if (Parser.GetNextToken = lcBlank) and (Parser.GetNextToken = lcNumber) then
        FSQL := Copy(FSQL, 1, Parser.PrevPos) + '0' + Copy(FSQL, Parser.CurrPos + 1, MaxInt);
    end
    else
      if Code <> lcEnd then
        FSQL := Copy(FSQL, 1, Parser.PrevPos) + ' LIMIT 0 ' + Copy(FSQL, Parser.PrevPos + 1, MaxInt)
      else
        FSQL := FSQL + ' LIMIT 0';
  finally
    Parser.Free;
  end;
end;

procedure TMySQLCommand.FillCachedSQL;
var
  Parser: TMyParser;
  StartPos: integer;
  Code: Integer;
begin
  Parser := TMyParser.Create(FSQL);
  try
    Parser.OmitBlank := False;
    Parser.OmitComment := True;
    Parser.QuotedString := True;

    StartPos := Parser.CurrPos;
    repeat
      Code := Parser.GetNextToken;
      if Code = lxQuestion then begin
        FCachedSQL.Append(Copy(FSQL, StartPos + 1, Parser.CurrPos - StartPos - 1));
        StartPos := Parser.CurrPos;
      end;
    until Code = lcEnd;
    FCachedSQL.Append(Copy(FSQL, StartPos + 1, Parser.CurrPos - StartPos));
  finally
    Parser.Free;
  end;
end;

function TMySQLCommand.GetParam(const ParamIdx: integer; var IOParamCnt: integer): TParamDesc;
var
  i, j: integer;
begin
  if not FStoredProc then begin
    Result := FParams[ParamIdx];
    Exit;
  end;

  if IOParamCnt = -1 then begin
    IOParamCnt := 0;
    for i := 0 to FParams.Count - 1 do
      if FParams[i].GetParamType = pdInputOutput then
        Inc(IOParamCnt);
  end;

  if ParamIdx < IOParamCnt then begin // Search pdInputOutput param
    j := 0;
    for i := 0 to FParams.Count - 1 do begin
      Result := FParams[i];
      if Result.GetParamType = pdInputOutput then begin
        if j = ParamIdx then
          Exit;
        Inc(j);
      end;
    end;
  end
  else begin // Search pdInput param
    j := IOParamCnt;
    for i := 0 to FParams.Count - 1 do begin
      Result := FParams[i];
      if Result.GetParamType = pdInput then begin
        if j = ParamIdx then
          Exit;
        Inc(j);
      end;
    end;
  end;

  Result := nil;
  // CR 19492 Assert(False, IntToStr(ParamIdx) + #$D#$A + FSQL);
end;

procedure TMySQLCommand.PlaceParamValues(SQL: AnsiStringBuilder);
var
{$IFDEF HAVE_COMPRESS}
  SendBlobCompressed: boolean;
{$ENDIF}
  IOParamCnt: integer;
  i, Prev: integer;
  Param: TParamDesc;
  ParamIdx: integer;
  Str: string;
begin
{$IFDEF HAVE_COMPRESS}
  case FCompressBlob of
    cbServer, cbClientServer:
      SendBlobCompressed := True;
    else
      SendBlobCompressed := False;
  end;
{$ENDIF}

  SQL.Length := 0;
  IOParamCnt := -1;
  if FParamsInfo.Count = 0 then begin // dbExpress or ParamCheck = False case
    if FCachedSQL.Count = 0 then
      FillCachedSQL;

    ParamIdx := 0;
    for i := 0 to FCachedSQL.Count - 1 do begin
      if FConnection.FIsUtf8 then
        SQL.Append(CRFunctions.UTF8Encode(WideString(FCachedSQL[i])))
      else
        SQL.Append(AnsiString(FCachedSQL[i]));
      if i <> FCachedSQL.Count - 1 then begin
        Param := GetParam(ParamIdx, IOParamCnt);
        if Param <> nil then begin
          if Param.GetParamType = pdResult then begin  // cr-mda 21751
            Inc(ParamIdx);
            Param := GetParam(ParamIdx, IOParamCnt);
          end;
          if Param <> nil then
            FConnection.AppendValueToSQL(SQL, Param.GetDataType, Param.GetItemPtr(0)^, Param.GetNull, FConnection.FUseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF});
        end;
      end;
      Inc(ParamIdx);
    end;
  end
  else begin
    if not FStoredProc and (FParamsInfo.Count <> Params.Count) then
      DatabaseErrorFmt(SIncorrectParamCount, [FParamsInfo.Count, Params.Count]);

    Prev := 1;
    for i := 0 to FParamsInfo.Count - 1 do begin
      Str := Copy(FSQL, Prev, FParamsInfo[i].StartPosition - Prev);
      if FConnection.FIsUtf8 then
        SQL.Append(CRFunctions.UTF8Encode(WideString(Str)))
      else
        SQL.Append(AnsiString(Str));
      Prev := FParamsInfo[i].EndPosition;

      if (FParamsInfo[i].ParamRef <> nil) and not FStoredProc then
        Param := FParamsInfo[i].ParamRef
      else
        Param := GetParam(i, IOParamCnt);

      if Param <> nil then
        FConnection.AppendValueToSQL(SQL, Param.GetDataType, Param.GetItemPtr(0)^, Param.GetNull, FConnection.FUseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF});
    end;
    Str := Copy(FSQL, Prev, Length(FSQL) - Prev + 1);
    if FConnection.FIsUtf8 then
      SQL.Append(CRFunctions.UTF8Encode(WideString(Str)))
    else
      SQL.Append(AnsiString(Str));
  end;
end;

procedure TMySQLCommand.InternalExecute;

  procedure DoExecute;
  var
    SB: AnsiStringBuilder;
    i, Cnt: integer;
    AnsiSQL: AnsiString;
    OpenNextStatus: integer;
    RowsAffectedNext: integer;
    RowsAffectedStatus: integer;
  begin
    Assert(FConnection <> nil);
    Assert(FConnection.FMySQL <> nil);

    if not FOpenNext then begin
      if (FParams.Count = 0) or (Pos('?', FSQL) = 0) then begin // query without parameters
        if Assigned(FOnExecute) then
          FOnExecute(FSQL);

        if FConnection.FIsUtf8 and (FBatchIters = 1) and (FBatchOffset = 0) then
          AnsiSQL := CRFunctions.UTF8Encode(WideString(FSQL))
        else
          AnsiSQL := AnsiString(FSQL);
      end
      else begin // query may contain parameters
        Cnt := 0;
        for i := 0 to FParams.Count - 1 do
          Inc(Cnt, FParams[i].GetSize);
        SB := AnsiStringBuilder.Create(Cnt * 2 + Length(FSQL));
        try
        {$IFDEF PERF_COUNTER}
          PerfCounters[5].Start;
        {$ENDIF}
          PlaceParamValues(SB);
        {$IFDEF PERF_COUNTER}
          PerfCounters[5].Stop;
        {$ENDIF}
          AnsiSQL := SB.ToString;

          if Assigned(FOnExecute) then
            if FConnection.FIsUtf8 then
              FOnExecute(string(CRFunctions.UTF8Decode(AnsiSQL)))
            else
              FOnExecute(string(AnsiSQL));
        finally
          SB.Free;
        end;
      end;

      Check(FConnection.FMySQLAPI.mysql_real_query(FConnection.FMySQL, PAnsiChar(AnsiSQL), Length(AnsiSQL)));
    end;

    FRowsAffectedPrev := FRowsAffected;
    FRowsAffected := CalcRowsAffected;
    if FRequestResultset then begin
      if not FOpenNext then begin
        Assert(FSQLResult = nil);
        // FConnection.FMySQLAPI.mysql_store_result is too slow vs FConnection.FMySQLAPI.mysql_use_result
        FSQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
        // Check(FSQLResult) does not need because FSQLResult may be nil!
        Check;

        if FConnection.IsClient41 and (FConnection.FMySQLAPI.mysql_more_results(FConnection.FMySQL) <> 0) then begin
          OpenNextStatus := 0;
          RowsAffectedNext := FRowsAffected;
          while (FSQLResult = nil) and (OpenNextStatus = 0) do begin
            OpenNextStatus := ShortInt(FConnection.FMySQLAPI.mysql_next_result(FConnection.FMySQL));
            // OpenNextStatus
            //  0  Successful and there are more results
            // -1  Successful and there are no more results
            // >0  An error occurred
            if OpenNextStatus > 0 then begin
              FCursorState := csInactive;
              Check(OpenNextStatus); // error
            end;

            FRowsAffectedPrev := RowsAffectedNext;
            RowsAffectedNext := CalcRowsAffected;
            if RowsAffectedNext > 0 then
              FRowsAffected := FRowsAffected + RowsAffectedNext;
            FSQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
            Check;
          end;
        end;
      end;
    end
    else if (FBatchIters > 1) or (FBatchOffset > 0) then begin
      Assert(FSQLResult = nil);
      FSQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
      Check;

      if FConnection.IsClient41 then begin
        RowsAffectedStatus := 0;
        RowsAffectedNext := FRowsAffected;
        while (FSQLResult = nil) and (RowsAffectedStatus = 0) do begin
          RowsAffectedStatus := ShortInt(FConnection.FMySQLAPI.mysql_next_result(FConnection.FMySQL));
          if RowsAffectedStatus > 0 then begin
            FCursorState := csInactive;
            Check(RowsAffectedStatus);
          end;

          FRowsAffectedPrev := RowsAffectedNext;
          RowsAffectedNext := CalcRowsAffected;
          if RowsAffectedNext > 0 then
            FRowsAffected := FRowsAffected + RowsAffectedNext;
          FSQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
          Check;
        end;
      end;

      Close;
    end
    else
      Close;
  end;

  procedure DoExecutePrepared;
  begin
    CheckStmt(FConnection.FMySQLAPI._mysql_stmt_execute(Fstmt, Self, FConnection.FUseUnicode));

    FRowsAffectedPrev := FRowsAffected;
    FRowsAffected := CalcRowsAffected;
    if FRequestResultset then begin
      FSQLResult := FConnection.FMySQLAPI.mysql_stmt_result_metadata(Fstmt);
      Check;
    end
    else
      Close;
  end;

begin
  // non-SP statements: nonsense
  //     SP statements: wait for fetch
  FCanReadParams := False;
  FIsSelectParams := False;

  FConnection.FMySQLAPI.SetTimeout(FConnection.FMySQL, FCommandTimeout);
  if FConnection.FMySQLAPI is TMySQLAPIDirect then
    TMySqlSession(FConnection.FMySQL).SkipPacket := FStoredProc;

  FExecuting := True;
  FWaitForBreak := False;
  SetCursorState(csExecuting);
  try
    if not GetPrepared then
      DoExecute
    else
      DoExecutePrepared;
    // SetCursorState(csExecuted); - setted on CalcRowsAffected
  finally
    if FSQLResult = nil then
      SetCursorState(csInactive);
    FBreakExecCS.Acquire;
    FExecuting := False;
    FWaitForBreak := False;
    FBreakExecCS.Release;
  end;
end;

procedure TMySQLCommand.Execute;
var
  OldCursorState: TCursorState;
  Connection, UsedConnection: TMySQLConnection;
  WarningCount: cardinal;
  Warnings: TMyErrors;
begin
  if (FCursorState <> csInactive) and (FCursorState <> csPrepared) then
    Exit;

  Connection := FConnection;
  Connection.Lock;
  try
    Assert(FConnection <> nil);
    if FCreateConnection and FUseCreateConnection then
      QuerySwapConnection;
    UsedConnection := FConnection;
    UsedConnection.Lock;
    try
      Connection.Release;
      Connection := nil;

      OldCursorState := GetCursorState;
      try
        InternalExecute;

      {$IFDEF AUTOTEST}
        inherited Execute;
      {$ENDIF}
      except
        if GetCursorState <> csInactive then  // ODAC: on lost connection
          SetCursorState(OldCursorState);
        if Assigned(FAfterExecute) then
          FAfterExecute(False);
        if FWaitForBreak then
          Abort
        else
          raise;
      end;

    finally
      if Assigned(UsedConnection.FOnWarning) and (UsedConnection.GetLastWarningCount > 0) then begin
        if UsedConnection.FMySQLAPI is TMySQLAPIDirect then
          WarningCount := TMySqlSession(UsedConnection.FMySQL).warningCount
        else
          WarningCount := 0;
        Warnings := UsedConnection.RequestWarnings;
        try
          UsedConnection.FOnWarning(Warnings);
        finally
          if UsedConnection.FMySQLAPI is TMySQLAPIDirect then
            TMySqlSession(UsedConnection.FMySQL).warningCount := WarningCount;
          Warnings.Free;
        end;
      end;

      UsedConnection.Release;
    end;

    if Assigned(FAfterExecute) then
      FAfterExecute(True);
  finally
    if Connection <> nil then
      Connection.Release;
  end;
end;

procedure TMySQLCommand.Close;
var
  SQLResult: PMYSQL_RES;
  res: shortint;
begin
  if not GetPrepared then begin
    if FSQLResult = nil then
      SQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL)
    else
      SQLResult := FSQLResult;
    if FConnection.FMySQLAPI is TMySQLAPIDirect then
      TMySqlSession(FConnection.FMySQL).SkipPacket := False;
    if SQLResult <> nil then
      FConnection.FMySQLAPI.mysql_free_result(SQLResult);

    if FConnection.IsClient41 then begin
      res := ShortInt(FConnection.FMySQLAPI.mysql_next_result(FConnection.FMySQL));
      while res = 0 do begin
        SQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
        if SQLResult <> nil then
          FConnection.FMySQLAPI.mysql_free_result(SQLResult);
        res := ShortInt(FConnection.FMySQLAPI.mysql_next_result(FConnection.FMySQL));
      end;
      if res > 0 then
        Check;
    end;
  end
  else begin
    FConnection.FMySQLAPI.mysql_stmt_free_result(Fstmt);
  end;
end;

procedure TMySQLCommand.UseNextResult;
begin
  FSQLResult := FSQLResultNext;
  FRowsAffected := FRowsAffectedNext;
  FSQLResultNext := nil;
end;

procedure TMySQLCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TMySQLConnection(Value);
  end;
end;

procedure TMySQLCommand.SetSQL(const Value: string);
begin
  inherited;
{$IFDEF LITE}
  FParamsInfo.Clear;
{$ENDIF}
  FCachedSQL.Clear;
end;

function TMySQLCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TMySQLCommand.SetCursorState(Value: TCursorState);
begin
  FCursorState := Value;
end;

function TMySQLCommand.GetProp(Prop: integer; out Value: variant): boolean;
var
  ra: integer;
begin
  Result := True;

  case Prop of
    prRowsProcessed: begin
      if FIsSelectParams then begin
        Assert(FStoredProc);
        ra := FRowsAffectedPrev;
      end
      else
        ra := FRowsAffected;
      if ra = -1 then
        Value := 0
      else
        Value := ra;
    end;
    prCanReadParams:
      Value := FCanReadParams;
    prIsSelectParams:
      Value := FIsSelectParams;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TMySQLCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommandTimeout := Value;
    prCanReadParams:
      FCanReadParams := Value;
    prIsStoredProc:
      FStoredProc := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TMySQLCommand.BreakExec;
var
  Conn: TMySQLConnection;
  s: AnsiString;
begin
  Conn := nil;
  FBreakExecCS.Acquire;
  try
    FWaitForBreak := True;

    if FExecuting then begin
      Conn := TMySQLConnection.Create;
      Conn.Assign(FConnection);
      Conn.Connect('');

      try
        s := AnsiString('KILL ' + IntToStr(FConnection.FMySQLAPI.mysql_thread_id(FConnection.MySQL)));
        Conn.FMySQLAPI.mysql_real_query(Conn.FMySQL, PAnsiChar(s), LengthA(s)); // may return error if connection already terminated
      except
        // silent
      end;
    end;
  finally
    Conn.Free;
    FBreakExecCS.Release;
  end;
end;

function TMySQLCommand.IsLabelSyntax(Code: integer; PrevCode: integer): boolean;
begin
  Result :=  (PrevCode = lcIdent) and
             ((Code = lxBEGIN) or (Code = lxLOOP) or
             (Code = lxREPEAT) or (Code = LxWHILE));
end;

class function TMySQLCommand.GetTableInfoClass: TTableInfoClass;
begin
  Result := TMyTableInfo;
end;

class function TMySQLCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TMySQLInfo;
end;

class function TMySQLCommand.GetParserClass: TSQLParserClass;
begin
  Result := TMyParser;
end;

{$IFNDEF LITE}
class function TMySQLCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TMyMapRules;
end;
{$ENDIF}

{ TMyTableInfo }

function TMyTableInfo.GetTableNameFull: string;
begin
  Result := FTableNameFull;
end;

procedure TMyTableInfo.SetTableNameFull(const Value: string);
begin
  FTableNameFull := Value;
end;

{ TMySQLInfo }

function TMySQLInfo.LeftQuote: Char;
begin
  Result := Char('`');
end;

function TMySQLInfo.RightQuote: Char;
begin
  Result := Char('`');
end;

function TMySQLInfo.IsCursorSQLType(Code: Integer): boolean;
begin
  Result := Code = lxSHOW;
end;

function TMySQLInfo.IsQuoted(const Value: string): boolean;
var
  l: integer;
begin
//  Assert(Pos('.', Value) = 0, 'In func IsQuotedName delimited values not allowed'); // If fieldname contains '.'

  l := Length(Value);
  if (l <= 1) then
    Result := False
  else
    Result :=
      ((Value[1] = '`') and (Value[l] = '`')) or
      ((Value[1] = '"') and (Value[l] = '"')) or
      ((Value[1] = '''') and (Value[l] = ''''));
end;

function TMySQLInfo.Quote(const Value: string; const LeftQ: Char; const RightQ: Char): string;
begin
  if not IsQuoted(Value) then
    Result := AnsiQuotedStr(Value, '`')
  else
    Result := Value;
end;

function TMySQLInfo.UnQuote(const Value: string): string;
begin
  if IsQuoted(Value) then
    Result := AnsiDequotedStr(Value, Value[1])
  else
    Result := Value;
end;

procedure TMySQLInfo.SplitObjectName(const Name: string; out DataBase: string; out ObjName: string);
var
  Info: TSQLObjectInfo;
begin
  inherited SplitObjectName(Name, Info);
  DataBase := Info.Schema;
  ObjName := Info.Name;
end;

procedure TMySQLInfo.SplitObjectName(const Name: string; out Info: TSQLObjectInfo);
begin
  inherited SplitObjectName(Name, Info);
  Info.Catalog := Info.Schema;
  Info.Schema := '';
end;

function TMySQLInfo.NamesFromList(List: TStrings; NormalizedName: boolean = True; Delimiter: string = ';'): string;
begin
  Result := inherited NamesFromList(List, NormalizedName, ',');
end;

{ TMySQLRecordset }

constructor TMySQLRecordset.Create;
begin
  inherited;
  FNullForZeroDate := True;
  FFetchRows := 25;
  FEnableBoolean := True;
  FBinaryAsString := True;
  FCreateConnection := True;
  FUseHandler := False;
end;

function TMySQLRecordset.GetFieldDescType: TFieldDescClass;
begin
  Result := TMySQLFieldDesc;
end;

procedure TMySQLRecordset.ExplicitInitFields;
var
  NeedReset: boolean;
begin
  NeedReset := GetCommand.GetCursorState <= csPrepared;
  try
    inherited;
  finally
    if NeedReset then begin
      FCommand.SetCursorState(csInactive);
      FCommand.CommandType := ctUnknown;
    end;
  end;
end;

procedure TMySQLRecordset.Check(const Status: IntPtr);
begin
  Assert(FCommand.FConnection <> nil);
  if Status = nil then
    FCommand.FConnection.MySQLError(Component);
end;

procedure TMySQLRecordset.Check(const Status: integer);
begin
  Assert(FCommand.FConnection <> nil);
  if Status <> 0 then
    FCommand.FConnection.MySQLError(Component);
end;

procedure TMySQLRecordset.Check;
begin
  Assert(FCommand.FConnection <> nil);
  FCommand.FConnection.Check(Component);
end;

procedure TMySQLRecordset.CreateCommand;
var
  Cmd: TMySQLCommand;
begin
  Cmd := TMySQLCommand.Create;
  Cmd.FRequestResultset := True;
  Cmd.FEnableBoolean := FEnableBoolean;
  Cmd.FCreateConnection := FCreateConnection;
  Cmd.FUseCreateConnection := not FFetchAll;
  SetCommand(Cmd);
end;

procedure TMySQLRecordset.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TMySQLCommand(Value);
end;

function TMySQLRecordset.GetIsClient41: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FIsClient41 := FCommand.FConnection.IsClient41;
  Result := FIsClient41;
end;

function TMySQLRecordset.GetUseUnicode: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FUseUnicode := FCommand.FConnection.FUseUnicode;
  Result := FUseUnicode;
end;

function TMySQLRecordset.GetIsUtf8: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FIsUtf8 := FCommand.FConnection.FIsUtf8;
  Result := FIsUtf8;
end;

function TMySQLRecordset.GetOptimizedBigInt: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FOptimizedBigInt := FCommand.FConnection.FOptimizedBigInt;
  Result := FOptimizedBigInt;
end;

function TMySQLRecordset.GetNullForZeroDelphiDate: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FNullForZeroDelphiDate := FCommand.FConnection.FNullForZeroDelphiDate;
  Result := FNullForZeroDelphiDate;
end;

function TMySQLRecordset.GetServerPrimaryVer: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FServerPrimaryVer := FCommand.FConnection.FServerPrimaryVer;
  Result := FServerPrimaryVer;
end;

function TMySQLRecordset.GetServerMinorVer: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FServerMinorVer := FCommand.FConnection.FServerMinorVer;
  Result := FServerMinorVer;
end;

function TMySQLRecordset.GetServerReleaseVer: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FServerReleaseVer := FCommand.FConnection.FServerReleaseVer;
  Result := FServerReleaseVer;
end;

function TMySQLRecordset.CanDisconnect: boolean;
begin
  Result := inherited CanDisconnect;

  Assert(FCommand <> nil);
  Assert(FCommand.FConnection <> nil);
  if Result and FUseHandler and
    (not FCommand.FConnection.IsClient41 or not FCommand.FConnection.IsServer41) then // See TCustomMyTable.SeparatedHandler
    Result := False;
end;

function TMySQLRecordset.RowsReturn: boolean;
begin
  if FCommand.CommandType <> ctUnknown then
    Result := inherited RowsReturn
  else        //we need to know this info even if CommandType is not set(TCustomDADataSet.DoAfterExecute)
    Result := (FCommand.FSQLResult <> nil) or (FCommand.FOpenNext and (FCommand.FSQLResultNext <> nil));
end;

function TMySQLRecordset.RequiredReInitFields: boolean;
var
  OldFieldCount, NewFieldCount: integer;
begin
  Result := False;

  if FCommand.FStoredProc and (FCommand.FSQLResult <> nil) then begin
    OldFieldCount := Length(FValueLens); // using FFields.Count is incorrect if there are dkCalculated fields
    NewFieldCount := FCommand.FConnection.FMySQLAPI.mysql_num_fields(FCommand.FSQLResult);
    if (OldFieldCount > 0) and (OldFieldCount <> NewFieldCount) then
      Result := True;
  end;
end;

procedure TMySQLRecordset.ExecCommand(Iters: integer = 1; Offset: integer = 0);
begin
  try
    FOpenNextState := osNotChecked;
    try
      inherited;

      if FCommand.FOpenNext and (FCommand.CommandType = ctUnknown) then
        FCommand.UseNextResult;

    except
      FreeResult(False {nonsense on error processing}, True);
      FCommand.ReleaseSwapConnection;
      raise;
    end;

    if FCommand.FSQLResult <> nil then begin
      FCommand.CommandType := ctCursor;
      FCommand.SetCursorState(csExecuted);
    end
    else
      if not FCommand.FOpenNext then begin
        FCommand.CommandType := ctStatement;
        FreeResult(True, True);
        if FCommand.FSQLResultNext <> nil then begin
          FCommand.UseNextResult;
          FCommand.CommandType := ctCursor;
        end
        else
          FCommand.CommandType := ctStatement;
      end;
  finally
    if FCommand.CommandType <> ctCursor then
      FCommand.SetCursorState(csInactive);
  end;
end;

procedure TMySQLRecordset.Disconnect;
begin
  if not Prepared then
    FreeResult(False, True);

  GetIsClient41;
  GetUseUnicode;
  GetOptimizedBigInt;
  GetServerPrimaryVer;
  GetServerMinorVer;

  inherited;
end;

procedure TMySQLRecordset.InternalPrepare;
var
  cColumns: cardinal;
begin
  try
    inherited;

    Assert(FCommand.Fstmt <> nil);
    cColumns := FCommand.FConnection.MySQLAPI.mysql_stmt_field_count(FCommand.Fstmt);

    if cColumns > 0 then
      FCommand.CommandType := ctCursor
    else
      FCommand.CommandType := ctStatement;
    FCommand.FSQLResult := FCommand.FConnection.FMySQLAPI.mysql_stmt_result_metadata(FCommand.Fstmt);
  except
    InternalUnPrepare;
    raise;
  end;
end;

procedure TMySQLRecordset.InternalUnPrepare;
begin
  try
    if (FCommand.FSQLResult <> nil) and (FCommand.Fstmt <> nil {Prepared cannot be used. See TData.UnPrepare for details}) then
      FCommand.FSQLResult := nil;

    inherited;
  finally
    FCommand.SetCursorState(csInactive);
    FCommand.CommandType := ctUnknown;
    FCommand.ReleaseSwapConnection;
  end;
end;

procedure TMySQLRecordset.FreeResult(const TryGetNextResult, TryToFinishFetch: boolean);
var
  i: integer;
  s: AnsiString;
  row: PMYSQL_ROW;
  ForbidQueries: boolean; // CR-M14748
  OpenNextStatus: integer;
  Connection: TMySQLConnection;
begin
  {OFS('+TMySQLRecordset.FreeResult (TryGetNextResult = ' + BoolToStr(TryGetNextResult, True) + ', TryToFinishFetch = ' + BoolToStr(TryToFinishFetch, True) + ')');
  OFS('OpenNextState = ' + IntToStr(Integer(FOpenNextState)));
  OFS('CommandType = ' + IntToStr(Integer(CommandType)));
  if FCommand <> nil then
    OFS('Command.CursorState = ' + IntToStr(Integer(FCommand.FCursorState)));}
  Connection := nil;
  if (FCommand.FConnectionSwap = nil) and (FCommand.FConnection <> nil) then begin
    Connection := FCommand.FConnection;
    Connection.Lock;
  end;
  try
    // Called with TryGetNextResult = True on
    //   - Executing command with first non-SELECT statement (INSERT;SELECT;SELECT;) - TMySQLRecordset.ExecCommand
    //   - Closing cursor - TMySQLRecordset.InternalClose
    //   - Fetch ending - TMySQLRecordset.Fetch.ProcessRow

    ForbidQueries := False;
    SetLength(FFetchBnd, 0);
    if Prepared then begin
      // skip underfetched rows
      // without KILL optimization
      if (FCommand.FCursorState <> csPrepared) and (FCommand.CommandType = ctCursor) then begin
        // ??? FCommand.FConnection.FMySQLAPI.mysql_stmt_free_result(FCommand.Fstmt);
        i := FCommand.FConnection.FMySQLAPI.mysql_stmt_errno(FCommand.Fstmt);
        repeat
          FCommand.CheckStmt(i);
          FCommand.FConnection.FMySQLAPI._mysql_stmt_bind_result(FCommand.Fstmt, FFetchBnd);
          i := FCommand.FConnection.FMySQLAPI._mysql_stmt_fetch(FCommand.Fstmt, row);
        until (i = MYSQL_NO_DATA) or (i = MYSQL_DATA_TRUNCATED);
      end;

      Exit;
    end;

    if (FCommand = nil) or
      (FCommand.FConnection = nil) or
      (FCommand.FConnection.FMySQLAPI = nil) or
      (FCommand.FConnection.FMySQL = nil)
    then begin
      FOpenNextState := osNoMoreResults;
      if FCommand <> nil then begin
        FCommand.CommandType := ctUnknown;
        FCommand.FCursorState := csInactive;
      end;
      Exit;
    end;

    if FCommand.FSQLResult <> nil then begin
      Assert(FCommand <> nil);
      Assert(FCommand.FConnection <> nil);

      if FFetchAll
        or not FCreateConnection{prCreateConnection}
        // or (FConnectionSwap = nil) // Disconnected mode
        or not FIsCanFastClose then begin
        if TryToFinishFetch then begin
          if FWaitForFetchBreak then begin
            FCommand.Executing := True;
            try
              FCommand.BreakExec;
            finally
              FCommand.Executing := False;
            end;
          end;
          while FCommand.FConnection.FMySQLAPI.mysql_fetch_row(FCommand.FSQLResult) <> nil do;
        end;
      end
      else begin
        Assert(FCommand.FConnectionSwap <> nil);

        // Quick terminate http:/devart.com/forums/viewtopic.php?p=9095
        i := FFetchRows;
        while FCommand.FConnection.FMySQLAPI.mysql_fetch_row(FCommand.FSQLResult) <> nil do begin
          Dec(i);
          if i = 0 then begin
            s := AnsiString('KILL ' + IntToStr(FCommand.FConnection.FMySQLAPI.mysql_thread_id(FCommand.FConnection.MySQL)));
            FCommand.FConnectionSwap.FMySQLAPI.mysql_real_query(FCommand.FConnectionSwap.FMySQL, PAnsiChar(s), LengthA(s)); // may return error if connection already terminated
            ForbidQueries := True;
            while FCommand.FConnection.FMySQLAPI.mysql_fetch_row(FCommand.FSQLResult) <> nil do;
            Break;
          end;
        end;
      end;

      if IsClient41 and
        (FCommand.FConnection.FMySQLAPI.mysql_more_results(FCommand.FConnection.MySQL) <> 0) then
        FOpenNextState := osMoreResults
      else begin
        FOpenNextState := osNoMoreResults;
        FCommand.ReleaseSwapConnection(ForbidQueries); //! Before FCommand.FConnection.FMySQLAPI.mysql_free_result to bypass mem leaks
      end;
      FCommand.FConnection.FMySQLAPI.mysql_free_result(FCommand.FSQLResult);
      FCommand.FSQLResult := nil;
    end
    else
      if (FOpenNextState <> osNoMoreResults) and ((FCommand.FSQLResultNext <> nil) or (FCommand.FSQLResult = nil)) then
        FOpenNextState := osMoreResults;

    if (FOpenNextState = osNoMoreResults) or not TryGetNextResult or not IsClient41 then
      Exit;

    try
      FCommand.FRowsAffectedNext := -2;
      while FCommand.FSQLResultNext = nil do begin
        OpenNextStatus := ShortInt(FCommand.FConnection.FMySQLAPI.mysql_next_result(FCommand.FConnection.FMySQL));
        // OpenNextStatus
        //  0  Successful and there are more results
        // -1  Successful and there are no more results
        // >0  An error occurred

        if OpenNextStatus > 0 then begin
          FOpenNextState := osError;
          FCommand.FCursorState := csInactive;
          FCommand.CommandType := ctUnknown;
          Check(OpenNextStatus); // error
        end
        else
        if OpenNextStatus = 0 then
          FOpenNextState := osMoreResults
        else // OpenNextStatus < 0
          FOpenNextState := osNoMoreResults;

        if FCommand.FRowsAffectedNext = -2 then
          FCommand.FRowsAffectedPrev := FCommand.FRowsAffected
        else
          FCommand.FRowsAffectedPrev := FCommand.FRowsAffectedNext;
        FCommand.FRowsAffectedNext := FCommand.CalcRowsAffected;
        try
          FCommand.FSQLResultNext := FCommand.FConnection.FMySQLAPI.mysql_use_result(FCommand.FConnection.FMySQL); // ??? check mysql_field_count
          // Check(FSQLResultNext) does not need because FSQLResultNext may be nil!
          Check;
        except
          if FCommand.FSQLResultNext <> nil then begin
            FCommand.FConnection.FMySQLAPI.mysql_free_result(FCommand.FSQLResultNext);
            FCommand.FSQLResultNext := nil;
          end;
          raise;
        end;
        if FOpenNextState <> osMoreResults then
          Break;
      end;
    finally
      if FOpenNextState <> osMoreResults then
        FCommand.ReleaseSwapConnection(ForbidQueries);
    end;
  finally
    if (FCommand <> nil) then begin
      if ((FCommand.GetCursorState = csFetching) or (FCommand.GetCursorState = csFetchingAll)) then
        FCommand.SetCursorState(csFetched);
      if Connection <> nil then
        Connection.Release;
    end;

    {OFS('OpenNextState = ' + IntToStr(Integer(FOpenNextState)));
    OFS('CommandType = ' + IntToStr(Integer(CommandType)));
    if FCommand <> nil then
      OFS('Command.CursorState = ' + IntToStr(Integer(FCommand.FCursorState)));
    OFS('-TMySQLRecordset.FreeResult');}
  end;
end;

procedure TMySQLRecordset.DrainResults;
begin
  repeat
    FreeResult(True, True);
    if FCommand <> nil then
      FCommand.UseNextResult;
  until FOpenNextState <> osMoreResults;
end;

procedure TMySQLRecordset.Reopen;
begin
  DrainResults;

  inherited;
end;

function TMySQLRecordset.CheckNextResult: boolean;
begin
  Result := FOpenNextState = osMoreResults;
end;

procedure TMySQLRecordset.InternalOpen(DisableInitFields: boolean = False);
begin
  try
    inherited;
  except
    FreeResult(False {nonsense on error processing}, True);
    raise;
  end;
end;

procedure TMySQLRecordset.InternalClose;
var
  i: integer;
  FieldDesc: TMySQLFieldDesc;
begin
  if (FCommand <> nil) and FCommand.FOpenNext then
    FreeResult(True, True)
  else
    DrainResults;

  if FCommand <> nil then begin
    FCommand.FCursorState := csInactive;
    if not Prepared then
      FCommand.CommandType := ctUnknown;
  end;

  // For refreshQuick
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TMySQLFieldDesc(FFields[i]);
    if (FieldDesc.MySQLType = FIELD_TYPE_TIMESTAMP) and (FieldDesc.TableInfo <> nil) then
      TMyTableInfo(FieldDesc.TableInfo).FMaxTimestamp := 0;
  end;

  inherited;
end;

procedure TMySQLRecordset.FillTablesAliases(Parser: TSQLParser);

  function IsInNestedQuery(BracketCount: integer): boolean;
  begin
    Result := BracketCount > 0;
  end;

var
  SQLObjName: string;// Table name
  StLex, Alias: string;
  Code, PrevCode: integer;
  Field: TFieldDesc;
  ObjNameFull: string;
  HandlerSyntax: boolean;
  BracketCount: integer;
  TableInfo: TCRTableInfo;
  PrevWasNestedQuery: boolean;
  i, j: integer;
begin
  PrevWasNestedQuery := False;
  Parser.ToBegin;
  HandlerSyntax := FUseHandler and (Parser.ToLexem(lxHANDLER) <> lcEnd);
  BracketCount := 0;
  if HandlerSyntax or (Parser.ToLexem(lxSELECT) <> lcEnd) then begin
    if HandlerSyntax or (Parser.ToLexem(lxFROM, True) <> lcEnd) then
      repeat
        repeat
          Code := Parser.GetNext(StLex); // Omit blank
        until Code <> lcBlank;

        Alias := '';

        if not HandlerSyntax then begin
          if not PrevWasNestedQuery then begin
            if IsInNestedQuery(BracketCount) then begin
              if Code = lxLeftBracket then begin
                Code := Parser.ToLexem([], True); // goto lxRightBracket

                if Code <> lcEnd then begin
                  repeat
                    Code := Parser.GetNext(StLex); // Omit blank
                  until Code <> lcBlank;
                  SQLObjName := StLex;
                end
                else
                  SQLObjName := '';
              end
              else
                SQLObjName := StLex;
            end
            else begin
              if Code = lxLeftBracket then begin
                repeat
                  Code := Parser.ToLexem([lxFROM], True);

                  if Code = lxFROM then begin
                    Inc(BracketCount);

                    repeat
                      Code := Parser.GetNext(StLex); // Omit blank
                    until (Code <> lcBlank);
                    SQLObjName := StLex;
                  end
                  else
                    SQLObjName := '';
                until Code <> lxLeftBracket ;
              end
              else
                SQLObjName := StLex;
            end;

            // TableName
            PrevCode := 0;
            while True do begin
              Code := Parser.GetNext(StLex);

              if Code = lcBlank then begin
                repeat
                  Code := Parser.GetNext(StLex); // Omit blank
                until Code <> lcBlank;

                if (Code <> lxPoint) and (PrevCode <> lxPoint) then
                  Break;
              end;

              if (Code in [lxComma, lxSemicolon, lxRightBracket, lcEnd]) or ((Code >= lxSQLFirst) and (PrevCode <> lxPoint)) then
                Break;

              PrevCode := Code;
              SQLObjName := SQLObjName + StLex;
            end;
          end
          else
            SQLObjName := '';

          if not (Code in [lcEnd, lxComma, lxSemicolon]) then begin
            if Code = lxAS then begin
              repeat
                Code := Parser.GetNext(Alias); // Omit blank
              until Code <> lcBlank;

              Code := Parser.GetNextToken;
            end
            else
            if Code = lcIdent then begin
              Alias := StLex;
              Code := Parser.GetNextToken;
            end
            else begin
              Alias := '';
              Parser.Back;
            end;

            if PrevWasNestedQuery then begin
              SQLObjName := Alias;
              PrevWasNestedQuery := False;
            end;

            if Code <> lxComma then begin
              if (Code <> lxRightBracket) or (BracketCount <= 1) then
                Code := Parser.ToLexem([lxSemicolon, lxComma, lxJOIN, lxWHERE, lxHAVING, lxGROUP, lxORDER, lxUNION], IsInNestedQuery(BracketCount));

              if Code = lxSemicolon then
                Code := Parser.ToLexem(lxFROM, True);

              if Code in [lxWHERE, lxHAVING, lxGROUP, lxORDER] then
                Code := Parser.ToLexem([lxUNION], IsInNestedQuery(BracketCount));

              if Code = lxUNION then
                Code := Parser.ToLexem(lxFROM, True);

              if IsInNestedQuery(BracketCount) and (Code = lxRightBracket) then begin
                Dec(BracketCount);
                PrevWasNestedQuery := True;
              end
              else
                if Code <> lcEnd then
                  Code := lxComma
            end;
          end;
        end
        else
          SQLObjName := StLex; // TableName

        if SQLObjName <> '' then begin
          SQLObjName := MySQLInfo.NormalizeName(SQLObjName, False, True);

          // Remove Database name (not supported in this version of MySQL)
          ObjNameFull := SQLObjName;
          while True do begin
            i := Pos('.', SQLObjName);
            if i = 0 then
              Break;
            SQLObjName := Copy(SQLObjName, i + 1, 1000);
          end;
          if SQLObjName = '' then
            DatabaseError('TableName cannot be empty');

          if Alias = '' then begin
            i := GetSQLObjectIndex(SQLObjName);
            // i may be -1 for queries like "SELECT count(uid) FROM ALL_TYPES"
            if i <> - 1 then begin
              TableInfo := FTablesInfo[i];
              if not AnsiSameText(TableInfo.TableName, TableInfo.TableAlias) and
                  AnsiSameText(TableInfo.TableAlias, SQLObjName) then begin //IsView
                TableInfo.IsView := True;
                for j := 0 to FFields.Count - 1 do
                  if TCRFieldDesc(FFields[j]).TableInfo = TableInfo then
                    FFields[j].ActualName := Fields[j].Name;
              end;

              TableInfo.TableAlias := '';
            end
            else
              if ServerPrimaryVer >= 5 then
                i := AddSQLObjectIfNeed(SQLObjName, True);
          end
          else begin
            i := AddSQLObjectIfNeed(Alias{MySQL specific}, False);
            FTablesInfo[i].TableName := MySQLInfo.NormalizeName(SQLObjName);
            FTablesInfo[i].TableAlias := Alias;
          end;

          // i may be -1 for queries like "SELECT count(uid) FROM ALL_TYPES"
          if i <> - 1 then
            FTablesInfo[i].TableNameFull := MySQLInfo.NormalizeName(ObjNameFull);
        end;
      until (Code <> lxComma) and (Code <> lxRightBracket);
  end
  else begin
    Parser.ToBegin;
    // Correct MySQL "SHOW COLUMNS" bug and "DESC" bug
    Code := Parser.ToLexem([lxSHOW, lxDESC]);
    if Code = lxSHOW then
      Code := Parser.ToLexem(lxCOLUMNS);

    if Code <> lcEnd then begin
      Field := Fields.FindField('Type');
      if Field <> nil then begin
        Field.Length := 256;
        if not FLongStrings then
          Field.DataType := dtMemo;
      end;
    end;
  end;

  // correct ObjNameFull
  for i := 0 to FTablesInfo.Count - 1 do
    if FTablesInfo[i].TableNameFull = '' then
      FTablesInfo[i].TableNameFull := FTablesInfo[i].TableName;
end;

procedure TMySQLRecordset.InternalCreateFieldDescs(Parser: TSQLParser);
var
  ShowStatement, ExplainStatement4{CR11007}: boolean;
  LenInChar: cardinal;

  function IsFlagSetted(const Flags: cardinal; const Flag: cardinal): boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Result := (Flags and Flag) <> 0;
  end;

  function GetFieldLengthInChars(const pField: TMYSQL_FIELD): cardinal;
  var
    lcFieldName: string; IsCharTypes: boolean;
  begin
    IsCharTypes := (pField.MyType in [FIELD_TYPE_VAR_STRING, FIELD_TYPE_VARCHAR,
      FIELD_TYPE_STRING, FIELD_TYPE_NULL, FIELD_TYPE_ENUM, FIELD_TYPE_SET,
      FIELD_TYPE_GEOMETRY, FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB,
      FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB, FIELD_TYPE_JSON]) and
      (pField.CharsetNr <> 63) and not ShowStatement and
      not ((pField.OrgTable = '') and (pField.LengthInBytes = 1023));
    if UseUnicode and IsCharTypes then begin
      Result := pField.LengthInBytes div FCommand.FConnection.FDBCharLength;
      LenInChar := Result;
    end
    else if (pField.MyType in [FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME, FIELD_TYPE_DATE, FIELD_TYPE_TIME]) and (pField.CharsetNr <> 63) then begin
      Result := pField.LengthInBytes div FCommand.FConnection.FDBCharLength;
      LenInChar := Result;
    end
    else begin
      Result := pField.LengthInBytes;
      if IsCharTypes then
        LenInChar := pField.LengthInBytes div FCommand.FConnection.FDBCharLength
      else
        LenInChar := Result;
    end;

    // Correct MySQL "SHOW ..." bugs
    if ShowStatement then begin
      lcFieldName := LowerCase(string(pField.Name));
      Parser.ToBegin;

      if (lcFieldName = 'status')
        and (Result <= 10)
        and (Parser.ToLexem(lxSTATUS) <> lcEnd) then
        Result := MaxWord
      else
      if ((lcFieldName = 'privilege') or (lcFieldName = 'context'))
        and (Result < 30)
        and (Parser.ToLexem(lxPRIVILEGES) <> lcEnd) then
        Result := 40
      else
      if ((lcFieldName = 'state') or (lcFieldName = 'command'))
        and (Parser.ToLexem(lxPROCESSLIST) <> lcEnd) then
        Result := Result * 3
      else
      if (lcFieldName = 'slave_io_state')
        and (Result < 80)
        and (Parser.ToLexem(lxSLAVE) <> lcEnd) and (Parser.ToLexem(lxSTATUS) <> lcEnd) then
        Result := 80
      else
      if ((lcFieldName = 'slave_io_running') or (lcFieldName = 'slave_sql_running'))
        and (Result < 3)
        and (Parser.ToLexem(lxSLAVE) <> lcEnd) and (Parser.ToLexem(lxSTATUS) <> lcEnd) then
        Result := Result * 3
      else
      if ((lcFieldName = 'last_error') or
          (lcFieldName = 'last_io_error') or
          (lcFieldName = 'last_sql_error'))
        and (Result < 80)
        and (Parser.ToLexem(lxSLAVE) <> lcEnd) and (Parser.ToLexem(lxSTATUS) <> lcEnd) then
        Result := 2048
      else
      if ((ServerPrimaryVer = 4) and (ServerMinorVer = 0)) or (ServerPrimaryVer = 5) then begin
        if (lcFieldName = 'create table') and (Parser.ToLexem(lxCREATE) <> lcEnd) then
          Result := MaxWord
        else
        if ServerPrimaryVer = 5 then begin
          if ((lcFieldName = 'create procedure') or (lcFieldName = 'create function')) and (Parser.ToLexem(lxCREATE) <> lcEnd) then
            Result := MaxWord
          else
          if ServerReleaseVer > 51 then begin
            if lcFieldName = 'user' then
              Result := 16 //max allowed user name
            else
            if lcFieldName = 'host' then
              Result := 60;
          end;
        end;
      end;

      if (ServerPrimaryVer = 5) and (ServerMinorVer = 0) then begin
        Parser.ToBegin;
        if Parser.ToLexem(lxENGINES) <> lcEnd then
          Result := Result * 3 + 1;
      end;
    end;

    if ExplainStatement4 then
      if LowerCase({$IFDEF NEXTGEN}string{$ENDIF}(pField.Name)) = 'type' then
        Result := MaxWord - 1; // to prevent range check error at Field.Size := LengthInChars + 1;
  end;

  procedure DescribeFieldDesc(const pField: TMYSQL_FIELD; Field: TMySQLFieldDesc);
  var
    Idx: integer;
    pField_Name, pField_OrgName, pField_Table, pField_OrgTable: string;
    InternalType, SubDataType: word;
    Len, Scale: Integer;
    Fixed: boolean;
    FieldLengthInChars: cardinal;
  begin
    // SELECT FieldName FieldAlias FROM TableName TableAlias
    // pField.Name = 'FieldAlias'
    // pField.Table = 'TableAlias'

    if IsUtf8 then begin
      pField_Name := string(CRFunctions.UTF8Decode(pField.Name));
      pField_OrgName := string(CRFunctions.UTF8Decode(pField.OrgName));
      pField_Table := string(CRFunctions.UTF8Decode(pField.Table));
      pField_OrgTable := string(CRFunctions.UTF8Decode(pField.OrgTable));
    end
    else begin
      pField_Name := string(pField.Name);
      pField_OrgName := string(pField.OrgName);
      pField_Table := string(pField.Table);
      pField_OrgTable := string(pField.OrgTable);
    end;

    Field.Name := pField_Name;
    // IsView := (pField.OrgTable <> '') and (UpperCase(pField.OrgTable) <> UpperCase(pField.Table));
    Field.ActualName := pField_OrgName;

    if pField_Table <> '' then begin
      Idx := AddSQLObjectIfNeed(pField_Table, False);
      if pField_OrgTable <> '' then
        FTablesInfo[Idx].TableName := MySQLInfo.NormalizeName(pField_OrgTable);
      FTablesInfo[Idx].TableAlias := MySQLInfo.NormalizeName(pField_Table); // pField^.table may contain or object name or alias
      Field.TableInfo := FTablesInfo[Idx];
    end;

    FieldLengthInChars := GetFieldLengthInChars(pField);
    if not FNumberFieldsAsString and (pField.MyType in [FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL]) then begin // MySQL peculiarity
      if pField.Decimals = 0 then begin
        FieldLengthInChars := FieldLengthInChars - 1;
        LenInChar := LenInChar - 1;
      end
      else begin
        FieldLengthInChars := FieldLengthInChars - 2;
        LenInChar := LenInChar - 2;
      end;
    end;

    Field.FMySQLType := pField.MyType;
    Field.DBType := TMyConverterManager.GetDBType(pField.MyType, FieldLengthInChars, pField.Flags, pField.CharsetNr);
    if FieldLengthInChars > cardinal(MaxInt) then
      Field.DBLength := MaxInt
    else
      Field.DBLength := FieldLengthInChars;
    Field.DBScale := pField.Decimals;

    DetectFieldType({$IFNDEF LITE}Field.Name,{$ENDIF} Field.DBType, Field.DBLength, Field.DBScale, Field.DBLength, pField.Table, InternalType, SubDataType, Len, Scale, Fixed);
    if LenInChar > cardinal(MaxInt) then  //FieldLengthInChars may contain a value in bytes
      Field.DBLength := MaxInt
    else
      Field.DBLength := LenInChar;

    Field.DataType := InternalType;
    Field.SubDataType := SubDataType;
    if Len > High(Word) {maximum TFieldDesc.Length} then
      Field.Length := High(Word)
    else
      Field.Length := Len;
    Field.Scale := Scale;
    Field.Fixed := Fixed;
    Field.FIsUnsigned := IsFlagSetted(pField.Flags, UNSIGNED_FLAG);
    Field.ReadOnly := {IsFlagSetted(FieldFlags, AUTO_INCREMENT_FLAG) or} (Field.FMySQLType = FIELD_TYPE_NULL){ or (Field.TableName = '') - error on modifying SELECT CAST() ... };
    Field.IsAutoIncrement := IsFlagSetted(pField.Flags, AUTO_INCREMENT_FLAG);
    Field.Required := not Field.IsAutoIncrement and (pField.MyType <> FIELD_TYPE_TIMESTAMP)
      and IsFlagSetted(pField.Flags, NOT_NULL_FLAG);
    Field.FIsBinary := IsFlagSetted(pField.Flags, BINARY_FLAG);
    Field.FIsKey := IsFlagSetted(pField.Flags, PRI_KEY_FLAG + UNIQUE_KEY_FLAG + MULTIPLE_KEY_FLAG + PART_KEY_FLAG);
    Field.FIsPrimaryKey := IsFlagSetted(pField.Flags, PRI_KEY_FLAG);
    if (ServerPrimaryVer > 5) or ((ServerPrimaryVer = 5) and (ServerMinorVer > 1)) or ((ServerPrimaryVer = 5) and (ServerMinorVer = 1) and (ServerReleaseVer >= 23)) then //https://bugs.mysql.com/bug.php?id=30081 - mysql bug
      Field.FIsCurrentTimestamp := IsFlagSetted(pField.Flags, TIMESTAMP_FLAG) and IsFlagSetted(pField.Flags, ON_UPDATE_NOW_FLAG)
    else
      Field.FIsCurrentTimestamp := IsFlagSetted(pField.Flags, TIMESTAMP_FLAG);
    if (Field.TableInfo <> nil) and Field.FIsCurrentTimestamp then
      TMyTableInfo(Field.TableInfo).FTimestampField := Field;

    Field.Size := GetBufferSize(Field.DataType, Field.Length);
  end;

(*procedure CorrectFieldDescsTableName;
  var
    i, j: integer;
    Field: TMySQLFieldDesc;
    IsAlias: boolean;
  begin
    for i := 0 to Fields.Count - 1 do begin
      Field := TMySQLFieldDesc(Fields[i]);
      if Field.FieldDescKind = fdkData then begin
        j := GetSQLObjectIndex(Field.TableInfo.TableName, IsAlias);
        if (j <> - 1) and IsAlias then begin// Field.TableName - is alias and we need to correct it
          Field.TableName := FTablesInfo[j].TableName;
          // Field.ActualName := Field.TableName + '.' + Field.Name;
        end;
      end;
    end;
  end;*)
var
  Field: TMySQLFieldDesc;
  pField: TMYSQL_FIELD;
  i, FieldCount: integer;
begin
  // Optimization
  if FCommand.FPrepared and (FFields.Count > 0) then
    Exit;

  // 'SHOW CHARSET'
  // 'SHOW ENGINES'
  // 'SHOW INNODB STATUS'
  // 'SHOW PRIVILEGES' {NonUnicode too - "Create temporary table"}
  // 'SHOW STATUS'
  // 'SHOW TABLE STATUS'
  // 'SHOW VARIABLES'
  SetLength(FNulls, 0);
  Parser.ToBegin;
  ShowStatement := Parser.ToLexem(lxSHOW) <> lcEnd;

  ExplainStatement4 := ServerPrimaryVer <= 4;
  if ExplainStatement4 then begin
    Parser.ToBegin;
    ExplainStatement4 := Parser.ToLexem(lxEXPLAIN) <> lcEnd;
    if not ExplainStatement4 then begin
      Parser.ToBegin;
      ExplainStatement4 := Parser.ToLexem(lxDESCRIBE) <> lcEnd;
    end;
  end;

  Assert(FCommand.FSQLResult <> nil); // Query does not return cursor
  FieldCount := FCommand.FConnection.FMySQLAPI.mysql_num_fields(FCommand.FSQLResult);

  Assert(FTablesInfo <> nil);
  FTablesInfo.BeginUpdate;
  try
    for i := 0 to FieldCount - 1 do begin
      pField := FCommand.FConnection.FMySQLAPI._mysql_fetch_field_direct(FCommand.FSQLResult, i, UseUnicode);

      Field := TMySQLFieldDesc(CreateFieldDesc);
      try
        Field.FieldNo := i + 1;
        Field.ActualFieldNo := i;
        DescribeFieldDesc(pField, Field);
        FFields.Add(Field);
      except
        Field.Free;
        raise;
      end;
    end;
  finally
    FTablesInfo.EndUpdate;
  end;

  FillTablesAliases(Parser);

  // Set correct values for FieldDesc.TableName and correct FieldDesc.ActualName
  // Example:
  //   for 'SELECT FieldName FROM TableName' FieldDesc['FieldName'].TableName = 'TableName'
  //   for 'SELECT FieldName FROM TableName TableAlias' FieldDesc['FieldName'].TableName = 'TableAlias' (incorrect)
  // CorrectFieldDescsTableName;
  // For
  //   SELECT FieldName FieldAlias FROM TableName TableAlias
  // must be
  //   Field.Name = 'FieldAlias'
  //   Field.ActualName = 'FieldAlias'
  //   Field.TableName = 'ALL_TYPES'
end;

procedure TMySQLRecordset.CreateFieldDescs;
var
  Parser: TMyParser;
  OldSQL: string;
  OldCachedSQL: string; // CR11914, 12021
begin
  Parser := TMyParser.Create(FCommand.SQL);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  Parser.Uppered := False; // !!! do not change TableName and TableAlias case

  try
    FIsCanFastClose := False;
    case FCommand.CommandType of
      ctUnknown: begin // This is a FieldDefs.Update call
        try
          OldSQL := FCommand.SQL;
          OldCachedSQL := FCommand.FCachedSQL.Text;
          if not FCommand.FStoredProc then
            FCommand.SetLimit0;

          Assert(FCommand.FSQLResult = nil);
          Assert(FCommand.FSQLResultNext = nil);
          FOpenNextState := osNotChecked;
          FCommand.Execute;

          if FCommand.FSQLResult = nil then begin // Search for first resultset
            FreeResult(True, True);
            FCommand.UseNextResult;
          end;

          if FCommand.FSQLResult <> nil then // This is SELECT statement (statement with fields)
            InternalCreateFieldDescs(Parser);

          Exit;
        finally
          DrainResults;

          if not Prepared then
            FCommand.FCursorState := csInactive;

          FCommand.SetProp(prDisableParamScan, True);
          try
            FCommand.SQL := OldSQL;
          finally
            FCommand.SetProp(prDisableParamScan, False);
          end;
          FCommand.FCachedSQL.Text := OldCachedSQL;
        end;
      end;
      ctCursor: begin
        InternalCreateFieldDescs(Parser);

        if FCommand.FConnectionSwap <> nil then begin
          Parser.ToBegin;
          if Parser.ToLexem(lxSELECT) <> lcEnd then
            FIsCanFastClose := Parser.ToLexem(lxSemicolon) = lcEnd;
        end;
      end;
    end;

    if FFields.Count = 0 then begin
      FreeResult(False, True);
      DatabaseError(SNotRows, nil);
    end;
  finally
    Parser.Free;
  end;
end;

function TMySQLRecordset.CanUseAllKeyFields: boolean;
begin
  Result := (FUpdatingTableInfoIdx < 0) or FFullRefresh;
end;

function TMySQLRecordset.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

function TMySQLRecordset.GetTimestampField: TCRFieldDesc;
begin
  if UpdatingTableInfo <> nil then
    Result := TMyTableInfo(UpdatingTableInfo).TimestampField
  else
    Result := nil;
end;

procedure TMySQLRecordset.DetectIdentityField;
var
  UpdTableInfo: TCRTableInfo;
  FieldDesc: TCRFieldDesc;
  PKCount, i: integer;
begin
  FIdentityField := nil;
  FIdentityIsPartOfComplexPK := False;
  UpdTableInfo := UpdatingTableInfo;
  if UpdTableInfo = nil then
    Exit;

  PKCount := 0;
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if TMySQLFieldDesc(FFields[i]).IsPrimaryKey then
      Inc(PKCount);
    if (FieldDesc.FieldDescKind = fdkData) and FieldDesc.IsAutoIncrement and (FieldDesc.TableInfo = UpdTableInfo) then
      FIdentityField := FieldDesc;
  end;
  FIdentityIsPartOfComplexPK := (FIdentityField <> nil) and (PKCount > 1);
end;

procedure TMySQLRecordset.DetectFieldType({$IFNDEF LITE}const FieldName: string;{$ENDIF} DBType: Word; DBLength, DBScale: Integer;
  const LengthInBytes: cardinal; const TableName: AnsiString;
  out InternalType, SubDataType: word; out Length, Scale: Integer; out Fixed: boolean);
{$IFNDEF LITE}
var
  FetchConverter: TFetchConverter;
{$ENDIF}
begin
  Length := 0;
  Scale := 0;
  SubDataType := dtUnknown;

{$IFNDEF LITE}
  FetchConverter := GetMapFetchConverter(FieldName, DBType, DBLength, DBScale);
  if FetchConverter <> nil then begin
    InternalType := FetchConverter.InternalDataType;
    if (DBType = myTime) and (InternalType = dtDateTime) then
      SubDataType := dtTime;
    Fixed := (DBType in [myBinary, myEnum, mySet, myChar]);
  end
  else
{$ENDIF}
  begin
    // Calc InternalType
    if FFieldsAsString and
      not (DBType in [myTinyBlob, myTinyText, myBlob, myText, myMediumBlob, myMediumText, myLongBlob, myLongText, myJSON])
    then
      InternalType := dtString
    else
    if FNumberFieldsAsString and
       (DBType in [myDecimal, myBit, myTinyUnsigned, myTiny, mySmallUnsigned, mySmall,
                   myMediumUnsigned, myMedium, myIntUnsigned, myInt, myBigintUnsigned, myBigint,
                   myFloat, myDouble,
                   myTimestamp, myDate, myTime, myDateTime, myYear])
    then
      InternalType := dtString
    else begin
      Assert(FCommand.FConnection <> nil);
      ConvertMySQLTypeToInternalFormat(DBType, LengthInBytes, DBScale, FEnableBoolean,
        OptimizedBigInt, FBinaryAsString, UseUnicode,
        FCommand.EnableBCD or FCommand.FConnection.EnableBCD,
        FCommand.EnableFMTBCD or FCommand.FConnection.EnableFMTBCD,
        FCommand.FConnection.FCheckPrecision, InternalType, Fixed);

      if DBType = myFloat then
        SubDataType := dtSingle;
    end;

    // Correct MySQL problem with IF and LONGMEMO and LONGBLOB fields   // CR-M20947
    if (DBType in [myVarchar, myVarbinary]) and (LengthInBytes = 65535)
      and __Strings65535ToMemo and (TableName = '') then
      if DBType = myVarbinary then
        InternalType := dtBlob
      else
        if UseUnicode then
          InternalType := dtWideMemo
        else
          InternalType := dtMemo;

    if not FLongStrings and (DBLength > 255)
      {$IFDEF FPC}or (DBLength = 0{StoredFunc result}){$ENDIF}
    then
      if InternalType = dtString then
        InternalType := dtMemo
      else
      if InternalType = dtWideString then
        InternalType := dtWideMemo;
  end;

  if Fixed then
    SubDataType := dtFixedChar;

  // FlatBuffers = False support
  if (not FFlatBuffers and (DBLength >= FlatBufferLimit))
    {$IFNDEF FPC}or (DBLength = 0{StoredFunc result}){$ENDIF}
  then
    case InternalType of
      dtString:
        InternalType := dtExtString;
      dtWideString:
        InternalType := dtExtWideString;
      dtBytes, dtVarBytes:
        InternalType := dtExtVarBytes;
    end;

  if not (InternalType in [dtBlob, dtMemo, dtWideMemo]) then // Length in symbols. For UTF8 Length(WideString) always smaller then Length(UTF8String)
    Length := DBLength;

  if (InternalType = dtString) and (FNumberFieldsAsString or FFieldsAsString) then
    case DBType  of
      myTiny, myTinyUnsigned:
        Length := 4;
      mySmall, mySmallUnsigned:
        Length := 6;
      myMedium, myMediumUnsigned:
        Length := 9;
      myInt, myIntUnsigned:
        Length := 11;
      myBigint, myBigintUnsigned:
        Length := 20;
      myFloat:
        Length := 24;
      myDouble:
        Length := 34;
      myDatetime, myTimestamp:
        Length := 19;
      myDate, myTime:
        Length := 10;
  end;

  case InternalType of
    dtSingle, dtFloat:
      Scale := DBScale;
    dtBCD:
      Scale := DBScale;
    dtFmtBCD: begin
      if Length > MaxFMTBcdFractionSize then
        Length := MaxFMTBcdFractionSize;
      Scale := DBScale;
    end;
    dtDateTime, dtDate, dtTime: begin
      if IsFractionSupported then
        Scale := DBScale;
    end;
    dtExtString, dtExtWideString: begin
      if (Length = 0) and (DBType <> myNull) then
        Length := SizeForFieldsWithZeroLength;
    end;
  end;
end;

procedure TMySQLRecordset.FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray;
  ForceUseAllKeyFields: boolean);

  procedure CheckPrimaryKeys;
  var
    HasPrimaryKeys: boolean;
    i, j: integer;
  begin
    HasPrimaryKeys := False;
    for i := Length(KeyFieldDescs) - 1 downto 0 do begin
      HasPrimaryKeys := TMySQLFieldDesc(KeyFieldDescs[i]).IsPrimaryKey;
      if HasPrimaryKeys then
        Break;
    end;

    if HasPrimaryKeys then begin
      j := 0;
      for i := 0 to Length(KeyFieldDescs) - 1 do begin
        if TMySQLFieldDesc(KeyFieldDescs[i]).IsPrimaryKey then begin
          if i <> j then
            KeyFieldDescs[j] := KeyFieldDescs[i];
          Inc(j);
        end;
      end;
      if Length(KeyFieldDescs) <> j then
        SetLength(KeyFieldDescs, j);
    end;
  end;

var
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  SetLength(KeyFieldDescs, 0);

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if FieldDesc.FieldDescKind <> fdkData then
      continue;

    if ForceUseAllKeyFields or (FieldDesc.TableInfo = UpdatingTableInfo) or
      ((FieldDesc.TableInfo = nil) and not FSetFieldsReadOnly) then
      if FieldDesc.IsKey then begin
        SetLength(KeyFieldDescs, Length(KeyFieldDescs) + 1);
        KeyFieldDescs[High(KeyFieldDescs)] := FieldDesc;
      end;
  end;

  CheckPrimaryKeys;
end;

procedure TMySQLRecordset.FillDataFieldDescs(out DataFieldDescs: TFieldDescArray;
  ForceUseAllKeyFields: boolean);
var
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  SetLength(DataFieldDescs, 0);

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if (FieldDesc.FieldDescKind = fdkData) and FieldDesc.Updateable and not FieldDesc.ReadOnly then
      if ForceUseAllKeyFields or (FieldDesc.TableInfo = UpdatingTableInfo) or
        ((FieldDesc.TableInfo = nil) and not FSetFieldsReadOnly) then begin
        SetLength(DataFieldDescs, Length(DataFieldDescs) + 1);
        DataFieldDescs[High(DataFieldDescs)] := FieldDesc;
      end;
  end;
end;


{$IFNDEF LITE}
function TMySQLRecordset.GetMapOnDemandConverter(const FieldName: string; DataType: Word; DBType: Word; DBLength, DBScale: Integer; out MapRule: TCRMapRule): TOnDemandConverter;
begin
  if (DBType = myMediumUnsigned) and (DataType = dtInt32) then
    DataType := dtUInt32;

  Result := inherited GetMapOnDemandConverter(FieldName, DataType, DBType, DBLength, DBScale, MapRule);
end;
{$ENDIF}

function TMySQLRecordset.GetCorrectedDataType(const FieldDesc: TFieldDesc): word;
begin
  Result := FieldDesc.DataType;
  Assert(FieldDesc is TMySQLFieldDesc);
  if (Result = dtFloat) and (
    (TMySQLFieldDesc(FieldDesc).MySQLType = FIELD_TYPE_DECIMAL) or
    (TMySQLFieldDesc(FieldDesc).MySQLType = FIELD_TYPE_NEWDECIMAL)
  ) and
  (ServerPrimaryVer >= 5) then
    Result := dtBCD;
end;

function TMySQLRecordset.IsFractionSupported: boolean;
begin
  Result := False;
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    Result := FCommand.FConnection.IsFractionSupported
end;

procedure TMySQLRecordset.IncFetchBlockOffset(var FetchBlockOffset: integer; FieldDesc: TFieldDesc);
{$IFOPT C+}
var
  OldFetchBlockOffset: integer;
{$ENDIF}
begin
{$IFOPT C+}
  OldFetchBlockOffset := FetchBlockOffset;
{$ENDIF}
  case FieldDesc.DataType of
    dtVarBytes, dtExtVarBytes, dtExtString:
      Inc(FetchBlockOffset, sizeof(cardinal) + FieldDesc.Length + 2);
    dtWideString, dtExtWideString:
      Inc(FetchBlockOffset, sizeof(cardinal) + (FieldDesc.Length + 1) * FCommand.FConnection.FDBCharLength);
    dtMemo, dtWideMemo, dtBlob:
      case TMySQLFieldDesc(FieldDesc).MySQLType of
        FIELD_TYPE_TINY_BLOB: // TINYBLOB, TINYTEXT
          Inc(FetchBlockOffset, $FF + sizeof(cardinal));
        FIELD_TYPE_GEOMETRY,
        FIELD_TYPE_MEDIUM_BLOB, // MEDIUMBLOB, MEDIUMTEXT
        FIELD_TYPE_LONG_BLOB, // LONGBLOB, LONGTEXT
        FIELD_TYPE_BLOB, // BLOB, TEXT
        FIELD_TYPE_JSON, // JSON
        FIELD_TYPE_VAR_STRING,
        FIELD_TYPE_STRING:
          Inc(FetchBlockOffset, sizeof(cardinal) + DefaultPieceSize);
      end;
    dtDate, dtTime, dtDateTime:
      Inc(FetchBlockOffset, sizeof(cardinal) + sizeof(MYSQL_TIME));
    dtSingle, dtFloat{corrected, see GetCorrectedDataType}, dtBCD, dtFMTBCD:
      Inc(FetchBlockOffset, sizeof(cardinal) + FieldDesc.Length + FieldDesc.Scale + 5);
    else
      Assert(False);
  end;
{$IFOPT C+}
  Assert(FetchBlockOffset - OldFetchBlockOffset > 4, IntToStr(FieldDesc.FieldNo) + ' ' + IntToStr(FieldDesc.DataType));
{$ENDIF}
end;

function TMySQLRecordset.IsNeedFetchBlock(const DataType: word): boolean; // Return True if field need to fetch into separate buffer
begin
  case DataType of
    dtExtString, dtExtWideString, dtWideString, dtBlob, dtMemo, dtWideMemo,
    dtVarBytes, dtExtVarBytes,
    dtBCD, dtFMTBCD,
    dtDate, dtTime, dtDateTime:
      Result := True;
    else
      Result := False;
  end;
end;

procedure TMySQLRecordset.AllocFetchBuffer; // Bind fields
  procedure FillBindingForField(FieldDesc: TFieldDesc; var pBnd: MYSQL_BIND);
  var
    InternalType: word;
    OldFetchBlockSize: integer;
  begin
    InternalType := GetCorrectedDataType(FieldDesc);
    pBnd.buffer_type := ConvertInternalTypeMySQLFormat(InternalType);
    pBnd.buffer := IntPtr(NativeInt(FFetchBufferSize));
    pBnd.length := nil;

    if IsNeedFetchBlock(InternalType) then begin
      OldFetchBlockSize := FFetchBufferSize;
      IncFetchBlockOffset(FFetchBufferSize, FieldDesc);
      pBnd.buffer_length := FFetchBufferSize - OldFetchBlockSize - sizeof(cardinal);
    end
    else
      case InternalType of
        dtInt8, dtBoolean:
          pBnd.buffer_length := sizeof(byte);
        dtInt16, dtUInt8:
          pBnd.buffer_length := sizeof(smallint);
        dtUInt16:
          pBnd.buffer_length := sizeof(word);
        dtInt32:
          pBnd.buffer_length := sizeof(integer);
        dtUInt32, dtInt64, dtUInt64:
          pBnd.buffer_length := sizeof(Int64);

        dtSingle, dtFloat:
          pBnd.buffer_length := sizeof(double);
        dtBCD, dtFMTBCD:
          pBnd.buffer_length := FieldDesc.Length + 1;

        dtBytes, dtVarBytes:
          pBnd.buffer_length := FieldDesc.Size;
        dtString:
          pBnd.buffer_length := FieldDesc.Size;
        dtExtString:
          pBnd.buffer_length := FieldDesc.Length + 1;
        dtWideString:
          pBnd.buffer_length := FieldDesc.Size * FCommand.FConnection.FDBCharLength;
        dtExtWideString:
          pBnd.buffer_length := (FieldDesc.Length + 1) * FCommand.FConnection.FDBCharLength;

        else
          Assert(False, Format('Invalid internal field type $%X (%d) for field %s', [InternalType, InternalType, FieldDesc.Name]));
      end;
  end;

var
  i: integer;
  FieldCnt: integer;
begin
  if Length(FNulls) > 0 then
    Exit;

  FieldCnt := FFields.Count;
  while (FieldCnt > 0) and (FFields[FieldCnt - 1].FieldDescKind = fdkCalculated) do
    Dec(FieldCnt);

  SetLength(FNulls, FieldCnt);
  SetLength(FValueLens, FieldCnt);

  // Unprepared or PreparedDirect
  if not Prepared or IsClass(FCommand.FConnection.FMySQLAPI, TMySQLAPIDirect) then
    Exit;

  // PreparedClient
{$IFNDEF LITE}
  Assert(FSmartFetchState = sfNone);
{$ENDIF}
  if FieldCnt <> 0 then begin
    SetLength(FFetchBnd, FieldCnt);
    FillChar(@FFetchBnd[0], FieldCnt * SizeOf(FFetchBnd[0]), 0);

    FFetchBufferSize := 0;
    for i := 0 to FieldCnt - 1 do
      FillBindingForField(FFields[i], FFetchBnd[i]);

    FFetchBuffer := Marshal.AllocHGlobal(FFetchBufferSize);

    for i := 0 to FieldCnt - 1 do
      if IsNeedFetchBlock(GetCorrectedDataType(FFields[i])) then begin
        FFetchBnd[i].length := PtrOffset(FFetchBuffer, NativeInt(FFetchBnd[i].buffer));
        FFetchBnd[i].buffer := PtrOffset(FFetchBuffer, NativeInt(NativeUInt(FFetchBnd[i].buffer) + sizeof(cardinal)));
      end;
  end;
end;

function TMySQLRecordset.NeedInitFieldsOnFetch: boolean;
var
  Field: TMySQLFieldDesc;
  pField: TMYSQL_FIELD;
  i, FieldCount: integer;
begin
  Result := inherited NeedInitFieldsOnFetch;

  if Prepared and not Result then begin
    FieldCount := FCommand.FConnection.FMySQLAPI.mysql_num_fields(FCommand.FSQLResult);
    for i := 0 to FFields.Count - 1 do begin
      Field := TMySQLFieldDesc(FFields[i]);
      if (Field.FieldDescKind <> fdkData) or (Field.ActualFieldNo = -1) {KeyOnly SmartFetchState} or (Field.FFieldNo > FieldCount) then
        continue;

      pField := FCommand.FConnection.FMySQLAPI._mysql_fetch_field_direct(FCommand.FSQLResult, Field.FFieldNo - 1, UseUnicode);
      if Field.FMySQLType <> pField.MyType then begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TMySQLRecordset.ReadParams;
var
  i: integer;
  Len: Word;
  Param: TParamDesc;
  RecBuf, p: IntPtr;
  Field: TFieldDesc;
  Value: Variant;
  Blob, ParamBlob: TBlob;
  Data: TBytes;
begin
  AllocRecBuf(RecBuf);
  try
    BlockMan.GetRecord(FirstItem, RecBuf);

    for i := 0 to FCommand.Params.Count - 1 do begin
      Param := FCommand.Params[i];
      if not IsLargeDataTypeUsed(Param) then
        case Param.GetParamType of
          pdResult: begin
            Field := FFields[0];
            GetFieldAsVariant(Field, RecBuf, Value);
            Param.Value := Value;
          end;
          pdInputOutput, pdOutput: begin
            Field := FindField('@' + Param.GetName);
            if (Field <> nil) and not IsLargeDataTypeUsed(Param) then begin
              GetFieldAsVariant(Field, RecBuf, Value);
              Param.Value := Value;
            end;
          end;
        end
      else
      if (Param.GetParamType in [pdResult, pdInputOutput, pdOutput]) then begin
        if Param.GetParamType = pdResult then
          Field := FFields[0]
        else
          Field := FindField('@' + Param.GetName);
        if Field <> nil then
          case Field.DataType of
            dtBlob, dtMemo, dtWideMemo: begin
              Blob := GetBlob(Field, RecBuf);
              ParamBlob := TBlob(Param.GetObject);
              if Blob.IsUnicode = ParamBlob.IsUnicode then
                ParamBlob.Assign(Blob)
              else
                if ParamBlob.IsUnicode then
                  ParamBlob.AsWideString := Blob.AsWideString
                else
                  ParamBlob.AsString := Blob.AsString;
              Param.SetNull(Blob.IsNull);
            end;
            dtExtString: begin
              Len := Marshal.ReadUInt16(RecBuf, Field.Offset);
              p := Marshal.ReadIntPtr(RecBuf, Field.DataOffset);
              if Param.GetDataType in [dtBlob, dtMemo, dtWideMemo] then begin
                ParamBlob := TBlob(Param.GetObject);
                ParamBlob.Clear;
                ParamBlob.Write(0, Len, p);
                Param.SetNull(ParamBlob.IsNull);
              end
              else
                Param.Value := Marshal.PtrToStringAnsi(p, Len);
            end;
            dtExtWideString: begin
              Len := Marshal.ReadUInt16(RecBuf, Field.Offset);
              p := Marshal.ReadIntPtr(RecBuf, Field.DataOffset);
              if Param.GetDataType in [dtBlob, dtMemo, dtWideMemo] then begin
                ParamBlob := TBlob(Param.GetObject);
                ParamBlob.Clear;
                ParamBlob.AsWideString := Marshal.PtrToStringUni(p, Len);
                Param.SetNull(ParamBlob.IsNull);
              end
              else
                Param.Value := Marshal.PtrToStringUni(p, Len);
            end;
            dtExtVarBytes: begin
              Len := Marshal.ReadUInt16(RecBuf, Field.Offset);
              p := Marshal.ReadIntPtr(RecBuf, Field.DataOffset);
              if Param.GetDataType in [dtBlob, dtMemo, dtWideMemo] then begin
                ParamBlob := TBlob(Param.GetObject);
                ParamBlob.Clear;
                ParamBlob.Write(0, Len, p);
                Param.SetNull(ParamBlob.IsNull);
              end
              else begin
                SetLength(Data, Len);
                Marshal.Copy(p, Data, 0, Len);
                Param.Value := Data;
              end;
            end;
          else
            raise Exception.Create(SDataTypeNotSupported);
          end;
      end;
    end;
  finally
    FreeRecBuf(RecBuf);
  end;

  FCommand.FCanReadParams := True;
  FCommand.FIsSelectParams := True;
  if Assigned(FCommand.ReadParams) then
    FCommand.ReadParams;
end;

function TMySQLRecordset.CreateBlob(Field: TFieldDesc): TBlob;
begin
{$IFDEF HAVE_COMPRESS}
  if FCommand.FCompressBlob <> cbNone then
    Result := TCompressedBlob.Create(Field.DataType = dtWideMemo)
  else
{$ENDIF}
    Result := TBlob.Create(Field.DataType = dtWideMemo);
  Result.RollbackEnabled := True;
end;

{$IFDEF HAVE_COMPRESS}
procedure TMySQLRecordset.SetCompressed(FieldNo: integer; Blob: TBlob);
begin
  if not (Blob is TCompressedBlob) then
    Exit;

  try
    case FCommand.FCompressBlob of
      cbNone, cbServer:
        TCompressedBlob(Blob).Compressed := False;
      cbClient, cbClientServer:
        TCompressedBlob(Blob).Compressed := True;
    end;
  except
    Blob.Clear;
    FNulls[FieldNo - 1] := Byte(True);
  end;
end;
{$ENDIF}

procedure TMySQLRecordset.ReadFieldValue(Field: TMySQLFieldDesc; pData: IntPtr; RecNo: integer; const row: PMYSQL_ROW);
var
  FieldMySQLType: TMySqlFieldType;
  ActualFieldNo: integer;
  ValueBuf: IntPtr;
  ValueLenPtr: PWord;

  function CalcLen(Len, FixedLen: integer): integer;
  begin
    if not TrimFixedChar and Field.Fixed then
      Result := FixedLen
    else
      Result := Len;
  end;

  procedure FillWideChar(X: IntPtr; Count: Integer; Value: Int16);
  var
    i: integer;
  begin
    i := 0;
    while i < Count do begin
      Marshal.WriteInt16(X, i, Value);
      i := i + 2;
    end;
  end;

  function GetAnsiFixedByteLength(Size: integer): Integer;
  begin
    if Field.Fixed then
      Result := Field.Length
    else
      Result := Size;
  end;

  function GetWideFixedByteLength(Size: integer): Integer;
  begin
    if Field.Fixed then
      Result := Field.Length * 2
    else
      Result := Size;
  end;

  // For RefreshQuick
  procedure SaveMaxTimestamp(FieldDesc: TMySQLFieldDesc; const Value: TDateTime);
  begin
    if (FieldDesc.TableInfo <> nil) and (FieldDesc = TMyTableInfo(FieldDesc.TableInfo).TimestampField) then
      if TMyTableInfo(FieldDesc.TableInfo).FMaxTimestamp < Value then
        TMyTableInfo(FieldDesc.TableInfo).FMaxTimestamp := Value;
  end;

  function ReadMaxPossibleLen: integer;
  begin
    Result := FValueLens[ActualFieldNo];
    if (Result > Field.Length) and (Field.Length > 0 {StoredFunc result}) and not UseUnicode then
      Result := Field.Length;
  end;

  function WordAt(const Value: TValueArr; Off: integer): word; {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Result := Byte(Value[Off]) - $30 {Ord('0')};
    Result := Result * 10 + (Byte(Value[Off + 1]) - $30);
  end;

  function IntAt(const Value: TValueArr; Off: integer): integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Result := Byte(Value[Off]) - $30 {Ord('0')};
    Result := Result * 10 + (Byte(Value[Off + 1]) - $30);
    Result := Result * 10 + (Byte(Value[Off + 2]) - $30);
    Result := Result * 10 + (Byte(Value[Off + 3]) - $30);
  end;

  function HourAt(const Value: TValueArr; var Off: integer): cardinal;
  begin
    if Byte(Value[Off]) = Byte('-') then
      Inc(Off);
    Result := 0;
    while Byte(Value[Off]) <> Byte(':') do begin
      if Result >= 1000 then
        DatabaseErrorFmt('Wrong time format at field %s', [Field.Name]);

      Result := Result * 10 + cardinal(Byte(Value[Off]) - $30 {Ord('0')});
      Inc(Off);
    end;
  end;

  function MSecAt(const Value: TValueArr; Off: integer; Decimals: integer): cardinal; {$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    i: integer;
  begin
    Result := 0;
    if Decimals > 0 then begin
      for i := 0 to Decimals - 1 do
        Result := Result * 10 + cardinal(Byte(Value[Off + i]) - $30 {Ord('0')});
      for i := Decimals to 5 do
        Result := Result * 10;
    end;
  end;

  // Must be sync with MyServices DateTimeFromStr!!!
  function ConvertToDateTime(var Res: TDateTime): boolean;
  var
    ValueArr: TValueArr;
    ValueArrOff, Len: integer;

    function Year2: integer;
    begin
      Result := WordAt(ValueArr, ValueArrOff);
      if Result >= 70 then
        Result := 1900 + Result
      else
        Result := 2000 + Result;
    end;

  const
    HoursPerDay = 24;

  var
    IsNegative: boolean;
    Hour: Cardinal;
    Fraction: Word;
  begin
    Result := True;
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);

    case FieldMySQLType of
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME: begin
        case Len of
          21..26: // YYYY-MM-DD HH:MM:SS.Z .. YYYY-MM-DD HH:MM:SS.ZZZZZZ
            Result := TryEncodeDateTime(
              IntAt(ValueArr, ValueArrOff), WordAt(ValueArr, ValueArrOff + 5), WordAt(ValueArr, ValueArrOff + 8),
              WordAt(ValueArr, ValueArrOff + 11), WordAt(ValueArr, ValueArrOff + 14), WordAt(ValueArr, ValueArrOff + 17),
              MSecAt(ValueArr, ValueArrOff + 20, Field.Scale) div 1000, Res);
          19: // YYYY-MM-DD HH:MM:SS
            Result := TryEncodeDateTime(
              IntAt(ValueArr, ValueArrOff), WordAt(ValueArr, ValueArrOff + 5), WordAt(ValueArr, ValueArrOff + 8),
              WordAt(ValueArr, ValueArrOff + 11), WordAt(ValueArr, ValueArrOff + 14), WordAt(ValueArr, ValueArrOff + 17),
              0, Res);
          14: // YYYYMMDDHHMMSS
            Result := TryEncodeDateTime(
              IntAt(ValueArr, ValueArrOff), WordAt(ValueArr, ValueArrOff + 4), WordAt(ValueArr, ValueArrOff + 6),
              WordAt(ValueArr, ValueArrOff + 8), WordAt(ValueArr, ValueArrOff + 10), WordAt(ValueArr, ValueArrOff + 12),
              0, Res);
          12: // YYMMDDHHMMSS
            Result := TryEncodeDateTime(
              Year2, WordAt(ValueArr, ValueArrOff + 2), WordAt(ValueArr, ValueArrOff + 4),
              WordAt(ValueArr, ValueArrOff + 6), WordAt(ValueArr, ValueArrOff + 8), WordAt(ValueArr, ValueArrOff + 10),
              0, Res);
          10: // YYMMDDHHMM
            Result := TryEncodeDateTime(
              Year2, WordAt(ValueArr, ValueArrOff + 2), WordAt(ValueArr, ValueArrOff + 4),
              WordAt(ValueArr, ValueArrOff + 6), WordAt(ValueArr, ValueArrOff + 8), 0,
              0, Res);
          8:  // YYYYMMDD
            Result := TryEncodeDate(
              IntAt(ValueArr, ValueArrOff), WordAt(ValueArr, ValueArrOff + 4), WordAt(ValueArr, ValueArrOff + 6), Res);
          6:  // YYMMDD
            Result := TryEncodeDate(Year2, WordAt(ValueArr, ValueArrOff + 2), WordAt(ValueArr, ValueArrOff + 4), Res);
          4:  // YYMM
            Result := TryEncodeDate(Year2, WordAt(ValueArr, ValueArrOff + 2), 1, Res);
          2:  // YY
            Result := TryEncodeDate(Year2, 1, 1, Res);
        else
          DatabaseErrorFmt('Invalid FIELD_TYPE_TIMESTAMP Field.Length (%d)', [Field.Length]);
        end;

        // For RefreshQuick
        if Field.IsCurrentTimestamp then
          SaveMaxTimestamp(Field, Res);
      end;
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE: begin // YYYY-MM-DD
        Result := TryEncodeDate(
          IntAt(ValueArr, ValueArrOff), WordAt(ValueArr, ValueArrOff + 5), WordAt(ValueArr, ValueArrOff + 8), Res);
      end;
      FIELD_TYPE_TIME: begin // HH:MM:SS
        IsNegative := Byte(ValueArr[ValueArrOff]) = Byte('-');
        if (Len <= 8) or (IsNegative and (Len = 9)) then
          Fraction := 0
        else
          Fraction := Field.Scale;
        Hour := HourAt(ValueArr, ValueArrOff); // don't replace - ValueArrOff is var parameter
        Res := (Hour * 3600000 + WordAt(ValueArr, ValueArrOff + 1) * 60000 + WordAt(ValueArr, ValueArrOff + 4) * 1000 +
          MSecAt(ValueArr, ValueArrOff + 7, Fraction) div 1000) / MSecsPerDay;
        if IsNegative then
          Res := - Res;
      end;
    end;
  end;


  function ConvertToDateTimePrep(var Res: TDateTime): boolean;
  var
    ValueArr: TValueArr;
    ValueArrOff, Len: integer;
    ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  begin
    Result := True;
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);

    case FieldMySQLType of
      FIELD_TYPE_TIME: begin
        if Len = 0 then
          Result := False
        else begin
          Assert((Len = 12) or (Len = 8), 'Error ConvertToDateTimePrep function Length = ' + IntToStr(Len));
          ADay := Marshal.ReadInt32(ValueArr, ValueArrOff + 1);
          AHour := Marshal.ReadByte(ValueArr, ValueArrOff + 5);
          AMinute := Marshal.ReadByte(ValueArr, ValueArrOff + 6);
          ASecond := Marshal.ReadByte(ValueArr, ValueArrOff + 7);
          if Len = 12 then
            AMilliSecond := Marshal.ReadInt32(ValueArr, ValueArrOff + 8) div 1000
          else
            AMilliSecond := 0;
          Result := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, Res);
          if Result then
            Res := Res + ADay;
        end;
      end;
      FIELD_TYPE_DATE, FIELD_TYPE_DATETIME, FIELD_TYPE_TIMESTAMP: begin
        case Len of
          4: Result := TryEncodeDate(
            Marshal.ReadInt16(ValueArr, ValueArrOff),
            Marshal.ReadByte(ValueArr, ValueArrOff + 2),
            Marshal.ReadByte(ValueArr, ValueArrOff + 3),
            Res);
          7: Result := TryEncodeDateTime(
            Marshal.ReadInt16(ValueArr, ValueArrOff),
            Marshal.ReadByte(ValueArr, ValueArrOff + 2),
            Marshal.ReadByte(ValueArr, ValueArrOff + 3),
            Marshal.ReadByte(ValueArr, ValueArrOff + 4),
            Marshal.ReadByte(ValueArr, ValueArrOff + 5),
            Marshal.ReadByte(ValueArr, ValueArrOff + 6),
            0,
            Res);
          11: Result := TryEncodeDateTime(
            Marshal.ReadInt16(ValueArr, ValueArrOff),
            Marshal.ReadByte(ValueArr, ValueArrOff + 2),
            Marshal.ReadByte(ValueArr, ValueArrOff + 3),
            Marshal.ReadByte(ValueArr, ValueArrOff + 4),
            Marshal.ReadByte(ValueArr, ValueArrOff + 5),
            Marshal.ReadByte(ValueArr, ValueArrOff + 6),
            Marshal.ReadInt32(ValueArr, ValueArrOff + 7) div 1000,
            Res);
          0: Result := False;
        else
          DatabaseErrorFmt('Invalid FIELD_TYPE_DATETIME Length = %d', [Len]);
        end;
        // For RefreshQuick
        if Field.IsCurrentTimestamp then
          SaveMaxTimestamp(Field, Res);
      end;
    end;
  end;

  procedure ConvertFromBit;
  var
    ValueArr: TValueArr;
    ValueArrOff, Len: integer;
    b: boolean;
    i64: Int64;
    i: integer;
  begin
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);

    if Field.DataType = dtBoolean then begin
      b := Marshal.ReadByte(ValueArr, ValueArrOff + Len - 1) <> 0;
      Marshal.WriteInt16(ValueBuf, smallint(WordBool(b)));
    end
    else begin
      i64 := 0;
      for i := 0 to Len - 1 do
        i64 := (i64 shl 8) + Marshal.ReadByte(ValueArr, ValueArrOff + i);

      case Field.Size of
        8: Marshal.WriteInt64(ValueBuf, i64);
        4: Marshal.WriteInt32(ValueBuf, integer(i64));
        2: Marshal.WriteInt16(ValueBuf, smallint(i64));
        1: Marshal.WriteByte(ValueBuf, byte(i64));
      end;
    end;
  end;

  procedure ConvertToInt;
  var
    ValueArr: TValueArr;
    ValueArrOff, Len: integer;

    function PCharToInt: integer;
    var
      ValueArrOffEnd: integer;
      Sign: boolean;
      i: integer;
      b: byte;
    begin
      ValueArrOffEnd := ValueArrOff + Len;
      Result := 0;

      Sign := Byte(ValueArr[ValueArrOff]) = Byte('-');
      if Sign then
        i := ValueArrOff + 1
      else
        i := ValueArrOff;
      while i < ValueArrOffEnd do begin
        b := Byte(ValueArr[i]) - $30 {Ord('0')};
        if Sign then
          Result := Result * 10 - b
        else
          Result := Result * 10 + b;
        Inc(i);
      end;
    end;

    function PCharToInt64: int64;
    var
      ValueArrOffEnd: integer;
      Sign: boolean;
      i: integer;
      b: byte;
    begin
      ValueArrOffEnd := ValueArrOff + Len;
      Result := 0;

      Sign := Byte(ValueArr[ValueArrOff]) = Byte('-');
      if Sign then
        i := ValueArrOff + 1
      else
        i := ValueArrOff;
      while i < ValueArrOffEnd do begin
      {$IFDEF OVERFLOWCHECKINGON}
        {$OVERFLOWCHECKS OFF}
      {$ENDIF}
        b := Byte(ValueArr[i]) - $30 {Ord('0')};
        Result := Result * 10 + b;
        Inc(i);
      {$IFDEF OVERFLOWCHECKINGON}
        {$OVERFLOWCHECKS ON}
      {$ENDIF}
      end;

      if Sign then
        Result := - Result;
    end;

  var
    i64: Int64;
  begin
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);

    case Field.DataType of
      dtBoolean:
        Marshal.WriteInt16(ValueBuf, SmallInt(WordBool(Boolean(PCharToInt)))); // Convert to boolean is useful to bypass Delphi bug
      else
      // Integer fields
        case Field.Size of
          8: begin
            // Do not change! (CB5 Internal Compiler Error: C1093)
            i64 := Int64(PCharToInt64);
            Marshal.WriteInt64(ValueBuf, i64);
          end;
          4: begin
            if Field.DataType = dtUInt32 then
              Marshal.WriteInt32(ValueBuf, Integer(PCharToInt64))
            else
              Marshal.WriteInt32(ValueBuf, PCharToInt);
          end;
          2: Marshal.WriteInt16(ValueBuf, SmallInt(PCharToInt));
          1: Marshal.WriteByte(ValueBuf, byte(PCharToInt));
        end;
    end;
  end;

  procedure ConvertToIntPrep;
  var
    ValueArr: TValueArr;
    ValueArrOff, Len: integer;

    function BinToInt64: int64;
    begin
      Result := 0;
      case Len of
        1: Result := ShortInt{Convert to signed}(Marshal.ReadByte(ValueArr, ValueArrOff));
        2: Result := Marshal.ReadInt16(ValueArr, ValueArrOff);
        4: Result := Marshal.ReadInt32(ValueArr, ValueArrOff);
        8: Result := Marshal.ReadInt64(ValueArr, ValueArrOff);
      else
        Assert(False);
      end;
    end;

    function BinToUInt32: cardinal;
    begin
      Result := 0;
      case Len of
        1: Result := Marshal.ReadByte(ValueArr, ValueArrOff);
        2: Result := word(Marshal.ReadInt16(ValueArr, ValueArrOff));
        4: Result := cardinal(Marshal.ReadInt32(ValueArr, ValueArrOff));
        8: Result := cardinal(Marshal.ReadInt64(ValueArr, ValueArrOff));
      else
        Assert(False);
      end;
    end;

  var
    i64: Int64;
  begin
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);

    case Field.DataType of
      // Signed Integer fields
      dtInt8, dtInt16, dtInt32, dtInt64: begin
        case Field.Size of
          8: begin
            // Do not change! (CB5 Internal Compiler Error: C1093)
            i64 := Int64(BinToInt64);
            Marshal.WriteInt64(ValueBuf, i64);
          end;
          4: Marshal.WriteInt32(ValueBuf, BinToInt64);
          2: Marshal.WriteInt16(ValueBuf, BinToInt64);
          1: Marshal.WriteByte(ValueBuf, byte(BinToInt64));
        end;
      end;
      dtBoolean:
        Marshal.WriteInt16(ValueBuf, SmallInt(WordBool(Boolean(BinToInt64)))); // Convert to boolean is useful to bypass Delphi bug
      // UnSigned Integer fields
      dtUInt8, dtUInt16, dtUInt32, dtUInt64: begin
        case Field.Size of
          8: begin
            i64 := Int64(BinToInt64);
            Marshal.WriteInt64(ValueBuf, i64);
          end;
          4: Marshal.WriteInt32(ValueBuf, Int32(BinToUInt32));
          2: Marshal.WriteInt16(ValueBuf, Int16(BinToUInt32));
          1: Marshal.WriteByte(ValueBuf, BinToUInt32);
        else
          Assert(False);
        end;
      end;
      else
        Assert(False);
    end;
  end;

  function ConvertToFloat: double;  // Nearly copied from TMySqlBind.GetDouble

    function Pow10(Value: double; Pow: integer): double;
    begin
    {$IFDEF VER16P}
      Result := Power10(Value, Pow);
    {$ELSE}
      Result := Value * IntPower(Base10, Pow)
    {$ENDIF}
    end;

    function GetIntAt(const buffer: TValueArr; off: integer; count: integer): Int32;
    var
      _end, i: integer;
      IsNegative: boolean;
    begin
      _end := off + count;
      if (byte(buffer[off]) = byte('-')) then begin
        IsNegative := True;
        Inc(off);
      end
      else begin
        IsNegative := False;
        if byte(buffer[off]) = byte('+') then
          Inc(off);
      end;

      Result := 0;
      for i := off to _end - 1 do begin
        if (byte(buffer[off]) < $30 {Ord('0')}) or (byte(buffer[off]) > $39 {Ord('9')}) then
          raise EConvertError.CreateFmt(SInvalidInteger, [Copy(buffer + off, 1, count)]);
        Result := Result * 10 + (byte(buffer[i]) - $30{Ord('0')});
      end;
      if IsNegative then
        Result := - Result;
    end;

  const
    PartLen = 18;
  var
    ValueArr: TValueArr;
    ValueArrOff, Len: integer;
    start, end1, end2, off: integer;
    point: integer;
    e: integer;
    IsNegative: boolean;
    Result1, Result2: int64;
    currVal: Byte;
  begin
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);
    start := ValueArrOff;

    if Len > PartLen then begin
      end1 := start + PartLen - 1;
      if Len > PartLen * 2 then
        end2 := start + PartLen * 2 - 1
      else
        end2 := start + Len - 1
    end
    else begin
      end1 := start + Len - 1;
      end2 := end1;
    end;

    if byte(ValueArr[start]) = byte('-') then begin
      IsNegative := True;
      Inc(start);
    end
    else begin
      IsNegative := False;
      if byte(ValueArr[start]) = byte('+') then
        Inc(start);
    end;

    point := -1;
    Result1 := 0;
    Result2 := 0;
    e := 0;

    for off := start to end1 do begin
      currVal := byte(ValueArr[off]);
      if (currVal < $30 {Ord('0')}) or (currVal > $39 {Ord('9')}) then begin
        if (point = -1) and (currVal = byte('.')) then
          point := off
        else
          if (currVal = byte('e')) or (currVal = byte('E')) then begin
            e := GetIntAt(ValueArr, off + 1, ValueArrOff + Len - off - 1); // getting mantissa
            end1 := off - 1;
            end2 := end1;
            break;
          end
          else
            raise EConvertError.CreateFmt(SInvalidFloat, [Copy(ValueArr + ValueArrOff, 1, Len)]);
      end
      else
        Result1 := Result1 * 10 + (currVal - $30 {Ord('0')});
    end;

    for off := end1 + 1 to end2 do begin
      currVal := byte(ValueArr[off]);
      if (currVal < $30 {Ord('0')}) or (currVal > $39 {Ord('9')}) then begin
        if (point = -1) and (currVal = byte('.')) then
          point := off
        else
          if (currVal = byte('e')) or (currVal = byte('E')) then begin
            e := GetIntAt(ValueArr, off + 1, ValueArrOff + Len - off - 1); // getting mantissa
            end2 := off - 1;
            break;
          end
          else
            raise EConvertError.CreateFmt(SInvalidFloat, [Copy(ValueArr + ValueArrOff, 1, Len)]);
      end
      else
        Result2 := Result2 * int64(10) + (currVal - $30 {Ord('0')});
    end;

    if end2 <> end1 then begin // long float (length is more than PartLen)
      if point = -1 then // without floating point
        Result := Pow10(Pow10(Result1, end2 - end1) + Result2, e)
      else
      if point <= end1 then
        Result := Pow10(Pow10(Result1, end2 - end1) + Result2, e + point - end2)
      else
        Result := Pow10(Pow10(Result1, end2 - end1 - 1) + Result2, e + point - end2)
    end
    else
      if point = -1 then
        Result := Pow10(Result1, e)
      else
        Result := Pow10(Result1, e + point - end2);

    if IsNegative then
      Result := - Result;
  end;

  function ConvertToFloatPrep: double;
  var
    ValueArr: TValueArr;
    ValueArrOff, Len: integer;
  begin
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);

    case Len of
      4: Result := Single((@ValueArr[ValueArrOff])^);
      8: Result := Double((@ValueArr[ValueArrOff])^);
    else
      Result := 0;
      Assert(False);
    end;
  end;

  procedure ConvertToBCD;
  var
    ValueArr: TValueArr;
    ValueArrOff, ValueArrOffEnd, Len: integer;
    c: Currency;
    i: integer;
  begin
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);
    ValueArrOffEnd := ValueArrOff + Len;

    if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then begin
      i := ValueArrOff;
      while i <> ValueArrOffEnd do begin
        if ValueArr[i] = '.' then begin
        {$IFDEF NEXTGEN}
          PByte(ValueArr)[i] := byte(FormatSettings.DecimalSeparator);
        {$ELSE}
          ValueArr[i] := AnsiChar({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        {$ENDIF}
          Break;
        end;
        Inc(i);
      end;
    end;

    c := StrToCurr(string(Copy(ValueArr + ValueArrOff, 1, Len)));
    PCurrency(ValueBuf)^ := c;
  end;

  procedure ConvertToFmtBCD;
  var
    ValueArr: TValueArr;
    ValueArrOff, ValueArrOffEnd, Len: integer;
    Bcd: TBcd;
    i: integer;
  begin
    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);
    ValueArrOffEnd := ValueArrOff + Len;

    if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then begin
      i := ValueArrOff;
      while i <> ValueArrOffEnd do begin
        if ValueArr[i] = '.' then begin
        {$IFDEF NEXTGEN}
          PByte(ValueArr)[i] := byte(FormatSettings.DecimalSeparator);
        {$ELSE}
          ValueArr[i] := AnsiChar({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        {$ENDIF}
          Break;
        end;
        Inc(i);
      end;
    end;

    Bcd := StrToBcd(string(Copy(ValueArr + ValueArrOff, 1, Len)));
    CRFunctions.NormalizeBcd(Bcd, PBcd(ValueBuf)^, Field.Length, Field.Scale);
  end;


  procedure ConvertToBlob;
  var
    Blob: TBlob;
    Len: integer;
    wsLen: integer;
    ValueArr: TValueArr;
    ValueArrOff: integer;
  begin
    Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(ValueBuf)));
    if Blob = nil then
      Blob := CreateBlob(Field);
    Blob.RollbackEnabled := False;

    Len := FValueLens[ActualFieldNo];
    ValueArr := FMySQLAPI._mysql_fetch_value_arr(row, ActualFieldNo, ValueArrOff, Len);

    if Len > 0 then begin // IsNull already tested
      if Blob.IsUnicode then begin
        wsLen := (Len + 1) * sizeof(WideChar);
        if wsLen > Length(FWsBuff) then
          SetLength(FWsBuff, wsLen);

      {$IFNDEF FPC}
        if not UseUnicode then
          Len := AnsiToWs(@FWsBuff[0], wsLen, @ValueArr[ValueArrOff], Len, False)
        else
      {$ENDIF}
          Len := Utf8ToWs(@FWsBuff[0], wsLen, @ValueArr[ValueArrOff], Len, False);
        Blob.Write(0, Len, @FWsBuff[0]);
      end
      else
        Blob.Write(0, Len, ValueArr + ValueArrOff);

    {$IFDEF HAVE_COMPRESS}
      SetCompressed(Field.FieldNo, Blob);
    {$ENDIF}
      Blob.RollbackEnabled := True;
    end
    else
      Blob.Write(0, 0, nil);


    Marshal.WriteIntPtr(ValueBuf, Blob.GCHandle);
  end;

  procedure ConvertToString(ExtString: boolean = False);
  var
    StringPtr: IntPtr;
    FixedLen, Len: integer;
  {$IFNDEF FPC}
    wsLen: integer;
  {$ENDIF}
  begin
    // For ExtString:
    // For some fields which were taken as results of string functions MySQL server returns
    // data length less than real data length
    // If FlatBuffers=False than the size of allocated memory is determined for each string
    if ExtString then begin
      Len := FValueLens[ActualFieldNo];
      StringPtr := nil;
    end
    else begin
      Len := ReadMaxPossibleLen;
      StringPtr := ValueBuf;
    end;
    FixedLen := GetAnsiFixedByteLength(Len);

 {$IFNDEF FPC}
   if UseUnicode then begin
      if Len + 1 > Length(FUtf8Buff) then
        SetLength(FUtf8Buff, Len + 1);
      if Len > 0 then begin
        FMySQLAPI._mysql_fetch_value_to_buff(row, ActualFieldNo, FUtf8Buff, 0, Len);
        wsLen := (Len + 1) * SizeOf(WideChar);
        if wsLen > Length(FWsBuff) then
          SetLength(FWsBuff, wsLen);
        wsLen := Utf8ToWs(@FWsBuff[0], wsLen, @FUtf8Buff[0], Len, False);
      end
      else
        wsLen := 0;

      if ExtString then begin
        StringPtr := FStringHeap.NewBuf(CalcLen(wsLen shr 1, FixedLen) + 1);
        Len := WsToAnsi(StringPtr, (wsLen shr 1) + 1, @FWsBuff[0], wsLen, False);
      end
      else
        WsToAnsi(StringPtr, Field.Length + 1, @FWsBuff[0], wsLen, True);
        ValueLenPtr^ := Word(Len);
    end
    else
 {$ENDIF}
      if ExtString then begin
        StringPtr := FStringHeap.NewBuf(CalcLen(Len, FixedLen) + 1);
        FMySQLAPI._mysql_fetch_value_to_buff(row, ActualFieldNo, StringPtr, Len);
      end
      else
        FMySQLAPI._mysql_fetch_value_to_str(row, ActualFieldNo, StringPtr, Len);

    if not TrimFixedChar and Field.Fixed and (Len < FixedLen) then begin
      FillChar(PtrOffset(StringPtr, Len), FixedLen - Len, $20);
      Marshal.WriteByte(StringPtr, FixedLen, 0);
        ValueLenPtr^ := Word(FixedLen);
    end
    else begin
      if ExtString then begin
        Marshal.WriteByte(StringPtr, Len, 0);
          ValueLenPtr^ := Word(FValueLens[ActualFieldNo]);
      end
      else
          ValueLenPtr^ := Word(Len);
    end;

    if ExtString then
      Marshal.WriteIntPtr(ValueBuf, StringPtr);
  end;

  procedure ConvertToWideString;
  var
    FixedLen, Len: integer;
  begin
    Len := ReadMaxPossibleLen;
    FixedLen := GetWideFixedByteLength(Len);
    if Len + 1 > Length(FUtf8Buff) then
      SetLength(FUtf8Buff, Len + 1);
    if Len > 0 then
      FMySQLAPI._mysql_fetch_value_to_buff(row, ActualFieldNo, FUtf8Buff, 0, Len);

  {$IFNDEF FPC}
    if not UseUnicode then
      Len := AnsiToWs(ValueBuf, (Field.Length + 1) * SizeOf(WideChar), @FUtf8Buff[0], Len, True)
    else
  {$ENDIF}
      Len := Utf8ToWs(ValueBuf, (Field.Length + 1) * SizeOf(WideChar), @FUtf8Buff[0], Len, True);

    if not TrimFixedChar and Field.Fixed and (Len < FixedLen + SizeOf(WideChar)) then begin
      FillWideChar(PtrOffset(ValueBuf, Len - 2), (FixedLen - Len) +  SizeOf(WideChar) , $0020);
      Marshal.WriteInt16(ValueBuf, FixedLen, 0);
        ValueLenPtr^ := Word(FixedLen shr 1);
    end
    else begin
        ValueLenPtr^ := Word((Len - sizeof(WideChar)) shr 1){#0};
    end;
  end;

  procedure ConvertToExtWideString;
  var
    ExtStringPtr: IntPtr;
    FixedLen, Len, wsLen: integer;
  begin
    Len := FValueLens[ActualFieldNo];
    FixedLen := GetWideFixedByteLength(Len);
    if Len + 1 > Length(FUtf8Buff) then
      SetLength(FUtf8Buff, Len + 1);
    if Len > 0 then
      FMySQLAPI._mysql_fetch_value_to_buff(row, ActualFieldNo, FUtf8Buff, 0, Len);

    wsLen := (Len + 1) * sizeof(WideChar);
    if wsLen > Length(FWsBuff) then
      SetLength(FWsBuff, wsLen);
  {$IFNDEF FPC}
    if not UseUnicode then
      Len := AnsiToWs(@FWsBuff[0], wsLen, @FUtf8Buff[0], Len, True)
    else
  {$ENDIF}
      Len := Utf8ToWs(@FWsBuff[0], wsLen, @FUtf8Buff[0], Len, True);
    ExtStringPtr := FStringHeap.NewBuf(CalcLen(Len, FixedLen) + SizeOf(WideChar));
    Marshal.Copy(FWsBuff, 0, ExtStringPtr, Len);

    if not TrimFixedChar and Field.Fixed and (Len < FixedLen + SizeOf(WideChar)) then begin
      FillWideChar(PtrOffset(ExtStringPtr, Len - 2), (FixedLen - Len) + SizeOf(WideChar), $0020);
      Marshal.WriteInt16(ExtStringPtr, FixedLen, 0);
        ValueLenPtr^ := Word(FixedLen shr 1);
    end
    else begin
        ValueLenPtr^ := Word((Len - sizeof(WideChar)) shr 1){#0};
    end;

    Marshal.WriteIntPtr(ValueBuf, ExtStringPtr);
  end;

  procedure ConvertToBytes;
  var
    Len: integer;
  begin
    Len := ReadMaxPossibleLen;
      ValueLenPtr^ := Word(Len);
    FMySQLAPI._mysql_fetch_value_to_buff(row, ActualFieldNo, ValueBuf, Len);
  end;

  procedure ConvertToVarBytes;
  var
    Len: integer;
  begin
    Len := ReadMaxPossibleLen;
      ValueLenPtr^ := Word(Len);
    FMySQLAPI._mysql_fetch_value_to_buff(row, ActualFieldNo, ValueBuf, Len);
  end;

  procedure ConvertToExtVarBytes;
  var
    Len: integer;
    HeapBuf: IntPtr;
  begin
    Len := FValueLens[ActualFieldNo];
      ValueLenPtr^ := Word(Len);
    HeapBuf := FStringHeap.NewBuf(Len + sizeof(word));
    FMySQLAPI._mysql_fetch_value_to_buff(row, ActualFieldNo, HeapBuf, Len);
    Marshal.WriteIntPtr(ValueBuf, HeapBuf);
  end;

var
  dt: TDateTime;
  b: boolean;
  d: double;
  s: single;
  Blob: TBlob;
begin
  ActualFieldNo := Field.ActualFieldNo;
  FNulls[Field.FieldNo - 1] := Byte(FMySQLAPI._mysql_fetch_value_is_null(row, ActualFieldNo));
  FieldMySQLType := Field.MySQLType;

    ValueBuf := PtrOffset(pData, Field.DataOffset);
    if Field.HasValueLen then
      ValueLenPtr := PtrOffset(pData, Field.Offset)
    else
      ValueLenPtr := nil;

  if not boolean(FNulls[Field.FieldNo - 1]) then begin
    case Field.DataType of
      dtBoolean,
      // Signed Integer fields
      dtInt8, dtInt16, dtInt32, dtInt64,
      // UnSigned Integer fields
      dtUInt8, dtUInt16, dtUInt32, dtUInt64:
        if FieldMySQLType = FIELD_TYPE_BIT then
          ConvertFromBit
        else
        if Prepared then
          ConvertToIntPrep
        else
          ConvertToInt;

      // Float fields
      dtSingle: begin
        if Prepared and (FieldMySQLType in [FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE]) then
          s := ConvertToFloatPrep
        else
          s := ConvertToFloat;
        Marshal.WriteInt32(ValueBuf, CRBitConverter.SingleToInt32Bits(s));
      end;
      dtFloat: begin
        if Prepared and (FieldMySQLType in [FIELD_TYPE_FLOAT, FIELD_TYPE_DOUBLE]) then
          d := ConvertToFloatPrep
        else
          d := ConvertToFloat;
        Marshal.WriteInt64(ValueBuf, BitConverter.DoubleToInt64Bits(d));
      end;

      dtBCD:
        ConvertToBCD;
      dtFmtBCD:
        ConvertToFmtBCD;
      dtDateTime, dtDate, dtTime: begin
        if Prepared and (FieldMySQLType <> FIELD_TYPE_NEWDATE) then
          b := ConvertToDateTimePrep(dt)
        else
          b := ConvertToDateTime(dt);
        if b then begin
          if NullForZeroDelphiDate and (Double(dt) = 0) then
            FNulls[Field.FieldNo - 1] := Byte(True) // SetNull(i + 1, pRec, True)
          else
            Marshal.WriteInt64(ValueBuf, BitConverter.DoubleToInt64Bits(dt)); // PDateTime(pValue)^ := dt
        end
        else
          if FNullForZeroDate then
            FNulls[Field.FieldNo - 1] := Byte(True) // SetNull(i + 1, pRec, True)
          else
            Marshal.WriteInt64(ValueBuf, BitConverter.DoubleToInt64Bits(ZeroDate)); // PDateTime(pValue)^ := ZeroDate
      end;

      dtString:
        ConvertToString;
      dtExtString: begin
          ConvertToString(True)
      end;
      dtWideString:
        ConvertToWideString;
      dtExtWideString: begin
          ConvertToExtWideString;
      end;
      dtBlob, dtMemo, dtWideMemo:
        ConvertToBlob;
      dtBytes:
        ConvertToBytes;
      dtVarBytes:
          ConvertToVarBytes;
      dtExtVarBytes: begin
          ConvertToExtVarBytes;
      end;

      else
        Assert(False);
    end;
  end
  else begin
    case Field.DataType of
      dtBlob, dtMemo, dtWideMemo: begin
        if Marshal.ReadIntPtr(ValueBuf) = nil then begin
          Blob := CreateBlob(Field);
          Marshal.WriteIntPtr(ValueBuf, Blob.GCHandle);
        end;
      end;
      dtExtString, dtExtWideString, dtExtVarBytes:
        Marshal.WriteIntPtr(ValueBuf, nil);
    end;
  end;
end;

procedure TMySQLRecordset.GetDataFromRow(pData: IntPtr; RecNo: integer; const row: PMYSQL_ROW);
var
  Field: TMySQLFieldDesc;
  i: integer;
begin
  if Prepared then
    FMySQLAPI._mysql_stmt_fetch_lengths(row, FValueLens)
  else
    FMySQLAPI._mysql_fetch_lengths(FCommand.FSQLResult, FValueLens);

  for i := 0 to FFields.Count - 1 do begin
    Field := TMySQLFieldDesc(FFields[i]);
    if (Field.FieldDescKind <> fdkData) or (Field.ActualFieldNo = -1) {KeyOnly SmartFetchState} then
      continue;

    ReadFieldValue(Field, pData, RecNo, row);
  end;

  Marshal.Copy(FNulls, 0, PtrOffset(pData, DataSize), Length(FNulls));
end;

procedure TMySQLRecordset.GetDataFromRowPrepClient(const pData: IntPtr);
var
  CorrectDecimalSeparator: boolean;
  Field: TFieldDesc;
  pValue, pFetchBlockValue, pFetchBlockLen: IntPtr;
  pValueLen: PWord;

  procedure SetZeroDate(Field: TFieldDesc);
  begin
    if FNullForZeroDate then
      SetNull(Field, pData, True)
    else
      Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(ZeroDate)); // PDateTime(pValue)^ := ZeroDate
  end;

  procedure ConvertToBCD(DataType: word);
  var
    ValueArr: TValueArr;
    Len: integer;

    d: double;
    s: single;
    c: Currency;
    i: integer;
  begin
    Len := Marshal.ReadInt32(pFetchBlockLen);
    ValueArr := pFetchBlockValue;

    if CorrectDecimalSeparator then begin
      i := 0;
      while i <> Len do begin
        if ValueArr[i] = '.' then begin
        {$IFDEF NEXTGEN}
          PByte(ValueArr)[i] := byte(FormatSettings.DecimalSeparator);
        {$ELSE}
          ValueArr[i] := AnsiChar({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        {$ENDIF}
          Break;
        end;
        Inc(i);
      end;
    end;

    case DataType of
      dtSingle: begin
        s := StrToFloat(string(Copy(ValueArr, 1, Len)));
        PSingle(pValue)^ := s;
      end;
      dtFloat: begin
        d := StrToFloat(string(Copy(ValueArr, 1, Len)));
        PDouble(pValue)^ := d;
      end;
      dtBCD: begin
        c := StrToCurr(string(Copy(ValueArr, 1, Len)));
        PCurrency(pValue)^ := c;
      end;
      else
        Assert(False);
    end;
  end;

  procedure ConvertToFmtBCD;
  var
    ValueArr: TValueArr;
    Len: integer;
    Bcd: TBcd;
    i: integer;
  begin
    Len := Marshal.ReadInt32(pFetchBlockLen);
    ValueArr := pFetchBlockValue;

    if CorrectDecimalSeparator then begin
      i := 0;
      while i <> Len do begin
        if ValueArr[i] = '.' then begin
        {$IFDEF NEXTGEN}
          PByte(ValueArr)[i] := byte(FormatSettings.DecimalSeparator);
        {$ELSE}
          ValueArr[i] := AnsiChar({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        {$ENDIF}
          Break;
        end;
        Inc(i);
      end;
    end;

    Bcd := StrToBcd(string(Copy(ValueArr, 1, Len)));
    CRFunctions.NormalizeBcd(Bcd, PBcd(pValue)^, Field.Length, Field.Scale);
  end;

var
  FieldIdx: integer;
  FetchBlockOffset: integer;
  Blob: TBlob;
  pc: IntPtr;
  Len: cardinal;
  DataSize: Integer;
  WsBuffSize: integer;
  p: cardinal;
  dt: TDateTime;
  mdt: PMYSQL_TIME;
  si: SmallInt;
  DataType: word;

begin
  CorrectDecimalSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.'; // For performance reason

  FetchBlockOffset := 0;
  for FieldIdx := 0 to FFields.Count - 1 do begin
    Field := FFields[FieldIdx];
    if (Field.FieldDescKind <> fdkData) or (Field.ActualFieldNo = -1) {KeyOnly SmartFetchState} then
      continue;

    pValue := PtrOffset(pData, Field.DataOffset);
    if Field.HasValueLen then
      pValueLen := PtrOffset(pData, Field.Offset)
    else
      pValueLen := nil;

    DataType := Field.DataType; // not corrected!
    if IsNeedFetchBlock(GetCorrectedDataType(Field)) then begin
      pFetchBlockLen := PtrOffset(FFetchBuffer, FetchBlockOffset);
      pFetchBlockValue := PtrOffset(pFetchBlockLen, sizeof(cardinal));

      case DataType of
        dtString: begin
          if GetNull(Field, pData) then
            pValueLen^ := 0;
        end;
        dtWideString: begin
          if GetNull(Field, pData) then
            pValueLen^ := 0
          else begin
            DataSize := Marshal.ReadInt32(pFetchBlockLen);
            DataSize := Utf8ToWs(pValue, Field.Size, pFetchBlockValue, DataSize, True);
            DataSize := (DataSize shr 1) - 1;
            pValueLen^ := Word(DataSize);
          end;
        end;
        dtExtString: begin
          if GetNull(Field, pData) then begin
            pValueLen^ := 0;
            Marshal.WriteIntPtr(pValue, nil);
          end
          else begin
            DataSize := Marshal.ReadInt32(pFetchBlockLen);
            if TrimFixedChar then
              PIntPtr(pValue)^ := FStringHeap.AllocTrimmedStr(pFetchBlockValue, DataSize)
            else
              PIntPtr(pValue)^ := FStringHeap.AllocStr(pFetchBlockValue, DataSize);
            pValueLen^ := Word(DataSize);
          end;
        end;
        dtExtWideString: begin
          if GetNull(Field, pData) then begin
            pValueLen^ := 0;
            Marshal.WriteIntPtr(pValue, nil);
          end
          else begin
            DataSize := Marshal.ReadInt32(pFetchBlockLen);
            WsBuffSize := (DataSize + 1) * sizeof(WideChar);
            if WsBuffSize > Length(FWsBuff) then
              SetLength(FWsBuff, WsBuffSize);
            DataSize := Utf8ToWs(@FWsBuff[0], WsBuffSize, pFetchBlockValue, DataSize, True);
            DataSize := (DataSize shr 1) - 1;
            if TrimFixedChar then
              PIntPtr(pValue)^ := FStringHeap.AllocTrimmedWideStr(@FWsBuff[1], DataSize)
            else
              PIntPtr(pValue)^ := FStringHeap.AllocWideStr(@FWsBuff[1], DataSize);
            pValueLen^ := Word(DataSize);
          end;
        end;
        dtVarBytes: begin
          if GetNull(Field, pData) then
            pValueLen^ := 0
          else begin
            pValueLen^ := Word(Marshal.ReadInt32(pFetchBlockLen));
            CopyBuffer(pFetchBlockValue, pValue, pValueLen^);
          end;
        end;
        dtExtVarBytes: begin
          if GetNull(Field, pData) then begin
            pValueLen^ := 0;
            Marshal.WriteIntPtr(pValue, nil)
          end
          else begin
            pValueLen^ := Word(Marshal.ReadInt32(pFetchBlockLen));
            PIntPtr(pValue)^ := FStringHeap.NewBuf(pValueLen^);
            CopyBuffer(pFetchBlockValue, PIntPtr(pValue)^, pValueLen^);
          end;
        end;
        dtMemo, dtWideMemo, dtBlob: begin
          Blob := CreateBlob(Field);
          if not GetNull(Field, pData) then begin
            Len := cardinal(Marshal.ReadInt32(pFetchBlockLen));
            if Len > 0 then begin
              Blob.RollbackEnabled := False;
              pc := pFetchBlockValue;
              p := 0;

              while Len > 0 do begin
                if Blob.IsUnicode and (Integer(Len) > Length(FUtf8Buff)) then
                  SetLength(FUtf8Buff, Len);

                if Len > cardinal(DefaultPieceSize) then
                  DataSize := DefaultPieceSize
                else
                  DataSize := Len;

                if Blob.IsUnicode then
                  CopyBuffer(pc, @FUtf8Buff[p], DataSize)
                else
                  Blob.Write(p, DataSize, pc);
                Inc(p, DataSize);
                Dec(Len, DataSize);

                if Len > 0 then
                  FCommand.FConnection.FMySQLAPI._mysql_stmt_fetch_column(FCommand.Fstmt, FFetchBnd, Field.ActualFieldNo, p);
              end;

              if Blob.IsUnicode then begin
                Len := Length(FUtf8Buff);
                WsBuffSize := (Len + 1) * sizeof(WideChar);
                if WsBuffSize > Length(FWsBuff) then
                  SetLength(FWsBuff, WsBuffSize);

                DataSize := Utf8ToWs(@FWsBuff[0], WsBuffSize, @FUtf8Buff[0], Len, False);
                Blob.Write(0, DataSize, @FWsBuff[0]);
              end;

            {$IFDEF HAVE_COMPRESS}
              SetCompressed(Field.FieldNo, Blob);
            {$ENDIF}
              Blob.RollbackEnabled := True;
            end;
          end;
          Marshal.WriteIntPtr(pValue, Blob.GCHandle);
        end;
        dtDate, dtTime, dtDateTime:
          if not GetNull(Field, pData) then begin
            mdt := pFetchBlockValue;
            case mdt.time_type of
              MYSQL_TIMESTAMP_NONE,
              MYSQL_TIMESTAMP_ERROR:
                SetZeroDate(Field);
              MYSQL_TIMESTAMP_DATE: begin
                dt := EncodeDate(mdt.year, mdt.month, mdt.day);
                if NullForZeroDelphiDate and (Double(dt) = 0) then
                  SetNull(Field, pData, True)
                else
                  Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(dt));
              end;
              MYSQL_TIMESTAMP_DATETIME: begin
                if TryEncodeDateTime(mdt.year, mdt.month, mdt.day, mdt.hour, mdt.minute, mdt.second, mdt.second_part, dt) then begin
                  if NullForZeroDelphiDate and (Double(dt) = 0) then
                    SetNull(Field, pData, True)
                  else
                    Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(dt));
                end
                else
                  SetZeroDate(Field);
              end;
              MYSQL_TIMESTAMP_TIME: begin
                dt := EncodeTime(mdt.hour, mdt.minute, mdt.second, mdt.second_part);
                Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(dt));
              end;
            end;
          end;
        dtSingle, dtFloat, dtBCD:
          if not GetNull(Field, pData) then
            ConvertToBCD(DataType);
        dtFMTBCD:
          if not GetNull(Field, pData) then
            ConvertToFmtBcd;
        else
          Assert(False);
      end;
      IncFetchBlockOffset(FetchBlockOffset, Field);
      Assert(FetchBlockOffset <= FFetchBufferSize);
    end
    else begin
      case DataType of
        dtBoolean: begin
          si := Marshal.ReadByte(pValue);
          Marshal.WriteInt16(pValue, SmallInt(WordBool(Boolean(si)))); // Convert to boolean is useful to bypass Delphi bug
        end;
        dtInt8: begin
          si := ShortInt(Marshal.ReadByte(pValue));
          Marshal.WriteInt16(pValue, si); // Convert to boolean is useful to bypass Delphi bug
        end;
      end;
    end;
  end;
end;

procedure TMySQLRecordset.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);
var
  pRec: IntPtr;
  row: PMYSQL_ROW;
  res: Integer;
  r: boolean;
  Direct: boolean;
  FieldDesc: TFieldDesc;
  i, j: integer;
begin

  RowsObtained := 0;
  try
    FMySQLAPI := FCommand.FConnection.FMySQLAPI; // For performance reason
    Direct := IsClass(FMySQLAPI, TMySQLAPIDirect);

    pRec := PtrOffset(Block, sizeof(TBlockHeader) + sizeof(TItemHeader));
    for i := 0 to FFetchRows - 1 do begin
      if not Prepared then begin
        row := FMySQLAPI.mysql_fetch_row(FCommand.FSQLResult);
        r := row <> nil;
        if not r then
          Check;
      end
      else begin
        // Preparing BlockMan block
        for j := 0 to Length(FFetchBnd) - 1 do begin
          FieldDesc := FFields[j];
          FFetchBnd[j].is_null := PtrOffset(pRec, DataSize + FieldDesc.FieldNo - 1);
          if not IsNeedFetchBlock(GetCorrectedDataType(FieldDesc)) then begin
            FFetchBnd[j].buffer := PtrOffset(pRec, FieldDesc.Offset);
            FFetchBnd[j].length := nil;
          end;
        end;

        FCommand.FConnection.CheckStmt(FCommand.Fstmt, FMySQLAPI._mysql_stmt_bind_result(FCommand.Fstmt, FFetchBnd), Component);
        res := FMySQLAPI._mysql_stmt_fetch(FCommand.Fstmt, row);
        r := res <> MYSQL_NO_DATA;
        if r and (res <> MYSQL_DATA_TRUNCATED) then
          FCommand.CheckStmt(res);
      end;

      if r then begin
        if Direct or not Prepared then
          GetDataFromRow(pRec, i, row)
        else
          GetDataFromRowPrepClient(pRec);

        Inc(RowsObtained);
      end
      else begin
          FreeResult(not Prepared {nonsense on prepared statements}, True);

        FCommand.SetCursorState(csFetched);
        Exit;
      end;

      pRec := PtrOffset(pRec, RecordSize + sizeof(TItemHeader));
    end;

  {$IFNDEF LITE}
    if FSmartFetchState = sfDataByKey then begin
      FreeResult(not Prepared {nonsense on prepared statements}, True);
      FCommand.SetCursorState(csFetched);
    end;
  {$ENDIF}
  except
    FreeResult(False {nonsense on error processing}, False);
    raise;
  end;
end;

procedure TMySQLRecordset.ProcessFetchedBlock(Block: PBlockHeader; FetchBack: boolean);
begin
    if FCommand.FStoredProc and (FRecordCount = 1) and (FCommand.Params.Count > 0) then
      ReadParams;
end;


procedure TMySQLRecordset.SetToEnd;
begin
  FetchAll;
  inherited;
end;

procedure TMySQLRecordset.BreakFetch;
begin
  inherited;

  if (FCommand <> nil) and
    ((FCommand.GetCursorState = csFetching) or (FCommand.GetCursorState = csFetchingAll)) then
    FCommand.SetCursorState(csFetched);
end;

function TMySQLRecordset.GetSQLObjectIndex(const ObjName: string; out IsAlias: boolean): integer; // MySQL specific - Search by name and by alias!!!
var
  TableName: string;
  DataBase, Name: string;
begin
  Result := - 1;
  if FCommand.FConnection = nil then
    Exit;

  MySQLInfo.SplitObjectName(ObjName, DataBase, Name);
  Name := MySQLInfo.NormalizeName(Name, False, True);
  DataBase := MySQLInfo.NormalizeName(DataBase, False, True);
  TableName := GenerateTableName(DataBase, Name, FCommand.FConnection.FDatabase);

  Result := FTablesInfo.IndexByName(TableName);
  if Result <> - 1 then
    IsAlias := False;

  if Result = - 1 then begin
    Result := FTablesInfo.IndexByAlias(TableName);
    if Result <> - 1 then
      IsAlias := True;
  end;
end;

function TMySQLRecordset.GetSQLObjectIndex(const ObjName: string): integer; // MySQL specific - Search by name and by alias!!!
var
  IsAlias: boolean;
begin
  Result := GetSQLObjectIndex(ObjName, IsAlias);
end;

function TMySQLRecordset.AddSQLObjectIfNeed(const SQLObjName: string; const IsView: boolean): integer; // Add new SQLObject, if need. If SQLObject already present then return its index
var
  TableInfo: TMyTableInfo;
  SQLObjectName: string;
begin
  Assert(FTablesInfo <> nil);
  SQLObjectName := MySQLInfo.NormalizeName(SQLObjName);
  Result := GetSQLObjectIndex(SQLObjectName);
  if Result = - 1 then begin
    TableInfo := FTablesInfo.Add as TMyTableInfo;
    Result := FTablesInfo.IndexOf(TableInfo);
    TableInfo.TableName := SQLObjectName;
    TableInfo.IsView := IsView;
  end;
end;

function TMySQLRecordset.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prBinaryAsString:
      Value := FBinaryAsString;
    prCreateConnection:
      Value := FCreateConnection;
    prCommandTimeout:
      Value := FCommand.FCommandTimeout;
    prEnableBoolean:
      Value := FEnableBoolean;
    prOpenNext:
      Value := FCommand.FOpenNext;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TMySQLRecordset.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prFetchAll:
      if not FLockFetchAll then begin
        FFetchAll := Value and not FUniDirectional;
        if FCommand <> nil then
          FCommand.FUseCreateConnection := not FFetchAll;
      end;
    prBinaryAsString:
      FBinaryAsString := Value;
    prCreateConnection: begin
      FCreateConnection := Value;
      if FCommand <> nil then
        FCommand.FCreateConnection := FCreateConnection;
    end;
    prCommandTimeout: begin
      Assert(FCommand <> nil);
      FCommand.FCommandTimeout := Value;
    end;
    prEnableBoolean:
      FEnableBoolean := Value;
    prFieldsAsString:
      FFieldsAsString := Value;
    prNumberFieldsAsString:
      FNumberFieldsAsString := Value;
    prNullForZeroDate:
      FNullForZeroDate := Value;
    prOpenNext:
      FCommand.FOpenNext := Value;
    prUseHandler:
      FUseHandler := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMySQLRecordset.IsCaseSensitive: boolean;
begin
  Result := False;
end;

{ TMySQLTransaction }

constructor TMySQLTransaction.Create;
begin
  inherited Create;
end;

destructor TMySQLTransaction.Destroy;
begin
  inherited;
end;

function TMySQLTransaction.GetConnection: TMySQLConnection;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  if FConnections.Count > 1 then
    raise Exception.Create(SMultiConnectionsInTransaction);

  Result := FConnections[0] as TMySQLConnection;
  if (Result = nil) or not Result.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);
end;

procedure TMySQLTransaction.StartTransaction;
var
  Connection: TMySQLConnection;
  s: string;
begin
  if FReadOnly then
    raise Exception.Create(SReadOnlyTransactionNotSupported);

  CheckInactive;
  Connection := GetConnection;

  // default isolation level is REPEATABLE READ
  // but default can be changed by MySQL server configuration or executing SET TRANSACTION
  case FIsolationLevel of
    ilReadCommitted:
      s := 'SET TRANSACTION ISOLATION LEVEL READ COMMITTED';
    ilReadUnCommitted:
      s := 'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED';
    ilRepeatableRead:
      s := 'SET TRANSACTION ISOLATION LEVEL REPEATABLE READ';
    ilIsolated:
      s := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;
  if Connection.GetIsServer41 then  // multistatement execution is supported since MySQL 4.1
    Connection.ExecuteSQL(s + '; BEGIN')
  else begin
    Connection.ExecuteSQL(s);
    Connection.ExecuteSQL('BEGIN');
  end;
  FActive := True;
  FNativeTransaction := True;
end;

procedure TMySQLTransaction.Commit;
begin
  CheckActive;
  if FNativeTransaction then
    GetConnection.ExecuteSQL('COMMIT');
  FActive := False;
end;

procedure TMySQLTransaction.Rollback;
begin
  CheckActive;
  if FNativeTransaction then
    GetConnection.ExecuteSQL('ROLLBACK');
  FActive := False;
end;

procedure TMySQLTransaction.Savepoint(const Name: string);
begin
  CheckActive;
  GetConnection.ExecuteSQL('SAVEPOINT ' + Name);
end;

procedure TMySQLTransaction.ReleaseSavepoint(const Name: string);
begin
  CheckActive;
  GetConnection.ExecuteSQL('RELEASE SAVEPOINT ' + Name);
end;

procedure TMySQLTransaction.RollbackToSavepoint(const Name: string);
begin
  CheckActive;
  GetConnection.ExecuteSQL('ROLLBACK TO SAVEPOINT ' + Name);
end;

{$IFNDEF LITE}
{ TMySQLMetaData }

function TMySQLMetaData.CreateRecordSet: TCRRecordSet;
begin
  FRecordSet := TMySQLRecordSet.Create;
  FRecordSet.TrimFixedChar := True;

  Result := FRecordSet;
end;

function TMySQLMetaData.GetTypesForSQL(const ObjectTypes: string): string;
var
  i: integer;
  TypesList: TStringList;
begin
  TypesList := TStringList.Create;
  try
    ParseTypes(ObjectTypes, TypesList);
    Result := '';
    for i := 0 to TypesList.Count - 1 do begin
      if TypesList[i] <> '' then begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result +  AnsiQuotedStr(TypesList[i], '''');
      end;
    end;
  finally
    TypesList.Free;
  end;
end;

function TMySQLMetaData.GetTables(Restrictions: TStrings): TData;
const
  fmtGetTablesSQL = 'SELECT ' +
    'cast('''' as char(1)) TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE ' +
    'FROM information_schema.TABLES ' +
    '%s ORDER BY TABLE_SCHEMA, TABLE_NAME COLLATE utf8_unicode_ci ';
var
  SQL, WhereClause, Schema, TableName, TableTypes, QuotedTypes, Scope: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);
  Scope :=  AnsiUpperCase(Trim(Restrictions.Values['SCOPE']));

  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    if FRecordSet.FCommand.FConnection.FDatabase = '' then begin
      CreateTablesFields;
      FMemData.Open;
    end
    else begin
      SQL := 'SHOW TABLES';
      if Schema <> '' then
        SQL := SQL + ' FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(Schema);
      FRecordSet.Close;
      FRecordSet.SetSQL(SQL);
      FRecordSet.Open;

      CreateTablesFields;
      FMemData.Open;
      CopyTablesData(Restrictions);
      FRecordSet.Close;
      FMemData.SetToBegin;
    end;
    Result := FMemData;
  end
  else begin
    WhereClause := '';
    if Scope <> 'LOCAL' then
      AddWhere(WhereClause, 'TABLE_SCHEMA', Schema)
    else
      WhereClause := 'TABLE_SCHEMA = DATABASE()';
    AddWhere(WhereClause, 'TABLE_NAME', TableName);

    QuotedTypes := GetTypesForSQL(TableTypes);
    if QuotedTypes <> '' then begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      QuotedTypes := StringReplace(QuotedTypes, '''TABLE''', '''BASE TABLE''', [rfReplaceAll]);
      WhereClause := WhereClause + 'TABLE_TYPE IN (' + QuotedTypes + ')';
    end;

    if WhereClause <> '' then
      WhereClause := 'WHERE ' + WhereClause;

    FRecordSet.Close;
    FRecordSet.SetSQL(Format(fmtGetTablesSQL, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
  end;
end;

procedure TMySQLMetaData.CopyTablesData(Restrictions: TStrings);
const
  snTABLE_NAME    = 1;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
var
  Schema: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  if Schema <> '' then
    Schema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FRecordSet.FCommand.FConnection.FDatabase;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME], [dnTABLE_NAME]);
    FMemDataHelper.FieldValues[dnSCHEMA] := Schema;
    FMemDataHelper.AppendRecord;
  end;
end;

function TMySQLMetaData.GetColumns(Restrictions: TStrings): TData;
const
  fmtGetColumnsSQL = 'SELECT ' +
    'cast('''' as char(1)) TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, ' +
    'ORDINAL_POSITION POSITION, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH DATA_LENGTH, ' +
    'NUMERIC_PRECISION DATA_PRECISION, NUMERIC_SCALE DATA_SCALE, ' +
    '(case IS_NULLABLE when ''YES'' then 1 else 0 end) NULLABLE, COLUMN_DEFAULT DEFAULT_VALUE ' +
    'FROM information_schema.COLUMNS ' +
    '%s ORDER BY TABLE_SCHEMA, TABLE_NAME, ORDINAL_POSITION ';

  fmtGetColumnsSQL56 = 'SELECT ' +
    'cast('''' as char(1)) TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, ' +
    'ORDINAL_POSITION POSITION, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH DATA_LENGTH, ' +
    'IF(DATETIME_PRECISION IS NULL, NUMERIC_PRECISION, DATETIME_PRECISION) DATA_PRECISION, ' +
    'NUMERIC_SCALE DATA_SCALE, ' +
    '(case IS_NULLABLE when ''YES'' then 1 else 0 end) NULLABLE, COLUMN_DEFAULT DEFAULT_VALUE ' +
    'FROM information_schema.COLUMNS ' +
    '%s ORDER BY TABLE_SCHEMA, TABLE_NAME, ORDINAL_POSITION ';

var
  SQL, WhereClause, Schema, TableName, ColumnName, Scope: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);
  Scope := Trim(Restrictions.Values['SCOPE']);

  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    if TableName = '' then begin
      raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);
    end
    else begin
      SQL := 'SHOW COLUMNS FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(TableName);
      if Schema <> '' then
        SQL := SQL + ' FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(Schema);
      FRecordSet.Close;
      FRecordSet.SetSQL(SQL);
      FRecordSet.Open;

      CreateColumnsFields;
      FMemData.Open;
      CopyColumnsData(Restrictions);
      FRecordSet.Close;
      FMemData.SetToBegin;
    end;
    Result := FMemData;
  end
  else begin
    WhereClause := '';
    if Scope <> 'LOCAL' then
      AddWhere(WhereClause, 'TABLE_SCHEMA', Schema)
    else
      WhereClause := 'TABLE_SCHEMA = DATABASE()';
    AddWhere(WhereClause, 'TABLE_NAME', TableName);
    AddWhere(WhereClause, 'COLUMN_NAME', ColumnName);
    if WhereClause <> '' then
      WhereClause := 'WHERE ' + WhereClause;

    FRecordSet.Close;
    if (FRecordSet.FCommand.FConnection.ServerPrimaryVer = 5) and (FRecordSet.FCommand.FConnection.ServerMinorVer >= 6) then
      FRecordSet.SetSQL(Format(fmtGetColumnsSQL56, [WhereClause]))
    else
      FRecordSet.SetSQL(Format(fmtGetColumnsSQL, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
  end;
end;

procedure TMySQLMetaData.CopyColumnsData(Restrictions: TStrings);
const
  snCOLUMN_NAME   = 1;
  snDATA_TYPE     = 2;
  snNULLABLE      = 3;
  snDEFAULT       = 5;

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

var
  Schema, TableName: string;
  NullableStr: string;
  FieldNo, Nullable: integer;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  if Schema <> '' then
    Schema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FRecordSet.FCommand.FConnection.FDatabase;
  TableName := FRecordSet.FCommand.SQLInfo.NormalizeName(TableName, False, True);

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  FieldNo := 1;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCOLUMN_NAME, snDATA_TYPE, snDEFAULT],
      [dnCOLUMN_NAME, dnDATA_TYPE, dnDEFAULT]);

    FMemDataHelper.FieldValues[dnSCHEMA] := Schema;
    FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
    FMemDataHelper.FieldValues[dnPOSITION] := FieldNo;
    NullableStr := VarToStr(FRecordSetHelper.FieldValues[snNULLABLE]);
    if (NullableStr = 'Y') or (NullableStr = 'YES') then
      Nullable := 1
    else
      Nullable := 0;
    FMemDataHelper.FieldValues[dnNULLABLE] := Nullable;

    FMemDataHelper.AppendRecord;
    Inc(FieldNo);
  end;
end;

function TMySQLMetaData.GetProcedures(Restrictions: TStrings): TData;
const
  fmtGetProceduresSQL = 'SELECT ' +
    'cast('''' as char(1)) PROCEDURE_CATALOG, ROUTINE_SCHEMA PROCEDURE_SCHEMA, ' +
    'ROUTINE_NAME PROCEDURE_NAME, ROUTINE_TYPE PROCEDURE_TYPE ' +
    'FROM information_schema.ROUTINES ' +
    '%s ORDER BY ROUTINE_SCHEMA, ROUTINE_NAME ';
var
  WhereClause, Schema, ProcName, ProcTypes, QuotedTypes, Scope: string;
begin
  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    CreateProceduresFields;
    FMemData.Open;
    Result := FMemData;
    exit;
  end;

  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ProcTypes := Trim(Restrictions.Values['PROCEDURE_TYPE']);
  Scope :=  AnsiUpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'ROUTINE_SCHEMA', Schema)
  else
    WhereClause := 'ROUTINE_SCHEMA = DATABASE()';
  AddWhere(WhereClause, 'ROUTINE_NAME', ProcName);

  QuotedTypes := GetTypesForSQL(ProcTypes);
  if QuotedTypes <> '' then begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'ROUTINE_TYPE IN (' + QuotedTypes + ')';
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.Close;
  FRecordSet.SetSQL(Format(fmtGetProceduresSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TMySQLMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
var
  Schema, ProcName: string;
  IsFunc: boolean;
  ParamList, ReturnParam: string;
  ParamInfos: TParamInfos;
  i, pos: integer;
  UQSchema, UQProcName: string;
  Direction: string;

const
  dnCATALOG        = 1;
  dnSCHEMA         = 2;
  dnPROC_NAME      = 3;
  dnPARAM_NAME     = 4;
  dnPOSITION       = 5;
  dnDIRECTION      = 6;
  dnDATA_TYPE      = 7;
  dnDATA_LENGTH    = 8;
  dnDATA_PRECISION = 9;
  dnDATA_SCALE     = 10;

begin
  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    CreateProcedureParametersFields;
    FMemData.Open;
    Result := FMemData;
    exit;
  end;

  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);

  if ProcName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['PROCEDURE_NAME']);

  // TODO: Schema is not supported by GetSPParams
  FRecordSet.FCommand.GetSPParams(ProcName, IsFunc, ParamList, ReturnParam);

  ParamInfos := nil;
  if IsFunc then
    FRecordSet.FCommand.DescribeParams(ReturnParam, True, ParamInfos, nil, TMySQLCommand(FRecordSet.FCommand).FEnableBoolean);
  FRecordSet.FCommand.DescribeParams(ParamList, False, ParamInfos, nil, TMySQLCommand(FRecordSet.FCommand).FEnableBoolean);

  CreateProcedureParametersFields;
  FMemData.Open;

  if Schema <> '' then
    UQSchema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    UQSchema := FRecordSet.FCommand.FConnection.FDatabase;
  UQProcName := FRecordSet.FCommand.SQLInfo.NormalizeName(ProcName, False, True);

  FMemDataHelper.AllocBuffer;
  for i := 0 to High(ParamInfos) do begin
    FMemDataHelper.InitRecord;

    FMemDataHelper.FieldValues[dnSCHEMA] := UQSchema;
    FMemDataHelper.FieldValues[dnPROC_NAME] := UQProcName;
    FMemDataHelper.FieldValues[dnPARAM_NAME] := ParamInfos[i].Name;
    pos := i;
    if not IsFunc then
      Inc(pos);
    FMemDataHelper.FieldValues[dnPOSITION] := pos;

    case ParamInfos[i].Direction of
      pdInput:
        Direction := 'IN';
      pdOutput, pdResult:
        Direction := 'OUT';
      pdInputOutput:
        Direction := 'IN/OUT';
    end;
    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;
    FMemDataHelper.FieldValues[dnDATA_TYPE] := ParamInfos[i].ParamType;
    if ParamInfos[i].Size >= 0 then
      FMemDataHelper.FieldValues[dnDATA_LENGTH] := ParamInfos[i].Size;

    FMemDataHelper.AppendRecord;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TMySQLMetaData.GetIndex(Restrictions: TStrings; Columns: boolean): TData;
var
  TableName, TableSchema, SQL: string;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);

  if TableName = '' then begin
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);
  end
  else begin
    SQL := 'SHOW INDEX FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(TableName);
    if TableSchema <> '' then
      SQL := SQL + ' FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(TableSchema);
    FRecordSet.Close;
    FRecordSet.SetSQL(SQL);
    FRecordSet.Open;

    if Columns then
      CreateIndexColumnsFields
    else
      CreateIndexesFields;
    FMemData.Open;
    if Columns then
      CopyIndexColumnsData(Restrictions)
    else
      CopyIndexesData(Restrictions);
    FRecordSet.Close;
    FMemData.SetToBegin;
  end;
  Result := FMemData;
end;

function TMySQLMetaData.GetIndexes(Restrictions: TStrings): TData;
begin
  Result := GetIndex(Restrictions, False);
end;

procedure TMySQLMetaData.CopyIndexesData(Restrictions: TStrings);
const
  snTABLE_NAME    = 1;
  snNON_UNIQUE    = 2;
  snINDEX_NAME    = 3;

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
var
  Schema, OldIndexName, IndexName: string;
  Unique: integer;
  v: Variant;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  if Schema <> '' then
    Schema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FRecordSet.FCommand.FConnection.FDatabase;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  OldIndexName := '';
  while FRecordSetHelper.NextRecord do begin
    IndexName := VarToStr(FRecordSetHelper.FieldValues[snINDEX_NAME]);
    if IndexName = OldIndexName then
      continue;
    OldIndexName := IndexName;

    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_NAME, snINDEX_NAME],
      [dnTABLE_NAME, dnINDEX_NAME]);

    FMemDataHelper.FieldValues[dnTABLE_SCHEMA] := Schema;
    FMemDataHelper.FieldValues[dnINDEX_SCHEMA] := Schema;

    v := FRecordSetHelper.FieldValues[snNON_UNIQUE];
    if v = 0 then
      Unique := 1
    else
      Unique := 0;
    FMemDataHelper.FieldValues[dnUNIQUE] := Unique;

    FMemDataHelper.AppendRecord;
  end;
end;

function TMySQLMetaData.GetIndexColumns(Restrictions: TStrings): TData;
begin
  Result := GetIndex(Restrictions, True);
end;

procedure TMySQLMetaData.CopyIndexColumnsData(Restrictions: TStrings);
const
  snTABLE_NAME      = 1;
  snINDEX_NAME      = 3;
  snPOSITION        = 4;
  snCOLUMN_NAME     = 5;
  snCOLLATION       = 6;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnINDEX_CATALOG   = 4;
  dnINDEX_SCHEMA    = 5;
  dnINDEX_NAME      = 6;
  dnCOLUMN_NAME     = 7;
  dnPOSITION        = 8;
  dnSORT_ORDER      = 9;
var
  Schema: string;
  SortOrder: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  if Schema <> '' then
    Schema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FRecordSet.FCommand.FConnection.FDatabase;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_NAME, snINDEX_NAME, snCOLUMN_NAME, snPOSITION],
      [dnTABLE_NAME, dnINDEX_NAME, dnCOLUMN_NAME, dnPOSITION]);

    FMemDataHelper.FieldValues[dnTABLE_SCHEMA] := Schema;
    FMemDataHelper.FieldValues[dnINDEX_SCHEMA] := Schema;

    SortOrder := VarToStr(FRecordSetHelper.FieldValues[snCOLLATION]);
    if SortOrder = 'A' then
      SortOrder := 'ASC'
    else
    if SortOrder = 'D' then
      SortOrder := 'DESC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := SortOrder;

    FMemDataHelper.AppendRecord;
  end;
end;

function TMySQLMetaData.GetConstraints(Restrictions: TStrings): TData;
const
  fmtGetConstraintsSQL = 'SELECT ' +
    'cast('''' as char(1)) TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, CONSTRAINT_NAME, ' +
    'CONSTRAINT_TYPE, cast('''' as char(1)) INDEX_CATALOG, ' +
    'cast('''' as char(1)) INDEX_OWNER, cast('''' as char(1)) INDEX_NAME ' +
    'FROM information_schema.table_constraints ' +
    '%s ORDER BY TABLE_SCHEMA, TABLE_NAME, CONSTRAINT_NAME';
var
  WhereClause, Schema, TableName, ConstraintName, Types, QuotedTypes: string;
begin
  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    CreateConstraintsFields;
    FMemData.Open;
    Result := FMemData;
    exit;
  end;

  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := Trim(Restrictions.Values['CONSTRAINT_TYPE']);

  WhereClause := '';
  AddWhere(WhereClause, 'TABLE_SCHEMA', Schema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'CONSTRAINT_NAME', ConstraintName);

  QuotedTypes := GetTypesForSQL(Types);
  if QuotedTypes <> '' then begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'CONSTRAINT_TYPE IN (' + QuotedTypes + ')';
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.Close;
  FRecordSet.SetSQL(Format(fmtGetConstraintsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TMySQLMetaData.GetConstraintColumns(Restrictions: TStrings): TData;
const
  fmtGetConstraintColumnsSQL = 'SELECT ' +
    'cast('''' as char(1)) TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, CONSTRAINT_NAME, ' +
    'COLUMN_NAME, ORDINAL_POSITION AS COLUMN_POSITION ' +
    'FROM information_schema.KEY_COLUMN_USAGE ' +
    '%s ORDER BY TABLE_SCHEMA, TABLE_NAME, CONSTRAINT_NAME';
var
  WhereClause, Schema, TableName, ConstraintName, ColumnName: string;
begin
  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    CreateConstraintColumnsFields;
    FMemData.Open;
    Result := FMemData;
    exit;
  end;

  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'TABLE_SCHEMA', Schema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'CONSTRAINT_NAME', ConstraintName);
  AddWhere(WhereClause, 'COLUMN_NAME', ColumnName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.Close;
  FRecordSet.SetSQL(Format(fmtGetConstraintColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TMySQLMetaData.GetDatabases(Restrictions: TStrings): TData;
begin
  FRecordSet.Close;
  FRecordSet.SetSQL('SHOW DATABASES');
  FRecordSet.Open;

  CreateDatabasesFields;
  FMemData.Open;
  CopyDatabasesData(Restrictions);
  FRecordSet.Close;
  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TMySQLMetaData.CopyDatabasesData(Restrictions: TStrings);
const
  snDB_NAME = 1;
  dnDB_NAME = 1;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snDB_NAME], [dnDB_NAME]);
    FMemDataHelper.AppendRecord;
  end;
end;
{$ENDIF}

{ TMySQLLoader }

constructor TMySQLLoader.Create;
begin
  inherited;

  FBuffer := AnsiStringBuilder.Create(16384 {default net_buffer_length value} * 10);
  FRowBuffer := AnsiStringBuilder.Create(1000);
  //FCommandTimeout := 30;
end;

destructor TMySQLLoader.Destroy;
begin
  FRowBuffer.Free;
  FBuffer.Free;

  inherited;
end;

function TMySQLLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prLock:
      FLock := Value;
    prDelayed:
      FDelayed := Value;
    prRowsPerQuery:
      FRowsPerQuery := Value;
    prDuplicateKeys:
      FDuplicateKeys := _TMyDuplicateKeys(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMySQLLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prLock:
      Value := FLock;
    prDelayed:
      Value := FDelayed;
    prRowsPerQuery:
      Value := FRowsPerQuery;
    prDuplicateKeys:
      Value := Variant(FDuplicateKeys);
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TMySQLLoader.Clear;
var
  s: AnsiString;
begin
  FBuffer.Length := 0;
  if FConnection.FIsUtf8 then
    s := CRFunctions.Utf8Encode(WideString(FInsHeader))
  else
    s := AnsiString(FInsHeader);
  FInsHeaderLen := LengthA(s);
  FBuffer.Append(s);
end;

procedure TMySQLLoader.Reset;
begin
  FInsHeader := '';
  FInsHeaderLen := 0;
  Clear;
  inherited;
end;

procedure TMySQLLoader.Prepare;
var
  ColumnList: string;
  i: integer;
begin
  inherited;

  ColumnList := '';
  for i := 0 to Columns.Count - 1 do begin
    if i > 0 then
      ColumnList := ColumnList + ', ';
    if QuoteNames then
      ColumnList := ColumnList + MySQLInfo.Quote(Columns[i].Name)
    else
      ColumnList := ColumnList + MySQLInfo.QuoteIfNeed(Columns[i].Name);
  end;

  case FDuplicateKeys of
    _dkNone:
      if FDelayed then
        FInsHeader := 'INSERT DELAYED INTO '
      else
        FInsHeader := 'INSERT INTO ';
    _dkIgnore:
      if FDelayed then
        FInsHeader := 'INSERT DELAYED IGNORE INTO '
      else
        FInsHeader := 'INSERT IGNORE INTO ';
    _dkReplace:
      if FDelayed then
        FInsHeader := 'REPLACE DELAYED INTO '
      else
        FInsHeader := 'REPLACE INTO ';
  end;
  FInsHeader := FInsHeader + FTableName + ' (' + ColumnList + ') VALUES ';
  Clear;

  SetLength(FValues, Columns.Count);

  if FLock then
    ExecSQL(AnsiString('LOCK TABLES ' + FTableName + ' WRITE'));
end;

procedure TMySQLLoader.Flush;
begin
  if (FBuffer.Length = 0) or (FBuffer.Length = FInsHeaderLen) then
    Exit;

  ExecSQL(FBuffer.ToString);

  Clear;
end;

procedure TMySQLLoader.ExecSQL(const SQL: AnsiString);
var
  ConnectionTimeout: integer;
  v: variant;
  SQLResult: PMYSQL_RES;
begin
  FConnection.GetProp(prConnectionTimeout, v);
  ConnectionTimeout := v;
  //Connection.ConnectionTimeout := CommandTimeout;
  FConnection.MySQLAPI.SetTimeout(FConnection.MySQL, ConnectionTimeout);
  FConnection.Check(FConnection.MySQLAPI.mysql_real_query(FConnection.MySQL, PAnsiChar(SQL), LengthA(SQL)), nil);
  SQLResult := FConnection.MySQLAPI.mysql_use_result(FConnection.MySQL);
  if SQLResult <> nil then
    FConnection.MySQLAPI.mysql_free_result(SQLResult);
end;

procedure TMySQLLoader.CalculateRowBuffer;
var
  i: integer;
begin
  FRowBuffer.Length := 0;
  for i := 0 to Length(FValues) - 1 do begin
    if i <> 0 then
      FRowBuffer.Append(', ');

    FConnection.AppendValueToSQL(FRowBuffer, Columns[i].DataType, FValues[i],
      VarIsEmpty(FValues[i]) or VarIsNull(FValues[i]), FConnection.FUseUnicode {$IFDEF HAVE_COMPRESS}, False{$ENDIF});
    DoReleaseObjectRef(FValues[i]);
    if FUseBlankValues then
      FValues[i] := Null;
  end;
end;

procedure TMySQLLoader.AppendRowBufferToBuffer;
begin
  if FBuffer.Length <> FInsHeaderLen then
    FBuffer.Append(', ');

  FBuffer.Append('(');
  FBuffer.Append(FRowBuffer);
  FBuffer.Append(')');
  Inc(FLoadedRows);
end;

function TMySQLLoader.IsPutColumnDataAllowed: boolean;
begin
  Result := FInsHeader <> '';
end;

procedure TMySQLLoader.PutColumnData(Col, Row: integer; const Value: variant);
begin
  if (FLastRow <> -1) and (Row > FLastRow + 1) then begin
    if Row <> FLastRow + 2 then
      raise Exception.Create('Invalid row number');

    if FRowsPerQuery > 0 then begin
      CalculateRowBuffer;
      AppendRowBufferToBuffer;
      if (Row - 1) mod FRowsPerQuery = 0 then
        Flush;
    end
    else begin
      CalculateRowBuffer;
      if (FRowBuffer.Length + FBuffer.Length + 10 {Length(', ()') + reserve} > 16384 {default net_buffer_length value} * 10{??? perf. opt})
        and (FBuffer.Length <> FInsHeaderLen)
      then
        Flush;
      AppendRowBufferToBuffer;
    end;
  end;

  inherited;

  FValues[Col] := Value;
  DoAddObjectRef(FValues[Col]);
end;

procedure TMySQLLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then begin
    CalculateRowBuffer;
    AppendRowBufferToBuffer;
  end;
  Flush;
end;

procedure TMySQLLoader.Finish;
begin
  if FLock then
    ExecSQL('UNLOCK TABLES');
  Reset;
end;

procedure TMySQLLoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TMySQLConnection(Value);
end;

procedure TMySQLLoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  inherited;

  if FieldDesc.DataType = dtBoolean then
    Column.DataType := dtInteger;
end;

initialization

{$IFDEF VER8P}
  ZeroDate := EncodeDate(100, 1, 1);
{$ELSE}
  ZeroDate := not $A9558; // EncodeDate(1, 1, 1);
{$ENDIF}

{$IFDEF HAVE_COMPRESS_INTERFACE}
{$IFNDEF HAVE_COMPRESS_INTERNAL}
  CompressProc := {$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.compress;
  UnCompressProc := {$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.uncompress;
{$ENDIF}
{$ENDIF}
  MySQLInfo := TMySQLInfo.Create(nil);

finalization
  MySQLInfo.Free;

end.
