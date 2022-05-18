
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I PgDac.inc}
unit PgClassesUni;


interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysConst, SysUtils, SyncObjs, Types,
  Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
  CLRClasses,
  CRTypes, CRParser, MemData, CRAccess, CRVio,
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  PgCall, PgSQLNet, PgSQLProtocol, PgError;
{$ELSE}
  PgCallUni, PgSQLNetUni, PgSQLProtocolUni, PgErrorUni;
{$ENDIF}

const
  dtVoid            = 100;
  dtPgName          = 101;
  dtPgChar          = 102;
  dtNumeric         = 105;
  dtPgSingle        = 106;
  dtOID             = 107;
  dtPgLargeObject   = 108;

  dtPgDate          = 109;
  dtPgTime          = 110;
  dtPgTimeStamp     = 111;
  dtPgInterval      = 112;
  dtPgTimeStampTZ   = 113;
  dtPgTimeTZ        = 114;

  dtPgPoint         = 115;
  dtPgLSeg          = 116;
  dtPgBox           = 117;
  dtPgPath          = 118;
  dtPgPolygon       = 119;
  dtPgCircle        = 120;

  dtPgBit           = 121;
  dtPgVarBit        = 122;

  dtNumericFloating = 123;
  MaxNumericWords   = 250;

  dtPgJsonb         = 124;
  JsonbVersion      = 1;

  dtPgLine          = 125;

{$HPPEMIT 'struct TEventInfo;'}
{$HPPEMIT 'class TPgSQLParamDesc;'}

type
  TPgSQLTypes = class;
  TPgSQLCommand = class;
  TPgSQLRecordSet = class;
  TPgSQLTransaction = class;
  TPgCursor = class;
  TPgSQLFieldDesc = class;
{$IFNDEF LITE}
  TPgSQLNotificationsHandler = class;
  TPgSQLNotificationsThread = class;
{$ENDIF}

  TProtocolVersion = (pv20, pv30, pvAuto);

  TPgSQLNotificationEvent = procedure (const Name: string; const PID: integer; const Message: string) of object;

  TSSLMode = (smDisable, smRequire, smPrefer, smAllow, smVerifyCA, smVerifyFull);

  _TStringArray = array of string;


  TPgExtFieldsInfoThread = class (TThread)
  private
    FPauseEvent: TEvent;
    FResultEvent: TEvent;
    FReadyEvent: TEvent;
    FLastError: Exception;

    FConnection: TCRConnection;
    FRecordSet: TCRRecordSet;
    FTablesInfo: TCRTablesInfo;
    FDefaultValues: Boolean;
    FUseProtocol30: Boolean;

    procedure SelectExtFieldsInfo;
  protected
    procedure Execute; override;
  public
    constructor Create(Connection: TCRConnection);
    destructor Destroy; override;

    function Run(TablesInfo: TCRTablesInfo; DefaultValues, UseProtocol30: Boolean): Boolean;
    procedure CloseConnection;
    procedure CheckLastError;
    procedure WaitForResult;
    procedure Release;

    property Connection: TCRConnection read FConnection;
    property RecordSet: TCRRecordSet read FRecordSet;
    property UseProtocol30: Boolean read FUseProtocol30;
  end;

  TPgNeedExtFieldsInfo = record
    NeedInfo: Boolean;
    InfoThread: TPgExtFieldsInfoThread;
  end;

  TPgSQLConnection = class(TCRConnection)
  private
    FDatabase: string;
    FPort: Integer;
    FProtocolVersion: TProtocolVersion;
    FConnectionTimeout: integer;
    FApplicationName: string;
    FSSLMode: TSSLMode;
    FUseUnicode: boolean;
    FCharset,
    FMessagesCharset: string;
    FCharsetID,
    FCharLength: integer;
    FSchema: string;
    FMultipleSchema: boolean;
    FSchemas: _TStringArray;
    FCachedSchema: string;
    FEnablePgTimeStamps: boolean;
    FIntervalAsString: boolean;
    FEnableGeometrics: boolean;
    FEnableComposites: boolean;
    FEnableDomains: boolean;
    FImmediateNotices: boolean;

    FLock: TCriticalSection;
    FProtocol: TPgSQLProtocol;
    FTypes: TPgSQLTypes;
  {$IFNDEF LITE}
    FNotificationsHandler: TPgSQLNotificationsHandler;
  {$ENDIF}
    FInTransaction: boolean;
    FIntegerDateTimes: boolean;
    FByteaOutputAsHex: boolean;
    FBCDPrecision, FBCDScale: integer;
    FIPVersion: TIPVersion;
    FLastInsertId: Int64;
    FRedhisftConnection: boolean;
    FMultipleConnections: boolean;
    FSwapConnection: TPgSQLConnection;
    FExtFieldInfoConnection: TPgSQLConnection;
    FExtFieldInfoThread: TPgExtFieldsInfoThread;

    FOnNotice: TPgProtocolNoticeEvent;
    FOnNotification: TPgSQLNotificationEvent;

    procedure ProcessInternalException(E: EPgError; Component: TObject);

    function GetNewConnection: TPgSQLConnection;
    function QuerySwapConnection: TPgSQLConnection;
    procedure ReturnSwapConnection(var Value: TPgSQLConnection);
    procedure ReleaseSwapConnection;
    function GetExtFieldInfoThread(TablesInfo: TCRTablesInfo; DefaultValues, UseProtocol30: Boolean): TPgExtFieldsInfoThread;
    procedure ReleaseExtFieldInfoThread(var Value: TPgExtFieldsInfoThread);
    procedure TerminateExtFieldInfoThread(var Value: TPgExtFieldsInfoThread);
    procedure CloseExtFieldInfoConnection;

    procedure SetClientEncoding;
    procedure GetIntegerDateTimes;
    procedure GetByteaOutput;

    procedure DoOnNotice(Errors: TPgErrors);
    procedure DoOnNotification(const Name: string; const PID: integer; const Message: string);

    function GetSchemaNamesForSQL(Flag: integer): string;
  protected
    procedure InitCommandProp(Command: TCRCommand); override;
    procedure InitRecordSetProp(RecordSet: TCRRecordSet); override;

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
    class function GetMapRulesClass: TCRMapRulesClass; override;

    function CreateObject(DataType: Word; Transaction: TCRTransaction): TDBObject; override;
  {$ENDIF}

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    function CheckIsValid: boolean; override;
  {$IFNDEF NODBACCESS}
    procedure ReturnToPool; override;
  {$ENDIF}

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;
    procedure Ping; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    procedure SetServer(const Value: string); override;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;
    function GetMajorServerVersion: integer;
    function GetMinorServerVersion: integer;
    function VersionIsEqualOrHigher(MajorVer, MinorVer: integer): boolean;

    function GetProcessID: integer;
    function GetProtocolVersion: TProtocolVersion;
    function GetProtocol: TPgSQLProtocol;
    function GetTypes: TPgSQLTypes;
    function GetCurrentSchema: string;
    procedure SetCurrentSchema(Value: string);
    function GetCachedSchema: string;
    function GetInTransaction: boolean;
  {$IFNDEF LITE}
    function GetNotificationsHandler: TPgSQLNotificationsHandler;
  {$ENDIF}

    function CanChangeDatabase: boolean; override;

    procedure BreakExec;

    property OnNotice: TPgProtocolNoticeEvent read FOnNotice write FOnNotice;
    property OnNotification: TPgSQLNotificationEvent read FOnNotification write FOnNotification;
  end;

  TPgSQLTransaction = class(TCRTransaction)
  private
    FCommand: TPgSQLCommand;
    FImplicitlyStarted: boolean;
  {$IFDEF DEBUG_MULTITHREAD}
    FStartCount: integer;
  {$ENDIF}

    procedure CheckCommand;
    procedure ExecuteSQL(const SQL: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure StartTransaction; override;
    procedure StartImplicitTransaction;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: string); override;
    procedure ReleaseSavepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;
  end;

  TPgSQLTypes = class
  private
  {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TPgSQLConnection;
  {$IFNDEF LITE}
    FUnknownTypes: TThreadList;
    FTypes: TThreadList;

    function IsUnknownType(TypeCode: integer): boolean;
    function FindType(TypeCode: integer; TypeClass: TClass): TObjectType; overload;
    function FindType(const TypeName: string; TypeClass: TClass): TObjectType; overload;
    function GetType(TypeCode: integer; TypeClass: TClass): TObjectType; overload;
    function GetType(const TypeName: string; TypeClass: TClass): TObjectType; overload;
  {$ENDIF}

  public
    constructor Create(Connection: TPgSQLConnection);
    destructor Destroy; override;

    procedure DecodeTypeLength(TypeOID: Integer; TypeModifier: Integer; var Length, Scale: Integer);
    procedure DetectDataType(TypeOID: integer; {$IFNDEF LITE}FetchConverter: TFetchConverter;{$ENDIF}
      var DataType, SubDataType: word; var Length, Scale, Size: integer;
      var Fixed: boolean; var ObjectType: TObjectType;
      LongStrings, FlatBuffers, OIDAsInt, EnableBCD, EnableFMTBCD,
      CursorAsString, UnknownAsString, FieldsAsText: boolean);
    function GetInternalType(TypeCode: integer; var ObjectType: TObjectType):word;
    function DataTypeToOID(DataType, SubDataType: word): integer;
  {$IFNDEF LITE}
    function GetDomainType(TypeCode: integer): TObjectType;
    function GetRowType(TypeCode: integer): TObjectType; overload;
    function GetRowType(const TypeName: string): TObjectType; overload;
  {$ENDIF}
  end;

  TPgSQLParamDesc = class(TParamDesc)
  private
  {$IFDEF LITE}
    FTypeOID: integer;
  {$ENDIF}
  public
    procedure SetNull(const Value: boolean); override;

    function GetAsCursor: TPgCursor;
    procedure SetAsCursor(Value: TPgCursor);
  end;

  TPgSQLInfo = class(TSQLInfo)
  protected
    function HasOnlyLexem: boolean; override;
    procedure ParseExtColumnName(Parser: TSQLParser; var Code: integer; var Str: string); override;
  public
    function IdentCase: TIdentCase; override;
    function QuoteSchemasIfNeed(const Value: string; var Values: _TStringArray): string;
  end;

  TPgSQLFieldDesc = class(TCRFieldDesc)
  private
    FIsTextMode: boolean;
    FTableOID: integer;
    FTableCol: smallint;
  public
    property TableOID: integer read FTableOID write FTableOID;
    property TableCol: smallint read FTableCol write FTableCol;
  end;

  TPgCursor = class(TCRCursor)
  private
    FStmtHandle: TPgSQLStatement;
    FNativeStatement: boolean;

    procedure CreateStatement;
    procedure FreeStatement;
  protected
    FState: TCursorState;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetStmtHandle(const Handle: TPgSQLStatement);

    function CanFetch: boolean; override;
    property State: TCursorState read FState;
  end;

  TProcParamsInfo = record
    RetTypeOID: integer;
    IsReturnSet: boolean;
    TypeOIDs: TIntegerDynArray;
    Modes, Names: _TStringArray;
    Schema: string;
  end;

  TPgSQLCommand = class(TCRCommand)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TPgSQLConnection;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FUsedConnection: TPgSQLConnection;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FRecordSet: TPgSQLRecordSet;
    FCommandTimeout: integer;
    FOIDAsInt: boolean;
    FFieldsAsText: boolean;
    FUseParamTypes: boolean;
    FDistinctParams: boolean;
    FExplicitlyPrepared: boolean;

    FCursor: TPgCursor;
    FCursorRef: TPgCursor;
    FNextCursorRef: TPgCursor;
    FNativePreparation: boolean;

    FForcedProtocol: TProtocolVersion;
    FUseSimpleProtocol: boolean;

    FLastInsertOID: Int64;
    FRowsAffected: integer;

    FInParamsCount: integer;
    FInParamRefs: array of TPgSQLParamDesc;
    FOutParamsCount: integer;

    FLeftmostBracket,
    FRightmostBracket: integer;

    function GetFormatCode(DataType: Word; TypeModifier, Scale: integer; IsField: boolean): integer;

    function UseReader: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function NeedReader: boolean;
    function UseSimpleProtocol: boolean;
    function NeedFetchConnection: boolean;
    procedure PrepareFetchConnection;
    procedure ReleaseFetchConnection(InErrorProcessing: boolean);
    procedure CheckConnection;

    function GetFinalSQL: string;

    procedure MakeSPParam(const Name: string; Oid: integer; Direction: char);

    function NativeCursor: boolean;
    function OutParamsReturn: boolean;
    function RowsReturn: boolean;

    procedure UpdateRowsAffected;

    function IsCursor: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function HasParams: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}

    function LockProtocol: TPgSQLProtocol; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure UnlockProtocol(Protocol: TPgSQLProtocol); {$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    // simple (Text) protocol
    function GetFinalSQLWithParamValues: string;

    // binary protocol
    procedure CallbackBindParamValue(Dest: TPgSQLNet; ParamNo: integer; var ItemDesc: TPgSQLItemDesc);
    procedure PerformPrepare(const SQL: string; ParsedSQLType: TParsedSQLType; const ParamTypes: TIntegerDynArray);
    procedure PerformUnprepare;
    procedure PerformBindExecute;
    procedure PerformClosePortal;

    procedure InternalPrepare;
    procedure InternalUnprepare;
    procedure InternalExecute;
    procedure DescribeFields;
    procedure DescribeParams;
    procedure ReadOutParams;
    procedure SplitParams;
    procedure ResetParams;

    function GetParsedSQLType: TParsedSQLType;
    function GetBatchSQL(ForSimpleProtocol: boolean; Iters, Offset: integer): string; reintroduce;
    function GetBatchIters(Iters: integer): integer; override;
    procedure InternalExecuteBatch(Iters, Offset: integer); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    class function GetParamDescClass: TParamDescClass; override;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    function ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string; override;
    function IsValidBatchSQL: boolean; override;
    procedure InitProcParams(const Name: string; Overload: integer);
    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; overload; override;
    function ForceCreateSPParams: boolean; override;
    procedure GetProcParamsInfo(const Schema, Name: string; overload: integer;
      var ParamsInfo: TProcParamsInfo);

    function GetPrepared: boolean; override;
    procedure Prepare; override;
    procedure Unprepare; override;
    procedure Execute; override;
    procedure ExecuteBatch(Iters, Offset: integer); override;
    procedure Close; override;

    procedure SetConnection(Value: TCRConnection); override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    function GetProtocol: TPgSQLProtocol;
    function GetCursor: TCRCursor; override;
    procedure SetCursor(Value: TCRCursor); override;
    function GetFirstCursor(): TPgCursor;
    function GetNextCursor(): TPgCursor;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;
    procedure BreakExec; override;

    property EnableBCD;
    property EnableFMTBCD;
  end;

  TCreateFieldObjectFunc = function(DataType: word): TSharedObject of Object;

  TPgSQLRecordSet = class(TCRRecordSet)
  private
    FFetchCursor: TPgCursor;
    FCreateFieldObjectFunc: TCreateFieldObjectFunc;
    FActualFields: array of integer;
    FDescribeExecute: boolean;
    FNeedExtFieldsInfo: TPgNeedExtFieldsInfo;
    FIsFetchStart: Boolean;
    FPrefetchRows: Integer;
    FPrefetchedRowCount: Integer;
    FAllRowsPrefetched: Boolean;

    FCacheBlobs: boolean;
    FDeferredBlobRead: boolean;
    FCursorAsString: boolean;
    FUnknownAsString: boolean;
    FCursorWithHold: boolean;
    FNoFetchData: boolean;
    FExplicitlyPrepared: boolean;

    procedure SetCommandType;
    procedure CalcPrefetchRows(StmtHandle: TPgSQLStatement);
    procedure ReceiveFetchBuffer(Source: TPgBufferProvider; Size: integer; FetchBlock: IntPtr;
      Row, Col: integer);

    procedure GetExtFieldsInfo;
    function RequestExtFieldsInfo: TPgExtFieldsInfoThread;
    procedure ReceiveExtFieldsInfo(InfoThread: TPgExtFieldsInfoThread);
    procedure ReleaseExtFieldsInfo;
    class function GetColumnInfo(const SQL: string; SQLInfo: TSQLInfo; TablesInfo: TCRTablesInfo): TCRColumnsInfo;
    class function GetExtFieldsInfoSQL(TablesInfo: TCRTablesInfo; UseProtocol30, ServerVersionIs81, DefaultValues: Boolean): string;
    class procedure ProcessExtFieldsInfo(RecordSet: TCRRecordSet; ColumnsInfo: TCRColumnsInfo;
      TablesInfo: TCRTablesInfo; Fields: TFieldDescs; UseProtocol30: boolean);
  protected
    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
    function UsedConnection: TPgSQLConnection;

    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InitFetchCursor;

    function NeedInitFieldsOnFetch: boolean; override;
    procedure CreateFieldDescs; override;
  {$IFNDEF LITE}
    function InternalCompareFieldValue(ValuePtr: IntPtr; ValueLen: Word; ValueType: Word; DataBuf: IntPtr; DataLen: Word; FieldType: Word; HasParent: boolean; IsFixed: boolean; const Options: TCompareOptions): integer; override;
  {$ENDIF}

    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

  { Fetch }
    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); override;
    procedure ProcessFetchedBlock(Block: PBlockHeader; FetchBack: boolean); override;
    function ProcessFetchedException(E: Exception): boolean; override;
    function NeedUnPrepareAfterFetch: boolean; override;
    procedure DoBeforeFetch(out Cancel: boolean); override;

  {$IFNDEF LITE}
    procedure SyncKeyFields(KeyFields, OriginFields: TFieldDescs); override;
    procedure SyncDataFields(DataFields, OriginFields: TFieldDescs); override;
  {$ENDIF}

    function IsSupportedDataType(DataType: word): boolean; override;
    function IsSpecificType(const Field: TFieldDesc; var DataType: word; var DataTypeName: string; var Len, Scale: integer): boolean; override;

    property IsFetchAll: boolean read FFetchAll;
  public
    constructor Create; override;
    destructor Destroy; override;

  { Fetch }
    procedure ExecFetch(DisableInitFields: boolean); override;
    procedure FetchAll; override;
    function CanDisconnect: boolean; override;

  { Open/Close }
    procedure Prepare; override;
    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override;
    function IsFullReopen: boolean; override;
    procedure Reopen; override;

  { Fields }
    procedure DetectIdentityField; override;
    function GetFieldDescType: TFieldDescClass; override;
    procedure GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean); override;
    procedure PutFieldData(Field: TFieldDesc; DataBuf: IntPtr; DataLenPtr: PWord; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False); override;
    procedure GetMappedFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: variant; UseRollback: boolean = False; FlatRecBuf: boolean = False); override;
    procedure DetectFieldType(const FieldName: string; TableCol, TypeOid, TypeModifier: integer;
      var DBType, DataType, SubDataType: word; var Length, Scale, Size: integer; var Fixed: boolean; var ObjectType: TObjectType);
    class function IsBlobDataType(DataType: word): boolean; override;
    class function IsObjectDataType(DataType: word): boolean; override;
    class function IsSharedObjectDataType(DataType: word): boolean; override;

  { Records }
    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    class procedure InternalCreateComplexField(RecBuf: IntPtr; Connection: TPgSQLConnection;
      Field: TFieldDesc; Offset: integer; DataType: word; SubDataType: word; ObjectType: TObjectType;
      CreateFieldObjectFunc: TCreateFieldObjectFunc; CacheBlobs: boolean; FetchToOuterBuffer: Boolean = False);
    procedure FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
  {$IFNDEF LITE}
    procedure CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc); override;
    function GetSortOptions(SortColumn: TSortColumn): TCompareOptions; override;
    function GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; override;
 {$ENDIF}

  { Navigation }
    procedure SetToEnd; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    property OnCreateFieldObject: TCreateFieldObjectFunc read FCreateFieldObjectFunc write FCreateFieldObjectFunc;
  end;

{ TPgSQLMetaData }

  TPgSQLMetaData = class (TCRMetaData)
  protected
    function GetConnection: TPgSQLConnection;
    function CreateRecordSet: TCRRecordSet; override;

    function InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData; override;
    procedure InternalGetMetaDataKindsList(List: TStringList); override;
    procedure InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string); override;

    function GetTables(Restrictions: TStrings): TData; override;
    function GetColumns(Restrictions: TStrings): TData; override;
    function GetProcedures(Restrictions: TStrings): TData; override;
  {$IFNDEF LITE}
    procedure CreateProcedureParametersFields; override;
    function GetProcedureParameters(Restrictions: TStrings): TData; override;
  {$ENDIF}
    function GetIndexes(Restrictions: TStrings): TData; override;
    function GetIndexColumns(Restrictions: TStrings): TData; override;
    function GetConstraints(Restrictions: TStrings): TData; override;
    function GetConstraintColumns(Restrictions: TStrings): TData; override;

    function GetDataTypes(Restrictions: TStrings): TData; override;
    function GetDatabases(Restrictions: TStrings): TData; override;
    function GetUsers(Restrictions: TStrings): TData; override;
    function GetSequences(Restrictions: TStrings): TData;
    function GetSchemas(Restrictions: TStrings): TData;
  end;

{ TPgBufferConverter }

  TPgBufferConverter = class
  public
    { Writing }
    class procedure VarToFmtBcd(const Value: variant; var Bcd: TBcd);
    class procedure VarToPgDate(const Value: variant;
      var PgDate: TSharedObject; var NeedFree: boolean);
    class procedure VarToPgTime(const Value: variant; WithTimeZone: boolean;
      var PgTime: TSharedObject; var NeedFree: boolean);
    class procedure VarToPgTimeStamp(const Value: variant;
      var PgTimeStamp: TSharedObject; var NeedFree: boolean);
    class procedure VarToPgInterval(const Value: variant;
      var PgInterval: TSharedObject; var NeedFree: boolean);
    class procedure VarToOID(const Value: variant; var OID: integer);
  {$IFNDEF LITE}
    class procedure VarToPgGeometric(const Value: variant; DataType: word;
      var PgGeometric: TSharedObject; var NeedFree: boolean);
    class procedure VarToPgRow(const Value: variant; ObjectType: TObjectType;
      var PgRow: TDBObject; var NeedFree: boolean);
  {$ENDIF}
    class procedure VarToCursor(const Value: variant; var CursorName: string);

    { Reading }
    class procedure ReadString(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr; Trim: boolean);
    class procedure ReadWideString(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr; Trim: boolean);
    class procedure ReadMemo(Source: TPgBufferProvider; Size: integer; Dest: IntPtr; IsJsonb: boolean = False);
    class procedure ReadWideMemo(Source: TPgBufferProvider; Size: integer; Dest: IntPtr; IsJsonb: boolean = False);
    class procedure ReadPgCursor(Source: TPgBufferProvider; Size: integer; Dest: IntPtr;
      Connection: TPgSQLConnection);
    class procedure ReadGuid(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr; WithBraces: boolean);
  end;

{ TPgBinaryConverter }

  TPgNumeric = record
    NDigits: integer;
    Weight: integer;
    Sign: integer;
    DScale: integer;
    Digits: array [0 .. MaxNumericWords - 1] of word;
    StartPos: integer;
  end;

  TPgBinaryConverter = class (TPgBufferConverter)
  private
    class procedure ReadSingleAsFloat(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadSingle(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadNumeric(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadNumericAsInt(Source: TPgBufferProvider; Dest: IntPtr; DataType: Word);

    class procedure StringToNumeric(const Value: string; var Numeric: TPgNumeric);
    class procedure IntToNumeric(Value: Int64; var Numeric: TPgNumeric);
    class procedure DoubleToNumeric(Value: double; var Numeric: TPgNumeric);
    class procedure BcdToNumeric(BCD: TBCD; var Numeric: TPgNumeric);
    class procedure StripNumeric(var Numeric: TPgNumeric);

  public
    class procedure WriteValue(Value: Variant; Dest: TPgSQLNet;
      DataType, SubDataType: word; ObjectType: TObjectType; Connection: TPgSQLConnection; UseBinaryFormat: boolean = False);
    class procedure WriteSmallInt(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteInteger(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteBigInt(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteDouble(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteSingle(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteNumeric(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteCurrency(Value: Variant; Dest: TPgSQLNet;
      Connection: TPgSQLConnection);
    class procedure WriteBoolean(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteString(Value: Variant; Dest: TPgSQLNet; UseUnicode: boolean; IsJsonb: boolean = False);
    class procedure WriteWideString(Value: Variant; Dest: TPgSQLNet; UseUnicode: boolean; IsJsonb: boolean = False);
    class procedure WriteOID(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteDate(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteTime(Value: Variant; Dest: TPgSQLNet;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure WriteTimeStamp(Value: Variant; Dest: TPgSQLNet;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure WriteInterval(Value: Variant; Dest: TPgSQLNet;
      IntegerDateTimes: boolean);
    class procedure WriteGuid(Value: Variant; Dest: TPgSQLNet; UseBinaryFormat: boolean = False);
  {$IFNDEF LITE}
    class procedure WritePgGeometric(Value: Variant; Dest: TPgSQLNet;
      DataType: word);
    class procedure WritePgRow(Value: Variant; Dest: TPgSQLNet;
      ObjectType: TObjectType; Connection: TPgSQLConnection);
  {$ENDIF}

    class procedure ReadValue(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr;
      DataType, SubDataType: word; Trim: boolean; Connection: TPgSQLConnection);
    class procedure ReadInt16(Source: TPgBufferProvider; Dest: IntPtr; SubDataType: Word);
    class procedure ReadInt32(Source: TPgBufferProvider; Dest: IntPtr; SubDataType: Word);
    class procedure ReadInt64(Source: TPgBufferProvider; Dest: IntPtr; SubDataType: Word);
    class procedure ReadDouble(Source: TPgBufferProvider; Dest: IntPtr; SubDataType: Word);
    class procedure ReadBCD(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadFMTBCD(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadCurrency(Source: TPgBufferProvider; Dest: IntPtr; Connection: TPgSQLConnection);
    class procedure ReadBoolean(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadBlob(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadPgLargeObject(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadDate(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadTime(Source: TPgBufferProvider; Dest: IntPtr;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure ReadDateTime(Source: TPgBufferProvider; Dest: IntPtr;
      IntegerDateTimes, WithTimeZone: boolean);
  {$IFNDEF LITE}
    class procedure ReadPgDate(Source: TPgBufferProvider; Dest: IntPtr);
    class procedure ReadPgTime(Source: TPgBufferProvider; Dest: IntPtr;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure ReadPgTimeStamp(Source: TPgBufferProvider; Dest: IntPtr;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure ReadPgInterval(Source: TPgBufferProvider; Dest: IntPtr;
      IntegerDateTimes: boolean);
    class procedure ReadPgGeometric(Source: TPgBufferProvider; Size: integer;
      Dest: IntPtr; DataType: word);
    class procedure ReadPgRow(Source: TPgBufferProvider; Dest: IntPtr;
      Trim: boolean; Connection: TPgSQLConnection);
  {$ENDIF}
  end;

{ TPgTextConverter }

  TPgTextConverter = class (TPgBufferConverter)
  public
    class function GetInt64(Source: TPgBufferProvider; Size: integer): Int64;
    class function EscapeBuffer(const Value: TBytes; UseUnicode, StringQuote, ByteaQuote, LoaderQuote: boolean; EscapeSign: boolean): string;
    class function EscapeString(const Value: string; UseUnicode, StringQuote, ByteaQuote, LoaderQuote: boolean; EscapeSign: boolean): string;

    class function ValueToText(const Value: variant; DataType: word; SubDataType: Word;
      UseUnicode: boolean; Quote: boolean; LoaderQuote: boolean; EscapeSign: boolean; ForceTimezone: boolean): string;
    class function BooleanToText(const Value: variant): string;
    class function BlobToText(const Value: variant; UseUnicode, StringQuote, ByteaQuote, LoaderQuote, EscapeSign: boolean): string;
    class function DateToText(const Value: variant): string;
    class function TimeToText(const Value: variant; HasTimeZone: boolean): string;
    class function TimeStampToText(const Value: variant; HasTimeZone: boolean): string;
    class function IntervalToText(const Value: variant): string;
    class function LargeObjectToText(const Value: variant): string;
    class function GUIDToText(const Value: variant): string;
  {$IFNDEF LITE}
    class function PgGeometricToText(const Value: variant; DataType: word): string;
    class function PgRowToText(const Value: variant): string;
  {$ENDIF}
    class function CursorToText(const Value: variant; UseUnicode: boolean): string;

    class procedure ReadValue(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr;
      DataType, SubDataType: word; Trim: boolean; Connection: TPgSQLConnection);
    class procedure ReadWideString(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr; Trim: boolean; UseUnicode: boolean);
    class procedure ReadInt16(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadInt32(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadInt64(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadDouble(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadBCD(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadFMTBCD(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadCurrency(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadBoolean(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadBlob(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadBlobAsHex(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadPgLargeObject(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadDate(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadTime(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadDateTime(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
  {$IFNDEF LITE}
    class procedure ReadPgDate(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadPgTime(Source: TPgBufferProvider; Size: integer; Dest: IntPtr; WithTimeZone: boolean);
    class procedure ReadPgTimeStamp(Source: TPgBufferProvider; Size: integer; Dest: IntPtr; WithTimeZone: boolean);
    class procedure ReadPgInterval(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadPgGeometric(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
    class procedure ReadPgRow(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
  {$ENDIF}
  end;

{$IFNDEF LITE}

{ TPgSQLAlerter }

  TPgSQLAlerter = class(TCRAlerter)
  private
    FOnEvent: TPgSQLNotificationEvent;

  protected
    procedure DoOnEvent(const EventName: string; const PID: integer; const Message: string);

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SendEvent(const EventName, Message: string); override;
    procedure Start; override;
    procedure Stop; override;

    property OnEvent: TPgSQLNotificationEvent read FOnEvent write FOnEvent;
  end;

{ TPgSQLNotificationsHandler }

  TEventInfo = record
    EventName: string;
    Callbacks: array of TPgSQLNotificationEvent;
  end;

  TPgSQLNotification = class
  public
    PID: integer;
    Name: string;
    Message: string;
  end;

  TPgSQLNotificationsHandler = class
  private
    FBaseConnection: TPgSQLConnection;
    FConnection: TPgSQLConnection;
    FEvents: array of TEventInfo;
    FThread: TPgSQLNotificationsThread;
{$IFDEF MSWINDOWS}
    FGCHandle: IntPtr;
{$ELSE}
    FLastNotification: TPgSQLNotification;
{$ENDIF}
    FLockConnection: TCriticalSection;
    FConnectionIsLocked: boolean;
    FAllowLockEvent: TEvent;

    function EventIndex(const Name: string): integer;
    procedure Start;
    procedure Stop;
    procedure Listen(const Name: string);
    procedure Unlisten(const Name: string);
    procedure LockConnection;
{$IFDEF MSWINDOWS}
    function GetGCHandle: IntPtr;
{$ELSE}
    procedure DoNotification;
{$ENDIF}
    procedure ProcessNotification(const Name: string; const PID: integer; const Message: string);
    procedure DoOnNotification(NotificationPID: Integer; const NotificationName: string; const NotificationMsg: string);

{$IFDEF MSWINDOWS}
    property GCHandle: IntPtr read GetGCHandle;
{$ENDIF}

  public
    constructor Create(BaseConnection: TPgSQLConnection);
    destructor Destroy; override;

    procedure RegisterEvents(Events: TStrings; Callback: TPgSQLNotificationEvent);
    procedure UnregisterEvents(Events: TStrings; CallbackObject: TObject);
  end;

  TPgSQLNotificationsThread = class (TThread)
  private
    FHandler: TPgSQLNotificationsHandler;
  protected
    procedure Execute; override;
  end;

{ TPgSQLLoaderColumn }

  TPgSQLLoaderColumn = class(TCRLoaderColumn)
  private
    FSubDataType: integer;
    FRowType: TObjectType;
    FRowTypeName: string;

  public
    property SubDataType: integer read FSubDataType write FSubDataType;
    property RowTypeName: string read FRowTypeName write FRowTypeName;
  end;

{ TPgSQLLoader }

  TPgSQLLoader = class(TCRLoader)
  private
    FConnection: TPgSQLConnection;
    FCommand: TPgSQLCommand;
    FRowValues: array of variant;
    FPrepared: boolean;
    FBlockOpened: boolean;
    FBufferSize: integer;
    FTextMode: boolean;
    FIsTextMode: boolean;
    FNet: TPgSQLNet;

  protected
    procedure SetConnection(Value: TCRConnection); override;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); override;

    procedure LoadRow;
    procedure WriteHeader;
    procedure BeginDataBlock;
    procedure EndDataBlock;

    procedure DoAfterLoadRow;
    function IsPutColumnDataAllowed: boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    class function GetColumnClass: TCRLoaderColumnClass; override;
    procedure Reset; override;
    procedure Prepare; override;
    procedure DoLoad; override;
    procedure Finish; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
  end;

{$ENDIF}

var
  PgSQLInfo: TPgSQLInfo;
  BCDPrecision: string = '';

const
  DefValProtocol = pvAuto;
  DefValSSLMode = smDisable;
  DefValMultipleConnections = True;
  MaxBatchParams = 65535;

implementation


uses
  Math,
{$IFDEF MSWINDOWS}
  Messages, Registry,
{$ENDIF}
  CRProps, CRFunctions, CRTimeStamp, MemUtils, DAConsts,
{$IFNDEF UNIDACPRO}
  PgConsts, PgProps, {$IFNDEF LITE}PgDataTypeMap,{$ENDIF} PgParser, PgObjects;
{$ELSE}
  UniConsts,
  PgConstsUni, PgPropsUni, PgDataTypeMapUni, PgParserUni, PgObjectsUni;
{$ENDIF}

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
var
  hUtilWindow: HWND;
{$ENDIF}
{$ENDIF}

function CheckTypeCorrespondence(SourceType: Word; DestType: Word): boolean;
begin
  case SourceType of
    dtMemo, dtWideMemo:
      Result := (DestType = SourceType) or
        (DestType in [dtBlob, dtMemo, dtWideMemo, dtString, dtWideString]);
    dtBlob:
      Result := DestType in [dtBlob, dtMemo, dtWideMemo];
    dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ,
    dtPgInterval,
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine,
    dtObject:
      Result := DestType = SourceType;
  else
    Result := True;
  end;
end;

procedure GetPrecAndScale(const Value: string; var Precision, Scale: integer);
var
  P: integer;
begin
  Precision := 0;
  Scale := 0;
  if Trim(Value) = '' then
    Exit;
  P := Pos(',', Value);
  if P > 0 then begin
    Precision := StrToInt(Trim(Copy(Value, 1, P - 1)));
    Scale := StrToInt(Trim(Copy(Value, P + 1, Integer(Length(Value) - P))));
  end
  else begin
    Precision := StrToInt(Trim(Value));
    Scale := 0;
  end;
end;

{ TPgBufferConverter}

const
  MaxDecimalWords = 7;
  DecimalArray : array [0 .. MaxDecimalWords] of double = (
    1,
    10000,
    100000000,
    1000000000000,
    10000000000000000,
    100000000000000000000.0,
    1000000000000000000000000.0,
    10000000000000000000000000000.0);

  DecimalArrayInt : array [0 .. 4] of Int64 = (
    1,
    10000,
    100000000,
    1000000000000,
    10000000000000000);

  DecimalArrayBCD : array [0 .. 3] of Int64 = (
    10000,
    100000000,
    1000000000000,
    10000000000000000);

  NUMERIC_POS = $0000;
  NUMERIC_NEG = $4000;
  DEC_DIGITS  = 4;

class procedure TPgBufferConverter.VarToFmtBcd(const Value: variant; var Bcd: TBcd);
var
  i64: int64;
begin
  case VarType(Value) of
    varSmallint, varInteger, varByte, varWord, varShortInt:
      Bcd := IntegerToBcd(Value);
    varLongWord, varInt64: begin
      i64 := Value;
      Bcd := StrToBcd(IntToStr(i64));
    end;
    varSingle, varDouble, varCurrency:
      Bcd := DoubleToBcd(Value);
  else
    if VarIsStr(Value) then
      Bcd := StrToBcd(Value)
    else
    if VarType(Value) = VarFMTBcd then
      Bcd := VarToBcd(Value)
    else
      raise EConvertError.Create(SCannotConvertType);
  end;
end;

class procedure TPgBufferConverter.VarToPgDate(const Value: variant;
  var PgDate: TSharedObject; var NeedFree: boolean);
var
  Days: integer;
  Date: TDateTime;
  Obj: TObject;
  ts: TCustomPgTimeStamp;
begin
  PgDate := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varDate, varDouble: begin
        ts := TPgDate.Create;
        PgDate := ts;
        NeedFree := True;
        Date := Value;
        TPgDate.FromDateTime(Date, Days);
        ts.Days := Days;
      end;
      varSharedObject: begin
        Obj := TObject(TVarData(Value).VPointer);
        if not IsClass(Obj, TCustomPgTimeStamp) then
          raise Exception.Create(SCannotConvertType);
        PgDate := TCustomPgTimeStamp(Obj);
      end;
    else
      if VarType(Value) = VarSQLTimeStamp then begin
        ts := TPgDate.Create;
        PgDate := ts;
        NeedFree := True;
        TPgDate.FromSQLTimeStamp(VarToSqlTimeStamp(Value), Days);
        ts.Days := Days;
      end
      else
      if VarIsStr(Value) then begin
        ts := TPgDate.Create;
        PgDate := ts;
        NeedFree := True;
        TPgDate.FromString(VarToStr(Value), Days, False);
        ts.Days := Days;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgDate.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToPgTime(const Value: variant; WithTimeZone: boolean;
  var PgTime: TSharedObject; var NeedFree: boolean);
var
  Ticks: int64;
  TimeZoneOffset: integer;
  Date: TDateTime;
  Obj: TObject;
  ts: TPgTime;
begin
  PgTime := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varDate, varDouble: begin
        ts := TPgTime.Create;
        PgTime := ts;
        NeedFree := True;
        Date := Value;
        TPgTime.FromDateTime(Date, Ticks);
        ts.Ticks := Ticks;
        if WithTimeZone then
          ts.TimeZoneOffset := GetUtcOffset(Now);
      end;
      varSharedObject: begin
        Obj := TObject(TVarData(Value).VPointer);
        if not IsClass(Obj, TCustomPgTimeStamp) then
          raise Exception.Create(SCannotConvertType);
        PgTime := TCustomPgTimeStamp(Obj);
      end;
    else
      if VarType(Value) = VarSQLTimeStamp then begin
        ts := TPgTime.Create;
        PgTime := ts;
        NeedFree := True;
        TPgTime.FromSQLTimeStamp(VarToSqlTimeStamp(Value), Ticks);
        ts.Ticks := Ticks;
        if WithTimeZone then
          ts.TimeZoneOffset := GetUtcOffset(Now);
      end
      else
      if VarIsStr(Value) then begin
        ts := TPgTime.Create;
        PgTime := ts;
        NeedFree := True;
        TPgTime.FromString(VarToStr(Value), Ticks, TimeZoneOffset, False);
        ts.Ticks := Ticks;
        ts.TimeZoneOffset := TimeZoneOffset;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgTime.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToPgTimeStamp(const Value: variant;
  var PgTimeStamp: TSharedObject; var NeedFree: boolean);
var
  Days: integer;
  Ticks: int64;
  Date: TDateTime;
  Obj: TObject;
  ts: TCustomPgTimeStamp;
begin
  PgTimeStamp := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varDate, varDouble: begin
        ts := TPgTimeStamp.Create;
        PgTimeStamp := ts;
        NeedFree := True;
        Date := Value;
        TPgTimeStamp.FromDateTime(Date, Days, Ticks);
        ts.Days := Days;
        ts.Ticks := Ticks;
      end;
      varSharedObject: begin
        Obj := TObject(TVarData(Value).VPointer);
        if not IsClass(Obj, TCustomPgTimeStamp) then
          raise Exception.Create(SCannotConvertType);
        PgTimeStamp := TCustomPgTimeStamp(Obj);
      end;
    else
      if VarType(Value) = VarSQLTimeStamp then begin
        ts := TPgTimeStamp.Create;
        PgTimeStamp := ts;
        NeedFree := True;
        TPgTimeStamp.FromSQLTimeStamp(VarToSqlTimeStamp(Value), Days, Ticks);
        ts.Days := Days;
        ts.Ticks := Ticks;
      end
      else
      if VarIsStr(Value) then begin
        ts := TPgTimeStamp.Create;
        PgTimeStamp := ts;
        NeedFree := True;
        TPgTimeStamp.FromString(VarToStr(Value), Days, Ticks, False);
        ts.Days := Days;
        ts.Ticks := Ticks;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgTimeStamp.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToPgInterval(const Value: variant;
  var PgInterval: TSharedObject; var NeedFree: boolean);
var
  Months, Days: integer;
  Seconds: double;
  Obj: TObject;
  Int: TPgInterval;
begin
  PgInterval := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varSharedObject: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TPgInterval) then
          raise Exception.Create(SCannotConvertType);
        PgInterval := TPgInterval(Obj);
      end;
    else
      if VarIsStr(Value) then begin
        Int := TPgInterval.Create;
        PgInterval := Int;
        NeedFree := True;
        TPgInterval.FromString(VarToStr(Value), Months, Days, Seconds);
        Int.MonthsFull := Months;
        Int.Days := Days;
        Int.SecondsFull := Seconds;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgInterval.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToOID(const Value: variant; var OID: integer);
var
  Obj: TObject;
  Lob: TPgSQLLargeObject;
begin
  case VarType(Value) of
    varSharedObject: begin
    {$IFDEF CLR}
      Obj := Value;
    {$ELSE}
      Obj := TObject(TVarData(Value).VPointer);
    {$ENDIF}
      if not IsClass(Obj, TPgSQLLargeObject) then
        raise Exception.Create(SCannotConvertType);
      Lob := TPgSQLLargeObject(Obj);
      OID := Lob.OID;
    end;
  else
    OID := Value;
  end;
end;

{$IFNDEF LITE}
class procedure TPgBufferConverter.VarToPgGeometric(const Value: variant; DataType: word;
  var PgGeometric: TSharedObject; var NeedFree: boolean);
var
  Obj: TObject;
begin
  PgGeometric := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varSharedObject: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TPgGeometric) then
          raise Exception.Create(SCannotConvertType);

        PgGeometric := TPgGeometric(Obj);
      end;
    else
      if VarIsStr(Value) then begin
        case DataType of
          dtPgPoint:
            PgGeometric := TPgPoint.Create;
          dtPgLSeg:
            PgGeometric := TPgLSeg.Create;
          dtPgBox:
            PgGeometric := TPgBox.Create;
          dtPgPath:
            PgGeometric := TPgPath.Create;
          dtPgPolygon:
            PgGeometric := TPgPolygon.Create;
          dtPgCircle:
            PgGeometric := TPgCircle.Create;
          dtPgLine:
            PgGeometric := TPgLine.Create;
        else
          Assert(False);
        end;
        NeedFree := True;
        TPgGeometric(PgGeometric).AsString := Value;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgGeometric.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToPgRow(const Value: variant; ObjectType: TObjectType;
  var PgRow: TDBObject; var NeedFree: boolean);
var
  Obj: TObject;
begin
  PgRow := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varSharedObject: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TPgRow) then
          raise Exception.Create(SCannotConvertType);

        PgRow := TPgRow(Obj);
        if (ObjectType <> nil) and (PgRow.ObjectType <> ObjectType) then
          raise Exception.Create(SCannotConvertType);
      end;
    else
      if VarIsStr(Value) then begin
        Assert(ObjectType <> nil);
        PgRow := TPgRow.Create(TPgRowType(ObjectType));
        NeedFree := True;
        TPgRow(PgRow).AsString := Value;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgRow.Free;
    raise;
  end;
end;
{$ENDIF NDEF LITE}

class procedure TPgBufferConverter.VarToCursor(const Value: variant; var CursorName: string);
var
  Obj: TObject;
  Cursor: TPgRefCursor;
begin
  case VarType(Value) of
    varSharedObject: begin
    {$IFDEF CLR}
      Obj := Value;
    {$ELSE}
      Obj := TObject(TVarData(Value).VPointer);
    {$ENDIF}
      if not IsClass(Obj, TPgRefCursor) then
        raise Exception.Create(SCannotConvertType);
      Cursor := TPgRefCursor(Obj);
      CursorName := Cursor.CursorName;
    end;
  else
    CursorName := VarToStr(Value);
  end;
end;

class procedure TPgBufferConverter.ReadString(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr; Trim: boolean);
var
  EndPtr: PByte;
begin
  Size := Source.ReadAnsiString(Dest, Size);
  if Trim then begin
    EndPtr := PtrOffset(Dest, Size - sizeof(AnsiChar));
    while (NativeUInt(EndPtr) >= NativeUInt(Dest)) and (EndPtr^ in [$00, $20 {' '}]) do
      Dec(EndPtr);
    Inc(EndPtr);
    EndPtr^ := $00;
    Size := PtrSubstract(EndPtr, Dest);
  end;
end;

class procedure TPgBufferConverter.ReadWideString(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr; Trim: boolean);
var
  EndPtr: PWord;
begin
  Size := Source.ReadWideString(Dest, Size);
  if Trim then begin
    EndPtr := PtrOffset(Dest, Size - sizeof(WideChar));
    while (NativeUInt(EndPtr) >= NativeUInt(Dest)) and (EndPtr^ in [$0000, $0020 {' '}]) do
      Dec(EndPtr);
    Inc(EndPtr);
    EndPtr^ := $0000;
    Size := PtrSubstract(EndPtr, Dest);
  end;
  Size := Size shr 1;
end;

class procedure TPgBufferConverter.ReadMemo(Source: TPgBufferProvider; Size: integer; Dest: IntPtr; IsJsonb: boolean = False);
var
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
begin
  if IsJsonb and (Size > 0) then begin
    Source.ReadByte;
    Dec(Size);
  end;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  if Size > 0 then begin
    Blob.AllocPiece(Piece, Size);
    try
      BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
      Source.ReadAnsiString(BlobData, Size, False);
      Piece.Used := Size;
      Blob.AppendPiece(Piece);
    except
      Blob.FreePiece(Piece);
    end;
  end
  else
    Blob.AsString := '';
end;

class procedure TPgBufferConverter.ReadWideMemo(Source: TPgBufferProvider; Size: integer; Dest: IntPtr; IsJsonb: boolean = False);
var
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
begin
  if IsJsonb and (Size > 0) then begin
    Source.ReadByte;
    Dec(Size);
  end;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  if Size > 0 then begin
    Blob.AllocPiece(Piece, Size * 2);
    try
      BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
      Piece.Used := Source.ReadWideString(BlobData, Size, False);
      Blob.CompressPiece(Piece);
      Blob.AppendPiece(Piece);
    except
      Blob.FreePiece(Piece);
    end;
  end
  else
    Blob.AsString := '';
end;

class procedure TPgBufferConverter.ReadPgCursor(Source: TPgBufferProvider; Size: integer;
  Dest: IntPtr; Connection: TPgSQLConnection);
var
  Cursor: TPgRefCursor;
  CursorName: string;
begin
  Cursor := TPgRefCursor(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  CursorName := Source.ReadString(Size);
  Cursor.Assign(Connection, CursorName);
end;

class procedure TPgBufferConverter.ReadGuid(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr; WithBraces: boolean);
var
  Str: AnsiString;
begin
  Str := Source.ReadAnsiString(Size);
  if WithBraces and (Str <> '') and (Str[1] <> '{') then begin
    Marshal.WriteByte(Dest, Byte('{'));
    Move({$IFNDEF NEXTGEN}Str[1]{$ELSE}Str.Ptr^{$ENDIF}, PtrOffset(Dest, 1)^, Size);
    Inc(Size);
    Marshal.WriteByte(Dest, Size, Byte('}'));
    Inc(Size);
  end
  else
    Move({$IFNDEF NEXTGEN}Str[1]{$ELSE}Str.Ptr^{$ENDIF}, Dest^, Size);
  Marshal.WriteByte(Dest, Size, 0);
end;

{ TPgBinaryConverter }

class procedure TPgBinaryConverter.WriteValue(Value: Variant; Dest: TPgSQLNet;
  DataType, SubDataType: word; ObjectType: TObjectType; Connection: TPgSQLConnection; UseBinaryFormat: boolean = False);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) or (DataType = dtCursor) then
    Dest.WriteInt32(-1)
  else begin
    // PgLoader support
    case DataType of
      dtNumeric,
      dtNumericFloating: begin
        DataType := dtFloat;
        SubDataType := dtNumeric;
      end;
      dtPgTimeTZ: begin
        DataType := dtTime;
        SubDataType := dtPgTimeTZ;
      end;
      dtPgTimeStampTZ: begin
        DataType := dtDateTime;
        SubDataType := dtPgTimeStampTZ;
      end;
    end;

    Dest.EnterSizeBlock;
    try
      case DataType of
        dtSmallInt:
          WriteSmallInt(Value, Dest);
        dtInteger:
          if SubDataType = dtOID then
            WriteOID(Value, Dest)
          else
            WriteInteger(Value, Dest);
        dtLargeInt:
          WriteBigInt(Value, Dest);
        dtFloat, dtBCD, dtFMTBCD:
          case SubDataType of
            dtPgSingle:
              WriteSingle(Value, Dest);
            dtNumeric,
            dtNumericFloating:
              WriteNumeric(Value, Dest);
          {$IFDEF LITE}
            dtLargeint:
              WriteBigInt(Value, Dest);
          {$ENDIF}
          else
            WriteDouble(Value, Dest);
          end;
        dtCurrency:
          WriteCurrency(Value, Dest, Connection);
        dtBoolean:
          WriteBoolean(Value, Dest);
        dtString, dtExtString, dtMemo, dtBlob:
          WriteString(Value, Dest, Connection.FUseUnicode, SubDataType = dtPgJsonb);
        dtWideString, dtExtWideString, dtWideMemo:
          WriteWideString(Value, Dest, Connection.FUseUnicode, SubDataType = dtPgJsonb);
        dtDate, dtPgDate:
          WriteDate(Value, Dest);
        dtTime, dtPgTime, dtPgTimeTZ:
          WriteTime(Value, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeTZ);
        dtDateTime, dtPgTimeStamp, dtPgTimeStampTZ:
          WriteTimeStamp(Value, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeStampTZ);
        dtPgInterval:
          WriteInterval(Value, Dest, Connection.FIntegerDateTimes);
        dtPgLargeObject:
          WriteOID(Value, Dest);
        dtGuid:
          WriteGuid(Value, Dest, UseBinaryFormat);
      {$IFNDEF LITE}
        dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine:
          WritePgGeometric(Value, Dest, DataType);
        dtObject:
          WritePgRow(Value, Dest, ObjectType, Connection);
      {$ENDIF}
      end;
    finally
      Dest.LeaveSizeBlock;
    end;
  end;
end;

class procedure TPgBinaryConverter.WriteSmallInt(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteInt16(Value);
end;

class procedure TPgBinaryConverter.WriteInteger(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteInt32(Value);
end;

class procedure TPgBinaryConverter.WriteBigInt(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteInt64(Value);
end;

class procedure TPgBinaryConverter.WriteDouble(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteDouble(Value);
end;

class procedure TPgBinaryConverter.WriteSingle(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteSingle(Value);
end;

class procedure TPgBinaryConverter.StringToNumeric(const Value: string; var Numeric: TPgNumeric);
var
  p: integer;

  procedure InvalidString;
  begin
    raise EConvertError.CreateFmt('Invalid NUMERIC string: ''%s''', [Value]);
  end;

  procedure CheckLength;
  begin
    if p > Length(Value) then
      InvalidString;
  end;

  function IsDigit(c: char): boolean;
  begin
    Result := (c >= '0') and (c <= '9');
  end;

const
  MaxDigits = 1000;
var
  HaveDp: boolean;
  Digits: array [0 .. MaxDigits + 8 - 1] of byte;
  i, j, DWeight, DScale, DDigits, Exp, Offset, NDigits: integer;
begin
  p := 1;
  while (p <= Length(Value)) and (Value[p] = ' ') do
    Inc(p);

  CheckLength;
  Numeric.Sign := NUMERIC_POS;
  case Value[p] of
    '+':
      Inc(p);
    '-': begin
      Numeric.Sign := NUMERIC_NEG;
      Inc(p);
    end;
  end;

  CheckLength;
  HaveDp := False;
  if (Value[p] = '.') or (Value[p] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) then begin
    HaveDp := True;
    Inc(p);
  end;

  CheckLength;
  if not IsDigit(Value[p]) then
    InvalidString;

  Digits[0] := 0;
  Digits[1] := 0;
  Digits[2] := 0;
  Digits[3] := 0;
  i := DEC_DIGITS;
  DWeight := -1;
  DScale := 0;

  while p <= Length(Value) do begin
    if IsDigit(Value[p]) then begin
      Digits[i] := Ord(Value[p]) - Ord('0');
      Inc(i);
      Inc(p);
      if HaveDp then
        Inc(DScale)
      else
        Inc(DWeight);
    end
    else
    if (Value[p] = '.') or (Value[p] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) then begin
      if HaveDp then
        InvalidString;
      HaveDp := True;
      Inc(p);
    end
    else
      break;
  end;

  DDigits := i - DEC_DIGITS;
  Digits[i] := 0;
  Digits[i + 1] := 0;
  Digits[i + 2] := 0;

  if (p <= Length(Value)) and ((Value[p] = 'e') or (Value[p] = 'E')) then begin
    Inc(p);
    i := p;
    if (i <= Length(Value)) and ((Value[i] = '+') or (Value[i] = '-')) then
      Inc(i);
    while (i <= Length(Value)) and IsDigit(Value[i]) do
      Inc(i);
    if i = p then
      InvalidString;
    exp := StrToInt(Copy(Value, p, i - p));
    p := i;
    DWeight := DWeight + exp;
    DScale := DScale - exp;
    if DScale < 0 then
      DScale := 0;
  end;

  while p <= Length(Value) do begin
    if Value[p] <> ' ' then
      InvalidString;
    Inc(p);
  end;

  if DWeight >= 0 then
    Numeric.Weight := (DWeight + 1 + DEC_DIGITS - 1) div DEC_DIGITS - 1
  else
    Numeric.Weight := -((-DWeight - 1) div DEC_DIGITS + 1);

  Offset := (Numeric.Weight + 1) * DEC_DIGITS - (DWeight + 1);
  NDigits := (DDigits + Offset + DEC_DIGITS - 1) div DEC_DIGITS;
  Numeric.NDigits := NDigits;
  Numeric.DScale := DScale;
  Numeric.StartPos := 0;

  i := DEC_DIGITS - Offset;
  j := 0;
  while NDigits > 0 do begin
    Numeric.Digits[j] := ((Digits[i] * 10 + Digits[i + 1]) * 10 +
      Digits[i + 2]) * 10 + Digits[i + 3];
    i := i + DEC_DIGITS;
    Inc(j);
    Dec(NDigits);
  end;
end;

class procedure TPgBinaryConverter.IntToNumeric(Value: Int64; var Numeric: TPgNumeric);
var
  i, j: integer;
  w: word;
begin
  // Detect negation & size
  if Value < 0 then begin
    Numeric.Sign := NUMERIC_NEG;
    Value := -Value;
  end
  else
    Numeric.Sign := NUMERIC_POS;

  i := 0;
{$IFNDEF CLR}
  // for Int32 use Integer instead of Int64 for performance improving
  if Int64Rec(Value).Hi = $0 then begin
    j := Value;
    while j <> 0 do begin
      w := j mod 10000;
      Numeric.Digits[i] := w;
      j := j div 10000;
      Inc(i);
    end;
  end
  else
{$ENDIF}
    while Value <> 0 do begin
      w := Value mod 10000;
      Numeric.Digits[i] := w;
      Value := Value div 10000;
      Inc(i);
    end;

  // Save zero value
  if i = 0 then begin
    Numeric.Digits[i] := 0;
    Inc(i);
  end;

  // Reverse array
  for j := 0 to (i shr 1) - 1 do begin
    w := Numeric.Digits[j];
    Numeric.Digits[j] := Numeric.Digits[i - j - 1];
    Numeric.Digits[i - j - 1] := w;
  end;

  Numeric.NDigits := i;
  Numeric.DScale := 0;
  Numeric.StartPos := 0;
  Numeric.Weight := i - 1;
end;

class procedure TPgBinaryConverter.DoubleToNumeric(Value: double; var Numeric: TPgNumeric);
{$IFNDEF CLR}
const
  DecMultiplier: array[0..3] of word = (1, 10, 100, 1000);
var
  i, j: integer;
  b: byte;
  w: word;
  e: Extended;
  FloatRec: TFloatRec;
  Digit: Integer;
{$ELSE}
var
  str: string;
{$ENDIF}
begin
{$IFNDEF CLR}
  // Convert to decimal
  e := Value;
  if Frac(e) = 0 then
    FloatToDecimal(FloatRec, e, fvExtended, 18, 9999)
  else
    FloatToDecimal(FloatRec, e, fvExtended, 15, 9999);

  // Detect negation
  if FloatRec.Negative then
    Numeric.Sign := NUMERIC_NEG
  else
    Numeric.Sign := NUMERIC_POS;

  Numeric.NDigits := 0;
  Numeric.DScale := 0;
  Numeric.StartPos := 0;

  // Save integer part
  i := 0;
  j := 0;
  w := 0;
  while i < FloatRec.Exponent do begin
    b := Byte(FloatRec.Digits[i]);
    if b > 0 then begin
      b := b - Byte('0');
      Inc(i);
    end
    else
      Dec(FloatRec.Exponent);
    Digit := (FloatRec.Exponent - i) and $03;
    w := w + b * DecMultiplier[(FloatRec.Exponent - i) and $03];
    if (Digit = 0) or (i = FloatRec.Exponent) then begin
      Numeric.Digits[j] := w;
      Inc(Numeric.NDigits);
      Inc(j);
      w := 0;
    end;
  end;

  // Save zero value
  if i = 0 then begin
    Numeric.Digits[j] := 0;
    Inc(Numeric.NDigits);
    Inc(j);
  end;

  // Save float part
  i := FloatRec.Exponent;
  w := 0;
  while i <= 20 do begin
    if i >= 0 then begin
      b := Byte(FloatRec.Digits[i]);
      if b = 0 then
        break;
      b := b - Byte('0')
    end
    else
      b := 0;
    Digit := (FloatRec.Exponent - i + 3) and $03;
    Inc(i);
    w := w + b * DecMultiplier[Digit];
    Inc(Numeric.DScale);
    if (Digit = 0) or (i = 20) or ((i >= 0) and (FloatRec.Digits[i] = {$IFDEF VER17P}0{$ELSE}#0{$ENDIF})) then begin
      Numeric.Digits[j] := w;
      Inc(Numeric.NDigits);
      Inc(j);
      w := 0;
    end;
  end;

  Numeric.Weight := Numeric.NDigits - (Numeric.DScale + 3) div 4 - 1;
{$ELSE}
  str := FloatToStr(Value);
  StringToNumeric(str, Numeric);
{$ENDIF}
end;

class procedure TPgBinaryConverter.BcdToNumeric(BCD: TBCD; var Numeric: TPgNumeric);
const
  DecMultiplier: array[0..3] of word = (1, 10, 100, 1000);
var
  i, j: integer;
  b: byte;
  w: word;
  Digit, IntDigits: integer;
begin
  // Detect negation
  if IsBcdNegative(BCD) then
    Numeric.Sign := NUMERIC_NEG
  else
    Numeric.Sign := NUMERIC_POS;

  IntDigits := BCD.Precision - BCD.SignSpecialPlaces and $3F;
  Numeric.NDigits := 0;
  Numeric.DScale := 0;
  Numeric.StartPos := 0;

  // Save integer part
  i := 0;
  j := 0;
  w := 0;
  while i < IntDigits do begin
    b := BCD.Fraction[i shr 1];
    if i and 1  = 0 then
      b := b shr 4
    else
      b := b and $0F;
    Inc(i);
    Digit := (IntDigits - i) and $03;
    w := w + b * DecMultiplier[Digit];
    if Digit = 0 then begin
      Numeric.Digits[j] := w;
      Inc(Numeric.NDigits);
      Inc(j);
      w := 0;
    end;
  end;

  // Save zero value
  if i = 0 then begin
    Numeric.Digits[j] := 0;
    Inc(Numeric.NDigits);
    Inc(j);
  end;

  // Save float part
  while i < BCD.Precision do begin
    b := BCD.Fraction[i shr 1];
    if i and 1  = 0 then
      b := b shr 4
    else
      b := b and $0F;
    Digit := (IntDigits - i + 3) and $03;
    Inc(i);
    w := w + b * DecMultiplier[Digit];
    Inc(Numeric.DScale);
    if (Digit = 0) or (i = BCD.Precision) then begin
      Numeric.Digits[j] := w;
      Inc(Numeric.NDigits);
      Inc(j);
      w := 0;
    end;
  end;

  Numeric.Weight := Numeric.NDigits - (Numeric.DScale + 3) div 4 - 1;
end;

class procedure TPgBinaryConverter.StripNumeric(var Numeric: TPgNumeric);
var
  p, c: integer;
begin
  p := Numeric.StartPos;
  while (p < Numeric.StartPos + Numeric.NDigits) and (Numeric.Digits[p] = 0) do
    Inc(p);

  c := p - Numeric.StartPos;
  Numeric.Weight := Numeric.Weight - c;
  Numeric.NDigits := Numeric.NDigits - c;
  Numeric.StartPos := p;

  p := Numeric.StartPos + Numeric.NDigits - 1;
  while (p >= Numeric.StartPos) and (Numeric.Digits[p] = 0) do
    Dec(p);

  Numeric.NDigits := p - Numeric.StartPos + 1;

  if Numeric.NDigits = 0 then begin
    Numeric.Sign := NUMERIC_POS;
    Numeric.Weight := 0;
  end;
end;

class procedure TPgBinaryConverter.WriteNumeric(Value: Variant; Dest: TPgSQLNet);
var
  Numeric: TPgNumeric;
  d: Double;
  i: Integer;
  i64: Int64;
begin
  case VarType(Value) of
    varSingle, varDouble: begin
      d := Value;
      DoubleToNumeric(d, Numeric);
    end;
    varSmallint,varInteger,varByte,varWord,varShortInt:
      IntToNumeric(Integer(Value), Numeric);
    varLongWord, varInt64: begin
      i64 := Value;
      IntToNumeric(i64, Numeric);
    end;
    varCurrency:
      StringToNumeric(CurrToStr(Value), Numeric);
    varBoolean:
      IntToNumeric(integer(Value), Numeric);
  else
    if VarIsStr(Value) then
      StringToNumeric(Value, Numeric)
    else
    if VarType(Value) = VarFMTBcd then
      BcdToNumeric(VarToBcd(Value), Numeric)
    else
      raise Exception.Create(SCannotConvertType);
  end;

  StripNumeric(Numeric);

  Dest.WriteWord(Numeric.NDigits);
  Dest.WriteInt16(Numeric.Weight);
  Dest.WriteWord(Numeric.Sign);
  Dest.WriteWord(Numeric.DScale);

  for i := Numeric.StartPos to Numeric.StartPos + Numeric.NDigits - 1 do
    Dest.WriteWord(Numeric.Digits[i]);
end;

class procedure TPgBinaryConverter.WriteCurrency(Value: Variant; Dest: TPgSQLNet;
  Connection: TPgSQLConnection);
var
  c: Currency;
begin
  c := Value;
  if Connection.VersionIsEqualOrHigher(8,3) then
    Dest.WriteInt64(Round(c * 100))
  else
    Dest.WriteInt32(Round(c * 100));
end;

class procedure TPgBinaryConverter.WriteBoolean(Value: Variant; Dest: TPgSQLNet);
var
  b: boolean;
begin
  b := Value;

  Dest.WriteByte(Byte(b));
end;

class procedure TPgBinaryConverter.WriteString(Value: Variant; Dest: TPgSQLNet; UseUnicode: boolean; IsJsonb: boolean = False);
var
  sa: AnsiString;
  data: TBytes;
  Obj: TObject;
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
  lb, hb: integer;
begin
{$IFNDEF VER10P}
  SetLength(data, 0);
{$ENDIF}
  case VarType(Value) of
    varSharedObject: begin
      Obj := TObject(TVarData(Value).VPointer);
      if not IsClass(Obj, TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      if Blob.IsUnicode then begin
      {$IFNDEF VER9P}
        SetLength(data, 0); // anti-warning
      {$ENDIF}
        data := Encoding.Default.GetBytes(Blob.AsWideString);
        if IsJsonb and (Length(data) > 0) and (data[0] in [$5B, $7B]) then
          Dest.WriteByte(JsonbVersion);
        Dest.WriteBytes(data);
      end
      else begin
        Piece := Blob.FirstPiece;

        if (Piece = nil) and IsJsonb then
          Dest.WriteByte(JsonbVersion);

        while IntPtr(Piece) <> nil do begin
          BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
          if IsJsonb and (Piece = Blob.FirstPiece) and (Piece.Used > 0) and (Byte(BlobData^) in [$5B, $7B]) then
            Dest.WriteByte(JsonbVersion);
          Dest.WriteBytes(BlobData, 0, Piece.Used);
          Piece := Piece.Next;
        end;
      end;
    end;
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        Dest.WriteBytes(TValueArr(BlobData), 0, hb - lb + 1);
      finally
        VarArrayUnlock(Value);
      end;
    end;
  else
    if VarIsStr(Value) then
      if UseUnicode then
        sa := CRFunctions.Utf8Encode(WideString(Value))
      else
        sa := AnsiString(Value)
    else
      if UseUnicode then
        sa := CRFunctions.Utf8Encode(WideString(VarToStr(Value)))
      else
        sa := AnsiString(VarToStr(Value));

    if IsJsonb and ((LengthA(sa) = 0) or (ord(sa[1]{$IFDEF NEXTGEN}[1]{$ENDIF}) in [$5B, $7B])) then
      Dest.WriteByte(JsonbVersion);
    Dest.WriteBytes(TValueArr(sa), 0, LengthA(sa));
  end;
end;

class procedure TPgBinaryConverter.WriteWideString(Value: Variant; Dest: TPgSQLNet; UseUnicode: boolean; IsJsonb: boolean = False);
var
  sa: AnsiString;
  Obj: TObject;
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
  Utf8Buf: TBytes;
  Count: integer;
  MaxLen: integer;
  lb, hb: integer;
begin
  case VarType(Value) of
    varSharedObject: begin
      SetLength(Utf8Buf, 0);
      Obj := TObject(TVarData(Value).VPointer);
      if not IsClass(Obj, TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      if not Blob.IsUnicode then begin
        sa := CRFunctions.UTF8Encode(Blob.AsWideString);
        if IsJsonb and (LengthA(sa) > 0) and (ord(sa[1]{$IFDEF NEXTGEN}[1]{$ENDIF}) in [$5B, $7B]) then
          Dest.WriteByte(JsonbVersion);
        Dest.WriteBytes(TValueArr(sa), 0, LengthA(sa));
        exit;
      end;
      Piece := Blob.FirstPiece;

      if (Piece = nil) and IsJsonb then
        Dest.WriteByte(JsonbVersion);

      while IntPtr(Piece) <> nil do begin
        BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
        MaxLen := (Piece.Used shr 1) * 3 + 1;
        if Length(Utf8Buf) < MaxLen then
          SetLength(Utf8Buf, MaxLen);
        Count := {$IFNDEF NEXTGEN}CRFunctions.{$ENDIF}UnicodeToUtf8(TValueArr(Utf8Buf), MaxLen, BlobData, Piece.Used shr 1);
        if Count > 0 then begin
          if IsJsonb and (Piece = Blob.FirstPiece) and (Byte(BlobData^) in [$5B, $7B]) then
            Dest.WriteByte(JsonbVersion);
          Dest.WriteBytes(TValueArr(Utf8Buf), 0, Count - 1);
        end;
        Piece := Piece.Next;
      end;
    end;
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        Dest.WriteBytes(TValueArr(BlobData), 0, hb - lb + 1);
      finally
        VarArrayUnlock(Value);
      end;
    end;
  else
    if VarIsStr(Value) then
      if UseUnicode then
        sa := CRFunctions.UTF8Encode(WideString(Value))
      else
        sa := AnsiString(Value)
    else
      if UseUnicode then
        sa := CRFunctions.UTF8Encode(WideString(VarToStr(Value)))
      else
        sa := AnsiString(VarToStr(Value));

    if IsJsonb and ((LengthA(sa) = 0) or (ord(sa[1]{$IFDEF NEXTGEN}[1]{$ENDIF}) in [$5B, $7B])) then
      Dest.WriteByte(JsonbVersion);
    Dest.WriteBytes(TValueArr(sa), 0, LengthA(sa));
  end;
end;

class procedure TPgBinaryConverter.WriteOID(Value: Variant; Dest: TPgSQLNet);
var
  OID: integer;
begin
  VarToOID(Value, OID);
  Dest.WriteInt32(OID);
end;

class procedure TPgBinaryConverter.WriteDate(Value: Variant; Dest: TPgSQLNet);
var
  Obj: TSharedObject;
  ts: TCustomPgTimeStamp;
  NeedFree: boolean;
begin
  VarToPgDate(Value, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);
    Dest.WriteInt32(ts.Days);
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WriteTime(Value: Variant; Dest: TPgSQLNet;
  IntegerDateTimes, WithTimeZone: boolean);
var
  Obj: TSharedObject;
  ts: TCustomPgTimeStamp;
  NeedFree: boolean;
begin
  VarToPgTime(Value, WithTimeZone, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);

    if IntegerDateTimes then
      Dest.WriteInt64(ts.Ticks)
    else
      Dest.WriteDouble(ts.Ticks / McSecsPerSec);

    if WithTimeZone then
      Dest.WriteInt32(-ts.TimeZoneOffset);
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WriteTimeStamp(Value: Variant; Dest: TPgSQLNet;
  IntegerDateTimes, WithTimeZone: boolean);
var
  Obj: TSharedObject;
  ts: TCustomPgTimeStamp;
  NeedFree: boolean;
  Days: integer;
  Ticks: int64;
begin
  VarToPgTimeStamp(Value, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);
    Days := ts.Days;
    Ticks := ts.Ticks;

    if WithTimeZone and (Ticks <> High(Int64)) and (Ticks <> Low(Int64)) then
      LocalTimeToUTCTime(Days, Ticks);

    if IntegerDateTimes then begin
      if (Ticks = High(Int64)) or (Ticks = Low(Int64)) then
        Dest.WriteInt64(Ticks)
      else
        Dest.WriteInt64(Days * McSecsPerDay + Ticks)
    end
    else begin
      if Ticks = High(Int64) then
        Dest.WriteDouble(Infinity)
      else
      if Ticks = Low(Int64) then
        Dest.WriteDouble(-Infinity)
      else
        Dest.WriteDouble(Days * (SecsPerDay + 0.0) + Ticks / McSecsPerSec);
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WriteInterval(Value: Variant; Dest: TPgSQLNet;
  IntegerDateTimes: boolean);
var
  Obj: TSharedObject;
  Int: TPgInterval;
  NeedFree: boolean;
begin
  VarToPgInterval(Value, Obj, NeedFree);
  try
    Int := TPgInterval(Obj);

    if IntegerDateTimes then
      Dest.WriteInt64(Round(Int.SecondsFull * McSecsPerSec))
    else
      Dest.WriteDouble(Int.SecondsFull);

    Dest.WriteInt32(Int.Days);
    Dest.WriteInt32(Int.MonthsFull);
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WriteGuid(Value: Variant; Dest: TPgSQLNet; UseBinaryFormat: boolean = False);
var
  Guid: TGUID;
  Buffer: TBytes;
begin
  if UseBinaryFormat then begin
    Guid := StringToGUID(VarToStr(Value));

    SetLength(Buffer, 16);
    PCardinal(@Buffer[0])^ := (Swap(Word(Guid.D1)) shl 16) or Swap(Word(Guid.D1 shr 16));
    PWord(@Buffer[4])^ := Swap(Guid.D2);
    PWord(@Buffer[6])^ := Swap(Guid.D3);
    Move(Guid.D4, Buffer[8], 8);

    Dest.WriteBytes(Buffer);
  end
  else
    Dest.WriteBytes(Encoding.Default.GetBytes(VarToStr(Value)));
end;

{$IFNDEF LITE}
class procedure TPgBinaryConverter.WritePgGeometric(Value: Variant; Dest: TPgSQLNet;
  DataType: word);

  procedure WritePoint(Point: TPgPoint);
  begin
    Dest.WriteDouble(Point.X);
    Dest.WriteDouble(Point.Y);
  end;

var
  i: integer;
  Obj: TSharedObject;
  PgGeometric: TPgGeometric;
  NeedFree: boolean;
begin
  VarToPgGeometric(Value, DataType, Obj, NeedFree);
  try
    PgGeometric := TPgGeometric(Obj);

    case DataType of
      dtPgPoint:
        WritePoint(TPgPoint(PgGeometric));
      dtPgLSeg:
        with TPgLSeg(PgGeometric) do begin
          WritePoint(StartPoint);
          WritePoint(EndPoint);
        end;
      dtPgBox:
        with TPgBox(PgGeometric) do begin
          WritePoint(LowerLeft);
          WritePoint(UpperRight);
        end;
      dtPgPath:
        with TPgPath(PgGeometric) do begin
          if IsClosedPath then
            i := 1
          else
            i := 0;
          Dest.WriteByte(i);
          Dest.WriteInt32(Count);
          for i := 0 to Count - 1 do
            WritePoint(Points[i]);
        end;
      dtPgPolygon:
        with TPgPolygon(PgGeometric) do begin
          Dest.WriteInt32(Count);
          for i := 0 to Count - 1 do
            WritePoint(Points[i]);
        end;
      dtPgCircle:
        with TPgCircle(PgGeometric) do begin
          WritePoint(Center);
          Dest.WriteDouble(Radius);
        end;
      dtPgLine:
        with TPgLine(PgGeometric) do begin
          Dest.WriteDouble(A);
          Dest.WriteDouble(B);
          Dest.WriteDouble(C);
        end;
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WritePgRow(Value: Variant; Dest: TPgSQLNet;
  ObjectType: TObjectType; Connection: TPgSQLConnection);
var
  Obj: TDBObject;
  Row: TPgRow;
  NeedFree: boolean;
  i: integer;
  Attr: TPgAttribute;
  AttrValue: variant;
begin
  VarToPgRow(Value, ObjectType, Obj, NeedFree);
  try
    Row := TPgRow(Obj);
    Dest.WriteInt32(ObjectType.AttributeCount);
    for i := 0 to ObjectType.AttributeCount - 1 do begin
      Attr := TPgAttribute(ObjectType.Attributes[i]);

      Dest.WriteInt32(Attr.TypeOID);

      AttrValue := Row.AttrValueEx(Attr.Name, True);
      WriteValue(AttrValue, Dest, Attr.DataType, Attr.SubDataType, Attr.ObjectType,
        Connection);
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;
{$ENDIF NDEF LITE}

class procedure TPgBinaryConverter.ReadValue(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr;
  DataType, SubDataType: word; Trim: boolean; Connection: TPgSQLConnection);
begin
  case DataType of
    dtSmallint:
      ReadInt16(Source, Dest, SubDataType);
    dtInteger:
      ReadInt32(Source, Dest, SubDataType);
    dtInt64:
      ReadInt64(Source, Dest, SubDataType);
    dtBoolean:
      ReadBoolean(Source, Dest);
    dtSingle:
      ReadSingle(Source, Dest);
    dtFloat:
      ReadDouble(Source, Dest, SubDataType);
    dtBCD:
      ReadBCD(Source, Dest);
    dtFMTBCD:
      ReadFMTBCD(Source, Dest);
    dtCurrency:
      ReadCurrency(Source, Dest, Connection);
    dtString, dtExtString:
      ReadString(Source, Size, Dest, Trim);
    dtWideString, dtExtWideString:
      ReadWideString(Source, Size, Dest, Trim);
    dtDate:
      ReadDate(Source, Dest);
    dtTime:
      ReadTime(Source, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeTZ);
    dtDateTime:
      ReadDateTime(Source, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeStampTZ);
  {$IFNDEF LITE}
    dtPgDate:
      ReadPgDate(Source, Dest);
    dtPgTime:
      ReadPgTime(Source, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeTZ);
    dtPgTimeStamp, dtPgTimeStampTZ:
      ReadPgTimeStamp(Source, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeStampTZ);
    dtPgInterval:
      ReadPgInterval(Source, Dest, Connection.FIntegerDateTimes);
  {$ENDIF}
    dtMemo:
      ReadMemo(Source, Size, Dest, SubDataType = dtPgJsonb);
    dtWideMemo:
      ReadWideMemo(Source, Size, Dest, SubDataType = dtPgJsonb);
    dtBlob:
      ReadBlob(Source, Size, Dest);
    dtPgLargeObject:
      ReadPgLargeObject(Source, Dest);
    dtGuid:
      ReadGuid(Source, Size, Dest, Connection.UuidWithBraces);
  {$IFNDEF LITE}
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine:
      ReadPgGeometric(Source, Size, Dest, DataType);
    dtObject:
      ReadPgRow(Source, Dest, Trim, Connection);
    dtCursor:
      ReadPgCursor(Source, Size, Dest, Connection);
  {$ENDIF}
  end;
end;

class procedure TPgBinaryConverter.ReadInt16(Source: TPgBufferProvider; Dest: IntPtr; SubDataType: Word);
begin
  case SubDataType of
    dtNumeric,
    dtNumericFloating:
      ReadNumericAsInt(Source, Dest, dtInt16);
  else
    Marshal.WriteInt16(Dest, Source.ReadInt16);
  end;
end;

class procedure TPgBinaryConverter.ReadInt32(Source: TPgBufferProvider; Dest: IntPtr; SubDataType: Word);
begin
  case SubDataType of
    dtNumeric,
    dtNumericFloating:
      ReadNumericAsInt(Source, Dest, dtInt32);
  else
    Marshal.WriteInt32(Dest, Source.ReadInt32);
  end;
end;

class procedure TPgBinaryConverter.ReadInt64(Source: TPgBufferProvider; Dest: IntPtr; SubDataType: Word);
begin
  case SubDataType of
    dtNumeric,
    dtNumericFloating:
      ReadNumericAsInt(Source, Dest, dtInt64);
  else
    Marshal.WriteInt64(Dest, Source.ReadInt64);
  end;
end;

class procedure TPgBinaryConverter.ReadDouble(Source: TPgBufferProvider; Dest: IntPtr; SubDataType: word);
begin
  case SubDataType of
    dtNumeric,
    dtNumericFloating:
      ReadNumeric(Source, Dest);
    dtPgSingle:
      ReadSingleAsFloat(Source, Dest);
  {$IFDEF LITE}
    dtLargeint:
      Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Source.ReadInt64));
  {$ENDIF}
  else
    Marshal.WriteByte(Dest, 7, Source.ReadByte);
    Marshal.WriteByte(Dest, 6, Source.ReadByte);
    Marshal.WriteByte(Dest, 5, Source.ReadByte);
    Marshal.WriteByte(Dest, 4, Source.ReadByte);
    Marshal.WriteByte(Dest, 3, Source.ReadByte);
    Marshal.WriteByte(Dest, 2, Source.ReadByte);
    Marshal.WriteByte(Dest, 1, Source.ReadByte);
    Marshal.WriteByte(Dest, 0, Source.ReadByte);
  end;
end;

class procedure TPgBinaryConverter.ReadBCD(Source: TPgBufferProvider; Dest: IntPtr);
var
  i, c, a: integer;
  WordCount: integer;
  Weight, Sign: integer;
  Value: int64;
  w: word;
begin
  WordCount := Source.ReadWord;
  Weight := Source.ReadInt16;
  Sign := Source.ReadWord;
  Source.ReadWord; // read scale

  Value := 0;
  if WordCount > 0 then begin
    i := 0;
    while i <= Weight do begin
      w := Source.ReadWord;
      if Weight - i <= High(DecimalArrayBCD) then begin
        if Weight - i = High(DecimalArrayBCD) then
          w := w mod 100;
        Value := Value + w * DecimalArrayBCD[Weight - i];
      end;
      Inc(i);
      if i = WordCount then
        break;
    end;

    if i < WordCount then begin
      c := i;
      if Weight < 0 then
        a := -Weight
      else
        a := 1;

      for i := a to WordCount - Weight - 1 do begin
        w := Source.ReadWord;
        if i = 1 then
          Value := Value + w;
        Inc(c);
        if c = WordCount then
          break;
      end;
    end;

    if Sign > 0 then
      Value := -Value;
  end;
  Marshal.WriteInt64(Dest, Value)
end;

class procedure TPgBinaryConverter.ReadFMTBCD(Source: TPgBufferProvider; Dest: IntPtr);
var
  i, c, a, Prec, IntDigits: integer;
  WordCount: integer;
  Weight, Sign: integer;
  Value: TBcd;
  w: word;

  procedure SaveDigit(Val: byte);
  var
    p: integer;
  begin
    p := Prec shr 1;
    if not Odd(Prec) then
      Val := Val shl 4;
    Value.Fraction[p] := Value.Fraction[p] or Val;
    Inc(Prec);
  end;

  procedure Save2Digits(Val: byte; RemoveZero: boolean);
  var
    hd, ld: byte;
  begin
    hd := Val div 10;
    ld := Val mod 10;

    if not (RemoveZero and (Prec = 0) and (hd = 0)) then
      SaveDigit(hd);

    SaveDigit(ld);
  end;

  procedure Save4Digits(Val: word; RemoveZero: boolean);
  var
    hd, ld: byte;
  begin
    hd := Val div 100;
    ld := Val mod 100;

    if not (RemoveZero and (Prec = 0) and (hd = 0)) then
      Save2Digits(hd, RemoveZero);

    Save2Digits(ld, RemoveZero);
  end;

  procedure RemoveTrailZeros;
  var
    p: integer;
    Val: byte;
  begin
    while Prec > IntDigits do begin
      p := (Prec - 1) shr 1;
      Val := Value.Fraction[p];
      if Odd(Prec) then
        Val := Val shr 4
      else
        Val := Val and $0F;

      if Val = 0 then
        Dec(Prec)
      else
        break;
    end;
  end;

begin
  WordCount := Source.ReadWord;
  Weight := Source.ReadInt16;
  Sign := Source.ReadWord;
  Source.ReadWord; // read scale

  System.FillChar(Value, SizeOfTBcd, 0);
  Prec := 0;
  IntDigits := 0;
  if WordCount > 0 then begin
    i := 0;
    while i <= Weight do begin
      w := Source.ReadWord;
      if Weight - i < 16 then
        Save4Digits(w, True);
      Inc(i);
      if i = WordCount then begin
        Inc(Prec, (Weight - i + 1) * 4);
        break;
      end;
    end;

    IntDigits := Prec;

    if i < WordCount then begin
      c := i;
      if Weight < 0 then
        a := -Weight
      else
        a := 1;

      Prec := Prec + (a - 1) * 4;

      for i := a to WordCount - Weight - 1 do begin
        w := Source.ReadWord;
        if Prec <= 63 then
          Save4Digits(w, False);
        Inc(c);
        if c = WordCount then
          break;
      end;

      RemoveTrailZeros;
    end;
  end
  else begin
    Prec := 1;
    IntDigits := 1;
  end;

  Value.Precision := Prec;
  Value.SignSpecialPlaces := Prec - IntDigits;
  if Sign > 0 then
    Value.SignSpecialPlaces := Value.SignSpecialPlaces or $80;
  PBcd(Dest)^ := Value;
end;

class procedure TPgBinaryConverter.ReadCurrency(Source: TPgBufferProvider; Dest: IntPtr; Connection: TPgSQLConnection);
var
  d: double;
begin
  if Connection.VersionIsEqualOrHigher(8,3) then
    d := Source.ReadInt64 / 100
  else
    d := Source.ReadInt32 / 100;
  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(d));
end;

class procedure TPgBinaryConverter.ReadSingleAsFloat(Source: TPgBufferProvider; Dest: IntPtr);
var
  i: integer;
  d: double;
begin
  i := Source.ReadInt32;
  d := PSingle(@i)^;

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(d));
end;

class procedure TPgBinaryConverter.ReadSingle(Source: TPgBufferProvider; Dest: IntPtr);
var
  i: integer;
begin
  i := Source.ReadInt32;

  Marshal.WriteInt32(Dest, i);
end;

class procedure TPgBinaryConverter.ReadNumeric(Source: TPgBufferProvider; Dest: IntPtr);
var
  i, c: integer;
  WordCount: integer;
  Weight: Integer;
  Sign: integer;
  Value: double;
  w: word;
begin
  WordCount := Source.ReadWord;
  Weight := Source.ReadInt16;
  Sign := Source.ReadWord;
  Source.ReadWord; // read scale

  Value := 0;
  if WordCount > 0 then begin
    i := 0;
    while i < WordCount do begin
      w := Source.ReadWord;
      Value := Value * 10000 + w;
      Inc(i);
    end;

    c := Weight + 1 - WordCount;
    if c > MaxDecimalWords then
      Value := Value * Power(10, c * 4)
    else if c > 0 then
      Value := Value * DecimalArray[c]
    else if c < -MaxDecimalWords then
      Value := Value * Power(10, c * 4)
    else if c < 0 then
      Value := Value / DecimalArray[-c];

    if Sign > 0 then
      Value := -Value;
  end;
  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Value));
end;

class procedure TPgBinaryConverter.ReadNumericAsInt(Source: TPgBufferProvider; Dest: IntPtr; DataType: Word);
var
  i: integer;
  WordCount: integer;
  Weight, Sign: integer;
  Value: int64;
  w: word;
begin
  WordCount := Source.ReadWord;
  Weight := Source.ReadInt16;
  Sign := Source.ReadWord;
  Source.ReadWord; // read scale

  Value := 0;
  if WordCount > 0 then begin
    i := 0;
    while i <= Weight do begin
      w := Source.ReadWord;
      if Weight - i <= High(DecimalArrayInt) then begin
        if Weight - i = High(DecimalArrayInt) then
          w := w mod 100;
        Value := Value + w * DecimalArrayInt[Weight - i];
      end;
      Inc(i);
      if i = WordCount then
        break;
    end;

    if i < WordCount then
      raise EInvalidCast.Create(SInvalidCast);

    if Sign > 0 then
      Value := -Value;
  end;

  case DataType of
    dtSmallint:
      Marshal.WriteInt16(Dest, Value);
    dtInteger:
      Marshal.WriteInt32(Dest, Value);
    dtLargeint:
      Marshal.WriteInt64(Dest, Value);
  else
    raise EInvalidCast.Create(SInvalidCast);
  end;
end;

class procedure TPgBinaryConverter.ReadBoolean(Source: TPgBufferProvider; Dest: IntPtr);
begin
  Marshal.WriteInt16(Dest, Source.ReadByte);
end;

class procedure TPgBinaryConverter.ReadBlob(Source: TPgBufferProvider; Size: integer;
  Dest: IntPtr);
var
  Blob: TBlob;
  BlobData: IntPtr;
  Piece: PPieceHeader;
begin
  if Size = 0 then
    Exit;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Blob.AllocPiece(Piece, Size);
  try
    BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
    Source.ReadBytes(BlobData, 0, Size);
    Piece.Used := Size;
    Blob.AppendPiece(Piece);
  except
    Blob.FreePiece(Piece);
  end;
end;

class procedure TPgBinaryConverter.ReadPgLargeObject(Source: TPgBufferProvider; Dest: IntPtr);
var
  Obj: TPgSQLLargeObject;
begin
  Obj := TPgSQLLargeObject(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Obj.OID := Source.ReadInt32;
  TPgObjectsUtils.LargeObjectResetOldOID(Obj);
end;

class procedure TPgBinaryConverter.ReadDate(Source: TPgBufferProvider; Dest: IntPtr);
var
  Days: integer;
  Date: TDateTime;
begin
  Days := Source.ReadInt32;
  Date := PostgresBaseDate + Days;
  if Date < MinDateTime then
    Date := MinDateTime;

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

class procedure TPgBinaryConverter.ReadTime(Source: TPgBufferProvider; Dest: IntPtr;
  IntegerDateTimes, WithTimeZone: boolean);
var
  Seconds: double;
  Date: TDateTime;
begin
  if IntegerDateTimes then
    Seconds := Source.ReadInt64 / 1E6
  else
    Seconds := Source.ReadDouble;

  Date := Seconds / SecsPerDay;

  if WithTimeZone then
    Source.ReadInt32; // time zone (seconds)

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

class procedure TPgBinaryConverter.ReadDateTime(Source: TPgBufferProvider; Dest: IntPtr;
  IntegerDateTimes, WithTimeZone: boolean);
var
  t: Int64;
  Seconds: double;
  Date: TDateTime;
begin
  if IntegerDateTimes then begin
    t := Source.ReadInt64;
    if t = High(Int64) then
      Seconds := Infinity
    else
    if t = Low(Int64) then
      Seconds := -Infinity
    else
      Seconds := t / 1000000;
  end
  else
    Seconds := Source.ReadDouble;

  if Seconds = Infinity then
    Date := MaxDateTime
  else
  if Seconds = -Infinity then
    Date := MinDateTime
  else begin
    Date := AddTimeSpan(PostgresBaseDate, Seconds / SecsPerDay);
    if WithTimeZone then
      Date := UTCTimeToLocalTime(Date);
  end;

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;


{$IFNDEF LITE}
class procedure TPgBinaryConverter.ReadPgDate(Source: TPgBufferProvider; Dest: IntPtr);
var
  d: TPgDate;
begin
  d := TPgDate(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  d.Days := Source.ReadInt32;
end;

class procedure TPgBinaryConverter.ReadPgTimeStamp(Source: TPgBufferProvider; Dest: IntPtr;
  IntegerDateTimes, WithTimeZone: boolean);
var
  t: Int64;
  Seconds: double;
  ts: TPgTimeStamp;
  Days: integer;
  Ticks: int64;
begin
  ts := TPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));

  if IntegerDateTimes then begin
    t := Source.ReadInt64;
    if (t = High(Int64)) or (t = Low(Int64)) then
      ts.Ticks := t
    else begin
      ts.Ticks := t mod McSecsPerDay;
      if ts.Ticks < 0 then
        ts.Ticks := ts.Ticks + McSecsPerDay;
      ts.Days := (t - ts.Ticks) div McSecsPerDay;
    end;
  end
  else begin
    Seconds := Source.ReadDouble;
    if Seconds = Infinity then
      ts.Ticks := High(Int64)
    else
    if Seconds = -Infinity then
      ts.Ticks := Low(Int64)
    else begin
      ts.Days := Floor(Seconds / SecsPerDay);
      ts.Ticks := Round((Seconds - ts.Days * (SecsPerDay + 0.0)) * McSecsPerSec);
    end;
  end;
  if WithTimeZone then begin
    ts.HasTimeZone := True;
    if (ts.Ticks <> High(Int64)) and (ts.Ticks <> Low(Int64)) then begin
      Days := ts.Days;
      Ticks := ts.Ticks;
      UTCTimeToLocalTime(Days, Ticks);
      ts.Days := Days;
      ts.Ticks := Ticks;
    end;
  end;
end;

class procedure TPgBinaryConverter.ReadPgTime(Source: TPgBufferProvider; Dest: IntPtr;
  IntegerDateTimes, WithTimeZone: boolean);
var
  Seconds: double;
  ts: TPgTime;
  TimeZone: integer;
  Days: integer;
  Ticks: int64;
begin
  ts := TPgTime(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));

  if IntegerDateTimes then begin
    ts.Ticks := Source.ReadInt64;
  end
  else begin
    Seconds := Source.ReadDouble;
    ts.Ticks := Round(Seconds * McSecsPerSec);
  end;
  if WithTimeZone then begin
    ts.HasTimeZone := True;
    if (ts.Ticks <> High(Int64)) and (ts.Ticks <> Low(Int64)) then begin
      TimeZone := -Source.ReadInt32;
      TPgTimeStamp.FromDateTime(Now, Days, Ticks);
      Ticks := ts.Ticks;
      ServerTimeToLocalTime(Days, Ticks, TimeZone);
      ts.Ticks := Ticks;
      ts.TimeZoneOffset := TimeZone;
    end;
  end;
end;

class procedure TPgBinaryConverter.ReadPgInterval(Source: TPgBufferProvider; Dest: IntPtr;
  IntegerDateTimes: boolean);
var
  Interval: TPgInterval;
begin
  Interval := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  if IntegerDateTimes then
    Interval.SecondsFull := Source.ReadInt64 / McSecsPerSec
  else
    Interval.SecondsFull := Source.ReadDouble;

  Interval.Days := Source.ReadInt32;
  Interval.MonthsFull := Source.ReadInt32;
end;

class procedure TPgBinaryConverter.ReadPgGeometric(Source: TPgBufferProvider; Size: integer;
  Dest: IntPtr; DataType: word);

  procedure ReadPoint(Point: TPgPoint);
  begin
    Point.X := Source.ReadDouble;
    Point.Y := Source.ReadDouble;
  end;

var
  i: integer;
  PgGeometric: TPgGeometric;
begin
  PgGeometric := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  case DataType of
    dtPgPoint:
      ReadPoint(TPgPoint(PgGeometric));
    dtPgLSeg:
      with TPgLSeg(PgGeometric) do begin
        ReadPoint(StartPoint);
        ReadPoint(EndPoint);
      end;
    dtPgBox:
      with TPgBox(PgGeometric) do begin
        ReadPoint(LowerLeft);
        ReadPoint(UpperRight);
      end;
    dtPgPath:
      with TPgPath(PgGeometric) do begin
        i := Source.ReadByte;
        IsClosedPath := i <> 0;
        Count := Source.ReadInt32;
        for i := 0 to Count - 1 do
          ReadPoint(Points[i]);
      end;
    dtPgPolygon:
      with TPgPath(PgGeometric) do begin
        Count := Source.ReadInt32;
        for i := 0 to Count - 1 do
          ReadPoint(Points[i]);
      end;
    dtPgCircle:
      with TPgCircle(PgGeometric) do begin
        ReadPoint(Center);
        Radius := Source.ReadDouble;
      end;
    dtPgLine:
      with TPgLine(PgGeometric) do begin
        A := Source.ReadDouble;
        B := Source.ReadDouble;
        C := Source.ReadDouble;
      end;
  end;
end;

class procedure TPgBinaryConverter.ReadPgRow(Source: TPgBufferProvider; Dest: IntPtr;
  Trim: boolean; Connection: TPgSQLConnection);
var
  Row: TPgRow;
  RowType: TPgRowType;
  Buffer, IndPtr, FieldBuf, ReadBuf, StrBuf: IntPtr;
  Count, i, Size: integer;
  Attr: TPgAttribute;
begin
  Row := TPgRow(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  RowType := Row.RowType;
  Buffer := Row.Buffer;
  IndPtr := PtrOffset(Buffer, RowType.Size);

  Count := Source.ReadInt32;
  for i := 0 to Count - 1 do begin
    Attr := TPgAttribute(RowType.Attributes[i]);

    Source.ReadInt32; // attr type OID
    Size := Source.ReadInt32;
    if Size >= 0 then begin
      FieldBuf := PtrOffset(Buffer, Attr.Offset);
      case Attr.DataType of
        dtExtString: begin
          StrBuf := Marshal.AllocHGlobal(Size + 1);
          ReadBuf := StrBuf;
        end;
        dtExtWideString: begin
          StrBuf := Marshal.AllocHGlobal((Size + 1) * 2);
          ReadBuf := StrBuf;
        end;
      else
        StrBuf := nil;
        ReadBuf := FieldBuf;
      end;
      ReadValue(Source, Size, ReadBuf, Attr.DataType, Attr.SubDataType, Trim, Connection);
      if StrBuf <> nil then
        Marshal.WriteIntPtr(FieldBuf, StrBuf);
      Marshal.WriteByte(IndPtr, i, 0);
    end
    else
      Marshal.WriteByte(IndPtr, i, Byte(-1));
  end;
end;
{$ENDIF NDEF LITE}

{ TPgTextConverter }

class function TPgTextConverter.EscapeBuffer(const Value: TBytes; UseUnicode, StringQuote, ByteaQuote, LoaderQuote: boolean; EscapeSign: boolean): string;
var
  i, Start: integer;
  sb: AnsiStringBuilder;
  b: byte;
  HasEscape: boolean;
begin
  HasEscape := False;
  sb := AnsiStringBuilder.Create(Length(Value));

  try
    if not (StringQuote or ByteaQuote or LoaderQuote) then
      sb.Append(Value, 0, Length(Value))
    else begin
      if StringQuote then
        sb.Append('''');
      Start := 0;
      for i := 0 to Length(Value) - 1 do begin
        b := Value[i];
        if (b < 32) or
           ((UseUnicode or ByteaQuote) and (b > 126)) or
           (b = 92{'\'}) or
           StringQuote and (b = 39{''''})
        then begin
          HasEscape := True;
          sb.Append(Value, Start, i - Start);
          if b = 92{'\'} then begin
            if ByteaQuote and (StringQuote or LoaderQuote) then
              sb.Append('\\\\')
            else
              sb.Append('\\');
          end
          else if b = 39{''''} then
            sb.Append('''''')
          else begin
            if ByteaQuote and (StringQuote or LoaderQuote) then
              sb.Append('\\')
            else
              sb.Append('\');
            sb.Append(AnsiChar((b shr 6) and 3 + 48));
            sb.Append(AnsiChar((b shr 3) and 7 + 48));
            sb.Append(AnsiChar(b and 7 + 48));
          end;
          Start := i + 1;
        end;
      end;

      sb.Append(Value, Start, Length(Value) - Start);

      if StringQuote then begin
        sb.Append('''');
        if EscapeSign and HasEscape then
          sb.Insert(0, 'E');
      end;
    end;

    Result := string(sb.ToString);
  finally
    sb.Free;
  end;
end;

class function TPgTextConverter.EscapeString(const Value: string; UseUnicode, StringQuote, ByteaQuote, LoaderQuote: boolean; EscapeSign: boolean): string;
begin
  if not (StringQuote or ByteaQuote or LoaderQuote) then begin
    Result := Value;
    Exit;
  end;

  if UseUnicode then
    Result := EscapeBuffer(Encoding.UTF8.GetBytes(WideString(Value)), UseUnicode, StringQuote, ByteaQuote, LoaderQuote, EscapeSign)
  else
    Result := EscapeBuffer(Encoding.Default.GetBytes(string(Value)), UseUnicode, StringQuote, ByteaQuote, LoaderQuote, EscapeSign);
end;

class procedure TPgTextConverter.ReadValue(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr;
  DataType, SubDataType: word; Trim: boolean; Connection: TPgSQLConnection);
begin
  case DataType of
    dtSmallint:
      ReadInt16(Source, Size, Dest);
    dtInteger:
      ReadInt32(Source, Size, Dest);
    dtInt64:
      ReadInt64(Source, Size, Dest);
    dtBoolean:
      ReadBoolean(Source, Size, Dest);
    dtFloat:
      ReadDouble(Source, Size, Dest);
    dtBCD:
      ReadBCD(Source, Size, Dest);
    dtFMTBCD:
      ReadFMTBCD(Source, Size, Dest);
    dtCurrency:
      ReadCurrency(Source, Size, Dest);
    dtString, dtExtString:
      ReadString(Source, Size, Dest, Trim);
    dtWideString, dtExtWideString:
      ReadWideString(Source, Size, Dest, Trim, Connection.FUseUnicode);
    dtDate:
      ReadDate(Source, Size, Dest);
    dtTime:
      ReadTime(Source, Size, Dest);
    dtDateTime:
      ReadDateTime(Source, Size, Dest);
  {$IFNDEF LITE}
    dtPgDate:
      ReadPgDate(Source, Size, Dest);
    dtPgTime:
      ReadPgTime(Source, Size, Dest, SubDataType = dtPgTimeTZ);
    dtPgTimeStamp, dtPgTimeStampTZ:
      ReadPgTimeStamp(Source, Size, Dest, SubDataType = dtPgTimeStampTZ);
    dtPgInterval:
      ReadPgInterval(Source, Size, Dest);
  {$ENDIF}
    dtMemo:
      ReadMemo(Source, Size, Dest);
    dtWideMemo:
      ReadWideMemo(Source, Size, Dest);
    dtBlob:
      if Connection.FByteaOutputAsHex then
        TPgTextConverter.ReadBlobAsHex(Source, Size, Dest)
      else
        ReadBlob(Source, Size, Dest);
    dtPgLargeObject:
      ReadPgLargeObject(Source, Size, Dest);
    dtGuid:
      ReadGuid(Source, Size, Dest, Connection.UuidWithBraces);
  {$IFNDEF LITE}
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine:
      ReadPgGeometric(Source, Size, Dest);
    dtObject:
      ReadPgRow(Source, Size, Dest);
    dtCursor:
      ReadPgCursor(Source, Size, Dest, Connection);
  {$ENDIF}
  end;
end;

class procedure TPgTextConverter.ReadWideString(Source: TPgBufferProvider; var Size: integer; Dest: IntPtr; Trim, UseUnicode: boolean);
var
  ws: WideString;
  DataSize: Integer;
  EndPtr: PWord;
begin
  ws := Source.ReadWideString(Size);
  Size := Length(ws);
  DataSize := Size * sizeof(WideChar);
  if ws <> '' then
    Move(ws[1], Dest^, DataSize);
  Marshal.WriteInt16(Dest, DataSize, 0);
  if Trim then begin
    EndPtr := PtrOffset(Dest, DataSize - sizeof(WideChar));
    while (NativeUInt(EndPtr) >= NativeUInt(Dest)) and (EndPtr^ in [$0000, $0020 {' '}]) do
      Dec(EndPtr);
    Inc(EndPtr);
    EndPtr^ := $0000;
    Size := PtrSubstract(EndPtr, Dest) shr 1;
  end;
end;

class function TPgTextConverter.GetInt64(Source: TPgBufferProvider; Size: integer): Int64;
var
  s: string;
begin
  s := Source.ReadString(Size);
  Result := StrToInt64(s);
end;

class procedure TPgTextConverter.ReadInt16(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
begin
  Marshal.WriteInt16(Dest, GetInt64(Source, Size));
end;

class procedure TPgTextConverter.ReadInt32(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
begin
  Marshal.WriteInt32(Dest, GetInt64(Source, Size));
end;

class procedure TPgTextConverter.ReadInt64(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
begin
  Marshal.WriteInt64(Dest, GetInt64(Source, Size));
end;

class procedure TPgTextConverter.ReadDouble(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  Value: Double;
  s: string;
{$IFDEF USE_TFORMATSETTINGS}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
  s := Source.ReadString(Size);
{$IFDEF USE_TFORMATSETTINGS}
  FmtSet.DecimalSeparator := '.';
  Value := StrToFloat(s, FmtSet);
{$ELSE}
  if DecimalSeparator <> '.' then
    s := StringReplace(s, '.', DecimalSeparator, []);
  Value := StrToFloat(s);
{$ENDIF}

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Value));
end;

class procedure TPgTextConverter.ReadBCD(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  s: string;
{$IFDEF USE_TFORMATSETTINGS}
  FmtSet: TFormatSettings;
{$ENDIF}
  Value: currency;
  i64: int64;
begin
  s := Source.ReadString(Size);
{$IFDEF USE_TFORMATSETTINGS}
  FmtSet.DecimalSeparator := '.';
  Value := StrToCurr(s, FmtSet);
{$ELSE}
  if DecimalSeparator <> '.' then
    s := StringReplace(s, '.', DecimalSeparator, []);
  Value := StrToCurr(s);
{$ENDIF}
{$IFDEF CLR}
  i64 := Decimal.ToOACurrency(Value);
{$ELSE}
  i64 := PInt64(@Value)^;
{$ENDIF}
  Marshal.WriteInt64(Dest, i64);
end;

class procedure TPgTextConverter.ReadFMTBCD(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  s: string;
  Value: TBcd;
begin
  s := Source.ReadString(Size);
  if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
    s := StringReplace(s, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []);
  Value := StrToBcd(s);
  PBcd(Dest)^ := Value;
end;

class procedure TPgTextConverter.ReadCurrency(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  b: Byte;
  i: integer;
  IsNegative: boolean;
  Value: Int64;
begin
  Value := 0;
  IsNegative := False;
  for i := 1 to Size do begin
    b := Source.ReadByte;
    case b of
      ord('-'), ord('('):
        IsNegative := True;
      ord('+'), ord('$'), ord(','), ord('.'):;
      ord('0')..ord('9'):
        Value := Value * 10 + b - Ord('0');
    end;
  end;
  if IsNegative then
    Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(- Value / 100))
  else
    Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Value / 100))
end;

class procedure TPgTextConverter.ReadBoolean(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  s: string;
  b: boolean;
begin
  s := Source.ReadString(Size);
  b := (s = 't');
  Marshal.WriteInt16(Dest, Smallint(b));
end;

class procedure TPgTextConverter.ReadBlob(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  Blob: TBlob;
  BlobData: IntPtr;
  Piece: PPieceHeader;
  ReadPos: integer;
  WritePos: integer;
begin
  if Size = 0 then
    Exit;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Blob.AllocPiece(Piece, Size);
  try
    BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));

    Source.ReadBytes(BlobData, 0, Size);

    ReadPos := 0;
    WritePos := 0;
    while ReadPos < Size do begin
      if Marshal.ReadByte(BlobData, ReadPos) = Ord('\') then begin
        if Marshal.ReadByte(BlobData, ReadPos + 1) = Ord('\') then begin
          Marshal.WriteByte(BlobData, WritePos, Ord('\'));
          Inc(ReadPos);
        end
        else begin
          Marshal.WriteByte(BlobData, WritePos,
            (Marshal.ReadByte(BlobData, ReadPos + 1) - 48) * 64 +
            (Marshal.ReadByte(BlobData, ReadPos + 2) - 48) * 8 +
            Marshal.ReadByte(BlobData, ReadPos + 3) - 48);
          Inc(ReadPos, 3);
        end;
      end
      else
      if ReadPos <> WritePos then
        Marshal.WriteByte(BlobData, WritePos, Marshal.ReadByte(BlobData, ReadPos));
      Inc(ReadPos);
      Inc(WritePos);
    end;

    Piece.Used := WritePos;
    if Piece.Used > 0 then
      Blob.CompressPiece(Piece);
    Blob.AppendPiece(Piece);
  except
    Blob.FreePiece(Piece);
  end;
end;

class procedure TPgTextConverter.ReadBlobAsHex(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  Blob: TBlob;
  BlobData: IntPtr;
  Piece: PPieceHeader;

  function HexBlobToBin(Text: IntPtr; Buffer: IntPtr; BufSize: integer): integer;
  const
    Convert: array[$30{Ord('0')}..$66{Ord('f')}] of SmallInt =
      (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1,
       -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
       -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
       -1, 10, 11, 12, 13, 14, 15);
  var
    b1, b2: byte;
    i: integer;
  begin
    i := BufSize;
    while i > 0 do begin
      b1 := byte(Text^);
      b2 := byte(PtrOffset(Text, 1)^);
      if (b1 < $30) or (b1 > $66) or (b2 < $30) or (b2 > $66) then
        Break;
      PByte(Buffer)^ := (Convert[b1] shl 4) + Convert[b2];
      Buffer := PtrOffset(Buffer, 1);
      Text := PtrOffset(Text, 2);
      Dec(i);
    end;
    Result := BufSize - i;
  end;

begin
  if Size = 0 then
    Exit;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Blob.AllocPiece(Piece, Size);
  try
    BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));

    Source.ReadBytes(BlobData, 0, Size);

    HexBlobToBin(PtrOffset(BlobData,  2), BlobData, (Size - 2) shr 1);

    Piece.Used := (Size - 2) shr 1;
    if Piece.Used > 0 then
      Blob.CompressPiece(Piece);
    Blob.AppendPiece(Piece);
  except
    Blob.FreePiece(Piece);
  end;
end;

class procedure TPgTextConverter.ReadPgLargeObject(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  Obj: TPgSQLLargeObject;
begin
  Obj := TPgSQLLargeObject(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Obj.OID := GetInt64(Source, Size);
  TPgObjectsUtils.LargeObjectResetOldOID(Obj);
end;

class procedure TPgTextConverter.ReadDate(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  s: string;
  Days: integer;
  Date: TDateTime;
begin
  s := Source.ReadString(Size);
  TPgDate.FromString(s, Days, True);
  Date := TPgDate.ToDateTime(Days);

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

class procedure TPgTextConverter.ReadTime(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  s: string;
  Ticks: int64;
  TZOffset: integer;
  Date: TDateTime;
begin
  s := Source.ReadString(Size);
  TPgTime.FromString(s, Ticks, TZOffset, True);
  Date := TPgTime.ToDateTime(Ticks);

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

class procedure TPgTextConverter.ReadDateTime(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  s: string;
  Days: integer;
  Ticks: int64;
  Date: TDateTime;
begin
  s := Source.ReadString(Size);
  TPgTimeStamp.FromString(s, Days, Ticks, True);
  Date := TPgTimeStamp.ToDateTime(Days, Ticks);

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;


{$IFNDEF LITE}
class procedure TPgTextConverter.ReadPgDate(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  d: TPgDate;
  s: string;
  Days: integer;
begin
  d := TPgDate(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := Source.ReadString(Size);
  TPgDate.FromString(s, Days, True);
  d.Days := Days;
end;

class procedure TPgTextConverter.ReadPgTime(Source: TPgBufferProvider; Size: integer; Dest: IntPtr;
  WithTimeZone: boolean);
var
  ts: TPgTime;
  s: string;
  tz: integer;
  Ticks: int64;
begin
  ts := TPgTime(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := Source.ReadString(Size);
  TPgTime.FromString(s, Ticks, tz, True);
  ts.Ticks := Ticks;
  if WithTimeZone then begin
    ts.HasTimeZone := True;
    ts.TimeZoneOffset := tz;
  end;
end;

class procedure TPgTextConverter.ReadPgTimeStamp(Source: TPgBufferProvider; Size: integer; Dest: IntPtr;
  WithTimeZone: boolean);
var
  ts: TPgTimeStamp;
  s: string;
  Days: integer;
  Ticks: int64;
begin
  ts := TPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := Source.ReadString(Size);
  TPgTimeStamp.FromString(s, Days, Ticks, True);
  ts.Days := Days;
  ts.Ticks := Ticks;

  if WithTimeZone then
    ts.HasTimeZone := True;
end;

class procedure TPgTextConverter.ReadPgInterval(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  Interval: TPgInterval;
  s: string;
  Months, Days: integer;
  Seconds: double;
begin
  s := Source.ReadString(Size);
  Interval := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  TPgInterval.FromString(s, Months, Days, Seconds);
  Interval.MonthsFull := Months;
  Interval.Days := Days;
  Interval.SecondsFull := Seconds;
end;

class procedure TPgTextConverter.ReadPgGeometric(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  Obj: TPgGeometric;
  s: string;
begin
  Obj := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := Source.ReadString(Size);
  Obj.AsString := s;
end;

class procedure TPgTextConverter.ReadPgRow(Source: TPgBufferProvider; Size: integer; Dest: IntPtr);
var
  Obj: TPgRow;
  s: string;
begin
  Obj := TPgRow(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := Source.ReadString(Size);
  Obj.AsString := s;
end;
{$ENDIF}

class function TPgTextConverter.ValueToText(const Value: variant; DataType: word; SubDataType: Word;
  UseUnicode: boolean; Quote: boolean; LoaderQuote: boolean; EscapeSign: boolean; ForceTimezone: boolean): string;

  procedure ReplaceSeparator;
  var
    p: integer;
  begin
    if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then begin
      p := Pos({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, Result);
      if p > 0 then
        Result[p] := '.';
    end;
  end;

var
  i64: int64;
  bcd: TBcd;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
{$ENDIF}
  case DataType of
    dtSmallint, dtInt8: begin
      Result := IntToStr(Smallint(Value));
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtInteger: begin
      Result := IntToStr(Integer(Value));
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtLargeInt: begin
      i64 := Value;
      Result := IntToStr(i64);
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtUInt8: begin
      Result := IntToStr({$IFDEF VER12P}byte{$ELSE}Smallint{$ENDIF}(Value));
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtUInt16: begin
      Result := IntToStr(Word(Value));
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtUInt32: begin
      Result := IntToStr(Cardinal(Value));
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtSingle,
    dtFloat,
    dtNumeric,
    dtNumericFloating,
    dtPgSingle: begin
      Result := FloatToStr(Extended(Value){$IFDEF VER7P}, FmtSet{$ENDIF});
    {$IFNDEF VER7P}
      ReplaceSeparator;
    {$ENDIF}
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtCurrency: begin
      Result := FormatFloat('#################.##', Extended(Value){$IFDEF VER7P}, FmtSet{$ENDIF});
    {$IFNDEF VER7P}
      ReplaceSeparator;
    {$ENDIF}
    end;
    dtBCD: begin
      Result := CurrToStr(Currency(Value){$IFDEF VER7P}, FmtSet{$ENDIF});
    {$IFNDEF VER7P}
      ReplaceSeparator;
    {$ENDIF}
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtFMTBCD: begin
      VarToFmtBcd(Value, Bcd);
      Result := BcdToStr(Bcd);
      ReplaceSeparator;
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtBoolean:
      Result := BooleanToText(Value);
    dtUnknown, dtString, dtWideString, dtExtString, dtExtWideString:
      Result := Value;
    dtBlob, dtMemo, dtWideMemo: begin
      Result := BlobToText(Value,
                           (DataType <> dtBlob) and (UseUnicode{$IFDEF ODBC_UTF8} or (DataType = dtWideMemo){$ENDIF}),
                           Quote, DataType = dtBlob, LoaderQuote, EscapeSign);
      Exit;
    end;
    dtDate, dtPgDate:
      Result := DateToText(Value);
    dtTime, dtPgTime, dtPgTimeTZ:
      if SubDataType = dtPgTimeTZ then
        Result := TimeToText(Value, True)
      else
        Result := TimeToText(Value, False);
    dtSQLTimeStamp, dtSQLTimeStampOffset,
    dtDateTime, dtPgTimeStamp, dtPgTimeStampTZ:
      if (SubDataType = dtPgTimeStampTZ) or ForceTimezone then
        Result := TimeStampToText(Value, True)
      else
        Result := TimeStampToText(Value, False);
    dtPgLargeObject: begin
      Result := LargeObjectToText(Value);
      Quote := False;
    end;
    dtGuid:
      Result := GUIDToText(Value);
  {$IFNDEF LITE}
    dtPgInterval:
      Result := IntervalToText(Value);
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine:
      Result := PgGeometricToText(Value, DataType);
    dtObject:
      Result := PgRowToText(Value);
  {$ENDIF}
    dtCursor:
      Result := CursorToText(Value, UseUnicode);
  else
    Assert(False, IntToStr(DataType));
  end;

  Result := EscapeString(Result, UseUnicode, Quote, DataType = dtBlob, LoaderQuote, EscapeSign);
end;

class function TPgTextConverter.BooleanToText(const Value: variant): string;
var
  b: boolean;
begin
  b := Value;
  if b then
    Result := 't'
  else
    Result := 'f';
end;

class function TPgTextConverter.BlobToText(const Value: variant; UseUnicode, StringQuote, ByteaQuote, LoaderQuote, EscapeSign: boolean): string;
var
  Obj: TObject;
  Blob: TBlob;
  BlobData: IntPtr;
  lb, hb: integer;
  data: TBytes;
begin
{$IFNDEF VER9P}
  SetLength(data, 0);
{$ENDIF}
  case VarType(Value) of
    varSharedObject: begin
      Obj := TObject(TVarData(Value).VPointer);
      if not IsClass(Obj, TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      if UseUnicode then
        data := Encoding.UTF8.GetBytes(Blob.AsWideString)
      else
        data := Blob.AsBytes;
    end;
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        SetLength(data, hb - lb + 1);
        Move(BlobData^, Pointer(data)^, hb - lb + 1);
      finally
        VarArrayUnlock(Value);
      end;
    end;
  else
    if VarIsStr(Value) then begin
      if UseUnicode then
        data := Encoding.UTF8.GetBytes(WideString(Value))
      else
        data := Encoding.Default.GetBytes(string(Value));
    end
    else
      raise Exception.Create(SCannotConvertType);
  end;

  Result := EscapeBuffer(data, UseUnicode, StringQuote, ByteaQuote, LoaderQuote, EscapeSign);
end;

class function TPgTextConverter.DateToText(const Value: variant): string;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  ts: TCustomPgTimeStamp;
begin
  VarToPgDate(Value, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);
    Result := TPgDate.ConvertToString(ts.Days, 'YYYY-MM-DD');
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class function TPgTextConverter.TimeToText(const Value: variant; HasTimeZone: boolean): string;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  ts: TCustomPgTimeStamp;
begin
  VarToPgTime(Value, True, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);
    Result := TPgTime.ConvertToString(ts.Ticks, HasTimeZone, ts.TimeZoneOffset, 'HH:NN:SS.ZZZZZZ');
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class function TPgTextConverter.TimeStampToText(const Value: variant; HasTimeZone: boolean): string;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  ts: TCustomPgTimeStamp;
  tz: integer;
begin
  VarToPgTimeStamp(Value, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);

    if ts.Ticks = High(Int64) then
      Result := 'infinity'
    else
    if ts.Ticks = Low(Int64) then
      Result := '-infinity'
    else begin
      tz := GetUTCOffset(TPgTimeStamp.ToDateTime(ts.Days, ts.Ticks));
      Result := TPgTimeStamp.ConvertToString(ts.Days, ts.Ticks, HasTimeZone, tz,
        'YYYY-MM-DD HH:NN:SS.ZZZZZZ');
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class function TPgTextConverter.IntervalToText(const Value: variant): string;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  Int: TPgInterval;
begin
  VarToPgInterval(Value, Obj, NeedFree);
  try
    Int := TPgInterval(Obj);
    Result := TPgInterval.ConvertToString(Int.MonthsFull, Int.Days, Int.SecondsFull);
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class function TPgTextConverter.LargeObjectToText(const Value: variant): string;
var
  OID: integer;
begin
  VarToOID(Value, OID);
  Result := IntToStr(OID);
end;

class function TPgTextConverter.GUIDToText(const Value: variant): string;
begin
  Result := Value;
end;

{$IFNDEF LITE}
class function TPgTextConverter.PgGeometricToText(const Value: variant; DataType: word): string;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  PgGeometric: TPgGeometric;
begin
  VarToPgGeometric(Value, DataType, Obj, NeedFree);
  PgGeometric := TPgGeometric(Obj);
  Result := PgGeometric.AsString;
end;

class function TPgTextConverter.PgRowToText(const Value: variant): string;
var
  Obj: TDBObject;
  NeedFree: boolean;
  PgRow: TPgRow;
begin
  VarToPgRow(Value, nil, Obj, NeedFree);
  Assert(not NeedFree);
  PgRow := TPgRow(Obj);
  Result := PgRow.AsString;
end;
{$ENDIF}

class function TPgTextConverter.CursorToText(const Value: variant; UseUnicode: boolean): string;
var
  CursorName: string;
begin
  VarToCursor(Value, CursorName);
  if UseUnicode then
    Result := {$IFNDEF IS_UNICODE}CRFunctions.UTF8Encode{$ENDIF}(WideString(CursorName))
  else
    Result := CursorName;
end;

{ TExtInfoThread }

constructor TPgExtFieldsInfoThread.Create(Connection: TCRConnection);
begin
  inherited Create(False);

  FPauseEvent := TEvent.Create(nil, True, False, '');
  FResultEvent := TEvent.Create(nil, True, True, '');
  FReadyEvent := TEvent.Create(nil, True, True, '');

  FConnection := Connection;
  FreeOnTerminate := True;
end;

destructor TPgExtFieldsInfoThread.Destroy;
begin
  FConnection.ReleaseRecordSet(FRecordSet);
  FConnection.Free;

  FPauseEvent.Free;
  FResultEvent.Free;
  FReadyEvent.Free;

  inherited;
end;

procedure TPgExtFieldsInfoThread.SelectExtFieldsInfo;
var
  SQL: string;
  ServerVersionIs81: Boolean;
begin
  FConnection.Connect('');
  ServerVersionIs81 := TPgSQLConnection(FConnection).VersionIsEqualOrHigher(8, 1);

  SQL := TPgSQLRecordSet.GetExtFieldsInfoSQL(FTablesInfo, FUseProtocol30, ServerVersionIs81, FDefaultValues);

  if FRecordSet = nil then begin
    FRecordSet := FConnection.GetRecordSet;
    FRecordSet.SetProp(prFetchAll, True);
    FRecordSet.SetProp(prOIDAsInt, True);
    FRecordSet.SetSQL(SQL);
  end
  else begin
    FRecordSet.Close;
    if TPgSQLRecordSet(FRecordSet).FCommand.SQL <> SQL then
      FRecordSet.SetSQL(SQL);
  end;

  try
    FRecordSet.Open;
  except
    FConnection.ReleaseRecordSet(FRecordSet);
    FConnection.Disconnect;
    raise;
  end;
end;

procedure TPgExtFieldsInfoThread.Execute;
begin
  while True do
    if FPauseEvent.WaitFor(INFINITE) = wrSignaled then begin
      FPauseEvent.ResetEvent;

      if Terminated then
        Break;

      try
        SelectExtFieldsInfo;
      except
        on E: Exception do begin
          AcquireExceptionObject;
          try
            FLastError.Free;
          finally
            FLastError := E;
          end;
        end;
      end;

      FResultEvent.SetEvent;
    end;
end;

function TPgExtFieldsInfoThread.Run(TablesInfo: TCRTablesInfo; DefaultValues, UseProtocol30: Boolean): Boolean;
begin
  if FReadyEvent.WaitFor(0) = wrSignaled then begin
    FReadyEvent.ResetEvent;
    FResultEvent.ResetEvent;

    FTablesInfo := TablesInfo;
    FDefaultValues := DefaultValues;
    FUseProtocol30 := UseProtocol30;
    FPauseEvent.SetEvent;

    Result := True;
  end
  else
    Result := False;
end;

procedure TPgExtFieldsInfoThread.CloseConnection;
begin
  FResultEvent.WaitFor(INFINITE);
  FConnection.ReleaseRecordSet(FRecordSet);
  FConnection.Disconnect;
end;

procedure TPgExtFieldsInfoThread.CheckLastError;
begin
  if FLastError = nil then
    Exit;

  try
    raise FLastError;
  finally
    FLastError := nil;
  end;
end;

procedure TPgExtFieldsInfoThread.WaitForResult;
begin
  FResultEvent.WaitFor(INFINITE);
end;

procedure TPgExtFieldsInfoThread.Release;
begin
  FResultEvent.WaitFor(INFINITE);
  FConnection.ReleaseRecordSet(FRecordSet);
  FReadyEvent.SetEvent;
end;

{ TPgSQLConnection }

constructor TPgSQLConnection.Create;
begin
  inherited;

  FLock := TCriticalSection.Create;

  FProtocolVersion := pvAuto;
  FTypes := TPgSQLTypes.Create(Self);
  GetPrecAndScale(BCDPrecision, FBCDPrecision, FBCDScale);
  FIPVersion := ivIPv4;
  FImmediateNotices := False;
  FNativeConnection := True;
  FMultipleConnections := DefValMultipleConnections;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' create Connection $' + IntToHex(Integer(Self), 8)));
{$ENDIF}
end;

destructor TPgSQLConnection.Destroy;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' before destroy Connection $' + IntToHex(Integer(Self), 8)));
{$ENDIF}

  Disconnect;
  if FNativeConnection and (FProtocol <> nil) then
    FreeAndNil(FProtocol);

  FTypes.Free;
{$IFNDEF LITE}
  FNotificationsHandler.Free;
{$ENDIF}
  SetLength(FSchemas, 0);

  inherited;

  FLock.Enter;
  try
    TerminateExtFieldInfoThread(FExtFieldInfoThread);
  finally
    FLock.Leave;
  end;
  FLock.Free;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' after destroy Connection $' + IntToHex(Integer(Self), 8)));
{$ENDIF}
end;

function TPgSQLConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prPort:
      FPort := Value;
    prUseUnicode:
      if Value <> FUseUnicode then begin
        if not FRedhisftConnection then
          FUseUnicode := Value
        else
          FUseUnicode := True;

        if GetConnected then begin
          FProtocol.SetUseUnicode(Value);
          SetClientEncoding;
        end;
      end;
    prCharset:
      if not FRedhisftConnection then begin
        if Value <> FCharset then begin
          FCharset := string(Trim(Value));
          if GetConnected then
            SetClientEncoding;
        end;
      end
      else
        FCharset := '';
    prMessagesCharset:
      if Value <> FMessagesCharset then begin
        FMessagesCharset := string(Trim(Value));
        if FProtocol <> nil then
          FProtocol.MessagesCharset := FMessagesCharset;
      end;
    prSchema:
      if not FRedhisftConnection then begin
        if Value <> FSchema then begin
          if GetConnected then
            SetCurrentSchema(Value);
          FSchema := Value;
        end;
      end
      else
        FSchema := '';
    prProtocolVersion:
      if not FRedhisftConnection then
        FProtocolVersion := TProtocolVersion(Value)
      else
        FProtocolVersion := pv30;
    prEnablePgTimeStamps:
      FEnablePgTimeStamps := Value;
    prIntervalAsString:
      FIntervalAsString := Value;
    prEnableGeometrics:
      FEnableGeometrics := Value;
    prEnableComposites:
      FEnableComposites := Value;
    prEnableDomains:
      FEnableDomains := Value;
    prConnectionTimeout: begin
      FConnectionTimeout := Value;
      if FConnected then
        FProtocol.SetTimeout(FConnectionTimeout);
    end;
    prIPVersion:
      FIPVersion := Value;
    prSSLMode: begin
      FSSLMode := TSSLMode(Integer(Value));
      if FSSLMode in [smVerifyCA, smVerifyFull] then begin
        FSSLOptions.TrustServerCertificate := False;
        FSSLOptions.ForceUseTrustServerCertificate := True;
        if Value = smVerifyFull then
          FSSLOptions.IdentityDNSName := FServer;
      end
      else begin
        FSSLOptions.ForceUseTrustServerCertificate := False;
      end;
    end;
    prApplicationName:
      FApplicationName := Value;
    prBCDPrecision:
      GetPrecAndScale(Value, FBCDPrecision, FBCDScale);
    prLastInsertId:
      FLastInsertId := Value;
    prImmediateNotices: begin
      FImmediateNotices := Value;
      if FConnected then
        FProtocol.SetImmediateNotices(Value);
    end;
    prRedshiftConnection:
      FRedhisftConnection := Value;
    prMultipleConnections:
      FMultipleConnections := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TPgSQLConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prPort:
      Value := FPort;
    prSchema:
      Value := FSchema;
    prUseUnicode:
      Value := FUseUnicode;
    prCharset:
      Value := FCharset;
    prMessagesCharset:
      Value := FMessagesCharset;
    prProtocolVersion:
      Value := Variant(FProtocolVersion);
    prEnablePgTimeStamps:
      Value := FEnablePgTimeStamps;
    prIntervalAsString:
      Value := FIntervalAsString;
    prEnableGeometrics:
      Value := FEnableGeometrics;
    prEnableComposites:
      Value := FEnableComposites;
    prEnableDomains:
      Value := FEnableDomains;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prIPVersion:
      Value := FIPVersion;
    prSSLMode:
      Value := Integer(FSSLMode);
    prLastInsertId:
      Value := FLastInsertId;
    prImmediateNotices:
      Value := FImmediateNotices;
    prRedshiftConnection:
      Value := FRedhisftConnection;
    prMultipleConnections:
      Value := FMultipleConnections;
  else
    Result := inherited GetProp(Prop, Value);
  end
end;

function TPgSQLConnection.CheckIsValid: boolean;
begin
  if not FIsValid then
    Result := False
  else begin
    try
      FProtocol.Ping;
    except
      FIsValid := False;
    end;
    Result := FIsValid;
  end;
end;

{$IFNDEF NODBACCESS}
procedure TPgSQLConnection.ReturnToPool;
begin
  inherited;

  FOnNotice := nil;
  FOnNotification := nil;
end;
{$ENDIF}

procedure TPgSQLConnection.Connect(const ConnectString: string);

  procedure InternalConnect(ProtocolClass: TPgSQLProtocolClass;
    SuppressError: boolean);
  var
    Port: integer;
  begin
    if FProtocol = nil then
      FProtocol := ProtocolClass.Create
    else
      FProtocol.Reset;
    FProtocol.SetUseUnicode(FUseUnicode);
    FProtocol.MessagesCharset := FMessagesCharset;
    try
      FProtocol.SetSSLOptions(_TSSLMode(FSSLMode),
        {$IFNDEF LITE}FIOHandler, FHttpOptions, FProxyOptions, {$ELSE}nil, nil, nil,{$ENDIF}
        FSSLOptions);

      Port := FPort;
      if Port = 0 then
        if not FRedhisftConnection then
          Port := PgDefValPort
        else
          Port := RsDefValPort;

      FProtocol.Connect(FServer, Port, FConnectionTimeout,
        FDatabase, FUserName, FPassword, FApplicationName, FIPVersion);
    except
      on E: Exception do begin
        FProtocol.Free;
        FProtocol := nil;
        if E is EPgError then begin
          if not SuppressError then
            ProcessInternalException(EPgError(E), Component);
        end
        else
          raise;
      end;
    end;
  end;

begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' BEGIN CONNECT Connection $' + IntToHex(Integer(Self), 8)));
{$ENDIF}

  if FConnected then
    Exit;

  try
    case FProtocolVersion of
      pv20:
        InternalConnect(TPgSQLProtocol20, False);
      pv30,
      pvAuto:
        InternalConnect(TPgSQLProtocol30, False);
    else
      Assert(False);
    end;

    if FRedhisftConnection <> FProtocol.IsRedshiftServer then
      if FRedhisftConnection then
        raise Exception.Create(SPostreSQLNotSupported)
      else
        raise Exception.Create(SRedshiftNotSupported);

    FConnected := True;

    FProtocol.SetImmediateNotices(FImmediateNotices);
    FProtocol.OnNotice := DoOnNotice;
    FProtocol.OnNotification := DoOnNotification;

    SetClientEncoding;
    if not FRedhisftConnection then
      GetIntegerDateTimes
    else
      FIntegerDateTimes := True;
    GetByteaOutput;

    if FSchema <> '' then
      SetCurrentSchema(FSchema);

    inherited;

  except
    on EFailOver do;
    else begin
      try
        Disconnect;
      except
      end;
      raise;
    end
  end;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' END CONNECT Connection $' + IntToHex(Integer(Self), 8)));
{$ENDIF}
end;

procedure TPgSQLConnection.Disconnect;
begin
  if not FConnected then
    Exit;

  FCachedSchema := '';
  FConnected := False;

  if FNativeConnection then
    if (FProtocol <> nil) and (FProtocol.Net.Vio <> nil) and (FProtocol.Net.Vio.Connected) then
      FProtocol.Disconnect;

  ReleaseSwapConnection;
  CloseExtFieldInfoConnection;

  inherited;
end;

procedure TPgSQLConnection.Ping;
begin
  FProtocol.Ping;
end;

procedure TPgSQLConnection.Assign(Source: TCRConnection);
var
  Src: TPgSQLConnection;
begin
  inherited;

  Src := TPgSQLConnection(Source);
  FDatabase := Src.FDatabase;
  FPort := Src.FPort;
  FProtocolVersion := Src.FProtocolVersion;
  FConnectionTimeout := Src.FConnectionTimeout;
  FSSLMode := Src.FSSLMode;
  FUseUnicode := Src.FUseUnicode;
  FCharset := Src.FCharset;
  FMessagesCharset := Src.FMessagesCharset;
  FSchema := Src.FSchema;
  FEnablePgTimeStamps := Src.FEnablePgTimeStamps;
  FIntervalAsString := Src.FIntervalAsString;
  FEnableGeometrics := Src.FEnableGeometrics;
  FEnableComposites := Src.FEnableComposites;
  FEnableDomains := Src.FEnableDomains;
  FBCDPrecision := Src.FBCDPrecision;
  FBCDScale := Src.FBCDScale;
  FApplicationName := Src.FApplicationName;
  FIPVersion := Src.FIPVersion;
  FIntegerDateTimes := Src.FIntegerDateTimes;
  FImmediateNotices := Src.FImmediateNotices;
  FRedhisftConnection := Src.FRedhisftConnection;
  FMultipleConnections := Src.FMultipleConnections;
end;

procedure TPgSQLConnection.AssignConnect(Source: TCRConnection);
var
  Src: TPgSQLConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TPgSQLConnection(Source);
      Assign(Src);
      FProtocol := Src.FProtocol;
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := (FProtocol <> nil) and (FProtocol.Net <> nil) and (FProtocol.Net.Vio <> nil);
      FNativeConnection := False;
    end;
  end;
end;

procedure TPgSQLConnection.SetServer(const Value: string);
begin
  if Value <> FServer then begin
    inherited;

    if FSSLMode = smVerifyFull then
      FSSLOptions.IdentityDNSName := FServer;
  end;
end;

function TPgSQLConnection.GetSchemaNamesForSQL(Flag: integer): string;
const
  ListDelimiters: array[0..1] of string = (',', ' ');
var
  i: integer;
  s: string;
begin
  Result := '';
  for i := Low(FSchemas) to High(FSchemas) do begin
    if Result <> '' then
      Result := Result + ListDelimiters[Flag];
    s := PgSQLInfo.Quote(FSchemas[i], char(''''), char(''''));
    case Flag of
      0: Result := Result + s;
      1: Result := Result + 'WHEN ' + s + ' THEN ' + IntToStr(i);
    end;
  end;
end;

procedure TPgSQLConnection.InitCommandProp(Command: TCRCommand);
begin
  Command.SetProp(prSimpleQueryExecute, True);
end;

procedure TPgSQLConnection.InitRecordSetProp(RecordSet: TCRRecordSet);
begin
  RecordSet.SetProp(prSimpleQueryExecute, True);
  RecordSet.SetProp(prExtendedFieldsInfo, False);
end;

function TPgSQLConnection.GetServerVersion: string;
begin
  Result := FProtocol.ServerVersion;
end;

function TPgSQLConnection.GetServerVersionFull: string;
begin
  Result := FProtocol.ServerVersionFull;
end;

function TPgSQLConnection.GetClientVersion: string;
begin
  Result := '8.0 Direct';
end;

function TPgSQLConnection.GetMajorServerVersion: integer;
begin
  if GetConnected then
    Result := FProtocol.MajorServerVersion
  else
    Result := 0;
end;

function TPgSQLConnection.GetMinorServerVersion: integer;
begin
  if GetConnected then
    Result := FProtocol.MinorServerVersion
  else
    Result := 0;
end;

function TPgSQLConnection.VersionIsEqualOrHigher(MajorVer, MinorVer: integer): boolean;
begin
  Result := (GetMajorServerVersion > MajorVer) or
    ((GetMajorServerVersion = MajorVer) and (GetMinorServerVersion >= MinorVer));
end;

function TPgSQLConnection.GetProcessID: integer;
begin
  Assert(FProtocol <> nil);
  Result := FProtocol.GetBackendPID;
end;

function TPgSQLConnection.GetProtocolVersion: TProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TPgSQLConnection.GetProtocol: TPgSQLProtocol;
begin
  Result := FProtocol;
end;

function TPgSQLConnection.GetTypes: TPgSQLTypes;
begin
  Result := FTypes;
end;

function TPgSQLConnection.CanChangeDatabase: boolean;
begin
  Result := False;
end;

procedure TPgSQLConnection.BreakExec;
begin
  if FProtocol <> nil then
    FProtocol.RequestCancel;
end;

function TPgSQLConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TPgSQLCommand;
end;

function TPgSQLConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TPgSQLRecordSet;
end;

function TPgSQLConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TPgSQLTransaction;
end;

{$IFNDEF LITE}

function TPgSQLConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TPgSQLLoader;
end;

function TPgSQLConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TPgSQLMetaData;
end;

class function TPgSQLConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TPgSQLCommand.GetMapRulesClass;
end;

function TPgSQLConnection.CreateObject(DataType: Word; Transaction: TCRTransaction): TDBObject;
begin
  case DataType of
    dtObject:
      Result := TPgRow.Create(nil);
    else
      raise Exception.Create('Invalid object type');
  end;
end;

{$ENDIF}

procedure TPgSQLConnection.ProcessInternalException(E: EPgError; Component: TObject);
var
  PgError: EPgError;
  Fail: boolean;
begin
  PgError := EPgError.Create(E.Severity, E.ErrorCode, E.Message, E.DetailMsg, E.Hint, E.CallStack,
    E.FileName, E.ProcedureName, E.Position, E.LineNumber);
{$IFNDEF NODBACCESS}
  PgError.Component := Component;
{$ENDIF}
  try
    E := PgError;
    Fail := True;
    DoError(E, Fail);
    if Fail then
      PgError := nil; // don't free
  finally
    PgError.Free;
  end;

  if Fail then
    raise E
  else
    Abort;
end;

function TPgSQLConnection.GetNewConnection: TPgSQLConnection;
begin
{$IFNDEF NODBACCESS}
  if Assigned(GetPooledConnection) then
    Result := TPgSQLConnection(GetPooledConnection)
  else
{$ENDIF}
    Result := TPgSQLConnection.Create;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' GetNewConnection = $' + IntToHex(Integer(Result), 8)));
{$ENDIF}

  if not Result.FConnected then begin
    Result.Assign(Self);
    Result.OnReconnectSuccess := Self.OnReconnectSuccess;
    Result.OnReconnectError := Self.OnReconnectError;
    Result.OnError := Self.OnError;
    Result.OnNotice := Self.OnNotice;
    Result.OnNotification := Self.OnNotification;
  end;
end;

function TPgSQLConnection.QuerySwapConnection: TPgSQLConnection;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Connection $' + IntToHex(Integer(Self), 8) + ' BEGIN QUERY CONNECTION'));
{$ENDIF}

  FLock.Enter;
  try
    if FSwapConnection <> nil then begin
      Result := FSwapConnection;
      FSwapConnection := nil;
    end
    else
      Result := GetNewConnection;

    if not Result.FConnected then
      Result.Connect('');
  finally
    FLock.Leave;
  end;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Connection $' + IntToHex(Integer(Self), 8) + ' END QUERY CONNECTION = $' + IntToHex(Integer(Result), 8)));
{$ENDIF}
end;

procedure TPgSQLConnection.ReturnSwapConnection(var Value: TPgSQLConnection);
begin
  if Value = nil then
    Exit;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Connection $' + IntToHex(Integer(Self), 8) + ' BEGIN RETURN CONNECTION $' + IntToHex(Integer(Value), 8)));
{$ENDIF}
  FLock.Enter;
  try
  {$IFNDEF NODBACCESS}
    if Value.Pool <> nil then
      try
        Value.ReturnToPool;
      finally
        Value := nil;
      end
    else
  {$ENDIF}
    begin
      if Value.IsValid and (FSwapConnection = nil) then begin
        FSwapConnection := Value;
        Value := nil;
      end
      else
        FreeAndNil(Value);

      ReleaseSwapConnection;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TPgSQLConnection.ReleaseSwapConnection;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Connection $' + IntToHex(Integer(Self), 8) + ' BEGIN RELEASE CONNECTION $' + IntToHex(Integer(FSwapConnection), 8)));
{$ENDIF}

  FLock.Enter;
  try
    if not FConnected then
      FreeAndNil(FSwapConnection);
  finally
    FLock.Leave;
  end;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Connection $' + IntToHex(Integer(Self), 8) + ' END RELEASE CONNECTION $' + IntToHex(Integer(FSwapConnection), 8)));
{$ENDIF}
end;

function TPgSQLConnection.GetExtFieldInfoThread(TablesInfo: TCRTablesInfo; DefaultValues, UseProtocol30: Boolean): TPgExtFieldsInfoThread;
begin
  // if single connection then use GetExtFieldsInfo
  if not FMultipleConnections then begin
    Result := nil;
    Exit;
  end;

  FLock.Enter;
  try
    if FExtFieldInfoThread = nil then begin
      FExtFieldInfoConnection := TPgSQLConnection.Create;
      FExtFieldInfoThread := TPgExtFieldsInfoThread.Create(FExtFieldInfoConnection);
    end;

    if not FExtFieldInfoConnection.FConnected then begin
      FExtFieldInfoConnection.Assign(Self);
      FExtFieldInfoConnection.FMultipleConnections := False;
      FExtFieldInfoConnection.Connect('');
    end;

    if FExtFieldInfoThread.Run(TablesInfo, DefaultValues, UseProtocol30) then
      Result := FExtFieldInfoThread
    else
      Result := nil; // if thread is busy then use GetExtFieldsInfo
  finally
    FLock.Leave;
  end;
end;

procedure TPgSQLConnection.ReleaseExtFieldInfoThread(var Value: TPgExtFieldsInfoThread);
begin
  if Value = nil then
    Exit;

  FLock.Enter;
  try
    if Value = FExtFieldInfoThread then
      try
        Value := nil;
        FExtFieldInfoThread.Release;
      except
        TerminateExtFieldInfoThread(FExtFieldInfoThread);
        raise;
      end
    else
      TerminateExtFieldInfoThread(Value);
  finally
    FLock.Leave;
  end;
end;

procedure TPgSQLConnection.TerminateExtFieldInfoThread(var Value: TPgExtFieldsInfoThread);
begin
  if Value <> nil then begin
    Value.Terminate;
    Value.Run(nil, False, False); // run for FreeOnTerminate
    Value := nil;
  end;
end;

procedure TPgSQLConnection.CloseExtFieldInfoConnection;
begin
  FLock.Enter;
  try
    if FExtFieldInfoThread <> nil then
      FExtFieldInfoThread.CloseConnection;
  finally
    FLock.Leave;
  end;
end;

procedure TPgSQLConnection.SetClientEncoding;

  function GetCharLength: integer;
  begin
    Result := 1;

    if not FUseUnicode then
      case FCharsetID of
        PG_BIG5,
        PG_GBK,
        PG_SJIS,
        PG_UHC: Result := 2;
        PG_EUC_JP,
        PG_EUC_JIS_2004,
        PG_EUC_CN,
        PG_EUC_KR,
        PG_EUC_TW,
        PG_JOHAB: Result := 3;
        PG_GB18030,
        PG_UTF8: Result := 4;
      end;
  end;

var
  SQL: string;
  DefaultEnc,
  Encoding: string;
begin
  SQL := '';
  DefaultEnc := UpperCase(FProtocol.GetServerParameter('client_encoding'));
  if FUseUnicode then begin
    if VersionIsEqualOrHigher(8, 1) then begin
      if DefaultEnc <> 'UTF8' then
        SQL := 'SET client_encoding=''UTF8''';
    end
    else begin
      if DefaultEnc <> 'UNICODE' then
        SQL := 'SET client_encoding=''UNICODE''';
    end;
  end
  else begin
    Encoding := FCharset;

  {$IFNDEF FPC}
    {$IFDEF MSWINDOWS}
      if Encoding = '' then
        Encoding := DetectEncoding;
    {$ELSE}
      {$IFDEF LINUX}
        if Encoding = '' then
          Encoding := 'UTF8';
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    if Encoding = '' then
      Encoding := 'UTF8';
  {$ENDIF}

    if (Encoding <> '') and (UpperCase(Encoding) <> DefaultEnc) then
      SQL := Format('SET client_encoding=''%s''', [Encoding]);
  end;

  if SQL <> '' then
    ExecuteSQL(SQL);

  FCharsetID := EncodingToCodePage(Encoding);
  FCharLength := GetCharLength;
end;

procedure TPgSQLConnection.GetIntegerDateTimes;
var
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
begin
  if not VersionIsEqualOrHigher(8,0) then begin
    FIntegerDateTimes := False;
    Exit;
  end;

  RecordSet := GetRecordSet;
  try
    RecordSet.SetSQL('show integer_datetimes');
    RecordSet.Open;

    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        FIntegerDateTimes := False
      else begin
        RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
        FIntegerDateTimes := LowerCase(VarToStr(v)) = 'on';
      end;
    finally
      RecordSet.FreeRecBuf(RecBuf);
    end;
  finally
    ReleaseRecordset(RecordSet);
  end;
end;

procedure TPgSQLConnection.GetByteaOutput;
var
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
begin
  if not VersionIsEqualOrHigher(9,0) then begin
    FByteaOutputAsHex := False;
    Exit;
  end;

  RecordSet := GetRecordSet;
  try
    RecordSet.SetConnection(Self);
    RecordSet.SetSQL('show bytea_output');
    RecordSet.Open;

    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        FByteaOutputAsHex := False
      else begin
        RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
        FByteaOutputAsHex := LowerCase(VarToStr(v)) = 'hex';
      end;
    finally
      RecordSet.FreeRecBuf(RecBuf);
    end;
  finally
    ReleaseRecordset(RecordSet);
  end;
end;

function TPgSQLConnection.GetCurrentSchema: string;
var
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  RecordSet := GetRecordSet;
  try
    RecordSet.SetSQL('SELECT CURRENT_SCHEMA()');
    RecordSet.Open;

    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        Result := ''
      else begin
        RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
        Result := VarToStr(v);
      end;
    finally
      RecordSet.FreeRecBuf(RecBuf);
    end;
  finally
    ReleaseRecordset(RecordSet);
  end;
end;

function TPgSQLConnection.GetCachedSchema: string;
begin
  if FCachedSchema = '' then
    if FSchema <> '' then
      FCachedSchema := FSchema
    else
      FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;

procedure TPgSQLConnection.SetCurrentSchema(Value: string);
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if Value = '' then begin
    Value := 'public';
    SetLength(FSchemas, 1);
    FSchemas[0] := Value;
  end
  else
    Value := PgSQLInfo.QuoteSchemasIfNeed(Value, FSchemas);

  ExecuteSQL('SET search_path TO ' + Value);
  FSchema := Value;
  FCachedSchema := '';
  FMultipleSchema := Length(FSchemas) > 1;
end;

function TPgSQLConnection.GetInTransaction: boolean;
begin
  Result := FInTransaction;
end;

{$IFNDEF LITE}
function TPgSQLConnection.GetNotificationsHandler: TPgSQLNotificationsHandler;
begin
  if FNotificationsHandler = nil then
    FNotificationsHandler := TPgSQLNotificationsHandler.Create(Self);

  Result := FNotificationsHandler;
end;
{$ENDIF}

procedure TPgSQLConnection.DoOnNotice(Errors: TPgErrors);
begin
  if Assigned(FOnNotice) then
    FOnNotice(Errors);
end;

procedure TPgSQLConnection.DoOnNotification(const Name: string; const PID: integer; const Message: string);
begin
  if Assigned(FOnNotification) then
    FOnNotification(Name, PID, Message);
end;

{ TPgSQLTransaction }

constructor TPgSQLTransaction.Create;
begin
  inherited;

  FImplicitlyStarted := False;
{$IFDEF DEBUG_MULTITHREAD}
  FStartCount := 0;
{$ENDIF}
end;

destructor TPgSQLTransaction.Destroy;
begin
  FCommand.Free;

  inherited;

{$IFDEF DEBUG_MULTITHREAD}
  if FStartCount <> 0 then
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + 'ERROR Transaction $' + IntToHex(Integer(Self), 8) + ' StartCount = ' + IntToStr(FStartCount)));
{$ENDIF}
end;

procedure TPgSQLTransaction.CheckCommand;
begin
  if FCommand = nil then
    FCommand := TPgSQLConnection(FConnections[0]).CreateCommand as TPgSQLCommand;
end;

procedure TPgSQLTransaction.ExecuteSQL(const SQL: string);
begin
  FCommand.SetSQL(SQL);
  FCommand.Execute;
end;

procedure TPgSQLTransaction.StartTransaction;
var
  CommandText: string;
  Connection: TPgSQLConnection;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  Connection := TPgSQLConnection(FConnections[0]);

  if not Connection.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);

  CommandText := 'BEGIN; SET TRANSACTION ISOLATION LEVEL ';

  case FIsolationLevel of
    ilReadCommitted:
      CommandText := CommandText + 'READ COMMITTED';
    ilIsolated, ilSnapshot:
      CommandText := CommandText + 'SERIALIZABLE';
    ilRepeatableRead:
      CommandText := CommandText + 'REPEATABLE READ';
    ilReadUncommitted:
      CommandText := CommandText + 'READ UNCOMMITTED';
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;

  if FReadOnly then
    CommandText := CommandText + ' READ ONLY';

{$IFDEF DEBUG_MULTITHREAD}
  InterlockedIncrement(FStartCount);
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Transaction $' + IntToHex(Integer(Self), 8) + ' START'));
{$ENDIF}

  CheckCommand;
  ExecuteSQL(CommandText);

  FActive := True;
  FNativeTransaction := True;
  Connection.FInTransaction := True;
end;

procedure TPgSQLTransaction.StartImplicitTransaction;
begin
  FImplicitlyStarted := True;

  StartTransaction;
end;

procedure TPgSQLTransaction.Commit;
var
  Connection: TPgSQLConnection;
begin
  CheckActive;

{$IFDEF DEBUG_MULTITHREAD}
  InterlockedDecrement(FStartCount);
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Transaction $' + IntToHex(Integer(Self), 8) + ' COMMIT'));
{$ENDIF}

  Connection := TPgSQLConnection(FConnections[0]);
  if FNativeTransaction then
    ExecuteSQL('COMMIT');

  FActive := False;
  FImplicitlyStarted := False;
  Connection.FInTransaction := False;
end;

procedure TPgSQLTransaction.Rollback;
var
  Connection: TPgSQLConnection;
begin
  CheckActive;

{$IFDEF DEBUG_MULTITHREAD}
  InterlockedDecrement(FStartCount);
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Transaction $' + IntToHex(Integer(Self), 8) + ' ROLLBACK'));
{$ENDIF}

  Connection := TPgSQLConnection(FConnections[0]);
  if FNativeTransaction then
    ExecuteSQL('ROLLBACK');

  FActive := False;
  FImplicitlyStarted := False;
  Connection.FInTransaction := False;
end;

procedure TPgSQLTransaction.Savepoint(const Name: string);
begin
  CheckActive;

  CheckCommand;
  ExecuteSQL('SAVEPOINT ' + Name);
end;

procedure TPgSQLTransaction.ReleaseSavepoint(const Name: string);
begin
  CheckActive;

  ExecuteSQL('RELEASE SAVEPOINT ' + Name);
end;

procedure TPgSQLTransaction.RollbackToSavepoint(const Name: string);
begin
  CheckActive;

  ExecuteSQL('ROLLBACK TO SAVEPOINT ' + Name);
end;

{ TPgSQLTypes }

constructor TPgSQLTypes.Create(Connection: TPgSQLConnection);
begin
  inherited Create;

  FConnection := Connection;
{$IFNDEF LITE}
  FUnknownTypes := TThreadList.Create;
  FTypes := TThreadList.Create;
{$ENDIF}
end;

destructor TPgSQLTypes.Destroy;
{$IFNDEF LITE}
var
  i: integer;
  List: TList;
{$ENDIF}
begin
{$IFNDEF LITE}
  FUnknownTypes.Free;
  List := FTypes.LockList;
  for i := 0 to List.Count - 1 do
    TPgType(List[i]).Free;
  FTypes.UnlockList;
  FTypes.Free;
{$ENDIF}

  inherited;
end;

procedure TPgSQLTypes.DecodeTypeLength(TypeOID: Integer; TypeModifier: Integer; var Length, Scale: Integer);
begin
  Length := 0;
  Scale := 0;

  case TypeOID of
    SQL_PG_CHAR, SQL_PG_VARCHAR:
      if TypeModifier <> -1 then
        Length := (TypeModifier - 4) * FConnection.FCharLength;
    SQL_PG_NUMERIC:
      if TypeModifier <> -1 then begin
        Length := (TypeModifier - 4) shr 16 and $ffff;
        Scale := (TypeModifier - 4) and $ffff;
      end;
    SQL_PG_TIME, SQL_PG_TIMETZ, SQL_PG_TIMESTAMP, SQL_PG_TIMESTAMPTZ:
      if TypeModifier < 0 then
        Scale := 0
      else
        Scale := TypeModifier - 6;
    SQL_PG_BIT, SQL_PG_VARBIT:
      if TypeModifier <> -1 then
        Length := TypeModifier;
  end;
end;

procedure TPgSQLTypes.DetectDataType(TypeOID: integer; {$IFNDEF LITE}FetchConverter: TFetchConverter;{$ENDIF}
  var DataType, SubDataType: word; var Length, Scale, Size: integer;
  var Fixed: boolean; var ObjectType: TObjectType;
  LongStrings, FlatBuffers, OIDAsInt, EnableBCD, EnableFMTBCD,
  CursorAsString, UnknownAsString, FieldsAsText: boolean);
const
  dsMaxStringSize = 8192;
begin
  ObjectType := nil;

  Fixed := TypeOID = SQL_PG_CHAR;

{$IFNDEF LITE}
  if FetchConverter <> nil then begin
    DataType := FetchConverter.InternalDataType;
    SubDataType := GetInternalType(TypeOID, ObjectType);
  end
  else
{$ENDIF}
  begin
    DataType := GetInternalType(TypeOID, ObjectType);

    SubDataType := DataType;

    if (SubDataType = dtNumeric) and ((Length + Scale) = 0) then
      SubDataType := dtNumericFloating;

    case DataType of
      dtFixedChar, dtPgName, dtPgChar, dtVoid, dtPgBit, dtPgVarBit:
        DataType := dtString;
    else
      if FieldsAsText then
        DataType := dtString
      else begin
        case DataType of
          dtUnknown:
            DataType := dtString;
          dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine:
            if not FConnection.FEnableGeometrics then
              DataType := dtString;
          dtPgInterval:
            if FConnection.FIntervalAsString then
              DataType := dtString;
          dtCursor:
            if CursorAsString then
              DataType := dtString;
        end;
      end;
    end;

    case DataType of
      dtString: begin
        if (Length = 0) and not(SubDataType in [dtPgName, dtPgChar, dtVoid, dtCursor]) then begin
          if UnknownAsString then
            Length := dsMaxStringSize - 1 // TODO: trim on fetch
          else
            DataType := dtMemo;
        end;
        if DataType <> dtMemo then begin
          if (Length > High(Word)) or (not LongStrings and (Length > 255)) then
            DataType := dtMemo;
        end;
        if FConnection.FUseUnicode then
          case DataType of
            dtString:
              DataType := dtWideString;
            dtExtString:
              DataType := dtExtWideString;
            dtMemo: begin
              DataType := dtWideMemo;
              if SubDataType = dtString then
                SubDataType := dtWideString;
            end;
          end;
      end;
      dtPgSingle:
        DataType := dtFloat;
      dtNumeric,
      dtNumericFloating: begin
      {$IFDEF LITE}
        if EnableFMTBCD or FConnection.EnableFMTBCD then begin
           if (FConnection.FBCDPrecision > 0) and ((Length + Scale) <= FConnection.FBCDPrecision) and (Scale <= FConnection.FBCDScale) then
            DataType := dtBCD
          else
            DataType := dtFMTBCD;
        end
        else
          DataType := dtFloat;
      {$ELSE}
        if (EnableBCD or FConnection.EnableBCD) and (FConnection.FBCDPrecision > 0)
        and ((Length + Scale) <= FConnection.FBCDPrecision) and (Scale <= FConnection.FBCDScale) then
            DataType := dtBCD
        else if EnableFMTBCD or FConnection.EnableFMTBCD then
          DataType := dtFMTBCD
        else if EnableBCD or FConnection.EnableBCD then
          DataType := dtBCD
        else
          DataType := dtFloat;
      {$ENDIF}
      end;
      dtPgDate:
        if not FConnection.FEnablePgTimeStamps then
          DataType := dtDate;
      dtPgTime:
        if not FConnection.FEnablePgTimeStamps then
          DataType := dtTime;
      dtPgTimeTZ:
        if FConnection.FEnablePgTimeStamps then
          DataType := dtPgTime
        else
          DataType := dtTime;
      dtPgTimeStamp:
        if not FConnection.FEnablePgTimeStamps then
          DataType := dtDateTime;
      dtPgTimeStampTZ:
        if FConnection.FEnablePgTimeStamps then
          DataType := dtPgTimeStamp
        else
          DataType := dtDateTime;
      dtOID:
        if OIDAsInt then
          DataType := dtInteger
        else
          DataType := dtPgLargeObject;
      dtMemo:
        if FConnection.FUseUnicode then
          DataType := dtWideMemo;
    {$IFDEF LITE}
      dtLargeint:
        if not (EnableFMTBCD or FConnection.EnableFMTBCD) then
          DataType := dtFloat;
    {$ENDIF}
      dtPgJsonb:
        if FConnection.FUseUnicode then
          DataType := dtWideMemo
        else
          DataType := dtMemo;
    end;
  end;

  case SubDataType of
    dtVoid:
      Length := 0;
    dtPgName:
      if FConnection.FRedhisftConnection then
        Length := 127
      else
        Length := 63;
    dtPgChar:
      Length := 1;
    dtGuid:
      Length := 38;
    dtCursor:
      Length := 63;
  {$IFDEF FPC}
    dtPgSingle,
    dtFloat:
      Length := 15;
  {$ENDIF}
    dtNumeric,
    dtNumericFloating: begin
    {$IFDEF LITE}
      if Length = 0 then begin
        Length := 32;
        Scale := 8;
      end;
    {$ENDIF}
      if DataType = dtFMTBCD then begin
        if Length > MaxFMTBcdDigits then
          Length := MaxFMTBcdDigits;
        if Scale > Length then // if length was reduced
          Scale := Length;
      end
      else
      if DataType = dtBCD then begin
        if Length > MaxBcdPrecision then
          Length := MaxBcdPrecision;
        if Scale > MaxBcdScale then
          Scale := MaxBcdScale;
      end;
    end;
  end;

  if not FlatBuffers then
    if ((SubDataType in [dtString, dtWideString]) or ((SubDataType in [dtUnknown, dtFixedChar]) and UnknownAsString)) and (Length >= FlatBufferLimit) then
      if DataType = dtString then
        DataType := dtExtString
      else if DataType = dtWideString then
        DataType := dtExtWideString;

  Size := TPgSQLRecordSet.GetBufferSize(DataType, Length);
end;

function TPgSQLTypes.GetInternalType(TypeCode: integer; var ObjectType: TObjectType): word;
{$IFNDEF LITE}
var
  DomainType: TObjectType;
{$ENDIF}
begin
  ObjectType := nil;
  case TypeCode of
    SQL_PG_VOID:
      Result := dtVoid;
    SQL_PG_CHAR:
      Result := dtFixedChar;
    SQL_PG_VARCHAR:
      Result := dtString;
    SQL_PG_NAME:
      Result := dtPgName;
    SQL_PG_CHARACTER:
      Result := dtPgChar;
    SQL_PG_SMALLINT:
      Result := dtSmallInt;
    SQL_PG_INT:
      Result := dtInteger;
    SQL_PG_BIGINT:
      Result := dtLargeInt;
    SQL_PG_REAL:
      Result := dtPgSingle;
    SQL_PG_DOUBLE:
      Result := dtFloat;
    SQL_PG_NUMERIC:
      Result := dtNumeric;
    SQL_PG_MONEY:
      Result := dtCurrency;
    SQL_PG_TEXT:
      Result := dtMemo;
    SQL_PG_BYTEA:
      Result := dtBlob;
    SQL_PG_DATE:
      Result := dtPgDate;
    SQL_PG_TIME:
      Result := dtPgTime;
    SQL_PG_TIMETZ:
      Result := dtPgTimeTZ;
    SQL_PG_TIMESTAMP:
      Result := dtPgTimeStamp;
    SQL_PG_TIMESTAMPTZ:
      Result := dtPgTimeStampTZ;
    SQL_PG_INTERVAL:
      Result := dtPgInterval;
    SQL_PG_OID:
      Result := dtOID;
    SQL_PG_BOOLEAN:
      Result := dtBoolean;
    SQL_PG_POINT:
      Result := dtPgPoint;
    SQL_PG_LSEG:
      Result := dtPgLSeg;
    SQL_PG_BOX:
      Result := dtPgBox;
    SQL_PG_PATH:
      Result := dtPgPath;
    SQL_PG_POLYGON:
      Result := dtPgPolygon;
    SQL_PG_CIRCLE:
      Result := dtPgCircle;
    SQL_PG_LINE:
      Result := dtPgLine;
    SQL_PG_REFCURSOR:
      Result := dtCursor;
    SQL_PG_UUID:
      Result := dtGuid;
    SQL_PG_BIT:
      Result := dtPgBit;
    SQL_PG_VARBIT:
      Result := dtPgVarBit;
    SQL_PG_JSONB:
      Result := dtPgJsonb;
  else
  {$IFNDEF LITE}
    if FConnection.FEnableComposites or FConnection.FEnableDomains then begin
      case TypeCode of
        PG_TYPE_RELTYPE_OID, PG_ATTRIBUTE_RELTYPE_OID, PG_PROC_RELTYPE_OID, PG_CLASS_RELTYPE_OID: ;
      else
        if (TypeCode < 10000) or IsUnknownType(TypeCode) then begin
          Result := dtUnknown;
          Exit;
        end;
      end;

      if FConnection.FEnableComposites then
        ObjectType := GetRowType(TypeCode);
      if (ObjectType <> nil) and TPgType(ObjectType).IsDescribed then
        Result := dtObject
      else if FConnection.FEnableDomains then begin
        DomainType := GetDomainType(TypeCode);
        if (DomainType <> nil) and TPgDomainType(DomainType).IsDescribed then begin
          Result := GetInternalType(TPgDomainType(DomainType).BaseTypeOID, ObjectType);
          Exit;
        end
        else begin
          FUnknownTypes.Add(pointer({$IFNDEF FPC}TypeCode{$ELSE}(@TypeCode)^{$ENDIF}));
          Result := dtUnknown;
        end;
      end
      else begin
        FUnknownTypes.Add(pointer({$IFNDEF FPC}TypeCode{$ELSE}(@TypeCode)^{$ENDIF}));
        Result := dtUnknown;
      end;
    end
    else
  {$ENDIF}
      Result := dtUnknown;
  end;
end;

function TPgSQLTypes.DataTypeToOID(DataType, SubDataType: word): integer;
begin
  case DataType of
    dtSmallInt, dtInt8:
      Result := SQL_PG_SMALLINT;
    dtInteger:
      if SubDataType = dtOID then
        Result := SQL_PG_OID
      else
        Result := SQL_PG_INT;
    dtWord:
      Result := SQL_PG_INT;
    dtLargeInt, dtLongword:
      Result := SQL_PG_BIGINT;
    dtFloat:
      Result := SQL_PG_DOUBLE;
    dtBCD, dtFMTBCD, dtNumeric, dtNumericFloating:
      Result := SQL_PG_NUMERIC;
    dtCurrency:
      Result := SQL_PG_MONEY;
    dtBoolean:
      Result := SQL_PG_BOOLEAN;
    dtMemo, dtWideMemo:
      Result := SQL_PG_TEXT;
    dtBlob:
      Result := SQL_PG_BYTEA;
    dtDate, dtPgDate:
      Result := SQL_PG_DATE;
    dtTime, dtPgTime:
      if SubDataType = dtPgTimeTZ then
        Result := SQL_PG_TIMETZ
      else
        Result := SQL_PG_TIME;
    dtPgTimeTZ:
      Result := SQL_PG_TIMETZ;
    dtDateTime, dtPgTimeStamp:
      if SubDataType = dtPgTimeStampTZ then
        Result := SQL_PG_TIMESTAMPTZ
      else
        Result := SQL_PG_TIMESTAMP;
    dtPgTimeStampTZ:
      Result := SQL_PG_TIMESTAMPTZ;
    dtPgInterval:
      Result := SQL_PG_INTERVAL;
    dtPgPoint:
      Result := SQL_PG_POINT;
    dtPgLSeg:
      Result := SQL_PG_LSEG;
    dtPgBox:
      Result := SQL_PG_BOX;
    dtPgPath:
      Result := SQL_PG_PATH;
    dtPgPolygon:
      Result := SQL_PG_POLYGON;
    dtPgCircle:
      Result := SQL_PG_CIRCLE;
    dtPgLine:
      Result := SQL_PG_LINE;
    dtCursor:
      Result := SQL_PG_REFCURSOR;
    dtPgLargeObject:
      Result := SQL_PG_OID;
    dtGuid:
      Result := SQL_PG_UUID;
  else
    Result := SQL_PG_VARCHAR;
  end;
end;

{$IFNDEF LITE}
function TPgSQLTypes.IsUnknownType(TypeCode: integer): boolean;
var
  i: integer;
  List: TList;
begin
  Result := False;

  List := FUnknownTypes.LockList;
  try
    for i := 0 to List.Count - 1 do
      if Integer(NativeUInt(List.Items[i])) = TypeCode then begin
        Result := True;
        exit;
      end;
  finally
    FUnknownTypes.UnlockList;
  end;
end;

function TPgSQLTypes.FindType(TypeCode: integer; TypeClass: TClass): TObjectType;
var
  i: integer;
  List: TList;
  t: TPgType;
begin
  Result := nil;

  List := FTypes.LockList;
  try
    for i := 0 to List.Count - 1 do
      if TObject(List.Items[i]) is TypeClass then begin
        t := TPgType(List.Items[i]);
        if t.TypeOID = TypeCode then begin
          Result := t;
          if t.IsDescribed then
            Exit;
        end;
      end;
  finally
    FTypes.UnlockList;
  end;
end;

function TPgSQLTypes.FindType(const TypeName: string; TypeClass: TClass): TObjectType;
var
  i: integer;
  List: TList;
  t: TPgType;
begin
  Result := nil;

  List := FTypes.LockList;
  try
    for i := 0 to List.Count - 1 do
      if TObject(List.Items[i]) is TypeClass then begin
        t := TPgType(List.Items[i]);
        if t.Name = TypeName then begin
          Result := t;
          exit;
        end;
      end;
  finally
    FTypes.UnlockList;
  end;
end;

function TPgSQLTypes.GetType(TypeCode: integer; TypeClass: TClass): TObjectType;
var
  Con: TPgSQLConnection;
  PgType: TPgType;
begin
  Result := FindType(TypeCode, TypeClass);

  if Result = nil then begin
    Con := FConnection.QuerySwapConnection;

    try
      if TypeClass = TPgDomainType then
        PgType := TPgDomainType.Create(TypeCode)
      else if TypeClass = TPgRowType then
        PgType := TPgRowType.Create(TypeCode)
      else
        PgType := TPgType.Create(TypeCode);

      try
        PgType.Describe(Con, TypeCode);
        FTypes.Add(PgType);
        Result := PgType;
      except
        PgType.Free;
        raise;
      end;
    finally
      FConnection.ReturnSwapConnection(Con);
    end;
  end;
end;

function TPgSQLTypes.GetType(const TypeName: string; TypeClass: TClass): TObjectType;
var
  Con: TPgSQLConnection;
  PgType: TPgType;
  Schema, Name, NormName: string;
  Info: TSQLObjectInfo;
begin
  PgSQLInfo.SplitObjectName(TypeName, Info);
  if Info.Schema <> '' then
    Schema := PgSQLInfo.NormalizeName(Info.Schema, True)
  else
    Schema := '"' + FConnection.GetCachedSchema + '"';
  Name := PgSQLInfo.NormalizeName(Info.Name, True);
  NormName := Schema + '.' + Name;
  Result := FindType(NormName, TypeClass);

  if Result = nil then begin
    Con := FConnection.QuerySwapConnection;

    try
      if TypeClass = TPgDomainType then
        PgType := TPgDomainType.Create
      else if TypeClass = TPgRowType then
        PgType := TPgRowType.Create
      else
        PgType := TPgType.Create;

      try
        if PgType.Describe(Con, TypeName) then begin
          FTypes.Add(PgType);
          Result := PgType;
        end
        else
          PgType.Free;
      except
        PgType.Free;
        raise;
      end;
    finally
      FConnection.ReturnSwapConnection(Con);
    end;
  end;
end;

function TPgSQLTypes.GetDomainType(TypeCode: integer): TObjectType;
begin
  Result := GetType(TypeCode, TPgDomainType);
end;

function TPgSQLTypes.GetRowType(TypeCode: integer): TObjectType;
begin
  Result := GetType(TypeCode, TPgRowType);
end;

function TPgSQLTypes.GetRowType(const TypeName: string): TObjectType;
begin
  Result := GetType(TypeName, TPgRowType);
end;
{$ENDIF NDEF LITE}

{ TPgSQLParamDesc }

procedure TPgSQLParamDesc.SetNull(const Value: boolean);
begin
  FIsNull := Value;

  if (TVarData(FData).VType <> varSharedObject) and (FArraySize = 1) then
    FData := Unassigned;
end;

function TPgSQLParamDesc.GetAsCursor: TPgCursor;
begin
  Result := GetObject as TPgCursor;
end;

procedure TPgSQLParamDesc.SetAsCursor(Value: TPgCursor);
begin
  SetObject(Value);
end;

{ TPgSQLInfo }

function TPgSQLInfo.HasOnlyLexem: boolean;
begin
  Result := True;
end;

procedure TPgSQLInfo.ParseExtColumnName(Parser: TSQLParser; var Code: integer; var Str: string);
begin
  inherited;

  // type convertion
  if Code = lxColon then begin
    Code := Parser.GetNext(Str);
    if Code = lxColon then begin
      Parser.GetNext(Str);
      Code := Parser.GetNext(Str);
    end
    else
      Parser.Back;
  end;
end;

function TPgSQLInfo.IdentCase: TIdentCase;
begin
  Result := icLower;
end;

function TPgSQLInfo.QuoteSchemasIfNeed(const Value: string; var Values: _TStringArray): string;
var
  Parser: TPgParser;
  Code: integer;
  St, SchemaName: string;
begin
  Result := '';
  SetLength(Values, 0);

  Parser := TPgParser.Create(Value);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    Parser.OmitKeywords := True;
    Parser.QuotedString := True;
    Parser.DecSeparator := '.';
    Parser.ToBegin;

    SchemaName := '';
    while True do begin
      Code := Parser.GetNext(St);

      if (Code = lxComma) or (Code = lcEnd) then begin
        if Result <> '' then
          Result := Result + ',';
        SchemaName := QuoteIfNeed(SchemaName);
        Result := Result + SchemaName;
        SetLength(Values, Length(Values) + 1);
        Values[High(Values)] := Unquote(SchemaName);
        if Code = lcEnd then
          break
        else
          SchemaName := '';
      end
      else
        SchemaName := SchemaName + St;
    end;
  finally
    Parser.Free;
  end;
end;

{ TPgCursor }

constructor TPgCursor.Create;
begin
  inherited;

  FNativeStatement := True;
end;

destructor TPgCursor.Destroy;
begin
  FreeStatement;

  inherited;
end;

procedure TPgCursor.SetStmtHandle(const Handle: TPgSQLStatement);
begin
  if Handle <> FStmtHandle then begin
    if FStmtHandle <> nil then
      FreeStatement;
    FStmtHandle := Handle;
    FNativeStatement := False;
  end;
end;

function TPgCursor.CanFetch: boolean;
begin
  Result := (FState > csInactive) and (FState < csFetched);
end;

procedure TPgCursor.CreateStatement;
begin
  FStmtHandle := TPgSQLStatement.Create;
end;

procedure TPgCursor.FreeStatement;
begin
  if FNativeStatement then
    FStmtHandle.Free;
  FStmtHandle := nil;
end;

{ TPgSQLCommand }

constructor TPgSQLCommand.Create;
begin
  inherited;

  FCursor := TPgCursor.Create;
  FCursorRef := FCursor;

  FDistinctParams := True;
  FForcedProtocol := pvAuto;
end;

destructor TPgSQLCommand.Destroy;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' before destroy Command $' + IntToHex(Integer(Self), 8)));
{$ENDIF}

{$IFDEF AUTOREFCOUNT}
  FCursorRef := nil;
  FNextCursorRef := nil;
{$ENDIF}
  FCursor.Free;
  if (FUsedConnection <> FConnection) and (FConnection <> nil) then begin
  {$IFDEF DEBUG_MULTITHREAD}
//    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' > release SwapConnection $' + IntToHex(Integer(FSwapConnection), 8)));
  {$ENDIF}
    ReleaseFetchConnection(False);
  end;

  inherited;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' after destroy Command $' + IntToHex(Integer(Self), 8)));
{$ENDIF}
end;

class function TPgSQLCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TPgSQLInfo;
end;

class function TPgSQLCommand.GetParserClass: TSQLParserClass;
begin
  Result := TPgParser;
end;

class function TPgSQLCommand.GetParamDescClass: TParamDescClass;
begin
  Result := TPgSQLParamDesc;
end;

{$IFNDEF LITE}
class function TPgSQLCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TPgMapRules;
end;
{$ENDIF}

function TPgSQLCommand.ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string;
var
  ParsedSQL: StringBuilder;
  Parser: TPgParser;
  Code, p: integer;
  St, TempSt: string;
  l: integer;
  ParamName: string;
  ParamIndex: integer;
  Param: TParamDesc;

  LeftQuote, RightQuote: char;
  IsParam: boolean;
  BracketsCount: integer;

  function IndexOfParam(const Name: string): integer;
  var
    i: integer;
  begin
    Result := -1;
    for i := 0 to Params.Count - 1 do
    if (Params.Items[i] <> nil) then
      if SameText(TParamDesc(Params[i]).GetName, Name) then begin
        Result := i;
        break;
      end;
  end;

begin
  Assert(Params <> nil);

  FParamsInfo.Clear;
  if FScanParams then
    Params.Clear;

  ParseSQLType(SQL);
  ParsedSQL := StringBuilder.Create(Length(SQL) + Length(SQL) div 2);
  try
    Parser := TPgParser.Create(SQL);
    try
      Parser.OmitBlank := False;
      Parser.OmitComment := False;
      Parser.QuotedString := True;
      Parser.Uppered := False;
      Parser.DecSeparator := '.';
      Parser.ToBegin;

      LeftQuote := '"';
      RightQuote := '"';

      BracketsCount := 0;
      FLeftmostBracket := 0;
      FRightmostBracket := 0;

      repeat
        IsParam := False;
        repeat
          Code := Parser.GetNext(St);

          // PostgreSQL conversion (::) workaround
          if (Code = lxColon)  then begin
            begin
              TempSt := St;
              Code := Parser.GetNext(St);
              if (Code = lcIdent)or (Code = lcNumber) or (Code >= lxSQLFirst) then
                IsParam := True
              else
                St := TempSt + St;
            end;
          end;

          if not IsParam then begin
            ParsedSQL.Append(St);

            if Code = lxLeftBracket then begin
              if BracketsCount = 0 then
                FLeftmostBracket := ParsedSQL.Length;
              Inc(BracketsCount);
            end
            else if Code = lxRightBracket then begin
              Dec(BracketsCount);
              if BracketsCount = 0 then
                FRightmostBracket := ParsedSQL.Length;
            end;

            if Code = lxReturning then
              if FParsedSQLType = qtInsert then
                FParsedSQLType := qtInsertReturning
              else if FParsedSQLType = qtUpdate then
                FParsedSQLType := qtUpdateReturning;
          end;
        until (Code = lcEnd) or IsParam;

        if IsParam then begin
          ParamName := St;

          l := Length(ParamName);
          if (ParamName[1] = LeftQuote) and (ParamName[l] = RightQuote) then
            ParamName := Copy(ParamName, 2, l - 2);

          if FDistinctParams then
            ParamIndex := IndexOfParam(ParamName)
          else
            ParamIndex := -1;

          if ParamIndex > -1 then
            Param := Params[ParamIndex]
          else
            Param := nil;

          if FScanParams and (ParamIndex = - 1) then begin
            Param := AddParam;
            Param.SetName(ParamName);
            ParamIndex := FParams.Count - 1;
          end;

          p := ParsedSQL.Length + 1;
          if ParamIndex = -1 then
            ParamIndex := FParamsInfo.Count;

          ParsedSQL.Append('$' + IntToStr(ParamIndex + 1));
          AddParamPosition(ParamName, p, ParsedSQL.Length + 1, Param);
        end;
      until Code = lcEnd;
    finally
      Parser.Free;
    end;

    Result := ParsedSQL.ToString;
  finally
    ParsedSQL.Free;
  end;
end;

function TPgSQLCommand.IsValidBatchSQL: boolean;
begin
  Result := inherited IsValidBatchSQL or (FParsedSQLType = qtSelectProc);

  if Result then
    Result := Pos('ON CONFLICT', UpperCase(FSQL)) <= 0;
end;

procedure TPgSQLCommand.MakeSPParam(const Name: string; Oid: integer; Direction: char);
var
  Param: TParamDesc;
  DataType, SubDataType: word;
  Length, Scale, Size: integer;
  Fixed: boolean;
  ObjectType: TObjectType;
begin
  Param := AddParam;
  Param.SetName(Name);

  case Direction of
    'i': Param.SetParamType(pdInput);
    'o': Param.SetParamType(pdOutput);
    'b': Param.SetParamType(pdInputOutput);
    'r': Param.SetParamType(pdResult);
  else
    Assert(False);
  end;

  FUsedConnection.FTypes.DecodeTypeLength(Oid, -1, Length, Scale);

  FUsedConnection.FTypes.DetectDataType(Oid, {$IFNDEF LITE}nil,{$ENDIF} DataType, SubDataType,
    Length, Scale, Size, Fixed, ObjectType,
    True, True, FOIDAsInt, EnableBCD, EnableFMTBCD,
    False, True, False);

  Param.SetDataType(DataType);
  Param.SetSubDataType(SubDataType);
{$IFDEF LITE}
  Param.SetSize(Size);
{$ENDIF}
end;

procedure TPgSQLCommand.GetProcParamsInfo(const Schema, Name: string; Overload: integer;
  var ParamsInfo: TProcParamsInfo);

  function Split(const s: string; delim: char): _TStringArray;
  var
    i, p, c: integer;
  begin
    SetLength(Result, Length(s));
    if Length(s) = 0 then
      exit;
    c := 0;
    p := 1;
    for i := 1 to Length(s) do begin
      if s[i] = delim then begin
        Result[c] := Copy(s, p, i - p);
        Inc(c);
        p := i + 1;
      end;
    end;
    Result[c] := Copy(s, p, MaxInt);
    Inc(c);
    SetLength(Result, c);
  end;

const
  SQLSelectClause7 =
    'SELECT p.prorettype, p.proretset, p.proargtypes, null as proallargtypes, null as proargmodes, null as proargnames, n.nspname ';

  SQLSelectClause8 =
    'SELECT p.prorettype, p.proretset, p.proargtypes, p.proallargtypes, p.proargmodes, p.proargnames, n.nspname ';

  SQLFromWhereClause =
    'FROM pg_proc p ' +
    '  INNER JOIN pg_namespace n ON n.oid = p.pronamespace ' +
    'WHERE n.nspname = ''%s'' AND p.proname = ''%s'' ';

  SQLFromWhereClauseMulti =
    'FROM pg_proc p ' +
    '  INNER JOIN pg_namespace n ON n.oid = p.pronamespace ' +
    'WHERE n.nspname in (%s) AND p.proname = ''%s'' ' +
    'ORDER BY CASE n.nspname %s END ';

  SQLOffsetLimitClause = 'OFFSET %d LIMIT 1';

var
  RecordSet: TPgSQLRecordSet;
  i: integer;
  RecBuf: IntPtr;
  v: variant;
  SQL: string;
  s: string;
  stypes: _TStringArray;
  Field0: TFieldDesc;
  Field1: TFieldDesc;
  Field2: TFieldDesc;
  Field3: TFieldDesc;
  Field4: TFieldDesc;
  Field5: TFieldDesc;
  Field6: TFieldDesc;
begin
  stypes := nil;
  RecordSet := TPgSQLRecordSet(FConnection.GetRecordSet);
  try
    RecordSet.SetConnection(FConnection);
    RecordSet.SetProp(prOIDAsInt, True);

    if TPgSQLCommand(RecordSet.GetCommand).FConnection.VersionIsEqualOrHigher(8, 0) then
      SQL := SQLSelectClause8
    else
      SQL := SQLSelectClause7;

    if not FUsedConnection.FMultipleSchema then
      SQL := Format(SQL + SQLFromWhereClause + SQLOffsetLimitClause, [Schema, Name, Overload - 1])
    else
      SQL := Format(SQL + SQLFromWhereClauseMulti + SQLOffsetLimitClause,
        [FUsedConnection.GetSchemaNamesForSQL(0), Name, FUsedConnection.GetSchemaNamesForSQL(1), Overload - 1]);

    RecordSet.SetSQL(SQL);
    RecordSet.Open;

    Field0 := RecordSet.Fields[0];
    Field1 := RecordSet.Fields[1];
    Field2 := RecordSet.Fields[2];
    Field3 := RecordSet.Fields[3];
    Field4 := RecordSet.Fields[4];
    Field5 := RecordSet.Fields[5];
    Field6 := RecordSet.Fields[6];

    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        raise Exception.CreateFmt(SStoredProcNotFound, [Schema, Name]);

      RecordSet.GetFieldAsVariant(Field0, RecBuf, v);
      ParamsInfo.RetTypeOid := v;

      RecordSet.GetFieldAsVariant(Field1, RecBuf, v);
      ParamsInfo.IsReturnSet := v;

      if not RecordSet.GetNull(Field3, RecBuf) then begin
        RecordSet.GetFieldAsVariant(Field3, RecBuf, v);
        s := VarToStr(v);
        s := Copy(s, 2, Length(s) - 2);
        stypes := Split(s, ',');
      end
      else begin
        RecordSet.GetFieldAsVariant(Field2, RecBuf, v);
        s := VarToStr(v);
        stypes := Split(s, ' ');
      end;
      SetLength(ParamsInfo.TypeOIDs, Length(stypes));
      for i := 0 to Length(stypes) - 1 do
        ParamsInfo.TypeOIDs[i] := StrToInt(stypes[i]);

      if not RecordSet.GetNull(Field4, RecBuf) then begin
        RecordSet.GetFieldAsVariant(Field4, RecBuf, v);
        s := VarToStr(v);
        s := Copy(s, 2, Length(s) - 2);
        ParamsInfo.Modes := Split(s, ',');
      end;

      if not RecordSet.GetNull(Field5, RecBuf) then begin
        RecordSet.GetFieldAsVariant(Field5, RecBuf, v);
        s := VarToStr(v);
        s := Copy(s, 2, Length(s) - 2);
        ParamsInfo.Names := Split(s, ',');
      end
      else begin
        SetLength(ParamsInfo.Names, Length(stypes));
        for i := 0 to Length(stypes) - 1 do
          ParamsInfo.Names[i] := 'p' + IntToStr(i + 1);
      end;

      if not RecordSet.GetNull(Field6, RecBuf) then begin
        RecordSet.GetFieldAsVariant(Field6, RecBuf, v);
        s := VarToStr(v);
        ParamsInfo.Schema := s;
      end;

    finally
      RecordSet.FreeRecBuf(RecBuf);
    end;
  finally
    FConnection.ReleaseRecordSet(TCRRecordset(RecordSet));
  end;
end;

procedure TPgSQLCommand.InitProcParams(const Name: string; Overload: integer);
var
  ProcName, Schema: string;
  ParamIndex: Integer;
  i: integer;
  ParamsInfo: TProcParamsInfo;
  Info: TSQLObjectInfo;
  HasOutParams: boolean;
  s, ParamName: string;
  Mode: char;
begin
  PgSQLInfo.SplitObjectName(Name, Info);

  if Info.Schema <> '' then
    Schema := PgSQLInfo.NormalizeName(Info.Schema, False, True)
  else
    Schema := FUsedConnection.GetCachedSchema;
  ProcName := PgSQLInfo.NormalizeName(Info.Name, False, True);

  GetProcParamsInfo(Schema, ProcName, Overload, ParamsInfo);

  FParams.Clear;

  if not ParamsInfo.IsReturnSet and (ParamsInfo.RetTypeOid <> SQL_PG_VOID) then begin
    HasOutParams := False;
    for i := 0 to Length(ParamsInfo.Modes) - 1 do
      if ParamsInfo.Modes[i] <> 'i' then begin
        HasOutParams := True;
        break;
      end;

    if not HasOutParams then
      MakeSPParam('result', ParamsInfo.RetTypeOid, 'r');
  end;

  for i := 0 to Length(ParamsInfo.TypeOIDs) - 1 do begin
    if i < Length(ParamsInfo.Names) then begin
      ParamName := ParamsInfo.Names[i];
      ParamName := PgSQLInfo.UnQuote(ParamName);
      if ParamName <> '' then
        ParamName := StringReplace(ParamName, ' ' , '_', [rfReplaceAll])
      else
        ParamName := IntToStr(i + 1);
    end
    else
      ParamName := IntToStr(i + 1);
    if FParams.FindParam(ParamName) <> nil then begin
      ParamIndex := 1;
      while FParams.FindParam(ParamName + '_' + IntToStr(ParamIndex)) <> nil do
        Inc(ParamIndex);
      ParamName := ParamName + '_' + IntToStr(ParamIndex);
    end;

    if i < Length(ParamsInfo.Modes) then begin
      s := ParamsInfo.Modes[i];
      if s <> '' then
        Mode := s[1]
      else
        Mode := 'i';
    end
    else
      Mode := 'i';

    if ParamsInfo.IsReturnSet and (Mode = 'b') then
      Mode := 'i';

    if not ParamsInfo.IsReturnSet or (Mode = 'i') then
      MakeSPParam(ParamName, ParamsInfo.TypeOIDs[i], Mode);
  end;
end;

function TPgSQLCommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;
var
  ProcName, s, si: string;
  Overload, OverloadPos, i, j, p, OutCount: integer;
  Added: boolean;
begin
  Overload := 1;
  ProcName := Name;
  OverloadPos := Pos(':', Name);
  if OverloadPos > 0 then begin
    try
      Overload := StrToInt(Trim(Copy(Name, OverloadPos + 1, Length(Name))));
      ProcName := Copy(Name, 1, OverloadPos - 1);
    except
    end;
    if Overload = 0 then
      Overload := 1;
  end;

  if NeedDescribe then
    InitProcParams(ProcName, Overload);

  OutCount := 0;
  for i := 0 to FParams.Count - 1 do begin
    if FParams[i].GetParamType > pdInput then begin
      Inc(OutCount);
      if OutCount > 1 then
        break;
    end;
  end;

  if OutCount = 1 then
    s := 'SELECT '
  else
    s := 'SELECT * FROM ';
  s := s + ProcName + '(';
  si := s;
  Added := False;
  j := 1;

  FParamsInfo.Clear;
  for i := 0 to FParams.Count - 1 do begin
    if FParams[i].GetParamType in [pdInput, pdInputOutput] then begin
      if Added then begin
        s := s + ', ';
        si := si + ', ';
      end;
      s := s + ':' + FParams[i].GetName;
      p := Length(si) + 1;
      si := si + '$' + IntToStr(j);
      AddParamPosition(FParams[i].GetName, p, Length(si) + 1, FParams[i]);
      Inc(j);
      Added := True;
    end;
  end;

  s := s + ')';
  si := si + ')';

  FSQL := si;
  FUserSQL := s;
  FParsedSQLType := qtSelect;
  Result := s;
end;


function TPgSQLCommand.ForceCreateSPParams: boolean;
begin
  Result := True;
end;

procedure TPgSQLCommand.Prepare;
begin
{$IFDEF DEBUG_MULTITHREAD}
   OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') prepare = ' + FSQL));
{$ENDIF}

  if (GetCursorState > csPrepared) or
     (GetCursorState > csInactive) and (FCursorRef.FStmtHandle <> nil) and FCursorRef.FStmtHandle.Prepared
  then begin
  {$IFDEF DEBUG_MULTITHREAD}
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') prepare GetCursorState = ' + IntToStr(integer(GetCursorState)) + ' - exit'));
  {$ENDIF}
    Exit;
  end;

  if NativeCursor and (Trim(FSQL) = '') then
    raise Exception.Create(SEmptySQLStatement);

  SplitParams;

  FCursorRef.CreateStatement;
  try
    FUseSimpleProtocol := UseSimpleProtocol;
    if FUseSimpleProtocol then
      SetCursorState(csPrepared)
    else begin
      PrepareFetchConnection;
      try
        CheckConnection;

        if not NativeCursor and not FUsedConnection.GetInTransaction then
          raise Exception.Create(SRefCursorNeedTransaction);

        InternalPrepare;
      except
        on E: EPgError do begin
          ReleaseFetchConnection(True);
          FUsedConnection.ProcessInternalException(E, Component);
        end;
        on E: Exception do begin
          ReleaseFetchConnection(True);
          raise;
        end;
      end;
    end;
  except
    FCursorRef.FreeStatement;
    raise;
  end;
end;

procedure TPgSQLCommand.Unprepare;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') unprepare = ' + FSQL));
{$ENDIF}

  if not NativeCursor and not FNativePreparation then
    Exit;

  try
    ResetParams;
    if GetPrepared then
      InternalUnprepare;
  finally
    FExplicitlyPrepared := False;
    ReleaseFetchConnection(False);
    inherited;
  end;
end;

function TPgSQLCommand.GetPrepared: boolean;
begin
  Result := GetCursorState >= csPrepared;
end;

procedure TPgSQLCommand.Execute;
begin
  if GetCursorState > csPrepared then begin
  {$IFDEF DEBUG_MULTITHREAD}
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') execute GetCursorState = ' + IntToStr(integer(GetCursorState)) + ' - exit'));
  {$ENDIF}
    Exit;
  end;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') execute = ' + FSQL));
{$ENDIF}
  PrepareFetchConnection;
  try
    try
      InternalExecute;
    except
      if (FRecordSet = nil) and Assigned(FAfterExecute) then
        FAfterExecute(False);
      raise;
    end;
    if (FRecordSet = nil) and Assigned(FAfterExecute) then
      FAfterExecute(True);
  except
    ReleaseFetchConnection(True);
    raise;
  end;
end;

procedure TPgSQLCommand.ExecuteBatch(Iters, Offset: integer);
begin
  if GetCursorState > csPrepared then
    Exit;

  inherited;
end;

procedure TPgSQLCommand.Close;
begin
  PerformClosePortal;
end;

procedure TPgSQLCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TPgSQLConnection(Value);
    FUsedConnection := FConnection;
  end;
end;

function TPgSQLCommand.GetCursorState: TCursorState;
begin
  Result := FCursorRef.State;
end;

procedure TPgSQLCommand.SetCursorState(Value: TCursorState);
begin
  FCursorRef.FState := Value;
end;

function TPgSQLCommand.GetProtocol: TPgSQLProtocol;
begin
  Result := FUsedConnection.FProtocol;
end;

function TPgSQLCommand.GetCursor: TCRCursor;
begin
  Result := FCursorRef;
end;

procedure TPgSQLCommand.SetCursor(Value: TCRCursor);
begin
  if FCursorRef <> Value then begin
    if (FCursorRef <> FCursor) then
      FCursorRef.Release;

    if Value <> nil then begin
      FCursorRef := TPgCursor(Value);
      FCursorRef.AddRef;

      FNativePreparation := FCursorRef.State < csPrepared;
      FExplicitlyPrepared := not FNativePreparation;
    end
    else
      FCursorRef := FCursor;
  end;
end;

function TPgSQLCommand.GetFirstCursor: TPgCursor;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Params.Count - 1 do
    if Params[i].GetParamType >= pdOutput then
      if Params[i].GetDataType = dtCursor then
        if Result = nil then
          Result := TPgSQLParamDesc(Params[i]).GetAsCursor
        else begin
          FNextCursorRef := TPgSQLParamDesc(Params[i]).GetAsCursor;
          exit;
        end;
  FNextCursorRef := nil;
end;

function TPgSQLCommand.GetNextCursor: TPgCursor;
var
  i: integer;
  Param: TPgSQLParamDesc;
  Cursor: TPgCursor;
  Found: boolean;
begin
  Result := FNextCursorRef;

  if Result <> nil then begin
    Found := False;
    for i := 0 to FParams.Count - 1 do begin
      Param := TPgSQLParamDesc(FParams[i]);
      if Params[i].GetParamType >= pdOutput then
        if Param.GetDataType = dtCursor then begin
          Cursor := Param.GetAsCursor;
          if not Found then begin
            if Cursor = FNextCursorRef then
              Found := True;
          end
          else
          if Cursor.CanFetch then begin
            FNextCursorRef := Cursor;
            exit;
          end;
        end;
      end;
    end;
  FNextCursorRef := nil;
end;

function TPgSQLCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prSimpleQueryExecute:
      if Value then
        FForcedProtocol := pv20
      else
        FForcedProtocol := pvAuto;
    prCommandTimeout:
      FCommandTimeout := Value;
    prOIDAsInt:
      FOIDAsInt := Value;
    prUseParamTypes:
      FUseParamTypes := Value;
    prDistinctParams: if FDistinctParams <> Value then begin
      FDistinctParams := Value;
      SetSQL(FUserSQL);
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TPgSQLCommand.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prLastInsertId:
      Value := FLastInsertOID;
    prRowsProcessed:
      Value := FRowsAffected;
    prCommandTimeout:
      Value := FCommandTimeout;
    prUseParamTypes:
      Value := FUseParamTypes;
    prDistinctParams:
      Value := FDistinctParams;
    prIsSelectParams:
      Value := (GetParsedSQLType = qtInsertReturning) or (GetParsedSQLType = qtUpdateReturning);
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TPgSQLCommand.GetFormatCode(DataType: Word; TypeModifier, Scale: integer; IsField: boolean): integer;
begin
  if (IsField and FFieldsAsText) or FUseSimpleProtocol then
    Result := 0
  else begin
    case DataType of
      dtUnknown, dtString, dtPgName, dtPgChar, dtVoid, dtGuid, dtPgBit, dtPgVarBit:
        Result := 0;
      dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine:
        Result := Byte(FUsedConnection.FEnableGeometrics);
      dtPgInterval:
        Result := Byte(not FUsedConnection.FIntervalAsString);
      dtObject:
        Result := Byte(FUsedConnection.FEnableComposites);
    else
      Result := 1; //Byte(not FUsedConnection.FTypes.IsTextOnly(TypeOid));
    end;
  end;
end;

function TPgSQLCommand.UseReader: boolean;
begin
  Result := (FUsedConnection = FConnection) and
            FUsedConnection.FProtocol.IsReaderSupported;
end;

function TPgSQLCommand.NeedReader: boolean;
begin
  Result := UseReader and NeedFetchConnection;
end;

function TPgSQLCommand.UseSimpleProtocol: boolean;
var
  i: Integer;
  Protocol: TProtocolVersion;
begin
  if FUsedConnection <> nil then
    Protocol := FUsedConnection.FProtocolVersion
  else
    Protocol := pvAuto;

  // SimpleQueryExecute = True or GetExtFieldsInfo or GetIntegerDatetimes etc.
  if (Protocol = pv20) or (FForcedProtocol = pv20) then begin
    Result := True;
    Exit;
  end;

  if (FRecordSet <> nil) and FRecordSet.FCursorWithHold and not FRecordSet.FFetchAll then begin
    Result := True;
    Exit;
  end;

  if (Protocol = pv30) or ((Protocol = pvAuto) and (FForcedProtocol = pv30)) then begin
    Result := False;
    Exit;
  end;

  if FExplicitlyPrepared or ((FRecordSet <> nil) and FRecordSet.FExplicitlyPrepared) then begin
    Result := False;
    Exit;
  end;

  if IsCursor then begin
    Result := False;
    Exit;
  end;

  for i := 0 to Params.Count - 1 do
    if TPgSQLParamDesc(Params[i]).FDataType in [dtBlob, dtMemo, dtWideMemo] then begin
      Result := False;
      Exit;
    end;

  Result := True;
end;

function TPgSQLCommand.NeedFetchConnection: boolean;
begin
  Result := (GetParsedSQLType = qtSelect) and
            (FRecordSet <> nil) and
            ((not FRecordSet.IsFetchAll and not FRecordSet.FCursorWithHold) or FConnection.FRedhisftConnection) and
            not FConnection.GetInTransaction
end;

procedure TPgSQLCommand.PrepareFetchConnection;
var
  SwapConnection: TPgSQLConnection;
begin
  if FConnection.FMultipleConnections and NeedFetchConnection and (FUsedConnection = FConnection) then begin
    SwapConnection := FConnection.QuerySwapConnection;
    try
      SwapConnection.GetInternalTransaction.StartTransaction;
    except
      FConnection.ReturnSwapConnection(SwapConnection);
      raise;
    end;
    FUsedConnection := SwapConnection;
  end;
end;

procedure TPgSQLCommand.ReleaseFetchConnection(InErrorProcessing: boolean);
var
  SwapConnection: TPgSQLConnection;
begin
  if FUsedConnection <> FConnection then
    try
      if InErrorProcessing then
        FUsedConnection.GetInternalTransaction.Rollback
      else
        FUsedConnection.GetInternalTransaction.Commit;
    finally
      SwapConnection := FUsedConnection;
      FUsedConnection := FConnection;
      FConnection.ReturnSwapConnection(SwapConnection);
    end;
end;

procedure TPgSQLCommand.CheckConnection;
begin
  if FUsedConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
end;

function TPgSQLCommand.GetFinalSQL: string;
begin
  if not NativeCursor and (FCursorRef is TPgRefCursor) then
    Result := PgSQLInfo.NormalizeName(TPgRefCursor(FCursorRef).CursorName, True)
  else
    Result := FSQL;
end;

function TPgSQLCommand.GetFinalSQLWithParamValues: string;
var
  FinalSQL: string;
  s: string;
  i: integer;
  Param: TPgSQLParamDesc;
  ValuesSQL: StringBuilder;
  PrevPos: integer;
  CurrPos: integer;
begin
  FinalSQL := GetFinalSQL;
  if UseSimpleProtocol and NativeCursor and (FParamsInfo.Count > 0) then begin
    ValuesSQL := StringBuilder.Create(Length(FinalSQL));
    try
      PrevPos := 1;

      for i := 0 to FParamsInfo.Count - 1 do begin
        CurrPos := FParamsInfo[i].StartPosition;
        if FParamsInfo[i].ParamRef <> nil then
          Param := TPgSQLParamDesc(FParamsInfo[i].ParamRef)
        else
          Param := TPgSQLParamDesc(Params[i]);

        s := Copy(FinalSQL, PrevPos, CurrPos - PrevPos);
        ValuesSQL.Append(s);

        if Param.GetNull or (Param.GetDataType = dtCursor) then
          ValuesSQL.Append('null')
        else
          ValuesSQL.Append(TPgTextConverter.ValueToText(Param.Value, Param.GetDataType, Param.GetSubDataType,
                           FUsedConnection.FUseUnicode, True, False, FUsedConnection.VersionIsEqualOrHigher(8, 2), True));

        PrevPos := FParamsInfo[i].EndPosition;
      end;

      s := Copy(FinalSQL, PrevPos, MaxInt);
      ValuesSQL.Append(s);

      Result := ValuesSQL.ToString;
    finally
      ValuesSQL.Free;
    end;
  end
  else
    Result := FinalSQL;
end;

procedure TPgSQLCommand.CallbackBindParamValue(Dest: TPgSQLNet; ParamNo: integer; var ItemDesc: TPgSQLItemDesc);
var
  Param: TPgSQLParamDesc;
  ParamNull: boolean;
  ParamValuePtr: PVariant;
  OwnerCommand: TPgSQLCommand;
  OwnerParamsCount,
  OwnerParamIndex,
  OwnerItemIndex: integer;
begin
  OwnerCommand := TPgSQLCommand(BatchOwner);
  if OwnerCommand = nil then begin
    Param := FInParamRefs[ParamNo];
    Assert(Param <> nil);

    OwnerItemIndex := 0;
  end
  else begin
    OwnerParamsCount := OwnerCommand.FParamsInfo.Count;
    OwnerParamIndex := ParamNo mod OwnerParamsCount;
    OwnerItemIndex := FBatchOffset + ParamNo div OwnerParamsCount;

    Param := TPgSQLParamDesc(OwnerCommand.FParamsInfo[OwnerParamIndex].ParamRef);
  end;
  ParamNull := Param.ItemNull[OwnerItemIndex];
  ParamValuePtr := Param.GetItemPtr(OwnerItemIndex);

  if ParamNull then
    TPgBinaryConverter.WriteValue(Null, Dest, 0, 0, nil, FUsedConnection)
  else begin
    if not ItemDesc.Described then begin
      FUsedConnection.FTypes.DecodeTypeLength(ItemDesc.TypeOid, ItemDesc.TypeModifier, ItemDesc.Length, ItemDesc.Scale);
      FUsedConnection.FTypes.DetectDataType(ItemDesc.TypeOid, {$IFNDEF LITE}nil,{$ENDIF}
        ItemDesc.DataType, ItemDesc.SubDataType, ItemDesc.Length, ItemDesc.Scale, ItemDesc.Size, ItemDesc.Fixed, ItemDesc.ObjectType,
        True, True, True, False, False, False, True, False);
      ItemDesc.Described := True;
    end;
    TPgBinaryConverter.WriteValue(ParamValuePtr^, Dest, ItemDesc.DataType, ItemDesc.SubDataType, ItemDesc.ObjectType,
      FUsedConnection);
  end;
end;

procedure TPgSQLCommand.PerformPrepare(const SQL: string; ParsedSQLType: TParsedSQLType; const ParamTypes: TIntegerDynArray);
begin
  FCursorRef.FStmtHandle.Prepared := False;
  if not FUseSimpleProtocol then begin
    FUsedConnection.FProtocol.PrepareStmt(FCursorRef.FStmtHandle, SQL, ParsedSQLType, ParamTypes);
    FCursorRef.FStmtHandle.Prepared := True;
  end;
end;

procedure TPgSQLCommand.PerformUnprepare;
begin
  if FCursorRef.FStmtHandle <> nil then begin
    FCursorRef.FStmtHandle.Prepared := False;
    // Protocol = nil after Connection lost
    if FUsedConnection.FProtocol <> nil then
      if not FUseSimpleProtocol then
        FUsedConnection.FProtocol.UnPrepareStmt(FCursorRef.FStmtHandle);
  end;
end;

procedure TPgSQLCommand.PerformBindExecute;
begin
  FUsedConnection.FProtocol.BindExecutePreparedStmt(FCursorRef.FStmtHandle, CallbackBindParamValue, IsCursor);
end;

procedure TPgSQLCommand.PerformClosePortal;
begin
  if GetCursorState > csPrepared then begin
    // Protocol = nil after Connection lost
    if (FUsedConnection.FProtocol <> nil) and (FCursorRef.FStmtHandle <> nil) then
      FUsedConnection.FProtocol.CloseStmt(FCursorRef.FStmtHandle);
    SetCursorState(csPrepared);
  end;
end;

procedure TPgSQLCommand.InternalPrepare;
var
  i: integer;
  ParamTypes: TIntegerDynArray;
  LockedProtocol: TPgSQLProtocol;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') LOCK ON prepare = ' + FSQL));
{$ENDIF}
  // store locked protocol for FINALLY section
  LockedProtocol := LockProtocol;
  try
    if FUseParamTypes and NativeCursor then begin
      SetLength(ParamTypes, FInParamsCount);
      for i := 0 to FInParamsCount - 1 do
        ParamTypes[i] := FUsedConnection.FTypes.DataTypeToOID(
          FInParamRefs[i].GetDataType, FInParamRefs[i].GetSubDataType);
    end
    else
      ParamTypes := nil;

    try
      PerformPrepare(GetFinalSQL, GetParsedSQLType, ParamTypes);

      DescribeParams;

      DescribeFields;
    except
      if (FUsedConnection.FProtocol <> nil) and not FUseSimpleProtocol then
        PerformUnprepare;
      raise;
    end;

    SetCursorState(csPrepared);
  finally
  {$IFDEF DEBUG_MULTITHREAD}
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') UNLOCK ON prepare = ' + FSQL));
  {$ENDIF}
    UnlockProtocol(LockedProtocol);
  end;
end;

procedure TPgSQLCommand.InternalUnprepare;
var
  LockedProtocol: TPgSQLProtocol;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') LOCK ON unprepare = ' + FSQL));
{$ENDIF}
  // store locked protocol for FINALLY section
  LockedProtocol := LockProtocol;
  try
    PerformClosePortal;
    if not FUseSimpleProtocol then
      PerformUnprepare;
    FCursorRef.FreeStatement;
  finally
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') UNLOCK ON unprepare = ' + FSQL));
{$ENDIF}
    UnlockProtocol(LockedProtocol);
  end;
end;

procedure TPgSQLCommand.InternalExecute;
var
  i: integer;
  Param: TPgSQLParamDesc;
  LargeObject: TPgSQLLargeObject;
  NeedPrepare,
  OldExplicitlyPrepared: boolean;
  OldCursorState: TCursorState;
  LockedProtocol: TPgSQLProtocol;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') LOCK ON execute = ' + FSQL));
{$ENDIF}
  // store locked protocol for FINALLY section
  LockedProtocol := LockProtocol;
  try
    NeedPrepare := not GetPrepared;
    FExplicitlyPrepared := (FRecordSet = nil) and GetPrepared;
    if NeedPrepare then
      Prepare
    else begin
      if not FCursorRef.FStmtHandle.Prepared and not FUseSimpleProtocol then begin
        OldExplicitlyPrepared := FExplicitlyPrepared;
        try
          Unprepare;
          FExplicitlyPrepared := OldExplicitlyPrepared;
          Prepare;
        finally
          FExplicitlyPrepared := OldExplicitlyPrepared;
        end;
      end;
      if FCursorRef.FStmtHandle.Prepared and FUseSimpleProtocol then begin
        OldExplicitlyPrepared := FExplicitlyPrepared;
        try
          Unprepare;
          FExplicitlyPrepared := OldExplicitlyPrepared;
          Prepare;
        finally
          FExplicitlyPrepared := OldExplicitlyPrepared;
        end;
      end;
    end;

    OldCursorState := GetCursorState;
    try
      for i := 0 to FParams.Count - 1 do begin
        Param := TPgSQLParamDesc(FParams[i]);
        case Param.GetDataType of
          dtPgLargeObject: begin
            LargeObject := Param.GetObject as TPgSQLLargeObject;
            if LargeObject.Cached then begin
              LargeObject.Connection := FUsedConnection;
              LargeObject.WriteBlob;
            end;
          end;
        {$IFDEF LITE}
          dtBlob:
            if not FOIDAsInt and (Param.FTypeOID = SQL_PG_OID) then begin
              LargeObject := TPgSQLLargeObject.Create(FUsedConnection);
              LargeObject.SetData(TBlob(Param.GetObject).GetData);
              TBlob(Param.GetObject).Free;
              Param.SetObject(LargeObject);
              LargeObject.CreateObject;
              LargeObject.WriteBlob;
            end;
        {$ENDIF}
        end;
      end;

      FUsedConnection.FProtocol.SetTimeout(FCommandTimeout);
      FUsedConnection.FProtocol.SetRowsAffected(FCursorRef.FStmtHandle, 0);

      if FUseSimpleProtocol then
        FUsedConnection.FProtocol.ExecuteStmt(FCursorRef.FStmtHandle, GetFinalSQLWithParamValues, GetParsedSQLType)
      else
        PerformBindExecute;

      SetCursorState(csExecuted);

      if OutParamsReturn and (CommandType <> ctCursor) then
        ReadOutParams;

      if (FRecordSet = nil) and (CommandType <> ctCursor) then
        PerformClosePortal;

      // when we execute insert statement with returnin this information
      // is be available after fetch
      FLastInsertOID := FUsedConnection.FProtocol.LastInsertOID(FCursorRef.FStmtHandle);
      FUsedConnection.SetProp(prLastInsertId, FLastInsertOID);
      UpdateRowsAffected;
    except
      on E: Exception do begin
  //      if UseSimpleProtocol then begin
  //        if GetPrepared then
  //          Unprepare;
  //      end
  //      else
          SetCursorState(OldCursorState);
        FRowsAffected := 0;

        if E is EPgError then
          FUsedConnection.ProcessInternalException(EPgError(E), Component)
        else
          raise;
      end;
    end;

    if NeedPrepare then
      UnPrepare;
  finally
  {$IFDEF DEBUG_MULTITHREAD}
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Command $' + IntToHex(Integer(Self), 8) + ' (Recordset $' + IntToHex(Integer(FRecordSet), 8) + ') UNLOCK ON execute = ' + FSQL));
  {$ENDIF}
    UnlockProtocol(LockedProtocol);
  end;
end;

procedure TPgSQLCommand.DescribeFields;
var
  i: integer;
  OutParams: TPgSQLItemDescs;
  DataType: Word;
  ObjectType: TObjectType;
begin
  FUsedConnection.FProtocol.DescribeFields(FCursorRef.FStmtHandle, OutParams);
  for i := 0 to Length(OutParams) - 1 do begin
    DataType := FUsedConnection.FTypes.GetInternalType(OutParams[i].TypeOid, ObjectType);
    OutParams[i].FormatCode := GetFormatCode(DataType, OutParams[i].TypeModifier, 0, True);
  end;
end;

procedure TPgSQLCommand.DescribeParams;
var
  i: integer;
  InParams: TPgSQLItemDescs;
  DataType: Word;
  ObjectType: TObjectType;
begin
  if NativeCursor then begin
    FUsedConnection.FProtocol.DescribeParams(FCursorRef.FStmtHandle, InParams);

  {$IFNDEF LITE}
    if (BatchOwner = nil) and (Length(InParams) <> FInParamsCount) then
  {$ELSE}
// possible AV when setting a param value with AsDateTime
// http://qc.embarcadero.com/wc/qcmain.aspx?d=9172
    if Length(FInParamRefs) <> Length(InParams) then
  {$ENDIF}
      raise Exception.Create(SInvalidParams);

    for i := 0 to Length(InParams) - 1 do begin
      DataType := FUsedConnection.FTypes.GetInternalType(InParams[i].TypeOid, ObjectType);
      InParams[i].FormatCode := GetFormatCode(DataType, InParams[i].TypeModifier, 0, False);
    end;
  {$IFDEF LITE}
    for i := 0 to Length(InParams) - 1 do
      FInParamRefs[i].FTypeOID := InParams[i].TypeOid;
  {$ENDIF}
  end;
end;

procedure TPgSQLCommand.ReadOutParams;
var
  RecordSet: TCRRecordSet;
  i, n: integer;
  Param: TPgSQLParamDesc;
  Field: TFieldDesc;
  RecBuf: IntPtr;
  Value: Variant;
  SourceObj: TSharedObject;
  Blob: TBlob;
  Command,
  OwnerCommand: TPgSQLCommand;
  ParamsCount,
  OwnerParamsCount,
  ItemIndex: integer;
begin
  RecordSet := FConnection.CreateRecordSet;
  try
    TPgSQLRecordSet(RecordSet).SetTrimFixedChar(Self.FTrimFixedChar);

    OwnerCommand := TPgSQLCommand(BatchOwner);
    if OwnerCommand = nil then begin
      ParamsCount := FParams.Count;
      OwnerParamsCount := ParamsCount;
    end
    else begin
      OwnerParamsCount := OwnerCommand.FParams.Count;
      ParamsCount := OwnerParamsCount;
    end;

    RecordSet.SetConnection(FUsedConnection);
    RecordSet.SetProp(prFlatBuffers, False);
    RecordSet.SetProp(prOIDAsInt, True);
    RecordSet.SetProp(prCursorAsString, True);
    RecordSet.SetProp(prExtendedFieldsInfo, False);

    Command := TPgSQLCommand(RecordSet.GetCommand);
    Command.SetCursor(FCursor);
    Command.FForcedProtocol := FForcedProtocol;
    Command.FUseSimpleProtocol := FUseSimpleProtocol;
    RecordSet.Open;
    RecordSet.AllocRecBuf(IntPtr(RecBuf));
    try
      ItemIndex := 0;
      if OwnerCommand <> nil then
        ItemIndex := FBatchOffset;
      repeat
        RecordSet.GetNextRecord(RecBuf);
        if RecordSet.Eof then
          Exit;

        n := 0;
        for i := 0 to ParamsCount - 1 do begin
          if OwnerCommand = nil then
            Param := TPgSQLParamDesc(FParams[i])
          else
            Param := TPgSQLParamDesc(OwnerCommand.FParamsInfo[i mod OwnerParamsCount].ParamRef);
          Assert(Param <> nil);

          if Param.GetParamType in [pdOutput, pdInputOutput, pdResult] then begin
            if n >= RecordSet.Fields.Count then
              break;
            repeat
              Field := RecordSet.Fields[n];
              Inc(n);
            until Field.ParentField = nil;

            if not CheckTypeCorrespondence(Param.GetDataType, Field.DataType) then
              raise Exception.CreateFmt(SInvalidOutParamDataType, [Param.GetName]);

            if RecordSet.GetNull(Field, RecBuf) then
              Param.SetItemNull(ItemIndex, True)
            else begin
              Param.SetNull(False);
              Param.SetItemNull(ItemIndex, False);
              if Field.IsComplex and
                not (Field.DataType in [dtExtString, dtExtWideString])
              then begin
                SourceObj := TPgSQLRecordSet(RecordSet).InternalGetObject(Field, RecBuf);
                case Field.DataType of
                  dtBlob, dtMemo, dtWideMemo: begin
                    if Param.GetDataType = dtString then
                      Param.ItemValue[ItemIndex] := TBlob(SourceObj).AsString
                    else
                    if Param.GetDataType = dtWideString then
                      Param.ItemValue[ItemIndex] := TBlob(SourceObj).AsWideString
                    else begin
                      Blob := TBlob(TVarData(Param.ItemValue[ItemIndex]).VPointer);
                      // cannot be used in UniDAC
                      {Blob.GetData.Clear;
                      Blob.IsUnicode := TBlob(SourceObj).IsUnicode;
                      Blob.SetData(TBlob(SourceObj).GetData);}
                      Blob.Assign(TBlob(SourceObj));
                    end;
                  end;
                {$IFNDEF LITE}
                  dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ:
                    TCustomPgTimeStamp(TVarData(Param.ItemValue[ItemIndex]).VPointer).Assign(TCustomPgTimeStamp(SourceObj));
                  dtPgInterval:
                    TPgInterval(TVarData(Param.ItemValue[ItemIndex]).VPointer).Assign(TPgInterval(SourceObj));
                  dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine:
                    TPgGeometric(TVarData(Param.ItemValue[ItemIndex]).VPointer).Assign(TPgGeometric(SourceObj));
                  dtObject: begin
                    TPgRow(TVarData(Param.ItemValue[ItemIndex]).VPointer).RowType := TPgRow(SourceObj).RowType;
                    TPgRow(TVarData(Param.ItemValue[ItemIndex]).VPointer).Assign(TPgRow(SourceObj));
                  end;
                {$ENDIF}
                end
              end
              else begin
                RecordSet.GetFieldAsVariant(Field, RecBuf, Value);
                case Param.GetDataType of
                  dtPgLargeObject:
                    TPgSQLLargeObject(TVarData(Param.ItemValue[ItemIndex]).VPointer).OID := Value;
                  dtCursor:
                    TPgRefCursor(TVarData(Param.ItemValue[ItemIndex]).VPointer).Assign(FUsedConnection, Value);
                else
                  Param.ItemValue[ItemIndex] := Value;
                end;
              end;
            end;
          end;
        end;
        Inc(ItemIndex);
      until False;
    finally
      Marshal.FreeHGlobal(RecBuf);
      RecordSet.Close;
    end;
  finally
    RecordSet.Free;
  end;
end;

procedure TPgSQLCommand.SplitParams;
var
  i: integer;
  Index: integer;
begin
  FInParamsCount := 0;
  FOutParamsCount := 0;

  for i := 0 to FParams.Count - 1 do begin
    if FParams[i].GetParamType in [pdUnknown, pdInput, pdInputOutput] then
      Inc(FInParamsCount);
    if FParams[i].GetParamType in [pdInputOutput, pdOutput, pdResult] then
      Inc(FOutParamsCount);
  end;

  if not FUseSimpleProtocol then begin
    SetLength(FInParamRefs, FInParamsCount);

    Index := 0;
    for i := 0 to FParams.Count - 1 do
      if FParams[i].GetParamType in [pdUnknown, pdInput, pdInputOutput] then begin
        FInParamRefs[Index] := TPgSQLParamDesc(FParams[i]);
        Inc(Index);
      end;
  end;
end;

procedure TPgSQLCommand.ResetParams;
begin
  FInParamsCount := 0;
  FOutParamsCount := 0;

  SetLength(FInParamRefs, 0);
end;

function TPgSQLCommand.GetParsedSQLType: TParsedSQLType;
begin
  if not NativeCursor then
    Result := qtCursor
  else
    Result := FParsedSQLType;
end;

function TPgSQLCommand.GetBatchSQL(ForSimpleProtocol: boolean; Iters, Offset: integer): string;
var
  OriginalSQL: string;
  BatchSQL: StringBuilder;
  i, ParamIndex, OpenBracket, CloseBracket, SQLLength: integer;
  HasParams: boolean;

  procedure AppendParamValue(ParamIndex, ItemIndex: integer);
  var
    ParamDesc: TParamDesc;
  begin
    ParamDesc := FParamsInfo[ParamIndex].ParamRef;
    if ParamDesc.ItemNull[ItemIndex] or (ParamDesc.GetDataType = dtCursor) then
      BatchSQL.Append('null')
    else
      BatchSQL.Append(TPgTextConverter.ValueToText(ParamDesc.ItemValue[ItemIndex], ParamDesc.GetDataType, ParamDesc.GetSubDataType,
                       FUsedConnection.FUseUnicode, True, False, FUsedConnection.VersionIsEqualOrHigher(8, 2), True));
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

      if not ForSimpleProtocol then
        BatchSQL.Append('$' + IntToStr(ParamIndex))
      else
        AppendParamValue(i, Offset + Iteration);

      n := FParamsInfo[i].EndPosition - 1;

      if i = (FParamsInfo.Count - 1) then
        BatchSQL.Append(OriginalSQL, n, CloseBracket - n);
      Inc(ParamIndex);
    end;
  end;

begin
  OriginalSQL := Trim(FSQL);
  Result := OriginalSQL;

  SQLLength := Length(OriginalSQL);
  BatchSQL := StringBuilder.Create(SQLLength * Iters);
  ParamIndex := 1;
  HasParams := FParamsInfo.Count > 0;
  try
    case FParsedSQLType of
      qtInsert,
      qtInsertReturning:
        if HasParams then begin
          OpenBracket := FLeftmostBracket;
          CloseBracket := FRightmostBracket;

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
      qtSelectProc,
      qtUpdate,
      qtUpdateReturning,
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

function TPgSQLCommand.GetBatchIters(Iters: integer): integer;
begin
  if HasParams and ((FParamsInfo.Count * Iters) > MaxBatchParams) then
    Result := MaxBatchParams div FParamsInfo.Count
  else
    Result := MaxBatchParams;
  if Result > Iters then
    Result := Iters;
end;

procedure TPgSQLCommand.InternalExecuteBatch(Iters, Offset: integer);
var
  n,
  BatchSize: integer;
  BatchCommand: TPgSQLCommand;
  NeedSetSQL,
  NeedPrepare: boolean;
begin
  BatchCommand := TPgSQLCommand(GetBatchCommand);
  BatchCommand.FParsedSQLType := FParsedSQLType;
  if (BatchCommand.FParsedSQLType <> qtInsert) and (BatchCommand.FParsedSQLType <> qtInsertReturning) then
    BatchCommand.FForcedProtocol := pv20
  else
    BatchCommand.FForcedProtocol := pv30;

  n := 0;
  FRowsAffected := 0;
  FParamsProcessed := 0;
  BatchSize := GetBatchIters(Iters);

  NeedPrepare := not BatchCommand.UseSimpleProtocol;
  try
    while n < Iters do begin
      if n + BatchSize > Iters then
        BatchSize := Iters - n;

      NeedSetSQL := not NeedPrepare or (BatchCommand.FUserSQL <> FUserSQL) or (FLastBatchIters <> BatchSize);

      if NeedSetSQL then begin
        if BatchCommand.GetPrepared then
          BatchCommand.Unprepare;

        BatchCommand.FSQL := GetBatchSQL(not NeedPrepare, BatchSize, Offset + n);
        BatchCommand.FUserSQL := FUserSQL;
      end;
      if NeedPrepare and not BatchCommand.GetPrepared then
        BatchCommand.Prepare;

      if (CommandType = ctCursor) and BatchCommand.GetPrepared then begin
        BatchCommand.FCursor.FStmtHandle.FetchAll := FCursor.FStmtHandle.FetchAll;
        BatchCommand.FCursor.FStmtHandle.WithHold := FCursor.FStmtHandle.WithHold;
        BatchCommand.FCursor.FStmtHandle.FetchRows := FCursor.FStmtHandle.FetchRows;
      end;

      BatchCommand.FBatchIters := 1;
      BatchCommand.FBatchOffset := Offset + n;
      BatchCommand.FCommandType := FCommandType;
      BatchCommand.Execute;

      Inc(FRowsAffected, BatchCommand.FRowsAffected);
      Inc(FParamsProcessed, BatchSize);
      FLastInsertOID := BatchCommand.FLastInsertOID;
      Inc(n, BatchSize);
      FLastBatchIters := BatchSize;
    end;

    SetCursorState(csExecuted);
  finally
    if NeedPrepare and not GetPrepared then
      BatchCommand.Unprepare;
  end;
end;

function TPgSQLCommand.NativeCursor: boolean;
begin
  Result := FCursor = FCursorRef;
end;

function TPgSQLCommand.OutParamsReturn: boolean;
begin
  CheckConnection;

  Result := False;
  if GetPrepared then
    Result := NativeCursor and
      (FUsedConnection.FProtocol.RowsReturn(FCursorRef.FStmtHandle) and
      ((FOutParamsCount > 0) or FUsedConnection.FProtocol.IsVoidFunc(FCursorRef.FStmtHandle)
        or (FRecordSet = nil) or (FRecordSet.FDescribeExecute)));
end;

function TPgSQLCommand.RowsReturn: boolean;
begin
  CheckConnection;

  Result := False;
  if GetPrepared then
    Result := (FCursorRef.FStmtHandle <> nil) and
      FUsedConnection.FProtocol.RowsReturn(FCursorRef.FStmtHandle) and
      ((FRecordSet <> nil) and FRecordSet.FDescribeExecute or not OutParamsReturn);
end;

procedure TPgSQLCommand.UpdateRowsAffected;
begin
  FRowsAffected := FUsedConnection.FProtocol.GetRowsAffected(FCursorRef.FStmtHandle);
  if (GetParsedSQLType = qtSelect) and (FRowsAffected <> 0) then
    FRowsAffected := 0;
end;

function TPgSQLCommand.IsCursor: boolean;
begin
  Result := (ParsedSQLType in [qtSelect, qtCursor, qtSelectProc]) or
            (CommandType = ctCursor) or
            RowsReturn;
end;

function TPgSQLCommand.HasParams: boolean;
begin
  Result := FParamsInfo.Count > 0;
end;

function TPgSQLCommand.LockProtocol: TPgSQLProtocol;
begin
  if FUsedConnection = FConnection then begin
    Result := FConnection.FProtocol;
    Result.Lock;
  end
  else
    Result := nil;
end;

procedure TPgSQLCommand.UnlockProtocol(Protocol: TPgSQLProtocol);
begin
  if Protocol <> nil then
    Protocol.Unlock;
end;

procedure TPgSQLCommand.BreakExec;
begin
  if (GetCursorState <> csInactive) and (FUsedConnection <> nil) then
    FUsedConnection.BreakExec;
end;

{ TPgSQLRecordSet }

constructor TPgSQLRecordSet.Create;
begin
  inherited;

  FFetchAll := True;
  FFetchRows := 25;
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' create Recordset $' + IntToHex(Integer(Self), 8) + ' > create Command $' + IntToHex(Integer(FCommand), 8)));
{$ENDIF}
end;

destructor TPgSQLRecordSet.Destroy;
begin
  Close;

{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' destroy Recordset $' + IntToHex(Integer(Self), 8) + ' > destroy Command $' + IntToHex(Integer(FCommand), 8)));
{$ENDIF}
  inherited;
end;

procedure TPgSQLRecordSet.ExecFetch(DisableInitFields: boolean);
var
  LockedProtocol: TPgSQLProtocol;
begin
  FIsFetchStart := True;
  FPrefetchedRowCount := 0;
  FAllRowsPrefetched := True;

  TPgSQLCommand(FCommand).PrepareFetchConnection;
  try
  {$IFDEF DEBUG_MULTITHREAD}
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Recordset $' + IntToHex(Integer(Self), 8) + ' (Command $' + IntToHex(Integer(FCommand), 8) + ') LOCK ON ExecFetch = ' + TPgSQLCommand(FCommand).FSQL));
  {$ENDIF}
    // store locked protocol for FINALLY section
    LockedProtocol := TPgSQLCommand(FCommand).LockProtocol;
    try
      inherited;
    finally
    {$IFDEF DEBUG_MULTITHREAD}
      OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Recordset $' + IntToHex(Integer(Self), 8) + ' (Command $' + IntToHex(Integer(FCommand), 8) + ') UNLOCK ON ExecFetch = ' + TPgSQLCommand(FCommand).FSQL));
    {$ENDIF}
      TPgSQLCommand(FCommand).UnlockProtocol(LockedProtocol);
    end;
  except
    TPgSQLCommand(FCommand).ReleaseFetchConnection(True);
    raise
  end;
end;

procedure TPgSQLRecordSet.FetchAll;
var
  Fetched: boolean;
begin
  if (FCommand.GetCursorState >= csExecuted) and (FCommand.GetCursorState < csFetchingAll) then begin
    FCommand.SetCursorState(csFetchingAll);
    try
      while True do begin
        repeat
          Fetched := Fetch(False);
        until FNoFetchData or not Fetched;

        if (UsedConnection <> nil) and UsedConnection.GetConnected and UsedConnection.FProtocol.HasMoreResultSets then begin
          FreeData;
          InitData;
          FEOF := False;
          CheckFieldDescs;
        end
        else
          Break;
      end;
    finally
      if FCommand.GetCursorState <> csInactive then
        FCommand.SetCursorState(csFetched);
    end;
  end;
end;

function TPgSQLRecordSet.CanDisconnect: boolean;
begin
  Result := not FNeedExtFieldsInfo.NeedInfo and inherited CanDisconnect;
end;

function TPgSQLRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TPgSQLFieldDesc;
end;

procedure TPgSQLRecordSet.Prepare;
begin
  FExplicitlyPrepared := True;

  inherited;
end;

procedure TPgSQLRecordSet.ExecCommand(Iters: integer; Offset: integer);
var
  NeedPrepare: boolean;
  Command: TPgSQLCommand;
begin
  Command := TPgSQLCommand(GetCommand);

      // to prevent re-executing on TPgQuery.Execute with SQL = 'select ...'
      if (Command.GetCursorState >= csExecuted) and (FFetchCursor <> nil) then begin
      {$IFDEF DEBUG_MULTITHREAD}
        OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Recordset $' + IntToHex(Integer(Self), 8) + ' ExecCommand GetCursorState = ' + IntToStr(integer(Command.GetCursorState)) + ' - exit'));
      {$ENDIF}
        Exit;
      end;

      try
        NeedPrepare := not Prepared;
        if NeedPrepare then
          InternalPrepare;

        try
          FFetchCursor.FStmtHandle.WithHold := FCursorWithHold;
          if FFetchAll or Command.NeedReader then begin
            FFetchCursor.FStmtHandle.FetchAll := True;
            FFetchCursor.FStmtHandle.FetchRows := 0;
            FFetchCursor.FStmtHandle.PrefetchRows := 0;
          end
          else begin
            FFetchCursor.FStmtHandle.FetchAll := False;
            FFetchCursor.FStmtHandle.FetchRows := FFetchRows;
            CalcPrefetchRows(FFetchCursor.FStmtHandle);
          end;

          inherited;

          if Command.FUseSimpleProtocol then // no fields on prepare
            SetCommandType;

          if (Command.CommandType = ctCursor) and (Iters > 1) then
            FFetchCursor.SetStmtHandle(TPgSQLCommand(Command.GetBatchCommand).FCursor.FStmtHandle);
        except
          if NeedPrepare then
            InternalUnprepare;
          raise;
        end;

        if Command.CommandType <> ctCursor then
          if NeedPrepare then
            InternalUnprepare
          else
            Command.PerformClosePortal
      except
        if not FDescribeExecute and Assigned(Command.AfterExecute) then
          Command.AfterExecute(False);
        raise;
      end;
      if not FDescribeExecute and Assigned(Command.AfterExecute) then
        Command.AfterExecute(True);
end;

function TPgSQLRecordSet.IsFullReopen: boolean;
begin
  Result := inherited IsFullReopen or not TPgSQLCommand(GetCommand).NativeCursor;
end;

procedure TPgSQLRecordSet.Reopen;
begin
  if not IsFullReopen then
    TPgSQLCommand(GetCommand).PerformClosePortal;

  inherited;
end;

procedure TPgSQLRecordSet.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
begin
  InternalCreateComplexField(RecBuf, UsedConnection, Field, Field.DataOffset,
    Field.DataType, Field.SubDataType, Field.ObjectType, FCreateFieldObjectFunc, FCacheBlobs
    );
end;

class procedure TPgSQLRecordSet.InternalCreateComplexField(RecBuf: IntPtr; Connection: TPgSQLConnection;
  Field: TFieldDesc; Offset: integer; DataType: word; SubDataType: word; ObjectType: TObjectType;
  CreateFieldObjectFunc: TCreateFieldObjectFunc; CacheBlobs: boolean; FetchToOuterBuffer: Boolean = False);
var
  Ptr: IntPtr;
  Obj: TSharedObject;
begin
  Obj := nil;
  Ptr := PtrOffset(RecBuf, Offset);

  if Assigned(CreateFieldObjectFunc) then
    Obj := CreateFieldObjectFunc(DataType);

  case DataType of
    // copied from MemData
    dtBlob, dtMemo, dtWideMemo: begin
      Obj := TCompressedBlob.Create; // TODO: UniDAC require TCompressedBlob
      if DataType = dtWideMemo then
        TBlob(Obj).IsUnicode := True;
      // RollBack is always on for LOB fields. Otherwise modification
      // that cannot be canceled is possible.
      TBlob(Obj).EnableRollback;
    end;
    dtExtString, dtExtWideString, dtExtVarBytes:
      Marshal.WriteIntPtr(Ptr, nil);
    //
    dtPgLargeObject: begin
      if Obj = nil then                       // UniDac and dbExpress
        Obj := TPgSQLLargeObject.Create(Connection);

      TPgSQLLargeObject(Obj).Cached := CacheBlobs;
    end;
  {$IFNDEF LITE}
    dtPgDate:
      Obj := TPgDate.Create;
    dtPgTime: begin
      Obj := TPgTime.Create;
      if SubDataType = dtPgTimeTZ then
        TPgTime(Obj).HasTimeZone := True;
    end;
    dtPgTimeStamp, dtPgTimeStampTZ: begin
      Obj := TPgTimeStamp.Create;
      if SubDataType = dtPgTimeStampTZ then
        TPgTimeStamp(Obj).HasTimeZone := True;
    end;
    dtPgInterval:
      Obj := TPgInterval.Create;
    dtPgPoint:
      Obj := TPgPoint.Create;
    dtPgLSeg:
      Obj := TPgLSeg.Create;
    dtPgBox:
      Obj := TPgBox.Create;
    dtPgPath:
      Obj := TPgPath.Create;
    dtPgPolygon:
      Obj := TPgPolygon.Create;
    dtPgCircle:
      Obj := TPgCircle.Create;
    dtPgLine:
      Obj := TPgLine.Create;
    dtObject:
      Obj := TPgRow.Create(TPgRowType(ObjectType));
  {$ENDIF}
    dtCursor:
      Obj := TPgRefCursor.Create;
  else
    inherited;
  end;

  if Obj <> nil then
    Marshal.WriteIntPtr(Ptr, Obj.GCHandle);
end;

procedure TPgSQLRecordSet.FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Obj: TSharedObject;
  b: boolean;
begin
  case Field.DataType of
    dtPgLargeObject,
    dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ, dtPgInterval,
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine,
    dtCursor, dtObject: begin
      Obj := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
      b := (Obj <> nil) and (Obj.RefCount = 1);
      Obj.Free;
      if b then
        Marshal.WriteIntPtr(RecBuf, Field.DataOffset, nil);
    end;
  else
    inherited;
  end;
end;

{$IFNDEF LITE}
procedure TPgSQLRecordSet.CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc);
var
  ValueOffset: Integer;
  SrcPtr, DestPtr: IntPtr;
begin
  ValueOffset := Field.DataOffset;

  case Field.DataType of
    dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      TCustomPgTimeStamp(GetGCHandleTarget(DestPtr)).Assign(TCustomPgTimeStamp(GetGCHandleTarget(SrcPtr)));
    end;
    dtPgInterval: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      TPgInterval(GetGCHandleTarget(DestPtr)).Assign(TPgInterval(GetGCHandleTarget(SrcPtr)));
    end;
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      TPgGeometric(GetGCHandleTarget(DestPtr)).Assign(TPgGeometric(GetGCHandleTarget(SrcPtr)));
    end;
    dtObject: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      TPgRow(GetGCHandleTarget(DestPtr)).Assign(TPgRow(GetGCHandleTarget(SrcPtr)));
    end
  else
    inherited;
  end;
end;

function TPgSQLRecordSet.InternalCompareFieldValue(ValuePtr: IntPtr; ValueLen: Word; ValueType: Word; DataBuf: IntPtr; DataLen: Word; FieldType: Word; HasParent: boolean; IsFixed: boolean; const Options: TCompareOptions): integer;
var
  str, str1: {$IFDEF NEXTGEN}string{$ELSE}AnsiString{$ENDIF};
  v: variant;
  Obj: TSharedObject;
  NeedFree: boolean;
  ts1, ts2: TCustomPgTimeStamp;
  int1, int2: TPgInterval;
  Geom1, Geom2: TPgGeometric;
begin
  Obj := nil;
  NeedFree := False;
  try
    case FieldType of
      dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ: begin
        ts2 := TCustomPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf)));
        ts1 := nil;
        v := Unassigned;
        case ValueType of
          dtDateTime, dtDate, dtTime:
            v := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
          dtString:
            v := Marshal.PtrToStringAnsi(ValuePtr);
          dtWideString:
            v := Marshal.PtrToStringUni(ValuePtr);
          dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ:
            ts1 := TCustomPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        else
          Assert(False);
        end;
        if ts1 = nil then begin
          case FieldType of
            dtPgDate:
              TPgBufferConverter.VarToPgDate(v, Obj, NeedFree);
            dtPgTime:
              TPgBufferConverter.VarToPgTime(v, ts2.HasTimeZone, Obj, NeedFree);
            dtPgTimeStamp, dtPgTimeStampTZ:
              TPgBufferConverter.VarToPgTimeStamp(v, Obj, NeedFree);
          end;
          ts1 := TCustomPgTimeStamp(Obj);
        end;
        Result := ts1.Compare(ts2);
      end;
      dtPgInterval: begin
        int2 := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf)));
        int1 := nil;
        v := Unassigned;
        case ValueType of
          dtString:
            v := Marshal.PtrToStringAnsi(ValuePtr);
          dtWideString:
            v := Marshal.PtrToStringUni(ValuePtr);
          dtPgInterval:
            int1 := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        else
          Assert(False);
        end;
        if int1 = nil then begin
          TPgBufferConverter.VarToPgInterval(v, Obj, NeedFree);
          int1 := TPgInterval(Obj);
        end;
        Result := int1.Compare(int2);
      end;
      dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine: begin
        Geom2 := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf)));
        Geom1 := nil;
        v := Unassigned;
        case ValueType of
          dtString:
            v := Marshal.PtrToStringAnsi(ValuePtr);
          dtWideString:
            v := Marshal.PtrToStringUni(ValuePtr);
          dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine:
            Geom1 := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        else
          Assert(False);
        end;
        if Geom1 = nil then begin
          TPgBufferConverter.VarToPgGeometric(v, FieldType, Obj, NeedFree);
          Geom1 := TPgGeometric(Obj);
        end;
        Result := AnsiCompareStr(Geom1.AsString, Geom2.AsString);
      end;
      dtGuid: begin
        str := {$IFDEF NEXTGEN}string{$ELSE}AnsiString{$ENDIF}(Marshal.PtrToStringAnsi(DataBuf));
        case ValueType of
          dtString, dtGuid: begin
            str1 := {$IFDEF NEXTGEN}string{$ELSE}AnsiString{$ENDIF}(Marshal.PtrToStringAnsi(ValuePtr));
            if TPgSQLCommand(GetCommand).FUsedConnection.UuidWithBraces and (str1 <> '') and (str1[1] <> '{') then
              str1 := '{' + str1 + '}';
          end;
        else
          Assert(False);
        end;
        Result := AnsiCompareStr(AnsiUpperCase(str), AnsiUpperCase(str1));
      end;
    else
      Result := inherited InternalCompareFieldValue(ValuePtr, ValueLen, ValueType, DataBuf, DataLen, FieldType, HasParent, IsFixed, Options);
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

function TPgSQLRecordSet.GetSortOptions(SortColumn: TSortColumn): TCompareOptions;
begin
  Result := inherited GetSortOptions(SortColumn) + [coInvertNullOrder];
end;

function TPgSQLRecordSet.GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
begin
  CheckFetched(RecBuf, Field);

  if not Field.HasParent then begin
    case Field.DataType of
      dtObject:
        Result := TPgRow(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset))).IsNull;
      dtPgPoint,
      dtPgLSeg,
      dtPgBox,
      dtPgPath,
      dtPgPolygon,
      dtPgCircle,
      dtPgLine:
        Result := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset))).IsNull;
      dtPgInterval:
        Result := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset))).IsNull;
      dtPgDate,
      dtPgTime,
      dtPgTimeStamp,
      dtPgTimeStampTZ,
      dtPgTimeTZ:
        Result := TCustomPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset))).IsNull;
      else begin
        if Field.FieldDescKind <> fdkCalculated then
          Result := Marshal.ReadByte(RecBuf, FDataSize + Field.FieldNo - 1) = 1
        else
          Result := Marshal.ReadByte(RecBuf, FRecordSize + FCalcDataSize + Field.ActualFieldNo - 1) = 1;
        if Result then
          Result := GetNullByBlob(Field, RecBuf);
      end;
    end;
  end
  else
    Result := GetChildFieldIsNull(Field, RecBuf);
end;

{$ENDIF}

procedure TPgSQLRecordSet.SetToEnd;
begin
  FetchAll;

  inherited;
end;

function TPgSQLRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prFetchRows: begin
      Result := inherited SetProp(Prop, Value);
      if (FFetchRows <> Value) and (FFetchCursor <> nil) and (FFetchCursor.FStmtHandle <> nil) then begin
        FFetchCursor.FStmtHandle.FetchRows := Value;
        CalcPrefetchRows(FFetchCursor.FStmtHandle);
      end;
    end;
    prPrefetchRows: begin
      if FPrefetchRows <> Value then begin
        FPrefetchRows := Value;
        if (FFetchCursor <> nil) and (FFetchCursor.FStmtHandle <> nil) then
          CalcPrefetchRows(FFetchCursor.FStmtHandle);
      end;
    end;
    prOIDAsInt:
      TPgSQLCommand(GetCommand).FOIDAsInt := Value;
    prCacheBlobs:
      FCacheBlobs := Value;
    prDeferredBlobRead:
      FDeferredBlobRead := Value;
    prCommandTimeout: begin
      Assert(FCommand <> nil);
      TPgSQLCommand(GetCommand).FCommandTimeout := Value;
    end;
    prCursorAsString:
      FCursorAsString := Value;
    prUnknownAsString:
      FUnknownAsString := Value;
    prFieldsAsText:
      TPgSQLCommand(GetCommand).FFieldsAsText := Value;
    prSimpleQueryExecute:
      if Value then
        TPgSQLCommand(GetCommand).FForcedProtocol := pv20
      else
        TPgSQLCommand(GetCommand).FForcedProtocol := pvAuto;
    prUseParamTypes:
      TPgSQLCommand(GetCommand).FUseParamTypes := Value;
    prCursorWithHold: begin
      // Command should be unprepared on CursorWithHold prop changed
      if FCursorWithHold <> Value then
        UnPrepare;
      FCursorWithHold := Value;
    end;
    prDistinctParams: if TPgSQLCommand(GetCommand).FDistinctParams <> Value then begin
      UnPrepare;
      FCommand.SetProp(prDistinctParams, Value);
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TPgSQLRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prOIDAsInt:
      Value := TPgSQLCommand(GetCommand).FOIDAsInt;
    prCacheBlobs:
      Value := FCacheBlobs;
    prDeferredBlobRead:
      Value := FDeferredBlobRead;
    prCommandTimeout: begin
      Assert(FCommand <> nil);
      Value := TPgSQLCommand(GetCommand).FCommandTimeout;
    end;
    prUnknownAsString:
      Value := FUnknownAsString;
    prFieldsAsText:
      Value := TPgSQLCommand(GetCommand).FFieldsAsText;
    prSimpleQueryExecute:
      Value := TPgSQLCommand(GetCommand).FForcedProtocol = pv20;
    prUseParamTypes:
      Value := TPgSQLCommand(GetCommand).FUseParamTypes;
    prCursorWithHold:
      Value := FCursorWithHold;
    prDistinctParams:
      Value := TPgSQLCommand(GetCommand).FDistinctParams;
  else
    Result := inherited GetProp(Prop, Value);
  end
end;

procedure TPgSQLRecordSet.CreateCommand;
begin
  SetCommand(TPgSQLCommand.Create);
end;

procedure TPgSQLRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  if GetCommand <> nil then
    TPgSQLCommand(GetCommand).FRecordSet := Self;
end;

function TPgSQLRecordSet.UsedConnection: TPgSQLConnection;
begin
  Result := TPgSQLCommand(GetCommand).FUsedConnection;
end;

procedure TPgSQLRecordSet.InternalPrepare;
begin
  inherited;

  InitFetchCursor;
end;

procedure TPgSQLRecordSet.InternalUnPrepare;
begin
  if not TPgSQLCommand(GetCommand).NativeCursor then
    try
      FCommand.Unprepare;
    finally
      FCommand.SetCursor(nil);
    end;

  inherited;

  FCommand.CommandType := ctUnknown;
  FExplicitlyPrepared := False;
end;

procedure TPgSQLRecordSet.InitFetchCursor;
var
  RefCursor: TPgCursor;
  Command: TPgSQLCommand;
begin
  RefCursor := nil;
  Command := TPgSQLCommand(GetCommand);
  if Command.NativeCursor and (Command.FOutParamsCount > 0) then
    RefCursor := Command.GetFirstCursor;

  if RefCursor <> nil then begin
    if Command.GetCursorState < csExecuted then
      Command.Execute();

    Command.SetCursor(RefCursor);
    try
      Command.Prepare;
    except
      Command.SetCursor(nil);
      Command.Unprepare;
      raise;
    end;
    FFetchCursor := RefCursor;
  end
  else
    FFetchCursor := Command.FCursorRef;

  if Command.FUseSimpleProtocol then begin // no fields on prepare
    if not Command.NativeCursor then
      Command.CommandType := ctCursor
    else
      Command.CommandType := ctStatement;
  end
  else
    SetCommandType;
end;

function TPgSQLRecordSet.NeedInitFieldsOnFetch: boolean;
begin
  Result := not TPgSQLCommand(GetCommand).NativeCursor or
            inherited NeedInitFieldsOnFetch;
end;

procedure TPgSQLRecordSet.DetectIdentityField;
var
  FieldDesc: TPgSQLFieldDesc;
  i: integer;
begin
  FIdentityField := nil;

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TPgSQLFieldDesc(FFields[i]);
    if (CompareText(FieldDesc.ActualName, 'OID') = 0) and (FieldDesc.TableCol <= 0) then
      if FieldDesc.TableInfo = UpdatingTableInfo then begin
        FIdentityField := FieldDesc;
        Exit;
      end;
  end;
end;

procedure TPgSQLRecordSet.CreateFieldDescs;

  procedure DescribeFieldDesc(Field: TPgSQLFieldDesc; TableCol, TypeOid, TypeModifier: integer);
  var
    DBType, DataType, SubDataType: word;
    Length, Scale, Size: integer;
    Fixed: boolean;
    ObjectType: TObjectType;
  begin
    DetectFieldType(Field.Name, TableCol, TypeOid, TypeModifier,
      DBType, DataType, SubDataType, Length, Scale, Size, Fixed, ObjectType);

    Field.DBLength := Length;
    Field.DBScale := Scale;
  {$IFNDEF LITE}
    Field.DBType := DBType;
  {$ENDIF}
    Field.FIsTextMode := TPgSQLCommand(GetCommand).GetFormatCode(DataType, TypeModifier, Scale, True) = 0;
    Field.DataType := DataType;
    Field.SubDataType := SubDataType;
    Field.Size := Size;
    Field.Length := Length;
    Field.Scale := Word(Scale);
    Field.Fixed := Fixed;
    Field.ObjectType := ObjectType;
  end;

var
  OldCursorState: TCursorState;
  Field: TPgSQLFieldDesc;
  PgSQLItemDescs: TPgSQLItemDescs;
  FieldsCount: Integer;
  ItemDesc: TPgSQLItemDesc;
  i: integer;
begin
  OldCursorState := FCommand.GetCursorState;
  if OldCursorState = csInactive then begin
    if not TPgSQLCommand(GetCommand).UseSimpleProtocol then
      InternalPrepare
    else begin
      FDescribeExecute := True;
      try
        ExecCommand(1, 0);
      finally
        FDescribeExecute := False;
      end;
    end;
  end;

  try
    if FCommand.CommandType <> ctCursor then
      raise Exception.Create(SNotRows);

    UsedConnection.FProtocol.DescribeFields(FFetchCursor.FStmtHandle, PgSQLItemDescs);

    FieldsCount := Length(PgSQLItemDescs);
    SetLength(FActualFields, FieldsCount);
    for i := 0 to FieldsCount - 1 do begin
      Field := TPgSQLFieldDesc(CreateFieldDesc);
      ItemDesc := PgSQLItemDescs[i];

      Field.Name := ItemDesc.FieldName;
      Field.ActualName := Field.Name;
      Field.FieldNo := FFields.Count + 1;
      Field.TableOID := ItemDesc.TableOid;
      Field.TableCol := ItemDesc.TableCol;

      DescribeFieldDesc(Field, ItemDesc.TableCol, ItemDesc.TypeOid, ItemDesc.TypeModifier);
      Field.ActualFieldNo := i;
      FActualFields[i] := i;
      FFields.Add(Field);

      if Field.DataType = dtObject then
        InitObjectFields(Field.ObjectType, Field);
    end;

    FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, FTablesInfo);
    FTablesInfo.CaseSensitive := True;

    if (FTablesInfo.Count > 0) and
       (FExtendedFieldsInfo or FDefaultValues or (FFieldOrigins <> foNone))
    then begin
      FNeedExtFieldsInfo.NeedInfo := True;
      if not (FCommand.GetCursorState in [csExecuted, csFetching, csFetchingAll]) {$IFNDEF LITE}or (FSmartFetchState = sfMetaInfo){$ENDIF} then
        GetExtFieldsInfo
      else {$IFNDEF LITE}if FSmartFetchState = sfNone then{$ENDIF}
        FNeedExtFieldsInfo.InfoThread := RequestExtFieldsInfo;
    end
    else
      FNeedExtFieldsInfo.NeedInfo := False;
  finally
    if OldCursorState = csInactive then
      InternalUnPrepare;
  end;
end;

procedure TPgSQLRecordSet.GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean);
begin
  if Field.DataType in [dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine,
                        dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ, dtPgTimeTZ,
                        dtPgInterval, dtPgLargeObject, dtObject] then
    CopyBuffer(DataBuf, Dest, sizeof(IntPtr))
  else
    inherited;
end;

procedure TPgSQLRecordSet.PutFieldData(Field: TFieldDesc; DataBuf: IntPtr; DataLenPtr: PWord; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False);
var
  i, pw: Integer;
  d: Double;
begin
  inherited PutFieldData(Field, DataBuf, DataLenPtr, ValuePtr, ValueLen, NeedConvert, IsDatabaseValue);

  // fix storing TimeStamp(0) .. TimeStamp(5) data types
  if (Field.SubDataType in [dtPgTime, dtPgTimeTZ, dtPgTimeStamp, dtPgTimeStampTZ]) and
     (SmallInt(Field.Scale) < 0)
  then begin
    pw := 86400;
    i := SmallInt(Field.Scale);
    while i > -6 do begin
      pw := pw * 10;
      Dec(i);
    end;
  {$IFDEF FPC}
    if Field.SubDataType in [dtPgTime, dtPgTimeTZ] then begin
      d := Marshal.ReadInt32(DataBuf) / MSecsPerDay;
      d := Round(d * pw) / pw;
      Marshal.WriteInt32(DataBuf, Round(d * MSecsPerDay));
    end
    else begin
  {$ENDIF}
      d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(DataBuf));
      d := Round(d * pw) / pw;
      Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(d));
  {$IFDEF FPC}
    end;
  {$ENDIF}
  end;
end;

procedure TPgSQLRecordSet.GetMappedFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: variant; UseRollback: boolean = False; FlatRecBuf: boolean = False);
{$IFNDEF LITE}
var
  PgGeometric: TPgGeometric;
  PgTimeStamp: TCustomPgTimeStamp;
  PgInterval: TPgInterval;
{$ENDIF}
begin
{$IFNDEF LITE}
  case Field.DataType of
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine: begin
      PgGeometric := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
      Value := PgGeometric.AsString;
    end;
    dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ, dtPgTimeTZ: begin
      PgTimeStamp := TCustomPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
      Value := PgTimeStamp.AsString;
    end;
    dtPgInterval: begin
      PgInterval := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
      Value := PgInterval.AsString;
    end;
  else
{$ENDIF}
    inherited;
{$IFNDEF LITE}
  end;
{$ENDIF}
end;

procedure TPgSQLRecordSet.DetectFieldType(const FieldName: string; TableCol, TypeOid, TypeModifier: integer;
  var DBType, DataType, SubDataType: word; var Length, Scale, Size: integer; var Fixed: boolean; var ObjectType: TObjectType);
{$IFNDEF LITE}
var
  FetchConverter: TFetchConverter;
{$ENDIF}
begin
  UsedConnection.FTypes.DecodeTypeLength(TypeOid, TypeModifier, Length, Scale);

{$IFNDEF LITE}
  DBType := TPgConverterManager.GetDBType(TypeOid);
  FetchConverter := GetMapFetchConverter(FieldName, DBType, Length, Scale);
{$ENDIF}

  UsedConnection.FTypes.DetectDataType(TypeOid, {$IFNDEF LITE}FetchConverter,{$ENDIF}
    DataType, SubDataType, Length, Scale, Size, Fixed, ObjectType,
    FLongStrings, FFlatBuffers, TPgSQLCommand(GetCommand).FOIDAsInt,
    TPgSQLCommand(GetCommand).EnableBCD, TPgSQLCommand(GetCommand).EnableFMTBCD, FCursorAsString,
    FUnknownAsString, TPgSQLCommand(GetCommand).FFieldsAsText);

  // OID column
  if (CompareText(FieldName, 'OID') = 0) and
    (TableCol < 0) and
    (DataType = dtPgLargeObject)
  then
    DataType := dtInteger;
end;

procedure TPgSQLRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  try
    inherited;
  except
    ReleaseExtFieldsInfo;
    raise;
  end;

  if FNeedExtFieldsInfo.NeedInfo then begin
    if FNeedExtFieldsInfo.InfoThread <> nil then
      ReceiveExtFieldsInfo(FNeedExtFieldsInfo.InfoThread)
    else
      GetExtFieldsInfo;
    DoAfterFetch;
  end;
end;

procedure TPgSQLRecordSet.InternalClose;
begin
  TPgSQLCommand(GetCommand).PerformClosePortal;

  inherited;

  if not Prepared then
    InternalUnprepare;
end;

class function TPgSQLRecordSet.IsBlobDataType(DataType: word): boolean;
begin
  Result := (DataType = dtPgLargeObject) or inherited IsBlobDataType(DataType);
end;

class function TPgSQLRecordSet.IsObjectDataType(DataType: word): boolean;
begin
  Result := (DataType = dtObject);
end;

class function TPgSQLRecordSet.IsSharedObjectDataType(DataType: word): boolean;
begin
  Result := IsBlobDataType(DataType) or IsObjectDataType(DataType) or
            (DataType in [dtCursor,
                          dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ, dtPgInterval,
                          dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle,
                          dtPgLine]);
end;

procedure TPgSQLRecordSet.SetCommandType;
begin
  if TPgSQLCommand(GetCommand).RowsReturn then
    FCommand.CommandType := ctCursor
  else
    FCommand.CommandType := ctStatement;
end;

procedure TPgSQLRecordSet.CalcPrefetchRows(StmtHandle: TPgSQLStatement);
const
  MaxAutoPrefetch = 50;
begin
{$IFNDEF LITE}
  if FSmartFetchState <> sfNone then
    StmtHandle.PrefetchRows := 0
  else
{$ENDIF}
  if FPrefetchRows < 0 then
    StmtHandle.PrefetchRows := 0
  else if FPrefetchRows = 0 then
    if FFetchRows > MaxAutoPrefetch then
      StmtHandle.PrefetchRows := MaxAutoPrefetch
    else
      StmtHandle.PrefetchRows := FFetchRows
  else
    StmtHandle.PrefetchRows := FPrefetchRows;
end;

procedure TPgSQLRecordSet.ReceiveFetchBuffer(Source: TPgBufferProvider; Size: integer;
  FetchBlock: IntPtr; Row, Col: integer);
var
  Field: TPgSQLFieldDesc;
  IsNull: boolean;
  RecBuf, Buf: IntPtr;
  HeapBuffer: IntPtr;
  DataBuf: IntPtr;
  DataLenPtr: PWord;
begin
  Field := TPgSQLFieldDesc(FFields[FActualFields[Col]]);
  RecBuf := PtrOffset(FetchBlock, SizeOf(TBlockHeader) + (RecordSize + SizeOf(TItemHeader)) * Row + SizeOf(TItemHeader));

  CreateComplexField(RecBuf, Field);

    DataBuf := PtrOffset(RecBuf, Field.DataOffset);
    if Field.HasValueLen then
      DataLenPtr := PtrOffset(RecBuf, Field.Offset)
    else
      DataLenPtr := nil;

  if (Field.ActualFieldNo > -1) then
    IsNull := Size = -1
  else
    IsNull := True;

  SetNull(Field, RecBuf, IsNull);

  if not IsNull then begin
    case Field.DataType of
      dtExtString: begin
        HeapBuffer := StringHeap.NewBuf(Size + 1);
        Buf := HeapBuffer;
      end;
      dtExtWideString: begin
        HeapBuffer := StringHeap.NewBuf((Size + 1) * 2);
        Buf := HeapBuffer;
      end;
    else
      HeapBuffer := nil;
      Buf := DataBuf;
    end;

    if Buf <> nil then
      if Field.FIsTextMode then
        TPgTextConverter.ReadValue(Source, Size, Buf, Field.DataType, Field.SubDataType,
          Field.Fixed and TrimFixedChar, UsedConnection)
      else
        TPgBinaryConverter.ReadValue(Source, Size, Buf, Field.DataType, Field.SubDataType,
          Field.Fixed and TrimFixedChar, UsedConnection);

    if HeapBuffer <> nil then
      Marshal.WriteIntPtr(DataBuf, HeapBuffer);

    if DataLenPtr <> nil then
      DataLenPtr^ := Word(Size);
  end;
end;

procedure TPgSQLRecordSet.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);
var
  Command: TPgSQLCommand;
  Protocol: TPgSQLProtocol;
begin
  Command := TPgSQLCommand(GetCommand);
  Protocol := Command.FUsedConnection.FProtocol;

  // read first block always without thread
  if FIsFetchStart then begin
  {$IFDEF DEBUG_MULTITHREAD}
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Recordset $' + IntToHex(Integer(Self), 8) + ' FetchBlock PREFETCH'));
  {$ENDIF}
    FIsFetchStart := False;
    RowsObtained := Protocol.FetchStmt(FFetchCursor.FStmtHandle, FFetchRows, 0, Block, ReceiveFetchBuffer);
    FNoFetchData := Protocol.NoData(FFetchCursor.FStmtHandle){$IFNDEF LITE} or (FSmartFetchState = sfDataByKey){$ENDIF};
    if not FNoFetchData then begin
      FFetchCursor.FStmtHandle.ReadStream.Reset(Protocol);
      FFetchCursor.FStmtHandle.ReadStream.UseUnicode := Protocol.Net.UseUnicode;
      if Command.NeedReader then
        Protocol.Reader.Run(FFetchCursor.FStmtHandle.ReadStream)
      else if FFetchCursor.FStmtHandle.PrefetchRows > 0 then begin
        FPrefetchedRowCount := FFetchCursor.FStmtHandle.ReadStream.PrefetchStmt(Protocol);
        FAllRowsPrefetched := FPrefetchedRowCount >= FFetchCursor.FStmtHandle.PrefetchRows;
      end;
    end;
  end
  else if not Command.NeedReader then begin
  {$IFDEF DEBUG_MULTITHREAD}
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Recordset $' + IntToHex(Integer(Self), 8) + ' FetchBlock'));
  {$ENDIF}

    if FPrefetchedRowCount >= FFetchRows then begin
      RowsObtained := FFetchCursor.FStmtHandle.ReadStream.FetchStmt(Protocol, FFetchRows, Block, ReceiveFetchBuffer);
      FPrefetchedRowCount := FPrefetchedRowCount - RowsObtained;
    end
    else if FPrefetchedRowCount > 0 then begin
      RowsObtained := FFetchCursor.FStmtHandle.ReadStream.FetchStmt(Protocol, FPrefetchedRowCount, Block, ReceiveFetchBuffer);
      FPrefetchedRowCount := FPrefetchedRowCount - RowsObtained;
    end
    else
      RowsObtained := 0;

    if FPrefetchedRowCount > 0 then
      FNoFetchData := False
    else if not FAllRowsPrefetched then
      FNoFetchData := True
    else if FFetchRows - RowsObtained > 0 then begin
      RowsObtained := Protocol.FetchStmt(FFetchCursor.FStmtHandle, FFetchRows, RowsObtained, Block, ReceiveFetchBuffer);
      FNoFetchData := Protocol.NoData(FFetchCursor.FStmtHandle){$IFNDEF LITE} or (FSmartFetchState = sfDataByKey){$ENDIF};
      if not FNoFetchData and (FFetchCursor.FStmtHandle.PrefetchRows > 0) then begin
        FFetchCursor.FStmtHandle.ReadStream.IsLastRow := False;
        FPrefetchedRowCount := FFetchCursor.FStmtHandle.ReadStream.PrefetchStmt(Protocol);
        FAllRowsPrefetched := FPrefetchedRowCount >= FFetchCursor.FStmtHandle.PrefetchRows
      end;
    end
    else
      FNoFetchData := False;
  end
  else begin
  {$IFDEF DEBUG_MULTITHREAD}
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Recordset $' + IntToHex(Integer(Self), 8) + ' FetchBlock'));
  {$ENDIF}

    RowsObtained := FFetchCursor.FStmtHandle.ReadStream.FetchStmt(Protocol, FFetchRows, Block, ReceiveFetchBuffer);
    if not FFetchCursor.FStmtHandle.ReadStream.IsLastRow then
      FNoFetchData := False
    else
      FNoFetchData := not FFetchCursor.FStmtHandle.ReadStream.HasData;
  end;

  if FNoFetchData and not Protocol.HasMoreResultSets then
    Command.SetCursorState(csFetched);

  Command.UpdateRowsAffected;
end;

procedure TPgSQLRecordSet.ProcessFetchedBlock(Block: PBlockHeader; FetchBack: boolean);

  procedure ReadBlobs(FirstItem: PItemHeader);
  var
    i: integer;
    Item: PItemHeader;
    ObjPtr: IntPtr;
    SharedPiece: PPieceHeader;
    Lob: TPgSQLLargeObject;
    Field: TFieldDesc;
  begin
    Item := FirstItem;
    SharedPiece := nil;
    try
      while IntPtr(Item) <> nil do begin
        for i := 0 to FFields.Count - 1 do begin
          Field := FFields[i];
          if (Field.DataType = dtPgLargeObject) then begin
            ObjPtr := Marshal.ReadIntPtr(Item, SizeOf(TItemHeader) + Field.DataOffset);
            Lob := TPgSQLLargeObject(GetGCHandleTarget(ObjPtr));
            if Lob.IsCreated then begin
              if IntPtr(SharedPiece) = nil then
                TBlob.AllocPiece(SharedPiece, DefaultPieceSize);
              Lob.ReadBlob(SharedPiece);
            end;
          end;
        end;
        Item := Item.Next;
      end;
    finally
      if IntPtr(SharedPiece) <> nil then
        Marshal.FreeHGlobal(SharedPiece);
    end;
  end;

begin
  if not FDeferredBlobRead and FCacheBlobs then begin
    if not FFetchAll then
      ReadBlobs(PtrOffset(Block, sizeof(TBlockHeader)))
    else
    if FCommand.GetCursorState = csFetched then
      ReadBlobs(FirstItem);
  end;
end;

function TPgSQLRecordSet.ProcessFetchedException(E: Exception): boolean;
begin
  if E is EpgError then begin
    FCommand.SetCursorState(csFetched);
    TPgSQLCommand(GetCommand).PerformClosePortal;
    UsedConnection.ProcessInternalException(EPgError(E), Component);
    Result := True;
  end
  else
    Result := False;
end;

function TPgSQLRecordSet.NeedUnPrepareAfterFetch: boolean;
begin
  Result := not Prepared;
end;

procedure TPgSQLRecordSet.DoBeforeFetch(out Cancel: boolean);
var
  Fetched: boolean;
begin
  inherited;

  if Cancel then begin
    if UsedConnection.FMultipleConnections then
      UsedConnection.FProtocol.Lock;
    try
      repeat
        Fetched := UsedConnection.FProtocol.FetchStmt(FFetchCursor.FStmtHandle, FFetchRows, 0, nil, nil) > 0;
        FNoFetchData := UsedConnection.FProtocol.NoData(FFetchCursor.FStmtHandle);
      until FNoFetchData or not Fetched or not FFetchAll;

      FCommand.SetCursorState(csFetched);
    finally
      if UsedConnection.FMultipleConnections then
        UsedConnection.FProtocol.Unlock;
    end;
  end;
end;

{$IFNDEF LITE}
procedure TPgSQLRecordSet.SyncKeyFields(KeyFields, OriginFields: TFieldDescs);
var
  FieldsCount, i, ActualFieldNo: integer;
begin
  inherited;

  FieldsCount := OriginFields.Count;
  SetLength(FActualFields, FieldsCount);

  for i := 0 to FieldsCount - 1 do
    FActualFields[i] := -1;

  for i := 0 to FieldsCount - 1 do begin
    ActualFieldNo := OriginFields[i].ActualFieldNo;
    if ActualFieldNo <> -1 then
      FActualFields[ActualFieldNo] := i;
  end;
end;

procedure TPgSQLRecordSet.SyncDataFields(DataFields, OriginFields: TFieldDescs);
var
  FieldsCount, i, ActualFieldNo: integer;
begin
  inherited;

  FieldsCount := OriginFields.Count;
  SetLength(FActualFields, FieldsCount);

  for i := 0 to FieldsCount - 1 do
    FActualFields[i] := -1;

  for i := 0 to FieldsCount - 1 do begin
    ActualFieldNo := OriginFields[i].ActualFieldNo;
    if ActualFieldNo <> -1 then
      FActualFields[ActualFieldNo] := i;
  end;
end;
{$ENDIF}

function TPgSQLRecordSet.IsSupportedDataType(DataType: word): boolean;
begin
  Result := DataType in [dtString .. dtWideMemo, dtPgDate .. dtPgLine];
end;

function TPgSQLRecordSet.IsSpecificType(const Field: TFieldDesc; var DataType: word; var DataTypeName: string; var Len, Scale: integer): boolean;
begin
  Result := True;

  case Field.DataType of
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle, dtPgLine: begin
      DataType := dtMemo;
      DataTypeName := 'MEMO';
      Len := -1;
      Scale := -1;
    end;
    dtPgDate, dtPgTime, dtPgTimeStamp, dtPgTimeStampTZ, dtPgInterval: begin
      DataType := dtString;
      DataTypeName := 'VARCHAR';
      Len := 255;
      Scale := -1;
    end;
  else
    Result := inherited IsSpecificType(Field, DataType, DataTypeName, Len, Scale);
  end;
end;

procedure TPgSQLRecordSet.GetExtFieldsInfo;
var
  SQL: String;
  Command: TPgSQLCommand;
  Connection: TPgSQLConnection;
  ColumnsInfo: TCRColumnsInfo;
  ServerVersionIs81: Boolean;
  UseProtocol30: Boolean;
  RecordSet: TCRRecordSet;
begin
  FNeedExtFieldsInfo.NeedInfo := False;

  Command := TPgSQLCommand(GetCommand);
  Connection := Command.FConnection;

  ColumnsInfo := GetColumnInfo(Command.SQL, Connection.SQLInfo, FTablesInfo);
  try
    RecordSet := Connection.GetRecordSet;
    try
      RecordSet.SetProp(prFetchAll, True);
      RecordSet.SetProp(prOIDAsInt, True);

      ServerVersionIs81 := TPgSQLConnection(Connection).VersionIsEqualOrHigher(8, 1);
      UseProtocol30 := Command.FConnection.FProtocolVersion <> pv20;
      SQL := TPgSQLRecordSet.GetExtFieldsInfoSQL(FTablesInfo, UseProtocol30, ServerVersionIs81, FDefaultValues);


      RecordSet.SetSQL(SQL);
      RecordSet.Open;

      ProcessExtFieldsInfo(RecordSet, ColumnsInfo, FTablesInfo, FFields, UseProtocol30);
    finally
      RecordSet.SetProp(prOIDAsInt, False);
      Connection.ReleaseRecordSet(RecordSet);
    end;
  finally
    ColumnsInfo.Free;
  end;
end;

function TPgSQLRecordSet.RequestExtFieldsInfo: TPgExtFieldsInfoThread;
begin
  Result := TPgSQLCommand(GetCommand).FConnection.GetExtFieldInfoThread(
    FTablesInfo,
    FDefaultValues,
    TPgSQLCommand(GetCommand).FConnection.FProtocolVersion <> pv20
  );
end;

procedure TPgSQLRecordSet.ReceiveExtFieldsInfo(InfoThread: TPgExtFieldsInfoThread);
var
  Command: TPgSQLCommand;
  ColumnsInfo: TCRColumnsInfo;
  RecordSet: TCRRecordSet;
begin
  try
    FNeedExtFieldsInfo.NeedInfo := False;

    Command := TPgSQLCommand(GetCommand);
    // get ColumnInfo before FExtFieldsInfoThread.WaitForResult for performance
    ColumnsInfo := GetColumnInfo(Command.SQL, Command.SQLInfo, FTablesInfo);
    try
      InfoThread.WaitForResult;
      InfoThread.CheckLastError;

      RecordSet := InfoThread.RecordSet;
      if RecordSet = nil then
        Exit;

      ProcessExtFieldsInfo(RecordSet, ColumnsInfo, FTablesInfo, FFields, InfoThread.UseProtocol30);
    finally
      ColumnsInfo.Free;
    end;
  finally
    ReleaseExtFieldsInfo;
  end;
end;

procedure TPgSQLRecordSet.ReleaseExtFieldsInfo;
begin
  TPgSQLCommand(GetCommand).FConnection.ReleaseExtFieldInfoThread(FNeedExtFieldsInfo.InfoThread);
end;

class function TPgSQLRecordSet.GetColumnInfo(const SQL: string; SQLInfo: TSQLInfo; TablesInfo: TCRTablesInfo): TCRColumnsInfo;
var
  i, j: Integer;
  ColInfo: TCRColumnInfo;
begin
  Result := TCRColumnsInfo.Create;
  try
    SQLInfo.ParseColumnsInfo(SQL, Result);

    // Parse tables info in select list
    for i := 0 to Result.Count - 1 do begin
      ColInfo := Result[i];
      if ColInfo.Table <> '' then
        for j := 0 to TablesInfo.Count - 1 do
          if (ColInfo.Table = TablesInfo[j].TableAlias) or
            (ColInfo.Table = TablesInfo[j].TableName)
          then begin
            ColInfo.TableIndex := j;
            Break;
          end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TPgSQLRecordSet.GetExtFieldsInfoSQL(TablesInfo: TCRTablesInfo; UseProtocol30, ServerVersionIs81, DefaultValues: Boolean): string;
var
  i: integer;
  SchemaName: string;
  TableName: string;
  SQL: StringBuilder;
  CaseSQL: StringBuilder;
  WhereSQL: StringBuilder;
  Info: TSQLObjectInfo;
begin
  SQL := StringBuilder.Create(4096);
  CaseSQL := StringBuilder.Create(4096);
  WhereSQL := StringBuilder.Create(4096);
  try
    for i := 0 to TablesInfo.Count - 1 do begin
      PgSQLInfo.SplitObjectName(TablesInfo.Items[i].TableName, Info);

      TableName := PgSQLInfo.ToStringConst(PgSQLInfo.NormalizeName(Info.Name, False, True));
      CaseSQL.Append(' when relname = ' + TableName + ' then ' + IntToStr(i));
      if i > 0 then
        WhereSQL.Append(' or ');

      if UseProtocol30 then
        WhereSQL.Append('(pc.relname = ' + TableName + ')')
      else begin
        WhereSQL.Append('((pc.relname = ' + TableName + ')');
        if Info.Schema <> '' then begin
          SchemaName := PgSQLInfo.ToStringConst(PgSQLInfo.NormalizeName(Info.Schema, False, True));
          WhereSQL.Append(' and (pn.nspname = ' + SchemaName + '))')
        end
        else
          WhereSQL.Append(' and (pn.nspname = current_schema()))')
      end;
    end;

    SQL.Append('select');

    // select list
    SQL.Append(' pa.attname, case '  + CaseSQL.ToString + ' end as idx, pa.attnotnull');
    if DefaultValues then
      SQL.Append(', pg_get_expr(pd.adbin, pd.adrelid) as adsrc')
    else
      SQL.Append(', NULL as adsrc');
    SQL.Append(', pi.indisprimary');
    if UseProtocol30 then
      SQL.Append(', pc.oid, pa.attnum');

    // from
    SQL.Append(' from pg_attribute pa');

    // join pg_class
    SQL.Append(' join pg_catalog.pg_class pc on (pa.attrelid = pc.oid)');
    // join pg_namespace
    if not UseProtocol30 then
      SQL.Append(' join pg_catalog.pg_namespace pn on (pc.relnamespace = pn.oid)');
    // join pg_attrdef
    if DefaultValues then
      SQL.Append(' left join pg_catalog.pg_attrdef pd on (pd.adnum = pa.attnum) and (pd.adrelid = pc.oid)');
    SQL.Append(' left join pg_catalog.pg_index pi on (pi.indrelid = pc.oid) and ');
    if ServerVersionIs81 then
      SQL.Append('(pa.attnum = any(pi.indkey))')
    else
      SQL.Append('(pa.attnum in (pi.indkey[0], pi.indkey[1], pi.indkey[2], pi.indkey[3], pi.indkey[4], pi.indkey[5], pi.indkey[6], pi.indkey[7], '+
                 'pi.indkey[8], pi.indkey[9], pi.indkey[10], pi.indkey[11], pi.indkey[12], pi.indkey[13], pi.indkey[14], pi.indkey[15]))');

    // where
    SQL.Append(' where (pa.attnum > 0)');
    SQL.Append(' and (' + WhereSQL.ToString + ')');

    // order by
    SQL.Append(' order by idx, pa.attnum');

    Result := SQL.ToString;
  finally
    SQL.Free;
    CaseSQL.Free;
    WhereSQL.Free;
  end;
end;

class procedure TPgSQLRecordSet.ProcessExtFieldsInfo(RecordSet: TCRRecordSet; ColumnsInfo: TCRColumnsInfo;
  TablesInfo: TCRTablesInfo; Fields: TFieldDescs; UseProtocol30: boolean);

  function Locate1(RecordSet: TCRRecordSet; RecBuf: IntPtr; Field: TFieldDesc; Value: string): boolean;
  var
    v: variant;
  begin
    RecordSet.SetToBegin;
    while True do begin
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        break;
      RecordSet.GetFieldAsVariant(Field, RecBuf, v);
      if VarToStr(v) = Value then begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;

  function Locate2(RecordSet: TCRRecordSet; RecBuf: IntPtr; Field1, Field2: TFieldDesc; Value1, Value2: integer): boolean;
  var
    v: variant;
  begin
    RecordSet.SetToBegin;
    while True do begin
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        break;
      RecordSet.GetFieldAsVariant(Field1, RecBuf, v);
      if v = Value1 then begin
        RecordSet.GetFieldAsVariant(Field2, RecBuf, v);
        if v = Value2 then begin
          Result := True;
          exit;
        end;
      end;
    end;
    Result := False;
  end;

const
  fnNAME = 0;
  fnIDX  = 1;
  fnNOTNULL = 2;
  fnDEFAULT = 3;
  fnPRIMARY = 4;
  fnOID  = 5;
  fnNUM  = 6;
var
  i, j, p: integer;
  RedhisftConnection: Boolean;
  RecBuf: IntPtr;
  ColInfo: TCRColumnInfo;
  Value: variant;
  DefVal: string;
  FieldDesc: TPgSQLFieldDesc;
  AsteriskTableIndex: integer;
  Located: boolean;

  FieldNAME: TFieldDesc;
  FieldIDX: TFieldDesc;
  FieldNOTNULL: TFieldDesc;
  FieldDEFAULT: TFieldDesc;
  FieldPRIMARY: TFieldDesc;
  FieldOID: TFieldDesc;
  FieldNUM: TFieldDesc;
begin
  RedhisftConnection := TPgSQLConnection(RecordSet.GetConnection).FRedhisftConnection;

  RecordSet.AllocRecBuf(RecBuf);
  try
    FieldNAME := RecordSet.Fields[fnNAME];
    FieldIDX := RecordSet.Fields[fnIDX];
    FieldNOTNULL := RecordSet.Fields[fnNOTNULL];
    FieldDEFAULT := RecordSet.Fields[fnDEFAULT];
    FieldPRIMARY := RecordSet.Fields[fnPRIMARY];

    // supplement parsed information
    if not UseProtocol30 then begin
      p := 0;
      while p < ColumnsInfo.Count do begin
        ColInfo := ColumnsInfo[p];
        if ColInfo.Name = '*' then begin
          AsteriskTableIndex := ColInfo.TableIndex;
          ColumnsInfo.Delete(p);

          RecordSet.SetToBegin;
          while True do begin
            RecordSet.GetNextRecord(RecBuf);
            if RecordSet.Eof then
              Break;
            Value := Unassigned;
            if AsteriskTableIndex <> -1 then
              RecordSet.GetFieldAsVariant(FieldIDX, RecBuf, Value);
            if (AsteriskTableIndex = -1) or
              (Value = AsteriskTableIndex)
            then begin
              ColInfo := TCRColumnInfo.Create;
              RecordSet.GetFieldAsVariant(FieldNAME, RecBuf, Value);
              ColInfo.Name := VarToStr(Value);
              RecordSet.GetFieldAsVariant(FieldIDX, RecBuf, Value);
              ColInfo.TableIndex := Value;
              RecordSet.GetFieldAsVariant(FieldDEFAULT, RecBuf, Value);
              ColInfo.Expr := VarToStr(Value);
              RecordSet.GetFieldAsVariant(FieldNOTNULL, RecBuf, Value);
              ColInfo.Required := Boolean(Value);
              ColumnsInfo.Insert(p, ColInfo);
              Inc(p);
            end;
          end;
        end
        else begin
          Located := Locate1(RecordSet, RecBuf, FieldNAME, ColInfo.Name);
          if Located then begin
            RecordSet.GetFieldAsVariant(FieldIDX, RecBuf, Value);
            if ColInfo.TableIndex = -1 then
              ColInfo.TableIndex := Value;
            RecordSet.GetFieldAsVariant(FieldDEFAULT, RecBuf, Value);
            ColInfo.Expr := VarToStr(Value);
            RecordSet.GetFieldAsVariant(FieldNOTNULL, RecBuf, Value);
            ColInfo.Required := Boolean(Value);
          end;
          Inc(p);
        end;
      end;
    end;

    for i := 0 to Fields.Count - 1 do begin
      FieldDesc := TPgSQLFieldDesc(Fields[i]);
      if FieldDesc.HasParent or (FieldDesc.FieldDescKind <> fdkData) then
        Continue;

      // field is not represented in ParsedFDs list because:
      // a) it is expression
      // b) it belongs to the table that have not got into TablesInfo
      ColInfo := nil;
      for j := 0 to ColumnsInfo.Count - 1 do
        if ((FieldDesc.ActualName = ColumnsInfo[j].Alias) or
          (ColumnsInfo[j].Alias = '') and (FieldDesc.ActualName = ColumnsInfo[j].Name)) and
          not ColumnsInfo[j].Used
        then begin
          ColInfo := ColumnsInfo[j];
          ColInfo.Used := True;
          Break;
        end;

      if UseProtocol30 then begin
        Assert(RecordSet <> nil);
        if FieldDesc.TableOID > 0 then begin
          FieldOID := RecordSet.Fields[fnOID];
          FieldNUM := RecordSet.Fields[fnNUM];
          if Locate2(RecordSet, RecBuf, FieldOID, FieldNUM, FieldDesc.TableOID, FieldDesc.TableCol) then begin
            RecordSet.GetFieldAsVariant(FieldNAME, RecBuf, Value);
            FieldDesc.ActualName := VarToStr(Value);
            // fnIDX can be incorrect if FROM contains the same table several times
            if (ColInfo <> nil) and (ColInfo.TableIndex >= 0) then
              FieldDesc.TableInfo := TablesInfo[ColInfo.TableIndex]
            else begin
              RecordSet.GetFieldAsVariant(FieldIDX, RecBuf, Value);
              FieldDesc.TableInfo := TablesInfo.Items[Value];
            end;
            RecordSet.GetFieldAsVariant(FieldDEFAULT, RecBuf, Value);
            DefVal := VarToStr(Value);
            if (FieldDesc.DataType in [dtInteger, dtLargeint]) and (Pos('nextval', DefVal) > 0) then
              FieldDesc.IsAutoIncrement := True; // serial, bigserial
            //if FDataSet.Options.DefaultValues then
            FieldDesc.DefaultExpr := DefVal;
            RecordSet.GetFieldAsVariant(FieldPRIMARY, RecBuf, Value);
            FieldDesc.IsKey := VarToStr(Value) <> '';
            if not FieldDesc.IsAutoIncrement then begin
              RecordSet.GetFieldAsVariant(FieldNOTNULL, RecBuf, Value);
                FieldDesc.Required := Boolean(Value);
            end;
          end;
        end
        else if RedhisftConnection and (TablesInfo.Count = 1) then
          FieldDesc.TableInfo := TablesInfo[0];
      end
      else begin
        if ColInfo <> nil then begin
          if ColInfo.Name <> '' then begin
            if ColInfo.Alias <> '' then
              FieldDesc.ActualName := ColInfo.Name;

            if ColInfo.TableIndex >= 0 then
              FieldDesc.TableInfo := TablesInfo[ColInfo.TableIndex];
              DefVal := ColInfo.Expr;
              if (FieldDesc.DataType in [dtInteger, dtLargeint]) and (Pos('nextval', DefVal) > 0) then
                FieldDesc.IsAutoIncrement := True; // serial, bigserial
              //if FDataSet.Options.DefaultValues then
              FieldDesc.DefaultExpr := DefVal;
              FieldDesc.Required := not FieldDesc.IsAutoIncrement and ColInfo.Required;
          end;
        end;
      end;
    end;
  finally
    RecordSet.FreeRecBuf(RecBuf);
  end;
end;

{ TPgSQLMetaData }

function TPgSQLMetaData.GetConnection: TPgSQLConnection;
begin
  Result := TPgSQLRecordSet(FRecordSet).UsedConnection;
end;

function TPgSQLMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TPgSQLRecordSet.Create;
  Result.SetProp(prOIDAsInt, True);
  Result.SetProp(prExtendedFieldsInfo, False);
  Result.SetProp(prSimpleQueryExecute, True);
end;

function TPgSQLMetaData.InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData;
begin
  if MetaDataKind = 'sequences' then
    Result := GetSequences(Restrictions)
  else if MetaDataKind = 'schemas' then
    Result := GetSchemas(Restrictions)
  else if MetaDataKind = 'databases' then
    Result := GetDatabases(Restrictions)
  else
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions);
end;

procedure TPgSQLMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  inherited;

  List.Add('Databases');
  List.Add('DataTypes');
  List.Add('Users');
  List.Add('Sequences');
  List.Add('Schemas');

  List.Sort;
end;

procedure TPgSQLMetaData.InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string);
begin
  List.Clear;

  if MetaDataKind = 'datatypes' then begin
    List.Add('DATATYPE_SCHEMA');
    List.Add('DATATYPE_NAME');
    List.Add('DATATYPE_OID');
    List.Add('DATATYPE_TYPE');
  end
  else
  if MetaDataKind = 'sequences' then begin
    List.Add('SEQUENCE_SCHEMA');
    List.Add('SEQUENCE_NAME');
    List.Add('SEQUENCE_OID');
  end
  else
    inherited;
end;

function TPgSQLMetaData.GetTables(Restrictions: TStrings): TData;
const
  fmtGetTablesSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, n.nspname AS TABLE_SCHEMA, c.relname AS TABLE_NAME, ' +
    'c.oid AS TABLE_OID, ' +
    'CASE ' +
    '  WHEN c.relkind = ''r'' THEN ''TABLE'' ' +
    '  WHEN c.relkind = ''v'' THEN ''VIEW'' ' +
    'END::varchar(5) AS TABLE_TYPE ' +
    'FROM pg_class c ' +
    '  INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    'WHERE %s c.relkind IN (%s) ORDER BY n.nspname, c.relname';
var
  WhereClause, TableName, Schema, TableOID, TableTypes, QuotedTypes, Scope: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else if not GetConnection.FMultipleSchema then
    AddWhere(WhereClause, 'n.nspname', GetConnection.GetCachedSchema)
  else begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + '(n.nspname in (' + GetConnection.GetSchemaNamesForSQL(0) + '))';
  end;
  AddWhere(WhereClause, 'c.relname', TableName);
  AddWhere(WhereClause, 'c.oid', TableOID);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  if UpperCase(TableTypes) = 'TABLE' then
    TableTypes := 'r'
  else
  if UpperCase(TableTypes) = 'VIEW' then
    TableTypes := 'v';

  if TableTypes <> '' then
    QuotedTypes := PgSQLInfo.ToStringConst(TableTypes)
  else
    QuotedTypes := '''r'', ''v''';

  FRecordSet.SetSQL(Format(fmtGetTablesSQL, [WhereClause, QuotedTypes]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetColumns(Restrictions: TStrings): TData;
const
  fmtColumnsSQL_start =
    'SELECT ' +
    'current_database() AS TABLE_CATALOG, n.nspname AS TABLE_SCHEMA, c.relname AS TABLE_NAME, ' +
    'c.oid AS TABLE_OID, a.attname AS COLUMN_NAME, a.attnum AS POSITION, ' +
    'a.atttypid AS DATA_TYPE, a.attlen AS DATA_LENGTH, ' +
    '0 AS DATA_PRECISION, 0 AS DATA_SCALE, a.atttypmod AS TYPE_MODIFIER ';

  fmtColumnsSQL_8 =
    ', (not a.attnotnull)::integer AS NULLABLE, ';

  fmtColumnsSQL_7 =
    ', CASE WHEN a.attnotnull=TRUE THEN 1 ELSE 0 END AS NULLABLE, ';

  fmtColumnsSQL_end =
    'pg_get_expr(ad.adbin, ad.adrelid) AS DEFAULT_VALUE ' +
    'FROM pg_class c ' +
    '  INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    '  INNER JOIN pg_attribute a ON a.attrelid = c.oid ' +
    '  LEFT JOIN pg_attrdef ad ON a.attrelid = ad.adrelid and a.attnum = ad.adnum ' +
    'WHERE %s a.attnum > 0 AND not a.attisdropped ORDER BY n.nspname, c.relname, a.attnum';
var
  WhereClause, TableName, Schema, TableOID, ColumnName, SQL, Scope: string;
  Command: TPgSQLCommand;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else if not GetConnection.FMultipleSchema then
    AddWhere(WhereClause, 'n.nspname', GetConnection.GetCachedSchema)
  else begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + '(n.nspname in (' + GetConnection.GetSchemaNamesForSQL(0) + '))';
  end;

  AddWhere(WhereClause, 'c.relname', TableName);
  AddWhere(WhereClause, 'c.oid', TableOID);
  AddWhere(WhereClause, 'a.attname', ColumnName);

  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  Assert(FRecordSet <> nil);
  Command := TPgSQLCommand(FRecordSet.GetCommand);
  Assert(Command.FConnection <> nil);

  SQL := fmtColumnsSQL_start;

  if Command.FConnection.VersionIsEqualOrHigher(8, 0) then
    SQL := SQL + fmtColumnsSQL_8
  else
    SQL := SQL + fmtColumnsSQL_7;

  SQL := SQL + fmtColumnsSQL_end;
  SQL := Format(SQL, [WhereClause]);

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetProcedures(Restrictions: TStrings): TData;
const
  fmtGetProceduresSQL = 'SELECT ' +
    'current_database() AS PROCEDURE_CATALOG, ' +
    'n.nspname AS PROCEDURE_SCHEMA, p.proname AS PROCEDURE_NAME, ' +
    '''FUNCTION'' AS PROCEDURE_TYPE ' +
    'FROM pg_proc p ' +
    '  INNER JOIN pg_namespace n ON n.oid = p.pronamespace ' +
    '%s ORDER BY n.nspname, p.proname';
var
  WhereClause, ProcName, Schema, Scope: string;
begin
  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else if not GetConnection.FMultipleSchema then
    AddWhere(WhereClause, 'n.nspname', GetConnection.GetCachedSchema)
  else begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + '(n.nspname in (' + GetConnection.GetSchemaNamesForSQL(0) + '))';
  end;

  AddWhere(WhereClause, 'p.proname', ProcName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetProceduresSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

{$IFNDEF LITE}
procedure TPgSQLMetaData.CreateProcedureParametersFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 100);
  AddField('PROCEDURE_SCHEMA', dtString, 100);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PARAMETER_NAME', dtString, 100);
  AddField('POSITION', dtInt32);
  AddField('DIRECTION', dtString, 10);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  FMemData.InitFields;
end;

function TPgSQLMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
var
  Schema, ProcName, OverloadStr, Database: string;
  Overload: integer;
  ParamsInfo: TProcParamsInfo;
  i, a: integer;
  s: string;
  HasOutParams: boolean;
  Mode: char;
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
  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  if Schema <> '' then
    Schema := PgSQLInfo.NormalizeName(Schema, False, True)
  else if not GetConnection.FMultipleSchema then
    Schema := GetConnection.GetCachedSchema
  else begin
    Schema := GetConnection.GetSchemaNamesForSQL(0);
  end;

  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ProcName := PgSQLInfo.NormalizeName(ProcName, False, True);
  if ProcName = '' then
    raise Exception.Create('PROCEDURE_NAME restriction must be set');

  OverloadStr := Trim(Restrictions.Values['PROCEDURE_OVERLOAD']);
  if OverloadStr <> '' then begin
    Overload := StrToInt(OverloadStr);
    if Overload <= 0 then
      Overload := 1;
  end
  else
    Overload := 1;

  TPgSQLCommand(TPgSQLRecordSet(FRecordSet).GetCommand).GetProcParamsInfo(Schema, ProcName, Overload, ParamsInfo);

  CreateProcedureParametersFields;
  FMemData.Open;

  Database := GetConnection.FDatabase;

  HasOutParams := False;
  for i := 0 to Length(ParamsInfo.Modes) - 1 do
    if ParamsInfo.Modes[i] <> 'i' then begin
      HasOutParams := True;
      break;
    end;

  FMemDataHelper.AllocBuffer;

  if HasOutParams then
    a := 0
  else
    a := -1;

  for i := a to High(ParamsInfo.TypeOIDs) do begin
    FMemDataHelper.InitRecord;

    FMemDataHelper.FieldValues[dnCATALOG] := Database;
    FMemDataHelper.FieldValues[dnSCHEMA] := ParamsInfo.Schema;
    FMemDataHelper.FieldValues[dnPROC_NAME] := ProcName;
    if (i >= 0) and (i < Length(ParamsInfo.Names)) and (ParamsInfo.Names[i] <> '""') then
      FMemDataHelper.FieldValues[dnPARAM_NAME] := ParamsInfo.Names[i];
    FMemDataHelper.FieldValues[dnPOSITION] := i + 1;

    if i = -1 then
      Mode := 'o'
    else
    if i < Length(ParamsInfo.Modes) then begin
      s := ParamsInfo.Modes[i];
      if s <> '' then
        Mode := s[1]
      else
        Mode := 'i';
    end
    else
      Mode := 'i';

    case Mode of
      'i':
        Direction := 'IN';
      'o':
        Direction := 'OUT';
      'b':
        Direction := 'IN/OUT';
    end;
    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;

    if i = -1 then
      FMemDataHelper.FieldValues[dnDATA_TYPE] := ParamsInfo.RetTypeOid
    else
      FMemDataHelper.FieldValues[dnDATA_TYPE] := ParamsInfo.TypeOIDs[i];

    FMemDataHelper.AppendRecord;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;
{$ENDIF}

function TPgSQLMetaData.GetIndexes(Restrictions: TStrings): TData;
const
  fmtIndexesSQL_start =
    'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS TABLE_SCHEMA, t.relname AS TABLE_NAME, t.oid AS TABLE_OID, ' +
    'current_database() AS INDEX_CATALOG, ' +
    'n.nspname AS INDEX_SCHEMA, c.relname AS INDEX_NAME, c.oid AS INDEX_OID ';

  fmtIndexesSQL_7 =
    ', CASE WHEN i.indisunique=TRUE THEN 1 ELSE 0 END AS UNIQUE ';

  fmtIndexesSQL_8 =
    ', i.indisunique::integer AS UNIQUE ';

  fmtIndexesSQL_end =
    'FROM pg_index i ' +
    '  INNER JOIN pg_class c ON c.oid = i.indexrelid ' +
    '  INNER JOIN pg_class t ON t.oid = i.indrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.relnamespace ' +
    '%s ORDER BY n.nspname, t.relname, c.relname';
var
  WhereClause, TableName, TableSchema, TableOID, IndexName, IndexOID, SQL: string;
  Command: TPgSQLCommand;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  IndexOID := Trim(Restrictions.Values['INDEX_OID']);

  WhereClause := '';
  AddWhere(WhereClause, 'n.nspname', TableSchema);
  AddWhere(WhereClause, 't.relname', TableName);
  AddWhere(WhereClause, 'i.indrelid', TableOID);
  AddWhere(WhereClause, 'c.relname', IndexName);
  AddWhere(WhereClause, 'i.indexrelid', IndexOID);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  Assert(FRecordSet <> nil);
  Command := TPgSQLCommand(FRecordSet.GetCommand);
  Assert(Command.FConnection <> nil);

  SQL := fmtIndexesSQL_start;

  if Command.FConnection.VersionIsEqualOrHigher(8, 1) then
    SQL := SQL + fmtIndexesSQL_8
  else
    SQL := SQL + fmtIndexesSQL_7;

  SQL := SQL + fmtIndexesSQL_end;
  SQL := Format(SQL, [WhereClause]);

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetIndexColumns(Restrictions: TStrings): TData;
const
  // ORDER BY indisprimary::integer DESC - index for Primary Key must be first

  fmtGetIndexColumnsSQL7 = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS TABLE_SCHEMA, t.relname AS TABLE_NAME, t.oid AS TABLE_OID, ' +
    'current_database() AS INDEX_CATALOG, ' +
    'n.nspname AS INDEX_SCHEMA, c.relname AS INDEX_NAME, c.oid AS INDEX_OID, ' +
    'a.attname AS COLUMN_NAME, a.attnum AS COLUMN_POSITION, 0 AS DESCENDING ' +
  {$IFDEF LITE}
    ', CASE WHEN i.indisunique=TRUE THEN 1 ELSE 0 END AS UNIQUE ' +
  {$ENDIF}
    'FROM pg_index i ' +
    '  INNER JOIN pg_class c ON c.oid = i.indexrelid ' +
    '  INNER JOIN pg_class t ON t.oid = i.indrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.relnamespace ' +
    '  INNER JOIN pg_attribute a ON a.attrelid = i.indrelid and ' +
    '  a.attnum in (i.indkey[0], i.indkey[1], i.indkey[2], i.indkey[3], i.indkey[4], i.indkey[5], i.indkey[6], i.indkey[7], '+
    '               i.indkey[8], i.indkey[9], i.indkey[10], i.indkey[11], i.indkey[12], i.indkey[13], i.indkey[14], i.indkey[15]) ' +
    '%s ORDER BY CASE WHEN indisprimary THEN 0 ELSE 1 END, n.nspname, t.relname, c.relname, a.attnum';

  fmtGetIndexColumnsSQL81 = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS TABLE_SCHEMA, t.relname AS TABLE_NAME, t.oid AS TABLE_OID, ' +
    'current_database() AS INDEX_CATALOG, ' +
    'n.nspname AS INDEX_SCHEMA, c.relname AS INDEX_NAME, c.oid AS INDEX_OID, ' +
    'a.attname AS COLUMN_NAME, a.attnum AS COLUMN_POSITION, 0 AS DESCENDING ' +
  {$IFDEF LITE}
    ', indisunique::integer AS UNIQUE ' +
  {$ENDIF}
    'FROM pg_index i ' +
    '  INNER JOIN pg_class c ON c.oid = i.indexrelid ' +
    '  INNER JOIN pg_class t ON t.oid = i.indrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.relnamespace ' +
    '  INNER JOIN pg_attribute a ON a.attrelid = i.indrelid and a.attnum = any(i.indkey) ' +
    '%s ORDER BY indisprimary::integer DESC, n.nspname, t.relname, c.relname, a.attnum';

  fmtGetIndexColumnsSQL83 = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS TABLE_SCHEMA, t.relname AS TABLE_NAME, t.oid AS TABLE_OID, ' +
    'current_database() AS INDEX_CATALOG, ' +
    'n.nspname AS INDEX_SCHEMA, c.relname AS INDEX_NAME, c.oid AS INDEX_OID, ' +
    'a.attname AS COLUMN_NAME, a.attnum AS COLUMN_POSITION,' +
    'indoption[index] & 1 AS DESCENDING ' +
  {$IFDEF LITE}
    ', indisunique::integer AS UNIQUE ' +
  {$ENDIF}
    'FROM ' +
    '  (SELECT generate_series(0, indnatts - 1), indrelid, indexrelid, indkey,' +
    '   indoption, indisunique, indisprimary FROM pg_index i)' +
    '   i(index, indrelid, indexrelid, indkey, indoption, indisunique, indisprimary)' +
    '  INNER JOIN pg_class c ON c.oid = indexrelid ' +
    '  INNER JOIN pg_class t ON t.oid = indrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.relnamespace ' +
    '  INNER JOIN pg_attribute a ON a.attrelid = indrelid and a.attnum = indkey[index] ' +
    '%s ORDER BY indisprimary::integer DESC, n.nspname, t.relname, c.relname, a.attnum';
var
  WhereClause, TableName, TableSchema, TableOID, IndexName, IndexOID, Uniqueness, SQL: string;
  Command: TPgSQLCommand;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  IndexOID := Trim(Restrictions.Values['INDEX_OID']);
  Uniqueness := Trim(Restrictions.Values['UNIQUE']);

  WhereClause := '';
  AddWhere(WhereClause, 'n.nspname', TableSchema);
  AddWhere(WhereClause, 't.relname', TableName);
  AddWhere(WhereClause, 'i.indrelid', TableOID);
  AddWhere(WhereClause, 'c.relname', IndexName);
  AddWhere(WhereClause, 'i.indexrelid', IndexOID);

  if (Uniqueness = '0') or (Uniqueness = '1') then begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    if Uniqueness = '1' then
      WhereClause := WhereClause + 'i.indisunique'
    else
      WhereClause := WhereClause + 'not i.indisunique';
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  Assert(FRecordSet <> nil);
  Command := TPgSQLCommand(FRecordSet.GetCommand);
  Assert(Command.FConnection <> nil);
  if Command.FConnection.VersionIsEqualOrHigher(8, 3) then
    SQL := Format(fmtGetIndexColumnsSQL83, [WhereClause])
  else if Command.FConnection.VersionIsEqualOrHigher(8, 1) then
    SQL := Format(fmtGetIndexColumnsSQL81, [WhereClause])
  else
    SQL := Format(fmtGetIndexColumnsSQL7, [WhereClause]);

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetConstraints(Restrictions: TStrings): TData;
const
  fmtGetConstraintsSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, n.nspname AS TABLE_SCHEMA, c.relname AS TABLE_NAME, ' +
    'c.oid AS TABLE_OID, con.conname AS CONSTRAINT_NAME, ' +
    'CASE con.contype ' +
    '  WHEN ''c'' THEN ''CHECK'' ' +
    '  WHEN ''f'' THEN ''FOREIGN KEY'' ' +
    '  WHEN ''p'' THEN ''PRIMARY KEY'' ' +
    '  WHEN ''u'' THEN ''UNIQUE'' ' +
    'END::varchar(11) AS CONSTRAINT_TYPE ' +
    'FROM pg_constraint con ' +
    '  INNER JOIN pg_class c ON c.oid = con.conrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    '%s ORDER BY n.nspname, c.relname, con.conname ';
var
  WhereClause, Schema, TableName, TableOID, ConstraintName, Types, Scope: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := LowerCase(Trim(Restrictions.Values['CONSTRAINT_TYPE']));
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else if not GetConnection.FMultipleSchema then
    AddWhere(WhereClause, 'n.nspname', GetConnection.GetCachedSchema)
  else begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + '(n.nspname in (' + GetConnection.GetSchemaNamesForSQL(0) + '))';
  end;

  AddWhere(WhereClause, 'n.nspname', Schema);
  AddWhere(WhereClause, 'c.relname', TableName);
  AddWhere(WhereClause, 'con.conrelid', TableOID);
  AddWhere(WhereClause, 'con.conname', ConstraintName);

  if Types <> '' then begin
    if Types = 'check' then
      Types := 'c'
    else
    if Types = 'foreign key' then
      Types := 'f'
    else
    if Types = 'primary key' then
      Types := 'p'
    else
    if Types = 'unique' then
      Types := 'u';

    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'con.contype = ' + PgSQLInfo.ToStringConst(Types);
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetConstraintsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetConstraintColumns(Restrictions: TStrings): TData;
const
  fmtGetConstraintColumnsSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, n.nspname AS TABLE_SCHEMA, c.relname AS TABLE_NAME, ' +
    'c.oid AS TABLE_OID, ' +
    'con.conname AS CONSTRAINT_NAME, ' +
    'a.attname  AS COLUMN_NAME ' +
    'FROM pg_constraint con ' +
    'INNER JOIN pg_class c ON c.oid = con.conrelid ' +
    'INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    'INNER JOIN pg_attribute a ON (con.conrelid = a.attrelid ' +
    'AND (a.attnum = any(con.conkey))) ' +
    ' %s ORDER BY n.nspname, c.relname, con.conname ';
var
  WhereClause, Schema, TableName, TableOID, ConstraintName, ColumnName, Scope: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else if not GetConnection.FMultipleSchema then
    AddWhere(WhereClause, 'n.nspname', GetConnection.GetCachedSchema)
  else begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + '(n.nspname in (' + GetConnection.GetSchemaNamesForSQL(0) + '))';
  end;

  AddWhere(WhereClause, 'n.nspname', Schema);
  AddWhere(WhereClause, 'c.relname', TableName);
  AddWhere(WhereClause, 'con.conrelid', TableOID);
  AddWhere(WhereClause, 'con.conname', ConstraintName);
  AddWhere(WhereClause, 'a.attname', ColumnName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetConstraintColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetDataTypes(Restrictions: TStrings): TData;
const
  fmtGetDataTypesSQL = 'SELECT ' +
    'current_database() AS DATATYPE_CATALOG, n.nspname AS DATATYPE_SCHEMA, ' +
    't.typname AS DATATYPE_NAME, t.oid AS DATATYPE_OID, t.typlen AS DATATYPE_LENGTH, ' +
    'CASE ' +
    '  WHEN t.typtype = ''b'' THEN ''base'' ' +
    '  WHEN t.typtype = ''c'' THEN ''composite'' ' +
    '  WHEN t.typtype = ''d'' THEN ''domain'' ' +
    '  WHEN t.typtype = ''e'' THEN ''enum'' ' +
    '  WHEN t.typtype = ''p'' THEN ''pseudo'' ' +
    'END::varchar(9) AS DATATYPE_TYPE, t.typrelid AS TABLE_OID, t.typbasetype as DATATYPE_BASETYPE ' +
    'FROM pg_type t ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.typnamespace ' +
    '%s ORDER BY n.nspname, t.typname';
var
  WhereClause, TypeName, Schema, OID, TypeType, SQL: string;
begin
  Schema := Trim(Restrictions.Values['DATATYPE_SCHEMA']);
  TypeName := Trim(Restrictions.Values['DATATYPE_NAME']);
  OID := Trim(Restrictions.Values['DATATYPE_OID']);
  TypeType := LowerCase(Trim(Restrictions.Values['DATATYPE_TYPE']));

  WhereClause := '';
  AddWhere(WhereClause, 'n.nspname', Schema);
  AddWhere(WhereClause, 't.typname', TypeName);
  AddWhere(WhereClause, 't.oid', OID);

  if TypeType <> '' then begin
    if TypeType = 'base' then
      TypeType := 'b'
    else
    if TypeType = 'composite' then
      TypeType := 'c'
    else
    if TypeType = 'domain' then
      TypeType := 'd'
    else
    if TypeType = 'enum' then
      TypeType := 'e'
    else
    if TypeType = 'pseudo' then
      TypeType := 'p';

    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 't.typtype = ' + PgSQLInfo.ToStringConst(TypeType);
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;
  SQL := Format(fmtGetDataTypesSQL, [WhereClause]);

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetSequences(Restrictions: TStrings): TData;
const
  fmtGetSequencesSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS SEQUENCE_SCHEMA, c.relname AS SEQUENCE_NAME, c.oid AS SEQUENCE_OID ' +
    'FROM pg_class c ' +
    '  INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    'WHERE %s c.relkind = ''S'' ORDER BY n.nspname, c.relname';
var
  WhereClause, Schema, ObjectName, OID, Scope: string;
begin
  Schema := Trim(Restrictions.Values['SEQUENCE_SCHEMA']);
  ObjectName := Trim(Restrictions.Values['SEQUENCE_NAME']);
  OID := Trim(Restrictions.Values['SEQUENCE_OID']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else
    AddWhere(WhereClause, 'n.nspname', '"' + GetConnection.GetCachedSchema + '"');
  AddWhere(WhereClause, 'c.relname', ObjectName);
  AddWhere(WhereClause, 'c.oid', OID);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  FRecordSet.SetSQL(Format(fmtGetSequencesSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetSchemas(Restrictions: TStrings): TData;
const
  fmtGetSchemasSQL = 'SELECT ' +
    'current_database() AS SCHEMA_CATALOG, ' +
    'n.nspname AS SCHEMA_NAME, n.oid AS SCHEMA_OID ' +
    'FROM pg_namespace n ' +
    'ORDER BY n.nspname';
begin
  FRecordSet.SetSQL(fmtGetSchemasSQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetDatabases(Restrictions: TStrings): TData;
const
  fmtGetDatabasesSQL = 'SELECT ' +
    'datname AS DATABASE_NAME, pg_encoding_to_char(encoding) AS DATABASE_CHARSET ' +
    'FROM pg_database ' +
    'ORDER BY datname';
begin
  FRecordSet.SetSQL(fmtGetDatabasesSQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetUsers(Restrictions: TStrings): TData;
const
  fmtGetUsersSQL = 'SELECT ' +
    'usename AS USER_NAME ' +
    'FROM pg_user ' +
    'ORDER BY usename';
begin
  FRecordSet.SetSQL(fmtGetUsersSQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

{$IFNDEF LITE}

{ TPgSQLAlerter }

constructor TPgSQLAlerter.Create;
begin
  inherited;
end;

destructor TPgSQLAlerter.Destroy;
begin
  inherited;
end;

function TPgSQLAlerter.SetProp(Prop: integer; const Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited SetProp(Prop, Value);
  //end;
end;

function TPgSQLAlerter.GetProp(Prop: integer; var Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited GetProp(Prop, Value);
  //end;
end;

procedure TPgSQLAlerter.SendEvent(const EventName, Message: string);
begin
  if (FConnection <> nil) and (TPgSQLConnection(FConnection).VersionIsEqualOrHigher(9, 0)) then
    TPgSQLConnection(FConnection).ExecuteSQL('NOTIFY ' + EventName + ', ''' + Message + '''')
  else
    TPgSQLConnection(FConnection).ExecuteSQL('NOTIFY ' + EventName);
end;

procedure TPgSQLAlerter.Start;
begin
  if not FActive then begin
    TPgSQLConnection(FConnection).GetNotificationsHandler.RegisterEvents(
      FEventNames, DoOnEvent);
    FActive := True;
  end;
end;

procedure TPgSQLAlerter.Stop;
begin
  if FActive then begin
    TPgSQLConnection(FConnection).GetNotificationsHandler.UnregisterEvents(
      FEventNames, Self);
    FActive := False;
  end;
end;

procedure TPgSQLAlerter.DoOnEvent(const EventName: string; const PID: integer; const Message: string);
var
  InhOnEvent: TCRAlerterEventCallback;
begin
  InhOnEvent := inherited OnEvent;
  if Assigned(InhOnEvent) then
    InhOnEvent(EventName, Message);

  if Assigned(FOnEvent) then
    FOnEvent(EventName, PID, Message);
end;

{ TPgSQLNotificationsHandler }

{$IFDEF MSWINDOWS}

const
  WM_NOTIFICATION = WM_USER + 1;

var
  LockWnd: TCriticalSection;
  WndCount: integer;

function WndProc(hWindow: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; {$IFNDEF CLR} stdcall; {$ENDIF}
var
  Handler: TPgSQLNotificationsHandler;
  Notif: TPgSQLNotification;
begin
  Result := 0;
  case Msg of
    WM_NOTIFICATION: begin
      try
        Handler := TPgSQLNotificationsHandler(GetGCHandleTarget(IntPtr(wParam)));
        Notif := TPgSQLNotification(GetGCHandleTarget(IntPtr(lParam)));
        FreeGCHandle(IntPtr(lParam));
        try
          Handler.DoOnNotification(Notif.PID, Notif.Name, Notif.Message);
        finally
          Notif.Free;
        end;
      except
        if Assigned(ApplicationHandleException) then
          ApplicationHandleException(nil);
      end;
    end
    else
      Result := DefWindowProc(hWindow, Msg, wParam, lParam);
  end;
end;

var
  WndClass: TWndClass = (
    style: 0;
    lpfnWndProc: @WndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: '';
    lpszClassName: 'PGDACUtilWnd'
  );

procedure AllocWnd;
var
  TempClass: TWndClass;
  ClassRegistered: boolean;
  WndProcPtr: IntPtr;
begin
  LockWnd.Enter;
  try
    if hUtilWindow = 0 then begin
      WndClass.hInstance := HInstance;
      ClassRegistered := Windows.GetClassInfo(HInstance, WndClass.lpszClassName,
        TempClass);
      WndProcPtr := @WndProc;

      if not ClassRegistered or ({$IFDEF FPC}@{$ENDIF}TempClass.lpfnWndProc <> WndProcPtr) then begin
        if ClassRegistered then
          Windows.UnregisterClass(WndClass.lpszClassName, HInstance);

        hUtilWindow := Windows.RegisterClass(WndClass);
      end;

      hUtilWindow := Windows.CreateWindowEx(WS_EX_TOOLWINDOW, 'PGDACUtilWnd',
        '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
      Assert(hUtilWindow > 0);
    {$IFDEF CPU64}
      Windows.SetWindowLongPtr(hUtilWindow, GWL_WNDPROC, NativeInt(WndProcPtr));
    {$ELSE}
      Windows.SetWindowLong(hUtilWindow, GWL_WNDPROC, Integer(WndProcPtr));
    {$ENDIF}
    end;
    Inc(WndCount);
  finally
    LockWnd.Leave;
  end;
end;

procedure FreeWnd;
var
  OldWnd: HWND;
begin
  LockWnd.Enter;
  try
    if hUtilWindow > 0 then begin
      Dec(WndCount);
      if WndCount = 0 then begin
        OldWnd := hUtilWindow;
        hUtilWindow := 0;
        Windows.DestroyWindow(OldWnd);
      end;
    end;
  finally
    LockWnd.Leave;
  end;
end;

{$ENDIF}

constructor TPgSQLNotificationsHandler.Create(BaseConnection: TPgSQLConnection);
begin
  inherited Create;

  FBaseConnection := BaseConnection;
  FConnection := TPgSQLConnection.Create;
  FConnection.OnNotification := ProcessNotification;
  FLockConnection := TCriticalSection.Create;
  FAllowLockEvent := TEvent.Create(nil, True, True, '');
end;

destructor TPgSQLNotificationsHandler.Destroy;
begin
  FConnection.Free;
  FLockConnection.Free;
  FAllowLockEvent.Free;
{$IFDEF MSWINDOWS}
  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);
{$ENDIF}

  inherited;
end;

procedure TPgSQLNotificationsHandler.RegisterEvents(Events: TStrings;
  Callback: TPgSQLNotificationEvent);
var
  i, j, l: integer;
  NormName: string;
begin
  try
    for i := 0 to Events.Count - 1 do begin
      NormName := PgSQLInfo.NormalizeName(Events[i], False, True);
      j := EventIndex(NormName);
      if j < 0 then begin
        Listen(NormName);
        j := Length(FEvents);
        SetLength(FEvents, j + 1);
        FEvents[j].EventName := NormName;
      end;
      l := Length(FEvents[j].Callbacks);
      SetLength(FEvents[j].Callbacks, l + 1);
      FEvents[j].Callbacks[l] := Callback;
    end;
  finally
    if FConnectionIsLocked then begin
      FLockConnection.Leave;
      FConnectionIsLocked := False;
    end;
  end;
end;

procedure TPgSQLNotificationsHandler.UnregisterEvents(Events: TStrings;
  CallbackObject: TObject);
var
  i: integer;
{$IFNDEF CLR}
  j, k, l, l2, m: integer;
  NormName: string;
{$ENDIF}
  UnlistenNames: array of string;
begin
  SetLength(UnlistenNames, 0);
  try
  {$IFNDEF CLR}
    for i := 0 to Events.Count - 1 do begin
      NormName := PgSQLInfo.NormalizeName(Events[i], False, True);
      j := EventIndex(NormName);
      if j < 0 then
        continue;
      l := Length(FEvents[j].Callbacks);
      for k := 0 to l - 1 do begin
        if TMethod(FEvents[j].Callbacks[k]).Data = CallbackObject then begin
          if l > 1 then begin
            for m := k + 1 to l - 1 do
              FEvents[j].Callbacks[m - 1] := FEvents[j].Callbacks[m];
            SetLength(FEvents[j].Callbacks, l - 1);
          end
          else begin
            for m := j + 1 to High(FEvents) do
              FEvents[m - 1] := FEvents[m];
            SetLength(FEvents, Length(FEvents) - 1);

            l2 := Length(UnlistenNames);
            SetLength(UnlistenNames, l2 + 1);
            UnlistenNames[l2] := NormName;
          end;
          break;
        end;
      end;
    end;
  {$ELSE}
    SetLength(FEvents, 0);
  {$ENDIF}

    if Length(FEvents) = 0 then
      Stop
    else
      for i := 0 to High(UnlistenNames) do
        Unlisten(UnlistenNames[i]);
  finally
    if FConnectionIsLocked then begin
      FLockConnection.Leave;
      FConnectionIsLocked := False;
    end;
  end;
end;

function TPgSQLNotificationsHandler.EventIndex(const Name: string): integer;
var
  i: integer;
begin
  for i := 0 to High(FEvents) do begin
    if FEvents[i].EventName = Name then begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

procedure TPgSQLNotificationsHandler.Start;
begin
  FConnection.Assign(FBaseConnection);
  FConnection.Connect('');
  FLockConnection.Enter;
  FConnectionIsLocked := True;

{$IFDEF MSWINDOWS}
  AllocWnd;
{$ENDIF}
  FThread := TPgSQLNotificationsThread.Create(True);
  FThread.FHandler := Self;
  FThread.{$IFNDEF FPC}Resume{$ELSE}Start{$ENDIF};
end;

procedure TPgSQLNotificationsHandler.Stop;
begin
{$IFDEF MSWINDOWS}
  FreeWnd;
{$ENDIF}
  FThread.Terminate;
  if not FConnectionIsLocked then
    LockConnection;
  try
    FConnection.Disconnect;
  finally
    FLockConnection.Leave;
    FConnectionIsLocked := False;
  end;
  FThread.WaitFor;
  FThread.Free;
  FThread := nil;
end;

procedure TPgSQLNotificationsHandler.Listen(const Name: string);
begin
  if not FConnection.GetConnected then
    Start
  else begin
    if not FConnectionIsLocked then
      LockConnection;
  end;

  FConnection.ExecuteSQL('LISTEN "' + Name + '"');
end;

procedure TPgSQLNotificationsHandler.Unlisten(const Name: string);
begin
  if not FConnectionIsLocked then
    LockConnection;

  FConnection.ExecuteSQL('UNLISTEN "' + Name + '"');
end;

procedure TPgSQLNotificationsHandler.LockConnection;
begin
  FAllowLockEvent.ResetEvent;
  FConnection.FProtocol.TerminateMessageLoop;
  FLockConnection.Enter; // wait until ProcessMessageQueue exits
  FAllowLockEvent.SetEvent;
  FConnectionIsLocked := True;
end;

{$IFDEF MSWINDOWS}
function TPgSQLNotificationsHandler.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;
{$ELSE}
procedure TPgSQLNotificationsHandler.DoNotification;
begin
  if FLastNotification <> nil then
    DoOnNotification(FLastNotification.PID, FLastNotification.Name, FLastNotification.Message);
end;
{$ENDIF}

procedure TPgSQLNotificationsHandler.ProcessNotification(const Name: string; const PID: integer; const Message: string);
var
  Notif: TPgSQLNotification;
begin
{$IFDEF MSWINDOWS}
  if hUtilWindow <> 0 then begin
{$ENDIF}
    Notif := TPgSQLNotification.Create;
    Notif.PID := PID;
    Notif.Name := Name;
    Notif.Message := Message;
  {$IFDEF MSWINDOWS}
    PostMessage(hUtilWindow, WM_NOTIFICATION, WPARAM(GCHandle), LPARAM(AllocGCHandle(Notif)));
  {$ELSE}
    try
      FLastNotification := Notif;
      FThread.Synchronize(DoNotification);
    finally
      FLastNotification := nil;
      Notif.Free;
    end;
  {$ENDIF}
{$IFDEF MSWINDOWS}
  end;
{$ENDIF}
end;

procedure TPgSQLNotificationsHandler.DoOnNotification(NotificationPID: Integer; const NotificationName: string; const NotificationMsg: string);
var
  i: integer;
  index: integer;
begin
  index := EventIndex(NotificationName);
  if index >= 0 then begin
    for i := 0 to High(FEvents[index].Callbacks) do begin
      if Assigned(FEvents[index].Callbacks[i]) then
        FEvents[index].Callbacks[i](NotificationName, NotificationPID, NotificationMsg);
    end;
  end;

end;

{ TPgSQLNotificationsThread }

procedure TPgSQLNotificationsThread.Execute;
var
  Protocol: TPgSQLProtocol;
begin
  Protocol := FHandler.FConnection.GetProtocol;
  Assert(Protocol <> nil);

  while not Terminated do begin
    FHandler.FAllowLockEvent.WaitFor(INFINITE);
    if Terminated then
      exit;
    FHandler.FLockConnection.Enter;
    try
      Protocol.ProcessMessageQueue({$IFNDEF NEXTGEN}#0{$ELSE}0{$ENDIF}, False, True);
    except
      FHandler.FLockConnection.Leave;
      exit;
    end;
    FHandler.FLockConnection.Leave;
  end;
end;

{ TPgSQLLoader }

constructor TPgSQLLoader.Create;
begin
  inherited;

  FCommand := TPgSQLCommand.Create;
  FBufferSize := WriteBufferSize;
end;

destructor TPgSQLLoader.Destroy;
begin
  FCommand.Free;

  inherited;
end;

function TPgSQLLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prBufferSize:
      FBufferSize := Value;
    prTextMode:
      FTextMode := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TPgSQLLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prBufferSize:
      Value := FBufferSize;
    prTextMode:
      Value := FTextMode;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

class function TPgSQLLoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TPgSQLLoaderColumn;
end;

procedure TPgSQLLoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  inherited;

  with TPgSQLLoaderColumn(Column) do begin
    FSubDataType := FieldDesc.SubDataType;
    FRowType := TPgRowType(FieldDesc.ObjectType);
    if FRowType <> nil then
      FRowTypeName := FRowType.Name;
  end;
end;

procedure TPgSQLLoader.Reset;
begin
  SetLength(FRowValues, 0); // to clear
  FPrepared := False;
  FBlockOpened := False;

  inherited;
end;

procedure TPgSQLLoader.Prepare;
var
  SQL: StringBuilder;
  i: integer;
  Col: TPgSQLLoaderColumn;
begin
  inherited;

  FIsTextMode := FTextMode or (FConnection.GetProtocolVersion = pv20);

  SetLength(FRowValues, Columns.Count);

  for i := 0 to Columns.Count - 1 do begin
    Col := TPgSQLLoaderColumn(Columns[i]);
    if not FIsTextMode and (Col.DataType = dtObject) and (Col.FRowType = nil) then
      Col.FRowType := TPgRowType(FConnection.GetTypes.GetRowType(Col.FRowTypeName));
  end;

  SQL := StringBuilder.Create(1024);
  try
    SQL.Append('COPY ');
    if not FIsTextMode then
      SQL.Append('BINARY ');

    SQL.Append(PgSQLInfo.NormalizeName(TableName) + '(');
    for i := 0 to Columns.Count - 1 do begin
      if i > 0 then
        SQL.Append(', ');
      SQL.Append(PgSQLInfo.NormalizeName(Columns[i].Name, QuoteNames));
    end;
    SQL.Append(') FROM STDIN');

    FCommand.SetSQL(SQL.ToString);
  finally
    SQL.Free;
  end;

  FCommand.SetConnection(FConnection);
  FCommand.Prepare;
  FCommand.Execute;

  if not FIsTextMode then begin
    BeginDataBlock;
    WriteHeader;
  end;

  FPrepared := True;
end;

function TPgSQLLoader.IsPutColumnDataAllowed: boolean;
begin
  Result := High(FRowValues) >= 0;
end;

procedure TPgSQLLoader.PutColumnData(Col, Row: integer; const Value: variant);
begin
  if (FLastRow <> -1) and (Row > FLastRow + 1) then
    LoadRow;

  inherited;

  FRowValues[Col] := Value;
  DoAddObjectRef(Value);
end;

procedure TPgSQLLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then
    LoadRow;
end;

procedure TPgSQLLoader.LoadRow;
var
  i: integer;
  s: string;
  Col: TPgSQLLoaderColumn;
  Value: variant;
  UseUnicode: boolean;
  Buffer: TBytes;
begin
{$IFNDEF VER8P}
  SetLength(Buffer, 0);
{$ENDIF}
  FConnection.GetProp(prUseUnicode, Value);
  UseUnicode := Value;

  BeginDataBlock;

  if not FIsTextMode then
    FNet.WriteInt16(Columns.Count);

  for i := 0 to Columns.Count - 1 do begin
    Col := TPgSQLLoaderColumn(Columns[i]);
    Value := null;
    Value := FRowValues[i];
    if FIsTextMode then begin
      if VarIsNull(Value) or VarIsEmpty(Value) then
        s := '\N'
      else
        s := TPgTextConverter.ValueToText(Value, Col.DataType, Col.SubDataType, UseUnicode,
                                          False, True, FConnection.VersionIsEqualOrHigher(8, 2), False);
    {$IFDEF IS_UNICODE}
      if UseUnicode then
        Buffer := Encoding.UTF8.GetBytes(s)
      else
    {$ENDIF}
        Buffer := Encoding.Default.GetBytes(s);
      FNet.WriteBytes(Buffer);
      if i < Columns.Count - 1 then
        FNet.WriteByte(9);
    end
    else
      TPgBinaryConverter.WriteValue(Value, FNet, Col.DataType, Col.FSubDataType, Col.FRowType, FConnection, True);
  end;

  if FIsTextMode then
    FNet.WriteByte(10);

  if FNet.GetWriteBufferSize >= FBufferSize then
    EndDataBlock;

  Inc(FLoadedRows);

  DoAfterLoadRow;
end;

procedure TPgSQLLoader.BeginDataBlock;
begin
  if not FBlockOpened then begin
    FConnection.GetProtocol.BeginCopyDataBlock(FNet);
    FBlockOpened := True;
  end;
end;

procedure TPgSQLLoader.EndDataBlock;
begin
  if FBlockOpened then begin
    FConnection.GetProtocol.EndCopyDataBlock;
    FBlockOpened := False;
  end;
end;

procedure TPgSQLLoader.DoAfterLoadRow;
var
  i: integer;
begin
  if FObjectReleaseNeeded then
    for i := Low(FRowValues) to High(FRowValues) do
      DoReleaseObjectRef(FRowValues[i]);

  if FUseBlankValues then
    for i := Low(FRowValues) to High(FRowValues) do
      FRowValues[i] := Unassigned;
end;

procedure TPgSQLLoader.WriteHeader;
var
  PgCopy: TBytes;
begin
  SetLength(PgCopy, 11);
  PgCopy[0] := Ord('P');
  PgCopy[1] := Ord('G');
  PgCopy[2] := Ord('C');
  PgCopy[3] := Ord('O');
  PgCopy[4] := Ord('P');
  PgCopy[5] := Ord('Y');
  PgCopy[6] := 10;
  PgCopy[7] := 255;
  PgCopy[8] := 13;
  PgCopy[9] := 10;
  PgCopy[10] := 0;

  FNet.WriteBytes(PgCopy);
  FNet.WriteInt32(0); // flags
  FNet.WriteInt32(0); // extension
end;

procedure TPgSQLLoader.Finish;
begin
  if FPrepared then begin
    BeginDataBlock;
    if FIsTextMode then begin
      FNet.WriteByte(ord('\'));
      FNet.WriteByte(ord('.'));
      FNet.WriteByte(10);
    end
    else
      FNet.WriteInt16(-1); // trailer

    EndDataBlock;
    FConnection.GetProtocol.PutCopyEnd;

    FCommand.UnPrepare;
  end;

  inherited;
end;

procedure TPgSQLLoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TPgSQLConnection(Value);
end;

{$ENDIF}

initialization
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  WndCount := 0;
  LockWnd := TCriticalSection.Create;
{$ENDIF}
{$ENDIF}

  PgSQLInfo := TPgSQLInfo.Create(nil);

finalization
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  LockWnd.Free;
{$ENDIF}
{$ENDIF}
  PgSQLInfo.Free;

end.
