
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Core Access
//  Created:            01.07.00
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRAccess;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, {$IFNDEF LITE}MTSCall,{$ENDIF}
{$ENDIF}
{$IFDEF UNIX}
  unix,
{$ENDIF}
  Classes, SysUtils, Types, SyncObjs,
  Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFNDEF LITE}
  CRDataTypeMap, CREncryption,
{$ENDIF}
  CLRClasses, CRTypes, CRTimeStamp, CRProps, CRParser, CRVio, MemData, MemUtils;

{$HPPEMIT 'class TCRFieldDesc;'}
{$HPPEMIT 'class TParamDesc;'}

type
  TCRConnection   = class;
  TCRTransaction  = class;
  TCRCommand      = class;
  TCRRecordSet    = class;
  TCRFieldDesc    = class;
  TParamDesc      = class;
  TParamDescs     = class;
  TDAParamsInfo   = class;
  TCRTablesInfo   = class;
  TSQLInfo        = class;
  TCRCursor       = class;
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  TMTSTransaction = class;
{$ENDIF}
{$ENDIF}
  TCRMetaData     = class;
  TCRLoaderColumn = class;
  TCRLoader       = class;
  TCRAlerter      = class;
  TSQLGenerator   = class;

  TCRConnectionClass = class of TCRConnection;
  TCRTransactionClass = class of TCRTransaction;
  TCRCommandClass = class of TCRCommand;
  TCRRecordSetClass = class of TCRRecordSet;
  TParamDescClass = class of TParamDesc;
  TTableInfoClass = class of TCRTableInfo;
  TSQLInfoClass = class of TSQLInfo;
  TCRMetaDataClass = class of TCRMetaData;
  TCRLoaderColumnClass = class of TCRLoaderColumn;
  TCRLoaderClass = class of TCRLoader;
  TCRAlerterClass = class of TCRAlerter;

  TCommandType = (ctUnknown, ctStatement, ctCursor);
  TParsedSQLType = (qtUnparsed, qtUnknown, qtSelect, qtCursor,
    qtInsert, qtUpdate, qtDelete, qtInsertReturning, qtUpdateReturning,
    qtExecuteBlock, qtCreate, qtDrop, qtSelectProc, qtSelectInto);

  TCursorState = (
    csInactive, // default state (TCRRecordSet.InternalOpen, TCustomDASQL.SQLChanged)
    csOpen, // ODAC only: OCI73 TOCICommand.InternalOpen
    csParsed, // ODAC only: OCI73 - statement parsed
    csPrepared, // statement prepared
    csBound, // ODAC only: parameters bound
    csExecuteFetchAll, // ODAC only:
    csExecuting, // ODAC only(?): statement is executing (TCRCommand.Execute)
    csExecuted, // statement successfully executed
    csFetching, // setted on first TCRRecordSet.Fetch
    csFetchingAll, // ODAC, IbDAC specific. Setted on the FetchAll start
    csFetched // fetch finished or canceled
  );

  TCRIsolationLevel = (ilReadCommitted, ilReadUnCommitted, ilRepeatableRead, ilIsolated, ilSnapshot, ilCustom);

  TErrorProc = procedure (E: Exception; var Fail, Reconnect: boolean; {$IFNDEF LITE}var Reexecute: boolean; {$ENDIF}
    ReconnectAttempt: integer{$IFNDEF LITE}; var ConnLostCause: TConnLostCause{$ENDIF}) of object;
  TReconnectProc = procedure of object;
  TConnectProc = procedure of object;
  TBoolProc = procedure (Value: boolean) of object;
  TBeforeFetchProc = procedure (var Cancel: boolean) of object;
  TAfterFetchProc = procedure of object;
  TDataChangeProc = procedure of object;
  TReadParamsProc = procedure of object;
  TGetSQLProc = function: string of object;
  TGetDBKeyListProc = function (const TableName, IndexName: string): string of object;
  TFillExtFieldsInfoProc = procedure of object;
  TGetPooledConnection = function: TCRConnection of object;
  TInfoMessageProc = procedure (E: Exception) of object;

  EFailOver = class(Exception)
  public
    FConnLostCause: TConnLostCause;

    constructor Create(ConnLostCause: TConnLostCause);
  end;

{$IFNDEF LITE}
  // Smart fetch
  TSmartFetchState = (sfNone, sfMetaInfo, sfKeyOnly, sfDataByKey);
  TFetchedStatus = (fsNotFetched, fsFetched, fsFree);
  TItemRefCount = packed record
    Used: boolean;
    Count: byte;
  end;
  PItemRefCount = ^TItemRefCount;
  TItemRefCountArr = array of TItemRefCount;

  ESmartFetchError = class(Exception);
{$ENDIF}

  TFieldDescArray = array of TCRFieldDesc;
  TKeyAndDataFields = record
    KeyFieldDescs: TFieldDescArray;
    DataFieldDescs: TFieldDescArray;
  end;

{ TSmartFetchInfo }

  TSmartFetchInfo = class
  private
    FSQLGenerator: TSQLGenerator;
  protected
    FGeneratedSmartFetchByKeySQL: boolean;
    FParamsInfo: TDAParamsInfo;
  public
    KeyFieldDescs: TFieldDescArray;
    SqlBeforeWhere: string;
    SqlAfterWhere: string;

    constructor Create(SQLGenerator: TSQLGenerator);
    destructor Destroy; override;

    property ParamsInfo: TDAParamsInfo read FParamsInfo;
    property SQLGenerator: TSQLGenerator read FSQLGenerator;
  end;

{ TSQLGenerator }

  TSQLGenerator = class
  public
    function CloneGenerator: TSQLGenerator; virtual; abstract;

    function GenerateTableSQL(const TableName, OrderFields: string): string; virtual; abstract;
    function GenerateRecCountSQL(UseBaseSQL: boolean = False): string; virtual; abstract;
    function GenerateSelectValues(const ValuesList: string): string; virtual; abstract;
    function GenerateEmptyTableSQL(const TableName: String): string; virtual; abstract;

    function GenerateSmartFetchMetaInfoSQL: string; virtual; abstract;
    function GenerateSmartFetchKeyOnlySQL(const PrefetchedFields: TFieldDescArray): string; virtual; abstract;
    function GenerateSmartFetchDataByKeySQL(SmartFetchInfo: TSmartFetchInfo; ParamsInfo: TDAParamsInfo; FirstItem: PItemHeader; RowCount: integer): string; virtual; abstract;
    procedure PrepareSmartFetchDataByKeySQL(const Sql: string; SmartFetchInfo: TSmartFetchInfo); virtual; abstract;
  end;

{ TCRConnector }

  TCRConnector = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCRConnection;


    FServerName: string;
    FServerVersion: string;
    FServerMajorVersion: integer;
    FServerMinorVersion: integer;

  public
    constructor Create(Owner: TCRConnection); virtual;

    class function CloneException(E: Exception): Exception; virtual;

    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure SetDatabase(const Value: string); virtual; abstract;


  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    procedure Enlist(Transaction: TMTSTransaction); virtual;
    procedure UnEnlist(Transaction: TMTSTransaction); virtual;
  {$ENDIF}
  {$ENDIF}

    function GetClientVersionFull: string; virtual;
    function GetClientVersion: string; virtual; abstract;
    function GetClientMajorVersion: integer; virtual; abstract;

    property Connection: TCRConnection read FOwner;
    property ServerName: string read FServerName;
    property ServerVersion: string read FServerVersion;
    property ServerMajorVersion: integer read FServerMajorVersion;
    property ServerMinorVersion: integer read FServerMinorVersion;
  end;

{ TCRConnection }

  TCRConnection = class
  private
    FOnError: TErrorProc;
    FOnInfoMessage: TInfoMessageProc;
    FOnReconnectError: TReconnectProc;
    FOnReconnectSuccess: TReconnectProc;
    FConnectionTime: Cardinal;
    FSQLinfo: TSQLInfo;
  {$IFNDEF LITE}
    FDataTypeMap: TCRMapRules;
  {$ENDIF}

    FGetPooledConnection: TGetPooledConnection; // Get connection from Pooling
    FAdditional: boolean; // This is additional connection for FetchAl = False mode

    FCommand: TCRCommand;
    FRecordSet: TCRRecordSet;
    FCommandLock: TCriticalSection;

    function GetSQLInfo: TSQLInfo;
  protected
    FInternalTransaction: TCRTransaction;
    FConnected: boolean;
    FNativeConnection: boolean;
    FUsername: string;
    FPassword: string;
    FServer: string;
    FDatabase: string;
    FAutoCommit: boolean;
    FConvertEOL: boolean;
    FIsValid: boolean;
    FPool: TObject;
    FPoolVersion: integer;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FComponent: TObject;
    FDisconnectedMode: boolean;
    FIsolationLevel: TCRIsolationLevel;
    FEnableBCD: boolean;
    FEnableFMTBCD: boolean;
    FUuidWithBraces: boolean;

    FInProcessError: boolean;
    FReconnected: boolean;
    FDefaultSortType: TSortType;

  {$IFNDEF LITE}
    FIOHandler: TCRIOHandler;
    FHttpOptions: THttpOptions;
    FProxyOptions: TProxyOptions;
  {$ENDIF}
    FSSLOptions: TSSLOptions;

    function CreateSQLInfo: TSQLInfo; virtual;
  {$IFNDEF LITE}
    function CreateDataTypeMap: TCRMapRules; virtual;
  {$IFDEF MSWINDOWS}
    procedure Enlist(MTSTransaction: TMTSTransaction); virtual;
    procedure UnEnlist(MTSTransaction: TMTSTransaction); virtual;
  {$ENDIF}
  {$ENDIF}

    function GetInternalCommandClass: TCRCommandClass; virtual;
    function CheckCommand(Command: TCRCommand): boolean; virtual;
    function CreateCommand: TCRCommand;
    procedure InitCommandProp(Command: TCRCommand); virtual;
    function CheckRecordSet(RecordSet: TCRRecordSet): boolean; virtual;
    function CreateRecordSet: TCRRecordSet;
    procedure InitRecordSetProp(RecordSet: TCRRecordSet); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; virtual; abstract;
    function GetRecordSetClass: TCRRecordSetClass; virtual; abstract;
    function GetTransactionClass: TCRTransactionClass; virtual; abstract;
  {$IFNDEF LITE}
    function GetLoaderClass: TCRLoaderClass; virtual; abstract;
    function GetMetaDataClass: TCRMetaDataClass; virtual; abstract;
    class function GetMapRulesClass: TCRMapRulesClass; virtual;

    function CreateObject(DataType: Word; Transaction: TCRTransaction): TDBObject; virtual;
  {$ENDIF}

    procedure DoError(E: Exception; var Fail: boolean); virtual;

    procedure Connect(const ConnectString: string); virtual;
    procedure Disconnect; virtual; abstract;
    procedure Ping; virtual; abstract;
    procedure Assign(Source: TCRConnection); virtual;
    procedure AssignConnect(Source: TCRConnection); virtual; abstract;

    function GetCommand: TCRCommand;
    procedure ReleaseCommand(var Command: TCRCommand);
    procedure ExecuteSQL(const SQL: string);
    function GetRecordSet: TCRRecordSet;
    procedure ReleaseRecordSet(var RecordSet: TCRRecordSet);
    function OpenRecordSet(const SQL: string): TCRRecordSet;

    function GetConnected: boolean;
    procedure SetConnected(Value: boolean);

    function GetInternalTransaction: TCRTransaction; virtual;
    procedure SetIsolationLevel(const Value: TCRIsolationLevel); virtual;

    procedure SetUsername(const Value: string); virtual;
    procedure SetPassword(const Value: string); virtual;
    procedure SetServer(const Value: string); virtual;
    function GetUsername: string;
    function GetPassword: string;
    function GetServer: string;

    function CheckIsValid: boolean; virtual; abstract;
  {$IFNDEF NODBACCESS}
    procedure ReturnToPool; virtual;
  {$ENDIF}

    function GetServerVersion: string; virtual; abstract;
    function GetServerVersionFull: string; virtual; abstract;
    function GetClientVersion: string; virtual; abstract;
    function GetConnector: TCRConnector; virtual;

    function CanChangeDatabase: boolean; virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; out Value: variant): boolean; virtual;

    property OnError: TErrorProc read FOnError write FOnError;
    property OnInfoMessage: TInfoMessageProc read FOnInfoMessage write FOnInfoMessage;
    property OnReconnectError: TReconnectProc read FOnReconnectError write FOnReconnectError;
    property OnReconnectSuccess: TReconnectProc read FOnReconnectSuccess write FOnReconnectSuccess;

    property AutoCommit: boolean read FAutoCommit write FAutoCommit;
    property EnableBCD: boolean read FEnableBCD write FEnableBCD;
    property EnableFMTBCD: boolean read FEnableFMTBCD write FEnableFMTBCD;
    property Database: string read FDatabase write FDatabase;
    property ConnectionTime: Cardinal read FConnectionTime;
    property IsValid: boolean read FIsValid write FIsValid;
    property Pool: TObject read FPool write FPool;
    property PoolVersion: integer read FPoolVersion write FPoolVersion;
    property Component: TObject read FComponent write FComponent; // Is needed for failover
    property DisconnectedMode: boolean read FDisconnectedMode write FDisconnectedMode;
    property NativeConnection: boolean read FNativeConnection;
    property UuidWithBraces: boolean read FUuidWithBraces write FUuidWithBraces;
  {$IFNDEF LITE}
    property IOHandler: TCRIOHandler read FIOHandler write FIOHandler;
    property DataTypeMap: TCRMapRules read FDataTypeMap;
  {$ENDIF}
    property SQLInfo: TSQLInfo read GetSQLInfo;

    property Additional: boolean read FAdditional write FAdditional; // This is additional connection for FetchAl = False mode
    property GetPooledConnection: TGetPooledConnection read FGetPooledConnection write FGetPooledConnection;
  end;

{ TCRCommand }

  TCRCommand = class
  private
    FSQLInfo: TSQLInfo;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FBatchOwner: TCRCommand;
    FBatchCommand: TCRCommand;

    function GetSQLInfo: TSQLInfo;
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FComponent: TObject;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TCRConnection;
    FCommandType: TCommandType;
    FSQL: string;
    FUserSQL: string;
    FParsedSQLType: TParsedSQLType;
    FParams: TParamDescs;
    FParamsInfo: TDAParamsInfo;
    FAutoCommit: boolean;
    FAfterExecute: TBoolProc;
    FScanParams: boolean;
    FDisableParamScan: boolean;
    FQuoteNames: boolean;
    FTrimFixedChar: boolean;
  {$IFDEF HAVE_COMPRESS}
    FCompressBlob: TCompressBlobMode;
  {$ENDIF}
    FEnableBCD: boolean;
    FEnableFMTBCD: boolean;
    FReadParams: TReadParamsProc;
    FBatchIters: integer;
    FBatchOffset: integer;
    FLastBatchIters: integer;
    FParamsProcessed: integer;
    FExecuting: boolean;

    function CreateSQLInfo: TSQLInfo; virtual;
    procedure ParseSQLParam(ParsedSQL: StringBuilder; Parser: TSQLParser; Params: TParamDescs; LeftQuote, RightQuote: Char; const RenamePrefix: string; PrevCode: integer = -1);
    procedure CheckSQLParamType(ParsedSQL: StringBuilder; Parser: TSQLParser; Param: TParamDesc); virtual;
    procedure AddParamPosition(const ParamName: string; StartPosition, EndPosition: integer; ParamRef: TParamDesc);

    function IsLabelSyntax(Code: integer; PrevCode: integer): boolean; virtual;

    procedure CreateBatchCommand; virtual;
    function GetBatchCommand: TCRCommand;
    function GetBatchSQL(Iters, Offset: integer): string; virtual;
    function GetBatchIters(Iters: integer): integer; virtual;
    function NeedBatchTransaction: boolean; virtual;
    function NeedBatchSavepoint: boolean; virtual;
    procedure InternalExecuteBatch(Iters, Offset: integer); virtual;

    property BatchOwner: TCRCommand read FBatchOwner;

    property EnableBCD: boolean read FEnableBCD write FEnableBCD;
    property EnableFMTBCD: boolean read FEnableFMTBCD write FEnableFMTBCD;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    class function GetTableInfoClass: TTableInfoClass; virtual;
    class function GetSQLInfoClass: TSQLInfoClass; virtual;
    class function GetParserClass: TSQLParserClass; virtual;
    class function GetParamDescClass: TParamDescClass; virtual;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; virtual;
  {$ENDIF}

    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function GetPrepared: boolean; virtual; abstract;

    procedure Execute; virtual; {$IFNDEF AUTOTEST}abstract;{$ENDIF}
    procedure ExecuteBatch(Iters, Offset: integer); virtual;
    procedure Close; virtual; //TODO abstract;

    procedure SetConnection(Value: TCRConnection); virtual;
    function GetConnection: TCRConnection;
    procedure SetTransaction(Value: TCRTransaction); virtual;
    function GetTransaction: TCRTransaction; virtual;
    function GetCursorState: TCursorState; virtual; abstract;
    procedure SetCursorState(Value: TCursorState); virtual; abstract;

    procedure SetSQL(const Value: string); virtual;
    function ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string; overload; virtual;
    procedure ParseSQL; overload;
    procedure ParseSQLType; overload; virtual;
    procedure ParseSQLType(const SQL: string); overload; virtual;
    function IsValidBatchSQL: boolean; virtual;
    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; virtual; abstract;
    function ForceCreateSPParams: boolean; virtual;

  { Params }
    class function IsAllowedArrayType(DataType: Word): boolean; virtual;
    procedure ClearParams;
    function AddParam: TParamDesc; virtual;

    function GetCursor: TCRCursor; virtual;
    procedure SetCursor(Value: TCRCursor); virtual;

    function SetProp(Prop: integer; const Value: Variant): boolean; virtual;
    function GetProp(Prop: integer; out Value: Variant): boolean; virtual;

    procedure BreakExec; virtual;

    property Executing: boolean read FExecuting write FExecuting;
    property CommandType: TCommandType read FCommandType write FCommandType;
    property SQL: string read FSQL write SetSQL;
    property Component: TObject read FComponent write FComponent; // Is needed for failover
    property AfterExecute: TBoolProc read FAfterExecute write FAfterExecute;
    property ReadParams: TReadParamsProc read FReadParams write FReadParams; // for SDAC
    property Params: TParamDescs read FParams;
    property ParamsInfo: TDAParamsInfo read FParamsInfo write FParamsInfo;
    property SQLInfo: TSQLInfo read GetSQLInfo;
    property ParsedSQLType: TParsedSQLType read FParsedSQLType;
  end;

{ $IFNDEF LITE}

{ TCRTableInfo }

  TCRTableInfo = class(TObject)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCRTablesInfo;
    FIndex: Integer;
    FTableName: string;
    FTableAlias: string;
    FIsView: boolean;
    procedure SetTableName(const Value: string);
    procedure SetTableAlias(const Value: string);
    function GetTableNameFull: string; virtual;
    procedure SetTableNameFull(const Value: string); virtual;
  public
    constructor Create(Owner: TCRTablesInfo); virtual;

    property TableName: string read FTableName write SetTableName;
    property TableAlias: string read FTableAlias write SetTableAlias;
    property TableNameFull: string read GetTableNameFull write SetTableNameFull;
    property IsView: boolean read FIsView write FIsView;
    property Index: Integer read FIndex write FIndex;
  end;

  TCRTablesInfo = class
  private
    function GetItem(Index: Integer): TCRTableInfo;
    procedure SetItem(Index: Integer; const Value: TCRTableInfo);
  protected
    FCaseSensitive: boolean;
    FList: array of TCRTableInfo;
    FUpdateCount: Integer;
    FTableInfoClass: TTableInfoClass;
    FTableNameList: TStringList;
    FTableAliasList: TStringList;
    procedure InternalAdd(TableInfo: TCRTableInfo);
    procedure Changed;
    procedure TableNameChanged;
    procedure TableAliasChanged;
    function GetCount: Integer;
    procedure SetCaseSensitive(Value: boolean);
  public
    constructor Create(TableInfoClass: TTableInfoClass);
    destructor Destroy; override;

    function Add: TCRTableInfo;
    procedure Clear;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function FindByName(const TableName: string): TCRTableInfo;
    function IndexOf(TableInfo: TCRTableInfo): Integer;
    function IndexByName(const TableName: string): Integer;
    function IndexByAlias(const TableAlias: string): Integer;
    function FindByNameAndAlias(const TableName, TableAlias: string): TCRTableInfo;
    property Items[Index: Integer]: TCRTableInfo read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property TableInfoClass: TTableInfoClass read FTableInfoClass;
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive;
  end;

  TCRColumnInfo = class
  public
    Name: string;
    Table: string;
    TableIndex: Integer;
    Expr: string; // expression
    Alias: string;
    Used: boolean; // is ColumnInfo used by another Field
    Described: boolean;
    Required: boolean;
  end;

  TCRColumnsInfo = class(TCRObjectList)
  private
    function GetItem(Index: integer): TCRColumnInfo;
  public
    property Items[Index: Integer]: TCRColumnInfo read GetItem; default;
  end;

{ $ENDIF}

  TIdentCase = (icUpper, icLower, icMixed, icMixedCaseSensitive);

  TSQLObjectInfo = record
    Name: string;
    Schema: string;
    Catalog: string;
    DBLink: string;
    Synonym: string;
    Flag: byte;
  end;

  TSQLObjectsInfo = array of TSQLObjectInfo;

  TRequestFieldsInfoProc = procedure (Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo) of object;

{ TSQLInfo }

  TSQLInfo = class
  private
    FParserClass: TSQLParserClass;
  protected
    procedure ParseExtColumnName(Parser: TSQLParser; var Code: integer; var Str: string); virtual;
    function FirstCharQuotesNeed(Ch: Char; IdCase: TIdentCase): boolean; virtual;
    function NextCharQuotesNeed(Ch: Char; IdCase: TIdentCase): boolean; virtual;
    function HasAsLexem: boolean; virtual;
    function HasOnlyLexem: boolean; virtual;
    procedure ParseExtTableInfo(Parser: TSQLParser; var CodeLexem: integer; var StLex: string; var Name: string); virtual;
  public
    constructor Create(ParserClass: TSQLParserClass); virtual;

    function LeftQuote: Char; virtual;
    function RightQuote: Char; virtual;
    function IdentCase: TIdentCase; virtual;
    function ParamQuoteAllowed: boolean; virtual;
    function ProcedureOverloadSeparator: Char; virtual;
    function IsCursorSQLType(Code: Integer): boolean; virtual;
    function DetectReturningSQLType: boolean; virtual;

    function Quote(const Value: string): string; overload;
    function Quote(const Value: string; const LeftQ: Char; const RightQ: Char): string; overload; virtual;
    function UnQuote(const Value: string): string; virtual;
    function IsQuoted(const Value: string): boolean; virtual;
    function QuotesNeeded(const Value: string): boolean; virtual;
    function QuoteIfNeed(const Value: string): string;
    function NormalizeName(const Value: string; QuoteNames: boolean = False; UnQuoteNames: boolean = False): string; overload; virtual;
    function NormalizeName(const Value: string; const LeftQ: Char; const RightQ: Char; QuoteNames: boolean = False; UnQuoteNames: boolean = False): string; overload; virtual;
    function ToStringConst(const Value: string): string;

    procedure SplitObjectName(const Name: string; out Info: TSQLObjectInfo); virtual;
    procedure ParseTablesInfo(const SQL: string; TablesInfo: TCRTablesInfo); virtual;
    procedure ParseColumnsInfo(const SQL: string; ColumnsInfo: TCRColumnsInfo); virtual;

    function NamesFromList(List: TStrings; NormalizedName: boolean = True; Delimiter: string = ';'): string; virtual;
    procedure NamesToList(Value: string; List: TStrings; NormalizedName: boolean = True); virtual;
  end;

{ TCRFieldDesc}

  TCRFieldDesc = class (TFieldDesc)
  private
    // Data Type Mapping
    FDBType: Word;
    FDBLength: Integer;
    FDBScale: Integer;
  {$IFNDEF LITE}
    FIsImplicityPrefetched: boolean;
    FIsPrefetched: boolean;

    FMapRule: TCRMapRule;
    FMapLength: Integer;
    FMapScale: Integer;
    FMapHasValueLen: Boolean;
    FOnDemandConverter: TOnDemandConverter;
    FEncryptor: TEncryptionMethod;
    FDecryptor: TDecryptionMethod;
    FEncryptState: Integer;
  {$ENDIF}

  protected
    FIsNational: boolean;
    FTableInfo: TCRTableInfo;
    FActualNameQuoted: array[boolean] of string; // cache for [QuoteNames]
    FDefaultExpr: string;

  {$IFNDEF LITE}
    function GetMapLength: Integer; override;
    function GetMapDataType: Word; override;
  {$ENDIF}
    procedure SetFieldReadOnly(SetFieldsReadOnly: boolean; UpdatingTableInfo: TCRTableInfo); virtual;
  public
    constructor Create(RecordSetClass: TRecordSetClass); override;

    procedure Assign(FieldDesc: TFieldDesc); override;
    function ActualNameQuoted(SQLInfo: TSQLInfo; QuoteNames: boolean): string; virtual;

    property IsNational: boolean read FIsNational;

    property TableInfo: TCRTableInfo read FTableInfo write FTableInfo;
    property DefaultExpr: string read FDefaultExpr write FDefaultExpr;

    property DBType: Word read FDBType write FDBType;
    property DBLength: Integer read FDBLength write FDBLength;
    property DBScale: Integer read FDBScale write FDBScale;
  {$IFNDEF LITE}
    property IsPrefetched: boolean read FIsPrefetched write FIsPrefetched;

    property MapRule: TCRMapRule read FMapRule write FMapRule;
    property MapLength: Integer read GetMapLength write FMapLength;
    property MapScale: Integer read FMapScale write FMapScale;
    property MapHasValueLen: Boolean read FMapHasValueLen write FMapHasValueLen;
    property OnDemandConverter: TOnDemandConverter read FOnDemandConverter write FOnDemandConverter;

    property Encryptor: TEncryptionMethod read FEncryptor write FEncryptor;
    property Decryptor: TDecryptionMethod read FDecryptor write FDecryptor;
    property EncryptState: Integer read FEncryptState write FEncryptState;
  {$ENDIF}
  end;

{ TCRRecordSet }
  TFieldOrigins = (foNone, foField, foTableAndField, foTableAliasAndField);

  TCRRecordSet = class (TMemData)
  protected
    FCommand: TCRCommand;
    FSQLGenerator: TSQLGenerator;
    FTmpFields: TFieldDescs;
    FUniDirectional: boolean;
    FFetchRows: integer;
    FFetchAll: boolean;
    FLockFetchAll: boolean;
    FOnConnectRequest: TConnectProc;
    FOnDisconnectRequest: TConnectProc;

    FNoCountData: boolean;
    FLongStrings: boolean;
    FFlatBuffers: boolean;
    FExtendedFieldsInfo: boolean;
    FDefaultValues: boolean;
    FFieldOrigins: TFieldOrigins;
    FReadOnly: boolean;
    FFullRefresh: boolean;
    FSetFieldsReadOnly: boolean;
    FKeyFields: string;
    FUpdatingTable: string;
    FFetchBuffer: IntPtr;
    FFetchBufferSize: integer;

    FAfterExecFetch: TBoolProc;
    FAfterFetchAll: TBoolProc;
    FOnBeforeFetch: TBeforeFetchProc;
    FOnAfterFetch: TAfterFetchProc;
    FOnDataChanged: TDataChangeProc;
    FOnReopen: TDataChangeProc;
    FWaitForFetchBreak: boolean;
    FLastRowsObtained: integer;

  {$IFNDEF LITE}
    FInsertAllSetFields: boolean;
    FForceInitFieldsOnFetch: boolean;

    FChangedIndicatorOffset: Integer;
    FInFetching: Boolean;
    FEncryptStateOffset: Integer;
    FFetchStateOffset: Integer;
    FRefCountOffset: Integer;
    FItemPtrOffset: Integer;

    FConvertInfo: TConvertInfo;
    FDataTypeMap: TCRMapRules;
    FEncryptor: TCREncryptor;
    FEncryptedFields: string;

    FOriginalSQL: string;
    FOriginalParamsCount: integer;
    FSmartFetchState: TSmartFetchState;
    FGetKeyValuesSQL: TGetSQLProc;
    FPrefetchedFields: string;
    FNotFetchPrefetchedFields: boolean;
    FLiveBlockOnSmartFetch: boolean;
    FDataFieldsWasSynchronized: boolean;
    FSmartFetchInfo: TSmartFetchInfo;
    FIsFetchingDataByKey: boolean;
    FSmartFetchBlock: PBlockHeader;
    FItemRefCounts: TItemRefCountArr;
    FLastItemRefInd: Integer;
    FFirstFetchedItem: PItemHeader;
  {$ENDIF}

    FTablesInfo: TCRTablesInfo;
    FUpdTableIsArtificial: boolean;
    FUpdatingTableInfoIdx: integer;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FIdentityField: TCRFieldDesc;
    FIdentityIsPartOfComplexPK: boolean;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FKeyGeneratorField: TCRFieldDesc;
    FGetDBKeyList: TGetDBKeyListProc;
    FFillExtFieldsInfo: TFillExtFieldsInfoProc;
    FExtFieldsInfoInited: Boolean;

    FCachedKeyFieldDescs: array[Boolean] of TFieldDescArray;
    FKeyFieldDescsIsCached: array[Boolean] of Boolean;
    FCachedDataFieldDescs: array[Boolean] of TFieldDescArray;
    FDataFieldDescsIsCached: array[Boolean] of Boolean;

    procedure CreateCommand; virtual; abstract;
    procedure FreeCommand;
    procedure SetCommand(Value: TCRCommand); virtual;
    procedure SetTrimFixedChar(Value: Boolean); override;
  {$IFNDEF LITE}
    procedure InitRecordSize; override;
    function GetChangedIndicatorSize: Integer;

  { Data Type Map }
    function CreateDataTypeMap: TCRMapRules;

  { Encryption }
    procedure DecryptBuffer(Item: PItemHeader);
    function FieldIsEncrypted(const FieldName: string): boolean;
    function GetDataTypeName(Field: TCRFieldDesc): string;
    function GetEncryptStateSize: Integer;
  {$ENDIF}

  { Fields}
    function ExtFieldsInfoIsInternal: boolean; virtual;
    procedure ClearCachedKeyFieldDescs;
    function CanUseAllKeyFields: boolean; virtual;
    function IdentityFieldIsData: boolean; virtual;
    procedure FillFieldDescs(out FieldDescs: TFieldDescArray;
      const FieldNames: string; CheckFields: boolean);
    procedure FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray;
      ForceUseAllKeyFields: boolean); virtual;
    procedure FillDataFieldDescs(out DataFieldDescs: TFieldDescArray;
      ForceUseAllKeyFields: boolean); virtual;
    function FindTableInfoBySimpleName(const Name: string): integer; virtual;
  {$IFNDEF LITE}
    procedure ApplyColumnsInfo(Columns: TCRColumnsInfo; ReadFieldsFromServer: boolean; DefaultTable: integer; AsteriskCount: integer); virtual;
    procedure RequestFieldsInfo(Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo); virtual;
  {$ENDIF}

  { Open/Close }
    function NeedInitFieldsOnPrepare: boolean; virtual;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

  { Fetch }
    procedure AllocFetchBuffer; virtual;
    procedure FreeFetchBuffer; virtual;
    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); virtual;
    procedure ProcessFetchedBlock(Block: PBlockHeader; FetchBack: boolean); virtual;
    procedure ProcessNoResult(FetchBack: boolean); virtual;
    function ProcessFetchedException(E: Exception): boolean; virtual;
    function FetchingAccessible(FetchBack: boolean): boolean; virtual;
    function InternalFetch(FetchBack: boolean = False): boolean; virtual;

    procedure InitBlock(Block: PBlockHeader); virtual;
    procedure ClearBlock(Block: PBlockHeader);
    procedure CreateBlockStruct(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean = False{$IFNDEF LITE}; StandAloneBlock: Boolean = False{$ENDIF});
    function CanFetchBack: boolean; virtual; // Return True, if BlockMan is store only one block of records
    procedure InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean); virtual;
    procedure DoBeforeFetch(out Cancel: boolean); virtual;
    procedure DoAfterFetch; virtual;

    function NeedInitFieldsOnFetch: boolean; virtual;
    function NeedUnPrepareAfterFetch: boolean; virtual;
    function RequiredReInitFields: boolean; virtual;

  {$IFNDEF LITE}
    procedure InitItem(Item: PItemHeader); override;
    procedure DeleteItem(Item: PItemHeader); override;

  { Smart Fetch }
    function GetFetchStateSize: Integer;
    function GetRefCountSize: Integer;
    function GetItemPtrSize: Integer;
    function IsFieldPrefetched(Field: TFieldDesc): boolean;
    procedure CheckFetched(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure CheckIfFetchedItem(var CheckedItem: PItemHeader; var AnyRecWasDeleted: boolean);

    procedure SyncKeyFields(KeyFields, OriginFields: TFieldDescs); virtual;
    procedure SyncDataFields(DataFields, OriginFields: TFieldDescs); virtual;

    procedure ClearDataByKeyParams;
    procedure SetSmartFetchSQL(FirstFetchingItem: PItemHeader = nil; RowCount: integer = 0);

    procedure FetchDataByKey(FirstFetchingItem: PItemHeader; RowCount: integer);
    procedure SyncBlocks(FirstFetchingItem: PItemHeader; RowCount: integer);
    function GetFetchedStatus(RecBuf: IntPtr): TFetchedStatus;
    procedure SetFetchedStatus(RecBuf: IntPtr; Value: TFetchedStatus);
    function GetRefCountInd(RecBuf: IntPtr): integer;
    procedure WriteRefCountInd(RecBuf: IntPtr; RefInd: integer);
    function GetNewRefCountInd: integer;
    procedure DisposeRefCountInd(RefInd: integer);
    function GetItemFromRecBuf(RecBuf: IntPtr): PItemHeader;
    procedure WriteItemPtrIntoRecBuf(RecBuf: IntPtr; Item: PItemHeader);
  {$ENDIF}

  { Items/Data }
    procedure FreeData; override;
   // procedure PrepareData; override;

  { TablesInfo }
    function GetTableInfoClass: TTableInfoClass;
    function GetUpdatingTableInfo: TCRTableInfo;

  { Sorting }
    procedure CheckIndexFields; override;
    procedure SetSortDefaults(SortColumn: TSortColumn); override;

    function GetComponent: TObject;
    procedure SetComponent(Value: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;

  { Open/Close }
    function IsFullReopen: boolean; override;
    procedure Reopen; override;
    procedure Prepare; override;
    procedure UnPrepare; override;
    procedure Disconnect; virtual;

    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); virtual; // Execute command
    procedure CloseCommand; // Close command

  { Fields }
    function GetFieldDescType: TFieldDescClass; override;
    procedure CheckFieldDescs;
  {$IFNDEF LITE}
    procedure InitExtFieldsInfo;
    procedure GetExtFieldsInfo(RequestFieldsInfo: TRequestFieldsInfoProc);
    procedure ClearFields; override;
    procedure InternalInitFieldDescs; override;
    function IsEqualDataTypes(Field: TFieldDesc; MapRule: TCRMapRule): boolean; overload;
    procedure SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean); override;
    function GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; override;
    function GetFieldObject(Field: TFieldDesc; RecBuf: IntPtr): TSharedObject;
  {$ENDIF}
    function GetMappedDataBuf(FieldDesc: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; var DataType: Word; var HasParent, IsFixed: boolean): IntPtr; override;
    function GetBufForDataMappingConverter(Source: IntPtr; DataType: Word; HasParent: boolean; NeedConvert: boolean): IntPtr;
    procedure PutBufAfterDataMappingConverter(Source, Dest: IntPtr; DataType: Word; HasParent: boolean);
    procedure GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean); override;
    procedure PutFieldData(Field: TFieldDesc; DataBuf: IntPtr; DataLenPtr: PWord; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False); override;

    procedure InitUpdatingTableIdx;
    procedure ClearCachedFieldDescs;
    procedure GetKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllFields: boolean = False);
    procedure GetDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllFields: boolean = False);
    procedure GetKeyAndDataFields(out KeyAndDataFields: TKeyAndDataFields; ForceUseAllFields: boolean);
    procedure DetectIdentityField; virtual;
    procedure DetectKeyGeneratorField; virtual;
    procedure ClearKeyGeneratorField;
    procedure SetFieldsReadOnly;
    procedure SetIdentityField(Value: TCRFieldDesc);

  { Records }
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure GetPriorRecord(RecBuf: IntPtr); override;
    procedure DeleteRecord; override;

  { Fetch }
    function Fetch(FetchBack: boolean = False): boolean; override;
    procedure ExecFetch(DisableInitFields: boolean); virtual;
    procedure FetchAll; virtual;
    procedure BreakFetch; virtual;// Breaks fetch. Can be called from other thread or in non-blocking mode
    procedure WaitForFetch; virtual; // Wait for fetch first block of data in non-blocking mode
    function CanDisconnect: boolean; virtual;


    function RowsReturn: boolean; virtual;

    function GetCommand: TCRCommand;
    function GetConnection: TCRConnection;
    procedure SetConnection(Value: TCRConnection); virtual;
    procedure SetTransaction(Value: TCRTransaction); virtual;
    procedure SetSQL(const Value: string); virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; out Value: variant): boolean; virtual;

  { Filter }
    function IsCaseSensitive: boolean; virtual;
    procedure FilterUpdated; override;

  {$IFNDEF LITE}
    function GetChanged(Field: TFieldDesc; RecBuf: IntPtr): boolean; override;
    procedure SetChanged(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean); override;
    procedure ClearChangedIndicators(RecBuf: IntPtr);

  { Data Type Map }
    function GetMapRule(Field: TCRFieldDesc): TCRMapRule; overload;
    function GetMapRule(const FieldName: string; DBType: Word; DBLength, DBScale: Integer): TCRMapRule; overload;
    function GetMapFetchConverter(DBType: Word; DBLength, DBScale: Integer): TFetchConverter; overload;
    function GetMapFetchConverter(const FieldName: string; DBType: Word; DBLength, DBScale: Integer): TFetchConverter; overload;
    function GetMapOnDemandConverter(Field: TCRFieldDesc; out MapRule: TCRMapRule): TOnDemandConverter; overload; virtual;
    function GetMapOnDemandConverter(DataType: Word; DBType: Word; DBLength, DBScale: Integer; out MapRule: TCRMapRule): TOnDemandConverter; overload;
    function GetMapOnDemandConverter(const FieldName: string; DataType: Word; DBType: Word; DBLength, DBScale: Integer; out MapRule: TCRMapRule): TOnDemandConverter; overload; virtual;

  { Encryption }
    function GetEncrypted(Field: TFieldDesc; RecBuf: IntPtr): boolean;
    procedure SetEncrypted(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
    function IsEncryptableDataType(DataType: Word): boolean; virtual;
    function GetDecryptDataType(DataType: Word): Word; virtual;

  { Smart fetch }
    procedure InitRecord(RecBuf: IntPtr); override;
    procedure PutRecord(RecBuf: IntPtr); override;
    procedure AddRef(RecBuf: IntPtr); override;
    procedure ReleaseRef(RecBuf: IntPtr; IsResync: boolean; WithBlob: boolean); override;

    property FetchRows: Integer read FFetchRows;
    property DataTypeMap: TCRMapRules read FDataTypeMap;
    property Encryptor: TCREncryptor read FEncryptor write FEncryptor;
    property ForceInitFieldsOnFetch: boolean read FForceInitFieldsOnFetch write FForceInitFieldsOnFetch;

    property SmartFetchState: TSmartFetchState read FSmartFetchState write FSmartFetchState;
    property OriginalSQL: string read FOriginalSQL;
    property GetKeyValuesSQL: TGetSQLProc read FGetKeyValuesSQL write FGetKeyValuesSQL;
    property NotFetchPrefetchedFields: boolean read FNotFetchPrefetchedFields write FNotFetchPrefetchedFields;
  {$ENDIF}

    function NeedConvertEOL: boolean; override;

    property AfterExecFetch: TBoolProc read FAfterExecFetch write FAfterExecFetch;
    property AfterFetchAll: TBoolProc read FAfterFetchAll write FAfterFetchAll;
    property OnBeforeFetch: TBeforeFetchProc read FOnBeforeFetch write FOnBeforeFetch;
    property OnAfterFetch: TAfterFetchProc read FOnAfterFetch write FOnAfterFetch;
    property OnDataChanged: TDataChangeProc read FOnDataChanged write FOnDataChanged;
    property OnReopen: TDataChangeProc read FOnReopen write FOnReopen;

  { Sorting }
    procedure SortItems; override;

  { TablesInfo }
    property TablesInfo: TCRTablesInfo read FTablesInfo;
    property IdentityField: TCRFieldDesc read FIdentityField;
    property KeyGeneratorField: TCRFieldDesc read FKeyGeneratorField;
    property UpdatingTableInfoIdx: integer read FUpdatingTableInfoIdx;
    property UpdatingTableInfo: TCRTableInfo read GetUpdatingTableInfo;
    property UpdTableIsArtificial: boolean read FUpdTableIsArtificial;
    property GetDBKeyList: TGetDBKeyListProc read FGetDBKeyList write FGetDBKeyList;
    property FillExtFieldsInfo: TFillExtFieldsInfoProc read FFillExtFieldsInfo write FFillExtFieldsInfo;
    property SQLGenerator: TSQLGenerator read FSQLGenerator write FSQLGenerator;

    property OnConnectRequest: TConnectProc read FOnConnectRequest write FOnConnectRequest;
    property OnDisconnectRequest: TConnectProc read FOnDisconnectRequest write FOnDisconnectRequest;
    property Component: TObject read GetComponent write SetComponent;
  end;

{ TParamDesc }

  TParamDirection = (pdUnknown, pdInput, pdOutput, pdInputOutput, pdResult);

  TParamDesc = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FCommand: TCRCommand;

    FName: string;
    FDataType: word;
    FSubDataType: word;
    FParamType: TParamDirection;

    FSize: integer;
    FPrecision: integer;
    FScale: integer;
    FData: Variant; // pointer;
    FIsNull: boolean;
    FIsBound: boolean;
    FConvertEOL: boolean;
    FNational: boolean;
    FParamObject: TSharedObject;
    FArraySize: integer;

  {$IFNDEF LITE}
    FEncryptor: TEncryptionMethod;
    FEncryptedBlob: TBlob;
  {$ENDIF}

  {$IFNDEF LITE}
    function EncryptValue(DataType: Word; const Value: Variant): Variant;
    function EncryptObject(DataType: Word; const Value: TSharedObject): TSharedObject;
  {$ENDIF}

    procedure AllocBuffer; virtual;
    procedure FreeBuffer; virtual;

    procedure CheckIndex(Index: integer); {$IFDEF USE_INLINE}inline;{$ENDIF}

    function GetItemNull(Index: integer): boolean; virtual;
    procedure SetItemNull(Index: integer; Value: boolean); virtual;
    function GetItemObject(Index: integer): TSharedObject; virtual;
    procedure SetItemObject(Index: integer; Value: TSharedObject); virtual;
    function GetItemValue(Index: integer): Variant; virtual;
    procedure SetItemValue(Index: integer; const Value: Variant); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TParamDesc); virtual;

    procedure Clear; virtual;

    function GetMinDefaultSize: Integer; virtual;
    function GetMaxStringSize(Connection: TCRConnection): Integer; virtual;

    function GetName: string;
    procedure SetName(const Value: string);

    function GetDataType: word;
    procedure SetDataType(Value: word); virtual;

    function GetSubDataType: word;
    procedure SetSubDataType(Value: word); virtual;

    function GetParamType: TParamDirection;
    procedure SetParamType(Value: TParamDirection); virtual;

    function GetArraySize: integer;
    procedure SetArraySize(Value: integer); virtual;

    function GetSize: integer;
    procedure SetSize(Value: integer); virtual;

    function GetPrecision: integer;
    procedure SetPrecision(Value: integer);

    function GetScale: integer;
    procedure SetScale(Value: integer);

    function GetValue: Variant; virtual;
    procedure SetValue(const Value: Variant); virtual;
    procedure SetValueArr(PValueArr: PVariantArray); virtual;
    procedure SetObjectArr(PValueArr: PVariantArray); virtual;

    function GetItemPtr(Index: integer): PVariant; virtual;

    function IsObjectValue: boolean; virtual;
    function GetObject: TSharedObject; virtual;
    procedure SetObject(Value: TSharedObject); virtual;

    function GetNull: boolean; virtual;
    procedure SetNull(const Value: boolean); virtual;

    function GetIsBound: boolean;
    procedure SetIsBound(Value: boolean); virtual;

    function GetNational: boolean; virtual;
    procedure SetNational(Value: boolean); virtual;

    procedure SetConvertEOL(const Value: boolean);
  {$IFNDEF LITE}
    procedure SetEncryptor(const Value: TEncryptionMethod);
  {$ENDIF}

    class function NeedAssignScalarValues: boolean; virtual;
    class function NeedAssignObjectValues: boolean; virtual;

    property Value: Variant read FData write SetValue;
    property ItemNull[Index: integer]: boolean read GetItemNull write SetItemNull;
    property ItemObject[Index: integer]: TSharedObject read GetItemObject write SetItemObject;
    property ItemValue[Index: integer]: Variant read GetItemValue write SetItemValue;
  end;

{ TParamDescs }

  TParamDescs = class (TCRObjectList)
  private
    function GetItems(Index: integer): TParamDesc; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    function FindParam(const Name: string): TParamDesc;
    function ParamByName(const Name: string): TParamDesc;
    property Items[Index: integer]: TParamDesc read GetItems; default;
  end;

{ TDAParamInfo }

  TDAParamInfo = class(TCollectionItem)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FieldDesc: TCRFieldDesc;
    Old: boolean;
    ParamName: string;
    ParamType: TParamDirection;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    ParamRef: TParamDesc;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    SB: StringBuilder;
    StartPosition: integer;
    EndPosition: integer;
  end;

  TDAParamsInfo = class(TCollection)
  protected
    function GetItem(Index: Integer): TDAParamInfo;
    procedure SetItem(Index: Integer; Value: TDAParamInfo);
  public
    property Items[Index: Integer]: TDAParamInfo read GetItem write SetItem; default;
  end;

{ TCRConnections }

  TCRConnections = class(TList)
  private
    function GetItems(Index: integer): TCRConnection;
  public
    property Items[Index: integer]: TCRConnection read GetItems; default;
  end;

{ TCRTransaction }

  TCRErrorProc = procedure (E: Exception; var Fail: boolean) of object;

  TCRTransactionAction = (taCommit, taRollback);

  TCRTransaction = class
  private
    FLock: TCriticalSection;
  protected
    FActive: boolean;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FComponent: TObject;
    FOnError: TCRErrorProc;
    FConnections: TCRConnections;
    FIsolationLevel: TCRIsolationLevel;
    FReadOnly: boolean;
    FNativeTransaction: boolean;

    procedure Lock; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Unlock; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure CheckInactive;
    procedure CheckActive;

    function AddConnection(Connection: TCRConnection): boolean; virtual;
    function RemoveConnection(Connection: TCRConnection): boolean; virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    // return transaction state w/o server query
    function GetInTransaction: boolean; virtual;
    // In ODAC executes query to detect actual transaction state.
    // If CanActivate = True, transaction start and transaction end on server side can be detected.
    // If CanActivate = False, only transaction end can be detected.
    function DetectInTransaction(CanActivate: boolean): boolean; virtual;
    procedure AssignConnect(Source: TCRTransaction); virtual;

    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;
    function GetMultiTransactionID: Int64; virtual;

    procedure CommitRetaining; virtual;
    procedure RollbackRetaining; virtual;
    procedure Savepoint(const Name: string); virtual;
    procedure ReleaseSavepoint(const Name: string); virtual;
    procedure RollbackToSavepoint(const Name: string); virtual;

    procedure Reset; virtual;
    function CanRestoreAfterFailover: boolean; virtual;

    property Component: TObject read FComponent write FComponent; // Is needed for failover
    property OnError: TCRErrorProc read FOnError write FOnError;
  end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  TMTSTransaction = class(TCRTransaction)
  private
    FMTSGC: ICRTransactionDispenserSC;
    FMTSTrans: ICRTransactionSC;

  protected
    procedure StartMTSTransaction; virtual;
    procedure CompleteMTSTransaction(Commit: boolean); virtual;

    procedure EnlistLink(Connection: TCRConnection); virtual;
    procedure UnEnlistLink(Connection: TCRConnection); virtual;
  public
    destructor Destroy; override;

    function AddConnection(Connection: TCRConnection): boolean; override;
    function RemoveConnection(Connection: TCRConnection): boolean; override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: string); override;
    procedure ReleaseSavepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;

    property MTSTransaction: ICRTransactionSC read FMTSTrans;
  end;
{$ENDIF}
{$ENDIF}

{ TCRMetaData }

{$IFDEF LITE}
  {$DEFINE DBX_METADATA}
{$ENDIF}

  TDataHelper = class
  private
    FData: TData;
    FRecBuf: IntPtr;

    procedure FreeBuffer;
    function GetFieldValue(Index: integer): variant;
    procedure SetFieldValue(Index: integer; const Value: variant);

  public
    constructor Create(Data: TData);
    destructor Destroy; override;

    procedure AllocBuffer;
    procedure InitRecord;
    procedure AppendRecord;
    function NextRecord: boolean;

    property FieldValues[Index: integer]: variant read GetFieldValue write SetFieldValue;
  end;

  TBooleanArray = array of boolean;

  TCRMetaData = class
  protected
    FMemData: TMemData;
    FMemDataHelper: TDataHelper;
    FRecordSet: TCRRecordSet;
    FRecordSetHelper: TDataHelper;
    FOperator: string;

    function CreateRecordSet: TCRRecordSet; virtual; abstract;
    procedure AddField(const AName: string; AType: integer; ALength: integer = -1);
    procedure CopyRecord(const SourceIndices, DestIndices: array of integer);
    function ParseTypes(const ObjectTypes: string; AllTypes: array of string): TBooleanArray; overload;
    procedure ParseTypes(const ObjectTypes: string; {out}TypesList: TStringList); overload;
    procedure AddWhere(var WhereClause: string; const Name, Value: string; AddEmpty: boolean = False);

    function InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData; overload; virtual;

    procedure CreateMetaDataKindsFields; virtual;
    procedure InternalGetMetaDataKindsList(List: TStringList); virtual;
    function GetMetaDataKinds: TData; virtual;
    procedure CreateRestrictionsFields; virtual;
    procedure InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string); virtual;
    function GetRestrictions(Restrictions: TStrings): TData; virtual;
    procedure CreateTablesFields; virtual;
    function GetTables(Restrictions: TStrings): TData; virtual; abstract;
    procedure CreateColumnsFields; virtual;
    function GetColumns(Restrictions: TStrings): TData; virtual; abstract;
    procedure CreateProceduresFields; virtual;
    function GetProcedures(Restrictions: TStrings): TData; virtual; abstract;
    procedure CreateProcedureParametersFields; virtual;
    function GetProcedureParameters(Restrictions: TStrings): TData; virtual; abstract;
    procedure CreateIndexesFields; virtual;
    function GetIndexes(Restrictions: TStrings): TData; virtual; abstract;
    procedure CreateIndexColumnsFields; virtual;
    function GetIndexColumns(Restrictions: TStrings): TData; virtual; abstract;
    procedure CreateConstraintsFields; virtual;
    function GetConstraints(Restrictions: TStrings): TData; virtual; abstract;
    procedure CreateConstraintColumnsFields; virtual;
    function GetConstraintColumns(Restrictions: TStrings): TData; virtual;

    function GetDatabases(Restrictions: TStrings): TData; virtual;
    procedure CreateDatabasesFields; virtual;
    function GetDataTypes(Restrictions: TStrings): TData; virtual;
    function GetUsers(Restrictions: TStrings): TData; virtual;
    function GetRoles(Restrictions: TStrings): TData; virtual;
    function GetUDTs(Restrictions: TStrings): TData; virtual;
    function GetPackages(Restrictions: TStrings): TData; virtual;
  {$IFDEF DBX_METADATA}
    procedure CreateObjectListFields; virtual;
  {$ENDIF}

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetMetaData(Connection: TCRConnection; Transaction: TCRTransaction;
      const MetaDataKind: string; Restrictions: TStrings): TData; virtual;

    procedure GetMetaDataKindsList(List: TStrings);
    procedure GetRestrictionsList(List: TStrings; const MetaDataKind: string);
  end;

{ TCRCursor }

  TCRCursor = class (TSharedObject)
  public
    function CanFetch: boolean; virtual; abstract;
  end;

{ TCRLoader }

  TCRLoaderColumn = class
  private
    FActualFieldNo: integer;
    FName: string;
    FDataType: word;
    FSubDataType: word;
    FSize: integer;
    FDataSize: integer;
    FPrecision: integer;
    FScale: integer;
  public
    constructor Create; virtual;

    procedure UpdateDataType(Value: word); virtual;

    property ActualFieldNo: integer read FActualFieldNo write FActualFieldNo;
    property Name: string read FName write FName;
    property DataType: word read FDataType write FDataType;
    property SubDataType: word read FSubDataType write FSubDataType;
    property Size: integer read FSize write FSize;
    property DataSize: integer read FDataSize write FDataSize;
    property Precision: integer read FPrecision write FPrecision;
    property Scale: integer read FScale write FScale;
  end;

  TCRLoaderColumns = class(TCRObjectList)
  private
    function GetColumn(Index: integer): TCRLoaderColumn;
    procedure SetColumn(Index: integer; Value: TCRLoaderColumn);
  public
    function GetColumnIndexByName(const Name: string): Integer;
    function FindColumnByName(const Name: string): TCRLoaderColumn;
    function GetColumnByName(const Name: string): TCRLoaderColumn;

    property Items[Index: integer]: TCRLoaderColumn read GetColumn write SetColumn; default;
  end;

  TCRLoader = class
  protected
    FConnection: TCRConnection;
    FTransaction: TCRTransaction;
    FTableName: string;
    FColumns: TCRLoaderColumns;
    FLastRow: integer;
    FLoadedRows: integer;
    FSkipReadOnlyFieldDescs: boolean;
    FObjectReleaseNeeded: boolean;
    FQuoteNames: boolean;
    FUseBlankValues: boolean;

    procedure SetConnection(Value: TCRConnection); virtual;
    procedure SetTransaction(Value: TCRTransaction); virtual;
    procedure CheckTableName;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); virtual;

    procedure DoPrepare; virtual;
    procedure DoPutColumnData(Col: integer; Row: integer; const Value: variant); virtual;
    procedure DoAddObjectRef(const Value: variant);
    procedure DoReleaseObjectRef(const Value: variant);
    function IsPutColumnDataAllowed: boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    class function GetColumnClass: TCRLoaderColumnClass; virtual;
    procedure Prepare; overload; virtual;
    procedure Prepare(RecordCount: integer); overload; virtual;
    procedure Reset; virtual;
    procedure DoLoad; virtual;
    procedure Finish; virtual;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); virtual;
    procedure CreateColumns;
    procedure CheckColumnsInfo;
    procedure DiscardRow;

    property Connection: TCRConnection read FConnection write SetConnection;
    property Transaction: TCRTransaction read FTransaction write SetTransaction;
    property TableName: string read FTableName write FTableName;
    property Columns: TCRLoaderColumns read FColumns;
    property QuoteNames: Boolean read FQuoteNames write FQuoteNames;
    property UseBlankValues: Boolean read FUseBlankValues write FUseBlankValues;
  end;

{ TCRSimpleLoader }

  TCRSimpleLoader = class (TCRLoader)
  protected
    FCommand: TCRCommand;

    procedure CreateCommand; virtual; abstract;

    procedure DoLoadRow; virtual;
    procedure DoPrepare; override;
    procedure DoPutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoAfterLoadRow;
    function IsPutColumnDataAllowed: boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;
  end;

{ TCRAlerter }

  TCRAlerterEventCallback = procedure(const EventName, Message: string) of object;
  TCRAlerterErrorCallback = procedure(E: Exception) of object;

  TCRAlerter = class
  protected
    FConnection: TCRConnection;
    FTransaction: TCRTransaction;
    FEventNames: TStrings;
    FActive: boolean;
    FOnEvent: TCRAlerterEventCallback;
    FOnError: TCRAlerterErrorCallback;

    procedure SetConnection(Value: TCRConnection); virtual;
    procedure SetTransaction(Value: TCRTransaction); virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    procedure SendEvent(const EventName, Message: string); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    function IsActive: boolean; virtual;

    property Connection: TCRConnection read FConnection write SetConnection;
    property Transaction: TCRTransaction read FTransaction write SetTransaction;
    property EventNames: TStrings read FEventNames;
    property OnEvent: TCRAlerterEventCallback read FOnEvent write FOnEvent;
    property OnError: TCRAlerterErrorCallback read FOnError write FOnError;
  end;

function GenerateTableName(const FieldDesc: TFieldDesc): string;

var
  DefaultSQLInfo: TSQLInfo = nil;
  MacroChar: Char = '&';

{$IFDEF AUTOTEST}
  __ServerExecuteCount: integer;
  __ServerPrepareCount: integer;
  __SwapConnectionCount: integer;
{$ENDIF}

implementation

uses
  RTLConsts,
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  ComObj,
{$ENDIF}
  CRConnectionPool,
{$ENDIF}
  CRFunctions, DAConsts;

{ TCRAccess }

function GenerateTableName(const FieldDesc: TFieldDesc): string;
begin
  if TCRFieldDesc(FieldDesc).TableInfo <> nil then
    Result := TCRFieldDesc(FieldDesc).TableInfo.TableName
  else
    Result := '';
end;

function ExtractFieldName(const Fields: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(Fields)) and (Fields[I] <> ';') do Inc(I);
  Result := Trim(Copy(Fields, Pos, I - Pos));
  if (I <= Length(Fields)) and (Fields[I] = ';') then Inc(I);
  Pos := I;
end;

{ EFailOver }

constructor EFailOver.Create(ConnLostCause: TConnLostCause);
begin
  FConnLostCause := ConnLostCause;
  inherited Create(''{$IFDEF FPC}+#0{$ENDIF});
end;

{ TCRConnector }

constructor TCRConnector.Create(Owner: TCRConnection);
begin
  inherited Create;

  FOwner := Owner;
end;

class function TCRConnector.CloneException(E: Exception): Exception;
begin
  Result := Exception.Create(E.Message);
end;

function TCRConnector.GetClientVersionFull: string;
begin
  Result := '';
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
procedure TCRConnector.Enlist(Transaction: TMTSTransaction);
begin
  raise Exception.Create('MS DTC transactions not supported');
end;

procedure TCRConnector.UnEnlist(Transaction: TMTSTransaction);
begin
  raise Exception.Create('MS DTC transactions not supported');
end;
{$ENDIF}
{$ENDIF}


{ TCRConnection }

constructor TCRConnection.Create;
begin
  inherited;

  FConnected := False;
  FNativeConnection := True;
  FIsValid := True;
  FInternalTransaction := GetTransactionClass.Create;
  FInternalTransaction.AddConnection(Self);
  FSQLinfo := nil;
  FGetPooledConnection := nil;
  FUuidWithBraces := True;

  FSSLOptions := TSSLOptions.Create;
{$IFNDEF LITE}
  FIOHandler := nil;
  FHttpOptions := THttpOptions.Create;
  FProxyOptions := TProxyOptions.Create;
  FDataTypeMap := CreateDataTypeMap;
{$ENDIF}

  FCommandLock := TCriticalSection.Create;
end;

destructor TCRConnection.Destroy;
begin
  Disconnect;
  FInternalTransaction.Free;
  FSQLinfo.Free;
  FSSLOptions.Free;
{$IFNDEF LITE}
  FHttpOptions.Free;
  FProxyOptions.Free;
  FDataTypeMap.Free;
{$ENDIF}

  FCommand.Free;
  FRecordSet.Free;
  FCommandLock.Free;

  inherited;
end;

function TCRConnection.GetSQLInfo: TSQLInfo;
begin
  if FSQLInfo = nil then
    FSQLInfo := CreateSQLInfo;
  Result := FSQLInfo;
end;

function TCRConnection.CreateSQLInfo: TSQLInfo;
begin
  Result := GetCommandClass.GetSQLInfoClass.Create(GetCommandClass.GetParserClass);
end;

{$IFNDEF LITE}
function TCRConnection.CreateDataTypeMap: TCRMapRules;
begin
  Result := GetMapRulesClass.Create;
end;

{$IFDEF MSWINDOWS}
procedure TCRConnection.Enlist(MTSTransaction: TMTSTransaction);
begin
  raise Exception.Create('MS DTC transactions not supported');
end;

procedure TCRConnection.UnEnlist(MTSTransaction: TMTSTransaction);
begin
  raise Exception.Create('MS DTC transactions not supported');
end;
{$ENDIF}
{$ENDIF}

function TCRConnection.GetInternalCommandClass: TCRCommandClass;
begin
  Result := GetCommandClass;
end;

function TCRConnection.CheckCommand(Command: TCRCommand): boolean;
begin
  Result := True;
end;

function TCRConnection.CreateCommand: TCRCommand;
begin
  Result := GetInternalCommandClass.Create;
  Result.SetConnection(Self);
  Result.Component := Component;
  InitCommandProp(Result);
end;

procedure TCRConnection.InitCommandProp(Command: TCRCommand);
begin
  // Empty
end;

function TCRConnection.CheckRecordSet(RecordSet: TCRRecordSet): boolean;
begin
  Result := True;
end;

function TCRConnection.CreateRecordSet: TCRRecordSet;
begin
  Result := GetRecordSetClass.Create;
  Result.SetConnection(Self);
  InitRecordSetProp(Result);
end;

procedure TCRConnection.InitRecordSetProp(RecordSet: TCRRecordSet);
begin
  RecordSet.SetProp(prFlatBuffers, False);
  RecordSet.SetProp(prLongStrings, True);
  RecordSet.SetProp(prFetchAll, True);
end;

{$IFNDEF LITE}

class function TCRConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TCRCommand.GetMapRulesClass;
end;

function TCRConnection.CreateObject(DataType: Word; Transaction: TCRTransaction): TDBObject;
begin
  raise Exception.Create(SOperationNotSupported);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

{$ENDIF}

function TCRConnection.GetCommand: TCRCommand;
begin
  FCommandLock.Enter;
  try
    if FCommand <> nil then begin
      if CheckCommand(FCommand) then begin
        Result := FCommand;
        FCommand := nil;
      end
      else begin
        FreeAndNil(FCommand);
        Result := CreateCommand;
      end;
    end
    else
      Result := CreateCommand;
  finally
    FCommandLock.Leave;
  end;
end;

procedure TCRConnection.ReleaseCommand(var Command: TCRCommand);
begin
  if Command = nil then
    Exit;

  FCommandLock.Enter;
  try
    if FCommand = nil then begin
      FCommand := Command;
      Command := nil;
    end
    else
      FreeAndNil(Command);
  finally
    FCommandLock.Leave;
  end;
end;

procedure TCRConnection.ExecuteSQL(const SQL: string);
var
  Command: TCRCommand;
begin
  Command := GetCommand;
  try
    Command.SetSQL(SQL);
    Command.Execute;
  finally
    ReleaseCommand(Command);
  end;
end;

function TCRConnection.GetRecordSet: TCRRecordSet;
begin
  FCommandLock.Enter;
  try
    if FRecordSet <> nil then begin
      if CheckRecordSet(FRecordSet) then begin
        Result := FRecordSet;
        FRecordSet := nil;
      end
      else begin
        FreeAndNil(FRecordSet);
        Result := CreateRecordSet;
      end;
    end
    else
      Result := CreateRecordSet;
  finally
    FCommandLock.Leave;
  end;
end;

procedure TCRConnection.ReleaseRecordSet(var RecordSet: TCRRecordSet);
begin
  if RecordSet = nil then
    Exit;

  try
    RecordSet.Close;
  except
    FreeAndNil(RecordSet);
    raise;
  end;

  FCommandLock.Enter;
  try
    if FRecordSet = nil then begin
      FRecordSet := RecordSet;
      RecordSet := nil;
    end
    else
      FreeAndNil(RecordSet);
  finally
    FCommandLock.Leave;
  end;
end;

function TCRConnection.OpenRecordSet(const SQL: string): TCRRecordSet;
begin
  Result := GetRecordSet;
  try
    Result.SetSQL(SQL);
    Result.Open;
    if Result.RecordCount = 0 then
      ReleaseRecordSet(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCRConnection.Assign(Source: TCRConnection);
begin
  FServer := Source.FServer;
  FUsername := Source.FUsername;
  FPassword := Source.FPassword;
  FEnableBCD := Source.FEnableBCD;
  FEnableFMTBCD := Source.FEnableFMTBCD;
  FOnError := Source.FOnError;
{$IFNDEF LITE}
  FIOHandler := Source.FIOHandler;
  FHttpOptions.Assign(Source.FHttpOptions);
  FProxyOptions.Assign(Source.FProxyOptions);
{$ENDIF}
  FSSLOptions.Assign(Source.FSSLOptions);
end;

procedure TCRConnection.DoError(E: Exception; var Fail: boolean);
var
  Reconnect: boolean;
  Attempt: integer;
  OldConnectionTimeout: variant;
{$IFNDEF LITE}
  Reexecute: boolean;
  ConnLostCause: TConnLostCause;
{$ENDIF}
begin
  Attempt := 0;
  while not FInProcessError do begin
    Reconnect := Attempt > 0;
  {$IFNDEF LITE}
    Reexecute := False;
  {$ENDIF}
    if Assigned(OnError) then begin
      FInProcessError := True;
      try
      {$IFNDEF LITE}
        ConnLostCause := clUnknown;
      {$ENDIF}
        OnError(E, Fail, Reconnect{$IFNDEF LITE}, Reexecute{$ENDIF}, Attempt{$IFNDEF LITE}, ConnLostCause{$ENDIF});
      finally
        FInProcessError := False;
      end;
    end;

    if Reconnect then begin
      FReconnected := False;
      try
        FInProcessError := True;
        if Attempt = 0 then begin
          try
            GetProp(prConnectionTimeout, OldConnectionTimeout);
            SetProp(prConnectionTimeout, 1);
            Disconnect;
          finally
            SetProp(prConnectionTimeout, OldConnectionTimeout);
          end;
        end;
      except // don't raise exception
      end;
      try
        Connect('');
        if Assigned(FOnReconnectSuccess) then
          FOnReconnectSuccess;
        FReconnected := True;
      except // don't raise exception
      end;
      FInProcessError := False;

    {$IFNDEF LITE}
      if FReconnected and Reexecute then
        raise EFailOver.Create(ConnLostCause); //Failover
    {$ENDIF}

      inc(Attempt);
    end;

    if not Reconnect and (Attempt > 0) then
      if not FReconnected and Assigned(OnReconnectError) then begin
        FInProcessError := True;
        FConnected := True; // to bypass "Value <> GetConnected" check in TCustomDAConnection.SetConnected
        try
          OnReconnectError;
        except // don't raise exception
        end;
        FConnected := False;
        FInProcessError := False;
      end;

    if not Reconnect or FReconnected then
      break;
  end;
end;

procedure TCRConnection.Connect(const ConnectString: string);
begin
  FConnectionTime := {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
  FIsValid := True;
end;

function TCRConnection.GetConnected: boolean;
begin
  Result := FConnected;
end;

procedure TCRConnection.SetConnected(Value: boolean);
begin
  if Value then
    Connect('')
  else
    Disconnect;
end;

function TCRConnection.GetInternalTransaction: TCRTransaction;
begin
  Result := FInternalTransaction;
end;

procedure TCRConnection.SetIsolationLevel(const Value: TCRIsolationLevel);
begin
  FIsolationLevel := Value;
end;

procedure TCRConnection.SetUsername(const Value: string);
begin
  FUsername := Value;
end;

procedure TCRConnection.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TCRConnection.SetServer(const Value: string);
begin
  FServer := Value;
end;

function TCRConnection.GetUsername: string;
begin
  Result := FUsername;
end;

function TCRConnection.GetPassword: string;
begin
  Result := FPassword;
end;

function TCRConnection.GetServer: string;
begin
  Result := FServer;
end;

{$IFNDEF NODBACCESS}
procedure TCRConnection.ReturnToPool;
begin
  Assert(FPool <> nil);
  FOnError := nil;
  Component := nil;
  if TCRConnectionPool(FPool).PutConnection(Self) then
    if FIOHandler <> nil then
      TCRIOHandlerUtils.RegisterClient(FIOHandler, Self);
end;
{$ENDIF}

function TCRConnection.GetConnector: TCRConnector;
begin
  Result := nil;
end;

function TCRConnection.CanChangeDatabase: boolean;
begin
  Result := True;
end;

function TCRConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prServer:
      SetServer(Value);
    prUsername:
      SetUsername(Value);
    prPassword:
      SetPassword(Value);
    prConnectionTimeout: ;
    prAutoCommit:
      FAutoCommit := Value;
    prConvertEOL:
      FConvertEOL := Value;
    prDisconnectedMode:
      FDisconnectedMode := Boolean(Value);
    prDefaultSortType:
      FDefaultSortType := TSortType(Value);
    prDatabase: ;
    prPort: ;
    prIsolationLevel:
      SetIsolationLevel(TCRIsolationLevel(Value));
    prEnableBCD:
      FEnableBCD := Value;
    prEnableFmtBCD:
      FEnableFMTBCD := Value;
    prUuidWithBraces:
      FUuidWithBraces := Value;

  {$IFNDEF LITE}
    prUseHttp:
      FHttpOptions.Enabled := Value;
    prHttpUrl:
      FHttpOptions.Url := Value;
    prHttpUsername:
      FHttpOptions.Username := Value;
    prHttpPassword:
      FHttpOptions.Password := Value;
    prHttpTrustServerCertificate:
      FHttpOptions.TrustServerCertificate := Value;

    prProxyHostname:
      FProxyOptions.Hostname := Value;
    prProxyPort:
      FProxyOptions.Port := integer(Value);
    prProxyUsername:
      FProxyOptions.Username := Value;
    prProxyPassword:
      FProxyOptions.Password := Value;
    prProxySocksVersion:
      FProxyOptions.SocksVersion := TCRSocksVersion(integer(Value));
    prProxyResolveDNS:
      FProxyOptions.ResolveDNS := boolean(Value);
  {$ENDIF}

    prUseSSL:
      FSSLOptions.Enabled := Value;
    prSSLCA:
      FSSLOptions.CA := Value;
    prSSLCert:
      FSSLOptions.Cert := Value;
    prSSLKey:
      FSSLOptions.Key := Value;
    prSSLCipher:
      FSSLOptions.Cipher := Value;
    prSSLIgnoreServerCertificateValidity:
      FSSLOptions.IgnoreServerCertificateValidity := Value;
    prSSLIgnoreServerCertificateConstraints:
      FSSLOptions.IgnoreServerCertificateConstraints := Value;
    prSSLTrustServerCertificate:
      FSSLOptions.TrustServerCertificate := Value;
    prSSLIgnoreServerCertificateInsecurity:
      FSSLOptions.IgnoreServerCertificateInsecurity := Value;

  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prServer:
      Value := GetServer;
    prUsername: // used in Oracle dbExpress driver (TOraSQLMetaData.getProcedureParams)
                // to detect if schema name need to be included in procedure name
      Value := GetUsername;
    prPassword:
      Value := GetPassword;
    prConnectionTimeout:
      Value := 0;
    prAutoCommit:
      Value := FAutoCommit;
    prMaxStringSize:
      Value := 0;
    prLastInsertId:
      Value := 0;
    prIsolationLevel:
      Value := FIsolationLevel;
    prUuidWithBraces:
      Value := FUuidWithBraces;

  {$IFNDEF LITE}
    prUseHttp:
      Value := FHttpOptions.Enabled;
    prHttpUrl:
      Value := FHttpOptions.Url;
    prHttpUsername:
      Value := FHttpOptions.Username;
    prHttpPassword:
      Value := FHttpOptions.Password;
    prHttpTrustServerCertificate:
      Value := FHttpOptions.TrustServerCertificate;

    prProxyHostname:
      Value := FProxyOptions.Hostname;
    prProxyPort:
      Value := FProxyOptions.Port;
    prProxyUsername:
      Value := FProxyOptions.Username;
    prProxyPassword:
      Value := FProxyOptions.Password;
    prProxySocksVersion:
      Value := integer(FProxyOptions.SocksVersion);
    prProxyResolveDNS:
      Value := FProxyOptions.ResolveDNS;
  {$ENDIF}

    prUseSSL:
      Value := FSSLOptions.Enabled;
    prSSLCA:
      Value := FSSLOptions.CA;
    prSSLCert:
      Value := FSSLOptions.Cert;
    prSSLKey:
      Value := FSSLOptions.Key;
    prSSLCipher:
      Value := FSSLOptions.Cipher;
    prSSLIgnoreServerCertificateValidity:
      Value := FSSLOptions.IgnoreServerCertificateValidity;
    prSSLIgnoreServerCertificateConstraints:
      Value := FSSLOptions.IgnoreServerCertificateConstraints;
    prSSLTrustServerCertificate:
      Value := FSSLOptions.TrustServerCertificate;
    prSSLIgnoreServerCertificateInsecurity:
      Value := FSSLOptions.IgnoreServerCertificateInsecurity;

  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

{ TCRCommand }

constructor TCRCommand.Create;
begin
  inherited;

  FParams := TParamDescs.Create;
  FParamsInfo := TDAParamsInfo.Create(TDAParamInfo);
  FScanParams := True;
  FSQLInfo := nil;

  FBatchOwner := nil;
  FBatchCommand := nil;
  FBatchIters := 1;
  FBatchOffset := 0;
  FLastBatchIters  := 0;
  FParamsProcessed := 0;
end;

destructor TCRCommand.Destroy;
begin
  FBatchCommand.Free;
  FParams.Free;
  FParamsInfo.Free;
  FSQLInfo.Free;

  inherited;
end;

class function TCRCommand.GetTableInfoClass: TTableInfoClass;
begin
  Result := TCRTableInfo;
end;

class function TCRCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TSQLInfo;
end;

class function TCRCommand.GetParserClass: TSQLParserClass;
begin
  Assert(False, 'Must be overrided');
  Result := TSQLParser;
end;

class function TCRCommand.GetParamDescClass: TParamDescClass;
begin
  Result := TParamDesc;
end;

{$IFNDEF LITE}
class function TCRCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TCRMapRules;
end;
{$ENDIF}

procedure TCRCommand.Prepare;
begin
  SetCursorState(csPrepared);
end;

procedure TCRCommand.Unprepare;
begin
  SetCursorState(csInactive);

  if (FBatchCommand <> nil) and (FBatchCommand.GetPrepared) then
    FBatchCommand.Unprepare;
end;

{$IFDEF AUTOTEST}
procedure TCRCommand.Execute;
begin
  Inc(__ServerExecuteCount);
end;
{$ENDIF}

procedure TCRCommand.ExecuteBatch(Iters, Offset: integer);

  function GetSavepointName: string;
  const
    SavepointPrefix = 'SAVEPOINT_BU_';
  begin
    Result := IntToHex(NativeInt(Self), SizeOf(NativeInt) * 2);
    Result := SavepointPrefix + Result;
  end;

var
  Transaction: TCRTransaction;
  IsExplicitTransaction: boolean;
  SavepointName: string;
begin
  if not IsValidBatchSQL then
    raise Exception.Create(SInvalidBatchOperation);

  if (FParams.Count > 0) and (Iters + Offset > FParams[0].GetArraySize) then
    raise Exception.Create(SInvalidBatchParameters);

//  if GetCursorState > csPrepared then
//    Exit;

  CreateBatchCommand;

  Transaction := GetConnection.GetInternalTransaction;
  IsExplicitTransaction := Transaction.FActive;
  if IsExplicitTransaction then
    Transaction.Lock;
  try
    SavepointName := '';
    // NeedBatchTransaction = True:
    // - for InterBase/Firebird due to need for a transaction for any operation
    // - for SQLite due to significant performance degradation without a transaction
    if (NeedBatchTransaction or (GetBatchIters(Iters) <> Iters)) and not IsExplicitTransaction then
      Transaction.StartTransaction;
    // NeedBatchRollback = True for RDBMSes where a DML operation is performed as a set of statements
    // like INSERT ...; INSERT ...;
    if (NeedBatchSavepoint or (GetBatchIters(Iters) <> Iters)) and Transaction.FActive and IsExplicitTransaction then begin
      SavepointName := GetSavepointName;
      Transaction.Savepoint(SavepointName);
    end;

    try
      InternalExecuteBatch(Iters, Offset);

      if Transaction.FActive and not IsExplicitTransaction then
        Transaction.Commit;
    except
      if (NeedBatchTransaction or NeedBatchSavepoint) and Transaction.FActive then
        if SavepointName <> '' then
          Transaction.RollbackToSavepoint(SavepointName)
        else if not IsExplicitTransaction then
          Transaction.Rollback;

      if Assigned(FAfterExecute) then
        FAfterExecute(False);

      raise;
    end;
  finally
    if IsExplicitTransaction then
      Transaction.Unlock;
  end;

  if Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TCRCommand.Close;
begin
  // Empty
end;

procedure TCRCommand.SetConnection(Value: TCRConnection);
begin
  FConnection := Value;
end;

function TCRCommand.GetConnection: TCRConnection;
begin
  Result := FConnection;
end;

procedure TCRCommand.SetTransaction(Value: TCRTransaction);
begin
  // should be overloaded for Multiple Transactions databases
end;

function TCRCommand.GetTransaction: TCRTransaction;
begin
  // should be overloaded for Multiple Transactions databases
  Result := nil;
end;

procedure TCRCommand.SetSQL(const Value: string);
begin
  FParsedSQLType := qtUnparsed;
  FBatchIters := 1;
  FBatchOffset := 0;
  FLastBatchIters := 0;

  // Parameters must be recreated on Change SQL
  // for Value = FUserSQL parameters must be recreated also
  FUserSQL := Value;

  if FDisableParamScan then
    FSQL := Value
  else
    FSQL := ParseSQL(Value, FParams);
end;

function TCRCommand.GetSQLInfo: TSQLInfo;
begin
  if FConnection <> nil then
    Result := FConnection.SQLInfo
  else begin
    if FSQLInfo = nil then
      FSQLInfo := CreateSQLInfo;
    Result := FSQLInfo;
  end;
end;

function TCRCommand.CreateSQLInfo: TSQLInfo;
begin
  Result := GetSQLInfoClass.Create(Self.GetParserClass);
end;

procedure TCRCommand.AddParamPosition(const ParamName: string; StartPosition, EndPosition: integer; ParamRef: TParamDesc);
var
  ParamInfo: TDAParamInfo;
begin
  ParamInfo := TDAParamInfo(FParamsInfo.Add);
  ParamInfo.ParamName := ParamName;
  ParamInfo.ParamRef := ParamRef;
  ParamInfo.StartPosition := StartPosition;
  ParamInfo.EndPosition := EndPosition;
  ParamInfo.SB := nil;
end;

function TCRCommand.IsValidBatchSQL: boolean;
begin
  if FParsedSQLType = qtUnparsed then
    ParseSQLType;

  Result := FParsedSQLType in [qtInsert, qtUpdate, qtDelete, qtInsertReturning, qtUpdateReturning];
end;

procedure TCRCommand.CreateBatchCommand;
begin
  if Assigned(FBatchCommand) then
    Exit;

  FBatchCommand := FConnection.GetCommandClass.Create;
  FBatchCommand.FBatchOwner := Self;
  FBatchCommand.SetConnection(FConnection);

  FBatchCommand.SetProp(prDisableParamScan, True);
  FBatchCommand.FTrimFixedChar := Self.FTrimFixedChar;
end;

function TCRCommand.GetBatchCommand: TCRCommand;
begin
  Result := FBatchCommand;
end;

function TCRCommand.GetBatchSQL(Iters, Offset: integer): string;
begin
  Result := FSQL;
end;

function TCRCommand.GetBatchIters(Iters: integer): integer;
begin
  Result := Iters;
end;

function TCRCommand.NeedBatchTransaction: boolean;
begin
  Result := False;
end;

function TCRCommand.NeedBatchSavepoint: boolean;
begin
  Result := False;
end;

procedure TCRCommand.InternalExecuteBatch(Iters, Offset: integer);
var
  BatchCommand: TCRCommand;
begin
  BatchCommand := GetBatchCommand;
  Assert(BatchCommand <> nil);

  BatchCommand.SetSQL(GetBatchSQL(Iters, Offset));
  BatchCommand.FBatchIters := Iters;
  BatchCommand.FBatchOffset := Offset;
  BatchCommand.FParamsProcessed := 0;
  BatchCommand.Execute;
end;

function TCRCommand.IsLabelSyntax(Code: integer; PrevCode: integer): boolean;
begin
  Result := False;
end;

procedure TCRCommand.CheckSQLParamType(ParsedSQL: StringBuilder; Parser: TSQLParser; Param: TParamDesc);
begin
  // Empty
end;

procedure TCRCommand.ParseSQLParam(ParsedSQL: StringBuilder; Parser: TSQLParser; Params: TParamDescs; LeftQuote, RightQuote: Char; const RenamePrefix: string; PrevCode: integer = -1);
var
  Code: integer;
  St: string;
  DogPresent: boolean;
  l, p: integer;
  ParamName: string;
  Param: TParamDesc;
begin
  Code := Parser.GetNext(St);
  DogPresent := Code = lxAt;
  if DogPresent then
    Code := Parser.GetNext(St); // Omit '@' in ParamName for BDE compatibility

  if ((Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst)) and not IsLabelSyntax(Code, PrevCode) // and (St <> '=')
  then begin
    if DogPresent then
      ParamName := '@' + St
    else
      ParamName := St;

    if Code = lcNumber then begin
      Code := Parser.GetNext(St);
      if Code = lcIdent then
        ParamName := ParamName + St
      else
        Parser.Back;
    end;

    l := Length(ParamName);
    // remove quotes
    if (ParamName[1] = LeftQuote) and (ParamName[l] = RightQuote) then
      ParamName := Copy(ParamName, 2, l - 2);
    if Params <> nil then begin
      if FScanParams then begin
        Param := AddParam;
        Param.SetName(ParamName);
      end
      else
        if Params.Count > FParamsInfo.Count then
          Param := Params.ParamByName(ParamName)
        else
          Param := nil;

      p := ParsedSQL.Length + 1;
      ParsedSQL.Append('?');
      AddParamPosition(ParamName, p, ParsedSQL.Length + 1, Param);
      CheckSQLParamType(ParsedSQL, Parser, Param);
    end
    else
      ParsedSQL.Append(RenamePrefix + ParamName);
  end
  else begin // Labels in SQL Server, MySQL syntax and PL SQL Blocks (a := b).
    ParsedSQL.Append(':');
    if DogPresent then
      ParsedSQL.Append('@');
    ParsedSQL.Append(St);
  end;
end;

function TCRCommand.ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string;
var
  ParsedSQL: StringBuilder;
  Parser: TSQLParser;
  PrevCode, Code: integer;
  St: string;
  StartPos: integer;
  LeftQuote, RightQuote: Char;
begin

  FParamsInfo.Clear;
  if FScanParams and (Params <> nil) then
    Params.Clear;

  if (Pos(':', SQL) = 0) then begin
    Result := SQL;
    Exit;
  end;

  FParsedSQLType := qtUnknown;

  LeftQuote := SQLInfo.LeftQuote;
  RightQuote := SQLInfo.RightQuote;

  Code := lcEnd;

  ParsedSQL := StringBuilder.Create(Length(SQL) + (Length(SQL) shr 1));
  try
    Parser := GetParserClass.Create(SQL);
    try
      Parser.OmitBlank := False;
      Parser.OmitComment := True;
      Parser.QuotedString := True;
      Parser.DecSeparator := '.';

      StartPos := Parser.CurrPos;
      repeat
        PrevCode := Code;
        Code := Parser.GetNext(St);
        if (Code = lxColon) then begin
          ParsedSQL.Append(Copy(SQL, StartPos + 1, Parser.CurrPos - StartPos - 1));
          ParseSQLParam(ParsedSQL, Parser, Params, LeftQuote, RightQuote, RenamePrefix, PrevCode);
          StartPos := Parser.CurrPos;
        end
        else if FParsedSQLType = qtUnknown then
          case Code of
            lxSELECT, lxWITH:
              FParsedSQLType := qtSelect;
            lxINSERT:
              FParsedSQLType := qtInsert;
            lxUPDATE:
              FParsedSQLType := qtUpdate;
            lxDELETE:
              FParsedSQLType := qtDelete;
            else
              if SQLInfo.IsCursorSQLType(Code) then
                FParsedSQLType := qtCursor;
          end
        else if Code = lxRETURNING then
          if SQLInfo.DetectReturningSQLtype then
            case FParsedSQLType of
              qtInsert:
                FParsedSQLType := qtInsertReturning;
              qtUpdate:
                FParsedSQLType := qtUpdateReturning;
            end;
      until Code = lcEnd;
      ParsedSQL.Append(Copy(SQL, StartPos + 1, Parser.CurrPos - StartPos));
    finally
      Parser.Free;
    end;

    Result := ParsedSQL.ToString;
  finally
    ParsedSQL.Free;
  end;
end;

procedure TCRCommand.ParseSQL;
begin
  FSQL := ParseSQL(FUserSQL, FParams);
end;

procedure TCRCommand.ParseSQLType;
begin
  ParseSQLType(FSQL);
end;

procedure TCRCommand.ParseSQLType(const SQL: string);
var
  Parser: TSQLParser;
  Code: integer;
  Lexem: string;
begin
  FParsedSQLType := qtUnknown;

  Parser := GetParserClass.Create(SQL);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    Parser.QuotedString := True;
    Parser.DecSeparator := '.';

    Code := Parser.GetNext(Lexem);

    case Code of
      lxSELECT, lxWITH: begin
        FParsedSQLType := qtSelect;
        if Code = lxSELECT then
          case Parser.ToLexem([lxFROM, lxINTO]) of
            lcEnd:
              FParsedSQLType := qtSelectProc;
            lxINTO:
              FParsedSQLType := qtSelectInto;
          end;
      end;
      lxINSERT: begin
        FParsedSQLType := qtInsert;
        if SQLInfo.DetectReturningSQLtype then
          if Parser.ToLexem(lxRETURNING) <> lcEnd then
            FParsedSQLType := qtInsertReturning;
      end;
      lxUPDATE: begin
        FParsedSQLType := qtUpdate;
        if SQLInfo.DetectReturningSQLtype then
          if Parser.ToLexem(lxRETURNING) <> lcEnd then
            FParsedSQLType := qtUpdateReturning;
      end;
      lxDELETE:
        FParsedSQLType := qtDelete;
      else
        if SQLInfo.IsCursorSQLType(Code) then
          FParsedSQLType := qtCursor;
    end;
  finally
    Parser.Free;
  end;
end;

function TCRCommand.ForceCreateSPParams: boolean;
begin
  Result := False;
end;

{ Params }

class function TCRCommand.IsAllowedArrayType(DataType: Word): boolean;
begin
  Result := DataType in [dtUnknown..dtWideMemo];
end;

procedure TCRCommand.ClearParams;
begin
  FParams.Clear;
end;

function TCRCommand.AddParam: TParamDesc;
begin
  Result := GetParamDescClass.Create;
  FParams.Add(Result);
  Result.FCommand := Self;
end;

function TCRCommand.GetCursor: TCRCursor;
begin
  Result := nil;
end;

procedure TCRCommand.SetCursor(Value: TCRCursor);
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRCommand.BreakExec;
begin
end;

function TCRCommand.SetProp(Prop: integer; const Value: Variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      FAutoCommit := Value;
  {$IFDEF HAVE_COMPRESS}
    prCompressBlobMode:
      FCompressBlob := TCompressBlobMode(Integer(Value));
  {$ENDIF}
    prScanParams:
      if FScanParams <> Boolean(Value) then begin
        FScanParams := Boolean(Value);
        if FScanParams and (FUserSQL <> '') then
          ParseSQL;
      end;
    prDisableParamScan:
      FDisableParamScan := Value;
    prQuoteNames:
      FQuoteNames := Value;
    prCanReadParams:
      ;
    prEnableBCD:
      FEnableBCD := Value;
    prEnableFmtBCD:
      FEnableFMTBCD := Value;
    prIsStoredProc:
      ;
    prParamsProcessed:
      ;
    prNonBlocking:
      ;
    prBatchIters:
      FBatchIters := Value;
    prBatchOffset:
      FBatchOffset := Value;
    prCommandTimeout:
      ;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRCommand.GetProp(Prop: integer; out Value: Variant): boolean;
begin
  Result := True;
  case Prop of
    prCanReadParams:
      Value := True;
    prIsSelectParams:
      Value := False;
    prUseDescribeParams:
      Value := False;
    prScanParams:
      Value := FScanParams;
    prDisableParamScan:
      Value := FDisableParamScan;
    prQuoteNames:
      Value := FQuoteNames;
    prParamsProcessed:
      Value := FParamsProcessed;
    prNonBlocking:
      Value := False;
    prCommandTimeout:
      Value := 0;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

{ $IFNDEF LITE}

{ TCRTableInfo }

constructor TCRTableInfo.Create(Owner: TCRTablesInfo);
begin
  inherited Create;
  FOwner := Owner;
  FIndex := -1;
end;

procedure TCRTableInfo.SetTableName(const Value: string);
begin
  FTableName := Value;
  if FOwner <> nil then
    FOwner.TableNameChanged;
end;

procedure TCRTableInfo.SetTableAlias(const Value: string);
begin
  FTableAlias := Value;
  if FOwner <> nil then
    FOwner.TableAliasChanged;
end;

function TCRTableInfo.GetTableNameFull: string;
begin
  Result := FTableName;
end;

procedure TCRTableInfo.SetTableNameFull(const Value: string);
begin
end;

{ TCRTablesInfo }

constructor TCRTablesInfo.Create(TableInfoClass: TTableInfoClass);
begin
  inherited Create;
  FTableInfoClass := TableInfoClass;
  FTableNameList := TStringList.Create;
  FTableAliasList := TStringList.Create;
  FTableNameList.CaseSensitive := False;
  FTableAliasList.CaseSensitive := False;
  FUpdateCount := 0;
end;

destructor TCRTablesInfo.Destroy;
begin
  Clear;
  FTableNameList.Free;
  FTableAliasList.Free;
  inherited Destroy;
end;

procedure TCRTablesInfo.InternalAdd(TableInfo: TCRTableInfo);
var
  c: integer;
begin
  c := Count;
  SetLength(FList, c + 1);
  FList[c] := TableInfo;
  TableInfo.Index := c;
end;

function TCRTablesInfo.Add: TCRTableInfo;
begin
  Result := FTableInfoClass.Create(Self);
  InternalAdd(Result);
end;

procedure TCRTablesInfo.Clear;
var
  i: Integer;
begin
  if Count > 0 then begin
    for i := 0 to Count - 1 do
      FList[i].Free;
    SetLength(FList, 0);
    Changed;
  end;
end;

procedure TCRTablesInfo.Changed;
begin
  TableNameChanged;
  TableAliasChanged;
end;

procedure TCRTablesInfo.TableNameChanged;
var
  i: Integer;
begin
  if FUpdateCount = 0 then begin
    FTableNameList.Clear;
    FTableNameList.Sorted := False;
    for i := 0 to Count - 1 do
      FTableNameList.AddObject(FList[i].TableName, FList[i]);
    FTableNameList.Sorted := True;
  end;
end;

procedure TCRTablesInfo.TableAliasChanged;
var
  i: Integer;
begin
  if FUpdateCount = 0 then begin
    FTableAliasList.Clear;
    FTableAliasList.Sorted := False;
    for i := 0 to Count - 1 do
      FTableAliasList.AddObject(FList[i].TableAlias, FList[i]);
    FTableAliasList.Sorted := True;
  end;
end;

function TCRTablesInfo.GetCount: Integer;
begin
  Result := Length(FList);
end;

procedure TCRTablesInfo.SetCaseSensitive(Value: boolean);
begin
  FCaseSensitive := Value;
  FTableNameList.CaseSensitive := Value;
  FTableAliasList.CaseSensitive := Value;
end;

procedure TCRTablesInfo.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCRTablesInfo.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

function TCRTablesInfo.FindByName(const TableName: string): TCRTableInfo;
var
  Index: integer;
begin
  Index := IndexByName(TableName);
  if Index = -1 then
    Result := nil
  else
    Result := FList[Index];
end;

function TCRTablesInfo.IndexOf(TableInfo: TCRTableInfo): Integer;
begin
  Result := 0;
  while (Result < Count) and (FList[Result] <> TableInfo) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

function TCRTablesInfo.IndexByName(const TableName: string): Integer;
var
  i: integer;
begin
  if FUpdateCount = 0 then begin
    Result := FTableNameList.IndexOf(TableName);
    if Result <> -1 then
      Result := TCRTableInfo(FTableNameList.Objects[Result]).Index;
  end
  else begin
    Result := -1;
    for i := 0 to Count - 1 do
      if FCaseSensitive and (FList[i].TableName = TableName) or
        not FCaseSensitive and SameText(FList[i].TableName, TableName)
      then begin
        Result := i;
        Break;
      end;
  end;
end;

function TCRTablesInfo.IndexByAlias(const TableAlias: string): Integer;
var
  i: integer;
begin
  if FUpdateCount = 0 then begin
    Result := FTableAliasList.IndexOf(TableAlias);
    if Result <> -1 then
      Result := TCRTableInfo(FTableAliasList.Objects[Result]).Index;
  end
  else
  begin
    Result := -1;
    for i := 0 to Count - 1 do
      if FCaseSensitive and (FList[i].TableAlias = TableAlias) or
        not FCaseSensitive and SameText(FList[i].TableAlias, TableAlias)
      then begin
        Result := i;
        Break;
      end;
  end;
end;

function TCRTablesInfo.FindByNameAndAlias(const TableName, TableAlias: string): TCRTableInfo;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (FCaseSensitive and (FList[i].TableName = TableName) and ((FList[i].TableAlias = TableAlias) or (FList[i].TableAlias = ''))) or
      (not FCaseSensitive and SameText(FList[i].TableName, TableName) and (SameText(FList[i].TableAlias, TableAlias) or (FList[i].TableAlias = '')))
    then begin
      Result := FList[i];
      Break;
    end;
end;

function TCRTablesInfo.GetItem(Index: Integer): TCRTableInfo;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FList[Index];
end;

procedure TCRTablesInfo.SetItem(Index: Integer; const Value: TCRTableInfo);
begin
  if (Index >= 0) and (Index < Count) then
    if Value <> FList[Index] then
      FList[Index] := Value;
end;

{ TCRColumnsInfo }

function TCRColumnsInfo.GetItem(Index: integer): TCRColumnInfo;
begin
  Result := TCRColumnInfo(inherited Items[Index]);
end;

{ TSQLInfo }

constructor TSQLInfo.Create(ParserClass: TSQLParserClass);
begin
  inherited Create;
  FParserClass := ParserClass;
end;

function TSQLInfo.LeftQuote: Char;
begin
  Result := '"';
end;

function TSQLInfo.RightQuote: Char;
begin
  Result := '"';
end;

function TSQLInfo.IdentCase: TIdentCase;
begin
  Result := icMixed;
end;

function TSQLInfo.ParamQuoteAllowed: boolean;
begin
  Result := True;
end;

function TSQLInfo.ProcedureOverloadSeparator: Char;
begin
  Result := ':';
end;

function TSQLInfo.IsCursorSQLType(Code: Integer): boolean;
begin
  Result := False;
end;

function TSQLInfo.DetectReturningSQLType: boolean;
begin
  Result := False;
end;

function TSQLInfo.IsQuoted(const Value: string): boolean;
var
  l: integer;
begin
  l := Length(Value);
  if (l <= 1) then
    Result := False
  else
    Result := (Value[1] = LeftQuote) and (Value[l] = RightQuote);
end;

function TSQLInfo.Quote(const Value: string): string;
begin
  Result := Quote(Value, LeftQuote, RightQuote);
end;

function TSQLInfo.Quote(const Value: string; const LeftQ: Char; const RightQ: Char): string;
begin
  if not IsQuoted(Value) then
    Result := LeftQ + Value + RightQ
  else
    Result := Value;
end;

function TSQLInfo.UnQuote(const Value: string): string;
begin
  if IsQuoted(Value) then
    Result := Copy(Value, 2, length(Value) - 2)
  else
    Result := Value;
end;

function TSQLInfo.QuotesNeeded(const Value: string): boolean;
var
  i: integer;
  IdCase: TIdentCase;
begin
  Result := False;
  if Value = '' then
    Exit;

  IdCase := IdentCase;

  Result := FirstCharQuotesNeed(Value[1], IdCase);

  i := 2;
  while not Result and (i <= Length(Value)) do begin
    Result := NextCharQuotesNeed(Value[i], IdCase);
    Inc(i);
  end;
end;

function TSQLInfo.QuoteIfNeed(const Value: string): string;
begin
  if QuotesNeeded(Value) then
    Result := Quote(Value)
  else
    Result := Value;
end;

function TSQLInfo.NormalizeName(const Value: string; QuoteNames: boolean = False; UnQuoteNames: boolean = False): string;
begin
  Result := NormalizeName(Value, LeftQuote, RightQuote, QuoteNames, UnQuoteNames);
end;

function TSQLInfo.NormalizeName(const Value: string; const LeftQ: Char; const RightQ: Char; QuoteNames: boolean = False; UnQuoteNames: boolean = False): string;
var
  i: integer;
begin
  if Value = '' then begin
    Result := '';
    Exit;
  end;

  if Value[1] = LeftQ then begin
    i := Pos(RightQ + '.', Value);
    if i > 0 then
      Inc(i);
  end
  else begin
    i := Pos('.', Value);
  end;

  if i <> 0 then
    Result := NormalizeName(Copy(Value, 1, i - 1), LeftQ, RightQ, QuoteNames, UnQuoteNames) + '.' +
      NormalizeName(Copy(Value, i + 1, Length(Value) - i), LeftQ, RightQ, QuoteNames, UnQuoteNames)
  else begin
    Result := Value;
    if not QuoteNames then begin
      case IdentCase of
        icUpper:
          if not IsQuoted(Result) then
            Result := AnsiUpperCase(Result);
        icLower:
          if not IsQuoted(Result) then
            Result := AnsiLowerCase(Result);
      end;
    end;

    if not UnQuoteNames and (QuoteNames or QuotesNeeded(Result)) then
      Result := Quote(Result, LeftQ, RightQ)
    else begin
      Result := UnQuote(Result);
      if QuoteNames and QuotesNeeded(Result) then
        Result := Quote(Result, LeftQ, RightQ)
    end;
  end;
end;

function TSQLInfo.ToStringConst(const Value: string): string;
begin
  Result := '''' + StringReplace(Value, '''', '''''', [rfReplaceAll]) + '''';
end;

procedure TSQLInfo.SplitObjectName(const Name: string; out Info: TSQLObjectInfo);
var
  Str: string;
  i, FirstIdx, Len, ParamIdx: integer;
  InQuote: boolean;
  Lq, Rq: Char;
begin
  Info.Name := '';
  Info.Schema := '';
  Info.Catalog := '';
  Info.DBLink := '';

  Len := Length(Name);
  FirstIdx := Len;
  ParamIdx := 1;
  InQuote := False;
  Lq := LeftQuote;
  Rq := RightQuote;

  for i := Len downto 0 do begin
    if i > 0 then begin
      if not InQuote then begin
        if Name[i] = Rq then
          InQuote := True;
      end
      else
        if Name[i] = Lq then
          InQuote := False;
    end;

    if (i = 0) or (not InQuote and (Name[i] = '.')) then begin
      Str := Copy(Name, i + 1, FirstIdx - i);
      case ParamIdx of
        1: Info.Name := Str;
        2: Info.Schema := Str;
        3: Info.Catalog := Str;
      end;
      Inc(ParamIdx);
      FirstIdx := i - 1;
    end;
  end;
end;

procedure TSQLInfo.ParseTablesInfo(const SQL: string; TablesInfo: TCRTablesInfo);

  // find close bracket and skip all inside
  procedure ToEndBracket(Parser: TSQLParser; var CodeLexem: integer; var StLex: string);
  var
    BracketCount: integer;
  begin
    BracketCount := 1;
    repeat
      CodeLexem := Parser.GetNext(StLex);

      if CodeLexem = lxLeftBracket then
        Inc(BracketCount)
      else if CodeLexem = lxRightBracket then
        Dec(BracketCount);
    until (BracketCount = 0) or (CodeLexem = lcEnd);
  end;

  procedure ParseTableName(Parser: TSQLParser; var CodeLexem: integer; var StLex: string; InBrackets: boolean);
  var
    Name, Alias: string;
    TableInfo: TCRTableInfo;
  begin
    repeat
      // exit on WHERE and other clause lexems
      if not InBrackets and Parser.IsClauseLexem(CodeLexem) then
        exit
      // bypass end bracket
      else if CodeLexem = lxRightBracket then
        exit
      // bypass join
      else if CodeLexem in [lxJOIN, lxINNER, lxLEFT, lxRIGHT, lxFULL, lxOUTER] then
        CodeLexem := Parser.GetNext(StLex)
      // bypass comma
      else if CodeLexem = lxComma then
        CodeLexem := Parser.GetNext(StLex)
      //parse table name
      else begin
        // parse brackets recursive
        if CodeLexem = lxLeftBracket then begin
          CodeLexem := Parser.GetNext(StLex);

          // Oracle supports WITH lexem in subqueries
          if not (CodeLexem in [lxWITH, lxSELECT]) then
            ParseTableName(Parser, CodeLexem, StLex, True)
          else // skip subquery
            ToEndBracket(Parser, CodeLexem, StLex);

          if CodeLexem = lcEnd then
            Exit;

          // skip end bracket
          if CodeLexem = lxRightBracket then
            CodeLexem := Parser.GetNext(StLex);
        end
        else begin
          Name := '';

          // PostgreSQL can containt ONLY lexeme before table name, skip it
          if HasOnlyLexem then
            if CodeLexem = lxONLY then
              CodeLexem := Parser.GetNext(StLex);

          while (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) or (CodeLexem = lxPoint) do begin
              Name := Name + StLex;
              if CodeLexem <> lxPoint then begin
                CodeLexem := Parser.GetNext(StLex);
                if CodeLexem = lxPoint then
                  Name := Name + '.'
                else
                  break;
              end;
              CodeLexem := Parser.GetNext(StLex);
          end;

          if Name <> '' then begin
            if HasAsLexem then
              if CodeLexem = lxAS then
                CodeLexem := Parser.GetNext(StLex);

            // Oracle extended table info
            ParseExtTableInfo(Parser, CodeLexem, StLex, Name);

            if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) and
               (CodeLexem <> lxJOIN) and (CodeLexem <> lxINNER) and
               (CodeLexem <> lxLEFT) and (CodeLexem <> lxRIGHT) and
               (CodeLexem <> lxFULL) and (CodeLexem <> lxON) and
               not Parser.IsClauseLexem(CodeLexem)
            then begin
              Alias := StLex;
              CodeLexem := Parser.GetNext(StLex);
            end
            else
              Alias := '';

            if CodeLexem <> lxLeftBracket then begin // skip stored functions
              Name := NormalizeName(Name);
              Alias := NormalizeName(Alias);
              TableInfo := TablesInfo.FindByNameAndAlias(Name, Alias);
              if TableInfo = nil then begin
                TableInfo := TablesInfo.Add;
                TableInfo.TableName := Name;
                TableInfo.TableAlias := Alias;
              end;
            end;
          end;
        end;

        if CodeLexem = lcEnd then
          Exit;

        // bypass subqueries, function calls, ON clause of join
        repeat
          if CodeLexem = lxLeftBracket then begin
            ToEndBracket(Parser, CodeLexem, StLex);
            if CodeLexem <> lcEnd then
              CodeLexem := Parser.GetNext(StLex);
          end;

          if CodeLexem in [lxJOIN, lxINNER, lxLEFT, lxRIGHT, lxFULL] then begin
            CodeLexem := Parser.GetNext(StLex);
            break;
          end
          else if CodeLexem = lxComma then begin
            CodeLexem := Parser.GetNext(StLex);
            break;
          end
          else if CodeLexem = lxRightBracket then
            exit
          else if not InBrackets and Parser.IsClauseLexem(CodeLexem) then
            Exit;

          CodeLexem := Parser.GetNext(StLex);
        until (CodeLexem = lcEnd);
      end;
    until CodeLexem = lcEnd;
  end;

var
  Parser: TSQLParser;
  StLex: string;
  CodeLexem: integer;
begin
  Assert(FParserClass <> nil);
  Parser := FParserClass.Create(SQL);
  TablesInfo.BeginUpdate;
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  try
    CodeLexem := Parser.ToLexem([lxWITH, lxSELECT]);
    if CodeLexem = lxWITH then
      CodeLexem := Parser.ToLexem(lxSELECT, True);

    if CodeLexem <> lcEnd then begin
      CodeLexem := Parser.ToLexem(lxFROM, True);
      if CodeLexem <> lcEnd then begin
        CodeLexem := Parser.GetNext(StLex);
        ParseTableName(Parser, CodeLexem, StLex, False);
      end;
    end;
  finally
    TablesInfo.EndUpdate;
    Parser.Free;
  end;
end;

procedure TSQLInfo.ParseColumnsInfo(const SQL: string; ColumnsInfo: TCRColumnsInfo);
var
  Info: TCRColumnInfo;
  Parser: TSQLParser;
  Str: string;
  PrevCode, Code: integer;
  Name, Alias, Table, Expr: string;
  IsExpr: boolean;
  BracketCount: integer;
begin
  Assert(FParserClass <> nil);
  Parser := FParserClass.Create(SQL);
  Parser.QuotedString := True;
  try
    Parser.OmitComment := True;

    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code <> lxWITH then
      Parser.Back;

    if Parser.ToLexem(lxSELECT, True) = lxSELECT then begin
      BracketCount := 0;
      while True do begin
        Code := Parser.GetNext(Str);

        if Parser.IsSelectModifier(Code) then
          Continue;

        IsExpr := False;
        Name := '';
        Table := '';
        Alias := '';
        Expr := '';

        if (Code = lcIdent) or (Code = lxAsterisk) or ((Code >= lxSQLFirst) and (Code <> lxFROM)) then begin
          Name := Str;
          Code := Parser.GetNext(Str);
          if Code = lxPoint then begin
            Code := Parser.GetNext(Str);
            if (Code = lcIdent) or (Code >= lxSQLFirst) or (Code = lxAsterisk) then begin
              Table := Name;
              Name := Str;
              Code := Parser.GetNext(Str);
              if Code = lxPoint then begin
                Code := Parser.GetNext(Str);
                if (Code = lcIdent) or (Code >= lxSQLFirst) or (Code = lxAsterisk) then begin
                  Table := Table + '.' + Name;
                  Name := Str;
                  Code := Parser.GetNext(Str);
                  if Code = lxPoint then begin
                    Code := Parser.GetNext(Str);
                    if (Code = lcIdent) or (Code >= lxSQLFirst) or (Code = lxAsterisk) then begin
                      Table := Table + '.' + Name;
                      Name := Str;
                      Code := Parser.GetNext(Str);
                    end;
                  end;
                end;
              end;
            end;
          end;

          ParseExtColumnName(Parser, Code, Str);

          if Code = lxAS then
            Parser.GetNext(Str);

          if (Code = lcIdent) or (Code >= lxSQLFirst) and (Code <> lxFROM) then begin
            Alias := Str;
            Code := Parser.GetNext(Str);
          end;
        end;

        PrevCode := lcEnd;
        while Code <> lcEnd do begin
          if ((Code = lxComma) or (Code = lxFROM)) and (BracketCount = 0) then
            Break;

          if not IsExpr then begin
            if Table = '' then
              Expr := Name
            else
              Expr := Table + '.' + Name;
            Alias := '';
            IsExpr := True;
          end
          else
          if Alias <> '' then begin
            Expr := Expr + Alias;
            Alias := '';
          end;

          if Code = lxLeftBracket then
            Inc(BracketCount)
          else if Code = lxRightBracket then
            Dec(BracketCount);

          if BracketCount = 0 then begin
            if Code = lxAS then begin
              Code := Parser.GetNext(Str);
              continue;
            end;

            if ((Code = lcIdent) or (Code >= lxSQLFirst)) and
               ((PrevCode = lcIdent) or (PrevCode = lcNumber) or (PrevCode = lcString) or
                (PrevCode >= lxSQLFirst) or (PrevCode = lxRightBracket))
            then
              Alias := Str
            else
              Expr := Expr + Str;
          end;

          PrevCode := Code;
          Code := Parser.GetNext(Str);
        end;

        if not IsExpr and (Table = '') and Parser.IsFunctionOrConst(Name) then begin
          IsExpr := True;
          Expr := Name;
        end;

        Info := TCRColumnInfo.Create;
        if not IsExpr then begin
          Info.Name := NormalizeName(Name, False, True);
          Info.Table := NormalizeName(Table, False, True);
        end
        else
          Info.Expr := Expr;
        Info.TableIndex := -1;
        Info.Alias := NormalizeName(Alias, False, True);
        ColumnsInfo.Add(Info);

        if (Code = lcEnd) or (Code = lxFROM) then
          Break;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSQLInfo.ParseExtColumnName(Parser: TSQLParser; var Code: integer; var Str: string);
begin
end;

function TSQLInfo.FirstCharQuotesNeed(Ch: Char; IdCase: TIdentCase): boolean;
begin
  case IdCase of
    icUpper:
      case Ch of
        'A'..'Z': Result := False;
      else
        Result := True;
      end;
    icLower:
      case Ch of
        'a'..'z': Result := False;
      else
        Result := True;
      end;
  else
    case Ch of
      'a'..'z', 'A'..'Z': Result := False;
    else
      Result := True;
    end;
  end;
end;

function TSQLInfo.NextCharQuotesNeed(Ch: Char; IdCase: TIdentCase): boolean;
begin
  case IdCase of
    icUpper:
      case Ch of
        'A'..'Z', '_', '0'..'9', '$': Result := False;
      else
        Result := True;
      end;
    icLower:
      case Ch of
        'a'..'z', '_', '0'..'9', '$': Result := False;
      else
        Result := True;
      end;
  else
    case Ch of
      'a'..'z', 'A'..'Z', '_', '0'..'9', '$': Result := False;
    else
      Result := True;
    end;
  end;
end;

function TSQLInfo.HasAsLexem: boolean;
begin
  Result := True
end;

function TSQLInfo.HasOnlyLexem: boolean;
begin
  Result := False;
end;

procedure TSQLInfo.ParseExtTableInfo(Parser: TSQLParser; var CodeLexem: integer; var StLex: string; var Name: string);
begin
  // Dummy
end;

function TSQLInfo.NamesFromList(List: TStrings; NormalizedName: boolean = True; Delimiter: string = ';'): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to List.Count - 1 do begin
    if i > 0 then
      Result := Result + Delimiter;
    if NormalizedName then
      Result := Result + NormalizeName(List[i])
    else
      Result := Result + List[i];
  end;
end;

procedure TSQLInfo.NamesToList(Value: string; List: TStrings; NormalizedName: boolean = True);
var
  St: string;
  i: integer;
begin
  Value := Trim(Value);
  List.Clear;

  St := '';
  for i := 1 to Length(Value) do
    if (Value[i] = ',') or (Value[i] = ';') then begin
      if NormalizedName then
        St := NormalizeName(Trim(St), False, True)
      else
        St := Trim(St);
      if St <> '' then
        List.Add(St);
      St := '';
    end
    else
      St := St + Value[i];

  if NormalizedName then
    St := NormalizeName(Trim(St), False, True)
  else
    St := Trim(St);
  if St <> '' then
    List.Add(St);
end;

{ TCRFieldDesc }

constructor TCRFieldDesc.Create;
begin
  inherited;

{$IFNDEF LITE}
  FDBType := 0;
  FDBLength := 0;
  FDBScale := 0;
  FMapRule := nil;
  FMapLength := -1;
  FMapScale := -1;
  FMapHasValueLen := False;
  FOnDemandConverter := nil;
  FEncryptState := -1;
{$ENDIF}
end;

{$IFNDEF LITE}

function TCRFieldDesc.GetMapLength: Integer;
begin
  if FOnDemandConverter <> nil then
    Result := FMapLength
  else
    Result := Length;
end;

function TCRFieldDesc.GetMapDataType: Word;
begin
  if FOnDemandConverter <> nil then
    Result := FOnDemandConverter.DestDataType
  else
    Result := DataType;
end;
{$ENDIF}

procedure TCRFieldDesc.SetFieldReadOnly(SetFieldsReadOnly: boolean; UpdatingTableInfo: TCRTableInfo);
begin
  if FieldDescKind <> fdkData then
    Exit;

  if SetFieldsReadOnly then begin
    if (TableInfo = nil) or (TableInfo <> UpdatingTableInfo) then
      ReadOnly := True;
  end
  else begin
    ReadOnly := False;
    if (TableInfo = nil) or (TableInfo <> UpdatingTableInfo) then
      Required := False;
  end;
end;

function TCRFieldDesc.ActualNameQuoted(SQLInfo: TSQLInfo; QuoteNames: boolean): string;
begin
  if FActualNameQuoted[QuoteNames] <> '' then
    Result := FActualNameQuoted[QuoteNames]
  else
  begin
    if QuoteNames or SQLInfo.QuotesNeeded(ActualName) then
      Result := SQLInfo.Quote(ActualName)
    else
      Result := ActualName;
    FActualNameQuoted[QuoteNames] := Result;
  end;
end;

procedure TCRFieldDesc.Assign(FieldDesc: TFieldDesc);
begin
  inherited;

{$IFNDEF LITE}
  Assert(IsClass(FieldDesc, TCRFieldDesc));

  DBType := TCRFieldDesc(FieldDesc).DBType;
  DBLength := TCRFieldDesc(FieldDesc).DBLength;
  DBScale := TCRFieldDesc(FieldDesc).DBScale;
  MapRule := TCRFieldDesc(FieldDesc).MapRule;
  MapLength := TCRFieldDesc(FieldDesc).MapLength;
  MapScale := TCRFieldDesc(FieldDesc).MapScale;
  MapHasValueLen := TCRFieldDesc(FieldDesc).MapHasValueLen;
  OnDemandConverter := TCRFieldDesc(FieldDesc).OnDemandConverter;
{$ENDIF}
end;

{ $ENDIF}

{ TSmartFetchInfo }

constructor TSmartFetchInfo.Create(SQLGenerator: TSQLGenerator);
begin
  inherited Create;

  FSQLGenerator := SQLGenerator.CloneGenerator;
  FParamsInfo := TDAParamsInfo.Create(TDAParamInfo);
end;

destructor TSmartFetchInfo.Destroy;
begin
  FSQLGenerator.Free;
  FParamsInfo.Free;
  inherited;
end;

{ TCRRecordSet }

constructor TCRRecordSet.Create;
begin
  inherited;

  FAfterExecFetch := nil;
  FAfterFetchAll := nil;

  CreateCommand;
  FTablesInfo := TCRTablesInfo.Create(GetTableInfoClass);
{$IFNDEF LITE}
  FDataTypeMap := CreateDataTypeMap;

  FGetKeyValuesSQL := nil;
  FFirstFetchedItem := nil;
  FLiveBlockOnSmartFetch := True;
  FTmpFields := TFieldDescs.Create;
{$ENDIF}

  FLongStrings := True;
  FFlatBuffers := True;
end;

destructor TCRRecordSet.Destroy;
begin
  Close;
  FreeFetchBuffer;
  FreeCommand;

  FTablesInfo.Free;

  inherited;

{$IFNDEF LITE}
  FDataTypeMap.Free;
  FTmpFields.Free;
  FSmartFetchInfo.Free;
{$ENDIF}
end;

procedure TCRRecordSet.FreeData;
begin
{$IFNDEF LITE}
  if FSmartFetchState <> sfNone then begin
    FFirstFetchedItem := nil;
    SetLength(FItemRefCounts, 0);
    FLastItemRefInd := 0;

    if FSmartFetchBlock <> nil then begin
      BlockMan.FreeBlock(FSmartFetchBlock, True);
      FSmartFetchBlock := nil;
    end;
  end;
{$ENDIF}

  inherited;
end;

procedure TCRRecordSet.FreeCommand;
begin
  FCommand.Free;
  SetCommand(nil);
end;

procedure TCRRecordSet.SetCommand(Value: TCRCommand);
begin
  FCommand := Value;
end;

procedure TCRRecordSet.SetTrimFixedChar(Value: Boolean);
begin
  inherited;

  if FCommand <> nil then
    FCommand.FTrimFixedChar := Value;
end;

function TCRRecordSet.CanFetchBack: boolean;
begin
  Result := False;
end;

{ Open/Close }

{$IFNDEF LITE}

procedure TCRRecordSet.ClearDataByKeyParams;
var
  i: integer;
begin
  if (FSmartFetchInfo = nil) or not FSmartFetchInfo.FGeneratedSmartFetchByKeySQL then
    Exit;

  /// delete parameter, that was created for block data by GenerateSmartFetchDataByKeySQL
  /// and left parameters from original datataset
  i := 0;
  while i < FCommand.ParamsInfo.Count do begin
    if (i >= FSmartFetchInfo.FParamsInfo.Count) or (FCommand.ParamsInfo[i].ParamRef <> FSmartFetchInfo.FParamsInfo[i].ParamRef) then begin
      FCommand.Params.Remove(FCommand.ParamsInfo[i].ParamRef);
      FCommand.ParamsInfo.Delete(i);
      FSmartFetchInfo.FGeneratedSmartFetchByKeySQL := False;
    end
    else
      Inc(i);
  end;
end;

procedure TCRRecordSet.SetSmartFetchSQL(FirstFetchingItem: PItemHeader = nil; RowCount: integer = 0);
var
  PrefetchedFieldDescs: TFieldDescArray;
  SQL: string;
begin
  Assert(FSQLGenerator <> nil);

  FCommand.FScanParams := False;
  try
    if FSmartFetchInfo = nil then
      FSmartFetchInfo := TSmartFetchInfo.Create(FSQLGenerator);

    case FSmartFetchState of
      sfMetaInfo:
        FCommand.SQL := FSmartFetchInfo.FSQLGenerator.GenerateSmartFetchMetaInfoSQL;

      sfKeyOnly: begin
        if Assigned(FGetKeyValuesSQL) then
          SQL := FGetKeyValuesSQL
        else
          SQL := '';

        if SQL = '' then begin
          FillFieldDescs(PrefetchedFieldDescs, FPrefetchedFields, True);
          SQL := FSmartFetchInfo.FSQLGenerator.GenerateSmartFetchKeyOnlySQL(PrefetchedFieldDescs);
        end;
        FCommand.SQL := SQL;
      end;

      sfDataByKey: begin
        if not FDataFieldsWasSynchronized then begin
          GetKeyFieldDescs(FSmartFetchInfo.KeyFieldDescs);
          if Length(FSmartFetchInfo.KeyFieldDescs) = 0 then
            raise ESmartFetchError.Create(SNoKeyFields);

          FCommand.SQL := FOriginalSQL;
          FSmartFetchInfo.FParamsInfo.Assign(FCommand.ParamsInfo);
          FSmartFetchInfo.FSQLGenerator.PrepareSmartFetchDataByKeySQL(FCommand.SQL, FSmartFetchInfo);
        end
        else
          ClearDataByKeyParams;

        if (FirstFetchingItem = nil) or (RowCount = 0) then
          raise ESmartFetchError.Create(SNoDataToFetch);

        FCommand.FSQL := FSmartFetchInfo.FSQLGenerator.GenerateSmartFetchDataByKeySQL(FSmartFetchInfo, FCommand.ParamsInfo, FirstFetchingItem, RowCount);
        FSmartFetchInfo.FGeneratedSmartFetchByKeySQL := True;
      end;
    else
      Assert(False, SUnknownSmartFetchMode);
    end;

    if FCommand.SQL = '' then
      raise ESmartFetchError.Create(SOnlySelectAllowed);
  finally
    FCommand.FScanParams := True;
  end;
end;

{$ENDIF}

procedure TCRRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  try
    FWaitForFetchBreak := False;
    FNoCountData := False;

    inherited;

    FEOF := False;

  {$IFNDEF LITE}
    if FSmartFetchState <> sfNone then begin
      Assert(not Prepared);
      FOriginalSQL := FCommand.FUserSQL;
      try
        FOriginalParamsCount := FCommand.Params.Count;

        FSmartFetchState := sfMetaInfo;
        SetSmartFetchSQL;
        CheckFieldDescs;

        FSmartFetchState := sfKeyOnly;
        SetSmartFetchSQL;
        ExecFetch(False);

        FSmartFetchState := sfDataByKey;
      except
        FCommand.FScanParams := False;
        try
          FCommand.SQL := FOriginalSQL;
        finally
          FCommand.FScanParams := True;
        end;
        FOriginalSQL := '';
        raise;
      end;
    end
    else
  {$ENDIF}
      ExecFetch(DisableInitFields);

  except
    if not Prepared then
      InternalUnprepare;

    if FCommand.GetCursorState >= csExecuted then
      if not Prepared then
        FCommand.SetCursorState(csInactive)
      else
        FCommand.SetCursorState(csPrepared);

    raise;
  end
end;

procedure TCRRecordSet.InternalClose;
begin
  FNoCountData := False;
  FExtFieldsInfoInited := False;

  FreeFetchBuffer;

{$IFNDEF LITE}
  if (FSmartFetchState <> sfNone) and (FOriginalSQL <> '') then begin
    FSmartFetchState := sfMetaInfo;

    FCommand.FScanParams := False;
    try
      ClearDataByKeyParams;
      FCommand.SQL := FOriginalSQL;
      FOriginalSQL := '';
    finally
      FCommand.FScanParams := True;
    end;
  end;
{$ENDIF}
end;

function TCRRecordSet.IsFullReopen: boolean;
begin
{$IFNDEF LITE}
  Result := FSmartFetchState <> sfNone;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TCRRecordSet.Reopen;
begin
  if not IsFullReopen then begin
    if FCommand.GetCursorState > csPrepared then
      FCommand.SetCursorState(csPrepared);

    FreeData;
    InitData;
    if Assigned(FOnReopen) then
      // perform dataset resync to prevent AV if grid is repainted in BeforeFetch/AfterFetch events
      FOnReopen;

    InternalOpen(True);
    if Filtered then
      FilterUpdated;
  end
  else
    inherited;
end;

function TCRRecordSet.NeedInitFieldsOnPrepare: boolean;
begin
  Result := FCommand.CommandType = ctCursor;
end;

procedure TCRRecordSet.InternalPrepare;
begin
  FCommand.Prepare;
end;

procedure TCRRecordSet.InternalUnPrepare;
begin
  FCommand.UnPrepare;
end;

procedure TCRRecordSet.Prepare;
begin
  if not Prepared then begin
    inherited;

    if NeedInitFieldsOnPrepare then
      try
        InitFields;
        Prepared := True;
      except
        Prepared := False;
        InternalUnPrepare;
        raise;
      end;
  end;
end;

procedure TCRRecordSet.Unprepare;
begin
  try
    inherited;
  finally
    FTablesInfo.Clear;
  end;
end;

procedure TCRRecordSet.Disconnect;
begin
  InternalUnprepare; //Remove all links to DB but not close Data
  Prepared := False; //Set recordset unprepared in case of disconnect mode and
                     //explicit disconnection this will prevent from wrong Prepare state
end;

procedure TCRRecordSet.ExecCommand(Iters: integer = 1; Offset: integer = 0);
begin
  if (Iters = 1) and (Offset = 0) and ((FCommand.FParams.Count = 0) or (FCommand.FParams[0].GetArraySize <= 1)) then begin
    FCommand.SetProp(prBatchIters, 1);
    FCommand.SetProp(prBatchOffset, 0);
    FCommand.Execute;
  end
  else
    FCommand.ExecuteBatch(Iters, Offset);
  FWaitForFetchBreak := False;
end;

procedure TCRRecordSet.CloseCommand;
begin
  FCommand.Close;
  FCommand.SetCursorState(csInactive);
  FCommand.CommandType := ctUnknown;
end;

{$IFNDEF LITE}
procedure TCRRecordSet.SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
begin
  inherited;
  SetEncrypted(Field, RecBuf, not Value);
end;

function TCRRecordSet.GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
begin
  if not FInFetching then
    CheckFetched(RecBuf, Field);
  Result := inherited GetNull(Field, RecBuf);
end;

function TCRRecordSet.GetFieldObject(Field: TFieldDesc; RecBuf: IntPtr): TSharedObject;
begin
  if not FInFetching then
    CheckFetched(RecBuf, Field);
  Result := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
end;

procedure TCRRecordSet.FetchDataByKey(FirstFetchingItem: PItemHeader; RowCount: integer);
begin
  FIsFetchingDataByKey := True;
  if Assigned(OnConnectRequest) then // for Disconnected mode
    OnConnectRequest();
  try
    CloseCommand;

    if FSmartFetchBlock <> nil then
      FSmartFetchBlock.UsedItems := 0;
    SetSmartFetchSQL(FirstFetchingItem, RowCount);
    ExecFetch(False);
    SyncBlocks(FirstFetchingItem, RowCount);
  finally
    FIsFetchingDataByKey := False;
    if Assigned(OnDisconnectRequest) then // for Disconnected mode
      OnDisconnectRequest();
  end;
end;

procedure TCRRecordSet.CheckIfFetchedItem(var CheckedItem: PItemHeader; var AnyRecWasDeleted: boolean);
var
  Item, FirstRec, LastRec, NewItem: PItemHeader;
  i, RowCount: integer;
  RefCountInd: integer;
  CurRecWasDeleted: boolean;
  RecBuf: IntPtr;
  F: TFieldDesc;
begin
  if CheckedItem = nil then
    Exit;

  if GetFetchedStatus(PtrOffset(CheckedItem, SizeOf(TItemHeader))) = fsNotFetched then begin
    // freeing the last fetched data block
    if FLiveBlockOnSmartFetch then begin
      RowCount := 0;
      while (RowCount < FetchRows) and (FFirstFetchedItem <> nil) do begin
        RecBuf := PtrOffset(FFirstFetchedItem, SizeOf(TItemHeader));
        if GetFetchedStatus(RecBuf) = fsFetched then begin
          RefCountInd := GetRefCountInd(RecBuf);

          if RefCountInd > -1 then begin
            if FItemRefCounts[RefCountInd].Count = RefNull then begin
              if HasComplexFields then
                for i := 0 to Fields.Count - 1 do begin
                  F := FFields[i];
                  if F.IsComplex and not F.HasParent and (F.FieldDescKind <> fdkCalculated) and
                    not TCRFieldDesc(F).FIsImplicityPrefetched
                  then
                    FreeComplexField(RecBuf, F);
                end;

              DisposeRefCountInd(RefCountInd);
              WriteRefCountInd(RecBuf, -1);
            end
            else
              FItemRefCounts[RefCountInd].Count := FItemRefCounts[RefCountInd].Count - 1;

            if FItemRefCounts[RefCountInd].Count > 0 then
              SetFetchedStatus(RecBuf, fsFree)
            else
              SetFetchedStatus(RecBuf, fsNotFetched);
          end
          else
            SetFetchedStatus(RecBuf, fsNotFetched);
        end
        else
          Break;

        FFirstFetchedItem := FFirstFetchedItem.Next;
        Inc(RowCount);
      end;
    end;

    // searching for the first row to fetch
    RowCount := 1;
    LastRec := CheckedItem;
    while (RowCount < FetchRows) and (LastRec.Next <> nil) do begin
      if GetFetchedStatus(PtrOffset(LastRec.Next, SizeOf(TItemHeader))) <> fsNotFetched then
        Break;

      LastRec := LastRec.Next;
      Inc(RowCount);
    end;
    FirstRec := CheckedItem;
    while (RowCount < FetchRows) and (FirstRec.Prev <> nil) do begin
      if GetFetchedStatus(PtrOffset(FirstRec.Prev, SizeOf(TItemHeader))) <> fsNotFetched then
        Break;

      FirstRec := FirstRec.Prev;
      Inc(RowCount);
    end;

    // fetching block
    FFirstFetchedItem := FirstRec;
    FetchDataByKey(FirstRec, RowCount);

    // deleting records, that are deleted by another user
    CurRecWasDeleted := False;
    Item := FFirstFetchedItem;
    for i := 1 to RowCount do begin
      if Item = nil then
        Break;

      if (Item.Status = isDeleted) and (Item.UpdateResult = urApplied) then begin
        if Item.Next = nil then
          NewItem := Item.Prev
        else
          NewItem := Item.Next;

        if Item = CheckedItem then begin
          CheckedItem := NewItem;
          CurRecWasDeleted := True;
        end;

        DeleteItem(Item);
        Dec(FRecordCount);
        ReorderItems(NewItem, roDelete);
        AnyRecWasDeleted := True;
        Item := NewItem;
      end
      else
        Item := Item.Next;
    end;

    if CurRecWasDeleted then
      CheckIfFetchedItem(CheckedItem, AnyRecWasDeleted);
  end;
end;

function TCRRecordSet.IsFieldPrefetched(Field: TFieldDesc): boolean;
begin
  Result := (Field <> nil) and
            ((Field.FieldDescKind <> fdkData) or
            TCRFieldDesc(Field).IsPrefetched or
            (TCRFieldDesc(Field).FIsImplicityPrefetched and FNotFetchPrefetchedFields));
end;

procedure TCRRecordSet.CheckFetched(RecBuf: IntPtr; Field: TFieldDesc);
var
  CheckedItem, OldCheckedItem: PItemHeader;
  AnyRecWasDeleted: boolean;
  RefCountInd: integer;
begin
  if (FSmartFetchState = sfDataByKey) and not FIsFetchingDataByKey then begin
    if not IsFieldPrefetched(Field) and (GetFetchedStatus(RecBuf) = fsNotFetched) then begin
      CheckedItem := GetItemFromRecBuf(RecBuf);
      if CheckedItem = nil then
        Exit;

      AnyRecWasDeleted := False;
      OldCheckedItem := CheckedItem;
      CheckIfFetchedItem(CheckedItem, AnyRecWasDeleted);
      if CheckedItem = nil then
        raise ESmartFetchError.Create(SRecordNotFound);

      if PtrCompare(RecBuf, PtrOffset(CheckedItem, SizeOf(TItemHeader))) <> 0 then begin
        // copying the fetched record
        RefCountInd := GetRefCountInd(RecBuf);
        if RefCountInd > -1 then begin
          Assert(FItemRefCounts[RefCountInd].Count >= RefNull);

          if FItemRefCounts[RefCountInd].Count = RefNull then begin
            FreeComplexFields(RecBuf, True);
            DisposeRefCountInd(RefCountInd);
          end
          else
            FItemRefCounts[RefCountInd].Count := FItemRefCounts[RefCountInd].Count - 1;

          AddRef(PtrOffset(CheckedItem, SizeOf(TItemHeader)));
        end;

        BlockMan.GetRecord(CheckedItem, RecBuf);
        WriteItemPtrIntoRecBuf(RecBuf, CheckedItem);
      end;

      if AnyRecWasDeleted and Assigned(FOnDataChanged) then
        FOnDataChanged;

      if OldCheckedItem <> CheckedItem then
        if FPrefetchedFields <> '' then
          raise ESmartFetchError.Create(SRecordNotFound);
    end;
  end;
end;

{$ENDIF}

function TCRRecordSet.GetMappedDataBuf(FieldDesc: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; var DataType: Word; var HasParent, IsFixed: boolean): IntPtr;
{$IFNDEF LITE}
var
  MappedBufSize: Integer;
  TmpBuf: IntPtr;
{$ENDIF}
begin
{$IFNDEF LITE}
  if TCRFieldDesc(FieldDesc).OnDemandConverter <> nil then begin
    DataType := FieldDesc.MapDataType;

    MappedBufSize := TCRFieldDesc(FieldDesc).MapLength;
    Result := Marshal.AllocHGlobal(GetBufferSize(DataType, MappedBufSize));
    try
      TmpBuf := GetBufForDataMappingConverter(Result, DataType, FieldDesc.HasParent, False);
      try
        GetFieldData(FieldDesc, DataBuf, DataLen, TmpBuf, True);
        PutBufAfterDataMappingConverter(TmpBuf, Result, DataType, HasParent);
      finally
        if TmpBuf <> Result then
          Marshal.FreeHGlobal(TmpBuf);
      end;
    except
      Marshal.FreeHGlobal(Result);
      raise;
    end;

    HasParent := False;
    IsFixed := False;
  end
  else
{$ENDIF}
    Result := DataBuf;
end;

function TCRRecordSet.GetBufForDataMappingConverter(Source: IntPtr; DataType: Word; HasParent: boolean; NeedConvert: boolean): IntPtr;
begin
  case DataType of
    dtDateTime: begin
      Result := Marshal.AllocHGlobal(8);
      if NeedConvert then
        GetDateFromBuf(Source, Result, HasParent, dfMSecs);
    end;
    dtDate: begin
      Result := Marshal.AllocHGlobal(4);
      if NeedConvert then
        GetDateFromBuf(Source, Result, HasParent, dfDate);
    end;
    dtTime: begin
      Result := Marshal.AllocHGlobal(4);
      if NeedConvert then
        GetDateFromBuf(Source, Result, HasParent, dfTime);
    end;
    else
      Result := Source;
  end;
end;

procedure TCRRecordSet.PutBufAfterDataMappingConverter(Source, Dest: IntPtr; DataType: Word; HasParent: boolean);
begin
  case DataType of
    dtDateTime:
      PutDateToBuf(Dest, Source, HasParent, dfMSecs);
    dtDate:
      PutDateToBuf(Dest, Source, HasParent, dfDate);
    dtTime:
      PutDateToBuf(Dest, Source, HasParent, dfTime);
  end;
end;

procedure TCRRecordSet.GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean);
{$IFNDEF LITE}
var
  Buf: IntPtr;
  Status: TConvertStatus;
  Value: Variant;
  Str: string;
{$ENDIF}
begin
{$IFNDEF LITE}
  if NeedConvert and (TCRFieldDesc(Field).OnDemandConverter <> nil) then begin
    Buf := GetBufForDataMappingConverter(DataBuf, Field.DataType, Field.HasParent, True);

    try
      FConvertInfo.StringHeap := FStringHeap;
      FConvertInfo.Source := Buf;
      FConvertInfo.SourceOffset := 0;
      if Field.HasValueLen then
        FConvertInfo.SourceLen := DataLen
      else
        FConvertInfo.SourceLen := Field.Length;
      FConvertInfo.SourceScale := Field.Scale;
      FConvertInfo.Dest := Dest;
      FConvertInfo.DestOffset := 0;
      FConvertInfo.DestLen := TCRFieldDesc(Field).MapLength;
      FConvertInfo.DestScale := TCRFieldDesc(Field).MapScale;
      FConvertInfo.IgnoreConvertErrors := TCRFieldDesc(Field).MapRule.IgnoreErrors;
      FConvertInfo.Format := TCRFieldDesc(Field).MapRule.Format;

      Status := TCRFieldDesc(Field).OnDemandConverter.GetDataConverter(FConvertInfo);

      if (Status <> csSuccess) and not TCRFieldDesc(Field).MapRule.IgnoreErrors then begin
        Datalen := 0;
        if Field.DataType = dtSQLTimeStamp then begin
          CRTimeStamp.DateTimeToString(Str, FConvertInfo.Format, PSQLTimeStamp(DataBuf)^, FormatSettings);
          Value := Str;
        end
        else
          GetDataAsVariant(DataBuf, DataSize, Field.DataType, Field.SubDataType, Field.HasParent, Field.Fixed, Value, False);
        raise EDataMappingError.CreateFmt('Field: ' + Field.Name +  #13#10 + ConvertStatusErrors[Status], [VarToStr(Value)]);
      end
      else if TCRFieldDesc(Field).MapHasValueLen then
        DataLen := FConvertInfo.DestLen
      else
        Datalen := 0;
    finally
      if Buf <> DataBuf then
        Marshal.FreeHGlobal(Buf);
    end;
  end
  else
{$ENDIF}
    inherited;
end;

procedure TCRRecordSet.PutFieldData(Field: TFieldDesc; DataBuf: IntPtr; DataLenPtr: PWord; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False);
{$IFNDEF LITE}
var
  Buf: IntPtr;
  Status: TConvertStatus;
  Value: Variant;
  Str: string;
{$ENDIF}
begin
{$IFNDEF LITE}
  if NeedConvert and (TCRFieldDesc(Field).OnDemandConverter <> nil) then begin
    Buf := GetBufForDataMappingConverter(DataBuf, Field.DataType, Field.HasParent, False);

    try
      FConvertInfo.StringHeap := FStringHeap;
      FConvertInfo.Source := ValuePtr;
      FConvertInfo.SourceOffset := 0;
      if TCRFieldDesc(Field).MapHasValueLen then
        FConvertInfo.SourceLen := ValueLen
      else
        FConvertInfo.SourceLen := TCRFieldDesc(Field).MapLength;
      FConvertInfo.SourceScale := TCRFieldDesc(Field).MapScale;
      FConvertInfo.Dest := Buf;
      FConvertInfo.DestOffset := 0;
      FConvertInfo.DestLen := Field.Length;
      FConvertInfo.DestScale := Field.Scale;
      FConvertInfo.IgnoreConvertErrors := TCRFieldDesc(Field).MapRule.IgnoreErrors;
      FConvertInfo.Format := TCRFieldDesc(Field).MapRule.Format;

      Status := TCRFieldDesc(Field).OnDemandConverter.PutDataConverter(FConvertInfo);

      if (Status <> csSuccess) and not TCRFieldDesc(Field).MapRule.IgnoreErrors then begin
        if Field.HasValueLen then
          DataLenPtr^ := 0;
        if Field.DataType = dtSQLTimeStamp then begin
          CRTimeStamp.DateTimeToString(Str, FConvertInfo.Format, PSQLTimeStamp(DataBuf)^, FormatSettings);
          Value := Str;
        end
        else
          GetDataAsVariant(ValuePtr, ValueLen, Field.DataType, Field.SubDataType, Field.HasParent, Field.Fixed, Value, False);
        raise EDataMappingError.CreateFmt('Field: ' + Field.Name +  #13#10 + ConvertStatusErrors[Status], [VarToStr(Value)]);
      end
      else if Field.HasValueLen then
        DataLenPtr^ := FConvertInfo.DestLen;

      PutBufAfterDataMappingConverter(Buf, DataBuf, Field.DataType, Field.HasParent);
    finally
      if Buf <> DataBuf then
        Marshal.FreeHGlobal(Buf);
    end;
  end
  else
{$ENDIF}
    inherited;
end;

{$IFNDEF LITE}
procedure TCRRecordSet.SyncKeyFields(KeyFields, OriginFields: TFieldDescs);
var
  KeyField, OriginField: TCRFieldDesc;
  i: integer;
begin
  if KeyFields.Count = 0 then
    raise ESmartFetchError.Create(SNoKeyFields);

  for i := 0 to OriginFields.Count - 1 do begin
    OriginField := TCRFieldDesc(OriginFields[i]);
    OriginField.IsPrefetched := False;
    OriginField.FIsImplicityPrefetched := False;
    if OriginField.FieldDescKind <> fdkCalculated then
      OriginField.ActualFieldNo := -1;
  end;

  for i := 0 to KeyFields.Count - 1 do begin
    KeyField := TCRFieldDesc(KeyFields[i]);
    OriginField := TCRFieldDesc(OriginFields.FindField(KeyField.Name));
    if (KeyField.TableInfo <> nil) and KeyField.Hidden and ((OriginField = nil) or not SameText(OriginField.TableInfo.TableName, KeyField.TableInfo.TableName)) then
      Continue;

    if OriginField <> nil then begin
      if (OriginField.DBType <> KeyField.DBType) or
         (OriginField.FDBLength <> KeyField.FDBLength) or
         (OriginField.FDBScale <> KeyField.FDBScale) or
         (OriginField.DataType <> KeyField.DataType) or
         (IsCaseSensitive and (OriginField.ActualName <> KeyField.ActualName)) or
         (not IsCaseSensitive and not SameText(OriginField.ActualName, KeyField.ActualName))
      then
        raise ESmartFetchError.Create(SFieldsNotCorresponding);

      if FPrefetchedFields <> '' then
        OriginField.IsPrefetched := True;
      OriginField.FIsImplicityPrefetched := True;
      OriginField.ActualFieldNo := KeyField.ActualFieldNo;
    end
    else
      raise ESmartFetchError.Create(SNoKeyFields);
  end;
end;

procedure TCRRecordSet.SyncDataFields(DataFields, OriginFields: TFieldDescs);
var
  DataField, OriginField: TCRFieldDesc;
  i: integer;
begin
  if DataFields.Count > OriginFields.Count then
    raise ESmartFetchError.Create(SFieldsNotCorresponding);

  for i := 0 to DataFields.Count - 1 do begin
    DataField := TCRFieldDesc(DataFields[i]);
    OriginField := TCRFieldDesc(OriginFields[i]);

    if (OriginField.DBType <> DataField.DBType) or
       (OriginField.FDBLength <> DataField.FDBLength) or
       (OriginField.FDBScale <> DataField.FDBScale) or
       (OriginField.DataType <> DataField.DataType) or
       (IsCaseSensitive and (OriginField.ActualName <> DataField.ActualName)) or
       (not IsCaseSensitive and not SameText(OriginField.ActualName, DataField.ActualName))
    then
      raise ESmartFetchError.Create(SFieldsNotCorresponding);

    OriginField.ActualFieldNo := DataField.ActualFieldNo;
  end;

  for i := DataFields.Count to OriginFields.Count - 1 do
    if OriginFields[i].FieldDescKind = fdkData then
      raise ESmartFetchError.Create(SFieldsNotCorresponding);
end;
{$ENDIF}

procedure TCRRecordSet.ClearCachedKeyFieldDescs;
begin
  SetLength(FCachedKeyFieldDescs[False], 0);
  SetLength(FCachedKeyFieldDescs[True], 0);
  FKeyFieldDescsIsCached[False] := False;
  FKeyFieldDescsIsCached[True] := False;
end;

procedure TCRRecordSet.ClearCachedFieldDescs;
begin
  FExtFieldsInfoInited := False;

  ClearCachedKeyFieldDescs;

  SetLength(FCachedDataFieldDescs[False], 0);
  SetLength(FCachedDataFieldDescs[True], 0);
  FDataFieldDescsIsCached[False] := False;
  FDataFieldDescsIsCached[True] := False;

  FUpdatingTableInfoIdx := -1;
  FIdentityField := nil;
  FIdentityIsPartOfComplexPK := False;
end;

procedure TCRRecordSet.InitUpdatingTableIdx;
var
  NormalizedName: string;
  TableInfo: TCRTableInfo;
  i: integer;
begin
  NormalizedName := FCommand.SQLInfo.NormalizeName(FUpdatingTable);

  FUpdTableIsArtificial := False;
  if FTablesInfo.Count = 0 then begin
    if NormalizedName <> '' then begin
      TableInfo := FTablesInfo.Add;
      TableInfo.TableName := NormalizedName;
      FUpdatingTableInfoIdx := 0;
      FUpdTableIsArtificial := True;
    end
    else
      FUpdatingTableInfoIdx := -1;
  end
  else
  if NormalizedName = '' then // Select default updating table
    FUpdatingTableInfoIdx := 0
  else begin
    i := FTablesInfo.IndexByName(NormalizedName);
    if i = -1 then
      i := FTablesInfo.IndexByAlias(NormalizedName);
    if i = - 1 then
      i := FindTableInfoBySimpleName(NormalizedName);

    if i = - 1 then
      raise Exception.CreateFmt(SBadUpdatingTable, [NormalizedName]);

    FUpdatingTableInfoIdx := i;
  end;
end;

function TCRRecordSet.FindTableInfoBySimpleName(const Name: string): integer;
begin
  Result := -1;
end;

procedure TCRRecordSet.GetKeyFieldDescs(out KeyFieldDescs: TFieldDescArray;
  ForceUseAllFields: boolean = False);
var
  UseAllKeyFields: boolean;
begin
// ForceUseAllFields parameter is used to generate condition for full refresh
// When ForceUseAllFields is True GetKeyFieldsDesc should return KeyFields from
// all tables (not only updating). If there is no possibility do this (ODAC, IBDAC)
// CanUseAllKeyFields function should return False

  UseAllKeyFields := ForceUseAllFields and CanUseAllKeyFields;
  if FKeyFieldDescsIsCached[UseAllKeyFields] then
    KeyFieldDescs := FCachedKeyFieldDescs[UseAllKeyFields]
  else begin
    if FKeyFields <> '' then
      FillFieldDescs(KeyFieldDescs, FKeyFields, True)
    else
    if (FIdentityField <> nil) and not FIdentityIsPartOfComplexPK then begin
      SetLength(KeyFieldDescs, 1);
      KeyFieldDescs[0] := FIdentityField;
    end
    else
      FillKeyFieldDescs(KeyFieldDescs, UseAllKeyFields);

    FCachedKeyFieldDescs[UseAllKeyFields] := KeyFieldDescs;
    FKeyFieldDescsIsCached[UseAllKeyFields] := True;
  end;
end;

procedure TCRRecordSet.GetDataFieldDescs(out DataFieldDescs: TFieldDescArray;
  ForceUseAllFields: boolean = False);
begin
  if FDataFieldDescsIsCached[ForceUseAllFields] then
    DataFieldDescs := FCachedDataFieldDescs[ForceUseAllFields]
  else begin
    FillDataFieldDescs(DataFieldDescs, ForceUseAllFields);
    FCachedDataFieldDescs[ForceUseAllFields] := DataFieldDescs;
    FDataFieldDescsIsCached[ForceUseAllFields] := True;
  end;
end;

procedure TCRRecordSet.GetKeyAndDataFields(out KeyAndDataFields: TKeyAndDataFields;
  ForceUseAllFields: boolean);
begin
  GetKeyFieldDescs(KeyAndDataFields.KeyFieldDescs, ForceUseAllFields);
  GetDataFieldDescs(KeyAndDataFields.DataFieldDescs, ForceUseAllFields);
end;

function TCRRecordSet.CanUseAllKeyFields: boolean;
begin
  Result := False;
end;

function TCRRecordSet.IdentityFieldIsData: boolean;
begin
  Result := False;
end;

procedure TCRRecordSet.FillFieldDescs(out FieldDescs: TFieldDescArray;
  const FieldNames: string; CheckFields: boolean);
var
  Pos: integer;
  FieldName: string;
  FieldDesc: TCRFieldDesc;
begin
  SetLength(FieldDescs, 0);

  Pos := 1;
  while Pos <= Length(FieldNames) do begin
    FieldName := ExtractFieldName(FieldNames, Pos);
    FieldName := FCommand.SQLInfo.NormalizeName(FieldName, False, True);
    if FieldName <> '' then begin
      FieldDesc := TCRFieldDesc(FindField(FieldName));
      if FieldDesc <> nil then begin
        SetLength(FieldDescs, Length(FieldDescs) + 1);
        FieldDescs[High(FieldDescs)] := FieldDesc;
      end
      else
        if CheckFields then
          raise Exception.CreateFmt(SFieldNotFound, [FieldName]);
    end;
  end;
end;

procedure TCRRecordSet.FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray;
  ForceUseAllKeyFields: boolean);
begin
  if UpdatingTableInfo <> nil then begin // Read key fields info from server
    Assert(Assigned(GetDBKeyList));
    FillFieldDescs(KeyFieldDescs, GetDBKeyList(UpdatingTableInfo.TableName, ''), False);
  end
  else
    SetLength(KeyFieldDescs, 0);
end;

procedure TCRRecordSet.FillDataFieldDescs(out DataFieldDescs: TFieldDescArray;
  ForceUseAllKeyFields: boolean);

  function CheckField(FieldDesc: TCRFieldDesc): boolean;
  begin
    Result := False;

    if FieldDesc.HasParent or (FieldDesc.FieldDescKind <> fdkData) then
      Exit;

    if not ForceUseAllKeyFields and
      ((FieldDesc.TableInfo = nil) or (FieldDesc.TableInfo <> UpdatingTableInfo))
    then
      Exit;

    if FieldDesc <> FIdentityField then
      Result := not FieldDesc.ReadOnly
    else if IdentityFieldIsData then
      Result := not FieldDesc.ReadOnly
  end;

  procedure AddFieldDesc(FieldDesc: TCRFieldDesc);
  begin
    SetLength(DataFieldDescs, Length(DataFieldDescs) + 1);
    DataFieldDescs[High(DataFieldDescs)] := FieldDesc;
  end;

var
  i: integer;
  FieldDesc, RootFieldDesc: TCRFieldDesc;
begin
  SetLength(DataFieldDescs, 0);

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    // add standard fields if they is found in the dataset
    if FieldDesc.ParentField = nil then begin
      if (FieldDesc.Updateable or (FieldDesc.DataType = dtTable)) and CheckField(FieldDesc) then
        AddFieldDesc(FieldDesc);
    end
    // add object fields
    else begin
      // find root field
      RootFieldDesc := TCRFieldDesc(FieldDesc.ParentField);
      while RootFieldDesc.ParentField <> nil do
        RootFieldDesc := TCRFieldDesc(RootFieldDesc.ParentField);

      // check if root field was added (it is always last)
      if (High(DataFieldDescs) <> -1) and
         (DataFieldDescs[High(DataFieldDescs)] = RootFieldDesc)
      then
        continue;

      // if root has not been added and child field is found in the dataset
      // then add root field to the field list
      if FieldDesc.Updateable and CheckField(RootFieldDesc) then
        AddFieldDesc(RootFieldDesc);
    end;
  end;
end;

{$IFNDEF LITE}
procedure TCRRecordSet.RequestFieldsInfo(Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo);
begin

end;
{$ENDIF}

function TCRRecordSet.GetUpdatingTableInfo: TCRTableInfo;
begin
  if (FTablesInfo.Count > 0) and (FUpdatingTableInfoIdx > -1) then
    Result := FTablesInfo[FUpdatingTableInfoIdx]
  else
    Result := nil;
end;

procedure TCRRecordSet.DetectIdentityField;
var
  UpdTableInfo: TCRTableInfo;
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  FIdentityField := nil;
  FIdentityIsPartOfComplexPK := False;
  UpdTableInfo := UpdatingTableInfo;
  if UpdTableInfo = nil then
    Exit;

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if (FieldDesc.FieldDescKind = fdkData) and FieldDesc.IsAutoIncrement and
      (FieldDesc.TableInfo = UpdTableInfo) then begin
      FIdentityField := FieldDesc;
      Exit;
    end;
  end;
end;

procedure TCRRecordSet.DetectKeyGeneratorField;
var
  FieldName: string;
  Pos: integer;
begin
  if FKeyFields <> '' then begin
    Pos := 1;
    FieldName := ExtractFieldName(FKeyFields, Pos);
    FieldName := FCommand.SQLInfo.NormalizeName(FieldName, False, True);
    FKeyGeneratorField := TCRFieldDesc(FindField(FieldName));
  end
  else
    ClearKeyGeneratorField;
end;

procedure TCRRecordSet.ClearKeyGeneratorField;
begin
  FKeyGeneratorField := nil;
end;

procedure TCRRecordSet.SetFieldsReadOnly;
var
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    FieldDesc.SetFieldReadOnly(FSetFieldsReadOnly, UpdatingTableInfo);
  end;
end;

procedure TCRRecordSet.SetIdentityField(Value: TCRFieldDesc);
begin
  FIdentityField := Value;
end;

{$IFNDEF LITE}
procedure TCRRecordSet.InitItem(Item: PItemHeader);
var
  RecBuf: IntPtr;
begin
  inherited;
  RecBuf := PtrOffset(Item, SizeOf(TItemHeader));
  SetFetchedStatus(RecBuf, fsNotFetched);
  WriteRefCountInd(RecBuf, -1);
  WriteItemPtrIntoRecBuf(RecBuf, Item);
end;

procedure TCRRecordSet.DeleteItem(Item: PItemHeader);
begin
  if (Item = FFirstFetchedItem) and (FFirstFetchedItem <> nil) then
    FFirstFetchedItem := FFirstFetchedItem.Next;

  inherited;
end;

procedure TCRRecordSet.InitRecord(RecBuf: IntPtr);
begin
  inherited;
  ClearChangedIndicators(RecBuf);
  SetFetchedStatus(RecBuf, fsFetched);
  WriteRefCountInd(RecBuf, -1);
end;

procedure TCRRecordSet.PutRecord(RecBuf: IntPtr);
begin
  WriteItemPtrIntoRecBuf(RecBuf, CurrentItem);
  inherited;
end;

function TCRRecordSet.GetFetchStateSize: Integer;
begin
  if FSmartFetchState = sfNone then
    Result := 0
  else
    Result := SizeOf(byte);
end;

function TCRRecordSet.GetRefCountSize: Integer;
begin
  if FSmartFetchState = sfNone then
    Result := 0
  else
    Result := SizeOf(Integer);
end;

function TCRRecordSet.GetItemPtrSize: Integer;
begin
  if FSmartFetchState = sfNone then
    Result := 0
  else
    Result := SizeOf(IntPtr);
end;

function TCRRecordSet.GetFetchedStatus(RecBuf: IntPtr): TFetchedStatus;
begin
  if FSmartFetchState <> sfNone then
    Result := TFetchedStatus(PByte(PtrOffset(RecBuf, FFetchStateOffset))^)
  else
    Result := fsFetched;
end;

procedure TCRRecordSet.SetFetchedStatus(RecBuf: IntPtr; Value: TFetchedStatus);
begin
  if FSmartFetchState <> sfNone then
    PByte(PtrOffset(RecBuf, FFetchStateOffset))^ := byte(Value);
end;

function TCRRecordSet.GetRefCountInd(RecBuf: IntPtr): integer;
begin
  if FSmartFetchState <> sfNone then
    Result := Marshal.ReadInt32(RecBuf, FRefCountOffset)
  else
    Result := -1;
end;

procedure TCRRecordSet.WriteRefCountInd(RecBuf: IntPtr; RefInd: integer);
begin
  if FSmartFetchState <> sfNone then
    Marshal.WriteInt32(RecBuf, FRefCountOffset, RefInd);
end;

function TCRRecordSet.GetNewRefCountInd: integer;
begin
  while True do begin
    if FLastItemRefInd >= Length(FItemRefCounts) then
      SetLength(FItemRefCounts, Length(FItemRefCounts) + 64);
    if not FItemRefCounts[FLastItemRefInd].Used then
      Break;
    Inc(FLastItemRefInd);
  end;

  FItemRefCounts[FLastItemRefInd].Used := True;
  Result := FLastItemRefInd;
  Inc(FLastItemRefInd);
end;

procedure TCRRecordSet.DisposeRefCountInd(RefInd: integer);
begin
  FItemRefCounts[RefInd].Used := False;
  FItemRefCounts[RefInd].Count := 0;
  FLastItemRefInd := RefInd;
end;

function TCRRecordSet.GetItemFromRecBuf(RecBuf: IntPtr): PItemHeader;
begin
  if (FSmartFetchState <> sfNone) and (RecBuf <> nil) then
    Result := Marshal.ReadIntPtr(RecBuf, FItemPtrOffset)
  else
    Result := nil;
end;

procedure TCRRecordSet.WriteItemPtrIntoRecBuf(RecBuf: IntPtr; Item: PItemHeader);
begin
  if FSmartFetchState <> sfNone then
    Marshal.WriteIntPtr(RecBuf, FItemPtrOffset, Item);
end;

procedure TCRRecordSet.AddRef(RecBuf: IntPtr);
var
  RefCountInd: integer;
begin
  if FSmartFetchState = sfDataByKey then begin
    RefCountInd := GetRefCountInd(RecBuf);
    if RefCountInd = -1 then begin
      RefCountInd := GetNewRefCountInd;
      WriteRefCountInd(RecBuf, RefCountInd);
      AddRefComplexFields(RecBuf);
      FItemRefCounts[RefCountInd].Count := RefNull;
    end
    else
      FItemRefCounts[RefCountInd].Count := FItemRefCounts[RefCountInd].Count + 1;
  end
  else
    AddRefComplexFields(RecBuf, True);
end;

procedure TCRRecordSet.ReleaseRef(RecBuf: IntPtr; IsResync: boolean; WithBlob: boolean);

  procedure FreeAndCopyComplexFields(ItemRecBuf, RecBuf: IntPtr);
  var
    Field: TFieldDesc;
    DataBuf: IntPtr;
    i: integer;
  begin
    if HasComplexFields then
      for i := 0 to FFields.Count - 1 do begin
        Field := FFields[i];
        if Field.IsComplex and
          (not Field.IsBlob or WithBlob) and
          (not Field.HasParent) and
          (Field.FieldDescKind <> fdkCalculated)
        then begin
          FreeComplexField(ItemRecBuf, Field);
          DataBuf := Marshal.ReadIntPtr(ItemRecBuf, Field.DataOffset);
          Marshal.WriteIntPtr(RecBuf, Field.DataOffset, DataBuf);
          if Field.IsBlob and not TCRFieldDesc(Field).FIsImplicityPrefetched then
            Marshal.WriteIntPtr(ItemRecBuf, Field.DataOffset, nil);
        end;
      end;
  end;

var
  Item: PItemHeader;
  ItemRecBuf: IntPtr;
  RefCountInd: integer;
begin
  if FSmartFetchState = sfDataByKey then begin
    Item := GetItemFromRecBuf(RecBuf);
    Assert(Item <> nil);
    ItemRecBuf := PtrOffset(Item, SizeOf(TItemHeader));

    RefCountInd := GetRefCountInd(RecBuf);
    Assert(RefCountInd > -1);
    Assert(FItemRefCounts[RefCountInd].Count >= RefNull);

    if RefCountInd <> GetRefCountInd(ItemRecBuf) then begin
      if FItemRefCounts[RefCountInd].Count = RefNull then begin
        FreeComplexFields(RecBuf, WithBlob);
        DisposeRefCountInd(RefCountInd);
        WriteRefCountInd(RecBuf, -1);
      end
      else
        FItemRefCounts[RefCountInd].Count := FItemRefCounts[RefCountInd].Count - 1;
    end
    else begin
      if (FItemRefCounts[RefCountInd].Count = RefNull) and not IsResync then begin
        FreeAndCopyComplexFields(ItemRecBuf, RecBuf);
        DisposeRefCountInd(RefCountInd);
        WriteRefCountInd(RecBuf, -1);
        WriteRefCountInd(ItemRecBuf, -1);

        if GetFetchedStatus(ItemRecBuf) = fsFree then begin
          SetFetchedStatus(RecBuf, fsNotFetched);
          SetFetchedStatus(ItemRecBuf, fsNotFetched);
        end;
      end
      else
        FItemRefCounts[RefCountInd].Count := FItemRefCounts[RefCountInd].Count - 1;
    end;
  end
  else
    FreeComplexFields(RecBuf, WithBlob);
end;
{$ENDIF}

function TCRRecordSet.NeedInitFieldsOnFetch: boolean;
begin
  Result := {$IFNDEF LITE}FForceInitFieldsOnFetch or{$ENDIF}
            (FFields.Count = 0);
end;

procedure TCRRecordSet.CheckFieldDescs;
{$IFNDEF LITE}
var
  FOldFields: TFieldDescs;
{$ENDIF}
begin
{$IFNDEF LITE}
  case FSmartFetchState of
    sfNone:
      InitFields;

    sfMetaInfo: begin
      FDataFieldsWasSynchronized := False;
      InitFields;
      if Assigned(FillExtFieldsInfo) then
        FillExtFieldsInfo
      else
        GetExtFieldsInfo(nil);
    end;

    sfKeyOnly: begin
      FOldFields := FFields;
      try
        FFields := FTmpFields;
        CreateFieldDescs;
        GetExtFieldsInfo(nil);
        SyncKeyFields(FTmpFields, FOldFields);
      finally
        FFields := FOldFields;
        FTmpFields.Clear;
      end;
    end;

    sfDataByKey: begin
      if not FDataFieldsWasSynchronized then begin
        FOldFields := FFields;
        try
          FFields := FTmpFields;
          CreateFieldDescs;
          GetExtFieldsInfo(nil);
          SyncDataFields(FTmpFields, FOldFields);
        finally
          FFields := FOldFields;
          FTmpFields.Clear;
        end;

        FDataFieldsWasSynchronized := True;
      end;
    end;
  else
    Assert(False);
  end;
{$ELSE}
  InitFields;
{$ENDIF}
end;

procedure TCRRecordSet.ExecFetch(DisableInitFields: boolean);
begin
  if GetCommand.FParams.Count > 0 then
    ExecCommand(GetCommand.FParams[0].GetArraySize)
  else
    ExecCommand;
  if (not DisableInitFields or RequiredReInitFields) and
    (not Prepared or NeedInitFieldsOnFetch)
  then
    CheckFieldDescs;

  begin
    try
      if FFetchAll {$IFNDEF LITE}or (FSmartFetchState <> sfNone){$ENDIF} then
        FetchAll
      else
        Fetch;
    except
      on E: Exception do begin
        if Assigned(FAfterExecFetch) then
          FAfterExecFetch(False);
        raise;
      end;
    end;

    if Assigned(FAfterExecFetch) then
      FAfterExecFetch(True);
  end;
end;

procedure TCRRecordSet.BreakFetch; // Breaks fetch. Can be called from other thread or in non-blocking mode
begin
  FWaitForFetchBreak := True;
end;

procedure TCRRecordSet.WaitForFetch;
begin
end;

procedure TCRRecordSet.AllocFetchBuffer;
begin
end;

procedure TCRRecordSet.FreeFetchBuffer;
begin
  if FFetchBuffer <> nil then begin
    Marshal.FreeHGlobal(FFetchBuffer);
    FFetchBuffer := nil;
  end;

  FFetchBufferSize := 0;
end;

procedure TCRRecordSet.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);
begin
  RowsObtained := 0;
end;

procedure TCRRecordSet.ProcessFetchedBlock(Block: PBlockHeader; FetchBack: boolean);
begin
end;

procedure TCRRecordSet.ProcessNoResult(FetchBack: boolean);
begin
end;

function TCRRecordSet.ProcessFetchedException(E: Exception): boolean;
begin
  Result := False;
end;

function TCRRecordSet.FetchingAccessible(FetchBack: boolean): boolean;
begin
  Result := True;
end;

function TCRRecordSet.InternalFetch(FetchBack: boolean): boolean;
var
  pHBlock: PBlockHeader;
  NewBlock, StandAloneBlock: Boolean;
  OldFirstItem: IntPtr;
  OldLastItem: IntPtr;
begin
  Result := False;

  if FCommand.GetCursorState < csFetching then
    FCommand.SetCursorState(csFetching);

  if not FetchingAccessible(FetchBack) then
    Exit;

  StandAloneBlock := {$IFNDEF LITE}FSmartFetchState = sfDataByKey{$ELSE}False{$ENDIF};

{$IFNDEF LITE}
  if StandAloneBlock then
    NewBlock := FSmartFetchBlock = nil
  else
{$ENDIF}
    NewBlock := (BlockMan.FirstBlock = nil) or (not FUniDirectional and not CanFetchBack);

  if NewBlock then begin
    BlockMan.AllocBlock(pHBlock, FFetchRows, StandAloneBlock);

  {$IFNDEF LITE}
    if StandAloneBlock then
      FSmartFetchBlock := pHBlock;
  {$ENDIF}

    if FFetchBuffer = nil then
      AllocFetchBuffer;
  end
{$IFNDEF LITE}
  else if StandAloneBlock then begin
    if FSmartFetchBlock.ItemCount < FFetchRows then begin
      BlockMan.ReAllocBlock(FSmartFetchBlock, FFetchRows);
      if FFetchBuffer = nil then
        AllocFetchBuffer;
    end;

    pHBlock := FSmartFetchBlock;
  end
{$ENDIF}
  else begin
    pHBlock := BlockMan.FirstBlock; // overwrite data in unidirectional or scrollable mode
    ClearBlock(pHBlock);

  end;
  InitBlock(phBlock);

  OldFirstItem := FirstItem;
  OldLastItem := LastItem;
  try
  {$IFNDEF LITE}
    FInFetching := True;
    FetchBlock(pHBlock, FetchBack, FLastRowsObtained);
    FInFetching := False;
  {$ELSE}
    FetchBlock(pHBlock, FetchBack, FLastRowsObtained);
  {$ENDIF}
    Result := FLastRowsObtained > 0;

    if Result then begin
        CreateBlockStruct(pHBlock, FLastRowsObtained, FetchBack{$IFNDEF LITE}, StandAloneBlock{$ENDIF});

      ProcessFetchedBlock(pHBlock, FetchBack);

      if (FCommand.GetCursorState in [csFetched, csInactive]) and not StandAloneBlock then
        FreeFetchBuffer;
    end
    else begin
      ClearBlock(pHBlock);

      if not StandAloneBlock then begin
        BlockMan.FreeBlock(pHBlock);
        FreeFetchBuffer;
      end;

      ProcessNoResult(FetchBack);
      if BlockMan.FirstBlock = nil then
        InitData;
    end;

  except
    on E: Exception do begin
    {$IFNDEF LITE}
      FInFetching := False;
    {$ENDIF}
      ClearBlock(pHBlock);
      // BlockMan.FirstBlock = nil means that dataset was closed after some
      // fatal error and all blocks are already freed
      if not StandAloneBlock and (BlockMan.FirstBlock <> nil) then begin
        BlockMan.FreeBlock(pHBlock);
        // restore first and last items
        FirstItem := OldFirstItem;
        LastItem := OldLastItem;
        if FirstItem <> nil then
          FirstItem.Prev := nil;
        if LastItem <> nil then
          LastItem.Next := nil;
      end;

      if not ProcessFetchedException(E) then
        raise;
    end;
  end;
end;

function TCRRecordSet.Fetch(FetchBack: boolean = False): boolean;
var
  Cancel: boolean;
  OldCommandType: TCommandType;
begin
  Result := False;

  if not (FCommand.GetCursorState in [csFetched, csInactive]) then begin
    DoBeforeFetch(Cancel);
    if Cancel then
      Exit;
    try
      try
        Result := InternalFetch(FetchBack);
      except
        if FetchBack then
          FBOF := True
        else
          FEOF := True;
        raise;
      end;
    finally
      DoAfterFetch;
    end;
  end;

  if FCommand.GetCursorState = csFetched then
    if NeedUnPrepareAfterFetch then begin
      OldCommandType := FCommand.CommandType;
      InternalUnPrepare;
      // We need to save old CommandType to save old FieldDescs on Refresh
      FCommand.CommandType := OldCommandType;
      Prepared := False;
    end;
end;

function TCRRecordSet.NeedUnPrepareAfterFetch: boolean;
begin
  Result := False;
end;

function TCRRecordSet.RequiredReInitFields: boolean;
begin
  Result := False;
end;

{ Fields }

function TCRRecordSet.NeedConvertEOL: boolean;
begin
  if (FCommand = nil) or (FCommand.FConnection = nil) then
    Result := False
  else
    Result := FCommand.FConnection.FConvertEOL;
end;

function TCRRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TCRFieldDesc;
end;

function TCRRecordSet.ExtFieldsInfoIsInternal: boolean;
begin
  Result := True;
end;

{$IFNDEF LITE}

procedure TCRRecordSet.InitExtFieldsInfo;
begin
  if FExtFieldsInfoInited then
    Exit;

  InitUpdatingTableIdx;
  GetExtFieldsInfo(RequestFieldsInfo);
  DetectIdentityField;

  FExtFieldsInfoInited := True;
end;

procedure TCRRecordSet.GetExtFieldsInfo(RequestFieldsInfo: TRequestFieldsInfoProc);
var
  Tables: TSQLObjectsInfo;
  Columns: TCRColumnsInfo;
  ReadFieldsFromServer: boolean;
  Str, TableName: string;
  AsteriskCount, DefaultTable: integer;
  ColumnInfo: TCRColumnInfo;
  i, j: integer;
  SQLInfo: TSQLInfo;
  ParserClass: TSQLParserClass;
begin
  if ExtFieldsInfoIsInternal then
    Exit;

  // for the case when we can not determine tables list
  // select from (select from)
  if TablesInfo.Count = 0 then
    exit;

  SQLInfo := FCommand.SQLInfo;
  ParserClass := FCommand.GetParserClass;
  SetLength(Tables, TablesInfo.Count);
  for i := 0 to High(Tables) do begin
    SQLInfo.SplitObjectName(TablesInfo[i].TableName, Tables[i]);
    Tables[i].Name := SQLInfo.NormalizeName(Tables[i].Name, False, True);
    Tables[i].Schema := SQLInfo.NormalizeName(Tables[i].Schema, False, True);
    Tables[i].Catalog := SQLInfo.NormalizeName(Tables[i].Catalog, False, True);
    Tables[i].DBLink := SQLInfo.NormalizeName(Tables[i].DBLink, False, True);
  end;

  Str := FCommand.SQL;
  Columns := TCRColumnsInfo.Create;
  try
    SQLInfo.ParseColumnsInfo(Str, Columns);
    AsteriskCount := 0;
    DefaultTable := -1; // table index for fields from '*'
    ReadFieldsFromServer := UpdTableIsArtificial;
    for i := 0 to Columns.Count - 1 do begin
      ColumnInfo := Columns[i];
      if (ColumnInfo.Table <> '') and not UpdTableIsArtificial then
        for j := 0 to TablesInfo.Count - 1 do begin
          if TablesInfo[j].TableAlias <> '' then
            TableName := SQLInfo.NormalizeName(TablesInfo[j].TableAlias, False, True)
          else
            TableName := SQLInfo.NormalizeName(TablesInfo[j].TableName, False, True);
          if (ColumnInfo.Table = TableName) or
            // table name without schema
            (TablesInfo[j].TableAlias = '') and (ColumnInfo.Table = Tables[j].Name)
          then begin
            ColumnInfo.TableIndex := j;
            break;
          end;
        end;

      // We use Expr to set Alias.
      // Currently Expr is not properly normalized by ParseColumnsInfo.
      // Moreover Expr is returned without spaces. So Alias will not always correct.
      ColumnInfo.Expr := AnsiUpperCase(ColumnInfo.Expr);

      if (ColumnInfo.Alias = '') and (ColumnInfo.Name <> '*') then begin
        if ColumnInfo.Name <> '' then
          ColumnInfo.Alias := ColumnInfo.Name
        else
        // We need correct Alias do find corresponding FieldDesc.
        // Otherwise expression can be treated as field from '*'.
          ColumnInfo.Alias := ColumnInfo.Expr;
      end;

      if ColumnInfo.Name = '*' then begin
        Inc(AsteriskCount);
        // If Table <> '', we use this ColumnInfo.TableIndex for all FieldDescs that does not
        // have corresponding ColumnInfo.
        // If tables count > 1 and Table = '' or there are several '*' with different Table,
        // we cannot determine FieldDesc's table without query to ALL_TAB_COLUMNS.
        // There can be several tables even if TableInfo.Count = 1:
        // select * from t1, (select a from b) t2
        if (ColumnInfo.Table = '') or
          (AsteriskCount > 1) and (ColumnInfo.TableIndex <> DefaultTable)
        then
          ReadFieldsFromServer := True;

        // ReadFieldsFromServer will be reset to False if ExtendedFieldsInfo = False.
        // So DefaultTable must be set.
        // When ExtendedFieldsInfo = False we suggest that all fields w/o ColumnInfo
        // belong to Table from first '*' in query
        if AsteriskCount = 1 then begin // for first '*'
          if (ColumnInfo.Table = '') or UpdTableIsArtificial then
            DefaultTable := 0
          else
            DefaultTable := ColumnInfo.TableIndex; // can be -1 for table that is not present in TablesInfo
        end;
      end
      else
      if ParserClass.IsQuasiColumn(ColumnInfo.Name) then begin
      // Column not present in metadata - ROWID
        if ColumnInfo.Table = '' then
          ColumnInfo.TableIndex := 0;
      end
      else begin
        if (ColumnInfo.Table = '') and (ColumnInfo.Name <> '') then
           // Possibly it is a value or system variable
           ReadFieldsFromServer := True;
      end;
    end;

    ReadFieldsFromServer := ReadFieldsFromServer and
      (FExtendedFieldsInfo or (FFieldOrigins <> foNone));

    if ReadFieldsFromServer or FDefaultValues then
      if Assigned(RequestFieldsInfo) then
        RequestFieldsInfo(Tables, Columns);

    ApplyColumnsInfo(Columns, ReadFieldsFromServer, DefaultTable, AsteriskCount);
  finally
    Columns.Free;
  end;
end;

procedure TCRRecordSet.ApplyColumnsInfo(Columns: TCRColumnsInfo; ReadFieldsFromServer: boolean; DefaultTable: integer; AsteriskCount: integer);
var
  FieldDesc: TCRFieldDesc;
  IdentCase: TIdentCase;
  FieldName: string;
  ColumnInfo: TCRColumnInfo;
  i, j: integer;
begin
  IdentCase := FCommand.SQLInfo.IdentCase;
  for i := 0 to Fields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(Fields[i]);
    if FieldDesc.FieldDescKind <> fdkData then
      continue;

    FieldName := FieldDesc.ActualName;
    for j := 0 to Columns.Count - 1 do begin
      ColumnInfo := Columns[j];
      if not ColumnInfo.Used and
        ((IdentCase <> icMixed) and (FieldName = ColumnInfo.Alias) or
        (IdentCase = icMixed) and SameText(FieldName, ColumnInfo.Alias)) and
        (not ReadFieldsFromServer or (ColumnInfo.TableIndex <> -1))
      then begin
        if ColumnInfo.Name <> '' then begin
          ColumnInfo.Used := True;

          if ColumnInfo.TableIndex <> - 1 then
            FieldDesc.TableInfo := TablesInfo[ColumnInfo.TableIndex]
          else
            // When ExtendedFieldsInfo = False we suggest that field w/o Table
            // belong to first table in TablesInfo
            if not ReadFieldsFromServer and
              (ColumnInfo.Table = '') and (FieldDesc.TableInfo = nil)
            then
              FieldDesc.TableInfo := TablesInfo[0];

          if ColumnInfo.Alias <> '' then
            FieldDesc.ActualName := ColumnInfo.Name;
          FieldDesc.DefaultExpr := ColumnInfo.Expr;
        end;

        break;
      end
      else
        // If we don't find corresponding ColumnInfo,
        // we suggest that field belong to the same table that '*' belong
        if j = Columns.Count - 1 then begin
          if not ReadFieldsFromServer and
            (DefaultTable <> -1) and (FieldDesc.TableInfo = nil)
          then
            FieldDesc.TableInfo := TablesInfo[DefaultTable];
        end;
    end;
  end;
end;

function TCRRecordSet.IsEqualDataTypes(Field: TFieldDesc; MapRule: TCRMapRule): boolean;
begin
  if Field.DataType = MapRule.DataType then
    if Field.DataType in [dtString, dtExtString, dtWideString, dtExtWideString, dtBytes, dtVarBytes, dtExtVarBytes] then
      Result := ((MapRule.FieldLength = -1) or (MapRule.FieldLength = Field.Length))
    else if Field.DataType in [dtBCD, dtFMTBcd] then
      Result := ((MapRule.FieldLength = -1) or (MapRule.FieldLength = Field.Length)) and
                ((MapRule.FieldScale  = -1) or (MapRule.FieldScale = Field.Scale))
    else
      Result := True
  else
    Result := False;
end;

function TCRRecordSet.GetDataTypeName(Field: TCRFieldDesc): string;
var
  DBTypeInfo: TDBTypeInfo;
begin
  DBTypeInfo := DBTypeInfos.FindTypeInfo(Field.DBType);
  if DBTypeInfo <> nil then
    Result := DBTypeInfo.Name
  else
    Result := IntToStr(Field.DataType);
end;

procedure TCRRecordSet.InitRecordSize;
var
  i: integer;
  Align: integer;
  FieldDesc: TFieldDesc;
begin
  FDataSize := 0;
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := FFields[i];
    if (FieldDesc.FieldDescKind <> fdkCalculated) and not FieldDesc.HasParent then begin
      FieldDesc.Offset := FDataSize;

      if FieldDesc.DataType = dtWideString then begin
        Align := FieldDesc.Offset and 1;
        FieldDesc.Offset := FieldDesc.Offset + Align; // align 2
      end
      else
        Align := 0;

      if FieldDesc.HasValueLen then
        FieldDesc.DataOffset := FieldDesc.Offset + SizeOf(Word)
      else
        FieldDesc.DataOffset := FieldDesc.Offset;

      FDataSize := FDataSize + FieldDesc.Size + Align;
    end;
  end;

  FChangedIndicatorOffset := FDataSize + GetIndicatorSize;
  FEncryptStateOffset := FChangedIndicatorOffset + GetChangedIndicatorSize;
  FFetchStateOffset := FEncryptStateOffset + GetEncryptStateSize;
  FRefCountOffset := FFetchStateOffset + GetFetchStateSize;
  FItemPtrOffset := FRefCountOffset + GetRefCountSize;
  FRecordSize := FItemPtrOffset + GetItemPtrSize;
  Align := FRecordSize and 1;
  FRecordSize := FRecordSize + Align; // align 2
end;

procedure TCRRecordSet.ClearFields;
begin
  // ClearFields is called from InitFields
  FForceInitFieldsOnFetch := False;

  inherited;
end;

procedure TCRRecordSet.InternalInitFieldDescs;
var
  i, k: integer;
  FieldDesc: TCRFieldDesc;
  OnDemandConverter: TOnDemandConverter;
  MapRule: TCRMapRule;
  sl: TStrings;
begin
  inherited;

  // Data Type Mapping Init
  if FDataTypeMap.Enabled and ((FDataTypeMap.Count > 0) or (FCommand.FConnection <> nil) and (FCommand.FConnection.DataTypeMap.Count > 0)) then
    for i := 0 to FFields.Count - 1 do begin
      FieldDesc := FFields[i] as TCRFieldDesc;
      FieldDesc.MapRule := nil;
      FieldDesc.OnDemandConverter := nil;

      OnDemandConverter := GetMapOnDemandConverter(FieldDesc, MapRule);

      if (MapRule <> nil) and not IsEqualDataTypes(FieldDesc, MapRule) then
        if OnDemandConverter <> nil then begin
          FieldDesc.OnDemandConverter := OnDemandConverter;

          FieldDesc.MapRule := MapRule;
          FieldDesc.MapHasValueLen := HasValueLen(FieldDesc.OnDemandConverter.DestDataType);

          if MapRule.FieldLength > -1 then
            FieldDesc.MapLength := MapRule.FieldLength
          else if Assigned(OnDemandConverter.CalcLength) then begin
            FieldDesc.MapLength := FieldDesc.Length;
            FieldDesc.MapLength := OnDemandConverter.CalcLength(FieldDesc.MapLength);
          end
          else if FieldDesc.MapHasValueLen then
            FieldDesc.MapLength := 20
          else
            FieldDesc.MapLength := FieldDesc.Length;

          if MapRule.FieldScale > -1 then
            FieldDesc.MapScale := MapRule.FieldScale
          else if Assigned(OnDemandConverter.CalcScale) then begin
            FieldDesc.MapScale := FieldDesc.Scale;
            FieldDesc.MapScale := OnDemandConverter.CalcScale(FieldDesc.MapScale);
          end
          else
            FieldDesc.MapScale := FieldDesc.Scale;
        end
        // to raise exception at DA level
        else if OnDemandConverter = nil then
          FieldDesc.MapRule := MapRule;
    end;

  if (FEncryptor <> nil) and (FEncryptedFields <> '') then begin
    sl := TStringList.Create;
    try
      FCommand.SQLInfo.NamesToList(FEncryptedFields, sl);

      k := 0;
      for i := 0 to FFields.Count - 1 do begin
        FieldDesc := FFields[i] as TCRFieldDesc;
        if (sl.IndexOf(FieldDesc.Name) > -1) or (sl.IndexOf(FieldDesc.ActualName) > -1) then begin
          if (FieldDesc.FieldDescKind = fdkCalculated) or
             FieldDesc.HasParent or
             not IsEncryptableDataType(FieldDesc.DataType)
          then
            raise Exception.CreateFmt(SEncryptionNotSupported, [GetDataTypeName(FieldDesc)]);

          FieldDesc.Encryptor := TCREncryptorUtils.Encryptor(FEncryptor);
          FieldDesc.Decryptor := TCREncryptorUtils.Decryptor(FEncryptor);
          FieldDesc.EncryptState := ((k shr 3) shl 8) or (1 shl (k mod 8));
          Inc(k);
        end;
      end;
    finally
      sl.Free;
    end;
  end;
end;

function TCRRecordSet.GetChangedIndicatorSize: Integer;
begin
  if FInsertAllSetFields then
    Result := (FFields.Count - 1) shr 3 + 1
  else
    Result := 0;
end;

procedure TCRRecordSet.ClearChangedIndicators(RecBuf: IntPtr);
begin
  if FInsertAllSetFields then
    FillChar(PtrOffset(RecBuf, FChangedIndicatorOffset), GetChangedIndicatorSize, 0);
end;

function TCRRecordSet.GetChanged(Field: TFieldDesc; RecBuf: IntPtr): boolean;
var
  State: Byte;
begin
  if FInsertAllSetFields then begin
    State := Marshal.ReadByte(RecBuf, FChangedIndicatorOffset + (Field.FieldNo - 1) shr 3);
    Result := State and (1 shl ((Field.FieldNo - 1) mod 8)) <> 0;
  end
  else
    Result := False;
end;

procedure TCRRecordSet.SetChanged(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
var
  BitMask, State: Byte;
begin
  if FInsertAllSetFields then begin
    BitMask := 1 shl ((Field.FieldNo - 1) mod 8);
    State := Marshal.ReadByte(RecBuf, FChangedIndicatorOffset + (Field.FieldNo - 1) shr 3);
    if Value then
      State := State or BitMask
    else
      State := State and not BitMask;
    Marshal.WriteByte(RecBuf, FChangedIndicatorOffset + (Field.FieldNo - 1) shr 3, State);
  end;
end;

function TCRRecordSet.IsEncryptableDataType(DataType: Word): boolean;
begin
  Result := DataType in [dtString, dtExtString, dtWideString, dtExtWideString,
    {dtBytes,} dtVarBytes, dtExtVarBytes, dtBlob, dtMemo, dtWideMemo];
end;

function TCRRecordSet.GetDecryptDataType(DataType: Word): Word;
begin
  Result := DataType;
end;

function TCRRecordSet.GetEncryptStateSize: Integer;
var
  i, k: integer;
begin
  k := 0;
  for i := 0 to FFields.Count - 1 do
    if Assigned(TCRFieldDesc(FFields[i]).Decryptor) then
      Inc(k);

  if k > 0 then
    Result := (k - 1) shr 3 + 1
  else
    Result := 0;
end;

function TCRRecordSet.GetEncrypted(Field: TFieldDesc; RecBuf: IntPtr): boolean;
var
  EncryptState: Integer;
  EncryptBitMask, CurrentEncryptState: Byte;
begin
  EncryptState := (Field as TCRFieldDesc).EncryptState;
  if EncryptState >= 0 then begin
    EncryptBitMask := Byte(ShortInt(EncryptState));
    CurrentEncryptState := Marshal.ReadByte(RecBuf, FEncryptStateOffset + (EncryptState shr 8));
    Result := CurrentEncryptState and EncryptBitMask <> 0;
  end
  else
    Result := False;
end;

procedure TCRRecordSet.SetEncrypted(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
var
  EncryptState: Integer;
  EncryptBitMask, CurrentEncryptState: Byte;
begin
  EncryptState := (Field as TCRFieldDesc).EncryptState;
  if EncryptState >= 0 then begin
    EncryptBitMask := Byte(ShortInt(EncryptState));
    CurrentEncryptState := Marshal.ReadByte(RecBuf, FEncryptStateOffset + (EncryptState shr 8));
    if Value then
      CurrentEncryptState := CurrentEncryptState or EncryptBitMask
    else
      CurrentEncryptState := CurrentEncryptState and not EncryptBitMask;
    Marshal.WriteByte(RecBuf, FEncryptStateOffset + (EncryptState shr 8), CurrentEncryptState);
  end;
end;

procedure TCRRecordSet.DecryptBuffer(Item: PItemHeader);
var
  i: integer;
  RecBuf: IntPtr;
  DataBuf: IntPtr;
  DataLenPtr: PWord;
  FieldDesc: TCRFieldDesc;
  DataType, CryptType: Word;
  DataLen: Cardinal;
  Blob: TBlob;
  Data: TBytes;
  DecryptedDataType: TCRDecryptedDataType;
begin
  Assert(Assigned(FEncryptor));

  SetLength(Data, 0); // anti-warning
  RecBuf := PtrOffset(Item, SizeOf(TItemHeader));
  FillChar(PtrOffset(RecBuf, FEncryptStateOffset), GetEncryptStateSize, 0);
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := FFields[i] as TCRFieldDesc;
    if not Assigned(FieldDesc.Decryptor) then
      Continue;

    if (FSmartFetchState = sfKeyOnly) and not IsFieldPrefetched(FieldDesc) then
      Continue;

    if GetNull(FieldDesc, RecBuf) then
      Continue;

    DataType := GetDecryptDataType(FieldDesc.DataType);
    DataBuf := PtrOffset(RecBuf, FieldDesc.DataOffset);
    if FieldDesc.HasValueLen then
      DataLenPtr := PtrOffset(RecBuf, FieldDesc.Offset)
    else
      DataLenPtr := nil;
    DataLen := FieldDesc.Length;

    case DataType of
      dtString, dtExtString: begin
        DataLen := DataLenPtr^;
        if DataType = dtExtString then
          DataBuf := PIntPtr(DataBuf)^;
        CryptType := dtString;
      end;
      dtWideString, dtExtWideString: begin
        DataLen := DataLenPtr^;
        if DataType = dtExtWideString then
          DataBuf := PIntPtr(DataBuf)^;
        CryptType := dtWideString;
      end;
      dtBytes, dtVarBytes, dtExtVarBytes: begin
        DataLen := DataLenPtr^;
        if DataType = dtExtVarBytes then
          DataBuf := PIntPtr(DataBuf)^;
        CryptType := dtBytes;
      end;
      dtMemo, dtWideMemo: begin
        Blob := TBlob(InternalGetObject(FieldDesc, RecBuf));
        Data := Blob.AsBytes;
        if Blob.IsUnicode then begin
          DataLen := Length(Data) shr 1;
          CryptType := dtWideMemo;
        end
        else begin
          DataLen := Length(Data);
          CryptType := dtMemo;
        end;
        if DataLen > 0 then
          DataBuf := @Data[0]
        else
          DataBuf := nil;
      end;
      dtBlob: begin
        Blob := TBlob(InternalGetObject(FieldDesc, RecBuf));
        DataBuf := Blob.GCHandle;
        DataLen := GetBlobSize(FieldDesc, RecBuf);
        CryptType := dtBlob;
      end;
    else
      raise Exception.CreateFmt(SEncryptionNotSupported, [GetDataTypeName(FieldDesc)]);
    end;

    if DataLen > 0 then begin
      FieldDesc.Decryptor(DataBuf, CryptType, DataLen, DecryptedDataType);

      if DecryptedDataType <> ddtNonEncrypted then
        case DataType of
          dtString, dtExtString: begin
            DataLenPtr^ := DataLen;
            Marshal.WriteByte(DataBuf, DataLen, 0);
          end;
          dtWideString, dtExtWideString: begin
            DataLenPtr^ := DataLen;
            Marshal.WriteInt16(DataBuf, DataLen, 0);
          end;
          dtVarBytes, dtExtVarBytes:
            DataLenPtr^ := DataLen;
          dtMemo, dtWideMemo: begin
            Blob := TBlob(InternalGetObject(FieldDesc, RecBuf));
            Blob.RollbackEnabled := False;
            try
              Blob.Clear;
              if DataLen > 0 then
                Blob.Write(0, DataLen, @Data[0]);
            finally
              Blob.RollbackEnabled := True;
            end;
          end;
          dtBlob: begin
            if DataLen = 0 then begin
              Blob := TBlob(InternalGetObject(FieldDesc, RecBuf));
              Blob.RollbackEnabled := False;
              Blob.Clear;
              Blob.RollbackEnabled := True;
            end;
          end;
        end;

      if (DecryptedDataType = ddtError) and (DataLen = 0) then
        SetNull(FieldDesc, RecBuf, True)
      else
        SetEncrypted(FieldDesc, RecBuf, DecryptedDataType = ddtDecrypted);
    end;
  end;
end;
{$ENDIF}

procedure TCRRecordSet.InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean);
begin
  if Filtered and not FFetchAll then begin
    if FetchBack then
      InitFetchedItems(PtrOffset(Block, SizeOf(TBlockHeader) + (RowsObtained - 1) * (SizeOf(TItemHeader) + RecordSize)), FNoCountData, FetchBack)
    else
      InitFetchedItems(PtrOffset(Block, SizeOf(TBlockHeader)), FNoCountData, FetchBack);
  end
  else
    if not (FetchBack or FNoCountData) then
      Inc(FRecordCount, RowsObtained);
end;

procedure TCRRecordSet.CreateBlockStruct(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean = False{$IFNDEF LITE}; StandAloneBlock: Boolean = False{$ENDIF});
var
  Item, FirstBlockItem, LastBlockItem{$IFNDEF LITE}, NextItem{$ENDIF}: PItemHeader;
  FirstItemOrder, LastItemOrder: integer;
  ItemSize: integer;
  i: integer;

{$IFNDEF LITE}
  procedure DecryptItems;
  var
    i: integer;
  begin
    if FEncryptor <> nil then begin
      Item := FirstBlockItem;
      for i := 1 to RowsObtained do begin
        DecryptBuffer(Item);
        Item := Item.Next;
      end;
    end;
  end;
{$ENDIF}

begin
  if FirstItem <> nil then
    FirstItemOrder := FirstItem.Order
  else
    FirstItemOrder := 0;
  if LastItem <> nil then
    LastItemOrder := LastItem.Order
  else
    LastItemOrder := 0;

  // Create Items
  ItemSize := RecordSize + SizeOf(TItemHeader);
  FirstBlockItem := PtrOffset(Block, SizeOf(TBlockHeader));
  LastBlockItem := PtrOffset(Block, SizeOf(TBlockHeader) + (RowsObtained - 1) * ItemSize);
  Item := FirstBlockItem;

  for i := 1 to RowsObtained do begin
    Item.Block := Block;
    Item.Flag := flUsed;
    InitItem(Item);
    Item := PtrOffset(Item, ItemSize);
  end;

  // Free items
  for i := RowsObtained + 1 to Block.ItemCount do begin
    Item.Prev := nil;
    Item.Next := nil;
    Item.Block := Block;
    Item.Flag := flFree;
    Item.Rollback := nil;
    Item.Status := isUnmodified;
    Item.UpdateResult := urNone;

    if {$IFNDEF LITE}not StandAloneBlock{$ELSE}True{$ENDIF} then begin
      Item.Next := BlockMan.FirstFree;
      if BlockMan.FirstFree <> nil then
        BlockMan.FirstFree.Prev := Item;
      BlockMan.FirstFree := Item;
    end;

    FreeComplexFields(PtrOffset(Item, SizeOf(TItemHeader)), True);

    Item := PtrOffset(Item, ItemSize);
  end;
  Block.UsedItems := RowsObtained;

  // Init items
{$IFNDEF LITE}
  if StandAloneBlock then begin
    Item := FirstBlockItem;
    for i := 1 to RowsObtained - 1 do begin
      NextItem := PtrOffset(Item, ItemSize);
      NextItem.Prev := Item;
      Item.Next := NextItem;
      Item := NextItem;
    end;
    FirstBlockItem.Prev := nil;
    LastBlockItem.Next := nil;
    DecryptItems;
    UpdateCachedBuffer(FirstBlockItem, LastBlockItem);
  end
  else
{$ENDIF}
  begin
    if FetchBack then begin
      Item := LastBlockItem;
      if LastItem = nil then
        LastItem := Item;
      if FirstItem <> nil then
        FirstItem.Order := FirstItemOrder;

      for i := RowsObtained downto 1 do begin
        Item.Prev := nil;
        Item.Next := FirstItem;

        if FirstItem <> nil then begin
          FirstItem.Prev := Item;
          Item.Order := FirstItem.Order - 1;
        end
        else
          Item.Order := FRecordCount;

        FirstItem := Item;
        Item := PtrOffset(Item, - ItemSize);
      end;
    end
    else begin
      Item := FirstBlockItem;
      if FUniDirectional or (FirstItem = nil) then
        FirstItem := Item;
      if LastItem <> nil then
        LastItem.Order := LastItemOrder;

      for i := 1 to RowsObtained do begin
        Item.Prev := LastItem;
        Item.Next := nil;

        if LastItem <> nil then begin
          Item.Order := LastItem.Order + 1;
          if LastItem.Flag <> flFree then
            LastItem.Next := Item;
        end
        else
          Item.Order := 1;

        LastItem := Item;
        Item := PtrOffset(Item, ItemSize);
      end;
    end;

    FirstItem.Prev := nil;
    LastItem.Next := nil;

  {$IFNDEF LITE}
    DecryptItems;
  {$ENDIF}
    UpdateCachedBuffer(FirstBlockItem, LastBlockItem);
    InitFetchedBlock(Block, RowsObtained, FetchBack);

    if not (FetchBack or FNoCountData) then
      Inc(FRowsFetched, RowsObtained);

    if Block.ItemCount > RowsObtained then
      if not FetchBack then
        FNoCountData := True;
  end;
end;

procedure TCRRecordSet.InitBlock(Block: PBlockHeader);
var
  i, j: integer;
  Item: PItemHeader;
  FieldDesc: TFieldDesc;
  ItemSize: Integer;
begin
  if HasComplexFields then begin
    Item := PtrOffset(Block, SizeOf(TBlockHeader) + SizeOf(TItemHeader));

    ItemSize := SizeOf(TItemHeader) + RecordSize;
    for i := 1 to Block.ItemCount do begin
      for j := 0 to FFields.Count - 1 do begin
        FieldDesc := FFields[j];
        if FieldDesc.IsComplex and not FieldDesc.HasParent and (FieldDesc.FieldDescKind <> fdkCalculated) then
          Marshal.WriteIntPtr(Item, FieldDesc.DataOffset, nil);
      end;

      Item := PtrOffset(Item, ItemSize);
    end;
  end;
end;

procedure TCRRecordSet.ClearBlock(Block: PBlockHeader);
var
  i: integer;
  Item: PItemHeader;
  ItemSize: Integer;
begin
  if HasComplexFields then begin
    Item := PtrOffset(Block, SizeOf(TBlockHeader) + SizeOf(TItemHeader));

    ItemSize := SizeOf(TItemHeader) + RecordSize;
    for i := 1 to Block.ItemCount do begin
      // this condition doesn't allow to free Complex fields in ODAC
      //if Item.Flag <> flFree then
      FreeComplexFields(Item, True);
      Item := PtrOffset(Item, ItemSize);
    end;
  end;
end;

{$IFNDEF LITE}
procedure TCRRecordSet.SyncBlocks(FirstFetchingItem: PItemHeader; RowCount: integer);
var
  i, j, k: integer;
  CurItem, NewItem: PItemHeader;
  CurRecBuf, NewRecBuf: IntPtr;
  ItemFound: boolean;
  ItemsIsFound: TBooleanArray;
  KeyFieldDescs: TFieldDescArray;
begin
  if FSmartFetchBlock = nil then
    SetLength(ItemsIsFound, 0)
  else begin
    SetLength(ItemsIsFound, FSmartFetchBlock.UsedItems);
    GetKeyFieldDescs(KeyFieldDescs);
  end;

  CurItem := FirstFetchingItem;
  for i := 1 to RowCount do begin
    if (CurItem.Flag = flUsed) and (GetFetchedStatus(PtrOffset(CurItem, SizeOf(TItemHeader))) = fsNotFetched) then begin
      CurRecBuf := PtrOffset(CurItem, SizeOf(TItemHeader));

      ItemFound := False;
      if FSmartFetchBlock <> nil then begin
        NewItem := PtrOffset(FSmartFetchBlock, SizeOf(TBlockHeader));
        for j := 0 to FSmartFetchBlock.UsedItems - 1 do begin
          if not ItemsIsFound[j] then begin
            NewRecBuf := PtrOffset(NewItem, SizeOf(TItemHeader));
            ItemFound := True;
            for k := 0 to High(KeyFieldDescs) do
              if CompareFields(CurRecBuf, NewRecBuf, KeyFieldDescs[k], [], False) <> 0 then begin
                ItemFound := False;
                Break;
              end;

            if ItemFound then begin
              ItemsIsFound[j] := True;
              Break;
            end;
          end;
          NewItem := NewItem.Next;
        end;
      end
      else
        NewItem := nil;

      if ItemFound then begin
        FreeComplexFields(CurRecBuf, True);
        BlockMan.CopyRecord(NewItem, CurItem);

        CurItem.Flag := flUsed;
        CurItem.Rollback := nil;
        CurItem.Status := isUnmodified;
        CurItem.UpdateResult := urNone;
        CurItem.FilterResult := fsNotChecked;
        SetFetchedStatus(CurRecBuf, fsFetched); // need because CopyRecord rewrite Status
        WriteItemPtrIntoRecBuf(CurRecBuf, CurItem); // need because CopyRecord rewrite ItemPtr
        AddRef(CurRecBuf);
      end
      else begin
        // if the record was not found, this means it was deleted by another user and we should delete it
        CurItem.Status := isDeleted;
        CurItem.UpdateResult := urApplied;
        CurItem.FilterResult := fsOmitted;
      end;
    end;

    CurItem := CurItem.Next;
  end;

  if (FSmartFetchBlock <> nil) and (FSmartFetchBlock.UsedItems > 0) then
    ClearBlock(FSmartFetchBlock);
end;
{$ENDIF}

procedure TCRRecordSet.DoBeforeFetch(out Cancel: boolean);
begin
  if FWaitForFetchBreak then
    raise Exception.Create(SOpeningWasCanceled);

  Cancel := False;
  if Assigned(FOnBeforeFetch) then
    FOnBeforeFetch(Cancel);

  if Cancel then begin
    // reset cursor state for FetchAll
    if (FCommand.GetCursorState = csFetchingAll) or (FCommand.GetCursorState = csFetching) then
      FCommand.SetCursorState(csFetched);
  end;
end;

procedure TCRRecordSet.DoAfterFetch;
begin
  if Assigned(FOnAfterFetch) then
    FOnAfterFetch;
end;

procedure TCRRecordSet.SortItems;
begin
  if not FOrderSaved and (IndexFieldCount = 0) then
    Exit;

  FetchAll;

  inherited SortItems;
end;

procedure TCRRecordSet.GetNextRecord(RecBuf: IntPtr);
var
  Found: boolean;
  Item: PItemHeader;
begin
  if not EOF then begin
    if FirstItem = nil then begin
      if not Fetch then begin
        FEOF := True;
        Exit;
      end
      else
        CurrentItem := FirstItem;
    end
    else
      if CurrentItem = nil then
        CurrentItem := FirstItem
      else
        CurrentItem := CurrentItem.Next;

    repeat
      if CurrentItem = nil then begin
        Item := LastItem;

        if not Fetch then begin
          FEOF := True;
          Exit;
        end
        else begin
          if FUniDirectional or CanFetchBack then begin
            FirstItem.Prev := nil;  // remove cycle link
            LastItem.Next := nil;
          end;
          if (Item.Next = nil) or FUniDirectional then
            CurrentItem := FirstItem
          else
            CurrentItem := Item.Next;
        end
      end;

      Found := not OmitRecord(CurrentItem);
      if not Found then
        CurrentItem := CurrentItem.Next;
    until Found;

    FBOF := False;
    FEOF := False;

    if RecBuf <> nil then
      GetRecord(RecBuf);
  end;
end;

procedure TCRRecordSet.GetPriorRecord(RecBuf: IntPtr);
var
  Found: boolean;
  Item: PItemHeader;
begin
  if FUniDirectional then begin
    FBOF := True;
    CurrentItem := nil;
  end
  else
    if not CanFetchBack then
      inherited
    else
      if not BOF then begin
        if LastItem = nil then begin
          if not Fetch(True){FetchBack!} then begin
            FBOF := True;
            Exit;
          end
          else
            CurrentItem := LastItem;
        end
        else
          if CurrentItem = nil then
            CurrentItem := LastItem
          else
            CurrentItem := CurrentItem.Prev;

        repeat
          if CurrentItem = nil then begin
            Item := FirstItem;

            if not Fetch(True){FetchBack!} then begin
              FBOF := True;
              Exit;
            end
            else begin
              FirstItem.Prev := nil;  // remove cycle link
              LastItem.Next := nil;
              if Item.Prev = nil then
                CurrentItem := LastItem
              else
                CurrentItem := Item.Prev;
            end;
          end;

          Found := not OmitRecord(CurrentItem);
          if not Found then
            CurrentItem := CurrentItem.Prev;
        until Found;

        FBOF := False;
        FEOF := False;
        if RecBuf <> nil then
          GetRecord(RecBuf);
      end;
end;

procedure TCRRecordSet.DeleteRecord;
var
  OldOrder: Integer;
begin
  if FUniDirectional then begin
    InternalDelete;
    OldOrder := CurrentItem.Order;
    if CurrentItem.Next = nil then begin
      CurrentItem.Order := OldOrder - 1;
      Fetch;
      CurrentItem := FirstItem;
    end
    else begin
      RemoveRecord;
      if (CurrentItem.Next = nil) and (CurrentItem.Prev = nil) then
        CurrentItem.Order := OldOrder;
    end;
  end
  else
    inherited;
end;

procedure TCRRecordSet.FetchAll;
begin
  while Fetch do;
end;

function TCRRecordSet.CanDisconnect: boolean;
var
  CursorState: TCursorState;
begin
  Assert(FCommand <> nil);
  CursorState := FCommand.GetCursorState;
  Result := (CursorState = csInactive) or (CursorState = csFetched);
end;

function TCRRecordSet.RowsReturn: boolean;
begin
  Result := FCommand.CommandType = ctCursor;
end;


function TCRRecordSet.GetCommand: TCRCommand;
begin
  Result := FCommand;
end;

function TCRRecordSet.GetConnection: TCRConnection;
begin
  Result := FCommand.GetConnection;
end;

procedure TCRRecordSet.SetConnection(Value: TCRConnection);
begin
  FCommand.SetConnection(Value);
end;

procedure TCRRecordSet.SetTransaction(Value: TCRTransaction);
begin
  FCommand.SetTransaction(Value);
end;

procedure TCRRecordSet.SetSQL(const Value: string);
begin
  FCommand.SetSQL(Value);
end;

function TCRRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prFetchAll:
      Value := FFetchAll;
    prExtendedFieldsInfo:
      Value := FExtendedFieldsInfo;
    prDefaultValues:
      Value := FDefaultValues;
    prFieldOrigins:
      Value := FFieldOrigins;
    prReadOnly:
      Value := FReadOnly;
    prFullRefresh:
      Value := FFullRefresh;
    prSetFieldsReadOnly:
      Value := FSetFieldsReadOnly;
    prKeyFields:
      Value := FKeyFields;
  {$IFNDEF LITE}
    prInsertAllSetFields:
      Value := FInsertAllSetFields;
  {$ENDIF}
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prUniDirectional: begin
      FUniDirectional := Value;
      if FUniDirectional then
        FFetchAll := False;
    end;
    prFetchRows:
      if FFetchRows <> Value then  begin
        FreeFetchBuffer;
        FFetchRows := Value;
      end;
    prFetchAll:
      if not FLockFetchAll then
        FFetchAll := Value and not FUniDirectional;
    prLockFetchAll:
      FLockFetchAll := Value;
    prLongStrings:
      FLongStrings := Value;
    prFlatBuffers:
      FFlatBuffers := Value;
    prIndexFieldNames:
      SetIndexFieldNames(Value);
  {$IFDEF HAVE_COMPRESS}
    prCompressBlobMode:
      FCommand.FCompressBlob := TCompressBlobMode(Integer(Value));
  {$ENDIF}
    prReadOnly:
      FReadOnly := Value;
    prExtendedFieldsInfo:
      FExtendedFieldsInfo := Value;
    prDefaultValues:
      FDefaultValues := Value;
    prFieldOrigins:
      FFieldOrigins := Value;
    prRoAfterUpdate:
      ;
    prFullRefresh:
      FFullRefresh := Value;
    prSetFieldsReadOnly:
      FSetFieldsReadOnly := Value;
    prKeyFields: begin
      FKeyFields := Value;
      ClearCachedKeyFieldDescs;
    end;
    prUpdatingTable: begin
      FUpdatingTable := Value;
      ClearCachedFieldDescs;
      if Active then begin
        InitUpdatingTableIdx;
        DetectIdentityField;
      end;
    end;
  {$IFNDEF LITE}
    prEncryptedFields:
      FEncryptedFields := Value;
    prPrefetchedFields:
      FPrefetchedFields := Value;
    prLiveBlockOnSmartFetch:
      FLiveBlockOnSmartFetch := Value;
    prInsertAllSetFields:
      FInsertAllSetFields := Value;
  {$ENDIF}
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

procedure TCRRecordSet.SetComponent(Value: TObject);
begin
  FCommand.Component := Value;
end;

function TCRRecordSet.GetComponent: TObject;
begin
  Result := FCommand.Component;
end;

function TCRRecordSet.IsCaseSensitive: boolean;
begin
  Result := True;
end;

procedure TCRRecordSet.FilterUpdated;
var
  NotFetched: boolean;
begin
  NotFetched := (FCommand.GetCursorState < csFetched) and (FCommand.GetCursorState <> csInactive);
  inherited FilterUpdated;
  FEOF := FEOF and not NotFetched;
end;

{$IFNDEF LITE}
function TCRRecordSet.FieldIsEncrypted(const FieldName: string): boolean;
var
  sl: TStrings;
begin
  Result := False;

  if (FEncryptor = nil) or (FEncryptedFields = '') then
    Exit;

  sl := TStringList.Create;
  try
    FCommand.SQLInfo.NamesToList(FEncryptedFields, sl);
    Result := sl.IndexOf(FieldName) > -1;
  finally
    sl.Free;
  end;
end;

{ Data Type Map }

function TCRRecordSet.CreateDataTypeMap: TCRMapRules;
begin
  Result := FCommand.GetMapRulesClass.Create;
end;

function TCRRecordSet.GetMapRule(Field: TCRFieldDesc): TCRMapRule;
begin
  Result := GetMapRule(Field.Name, Field.DBType, Field.DBLength, Field.DBScale);
end;

function TCRRecordSet.GetMapRule(const FieldName: string; DBType: Word; DBLength, DBScale: Integer): TCRMapRule;
begin
  Result := FDataTypeMap.DetectFieldNameMapRule(FieldName, DBType, DBLength, DBScale);
  if (Result = nil) and (FCommand.FConnection <> nil) then
    Result := FCommand.FConnection.DataTypeMap.DetectFieldNameMapRule(FieldName, DBType, DBLength, DBScale);
  if (Result = nil) then
    Result := FDataTypeMap.DetectDBTypeMapRule(DBType, DBLength, DBScale);
  if (Result = nil) and (FCommand.FConnection <> nil) then
    Result := FCommand.FConnection.DataTypeMap.DetectDBTypeMapRule(DBType, DBLength, DBScale);
end;

function TCRRecordSet.GetMapFetchConverter(DBType: Word; DBLength, DBScale: Integer): TFetchConverter;
begin
  Result := GetMapFetchConverter('', DBType, DBLength, DBScale);
end;

function TCRRecordSet.GetMapFetchConverter(const FieldName: string; DBType: Word; DBLength, DBScale: Integer): TFetchConverter;
var
  Rule: TCRMapRule;
begin
  if FieldIsEncrypted(FieldName) then begin
    Result := nil;
    Exit;
  end;

  Rule := GetMapRule(FieldName, DBType, DBLength, DBScale);

  if Rule <> nil then
    Result := FDataTypeMap.GetConverterManager.DetectFetchConverter(DBType, DBLength, DBScale, Rule.DataType)
  else
    Result := nil;
end;

function TCRRecordSet.GetMapOnDemandConverter(Field: TCRFieldDesc; out MapRule: TCRMapRule): TOnDemandConverter;
begin
  Result := GetMapOnDemandConverter(Field.Name, Field.DataType, Field.DBType, Field.DBLength, Field.DBScale, MapRule);
end;

function TCRRecordSet.GetMapOnDemandConverter(DataType: Word; DBType: Word; DBLength, DBScale: Integer; out MapRule: TCRMapRule): TOnDemandConverter;
begin
  Result := GetMapOnDemandConverter('', DataType, DBType, DBLength, DBScale, MapRule);
end;

function TCRRecordSet.GetMapOnDemandConverter(const FieldName: string; DataType: Word; DBType: Word; DBLength, DBScale: Integer; out MapRule: TCRMapRule): TOnDemandConverter;

  function IsEqualDataType(MapDataType, FieldDataType: Word): boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    case MapDataType of
      dtString:
        Result := FieldDataType in [dtString, dtExtString];
      dtWideString:
        Result := FieldDataType in [dtWideString, dtExtWideString];
      else
        Result := MapDataType = FieldDataType;
    end;
  end;

begin
  MapRule := GetMapRule(FieldName, DBType, DBLength, DBScale);

  if MapRule <> nil then
    if not IsEqualDataType(MapRule.DataType, DataType) or
       ((MapRule.FieldLength <> -1) and (MapRule.FieldLength <> DBLength)) or
       ((MapRule.FieldScale <> -1)  and (MapRule.FieldScale <> DBScale))
    then begin
      case DataType of
        dtVarBytes:
          DataType := dtBytes;
        dtExtVarBytes:
          DataType := dtExtBytes;
      end;

      Result := FDataTypeMap.GetConverterManager.DetectOnDemandConverter(DataType, MapRule.DataType);
    end
    else begin
      MapRule := nil;
      Result := nil;
    end
  else
    Result := nil;
end;
{$ENDIF}

function TCRRecordSet.GetTableInfoClass: TTableInfoClass;
begin
  Result := FCommand.GetTableInfoClass;
end;

procedure TCRRecordSet.CheckIndexFields;
begin
  if FUniDirectional then
    raise Exception.Create(SIndexFieldNamesWithUniDirectional);
end;

procedure TCRRecordSet.SetSortDefaults(SortColumn: TSortColumn);
begin
  inherited;

  if FCommand.FConnection <> nil then
    SortColumn.SortType := FCommand.FConnection.FDefaultSortType;
end;

{ TParamDesc }

constructor TParamDesc.Create;
begin
  inherited;

  FDataType := dtUnknown;
  FIsBound := True;
  FIsNull := True;

  FArraySize := 1;
end;

destructor TParamDesc.Destroy;
begin
{$IFNDEF LITE}
  FEncryptedBlob.Free;
{$ENDIF}

  FreeBuffer;

  inherited;
end;

procedure TParamDesc.Assign(Source: TParamDesc);
begin
  FName := Source.FName;
  FDataType := Source.FDataType;
  FSubDataType := Source.FSubDataType;
  FParamType := Source.FParamType;

  FSize := Source.FSize;
  FIsNull := Source.FIsNull;
  FIsBound := Source.FIsBound;
  FConvertEOL := Source.FConvertEOL;
  FNational := Source.FNational;
end;

{$IFNDEF LITE}

function TParamDesc.EncryptValue(DataType: Word; const Value: Variant): Variant;
var
  CryptType: Word;
  DataBuf: TValueArr;
  DataLen: cardinal;
  s: AnsiString;
  ws: WideString;
begin
  Assert(Assigned(FEncryptor));

  if not (TVarData(Value).VType in [varEmpty, varNull]) then begin
    case DataType of
      dtString, dtExtString: begin
        CryptType := dtString;
      {$IFNDEF NEXTGEN}
        if VarIsStr(Value) then begin
          DataBuf := TVarData(Value).VPointer;
          if DataBuf <> nil then
            DataLen := StrLen(PAnsiChar(DataBuf))
          else
            DataLen := 0;
        end
        else
      {$ENDIF}
        begin
          s := AnsiString(VarToStr(Value));
          DataBuf := PAnsiChar(s);
          DataLen := LengthA(s);
        end;
      end;
      dtWideString, dtExtWideString: begin
        CryptType := dtWideString;
        if VarIsStr(Value) then begin
          DataBuf := TVarData(Value).VPointer;
          if DataBuf <> nil then
            DataLen := StrLenW(PWideChar(DataBuf)) * SizeOf(WideChar)
          else
            DataLen := 0;
        end
        else begin
          ws := VarToWideStr(Value);
          DataBuf := IntPtr(PWideChar(ws));
          DataLen := Length(ws) * SizeOf(WideChar);
        end;
      end;
      dtBytes, dtVarBytes, dtExtVarBytes: begin
        CryptType := dtBytes;
        if VarType(Value) = varArray + varByte then begin
          DataBuf := TVarData(Value).VArray.Data;
          DataLen := TVarData(Value).VArray.Bounds[0].ElementCount;
        end
        else begin
          s := AnsiString(VarToStr(Value));
          DataLen := LengthA(s);
          DataBuf := PAnsiChar(s);
        end;
      end;
      dtBlob, dtMemo, dtWideMemo: begin
        CryptType := dtBytes;
        s := AnsiString(VarToStr(Value));
        DataBuf := PAnsiChar(s);
        DataLen := LengthA(s);
      end;
    else
      raise Exception.CreateFmt(SEncryptionNotSupported, [IntToStr(DataType)]);
    end;

    Result := Unassigned; // Delphi bug
    Result := FEncryptor(DataBuf, CryptType, DataLen);
    if DataLen = 0 then
      Result := Value;
  end
  else
    Result := Value;
end;

function TParamDesc.EncryptObject(DataType: Word; const Value: TSharedObject): TSharedObject;
var
  v: Variant;
  Data: TBytes;
  DataBuf: TValueArr;
  DataLen: Cardinal;
begin
  Assert(Assigned(FEncryptor));

{$IFNDEF VER9P}
  SetLength(Data, 0); // anti-warning
{$ENDIF}

  if Value <> nil then begin
    FEncryptedBlob.Free;
    FEncryptedBlob := TBlob(Value).Clone(False, False);

    case DataType of
      dtBlob, dtMemo, dtWideMemo: begin
        Data := TBlob(Value).AsBytes;
        DataLen := Length(Data);
        if DataLen > 0 then
          DataBuf := @Data[0]
        else
          DataBuf := nil;

        if DataType = dtBlob then
          v := FEncryptor(DataBuf, dtBytes, DataLen)
        else if TBlob(Value).IsUnicode then
          v := FEncryptor(DataBuf, dtWideMemo, DataLen)
        else
          v := FEncryptor(DataBuf, dtMemo, DataLen);

        if DataLen > 0 then begin
          Data := TBytes(v);
          FEncryptedBlob.AsBytes := Data;
        end;
      end;
    else
      raise Exception.CreateFmt(SEncryptionNotSupported, [IntToStr(DataType)]);
    end;

    Result := FEncryptedBlob;
  end
  else
    Result := nil;
end;

{$ENDIF}

procedure TParamDesc.AllocBuffer;
begin
end;

procedure TParamDesc.FreeBuffer;
begin
  if TVarData(FData).VType = varValueArrayRef then begin
    TVarData(FData).VType := varEmpty;
  end;
end;

procedure TParamDesc.CheckIndex(Index: integer);
begin
  if Index < 0 then
    raise ERangeError.Create(SInvalidIndex)
  else if (Index > 0) and (Index >= FArraySize) then
    raise ERangeError.Create(SInvalidIndex);
end;

function TParamDesc.GetItemNull(Index: integer): boolean;
var
  ValuePtr: PVarData;
begin
  if FArraySize > 1 then begin
    ValuePtr := PVarData(GetItemPtr(Index));
    if IsObjectValue and (ValuePtr.VType = varSharedObject) then
      Result := (ValuePtr.VPointer = nil) or TBlob(ValuePtr.VPointer).IsNull
    else
      Result := ValuePtr.VType in [varEmpty, varNull];
  end
  else
    Result := GetNull;
end;

procedure TParamDesc.SetItemNull(Index: integer; Value: boolean);
var
  Obj: TSharedObject;
begin
  if Value then begin
    if FArraySize > 1 then begin
      if IsObjectValue then begin
        Obj := GetItemObject(Index);
        if (Obj <> nil) and (Obj is TBlob) then
          TBlob(Obj).Clear;
      end
      else
        SetItemValue(Index, Unassigned);
    end
    else
      SetNull(Value);
  end;
end;

function TParamDesc.GetItemObject(Index: integer): TSharedObject;
var
  ItemData: PVariant;
begin
  ItemData := GetItemPtr(Index);
  Result := TSharedObject.FromVariant(ItemData^);
end;

procedure TParamDesc.SetItemObject(Index: integer; Value: TSharedObject);
var
  ItemData: PVariant;
  OldValue: TSharedObject;
begin
  CheckIndex(Index);

  ItemData := GetItemPtr(Index);
  OldValue := TSharedObject.FromVariant(ItemData^);
  if Value = OldValue then
    Exit;

  // Decrease RefCount for existing object
  if OldValue <> nil then
    OldValue.Release;

  if Value <> nil then begin
    ItemData^ := Value.ToVariant;
    // Increase RefCount for Value
    Value.AddRef;
  end
  else
    ItemData^ := Null;
end;

function TParamDesc.GetItemValue(Index: integer): Variant;
begin
  Result := GetItemPtr(Index)^;
end;

procedure TParamDesc.SetItemValue(Index: integer; const Value: Variant);
var
  v: PVariant;
begin
  CheckIndex(Index);

  v := GetItemPtr(Index);
  v^ := Unassigned;
{$IFNDEF LITE}
  if Assigned(FEncryptor) then
    v^ := EncryptValue(FDataType, Value)
  else
{$ENDIF}
    v^ := Value;

  if FArraySize <= 1 then
    FIsNull := TVarData(Value).VType in [varEmpty, varNull];
end;

procedure TParamDesc.Clear;
begin
  if FArraySize > 1 then
    SetArraySize(1);

  if FParamObject <> nil then
    TBlob(FParamObject).Clear;
end;

function TParamDesc.GetMinDefaultSize: Integer;
begin
  Result := 1;
end;

function TParamDesc.GetMaxStringSize(Connection: TCRConnection): Integer;
var
  v: Variant;
begin
  Connection.GetProp(prMaxStringSize, v);
  Result := v;
end;

function TParamDesc.GetName: string;
begin
  Result := FName;
end;

procedure TParamDesc.SetName(const Value: string);
begin
  FName := Value;
end;

function TParamDesc.GetDataType: word;
begin
  Result := FDataType;
end;

procedure TParamDesc.SetDataType(Value: word);
begin
  if FDataType <> Value then begin
    if FArraySize > 1 then
      FreeBuffer;
    FDataType := Value;
  end;
end;

function TParamDesc.GetSubDataType: word;
begin
  Result := FSubDataType;
end;

procedure TParamDesc.SetSubDataType(Value: word);
begin
  FSubDataType := Value;
end;

function TParamDesc.GetParamType: TParamDirection;
begin
  Result := FParamType;
end;

procedure TParamDesc.SetParamType(Value: TParamDirection);
begin
  FParamType := Value;
end;

function TParamDesc.GetArraySize: integer;
begin
  Result := FArraySize;
end;

procedure TParamDesc.SetArraySize(Value: integer);
begin
  if Value <> FArraySize then begin
    FreeBuffer;
    FArraySize := Value;
  end;
end;

function TParamDesc.GetSize: integer;
begin
  Result := FSize;
end;

procedure TParamDesc.SetSize(Value: integer);
begin
  if FSize <> Value then begin
    if FArraySize > 1 then
      FreeBuffer;
    FSize := Value;
  end;
end;

function TParamDesc.GetPrecision: integer;
begin
  Result := FPrecision;
end;

procedure TParamDesc.SetPrecision(Value: integer);
begin
  FPrecision := Value;
end;

function TParamDesc.GetScale: integer;
begin
  Result := FScale;
end;

procedure TParamDesc.SetScale(Value: integer);
begin
  FScale := Value;
end;

function TParamDesc.GetValue: Variant;
begin
  Result := FData;
end;

procedure TParamDesc.SetValue(const Value: Variant);
begin
  if TVarData(Value).VType = varValueArrayRef then
    SetValueArr(TVarData(Value).VPointer)
  else begin
  {$IFNDEF LITE}
    if Assigned(FEncryptor) then
      FData := EncryptValue(FDataType, Value)
    else
  {$ENDIF}
      FData := Value;

    FIsNull := TVarData(FData).VType in [varEmpty, varNull];
  end;
end;

procedure TParamDesc.SetValueArr(PValueArr: PVariantArray);
begin
  FreeBuffer;

  FData := Unassigned;
  TVarData(FData).VType := varValueArrayRef;
  TVarData(FData).VPointer := PValueArr;

  FArraySize := Length(PValueArr^);
  FIsNull := False;
end;

procedure TParamDesc.SetObjectArr(PValueArr: PVariantArray);
begin
  SetValueArr(PValueArr);
end;

function TParamDesc.GetItemPtr(Index: integer): PVariant;
begin
  CheckIndex(Index);

  if FArraySize > 1 then
    Result := @PVariantArray(TVarData(FData).VPointer)^[Index]
  else
    Result := @FData;
end;

function TParamDesc.GetNull: boolean;
begin
  Result := FIsNull;
end;


function TParamDesc.IsObjectValue: boolean;
begin
  Result := FDataType in [dtMemo, dtWideMemo, dtBlob, dtObject];
end;

function TParamDesc.GetObject: TSharedObject;
begin
  if TVarData(FData).VType = varSharedObject then
    Result := TVarData(FData).VPointer
  else
    Result := nil;
end;

procedure TParamDesc.SetObject(Value: TSharedObject);
begin
  FData := Unassigned;
  TVarData(FData).VType := varSharedObject;
{$IFNDEF LITE}
  if Assigned(FEncryptor) then
    TVarData(FData).VPointer := EncryptObject(FDataType, Value)
  else
{$ENDIF}
    TVarData(FData).VPointer := Value;
end;

procedure TParamDesc.SetNull(const Value: boolean);
begin
  FIsNull := Value;
  if (TVarData(FData).VType <> varSharedObject) and (FArraySize = 1) then
    if Value then
      FData := Null
    else
      FData := Unassigned;
end;

function TParamDesc.GetIsBound: boolean;
begin
  Result := FIsBound;
end;

procedure TParamDesc.SetIsBound(Value: boolean);
begin
  FIsBound := Value;
end;

function TParamDesc.GetNational: boolean;
begin
  Result := FNational;
end;

procedure TParamDesc.SetNational(Value: boolean);
begin
  FNational := Value;
end;

procedure TParamDesc.SetConvertEOL(const Value: boolean);
begin
  FConvertEOL := Value;
end;

{$IFNDEF LITE}
procedure TParamDesc.SetEncryptor(const Value: TEncryptionMethod);
begin
  FEncryptor := Value;
end;
{$ENDIF}

class function TParamDesc.NeedAssignScalarValues: boolean;
begin
  Result := False;
end;

class function TParamDesc.NeedAssignObjectValues: boolean;
begin
  Result := False;
end;

{ TParamDescs }

function TParamDescs.FindParam(const Name: string): TParamDesc;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (Items[i] <> nil) and SameText(TParamDesc(Items[i]).FName, Name) then begin
      Result := Items[i];
      break;
    end;
end;

function TParamDescs.ParamByName(const Name: string): TParamDesc;
begin
  Result := FindParam(Name);

  if Result = nil then
    Assert(False);
    //raise Exception.Create(Format(SParamNotFound, [Name]));
end;

function TParamDescs.GetItems(Index: integer): TParamDesc;
begin
  Result := TParamDesc(inherited Items[Index]);
end;

{ TDAParamsInfo }

procedure TDAParamInfo.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TDAParamInfo) then begin
    TDAParamInfo(Dest).FieldDesc := FieldDesc;
    TDAParamInfo(Dest).Old := Old;
    TDAParamInfo(Dest).ParamName := ParamName;
    TDAParamInfo(Dest).ParamType := ParamType;

    TDAParamInfo(Dest).ParamRef := ParamRef;
    TDAParamInfo(Dest).SB := SB;
    TDAParamInfo(Dest).StartPosition := StartPosition;
    TDAParamInfo(Dest).EndPosition := EndPosition;
  end
  else
    inherited;
end;

function TDAParamsInfo.GetItem(Index: Integer): TDAParamInfo;
begin
  Result := TDAParamInfo(inherited GetItem(Index));
end;

procedure TDAParamsInfo.SetItem(Index: Integer; Value: TDAParamInfo);
begin
  inherited SetItem(Index, Value);
end;

{ TCRConnections }

function TCRConnections.GetItems(Index: integer): TCRConnection;
begin
  Result := TCRConnection(inherited Items[Index]);
end;

{ TCRTransaction }

constructor TCRTransaction.Create;
begin
  inherited;

  FLock := TCriticalSection.Create;

  FConnections := TCRConnections.Create;
  FNativeTransaction := True;
end;

destructor TCRTransaction.Destroy;
begin
  // Transaction is closed by component layer.
  // Destructor of derived class can free resouces that are need to close transaction
  // before calling inherited.
  // CloseTransaction;

  FConnections.Free;

  FLock.Free;

  inherited;
end;


procedure TCRTransaction.Lock;
begin
  FLock.Enter;
end;

procedure TCRTransaction.Unlock;
begin
  FLock.Leave;
end;

procedure TCRTransaction.CheckInactive;
begin
  if GetInTransaction then
    raise Exception.Create(SInTransaction);
end;

procedure TCRTransaction.CheckActive;
begin
  if not GetInTransaction then
    raise Exception.Create(SNotInTransaction);
end;

function TCRTransaction.AddConnection(Connection: TCRConnection): boolean;
begin
  if FConnections.IndexOf(Connection) = -1 then begin
    FConnections.Add(Connection);
    Result := True;
  end
  else
    Result := False;
end;

function TCRTransaction.RemoveConnection(Connection: TCRConnection): boolean;
begin
  if FConnections.IndexOf(Connection) >= 0 then begin
    FConnections.Remove(Connection);
    Result := True;
  end
  else
    Result := False;
end;

function TCRTransaction.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prIsolationLevel:
      FIsolationLevel := TCRIsolationLevel(Value);
    prTransactionReadOnly:
      FReadOnly := Value;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRTransaction.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prIsolationLevel:
      Value := Variant(FIsolationLevel);
    prTransactionReadOnly:
      Value := FReadOnly;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

procedure TCRTransaction.StartTransaction;
begin
  Assert(False, 'Must be overrided');
end;

procedure TCRTransaction.Commit;
begin
  Assert(False, 'Must be overrided');
end;

procedure TCRTransaction.Rollback;
begin
  Assert(False, 'Must be overrided');
end;

function TCRTransaction.GetMultiTransactionID: Int64;
begin
  Result := 0;
end;

procedure TCRTransaction.CommitRetaining;
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRTransaction.RollbackRetaining;
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRTransaction.Savepoint(const Name: string);
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRTransaction.ReleaseSavepoint(const Name: string);
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRTransaction.RollbackToSavepoint(const Name: string);
begin
  raise Exception.Create(SOperationNotSupported);
end;

function TCRTransaction.GetInTransaction: boolean;
begin
  Result := FActive;
end;

function TCRTransaction.DetectInTransaction(CanActivate: boolean): boolean;
begin
  Result := GetInTransaction;
end;

procedure TCRTransaction.AssignConnect(Source: TCRTransaction);
begin
  FActive := Source.FActive;
  if FActive then
    FNativeTransaction := False;
end;

procedure TCRTransaction.Reset;
begin
  FActive := False;
end;

function TCRTransaction.CanRestoreAfterFailover: boolean;
begin
  Result := False;
end;

{ TMTSTransaction }

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}

destructor TMTSTransaction.Destroy;
begin
  FMTSGC := nil;

  inherited;
end;

procedure TMTSTransaction.StartMTSTransaction;
var
  MTSIsolationLevel: integer;
  MTSFlags: integer;
  MTSTransaction: ICRTransaction;
  MTSOptions: ICRTransactionOptions;
begin
{$IFDEF FPC}
  raise Exception.Create(SFPCNotSupported);
{$ENDIF}

  InitMSDTC;

  if FMTSGC = nil then
    OleCheck(DtcGetTransactionManagerEx(nil, nil, IID_ITransactionDispenser, 0, nil, FMTSGC));

  case FIsolationLevel of
    ilReadUnCommitted:
      MTSIsolationLevel := ISOLATIONLEVEL_READUNCOMMITTED;
    ilReadCommitted:
      MTSIsolationLevel := ISOLATIONLEVEL_READCOMMITTED;
    ilRepeatableRead:
      MTSIsolationLevel := ISOLATIONLEVEL_REPEATABLEREAD;
    ilIsolated:
      MTSIsolationLevel := ISOLATIONLEVEL_SERIALIZABLE;
  else
    raise Exception.Create(SIsolationLevelNotSupportedWithMTS);
  end;
  MTSFlags := 0;
  MTSOptions := nil;

  FMTSGC.BeginTransaction(nil, MTSIsolationLevel, MTSFlags, MTSOptions, MTSTransaction);
  Supports(MTSTransaction, ICRTransactionSC, FMTSTrans);
end;

procedure TMTSTransaction.CompleteMTSTransaction(Commit: boolean);
var
  i: integer;
begin
  Assert(FMTSTrans <> nil);
  FActive := False;
  try
    if Commit then
      FMTSTrans.Commit(False, 0, 0)
    else
      FMTSTrans.Abort(nil, False, False);
  finally
    FMTSTrans := nil;
  end;

  for i := 0 to FConnections.Count - 1 do //needunenlist
    UnEnlistLink(FConnections[i]);
end;

procedure TMTSTransaction.EnlistLink(Connection: TCRConnection);
begin
  Connection.Enlist(Self);
end;

procedure TMTSTransaction.UnEnlistLink(Connection: TCRConnection);
begin
  Connection.UnEnlist(Self);
end;

function TMTSTransaction.AddConnection(Connection: TCRConnection): boolean;
begin
  Result := inherited AddConnection(Connection);
  if Result and FActive then
    EnlistLink(Connection);
end;

function TMTSTransaction.RemoveConnection(Connection: TCRConnection): boolean;
begin
  Result := inherited RemoveConnection(Connection);
  if Result and FActive then
    UnEnlistLink(Connection);
end;

procedure TMTSTransaction.StartTransaction;
var
  i: integer;
begin
  CheckInactive;

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].GetConnected then
      raise Exception.Create(SConnectionInTransactionNotActive);

  StartMTSTransaction;

  FActive := True;
  try
    for i := 0 to FConnections.Count - 1 do
      EnlistLink(FConnections[i]);
  except
    Rollback;
    raise;
  end;
end;

procedure TMTSTransaction.Commit;
begin
  CheckActive;
  CompleteMTSTransaction(True);
end;

procedure TMTSTransaction.Rollback;
begin
  CheckActive;
  CompleteMTSTransaction(False);
end;

procedure TMTSTransaction.Savepoint(const Name: string);
var
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    FConnections[i].FInternalTransaction.Savepoint(Name);
  end;
end;

procedure TMTSTransaction.ReleaseSavepoint(const Name: string);
var
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    FConnections[i].FInternalTransaction.ReleaseSavepoint(Name);
  end;
end;

procedure TMTSTransaction.RollbackToSavepoint(const Name: string);
var
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    FConnections[i].FInternalTransaction.RollbackToSavepoint(Name);
  end;
end;

{$ENDIF}
{$ENDIF}

{ TCRMetaData }

constructor TCRMetaData.Create;
begin
  inherited;

  FMemData := TMemData.Create;
  FMemData.AutoInitFields := False;
  FRecordSet := CreateRecordSet;
{$IFNDEF LITE}
  FRecordSet.DataTypeMap.Enabled := False;
{$ENDIF}
  FRecordSet.SetProp(prFetchAll, True);
  FRecordSet.SetProp(prFlatBuffers, False);

  FMemDataHelper := TDataHelper.Create(FMemData);
  FRecordSetHelper := TDataHelper.Create(FRecordSet);

  FOperator := '=';
end;

destructor TCRMetaData.Destroy;
begin
  FMemDataHelper.Free;
  FRecordSetHelper.Free;
  FRecordSet.Free;
  FMemData.Free;

  inherited;
end;

function TCRMetaData.GetMetaData(Connection: TCRConnection; Transaction: TCRTransaction;
  const MetaDataKind: string; Restrictions: TStrings): TData;
begin
  FMemData.Close;
  FRecordSet.Close;
  FRecordSet.SetConnection(Connection);
  FRecordSet.SetTransaction(Transaction);
  Result := InternalGetMetaData(LowerCase(Trim(MetaDataKind)), Restrictions);
end;

function TCRMetaData.InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData;
begin
  // kinds required for all products
  if (MetaDataKind = '') or (MetaDataKind = 'metadatakinds') then
    Result := GetMetaDataKinds
  else
  if MetaDataKind = 'restrictions' then
    Result := GetRestrictions(Restrictions)
  else
  if MetaDataKind = 'tables' then
    Result := GetTables(Restrictions)
  else
  if MetaDataKind = 'columns' then
    Result := GetColumns(Restrictions)
  else
  if MetaDataKind = 'procedures' then
    Result := GetProcedures(Restrictions)
  else
  if MetaDataKind = 'procedureparameters' then
    Result := GetProcedureParameters(Restrictions)
  else
  if MetaDataKind = 'indexes' then
    Result := GetIndexes(Restrictions)
  else
  if MetaDataKind = 'indexcolumns' then
    Result := GetIndexColumns(Restrictions)
  else
  if MetaDataKind = 'constraints' then
    Result := GetConstraints(Restrictions)
  else
  if MetaDataKind = 'constraintcolumns' then
    Result := GetConstraintColumns(Restrictions)

  // the following kinds are reqiured for DBXpress only
  else
  if MetaDataKind = 'databases' then
    Result := GetDatabases(Restrictions)
  else
  if MetaDataKind = 'datatypes' then
    Result := GetDatatypes(Restrictions)
  else
  if MetaDataKind = 'users' then
    Result := GetUsers(Restrictions)
  else
  if MetaDataKind = 'roles' then
    Result := GetRoles(Restrictions)
  else
  if MetaDataKind = 'userdefinedtypes' then
    Result := GetUDTs(Restrictions)
  else
  if MetaDataKind = 'packages' then
    Result := GetPackages(Restrictions)
  else
    raise Exception.Create(SUnsupportedMetaDataKind);
end;

procedure TCRMetaData.GetMetaDataKindsList(List: TStrings);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    InternalGetMetaDataKindsList(StrList);
    List.Assign(StrList);
  finally
    StrList.Free;
  end;
end;

procedure TCRMetaData.GetRestrictionsList(List: TStrings; const MetaDataKind: string);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    InternalGetRestrictionsList(StrList, LowerCase(Trim(MetaDataKind)));
    List.Assign(StrList);
  finally
    StrList.Free;
  end;
end;

procedure TCRMetaData.AddField(const AName: string; AType: integer; ALength: integer = -1);
var
  Field: TFieldDesc;
  Size: integer;
begin
  Field := TFieldDesc.Create(TRecordSetClass(FRecordSet.ClassType));
  Field.Name := AName;
  Field.ActualName := Field.Name;
  Field.DataType := AType;
  case AType of
    dtInt8, dtUInt8: Size := SizeOf(Byte);
    dtInt16, dtUInt16: Size := SizeOf(Word);
    dtInt32: Size := SizeOf(Integer);
    dtInt64: Size := SizeOf(Int64);
    dtFloat: Size := SizeOf(Double);
    dtBoolean: Size := SizeOf(Boolean);
    dtDateTime: Size := SizeOf(TDateTime);
    dtString: Size := ALength + 1;
    dtWideString: Size := (ALength + 1) * 2;
  else
    Size := SizeOf(IntPtr);
  end;
  Field.Size := Size;
  if ALength > 0 then
    Field.Length := ALength;

  Field.FieldNo := FMemData.Fields.Add(Field) + 1;
end;

procedure TCRMetaData.CopyRecord(const SourceIndices, DestIndices: array of integer);
var
  i: integer;
begin
  for i := 0 to High(SourceIndices) do
    FMemDataHelper.FieldValues[DestIndices[i]] := FRecordSetHelper.FieldValues[SourceIndices[i]];

{$IFDEF DBX_METADATA}
  FMemDataHelper.FieldValues[1] := FRecordSet.RecordNo;
{$ENDIF}
end;

function TCRMetaData.ParseTypes(const ObjectTypes: string; AllTypes: array of string): TBooleanArray;
var
  TypeName, RestTypes: string;
  i, p: integer;
begin
  SetLength(Result, High(AllTypes) + 1);

  if ObjectTypes <> '' then begin
    for i := 0 to High(AllTypes) do
      Result[i] := False;
    RestTypes := ObjectTypes;
    repeat
      p := Pos(',', RestTypes);
      if p > 0 then
        TypeName := Copy(RestTypes, 1, p - 1)
      else
        TypeName := RestTypes;
      TypeName := AnsiUpperCase(Trim(TypeName));
      for i := 0 to High(AllTypes) do
        if TypeName = AnsiUpperCase(AllTypes[i]) then begin
          Result[i] := True;
          break;
        end;
      RestTypes := Copy(RestTypes, p + 1, Length(RestTypes) - p);
    until p = 0;
  end
  else
    for i := 0 to High(AllTypes) do
      Result[i] := True;
end;

procedure TCRMetaData.ParseTypes(const ObjectTypes: string; TypesList: TStringList);
var
  Types, S: string;
  p: integer;
begin
  TypesList.Clear;
  TypesList.Sorted := True;
  TypesList.Duplicates := dupIgnore;
  Types := ObjectTypes;
  repeat
    p := Pos(',', Types);
    if p > 0 then begin
      S := Trim(Copy(Types, 1, p - 1));
      Types := Copy(Types, p + 1, MaxInt);
    end
    else
      S := Trim(Types);
    if S <> '' then
      TypesList.Add(AnsiUpperCase(S));
  until p = 0;

  if TypesList.Count = 0 then
    TypesList.Add('');
end;

procedure TCRMetaData.AddWhere(var WhereClause: string; const Name, Value: string;
  AddEmpty: boolean = False);
var
  NormValue: string;
begin
  if AddEmpty or (Value <> '') then begin
    NormValue := FRecordSet.GetCommand.SQLInfo.NormalizeName(Value, False, True);
    if AddEmpty or (NormValue <> '') then begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      if Value <> '' then
        WhereClause := WhereClause + Name + ' ' + FOperator + ' ' + AnsiQuotedStr(NormValue, '''')
      else
        WhereClause := WhereClause + Name + ' IS NULL';
    end;
  end;
end;

procedure TCRMetaData.CreateMetaDataKindsFields;
begin
  FMemData.Fields.Clear;
  AddField('METADATA_NAME', dtString, 50);
  FMemData.InitFields;
end;

procedure TCRMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  List.Clear;
  List.Sorted := False;
  List.Add('MetaDataKinds');
  List.Add('Restrictions');
  List.Add('Tables');
  List.Add('Columns');
  List.Add('Procedures');
  List.Add('ProcedureParameters');
  List.Add('Indexes');
  List.Add('IndexColumns');
  List.Add('Constraints');
  List.Add('ConstraintColumns');
  List.Sorted := True;
end;

function TCRMetaData.GetMetaDataKinds: TData;
var
  List: TStringList;
  i: integer;
begin
  List := TStringList.Create;
  try
    GetMetaDataKindsList(List);
    CreateMetaDataKindsFields;
    FMemData.Open;
    Result := FMemData;
    FMemDataHelper.AllocBuffer;
    for i := 0 to List.Count - 1 do begin
      FMemDataHelper.InitRecord;
      FMemDataHelper.FieldValues[1] := List[i];
      FMemDataHelper.AppendRecord;
    end;
    FMemData.SetToBegin;
  finally
    List.Free;
  end;
end;

procedure TCRMetaData.CreateRestrictionsFields;
begin
  FMemData.Fields.Clear;
  AddField('METADATA_NAME', dtString, 50);
  AddField('RESTRICTION_NAME', dtString, 50);
  FMemData.InitFields;
end;

procedure TCRMetaData.InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string);
begin
  List.Clear;

  if MetaDataKind = 'restrictions' then begin
    List.Add('METADATA_NAME');
  end
  else
  if MetaDataKind = 'tables' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('TABLE_TYPE');
  end
  else
  if MetaDataKind = 'columns' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('COLUMN_NAME');
  end
  else
  if MetaDataKind = 'procedures' then begin
    List.Add('PROCEDURE_CATALOG');
    List.Add('PROCEDURE_SCHEMA');
    List.Add('PROCEDURE_NAME');
    List.Add('PROCEDURE_TYPE');
  end
  else
  if MetaDataKind = 'procedureparameters' then begin
    List.Add('PROCEDURE_CATALOG');
    List.Add('PROCEDURE_SCHEMA');
    List.Add('PROCEDURE_NAME');
    List.Add('PARAMETER_NAME');
  end
  else
  if MetaDataKind = 'indexes' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('INDEX_NAME');
  end
  else
  if MetaDataKind = 'indexcolumns' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('INDEX_NAME');
  end
  else
  if MetaDataKind = 'constraints' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('CONSTRAINT_NAME');
    List.Add('CONSTRAINT_TYPE');
  end
  else
  if MetaDataKind = 'constraintcolumns' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('CONSTRAINT_NAME');
    List.Add('COLUMN_NAME');
  end;
end;

function TCRMetaData.GetRestrictions(Restrictions: TStrings): TData;
var
  MetaInfoName: string;
  MetaDataKinds, RestrList: TStringList;
  i, j: integer;
begin
  MetaInfoName := LowerCase(Trim(Restrictions.Values['METAINFO_NAME']));
  CreateRestrictionsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  RestrList := TStringList.Create;
  try
    if MetaInfoName <> '' then begin
      GetRestrictionsList(RestrList, MetaInfoName);
      for j := 0 to RestrList.Count - 1 do begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[1] := MetaInfoName;
        FMemDataHelper.FieldValues[2] := RestrList[j];
        FMemDataHelper.AppendRecord;
      end;
    end
    else begin
      MetaDataKinds := TStringList.Create;
      try
        GetMetaDataKindsList(MetaDataKinds);
        for i := 0 to MetaDataKinds.Count - 1 do begin
          GetRestrictionsList(RestrList, LowerCase(MetaDataKinds[i]));

          for j := 0 to RestrList.Count - 1 do begin
            FMemDataHelper.InitRecord;
            FMemDataHelper.FieldValues[1] := MetaDataKinds[i];
            FMemDataHelper.FieldValues[2] := RestrList[j];
            FMemDataHelper.AppendRecord;
          end;
        end;
      finally
        MetaDataKinds.Free;
      end;
    end;
  finally
    RestrList.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TCRMetaData.CreateTablesFields;
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
{$IFNDEF DBX_METADATA}
  AddField('TABLE_TYPE', dtString, 50);
{$ELSE}
  AddField('TABLE_TYPE', dtInt32);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateColumnsFields;
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

procedure TCRMetaData.CreateProceduresFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
  AddField('PROC_NAME', dtString, 100);
  AddField('PROC_TYPE', dtInt32);
  AddField('IN_PARAMS', dtInt32);
  AddField('OUT_PARAMS', dtInt32);
{$ELSE}
  AddField('PROCEDURE_CATALOG', dtString, 100);
  AddField('PROCEDURE_SCHEMA', dtString, 100);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PROCEDURE_TYPE', dtString, 50);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateProcedureParametersFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
  AddField('PROC_NAME', dtString, 100);
  AddField('PARAM_NAME', dtString, 100);
  AddField('PARAM_POSITION', dtInt32);
  AddField('PARAM_TYPE', dtInt32);
  AddField('PARAM_DATATYPE', dtInt32);
  AddField('PARAM_TYPENAME', dtString, 50);
  AddField('PARAM_SUBTYPE', dtInt32);
  AddField('PARAM_LENGTH', dtInt32);
  AddField('PARAM_PRECISION', dtInt32);
  AddField('PARAM_SCALE', dtInt32);
  AddField('PARAM_NULLABLE', dtInt32);
{$ELSE}
  AddField('PROCEDURE_CATALOG', dtString, 100);
  AddField('PROCEDURE_SCHEMA', dtString, 100);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PARAMETER_NAME', dtString, 100);
  AddField('POSITION', dtInt32);
  AddField('DIRECTION', dtString, 10);
  AddField('DATA_TYPE', dtString, 50);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateIndexesFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 100);
  AddField('INDEX_SCHEMA', dtString, 100);
  AddField('INDEX_NAME', dtString, 100);
  AddField('UNIQUE', dtInt32);
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateIndexColumnsFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
{$ELSE}
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 100);
  AddField('INDEX_SCHEMA', dtString, 100);
{$ENDIF}
  AddField('INDEX_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('COLUMN_POSITION', dtInt32);
{$IFDEF DBX_METADATA}
  AddField('PKEY_NAME', dtString, 100);
  AddField('INDEX_TYPE', dtInt32);
  AddField('SORT_ORDER', dtString, 1);
  AddField('FILTER', dtString, 6);
{$ELSE}
  AddField('SORT_ORDER', dtString, 4);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateConstraintsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('CONSTRAINT_NAME', dtString, 100);
  AddField('CONSTRAINT_TYPE', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 100);
  AddField('INDEX_SCHEMA', dtString, 100);
  AddField('INDEX_NAME', dtString, 100);
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateConstraintColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('CONSTRAINT_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('COLUMN_POSITION', dtInt32);
  FMemData.InitFields;
end;

{$IFDEF DBX_METADATA}
procedure TCRMetaData.CreateObjectListFields;
begin
  FMemData.Fields.Clear;
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
  AddField('OBJECT_NAME', dtString, 100);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TCRMetaData.CreateDatabasesFields;
begin
  FMemData.Fields.Clear;
  AddField('DATABASE_NAME', dtString, 100);
  FMemData.InitFields;
end;

function TCRMetaData.GetConstraintColumns(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TCRMetaData.GetDatabases(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TCRMetaData.GetDataTypes(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TCRMetaData.GetUsers(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TCRMetaData.GetRoles(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TCRMetaData.GetUDTs(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TCRMetaData.GetPackages(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

{ TDataHelper }

constructor TDataHelper.Create(Data: TData);
begin
  inherited Create;

  FData := Data;
end;

destructor TDataHelper.Destroy;
begin
  FreeBuffer;

  inherited;
end;

procedure TDataHelper.AllocBuffer;
begin
  FreeBuffer;
  FData.AllocRecBuf(FRecBuf);
end;

procedure TDataHelper.FreeBuffer;
begin
  if FRecBuf <> nil then begin
    Marshal.FreeHGlobal(FRecBuf);
    FRecBuf := nil;
  end;
end;

procedure TDataHelper.InitRecord;
begin
  FData.InitRecord(FRecBuf);
  if FData.HasComplexFields then
    FData.CreateComplexFields(FRecBuf, True);
end;

procedure TDataHelper.AppendRecord;
begin
  FData.AppendRecord(FRecBuf);
end;

function TDataHelper.NextRecord: boolean;
begin
  FData.GetNextRecord(FRecBuf);
  Result := not FData.Eof;
end;

function TDataHelper.GetFieldValue(Index: integer): variant;
begin
  FData.GetFieldAsVariant(FData.Fields[Index - 1], FRecBuf, Result);
end;

procedure TDataHelper.SetFieldValue(Index: integer; const Value: variant);
begin
  FData.PutFieldAsVariant(FData.Fields[Index - 1], FRecBuf, Value);
end;

{ TCRLoaderColumn }

constructor TCRLoaderColumn.Create;
begin
  inherited;
  FActualFieldNo := -1;
end;

procedure TCRLoaderColumn.UpdateDataType(Value: word);
begin
  FDataType := Value;
end;

{ TCRLoaderColumns }

function TCRLoaderColumns.GetColumnIndexByName(const Name: string): Integer;
var
  i: Integer;
  ColumnNameUpper: string;
begin
  Result := -1;

  ColumnNameUpper := AnsiUpperCase(Name);
  for i := 0 to Count - 1 do
    if AnsiUpperCase(Items[i].Name) = ColumnNameUpper then begin
      Result := i;
      Exit;
    end;
end;

function TCRLoaderColumns.FindColumnByName(const Name: string): TCRLoaderColumn;
var
  ColumnIndex: integer;
begin
  ColumnIndex := GetColumnIndexByName(Name);
  if ColumnIndex <> -1 then
    Result := Items[ColumnIndex]
  else
    Result := nil;
end;


function TCRLoaderColumns.GetColumnByName(const Name: string): TCRLoaderColumn;
var
  ColumnIndex: integer;
begin
  ColumnIndex := GetColumnIndexByName(Name);
  if ColumnIndex <> -1 then
    Result := Items[ColumnIndex]
  else
    raise Exception.Create('Invalid column name');
end;

function TCRLoaderColumns.GetColumn(Index: integer): TCRLoaderColumn;
begin
  Result := TCRLoaderColumn(inherited Items[Index]);
end;

procedure TCRLoaderColumns.SetColumn(Index: integer; Value: TCRLoaderColumn);
begin
  inherited Items[Index] := Value;
end;

{ TCRLoader }

constructor TCRLoader.Create;
begin
  inherited;

  FSkipReadOnlyFieldDescs := True;
  FColumns := TCRLoaderColumns.Create;
  FObjectReleaseNeeded := False;
  FUseBlankValues := True;
end;

destructor TCRLoader.Destroy;
begin
  FColumns.Free;

  inherited;
end;

function TCRLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prQuoteNames:
      FQuoteNames := Value;
    prUseBlankValues:
      FUseBlankValues := Value;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prQuoteNames:
      Value := FQuoteNames;
    prUseBlankValues:
      Value := FUseBlankValues;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

procedure TCRLoader.DoPutColumnData(Col: integer; Row: integer; const Value: variant);
begin
end;

procedure TCRLoader.DoAddObjectRef(const Value: variant);
var
  Obj: TSharedObject;
begin
  if (VarType(Value) = varSharedObject) then begin
    Obj := TSharedObject.FromVariant(Value);
    Obj.AddRef;
    FObjectReleaseNeeded := True;
  end;
end;

procedure TCRLoader.DoReleaseObjectRef(const Value: variant);
var
  Obj: TSharedObject;
begin
  if (VarType(Value) = varSharedObject) then begin
    Obj := TSharedObject.FromVariant(Value);
    Obj.Release;
  end;
end;

function TCRLoader.IsPutColumnDataAllowed: boolean;
begin
  Result := True;
end;

procedure TCRLoader.PutColumnData(Col: integer; Row: integer; const Value: variant);
begin
  if Col >= FColumns.Count then
    raise Exception.Create('Invalid column number');

  if (Row < FLastRow + 1) or (Row < 1) or (Row > FLastRow + 2) then
    raise Exception.Create('Invalid row number');

  if not IsPutColumnDataAllowed then
    raise Exception.Create('The PutColumnData method must be called from the OnPutData event only');

  FLastRow := Row - 1;

  DoPutColumnData(Col, Row, Value);
end;

procedure TCRLoader.CheckTableName;
begin
  if Trim(FTableName) = '' then
    raise Exception.Create(STableNameNotDefined);
end;

procedure TCRLoader.DoPrepare;
begin
end;

procedure TCRLoader.Prepare;
begin
  FObjectReleaseNeeded := False;

  CheckTableName;
  Reset;

  DoPrepare;
end;

procedure TCRLoader.Prepare(RecordCount: integer);
begin
  Prepare;
end;

procedure TCRLoader.Finish;
begin
end;

procedure TCRLoader.Reset;
begin
  FLastRow := -1;
  FLoadedRows := 0;
end;

procedure TCRLoader.DoLoad;
begin
end;

procedure TCRLoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  Column.ActualFieldNo := FieldDesc.ActualFieldNo;
  Column.Name := FieldDesc.Name;
  Column.DataType := FieldDesc.DataType;
  Column.SubDataType := FieldDesc.SubDataType;
  case FieldDesc.DataType of
    dtInt8, dtUInt8, dtSmallint, dtInteger, dtInt64, dtWord, dtLongword, dtUInt64,
    dtFloat, dtCurrency, dtSingle,
    dtBCD, dtFMTBcd: begin
      Column.Precision := FieldDesc.Length;
      Column.Scale := FieldDesc.Scale;
    end;
  else
    Column.Size := FieldDesc.Length;
    Column.DataSize := FieldDesc.Size;
  end;
end;

procedure TCRLoader.CreateColumns;
var
  RecordSet: TCRRecordSet;
  FieldDesc: TFieldDesc;
  Col: TCRLoaderColumn;
  i: integer;
begin
  CheckTableName;
  FColumns.Clear;
  RecordSet := FConnection.GetRecordSetClass.Create;
  RecordSet.SetConnection(FConnection);
  RecordSet.SetTransaction(FTransaction);
  RecordSet.SetProp(prFetchAll, True);
  RecordSet.SetSQL('SELECT * FROM ' + FTableName + ' WHERE 1=0'); // CR-M15322
  try
    RecordSet.Open;
    for i := 0 to RecordSet.Fields.Count - 1 do begin
      FieldDesc := RecordSet.Fields[i];
      if not (FieldDesc.ReadOnly and FSkipReadOnlyFieldDescs) then begin
        Col := GetColumnClass.Create;
        FillColumn(Col, FieldDesc);
        FColumns.Add(Col);
      end;
    end;
    RecordSet.Close;
  finally
    RecordSet.Free;
  end;
end;

procedure TCRLoader.CheckColumnsInfo;
var
  RecordSet: TCRRecordSet;
  FieldDesc: TFieldDesc;
  Col: TCRLoaderColumn;
  i: integer;
begin
  CheckTableName;
  RecordSet := FConnection.GetRecordSetClass.Create;
  RecordSet.SetConnection(FConnection);
  RecordSet.SetTransaction(FTransaction);
  RecordSet.SetProp(prFetchAll, True);
  RecordSet.SetSQL('SELECT * FROM ' + FTableName + ' WHERE 1=0');
  try
    RecordSet.Open;
    for i := 0 to FColumns.Count - 1 do begin
      Col := FColumns[i];
      if Col.ActualFieldNo < 0 then begin
        FieldDesc := RecordSet.Fields.FindField(Col.Name);
        if FieldDesc <> nil then
          Col.ActualFieldNo := FieldDesc.ActualFieldNo;
      end;
    end;
    RecordSet.Close;
  finally
    RecordSet.Free;
  end;
end;

procedure TCRLoader.DiscardRow;
begin
  if FLastRow >= FLoadedRows then
    Dec(FLastRow);
end;

class function TCRLoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TCRLoaderColumn;
end;

procedure TCRLoader.SetConnection(Value: TCRConnection);
begin
  FConnection := Value;
end;

procedure TCRLoader.SetTransaction(Value: TCRTransaction);
begin
  FTransaction := Value;
end;

{ TCRSimpleLoader }

constructor TCRSimpleLoader.Create;
begin
  inherited;

  CreateCommand;
  FCommand.SetProp(prDisableParamScan, True);
end;

destructor TCRSimpleLoader.Destroy;
begin
  FCommand.Free;

  inherited;
end;

procedure TCRSimpleLoader.Reset;
begin
  inherited;

  FCommand.Params.Clear;
end;

procedure TCRSimpleLoader.DoPrepare;
var
  i: integer;
  Col: TCRLoaderColumn;
  Param: TParamDesc;
  InsertSB: StringBuilder;
begin
  FCommand.Unprepare;
  FCommand.SetConnection(FConnection);
  InsertSB := StringBuilder.Create(1024);
  try
    InsertSB.Append('INSERT INTO ' + FCommand.SQLInfo.NormalizeName(FTableName) + '(');
    for i := 0 to Columns.Count - 1 do begin
      Col := Columns[i];
      if i > 0 then
        InsertSB.Append(',');
      InsertSB.Append(FCommand.SQLInfo.NormalizeName(Col.Name, QuoteNames));
    end;
    InsertSB.Append(') VALUES (');

    for i := 0 to Columns.Count - 1 do begin
      Col := Columns[i];
      if FCommand.Params.Count <= i then begin
        Param := FCommand.GetParamDescClass.Create;
        Param.SetDataType(Col.DataType);
        Param.SetSize(Col.Size);
        Param.SetParamType(pdInput);
        FCommand.Params.Add(Param);
      end;

      if i > 0 then
        InsertSB.Append(',?')
      else
        InsertSB.Append('?');
    end;
    InsertSB.Append(')');

    FCommand.SetSQL(InsertSB.ToString);
  finally
    InsertSB.Free;
  end;
  FCommand.Prepare;
end;

function TCRSimpleLoader.IsPutColumnDataAllowed: boolean;
begin
  Result := FCommand.SQL <> '';
end;

procedure TCRSimpleLoader.DoPutColumnData(Col: integer; Row: integer; const Value: variant);
var
  Param: TParamDesc;
begin
  Param := FCommand.Params[Col];
  Param.SetValue(Value);
  DoAddObjectRef(Value);
end;

procedure TCRSimpleLoader.PutColumnData(Col, Row: integer; const Value: variant);
begin
  if (FLastRow <> -1) and (Row > FLastRow + 1) then
    DoLoadRow;

  inherited;
end;

procedure TCRSimpleLoader.DoAfterLoadRow;
var
  i: integer;
begin
  if FObjectReleaseNeeded then
    for i := 0 to FCommand.Params.Count - 1 do
      DoReleaseObjectRef(FCommand.Params[i].Value);

  if FUseBlankValues then
    for i := 0 to FCommand.Params.Count - 1 do
      FCommand.Params[i].SetNull(True);
end;

procedure TCRSimpleLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then
    DoLoadRow;
end;

procedure TCRSimpleLoader.DoLoadRow;
begin
  FCommand.Execute;
  FLoadedRows := FLastRow + 1;

  DoAfterLoadRow;
end;

procedure TCRSimpleLoader.Finish;
begin
  FCommand.Unprepare;
  Reset;

  inherited;
end;

{ TCRAlerter }

constructor TCRAlerter.Create;
begin
  inherited;

  FEventNames := TStringList.Create;
end;

destructor TCRAlerter.Destroy;
begin
  FEventNames.Free;

  inherited;
end;

function TCRAlerter.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      ;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRAlerter.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TCRAlerter.IsActive: boolean;
begin
  Result := FActive;
end;

procedure TCRAlerter.SetConnection(Value: TCRConnection);
begin
  FConnection := Value;
end;

procedure TCRAlerter.SetTransaction(Value: TCRTransaction);
begin
  FTransaction := Value;
end;

initialization
  DefaultSQLInfo := TSQLInfo.Create(nil);

finalization
  DefaultSQLInfo.Free;

end.
