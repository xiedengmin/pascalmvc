
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 2006-2021 Devart. All right reserved.
//  InterBase Classes
//////////////////////////////////////////////////

{$I IbDac.inc}
unit IBCClassesUni;

{$J+}
{$WARN SYMBOL_PLATFORM OFF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, SyncObjs, Variants, FMTBcd,
{$IFDEF VER10}
  WideStrUtils,
{$ENDIF}
  CLRClasses, CRFunctions, MemData, MemUtils, CRTypes, CRParser, CRAccess, CRVio,
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  IBCCall, IBCError;
{$ELSE}
  IBCCallUni, IBCErrorUni;
{$ENDIF}

const
  dtDbKey              = 101;

  LocalBufferLen       = 1024;
  HugeLocalBufferLen   = LocalBufferLen * 20;

  IBC_MAX_EVENTS       = 15;
  IBC_MAX_EVENT_LENGTH = 64;

  MaxSQLLength         = 32768;

  DefValIPVer = ivIPBoth;

const
  SSLParamNames : array[0..4] of string = ('serverPublicFile', 'serverPublicPath',
    'clientCertFile', 'clientPassPhraseFile', 'clientPassPhrase');

type
  TXSQLVARType = (vtGDS, vtGDS7);
  _TIBCProtocol = (_TCP, _NetBEUI, _SPX, _Local);
  _TStringArray = array of string;
  TInt16Array = array of Int16;

  TGDSCommand = class;
  TGDSTransaction = class;
  TGDSConnection = class;
  TIBCBlob = class;

  TIBCBlobMode =(bmStream, bmValue, bmNull);
  TGetBlobData = procedure(const Param: TParamDesc; out Data: Pointer; out Size: word; out WriteMode: TIBCBlobMode) of object;

  TGDSSSLOptions = class(TPersistent)
  private
    FEnabled: boolean;
    FServerPublicFile: string;
    FServerPublicPath: string;
    FClientCertFile: string;
    FClientPassPhraseFile: string;
    FClientPassPhrase: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function Equals(SSLOptions: TGDSSSLOptions): boolean; reintroduce;
    function GetSSLString: string;
  published
    property Enabled: boolean read FEnabled write FEnabled;
    property ServerPublicFile: string read FServerPublicFile write FServerPublicFile;
    property ServerPublicPath: string read FServerPublicPath write FServerPublicPath;
    property ClientCertFile: string read FClientCertFile write FClientCertFile;
    property ClientPassPhraseFile: string read FClientPassPhraseFile write FClientPassPhraseFile;
    property ClientPassPhrase: string read FClientPassPhrase write FClientPassPhrase;
  end;

  TGDSDatabaseInfo = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FGDSConnection: TGDSConnection;
    FUserNames: TStringList;
    FBackoutCount: TStringList;
    FDeleteCount: TStringList;
    FExpungeCount: TStringList;
    FInsertCount: TStringList;
    FPurgeCount: TStringList;
    FReadIdxCount: TStringList;
    FReadSeqCount: TStringList;
    FUpdateCount: TStringList;
    FActiveTransactions: TStringList;
    procedure CheckConnection;
    procedure GetDatabaseInfo(LocalBuffer: IntPtr; DbInfoCommand: IntPtr; LocalBufferLength: Short = LocalBufferLen);
    function GetDBFileName: string;
    function GetDBSiteName: string;
    function GetDBImplementationNo: integer;
    function GetDBImplementationClass: integer;
    function GetVersion(DatabaseInfoCommand: integer): string;
    function GetUserNames: TStringList;
    function GetBackoutCount: TStringList;
    function GetDeleteCount: TStringList;
    function GetExpungeCount: TStringList;
    function GetInsertCount: TStringList;
    function GetPurgeCount: TStringList;
    function GetReadIdxCount: TStringList;
    function GetReadSeqCount: TStringList;
    function GetUpdateCount: TStringList;
    function GetIsRemoteConnect: boolean;
    function GetActiveTransactions: TStringList;
    function GetStringDatabaseInfo(DatabaseInfoCommand: integer): string;
    function GetBooleanDatabaseInfo(DatabaseInfoCommand: integer): boolean;
    function GetIntDatabaseInfo(DatabaseInfoCommand: integer): integer;
    function GetOperationCounts(DataBaseInfoCommand: integer; var FOperation: TStringList): TStringList;
    function GetMajorServerVersion: integer;
    function GetMinorServerVersion: integer;

    property Owner: TGDSConnection read FGDSConnection write FGDSConnection;
  public
    constructor Create;
    destructor Destroy; override;
    property Allocation: integer index isc_info_allocation read GetIntDatabaseInfo;
    property BaseLevel: integer index isc_info_base_level read GetIntDatabaseInfo;
    property DBFileName: string read GetDBFileName;
    property DBSiteName: string read GetDBSiteName;
    property IsRemoteConnect: boolean read GetIsRemoteConnect;
    property DBImplementationNo: integer read GetDBImplementationNo;
    property DBImplementationClass: integer read GetDBImplementationClass;
    property NoReserve: integer index isc_info_no_reserve read GetIntDatabaseInfo;
    property ODSMinorVersion: integer index isc_info_ods_minor_version read GetIntDatabaseInfo;
    property ODSMajorVersion: integer index isc_info_ods_version read GetIntDatabaseInfo;
    property MajorServerVersion: integer read GetMajorServerVersion;
    property MinorServerVersion: integer read GetMinorServerVersion;
    property PageSize: integer index isc_info_page_size read GetIntDatabaseInfo;
    property Version: string index isc_info_version read GetVersion;
    property CurrentMemory: integer index isc_info_current_memory read GetIntDatabaseInfo;
    property ForcedWrites: integer index isc_info_forced_writes read GetIntDatabaseInfo;
    property MaxMemory: integer index isc_info_max_memory read GetIntDatabaseInfo;
    property NumBuffers: integer index isc_info_num_buffers read GetIntDatabaseInfo;
    property SweepInterval: integer index isc_info_sweep_interval read GetIntDatabaseInfo;
    property UserNames: TStringList read GetUserNames;
    property Fetches: integer index isc_info_fetches read GetIntDatabaseInfo;
    property Marks: integer index isc_info_marks read GetIntDatabaseInfo;
    property Reads: integer index isc_info_reads read GetIntDatabaseInfo;
    property Writes: integer index isc_info_writes read GetIntDatabaseInfo;
    property BackoutCount: TStringList read GetBackoutCount;
    property DeleteCount: TStringList read GetDeleteCount;
    property ExpungeCount: TStringList read GetExpungeCount;
    property InsertCount: TStringList read GetInsertCount;
    property PurgeCount: TStringList read GetPurgeCount;
    property ReadIdxCount: TStringList read GetReadIdxCount;
    property ReadSeqCount: TStringList read GetReadSeqCount;
    property UpdateCount: TStringList read GetUpdateCount;
    property DBSQLDialect: integer index isc_info_db_SQL_dialect read GetIntDatabaseInfo;
    property ReadOnly: Boolean index isc_info_db_read_only read GetBooleanDatabaseInfo;
    property LogFile: integer index isc_info_logfile read GetIntDatabaseInfo;
    property CurLogFileName: string index isc_info_cur_logfile_name read GetStringDatabaseInfo;
    property CurLogPartitionOffset: integer index isc_info_cur_log_part_offset read GetIntDatabaseInfo;
    property NumWALBuffers: integer index isc_info_num_wal_buffers read GetIntDatabaseInfo;
    property WALBufferSize: integer index isc_info_wal_buffer_size read GetIntDatabaseInfo;
    property WALCheckpointLength: integer index isc_info_wal_ckpt_length read GetIntDatabaseInfo;
    property WALCurCheckpointInterval: integer index isc_info_wal_cur_ckpt_interval read GetIntDatabaseInfo;
    property WALPrvCheckpointFilename: string index isc_info_wal_prv_ckpt_fname read GetStringDatabaseInfo;
    property WALPrvCheckpointPartOffset: integer index isc_info_wal_prv_ckpt_poffset read GetIntDatabaseInfo;
    property WALGroupCommitWaitUSecs: integer index isc_info_wal_grpc_wait_usecs read GetIntDatabaseInfo;
    property WALNumIO: integer index isc_info_wal_num_io read GetIntDatabaseInfo;
    property WALAverageIOSize: integer index isc_info_wal_avg_io_size read GetIntDatabaseInfo;
    property WALNumCommits: integer index isc_info_wal_num_commits read GetIntDatabaseInfo;
    property WALAverageGroupCommitSize: integer index isc_info_wal_avg_grpc_size read GetIntDatabaseInfo;
    property AttachmentID: integer index isc_info_attachment_id read GetIntDatabaseInfo;
    property IsEncrypted: boolean index isc_info_db_encrypted read GetBooleanDatabaseInfo;
    property IsEUAActive: boolean index isc_info_db_eua_active read GetBooleanDatabaseInfo;
  { Firebird }
    property FBVersion: string index frb_info_firebird_version read GetVersion;
    property InfoAttCharset: integer index frb_info_att_charset read GetIntDatabaseInfo;
    property InfoDbClass: integer index frb_info_db_class read GetIntDatabaseInfo;
    property InfoOldestTransaction: integer index frb_info_oldest_transaction read GetIntDatabaseInfo;
    property InfoOldestActive: integer index frb_info_oldest_active read GetIntDatabaseInfo;
    property InfoOldestSnapshot: integer index frb_info_oldest_snapshot read GetIntDatabaseInfo;
    property InfoNextTransaction: integer index frb_info_next_transaction read GetIntDatabaseInfo;
    property InfoDbProvider: integer index frb_info_db_provider read GetIntDatabaseInfo;
    property InfoActiveTransactions: TStringList read GetActiveTransactions;
  end;

  TGDSConnection = class (TCRConnection)
  private
    FDatabaseHandle: PISC_DB_HANDLE;
    FDPB: TBytes;
    FDPBLength: integer;
    FStatusVector: TStatusVector;
    FLastError: integer;
    FGDS: TGDS;

    { Parameters }
    FDatabase: string;
    FProtocol: _TIBCProtocol;
    FIPVersion: TIPVersion;
    FPort: string;
    FRole: string;
    FDBSQLDialect: integer;
    FSQLDialect: integer;
    FReadOnly: Boolean;
    FReadOnlyCached: Boolean;
    FIsFBServerCached: Boolean;
    FIsFBServer: Boolean;
    FOptimizedNumerics: boolean;
    FEnableLargeint: boolean;
    FEnableMemos: boolean;
    FSimpleNumericMap: boolean;
    FTrustedAuthentication: boolean;
    FNoDBTriggers: boolean;
    FSSLOptions: TGDSSSLOptions;

    { Charset options }
    FCharset: string;
    FCharsetId: word;
    FDBCharsetId: word;
    FCharLength: word;
    FDBCharLength: word;
    FQueryCharLength: boolean;
    FUseUnicode: boolean;
    FClientLibrary: string;

    FParams: TStrings;
    FParamsChanged: boolean;
    FDatabaseInfo: TGDSDatabaseInfo;
    FMajorServerVersion: integer;
    FMinorServerVersion: integer;
    FServerVersion: string;
    FServerVersionFull: string;
  {$IFDEF LITE}
    FWireCompression: boolean;
  {$ENDIF}

    procedure ParamsChange(Sender: TObject);
    procedure ParamsChanging(Sender: TObject);

    procedure CheckInactive;
    procedure CheckClientSQLDialect;
    procedure GetDatabaseParameters;
    function GetCharLength(CharsetID: integer; const RelName: string): integer; overload;
    procedure FlushProps;
    procedure GDSNotification;
  protected
    procedure InitCommandProp(Command: TCRCommand); override;
    procedure InitRecordSetProp(RecordSet: TCRRecordSet); override;

    procedure InitGDS;
    procedure CreateDPB;

    property InProcessError: boolean read FInProcessError;
    property AutoCommit;
    property EnableBCD;
    property EnableFMTBCD;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCharLength: integer; overload;
    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
  {$IFNDEF LITE}
    function GetLoaderClass: TCRLoaderClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    function CreateObject(DataType: Word; Transaction: TCRTransaction): TDBObject; override;
  {$ENDIF}

    procedure Check(Status: ISC_STATUS);
    procedure IBCError(var StatusVector: TStatusVector; UseCallback: boolean; Component: TObject); virtual;

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;
    procedure Ping; override;

  { Connect control }
    procedure AssignConnect(Source: TCRConnection); override;
    function GetLastError: integer;
    procedure SetLastError(Value: integer);
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;
    procedure SetUsername(const Value: string); override;
    procedure SetPassword(const Value: string); override;
    function CheckIsValid: boolean; override;
    function CanChangeDatabase: boolean; override;

  { InterBase support}
    function GetDatabaseHandle: TISC_DB_HANDLE;
    procedure SetDatabaseHandle(Value: TISC_DB_HANDLE);
    procedure CreateDatabase;
    procedure DropDatabase;
    procedure OnlineDump(const DumpFileName: string; const Overwrite: boolean; const AdditionalDumpFiles: array of const);

    function IsFBServer: boolean;
    function IsClientFB3: boolean;
    function GetMajorServerVersion: integer;
    function GetMinorServerVersion: integer;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;

    procedure SetParams(const Value: TStrings);
    function GetParams: TStrings;

    property DatabaseInfo: TGDSDatabaseInfo read FDatabaseInfo;
    property GDS: TGDS read FGDS;
  end;

  { TGDSTransaction }
  TIBCIsolationLevel = (iblSnapshot, iblReadCommitted, iblReadOnlyReadCommitted, iblTableStability, iblReadOnlyTableStability, iblCustom);

  TGDSTransaction = class(TCRTransaction)
  private
    FTransactionHandle: TISC_TR_HANDLE;
    FTPB: TBytes;
    FTPBLength: integer;

    FStatusVector: TStatusVector;
    FLastError: integer;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FGDS: TGDS;

    { Parameters }
    FParams: TStrings;
    FParamsChanged: boolean;

    //FOnClose: TNotifyEvent; // not used

    procedure ParamsChange(Sender: TObject);
    procedure ParamsChanging(Sender: TObject);
    procedure ExecuteImmediate(const SQL: string);
  protected
    procedure CreateTPB;
    procedure IBCError(var StatusVector: TStatusVector; UseCallback: boolean); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Check(Status: ISC_STATUS);

  { Transaction control }
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
    procedure StartTransaction; override;
    procedure AssignConnect(Source: TCRTransaction); override;
    procedure Reset; override;
    function CanRestoreAfterFailover: boolean; override;

  { Savepoit control }
    procedure ReleaseSavepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;
    procedure Savepoint(const Name: string); override;

    procedure AssignTransaction(Source: TGDSTransaction);
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

  { Connections list control }
    function GetConnection(Idx: integer): TGDSConnection;

    procedure SetParams(const Value: TStrings);
    function GetParams: TStrings;

    function GetInTransaction: boolean; override;
    //property OnClose: TNotifyEvent read FOnClose write FOnClose;

    function GetMultiTransactionID: Int64; override;

    property GDS: TGDS read FGDS;
    property NativeTransaction: boolean read FNativeTransaction;
    property TransactionHandle: TISC_TR_HANDLE read FTransactionHandle write FTransactionHandle;
  end;

  TSQLDA = class;

  TSQLVARAccessor = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TSQLDA;
    FXSQLVAR: IntPtr;
    FXSQLVARType: TXSQLVARType;
    FRelAliasName: string;

    procedure ReadStringValue(Offset: integer; var Value: string);
    procedure WriteStringValue(Offset: integer; const Value: string);

    function GetSqlType: Short;
    procedure SetSqlType(const Value: Short);
    function GetSqlScale: Short;
    procedure SetSQLScale(const Value: Short);
    function GetSqlPrecision: Short;
    procedure SetSqlPrecision(const Value: Short);
    function GetSqlSubtype: Short;
    procedure SetSqlSubtype(const Value: Short);
    function GetSqlLen: Word;
    procedure SetSqlLen(const Value: Word);
    function GetSqlData: IntPtr;
    procedure SetSqlData(const Value: IntPtr);
    function GetSqlInd: IntPtr;
    procedure SetSqlInd(const Value: IntPtr);
    function GetSqlName_length: Short;
    procedure SetSqlName_length(const Value: Short);
    function GetSqlName: string;
    procedure SetSqlName(const Value: string);
    function GetRelName_Length: Short;
    procedure SetRelName_Length(const Value: Short);
    function GetRelName: string;
    procedure SetRelName(const Value: string);
    function GetOwnName_Length: Short;
    procedure SetOwnName_Length(const Value: Short);
    function GetOwnName: string;
    procedure SetOwnName(const Value: string);
    function GetAliasName_Length: Short;
    procedure SetAliasName_Length(const Value: Short);
    function GetAliasName: string;
    procedure SetAliasName(const Value: string);
    function GetIsNullable: boolean;
    function GetRelAliasName: string;
    procedure SetRelAliasName(const Value: string);
  protected
    procedure SetXSQLVAR(XSQLVAR: IntPtr; XSQLVARType: TXSQLVARType);
  public
    constructor Create(AOwner: TSQLDA);

    property sqltype: Short read GetSqlType write SetSqlType;
    property sqlscale: Short read GetSqlScale write SetSQLScale;
    property sqlprecision: Short read GetSqlPrecision write SetSQLPrecision;
    property sqlsubtype: Short read GetSqlSubtype write SetSqlSubtype;
    property sqllen: Word read GetSqlLen write SetSqlLen;
    property sqldata: IntPtr read GetSqlData write SetSqlData;
    property sqlind: IntPtr read GetSqlInd write SetSqlInd;
    property sqlname_length: Short read GetSqlName_length write SetSqlName_length;
    property sqlname: string read GetSqlName write SetSqlName;
    property relname_length: Short read GetRelName_Length write SetRelName_Length;
    property relname: string read GetRelName write SetRelName;
    property ownname_length: Short read GetOwnName_Length write SetOwnName_Length;
    property ownname: string read GetOwnName write SetOwnName;
    property aliasname_length: Short read GetAliasName_Length write SetAliasName_Length;
    property aliasname: string read GetAliasName write SetAliasName;
    property relaliasname: string read GetRelAliasName write SetRelAliasName;
    property IsNullable: boolean read GetIsNullable;
  end;

  TSQLDA = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TGDSConnection;
    FXSQLDA: IntPtr;
    FXSQLVARType: TXSQLVARType;
    FXSQLVARs: array of TSQLVARAccessor;

    function GetVersion: Short;
    procedure SetVersion(const Value: Short);
    function GetSqldaid: String;
    procedure SetSqldaid(const Value: String);
    function GetSqldabc: Long;
    procedure SetSqldabc(const Value: Long);
    function GetSqln: Short;
    procedure SetSqln(const Value: Short);
    function GetSqld: Short;
    procedure SetSqld(const Value: Short);
    function GetSqlvar: IntPtr;
    procedure SetSqlvar(const Value: IntPtr);
    function GetSQLVars(Index: integer): TSQLVARAccessor;
  public
    constructor Create(Connection: TGDSConnection; SQLVARType: TXSQLVARType);
    destructor Destroy; override;

    procedure AllocSQLDA(n: integer);
    procedure FreeSQLDA;

    property Version: Short read GetVersion write SetVersion;
    property SqldaID: String read GetSqldaid write SetSqldaid;
    property Sqldabc: Long read GetSqldabc write SetSqldabc;
    property Sqln: Short read GetSqln write SetSqln;
    property Sqld: Short read GetSqld write SetSqld;
    property SqlVar: IntPtr read GetSqlvar write SetSqlvar;

    property Vars[Index: integer]: TSQLVARAccessor read GetSQLVars; default;
  end;

  TIBCParamDesc = class (TParamDesc)
  private
    FBindBuffer: IntPtr;
    FBindBufferLength,
    FBindBufferSize: integer;
    FBindBufferDataType: word;
    FIndicator: TInt16Array;
    FDBType: integer;
    FIsInternalBuffer: boolean;

    function CreateObject: TSharedObject;

  {$IFNDEF LITE}
    // used in Loader
    procedure InternalAllocBuffer;
  {$ENDIF}
  protected
    function GetCommand: TGDSCommand;

    procedure AllocBindBuffer(BufferLength: integer; const SQLVAR: TSQLVARAccessor);
    procedure FreeBindBuffer;

    procedure FreeBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;

    procedure SetValue(const Value: Variant); override;

    function GetInternalValuePtr(Index: integer): IntPtr;{$IFDEF USE_INLINE} inline;{$ENDIF}
    function GetIndicatorPtr(Index: integer): IntPtr;{$IFDEF USE_INLINE} inline;{$ENDIF}
    function GetValuePtr(const Value: PVariant): IntPtr;{$IFDEF USE_INLINE} inline;{$ENDIF}

    procedure SyncIndicator(ValueIndex: integer; IsNull: boolean);

    function IsObjectValue: boolean; override;

    function GetBatchIters: integer;
    function GetBatchOffset: integer;

    property Precision: integer read GetPrecision write SetPrecision;
    property Scale: integer read GetScale write SetScale;
  end;

  TGDSCommand = class (TCRCommand)
  private
    FSQLType: word;
    FIntegerPrecision: integer;
    FFloatPrecision: integer;
    FQueryRowsAffected: boolean;
    FCacheBlobs: boolean;
    FStreamedBlobs: boolean;
    FCacheArrays: boolean;
    FUseDescribeParams: boolean;
    FRowsInserted: integer;
    FRowsUpdated: integer;
    FRowsDeleted: integer;
    FRowsAffected: integer;
    FGetBlobData: TGetBlobData;

    FState: TCursorState;

    FStmtHandle: PISC_STMT_HANDLE;
    FCursor: IntPtr;
    FStatusVector: TStatusVector;

    FGCHandle: IntPtr;

    FInXSQLDA: TSQLDA;
    FOutXSQLDA: TSQLDA;

    FInfoTypeBuf,
    FRowsAffectedBuf,
    FRelAliasesBuf: IntPtr;

    function GetIsFBConnection: boolean;
    function GetGCHandle: IntPtr;
    function GetPlan: string;
    function GetGDS: TGDS;

    procedure TrimBuffer(Buf: IntPtr; var BufLen: integer);

    procedure CheckSQLDA(InVarsCount: integer = 0; OutVarsCount: integer = 0);
    procedure FillSQLDA(Full: boolean = True);

    function IsBatchCommand: boolean;{$IFDEF USE_INLINE} inline;{$ENDIF}
    function GetBatchSQL(var Iters: integer): string; reintroduce;

    function ConvertParamValue(const Param: TIBCParamDesc; const Source: PVariant; const Dest: IntPtr; const SQLVAR: TSQLVARAccessor): IntPtr;
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TGDSConnection;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FTransaction: TGDSTransaction;

    procedure RaiseError(const Msg: string); virtual;

    procedure CheckDatabase;
    procedure CheckTransaction;

    procedure Check(Status: ISC_STATUS);
    procedure CheckActive;

    function GetActive: boolean;
    procedure GetRowsAffected;

    procedure ReadOutParams;
    procedure DescribeParam(const Name: string; Param: TIBCParamDesc; SQLVAR: TSQLVARAccessor);
    procedure DescribeParams(Full: boolean = True; InParamsCount: integer = 0);

    procedure CreateBatchCommand; override;

    property Params;

    property GCHandle: IntPtr read GetGCHandle;

  public
    constructor Create; override; //CLR requirement
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    class function GetParamDescClass: TParamDescClass; override;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    procedure InternalExecute;
    function ExecuteNext: boolean;

    procedure Finish;

    procedure SetSQL(const Value: string); override;
    function ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string; override;
    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; override;
    function ForceCreateSPParams: boolean; override;

  { Params }
    procedure BindParams;

    procedure BreakExec; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;

    procedure Execute; override;
    procedure ExecuteBatch(Iters: integer = 1; Offset: integer = 0); overload; override;
    procedure ExecuteBatch(const Statements: _TStringArray); reintroduce; overload;
    procedure Close; override;

    procedure SetConnection(Value: TCRConnection); override;
    function GetTransaction: TCRTransaction; override;
    procedure SetTransaction(Value: TCRTransaction); override;
    function GetSQLType: integer;
    function GetSQLTypeEx: integer;

    function GetStmtHandle: TISC_STMT_HANDLE;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    property GDS: TGDS read GetGDS;

    property EnableBCD;
    property EnableFMTBCD;

    property GetBlobData: TGetBlobData read FGetBlobData write FGetBlobData;
  end;

  TIBCFieldDesc = class (TCRFieldDesc)
  protected
    FCharsetID: integer;
    FCharLength: integer;
    FSQLSubType: integer;
    FDomainName: string;
    FIsFetchBlock: boolean; // performance tweaks
    FIsProcess: boolean; // performance tweaks
    FSkipSetReadOnly: boolean;

    procedure SetDataType(Value: Word); override;
    procedure SetFieldReadOnly(SetFieldsReadOnly: boolean; UpdatingTableInfo: TCRTableInfo); override;
  public
    property DomainName: string read FDomainName write FDomainName;
    property CharsetID: integer read FCharsetID write FCharsetID;
    property CharLength: integer read FCharLength write FCharLength;
    property SQLSubType: integer read FSQLSubType write FSQLSubType;
    property IsFetchBlock: boolean read FIsFetchBlock;
    property IsProcess: boolean read FIsProcess;
  end;

  TIBCSQLInfo = class(TSQLInfo)
  public
    function IdentCase: TIdentCase; override;
    function DetectReturningSQLType: boolean; override;
  end;

  TGDSRecordSet = class (TCRRecordSet)
  private
    FAutoClose: boolean;
    FFieldXSQLDA: TSQLDA;

    FFieldsAsString: boolean;
    FComplexArrayFields: boolean;
    FDeferredBlobRead: boolean;
    FDeferredArrayRead: boolean;

    FHasFlatUnicodeFields: boolean; //Recordset has non complex unicode fields that are not stored in String Heap
    FGCHandle: IntPtr;

    FIsFBConnection: boolean;
    FMinorServerVersion: integer;
    FMajorServerVersion: integer;
    FUseUnicode: boolean;
    FCharLength: integer;
    FSQLDialect: integer;
    FBooleanDomainFields: boolean;
    FSetDomainNames: boolean;
    FExtFieldsInfoRecordSet: boolean;

    function GetIsFBConnection: boolean;
    function GetMinorServerVersion: integer;
    function GetMajorServerVersion: integer;
    function GetUseUnicode: boolean;
    function GetCharLength: integer;
    function GetSQLDialect: integer;
    function GetGCHandle: IntPtr;
    function GetGDS: TGDS;

    function GetComplexFldOffset(FieldDesc: TFieldDesc): integer;
    function CreateRecordset(SQL: string): TGDSRecordset;
    function GetTableNames(OnlyForBooleanFields: boolean): string;
    procedure GetExtFieldsInfo;
  protected
    FCommand: TGDSCommand;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TGDSConnection;

    procedure Check(Status: ISC_STATUS);

    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
    function GetTransaction: TCRTransaction;

  { Open/Close }
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalClose; override;

  { Fetch }
    procedure AllocFetchBuffer; override;
    procedure InitBlock(Block: PBlockHeader); override;
    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); override;
    function NeedUnPrepareAfterFetch: boolean; override;

  { Fields }
    procedure InitRecordSize; override;
    procedure CreateFieldDescs; override;
    function GetIndicatorItemSize: Integer; override;
    function GetArrayFieldName(ObjectType: TObjectType; ItemIndex: integer): string; override;

    property GCHandle: IntPtr read GetGCHandle;
    //PreCached FConection properties
    property IsFBConnection: boolean read GetIsFBConnection;
    property MinorServerVersion: integer read GetMinorServerVersion;
    property MajorServerVersion: integer read GetMajorServerVersion;
    property UseUnicode: boolean read GetUseUnicode;
    property CharLength: integer read GetCharLength;
    property SQLDialect: integer read GetSQLDialect;

  public
    constructor Create; override; //CLR requirement
    destructor Destroy; override;

  { Open/Close }
    procedure Reopen; override;
    procedure SetCommandType;
    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override; // Execute command
    procedure Disconnect; override;

  { Filter/Find/Locate/Sorting }
    function GetSortOptions(SortColumn: TSortColumn): TCompareOptions; override;

  { Fetch }
    procedure ExecFetch(DisableInitFields: boolean); override;
    procedure FetchAll; override;
    function RowsReturn: boolean; override;

  { Fields }
    class function GetBufferSize(DataType: Word; LengthInChars: integer): integer; override;
    procedure DetectIdentityField; override;
    function GetFieldDescType: TFieldDescClass; override;
    procedure SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean); override;
    function GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; override;
    procedure DetectFieldType({$IFNDEF LITE}const FieldName: string;{$ENDIF} DBType: Word; DBLength: Integer; DBScale: SmallInt;
      IsDbKey: Boolean; CharsetID: integer; CharLength: Integer;
      out DataType, SubDataType: Word; out Length, Scale: Integer; out Fixed: boolean);

    procedure GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType, SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean); override;

    class function IsObjectDataType(DataType: word): boolean; override;
    class function IsFetchBlockField(DataType: word): boolean;
    class function IsProcessFieldType(DataType: word): boolean;

  { Records }
    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc); override;

  { Navigation }
    procedure SetToEnd; override;

    procedure SetConnection(Value: TCRConnection); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

  {$IFNDEF LITE}
    function GetMapRule(const FieldName: string; DBType: Word; DBLength, DBScale, CharsetID: integer): TCRMapRule;
    function GetMapFetchConverter(const FieldName: string; DBType: Word; DBLength, DBScale, CharsetID: integer): TFetchConverter;
    function GetMapOnDemandConverter(Field: TCRFieldDesc; out MapRule: TCRMapRule): TOnDemandConverter; override;
  {$ENDIF}

    property GDS: TGDS read GetGDS;
  end;

{$IFDEF LITE}
  {$DEFINE DBX_METADATA}
{$ENDIF}

{$IFNDEF LITE}

{ TGDSMetaData }

  TGDSMetaData = class (TCRMetaData)
  protected
    function CreateRecordSet: TCRRecordSet; override;

    function InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData; override;
    procedure InternalGetMetaDataKindsList(List: TStringList); override;
    procedure InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string); override;

    function GetTables(Restrictions: TStrings): TData; override;
    procedure CopyTablesData(Restrictions: TStrings); virtual;

    function GetColumns(Restrictions: TStrings): TData; override;
    procedure CopyColumnsData(Restrictions: TStrings); virtual;

    function GetProcedures(Restrictions: TStrings): TData; override;
    procedure CopyProceduresData(Restrictions: TStrings); virtual;

    function GetProcedureParameters(Restrictions: TStrings): TData; override;
    procedure CopyProcedureParametersData(Restrictions: TStrings); virtual;

    function GetIndexes(Restrictions: TStrings): TData; override;
    procedure CopyIndexesData(Restrictions: TStrings); virtual;

    function GetIndexColumns(Restrictions: TStrings): TData; override;
    procedure CopyIndexColumnsData(Restrictions: TStrings); virtual;

    function GetConstraints(Restrictions: TStrings): TData; override;
    procedure CopyConstraintsData(Restrictions: TStrings); virtual;

    function GetConstraintColumns(Restrictions: TStrings): TData; override;
    procedure CopyConstraintColumnsData(Restrictions: TStrings); virtual;

    function GetRoles(Restrictions: TStrings): TData; override;

    function GetGenerators(Restrictions: TStrings): TData;

  {$IFNDEF DBX_METADATA}
    procedure CreateTablesFields; override;
    procedure CreateColumnsFields; override;
    procedure CreateProceduresFields; override;
    procedure CreateProcedureParametersFields; override;
    procedure CreateIndexesFields; override;
    procedure CreateIndexColumnsFields; override;
    procedure CreateConstraintsFields; override;
    procedure CreateConstraintColumnsFields; override;
  {$ENDIF}
  end;

{ TGDSLoader }

  _TIBCInsertMode = (_imInsert, _imUpdateOrInsert);

  TGDSLoaderColumn = class(TCRLoaderColumn)
  private
    FDataTypeName: string;
  end;

  TGDSLoader = class(TCRLoader)
  private
    FConnection: TGDSConnection;
    FCommand: TGDSCommand;
    FInsertHeader: string;
    FGeneratedRows: integer;
    FInsertMode: _TIBCInsertMode;
    FRowsPerBatch: integer;

  protected
    procedure SetConnection(Value: TCRConnection); override;

    procedure GenerateSQL(BatchSize: integer);
    procedure FlushRows;
    function IsPutColumnDataAllowed: boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    class function GetColumnClass: TCRLoaderColumnClass; override;
    procedure Reset; override;
    procedure Prepare; overload; override;
    procedure Prepare(RecordCount: integer); overload; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;
  end;

{ TGDSAlerter }

  TGDSAlerterEventCallback = procedure(const EventName: string; EventCount: integer) of object;

  TGDSAlerterThread = class;

  TGDSAlerter = class(TCRAlerter)
  private
    FThreads: TList;
    FGCHandle: IntPtr;
    FOnEvent: TGDSAlerterEventCallback;

    function GetGCHandle: IntPtr;

  protected
    procedure DoOnEvent(const EventName: string; EventCount: integer);
    property GCHandle: IntPtr read GetGCHandle;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SendEvent(const EventName, Message: string); override;
    procedure Start; override;
    procedure Stop; override;

    property OnEvent: TGDSAlerterEventCallback read FOnEvent write FOnEvent;
  end;

{ TGDSAlerterThread }

  TGDSAlerterThread = class(TThread)
  private
    FAlerter: TGDSAlerter;
    FConnection: TGDSConnection;
    FStatusVector: TStatusVector;
    FEventBuffer: IntPtr;
    FEventBufferLen: Integer;
    FResultBuffer: IntPtr;
    FEventIDPtr: IntPtr;
    FEventGroup: Integer;
    FEventCount: Integer;
    FFirstActivation: Boolean;
    FGCHandle: IntPtr;
    FSignal: TEvent;

    procedure RegisterEvents;
    procedure QueueEvents;
    procedure CancelEvents;
    procedure CheckEvents;
    procedure UpdateResultBuffer(Length: SmallInt; Updated: IntPtr);
    procedure Check(Status: ISC_STATUS);

  protected
    procedure Execute; override;

  public
    constructor Create(Alerter: TGDSAlerter; EventGroup: Integer);
    destructor Destroy; override;

    procedure Terminate;
  end;

{$ENDIF}

  TIBCBlob = class(TCompressedBlob)
  private
    FID: PISC_QUAD;
    FHandle: PISC_BLOB_HANDLE;
    FStatusVector: TStatusVector;
    FDbHandle: PISC_DB_HANDLE;
    FTrHandle: PISC_TR_HANDLE;
    FConnection: TGDSConnection;
    FTransaction: TGDSTransaction;
    FDbHandleChecked,
    FTrHandleChecked: boolean;

    FInfoReaded: boolean;
    FBlobLength: Cardinal;
    FNumSegments: Cardinal;
    FMaxSegmentSize: Word;

    FNativeHandle: boolean;
    FNeedFree: boolean;

    FBPB: TBytes;
    FBPBLength: integer;

    FParamsChanged: boolean;

    FCached: boolean;
    FStreamed: boolean;
    FIsPrepared: boolean;
    FStreamIsNull: boolean;
    FForceCachedMode: boolean;

    FSubTypeChanged: boolean;
    FConversionSubTypeChanged: boolean;
    FSubType: integer;
    FCharsetID: integer;
    FConversionSubType: integer;
    FConversionCharsetID: integer;
    FForceEncodeToUtf8: boolean;

    function GetID: TISC_QUAD;
    function GetHandle: TISC_BLOB_HANDLE;
    function GetDbHandle: TISC_DB_HANDLE;
    function GetTrHandle: TISC_TR_HANDLE;
    procedure SetID(Value: TISC_QUAD);
    procedure SetHandle(Value: TISC_BLOB_HANDLE);
    procedure SetDbHandle(Value: TISC_DB_HANDLE);
    procedure SetTrHandle(Value: TISC_TR_HANDLE);
    procedure SetConnection(Value: TGDSConnection);
    procedure SetTransaction(Value: TGDSTransaction);
    procedure SetSubType(Value: integer);
    procedure SetCharsetID(Value: integer);
    procedure SetConversionSubType(Value: integer);
    procedure SetConversionCharsetID(Value: integer);
    procedure SetStreamed(Value: boolean);
    procedure SetCached(const Value: boolean);
    procedure SetForceEncodeToUtf8(const Value: boolean);
    function GetGDS: TGDS;

  protected
    FNeedReadBlob: boolean;

    procedure Init(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
    function GetIDPtr: PISC_QUAD;
    procedure Check(Status: ISC_STATUS);
    procedure GetBlobInfo;
    procedure CheckValue; override;
    function GetSize: cardinal; override;
    function GetSizeAnsi: cardinal; override;
    procedure CheckAlloc;
    procedure CheckDatabase;
    procedure CheckTransaction;
    function CharSize: Byte;

    procedure CreateBPB;

    function CreateClone: TBlob; override;

  public
    constructor Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE); overload;
    constructor Create(Connection: TGDSConnection; Transaction: TGDSTransaction; IsUnicode: boolean = False; NeedFree: boolean = False); overload;
    destructor Destroy; override;

    procedure PrepareBlob;
    procedure WritePieceBlob(Size: Word; Ptr: IntPtr);

    procedure AllocBlob(Read: boolean = True);
    procedure CloseBlob;
    procedure FreeBlob; override;
    procedure Disconnect; override;

    function IsInit: boolean;
    function LengthBlob: integer;

    procedure ReadBlob;
    procedure WriteBlob;
    procedure CheckInfo;

    function Read(Position, Count: cardinal; Dest: IntPtr): cardinal; override;
    procedure Write(Position, Count: cardinal; Source: IntPtr); override;
    procedure Clear; overload; override;
    procedure Clear(ResetStreamIsNull: boolean); reintroduce; overload;
    procedure Truncate(NewSize: cardinal); override;

    property ID: TISC_QUAD read GetID write SetID;
    property Handle: TISC_BLOB_HANDLE read GetHandle write SetHandle;

    property DbHandle: TISC_DB_HANDLE read GetDbHandle write SetDbHandle;
    property TrHandle: TISC_TR_HANDLE read GetTrHandle write SetTrHandle;
    property Connection: TGDSConnection read FConnection write SetConnection;
    property Transaction: TGDSTransaction read FTransaction write SetTransaction;
    property GDS: TGDS read GetGDS;

    property SubType: integer read FSubType write SetSubType;
    property CharsetID: integer read FCharsetID write SetCharsetID;
    property ConversionSubType: integer read FSubType write SetConversionSubType;
    property ConversionCharsetID: integer read FConversionCharsetID write SetConversionCharsetID;

    property NumSegments: Cardinal read FNumSegments;
    property MaxSegmentSize: Word read FMaxSegmentSize;

    property Streamed: boolean read FStreamed write SetStreamed;
    property Cached: boolean read FCached write SetCached;
    property ForceEncodeToUtf8: boolean read FForceEncodeToUtf8 write SetForceEncodeToUtf8;
  end;

  function Reverse2(Value: word): Word;
  function Reverse4(Value: cardinal): cardinal;

  function XSQLDA_LENGTH(n: Long; XSQLVARType: TXSQLVARType): Long;

  procedure DateTimeToIBCTimeStamp(DateTime: TDateTime; const Buf: IntPtr);
  procedure DateTimeToIBCDate(DateTime: TDateTime; const Buf: IntPtr);
  procedure DateTimeToIBCTime(DateTime: TDateTime; const Buf: IntPtr);
  procedure IBCTimeStampToDateTime(ib_date: IntPtr; const Buf: IntPtr);
  procedure IBCDateToDateTime(ib_date: IntPtr; const Buf: IntPtr);
  procedure IBCTimeToDateTime(ib_time: IntPtr; const Buf: IntPtr);

  //procedure SetStrTerminator(Buf: IntPtr; Size, CharLen: integer);

  { BCD converting support }
  function IBCDecimalToBcd(Value: int64; Scale: integer): TBcd;

  function SSLParamsToString(Enabled: boolean; const ParamValue: array of string): string;
  function GetFullDatabaseName(const Host: string; const Port: string; Protocol: _TIBCProtocol;
    const FileName: string; IsClientFB3: boolean; IPVersion: TIPVersion; const SSLOptions: string): string;
  function GetPortPosFromHost(const Host: string; const PortSymbol: string; var Port: string): integer;
  function GetPortFromHost(const Host: string; var Port: string): boolean;
  function GetDBNamePos(const Database: string): integer;
  procedure ParseDatabaseName(const Database: string; var Host: string; var Port: string; var Protocol: _TIBCProtocol;
    var FileName: string; ParsePort: boolean = True; NamePos: integer = -1);
  function GetIsURLConnectionString(const Database: string): boolean;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  procedure AllocIBDACWnd;
{$ENDIF}
{$ENDIF}

var
  IntegerPrecision: integer = 10;
  BcdPrecision: integer = 18;
  FloatPrecision: integer   = 16;
  IBCSQLInfo: TIBCSQLInfo;
  DisableReturningKeyword: boolean = False;
  ForceUsingDefaultPort: boolean = False;

implementation


uses
  Math,
{$IFNDEF UNIDACPRO}
  IBCConsts, IBCProps, IBCParser, IBCArray, IBCDataTypeMap,
{$ELSE}
  IBCConstsUni, IBCPropsUni, IBCParserUni, IBCArrayUni, IBCDataTypeMapUni,
{$ENDIF}
{$IFNDEF FPC}
  SqlTimSt,
{$ENDIF}
  CRProps, CRTimeStamp, DAConsts;

var
  GlobalCS, EventsCS: TCriticalSection;

const
  // if BooleanDomainFields option is set
  BooleanIntTypes = [dtSmallint];
  // if SetDomainNames option is set
  AllBooleanIntTypes = [dtSmallint, dtInteger, dtInt64];

{$IFDEF MSWINDOWS}
const
  WM_EVENTRECIEVED = WM_USER + 1;
{$ENDIF}

{$IFNDEF LITE}
var
{$IFDEF MSWINDOWS}
  hIBDACWindow: HWND = 0;
  IBDACWindowCnt: Integer = 0;
{$ENDIF}
  ISCCallbackPtr: IntPtr;

type
  TAlertEvent = class
  public
    Name: string;
    Count: integer;
  end;

{$ENDIF}

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
function WndProc(hWindow: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Alerter: TGDSAlerter;
  Event: TAlertEvent;
begin
  Result := 0;
  try
    case Msg of
      WM_EVENTRECIEVED: begin
        Alerter := TGDSAlerter(GetGCHandleTarget(IntPtr(wParam)));
        Event := TAlertEvent(GetGCHandleTarget(IntPtr(lParam)));
        FreeGCHandle(IntPtr(lParam));
        try
          Alerter.DoOnEvent(Event.Name, Event.Count);
        finally
          Event.Free;
        end;
      end;
    else
      Result := DefWindowProc(hWindow, Msg, wParam, lParam);
    end;
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(nil);
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
    lpszClassName: 'IBDACUtilWnd'
  );

procedure AllocIBDACWnd;
var
  TempClass: TWndClass;
  ClassRegistered: boolean;
  WndProcPtr: IntPtr;
begin
  GlobalCS.Enter;
  try
    if hIBDACWindow = 0 then begin
      WndClass.hInstance := HInstance;
      ClassRegistered := Windows.GetClassInfo(HInstance, WndClass.lpszClassName,
        TempClass);
      WndProcPtr := @WndProc;

      if not ClassRegistered or ({$IFDEF FPC}@{$ENDIF}TempClass.lpfnWndProc <> WndProcPtr) then begin
        if ClassRegistered then
          Windows.UnregisterClass(WndClass.lpszClassName, HInstance);

        hIBDACWindow := Windows.RegisterClass(WndClass);
      end;

      hIBDACWindow := Windows.CreateWindowEx(WS_EX_TOOLWINDOW, 'IBDACUtilWnd',
        '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
      Assert(hIBDACWindow <> 0);
    {$IFDEF CPU64}
      Windows.SetWindowLongPtr(hIBDACWindow, GWL_WNDPROC, NativeInt(WndProcPtr));
    {$ELSE}
      Windows.SetWindowLong(hIBDACWindow, GWL_WNDPROC, Integer(WndProcPtr));
    {$ENDIF}
    end;
    Inc(IBDACWindowCnt);
  finally
    GlobalCS.Leave;
  end;
end;
{$ENDIF}
{$ENDIF}

function DataTypeName(DataType: Word; Size, Precision, Scale: integer; SqlType: smallint = 0; SubSqlType: smallint = 0): string;
begin
  case DataType of
    dtUnknown, dtString, dtExtString,
    dtWideString, dtExtWideString,
    dtFixedChar, dtFixedWideChar:
      Result := 'VARCHAR(' + IntToStr(Size) + ')';
    dtBytes, dtVarBytes, dtExtVarBytes:
      if ((SqlType = SQL_IB_TEXT) or (SqlType = SQL_IB_VARYING)) and (Byte(SubSqlType) <> CH_OCTETS) then
        Result := 'VARCHAR(' + IntToStr(Size) + ')'
      else if Size > 0 then begin
        if DataType = dtBytes then
          Result := 'CHAR(' + IntToStr(Size) + ') CHARACTER SET OCTETS'
        else
          Result := 'VARCHAR(' + IntToStr(Size) + ') CHARACTER SET OCTETS';
      end
      else
        Result := 'BLOB';
    dtInt8, dtInt16:
      Result := 'SMALLINT';
    dtInt32, dtUInt16:
      Result := 'INTEGER';
    dtInt64, dtUInt32:
      Result := 'BIGINT';
    dtSingle:
      Result := 'FLOAT';
    dtFloat, dtCurrency:
      Result := 'DOUBLE PRECISION';
    dtFMTBCD,
    dtBCD:
      Result := 'NUMERIC(' + IntToStr(Precision) + ',' + IntToStr(Scale) + ')';
    dtDate:
      Result := 'DATE';
    dtTime:
      Result := 'TIME';
    dtSQLTimeStamp,
    dtDateTime:
      Result := 'TIMESTAMP';
    dtBoolean:
      Result := 'BOOLEAN';
    dtBlob, dtMemo, dtWideMemo:
      Result := 'BLOB';
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

{ TGDSSSLOptions }

procedure TGDSSSLOptions.AssignTo(Dest: TPersistent);
begin
  if Dest.ClassName = TGDSSSLOptions.ClassName then begin
    TGDSSSLOptions(Dest).FEnabled := FEnabled;
    TGDSSSLOptions(Dest).FServerPublicFile := FServerPublicFile;
    TGDSSSLOptions(Dest).FServerPublicPath := FServerPublicPath;
    TGDSSSLOptions(Dest).FClientCertFile := FClientCertFile;
    TGDSSSLOptions(Dest).FClientPassPhraseFile := FClientPassPhraseFile;
    TGDSSSLOptions(Dest).FClientPassPhrase := FClientPassPhrase;
  end
  else
    inherited;
end;


function TGDSSSLOptions.Equals(SSLOptions: TGDSSSLOptions): boolean;
begin
  Result := False;
  if SslOptions <> nil then
    Result :=
      (SSLOptions.FEnabled = FEnabled) and
      (SSLOptions.FServerPublicFile = FServerPublicFile) and
      (SSLOptions.FServerPublicPath = FServerPublicPath) and
      (SslOptions.FClientCertFile = FClientCertFile) and
      (SSLOptions.FClientPassPhraseFile = FClientPassPhraseFile) and
      (SSLOptions.FClientPassPhrase = FClientPassPhrase);
end;

function TGDSSSLOptions.GetSSLString: string;
begin
  Result := SSLParamsToString(FEnabled,
    [FServerPublicFile, FServerPublicPath, FClientCertFile, FClientPassPhraseFile, FClientPassPhrase]);
end;

{ TGDSDatabaseInfo }

constructor TGDSDatabaseInfo.Create;
begin
  inherited Create;

  FUserNames := TStringList.Create;
  FBackoutCount := nil;
  FDeleteCount := nil;
  FExpungeCount := nil;
  FInsertCount := nil;
  FPurgeCount := nil;
  FReadIdxCount := nil;
  FReadSeqCount := nil;
  FUpdateCount := nil;
  FActiveTransactions := nil;
end;

destructor TGDSDatabaseInfo.Destroy;
begin
  FActiveTransactions.Free;
  FUserNames.Free;
  FBackoutCount.Free;
  FDeleteCount.Free;
  FExpungeCount.Free;
  FInsertCount.Free;
  FPurgeCount.Free;
  FReadIdxCount.Free;
  FReadSeqCount.Free;
  FUpdateCount.Free;
  inherited Destroy;
end;

procedure TGDSDatabaseInfo.CheckConnection;
begin
  if FGDSConnection = nil then
    RaiseError(SConnectionNotDefined);
  if FGDSConnection.GDS = nil then
    RaiseError(SConnectionNotConnected);
end;

procedure TGDSDatabaseInfo.GetDatabaseInfo(LocalBuffer: IntPtr; DbInfoCommand: IntPtr; LocalBufferLength: Short = LocalBufferLen);
var
  Res: ISC_STATUS;
begin
  FGDSConnection.GDS.Busy;
  try
    Res := FGDSConnection.GDS.isc_database_info(FGDSConnection.FStatusVector,
      FGDSConnection.FDatabaseHandle, 1, DbInfoCommand, LocalBufferLength, LocalBuffer);
  finally
    FGDSConnection.GDS.Release;
  end;
  FGDSConnection.Check(Res);
end;

function TGDSDatabaseInfo.GetDBFileName: string;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_db_id);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Marshal.WriteByte(PtrOffset(LocalBuffer, 5 + Marshal.ReadByte(LocalBuffer, 4)), 0);
    Result := string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, 5)));
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetDBSiteName: string;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
  Length: integer;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_db_id);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Length := Marshal.ReadByte(PtrOffset(LocalBuffer, 5 + Integer(Marshal.ReadByte(LocalBuffer, 4))));
    Result := string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, 6 + Marshal.ReadByte(LocalBuffer, 4)), Length));
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetIsRemoteConnect: boolean;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_db_id);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result := Marshal.ReadByte(LocalBuffer, 3) = 4;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetDBImplementationNo: integer;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_implementation);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result := Marshal.ReadByte(LocalBuffer, 3);
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetDBImplementationClass: integer;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_implementation);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result := Marshal.ReadByte(LocalBuffer, 4);
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetVersion(DatabaseInfoCommand: integer): string;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, DatabaseInfoCommand);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    if Marshal.ReadByte(LocalBuffer, 0) = DatabaseInfoCommand then
      Result := string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, 5),
        Marshal.ReadByte(LocalBuffer, 4)))
    else
      Result := '';
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetUserNames: TStringList;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
  i, UserLength: integer;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(HugeLocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_user_names);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand, HugeLocalBufferLen);
    Result := FUserNames;
    FUserNames.Clear;
    i := 0;
    while Marshal.ReadByte(LocalBuffer, i) = isc_info_user_names do
    begin
      Inc(i, 3);
      UserLength := Marshal.ReadByte(LocalBuffer, i);
      Inc(i, 1);
      Result.Add(string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, i), UserLength)));
      Inc(i, UserLength);
    end;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetBackoutCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_backout_count, FBackoutCount);
end;

function TGDSDatabaseInfo.GetDeleteCount: TstringList;
begin
  Result := GetOperationCounts(isc_info_delete_count, FDeleteCount);
end;

function TGDSDatabaseInfo.GetExpungeCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_expunge_count, FExpungeCount);
end;

function TGDSDatabaseInfo.GetInsertCount: TstringList;
begin
  Result := GetOperationCounts(isc_info_insert_count, FInsertCount);
end;

function TGDSDatabaseInfo.GetPurgeCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_purge_count, FPurgeCount);
end;

function TGDSDatabaseInfo.GetReadIdxCount: TstringList;
begin
  Result := GetOperationCounts(isc_info_read_idx_count, FReadIdxCount);
end;

function TGDSDatabaseInfo.GetReadSeqCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_read_seq_count, FReadSeqCount);
end;

function TGDSDatabaseInfo.GetUpdateCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_update_count, FUpdateCount);
end;

function TGDSDatabaseInfo.GetStringDatabaseInfo(DatabaseInfoCommand: integer): string;
var
  DbInfoCommand: IntPtr;
  LocalBuffer: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, DatabaseInfoCommand);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Marshal.WriteByte(PtrOffset(LocalBuffer, 4 + Marshal.ReadByte(LocalBuffer, 3)), 0);
    Result := string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, 4),
      Marshal.ReadByte(PtrOffset(LocalBuffer, 4 + Marshal.ReadByte(PtrOffset(LocalBuffer, 3))))));
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DBInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetBooleanDatabaseInfo(DatabaseInfoCommand: integer): boolean;
begin
  if DatabaseInfoCommand = isc_info_db_read_only then
    if (ODSMajorVersion < 10) then begin
      Result := False;
      Exit;
    end;
  Result := Byte(GetIntDatabaseInfo(DatabaseInfoCommand)) <> 0;
end;

function TGDSDatabaseInfo.GetIntDatabaseInfo(DatabaseInfoCommand: integer): integer;
var
  DbInfoCommand: IntPtr;
  LocalBuffer: IntPtr;
begin
  Result := -1;
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, DatabaseInfoCommand);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    if (DatabaseInfoCommand = isc_info_db_SQL_dialect) and (Marshal.ReadByte(LocalBuffer, 0) <> isc_info_db_SQL_dialect) then
      Result := 1
    else
    if Marshal.ReadByte(LocalBuffer, 0) <> DatabaseInfoCommand then
      Result := 0
    else
    if (DatabaseInfoCommand = isc_info_base_level) then
      Result := Marshal.ReadByte(LocalBuffer, 4)
    else
    case Marshal.ReadInt16(LocalBuffer, 1) of
      0: Result := 0;
      1: Result := Marshal.ReadByte(LocalBuffer, 3);
      2: Result := Marshal.ReadInt16(LocalBuffer, 3);
      4: Result := Marshal.ReadInt32(LocalBuffer, 3);
      8: Result := Integer(Marshal.ReadInt64(LocalBuffer, 3));
      else
        RaiseError('Unsupported length');
    end;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetActiveTransactions: TStringList;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
  i: integer;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, frb_info_active_transactions);
    if FActiveTransactions = nil then
      FActiveTransactions := TStringList.Create;
    Result := FActiveTransactions;
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result.Clear;
    i := 0;
    while Marshal.ReadByte(LocalBuffer, i) = frb_info_active_transactions do begin
      Inc(i, 3);
      Result.Add(IntToStr(Marshal.ReadInt32(LocalBuffer, i)));
      Inc(i, 4);
    end;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DBInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetOperationCounts(DataBaseInfoCommand: integer; var FOperation: TStringList): TStringList;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
  i, QtdTables, IdTable, QtdOperations: integer;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(HugeLocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, DataBaseInfoCommand);
    if FOperation = nil then
      FOperation := TStringList.Create;
    Result := FOperation;
    GetDatabaseInfo(LocalBuffer, DbInfoCommand, HugeLocalBufferLen);
    FOperation.Clear;
    QtdTables := Marshal.ReadInt16(LocalBuffer, 1) div 6;
    for i := 0 to QtdTables - 1 do
    begin
      IdTable := Marshal.ReadInt16(LocalBuffer, 3 + (i * 6));
      QtdOperations := Marshal.ReadInt32(LocalBuffer, 5 + (i * 6));
      FOperation.Add(IntToStr(IdTable) + '=' + IntToStr(QtdOperations));
    end;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DBInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetMajorServerVersion: integer;
begin
  CheckConnection;
  Result := FGDSConnection.GetMajorServerVersion;
end;

function TGDSDatabaseInfo.GetMinorServerVersion: integer;
begin
  CheckConnection;
  Result := FGDSConnection.GetMinorServerVersion;
end;

{ TGDSConnection }

constructor TGDSConnection.Create;
begin
  inherited Create;

  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FDatabaseHandle := Marshal.AllocHGlobal(SizeOf(TISC_DB_HANDLE));
  Marshal.WriteIntPtr(FDatabaseHandle, nil);
  SetLength(FDPB, 0);
  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  FDatabaseInfo := TGDSDatabaseInfo.Create;
  FDatabaseInfo.Owner := Self;
  FSSLOptions := TGDSSSLOptions.Create;

  FProtocol := _TCP;
  FIPVersion := DefValIPVer;
  FPort := DefaultIBDACSPort;
  FQueryCharLength := True;
  FSQLDialect := 3;
  FTrustedAuthentication := False;
  FNoDBTriggers := False;
{$IFDEF LITE}
  FWireCompression := False;
{$ENDIF}
  FlushProps;
end;

destructor TGDSConnection.Destroy;
begin
  if FGDS <> nil then
    FGDS.UnRegisterNotification(GDSNotification);

  Disconnect;

  FParams.Free;
  FDatabaseInfo.Free;
  FSSLOptions.Free;

  SetLength(FDPB, 0);
  Marshal.FreeHGlobal(FDatabaseHandle);
  Marshal.FreeHGlobal(FStatusVector);

  inherited;
end;

procedure TGDSConnection.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    IBCError(FStatusVector, True, Component);
end;

procedure TGDSConnection.IBCError(var StatusVector: TStatusVector; UseCallback: boolean; Component: TObject);
var
  Msg, SqlErrMsg: string;
  ErrorNumber, ErrorCode: integer;
  Fail: boolean;
begin
  GDS.Busy;
  try
    ErrorCode := GDS.isc_sqlcode(StatusVector);
  finally
    GDS.Release;
  end;
  GDS.GetIBError(ErrorCode, False{FUseUnicode}, ErrorNumber, StatusVector, Msg, SqlErrMsg);
  FLastError := ErrorCode;
  try
    raise EIBCError.Create(FLastError, ErrorNumber, {$IFDEF LITE}SqlErrMsg + '.' + DALineSeparator + {$ENDIF}Msg, SqlErrMsg);
  except
    on E: EIBCError do begin
    {$IFNDEF NODBACCESS}
      E.Component := Component;
    {$ENDIF}
      Fail := True;
      if UseCallback then begin
        GDS.AlerterFatalError := GDS.AlerterFatalError and EIBCError.IsFatalError(ErrorNumber);
        try
          DoError(E, Fail);
        finally
          GDS.AlerterFatalError := False;
        end;
      end;
      if Fail then
        raise
      else
        Abort;
    end;
  end;
end;

procedure TGDSConnection.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TGDSConnection.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;


procedure TGDSConnection.CheckInactive;
begin
  if FConnected then
    RaiseError(SConnectionOpen);
end;

procedure TGDSConnection.InitCommandProp(Command: TCRCommand);
begin
  Command.SetTransaction(Self.GetInternalTransaction);
end;

procedure TGDSConnection.InitRecordSetProp(RecordSet: TCRRecordSet);
begin
  inherited;

  RecordSet.SetProp(prExtendedFieldsInfo, False);
end;

procedure TGDSConnection.InitGDS;
var
  ClientLib: string;
  DatabaseExtension: string;
begin
  ClientLib := FClientLibrary;
  FirebirdDatabaseDetected := False;

  if ClientLib = '' then begin
    DatabaseExtension := LowerCase(ExtractFileExt(FDatabase));
    FirebirdDatabaseDetected := (DatabaseExtension = '.fdb') or (DatabaseExtension = '.fb');
  end;

{$IFDEF AUTOREFCOUNT}
  if FGDS <> nil then
    FGDS.__ObjRelease;
{$ENDIF}
  FGDS := GDSList.GetGDS(ClientLib);
  if FGDS <> nil then begin
  {$IFDEF AUTOREFCOUNT}
    FGDS.__ObjAddRef;
  {$ENDIF}
    FGDS.RegisterNotification(GDSNotification);
  end;
end;

procedure TGDSConnection.CreateDPB;
const
  DPBPrefix = 'isc_dpb_';
  DPBConstantNames: array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file_name',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach',
    'gbak_ods_version',
    'gbak_ods_minor_version',
    'set_group_commit',
    'gbak_validate',
    'no_db_triggers',
    'trusted_auth',
    'process_name',
    'instance_name',
    'old_overwrite',
    'utf8_filename',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    'sys_encrypt_password',
    '',
    '',
    '',
    '',
    ''
  );

  procedure CheckDPBSize (NeededSize: integer; var DPBSize: integer);
  begin
    if DPBSize < NeededSize then begin
      DPBSize := ((NeededSize div 2048) + 1) * 2048;
      SetLength(FDPB, DPBSize);
    end;
  end;

var
  i, j: integer;
  DPBVal: UShort;
  ParamName, ParamValue: string;
  DPBSize: integer;
  vCharset: string;
  Buf: TBytes;
begin
  SetLength(Buf, 0);
  DPBSize := 2048; //Start size that must cover common user needs
  SetLength(FDPB, DPBSize);
  FDPBLength := 1;
  FDPB[0] := isc_dpb_version1;
  if FUseUnicode and (UpperCase(FCharset) <> 'UTF8') then
    vCharset := 'UNICODE_FSS'
  else
    vCharset := FCharset;
  try
    CheckDPBSize(FDPBLength + 6 + Length(vCharset) + Length(FPassword) + Length(FUsername), DPBSize);
    if FUsername <> '' then begin
      FDPB[FDPBLength] := isc_dpb_user_name;
      if (GDS.ClientVersion.Version >= 3.0) and (GDS.ClientVersion.Version < 6) then
        Buf := Encoding.UTF8.GetBytes(FUsername)
      else
        Buf := Encoding.Default.GetBytes(FUsername);
      FDPB[FDPBLength + 1] := Length(Buf);
      CopyBuffer(@Buf[0], @FDPB[FDPBLength + 2], Length(Buf));
      Inc(FDPBLength, 2 + Length(Buf));
    end;

    if FPassword <> '' then begin
      FDPB[FDPBLength] := isc_dpb_password;
      if (GDS.ClientVersion.Version >= 3.0) and (GDS.ClientVersion.Version < 6) then
        Buf := Encoding.UTF8.GetBytes(FPassword)
      else
        Buf := Encoding.Default.GetBytes(FPassword);
      FDPB[FDPBLength + 1] := Length(Buf);
      CopyBuffer(@Buf[0], @FDPB[FDPBLength + 2], Length(Buf));
      Inc(FDPBLength, 2 + Length(Buf));
    end;
    if vCharset <> '' then begin
      FDPB[FDPBLength] := isc_dpb_lc_ctype;
      Buf := Encoding.Default.GetBytes(vCharset);
      FDPB[FDPBLength + 1] := Length(Buf);
      CopyBuffer(@Buf[0], @FDPB[FDPBLength + 2], Length(Buf));
      Inc(FDPBLength, 2 + Length(Buf));
    end;

    if FRole <> '' then begin
      FDPB[FDPBLength] := isc_dpb_sql_role_name;
      if (GDS.ClientVersion.Version >= 2.5) and (GDS.ClientVersion.Version < 6) then
        Buf := Encoding.UTF8.GetBytes(FRole)
      else
        Buf := Encoding.Default.GetBytes(FRole);
      FDPB[FDPBLength + 1] := Length(Buf);
      CopyBuffer(@Buf[0], @FDPB[FDPBLength + 2], Length(Buf));
      Inc(FDPBLength, 2 + Length(Buf));
    end;

    if FTrustedAuthentication then begin
      CheckDPBSize(FDPBLength + 3, DPBSize);
      FDPB[FDPBLength] := isc_dpb_trusted_auth;
      FDPB[FDPBLength + 1] := 1;
      FDPB[FDPBLength + 2] := 0;
      Inc(FDPBLength, 3);
    end;

    if FNoDBTriggers then begin
      CheckDPBSize(FDPBLength + 3, DPBSize);
      FDPB[FDPBLength] := isc_dpb_no_db_triggers;
      FDPB[FDPBLength + 1] := 1;
      FDPB[FDPBLength + 2] := 1;
      Inc(FDPBLength, 3);
    end;

    //SQLDialect
    CheckDPBSize(FDPBLength + 3, DPBSize);
    FDPB[FDPBLength] := isc_dpb_sql_dialect;
    FDPB[FDPBLength + 1] := 1;
    FDPB[FDPBLength + 2] := Byte(FSQLDialect);
    Inc(FDPBLength, 3);

    for i := 0 to FParams.Count - 1 do begin
      ParamName := LowerCase(Trim(FParams.Names[i]));
      if ParamName = '' then
        ParamName := LowerCase(Trim(FParams[i]));
      if (FParams.Names[i] = '') and (ParamName <> 'utf8_filename') then
        ParamName := '';
      if ParamName = '' then
        continue;
      ParamValue := Trim(Copy(FParams[i], Pos('=', FParams[i]) + 1, Length(FParams[i])));
      if (Pos(DPBPrefix, ParamName) = 1) then
        Delete(ParamName, 1, Length(DPBPrefix));
      DPBVal := 0;
      { Find the parameter }
      if ParamName = 'wirecompression' then begin
        DPBVal := isc_dpb_config;
        ParamName := 'WireCompression';
      end
      else
      for j := 1 to isc_dpb_last_dpb_constant do
        if (ParamName = DPBConstantNames[j]) then begin
          DPBVal := j;
          Break;
        end;
      case DPBVal of
        isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc, isc_dpb_sys_user_name,
        isc_dpb_license, isc_dpb_encrypt_key, isc_dpb_lc_messages, isc_dpb_lc_ctype,
        isc_dpb_process_name, isc_dpb_instance_name, isc_dpb_old_file_name, isc_dpb_sys_encrypt_password: begin
          CheckDPBSize(FDPBLength + 2 + Length(ParamValue), DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := Length(ParamValue);
          Encoding.Default.GetBytes(ParamValue, {$IFDEF MOBILE}0{$ELSE}1{$ENDIF}, Length(ParamValue), FDPB, FDPBLength + 2);
          Inc(FDPBLength, 2 + Length(ParamValue));
        end;
        isc_dpb_config:
          if (GDS.ClientVersion.Version >= 3.0) and (GDS.ClientVersion.Version < 6) then begin
            ParamValue := ParamName + '=' + ParamValue;
            CheckDPBSize(FDPBLength + 2 + Length(ParamValue), DPBSize);
            FDPB[FDPBLength] := DPBVal;
            FDPB[FDPBLength + 1] := Length(ParamValue);
            Encoding.Default.GetBytes(ParamValue, {$IFDEF MOBILE}0{$ELSE}1{$ENDIF}, Length(ParamValue), FDPB, FDPBLength + 2);
            Inc(FDPBLength, 2 + Length(ParamValue));
          end;
        isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
        isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify, isc_dpb_sql_dialect,
        isc_dpb_overwrite, isc_dpb_old_file_size:  begin
          CheckDPBSize(FDPBLength + 3, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 1;
          FDPB[FDPBLength + 2] := Byte(StrToInt(ParamValue));
          Inc(FDPBLength, 3);
        end;
        isc_dpb_online_dump, isc_dpb_old_overwrite:  begin
          CheckDPBSize(FDPBLength + 2, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := Byte(StrToInt(ParamValue));
          Inc(FDPBLength, 2);
        end;
        isc_dpb_utf8_filename:  begin
          CheckDPBSize(FDPBLength + 2, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 0;
          Inc(FDPBLength, 2);
        end;
        isc_dpb_sweep: begin
          CheckDPBSize(FDPBLength + 3, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 1;
          FDPB[FDPBLength + 2] := isc_dpb_records;
          Inc(FDPBLength, 3);
        end;
        isc_dpb_sweep_interval: begin
          CheckDPBSize(FDPBLength + 6, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 4;
          j := StrToInt(ParamValue);
          FDPB[FDPBLength + 2] := Byte(j);
          FDPB[FDPBLength + 3] := Byte(j shr 8);
          FDPB[FDPBLength + 4] := Byte(j shr 16);
          FDPB[FDPBLength + 5] := Byte(j shr 24);
          Inc(FDPBLength, 6);
        end;
        isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
        isc_dpb_quit_log:
        begin
          CheckDPBSize(FDPBLength + 3, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 1;
          FDPB[FDPBLength + 2] := 0;
          Inc(FDPBLength, 3);
        end;
        isc_dpb_sql_role_name: begin
          if (GDS.ClientVersion.Version >= 2.5) and (GDS.ClientVersion.Version < 6) then
            Buf := Encoding.UTF8.GetBytes(ParamValue)
          else
            Buf := Encoding.Default.GetBytes(ParamValue);
          CheckDPBSize(FDPBLength + 2 + Length(Buf), DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := Length(Buf);
          CopyBuffer(@Buf[0], @FDPB[FDPBLength + 2], Length(Buf));
          Inc(FDPBLength, 2 + Length(Buf));
        end;
        else begin
          if (DPBVal > 0) and
             (DPBVal <= isc_dpb_last_dpb_constant) then
            RaiseError(Format(SDPBConstantNotSupported, [DPBConstantNames[DPBVal]]))
          else
            RaiseError(Format(SDPBConstantUnknown,[ParamName]));
        end;
      end;
    end;
  except
    SetLength(FDPB, 0);
    FDPBLength := 0;
    raise;
  end;
end;

function TGDSConnection.GetCharLength: integer;
begin
  Result := FCharLength;
end;

function TGDSConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TGDSCommand;
end;

function TGDSConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TGDSRecordSet;
end;

function TGDSConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TGDSTransaction;
end;

{$IFNDEF LITE}

function TGDSConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TGDSLoader;
end;

function TGDSConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TGDSMetaData;
end;

class function TGDSConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TGDSCommand.GetMapRulesClass;
end;

function TGDSConnection.CreateObject(DataType: Word; Transaction: TCRTransaction): TDBObject;
begin
  case DataType of
    dtArray:
      Result := TCustomIBCArray.Create(Self, TGDSTransaction(Transaction));
    else
      raise Exception.Create('Invalid object type');
  end;
end;

{$ENDIF}

procedure TGDSConnection.Connect(const ConnectString: string);
var
  FullDBName: string;
  FullDBNameB: TBytes;
  Res: ISC_STATUS;
begin
  SetLength(FullDBNameB, 0);

  if not FConnected then begin
    InitGDS;

    FullDBName := GetFullDatabaseName(FServer, FPort, FProtocol, FDatabase, IsClientFB3, FIPVersion, FSSLOptions.GetSSLString);
    if (GDS.ClientVersion.Version >= 2.5) and (GDS.ClientVersion.Version < 6) then begin
      FullDBNameB := Encoding.UTF8.GetBytes(FullDBName);
      if FParams.IndexOf('utf8_filename') < 0 then
        FParams.Add('utf8_filename');
    {$IFDEF LITE}
      if FWireCompression and (FParams.IndexOfName('WireCompression') < 0) then
        FParams.Add('WireCompression=True');
    {$ENDIF LITE}
    end
    else
      FullDBNameB := Encoding.Default.GetBytes(FullDBName);


    if FParamsChanged then begin
      CreateDPB;
      FParamsChanged := False;
    end;

    try
      GDS.Busy;
      try
        Res := GDS.isc_attach_database(FStatusVector, Length(FullDBNameB), FullDBNameB, FDatabaseHandle, FDPBLength, {$IFDEF FPC}PChar{$ENDIF}(FDPB));
      finally
        GDS.Release;
      end;
      Check(Res);
    except
      on EFailOver do; //This exception raised after FailOver Reconnect in DoError so DBHandle is valid
      else begin
        //Check(isc_detach_database(FStatusVector, FDatabaseHandle));
        Marshal.WriteIntPtr(FDatabaseHandle, nil);
        raise;
      end;
    end;

    inherited;
    FConnected := True;

    if FDisconnectedMode then begin
      //Clean cached values on connection to obtain current values from DB
      FServerVersion := '';
      FServerVersionFull := '';
      FMajorServerVersion := 0;
      FMinorServerVersion := -1;
      FReadOnlyCached := False;
      FIsFBServerCached := False;
    end;

    CheckClientSQLDialect;
    // get connection parameters
    //if (FCharset <> '') or FQueryCharLength then
    GetDatabaseParameters;

  end;
end;

procedure TGDSConnection.CheckClientSQLDialect;
begin
  FDBSQLDialect := FDatabaseInfo.DBSQLDialect;
  if (FDBSQLDialect < FSQLDialect) then
    FSQLDialect := FDBSQLDialect;
end;

procedure TGDSConnection.Disconnect;
var
  Res: ISC_STATUS;
begin
  if FConnected then begin
    if FInternalTransaction.GetInTransaction then begin
      Assert(TGDSTransaction(FInternalTransaction).GDS <> nil);
        FInternalTransaction.Commit;
    end;
    FConnected := False;

    try
      if FNativeConnection and not GDS.AlerterFatalError then begin
        GDS.Busy;
        try
          Res := GDS.isc_detach_database(FStatusVector, FDatabaseHandle);
        finally
          GDS.Release;
        end;
        Check(Res);
      end;
    finally
      Marshal.WriteIntPtr(FDatabaseHandle, nil);
      FNativeConnection := True;
      if not FDisconnectedMode then
        FlushProps;
    end;
  end;
end;

procedure TGDSConnection.Ping;
var
  Command: TCRCommand;
  ImplicitStart: boolean;
begin
  Command := CreateCommand;
  try
    Command.SetSQL('SELECT 1 FROM RDB$DATABASE');
    ImplicitStart := Command.GetTransaction.GetInTransaction;
    if not ImplicitStart then
      Command.GetTransaction.StartTransaction;
    try
      Command.Execute;
    finally
      if not ImplicitStart then
        Command.GetTransaction.Commit;
    end;
  finally
    ReleaseCommand(Command);
  end;
end;

{ Connect control }
procedure TGDSConnection.AssignConnect(Source: TCRConnection);
var
  Src: TGDSConnection;
  InTransaction: boolean;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TGDSConnection(Source);
      FPort := Src.FPort;
      FCharset := Src.FCharset;
      FCharLength := Src.FCharLength;

      FCharsetId := Src.FCharsetId;
      FDBSQLDialect := Src.FDBSQLDialect;
      FUseUnicode := Src.FUseUnicode;
      FClientLibrary := Src.FClientLibrary;

      FGDS := Src.FGDS;

      SetDatabaseHandle(Src.GetDatabaseHandle);

      if GDS.ClientVersion.Version < 3 then begin
        InTransaction := Src.FInternalTransaction.GetInTransaction;
        if InTransaction then
          Src.FInternalTransaction.Commit;

        FInternalTransaction.AssignConnect(Src.FInternalTransaction);

        if InTransaction then
          Src.FInternalTransaction.StartTransaction;
      end;
    end
    else
      SetDatabaseHandle(nil);
  end;
end;

function TGDSConnection.GetLastError: integer;
begin
  Result := FLastError;
end;

procedure TGDSConnection.SetLastError(Value: integer);
begin
  FLastError := Value;
end;

procedure TGDSConnection.GetDatabaseParameters;
var
  SQL: string;
  RecordSet: TGDSRecordSet;
  RecBuf: IntPtr;
  v: variant;
  CharsetIdNeeded: boolean;
  vCharset: string;
begin
  if FConnected then begin
    if FUseUnicode and (UpperCase(FCharset) <> 'UTF8') then
      vCharset := 'UNICODE_FSS'
    else
      vCharset := FCharset;
    CharsetIdNeeded := vCharset <> '';
    RecordSet := TGDSRecordSet.Create;
    try
      RecordSet.SetConnection(Self);
      RecordSet.SetTransaction(FInternalTransaction);
      FInternalTransaction.StartTransaction;
      SQL := 'SELECT CST.RDB$CHARACTER_SET_ID CONN_CH, CST.RDB$BYTES_PER_CHARACTER CONN_CH_LEN,' + #13#10 +
        'DB.RDB$CHARACTER_SET_NAME DB_CH, DB_CST.RDB$BYTES_PER_CHARACTER DB_CH_LEN, ' +
        'DB_CST.RDB$CHARACTER_SET_ID DB_CH_ID' + #13#10 +
        'FROM RDB$CHARACTER_SETS CST, RDB$DATABASE DB, RDB$CHARACTER_SETS DB_CST' + #13#10 +
        'WHERE ((DB_CST.RDB$CHARACTER_SET_NAME = DB.RDB$CHARACTER_SET_NAME) ' +
        'OR ((DB_CST.RDB$CHARACTER_SET_NAME = ''NONE'') AND (DB.RDB$CHARACTER_SET_NAME IS NULL)))'; //NULL for character set NONE. https://firebirdsql.org/file/documentation/reference_manuals/fblangref25-en/html/fblangref-appx04-database.html
        if CharsetIdNeeded then
          SQL := SQL + #13#10 + 'AND (CST.RDB$CHARACTER_SET_NAME = UPPER(''' + vCharset + '''))'
        else
          SQL := SQL + #13#10 + 'AND ((CST.RDB$CHARACTER_SET_NAME = DB.RDB$CHARACTER_SET_NAME) ' +
            'OR ((CST.RDB$CHARACTER_SET_NAME = ''NONE'') AND (DB.RDB$CHARACTER_SET_NAME IS NULL)))'; //NULL for character set NONE. https://firebirdsql.org/file/documentation/reference_manuals/fblangref25-en/html/fblangref-appx04-database.html
      RecordSet.SetSQL(SQL);
      RecordSet.FCommand.SetProp(prQueryRowsAffected, False);
      RecordSet.SetProp(prFlatBuffers, False);
      RecordSet.SetProp(prLongStrings, True);
      RecordSet.SetProp(prExtendedFieldsInfo, False);
      RecordSet.Open;
      RecordSet.FetchAll;
      RecordSet.SetToBegin;

      FCharsetId := 0;
      FCharLength := 1;

      if RecordSet.RecordCount = 0 then
        Exit;
      Assert(RecordSet.RecordCount = 1, 'GetDatabaseParameters');

      RecBuf := nil;
      RecordSet.AllocRecBuf(RecBuf);
      try
        while True do begin
          RecordSet.GetNextRecord(RecBuf);
          if RecordSet.Eof then
            Break;
          if FQueryCharLength then begin
            RecordSet.GetFieldAsVariant(RecordSet.Fields[1], RecBuf, v); // Connection Charset Bytes
            FCharLength := v;
          end;
          if CharsetIdNeeded then begin
            RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v); // Connection Charset ID
            FCharsetId := v
          end;
          if not RecordSet.GetNull(RecordSet.Fields[2], RecBuf) then begin
            RecordSet.GetFieldAsVariant(RecordSet.Fields[3], RecBuf, v); // Database Charset Bytes
            FDBCharLength := v;
            //RecordSet.GetFieldAsVariant(3, RecBuf, v); // Database Charset Name
            RecordSet.GetFieldAsVariant(RecordSet.Fields[4], RecBuf, v); // Database Charset ID
            FDBCharsetId := v;
          end
          else begin
            FDBCharsetId := 0;
            FDBCharLength := 1;
          end;
        end;
      finally
        if RecBuf <> nil then
          RecordSet.FreeRecBuf(RecBuf);
      end;
    finally
      RecordSet.Free;
      FInternalTransaction.Commit;
    end;
  end;
end;

function TGDSConnection.GetCharLength(CharsetID: integer; const RelName: string): integer;
begin
  case CharsetID and $ff of
    5, 6, 8, 44, 56, 57, 64:
      Result := 2;
    3:
      if (Pos('RDB$', RelName) = 1) and ((GetMajorServerVersion >= 8) or (GetMajorServerVersion < 2)) then
        Result := 1
      else
        Result := 3;
    4, 59:
      Result := 4;
  else
    Result := 1;
  end;
end;

procedure TGDSConnection.FlushProps;
begin
  FDBSQLDialect := 3;
  FCharsetId := 0;
  FServerVersion := '';
  FServerVersionFull := '';
  FMajorServerVersion := 0;
  FMinorServerVersion := -1;
  FReadOnlyCached := False;
  FIsFBServerCached := False;
  if FQueryCharLength then
    FCharLength := 0;
end;

procedure TGDSConnection.GDSNotification;
begin
  Disconnect;

  FGDS := nil;
end;

function TGDSConnection.SetProp(Prop: integer; const Value: variant): boolean;
var
  OldCharsetId: word;
  OldCharset: string;
  S: string;
begin
  Result := True;
  case Prop of
    prUseUnicode: begin
      FUseUnicode := Boolean(Value);
      FQueryCharLength :=  True;
      FParamsChanged := True;
    end;
    prRole:
      if FRole <> string(Value) then begin
        FRole := string(Value);
        FParamsChanged := True;
      end;
    prEnableMemos:
      FEnableMemos := Boolean(Value);
    prOptimizedNumerics:
      FOptimizedNumerics := Boolean(Value);
    prEnableLargeint:
      FEnableLargeint := Boolean(Value);
    prSQLDialect:
      FSQLDialect := Integer(Value);
    prCharLength:
      if (FQueryCharLength and (Word(Value) <> 0))
        or (FCharLength <> Word(Value))
      then begin
        FCharLength := Word(Value);
        FQueryCharLength := FCharLength = 0;
        if FCharLength = 0 then
          GetDatabaseParameters;
      end;
    prCharset: begin
      S := Value;
      if FCharset <> S then begin
        OldCharset := FCharset;
        OldCharsetId := FCharsetId;
        FCharset := S;
        FParamsChanged := True;
        try
          GetDatabaseParameters;
        except
          FCharset := OldCharset;
          FCharsetId := OldCharsetId;
          raise;
        end;
      end;
    end;
    prClientLibrary:
      FClientLibrary := Value;
    prSimpleNumericMap:
      FSimpleNumericMap := Value;
    prDatabase:
      FDatabase := Value;
    prProtocol: begin
      FProtocol := _TIBCProtocol(Value);
    end;
    prIPVersion:
      FIPVersion := Value;
    prPort:
      FPort := Value;
    prTrustedAuthentication:
      if FTrustedAuthentication <> Value then begin
        FTrustedAuthentication := Value;
        FParamsChanged := True;
      end;
    prNoDBTriggers:
      if FNoDBTriggers <> Value then begin
        FNoDBTriggers := Value;
        FParamsChanged := True;
      end;
    prForceUsingDefaultPort:
      ForceUsingDefaultPort := Value;
    prForceUnloadClientLibrary:
      ForceUnloadClientLibrary := Value;
    prConnectionParams: begin
      if not ((Pool <> nil) and (Value = '') and (FParams.IndexOf('utf8_filename') >= 0)) then begin
        if (Value <>  FParams.Text) and FConnected then
          Disconnect;
        FParams.Text := '';
        ExtractStrings([';'], [], PChar(Trim(VarToStr(Value))), FParams);
        FParamsChanged := True;
      end;
    end;
  {$IFDEF LITE}
    prWireCompression: begin
      FWireCompression := Value;
      FParamsChanged := True;
    end;
  {$ENDIF}
    //SSL
    prUseSSL:
      FSSLOptions.Enabled := Value;
    prServerPublicFile:
      FSSLOptions.FServerPublicFile := Value;
    prServerPublicPath:
      FSSLOptions.FServerPublicPath := Value;
    prClientCertFile:
      FSSLOptions.FClientCertFile := Value;
    prClientPassPhraseFile:
      FSSLOptions.FClientPassPhraseFile := Value;
    prClientPassPhrase:
      FSSLOptions.FClientPassPhrase := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prMaxStringSize:
      Value := MaxSQLLength div FCharLength;
    prUseUnicode:
      Value := FUseUnicode;
    prRole:
      Value := FRole;
    prCharLength:
      if FQueryCharLength then
        Value := 0
      else
        Value := FCharLength;
    prCharset:
      Value := FCharset;
    prClientLibrary:
      Value := FClientLibrary;
    prDatabase:
      Value := FDatabase;
    prProtocol:
      Value := integer(FProtocol);
    prIPVersion:
      Value := FIPVersion;
    prEnableMemos:
      Value := FEnableMemos;
    prSQLDialect:
      Value := FSQLDialect;
    prDBSQLDialect:
      Value := FDBSQLDialect;
    prReadOnly: begin
      if not FReadOnlyCached then begin
        FReadOnly := FDatabaseInfo.ReadOnly;
        FReadOnlyCached := True;
      end;
      Value := FReadOnly;
    end;
    prPort:
      Value := FPort;
    prTrustedAuthentication:
      Value := FTrustedAuthentication;
    prNoDBTriggers:
      Value := FNoDBTriggers;
    prForceUsingDefaultPort:
      Value := ForceUsingDefaultPort;
    prConnectionParams:
      Value := Trim(FParams.Text);
  {$IFDEF LITE}
    prWireCompression:
      Value := FWireCompression;
  {$ENDIF}
    //SSL
    prUseSSL:
      Value := FSSLOptions.Enabled;
    prServerPublicFile:
      Value := FSSLOptions.FServerPublicFile;
    prServerPublicPath:
      Value := FSSLOptions.FServerPublicPath;
    prClientCertFile:
      Value := FSSLOptions.FClientCertFile;
    prClientPassPhraseFile:
      Value := FSSLOptions.FClientPassPhraseFile;
    prClientPassPhrase:
      Value := FSSLOptions.FClientPassPhrase;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TGDSConnection.SetUsername(const Value: string);
begin
  if Value <> FUsername then
    FParamsChanged := True;
  inherited SetUsername(Value);
end;

procedure TGDSConnection.SetPassword(const Value: string);
begin
  if Value <> FPassword then
    FParamsChanged := True;
  inherited SetPassword(Value);
end;

function TGDSConnection.CheckIsValid: boolean;
begin
  if not FIsValid then
    Result := False
  else begin
    FIsValid := True;
    try
      if FDatabaseInfo.BaseLevel = 0 then
        ;
    except
      FIsValid := False;
    end;
    Result := FIsValid;
  end;
end;

function TGDSConnection.CanChangeDatabase: boolean;
begin
  Result := False;
end;

{ InterBase support}
function TGDSConnection.GetDatabaseHandle: TISC_DB_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FDatabaseHandle);
end;

procedure TGDSConnection.SetDatabaseHandle(Value: TISC_DB_HANDLE);
begin
  if Value <> GetDatabaseHandle then begin
    if FNativeConnection then
      CheckInactive
    else
      Disconnect;

    Marshal.WriteIntPtr(FDatabaseHandle, Value);
    FNativeConnection := Value = nil;
    FConnected := Value <> nil;

    if (Value <> nil) and (FGDS = nil) then begin
      FGDS := GDSList.GetGDS(FClientLibrary);
      if FGDS <> nil then begin
      {$IFDEF AUTOREFCOUNT}
        FGDS.__ObjAddRef;
      {$ENDIF}
        FGDS.RegisterNotification(GDSNotification);
      end;
    end;
  end;
end;

procedure TGDSConnection.CreateDatabase;
var
  tr_handle: PISC_TR_HANDLE;
  PParams: IntPtr;
  InSQLDA: TSQLDA;
  FullDBName: string;
  Res: ISC_STATUS;
  ASQL: AnsiString;
  UTF8ParamDeleted: boolean;
begin
  CheckInactive;
  if FGDS = nil then begin
    FGDS := GDSList.GetGDS(FClientLibrary);
    if FGDS <> nil then begin
    {$IFDEF AUTOREFCOUNT}
      FGDS.__ObjAddRef;
    {$ENDIF}
      FGDS.RegisterNotification(GDSNotification);
    end;
  end;
  FullDBName := GetFullDatabaseName(FServer, FPort, FProtocol, FDatabase, IsClientFB3, FIPVersion, FSSLOptions.GetSSLString);
  UTF8ParamDeleted := False;
  if FParams.IndexOf('utf8_filename') >= 0 then begin
    FParams.Delete(FParams.IndexOf('utf8_filename'));
    UTF8ParamDeleted := True;
  end;
  ASQL := AnsiString('CREATE DATABASE ''' + FullDBName + ''' ' + FParams.Text);
  PParams := Marshal.StringToHGlobalAnsi(ASQL);
  if UTF8ParamDeleted then
    FParams.Add('utf8_filename');
  if GDS.ClientVersion.Version >= 7 then
    InSQLDA := TSQLDA.Create(Self, vtGDS7)
  else
    InSQLDA := TSQLDA.Create(Self, vtGDS);
  InSQLDA.AllocSQLDA(0);
  tr_handle := Marshal.AllocHGlobal(SizeOf(TISC_TR_HANDLE));
  try
    Marshal.WriteIntPtr(tr_handle, nil);
    GDS.Busy;
    try
      Res := GDS.isc_dsql_execute_immediate(FStatusVector, FDatabaseHandle,
               tr_handle, 0, PParams, FSQLDialect, InSQLDA.FXSQLDA);
    finally
      GDS.Release;
    end;
    Check(Res);
    FDBSQLDialect := FDatabaseInfo.DBSQLDialect;
    FConnected := True;
  finally
    Marshal.FreeCoTaskMem(PParams);
    InSQLDA.Free;
    Marshal.FreeHGlobal(tr_handle);
  end;
end;

procedure TGDSConnection.DropDatabase;
var
  Res: ISC_STATUS;
begin
  if not FConnected then
    RaiseError(SConnectionClosed);

  try
    GDS.Busy;
    try
      Res := GDS.isc_drop_database(FStatusVector, FDatabaseHandle);
    finally
      GDS.Release;
    end;
    Check(Res);
  finally
    Marshal.WriteIntPtr(FDatabaseHandle, nil);
    FNativeConnection := True;
    FConnected := False;
  end;
end;

procedure TGDSConnection.OnlineDump(const DumpFileName: string; const Overwrite: boolean; const AdditionalDumpFiles: array of const);
var
  FullDBName: string;
  FileName: string;
  FullDBNameB: TBytes;
  OldParams: TStrings;
  Res: ISC_STATUS;
  FilesCnt, FileSize, i: integer;
  HasSizes: boolean;
begin
  CheckInactive;
  if FGDS = nil then begin
    FGDS := GDSList.GetGDS(FClientLibrary);
    if FGDS <> nil then begin
    {$IFDEF AUTOREFCOUNT}
      FGDS.__ObjAddRef;
    {$ENDIF}
      FGDS.RegisterNotification(GDSNotification);
    end;
  end;
  FullDBName := GetFullDatabaseName(FServer, FPort, FProtocol, FDatabase, IsClientFB3, FIPVersion, FSSLOptions.GetSSLString);
  OldParams := TStringList.Create;
  try
    OldParams.Assign(GetParams);
    FParams.Clear;
    FParams.Add('online_dump=1');
    if Overwrite then
      FParams.Add('old_overwrite=1')
    else
      FParams.Add('old_overwrite=0');

    FParams.Add('old_file_name=' + DumpFileName);

    FilesCnt := Length(AdditionalDumpFiles);
    HasSizes := (FilesCnt > 1) and (AdditionalDumpFiles[1].VType = vtInteger);
    i := 0;
    while i < FilesCnt do begin
      if not (AdditionalDumpFiles[i].VType in [{$IFNDEF NEXTGEN}vtString, vtAnsiString, {$ENDIF}vtWideString]) then
        RaiseError('');

      case AdditionalDumpFiles[i].VType of
      {$IFNDEF NEXTGEN}
        vtAnsiString:
          FileName := string(AdditionalDumpFiles[i].VAnsiString);
        vtString:
          FileName := string(AdditionalDumpFiles[i].VString);
      {$ENDIF}
        vtWideString:
          FileName := string(AdditionalDumpFiles[i].VWideString);
      else
        RaiseError(Format(SInvalidArgument, ['AdditionalDumpFiles']));
      end;
      FParams.Add('old_file_name=' + FileName);
      inc(i);

      if HasSizes then begin
        if (i >= FilesCnt) or (AdditionalDumpFiles[i].VType <> vtInteger) then
          RaiseError(Format(SInvalidArgument, ['AdditionalDumpFiles']));

        FileSize := AdditionalDumpFiles[i].VInteger;
        FParams.Add('old_file_size=' + IntToStr(FileSize));
        inc(i);
      end;
    end;

    if (GDS.ClientVersion.Version >= 2.5) and (GDS.ClientVersion.Version < 6) then begin
      FullDBNameB := Encoding.UTF8.GetBytes(FullDBName);
      if FParams.IndexOf('utf8_filename') < 0 then
        FParams.Add('utf8_filename');
    {$IFDEF LITE}
      if FWireCompression and (FParams.IndexOfName('WireCompression') < 0) then
        FParams.Add('WireCompression=True');
    {$ENDIF LITE}
    end
    else
      FullDBNameB := Encoding.Default.GetBytes(FullDBName);

    CreateDPB;

    GDS.Busy;
    try
      Res := GDS.isc_attach_database(FStatusVector, Length(FullDBNameB), FullDBNameB, FDatabaseHandle, FDPBLength, {$IFDEF FPC}PChar{$ENDIF}(FDPB));
    finally
      GDS.Release;
    end;
    Check(Res);
  finally
    SetParams(OldParams);
    FParamsChanged := True;
    OldParams.Free;
  end;
end;

procedure TGDSConnection.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

function TGDSConnection.GetParams: TStrings;
begin
  Result := FParams;
end;

function TGDSConnection.IsFBServer: boolean;
begin
  if not FIsFBServerCached then begin
    FIsFBServer := FDatabaseInfo.FBVersion <> '';
    FIsFBServerCached := True;
  end;
  Result := FIsFBServer;
end;

function TGDSConnection.IsClientFB3: boolean;
begin
  if FGDS <> nil then
    Result := FGDS.ClientVersion.IsFirebird and (FGDS.ClientVersion.Version >= 3.0)
  else
    Result := False;
end;

function TGDSConnection.GetMajorServerVersion: integer;
begin
  if FServerVersion = '' then
    GetServerVersion;

  Result := FMajorServerVersion;
end;

function TGDSConnection.GetMinorServerVersion: integer;
begin
  if FServerVersion = '' then
    GetServerVersion;

  Result := FMinorServerVersion;
end;

function TGDSConnection.GetServerVersion: string;
var
  DotPos, StartPos, EndPos: integer;
begin
  if FServerVersion = '' then begin
    if FServerVersionFull = '' then
      GetServerVersionFull;

    if Length(FServerVersionFull) >= 7 then begin
      DotPos := Pos('.', FServerVersionFull);
      StartPos := DotPos - 1;
      EndPos := Pos(' ', FServerVersionFull) - 1;
      if EndPos <= 0 then
        EndPos := Length(FServerVersionFull);
      if (StartPos > 1) and (EndPos >= StartPos) then begin
        if CharInSet(FServerVersionFull[StartPos - 1], ['0'..'9']) then
          Dec(StartPos);

        FServerVersion := Copy(FServerVersionFull, StartPos, EndPos - StartPos + 1);

        FMajorServerVersion := StrToInt(Copy(FServerVersionFull, StartPos, DotPos - StartPos));
        FMinorServerVersion := StrToInt(FServerVersionFull[DotPos + 1]);
      end;
    end;
  end;

  Result := FServerVersion;
end;

function TGDSConnection.GetServerVersionFull: string;
begin
  if FServerVersionFull = '' then begin
    if IsFBServer then
      FServerVersionFull := FDatabaseInfo.FBVersion
    else
      FServerVersionFull := FDatabaseInfo.Version;
  end;
  Result := FServerVersionFull;
end;

function TGDSConnection.GetClientVersion: string;
begin
  Assert(GDS <> nil);
  Result := GDS.ClientVersion.VersionStr;
end;

{TGDSTransaction}

constructor TGDSTransaction.Create;
begin
  inherited Create;

  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FTransactionHandle := nil;
  SetLength(FTPB, 0);

  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
end;

destructor TGDSTransaction.Destroy;
begin
//  CloseTransaction;
  FTransactionHandle := nil;
  FParams.Free;
  SetLength(FTPB, 0);
  Marshal.FreeHGlobal(FStatusVector);
  inherited;
end;

procedure TGDSTransaction.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    if FConnections.Count = 1 then
      TGDSConnection(FConnections[0]).IBCError(FStatusVector, True, Component)
    else
      IBCError(FStatusVector, True);
end;

procedure TGDSTransaction.IBCError(var StatusVector: TStatusVector; UseCallback: boolean);
var
  Msg, SqlErrMsg: string;
  ErrorNumber, ErrorCode: integer;
  Fail: boolean;
begin
  GDS.Busy;
  try
    ErrorCode := GDS.isc_sqlcode(StatusVector);
  finally
    GDS.Release;
  end;
  GDS.GetIBError(ErrorCode, False, ErrorNumber, StatusVector, Msg, SqlErrMsg);
  FLastError := ErrorCode;
  try
    raise EIBCError.Create(FLastError, ErrorNumber, Msg, SqlErrMsg);
  except
    on E: EIBCError do begin
    {$IFNDEF NODBACCESS}
      E.Component := Component;
    {$ENDIF}
      Fail := True;
      if UseCallback then
        if Assigned(OnError) then
          OnError(E, Fail);
      if Fail then
        raise
      else
        Abort;
    end;
  end;
end;

procedure TGDSTransaction.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TGDSTransaction.ParamsChanging(Sender: TObject);
begin
  if FNativeTransaction then
    CheckInActive;
end;

procedure TGDSTransaction.Commit;
var
  Res: ISC_STATUS;
  ErrorNumber: NativeInt;
  NeedClose: boolean;
begin
  CheckActive;
  if FNativeTransaction then begin
    if not GDS.AlerterFatalError then begin
      GDS.Busy;
      try
        Res := GDS.isc_commit_transaction(FStatusVector, @FTransactionHandle);
      finally
        GDS.Release;
      end;
      if Res > 0 then begin
        ErrorNumber := NativeInt(PtrOffset(FStatusVector, 1 * SizeOf(ISC_STATUS))^);

        NeedClose := EIBCError.IsFatalError(ErrorNumber);
      end
      else
        NeedClose := True;
      if NeedClose then
        FTransactionHandle := nil;
      Check(Res);
    end
    else
      FTransactionHandle := nil;
  end
  //else
  //  RaiseError(SCantEndSharedTransaction);
end;

procedure TGDSTransaction.CommitRetaining;
var
  Res: ISC_STATUS;
begin
  CheckActive;
  GDS.Busy;
  try
    Res := GDS.isc_commit_retaining(FStatusVector, @FTransactionHandle);
  finally
    GDS.Release;
  end;
  Check(Res);
end;

procedure TGDSTransaction.Rollback;
var
  Res: ISC_STATUS;
begin
  CheckActive;
  if FNativeTransaction then begin
    if not GDS.AlerterFatalError then begin
      GDS.Busy;
      try
        Res := GDS.isc_rollback_transaction(FStatusVector, @FTransactionHandle);
      finally
        GDS.Release;
      end;
      FTransactionHandle := nil;
      Check(Res);
    end
    else
      FTransactionHandle := nil;
  end
  //else
  //  RaiseError(SCantEndSharedTransaction);
end;

procedure TGDSTransaction.RollbackRetaining;
var
  Res: ISC_STATUS;
begin
  CheckActive;
  GDS.Busy;
  try
    Res := GDS.isc_rollback_retaining(FStatusVector, @FTransactionHandle);
  finally
    GDS.Release;
  end;
  Check(Res);
end;

procedure TGDSTransaction.CreateTPB;
const
  TPBPrefix = 'isc_tpb_';
  TPBConstantNames: array[1..isc_tpb_last_tpb_constant] of string = (
    'consistency',
    'concurrency',
    'shared',
    'protected',
    'exclusive',
    'wait',
    'nowait',
    'read',
    'write',
    'lock_read',
    'lock_write',
    'verb_time',
    'commit_time',
    'ignore_limbo',
    'read_committed',
    'autocommit',
    'rec_version',
    'no_rec_version',
    'restart_requests',
    'no_auto_undo',
    'no_savepoint',
    'lock_timeout'
  );

  procedure CheckTPBSize (NeededSize: integer; var TPBSize: integer);
  begin
    if TPBSize < NeededSize then begin
      TPBSize := ((NeededSize div 2048) + 1) * 2048;
      SetLength(FTPB, TPBSize);
    end;
  end;

var
  TPBSize: integer;

  procedure TPBIsolationLevel;
  begin
    case FIsolationLevel of
      ilRepeatableRead,
      ilSnapshot: begin
         CheckTPBSize(FTPBLength + 3, TPBSize);
         FTPB[FTPBLength] := isc_tpb_concurrency;
         FTPB[FTPBLength + 1] := isc_tpb_nowait;
         inc(FTPBLength, 3);
      end;
      ilReadCommitted: begin
         CheckTPBSize(FTPBLength + 4, TPBSize);
         FTPB[FTPBLength] := isc_tpb_read_committed;
         FTPB[FTPBLength + 1] := isc_tpb_rec_version;
         FTPB[FTPBLength + 2] := isc_tpb_nowait;
         inc(FTPBLength, 4);
      end;
      ilIsolated: begin
         CheckTPBSize(FTPBLength + 2, TPBSize);
         FTPB[FTPBLength] := isc_tpb_consistency;
         inc(FTPBLength, 2);
      end;
    else
      RaiseError(SUnsupportedIsolationLevel);
    end;
    if FReadOnly then
      FTPB[FTPBLength - 1] := isc_tpb_read
    else
      FTPB[FTPBLength - 1] := isc_tpb_write;
  end;

var
  i, j: integer;
  TPBVal: UShort;
  ParamName, ParamValue: string;
  Found: boolean;

begin
  TPBSize := 2048; //Start size that must cover common user needs
  SetLength(FTPB, TPBSize);
  FTPBLength := 1;
  FTPB[0] := isc_tpb_version3;

  if FIsolationLevel <> ilCustom then
    TPBIsolationLevel; //Set isolation Level

  try
    for i := 0 to FParams.Count - 1 do begin
      if Pos('=', FParams[i]) > 0 then begin
        ParamName := Trim(LowerCase(FParams.Names[i]));
        ParamValue := Copy(FParams[i], Pos('=', FParams[i]) + 1, Length(FParams[i]));
      end
      else begin
        ParamName := Trim(LowerCase(FParams[i]));
        ParamValue := '';
      end;
      if ParamName = '' then
        continue;
      if (Pos(TPBPrefix, ParamName) = 1) then
        Delete(ParamName, 1, Length(TPBPrefix));
      TPBVal := 0;
      { Find the parameter }
      for j := 1 to isc_tpb_last_tpb_constant do
        if (ParamName = TPBConstantNames[j]) then begin
          TPBVal := j;
          Break;
        end;

      Found := False;
      for j := 0 to FTPBLength - 1 do
        if FTPB[j] = TPBVal then begin
          Found := True;
          Break;
        end;

      if Found then
        continue;

      case TPBVal of
        isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
        isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
        isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
        isc_tpb_read_committed, isc_tpb_autocommit, isc_tpb_rec_version, isc_tpb_no_rec_version,
        isc_tpb_restart_requests, isc_tpb_no_auto_undo, isc_tpb_no_savepoint: begin
          CheckTPBSize(FTPBLength + 1, TPBSize);
          FTPB[FTPBLength] := TPBVal;
          Inc(FTPBLength)
        end;
        unique_isc_tpb_lock_timeout: begin
          CheckTPBSize(FTPBLength + 6, TPBSize);
          FTPB[FTPBLength] := isc_tpb_lock_timeout;
          FTPB[FTPBLength + 1] := 4;
          j := StrToIntDef(ParamValue, 10);
          FTPB[FTPBLength + 2] := Byte(j);
          FTPB[FTPBLength + 3] := Byte(j shr 8);
          FTPB[FTPBLength + 4] := Byte(j shr 16);
          FTPB[FTPBLength + 5] := Byte(j shr 24);
          Inc(FTPBLength, 6);
        end;
        isc_tpb_lock_read, isc_tpb_lock_write: begin
          CheckTPBSize(FTPBLength + 2 + Length(ParamValue), TPBSize);
          FTPB[FTPBLength] := TPBVal;
          FTPB[FTPBLength + 1] := Length(ParamValue);
          Encoding.Default.GetBytes(ParamValue, {$IFDEF MOBILE}0{$ELSE}1{$ENDIF}, Length(ParamValue), FTPB, FTPBLength + 2);
          Inc(FTPBLength, 2 + Length(ParamValue));
        end;
        else begin
          if (TPBVal > 0) and
             (TPBVal <= isc_tpb_last_tpb_constant) then
            RaiseError(Format(STPBConstantNotSupported, [TPBConstantNames[TPBVal]]))
          else
            RaiseError(Format(STPBConstantUnknown, [ParamName]));
        end;
      end;
    end;
  except
    SetLength(FTPB, 0);
    FTPBLength := 0;
    raise;
  end;
end;

procedure TGDSTransaction.StartTransaction;
var
  pTEB: PISC_TEB_ARRAY;
  TPBHandle, PTPB: IntPtr;
  i: integer;
  TEBOffset: integer;
  Res: ISC_STATUS;
begin
  CheckInactive;

  if FConnections.Count = 0 then
    RaiseError(SNoConnectionsInTransaction);

  for i := 0 to FConnections.Count - 1 do
   if FConnections[i] <> nil then begin
     if not FConnections[i].GetConnected then
       RaiseError(SConnClosedOnTrStart);
   end;

  FGDS := TGDSConnection(FConnections[0]).GDS;

  if FParamsChanged then begin
    CreateTPB;
    FParamsChanged := False;
  end;

  pTEB := Marshal.AllocHGlobal(FConnections.Count * SizeOfISC_TEB);
  try
    TPBHandle := AllocGCHandle(FTPB, True);
    PTPB := GetAddrOfPinnedObject(TPBHandle);
    TEBOffset := 0;
    for i := 0 to FConnections.Count - 1 do begin
      Marshal.WriteIntPtr(pTEB, TEBOffset, TGDSConnection(FConnections[i]).FDatabaseHandle);
      Marshal.WriteInt32(pTEB, TEBOffset + OffsetOf_TISC_TEB_tpb_length, FTPBLength);
      Marshal.WriteIntPtr(pTEB, TEBOffset + OffsetOf_TISC_TEB_tpb_address, PTPB);
      Inc(TEBOffset, SizeOfISC_TEB);
    end;
    try
      try
        GDS.Busy;
        try
          Res := GDS.isc_start_multiple(FStatusVector, @FTransactionHandle, FConnections.Count, pTEB);
        finally
          GDS.Release;
        end;
        Check(Res);
      except
        FTransactionHandle := nil;
        raise;
      end;
    finally
      FreeGCHandle(TPBHandle);
    end;
  finally
    Marshal.FreeHGlobal(pTEB);
  end;
  FNativeTransaction := True;
end;

procedure TGDSTransaction.Reset;
begin
  FTransactionHandle := nil;
end;

procedure TGDSTransaction.AssignConnect(Source: TCRTransaction);
begin
  inherited;

  FTransactionHandle := TGDSTransaction(Source).FTransactionHandle;
  FGDS := TGDSTransaction(Source).FGDS;
end;

function TGDSTransaction.CanRestoreAfterFailover: boolean;
begin
  Result := FReadOnly and (FIsolationLevel = ilReadCommitted);
end;

{ Savepoint control }
procedure TGDSTransaction.ExecuteImmediate(const SQL: string);
var
{$IFDEF NEXTGEN}
  ASQL: AnsiString;
{$ENDIF}
  PSQL: IntPtr;
  Conn: TGDSConnection;
  Res: ISC_STATUS;
  i: integer;
begin
{$IFDEF NEXTGEN}
  ASQL := AnsiString(SQL);
  PSQL := Marshal.StringToHGlobalAnsi(ASQL);
{$ELSE}
  PSQL := Marshal.StringToHGlobalAnsi(AnsiString(SQL));
{$ENDIF}
  try
    for i := 0 to FConnections.Count - 1 do begin
      Conn := TGDSConnection(FConnections[i]);
      GDS.Busy;
      try
        Res := GDS.isc_dsql_execute_immediate(FStatusVector, Conn.FDatabaseHandle,
          @FTransactionHandle, 0, PSQL, Conn.FSQLDialect, nil);
      finally
        GDS.Release;
      end;
      Check(Res);
    end;
  finally
    Marshal.FreeCoTaskMem(PSQL);
  end;
end;

procedure TGDSTransaction.ReleaseSavepoint(const Name: string);
begin
  CheckActive;
  ExecuteImmediate('RELEASE SAVEPOINT ' + Name);
{ This code unsupported in FB and raises AV when we execute it from
  IB client against FB database

  pString := Marshal.StringToHGlobalAnsi(Name);
  try
    Check(isc_release_savepoint(FStatusVector, FTransactionHandle, pString));
  finally
    Marshal.FreeCoTaskMem(pString);
  end;
}
end;

procedure TGDSTransaction.RollbackToSavepoint(const Name: string);
begin
  CheckActive;
  ExecuteImmediate('ROLLBACK TO SAVEPOINT ' + Name);
{ This code unsupported in FB and raises AV when we execute it from
  IB client against FB database

  pString := Marshal.StringToHGlobalAnsi(Name);
  try
    Check(isc_rollback_savepoint(FStatusVector, FTransactionHandle, pString, 0));
  finally
    Marshal.FreeCoTaskMem(pString);
  end;
}
end;

procedure TGDSTransaction.Savepoint(const Name: string);
begin
  CheckActive;
  ExecuteImmediate('SAVEPOINT ' + Name);
{ This code unsupported in FB and raises AV when execute it from
  IB client against FB database

  pString := Marshal.StringToHGlobalAnsi(Name);
  try
    Check(isc_start_savepoint(FStatusVector, FTransactionHandle, pString));
  finally
    Marshal.FreeCoTaskMem(pString);
  end;
}
end;

{ Transaction control}
procedure TGDSTransaction.AssignTransaction(Source: TGDSTransaction);
begin
  if Source <> Self then begin
    CheckInactive;
    if Source <> nil then
      FTransactionHandle := Source.FTransactionHandle
    else
      FTransactionHandle := nil;
  end;
end;

function TGDSTransaction.GetInTransaction: boolean;
begin
  Result := FTransactionHandle <> nil;
end;

function TGDSTransaction.GetMultiTransactionID: Int64;
var
  Res: ISC_STATUS;
  ItemListBuff: TBytes;
  ResultBuff: IntPtr;
  Len: integer;
begin
  CheckActive;
  SetLength(ItemListBuff, 1);
  ItemListBuff[0] := isc_info_tra_id;
  ResultBuff := Marshal.AllocHGlobal(64);
  try
    GDS.Busy;
    try
      Res := GDS.isc_transaction_info(FStatusVector, @FTransactionHandle, 1, ItemListBuff, 64, ResultBuff);
    finally
      GDS.Release;
    end;

    Check(Res);
    Len := Marshal.ReadByte(ResultBuff);
    case Len of
      4: Result := Marshal.ReadInt32(ResultBuff, 3);
      8: Result := Marshal.ReadInt64(ResultBuff, 3);
    else
      Result := 0;
      RaiseError('Unsupported length of TransactionID (isc_info_tra_id)');
    end;
  finally
    Marshal.FreeHGlobal(ResultBuff);
  end;
end;

function TGDSTransaction.GetConnection(Idx: integer): TGDSConnection;
begin
  Result := TGDSConnection(FConnections[Idx]);
end;

function TGDSTransaction.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prTransactionParams:
      if FIsolationLevel = ilCustom then begin
        FParams.Text := '';
        ExtractStrings([';'], [], PChar(Trim(VarToStr(Value))), FParams);
      end
      else
        FParams.Text := '';
    prIsolationLevel:
      if FIsolationLevel <> TCRIsolationLevel(Value) then begin
        FIsolationLevel := TCRIsolationLevel(Value);
        FParamsChanged := True;
      end;
    prTransactionReadOnly:
      if FReadOnly <> Value then begin
        FReadOnly := Value;
        FParamsChanged := True;
      end
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSTransaction.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prTransactionParams:
      Value := Trim(FParams.Text)
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TGDSTransaction.SetParams(const Value: TStrings);
var
  ParamStr: string;
begin
  if FIsolationLevel = ilCustom then begin
    FParams.Text := '';
    ParamStr := Value.Text;
    if ParamStr <> '' then
      ExtractStrings([';'], [], PChar(ParamStr), FParams);
  end
  else
    FParams.Assign(Value);
end;

function TGDSTransaction.GetParams: TStrings;
begin
  Result := FParams;
end;

{ TGDSCommand }

constructor TGDSCommand.Create;
var
  GUID: TGUID;
  UniqCursor: String;
  UniqCursorLen: integer;

  function GUIDToString(const GUID: TGUID): string;
  {$IFDEF CLR}
   var
     SB: StringBuilder;
   begin
     SB := StringBuilder.Create(35);
     try
       Result := UpperCase(SysUtils.GUIDToString(GUID));
       SB.Append('IBC');
       SB.Append(Copy(Result, 1, 8));
       SB.Append(Copy(Result, 10, 4));
       SB.Append(Copy(Result, 15, 4));
       SB.Append(Copy(Result, 20, 4));
       SB.Append(Copy(Result, 25, 12));
       Result := SB.ToString;
     finally
       SB.Free;
     end;
  {$ELSE}
   begin
     SetLength(Result, 33);
     StrLFmt(PChar(Result), 33,'A%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
       [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
       GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
  {$ENDIF}
  end;
begin
  inherited Create;

  FAutoCommit := False;
  FCacheBlobs := True;
  FCacheArrays := True;
  FSQLType := SQL_UNKNOWN;
  FParsedSQLType := qtUnknown;
  FIntegerPrecision := IntegerPrecision;
  FFloatPrecision := FloatPrecision;
  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FState := csInactive;
  FStmtHandle := Marshal.AllocHGlobal(Sizeof(TISC_STMT_HANDLE));
  Marshal.WriteIntPtr(FStmtHandle, nil);
  CreateGuid(GUID);
  UniqCursor := GUIDToString(GUID);
  UniqCursorLen := Length(UniqCursor);
  FCursor := Marshal.AllocHGlobal(UniqCursorLen + 1);
  Marshal.WriteByte(FCursor, UniqCursorLen, 0);
  CopyBufferAnsi(AnsiString(UniqCursor), FCursor, UniqCursorLen);
  FQueryRowsAffected := True;
  FInfoTypeBuf := Marshal.AllocHGlobal(SizeOf(Byte) * 5);
  FRowsAffectedBuf := Marshal.AllocHGlobal(LocalBufferLen);
  FRelAliasesBuf := Marshal.AllocHGlobal(LocalBufferLen);
end;

destructor TGDSCommand.Destroy;
begin
  if GetCursorState > csInactive then
    Finish;
  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);
  FInXSQLDA.Free;
  FOutXSQLDA.Free;
  Marshal.FreeHGlobal(FStmtHandle);
  Marshal.FreeHGlobal(FStatusVector);
  Marshal.FreeHGlobal(FCursor);
  Marshal.FreeHGlobal(FInfoTypeBuf);
  Marshal.FreeHGlobal(FRowsAffectedBuf);
  Marshal.FreeHGlobal(FRelAliasesBuf);

  inherited;
end;

procedure TGDSCommand.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure TGDSCommand.CheckDatabase;
begin
  if FConnection = nil then
    RaiseError(SConnectionNotDefined);
  if not FConnection.GetConnected then
    RaiseError(SConnectionClosed);
end;

procedure TGDSCommand.CheckTransaction;
begin
  if (FTransaction = nil) then
    RaiseError(STransactionNotAssigned);
  if not FTransaction.GetInTransaction then
    RaiseError(SNotInTransaction);
end;

procedure TGDSCommand.CheckActive;
begin
  if GetCursorState = csInactive then
    RaiseError(SCursorNotOpened);
end;

function TGDSCommand.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

procedure TGDSCommand.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    FConnection.IBCError(FStatusVector, True, Component);
end;

function TGDSCommand.GetActive: boolean;
begin
  Result := FState <> csInactive;
end;

procedure TGDSCommand.GetRowsAffected;
var
  Res: ISC_STATUS;
begin
  if (not FQueryRowsAffected) or (not GetPrepared or (FSQLType = SQL_DDL) or
    (FSQLType = SQL_SET_GENERATOR) or (FSQLType = SQL_ROLLBACK) or (FSQLType = SQL_COMMIT) or
    ((FSQLType = SQL_EXEC_PROCEDURE) and (FParsedSQLType <> qtInsertReturning) and (FParsedSQLType <> qtExecuteBlock))) then begin
    FRowsInserted := 0;
    FRowsUpdated := 0;
    FRowsDeleted := 0;
    FRowsAffected := 0;
  end
  else begin
    Marshal.WriteByte(FInfoTypeBuf, isc_info_sql_records);
    GDS.Busy;
    try
      Res := GDS.isc_dsql_sql_info(FStatusVector, FStmtHandle, 1, FInfoTypeBuf, LocalBufferLen, FRowsAffectedBuf);
    finally
      GDS.Release;
    end;
    Check(Res);

    if (Marshal.ReadByte(FRowsAffectedBuf) <> isc_info_sql_records) then
      RaiseError(SSQLInfoError);

    FRowsUpdated := Marshal.ReadInt32(FRowsAffectedBuf, 6);
    FRowsDeleted := Marshal.ReadInt32(FRowsAffectedBuf, 13);
    FRowsInserted := Marshal.ReadInt32(FRowsAffectedBuf, 27);
    FRowsAffected := FRowsInserted + FRowsUpdated + FRowsDeleted;
  end;
end;

procedure TGDSCommand.DescribeParam(const Name: string; Param: TIBCParamDesc; SQLVAR: TSQLVARAccessor);
var
  DataType: SmallInt;
  DataSize: integer;
  DBType: SmallInt;
  Precision, Scale: integer;
begin
  DataType := -1;
  DataSize := -1;

  if Name = '' then //Output parameters
    Param.SetName(SQLVAR.GetAliasName)
  else
    Param.SetName(Name);
  Param.Scale := 0;
  Param.Precision := 0;
  Param.FSize := 0;

  DBType := SQLVAR.sqltype;
  Param.FDBType := DBType;

  case DBType of
    SQL_IB_VARYING, SQL_IB_TEXT: begin
      DataSize := SQLVAR.sqllen;
      if Byte(SQLVAR.sqlsubtype) = CH_OCTETS then begin
        if DBType = SQL_IB_TEXT then
          Param.FDataType := dtBytes
        else
          Param.FDataType := dtVarBytes;
      end
      else
        if FConnection.FUseUnicode then begin
          DataSize := DataSize div FConnection.GetCharLength(SQLVAR.sqlsubtype, SQLVAR.sqlname);
          if DBType = SQL_IB_TEXT then
            Param.FDataType := dtFixedWideChar
          else
            Param.FDataType := dtWideString;
          SQLVAR.sqllen := DataSize * FConnection.GetCharLength(SQLVAR.sqlsubtype, SQLVAR.sqlname); //to set proper out param len if DB Charset is not UTF8
        end
        else
          if DBType = SQL_IB_TEXT then
            Param.FDataType := dtFixedChar
          else
            Param.FDataType := dtString;
    end;
    SQL_IB_FLOAT: begin
      DataType := dtFloat;
      Param.SetSubDataType(dtSingle);
    end;
    SQL_IB_DOUBLE: begin
      DataType := dtFloat;
      Param.SetSubDataType(dtFloat);
    end;
    SQL_IB_SHORT, SQL_IB_LONG, SQL_IB_INT64: begin
      case DBType of
        SQL_IB_SHORT: begin
          Precision := IntegerPrecision div 2;
          DataType := dtSmallint;
        end;
        SQL_IB_LONG: begin
          Precision := IntegerPrecision;
          DataType := dtInteger;
        end;
        else begin
          Precision := IntegerPrecision * 2;
          DataType := dtInt64;
        end;
      end;
      Scale := SQLVAR.sqlscale;
      Param.Precision := Precision;
      Param.Scale := ABS(Scale);
      if (Scale <> 0) or (FConnection.FSimpleNumericMap and (SQLVAR.sqlsubtype <> 0)) then begin
        if (Scale >= (-4)) and (FConnection.EnableBCD or FEnableBCD) then
          DataType := dtBCD
        else if (FConnection.FSQLDialect <> 1) and (FConnection.EnableFMTBCD or FEnableFMTBCD) then
          DataType := dtFMTBCD
        else
          DataType := dtFloat;
      end;
    end;
    SQL_IB_BOOLEAN,
    SQL_FB_BOOLEAN: begin
      DataType := dtBoolean;
      Param.SetSubDataType(dtBoolean);
    end;
    SQL_IB_TYPE_TIME: begin
      DataType := dtTime;
    end;
    SQL_IB_TYPE_DATE: begin
      DataType := dtDate;
    end;
    SQL_IB_TIMESTAMP: begin
      DataType := dtDateTime;
    end;
    SQL_IB_QUAD, SQL_IB_BLOB: begin
      if FConnection.FEnableMemos and (SQLVAR.sqlsubtype = isc_blob_text) then begin
        if FConnection.FUseUnicode and
          ((FConnection.FDBCharsetId = CH_UNICODE_FSS) or
           (FConnection.FDBCharsetId = CH_UTF8) or
           ((FConnection.GDS.ClientVersion.Version > 7) and (FConnection.FDBCharsetId = CH_UTF8_IBXE)) or
           (FConnection.FDBCharsetId = CH_NONE))
        then
          DataType := dtWideMemo
        else
          DataType := dtMemo;
      end
      else
        DataType := dtBlob;
    end;
    SQL_IB_ARRAY:
      DataType := dtArray;
  end;

  if ((DataType = -1) or (DataType  = SmallInt(Param.FDataType))) and ((DataSize = -1) or (DataSize = Param.FSize)) then
    Exit;

  if (DataType <> -1) and (DataType <> SmallInt(Param.FDataType)) then
    Param.SetDataType(DataType);

  if DataSize <> Param.FSize then
    Param.SetSize(DataSize);
end;

procedure TGDSCommand.DescribeParams(Full: boolean = True; InParamsCount: integer = 0);
var
  i, OutParamsCount: integer;
  Param: TIBCParamDesc;
begin
  FillSQLDA(Full);

  if Full then begin
    InParamsCount := FInXSQLDA.GetSqld;
    for i := 0 to InParamsCount - 1 do begin
      if i >= Params.Count then begin
        Param := TIBCParamDesc(AddParam);
        Param.SetParamType(pdInput);
      end
      else
        Param := TIBCParamDesc(Params[i]);
      DescribeParam(Param.GetName, Param, FInXSQLDA.Vars[i]);
    end;
  end;

  if FSQLType <> SQL_SELECT then begin
    OutParamsCount := FOutXSQLDA.GetSqld;
    for i := 0 to OutParamsCount - 1 do begin
      if i >= Params.Count - InParamsCount then begin
        Param := TIBCParamDesc(AddParam);
        Param.SetParamType(pdOutput);
      end
      else
        Param := TIBCParamDesc(Params[InParamsCount + i]);
      if Full or (Param.GetDataType = dtUnknown) then
        DescribeParam(Param.GetName, Param, FOutXSQLDA.Vars[i]);
    end;
  end
  else
    OutParamsCount := 0;

  if Full then
    for i := Params.Count - 1 downto InParamsCount + OutParamsCount do
      Params.Delete(i);
end;

procedure TGDSCommand.CreateBatchCommand;
var
  BatchCommand: TGDSCommand;
begin
  inherited;

  BatchCommand := TGDSCommand(GetBatchCommand);
  BatchCommand.SetTransaction(FTransaction);
  BatchCommand.AfterExecute := Self.AfterExecute;
end;

procedure TGDSCommand.InternalExecute;
var
  OldPrepared: boolean;
  i, ItemIndex: integer;
  Param: TIBCParamDesc;
  Res: integer;
  OldCursorState: TCursorState;
  OwnerCommand: TGDSCommand;
  ParamsCount: integer;
  OwnerParamsCount: integer;
  Blob: TIBCBlob;
  Arr: TCustomIBCArray;
  WriteSize: word;
  Ptr: IntPtr;
  WriteMode: TIBCBlobMode;
begin
  OldPrepared := GetPrepared;
  if not OldPrepared then
    Prepare;
  try
    BindParams;

    OwnerCommand := TGDSCommand(BatchOwner);
    if OwnerCommand = nil then begin
      ParamsCount := Params.Count;
      OwnerParamsCount := ParamsCount;
    end
    else begin
      OwnerParamsCount := OwnerCommand.FParamsInfo.Count;
      if OwnerParamsCount > 0 then
        ParamsCount := OwnerParamsCount * FBatchIters
      else
        ParamsCount := 0;
    end;

    //writing Blob parameters to create valid Blob_ID
    for i := 0 to ParamsCount - 1 do begin
      ItemIndex := FBatchOffset + i div OwnerParamsCount;
      if OwnerCommand = nil then
        Param := TIBCParamDesc(Params[i])
      else
        Param := TIBCParamDesc(OwnerCommand.FParamsInfo[i mod OwnerParamsCount].ParamRef);
      if not Param.GetIsBound then
        Continue;
      case Param.FDataType of
        dtBlob, dtMemo, dtWideMemo: begin
          Blob := TIBCBlob(Param.GetItemObject(ItemIndex));
          if Blob <> nil then begin
            if not Blob.FForceCachedMode then
              Blob.FCached := FCacheBlobs;
            Blob.FStreamed := FStreamedBlobs;
            if not Assigned(GetBlobData) then
              Blob.Cached := True;

            if Blob.Cached or Blob.FStreamed then begin
            {$IFDEF HAVE_COMPRESS}
              case FCompressBlob of
                cbServer, cbClientServer:
                  Blob.Compressed := True;
                else
                  Blob.Compressed := False;
              end;
            {$ENDIF}
              if Param.GetParamType <= pdInput then begin
                if Assigned(GetBlobData) and Blob.FStreamed then begin
                  Ptr := nil;
                  WriteSize := 0;
                  GetBlobData(Param, Ptr, WriteSize, WriteMode);

                  if WriteMode = bmValue then begin
                    Blob.WriteBlob;
                    Continue;
                  end;

                  Blob.Clear;
                  if WriteMode = bmNull then
                    Param.SyncIndicator(ItemIndex, True)
                  else begin
                    Blob.PrepareBlob;
                    try
                      Param.SyncIndicator(ItemIndex, False);
                      while WriteSize <> 0 do begin
                        Blob.WritePieceBlob(WriteSize, Ptr);
                        GetBlobData(Param, Ptr, WriteSize, WriteMode);
                      end;
                      Blob.CheckInfo; //To achieve information about real blob state on server side
                                 //Current blobID will be invalid after insert or update operation
                    finally
                      Blob.CloseBlob;
                    end;
                  end;
                end
                else
                  if Blob.Cached then
                    if not Param.GetItemNull(ItemIndex) then
                      Blob.WriteBlob
              end;
            end;
          end;
        end;
        dtArray:
          if (Param.GetParamType <= pdInput) and not Param.GetItemNull(ItemIndex) then begin
            Arr := TCustomIBCArray(Param.GetItemObject(ItemIndex));
            if (Arr <> nil) and Arr.Cached then
              Arr.WriteArray;
          end;
      end;
    end;

    //Execute statement
    OldCursorState := GetCursorState;
    SetCursorState(csExecuting);

    GDS.Busy;
    try
      case FSQLType of
        SQL_SELECT:
          Res := GDS.isc_dsql_execute2(FStatusVector, @FTransaction.FTransactionHandle,
            FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA, nil);
        SQL_SELECT_FOR_UPD: begin
          Res := GDS.isc_dsql_execute2(FStatusVector, @FTransaction.FTransactionHandle,
            FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA, nil);
          Check(GDS.isc_dsql_set_cursor_name(FStatusVector, FStmtHandle, FCursor, 0));
        end;
        SQL_EXEC_PROCEDURE: begin
          Res := GDS.isc_dsql_execute2(FStatusVector, @FTransaction.FTransactionHandle,
            FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA, FOutXSQLDA.FXSQLDA);
        end
        else
          Res := GDS.isc_dsql_execute(FStatusVector, @FTransaction.FTransactionHandle,
            FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA);
      end;
    finally
      GDS.Release;
    end;

    //Change cursor state
    if Res <> 0 then
      SetCursorState(OldCursorState) // In future thr error code should be checked and cursor unprepared
    else begin
      SetCursorState(csExecuted);
      try
        GetRowsAffected;
      except
        if GetPrepared then
          Finish;
        raise;
      end;
    end;
    Check(Res);
  finally
    if not OldPrepared then
      Finish;
  end;
end;

function TGDSCommand.ExecuteNext: boolean;
var
  Res: ISC_STATUS;
begin
  Result := False;
  if GetCursorState = csExecuting then
    Exit;
  FExecuting := True;
  try
    if GetCursorState <> csExecuted then
      RaiseError(SCursorNotOpened);

      SetCursorState(csExecuting);

    case FSQLType of
      SQL_EXEC_PROCEDURE, SQL_SELECT, SQL_SELECT_FOR_UPD: begin
        GDS.Busy;
        try
          Res := GDS.isc_dsql_fetch(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FOutXSQLDA.FXSQLDA);
        finally
          GDS.Release;
        end;
        if (Res = 100) then
          Result := False
        else begin
          if Res = 0 then
            SetCursorState(csExecuted)
          else
            SetCursorState(csInactive);
          Check(Res);
          Result := True
        end;
      end
      else
        RaiseError(SCantExecuteNonSelectStatement);
    end;
  except
    FExecuting := False;
    if Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;
  ReadOutParams;
  FExecuting := False;
  if Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TGDSCommand.ReadOutParams;
var
  i: integer;
  OutParamsCount: integer;
  SQLVAR: TSQLVARAccessor;
  Param: TIBCParamDesc;
  Blob: TIBCBlob;
  Arr: TCustomIBCArray;
  BcdValue: TBCD;
  bcdscale: integer;
  Len: integer;
  Offset: integer;
  ws: PWideChar;

  OwnerCommand: TGDSCommand;
  ParamsCount: integer;
  OwnerParamsCount: integer;

  ParamIndicatorPtr: IntPtr;
  ParamPtr: PVariant;
  InternalValuePtr: IntPtr;
  ItemIndex,
  ValueIndex: integer;
  ItemNull: boolean;
begin
  if (FOutXSQLDA = nil) or (FOutXSQLDA.FXSQLDA = nil) then
    Exit;

  OwnerCommand := TGDSCommand(BatchOwner);
  if OwnerCommand = nil then begin
    ParamsCount := Params.Count;
    OwnerParamsCount := ParamsCount;
  end
  else begin
    OwnerParamsCount := OwnerCommand.FParamsInfo.Count;
    if OwnerParamsCount > 0 then
      ParamsCount := OwnerParamsCount * FBatchIters
    else
      ParamsCount := 0;
  end;

  OutParamsCount := 0;
  for i := 0 to ParamsCount - 1 do begin
    if OwnerCommand = nil then
      Param := TIBCParamDesc(Params[i])
    else
      Param := TIBCParamDesc(OwnerCommand.FParamsInfo[i mod OwnerParamsCount].ParamRef);

    if Param.GetParamType > pdInput then begin
      SQLVAR := FOutXSQLDA.Vars[OutParamsCount];

      ItemIndex := FBatchOffset + i div OwnerParamsCount;
      if IsBatchCommand then
        ValueIndex := i div OwnerParamsCount
      else
        ValueIndex := 0;

      ParamIndicatorPtr := Param.GetIndicatorPtr(ValueIndex);
      ParamPtr := Param.GetItemPtr(ItemIndex);
      InternalValuePtr := Param.GetInternalValuePtr(ValueIndex);

      ItemNull := Int16(ParamIndicatorPtr^) = -1;
      Param.SetItemNull(ItemIndex, ItemNull);
      if not ItemNull then
        case SQLVAR.sqltype of
          SQL_IB_SHORT:
            case Param.GetDataType of
              dtInt8:
                ParamPtr^ := Marshal.ReadInt8(InternalValuePtr);
              dtUInt8:
                ParamPtr^ := Marshal.ReadByte(InternalValuePtr);
              dtInt16:
                ParamPtr^ := Marshal.ReadInt16(InternalValuePtr);
              dtUInt16:
                ParamPtr^ := Marshal.ReadUInt16(InternalValuePtr);
              dtBoolean:
                ParamPtr^ := boolean(InternalValuePtr^);
            end;
          SQL_IB_LONG:
            case Param.GetDataType of
              dtInt32:
                ParamPtr^ := Marshal.ReadInt32(InternalValuePtr);
              dtUInt32:
                ParamPtr^ := Marshal.ReadUInt32(InternalValuePtr);
            end;
          SQL_IB_FLOAT:
            ParamPtr^ := Single(InternalValuePtr^);
          SQL_IB_DOUBLE:
            ParamPtr^ := Marshal.ReadDouble(InternalValuePtr);
          SQL_IB_BOOLEAN,
          SQL_FB_BOOLEAN:
            ParamPtr^ := boolean(InternalValuePtr^);
          SQL_IB_VARYING, SQL_IB_TEXT: begin
            if (not FConnection.FUseUnicode) or (Byte(SQLVAR.sqlsubtype) = CH_OCTETS) then begin
              if SQLVAR.sqltype = SQL_IB_TEXT then begin
                Offset := 0;
                Len := SQLVAR.sqllen;
              end
              else begin
                Offset := 2;
                Len := Marshal.ReadInt16(InternalValuePtr);
              end;

              if (Param.FDataType in [dtFixedChar, dtFixedWideChar]) and FTrimFixedChar then
                TGDSCommand(Param.FCommand).TrimBuffer(PtrOffset(InternalValuePtr, Offset), Len);

              ParamPtr^ := Marshal.PtrToStringAnsi(PtrOffset(InternalValuePtr, Offset), Len);
            end
            else begin
              if SQLVAR.sqltype = SQL_IB_TEXT then begin
                Offset := 0;
                Len := SQLVAR.sqllen;
              end
              else begin
                Offset := 2;
                Len := Marshal.ReadInt16(InternalValuePtr);
              end;

              if (Param.FDataType in [dtFixedChar, dtFixedWideChar]) and FTrimFixedChar then
                TGDSCommand(Param.FCommand).TrimBuffer(PtrOffset(InternalValuePtr, Offset), Len);

              if (Param.FDataType = dtString) or (Param.FDataType = dtFixedChar) then
                ParamPtr^ := {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF}.Utf8ToAnsi(PtrOffset(InternalValuePtr, Offset), Len)
              else if (Param.FDataType = dtWideString) or (Param.FDataType = dtFixedWideChar) then begin
                ws := Marshal.AllocHGlobal(Param.FBindBufferSize);
                try
                  Utf8ToWS(PtrOffset(InternalValuePtr, Offset), Len, ws, Param.FBindBufferSize);
                  ParamPtr^ := WideString(ws);
                finally
                  Marshal.FreeHGlobal(ws);
                end;
              end;
            end;
          end;
          SQL_IB_TYPE_DATE: begin
            TVarData(ParamPtr^).VType := varDate;
            IBCDateToDateTime(InternalValuePtr, @TVarData(ParamPtr^).VDate);
          end;
          SQL_IB_TYPE_TIME: begin
            TVarData(ParamPtr^).VType := varDate;
            IBCTimeToDateTime(InternalValuePtr, @TVarData(ParamPtr^).VDate);
          end;
          SQL_IB_TIMESTAMP: begin
            TVarData(ParamPtr^).VType := varDate;
            IBCTimeStampToDateTime(InternalValuePtr, @TVarData(ParamPtr^).VDate);
          end;
          SQL_IB_INT64: begin
            case Param.GetDataType of
              dtInt64:
                ParamPtr^ := Marshal.ReadInt64(InternalValuePtr);
              dtUInt64:
                ParamPtr^ := Marshal.ReadUInt64(InternalValuePtr);
              dtCurrency, dtBCD:
                ParamPtr^ := Currency(InternalValuePtr^);
              dtFMTBCD: begin
                bcdscale := (-1 * SQLVAR.GetSqlScale);
                BcdValue := IBCDecimalToBcd(Marshal.ReadInt64(InternalValuePtr), BcdScale);
                ParamPtr^ := VarFMTBcdCreate(BcdValue);
              end;
            end;
          end;
        end;

      //Read Blob params
      if not ItemNull then
        case Param.FDataType of
          dtBlob, dtMemo, dtWideMemo: begin
            Blob := TIBCBlob(Param.GetItemObject(ItemIndex));

            if Blob = nil then begin
              Blob := Param.CreateObject as TIBCBlob;
              Blob.Connection := FConnection;
              Blob.Transaction := FTransaction;
              Move(InternalValuePtr^, TIBCBlob(Blob).FID^, SizeOf(TISC_QUAD));
              TVarData(ParamPtr^).VType := varSharedObject;
              TVarData(ParamPtr^).VPointer := Blob;
            end;

            Blob.Cached := FCacheBlobs;
            Blob.Streamed := FStreamedBlobs;
            Blob.CloseBlob;
            if Blob.Cached then
              Blob.ReadBlob;
          end;
          dtArray: begin
            Arr := TCustomIBCArray(Param.GetItemObject(ItemIndex));

            if Arr = nil then begin
              Arr := Param.CreateObject as TCustomIBCArray;
              Arr.Connection := FConnection;
              Arr.Transaction := FTransaction;
              Move(InternalValuePtr^, TIBCArrayUtils.GetArrayIDPtr(Arr)^, SizeOf(TISC_QUAD));
              TVarData(ParamPtr^).VType := varSharedObject;
              TVarData(ParamPtr^).VPointer := Arr;
            end;

            Arr.Cached := FCacheArrays;
            if Arr.Cached then
              Arr.ReadArray;
          end;
        end;
      inc(OutParamsCount);
    end;
  end;
end;

procedure TGDSCommand.Finish;
var
  Res: ISC_STATUS;
begin
  Res := 0;
  try
    if (GetStmtHandle <> nil) and FConnection.GetConnected and not GDS.AlerterFatalError then begin
{This code COULD be called for Statements with isc_dsql_set_cursor_name to close open cursors
 but we should ensure that cursor exists or function fails
      if FSQLType = SQL_SELECT_FOR_UPD then
        Check(isc_dsql_free_statement(FStatusVector, FStmtHandle, DSQL_close));
}
      GDS.Busy;
      try
        if GetStmtHandle <> nil then begin
          Res := GDS.isc_dsql_free_statement(FStatusVector, FStmtHandle, DSQL_drop);
          Marshal.WriteIntPtr(FStmtHandle, nil);
        end;
      finally
        GDS.Release;
      end;
    end;
  finally
    Marshal.WriteIntPtr(FStmtHandle, nil);
    FSQLType := SQL_UNKNOWN;
    SetCursorState(csInactive);
  end;
  Check(Res);                 //We should raise exception AFTER reseting FStmtHandle to avoid
                              //infinite loop during fatal error handle closing
end;

function TGDSCommand.GetIsFBConnection: boolean;
begin
  if (FConnection <> nil) and FConnection.GetConnected then
    Result := FConnection.IsFBServer
  else
    Result := True;
end;

{ Params }

procedure TGDSCommand.SetSQL(const Value: string);
var
  OldScanParams: boolean;
begin
  FUserSQL := Value;
  OldScanParams := FScanParams;

  try
    if FScanParams then
      FScanParams := not FDisableParamScan;
    FSQL := ParseSQL(Value, FParams);
  finally
    FScanParams := OldScanParams;
  end;
end;

function TGDSCommand.ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string;
var
  ParsedSQL: StringBuilder;
  Parser: TIBCParser;
  Code, PrevCode: integer;
  St, St2: string;
  l: integer;
  ParamName: string;
  Param: TIBCParamDesc;

  LeftQuote, RightQuote: char;

  InDDL: boolean;
  InReturning: boolean;
  IsParam: boolean;
  IsFirstLexem: boolean;
  IsAlias: boolean;

begin
  Assert(Params <> nil);

  LeftQuote := '"';
  RightQuote := '"';

  FParsedSQLType := qtUnknown;
  InDDL := False;
  InReturning := False;
  IsAlias := False;
  Code := 0;
  PrevCode := 0;

  FParamsInfo.Clear;
  if FScanParams then
    Params.Clear;

  ParsedSQL := StringBuilder.Create(Length(SQL) + Length(SQL) div 2);
  try
    Parser := TIBCParser.Create(SQL);
    try
      Parser.OmitBlank := False;
      Parser.OmitComment := False;
      Parser.QuotedString := True;
      Parser.Uppered := False;
      Parser.DecSeparator := '.';
      Parser.ToBegin;
      IsFirstLexem := True;
      repeat
        repeat
          if not InReturning and ((Code = lxReturning) and not DisableReturningKeyword and GetIsFBConnection and not InDDL) then begin
            InReturning := True;
            ParamName := '';
            //Set ParsedSQLType to obtain right affected record count
            FParsedSQLType := qtInsertReturning;
          end;

          if (Code <> lcBlank) and (Code <> lcComment) then
            PrevCode := Code;

          Code := Parser.GetNext(St);

          if IsFirstLexem then begin
            if (Code = lxSELECT) or (Code = lxWITH) then
              FParsedSQLType := qtSelect
            else if (Code = lxINSERT) and (FParsedSQLType <> qtInsertReturning) then
              FParsedSQLType := qtInsert
            else if Code = lxUPDATE then
              FParsedSQLType := qtUpdate
            else if Code = lxDELETE then
              FParsedSQLType := qtDelete;

            if (Code <> 0) and (Code <> lcBlank) and (Code <> lcComment) then
              IsFirstLexem := False;
          end;

          if (Code = lxBLOCK) and (PrevCode = lxEXECUTE) then
            FParsedSQLType := qtExecuteBlock;

          InDDL := InDDL or (Code = lxBEGIN) or (Code = lxCREATE) or (Code = lxDROP) or (Code = lxALTER)
            or (Code = lxDECLARE) or (Code = lxGRANT) or (Code = lxREVOKE) ;

          IsParam := ((Code = lxColon)) and not InDDL;

          if not IsParam then
            ParsedSQL.Append(St);
        until (Code = lcEnd) or IsParam or InReturning;

        if IsParam or (InReturning and FScanParams) then begin
          if IsParam then
            Code := Parser.GetNext(St);

          if (Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst) then begin
              St2 := St;
            l := Length(St2);
            // remove quotes
            if (St2[1] = LeftQuote) and (St2[l] = RightQuote) then
              St2 := Copy(St2, 2, l - 2);
            if InReturning then begin
              if ParamName = '' then
                ParamName := 'RET_' + St2
              else
              if Code = lxAS then
                IsAlias := True
              else begin
                if IsAlias then begin
                  ParamName := 'RET_' + St2;
                  IsAlias := False;
                end
                else
                  ParamName := ParamName + '_' + St2;
              end;
            end
            else
              ParamName := St2;

            if not InReturning then begin
              if FScanParams then begin
                Param := TIBCParamDesc(AddParam);
                Param.SetName(ParamName);
                AddParamPosition(ParamName, Parser.CurrPos - Length(St), Parser.CurrPos, Param);
              end;

              ParsedSQL.Append('?');
            end;
          end
          else
          if InReturning and ((Code = lxComma) or (Code = lcEnd)) and (ParamName <> '') then begin
            Param := TIBCParamDesc(AddParam);
            Param.SetName(ParamName);
            Param.SetParamType(pdOutput);
            ParamName := '';
          end;
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

procedure TGDSCommand.BindParams;
var
  i: integer;
  InParamsCount, DefaultParamsCount: integer;
  Param: TIBCParamDesc;
  SQLVAR: TSQLVARAccessor;
  NeedDescribe: boolean;

  OwnerCommand: TGDSCommand;
  ParamsCount: integer;
  OwnerParamsCount: integer;

  ParamType: TParamDirection;
  ParamDataType: word;
  ParamNull: boolean;
  ParamIndicatorPtr: IntPtr;
  ParamPtr: PVariant;
  ParamValuePtr,
  InternalValuePtr: IntPtr;
  ItemIndex,
  ValueIndex: integer;
begin
  InParamsCount := 0;
  DefaultParamsCount := 0;
  NeedDescribe := False;

  OwnerCommand := TGDSCommand(BatchOwner);
  if OwnerCommand = nil then begin
    ParamsCount := Params.Count;
    OwnerParamsCount := ParamsCount;
  end
  else begin
    OwnerParamsCount := OwnerCommand.FParamsInfo.Count;
    if OwnerParamsCount > 0 then
      ParamsCount := OwnerParamsCount * FBatchIters
    else
      ParamsCount := 0;
  end;

  for i := 0 to ParamsCount - 1 do begin
    if OwnerCommand = nil then
      Param := TIBCParamDesc(Params[i])
    else
      Param := TIBCParamDesc(OwnerCommand.FParamsInfo[i mod OwnerParamsCount].ParamRef);

    if Param.GetParamType in [pdUnknown, pdInput, pdInputOutput] then begin
      Inc(InParamsCount);
      if not Param.GetIsBound then
        Inc(DefaultParamsCount);
    end
    else
      NeedDescribe := NeedDescribe or (Param.GetDataType = dtUnknown)
        or ((Param.GetDataType in [dtString, dtFixedChar, dtWideString, dtFixedWideChar, dtBytes, dtVarBytes]) and
        ((Param.FSize = 0) or (Param.FSize = Param.GetMaxStringSize(Param.GetCommand.GetConnection))))
        or ((Param.GetDataType = dtFMTBCD) and (Param.Scale = 0))
  end;
  CheckSQLDA(InParamsCount - DefaultParamsCount, ParamsCount - InParamsCount);

  { Perfomance improvement }
  if NeedDescribe then
    DescribeParams(False, InParamsCount); // describe OUT parameters only

  for i := 0 to ParamsCount - 1 do begin
    if not IsBatchCommand then
      Param := TIBCParamDesc(Params[i])
    else
      Param := TIBCParamDesc(OwnerCommand.FParamsInfo[i mod OwnerParamsCount].ParamRef);
    if not Param.GetIsBound then
      Continue;

    ParamType := Param.GetParamType;
    ParamDataType := Param.FDataType;

    if i < InParamsCount then
      SQLVAR := FInXSQLDA.Vars[i]
    else
      SQLVAR := FOutXSQLDA.Vars[i - InParamsCount];

    Param.AllocBindBuffer(FBatchIters, SQLVAR);

    ItemIndex := FBatchOffset + i div OwnerParamsCount;
    if IsBatchCommand then
      ValueIndex := i div OwnerParamsCount
    else
      ValueIndex := 0;

    ParamNull := Param.ItemNull[ItemIndex];
    Param.SyncIndicator(ValueIndex, ParamNull);
    ParamIndicatorPtr := Param.GetIndicatorPtr(ValueIndex);

    ParamPtr := Param.GetItemPtr(ItemIndex);
    InternalValuePtr := Param.GetInternalValuePtr(ValueIndex);
    if not ParamNull or (ParamType > pdInput) then
      ParamValuePtr := ConvertParamValue(Param, ParamPtr, InternalValuePtr, SQLVAR)
    else
      ParamValuePtr := nil;  

    SQLVAR.sqlind := ParamIndicatorPtr;
    SQLVAR.sqlprecision := 0;

    case ParamDataType of
      dtUnknown:
        if ParamNull and (ParamType <= pdInput) then
          SQLVAR.sqltype := SQL_IB_TEXT or 1
        else
          raise Exception.Create(Format(SUnknownParamDataType, [Param.GetName]));
      dtString, dtFixedChar: begin
        if (ParamType > pdInput) and (ParamDataType = dtString) then
          SQLVAR.sqltype := SQL_IB_VARYING or 1
        else
          SQLVAR.sqltype := SQL_IB_TEXT or 1;
      end;
      dtWideString, dtFixedWideChar, dtGuid: begin
        if (ParamType = pdOutput) and (ParamDataType = dtWideString) then
          SQLVAR.sqltype := SQL_IB_VARYING or 1
        else
          SQLVAR.sqltype := SQL_IB_TEXT or 1;
      end;
      dtBytes, dtVarBytes: begin
        if (ParamDataType = dtVarBytes) then
          SQLVAR.sqltype := SQL_IB_VARYING or 1
        else
          SQLVAR.sqltype := SQL_IB_TEXT or 1;
      end;
      dtDateTime, dtDate, dtTime, dtSQLTimeStamp: begin
        if ((ParamDataType = dtDate)) and (FConnection.FDBSQLDialect >= 3) then begin
          SQLVAR.sqltype := SQL_IB_TYPE_DATE or 1;
          SQLVAR.sqllen := SizeOf(ISC_DATE);
        end
        else
        if ((ParamDataType = dtTime)) and (FConnection.FDBSQLDialect >= 3) then begin
          SQLVAR.sqltype := SQL_IB_TYPE_TIME or  1;
          SQLVAR.sqllen := SizeOf(ISC_TIME);
        end
        else begin
          SQLVAR.sqltype := SQL_IB_TIMESTAMP or 1;
          SQLVAR.sqllen := SizeOf(TISC_QUAD);
        end;
      end;
      dtInt8, dtSmallint: begin
        SQLVAR.sqltype := SQL_IB_SHORT or 1;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(SmallInt);
      end;
      dtUInt8, dtWord: begin
         SQLVAR.sqltype := SQL_IB_SHORT or 1;
         SQLVAR.sqlscale := 0;
         SQLVAR.sqllen := SizeOf(SmallInt);
      end;
      dtInteger: begin
        SQLVAR.sqltype := SQL_IB_LONG or 1;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(Long);
      end;
      dtSingle: begin
        SQLVAR.sqlscale := 0;
        SQLVAR.sqltype := SQL_IB_FLOAT or 1;
        SQLVAR.sqllen := SizeOf(Single);
      end;
      dtFloat: begin
        SQLVAR.sqlscale := 0;
        SQLVAR.sqltype := SQL_IB_DOUBLE or 1;
        SQLVAR.sqllen := SizeOf(Double);
      end;
      dtLargeint, dtUInt32: begin
        SQLVAR.sqltype := SQL_IB_INT64 or 1;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(Int64);
      end;
      dtCurrency, dtBCD: begin
        SQLVAR.sqltype := SQL_IB_INT64 or 1;
        SQLVAR.sqlscale := -4;
        SQLVAR.sqllen := SizeOf(Int64);
      end;
      dtFMTBCD: begin
        SQLVAR.sqltype := SQL_IB_INT64 or 1;
        SQLVAR.sqllen := SizeOf(Int64);
      end;
      dtBoolean: begin
        SQLVAR.sqllen := SizeOf(SmallInt);
        if GDS.ClientVersion.Version < 7 then begin
          if FConnection.IsFBServer and (GDS.ClientVersion.Version >= 3) and ((Param.FSubDataType = dtBoolean) or (Param.FSubDataType = 0)) then begin
            SQLVAR.sqltype := SQL_FB_BOOLEAN or 1;
            SQLVAR.sqllen := SizeOf(byte);
          end
          else
            SQLVAR.sqltype := SQL_IB_SHORT or 1;
        end
        else
          SQLVAR.sqltype := SQL_IB_BOOLEAN or 1;
        SQLVAR.sqlscale := 0;
      end;
      dtBlob, dtMemo, dtWideMemo: begin
        SQLVAR.sqltype := SQL_IB_BLOB or 1;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(TISC_QUAD);
      end;
      dtArray: begin
        SQLVAR.sqltype := SQL_IB_ARRAY or 1;
        SQLVAR.sqllen := SizeOf(TISC_QUAD);
      end;
    end;
    SQLVAR.sqldata := ParamValuePtr;
  end;
end;

procedure TGDSCommand.BreakExec;
var
  Res: ISC_STATUS;
begin
  Res := 0;
  if Executing and (FConnection.FDatabaseHandle <> nil) then
    Res := GDS.fb_cancel_operation(FStatusVector, FConnection.FDatabaseHandle, CANCEL_OPTION);

  if Res <> gds_nothing_to_cancel then
    Check(Res);
end;

function TGDSCommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;

  function NormalizeStrValue(const Value: string): string;
  begin
    Result := Trim(Value);
    if (FConnection <> nil) and (FConnection.FSQLDialect = 3) then
      Result := SQLInfo.NormalizeName(Value, FQuoteNames)
    else
      Result := UpperCase(Result);
  end;

var
  RecordSet: TGDSRecordSet;
  SQL, NormName: string;
  v: Variant;
  RecBuf: IntPtr;
  Param: TIBCParamDesc;
  i: integer;
  Added, OldUseDescribe: boolean;
begin
  if IsQuery then
    Result := 'SELECT * FROM ' + NormalizeStrValue(Name)
  else
    Result := 'EXECUTE PROCEDURE ' + NormalizeStrValue(Name);
  try
    if NeedDescribe then begin
      ClearParams;
      CheckDatabase;
      CheckTransaction;
      RecordSet := TGDSRecordSet.Create;
      try
        RecordSet.SetConnection(FConnection);
        RecordSet.SetTransaction(FTransaction);
        if SQLInfo.IsQuoted(Trim(Name)) then
          NormName := SQLInfo.UnQuote(Trim(Name))
        else
          NormName := UpperCase(Trim(Name));

        SQL := 'SELECT P.RDB$PARAMETER_NAME ' + #13#10 +
          'FROM RDB$PROCEDURE_PARAMETERS P' + #13#10;
        if FConnection.FIsFBServer and (FConnection.GetMajorServerVersion >= 3) then
          SQL := SQL +
            'WHERE CASE WHEN P.RDB$PACKAGE_NAME IS NOT NULL THEN TRIM(P.RDB$PACKAGE_NAME)||''.''||TRIM(P.RDB$PROCEDURE_NAME) ELSE TRIM(P.RDB$PROCEDURE_NAME) END = ' +
            AnsiQuotedStr(NormName, '''') + #13#10
        else
          SQL := SQL + 'WHERE P.RDB$PROCEDURE_NAME = ' + AnsiQuotedStr(NormName, '''') + #13#10;
        SQL := SQL + '  AND P.RDB$PARAMETER_TYPE = 0 ' + #13#10 +
          'ORDER BY P.RDB$PARAMETER_NUMBER';
        RecordSet.SetSQL(SQL);
        RecordSet.SetProp(prFlatBuffers, False);
        RecordSet.SetProp(prLongStrings, True);
        RecordSet.SetProp(prExtendedFieldsInfo, False);
        RecordSet.Open;
        RecordSet.FetchAll;
        RecordSet.SetToBegin;
        FSQL := '';
        if RecordSet.RecordCount > 0 then begin
          RecBuf := nil;
          Result := Result + '(';
          FSQL := Result;
          RecordSet.AllocRecBuf(RecBuf);
          try
            RecordSet.GetNextRecord(RecBuf);
            while not RecordSet.Eof do begin
              RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v); //Parameter name
              //Add param to find out Param Name
              Param := TIBCParamDesc(AddParam);
              Param.SetParamType(pdInput);
              Param.SetName(string(Trim(v)));
              FSQL := FSQL + '?';
              Result := Result + ':' + NormalizeStrValue(Param.GetName);
              RecordSet.GetNextRecord(RecBuf);
              if not RecordSet.Eof then
                SQL := ', '
              else
                SQL := ')';
              FSQL := FSQL + SQL;
              Result := Result + SQL;
            end;
          finally
            if RecBuf <> nil then
              RecordSet.FreeRecBuf(RecBuf);
          end;
        end
        else
          FSQL := Result; //Only out paramas or no params
      finally
        RecordSet.Free;
      end;
      FUserSQL := Result;
      OldUseDescribe := FUseDescribeParams;
      FUseDescribeParams := True;
      try
        Prepare; // Desribe In and Out Params
        Unprepare;
      finally
        FUseDescribeParams := OldUseDescribe;
      end;
    end
    else begin
      //Create SQL using params
      //We assume that param order is equal to param order in StoredProcedure
      SQL := Result;
      Added := False;
      for i := 0 to FParams.Count - 1 do begin
        Param := TIBCParamDesc(FParams[i]);
        if (Param.GetParamType = pdInput) and Param.GetIsBound then begin
          if not Added then begin
            SQL := SQL + '(';
            Result := Result + '(';
          end
          else begin
            SQL := SQL + ', ';
            Result := Result + ', ';
          end;
          SQL := SQL + '?';
          Result := Result + ':' + NormalizeStrValue(Param.GetName);
          Added := True;
        end;
      end;
      if Added then begin
        SQL := SQL + ')';
        Result := Result + ')';
      end;
      FSQL := SQL;
      FUserSQL := Result;
    end;
  finally
    FParsedSQLType := qtUnknown;
  end;
end;

function TGDSCommand.ForceCreateSPParams: boolean;
begin
  Result := not FUseDescribeParams;
end;

procedure TGDSCommand.Prepare;
var
  pSQL: PStr;
  LocalBuffer: IntPtr;
  SwapLen: integer;
begin
  CheckDatabase;
  CheckTransaction;
  if GetCursorState <> csInactive then
    Exit;
  FSQLType := SQL_UNKNOWN;
  if Trim(FSQL) = '' then //to avoid server crash
    RaiseError(SEmptySQL);
  try
    GDS.Busy;
    try
      Check(GDS.isc_dsql_alloc_statement2(FStatusVector, FConnection.FDatabaseHandle, FStmtHandle));
      pSQL := StringToPtrGDS(FSQL, FConnection.FUseUnicode);
      try
        Check(GDS.isc_dsql_prepare(FStatusVector, @FTransaction.FTransactionHandle, FStmtHandle, 0,
          pSQL, FConnection.FSQLDialect, nil));
      finally
        FreeStringGDS(pSQL, FConnection.FUseUnicode);
      end;
      Marshal.WriteByte(FInfoTypeBuf, isc_info_sql_stmt_type);
      LocalBuffer := Marshal.AllocHGlobal(8);
      try
        Check(GDS.isc_dsql_sql_info(FStatusVector, FStmtHandle, 1, FInfoTypeBuf, 8, LocalBuffer));
        if (Marshal.ReadByte(LocalBuffer) <> isc_info_sql_stmt_type) then
          RaiseError(SSQLInfoError);
        SwapLen := Marshal.ReadInt16(LocalBuffer, 1);
        case SwapLen of
          2: FSQLType := Marshal.ReadInt16(LocalBuffer, 3);
          4: FSQLType := Marshal.ReadInt32(LocalBuffer, 3);
        end;
      finally
        Marshal.FreeHGlobal(LocalBuffer);
      end;
    finally
      GDS.Release;
    end;
    //Validate statement type
    if (FSQLType = SQL_PUT_SEGMENT) or (FSQLType = SQL_GET_SEGMENT) or
       (FSQLType = SQL_START_TRANS) then
      RaiseError(SWrongSQLType);

    if FUseDescribeParams then
      DescribeParams;

    inherited;

  except
    Finish;
    raise;
  end;
end;

procedure TGDSCommand.Unprepare;
begin
  if GetCursorState = csInactive then
    Exit;
  Finish;

  inherited;
end;

function TGDSCommand.GetPrepared: boolean;
begin
  Result := GetCursorState >= csPrepared;
end;

procedure TGDSCommand.Execute;
begin
  if GetCursorState = csExecuting then
    Exit;

  FExecuting := True;
  try
    InternalExecute;
  except
    FExecuting := False;
    if Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;

  ReadOutParams;

  FExecuting := False;
  if Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TGDSCommand.ExecuteBatch(Iters: integer = 1; Offset: integer = 0);
var
  BatchCommand: TGDSCommand;
  NeedSetSQL,
  NeedPrepare: boolean;
  SimpleExecute: boolean;
  BatchSize,
  LastBatchSize: integer;
  i, n,
  RowsAffected,
  ParamsProcessed: integer;
  SavepointName: string;
  OldDescribeParams: boolean;

  function GetSavepointName: string;
  const
    SavepointPrefix = 'SAVEPOINT_BU_';
  begin
    Result := IntToHex(NativeInt(Self), SizeOf(NativeInt) * 2);
    Result := SavepointPrefix + Result;
  end;

begin
  if GetCursorState = csExecuting then
    Exit;

  if not (FParsedSQLType in [qtInsert, qtUpdate, qtDelete, qtInsertReturning]) then
    raise Exception.Create(SInvalidBatchOperation);

  if (FParams.Count > 0) and (Iters + Offset > FParams[0].GetArraySize) then
    raise Exception.Create(SInvalidBatchParameters);

  CreateBatchCommand;

  SimpleExecute := not (FConnection.FIsFBServer and (FConnection.FMajorServerVersion >= 2));
  FExecuting := True;
  BatchCommand := TGDSCommand(GetBatchCommand);

  NeedPrepare := not GetPrepared;
  if NeedPrepare or not FUseDescribeParams then begin
    OldDescribeParams := FUseDescribeParams;
    try
      if NeedPrepare then begin
        FUseDescribeParams := False;
        Prepare;
      end;
      FillSQLDA;
    finally
      FUseDescribeParams := OldDescribeParams;
    end;
  // TODO: check
  end;

  try
    n := 0;
    for i := 0 to High(FInXSQLDA.FXSQLVARs) do
      n := n + 28 + FInXSQLDA.FXSQLVARs[i].sqllen;
    for i := 0 to High(FOutXSQLDA.FXSQLVARs) do
      n := n + 28 + FOutXSQLDA.FXSQLVARs[i].sqllen;
    if n > 0 then
      BatchSize := MaxSQLLength div n
    else
      BatchSize := MaxSQLLength;
    n := MaxSQLLength div (Length(FSQL) + (Length(FInXSQLDA.FXSQLVARs) + Length(FOutXSQLDA.FXSQLVARs)) * 200 + 4);
    if n < BatchSize then
      BatchSize := n;
    if BatchSize > 255 then
      BatchSize := 255;

    if (BatchSize <= 1) or (FParsedSQLType = qtInsertReturning) then
      SimpleExecute := True;

    if SimpleExecute then begin
      SavepointName := GetSavepointName;
      GetTransaction.Savepoint(SavepointName);
      try
        RowsAffected := 0;
        ParamsProcessed := 0;
        for i := 0 to Iters - 1 do begin
          FBatchIters := 1;
          FBatchOffset := Offset + i;
          InternalExecute;

          ReadOutParams;

          Inc(RowsAffected, FRowsAffected);
          Inc(ParamsProcessed);
        end;
      except
        FExecuting := False;

        GetTransaction.RollbackToSavepoint(SavepointName);

        if Assigned(FAfterExecute) then
          FAfterExecute(False);
        raise;
      end;

      FExecuting := False;
      FRowsAffected := RowsAffected;
      FParamsProcessed := ParamsProcessed;
      if Assigned(FAfterExecute) then
        FAfterExecute(True);
    end
    else begin
      if BatchSize <> Iters then begin
        SavepointName := GetSavepointName;
        GetTransaction.Savepoint(SavepointName);
      end
      else
        SavepointName := '';
      try
        n := 0;
        LastBatchSize := 0;
        RowsAffected := 0;
        ParamsProcessed := 0;
        BatchCommand.FRowsAffected := 0;
        BatchCommand.FParamsProcessed := 0;

        while n < Iters do begin
          if n + BatchSize > Iters then
            BatchSize := Iters - n;

          NeedSetSQL := LastBatchSize <> BatchSize;

          if NeedSetSQL then begin
            if BatchCommand.GetPrepared then
              BatchCommand.Unprepare;

            BatchCommand.SetSQL(GetBatchSQL(BatchSize));

            BatchCommand.Prepare;
            BatchCommand.FSQLType := FSQLType;
            BatchCommand.FParsedSQLType := FParsedSQLType;
          end;

          BatchCommand.FBatchIters := BatchSize;
          BatchCommand.FBatchOffset := Offset + n;
          BatchCommand.InternalExecute;

          Inc(RowsAffected, BatchCommand.FRowsAffected);
          Inc(ParamsProcessed, BatchSize);
          Inc(n, BatchSize);
          LastBatchSize := BatchSize;
        end;
      except
        if SavepointName <> '' then
          GetTransaction.RollbackToSavepoint(SavepointName);
        if Assigned(FAfterExecute) then
          FAfterExecute(False);
        FExecuting := False;
        raise;
      end;

      FExecuting := False;
      FRowsAffected := RowsAffected;
      FParamsProcessed := ParamsProcessed;
      if Assigned(FAfterExecute) then
        FAfterExecute(True);
    end;
  finally
    if NeedPrepare then begin
      if GetPrepared then
        Unprepare;
    end;
  end;
end;

procedure TGDSCommand.ExecuteBatch(const Statements: _TStringArray);
var
  Status, SqlPtr, RowsAff: IntPtr;
  Count, i: integer;
  Sql: array of PStr;
{$IFDEF CLR}
  SqlPtrs: array of IntPtr;
  Len: integer;
{$ENDIF}
  Res: ISC_STATUS;
begin
  Count := Length(Statements);
  if Count = 0 then
    exit;
  SetLength(Sql, Count);
  Status := Marshal.AllocHGlobal(Count * SizeOf(ISC_STATUS));
{$IFDEF CLR}
  SqlPtr := Marshal.AllocHGlobal(Count * SizeOf(IntPtr));
  SetLength(SqlPtrs, Count);
{$ELSE}
  SqlPtr := Sql;
{$ENDIF}
  RowsAff := Marshal.AllocHGlobal(Count * SizeOf(Cardinal));
  try
    for i := 0 to Count - 1 do begin
      Sql[i] := StringToPtrGDS(Statements[i], FConnection.FUseUnicode);
    {$IFDEF CLR}
      Len := Length(Sql[i]);
      SqlPtrs[i] := Marshal.AllocHGlobal(Len + 1);
      Marshal.Copy(Sql[i], 0, SqlPtrs[i], Len);
      Marshal.WriteByte(SqlPtrs[i], Len, 0);
      Marshal.WriteIntPtr(SqlPtr, i * SizeOf(IntPtr), SqlPtrs[i]);
    {$ENDIF}
    end;
    GDS.Busy;
    try
      Res := GDS.isc_dsql_batch_execute_immed(Status, FConnection.FDatabaseHandle,
        {$IFNDEF CLR}@{$ENDIF}FTransaction.FTransactionHandle, FConnection.FSQLDialect, Count, SqlPtr,
        RowsAff);
    finally
      GDS.Release;
    end;
    Check(Res);
  finally
    Marshal.FreeHGlobal(Status);
    Marshal.FreeHGlobal(RowsAff);
    for i := 0 to Count - 1 do begin
      FreeStringGDS(Sql[i], FConnection.FUseUnicode);
    {$IFDEF CLR}
      Marshal.FreeHGlobal(SqlPtrs[i]);
    {$ENDIF}
    end;
  {$IFDEF CLR}
    Marshal.FreeHGlobal(SqlPtr);
  {$ENDIF}
  end;
end;

procedure TGDSCommand.Close;
begin
  if GetCursorState > csInactive then
    Finish;
end;

procedure TGDSCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    if GetActive then
      Finish;

    inherited;

    FConnection := TGDSConnection(Value);
  end;
end;

function TGDSCommand.GetTransaction: TCRTransaction;
begin
  Result := FTransaction;
end;

procedure TGDSCommand.SetTransaction(Value: TCRTransaction);
begin
  if Value <> FTransaction then begin
    if GetActive then
      Finish;

    FTransaction := TGDSTransaction(Value);
  end;
end;

function TGDSCommand.GetSQLType: integer;
begin
  Result := FSQLType;
end;

function TGDSCommand.GetSQLTypeEx: integer;
begin
  if FParsedSQLType = qtExecuteBlock then
    Result := SQL_EX_EXECUTE_BLOCK
  else
    Result := FSQLType;
end;

function TGDSCommand.GetStmtHandle: TISC_STMT_HANDLE;
begin
  Result := nil;
  if FStmtHandle <> nil then
    Result := Marshal.ReadIntPtr(FStmtHandle);
end;

function TGDSCommand.GetCursorState: TCursorState;
begin
  Result := FState;
end;

procedure TGDSCommand.SetCursorState(Value: TCursorState);
begin
  FState := Value;
end;

function TGDSCommand.GetPlan: string;
const
  Buffer_Size = 16384;
var
  Buffer: IntPtr;
  PlanLength: integer;
  Res: ISC_STATUS;
begin
  Result := '';
  if GetPrepared and (FSQLType in [SQL_SELECT, SQL_UPDATE, SQL_DELETE,
    SQL_EXEC_PROCEDURE, SQL_SELECT_FOR_UPD]) then begin
    Buffer := Marshal.AllocHGlobal(Buffer_Size);
    try
      Marshal.WriteByte(FInfoTypeBuf, isc_info_sql_get_plan);
      GDS.Busy;
      try
        Res := GDS.isc_dsql_sql_info(FStatusVector, FStmtHandle, 2, FInfoTypeBuf, Buffer_Size, Buffer);
      finally
        GDS.Release;
      end;
      Check(Res);
      if Marshal.ReadByte(Buffer) = isc_info_sql_get_plan then begin
        PlanLength := Marshal.ReadInt16(Buffer, 1);
        Result := PtrToStringGDS(PtrOffset(Buffer, 3), PlanLength, FConnection.FUseUnicode);
        Result := Trim(Result);
      end;
    finally
      Marshal.FreeHGlobal(Buffer);
    end;
  end;
end;

function TGDSCommand.GetGDS: TGDS;
begin
  Result := FConnection.FGDS;
end;

procedure TGDSCommand.TrimBuffer(Buf: IntPtr; var BufLen: integer);
var
  p: IntPtr;
begin
  p := PtrOffset(Buf, BufLen - 1);
  while (BufLen > 0) and (Marshal.ReadByte(p) = Byte(' ')) do begin
    Dec(BufLen);
    p := PtrOffset(p, -1);
  end;
end;

procedure TGDSCommand.CheckSQLDA(InVarsCount: integer = 0; OutVarsCount: integer = 0);
var
  GDSVersion: currency;
begin
  GDSVersion := GDS.ClientVersion.Version;
  if (FInXSQLDA = nil) or
    ((GDSVersion < 7) and (FInXSQLDA.FXSQLVARType = vtGDS7)) or
    ((GDSVersion >= 7) and (FInXSQLDA.FXSQLVARType = vtGDS)) then begin
    FInXSQLDA.Free;
    if GDSVersion >= 7 then
      FInXSQLDA := TSQLDA.Create(FConnection, vtGDS7)
    else
      FInXSQLDA := TSQLDA.Create(FConnection, vtGDS);
  end;
  FInXSQLDA.AllocSQLDA(InVarsCount);

  if (FOutXSQLDA = nil) or
    ((GDSVersion < 7) and (FOutXSQLDA.FXSQLVARType = vtGDS7)) or
    ((GDSVersion >= 7) and (FOutXSQLDA.FXSQLVARType = vtGDS)) then begin
    FOutXSQLDA.Free;
    if GDSVersion >= 7 then
      FOutXSQLDA := TSQLDA.Create(FConnection, vtGDS7)
    else
      FOutXSQLDA := TSQLDA.Create(FConnection, vtGDS);
  end;
  FOutXSQLDA.AllocSQLDA(OutVarsCount);
end;

procedure TGDSCommand.FillSQLDA(Full: boolean = True);
begin
  if Full then
    CheckSQLDA;

  GDS.Busy;
  try
    if Full then begin
      Check(GDS.isc_dsql_describe_bind(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA));
      if FInXSQLDA.GetSqld > FInXSQLDA.GetSqln then begin
        FInXSQLDA.AllocSQLDA(FInXSQLDA.GetSqld);
        Check(GDS.isc_dsql_describe_bind(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA));
      end;
    end;
    if FSQLType <> SQL_SELECT then begin
      Check(GDS.isc_dsql_describe(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FOutXSQLDA.FXSQLDA));
      if FOutXSQLDA.GetSqld > FOutXSQLDA.GetSqln then begin
        FOutXSQLDA.AllocSQLDA(FOutXSQLDA.GetSqld);
        Check(GDS.isc_dsql_describe(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FOutXSQLDA.FXSQLDA));
      end;
    end;
  finally
    GDS.Release;
  end;
end;

function TGDSCommand.IsBatchCommand: boolean;
begin
  Result := BatchOwner <> nil;
end;

function TGDSCommand.GetBatchSQL(var Iters: integer): string;
var
  OriginalSQL: string;
  BatchSQL: StringBuilder;
  Tail: string;
  i, ParamIndex: integer;
  HasParams: boolean;
  SQLVAR: TSQLVARAccessor;

  procedure AppendArguments;
  var
    i, j, n, Len, Prec, Scale: integer;
    ParamDesc: TIBCParamDesc;
  begin
    n := 0;
    for i := 0 to Iters - 1 do begin
      for j := 0 to FParamsInfo.Count - 1 do begin
        ParamDesc := TIBCParamDesc(FParamsInfo[j].ParamRef);
        if n > 0 then
          BatchSQL.Append(',');
        SQLVAR := FInXSQLDA.FXSQLVARs[j];
        Len := SQLVAR.sqllen div FConnection.GetCharLength(SQLVAR.sqlsubtype, SQLVAR.sqlname);
        Prec := ParamDesc.Precision;
        Scale := ParamDesc.Scale;
        if (ParamDesc.GetDataType in [dtBCD, dtFMTBCD]) and (Prec = 0) then begin
          Prec := BcdPrecision;
          Scale := Abs(SQLVAR.sqlscale);
        end;
        BatchSQL.Append('p' + IntToStr(n) + ' ' + DataTypeName(ParamDesc.GetDataType, Len, Prec, Scale, SQLVAR.sqltype, SQLVAR.sqlsubtype) + ' = ?');
        Inc(n);
      end;
    end;
  end;

  procedure AppendParams(Iteration, StartIndex: integer);
  var
    i, n: integer;
  begin
    n := StartIndex;
    for i := 0 to FParamsInfo.Count - 1 do begin
      BatchSQL.Append(OriginalSQL, n, FParamsInfo[i].StartPosition - n - 1);
      BatchSQL.Append(':p' + IntToStr(ParamIndex));

      n := FParamsInfo[i].EndPosition;
      Inc(ParamIndex);
    end;
  end;

begin
  OriginalSQL := Trim(FUserSQL);
  Result := OriginalSQL;

  HasParams := FParamsInfo.Count > 0;
  BatchSQL := StringBuilder.Create(Length(OriginalSQL) * Iters);
  try
    if HasParams then
      Tail := Copy(OriginalSQL, FParamsInfo[FParamsInfo.Count - 1].EndPosition + 1, Length(OriginalSQL) - FParamsInfo[FParamsInfo.Count - 1].EndPosition)
    else
      Tail := OriginalSQL;
    if (Tail = '') or (pos(';', Tail) <= 0) then
      Tail := Tail + ';';

    BatchSQL.Append('EXECUTE BLOCK ');
    if HasParams then
      BatchSQL.Append('(');
    AppendArguments;
    if HasParams then
      BatchSQL.Append(') ');
    BatchSQL.Append('AS BEGIN ');

    ParamIndex := 0;
    for i := 0 to Iters - 1 do begin
      AppendParams(i, 0);
      BatchSQL.Append(Tail);
    end;

    BatchSQL.Append(' END');

    Result := BatchSQL.ToString;
  finally
    BatchSQL.Free;
  end;
end;

function TGDSCommand.ConvertParamValue(const Param: TIBCParamDesc; const Source: PVariant; const Dest: IntPtr; const SQLVAR: TSQLVARAccessor): IntPtr;
var
  Connection: TGDSConnection;
  sa: AnsiString;
  ws: WideString;
  Blob: TIBCBlob;
  Arr: TCustomIBCArray;
  BcdValue: TBcd;
  TimeStampValue: TSQLTimeStamp;
  TimeStampOfsValue: TSQLTimeStampOffset;
  Int64Value: Int64;
  UInt64Value: UInt64;
begin
  Result := Dest;
  Connection := TGDSConnection(FConnection);

  case Param.FDataType of
    dtInt8:
      if Param.FParamType <= pdInput then
        ShortInt(Dest^) := ShortInt(Source^);
    dtUInt8:
      if Param.FParamType <= pdInput then
        byte(Dest^) := byte(Source^);
    dtInt16:
      if Param.FParamType <= pdInput then
        Int16(Dest^) := Int16(Source^);
    dtUInt16:
      if Param.FParamType <= pdInput then
        UInt16(Dest^) := UInt16(Source^);
    dtInt32:
      if Param.FParamType <= pdInput then
        Int32(Dest^) := Int32(Source^);
    dtUInt32:
      if Param.FParamType <= pdInput then
        UInt32(Dest^) := UInt32(Source^);
    dtInt64:
      if Param.FParamType <= pdInput then begin
        Int64Value := Source^;
        Int64(Dest^) := Int64Value;
      end;
    dtUInt64:
      if Param.FParamType <= pdInput then begin
        UInt64Value := Source^;
        UInt64(Dest^) := UInt64Value;
      end;
    dtSingle:
      if Param.FParamType <= pdInput then
        Single(Dest^) := Single(Source^);
    dtExtended, dtFloat:
      if Param.FParamType <= pdInput then
        Marshal.WriteDouble(Dest, double(Source^));
    dtCurrency, dtBCD:
      if Param.FParamType <= pdInput then
        Currency(Dest^) := Currency(Source^);
    dtBoolean:
      if Param.FParamType <= pdInput then begin
        boolean(Dest^) := boolean(Source^);
        byte(PtrOffset(Dest, 1)^) := 0;
      end;
    dtDate:
      if Connection.FSQLDialect = 3 then begin
        if Param.FParamType <= pdInput then
          DateTimeToIBCDate(TDateTime(Source^), Dest);
      end
      else if Param.FParamType <= pdInput then
        DateTimeToIBCTimeStamp(TDateTime(Source^), Dest);
    dtTime:
      if Connection.FSQLDialect = 3 then begin
        if Param.FParamType <= pdInput then
          DateTimeToIBCTime(TDateTime(Source^), Dest)
      end
      else if Param.FParamType <= pdInput then
        DateTimeToIBCTimeStamp(TDateTime(Source^), Dest);
    dtDateTime, dtSQLTimeStamp:
      if Param.FParamType <= pdInput then
        if VarIsSQLTimeStamp(Source^) then begin
          TimeStampValue := VarToSQLTimeStamp(Source^);
          DateTimeToIBCTimeStamp(SQLTimeStampToDateTime(TimeStampValue), Dest);
        end
        else if VarIsSQLTimeStampOffset(Source^) then begin
          TimeStampOfsValue := VarToSQLTimeStampOffset(Source^);
          DateTimeToIBCTimeStamp(SQLTimeStampOffsetToDateTime(TimeStampOfsValue), Dest);
        end
        else
          DateTimeToIBCTimeStamp(TDateTime(Source^), Dest);
    dtString, dtFixedChar:
      if Param.FParamType <= pdInput then begin
        sa := AnsiString(Source^);
        if Connection.FUseUnicode then
          SQLVAR.sqllen := {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF}.AnsiToUtf8(PAnsiChar(sa), Dest, Param.FBindBufferSize)
        else begin
        {$IFNDEF ODBC_UTF8}
          Move(Pointer(sa)^, Dest^, LengthA(sa));
          SQLVAR.sqllen := LengthA(sa);
        {$ELSE}
          SQLVAR.sqllen := {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF}.AnsiToUtf8(PAnsiChar(sa), Dest, Param.FBindBufferSize);
        {$ENDIF}
        end;
      end;
    dtWideString, dtFixedWideChar, dtGuid:
      if Param.FParamType <= pdInput then begin
        ws := WideString(Source^);
        if Connection.FUseUnicode then
          SQLVAR.sqllen := {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF}.WsToUtf8(PWideChar(ws), Dest, Param.FBindBufferSize)
        else begin
        {$IFNDEF ODBC_UTF8}
          sa := AnsiString(ws);
          Move(Pointer(sa)^, Dest^, LengthA(sa));
          SQLVAR.sqllen := LengthA(sa);
        {$ELSE}
          SQLVAR.sqllen := WsToUtf8(PWideChar(ws), Dest, Param.FBindBufferSize);
        {$ENDIF}
        end;
      end;
    dtFMTBCD: begin
      if Param.FParamType <= pdInput then begin
        BcdValue := VarToBcd(Source^);
        SQLVAR.sqlscale := -1 * Integer(BcdScale(BcdValue));
        BcdValue.SignSpecialPlaces := BcdValue.SignSpecialPlaces and 128;
        Marshal.WriteInt64(Dest, StrToInt64(BcdToStr(BcdValue)));
      end
      else if Param.Scale = 0 then begin
        Param.Scale := -1 * SQLVAR.sqlscale;
        Param.Precision := SQLVAR.sqlprecision;
      end
      else begin
        SQLVAR.sqlscale := -1 * Param.Scale;
        SQLVAR.sqlprecision := Param.Precision;
      end;
    end;
    dtBytes, dtVarBytes: begin
      if Param.FParamType <= pdInput then begin
        if Result = nil then
          SQLVAR.sqllen := 0
        else if (Param.FDataType = dtBytes) and (Param.FSize > 0) then begin
          SQLVAR.sqllen := VarArrayHighBound(Source^, 1) + 1;
          Move(Param.GetValuePtr(Source)^, Dest^, SQLVAR.sqllen);
        end
        else if Param.FDataType = dtVarBytes then begin
          SQLVAR.sqllen := VarArrayHighBound(Source^, 1) + 1;
          Marshal.WriteInt16(Dest, SQLVAR.sqllen);
          Move(Param.GetValuePtr(Source)^, PtrOffset(Dest, 2)^, SQLVAR.sqllen);
        end;
      end
      else if Param.FSize > 0 then
        SQLVAR.sqllen := Param.FSize;
    end;
    dtBlob, dtMemo, dtWideMemo: begin
      Blob := TIBCBlob(TSharedObject.FromVariant(Source^));

      if Param.FParamType <= pdInput then begin
        if Blob <> nil then begin
          Blob.Connection := FConnection;
          Blob.Transaction := FTransaction;
          Blob.Streamed := FStreamedBlobs;
        end;
//      end
//      else if Blob = nil then begin
//        raise Exception.Create('Data type for an OUT parameter is not set either to ftMemo or to ftBlob');
      end;

      if Blob <> nil then begin
        Result := Blob.GetIDPtr;
        SQLVAR.sqlsubtype := Blob.SubType;
      end
      else if Param.FParamType <= pdInput then
        Result := nil;
    end;
    dtArray: begin
      Arr := TCustomIBCArray(TSharedObject.FromVariant(Source^));

      if Param.FParamType <= pdInput then begin
        if Arr <> nil then begin
          Arr.Connection := FConnection;
          Arr.Transaction := FTransaction;
        end;
//      end
//      else if Arr = nil then begin
//        raise Exception.Create('Data type for an OUT parameter is not set to ftArray');
      end;

      if Arr <> nil then begin
        Result := TIBCArrayUtils.GetArrayIDPtr(Arr);
        SQLVAR.sqlscale := Arr.ItemScale;
      end
      else if Param.FParamType <= pdInput then
        Result := nil;
    end;
  end;
end;

function TGDSCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prQueryRowsAffected:
      FQueryRowsAffected := Value;
    prParsedSQLType:
      FParsedSQLType := Value;
    prCacheBlobs:
      FCacheBlobs := Value;
    prStreamedBlobs:
      FStreamedBlobs := Value;
    prCacheArrays:
      FCacheArrays := Value;
    prUseDescribeParams:
      FUseDescribeParams := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSCommand.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prSQLType:
      Value := FSQLType;
    prRowsProcessed: begin
      Value := FRowsAffected;
    end;
    prQueryRowsAffected:
      Value := FQueryRowsAffected;
    prRowsInserted:
      Value := FRowsInserted;
    prRowsUpdated:
      Value := FRowsUpdated;
    prRowsDeleted:
      Value := FRowsDeleted;
    prParsedSQLType:
      Value := FParsedSQLType;
    prPlan:
      Value := GetPlan;
    prCacheBlobs:
      Value := FCacheBlobs;
    prStreamedBlobs:
      Value := FStreamedBlobs;
    prCacheArrays:
      Value := FCacheArrays;
    prUseDescribeParams:
      Value := FUseDescribeParams;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

class function TGDSCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TIBCSQLInfo;
end;

class function TGDSCommand.GetParserClass: TSQLParserClass;
begin
  Result := TIBCParser;
end;

class function TGDSCommand.GetParamDescClass: TParamDescClass;
begin
  Result := TIBCParamDesc;
end;

{$IFNDEF LITE}
class function TGDSCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TIBCMapRules;
end;
{$ENDIF}

{ TSQLDA }

procedure TSQLDA.AllocSQLDA(n: integer);
var
  i: integer;
begin
  if ((FXSQLDA = nil) or (sqln <> n)) and (n >= 0) then begin
    FreeSQLDA;

    FXSQLDA := Marshal.AllocHGlobal(XSQLDA_LENGTH(n, FXSQLVARType));
    FillChar(FXSQLDA, XSQLDA_LENGTH(n, FXSQLVARType), 0);
    if FXSQLVARType = vtGDS then
      Version := SQLDA_VERSION1
    else
      Version := SQLDA_VERSION2;
    SetLength(FXSQLVARs, n);
    for i := 0 to n - 1 do begin
      FXSQLVARs[i] := TSQLVARAccessor.Create(Self);
      if FXSQLVARType = vtGDS then
        FXSQLVARs[i].SetXSQLVAR(PtrOffset(FXSQLDA, XSQLDA_LENGTH(0, FXSQLVARType) + i * SizeOfXSQLVAR_V1), FXSQLVARType)
      else
        FXSQLVARs[i].SetXSQLVAR(PtrOffset(FXSQLDA, XSQLDA_LENGTH(0, FXSQLVARType) + i * SizeOfXSQLVAR), FXSQLVARType);
    end;
    sqln := n;
    sqld := n;
  end;
end;

constructor TSQLDA.Create(Connection: TGDSConnection; SQLVARType: TXSQLVARType);
begin
  inherited Create;

  FConnection := Connection;
  FXSQLVARType := SQLVARType;
end;

destructor TSQLDA.Destroy;
begin
  FreeSQLDA;

  inherited;
end;

procedure TSQLDA.FreeSQLDA;
var
  i: integer;
begin
  if FXSQLDA <> nil then begin
    for i := 0 to length(FXSQLVARS) - 1 do
      FXSQLVARS[i].Free;
    SetLength(FXSQLVARs, 0);
    Marshal.FreeHGlobal(FXSQLDA);
  end;
end;

function TSQLDA.GetVersion: Short;
begin
  Result := Marshal.ReadInt16(FXSQLDA);
end;

procedure TSQLDA.SetVersion(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLDA, Value);
end;

function TSQLDA.GetSqldaid: String;
begin
  //unused
  Result := '';
end;

procedure TSQLDA.SetSqldaid(const Value: String);
begin
  //unused
end;

function TSQLDA.GetSqldabc: Long;
begin
  Result := Marshal.ReadInt32(FXSQLDA, OffsetOf_XSQLDA_sqldabc);
end;

procedure TSQLDA.SetSqldabc(const Value: Long);
begin
  Marshal.WriteInt32(FXSQLDA, OffsetOf_XSQLDA_sqldabc, Value);
end;

function TSQLDA.GetSqln: Short;
begin
  Result := Marshal.ReadInt16(FXSQLDA, OffsetOf_XSQLDA_sqln);
end;

procedure TSQLDA.SetSqln(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLDA, OffsetOf_XSQLDA_sqln, Value);
end;

function TSQLDA.GetSqld: Short;
begin
  Result := Marshal.ReadInt16(FXSQLDA, OffsetOf_XSQLDA_sqld);
end;

procedure TSQLDA.SetSqld(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLDA, OffsetOf_XSQLDA_sqld, Value);
end;

function TSQLDA.GetSqlvar: IntPtr;
begin
  Result := Marshal.ReadIntPtr(FXSQLDA, OffsetOf_XSQLDA_sqlvar);
end;

procedure TSQLDA.SetSqlvar(const Value: IntPtr);
begin
  Marshal.WriteIntPtr(FXSQLDA, OffsetOf_XSQLDA_sqlvar, Value);
end;

function TSQLDA.GetSQLVars(Index: integer): TSQLVARAccessor;
begin
  Result := FXSQLVars[Index];
end;

{ TSQLVARAccessor }

constructor TSQLVARAccessor.Create(AOwner: TSQLDA);
begin
  inherited Create;

  FOwner := AOwner;
  FRelAliasName := '';
end;

function TSQLVARAccessor.GetAliasName: string;
begin
  if FXSQLVARType = vtGDS7 then
    ReadStringValue(OffsetOf_XSQLVAR_aliasname, Result)
  else
    ReadStringValue(OffsetOf_XSQLVAR_V1_aliasname, Result);
end;

procedure TSQLVARAccessor.SetAliasName(const Value: string);
begin
  if FXSQLVARType = vtGDS7 then
    WriteStringValue(OffsetOf_XSQLVAR_aliasname, Value)
  else
    WriteStringValue(OffsetOf_XSQLVAR_V1_aliasname, Value);
end;

function TSQLVARAccessor.GetAliasName_Length: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_aliasname_length)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_aliasname_length);
end;

procedure TSQLVARAccessor.SetAliasName_Length(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_aliasname_length, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_aliasname_length, Value);
end;

function TSQLVARAccessor.GetOwnName: string;
begin
  if FXSQLVARType = vtGDS7 then
    ReadStringValue(OffsetOf_XSQLVAR_ownname, Result)
  else
    ReadStringValue(OffsetOf_XSQLVAR_V1_ownname, Result);
end;

procedure TSQLVARAccessor.SetOwnName(const Value: string);
begin
  if FXSQLVARType = vtGDS7 then
    WriteStringValue(OffsetOf_XSQLVAR_ownname, Value)
  else
    WriteStringValue(OffsetOf_XSQLVAR_V1_ownname, Value);
end;

function TSQLVARAccessor.GetOwnName_Length: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_ownname_length)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_ownname_length);
end;

procedure TSQLVARAccessor.SetOwnName_Length(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_ownname_length, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_ownname_length, Value);
end;

function TSQLVARAccessor.GetRelName: string;
begin
  if FXSQLVARType = vtGDS7 then
    ReadStringValue(OffsetOf_XSQLVAR_relname, Result)
  else
    ReadStringValue(OffsetOf_XSQLVAR_V1_relname, Result);
end;

procedure TSQLVARAccessor.SetRelName(const Value: string);
begin
  if FXSQLVARType = vtGDS7 then
    WriteStringValue(OffsetOf_XSQLVAR_relname, Value)
  else
    WriteStringValue(OffsetOf_XSQLVAR_V1_relname, Value);
end;

function TSQLVARAccessor.GetRelName_Length: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_relname_length)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_relname_length);
end;

procedure TSQLVARAccessor.SetRelName_Length(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_relname_length, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_relname_length, Value);
end;

function TSQLVARAccessor.GetSqlName: string;
begin
  if FXSQLVARType = vtGDS7 then
    ReadStringValue(OffsetOf_XSQLVAR_sqlname, Result)
  else
    ReadStringValue(OffsetOf_XSQLVAR_V1_sqlname, Result);
end;

procedure TSQLVARAccessor.SetSqlName(const Value: string);
begin
  if FXSQLVARType = vtGDS7 then
    WriteStringValue(OffsetOf_XSQLVAR_sqlname, Value)
  else
    WriteStringValue(OffsetOf_XSQLVAR_V1_sqlname, Value);
end;

function TSQLVARAccessor.GetSqlName_length: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlname_length)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlname_length);
end;

procedure TSQLVARAccessor.SetSqlName_length(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlname_length, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlname_length, Value);
end;

function TSQLVARAccessor.GetSqlInd: IntPtr;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_sqlind)
  else
    Result := Marshal.ReadIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlind);
end;

procedure TSQLVARAccessor.SetSqlInd(const Value: IntPtr);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_sqlind, Value)
  else
    Marshal.WriteIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlind, Value);
end;

function TSQLVARAccessor.GetSqlData: IntPtr;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_sqldata)
  else
    Result := Marshal.ReadIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqldata);
end;

procedure TSQLVARAccessor.SetSqlData(const Value: IntPtr);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_sqldata, Value)
  else
    Marshal.WriteIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqldata, Value);
end;

function TSQLVARAccessor.GetSqlLen: Word;
begin
  if FXSQLVARType = vtGDS7 then
    Result := word(Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqllen))
  else
    Result := word(Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqllen));
end;

procedure TSQLVARAccessor.SetSqlLen(const Value: Word);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqllen, SmallInt(Value))
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqllen, SmallInt(Value));
end;

function TSQLVARAccessor.GetSqlSubtype: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlsubtype)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlsubtype);
end;

procedure TSQLVARAccessor.SetSqlSubtype(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlsubtype, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlsubtype, Value);
end;

function TSQLVARAccessor.GetSqlPrecision: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlprecision)
  else
    Result := 0;
end;

procedure TSQLVARAccessor.SetSqlPrecision(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlprecision, Value);
end;

function TSQLVARAccessor.GetSqlScale: Short;
begin
  Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlscale);
end;

procedure TSQLVARAccessor.SetSQLScale(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlscale, Value);
end;

function TSQLVARAccessor.GetSqlType: Short;
begin
  Result := Marshal.ReadInt16(FXSQLVAR) and (not 1);
end;

procedure TSQLVARAccessor.SetSqlType(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLVAR, Value);
end;

function TSQLVARAccessor.GetIsNullable: boolean;
begin
  Result := (Marshal.ReadInt16(FXSQLVAR) and 1) = 1;
end;

function TSQLVARAccessor.GetRelAliasName: string;
begin
  Result := FRelAliasName;
end;

procedure TSQLVARAccessor.SetRelAliasName(const Value: string);
begin
  FRelAliasName := Value;
end;

procedure TSQLVARAccessor.ReadStringValue(Offset: integer; var Value: string);
var
  Size: integer;
begin
  Size := Marshal.ReadInt16(FXSQLVAR, Offset - SizeOf(Short));
  Value := PtrToStringGDS(PtrOffset(FXSQLVAR, Offset), Size, FOwner.FConnection.FUseUnicode);
end;

procedure TSQLVARAccessor.WriteStringValue(Offset: integer; const Value: string);
begin
  Marshal.WriteInt16(FXSQLVAR, Offset - SizeOf(Short), Length(Value));
  CopyBufferAnsi(AnsiString(Value), PtrOffset(FXSQLVAR, Offset), Length(Value));
end;

procedure TSQLVARAccessor.SetXSQLVAR(XSQLVAR: IntPtr; XSQLVARType: TXSQLVARType);
begin
  FXSQLVAR := XSQLVAR;
  FXSQLVARType := XSQLVARType;
end;

{ TGDSParamDesc }

constructor TIBCParamDesc.Create;
begin
  inherited Create;

  FDBType := -1;

  FBindBufferDataType := dtUnknown;
  FBindBufferLength := 0;
  FBindBufferSize := 0;

  FBindBuffer := nil;
  SetLength(FIndicator, 0);

  FIsInternalBuffer := False;
end;

destructor TIBCParamDesc.Destroy;
begin
  inherited;

  FreeBindBuffer;
end;

procedure TIBCParamDesc.Clear;
begin
  inherited;

//  if FArraySize > 1 then
//    SetArraySize(1);
//
//  if FParamObject <> nil then
//  {$IFDEF ODBC_DRIVER}
//    if IsObjectValue then
//      TBlob(FParamObject).Clear
//    else if FDataType = dtArray then
//      TCustomIBCArray(FParamObject).ClearArray;
//  {$ELSE}
//    TBlob(FParamObject).Clear;
//  {$ENDIF}
end;

procedure TIBCParamDesc.SetValue(const Value: Variant);
{$IFNDEF LITE}
var
  EncryptDataType: word;
{$ENDIF}
begin
{$IFNDEF LITE}
  if (TVarData(Value).VType <> varValueArrayRef) and
     Assigned(FEncryptor) and
     (FDataType in [dtFixedChar, dtFixedWideChar])
  then begin
    EncryptDataType := FDataType;
    case FDataType of
      dtFixedChar:
        EncryptDataType := dtString;
      dtFixedWideChar:
        EncryptDataType := dtWideString;
    end;
    FData := EncryptValue(EncryptDataType, Value);
    FIsNull := TVarData(FData).VType in [varEmpty, varNull];
  end
  else
  {$ENDIF}
    inherited;
end;

function TIBCParamDesc.CreateObject: TSharedObject;
begin
  if FDataType = dtArray then
    Result := TCustomIBCArray.Create(TGDSConnection(nil), TGDSTransaction(nil), True)
  else
    Result := TIBCBlob.Create(TGDSConnection(nil), TGDSTransaction(nil), FDataType = dtWideMemo, True);
end;

{$IFNDEF LITE}
procedure TIBCParamDesc.InternalAllocBuffer;
var
  PValueArr: PVariantArray;
begin
  if FArraySize > 1 then begin
    New(PValueArr);
    SetLength(PValueArr^, FArraySize);
    TVarData(FData).VType := varValueArrayRef;
    TVarData(FData).VPointer := PValueArr;
    FIsInternalBuffer := True;
  end;
end;
{$ENDIF}

function TIBCParamDesc.GetInternalValuePtr(Index: integer): IntPtr;
begin
  if FBindBuffer = nil then
    Result := GetItemPtr(Index)
  else
    Result := PtrOffset(FBindBuffer, Index * FBindBufferSize);
end;

function TIBCParamDesc.GetIndicatorPtr(Index: integer): IntPtr;
begin
  Result := @FIndicator[Index];
end;

function TIBCParamDesc.GetValuePtr(const Value: PVariant): IntPtr;
begin
  case TVarData(Value^).VType of
    varEmpty, varNull, varUnknown:
      Result := nil;
    varArray or varByte:
      if TVarData(Value^).VArray <> nil then
        Result := TVarData(Value^).VArray^.Data
      else
        Result := nil;
  else
    raise Exception.Create('Unknown value type');
  end;
end;

function TIBCParamDesc.GetCommand: TGDSCommand;
begin
  Result := TGDSCommand(FCommand);
end;

procedure TIBCParamDesc.AllocBindBuffer(BufferLength: integer; const SQLVAR: TSQLVARAccessor);
var
  TmpArraySize,
  TmpBufferSize: integer;
  Connection: TGDSConnection;
begin
  if BufferLength = 0 then
    TmpArraySize := GetArraySize
  else
    TmpArraySize := BufferLength;
  if TmpArraySize = 0 then
    TmpArraySize := 1;

  Connection := TGDSConnection(GetCommand.GetConnection);
  case FDataType of
    dtInt8, dtSmallint,
    dtUInt8, dtWord,
    dtBoolean:
      TmpBufferSize := SizeOf(Smallint);
    dtInteger, dtUInt32:
      TmpBufferSize := SizeOf(Long);
    dtSingle:
      TmpBufferSize := SizeOf(Single);
    dtFloat, dtCurrency, dtBCD:
      TmpBufferSize := SizeOf(Double);
    dtInt64, dtUInt64, dtFMTBCD:
      TmpBufferSize := SizeOf(Int64);
    dtDateTime, dtDate, dtTime, dtSQLTimeStamp:
      if ((FDataType = dtDate)) and (Connection.FDBSQLDialect >= 3) then
        TmpBufferSize := SizeOf(ISC_DATE)
      else if ((FDataType = dtTime)) and (Connection.FDBSQLDialect >= 3) then
        TmpBufferSize := SizeOf(ISC_TIME)
      else
        TmpBufferSize := SizeOf(TISC_QUAD);
    dtString, dtFixedChar: begin
      if (FParamType > pdInput) and ((FSize = 0) or (FSize = MaxSQLLength)) then
        SetSize(SQLVAR.sqllen);
      if Connection.FUseUnicode then
        TmpBufferSize := FSize * Connection.FCharLength
      else
        TmpBufferSize := FSize;
      TmpBufferSize := SizeOf(Smallint) + TmpBufferSize + SizeOf(Char);
    end;
    dtWideString, dtFixedWideChar: begin
      if (FParamType > pdInput) and ((FSize = 0) or (FSize = MaxSQLLength)) then
        SetSize(SQLVAR.sqllen);
      if Connection.FUseUnicode then
        TmpBufferSize := FSize * Connection.FCharLength
      else
        TmpBufferSize := FSize * 4;
      TmpBufferSize := SizeOf(Smallint) + TmpBufferSize + SizeOf(WideChar);
    end;
    dtBytes, dtVarBytes: begin
      if (FParamType > pdInput) and ((FSize = 0) or (FSize = MaxSQLLength)) then
        SetSize(SQLVAR.sqllen);
      TmpBufferSize := SizeOf(Smallint) + FSize;
    end;
    dtMemo, dtWideMemo, dtBlob, dtArray:
      TmpBufferSize := SizeOf(TISC_QUAD);
    dtGuid, dtUnknown:
      TmpBufferSize := 0;
  else
    raise Exception.Create(SUnknownDataType);
  end;

  if (FBindBufferDataType <> FDataType) or
     (FBindBufferLength <> TmpArraySize) or
     (FBindBufferSize <> TmpBufferSize)
  then begin
    FreeBindBuffer;

    SetLength(FIndicator, TmpArraySize);
    FillChar(@FIndicator[0], SizeOf(Int16) * TmpArraySize, $FF);

    FBindBufferLength := TmpArraySize;
    FBindBufferSize := TmpBufferSize;
    FBindBufferDataType := FDataType;

    FBindBuffer := Marshal.AllocHGlobal(FBindBufferLength * FBindBufferSize);
  end;
end;

procedure TIBCParamDesc.FreeBindBuffer;
begin
  SetLength(FIndicator, 0);
  Marshal.FreeHGlobal(FBindBuffer);

  FBindBuffer := nil;
  FBindBufferDataType := dtUnknown;
  FBindBufferLength := 0;
  FBindBufferSize := 0;
end;

procedure TIBCParamDesc.FreeBuffer;
var
  PValueArr: PVariantArray;
begin
  if (TVarData(FData).VType = varValueArrayRef) and FIsInternalBuffer then begin
    TVarData(FData).VType := varEmpty;
    PValueArr := TVarData(FData).VPointer;
    if PValueArr <> nil then
      Dispose(PValueArr);
    FIsInternalBuffer := False;  
  end;
  
  inherited;
end;


function TIBCParamDesc.IsObjectValue: boolean;
begin
  Result := (FDataType = dtArray) or inherited IsObjectValue;
end;

function TIBCParamDesc.GetBatchIters: integer;
begin
  Result := TGDSCommand(FCommand).FBatchIters;
end;

function TIBCParamDesc.GetBatchOffset: integer;
begin
  Result := TGDSCommand(FCommand).FBatchOffset;
end;

procedure TIBCParamDesc.SyncIndicator(ValueIndex: integer; IsNull: boolean{; ForceNotNull: boolean = False});
var
  Ind: Int16;
begin
  if IsNull then
    Ind := -1
  else
    Ind := 0;
  FIndicator[ValueIndex] := Ind;
end;

{ TIBCFieldDesc }

procedure TIBCFieldDesc.SetDataType(Value: Word);
begin
  inherited;
  FIsFetchBlock := TGDSRecordSet.IsFetchBlockField(Value);
  FIsProcess := TGDSRecordSet.IsProcessFieldType(Value);
end;

procedure TIBCFieldDesc.SetFieldReadOnly(SetFieldsReadOnly: boolean; UpdatingTableInfo: TCRTableInfo);
begin
  if not FSkipSetReadOnly then
    inherited;
end;

{ TIBCSQLInfo }

function TIBCSQLInfo.IdentCase: TIdentCase;
begin
  Result := icUpper;
end;

function TIBCSQLInfo.DetectReturningSQLType: boolean;
begin
  Result := True;
end;

{ TGDSRecordSet }

constructor TGDSRecordSet.Create;
begin
  inherited Create;

  FFetchRows := 25;
end;

destructor TGDSRecordSet.Destroy;
begin
  Close;

  FFieldXSQLDA.Free;

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

procedure TGDSRecordSet.CreateCommand;
begin
  SetCommand(TGDSCommand.Create);
end;

procedure TGDSRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TGDSCommand(Value);
  if FCommand <> nil then begin
    FConnection := FCommand.FConnection;
  end;
end;

function TGDSRecordSet.GetTransaction: TCRTransaction;
begin
  Result := FCommand.GetTransaction;
end;

procedure TGDSRecordSet.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    FConnection.IBCError(FCommand.FStatusVector, True, Component);
end;

{ Open /Close }

procedure TGDSRecordSet.InternalPrepare;
begin
  FCommand.Prepare;
  SetCommandType;
end;

procedure TGDSRecordSet.SetCommandType;
begin
  case FCommand.FSQLType of
    SQL_UNKNOWN:
      FCommand.CommandType := ctUnknown;
    SQL_SELECT, SQL_SELECT_FOR_UPD:
      FCommand.CommandType := ctCursor;
    else
      FCommand.CommandType := ctStatement;
  end;
end;

procedure TGDSRecordSet.InternalUnPrepare;
begin
  try
    inherited;
  finally
    FCommand.CommandType := ctUnknown;
  end;
end;

procedure TGDSRecordSet.InternalClose;
var
  Res: integer;
begin
  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  Res := 0;
  if Active and (FCommand.GetStmtHandle <> nil) then
    case FCommand.FSQLType of
      SQL_SELECT, SQL_SELECT_FOR_UPD: begin
        GDS.Busy;
        try
          Res := GDS.isc_dsql_free_statement(FCommand.FStatusVector, FCommand.FStmtHandle, DSQL_close);
        {A cursor only needs to be closed in this manner if it was previously opened and associated with stmt_handle by ­isc_dsql_set_cursor_name().
        http://docwiki.embarcadero.com/InterBase/XE7/en/Isc_dsql_free_statement()}
        finally
          GDS.Release;
        end;
      end;
    end;

  inherited;

  if not Prepared then
    InternalUnprepare;
  Check(Res);
end;

procedure TGDSRecordSet.ExecCommand(Iters: integer = 1; Offset: integer = 0); // Execute command
var
  NeedPrepare: boolean;
begin
  NeedPrepare := (FCommand.CommandType <> ctCursor) and not Prepared;
  if NeedPrepare then
    FCommand.Prepare;
  try
    inherited;

    SetCommandType;
  finally // for Unprepare on Exception
    if (FCommand.CommandType <> ctCursor) and NeedPrepare then
      FCommand.Unprepare;
  end;
end;

procedure TGDSRecordSet.ExecFetch(DisableInitFields: boolean);
var
  OldExecuted: boolean;
begin
  FCommand.Executing := True;
  try
    OldExecuted := (FCommand.GetCursorState = csExecuted);

    if not OldExecuted then begin
      if not Prepared then
        InternalPrepare;
      FCommand.BindParams;
      FCommand.InternalExecute;
    end;

    if not RowsReturn then
      RaiseError(SNotRows);

    if not DisableInitFields and
      (not Prepared or NeedInitFieldsOnFetch)
    then
      CheckFieldDescs;

  except
    on E: Exception do begin
      try
        if Prepared and (FCommand.GetCursorState < csPrepared) then
          Prepared := False;
        if Assigned(FAfterExecFetch) then
          FAfterExecFetch(False);
      finally
        FCommand.Executing := False;
      end;
      raise;
    end;
  end;
  try
    if FFetchAll {$IFNDEF LITE}or (FSmartFetchState <> sfNone){$ENDIF} then
      try
        FetchAll;
      except
        Active := True;
        InternalClose;
        Active := False;
        raise;
      end;

    if Assigned(FAfterExecFetch) then
      FAfterExecFetch(True);
  finally
    FCommand.Executing := False;
  end;
end;

procedure TGDSRecordSet.Reopen;
var
  Res: ISC_STATUS;
begin
  if not IsFullReopen then begin
    if Active and (FCommand.GetStmtHandle <> nil) then begin
      GDS.Busy;
      try
        Res := GDS.isc_dsql_free_statement(FCommand.FStatusVector, FCommand.FStmtHandle, DSQL_close)
      finally
        GDS.Release;
      end;
      Check(Res);
    end;
  end;

  inherited;
end;

procedure TGDSRecordSet.Disconnect;
begin
  if (FConnection <> nil) and not FConnection.InProcessError and not GDS.AlerterFatalError then begin
    //Cache connection depenednt information
    GetIsFBConnection;
    GetMinorServerVersion;
    GetMajorServerVersion;
    GetUseUnicode;
    GetCharLength;
    GetSQLDialect;
  end;

  inherited;
end;

{ Fields}

class function TGDSRecordSet.GetBufferSize(DataType: Word; LengthInChars: integer): integer;
begin
  case DataType of
    dtVarBytes:
      Result := sizeof(word) {Readed bytes} + LengthInChars {Data} + 1 {Terminator};
  else
    Result := inherited GetBufferSize(DataType, LengthInChars);
  end;
end;

procedure TGDSRecordSet.DetectIdentityField;
{$IFNDEF FPC}
var
  FieldDesc: TCRFieldDesc;
  TableInfo: TCRTableInfo;
  i: integer;
{$ENDIF}
begin
  FIdentityField := nil;

{$IFNDEF FPC}
  TableInfo := UpdatingTableInfo;
  if TableInfo <> nil then begin
    for i := 0 to FFields.Count - 1 do begin
      FieldDesc := TCRFieldDesc(FFields[i]);
      if (FieldDesc.SubDataType = dtDbKey) and (FieldDesc.TableInfo = TableInfo) then begin
        FIdentityField := FieldDesc;
        Exit;
      end;
    end;
  end;
{$ENDIF}
end;

function TGDSRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TIBCFieldDesc;
end;

function TGDSRecordSet.GetIndicatorItemSize: Integer;
begin
  Result := SizeOf(Short);
end;

procedure TGDSRecordSet.InitRecordSize;
var
  i: integer;
  FieldDesc: TFieldDesc;
begin
  inherited;

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := FFields[i];
    if (FieldDesc.DataType = dtBoolean) and (FieldDesc.SubDataType in AllBooleanIntTypes) then
      FieldDesc.Size := 2;
  end;
end;

procedure TGDSRecordSet.CreateFieldDescs;

  procedure ConvertOLEDBTypeToInternalFormat(const InternalType: SmallInt;
    const CharsetID: integer; const SQLPrecision, SQLScale, SQLSubType: integer;
    const IsDbKey: Boolean;
    var DataType, SubType: Word; var DataSize: integer; out Precision: integer);
  begin
    Precision := 0;
    SubType := dtUnknown;
    case InternalType of
      SQL_IB_VARYING, SQL_IB_TEXT: begin
        if IsDbKey then
          DataType := dtBytes
        else
          if CharsetID = CH_OCTETS then begin
            if InternalType = SQL_IB_VARYING then
              DataType := dtVarBytes
            else
              DataType := dtBytes;
          end
          else begin
            if FConnection.FUseUnicode then
              DataType := dtWideString
            else
              DataType := dtString;
          end;
      end;
      SQL_IB_SHORT, SQL_IB_LONG, SQL_IB_INT64: begin
        case InternalType of
          SQL_IB_SHORT: begin
            if FConnection.FSimpleNumericMap then
              Precision := 4
            else
              Precision := IntegerPrecision div 2;
            DataType := dtSmallint;
          end;
          SQL_IB_LONG: begin
            if FConnection.FSimpleNumericMap then
              Precision := 9
            else
              Precision := IntegerPrecision;
            DataType := dtInteger;
          end;
          else begin
            if FConnection.FSimpleNumericMap then
              Precision := 18
            else
              Precision := IntegerPrecision * 2;
            DataType := dtInt64;
          end;
        end;

        if SQLPrecision <> 0 then
          Precision := SQLPrecision;
      {$IFDEF LITE}
        if Precision < 18 then   // In DbxIda we can not return ftfmtbcd field type.
          Precision := 15;       // Using Precision parameter we make dbExpress represent ftbcd as ftfmtbcd
      {$ENDIF}

      {$IFNDEF LITE}
        if (SQLScale <> 0) or (FConnection.FSimpleNumericMap and (SQLSubType <> 0)) then
          if (SQLScale <= 4) and (FConnection.EnableBCD or FCommand.EnableBCD)
            and (not FConnection.FSimpleNumericMap or (Precision <= MaxBcdPrecision - MaxBCDScale))
          then
            DataType := dtBCD
          else
      {$ENDIF}
          if not (FConnection.FSQLDialect = 1) and
          {$IFNDEF LITE}
            FConnection.EnableFMTBCD or FCommand.EnableFMTBCD
          {$ELSE}
            ((not FConnection.FOptimizedNumerics and ((SQLSubType <> 0) or (DataType = dtInt64) and (not FConnection.FEnableLargeint or (SQLScale <> 0))))
            or (FConnection.FOptimizedNumerics and (not FConnection.FEnableLargeint or (SQLScale <> 0)) and FConnection.FEnableFMTBCD and (DataType = dtInt64)))
          {$ENDIF}
          then
            DataType := dtFMTBCD
          else
        {$IFDEF LITE}
          if (SQLScale <> 0) or (DataType = dtInt64) and not FConnection.FEnableLargeint then
        {$ENDIF}
            DataType := dtFloat;

        DataSize := Precision + 1; //+ Sign
      end;
      SQL_IB_FLOAT: begin
        DataType := dtFloat;
        SubType := dtSingle;
        DataSize := FloatPrecision + 1; //+ Sign
      end;
      SQL_IB_DOUBLE: begin
        DataType := dtFloat;
        SubType := dtFloat;
        DataSize := FloatPrecision + 1; //+ Sign
      end;
      SQL_IB_BOOLEAN,
      SQL_FB_BOOLEAN: begin
        DataType := dtBoolean;
        DataSize := 1; //String boolean representation is '0' or '1'
      end;
      SQL_IB_TYPE_TIME: begin
        DataType := dtTime;
        DataSize := 13; //Time format HH:MM:SS.0000
      end;
      SQL_IB_TYPE_DATE: begin
        DataType := dtDate;
        DataSize := 11; //Time format YYYY-MM-DD (SQL Dial 3) or YYYY-MMM-DD (SQL Dial 1)
      end;
      SQL_IB_TIMESTAMP: begin
        DataType := dtDateTime;
        DataSize := 25; //Time format YYYY-MM-DD HH:MM:SS.0000 (SQL Dial 3)
                        //or YYYY-MMM-DD HH:MM:SS.0000 (SQL Dial 1)
      end;
      SQL_IB_QUAD, SQL_IB_BLOB: begin
        if FConnection.FEnableMemos and (SQLSubType = isc_blob_text) then begin
          if FConnection.FUseUnicode and
            ((CharsetId = CH_UNICODE_FSS) or
             (CharsetId = CH_UTF8) or
             ((FConnection.GDS.ClientVersion.Version > 7) and (FConnection.FDBCharsetId = CH_UTF8_IBXE)) or
             (CharsetId = CH_NONE))
          then
            DataType := dtWideMemo
          else
            DataType := dtMemo;
        end
        else
          DataType := dtBlob;
      end;
      SQL_IB_ARRAY: begin
        DataType := dtArray;
      end
      else
        RaiseError(SDataTypeNotSupported);
    end;
  end;

  procedure FillTablesAliases;
  var
    Parser: TIBCParser;
    TableName: string;// Table or view name
    StLex, Alias: string;
    Code: integer;
    TableInfo: TCRTableInfo;
  begin
    Parser := TIBCParser.Create(FCommand.SQL);
    Parser.OmitBlank := False;
    Parser.OmitComment := True;
    try
      Code := Parser.ToLexem([lxWITH, lxSELECT]);
      if Code = lxWITH then
        Code := Parser.ToLexem(lxSELECT, True);

      if Code <> lcEnd then
        if Parser.ToLexem(lxFROM, True) <> lcEnd then
          repeat
            repeat
              Code := Parser.GetNext(StLex);// Omit blank
            until Code <> lcBlank;

            // TableName
            TableName := StLex;
            while True do begin
              Code := Parser.GetNext(StLex);

              if (Code = lxComma) or (Code = lxLeftBracket) or (Code = lxRightBracket) then  // InterBase syntax "select * from storedproc(:p1,:p2)"
                Break;

              if (Code <> lcEnd) and (Code <> lcBlank) then
                TableName := TableName + StLex
              else
                Break;
            end;

            if Code = lcBlank then begin
              // 'AS' clause
              if Parser.GetNextToken = lxAS then
                Parser.GetNextToken // Omit blank
              else
                Parser.Back;

              // Alias
              Code := Parser.GetNext(Alias);
              if Code = lcIdent then begin
                repeat
                  Code := Parser.GetNextToken;// Omit blank
                until Code <> lcBlank;
              end
              else if Code = lxComma then
                Alias := ''
              else  begin
                Parser.Back;
                Alias := '';
              end;
            end
            else
              Alias := '';

            Assert(TableName <> '', 'TableName cannot be empty');
            TableName := FCommand.SQLInfo.NormalizeName(TableName);
            if Alias <> '' then
              TableInfo := FTablesInfo.FindByNameAndAlias(TableName, Alias)
            else
              TableInfo := FTablesInfo.FindByName(TableName);
            if TableInfo = nil then begin
              if Alias <> '' then
                TableInfo := FTablesInfo.FindByNameAndAlias(FCommand.SQLInfo.UnQuote(TableName), Alias)
              else
                TableInfo := FTablesInfo.FindByName(FCommand.SQLInfo.UnQuote(TableName));
              if TableInfo = nil then
                TableInfo := FTablesInfo.Add;
              TableInfo.TableName := TableName; //Add/(Replace quoted) table name
              TableInfo.TableAlias := '';
            end;

            if Alias <> '' then
              TableInfo.TableAlias := FCommand.SQLInfo.NormalizeName(Alias);
          until Code <> lxComma;
    finally
      Parser.Free;
    end;
  end;

  procedure GetBlobCharset(Field: TIBCFieldDesc; const FldTableName: string);
  //This operation could be done by isc_blob_lookup_desc2 or by Query to system table
  var
    TableName, FieldName: PStr;
    BlobDesc: IntPtr;
    Res: ISC_STATUS;
  begin
    if (GDS.ClientVersion.Version >= 2.1) and (GDS.ClientVersion.Version < 6) then begin
      if FConnection.FCharsetId <> CH_UTF8 then
        Field.CharsetID := CH_UNICODE_FSS
      else
        Field.CharsetID := CH_UTF8;
      Field.CharLength := FConnection.GetCharLength(Field.CharsetID, Field.ActualName);
    end
    else begin
      TableName := StringToPtrGDS(FldTableName, FConnection.FUseUnicode);
      FieldName := StringToPtrGDS(Field.ActualName, FConnection.FUseUnicode);
      BlobDesc := Marshal.AllocHGlobal(SizeOfBLOB_DESC);
      try
        GDS.Busy;
        try
          Res := GDS.isc_blob_lookup_desc(FCommand.FStatusVector, FConnection.FDatabaseHandle,
            {$IFNDEF CLR}@{$ENDIF}FCommand.FTransaction.FTransactionHandle, TableName, FieldName, BlobDesc, nil);
        finally
          GDS.Release;
        end;
        if Res <> isc_field_not_defined then begin
          Check(Res);
          Field.CharsetID := Marshal.ReadInt16(BlobDesc, 2);
          Field.CharLength := FConnection.GetCharLength(Field.CharsetID, Field.ActualName);
        end;
      finally
        FreeStringGDS(TableName, FConnection.FUseUnicode);
        FreeStringGDS(FieldName, FConnection.FUseUnicode);
        Marshal.FreeHGlobal(BlobDesc);
      end;
    end;
  end;

  procedure DescribeDefineFieldDesc(Field: TIBCFieldDesc; SQLVAR: TSQLVARAccessor; FieldNo: integer);

    function GetFieldLengthInChars(const Field: TIBCFieldDesc): cardinal;
    begin
      if (Field.DBType = ibcChar) or (Field.DBType = ibcVarchar) then
        Result := Field.DBLength div Field.CharLength
      else
        Result := Field.DBLength;
    end;

    procedure ChangeSqlType(SQLVAR: TSQLVARAccessor; SqlType: Short);
    begin
      if SQLVAR.IsNullable then
        SQLVAR.sqltype := SqlType or 1
      else
        SQLVAR.sqltype := SqlType;
    end;

  var
    TableInfo: TCRTableInfo;
    FArrTableName, FldTableName, FldTableAlias,
    FieldName: string;
    ArrType: TIBCArrayType;
    DataType,
    SubDataType: Word;
    Len, Scale: Integer;
    Fixed: Boolean;
  begin
    Field.Name := SQLVAR.aliasname;
    Field.ActualName := SQLVAR.sqlname;
    if Field.ActualName <> '' then
      FieldName := Field.ActualName
    else
      FieldName := Field.Name;
    Field.ActualFieldNo := FieldNo - 1;
    Field.ReadOnly := SQLVAR.relname = '';
    Field.CharsetID := 0;
    Field.CharLength := 1;
    Field.Length := 0;
    Field.Scale := 0;
    Field.SubDataType := dtUnknown;
    FldTableName := FCommand.SQLInfo.QuoteIfNeed(SQLVAR.relname);
    FldTableAlias := FCommand.SQLInfo.QuoteIfNeed(SQLVAR.relaliasname);

    if FldTableName <> '' then begin
      if FldTableAlias <> '' then
        TableInfo := FTablesInfo.FindByNameAndAlias(FldTableName, FldTableAlias)
      else
        TableInfo := FTablesInfo.FindByName(FldTableName);
      if TableInfo = nil then begin
        TableInfo := FTablesInfo.Add;
        TableInfo.TableName := FldTableName;
        TableInfo.TableAlias := FldTableAlias;
      end;
      Field.TableInfo := TableInfo;
    end
    else
      Field.TableInfo := nil;

    Field.DBType := TIBCConverterManager.GetDBType(SQLVAR.sqltype, SQLVAR.sqlsubtype);
    Field.DBLength := SQLVAR.sqllen;

    Field.DBScale := Abs(SQLVAR.sqlscale);
    Field.SQLSubType := SQLVAR.sqlsubtype;

    case SQLVAR.sqltype of
      SQL_IB_VARYING, SQL_IB_TEXT: begin
        if (Field.ActualName = 'DB_KEY') or (Field.ActualName = 'RDB$DB_KEY') then begin
          // DB_KEY field definition
          Field.Name := 'RDB$DB_KEY';       //Full DB_KEY name for correct Update/Insert/Delete SQLs generation
          Field.ActualName := 'RDB$DB_KEY';
          Field.SubDataType := dtDbKey;
          Field.ReadOnly := True;
        end;

        Field.CharsetID := Byte(SQLVAR.sqlsubtype); //LSB- CharsetID, MSB- CollateID
        Field.CharLength := FConnection.GetCharLength(Field.CharsetID, FieldName);
      end;
      SQL_IB_SHORT, SQL_IB_LONG, SQL_IB_INT64: begin
      Field.DBLength := SQLVAR.sqlprecision; // (ODBC) To work with InterBase as with Firebird
        if Field.DBLength = 0 then
          case SQLVAR.sqltype of
            SQL_IB_SHORT: Field.DBLength := 4;
            SQL_IB_LONG:  Field.DBLength := 9;
            SQL_IB_INT64: Field.DBLength := 18;
          end;
      end;
      SQL_IB_QUAD, SQL_IB_BLOB: begin
        if FConnection.FUseUnicode and (FldTableName <> '') then  //We need Blob CharsetID to perform right UTF conversion
          GetBlobCharset(Field, FldTableName);
      end;
    end;

    DetectFieldType({$IFNDEF LITE}Field.Name,{$ENDIF} Field.DBType, Field.DBLength, Field.DBScale, Field.SubDataType = dtDbKey, Field.CharsetID, Field.CharLength, DataType, SubDataType, Len, Scale, Fixed);

    Field.DataType := DataType;
    if Field.SubDataType = dtUnknown then
      Field.SubDataType := SubDataType;
    if SQLVAR.sqlprecision <> 0 then // (ODBC) To work with InterBase as with Firebird
      Field.Length := SQLVAR.sqlprecision
	  else
      Field.Length := Len;
    Field.Scale := Scale;

    if FFieldsAsString and (DataType in [dtFixedChar, dtString, dtExtString, dtFixedWideChar, dtWideString, dtExtWideString]) then begin
      SQLVAR.sqllen := Len;
      ChangeSqlType(SQLVAR, SQL_IB_TEXT); //SetUp SQLVAR sqltype to made server convert datatype to strings
    end;

    if (Field.DataType = dtWideString) or (Field.SubDataType in [dtWideString, dtFixedWideChar]) then
      SQLVAR.sqllen := Len * Field.CharLength; //to set proper field length

    if (not FFlatBuffers and (Field.Length >= FlatBufferLimit)) or
       ((Field.DataType = dtWideString) and ((Field.Length + SizeOf(Short)) > ($FFFF div SizeOf(WideChar))))
    then begin
      if Field.DataType = dtString then
        Field.DataType := dtExtString
      else
      if Field.DataType = dtWideString then
        Field.DataType := dtExtWideString
      else
      if (Field.DataType in [dtBytes, dtVarBytes]) and (Field.SubDataType <> dtDbKey) then
        Field.DataType := dtExtVarBytes;
    end;

    Field.Size := GetBufferSize(Field.DataType, Field.Length);

    case Field.DataType of
      dtBCD: begin
        ChangeSqlType(SQLVAR, SQL_IB_INT64); //SetUp SQLVAR sqltype to made server convert numbers to Currency
        SQLVAR.sqlscale := -4;
        SQLVAR.sqllen := SizeOf(Currency);
      end;
      dtFloat:
        if SQLVAR.sqltype <> SQL_IB_FLOAT then begin
          ChangeSqlType(SQLVAR, SQL_IB_DOUBLE); //SetUp SQLVAR sqltype to made server convert numbers to Float
          SQLVAR.sqlscale := 0;
          SQLVAR.sqllen := SizeOf(Double);
        end;
      dtBoolean: begin
        if (GDS.ClientVersion.Version < 7) and not (FConnection.IsFBServer and (GDS.ClientVersion.Version >= 3)) then begin
          ChangeSqlType(SQLVAR, SQL_IB_SHORT);
          SQLVAR.sqllen := SizeOf(Smallint);
        end;
      end;
      dtExtVarBytes:
        ChangeSqlType(SQLVAR, SQL_IB_VARYING); //Map all long OCTETS fields as VarChar
      dtArray: begin
        if FCommand.SQLInfo.IsQuoted(Trim(FldTableName)) then
          FArrTableName := FCommand.SQLInfo.UnQuote(Trim(FldTableName))
        else
          FArrTableName := Trim(FldTableName);
        ArrType := TIBCArrayType.Create(FConnection, FCommand.FTransaction, FArrTableName, Field.ActualName);
        try
          Field.ObjectType := ArrType;
        finally
          ArrType.Release;
        end;
      end;
    end;

    Field.Fixed := SQLVAR.sqltype = SQL_IB_TEXT;
    Field.Required := not SQLVAR.IsNullable and (FldTableName <> '') and (Field.SubDataType <> dtDbKey); // DB_KEY is readonly field
    FHasFlatUnicodeFields := FHasFlatUnicodeFields or (Field.DataType = dtWideString);
  end;

  procedure FillFieldsDomainNames(OnlyForBooleanFields: boolean = False);
  var
    i: integer;
    Filter, TableName, RelName, FieldName, Domain: string;
    RecordSet: TGDSRecordSet;
    RecBuf: IntPtr;
    Val: variant;
    Field: TIBCFieldDesc;
  begin
    Filter := GetTableNames(OnlyForBooleanFields);
    if Filter = '' then
      Exit;

    RecordSet := CreateRecordset(Format(
      'SELECT RDB$RELATION_NAME, RDB$FIELD_NAME, RDB$FIELD_SOURCE ' +
      'FROM RDB$RELATION_FIELDS ' +
      'WHERE RDB$RELATION_NAME IN (%s)', [Filter]));
    RecordSet.Open;
    RecordSet.FetchAll;
    RecordSet.SetToBegin;
    try
      if RecordSet.RecordCount > 0 then begin
        RecordSet.AllocRecBuf(RecBuf);
        try
          RecordSet.GetNextRecord(RecBuf);
          while not RecordSet.Eof do begin
            RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, Val);
            RelName := VarToStr(Val);
            RecordSet.GetFieldAsVariant(RecordSet.Fields[1], RecBuf, Val);
            FieldName := VarToStr(Val);
            RecordSet.GetFieldAsVariant(RecordSet.Fields[2], RecBuf, Val);
            Domain := VarToStr(Val);
            for i := 0 to FFields.Count - 1 do begin
              Field := TIBCFieldDesc(FFields[i]);
              if (Field.ParentField = nil) and (Field.TableInfo <> nil) then
                if not OnlyForBooleanFields or (Field.DataType in BooleanIntTypes) then begin
                  TableName := Field.TableInfo.TableName;
                  if (Field.ActualName = FieldName) and (TableName = RelName) then
                    Field.DomainName := Domain;
                end;
            end;
            RecordSet.GetNextRecord(RecBuf);
          end;
        finally
          Marshal.FreeHGlobal(RecBuf);
        end;
      end;
    finally
      RecordSet.Free;
    end;
  end;

  procedure DetectBooleanIntFields;
  var
    i: integer;
    Domain: string;
    Field: TFieldDesc;
  begin
    if not FSetDomainNames then
      FillFieldsDomainNames(True);

    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if Field.DataType in AllBooleanIntTypes then begin
        Domain := UpperCase(TIBCFieldDesc(Field).DomainName);
        if Pos('BOOLEAN', Domain) > 0 then begin
          Field.SubDataType := Field.DataType;
          Field.DataType := dtBoolean;
        end;
      end;
    end;
  end;

  function ReadInteger(var Buffer: IntPtr): integer;
  var
    Len: integer;
  begin
    Len := GDS.isc_vax_integer(Buffer, 2);
    Buffer := PtrOffset(Buffer, 2);
    Result := GDS.isc_vax_integer(Buffer, Len);
    Buffer := PtrOffset(Buffer, Len);
  end;

var
  i, cnt: integer;
  FieldsCount: integer;
  Field: TIBCFieldDesc;
  OldCursorState: TCursorState;
  CanGetAliasesInfo: boolean;
  Res: ISC_STATUS;
  BufPtr, EndBufPtr: IntPtr;
  b: byte;
  VarIndex, AliasLen: integer;
begin
  OldCursorState := FCommand.GetCursorState;
  try
    if FCommand.GetCursorState = csInactive then
      FCommand.Prepare;
    FCommand.CheckActive;

    if (FFieldXSQLDA = nil) or
      ((GDS.ClientVersion.Version < 7) and (FFieldXSQLDA.FXSQLVARType = vtGDS7)) or
      ((GDS.ClientVersion.Version >= 7) and (FFieldXSQLDA.FXSQLVARType = vtGDS)) then begin
      FFieldXSQLDA.Free;
      if GDS.ClientVersion.Version >= 7 then
        FFieldXSQLDA := TSQLDA.Create(FConnection, vtGDS7)
      else
        FFieldXSQLDA := TSQLDA.Create(FConnection, vtGDS);
      FFieldXSQLDA.AllocSQLDA(0);
    end;

    //Extract felds count and describe them
    GDS.Busy;
    try
      Check(GDS.isc_dsql_describe(FCommand.FStatusVector, FCommand.FStmtHandle, FConnection.FSQLDialect, FFieldXSQLDA.FXSQLDA));
      FieldsCount := FFieldXSQLDA.GetSqld;
      if FieldsCount > FFieldXSQLDA.GetSqln then begin
        FFieldXSQLDA.AllocSQLDA(FieldsCount);
        Check(GDS.isc_dsql_describe(FCommand.FStatusVector, FCommand.FStmtHandle, FConnection.FSQLDialect, FFieldXSQLDA.FXSQLDA));
      end;
    finally
      GDS.Release;
    end;

    //Get field table aliases
    CanGetAliasesInfo := IsFBConnection and (MajorServerVersion >= 2);
    if CanGetAliasesInfo then begin
      GDS.Busy;
      try
        Marshal.WriteByte(FCommand.FInfoTypeBuf, isc_info_sql_select);
        Marshal.WriteByte(PtrOffset(FCommand.FInfoTypeBuf, SizeOf(Byte)), isc_info_sql_describe_vars);
        Marshal.WriteByte(PtrOffset(FCommand.FInfoTypeBuf, SizeOf(Byte) * 2), isc_info_sql_sqlda_seq);
        Marshal.WriteByte(PtrOffset(FCommand.FInfoTypeBuf, SizeOf(Byte) * 3), frb_info_sql_relation_alias);
        Marshal.WriteByte(PtrOffset(FCommand.FInfoTypeBuf, SizeOf(Byte) * 4), isc_info_sql_describe_end);

        Res := GDS.isc_dsql_sql_info(FCommand.FStatusVector, FCommand.FStmtHandle, SizeOf(Byte) * 5,
          FCommand.FInfoTypeBuf, LocalBufferLen, FCommand.FRelAliasesBuf);
        Check(Res);

        BufPtr := FCommand.FRelAliasesBuf;
        EndBufPtr := PtrOffset(BufPtr, LocalBufferLen);
        if (Marshal.ReadByte(BufPtr) = isc_info_sql_select) and (Marshal.ReadByte(BufPtr, SizeOf(Byte)) = isc_info_sql_describe_vars) then begin
          BufPtr := PtrOffset(BufPtr, 2);
          cnt := ReadInteger(BufPtr);
          i := 0;
          VarIndex := 0;
          while (i < cnt) and (PtrCompare(BufPtr, EndBufPtr) < 0) do begin
            b := Marshal.ReadByte(BufPtr);
            BufPtr := PtrOffset(BufPtr, 1);
            case b of
              isc_info_sql_sqlda_seq:
                VarIndex := ReadInteger(BufPtr) - 1;
              frb_info_sql_relation_alias: begin
                AliasLen := GDS.isc_vax_integer(BufPtr, 2);
                BufPtr := PtrOffset(BufPtr, 2);
                if AliasLen > 0 then begin
                  FFieldXSQLDA.Vars[VarIndex].relaliasname := string(Marshal.PtrToStringAnsi(BufPtr, AliasLen));
                  BufPtr := PtrOffset(BufPtr, AliasLen);
                end
                else
                  FFieldXSQLDA.Vars[VarIndex].relaliasname := '';
                Inc(i);
              end;
              isc_info_end:
                Break;
              isc_info_sql_describe_end,
              isc_info_truncated:;
            end;
          end;
        end;
      finally
        GDS.Release;
      end;
    end;

    FTablesInfo.BeginUpdate;
    try
      FTablesInfo.CaseSensitive := True;

      FHasFlatUnicodeFields := False;
      i := 0;
      while i < FieldsCount do begin
        Field := TIBCFieldDesc(CreateFieldDesc);
        DescribeDefineFieldDesc(Field, FFieldXSQLDA.Vars[i], i + 1);
        Field.FieldNo := FFields.Count + 1;
        FFields.Add(Field);

        if (Field.DataType = dtObject) or ((Field.DataType = dtArray) and FComplexArrayFields) then
          InitObjectFields(Field.ObjectType, Field);

        Inc(i);
      end;

      if not CanGetAliasesInfo then
        FillTablesAliases;

      if FSetDomainNames then
        FillFieldsDomainNames;

      if FBooleanDomainFields then
        DetectBooleanIntFields;
    finally
      FTablesInfo.EndUpdate;
    end;

    if FExtendedFieldsInfo and not FExtFieldsInfoRecordSet then
      GetExtFieldsInfo
  finally
    if OldCursorState = csInactive then
      FCommand.Finish
    else
      FCommand.SetCursorState(OldCursorState);
  end;
end;

function TGDSRecordSet.GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
var
  DBObject: IntPtr;
begin
  CheckFetched(RecBuf, Field);

  if not Field.HasParent then
    if Field.DataType = dtArray then begin
      DBObject := Marshal.ReadIntPtr(RecBuf, Field.DataOffset);
      if DBObject <> nil then
        Result := TCustomIBCArray(GetGCHandleTarget(DBObject)).IsNull
      else
        Result := True;
    end
    else begin
      if Field.FieldDescKind <> fdkCalculated then
        Result := Marshal.ReadInt16(RecBuf, FDataSize + (Field.FieldNo - 1) * SizeOf(Short)) = -1
      else
        Result := Marshal.ReadInt16(RecBuf, FRecordSize + FCalcDataSize + (Field.ActualFieldNo - 1) * SizeOf(Short)) = -1;
    end
  else
    Result := GetChildFieldIsNull(Field, RecBuf);

  if Result then
    Result := GetNullByBlob(Field, RecBuf);
end;

procedure TGDSRecordSet.DetectFieldType({$IFNDEF LITE}const FieldName: string;{$ENDIF} DBType: Word; DBLength: Integer; DBScale: SmallInt;
  IsDbKey: Boolean; CharsetID: integer; CharLength: Integer;
  out DataType, SubDataType: Word; out Length, Scale: Integer; out Fixed: boolean);

  procedure ConvertOLEDBTypeToInternalFormat(out DataSize: integer; out Precision: integer);
  begin
    Precision := 0;
    DataSize := DBLength;
    SubDataType := dtUnknown;
    case DBType of
      ibcCharBin:
        DataType := dtBytes;
      ibcVarcharBin:
        if IsDbKey then
          DataType := dtBytes
        else
          DataType := dtVarBytes;
      ibcChar, ibcVarchar:
        if IsDbKey then
          DataType := dtBytes
        else if FConnection.FUseUnicode then
          DataType := dtWideString
        else
          DataType := dtString;
      ibcSmallint, ibcInteger, ibcBigint, ibcNumeric, ibcDecimal: begin
        case DBType of
          ibcSmallint: begin
            if FConnection.FSimpleNumericMap then
              Precision := 4
            else
              Precision := IntegerPrecision div 2;
            DataType := dtSmallint;
          end;
          ibcInteger: begin
            if FConnection.FSimpleNumericMap then
              Precision := 9
            else
              Precision := IntegerPrecision;
            DataType := dtInteger;
          end;
          ibcBigint: begin
            if FConnection.FSimpleNumericMap then
              Precision := 18
            else
              Precision := IntegerPrecision * 2;
            DataType := dtInt64;
          end;
          else begin
            if DBLength <= 4 then begin
              if FConnection.FSimpleNumericMap then
                Precision := 4
              else
                Precision := IntegerPrecision div 2;
              DataType := dtSmallint;
            end
            else if DBLength <= 9 then begin
              if FConnection.FSimpleNumericMap then
                Precision := 9
              else
                Precision := IntegerPrecision;
              DataType := dtInteger;
            end
            else if DBLength <= 18 then begin
              if FConnection.FSimpleNumericMap then
                Precision := 18
              else
                Precision := IntegerPrecision * 2;
              DataType := dtInt64;
            end;
          end;
        end;

      {$IFDEF LITE}
        if Precision < 18 then   // In DbxIda we can not return ftfmtbcd field type.
          Precision := 15;       // Using Precision parameter we make dbExpress represent ftbcd as ftfmtbcd
      {$ENDIF}

      {$IFNDEF LITE}
        if (DBScale <> 0) or (FConnection.FSimpleNumericMap and ((DBType = ibcNumeric) or (DBType = ibcDecimal))) then
          if (DBScale <= 4) and (FConnection.EnableBCD or FCommand.EnableBCD)
            and (not FConnection.FSimpleNumericMap or (Precision <= MaxBcdPrecision - MaxBCDScale))
          then
            DataType := dtBCD
          else
      {$ENDIF}
          if not (FConnection.FSQLDialect = 1) and
          {$IFNDEF LITE}
            FConnection.EnableFMTBCD or FCommand.EnableFMTBCD
          {$ELSE}
            ((not FConnection.FOptimizedNumerics and ((DBType = ibcNumeric) or (DBType = ibcDecimal) or (DataType = dtInt64) and (not FConnection.FEnableLargeint or (Scale <> 0))))
            or (FConnection.FOptimizedNumerics and (not FConnection.FEnableLargeint or (Scale <> 0)) and FConnection.FEnableFMTBCD and (DataType = dtInt64)))
          {$ENDIF}
          then
            DataType := dtFMTBCD
          else
        {$IFDEF LITE}
          if (Scale <> 0) or (DataType = dtInt64) and not FConnection.FEnableLargeint then
        {$ENDIF}
            DataType := dtFloat;

        DataSize := Precision + 1; //+ Sign
      end;
      ibcFloat: begin
        DataType := dtFloat;
        SubDataType := dtSingle;
        DataSize := FloatPrecision + 1; //+ Sign
      end;
      ibcDouble: begin
        DataType := dtFloat;
        SubDataType := dtFloat;
        DataSize := FloatPrecision + 1; //+ Sign
      end;
      ibcBoolean: begin
        DataType := dtBoolean;
        DataSize := 1; //String boolean representation is '0' or '1'
      end;
      ibcTime: begin
        DataType := dtTime;
        DataSize := 13; //Time format HH:MM:SS.0000
      end;
      ibcDate: begin
        DataType := dtDate;
        DataSize := 11; //Time format YYYY-MM-DD (SQL Dial 3) or YYYY-MMM-DD (SQL Dial 1)
      end;
      ibcTimestamp: begin
        DataType := dtDateTime;
        DataSize := 25; //Time format YYYY-MM-DD HH:MM:SS.0000 (SQL Dial 3)
                        //or YYYY-MMM-DD HH:MM:SS.0000 (SQL Dial 1)
      end;
      ibcText:
        if FConnection.FEnableMemos then
          if FConnection.FUseUnicode and
            ((CharsetId = CH_UNICODE_FSS) or
             (CharsetId = CH_UTF8) or
             ((FConnection.GDS.ClientVersion.Version > 7) and (FConnection.FDBCharsetId = CH_UTF8_IBXE)) or
             (CharsetId = CH_NONE))
          then
            DataType := dtWideMemo
          else
            DataType := dtMemo
        else
          DataType := dtBlob;
      ibcBlob:
        DataType := dtBlob;
      {$IFNDEF UNIDACPRO}IBCDataTypeMap{$ELSE}IBCDataTypeMapUni{$ENDIF}.ibcArray:
        DataType := dtArray;
      else
        RaiseError(SDataTypeNotSupported);
    end;
  end;

var
{$IFNDEF LITE}
  FetchConverter: TFetchConverter;
{$ENDIF}
  DataSize: integer;
begin
  Length := DBLength;
  Scale := DBScale;

{$IFNDEF LITE}
  FetchConverter := GetMapFetchConverter(FieldName, DBType, DBLength, DBScale, CharsetID);
  if FetchConverter <> nil then begin
    DataType := FetchConverter.InternalDataType;
    SubDataType := FetchConverter.DBType;
    DataSize := DBLength;
  end
  else
{$ENDIF}
  begin
    ConvertOLEDBTypeToInternalFormat(DataSize, Length);

    if FFieldsAsString and not (DataType in [dtBlob, dtMemo, dtWideMemo, dtArray]) then begin
      SubDataType := DataType;
      if FConnection.FUseUnicode and not (DataType in [dtBytes, dtVarBytes]) then
        DataType := dtWideString
      else
        DataType := dtString;
    end;

    if not FLongStrings and (DataSize > 255) then begin
      if DataType = dtString then begin
        DataType := dtMemo;
        SubDataType := dtString;
      end
      else
      if DataType = dtWideString then begin
        DataType := dtWideMemo;
        SubDataType := dtWideString;
      end;
    end;
  end;

  if (DataType = dtWideString) or (SubDataType in [dtWideString, dtFixedWideChar]) then
    // if (Copy(Field.ActualName, 1, 4) <> 'RDB$') or (FConnection.FDBCharsetId = CH_UTF8) then
    DataSize := DataSize div CharLength;  //Size in Chars

  case DataType of
    dtBCD: begin
      Scale := DBScale;
    end;
    dtFMTBCD: begin
      Scale := DBScale;
    end;
    dtFloat: begin
      Scale := DBScale;
    end;
    dtString:
      Length := DataSize;
    dtWideString, dtExtString, dtExtWideString,
    dtBytes, dtVarBytes:
      Length := DataSize;
    dtExtVarBytes:
      Length := DataSize;
    dtBlob, dtMemo, dtWideMemo: begin
      if SubDataType in [dtString, dtWideString] then
        Length := DataSize;
    end;
  end;

  Fixed := (DBType = ibcChar) or (DBType = ibcCharBin);
end;

procedure TGDSRecordSet.SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
var
  Ind: Short;
  Blob: TBlob;
begin
  if not Field.HasParent then begin
    if Value then
      Ind := -1
    else
      Ind := 0;
    if Field.FieldDescKind <> fdkCalculated then
      Marshal.WriteInt16(RecBuf, FDataSize + (Field.FieldNo - 1) * SizeOf(Short), Ind)
    else
      Marshal.WriteInt16(RecBuf, FRecordSize + FCalcDataSize + (Field.ActualFieldNo - 1) * SizeOf(Short), Ind);

    if Value and Field.IsBlob then begin // clear Blob value
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(PtrOffset(RecBuf, Field.DataOffset))));
      if Blob <> nil then
        Blob.Clear;
    end;
  end
  else
    PutChildField(Field, RecBuf, nil, 0);

{$IFNDEF LITE}
  SetEncrypted(Field, RecBuf, not Value);
{$ENDIF}
end;

{ Records }

procedure TGDSRecordSet.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Ptr: IntPtr;
  Blob: TBlob;
  Lob: TIBCBlob;
  IBCArray: TCustomIBCArray;
begin
  Ptr := PtrOffset(RecBuf, Field.DataOffset);
  case Field.DataType of
    dtBlob, dtMemo, dtWideMemo: begin
       //This is VarChar or Char field mapped on Memo
      if Field.SubDataType in [dtString, dtFixedChar, dtWideString, dtFixedWideChar] then begin
        inherited;

        Blob := TBlob(GetGCHandleTarget( Marshal.ReadIntPtr(PtrOffset(RecBuf, Field.DataOffset))));
        Blob.IsUnicode := UseUnicode and (TIBCFieldDesc(Field).CharsetID <> CH_OCTETS); //None and OCTETS are not converted to UTF8
      end
      else begin
        if (FConnection <> nil) and (FConnection.GetConnected) then
          Lob := TIBCBlob.Create(FConnection, FCommand.FTransaction) //We set Update Transaction on WriteParams/BindParam
        else
          Lob := TIBCBlob.Create(TGDSConnection(nil), TGDSTransaction(nil)); //Set DB and Transaction Handles on BindParams
        Lob.CharsetId := TIBCFieldDesc(Field).CharsetID;
        if Field.DataType = dtWideMemo then
          Lob.IsUnicode := True;
        Lob.Cached := FCommand.FCacheBlobs;
        Lob.Streamed := FCommand.FStreamedBlobs;
        Lob.FSubType := TIBCFieldDesc(Field).SQLSubType;
        Marshal.WriteIntPtr(Ptr, Lob.GCHandle);
      end;
    end;
    dtArray: begin
      if (FConnection <> nil) and (FConnection.GetConnected) then
        IBCArray := TCustomIBCArray.Create(FConnection, FCommand.FTransaction,
          GenerateTableName(Field), Field.ActualName)
      else
        IBCArray := TCustomIBCArray.Create(TGDSConnection(nil), TGDSTransaction(nil),
          GenerateTableName(Field), Field.ActualName);
      IBCArray.Cached := FCommand.FCacheArrays;
      IBCArray.ArrayType := TIBCArrayType(Field.ObjectType);
      Marshal.WriteIntPtr(Ptr, IBCArray.GCHandle);
    end
    else
      inherited;
  end;
end;

procedure TGDSRecordSet.FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  SharedObject: TSharedObject;
  b: boolean;
begin
  case Field.DataType of
    dtArray: begin
      SharedObject := TSharedObject(GetGCHandleTarget( Marshal.ReadIntPtr(PtrOffset(RecBuf, Field.DataOffset))));
      b := (SharedObject <> nil) and (SharedObject.RefCount = 1);
      // see TData.FreeComplexField for details
      SharedObject.Free;
      if b then
        Marshal.WriteIntPtr(RecBuf, Field.DataOffset, nil);
    end;
  else
    inherited;
  end;
end;

procedure TGDSRecordSet.CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc);
var
  SrcPtr, DestPtr: IntPtr;
  IBCArraySrc, IBCArrayDest: TCustomIBCArray;
begin
  case Field.DataType of
    dtArray: begin
      SrcPtr := Marshal.ReadIntPtr(PtrOffset(SourceRecBuf, Field.DataOffset));
      DestPtr := Marshal.ReadIntPtr(PtrOffset(DestRecBuf, Field.DataOffset));
      IBCArraySrc := TCustomIBCArray(GetGCHandleTarget(SrcPtr));
      IBCArrayDest := TCustomIBCArray(GetGCHandleTarget(DestPtr));
      IBCArrayDest.Assign(IBCArraySrc);
    end;
  else
    inherited;
  end;
end;

function TGDSRecordSet.GetSortOptions(SortColumn: TSortColumn): TCompareOptions;
begin
  Result := inherited GetSortOptions(SortColumn);

  if not (IsFBConnection and (MajorServerVersion >= 2)) then
    if not SortColumn.DescendingOrder then
      Result := Result + [coInvertNullOrder]; //InterBase specific NULL order
end;

function TGDSRecordSet.GetArrayFieldName(ObjectType: TObjectType; ItemIndex: integer): string;
begin
  Result :=  '[' + IntToStr(TIBCArrayType(ObjectType).LowBound + ItemIndex) + ']';
end;

{ Fetch }

class function TGDSRecordSet.IsObjectDataType(DataType: word): boolean;
begin
  Result := (DataType = dtArray);
end;

class function TGDSRecordSet.IsFetchBlockField(DataType: word): boolean;
begin
  Result := IsComplexDataType(DataType) or (DataType = dtWideString);
end;

class function TGDSRecordSet.IsProcessFieldType(DataType: word): boolean;
begin
  case DataType of
    dtDate, dtTime, dtDateTime, dtFMTBCD:
      Result := True;
    dtString:
      Result := True;
    dtBytes, dtVarBytes:
      Result := True;
    else
      Result := IsFetchBlockField(DataType);
  end;
end;

procedure TGDSRecordSet.AllocFetchBuffer;
var
  i: integer;
  FieldDesc: TFieldDesc;
begin
  if not (HasComplexFields or FHasFlatUnicodeFields) then
    Exit;

  if FFetchBuffer <> nil then
    FreeFetchBuffer;

  FFetchBufferSize := 0;
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := FFields[i];
    if (FieldDesc.FieldDescKind = fdkData) and
      not FieldDesc.HasParent and TIBCFieldDesc(FieldDesc).IsFetchBlock
    then
      Inc(FFetchBufferSize, GetComplexFldOffset(FieldDesc));
  end;
  if FFetchBufferSize > 0 then
    FFetchBuffer := Marshal.AllocHGlobal(FFetchBufferSize * FFetchRows);
end;

function TGDSRecordSet.GetComplexFldOffset(FieldDesc: TFieldDesc): integer;
begin
  Result := SizeOf(IntPtr);
  if (FieldDesc.DataType = dtExtString) or (FieldDesc.DataType = dtExtVarBytes) or
    (FieldDesc.DataType = dtMemo) and (FieldDesc.SubDataType in [dtString, dtFixedChar]) or
    (FieldDesc.DataType = dtExtWideString) or (FieldDesc.DataType = dtWideString) or
    (FieldDesc.DataType = dtWideMemo) and (FieldDesc.SubDataType in [dtWideString, dtFixedWideChar])
  then begin
    if (FieldDesc.DataType = dtExtString) or (FieldDesc.DataType = dtExtVarBytes) or
      (FieldDesc.DataType = dtMemo)
    then
      if (CharLength > 1) and (FieldDesc.DataType <> dtExtVarBytes) then
        Result := FieldDesc.Length * CharLength + 1 //+ 1 for terminator
      else
        Result := FieldDesc.Length + 1 //+ 1 for terminator
    else
      if (FieldDesc.DataType = dtExtWideString) or (FieldDesc.DataType = dtWideString) or
        (FieldDesc.DataType = dtWideMemo)
      then
        Result := FieldDesc.Length * TIBCFieldDesc(FieldDesc).CharLength + 1;
    if not FieldDesc.Fixed then
      Inc(Result, SizeOf(Short)); // SizeOf(Short) - string length storage
  end
  else
  case FieldDesc.DataType of
    dtBlob, dtMemo, dtWideMemo, dtArray:
      Result := SizeOf(TISC_QUAD);
  end;
end;


function TGDSRecordSet.CreateRecordset(SQL: string): TGDSRecordset;
begin
  Result := TGDSRecordSet.Create;
  Result.SetConnection(FConnection);
  Result.SetTransaction(FCommand.FTransaction);
  Result.SetSQL(SQL);

  Result.SetProp(prFlatBuffers, False);
  Result.SetProp(prLongStrings, True);
  Result.TrimFixedChar := True;
end;

function TGDSRecordSet.GetTableNames(OnlyForBooleanFields: boolean): string;
var
  i: integer;
  TableNames: TStringList;
  Field: TIBCFieldDesc;
  TableName: string;
begin
  Result := '';
  TableNames := TStringList.Create;
  try
    TableNames.Sorted := True;
    TableNames.Duplicates := dupIgnore;
    for i := 0 to FFields.Count - 1 do begin
      Field := TIBCFieldDesc(FFields[i]);
      if (Field.ParentField = nil) and (Field.TableInfo <> nil) then
        if not OnlyForBooleanFields or (Field.DataType in BooleanIntTypes) then begin
          TableName := Field.TableInfo.TableName;
          if TableName <> '' then
            TableNames.Add(TableName);
        end;
    end;
    if TableNames.Count = 0 then
      exit;

    for i := 0 to TableNames.Count - 1 do begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + AnsiQuotedStr(TableNames[i], '''');
    end;
  finally
    TableNames.Free;
  end;
end;

procedure TGDSRecordSet.GetExtFieldsInfo;
var
  Filter, FieldName: string;
  RecordSet: TGDSRecordSet;
  RecBuf: IntPtr;
  Val: variant;
  FieldDesc: TIBCFieldDesc;
begin
  Filter := GetTableNames(False);
  if Filter = '' then
    Exit;

  RecordSet := CreateRecordset(Format(
    'SELECT RELFL.RDB$FIELD_NAME, FLD.RDB$COMPUTED_BLR ' +
    'FROM RDB$FIELDS FLD ' +
    'JOIN RDB$RELATION_FIELDS RELFL ON (RELFL.RDB$FIELD_SOURCE = Fld.RDB$FIELD_NAME) ' +
    'JOIN RDB$RELATIONS REL ON (REL.RDB$RELATION_NAME = RELFL.RDB$RELATION_NAME) ' +
    'WHERE (RELFL.RDB$RELATION_NAME IN (%s)) ' +
    'AND (NOT Fld.RDB$COMPUTED_BLR IS NULL) ' +
    'AND (REL.RDB$VIEW_BLR IS NULL)', [Filter]));
  RecordSet.FExtFieldsInfoRecordSet := True;
  RecordSet.Open;
  RecordSet.FetchAll;
  RecordSet.SetToBegin;
  try
    if RecordSet.RecordCount > 0 then begin
      RecordSet.AllocRecBuf(RecBuf);
      try
        RecordSet.GetNextRecord(RecBuf);
        while not RecordSet.Eof do begin
          RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, Val);
          FieldName := VarToStr(Val);
          RecordSet.GetFieldAsVariant(RecordSet.Fields[1], RecBuf, Val);
          FieldDesc := FindField(FieldName) as TIBCFieldDesc;
          if (FieldDesc <> nil) and not VarIsNull(Val) then begin
            FieldDesc.ReadOnly := True;
            FieldDesc.FSkipSetReadOnly := True;
          end;
          RecordSet.GetNextRecord(RecBuf);
        end;
      finally
        Marshal.FreeHGlobal(RecBuf);
      end;
    end;
  finally
    RecordSet.Free;
  end;
end;

procedure TGDSRecordSet.InitBlock(Block: PBlockHeader);
var
  i, j: integer;
  Ptr: IntPtr;
  FieldPtr: IntPtr;
  FieldDesc: TFieldDesc;
  ComplexFieldOffset: integer;
  ItemSize: integer;
begin
// Create complex filds
  if not HasComplexFields then
    Exit;

  ComplexFieldOffset := 0;
  ItemSize := RecordSize + SizeOf(TItemHeader);
  for i := 0 to Block.ItemCount - 1 do begin
    Ptr := PtrOffset(Block, SizeOf(TBlockHeader) + i * ItemSize + SizeOf(TItemHeader));
    CreateComplexFields(Ptr, True);

    for j := 0 to FFields.Count - 1 do begin
      FieldDesc := FFields[j];
      if not FieldDesc.HasParent and TIBCFieldDesc(FieldDesc).IsFetchBlock and
        (FieldDesc.FieldDescKind = fdkData)
      then begin
        FieldPtr := PtrOffset(FFetchBuffer, ComplexFieldOffset);
        if (FieldDesc.DataType = dtMemo) and (FieldDesc.SubDataType in [dtString, dtFixedChar]) then // String as memo
          Marshal.WriteByte(FieldPtr, 0)
        else
        if (FieldDesc.DataType = dtWideMemo) and (FieldDesc.SubDataType in [dtWideString, dtFixedWideChar]) then
          Marshal.WriteInt16(FieldPtr, 0)
        else
        case FieldDesc.DataType of
          dtBlob, dtMemo, dtWideMemo, dtArray:
            Marshal.WriteInt64(FieldPtr, 0);
          dtWideString:
            if not FieldDesc.Fixed then
              Marshal.WriteInt16(FieldPtr, 0);
          dtExtString, dtExtWideString, dtExtVarBytes:
            Marshal.WriteIntPtr(FieldPtr, nil);
        end;
        Inc(ComplexFieldOffset, GetComplexFldOffset(FieldDesc));
      end;
    end;
  end;
end;

procedure TGDSRecordSet.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);

  procedure FillWideChar(var X; CharCount: Integer; const Value: WideChar);
  var
    PW: PWideChar;
  begin
    PW := @X;
    for CharCount := CharCount downto 1 do begin
      PW^ := Value;
      Inc(PW);
    end;
  end;

  procedure PrepareBlock;
  var
    i, j, k: integer;
    RecBuf: IntPtr;
    ObjPtr: IntPtr;
    Piece: PPieceHeader;
    SourcePtr: IntPtr;
    DataBuf: IntPtr;
    DataLenPtr: PWord;
    Ptr: IntPtr;
    FieldDesc: TCRFieldDesc;
    ComplexFieldOffset: integer;
    ItemSize: integer;
    BcdValue: TBCD;
    bcdscale: integer;
    HeapBuf: IntPtr;
    wsLen: integer;
    wsBuff: IntPtr;
    FieldDataType: word;
    Len: integer;

  begin
    ItemSize := RecordSize + SizeOf(TItemHeader);
    ComplexFieldOffset := 0;
    wsBuff := nil;
    wsLen := 0;
    try
      for i := 0 to RowsObtained - 1 do begin
        RecBuf := PtrOffset(Block, SizeOf(TBlockHeader) + i * ItemSize + SizeOf(TItemHeader));
        for j := 0 to FFields.Count - 1 do begin
          FieldDesc := TCRFieldDesc(FFields[j]);
          if not FieldDesc.HasParent and TIBCFieldDesc(FieldDesc).IsProcess and (FieldDesc.FieldDescKind = fdkData) then begin
            if (FieldDesc.ActualFieldNo > -1) {KeyOnly SmartFetchState} then begin
              ObjPtr := Marshal.ReadIntPtr(PtrOffset(RecBuf, FieldDesc.DataOffset));
              SourcePtr := PtrOffset(FFetchBuffer, ComplexFieldOffset);

                DataBuf := PtrOffset(RecBuf, FieldDesc.DataOffset);

              if (FieldDesc.DataType = dtMemo) and (FieldDesc.SubDataType in [dtString, dtFixedChar]) or // String as memo
                 (FieldDesc.DataType = dtWideMemo) and (FieldDesc.SubDataType in [dtWideString, dtFixedWideChar])
              then begin
                Len := Word(FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqllen);

                if FieldDesc.SubDataType in [dtString, dtFixedChar] then begin
                  if FieldDesc.Fixed and TrimFixedChar then begin
                    Ptr := PtrOffset(SourcePtr, Len - 1);
                    while (NativeUInt(Ptr) >= NativeUInt(SourcePtr)) and (PByte(Ptr)^ = $00) do
                      Dec(PByte(Ptr));
                    Len := PtrSubstract(Ptr, SourcePtr) + 1;
                  end;
                  if Len > 0 then begin
                    TBlob(GetGCHandleTarget(ObjPtr)).AllocPiece(Piece, Len); // for term
                    TBlob(GetGCHandleTarget(ObjPtr)).AppendPiece(Piece);
                    CopyBuffer(SourcePtr, PtrOffset(Piece, SizeOf(TPieceHeader)), Len);
                    Piece.Used := Len;
                  end;
                end
                else begin
                  if TrimFixedChar and FieldDesc.Fixed then
                    FCommand.TrimBuffer(SourcePtr, Len);

                  if Len > 0 then begin
                    TBlob(GetGCHandleTarget(ObjPtr)).AllocPiece(Piece, FieldDesc.Length * SizeOf(WideChar));
                    TBlob(GetGCHandleTarget(ObjPtr)).AppendPiece(Piece);
                    Ptr := PtrOffset(Piece, SizeOf(TPieceHeader));

                    Len := Utf8ToWs(SourcePtr, Len, Ptr, FieldDesc.Length * SizeOf(WideChar), False) - SizeOf(WideChar);
                    Piece.Used := Len;
                    TBlob(GetGCHandleTarget(ObjPtr)).Compress;
                  end;
                end;
              end
              else begin
                FieldDataType := FieldDesc.DataType;


                if FieldDesc.HasValueLen then begin
                  DataLenPtr := PtrOffset(RecBuf, FieldDesc.Offset);
                  if FieldDesc.Fixed or (FieldDesc.SubDataType in [dtFixedChar, dtFixedWideChar]) then
                    Len := Word(FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqllen)
                  else if TIBCFieldDesc(FieldDesc).IsFetchBlock then begin
                    Len := Marshal.ReadInt16(FFetchBuffer, ComplexFieldOffset);
                    SourcePtr := PtrOffset(SourcePtr, SizeOf(Short));
                  end
                  else
                    Len := DataLenPtr^;
                end
                else begin
                  DataLenPtr := nil;
                  Len := 0;
                end;

                case FieldDataType of
                  dtExtString: begin
                    if FieldDesc.Fixed and TrimFixedChar then
                      PIntPtr(DataBuf)^ := StringHeap.AllocTrimmedStr(SourcePtr, Len)
                    else
                      PIntPtr(DataBuf)^ := StringHeap.AllocStr(SourcePtr, Len); //Remove len fron VARCHAR
                    DataLenPtr^ := Word(Len);
                  end;
                  dtExtWideString: begin
                    if TrimFixedChar and FieldDesc.Fixed then
                      FCommand.TrimBuffer(SourcePtr, Len);

                    k := FieldDesc.Length * SizeOf(WideChar);
                    if wsLen < k then begin
                      wsLen := k;
                      if wsBuff <> nil then
                        Marshal.FreeHGlobal(wsBuff);
                      wsBuff := Marshal.AllocHGlobal(wsLen);
                    end;

                    if Len > 0 then
                      Len := Utf8ToWs(SourcePtr, Len, wsBuff, wsLen, False);

                    if FieldDesc.Fixed then
                      Len := Min(Len, FieldDesc.Length * SizeOf(WideChar));

                    if not TrimFixedChar and FieldDesc.Fixed and (wsLen > Len) then begin //ODBC
                      k := (wsLen - Len) shr 1;
                      FillWideChar(PtrOffset(wsBuff, Len)^, k, ' ');
                      Len := wsLen;
                    end;

                    Len := Len shr 1;
                    HeapBuf := StringHeap.AllocWideStr(wsBuff, Len);
                    Marshal.WriteIntPtr(DataBuf, HeapBuf);

                    DataLenPtr^ := Word(Len);
                  end;
                  dtWideString: begin
                    if TrimFixedChar and FieldDesc.Fixed then
                      FCommand.TrimBuffer(SourcePtr, Len);

                    wsLen := (FieldDesc.Length + 1) * SizeOf(WideChar);
                    Len := Utf8ToWs(SourcePtr, Len, DataBuf, wsLen) - SizeOf(WideChar);
                    DataLenPtr^ := Word(Len shr 1);
                  end;
                  dtString: begin
                    if TrimFixedChar and FieldDesc.Fixed then
                      FCommand.TrimBuffer(DataBuf, Len);
                    Marshal.WriteByte(DataBuf, Len, 0);
                    DataLenPtr^ := Word(Len);
                  end;
                  dtBytes: begin
                    DataLenPtr^ := Word(Len);
                  end;
                  dtExtVarBytes: begin
                    HeapBuf := StringHeap.NewBuf(Len);
                    Marshal.WriteIntPtr(DataBuf, HeapBuf);


                    CopyBuffer(SourcePtr, HeapBuf, Len);
                    DataLenPtr^ := Word(Len);
                  end;
                  dtBlob, dtMemo, dtWideMemo: begin
                    TIBCBlob(GetGCHandleTarget(ObjPtr)).ID := Marshal.ReadInt64(SourcePtr);
                  end;
                  dtArray: begin
                    TCustomIBCArray(GetGCHandleTarget(ObjPtr)).ArrayID := Marshal.ReadInt64(SourcePtr);
                  end;
                  dtDate, dtTime, dtDateTime: begin
                    SourcePtr := PtrOffset(RecBuf, FieldDesc.DataOffset);
                    case FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqltype of  //We need to know exact SQLTYPE to use right conversion
                      SQL_IB_TYPE_TIME:
                        IBCTimeToDateTime(SourcePtr, DataBuf);
                      SQL_IB_TYPE_DATE:
                        IBCDateToDateTime(SourcePtr, DataBuf);
                      SQL_IB_TIMESTAMP:
                        IBCTimeStampToDateTime(SourcePtr, DataBuf);
                    end;
                    continue;
                  end;
                  dtFMTBCD: begin
                    SourcePtr := PtrOffset(RecBuf, FieldDesc.DataOffset);
                    bcdscale := (-1 * FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].GetSqlScale);
                    case FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqltype of
                      SQL_IB_SHORT:
                        BcdValue := IBCDecimalToBcd(Marshal.ReadInt16(SourcePtr), bcdscale);
                      SQL_IB_LONG:
                        BcdValue := IBCDecimalToBcd(Marshal.ReadInt32(SourcePtr), bcdscale);
                      else
                        BcdValue := IBCDecimalToBcd(Marshal.ReadInt64(SourcePtr), bcdscale);
                    end;
                    PBcd(DataBuf)^ := BcdValue;
                    continue;
                  end;
                end;
              end;
            end;
            if TIBCFieldDesc(FieldDesc).IsFetchBlock then
              Inc(ComplexFieldOffset, GetComplexFldOffset(FieldDesc));
          end;
        end;
      end;
    finally
      if wsBuff <> nil then
        Marshal.FreeHGlobal(wsBuff);
    end;
  end;

var
  pRec, pInd: IntPtr;
  ComplexFieldOffset: integer;
  FieldDesc: TCRFieldDesc;
  BufferPtr, IndPtr, ObjPtr: IntPtr;
  Res: ISC_STATUS;
  Item: PItemHeader;
  ItemSize: integer;
  i, j: integer;
begin
  ItemSize := RecordSize + SizeOf(TItemHeader);
  pRec := PtrOffset(Block, SizeOf(TBlockHeader) + sizeof(TItemHeader));
  pInd := PtrOffset(pRec, DataSize);

{ DefineData }
  ComplexFieldOffset := 0;
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then begin  // Do not fetch Child, Calculated or LookUp fields
      // this code was never called before Blob as Memo support
      {if (FieldDesc.DataType in [dtMemo]) and
        (FieldDesc.SubDataType <> dtString) and (FieldDesc.SubDataType <> dtWideString) then begin
        Inc(ComplexFieldOffset, SizeOf(IntPtr));
      end
      else begin}
      if (FieldDesc.ActualFieldNo > -1) then begin {KeyOnly SmartFetchState}
        if TIBCFieldDesc(FieldDesc).IsFetchBlock then
          BufferPtr := PtrOffset(FFetchBuffer, ComplexFieldOffset)
        else begin
          if (FieldDesc.DataType = dtString) and not FieldDesc.Fixed then
            BufferPtr := PtrOffset(pRec, FieldDesc.Offset) //To Fetch VARCHAR types with length
          else
            BufferPtr := PtrOffset(pRec, FieldDesc.DataOffset);
        end;

        IndPtr := PtrOffset(pInd, i * SizeOf(Short));


        // Define data ptr and Ind Ptr to FieldSQLDA
        FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqlind := IndPtr;
        FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqldata := BufferPtr;
      //end;
      end;
      if TIBCFieldDesc(FieldDesc).IsFetchBlock then
        Inc(ComplexFieldOffset, GetComplexFldOffset(FieldDesc));
    end;
  end;

  FCommand.CheckActive;
  RowsObtained := 0;
  for i := 1 to FFetchRows do begin
    GDS.Busy;
    try
      Res := GDS.isc_dsql_fetch(FCommand.FStatusVector, FCommand.FStmtHandle, FConnection.FSQLDialect, FFieldXSQLDA.FXSQLDA);
    finally
      GDS.Release;
    end;

    if Res <> 0 then
      if Res = 100 then begin
        FCommand.SetCursorState(csFetched);
        Break;
      end
      else
        try
          Check(Res);
        except
          FCommand.SetCursorState(csFetched);
          raise;
        end
    else begin
      Inc(RowsObtained);

      //Setup SQLVARs with new data offsets
      for j := 0 to FFields.Count - 1 do begin
        FieldDesc := TCRFieldDesc(FFields[j]);
        if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) and
          (FieldDesc.ActualFieldNo > -1) {KeyOnly SmartFetchState}
        then begin
          IndPtr := FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqlind;
          BufferPtr := FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqldata;

          //Terminate fetched strings
          case FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqltype of
            SQL_IB_FLOAT:
              Double(BufferPtr^) := Single(BufferPtr^);
            SQL_FB_BOOLEAN:
              WordBool(BufferPtr^) := WordBool(Byte(BufferPtr^));
          end;

          if TIBCFieldDesc(FieldDesc).IsFetchBlock then
            BufferPtr := PtrOffset(BufferPtr, FFetchBufferSize)
          else
            BufferPtr := PtrOffset(BufferPtr, ItemSize);

          if not FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].IsNullable then
            Marshal.WriteInt16(IndPtr, 0); //NotNull
          IndPtr := PtrOffset(IndPtr, ItemSize);


          FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqlind := IndPtr;
          FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo].sqldata := BufferPtr;
        end;
      end; //for j
    end;
  end; //for i

  if RowsObtained > 0 then begin
    if HasFields([dtMemo, dtWideMemo, dtBlob, dtArray,
      dtString, dtExtString, dtWideString, dtExtWideString,
      dtBytes, dtVarBytes, dtExtVarBytes,
      dtDate, dtTime, dtDateTime, dtFMTBCD])
    then
      PrepareBlock;

    // Prepare complex field
    if HasComplexFields then begin
      Item := PtrOffset(Block, SizeOf(TBlockHeader));
      for i := 1 to RowsObtained do begin
        for j := 0 to FFields.Count - 1 do begin
          FieldDesc := TCRFieldDesc(FFields[j]);
          if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) and
            (FieldDesc.ActualFieldNo > -1) {KeyOnly SmartFetchState}
          then begin
            ObjPtr := Marshal.ReadIntPtr(PtrOffset(Item, SizeOf(TItemHeader) + FieldDesc.DataOffset));
            case FieldDesc.DataType of
              dtBlob, dtMemo, dtWideMemo: begin
                if (FieldDesc.SubDataType <> dtString) and (FieldDesc.SubDataType <> dtWideString) then begin
                  TIBCBlob(GetGCHandleTarget(ObjPtr)).FNeedReadBlob := True;
                  if not FDeferredBlobRead and FCommand.FCacheBlobs then begin
                    TIBCBlob(GetGCHandleTarget(ObjPtr)).ReadBlob;
                  {$IFDEF HAVE_COMPRESS}
                    if (FCommand.FCompressBlob = cbClient) or (FCommand.FCompressBlob = cbClientServer) then
                        TIBCBlob(GetGCHandleTarget(ObjPtr)).Compressed := True;
                  {$ENDIF}
                  end;
                end
              end;
              dtArray:
                if not FDeferredArrayRead and FCommand.FCacheArrays then
                  TCustomIBCArray(GetGCHandleTarget(ObjPtr)).ReadArray;
            end;
          end;
        end;
        Item := PtrOffset(Item, ItemSize);
      end;
    end;
  end;

{$IFNDEF LITE}
  if FSmartFetchState = sfDataByKey then
    FCommand.SetCursorState(csFetched);
{$ENDIF}
end;

function TGDSRecordSet.NeedUnPrepareAfterFetch: boolean;
begin
  Result := FAutoClose;
end;

procedure TGDSRecordSet.FetchAll;
begin
  if (FCommand.GetCursorState < csFetchingAll) and FCommand.GetActive then begin
    FCommand.SetCursorState(csFetchingAll);
    try
      while Fetch do;
    except
      if FCommand.GetCursorState <> csInactive then
        FCommand.SetCursorState(csFetched);
      raise;
    end;
  end;
end;

function TGDSRecordSet.RowsReturn: boolean;
begin
 if FCommand.CommandType <> ctUnknown then
    Result := inherited RowsReturn
  else                              //we need to know this info even if CommandType is not set(TCustomDADataSet.DoAfterExecute)
    Result := (FCommand.FSQLType = SQL_SELECT) or (FCommand.FSQLType = SQL_SELECT_FOR_UPD);
end;

{ Navigation }
procedure TGDSRecordSet.SetToEnd;
begin
  FetchAll;

  inherited;
end;

procedure TGDSRecordSet.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TGDSConnection(Value);
end;

function TGDSRecordSet.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

function TGDSRecordSet.GetGDS: TGDS;
begin
  Result := FConnection.FGDS;
end;

function TGDSRecordSet.GetIsFBConnection: boolean;
begin
  if FConnection <> nil then
    FIsFBConnection := FConnection.IsFBServer;
  Result := FIsFBConnection;
end;

function TGDSRecordSet.GetMinorServerVersion: integer;
begin
  if FConnection <> nil then
    FMinorServerVersion := FConnection.GetMinorServerVersion;
  Result := FMinorServerVersion;
end;

function TGDSRecordSet.GetMajorServerVersion: integer;
begin
  if FConnection <> nil then
    FMajorServerVersion := FConnection.GetMajorServerVersion;
  Result := FMajorServerVersion;
end;

function TGDSRecordSet.GetUseUnicode: boolean;
begin
  if FConnection <> nil then
    FUseUnicode := FConnection.FUseUnicode;
  Result := FUseUnicode;
end;

function TGDSRecordSet.GetCharLength: integer;
begin
  if FConnection <> nil then
    FCharLength := FConnection.FCharLength;
  Result := FCharLength;
end;

function TGDSRecordSet.GetSQLDialect: integer;
begin
  if FConnection <> nil then
    FSQLDialect := FConnection.FSQLDialect;
  Result := FSQLDialect;
end;

function TGDSRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoClose:
      FAutoClose := Boolean(Value);
    prFieldsAsString:
      FFieldsAsString := Boolean(Value);
    prComplexArrayFields:
      FComplexArrayFields := Boolean(Value);
    prDeferredArrayRead:
      FDeferredArrayRead := Boolean(Value);
    prDeferredBlobRead:
      FDeferredBlobRead := Boolean(Value);
    prBooleanDomainFields:
      FBooleanDomainFields := Boolean(Value);
    prSetDomainNames:
      FSetDomainNames := Boolean(Value);
    prQueryRowsAffected:
      FCommand.SetProp(prQueryRowsAffected, Boolean(Value));
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCursor:
      Value := Marshal.PtrToStringAnsi(FCommand.FCursor);
    prAutoClose:
      Value := FAutoClose;
    prFieldsAsString:
      Value := FFieldsAsString;
    prComplexArrayFields:
      Value := FComplexArrayFields;
    prDeferredArrayRead:
      Value := FDeferredArrayRead;
    prDeferredBlobRead:
      Value := FDeferredBlobRead;
    prBooleanDomainFields:
      Value := FBooleanDomainFields;
    prSetDomainNames:
      Value := FSetDomainNames;
    prQueryRowsAffected:
      FCommand.GetProp(prQueryRowsAffected, Value);
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

{$IFNDEF LITE}
function TGDSRecordSet.GetMapRule(const FieldName: string; DBType: Word; DBLength, DBScale, CharsetID: integer): TCRMapRule;
begin
  Result := FDataTypeMap.DetectFieldNameMapRule(FieldName, DBType, DBLength, DBScale);
  if (Result = nil) and (FCommand.FConnection <> nil) then
    Result := FCommand.FConnection.DataTypeMap.DetectFieldNameMapRule(FieldName, DBType, DBLength, DBScale);
  if (Result = nil) then
    Result := TIBCMapRules(FDataTypeMap).DetectDBTypeMapRule(DBType, DBLength, DBScale, CharsetID);
  if (Result = nil) and (FCommand.FConnection <> nil) then
    Result := TIBCMapRules(FCommand.FConnection.DataTypeMap).DetectDBTypeMapRule(DBType, DBLength, DBScale, CharsetID);
end;

function TGDSRecordSet.GetMapFetchConverter(const FieldName: string; DBType: Word; DBLength, DBScale, CharsetID: integer): TFetchConverter;
var
  Rule: TCRMapRule;
begin
  if FieldIsEncrypted(FieldName) then begin
    Result := nil;
    Exit;
  end;

  Rule := GetMapRule(FieldName, DBType, DBLength, DBScale, CharsetID);

  if Rule <> nil then
    Result := FDataTypeMap.GetConverterManager.DetectFetchConverter(DBType, DBLength, DBScale, Rule.DataType)
  else
    Result := nil;
end;

function TGDSRecordSet.GetMapOnDemandConverter(Field: TCRFieldDesc; out MapRule: TCRMapRule): TOnDemandConverter;

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
  MapRule := GetMapRule(Field.Name, Field.DBType, Field.DBLength, Field.DBScale, TIBCFieldDesc(Field).CharsetID);

  if MapRule <> nil then
    if not IsEqualDataType(MapRule.DataType, Field.DataType) or
       (TIBCMapRule(MapRule).CharsetID = -1) or
       ((MapRule.FieldLength <> -1) and (MapRule.FieldLength <> Field.DBLength)) or
       ((MapRule.FieldScale <> -1)  and (MapRule.FieldScale <> Field.DBScale))
    then
      Result := FDataTypeMap.GetConverterManager.DetectOnDemandConverter(Field.DataType, MapRule.DataType)
    else begin
      MapRule := nil;
      Result := nil;
    end
  else
    Result := nil;
end;
{$ENDIF}

procedure TGDSRecordSet.GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType, SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean);
var
  ObjPtr: IntPtr;
  Blob: TBlob;
begin
  case DataType of
    dtBlob, dtMemo, dtWideMemo: begin
//blob SubType is rarely set to right value:   if TIBCBlob(GetGCHandleTarget(ObjPtr)).SubType = isc_blob_text then
      if (SubDataType <> dtString) and (SubDataType <> dtWideString) then begin
        ObjPtr := Marshal.ReadIntPtr(DataBuf);
        Blob := TBlob(GetGCHandleTarget(ObjPtr));
        if UseRollback and not Blob.UseRollback and Blob.CanRollback then begin
          Blob.UseRollback := True;
          try
            Value := Blob.AsString;
          finally
            Blob.UseRollback := False;
          end;
        end
        else
          Value := Blob.AsString;
      end
      else
        inherited;
    end;
    dtArray: begin
      ObjPtr := Marshal.ReadIntPtr(DataBuf);
      Value := TCustomIBCArray(GetGCHandleTarget(ObjPtr)).Items;
    end;
  else
    inherited;
  end
end;

{$IFNDEF LITE}

{ TGDSMetaData }

function TGDSMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TGDSRecordSet.Create;
  Result.SetProp(prAutoClose, True);
  Result.SetProp(prExtendedFieldsInfo, False);
  Result.TrimFixedChar := True;
end;

function TGDSMetaData.InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData;
begin
  if MetaDataKind = 'generators' then
    Result := GetGenerators(Restrictions)
  else
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions);
end;

procedure TGDSMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  inherited;

  List.Add('Roles');
  List.Add('Generators');

  List.Sort;
end;

procedure TGDSMetaData.InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string);
begin
  List.Clear;

  if MetaDataKind = 'roles' then begin
    List.Add('ROLE_NAME');
  end
  else
  if MetaDataKind = 'generators' then begin
    List.Add('GENERATOR_NAME');
  end
  else
    inherited;
end;

function TGDSMetaData.GetTables(Restrictions: TStrings): TData;
const
  fmtGetTablesSQL = 'SELECT RDB$RELATION_NAME, ' +
    'RDB$VIEW_BLR, RDB$SYSTEM_FLAG, RDB$DESCRIPTION ' +
    'FROM RDB$RELATIONS %s ' +
    'ORDER BY RDB$RELATION_NAME';
var
  WhereClause, TableName, TableTypes, Scope, TypeFilter: string;
  BoolTypes: TBooleanArray;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);
  if (TableTypes = '') and (Scope = 'LOCAL') then
    TableTypes := 'TABLE, VIEW';

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$RELATION_NAME', TableName);

  BoolTypes := nil;
  if TableTypes <> '' then begin
    BoolTypes := ParseTypes(TableTypes, ['TABLE', 'VIEW', 'SYSTEM TABLE']);

    TypeFilter := '';
    if not BoolTypes[0] then
      TypeFilter := 'NOT (RDB$VIEW_BLR IS NULL)';
    if not BoolTypes[1] then begin
      if TypeFilter <> '' then
        TypeFilter := TypeFilter + ' AND ';
      TypeFilter := TypeFilter + 'RDB$VIEW_BLR IS NULL';
    end;
    if TypeFilter <> '' then
      TypeFilter := TypeFilter + ' AND ';
    TypeFilter := TypeFilter + 'RDB$SYSTEM_FLAG = 0';

    if BoolTypes[2] then begin
      if TypeFilter <> '' then
        TypeFilter := TypeFilter + ' OR ';
      TypeFilter := TypeFilter + 'RDB$SYSTEM_FLAG <> 0';
    end;

    if TypeFilter <> '' then begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND (' + TypeFilter + ')'
      else
        WhereClause := TypeFilter;
    end;
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetTablesSQL, [WhereClause]));
  FRecordSet.Open;
  CopyTablesData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TGDSMetaData.CreateTablesFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('TABLE_TYPE', dtString, 20);
  AddField('DESCRIPTION', dtMemo);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyTablesData(Restrictions: TStrings);
const
  snTABLE_NAME    = 1;
  snVIEW_BLR      = 2;
  snSYSTEM_FLAG   = 3;
  snDESCRIPTION   = 4;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
  dnDESCRIPTION   = 5;
var
  TypeName: string;
  Value: variant;
begin
  CreateTablesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snDESCRIPTION], [dnTABLE_NAME, dnDESCRIPTION]);

    if FRecordSetHelper.FieldValues[snSYSTEM_FLAG] = 1 then
      TypeName := 'SYSTEM TABLE'
    else begin
      Value := FRecordSetHelper.FieldValues[snVIEW_BLR];
      if VarIsNull(Value) then
        TypeName := 'TABLE'
      else
        TypeName := 'VIEW';
    end;

    FMemDataHelper.FieldValues[dnTABLE_TYPE] := TypeName;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetColumns(Restrictions: TStrings): TData;
const
  fmtGetColumnsSQL = 'SELECT RF.RDB$RELATION_NAME, RF.RDB$FIELD_NAME, ' +
    'RF.RDB$FIELD_POSITION, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, ' +
    'F.RDB$FIELD_LENGTH, F.RDB$FIELD_PRECISION, F.RDB$FIELD_SCALE, ' +
    'RF.RDB$NULL_FLAG, ' +
    'CASE WHEN RF.RDB$DEFAULT_VALUE IS NOT NULL THEN RF.RDB$DEFAULT_VALUE ELSE F.RDB$DEFAULT_VALUE END, ' +
    'CASE WHEN RF.RDB$DESCRIPTION IS NOT NULL THEN RF.RDB$DESCRIPTION ELSE F.RDB$DESCRIPTION END ' +
    'FROM RDB$RELATION_FIELDS RF ' +
    'INNER JOIN RDB$FIELDS F ON (RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME) %s ' +
    'ORDER BY RF.RDB$RELATION_NAME, RF.RDB$FIELD_POSITION';
var
  WhereClause, TableName, ColumnName: string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'RF.RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'RF.RDB$FIELD_NAME', ColumnName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  CopyColumnsData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TGDSMetaData.CreateColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('POSITION', dtInt32);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_SUBTYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  AddField('NULLABLE', dtInt32);
  AddField('DEFAULT_VALUE', dtMemo);
  AddField('DESCRIPTION', dtMemo);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyColumnsData(Restrictions: TStrings);
const
  snTABLE_NAME    = 1;
  snCOLUMN_NAME   = 2;
  snPOSITION      = 3;
  snTYPE          = 4;
  snSUBTYPE       = 5;
  snLENGTH        = 6;
  snPRECISION     = 7;
  snSCALE         = 8;
  snNULL_FLAG     = 9;
  snDEFAULT_VALUE = 10;
  snDESCRIPTION   = 11;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnCOLUMN_NAME   = 4;
  dnPOSITION      = 5;
  dnTYPE          = 6;
  dnSUBTYPE       = 7;
  dnLENGTH        = 8;
  dnPRECISION     = 9;
  dnSCALE         = 10;
  dnNULLABLE      = 11;
  dnDEFAULT_VALUE = 12;
  dnDESCRIPTION   = 13;
var
  Value: variant;
  Nullable: integer;
begin
  CreateColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_NAME, snCOLUMN_NAME, snPOSITION, snTYPE, snSUBTYPE, snLENGTH, snPRECISION, snSCALE, snDEFAULT_VALUE, snDESCRIPTION],
      [dnTABLE_NAME, dnCOLUMN_NAME, dnPOSITION, dnTYPE, dnSUBTYPE, dnLENGTH, dnPRECISION, dnSCALE, dnDEFAULT_VALUE, dnDESCRIPTION]);

    Value := FRecordSetHelper.FieldValues[snNULL_FLAG];
    if VarIsNull(Value) then
      Nullable := 1
    else
      Nullable := 0;
    FMemDataHelper.FieldValues[dnNULLABLE] := Nullable;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetProcedures(Restrictions: TStrings): TData;
const
  fmtGetProceduresSQL = 'SELECT RDB$PROCEDURE_NAME, ' +
    'RDB$PROCEDURE_INPUTS, RDB$PROCEDURE_OUTPUTS ' +
    'FROM RDB$PROCEDURES ' +
    '%s ORDER BY RDB$PROCEDURE_NAME';
  fmtGetProceduresSQL30 = 'SELECT RDB$PACKAGE_NAME, ' +
    'RDB$PROCEDURE_NAME, RDB$PROCEDURE_INPUTS, RDB$PROCEDURE_OUTPUTS ' +
    'FROM RDB$PROCEDURES ' +
    '%s ORDER BY RDB$PROCEDURE_NAME';
var
  WhereClause, ProcName: string;
begin
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$PROCEDURE_NAME', ProcName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  if TGDSRecordSet(FRecordSet).FCommand.FConnection.FIsFBServer and (TGDSRecordSet(FRecordSet).FCommand.FConnection.GetMajorServerVersion >= 3) then
    FRecordSet.SetSQL(Format(fmtGetProceduresSQL30, [WhereClause]))
  else
    FRecordSet.SetSQL(Format(fmtGetProceduresSQL, [WhereClause]));
  FRecordSet.Open;
  CopyProceduresData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TGDSMetaData.CreateProceduresFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 0);
  AddField('PROCEDURE_SCHEMA', dtString, 0);
  if TGDSRecordSet(FRecordSet).FCommand.FConnection.FIsFBServer and (TGDSRecordSet(FRecordSet).FCommand.FConnection.GetMajorServerVersion >= 3) then
    AddField('PROCEDURE_PACKAGE', dtString, 100);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PROCEDURE_TYPE', dtString, 0);
  AddField('IN_PARAMETERS', dtInt32);
  AddField('OUT_PARAMETERS', dtInt32);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyProceduresData(Restrictions: TStrings);
const
  snPROCEDURE_NAME = 1;
  snIN_PARAMETERS  = 2;
  snOUT_PARAMETERS = 3;

  dnPROCEDURE_NAME = 3;
  dnPROCEDURE_TYPE = 4;
  dnIN_PARAMETERS  = 5;
  dnOUT_PARAMETERS = 6;

  snPACKAGE_NAME30   = 1;
  snPROCEDURE_NAME30 = 2;
  snIN_PARAMETERS30  = 3;
  snOUT_PARAMETERS30 = 4;

  dnPACKAGE_NAME30   = 3;
  dnPROCEDURE_NAME30 = 4;
  dnPROCEDURE_TYPE30 = 5;
  dnIN_PARAMETERS30  = 6;
  dnOUT_PARAMETERS30 = 7;
var
  Value: variant;
begin
  CreateProceduresFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    if TGDSRecordSet(FRecordSet).FCommand.FConnection.FIsFBServer and (TGDSRecordSet(FRecordSet).FCommand.FConnection.GetMajorServerVersion >= 3) then begin
      CopyRecord([snPACKAGE_NAME30, snPROCEDURE_NAME30], [dnPACKAGE_NAME30, dnPROCEDURE_NAME30]);

      Value := FRecordSetHelper.FieldValues[snIN_PARAMETERS30];
      if VarIsNull(Value) then
        Value := 0;
      FMemDataHelper.FieldValues[dnIN_PARAMETERS30] := Value;

      Value := FRecordSetHelper.FieldValues[snOUT_PARAMETERS30];
      if VarIsNull(Value) then
        Value := 0;
      FMemDataHelper.FieldValues[dnOUT_PARAMETERS30] := Value;
    end
    else begin
      CopyRecord([snPROCEDURE_NAME], [dnPROCEDURE_NAME]);

      Value := FRecordSetHelper.FieldValues[snIN_PARAMETERS];
      if VarIsNull(Value) then
        Value := 0;
      FMemDataHelper.FieldValues[dnIN_PARAMETERS] := Value;

      Value := FRecordSetHelper.FieldValues[snOUT_PARAMETERS];
      if VarIsNull(Value) then
        Value := 0;
      FMemDataHelper.FieldValues[dnOUT_PARAMETERS] := Value;
    end;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
const
  fmtGetProcedureParametersSQL = 'SELECT P.RDB$PROCEDURE_NAME, P.RDB$PARAMETER_NAME, ' +
    'P.RDB$PARAMETER_NUMBER, P.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, ' +
    'F.RDB$FIELD_LENGTH, F.RDB$FIELD_PRECISION, F.RDB$FIELD_SCALE ' +
    'FROM RDB$PROCEDURE_PARAMETERS P ' +
    'INNER JOIN RDB$FIELDS F ON (P.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME) ' +
    '%s ORDER BY P.RDB$PROCEDURE_NAME, P.RDB$PARAMETER_TYPE, P.RDB$PARAMETER_NUMBER';
  fmtGetProcedureParametersSQL30 = 'SELECT RDB$PACKAGE_NAME, P.RDB$PROCEDURE_NAME, P.RDB$PARAMETER_NAME, ' +
    'P.RDB$PARAMETER_NUMBER, P.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, ' +
    'F.RDB$FIELD_LENGTH, F.RDB$FIELD_PRECISION, F.RDB$FIELD_SCALE ' +
    'FROM RDB$PROCEDURE_PARAMETERS P ' +
    'INNER JOIN RDB$FIELDS F ON (P.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME) ' +
    '%s ORDER BY P.RDB$PROCEDURE_NAME, P.RDB$PARAMETER_TYPE, P.RDB$PARAMETER_NUMBER';
var
  WhereClause, ProcName, ParamName: string;
begin
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ParamName := Trim(Restrictions.Values['PARAMETER_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'P.RDB$PROCEDURE_NAME', ProcName);
  AddWhere(WhereClause, 'P.RDB$PARAMETER_NAME', ParamName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  if TGDSRecordSet(FRecordSet).FCommand.FConnection.FIsFBServer and (TGDSRecordSet(FRecordSet).FCommand.FConnection.GetMajorServerVersion >= 3) then
    FRecordSet.SetSQL(Format(fmtGetProcedureParametersSQL30, [WhereClause]))
  else
    FRecordSet.SetSQL(Format(fmtGetProcedureParametersSQL, [WhereClause]));
  FRecordSet.Open;
  CopyProcedureParametersData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TGDSMetaData.CreateProcedureParametersFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 0);
  AddField('PROCEDURE_SCHEMA', dtString, 0);
  if TGDSRecordSet(FRecordSet).FCommand.FConnection.FIsFBServer and (TGDSRecordSet(FRecordSet).FCommand.FConnection.GetMajorServerVersion >= 3) then
    AddField('PROCEDURE_PACKAGE', dtString, 100);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PARAMETER_NAME', dtString, 100);
  AddField('POSITION', dtInt32);
  AddField('DIRECTION', dtString, 10);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_SUBTYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyProcedureParametersData(Restrictions: TStrings);
const
  snPROCEDURE_NAME = 1;
  snPARAMETER_NAME = 2;
  snPOSITION       = 3;
  snDIRECTION      = 4;
  snTYPE           = 5;
  snSUBTYPE        = 6;
  snLENGTH         = 7;
  snPRECISION      = 8;
  snSCALE          = 9;

  dnPROCEDURE_NAME = 3;
  dnPARAMETER_NAME = 4;
  dnPOSITION       = 5;
  dnDIRECTION      = 6;
  dnTYPE           = 7;
  dnSUBTYPE        = 8;
  dnLENGTH         = 9;
  dnPRECISION      = 10;
  dnSCALE          = 11;

  snPACKAGE_NAME30   = 1;
  snPROCEDURE_NAME30 = 2;
  snPARAMETER_NAME30 = 3;
  snPOSITION30       = 4;
  snDIRECTION30      = 5;
  snTYPE30           = 6;
  snSUBTYPE30        = 7;
  snLENGTH30         = 8;
  snPRECISION30      = 9;
  snSCALE30          = 10;

  dnPACKAGE_NAME30   = 3;
  dnPROCEDURE_NAME30 = 4;
  dnPARAMETER_NAME30 = 5;
  dnPOSITION30       = 6;
  dnDIRECTION30      = 7;
  dnTYPE30           = 8;
  dnSUBTYPE30        = 9;
  dnLENGTH30         = 10;
  dnPRECISION30      = 11;
  dnSCALE30          = 12;
var
  Value: variant;
  Direction: string;
begin
  CreateProcedureParametersFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    if TGDSRecordSet(FRecordSet).FCommand.FConnection.FIsFBServer and (TGDSRecordSet(FRecordSet).FCommand.FConnection.GetMajorServerVersion >= 3) then begin
      CopyRecord(
        [snPACKAGE_NAME30, snPROCEDURE_NAME30, snPARAMETER_NAME30, snPOSITION30, snTYPE30, snSUBTYPE30, snLENGTH30, snPRECISION30, snSCALE30],
        [dnPACKAGE_NAME30, dnPROCEDURE_NAME30, dnPARAMETER_NAME30, dnPOSITION30, dnTYPE30, dnSUBTYPE30, dnLENGTH30, dnPRECISION30, dnSCALE30]);

      Value := FRecordSetHelper.FieldValues[snDIRECTION30];
      if Value = 0 then
        Direction := 'IN'
      else
        Direction := 'OUT';
      FMemDataHelper.FieldValues[dnDIRECTION30] := Direction;
    end
    else begin
      CopyRecord(
        [snPROCEDURE_NAME, snPARAMETER_NAME, snPOSITION, snTYPE, snSUBTYPE, snLENGTH, snPRECISION, snSCALE],
        [dnPROCEDURE_NAME, dnPARAMETER_NAME, dnPOSITION, dnTYPE, dnSUBTYPE, dnLENGTH, dnPRECISION, dnSCALE]);

      Value := FRecordSetHelper.FieldValues[snDIRECTION];
      if Value = 0 then
        Direction := 'IN'
      else
        Direction := 'OUT';
      FMemDataHelper.FieldValues[dnDIRECTION] := Direction;
    end;
    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetIndexes(Restrictions: TStrings): TData;
const
  fmtGetIndexesSQL = 'SELECT RDB$RELATION_NAME, RDB$INDEX_NAME, ' +
    'RDB$UNIQUE_FLAG, RDB$INDEX_TYPE ' +
    'FROM RDB$INDICES ' +
    '%s ORDER BY RDB$RELATION_NAME, RDB$INDEX_NAME';
var
  WhereClause, TableName, IndexName: string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'RDB$INDEX_NAME', IndexName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetIndexesSQL, [WhereClause]));
  FRecordSet.Open;
  CopyIndexesData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TGDSMetaData.CreateIndexesFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 0);
  AddField('INDEX_SCHEMA', dtString, 0);
  AddField('INDEX_NAME', dtString, 100);
  AddField('UNIQUE', dtInt32);
  AddField('SORT_ORDER', dtString, 4);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyIndexesData(Restrictions: TStrings);
const
  snTABLE_NAME    = 1;
  snINDEX_NAME    = 2;
  snUNIQUE_FLAG   = 3;
  snINDEX_TYPE    = 4;

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
  dnSORT_ORDER    = 8;
var
  SortOrder: string;
  Unique: integer;
begin
  CreateIndexesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snINDEX_NAME], [dnTABLE_NAME, dnINDEX_NAME]);

    if FRecordSetHelper.FieldValues[snUNIQUE_FLAG] = 1 then
      Unique := 1
    else
      Unique := 0;
    FMemDataHelper.FieldValues[dnUNIQUE] := Unique;

    if FRecordSetHelper.FieldValues[snINDEX_TYPE] = 1 then
      SortOrder := 'DESC'
    else
      SortOrder := 'ASC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := SortOrder;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetIndexColumns(Restrictions: TStrings): TData;
const
  fmtGetIndexColumnsSQL = 'SELECT I.RDB$RELATION_NAME TABLE_NAME, I.RDB$INDEX_NAME INDEX_NAME, ' +
    'S.RDB$FIELD_NAME COLUMN_NAME, S.RDB$FIELD_POSITION COLUMN_POSITION, ' +
    'I.RDB$INDEX_TYPE ' +
    'FROM RDB$INDICES I ' +
    'INNER JOIN RDB$INDEX_SEGMENTS S ON (I.RDB$INDEX_NAME = S.RDB$INDEX_NAME) ' +
    '%s ORDER BY S.RDB$INDEX_NAME, S.RDB$FIELD_POSITION';
var
  WhereClause, TableName, IndexName, Uniqueness: string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  Uniqueness := Trim(Restrictions.Values['UNIQUE']);

  WhereClause := '';
  AddWhere(WhereClause, 'I.RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'I.RDB$INDEX_NAME', IndexName);
  if (Uniqueness = '0') or (Uniqueness = '1') then
    AddWhere(WhereClause, 'I.RDB$UNIQUE_FLAG', Uniqueness);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetIndexColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  CopyIndexColumnsData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TGDSMetaData.CreateIndexColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 0);
  AddField('INDEX_SCHEMA', dtString, 0);
  AddField('INDEX_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('COLUMN_POSITION', dtInt32);
  AddField('SORT_ORDER', dtString, 4);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyIndexColumnsData(Restrictions: TStrings);
const
  snTABLE_NAME    = 1;
  snINDEX_NAME    = 2;
  snCOLUMN_NAME   = 3;
  snPOSITION      = 4;
  snINDEX_TYPE    = 5;

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnCOLUMN_NAME   = 7;
  dnPOSITION      = 8;
  dnSORT_ORDER    = 9;
var
  SortOrder: string;
begin
  CreateIndexColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snINDEX_NAME, snCOLUMN_NAME, snPOSITION],
      [dnTABLE_NAME, dnINDEX_NAME, dnCOLUMN_NAME, dnPOSITION]);

    if FRecordSetHelper.FieldValues[snINDEX_TYPE] = 1 then
      SortOrder := 'DESC'
    else
      SortOrder := 'ASC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := SortOrder;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetConstraints(Restrictions: TStrings): TData;
const
  fmtGetConstraintsSQL = 'SELECT RDB$RELATION_NAME, RDB$CONSTRAINT_NAME, ' +
    'RDB$CONSTRAINT_TYPE, RDB$INDEX_NAME ' +
    'FROM RDB$RELATION_CONSTRAINTS ' +
    '%s ORDER BY RDB$RELATION_NAME, RDB$CONSTRAINT_NAME';
var
  WhereClause, TableName, ConstraintName, Types, TypesFilter: string;
  BoolTypes: TBooleanArray;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := Trim(Restrictions.Values['CONSTRAINT_TYPE']);

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'RDB$CONSTRAINT_NAME', ConstraintName);

  BoolTypes := nil;
  if Types <> '' then begin
    BoolTypes := ParseTypes(Types, ['CHECK', 'PRIMARY KEY', 'UNIQUE', 'FOREIGN KEY', 'UNKNOWN']);
    if not (BoolTypes[0] and BoolTypes[1] and BoolTypes[2] and BoolTypes[3] and BoolTypes[4])
    then begin
      TypesFilter := '';
      for i := 0 to High(BoolTypes) - 1 do begin
        if BoolTypes[i] xor BoolTypes[4] then begin
          if TypesFilter <> '' then
            TypesFilter := TypesFilter + ', ';
          case i of
            0: TypesFilter := TypesFilter + '''PCHECK'', ''NOT NULL''';
            1: TypesFilter := TypesFilter + '''PRIMARY KEY''';
            2: TypesFilter := TypesFilter + '''UNIQUE''';
            3: TypesFilter := TypesFilter + '''FOREIGN KEY''';
          end;
        end;
      end;
      if TypesFilter = '' then
        TypesFilter := '0 = 1'
      else begin
        TypesFilter := 'RDB$CONSTRAINT_TYPE IN (' + TypesFilter + ')';
        if BoolTypes[4] then
          TypesFilter := 'NOT ' + TypesFilter;
      end;
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      WhereClause := WhereClause + TypesFilter;
    end;
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetConstraintsSQL, [WhereClause]));
  FRecordSet.Open;
  CopyConstraintsData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TGDSMetaData.CreateConstraintsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('CONSTRAINT_NAME', dtString, 100);
  AddField('CONSTRAINT_TYPE', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 0);
  AddField('INDEX_SCHEMA', dtString, 0);
  AddField('INDEX_NAME', dtString, 100);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyConstraintsData(Restrictions: TStrings);
const
  snTABLE_NAME      = 1;
  snCONSTRAINT_NAME = 2;
  snCONSTRAINT_TYPE = 3;
  snINDEX_NAME      = 4;

  dnCATALOG         = 1;
  dnSCHEMA          = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCONSTRAINT_TYPE = 5;
  dnINDEX_CATALOG   = 6;
  dnINDEX_SCHEMA    = 7;
  dnINDEX_NAME      = 8;
var
  SourceType, DestType: string;
begin
  CreateConstraintsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snCONSTRAINT_NAME, snINDEX_NAME],
      [dnTABLE_NAME, dnCONSTRAINT_NAME, dnINDEX_NAME]);

    SourceType := VarToStr(FRecordSetHelper.FieldValues[snCONSTRAINT_TYPE]);
    if (SourceType = 'PRIMARY KEY') or (SourceType = 'FOREIGN KEY') or
      (SourceType = 'UNIQUE')
    then
      DestType := SourceType
    else
    if (SourceType = 'PCHECK') or (SourceType = 'NOT NULL') then
      DestType := 'CHECK'
    else
      DestType := 'UNKNOWN';
    FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := DestType;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetConstraintColumns(Restrictions: TStrings): TData;
const
  fmtGetConstraintColumnsSQL = 'SELECT I.RDB$RELATION_NAME, ' +
    'RC.RDB$CONSTRAINT_NAME, ' +
    'S.RDB$FIELD_NAME AS COLUMN_NAME, ' +
    '(S.RDB$FIELD_POSITION + 1) AS COLUMN_POSITION ' +
    'FROM RDB$INDEX_SEGMENTS S ' +
    'INNER JOIN RDB$INDICES I ON I.RDB$INDEX_NAME = S.RDB$INDEX_NAME ' +
    'INNER JOIN RDB$RELATION_CONSTRAINTS RC ON RC.RDB$INDEX_NAME = S.RDB$INDEX_NAME ' +
    '%s ORDER BY S.RDB$FIELD_POSITION ';
var
  WhereClause, TableName, ConstraintName, ColumnName : string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'I.RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'RC.RDB$CONSTRAINT_NAME', ConstraintName);
  AddWhere(WhereClause, 'S.RDB$FIELD_NAME', ColumnName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetConstraintColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  CopyConstraintColumnsData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TGDSMetaData.CreateConstraintColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('CONSTRAINT_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('COLUMN_POSITION', dtInt32);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyConstraintColumnsData(Restrictions: TStrings);
const
  snTABLE_NAME      = 1;
  snCONSTRAINT_NAME = 2;
  snCOLUMN_NAME     = 3;
  snCOLUMN_POSITION = 4;

  dnCATALOG         = 1;
  dnSCHEMA          = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCOLUMN_NAME     = 5;
  dnCOLUMN_POSITION = 6;
begin
  CreateConstraintColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snCONSTRAINT_NAME, snCOLUMN_NAME, snCOLUMN_POSITION],
      [dnTABLE_NAME, dnCONSTRAINT_NAME, dnCOLUMN_NAME, dnCOLUMN_POSITION]);
    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

  function TGDSMetaData.GetRoles(Restrictions: TStrings): TData;
const
  GetRolesSQL = 'SELECT RDB$ROLE_NAME ROLE_NAME ' +
    'FROM RDB$ROLES ' +
    '%s ORDER BY RDB$ROLE_NAME';
var
  WhereClause, RoleName: string;
begin
  RoleName := Trim(Restrictions.Values['ROLE_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$ROLE_NAME', RoleName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(GetRolesSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TGDSMetaData.GetGenerators(Restrictions: TStrings): TData;
const
  SQL = 'SELECT RDB$GENERATOR_NAME GENERATOR_NAME ' +
    'FROM RDB$GENERATORS ' +
    '%s ORDER BY RDB$GENERATOR_NAME';
var
  WhereClause, ObjName, Scope: string;
begin
  ObjName := Trim(Restrictions.Values['GENERATOR_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';

  if Scope = 'LOCAL' then
    WhereClause := 'RDB$SYSTEM_FLAG = 0';

  AddWhere(WhereClause, 'RDB$GENERATOR_NAME', ObjName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(SQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

{ TGDSLoader }

constructor TGDSLoader.Create;
begin
  inherited;

  FCommand := TGDSCommand.Create;
  FCommand.SetProp(prUseDescribeParams, True);
  FCommand.SetProp(prQueryRowsAffected, False);
  FInsertMode := _imInsert;
  FRowsPerBatch := 50;
end;

destructor TGDSLoader.Destroy;
begin
  FCommand.Free;

  inherited;
end;

function TGDSLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prInsertMode:
      FInsertMode := _TIBCInsertMode(Value);
    prRowsPerBatch:
      FRowsPerBatch := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prInsertMode:
      Value := Variant(FInsertMode);
    prRowsPerBatch:
      Value := FRowsPerBatch;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

class function TGDSLoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TGDSLoaderColumn;
end;

procedure TGDSLoader.Reset;
begin
  inherited;

  FCommand.Params.Clear;
end;

procedure TGDSLoader.Prepare;
begin
  Prepare(FRowsPerBatch);
end;

procedure TGDSLoader.Prepare(RecordCount: integer);
var
  i, BatchSize: integer;
  Col: TGDSLoaderColumn;
begin
  inherited Prepare;

  if FConnection.IsFBServer and (FConnection.GetMajorServerVersion >= 2) then begin
    if FRowsPerBatch > RecordCount then
      FRowsPerBatch := RecordCount;
    BatchSize := FRowsPerBatch;
  end
  else
    BatchSize := 1;

  case FInsertMode of
    _imInsert:
      FInsertHeader := 'INSERT INTO ';
    _imUpdateOrInsert:
      FInsertHeader := 'UPDATE OR INSERT INTO ';
  else
    Assert(False);
  end;
  FInsertHeader := FInsertHeader + FCommand.SQLInfo.NormalizeName(FTableName) + '(';

  for i := 0 to Columns.Count - 1 do begin
    Col := TGDSLoaderColumn(Columns[i]);
    if i > 0 then
      FInsertHeader := FInsertHeader + ',';
    FInsertHeader := FInsertHeader + FCommand.SQLInfo.NormalizeName(Columns[i].Name, QuoteNames);

    if BatchSize > 1 then
      Col.FDataTypeName := DataTypeName(Col.DataType, Col.Size, Col.Precision, Col.Scale);
  end;
  FInsertHeader := FInsertHeader + ') VALUES (';

  FCommand.SetConnection(FConnection);
  FCommand.SetTransaction(FTransaction);

  GenerateSQL(BatchSize);
end;

procedure TGDSLoader.GenerateSQL(BatchSize: integer);
var
  i: integer;
  ParamName: string;
  InsertSB: StringBuilder;
begin
  FCommand.Unprepare;
  FGeneratedRows := 0;
  InsertSB := StringBuilder.Create(MaxSQLLength);
  try
    InsertSB.Append(FInsertHeader);
    for i := 0 to Columns.Count - 1 do begin
      ParamName := 'P' + IntToStr(i);
      if i > 0 then
        InsertSB.Append(',');
      InsertSB.Append(':' + ParamName);
    end;
    InsertSB.Append(')');
    Inc(FGeneratedRows, BatchSize);
    FCommand.SetSQL(InsertSB.ToString);
    FCommand.Prepare;
    FCommand.Unprepare;
    for i := 0 to FCommand.Params.Count - 1 do begin
      FCommand.Params[i].SetArraySize(BatchSize);
      TIBCParamDesc(FCommand.Params[i]).InternalAllocBuffer;
    end;
  finally
    InsertSB.Free;
  end;
end;

function TGDSLoader.IsPutColumnDataAllowed: boolean;
begin
  Result := FInsertHeader <> '';
end;

procedure TGDSLoader.PutColumnData(Col, Row: integer; const Value: variant);
var
  Param: TIBCParamDesc;
begin
  if (FLastRow <> -1) and (Row = FLastRow + 2) and
    (FLastRow + 1 - FLoadedRows >= FGeneratedRows)
  then
    FlushRows;

  inherited;

  Param := TIBCParamDesc(FCommand.Params[Col]);
  Param.SetItemValue(Row - 1 - FLoadedRows, Value);
end;

procedure TGDSLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then
    FlushRows;
end;

procedure TGDSLoader.FlushRows;
var
  i, j, n: integer;
  Param: TIBCParamDesc;
begin
  if (FCommand.Params.Count > 0) and (FCommand.Params[0].GetArraySize > 1) then begin
    if FLastRow + 1 - FLoadedRows <> FGeneratedRows then
      n := FLastRow + 1 - FLoadedRows
    else
      n := FCommand.Params[0].GetArraySize;
    FCommand.ExecuteBatch(n);
  end
  else begin
    n := 1;
    FCommand.Execute;
  end;

  FLoadedRows := FLastRow + 1;

  if FUseBlankValues then
    if (FCommand.Params.Count > 0) and (FCommand.Params[0].GetArraySize > 1) then begin
      for i := 0 to FCommand.Params.Count - 1 do begin
        Param := TIBCParamDesc(FCommand.Params[i]);
        for j := 0 to n - 1 do
          Param.SetItemNull(j, True);
      end;
    end
    else
      for i := 0 to FCommand.Params.Count - 1 do
        FCommand.Params[i].SetNull(True);
end;

procedure TGDSLoader.Finish;
begin
  Reset;

  inherited;
end;

procedure TGDSLoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TGDSConnection(Value);
end;

{ TGDSAlerter }

constructor TGDSAlerter.Create;
begin
  inherited;

  FThreads := TList.Create;
end;

destructor TGDSAlerter.Destroy;
begin
  Stop;

  FThreads.Free;

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

function TGDSAlerter.SetProp(Prop: integer; const Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited SetProp(Prop, Value);
  //end;
end;

function TGDSAlerter.GetProp(Prop: integer; var Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited GetProp(Prop, Value);
  //end;
end;

procedure TGDSAlerter.SendEvent(const EventName, Message: string);
var
  Command: TGDSCommand;
begin
  if not TGDSConnection(FConnection).IsFBServer or
    (TGDSConnection(FConnection).GetMajorServerVersion < 2)
  then
    RaiseError(SSendEventNotSupported);

  Command := TGDSCommand(TGDSConnection(FConnection).CreateCommand);
  try
    Command.SetTransaction(Transaction);
    Command.SetSQL(
      'EXECUTE BLOCK (Name VARCHAR(1000) = :Name) AS'#13#10 +
      'BEGIN POST_EVENT Name; END');

    with Command.Params[0] do begin
      SetParamType(pdInput);
      if TGDSConnection(FConnection).FUseUnicode then begin
        SetDataType(dtWideString);
        SetSize(Length(WideString(EventName)));
      end
      else begin
        SetDataType(dtString);
        SetSize(LengthA(AnsiString(EventName)));
      end;
      SetValue(EventName);
    end;

    Command.Execute;
  finally
    Command.Free;
  end;
end;

procedure TGDSAlerter.Start;
var
  i, j: Integer;
  Name: string;
  Thread: TGDSAlerterThread;
begin
  if FActive then
    exit;

  Assert(FEventNames.Count > 0);

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  AllocIBDACWnd;
{$ENDIF}
{$ENDIF}

  for i := FEventNames.Count - 1 downto 0 do begin
    Name := Trim(FEventNames[i]);
    if Name = '' then
      FEventNames.Delete(i)
    else
    if Length(Name) > IBC_MAX_EVENT_LENGTH - 1 then
      RaiseError(Format(SEventNameTooLong, [IBC_MAX_EVENT_LENGTH]))
    else begin
      for j := 0 to i - 1 do
        if AnsiSameText(Name, FEventNames[j]) then begin
          FEventNames.Delete(i);
          Break;
        end;
    end;
  end;

  FActive := True;
  for i := 0 to (FEventNames.Count - 1) div IBC_MAX_EVENTS do begin
    Thread := TGDSAlerterThread.Create(Self, i);
    FThreads.Add(Thread);
  end;
end;

procedure TGDSAlerter.Stop;
var
  i: integer;
  AlerterThread: TGDSAlerterThread;
begin
  if not FActive then
    exit;

  FActive := False;
  for i := 0 to FThreads.Count - 1 do begin
    AlerterThread := TGDSAlerterThread(FThreads[i]);
    AlerterThread.Terminate;
  {$IFDEF MSWINDOWS}
    if GetCurrentThreadId <> AlerterThread.ThreadID then begin
      AlerterThread.WaitFor;
      AlerterThread.Free;
    end
    else
      AlerterThread.FreeOnTerminate := True; //when FatalError
  {$ELSE}
    AlerterThread.WaitFor;
    AlerterThread.Free;
  {$ENDIF}
  end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  GlobalCS.Enter;
  try
    if hIBDACWindow <> 0 then begin
      Dec(IBDACWindowCnt);
      if IBDACWindowCnt = 0 then begin
        DestroyWindow(hIBDACWindow);
        hIBDACWindow := 0;
      end;
    end;
  finally
    GlobalCS.Leave;
  end;
{$ENDIF}
{$ENDIF}

  FThreads.Clear;
end;

procedure TGDSAlerter.DoOnEvent(const EventName: string; EventCount: Integer);
var
  InhOnEvent: TCRAlerterEventCallback;
begin
  InhOnEvent := inherited OnEvent;
  if Assigned(InhOnEvent) then
    InhOnEvent(EventName, '');

  if Assigned(FOnEvent) then
    FOnEvent(EventName, EventCount);
end;

function TGDSAlerter.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

{ TGDSAlerterThread }

constructor TGDSAlerterThread.Create(Alerter: TGDSAlerter; EventGroup: Integer);
begin
  inherited Create(True);

  FAlerter := Alerter;
  FConnection := TGDSConnection(FAlerter.Connection);
  FEventGroup := EventGroup;
  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_STATUS));
  FEventIDPtr := Marshal.AllocHGlobal(SizeOf(Integer));
  FGCHandle := AllocGCHandle(Self);
  FSignal := TEvent.Create(nil, False, False, '');

{$IFNDEF FPC}
  Resume;
{$ELSE}
  Start;
{$ENDIF}
end;

destructor TGDSAlerterThread.Destroy;
begin
  CancelEvents;
  Marshal.FreeHGlobal(FEventIDPtr);
  Marshal.FreeHGlobal(FStatusVector);
  FreeGCHandle(FGCHandle);
  FSignal.Free;

  inherited;
end;

procedure TGDSAlerterThread.Terminate;
begin
  inherited;

  CancelEvents;
  FSignal.SetEvent;
end;

procedure TGDSAlerterThread.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    try
      FConnection.GDS.AlerterFatalError := True;
      FConnection.IBCError(FStatusVector, True, FConnection.Component);
    finally
      FConnection.GDS.AlerterFatalError := False;
    end;
end;

procedure TGDSAlerterThread.RegisterEvents;
var
  i, Index: integer;
  EventNames: array [0 .. IBC_MAX_EVENTS - 1] of AnsiString;
  EBP: array [0 .. IBC_MAX_EVENTS - 1] of IntPtr;
begin
  FEventBuffer := nil;
  FResultBuffer := nil;
  FEventBufferLen := 0;
  FFirstActivation := True;
  FEventCount := (FAlerter.EventNames.Count - (FEventGroup * IBC_MAX_EVENTS));
  if (FEventCount > IBC_MAX_EVENTS) then
    FEventCount := IBC_MAX_EVENTS;

  for i := 0 to IBC_MAX_EVENTS - 1 do
    EBP[i] := nil;
  for i := 0 to FEventCount - 1 do begin
    Index := i + FEventGroup * IBC_MAX_EVENTS;
    EventNames[i] := AnsiString(FAlerter.EventNames[Index]);
    EBP[i] := Marshal.StringToHGlobalAnsi(EventNames[i]);
  end;

  FConnection.GDS.Busy;
  try
    FEventBufferLen := FConnection.GDS.isc_event_block(FEventBuffer, FResultBuffer, FEventCount,
      EBP[0], EBP[1], EBP[2], EBP[3], EBP[4], EBP[5], EBP[6], EBP[7], EBP[8], EBP[9],
      EBP[10], EBP[11], EBP[12], EBP[13], EBP[14]);
  finally
    FConnection.GDS.Release;
  end;
end;

procedure TGDSAlerterThread.QueueEvents;
var
  Res: ISC_STATUS;
begin
  GlobalCS.Enter;
  try
    if FEventBuffer = nil then
      Exit;

    FConnection.GDS.Busy;
    try
      Res := FConnection.GDS.isc_que_events(FStatusVector,
        FConnection.FDatabaseHandle, FEventIDPtr, FEventBufferLen,
        FEventBuffer, ISCCallbackPtr, FGCHandle);
    finally
      FConnection.GDS.Release;
    end;
    Check(Res);
  finally
    GlobalCS.Leave;
  end;
end;

procedure TGDSAlerterThread.CancelEvents;
begin
  GlobalCS.Enter;
  try
    if FEventBuffer = nil then
      Exit;

    FConnection.GDS.Busy;
    try
      FConnection.GDS.isc_cancel_events(FStatusVector,
        FConnection.FDatabaseHandle, FEventIDPtr);

      EventsCS.Enter;
      try
        FConnection.GDS.isc_free(FEventBuffer);
        FEventBuffer := nil;
        FConnection.GDS.isc_free(FResultBuffer);
        FResultBuffer := nil;
        FEventBufferLen := 0;
      finally
        EventsCS.Leave;
      end;

    finally
      FConnection.GDS.Release;
    end;
  finally
    GlobalCS.Leave;
  end;
end;

procedure TGDSAlerterThread.CheckEvents;
var
  FiredNum: Int32;
  i: Integer;
{$IFDEF MSWINDOWS}
  Event: TAlertEvent;
{$ENDIF}
begin
  GlobalCS.Enter;
  try
    if FEventBuffer = nil then
      Exit;

    FConnection.GDS.Busy;
    try
      FConnection.GDS.isc_event_counts(FStatusVector, FEventBufferLen, FEventBuffer, FResultBuffer);
    finally
      FConnection.GDS.Release;
    end;

    if not FFirstActivation then begin
      for i := 0 to FEventCount - 1 do begin
        FiredNum := Int32(PtrOffset(FStatusVector, i * SizeOf(Long))^);
        if FiredNum > 0 then begin
        {$IFDEF MSWINDOWS}
          Event := TAlertEvent.Create;
          Event.Name := FAlerter.EventNames[(FEventGroup * IBC_MAX_EVENTS) + i];
          Event.Count := FiredNum;
          PostMessage(hIBDACWindow, WM_EVENTRECIEVED, WPARAM(FAlerter.GCHandle),
            LPARAM(AllocGCHandle(Event)));
        {$ELSE}
          FAlerter.DoOnEvent(FAlerter.EventNames[(FEventGroup * IBC_MAX_EVENTS) + i], FiredNum);
        {$ENDIF}
        end;
      end;
    end
    else
      FFirstActivation := False;
  finally
    GlobalCS.Leave;
  end;
end;

procedure TGDSAlerterThread.UpdateResultBuffer(Length: SmallInt; Updated: IntPtr);
begin
  if FResultBuffer <> nil then
    CopyBuffer(Updated, FResultBuffer, Length);
end;

procedure TGDSAlerterThread.Execute;
begin
  try
    RegisterEvents;
    QueueEvents;
    while not Terminated do begin
      FSignal.WaitFor($FFFFFFFF);
      if not Terminated then
        CheckEvents;
      if not Terminated then
        QueueEvents;
    end;
  except
  end;
end;

procedure EventCallback(P: IntPtr; Length: SmallInt; Updated: IntPtr); {$IFNDEF CLR} cdecl; {$ENDIF}
var
  Thread: TGDSAlerterThread;
begin
  if (P <> nil) and (Updated <> nil) then begin
    EventsCS.Enter;
    try
      Thread := TGDSAlerterThread(GetGCHandleTarget(P));
      Thread.UpdateResultBuffer(Length, Updated);
      Thread.FSignal.SetEvent;
    finally
      EventsCS.Leave;
    end;
  end;
end;

{$ENDIF}

{ TIBCBlob }

constructor TIBCBlob.Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
begin
  inherited Create;

  Init(DbHandle, TrHandle);
end;

constructor TIBCBlob.Create(Connection: TGDSConnection; Transaction: TGDSTransaction; IsUnicode: boolean = False; NeedFree: boolean = False);
begin
  inherited Create(IsUnicode);

  FNeedFree := NeedFree;

  FConnection := Connection;
  FTransaction := Transaction;
  if (FConnection <> nil) and (FTransaction <> nil) then
    Init(FConnection.GetDatabaseHandle, FTransaction.FTransactionHandle)
  else
    Init(nil, nil);
end;

procedure TIBCBlob.Init(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
begin
  FID := Marshal.AllocHGlobal(SizeOf(TISC_QUAD));
  Marshal.WriteInt64(FID, 0);
  FHandle := Marshal.AllocHGlobal(SizeOf(TISC_BLOB_HANDLE));
  Marshal.WriteIntPtr(FHandle, nil);
  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FDbHandle := Marshal.AllocHGlobal(SizeOf(TISC_DB_HANDLE));
  FTrHandle := Marshal.AllocHGlobal(SizeOf(TISC_TR_HANDLE));
  Marshal.WriteIntPtr(FDbHandle, DbHandle);
  Marshal.WriteIntPtr(FTrHandle, TrHandle);
  FDbHandleChecked := False;
  FTrHandleChecked := False;

///  PieceSize := 16384;
  FCharsetID := -1;
  FConversionCharsetID := -1;

  FStreamIsNull := True;
  FCached := True;
  FIsPrepared := False;
  FParamsChanged := True;
  FForceEncodeToUtf8 := False;
end;

destructor TIBCBlob.Destroy;
begin
  FreeBlob;

  Marshal.FreeHGlobal(FID);
  Marshal.FreeHGlobal(FHandle);
  Marshal.FreeHGlobal(FStatusVector);
  Marshal.FreeHGlobal(FDbHandle);
  Marshal.FreeHGlobal(FTrHandle);

  inherited Destroy;
end;

procedure TIBCBlob.PrepareBlob;
begin
  CheckDatabase;
  CheckTransaction;
  CloseBlob;
  AllocBlob(False);
  FIsPrepared := True;
end;

procedure TIBCBlob.WritePieceBlob(Size: Word; Ptr: IntPtr);
begin
  GDS.Busy;
  try
    Check(GDS.isc_put_segment(FStatusVector, FHandle, Size, Ptr));
  finally
    GDS.Release;
  end;
end;

function TIBCBlob.CreateClone: TBlob;
begin
  Result := TIBCBlob.Create(FConnection, FTransaction);

  TIBCBlob(Result).FIsUnicode := FIsUnicode;
  TIBCBlob(Result).FCached := FCached;
  TIBCBlob(Result).FStreamed := FStreamed;
  TIBCBlob(Result).FSubType := FSubType;
  TIBCBlob(Result).FConversionSubType := FConversionSubType;
  TIBCBlob(Result).FCharsetID := FCharsetID;
  TIBCBlob(Result).FConversionCharsetID := FConversionCharsetID;
end;

procedure TIBCBlob.Check(Status: ISC_STATUS);
var
  SqlErrMsg, Msg: string;
  ErrorNumber, ErrorCode: integer;
begin
  if Status > 0 then begin
    GDS.Busy;
    try
      ErrorCode := GDS.isc_sqlcode(FStatusVector);
    finally
      GDS.Release;
    end;
    GDS.GetIBError(ErrorCode, False, ErrorNumber, FStatusVector, Msg, SqlErrMsg);
    raise EIBCError.Create(ErrorCode, ErrorNumber, Msg, SqlErrMsg);
  end;
end;

procedure TIBCBlob.CreateBPB;
const
  DEFAULT_BPB_SIZE = 128;

  procedure CheckBPBSize (NeededSize: integer; var BPBSize: integer);
  begin
    if BPBSize < NeededSize then begin
      BPBSize := ((NeededSize div DEFAULT_BPB_SIZE) + 1) * DEFAULT_BPB_SIZE;
      SetLength(FBPB, BPBSize);
    end;
  end;

var
  BPBSize: integer;
begin
  BPBSize := DEFAULT_BPB_SIZE; //Start size that must cover common user needs
  SetLength(FBPB, BPBSize);
  FBPBLength := 0;
  if not(FSubTypeChanged or (FCharsetID >= 0) or FConversionSubTypeChanged or (FConversionCharsetID >= 0) or FStreamed) then
    Exit;
  FBPBLength := 1;
  FBPB[0] := isc_bpb_version1;
  try
    CheckBPBSize(FBPBLength + 19, BPBSize);
    if FSubTypeChanged then begin
      FBPB[FBPBLength] := isc_bpb_source_type;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := FSubType;
      Inc(FBPBLength, 3);
    end;
    if FConversionSubTypeChanged then begin
      FBPB[FBPBLength] := isc_bpb_target_type;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := FConversionSubType;
      Inc(FBPBLength, 3);
    end;
    if FCharsetID >= 0 then begin
      FBPB[FBPBLength] := isc_bpb_source_interp;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := FCharsetID;
      Inc(FBPBLength, 3);
    end;
    if FConversionCharsetID >= 0 then begin
      FBPB[FBPBLength] := isc_bpb_target_interp;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := FConversionCharsetID;
      Inc(FBPBLength, 3);
    end;
    if FStreamed then begin
      FBPB[FBPBLength] := isc_bpb_type;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := isc_bpb_type_stream;
      Inc(FBPBLength, 3);
    end;
  except
    SetLength(FBPB, 0);
    FBPBLength := 0;
    raise;
  end;
end;

procedure TIBCBlob.AllocBlob(Read: boolean = True);
begin
  if Marshal.ReadIntPtr(FHandle) = nil then begin
    CheckDatabase;
    CheckTransaction;

    if FParamsChanged then begin
      CreateBPB;
      FParamsChanged := False;
    end;

    GDS.Busy;
    try
      if Read then begin
        if Marshal.ReadInt64(FID) <> 0 then
          Check(GDS.isc_open_blob2(FStatusVector, FDbHandle, FTrHandle, FHandle, FID, FBPBLength, {$IFDEF FPC}PChar{$ENDIF}(FBPB)))
        else
          RaiseError(SCantReadEmptyBlobID);
      end
      else begin
        Marshal.WriteInt64(FID, 0);
        Check(GDS.isc_create_blob2(FStatusVector, FDbHandle, FTrHandle, FHandle, FID, FBPBLength, {$IFDEF FPC}PChar{$ENDIF}(FBPB)));
      end;
      FNativeHandle := True;
    finally
      GDS.Release;
    end;
  end;
  if Read and not FInfoReaded then
    GetBlobInfo; //We check if existing blob is streamed and determine its size
end;

procedure TIBCBlob.CloseBlob;
begin
  if Marshal.ReadIntPtr(FHandle) <> nil then begin
    GDS.Busy;
    try
      if FNativeHandle then
        Check(GDS.isc_close_blob(FStatusVector, FHandle));
      Marshal.WriteIntPtr(FHandle, nil);
      FInfoReaded := False;
      FNativeHandle := False;
    finally
      GDS.Release;
    end;
  end;
end;

procedure TIBCBlob.FreeBlob;
begin
  CloseBlob;
  Marshal.WriteInt64(FID, 0);
end;

procedure TIBCBlob.Disconnect;
begin
  FreeBlob;
end;

procedure TIBCBlob.CheckInfo;
begin
  if not FInfoReaded then
    AllocBlob;
end;

procedure TIBCBlob.GetBlobInfo;
var
  BlobItems: IntPtr;
  ItemsSize: integer;
  BlobInfo: IntPtr;
  InfoSize: integer;

  ItemOffset, ItemSize: integer;
  InfoItem: byte;
  Res: ISC_STATUS;

  function GetInfo(Info: IntPtr; OffSet, Size: integer): integer;
  begin
    case Size of
      1:  Result := Marshal.ReadByte(Info, OffSet);
      2:  Result := Marshal.ReadInt16(Info, OffSet);
      4:  Result := Marshal.ReadInt32(Info, OffSet);
    else
      Result := Marshal.ReadInt64(Info, OffSet);
    end;
  end;

begin
  InfoSize := 32 * 1; //SizeOf(Char) CLR fix
  BlobInfo := Marshal.AllocHGlobal(InfoSize);

  ItemsSize := 4 * 1; //SizeOf(Char) CLR fix
  BlobItems := Marshal.AllocHGlobal(ItemsSize);
  try
    Marshal.WriteByte(BlobItems, 0, isc_info_blob_num_segments);
    Marshal.WriteByte(BlobItems, 1, isc_info_blob_max_segment);
    Marshal.WriteByte(BlobItems, 2, isc_info_blob_total_length);
    Marshal.WriteByte(BlobItems, 3, isc_info_blob_type);
    GDS.Busy;
    try
      Res := GDS.isc_blob_info(FStatusVector, FHandle, ItemsSize, BlobItems, InfoSize, BlobInfo);
    finally
      GDS.Release;
    end;
    Check(Res);

    ItemOffset := 0;
    while (ItemOffset < InfoSize) and (Marshal.ReadByte(BlobInfo, ItemOffset) <> isc_info_end) do begin
      InfoItem := Marshal.ReadByte(BlobInfo, ItemOffset);
      ItemSize := Marshal.ReadInt16(BlobInfo, ItemOffset + 1);
      case InfoItem of
        isc_info_blob_num_segments:
          FNumSegments := GetInfo(BlobInfo, ItemOffset + 3, ItemSize);
        isc_info_blob_max_segment:
          FMaxSegmentSize := GetInfo(BlobInfo, ItemOffset + 3, ItemSize);
        isc_info_blob_total_length:
          FBlobLength := GetInfo(BlobInfo, ItemOffset + 3, ItemSize);
        isc_info_blob_type: begin
          FStreamed := GetInfo(BlobInfo, ItemOffset + 3, ItemSize) = 1; { 0 = segmented, 1 = streamed }
        end;
      end;
      ItemOffset := ItemOffset + 3 + ItemSize;
    end;
  finally
    Marshal.FreeHGlobal(BlobInfo);
    Marshal.FreeHGlobal(BlobItems);
  end;
  FInfoReaded := True;
end;

procedure TIBCBlob.CheckValue;
begin
  if FNeedReadBlob then
    ReadBlob;
end;

function TIBCBlob.GetSize: Cardinal;
begin
  if FNeedReadBlob and FIsUnicode then
    ReadBlob;

  if FNeedReadBlob then
    Result := LengthBlob
  else
    Result := inherited GetSize;
end;

function TIBCBlob.GetSizeAnsi: cardinal;
begin
  Assert(FIsUnicode);
  if FNeedReadBlob then
    ReadBlob;

  Result := inherited GetSizeAnsi;
end;

procedure TIBCBlob.CheckAlloc;
begin
  if Marshal.ReadIntPtr(FHandle) = nil then
    RaiseError(SBlobNotAllocatted);
end;

procedure TIBCBlob.CheckDatabase;
begin
  if not FDbHandleChecked then
    if FConnection <> nil then begin
      Marshal.WriteIntPtr(FDbHandle, FConnection.GetDatabaseHandle);
      FDbHandleChecked := True;
    end;

  if FDbHandle = nil then
    RaiseError(SConnectionNotDefined);
end;

procedure TIBCBlob.CheckTransaction;
begin
  if not FTrHandleChecked then
    if FTransaction <> nil then begin
      Marshal.WriteIntPtr(FTrHandle, FTransaction.FTransactionHandle);
      FTrHandleChecked := True;
    end;

  if FTrHandle = nil then
    RaiseError(STransactionNotAssigned);
end;

function TIBCBlob.IsInit: boolean;
begin
  Result := Marshal.ReadInt64(FID) <> 0;
end;

function TIBCBlob.LengthBlob: integer;
begin
  CheckDatabase;
  CheckTransaction;
  if IsInit then begin
    CheckInfo;
    Result := FBlobLength;
    // reset FNeedReadBlob to avoid multiple LengthBlob calls on empty LOB
    if Result = 0 then
      FNeedReadBlob := False;
  end
  else begin
    Result := 0;
    FNeedReadBlob := False;
  end;
end;

function TIBCBlob.CharSize: Byte;
begin
  Result := 1;
  if FISUnicode then Result := 2;
end;

procedure TIBCBlob.ReadBlob;
var
  WsPiece, Piece: PPieceHeader;
  PActualReadLen: IntPtr;
  PActualPosition: IntPtr;
  Res: integer;
  BlobLength: Integer;
  BuffPtr: IntPtr;
  UsedCount: Integer;
  MaxPieceSize: Integer;
  Len: integer;
  StatusVector: NativeInt;
begin
  CheckDatabase;
  CheckTransaction;
  FData.Clear;
  if IsInit then begin
    if GetHandle = nil then
      AllocBlob(True)
    else if FStreamed then begin
      PActualPosition := Marshal.AllocHGlobal(SizeOf(ISC_LONG));
      try
        GDS.Busy;
        try
          Res := GDS.isc_seek_blob(FStatusVector, FHandle, 0, 0, PActualPosition);
        finally
          GDS.Release;
        end;
        Check(Res);
      finally
        Marshal.FreeHGlobal(PActualPosition);
      end;
    end;

    PActualReadLen := Marshal.AllocHGlobal(SizeOf(Word));
    try
      BlobLength := LengthBlob;
      if (BlobLength > 0) then begin
        GDS.Busy;
        try
          UsedCount := 0;
          if (FMaxSegmentSize < PieceSize) and (FMaxSegmentSize > 0) then
            MaxPieceSize := FMaxSegmentSize
          else
            MaxPieceSize := PieceSize;

          while UsedCount < BlobLength do begin
            if FIsUnicode then
              AllocPiece(Piece, BlobLength)
            else
              if (BlobLength - UsedCount) > MaxPieceSize then
                AllocPiece(Piece, MaxPieceSize)
              else
                AllocPiece(Piece, BlobLength - UsedCount);

            try
              Res := 0;

              while Piece.Used < Piece.Size do begin
                BuffPtr := PtrOffset(Piece, Sizeof(TPieceHeader) + Piece.Used);
                Res := GDS.isc_get_segment(FStatusVector, FHandle, PActualReadLen, Min(Piece.Size - Piece.Used, $FFFF{MaxWord}), BuffPtr);
                StatusVector := NativeInt(PtrOffset(FStatusVector, 1 * SizeOf(ISC_STATUS))^);
              if not ((Res = 0) or (StatusVector = isc_segment) or (StatusVector = 0)) then
                  if Res = isc_segstr_eof then
                    Break
                  else
                    Check(Res);
                Piece.Used := Piece.Used + word(Marshal.ReadInt16(PActualReadLen));
              end;
              UsedCount := UsedCount + Piece.Used;
            except
              FreePiece(Piece);
              raise;
            end;

            AppendPiece(Piece);
            if Res = isc_segstr_eof then
              Break;
          end;
        finally
          GDS.Release;
        end;

        if FIsUnicode then begin //BLOB unicode settings is separate from connection unicode settings
          //Convert UTF8 to UTF16
          Len := UsedCount * SizeOf(WideChar); //if all utf8 chars lesser than 128
          AllocPiece(WsPiece, Len);
          try
            Len := Utf8ToWs(PtrOffset(Piece, Sizeof(TPieceHeader)), UsedCount, PtrOffset(WsPiece, Sizeof(TPieceHeader)), Len, False);
            WsPiece.Used := Len;
            FreePiece(Piece);
            CompressPiece(WsPiece);
          except
            FreePiece(WsPiece);
            raise;
          end;
          AppendPiece(WsPiece);
        end;

        if not FStreamed then
          FCached := True;
      end;
    finally
      Marshal.FreeHGlobal(PActualReadLen);
      if FCached or not FStreamed then
        CloseBlob;
    end;
  end;
  FNeedReadBlob := False;
end;

procedure TIBCBlob.WriteBlob;
var
  Piece: PPieceHeader;
  BufLen: Integer;
  WriteSize: word;
  Offset, Chars: integer;
  utfPiece: IntPtr;
{$IFNDEF FPC}
  CodePage: Word;
{$ENDIF}
begin
{$IFNDEF FPC}
  CodePage := CP_UTF8;
{$ENDIF}
  PrepareBlob;
  try
    utfPiece := nil;
    if FIsUnicode then
      utfPiece := Marshal.AllocHGlobal(PieceSize + 1);

    Piece := FirstPiece;
    try
      while IntPtr(Piece) <> nil do begin
        BufLen := Piece.Used;
        Offset := 0;
      {$IFNDEF VER24P}
        WriteSize := 0;
      {$ENDIF}
        while BufLen > 0 do begin
          if FIsUnicode then begin
            if BufLen = 1 then
              Break;
            if BufLen div 2 > PieceSize div 3 then
              Chars := PieceSize div 3
            else
              Chars := BufLen div 2;

          {$IFNDEF FPC}
            CharSetIDToCodePage(Connection.FCharsetId, CodePage, Connection.FIsFBServer);
            if (Connection.FCharset = '') or (CodePage = CP_UTF8) or (FForceEncodeToUtf8) then begin
          {$ENDIF}
              WriteSize := {$IFNDEF NEXTGEN}CRFunctions.{$ENDIF}UnicodeToUtf8(PAChar(utfPiece), PieceSize + 1,
                PtrOffset(Piece, Sizeof(TPieceHeader) + Offset), Chars);
              if WriteSize > 0 then
                Dec(WriteSize); // #0
          {$IFNDEF FPC}
            end
            else
              WriteSize := {$IFDEF VER15P}LocaleCharsFromUnicode{$ELSE}WideCharToMultiByte{$ENDIF}(CodePage, 0, PWideChar(PtrOffset(Piece, Sizeof(TPieceHeader) + Offset)), Chars, PAChar(utfPiece), PieceSize + 1, nil, nil);
          {$ENDIF}

            WritePieceBlob(WriteSize, utfPiece);

            WriteSize := Chars * 2;
          end
          else begin
            if BufLen > PieceSize then
              WriteSize := PieceSize
            else
              WriteSize := BufLen;

            WritePieceBlob(WriteSize, PtrOffset(Piece, Sizeof(TPieceHeader) + Offset));
          end;

          Dec(BufLen, WriteSize);
          Inc(Offset, WriteSize);
        end;
        Piece := Piece.Next;
      end;
      CheckInfo; //To achieve information about real blob state on server side
                 //Current blobID will be invalid after insert or update operation
      FBlobLength := Size; //temporary blob size fix (info returns first segment length)
    finally
      if FIsUnicode then
        Marshal.FreeHGlobal(utfPiece);
    end;
  finally
    CloseBlob;
  end;
end;

function TIBCBlob.Read(Position, Count: cardinal; Dest: IntPtr): cardinal;
var
  PActualReadLen, PActualPosition: IntPtr;
  ReadOffset: cardinal;
  ReadSize: word;
  Res: integer;
begin
  Result := 0;
  if not Cached and (Marshal.ReadIntPtr(FHandle) <> nil) then begin
    CheckAlloc;
    CheckDatabase;
    CheckTransaction;
    if IsInit then begin
      if FStreamed then begin
        PActualPosition := Marshal.AllocHGlobal(SizeOf(ISC_LONG));
        try
          GDS.Busy;
          try
            Res := GDS.isc_seek_blob(FStatusVector, FHandle, 0, Position, PActualPosition);
          finally
            GDS.Release;
          end;
          Check(Res);
          if Cardinal(Marshal.ReadInt32(PActualPosition)) <> Position then
            RaiseError(SCantSetReadPosition);
        finally
          Marshal.FreeHGlobal(PActualPosition);
        end;
      end;

      PActualReadLen := Marshal.AllocHGlobal(SizeOf(Word));
      try
        GDS.Busy;
        try
          ReadOffset := 0;
          while Cardinal(Count - ReadOffset) > 0 do begin
            if Cardinal(Count - ReadOffset) > Cardinal(PieceSize) then
              ReadSize := PieceSize
            else
              ReadSize := Cardinal(Count - ReadOffset);
            Res := GDS.isc_get_segment(FStatusVector, FHandle, PActualReadLen, ReadSize, PtrOffset(Dest, ReadOffset));
            ReadOffset := ReadOffset + Word(Marshal.ReadInt16(PActualReadLen));
            if not ((Res = 0) or (NativeInt(PtrOffset(FStatusVector, 1 * SizeOf(ISC_STATUS))^) = isc_segment)) then
              if Res = isc_segstr_eof then begin
                Result := ReadOffset;
                if not FStreamed then begin
                  CloseBlob;
                  FCached := True;
                end;
                Exit;
              end
              else
                Check(Res);
          end;
          Result := ReadOffset;
        finally
          GDS.Release;
        end;
      {Unicode support BLOB Streamed read - no UTF16 conversion}
      finally
        Marshal.FreeHGlobal(PActualReadLen);
      end;
    end;
  end
  else
    Result := inherited Read(Position, Count, Dest);
end;

procedure TIBCBlob.Write(Position, Count: cardinal; Source: IntPtr);
var
  PActualPosition: IntPtr;
  Res: integer;
begin
  FStreamIsNull := False;
  if not Cached and not FForceCachedMode and FIsPrepared then begin
    if FStreamed then begin
      PActualPosition := Marshal.AllocHGlobal(SizeOf(ISC_LONG));
      try
        GDS.Busy;
        try
          Res := GDS.isc_seek_blob(FStatusVector, FHandle, 0, Position, PActualPosition);
        finally
          GDS.Release;
        end;
        Check(Res);
        if Cardinal(Marshal.ReadInt32(PActualPosition)) <> Position then
          RaiseError(SCantSetReadPosition);
      finally
        Marshal.FreeHGlobal(PActualPosition);
      end;
    end;

    WritePieceBlob(Count, Source);
  end
  else begin
    if FForceCachedMode or not FIsPrepared then begin
      FCached := True;
      FForceCachedMode := True;
    end;
    inherited Write(Position, Count, Source);
  end;
end;

procedure TIBCBlob.Clear;
begin
  Clear(False);
end;

procedure TIBCBlob.Clear(ResetStreamIsNull: boolean);
begin
  FNeedReadBlob := False;
  if ResetStreamIsNull then
    FStreamIsNull := True;

  inherited Clear;
end;

procedure TIBCBlob.Truncate(NewSize: cardinal);
begin
  if NewSize = 0 then
    FNeedReadBlob := False
  else
    CheckValue;

  inherited Truncate(NewSize);

  if FStreamed then
    Marshal.WriteInt64(FID, 0);
end;

function TIBCBlob.GetIDPtr: PISC_QUAD;
begin
  Result := FID;
end;

function TIBCBlob.GetID: TISC_QUAD;
begin
  Result := Marshal.ReadInt64(FID);
end;

procedure TIBCBlob.SetID(Value: TISC_QUAD);
begin
  FreeBlob;
  Marshal.WriteInt64(FID, Value);
end;

function TIBCBlob.GetHandle: TISC_BLOB_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FHandle);
end;

procedure TIBCBlob.SetHandle(Value: TISC_BLOB_HANDLE);
begin
  FreeBlob;
  Marshal.WriteIntPtr(FHandle, Value);
  if Marshal.ReadIntPtr(FHandle) <> nil then
    FNativeHandle := False;
end;

function TIBCBlob.GetDbHandle: TISC_DB_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FDbHandle);
end;

function TIBCBlob.GetTrHandle: TISC_TR_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FTrHandle);
end;

procedure TIBCBlob.SetDbHandle(Value: TISC_DB_HANDLE);
begin
  Marshal.WriteIntPtr(FDbHandle, Value);
end;

procedure TIBCBlob.SetTrHandle(Value: TISC_TR_HANDLE);
begin
  Marshal.WriteIntPtr(FTrHandle, Value);
end;

procedure TIBCBlob.SetConnection(Value: TGDSConnection);
begin
  if Value <> nil then
    SetDbHandle(Value.GetDatabaseHandle);
  FConnection := Value;
end;

procedure TIBCBlob.SetTransaction(Value: TGDSTransaction);
begin
  if Value <> nil then
    SetTrHandle(Value.FTransactionHandle);
  FTransaction := Value;
end;

procedure TIBCBlob.SetSubType(Value: integer);
begin
  FSubTypeChanged := True;
  FParamsChanged := True;
  FSubType := Value;
end;

procedure TIBCBlob.SetCharsetID(Value: integer);
begin
  FParamsChanged := True;
  FCharsetID := Value;
end;

procedure TIBCBlob.SetConversionSubType(Value: integer);
begin
  FConversionSubTypeChanged := True;
  FParamsChanged := True;
  FConversionSubType := Value;
end;

procedure TIBCBlob.SetConversionCharsetID(Value: integer);
begin
  FParamsChanged := True;
  FConversionCharsetID := Value;
end;

procedure TIBCBlob.SetStreamed(Value: boolean);
begin
  if FStreamed <> Value then begin
    if not Value and (GetHandle <> nil) then
      RaiseError(SCannotChangeStreamed);
    FStreamed := Value;
    FParamsChanged := True;
    FInfoReaded := False;
  end;
end;

procedure TIBCBlob.SetCached(const Value: boolean);
begin
  if FCached <> Value then begin
    if not Value then begin
      if (IntPtr(FirstPiece) <> nil) then   //Blob is already readed
        RaiseError(SCannotDisableBlobCache)
    end
    else
      ReadBlob;
    FCached := Value;
  end;
end;

procedure TIBCBlob.SetForceEncodeToUtf8(const Value: boolean);
begin
  FParamsChanged := True;
  FForceEncodeToUtf8 := Value;
end;

function TIBCBlob.GetGDS: TGDS;
begin
  if FConnection <> nil then
    Result := FConnection.FGDS
  else
    Result := GDSList.GetGDS('');
end;
//******************************************************************************

function Reverse2(Value: word): Word;
begin
  Result := Swap(Value);
//  Result := Word((byte(Value) shl 8) or byte(Value shr 8));
end;

function Reverse4(Value: cardinal): cardinal;
begin
  Result := cardinal((byte(Value) shl 24) or (byte(Value shr 8) shl 16)
    or (byte(Value shr 16) shl 8) or byte(Value shr 24));
end;

function XSQLDA_LENGTH(n: Long; XSQLVARType: TXSQLVARType): Long;
begin
  if XSQLVARType = vtGDS7 then
    Result := SizeOfXSQLDA + ((n - 1)  * SizeOfXSQLVAR)
  else
    Result := SizeOfXSQLDA_V1 + ((n - 1) * SizeOfXSQLVAR_V1);
end;

{Date conversions}

const
  IBDateDelta = 15018;

procedure DateTimeToIBCTimeStamp(DateTime: TDateTime; const Buf: IntPtr);
var
  Date, Time: integer;
begin
  Date := Trunc(DateTime) + IBDateDelta;
  Time := Round(Abs(Frac(DateTime)) * (MSecsPerDay * 10));
  Marshal.WriteInt32(Buf, Date);
  Marshal.WriteInt32(Buf, SizeOf(integer), Time);
end;

procedure DateTimeToIBCDate(DateTime: TDateTime; const Buf: IntPtr);
var
  Date: integer;
begin
  Date := Trunc(DateTime) + IBDateDelta;
  Marshal.WriteInt32(Buf, Date);
end;

procedure DateTimeToIBCTime(DateTime: TDateTime; const Buf: IntPtr);
var
  Time: integer;
begin
  Time := Round(Abs(Frac(DateTime)) * (MSecsPerDay * 10));
  Marshal.WriteInt32(Buf, Time);
end;

procedure IBCTimeStampToDateTime(ib_date: IntPtr; const Buf: IntPtr);
var
  Date, Time, DateTime: TDateTime;
begin
  Date := Marshal.ReadInt32(ib_date) - IBDateDelta;
  Time := Marshal.ReadInt32(ib_date, SizeOf(integer)) / (MSecsPerDay * 10);
  if Date < 0 then
    DateTime := Date - Time
  else
    DateTime := Date + Time;
  Marshal.WriteInt64(Buf, BitConverter.DoubleToInt64Bits(DateTime));
end;

procedure IBCDateToDateTime(ib_date: IntPtr; const Buf: IntPtr);
var
  Date: TDateTime;
begin
  Date := Marshal.ReadInt32(ib_date) - IBDateDelta;
  Marshal.WriteInt64(Buf, BitConverter.DoubleToInt64Bits(Date));
end;

procedure IBCTimeToDateTime(ib_time: IntPtr; const Buf: IntPtr);
var
  Time: TDateTime;
begin
  Time := Marshal.ReadInt32(ib_time) / (MSecsPerDay * 10);
  Marshal.WriteInt64(Buf, BitConverter.DoubleToInt64Bits(Time));
end;

{SQL parsing}

{function IsStoredProcCall(SQL: string): boolean;
var
  Parser: TIBCParser;
  PrevCode, Code: integer;
  StLex: string;
begin
  Result := False;
  PrevCode := 0;
  Parser := TIBCParser.Create(SQL);
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  try
    repeat
      Code := Parser.GetNext(StLex);
      if (PrevCode = lxEXECUTE) and (Code = lxPROCEDURE) then
        Result := True;
      PrevCode := Code;
    until (Code = lcEND) or Result;
  finally
    Parser.Free;
  end;
end;}

{procedure SetStrTerminator(Buf: IntPtr; Size, CharLen: integer);
begin
  case CharLen of
    0, 1:
      Marshal.WriteByte(Buf, Size, 0);
    2:
      Marshal.WriteInt16(Buf, Size, 0);
    3: begin
      Marshal.WriteInt16(Buf, Size, 0);
      Marshal.WriteByte(Buf, Size + 2, 0);
    end;
  end;
end;}

function IBCDecimalToBcd(Value: int64; Scale: integer): TBcd;
var
  StrVal: string;
  StrLen: integer;
  Negative: boolean;
  bcdstr: string;
  i: integer;
begin
  if Value < 0 then begin
    Value := -Value;
    Negative := True;
  end
  else
    Negative := False;

  StrVal := IntToStr(Value);
  StrLen := Length(StrVal);
  if (StrLen <= Scale) and (Value <> 0) then begin // Value <> 0 - in some cases IB
    SetLength(bcdstr, Scale + 2);                  // returs Value: 0 Scale: 1
    for i := 1 to Scale - StrLen + 2 do
      bcdstr[i] := '0';
    bcdstr[2] := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
    Move(StrVal[1], bcdstr[Scale - StrLen + 3], StrLen * SizeOf(Char));
  end
  else
    bcdstr := StrVal;

  if Negative then
    bcdstr := '-' + bcdstr;

  Result := StrToBcd(bcdstr);

  if (StrLen > Scale) and (Scale > 0) then
    Result.SignSpecialPlaces := Result.SignSpecialPlaces + Scale;
end;

function SSLParamsToString(Enabled: boolean; const ParamValue: array of string): string;
var
  SSLParams: string;
  i: integer;

  procedure AppendSSLParam(const ParamName, ParamValue: string);
  begin
    if ParamValue <> '' then
      SSLParams := SSLParams + '?' + ParamName + '=' + ParamValue;
  end;

begin

  if Enabled = False then
    Result := ''
  else begin
    SSLParams := '';
    for i := 0 to High(SSLParamNames) - 1 do
      AppendSSLParam(SSLParamNames[i], ParamValue[i]);
    Result := Format('?ssl=true%s??', [SSLParams]);
  end;
end;

function GetFullDatabaseName(const Host: string; const Port: string; Protocol: _TIBCProtocol;
  const FileName: string; IsClientFB3: boolean; IPVersion: TIPVersion; const SSLOptions: string): string;
var
  _Host, _Port: string;
  PortPos: integer;
begin
  Result := '';

  if not GetIsURLConnectionString(FileName) then
    if (Protocol = _Local) and (Host <> '') then begin
      if IsClientFB3 then
      {$IFDEF MSWINDOWS}
        Result := 'xnet://';
      {$ENDIF}
      {$IFDEF POSIX}
        Result := 'inet://';
      {$ENDIF}
    end
    else if IPVersion <> ivIPBoth then begin
      case IPVersion of
        ivIPv4:
          Result := 'inet4://';
        ivIPv6:
          Result := 'inet6://';
      end;

      PortPos := GetPortPosFromHost(Host, '/', _Port);
      if PortPos > 0 then begin
        _Host := Copy(Host, 1, PortPos - 1);
        Result := Result + _Host + ':' + _Port + '/' + FileName;
        Exit;
      end;

      if Host <> '' then
        Result := Result + Host;
      if Port <> '' then
        Result := Result + ':' + Port;
      if (Host <> '')  or (Port <> '') then
        Result := Result + '/';
    end
    else if Host <> '' then
      case Protocol of
        _TCP: begin
          if GetPortFromHost(Host, _Port) then
            Result := Host + SSLOptions + ':'
          else begin
            _Port := Port;
            if (_Port = '') or (((_Port = DefaultUniDACSPort) or (_Port = DefaultIBDACSPort) or (LowerCase(_Port) = DefaultIBDACServiceName)) and not ForceUsingDefaultPort) then
              Result := Host + ':'
            else begin
              if Host[length(Host)] = '/' then
                Result := Host + _Port + SSLOptions + ':'
              else
                Result := Host + '/' + _Port + SSLOptions + ':';
            end;
          end;
        end;
        _NetBEUI: begin
          if GetPortFromHost(Host, _Port) then
            Result := '\\' + Host + '\'
          else begin
            _Port := Port;
            if (_Port = '') or (((_Port = DefaultUniDACSPort) or (_Port = DefaultIBDACSPort) or (LowerCase(_Port) = DefaultIBDACServiceName)) and not ForceUsingDefaultPort) then
              Result := '\\' + Host + '\'
            else begin
              if Host[length(Host)] = '@' then
                Result := '\\' + Host + _Port + '\'
              else
                Result := '\\' + Host + '@' + _Port + '\';
            end;
          end;
        end;
        _SPX:
          Result := Host + '@';
      end;

  Result := Result + FileName;
end;

function GetPortPosFromHost(const Host: string; const PortSymbol: string; var Port: string): integer;
begin
  Result := Pos(PortSymbol, Host);
  if (Result > 0) and (length(Host) > Result) then
    Port := Copy(Host, Result + 1, MaxInt)
  else
    Result := 0;
end;

function GetPortFromHost(const Host: string; var Port: string): boolean;
begin
  Result := True;

  if GetPortPosFromHost(Host, '/', Port) > 0 then
    Exit;

  if GetPortPosFromHost(Host, '@', Port) > 0 then
    Exit;

  Result := False;
end;

function GetDBNamePos(const Database: string): integer;
var
  i: integer;
  InBrackets: boolean;
begin
  Result := Pos('\\', Database);
  if Result = 1 then begin
    Result := Pos('\', Copy(Database, 3, MaxInt)) + 3;
    Exit;
  end;
  Result := Pos('@', Database);
  if Result > 0 then begin
    Inc(Result);
    Exit;
  end;

  InBrackets := False;
  for i := 1 to Length(DataBase) do begin //IPv6
    case Database[i] of
      '[': InBrackets := True;
      ']': InBrackets := False;
      ':': if not InBrackets then begin
        Result := i;
        Break;
      end;
    end;
  end;
  if (Result > 0) and (Length(Database) > (Result + 1)) and (Database[Result + 1] <> '\') then
    Inc(Result)
  else
    Result := -1;
end;

procedure ParseDatabaseName(const Database: string; var Host: string; var Port: string; var Protocol: _TIBCProtocol;
  var FileName: string; ParsePort: boolean = True; NamePos: integer = -1);
var
  PortPos: integer;
begin
  if NamePos > 0 then begin
    case Database[NamePos - 1] of
      '@': begin
        Protocol := _SPX;
        Host := Copy(Database, 1, NamePos - 2);
        Port := '';
      end;
      ':': begin
        Protocol := _TCP;
        Host := Copy(Database, 1, NamePos - 2);
        if ParsePort then begin
          PortPos := GetPortPosFromHost(Host, '/', Port);
          if PortPos > 0 then
            Host:= Copy(Host, 1, PortPos - 1)
          else
            Port := DefaultIBDACSPort;
        end;
      end;
      '\': begin
        Protocol := _NetBEUI;
        Host := Copy(Database, 3, NamePos - 4);
        if ParsePort then begin
          PortPos := GetPortPosFromHost(Host, '@', Port);
          if PortPos > 0 then
            Host:= Copy(Host, 1, PortPos - 1)
          else
            Port := DefaultIBDACServiceName;
        end;
      end;
    end;
    FileName := Copy(Database, NamePos, MaxInt);
  end
  else begin
    Host := '';
    Protocol := _TCP;
    Port := DefaultIBDACSPort;
    FileName := Database;
  end;
end;

function GetIsURLConnectionString(const Database: string): boolean;
begin
  Result :=  Pos('://', Database) > 0;
end;

initialization

  GlobalCS := TCriticalSection.Create;
  EventsCS := TCriticalSection.Create;
  IBCSQLInfo := TIBCSQLInfo.Create(nil);

{$IFNDEF LITE}
  ISCCallbackPtr := @EventCallback;
{$ENDIF}

finalization
  IBCSQLInfo.Free;

  GLobalCS.Free;
  EventsCS.Free;

end.

