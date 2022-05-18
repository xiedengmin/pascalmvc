
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I UniDac.inc}
unit Uni;

interface

uses
  Classes, SysUtils, Variants, DB, StrUtils,
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  CLRClasses,
  CRTypes, CRParser, CRVio, CRAccess, CRConnectionPool, CRConnectionString,
{$IFNDEF STD}
  CREncryption,
{$ENDIF}
{$IFDEF VER17P}
  Generics.Collections,
{$ENDIF}
  MemData, {$IFNDEF FPC}MemDS,{$ELSE}MemDataSet,{$ENDIF}
  DBAccess, UniConsts, UniProvider, UniConnectionString;

{$I UniDacVer.inc}


type
  TUniConnection = class;
  TUniParam = class;
  TUniParams = class;
  TUniSQL = class;
  TUniTransaction = class;
  TCustomUniDataSet = class;
  TUniUpdateSQL = class;
  TUniCursor = class;
{$IFNDEF STD}
  TUniEncryptor = class;
{$ENDIF}

  TGetUniProviderFunc = function: TUniProvider of object;

  EUniError = class(EDAError)
  private
    FInnerError: EDAError;
  public
    constructor Create(AInnerError: EDAError);
    destructor Destroy; override;

    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;

    property InnerError: EDAError read FInnerError;
  end;

  TSpecificOptionsList = class(TStringList)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TComponent;
    FGetProviderFunc: TGetUniProviderFunc;

    function GetProvider: TUniProvider;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);

    function ValidateOption(const FullName, Value: string): string; overload;
    function ValidateOption(const Prefix, Name, Value: string): string; overload;
    function ValidateOption(const FullName: string): string; overload;
    function ResolveDeprecated(const FullName: string): string; overload;
    function ResolveDeprecated(const Prefix, Name: string): string; overload;
    procedure SplitOptionName(const FullName: string; var Prefix, Name: string);
  protected
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;

    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
  public
    constructor Create(AOwner: TComponent; GetUniProviderFunc: TGetUniProviderFunc);

    function IndexOfName(const Name: string): Integer; override;

    property Values[const Name: string]: string read GetValue write SetValue;
  end;

  TSpecificOptionsHolder = class
  private
    FValues: TSpecificOptionsList;
    FIsModified: boolean;
  protected
    procedure ValuesChanging(Sender: TObject); virtual;
    procedure ValuesChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent; GetUniProviderFunc: TGetUniProviderFunc);
    destructor Destroy; override;

    property Values: TSpecificOptionsList read FValues;
    property IsModified: boolean read FIsModified write FIsModified;
  end;

  TUniConnectionOptions = class(TDAConnectionOptions)
  private
    function GetConvertEOL: boolean;
    procedure SetConvertEOL(Value: boolean);

  published
    property ConvertEOL: boolean read GetConvertEOL write SetConvertEOL default False;

    property DisconnectedMode;
    property KeepDesignConnected;
    property LocalFailover;
    property DefaultSortType;
    property EnableBCD;
    property EnableFMTBCD;
  end;

  TUniConnectionSpecificOptions = class(TSpecificOptionsHolder)
  protected
    procedure ValuesChanging(Sender: TObject); override;
  end;

  TUniMacro = class(TCollectionItem)
  private
    FName: string;
    FValue: string;
    FCondition: string;

    procedure SetName(const Value: string);
    procedure SetValue(const Value: string);
    procedure SetCondition(const Value: string);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    //function GetDisplayName: string; override;

  public

  published
    property Name: string read FName write SetName;
    property Value: string read FValue write SetValue;
    property Condition: string read FCondition write SetCondition;
  end;

  TUniMacros = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TUniMacro;
    procedure SetItem(Index: integer; Value: TUniMacro);
    procedure NotifyOwner;

  protected
    procedure Update(Item: TCollectionItem); override;

  public
    constructor Create(Owner: TPersistent);
    procedure Add(const Name, Value: string; const Condition: string = '');
    function FindMacro(const Name: string): TUniMacro;
    function MacroByName(const Name: string): TUniMacro;

    property Items[Index: integer]: TUniMacro read GetItem write SetItem; default;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniConnection = class(TCustomDAConnection)
  private
    FInternalTransaction: TDATransaction;
    FProviderName: string;
    FProvider: TUniProvider;
    FPort: integer;
    FDatabase: string;
    FSpecificOptions: TUniConnectionSpecificOptions;
    FMacros: TUniMacros;
    FSQLFormatter: TUniSqlFormatter;
    FMacroNames: TStringList;
    FMacroValues: TStringList;
    FMacrosChanged: boolean;
    FMacrosVersion: integer;

    function GetProviderName: string;
    procedure SetProviderName(Value: string);

  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(Value: integer);
    procedure SetDatabase(const Value: string);
    function GetSpecificOptions: TSpecificOptionsList;
    procedure SetSpecificOptions(Value: TSpecificOptionsList);
    function GetTransaction(Index: Integer): TUniTransaction;
    function GetOptions: TUniConnectionOptions;
    procedure SetOptions(Value: TUniConnectionOptions);
    procedure SetMacros(Value: TUniMacros);
    function IsMacrosStored: boolean;
    function GetDefaultTransaction: TUniTransaction;
    procedure SetDefaultTransaction(const Value: TUniTransaction);
    function GetSQL: TUniSQL;

  protected
    procedure CheckProvider; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function CanGetProvider: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetProvider: TUniProvider;

    function GetIConnectionClass: TCRConnectionClass; override;
    function GetICommandClass: TCRCommandClass; override;
    function GetIRecordSetClass: TCRRecordSetClass; override;
    function GetIMetaDataClass: TCRMetaDataClass; override;
    function GetITransactionClass: TCRTransactionClass; override;

    procedure SetConnectionParameters(ConnectionParameters: TCRConnectionParameters); override;
    procedure SetBaseConnectionProps(Connection: TCRConnection); override;
    procedure SetConnectionProps(Connection: TCRConnection); override;
    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    procedure CreateIConnection; override;
    procedure SetIConnection(Value: TCRConnection); override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;

    procedure AssignTo(Dest: TPersistent); override;

    function SQLMonitorClass: TClass; override;
    function ConnectDialogClass: TConnectDialogClass; override;
    function CreateOptions: TDAConnectionOptions; override;
    function CreateConnectionStringBuilder: TCRConnectionStringBuilder; override;
    function IsMultipleTransactionsSupported: boolean; override;
    function UsedTransaction: TDATransaction; override;
    function GetInTransaction: boolean; override;
    function GetInternalDefTransaction: TUniTransaction;

    procedure DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean; ReconnectAttempt: integer;
      var ConnLostCause: TConnLostCause); override;
    procedure AssignConnectOptions(Source: TCustomDAConnection); override;
    procedure CheckSqlFormatter;
    procedure DetectActiveMacros;
    procedure ExpandMacros(var SQL: string);

    function DefaultTableSchema: string; override;

    function GetServerVersion: string;
    function GetServerVersionFull: string;
    function GetClientVersion: string;

    function GetConnectionStringParam(ParamCode: integer): variant; override;
    procedure SetConnectionStringParam(ParamCode: integer; const ParamValue: variant); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ExecProc(const Name: string; const Params: array of variant): variant; override;
    function ExecProcEx(const Name: string; const Params: array of variant): variant; override;

    function CreateDataSet(AOwner: TComponent = nil): TCustomDADataSet; override;
    function CreateSQL: TCustomDASQL; override;
    function CreateTransaction: TDATransaction; override;
    function CreateMetaData: TDAMetaData; override;

    procedure AssignConnect(Source: TUniConnection);
    function ParamByName(const Name: string): TUniParam;
    function MacroByName(const Name: string): TUniMacro;
    function ActiveMacroValueByName(const Name: string): Variant;

  {$IFNDEF STD}
    procedure EncryptTable(const TableName: string; Encryptor: TUniEncryptor; const Fields: string); reintroduce;
  {$ENDIF}

    procedure StartTransaction; overload; override;
    procedure StartTransaction(IsolationLevel: TCRIsolationLevel; ReadOnly: boolean = False); reintroduce; overload;
    procedure CommitRetaining;
    procedure RollbackRetaining;
    procedure Savepoint(const Name: string);
    procedure RollbackToSavepoint(const Name: string);
    procedure ReleaseSavepoint(const Name: string);

    property TransactionCount;
    property Transactions[Index: integer]: TUniTransaction read GetTransaction;
    property ServerVersion: string read GetServerVersion;
    property ServerVersionFull: string read GetServerVersionFull;
    property ClientVersion: string read GetClientVersion;

    property SQL: TUniSQL read GetSQL;

  published
    property AutoCommit;
    property DataTypeMap;
    property ProviderName: string read GetProviderName write SetProviderName;
    property Port: integer read FPort write SetPort default DefValPort;
    property Database: string read FDatabase write SetDatabase;
    property SpecificOptions: TSpecificOptionsList read GetSpecificOptions write SetSpecificOptions;
    property Options: TUniConnectionOptions read GetOptions write SetOptions;
    property Macros: TUniMacros read FMacros write SetMacros stored IsMacrosStored;

    property DefaultTransaction: TUniTransaction read GetDefaultTransaction write SetDefaultTransaction;

    property IOHandler;
    property PoolingOptions;
    property Pooling;
    property Debug;
    property Username;
    property Password;
    property Server;
    property Connected stored IsConnectedStored;
    property ConnectDialog;
    property LoginPrompt;
    property ConnectString;

    property AfterConnect;
    property BeforeConnect;
    property AfterDisconnect;
    property BeforeDisconnect;
    property OnLogin;
    property OnError;
    property OnConnectionLost;
  end;

  TUniTransactionSpecificOptions = class(TSpecificOptionsHolder)
  protected
    procedure ValuesChanged(Sender: TObject); override;
    procedure ValuesChanging(Sender: TObject); override;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniTransaction = class(TDATransaction)
  private
    FSpecificOptions: TUniTransactionSpecificOptions;

    function GetDefaultConnection: TUniConnection;
    procedure SetDefaultConnection(Value: TUniConnection);
    function GetConnection(Index: integer): TUniConnection;
    function GetSpecificOptions: TSpecificOptionsList;
    procedure SetSpecificOptions(const Value: TSpecificOptionsList);

  protected
    procedure AssignTo(Dest: TPersistent); override;

    function GetProvider: TUniProvider; {$IFDEF USE_INLINE}inline;{$ENDIF}

    function GetITransactionClass: TCRTransactionClass; override;
    procedure SetITransaction(Value: TCRTransaction); override;
    function SQLMonitorClass: TClass; override;
    procedure CheckActive; override;
    procedure CheckInactive; override;

    procedure DoSavepoint(const Name: string); override;
    procedure DoReleaseSavepoint(const Name: string); override;
    procedure DoRollbackToSavepoint(const Name: string); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure CommitRetaining;
    procedure RollbackRetaining;

    procedure Savepoint(Name: string);
    procedure ReleaseSavePoint(Name: string);
    procedure RollbackToSavepoint(Name: string);

    procedure AddConnection(Connection: TUniConnection);
    procedure RemoveConnection(Connection: TUniConnection);

    property Connections[Index: integer]: TUniConnection read GetConnection;
    property ConnectionsCount;

  published
    property DefaultConnection: TUniConnection read GetDefaultConnection write SetDefaultConnection stored IsInternalTrStored;
    property SpecificOptions: TSpecificOptionsList read GetSpecificOptions write SetSpecificOptions;

    property TransactionType;
    property IsolationLevel;
    property ReadOnly;
    property DefaultCloseAction;

    property OnError;
    property OnStart;
    property OnCommit;
    property OnRollback;
    property OnCommitRetaining;
    property OnRollbackRetaining;
  end;

{$IFNDEF STD}
{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniEncryptor = class(TCREncryptor)
  end;
{$ENDIF}

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniSQL = class(TCustomDASQL)
  private
    FSpecificOptions: TSpecificOptionsHolder;
    FFixedUsedTransaction: TDATransaction;
    FWriteAllParams: boolean;
    FParamRefs: TList;
    FLockParamRefsReset: boolean;
    FMacrosVersion: integer;
    FEnableUniSQL: boolean;

    procedure ResetParamRefs;

    function GetParams: TUniParams;
    procedure SetParams(Value: TUniParams);
    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetSpecificOptions: TSpecificOptionsList;
    procedure SetSpecificOptions(Value: TSpecificOptionsList);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);

  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure SetICommand(Value: TCRCommand); override;
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; override;

    function FindProvider: TUniProvider; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetProvider: TUniProvider; {$IFDEF USE_INLINE}inline;{$ENDIF}

    function CreateParamsObject: TDAParams; override;

    function UsedTransaction: TDATransaction; override;

    procedure BeginConnection(NoConnectCheck: boolean = True); override;
    procedure EndConnection; override;
    function GetTransaction: TDATransaction; override;

    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalExecute(Iters: integer; Offset: integer); override;
    function GetFinalSQL: string; override;
    procedure CheckUniMacros;

    function NeedRecreateProcCall: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function ParseSQL(const SQL: string; Params: TDAParams): string; override;
    procedure AssembleSQL; override;
    procedure CreateParams; override;
    procedure WriteParams(WriteValue: boolean = True); override;
    procedure ReadParams; override;
    function FindResultParam: TDAParam; override;
    procedure InternalCreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean = False); override;
    procedure AssignParamDesc(Param: TDAParam; ParamDesc: TParamDesc); override;
    procedure AssignParamValue(ParamDesc: TParamDesc; Param: TDAParam); override;

    // CLR cross-assembly
//    function UsedConnection: TCustomDAConnection; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindParam(const Value: string): TUniParam;
    function ParamByName(const Value: string): TUniParam;

    procedure CreateProcCall(const Name: string);

    property LastInsertId: int64 read FLastInsertId;

  published
    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
    property Params: TUniParams read GetParams write SetParams stored False;
    property SpecificOptions: TSpecificOptionsList read GetSpecificOptions write SetSpecificOptions;

    property ParamCheck; // before SQL
    property SQL;
    property Macros;
    property Debug;

    property BeforeExecute;
    property AfterExecute;
  end;

{$IFNDEF STD}

  TUniEncryption = class(TDAEncryption)
  private
    function GetEncryptor: TUniEncryptor;
    procedure SetEncryptor(Value: TUniEncryptor);
  published
    property Encryptor: TUniEncryptor read GetEncryptor write SetEncryptor;
  end;

{$ENDIF}

  TUniDataSetOptions = class(TDADataSetOptions)
  public
    constructor Create(Owner: TCustomDADataSet);

  published
    property FullRefresh;
    property TrimFixedChar;
    property TrimVarChar;
    property SetEmptyStrToNull;
    property PrepareUpdateSQL;
    property SetFieldsReadOnly default True;
    property RequiredFields;
    property StrictUpdate;
    property NumberRange;
    property QueryRecCount;
    property AutoPrepare;
    property ReturnParams;
    property LongStrings;
    property FlatBuffers;
    property RemoveOnRefresh;
    property QuoteNames;
    property DetailDelay;
  {$IFDEF HAVE_COMPRESS}
    property CompressBlobMode;
  {$ENDIF}
    property LocalMasterDetail;
    property CacheCalcFields;
    property FieldOrigins;
    property DefaultValues;
    property UpdateBatchSize;
    property UpdateAllFields;
    property EnableBCD;
    property EnableFMTBCD;
    property MasterFieldsNullable;
    property InsertAllSetFields;
  end;

  TUniDataSetSpecificOptions = class(TSpecificOptionsHolder)
  protected
    procedure ValuesChanging(Sender: TObject); override;
  end;

  TCustomUniDataSet = class(TCustomDADataSet)
  private
    FSpecificOptions: TUniDataSetSpecificOptions;
    FFixedUsedTransaction: TUniTransaction;
    FCursor: TCRCursor;
    FLockFetchAll: boolean;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
  {$IFNDEF STD}
    function GetEncryption: TUniEncryption;
    procedure SetEncryption(Value: TUniEncryption);
  {$ENDIF}
    function GetOptions: TUniDataSetOptions;
    procedure SetOptions(Value: TUniDataSetOptions);
    function GetSpecificOptions: TSpecificOptionsList;
    procedure SetSpecificOptions(Value: TSpecificOptionsList);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);
    function GetUpdateTransaction: TUniTransaction;
    procedure SetUpdateTransaction(Value: TUniTransaction);
    function GetParams: TUniParams;
    procedure SetParams(Value: TUniParams);
    function GetUpdateObject: TUniUpdateSQL;
    procedure SetUpdateObject(Value: TUniUpdateSQL);

  protected
    procedure AssignTo(Dest: TPersistent); override;

    function GetCRCursor: TCRCursor; override;
    procedure SetCRCursor(Value: TCRCursor); override;

    function FindProvider: TUniProvider; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetProvider: TUniProvider; {$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure CheckInactive; override;
    procedure SetIRecordSet(Value: TData); override;

    procedure CreateCommand; override;

    function GetDataSetServiceClass: TDataSetServiceClass; override;
    procedure SetDataSetService(Value: TDataSetService); override;

    function CreateOptions: TDADataSetOptions; override;
  {$IFNDEF STD}
    function CreateEncryption: TDAEncryption; override;
  {$ENDIF}

  { Open/Close }
    procedure InternalExecute(Iters: integer; Offset: integer); override;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure CloseCursor; override;

  { Fields }
    function NeedComplexUpdateFieldDefList: boolean; override;

  { Transactions }
    function GetTransaction: TDATransaction; override;
    procedure SetTransaction(Value: TDATransaction); override;
    function UsedTransaction: TDATransaction; override;
    procedure BeginConnection(NoConnectCheck: boolean = True); override;
    procedure EndConnection; override;
//    function UsedConnection: TCustomDAConnection; override;
  {$IFNDEF FPC}
    function GetPSTransaction: TDATransaction;
    function PSInTransaction: Boolean; override;
    procedure PSStartTransaction; override;
  {$ENDIF}

  { Field Management }
    function GetFieldTypeMapClass: TFieldTypeMapClass; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; overload; override;
  {$IFDEF VER12P}
    function GetFieldClass(FieldDef: TFieldDef): TFieldClass; overload; override;
  {$ENDIF}

  { SQL Modifications }
    function GetParserClass: TSQLParserClass; override;
    function SQLAddWhere(const SQLText, Condition: string): string; override;
    function SQLSetOrderBy(const SQLText, Fields: string): string; override;

    procedure SetLockFetchAll(Value: boolean);
    function GetFetchAll: boolean; override;
    procedure SetFetchAll(Value: boolean); override;
    function GetNonBlocking: boolean; override;
    procedure QuickOpen(var Info: TQuickOpenInfo; Refresh: boolean = False); override;
    procedure Restore(const Info: TQuickOpenInfo; RestoreActive: boolean = True); override;

    property LockFetchAll: boolean read FLockFetchAll write SetLockFetchAll;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindParam(const Value: string): TUniParam;
    function ParamByName(const Value: string): TUniParam;

  {$HPPEMIT '#ifdef GetObject'}
  {$HPPEMIT '#undef GetObject'}
  {$HPPEMIT '#endif'}
    function GetObject(const FieldName: string): TDBObject;
    function GetArray(const FieldName: string): TDBObject;

    procedure Prepare; override;
    procedure UnPrepare; override;
    procedure CreateProcCall(const Name: string);
    function OpenNext: boolean;
    procedure RefreshQuick(const CheckDeleted: boolean);

  {$IFNDEF STD}
    property Encryption: TUniEncryption read GetEncryption write SetEncryption;
    property SmartFetch;
  {$ENDIF}

    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
    property UpdateTransaction: TUniTransaction read GetUpdateTransaction write SetUpdateTransaction;
    property Params: TUniParams read GetParams write SetParams stored False;
    property Options: TUniDataSetOptions read GetOptions write SetOptions;
    property SpecificOptions: TSpecificOptionsList read GetSpecificOptions write SetSpecificOptions;
    property UpdateObject: TUniUpdateSQL read GetUpdateObject write SetUpdateObject;
    property LastInsertId: int64 read FLastInsertId;

    property Cursor;
    property DMLRefresh;
    property KeyFields;
    property LockMode;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniQuery = class(TCustomUniDataSet)
  published
    property UpdatingTable;
    property KeyFields;
    property SQLInsert;
    property SQLDelete;
    property SQLUpdate;
    property SQLLock;
    property SQLRefresh;
    property SQLRecCount;
    property LocalUpdate;

    property DataTypeMap;
  {$IFNDEF STD}
    property Encryption;
    property SmartFetch;
  {$ENDIF}
    property Connection;
    property Transaction;
    property UpdateTransaction;
    property ParamCheck; // before SQL
    property SQL;
    property MasterSource;
    property MasterFields;
    property DetailFields;
    property Debug;
    property Macros;
    property Params;
    property FetchRows;
    property ReadOnly;
    property UniDirectional;
    property CachedUpdates;
    property FilterSQL;
    property DMLRefresh;
    property LockMode;
    property RefreshOptions;
    property Options;
    property SpecificOptions;

    property BeforeExecute;
    property AfterExecute;
    property BeforeUpdateExecute;
    property AfterUpdateExecute;
    property OnUpdateError;
    property OnUpdateRecord;
    property BeforeFetch;
    property AfterFetch;

    property UpdateObject;

    property Active;
    property AutoCalcFields;
    property Constraints stored IsConstraintsStored;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
  {$IFNDEF FPC}
    property ObjectView default False;
  {$ENDIF}

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AfterRefresh;
    property BeforeRefresh;
    property CheckMode;

    property Fields;
  end;

  TCustomUniTable = class(TCustomUniDataSet)
  private
    FTableName: string;
    FOrderFields: string;
    FSQLIsPrepared: boolean;

    procedure SetTableName(const Value: string);
    procedure SetOrderFields(const Value: string);

  {$IFNDEF FPC}
  protected
  { IProviderSupport }
    function PSGetTableName: string; override;
    procedure PSSetParams(AParams: DB.TParams); override;
    procedure PSSetCommandText(const CommandText: string); override;
  {$ENDIF}

  protected
    procedure SetDataSetService(Value: TDataSetService); override;
  { Open/Close }
    procedure OpenCursor(InfoQuery: boolean); override;

    procedure AssignTo(Dest: TPersistent); override;

  { SQL Modifications }
    procedure CheckSQL; override;
    procedure SetFilterSQL(const Value: string); override;
    function GetFinalSQL: string; override;

  public
    constructor Create(Owner: TComponent); override;

  { Open/Close }
    procedure PrepareSQL;
    procedure Prepare; override;
    procedure Execute; override;

    procedure EmptyTable;

    property TableName: string read FTableName write SetTableName;
    property OrderFields: string read FOrderFields write SetOrderFields;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniTable = class (TCustomUniTable)
  published
    property TableName;
    property OrderFields;

    property DataTypeMap;
  {$IFNDEF STD}
    property Encryption;
    property SmartFetch;
  {$ENDIF}
    property Connection;
    property Transaction;
    property UpdateTransaction;
    property MasterSource;
    property MasterFields;
    property DetailFields;
    property ReadOnly;
    property KeyFields;
    property FilterSQL;
    property DMLRefresh;
    property Debug;
    property Params;
    property FetchRows;
    property UniDirectional;
    property CachedUpdates;
    property LockMode default lmOptimistic;
    property RefreshOptions;

    property OnUpdateError;
    property OnUpdateRecord;

    property UpdateObject;

    property Active;
    property AutoCalcFields;
    property Constraints stored IsConstraintsStored;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
  {$IFNDEF FPC}
    property ObjectView default False;
  {$ENDIF}

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AfterRefresh;
    property BeforeRefresh;
    property CheckMode;

    property Fields;
    property Options;
    property SpecificOptions;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniStoredProc = class (TCustomUniDataSet)
  private
    FStoredProcName: string;
    FIsQuery: boolean;

    procedure SetStoredProcName(const Value: string);

  {$IFNDEF FPC}
  protected
  { IProviderSupport }
    procedure PSSetCommandText(const CommandText: string); override;
  {$ENDIF}

  protected
    procedure CreateCommand; override;
    procedure BeforeOpenCursor(InfoQuery: boolean); override;
    procedure DoBeforeExecute; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Prepare; override;
    procedure PrepareSQL(IsQuery: boolean = False);

    procedure ExecProc; // for BDE compatibility

  published
    property StoredProcName: string read FStoredProcName write SetStoredProcName;

    property SQL;
    property SQLInsert;
    property SQLDelete;
    property SQLUpdate;
    property SQLLock;
    property SQLRefresh;
    property SQLRecCount;

    property DataTypeMap;
  {$IFNDEF STD}
    property Encryption;
    property SmartFetch;
  {$ENDIF}
    property Connection;
    property Transaction;
    property UpdateTransaction;
    property Debug;
    property Params;
    property FetchRows;
    property ReadOnly;
    property UniDirectional;
    property CachedUpdates;
    property LockMode;
    property RefreshOptions;
    property Options;
    property SpecificOptions;

    property BeforeExecute;
    property AfterExecute;
    property BeforeUpdateExecute;
    property AfterUpdateExecute;
    property OnUpdateError;
    property OnUpdateRecord;

    property UpdateObject;

    property Active;
    property AutoCalcFields;
    property Constraints stored IsConstraintsStored;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
  {$IFNDEF FPC}
    property ObjectView default False;
  {$ENDIF}
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AfterRefresh;
    property BeforeRefresh;

    property Fields;
  end;

{ TUniUpdateSQL }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniUpdateSQL = class (TCustomDAUpdateSQL)
  protected
    function DataSetClass: TCustomDADataSetClass; override;
    function SQLClass: TCustomDASQLClass; override;
  end;

{ TUniDataSource }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniDataSource = class (TCRDataSource)
  end;

{ TUniMetaData }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniMetaData = class (TDAMetaData)
  private
    FFixedUsedTransaction: TDATransaction;
    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);

  protected
    procedure CloseCursor; override;
    function UsedTransaction: TDATransaction; override;
    procedure SetTransaction(Value: TDATransaction); override;
    procedure BeginConnection; override;
    procedure EndConnection; override;

  published
    property Active;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeScroll;
    property AfterScroll;
    property OnFilterRecord;

    property MetaDataKind;
    property Restrictions;

    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
  end;

{ TUniBlob }

  TUniBlob = class(TCompressedBlob)
  private
    FNativeBlob: TBlob;

    function GetNativeBlob: TBlob;
    procedure SetNativeBlob(Value: TBlob);
  public
    destructor Destroy; override;

    procedure Disconnect; override;
  end;

{ TUniCursor }

  TUniCursor = class(TCRCursor)
  private
    FNativeCursor: TCRCursor;

  protected

    function GetNativeCursor: TCRCursor;
    procedure SetNativeCursor(Value: TCRCursor);
  public
    destructor Destroy; override;

    procedure Disconnect; override;
    function CanFetch: boolean; override;
  end;

{ TUniParam }

  TUniParam = class(TDAParam)
  private
    function GetAsObject: TDBObject;
    procedure SetAsObject(Value: TDBObject);
    function GetAsArray: TDBObject;
    procedure SetAsArray(Value: TDBObject);
  protected
    FNativeDataArr: TVariantArray;

    function CreateObject: TSharedObject; override;
    function GetNativeParamObject(SourceObject: TSharedObject): TSharedObject; override;

    function IsBlobDataType(DataType: TFieldType): boolean; override;
    function IsSharedObjectDataType(DataType: TFieldType): boolean; override;
    function IsObjectDataType(DataType: TFieldType): boolean;
    function IsObject: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsArrayDataType(DataType: TFieldType): boolean;
    function IsArray: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}

    function GetIsNull: boolean; override;
  public
    function GetConnection: TUniConnection;
    function GetNativeParamObject: TSharedObject; override;

    property AsCursor;
    property AsObject: TDBObject read GetAsObject write SetAsObject;
    property AsArray: TDBObject read GetAsArray write SetAsArray;
  published
    property National default False;
  end;

{ TUniParams }

  TUniParams = class(TDAParams)
  private
    function GetItem(Index: integer): TUniParam;
    procedure SetItem(Index: integer; Value: TUniParam);

    function GetOwner: TPersistent; reintroduce;// CLR cross-assembly
    procedure SetOwner(Owner: TPersistent);
  protected
    procedure Update(Item: TCollectionItem); override;

    property ParamsChangeType: TParamsChangeType read FParamsChangeType write FParamsChangeType; // required for CLR
  public
    constructor Create(Owner: TPersistent);

    function FindParam(const Value: string): TUniParam;
    function ParamByName(const Value: string): TUniParam;

    property Items[Index: integer]: TUniParam read GetItem write SetItem; default;
  end;

  TUniUtils = class
  public
    class function CanGetProvider(Connection: TUniConnection): boolean;
    class function GetProvider(Connection: TUniConnection): TUniProvider;
    class function GetCRConnection(Connection: TUniConnection): TCRConnection;
  end;

  procedure GetServerList(const ProviderName: string; List: TStrings; SpecificOptions: TStrings = nil);

var
  DefConnectDialogClassProc: function: TClass = nil;

var
  EnableUniSQL: boolean = True;
  OldTransactionBehaviour: boolean = False;

const
  DACProductName = 'UniDAC';



implementation

uses
  CRProps, CRFunctions, CRServerEnumerator, MemUtils,
  DAConsts, UniSQLMonitor;


procedure HandleInternalError;
var
  E: TObject;
begin
  E := AcquireExceptionObject;

  if E is EDAError then
    raise EUniError.Create(EDAError(E))
  else
    raise E;
end;

{ EUniError }

constructor EUniError.Create(AInnerError: EDAError);
begin
  inherited
  Message := AInnerError.Message;
  FErrorCode := AInnerError.ErrorCode;
  FComponent := AInnerError.Component;
  FInnerError := AInnerError;
end;

destructor EUniError.Destroy;
begin
  FInnerError.Free;

  inherited;
end;

function EUniError.IsFatalError: boolean;
begin
  if FInnerError <> nil then
    Result := FInnerError.IsFatalError
  else
    Result := False;
end;

function EUniError.IsKeyViolation: boolean;
begin
  if FInnerError <> nil then
    Result := FInnerError.IsKeyViolation
  else
    Result := False;
end;

{  TSpecificOptionsList }

constructor TSpecificOptionsList.Create(AOwner: TComponent; GetUniProviderFunc: TGetUniProviderFunc);
begin
  inherited Create;

  FOwner := AOwner;
  FGetProviderFunc := GetUniProviderFunc;
end;

function TSpecificOptionsList.GetProvider: TUniProvider;
begin
  Result := nil;

  if Assigned(FGetProviderFunc) then
    Result := FGetProviderFunc;

  if Result = nil then
    DatabaseError(SProviderNotDefined);
end;

function TSpecificOptionsList.GetValue(const Name: string): string;
var
  I: Integer;
  P: Integer;
  S: string;
begin
  Result := '';
  I := IndexOfName(Name);
  if I < 0 then
    ValidateOption(Name)
  else begin
    S := Get(I);
    P := Pos({$IFDEF VER7P}NameValueSeparator{$ELSE}'='{$ENDIF}, S);
    if P > 0 then
      Result := Copy(S, P + 1, MaxInt)
  end;
end;

procedure TSpecificOptionsList.SetValue(const Name, Value: string);
var
  ValidName: string;
begin
  ValidName := ValidateOption(Name, Value);

  inherited Values[ValidName] := Value;
end;

function TSpecificOptionsList.ValidateOption(const FullName, Value: string): string;
var
  Prefix, Name: string;
begin
  SplitOptionName(FullName, Prefix, Name);

  Name := ValidateOption(Prefix, Name, Value);

  if Prefix <> '' then
    Result := Prefix + '.' + Name
  else
    Result := Name;
end;

function TSpecificOptionsList.ValidateOption(const Prefix, Name, Value: string): string;
var
  Provider: TUniProvider;
begin
  if Prefix <> ''  then begin
    Provider := UniProviders.GetProvider(Prefix);
    if Provider = nil then
      CheckProviderName(Prefix);
  end
  else
    Provider := GetProvider;

  Result := Provider.ValidateOption(FOwner, Provider.GetProviderName, Name, Value);
end;

function TSpecificOptionsList.ValidateOption(const FullName: string): string;
var
  Prefix, Name: string;
  Provider: TUniProvider;
begin
  SplitOptionName(FullName, Prefix, Name);

  if Prefix <> ''  then begin
    Provider := UniProviders.GetProvider(Prefix);
    if Provider = nil then
      CheckProviderName(Prefix);
  end
  else
    Provider := GetProvider;

  Result := Provider.ValidateOption(FOwner, Provider.GetProviderName, Name);
end;

function TSpecificOptionsList.ResolveDeprecated(const FullName: string): string;
var
  Prefix, Name: string;
begin
  SplitOptionName(FullName, Prefix, Name);

  Name := ResolveDeprecated(Prefix, Name);

  if Prefix <> '' then
    Result := Prefix + '.' + Name
  else
    Result := Name;
end;

function TSpecificOptionsList.ResolveDeprecated(const Prefix, Name: string): string;
var
  Provider: TUniProvider;
  OptionsList: TOptionsList;
  Option: Toption;
begin
  if Prefix <> ''  then begin
    Provider := UniProviders.GetProvider(Prefix);
    if Provider = nil then
      CheckProviderName(Prefix);
  end
  else
    Provider := GetProvider;

  OptionsList := Provider.GetComponentOptionsList(FOwner);
  if OptionsList <> nil then begin
    Option := OptionsList.OptionByDeprecatedName(Name);
    if Option <> nil then
      Result := Option.OptionName
    else
      Result := '';
  end
  else
    Result := '';
end;

procedure TSpecificOptionsList.SplitOptionName(const FullName: string; var Prefix, Name: string);
var
  i: integer;
begin
  i := Pos('.', FullName);
  if i = 0 then begin
    Prefix := '';
    Name := FullName;
  end
  else begin
    Prefix := copy(FullName, 1, i - 1);
    Name := copy(FullName, i + 1, Length(FullName) - i);
  end;
end;

procedure TSpecificOptionsList.ReadData(Reader: TReader);
var
  str: string;
  Prefix, Name, Value: string;
  IsConnectionOptions: boolean;
begin
  IsConnectionOptions := (FOwner <> nil) and (FOwner is TUniConnection);

  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do begin
      str := Reader.ReadString;
      if IsConnectionOptions then begin
        ExtractOption(str, Prefix, Name, Value);
        if Name = 'EncryptedKey' then begin
          Value := TUniConnection(FOwner).DecryptFromHex(Value);
          if Prefix <> '' then
            Add(Prefix + '.EncryptionKey=' + Value)
          else
            Add('EncryptionKey=' + Value)
        end
        else
          Add(str)
      end
      else
        Add(str);
    end;
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TSpecificOptionsList.WriteData(Writer: TWriter);
var
  i: Integer;
  str: string;
  Prefix, Name, Value: string;
  IsConnectionOptions: boolean;
begin
  IsConnectionOptions := (FOwner <> nil) and (FOwner is TUniConnection);

  Writer.WriteListBegin;
  for i := 0 to Count - 1 do begin
    str := Strings[i];
    if IsConnectionOptions then begin
      ExtractOption(str, Prefix, Name, Value);
      if Name = 'EncryptionKey' then begin
        Value := TUniConnection(FOwner).EncryptToHex(Value);
        if Prefix <> '' then
          Writer.WriteString(Prefix + '.EncryptedKey=' + Value)
        else
          Writer.WriteString('EncryptedKey=' + Value);
      end
      else
        Writer.WriteString(str)
    end
    else
      Writer.WriteString(str);
  end;
  Writer.WriteListEnd;
end;

procedure TSpecificOptionsList.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      if Filer.Ancestor is TStrings then
        Result := not Equals(TStrings(Filer.Ancestor))
      else
        Result := True
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TSpecificOptionsList.Put(Index: Integer; const S: string);
var
  Prefix, Name, Value: string;
begin
  if S = '' then
    inherited Put(Index, S)
  else begin
    ExtractOption(S, Prefix, Name, Value);
    Name := ValidateOption(Prefix, Name, Value);
    if Prefix <> '' then
      inherited Put(Index, Prefix + '.' + Name + '=' + Value)
    else
      inherited Put(Index, GetProvider.GetProviderName + '.' + Name + '=' + Value);
  end;
end;

procedure TSpecificOptionsList.InsertItem(Index: Integer; const S: string; AObject: TObject);
var
  Prefix, Name, Value: string;
begin
  if S = '' then
    inherited InsertItem(Index, S, AObject)
  else begin
    ExtractOption(S, Prefix, Name, Value);
    Name := ValidateOption(Prefix, Trim(Name), Value);
    if Prefix <> '' then
      inherited InsertItem(Index, Prefix + '.' + Name + '=' + Value, AObject)
    else
      inherited InsertItem(Index, GetProvider.GetProviderName + '.' + Name + '=' + Value, AObject);
  end;
end;

function TSpecificOptionsList.IndexOfName(const Name: string): Integer;
var
  Prefix: string;
  ValidName: string;
begin
  Result := inherited IndexOfName(Name);
  if Result = -1 then begin
    ValidName := ResolveDeprecated(Name);
    if ValidName <> '' then
      Result := inherited IndexOfName(ValidName);
  end;

  if (Result = -1) and (Name <> '') and (Pos('.', Name) = 0) then begin
    Prefix := GetProvider.GetProviderName;
    Result := inherited IndexOfName(Prefix + '.' + Name);
    if Result = -1 then begin
      ValidName := ResolveDeprecated(Prefix, Name);
      Result := inherited IndexOfName(Prefix + '.' + ValidName);
    end;
  end;
end;

{ TSpecificOptionsHolder }

constructor TSpecificOptionsHolder.Create(AOwner: TComponent; GetUniProviderFunc: TGetUniProviderFunc);
begin
  inherited Create;

  FValues := TSpecificOptionsList.Create(AOwner, GetUniProviderFunc);
  FValues.OnChanging := ValuesChanging;
  FValues.OnChange := ValuesChanged;
end;

destructor TSpecificOptionsHolder.Destroy;
begin
  FValues.Free;

  inherited;
end;

procedure TSpecificOptionsHolder.ValuesChanging(Sender: TObject);
begin

end;

procedure TSpecificOptionsHolder.ValuesChanged(Sender: TObject);
begin
  FIsModified := True;
end;

{ TUniConnectionOptions }

function TUniConnectionOptions.GetConvertEOL: boolean;
begin
  Result := FOwner.ConvertEOL;
end;

procedure TUniConnectionOptions.SetConvertEOL(Value: boolean);
begin
  FOwner.ConvertEOL := Value;
end;

{ TUniConnectionSpecificOptions }

procedure TUniConnectionSpecificOptions.ValuesChanging(Sender: TObject);
begin
  Assert(FValues.FOwner <> nil);

  if TUniConnection(FValues.FOwner).Connected then
    TUniConnection(FValues.FOwner).Disconnect;
end;

{ TUniMacro }

procedure TUniMacro.AssignTo(Dest: TPersistent);
begin
  if Dest is TUniMacro then begin
    TUniMacro(Dest).Name := Name;
    TUniMacro(Dest).Value := Value;
    TUniMacro(Dest).Condition := Condition;
  end
  else
    inherited;
end;

procedure TUniMacro.SetName(const Value: string);
begin
  if Value <> FName then begin
    FName := Value;
    TUniMacros(Collection).NotifyOwner;
  end;
end;

procedure TUniMacro.SetValue(const Value: string);
begin
  if Value <> FValue then begin
    FValue := Value;
    TUniMacros(Collection).NotifyOwner;
  end;
end;

procedure TUniMacro.SetCondition(const Value: string);
begin
  if Value <> FCondition then begin
    FCondition := Value;
    TUniMacros(Collection).NotifyOwner;
  end;
end;

{ TUniMacros }

constructor TUniMacros.Create(Owner: TPersistent);
begin
  inherited Create(Owner, TUniMacro);
end;

procedure TUniMacros.Add(const Name, Value: string; const Condition: string = '');
var
  Def: TUniMacro;
begin
  Def := TUniMacro(inherited Add);
  Def.Name := Name;
  Def.Value := Value;
  Def.Condition := Condition;
end;

function TUniMacros.FindMacro(const Name: string): TUniMacro;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function TUniMacros.MacroByName(const Name: string): TUniMacro;
begin
  Result := FindMacro(Name);

  if Result = nil then begin
    Result := TUniMacro(inherited Add);
    Result.Name := Name;
  end;
end;

function TUniMacros.GetItem(Index: integer): TUniMacro;
begin
  Result := TUniMacro(inherited Items[Index]);
end;

procedure TUniMacros.SetItem(Index: integer; Value: TUniMacro);
begin
  inherited SetItem(Index, Value);
end;

procedure TUniMacros.NotifyOwner;
begin
  if (UpdateCount = 0) and (Owner is TUniConnection) then begin
    TUniConnection(Owner).FMacrosChanged := True;
    Inc(TUniConnection(Owner).FMacrosVersion);
  end;
end;

procedure TUniMacros.Update(Item: TCollectionItem);
begin
  inherited;

  NotifyOwner;
end;

{ TUniConnection }

constructor TUniConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSpecificOptions := TUniConnectionSpecificOptions.Create(Self, GetProvider);

  if OldTransactionBehaviour then begin
    FInternalTransaction := CreateTransaction;
    TUniTransaction(FInternalTransaction).DefaultConnection := Self;
    TUniTransaction(FInternalTransaction).IsolationLevel := ilReadCommitted;
  end;

  FMacros := TUniMacros.Create(Self);
  FMacroNames := TStringList.Create;
  FMacroValues := TStringList.Create;
end;

destructor TUniConnection.Destroy;
begin
  FSpecificOptions.Free;
  if OldTransactionBehaviour then
    FInternalTransaction.Free;
  FMacros.Free;
  FMacroNames.Free;
  FMacroValues.Free;
  FSQLFormatter.Free;

  inherited;
end;

procedure TUniConnection.CheckProvider;
begin
  if FProvider = nil then
    CheckProviderName(ProviderName);
end;

function TUniConnection.CanGetProvider: boolean;
begin
  Result := FProvider <> nil;
end;

function TUniConnection.GetProvider: TUniProvider;
begin
  CheckProvider;
  Result := FProvider;
end;

function TUniConnection.GetIConnectionClass: TCRConnectionClass;
begin
  Result := FProvider.GetConnectionClass;
end;

function TUniConnection.GetICommandClass: TCRCommandClass;
begin
  CheckProvider;
  if FIConnection = nil then
    CreateIConnection;

  Result := FIConnection.GetCommandClass;
end;

function TUniConnection.GetIRecordSetClass: TCRRecordSetClass;
begin
  CheckProvider;
  if FIConnection = nil then
    CreateIConnection;

  Result := FIConnection.GetRecordSetClass;
end;

function TUniConnection.GetIMetaDataClass: TCRMetaDataClass;
begin
  CheckProvider;
  if FIConnection = nil then
    CreateIConnection;

  Result := FIConnection.GetMetaDataClass;
end;

function TUniConnection.GetITransactionClass: TCRTransactionClass;
begin
  CheckProvider;
  if FIConnection = nil then
    CreateIConnection;

  Result := FIConnection.GetTransactionClass;
end;

procedure TUniConnection.SetConnectionParameters(ConnectionParameters: TCRConnectionParameters);
begin
  inherited;

  if FProvider.IsDatabaseSupported then //upd1
    ConnectionParameters.SetProp(prDatabase, FDatabase);
  if FProvider.IsPortSupported then
    ConnectionParameters.SetProp(prPort, Port);

  FProvider.SetObjectProps(ConnectionParameters, FSpecificOptions.Values);
end;

procedure TUniConnection.SetBaseConnectionProps(Connection: TCRConnection);
begin
  inherited;

  if FProvider.IsDatabaseSupported then
    Connection.SetProp(prDatabase, FDatabase);
  if FProvider.IsPortSupported then
    Connection.SetProp(prPort, Port);
end;

procedure TUniConnection.SetConnectionProps(Connection: TCRConnection);
begin
  inherited;

  Connection.SetProp(prDefaultSortType, Variant(Options.DefaultSortType));

  // if connection is just created we need to set all options
  FProvider.SetObjectProps(Connection, FSpecificOptions.Values);
  FSpecificOptions.IsModified := False;
end;

function TUniConnection.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := FProvider.GetConnectionParametersClass;
end;

function TUniConnection.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  if FProvider.IsPoolingSupported then
    Result := FProvider.GetConnectionPoolingManagerClass
  else
    Result := nil;
end;

procedure TUniConnection.CreateIConnection;
begin
  CheckProvider;

  if FIConnection <> nil then begin
    if FSpecificOptions.IsModified then begin
      FProvider.SetObjectProps(FIConnection, FSpecificOptions.Values);
      FSpecificOptions.IsModified := False;
    end;
  end
  else
    inherited;
end;

procedure TUniConnection.SetIConnection(Value: TCRConnection);
begin
  inherited;

  if FIConnection <> nil then
    FIConnection.SetProp(prAutoCommit, AutoCommit);
end;

function TUniConnection.ExecProc(const Name: string; const Params: array of variant): variant;
begin
  try
    Result := inherited ExecProc(Name, Params);
  finally
    TUniSQL(FCommand).FEnableUniSQL := EnableUniSQL;
  end;
end;

function TUniConnection.ExecProcEx(const Name: string; const Params: array of variant): variant;
begin
  try
    Result := inherited ExecProcEx(Name, Params);
  finally
    TUniSQL(FCommand).FEnableUniSQL := EnableUniSQL;
  end;
end;

function TUniConnection.CreateDataSet(AOwner: TComponent = nil): TCustomDADataSet;
begin
  Result := TCustomUniDataSet.Create(AOwner);
  Result.Connection := Self;
end;

function TUniConnection.CreateSQL: TCustomDASQL;
begin
  Result := TUniSQL.Create(nil);
  Result.Connection := Self;
  Result.SQL.Text := '';
  TDBAccessUtils.SetAutoCommit(Result, True);
end;

function TUniConnection.CreateTransaction: TDATransaction;
begin
  Result := TUniTransaction.Create(nil);
  Result.DefaultConnection := Self;
end;

function TUniConnection.CreateMetaData: TDAMetaData;
begin
  Result := TUniMetaData.Create(nil);
  Result.Connection := Self;
end;

procedure TUniConnection.AssignConnect(Source: TUniConnection);
begin
  inherited AssignConnect(Source);
end;

procedure TUniConnection.DoConnect;
var
  Database: Variant;
begin

  try
    inherited;
  except
    HandleInternalError;
  end;

  if FProvider.IsDatabaseSupported and (FDatabase = '') then begin
    FIConnection.GetProp(prDatabase, Database);
    FDatabase := string(Database);
  end;
end;

procedure TUniConnection.DoDisconnect;
begin
  try
    inherited;
  except
    HandleInternalError;
  end;
end;

procedure TUniConnection.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniConnection then begin
    TUniConnection(Dest).ProviderName := ProviderName;
    TUniConnection(Dest).Database := Database;
    TUniConnection(Dest).Port := Port;
    TUniConnection(Dest).Macros.Assign(Macros);
    TUniConnection(Dest).IOHandler := IOHandler;
    TUniConnection(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TUniConnection.SQLMonitorClass: TClass;
begin
  Result := TUniSQLMonitor;
end;

function TUniConnection.ConnectDialogClass: TConnectDialogClass;
begin
  if Assigned(DefConnectDialogClassProc) then
    Result := TConnectDialogClass(DefConnectDialogClassProc)
  else
    Result := nil;
end;

function TUniConnection.CreateOptions: TDAConnectionOptions;
begin
  Result := TUniConnectionOptions.Create(Self);
end;

function TUniConnection.CreateConnectionStringBuilder: TCRConnectionStringBuilder;
begin
  Result := TUniConnectionStringBuilder.Create(GetConnectionStringParam, SetConnectionStringParam);

  if FProvider <> nil then
    TUniConnectionStringBuilder(Result).ExtStringBuilderClass := FProvider.GetConnectionStringClass;
end;

function TUniConnection.IsMultipleTransactionsSupported: boolean;
var
  Provider: TUniProvider;
begin
  Provider := GetProvider;
  if Provider <> nil then
    Result := Provider.IsDataSetNeedTransaction
  else
    Result := inherited IsMultipleTransactionsSupported;
end;

function TUniConnection.GetProviderName: string;
begin
  Result := FProviderName;
end;

procedure TUniConnection.SetProviderName(Value: string);
begin
  if AnsiCompareText(Trim(Value), FProviderName) <> 0 then begin
    if Connected then
      DatabaseError('This operation is not allowed on active connection');

    if Trim(Value) = '' then
      FProvider := nil
    else
      FProvider := UniProviders.GetProvider(Value);

    FreeIConnection; // TODO:

    FProviderName := Trim(Value);
    UniqueString(FProviderName);
    FMacrosChanged := True;

    if FConnectionStringBuilder <> nil then
      if FProvider <> nil then
        TUniConnectionStringBuilder(FConnectionStringBuilder).ExtStringBuilderClass := FProvider.GetConnectionStringClass
      else
        TUniConnectionStringBuilder(FConnectionStringBuilder).ExtStringBuilderClass := nil;
  end;
end;

procedure TUniConnection.SetDatabase(const Value: string);
begin
  if FDatabase <> Trim(Value) then begin
    if Connected and not FIConnection.CanChangeDatabase then
      Disconnect;
    FDatabase := Trim(Value);
    UniqueString(FDatabase);
    if FIConnection <> nil then
      FIConnection.SetProp(prDatabase, Value);
  end;
end;

procedure TUniConnection.SetPort(Value: integer);
begin
  if FPort <> Value then begin
    Disconnect;
    FPort := Value;
    if FIConnection <> nil then
      FIConnection.SetProp(prPort, Value);
  end;
end;

function TUniConnection.GetSpecificOptions: TSpecificOptionsList;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniConnection.SetSpecificOptions(Value: TSpecificOptionsList);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TUniConnection.GetTransaction(Index: Integer): TUniTransaction;
begin
  Result := TUniTransaction(inherited Transactions[Index]);
end;

function TUniConnection.GetOptions: TUniConnectionOptions;
begin
  Result := TUniConnectionOptions(inherited Options);
end;

procedure TUniConnection.SetOptions(Value: TUniConnectionOptions);
begin
  inherited Options := Value;
end;

procedure TUniConnection.SetMacros(Value: TUniMacros);
begin
  FMacros.Assign(Value);
end;

function TUniConnection.IsMacrosStored: boolean;
begin
  Result := FMacros.Count > 0;
end;

function TUniConnection.GetDefaultTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited DefaultTransaction);
end;

procedure TUniConnection.SetDefaultTransaction(const Value: TUniTransaction);
begin
  inherited DefaultTransaction := Value;

  if Value <> nil then begin
    Value.FDefaultConnection := Self;
    Value.FShareTransaction := not IsMultipleTransactionsSupported;
  end;
end;

function TUniConnection.GetSQL: TUniSQL;
begin
  Result := TUniSQL(InternalGetSQL);
end;

function TUniConnection.ParamByName(const Name: string): TUniParam;
begin
  Result := TUniParam(inherited ParamByName(Name));
end;

function TUniConnection.MacroByName(const Name: string): TUniMacro;
begin
  Result := FMacros.MacroByName(Name);
end;

function TUniConnection.ActiveMacroValueByName(const Name: string): Variant;
var
  Index: Integer;
begin
  Result := null;

  if CanGetProvider then begin
    CheckSqlFormatter;
    if FMacrosChanged then
      DetectActiveMacros;

    Index := FMacroNames.IndexOf(Name);
    if Index >= 0 then
      Result := FMacroValues[Index];
  end;
end;

{$IFNDEF STD}
procedure TUniConnection.EncryptTable(const TableName: string; Encryptor: TUniEncryptor; const Fields: string);
begin
  inherited EncryptTable(TableName, Encryptor, Fields);
end;
{$ENDIF}

function TUniConnection.UsedTransaction: TDATransaction;
begin
  if IsMultipleTransactionsSupported and not DefaultTransaction.Active then begin
    if not OldTransactionBehaviour then
      Result := FInternalDefTransaction
    else
      Result := FInternalTransaction;
  end
  else
    Result := inherited UsedTransaction;
end;

function TUniConnection.GetInTransaction: boolean;
begin
  Result := DefaultTransaction.Active;
end;

function TUniConnection.GetInternalDefTransaction: TUniTransaction;
begin
  Result := TUniTransaction(FInternalDefTransaction);
end;

procedure TUniConnection.StartTransaction;
begin
  inherited StartTransaction;
end;

procedure TUniConnection.StartTransaction(IsolationLevel: TCRIsolationLevel; ReadOnly: boolean = False);
begin
  if not DefaultTransaction.Active then begin
    TUniTransaction(DefaultTransaction).IsolationLevel := IsolationLevel;
    TUniTransaction(DefaultTransaction).ReadOnly := ReadOnly;
  end;

  inherited StartTransaction;
end;

procedure TUniConnection.CommitRetaining;
begin
  DoCommitRetaining;
end;

procedure TUniConnection.RollbackRetaining;
begin
  DoRollbackRetaining;
end;

procedure TUniConnection.Savepoint(const Name: string);
begin
  DoSavepoint(Name);
end;

procedure TUniConnection.ReleaseSavepoint(const Name: string);
begin
  DoReleaseSavepoint(Name);
end;

procedure TUniConnection.RollbackToSavepoint(const Name: string);
begin
  DoRollbackToSavepoint(Name);
end;

function TUniConnection.GetServerVersion: string;
begin
  if not Connected then
    raise Exception.Create(SConnectionNotConnected);

  Result := FIConnection.GetServerVersion;
end;

function TUniConnection.GetServerVersionFull: string;
begin
  if not Connected then
    raise Exception.Create(SConnectionNotConnected);

  Result := FIConnection.GetServerVersionFull;
end;

function TUniConnection.GetClientVersion: string;
begin
  if not Connected then
    raise Exception.Create(SConnectionNotConnected);

  Result := FIConnection.GetClientVersion;
end;

function TUniConnection.GetConnectionStringParam(ParamCode: integer): variant;
var
  OptionsList: TOptionsList;
  opt: TOption;
  optStr: string;
begin
  case ParamCode of
    cpProviderName:
      Result := Self.ProviderName;
    prServer:
      Result := Self.Server;
    prUsername:
      Result := Self.Username;
    prPassword:
      Result := Self.Password;
    prDatabase:
      Result := Self.Database;
    prPort:
      Result := Self.Port;
    else
      if FProvider <> nil then begin
        OptionsList := FProvider.GetConnectionOptions;
        opt := OptionsList.OptionByCode(ParamCode);
        if opt <> nil then begin
          optStr := Self.SpecificOptions.Values[opt.OptionName];
          if optStr = '' then
            Result := opt.GetDefaultValue
          else
            Result := opt.GetAsNative(optStr);
        end
        else
          Result := inherited GetConnectionStringParam(ParamCode);
      end
      else
        Result := inherited GetConnectionStringParam(ParamCode);
    end;
end;

procedure TUniConnection.SetConnectionStringParam(ParamCode: integer; const ParamValue: variant);
var
  OptionsList: TOptionsList;
  opt: TOption;
begin
  case ParamCode of
    cpProviderName:
      SetProviderName(ParamValue);
    prServer:
      Self.Server := VarToStr(ParamValue);
    prUsername:
      Self.Username := VarToStr(ParamValue);
    prPassword:
      Self.Password := VarToStr(ParamValue);
    prDatabase:
      Self.Database := VarToStr(ParamValue);
    prPort:
      Self.Port := StrToInt(ParamValue);
    else
      if FProvider <> nil then begin
        OptionsList := FProvider.GetConnectionOptions;
        opt := OptionsList.OptionByCode(ParamCode);
        if opt <> nil then
          Self.SpecificOptions.Values[opt.OptionName] := opt.GetAsString(ParamValue)
        else
          inherited SetConnectionStringParam(ParamCode, ParamValue)
      end
      else
        inherited SetConnectionStringParam(ParamCode, ParamValue)
  end;
end;

function TUniConnection.DefaultTableSchema: string;
var
  Provider: TUniProvider;
begin
  if ProviderName = '' then
    DatabaseError(SProviderNotDefined);

  Provider := UniProviders.GetProvider(ProviderName);

  if Provider = nil then
    CheckProviderName(ProviderName);

  Result := Provider.DefaultTableSchema;
end;

procedure TUniConnection.DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean;
  ReconnectAttempt: integer; var ConnLostCause: TConnLostCause);
var
  UniErr: EUniError;
begin
  if E is EDAError then begin
    UniErr := EUniError.Create(EDAError(E));
    try
      inherited DoError(UniErr, Fail, Reconnect, Reexecute, ReconnectAttempt, ConnLostCause);
    finally
      UniErr.FInnerError := nil;
      UniErr.Free;
    end;
  end
  else
    inherited DoError(E, Fail, Reconnect, Reexecute, ReconnectAttempt, ConnLostCause);
end;

procedure TUniConnection.AssignConnectOptions(Source: TCustomDAConnection);
var
  i: integer;
begin
  inherited;

  ProviderName := TUniConnection(Source).ProviderName;
  Database := TUniConnection(Source).Database;
  Port := TUniConnection(Source).Port;
  // TStringList.Assign does not work across modules
  SpecificOptions.Clear;
  for i := 0 to TUniConnection(Source).SpecificOptions.Count - 1 do
    SpecificOptions.Add(TUniConnection(Source).SpecificOptions[i]);
end;

procedure TUniConnection.CheckSqlFormatter;
var
  Cls: TUniSqlFormatterClass;
begin
  Cls := FProvider.GetSqlFormatterClass;
  if not (FSQLFormatter is Cls) then begin
    FSQLFormatter.Free;
    FSQLFormatter := Cls.Create;
    FSQLFormatter.SetUserMacros(FMacroNames, FMacroValues);
    FSQLFormatter.SetParserClass(FProvider.GetParserClass);
  end;
end;

procedure TUniConnection.DetectActiveMacros;

  function GetActive(Depth: integer; const MacroName: string): boolean;
  var
    i: integer;
    Macro: TUniMacro;
    Name, Cond: string;
  begin
    if Depth > Macros.Count then
      raise Exception.Create(SCyclicConditions);

    Result := FSQLFormatter.CheckIfCondition(MacroName);
    if not Result then begin
      for i := 0 to Macros.Count - 1 do begin
        Macro := Macros[i];
        Name := AnsiUpperCase(Macro.Name);
        if (Name = MacroName) then begin
          Cond := Trim(AnsiUpperCase(Macro.Condition));
          if (Cond = '') or GetActive(Depth + 1, Cond) then begin
            Result := True;
            break;
          end;
        end;
      end;
    end;
  end;

var
  i, j, k: integer;
  Macro: TUniMacro;
  UncondMacros: TStringList;
  Name, Cond: string;
begin
  FMacroNames.Clear;
  FMacroValues.Clear;
  UncondMacros := TStringList.Create;
  try
    for i := 0 to Macros.Count - 1 do begin
      Macro := Macros[i];
      Cond := Trim(AnsiUpperCase(Macro.Condition));

      if (Cond = '') or GetActive(1, Cond) then begin
        Name := AnsiUpperCase(Macro.Name);
        j := FMacroNames.IndexOf(Name);

        if j >= 0 then begin
          k := UncondMacros.IndexOf(Name);
          if (Cond <> '') and (k >= 0) then begin
            // replace value with conditional macro
            FMacroValues[j] := Macro.Value;
            UncondMacros.Delete(k);
          end
          else
            // ignore value
        end
        else begin
          FMacroNames.Add(Name);
          FMacroValues.Add(Macro.Value);
        end;

        if Cond = '' then
          UncondMacros.Add(Name);
      end;
    end;
  finally
    UncondMacros.Free;
  end;
  FMacrosChanged := False;
end;

procedure TUniConnection.ExpandMacros(var SQL: string);
begin
  if CanGetProvider then begin
    CheckSqlFormatter;
    if FMacrosChanged then
      DetectActiveMacros;
    FSQLFormatter.Expand(SQL);
  end;
end;

{ TUniTransactionSpecificOptions }

procedure TUniTransactionSpecificOptions.ValuesChanged(Sender: TObject);
var
  ITransaction: TCRTransaction;
begin
  inherited;

  ITransaction := TDBAccessUtils.GetITransaction(TUniTransaction(FValues.FOwner));
  if ITransaction <> nil then
    Values.GetProvider.SetObjectProps(ITransaction, Values);
end;

procedure TUniTransactionSpecificOptions.ValuesChanging(Sender: TObject);
begin
  Assert(FValues.FOwner <> nil);

  TUniTransaction(FValues.FOwner).CheckInactive;
end;

{ TUniSQL }

constructor TUniSQL.Create(AOwner: TComponent);
begin
  inherited;

  FSpecificOptions := TSpecificOptionsHolder.Create(Self, GetProvider);

  TUniParams(Params).SetOwner(Self);
  FParamRefs := TList.Create;
  FEnableUniSQL := EnableUniSQL;

  AutoCommit := True;
end;

destructor TUniSQL.Destroy;
begin
  FSpecificOptions.Free;
  FParamRefs.Free;

  inherited;
end;

procedure TUniSQL.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniSQL then
    TUniSQL(Dest).SpecificOptions.Assign(SpecificOptions);
end;

procedure TUniSQL.SetICommand(Value: TCRCommand);
begin
  inherited;

  if FICommand <> nil then
    GetProvider.SetObjectProps(FICommand, FSpecificOptions.Values);
end;

function TUniSQL.GetFieldTypeMapClass: TDAFieldTypeMapClass;
var
  vProvider: TUniProvider;
begin
  vProvider := FindProvider;
  if vProvider <> nil then
    Result := vProvider.GetFieldTypeMapClass
  else
    Result := inherited GetFieldTypeMapClass
end;

function TUniSQL.CreateParamsObject: TDAParams;
begin
  // Note: Owner should be <> nil only for TUniSQL.Params, see ResetParamRefs
  Result := TUniParams.Create(nil);
end;

function TUniSQL.UsedTransaction: TDATransaction;
begin
  if FFixedUsedTransaction <> nil then
    Result := FFixedUsedTransaction
  else
    Result := inherited UsedTransaction;
end;

procedure TUniSQL.BeginConnection(NoConnectCheck: boolean = True);
begin
  inherited;

  if FSpecificOptions.IsModified then begin
    GetProvider.SetObjectProps(FICommand, FSpecificOptions.Values);
    GetProvider.SetObjectProps(Self, FSpecificOptions.Values);
    FSpecificOptions.IsModified := False;
  end;

  if FFixedUsedTransaction = nil then
    FFixedUsedTransaction := UsedTransaction;
end;

procedure TUniSQL.EndConnection;
begin
  inherited;

  if not Prepared then
    FFixedUsedTransaction := nil;
end;

function TUniSQL.GetTransaction: TDATransaction;
var
  vConnection: TCustomDAConnection;
begin
  Result := inherited GetTransaction;

  if (Result = nil) and (FDataSet <> nil) then
    Result := TDBAccessUtils.UsedTransaction(FDataSet);

  if not OldTransactionBehaviour then
    if (Result = nil) and not (csAncestor in ComponentState) then begin
      vConnection := UsedConnection;
      if (vConnection <> nil) and (TUniConnection(vConnection).DefaultTransaction <> nil) then
        Result := TUniConnection(vConnection).DefaultTransaction
    end;
end;

procedure TUniSQL.InternalPrepare;
begin
  CheckUniMacros;
  try
    inherited;
  except
    HandleInternalError;
  end;
end;

procedure TUniSQL.InternalUnPrepare;
begin
  try
    inherited;
  except
    HandleInternalError;
  end;
end;

procedure TUniSQL.InternalExecute(Iters: integer; Offset: integer);
begin
  CheckUniMacros;
  try
    inherited;
  except
    HandleInternalError;
  end;
end;

function TUniSQL.GetFinalSQL: string;
var
  vConnection: TUniConnection;
begin
  Result := inherited GetFinalSQL;

  if FEnableUniSQL and EnableUniSQL then begin
    vConnection := TUniConnection(UsedConnection);
    if vConnection <> nil then
      vConnection.ExpandMacros(Result);
  end;
end;

procedure TUniSQL.CheckUniMacros;
var
  vConnection: TUniConnection;
begin
  if EnableUniSQL then begin
    vConnection := TUniConnection(UsedConnection);
    if (vConnection <> nil) and (FMacrosVersion < vConnection.FMacrosVersion) then begin
      if FICommand <> nil then begin
        SetICommandSQL;
        WriteParams;
      end;
      FMacrosVersion := vConnection.FMacrosVersion;
    end;
  end;
end;

function TUniSQL.IsInOutParamSupported: boolean;
begin
  Result := GetProvider.IsInOutParamSupported;
end;

function TUniSQL.NeedRecreateProcCall: boolean;
begin
  Result := GetProvider.NeedRecreateProcCall;
end;

function TUniSQL.ParseSQL(const SQL: string; Params: TDAParams): string;
var
  ParamsSupported: boolean;
  ParsedSQL: StringBuilder;
  Parser: TSQLParser;
  ParserClass: TSQLParserClass;
  StartPos: integer;
  vConnection: TUniConnection;
  vProvider: TUniProvider;

  LeftQuote, RightQuote: char;
  AllParams: TStringList;

  procedure ParseSQLParam;
  var
    Code: integer;
    St: string;
    DogPresent: boolean;
    l: integer;
    ParamName: string;
  begin
    Code := Parser.GetNext(St);
    DogPresent := Code = lxAt;
    if DogPresent then
      Code := Parser.GetNext(St); // Omit '@' in ParamName for BDE compatibility

    if (Params <> nil) and ((Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst)) // and (St <> '=')
    then begin
      if DogPresent then
        ParamName := '@' + St
      else
        ParamName := St;

      l := Length(ParamName);
      // remove quotes
      if (ParamName[1] = LeftQuote) and (ParamName[l] = RightQuote) then
        ParamName := Copy(ParamName, 2, l - 2);

      if AllParams.IndexOf(ParamName) < 0 then begin
        TDAParam(Params.Add).Name := ParamName;
        AllParams.Add(ParamName);
      end;
      ParsedSQL.Append('?');
    end
    else begin // Labels in SQL Server, MySQL syntax and PL SQL Blocks (a := b).
      ParsedSQL.Append(':');
      if DogPresent then
        ParsedSQL.Append('@');
      ParsedSQL.Append(St);
    end;
  end;

begin
  vProvider := FindProvider;
  if vProvider <> nil then begin
    vConnection := TUniConnection(UsedConnection);
    vConnection.CheckSqlFormatter;
    LeftQuote := vConnection.FSQLFormatter.LeftQuote;
    RightQuote := vConnection.FSQLFormatter.RightQuote;
    ParserClass := vProvider.GetParserClass;
    ParamsSupported := vProvider.IsParamsSupported;
  end
  else begin
    LeftQuote := '"';
    RightQuote := '"';
    ParserClass := TSQLParser;
    ParamsSupported := True;
  end;

  if Params <> nil then begin
    Params.BeginUpdate;
    Params.Clear;
  end;
  try
    if ParamsSupported then begin
      ParsedSQL := StringBuilder.Create(Length(SQL) + Length(SQL) div 2);
      AllParams := TStringList.Create;
      AllParams.Sorted := True;
      try
        Parser := ParserClass.Create(SQL);
        try
          Parser.OmitBlank := False;
          Parser.OmitComment := True;
          Parser.QuotedString := True;
          Parser.ToBegin;
          StartPos := Parser.CurrPos;
          while Parser.ToLexem(':') do begin
            ParsedSQL.Append(Copy(SQL, StartPos + 1, Parser.CurrPos - StartPos - 1));
            ParseSQLParam;

            StartPos := Parser.CurrPos;
          end;
          ParsedSQL.Append(Copy(SQL, StartPos + 1, Parser.CurrPos - StartPos));
        finally
          Parser.Free;
        end;
        Result := ParsedSQL.ToString;
      finally
        ParsedSQL.Free;
        AllParams.Free;
      end;
    end
    else
      Result := SQL;
  finally
    if Params <> nil then
      Params.EndUpdate;
  end;
end;

procedure TUniSQL.AssembleSQL;
var
  i: integer;
  List: TDAParams;
  NativeSQL: string;
  vConnection: TCustomDAConnection;
begin
  FWriteAllParams := FLockScanParams;

  vConnection := UsedConnection;
  if (vConnection <> nil) and (TUniConnection(vConnection).FIConnection <> nil) then
    inherited
  else begin
    if FDataSet = nil then
      NativeSQL := FinalSQL
    else
      NativeSQL := FDataSet.FinalSQL;

    if (ParamCheck or (csDesigning in ComponentState)) and not FLockScanParams then begin
      List := CreateParamsObject;
      try
        List.BeginUpdate;
        try
          ParseSQL(NativeSQL, List);
          List.AssignValues(Params);
        finally
          List.EndUpdate;
        end;

        Params.Clear;
        Params.Assign(List);
        FParamRefs.Clear;
        for i := 0 to Params.Count - 1 do
          FParamRefs.Add(Params[i]);
      finally
        List.Free;
      end;
    end;
  end;

  if vConnection <> nil then
    FMacrosVersion := TUniConnection(vConnection).FMacrosVersion;
end;

procedure TUniSQL.CreateParams;
var
  ParamDesc: TParamDesc;
  Param: TDAParam;
  i, j: integer;
  AllParams: TStringList;
begin
  AllParams := TStringList.Create;
  AllParams.Sorted := True;
  FLockParamRefsReset := True;
  Params.BeginUpdate;
  try
    Params.Clear;
    FParamRefs.Clear;
    for i := 0 to FICommand.Params.Count - 1 do begin
      ParamDesc := FICommand.Params[i];
      j := AllParams.IndexOf(ParamDesc.GetName);
      if j = -1 then begin
        Param := Params.Add as TDAParam;
        AssignParamDesc(Param, ParamDesc);
        AllParams.AddObject(ParamDesc.GetName, Param);
      end
      else
        Param := TDAParam(AllParams.Objects[j]);

      FParamRefs.Add(Param);
    end;
  finally
    Params.EndUpdate;
    FLockParamRefsReset := False;
    AllParams.Free;
  end;
end;

procedure TUniSQL.WriteParams(WriteValue: boolean = True);
var
  Param: TDAParam;
  ParamDesc: CRAccess.TParamDesc;
  i: integer;
  UseParamRefs: boolean;
begin
  if (ParamCheck and not FWriteAllParams) or (Params.ParamsChangeType = ctUsers) then begin
    UseParamRefs := FParamRefs.Count = FICommand.Params.Count;
    if not UseParamRefs then
      FParamRefs.Clear;

    for i := 0 to FICommand.Params.Count - 1 do begin
      ParamDesc := FICommand.Params[i];
      if UseParamRefs then
        Param := TDAParam(FParamRefs[i])
      else begin
        Param := FindParam(ParamDesc.GetName);

        if (Param = nil) and (Params.ParamsChangeType = ctGenerated) then begin
          Param := Params.Add as TDAParam;
          AssignParamDesc(Param, ParamDesc);
        end;

        FParamRefs.Add(Param);
      end;

      if Param <> nil then begin
        AssignParam(ParamDesc, Param);

        if WriteValue then begin
        {$IFDEF PERF_COUNTER}
          PerfCounters[5].Start;
        {$ENDIF}
          AssignParamValue(ParamDesc, Param);
        {$IFDEF PERF_COUNTER}
          PerfCounters[5].Stop;
        {$ENDIF}
        end;
      end;
    end;

    if Params.ParamsChangeType = ctUsers then begin
      if FICommand.Params.Count <> Params.Count then // for ReadParams
        FParamRefs.Clear
      else
        for i := 0 to FICommand.Params.Count - 1 do begin
          if FICommand.Params[i].GetName <> Params[i].Name then begin
            FParamRefs.Clear;
            break;
          end;
        end;

      inherited;
    end
    else
      Params.ParamsChangeType := ctGenerated;
  end
  else
    inherited;
end;

procedure TUniSQL.ReadParams;
var
  i: integer;
  Param: TDAParam;
  ParamDesc: TParamDesc;
  CanReadParams: Variant;
begin
  if ParamCheck and not FWriteAllParams then begin  //upd1 merge with dbaccess
    Assert(FICommand <> nil);
    FICommand.GetProp(prCanReadParams, CanReadParams);
    if CanReadParams then begin
      for i := 0 to FICommand.Params.Count - 1 do begin
        ParamDesc := FICommand.Params[i];
        if FParamRefs.Count > i then
          Param := TDAParam(FParamRefs[i])
        else
          Param := Params.FindParam(ParamDesc.GetName);

        if (Param <> nil) and (Param.ParamType <> ptInput) and
          (IsInOutParamSupported or (Param.ParamType <> ptUnknown)) // if in/out not supported treat Unknown as Input
        then
          AssignParamDescValue(Param, ParamDesc);
      end;

      FICommand.SetProp(prCanReadParams, False); // For SDAC
    end;
  end
  else
    inherited;
end;

procedure TUniSQL.ResetParamRefs;
begin
  if not (csDestroying in ComponentState) and not FLockParamRefsReset then
    FParamRefs.Clear;
end;

function TUniSQL.FindResultParam: TDAParam;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Params.Count - 1 do
    if Params[i].ParamType = ptResult then
      Result := Params[i];

  // ODAC compatibility
  if Result = nil then
    Result := Params.FindParam('Result');

  if Result <> nil then
    if Result.DataType = ftUnknown then
      Result.DataType := ftInteger;
end;

function TUniSQL.FindParam(const Value: string): TUniParam;
var
  vProvider: TUniProvider;
begin
  vProvider := FindProvider;
  if vProvider <> nil then
    Result := Params.FindParam(vProvider.CheckParamName(Value))
  else
    Result := Params.FindParam(Value);
end;

function TUniSQL.ParamByName(const Value: string): TUniParam;
var
  vProvider: TUniProvider;
begin
  vProvider := FindProvider;
  if vProvider <> nil then
    Result := Params.ParamByName(vProvider.CheckParamName(Value))
  else
    Result := Params.ParamByName(Value);
end;

procedure TUniSQL.CreateProcCall(const Name: string);
begin
  InternalCreateProcCall(Name, True);
end;

function TUniSQL.GetParams: TUniParams;
begin
  Result := TUniParams(inherited Params);
end;

procedure TUniSQL.SetParams(Value: TUniParams);
begin
  inherited Params := Value;
end;

function TUniSQL.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniSQL.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniSQL.GetSpecificOptions: TSpecificOptionsList;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniSQL.SetSpecificOptions(Value: TSpecificOptionsList);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TUniSQL.GetUniTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TUniSQL.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

function TUniSQL.FindProvider: TUniProvider;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then
    Result := TUniConnection(vConnection).FProvider
  else
    Result := nil;
end;

function TUniSQL.GetProvider: TUniProvider;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniConnection(vConnection).GetProvider;
end;

procedure TUniSQL.InternalCreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean = False);
begin
  try
    if not NeedDescribe then
      FWriteAllParams := True;

    inherited;

    FEnableUniSQL := False;
  finally
    FWriteAllParams := False;
  end;
end;

procedure TUniSQL.AssignParamDesc(Param: TDAParam; ParamDesc: TParamDesc);
var
  DBObject: TDBObject;
begin
  inherited;

  if TUniParam(Param).IsObject then begin
    DBObject := TDBObject(ParamDesc.GetObject);
    if DBObject <> nil then begin
      TUniParam(Param).ParamObject := DBObject;
      ParamDesc.SetObject(nil);
    end;
  end;
end;

procedure TUniSQL.AssignParamValue(ParamDesc: TParamDesc; Param: TDAParam);
var
  UniParam: TUniParam;
  ParamDataType: TFieldType;
  ParamObject: TSharedObject;
  ValueCount, i: integer;
  ValuePtr: PVarData;
begin
  ValueCount := Param.ValueCount;

  if ValueCount <= 1 then
    inherited AssignParamValue(ParamDesc, Param)
  else begin
    UniParam := TUniParam(Param);

    ParamDataType := UniParam.GetDataType;
    if UniParam.IsBlobDataType(ParamDataType) or (ParamDataType = ftCursor) then begin
      SetLength(UniParam.FNativeDataArr, ValueCount);

      for i := 0 to UniParam.ValueCount - 1 do begin
        ValuePtr := PVarData(@UniParam.FDataArr[i]);
        if (ValuePtr.VType <> varSharedObject) or (ValuePtr.VPointer = nil) then begin
          ParamObject := UniParam.CreateObject;
          ValuePtr.VType := varSharedObject;
          ValuePtr.VPointer := ParamObject;
        end;
        ParamObject := UniParam.GetNativeParamObject(TSharedObject(ValuePtr.VPointer));
        UniParam.FNativeDataArr[i] := ParamObject.ToVariant;
      end;

      ParamDesc.SetObjectArr(@UniParam.FNativeDataArr);
    end
    else
      ParamDesc.SetValueArr(@UniParam.FDataArr);
  end;
end;

//function TUniSQL.UsedConnection: TCustomDAConnection;
//begin
//  Result := inherited UsedConnection;
//end;

{$IFNDEF STD}

{ TUniEncryption }

function TUniEncryption.GetEncryptor: TUniEncryptor;
begin
  Result := TUniEncryptor(inherited Encryptor);
end;

procedure TUniEncryption.SetEncryptor(Value: TUniEncryptor);
begin
  inherited Encryptor := Value;
end;

{$ENDIF}

{ TUniDataSetOptions }

constructor TUniDataSetOptions.Create(Owner: TCustomDADataSet);
begin
  inherited;

  SetFieldsReadOnly := True;
end;

{ TUniDataSetSpecificOptions }

procedure TUniDataSetSpecificOptions.ValuesChanging(Sender: TObject);
begin
  Assert(FValues.FOwner <> nil);

  TCustomUniDataSet(FValues.FOwner).CheckInactive;
end;

{ TCustomUniDataSet }

constructor TCustomUniDataSet.Create(AOwner: TComponent);
begin
  inherited;

  FSpecificOptions := TUniDataSetSpecificOptions.Create(Self, GetProvider);
end;

destructor TCustomUniDataSet.Destroy;
begin
  FSpecificOptions.Free;
  FCursor.Free;

  inherited;
end;

procedure TCustomUniDataSet.CreateProcCall(const Name: string);
begin
  InternalCreateProcCall(Name, True);
end;

function TCustomUniDataSet.OpenNext: boolean;
begin
  Result := DoOpenNext;
end;

procedure TCustomUniDataSet.RefreshQuick(const CheckDeleted: boolean);
begin
  InternalRefreshQuick(CheckDeleted);
end;

function TCustomUniDataSet.FindParam(const Value: string): TUniParam;
var
  vProvider: TUniProvider;
begin
  vProvider := FindProvider;
  if vProvider <> nil then
    Result := Params.FindParam(vProvider.CheckParamName(Value))
  else
    Result := Params.FindParam(Value);
end;

function TCustomUniDataSet.ParamByName(const Value: string): TUniParam;
var
  vProvider: TUniProvider;
begin
  vProvider := FindProvider;
  if vProvider <> nil then
    Result := Params.ParamByName(vProvider.CheckParamName(Value))
  else
    Result := Params.ParamByName(Value);
end;

function TCustomUniDataSet.GetObject(const FieldName: string): TDBObject;
var
  Field: TFieldDesc;
  IsBlank: boolean;
  RecBuf: TRecordBuffer;
  DataBuf: IntPtr;
  DataLen: Word;
  Obj: TObject;
begin
  if GetActiveRecBuf(RecBuf) then begin
    Field := Data.FieldByName(FieldName);
    if not Field.IsObject then
      DatabaseError('Field type should be Object');

    DataBuf := nil;
    Data.GetField(Field, RecBuf, @DataBuf, DataLen, False, IsBlank);
    Obj := GetGCHandleTarget(DataBuf);
    if Obj is TDBObject then
      Result := TDBObject(Obj)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TCustomUniDataSet.GetArray(const FieldName: string): TDBObject;
var
  Field: TFieldDesc;
  IsBlank: boolean;
  RecBuf: TRecordBuffer;
  DataBuf: IntPtr;
  DataLen: Word;
  Obj: TObject;
begin
  if GetActiveRecBuf(RecBuf) then begin
    Field := Data.FieldByName(FieldName);
    if Field.DataType <> dtArray then
      DatabaseError('Field type should be Array');

    DataBuf := nil;
    Data.GetField(Field, RecBuf, @DataBuf, DataLen, False, IsBlank);
    Obj := GetGCHandleTarget(DataBuf);
    if Obj is TDBObject then
      Result := TDBObject(Obj)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TCustomUniDataSet.FindProvider: TUniProvider;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then
    Result := TUniConnection(vConnection).FProvider
  else
    Result := nil;
end;

function TCustomUniDataSet.GetProvider: TUniProvider;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniConnection(vConnection).GetProvider;
end;

procedure TCustomUniDataSet.CheckInactive;
begin
  inherited CheckInactive;
end;

procedure TCustomUniDataSet.SetIRecordSet(Value: TData);
begin
  inherited;

  if FICommand <> nil then
    GetProvider.SetObjectProps(FICommand, FSpecificOptions.Values);

  if FIRecordSet <> nil then begin
    FIRecordSet.SetProp(prLockFetchAll, FLockFetchAll);
    GetProvider.SetObjectProps(FIRecordSet, FSpecificOptions.Values);
  end;
end;

procedure TCustomUniDataSet.CreateCommand;
begin
  SetCommand(TUniSQL.Create(Self));
end;

function TCustomUniDataSet.GetDataSetServiceClass: TDataSetServiceClass;
begin
  Result := GetProvider.GetDataSetServiceClass;
end;

procedure TCustomUniDataSet.SetDataSetService(Value: TDataSetService);
begin
  inherited;

  if FDataSetService <> nil then
    GetProvider.SetObjectProps(FDataSetService, FSpecificOptions.Values);
end;

function TCustomUniDataSet.CreateOptions: TDADataSetOptions;
begin
  Result := TUniDataSetOptions.Create(Self);
end;

{$IFNDEF STD}
function TCustomUniDataSet.CreateEncryption: TDAEncryption;
begin
  Result := TUniEncryption.Create(Self);
end;
{$ENDIF}

function TCustomUniDataSet.NeedComplexUpdateFieldDefList: boolean;
var
  vProvider: TUniProvider;
begin
  vProvider := FindProvider;
  if vProvider<> nil then
    Result := vProvider.NeedComplexUpdateFieldDefList
  else
    Result := inherited NeedComplexUpdateFieldDefList;
end;


function TCustomUniDataSet.GetTransaction: TDATransaction;
var
  vConnection: TCustomDAConnection;
begin
  Result := inherited GetTransaction;

  if not OldTransactionBehaviour then
    if (Result = nil) and not (csAncestor in ComponentState) then begin
      vConnection := UsedConnection;
      if (vConnection <> nil) and (TUniConnection(vConnection).DefaultTransaction <> nil) then
        Result := TUniConnection(vConnection).DefaultTransaction
    end;
end;

procedure TCustomUniDataSet.SetTransaction(Value: TDATransaction);
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if (FFixedUsedTransaction <> nil) and (Value <> FFixedUsedTransaction) and
    (vConnection <> nil) and TUniConnection(vConnection).IsMultipleTransactionsSupported
  then begin
    Disconnect;

    if not Prepared and ((FIRecordSet = nil) or not FIRecordSet.Active) then
      FFixedUsedTransaction := nil;
  end;

  inherited;
end;

function TCustomUniDataSet.UsedTransaction: TDATransaction;
begin
  if FFixedUsedTransaction <> nil then
    Result := FFixedUsedTransaction
  else
    Result := inherited UsedTransaction;
end;

procedure TCustomUniDataSet.BeginConnection(NoConnectCheck: boolean = True);
var
  vProvider: TUniProvider;
begin
  inherited;

  vProvider := GetProvider;
  if FSpecificOptions.IsModified then begin
    vProvider.SetObjectProps(FIRecordSet, FSpecificOptions.Values);
    vProvider.SetObjectProps(FDataSetService, FSpecificOptions.Values);
    FSpecificOptions.IsModified := False;
  end;

  if FFixedUsedTransaction = nil then
    FFixedUsedTransaction := TUniTransaction(UsedTransaction);
end;

procedure TCustomUniDataSet.EndConnection;
begin
  inherited;

  if not Prepared and ((FIRecordSet = nil) or not FIRecordSet.Active) then
    FFixedUsedTransaction := nil;
end;

//function TCustomUniDataSet.UsedConnection: TCustomDAConnection;
//begin
//  Result := inherited UsedConnection;
//end;

{$IFNDEF FPC}
function TCustomUniDataSet.GetPSTransaction: TDATransaction;
var
  Con: TUniConnection;
begin
  Result := UsedUpdateTransaction;
  Con := TUniConnection(UsedConnection);
  if Result = Con.GetInternalDefTransaction then
    Result := Con.DefaultTransaction;
end;

function TCustomUniDataSet.PSInTransaction: Boolean;
begin
  Result := GetPSTransaction.Active;
end;

procedure TCustomUniDataSet.PSStartTransaction;
begin
  GetPSTransaction.StartTransaction;
end;
{$ENDIF}

procedure TCustomUniDataSet.Prepare;
begin
  TUniSQL(FCommand).CheckUniMacros;
  try
    inherited;
  except
    HandleInternalError;
  end;
end;

procedure TCustomUniDataSet.UnPrepare;
begin
  try
    inherited;
  except
    HandleInternalError;
  end;
end;

procedure TCustomUniDataSet.InternalExecute(Iters: integer; Offset: integer);
begin
  TUniSQL(FCommand).CheckUniMacros;
  try
    inherited;
  except
    HandleInternalError;
  end;
end;

procedure TCustomUniDataSet.OpenCursor(InfoQuery: boolean);
begin
  TUniSQL(FCommand).CheckUniMacros;
  try
    inherited;
  except
    HandleInternalError;
  end;
end;

procedure TCustomUniDataSet.CloseCursor;
begin
  try
    inherited;
  except
    HandleInternalError;
  end;

  if not Prepared then
    FFixedUsedTransaction := nil;
end;

function TCustomUniDataSet.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TCustomUniDataSet.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

{$IFNDEF STD}
function TCustomUniDataSet.GetEncryption: TUniEncryption;
begin
  Result := TUniEncryption(inherited Encryption);
end;

procedure TCustomUniDataSet.SetEncryption(Value: TUniEncryption);
begin
  Encryption.Assign(Value);
end;
{$ENDIF}

function TCustomUniDataSet.GetOptions: TUniDataSetOptions;
begin
  Result := TUniDataSetOptions(inherited Options);
end;

function TCustomUniDataSet.GetSpecificOptions: TSpecificOptionsList;
begin
  Result := FSpecificOptions.Values;
end;

procedure TCustomUniDataSet.SetOptions(Value: TUniDataSetOptions);
begin
  Options.Assign(Value);
end;

procedure TCustomUniDataSet.SetSpecificOptions(Value: TSpecificOptionsList);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TCustomUniDataSet.GetUniTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TCustomUniDataSet.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

function TCustomUniDataSet.GetUpdateTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited UpdateTransaction);
end;

procedure TCustomUniDataSet.SetUpdateTransaction(Value: TUniTransaction);
begin
  inherited UpdateTransaction := Value;
end;

function TCustomUniDataSet.GetParams: TUniParams;
begin
  Result := TUniParams(inherited Params);
end;

procedure TCustomUniDataSet.SetParams(Value: TUniParams);
begin
  inherited Params := Value;
end;

function TCustomUniDataSet.GetUpdateObject: TUniUpdateSQL;
begin
  Result := TUniUpdateSQL(inherited UpdateObject);
end;

procedure TCustomUniDataSet.SetUpdateObject(Value: TUniUpdateSQL);
begin
  inherited UpdateObject := Value;
end;

function TCustomUniDataSet.GetCRCursor: TCRCursor;
begin
  if FCursor <> nil then
    Result := FCursor
  else
    Result := inherited GetCRCursor;
end;

procedure TCustomUniDataSet.SetCRCursor(Value: TCRCursor);
begin
  if Value <> FCursor then begin
    if FCursor <> nil then
      FreeAndNil(FCursor);

    if Value is TUniCursor then begin
      FCursor := Value;
      FCursor.AddRef;
      Value := TUniCursor(Value).GetNativeCursor;
    end;

    inherited SetCRCursor(Value);
  end;
end;

procedure TCustomUniDataSet.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomUniDataSet then begin
    TCustomUniDataSet(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TCustomUniDataSet.GetFieldTypeMapClass: TFieldTypeMapClass;
var
  vProvider: TUniProvider;
begin
  vProvider := FindProvider;
  if vProvider <> nil then
    Result := vProvider.GetFieldTypeMapClass
  else
    Result := inherited GetFieldTypeMapClass;
end;

function TCustomUniDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftCursor then
    Result := TDACursorField
  else
    Result := inherited GetFieldClass(FieldType);
end;

{$IFDEF VER12P}
function TCustomUniDataSet.GetFieldClass(FieldDef: TFieldDef): TFieldClass;
begin
  Result := GetFieldClass(FieldDef.DataType);
end;
{$ENDIF}

function TCustomUniDataSet.GetParserClass: TSQLParserClass;
var
  vProvider: TUniProvider;
begin
  vProvider := FindProvider;
  if vProvider <> nil then
    Result := vProvider.GetParserClass
  else
    Result := TSQLParser;
end;

function TCustomUniDataSet.SQLAddWhere(const SQLText, Condition: string): string;
var
  vConnection: TUniConnection;
  vProvider: TUniProvider;
  UniCondition: string;
begin
  vProvider := FindProvider;
  if vProvider <> nil then begin
    UniCondition := Condition;
    if TUniSQL(FCommand).FEnableUniSQL then begin
      vConnection := TUniConnection(UsedConnection);
      if vConnection <> nil then
        vConnection.ExpandMacros(UniCondition);
    end;
    Result := _AddWhere(SQLText, UniCondition, vProvider.GetParserClass, False, MacroChar);
  end
  else
    Result := _AddWhere(SQLText, Condition, TSQLParser, False, MacroChar);
end;

function TCustomUniDataSet.SQLSetOrderBy(const SQLText, Fields: string): string;
var
  vConnection: TUniConnection;
  vProvider: TUniProvider;
  UniFields: string;
begin
  vProvider := FindProvider;
  if vProvider <> nil then begin
    UniFields := Fields;
    if TUniSQL(FCommand).FEnableUniSQL then begin
      vConnection := TUniConnection(UsedConnection);
      if vConnection <> nil then
        vConnection.ExpandMacros(UniFields);
    end;
    Result := _SetOrderBy(SQLText, UniFields, vProvider.GetParserClass);
  end
  else
    Result := _SetOrderBy(SQLText, Fields, TSQLParser);
end;

procedure TCustomUniDataSet.SetLockFetchAll(Value: boolean);
begin
  if Value <> FLockFetchAll then begin
    FLockFetchAll := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prLockFetchAll, Value);
  end;
end;

function TCustomUniDataSet.GetFetchAll: boolean;
var
  Value: Variant;
begin
  if FIRecordSet <> nil then begin
    FIRecordSet.GetProp(prFetchAll, Value);
    Result := Value;
  end
  else
    Result := False;
end;

procedure TCustomUniDataSet.SetFetchAll(Value: boolean);
var
  OldLock: boolean;
begin
  OldLock := LockFetchAll;
  LockFetchAll := False;
  try
    inherited;
  finally
    LockFetchAll := OldLock;
  end;
end;

function TCustomUniDataSet.GetNonBlocking: boolean;
var
  Value: Variant;
begin
  if FICommand <> nil then begin
    FICommand.GetProp(prNonBlocking, Value);
    Result := Value;
  end
  else
    Result := False;
end;

procedure TCustomUniDataSet.QuickOpen(var Info: TQuickOpenInfo; Refresh: boolean = False);
var
  OldLock: boolean;
begin
  OldLock := LockFetchAll;
  LockFetchAll := True;
  try
    inherited;
  finally
    LockFetchAll := OldLock;
  end;
end;

procedure TCustomUniDataSet.Restore(const Info: TQuickOpenInfo; RestoreActive: boolean = True);
var
  OldLock: boolean;
begin
  OldLock := LockFetchAll;
  LockFetchAll := True;
  try
    inherited;
  finally
    LockFetchAll := OldLock;
  end;
end;

{ TUniParams }

constructor TUniParams.Create(Owner: TPersistent);
begin
  inherited Create(TUniParam);

  FOwner := Owner;
  FNeedsUpdateItem := True;
end;

function TUniParams.GetOwner: TPersistent; // CLR cross-assembly
begin
  Result := FOwner;
end;

procedure TUniParams.SetOwner(Owner: TPersistent);
begin
  FOwner := Owner;
end;

function TUniParams.GetItem(Index: integer): TUniParam;
begin
  Result := TUniParam(inherited Items[Index]);
end;

procedure TUniParams.SetItem(Index: integer; Value: TUniParam);
begin
  inherited Items[Index] := Value;
end;

procedure TUniParams.Update(Item: TCollectionItem);
begin
  if FOwner <> nil then
    TUniSQL(FOwner).ResetParamRefs;

  inherited;
end;

function TUniParams.ParamByName(const Value: string): TUniParam;
var
  vProvider: TUniProvider;
begin
  if FOwner <> nil then
    vProvider := TUniSQL(FOwner).FindProvider
  else
    vProvider := nil;
  if vProvider <> nil then
    Result := TUniParam(inherited ParamByName(vProvider.CheckParamName(Value)))
  else
    Result := TUniParam(inherited ParamByName(Value));
end;

function TUniParams.FindParam(const Value: string): TUniParam;
var
  vProvider: TUniProvider;
begin
  if FOwner <> nil then
    vProvider := TUniSQL(FOwner).FindProvider
  else
    vProvider := nil;
  if vProvider <> nil then
    Result := TUniParam(inherited FindParam(vProvider.CheckParamName(Value)))
  else
    Result := TUniParam(inherited FindParam(Value));
end;

{ TUniTransaction }

constructor TUniTransaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSpecificOptions := TUniTransactionSpecificOptions.Create(Self, GetProvider);
end;

destructor TUniTransaction.Destroy;
begin
  FSpecificOptions.Free;

  inherited;
end;

procedure TUniTransaction.AssignTo(Dest: TPersistent);
begin
  if Dest is TUniTransaction then begin
    TUniTransaction(Dest).DefaultConnection := DefaultConnection;
    TUniTransaction(Dest).IsolationLevel := IsolationLevel;
    TUniTransaction(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TUniTransaction.GetProvider: TUniProvider;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniConnection(vConnection).GetProvider;
end;

function TUniTransaction.GetITransactionClass: TCRTransactionClass;
var
  vConnection: TCustomDAConnection;
begin
  if TransactionType = ttNative then begin
    vConnection := UsedConnection;
    if vConnection = nil then
      DatabaseError(SConnectionNotDefined);

    Result := TUniConnection(vConnection).GetITransactionClass;
  end
  else
    Result := inherited GetITransactionClass;
end;

procedure TUniTransaction.SetITransaction(Value: TCRTransaction);
begin
  inherited;

  if (UsedConnection <> nil) and (FITransaction <> nil) then
    GetProvider.SetObjectProps(FITransaction, FSpecificOptions.Values);
end;

function TUniTransaction.SQLMonitorClass: TClass;
begin
  Result := TUniSQLMonitor;
end;

procedure TUniTransaction.CheckActive;
begin
  inherited;
end;

procedure TUniTransaction.CheckInactive;
begin
  inherited;
end;

procedure TUniTransaction.Commit;
begin
  if (DefaultConnection <> nil) and DefaultConnection.IsMultipleTransactionsSupported then
    CheckActive;

  inherited;
end;

procedure TUniTransaction.Rollback;
begin
  if (DefaultConnection <> nil) and DefaultConnection.IsMultipleTransactionsSupported then
    CheckActive;

  inherited;
end;

procedure TUniTransaction.CommitRetaining;
begin
  DoCommitRetaining;
end;

procedure TUniTransaction.RollbackRetaining;
begin
  DoRollbackRetaining;
end;

procedure TUniTransaction.Savepoint(Name: string);
begin
  DoSavepoint(Name);
end;

procedure TUniTransaction.ReleaseSavePoint(Name: string);
begin
  DoReleaseSavePoint(Name);
end;

procedure TUniTransaction.RollbackToSavepoint(Name: string);
begin
  DoRollbackToSavepoint(Name);
end;

procedure TUniTransaction.DoSavepoint(const Name: string);
begin
  if (DefaultConnection <> nil) and DefaultConnection.IsMultipleTransactionsSupported then
    CheckActive;

  inherited;
end;

procedure TUniTransaction.DoReleaseSavePoint(const Name: string);
begin
  if (DefaultConnection <> nil) and DefaultConnection.IsMultipleTransactionsSupported then
    CheckActive;
  inherited;
end;

procedure TUniTransaction.DoRollbackToSavepoint(const Name: string);
begin
  if (DefaultConnection <> nil) and DefaultConnection.IsMultipleTransactionsSupported then
    CheckActive;

  inherited;
end;

procedure TUniTransaction.AddConnection(Connection: TUniConnection);
begin
  DoAddConnection(Connection);
end;

procedure TUniTransaction.RemoveConnection(Connection: TUniConnection);
begin
  DoRemoveConnection(Connection);
end;

function TUniTransaction.GetDefaultConnection: TUniConnection;
begin
  Result := TUniConnection(inherited DefaultConnection);
end;

procedure TUniTransaction.SetDefaultConnection(Value: TUniConnection);
begin
  inherited DefaultConnection := Value;
end;

function TUniTransaction.GetConnection(Index: integer): TUniConnection;
begin
  Result := TUniConnection(inherited Connections[Index]);
end;

function TUniTransaction.GetSpecificOptions: TSpecificOptionsList;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniTransaction.SetSpecificOptions(const Value: TSpecificOptionsList);
begin
  FSpecificOptions.Values.Assign(Value);
end;

{ TUniTable }

constructor TCustomUniTable.Create(Owner: TComponent);
begin
  inherited;

  LockMode := lmOptimistic;
end;

procedure TCustomUniTable.SetDataSetService(Value: TDataSetService);
begin
  inherited;

  if (FDataSetService <> nil) and FSQLIsPrepared then
    SQL.Clear;
end;

procedure TCustomUniTable.PrepareSQL;
begin
  if SQL.Count = 0 then begin
    if TableName = '' then
      DatabaseError(STableNameNotDefined);

    CheckIRecordSet;
    CheckDataSetService;
    SQL.Text := TDADataSetService(FDataSetService).SQLGenerator.GenerateTableSQL(TableName, OrderFields);
    FSQLIsPrepared := True;
  end;
end;

procedure TCustomUniTable.Prepare;
begin
  // User can select provider at this point.
  // It is needed for PrepareSQL
  BeginConnection;
  try
    PrepareSQL;

    inherited;
  finally
    EndConnection;
  end;
end;

procedure TCustomUniTable.Execute;
begin
  BeginConnection;
  try
    PrepareSQL;

    inherited;
  finally
    EndConnection;
  end;
end;

procedure TCustomUniTable.EmptyTable;
begin
  inherited EmptyTable(TableName);
end;

procedure TCustomUniTable.SetTableName(const Value: string);
begin
  if Value <> FTableName then begin
    if not (csLoading in ComponentState) then begin
      SQL.Clear;
      if (Connection <> nil) and (Connection.FProvider <> nil) then begin
        CheckDataSetService;
        FDataSetService.ResetTableKeyFields;
      end;
    end;

    FTableName := Trim(Value);
    FSQLIsPrepared := False;
  end;
end;

procedure TCustomUniTable.SetOrderFields(const Value: string);
var
  OldActive: boolean;
begin
  if Value <> FOrderFields then begin
    OldActive := Active;

    FOrderFields := Value;
    FSQLIsPrepared := False;

    if not (csLoading in ComponentState) then
      SQL.Clear;

    if OldActive then
      Open;
  end;
end;

{$IFNDEF FPC}
{ IProviderSupport }

function TCustomUniTable.PSGetTableName: string;
begin
  Result := AnsiUpperCase(TableName); // SQLResolver quotes table name
end;

procedure TCustomUniTable.PSSetParams(AParams: DB.TParams);
var
  St: string;
  i: integer;
begin
  if (Params.Count <> AParams.Count) then begin
    SQL.Text := '';
    St := '';

    for i := 0 to AParams.count - 1 do begin
      if St <> '' then
        St := St + ' AND ';
      St := St + AParams[i].Name + ' = :' + AParams[i].Name;
    end;

    PrepareSQL;

    if St <> '' then
      AddWhere(St);
  end;

  inherited;
end;

procedure TCustomUniTable.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    TableName := CommandText;
end;
{$ENDIF}

procedure TCustomUniTable.OpenCursor(InfoQuery: boolean);
begin
  // User can select provider at this point.
  // It is need for PrepareSQL
  BeginConnection;
  try
    PrepareSQL;

    inherited;
  finally
    EndConnection;
  end;
end;

procedure TCustomUniTable.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomUniTable then begin
    TCustomUniTable(Dest).SQL.Text := SQL.Text;
    TCustomUniTable(Dest).FTableName := TableName;
    TCustomUniTable(Dest).FOrderFields := OrderFields;
  end;
end;

procedure TCustomUniTable.CheckSQL;
begin
  PrepareSQL;
end;

procedure TCustomUniTable.SetFilterSQL(const Value: string);
begin
  if SQL.Count = 0 then
    FFilterSQL := Value
  else
    inherited;
end;

function TCustomUniTable.GetFinalSQL: string;
begin
  {if (TableName <> '') and not (csLoading in ComponentState) then
    PrepareSQL;} // GetFinalSQL is called from PrepareSQL\BeginConnection

  Result := inherited GetFinalSQL;
end;

{ TUniStoredProc }

procedure TUniStoredProc.AssignTo(Dest: TPersistent);
begin
  if Dest is TUniStoredProc then begin
    TUniStoredProc(Dest).SQL.Text := SQL.Text;
    TUniStoredProc(Dest).FStoredProcName := FStoredProcName;
    TUniStoredProc(Dest).FIsQuery := FIsQuery;
  end;

  inherited;
end;

procedure TUniStoredProc.PrepareSQL(IsQuery: boolean = False);
begin
  if (IsQuery <> FIsQuery) or GetForceSPInit or (SQL.Count = 0) then begin
    if StoredProcName = '' then
      DatabaseError(SStoredProcNotDefined);

    InternalCreateProcCall(StoredProcName, Params.Count = 0, IsQuery);
    FIsQuery := IsQuery;
  end;
end;

procedure TUniStoredProc.Prepare;
begin
  if not Prepared then
    PrepareSQL(False);

  inherited;
end;

procedure TUniStoredProc.CreateCommand;
begin
  inherited;

  TUniSQL(FCommand).FEnableUniSQL := False;
end;

procedure TUniStoredProc.DoBeforeExecute;
begin
  if GetForceSPInit or (SQL.Count = 0) then
    PrepareSQL(False);

  inherited;
end;

procedure TUniStoredProc.BeforeOpenCursor(InfoQuery: boolean);
begin
  if GetForceSPInit or (SQL.Count = 0) then
    PrepareSQL(True);

  inherited;
end;

procedure TUniStoredProc.ExecProc;
begin
  Execute;
end;

procedure TUniStoredProc.SetStoredProcName(const Value: string);
begin
  if Value <> FStoredProcName then begin
    if not (csReading in ComponentState) then
      SQL.Text := '';
    FStoredProcName := Trim(Value);
  end;
end;

{$IFNDEF FPC}
procedure TUniStoredProc.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    StoredProcName := CommandText;
end;
{$ENDIF}

{ TUniUpdateSQL }

function TUniUpdateSQL.DataSetClass: TCustomDADataSetClass;
begin
  Result := TCustomUniDataSet;
end;

function TUniUpdateSQL.SQLClass: TCustomDASQLClass;
begin
  Result := TUniSQL;
end;

{ TUniMetaData }

function TUniMetaData.UsedTransaction: TDATransaction;
begin
  if FFixedUsedTransaction <> nil then
    Result := FFixedUsedTransaction
  else
    Result := inherited UsedTransaction;
end;

procedure TUniMetaData.SetTransaction(Value: TDATransaction);
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if (FFixedUsedTransaction <> nil) and (Value <> FFixedUsedTransaction) and
     (vConnection <> nil) and TUniConnection(vConnection).IsMultipleTransactionsSupported
  then begin
    Close;

    if not Prepared and not Active then
      FFixedUsedTransaction := nil;
  end;

  inherited;
end;

procedure TUniMetaData.BeginConnection;
begin
  inherited;

  if FFixedUsedTransaction = nil then
    FFixedusedTransaction := UsedTransaction;
end;

procedure TUniMetaData.EndConnection;
begin
  inherited;

  if not Prepared and not Active then
    FFixedUsedTransaction := nil;
end;

procedure TUniMetaData.CloseCursor;
begin
  inherited;

  if not Prepared then
    FFixedUsedTransaction := nil;
end;

function TUniMetaData.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniMetaData.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniMetaData.GetUniTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TUniMetaData.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

{ TUniParam }

function TUniParam.GetConnection: TUniConnection;
var
  Owner: TPersistent;
begin
  Owner := TUniParams(GetOwner).GetOwner;
  if (Owner <> nil) and (Owner is TUniSQL) then
    Result := TUniConnection(TUniSQL(Owner).UsedConnection)
  else
    Result := nil;
end;

function TUniParam.GetAsObject: TDBObject;
begin
  if DataType = ftUnknown then
    DataType := ftADT;

  if IsObject then
    Result := ParamObject as TDBObject
  else
    Result := nil;
end;

procedure TUniParam.SetAsObject(Value: TDBObject);
begin
  inherited DataType := ftADT;

  ParamObject := Value;
end;

function TUniParam.GetAsArray: TDBObject;
begin
  if DataType = ftUnknown then
    DataType := ftArray;

  if IsArray then
    Result := ParamObject as TDBObject
  else
    Result := nil;
end;

procedure TUniParam.SetAsArray(Value: TDBObject);
begin
  inherited DataType := ftArray;

  ParamObject := Value;
end;

function TUniParam.CreateObject: TSharedObject;

  function GetCRTransaction: TCRTransaction;
  var
    Owner: TPersistent;
    Transaction: TUniTransaction;
  begin
    Owner := TUniParams(GetOwner).GetOwner;
    if (Owner <> nil) and (Owner is TUniSQL) then begin
      Transaction := TUniTransaction(TUniSQL(Owner).UsedTransaction);
      if Transaction <> nil then
        Result := Transaction.FITransaction
      else
        Result := nil
    end
    else
      Result := nil;
  end;

var
  vConnection: TUniConnection;
  Provider: TUniProvider;
  InternalType: Word;
  NeedUnicode: boolean;
begin
  case DataType of
    ftCursor:
      Result := TUniCursor.Create;
  else
    if IsBlob then begin
      Result := TUniBlob.Create;

      vConnection := GetConnection;
      if vConnection <> nil then
        Provider := TUniConnection(vConnection).GetProvider
      else
        Provider := nil;

      if Provider <> nil then
        NeedUnicode := Provider.NeedBlobUnicode(Self)
      else
        NeedUnicode := NeedBlobUnicode;

      // if False do nothing (old behavior)
      if NeedUnicode then
        TUniBlob(Result).IsUnicode := True;
    end
    else if IsObject then begin
      vConnection := GetConnection;
      if vConnection <> nil then begin
        InternalType := vConnection.GetFieldTypeMapClass.GetDataType(DataType);
        Result := vConnection.FIConnection.CreateObject(InternalType, GetCRTransaction);
      end
      else
        Result := nil;
    end
    else
      Result := inherited CreateObject;
  end;
end;

function TUniParam.GetNativeParamObject(SourceObject: TSharedObject): TSharedObject;
var
  Provider: TUniProvider;
  UniBlob: TUniBlob;
  UniCursor: TUniCursor;
  ClassType: TClass;
  vConnection: TUniConnection;
begin
  if (SourceObject is TUniBlob) or (SourceObject is TUniCursor) then begin
    vConnection := GetConnection;
    if vConnection <> nil then
      Provider := TUniConnection(vConnection).GetProvider
    else
      Provider := nil;

    if Provider = nil then
      DatabaseError(SConnectionNotDefined);

    ClassType := Provider.GetParamObjectClass(Self);

    Result := nil; // to suppress warning
    if SourceObject is TUniBlob then begin
      UniBlob := TUniBlob(SourceObject);
      if (UniBlob.GetNativeBlob = nil) or (UniBlob.GetNativeBlob.ClassType <> ClassType) then
        UniBlob.SetNativeBlob(Provider.CreateParamObject(Self, UniBlob.IsUnicode) as TBlob);
      Result := UniBlob.GetNativeBlob;
    end
    else
    if SourceObject is TUniCursor then begin
      UniCursor := TUniCursor(SourceObject);
      if (UniCursor.GetNativeCursor = nil) or (UniCursor.GetNativeCursor.ClassType <> ClassType) then
        UniCursor.SetNativeCursor(Provider.CreateParamObject(Self, False) as TCRCursor);
      Result := UniCursor.GetNativeCursor;
    end
  end
  else
    Result := inherited GetNativeParamObject(SourceObject);
end;

function TUniParam.GetNativeParamObject: TSharedObject;
begin
  Result := GetNativeParamObject(GetAsBlobRef);
end;

function TUniParam.IsBlobDataType(DataType: TFieldType): boolean;
begin
  Result := inherited IsBlobDataType(DataType) or
    (DataType in [ftOraBlob, ftOraClob]);
end;

function TUniParam.IsSharedObjectDataType(DataType: TFieldType): boolean;
begin
  Result := inherited IsSharedObjectDataType(DataType) or
    (DataType in [ftOraBlob, ftOraClob, ftCursor, ftADT, {$IFDEF VER14P}ftObject,{$ENDIF} ftArray]);
end;

function TUniParam.IsObjectDataType(DataType: TFieldType): boolean;
var
  Connection: TUniConnection;
begin
  if DataType in [{$IFDEF VER14P}ftObject,{$ENDIF} ftADT, ftReference, ftArray] then begin
    Connection := GetConnection;
    if Connection <> nil then
      Result := Connection.GetFieldTypeMapClass.GetDataType(DataType) <> dtUnknown
    else
      Result := False
  end
  else
    Result := False;
end;

function TUniParam.IsObject: boolean;
begin
  Result := IsObjectDataType(DataType);
end;

function TUniParam.IsArrayDataType(DataType: TFieldType): boolean;
var
  Connection: TUniConnection;
begin
  if DataType in [ftArray] then begin
    Connection := GetConnection;
    if Connection <> nil then
      Result := Connection.GetFieldTypeMapClass.GetDataType(DataType) <> dtUnknown
    else
      Result := False
  end
  else
    Result := False;
end;

function TUniParam.IsArray: boolean;
begin
  Result := IsArrayDataType(DataType);
end;

function TUniParam.GetIsNull: boolean;
var
  Obj: TSharedObject;
begin
  if IsObject then begin
    Obj := ParamObject;
    if Obj is TDBObject then
      Result := TDBObject(Obj).IsNull
    else
      Result := inherited GetIsNull;
  end
  else
    Result := inherited GetIsNull;
end;

{ TUniBlob }

destructor TUniBlob.Destroy;
begin
  FNativeBlob.Free;

  inherited;
end;

procedure TUniBlob.Disconnect;
begin
  if FNativeBlob <> nil then
    FNativeBlob.Disconnect;
end;

function TUniBlob.GetNativeBlob: TBlob;
begin
  Result := FNativeBlob;
end;

procedure TUniBlob.SetNativeBlob(Value: TBlob);
begin
  if Value <> FNativeBlob then begin
    FNativeBlob.Free;
    FNativeBlob := Value;

    if FNativeBlob <> nil then
      FNativeBlob.SetData(FData);
  end;
end;

{ TUniCursor }

destructor TUniCursor.Destroy;
begin
  FNativeCursor.Free;

  inherited;
end;

function TUniCursor.GetNativeCursor: TCRCursor;
begin
  Result := FNativeCursor;
end;

procedure TUniCursor.SetNativeCursor(Value: TCRCursor);
begin
  if Value <> FNAtiveCursor then begin
    FNativeCursor.Free;
    FNativeCursor := Value;
  end;
end;

procedure TUniCursor.Disconnect;
begin
  if FNativeCursor <> nil then
    FNativeCursor.Disconnect;
end;

function TUniCursor.CanFetch: boolean;
begin
  if FNativeCursor = nil then
    Result := False
  else
    Result := FNativeCursor.CanFetch;
end;

{ TUniUtils }

class function TUniUtils.GetProvider(Connection: TUniConnection): TUniProvider;
begin
  Connection.CheckProvider;
  Result := Connection.FProvider;
end;

class function TUniUtils.CanGetProvider(Connection: TUniConnection): boolean;
begin
  Result := (Connection <> nil) and Connection.CanGetProvider;
end;

class function TUniUtils.GetCRConnection(Connection: TUniConnection): TCRConnection;
begin
  Result := Connection.FIConnection;
end;

procedure GetServerList(const ProviderName: string; List: TStrings; SpecificOptions: TStrings = nil);
var
  Provider: TUniProvider;
  ServerEnumerator: TCRServerEnumerator;
begin
  if ProviderName = '' then
    DatabaseError(SProviderNotDefined);

  Provider := UniProviders.GetProvider(ProviderName);

  if Provider = nil then
    CheckProviderName(ProviderName);

  ServerEnumerator := Provider.GetServerEnumeratorClass.Create;
  try
    if SpecificOptions <> nil then
      Provider.SetObjectProps(ServerEnumerator, SpecificOptions);

    ServerEnumerator.GetServerList(List);
  finally
    ServerEnumerator.Free;
  end;
end;

initialization

finalization

end.

