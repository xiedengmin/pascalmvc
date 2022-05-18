/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  DB Access
//  Created:            01.07.00
//////////////////////////////////////////////////

{$I Dac.inc}
unit DBAccess;

{$IFDEF NODBACCESS}
  Error
{$ENDIF}


interface

{$IFDEF FPC}
  {$DEFINE OWN_CLIENTS_REGISTRATION}
{$ENDIF}
{$IFDEF AUTOREFCOUNT}
  {$DEFINE OWN_CLIENTS_REGISTRATION}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  Classes, Types, SysUtils, StrUtils, TypInfo, SyncObjs,
  Variants, FmtBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF} DB,
  CLRClasses, CRXml,
{$IFNDEF FPC}
  DBCommon, DBConsts,
{$ELSE}
  DBConst,
{$ENDIF}
{$IFDEF VER17P}
  Generics.Collections,
{$ENDIF}
  MemData, MemUtils, {$IFDEF FPC}MemDataSet,{$ELSE}MemDS,{$ENDIF}
  CRTypes, CRAccess, CRParser, CRVio, CRDataTypeMap, CREncryption,
  CRConnectionString, CRServerEnumerator, CRConnectionPool,
  DASQLGenerator, DAConsts;

const
  OperationsStackDelta = 50;

type
  TCheckMode = (cmNone, cmException, cmRefresh);

  TDAFieldTypeMap = class;
  TCustomDAConnection = class;
  TDATransaction = class;
  TDATransactions = class;
  TCustomDASQL = class;
  TCustomDADataSet = class;
  TDAMetaData = class;
  TDAFieldTypeMapClass = class of TDAFieldTypeMap;
  TCustomDASQLClass = class of TCustomDASQL;
  TCustomDADataSetClass = class of TCustomDADataSet;
  TDADataSetUpdater = class;
  TDADataSetService = class;
  TDADataSetServiceClass = class of TDADataSetService;
  TDADataSetUpdaterClass = class of TDADataSetUpdater;

  TCustomDAUpdateSQL = class;
  TDAParam = class;
  TMacro = class;
  TMacros = class;
  TCustomConnectDialog = class;
  TConnectDialogClass = class of TCustomConnectDialog;

{$IFDEF FPC}
  TConnectChangeEvent = procedure(Sender: TObject; Connecting: Boolean) of object;
{$ENDIF}

{ EDAError }

  EDAError = class (EDatabaseError)
  protected
    FErrorCode: integer;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FComponent: TObject;
  public
    constructor Create(ErrorCode: integer; const Msg: string);

    function IsFatalError: boolean; virtual;
    function IsKeyViolation: boolean; virtual;

    property ErrorCode: integer read FErrorCode;
    property Component: TObject read FComponent write FComponent;
  end;

{ TFieldTypeInfo }

  TFieldTypeInfo = class
  private
    FFieldType: TFieldType;
    FName: string;
    FLength: boolean;
    FScale: boolean;
  protected
  public
    constructor Create(FieldType: TFieldType; const Name: string; Length, Scale: boolean);

    property FieldType: TFieldType read FFieldType;
    property Name: string read FName;
    property Length: boolean read FLength;
    property Scale: boolean read FScale;
  end;

{ TFieldTypeInfos }

  TFieldTypeInfos = class
  private
    FTypeInfos: TCRObjectList;

    function GetCount: Integer;
    function GetTypeInfo(Index: integer): TFieldTypeInfo;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(FieldType: TFieldType; const Name: string; Length, Scale: boolean);
    procedure Delete(Index: Integer);
    procedure Clear;

    function Check(FieldType: TFieldType; Length, Scale: Integer): Exception;

    function FindTypeInfo(FieldType: TFieldType): TFieldTypeInfo; overload;
    function FindTypeInfo(const Name: string): TFieldTypeInfo; overload;

    property TypeInfos[Index: integer]: TFieldTypeInfo read GetTypeInfo;
    property Count: Integer read GetCount;
  end;

{ TDAFieldTypeMap }

  TDAFieldTypeMap = class (TFieldTypeMap)
  public
    class function GetFieldTypeInfos: TFieldTypeInfos; virtual;
  end;

{ TCustomDAConnection }
  TRetryMode = (rmRaise, rmReconnect, rmReconnectExecute);

  TFailOverOperation = record
    Operation: TConnLostCause;
    AllowFailOver: boolean;
  end;
  TOperationsStack = array of TFailOverOperation;// executed operations stack used to track dowm connection lost cause

  TDAConnectionErrorEvent = procedure (Sender: TObject; E: EDAError; var Fail: boolean) of object;
  TConnectionLostEvent = procedure (Sender: TObject; Component: TComponent; ConnLostCause: TConnLostCause;
                           var RetryMode: TRetryMode) of object;
  TDAConnectionLoginEvent = procedure(Connection: TCustomDAConnection; LoginParams: TStrings) of object;

  TDAConnectionOptions = class (TPersistent)
  private
    FKeepDesignConnected: boolean;
    FDisconnectedMode: boolean;
    FLocalFailover: boolean;
    FDefaultSortType: TSortType;
    FEnableBCD: boolean;
    FEnableFMTBCD: boolean;
    FAllowImplicitConnect: boolean;
    FUuidWithBraces: boolean;

    procedure SetDisconnectedMode(Value: boolean);
    procedure SetDefaultSortType(Value: TSortType);
    procedure SetEnableBCD(Value: boolean);
    procedure SetEnableFMTBCD(Value: boolean);
    procedure SetUuidWithBraces(const Value: boolean);

  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCustomDAConnection;

    procedure AssignTo(Dest: TPersistent); override;

    property EnableBCD: boolean read FEnableBCD write SetEnableBCD default False;
    property EnableFMTBCD: boolean read FEnableFMTBCD write SetEnableFMTBCD default False;
    property UuidWithBraces: boolean read FUuidWithBraces write SetUuidWithBraces default True;

  public
    constructor Create(Owner: TCustomDAConnection);

    property DisconnectedMode: boolean read FDisconnectedMode write SetDisconnectedMode default False;
    property KeepDesignConnected: boolean read FKeepDesignConnected write FKeepDesignConnected default True;
    property LocalFailover: boolean read FLocalFailover write FLocalFailover default False;
    property DefaultSortType: TSortType read FDefaultSortType write SetDefaultSortType default stCaseSensitive;

  published
    property AllowImplicitConnect: boolean read FAllowImplicitConnect write FAllowImplicitConnect default True;
  end;

  TPoolingOptions = class(TPersistent)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCustomDAConnection;
    FMaxPoolSize: integer;
    FMinPoolSize: integer;
    FConnectionLifetime: integer;
    FValidate: boolean;

    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TCustomDAConnection); virtual;

  published
    property MaxPoolSize: integer read FMaxPoolSize write FMaxPoolSize default DefValMaxPoolSize;
    property MinPoolSize: integer read FMinPoolSize write FMinPoolSize default DefValMinPoolSize;
    property ConnectionLifetime: integer read FConnectionLifetime write FConnectionLifetime default DefValConnectionLifetime;
    property Validate: boolean read FValidate write FValidate default DefValValidate;
  end;

  TDAConnectionSSLOptions = class (TPersistent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCustomDAConnection;
    FCipherList: string;
    FCACert: string;
    FKey: string;
    FCert: string;

    procedure SetCipherList(const Value: string);
    procedure SetCACert(const Value: string);
    procedure SetKey(const Value: string);
    procedure SetCert(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadCipherList(Reader: TReader);

    property Owner: TCustomDAConnection read FOwner;
  public
    constructor Create(Owner: TCustomDAConnection); virtual;
  published
    property CipherList: string read FCipherList write SetCipherList;
    property CACert: string read FCACert write SetCACert;
    property Key: string read FKey write SetKey;
    property Cert: string read FCert write SetCert;
  end;
  TDAConnectionSSLOptionsClass = class of TDAConnectionSSLOptions;

  TDAMapRule = class(TMapRule)
  private
    FFieldType: TFieldType;

    function IsFieldTypeStored: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToRule(Dest: TDAMapRule);

    procedure DoRuleChanged;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadExtFieldType(Reader: TReader);
    procedure WriteExtFieldType(Writer: TWriter);
  public
    constructor Create(Collection: TCollection); override;
  published
    property FieldName;

    property DBType default dtUnknown;
    property DBLengthMin default rlAny;
    property DBLengthMax default rlAny;
    property DBScaleMin default rlAny;
    property DBScaleMax default rlAny;

    property FieldType: TFieldType read FFieldType write FFieldType stored IsFieldTypeStored default ftUnknown ;
    property FieldLength default rlAny;
    property FieldScale default rlAny;

    property IgnoreErrors default False;
    property Format;
  end;

  TDAMapRuleClass = class of TDAMapRule;

  TDAMapRules = class (TMapRules)
  private
    FIgnoreInvalidRules: boolean;

    function GetItem(Index: Integer): TDAMapRule;
    procedure SetItem(Index: Integer; Value: TDAMapRule);
  protected
    class function GetMapRuleClass: TDAMapRuleClass; virtual;

    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToRules(Dest: TDAMapRules);
    procedure Update(Item: TCollectionItem); override;

    function GetDataType(FieldType: TFieldType): Word; virtual; abstract;
    function GetFieldTypeInfos: TFieldTypeInfos; virtual; abstract;

    procedure WriteTo(Dest: TCRMapRules); virtual;

    function CreateParser(const Rule: string): TParser;
    function ParseRule(Parser: TParser; out FieldName: string;
      out DBType: Word;
      out DBLengthMin, DBLengthMax: Integer;
      out DBScaleMin, DBScaleMax: Integer;
      out FieldType: TFieldType;
      out FieldLength, FieldScale: Integer;
      out IgnoreErrors: boolean;
      out Format: string): boolean;
    function CheckRule(const FieldName: string;
      DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      FieldType: TFieldType; FieldLength, FieldScale: Integer): Exception; virtual;
    procedure DoAddRule(const FieldName: string; DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      FieldType: TFieldType; FieldLength, FieldScale: Integer; IgnoreErrors: boolean; const Format: string);
    procedure DoRulesChanged; virtual; abstract;
  public
    constructor Create; virtual;

    procedure AddDBTypeRule(DBType: Word;
      FieldType: TFieldType;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;
    procedure AddDBTypeRule(DBType: Word;
      FieldType: TFieldType; FieldLength: Integer;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;
    procedure AddDBTypeRule(DBType: Word;
      FieldType: TFieldType; FieldLength, FieldScale: Integer;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;
    procedure AddDBTypeRule(DBType: Word; DBLengthMin, DBLengthMax: Integer;
      FieldType: TFieldType;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;
    procedure AddDBTypeRule(DBType: Word; DBLengthMin, DBLengthMax: Integer;
      FieldType: TFieldType; FieldLength: Integer;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;
    procedure AddDBTypeRule(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      FieldType: TFieldType;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;
    procedure AddDBTypeRule(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      FieldType: TFieldType; FieldLength, FieldScale: Integer;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;

    procedure AddFieldNameRule(const FieldName: string;
      FieldType: TFieldType;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;
    procedure AddFieldNameRule(const FieldName: string;
      FieldType: TFieldType; FieldLength: Integer;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;
    procedure AddFieldNameRule(const FieldName: string;
      FieldType: TFieldType; FieldLength, FieldScale: Integer;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;

    procedure AddRule(const FieldName: string; DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      FieldType: TFieldType; FieldLength, FieldScale: Integer;
      IgnoreErrors: boolean = False; const Format: string = ''); overload;

    procedure AddRule(const Rule: string); overload;
    procedure AddRules(const Rules: string);

    property Items[Index: Integer]: TDAMapRule read GetItem write SetItem; default;
  published
    property IgnoreInvalidRules: boolean read FIgnoreInvalidRules write FIgnoreInvalidRules default False;
  end;

  TDAConnectionMapRules = class (TDAMapRules)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TCustomDAConnection;
  protected
    function GetDataType(FieldType: TFieldType): Word; override;
    function GetFieldTypeInfos: TFieldTypeInfos; override;

    function CheckRule(const FieldName: string;
      DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      FieldType: TFieldType; FieldLength, FieldScale: Integer): Exception; override;
    procedure DoRulesChanged; override;
  public
    constructor Create(Connection: TCustomDAConnection); reintroduce; virtual;

    property Connection: TCustomDAConnection read FConnection;
  end;
  TDAConnectionMapRulesClass = class of TDAConnectionMapRules;

  TDADataSetMapRules = class (TDAMapRules)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TCustomDADataSet;
  protected
    function GetDataType(FieldType: TFieldType): Word; override;
    function GetFieldTypeInfos: TFieldTypeInfos; override;

    function CheckRule(const FieldName: string;
      DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      FieldType: TFieldType; FieldLength, FieldScale: Integer): Exception; override;
    procedure DoRulesChanged; override;
  public
    constructor Create(DataSet: TCustomDADataSet); reintroduce; virtual;

    property DataSet: TCustomDADataSet read FDataSet;
  end;
  TDADataSetMapRulesClass = class of TDADataSetMapRules;

  TCustomDAConnection = class (TCustomConnection)
  private
  {$IFDEF OWN_CLIENTS_REGISTRATION}
    FClients: TList;
    FDataSets: TList;
    FConnectEvents: TList;
  {$ENDIF}
    FInProcessDisconnecting: boolean;
    FTransactions: TDATransactions;
    FUsername: string;
    FAutoCommit: boolean;
    FInProcessError: boolean;
    FConnectDialog: TCustomConnectDialog;
    FDebug: boolean;

    FOnError: TDAConnectionErrorEvent;
    FOnLogin: TDAConnectionLoginEvent;
    FConvertEOL: boolean;

    FOptions: TDAConnectionOptions;
    FPoolingOptions: TPoolingOptions;
    FPooling: boolean;
    FOnConnectionLost: TConnectionLostEvent;
    hRegisterClient: TCriticalSection;

    FDataTypeMap: TDAMapRules;

    function IsMapRulesStored: boolean;

    procedure SetDefaultTransaction(Value: TDATransaction);
    function GetDefaultTransaction: TDATransaction;
    function GetTransaction(Index: integer): TDATransaction;
    function GetTransactionsCount: integer;
    procedure SetUsername(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetAutoCommit(Value: boolean);
    procedure SetConnectDialog(Value: TCustomConnectDialog);
    procedure SetPooling(Value: boolean);
    procedure SetDebug(Value: boolean);

    procedure DoAfterConnect;
  protected
    FDefaultTransaction: TDATransaction;
    FInternalDefTransaction: TDATransaction;
    FConnectCount: integer;
    FSQLs: TList;
    FIConnection: TCRConnection;
    FStreamedConnected: boolean;
    FServer: string;
    FPassword: string;
    FShouldShowPrompt: boolean; // Disconnect mode flag that allow to avoid unnecessary Login porompt showing
    FOperationsStack: TOperationsStack ;  //FailOver support
    FOperationsStackLen: integer;
    FCommand: TCustomDASQL;
    FLockLoginPrompt: boolean;
    FConnectionStringBuilder: TCRConnectionStringBuilder;

    FIOHandler: TCRIOHandler;
    FSSLOptions: TDAConnectionSSLOptions;
    FHttpOptions: THttpOptions;
    FProxyOptions: TProxyOptions;

    procedure ClearRefs; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  {$IFNDEF LITE}
    procedure SetIOHandler(Value: TCRIOHandler);
  {$ENDIF}
    function GetSSLOptionsClass: TDAConnectionSSLOptionsClass; virtual;
    procedure SetSSLOptions(Value: TDAConnectionSSLOptions);
    procedure SetHttpOptions(Value: THttpOptions);
    procedure SetProxyOptions(Value: TProxyOptions);

  {$IFDEF OWN_CLIENTS_REGISTRATION}
    procedure SendConnectEvent(Connecting: Boolean);
    function GetDataSet(Index: Integer): TDataSet; override;
    function GetDataSetCount: Integer; override;
  {$ENDIF}

    procedure SetConnectionParameters(ConnectionParameters: TCRConnectionParameters); virtual;
    procedure SetBaseConnectionProps(Connection: TCRConnection); virtual;
    procedure SetHttpConnectionProps(Connection: TCRConnection);
    procedure SetConnectionProps(Connection: TCRConnection); virtual;
    function GetConnectionParametersClass: TCRConnectionParametersClass; virtual;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; virtual;
    function GetIConnection: TCRConnection;

    function GetIConnectionClass: TCRConnectionClass; virtual;
    function GetICommandClass: TCRCommandClass; virtual;
    function GetIRecordSetClass: TCRRecordSetClass; virtual;
    function GetIMetaDataClass: TCRMetaDataClass; virtual;
    function GetITransactionClass: TCRTransactionClass; virtual;

    function IsMultipleTransactionsSupported: boolean; virtual;
    function ApplyUpdatesInTransaction: boolean; virtual;

    procedure CreateIConnection; virtual;
    procedure FreeIConnection;
    procedure SetIConnection(Value: TCRConnection); virtual;
    function CreateICommand: TCRCommand;
    function CreateIRecordSet: TCRRecordSet;
    procedure ClearTransactionRefs;

    function GetFieldTypeMapClass: TDAFieldTypeMapClass; virtual;
    function CreateDataTypeMap: TDAConnectionMapRules; virtual;
    procedure SetDataTypeMap(Value: TDAMapRules);

    procedure Loaded; override;

    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); {$IFNDEF FPC}override;{$ENDIF}
    procedure UnRegisterClient(Client: TObject); {$IFNDEF FPC}override;{$ENDIF}

    function SQLMonitorClass: TClass; virtual; // TDASQLMonitorClass
    function ConnectDialogClass: TConnectDialogClass; virtual;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure DisconnectTransaction; virtual;
    procedure InternalConnect; virtual;
    procedure InternalDisconnect; virtual;
    procedure CheckInactive;

    function InternalGetServer: string; virtual; // for IBDAC
    function IsConnectedStored: boolean; virtual;
    function NeedPrompt: boolean; virtual;
  //Operations stack functionality
    function PushOperation(Operation: TConnLostCause; AllowFailOver: boolean = true): integer; virtual;
    function PopOperation: TConnLostCause; virtual;

    procedure ResetOnFatalError; virtual;
    procedure RestoreAfterFailOver; virtual;
    function IsFailOverAllowed: boolean; virtual;
    function DetectConnLostCause(Component: TObject): TConnLostCause; virtual;
    procedure DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean; ReconnectAttempt: integer;
      var ConnLostCause: TConnLostCause); virtual;

    procedure AssignTo(Dest: TPersistent); override;

  {$IFDEF FPC}
    procedure GetLoginParams(out ADatabaseName, AUserName, APassword: string); overload; override;
    procedure SetLoginParams(const ADatabaseName, AUserName, APassword: string); overload; override;
  {$ENDIF}
    procedure GetLoginParams(LoginParams: TStrings); overload; virtual;
    procedure SetLoginParams(LoginParams: TStrings); overload; virtual;
    function GetConnected: boolean; override;
    procedure SetConnected(Value: boolean); override;
    function GetConnectionString: string; virtual;
    procedure SetConnectionString(const Value: string); virtual;
    procedure SetServer(const Value: string); virtual;

    function DefaultTableSchema: string; virtual;

    procedure SuppressAutoCommit;
    procedure RestoreAutoCommit;
    function DetectInTransaction(CanActivate: boolean = False): boolean; virtual;
    function GetInTransaction: boolean; virtual;
    function UsedTransaction: TDATransaction; virtual;
    procedure SetConvertEOL(Value: boolean);
    procedure CheckCommand; virtual;
    procedure AssignConnectOptions(Source: TCustomDAConnection); virtual;

    function CreateOptions: TDAConnectionOptions; virtual;
    procedure SetOptions(Value: TDAConnectionOptions);
    function CreatePoolingOptions: TPoolingOptions; virtual;
    procedure SetPoolingOptions(Value: TPoolingOptions);
    function CreateConnectionStringBuilder: TCRConnectionStringBuilder; virtual;

  { Transaction control }
    function InternalAddTransaction(TR: TDATransaction): integer;
    procedure InternalRemoveTransaction(TR: TDATransaction);
    function DoAddTransaction(TR: TDATransaction): integer; virtual;
    procedure DoRemoveTransaction(TR: TDATransaction); virtual;

    procedure DoCommitRetaining; virtual;
    procedure DoRollbackRetaining; virtual;
    procedure DoSavepoint(const Name: string); virtual;
    procedure DoRollbackToSavepoint(const Name: string); virtual;
    procedure DoReleaseSavepoint(const Name: string); virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadEncryptedPassword(Reader: TReader);
    procedure ReadPassword(Reader: TReader);
    procedure WriteEncryptedPassword(Writer: TWriter);
    function Encrypt(const Value: TBytes): TBytes; virtual;
    function Decrypt(const Value: TBytes): TBytes; virtual;
    function EncryptToHex(const Value: string): string;
    function DecryptFromHex(const Value: string): string;

    function InternalGetSQL: TCustomDASQL;

    function GetConnectionStringParam(Param: integer): variant; virtual;
    procedure SetConnectionStringParam(Param: integer; const Value: variant); virtual;

    property DefaultTransaction: TDATransaction read GetDefaultTransaction write SetDefaultTransaction;
    property TransactionCount: integer read GetTransactionsCount;
    property Transactions[Index: integer]: TDATransaction read GetTransaction;
    property AutoCommit: boolean read FAutoCommit write SetAutoCommit default True;

  {$IFNDEF LITE}
    property IOHandler: TCRIOHandler read FIOHandler write SetIOHandler;
  {$ENDIF}
    property HttpOptions: THttpOptions read FHttpOptions write SetHttpOptions;
    property ProxyOptions: TProxyOptions read FProxyOptions write SetProxyOptions;
  public
    constructor Create(Owner: TComponent); overload; override;
    constructor Create(Owner: TComponent; const ConnectString: string); reintroduce; overload;
    destructor Destroy; override;

    procedure Connect; overload;
    procedure Connect(const ConnectString: string); overload;
    procedure Disconnect;
    procedure PerformConnect(Retry: boolean = False);
    procedure AssignConnect(Source: TCustomDAConnection);
    procedure Ping;

    function ParamByName(const Name: string): TDAParam;
    function ExecSQL(const Text: string): variant; overload;
    function ExecSQL(const Text: string; const Params: array of variant): variant; overload; virtual;
    function ExecSQLEx(const Text: string; const Params: array of variant): variant; virtual;
    function ExecProc(const Name: string; const Params: array of variant): variant; virtual;
    function ExecProcEx(const Name: string; const Params: array of variant): variant; virtual;

    procedure GetTableNames(List: TStrings; AllTables: boolean = False; OnlyTables: boolean = False); virtual;
    procedure GetDatabaseNames(List: TStrings); virtual;
    procedure GetStoredProcNames(List: TStrings; AllProcs: boolean = False); virtual;
    procedure GetFieldNames(const TableName: string; List: TStrings); virtual;
    procedure GetKeyFieldNames(const TableName: string; List: TStrings); virtual;

  { Transaction control }
    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    procedure ApplyUpdates; overload; virtual;
    procedure ApplyUpdates(const DataSets: array of TCustomDADataSet); overload; virtual;

    function CreateTransaction: TDATransaction; virtual;
    function CreateDataSet(AOwner: TComponent = nil): TCustomDADataSet; virtual;
    function CreateSQL: TCustomDASQL; virtual;
    function CreateMetaData: TDAMetaData; virtual;

    procedure EncryptTable(const TableName: string; Encryptor: TCREncryptor; const Fields: string); virtual;

    procedure RemoveFromPool;
    procedure MonitorMessage(const Msg: string);

    property ConnectString: string read GetConnectionString write SetConnectionString stored False;
    property Username: string read FUsername write SetUsername;
    property Password: string read FPassword write SetPassword stored False;
    property Server: string read FServer write SetServer;
    property InTransaction: boolean read GetInTransaction;
    property ConnectDialog: TCustomConnectDialog read FConnectDialog write SetConnectDialog;

    property OnError: TDAConnectionErrorEvent read FOnError write FOnError;
    property OnConnectionLost: TConnectionLostEvent read FOnConnectionLost write FOnConnectionLost;
    property OnLogin: TDAConnectionLoginEvent read FOnLogin write FOnLogin;

    {$IFDEF VER17P}[Default(DefValLoginPrompt)]{$ENDIF}
    property LoginPrompt default DefValLoginPrompt;
    property ConvertEOL: boolean read FConvertEOL write SetConvertEOL default False;
    property Debug: boolean read FDebug write SetDebug default False;

    property Options: TDAConnectionOptions read FOptions write SetOptions;
    property PoolingOptions: TPoolingOptions read FPoolingOptions write SetPoolingOptions;
    property Pooling: boolean read FPooling write SetPooling default DefValPooling;

    property DataTypeMap: TDAMapRules read FDataTypeMap write SetDataTypeMap stored IsMapRulesStored;
  end;

{ TDAConnections }
  TDAConnections = class(TList)
  private
    function GetItems(Index: integer): TCustomDAConnection;
  public
    property Items[Index: integer]: TCustomDAConnection read GetItems; default;

  end;

{ TDATransactions }
  TDATransactions = class (TList)
  private
    function GetItems(Index: Integer): TDATransaction;
  public
    property Items[Index: Integer]: TDATransaction read GetItems; default;
  end;

{ TDATransaction }

  TTransactionType = (ttNative, ttMTS);

  TDATransactionErrorEvent = procedure (Sender: TObject; E: EDAError; var Fail: boolean) of object;

  TDATransaction = class(TComponent)
  private
    procedure SetDefaultConnection(Value: TCustomDAConnection);
    procedure SetIsolationLevel(Value: TCRIsolationLevel);
    procedure SetReadOnly(Value: boolean);
    procedure SetTransactionType(Value: TTransactionType);
    function GetConnection(Index: integer): TCustomDAConnection;
    function GetConnectionsCount: integer;
    function GetActive: boolean;

  protected
    FDesignCreate: boolean;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDefaultConnection: TCustomDAConnection;

    FTrStartCount: integer;
    FUnCommitedStatementCount: integer;
    FExplicitlyStarted: boolean; // True if transaction is started by user
    FDisconnectedMode: boolean;
    FFailOverSatus: integer;
    FPrepared: boolean;

    FTransactionType: TTransactionType;
    FDefaultCloseAction: TCRTransactionAction;
    FIsolationLevel: TCRIsolationLevel;
    FReadOnly: boolean;
    FInProcessError: boolean;
    FOnError: TDATransactionErrorEvent;

    FITransaction: TCRTransaction;
    FShareTransaction: boolean; // use ITransaction from connection
    FConnections: TDAConnections;
    FOnStart: TNotifyEvent;
    FOnCommit: TNotifyEvent;
    FOnRollback: TNotifyEvent;
    FOnCommitRetaining: TNotifyEvent;
    FOnRollbackRetaining: TNotifyEvent;

    function IsInternalTrStored: boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetITransactionClass: TCRTransactionClass; virtual;
    procedure CheckITransaction;
    procedure CreateITransaction;
    procedure SetITransaction(Value: TCRTransaction); virtual;
    procedure FreeITransaction;
    procedure ClearRefs;

    function DetectInTransaction(CanActivate: boolean = False): boolean; virtual; // for ODAC
    procedure CheckActive; virtual;
    procedure CheckInactive; virtual;

    procedure Reset;
    procedure Restore;
    procedure CloseDataSets;
    procedure CloseTransaction(Force: boolean = False); virtual;
    procedure GainTransaction; virtual;
    procedure AutoCommitTransaction(NeedCommit: boolean); virtual;
    procedure ReleaseTransaction; virtual;
    function CanAutoCommitExplicitTransaction: boolean; virtual;

    function SQLMonitorClass: TClass; virtual;
    function UsedConnection: TCustomDAConnection; virtual;

    procedure PrepareTransaction(CheckOnly: boolean = False); // setup to start
    procedure UnPrepareTransaction; // reset after Commit or Rollback

    // Server specific methods
    procedure DoCommitRetaining; virtual;
    procedure DoRollbackRetaining; virtual;

    procedure DoSavepoint(const Name: string); virtual;
    procedure DoReleaseSavepoint(const Name: string); virtual;
    procedure DoRollbackToSavepoint(const Name: string); virtual;

    function InternalAddConnection(Connection: TCustomDAConnection): integer;
    procedure InternalRemoveConnection(Connection: TCustomDAConnection);

    function DoAddConnection(Connection: TCustomDAConnection): integer; virtual;
    procedure DoRemoveConnection(Connection: TCustomDAConnection); virtual;
    procedure DoClearConnections;

    procedure DoError(E: Exception; var Fail: boolean);

    property Connections[Index: integer]: TCustomDAConnection read GetConnection;
    property ConnectionsCount: integer read GetConnectionsCount;
    property TransactionType: TTransactionType read FTransactionType write SetTransactionType default ttNative;
    property IsolationLevel: TCRIsolationLevel read FIsolationLevel write SetIsolationLevel default ilReadCommitted;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Active: boolean read GetActive;

    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    property DefaultConnection: TCustomDAConnection read FDefaultConnection write SetDefaultConnection;
    property DefaultCloseAction: TCRTransactionAction read FDefaultCloseAction write FDefaultCloseAction default taRollback;
    property OnError: TDATransactionErrorEvent read FOnError write FOnError;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property OnRollback: TNotifyEvent read FOnRollback write FOnRollback;
    property OnCommitRetaining: TNotifyEvent read FOnCommitRetaining write FOnCommitRetaining;
    property OnRollbackRetaining: TNotifyEvent read FOnRollbackRetaining write FOnRollbackRetaining;
  end;

{ TDAParamValue }

  TDAParamValue = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FParam: TDAParam;
    FIndex: integer;
  protected
    procedure CheckDataType(Value: TFieldType; const CompatibleTypes: array of TFieldType); virtual;
    procedure CheckBlobDataType(Value: TFieldType);

    procedure SetAsSmallInt(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
    function GetAsInteger: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}; virtual;
    procedure SetAsInteger(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
    procedure SetAsWord(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
    function GetAsLargeInt: LargeInt; virtual;
    procedure SetAsLargeInt(Value: LargeInt); virtual;
  {$IFDEF VER12P}
    function GetAsLongWord: {$IFDEF VER26P}Cardinal{$ELSE}LongWord{$ENDIF}; virtual;
    procedure SetAsLongWord(Value: {$IFDEF VER26P}Cardinal{$ELSE}LongWord{$ENDIF}); virtual;
    procedure SetAsShortInt(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
    procedure SetAsByte(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
  {$ENDIF}
    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;
  {$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString; virtual;
    procedure SetAsAnsiString(const Value: AnsiString); virtual;
  {$ENDIF}
    function GetAsWideString: WideString; virtual;
    procedure SetAsWideString(const Value: WideString); virtual;

    function GetAsBCD: Currency; virtual;
    procedure SetAsBCD(const Value: Currency); virtual;
    function GetAsFMTBCD: TBcd; virtual;
    procedure SetAsFMTBCD(const Value: TBcd); virtual;
  {$IFNDEF FPC}
    function GetAsSQLTimeStamp: TSQLTimeStamp; virtual;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual;
  {$ENDIF}
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsBoolean(Value: Boolean); virtual;
    function GetAsCurrency: Currency; virtual;
    procedure SetAsCurrency(const Value: Currency); virtual;
    procedure SetAsDate(const Value: TDateTime); virtual;
    procedure SetAsTime(const Value: TDateTime); virtual;
    function GetAsDateTime: TDateTime; virtual;
    procedure SetAsDateTime(const Value: TDateTime); virtual;
  {$IFDEF VER14P}
    function GetAsSQLTimeStampOffset: TSQLTimeStampOffset; virtual;
    procedure SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); virtual;
    function GetAsSingle: Single; virtual;
    procedure SetAsSingle(const Value: Single); virtual;
  {$ENDIF}
    function GetAsFloat: Double; virtual;
    procedure SetAsFloat(const Value: Double); virtual;
    procedure SetAsMemo(const Value: string); virtual;
    function GetAsBytes: TBytes; virtual;
    procedure SetAsBytes(const Value: TBytes); virtual;
    procedure SetAsBlob(const Value: TBlobData); virtual;
    function GetAsBlobRef: TBlob; virtual;
    procedure SetAsBlobRef(const Value: TBlob); virtual;

    function GetAsVariant: Variant; virtual;
    procedure SetAsVariant(const Value: Variant); virtual;

    function GetIsNull: boolean; virtual;
    procedure SetIsNull(Value: boolean); virtual;

    function CreateValueObject: TSharedObject; virtual;
    procedure FreeValueObject; virtual;
    function AllocValueObject: TSharedObject;
    function GetValueObject: TSharedObject; virtual;
    procedure SetValueObject(Value: TSharedObject); virtual;

    property Param: TDAParam read FParam;
  public
    constructor Create(Param: TDAParam);
    procedure Clear; virtual;

    property Index: integer read FIndex;

    property AsSmallInt: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsSmallInt;
    property AsInteger: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsInteger;
    property AsWord: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsWord;
  {$IFDEF VER6P}
    property AsLargeInt: Int64 read GetAsLargeInt write SetAsLargeInt;
  {$ENDIF}
  {$IFDEF VER12P}
    property AsByte: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsByte;
    property AsShortInt: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsShortInt;
    property AsLongWord: {$IFDEF VER26P}Cardinal{$ELSE}LongWord{$ENDIF} read GetAsLongWord write SetAsLongWord;
  {$ENDIF}
    property AsString: string read GetAsString write SetAsString;
  {$IFNDEF NEXTGEN}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  {$ENDIF}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;

    property AsBCD: Currency read GetAsBCD write SetAsBCD;
    property AsFMTBCD: TBcd read GetAsFMTBCD write SetAsFMTBCD;
  {$IFNDEF FPC}
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
  {$ENDIF}
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  {$IFDEF VER14P}
    property AsSQLTimeStampOffset: TSQLTimeStampOffset read GetAsSQLTimeStampOffset write SetAsSQLTimeStampOffset;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
  {$ENDIF}
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsMemo: string read GetAsString write SetAsMemo;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
  {$IFDEF VER12P}
    property AsBlob: TBlobData read GetAsBytes write SetAsBlob;
  {$ELSE}{$IFDEF FPC}
    property AsBlob: TBlobData read GetAsBytes write SetAsBlob;
  {$ELSE}
    property AsBlob: TBlobData read GetAsString write SetAsBlob;
  {$ENDIF}{$ENDIF}
    property AsBlobRef: TBlob read GetAsBlobRef write SetAsBlobRef;

    property IsNull: boolean read GetIsNull write SetIsNull;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  TDAParamValueClass = class of TDAParamValue;

{ TDAParam }

  TDAParam = class (TParam)
  private
    FSize: integer;
    FSubDataType: word;
    FParamValue: TDAParamValue;

    function IsDataTypeStored: boolean;
    function IsValueStored: boolean;

    procedure CheckIndex(Index: integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckGetValue; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckSetValue; {$IFDEF USE_INLINE}inline;{$ENDIF}

  protected
    FParamObject: TSharedObject;
    FDataArr: TVariantArray;
    FValueCount: integer;
    FNational: boolean;
    FEncryptor: TEncryptionMethod;

    function NeedBlobUnicode: boolean; virtual;
    function GetNativeParamObject: TSharedObject; overload; virtual;
    function GetNativeParamObject(SourceObject: TSharedObject): TSharedObject; overload; virtual;
    function GetParamObject: TSharedObject;
    procedure SetParamObject(Value: TSharedObject);

    function IsSharedObjectDataType(DataType: TFieldType): boolean; virtual;
    function IsSharedObject: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsBlobDataType(DataType: TFieldType): boolean; virtual;
    function IsBlob: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetDataType: TFieldType; virtual;
    procedure SetDataType(Value: TFieldType); virtual;
    function GetSize: integer; virtual;
    procedure SetSize(Value: integer); virtual;

    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;
  {$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString; virtual;
    procedure SetAsAnsiString(const Value: AnsiString); virtual;
  {$ENDIF}
    function GetAsWideString: WideString; virtual;
    procedure SetAsWideString(const Value: WideString); virtual;
    function GetAsBytes: TBytes; virtual;
    procedure SetAsBytes(const Value: TBytes); virtual;

    function GetAsInteger: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}; virtual;
    procedure SetAsInteger(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
    procedure SetAsSmallInt(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
    procedure SetAsWord(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
    function GetAsFloat: Double; virtual;
    procedure SetAsFloat(Value: Double); virtual;
    function GetAsLargeInt: Int64; virtual;
    procedure SetAsLargeInt(const Value: Int64); virtual;
  {$IFDEF VER12P}
    procedure SetAsShortInt(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
    procedure SetAsByte(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF}); virtual;
  {$ENDIF}

    procedure SetAsBlob(const Value: TBlobData); virtual;
    procedure SetAsMemo(const Value: string); virtual;

    function GetAsBlobRef: TBlob; virtual;
    procedure SetAsBlobRef(const Value: TBlob); virtual;

    function GetAsMemoRef: TBlob; virtual;
    procedure SetAsMemoRef(const Value: TBlob); virtual;

    function GetAsVariant: Variant; virtual;
    procedure SetAsVariant(const Value: Variant); virtual;

  {$IFNDEF FPC}
    function GetAsSQLTimeStamp: TSQLTimeStamp; virtual;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual;
  {$ENDIF}

    function GetAsCursor: TCRCursor;
    procedure SetAsCursor(Value: TCRCursor);

    procedure SetText(const Value: string); virtual;

    function GetIsNull: boolean; virtual;
    procedure SetIsNull(Value: boolean);
    procedure SetNational(Value: boolean); virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadExtDataType(Reader: TReader);
    procedure WriteExtDataType(Writer: TWriter);

    function CreateObject: TSharedObject; virtual;
    procedure FreeObject; virtual;

    procedure AssignParam(Param: TParam);
    procedure AssignTo(Dest: TPersistent); override;

    procedure CheckArrayType(DataType: TFieldType); virtual;

    procedure FreeValues; virtual;
    function GetValueCount: integer; virtual;
    procedure SetValueCount(Value: integer); virtual;
    function GetParamValueClass: TDAParamValueClass; virtual;
    function GetParamValue(Index: integer): TDAParamValue;
    class function GetVarType(VarType: TVarType): TFieldType;

    property ParamObject: TSharedObject read GetParamObject write SetParamObject;
    property SubDataType: word read FSubDataType write FSubDataType;
    property National: Boolean read FNational write SetNational;

    property AsCursor: TCRCursor read GetAsCursor write SetAsCursor;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Assign(Source: TPersistent); override;
    procedure AssignField(Field: TField);
    procedure AssignFieldValue(Field: TField; const Value: Variant); virtual;

    procedure LoadFromFile(const FileName: string; BlobType: TBlobType);
    procedure LoadFromStream(Stream: TStream; BlobType: TBlobType); virtual;
    procedure SetBlobData(Buffer: IntPtr; Size: Integer); overload;
    procedure SetBlobData(Buffer: TValueBuffer); overload;

    property AsString: string read GetAsString write SetAsString;
  {$IFNDEF NEXTGEN}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  {$ENDIF}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property AsInteger: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsInteger;
    property AsSmallInt: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsSmallInt;
    property AsWord: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsWord;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property AsLargeInt: Int64 read GetAsLargeInt write SetAsLargeInt;
  {$IFDEF VER12P}
    property AsShortInt: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsShortInt;
    property AsByte: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF} read GetAsInteger write SetAsByte;
  {$ENDIF}
  {$IFDEF VER12P}
    property AsBlob: TBlobData read GetAsBytes write SetAsBlob;
  {$ELSE}{$IFDEF FPC}
    property AsBlob: TBlobData read GetAsBytes write SetAsBlob;
  {$ELSE}
    property AsBlob: TBlobData read GetAsString write SetAsBlob;
  {$ENDIF}{$ENDIF}
    property AsMemo: string read GetAsString write SetAsMemo;
    property AsBlobRef: TBlob read GetAsBlobRef write SetAsBlobRef;
    property AsMemoRef: TBlob read GetAsMemoRef write SetAsMemoRef;
  {$IFNDEF FPC}
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
  {$ENDIF}
    property IsNull: boolean read GetIsNull;
    property Text: string read GetAsString write SetText;

    property Values[Index: integer]: TDAParamValue read GetParamValue; default;
    property ValueCount: integer read GetValueCount write SetValueCount;
  published
    property DataType: TFieldType read GetDataType write SetDataType stored IsDataTypeStored;
    property ParamType default DB.ptUnknown;
    property Size: integer read GetSize write SetSize default 0;
    property Value: variant read GetAsVariant write SetAsVariant stored IsValueStored;
  end;

{ TDAParams }

  TParamsChangeType = (ctGenerated, ctUsers, ctUserChecked);

  TDAParams = class (TParams)
  private
    function GetItem(Index: integer): TDAParam;
    procedure SetItem(Index: integer; Value: TDAParam);

    function GetValueCount: integer;
    procedure SetValueCount(Value: integer);

  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TPersistent;
    FNeedsUpdateItem: boolean;
    FParamsChangeType: TParamsChangeType;

    function GetOwner: TPersistent; override;
    function GetConnection: TCustomDAConnection; virtual;
    procedure Update(Item: TCollectionItem); override;
    procedure Disconnect;

  public
    constructor Create(Owner: TPersistent); overload;

    function ParamByName(const Value: string): TDAParam;
    function FindParam(const Value: string): TDAParam;
    function CreateParam(FldType: TFieldType; const ParamName: string;
      ParamType: TParamType): TDAParam;
    property Items[Index: integer]: TDAParam read GetItem write SetItem; default;

    property ValueCount: integer read GetValueCount write SetValueCount;
  end;

{ TDACursorField }

  TDACursorField = class (TField)
  private
    function GetAsCursor: TCRCursor;
  protected
    function GetValue(out Value: TCRCursor): boolean;

  public
    constructor Create(Owner: TComponent); override;

    property AsCursor: TCRCursor read GetAsCursor;
  end;

  TLockMode = (lmNone, lmPessimistic, lmOptimistic);

  TFieldArray = array of TField;

  TLockTrStarted = (ltsNone, ltsOnLock, ltsOnLockCachedUpdates, ltsBeforeLockCachedUpdates);

  TStatementType = (stQuery, stInsert, stUpdate, stDelete, stLock, stRefresh,
    stCustom, stRefreshQuick, stRefreshCheckDeleted, stBatchUpdate, stRecCount);
  TStatementTypes = set of TStatementType;

  TDADataSetUpdater = class(TDataSetUpdater)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FUpdateQuery: TComponent;
    FIsUsedIndexNameForFields: boolean;

    procedure CopyRecBufToActiveRecord(SrcRecordSet: TData; SrcRecBuf: TRecordBuffer;
      const StatementTypes: TStatementTypes; out RecordWasChanged: boolean);
    procedure UpdateActiveRecordFromParams;
    procedure UpdateActiveRecord(const StatementTypes: TStatementTypes);
    procedure GetUQFields(const KeyFieldDescs: TFieldDescArray; const KeyFields: TFieldArray; out KeyFieldsUQ: TFieldArray);
    procedure CheckDeletedRecords;
    procedure RefreshQuickDataSet;

  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TCustomDADataSet;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSetService: TDADataSetService;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FFixedTransaction: TDATransaction;
    FLockTrStarted: TLockTrStarted; // when transaction has been started
    FUpdateComponents: array [TStatementType] of TComponent;
    FParamsInfo: TDAParamsInfo;

    FBatchSQLs: StringBuilder;
    FBatchParams: TDAParams;
    FBatchStatements: integer;

    FRefreshInUpdate: boolean; // for correct behavior of refresh record
    FOptionsForAutoGeneratedSQL: Boolean;
    FCheckMode: TCheckMode;

    function UseParamType(Param: TDAParam): boolean; virtual; //This function indicates ParamType using in PerformSQL
    procedure SetUpdateQuery(Value: TComponent); virtual;
    procedure SetDefaultParamType(Param: TDAParam); virtual;
    function NeedReturnParams: boolean; virtual;
    function ReturnParamsAsFields: boolean; virtual;
    function UseSQLGeneratorParams(const StatementTypes: TStatementTypes): boolean;
    function GetSQLSeparator: string; virtual;

    function RefreshAfterInsertAllowed: boolean; virtual;

    function IsNeedInsertPreconnect: boolean; virtual;
    function IsNeedEditPreconnect: boolean; virtual;
    function IsPreconnected: boolean; virtual;
    function RefreshByLockAllowed: boolean; virtual;
    function CanRefreshByLock: boolean;

    function GetICommand: TCRCommand;
    function GetIRecordSet: TCRRecordSet;
    procedure CheckIRecordSet;
    function GetLockMode: TLockMode;
    function GetUpdateObject: TCustomDAUpdateSQL;
    function GetUpdateSQL(StatementType: TStatementType): string;
    function UsedConnection: TCustomDAConnection;
    function UsedTransaction: TDATransaction;
    function UsedUpdateTransaction: TDATransaction;
    procedure SetRowsAffected(Value: Integer);
    procedure BeginConnection;
    procedure EndConnection;

    procedure SetIdentityFieldValue;
    function GetIdentityFieldValue(var Value: variant): boolean; virtual;
    function GetSavepointName(CachedUpdates: boolean): string;
    function GetLockSavepointName: string;
    function GetLockCachedUpdatesSavepointName: string;
    function SavepointAllowed: boolean; virtual;
    procedure SetSavepoint(SavepointName: string; CachedUpdates: boolean); virtual;
    procedure SetLockSavepoint;
    procedure SetLockCachedUpdatesSavepoint;
    procedure RollbackToSavepoint(SavepointName: string; CachedUpdates: boolean); virtual;
    procedure RollbackToLockSavepoint;
    procedure RollbackToLockCachedUpdatesSavepoint;
    procedure ResetLockCachedUpdatesSavepoint;

    function FieldByParamName(var ParamName: string; out Old: boolean; out AFieldNo: integer; out Master: Boolean): TField; virtual;
    function GetUpdateStatement(const StatementType: TStatementType): string; virtual;
    procedure CheckUpdateQuery(const StatementType: TStatementType); virtual;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType; const IsAutoGeneratedSQL: Boolean); virtual;
    procedure CheckUpdateSQL(const SQL: string; const StatementTypes: TStatementTypes;
      UseGenerator: boolean = True); virtual;
    procedure UpdateExecute(const StatementTypes: TStatementTypes); virtual;

  { RefreshQuick }
    function IsRefreshQuickField(FieldDesc: TFieldDesc): boolean; virtual;
    procedure SaveMaxRefreshQuickValue(FieldDesc: TFieldDesc; const Value: variant); virtual;

    procedure PrepareAppend; virtual;
    procedure PrepareUpdate; virtual;
    procedure PrepareDelete; virtual;
    procedure UnPrepareAppendUpdateDelete; virtual;
    procedure PrepareCachedUpdate; virtual;
    procedure FinishCachedUpdate; virtual;
    procedure UnPrepareCachedUpdate; virtual;
    procedure UnLockCachedUpdate; virtual;
    function PerformLock: boolean; virtual;
    function PerformUnLock: boolean; virtual;
    procedure EndUpdate(Success: boolean);
    function PerformAppend: boolean; override;
    function PerformUpdateDelete(const StatementType: TStatementType): boolean; virtual;
    function PerformDelete: boolean; override;
    function PerformUpdate: boolean; override;
    function PerformRefreshRecord: boolean; virtual;
    function PerformRefreshRecordInUpdate: boolean; virtual;
    function PerformRefreshQuick(CheckDeleted: boolean): boolean;
  {$IFDEF WITH_IPROVIDER}
    function PerformPSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): boolean;
  {$ENDIF}
    function CacheChanged: boolean; override;
    function CacheApplied: boolean; override;
    function CacheCanceled: boolean; override;

    function BatchUpdateAllowed: boolean; virtual;
    function BatchUpdate: boolean; override;
    function CanFlushBatch: boolean; override;
    procedure ClearBatch;
    procedure FlushBatch; override;
    function PrepareBatch(const SQL: string): string; virtual;

    procedure UnprepareUpdateObjects;
    procedure ReleaseParams(UpdateQuery: TComponent; AllParams: Boolean);
    function LockCompare(const Value1, Value2: variant): boolean; virtual;

    property UpdateQuery: TComponent read FUpdateQuery write SetUpdateQuery;
  public
    constructor Create(AOwner: TDataSetService); override;
    destructor Destroy; override;

    function SelectDbValue(const OperationName, SQL: string): variant;
    function GetDefaultExpressionValue(DefExpr: string; out Value: variant): boolean; virtual;

    procedure WriteUQParams(const StatementTypes: TStatementTypes);
    function PerformSQL(const SQL: string; const StatementTypes: TStatementTypes): boolean; virtual;

    property CheckMode: TCheckMode read FCheckMode write FCheckMode;
    property ParamsInfo: TDAParamsInfo read FParamsInfo;
  end;

  TDADataSetService = class(TDataSetService)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TCustomDADataSet;
    FUpdater: TDADataSetUpdater;
    FSQLGenerator: TDASQLGenerator;
    FFieldsExtInited: boolean;
    FIsAnyFieldCanBeModified: boolean;

    procedure SetDataSetUpdater(Value: TDataSetUpdater); override;

    procedure CreateSQLGenerator; virtual;
    procedure FreeSQLGenerator;
    procedure SetSQLGenerator(Value: TDASQLGenerator); virtual;

    procedure PreInitCursor; override; // occured just after Data.Open
    procedure InitCursor; virtual;
    procedure CloseCursor; virtual;

    procedure InitUpdatingTable;
    procedure InitFieldsOptions; virtual;
    procedure UpdateFieldsOptions; virtual;

    procedure SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc); virtual;
    procedure FillFieldsDefaultValues; virtual;
    procedure SetFieldsReadOnly; virtual;
    procedure SetFieldsReadOnlyOld;
    function GetIdentityField: TCRFieldDesc;
    function GetKeyGeneratorField: TCRFieldDesc;

    function DetectHiddenFields: TFieldArray; virtual;

    function DetectCanModify: boolean; virtual;

    function GetRecCount: integer; virtual;
    procedure BreakExec; virtual;
    function Executing: boolean; virtual;
    function GetCurrentSchema: string; virtual;

    procedure InitMasterParams(Params: TDAParams); virtual;

  { XML }
    procedure WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: string;
      XMLWriter: XMLTextWriter); override;

    function GetIConnection: TCRConnection;
    function GetICommand: TCRCommand;
    function GetIRecordSet: TCRRecordSet;
    procedure CheckIRecordSet;
    function UsedConnection: TCustomDAConnection;
    function IsDMLRefresh: boolean;
    function IsAutoCommit: boolean;
    function IsFetchAll: boolean;
    procedure SetAutoCommit(Value: boolean);
    procedure SetNeedAddRef(Value: boolean);
    procedure BeginConnection;
    procedure EndConnection;

  public
    constructor Create(AOwner: TMemDataSet); override;
    destructor Destroy; override;

    function GetProp(Prop: integer; var Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    procedure ClearFieldDescRefs;
    procedure ResetTableKeyFields; virtual;

    function GetDBKeyList(const TableName, IndexName: string): string; virtual;
    function OpenNext: boolean; virtual;
    function NeedParamValuesOnPrepare: boolean; virtual;
    procedure SetKeyGeneratorValue(const Value: variant);

    property IdentityField: TCRFieldDesc read GetIdentityField;
    property KeyGeneratorField: TCRFieldDesc read GetKeyGeneratorField;
    property Updater: TDADataSetUpdater read FUpdater;
    property SQLGenerator: TDASQLGenerator read FSQLGenerator;
  end;

  TDASQLGeneratorService = class(TSQLGeneratorService)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TCustomDADataSet;
  public
    procedure RaiseError(const Message: string); override;

    function GetOldRecBuf: IntPtr; override;
    function GetNewRecBuf: IntPtr; override;
    function BlobFieldModified(FieldDesc: TCRFieldDesc): boolean; override;
    function GetFieldObject(FieldDesc: TFieldDesc): TSharedObject; override;

    function GetUpdateCommand: TCRCommand; override;
    function GetDBKeyList(const TableName, IndexName: string): string; override;

    function ParamExists(const ParamName: string): boolean; override;

    function BaseSQL: string; override;
    function FinalSQL: string; override;
    function FilterSQL: string; override;

    property DataSet: TCustomDADataSet read FDataSet;
  end;

{ TCustomDADataSet }

  TRefreshOption = (roAfterInsert, roAfterUpdate, roBeforeEdit);
  TRefreshOptions = set of TRefreshOption;

  TBeforeExecuteEvent = procedure (Sender: TObject) of object;
  TAfterExecuteEvent = procedure (Sender: TObject; Result: boolean) of object;
  TUpdateExecuteEvent = procedure (Sender: TDataSet; StatementTypes: TStatementTypes;
    Params: TDAParams) of object;
  TBeforeFetchEvent = procedure (DataSet: TCustomDADataSet; var Cancel: boolean) of object;
  TAfterFetchEvent = procedure (DataSet: TCustomDADataSet) of object;

  TQuickOpenInfo = record
    OldActive: boolean;
    OldDebug: boolean;
    OldFetchAll: boolean;
    OldFetchRows: integer;
  end;

  TDAConditions = class;

  TDACondition = class(TCollectionItem)
  private
    FName: string;
    FValue: string;
    FEnabled: Boolean;
    procedure SetValue(const Value: string);
    procedure SetEnabled(Value: Boolean);
    procedure SetName(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Collection: TCollection); override;

    procedure Enable;
    procedure Disable;

  published
    property Name: string read FName Write SetName;
    property Value: string read FValue write SetValue;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TDAConditions = class(TCollection)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TPersistent;
    FBaseSQL: string;
    FEnabled: Boolean;
    FCurrentEnabledText: string;
    function GetItem(Index: integer): TDACondition;
    procedure SetItem(Index: Integer; Value: TDACondition);
    function GetWhereSQL: string;
    procedure RestoreBaseSQL;
    procedure AssignParams(ParamsSrc: TDAParams; ParamsDst: TDAParams);
    procedure SetText(const Value: string);
    function GetText: string;

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TPersistent);

    function IndexOf(const Name: string): Integer;
    function Find(const Name: string): TDACondition;
    function Get(const Name: string): TDACondition;
    function Add(const Name, Value: string; Enabled: Boolean = True): TDACondition; overload;
    function Add(const Value: string; Enabled: Boolean = True): TDACondition; overload;
    procedure Delete(Index: integer);
    procedure Remove(const Name: string);
    procedure Clear;
    procedure Enable;
    procedure Disable;

    property Items[Index: Integer]: TDACondition read GetItem write SetItem; default;
    property Condition[Index: Integer]: TDACondition read GetItem write SetItem;
    property Enabled: Boolean read FEnabled;
    property WhereSQL: string read GetWhereSQL;
    property Text: string read GetText write SetText; //FibPlus compatibility
  end;

  TDAEncryption = class (TPersistent)
  private
    FEncryptor: TCREncryptor;
    FFields: string;
    procedure SetEncryptor(Value: TCREncryptor);
    procedure SetFields(const Value: string);

  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCustomDADataSet;
    procedure AssignTo(Dest: TPersistent); override;
    function IsFieldEncrypted(FieldDesc: TFieldDesc): boolean; overload;
  public
    constructor Create(Owner: TCustomDADataSet);

    function IsFieldEncrypted(const FieldName: string): boolean; overload;
    function IsFieldEncrypted(FieldNo: Integer): boolean; overload;
    procedure EncryptDataSet(AutoCommitExplicitTransaction: Boolean = True);

    property Encryptor: TCREncryptor read FEncryptor write SetEncryptor;
  published
    property Fields: string read FFields write SetFields;
  end;

  TDADataSetOptions = class (TPersistent)
  private
    FSetFieldsReadOnly: boolean;
    FRequiredFields: boolean;
    FStrictUpdate: boolean;
    FNumberRange: boolean;
    FQueryRecCount: boolean;
    FAutoPrepare: boolean;
    FReturnParams: boolean;
    FTrimFixedChar: boolean;
    FTrimVarChar: boolean;
    FSetEmptyStrToNull: boolean;
    FLongStrings: boolean;
    FRemoveOnRefresh: boolean;
    FFlatBuffers: boolean;
    FQuoteNames: boolean;
  {$IFDEF HAVE_COMPRESS}
    FCompressBlobMode: TCompressBlobMode;
  {$ENDIF}
    FFullRefresh: boolean;
    FLocalMasterDetail: boolean;
    FFieldOrigins: TFieldOrigins;
    FDefaultValues: boolean;
    FExtendedFieldsInfo: boolean;
    FUpdateBatchSize: integer;
    FUpdateAllFields: boolean;
    FPrepareUpdateSQL: boolean;
    FEnableBCD: boolean;
    FEnableFMTBCD: boolean;
    FMasterFieldsNullable: boolean;
    FInsertAllSetFields: boolean;

    procedure SetSetFieldsReadOnly(Value: boolean);
    procedure SetFullRefresh(Value: boolean);
    procedure SetRequiredFields(Value: boolean);
    procedure SetNumberRange(Value: boolean);
    procedure SetTrimFixedChar(Value: boolean);
    procedure SetTrimVarChar(Value: boolean);
    procedure SetSetEmptyStrToNull(Value: boolean);
    procedure SetLongStrings(Value: boolean);
    procedure SetAutoPrepare(Value: boolean);
    procedure SetFlatBuffers(Value: boolean);
    function GetDetailDelay: integer;
    procedure SetDetailDelay(Value: integer);
  {$IFDEF HAVE_COMPRESS}
    procedure SetCompressBlobMode(Value: TCompressBlobMode);
  {$ENDIF}
    procedure SetLocalMasterDetail(Value: boolean);
    function GetCacheCalcFields: boolean;
    procedure SetCacheCalcFields(Value: boolean);
    procedure SetQuoteNames(Value: boolean);
    procedure SetFieldOrigins(Value: TFieldOrigins);
    function GetFieldsOrigin: boolean; deprecated;
    procedure SetFieldsOrigin(Value: boolean); deprecated;
    procedure SetDefaultValues(Value: boolean);
    procedure SetExtendedFieldsInfo(Value: boolean);
    procedure SetEnableBCD(Value: boolean);
    procedure SetEnableFMTBCD(Value: boolean);
    procedure SetMasterFieldsNullable(Value: boolean);
    procedure SetInsertAllSetFields(Value: boolean);

  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCustomDADataSet;

    procedure AssignTo(Dest: TPersistent); override;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadFieldsOriginal(Reader: TReader);

    property FullRefresh: boolean read FFullRefresh write SetFullRefresh default False;
    property TrimVarChar: boolean read FTrimVarChar write SetTrimVarChar default False;
    property SetEmptyStrToNull: boolean read FSetEmptyStrToNull write SetSetEmptyStrToNull default False;
    property ExtendedFieldsInfo: boolean read FExtendedFieldsInfo write SetExtendedFieldsInfo default False;
    property EnableBCD: boolean read FEnableBCD write SetEnableBCD default False;
    property EnableFMTBCD: boolean read FEnableFMTBCD write SetEnableFMTBCD default False;

  public
    constructor Create(Owner: TCustomDADataSet);

    property SetFieldsReadOnly: boolean read FSetFieldsReadOnly write SetSetFieldsReadOnly default True;
    property RequiredFields: boolean read FRequiredFields write SetRequiredFields default True;
    property StrictUpdate: boolean read FStrictUpdate write FStrictUpdate default True;
    property PrepareUpdateSQL: boolean read FPrepareUpdateSQL write FPrepareUpdateSQL default False;
    property NumberRange: boolean read FNumberRange write SetNumberRange default False;
    property QueryRecCount: boolean read FQueryRecCount write FQueryRecCount default False;
    property AutoPrepare: boolean read FAutoPrepare write SetAutoPrepare default False;
    property ReturnParams: boolean read FReturnParams write FReturnParams default False;
    property TrimFixedChar: boolean read FTrimFixedChar write SetTrimFixedChar default True;
    property LongStrings: boolean read FLongStrings write SetLongStrings default True;
    property FlatBuffers: boolean read FFlatBuffers write SetFlatBuffers default False;
    property RemoveOnRefresh: boolean read FRemoveOnRefresh write FRemoveOnRefresh default True;
    property QuoteNames: boolean read FQuoteNames write SetQuoteNames default False;
    property DetailDelay: integer read GetDetailDelay write SetDetailDelay default 0;
  {$IFDEF HAVE_COMPRESS}
    property CompressBlobMode: TCompressBlobMode read FCompressBlobMode write SetCompressBlobMode default cbNone;
  {$ENDIF}
    property LocalMasterDetail: boolean read FLocalMasterDetail write SetLocalMasterDetail default False;
    property CacheCalcFields: boolean read GetCacheCalcFields write SetCacheCalcFields default False;
    property FieldsOrigin: boolean read GetFieldsOrigin write SetFieldsOrigin; // Deprecated. Use FieldOrigins instead
    property FieldOrigins: TFieldOrigins read FFieldOrigins write SetFieldOrigins default foNone;
    property DefaultValues: boolean read FDefaultValues write SetDefaultValues default False;
    property UpdateBatchSize: Integer read FUpdateBatchSize write FUpdateBatchSize default 1;
    property UpdateAllFields: boolean read FUpdateAllFields write FUpdateAllFields default False;
    property MasterFieldsNullable: boolean read FMasterFieldsNullable write SetMasterFieldsNullable default False;
    property InsertAllSetFields: boolean read FInsertAllSetFields write SetInsertAllSetFields default False;
  end;

  TSmartFetchOptions = class (TPersistent)
  private
    FEnabled: Boolean;
    FLiveBlock: Boolean;
    FPrefetchedFields: string;
    FSQLGetKeyValues: TStrings;
    FSQLGetDataValues: TStrings;
    procedure SetEnabled(Value: Boolean);
    procedure SetLiveBlock(Value: Boolean);
    procedure SetPrefetchedFields(const Value: string);
    procedure SetSQLGetKeyValues(Value: TStrings);
///    procedure SetSQLGetDataValues(Value: TStrings);
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCustomDADataSet;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TCustomDADataSet);
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property LiveBlock: Boolean read FLiveBlock write SetLiveBlock default True;
    property PrefetchedFields: string read FPrefetchedFields write SetPrefetchedFields;
    property SQLGetKeyValues: TStrings read FSQLGetKeyValues write SetSQLGetKeyValues;
///    property SQLGetDataValues: TStrings read FSQLGetDataValues write SetSQLGetDataValues;
  end;

  TCustomDADataSet = class (TMemDataSet)
  private
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FUpdateTransaction: TDATransaction;
    FParams: TDAParams; // for easy reference of FCommand.Params
    FMacros: TMacros; // for easy reference of FCommand.Macros
    FFetchRows: integer;
    FDebug: boolean;
    FReadOnly: boolean;
    FUniDirectional: boolean;
    FAutoCommit: boolean;
    FUpdateObject: TCustomDAUpdateSQL;
    FRefreshOptions: TRefreshOptions;
    FOptions: TDADataSetOptions;
    FSmartFetchOptions: TSmartFetchOptions;
    FBaseSQL: string;
    FLockMode: TLockMode;
    FKeyFields: string;
    FDMLRefresh: boolean;
    FFindKeyOptions: TLocateExOptions;
    FDisconnected: boolean;
    FCheckMode: TCheckMode;

    FDataTypeMap: TDAMapRules;
    FEncryption: TDAEncryption;

    FWhereConditions: TDAConditions;

    FBeforeExecute: TBeforeExecuteEvent;
    FAfterExecute: TAfterExecuteEvent;
    FBeforeFetch: TBeforeFetchEvent;
    FAfterFetch: TAfterFetchEvent;
    FBeforeUpdateExecute: TUpdateExecuteEvent;
    FAfterUpdateExecute: TUpdateExecuteEvent;

    function IsMapRulesStored: boolean;

    procedure SetUpdateTransaction(Value: TDATransaction);
    function GetSQL: TStrings;
    procedure SetSQL(Value: TStrings);
    procedure SetFetchRows(Value: integer);
    function GetParams: TDAParams;
    procedure SetParams(Value: TDAParams);
    function GetParamCount: word;
    function GetParamCheck: boolean;
    procedure SetParamCheck(Value: boolean);
    function GetMacros: TMacros;
    procedure SetMacros(Value: TMacros);
    function GetMacroCount: word;
    function GetRowsAffected: integer;
    function GetParamsProcessed: integer;
    procedure SetUniDirectional(Value: boolean);
    procedure SetAutoCommit(Value: boolean);
    //procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetUpdateObject(Value: TCustomDAUpdateSQL);
    procedure SetOptions(Value: TDADataSetOptions);
    procedure SetSmartFetchOptions(Value: TSmartFetchOptions);
    procedure SaveModifiedSQL(const NewSQL: string);
    function GetBaseSQL: string;
    procedure SetEncryption(Value: TDAEncryption);
    procedure SetCheckMode(Value: TCheckMode);
  {$IFDEF WITH_IPROVIDER}
    function InternalPSExecuteStatement(const ASQL: string; AParams: TParams; Query: TCustomDADataSet): Integer;
  protected
  { IProviderSupport }
    FOldKeyFields: string; // To PSGetKeyFields after closing table (see SDAC 3034)
    FOldTableName: string; // PSGetTableName must return right value even after DataSet.Close
    function PSInTransaction: Boolean; override;
    procedure PSStartTransaction; override;
    procedure PSEndTransaction(Commit: Boolean); override;
    procedure PSExecute; override;
  {$IFDEF VER17P}
    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; overload; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams; var ResultSet: TDataSet): Integer; overload; override;
  {$ENDIF}
  {$IFNDEF NEXTGEN}
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: IntPtr = nil): Integer; override;
  {$ENDIF}
    function PSGetParams: DB.TParams; override;
    function PSGetQuoteChar: string; override;
    function PSGetTableName: string; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    procedure PSSetParams(AParams: DB.TParams); override;
    procedure PSSetCommandText(const CommandText: string); override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetKeyFields: string; override;
    procedure PSDetectKeyFields(DataSet: TDataSet);
  {$ENDIF}

    procedure ReadConditions(Reader: TReader);
    procedure WriteConditions(Writer: TWriter);

  protected
    FIRecordSet: TCRRecordSet;
    FICommand: TCRCommand;  // for perf
    FCommand: TCustomDASQL;
    FDataSetService: TDADataSetService;

    FFilterSQL: string; //CR ODAC (8387) vs (8751)
    FUpdatingTable: string; // Don't delete because FTablesInfo created only on Open

    FDesignCreate: boolean;
    FNonBlocking: boolean;
    FLockDebug: boolean; // locking trans debug info
    FUpdateSQL: array [TStatementType] of TStrings; // SQLInsert, SQLUpdate etc

    FRowsAffected: integer;
    FRecordCount: integer;
    FLastInsertId: int64;
    FParamsProcessed: integer;

    FFetchAll: boolean;
    FFetchCanceled: boolean;

    FStreamedOpen: Boolean;

    function GetFieldObject(Field: TField): TSharedObject; overload;
    function GetFieldObject(Field: TField; RecBuf: TRecordBuffer): TSharedObject; overload;
    function GetFieldObject(FieldDesc: TFieldDesc): TSharedObject; overload;
    function GetFieldObject(FieldDesc: TFieldDesc; RecBuf: TRecordBuffer): TSharedObject; overload;

    procedure CheckActive; override;
    procedure CheckInactive; override;
    procedure CreateIRecordSet; override;
    procedure FreeIRecordSet;
    procedure SetIRecordSet(Value: TData{TRecordSet}); override;
    procedure CheckIRecordSet;
    procedure SetIndexFieldNames(const Value: string); override;

    procedure CreateCommand; virtual;
    procedure FreeCommand;
    procedure SetCommand(Value: TCustomDASQL);

    function CreateDataTypeMap: TDADataSetMapRules; virtual;
    procedure SetDataTypeMap(Value: TDAMapRules);

    procedure SetDataSetService(Value: TDataSetService); override;

    function CreateOptions: TDADataSetOptions; virtual;
    function CreateEncryption: TDAEncryption; virtual;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetConnection(Value: TCustomDAConnection);
    function UsedConnection: TCustomDAConnection; virtual;
    procedure CheckConnection; virtual;
    procedure BeginConnection(NoConnectCheck: boolean = True); virtual;
    procedure EndConnection; virtual;
    procedure ConnectRequest;
    procedure DisconnectRequest;
    procedure Disconnect(NeedClose: boolean = True); virtual;
    procedure ConnectChange(Sender: TObject; Connecting: boolean); virtual;

    function IsTransactionStored: boolean;
    function GetTransaction: TDATransaction; virtual;
    procedure SetTransaction(Value: TDATransaction); virtual;
    function GetUsedTransaction: TDATransaction;
    function UsedTransaction: TDATransaction; virtual;
    function UsedUpdateTransaction: TDATransaction; virtual;

  { Fields }
    procedure SetKeyFields(const Value: string); virtual;
    function GetFieldTypeMapClass: TFieldTypeMapClass; override;
    function GetFieldType(FieldDesc: TFieldDesc; out FieldSize, FieldLength, FieldScale: Integer): TFieldType; override;
    procedure FillExtFieldsInfo;

  { TablesInfo }
    function GetTablesInfo: TCRTablesInfo;
    function GetSQLInfo: TSQLInfo;
    procedure SetUpdatingTable(const Value: string); virtual;
    function QuoteName(const AName: string): string;
    function GetDBKeyList(const TableName, IndexName: string): string;

  { Open/Close }
    procedure SetActive(Value: Boolean); override;
    procedure BeforeOpenCursor(InfoQuery: boolean); virtual;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure AfterOpenCursor(InfoQuery: boolean); virtual;
    procedure CloseCursor; override;
    function GetCursor: TCRCursor;
    function GetCRCursor: TCRCursor; virtual;
    procedure SetCRCursor(Value: TCRCursor); virtual;

    procedure GetCurrentKeys(out KeyFieldDescs: TFieldDescArray; out KeyFields: TFieldArray);
    procedure GetCurrentValues(const KeyFieldDescs: TFieldDescArray; out Values: variant);
    procedure GetCurrentKeysAndValues(out KeyFieldDescs: TFieldDescArray; out KeyFields: TFieldArray; out Values: variant);
    procedure DataReopen; override;
    procedure InternalRefresh; override;
    procedure InternalRefreshQuick(const CheckDeleted: boolean); virtual;
    procedure InternalExecute(Iters: integer; Offset: integer); virtual;
    procedure InternalClose; override;
    procedure DoAfterOpen; override;

    procedure SetDMLRefresh(Value: boolean);
    procedure SetRefreshOptions(Value: TRefreshOptions); virtual;
    function GetFetchAll: boolean; virtual;
    procedure SetFetchAll(Value: boolean); virtual;
    function GetNonBlocking: boolean; virtual;
    procedure SetNonBlocking(Value: boolean); virtual;
    function SQLAutoGenerated: boolean; virtual;

    function DoOpenNext: boolean;

    procedure QuickOpen(var Info: TQuickOpenInfo; Refresh: boolean = False); virtual;
    procedure Restore(const Info: TQuickOpenInfo; RestoreActive: boolean = True); virtual;

  { Edit }
    procedure SetReadOnly(Value: boolean); virtual;

    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalInsert; override;
    procedure InternalCancel; override;
    procedure InternalPost; override;
    procedure InternalDeferredPost; override;

    function GetUpdateSQLStatementTypes: TStatementTypes; virtual;
    function GetUpdateSQLIndex(Index: integer): TStrings;
    procedure SetUpdateSQLIndex(Index: integer; Value: TStrings);

    procedure SetFilterSQL(const Value: string); virtual; //CR ODAC (8387) vs (8751)
    procedure SetFiltered(Value: boolean); override;

    function LocateRecord(KeyFields: TList; KeyValues: variant;
      Options: TLocateExOptions; SavePos: boolean): boolean; override;

    function GetCanModify: boolean; override;
  {$IFNDEF FPC}
    procedure SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); override; // Need to support int64 fields on PerformSQL in RefreshRecord
  {$ENDIF}
    function CanRefreshField(Field: TField): boolean; virtual;
    procedure AssignFieldValue(Param: TDAParam; Field: TField; Old: boolean); overload; virtual;
    procedure AssignFieldValue(Param: TDAParam; FieldDesc: TFieldDesc; Old: boolean); overload; virtual;
    procedure AssignFieldType(Param: TDAParam; FieldDesc: TFieldDesc); virtual;
    procedure SetDefaultExpressionValues; override;

  { Master/Detail }
    function UseLocalMasterDetailFilter: boolean; override;
    function NeedDetailRefresh(Param: TDAParam; FieldValue: TSharedObject): boolean; virtual;
    function MDLinksRefreshed(Field: TField): boolean; override;
    procedure RefreshDetail(Sender: TObject); override;
    function SetMasterParams(AParams: TDAParams; MasterField: TField): boolean;
    procedure MDPropertiesChanged; override;
    function SplitFieldName(const Fields: string; var Pos: Integer): string; override;
    procedure InitMasterParams;

    procedure AssembleSQL;
    function GetForceSPInit: boolean;
    procedure InternalCreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean = False);
    procedure ScanMacros(Sender: TObject {$IFNDEF FPC}= nil{$ENDIF}); virtual;

    procedure DefineProperties(Filer: TFiler); override;

    procedure AssignTo(Dest: TPersistent); override;

    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute(Result: boolean); virtual;
    procedure DoAfterExecFetch(Result: boolean);
    procedure DoAfterFetchAll(Result: boolean);
    procedure DoAfterScroll; override;
    procedure DoOnBeforeFetch(var Cancel: boolean); virtual;
    procedure DoOnAfterFetch; virtual;
    procedure DoOnReopen;
    procedure DoOnFieldsChanged;
    function GetKeyValuesSQL: string;
    function GetDataValuesSQL: string;

    function GetRecordCount: integer; override;
    function GetIsQuery: boolean; virtual;

  { Before / After UpdateExecute }
    function AssignedBeforeUpdateExecute: boolean; virtual;
    procedure DoBeforeUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
      Params: TDAParams); virtual;
    function AssignedAfterUpdateExecute: boolean; virtual;
    procedure DoAfterUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
      Params: TDAParams); virtual;

  { Open }
    procedure InternalOpen; override;

  { SQL Modifications }
    function GetParserClass: TSQLParserClass; virtual;
    function SQLGetFrom(const SQLText: string): string; virtual;
    function SQLGetWhere(const SQLText: string): string; virtual;
    function SQLAddWhere(const SQLText, Condition: string): string; virtual;
    function SQLDeleteWhere(const SQLText: string): string; virtual;
    function SQLGetOrderBy(const SQLText: string): string; virtual;
    function SQLSetOrderBy(const SQLText, Fields: string): string; virtual;

    procedure CheckSQL; virtual;
    function GetFinalSQL: string; virtual;

  { Misc }
  {$IFDEF FPC}
    procedure DataEvent(Event: TDataEvent; Info: PtrInt); override;
  {$ELSE}{$IFDEF VER16P}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
  {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  {$ENDIF}{$ENDIF}

    function GetNextRecord: Boolean; override;
    function GetPriorRecord: Boolean; override;

    function LocateEx(const KeyFields: array of TField;
      const KeyFieldDescs: array of TCRFieldDesc; const KeyValues: variant;
      Options: TLocateExOptions): boolean; overload;

    procedure EmptyTable(const TableName: string);

    procedure SetConditions(Value: TDAConditions);

   { TablesInfo }
    property TablesInfo: TCRTablesInfo read GetTablesInfo;
    property SQLInfo: TSQLInfo read GetSQLInfo;
    property UpdatingTable: string read FUpdatingTable write SetUpdatingTable; // Does not need for public use
    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;
    property UpdateTransaction: TDATransaction read FUpdateTransaction write SetUpdateTransaction;
    property AutoCommit: boolean read FAutoCommit write SetAutoCommit default True;
    property FetchAll: boolean read GetFetchAll write SetFetchAll default False;
    property NonBlocking: boolean read GetNonBlocking write SetNonBlocking default False;
    property UpdateObject: TCustomDAUpdateSQL read FUpdateObject write SetUpdateObject;
    property DMLRefresh: boolean read FDMLRefresh write SetDMLRefresh default False;
    property LockMode: TLockMode read FLockMode write FLockMode default lmNone;
    property Cursor: TCRCursor read GetCRCursor write SetCRCursor;
    property SmartFetch: TSmartFetchOptions read FSmartFetchOptions write SetSmartFetchOptions;
    property Encryption: TDAEncryption read FEncryption write SetEncryption;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  { Open/Close }
    procedure Prepare; override;
    procedure UnPrepare; override;

    procedure Execute; overload; virtual;
    procedure Execute(Iters: integer; Offset: integer = 0); overload; virtual;
    procedure ExecSQL;  // for BDE compatibility
    function Executing: boolean;
    function Fetching: boolean;
    function FetchingAll: boolean;
    function Fetched: boolean; virtual;
    procedure BreakExec; virtual;

    function GetFieldDesc(const FieldName, TableName: string): TFieldDesc; overload;

  {$IFDEF VER17P}
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList<TField>); override;
  {$ELSE}
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
  {$ENDIF}

    {for BDE compatibility}
    function FindKey(const KeyValues: array of const): Boolean;
    procedure FindNearest(const KeyValues: array of const);
    procedure GotoCurrent(DataSet: TCustomDADataSet);

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

  { Edit }
    procedure ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds); override;

    property SQLInsert: TStrings index Integer(stInsert) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLDelete: TStrings index Integer(stDelete) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLUpdate: TStrings index Integer(stUpdate) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLRefresh: TStrings index Integer(stRefresh) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLLock: TStrings index Integer(stLock) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLRecCount: TStrings index Integer(stRecCount) read GetUpdateSQLIndex write SetUpdateSQLIndex;

    procedure RefreshRecord;
    procedure Lock; virtual;
    procedure UnLock;

    function FindParam(const Value: string): TDAParam;
    function ParamByName(const Value: string): TDAParam;

    function FindMacro(const Value: string): TMacro;
    function MacroByName(const Value: string): TMacro;

  { SQL Modifications }
    procedure SaveSQL;
    procedure RestoreSQL;
    function SQLSaved: boolean;

    procedure AddWhere(const Condition: string);
    procedure DeleteWhere;
    procedure SetOrderBy(const Fields: string);
    function GetOrderBy: string;

  { Additional data types }
    function GetField(FieldDesc: TFieldDesc): TField;
    function GetDataType(const FieldName: string): integer; virtual;
    function GetFieldPrecision(const FieldName: string): integer;
    function GetFieldScale(const FieldName: string): integer;
    function GetFieldObject(const FieldName: string): TSharedObject; overload;
    procedure GetKeyFieldNames(List: TStrings);

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property ParamCheck: boolean read GetParamCheck write SetParamCheck default True; // before SQL
    property SQL: TStrings read GetSQL write SetSQL;
    property FetchRows: integer read FFetchRows write SetFetchRows default 25;
    property Debug: boolean read FDebug write FDebug default False;
    property Params: TDAParams read GetParams write SetParams stored False;
    property ParamCount: word read GetParamCount;
    property Macros: TMacros read GetMacros write SetMacros stored False;
    property MacroCount: word read GetMacroCount;
    property UniDirectional: boolean read FUniDirectional write SetUniDirectional default False;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;
    property RowsAffected: integer read GetRowsAffected;
    property ParamsProcessed: integer read GetParamsProcessed;
    property IsQuery: boolean read GetIsQuery;
    property RefreshOptions: TRefreshOptions read FRefreshOptions write SetRefreshOptions default [];
    property Options: TDADataSetOptions read FOptions write SetOptions;
    property BaseSQL: string read GetBaseSQL;
    property FinalSQL: string read GetFinalSQL;
    property FilterSQL: string read FFilterSQL write SetFilterSQL;
    property KeyFields: string read FKeyFields write SetKeyFields;
    property Disconnected: boolean read FDisconnected write FDisconnected;
    property MasterSource;
    property MasterFields;
    property DetailFields;

    property DataTypeMap: TDAMapRules read FDataTypeMap write SetDataTypeMap stored IsMapRulesStored;
    property Conditions: TDAConditions read FWhereConditions write SetConditions stored False;

    property BeforeExecute: TBeforeExecuteEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TAfterExecuteEvent read FAfterExecute write FAfterExecute;
    property BeforeUpdateExecute: TUpdateExecuteEvent read FBeforeUpdateExecute write FBeforeUpdateExecute;
    property AfterUpdateExecute: TUpdateExecuteEvent read FAfterUpdateExecute write FAfterUpdateExecute;
    property BeforeFetch: TBeforeFetchEvent read FBeforeFetch write FBeforeFetch;
    property AfterFetch: TAfterFetchEvent read FAfterFetch write FAfterFetch;
    property CheckMode: TCheckMode read FCheckMode write SetCheckMode default cmException;
  end;

{ TCustomDASQL }

  TCustomDASQL = class (TComponent)
  private
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FSQL: TStrings;
    FParams: TDAParams;
    FParamCheck: boolean;
    FMacros: TMacros;
    FDebug: boolean;
    FChangeCursor: boolean;
    FSQLModified: boolean;
    FBatchIters, FBatchOffset: integer;

    FBeforeExecute: TBeforeExecuteEvent;
    FAfterExecute: TAfterExecuteEvent;
    {FOnDisconnect: TNotifyEvent;
    FGetFinalSQL: TGetFinalSQLEvent;
    FOnScanMacros: TNotifyEvent;}

    procedure SetTransaction(Value: TDATransaction);
    procedure SetSQL(Value: TStrings);
    function GetPrepared: boolean;
    procedure SetPrepared(Value: boolean);
    procedure SetParams(Value: TDAParams);
    function GetParamCount: word;
    procedure SetParamCheck(Value: boolean);
    function GetParamValues(const ParamName: string): variant;
    procedure SetParamValues(const ParamName: string; const Value: variant);
    procedure SetMacros(Value: TMacros);
    function GetMacroCount: word;
    function GetRowsAffected: integer;
    function GetParamsProcessed: integer;

  protected
    FAutoCommit: boolean;
    FICommand: TCRCommand;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TCustomDADataSet; // dataset that owns
    FDesignCreate: boolean;  // for design-time only
    FNonBlocking: boolean;
    FLockDebug: boolean; // locking trans debug info
    FLockAssembleSQL, FLockMacros, FLockScanParams: boolean;
    FStoredProcName: string;
    FStoredProcIsQuery: boolean;
    FIsSPInit: boolean;
    FLastInsertId: int64;

    function IsTransactionStored: boolean;

    procedure CreateICommand; virtual;
    procedure FreeICommand;
    procedure SetICommand(Value: TCRCommand); virtual;
    procedure CheckICommand;

    function CreateParamsObject: TDAParams; virtual;
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; virtual;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetAutoCommit(Value: boolean);
    function GetNonBlocking: boolean; virtual;
    procedure SetNonBlocking(Value: boolean); virtual;
    procedure SetConnection(Value: TCustomDAConnection);
    function UsedConnection: TCustomDAConnection; virtual;
    procedure CheckConnection; virtual;
    procedure BeginConnection(NoConnectCheck: boolean = True); virtual;
    procedure EndConnection; virtual;
    procedure Disconnect(NeedClose: boolean = True); virtual;
    procedure ConnectChange(Sender: TObject; Connecting: boolean); virtual;

    function GetTransaction: TDATransaction; virtual;
    function UsedTransaction: TDATransaction; virtual;

    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure InternalExecute(Iters: integer; Offset: integer); virtual;
    procedure InternalCreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean = False); virtual;
    function GetForceSPInit: boolean;

    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute(Result: boolean); virtual;

    // for Script
    function ParseSQL(const SQL: string; Params: TDAParams): string; virtual;

    procedure SQLChanged(Sender: TObject);
    procedure ProcessSQLChanged(LockMacros, SaveBaseSQL: boolean);
    procedure ScanMacros; virtual;
    function GetFinalSQL: string; virtual;
    procedure SetICommandSQL;
    procedure AssembleSQL; virtual;

    function NeedRecreateProcCall: boolean; virtual;
    procedure CheckSQL(Iters: integer = 1); virtual;

    function IsInOutParamSupported: boolean; virtual;
    function NeedConvertEOLForBlob: boolean; virtual;
    procedure AssignParam(ParamDesc: TParamDesc; Param: TDAParam); virtual;
    procedure AssignParamValue(ParamDesc: TParamDesc; Param: TDAParam); virtual;
    procedure AssignParamDesc(Param: TDAParam; ParamDesc: TParamDesc); virtual;
    procedure AssignParamDescValue(Param: TDAParam; ParamDesc: TParamDesc); virtual;
    procedure CreateParams; overload; virtual;
    procedure CreateParams(Params: TDAParams; ParamDescs: TParamDescs); overload; virtual;
    procedure WriteParams(WriteValue: boolean = True); virtual;
    procedure ReadParams; virtual;
    procedure UpdateParams;
    function FindResultParam: TDAParam; virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    procedure ReadMacroData(Reader: TReader);
    procedure WriteMacroData(Writer: TWriter);
    procedure ReadStoredProcName(Reader: TReader);
    procedure WriteStoredProcName(Writer: TWriter);
    procedure SetStoredProcName(const StoredProcName: string);
    procedure ReadStoredProcIsQuery(Reader: TReader);
    procedure WriteStoredProcIsQuery(Writer: TWriter);

    procedure AssignTo(Dest: TPersistent); override;

    property AutoCommit: boolean read FAutoCommit write SetAutoCommit default False;
    property NonBlocking: boolean read GetNonBlocking write SetNonBlocking default False;
    property StoredProcIsQuery: boolean read FStoredProcIsQuery write FStoredProcIsQuery;
    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Prepare; virtual;
    procedure UnPrepare; virtual;

    procedure Execute; overload; virtual;
    procedure Execute(Iters: integer; Offset: integer = 0); overload; virtual;

    procedure BreakExec;
    function Executing: boolean;
    function WaitExecuting(TimeOut: integer = 0): boolean;

    function FindParam(const Value: string): TDAParam;
    function ParamByName(const Value: string): TDAParam;

    function FindMacro(const Value: string): TMacro;
    function MacroByName(const Value: string): TMacro;

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property ParamCheck: boolean read FParamCheck write SetParamCheck default True; // before SQL
    property SQL: TStrings read FSQL write SetSQL;
    property Prepared: boolean read GetPrepared write SetPrepared;
    property Params: TDAParams read FParams write SetParams stored False;
    property ParamCount: word read GetParamCount;
    property ParamValues[const ParamName: string]: Variant read GetParamValues write SetParamValues; default;
    property Macros: TMacros read FMacros write SetMacros stored False;
    property MacroCount: word read GetMacroCount;
    property Debug: boolean read FDebug write FDebug default False;
    property ChangeCursor: boolean read FChangeCursor write FChangeCursor;
    property RowsAffected: integer read GetRowsAffected;
    property ParamsProcessed: integer read GetParamsProcessed;
    property FinalSQL: string read GetFinalSQL;

    property BeforeExecute: TBeforeExecuteEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TAfterExecuteEvent read FAfterExecute write FAfterExecute;
  end;

{ TDAMetaData }

  TDAMetaData = class(TMemDataSet)
  private
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FMetaDataKind: string;
    FRestrictions: TStrings;
    FIMetaData: TCRMetaData;
    FDesignCreate: boolean;

    procedure SetConnection(Value: TCustomDAConnection);
    procedure ConnectChange(Sender: TObject; Connecting: boolean);
    procedure SetMetaDataKind(const Value: string);
    procedure SetRestrictions(Value: TStrings);
    procedure RestrictionsChanged(Sender: TObject);

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function UsedConnection: TCustomDAConnection;
    function UsedTransaction: TDATransaction; virtual;
    function GetTransaction: TDATransaction; virtual;
    procedure SetTransaction(Value: TDATransaction); virtual;
    function IsTransactionStored: boolean;
    procedure BeginConnection; virtual;
    procedure EndConnection; virtual;
    procedure CheckIMetaData;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure InternalOpen; override;
    procedure CloseCursor; override;

    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetMetaDataKinds(List: TStrings);
    procedure GetRestrictions(List: TStrings; const MetaDataKind: string);

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property MetaDataKind: string read FMetaDataKind write SetMetaDataKind;
    property Restrictions: TStrings read FRestrictions write SetRestrictions;
  end;

{ TCustomDAUpdateSQL }

  TCustomDAUpdateSQL = class (TComponent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TCustomDADataSet;
    FSQLText: array [TStatementType] of TStrings;

    FUpdateObject: array [TStatementType] of TComponent;

  protected
    FDesignCreate: boolean;  // for design-time only

    // get/set FSQLText by TStatementType
    function GetSQLIndex(Index: integer): TStrings;
    procedure SetSQLIndex(Index: integer; Value: TStrings);

    // get/set FSQLText by TUpdateKind
    function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);

    // get/set FUpdateObject by TStatementType
    function GetObjectIndex(Index: integer): TComponent;
    procedure SetObjectIndex(Index: integer; Value: TComponent);

    function GetDataSet: TCustomDADataSet; virtual;
    procedure SetDataSet(DataSet: TCustomDADataSet); virtual;
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

    function DataSetClass: TCustomDADataSetClass; virtual;
    function SQLClass: TCustomDASQLClass; virtual;
    procedure CheckUpdateComponent(Component: TComponent); overload;
    procedure CheckUpdateComponent(Component: TComponent; NewDataset: TCustomDADataset); overload;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind); virtual;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    property DataSet: TCustomDADataSet read GetDataSet write SetDataSet;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;

  published
    property InsertSQL: TStrings index Integer(stInsert) read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index Integer(stDelete) read GetSQLIndex write SetSQLIndex;
    property ModifySQL: TStrings index Integer(stUpdate) read GetSQLIndex write SetSQLIndex;
    property RefreshSQL: TStrings index Integer(stRefresh) read GetSQLIndex write SetSQLIndex;
    property LockSQL: TStrings index Integer(stLock) read GetSQLIndex write SetSQLIndex;

    property InsertObject: TComponent index Integer(stInsert) read GetObjectIndex write SetObjectIndex;
    property DeleteObject: TComponent index Integer(stDelete) read GetObjectIndex write SetObjectIndex;
    property ModifyObject: TComponent index Integer(stUpdate) read GetObjectIndex write SetObjectIndex;
    property RefreshObject: TComponent index Integer(stRefresh) read GetObjectIndex write SetObjectIndex;
    property LockObject: TComponent index Integer(stLock) read GetObjectIndex write SetObjectIndex;
  end;

{ TMacro }

  TMacro = class (TCollectionItem)
  private
    FName: string;
    FValue: string;
    FActive: boolean;
    //FOwner: TComponent;

    procedure SetValue(const Value: string);
    procedure SetActive(Value: boolean);

    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetAsFloat: double;
    procedure SetAsFloat(Value: double);
    function GetAsInteger: integer;
    procedure SetAsInteger(Value: integer);
    function GetAsString: string;
    procedure SetAsString(const Value: string);

  protected
    procedure AssignTo(Dest: TPersistent); override;

    function IsEqual(Value: TMacro): boolean;

    function GetDisplayName: string; override;

  public
    constructor Create(Collection: TCollection); override;

    procedure Clear;

    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;

  published
    property Name: string read FName write FName;
    property Value: string read FValue write SetValue;
    property Active: boolean read FActive write SetActive default True;
  end;

{ TMacros }

  TMacros = class (TCollection)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TPersistent;

    procedure ReadBinaryData(Stream: TStream);
    //procedure WriteBinaryData(Stream: TStream);

    function GetItem(Index: integer): TMacro;
    procedure SetItem(Index: integer; Value: TMacro);
    procedure NotifyOwner(Item: TMacro);

  protected
    FParserClass: TSQLParserClass;

    procedure AssignTo(Dest: TPersistent); override;

    procedure DefineProperties(Filer: TFiler); override;

    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

    function GetMacroValue(Macro: TMacro): string; virtual;

  public
    constructor Create(Owner: TPersistent);

    procedure Scan(const SQL: string); // ParseSQL
    procedure AssignValues(Value: TMacros);
    function IsEqual(Value: TMacros): boolean;

    function FindMacro(const Value: string): TMacro;
    function MacroByName(const Value: string): TMacro;

    procedure Expand(var SQL: string);

    procedure SetParserClass(Value: TSQLParserClass);

    property Items[Index: integer]: TMacro read GetItem write SetItem; default;
  end;

{ TCustomConnectDialog }

  TLabelSet = (lsCustom, lsEnglish, lsFrench, lsGerman, lsItalian, lsPolish,
    lsPortuguese, lsRussian, lsSpanish);

  TConnectDialogOptionKind = (okServer, okUserName, okPassword, okDatabase,
    okPort, okDirect, okAuthentication, okRole, okClientLibrary, okProtocol,
    okSchema, okHome);

  TConnectDialogOption = class (TPersistent)
  private
    FOwner: TCustomConnectDialog;
    FKind: TConnectDialogOptionKind;
    FCaption: string;
    FVisible: Boolean;
    FOrder: Integer;
    procedure SetCaption(Value: string);
    procedure SetVisible(Value: Boolean);
    procedure SetOrder(Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TCustomConnectDialog; OptionKind: TConnectDialogOptionKind; Order: Integer; Visible: Boolean = True);
    property Kind: TConnectDialogOptionKind read FKind;
  published
    property Caption: string read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible;
    property Order: Integer read FOrder write SetOrder;
  end;
  TConnectDialogOptionArray = array of TConnectDialogOption;

  TCustomConnectDialog = class (TComponent)
  private
    FConnection: TCustomDAConnection;
    FRetries: word;
    FDialogClass: string;
    FSavePassword: boolean;
    FStoreLogInfo: boolean;
    FUseServerHistory: boolean;
    FNeedConnect: boolean;

    FCaption: string;
    FConnectButton: string;
    FCancelButton: string;

    FServerOption: TConnectDialogOption;
    FUserNameOption: TConnectDialogOption;
    FPasswordOption: TConnectDialogOption;

    procedure SetCaption(Value: string);
    function GetUserNameLabel: string;
    procedure SetUserNameLabel(Value: string);
    function GetPasswordLabel: string;
    procedure SetPasswordLabel(Value: string);
    function GetServerLabel: string;
    procedure SetServerLabel(Value: string);
    procedure SetConnectButton(Value: string);
    procedure SetCancelButton(Value: string);

    procedure SetServerOption(Value: TConnectDialogOption);
    procedure SetUserNameOption(Value: TConnectDialogOption);
    procedure SetPasswordOption(Value: TConnectDialogOption);
  protected
    FLabelSet: TLabelSet;
    InSetLabelSet: Boolean;
    FServerEnumerator: TCRServerEnumerator;

    function GetServerEnumeratorClass: TCRServerEnumeratorClass; virtual;
    procedure SetServerEnumerator(Value: TCRServerEnumerator); virtual;
    procedure CreateServerEnumerator;
    procedure FreeServerEnumerator;
    procedure CheckServerEnumerator;

  {$IFDEF MSWINDOWS}
    function GetString(Id: integer): string;
  {$ENDIF}
    procedure SetLabelSet(Value: TLabelSet); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function DefDialogClass: TClass; virtual;

  {$IFDEF MSWINDOWS}
    function GetKeyPath: string; virtual;
    function GetServerStoreName: string; virtual;
    function GetApplicationKeyPath: string;
    function GetServerListKeyPath: string; virtual;
    procedure SaveServerListToRegistry; virtual;
    procedure LoadServerListFromRegistry(List: TStrings); virtual;
    procedure SaveInfoToRegistry(Registry: TRegistry); virtual;
    procedure LoadInfoFromRegistry(Registry: TRegistry); virtual;
  {$ENDIF}
    //class function AcceptBlankPassword: boolean; virtual;

    procedure ReadServerCaptionProperty(Reader: TReader);
    procedure ReadUserNameCaptionProperty(Reader: TReader);
    procedure ReadPasswordCaptionProperty(Reader: TReader);
    procedure ReadSavePasswordProperty(Reader: TReader);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function Execute: boolean; virtual;

    procedure GetServerList(List: TStrings); virtual;

    procedure GetOptions(var Options: TConnectDialogOptionArray; Ordered: Boolean = True); virtual;
    procedure OptionChanged;

    property Connection: TCustomDAConnection read FConnection;

    property Retries: word read FRetries write FRetries default 3;
    property SavePassword: boolean read FSavePassword write FSavePassword default False;
    property StoreLogInfo: boolean read FStoreLogInfo write FStoreLogInfo default True;

    property DialogClass: string read FDialogClass write FDialogClass;

    property Caption: string read FCaption write SetCaption;
    property UsernameLabel: string read GetUserNameLabel write SetUserNameLabel;
    property PasswordLabel: string read GetPasswordLabel write SetPasswordLabel;
    property ServerLabel: string read GetServerLabel write SetServerLabel;
    property ConnectButton: string read FConnectButton write SetConnectButton;
    property CancelButton: string read FCancelButton write SetCancelButton;

    property Server: TConnectDialogOption read FServerOption write SetServerOption;
    property UserName: TConnectDialogOption read FUserNameOption write SetUserNameOption;
    property Password: TConnectDialogOption read FPasswordOption write SetPasswordOption;

    property LabelSet: TLabelSet read FLabelSet write SetLabelSet default lsEnglish;

    // transferred from protected section
    property UseServerHistory: boolean read FUseServerHistory write FUseServerHistory
      default{$IFDEF MSWINDOWS} True {$ELSE} False {$ENDIF};
  end;

  TTableInfo = record
    Name: string;
    Alias: string;
  end;
  TTablesInfo = array of TTableInfo;

type
{ TCRDataSource }

  TCRDataSource = class (TDataSource)
  protected
    FDesignCreate: boolean;

    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TComponent); override;
  end;

  TDBAccessUtils = class
  public
    class function IsSharedObjectDataType(Obj: TDAParam; DataType: TFieldType): boolean;
    class function IsBlobDataType(Obj: TDAParam; DataType: TFieldType): boolean;
    class function GetNational(Obj: TDAParam): boolean;

    class procedure CheckConnection(Obj: TCustomDADataSet); overload;
    class procedure CheckConnection(Obj: TCustomDASQL); overload;
    class function UsedConnection(Obj: TCustomDADataSet): TCustomDAConnection; overload;
    class function UsedConnection(Obj: TCustomDASQL): TCustomDAConnection; overload;
    class function UsedConnection(Obj: TDAMetaData): TCustomDAConnection; overload;
    class function UsedConnection(Obj: TComponent): TCustomDAConnection; overload;
    class procedure SetAutoCommit(Obj: TComponent; Value: boolean);
    class function GetAutoCommit(Obj: TCustomDAConnection): boolean; overload;
    class function GetAutoCommit(Obj: TCustomDADataSet): boolean; overload;
    class function GetAutoCommit(Obj: TCustomDASQL): boolean; overload;

    class procedure SetDesignCreate(Obj: TDATransaction; Value: boolean); overload;
    class function GetDesignCreate(Obj: TDATransaction): boolean; overload;
    class procedure SetDesignCreate(Obj: TCustomDADataSet; Value: boolean); overload;
    class function GetDesignCreate(Obj: TCustomDADataSet): boolean; overload;
    class procedure SetDesignCreate(Obj: TCustomDASQL; Value: boolean); overload;
    class function GetDesignCreate(Obj: TCustomDASQL): boolean; overload;
    class procedure SetDesignCreate(Obj: TCustomDAUpdateSQL; Value: boolean); overload;
    class function GetDesignCreate(Obj: TCustomDAUpdateSQL): boolean; overload;
    class procedure SetDesignCreate(Obj: TDAMetaData; Value: boolean); overload;
    class function GetDesignCreate(Obj: TDAMetaData): boolean; overload;
    class procedure SetDesignCreate(Obj: TCRDataSource; Value: boolean); overload;
    class function GetDesignCreate(Obj: TCRDataSource): boolean; overload;
    class procedure SetLockLoginPrompt(Obj: TCustomDAConnection; Value: Boolean);

    class function GetIConnection(Obj: TCustomDAConnection): TCRConnection;
    class procedure CreateIConnection(Obj: TCustomDAConnection);
    class function GetUpdateQuery(Obj: TCustomDADataSet): TComponent;
    class function GetTablesInfo(Obj: TCustomDADataSet): TCRTablesInfo;
    class function GetSQLInfo(Obj: TCustomDADataSet): TSQLInfo;
    class function GetUpdatingTable(Obj: TCustomDADataSet): string;
    class procedure SetUpdatingTable(Obj: TCustomDADataSet; Value: string);
    class procedure InternalConnect(Obj: TCustomDAConnection);
    class procedure InternalDisconnect(Obj: TCustomDAConnection);
    class procedure DisconnectTransaction(Obj: TCustomDAConnection);
    class procedure SuppressAutoCommit(Obj: TCustomDAConnection);
    class procedure RestoreAutoCommit(Obj: TCustomDAConnection);
    class function IsMultipleTransactionsSupported(Obj: TCustomDAConnection): boolean;
    class function PushOperation(Obj: TCustomDAConnection; Operation: TConnLostCause; AllowFailOver: boolean = true): integer;
    class function PopOperation(Obj: TCustomDAConnection): TConnLostCause;
    class procedure RestoreAfterFailOver(Obj: TCustomDAConnection);
    class function IsFailOverAllowed(Obj: TCustomDAConnection): boolean;

    class function UsedTransaction(Obj: TCustomDAConnection): TDATransaction; overload;
    class function UsedTransaction(Obj: TCustomDADataSet): TDATransaction; overload;
    class function UsedTransaction(Obj: TCustomDASQL): TDATransaction; overload;
    class function UsedTransaction(Obj: TComponent): TDATransaction; overload;

    class function GetTransaction(Obj: TCustomDADataSet): TDATransaction; overload;
    class function GetTransaction(Obj: TCustomDASQL): TDATransaction; overload;
    class function GetTransaction(Obj: TDAMetaData): TDATransaction; overload;
    class function GetDefaultTransaction(Obj: TCustomDAConnection): TDATransaction;

    class procedure SetTransaction(Obj: TCustomDADataSet; Value: TDATransaction); overload;
    class procedure SetTransaction(Obj: TCustomDASQL; Value: TDATransaction); overload;
    class procedure SetTransaction(Obj: TDAMetaData; Value: TDATransaction); overload;
    class procedure SetDefaultTransaction(Obj: TCustomDAConnection; Value: TDATransaction);

    class function GetFTransaction(Obj: TCustomDADataSet): TDATransaction; overload;
    class function GetFTransaction(Obj: TCustomDASQL): TDATransaction; overload;
    class function GetFTransaction(Obj: TDAMetaData): TDATransaction; overload;
    class function GetFDefaultTransaction(Obj: TCustomDAConnection): TDATransaction;

    class function GetITransaction(Obj: TDATransaction): TCRTransaction;
    class function GetConnectionCount(Obj: TDATransaction): integer;
    class function GetConnection(Obj: TDATransaction; Index: integer): TCustomDAConnection;
    class procedure Savepoint(Obj: TDATransaction; const Name: string);
    class procedure RollbackToSavepoint(Obj: TDATransaction; const Name: string);
    class procedure ReleaseSavepoint(Obj: TDATransaction; const Name: string);
    class procedure CommitRetaining(Obj: TDATransaction);
    class procedure RollbackRetaining(Obj: TDATransaction);
    class procedure GainTransaction(Obj: TDATransaction);
    class procedure ReleaseTransaction(Obj: TDATransaction);
    class procedure AutoCommitTransaction(Obj: TDATransaction; NeedCommit: boolean);
    class function GetMultiTransactionID(Obj: TDATransaction): Int64;

    class procedure Disconnect(Obj: TCustomDASQL);

    class function SQLGenerator(Obj: TCustomDADataSet): TDASQLGenerator;
    class function GetSQLs(Obj: TCustomDAConnection): TList;

    class procedure GetKeyAndDataFields(
      Obj: TCustomDADataSet;
      out KeyAndDataFields: TKeyAndDataFields;
      const ForceUseAllKeyFields: boolean);

    class function GetLockDebug(Obj: TComponent): boolean;
    class procedure SetLockDebug(Obj: TComponent; Value: boolean);
    class function FOwner(Obj: TDAConnectionOptions): TCustomDAConnection; overload;
    class function FOwner(Obj: TDADataSetOptions): TCustomDADataSet; overload;
    class function SQLMonitorClass(Obj: TCustomDAConnection): TClass;
    class function ConnectDialogClass(Obj: TCustomDAConnection): TConnectDialogClass;
    class function QuoteName(Obj: TCustomDADataSet; const AName: string): string;

    class procedure RegisterClient(Obj: TCustomDAConnection; Client: TObject; Event: TConnectChangeEvent = nil);
    class procedure UnRegisterClient(Obj: TCustomDAConnection; Client: TObject);

    class function GetIdentityField(Obj: TCustomDADataSet): TCRFieldDesc;

    class function GetSQL(Obj: TComponent): TStrings;
    class procedure SetSQL(Obj: TComponent; Value: TStrings);
    class function GetSQLText(Obj: TComponent): string;
    class procedure SetSQLText(Obj: TComponent; const SQLText: string; const LockScanParams, LockMacros: boolean);

    class function GetParams(Obj: TComponent): TDAParams;
    class procedure Execute(Obj: TComponent);
    class procedure Open(Obj: TComponent);
    class function GetRowsAffected(Obj: TComponent): integer;
    class function GetParamsProcessed(Obj: TComponent): integer;
    class function GetUpdateSQLStatementTypes(Obj: TCustomDADataSet): TStatementTypes;
    class function GetUpdateSQLIndex(Obj: TCustomDADataSet; StatementType: TStatementType): TStrings;
    class function ParseSQL(Obj: TCustomDASQL; const SQL: string; Params: TDAParams): string;
    class function CreateParamsObject(Obj: TCustomDASQL): TDAParams;

    class procedure SetDesigning(Obj: TComponent; Value: Boolean; SetChildren: Boolean = True);
    class function GetIRecordSet(Obj: TCustomDADataSet): TCRRecordSet;
    class procedure CheckIRecordSet(Obj: TCustomDADataSet);
    class function GetICommand(Obj: TComponent): TCRCommand; overload;
    class function GetICommand(Obj: TCustomDADataSet): TCRCommand; overload;
    class function GetICommand(Obj: TCustomDASQL): TCRCommand; overload;
    class function GetUpdater(Obj: TCustomDADataSet): TDADataSetUpdater;
    class function GetDataSetService(Obj: TCustomDADataSet): TDADataSetService;

    class function GetDataSetClass(Obj: TCustomDAUpdateSQL): TCustomDADataSetClass;
    class function GetSQLClass(Obj: TCustomDAUpdateSQL): TCustomDASQLClass;
    class function GetFieldTypeMapClass(Obj: TCustomDAConnection): TDAFieldTypeMapClass; overload;
    class function GetFieldTypeMapClass(Obj: TCustomDADataSet): TDAFieldTypeMapClass; overload;

    class function GetParserClass(Obj: TMacros): TSQLParserClass;
  {$IFDEF MSWINDOWS}
    class procedure SaveServerListToRegistry(Obj: TCustomConnectDialog); // used in connection editor
  {$ENDIF}
    class procedure SetConnection(Obj: TCustomConnectDialog; Value: TCustomDAConnection);
    class procedure SetUseServerHistory(Obj: TCustomConnectDialog; Value: boolean);
    class function GetNeedConnect(Obj: TCustomConnectDialog): boolean;
    class procedure SetNeedConnect(Obj: TCustomConnectDialog; Value: boolean);

    class procedure CreateProcCall(Obj: TCustomDASQL; const Name: string; NeedDescribe: boolean; IsQuery: boolean = False); overload;
    class procedure CreateProcCall(Obj: TCustomDADataSet; const Name: string; NeedDescribe: boolean; IsQuery: boolean = False); overload;
    class function GetCommand(Obj: TCustomDAConnection): TCustomDASQL;
    class function GetStreamedConnected(Obj: TCustomDAConnection): boolean;
    class procedure Loaded(Obj: TCustomDAConnection);
    class function GetAsCursor(Obj: TDAParam): TCRCursor;
    class function GetCursor(Obj: TCustomDADataSet): TCRCursor;
    class procedure SetCursor(Obj: TCustomDADataSet; Value: TCRCursor);
    class function GetFetchAll(Obj: TCustomDADataSet): boolean;
    class procedure SetFetchAll(Obj: TCustomDADataSet; Value: boolean);
    class procedure QuickOpen(Obj: TCustomDADataSet; var Info: TQuickOpenInfo);
    class procedure Restore(Obj: TCustomDADataSet; const Info: TQuickOpenInfo);
    class function GetLockMode(Obj: TCustomDADataSet): TLockMode;
    class procedure SetLockMode(Obj: TCustomDADataSet; const Value: TLockMode);
    class function GetLastInsertId(Obj: TCustomDADataSet): int64;
    class function GetFullRefresh(Obj: TCustomDADataSet): boolean;
  end;

const
  crSQLArrow = -30;

var
  ChangeCursor: boolean = True;
  SetCursorProc: procedure(Value: integer);
  ShowConnectFormProc: function(ConnectDialog: TCustomConnectDialog): boolean;
  ShowConnectFormProcFmx: function(ConnectDialog: TCustomConnectDialog): boolean;

  procedure SetCursor(Value: integer);

  function UpdateKindToStatementType(const UpdateKind: TUpdateKind): TStatementType;
  function StatementTypeToUpdateKind(const StatementType: TStatementType): TUpdateKind;
  procedure RecreateParamsRef(Params: TParams);

const
  StatementTypeNames: array[TStatementType] of string = (
    'Select', 'Insert', 'Update', 'Delete', 'Lock', 'Refresh', '', '', '', '', 'Record count');

  BlobTypes = [ftBlob, ftOraBlob, ftGraphic];
  MemoTypes = [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}, ftOraClob, ftFmtMemo];

var
  BaseSQLOldBehavior: boolean = False;
  SQLGeneratorCompatibility: boolean = False;
  ResyncBeforeFetch: boolean = False; // prevents AV on refresh
  BoundParams: boolean = True;
  OldFieldsReadOnly: boolean = False; // remove
  ParamStringAsAnsiString: boolean = False;
  OldCachedUpdateLockMode: boolean = False; // Old behavior LockMode for the CachedUpdate mode
  PreventPSKeyFields: Boolean = True;

implementation

uses
  Math,
{$IFDEF PERF_COUNTER}
  Debug,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Messages,
{$ENDIF}
  CRProps, CRFunctions, DASQLMonitor;

{$IFDEF MSWINDOWS}
{$R *.res}
{$ENDIF}

var
  FieldTypeInfos: TFieldTypeInfos;

procedure RecreateParamsRef(Params: TParams);
var
  i: integer;
  s: string;
  Param: TParam;
begin
  for i := 0 to Params.Count - 1 do begin
    Param := TParam(TCollection(Params).Items[i]);
    s := Param.Name;
    Param.Name := '';
    Params[i].Name := s; // Don't optimize this line!  This is necessary to force ParamRef initialization
  end;
end;

procedure SetCursor(Value: integer);
begin
  if Assigned(SetCursorProc) then
    SetCursorProc(Value);
end;

function UpdateKindToStatementType(const UpdateKind: TUpdateKind): TStatementType;
begin
  case UpdateKind of
    DB.ukModify:
      Result := stUpdate;
    DB.ukInsert:
      Result := stInsert;
    DB.ukDelete:
      Result := stDelete;
    else begin
      Result := stCustom; // To prevent compiler warning
      Assert(False);
    end;
  end;
end;

function StatementTypeToUpdateKind(const StatementType: TStatementType): TUpdateKind;
begin
  case StatementType of
    stUpdate:
      Result := DB.ukModify;
    stInsert:
      Result := DB.ukInsert;
    stDelete:
      Result := DB.ukDelete;
    else
    begin
      Result := DB.ukInsert; // To prevent compiler warning
      Assert(False);
    end;
  end;
end;

{ EDAError }

constructor EDAError.Create(ErrorCode: integer; const Msg: string);
begin
  inherited Create(Msg);

  FErrorCode := ErrorCode;
end;

function EDAError.IsFatalError: boolean;
begin
  Result := False;
end;

function EDAError.IsKeyViolation: boolean;
begin
  Result := False;
end;

{ TFieldTypeInfo }

constructor TFieldTypeInfo.Create(FieldType: TFieldType; const Name: string; Length, Scale: boolean);
begin
  inherited Create;

  FFieldType := FieldType;
  FName := Name;
  FLength := Length;
  FScale := Scale;
end;

{ TFieldTypeInfos }

constructor TFieldTypeInfos.Create;
begin
  inherited;

  FTypeInfos := TCRObjectList.Create;

  Add(ftBoolean,    'Boolean',    False, False);
{$IFDEF VER12P}
  Add(ftShortint,   'Shortint',   False, False);
  Add(ftByte,       'Byte',       False, False);
{$ENDIF}
  Add(ftSmallint,   'Smallint',   False, False);
  Add(ftWord,       'Word',       False, False);
  Add(ftInteger,    'Integer',    False, False);
{$IFDEF VER12P}
  Add(ftLongWord,   'LongWord',   False, False);
{$ENDIF}
  Add(ftLargeint,   'Largeint',   False, False);
  Add(ftFloat,      'Float',      False, False);
{$IFDEF VER12P}
  Add(TFieldType.ftExtended,   'Extended',   False, False);
  {$IFDEF VER14P}
  Add(TFieldType.ftSingle,     'Single',     False, False);
  {$ENDIF}
{$ENDIF}
  Add(ftCurrency,   'Currency',   False, False);
  Add(ftBCD,        'BCD',        True,  True);
  Add(ftFMTBCD,     'FMTBCD',     True,  True);
  Add(ftDate,       'Date',       False, False);
  Add(ftTime,       'Time',       False, False);
  Add(ftDateTime,   'DateTime',   False, False);
{$IFNDEF FPC}
  Add(ftTimeStamp,  'TimeStamp',  False, False);
{$ENDIF}
  Add(ftString,     'String',     True,  False);
  Add(ftWideString, 'WideString', True,  False);
  Add(ftMemo,       'Memo',       False, False);
{$IFDEF VER10P}
  Add(ftWideMemo,   'WideMemo',   False, False);
{$ENDIF}
{$IFDEF FPC}
  Add(ftWideMemo,   'WideMemo',   False, False);
{$ENDIF}
  Add(ftBytes,      'Bytes',      True,  False);
  Add(ftVarBytes,   'VarBytes',   True,  False);
  Add(ftBlob,       'Blob',       False, False);
  Add(ftGuid,       'Guid',       False, False);
end;

destructor TFieldTypeInfos.Destroy;
begin
  Clear;
  FTypeInfos.Free;
  inherited;
end;

function TFieldTypeInfos.GetCount: Integer;
begin
  Result := FTypeInfos.Count;
end;

function TFieldTypeInfos.GetTypeInfo(Index: integer): TFieldTypeInfo;
begin
  Result := TFieldTypeInfo(FTypeInfos[Index]);
end;

procedure TFieldTypeInfos.Add(FieldType: TFieldType; const Name: string; Length, Scale: boolean);
var
  FieldTypeInfo: TFieldTypeInfo;
begin
  FieldTypeInfo := TFieldTypeInfo.Create(FieldType, Name, Length, Scale);
  FTypeInfos.Add(FieldTypeInfo);
end;

procedure TFieldTypeInfos.Delete(Index: Integer);
begin
  FTypeInfos.Delete(Index);
end;

procedure TFieldTypeInfos.Clear;
begin
  FTypeInfos.Clear;
end;

function TFieldTypeInfos.Check(FieldType: TFieldType; Length, Scale: Integer): Exception;
var
  FieldTypeInfo: TFieldTypeinfo;
begin
  if FieldType = ftUnknown then
    Result := EInvalidFieldTypeMapping.Create(SNotDefinedFieldType)
  else begin
    FieldTypeInfo := FindTypeInfo(FieldType);
    if FieldTypeInfo = nil then
      Result := EInvalidFieldTypeMapping.CreateFmt(SUnsupportedFieldType, [IntToStr(integer(FieldType))])
    else if (FieldTypeInfo.Length = False) and (Length <> rlAny) then
      Result :=  EInvalidFieldTypeMapping.CreateFmt(SInvalidFieldLength, [FieldTypeInfo.Name])
    else if (FieldTypeInfo.Scale = False) and (Scale <> rlAny) then
      Result := EInvalidFieldTypeMapping.CreateFmt(SInvalidFieldScale, [FieldTypeInfo.Name])
    else
      Result := nil;
  end;
end;

function TFieldTypeInfos.FindTypeInfo(FieldType: TFieldType): TFieldTypeInfo;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if TypeInfos[i].FieldType = FieldType then begin
      Result := TypeInfos[i];
      Exit;
    end;
end;

function TFieldTypeInfos.FindTypeInfo(const Name: string): TFieldTypeInfo;
var
  UName: string;
  i: integer;
begin
  Result := nil;

  UName := AnsiUpperCase(Trim(Name));
  for i := 0 to Count - 1 do
    if AnsiUpperCase(TypeInfos[i].Name) = UName then begin
      Result := TypeInfos[i];
      Exit;
    end;
end;

{ TDAFieldTypeMap }

class function TDAFieldTypeMap.GetFieldTypeInfos: TFieldTypeInfos;
begin
  Result := FieldTypeInfos;
end;

{ TDAConnectionOptions }

constructor TDAConnectionOptions.Create(Owner: TCustomDAConnection);
begin
  inherited Create;

  FOwner := Owner;
  FKeepDesignConnected := True;
  FAllowImplicitConnect := True;
  FUuidWithBraces := True;
end;

procedure TDAConnectionOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAConnectionOptions then begin
    TDAConnectionOptions(Dest).KeepDesignConnected := KeepDesignConnected;
    TDAConnectionOptions(Dest).DisconnectedMode := DisconnectedMode;
    TDAConnectionOptions(Dest).LocalFailover := LocalFailover;
    TDAConnectionOptions(Dest).EnableBCD := EnableBCD;
    TDAConnectionOptions(Dest).EnableFMTBCD := EnableFMTBCD;
    TDAConnectionOptions(Dest).AllowImplicitConnect := AllowImplicitConnect;
    TDAConnectionOptions(Dest).UuidWithBraces := UuidWithBraces;
  end
  else
    inherited;
end;

procedure TDAConnectionOptions.SetDisconnectedMode(Value: boolean);
begin
  if Value <> DisconnectedMode then begin
    FOwner.Disconnect;
    FDisconnectedMode := Value;
    if FOwner.FIConnection <> nil then
      FOwner.FIConnection.SetProp(prDisconnectedMode, Value);
  end;
end;

procedure TDAConnectionOptions.SetDefaultSortType(Value: TSortType);
begin
  if Value <> FDefaultSortType then begin
    FDefaultSortType := Value;
    if FOwner.FIConnection <> nil then
      FOwner.FIConnection.SetProp(prDefaultSortType, Variant(Value));
  end;
end;

procedure TDAConnectionOptions.SetEnableBCD(Value: boolean);
begin
  FEnableBCD := Value;
  if FOwner.FIConnection <> nil then
    FOwner.FIConnection.SetProp(prEnableBCD, Value);
end;

procedure TDAConnectionOptions.SetEnableFMTBCD(Value: boolean);
begin
  FEnableFMTBCD := Value;
  if FOwner.FIConnection <> nil then
    FOwner.FIConnection.SetProp(prEnableFmtBCD, Value);
end;

procedure TDAConnectionOptions.SetUuidWithBraces(const Value: boolean);
begin
  FUuidWithBraces := Value;
  if FOwner.FIConnection <> nil then
    FOwner.FIConnection.SetProp(prUuidWithBraces, Value);
end;

{ TPoolingOptions }

constructor TPoolingOptions.Create(Owner: TCustomDAConnection);
begin
  inherited Create;

  FOwner := Owner;
  FMaxPoolSize := DefValMaxPoolSize;
end;

procedure TPoolingOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TPoolingOptions then begin
    TPoolingOptions(Dest).FMaxPoolSize := FMaxPoolSize;
    TPoolingOptions(Dest).FMinPoolSize := FMinPoolSize;
    TPoolingOptions(Dest).FConnectionLifetime := FConnectionLifetime;
    TPoolingOptions(Dest).FValidate := FValidate;
  end
  else
    inherited;
end;

{ TDAConnectionSSLOptions }

constructor TDAConnectionSSLOptions.Create(Owner: TCustomDAConnection);
begin
  inherited Create;

  FOwner := Owner;
end;

procedure TDAConnectionSSLOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAConnectionSSLOptions then begin
    TDAConnectionSSLOptions(Dest).CipherList := CipherList;
    TDAConnectionSSLOptions(Dest).CACert := CACert;
    TDAConnectionSSLOptions(Dest).Key := Key;
    TDAConnectionSSLOptions(Dest).Cert := Cert;
  end
  else
    inherited;
end;

procedure TDAConnectionSSLOptions.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('ChipherList', ReadCipherList, nil, False);
end;

procedure TDAConnectionSSLOptions.ReadCipherList(Reader: TReader);
begin
  CipherList := Reader.ReadString;
end;

procedure TDAConnectionSSLOptions.SetCACert(const Value: string);
begin
  if FCACert <> Value then begin
    TCustomDAConnection(FOwner).CheckInactive;
    FCACert := Value;
    if TCustomDAConnection(FOwner).FIConnection <> nil then
      TCustomDAConnection(FOwner).FIConnection.SetProp(prSSLCA, Value);
  end;
end;

procedure TDAConnectionSSLOptions.SetCert(const Value: string);
begin
  if FCert <> Value then begin
    TCustomDAConnection(FOwner).CheckInactive;
    FCert := Value;
    if TCustomDAConnection(FOwner).FIConnection <> nil then
      TCustomDAConnection(FOwner).FIConnection.SetProp(prSSLCert, Value);
  end;
end;

procedure TDAConnectionSSLOptions.SetCipherList(const Value: string);
begin
  if FCipherList <> Value then begin
    TCustomDAConnection(FOwner).CheckInactive;
    FCipherList := Value;
    if TCustomDAConnection(FOwner).FIConnection <> nil then
      TCustomDAConnection(FOwner).FIConnection.SetProp(prSSLCipher, Value);
  end;
end;

procedure TDAConnectionSSLOptions.SetKey(const Value: string);
begin
  if FKey <> Value then begin
    TCustomDAConnection(FOwner).CheckInactive;
    FKey := Value;
    if TCustomDAConnection(FOwner).FIConnection <> nil then
      TCustomDAConnection(FOwner).FIConnection.SetProp(prSSLKey, Value);
  end;
end;

{ TDAMapRule }

constructor TDAMapRule.Create(Collection: TCollection);
begin
  inherited;

  FFieldType := ftUnknown;
end;

function TDAMapRule.IsFieldTypeStored: boolean;
begin
  Result := Integer(FieldType) <= Integer(High(TFieldType));
end;

procedure TDAMapRule.DoRuleChanged;
begin
  TDAMapRules(GetOwner).DoRulesChanged;
end;

procedure TDAMapRule.DefineProperties(Filer: TFiler);

  function WriteData: boolean;
  begin
    Result := not IsFieldTypeStored;
  end;

begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('ExtFieldType', ReadExtFieldType, WriteExtFieldType, WriteData);
end;

procedure TDAMapRule.ReadExtFieldType(Reader: TReader);
begin
  FieldType := TFieldType(Reader.ReadInteger);
end;

procedure TDAMapRule.WriteExtFieldType(Writer: TWriter);
begin
  Writer.WriteInteger(Integer(FieldType));
end;

procedure TDAMapRule.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAMapRule then
    AssignToRule(TDAMapRule(Dest))
  else
    inherited;
end;

procedure TDAMapRule.AssignToRule(Dest: TDAMapRule);
begin
  inherited AssignToRule(Dest);

  Dest.FFieldType := FFieldType;

  Dest.DoRuleChanged;
end;

{ TCustomDAMapRules }

constructor TDAMapRules.Create;
begin
  inherited Create(GetMapRuleClass);

  FIgnoreInvalidRules := False;
end;

function TDAMapRules.GetItem(Index: Integer): TDAMapRule;
begin
  Result := TDAMapRule(inherited GetItem(Index));
end;

procedure TDAMapRules.SetItem(Index: Integer; Value: TDAMapRule);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

class function TDAMapRules.GetMapRuleClass: TDAMapRuleClass;
begin
  Result := TDAMapRule;
end;

procedure TDAMapRules.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAMapRules then
    AssignToRules(TDAMapRules(Dest))
  else
    inherited;
end;

procedure TDAMapRules.AssignToRules(Dest: TDAMapRules);
var
  i: Integer;
begin
  TDAMapRules(Dest).Clear;
  for i := 0 to Count - 1 do
    Items[i].AssignToRule(TDAMapRule(Dest.Add));

  TDAMapRules(Dest).DoRulesChanged;
end;

procedure TDAMapRules.Update(Item: TCollectionItem);
begin
  inherited;
  DoRulesChanged;
end;

procedure TDAMapRules.WriteTo(Dest: TCRMapRules);
var
  i: integer;
  Rule: TDAMapRule;
begin
  Dest.Clear;
  Dest.IgnoreInvalidRules := IgnoreInvalidRules;

  for i := 0 to Count - 1 do begin
    Rule := Items[i];
    Dest.AddRule(Rule.FieldName,
      Rule.DBType, Rule.DBLengthMin, Rule.DBLengthMax, Rule.DBScaleMin, Rule.DBScaleMax,
      GetDataType(Rule.FieldType), Rule.FieldLength, Rule.FieldScale,
      Rule.IgnoreErrors, Rule.Format);
  end;
end;

function TDAMapRules.CreateParser(const Rule: string): TParser;
begin
  Result := TParser.Create(Rule);
  Result.OmitBlank := False;
  Result.OmitComment := False;
  Result.OmitKeywords := False;
end;

function TDAMapRules.ParseRule(Parser: TParser;
  out FieldName: string;
  out DBType: Word;
  out DBLengthMin, DBLengthMax: Integer;
  out DBScaleMin, DBScaleMax: Integer;
  out FieldType: TFieldType;
  out FieldLength, FieldScale: Integer;
  out IgnoreErrors: boolean;
  out Format: string): boolean;

  function GetNextLexem(out Lexem: string): Integer;
  begin
    Result := Parser.GetNext(Lexem);
    if (Result = lcEnd) or (Result = lxSemicolon) then
      raise EInvalidMapRuleExpression.Create(SUnexpectedEndOfMapRuleExpr);
  end;

  procedure GetNextSymbol(SymbolCode: integer);
  var
    CodeLexem: integer;
    Lexem: string;
    SymbolStr: string;
  begin
    CodeLexem := GetNextLexem(Lexem);

    if CodeLexem <> SymbolCode then begin
      if Parser.IsSymbolCode(SymbolCode) then
        SymbolStr := Parser.GetSymbolByCode(SymbolCode)
      else
        SymbolStr := '';
      if SymbolStr <> '' then
        raise EInvalidMapRuleExpression.CreateFmt(SUnexpectedSymbolInMapRuleExpr, [SymbolStr, Lexem])
      else
        raise EInvalidMapRuleExpression.Create(SInvalidMapRuleExpr);
    end;
  end;

  function GetNextInt: Integer;
  var
    CodeLexem: integer;
    Lexem: string;
  begin
    CodeLexem := GetNextLexem(Lexem);

    if UpperCase(Lexem) = 'ANY' then
      Result := rlAny
    else if (CodeLexem <> lcNumber) or (not TryStrToInt(Lexem, Result)) then begin
      if Lexem <> '' then
        raise EInvalidMapRuleExpression.CreateFmt(SUnexpectedSymbolInMapRuleExpr, ['Integer value', Lexem])
      else
        raise EInvalidMapRuleExpression.Create(SInvalidMapRuleExpr);
    end;
  end;

var
  CodeLexem: integer;
  Lexem: string;
  Str: string;
  DBTypeInfo: TDBTypeInfo;
  FieldTypeInfo: TFieldTypeInfo;
begin
  FieldName := '';
  DBType := 0;
  DBLengthMin := -1;
  DBLengthMax := -1;
  DBScaleMin := -1;
  DBScaleMax := -1;
  FieldType := ftUnknown;
  FieldLength := -1;
  FieldScale := -1;
  IgnoreErrors := False;
  Format := '';

  // Skip blank in begin
  Parser.OmitBlank := True;

  CodeLexem := Parser.GetNext(Lexem);
  if CodeLexem = lcEnd then begin
    // Empty rule
    Result := False;
    Exit;
  end;

  // Field Name or Db Type can contain spaces
  Parser.OmitBlank := False;
  if CodeLexem = lxLeftSqBracket{'['} then begin
    // Parse field name
    while True do begin
      CodeLexem := GetNextLexem( Lexem);
      if CodeLexem = lxRightSqBracket{']'} then
        break
      else
        FieldName := FieldName + Lexem;
    end;

    Parser.OmitBlank := True;

    GetNextSymbol(lxDash{'-'});
  end
  else begin
    // Parse DB type
    Str := '';
    repeat
      Str := Str + Lexem;
      CodeLexem := GetNextLexem(Lexem);
    until (CodeLexem = lxLeftBracket{'('}) or (CodeLexem = lxDash{'-'});
    DBTypeInfo := DBTypeInfos.FindTypeInfo(Str, 0);
    if DBTypeInfo = nil then
      raise EInvalidMapRuleExpression.CreateFmt(SUnsupportedDBType, [Str]);
    DBType := DBTypeInfo.DBType;

    Parser.OmitBlank := True;

    if CodeLexem = lxLeftBracket{'('} then begin
      // Precision interval
      DBLengthMin := GetNextInt;
      GetNextSymbol(lxPoint{'.'});
      GetNextSymbol(lxPoint{'.'});
      DBLengthMax := GetNextInt;
      CodeLexem := GetNextLexem(Lexem);
      if CodeLexem = lxComma{','} then begin
        // Scale interval
        DBLengthMin := GetNextInt;
        GetNextSymbol(lxPoint{'.'});
        GetNextSymbol(lxPoint{'.'});
        DBScaleMax := GetNextInt;
        CodeLexem := GetNextLexem(Lexem);
      end;

      if CodeLexem <> lxRightBracket{')'} then
        raise EInvalidMapRuleExpression.CreateFmt(SUnexpectedSymbolInMapRuleExpr, [')', Lexem]);

      GetNextSymbol(lxDash{'-'});
    end;
  end;

  // operator "->"
  GetNextSymbol(lxMore{'>'});

  Parser.OmitBlank := False;

  GetNextLexem(Lexem);
  Str := '';
  repeat
    Str := Str + Lexem;
    CodeLexem := Parser.GetNext(Lexem);
  until (CodeLexem = lcEnd) or (CodeLexem = lxSemicolon{;}) or (CodeLexem = lxLeftBracket{'('}) or (CodeLexem = lxComma{','});
  FieldTypeInfo := GetFieldTypeInfos.FindTypeInfo(Str);
  if FieldTypeInfo = nil then
    raise EInvalidMapRuleExpression.CreateFmt(SUnsupportedFieldType, [Str]);
  FieldType := FieldTypeInfo.FieldType;

  Parser.OmitBlank := True;

  if CodeLexem = lxLeftBracket{'('} then begin
    FieldLength := GetNextInt;
    CodeLexem := GetNextLexem(Lexem);
    if CodeLexem = lxComma{','} then begin
      FieldScale := GetNextInt;
      CodeLexem := GetNextLexem(Lexem);
    end;

    if CodeLexem <> lxRightBracket{')'} then
      raise EInvalidMapRuleExpression.CreateFmt(SUnexpectedSymbolInMapRuleExpr, [')', Lexem]);

    CodeLexem := Parser.GetNext(Lexem);
  end;

  if CodeLexem = lxComma{','} then begin
    CodeLexem := GetNextLexem(Lexem);

    if CodeLexem = lcString then begin
      Format := Lexem;

      CodeLexem := Parser.GetNext(Lexem);
      if CodeLexem = lxComma{','} then
        CodeLexem := GetNextLexem(Lexem);
    end;

    if (CodeLexem = lcIdent) and (UpperCase(Lexem) = 'IGNOREERRORS') then begin
      IgnoreErrors := True;
      CodeLexem := Parser.GetNext(Lexem);
    end
    else
      if (CodeLexem <> lcEnd) and (CodeLexem <> lxSemicolon{;}) then
        raise EInvalidMapRuleExpression.CreateFmt(SUnexpectedSymbolInMapRuleExpr, ['IgnoreErrors', Lexem]);
  end;

  if (CodeLexem <> lcEnd) and (CodeLexem <> lxSemicolon{;}) then
    raise EInvalidMapRuleExpression.CreateFmt(SUnexpectedSymbolInMapRuleExpr, ['End of Rule', Lexem]);

  Result := True;
end;

function TDAMapRules.CheckRule(const FieldName: string;
  DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
  FieldType: TFieldType; FieldLength, FieldScale: Integer): Exception;
begin
  Result := GetFieldTypeInfos.Check(FieldType, FieldLength, FieldScale);
end;

procedure TDAMapRules.DoAddRule(const FieldName: string; DBType: Word;
  DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
  FieldType: TFieldType; FieldLength, FieldScale: Integer;
  IgnoreErrors: boolean; const Format: string);
var
  Rule: TDAMapRule;
begin
  Rule := TDAMapRule(Add);

  Rule.FieldName := FieldName;
  Rule.DBType := DBType;
  Rule.DBLengthMin := DBLengthMin;
  Rule.DBLengthMax := DBLengthMax;
  Rule.DBScaleMin := DBScaleMin;
  Rule.DBScaleMax := DBScaleMax;
  Rule.FieldType := FieldType;
  Rule.FieldLength := FieldLength;
  Rule.FieldScale := FieldScale;
  Rule.IgnoreErrors := IgnoreErrors;
  Rule.Format := Format;
end;

procedure TDAMapRules.AddDBTypeRule(DBType: Word;
  FieldType: TFieldType; IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule('', DBType, rlAny, rlAny, rlAny, rlAny, FieldType, rlAny, rlAny, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddDBTypeRule(DBType: Word;
  FieldType: TFieldType; FieldLength: Integer;
  IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule('', DBType, rlAny, rlAny, rlAny, rlAny, FieldType, FieldLength, rlAny, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddDBTypeRule(DBType: Word;
  FieldType: TFieldType; FieldLength, FieldScale: Integer;
  IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule('', DBType, rlAny, rlAny, rlAny, rlAny, FieldType, FieldLength, FieldScale, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddDBTypeRule(DBType: Word; DBLengthMin, DBLengthMax: Integer;
  FieldType: TFieldType; IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule('', DBType, DBLengthMin, DBLengthMax, rlAny, rlAny, FieldType, rlAny, rlAny, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddDBTypeRule(DBType: Word; DBLengthMin, DBLengthMax: Integer;
  FieldType: TFieldType; FieldLength: Integer;
  IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule('', DBType, DBLengthMin, DBLengthMax, rlAny, rlAny, FieldType, FieldLength, rlAny, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddDBTypeRule(DBType: Word; DBLengthMin, DBLengthMax,
  DBScaleMin, DBScaleMax: Integer; FieldType: TFieldType;
  IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule('', DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, FieldType, rlAny, rlAny, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddDBTypeRule(DBType: Word; DBLengthMin, DBLengthMax,
  DBScaleMin, DBScaleMax: Integer; FieldType: TFieldType; FieldLength,
  FieldScale: Integer; IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule('', DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, FieldType, FieldLength, FieldScale, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddFieldNameRule(const FieldName: string;
  FieldType: TFieldType; IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule(FieldName, 0, rlAny, rlAny, rlAny, rlAny, FieldType, rlAny, rlAny, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddFieldNameRule(const FieldName: string;
  FieldType: TFieldType; FieldLength: Integer;
  IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule(FieldName, 0, rlAny, rlAny, rlAny, rlAny, FieldType, FieldLength, rlAny, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddFieldNameRule(const FieldName: string;
  FieldType: TFieldType; FieldLength, FieldScale: Integer;
  IgnoreErrors: boolean = False; const Format: string = '');
begin
  AddRule(FieldName, 0, rlAny, rlAny, rlAny, rlAny, FieldType, FieldLength, FieldScale, IgnoreErrors, Format);
end;

procedure TDAMapRules.AddRule(const FieldName: string; DBType: Word;
  DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
  FieldType: TFieldType; FieldLength, FieldScale: Integer;
  IgnoreErrors: boolean = False; const Format: string = '');
var
  Error: Exception;
begin
  Error := CheckRule(FieldName,
                    DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax,
                    FieldType, FieldLength, FieldScale);

  if Error = nil then
    DoAddRule(FieldName,
              DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax,
              FieldType, FieldLength, FieldScale, IgnoreErrors, Format)
  else if FIgnoreInvalidRules then
    Error.Free
  else
    raise Error;
end;

procedure TDAMapRules.AddRule(const Rule: string);
var
  Parser: TParser;
  FieldName: string;
  DBType: Word;
  DBLengthMin, DBLengthMax: Integer;
  DBScaleMin, DBScaleMax: Integer;
  FieldType: TFieldType;
  FieldLength: Integer;
  FieldScale: Integer;
  IgnoreErrors: boolean;
  Format: string;
begin
  Parser := CreateParser(Rule);
  try
    if ParseRule(Parser, FieldName,
         DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax,
        FieldType, FieldLength, FieldScale, IgnoreErrors, Format)
    then
      AddRule(FieldName,
        DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax,
        FieldType, FieldLength, FieldScale, IgnoreErrors, Format);
  finally
    Parser.Free;
  end;
end;

procedure TDAMapRules.AddRules(const Rules: string);
var
  Parser: TParser;
  FieldName: string;
  DBType: Word;
  DBLengthMin, DBLengthMax: Integer;
  DBScaleMin, DBScaleMax: Integer;
  FieldType: TFieldType;
  FieldLength: Integer;
  FieldScale: Integer;
  IgnoreErrors: boolean;
  Format: string;
begin
  Parser := CreateParser(Rules);
  try
    while ParseRule(Parser, FieldName,
               DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax,
               FieldType, FieldLength, FieldScale, IgnoreErrors, Format)
    do
      AddRule(FieldName,
        DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax,
        FieldType, FieldLength, FieldScale, IgnoreErrors, Format);
  finally
    Parser.Free;
  end;
end;

{ TDAConnectionMapRules }

constructor TDAConnectionMapRules.Create(Connection: TCustomDAConnection);
begin
 inherited Create;
 FConnection := Connection;
end;

function TDAConnectionMapRules.GetDataType(FieldType: TFieldType): Word;
begin
  Result := FConnection.GetFieldTypeMapClass.GetDataType(FieldType);
end;

function TDAConnectionMapRules.GetFieldTypeInfos: TFieldTypeInfos;
begin
  Result := FConnection.GetFieldTypeMapClass.GetFieldTypeInfos;
end;

function TDAConnectionMapRules.CheckRule(const FieldName: string;
  DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
  FieldType: TFieldType; FieldLength, FieldScale: Integer): Exception;
begin
  Result := inherited CheckRule(FieldName, DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, FieldType, FieldLength, FieldScale);
  if Result = nil then
    Result := DBTypeInfos.Check(DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, FieldName = '');
end;

procedure TDAConnectionMapRules.DoRulesChanged;
begin
  FConnection.Disconnect;
end;

{ TDADataSetMapRules }

constructor TDADataSetMapRules.Create(DataSet: TCustomDADataSet);
begin
  inherited Create;
  FDataSet := DataSet;
end;

function TDADataSetMapRules.GetDataType(FieldType: TFieldType): Word;
begin
  Result := FDataSet.GetFieldTypeMapClass.GetDataType(FieldType);
end;

function TDADataSetMapRules.GetFieldTypeInfos: TFieldTypeInfos;
begin
  Result := TDAFieldTypeMapClass(FDataSet.GetFieldTypeMapClass).GetFieldTypeInfos;
end;

function TDADataSetMapRules.CheckRule(const FieldName: string;
  DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
  FieldType: TFieldType; FieldLength, FieldScale: Integer): Exception;
begin
  Result := inherited CheckRule(FieldName, DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, FieldType, FieldLength, FieldScale);
  if Result = nil then
    Result := DBTypeInfos.Check(DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, FieldName = '');
end;

procedure TDADataSetMapRules.DoRulesChanged;
begin
  FDataSet.Close;
end;

{ TCustomDAConnection }

constructor TCustomDAConnection.Create(Owner: TComponent);
var
  SSLOptionsClass: TDAConnectionSSLOptionsClass;
begin
  inherited Create(Owner);

{$IFDEF OWN_CLIENTS_REGISTRATION}
  FDataSets := TList.Create;
  FClients := TList.Create;
  FConnectEvents := TList.Create;
{$ENDIF}

  FTransactions := TDATransactions.Create;
  FInternalDefTransaction := CreateTransaction;
  RemoveFreeNotification(FInternalDefTransaction);
  InsertComponent(FInternalDefTransaction);
  FInternalDefTransaction.SetSubComponent(True);
  if Name <> '' then
    FInternalDefTransaction.Name := 'tr' + Name;
  FInternalDefTransaction.DefaultConnection := Self;
  FInternalDefTransaction.FShareTransaction := True;

  FSQLs := TList.Create;
  FAutoCommit := True;
  LoginPrompt := DefValLoginPrompt;

  FOptions := CreateOptions;
  FPoolingOptions := CreatePoolingOptions;

  SSLOptionsClass := GetSSLOptionsClass;
  if SSLOptionsClass <> nil then begin
    FSSLOptions := SSLOptionsClass.Create(Self);
    FHttpOptions := THttpOptions.Create;
    FProxyOptions := TProxyOptions.Create;
    FHttpOptions.ProxyOptions := FProxyOptions;
  end;

  FShouldShowPrompt := True;
  FDataTypeMap := CreateDataTypeMap;
  FConnectionStringBuilder := CreateConnectionStringBuilder;

  hRegisterClient := TCriticalSection.Create;
end;

constructor TCustomDAConnection.Create(Owner: TComponent; const ConnectString: string);
begin
  Create(Owner);
  Self.ConnectString := ConnectString;
end;

destructor TCustomDAConnection.Destroy;
var
  i: integer;
begin
  try
    Disconnect;
  finally
    if Assigned(FIOHandler) then
      TCRIOHandlerUtils.UnRegisterClient(FIOHandler, Self);

    FSSLOptions.Free;
    FHttpOptions.Free;
    FProxyOptions.Free;

    ClearRefs;
    FreeAndNil(FCommand);

    FTransactions.Remove(FInternalDefTransaction);
    FInternalDefTransaction.Free;
    FInternalDefTransaction := nil;

    for i := FTransactions.Count - 1 downto 0 do
      DoRemoveTransaction(FTransactions[i]);

    TDASQLMonitorClass(SQLMonitorClass).ObjectDestroyed(Self);

    inherited;

    FSQLs.Free; // placed after inherited for successful UnRegisterClient on destroy
    FreeIConnection;
    FTransactions.Free;
    FPoolingOptions.Free;
    FOptions.Free;
    FDataTypeMap.Free;
    FConnectionStringBuilder.Free;

    hRegisterClient.Free;

  {$IFDEF OWN_CLIENTS_REGISTRATION}
    FreeAndNil(FConnectEvents);
    FreeAndNil(FClients);
    FreeAndNil(FDataSets);
  {$ENDIF}
  end;
end;

function TCustomDAConnection.IsMapRulesStored: boolean;
begin
  Result := FDataTypeMap.Count > 0;
end;

procedure TCustomDAConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then begin
    if AComponent = Self then
      Close
    else
    if AComponent = FConnectDialog then
      FConnectDialog := nil
    else
    if AComponent = FDefaultTransaction then
      DefaultTransaction := nil;
  {$IFNDEF LITE}
    if AComponent = FIOHandler then
      IOHandler := nil;
  {$ENDIF}
  end;
end;

{$IFNDEF LITE}

procedure TCustomDAConnection.SetIOHandler(Value: TCRIOHandler);
begin
  if FIOHandler <> Value then begin
    if FIOHandler <> nil then begin
      TCRIOHandlerUtils.UnRegisterClient(FIOHandler, Self);
      FIOHandler.RemoveFreeNotification(Self);
    end;

    Disconnect;
    FIOHandler := Value;

    if Value <> nil then begin
      TCRIOHandlerUtils.RegisterClient(Value, Self);
      FIOHandler.FreeNotification(Self);
    end;

    if FIConnection <> nil then
      FIConnection.IOHandler := FIOHandler;
  end;
end;

{$ENDIF}

function TCustomDAConnection.GetSSLOptionsClass: TDAConnectionSSLOptionsClass;
begin
  Result := nil;
end;

procedure TCustomDAConnection.SetSSLOptions(Value: TDAConnectionSSLOptions);
begin
  if (FSslOptions <> nil) and (Value <> nil) then
    FSslOptions.Assign(Value)
  else if (FSslOptions <> nil) or (Value <> nil) then
    raise Exception.Create('Internal error: SslOptions is invalid');
end;

procedure TCustomDAConnection.SetHttpOptions(Value: THttpOptions);
begin
  if (FHttpOptions <> nil) and (Value <> nil) then
    FHttpOptions.Assign(Value)
  else if (FHttpOptions <> nil) or (Value <> nil) then
    raise Exception.Create('Internal error: HttpOptions is invalid');
end;

procedure TCustomDAConnection.SetProxyOptions(Value: TProxyOptions);
begin
  if (FProxyOptions <> nil) and (Value <> nil) then
    FProxyOptions.Assign(Value)
  else if (FProxyOptions <> nil) or (Value <> nil) then
    raise Exception.Create('Internal error: ProxyOptions is invalid');
end;

function TCustomDAConnection.GetIConnectionClass: TCRConnectionClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRConnection;
end;

function TCustomDAConnection.GetICommandClass: TCRCommandClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRCommand;
end;

function TCustomDAConnection.GetIRecordSetClass: TCRRecordSetClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRRecordSet;
end;

function TCustomDAConnection.GetIMetaDataClass: TCRMetaDataClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRMetaData;
end;

function TCustomDAConnection.GetITransactionClass: TCRTransactionClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRTransaction;
end;

function TCustomDAConnection.IsMultipleTransactionsSupported: boolean;
begin
  Result := False;
end;

function TCustomDAConnection.ApplyUpdatesInTransaction: boolean;
begin
  Result := IsMultipleTransactionsSupported;
end;

function TCustomDAConnection.CreateICommand: TCRCommand;
begin
  Result := GetICommandClass.Create;
  Result.SetConnection(nil);
end;

function TCustomDAConnection.CreateIRecordSet: TCRRecordSet;
begin
  Result := GetIRecordSetClass.Create;
  Result.SetConnection(FIConnection);
end;

procedure TCustomDAConnection.CreateIConnection;
begin
  if FIConnection = nil then
    SetIConnection(GetIConnection);
end;

procedure TCustomDAConnection.SetConnectionParameters(ConnectionParameters: TCRConnectionParameters);
begin
  ConnectionParameters.MinPoolSize := PoolingOptions.MinPoolSize;
  ConnectionParameters.MaxPoolSize := PoolingOptions.MaxPoolSize;
  ConnectionParameters.ConnectionLifeTime := PoolingOptions.ConnectionLifetime;
  ConnectionParameters.Validate := PoolingOptions.Validate;

  ConnectionParameters.Username := Username;
  ConnectionParameters.Password := Password;
  ConnectionParameters.Server := Server;

  ConnectionParameters.OnError := DoError;

  ConnectionParameters.IOHandler := FIOHandler;

  if FSSLOptions <> nil then begin
    ConnectionParameters.SetProp(prSSLCipher, FSSLOptions.CipherList);
    ConnectionParameters.SetProp(prSSLCA, FSSLOptions.CACert);
    ConnectionParameters.SetProp(prSSLKey, FSSLOptions.Key);
    ConnectionParameters.SetProp(prSSLCert, FSSLOptions.Cert);
  end;

  if FHttpOptions <> nil then begin
    ConnectionParameters.SetProp(prUseHttp, FHttpOptions.Enabled);
    ConnectionParameters.SetProp(prHttpUrl, FHttpOptions.Url);
    ConnectionParameters.SetProp(prHttpUsername, FHttpOptions.Username);
    ConnectionParameters.SetProp(prHttpPassword, FHttpOptions.Password);
    ConnectionParameters.SetProp(prHttpTrustServerCertificate, FHttpOptions.TrustServerCertificate);
  end;

  if FProxyOptions <> nil then begin
    ConnectionParameters.SetProp(prProxyHostname, FProxyOptions.Hostname);
    ConnectionParameters.SetProp(prProxyPort, FProxyOptions.Port);
    ConnectionParameters.SetProp(prProxyUsername, FProxyOptions.Username);
    ConnectionParameters.SetProp(prProxyPassword, FProxyOptions.Password);
    ConnectionParameters.SetProp(prProxySocksVersion, FProxyOptions.SocksVersion);
    ConnectionParameters.SetProp(prProxyResolveDNS, FProxyOptions.ResolveDNS);
  end;
end;

procedure TCustomDAConnection.SetBaseConnectionProps(Connection: TCRConnection);
begin
  Connection.SetUsername(Username);
  Connection.SetPassword(Password);
  Connection.SetServer(Server);

  Connection.OnError := DoError;

  Connection.IOHandler := FIOHandler;

  if FSSLOptions <> nil then begin
    Connection.SetProp(prSSLCipher, FSSLOptions.CipherList);
    Connection.SetProp(prSSLCA, FSSLOptions.CACert);
    Connection.SetProp(prSSLKey, FSSLOptions.Key);
    Connection.SetProp(prSSLCert, FSSLOptions.Cert);
  end;

  SetHttpConnectionProps(Connection);
end;

procedure TCustomDAConnection.SetHttpConnectionProps(Connection: TCRConnection);
begin
  if FHttpOptions <> nil then begin
    Connection.SetProp(prUseHttp, FHttpOptions.Enabled);
    Connection.SetProp(prHttpUrl, FHttpOptions.Url);
    Connection.SetProp(prHttpUsername, FHttpOptions.Username);
    Connection.SetProp(prHttpPassword, FHttpOptions.Password);
    Connection.SetProp(prHttpTrustServerCertificate, FHttpOptions.TrustServerCertificate);
  end;

  if FProxyOptions <> nil then begin
    Connection.SetProp(prProxyHostname, FProxyOptions.Hostname);
    Connection.SetProp(prProxyPort, FProxyOptions.Port);
    Connection.SetProp(prProxyUsername, FProxyOptions.Username);
    Connection.SetProp(prProxyPassword, FProxyOptions.Password);
    Connection.SetProp(prProxySocksVersion, FProxyOptions.SocksVersion);
    Connection.SetProp(prProxyResolveDNS, FProxyOptions.ResolveDNS);
  end;
end;

procedure TCustomDAConnection.SetConnectionProps(Connection: TCRConnection);
begin
  Connection.SetProp(prDisconnectedMode, Options.DisconnectedMode);
  Connection.SetProp(prEnableBCD, Options.EnableBCD);
  Connection.SetProp(prEnableFMTBCD, Options.EnableFMTBCD);
  Connection.SetProp(prUuidWithBraces, Options.UuidWithBraces);

  Connection.GetPooledConnection := GetIConnection;
end;

function TCustomDAConnection.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TCRConnectionParameters;
end;

function TCustomDAConnection.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TCRConnectionPoolManager;
end;

function TCustomDAConnection.GetIConnection: TCRConnection;
var
  ConnectionParameters: TCRConnectionParameters;
begin
  if Pooling and (GetConnectionPoolingManagerClass <> nil) then begin
    ConnectionParameters := GetConnectionParametersClass.Create;
    try
      SetConnectionParameters(ConnectionParameters);
      Result := GetConnectionPoolingManagerClass.GetConnection(ConnectionParameters, TDASQLMonitorClass(SQLMonitorClass));

      if FIOHandler <> nil then
        TCRIOHandlerUtils.UnRegisterClient(FIOHandler, Result);
    finally
      ConnectionParameters.Free;
    end;
  end
  else begin
    Result := GetIConnectionClass.Create;
    try
      SetBaseConnectionProps(Result);
    except
      Result.Free;
      raise;
    end;
  end;

  SetConnectionProps(Result);
end;

procedure TCustomDAConnection.FreeIConnection;
var
  OldConnection: TCRConnection;
begin
  if FIConnection = nil then
    Exit;

  ClearTransactionRefs;

  OldConnection := FIConnection;
  try
    SetIConnection(nil);
  finally
    if OldConnection.Pool <> nil then
      OldConnection.ReturnToPool
    else
      OldConnection.Free;
  end;
end;

procedure TCustomDAConnection.SetIConnection(Value: TCRConnection);
var
  i: integer;
begin
  if (Value <> nil) or
    ((FIConnection <> nil) and not (csDestroying in ComponentState)) then begin
    for i := 0 to FSQLs.Count - 1 do
      if TCustomDASQL(FSQLs[i]).FICommand <> nil then
        TCustomDASQL(FSQLs[i]).FICommand.SetConnection(Value);

    for i := 0 to DataSetCount - 1 do
      if (DataSets[i] is TCustomDADataSet) and (TCustomDADataSet(DataSets[i]).FIRecordSet <> nil) then
        TCustomDADataSet(DataSets[i]).FIRecordSet.SetConnection(Value);
  end;

  FIConnection := Value;

  if FIConnection <> nil then begin
  {$IFNDEF LITE}
    FIConnection.OnError := DoError;
  {$ENDIF}
    FIConnection.OnReconnectError := Disconnect;
    FIConnection.OnReconnectSuccess := DoAfterConnect;
    FIConnection.Component := Self;
  end;
end;

procedure TCustomDAConnection.ClearTransactionRefs;
var
  i: integer;
begin
  for i := 0 to FTransactions.Count - 1 do begin
    if FTransactions[i].FITransaction <> nil then
      if FTransactions[i].FShareTransaction then
        FTransactions[i].FreeITransaction
      else
        FTransactions[i].FITransaction.RemoveConnection(FIConnection);
  end;
end;

function TCustomDAConnection.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  CheckCommand;
  Result := FCommand.GetFieldTypeMapClass;
end;

function TCustomDAConnection.CreateDataTypeMap: TDAConnectionMapRules;
begin
  Result := TDAConnectionMapRules.Create(self);
end;

procedure TCustomDAConnection.SetDataTypeMap(Value: TDAMapRules);
begin
  FDataTypeMap.Assign(Value);
end;

function TCustomDAConnection.CreateDataSet(AOwner: TComponent = nil): TCustomDADataSet;
begin
  Result := TCustomDADataSet.Create(AOwner);
  Result.Connection := Self;
end;

function TCustomDAConnection.CreateSQL: TCustomDASQL;
begin
  Result := TCustomDASQL.Create(nil);
  Result.Connection := Self;
end;

function TCustomDAConnection.CreateTransaction: TDATransaction;
begin
  Result := TDATransaction.Create(nil);
  Result.DefaultConnection := Self;
end;

function TCustomDAConnection.CreateMetaData: TDAMetaData;
begin
  Result := TDAMetaData.Create(nil);
  Result.Connection := Self;
end;

procedure TCustomDAConnection.EncryptTable(const TableName: string; Encryptor: TCREncryptor; const Fields: string);
var
  ds: TCustomDADataSet;
begin
  ds := CreateDataSet;
  try
    ds.Encryption.Encryptor := Encryptor;
    ds.Encryption.Fields := Fields;
    ds.SQL.Text := 'SELECT * FROM ' + TableName;
    ds.Open;
    ds.Encryption.EncryptDataSet;
  finally
    ds.Free;
  end;
end;

procedure TCustomDAConnection.RemoveFromPool;
begin
  if FIConnection <> nil then
    FIConnection.IsValid := False;
end;

procedure TCustomDAConnection.MonitorMessage(const Msg: string);
begin
  TDASQLMonitorClass(SQLMonitorClass).CustomMessage(Self, Msg);
end;

procedure TCustomDAConnection.Ping;
begin
  InternalConnect;
  try
    FIConnection.Ping;
  finally
    InternalDisconnect;
  end;
end;

procedure TCustomDAConnection.Loaded;
begin
  inherited;

  try
    try
      if FStreamedConnected then
        if FOptions.KeepDesignConnected or (csDesigning in ComponentState) then
          SetConnected(True);
    except
      on E: Exception do
        if csDesigning in ComponentState then
          ShowException(E, ExceptAddr)
        else
          raise;
    end;
  finally
    FStreamedConnected := False;
  end;

{$IFNDEF VER7P}
  FInternalDefTransaction.Loaded;
{$ENDIF}
end;

procedure TCustomDAConnection.ClearRefs;
var
  i: integer;
begin
  while FSQLs.Count > 0 do
    TCustomDASQL(FSQLs[0]).Connection := nil;

  i := 0;
  while i < DataSetCount do
    if DataSets[i] is TCustomDADataSet then
      TCustomDADataSet(DataSets[i]).Connection := nil
    else
    if DataSets[i] is TDAMetaData then
      TDAMetaData(DataSets[i]).Connection := nil
    else
      Inc(i);
end;

procedure TCustomDAConnection.RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil);
begin
  hRegisterClient.Enter;
  try
  {$IFDEF OWN_CLIENTS_REGISTRATION}
    FClients.Add(Client);
    FConnectEvents.Add(TMethod(Event).Code);
    if Client is TDataSet then
      FDataSets.Add(Client);
  {$ELSE}
    inherited;
  {$ENDIF}

    if Client is TCustomDASQL then
      FSQLs.Add(Client);
  finally
    hRegisterClient.Leave;
  end;
end;

procedure TCustomDAConnection.UnRegisterClient(Client: TObject);
{$IFDEF OWN_CLIENTS_REGISTRATION}
var
  Index: Integer;
{$ENDIF}
begin
  hRegisterClient.Enter;
  try
  {$IFDEF OWN_CLIENTS_REGISTRATION}
    if Client is TDataSet then
      FDataSets.Remove(Client);
    Index := FClients.IndexOf(Client);
    if Index <> -1 then begin
      FClients.Delete(Index);
      FConnectEvents.Delete(Index);
    end;
  {$ELSE}
    inherited;
  {$ENDIF}

    if Client is TCustomDASQL then
      FSQLs.Remove(Client);
  finally
    hRegisterClient.Leave;
  end;
end;

{$IFDEF OWN_CLIENTS_REGISTRATION}
procedure TCustomDAConnection.SendConnectEvent(Connecting: Boolean);
var
  I: Integer;
  ConnectEvent: TConnectChangeEvent;
begin
  for I := 0 to FClients.Count - 1 do
  begin
    if FConnectEvents[I] <> nil then
    begin
      TMethod(ConnectEvent).Code := FConnectEvents[I];
      TMethod(ConnectEvent).Data := FClients[I];
      ConnectEvent(Self, Connecting);
    end;
    if TObject(FClients[I]) is TDataset then
    {$IFDEF FPC}
      TCustomDADataSet(FClients[I]).DataEvent(deConnectChange, PtrInt(Connecting));
    {$ELSE}
      TCustomDADataSet(FClients[I]).DataEvent(deConnectChange, NativeInt(Connecting));
    {$ENDIF}
  end;
end;

function TCustomDAConnection.GetDataSet(Index: Integer): TDataSet;
begin
  Result := FDataSets[Index];
end;

function TCustomDAConnection.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;
{$ENDIF}

function TCustomDAConnection.SQLMonitorClass: TClass;
begin
  Result := TCustomDASQLMonitor;
end;

function TCustomDAConnection.ConnectDialogClass: TConnectDialogClass;
begin
  Result := TCustomConnectDialog;
end;

function TCustomDAConnection.NeedPrompt: boolean;
begin
  Result := not FLockLoginPrompt and LoginPrompt and
    not ((csDesigning in ComponentState) and ((csReading in ComponentState) or FStreamedConnected));
end;

function TCustomDAConnection.IsConnectedStored: boolean;
begin
  Result := Connected and not Options.DisconnectedMode; //In disconnect mode Design-time connection disabled
end;

function TCustomDAConnection.InternalGetServer: string;
begin
  Result := FServer;
end;

procedure TCustomDAConnection.DoConnect;
var
  MessageID: cardinal;
begin
  TDASQLMonitorClass(SQLMonitorClass).DBConnect(Self, MessageID, True);

  if FIConnection <> nil then
    SetHttpConnectionProps(FIConnection);

  CreateIConnection;
  PushOperation(clConnect);
  StartWait;
  try
    FDataTypeMap.WriteTo(FIConnection.DataTypeMap);

    // FIConnection.InternalTransaction -> FInternalDefTransaction.FITransaction
    FInternalDefTransaction.CheckITransaction;

    FIConnection.SetUsername(FUsername);
    FIConnection.SetPassword(FPassword);
    FIConnection.SetServer(InternalGetServer);

    FIConnection.Connect('');
  finally
    StopWait;
    PopOperation;
  end;

  TDASQLMonitorClass(SQLMonitorClass).DBConnect(Self, MessageID, False);
end;

procedure TCustomDAConnection.DoDisconnect;
var
  MessageID: cardinal;
  OldConnection: TCRConnection;
begin
  try
    try
      DisconnectTransaction;
    finally
      if Connected then begin // Disconnect can be done in Commit or Rollback
        TDASQLMonitorClass(SQLMonitorClass).DBDisconnect(Self, MessageID, True);

      {$IFNDEF LITE}
        if (FIConnection.Pool <> nil) and FIConnection.IsValid then begin
          OldConnection := FIConnection;
          try
            SetIConnection(nil);
          finally
            OldConnection.ReturnToPool;
          end;
        end
        else
      {$ENDIF}
          FIConnection.Disconnect;

        TDASQLMonitorClass(SQLMonitorClass).DBDisconnect(Self, MessageID, False);
      end;
    end;
  except
    on E: EDAError do begin
      if not((csDestroying in ComponentState) and E.IsFatalError) then
        raise;
    end
    else
      raise;
  end;
end;

procedure TCustomDAConnection.DisconnectTransaction;
var
  i: integer;
begin
  for i := 0 to FTransactions.Count - 1 do
    FTransactions[i].CloseTransaction(True);

  ClearTransactionRefs;
end;

procedure TCustomDAConnection.InternalConnect;
var
  StoredConnectCount: integer;
  StoredLoginPrompt: boolean;
begin
  Inc(FConnectCount);
  StoredConnectCount := FConnectCount;
  StoredLoginPrompt := LoginPrompt;
  LoginPrompt := LoginPrompt and FShouldShowPrompt;
  try
    try
      Connect;
    except
      on EFailOver do;
      else begin
        if not Connected then
          Dec(StoredConnectCount);//Restore ConnectCount in case of Connection Failure
        raise;
      end;
    end;
  finally
    FConnectCount := StoredConnectCount;
    LoginPrompt := StoredLoginPrompt;
    FShouldShowPrompt := not Connected; //in case of Connect Exception LogPrompt appears again
  end;
end;

procedure TCustomDAConnection.InternalDisconnect;
begin
  Dec(FConnectCount);
  if FConnectCount < 0 then //This could happen in case of Commit/RollBack after Execute with AutoCommit = False
    FConnectCount := 0;
  if (FConnectCount = 0) and Options.DisconnectedMode then
    if not InTransaction then //Execute with AutoCommit = False, after execute InTransaction = True, so wait for Commit/RollBack or for
                              //execute with AutoCommit = True
      Disconnect;
  FShouldShowPrompt := False;
end;

procedure TCustomDAConnection.CheckInactive;
begin
  if Connected then
    if ([csUpdating, csDesigning] * ComponentState) <> [] then
      Close
    else
      DatabaseError(SConnectionOpen, Self);
end;

procedure TCustomDAConnection.Connect;
begin
  SetConnected(True);
end;

procedure TCustomDAConnection.Connect(const ConnectString: string);
begin
  Self.ConnectString := ConnectString;
  Connect;
end;

procedure TCustomDAConnection.Disconnect;
begin
  SetConnected(False);
end;

procedure TCustomDAConnection.PerformConnect(Retry: boolean);
begin
  if csReading in ComponentState then
    FStreamedConnected := True
  else begin
    if GetConnected then
      Exit;
    if not Retry and Assigned(BeforeConnect) then
      BeforeConnect(Self);
    DoConnect;
    Inc(FConnectCount);

    hRegisterClient.Enter;
    try
      SendConnectEvent(True);
    finally
      hRegisterClient.Leave;
    end;

    if Assigned(AfterConnect) then
      AfterConnect(Self);
  end;
end;

procedure TCustomDAConnection.AssignConnect(Source: TCustomDAConnection);
begin
  if Source <> Self then begin
    Disconnect;
    if Pooling or (Assigned(Source) and Source.Pooling) then
      raise Exception.Create(SCannotPerformIfPooling);
    if Source <> nil then begin
      AssignConnectOptions(Source);

      if (FIConnection = nil) and (Source.FIConnection <> nil) then
        CreateIConnection;
      if (FIConnection <> nil) and (Source.FIConnection = nil) then
        FreeIConnection;

      if (FIConnection <> nil) and (Source.FIConnection <> nil) then begin
        Source.DataTypeMap.AssignToRules(FDataTypeMap);
        FIConnection.AssignConnect(Source.FIConnection);
        if FIConnection.GetInternalTransaction.GetInTransaction then
          DefaultTransaction.PrepareTransaction;
      end;
    end
    else
      if FIConnection <> nil then
        FIConnection.AssignConnect(nil);
  end;
end;

function TCustomDAConnection.ParamByName(const Name: string): TDAParam;
begin
  Result := FCommand.ParamByName(Name);
end;

procedure ParamsError;
begin
  raise Exception.Create(SInvalidParamsArray);
end;

function TCustomDAConnection.ExecSQL(const Text: string): variant;
begin
  Result := ExecSQL(Text, []);
end;

function TCustomDAConnection.ExecSQL(const Text: string; const Params: array of variant): variant;
var
  i: integer;
  Param: TParam;
begin
  CheckCommand;
  FCommand.SQL.Text := ''; // drop params from previous sql
  FCommand.SQL.Text := Text;

  for i := 0 to FCommand.ParamCount - 1 do
    if i <= High(Params) then begin
      Param := FCommand.Params[i];

      if FCommand.IsInOutParamSupported then
        Param.ParamType := ptInputOutput
      else
        Param.ParamType := ptUnknown;

      // output value can be more then input value
      case VarType(Params[i]) of
        varByte, varWord, varLongWord, varShortInt, varSmallint, varInteger:
          Param.AsInteger := Params[i];
        else
          Param.Value := Params[i];
      end;
    end;

  Param := FCommand.FindResultParam;
  if Param <> nil then
    if Param.DataType = ftUnknown then // from ODAC
      Param.DataType := ftString;

  FCommand.Execute;

  if Param <> nil then
    Result := Param.Value
  else
    Result := Null;
end;

function TCustomDAConnection.ExecSQLEx(const Text: string; const Params: array of variant): variant;
var
  i: integer;
  PName: string;
  Param: TParam;
begin
  CheckCommand;
  FCommand.SQL.Text := ''; // drop params from previous sql
  FCommand.SQL.Text := Text;

  if High(Params) mod 2 <> 1 then
    if (High(Params) >= 0) and not((High(Params) = 0) and VarIsNull(Params[0])) then
      ParamsError;

  for i := 0 to (High(Params) + 1) div 2 - 1  do begin
    if VarIsStr(Params[i * 2]) then
      PName := Params[i * 2]
    else
      ParamsError;

    Param := FCommand.ParamByName(PName);

    if FCommand.IsInOutParamSupported then
      Param.ParamType := ptInputOutput
    else
      Param.ParamType := ptUnknown;

    // output value can be more then input value
    case VarType(Params[i * 2 + 1]) of
      varByte, varWord, varLongWord, varShortInt, varSmallint, varInteger:
        Param.AsInteger := Params[i * 2 + 1];
      else
        Param.Value := Params[i * 2 + 1];
    end;
  end;

  Param := FCommand.FindResultParam;
  if Param <> nil then
    if Param.DataType = ftUnknown then // from ODAC
      Param.DataType := ftString;

  FCommand.Execute;

  if Param <> nil then
    Result := Param.Value
  else
    Result := Unassigned;
end;

function TCustomDAConnection.ExecProc(const Name: string; const Params: array of variant): variant;
var
  i, j, n: integer;
  Param: TParam;
begin
  CheckCommand;
  FCommand.SQL.Text := ''; // drop params from previous sql
  FCommand.InternalCreateProcCall(Name, True);

  Param := FCommand.FindResultParam;
  j := 0;
  n := FCommand.ParamCount;
  for i := 0 to FCommand.ParamCount - 1 do begin
    if j > High(Params) then begin
      n := i;
      break;
    end;
    if (Param = nil) or (i <> Param.Index) then begin
      FCommand.Params[i].Value := Params[j];
      j := j + 1;
    end;
  end;

  for i := n to FCommand.ParamCount - 1 do
    if FCommand.Params[i].ParamType = ptInput then
      FCommand.Params[i].Bound := False;

  FCommand.Execute;

  if Param <> nil then
    Result := Param.Value
  else
    Result := Unassigned;
end;

function TCustomDAConnection.ExecProcEx(const Name: string; const Params: array of variant): variant;
var
  i: integer;
  PName: string;
  Param: TParam;
begin
  CheckCommand;
  FCommand.SQL.Text := ''; // drop params from previous sql
  FCommand.InternalCreateProcCall(Name, True);

  if (High(Params) + 1) mod 2 <> 0 then
    if not((High(Params) = 0) and VarIsNull(Params[0])) then
      ParamsError;

  for i := 0 to FCommand.ParamCount - 1 do
    if FCommand.Params[i].ParamType = ptInput then
      FCommand.Params[i].Bound := False;

  for i := 0 to (High(Params) + 1) div 2 - 1 do begin
    if VarIsStr(Params[i * 2]) then
      PName := Params[i * 2]
    else
      ParamsError;

    FCommand.ParamByName(PName).Value := Params[i*2+1];
  end;

  FCommand.Execute;

  Param := FCommand.FindResultParam;
  if Param <> nil then
    Result := Param.Value
  else
    Result := Unassigned;
end;

function TCustomDAConnection.DefaultTableSchema: string;
begin
  Result := '';
end;

procedure TCustomDAConnection.GetTableNames(List: TStrings; AllTables: boolean = False; OnlyTables: boolean = False);
var
  MetaData: TDAMetaData;
  Name, s: string;
  NameField: TField;
  SchemaField: TField;
  CatalogField: TField;
begin
  MetaData := CreateMetaData;
  List.BeginUpdate;
  try
    InternalConnect;
    try
      MetaData.MetaDataKind := 'tables';
      if not AllTables then
        MetaData.Restrictions.Add('SCOPE=LOCAL');
      if OnlyTables then
        MetaData.Restrictions.Add('TABLE_TYPE=TABLE');
      MetaData.Open;
      NameField := MetaData.FindField('TABLE_NAME');
      SchemaField := MetaData.FindField('TABLE_SCHEMA');
      CatalogField := MetaData.FieldByName('TABLE_CATALOG');
      List.Clear;
      while not MetaData.Eof do begin
        Name := VarToStr(NameField.Value);
        if FIConnection.SQLInfo.QuotesNeeded(Name) then
          Name := FIConnection.SQLInfo.Quote(Name);
        if AllTables then begin
          s := VarToStr(SchemaField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
          s := VarToStr(CatalogField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
        end
        else
          if DefaultTableSchema <> '' then begin
            s := VarToStr(SchemaField.Value);
            if s <> '' then begin
              if AnsiLowerCase(s) <> DefaultTableSchema then
                Name := s + '.' + Name;
            end;
          end;

        List.Add(Name);
        MetaData.Next;
      end;
    finally
      InternalDisconnect;
    end;
  finally
    List.EndUpdate;
    MetaData.Free;
  end;
end;

procedure TCustomDAConnection.GetDatabaseNames(List: TStrings);
var
  MetaData: TDAMetaData;
  Name: string;
  DatabaseField: TField;
begin
  MetaData := CreateMetaData;
  List.BeginUpdate;
  try
    MetaData.MetaDataKind := 'Databases';
    MetaData.Open;
    DatabaseField := MetaData.FindField('DATABASE_NAME');
    List.Clear;
    while not MetaData.Eof do begin
      Name := VarToStr(DatabaseField.Value);
      List.Add(Name);
      MetaData.Next;
    end;
  finally
    List.EndUpdate;
    MetaData.Free;
  end;
end;

procedure TCustomDAConnection.GetStoredProcNames(List: TStrings; AllProcs: boolean = False);
var
  MetaData: TDAMetaData;
  Name, s: string;
  CatalogField, SchemaField, NameField, PackageField: TField;
  OverloadField: TField;
begin
  MetaData := CreateMetaData;
  List.BeginUpdate;
  try
    InternalConnect;
    try
      MetaData.MetaDataKind := 'procedures';
      if not AllProcs then
        MetaData.Restrictions.Add('SCOPE=LOCAL');
      MetaData.Open;
      NameField := MetaData.FindField('PROCEDURE_NAME');
      SchemaField := MetaData.FindField('PROCEDURE_SCHEMA');
      CatalogField := MetaData.FindField('PROCEDURE_CATALOG');
      PackageField := MetaData.FindField('PROCEDURE_PACKAGE');
      OverloadField := MetaData.FindField('OVERLOAD');
      List.Clear;
      while not MetaData.Eof do begin
        Name := VarToStr(NameField.Value);
        if FIConnection.SQLInfo.QuotesNeeded(Name) then
          Name := FIConnection.SQLInfo.Quote(Name);
        if PackageField <> nil then begin
          s := VarToStr(PackageField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
        end;
        if AllProcs then begin
          s := VarToStr(SchemaField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
          s := VarToStr(CatalogField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
        end;
        if OverloadField <> nil then begin
          s := OverloadField.AsString;
          if s <> '' then
            Name := Name + FIConnection.SQLInfo.ProcedureOverloadSeparator + s;
        end;
        List.Add(Name);
        MetaData.Next;
      end;
    finally
      InternalDisconnect;
    end;
  finally
    List.EndUpdate;
    MetaData.Free;
  end;
end;

procedure TCustomDAConnection.GetFieldNames(const TableName: string; List: TStrings);
var
  MetaData: TDAMetaData;
  Name: string;
begin
  MetaData := CreateMetaData;
  List.BeginUpdate;
  try
    InternalConnect;
    try
      MetaData.MetaDataKind := 'columns';
      MetaData.Restrictions.Add('SCOPE=LOCAL');
      MetaData.Restrictions.Add('TABLE_NAME=' + TableName);
      MetaData.Open;
      List.Clear;
      while not MetaData.Eof do begin
        Name := VarToStr(MetaData.FieldByName('COLUMN_NAME').Value);
        if FIConnection.SQLInfo.QuotesNeeded(Name) then
          Name := FIConnection.SQLInfo.Quote(Name);
        List.Add(Name);
        MetaData.Next;
      end;
    finally
      InternalDisconnect;
    end;
  finally
    List.EndUpdate;
    MetaData.Free;
  end;
end;

procedure TCustomDAConnection.GetKeyFieldNames(const TableName: string; List: TStrings);
var
  MetaData: TDAMetaData;
  Name, Filter: string;
begin
  MetaData := CreateMetaData;
  List.BeginUpdate;
  try
    InternalConnect;
    try
      MetaData.MetaDataKind := 'Constraints';
      MetaData.Restrictions.Add('SCOPE=LOCAL');
      MetaData.Restrictions.Values['Table_Name'] := TableName;
      MetaData.Filter := 'CONSTRAINT_TYPE = "PRIMARY KEY"';
      MetaData.Filtered := True;
      MetaData.Open;
      List.Clear;
      if MetaData.IsEmpty then
        Exit
      else
        Filter := 'CONSTRAINT_NAME = ' + QuotedStr(MetaData.FieldByName('CONSTRAINT_NAME').AsString);
      MetaData.MetaDataKind := 'ConstraintColumns';
      MetaData.Filter := Filter;
      MetaData.Open;
      while not MetaData.Eof do begin
        Name := VarToStr(MetaData.FieldByName('COLUMN_NAME').Value);
        if FIConnection.SQLInfo.QuotesNeeded(Name) then
          Name := FIConnection.SQLInfo.Quote(Name);
        List.Add(Name);
        MetaData.Next;
      end;
    finally
      InternalDisconnect;
    end;
  finally
    List.EndUpdate;
    MetaData.Free;
  end;
end;

{ Transaction control }

procedure TCustomDAConnection.SuppressAutoCommit;
var
  Temp: boolean;
begin
  Temp := False;
  FIConnection.SetProp(prAutoCommit, Temp);
end;

procedure TCustomDAConnection.RestoreAutoCommit;
begin
  FIConnection.SetProp(prAutoCommit, FAutoCommit);
end;

function TCustomDAConnection.DetectInTransaction(CanActivate: boolean = False): boolean;
var
  i: integer;
begin
  for i := 0 to FTransactions.Count - 1 do begin
    Result := FTransactions[i].DetectInTransaction(CanActivate and (FTransactions[i] = FInternalDefTransaction));
    if Result then
      exit;
  end;
  Result := False;
end;

function TCustomDAConnection.GetInTransaction: boolean;
begin
  Result := DetectInTransaction;
end;

function TCustomDAConnection.UsedTransaction: TDATransaction;
var
  i: integer;
begin
  Result := nil;
  if not IsMultipleTransactionsSupported then begin
    for i := 0 to FTransactions.Count - 1 do
      if FTransactions[i].Active then begin
        Result := FTransactions[i];
        exit;
      end;
  end;
  if Result = nil then
    Result := DefaultTransaction;
end;

function TCustomDAConnection.InternalAddTransaction(TR: TDATransaction): integer;
begin
  Result := FTransactions.IndexOf(TR);
  if Result = -1 then
    Result := FTransactions.Add(TR);
end;

procedure TCustomDAConnection.InternalRemoveTransaction(TR: TDATransaction);
begin
  FTransactions.Remove(TR);
  if TR = FDefaultTransaction then
    FDefaultTransaction := nil;
end;

function TCustomDAConnection.DoAddTransaction(TR: TDATransaction): integer;
begin
  Result := InternalAddTransaction(TR);
  if TR <> nil then
    TR.InternalAddConnection(Self);
end;

procedure TCustomDAConnection.DoRemoveTransaction(TR: TDATransaction);
begin
  if TR <> nil then                     //First we should close Transaction
    TR.InternalRemoveConnection(Self);  //Bug with DataSet.DefaultTransaction = nil
                                        //after InternalRemoveTransaction
  InternalRemoveTransaction(TR);        //Remove CLOSED transaction from Transaction list
                                        //and set DefaultTransaction to nil
end;

procedure TCustomDAConnection.StartTransaction;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.StartTransaction;
end;

procedure TCustomDAConnection.Commit;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.Commit;
end;

procedure TCustomDAConnection.Rollback;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.Rollback;
end;

procedure TCustomDAConnection.DoCommitRetaining;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoCommitRetaining;
end;

procedure TCustomDAConnection.DoRollbackRetaining;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoRollbackRetaining;
end;

procedure TCustomDAConnection.DoSavepoint(const Name: string);
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoSavepoint(Name);
end;

procedure TCustomDAConnection.DoRollbackToSavepoint(const Name: string);
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoRollbackToSavepoint(Name);
end;

procedure TCustomDAConnection.DoReleaseSavepoint(const Name: string);
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoReleaseSavepoint(Name);
end;

procedure TCustomDAConnection.ApplyUpdates(const DataSets: array of TCustomDADataSet);
var
  DataSet: TCustomDADataSet;
  i: integer;
  ReApply: boolean;
  CUTransactions: TDATransactions;
  ImplicitStarts: array of boolean;
  CUTransaction: TDATransaction;
  CUDataSets: array of TCustomDADataSet;
  CUDataSetCount, CUTransactionCount: integer;
  OldAutoCommit: boolean;
begin
  CUTransactions := TDATransactions.Create;
  try
    CUTransactions.Capacity := TransactionCount;
    SetLength(CUDataSets, DataSetCount);
    CUTransactionCount := 0;
    CUDataSetCount := 0;

    //Form Update Transaction array and determine if we can use failover
    for i := 0 to High(DataSets) do begin
      DataSet := DataSets[i];
      if DataSet.Connection <> Self then
        DatabaseError(Format(SUpdateWrongDB, [DataSet.Name, Name]));

      if DataSet.Active and DataSet.CachedUpdates then begin
        CUTransaction := DataSet.UsedUpdateTransaction;
        if CUTransaction= nil then
          raise Exception.Create(STransactionNotAssigned);
        if CUTransactions.IndexOf(CUTransaction) = -1 then begin
          CUTransactions.Add(CUTransaction);
          Inc(CUTransactionCount);
        end;
        CUDataSets[CUDataSetCount] := DataSet;
        Inc(CUDataSetCount);
      end;
    end;
    SetLength(ImplicitStarts, CUTransactionCount);

    ReApply := False;
    PushOperation(clConnectionApply, IsFailOverAllowed);
    try
      repeat
        try
          //Start update transactions and detrmine the commit status
          for i := 0 to CUTransactionCount - 1 do
            if not CUTransactions[i].Active then begin
              if not ReApply then // do not change transaction start flag in case of failover
                ImplicitStarts[i] := True;
              CUTransactions[i].StartTransaction;
            end
            else
              if not ReApply then
                ImplicitStarts[i] := False;

          ReApply := False;
          //Perform updates on each DataSet
          for i := 0 to CUDataSetCount - 1 do begin
            DataSet := CUDataSets[i];
            OldAutoCommit := DataSet.AutoCommit;
            DataSet.AutoCommit := False;
            try
              DataSet.ApplyUpdates;
            finally
              DataSet.AutoCommit := OldAutoCommit;
            end;
          end;

          //Commit all UpdateTransaction (this shoud be quick operation - it could break FailOver feature)
          if AutoCommit then
            for i := 0 to CUTransactionCount - 1 do
              if not ApplyUpdatesInTransaction or ImplicitStarts[i] then
                CUTransactions[i].Commit
              else if IsMultipleTransactionsSupported then
                CUTransactions[i].DoCommitRetaining;

        except
          on EFailOver do begin
            RestoreAfterFailOver; //restart read transactions
            ReApply := True;
          end
          else begin
            //RollBack all (uncommited !) changes
            for i := 0 to CUTransactionCount - 1 do
              if CUTransactions[i].Active then  //Transaction could be closed during FatalError handling
                                                //or during commiting, in case that exception was raised at commit time
                if not ApplyUpdatesInTransaction or ImplicitStarts[i] then
                  CUTransactions[i].Rollback
                else if IsMultipleTransactionsSupported then
                  CUTransactions[i].DoRollbackRetaining;
            raise;
          end;
        end;
      until (not ReApply);
    finally
      PopOperation;
    end;

    for i := 0 to CUDataSetCount - 1 do begin //this is not server operation so it couldn't raise failover
      DataSet := CUDataSets[i];
      DataSet.CommitUpdates;
    end;
  finally
    CUTransactions.Free;
  end;
end;

procedure TCustomDAConnection.ApplyUpdates;
var
  i: integer;
  DataSetArray: array of TCustomDADataSet;
begin
  SetLength(DataSetArray, 0);
  for i := 0 to DataSetCount - 1 do
    if DataSets[i] is TCustomDADataSet then begin
      SetLength(DataSetArray, Length(DataSetArray) + 1);
      DataSetArray[Length(DataSetArray) - 1] := TCustomDADataSet(DataSets[i]);
    end;

  ApplyUpdates(DataSetArray);
end;

//Operations stack
function TCustomDAConnection.PushOperation(Operation: TConnLostCause; AllowFailOver: boolean = true): integer;
var
  FOOperation: TFailOverOperation;
begin
  if Options.LocalFailover then begin
    if FOperationsStackLen = Length(FOperationsStack) then
      SetLength(FOperationsStack, FOperationsStackLen + OperationsStackDelta);

    Result := FOperationsStackLen;
    FOOperation.Operation := Operation;
    FOOperation.AllowFailOver := AllowFailOver;
    FOperationsStack[Result] := FOOperation;
    Inc(FOperationsStackLen);
  end
  else
    Result := 0;
end;

function TCustomDAConnection.PopOperation: TConnLostCause;
begin
  if Options.LocalFailover and (FOperationsStackLen > 0) then begin
    Dec(FOperationsStackLen);
    Result := FOperationsStack[FOperationsStackLen].Operation;
  end
  else
    Result := clUnknown;
end;

procedure TCustomDAConnection.ResetOnFatalError;
var
  i: integer;
begin
  //Close quietly all transactions cause of invalid connection (FatalError + FailOver)
  for i := 0 to FTransactions.Count - 1 do
    FTransactions[i].Reset;
end;

procedure TCustomDAConnection.RestoreAfterFailOver;
var
  i: integer;
begin
  for i := 0 to FTransactions.Count - 1 do
    FTransactions[i].Restore;
end;

function TCustomDAConnection.IsFailOverAllowed: boolean;
//This function check all transactions started against this connection
//and detrmine if FaiOver allowed according to transactions states:
//Connection must not have any active non ReadOnly ReadCommited transactions to perform FailOver
var
  i: integer;
  CRTrans: TCRTransaction;
begin
  for i := 0 to FTransactions.Count - 1 do begin
    CRTrans := FTransactions[i].FITransaction;
    if (CRTrans <> nil) and CRTrans.GetInTransaction and not CRTrans.CanRestoreAfterFailover then begin
      Result := False;
      exit;
    end;
  end;
  Result := True;
end;

function TCustomDAConnection.DetectConnLostCause(Component: TObject): TConnLostCause;
var
  i: integer;
  AllowFailOver: boolean;
begin
  Result := clUnknown;
  AllowFailOver := True;
  for i := FOperationsStackLen - 1 downto 0 do begin

    if Result < FOperationsStack[i].Operation then begin
      Result := FOperationsStack[i].Operation;
      AllowFailOver := FOperationsStack[i].AllowFailOver;
    end;

    case Result of
      clConnect: begin
        if TCustomDAConnection(Component).FShouldShowPrompt then
          Result := clUnknown;// This is the first connect or non DisconnectedMode - so we should raise exception
        break;
      end;
      clOpen, clExecute: begin
        if ((Component is TCustomDADataSet) and not TCustomDADataSet(Component).IsQuery) or
          (Component is TCustomDASQL) then
          Inc(FConnectCount); // Add ConnectCount - > cause of EndConnection in TCustomDADataSet.DoAfterExecute
      end;
    end;
  end;

  if not AllowFailOver then
    Result := clUnknown;

  case Result of
    clExecute, clOpen, clServiceQuery, clTransStart:
      if not IsFailOverAllowed then
        Result := clUnknown;                    //can't perform FailOver cause of possible unnoticed server changes lost
  end;
end;

procedure TCustomDAConnection.DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean;
  ReconnectAttempt: integer; var ConnLostCause: TConnLostCause);
var
  i: integer;
  FatalError: boolean;
  RetryMode: TRetryMode;
begin
  ConnLostCause := clUnknown;
  TDASQLMonitorClass(SQLMonitorClass).DBError(EDAError(E));

  FatalError := EDAError(E).IsFatalError;

  if FatalError then begin

    with EDAError(E) do begin
      ConnLostCause := DetectConnLostCause(Component);

      Reconnect :=
        ((Connected and (ReconnectAttempt = 0)) or
         (Options.LocalFailover and (ReconnectAttempt > 0)) or       // After first abortive attempt Connected = False
         (Options.DisconnectedMode and (ConnLostCause = clConnect)))  // For disconnect mode TODO:
        and (IsFailOverAllowed or (ConnLostCause = clConnectionApply));

      if Reconnect then
        for i := 0 to DataSetCount - 1 do begin
          if not (DataSets[i] is TCustomDADataSet) then
            continue;
          if TCustomDADataSet(DataSets[i]).Prepared or
            ((DataSets[i].Active and not TCustomDADataSet(DataSets[i]).FetchAll
              and not TCustomDADataSet(DataSets[i]).Fetched) and
              not (ConnLostCause = clTransStart) and //In case of using data modification UpdateTransaction (IBDAC - StartTransaction method)(rm - Bug #14947)
              not ((ConnLostCause = clOpen) and (DataSets[i] <> Component)) and //In case of composing a query to update data (rm - Bug #14947)
              not ((ConnLostCause = clRefresh) and (DataSets[i] = Component)))  //In case of Refresh and Active dataset with unfetched data.
          then begin

            Reconnect := False;
            Break;
          end;
        end;

      if Reconnect then
        for i := 0 to FSQLs.Count - 1 do begin
          Assert(TObject(FSQLs[i]) is TCustomDASQL);
          if TCustomDASQL(FSQLs[i]).Prepared and not TCustomDASQL(FSQLs[i]).Executing then begin
            Reconnect := False;
            Break;
          end;
        end;
    end;
  end;

  if Reconnect then
    if Options.LocalFailover then begin
      if (ConnLostCause = clUnknown) or (ConnLostCause = clExecute) then
        RetryMode := rmRaise
      else
        RetryMode := rmReconnectExecute;

      if Assigned(FOnConnectionLost) then
        FOnConnectionLost(Self, TComponent(EDAError(E).Component), ConnLostCause, RetryMode);
      Reconnect := RetryMode > rmRaise;
      Reexecute := ((RetryMode > rmReconnect) and not (ConnLostCause = clUnknown)) or
        ((ConnLostCause = clConnect) and (RetryMode >= rmReconnect));
      Fail := not Reexecute;
    end;

  if not Reexecute then
    if Assigned(FOnError) then
      FOnError(Self, EDAError(E), Fail);

  if FatalError and (FIConnection <> nil) then begin
    FIConnection.IsValid := False;
    if FIConnection.Pool <> nil then
      TCRConnectionPool(FIConnection.Pool).Invalidate;
  end;

  if FatalError and (ReconnectAttempt = 0) then
    ResetOnFatalError;

  if not FInProcessError and not Reconnect and FatalError and
    (ReconnectAttempt = 0) // If Attempt > 0 disconnect was called on CRAccess level
  then begin
    FInProcessError := True;
    try
      Disconnect;
    except // don't raise exception
    end;
    FInProcessError := False;
  end;
end;

procedure TCustomDAConnection.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDAConnection then begin
    TCustomDAConnection(Dest).Username := Username;
    TCustomDAConnection(Dest).Password := Password;
    TCustomDAConnection(Dest).Server := Server;
    TCustomDAConnection(Dest).LoginPrompt := LoginPrompt;
    PoolingOptions.AssignTo(TCustomDAConnection(Dest).PoolingOptions);
    TCustomDAConnection(Dest).Pooling := Pooling;
    TCustomDAConnection(Dest).ConnectDialog := ConnectDialog;
    TCustomDAConnection(Dest).OnError := OnError;
    TCustomDAConnection(Dest).ConvertEOL := ConvertEOL;
    Options.AssignTo(TCustomDAConnection(Dest).Options);
    TCustomDAConnection(Dest).AutoCommit := AutoCommit;
    TCustomDAConnection(Dest).DataTypeMap.Assign(DataTypeMap);

    TCustomDAConnection(Dest).AfterConnect := AfterConnect;
    TCustomDAConnection(Dest).BeforeConnect := BeforeConnect;
    TCustomDAConnection(Dest).AfterDisconnect := AfterDisconnect;
    TCustomDAConnection(Dest).BeforeDisconnect := BeforeDisconnect;
    TCustomDAConnection(Dest).OnLogin := OnLogin;

    TCustomDAConnection(Dest).IOHandler := IOHandler;

    if TCustomDAConnection(Dest).FSSLOptions <> nil then
      TCustomDAConnection(Dest).FSSLOptions.Assign(FSSLOptions);
    if TCustomDAConnection(Dest).FHttpOptions <> nil then
      TCustomDAConnection(Dest).FHttpOptions.Assign(FHttpOptions);
    if TCustomDAConnection(Dest).FProxyOptions <> nil then
      TCustomDAConnection(Dest).FProxyOptions.Assign(FProxyOptions);
  end
  else
    inherited;
end;

{$IFDEF FPC}

procedure TCustomDAConnection.GetLoginParams(out ADatabaseName, AUserName, APassword: string);
begin
  ADatabaseName := FServer;
  AUserName := FUsername;
  APassword := FPassword;
end;

procedure TCustomDAConnection.SetLoginParams(const ADatabaseName, AUserName, APassword: string); overload;
begin
  Server := ADatabaseName;
  Username := AUserName;
  Password := APassword;
end;

{$ENDIF}

procedure TCustomDAConnection.GetLoginParams(LoginParams: TStrings);
begin
  LoginParams.Add(SLoginParameterServer + '=' + FServer);
  LoginParams.Add(SLoginParameterUsername + '=' + FUsername);
  LoginParams.Add(SLoginParameterPassword + '=' + FPassword);
end;

procedure TCustomDAConnection.SetLoginParams(LoginParams: TStrings);
begin
  Server := LoginParams.Values[SLoginParameterServer];
  Username := LoginParams.Values[SLoginParameterUsername];
  Password := LoginParams.Values[SLoginParameterPassword];
end;

function TCustomDAConnection.GetConnected: boolean;
begin
  Result := (FIConnection <> nil) and FIConnection.GetConnected;
end;

procedure TCustomDAConnection.SetConnected(Value: boolean);
var
  Dialog: TCustomConnectDialog;
  DialogResult: boolean;
  OldBeforeDisconnect: TNotifyEvent;
  StoredConnectCount: integer;
  LoginParams: TStrings;
begin
  OldBeforeDisconnect := nil;
{$IFDEF NEXTGEN}
  TMethod(OldBeforeDisconnect).Code := nil; //TODO!!!
{$ENDIF}
  try
    if Value <> GetConnected then begin
      try
        // ignore exceptions to disconnect all client
        if not Value then begin
          FConnectCount := 0; // Explicit disconnect
          FShouldShowPrompt := True;
          if Assigned(BeforeDisconnect) then
            BeforeDisconnect(Self);

          while True do
            try
              if FInProcessDisconnecting then
                Exit;

              hRegisterClient.Enter;
              try
                if FInProcessDisconnecting then
                  Exit;
                FInProcessDisconnecting := True;
              finally
                hRegisterClient.Leave;
              end;

              try
                SendConnectEvent(False);
                break;
              finally
                FInProcessDisconnecting := False;
              end;
            except
              on E: EDAError do
                if not E.IsFatalError then
                  raise;
            end;
        end
        else
          if not (csReading in ComponentState) then
            Inc(FConnectCount);
      finally
        if csReading in ComponentState then begin
          FStreamedConnected := Value;
        end
        else if not Value then begin
          OldBeforeDisconnect := BeforeDisconnect;
          if Assigned(BeforeDisconnect) then // Design-time event lose fix
            BeforeDisconnect := nil;

          inherited SetConnected(False); // There is no server operations
        end;
      end;
      if not (csReading in ComponentState) and Value then begin
        if LoginPrompt and not (csDesigning in ComponentState) and Assigned(FOnLogin) then begin
          LoginParams := TStringList.Create;
          try
            GetLoginParams(LoginParams);
            FOnLogin(Self, LoginParams);
            SetLoginParams(LoginParams);

            StoredConnectCount := FConnectCount;
            try
              try
                PerformConnect;
              except
                if not Connected then
                  Dec(StoredConnectCount);
                raise;
              end;
            finally
              FConnectCount := StoredConnectCount;
            end;
          finally
            LoginParams.Free;
          end;
        end
        else if NeedPrompt and (ConnectDialogClass <> nil) then begin
          if FConnectDialog = nil then
            Dialog := ConnectDialogClass.Create(nil)
          else
            Dialog := FConnectDialog;
          StoredConnectCount := FConnectCount;
          DialogResult := False;
          try
            Dialog.FConnection := Self;
            DialogResult := Dialog.Execute;
          finally
            if not DialogResult then
              Dec(StoredConnectCount);
            FConnectCount := StoredConnectCount;
            if FConnectDialog = nil then
              Dialog.Free;
          end;

          if not DialogResult then begin
            if FStreamedConnected or (csDesigning in ComponentState) then
              DatabaseError(SCannotConnect)
            else
              Abort;
          end;
        end
        else begin
          StoredConnectCount := FConnectCount;
          try
            try
              PerformConnect;
            except
              if not Connected then
                Dec(StoredConnectCount); // Restore ConnectCount in case of Connection Failure
              raise;
            end;
          finally
            FConnectCount := StoredConnectCount;
          end;
        end;
      end;
    end;
  finally
    if Assigned(OldBeforeDisconnect) then
      BeforeDisconnect := OldBeforeDisconnect;
  end;
end;

procedure TCustomDAConnection.SetDefaultTransaction(Value: TDATransaction);
begin
  if Value = FInternalDefTransaction then
    Value := nil;
  if FDefaultTransaction <> Value then begin
    if FDefaultTransaction <> nil then begin
      FDefaultTransaction.RemoveFreeNotification(Self);
      DoRemoveTransaction(FDefaultTransaction);
    end;

    if Value <> nil then begin
      DoAddTransaction(Value);
      Value.FreeNotification(Self);
    end;

    FDefaultTransaction := Value;
  end;
end;

function TCustomDAConnection.GetDefaultTransaction: TDATransaction;
begin
  Result := FDefaultTransaction;
  if Result = nil then
    Result := FInternalDefTransaction;
end;

function TCustomDAConnection.GetTransaction(Index: integer): TDATransaction;
begin
  Result := FTransactions[Index];
end;

function TCustomDAConnection.GetTransactionsCount: integer;
begin
  Result := FTransactions.Count;
end;

procedure TCustomDAConnection.SetUsername(const Value: string);
begin
  if Value <> FUsername then begin
    Disconnect;
    FUsername := Value;
    UniqueString(FUsername);
  end;
end;

procedure TCustomDAConnection.SetPassword(const Value: string);
begin
  if Value <> FPassword then begin
    Disconnect;
    FPassword := Value;
    UniqueString(FPassword);
  end;
end;

procedure TCustomDAConnection.SetServer(const Value: string);
begin
  if Value <> FServer then begin
    Disconnect;
    FServer := Value;
    UniqueString(FServer);
  end;
end;

function TCustomDAConnection.GetConnectionString: string;
begin
  Result := FConnectionStringBuilder.ConnectionString;
end;

procedure TCustomDAConnection.SetConnectionString(const Value: string);
begin
  FConnectionStringBuilder.ConnectionString := Value;
end;

procedure TCustomDAConnection.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('Password', ReadPassword, nil, True);
  Filer.DefineProperty('EncryptedPassword', ReadEncryptedPassword, WriteEncryptedPassword, (Password <> ''));
end;

procedure TCustomDAConnection.ReadPassword(Reader: TReader);
begin
  Password := Reader.ReadString;
end;

procedure TCustomDAConnection.ReadEncryptedPassword(Reader: TReader);
begin
  Password := DecryptFromHex(Reader.ReadString);
end;

procedure TCustomDAConnection.WriteEncryptedPassword(Writer: TWriter);
begin
  Writer.WriteString(EncryptToHex(Password));
end;

function TCustomDAConnection.Decrypt(const Value: TBytes): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(Value));
  for i := 0 to Length(Value) - 1 do
    Result[i] := not Value[i];
end;

function TCustomDAConnection.Encrypt(const Value: TBytes): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(Value));
  for i := 0 to Length(Value) - 1 do
    Result[i] := not Value[i];
end;

function TCustomDAConnection.EncryptToHex(const Value: string ): string;
var
  Buf: TBytes;
  Len: Integer;
begin
  Buf := Encoding.Unicode.GetBytes(WideString(Value));
  Buf := Encrypt(Buf);
  Len := Length(Buf);
  SetLength(Result, Len * 2);
{$IFDEF IS_UNICODE}
  BinToHexW(Buf, PWideChar(Result), Len);
{$ELSE}
  BinToHexA(Buf, PAnsiChar(Result), Len);
{$ENDIF}
end;

function TCustomDAConnection.DecryptFromHex(const Value: string): string;
var
{$IFDEF VER12P}
  HexStr: WideString;
{$ELSE}
  HexStr: AnsiString;
{$ENDIF}
  Buf: TBytes;
  Len: Integer;
begin
  HexStr := Value;
  Len := Length(HexStr) shr 1;
  SetLength(Buf, Len);
{$IFDEF IS_UNICODE}
  Len := HexToBinW(PWideChar(HexStr), Buf, Len);
{$ELSE}
  Len := HexToBinA(PAnsiChar(HexStr), Buf, Len);
{$ENDIF}
  SetLength(Buf, Len);
  Buf := Decrypt(Buf);
  result := string(Encoding.Unicode.GetString(Buf));
end;

function TCustomDAConnection.InternalGetSQL: TCustomDASQL;
begin
  CheckCommand;

  Result := FCommand;
end;

procedure TCustomDAConnection.SetAutoCommit(Value: boolean);
begin
  FAutoCommit := Value;
  if FIConnection <> nil then
    FIConnection.SetProp(prAutoCommit, FAutoCommit);
end;

procedure TCustomDAConnection.SetConvertEOL(Value: boolean);
begin
  FConvertEOL := Value;
  if FIConnection <> nil then
    FIConnection.SetProp(prConvertEOL, Value);
end;

procedure TCustomDAConnection.CheckCommand;
begin
  if FCommand = nil then begin
    FCommand := CreateSQL;
    FCommand.Debug := Debug;
  {$IFDEF NEXTGEN}
    FCommand.FConnection.__ObjRelease;
  {$ENDIF}
  end;
end;

procedure TCustomDAConnection.AssignConnectOptions(Source: TCustomDAConnection);
begin
  Username := Source.Username;
  Password := Source.Password;
  Server := Source.Server;
end;

function TCustomDAConnection.CreateOptions: TDAConnectionOptions;
begin
  Result := TDAConnectionOptions.Create(Self);
end;

procedure TCustomDAConnection.SetOptions(Value: TDAConnectionOptions);
begin
  FOptions.Assign(Value);
end;

function TCustomDAConnection.CreatePoolingOptions: TPoolingOptions;
begin
  Result := TPoolingOptions.Create(Self);
end;

procedure TCustomDAConnection.SetPoolingOptions(Value: TPoolingOptions);
begin
  FPoolingOptions.Assign(Value);
end;

function TCustomDAConnection.CreateConnectionStringBuilder: TCRConnectionStringBuilder;
begin
  Result := TCRConnectionStringBuilder.Create(GetConnectionStringParam, SetConnectionStringParam);
end;

procedure TCustomDAConnection.SetConnectDialog(Value: TCustomConnectDialog);
begin
  if Value <> FConnectDialog then begin
    if FConnectDialog <> nil then begin
      FConnectDialog.RemoveFreeNotification(Self);
      if FConnectDialog.FConnection = Self then
        FConnectDialog.FConnection := nil;
    end;

    FConnectDialog := Value;

    if FConnectDialog <> nil then begin
      FConnectDialog.FreeNotification(Self);
      FConnectDialog.FConnection := Self;
    end;
  end;
end;

procedure TCustomDAConnection.SetPooling(Value: boolean);
begin
  if FPooling <> Value then begin
    SetConnected(False);
    FreeIConnection;
  end;
  FPooling := Value;
end;

procedure TCustomDAConnection.SetDebug(Value: boolean);
begin
  FDebug := Value;
  if FCommand <> nil then
    FCommand.Debug := Value;
end;

function TCustomDAConnection.GetConnectionStringParam(Param: integer): variant;
begin
  Result := '';
  case Param of
    cpLoginPrompt:
      Result := Self.LoginPrompt;
    cpPooling:
      Result := Self.Pooling;
    cpConnectionLifetime:
      Result := Self.PoolingOptions.ConnectionLifetime;
    cpMaxPoolSize:
      Result := Self.PoolingOptions.MaxPoolSize;
    cpMinPoolSize:
      Result := Self.PoolingOptions.MinPoolSize;
    cpValidateConnection:
      Result := Self.PoolingOptions.Validate;

    prSSLCA:
      Result := FSSLOptions.CACert;
    prSSLCert:
      Result := FSSLOptions.Cert;
    prSSLCipher:
      Result := FSSLOptions.CipherList;
    prSSLKey:
      Result := FSSLOptions.Key;

    prUseHttp:
      Result := FHttpOptions.Enabled;
    prHttpUrl:
      Result := FHttpOptions.Url;
    prHttpUsername:
      Result := FHttpOptions.Username;
    prHttpPassword:
      Result := FHttpOptions.Password;
    prHttpTrustServerCertificate:
      Result := FHttpOptions.TrustServerCertificate;

    prProxyHostname:
      Result := FProxyOptions.Hostname;
    prProxyUsername:
      Result := FProxyOptions.Username;
    prProxyPassword:
      Result := FProxyOptions.Password;
    prProxyPort:
      Result := FProxyOptions.Port;
    prProxySocksVersion:
      Result := integer(FProxyOptions.SocksVersion);
    prProxyResolveDNS:
      Result := FProxyOptions.ResolveDNS;

  else
    raise Exception.Create('Connection parameter is unknown');
  end;
end;

procedure TCustomDAConnection.SetConnectionStringParam(Param: integer; const Value: variant);
begin
  case Param of
    cpLoginPrompt:
      Self.LoginPrompt := Value;
    cpPooling:
      Self.Pooling := Value;
    cpConnectionLifetime:
      Self.PoolingOptions.ConnectionLifetime := Value;
    cpMaxPoolSize:
      Self.PoolingOptions.MaxPoolSize := Value;
    cpMinPoolSize:
      Self.PoolingOptions.MinPoolSize := Value;
    cpValidateConnection:
      Self.PoolingOptions.Validate := Value;

    prSSLCA:
      FSSLOptions.CACert := Value;
    prSSLCert:
      FSSLOptions.Cert := Value;
    prSSLCipher:
      FSSLOptions.CipherList := Value;
    prSSLKey:
      FSSLOptions.Key := Value;

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
    prProxyUsername:
      FProxyOptions.Username := Value;
    prProxyPassword:
      FProxyOptions.Password := Value;
    prProxyPort:
      FProxyOptions.Port := Value;
    prProxySocksVersion:
      FProxyOptions.SocksVersion := TCRSocksVersion(integer(Value));
    prProxyResolveDNS:
      FProxyOptions.ResolveDNS := boolean(Value);

  else
    raise Exception.Create('Connection parameter is unknown');
  end;
end;

procedure TCustomDAConnection.DoAfterConnect;
begin
  if Assigned(AfterConnect) then
    AfterConnect(Self);
end;

{ TDAParamValue }

constructor TDAParamValue.Create(Param: TDAParam);
begin
  inherited Create;

  FParam := Param;
end;

procedure TDAParamValue.CheckDataType(Value: TFieldType; const CompatibleTypes: array of TFieldType);
var
  i: Integer;
  ParamDataType: TFieldType;
begin
  ParamDataType := Param.GetDataType;

  if ParamDataType = Value then
    Exit;

  for i := 0 to Length(CompatibleTypes) - 1 do
    if ParamDataType = CompatibleTypes[i] then
      Exit;

  if ParamDataType = ftUnknown then begin
    Param.SetDataType(Value);
    Exit;
  end;

  raise Exception.Create(SParameterTypeDiffers)
end;

procedure TDAParamValue.CheckBlobDataType(Value: TFieldType);
var
  ParamDataType: TFieldType;
begin
  ParamDataType := Param.GetDataType;

  if ParamDataType = Value then
    Exit;

  if FParam.IsBlobDataType(ParamDataType) then
    Exit;

  if ParamDataType = ftUnknown then begin
    Param.SetDataType(Value);
    Exit;
  end;

  raise Exception.Create(SParameterTypeDiffers)
end;

procedure TDAParamValue.SetAsSmallInt(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckDataType(ftSmallInt, [ftWord, ftInteger, ftLargeint{$IFDEF VER12P}, ftLongWord{$ENDIF}]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsSmallInt := Value;
end;

function TDAParamValue.GetAsInteger: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF};
begin
  if FParam.FValueCount > 1 then
    Result := Integer(FParam.FDataArr[Index])
  else
    Result := FParam.AsInteger;
end;

procedure TDAParamValue.SetAsInteger(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckDataType(ftInteger, [ftInteger, ftLargeint{$IFDEF VER12P}, ftLongWord{$ENDIF}]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsInteger := Value;
end;

procedure TDAParamValue.SetAsWord(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckDataType(ftWord, [ftSmallint, ftInteger, ftLargeint{$IFDEF VER12P}, ftLongWord{$ENDIF}]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsWord := Value;
end;

function TDAParamValue.GetAsLargeInt: LargeInt;
begin
  if FParam.FValueCount > 1 then
    Result := {$IFDEF VER16P}LargeInt{$ENDIF}(FParam.FDataArr[Index])
  else
    Result := FParam.AsLargeInt;
end;

procedure TDAParamValue.SetAsLargeInt(Value: LargeInt);
begin
  CheckDataType(ftLargeInt, [{$IFDEF VER12P}ftLongWord{$ENDIF}]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsLargeInt := Value;
end;

{$IFDEF VER12P}
function TDAParamValue.GetAsLongWord: {$IFDEF VER26P}Cardinal{$ELSE}LongWord{$ENDIF};
begin
  if FParam.FValueCount > 1 then
    Result := LongWord(FParam.FDataArr[Index])
  else
    Result := FParam.AsLongWord;
end;

procedure TDAParamValue.SetAsLongWord(Value: {$IFDEF VER26P}Cardinal{$ELSE}LongWord{$ENDIF});
begin
  CheckDataType(ftLongWord, [ftLargeint]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsLongWord := Value;
end;

procedure TDAParamValue.SetAsShortInt(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckDataType(ftShortInt, [ftByte, ftWord, ftInteger, ftLargeint, ftLongWord]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsShortInt := Value;
end;

procedure TDAParamValue.SetAsByte(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckDataType(ftByte, [ftShortint, ftWord, ftInteger, ftLargeint, ftLongWord]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsByte := Value;
end;
{$ENDIF}

function TDAParamValue.GetAsString: string;
begin
{$IFDEF VER12P}
  Result := AsWideString;
{$ELSE}
  Result := AsAnsiString;
{$ENDIF}
end;

procedure TDAParamValue.SetAsString(const Value: string);
var
  ParamDataType: TFieldType;
begin
  ParamDataType := FParam.DataType; // cache DataType value

  if not FParam.IsBlobDataType(ParamDataType) then
    CheckDataType(ftString, [ftFixedChar, ftWideString, TFieldType(ftFixedWideChar)]);
{$IFDEF VER12P}
  {$IFNDEF NEXTGEN}
  if ParamStringAsAnsiString then begin
    if not FParam.IsBlobDataType(ParamDataType) then
      if not (ParamDataType in [ftString, ftFixedChar]) then
        FParam.SetDataType(ftString);
    AsAnsiString := AnsiString(Value);
  end
  else
  {$ENDIF}
  begin
    if not FParam.IsBlobDataType(ParamDataType) then
      if not (ParamDataType in [ftWideString, TFieldType(ftFixedWideChar)]) then
        FParam.SetDataType(ftWideString);
    AsWideString := Value;
  end;
{$ELSE}
  if not (ParamDataType in [ftString, ftFixedChar]) then
    FParam.SetDataType(ftString);
  AsAnsiString := Value;
{$ENDIF}
end;

{$IFNDEF NEXTGEN}

function TDAParamValue.GetAsAnsiString: AnsiString;
var
  Obj: TSharedObject;
  ValuePtr: PVarData;
  ParamDataType: TFieldType;
begin
  if FParam.FValueCount > 1 then begin
    ParamDataType := Param.GetDataType;

    if FParam.IsBlobDataType(ParamDataType) then begin
      Obj := GetValueObject;
      if Obj <> nil then
        Result := TBlob(Obj).AsAnsiString
      else
        Result := '';
    end
    else begin
      ValuePtr := @FParam.FDataArr[Index];
      if ValuePtr.VType in [varEmpty, varNull] then
        Result := ''
      else
        case ParamDataType of
          ftDate:
            Result := AnsiString(DateToStr(ValuePtr.VDate));
          ftTime:
            Result := AnsiString(TimeToStr(ValuePtr.VDate));
          ftDateTime:
            Result := AnsiString(DateTimeToStr(ValuePtr.VDate));
          else
            Result := AnsiString(PVariant(ValuePtr)^);
        end;
    end;
  end
  else
    Result := FParam.AsAnsiString;
end;

procedure TDAParamValue.SetAsAnsiString(const Value: AnsiString);
var
  Obj: TSharedObject;
  ParamDataType: TFieldType;
begin
  ParamDataType := Param.GetDataType;

  if not FParam.IsBlobDataType(ParamDataType) then
    CheckDataType(ftString, [ftFixedChar]);

  if FParam.FValueCount > 1 then begin
    if FParam.IsBlobDataType(ParamDataType) then begin
      Obj := AllocValueObject;
      TBlob(Obj).AsAnsiString := Value;
    end
    else
      FParam.FDataArr[Index] := Value;
  end
  else
    FParam.AsAnsiString := Value;
end;

{$ENDIF}

function TDAParamValue.GetAsWideString: WideString;
var
  Obj: TSharedObject;
  ValuePtr: PVarData;
  ParamDataType: TFieldType;
begin
  if FParam.FValueCount > 1 then begin
    ParamDataType := Param.GetDataType;

    if FParam.IsBlobDataType(ParamDataType) then begin
      Obj := GetValueObject;
      if Obj <> nil then
        Result := TBlob(Obj).AsWideString
      else
        Result := '';
    end
    else begin
      ValuePtr := @FParam.FDataArr[Index];
      if ValuePtr.VType in [varEmpty, varNull] then
        Result := ''
      else
        case ParamDataType of
          ftDate:
            Result := WideString(DateToStr(ValuePtr.VDate));
          ftTime:
            Result := WideString(TimeToStr(ValuePtr.VDate));
          ftDateTime:
            Result := WideString(DateTimeToStr(ValuePtr.VDate))
          else
            Result := WideString(PVariant(ValuePtr)^);
        end;
    end;
  end
  else
    Result := Param.GetAsWideString;
end;

procedure TDAParamValue.SetAsWideString(const Value: WideString);
var
  Obj: TSharedObject;
  ParamDataType: TFieldType;
begin
  ParamDataType := Param.GetDataType;

  if not FParam.IsBlobDataType(ParamDataType) then
    CheckDataType(ftWideString, [TFieldType(ftFixedWideChar)]);

  if FParam.FValueCount > 1 then begin
    if FParam.IsBlobDataType(ParamDataType) then begin
      Obj := AllocValueObject;
      TBlob(Obj).AsWideString := Value;
    end
    else
      FParam.FDataArr[Index] := Value;
  end
  else
    FParam.AsWideString := Value;
end;

function TDAParamValue.GetAsBCD: Currency;
begin
  if IsNull then
    Result := 0
  else if FParam.FValueCount > 1 then
    Result := FParam.FDataArr[Index]
  else
  {$IFNDEF FPC}
    Result := FParam.AsBCD;
  {$ELSE}
    Result := FParam.AsCurrency;
  {$ENDIF}
end;

procedure TDAParamValue.SetAsBCD(const Value: Currency);
begin
  CheckDataType(ftBCD, []);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
  {$IFNDEF FPC}
    FParam.AsBCD := Value;
  {$ELSE}
    FParam.AsCurrency := Value;
  {$ENDIF}
end;

function TDAParamValue.GetAsFMTBCD: TBcd;
begin
  if IsNull then
    Result := NullBcd
  else if FParam.FValueCount > 1 then
    Result := VarToBcd(FParam.FDataArr[Index])
  else
    Result := FParam.AsFMTBCD;
end;

procedure TDAParamValue.SetAsFMTBCD(const Value: TBcd);
begin
  CheckDataType(ftFMTBCD, []);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := VARFMTBcdCreate(Value)
  else
    FParam.AsFMTBCD := Value;
end;

{$IFNDEF FPC}

function TDAParamValue.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  if IsNull then
    Result := NullSqlTimeStamp
  else if FParam.FValueCount > 1 then
    Result := VarToSQLTimeStamp(FParam.FDataArr[Index])
  else
    Result := FParam.AsSQLTimeStamp;
end;

procedure TDAParamValue.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  CheckDataType(ftTimeStamp, []);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := VarSQLTimeStampCreate(Value)
  else
    FParam.AsSQLTimeStamp := Value;
end;

{$ENDIF}

function TDAParamValue.GetAsBoolean: Boolean;
begin
  if IsNull then
    Result := False
  else if FParam.FValueCount > 1 then
    Result := FParam.FDataArr[Index]
  else
    Result := FParam.AsBoolean;
end;

procedure TDAParamValue.SetAsBoolean(Value: Boolean);
begin
  CheckDataType(ftBoolean, []);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsBoolean := Value;
end;

function TDAParamValue.GetAsCurrency: Currency;
begin
  if IsNull then
    Result := 0
  else if FParam.FValueCount > 1 then
    Result := FParam.FDataArr[Index]
  else
    Result := FParam.AsCurrency;
end;

procedure TDAParamValue.SetAsCurrency(const Value: Currency);
begin
  CheckDataType(ftCurrency, []);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsCurrency := Value;
end;

procedure TDAParamValue.SetAsDate(const Value: TDateTime);
begin
  CheckDataType(ftDate, [ftTime, ftDateTime]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsDate := Value;
end;

procedure TDAParamValue.SetAsTime(const Value: TDateTime);
begin
  CheckDataType(ftTime, [ftDate, ftDateTime]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsTime := Value;
end;

function TDAParamValue.GetAsDateTime: TDateTime;
begin
  if IsNull then
    Result := 0
  else if FParam.FValueCount > 1 then
    Result := VarToDateTime(FParam.FDataArr[Index])
  else
    Result := FParam.AsDateTime;
end;

procedure TDAParamValue.SetAsDateTime(const Value: TDateTime);
begin
  CheckDataType(ftDateTime, [ftDate, ftTime]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsDateTime := Value;
end;

{$IFDEF VER14P}

function TDAParamValue.GetAsSQLTimeStampOffset: TSQLTimeStampOffset;
begin
  if IsNull then
    Result := NullSqlTimeStampOffset
  else if FParam.FValueCount > 1 then
    Result := VarToSQLTimeStampOffset(FParam.FDataArr[Index])
  else
    Result := FParam.AsSQLTimeStampOffset;
end;

procedure TDAParamValue.SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset);
begin
  CheckDataType(ftTimeStampOffset, []);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := VarSQLTimeStampOffsetCreate(Value)
  else
    FParam.AsSQLTimeStampOffset := Value;
end;

function TDAParamValue.GetAsSingle: Single;
begin
  if IsNull then
    Result := 0
  else if FParam.FValueCount > 1 then
    Result := FParam.FDataArr[Index]
  else
    Result := FParam.AsSingle;
end;

procedure TDAParamValue.SetAsSingle(const Value: Single);
begin
  CheckDataType(TFieldType.ftSingle, [ftFloat]);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsSingle := Value;
end;

{$ENDIF}

function TDAParamValue.GetAsFloat: Double;
begin
  if IsNull then
    Result := 0
  else if FParam.FValueCount > 1 then
    Result := FParam.FDataArr[Index]
  else
    Result := FParam.AsFloat;
end;

procedure TDAParamValue.SetAsFloat(const Value: Double);
begin
  CheckDataType(ftFloat, []);
  if FParam.FValueCount > 1 then
    FParam.FDataArr[Index] := Value
  else
    FParam.AsFloat := Value;
end;

procedure TDAParamValue.SetAsMemo(const Value: string);
var
  Obj: TSharedObject;
begin
  CheckBlobDataType(ftMemo);
  Obj := AllocValueObject;
  TBlob(Obj).AsString := Value;
end;

function TDAParamValue.GetAsBytes: TBytes;
var
  Obj: TSharedObject;
  ParamDataType: TFieldType;
begin
  ParamDataType := Param.GetDataType;

  if not FParam.IsBlobDataType(ParamDataType) then
    CheckDataType(ftBytes, [ftVarBytes]);

  if FParam.FValueCount > 1 then begin
    if FParam.IsBlobDataType(ParamDataType) then begin
      Obj := GetValueObject;
      if Obj <> nil then
        Result := TBlob(Obj).AsBytes
      else
        Result := nil;
    end
    else
      Result := FParam.FDataArr[Index];
  end
  else
    Result := FParam.AsBytes;
end;

procedure TDAParamValue.SetAsBytes(const Value: TBytes);
var
  Obj: TSharedObject;
  ParamDataType: TFieldType;
begin
  ParamDataType := Param.GetDataType;
  if not FParam.IsBlobDataType(ParamDataType) then
    CheckDataType(ftBytes, [ftVarBytes]);

  if FParam.FValueCount > 1 then begin
    if FParam.IsBlobDataType(ParamDataType) then begin
      Obj := AllocValueObject;
      TBlob(Obj).AsBytes := Value;
    end
    else
      FParam.FDataArr[Index] := Value;
  end
  else
    FParam.AsBytes := Value;
end;

procedure TDAParamValue.SetAsBlob(const Value: TBlobData);
var
  Obj: TSharedObject;
begin
  CheckBlobDataType(ftBlob);
  Obj := AllocValueObject;
{$IFDEF VER12P}
  TBlob(Obj).AsBytes := Value;
{$ELSE}{$IFDEF FPC}
  TBlob(Obj).AsBytes := Value;
{$ELSE}
  TBlob(Obj).AsString := Value;
{$ENDIF}{$ENDIF}
end;

function TDAParamValue.GetAsBlobRef: TBlob;
var
  Obj: TSharedObject;
begin
  CheckBlobDataType(ftBlob);
  Obj := AllocValueObject;
  Result := TBlob(Obj)
end;

procedure TDAParamValue.SetAsBlobRef(const Value: TBlob);
begin
  CheckBlobDataType(ftBlob);
  SetValueObject(Value);
end;

function TDAParamValue.GetAsVariant: Variant;
var
  Obj: TSharedObject;
begin
  if FParam.FValueCount > 1 then
    if FParam.IsBlob then begin
      Obj := GetValueObject;
      if Obj <> nil then
        if TBlob(Obj).IsNull then
          Result := Null
        else
          Result := TBlob(Obj).AsString
      else
        Result := Unassigned;
    end
    else
      Result := FParam.FDataArr[Index]
  else
    Result := FParam.Value;
end;

procedure TDAParamValue.SetAsVariant(const Value: Variant);
var
  Obj: TSharedObject;
  ParamDataType: TFieldType;
begin
  if TVarData(Value).VType in [varEmpty, varNull] then
    SetIsNull(True)
  else begin
    ParamDataType := Param.GetDataType;
    if ParamDataType = ftUnknown then begin
      ParamDataType := TDAParam.GetVarType(VarType(Value));
      Param.SetDataType(ParamDataType);
    end;

    case ParamDataType of
      ftUnknown: SetAsString(Value);
    {$IFNDEF NEXTGEN}
      ftFixedChar: SetAsAnsiString(AnsiString(Value));
      ftString: SetAsAnsiString(AnsiString(Value));
    {$ELSE}
      ftFixedChar: SetAsString(Value);
      ftString: SetAsString(Value);
    {$ENDIF}
      ftSmallint: SetAsSmallInt(Value);
      ftInteger: SetAsInteger(Value);
      ftWord: SetAsWord(Value);
      ftBoolean: SetAsBoolean(Value);
      ftFloat: SetAsFloat(Value);
      ftCurrency: SetAsCurrency(Value);
      ftDate: SetAsDate(Value);
      ftTime: SetAsTime(Value);
      ftDateTime: SetAsDateTime(Value);
      ftBytes,
      ftVarBytes: SetAsBytes(Value);
      ftOraBlob, ftBlob:
        if TVarData(Value).VType = varSharedObject then
          SetValueObject(TVarData(Value).VPointer)
        else begin
          Obj := AllocValueObject;
          if VarType(Value) = varArray or varByte then
            TBlob(Obj).AsBytes := Value
          else
            TBlob(Obj).AsString := VarToStr(Value);
        end;
    {$IFDEF VER10P}
      ftWideMemo,
    {$ENDIF}
    {$IFDEF FPC}
      ftWideMemo,
    {$ENDIF}
      ftOraClob, ftMemo:
        if TVarData(Value).VType = varSharedObject then
          SetValueObject(TVarData(Value).VPointer)
        else
          SetAsMemo(Value);
    {$IFDEF VER12P}
      ftFixedWideChar: SetAsWideString(Value);
    {$ENDIF}
      ftWideString: SetAsWideString(Value);
      ftLargeint: SetAsLargeInt(Value);
      ftBCD: SetAsBCD(Value);
      ftFMTBcd: SetAsFMTBCD(VarToBcd(Value));
    {$IFNDEF FPC}
      ftTimeStamp: SetAsSQLTimeStamp(VarToSQLTimeStamp(Value));
    {$ENDIF}
    {$IFDEF VER12P}
      ftByte: SetAsByte(Value);
      ftShortint: SetAsShortInt(Value);
      ftLongWord: SetAsLongWord(Value);
    {$ENDIF}
    {$IFDEF VER14P}
      ftTimeStampOffset: SetAsSQLTimeStampOffset(VarToSQLTimeStampOffset(Value));
    {$ENDIF}
    {$IFDEF VER14P}
      TFieldType.ftSingle: SetAsSingle(Value);
    {$ENDIF}
    else
      if FParam.FValueCount > 1 then
        FParam.FDataArr[Index] := Value
      else
        FParam.Value := Value;
    end;
  end;
end;

function TDAParamValue.GetIsNull: boolean;
var
  ValuePtr: PVarData;
  ParamDataType: TFieldType;
begin
  if FParam.FValueCount > 1 then begin
    ValuePtr := @Param.FDataArr[Index];
    if ValuePtr.VType in [varEmpty, varNull] then
      Result := True
    else begin
      ParamDataType := Param.GetDataType;
      if FParam.IsSharedObjectDataType(ParamDataType) then
        if ValuePtr.VPointer = nil then
          Result := True
        else if FParam.IsBlobDataType(ParamDataType) then
          Result := TBlob(ValuePtr.VPointer).IsNull
        else
          Result := False
      else
        Result := False
    end;
  end
  else
    Result := FParam.IsNull;
end;

procedure TDAParamValue.SetIsNull(Value: boolean);
var
  Obj: TSharedObject;
  ParamDataType: TFieldType;
begin
  if FParam.FValueCount > 1 then begin
    if Value then begin
      ParamDataType := Param.GetDataType;
      if FParam.IsBlobDataType(ParamDataType) then begin
        Obj := GetValueObject;
        if Obj <> nil then
          TBlob(Obj).Clear
      end
      else begin
        if FParam.IsSharedObjectDataType(ParamDataType) then
          FreeValueObject;
        FParam.FDataArr[Index] := Unassigned;
      end;
    end;
  end
  else
    FParam.SetIsNull(Value);
end;

function TDAParamValue.CreateValueObject: TSharedObject;
var
  ValuePtr: PVarData;
begin
  Result := FParam.CreateObject;
  ValuePtr := @Param.FDataArr[Index];
  ValuePtr.VType := varSharedObject;
  ValuePtr.VPointer := Result;
end;

procedure TDAParamValue.FreeValueObject;
var
  ValuePtr: PVarData;
begin
  ValuePtr := @Param.FDataArr[Index];
  if ValuePtr.VPointer <> nil then begin
    TSharedObject(ValuePtr.VPointer).Free;
    ValuePtr.VType := varNull;
    ValuePtr.VPointer := nil;
  end;
end;

function TDAParamValue.AllocValueObject: TSharedObject;
begin
  Result := GetValueObject;
  if (Result = nil) and FParam.IsSharedObject then
    Result := CreateValueObject;
end;

function TDAParamValue.GetValueObject: TSharedObject;
var
  ValuePtr: PVarData;
begin
  if FParam.FValueCount > 1 then begin
    ValuePtr := @Param.FDataArr[Index];
    if ValuePtr.VType = varSharedObject then
      Result := TSharedObject(ValuePtr.VPointer)
    else
      Result := nil;
  end
  else
    Result := FParam.ParamObject;
end;

procedure TDAParamValue.SetValueObject(Value: TSharedObject);
var
  ValuePtr: PVarData;
begin
  if FParam.FValueCount > 1 then begin
    ValuePtr := @Param.FDataArr[Index];
    if ValuePtr.VType = varSharedObject then begin
      if ValuePtr.VPointer = Value then
        Exit;
      if ValuePtr.VPointer <> nil then
        FreeValueObject;
    end;

    if Value <> nil then begin
      ValuePtr.VType := varSharedObject;
      ValuePtr.VPointer := Value;
      Value.AddRef;
    end;
  end
  else
    FParam.ParamObject := Value;
end;

procedure TDAParamValue.Clear;
begin
  SetIsNull(True);
end;

{ TDAParam }

constructor TDAParam.Create(Collection: TCollection);
begin
  inherited;

  Bound := BoundParams;

  SetLength(FDataArr, 1);
  FDataArr[0] := Unassigned;
  FParamValue := nil;
  FValueCount := 1;
end;

destructor TDAParam.Destroy;
begin
  FreeObject;
  FreeValues;

  if FParamValue <> nil then
    FParamValue.Free;

  inherited;
end;

procedure TDAParam.Clear;
begin
  SetValueCount(1);

  if IsBlob then begin
    Assert(ParamObject is TBlob);
    TBlob(ParamObject).Clear;
  end
  else
    inherited Clear;
end;

procedure TDAParam.Assign(Source: TPersistent);

  procedure Load(const StreamPersist: IStreamPersist);
  var
    MS: TMemoryStream;
  begin
    if not (DataType in BlobTypes) then
      DataType := ftBlob;

    MS := TMemoryStream.Create;
    try
      StreamPersist.SaveToStream(MS);
      LoadFromStream(MS, DataType);
    finally
      MS.Free;
    end;
  end;

var
  i: integer;
  StreamPersist: IStreamPersist;
  SourceValuePtr: PVarData;
begin
  if Source is TDAParam then begin
    AssignParam(TParam(Source));
    ValueCount := TDAParam(Source).ValueCount;
    TParam(Self).Size := TParam(Source).Size; // CR11511
    FSize := TDAParam(Source).FSize; // CR11511
    if ValueCount <= 1 then
      ParamObject := TDAParam(Source).ParamObject
    else if TDAParam(Source).IsSharedObject then
      for i := 0 to ValueCount - 1 do begin
        SourceValuePtr := @TDAParam(Source).FDataArr[i];
        // do not use GetValueObject to avoid unnecessary objects creation
        if SourceValuePtr.VType = varSharedObject then
          GetParamValue(i).SetValueObject(SourceValuePtr.VPointer)
        else
          GetParamValue(i).SetIsNull(True);
      end;
    National := TDAParam(Source).National;
    FEncryptor := TDAParam(Source).FEncryptor;
  end
  else
  if Source is TParam then
    AssignParam(TParam(Source))
  else
  if Source is TField then
    AssignField(TField(Source))
  else
  if Source is TStrings then
    AsMemo := TStrings(Source).Text
  else
  if Supports(Source, IStreamPersist, StreamPersist) then
    Load(StreamPersist)
  else
    inherited Assign(Source);
end;

procedure TDAParam.AssignParam(Param: TParam);
var
  ParamDataType: TFieldType;
begin
  if Param <> nil then begin
    ParamDataType := Param.DataType; // cache DataType value

    SetDataType(ParamDataType);
    if Param.IsNull then
      Clear
    else begin
      if IsBlobDataType(ParamDataType) and not (Param is TDAParam) then
        // in MIDAS we need to do such assignment
        // as TDAParam.Value = TParam.Value
        Value := Param.Value
      else
        inherited Value := Param.Value;
    end;
    Name := Param.Name;
    if ParamType = ptUnknown then
      ParamType := Param.ParamType;
  end;
end;

procedure TDAParam.AssignTo(Dest: TPersistent);
begin
  if Dest is TField then
    TField(Dest).Value := Value
  else
    inherited AssignTo(Dest);
end;

procedure TDAParam.CheckArrayType(DataType: TFieldType);
var
  Connection: TCustomDAConnection;
  MappedDataType: Integer;
begin
  Assert(Self.Collection <> nil);

  Connection := (Self.Collection as TDAParams).GetConnection;

  if Connection <> nil then begin
    MappedDataType := Connection.GetFieldTypeMapClass.GetDataType(DataType);
    if not Connection.GetICommandClass.IsAllowedArrayType(MappedDataType) then
      raise Exception.Create(SInvalidDmlArrayType);
  end;
end;

procedure TDAParam.FreeValues;
var
  i: integer;
  ValuePtr: PVarData;
begin
  if IsSharedObject and (FValueCount > 1) then
    for i := 0 to FValueCount - 1 do begin
      ValuePtr := @FDataArr[i];
      if (ValuePtr.VType = varSharedObject) and (ValuePtr.VPointer <> nil) then
        TSharedObject(ValuePtr.VPointer).Free;
      ValuePtr.VType := varEmpty;
      ValuePtr.VPointer := nil;
    end;
end;

function TDAParam.GetValueCount: integer;
begin
  Result := FValueCount;
end;

procedure TDAParam.SetValueCount(Value: integer);
var
  OldValue: variant;
begin
  if Value <= 0 then
    Value := 1;

  if Value <> FValueCount then begin
    if Value > 1 then begin
      CheckArrayType(DataType);

      if FValueCount > 1 then begin
        FreeValues;
        SetLength(FDataArr, Value);
        FValueCount := Value;
      end
      else begin
        OldValue := Self.Value;
        SetLength(FDataArr, Value);
        FreeObject;
        FValueCount := Value;
        Values[0].Value := OldValue;
      end;
    end
    else if FValueCount > 1 then begin
      OldValue := Values[0].Value;
      FreeValues;
      SetLength(FDataArr, 1);
      FDataArr[0] := Unassigned;
      FValueCount := Value;
      Self.Value := OldValue;
    end;
  end;
end;

function TDAParam.GetParamValueClass: TDAParamValueClass;
begin
  Result := TDAParamValue;
end;

function TDAParam.GetParamValue(Index: integer): TDAParamValue;
begin
  CheckIndex(Index);

  if FParamValue = nil then
    FParamValue := GetParamValueClass.Create(Self);

  FParamValue.FIndex := Index;
  Result := FParamValue;
end;

class function TDAParam.GetVarType(VarType: TVarType): TFieldType;
begin
  Result := ftUnknown;
  case VarType of
    varByte: Result := {$IFDEF VER12P}ftByte{$ELSE}ftSmallint{$ENDIF};
    varSmallint: Result := ftSmallInt;
    varShortInt: Result := {$IFDEF VER12P}ftShortInt{$ELSE}ftSmallint{$ENDIF};
    varWord: Result := ftWord;
    varLongWord: Result := {$IFDEF VER12P}ftLongWord{$ELSE}ftInteger{$ENDIF};
    varInteger: Result := ftInteger;
    varCurrency: Result := ftBCD;
    varSingle: Result := {$IFDEF VER14P}TFieldType.ftSingle{$ELSE}ftFloat{$ENDIF};
    varDouble: Result := ftFloat;
    varDate: Result := ftDateTime;
    varBoolean: Result := ftBoolean;
    varString: if Result <> ftFixedChar then Result := ftString;
  {$IFDEF VER12P}
    varUString, varOleStr: if Result <> ftFixedWideChar then Result := ftWideString;
  {$ENDIF}
    varInt64: Result := ftLargeInt;
  else
  {$IFNDEF FPC}
    if VarType = varSQLTimeStamp then
      Result := ftTimeStamp
    else
  {$ENDIF}
  {$IFDEF VER14P}
    if VarType = varSQLTimeStampOffset then
      Result := ftTimeStampOffset
    else
  {$ENDIF}
    if VarType = varFMTBcd then
      Result := ftFMTBcd;
  end;
end;

procedure TDAParam.AssignField(Field: TField);
begin
  if Field <> nil then begin
    AssignFieldValue(Field, Field.Value);
    if Field.DataSet is TCustomDADataSet then
      Name := TCustomDADataSet(Field.DataSet).GetSQLInfo.NormalizeName(Field.FieldName, TCustomDADataSet(Field.DataSet).Options.QuoteNames)
    else
      Name := Field.FieldName;
  end;
end;

procedure TDAParam.AssignFieldValue(Field: TField; const Value: Variant);
begin
  if Field <> nil then begin
    if (Field.DataType = ftString) and TStringField(Field).FixedChar then
      DataType := ftFixedChar
    else
    if (Field.DataType = ftWideString) and TStringField(Field).FixedChar then
      DataType := TFieldType(ftFixedWideChar)
    else
    if (Field.DataType = ftMemo) and (Field.Size > 255) then
      DataType := ftString
    else
  {$IFDEF VER10P}
    if (Field.DataType = ftWideMemo) and (Field.Size > 255) then
      DataType := ftWideString
    else
  {$ENDIF}
  {$IFDEF FPC}
    if (Field.DataType = ftWideMemo) and (Field.Size > 255) then
      DataType := ftWideString
    else
  {$ENDIF}
      DataType := Field.DataType;
    if VarIsNull(Value) then
      Clear
    else
      Self.Value := Value;
  end;
end;

procedure TDAParam.LoadFromFile(const FileName: string; BlobType: TBlobType);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream, BlobType);
  finally
    Stream.Free;
  end;
end;

procedure TDAParam.LoadFromStream(Stream: TStream; BlobType: TBlobType);
begin
  if not (BlobType in (BlobTypes + MemoTypes)) then
    raise Exception.Create(SDataTypeNotSupported);

  DataType := BlobType;
  Stream.Position := 0;
  Assert(ParamObject is TBlob, SDataTypeNotSupported);
  TBlob(ParamObject).LoadFromStream(Stream);
end;

procedure TDAParam.SetBlobData(Buffer: IntPtr; Size: Integer);
{$IFDEF VER12P}
var
  NewBuf: TBytes;
{$ENDIF}
{$IFDEF FPC}
var
  NewBuf: TBytes;
{$ENDIF}
begin
{$IFDEF VER12P}
  SetLength(NewBuf, Size);
  Move(Buffer^, Pointer(NewBuf)^, Size);
  AsBlob := NewBuf;
{$ELSE}{$IFDEF FPC}
  SetLength(NewBuf, Size);
  Move(Buffer^, Pointer(NewBuf)^, Size);
  AsBlob := NewBuf;
{$ELSE}
  AsBlob := Encoding.Default.GetString(TBytes(Buffer), 0, Size);
{$ENDIF}{$ENDIF}
end;

procedure TDAParam.SetBlobData(Buffer: TValueBuffer);
begin
  DataType := ftBlob;
{$IFNDEF NEXTGEN}
  AsAnsiString := Marshal.PtrToStringAnsi(Buffer);
{$ELSE}
  AsWideString := string(Marshal.PtrToStringAnsi(Buffer));
{$ENDIF}
end;

function TDAParam.CreateObject: TSharedObject;
begin
  if DataType in [ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}, ftGraphic, ftFmtMemo] then begin
    Result := TBlob.Create;

    // if False do nothing (old behavior)
    if NeedBlobUnicode then
      TBlob(Result).IsUnicode := True;
  end
  else
    Result := nil;
end;

procedure TDAParam.FreeObject;
begin
  if FParamObject <> nil then begin
    FParamObject.Free;
    FParamObject := nil;
  end;
end;

function TDAParam.NeedBlobUnicode: boolean;
begin
{$IFDEF VER10P}
  if DataType = ftWideMemo then
    Result := True
  else
{$ENDIF}
{$IFDEF FPC}
  if DataType = ftWideMemo then
    Result := True
  else
{$ENDIF}
    Result := False;
end;

function TDAParam.GetNativeParamObject: TSharedObject;
begin
  Result := GetAsBlobRef;
end;

function TDAParam.GetNativeParamObject(SourceObject: TSharedObject): TSharedObject;
begin
  Result := SourceObject;
end;

function TDAParam.GetParamObject: TSharedObject;
begin
  if (FParamObject = nil) and IsSharedObject then
    FParamObject := CreateObject;

  Result := FParamObject;
end;

procedure TDAParam.SetParamObject(Value: TSharedObject);
begin
  if FParamObject = Value then
    Exit;

  FreeObject;

  if Value <> nil then begin
    FParamObject := Value;
    FParamObject.AddRef;
    inherited Value := 'Object'; // for IsNull = False
  end;
end;

function TDAParam.IsDataTypeStored: boolean;
begin
  Result := Integer(DataType) <= Integer(High(TFieldType));
end;

function TDAParam.IsValueStored: boolean;
begin
  Result := {$IFNDEF FPC}Bound and not VarIsArray(Value){$ELSE}False{$ENDIF};
end;

procedure TDAParam.CheckIndex(Index: integer);
begin
  if (Index < 0) or (Index >= FValueCount) then
    raise ERangeError.Create(SInvalidIndex);
end;

procedure TDAParam.CheckGetValue;
begin
  if FValueCount > 1 then
    raise Exception.Create(SCannotGetSingleValue);
end;

procedure TDAParam.CheckSetValue;
begin
  if FValueCount > 1 then
    SetValueCount(1);
end;

procedure TDAParam.DefineProperties(Filer: TFiler);

  function WriteData: boolean;
  begin
    Result := not IsDataTypeStored;
  end;

begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('ExtDataType', ReadExtDataType, WriteExtDataType, WriteData);
end;

procedure TDAParam.ReadExtDataType(Reader: TReader);
begin
  DataType := TFieldType(Reader.ReadInteger);
end;

procedure TDAParam.WriteExtDataType(Writer: TWriter);
begin
  Writer.WriteInteger(Integer(DataType));
end;

function TDAParam.IsSharedObjectDataType(DataType: TFieldType): boolean;
begin
  Result := DataType in [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}, ftBlob, ftGraphic, ftFmtMemo];
end;

function TDAParam.IsSharedObject: boolean;
begin
  Result := IsSharedObjectDataType(DataType);
end;

function TDAParam.IsBlobDataType(DataType: TFieldType): boolean;
begin
  Result := DataType in [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}, ftBlob, ftGraphic, ftFmtMemo];
end;

function TDAParam.IsBlob: boolean;
begin
  Result := IsBlobDataType(DataType);
end;

function TDAParam.GetDataType: TFieldType;
begin
  Result := inherited DataType;
end;

procedure TDAParam.SetDataType(Value: TFieldType);
begin
  if Value <> inherited DataType then begin
    if FValueCount > 1 then begin
      CheckArrayType(Value);
      if IsSharedObject then
        FreeValues;
    end;

    if IsSharedObject then
      FreeObject;

    inherited DataType := Value;

    SubDataType := dtUnknown;

    if IsSharedObject and (FValueCount <= 1) then
      FParamObject := CreateObject;
  end;
end;

function TDAParam.GetSize: integer;
begin
  Result := FSize;
end;

procedure TDAParam.SetSize(Value: integer);
begin
  FSize := Value;
end;

function TDAParam.GetAsString: string;
begin
{$IFDEF VER12P}
  Result := AsWideString;
{$ELSE}
  Result := AsAnsiString;
{$ENDIF}
end;

procedure TDAParam.SetAsString(const Value: string);
begin
{$IFDEF VER12P}
  {$IFNDEF NEXTGEN}
  if DataType in [ftString, ftFixedChar] then
    AsAnsiString := AnsiString(Value)
  else if ParamStringAsAnsiString then
    AsAnsiString := AnsiString(Value)
  else
  {$ENDIF}
    AsWideString := Value;
{$ELSE}
  AsAnsiString := Value;
{$ENDIF}
end;

{$IFNDEF NEXTGEN}

function TDAParam.GetAsAnsiString: AnsiString;
var
  ParamDataType: TFieldType;
begin
  CheckGetValue;
  if IsNull then
    Result := ''
  else begin
    ParamDataType := DataType;
    if IsBlobDataType(ParamDataType) then
      Result := TBlob(ParamObject).AsAnsiString
    else
      case ParamDataType of
        ftDate:
          Result := AnsiString(DateToStr(TVarData(Value).VDate));
        ftTime:
          Result := AnsiString(TimeToStr(TVarData(Value).VDate));
        ftDateTime:
          Result := AnsiString(DateTimeToStr(TVarData(Value).VDate));
        else
        {$IFDEF VER12P}
          Result := inherited AsAnsiString;
        {$ELSE}
          Result := inherited AsString;
        {$ENDIF};
      end;
  end;
end;

procedure TDAParam.SetAsAnsiString(const Value: AnsiString);
var
  ParamDataType: TFieldType;
begin
  CheckSetValue;
  ParamDataType := GetDataType;
  if IsBlobDataType(ParamDataType) then
    TBlob(ParamObject).AsAnsiString := Value
  else if Integer(ParamDataType) in [Integer(ftWideString), Integer(ftFixedWideChar)] then
    AsWideString := WideString(Value)
  else
  {$IFDEF VER12P}
    {$IFDEF IS_UTF8}
    // workaround for Linux: code page for AnsiString is unknown
    inherited AsWideString := WideString(Value);
    {$ELSE}
    inherited AsAnsiString := Value;
    {$ENDIF}
  {$ELSE}
    inherited AsString := Value;
  {$ENDIF}
end;

{$ENDIF}

function TDAParam.GetAsWideString: WideString;
var
  ParamDataType: TFieldType;
begin
  CheckGetValue;
  if IsNull then
    Result := ''
  else begin
    ParamDataType := GetDataType;
    if IsBlobDataType(ParamDataType) then
      Result := TBlob(ParamObject).AsWideString
    else
      case ParamDataType of
        ftDate:
          Result := WideString(DateToStr(TVarData(Value).VDate));
        ftTime:
          Result := WideString(TimeToStr(TVarData(Value).VDate));
        ftDateTime:
          Result := WideString(DateTimeToStr(TVarData(Value).VDate));
        else
          Result := inherited {$IFDEF VER12P}AsWideString{$ELSE}Value{$ENDIF};
      end;
  end;
end;

procedure TDAParam.SetAsWideString(const Value: WideString);
var
  ParamDataType: TFieldType;
begin
  CheckSetValue;
  ParamDataType := GetDataType;
  if IsBlobDataType(ParamDataType) then
    TBlob(ParamObject).AsWideString := Value
  else begin
  {$IFDEF VER12P}
    if ParamDataType = ftFixedChar then
      SetDataType(ftFixedWideChar);

    inherited AsWideString := Value;
  {$ELSE}{$IFDEF FPC}
    inherited AsWideString := Value;
  {$ELSE}
    inherited Value := Value;
  {$ENDIF}{$ENDIF}
  end;
end;

function TDAParam.GetAsBytes: TBytes;
var
  ParamDataType: TFieldType;
begin
  CheckGetValue;
  if IsNull then
    Result := nil
  else begin
    ParamDataType := GetDataType;
    if IsBlobDataType(ParamDataType) then
      Result := TBlob(ParamObject).AsBytes
    else begin
    {$IFDEF VER12P}
      Result := inherited AsBytes;
    {$ELSE}{$IFDEF FPC}
      Result := inherited AsBytes;
    {$ELSE}
      Result := inherited Value;
    {$ENDIF}{$ENDIF}
    end;
  end;
end;

procedure TDAParam.SetAsBytes(const Value: TBytes);
var
  ParamDataType: TFieldType;
begin
  CheckSetValue;
  ParamDataType := GetDataType;
  if IsBlobDataType(ParamDataType) then
    TBlob(ParamObject).AsBytes := Value
  else begin
  {$IFDEF VER12P}
    inherited AsBytes := Value;
  {$ELSE}{$IFDEF FPC}
    inherited AsBytes := Value;
  {$ELSE}
    if not (ParamDataType in [ftBytes, ftVarBytes]) then
      SetDataType(ftVarBytes);
    inherited Value := Value;
  {$ENDIF}{$ENDIF}
  end;
end;

function TDAParam.GetAsInteger: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF};
begin
  CheckGetValue;
  Result := inherited AsInteger;
end;

procedure TDAParam.SetAsInteger(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckSetValue;
  inherited AsInteger := Value
end;

procedure TDAParam.SetAsSmallInt(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckSetValue;
  inherited AsSmallInt := Value;
end;

procedure TDAParam.SetAsWord(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckSetValue;
  inherited AsWord := Value;
end;

function TDAParam.GetAsFloat: Double;
begin
  CheckGetValue;
  Result := inherited AsFloat;
end;

procedure TDAParam.SetAsFloat(Value: Double);
begin
  CheckSetValue;
  inherited AsFloat := Value;
end;

function TDAParam.GetAsLargeInt: Int64;
begin
  CheckGetValue;
{$IFNDEF VER12P}
  if IsNull then
    Result := 0
  else
    Result := Value;
{$ELSE}
  Result := inherited AsLargeInt;
{$ENDIF}
end;

procedure TDAParam.SetAsLargeInt(const Value: Int64);
begin
  CheckSetValue;
{$IFNDEF VER12P}
  DataType := ftLargeInt;
  Self.Value := Value;
{$ELSE}
  inherited AsLargeInt := Value;
{$ENDIF}
end;

{$IFDEF VER12P}
procedure TDAParam.SetAsShortInt(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckSetValue;
  inherited AsShortInt := Value;
end;

procedure TDAParam.SetAsByte(Value: {$IFDEF VER26P}Integer{$ELSE}LongInt{$ENDIF});
begin
  CheckSetValue;
  inherited AsByte := Value;
end;
{$ENDIF}

procedure TDAParam.SetAsBlob(const Value: TBlobData);
begin
  CheckSetValue;
  DataType := ftBlob;
{$IFDEF VER12P}
  TBlob(ParamObject).AsBytes := Value;
{$ELSE}{$IFDEF FPC}
  TBlob(ParamObject).AsBytes := Value;
{$ELSE}
  TBlob(ParamObject).AsString := Value;
{$ENDIF}{$ENDIF}
end;

procedure TDAParam.SetAsMemo(const Value: string);
begin
  CheckSetValue;
  DataType := ftMemo;
  TBlob(ParamObject).AsString := Value
end;

function TDAParam.GetAsBlobRef: TBlob;
begin
  CheckGetValue;

  if DataType = ftUnknown then
    DataType := ftBlob;

  if IsBlob then
    Result := ParamObject as TBlob
  else
    Result := nil;
end;

procedure TDAParam.SetAsBlobRef(const Value: TBlob);
begin
  CheckSetValue;
  inherited DataType := ftBlob;
  ParamObject := Value;
end;

function TDAParam.GetAsMemoRef: TBlob;
begin
  CheckGetValue;
  if DataType = ftUnknown then
    DataType := ftMemo;

  if IsBlob then
    Result := ParamObject as TBlob
  else
    Result := nil;
end;

procedure TDAParam.SetAsMemoRef(const Value: TBlob);
begin
  CheckSetValue;
{$IFDEF VER10P}
  if Value.IsUnicode then
    inherited DataType := ftWideMemo
  else
{$ENDIF}
{$IFDEF FPC}
  if Value.IsUnicode then
    inherited DataType := ftWideMemo
  else
{$ENDIF}
    inherited DataType := ftMemo;

  ParamObject := Value;
end;

function TDAParam.GetAsVariant: Variant;
var
  Obj: TSharedObject;
begin
  if FValueCount > 1 then
    Result := FDataArr
  else if IsBlob then begin
    Obj := ParamObject;
    if TBlob(Obj).IsNull then
      Result := Null
    else
      Result := TBlob(Obj).AsString;
  end
  else
    Result := inherited Value;
end;

procedure TDAParam.SetAsVariant(const Value: Variant);
var
  i: integer;
  LowIndex, HighIndex: integer;
  SafeArray: PVarArray;
  AllNulls: boolean;
begin
  if VarIsArray(Value) and (VarType(Value) <> varArray or varByte) then begin
    if VarType(Value) <> varByRef or varVariant then begin
      LowIndex := VarArrayLowBound(Value, 1);
      HighIndex := VarArrayHighBound(Value, 1);
      ValueCount := HighIndex - LowIndex + 1;
      if VarType(Value) = varArray or varVariant then
        SetDataType(ftUnknown)
      else begin
        AllNulls := True;
        for i := LowIndex to HighIndex do
          if not (TVarData(Value[i]).VType in [varEmpty, varNull]) then begin
            SetDataType(GetVarType(VarType(Value[i])));
            AllNulls := False;
            Break;
          end;
        if AllNulls then
          SetDataType(ftUnknown);
      end;
      for i := LowIndex to HighIndex do
        GetParamValue(i - LowIndex).Value := Value[i];
    end
    else
      Assert(TVarData(Value).VPointer = @FDataArr);
  end
  else begin
    CheckSetValue;

    if IsBlob then begin
      if VarType(Value) = varNull then
        TBlob(ParamObject).Clear
      else if VarType(Value) = varArray or varByte then begin
        TBlob(ParamObject).Clear;
        SafeArray := VarArrayAsPSafeArray(Value);
        i := SafeArray.Bounds[0].ElementCount;
        if i > 0 then
          TBlob(ParamObject).Write(0, i, SafeArray.Data);
      end
      else
        TBlob(ParamObject).AsString := VarToStr(Value);
    end
    else
      inherited Value := Value;
  end;
  Bound := True;
end;

{$IFNDEF FPC}

function TDAParam.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  CheckGetValue;
  Result := inherited AsSQLTimeStamp;
end;

procedure TDAParam.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  CheckSetValue;
  inherited AsSQLTimeStamp := Value;
end;

{$ENDIF}

function TDAParam.GetAsCursor: TCRCursor;
begin
  CheckGetValue;

  if DataType = ftUnknown then
    DataType := ftCursor;

  if DataType = ftCursor then
    Result := ParamObject as TCRCursor
  else
    Result := nil;
end;

procedure TDAParam.SetAsCursor(Value: TCRCursor);
begin
  CheckSetValue;
  inherited DataType := ftCursor;
  ParamObject := Value;
end;

procedure TDAParam.SetText(const Value: string);
begin
  if IsBlob then begin
    Assert(ParamObject is TBlob);
    TBlob(ParamObject).AsString := Value;
  end
  else
    inherited SetText(Value);
end;

function TDAParam.GetIsNull: boolean;
begin
  if FValueCount > 1 then
    Result := False
  else if IsBlob then begin
    Assert(ParamObject is TBlob);
    Result := TBlob(ParamObject).IsNull;
  end
  else
    Result := inherited GetIsNull;
end;

procedure TDAParam.SetIsNull(Value: boolean);
begin
  if FValueCount > 1 then begin
    if Value then
      SetValueCount(1);
  end
  else if Value then
    inherited Value := Null
  else
    inherited Value := '';
end;

procedure TDAParam.SetNational(Value: Boolean);
begin
  FNational := Value;
end;

{ TDAParams }

constructor TDAParams.Create(Owner: TPersistent);
begin
  inherited Create(TDAParam);

  FOwner := Owner;
  FNeedsUpdateItem := True;
  FParamsChangeType := ctGenerated;
end;

function TDAParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TDAParams.GetConnection: TCustomDAConnection;
begin
  Result := nil;

  if Owner is TCustomDASQL then
    Result := TCustomDASQL(Owner).UsedConnection
  else if Owner is TCustomDADataSet then
    Result := TCustomDADataSet(Owner).UsedConnection
  else if Owner <> nil then
    Assert(False, Owner.ClassName);
end;

procedure TDAParams.Update(Item: TCollectionItem);
begin
  if FParamsChangeType = ctUserChecked then
    FParamsChangeType := ctUsers;

  if FNeedsUpdateItem then
    inherited;
end;

procedure TDAParams.Disconnect;
var
  i, j: integer;
  Param: TDAParam;
  ParamObject: TSharedObject;
begin
  for i := 0 to Count - 1 do begin
    Param := Items[i];
    if Param.IsSharedObject then
      if Param.ValueCount > 1 then begin
        for j := 0 to ValueCount - 1 do begin
          ParamObject := TSharedObject.FromVariant(Param.FDataArr[j]);
          if ParamObject <> nil then
            ParamObject.Disconnect;
        end;
      end
      else begin
        ParamObject := Param.FParamObject;
        if ParamObject <> nil then
          ParamObject.Disconnect;
      end;
  end;
end;

function TDAParams.GetItem(Index: integer): TDAParam;
begin
  Result := TDAParam(inherited Items[Index]);
end;

procedure TDAParams.SetItem(Index: integer; Value: TDAParam);
begin
  inherited Items[Index] := Value;
end;

function TDAParams.GetValueCount: integer;
begin
  if Count = 0 then
    Result := 1
  else
    Result := Items[0].ValueCount;
end;

procedure TDAParams.SetValueCount(Value: integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].ValueCount := Value;
end;

function TDAParams.ParamByName(const Value: string): TDAParam;
begin
  Result := TDAParam(inherited ParamByName(Value));
end;

function TDAParams.FindParam(const Value: string): TDAParam;
begin
  Result := TDAParam(inherited FindParam(Value));
end;

function TDAParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TParamType): TDAParam;
begin
  Result := TDAParam(inherited CreateParam(ftUnknown, ParamName, ParamType));
  Result.DataType := FldType;
{$IFDEF FPC}
  Result.ParamType := ParamType;
{$ENDIF}
end;

{ TDACursorField }

constructor TDACursorField.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  SetDataType(ftCursor);
end;

function TDACursorField.GetAsCursor: TCRCursor;
begin
  if not GetValue(Result) then
    Result := nil;
end;

function TDACursorField.GetValue(out Value: TCRCursor): boolean;
var
  Handle: TValueBuffer;
begin
  Handle := AllocValueBuffer(sizeof(IntPtr));
  Marshal.WriteIntPtr(Handle, nil);
  try
    Result := GetData(Handle);
    if Result then
      Value := TCRCursor(GetGCHandleTarget(Marshal.ReadIntPtr(Handle)));
  finally
    FreeValueBuffer(Handle);
  end;
end;

{ TCustomDADataSet }

constructor TCustomDADataSet.Create(Owner: TComponent);
var
  stIdx: TStatementType;
begin
  inherited Create(Owner);

  for stIdx := Low(TStatementType) to High(TStatementType) do
    if stIdx in GetUpdateSQLStatementTypes then begin
      Assert(FUpdateSQL[stIdx] = nil);
      FUpdateSQL[stIdx] := TStringList.Create;
      TStringList(FUpdateSQL[stIdx]).OnChange := ScanMacros;
    end;

  FFetchRows := 25;
  FAutoCommit := True;
  FRowsAffected := -1;
  FParamsProcessed := 0;
  FRefreshOptions := [];

  CreateCommand;

  FDesignCreate := csDesigning in ComponentState;
  FOptions := CreateOptions;
  FSmartFetchOptions := TSmartFetchOptions.Create(Self);
  FDataTypeMap := CreateDataTypeMap;
  FEncryption := CreateEncryption;
  FCheckMode := cmException;
  FWhereConditions := TDAConditions.Create(Self);
end;

destructor TCustomDADataSet.Destroy;
var
  stIdx: TStatementType;
begin
  try
    Close;
    UnPrepare;
  finally
    if UsedConnection <> nil then
      UsedConnection.UnRegisterClient(Self);

    FreeCommand;

    FOptions.Free;
    FSmartFetchOptions.Free;
    FDataTypeMap.Free;
    FEncryption.Free;
    FEncryption := nil;
    FWhereConditions.Free;

    for stIdx := Low(FUpdateSQL) to High(FUpdateSQL) do begin
      FUpdateSQL[stIdx].Free;
      FUpdateSQL[stIdx] := nil;
    end;

    if UsedConnection <> nil then
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).ObjectDestroyed(Self);

    inherited;

    SetUpdateObject(nil);
  end;
end;

function TCustomDADataSet.IsMapRulesStored: boolean;
begin
  Result := FDataTypeMap.Count > 0;
end;

procedure TCustomDADataSet.CheckActive;
begin
  inherited CheckActive;
end;

procedure TCustomDADataSet.CheckInactive;
begin
  inherited CheckInactive;
end;

procedure TCustomDADataSet.CreateIRecordSet;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then
    SetIRecordSet(vConnection.CreateIRecordSet)
  else
    SetIRecordSet(nil);
end;

procedure TCustomDADataSet.FreeIRecordSet;
begin
  FIRecordSet.Free;
  SetIRecordSet(nil);
end;

procedure TCustomDADataSet.SetIRecordSet(Value: TData);
begin
  inherited;

  FIRecordSet := TCRRecordSet(Value); // Value as TCRRecordSet;
  if FIRecordSet <> nil then begin
    if FConnection <> nil then
      FIRecordSet.SetConnection(FConnection.FIConnection)
    else
      FIRecordSet.SetConnection(nil);

    if FTransaction <> nil then
      FIRecordSet.SetTransaction(FTransaction.FITransaction)
    else
      FIRecordSet.SetTransaction(nil);

    FICommand := FIRecordSet.GetCommand;
    FIRecordSet.SetProp(prUniDirectional, FUniDirectional);
    FIRecordSet.SetProp(prFetchRows, FFetchRows);
    FIRecordSet.SetProp(prFetchAll, FFetchAll);
    FIRecordSet.SetProp(prReadOnly, FReadOnly);
    FIRecordSet.SetProp(prRoAfterUpdate, roAfterUpdate in RefreshOptions);
    FIRecordSet.SetProp(prKeyFields, FKeyFields);
    FIRecordSet.SetProp(prUpdatingTable, FUpdatingTable);

    if FOptions <> nil then begin
      FIRecordSet.SetProp(prLongStrings, FOptions.FLongStrings);
      FIRecordSet.SetProp(prFlatBuffers, FOptions.FFlatBuffers);
      FIRecordSet.TrimFixedChar := FOptions.TrimFixedChar;
      FIRecordSet.TrimVarChar := FOptions.TrimVarChar;
      FIRecordSet.SetEmptyStrToNull := FOptions.SetEmptyStrToNull;

      FICommand.SetProp(prQuoteNames, FOptions.QuoteNames);
    {$IFDEF HAVE_COMPRESS}
      FIRecordSet.SetProp(prCompressBlobMode, Integer(FOptions.CompressBlobMode));
    {$ENDIF}
      FIRecordSet.SetProp(prDefaultValues, Options.DefaultValues);
      FIRecordSet.SetProp(prFieldOrigins, Options.FieldOrigins);
      FIRecordSet.SetProp(prFullRefresh, Options.FullRefresh);
      FIRecordSet.SetProp(prSetFieldsReadOnly, Options.SetFieldsReadOnly);
      FIRecordSet.SetProp(prExtendedFieldsInfo, Options.ExtendedFieldsInfo);
      FIRecordSet.SetProp(prInsertAllSetFields, Options.InsertAllSetFields);
      FICommand.SetProp(prEnableBCD, Options.EnableBCD);
      FICommand.SetProp(prEnableFmtBCD, Options.EnableFMTBCD);
    end;

    if FEncryption <> nil then begin
      FIRecordSet.SetProp(prEncryptedFields, FEncryption.Fields);
      FIRecordSet.Encryptor := FEncryption.Encryptor;
    end;

    if FSmartFetchOptions <> nil then begin
      if FSmartFetchOptions.Enabled then
        FIRecordSet.SmartFetchState := sfMetaInfo
      else
        FIRecordSet.SmartFetchState := sfNone;
      FIRecordSet.SetProp(prPrefetchedFields, FSmartFetchOptions.PrefetchedFields);
      FIRecordSet.SetProp(prLiveBlockOnSmartFetch, FSmartFetchOptions.LiveBlock);
    end;

    FIRecordSet.AfterExecFetch := DoAfterExecFetch;
    FIRecordSet.AfterFetchAll := DoAfterFetchAll;
    FIRecordSet.OnBeforeFetch := DoOnBeforeFetch;
    FIRecordSet.OnAfterFetch := DoOnAfterFetch;
    FIRecordSet.OnDataChanged := DoOnDataChanged;
    FIRecordSet.OnReopen := DoOnReopen;
    FIRecordSet.OnFieldsChanged := DoOnFieldsChanged;
    FIRecordSet.GetKeyValuesSQL := GetKeyValuesSQL;
///    FIRecordSet.GetDataValuesSQL := GetDataValuesSQL;
    FIRecordSet.GetDBKeyList := GetDBKeyList;
    FIRecordSet.FillExtFieldsInfo := FillExtFieldsInfo;

    FIRecordSet.OnConnectRequest := ConnectRequest;
    FIRecordSet.OnDisconnectRequest := DisconnectRequest;
    FIRecordSet.Component := Self;

    if FDataSetService <> nil then begin
      FIRecordSet.SQLGenerator := FDataSetService.SQLGenerator;
      FDataSetService.SQLGenerator.IRecordSet := FIRecordSet;
    end;
  end
  else
    FICommand := nil;

  if FCommand <> nil then
    FCommand.SetICommand(FICommand);

  if FICommand <> nil then begin
    FICommand.SetProp(prAutoCommit, FAutoCommit);
    FICommand.AfterExecute := DoAfterExecute;
  end;
end;

procedure TCustomDADataSet.CheckIRecordSet;
var
  ClassType: TClass;
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if (vConnection <> nil) then
    ClassType := vConnection.GetIRecordSetClass
  else
    ClassType := nil;

  if ((ClassType <> nil) and not IsClass(FIRecordSet, ClassType)) or (FIRecordSet = nil) then begin
    FreeIRecordSet;
    CreateIRecordSet;
  end;
end;

procedure TCustomDADataSet.CreateCommand;
begin
  SetCommand(TCustomDASQL.Create(Self));
end;

procedure TCustomDADataSet.FreeCommand;
begin
  FCommand.Free;
  FCommand := nil;
end;

procedure TCustomDADataSet.SetCommand(Value: TCustomDASQL);
begin
  //FreeCommand;

  FCommand := Value;
  if FCommand <> nil then begin
    FCommand.FDataSet := Self;
    FCommand.SetICommand(FICommand);

    FParams := FCommand.Params;
    FMacros := FCommand.Macros;
  end;
end;

function TCustomDADataSet.CreateDataTypeMap: TDADataSetMapRules;
begin
  Result := TDADataSetMapRules.Create(Self);
end;

procedure TCustomDADataSet.SetDataTypeMap(Value: TDAMapRules);
begin
  Assert(FDataTypeMap <> nil);
  FDataTypeMap.Assign(Value);
end;

procedure TCustomDADataSet.SetDataSetService(Value: TDataSetService);
begin
  inherited;

  FDataSetService := TDADataSetService(Value);
end;

function TCustomDADataSet.CreateOptions: TDADataSetOptions;
begin
  Result := TDADataSetOptions.Create(Self);
end;

function TCustomDADataSet.CreateEncryption: TDAEncryption;
begin
  Result := TDAEncryption.Create(self);
end;

procedure TCustomDADataSet.Loaded;
begin
  FStreamedOpen := True;
  try
    inherited;
    FDesignCreate := False;
  finally
    FStreamedOpen := False;
  end;
end;

function TCustomDADataSet.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection;
end;

procedure TCustomDADataSet.CheckConnection;
begin
  BeginConnection(False);
end;

procedure TCustomDADataSet.BeginConnection(NoConnectCheck: boolean = True);

  function HasDataSet(DAConnection: TCustomDAConnection; DataSet: TDataSet): boolean;
  var
    i: integer;
  begin
    for i := 0 to DAConnection.DataSetCount - 1 do begin
      if DAConnection.DataSets[i] = DataSet then begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;

var
  UseDefaultConnection: boolean;
  vConnection: TCustomDAConnection;
  vTransaction: TDATransaction;
begin
  vConnection := UsedConnection;
  if vConnection = nil then
    DatabaseError(SConnectionNotDefined);

  if not vConnection.Connected and not vConnection.Options.AllowImplicitConnect then
    raise Exception.Create(SConnectionNotConnected);

  vConnection.hRegisterClient.Enter;
  try
    if vConnection.FInProcessDisconnecting then
      raise Exception.Create(SConnectionNotConnected);

    if NoConnectCheck then
      vConnection.InternalConnect // We should call connect each time to update ConnectCount
    else
      if not vConnection.Connected then
        vConnection.Connect;

    UseDefaultConnection := (FConnection = nil) and not HasDataSet(vConnection, Self);

    CheckIRecordSet;
    CheckDataSetService;

    if vConnection.IsMultipleTransactionsSupported then begin
      vTransaction := UsedTransaction;
      if vTransaction = nil then
        DatabaseError(STransactionNotAssigned);

      if NoConnectCheck then
        vTransaction.GainTransaction // We should call each time to update TrStartCount
      else
       if not vTransaction.Active then
         vTransaction.StartTransaction;

      FIRecordSet.SetTransaction(vTransaction.FITransaction);
    end;

    // use default session
    if UseDefaultConnection then begin
      vConnection.RegisterClient(Self, ConnectChange);

      FIRecordSet.SetConnection(vConnection.FIConnection)
    end;
  finally
    vConnection.hRegisterClient.Release;
  end;
end;

procedure TCustomDADataSet.EndConnection;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;

  if vConnection <> nil then begin
    if vConnection.IsMultipleTransactionsSupported then
      UsedTransaction.ReleaseTransaction; // Release and Stop transaction

    vConnection.InternalDisconnect;
  end;
end;

procedure TCustomDADataSet.ConnectRequest;
begin
  BeginConnection;
end;

procedure TCustomDADataSet.DisconnectRequest;
begin
  EndConnection;
end;

procedure TCustomDADataSet.Disconnect(NeedClose: boolean = True);
var
  vConnection: TCustomDAConnection;
begin
  if NeedClose then begin
    Close;
    UnPrepare;
    FieldDefs.Updated := False;
  end
  else
    if FIRecordSet <> nil then
      FIRecordSet.Disconnect;

  Params.Disconnect;

  vConnection := UsedConnection;
  if vConnection <> nil then
    if not vConnection.Options.DisconnectedMode then
      FCommand.FIsSPInit := False;
end;

procedure TCustomDADataSet.ConnectChange(Sender: TObject; Connecting: boolean);
var
  vConnection: TCustomDAConnection;
begin
  if not Connecting then
    Disconnect(not (TCustomDAConnection(Sender).Options.DisconnectedMode or FDisconnected))
  else if not (Sender is TCustomDAConnection) and (FIRecordSet <> nil) then begin // Dll call
    vConnection := UsedConnection;
    Assert(vConnection <> nil);
    Assert(vConnection.FIConnection <> nil);
    FIRecordSet.SetConnection(vConnection.FIConnection);
  end;
end;

function TCustomDADataSet.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

function TCustomDADataSet.GetUsedTransaction: TDATransaction;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then begin
    if vConnection.IsMultipleTransactionsSupported then
      Result := Transaction
    else
      Result := nil;

    if Result = nil then
      Result := vConnection.UsedTransaction;
  end
  else
    Result := nil;
end;

function TCustomDADataSet.UsedTransaction: TDATransaction;
begin
  Result := GetUsedTransaction;
end;

function TCustomDADataSet.UsedUpdateTransaction: TDATransaction;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then begin
    if (FUpdateTransaction <> nil) and vConnection.IsMultipleTransactionsSupported then
      Result := FUpdateTransaction
    else
      Result := GetUsedTransaction;
  end
  else
    Result := nil;
end;

procedure TCustomDADataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FTransaction then
      FTransaction := nil
    else
    if AComponent = FUpdateTransaction then
      FUpdateTransaction := nil
    else
    if (FEncryption <> nil) and (AComponent = FEncryption.Encryptor) then
      FEncryption.Encryptor := nil;
end;

procedure TCustomDADataSet.SetKeyFields(const Value: string);
begin
  if FKeyFields <> Value then begin
    FKeyFields := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prKeyFields, Value);
  end;
end;

function TCustomDADataSet.GetFieldTypeMapClass: TFieldTypeMapClass;
begin
  Result := FCommand.GetFieldTypeMapClass;
end;

function TCustomDADataSet.GetFieldType(FieldDesc: TFieldDesc; out FieldSize, FieldLength, FieldScale: Integer): TFieldType;

  procedure UnsupportedDataTypeMapping(FieldDesc: TCRFieldDesc);
  var
    MapRule: TCRMapRule;
    FieldType: TFieldType;
    MapFieldType: TFieldType;
    DBTypeInfo: TDBTypeInfo;
    DBTypeName: string;
    LenStr: string;
    FieldTypeInfo: TFieldTypeInfo;
    FieldTypeName: string;
  begin
    MapRule := FieldDesc.MapRule;
    FieldType := GetFieldType(FieldDesc.DataType);
    MapFieldType := GetFieldType(MapRule.DataType);

    DBTypeInfo := DBTypeInfos.FindTypeInfo(FieldDesc.DBType);
    if DBTypeInfo <> nil then begin
      DBTypeName := DBTypeInfo.Name;
      LenStr := '';
      if FieldDesc.Length > 0 then
        LenStr := IntToStr(FieldDesc.Length);
      if FieldDesc.Scale > 0 then begin
        if LenStr <>'' then
          LenStr := LenStr + ',';
        LenStr := LenStr + IntToStr(FieldDesc.Scale);
      end;
      if LenStr <> '' then
        DBTypeName := DBTypeName + '(' + LenStr + ')';
    end
    else
      DBTypeName := 'Unknown(' + IntToStr(FieldDesc.DBType) + ')';

    FieldTypeInfo := TDAFieldTypeMapClass(GetFieldTypeMapClass).GetFieldTypeInfos.FindTypeInfo(MapFieldType);
    if FieldTypeInfo <> nil then begin
      FieldTypeName := FieldTypeInfo.Name;
      LenStr := '';
      if MapRule.FieldLength > 0 then
        LenStr := IntToStr(MapRule.FieldLength);
      if MapRule.FieldScale > 0 then begin
        if LenStr <>'' then
          LenStr := LenStr + ',';
        LenStr := LenStr + IntToStr(MapRule.FieldScale);
      end;
      if LenStr <> '' then
        FieldTypeName := FieldTypeName + '(' + LenStr + ')';
    end
    else
      FieldTypeName := 'Unknown(' + IntToStr(Integer(FieldType)) + ')';

    raise EUnsupportedDataTypeMapping.CreateFmt(SUnsupportedMapping, [DBTypeName, FieldTypeName]);
  end;

begin
  Assert(IsClass(FieldDesc, TCRFieldDesc));

  if TCRFieldDesc(FieldDesc).MapRule <> nil then begin
    if TCRFieldDesc(FieldDesc).OnDemandConverter <> nil then begin
      Result := GetFieldType(TCRFieldDesc(FieldDesc).OnDemandConverter.DestDataType);
      FieldSize := GetFieldDefSize(Result, TCRFieldDesc(FieldDesc).MapLength);
      FieldLength := TCRFieldDesc(FieldDesc).MapLength;
      FieldScale := TCRFieldDesc(FieldDesc).MapScale;
      Exit;
    end
    else if not DataTypeMap.IgnoreInvalidRules then begin
      Result := ftUnknown;
      FieldSize := 0;
      FieldLength := 0;
      FieldScale := 0;
      UnsupportedDataTypeMapping(TCRFieldDesc(FieldDesc));
      Exit;
    end;
  end;

  Result := GetFieldType(FieldDesc.DataType);
  FieldSize := GetFieldDefSize(Result, FieldDesc.Length);
  FieldLength := FieldDesc.Length;
  FieldScale := FieldDesc.Scale;
end;

procedure TCustomDADataSet.FillExtFieldsInfo;
begin
  if FDataSetService <> nil then
    FDataSetService.PreInitCursor;
end;

{ TablesInfo }

function TCustomDADataSet.GetTablesInfo: TCRTablesInfo;
begin
  if FIRecordSet <> nil then
    Result := FIRecordSet.TablesInfo
  else
    Result := nil;
end;

function TCustomDADataSet.GetSQLInfo: TSQLInfo;
begin
  if FICommand <> nil then
    Result := FICommand.SQLInfo
  else
    Result := DefaultSQLInfo;
end;

function TCustomDADataSet.QuoteName(const AName: string): string;
begin
  if FOptions.FQuoteNames or SQLInfo.QuotesNeeded(AName) then
    Result := SQLInfo.Quote(AName)
  else
    Result := AName;
end;

procedure TCustomDADataSet.SetUpdatingTable(const Value: string);
begin
  // Behaviour of UpdatingTable corrected:
  // We does not change value of the property,
  // but raise exception on open with invalid updating table
  if FUpdatingTable <> Value then begin
    FUpdatingTable := Value;

    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prUpdatingTable, Value);

    if Active then
      FDataSetService.InitUpdatingTable;
  end;
end;

function TCustomDADataSet.GetDBKeyList(const TableName, IndexName: string): string;
begin
  Assert(FDataSetService <> nil);
  Result := FDataSetService.GetDBKeyList(TableName, IndexName);
end;

{ Open/Close }

procedure TCustomDADataSet.Prepare;
var
  v: variant;
  MessageID: cardinal;
  vConnection: TCustomDAConnection;
begin
  if FSmartFetchOptions.Enabled then
    DatabaseErrorFmt(SSmartFetchIsUnallowed, ['Prepare']);

  if not Prepared and not Active then begin
    BeginConnection;

    // Get param values from master dataset to avoid bug with master/detail and
    // Execute method on detail dataset.
    if IsMasterDatasetActive and not FOptions.LocalMasterDetail then
      SetMasterParams(Params, nil);

    vConnection := UsedConnection;
    if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLPrepare(Self, FinalSQL, FParams, MessageID, True);

    StartWait;

    FCommand.WriteParams(FDataSetService.NeedParamValuesOnPrepare);
    DataTypeMap.WriteTo(FIRecordSet.DataTypeMap);

    inherited;

    FICommand.GetProp(prUseDescribeParams, v);
    if Boolean(v) then
      FCommand.UpdateParams;

    if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLPrepare(Self, FinalSQL, FParams, MessageID, False);
  end;
end;

procedure TCustomDADataSet.UnPrepare;
var
  NeedDisconnect: boolean;
  MessageID: cardinal;
  UnpreparePending: boolean;
  vConnection: TCustomDAConnection;
begin
  NeedDisconnect := Prepared;
  UnpreparePending := False;

  vConnection := UsedConnection;
  if Prepared and (vConnection <> nil) then
    if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then begin
      TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLUnprepare(Self, FinalSQL, FParams, MessageID, True);
      UnpreparePending := True;
    end;

  try
    inherited;
  finally
    if NeedDisconnect then
      EndConnection;
  end;
  if FIRecordSet <> nil then
    FIRecordSet.TablesInfo.Clear;
  //FUpdatingTableInfoIdx := -1;

  if UnpreparePending and (vConnection <> nil) then
    if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLUnprepare(Self, FinalSQL, FParams, MessageID, False);
end;

procedure TCustomDADataSet.SetActive(Value: Boolean);
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if not FStreamedOpen or (csDesigning in ComponentState) or (vConnection = nil) or
    vConnection.Options.KeepDesignConnected
  then
    inherited;
end;

procedure TCustomDADataSet.BeforeOpenCursor(InfoQuery: boolean);
begin
end;

procedure TCustomDADataSet.AfterOpenCursor(InfoQuery: boolean);
begin
end;

procedure TCustomDADataSet.OpenCursor(InfoQuery: boolean);
var
  v: variant;
  ReOpen: boolean;
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then
    vConnection.PushOperation(clOpen, vConnection.IsFailOverAllowed);
  try
  {$IFDEF WITH_IPROVIDER}
    FOldKeyFields := '';
  {$ENDIF}

    BeginConnection;
    try
      if Active then
        Exit;       // for open OnChangeConnect
      repeat
        ReOpen := False;

        BeforeOpenCursor(InfoQuery);
        // get param values from master dataset
        if not FOptions.LocalMasterDetail then
          if IsMasterDatasetActive then
            SetMasterParams(Params, nil)
          else
            if IsConnectedToMaster then
              InitMasterParams;

        if (not FetchAll or NonBlocking) and FOptions.QueryRecCount then
          FRecordCount := FDataSetService.GetRecCount
        else
          FRecordCount := 0;

        if NonBlocking then begin
          if not InfoQuery then begin
            SetCursor(crSQLArrow);
            DisableControls;
          end;
        end
        else
          StartWait;

        if not IsQuery or Prepared then
          DoBeforeExecute;

        try
          FCommand.CheckSQL;
          FCommand.WriteParams;
          DataTypeMap.WriteTo(FIRecordSet.DataTypeMap);
          inherited;
        except
          on E: TObject do begin
            if NonBlocking then begin
              EnableControls;
              StopWait;
            end;
            if E is EFailOver then begin
              vConnection.RestoreAfterFailOver;
              Reopen := True
            end
            else begin
              raise;
            end;
          end;
        end;
        FICommand.GetProp(prIsSelectParams, v);
        if v then begin
          // This is select of out parameters' values.
          // Set RowsAffected to a value received on execution of stored proc.
          FICommand.GetProp(prRowsProcessed, v);
          FRowsAffected := v;
        end
        else
          FRowsAffected := -1;
        FParamsProcessed := 0;

        AfterOpenCursor(InfoQuery);
      until (not ReOpen);
    finally
      if InfoQuery then
        EndConnection;
    end;
  finally
    if vConnection <> nil then
      vConnection.PopOperation;
  end;
end;

procedure TCustomDADataset.CloseCursor;
var
  NeedDisconnect: boolean;
begin
  NeedDisconnect := (FIRecordSet <> nil) and
    (FICommand.GetCursorState <> csInactive) and (not FIRecordSet.CanDisconnect); // if command is active and we doesn't
                                                                                  //already substract ConnectCount after all data fetch
  inherited;

  // FUpdateQuery may be prepared for optimization purposes
  if FDataSetService <> nil then
    FDataSetService.FUpdater.UnprepareUpdateObjects;

  if not Prepared or ((FIRecordSet <> nil) and FIRecordSet.FieldListDependsOnParams) then
    FieldDefs.Updated := False;

  if NeedDisconnect then //If there is opened cursor then we should disconnect
    EndConnection;
end;

procedure TCustomDADataset.InternalExecute(Iters: integer; Offset: integer);
begin
  FIRecordSet.ExecCommand(Iters, Offset);

  //if FIRecordSet.CommandType = ctCursor then
  //  InternalInitFieldDefs;   // TODO: does not work with AutoPrepare
end;

function TCustomDADataSet.DoOpenNext: boolean;
begin
  CheckDataSetService;
  Result := FDataSetService.OpenNext;
end;

procedure TCustomDADataSet.QuickOpen(var Info: TQuickOpenInfo; Refresh: boolean = False);
begin
  Info.OldActive := Active;
  Info.OldDebug := Debug;
  Info.OldFetchAll := FetchAll;
  Info.OldFetchRows := FetchRows;

  if not Active or Refresh then begin
    Debug := False;
    FetchAll := False;
    Close;
    FetchRows := 1;
    try
      Execute;
    except
      Restore(Info);
    end;
  end;
end;

procedure TCustomDADataSet.Restore(const Info: TQuickOpenInfo; RestoreActive: boolean = True);
begin
  Debug := Info.OldDebug;
  FetchAll := Info.OldFetchAll;
  if RestoreActive then begin
    if FetchRows <> Info.OldFetchRows then begin
      Close;
      FetchRows := Info.OldFetchRows;
    end;
    Active := Info.OldActive;
  end;
end;

procedure TCustomDADataSet.Execute;
begin
  Execute(1);
end;

procedure TCustomDADataSet.Execute(Iters: integer; Offset: integer = 0);
var
  ReExecute: boolean;
  MessageID: cardinal;
  OldLockDebug: boolean;
var
  vConnection: TCustomDAConnection;
begin
  if FSmartFetchOptions.Enabled then
    DatabaseErrorFmt(SSmartFetchIsUnallowed, ['Execute']);

  vConnection := UsedConnection;
  if vConnection <> nil then
    vConnection.PushOperation(clExecute);
  try
    if Executing then
      Exit;
    BeginConnection;
    if Active then
      Close;
    repeat
      ReExecute := False;

      if not NonBlocking then
        StartWait;
      try
        if Options.AutoPrepare then
          Prepare;
        if IsQuery then begin
          Open;
          EndConnection; //Here we decrement UsedConection.FConnectCount that was incremented in InternalExecute and then
                         //in OpenCursor, also we make disconection in case of all data fetched during Opening (Less or equal to one fetch block)
        end
        else begin
          DoBeforeExecute;

          // get param values from master dataset
          if not FOptions.LocalMasterDetail then
            if IsMasterDatasetActive then
              SetMasterParams(Params, nil)
            else
              if IsConnectedToMaster then
                InitMasterParams;

          if NonBlocking then
            SetCursor(crSQLArrow);

          FCommand.FBatchIters := Iters;
          FCommand.FBatchOffset := Offset;
          FCommand.CheckSQL;
          FCommand.WriteParams;

          if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
            TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, True);

          OldLockDebug := FLockDebug;
          try
            FLockDebug := True;

            InternalExecute(Iters, Offset);
            if IsQuery then begin
              Open;
              EndConnection; //Here we decrement UsedConection.FConnectCount that was incremented in InternalExecute and then
                             //in OpenCursor, also we make disconection in case of all data fetched during Opening (Less or equal to one fetch block)
            end;
          finally
            FLockDebug := OldLockDebug;
          end;

          if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
            TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, False);
        end;
      except
        on E: TObject do begin
          if NonBlocking then begin
            StopWait;
          end;
          if (E is EFailOver) and (EFailOver(E).FConnLostCause = clExecute) then begin
            vConnection.RestoreAfterFailOver; //Restore all read transactions
            ReExecute := True; //We should pass clConnectionApplyUpdates FailOver
          end
          else
            raise;
        end;
      end;
    until (not ReExecute);
  finally
    if vConnection <> nil then
      vConnection.PopOperation;
  end;
end;

procedure TCustomDADataSet.ExecSQL;
begin
  Execute;
end;

procedure TCustomDADataSet.DoBeforeExecute;
begin
  //This routine should be used for actions that performed before execute and
  //affected by local failover feature (like PrepareSQL in stored proc)

  if Assigned(FBeforeExecute) then
    FBeforeExecute(self);
end;

procedure TCustomDADataSet.DoAfterExecute(Result: boolean);
var
  Value: variant;
  AutoCommitUsed: boolean;
  vConnection: TCustomDAConnection;
begin
  if Result then begin
    FCommand.ReadParams;

    FICommand.GetProp(prRowsProcessed, Value);
    FRowsAffected := Value;
    FICommand.GetProp(prParamsProcessed, Value);
    FParamsProcessed := Value;

    if not IsQuery then begin
      UsedConnection.FIConnection.GetProp(prLastInsertId, Value);
      FLastInsertId := Value;
    end;
  end;

  if NonBlocking then begin
    EnableControls;
    StopWait;
  end;

  if not IsQuery then begin //Leave connection alive in case of SELECT .Execute instead of .Open to perform Fetch
    vConnection := UsedConnection;

    AutoCommitUsed := vConnection.AutoCommit and AutoCommit;

    if vConnection.Options.DisconnectedMode and vConnection.Connected then
      vConnection.DetectInTransaction(not AutoCommitUsed);

    if vConnection.IsMultipleTransactionsSupported then
      UsedTransaction.AutoCommitTransaction(AutoCommitUsed);
  end;

  if not Result or not IsQuery then
    EndConnection;    //we should read all Out parameters before disconnect
                      //In NonBlocking Mode this event must be called exactly after server execute
  if Assigned(FAfterExecute) then
    if not Assigned(FIRecordSet.AfterExecFetch) or not IsQuery or not Result then
      FAfterExecute(Self, Result);
end;

procedure TCustomDADataSet.DoAfterExecFetch(Result: boolean);
var
  vConnection: TCustomDAConnection;
begin
  if Result then
    if FCommand <> nil then
      FCommand.ReadParams;

  if NonBlocking then begin
    if Result then begin
      if State <> dsInactive then
        Resync([])
    end
    else
      Close;

    if not (FetchAll and Result) then
      StopWait;
    EnableControls;
  end;

  if not Result then begin
    EndConnection;

    vConnection := UsedConnection;
    if not IsQuery and vConnection.IsMultipleTransactionsSupported then
      UsedTransaction.AutoCommitTransaction(vConnection.AutoCommit and AutoCommit);
  end;

  if Assigned(FAfterExecute) then
    FAfterExecute(Self, Result);
end;

procedure TCustomDADataSet.DoAfterFetchAll(Result: boolean);
begin
  if NonBlocking then begin
    StopWait;
    if Trim(IndexFieldNames) <> '' then
      Resync([]);
  end;
end;

procedure TCustomDADataSet.DoAfterScroll;
begin
  if FFetchCanceled then begin
    Resync([]);
    FFetchCanceled := False;
  end;

  inherited;
end;

procedure TCustomDADataSet.DoOnBeforeFetch(var Cancel: boolean);
begin
  if not NonBlocking then
    StartWait;

  if Assigned(FBeforeFetch) then
    FBeforeFetch(Self, Cancel);
  FFetchCanceled := Cancel;
end;

procedure TCustomDADataSet.DoOnAfterFetch;
begin
  if not FetchAll or (FICommand.GetCursorState = csFetched) then
    if Assigned(FAfterFetch) then
      FAfterFetch(Self);

  if FIRecordSet.CanDisconnect then
    EndConnection; //Close connection after all data was fetched.
end;

procedure TCustomDADataSet.DoOnReopen;
begin
  if ResyncBeforeFetch then
    Resync([]);
end;

function TCustomDADataSet.Executing: boolean;
begin
  if FDataSetService <> nil then
    Result := FDataSetService.Executing
  else
    Result := False;

  if not Result then
    if FICommand <> nil then
      Result := FICommand.Executing;
end;

function TCustomDADataSet.Fetching: boolean;
begin
  if FICommand <> nil then
    Result := FICommand.GetCursorState in [csFetching, csFetchingAll]
  else
    Result := False;
end;

function TCustomDADataSet.FetchingAll: boolean;
begin
  if FICommand <> nil then
    Result := FICommand.GetCursorState = csFetchingAll
  else
    Result := False;
end;

function TCustomDADataSet.Fetched: boolean;
begin
  if FICommand <> nil then
    if FICommand.GetCursorState >= csFetched then
      Result := True
    else if not Executing then
      Result := Active and (FICommand.GetCursorState = csInactive)
    else
      Result := False
  else
    Result := False;
end;

procedure TCustomDADataSet.BreakExec;
begin
  if FDataSetService <> nil then
    FDataSetService.BreakExec;

  if FIRecordSet <> nil then begin
    if FICommand <> nil then
      FICommand.BreakExec;

    FIRecordSet.BreakFetch;
  end;
end;

function TCustomDADataSet.GetFieldDesc(const FieldName, TableName: string): TFieldDesc;
var
  i: Integer;
  Field: TCRFieldDesc;
begin
  Result := nil;

  if Data = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  for i := 0 to Data.Fields.Count - 1 do begin
    Field := TCRFieldDesc(Data.Fields[i]);

    if SameText(Field.ActualName, FieldName) and
       (Field.TableInfo <> nil) and
       SameText(Field.TableInfo.TableName, TableName)
    then begin
      Result := Field;
      Exit;
    end;
  end;
end;

procedure TCustomDADataSet.DoOnFieldsChanged;
begin
  if FDataSetService <> nil then
    FDataSetService.ClearFieldDescRefs;
end;

function TCustomDADataSet.GetKeyValuesSQL: string;
begin
  Result := FSmartFetchOptions.FSQLGetKeyValues.Text;
end;

function TCustomDADataSet.GetDataValuesSQL: string;
begin
  Result := FSmartFetchOptions.FSQLGetDataValues.Text;
end;

{ Before / After UpdateExecute }

function TCustomDADataSet.AssignedBeforeUpdateExecute: boolean;
begin
  Result := Assigned(FBeforeUpdateExecute);
end;

procedure TCustomDADataSet.DoBeforeUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
  Params: TDAParams);
begin
  if AssignedBeforeUpdateExecute then
    FBeforeUpdateExecute(Sender, StatementTypes, Params);
end;

function TCustomDADataSet.AssignedAfterUpdateExecute: boolean;
begin
  Result := Assigned(FAfterUpdateExecute);
end;

procedure TCustomDADataSet.DoAfterUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
  Params: TDAParams);
begin
  if AssignedAfterUpdateExecute then
    FAfterUpdateExecute(Sender, StatementTypes, Params);
end;

procedure TCustomDADataSet.GetCurrentKeys(out KeyFieldDescs: TFieldDescArray; out KeyFields: TFieldArray);
var
  DataFieldDescs: TFieldDescArray;
  KeyFieldsCount, DataFieldsCount: integer;
  i, j: integer;
  Delta: integer;
begin
  Assert(FIRecordSet <> nil);
  FIRecordSet.GetKeyFieldDescs(KeyFieldDescs);
  KeyFieldsCount := Length(KeyFieldDescs);

  if KeyFieldsCount = 0 then begin
    FIRecordSet.GetDataFieldDescs(DataFieldDescs);
    DataFieldsCount := Length(DataFieldDescs);
    if DataFieldsCount > 0 then begin
      Delta := 0;
      for i := DataFieldsCount - 1 downto 0 do
        if DataFieldDescs[i].IsBlob or DataFieldDescs[i].IsObject then begin
          Inc(Delta);
          for j := i to DataFieldsCount - Delta - 1 do
            DataFieldDescs[j] := DataFieldDescs[j + 1];
        end;

      SetLength(DataFieldDescs, DataFieldsCount - Delta);
      KeyFieldDescs := DataFieldDescs;
      KeyFieldsCount := DataFieldsCount - Delta;
    end;
  end;

  SetLength(KeyFields, KeyFieldsCount);
  for i := 0 to KeyFieldsCount - 1 do
    KeyFields[i] := GetField(KeyFieldDescs[i]);
end;

procedure TCustomDADataSet.GetCurrentValues(const KeyFieldDescs: TFieldDescArray; out Values: variant);
var
  KeyFieldsCount: integer;
  i: integer;
  RecBuf: TRecordBuffer;
  TmpVar: variant;
  EmptyRecBuf: boolean;
begin
  Values := Unassigned;
  KeyFieldsCount := Length(KeyFieldDescs);
  EmptyRecBuf := not GetActiveRecBuf(RecBuf);

  if KeyFieldsCount > 0 then begin
    Values := VarArrayCreate([0, KeyFieldsCount - 1], varVariant);
    for i := 0 to KeyFieldsCount - 1 do begin
      Values[i] := Unassigned;
      if not EmptyRecBuf then begin
        Data.GetMappedFieldAsVariant(KeyFieldDescs[i], RecBuf, TmpVar);
        Values[i] := TmpVar;
      end;
    end;
  end;
end;

procedure TCustomDADataSet.GetCurrentKeysAndValues(out KeyFieldDescs: TFieldDescArray; out KeyFields: TFieldArray; out Values: variant);
begin
  GetCurrentKeys(KeyFieldDescs, KeyFields);
  GetCurrentValues(KeyFieldDescs, Values);
end;

procedure TCustomDADataSet.DataReopen;
var
  FullReopen: boolean;
  OldRecordSize: Word;
begin
  FullReopen := Data.IsFullReopen;
  OldRecordSize := RecordSize;

  try
    Data.Reopen;//  RecordSize can be changed here
  except
    on E: Exception do begin
      if not (E is EFailOver) then
        Close;
      raise;
    end;
  end;

  if RecordSize <> OldRecordSize then begin  /// CR-D24236
    Close;
    Open;
  end
  else
    // On full reopen FieldDescs are recreated
    if FullReopen then
      FDataSetService.PreInitCursor;
end;

procedure TCustomDADataSet.InternalRefresh;
var
  MessageID: cardinal;
  KeyFieldDescs: TFieldDescArray;
  KeyFields: TFieldArray;
  Values: variant;
  KeyFieldsReaded: boolean;
  Retry: boolean;
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then
    vConnection.PushOperation(clRefresh, vConnection.IsFailOverAllowed);
  try
    KeyFieldsReaded := False;
    repeat
      BeginConnection;
      Retry := False;
      try
        FIRecordSet.NotFetchPrefetchedFields := True;
        try
          if not KeyFieldsReaded then
            GetCurrentKeysAndValues(KeyFieldDescs, KeyFields, Values);
          KeyFieldsReaded := True; //this will allow us to restore active record after failover
          if (not FetchAll or NonBlocking) and FOptions.QueryRecCount then
            FRecordCount := FDataSetService.GetRecCount
          else
            FRecordCount := 0;

          if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
            TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, 'Refresh', MessageID, True);

          StartWait;
          DoBeforeExecute;
          FCommand.WriteParams;

          inherited;

          if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
            TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, 'Refresh', MessageID, False);

          FRowsAffected := -1;

          if Length(KeyFields) = 0 then // CR N 11512
            First
          else begin
            GetCurrentKeys(KeyFieldDescs, KeyFields); // to re-read KeyFieldDescs
            if not LocateEx(KeyFields, KeyFieldDescs, Values, []) then
              First;
          end;
        finally
          FIRecordSet.NotFetchPrefetchedFields := False;
        end;

      except
        on E: Exception do begin
{         if NonBlocking then begin
            EnableControls;
            StopWait;
          end;}
          if E is EFailOver then begin
            vConnection.RestoreAfterFailOver;
            Retry := True
          end
          else
            raise;
        end;
      end;

    until not Retry;
  finally
    if vConnection <> nil then
      vConnection.PopOperation;
  end;
end;

procedure TCustomDADataSet.InternalRefreshQuick(const CheckDeleted: boolean);
begin
  CheckActive;
  FDataSetService.FUpdater.PerformRefreshQuick(CheckDeleted);
end;

procedure TCustomDADataSet.InternalClose;
begin
  try
    try
      inherited;
    except
      on E: EDAError do  // Borland's bug in DoInternalClose with FBufferCount
        if not E.IsFatalError then
          raise;
      else
        raise;
    end;
  finally
    // In case that User doesn't call Prepare directly
    if (FIRecordSet <> nil) and not Prepared then
      FIRecordSet.TablesInfo.Clear;
    if FDataSetService <> nil then
      FDataSetService.CloseCursor;
  end;
end;

procedure TCustomDADataSet.DoAfterOpen;
begin
  inherited;

{$IFDEF WITH_IPROVIDER}
  FOldTableName := PSGetTableName;
{$ENDIF}
end;

procedure TCustomDADataSet.SetDMLRefresh(Value: boolean);
begin
  if FDMLRefresh <> Value then begin
    FDMLRefresh := Value;
    if FDataSetService <> nil then
      FDataSetService.SQLGenerator.DMLRefresh := FDMLRefresh;
  end;
end;

procedure TCustomDADataSet.SetRefreshOptions(Value: TRefreshOptions);
begin
  FRefreshOptions := Value;

  if FIRecordSet <> nil then
    FIRecordSet.SetProp(prRoAfterUpdate, roAfterUpdate in RefreshOptions);
end;

{ Edit }

procedure TCustomDADataSet.SetReadOnly(Value: boolean);
begin
  if FReadOnly <> Value then begin
    FReadOnly := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prReadOnly, Value);
  end;
end;

procedure TCustomDADataSet.InternalEdit;
begin
  if FDataSetService.FUpdater.IsNeedEditPreconnect then   //TODO:
    BeginConnection;
  try
    FDataSetService.FUpdater.PrepareUpdate;

    inherited;
  except
    if FDataSetService.FUpdater.IsNeedEditPreconnect then
      EndConnection;
    raise;
  end;
end;

procedure TCustomDADataSet.InternalDelete;
begin
  FDataSetService.FUpdater.PrepareDelete;

  inherited;
end;

procedure TCustomDADataSet.InternalInsert;
begin
  if FDataSetService.FUpdater.IsNeedInsertPreconnect then   //TODO:
    BeginConnection;
  try
    FDataSetService.FUpdater.PrepareAppend;

    inherited;
  except
    if FDataSetService.FUpdater.IsNeedInsertPreconnect then
      EndConnection;
    raise;
  end;
end;

procedure TCustomDADataSet.InternalCancel;
begin
  try
    try
      FDataSetService.FUpdater.UnPrepareAppendUpdateDelete;  //TODO:
    finally
      inherited;
    end;
  finally
    if FDataSetService.FUpdater.IsPreconnected then
      EndConnection;
  end;
end;

procedure TCustomDADataSet.InternalPost;
var
  DataSet: TDataSet;
  MasterField, DetailField: TField;
  MasterName, DetailName: string;
  MasterPos, DetailPos: integer;
  RemoveRecord: Boolean;
begin
  // if Cancel was called in the BeforePost event
  if State = dsBrowse then
    exit;

  try
    inherited;

    if IsConnectedToMaster and not FOptions.LocalMasterDetail then begin
      DataSet := DataSource.DataSet;
      if (DataSet <> nil) and (DataSet.Active) then begin
        Assert(FIRecordSet <> nil);
        MasterPos := 1;
        DetailPos := 1;
        while True do begin
          MasterName := ExtractFieldName(FMasterFields, MasterPos);
          DetailName := ExtractFieldName(FDetailFields, DetailPos);
          if (MasterName <> '') and (DetailName <> '') then begin
            MasterField := DataSet.FindField(MasterName);
            if Assigned(MasterField) then begin
              DetailField := FindField(DetailName);
              if Assigned(DetailField) then begin // Fixed bug with case insensitive master/detail
                if (not FIRecordSet.IsCaseSensitive) and
                  (VarType(DetailField.AsVariant) = varString) and
                  (VarType(MasterField.AsVariant) = varString) then
                    RemoveRecord := AnsiCompareTextS(AnsiString(DetailField.AsVariant), AnsiString(MasterField.AsVariant)) <> 0
                else
                  if (not FIRecordSet.IsCaseSensitive) and
                    ((VarType(DetailField.AsVariant) = varOleStr)
                    {$IFDEF VER12P} or (VarType(DetailField.AsVariant) = varUString){$ENDIF}) and
                    ((VarType(MasterField.AsVariant) = varOleStr)
                    {$IFDEF VER12P} or (VarType(MasterField.AsVariant) = varUString){$ENDIF})
                    then
                      RemoveRecord := AnsiStrICompWS(DetailField.AsVariant, MasterField.AsVariant) <> 0
                  else
                    RemoveRecord := not VarEqual(DetailField.AsVariant, MasterField.AsVariant);
                if RemoveRecord then begin
                  Assert(not CachedUpdates, 'Can not use Master/Detail with CachedUpdates');
                  FIRecordSet.RemoveRecord;
                  Exit;
                end;
              end;
            end;
          end
          else
            break;
        end;
      end;
    end;
  finally
    if FDataSetService.FUpdater.IsPreconnected then
      EndConnection;
  end;
end;

procedure TCustomDADataSet.InternalDeferredPost;
begin
  inherited;
end;

procedure TCustomDADataSet.SetUpdateSQLIndex(Index: integer; Value: TStrings);
begin
  with FUpdateSQL[TStatementType(Index)] do begin
    BeginUpdate;
    try
      Assign(Value);
    finally
      EndUpdate;
    end;
  end;
end;

function TCustomDADataSet.GetUpdateSQLStatementTypes: TStatementTypes;
begin
  Result := [stInsert, stDelete, stUpdate, stRefresh, stLock, stRecCount];
end;

function TCustomDADataSet.GetUpdateSQLIndex(Index: integer): TStrings;
begin
  Result := FUpdateSQL[TStatementType(Index)];
end;

{$IFDEF VER17P}
procedure TCustomDADataSet.GetDetailLinkFields(MasterFields, DetailFields: TList<TField>);
{$ELSE}
procedure TCustomDADataSet.GetDetailLinkFields(MasterFields, DetailFields: TList);
{$ENDIF}
var
  i: integer;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and (Params.Count > 0) then begin
    MasterFields.Clear;
    DetailFields.Clear;
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields)
  end
  else
    inherited;
end;

function TCustomDADataSet.FindKey(const KeyValues: array of const): Boolean;

  function GetKeyValue(Index: integer): variant; // nearly copied from TField.AssignValue
    procedure Error;
    begin
      DatabaseErrorFmt(SFieldValueError, [IntToStr(Index)]);
    end;
  begin
    Result := Unassigned;

    with KeyValues[Index] do
      case VType of
        vtInteger:
          Result := Integer(VInteger);
        vtInt64:
          Result := VInt64^;
        vtBoolean:
          Result := Boolean(VBoolean);
      {$IFNDEF NEXTGEN}
        vtChar:
          Result := String(VChar);
      {$ENDIF}
        vtExtended:
          Result := Extended(VExtended^);
      {$IFNDEF NEXTGEN}
        vtString:
          Result := String(VString^);
      {$ENDIF}
        vtPointer, vtObject:
          if VPointer <> nil then Error;
      {$IFNDEF NEXTGEN}
        vtPChar:
          Result := String(VPChar);
        vtAnsiString:
          Result := String(VAnsiString);
      {$ENDIF}
        vtCurrency:
          Result := Currency(VCurrency^);
        vtVariant:
          if not VarIsClear(VVariant^) then
            Result := Variant(VVariant^);
        vtWideChar:
          Result := WideString(VWideChar);
        vtWideString:
          Result := WideString(VWideString);
      {$IFDEF VER12P}
        vtUnicodeString:
          Result := UnicodeString(VUnicodeString);
      {$ENDIF}
      else
        Error;
      end;
  end;

var
  KeyFieldDescs: TFieldDescArray;
  Values: variant;
  KeyFieldsCount: integer;
  KeyFields: TFieldArray;
  i: integer;
begin
  CheckBrowseMode;
  FIRecordSet.GetKeyFieldDescs(KeyFieldDescs);

  Values := Unassigned; // To prevent warning
  KeyFieldsCount := Length(KeyFieldDescs);
  SetLength(KeyFields, KeyFieldsCount);
  case KeyFieldsCount of
    0:
      DatabaseError(SKeyFieldsRequired);
    1: begin
      KeyFields[0] := GetField(KeyFieldDescs[0]);
      Values := GetKeyValue(0);
    end;
    else begin
      Values := VarArrayCreate([0, KeyFieldsCount - 1], varVariant);
      for i := 0 to KeyFieldsCount - 1 do begin
          KeyFields[i] := GetField(KeyFieldDescs[i]);

        if i <= High(KeyValues) then
          Values[i] := GetKeyValue(i)
        else
          Values[i] := Unassigned;
      end;
    end;
  end;

  Result := LocateEx(KeyFields, Values, FFindKeyOptions);
end;

procedure TCustomDADataSet.FindNearest(const KeyValues: array of const);
begin
  FFindKeyOptions := [lxNearest];
  try
    FindKey(KeyValues);
  finally
    FFindKeyOptions := [];
  end;
end;

procedure TCustomDADataSet.GotoCurrent(DataSet: TCustomDADataSet);
var
  KeyFieldDescs: TFieldDescArray;
  KeyFields, OwnKeyFields: TFieldArray;
  Values: variant;
  i: integer;
begin
  DataSet.GetCurrentKeysAndValues(KeyFieldDescs, KeyFields, Values);
  SetLength(OwnKeyFields, Length(KeyFields));
  for i := 0 to Length(KeyFields) - 1 do
    OwnKeyFields[i] := FieldByName(KeyFields[i].FieldName);

  if (Length(KeyFields) = 0) or not Locate(OwnKeyFields, Values, []) then
    First;
end;

procedure TCustomDADataSet.ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds);
var
  ReApply, AllowFailover: boolean;
  OldAutoCommit: boolean;
  ImplicitStart: boolean;
  ApplyTransaction: TDATransaction;
  vConnection: TCustomDAConnection;
begin
  CheckActive;

  vConnection := UsedConnection;
  if vConnection = nil then
    DatabaseError(SConnectionNotDefined);

  ApplyTransaction := UsedUpdateTransaction;
  AllowFailover := vConnection.IsFailOverAllowed and
    (not AutoCommit or vConnection.IsMultipleTransactionsSupported);
  vConnection.PushOperation(clApply, AllowFailover); //Add ApplyUpdates Operation to Operations Stack (FailOver)
  try
    repeat
      ReApply := False;
      try
        OldAutoCommit := AutoCommit;
        if vConnection.ApplyUpdatesInTransaction or
          (vConnection.Options.DisconnectedMode and (AutoCommit = False))
        then begin
          ImplicitStart := not ApplyTransaction.Active;
          if ImplicitStart then
            ApplyTransaction.StartTransaction;
          AutoCommit := False;  //This will turn ApplyUpdates to single Transaction operation
        end
        else
          ImplicitStart := False;

        // if transaction has been started in the Edit or Delete operation when LockMode = lmPessimistic
        if not ImplicitStart then
          ImplicitStart := FDataSetService.Updater.FLockTrStarted = ltsOnLockCachedUpdates;

        try
          inherited ApplyUpdates(UpdateRecKinds);
        finally
          AutoCommit := OldAutoCommit;
        end;

        if vConnection.ApplyUpdatesInTransaction then begin
          if AutoCommit then
            if ImplicitStart then
              ApplyTransaction.Commit
            else if vConnection.IsMultipleTransactionsSupported then
              ApplyTransaction.DoCommitRetaining;
        end;
      except
        on E: EFailOver do
          if E.FConnLostCause = clApply then begin
            vConnection.RestoreAfterFailOver;
            ReApply := True;
          end
          else
            raise;
      end;
    until (not ReApply);
  finally
    vConnection.PopOperation; //Remove ApplyUpdates Operation from Operations Stack
  end;
end;

procedure TCustomDADataSet.RefreshRecord;
begin
  CheckActive;

  case State of
    dsInsert: begin
      Cancel;
      Exit;
    end;
    dsEdit:
      Cancel;
    dsSetKey:
      Post;
  end;

  if not IsEmpty then begin
    UpdateCursorPos;
    FDataSetService.FUpdater.PerformRefreshRecord;
    if FRowsAffected = 0 then begin
      // remove deleted record from dataset
      if Options.RemoveOnRefresh then
        FIRecordSet.RemoveRecord;
      Resync([]);
    end
    else
      if FRowsAffected > 0 then
        Resync([]);
  //  DataEvent(deRecordChange, 0);
  end;
end;

procedure TCustomDADataSet.Lock;
begin
  CheckActive;
  FDataSetService.FUpdater.PerformLock;
end;

procedure TCustomDADataSet.UnLock;
begin
  CheckActive;
  FDataSetService.FUpdater.PerformUnLock;
end;

procedure TCustomDADataSet.AssignFieldValue(Param: TDAParam; Field: TField;
  Old: boolean);
var
  FieldDesc: TFieldDesc;
  FieldObject: TSharedObject;
  OldBlobValue: TBlob;
begin
  if Param.IsSharedObjectDataType(Field.DataType) then begin
    Param.DataType := Field.DataType;
    FieldObject := GetFieldObject(Field);
    if Old and (FieldObject is TBlob) then begin
      OldBlobValue := TBlob(FieldObject).Clone(True);
      try
        Param.ParamObject := OldBlobValue;
      finally
        OldBlobValue.Release;
      end;
    end
    else begin
      if Old then
        Param.ParamObject := GetFieldObject(Field, GetOldRecBuf)
      else
        Param.ParamObject := GetFieldObject(Field, GetNewRecBuf);
    end;
  end
  else
    if Old then
      Param.AssignFieldValue(Field, Field.OldValue)
    else
      Param.AssignFieldValue(Field, Field.NewValue);

  FieldDesc := GetFieldDesc(Field);
  if FieldDesc <> nil then
    AssignFieldType(Param, FieldDesc);
end;

procedure TCustomDADataSet.AssignFieldValue(Param: TDAParam; FieldDesc: TFieldDesc; Old: boolean);
var
  RecBuf: TRecordBuffer;
  Value: variant;
  FieldObject: TSharedObject;
  OldBlobValue: TBlob;
begin
  AssignFieldType(Param, FieldDesc);

  if Param.IsSharedObjectDataType(Param.DataType) then begin
    FieldObject := GetFieldObject(FieldDesc);
    if Old and (FieldObject is TBlob) then begin
      OldBlobValue := TBlob(FieldObject).Clone(True);
      try
        Param.ParamObject := OldBlobValue;
      finally
        OldBlobValue.Release;
      end;
    end
    else begin
      if Old then
        Param.ParamObject := GetFieldObject(FieldDesc, GetOldRecBuf)
      else
        Param.ParamObject := GetFieldObject(FieldDesc, GetNewRecBuf);
    end;
  end
  else begin
    if Old then
      RecBuf := GetOldRecBuf
    else
      RecBuf := GetNewRecBuf;

    if RecBuf <> nil then
      Data.GetFieldAsVariant(FieldDesc, RecBuf, Value)
    else
      Value := Null;

    if VarIsNull(Value) then
      Param.Clear
    else
      Param.Value := Value;
  end;
end;

procedure TCustomDADataSet.AssignFieldType(Param: TDAParam; FieldDesc: TFieldDesc);
var
  FieldType: TFieldType;
begin
  FieldType := GetFieldType(FieldDesc.DataType);

  if (FieldType = ftString) and FieldDesc.Fixed then
    Param.DataType := ftFixedChar
  else
  if (FieldType = ftWideString) and FieldDesc.Fixed then
    Param.DataType := TFieldType(ftFixedWideChar)
  else
  if (FieldType = ftMemo) and (FieldDesc.Length > 255) then
    Param.DataType := ftString
  else
{$IFDEF VER10P}
  if (FieldType = ftWideMemo) and (FieldDesc.Length > 255) then
    Param.DataType := ftWideString
  else
{$ENDIF}
{$IFDEF FPC}
  if (FieldType = ftWideMemo) and (FieldDesc.Length > 255) then
    Param.DataType := ftWideString
  else
{$ENDIF}
    Param.DataType := FieldType;

  if FieldDesc.SubDataType = 0 then
    Param.SubDataType := FieldDesc.DataType
  else
    Param.SubDataType := FieldDesc.SubDataType;
  Param.National := (FieldDesc as TCRFieldDesc).IsNational;
  Param.FEncryptor := (FieldDesc  as TCRFieldDesc).Encryptor;
  Param.Size := FieldDesc.Length;
  Param.NumericScale := FieldDesc.Scale;
end;

procedure TCustomDADataSet.SetDefaultExpressionValues;
var
  FieldList: TList;
  Parser: TSQLParser;

  procedure SelectDbValues(const OperationName, SQL: string);
  var
    MonitorClass: TDASQLMonitorClass;
    MessageID: cardinal;
    UpdateQuery: TCustomDADataSet;
    i: integer;
  begin
    FDataSetService.FUpdater.CheckUpdateQuery(stCustom);

    UpdateQuery := TCustomDADataSet(FDataSetService.FUpdater.FUpdateQuery);
    UpdateQuery.SQL.Text := SQL;

    BeginConnection;
    try
      MonitorClass := TDASQLMonitorClass(UsedConnection.SQLMonitorClass);
      if not FLockDebug and (MonitorClass.HasMonitor or Debug) then
        MonitorClass.SQLExecute(Self, SQL, UpdateQuery.Params, OperationName, MessageID, True);

      UpdateQuery.Open;

      if not FLockDebug and (MonitorClass.HasMonitor or Debug) then
        MonitorClass.SQLExecute(Self, SQL, UpdateQuery.Params, OperationName, MessageID, False);

      for i := 0 to FieldList.Count - 1 do
        TField(FieldList[i]).Value := UpdateQuery.Fields[i].Value;

      UpdateQuery.Close;
    finally
      EndConnection;
    end;
  end;

  function ExtractSimpleValue(Field: TField; const DefExpr: string; out Value: string): boolean;
  var
    Code, BrCount, p, ValCode: integer;
    Lexem: string;
  begin
    Parser.SetText(DefExpr);
    BrCount := 0;

    repeat
      Code := Parser.GetNext(Lexem);
      if Code = lxLeftBracket then
        Inc(BrCount)
      else
        break;
    until False;

    Result := True;
    Value := '';
    ValCode := lcEnd;

    if Code <> lcEnd then begin
      // default value is simple if there is only one lexem and this lexem is number or string
      if (Code = lcNumber) or
         (Code = lcString) or
         ((Code = lcIdent) and ((LowerCase(Lexem) = 'true') or (LowerCase(Lexem) = 'false')))
      then begin
        // strings are unquoted by parser
        Value := Lexem;
        ValCode := Code;

        if Code <> lcEnd then begin
          repeat
            Code := Parser.GetNext(Lexem);
            if Code = lxRightBracket then
              Dec(BrCount)
            else begin
              if Code <> lcEnd then
                Result := False;
              break;
            end;
          until False;
        end;
      end
      else
        Result := False;
    end;

    if BrCount <> 0 then
      Result := False;

    if Result and ((ValCode = lcNumber) or (Field is TNumericField)) and
      ({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.')
    then begin
      p := Pos('.', Value);
      if p > 0 then
        Value[p] := Char({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
    end;
  end;

{$IFDEF FPC}
  {$DEFINE OWN_DATE_FORMAT}
{$ENDIF}
{$IFNDEF MSWINDOWS}
  {$DEFINE OWN_DATE_FORMAT}
{$ENDIF}
var
  i: integer;
  VarValue: variant;
  DefExpr, StrValue, ExprList, SQL: string;
  ComplexDefault: boolean;
{$IFDEF OWN_DATE_FORMAT}
  OldShortDateFormat: string;
  OldDateSeparator: char;
{$ENDIF}
begin
{$IFDEF OWN_DATE_FORMAT}
  OldShortDateFormat := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
  OldDateSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator;
  {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat := 'YYYY-MM-DD';
  {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator := '-';
{$ENDIF}

  FInSettingDefaultExpressionValues := True;
  try
    ExprList := '';
    FieldList := TList.Create;
    Parser := FICommand.GetParserClass.Create('');
    try
      Parser.OmitBlank := True;
      Parser.OmitComment := True;
      Parser.QuotedString := False;
      Parser.DecSeparator := '.';

      for i := 0 to FieldCount - 1 do begin
        DefExpr := Fields[i].DefaultExpression;
        if DefExpr <> '' then begin
          ComplexDefault := False;
          if DefaultExpressionOldBehavior then begin
            try
              Fields[i].AsString := DefExpr;
            except
              ComplexDefault := True;
            end;
          end
          else begin
            if ExtractSimpleValue(Fields[i], DefExpr, StrValue) then begin
            {$IFDEF FPC}
              if Fields[i] is TBooleanField then
                Fields[i].Value := StrToBool(StrValue)
              else
            {$ENDIF}
            {$IFDEF OWN_DATE_FORMAT}
              if Fields[i] is TDateTimeField then
                Fields[i].AsString := StrValue
              else
            {$ENDIF}
              if Fields[i] is TNumericField then
                Fields[i].AsString := StrValue
              else
                Fields[i].Value := StrValue;
            end
            else
              ComplexDefault := True;
          end;

          if ComplexDefault then begin
            if FDataSetService.FUpdater.GetDefaultExpressionValue(DefExpr, VarValue) then
              Fields[i].Value := VarValue
            else begin
              if ExprList <> '' then
                ExprList := ExprList + ', ';
              ExprList := ExprList + DefExpr;
              FieldList.Add(Fields[i]);
            end;
          end;
        end;
      end;

      if ExprList <> '' then begin
        // select default values from Server
        SQL := FDataSetService.FSQLGenerator.GenerateSelectValues(ExprList);
        SelectDbValues('Select default values', SQL);
      end;
    finally
      FieldList.Free;
      Parser.Free;
    end;
  finally
    FInSettingDefaultExpressionValues := False;
  {$IFDEF OWN_DATE_FORMAT}
    {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat := OldShortDateFormat;
    {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator := OldDateSeparator;
  {$ENDIF}
  end;
end;

function TCustomDADataSet.GetCanModify: boolean;
begin
  Result := Active and FDataSetService.DetectCanModify;
end;

{$IFNDEF FPC}

procedure TCustomDADataSet.SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); // Need to support int64 fields on PerformSQL in RefreshRecord
var
  SaveState: TDataSetState;
begin
  if not (Field is TLargeintField) then
    inherited
  else
  begin
    // Nearly copied from inherited
    if Field.FieldKind <> fkData then
      Exit;

    SaveState := SetTempState(State);
    try
      if VarIsNull(Value) then
        Field.Clear
      else
        TLargeintField(Field).AsLargeInt := Value;
    finally
      RestoreState(SaveState);
    end;
  end;
end;

{$ENDIF}

function TCustomDADataSet.CanRefreshField(Field: TField): boolean;
begin
  Result := True;
end;

{ Master/Detail }

function TCustomDADataSet.UseLocalMasterDetailFilter: boolean;
begin
  Result := FOptions.LocalMasterDetail;
end;

procedure TCustomDADataSet.RefreshDetail(Sender: TObject);
var
  MessageID: cardinal;
  Retry: boolean;
  vConnection: TCustomDAConnection;
begin
  inherited;

  if not FOptions.LocalMasterDetail then begin
    DoBeforeRefresh;

    vConnection := UsedConnection;
    if vConnection <> nil then
      vConnection.PushOperation(clRefresh, vConnection.IsFailOverAllowed);
    try
      BeginConnection;
      DisableControls;
      try
        repeat
          Retry := False;
          try
            if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
              TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, 'Refresh', MessageID, True);

            StartWait;
            DoBeforeExecute;
            FCommand.WriteParams;

            CheckBrowseMode;
            UpdateCursorPos;
            try
              DataReopen;
              if FIRecordSet.IndexFieldCount > 0 then
                FIRecordSet.SortItems;
            finally
              Resync([]);
              First;
            end;

          except
            on E: Exception do begin
              if E is EFailOver then begin
                vConnection.RestoreAfterFailOver;
                Retry := True
              end
              else
                raise;
            end;
          end;
        until not Retry;

      finally
        EnableControls;
      end;

      if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
        TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, 'Refresh', MessageID, False);

      if (not FetchAll or NonBlocking) and FOptions.QueryRecCount then
        FRecordCount := FDataSetService.GetRecCount
      else
        FRecordCount := 0;

      FRowsAffected := -1;
    finally
      if vConnection <> nil then
        vConnection.PopOperation;

      DoAfterRefresh;
    end;
  end;
end;

function TCustomDADataSet.NeedDetailRefresh(Param: TDAParam; FieldValue: TSharedObject): boolean;
begin
  Result := Param.ParamObject <> FieldValue;

  if FieldValue <> nil then
    Param.ParamObject := FieldValue
  else begin
    Param.FreeObject;
    Param.FParamObject := Param.CreateObject;
  end
end;

function TCustomDADataSet.MDLinksRefreshed(Field: TField): boolean;
begin
  Result := SetMasterParams(Params, Field);
end;

function TCustomDADataSet.SetMasterParams(AParams: TDAParams; MasterField: TField): boolean;
var
  i, j: integer;
  DataSet: TDataSet;
  Param: TDAParam;
  Field: TField;
  Fields: TList;
  FieldDesc: TFieldDesc;
  SharedObject: TSharedObject;
  ParamName: string;
begin
  Result := False;

  if FOptions.LocalMasterDetail then
    Result := SetLocalMDLinks(MasterField)
  else
  if DataSource <> nil then begin
    DataSet := DataSource.DataSet;
    if (DataSet <> nil) and DataSet.Active then begin
      Fields := TList.Create;
      try
        // 0 .. AParams.Count-1 -> fields that found by name
        // AParams.Count .. AParams.Count*2-1 -> fields that found by actual name
        Fields.Count := AParams.Count * 2;

        for i := 0 to AParams.Count - 1 do begin
          Param := AParams[i];
          ParamName := SQLInfo.UnQuote(Param.Name);
          Field := DataSet.FindField(ParamName);
          if Field <> nil then
            Fields[i] := Field
          else if (DataSet is TCustomDADataSet) and (TCustomDADataSet(DataSet).FIRecordSet <> nil) then begin
            FieldDesc := TCustomDADataSet(DataSet).FIRecordSet.FindField(ParamName);
            if FieldDesc <> nil then begin
              Field := TCustomDADataSet(DataSet).GetField(FieldDesc);
              Fields[i + AParams.Count] := Field;
            end;
          end;
        end;

        // if field was found by name
        // then this field should not be found by actual name for another param
        for i := AParams.Count to Fields.Count - 1 do begin
          Field := Fields[i];
          if Field <> nil then
            for j := 0 to AParams.Count - 1 do
              if Field = Fields[j] then begin
                Fields[i] := nil;
                break;
              end;
        end;

        for i := 0 to AParams.Count - 1 do begin
          Param := AParams[i];

          // use field that was found by name
          // if absent, then field that was found by actual name
          if Fields[i] <> nil then
            Field := Fields[i]
          else
            Field := Fields[i + AParams.Count];

          if Field = nil then
            Continue;

          if Param.IsSharedObjectDataType(Field.DataType) then begin
            SharedObject := nil;
            if (DataSet is TCustomDADataset) then
              SharedObject := TCustomDADataset(DataSet).GetFieldObject(Field);

            Param.DataType := Field.DataType;
            Result := NeedDetailRefresh(Param, SharedObject) or Result;
          end
          else
            if not Active or
              ((VarIsEmpty(Param.Value) or not VarEqual(Param.Value, Field.Value)) and
              (not (VarIsEmpty(Param.Value) and Field.IsNull)))
            then begin
              Param.AssignFieldValue(Field, Field.Value);
              Result := True;
            end;

          if Param.ParamType = ptUnknown then
            Param.ParamType := ptInput;
        end;
      finally
        Fields.Free;
      end;
    end;
  end;
end;

procedure TCustomDADataSet.InitMasterParams;
begin
  FDataSetService.InitMasterParams(Params);
end;

procedure TCustomDADataSet.MDPropertiesChanged;
var
  OldActive: boolean;
begin
  OldActive := Active;

  Close;
  UnPrepare;
  AssembleSQL;

  if OldActive then
    Open;
end;

function TCustomDADataSet.SplitFieldName(const Fields: string; var Pos: Integer): string;
var
  Info: TSQLObjectInfo;
begin
  Result := ExtractFieldName(Fields, Pos);
  SQLInfo.SplitObjectName(Result, Info);
  Result := Info.Name;
end;

procedure TCustomDADataSet.AssembleSQL;
begin
  FCommand.FSQLModified := True;
  FCommand.AssembleSQL;

// close and unprepare to apply new sql
  UnPrepare;
end;

function TCustomDADataSet.GetForceSPInit: boolean;
begin
  Result := FCommand.GetForceSPInit;
end;

procedure TCustomDADataSet.InternalCreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean = False);
begin
  FCommand.InternalCreateProcCall(Name, NeedDescribe, IsQuery);
end;

procedure TCustomDADataSet.ScanMacros(Sender: TObject {$IFNDEF FPC}= nil{$ENDIF});
var
  AllSQL: string;
  stIdx: TStatementType;
begin
  AllSQL := SQL.Text;
  for stIdx := Low(FUpdateSQL) to High(FUpdateSQL) do
    if Assigned(FUpdateSQL[stIdx]) then
      AllSQL := AllSQL + FUpdateSQL[stIdx].Text;
  Macros.Scan(AllSQL);
end;

{function TCustomDADataSet.DoGetFinalSQL: string;
begin
  Result := GetFinalSQL;
end;

procedure TCustomDADataSet.DoScanMacros(Sender: TObject);
begin
  ScanMacros;
end;}

procedure TCustomDADataSet.DefineProperties(Filer: TFiler);
  function InternalWriteParams: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TCustomDADataSet(Filer.Ancestor).FParams)
    else
      Result := FParams.Count > 0;
  end;

  function WriteMacros: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FMacros.IsEqual(TCustomDADataSet(Filer.Ancestor).FMacros)
    else
      Result := FMacros.Count > 0;
  end;
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('ParamData', FCommand.ReadParamData, FCommand.WriteParamData, InternalWriteParams);
  Filer.DefineProperty('MacroData', FCommand.ReadMacroData, FCommand.WriteMacroData, WriteMacros);
  Filer.DefineProperty('CommandStoredProcName', FCommand.ReadStoredProcName, FCommand.WriteStoredProcName,
    FCommand.FStoredProcName <> '');
  Filer.DefineProperty('StoredProcIsQuery', FCommand.ReadStoredProcIsQuery,
    FCommand.WriteStoredProcIsQuery, FCommand.FStoredProcIsQuery);
  Filer.DefineProperty('Conditions', ReadConditions, WriteConditions, FWhereConditions.Count > 0);
end;

procedure TCustomDADataSet.ReadConditions(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FWhereConditions);
end;

procedure TCustomDADataSet.WriteConditions(Writer: TWriter);
begin
  Writer.WriteCollection(FWhereConditions);
end;

function TCustomDADataSet.FindParam(const Value: string): TDAParam;
begin
  Result := FParams.FindParam(Value);
end;

function TCustomDADataSet.ParamByName(const Value: string): TDAParam;
begin
  Result := FParams.ParamByName(Value);
end;

function TCustomDADataSet.FindMacro(const Value: string): TMacro;
begin
  Result := FMacros.FindMacro(Value);
end;

function TCustomDADataSet.MacroByName(const Value: string): TMacro;
begin
  Result := FMacros.MacroByName(Value);
end;

{ Additional data types }

function TCustomDADataSet.GetField(FieldDesc: TFieldDesc): TField;
var
  i: integer;
begin
  Assert(FieldDesc <> nil);
  Result := nil;
  for i := 0 to Fields.Count - 1 do
    if Fields[i].FieldNo = FieldDesc.FieldNo then begin
      Result := Fields[i];
      Break;
    end;
end;

function TCustomDADataSet.GetDataType(const FieldName: string): integer;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Result := FIRecordSet.FieldByName(FieldName).DataType
end;

function TCustomDADataSet.GetFieldPrecision(const FieldName: string): integer;
var
  Field: TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Field := FIRecordSet.FieldByName(FieldName);
  if (Field <> nil) and (Field.DataType in [dtInteger, dtLargeint, dtFloat, dtSingle,
    dtBCD, dtFmtBCD])
  then
    Result := Field.Length
  else
    Result := 0;
end;

function TCustomDADataSet.GetFieldScale(const FieldName: string): integer;
var
  Field: TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Field := FIRecordSet.FieldByName(FieldName);
  if (Field <> nil) and (Field.DataType in [dtInteger, dtLargeint, dtFloat, dtSingle,
    dtBCD, dtFmtBCD])
  then
    Result := Field.Scale
  else
    Result := 0;
end;

function TCustomDADataSet.GetFieldObject(FieldDesc: TFieldDesc): TSharedObject;
var
  RecBuf: TRecordBuffer;
begin
  if GetActiveRecBuf(RecBuf) then
    Result := GetFieldObject(FieldDesc, RecBuf)
  else
    Result := nil;
end;

function TCustomDADataSet.GetFieldObject(FieldDesc: TFieldDesc; RecBuf: TRecordBuffer): TSharedObject;
begin
  Assert(FIRecordSet <> nil);
  if not FieldDesc.IsComplex then
    DatabaseError(SNeedBlobType);

  Result := FIRecordSet.GetFieldObject(FieldDesc, RecBuf);
end;

function TCustomDADataSet.GetFieldObject(Field: TField): TSharedObject;
var
  FieldDesc: TFieldDesc;
begin
  FieldDesc := GetFieldDesc(Field);
  Result := GetFieldObject(FieldDesc);
end;

function TCustomDADataSet.GetFieldObject(Field: TField; RecBuf: TRecordBuffer): TSharedObject;
var
  FieldDesc: TFieldDesc;
begin
  FieldDesc := GetFieldDesc(Field);
  Result := GetFieldObject(FieldDesc, RecBuf);
end;

function TCustomDADataSet.GetFieldObject(const FieldName: string): TSharedObject;
var
  FieldDesc: TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  FieldDesc := FIRecordSet.FieldByName(FieldName);
  Result := GetFieldObject(FieldDesc);
end;

procedure TCustomDADataSet.GetKeyFieldNames(List: TStrings);
var
  KeyFieldDescs: TFieldDescArray;
  i: integer;
begin
  if Active then begin
    List.BeginUpdate;
    try
      List.Clear;
      FIRecordSet.GetKeyFieldDescs(KeyFieldDescs);
      for i := 0 to High(KeyFieldDescs) do
        List.Add(KeyFieldDescs[i].ActualName);
    finally
      List.EndUpdate;
    end;
  end;
end;

{$IFDEF WITH_IPROVIDER}

{ IProviderSupport }

function TCustomDADataSet.PSInTransaction: Boolean;
begin
  Result := UsedUpdateTransaction.Active;
end;

procedure TCustomDADataSet.PSStartTransaction;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if AutoCommit and (vConnection <> nil) and vConnection.AutoCommit then
    UsedUpdateTransaction.StartTransaction;
end;

procedure TCustomDADataSet.PSEndTransaction(Commit: Boolean);
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if AutoCommit and (vConnection <> nil) and vConnection.AutoCommit then
    if Commit then
      UsedUpdateTransaction.Commit
    else
      UsedUpdateTransaction.Rollback;
end;

procedure TCustomDADataSet.PSExecute;
begin
  Execute;
end;

{$IFDEF VER17P}
function TCustomDADataSet.PSExecuteStatement(const ASQL: string; AParams: TParams): Integer;
var
  Query: TCustomDADataSet;
begin
  CheckDataSetService;
  FDataSetService.FUpdater.CheckUpdateQuery(stCustom);
  Query := TCustomDADataSet(FDataSetService.FUpdater.FUpdateQuery);
  Query.FLockDebug := False;
  Query.Transaction := UsedUpdateTransaction;

  Result := InternalPSExecuteStatement(ASQL, AParams, Query);
end;

function TCustomDADataSet.PSExecuteStatement(const ASQL: string; AParams: TParams; var ResultSet: TDataSet): Integer;
var
  Query: TCustomDADataSet;
begin
  Query := UsedConnection.CreateDataSet;
  try
    Result := InternalPSExecuteStatement(ASQL, AParams, Query);
    ResultSet := Query;
  except
    Query.Free;
    raise;
  end;
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
function TCustomDADataSet.PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: IntPtr = nil): Integer;
var
  Query: TCustomDADataSet;
begin
  if Assigned(ResultSet) then begin
    Query := UsedConnection.CreateDataSet;
    try
      Result := InternalPSExecuteStatement(ASQL, AParams, Query);
      TDataSet(ResultSet^) := Query;
    except
      Query.Free;
      raise;
    end;
  end
  else begin
    CheckDataSetService;
    FDataSetService.FUpdater.CheckUpdateQuery(stCustom);
    Query := TCustomDADataSet(FDataSetService.FUpdater.FUpdateQuery);
    Query.FLockDebug := False;
    Query.Transaction := UsedUpdateTransaction;

    Result := InternalPSExecuteStatement(ASQL, AParams, Query);
  end;
end;
{$ENDIF}

function TCustomDADataSet.InternalPSExecuteStatement(const ASQL: string; AParams: TParams; Query: TCustomDADataSet): Integer;

  procedure SetSQL(SQL: TStrings);
  var
    i: integer;
    Code: Integer;
    StartPos: integer;
    ParsedSQL: StringBuilder;
    Parser: TSQLParser;
  begin
    // replace parameters in SQL
    ParsedSQL := StringBuilder.Create(Length(ASQL) + Length(ASQL) div 2);
    try
      Parser := GetParserClass.Create(ASQL);
      try
        Parser.OmitBlank := False;
        Parser.OmitComment := True;
        Parser.QuotedString := True;
        Parser.DecSeparator := '.';
        Parser.ToBegin;
        StartPos := Parser.CurrPos;
        i := 1;
        repeat
          Code := Parser.GetNextToken;
          if Code = lxQuestion then begin
            ParsedSQL.Append(Copy(ASQL, StartPos + 1, Parser.CurrPos - StartPos - 1));
            ParsedSQL.Append(':');
            ParsedSQL.Append(IntToStr(i));
            Inc(i);

            StartPos := Parser.CurrPos;
          end;
        until Code = lcEnd;
        ParsedSQL.Append(Copy(ASQL, StartPos + 1, Parser.CurrPos - StartPos));
      finally
        Parser.Free;
      end;

      SQL.Text := ParsedSQL.ToString;
    finally
      ParsedSQL.Free;
    end;
  end;

  procedure SetParams(Params: TDAParams);
  var
    i: integer;
  begin
    //FQuery.Params.Assign(AParams);  // params doesn't name
    for i := 0 to Params.Count - 1 do begin
      Params[i].Assign(AParams[i]);
      Params[i].Name := IntToStr(i + 1);
      if Params[i].ParamType = ptUnknown then
        Params[i].ParamType := ptInput;
    end;
  end;

begin
  SetSQL(Query.SQL);
  SetParams(Query.Params);

  Query.Debug := Debug;
  Query.Execute;
  Result := Query.RowsAffected;
end;

function TCustomDADataSet.PSGetParams: DB.TParams;
begin
  Result := Params;
end;

function TCustomDADataSet.PSGetQuoteChar: string;
begin
  Result := '"';
end;

function TCustomDADataSet.PSGetTableName: string;
begin
  if Active then begin
    if (FIRecordSet <> nil) and (FIRecordSet.UpdatingTableInfo <> nil) then
      Result := FIRecordSet.UpdatingTableInfo.TableName
    else
      Result := SQLInfo.NormalizeName(FUpdatingTable);
  end
  else begin
    Result := FOldTableName;

    if Result = '' then
      Result := FUpdatingTable;
    if Result = '' then
      Result := GetTableNameFromSQL(SQL.Text);

    Result := SQLInfo.NormalizeName(Result);
  end;
end;

function TCustomDADataSet.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

function TCustomDADataSet.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TCustomDADataSet.PSReset;
begin
  inherited PSReset;

  if Active then begin
    Close;
    Open;
  end;
end;

procedure TCustomDADataSet.PSSetParams(AParams: DB.TParams);
var
  Param: TDAParam;
  i: integer;
begin
  if AParams.Count <> 0 then begin
    if AParams.Count >= Params.Count then
      Params.Assign(AParams)
    else begin
      Params.BeginUpdate;
      try
        for i := 0 to AParams.Count - 1 do begin
          Param := Params.FindParam(AParams.Items[i].Name);
          if Param <> nil then
            Param.Assign(AParams.Items[i])
          else
            Params.Items[i].Assign(AParams.Items[i])
        end;
      finally
        Params.EndUpdate;
      end;
    end;
  end;
end;

procedure TCustomDADataSet.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    SQL.Text := CommandText;
end;

function TCustomDADataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
var
  UpdateAction: TUpdateAction;
begin
  Result := False;
  if Assigned(OnUpdateRecord) then
  begin
    UpdateAction := uaFail;
    OnUpdateRecord(Delta, UpdateKind, UpdateAction);
    Result := UpdateAction = uaApplied;
  end;

  if not Result and Assigned(Delta) then begin
    CheckDataSetService;
    Result := FDataSetService.FUpdater.PerformPSUpdateRecord(UpdateKind, Delta);
  end;
end;

function TCustomDADataSet.PSGetDefaultOrder: TIndexDef;

  function AddField(const Fields, NewField: string): string;
  begin
    if Fields <> '' then
      Result := Fields + ';' + NewField
    else
      Result := NewField;
  end;

var
  S, Token, SaveField: string;
  Parser: TBoolParser;
  Code, Index: integer;
begin
  Result := nil;
  S := GetOrderBy;
  if S = '' then
    Exit;

  Parser := TBoolParser.Create(S);
  Parser.DecSeparator := '.';
  try
    Result := TIndexDef.Create(nil);
    Parser.ToBegin();

    Code := Parser.GetNext(Token);
    while Code <> lcEnd do begin
      case Code of
        lcIdent, lcString: begin
          if 'DESC' = UpperCase(Token) then begin
            Result.DescFields := AddField(Result.DescFields, SaveField);
          end
          else
          if Assigned(FindField(Token)) then begin
            Result.Fields := AddField(Result.Fields, Token);
            SaveField := Token;
          end;
        end;
        lcNumber: begin
          try
            Index := StrToInt(Token);
            SaveField := FieldDefs[Index - 1].Name;
          except // float number
            Code := Parser.GetNext(Token);  //to prevent freezeng on errors. CR
            continue;
          end;
          Result.Fields := AddField(Result.Fields, SaveField);
        end;
      end;

      Code := Parser.GetNext(Token);
    end;
  finally
    Parser.Free;
  end;
end;

function TCustomDADataSet.PSGetKeyFields: string;
begin
  Result := inherited PSGetKeyFields;
  if Result = '' then begin
    if FOldKeyFields = '' then begin
      if FKeyFields <> '' then
        FOldKeyFields := FKeyFields
      else begin
        CheckDataSetService;
        if not PreventPSKeyFields then //Set product specific KeyField values or omit Server roundtrip on DS opening
          PSDetectKeyFields(Self);
      end;
    end;
    Result := FOldKeyFields;
  end;
end;

procedure TCustomDADataSet.PSDetectKeyFields(DataSet: TDataSet);
var
  i: integer;
  KeyField: TField;
  KeyFieldDescs: TFieldDescArray;
  QO: boolean;
  QOInfo: TQuickOpenInfo;
begin
  KeyFieldDescs := nil;
  QO := not Active and (Connection <> nil) and ((FIRecordSet = nil) or (FIRecordSet.Fields.Count = 0));
  if QO then
    QuickOpen(QOInfo);
  try
    if FIRecordSet <> nil then
      FIRecordSet.GetKeyFieldDescs(KeyFieldDescs, True); // For TClientDataSet with SDAC

    for i := 0 to High(KeyFieldDescs) do begin
      KeyField := DataSet.FindField(KeyFieldDescs[i].Name);
      if KeyField <> nil then begin
        KeyField.ProviderFlags := KeyField.ProviderFlags + [pfInKey];
        if FOldKeyFields = '' then
          FOldKeyFields := KeyFieldDescs[i].Name
        else
          FOldKeyFields := FOldKeyFields + ';' + KeyFieldDescs[i].Name;
      end;
    end;
  finally
    if QO then
      Restore(QOInfo);
  end;
end;

{$ENDIF}

procedure TCustomDADataSet.AssignTo(Dest: TPersistent);
var
  stIdx: TStatementType;
begin
  inherited;

  if Dest is TCustomDADataSet then begin
    TCustomDADataSet(Dest).Connection := Connection;
    TCustomDADataSet(Dest).Transaction := FTransaction;
    TCustomDADataSet(Dest).UpdateTransaction := FUpdateTransaction;
    TCustomDADataSet(Dest).ParamCheck := ParamCheck;  // before SQL
    TCustomDADataSet(Dest).SQL.Text := SQL.Text;
    for stIdx := Low(FUpdateSQL) to High(FUpdateSQL) do
      if Assigned(TCustomDADataSet(Dest).FUpdateSQL[stIdx]) and Assigned(FUpdateSQL[stIdx]) then
        TCustomDADataSet(Dest).FUpdateSQL[stIdx].Text := FUpdateSQL[stIdx].Text;

    TCustomDADataSet(Dest).FilterSQL := FilterSQL;
    TCustomDADataSet(Dest).Macros.Assign(Macros);
    TCustomDADataSet(Dest).Params.Assign(Params);
    TCustomDADataSet(Dest).Debug := Debug;

    TCustomDADataSet(Dest).FetchRows := FetchRows;
    TCustomDADataSet(Dest).UniDirectional := UniDirectional;
    TCustomDADataSet(Dest).AutoCommit := AutoCommit;
    TCustomDADataSet(Dest).RefreshOptions := RefreshOptions;
    TCustomDADataSet(Dest).UpdatingTable := UpdatingTable;
    TCustomDADataSet(Dest).KeyFields := KeyFields;
    TCustomDADataSet(Dest).LockMode := LockMode;
    TCustomDADataSet(Dest).DMLRefresh := DMLRefresh;
    TCustomDADataSet(Dest).Options.Assign(Options);
    TCustomDADataSet(Dest).FDataTypeMap.Assign(DataTypeMap);
    TCustomDADataSet(Dest).FEncryption.Assign(Encryption);
    TCustomDADataSet(Dest).FSmartFetchOptions.Assign(FSmartFetchOptions);
    TCustomDADataSet(Dest).FWhereConditions.Assign(FWhereConditions);

    if (FCommand <> nil) and (TCustomDADataSet(Dest).FCommand <> nil) then begin
      TCustomDADataSet(Dest).FCommand.FStoredProcName := FCommand.FStoredProcName;
      TCustomDADataSet(Dest).FCommand.FStoredProcIsQuery := FCommand.FStoredProcIsQuery;
    end;
  end;
end;

procedure TCustomDADataSet.SetConnection(Value: TCustomDAConnection);
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if (Value <> FConnection) or (Value <> vConnection) then begin

    if vConnection <> nil then begin
      Disconnect;
      vConnection.UnRegisterClient(Self);
      vConnection.UnRegisterClient(FCommand);
    end;

    FConnection := Value;

    if FConnection <> nil then begin
      Value.RegisterClient(Self, ConnectChange);

      if FIRecordSet <> nil then begin
        if FConnection.FIConnection <> nil then
          CheckIRecordSet;
        FIRecordSet.SetConnection(FConnection.FIConnection);
      end;
    end
    else
      if FIRecordSet <> nil then
        FIRecordSet.SetConnection(nil);

    DataEvent(dePropertyChange, 0);
  end;
end;

function TCustomDADataSet.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TCustomDADataSet.SetTransaction(Value: TDATransaction);
begin
  if (FTransaction <> Value) then begin
    if FTransaction <> nil then
      FTransaction.RemoveFreeNotification(Self);
    FTransaction := Value;
    if FTransaction <> nil then
      FTransaction.FreeNotification(Self);
  end;
end;

procedure TCustomDADataSet.SetUpdateTransaction(Value: TDATransaction);
begin
  if (FUpdateTransaction <> Value) then begin
    if FUpdateTransaction <> nil then
      FUpdateTransaction.RemoveFreeNotification(Self);
    FUpdateTransaction := Value;
    if FUpdateTransaction <> nil then
      FUpdateTransaction.FreeNotification(Self);
  end;
end;

function TCustomDADataSet.GetSQL: TStrings;
begin
  Result := FCommand.SQL;
end;

procedure TCustomDADataSet.SetSQL(Value: TStrings);
begin
  FCommand.SQL := Value;
end;

procedure TCustomDADataSet.SetFetchRows(Value: integer);
begin
  if FFetchRows <> Value then begin
    CheckInactive;
    if (Value < 1) or (Value > 65535) then
      DatabaseError(SInvalidFetchRows);

    FFetchRows := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prFetchRows, FetchRows);
  end;
end;

function TCustomDADataSet.GetFetchAll: boolean;
begin
  Result := FFetchAll;
end;

procedure TCustomDADataSet.SetFetchAll(Value: boolean);
begin
  if FFetchAll <> Value then begin
    FFetchAll := Value;
    if FFetchAll then
      UniDirectional := False;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prFetchAll, FFetchAll);
    if FFetchAll and Active then begin
      FIRecordSet.FetchAll;
      Resync([]);
    end;
  end;
end;

function TCustomDADataSet.GetNonBlocking: boolean;
begin
  Result := FNonBlocking;
end;

procedure TCustomDADataSet.SetNonBlocking(Value: boolean);
begin
  if FNonBlocking <> Value then begin
    FNonBlocking := Value;
    if FICommand <> nil then
      FICommand.SetProp(prNonBlocking, Value);
  end;
end;

function TCustomDADataSet.SQLAutoGenerated: boolean;
begin
  Result := False;
end;

function TCustomDADataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  Blob: TBlob;
  OldRollback: boolean;
begin
  if Field.DataSet = Self then
    Blob := GetBlob(Field)
  else
    Blob := GetBlob(Field.FieldName);
  if (Blob <> nil) and (Mode <> bmWrite) and
    UsedConnection.ConvertEOL and (Field.DataType in MemoTypes)
  then begin
    OldRollback := Blob.RollbackEnabled;
    Blob.RollbackEnabled := False;
    try
      Blob.AddCR;
    finally
      Blob.RollbackEnabled := OldRollback;
    end;
  end;

  Result := inherited CreateBlobStream(Field, Mode);
end;

function TCustomDADataSet.GetParams: TDAParams;
begin
  Result := FCommand.Params;
end;

procedure TCustomDADataSet.SetParams(Value: TDAParams);
begin
  FCommand.Params := Value;
end;

function TCustomDADataSet.GetParamCount: word;
begin
  Result := FParams.Count;
end;

function TCustomDADataSet.GetParamCheck: boolean;
begin
  Result := FCommand.ParamCheck;
end;

procedure TCustomDADataSet.SetParamCheck(Value: boolean);
begin
  FCommand.ParamCheck := Value;
end;

function TCustomDADataSet.GetMacros: TMacros;
begin
  Result := FCommand.Macros;
end;

procedure TCustomDADataSet.SetMacros(Value: TMacros);
begin
  FCommand.Macros := Value;
end;

function TCustomDADataSet.GetMacroCount: word;
begin
  Result := FMacros.Count;
end;

function TCustomDADataSet.GetRecordCount: integer;
begin
  if (FIRecordSet = nil) or Fetched or not Options.QueryRecCount or (FRecordCount = 0) then
    Result := inherited GetRecordCount
  else
    Result := FRecordCount - FIRecordSet.RowsFetched + FIRecordSet.RecordCount;
end;

function TCustomDADataSet.GetRowsAffected: integer;
begin
  Result := FRowsAffected;
end;

function TCustomDADataSet.GetParamsProcessed: integer;
begin
  Result := FParamsProcessed;
end;

procedure TCustomDADataSet.SetUniDirectional(Value: boolean);
begin
  if Value <> FUniDirectional then begin
    CheckInactive;
    FUniDirectional := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prUniDirectional, FUniDirectional);
    if FUniDirectional then
      FetchAll := False;
  end;
end;

procedure TCustomDADataSet.SetAutoCommit(Value: boolean);
begin
  FAutoCommit := Value;
  if FICommand <> nil then
    FICommand.SetProp(prAutoCommit, FAutoCommit);
end;

function TCustomDADataSet.GetIsQuery: boolean;
begin
  if FIRecordSet <> nil then
    Result := FIRecordSet.RowsReturn
  else
    Result := False;
end;

//------------------------------------------------------------------------------
// SQL Modification methods
//------------------------------------------------------------------------------

procedure TCustomDADataSet.SaveSQL;
begin
  FBaseSQL := SQL.Text;
end;

procedure TCustomDADataSet.RestoreSQL;
begin
  if FBaseSQL <> '' then begin
    SQL.Text := FBaseSQL;
    FBaseSQL := '';
  end;
end;

function TCustomDADataSet.SQLSaved: boolean;
begin
  Result := FBaseSQL <> '';
end;

/// SaveModifiedSQL is used to back up original sql text before modification.
procedure TCustomDADataSet.SaveModifiedSQL(const NewSQL: string);
var
  BaseSQL: string;
begin
  if NewSQL <> Trim(SQL.Text) then begin
    if FBaseSQL = '' then
      FBaseSQL := SQL.Text;
    if not BaseSQLOldbehavior then
      BaseSQL := FBaseSQL;

    SQL.Text := NewSQL;
    if not BaseSQLOldbehavior then
      FBaseSQL := BaseSQL;
  end;
end;

procedure TCustomDADataSet.InternalOpen;
var
  MessageID: cardinal;
  vConnection: TCustomDAConnection;
begin
  if Options.AutoPrepare then
    Prepare;

  vConnection := UsedConnection;
  if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
    TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, True);

  // When FetchAll is True and fetch is performed on recordset openning
  // we should avoid connection releasing to perform all operations
  // in InitCursor in the same connection
  BeginConnection;
  try
    inherited;

    FDataSetService.InitCursor;
  finally
    EndConnection;
  end;

  if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
    TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, False);

  FNeedAddRef := FUniDirectional or (FSmartFetchOptions.Enabled and FSmartFetchOptions.LiveBlock);
end;

function TCustomDADataSet.GetParserClass: TSQLParserClass;
begin
  Result := TSQLParser;
end;

function TCustomDADataSet.SQLGetFrom(const SQLText: string): string;
begin
  Result := _GetFrom(SQLText, GetParserClass, False, MacroChar);
end;

function TCustomDADataSet.SQLGetWhere(const SQLText: string): string;
begin
  Result := _GetWhere(SQLText, GetParserClass, False, MacroChar);
end;

function TCustomDADataSet.SQLAddWhere(const SQLText, Condition: string): string;
begin
  Result := _AddWhere(SQLText, Condition, GetParserClass, False, MacroChar);
end;

function TCustomDADataSet.SQLDeleteWhere(const SQLText: string): string;
begin
  Result := _SetWhere(SQLText, '', GetParserClass, True);
end;

function TCustomDADataSet.SQLGetOrderBy(const SQLText: string): string;
begin
  Result := _GetOrderBy(SQLText, GetParserClass);
end;

function TCustomDADataSet.SQLSetOrderBy(const SQLText, Fields: string): string;
begin
  Result := _SetOrderBy(SQLText, Fields, GetParserClass);
end;

procedure TCustomDADataSet.AddWhere(const Condition: string);
begin
  CheckSQL;
  SaveModifiedSQL(SQLAddWhere(Trim(SQL.Text), Condition));
end;

procedure TCustomDADataSet.DeleteWhere;
begin
  CheckSQL;
  SaveModifiedSQL(SQLDeleteWhere(Trim(SQL.Text)));
end;

procedure TCustomDADataSet.SetOrderBy(const Fields: string);
begin
  CheckSQL;
  SaveModifiedSQL(SQLSetOrderBy(Trim(SQL.Text), Fields));
end;

function TCustomDADataSet.GetOrderBy: string;
begin
  CheckSQL;
  Result := SQLGetOrderBy(Trim(SQL.Text));
end;

/// GetBaseSQL returns original sql text with expanded macros.
function TCustomDADataSet.GetBaseSQL: string;
begin
  if FBaseSQL <> '' then
    Result := FBaseSQL
  else
    Result := SQL.Text;

  if Macros.Count > 0 then
    Macros.Expand(Result);
end;

procedure TCustomDADataSet.CheckSQL;
begin

end;

function TCustomDADataSet.GetFinalSQL: string;
var
  Str, Where: string;
  MasterName: string;
  DetailName: string;
  MasterPos: integer;
  DetailPos: integer;
begin
  Str := FCommand.FinalSQL;

  if FFilterSQL <> '' then
    Str := SQLAddWhere(Str, FilterSQL);

  if IsConnectedToMaster and not FOptions.LocalMasterDetail then begin
    MasterPos := 1;
    DetailPos := 1;
    Where := '';
    while True do begin
      MasterName := SQLInfo.NormalizeName(ExtractFieldName(FMasterFields, MasterPos), SQLInfo.ParamQuoteAllowed and Options.QuoteNames);
      DetailName := SQLInfo.NormalizeName(ExtractFieldName(FDetailFields, DetailPos), Options.QuoteNames);
      if (MasterName <> '') and (DetailName <> '') then begin
        if Where <> '' then
          Where := Where + ' AND ';
        if Options.MasterFieldsNullable then
          Where := Where + ' ( ( ' + DetailName + ' = :' + MasterName + ' ) OR ' +
                   '( ( ' + DetailName + ' IS NULL ) AND ( :' + MasterName + ' IS NULL ) ) )'
        else
          Where := Where + DetailName + ' = :' + MasterName;
      end
      else
        break;
    end;
    if Where <> '' then
      Str := SQLAddWhere(Str, Where);
  end;

  Result := Str;
end;

{$IFDEF FPC}
procedure TCustomDADataSet.DataEvent(Event: TDataEvent; Info: PtrInt);
{$ELSE}{$IFDEF VER16P}
procedure TCustomDADataSet.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TCustomDADataSet.DataEvent(Event: TDataEvent; Info: Longint);
{$ENDIF}{$ENDIF}
begin
  inherited DataEvent(Event, Info);

  case Event of
    // read-only field property may be changed
    deLayoutChange:
      FDataSetService.UpdateFieldsOptions;
    // design-time only for improving performance
    deFieldListChange:
      if (csDesigning in ComponentState) and (not FDesignCreate)  then begin
        if (not Active) and Options.DefaultValues and (FDataSetService <> nil) then begin
          if Data.Fields.Count = 0 then
            InternalInitFieldDefs;
          FDataSetService.FillFieldsDefaultValues;
        end;
      end
      else if Prepared and (Fields.Count > 0) and ((Fields.Count > Data.Fields.Count) or (Data.FindField(Fields[Fields.Count - 1].FieldName) = nil)) then
        TCRRecordSet(Data).ForceInitFieldsOnFetch := True;
  end;
end;

function TCustomDADataSet.GetNextRecord: Boolean;
var
  OldCurrentRecord: integer;
  Buffer: TRecordBuffer;
begin
  OldCurrentRecord := CurrentRecord + 1;
  Result := inherited GetNextRecord;
  if Result and (FUniDirectional or (FSmartFetchOptions.Enabled and FSmartFetchOptions.LiveBlock)) then
    if (OldCurrentRecord > 0) and (OldCurrentRecord >= BufferCount) then begin
      Buffer := IntPtr(Buffers[CurrentRecord + 1]);
      if Buffer <> nil then
        FreeRefComplexFields(Buffer);
    end;
end;

function TCustomDADataSet.GetPriorRecord: Boolean;
var
  Buffer: TRecordBuffer;
begin
  Result := inherited GetPriorRecord;
  if Result and FSmartFetchOptions.Enabled and FSmartFetchOptions.LiveBlock then
    if CurrentRecord = 0 then begin
      Buffer := IntPtr(Buffers[BufferCount]);
      if Buffer <> nil then
        FreeRefComplexFields(Buffer);
    end;
end;

procedure TCustomDADataSet.SetUpdateObject(Value: TCustomDAUpdateSQL);
begin
  if Value <> nil then begin
    Value.CheckUpdateComponent(Value.ModifyObject, Self);
    Value.CheckUpdateComponent(Value.InsertObject, Self);
    Value.CheckUpdateComponent(Value.DeleteObject, Self);
    Value.CheckUpdateComponent(Value.RefreshObject, Self);
  end;
  if Value <> FUpdateObject then begin
    if Assigned(FUpdateObject) and (FUpdateObject.DataSet = Self) then
      FUpdateObject.DataSet := nil;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then begin
      if Assigned(FUpdateObject.DataSet) and
        (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := nil;
      FUpdateObject.DataSet := Self;
    end;
  end;
end;

procedure TCustomDADataSet.SetOptions(Value: TDADataSetOptions);
begin
  FOptions.Assign(Value);
end;

procedure TCustomDADataSet.SetSmartFetchOptions(Value: TSmartFetchOptions);
begin
  FSmartFetchOptions.Assign(Value);
end;

procedure TCustomDADataSet.SetFilterSQL(const Value: string);
var
  OldFilter: string;
  OldActive: boolean;
begin
  if Value <> FFilterSQL then begin
    OldFilter := FFilterSQL;
    FFilterSQL := Trim(Value);

    OldActive := Active;
    if not (csReading in ComponentState) then begin
      Close;
      UnPrepare;
    end;

    AssembleSQL;

    if OldActive then
      try
        Open;
        if Filtered and (Filter <> '') then begin
          Data.FilterUpdated;
          Resync([]);
          First;
        end;
      except
        FFilterSQL := OldFilter;
        AssembleSQL;
        raise;
      end;
  end;
end;

procedure TCustomDADataSet.SetFiltered(Value: boolean);
var
  KeyFieldDescs: TFieldDescArray;
  KeyFields: TFieldArray;
  Values: variant;
begin
  if Value <> Filtered then begin
    if Active then
      GetCurrentKeysAndValues(KeyFieldDescs, KeyFields, Values);

    inherited;

    if Active and (Length(KeyFields) <> 0) and not IsEmpty then
      LocateEx(KeyFields, KeyFieldDescs, Values, []);
  end;
end;

function TCustomDADataSet.LocateRecord(KeyFields: TList; KeyValues: variant;
  Options: TLocateExOptions; SavePos: boolean): boolean;
begin
  if NonBlocking then
    TCRRecordSet(Data).WaitForFetch;

  Result := inherited LocateRecord(KeyFields, KeyValues, Options, SavePos);
end;

function TCustomDADataSet.LocateEx(const KeyFields: array of TField;
  const KeyFieldDescs: array of TCRFieldDesc; const KeyValues: variant;
  Options: TLocateExOptions): boolean;
var
  Fields: TList;
  i: integer;
begin
  DoBeforeScroll;

  CheckActive;

  Fields := TList.Create;
  try
    for i := 0 to Length(KeyFields) - 1 do begin
      if KeyFields[i] <> nil then
        Fields.Add(GetFieldDesc(KeyFields[i]))
      else
        if KeyFieldDescs[i] <> nil then
          Fields.Add(KeyFieldDescs[i]);
    end;

    Result := LocateRecord(Fields, KeyValues, Options, False);
  finally
    Fields.Free;
  end;

  if Result then begin
    if LocateExOldBehavior or Data.Eof or Data.Bof then
      Resync([{rmExact, rmCenter}])
    else
      Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TCustomDADataSet.EmptyTable(const TableName: string);
begin
  if TableName = '' then
    DatabaseError(STableNameNotDefined);

  CheckIRecordSet;
  CheckDataSetService;
  UsedConnection.ExecSQL(FDataSetService.SQLGenerator.GenerateEmptyTableSQL(SQLInfo.NormalizeName(TableName, Options.QuoteNames)));

  if Active then
    Refresh;
end;

procedure TCustomDADataSet.SetConditions(Value: TDAConditions);
begin
  Assert(FWhereConditions <> nil);
  FWhereConditions.AssignTo(Value);
end;

function TCustomDADataSet.GetCursor: TCRCursor;
begin
  if FICommand <> nil then
    Result := FICommand.GetCursor
  else
    Result := nil;
end;

function TCustomDADataSet.GetCRCursor: TCRCursor;
begin
  Result := GetCursor;
end;

procedure TCustomDADataSet.SetCRCursor(Value: TCRCursor);
begin
  if Value <> GetCursor then begin
    CheckConnection;  //We doesn't increase FConnectCount using this funct
    Close;
    UnPrepare;
    FieldDefs.Updated := False;
    CheckIRecordSet;

    FICommand.SetCursor(Value);
    FICommand.CommandType := ctCursor;
  end;
end;

procedure TCustomDADataSet.SetEncryption(Value: TDAEncryption);
begin
  FEncryption.Assign(Value);
end;

procedure TCustomDADataSet.SetCheckMode(Value: TCheckMode);
begin
  FCheckMode := Value;
  if (FDataSetService <> nil) and (FDataSetService.Updater <> nil) then
    FDataSetService.Updater.CheckMode := Value;
end;

procedure TCustomDADataSet.SetIndexFieldNames(const Value: string);
begin
  if (Value <> '') and FSmartFetchOptions.Enabled and FSmartFetchOptions.LiveBlock then
    DatabaseError(SIndexFieldNamesWithLiveBlock, Self);

  inherited;
end;

{ TDACondition }

constructor TDACondition.Create(Collection: TCollection);
begin
  inherited;

  FEnabled := True;
end;

procedure TDACondition.Enable;
begin
  Enabled := True;
end;

procedure TDACondition.Disable;
begin
  Enabled := False;
end;

procedure TDACondition.SetValue(const Value: string);
begin
  if FValue <> Value then begin
    FValue := Value;

    if TDAConditions(Collection).FEnabled then
      TDAConditions(Collection).Enable;
  end;
end;

procedure TDACondition.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;

    if TDAConditions(Collection).FEnabled then
      TDAConditions(Collection).Enable;
  end;
end;

procedure TDACondition.SetName(const Value: string);
begin
  if FName <> Value then
    FName := Value;
end;

procedure TDACondition.AssignTo(Dest: TPersistent);
begin
  if Dest is TDACondition then begin
    TDACondition(Dest).FName := FName;
    TDACondition(Dest).FValue := FValue;
    TDACondition(Dest).FEnabled := FEnabled;
  end
  else
    inherited;
end;

{ TDAConditions }

constructor TDAConditions.Create(Owner: TPersistent);
begin
  inherited Create(TDACondition);

  FOwner := Owner;

  FBaseSQL := '';
  FEnabled := False;
  FCurrentEnabledText := '';
end;

function TDAConditions.GetItem(Index: integer): TDACondition;
begin
  Result := TDACondition(inherited Items[Index]);
end;

procedure TDAConditions.SetItem(Index: Integer; Value: TDACondition);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

function TDAConditions.GetWhereSQL: string;
var
  i, k: Integer;
begin
  Result := '';
  k := Pred(Count);
  if k >= 0 then begin
    for i := 0 to k do
      if (Condition[i].FEnabled) and (Condition[i].FValue <> '') then
        Result := Result + '(' + Condition[i].Value + ')' + DALineSeparator + 'AND' + DALineSeparator;
    SetLength(Result, Length(Result) - (Length(DALineSeparator) * 2 + 3));
  end;
end;

procedure TDAConditions.RestoreBaseSQL;
begin
  if TCustomDADataSet(FOwner).SQL.Text <> FBaseSQL then
    TCustomDADataSet(FOwner).SQL.Text := FBaseSQL;
end;

procedure TDAConditions.AssignParams(ParamsSrc: TDAParams; ParamsDst: TDAParams);
begin
  //rm - Bug #132896 - reset the value of the parameter used in TDACondition when a new TDACondition is added
  if (ParamsSrc <> nil) and (ParamsDst <> nil) then begin
    if ParamsDst.Count = 0 then
      ParamsDst.Assign(ParamsSrc); //save params value before executing RestoreBaseSQL
    ParamsDst.AssignValues(ParamsSrc); //if ParamsDst.Count <> 0 - restore only params values that were set before executing RestoreBaseSQL
  end;
end;

procedure TDAConditions.SetText(const Value: string);
var
  i: integer;
  lst: TStrings;
  Parser: TParser;
  s: string;
begin
  Clear;
  if Value = '' then exit;

  lst := TStringList.Create;
  try
    lst.Text := Value;
    for i := 0 to lst.Count - 1 do begin
      s := lst[i];
      Parser := TParser.Create(s);
      try
        if Parser.ToLexem(lxEqual) <> lcEnd then begin
          Add(Copy(s, 1, Parser.PrevPos), copy(s, Parser.CurrPos + 1, Length(s)), True);
        end;
      finally
        Parser.Free;
      end;
    end;
  finally
    lst.Free;
  end;
end;

function TDAConditions.GetText: string;
var
  i: integer;
  lst: TStrings;
  Cd: TDACondition;
begin
  Result := '';
  if Count = 0 then exit;

  lst := TStringList.Create;
  try
    for i := 0 to Count - 1 do begin
      Cd := GetItem(i);
      if Assigned(cd) then
        lst.Add(cd.Name + '=' + cd.Value);
    end;
    Result := lst.Text;
  finally
    lst.Free;
  end;
end;

procedure TDAConditions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAConditions then begin
    TDAConditions(Dest).FBaseSQL := FBaseSQL;
    TDAConditions(Dest).FEnabled := FEnabled;
    TDAConditions(Dest).FCurrentEnabledText := FCurrentEnabledText;
    TDAConditions(Dest).Assign(Self)
  end
  else
    inherited;
end;

function TDAConditions.IndexOf(const Name: string): Integer;
var
  i: integer;
begin
  Result := -1;
  if Name <> '' then
    for i := 0 to Count - 1 do
      if (Condition[i] <> nil) then
        if SameText(Condition[i].FName, Name) then begin
          Result := i;
          break;
        end;
end;

function TDAConditions.Find(const Name: string): TDACondition;
var
  i: integer;
begin
  i := IndexOf(Name);
  if i >= 0 then
    Result := Condition[i]
  else
    Result := nil;
end;

function TDAConditions.Get(const Name: string): TDACondition;
begin
  Result := Find(Name);

  if Result = nil then
    raise Exception.Create(Format(SConditionNotFound, [Name]));
end;

function TDAConditions.Add(const Name, Value: string; Enabled: Boolean): TDACondition;
var
  Condition: TDACondition;
begin
  Condition := Find(Name);
  if Condition = nil then
    Condition := TDACondition.Create(Self);

  Condition.FName := Name;
  Condition.FValue := Value;
  Condition.FEnabled := Enabled;
  Result := Condition;
  if FEnabled then
    Enable;
end;

function TDAConditions.Add(const Value: string; Enabled: Boolean): TDACondition;
begin
  Result := Add('', Value, Enabled);
end;

procedure TDAConditions.Delete(Index: integer);
begin
  inherited Delete(Index);

  if FEnabled then
    Enable;
end;

procedure TDAConditions.Remove(const Name: string);
begin
  Delete(IndexOf(Name));
end;

procedure TDAConditions.Clear;
begin
  inherited Clear;

  if FEnabled then
    Enable;
end;

procedure TDAConditions.Enable;
var
  AEnabledText: string;
  OldActive: Boolean;
  DataSetParams, Params: TDAParams;
begin
  AEnabledText := WhereSQL;
  Params := TDAParams.Create;
  DataSetParams := nil;
  try
    if not FEnabled or (AEnabledText <> FCurrentEnabledText) then begin
      FCurrentEnabledText := AEnabledText;
      OldActive := TCustomDADataSet(FOwner).Active;
      if FBaseSQL = '' then
        FBaseSQL := TCustomDADataSet(FOwner).SQL.Text
      else begin
        DataSetParams := TDBAccessUtils.GetParams(TCustomDADataSet(FOwner));
        AssignParams(DataSetParams, Params);
        RestoreBaseSQL;
      end;
      if AEnabledText <> '' then begin
        TCustomDADataSet(FOwner).AddWhere(AEnabledText);
        AssignParams(Params, DataSetParams);
      end;
      if OldActive then
        TCustomDADataSet(FOwner).Open;
    end;
    FEnabled := True;
  finally
    Params.Clear;
    Params.Free;
  end;
end;

procedure TDAConditions.Disable;
var
  OldActive: Boolean;
begin
  if FEnabled then begin
    OldActive := TCustomDADataSet(FOwner).Active;
    RestoreBaseSQL;
    FBaseSQL := '';
    FEnabled := False;
    FCurrentEnabledText := '';
    if OldActive then
      TCustomDADataSet(FOwner).Open;
  end;
end;

{ TDAEncryption }

constructor TDAEncryption.Create(Owner: TCustomDADataSet);
begin
  inherited Create;

  FOwner := Owner;
end;

procedure TDAEncryption.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAEncryption then begin
    TDAEncryption(Dest).Encryptor := Encryptor;
    TDAEncryption(Dest).Fields := Fields;
  end
  else
    inherited;
end;

function TDAEncryption.IsFieldEncrypted(FieldDesc: TFieldDesc): boolean;
var
  RecBuf: TRecordBuffer;
begin
  if (FieldDesc <> nil) and FOwner.GetActiveRecBuf(RecBuf) then
    Result := FOwner.FIRecordSet.GetEncrypted(FieldDesc, RecBuf)
  else
    Result := False;
end;

function TDAEncryption.IsFieldEncrypted(const FieldName: string): boolean;
var
  FieldDesc: TFieldDesc;
begin
  if FOwner.FIRecordSet <> nil then begin
    FieldDesc := FOwner.GetFieldDesc(FieldName);
    Result := IsFieldEncrypted(FieldDesc);
  end
  else
    Result := False;
end;

function TDAEncryption.IsFieldEncrypted(FieldNo: Integer): boolean;
var
  FieldDesc: TFieldDesc;
begin
  if FOwner.FIRecordSet <> nil then begin
    FieldDesc := FOwner.GetFieldDesc(FieldNo);
    Result := IsFieldEncrypted(FieldDesc);
  end
  else
    Result := False;
end;

procedure TDAEncryption.EncryptDataSet(AutoCommitExplicitTransaction: Boolean = True);
var
  Bookmark: TBookmark;
  tr: TDATransaction;
  ImplicitStart, OldUpdateAllFields: boolean;
  i: integer;
begin
  FOwner.CheckActive;
  if FOwner.State <> dsBrowse then
    DatabaseErrorFmt(SDataSetMustBeInState, [GetEnumName(TypeInfo(TDataSetState), integer(dsBrowse))], FOwner);
  if FOwner.UpdatesPending then
    DatabaseError(SDataSetMustNotHavePendingUpdates, FOwner);
  if FOwner.Encryption.Encryptor = nil then
    DatabaseError(SEncryptorNotDefined, FOwner);
  if FOwner.Encryption.Fields = '' then
    DatabaseError(SEncrFieldsNotDefined, FOwner);
  Bookmark := nil;
  try
    Bookmark := FOwner.GetBookmark;
    FOwner.DisableControls;
    OldUpdateAllFields := FOwner.Options.UpdateAllFields;
    tr := FOwner.UsedUpdateTransaction;
    try
      ImplicitStart := not tr.Active;
      if ImplicitStart then
        tr.StartTransaction;
      try
        FOwner.Options.UpdateAllFields := True;
        FOwner.First;
        while not FOwner.Eof do begin
          FOwner.Edit;
          for i := 0 to FOwner.FieldCount - 1 do
            if FOwner.Fields[i].CanModify then
              FOwner.Fields[i].Value := FOwner.Fields[i].Value;
          FOwner.Post;
          FOwner.Next;
        end;
        if FOwner.CachedUpdates then
          FOwner.ApplyUpdates;
        if ImplicitStart or AutoCommitExplicitTransaction then begin
          if not FOwner.Connection.IsMultipleTransactionsSupported then
            tr.Commit
          else
            tr.DoCommitRetaining;
          if FOwner.CachedUpdates then
            FOwner.CommitUpdates;
        end;
      except
        if FOwner.CachedUpdates then
          FOwner.CancelUpdates;
        if not FOwner.Connection.IsMultipleTransactionsSupported or ImplicitStart then
          tr.Rollback
        else
          tr.DoRollbackRetaining;
        raise;
      end;
    finally
      FOwner.Options.UpdateAllFields := OldUpdateAllFields;
    end;
  finally
    FOwner.GotoBookmark(Bookmark);
    FOwner.FreeBookmark(Bookmark);
    FOwner.EnableControls;
  end;
end;

procedure TDAEncryption.SetEncryptor(Value: TCREncryptor);
begin
  if FEncryptor <> Value then begin
    if FEncryptor <> nil then
      FEncryptor.RemoveFreeNotification(FOwner);

    FOwner.Disconnect;
    FEncryptor := Value;

    if Value <> nil then
      FEncryptor.FreeNotification(FOwner);

    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.Encryptor := FEncryptor;
  end;
end;

procedure TDAEncryption.SetFields(const Value: string);
begin
  if FFields <> Value then begin
    FOwner.CheckInactive;
    FFields := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prEncryptedFields, FFields);
  end;
end;

{ TDADataSetOptions }

constructor TDADataSetOptions.Create(Owner: TCustomDADataSet);
begin
  inherited Create;

  FOwner := Owner;

  SetFieldsReadOnly := True;
  RequiredFields := True;
  StrictUpdate := True;
  TrimFixedChar := True;
  LongStrings := True;
  FlatBuffers := False;
  RemoveOnRefresh := True;
  UpdateBatchSize := 1;
  MasterFieldsNullable := False;
  FieldOrigins := foNone;
end;

procedure TDADataSetOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDADataSetOptions then begin
    TDADataSetOptions(Dest).SetFieldsReadOnly := SetFieldsReadOnly;
    TDADataSetOptions(Dest).RequiredFields := RequiredFields;
    TDADataSetOptions(Dest).StrictUpdate := StrictUpdate;
    TDADataSetOptions(Dest).NumberRange := NumberRange;
    TDADataSetOptions(Dest).QueryRecCount := QueryRecCount;
    TDADataSetOptions(Dest).AutoPrepare := AutoPrepare;
    TDADataSetOptions(Dest).ReturnParams := ReturnParams;
    TDADataSetOptions(Dest).TrimFixedChar := TrimFixedChar;
    TDADataSetOptions(Dest).LongStrings := LongStrings;
    TDADataSetOptions(Dest).FlatBuffers := FlatBuffers;
    TDADataSetOptions(Dest).RemoveOnRefresh := RemoveOnRefresh;
    TDADataSetOptions(Dest).QuoteNames := QuoteNames;
    TDADataSetOptions(Dest).DetailDelay := DetailDelay;
  {$IFDEF HAVE_COMPRESS}
    TDADataSetOptions(Dest).CompressBlobMode := CompressBlobMode;
  {$ENDIF}
    TDADataSetOptions(Dest).LocalMasterDetail := LocalMasterDetail;
    TDADataSetOptions(Dest).CacheCalcFields := CacheCalcFields;
    TDADataSetOptions(Dest).FieldOrigins := FieldOrigins;
    TDADataSetOptions(Dest).DefaultValues := DefaultValues;
    TDADataSetOptions(Dest).UpdateBatchSize := UpdateBatchSize;
    TDADataSetOptions(Dest).UpdateAllFields := UpdateAllFields;

    TDADataSetOptions(Dest).FullRefresh := FullRefresh;
    TDADataSetOptions(Dest).TrimVarChar := TrimVarChar;
    TDADataSetOptions(Dest).SetEmptyStrToNull := SetEmptyStrToNull;
    TDADataSetOptions(Dest).PrepareUpdateSQL := PrepareUpdateSQL;
    TDADataSetOptions(Dest).ExtendedFieldsInfo := ExtendedFieldsInfo;
    TDADataSetOptions(Dest).EnableBCD := EnableBCD;
    TDADataSetOptions(Dest).EnableFMTBCD := EnableFMTBCD;
    TDADataSetOptions(Dest).FMasterFieldsNullable := MasterFieldsNullable;
    TDADataSetOptions(Dest).InsertAllSetFields := InsertAllSetFields;
  end
  else
    inherited;
end;

procedure TDADataSetOptions.SetSetFieldsReadOnly(Value: boolean);
begin
  if FSetFieldsReadOnly <> Value then begin
    FSetFieldsReadOnly := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prSetFieldsReadOnly, Value);
  end;
end;

procedure TDADataSetOptions.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);;
  Filer.DefineProperty('FieldsOrigin', ReadFieldsOriginal, nil, False);
end;

procedure TDADataSetOptions.ReadFieldsOriginal(Reader: TReader);
var
  EnableFieldsOriginal: boolean;
begin
  EnableFieldsOriginal := Reader.ReadBoolean;
  if EnableFieldsOriginal then
    FieldOrigins := foTableAndField
  else
    FieldOrigins := foNone;
end;

procedure TDADataSetOptions.SetFullRefresh(Value: boolean);
begin
  if FFullRefresh <> Value then begin
    FFullRefresh := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prFullRefresh, Value);
  end;
end;

procedure TDADataSetOptions.SetRequiredFields(Value: boolean);
begin
  if FRequiredFields <> Value then begin
    FRequiredFields := Value;
    FOwner.FLocalConstraints := FRequiredFields;
    FOwner.FieldDefs.Updated := False;
    if FOwner.Active then // for change RequiredFields in runtime
      FOwner.FieldDefs.Update;
  end;
end;

procedure TDADataSetOptions.SetNumberRange(Value: boolean);
begin
  FNumberRange := Value;
  FOwner.FNumberRange := FNumberRange;
end;

procedure TDADataSetOptions.SetTrimFixedChar(Value: boolean);
begin
  FTrimFixedChar := Value;
  if FOwner.Data <> nil then
    FOwner.Data.TrimFixedChar := FTrimFixedChar;
end;

procedure TDADataSetOptions.SetTrimVarChar(Value: boolean);
begin
  FTrimVarChar := Value;
  if FOwner.Data <> nil then
    FOwner.Data.TrimVarChar := FTrimVarChar;
end;

procedure TDADataSetOptions.SetSetEmptyStrToNull(Value: boolean);
begin
  FSetEmptyStrToNull := Value;
  if FOwner.Data <> nil then
    FOwner.Data.SetEmptyStrToNull := FSetEmptyStrToNull;
end;

procedure TDADataSetOptions.SetLongStrings(Value: boolean);
begin
  FOwner.CheckInactive;
  FLongStrings := Value;
  if FOwner.FIRecordSet <> nil then
    FOwner.FIRecordSet.SetProp(prLongStrings, FLongStrings);
  FOwner.FieldDefs.Updated := False;
end;

procedure TDADataSetOptions.SetAutoPrepare(Value: boolean);
begin
  if FAutoPrepare = Value then
    Exit;

  FOwner.Unprepare;
  FAutoPrepare := Value;
end;

procedure TDADataSetOptions.SetFlatBuffers(Value: boolean);
begin
  FOwner.CheckInactive;
  FFlatBuffers := Value;
  if FOwner.FIRecordSet <> nil then
    FOwner.FIRecordSet.SetProp(prFlatBuffers, Value);
end;

function TDADataSetOptions.GetDetailDelay: integer;
begin
  Result := FOwner.DetailDelay;
end;

procedure TDADataSetOptions.SetDetailDelay(Value: integer);
begin
  FOwner.DetailDelay := Value;
end;

procedure TDADataSetOptions.SetLocalMasterDetail(Value: boolean);
begin
  if Value <> FLocalMasterDetail then begin
    FOwner.CheckInactive;
    FLocalMasterDetail := Value;
    FOwner.SetLocalDetailFilter;

    if Value then
      FOwner.AssembleSQL;
  end;
end;

function TDADataSetOptions.GetCacheCalcFields: boolean;
begin
  Result := FOwner.FCacheCalcFields;
end;

procedure TDADataSetOptions.SetCacheCalcFields(Value: boolean);
begin
  if CacheCalcFields <> Value then begin
    FOwner.CheckInactive;
    FOwner.FCacheCalcFields := Value;
  end;
end;

procedure TDADataSetOptions.SetQuoteNames(Value: boolean);
begin
  if QuoteNames <> Value then begin
    FQuoteNames := Value;
    if FOwner.FICommand <> nil then begin
      FOwner.FICommand.SetProp(prQuoteNames, Value);
      // Refresh Master-Detail link
      FOwner.MDPropertiesChanged;
    end;
  end;
end;

{$IFDEF HAVE_COMPRESS}
procedure TDADataSetOptions.SetCompressBlobMode(Value: TCompressBlobMode);
begin
  if FCompressBlobMode <> Value then begin
    TCustomDADataSet(FOwner).CheckInactive;
    FCompressBlobMode := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prCompressBlobMode, Integer(Value));
    FOwner.FieldDefs.Updated := False;
  end;
end;
{$ENDIF}

procedure TDADataSetOptions.SetFieldOrigins(Value: TFieldOrigins);
begin
  if FFieldOrigins <> Value then begin
    FOwner.CheckInactive;
    FFieldOrigins := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prFieldOrigins, Value);
  end;
end;

function TDADataSetOptions.GetFieldsOrigin: boolean;
begin
  if FFieldOrigins = foNone then
    Result := False
  else
    Result := True;
end;

procedure TDADataSetOptions.SetFieldsOrigin(Value: boolean);
begin
  if not Value then
    FFieldOrigins := foNone
  else if FFieldOrigins = foNone then
    FFieldOrigins := foTableAndField;
end;

procedure TDADataSetOptions.SetDefaultValues(Value: boolean);
begin
  if FDefaultValues <> Value then begin
    FOwner.CheckInactive;
    FDefaultValues := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prDefaultValues, Value);
  end;
end;

procedure TDADataSetOptions.SetExtendedFieldsInfo(Value: boolean);
begin
  if FExtendedFieldsInfo <> Value then begin
    FOwner.CheckInactive;
    FExtendedFieldsInfo := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prExtendedFieldsInfo, Value);
  end;
end;

procedure TDADataSetOptions.SetEnableBCD(Value: boolean);
begin
  if FEnableBCD <> Value then begin
    FOwner.CheckInactive;
    FEnableBCD := Value;
    if FOwner.FICommand <> nil then
      FOwner.FICommand.SetProp(prEnableBCD, Value);
  end;
end;

procedure TDADataSetOptions.SetEnableFMTBCD(Value: boolean);
begin
  if FEnableFMTBCD <> Value then begin
    FOwner.CheckInactive;
    FEnableFMTBCD := Value;
    if FOwner.FICommand <> nil then
      FOwner.FICommand.SetProp(prEnableFmtBCD, Value);
  end;
end;

procedure TDADataSetOptions.SetMasterFieldsNullable(Value: Boolean);
begin
  if Value <> FMasterFieldsNullable then begin
    FMasterFieldsNullable := Value;
    FOwner.MDLinksRefreshed(nil);
  end;
end;

procedure TDADataSetOptions.SetInsertAllSetFields(Value: boolean);
begin
  if FInsertAllSetFields <> Value then begin
    FOwner.CheckInactive;
    FInsertAllSetFields := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prInsertAllSetFields, Value);
  end;
end;

{ TSmartFetchOptions }

constructor TSmartFetchOptions.Create(Owner: TCustomDADataSet);
begin
  inherited Create;

  FOwner := Owner;
  FSQLGetKeyValues := TStringList.Create;
  FSQLGetDataValues := TStringList.Create;
  FLiveBlock := True;
end;

destructor TSmartFetchOptions.Destroy;
begin
  FSQLGetKeyValues.Free;
  FSQLGetDataValues.Free;

  inherited;
end;

procedure TSmartFetchOptions.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FOwner.CheckInactive;

    if (FOwner.IndexFieldNames <> '') and Value and FLiveBlock then
      DatabaseError(SIndexFieldNamesWithLiveBlock, FOwner);

    FEnabled := Value;
    if FOwner.FIRecordSet <> nil then
      if Value then
        FOwner.FIRecordSet.SmartFetchState := sfMetaInfo
      else
        FOwner.FIRecordSet.SmartFetchState := sfNone;
  end;
end;

procedure TSmartFetchOptions.SetLiveBlock(Value: Boolean);
begin
  if FLiveBlock <> Value then begin
    FOwner.CheckInactive;

    if (FOwner.IndexFieldNames <> '') and Value then
      DatabaseError(SIndexFieldNamesWithLiveBlock, FOwner);

    FLiveBlock := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prLiveBlockOnSmartFetch, Value);
  end;
end;

procedure TSmartFetchOptions.SetPrefetchedFields(const Value: string);
begin
  if FPrefetchedFields <> Value then begin
    FOwner.CheckInactive;
    FPrefetchedFields := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prPrefetchedFields, Value);
  end;
end;

procedure TSmartFetchOptions.SetSQLGetKeyValues(Value: TStrings);
begin
  FSQLGetKeyValues.Assign(Value);
end;

///procedure TSmartFetchOptions.SetSQLGetDataValues(Value: TStrings);
///begin
///  FSQLGetDataValues.Assign(Value);
///end;

procedure TSmartFetchOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TSmartFetchOptions then begin
    TSmartFetchOptions(Dest).Enabled := FEnabled;
    TSmartFetchOptions(Dest).LiveBlock := FLiveBlock;
    TSmartFetchOptions(Dest).PrefetchedFields := FPrefetchedFields;
    TSmartFetchOptions(Dest).FSQLGetKeyValues.Assign(FSQLGetKeyValues);
    TSmartFetchOptions(Dest).FSQLGetDataValues.Assign(FSQLGetDataValues);
  end
  else
    inherited;
end;

{ TDADataSetUpdater }

constructor TDADataSetUpdater.Create(AOwner: TDataSetService);
begin
  FDataSetService := TDADataSetService(AOwner);
  Assert(FDataSetService <> nil);
  FDataSet := TCustomDADataSet(FDataSetService.FDataSet);

  inherited Create(AOwner);

  FBatchSQLs := StringBuilder.Create(100);
  FCheckMode := cmException;
  FParamsInfo := TDAParamsInfo.Create(TDAParamInfo);
end;

destructor TDADataSetUpdater.Destroy;
var
  stIdx: TStatementType;
begin
  FParamsInfo.Free;
  FBatchParams.Free;
  FBatchSQLs.Free;

  for stIdx := Low(FUpdateComponents) to High(FUpdateComponents) do begin
    FUpdateComponents[stIdx].Free;
    FUpdateComponents[stIdx] := nil;
  end;

  inherited;
end;

function TDADataSetUpdater.UseParamType(Param: TDAParam): boolean;
begin
  Result := False;
end;

procedure TDADataSetUpdater.SetUpdateQuery(Value: TComponent);
begin
  FUpdateQuery := Value;

  if Value is TCustomDADataSet then begin
    TCustomDADataSet(Value).CheckIRecordSet;
    FDataSetService.SQLGenerator.IUpdateRecordSet := TCustomDADataSet(Value).FIRecordSet;
  end
  else
    FDataSetService.SQLGenerator.IUpdateRecordSet := nil;
end;

procedure TDADataSetUpdater.SetDefaultParamType(Param: TDAParam);
begin
  Param.ParamType := ptInput;
end;

function TDADataSetUpdater.NeedReturnParams: boolean;
begin
  Result := FDataSet.Options.ReturnParams or FDataSet.FDMLRefresh;
end;

function TDADataSetUpdater.ReturnParamsAsFields: boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.UseSQLGeneratorParams(const StatementTypes: TStatementTypes): boolean;
begin
  Result := ([stRefreshQuick, stRefreshCheckDeleted] * StatementTypes = []) and
    not ((stRefresh in StatementTypes) and ((FDataSet.Params.Count > 0) or FDataSet.Options.FullRefresh));
end;

function TDADataSetUpdater.GetSQLSeparator: string;
begin
  Result := ';';
end;

function TDADataSetUpdater.RefreshAfterInsertAllowed: boolean;
begin
  Result := not FDataSet.FDMLRefresh or FDataSet.Options.FullRefresh;
end;

function TDADataSetUpdater.IsNeedInsertPreconnect: boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.IsNeedEditPreconnect: boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.IsPreconnected: boolean;
begin
  Result := ((FDataSet.State = dsEdit) and IsNeedEditPreconnect) or
    ((FDataSet.State = dsInsert) and IsNeedInsertPreconnect);
end;

function TDADataSetUpdater.RefreshByLockAllowed: boolean;
begin
  Result := True;
end;

function TDADataSetUpdater.CanRefreshByLock: boolean;
var
  SQLLock: string;
  SQLRefresh: string;
begin
  if RefreshByLockAllowed then begin
    SQLLock := GetUpdateStatement(stLock);
    SQLRefresh := GetUpdateStatement(stRefresh);
    Result := not (  ((SQLLock <> '') and (Pos('FOR', UpperCase(SQLLock)) <> 1)) or
     (roBeforeEdit in FDataSet.RefreshOptions) and ((SQLRefresh <> '') or FDataSet.Options.FullRefresh)  );
  end
  else
    Result := False;
end;

procedure TDADataSetUpdater.SetIdentityFieldValue();
var
  Value: variant;
  OldReadOnly: boolean;
  Field: TField;
  RecBuf: TRecordBuffer;
begin
  if (FDataSetService.IdentityField <> nil) and GetIdentityFieldValue(Value) then begin
    Field := FDataSet.GetField(FDataSetService.IdentityField);
    if Field = nil then begin
      RecBuf := FDataSet.GetNewRecBuf;
      if RecBuf <> nil then
        FDataSet.Data.PutFieldAsVariant(FDataSetService.IdentityField, RecBuf, Value);
    end
    else begin
      OldReadOnly := Field.ReadOnly;
      try
        FDataSet.SetTempState(FDataSet.State); // DisableControls
        Field.ReadOnly := False;
        Field.NewValue := Value;
      finally
        Field.ReadOnly := OldReadOnly;
        FDataSet.RestoreState(FDataSet.State);
      end;
    end;
  end;
end;

function TDADataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.GetSavepointName(CachedUpdates: boolean): string;
const
  LockPrefix = 'LOCK_';
  LockCachedUpdatesPrefix = 'LOCK_CU_';
  MaxLen = 30; // 30 - max length of Oracle ident
begin
  if CachedUpdates then
    Result := LockCachedUpdatesPrefix + FDataSet.Name
  else
    Result := LockPrefix + FDataSet.Name;

  if Length(Result) > MaxLen then
    Result := Copy(Result, 1, MaxLen);
end;

function TDADataSetUpdater.GetLockSavepointName: string;
begin
  Result := GetSavepointName(False);
end;

function TDADataSetUpdater.GetLockCachedUpdatesSavepointName: string;
begin
  Result := GetSavepointName(True);
end;

function TDADataSetUpdater.SavepointAllowed: boolean;
begin
  Result := True;
end;

procedure TDADataSetUpdater.SetSavepoint(SavepointName: string; CachedUpdates: boolean);
var
  UpdTransaction: TDATransaction;
begin
  UpdTransaction := UsedUpdateTransaction;
  if not UpdTransaction.Active then begin
    UpdTransaction.StartTransaction;
    if CachedUpdates then
      FLockTrStarted := ltsOnLockCachedUpdates
    else
      FLockTrStarted := ltsOnLock;
  end
  else begin
    if SavepointAllowed then
      UpdTransaction.DoSavepoint(SavepointName);
    if not FDataSet.CachedUpdates then
      FLockTrStarted := ltsNone
    else if CachedUpdates then
      FLockTrStarted := ltsBeforeLockCachedUpdates;
  end;

  FFixedTransaction := UpdTransaction;
end;

procedure TDADataSetUpdater.SetLockSavepoint;
begin
  SetSavepoint(GetLockSavepointName, False);
end;

procedure TDADataSetUpdater.SetLockCachedUpdatesSavepoint;
begin
  SetSavepoint(GetLockCachedUpdatesSavepointName, True);
end;

procedure TDADataSetUpdater.RollbackToSavepoint(SavepointName: string; CachedUpdates: boolean);
begin
  if FFixedTransaction = nil then
    exit;
  try
    if not CachedUpdates and (FLockTrStarted = ltsOnLock) then begin
      FFixedTransaction.Rollback;
      FLockTrStarted := ltsNone;
    end
    else if CachedUpdates and (FLockTrStarted = ltsOnLockCachedUpdates) then begin
      FFixedTransaction.Rollback;
      FLockTrStarted := ltsNone;
    end
    else begin
      if SavepointAllowed then begin
        FFixedTransaction.DoRollbackToSavepoint(SavepointName);
        if CachedUpdates and (FLockTrStarted = ltsBeforeLockCachedUpdates) then
          FLockTrStarted := ltsNone;
      end
      else begin
        if UsedConnection.IsMultipleTransactionsSupported then
          FFixedTransaction.DoRollbackRetaining
        else
          FFixedTransaction.Rollback;
        FLockTrStarted := ltsNone;
      end;
    end;
  except  // WAR for COMMIT after Lock call
  end;

  if not FDataSet.CachedUpdates or (FLockTrStarted = ltsNone) then
    FFixedTransaction := nil;
end;

procedure TDADataSetUpdater.RollbackToLockSavepoint;
begin
  RollbackToSavepoint(GetLockSavepointName, False);
end;

procedure TDADataSetUpdater.RollbackToLockCachedUpdatesSavepoint;
begin
  RollbackToSavepoint(GetLockCachedUpdatesSavepointName, True);
end;

procedure TDADataSetUpdater.ResetLockCachedUpdatesSavepoint;
begin
  // Reset transaction info on ApplyChanges
  FFixedTransaction := nil;
  FLockTrStarted := ltsNone;
end;

function TDADataSetUpdater.FieldByParamName(var ParamName: string; out Old: boolean; out AFieldNo: integer; out Master: Boolean): TField;
var
  e: integer;
  ds: TDataSet;
  pn: string;

  // Returns field that corresponds to ParamName
  function FindFieldByFieldNo(FieldNo: integer): TField;
  var
    i: integer;
  begin
    for i := 0 to ds.Fields.Count - 1 do begin
      Result := ds.Fields[i];
      if Result.FieldNo = FieldNo then
        Exit;
    end;
    Result := nil;
  end;

begin
  Result := nil;
  pn := ParamName;
  Old := CompareText(Copy(pn, 1, 4), 'OLD_') = 0;
  if Old then
    pn := Copy(pn, 5, Length(pn) - 4);
  Master := FDataSet.IsMasterDatasetActive and (CompareText(Copy(pn, 1, 4), 'MAS_') = 0);
  if Master then begin
    pn := Copy(pn, 5, Length(pn) - 4);
    ds := FDataSet.MasterSource.DataSet;
  end
  else
    ds := FDataSet;

  if Old or Master then begin
    Result := ds.FindField(ParamName);
    if Result <> nil then begin // fieldname is starting with OLD_ MAS_
      Old := False;
      Master := False;
    end
    else
      ParamName := pn;
  end;

  if Result = nil then begin
    if AnsiChar(ParamName[1]) in [AnsiChar('0')..AnsiChar('9')] then
      Val(ParamName, AFieldNo, e)
    else
      e := 1;
    if e = 0 then
      Result := FindFieldByFieldNo(AFieldNo)
    else
      AFieldNo := -1;
  end
  else begin
    AFieldNo := -1;
  end;

  if Result = nil then
    Result := ds.FindField(ParamName);
end;

function TDADataSetUpdater.GetUpdateStatement(const StatementType: TStatementType): string;
var
  UpdateSQL: TStrings;
  SelectSQL: string;
begin
  UpdateSQL := FDataSet.FUpdateSQL[StatementType];
  if UpdateSQL = nil then
    Result := ''
  else
  begin
    Result := UpdateSQL.Text;
    if StatementType = stRefresh then begin
      Result := Trim(Result);
      if Pos('WHERE', UpperCase(Result)) = 1 then begin
        if SQLGeneratorCompatibility then
          SelectSQL := FDataSet.BaseSQL
        else
          SelectSQL := FDataSet.FinalSQL;
        Result := FDataSet.SQLAddWhere(SelectSQL, Trim(Copy(Result, 6, Length(Result))));
      end;
    end
    else
    if StatementType = stLock then begin
      Result := Trim(Result);
      if Pos('FOR', UpperCase(Result)) = 1 then
        Result := GetUpdateStatement(stRefresh) + SLLineSeparator + Result;
    end;
  end;
end;

procedure TDADataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);

  function GetTransactionForUpdate: TDATransaction;
  begin
    if FFixedTransaction <> nil then
      Result := FFixedTransaction
    else
    // Refresh uses update transaction if it is active. Otherwise it uses read transaction
    if ((StatementType = stCustom) or // used in information queries
      (StatementType = stRefresh)) and
      ((FDataSet.FUpdateTransaction = nil) or not (FDataSet.FUpdateTransaction.Active))
    then
      Result := UsedTransaction
    else
      Result := UsedUpdateTransaction;
  end;

begin
  UpdateQuery := FUpdateComponents[StatementType];
  if UpdateQuery = nil then begin
    Assert(UsedConnection <> nil);
    UpdateQuery := UsedConnection.CreateDataSet(FDataSet);
  end
  else
    if IsClass(FUpdateQuery, TCustomDADataSet) then
      TCustomDADataSet(FUpdateQuery).Connection := UsedConnection
    else
    if FUpdateQuery is TCustomDASQL then
      TCustomDASQL(FUpdateQuery).Connection := UsedConnection
    else
      Assert(False, 'FUpdateQuery is ' + FUpdateQuery.ClassName);

  TDBAccessUtils.SetLockDebug(FUpdateQuery, True);

  if FUpdateQuery is TCustomDADataSet then begin
    TCustomDADataSet(FUpdateQuery).Close; // To prevent exception raising on setting properties
    TCustomDADataSet(FUpdateQuery).Transaction := GetTransactionForUpdate;
    TCustomDADataSet(FUpdateQuery).Encryption := FDataSet.Encryption;
    TCustomDADataSet(FUpdateQuery).CheckIRecordSet;
    TCustomDADataSet(FUpdateQuery).CheckDataSetService;
    TCustomDADataSet(FUpdateQuery).FIRecordSet.Component := FUpdateQuery;

    if StatementType = stBatchUpdate then begin
      TCustomDADataSet(FUpdateQuery).ParamCheck := False;
      TCustomDADataSet(FUpdateQuery).Params.Clear;
    end;

    TCustomDADataSet(FUpdateQuery).ReadOnly := True;
    TCustomDADataSet(FUpdateQuery).Options.NumberRange := False;
    TCustomDADataSet(FUpdateQuery).Options.TrimFixedChar := FDataSet.Options.TrimFixedChar;
    TCustomDADataSet(FUpdateQuery).Options.TrimVarChar := FDataSet.Options.TrimVarChar;
    TCustomDADataSet(FUpdateQuery).Options.SetEmptyStrToNull := FDataSet.Options.SetEmptyStrToNull;
    TCustomDADataSet(FUpdateQuery).Options.FlatBuffers := True;
    TCustomDADataSet(FUpdateQuery).Options.FullRefresh := FDataSet.Options.FullRefresh;
    TCustomDADataSet(FUpdateQuery).Options.InsertAllSetFields := FDataSet.Options.InsertAllSetFields;
    TCustomDADataSet(FUpdateQuery).Options.EnableBCD := FDataSet.Options.EnableBCD;
    TCustomDADataSet(FUpdateQuery).Options.EnableFMTBCD := FDataSet.Options.EnableFMTBCD;
  {$IFDEF HAVE_COMPRESS}
    TCustomDADataSet(FUpdateQuery).Options.CompressBlobMode := FDataSet.Options.CompressBlobMode;
  {$ENDIF}
    if StatementType in [stRefresh, stRefreshQuick, stRefreshCheckDeleted] then
      TCustomDADataSet(FUpdateQuery).Options.LongStrings := FDataSet.Options.LongStrings;

    SetUpdateQueryOptions(StatementType, False);
  end
  else
  if FUpdateQuery is TCustomDASQL then begin
    TCustomDASQL(FUpdateQuery).Transaction := GetTransactionForUpdate;
  end;

  FUpdateComponents[StatementType] := FUpdateQuery;
end;

procedure TDADataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType; const IsAutoGeneratedSQL: Boolean);
begin
end;

procedure TDADataSetUpdater.CheckUpdateSQL(const SQL: string;
  const StatementTypes: TStatementTypes; UseGenerator: boolean = True);
var
  NewSQL, OldSQL: string;
  StTypes: TStatementTypes;
  StatementType: TStatementType;
  stIdx: TStatementType;
  CheckSQLNeeded: boolean;
  UpdateObjectSQL: TStrings;
  IsAutoGeneratedSQL: boolean;
  Index: Integer;
  UseGeneratorParams: boolean;
  UQParams: TDAParams;
  ParamInfo: TDAParamInfo;
  Param: TDAParam;
  i: integer;
begin
  FParamsInfo.Clear;
  FIsUsedIndexNameForFields := False;
  IsAutoGeneratedSQL := False;
  CheckSQLNeeded := True;
  StTypes := StatementTypes;
  if (stLock in StTypes) and (stRefresh in StTypes) then
    Exclude(StTypes, stRefresh);
  StatementType := stCustom;
  for stIdx := Low(TStatementType) to High(TStatementType) do
    if stIdx in StTypes then begin
      if StatementType <> stCustom then
        StatementType := stCustom
      else
        StatementType := stIdx;
      if StatementType = stCustom then
        Break;
    end;

  UpdateQuery := nil;
  try
    if GetUpdateObject <> nil then begin
      UpdateQuery := GetUpdateObject.GetObjectIndex(Ord(StatementType));
      if FUpdateQuery <> nil then begin
        CheckSQLNeeded := False;
        if not ((FUpdateQuery is TCustomDADataSet) and TCustomDADataSet(FUpdateQuery).SQLAutoGenerated) and
          (TDBAccessUtils.GetSQL(FUpdateQuery).Count = 0)
        then
          DatabaseError(SUpdateObjectEmptySQL);
      end;
    end;
    if FUpdateQuery = nil then
      CheckUpdateQuery(StatementType);
  finally
    Assert(FUpdateQuery <> nil, 'FUpdateQuery = nil'{$IFNDEF FPC} + '. StatementTypes = ' + IntToStr(Word(StatementTypes)){$ENDIF});
  end;

  if CheckSQLNeeded then begin
    TDBAccessUtils.SetAutoCommit(FUpdateQuery,
      (StatementTypes * [stInsert, stUpdate, stDelete, stBatchUpdate] <> []) and FDataSetService.IsAutoCommit);

    NewSQL := SQL;
    if StatementType = stBatchUpdate then
      NewSQL := PrepareBatch(FBatchSQLs.ToString)
    else if NewSQL = '' then begin
      if GetUpdateObject <> nil then
        UpdateObjectSQL := GetUpdateObject.GetSQLIndex(Ord(StatementType))
      else
        UpdateObjectSQL := nil;

      if Assigned(UpdateObjectSQL) then
        NewSQL := UpdateObjectSQL.Text;

      if NewSQL = '' then begin
        NewSQL := GetUpdateStatement(StatementType);
        if (NewSQL <> '') and (FDataSet.Macros.Count > 0) then
          FDataSet.Macros.Expand(NewSQL);

        if (NewSQL = '') and UseGenerator then begin
          IsAutoGeneratedSQL := True;
          if BatchUpdate then
            Index := FBatchStatements
          else
            Index := -1;

          // options for Auto Generated SQL may be differ
          if FOptionsForAutoGeneratedSQL then
            SetUpdateQueryOptions(StatementType, True);

          UseGeneratorParams := UseSQLGeneratorParams(StatementTypes);
          FDataSetService.FSQLGenerator.SubstituteParamName := UseGeneratorParams;
          NewSQL := FDataSetService.FSQLGenerator.GenerateSQL(FParamsInfo,
             _TStatementType(StatementType),
            not (csDesigning in FDataSet.ComponentState) and not FDataSet.Options.UpdateAllFields, Index);
          FIsUsedIndexNameForFields := FDataSetService.FSQLGenerator.IsUsedIndexNameForFields;

          if FUpdateQuery is TCustomDADataSet then
            if not ((StatementType = stRefresh) and FDataSet.Options.FullRefresh) then
              TCustomDADataSet(FUpdateQuery).Encryption.Fields := FDataSetService.FSQLGenerator.RefreshSQLFields;

          if UseGeneratorParams then begin
            UQParams := TDBAccessUtils.GetParams(FUpdateQuery);
            UQParams.BeginUpdate;
            try
              UQParams.Clear;

              for i := 0 to FParamsInfo.Count - 1 do begin
                ParamInfo := FParamsInfo[i];
                Param := TDAParam(UQParams.Add);
                Param.Name := ParamInfo.ParamName;
                Param.ParamType := TParamType(ParamInfo.ParamType);
              end;
            finally
              UQParams.EndUpdate;
              RecreateParamsRef(UQParams);
            end;
          end;

          if (TDBAccessUtils.GetSQLText(FUpdateQuery) <> NewSQL) or
            not (FDataSet.Options.PrepareUpdateSQL and not UsedConnection.Options.DisconnectedMode) then
            TDBAccessUtils.SetSQLText(FUpdateQuery, NewSQL,
              UseGeneratorParams, StatementType in [stInsert, stUpdate, stDelete]);
        end;
      end;
    end;

    // Check whether SQL text is the same. For multiple update operations.
    if BatchUpdate and not ((StatementType = stBatchUpdate) or (StatementType = stLock) or (StatementType = stRefresh)) then begin
      if not IsAutoGeneratedSQL then
         NewSQL := GetICommand.ParseSQL(NewSQL, nil, ':' + FDataSetService.FSQLGenerator.IndexedPrefix + IntToStr(FBatchStatements) + '_');
      if NewSQL <> '' then begin
        if FBatchSQLs.Length <> 0 then
          FBatchSQLs.Append(#13#10);
        FBatchSQLs.Append(NewSQL);
        FBatchSQLs.Append(GetSQLSeparator);
        inc(FBatchStatements);
      end;
    end;

    Assert(FUpdateQuery <> nil);
    if not IsAutoGeneratedSQL then begin
      OldSQL := TDBAccessUtils.GetSQL(FUpdateQuery).Text;
    {$IFNDEF VER7P}
      NewSQL := NewSQL;
    {$ENDIF}
      if OldSQL <> NewSQL then begin
        TDBAccessUtils.GetParams(FUpdateQuery).Clear; /// Performance optimization - skipping reassigning old params values on changing SQL
        TDBAccessUtils.GetSQL(FUpdateQuery).Text := NewSQL;
      end;
    end;

    //Used user defined SQL and internal update object used so we could prepare Update object to obtain some performance gain
    if FDataSet.Options.PrepareUpdateSQL and not UsedConnection.Options.DisconnectedMode and not BatchUpdate then begin
      if FUpdateQuery is TCustomDADataSet then
        TCustomDADataSet(FUpdateQuery).Options.AutoPrepare := True
      else
      if FUpdateQuery is TCustomDASQL then
        TCustomDASQL(FUpdateQuery).Prepared := True;
    end;
  end;
end;

/// UpdateExecute performes execute of the UpdateQuery. We need this procedure
/// to get two update models: with and without explicit prepare.
procedure TDADataSetUpdater.UpdateExecute(const StatementTypes: TStatementTypes);
var
  MessageID: cardinal;
  i: integer;
  St: string;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if not FDataSet.FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or FDataSet.Debug) then begin
    St := '';
    for i := 0 to Integer(High(TStatementType)) do
      if TStatementType(i) in StatementTypes then begin
        if St <> '' then
          St := St + ',';
        St := St + Copy(GetEnumName(TypeInfo(TStatementType), i), 3,
          Length(GetEnumName(TypeInfo(TStatementType), i)));
      end;
    TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(FDataSet, TDBAccessUtils.GetSQL(FUpdateQuery).Text, TDBAccessUtils.GetParams(FUpdateQuery), St, MessageID, True);
  end;

  TDBAccessUtils.Execute(FUpdateQuery);

  if not FDataSet.FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or FDataSet.Debug) then
    TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(FDataSet, TDBAccessUtils.GetSQL(FUpdateQuery).Text, TDBAccessUtils.GetParams(FUpdateQuery), St, MessageID, False);
end;

function TDADataSetUpdater.LockCompare(const Value1, Value2: variant): boolean;
var
  DValue1, DValue2: Double;
begin
  Result := VarEqual(Value1, Value2);

  if not Result and (VarType(Value1) = VarType(Value2)) and
    ((VarType(Value1) = varDate) {$IFNDEF FPC}or (VarType(Value1) = VarSQLTimeStamp){$ENDIF})
  then begin
    DValue1 := Value1;
    DValue2 := Value2;

    if SameValue(DValue1, DValue2, DateTimeValueDelta) then
      Result := True;
  end
  else if not Result and (VarType(Value1) = VarType(Value2)) and (VarType(Value1) = varDouble) then begin
    DValue1 := Value1;
    DValue2 := Value2;

    if SameValue(DValue1, DValue2, DoubleValueDelta) then
      Result := True;
  end;
end;

procedure TDADataSetUpdater.WriteUQParams(const StatementTypes: TStatementTypes);

  function FindFieldDescByFieldNo(FieldNo: integer): TFieldDesc;
  var
    i: integer;
  begin
    for i := 0 to GetIRecordSet.Fields.Count - 1 do begin
      Result := GetIRecordSet.Fields[i];
      if Result.FieldNo = FieldNo then
        Exit;
    end;
    Result := nil;
  end;

  function SuppressBatchPrefix(Value: string): string;
  var
    i,e: integer;
  begin
    Result := Value;
    if BatchUpdate then begin
      i := Pos(FDataSetService.FSQLGenerator.IndexedPrefix, Value);
      if i = 1 then begin // if parameter name starts from "P_"
        e := i + 2;
        while (e <= Length(Result)) and (Result[e] <> '_') do
          inc(e);
        System.Delete(Result, i, e - i + 1);
      end;
    end;
  end;

  procedure AssignFieldValueEx(Param: TDAParam; Field: TField; Old: boolean; Master: boolean);
  var
    FieldDesc: TFieldDesc;
  begin
    if UseParamType(Param) and
      ((Param.ParamType = ptOutput) or (Param.ParamType = ptResult)) then begin
      Param.DataType := Field.DataType;
      Param.Value := Null;
    end
    else begin
      if Master then begin
        if Old then
          Param.AssignFieldValue(Field, Field.OldValue)
        else
          Param.AssignFieldValue(Field, Field.NewValue);
      end
      else begin
        FieldDesc := FindFieldDescByFieldNo(Field.FieldNo);
        if FieldDesc = nil then
          FieldDesc := GetIRecordSet.FindField(Field.FieldName);
        if FieldDesc = nil then
          DatabaseError(Format(SNoCorrespondParam, [Param.Name]));

        // FDataSet.AssignFieldValue(Param, Field, Old);
        // TODO: check this assignment
        FDataSet.AssignFieldValue(Param, FieldDesc, Old);
      end;
    end;
  end;

var
  i: integer;
  Param, Param1: TDAParam;
  ParamName: string;
  Old, Master: boolean;
  Field: TField;
  AFieldNo: integer;
  AFieldDesc: TFieldDesc;
  Params: TDAParams;
  LowIndex: integer;
  ParamInfoIndex: integer;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[5].Start;
{$ENDIF}

  // assigning parameter values from fields of the same name
  if BatchUpdate then begin
    // copy parameters from UpdateQuery to common collection
    if FBatchParams = nil then begin
      FBatchParams := FDataSet.FCommand.CreateParamsObject;
      FBatchParams.FOwner := nil;
    end;

    Params := TDBAccessUtils.GetParams(FUpdateQuery);
    FBatchParams.BeginUpdate;
    try
      for i := 0 to Params.Count - 1 do
        FBatchParams.CreateParam(Params[i].DataType, Params[i].Name, Params[i].ParamType);
    finally
      FBatchParams.FNeedsUpdateItem := False;
      FBatchParams.EndUpdate;
      RecreateParamsRef(FBatchParams);
      FBatchParams.FNeedsUpdateItem := True;
    end;

    Params := FBatchParams;
    LowIndex := Params.Count - TDBAccessUtils.GetParams(FUpdateQuery).Count;
  end
  else begin
    Params := TDBAccessUtils.GetParams(FUpdateQuery);
    LowIndex := 0;
  end;

  for i := LowIndex to Params.Count - 1 do begin
    Param := Params[i];
    AFieldNo := -1;
    ParamName := '';
    Field := nil;
    if BatchUpdate then
      ParamInfoIndex := i - LowIndex
    else if (stRefresh in StatementTypes) and (FParamsInfo.Count > 0) then
      ParamInfoIndex := i - (Params.Count - FParamsInfo.Count)
    else
      ParamInfoIndex := i;
    if (FParamsInfo.Count > 0) and ([stRefreshQuick, stRefreshCheckDeleted] * StatementTypes = []) and not ((ParamInfoIndex < 0) and (stRefresh in StatementTypes)) then begin
      Old := FParamsInfo.Items[ParamInfoIndex].Old;
      Field := FDataSet.GetField(FParamsInfo.Items[ParamInfoIndex].FieldDesc);
    end;

    if (FParamsInfo.Count = 0) or (Field = nil) then begin
      // param name can be quoted if generated by SQL Generator
      ParamName := FDataSet.SQLInfo.UnQuote(Param.Name);
      // should remove additional index before assigning value from field
      ParamName := SuppressBatchPrefix(ParamName);
      if (FDataSet.MasterSource <> nil) and (FDataSet.MasterFields = '') and (FDataSet.DetailFields = '') and (FDataSet.Params.Count > 0) and (stRefresh in StatementTypes) then begin
        if FDataSet.FindParam(ParamName) = nil then
          Field := FieldByParamName(ParamName, Old, AFieldNo, Master)
      end
      else
        Field := FieldByParamName(ParamName, Old, AFieldNo, Master);
    end
    else
      Master := False;

    if Field <> nil then
      AssignFieldValueEx(Param, Field, Old and (not ((FDataSet.State = dsInsert) and not FDataSet.FInDeferredPost)), Master) // OldValue is Null on Insert
    else begin
      Assert(ParamName <> '');

      AFieldDesc := nil;
      if AFieldNo >= 0 then
        AFieldDesc := FindFieldDescByFieldNo(AFieldNo);
      if AFieldDesc = nil then
        AFieldDesc := GetIRecordSet.FindField(ParamName);
      if (AFieldDesc <> nil) and (AFieldDesc.DataType in [dtObject,dtArray,dtTable]) then // object fields in not objectview
        FDataSet.AssignFieldValue(Param, AFieldDesc, False)
      else begin
        Param1 := FDataSet.FindParam(ParamName);
        if (Param1 <> nil) and (([stRefresh, stRefreshQuick, stRefreshCheckDeleted] * StatementTypes <> []) or (Param1.ParamType = ptResult)) then
          Param.Assign(Param1)  // assign param from param of SQL
        else
          if AFieldDesc <> nil then
            FDataSet.AssignFieldValue(Param, AFieldDesc, Old and (not ((FDataSet.State = dsInsert) and not FDataSet.FInDeferredPost)))
          else
          if not FDataSet.AssignedBeforeUpdateExecute then begin
            if not UseParamType(Param) then
              SetDefaultParamType(Param);
            if not (Param.ParamType in [ptOutput, ptResult]) then
              DatabaseError(Format(SNoCorrespondParam, [Param.Name]));
          end;
      end;
    end;
    if ([stRefreshQuick, stRefreshCheckDeleted] * StatementTypes <> []) and
       FDataSet.IsMasterDatasetActive then
      FDataSet.SetMasterParams(Params, nil);
    if not UseParamType(Param) then
      SetDefaultParamType(Param);
  end;
{$IFDEF PERF_COUNTER}
  PerfCounters[5].Stop;
{$ENDIF}
end;

procedure TDADataSetUpdater.CopyRecBufToActiveRecord(SrcRecordSet: TData; SrcRecBuf: TRecordBuffer;
  const StatementTypes: TStatementTypes; out RecordWasChanged: boolean);

  function FindFieldByFieldNo(FieldNo: integer): TField;
  var
    i: integer;
  begin
    for i := 0 to FDataSet.Fields.Count - 1 do begin
      Result := FDataSet.Fields[i];
      if Result.FieldNo = FieldNo then
        Exit;
    end;
    Result := nil;
  end;

var
  i: integer;
  RecBuf: TRecordBuffer;
  PrevRecBuf: TRecordBuffer;
  FieldDesc: TFieldDesc;
  FieldName: string;
  FieldDescIdx: integer;
  AFieldDesc: TFieldDesc;
  IsBlank, NativeBuffer: boolean;
  SharedObject: TSharedObject;
  Value, NewValue: variant;
  Field: TField;
  RecordSet: TCRRecordSet;
  DataBuf: IntPtr;
  DataLen: Word;
  ValuePtr: IntPtr;
  ValueSize: Word;
  NeedAddRef: boolean;
begin
  RecordWasChanged := False;
  NeedAddRef := False;
  if FDataSet.InCacheProcessing then
    RecBuf := GetIRecordSet.NewCacheRecBuf
  else begin
    RecBuf := IntPtr(FDataSet.ActiveBuffer);
    if not ((stLock in StatementTypes) and
       (FCheckMode = cmException) and
       not (roBeforeEdit in FDataSet.RefreshOptions))
    then begin
      FDataSet.FreeRefComplexFields(RecBuf);
      NeedAddRef := True;
    end;
  end;

  RecordSet := FDataSet.FIRecordSet;

  for i := 0 to SrcRecordSet.Fields.Count - 1 do begin
    if not SrcRecordSet.Fields[i].HasParent then begin
      FieldDesc := SrcRecordSet.Fields[i];
      FieldName := FieldDesc.Name;

      AFieldDesc := nil;
      if FIsUsedIndexNameForFields then begin
        FieldDescIdx := FDataSetService.FSQLGenerator.DecodeFieldIndex(FieldName);
        if FieldDescIdx >= 0 then
          AFieldDesc := RecordSet.Fields[FieldDescIdx];
      end;
      if AFieldDesc = nil then
        AFieldDesc := RecordSet.FindField(FieldDesc.Name);

      if (AFieldDesc <> nil){ and CanRefreshField(Field) and}
      then
        if FieldDesc.IsComplex and
           not (FieldDesc.DataType in [dtExtString, dtExtWideString, dtExtVarBytes])
        then begin
          if [stRefresh, stRefreshQuick, stRefreshCheckDeleted] * StatementTypes <> [] then begin
            Assert(AFieldDesc.IsComplex);
            DataBuf := RecordSet.GetFieldBuf(RecBuf, AFieldDesc, DataLen, IsBlank, NativeBuffer);
            SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf)));
            SharedObject.Free;

            ValuePtr := SrcRecordSet.GetFieldBuf(SrcRecBuf, FieldDesc, ValueSize, IsBlank, NativeBuffer);
            SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
            if SharedObject <> nil then
              SharedObject.AddRef;
            if FieldDesc.DataType = dtVariant then
              Move(ValuePtr^, DataBuf^, sizeof(TVariantObject))
            else
              RecordSet.PutField(AFieldDesc, RecBuf, ValuePtr, ValueSize, False);

            if IsBlank then
              RecordSet.SetNull(AFieldDesc, RecBuf, True);

            if (FieldDesc.DataType in [dtBlob, dtMemo, dtWideMemo]) and
               (FDataSet.State in [dsInsert, dsEdit])
            then
              TBlob(SharedObject).EnableRollback;
          end;
        end
        else begin
          Field := FindFieldByFieldNo(AFieldDesc.FieldNo);

          if Field <> nil then begin
            if (stLock in StatementTypes) and
               (FCheckMode = cmException) and
               not (roBeforeEdit in FDataSet.RefreshOptions)
            then begin
              // check
              if (FDataSet.State in dsEditModes) or FDataSet.CachedUpdates then
                PrevRecBuf := FDataSet.GetOldRecBuf
              else
                PrevRecBuf := FDataSet.GetNewRecBuf;
              RecordSet.GetMappedFieldAsVariant(AFieldDesc, PrevRecBuf, Value);

              SrcRecordSet.GetMappedFieldAsVariant(FieldDesc, SrcRecBuf, NewValue);
              if not LockCompare(Value, NewValue) then
                DatabaseError(SRecordChanged);
            end
            else begin
              SrcRecordSet.GetFieldAsVariant(FieldDesc, SrcRecBuf, NewValue);

              if IsRefreshQuickField(FieldDesc) then
                if not LockCompare(Field.NewValue, NewValue) then
                  RecordWasChanged := True;

              RecordSet.PutFieldAsVariant(AFieldDesc, RecBuf, NewValue, True);
            end;
          end;
        end;
    end;
  end;

  if (RecordSet.Fields.Count > 0) and (RecordSet.Fields[RecordSet.Fields.Count - 1].FieldDescKind = fdkCached) then
    FDataSet.DoGetCachedBuffer(RecBuf);

  if ([stRefresh, stRefreshQuick, stRefreshCheckDeleted] * StatementTypes <> []) and
    not FDataSet.InCacheProcessing and not FRefreshInUpdate
  then
    RecordSet.RefreshRecord(RecBuf);

  if NeedAddRef then
    FDataSet.AddRefComplexFields(RecBuf);
end;

procedure TDADataSetUpdater.UpdateActiveRecordFromParams;
var
  i: integer;
  Param: TDAParam;
  ParamName: string;
  Old, Master: boolean;
  Field: TField;
  AFieldNo: integer;
  FieldDesc: TFieldDesc;
  RecBuf: TRecordBuffer;
  IsBlank, NativeBuffer: boolean;
  SharedObject: TSharedObject;
  ReadOnly: boolean;
  DataBuf: IntPtr;
  DataLen: Word;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[5].Start;
{$ENDIF}
  for i := 0 to TDBAccessUtils.GetParams(FUpdateQuery).Count - 1 do begin
    Param := TDBAccessUtils.GetParams(FUpdateQuery)[i];
    if UseParamType(Param) then
      if Param.ParamType < ptOutput then
        Continue;

    if FParamsInfo.Count > 0 then begin
      Old := FParamsInfo.Items[i].Old;
      Field := FDataSet.GetField(FParamsInfo.Items[i].FieldDesc);
    end
    else begin
    // param name can be quoted if generated by SQL Generator
      ParamName := FDataSet.SQLInfo.UnQuote(Param.Name);
      Field := FieldByParamName(ParamName, Old, AFieldNo, Master);
      if Master then   // now, there is no way to determine the exact field
        Field := nil;  // to which return parameter value
    end;

    if Assigned(Field) and not Old and FDataSet.CanRefreshField(Field) then begin
      ReadOnly := Field.ReadOnly;
      if Field.ReadOnly then begin
        FDataSet.SetTempState(FDataSet.State); // DisableControls
        Field.ReadOnly := False;
      end;

      FieldDesc := GetIRecordSet.Fields[Field.FieldNo - 1];

      if (Param.ParamObject <> nil) and (FUpdateQuery is TCustomDADataSet) and
         FieldDesc.IsComplex and not (FieldDesc.DataType in [dtExtString, dtExtWideString, dtExtVarBytes, dtVariant])
      then begin
        // pass SharedObject from parameter to recordset
        if FDataSet.InCacheProcessing then
          RecBuf := GetIRecordSet.NewCacheRecBuf
        else
          RecBuf := IntPtr(FDataSet.ActiveBuffer);
        DataBuf := GetIRecordSet.GetFieldBuf(RecBuf, FieldDesc, DataLen, IsBlank, NativeBuffer);
        SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf)));
        SharedObject.Free;

        SharedObject := Param.ParamObject;
        SharedObject.AddRef;

        Marshal.WriteIntPtr(DataBuf, SharedObject.GCHandle);
        // GetIRecordSet.PutField may set Param.IsNull to False !!!
        if Param.IsNull then begin
          GetIRecordSet.PutField(FieldDesc, RecBuf, DataBuf, DataLen, False);
          GetIRecordSet.SetNull(FieldDesc, RecBuf, True);
        end
        else
          GetIRecordSet.PutField(FieldDesc, RecBuf, DataBuf, DataLen, False);

        if (FieldDesc.DataType in [dtBlob, dtMemo, dtWideMemo]) and
           (FDataSet.State in [dsInsert, dsEdit])
        then
          TBlob(SharedObject).EnableRollback;
      end
      else
        if Param.IsNull then
          Field.NewValue := Null
        else
          if not VarEqual(Field.Value, Param.Value) then
            Field.NewValue := Param.Value;

      if ReadOnly then begin
        Field.ReadOnly := True;
        FDataSet.RestoreState(FDataSet.State);
      end
    end;
  end;
{$IFDEF PERF_COUNTER}
  PerfCounters[5].Stop;
{$ENDIF}
end;

procedure TDADataSetUpdater.UpdateActiveRecord(const StatementTypes: TStatementTypes);
var
  UpdateRecordSet: TCRRecordSet;
  RefreshRecBuf: TRecordBuffer;
  RecordWasChanged: boolean;
begin
  Assert(FUpdateQuery is TCustomDADataSet);
  UpdateRecordSet := TCustomDADataSet(FUpdateQuery).FIRecordSet;

  UpdateRecordSet.AllocRecBuf(IntPtr(RefreshRecBuf));
  try
    UpdateRecordSet.SetToBegin;  // temp
    UpdateRecordSet.GetNextRecord(RefreshRecBuf);
    if not UpdateRecordSet.EOF then
      CopyRecBufToActiveRecord(UpdateRecordSet, RefreshRecBuf, StatementTypes, RecordWasChanged);
  finally
    UpdateRecordSet.FreeRecBuf(RefreshRecBuf);
  end;
end;

procedure TDADataSetUpdater.GetUQFields(const KeyFieldDescs: TFieldDescArray; const KeyFields: TFieldArray; out KeyFieldsUQ: TFieldArray);
var
  i: integer;
begin
  Assert(FUpdateQuery is TCustomDADataSet);

  SetLength(KeyFieldsUQ, Length(KeyFields));
  for i := 0 to Length(KeyFields) - 1 do
    if KeyFields[i] <> nil then
      KeyFieldsUQ[i] := TCustomDADataSet(FUpdateQuery).Fields.FindField(KeyFields[i].FieldName)
    else
      KeyFieldsUQ[i] := TCustomDADataSet(FUpdateQuery).Fields.FindField(KeyFieldDescs[i].Name);
end;

procedure TDADataSetUpdater.CheckDeletedRecords;
var
  KeyFieldDescs: TFieldDescArray;
  KeyFields, KeyFieldsUQ: TFieldArray;
  s, OldIndexFieldNames: string;
  v: variant;
  i: integer;
begin
  Assert(FUpdateQuery is TCustomDADataSet);

  FDataSet.UpdateCursorPos;
  FDataSet.First;
  FDataSet.GetCurrentKeys(KeyFieldDescs, KeyFields);
  GetUQFields(KeyFieldDescs, KeyFields, KeyFieldsUQ);
  s := '';
  for i := Low(KeyFieldDescs) to High(KeyFieldDescs) do begin
    if s <> '' then
      s := s + ';';
    s := s + KeyFieldDescs[i].Name;
  end;

  OldIndexFieldNames := TCustomDADataSet(FUpdateQuery).IndexFieldNames;
  try
    TCustomDADataSet(FUpdateQuery).IndexFieldNames := s;
    while not FDataSet.Eof do begin
      FDataSet.GetCurrentValues(KeyFieldDescs, v);
      if not TCustomDADataSet(FUpdateQuery).Locate(KeyFieldsUQ, v, []) then begin
        GetIRecordSet.RemoveRecord;
        Inc(FDataSet.FRowsAffected);
        FDataSet.Resync([]);
      end
      else
        FDataSet.Next;
    end;
  finally
    TCustomDADataSet(FUpdateQuery).Close;
    TCustomDADataSet(FUpdateQuery).IndexFieldNames := OldIndexFieldNames;
  end;
  FDataSet.Resync([]);
end;

procedure TDADataSetUpdater.RefreshQuickDataSet;
var
  UpdateRecordSet: TCRRecordSet;
  RefreshRecBuf: TRecordBuffer;
  RecBuf: TRecordBuffer;
  OldOnAppend: TOnModifyRecord;
  RQFieldDescs: TFieldDescArray;
  KeyFieldDescs: TFieldDescArray;
  KeyFields, KeyFieldsUQ: TFieldArray;
  FieldDesc: TCRFieldDesc;
  RecordWasChanged: boolean;
  v, NewValue: variant;
  i, n: integer;
begin
  Assert(FUpdateQuery is TCustomDADataSet);

  if TCustomDADataSet(FUpdateQuery).RecordCount > 0 then begin
    UpdateRecordSet := TCustomDADataSet(FUpdateQuery).FIRecordSet;

    // Refresh from fields and check
    FDataSet.UpdateCursorPos;
    UpdateRecordSet.AllocRecBuf(IntPtr(RefreshRecBuf));
    GetIRecordSet.AllocRecBuf(IntPtr(RecBuf));
    OldOnAppend := GetIRecordSet.OnAppend;
    GetIRecordSet.OnAppend := nil;

    try
      n := 0;
      SetLength(RQFieldDescs, UpdateRecordSet.Fields.Count);
      for i := 0 to UpdateRecordSet.Fields.Count - 1 do begin
        FieldDesc := TCRFieldDesc(UpdateRecordSet.Fields[i]);
        if IsRefreshQuickField(FieldDesc) then begin
          RQFieldDescs[n] := FieldDesc;
          Inc(n);
        end;
      end;
      SetLength(RQFieldDescs, n);

      // Get key fields list from base DataSet
      FDataSet.GetCurrentKeys(KeyFieldDescs, KeyFields);
      GetUQFields(KeyFieldDescs, KeyFields, KeyFieldsUQ);

      TCustomDADataSet(FUpdateQuery).First;
      while not TCustomDADataSet(FUpdateQuery).Eof do begin
        // And get values from FUpdateQuery
        v := VarArrayCreate([0, Length(KeyFieldsUQ) - 1], varVariant);
        for i := 0 to Length(KeyFieldsUQ) - 1 do
          v[i] := KeyFieldsUQ[i].Value;

        if not FDataSet.LocateEx(KeyFields, KeyFieldDescs, v, []) then begin
          GetIRecordSet.InitRecord(RecBuf);
          GetIRecordSet.CreateComplexFields(RecBuf, True);
          GetIRecordSet.AppendRecord(RecBuf);
          FDataSet.Resync([]);
        end;

        UpdateRecordSet.GetRecord(RefreshRecBuf);
        CopyRecBufToActiveRecord(UpdateRecordSet, RefreshRecBuf, [stRefreshQuick], RecordWasChanged);
        if RecordWasChanged then
          Inc(FDataSet.FRowsAffected);

        for i := 0 to Length(RQFieldDescs) - 1 do begin
          UpdateRecordSet.GetFieldAsVariant(RQFieldDescs[i], RefreshRecBuf, NewValue);
          SaveMaxRefreshQuickValue(RQFieldDescs[i], NewValue);
        end;

        TCustomDADataSet(FUpdateQuery).Next;
      end;
    finally
      GetIRecordSet.OnAppend := OldOnAppend;
      GetIRecordSet.FreeRecBuf(RecBuf);
      GetIRecordSet.FreeRecBuf(RefreshRecBuf);
    end;

    GetIRecordSet.SortItems;
    FDataSet.Resync([]);
  end;
end;

function TDADataSetUpdater.PerformSQL(const SQL: string; const StatementTypes: TStatementTypes): boolean;
var
  RecBuf: TRecordBuffer;
  TempParams: TDAParams;
  Param: TDAParam;
  UQIsDataSet: boolean;
  i: integer;
begin
  BeginConnection;
  try
    CheckUpdateSQL(SQL, StatementTypes);

    if TDBAccessUtils.GetSQL(FUpdateQuery).Count = 0 then begin
      Result := False;
      Exit;
    end;

    if StatementTypes = [stBatchUpdate] then begin
      TempParams := TDBAccessUtils.GetParams(FUpdateQuery);
      TempParams.Assign(FBatchParams);
      RecreateParamsRef(TempParams);
    end
    else
      // assigning parameter values from fields of the same name
      WriteUQParams(StatementTypes);

    // No need to call events and UpdateExecute when collecting statemets
    if not BatchUpdate or (StatementTypes = [stBatchUpdate]) then begin
      TDBAccessUtils.GetParams(FUpdateQuery).FParamsChangeType := ctUserChecked; //to allow writing added parameters for UniDAC
      FDataSet.DoBeforeUpdateExecute(FDataSet, StatementTypes, TDBAccessUtils.GetParams(FUpdateQuery));
      if FDataSet.AssignedBeforeUpdateExecute then begin
        for i := 0 to TDBAccessUtils.GetParams(FUpdateQuery).Count - 1 do begin
          Param := TDBAccessUtils.GetParams(FUpdateQuery)[i];
          if (Param.DataType = ftUnknown) and (Param.ParamType <> ptResult) then
            DatabaseError(Format(SUnknownParamDataType, [TDBAccessUtils.GetParams(FUpdateQuery)[i].Name]));
        end;
      end;

      try
        UpdateExecute(StatementTypes);
      finally
        TDBAccessUtils.GetParams(FUpdateQuery).FParamsChangeType := ctGenerated;
      end;
    end;

    try
      UQIsDataSet := FUpdateQuery is TCustomDADataSet;

      if UQIsDataSet and (StatementTypes = [stRefreshCheckDeleted]) then
        CheckDeletedRecords
      else
      if UQIsDataSet and (StatementTypes = [stRefreshQuick]) then
        RefreshQuickDataSet
      else
      if UQIsDataSet and ([stRefresh, stLock] * StatementTypes <> []) and TCustomDADataSet(FUpdateQuery).IsQuery then begin
        // Refresh from fields and check
        FDataSet.FRowsAffected := TCustomDADataSet(FUpdateQuery).RecordCount;

        if FDataSet.Options.StrictUpdate and (FDataSet.FRowsAffected <> 1) then
          DatabaseError(Format(SRefreshFailed, [FDataSet.FRowsAffected]));

        if (stRefresh in StatementTypes) or
           ((stLock in StatementTypes) and
            (FCheckMode in [cmException, cmRefresh]) and
            not (roBeforeEdit in FDataSet.RefreshOptions))
        then
          UpdateActiveRecord(StatementTypes);
      end
      else
      // strict update and DMLRefresh don't work in BatchUpdate mode
      if not BatchUpdate then begin
        FDataSet.FRowsAffected := TDBAccessUtils.GetRowsAffected(FUpdateQuery);

        if FDataSet.Options.StrictUpdate and
          //(Command.SQLType in [SQL_INSERT,SQL_UPDATE,SQL_DELETE]) and  /// for ODAC
          ((FDataSet.FRowsAffected = 0) or (FDataSet.FRowsAffected > 1))
        then
          if stLock in StatementTypes then
            DatabaseError(SRecordChanged)
          else
            DatabaseError(Format(SUpdateFailed, [FDataSet.FRowsAffected]));

        // Refresh fields from params of the same name
        if NeedReturnParams or (stRefresh in StatementTypes) then // DML Refresh
          if ReturnParamsAsFields then
            UpdateActiveRecord(StatementTypes)  // PostgreSQL returning support
          else
            UpdateActiveRecordFromParams;
      end;

    {$IFNDEF LITE}
      if FDataSet.InCacheProcessing then
        RecBuf := GetIRecordSet.NewCacheRecBuf
      else
        RecBuf := IntPtr(FDataSet.ActiveBuffer);
      GetIRecordSet.ClearChangedIndicators(RecBuf);
    {$ENDIF}

      // Lock statement can be UPDATE or SELECT FOR UPDATE for InterBase
      if (stLock in StatementTypes) and UsedConnection.IsMultipleTransactionsSupported and
        (FFixedTransaction <> nil)
      then begin
        Assert(FUpdateQuery is TCustomDADataSet);
        if not TCustomDADataSet(FUpdateQuery).IsQuery then
          Dec(FFixedTransaction.FUnCommitedStatementCount);
      end;

      // No need to call events and UpdateExecute when collecting statemets
      if not BatchUpdate or (StatementTypes = [stBatchUpdate]) then
        FDataSet.DoAfterUpdateExecute(FDataSet, StatementTypes, TDBAccessUtils.GetParams(FUpdateQuery));

      Result := True;
    finally
      if FUpdateQuery is TCustomDADataSet then
        TCustomDADataSet(FUpdateQuery).Close;

      if StatementTypes <> [stBatchUpdate] then
        ReleaseParams(FUpdateQuery, False);
    end;
  finally
    EndConnection;
  end;
end;

function TDADataSetUpdater.IsRefreshQuickField(FieldDesc: TFieldDesc): boolean;
begin
  Result := False;
end;

procedure TDADataSetUpdater.SaveMaxRefreshQuickValue(FieldDesc: TFieldDesc; const Value: variant);
begin

end;

{$IFDEF WITH_IPROVIDER}

function TDADataSetUpdater.PerformPSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): boolean;
var
  I: Integer;
  Old: Boolean;
  Param: TDAParam;
  Params: TDAParams;
  PName: string;
  Field: TField;
  Value: Variant;
  StatementTypes: TStatementTypes;
begin
  StatementTypes := [];
  Include(StatementTypes, UpdateKindToStatementType(UpdateKind));
  // modified fields in this dataset must not affect Update SQL
  // (real modification is in Delta).
  CheckUpdateSQL('', StatementTypes, False);

  if TDBAccessUtils.GetSQL(FUpdateQuery).Count = 0 then begin
    if PreventPSKeyFields and (FDataSet.FOldKeyFields = '') and (UpdateKind in [DB.ukModify, DB.ukDelete]) then
      FDataSet.PSDetectKeyFields(Delta);
    Result := False;
    Exit;
  end;

  Params := TDBAccessUtils.GetParams(FUpdateQuery);

  // Nearly copied from TUpdateSQL.SetParams
  with FUpdateQuery do
    for I := 0 to Params.Count - 1 do
    begin
      Param := Params[I];
      PName := Param.Name;
      Old := SameText(Copy(PName, 1, 4), 'OLD_');
      if Old then
        System.Delete(PName, 1, 4);
      Field := Delta.FindField(PName);
      if not Assigned(Field) then Continue;
      if Old then Param.AssignFieldValue(Field, Field.OldValue) else
      begin
        Value := Field.NewValue;
        if VarIsClear(Value) then
          Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end;
    end;

  UpdateExecute(StatementTypes);
  Result := True;
end;

{$ENDIF}

procedure TDADataSetUpdater.PrepareAppend;
begin

end;

procedure TDADataSetUpdater.PrepareUpdate;
var
  Statements: TStatementTypes;
  NeedLock: Boolean;
begin
  // keep old behavior by the OldCachedUpdateLockMode variable
  if FDataSet.CachedUpdates and OldCachedUpdateLockMode then
    exit;

  Statements := [];
  // record that has been inserted (UpdateStatus = usInserted)
  // but hasn't been saved to database (UpdateResult <> uaApplied)
  // cannot be refreshed before Edit
  if roBeforeEdit in FDataSet.RefreshOptions then
    if (not FDataSet.CachedUpdates) or
       (FDataSet.InternalGetUpdateResult = urApplied) or
       (FDataSet.UpdateStatus <> usInserted)
    then
      if CanRefreshByLock then
        Include(Statements, stRefresh)
      else
        PerformRefreshRecord;

  // need lock for not modified records only
  // if record is modified then it is already locked
  if GetLockMode = lmPessimistic then begin
    if FDataSet.CachedUpdates then
      NeedLock := (FDataSet.InternalGetUpdateResult = urApplied) or
                  (FDataSet.UpdateStatus = usUnmodified)
    else
      NeedLock := (FDataSet.State <> dsEdit);

    if NeedLock then
      Include(Statements, stLock);
  end;

  if Statements <> [] then begin
    if stLock in Statements then begin
      if FDataSet.CachedUpdates then
        PrepareCachedUpdate;
      SetLockSavepoint;
    end;

    try
      PerformSQL('', Statements);
    except
      // savepoint can be lost if connection is lost
      // or exception can be raised before set savepoint
      // so ignore all errors on rollback to savepoint
      if stLock in Statements then begin
        try
          // remove lock from the current record
          PerformUnlock;
        except
        end;
        if FDataSet.CachedUpdates then
        try
          // for disconnected mode: if cache doesn't have changed records
          // then finish transaction and close connection
          UnPrepareCachedUpdate;
        except
        end;
      end;

      raise;
    end;
  end;
end;

procedure TDADataSetUpdater.PrepareDelete;
var
  NeedLock: Boolean;
begin
  // keep old behavior by the OldCachedUpdateLockMode variable
  if FDataSet.CachedUpdates and OldCachedUpdateLockMode then
    exit;

  if GetLockMode = lmPessimistic then begin
    // need lock for not modified records only
    // if record is modified then it is already locked
    if FDataSet.CachedUpdates then
      NeedLock := (FDataSet.InternalGetUpdateResult = urApplied) or
                  (FDataSet.UpdateStatus = usUnmodified)
    else
      NeedLock := (FDataSet.State <> dsEdit);

    if NeedLock then begin
      if FDataSet.CachedUpdates then
        PrepareCachedUpdate;

      try
        PerformLock;
      except
        // savepoint can be lost if connection is lost
        // or exception can be raised before set savepoint
        // so ignore all errors on rollback to savepoint
        try
          // remove lock from the current record
          PerformUnlock;
        except
        end;
        if FDataSet.CachedUpdates then
        try
          // for disconnected mode: if cache doesn't have changed records
          // then finish transaction and close connection
          UnPrepareCachedUpdate;
        except
        end;

        raise;
      end;
    end;
  end;
end;

procedure TDADataSetUpdater.UnPrepareAppendUpdateDelete;
begin
  // keep old behavior by the OldCachedUpdateLockMode variable
  if FDataSet.CachedUpdates and OldCachedUpdateLockMode then
    exit;

  if (GetLockMode = lmPessimistic) and (FDataSet.State <> dsInsert) then
    // in the CachedUpdate mode if user will execute the following steps:
    // Edit - Post - Edit - Cancel
    // record must be locked after cancel: check InternalGetUpdateResult and UpdateStatus
    if (not FDataSet.CachedUpdates) or
       (FDataSet.InternalGetUpdateResult = urApplied) or (FDataSet.UpdateStatus = usUnmodified)
    then begin
      PerformUnlock;
      // for disconnected mode: if cache doesn't have changed records
      // then finish transaction and close connection
      if FDataSet.CachedUpdates then
        UnPrepareCachedUpdate;
    end;
end;

procedure TDADataSetUpdater.PrepareCachedUpdate;
begin
  if FDataSet.CachedUpdates and
     (not OldCachedUpdateLockMode) and
     (GetLockMode = lmPessimistic) and
     (not FDataSet.Data.HasUpdatedOrDeletedRecords) // if no updated records
  then
    // set savepoint for the CachedUpdates mode
    SetLockCachedUpdatesSavepoint;
end;

procedure TDADataSetUpdater.FinishCachedUpdate;
begin
  if FDataSet.CachedUpdates and
     (not OldCachedUpdateLockMode) and
     (GetLockMode = lmPessimistic) and
     (not FDataSet.Data.HasUpdatedOrDeletedRecords) // if no updated records
  then
    // on ApplyChanges reset information when transaction started
    ResetLockCachedUpdatesSavepoint;
end;

procedure TDADataSetUpdater.UnPrepareCachedUpdate;
begin
  if FDataSet.CachedUpdates and
     (not OldCachedUpdateLockMode) and
     (GetLockMode = lmPessimistic) and
     (not FDataSet.Data.HasUpdatedOrDeletedRecords) // if has locked records
  then
    // for disconnected mode: if cache doesn't have changed records
    // then finish transaction and close connection
    RollbackToLockCachedUpdatesSavepoint;
end;

procedure TDADataSetUpdater.UnLockCachedUpdate;
begin
  if FDataSet.CachedUpdates and
     (not OldCachedUpdateLockMode) and
     (GetLockMode = lmPessimistic) and
     FDataSet.Data.HasUpdatedOrDeletedRecords // if has locked records
  then
    // then rollback to the savepoint before first lock in the CachedUpdate mode
    RollbackToLockCachedUpdatesSavepoint;
end;

function TDADataSetUpdater.PerformLock: boolean;
begin
  Result := True;
  if not FDataSet.EOF then begin
    FDataSet.UpdateCursorPos;
    SetLockSavepoint;
    PerformSQL('', [stLock]);
  end;
end;

function TDADataSetUpdater.PerformUnLock: boolean;
begin
  Result := True;
  RollbackToLockSavepoint;
end;

function TDADataSetUpdater.PerformAppend: boolean;
begin
  Result := PerformSQL('', [stInsert]);

  if not Result and FDataSet.Options.StrictUpdate then
    DatabaseError(Format(SUpdateFailed, [0]));

  if Result then begin
    SetIdentityFieldValue;

    if (FUpdateQuery <> nil) and (FUpdateQuery is TCustomDADataSet) then
      TCustomDADataSet(FDataSet).FLastInsertId := TCustomDADataSet(FUpdateQuery).FLastInsertId;

    if (roAfterInsert in FDataSet.RefreshOptions) and RefreshAfterInsertAllowed then
      PerformRefreshRecordInUpdate;
  end;
end;

procedure TDADataSetUpdater.EndUpdate(Success: boolean);
var
  vConnection: TCustomDAConnection;
begin
  // when record was locked and nothing was done
  if not Success and (GetLockMode = lmOptimistic) then
    PerformUnlock;

  // when transaction was started by lock operation AutoCommit on internal layer is not performed
  if //Success and
    (FFixedTransaction <> nil) and FDataSetService.IsAutoCommit and
    (FLockTrStarted in [ltsOnLock, ltsOnLockCachedUpdates])
  then begin
    // for InterBase autocommit is performed in DoAferExecute
    vConnection := UsedConnection;
    if not vConnection.IsMultipleTransactionsSupported then
      FFixedTransaction.Commit
    else
      FFixedTransaction.AutoCommitTransaction(vConnection.AutoCommit and FDataSet.AutoCommit);
    FFixedTransaction := nil;
  end;
end;

function TDADataSetUpdater.PerformUpdateDelete(const StatementType: TStatementType): boolean;
begin
{$IFNDEF VER25P}
  Result := False;
{$ENDIF}

  BeginConnection;
  try
    if GetLockMode = lmOptimistic then
      if (not FDataSet.CachedUpdates) or
         (FDataSet.InternalGetUpdateResult = urApplied) or
         (FDataSet.UpdateStatus = usUnmodified)
      then begin
        // set savepoint for Lock in the CachedUpdate mode
        if FDataSet.CachedUpdates then
          PrepareCachedUpdate;

        try
          PerformLock; // can raise exception after lock
        except
          // savepoint can be lost if connection is lost
          // or exception can be raised before set savepoint
          // so ignore all errors on rollback to savepoint
          try
            // remove lock from the current record
            PerformUnlock;
          except
          end;
          if FDataSet.CachedUpdates then
          try
            // for disconnected mode: if cache doesn't have changed records
            // then finish transaction and close connection
            UnPrepareCachedUpdate;
          except
          end;

          raise;
        end;
      end;

    try
      Result := PerformSQL('', [StatementType]);

      if (StatementType = stUpdate) and
        Result and (roAfterUpdate in FDataSet.RefreshOptions) and
        (FDataSet.Options.FullRefresh or not FDataSetService.IsDMLRefresh)
      then
        PerformRefreshRecordInUpdate;

      EndUpdate(Result);
    except
      if not FDataSet.CachedUpdates then begin
        if GetLockMode <> lmNone then
          // savepoint can be lost if connection is lost
          // or exception can be raised before set savepoint
          // so ignore all errors on rollback to savepoint
          try
            // remove lock from the current record
            PerformUnlock;
          except
          end;
      end
      else if GetLockMode = lmOptimistic then begin
        // savepoint can be lost if connection is lost
        // or exception can be raised before set savepoint
        // so ignore all errors on rollback to savepoint
        try
          // remove lock from the current record
          PerformUnlock;
        except
        end;
        try
          // for disconnected mode: if cache doesn't have changed records
          // then finish transaction and close connection
          UnPrepareCachedUpdate;
        except
        end;
      end;

      raise;
    end;

  finally
    EndConnection;
  end;
end;

function TDADataSetUpdater.PerformDelete: boolean;
begin
  Result := PerformUpdateDelete(stDelete);
end;

function TDADataSetUpdater.PerformUpdate: boolean;
begin
  Result := PerformUpdateDelete(stUpdate);
end;

function TDADataSetUpdater.PerformRefreshRecord: boolean;
begin
  if FDataSet.CachedUpdates and not FDataSet.InCacheProcessing and
    (FDataSet.UpdateStatus = usModified) then
    FDataSet.RevertRecord; {TODO -cMemoryLeak: cause memory leak and DisposeBuf failed}

  FDataSet.FreeRefComplexFields(IntPtr(FDataSet.ActiveBuffer));
  Result := PerformSQL('', [stRefresh]);
end;

function TDADataSetUpdater.PerformRefreshRecordInUpdate: boolean;
begin
  FRefreshInUpdate := True;
  try
    Result := PerformRefreshRecord;
  finally
    FRefreshInUpdate := False;
  end;
end;

function TDADataSetUpdater.PerformRefreshQuick(CheckDeleted: boolean): boolean;
var
  KeyFieldDescs: TFieldDescArray;
  OldStrictUpdate, OldFiltered: boolean;
  KeyFields: TFieldArray;
  Values: variant;
  KeyFieldsCount: integer;
begin
  Result := True;
  FDataSet.DoBeforeRefresh;
  BeginConnection;
  try
    Assert(FDataSet.FIRecordSet <> nil);
    FDataSet.FIRecordSet.GetKeyFieldDescs(KeyFieldDescs);
    KeyFieldsCount := Length(KeyFieldDescs);
    if KeyFieldsCount = 0 then
      DatabaseError(SKeyFieldsReq);

    FDataSet.CheckBrowseMode;
    if FDataSet.FIRecordSet.UpdatingTableInfoIdx = - 1 then
      Exit;

    OldStrictUpdate := FDataSet.Options.StrictUpdate;
    OldFiltered := FDataSet.Filtered;
    FDataSet.DisableControls;
    try
      FDataSet.Filtered := False;
      FDataSet.Options.StrictUpdate := False;
      FDataSet.GetCurrentKeysAndValues(KeyFieldDescs, KeyFields, Values);
      FDataSet.FRowsAffected := 0;

      if CheckDeleted and not FDataSet.IsEmpty then
        PerformSQL('', [stRefreshCheckDeleted]);

      PerformSQL('', [stRefreshQuick]);

      if not FDataSet.LocateEx(KeyFields, KeyFieldDescs, Values, []) then
        FDataSet.First;
    finally
      FDataSet.Options.StrictUpdate := OldStrictUpdate;
      FDataSet.Filtered := OldFiltered;
      FDataSet.EnableControls;
      FDataSet.DoAfterRefresh;
    end;
  finally
    EndConnection;
  end;
end;

function TDADataSetUpdater.CacheChanged: boolean;
begin
  UnPrepareCachedUpdate;
  Result := True;
end;

function TDADataSetUpdater.CacheApplied: boolean;
begin
  FinishCachedUpdate;
  Result := True;
end;

function TDADataSetUpdater.CacheCanceled: boolean;
begin
  UnLockCachedUpdate;
  Result := True;
end;

function TDADataSetUpdater.BatchUpdateAllowed: boolean;
begin
  Result := True;
end;

function TDADataSetUpdater.BatchUpdate: boolean;
begin
  Result := BatchUpdateAllowed and
            FDataSet.InCacheProcessing and
            (FDataSet.Options.UpdateBatchSize > 1) and
            not (GetUpdateObject <> nil);
end;

function TDADataSetUpdater.CanFlushBatch: boolean;
begin
  Result := BatchUpdate and (FBatchStatements > 0) and (FBatchStatements >= FDataSet.Options.UpdateBatchSize);
end;

function TDADataSetUpdater.PrepareBatch(const SQL: string): string;
begin
  Result := SQL;
end;

procedure TDADataSetUpdater.FlushBatch;
begin
  if FBatchStatements > 0 then
    try
      PerformSQL('', [stBatchUpdate]);
    finally
      ClearBatch;
    end;
end;

procedure TDADataSetUpdater.ClearBatch;
begin
  FBatchSQLs.Length := 0;
  FBatchParams.Clear;
  FBatchStatements := 0;
end;

function TDADataSetUpdater.SelectDbValue(const OperationName, SQL: string): variant;
var
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
  UpdateQuery: TCustomDADataSet;
begin
  CheckUpdateQuery(stCustom);

  UpdateQuery := TCustomDADataSet(FUpdateQuery);
  UpdateQuery.SQL.Text := SQL;

  BeginConnection;
  try
    MonitorClass := TDASQLMonitorClass(UsedConnection.SQLMonitorClass);
    if not FDataSet.FLockDebug and (MonitorClass.HasMonitor or FDataSet.Debug) then
      MonitorClass.SQLExecute(FDataSet, SQL, UpdateQuery.Params, OperationName, MessageID, True);

    UpdateQuery.Execute;
    Result := UpdateQuery.Fields[0].Value;
    UpdateQuery.Close;

    if not FDataSet.FLockDebug and (MonitorClass.HasMonitor or FDataSet.Debug) then
      MonitorClass.SQLExecute(FDataSet, SQL, UpdateQuery.Params, OperationName, MessageID, False);
  finally
    EndConnection;
  end;
end;

function TDADataSetUpdater.GetDefaultExpressionValue(DefExpr: string; out Value: variant): boolean;
begin
  Result := False;
end;

procedure TDADataSetUpdater.UnprepareUpdateObjects;
var
  stIdx: TStatementType;
  UpdateComponent: TComponent;
begin
  for stIdx := Low(FUpdateComponents) to High(FUpdateComponents) do begin
    UpdateComponent := FUpdateComponents[stIdx];
    if UpdateComponent <> nil then begin
      if UpdateComponent is TCustomDADataSet then
        TCustomDADataSet(UpdateComponent).UnPrepare
      else if UpdateComponent is TCustomDASQL then
        TCustomDASQL(UpdateComponent).UnPrepare;

      ReleaseParams(UpdateComponent, True);
    end;
  end;
end;

procedure TDADataSetUpdater.ReleaseParams(UpdateQuery: TComponent; AllParams: Boolean);
var
  i: Integer;
  UpdateParams: TDAParams;
  UpdateParam: TDAParam;
  UpdateCommand: TCRCommand;
  UpdateParamDescs: TParamDescs;
  UpdateParamDesc: TParamDesc;
begin
  UpdateParams := TDBAccessUtils.GetParams(FUpdateQuery);
  for i := 0 to UpdateParams.Count - 1 do begin
    UpdateParam := UpdateParams[i];
    if UpdateParam.ValueCount > 1 then
      UpdateParam.FreeValues
    else
      UpdateParam.FreeObject;
  end;

  UpdateCommand := TDBAccessUtils.GetICommand(FUpdateQuery);
  if UpdateCommand <> nil then begin
    UpdateParamDescs := UpdateCommand.Params;
    for i := 0 to UpdateParamDescs.Count - 1 do begin
      UpdateParamDesc := UpdateParamDescs[i];
      if AllParams or UpdateParamDesc.IsObjectValue then
        UpdateParamDesc.SetDataType(dtUnknown);
    end;
  end;
end;

function TDADataSetUpdater.GetLockMode: TLockMode;
begin
  Result := FDataSet.LockMode;
end;

function TDADataSetUpdater.GetUpdateObject: TCustomDAUpdateSQL;
begin
  Result := FDataSet.UpdateObject;
end;

function TDADataSetUpdater.GetUpdateSQL(StatementType: TStatementType): string;
begin
  Result := FDataSet.FUpdateSQL[StatementType].Text;
end;

function TDADataSetUpdater.GetIRecordSet: TCRRecordSet;
begin
  Result := FDataSet.FIRecordSet;
end;

function TDADataSetUpdater.GetICommand: TCRCommand;
begin
  Result := FDataSet.FICommand;
end;

procedure TDADataSetUpdater.CheckIRecordSet;
begin
  FDataSet.CheckIRecordSet;
end;

function TDADataSetUpdater.UsedConnection: TCustomDAConnection;
begin
  Result := FDataSet.UsedConnection;
end;

function TDADataSetUpdater.UsedTransaction: TDATransaction;
begin
  Result := FDataSet.UsedTransaction;
end;

function TDADataSetUpdater.UsedUpdateTransaction: TDATransaction;
begin
  Result := FDataSet.UsedUpdateTransaction;
end;

procedure TDADataSetUpdater.SetRowsAffected(Value: Integer);
begin
  FDataSet.FRowsAffected := Value;
end;

procedure TDADataSetUpdater.BeginConnection;
begin
  FDataSet.BeginConnection;
end;

procedure TDADataSetUpdater.EndConnection;
begin
  FDataSet.EndConnection;
end;

{ TDADataSetService }

constructor TDADataSetService.Create(AOwner: TMemDataSet);
begin
  FDataSet := TCustomDADataSet(AOwner);

  inherited;

  CreateSQLGenerator;
end;

destructor TDADataSetService.Destroy;
begin
  FreeSQLGenerator;
  FUpdater := nil;

  inherited;
end;

function TDADataSetService.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeySequence:
      Value := FSQLGenerator.KeySequence;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TDADataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeySequence:
      FSQLGenerator.KeySequence := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TDADataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  inherited;

  FUpdater := TDADataSetUpdater(Value);
  if FUpdater <> nil then
    FUpdater.CheckMode := FDataSet.CheckMode;
end;

procedure TDADataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TDASQLGenerator.Create(TDASQLGeneratorService));
end;

procedure TDADataSetService.FreeSQLGenerator;
begin
  FSQLGenerator.Free;
  FSQLGenerator := nil;
end;

procedure TDADataSetService.SetSQLGenerator(Value: TDASQLGenerator);
begin
  FreeSQLGenerator;
  FSQLGenerator := Value;

  if FDataSet.FIRecordSet <> nil then
    FDataSet.FIRecordSet.SQLGenerator := Value;

  if FSQLGenerator <> nil then begin
    TDASQLGeneratorService(FSQLGenerator.Service).FDataSet := FDataSet;
    FSQLGenerator.IRecordSet := FDataSet.FIRecordSet;
    FSQLGenerator.DesignMode := csDesigning in FDataSet.ComponentState;
    FSQLGenerator.DMLRefresh := FDataSet.DMLRefresh;
  end;
end;

procedure TDADataSetService.PreInitCursor;
begin
  if FFieldsExtInited then
    exit;

  GetIRecordSet.InitExtFieldsInfo;
  SetFieldsReadOnly;

  FFieldsExtInited := True;

  inherited;
end;

procedure TDADataSetService.InitCursor;
var
  i: integer;
  HiddenFields: TFieldArray;
  Field: TField;
  FieldDesc: TCRFieldDesc;
begin
  InitFieldsOptions;

  for i := 0 to FDataSet.Fields.Count - 1 do begin
    Field := FDataSet.Fields[i];
    if Field.FieldKind <> fkData then
      continue;

    FieldDesc := TCRFieldDesc(FDataSet.GetFieldDesc(Field));
    SetFieldOrigin(Field, FieldDesc);

  {$IFNDEF FPC}
    if FieldDesc.IsAutoIncrement then
      Field.AutoGenerateValue := arAutoInc;
  {$ENDIF}
  end;

  HiddenFields := nil;
{$IFDEF VER20P}
  if not (lcPersistent in FDataSet.Fields.LifeCycles) then begin
{$ELSE}
  if FDataSet.DefaultFields then begin
{$ENDIF}
    HiddenFields := DetectHiddenFields;
    for i := 0 to High(HiddenFields) do begin
      HiddenFields[i].Visible := False;
      HiddenFields[i].ReadOnly := True;
    end;
  end;

  if Trim(SQLGenerator.KeySequence) <> '' then begin
    GetIRecordSet.DetectKeyGeneratorField;
    if GetIRecordSet.KeyGeneratorField <> nil then begin
      Field := FDataSet.GetField(GetIRecordSet.KeyGeneratorField);
      if Field <> nil then
        Field.Required := False;
    end;
  end
  else
    GetIRecordSet.ClearKeyGeneratorField;
end;

procedure TDADataSetService.CloseCursor;
begin
end;

procedure TDADataSetService.InitUpdatingTable;
begin
  SetFieldsReadOnlyOld;
  InitFieldsOptions;
end;

procedure TDADataSetService.InitFieldsOptions;
begin
  UpdateFieldsOptions;

{$IFDEF WITH_IPROVIDER}
  if GetIRecordSet.TablesInfo.Count > 0 then
    FDataSet.FOldKeyFields := FDataSet.PSGetKeyFields;
{$ENDIF}

  if FDataSet.Options.DefaultValues then
    FillFieldsDefaultValues;
end;

procedure TDADataSetService.UpdateFieldsOptions;
var
  i: integer;
begin
  for i := 0 to FDataSet.Fields.Count - 1 do
    if not FDataSet.Fields[i].ReadOnly then begin
      FIsAnyFieldCanBeModified := True;
      Exit;
    end;
  FIsAnyFieldCanBeModified := False;
end;

procedure TDADataSetService.SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc);
var
  TblName: string;
  TblAlias: string;
begin
  if FDataSet.Options.FieldOrigins <> foNone then begin
    Field.Origin := FieldDesc.ActualName;

    if FieldDesc.TableInfo <> nil then
      case FDataSet.Options.FieldOrigins of
        foTableAndField: begin
          TblName := FDataSet.SQLInfo.NormalizeName(FieldDesc.TableInfo.TableName, False, True);
          if TblName <> '' then
            Field.Origin := TblName + '.' + Field.Origin;
        end;
        foTableAliasAndField: begin
          TblAlias := FDataSet.SQLInfo.NormalizeName(FieldDesc.TableInfo.TableAlias, False, True);
          if TblAlias <> '' then
            Field.Origin := TblAlias + '.' + Field.Origin;
        end;
      end;
  end;
end;

procedure TDADataSetService.FillFieldsDefaultValues;
var
  Field: TField;
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  for i := 0 to FDataSet.Fields.Count - 1 do begin
    Field := FDataSet.Fields[i];
    if Field.FieldKind <> fkData then
      continue;

    FieldDesc := TCRFieldDesc(FDataSet.GetFieldDesc(Field));
    if FieldDesc <> nil then
      Field.DefaultExpression := FieldDesc.DefaultExpr;
  end;
end;

procedure TDADataSetService.SetFieldsReadOnly;
begin
  if OldFieldsReadOnly and not FDataSet.Options.SetFieldsReadOnly then
    exit;

  GetIRecordSet.SetFieldsReadOnly;
end;

procedure TDADataSetService.SetFieldsReadOnlyOld;
var
  i, j: integer;
  FieldDesc: TFieldDesc;
  DataFieldDescs: TFieldDescArray;

  function GetRootParent(FieldDesc: TFieldDesc): TFieldDesc;
  begin
    Result := FieldDesc;
    while Result.ParentField <> nil do
      Result := Result.ParentField;
  end;

begin
  if not FDataSet.ReadOnly then begin
    if FDataSet.Options.SetFieldsReadOnly then begin

      for i := 0 to FDataSet.Fields.Count - 1 do
        if FDataSet.Fields[i].FieldKind = fkData then
          FDataSet.Fields[i].ReadOnly := True;

      for i := 0 to FDataSet.FIRecordSet.Fields.Count - 1 do
        FDataSet.FIRecordSet.Fields[i].ReadOnly := False;

      FDataSet.FIRecordSet.GetDataFieldDescs(DataFieldDescs);

      for i := 0 to FDataSet.Fields.Count - 1 do begin
        FieldDesc := FDataSet.GetFieldDesc(FDataSet.Fields[i]);
        for j := 0 to High(DataFieldDescs) do
          if (FieldDesc = DataFieldDescs[j]) or (GetRootParent(FieldDesc) = DataFieldDescs[j]) then begin
            FDataSet.Fields[i].ReadOnly := False;
            Break;
          end;
      end;
    end;
  end;
end;

function TDADataSetService.DetectHiddenFields: TFieldArray;
begin
  SetLength(Result, 0);
end;

function TDADataSetService.DetectCanModify: boolean;
begin
  Result := not (FDataSet.ReadOnly or FDataSet.UniDirectional) and
    (FDataSet.LocalUpdate or
    FDataSet.CachedUpdates and
    Assigned(FDataSet.OnUpdateRecord) or
    Assigned(FDataSet.UpdateObject));
end;

function TDADataSetService.GetRecCount: integer;
begin
  Result := 0;
end;

procedure TDADataSetService.BreakExec;
begin

end;

function TDADataSetService.Executing: boolean;
begin
  Result := False;
end;

function TDADataSetService.GetCurrentSchema: string;
begin
  Result := '';
end;

procedure TDADataSetService.InitMasterParams(Params: TDAParams);
begin

end;

procedure TDADataSetService.WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: string; XMLWriter: XMLTextWriter);
begin
  inherited;

  if (FieldDesc is TCRFieldDesc) and
     (TCRFieldDesc(FieldDesc).TableInfo <> nil) and
     (TCRFieldDesc(FieldDesc).TableInfo.TableName <> '')
  then
    XmlWriter.WriteAttributeString('rs:basetable', TCRFieldDesc(FieldDesc).TableInfo.TableName);
end;

procedure TDADataSetService.ClearFieldDescRefs;
begin
  FFieldsExtInited := False;
  if FDataSet.FIRecordSet <> nil then
    FDataSet.FIRecordSet.ClearCachedFieldDescs;
end;

procedure TDADataSetService.ResetTableKeyFields;
begin
end;

function TDADataSetService.GetIdentityField: TCRFieldDesc;
begin
  if FDataSet.FIRecordSet <> nil then
    Result := FDataSet.FIRecordSet.IdentityField
  else
    Result := nil;
end;

function TDADataSetService.GetKeyGeneratorField: TCRFieldDesc;
begin
  if FDataSet.FIRecordSet <> nil then
    Result := FDataSet.FIRecordSet.KeyGeneratorField
  else
    Result := nil;
end;

procedure TDADataSetService.SetKeyGeneratorValue(const Value: variant);
var
  Field: TField;
begin
  Field := FDataSet.GetField(KeyGeneratorField);
  if Field <> nil then
    Field.NewValue := Value;
end;

function TDADataSetService.GetDBKeyList(const TableName, IndexName: string): string;
var
  MetaData: TDAMetaData;
  OldIndexName, OldIndexSchema, NewIndexName, NewIndexSchema: string;
  Info: TSQLObjectInfo;
begin
  Result := '';

  BeginConnection; // GetCurrentSchema requires an active connection
  try
    FDataSet.SQLInfo.SplitObjectName(TableName, Info);
    if Info.Schema = '' then
      Info.Schema := GetCurrentSchema;

    MetaData := TDAMetaData.Create(nil);
    try
      OldIndexSchema := '';
      OldIndexName := '';
      MetaData.Connection := UsedConnection;
      MetaData.Transaction := FDataSet.UsedTransaction;
      try
        MetaData.MetaDataKind := 'indexcolumns';
        MetaData.Restrictions.Text := 'table_catalog=' + Info.Catalog +
          #13#10'table_schema=' + Info.Schema +
          #13#10'table_name=' + Info.Name +
          #13#10'index_name=' + IndexName +
          #13#10'unique=1';
        MetaData.Open;
        while not MetaData.Eof do begin
          NewIndexName := VarToStr(MetaData.FieldByName('INDEX_NAME').Value);
          NewIndexSchema := VarToStr(MetaData.FieldByName('INDEX_SCHEMA').Value);
          if (Result <> '') and
             (OldIndexName <> '') and
             ((NewIndexName <> OldIndexName) or (NewIndexSchema <> OldIndexSchema))
          then
            Break;
          if Result <> '' then
            Result := Result + ';';
          Result := Result + VarToStr(MetaData.FieldByName('COLUMN_NAME').Value);
          OldIndexName := NewIndexName;
          OldIndexSchema := NewIndexSchema;
          MetaData.Next;
        end;
        MetaData.Close;
      finally
        MetaData.Transaction := nil;
      end;
    finally
      MetaData.Free;
    end;
  finally
    EndConnection;
  end;
end;

function TDADataSetService.OpenNext: boolean;
begin
  raise Exception.Create(SOperationNotSupported);
{$IFDEF FPC}
  Result := False;
{$ENDIF}
end;

function TDADataSetService.NeedParamValuesOnPrepare: boolean;
begin
  Result := False;
end;

function TDADataSetService.GetIConnection: TCRConnection;
begin
  if FDataSet.FICommand <> nil then
    Result := FDataSet.FICommand.GetConnection
  else
    Result := nil;
end;

function TDADataSetService.GetICommand: TCRCommand;
begin
  Result := FDataSet.FICommand;
end;

function TDADataSetService.GetIRecordSet: TCRRecordSet;
begin
  Result := FDataSet.FIRecordSet;
end;

procedure TDADataSetService.CheckIRecordSet;
begin
  FDataSet.CheckIRecordSet;
end;

function TDADataSetService.UsedConnection: TCustomDAConnection;
begin
  Result := FDataSet.UsedConnection;
end;

function TDADataSetService.IsDMLRefresh: boolean;
begin
  Result := FDataSet.DMLRefresh;
end;

function TDADataSetService.IsAutoCommit: boolean;
begin
  Result := FDataSet.AutoCommit;
end;

procedure TDADataSetService.SetAutoCommit(Value: boolean);
begin
  FDataSet.AutoCommit := Value;
end;

function TDADataSetService.IsFetchAll: boolean;
begin
  Result := FDataSet.FetchAll;
end;

procedure TDADataSetService.SetNeedAddRef(Value: boolean);
begin
  FDataSet.FNeedAddRef := Value;
end;

procedure TDADataSetService.BeginConnection;
begin
  FDataSet.BeginConnection;
end;

procedure TDADataSetService.EndConnection;
begin
  FDataSet.EndConnection;
end;

{ TDASQLGeneratorService }

procedure TDASQLGeneratorService.RaiseError(const Message: string);
begin
  DatabaseError(Message);
end;

function TDASQLGeneratorService.GetOldRecBuf: IntPtr;
begin
  Result := FDataSet.GetOldRecBuf;
end;

function TDASQLGeneratorService.GetNewRecBuf: IntPtr;
begin
  Result := FDataSet.GetNewRecBuf;
end;

function TDASQLGeneratorService.BlobFieldModified(FieldDesc: TCRFieldDesc): boolean;
var
  Field: TField;
begin
  // for Modified can be set manually for TBlobField
  Field := FDataSet.GetField(FieldDesc);
  Result := (Field is TBlobField) and TBlobField(Field).Modified;
end;

function TDASQLGeneratorService.GetFieldObject(FieldDesc: TFieldDesc): TSharedObject;
begin
  Result := FDataSet.GetFieldObject(FieldDesc);
end;

function TDASQLGeneratorService.GetUpdateCommand: TCRCommand;
begin
  if FDataSet.FDataSetService.FUpdater.FUpdateQuery <> nil then
    Result := TDBAccessUtils.GetICommand(FDataSet.FDataSetService.FUpdater.FUpdateQuery)
  else
    Result := nil;
end;

function TDASQLGeneratorService.GetDBKeyList(const TableName, IndexName: string): string;
begin
  Result := FDataSet.FDataSetService.GetDBKeyList(TableName, IndexName);
end;

function TDASQLGeneratorService.ParamExists(const ParamName: string): boolean;
begin
  Result := TDBAccessUtils.GetParams(FDataSet).FindParam(ParamName) <> nil;
end;

function TDASQLGeneratorService.BaseSQL: string;
begin
  Result := FDataSet.BaseSQL;
end;

function TDASQLGeneratorService.FinalSQL: string;
begin
  Result := FDataSet.FinalSQL;
end;

function TDASQLGeneratorService.FilterSQL: string;
begin
  Result := FDataSet.FilterSQL;
end;

{ TCustomDASQL }

constructor TCustomDASQL.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := SQLChanged;
  FParams := CreateParamsObject;
  FParamCheck := True;
  if Owner is TCustomDADataSet then
    FMacros := TMacros.Create(Owner)
  else
    FMacros := TMacros.Create(Self);
  FChangeCursor := True;

  FLockAssembleSQL :=False;
  FLockMacros := False;
  FLockScanParams := False;

  FDesignCreate := csDesigning in ComponentState;
end;

destructor TCustomDASQL.Destroy;
begin
  UnPrepare;

  if UsedConnection <> nil then
    UsedConnection.UnRegisterClient(Self);

{$IFDEF FPC}
  FMacros.FOwner := nil; // to prevent assemble SQL on removing items
{$ENDIF}
  FMacros.Free;
  FParams.Clear; // To prevent SharedObj leak on CLR
  FParams.Free;
  FSQL.Free;

  if FDataSet = nil then
    FreeICommand;

  if (FDataSet = nil) and (UsedConnection <> nil) then
    TDASQLMonitorClass(UsedConnection.SQLMonitorClass).ObjectDestroyed(Self);

  inherited;
end;

procedure TCustomDASQL.CreateICommand;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then
    SetICommand(vConnection.CreateICommand)
  else
    SetICommand(nil);
end;

procedure TCustomDASQL.FreeICommand;
begin
  FICommand.Free;
  SetICommand(nil);
end;

procedure TCustomDASQL.SetICommand(Value: TCRCommand);
begin
  FICommand := Value;

  if FICommand <> nil then begin
    if FDataSet = nil then begin
      if FConnection <> nil then
        FICommand.SetConnection(FConnection.FIConnection)
      else
        FICommand.SetConnection(nil);

      if FTransaction <> nil then
        FICommand.SetTransaction(FTransaction.FITransaction)
      else
        FICommand.SetTransaction(nil);
    end;

    FICommand.SetProp(prAutoCommit, FAutoCommit);
    FICommand.AfterExecute := DoAfterExecute;
    if (Owner is TCustomDADataSet) or (Owner is TCustomDAConnection) then
      FICommand.Component := Owner
    else
      FICommand.Component := Self;

    FICommand.SetProp(prScanParams, ParamCheck);

    FICommand.SetProp(prIsStoredProc, FStoredProcName <> '');
    FICommand.ReadParams := ReadParams; // Used in SDAC

    FMacros.SetParserClass(FICommand.GetParserClass);

    SetICommandSQL; // in UniDAC FICommand can be recreated on provider change
  end;
end;

procedure TCustomDASQL.CheckICommand;
var
  ClassType: TClass;
  vConnection: TCustomDAConnection;
begin
  if FDataSet <> nil then begin
    FDataSet.CheckIRecordSet;
    Exit;
  end;

  vConnection := UsedConnection;
  if (vConnection <> nil) then
    ClassType := vConnection.GetICommandClass
  else
    ClassType := nil;

  if (ClassType = nil) or not (FICommand is ClassType) then begin
    FreeICommand;
    CreateICommand;
  end;
end;

function TCustomDASQL.CreateParamsObject: TDAParams;
begin
  Result := TDAParams.Create(Self);
end;

function TCustomDASQL.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TDAFieldTypeMap;
end;

procedure TCustomDASQL.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

function TCustomDASQL.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection;
  if (Result = nil) and (FDataSet <> nil) then
    Result := FDataSet.UsedConnection;
end;

procedure TCustomDASQL.CheckConnection;
begin
  BeginConnection(False);
end;

procedure TCustomDASQL.BeginConnection(NoConnectCheck: boolean = True);
var
  UseDefaultConnection: boolean;
  vConnection: TCustomDAConnection;
  vTransaction: TDATransaction;
begin
  vConnection := UsedConnection;
  if vConnection = nil then
    DatabaseError(SConnectionNotDefined);

  if NoConnectCheck then
    vConnection.InternalConnect // We should call connect each time to update ConnectCount
  else
    if not vConnection.Connected then
      vConnection.Connect;

  UseDefaultConnection := (FConnection = nil) and (vConnection.FSQLs.IndexOf(Self) = -1);

  CheckICommand;

  if vConnection.IsMultipleTransactionsSupported then begin
    vTransaction := UsedTransaction;
    if vTransaction = nil then
      DatabaseError(STransactionNotAssigned);

    if NoConnectCheck then
      vTransaction.GainTransaction // We should call each time to update TrStartCount
    else
     if not vTransaction.Active then
       vTransaction.StartTransaction;

    FICommand.SetTransaction(vTransaction.FITransaction);
  end;

  // use default connection
  if UseDefaultConnection then begin
    vConnection.RegisterClient(Self, ConnectChange);

    FICommand.SetConnection(vConnection.FIConnection)
  end;
end;

procedure TCustomDASQL.EndConnection;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection.IsMultipleTransactionsSupported then
    UsedTransaction.ReleaseTransaction; // Release and Stop transaction

  if vConnection <> nil then
    vConnection.InternalDisconnect;
end;

procedure TCustomDASQL.Disconnect(NeedClose: boolean = True);
begin
  if FDataSet = nil then begin
    UnPrepare;
    Params.Disconnect;
    if UsedConnection.Options.DisconnectedMode then
      FIsSPInit := False;
  end
  else
    FDataSet.Disconnect(NeedClose);
end;

procedure TCustomDASQL.ConnectChange(Sender: TObject; Connecting: boolean);
begin
  if not Connecting then
    if FDataSet = nil then
      Disconnect
    else
      Disconnect(not (TCustomDAConnection(Sender).Options.DisconnectedMode or FDataSet.FDisconnected));
end;

function TCustomDASQL.UsedTransaction: TDATransaction;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then begin
    if vConnection.IsMultipleTransactionsSupported then begin
      Result := Transaction;
      if (Result = nil) and (FDataSet <> nil) then
        Result := FDataSet.UsedTransaction;
    end
    else
      Result := nil;

    if Result = nil then
      Result := vConnection.UsedTransaction;
  end
  else
    Result := nil;
end;

procedure TCustomDASQL.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FTransaction) then
    FTransaction := nil;
end;

procedure TCustomDASQL.InternalPrepare;
begin
  WriteParams(False);

  FICommand.Prepare;
end;

procedure TCustomDASQL.Prepare;
var
  v: variant;
  MessageID: cardinal;
  vConnection: TCustomDAConnection;
begin
  if not Prepared then begin
    BeginConnection;

    vConnection := UsedConnection;
    if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLPrepare(Self, FinalSQL, FParams, MessageID, True);

    InternalPrepare;

    FICommand.GetProp(prUseDescribeParams, v);
    if Boolean(v) then
      UpdateParams;

    if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLPrepare(Self, FinalSQL, FParams, MessageID, False);
  end;
end;

procedure TCustomDASQL.InternalUnPrepare;
begin
  FICommand.Unprepare;
end;

procedure TCustomDASQL.UnPrepare;
begin
  if Prepared then begin
    try
      InternalUnPrepare;
    finally
      EndConnection; // Diconnect after no longer prepared
    end;
  end;
end;

procedure TCustomDASQL.InternalCreateProcCall(const Name: string; NeedDescribe: boolean;
  IsQuery: boolean = False);
var
  ProcCallSQL: string;
begin
  if Name = '' then
    DatabaseError(SStoredProcNotDefined);

  SetStoredProcName(Name);

  BeginConnection;
  try
    if NeedDescribe then begin
      ProcCallSQL := FICommand.CreateProcCall(Name, NeedDescribe, IsQuery);
      CreateParams;
    end
    else if GetForceSPInit then begin
      ProcCallSQL := FICommand.CreateProcCall(Name, True, IsQuery);
      WriteParams(False);
    end
    else begin
      WriteParams(False);
      ProcCallSQL := FICommand.CreateProcCall(Name, False, IsQuery);
    end;

    FLockAssembleSQL := True;
    try
      SQL.Text := ProcCallSQL;
    finally
      FLockAssembleSQL := False;
    end;
  finally
    EndConnection;
  end;

  FStoredProcIsQuery := IsQuery;
  FIsSPInit := True;
end;

function TCustomDASQL.GetForceSPInit;
begin
  CheckICommand;
  Result := (not FIsSPInit) and FICommand.ForceCreateSPParams;
end;

procedure TCustomDASQL.InternalExecute(Iters: integer; Offset: integer);
var
  ReExecute: boolean;
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then
    vConnection.PushOperation(clExecute);
  try
    repeat
      ReExecute := False;
      try
        if (Iters = 1) and (Offset = 0) and ((FParams.Count = 0) or (FParams.ValueCount <= 1)) then begin
          FICommand.SetProp(prBatchIters, 1);
          FICommand.SetProp(prBatchOffset, 0);
          FICommand.Execute;
        end
        else
          FICommand.ExecuteBatch(Iters, Offset);
      except
        on E: EFailOver do
          if E.FConnLostCause = clExecute then begin
            Connection.RestoreAfterFailOver; //Restore all read transactions
            ReExecute := True; //We should pass clConnectionApplyUpdates FailOver
          end
          else
            raise;
      end;
    until (not ReExecute);
  finally
    if vConnection <> nil then
      vConnection.PopOperation;
  end;
end;

procedure TCustomDASQL.Execute;
begin
  Execute(1);
end;

procedure TCustomDASQL.Execute(Iters: integer; Offset: integer = 0);
var
  MessageID: cardinal;
  vConnection: TCustomDAConnection;
begin
  if not Executing then begin
    FBatchIters := Iters;
    FBatchOffset := Offset;

    BeginConnection;

    vConnection := UsedConnection;
    if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, True);

    if FChangeCursor then
      if NonBlocking then
        SetCursor(crSQLArrow)
      else
        StartWait;

    DoBeforeExecute;
    WriteParams;
    CheckSQL(Iters);
    InternalExecute(Iters, Offset);

    if not FLockDebug and (TDASQLMonitorClass(vConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(vConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, False);
  end;
end;

procedure TCustomDASQL.DoBeforeExecute;
begin
  if Assigned(FBeforeExecute) then
    FBeforeExecute(self);
end;

procedure TCustomDASQL.DoAfterExecute(Result: boolean);
var
  vConnection: TCustomDAConnection;
  Value: variant;
  AutoCommitUsed: boolean;
begin
  vConnection := UsedConnection;

  if Result then begin
    ReadParams;

    vConnection.FIConnection.GetProp(prLastInsertId, Value);
    FLastInsertId := Value;
  end;

  if FChangeCursor and NonBlocking then
    StopWait;

  AutoCommitUsed := vConnection.AutoCommit and AutoCommit;

  if vConnection.Options.DisconnectedMode and vConnection.Connected then
    vConnection.DetectInTransaction(not AutoCommitUsed);

  if vConnection.IsMultipleTransactionsSupported then
    UsedTransaction.AutoCommitTransaction(AutoCommitUsed);

  EndConnection; //we should read all Out parameters before disconnect, so
                 //in NonBlocking Mode this event must be called exactly after server execute
  if Assigned(FAfterExecute) then
    FAfterExecute(Self, Result);
end;

procedure TCustomDASQL.BreakExec;
begin
  if FICommand <> nil then
    FICommand.BreakExec;
end;

function TCustomDASQL.Executing: boolean;
begin
  if FICommand <> nil then
    Result := FICommand.Executing
  else
    Result := False;
end;

function TCustomDASQL.WaitExecuting(TimeOut: integer): boolean;
{$IFDEF MSWINDOWS}
var
  Msg: TMSG;
  StartT: {$IFNDEF FPC}DWORD{$ELSE}UInt64{$ENDIF};
  DeltaT: {$IFNDEF FPC}DWORD{$ELSE}UInt64{$ENDIF};
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  StartT := {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF};
  DeltaT := {$IFNDEF FPC}DWORD{$ELSE}UInt64{$ENDIF}(TimeOut * 1000);
  while Executing and ((TimeOut = 0) or (GetTickInterval(StartT, {$IFNDEF FPC}GetTickCount{$ELSE}GetTickCount64{$ENDIF}) < DeltaT)) do
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then begin
      if Msg.Message <> WM_QUIT then begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
{$ELSE}
  while Executing do;
{$ENDIF}

  Result := not Executing;
end;

procedure TCustomDASQL.ScanMacros;
begin
  if FDataSet = nil then
    FMacros.Scan(FSQL.Text)
  else
    FDataSet.ScanMacros{$IFDEF FPC}(nil){$ENDIF};
end;

function TCustomDASQL.GetFinalSQL: string;
var
  i: Integer;
begin
  Result := TRIM(FSQL.Text);

  // Copied from SysUtils
  i := Length(Result);
  while (i > 0) and (Result[i] <= ' ') do
    Dec(i);
  SetLength(Result, i);

  if FMacros.Count > 0 then
    FMacros.Expand(Result);
end;


function TCustomDASQL.ParseSQL(const SQL: string; Params: TDAParams): string;
var
  ParamDescs: TParamDescs;
begin
  CheckICommand;
  ParamDescs := TParamDescs.Create;
  try
    Result := FICommand.ParseSQL(SQL, ParamDescs);
    CreateParams(Params, ParamDescs);
  finally
    ParamDescs.Free;
  end;
end;

procedure TCustomDASQL.SetICommandSQL;
var
  FinSQL: string;
begin
  FinSQL := '';
  if FDataSet = nil then begin
    if SQL.Count > 0 then // don't generate SQL here
      FinSQL := FinalSQL;
  end
  else begin
    if FDataSet.SQL.Count > 0 then // don't generate SQL here
      FinSQL := FDataSet.FinalSQL;
  end;

  if FLockScanParams then begin
    FICommand.SetProp(prDisableParamScan, True);
    try
      FICommand.SetSQL(FinSQL);
    finally
      FICommand.SetProp(prDisableParamScan, False);
    end;
  end
  else
    FICommand.SetSQL(FinSQL); // replace parameters if it's needed

  FSQLModified := False;
end;

procedure TCustomDASQL.AssembleSQL;
var
  List: TDAParams;
  OldICommand: TCRCommand;
begin
{$IFDEF PERF_COUNTER}
  //PerfCounters[3].Start;
{$ENDIF}

  OldICommand := FICommand;
  // FDataSet.FinalSQL uses TablesInfo from FIRecordSet
  CheckICommand;

  if FICommand = OldICommand then
    SetICommandSQL;
  // else SetICommandSQL was called from SetICommand

  if (ParamCheck or (csDesigning in ComponentState)) and not FLockScanParams
  then begin
    List := CreateParamsObject;
    try
      // Internal param parsing
      List.Assign(FParams);
      CreateParams;
      FParams.AssignValues(List);
    finally
      List.Clear;
      List.Free;
    end;
  end;
{$IFDEF PERF_COUNTER}
  //PerfCounters[3].Stop;
{$ENDIF}
end;

function TCustomDASQL.NeedRecreateProcCall: boolean;
begin
  Result := False;
end;

procedure TCustomDASQL.CheckSQL(Iters: integer);
var
  OldSQL, NewUserSQL: string;
  OldPrepared: boolean;
begin
  if FSQLModified then
    SetICommandSQL; // for UniDAC

  OldSQL := FICommand.SQL;
  NewUserSQL := '';
  OldPrepared := Prepared;

  // NeedRecreateProcCall used for MSSQL that has a flag for default params
  if (Iters = 1) and (FStoredProcName <> '') and NeedRecreateProcCall then
    // if SQL = '' then we should call InitProcParams
    NewUserSQL := FICommand.CreateProcCall(FStoredProcName, (SQL.Count = 0), FStoredProcIsQuery);

  if FICommand.SQL <> OldSQL then begin
    if OldPrepared then
      if FDataSet = nil then
        Unprepare
      else
        FDataSet.UnPrepare;

    if NewUserSQL <> '' then begin
      FLockAssembleSQL := True;
      try
        SQL.Text := NewUserSQL;
      finally
        FLockAssembleSQL := False;
      end;
    end;

    if OldPrepared then
      if FDataSet = nil then
        Prepare
      else
        FDataSet.Prepare;
  end;
end;

// creates TDAParam objects if parameters was parsed by FICommand
procedure TCustomDASQL.CreateParams;
begin
  CreateParams(FParams, FICommand.Params);
end;

procedure TCustomDASQL.CreateParams(Params: TDAParams; ParamDescs: TParamDescs);
var
  i: integer;
begin
  Params.BeginUpdate;
  try
    Params.Clear;
    for i := 0 to ParamDescs.Count - 1 do
      AssignParamDesc(Params.Add as TDAParam, ParamDescs[i]);
  finally
    Params.EndUpdate;
  end;
end;

// Write values of parameters to FICommand
procedure TCustomDASQL.WriteParams(WriteValue: boolean = True);
var
  Param: TDAParam;
  ParamDesc: CRAccess.TParamDesc;
  ParamInfo: TDAParamInfo;
  i: integer;
begin
  for i := 0 to Params.Count - 1 do begin
    Param := Params[i];

    if (Params.FParamsChangeType = ctUsers) and (Param.Name <> '') then
      ParamDesc := FICommand.Params.FindParam(Param.Name)
    else
      if i < FICommand.Params.Count then
        ParamDesc := FICommand.Params[i]
      else
        ParamDesc := nil;
    if ParamDesc = nil then
      ParamDesc := FICommand.AddParam;

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

  if Params.FParamsChangeType <> ctUsers then
    while Params.Count < FICommand.Params.Count do
      FICommand.Params.Delete(FICommand.Params.Count - 1);
  Params.FParamsChangeType := ctGenerated;

  for i := 0 to FICommand.ParamsInfo.Count - 1 do begin
    ParamInfo := FICommand.ParamsInfo.Items[i];
    if (ParamInfo.ParamRef = nil) and (ParamInfo.ParamName <> '') then
      ParamInfo.ParamRef := FICommand.Params.FindParam(ParamInfo.ParamName);
  end;
end;

// Read values of parameters from FICommand
procedure TCustomDASQL.ReadParams;
var
  CanReadParams: Variant;
  i: integer;
  Param: TDAParam;
  ParamDesc: TParamDesc;
begin
  Assert(FICommand <> nil);
  FICommand.GetProp(prCanReadParams, CanReadParams);
  if CanReadParams then begin
    for i := 0 to FParams.Count - 1 do begin
      Param := FParams[i];
      ParamDesc := FICommand.Params[i];
      if (Param.ParamType <> ptInput) and
        (IsInOutParamSupported or (Param.ParamType <> ptUnknown)) // if in/out not supported treat Unknown as Input
      then begin
        if ParamDesc <> nil then
          AssignParamDescValue(Param, ParamDesc);
      end;
    end;
    FICommand.SetProp(prCanReadParams, False); // For SDAC
  end;
end;

procedure TCustomDASQL.UpdateParams;
var
  i: integer;
  Param: TDAParam;
begin
  Params.BeginUpdate;
  try
    for i := 0 to FICommand.Params.Count - 1 do begin
      if i < Params.Count then
        Param := Params[i]
      else
        Param := Params.Add as TDAParam;
      AssignParamDesc(Param, FICommand.Params[i]);
      if (FDataSet <> nil) and (FDataSet.FDataSetService <> nil) and FDataSet.FDataSetService.NeedParamValuesOnPrepare then
        AssignParamDescValue(Param, FICommand.Params[i]);
    end;
    for i := Params.Count - 1 downto FICommand.Params.Count do
      Params.Delete(i); // TCollection.Delete() calls Free
  finally
    Params.EndUpdate;
  end;
end;

function TCustomDASQL.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TCustomDASQL.NeedConvertEOLForBlob: boolean;
begin
  Result := False;
end;

procedure TCustomDASQL.AssignParam(ParamDesc: TParamDesc; Param: TDAParam);
var
  ParamSize: integer;
  PramMinSize: Integer;
  ParamDataType: TFieldType;
begin
  ParamDataType := Param.DataType; // cache DataType value

  ParamDesc.SetName(Param.Name);
  ParamDesc.SetDataType(GetFieldTypeMapClass.GetDataType(ParamDataType, Param.SubDataType));
  ParamDesc.SetSubDataType(Param.SubDataType);
  ParamDesc.SetNational(Param.National);
  ParamDesc.SetEncryptor(Param.FEncryptor);
  ParamDesc.SetParamType(TParamDirection(Param.ParamType));
  ParamDesc.SetIsBound(Param.Bound);
  ParamDesc.SetArraySize(Param.ValueCount);
  ParamDesc.SetPrecision(Param.Precision);
  ParamDesc.SetScale(Param.NumericScale);

  ParamSize := Param.Size;
  if (Integer(ParamDataType) in [Integer(ftString), Integer(ftFixedChar),
                                 Integer(ftWideString), Integer(ftFixedWideChar),
                                 Integer(ftBytes), Integer(ftVarBytes)])
  then begin
    if Param.Size = 0 then begin
      if ((Param.ParamType = ptInput) or
         (Param.ParamType = ptUnknown) and not IsInOutParamSupported) and // if in/out not supported treat Unknown as Input
         (Param.ValueCount <= 1)
      then begin
        case VarType(Param.Value) of
          varArray + varByte:
            ParamSize := VarArrayHighBound(Param.Value, 1) + 1;
        else
          if VarIsStr(Param.Value) then begin
            ParamSize := Length({$IFNDEF NEXTGEN}AnsiString{$ENDIF}(VarToStr(Param.Value)));
            if ParamSize = 0 then
              ParamSize := 1;
          end;
        end;

        PramMinSize := ParamDesc.GetMinDefaultSize;
        if ParamSize < PramMinSize then
          ParamSize := PramMinSize;
      end;

      if ParamSize = 0 then
        ParamSize := ParamDesc.GetMaxStringSize(UsedConnection.FIConnection);
    end;
  end;

  ParamDesc.SetSize(ParamSize); // Note: ParamSize in chars
end;

procedure TCustomDASQL.AssignParamValue(ParamDesc: TParamDesc; Param: TDAParam);
var
  dt: TDateTime;
  arr: TBytes;
  Value: variant;
  Blob: TBlob;
  OldRollback: boolean;
  ParamObject: TSharedObject;
  ParamDataType: TFieldType;
begin
{$IFNDEF VER9P}
  SetLength(arr, 0); // anti-warning
{$ENDIF}

  ParamDataType := Param.DataType; // cache DataType value

  if Param.FValueCount <= 1 then begin
    if Param.IsSharedObjectDataType(ParamDataType) then begin
      ParamObject := Param.GetNativeParamObject(Param.FParamObject);
      if Param.ParamType in [ptUnknown, ptInput, ptInputOutput] then begin
        if UsedConnection.ConvertEOL and
           ((ParamDataType in (MemoTypes)) or
           (ParamDataType = ftBlob) and NeedConvertEOLForBlob)
        then begin
          Blob := TBlob(ParamObject);
          OldRollback := Blob.RollbackEnabled;
          Blob.RollbackEnabled := False;
          try
            Blob.RemoveCR;
          finally
            Blob.RollbackEnabled := OldRollback;
          end;
        end;
      end
      else
        if ParamDataType in (BlobTypes + MemoTypes) then begin
          TBlob(ParamObject).FreeBlob;
          TBlob(ParamObject).Clear;
        end;
      ParamDesc.SetObject(ParamObject);
      ParamDesc.SetNull(Param.IsNull);
    end
    else begin
      ParamDesc.SetConvertEOL(UsedConnection.ConvertEOL);
      if Param.ParamType in [ptUnknown, ptInput, ptInputOutput] then begin
        ParamDesc.SetValue(Unassigned);
        Value := Param.Value;
        if (ParamDataType = ftDate) and VarIsStr(Value) then begin
          dt := System.Int(StrToDateTime(Value)); // drop time info
          ParamDesc.SetValue(dt);
        end
        // Convert param values if necessary
        else if (ParamDesc.GetDataType in [dtBytes, dtVarBytes]) and VarIsStr(Value) then begin
          arr := Encoding.Default.GetBytes({$IFDEF NEXTGEN}string{$ELSE}AnsiString{$ENDIF}(Value));
          ParamDesc.SetValue(arr);
        end
        else
          ParamDesc.SetValue(Value);
      end;
    end;
  end
  else if Param.IsSharedObjectDataType(ParamDataType) then
    ParamDesc.SetObjectArr(@Param.FDataArr)
  else
    ParamDesc.SetValueArr(@Param.FDataArr);
end;

procedure TCustomDASQL.AssignParamDesc(Param: TDAParam; ParamDesc: TParamDesc);
begin
  Param.Name := ParamDesc.GetName;
  Param.DataType := GetFieldTypeMapClass.GetFieldType(ParamDesc.GetDataType);
  Param.ParamType := TParamType(ParamDesc.GetParamType);
  Param.Size := ParamDesc.GetSize;
end;

procedure TCustomDASQL.AssignParamDescValue(Param: TDAParam; ParamDesc: TParamDesc);
var
  i: integer;
  Blob: TBlob;
  Obj: TSharedObject;
  OldRollback: boolean;
begin
  if ParamDesc.GetArraySize <= 1 then begin
    if Param.IsSharedObject then begin
      if (Param.DataType in MemoTypes) and UsedConnection.ConvertEOL then begin
        Blob := TBlob(Param.ParamObject);
        OldRollback := Blob.RollbackEnabled;
        Blob.RollbackEnabled := False;
        try
          Blob.AddCR;
        finally
          Blob.RollbackEnabled := OldRollback;
        end;
      end;

      if not Param.IsBlob then
        Param.SetIsNull(ParamDesc.GetNull);
    end
    else begin
      ParamDesc.SetConvertEOL(UsedConnection.ConvertEOL);
      Param.Value := ParamDesc.GetValue;
    end;
  end
  else if ParamDesc.NeedAssignObjectValues and Param.IsSharedObject then begin
    for i := 0 to ParamDesc.GetArraySize - 1 do
      if TSharedObject.FromVariant(Param.FDataArr[i]) = nil then begin
        Obj := ParamDesc.ItemObject[i];
        if Obj <> nil then
          Param.Values[i].SetValueObject(Obj);
      end;
  end
  else if ParamDesc.NeedAssignScalarValues and not Param.IsSharedObject then begin
    ParamDesc.SetConvertEOL(UsedConnection.ConvertEOL);
    if (FBatchIters = 1) and (FBatchOffset = 0) then
      Param.Value := ParamDesc.GetValue
    else
      for i := 0 to FBatchIters - 1 do
        Param.FDataArr[FBatchOffset + i] := ParamDesc.ItemValue[FBatchOffset + i];
  end
  else
    Param.ValueCount := ParamDesc.GetArraySize;
end;

function TCustomDASQL.FindResultParam: TDAParam;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Params.Count - 1 do
    if Params[i].ParamType = ptResult then
      Result := Params[i];
end;

procedure TCustomDASQL.DefineProperties(Filer: TFiler);
  function InternalWriteParams: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TCustomDASQL(Filer.Ancestor).FParams)
    else
      Result := FParams.Count > 0;
  end;

  function WriteMacros: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FMacros.IsEqual(TCustomDASQL(Filer.Ancestor).FMacros)
    else
      Result := FMacros.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, InternalWriteParams);
  Filer.DefineProperty('MacroData', ReadMacroData, WriteMacroData, WriteMacros);
  Filer.DefineProperty('CommandStoredProcName', ReadStoredProcName, WriteStoredProcName,
    FStoredProcName <> '');
  Filer.DefineProperty('StoredProcIsQuery', ReadStoredProcIsQuery, WriteStoredProcIsQuery,
    FStoredProcIsQuery);
end;

procedure TCustomDASQL.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TCustomDASQL.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(FParams);
end;

procedure TCustomDASQL.ReadMacroData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FMacros);
end;

procedure TCustomDASQL.WriteMacroData(Writer: TWriter);
begin
  Writer.WriteCollection(FMacros);
end;

procedure TCustomDASQL.ReadStoredProcName(Reader: TReader);
begin
  SetStoredProcName(Reader.ReadString);
end;

procedure TCustomDASQL.WriteStoredProcName(Writer: TWriter);
begin
  Writer.WriteString(FStoredProcName);
end;

procedure TCustomDASQL.SetStoredProcName(const StoredProcName: string);
begin
  FStoredProcName := StoredProcName;
  if FICommand <> nil then
    FICommand.SetProp(prIsStoredProc, StoredProcName <> '');
  FIsSPInit := False;
end;

procedure TCustomDASQL.ReadStoredProcIsQuery(Reader: TReader);
begin
  FStoredProcIsQuery := Reader.ReadBoolean;
end;

procedure TCustomDASQL.WriteStoredProcIsQuery(Writer: TWriter);
begin
  Writer.WriteBoolean(FStoredProcIsQuery);
end;

function TCustomDASQL.FindParam(const Value: string): TDAParam;
begin
  Result := FParams.FindParam(Value);
end;

function TCustomDASQL.ParamByName(const Value: string): TDAParam;
begin
  Result := FParams.ParamByName(Value);
end;

function TCustomDASQL.FindMacro(const Value: string): TMacro;
begin
  Result := FMacros.FindMacro(Value);
end;

function TCustomDASQL.MacroByName(const Value: string): TMacro;
begin
  Result := FMacros.MacroByName(Value);
end;

procedure TCustomDASQL.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDASQL then begin
    TCustomDASQL(Dest).Connection := Connection;
    TCustomDASQL(Dest).ParamCheck := ParamCheck;  // before SQL
    TCustomDASQL(Dest).SQL.Text := SQL.Text;
    TCustomDASQL(Dest).Macros.Assign(Macros);
    TCustomDASQL(Dest).Params.Assign(Params);
    TCustomDASQL(Dest).Debug := Debug;
    TCustomDASQL(Dest).AutoCommit := AutoCommit;
    TCustomDASQL(Dest).FStoredProcName := FStoredProcName;
    TCustomDASQL(Dest).FStoredProcIsQuery := FStoredProcIsQuery;
  end
  else
    inherited;
end;

procedure TCustomDASQL.SetConnection(Value: TCustomDAConnection);
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if (Value <> FConnection) or (Value <> vConnection) then begin
    if vConnection <> nil then begin
      Disconnect;
      vConnection.UnRegisterClient(Self);
    end;

    FConnection := Value;

    if FConnection <> nil then begin
      Value.RegisterClient(Self, ConnectChange);

      if FICommand <> nil then begin
        if FConnection.FIConnection <> nil then
          CheckICommand;
        FICommand.SetConnection(FConnection.FIConnection);
      end;
    end
    else
      if FICommand <> nil then
        FICommand.SetConnection(nil);
  end;
end;

procedure TCustomDASQL.SetTransaction(Value: TDATransaction);
begin
  if Value <> FTransaction then begin
    if FTransaction <> nil then
      FTransaction.RemoveFreeNotification(Self);
    FTransaction := Value;
    if FTransaction <> nil then
      FTransaction.FreeNotification(Self);
  end;
end;

function TCustomDASQL.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TCustomDASQL.SetSQL(Value: TStrings);
begin
 if FSQL.Text <> Value.Text then begin
    FSQL.BeginUpdate;
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;

procedure TCustomDASQL.SQLChanged(Sender: TObject);
begin
  ProcessSQLChanged(FLockMacros, BaseSQLOldBehavior);
end;

procedure TCustomDASQL.ProcessSQLChanged(LockMacros, SaveBaseSQL: boolean);
var
  Cmd: TCRCommand;
begin
  //if not (csReading in ComponentState) then begin
  if FDataSet = nil then
    UnPrepare
  else begin
    if not SaveBaseSQL then
      FDataSet.FBaseSQL := '';
    FDataSet.Close;
    FDataSet.UnPrepare;
    FDataSet.FieldDefs.Updated := False;

    if not FLockAssembleSQL and (FDataSet.Data is TCRRecordSet) then begin
      Cmd := TCRRecordSet(FDataSet.Data).GetCommand;
      if Cmd <> nil then
        Cmd.SetCursorState(csInactive);
      Cmd.CommandType := ctUnknown;
    end;

  {$IFDEF WITH_IPROVIDER}
    FDataSet.FOldTableName := '';
    FDataSet.FOldKeyFields := '';
  {$ENDIF}
  end;

  if not LockMacros then
    ScanMacros;

  if not FLockAssembleSQL then begin
    FSQLModified := True;
    AssembleSQL;
    SetStoredProcName('');
  end;
end;

function TCustomDASQL.GetPrepared: boolean;
begin
  if FICommand <> nil then
    Result := FICommand.GetPrepared
  else
    Result := False;
end;

procedure TCustomDASQL.SetPrepared(Value: boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

procedure TCustomDASQL.SetParams(Value: TDAParams);
begin
  FParams.AssignValues(Value);
end;

function TCustomDASQL.GetParamCount: word;
begin
  Result := FParams.Count;
end;

procedure TCustomDASQL.SetParamCheck(Value: boolean);
begin
  FParamCheck := Value;

  Value := Value or (csDesigning in ComponentState); // set value of ScanParams
  if FICommand <> nil then
    FICommand.SetProp(prScanParams, Value);

  if Value then
    AssembleSQL;
end;

function TCustomDASQL.GetParamValues(const ParamName: string): variant;
begin
  Result := FParams.ParamValues[ParamName];
end;

procedure TCustomDASQL.SetParamValues(const ParamName: string; const Value: variant);
begin
  FParams.ParamValues[ParamName] := Value;
end;

procedure TCustomDASQL.SetMacros(Value: TMacros);
begin
  FMacros.Assign(Value);
end;

function TCustomDASQL.GetMacroCount: word;
begin
  Result := FMacros.Count;
end;

procedure TCustomDASQL.SetAutoCommit(Value: boolean);
begin
  FAutoCommit := Value;
  if FICommand <> nil then
    FICommand.SetProp(prAutoCommit, FAutoCommit);
end;

function TCustomDASQL.GetNonBlocking: boolean;
begin
  Result := FNonBlocking;
end;

procedure TCustomDASQL.SetNonBlocking(Value: boolean);
begin
  if FNonBlocking <> Value then begin
    FNonBlocking := Value;
    if FICommand <> nil then
      FICommand.SetProp(prNonBlocking, Value);
  end;
end;

function TCustomDASQL.GetRowsAffected: integer;
var
  Value: variant;
begin
  if FICommand <> nil then begin
    FICommand.GetProp(prRowsProcessed, Value);
    Result := Value;
  end
  else
    Result := 0;
end;

function TCustomDASQL.GetParamsProcessed: integer;
var
  Value: variant;
begin
  if FICommand <> nil then begin
    FICommand.GetProp(prParamsProcessed, Value);
    Result := Value;
  end
  else
    Result := 0;
end;

function TCustomDASQL.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

{ TDAMetaData }

constructor TDAMetaData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FreeIRecordSet;
  SetIRecordSet(nil);

  FRestrictions := TStringList.Create;
  TStringList(FRestrictions).OnChange := RestrictionsChanged;

  FDesignCreate := csDesigning in ComponentState;
end;

destructor TDAMetaData.Destroy;
begin
  Close;
  FIMetaData.Free;
  FRestrictions.Free;
  SetIRecordSet(nil);
  if FConnection <> nil then
    FConnection.UnRegisterClient(Self);

  inherited Destroy;
end;

procedure TDAMetaData.GetMetaDataKinds(List: TStrings);
begin
  CheckIMetaData;

  FIMetaData.GetMetaDataKindsList(List);
end;

procedure TDAMetaData.GetRestrictions(List: TStrings; const MetaDataKind: string);
begin
  CheckIMetaData;

  FIMetaData.GetRestrictionsList(List, MetaDataKind);
end;

procedure TDAMetaData.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TDAMetaData.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) then
    if (AComponent = FConnection) then
      Connection := nil
    else
    if (AComponent = FTransaction) then
      Transaction := nil;
end;

function TDAMetaData.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection;
end;

function TDAMetaData.UsedTransaction: TDATransaction;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then begin
    if vConnection.IsMultipleTransactionsSupported then
      Result := Transaction
    else
      Result := nil;

    if Result = nil then
      Result := vConnection.UsedTransaction;
  end
  else
    Result := nil;
end;

function TDAMetaData.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

procedure TDAMetaData.BeginConnection;
var
  vConnection: TCustomDAConnection;
  vTransaction: TDATransaction;
begin
  vConnection := UsedConnection;
  if vConnection = nil then
    DatabaseError(SConnectionNotDefined);

  vConnection.InternalConnect;

  if vConnection.IsMultipleTransactionsSupported then begin
    vTransaction := UsedTransaction;
    Assert(vTransaction <> nil);
    vTransaction.GainTransaction;
  end;
end;

procedure TDAMetaData.EndConnection;
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;

  vConnection.InternalDisconnect;
  if vConnection.IsMultipleTransactionsSupported then
    UsedTransaction.ReleaseTransaction;
end;

procedure TDAMetaData.CheckIMetaData;
var
  ClassType: TCRMetaDataClass;
begin
  Assert(UsedConnection <> nil);
  ClassType := UsedConnection.GetIMetaDataClass;

  if not (FIMetaData is ClassType) then begin
    FIMetaData.Free;
    FIMetaData := ClassType.Create;
  end;
end;

procedure TDAMetaData.OpenCursor(InfoQuery: boolean);
var
  ReOpen: boolean;
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if vConnection <> nil then
    vConnection.PushOperation(clOpen, vConnection.IsFailOverAllowed);
  try
    BeginConnection;
    //StartWait;
    try
      repeat
        ReOpen := False;
        try
          inherited;
        except
            on EFailOver do begin //TODO: Add check for clOpen operation
              vConnection.RestoreAfterFailOver;
              Reopen := True;
            end
            else
              raise;
        end;
      until (not ReOpen);
    finally
      EndConnection;
    end;
  finally
    if vConnection <> nil then
      vConnection.PopOperation;
  end;
end;

procedure TDAMetaData.InternalOpen;
var
  AData: TData;
begin
  CheckIMetaData;
  AData := FIMetaData.GetMetaData(UsedConnection.FIConnection, UsedTransaction.FITransaction,
    FMetaDataKind, FRestrictions);
  SetIRecordSet(AData);

  inherited InternalOpen;
end;

procedure TDAMetaData.CloseCursor;
begin
  inherited;

  FieldDefs.Updated := False;
  if Data <> nil then
    Data.FilterText := '';
  SetIRecordSet(nil);
end;

procedure TDAMetaData.SetConnection(Value: TCustomDAConnection);
var
  vConnection: TCustomDAConnection;
begin
  vConnection := UsedConnection;
  if (Value <> FConnection) or (Value <> vConnection) then begin

    if vConnection <> nil then begin
      if not (csReading in ComponentState) then
        Close;
      vConnection.UnRegisterClient(Self);
    end;

    FConnection := Value;

    if FConnection <> nil then
      Value.RegisterClient(Self, ConnectChange);
  end;
end;

procedure TDAMetaData.ConnectChange(Sender: TObject; Connecting: boolean);
begin
  if not Connecting then begin
    if not TCustomDAConnection(Sender).Options.DisconnectedMode then
      Close
    else
      if Data is TCRRecordSet then
        TCRRecordSet(Data).Disconnect;
  end;
end;

function TDAMetaData.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TDAMetaData.SetTransaction(Value: TDATransaction);
begin
  if FTransaction <> Value then begin
    if FTransaction <> nil then
      FTransaction.RemoveFreeNotification(Self);
    FTransaction := Value;
    if FTransaction <> nil then
      FTransaction.FreeNotification(Self);
  end;
end;

procedure TDAMetaData.SetMetaDataKind(const Value: string);
begin
  if Value <> FMetaDataKind then begin
    if not (csReading in ComponentState) then
      Close;
    FMetaDataKind := Value;
  end;
end;

procedure TDAMetaData.SetRestrictions(Value: TStrings);
begin
  FRestrictions.Assign(Value);
end;

procedure TDAMetaData.RestrictionsChanged(Sender: TObject);
begin
  if not (csReading in ComponentState) then
    Close;
end;

{ TCustomDAUpdateSQL }

constructor TCustomDAUpdateSQL.Create(Owner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited;

  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKindToStatementType(UpdateKind)] := TStringList.Create;
  FSQLText[stRefresh] := TStringList.Create;
  FSQLText[stLock] := TStringList.Create;
end;

destructor TCustomDAUpdateSQL.Destroy;
var
  StatementType: TStatementType;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for StatementType := Low(TStatementType) to High(TStatementType) do
    FSQLText[StatementType].Free;

  inherited;
end;

procedure TCustomDAUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
var
  StatementType: TStatementType;
begin
  StatementType := UpdateKindToStatementType(UpdateKind);
  if FDataSet = nil then
     raise Exception.Create(SDataSetNotDefined);

  FDataSet.CheckDataSetService;
  FDataSet.FDataSetService.FUpdater.PerformSQL(FSQLText[StatementType].Text, [StatementType]);
end;

function TCustomDAUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := GetSQLIndex(Ord(UpdateKindToStatementType(UpdateKind)));
end;

procedure TCustomDAUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  SetSQLIndex(Ord(UpdateKindToStatementType(UpdateKind)), Value);
end;

function TCustomDAUpdateSQL.GetSQLIndex(Index: integer): TStrings;
begin
  Result := FSQLText[TStatementType(Index)];
end;

procedure TCustomDAUpdateSQL.SetSQLIndex(Index: integer; Value: TStrings);
begin
  FSQLText[TStatementType(Index)].Assign(Value);
end;

function TCustomDAUpdateSQL.GetDataSet: TCustomDADataSet;
begin
  Result := FDataSet;
end;

procedure TCustomDAUpdateSQL.SetDataSet(DataSet: TCustomDADataSet);
begin
  FDataSet := DataSet;
end;

procedure TCustomDAUpdateSQL.SetObjectIndex(Index: integer; Value: TComponent);
begin
  CheckUpdateComponent(Value);
  FUpdateObject[TStatementType(Index)] := Value;
end;

function TCustomDAUpdateSQL.GetObjectIndex(Index: integer): TComponent;
begin
  Result := FUpdateObject[TStatementType(Index)];
end;

function TCustomDAUpdateSQL.DataSetClass: TCustomDADataSetClass;
begin
  Result := TCustomDADataSet;
end;

function TCustomDAUpdateSQL.SQLClass: TCustomDASQLClass;
begin
  Result := TCustomDASQL;
end;

procedure TCustomDAUpdateSQL.CheckUpdateComponent(Component: TComponent; NewDataset: TCustomDADataset);
begin
  if Component <> nil then begin
    if not ((Component is SQLClass) or (Component is DataSetClass)) then
      raise Exception.Create(Format(SUpdateComponentInvalidType, [DataSetClass.ClassName, SQLClass.ClassName]));
    if NewDataSet = Component then
      raise Exception.Create(SUpdateComponentCircularReferences);
  end;
end;

procedure TCustomDAUpdateSQL.CheckUpdateComponent(Component: TComponent);
begin
  CheckUpdateComponent(Component, FDataset);
end;

procedure TCustomDAUpdateSQL.Notification(AComponent: TComponent; Operation: TOperation);
var
  stIdx: TStatementType;
begin
  inherited;
  if Operation = opRemove then
    for stIdx := Low(FUpdateObject) to High(FUpdateObject) do
      if FUpdateObject[stIdx] = AComponent then
        FUpdateObject[stIdx] := nil;
end;

procedure TCustomDAUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  ExecSQL(UpdateKind);
end;

procedure TCustomDAUpdateSQL.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TCustomDAUpdateSQL.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDAUpdateSQL then begin
    TCustomDAUpdateSQL(Dest).RefreshSQL := RefreshSQL;
    TCustomDAUpdateSQL(Dest).ModifySQL := ModifySQL;
    TCustomDAUpdateSQL(Dest).InsertSQL := InsertSQL;
    TCustomDAUpdateSQL(Dest).DeleteSQL := DeleteSQL;
    TCustomDAUpdateSQL(Dest).DataSet := DataSet;
    TCustomDAUpdateSQL(Dest).RefreshObject := RefreshObject;
    TCustomDAUpdateSQL(Dest).ModifyObject := ModifyObject;
    TCustomDAUpdateSQL(Dest).InsertObject := InsertObject;
    TCustomDAUpdateSQL(Dest).DeleteObject := DeleteObject;
  end
  else
    inherited;
end;

{ TMacro }

constructor TMacro.Create(Collection: TCollection);
begin
  inherited;

  FActive := True;
end;

procedure TMacro.AssignTo(Dest: TPersistent);
begin
  if Dest is TMacro then begin
    TMacro(Dest).Name := Name;
    TMacro(Dest).Value := Value;
    TMacro(Dest).Active := Active;
  end
  else
    inherited;
end;

function TMacro.IsEqual(Value: TMacro): boolean;
begin
  Result := (Name = Value.Name) and
    (Self.Value = Value.Value) and
    (Active = Value.Active);
end;

function TMacro.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

procedure TMacro.SetValue(const Value: string);
begin
  if Value <> FValue then begin
    FValue := Value;
    TMacros(Collection).NotifyOwner(Self);
  end;
end;

function TMacro.GetAsDateTime: TDateTime;
var
  St: string;
  iStart: integer;
  iEnd: integer;
  Len: integer;
begin
  St := Trim(FValue);
  Len := Length(FValue);
  if (Len > 0) and (St[1] = '''') then
    iStart := 2
  else
    iStart := 1;

  if (Len > 0) and (St[Length(St)] = '''') then
    iEnd := Length(St) - 1
  else
    iEnd := Length(St);

  Result := StrToDateTime(Copy(St, iStart, iEnd - iStart + 1));
end;

procedure TMacro.SetAsDateTime(Value: TDateTime);
begin
  Self.Value := '''' + DateTimeToStr(Value) + '''';
end;

function TMacro.GetAsFloat: double;
begin
  Result := StrToFloat(FValue);
end;

procedure TMacro.SetAsFloat(Value: double);
begin
  Self.Value := FloatToStr(Value);
end;

function TMacro.GetAsInteger: integer;
begin
  Result := StrToInt(FValue);
end;

procedure TMacro.SetAsInteger(Value: integer);
begin
  Self.Value := IntToStr(Value);
end;

function TMacro.GetAsString: string;
var
  St: string;
  iStart: integer;
  iEnd: integer;
  Len: integer;
begin
  St := Trim(FValue);
  Len := Length(FValue);
  if (Len > 0) and (St[1] = '''') then
    iStart := 2
  else
    iStart := 1;

  if (Len > 0) and (St[Length(St)] = '''') then
    iEnd := Length(St) - 1
  else
    iEnd := Length(St);

  Result := Copy(St, iStart, iEnd - iStart + 1);
end;

procedure TMacro.SetAsString(const Value: string);
begin
  Self.Value := '''' + Value + '''';
end;

procedure TMacro.SetActive(Value: boolean);
begin
  if Value <> FActive then begin
    FActive := Value;
    TMacros(Collection).NotifyOwner(Self);
  end;
end;

procedure TMacro.Clear;
begin
  Self.Value := '';
end;

{ TMacros }

constructor TMacros.Create(Owner: TPersistent);
begin
  inherited Create(TMacro);

  FOwner := Owner;
  FParserClass := TSQLParser;
end;

procedure TMacros.Scan(const SQL: string);
var
  Macro: TMacro;
  NewMacros: TMacros;
  Parser: TSQLParser;
  CodeLexem: integer;
  St, St2: string;
  MacroSt: string; // Delphi problem with compare MacroChar = St
  Changed, NeedNext: boolean;
  i: integer;

begin
  // performance reason
  if Pos(string(MacroChar), SQL) = 0 then begin
    Clear;
    Exit;
  end;

  NewMacros := TMacros.Create(nil);
  NewMacros.BeginUpdate;

  Parser := FParserClass.Create(SQL);
  MacroSt := MacroChar;
  Parser.OmitBlank := False;
  Parser.Uppered := False;
  try
    Parser.ToBegin;
    repeat
      repeat
        CodeLexem := Parser.GetNext(St);
      until (CodeLexem = lcEnd) or (St = MacroSt);
      repeat
        NeedNext := True;
        if (St = MacroSt) and Parser.IsMacroAllowed(CodeLexem) then begin
          CodeLexem := Parser.GetNext(St);
          if (CodeLexem = lcIdent) or
            Parser.IsNumericMacroNameAllowed and (CodeLexem = lcNumber) or
            (CodeLexem >= lxSQLFirst) // SQL reserved words are allowed
          then begin
            St2 := St;
            if CodeLexem = lcNumber then begin
              CodeLexem := Parser.GetNext(St);
              if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) then
                St2 := St2 + St
              else
                NeedNext := False
            end;
            Macro := NewMacros.FindMacro(St2);
            if Macro = nil then begin
              Macro := TMacro(NewMacros.Add);
              if FindMacro(St2) <> nil then
                Macro.Assign(FindMacro(St2))
              else
                Macro.Name := St2;
            end;
          end;
        end;
      until NeedNext;
    until CodeLexem = lcEnd;

    if Count <> NewMacros.Count then
      Changed := True
    else
    begin
      Changed := False;
      for i := 0 to Count - 1 do
        if not Items[i].IsEqual(NewMacros.Items[i]) then begin
          Changed := True;
          Break;
        end;
    end;

    if Changed then
      Assign(NewMacros);
  finally
    Parser.Free;
    NewMacros.Free;
  end;
end;

function TMacros.GetMacroValue(Macro: TMacro): string;
begin
  if Macro.Active then
    Result := Macro.Value
  else
    Result := '';
end;

procedure TMacros.Expand(var SQL: string);
var
  Parser: TSQLParser;
  CodeLexem: integer;
  Macro: TMacro;
  St, St2: string;
  MacroSt: string; // Delphi problem with compare MacroChar = St
  Result: string;
  NeedNext: boolean;
begin
  // performance reason
  if Pos(string(MacroChar), SQL) = 0 then
    Exit;

  Parser := FParserClass.Create(SQL);
  MacroSt := MacroChar;
  Parser.OmitBlank := False;
  Parser.Uppered := False;
  Parser.QuotedString := True;
  try
    Result := '';
    St := '';
    NeedNext := True;
    CodeLexem := 0; // to prevent warning
    Parser.ToBegin;
    while True do begin
      if NeedNext then
        CodeLexem := Parser.GetNext(St);

      if CodeLexem = lcEnd then
        Break;

      NeedNext := True;
      if (St = MacroSt) and Parser.IsMacroAllowed(CodeLexem) then begin
        CodeLexem := Parser.GetNext(St);
        if (CodeLexem = lcIdent) or
           (CodeLexem = lcNumber) and Parser.IsNumericMacroNameAllowed or
           (CodeLexem >= lxSQLFirst) // SQL reserved words is allowed
        then begin
          St2 := St;
          if CodeLexem = lcNumber then begin
            CodeLexem := Parser.GetNext(St);
            if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) then
              St2 := St2 + St
            else
              NeedNext := False
          end;
          Macro := FindMacro(St2);
          if Macro <> nil then
            Result := Result + GetMacroValue(Macro);
        end
        else
          Result := Result + MacroSt + St;
      end
      else
        Result := Result + St;
    end;
  finally
    Parser.Free;
  end;

  SQL := Result;
end;

procedure TMacros.AssignTo(Dest: TPersistent);
begin
  if Dest is TMacros then
    TMacros(Dest).Assign(Self)
  else
    inherited AssignTo(Dest);
end;

procedure TMacros.AssignValues(Value: TMacros);
var
  i: integer;
  Macro: TMacro;
begin
  for i := 0 to Value.Count - 1 do begin
    Macro := FindMacro(Value[i].Name);
    if Macro <> nil then
      Macro.Assign(Value[i]);
  end;
end;

procedure TMacros.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadBinaryData, nil, False);
end;

function TMacros.IsEqual(Value: TMacros): boolean;
var
  i: integer;
begin
  Result := True;
  if Self = Value then
    Exit;
  if Count = Value.Count then begin
    for i := 0 to Count - 1 do
      if (Items[i].Name <> Value[i].Name) or
        (Items[i].Value <> Value[i].Value) or
        (Items[i].Active <> Value[i].Active)
      then
        Result := False;
  end
  else
    Result := False;
end;

{  Structure of Data
  Version        1 (100) -- !!! Add in 100
  ItemCount      1
    NameLength   1
    Name         Length(Name)
    ValueLength  2
    Value        Length(Value)
    Active       1       -- !!! Add in 100
}

procedure TMacros.ReadBinaryData(Stream: TStream);
const
  BufLen = 1000;
var
  i, Len: word;
  Version: byte;
  B: boolean;
  Buf: TBytes;
  St: string;
begin
  SetLength(Buf, BufLen + 1{??? - array [0..BufLen] of byte});

  with Stream do begin
    ReadBuffer(Version, 1);  // Version or Count

    if Version = 100 then begin
      Len := 0;
      ReadBuffer(Len, 1);
    end;

    for i := 0 to Count - 1 do begin
      Len := 0;
      ReadBuffer(Len, 1);
      if Len > BufLen then
        Len := BufLen;
      ReadBuffer(Buf[0], Len);
      Buf[Len] := 0;
      St := Encoding.Default.GetString(Buf, 0, Len);
      with MacroByName(St) do begin
        ReadBuffer(Len, 2);
        if Len > BufLen then
          Len := BufLen;
        ReadBuffer(Buf[0], Len);
        Buf[Len] := 0;
        St := Encoding.Default.GetString(Buf, 0, Len);
        Value := St;

        if Version = 100 then begin
          ReadBuffer(B, 1);  // Active
          Active := B;
        end;
      end;
    end;
  end;
end;

function TMacros.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMacros.Update(Item: TCollectionItem);
begin
  inherited;

  NotifyOwner(TMacro(Item));
end;

function TMacros.GetItem(Index: integer): TMacro;
begin
  Result := TMacro(inherited Items[Index]);
end;

procedure TMacros.SetItem(Index: integer; Value: TMacro);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

procedure TMacros.NotifyOwner(Item: TMacro);
begin
  if (UpdateCount <> 0) or (FOwner = nil) then
    exit;

  if FOwner is TCustomDADataSet then
    with TCustomDADataSet(FOwner) do begin
      if not Active or (Item = nil) or (Pos(MacroChar + Item.Name, SQL.Text) <> 0) then
        FCommand.ProcessSQLChanged(True, True);
    end
  else begin
    TCustomDASQL(FOwner).ProcessSQLChanged(True, True);
  end;
end;

function TMacros.FindMacro(const Value: string): TMacro;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := TMacro(inherited Items[i]);
    if AnsiUpperCase(Result.Name) = AnsiUpperCase(Value) then
      Exit;
  end;
  Result := nil;
end;

function TMacros.MacroByName(const Value: string): TMacro;
begin
  Result := FindMacro(Value);

  if Result = nil then
    DatabaseErrorFmt(SMacroNotFound, [Value], FOwner as TComponent);
end;

procedure TMacros.SetParserClass(Value: TSQLParserClass);
begin
  FParserClass := Value;
end;

{ TConnectDialogOption }

constructor TConnectDialogOption.Create(Owner: TCustomConnectDialog; OptionKind: TConnectDialogOptionKind; Order: Integer; Visible: Boolean = True);
begin
  inherited Create;

  FOwner := Owner;
  FKind := OptionKind;
  FOrder := Order;
  FCaption := '';
  FVisible := Visible;
end;

procedure TConnectDialogOption.SetCaption(Value: string);
begin
  if FCaption <> Value then begin
    FCaption := Value;
    UniqueString(FCaption);
    FOwner.OptionChanged;
  end;
end;

procedure TConnectDialogOption.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
    FVisible := Value;
end;

procedure TConnectDialogOption.SetOrder(Value: Integer);
begin
  if (FOrder <> Value) and (Value >= 0) then
    FOrder := Value;
end;

procedure TConnectDialogOption.AssignTo(Dest: TPersistent);
begin
  if Dest is TConnectDialogOption then begin
    TConnectDialogOption(Dest).FCaption := FCaption;
    TConnectDialogOption(Dest).FVisible := FVisible;
  end
  else
    inherited;
end;

{ TCustomConnectDialog }

constructor TCustomConnectDialog.Create(Owner: TComponent);
begin
  inherited;

  InSetLabelSet := False;

  FServerOption := TConnectDialogOption.Create(Self, okServer, 0);
  FUserNameOption := TConnectDialogOption.Create(Self, okUserName, 1);
  FPasswordOption := TConnectDialogOption.Create(Self, okPassword, 2);

  FRetries := 3;
  FStoreLogInfo := True;
{$IFDEF MSWINDOWS}
  FUseServerHistory := True;
{$ENDIF}
  FNeedConnect := True;
end;

destructor TCustomConnectDialog.Destroy;
begin
  FreeServerEnumerator;

  FServerOption.Free;
  FUserNameOption.Free;
  FPasswordOption.Free;

  inherited;
end;

procedure TCustomConnectDialog.ReadServerCaptionProperty(Reader: TReader);
begin
  FServerOption.Caption := Reader.ReadString;
end;

procedure TCustomConnectDialog.ReadUserNameCaptionProperty(Reader: TReader);
begin
  FUserNameOption.Caption := Reader.ReadString;
end;

procedure TCustomConnectDialog.ReadPasswordCaptionProperty(Reader: TReader);
begin
  FPasswordOption.Caption := Reader.ReadString;
end;

procedure TCustomConnectDialog.ReadSavePasswordProperty(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TCustomConnectDialog.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ServerLabel', ReadServerCaptionProperty, nil, False);
  Filer.DefineProperty('UsernameLabel', ReadUserNameCaptionProperty, nil, False);
  Filer.DefineProperty('PasswordLabel', ReadPasswordCaptionProperty, nil, False);
  Filer.DefineProperty('SavePassword', ReadSavePasswordProperty, nil, False);
end;

procedure TCustomConnectDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (FConnection = AComponent) then
    FConnection := nil;
end;

function TCustomConnectDialog.DefDialogClass: TClass;
begin
  Result := nil;
end;

procedure TCustomConnectDialog.GetServerList(List: TStrings);
begin
{$IFDEF MSWINDOWS}
  if FUseServerHistory then
    LoadServerListFromRegistry(List)
  else
{$ENDIF}
  begin
    CheckServerEnumerator;
    SetServerEnumerator(FServerEnumerator); // set props
    FServerEnumerator.GetServerList(List);
  {$IFDEF MSWINDOWS}
    if List.Count = 0 then
      LoadServerListFromRegistry(List);
  {$ENDIF}
  end;
end;

procedure TCustomConnectDialog.GetOptions(var Options: TConnectDialogOptionArray; Ordered: Boolean = True);
  procedure QuickSort(Options: TConnectDialogOptionArray; iLo, iHi: Integer);
   var
     Lo, Hi, Pivot: Integer;
     TempOption: TConnectDialogOption;
   begin
     Lo := iLo;
     Hi := iHi;
     Pivot := Options[(Lo + Hi) shr 1].Order;
     repeat
       while Options[Lo].Order < Pivot do Inc(Lo);
       while Options[Hi].Order > Pivot do Dec(Hi);
       if Lo <= Hi then
       begin
         TempOption := Options[Lo];
         Options[Lo] := Options[Hi];
         Options[Hi] := TempOption;
         Inc(Lo) ;
         Dec(Hi) ;
       end;
     until Lo > Hi;
     if Hi > iLo then QuickSort(Options, iLo, Hi) ;
     if Lo < iHi then QuickSort(Options, Lo, iHi) ;
   end;
begin
  if Ordered then
    QuickSort(Options, Low(Options), High(Options));
end;

{$IFDEF MSWINDOWS}
function TCustomConnectDialog.GetKeyPath: string;
begin
  Result := '';
end;

function TCustomConnectDialog.GetApplicationKeyPath: string;
begin
  Result := GetKeyPath + 'Connect\' + ApplicationTitle;
end;

function TCustomConnectDialog.GetServerListKeyPath: string;
begin
  Result := GetKeyPath + 'Connect';
end;

function TCustomConnectDialog.GetServerStoreName: string;
begin
  Result := 'Server';
end;

procedure TCustomConnectDialog.LoadServerListFromRegistry(List: TStrings);
var
  ListKey, ServerKey: string;
  Registry: TRegistry;
  i: integer;
  KeyOpened: boolean;
begin
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    ListKey := GetServerListKeyPath;
    KeyOpened := Registry.OpenKey(ListKey, False);
    if not KeyOpened then begin
      ListKey := StringReplace(ListKey, 'Devart', 'CoreLab', [rfIgnoreCase]);
      KeyOpened := Registry.OpenKey(ListKey, False);
    end;
    if KeyOpened then begin
      List.Clear;
      ServerKey := GetServerStoreName;
      i := 1;
      while Registry.ValueExists(Format('%s %d', [ServerKey, i])) do begin
        List.Add(Registry.ReadString(Format('%s %d', [ServerKey, i])));
        Inc(i);
      end;
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TCustomConnectDialog.SaveServerListToRegistry;
var
  Registry: TRegistry;
  List: TStrings;
  i,j: integer;
  ServerKey: string;
begin
  if Connection.Server = '' then
    exit; // nothing to save

  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    List := TStringList.Create;
    try
      LoadServerListFromRegistry(List); // call before creating Devart key
      if Registry.OpenKey(GetServerListKeyPath, True) then begin
        ServerKey := GetServerStoreName;
        Registry.WriteString(Format('%s %d', [ServerKey, 1]), Connection.Server);
        i := 2;
        for j := 0 to List.Count - 1 do
          if not SameText(List[j], Connection.Server) then begin
            Registry.WriteString(Format('%s %d', [ServerKey, i]), List[j]);
            Inc(i);
          end;
        Registry.CloseKey;
      end;
    finally
      List.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TCustomConnectDialog.SaveInfoToRegistry(Registry: TRegistry);
begin
  // UserName
  if UserName.Visible then
    Registry.WriteString('Username', Connection.Username)
  else
    Registry.DeleteValue('Username');

  // Server
  if Server.Visible then
    Registry.WriteString(GetServerStoreName, Connection.Server)
  else
    Registry.DeleteValue(GetServerStoreName);

  // Password
  if Password.Visible and SavePassword then
    Registry.WriteString('Password', Connection.EncryptToHex(Connection.Password))
  else
    Registry.DeleteValue('Password');
end;

procedure TCustomConnectDialog.LoadInfoFromRegistry(Registry: TRegistry);
begin
  // Server
  if Server.Visible and Registry.ValueExists(GetServerStoreName) then
    Connection.FServer := Registry.ReadString(GetServerStoreName);

  // Username
  if UserName.Visible and Registry.ValueExists('Username') then
    Connection.FUsername := Registry.ReadString('Username');

  // Password
  if Password.Visible and Registry.ValueExists('Password') and SavePassword then
    Connection.FPassword := Connection.DecryptFromHex(Registry.ReadString('Password'));
end;
{$ENDIF}

{class function TCustomConnectDialog.AcceptBlankPassword: boolean;
begin
  Result := False;
end;}

function TCustomConnectDialog.Execute: boolean;
{$IFNDEF ANDROID}
var
  OldUsername, OldPassword, OldServer: string;
  IDE: boolean;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Key, CrKey: string;
  KeyOpened: boolean;
  Registry: TRegistry;
{$ENDIF}
begin
{$IFNDEF ANDROID}
  Result := False;

  if Connection = nil then
    DatabaseError(SConnectionNotDefined);

  OldUsername := Connection.Username;
  OldPassword := Connection.Password;
  OldServer := Connection.Server;

{$IFDEF MSWINDOWS}
  Key := GetApplicationKeyPath;
  Registry := nil;
{$ENDIF}
  try
    IDE := (Pos('Delphi', ApplicationTitle) = 1) or
      (Pos('C++Builder', ApplicationTitle) = 1) or
      (Pos('RAD Studio', ApplicationTitle) > 0) or
      (Pos('Lazarus', ApplicationTitle) > 0);
    if FStoreLogInfo then begin
    {$IFDEF MSWINDOWS}
      Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);

      KeyOpened := Registry.OpenKey(Key, False);
      if not KeyOpened then begin
        CrKey := StringReplace(Key, 'Devart', 'CoreLab', [rfIgnoreCase]);
        KeyOpened := Registry.OpenKey(CrKey, False);
      end;

      if KeyOpened and (not IDE or (Connection.Username = '')) then
        LoadInfoFromRegistry(Registry);

      if not IDE then
        if not SavePassword then
          Connection.FPassword := '';

      if KeyOpened then
        Registry.CloseKey;
    {$ENDIF}
    end;

    if IDE and (LowerCase(Copy(ClassName, Length(ClassName) - 2, 3)) = 'fmx') and Assigned(ShowConnectFormProcFmx) then
      Result := ShowConnectFormProcFmx(Self)
    else
    if Assigned(ShowConnectFormProc) then
      Result := ShowConnectFormProc(Self)
    else
      Result := False;

    if Result then begin
      if FStoreLogInfo then begin
      {$IFDEF MSWINDOWS}
        SaveServerListToRegistry;

        // StoreLogInfo can be changed by user since previous check
        if Registry = nil then
          Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);

        if Registry.OpenKey(Key, True) then begin
          SaveInfoToRegistry(Registry);
          Registry.CloseKey;
        end;
      {$ENDIF}
      end;
    end;
  finally
    if not Result then begin
      Connection.FUsername := OldUsername;
      Connection.FPassword := OldPassword;
      Connection.FServer := OldServer;
    end;
  {$IFDEF MSWINDOWS}
    Registry.Free;
  {$ENDIF}
  end;
{$ELSE}
  raise Exception.Create('Connect dialog is not supported on mobile platforms.'#13#10'You should set the LoginPrompt property of the Connection component to False.');
{$ENDIF}
end;

function TCustomConnectDialog.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRServerEnumerator;
end;

procedure TCustomConnectDialog.SetServerEnumerator(Value: TCRServerEnumerator);
begin
  if FServerEnumerator <> Value then begin
    if FServerEnumerator <> nil then
      FreeServerEnumerator;

    FServerEnumerator := Value;
  end;
end;

procedure TCustomConnectDialog.CreateServerEnumerator;
begin
  SetServerEnumerator(GetServerEnumeratorClass.Create);
end;

procedure TCustomConnectDialog.FreeServerEnumerator;
begin
  FServerEnumerator.Free;
  FServerEnumerator := nil;
end;

procedure TCustomConnectDialog.CheckServerEnumerator;
begin
  if not (FServerEnumerator is GetServerEnumeratorClass) then begin
    FreeServerEnumerator;
    CreateServerEnumerator;
  end;
end;

{$IFDEF MSWINDOWS}
function TCustomConnectDialog.GetString(Id: integer): string;
const
  BufLen = 50;
var
  Buf: array [0..BufLen] of char;
  Base: integer;
begin
  case FLabelSet of
    lsEnglish:
      Base := 100;
    lsFrench:
      Base := 200;
    lsGerman:
      Base := 300;
    lsItalian:
      Base := 400;
    lsPolish:
      Base := 500;
    lsPortuguese:
      Base := 600;
    lsRussian:
      Base := 0;
    lsSpanish:
      Base := 700;
  else
      Base := 100;
  end;

  Buf[0] := #0;
  LoadString(hInstance, Id + Base, @Buf, BufLen);
  Result := Buf;
end;
{$ENDIF}

procedure TCustomConnectDialog.SetLabelSet(Value: TLabelSet);
begin
  InSetLabelSet := True;
  try
  {$IFDEF MSWINDOWS}
    FLabelSet := Value;
    if FLabelSet <> lsCustom then begin
      FCaption := GetString(0);
      FConnectButton := GetString(4);
      FCancelButton := GetString(5);

      FServerOption.Caption := GetString(3);
      FUserNameOption.Caption := GetString(1);
      FPasswordOption.Caption := GetString(2);
    end;
  {$ELSE}
    FCaption := 'Connect';
    FConnectButton := 'Connect';
    FCancelButton := 'Cancel';

    FServerOption.Caption := 'Server';
    FUserNameOption.Caption := 'User Name';
    FPasswordOption.Caption := 'Password';
  {$ENDIF}
  finally
    InSetLabelSet := False;
  end;
end;

procedure TCustomConnectDialog.SetCaption(Value: string);
begin
  if not (csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FCaption := Value;
end;

function TCustomConnectDialog.GetUserNameLabel: string;
begin
  Result := FUserNameOption.Caption;
end;

procedure TCustomConnectDialog.SetUserNameLabel(Value: string);
begin
  FUserNameOption.Caption := Value;
end;

function TCustomConnectDialog.GetPasswordLabel: string;
begin
  Result := FPasswordOption.Caption;
end;

procedure TCustomConnectDialog.SetPasswordLabel(Value: string);
begin
  FPasswordOption.Caption := Value;
end;

function TCustomConnectDialog.GetServerLabel: string;
begin
  Result := FServerOption.Caption;
end;

procedure TCustomConnectDialog.SetServerLabel(Value: string);
begin
  FServerOption.Caption := Value;
end;

procedure TCustomConnectDialog.SetConnectButton(Value: string);
begin
  if not (csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FConnectButton := Value;
end;

procedure TCustomConnectDialog.SetCancelButton(Value: string);
begin
  if not (csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FCancelButton := Value;
end;

procedure TCustomConnectDialog.SetServerOption(Value: TConnectDialogOption);
begin
  FServerOption.Assign(Value);
end;

procedure TCustomConnectDialog.SetUserNameOption(Value: TConnectDialogOption);
begin
  FUserNameOption.Assign(Value);
end;

procedure TCustomConnectDialog.SetPasswordOption(Value: TConnectDialogOption);
begin
  FPasswordOption.Assign(Value);
end;

procedure TCustomConnectDialog.OptionChanged;
begin
  if not (csLoading in ComponentState) and not InSetLabelSet then
    FLabelSet := lsCustom;
end;

{ TDBAccessUtils }

class function TDBAccessUtils.IsSharedObjectDataType(Obj: TDAParam; DataType: TFieldType): boolean;
begin
  Result := Obj.IsSharedObjectDataType(DataType);
end;

class function TDBAccessUtils.IsBlobDataType(Obj: TDAParam; DataType: TFieldType): boolean;
begin
  Result := Obj.IsBlobDataType(DataType);
end;

class function TDBAccessUtils.GetNational(Obj: TDAParam): boolean;
begin
  Result := Obj.National;
end;

class procedure TDBAccessUtils.CheckConnection(Obj: TCustomDADataSet);
begin
  Obj.CheckConnection;
end;

class procedure TDBAccessUtils.CheckConnection(Obj: TCustomDASQL);
begin
  Obj.CheckConnection;
end;

class function TDBAccessUtils.UsedConnection(Obj: TCustomDADataSet): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class function TDBAccessUtils.UsedConnection(Obj: TCustomDASQL): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class function TDBAccessUtils.UsedConnection(Obj: TDAMetaData): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class function TDBAccessUtils.UsedConnection(Obj: TComponent): TCustomDAConnection;
begin
  Assert(Obj <> nil);
  if Obj is TCustomDADataSet then
    Result := TCustomDADataSet(Obj).UsedConnection
  else if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).UsedConnection
  else begin
    Result:= nil;
    Assert(False, Obj.ClassName);
  end;
end;

class procedure TDBAccessUtils.SetAutoCommit(Obj: TComponent; Value: boolean);
begin
  Assert(Obj <> nil);
  if Obj is TCustomDASQL then
    TCustomDASQL(Obj).AutoCommit := Value
  else
  if IsClass(Obj, TCustomDADataSet) then
    TCustomDADataSet(Obj).AutoCommit := Value
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetAutoCommit(Obj: TCustomDAConnection): boolean;
begin
  Result := Obj.AutoCommit;
end;

class function TDBAccessUtils.GetAutoCommit(Obj: TCustomDADataSet): boolean;
begin
  Result := Obj.AutoCommit;
end;

class function TDBAccessUtils.GetAutoCommit(Obj: TCustomDASQL): boolean;
begin
  Result := Obj.AutoCommit;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TDATransaction; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TDATransaction): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TCustomDADataSet; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TCustomDADataSet): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TCustomDASQL; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TCustomDASQL): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TCustomDAUpdateSQL; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TCustomDAUpdateSQL): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TDAMetaData; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TDAMetaData): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TCRDataSource; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TCRDataSource): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetLockLoginPrompt(Obj: TCustomDAConnection; Value: Boolean);
begin
  Obj.FLockLoginPrompt := Value;
end;

class function TDBAccessUtils.GetIConnection(Obj: TCustomDAConnection): TCRConnection;
begin
  Result := Obj.FIConnection;
end;

class procedure TDBAccessUtils.CreateIConnection(Obj: TCustomDAConnection);
begin
  Obj.CreateIConnection;
end;

class function TDBAccessUtils.GetUpdateQuery(Obj: TCustomDADataSet): TComponent;
begin
  Result := Obj.FDataSetService.FUpdater.FUpdateQuery;
end;

class function TDBAccessUtils.GetTablesInfo(Obj: TCustomDADataSet): TCRTablesInfo;
begin
  if Obj.FIRecordSet <> nil then
    Result := Obj.FIRecordSet.TablesInfo
  else
    Result := nil;
end;

class function TDBAccessUtils.GetSQLInfo(Obj: TCustomDADataSet): TSQLInfo;
begin
  Result := Obj.SQLInfo;
end;

class function TDBAccessUtils.GetUpdatingTable(Obj: TCustomDADataSet): string;
begin
  Result := Obj.UpdatingTable;
end;

class procedure TDBAccessUtils.SetUpdatingTable(Obj: TCustomDADataSet; Value: string);
begin
  Obj.UpdatingTable := Value;
end;

class procedure TDBAccessUtils.InternalConnect(Obj: TCustomDAConnection);
begin
  Assert(Obj <> nil);
  Obj.InternalConnect;
end;

class procedure TDBAccessUtils.InternalDisconnect(Obj: TCustomDAConnection);
begin
  Assert(Obj <> nil);
  Obj.InternalDisconnect;
end;

class procedure TDBAccessUtils.DisconnectTransaction(Obj: TCustomDAConnection);
begin
  Obj.DisconnectTransaction;
end;

class procedure TDBAccessUtils.SuppressAutoCommit(Obj: TCustomDAConnection);
begin
  Obj.SuppressAutoCommit;
end;

class procedure TDBAccessUtils.RestoreAutoCommit(Obj: TCustomDAConnection);
begin
  Obj.RestoreAutoCommit;
end;

class function TDBAccessUtils.IsMultipleTransactionsSupported(Obj: TCustomDAConnection): boolean;
begin
  Result := Obj.IsMultipleTransactionsSupported;
end;

class function TDBAccessUtils.PushOperation(Obj: TCustomDAConnection; Operation: TConnLostCause; AllowFailOver: boolean = true): integer;
begin
  Result := Obj.PushOperation(Operation, AllowFailOver);
end;

class function TDBAccessUtils.PopOperation(Obj: TCustomDAConnection): TConnLostCause;
begin
  Result := Obj.PopOperation;
end;

class procedure TDBAccessUtils.RestoreAfterFailOver(Obj: TCustomDAConnection);
begin
  Obj.RestoreAfterFailOver;
end;

class function TDBAccessUtils.IsFailOverAllowed(Obj: TCustomDAConnection): boolean;
begin
  Result := Obj.IsFailOverAllowed;
end;

class function TDBAccessUtils.UsedTransaction(Obj: TCustomDAConnection): TDATransaction;
begin
  Result := Obj.UsedTransaction;
end;

class function TDBAccessUtils.UsedTransaction(Obj: TCustomDADataSet): TDATransaction;
begin
  Result := Obj.UsedTransaction;
end;

class function TDBAccessUtils.UsedTransaction(Obj: TCustomDASQL): TDATransaction;
begin
  Result := Obj.UsedTransaction;
end;

class function TDBAccessUtils.UsedTransaction(Obj: TComponent): TDATransaction;
begin
  Assert(Obj <> nil);
  if Obj is TCustomDADataSet then
    Result := TCustomDADataSet(Obj).UsedTransaction
  else if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).UsedTransaction
  else begin
    Result:= nil;
    Assert(False, Obj.ClassName);
  end;
end;

class function TDBAccessUtils.GetTransaction(Obj: TCustomDADataSet): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class function TDBAccessUtils.GetTransaction(Obj: TCustomDASQL): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class function TDBAccessUtils.GetTransaction(Obj: TDAMetaData): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class function TDBAccessUtils.GetDefaultTransaction(Obj: TCustomDAConnection): TDATransaction;
begin
  Result := Obj.DefaultTransaction;
end;

class procedure TDBAccessUtils.SetTransaction(Obj: TCustomDADataSet; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class procedure TDBAccessUtils.SetTransaction(Obj: TCustomDASQL; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class procedure TDBAccessUtils.SetTransaction(Obj: TDAMetaData; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class procedure TDBAccessUtils.SetDefaultTransaction(Obj: TCustomDAConnection; Value: TDATransaction);
begin
  Obj.DefaultTransaction := Value;
end;

class function TDBAccessUtils.GetFTransaction(Obj: TCustomDADataSet): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

class function TDBAccessUtils.GetFTransaction(Obj: TCustomDASQL): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

class function TDBAccessUtils.GetFTransaction(Obj: TDAMetaData): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

class function TDBAccessUtils.GetFDefaultTransaction(Obj: TCustomDAConnection): TDATransaction;
begin
  Result := Obj.FDefaultTransaction;
end;

class function TDBAccessUtils.GetITransaction(Obj: TDATransaction): TCRTransaction;
begin
  Result := Obj.FITransaction;
end;

class function TDBAccessUtils.GetConnectionCount(Obj: TDATransaction): integer;
begin
  Result := Obj.ConnectionsCount;
end;

class function TDBAccessUtils.GetConnection(Obj: TDATransaction; Index: integer): TCustomDAConnection;
begin
  if Obj.ConnectionsCount > Index then
    Result := Obj.Connections[Index]
  else
    Result := nil;
end;

class procedure TDBAccessUtils.Savepoint(Obj: TDATransaction; const Name: string);
begin
  Obj.DoSavepoint(Name);
end;

class procedure TDBAccessUtils.RollbackToSavepoint(Obj: TDATransaction; const Name: string);
begin
  Obj.DoRollbackToSavepoint(Name);
end;

class procedure TDBAccessUtils.ReleaseSavepoint(Obj: TDATransaction; const Name: string);
begin
  Obj.DoReleaseSavepoint(Name);
end;

class procedure TDBAccessUtils.CommitRetaining(Obj: TDATransaction);
begin
  Obj.DoCommitRetaining;
end;

class procedure TDBAccessUtils.RollbackRetaining(Obj: TDATransaction);
begin
  Obj.DoRollbackRetaining;
end;

class procedure TDBAccessUtils.GainTransaction(Obj: TDATransaction);
begin
  Obj.GainTransaction;
end;

class procedure TDBAccessUtils.ReleaseTransaction(Obj: TDATransaction);
begin
  Obj.ReleaseTransaction;
end;

class procedure TDBAccessUtils.AutoCommitTransaction(Obj: TDATransaction; NeedCommit: boolean);
begin
  Obj.AutoCommitTransaction(NeedCommit);
end;

class function TDBAccessUtils.GetMultiTransactionID(Obj: TDATransaction): Int64;
begin
  if (Obj.FITransaction <> nil) and Obj.FITransaction.GetInTransaction then
    Result := Obj.FITransaction.GetMultiTransactionID
  else
    Result := 0;
end;

class procedure TDBAccessUtils.Disconnect(Obj: TCustomDASQL);
begin
  Obj.Disconnect;
end;

class function TDBAccessUtils.SQLGenerator(Obj: TCustomDADataSet): TDASQLGenerator;
begin
  Result := Obj.FDataSetService.FSQLGenerator;
end;

class procedure TDBAccessUtils.GetKeyAndDataFields(
  Obj: TCustomDADataSet;
  out KeyAndDataFields: TKeyAndDataFields;
  const ForceUseAllKeyFields: boolean);
begin
  Assert(Obj.FIRecordSet <> nil);
  Obj.FIRecordSet.GetKeyAndDataFields(KeyAndDataFields, ForceUseAllKeyFields);
end;

class function TDBAccessUtils.GetLockDebug(Obj: TComponent): boolean;
begin
  if Obj is TCustomDADataSet then
    Result := TCustomDADataSet(Obj).FLockDebug
  else
  if Obj is TCustomDASQL then
    Result := TCustomDASQL(Obj).FLockDebug
  else
  begin
    Result := False;
    Assert(False, 'Obj is ' + Obj.ClassName);
  end;
end;

class procedure TDBAccessUtils.SetLockDebug(Obj: TComponent; Value: boolean);
begin
  if IsClass(Obj, TCustomDADataSet) then
    TCustomDADataSet(Obj).FLockDebug := Value
  else
  if IsClass(Obj, TCustomDASQL) then
    TCustomDASQL(Obj).FLockDebug := Value
  else
    Assert(False, 'Obj is ' + Obj.ClassName);
end;

class function TDBAccessUtils.FOwner(Obj: TDAConnectionOptions): TCustomDAConnection;
begin
  Result := Obj.FOwner;
end;

class function TDBAccessUtils.FOwner(Obj: TDADataSetOptions): TCustomDADataSet;
begin
  Result := Obj.FOwner;
end;

class function TDBAccessUtils.SQLMonitorClass(Obj: TCustomDAConnection): TClass;
begin
  Result := Obj.SQLMonitorClass;
end;

class function TDBAccessUtils.ConnectDialogClass(Obj: TCustomDAConnection): TConnectDialogClass;
begin
  Result := Obj.ConnectDialogClass;
end;

class function TDBAccessUtils.QuoteName(Obj: TCustomDADataSet; const AName: string): string;
begin
  Obj.CheckDataSetService;
  Result := Obj.QuoteName(AName);
end;

class function TDBAccessUtils.GetSQLs(Obj: TCustomDAConnection): TList;
begin
  Result := Obj.FSQLs;
end;

class procedure TDBAccessUtils.RegisterClient(Obj: TCustomDAConnection; Client: TObject; Event: TConnectChangeEvent = nil);
begin
  Obj.RegisterClient(Client, Event);
end;

class procedure TDBAccessUtils.UnRegisterClient(Obj: TCustomDAConnection; Client: TObject);
begin
  Obj.UnRegisterClient(Client);
end;

class function TDBAccessUtils.GetIdentityField(Obj: TCustomDADataSet): TCRFieldDesc;
begin
  Assert(Obj <> nil);
  Assert(Obj.FDataSetService <> nil);
  Result := Obj.FDataSetService.IdentityField;
end;

class function TDBAccessUtils.GetSQL(Obj: TComponent): TStrings;
begin
  Result := nil;
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).SQL
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).SQL
  else
    Assert(False, Obj.ClassName);
end;

class procedure TDBAccessUtils.SetSQL(Obj: TComponent; Value: TStrings);
begin
  Assert(Obj <> nil);
  if Obj is TCustomDASQL then
    TCustomDASQL(Obj).SQL := Value
  else
  if Obj is TCustomDADataSet then
    TCustomDADataSet(Obj).SQL := Value
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetSQLText(Obj: TComponent): string;
begin
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).FinalSQL
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).FinalSQL
  else
    Assert(False, Obj.ClassName);
end;

class procedure TDBAccessUtils.SetSQLText(Obj: TComponent; const SQLText: string;
  const LockScanParams, LockMacros: boolean);
begin
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then begin
    try
      TCustomDASQL(Obj).FLockMacros := LockMacros;
      TCustomDASQL(Obj).FLockScanParams := LockScanParams;
      TCustomDASQL(Obj).SQL.Text := SQLText;
    finally
      TCustomDASQL(Obj).FLockMacros := False;
      TCustomDASQL(Obj).FLockScanParams := False;
    end;
  end
  else
  if IsClass(Obj, TCustomDADataSet) then
    SetSQLText(TCustomDADataSet(Obj).FCommand, SQLText, LockScanParams, LockMacros)
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetParams(Obj: TComponent): TDAParams;
begin
  Result := nil;
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).Params
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).Params
  else
    Assert(False, Obj.ClassName);
end;

class procedure TDBAccessUtils.Execute(Obj: TComponent);
begin
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    TCustomDASQL(Obj).Execute
  else
  if IsClass(Obj, TCustomDADataSet) then
    TCustomDADataSet(Obj).Execute
  else
    Assert(False, Obj.ClassName);
end;

class procedure TDBAccessUtils.Open(Obj: TComponent);
begin
  Assert(Obj <> nil);
  Assert(Obj is TCustomDADataSet);
  TCustomDADataSet(Obj).Open;
end;

class function TDBAccessUtils.GetRowsAffected(Obj: TComponent): integer;
begin
  Result := 0;
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).RowsAffected
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).RowsAffected
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetParamsProcessed(Obj: TComponent): integer;
begin
  Result := 0;
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).ParamsProcessed
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).ParamsProcessed
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetUpdateSQLStatementTypes(Obj: TCustomDADataSet): TStatementTypes;
begin
  Result := Obj.GetUpdateSQLStatementTypes;
end;

class function TDBAccessUtils.GetUpdateSQLIndex(Obj: TCustomDADataSet; StatementType: TStatementType): TStrings;
begin
  Result := nil;
  if Assigned(Obj.UpdateObject) then
    if Obj.UpdateObject.GetObjectIndex(Ord(StatementType)) = nil then
      Result := Obj.UpdateObject.GetSQLIndex(Ord(StatementType))
    else
      Exit;
  if Result = nil then
    Result := Obj.GetUpdateSQLIndex(Ord(StatementType));
end;

class function TDBAccessUtils.ParseSQL(Obj: TCustomDASQL; const SQL: string; Params: TDAParams): string;
begin
  Result := Obj.ParseSQL(SQL, Params);
end;

class function TDBAccessUtils.CreateParamsObject(Obj: TCustomDASQL): TDAParams;
begin
  Result := Obj.CreateParamsObject;
end;

class procedure TDBAccessUtils.SetDesigning(Obj: TComponent; Value: Boolean; SetChildren: Boolean = True);
begin
  if Obj is TCustomDADataSet then
    TCustomDADataSet(Obj).SetDesigning(Value{$IFNDEF FPC}, SetChildren{$ENDIF})
  else
  if Obj is TCustomDASQL then
    TCustomDASQL(Obj).SetDesigning(Value{$IFNDEF FPC}, SetChildren{$ENDIF})
  else
  if Obj is TCustomDAConnection then
    TCustomDAConnection(Obj).SetDesigning(Value{$IFNDEF FPC}, SetChildren{$ENDIF})
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetIRecordSet(Obj: TCustomDADataSet): TCRRecordSet;
begin
  Result := Obj.FIRecordSet;
end;

class procedure TDBAccessUtils.CheckIRecordSet(Obj: TCustomDADataSet);
begin
  Obj.CheckIRecordSet;
end;

class function TDBAccessUtils.GetICommand(Obj: TComponent): TCRCommand;
begin
  Result := nil;
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := GetICommand(TCustomDASQL(Obj))
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := GetICommand(TCustomDADataSet(Obj))
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetICommand(Obj: TCustomDADataSet): TCRCommand;
begin
  Result := Obj.FICommand;
end;

class function TDBAccessUtils.GetICommand(Obj: TCustomDASQL): TCRCommand;
begin
  Result := Obj.FICommand;
end;

class function TDBAccessUtils.GetUpdater(Obj: TCustomDADataSet): TDADataSetUpdater;
begin
  Assert(Obj.FDataSetService <> nil);
  Result := Obj.FDataSetService.FUpdater;
end;

class function TDBAccessUtils.GetDataSetService(Obj: TCustomDADataSet): TDADataSetService;
begin
  Result := Obj.FDataSetService;
end;

class function TDBAccessUtils.GetDataSetClass(Obj: TCustomDAUpdateSQL): TCustomDADataSetClass;
begin
  Result := Obj.DataSetClass;
end;

class function TDBAccessUtils.GetSQLClass(Obj: TCustomDAUpdateSQL): TCustomDASQLClass;
begin
  Result := Obj.SQLClass;
end;

class function TDBAccessUtils.GetParserClass(Obj: TMacros): TSQLParserClass;
begin
  Result := Obj.FParserClass;
end;

{$IFDEF MSWINDOWS}
class procedure TDBAccessUtils.SaveServerListToRegistry(Obj: TCustomConnectDialog);
begin
  Obj.SaveServerListToRegistry;
end;
{$ENDIF}

class procedure TDBAccessUtils.SetConnection(Obj: TCustomConnectDialog; Value: TCustomDAConnection);
begin
  Obj.FConnection := Value;
end;

class procedure TDBAccessUtils.SetUseServerHistory(Obj: TCustomConnectDialog; Value: boolean);
begin
  Obj.UseServerHistory := Value;
end;

class function TDBAccessUtils.GetNeedConnect(Obj: TCustomConnectDialog): boolean;
begin
  Result := Obj.FNeedConnect;
end;

class procedure TDBAccessUtils.SetNeedConnect(Obj: TCustomConnectDialog; Value: boolean);
begin
  Obj.FNeedConnect := Value;
end;

class procedure TDBAccessUtils.CreateProcCall(Obj: TCustomDASQL; const Name: string; NeedDescribe: boolean;
  IsQuery: boolean = False);
begin
  Obj.InternalCreateProcCall(Name, NeedDescribe, IsQuery);
end;

class procedure TDBAccessUtils.CreateProcCall(Obj: TCustomDADataSet; const Name: string; NeedDescribe: boolean;
  IsQuery: boolean = False);
begin
  Obj.InternalCreateProcCall(Name, NeedDescribe, IsQuery);
end;

class function TDBAccessUtils.GetCommand(Obj: TCustomDAConnection): TCustomDASQL;
begin
  Result := Obj.FCommand;
end;

class function TDBAccessUtils.GetFieldTypeMapClass(Obj: TCustomDAConnection): TDAFieldTypeMapClass;
begin
  Result := Obj.GetFieldTypeMapClass;
end;

class function TDBAccessUtils.GetFieldTypeMapClass(Obj: TCustomDADataSet): TDAFieldTypeMapClass;
begin
  Result := TDAFieldTypeMapClass(Obj.GetFieldTypeMapClass);
end;

class function TDBAccessUtils.GetStreamedConnected(Obj: TCustomDAConnection): boolean;
begin
  Result := Obj.FStreamedConnected;
end;

class procedure TDBAccessUtils.Loaded(Obj: TCustomDAConnection);
begin
  Obj.Loaded;
end;

class function TDBAccessUtils.GetAsCursor(Obj: TDAParam): TCRCursor;
begin
  Result := Obj.AsCursor;
end;

class function TDBAccessUtils.GetCursor(Obj: TCustomDADataSet): TCRCursor;
begin
  Result := Obj.Cursor;
end;

class procedure TDBAccessUtils.SetCursor(Obj: TCustomDADataSet; Value: TCRCursor);
begin
  Obj.Cursor := Value;
end;

class function TDBAccessUtils.GetFetchAll(Obj: TCustomDADataSet): boolean;
begin
  // in UniDAC returns correct value only if DataSet.Active
  Result := Obj.FetchAll;
end;

class procedure TDBAccessUtils.SetFetchAll(Obj: TCustomDADataSet; Value: boolean);
begin
  // in UniDAC this value can be overriden from specific options
  Obj.FetchAll := Value;
end;

class procedure TDBAccessUtils.QuickOpen(Obj: TCustomDADataSet; var Info: TQuickOpenInfo);
begin
  Obj.QuickOpen(Info, False);
end;

class procedure TDBAccessUtils.Restore(Obj: TCustomDADataSet; const Info: TQuickOpenInfo);
begin
  Obj.Restore(Info, True);
end;

class function TDBAccessUtils.GetLockMode(Obj: TCustomDADataSet): TLockMode;
begin
  Result := Obj.LockMode;
end;

class procedure TDBAccessUtils.SetLockMode(Obj: TCustomDADataSet; const Value: TLockMode);
begin
  Obj.LockMode := Value;
end;

class function TDBAccessUtils.GetLastInsertId(Obj: TCustomDADataSet): int64;
begin
  Result := Obj.FLastInsertId;
end;

class function TDBAccessUtils.GetFullRefresh(Obj: TCustomDADataSet): boolean;
begin
  Result := Obj.Options.FullRefresh;
end;

{ TCRDataSource }

constructor TCRDataSource.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FDesignCreate := csDesigning in ComponentState;
end;

procedure TCRDataSource.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TCRDataSource.AssignTo(Dest: TPersistent);
begin
  if Dest is TDataSource then begin
    TDataSource(Dest).DataSet := DataSet;
    TDataSource(Dest).AutoEdit := AutoEdit;
    TDataSource(Dest).Enabled := Enabled;
  end else
    inherited;
end;

{ TDAConnections }

function TDAConnections.GetItems(Index: integer): TCustomDAConnection;
begin
  Result := TCustomDAConnection(inherited Items[Index]);
end;

{ TDATransactions}

function TDATransactions.GetItems(Index: Integer): TDATransaction;
begin
  Result := TDATransaction(inherited Items[Index]);
end;

{ TDATransaction }

constructor TDATransaction.Create(AOwner: TComponent);
begin
  inherited;

  FDesignCreate := csDesigning in ComponentState;

  FConnections := TDAConnections.Create;
  FTransactionType := ttNative;
  FIsolationLevel := ilReadCommitted;
  FDefaultCloseAction := taRollback;
end;

destructor TDATransaction.Destroy;
begin
  CloseTransaction(True);
  ClearRefs;

  FreeITransaction;
  FConnections.Free;

  if not FShareTransaction then
    TDASQLMonitorClass(SQLMonitorClass).ObjectDestroyed(Self);

  inherited;
end;

procedure TDATransaction.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TDATransaction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = DefaultConnection) then
    DefaultConnection := nil;

  inherited;
end;

function TDATransaction.GetITransactionClass: TCRTransactionClass;
begin
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  if FTransactionType = ttMTS then
    Result := TMTSTransaction
  else
{$ENDIF}
{$ENDIF}
  begin
    Assert(False, 'Must be overrided');
    Result := TCRTransaction;
  end;
end;

procedure TDATransaction.CheckITransaction;
var
  ClassType: TCRTransactionClass;
begin
  ClassType := GetITransactionClass;

  if not (FITransaction is ClassType) then begin
    FreeITransaction;

    Assert(not FShareTransaction or (FDefaultConnection <> nil));
    if FShareTransaction then begin
      Assert(FDefaultConnection.FIConnection <> nil);
      SetITransaction(FDefaultConnection.FIConnection.GetInternalTransaction);
    end
    else begin
      CreateITransaction;
    end;

  end;
end;

procedure TDATransaction.CreateITransaction;
begin
  SetITransaction(GetITransactionClass.Create);
end;

procedure TDATransaction.SetITransaction(Value: TCRTransaction);
begin
  if FITransaction <> nil then
    FreeITransaction;

  FITransaction := Value;

  if FITransaction <> nil then begin
    FITransaction.SetProp(prIsolationLevel, Variant(FIsolationLevel));
    FITransaction.SetProp(prTransactionReadOnly, FReadOnly);

    FITransaction.OnError := DoError;
    FITransaction.Component := Self;
  end;
end;

procedure TDATransaction.FreeITransaction;
begin
  if not FShareTransaction then
    FITransaction.Free;
  FITransaction := nil;
end;

procedure TDATransaction.ClearRefs;
var
  i, j: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    for j := 0 to FConnections[i].DataSetCount - 1 do
      if FConnections[i].DataSets[j] is TCustomDADataSet then begin
        if TCustomDADataSet(FConnections[i].DataSets[j]).Transaction = Self then
          TCustomDADataSet(FConnections[i].DataSets[j]).Transaction := nil;
      end
      else
      if FConnections[i].DataSets[j] is TDAMetaData then begin
        if TDAMetaData(FConnections[i].DataSets[j]).Transaction = Self then
          TDAMetaData(FConnections[i].DataSets[j]).Transaction := nil;
      end;

    for j := 0 to FConnections[i].FSQLs.Count - 1 do
      if TCustomDASQL(FConnections[i].FSQLs[j]).Transaction = Self then
        TCustomDASQL(FConnections[i].FSQLs[j]).Transaction := nil;
  end;

  for i := FConnections.Count - 1 downto 0 do
    DoRemoveConnection(FConnections[i]);
end;

procedure TDATransaction.SetIsolationLevel(Value: TCRIsolationLevel);
begin
  CheckInactive;
  if FITransaction <> nil then
    FITransaction.SetProp(prIsolationLevel, Variant(Value));
  FIsolationLevel := Value;
end;

procedure TDATransaction.SetReadOnly(Value: boolean);
begin
  CheckInactive;
  if FITransaction <> nil then
    FITransaction.SetProp(prTransactionReadOnly, Value);
  FReadOnly := Value;
end;

procedure TDATransaction.SetTransactionType(Value: TTransactionType);
begin
  CheckInactive;

  if FTransactionType <> Value then begin
    FreeITransaction;

    FTransactionType := Value;
  end;
end;

function TDATransaction.GetConnection(Index: integer): TCustomDAConnection;
begin
  Result := FConnections.Items[Index];
end;

function TDATransaction.GetConnectionsCount: integer;
begin
  Result := FConnections.Count;
end;

function TDATransaction.DetectInTransaction(CanActivate: boolean = False): boolean;
begin
  if FITransaction <> nil then
    Result := FITransaction.GetInTransaction
  else
    Result := False;
end;

function TDATransaction.GetActive: boolean;
begin
  Result := DetectInTransaction;
end;

procedure TDATransaction.CheckActive;
begin
  if not Active then
    raise Exception.Create(SNotInTransaction);
end;

procedure TDATransaction.CheckInactive;
begin
  if Active then
    raise Exception.Create(SInTransaction);
end;

procedure TDATransaction.Reset; //This function called in case of FatalError + Failover
var
  i, j: Integer;
begin
  if Active then begin
    if FITransaction.CanRestoreAfterFailover then
      FFailOverSatus := FTrStartCount
    else
      FFailOverSatus := 0;

    //Close RecordSets handles
    for i := 0 to FConnections.Count - 1 do begin
      for j := 0 to FConnections[i].DataSetCount - 1 do
        if (FConnections[i].DataSets[j] is TCustomDADataSet) and
          (TCustomDADataSet(FConnections[i].DataSets[j]).UsedTransaction = Self) and
          (TCustomDADataSet(FConnections[i].DataSets[j]).FIRecordSet <> nil)
        then
          try
            TCustomDADataSet(FConnections[i].DataSets[j]).FIRecordSet.Disconnect;
          except
            on E: Exception do
              ; //catch handle freeing exceptions
          end;
      for j := 0 to FConnections[i].FSQLs.Count - 1 do
        if TCustomDASQL(FConnections[i].FSQLs[j]).UsedTransaction = Self then
          try
            TCustomDASQL(FConnections[i].FSQLs[j]).Disconnect;
          except
            on E: Exception do
              ; //catch handle freeing exceptions
          end;
    end;

    // Close transaction handle
    try
      case FDefaultCloseAction of
        taCommit:
          FITransaction.Commit;
        taRollback:
          FITransaction.Rollback;
      end;
    except
      FITransaction.Reset;
    end;

    if FTrStartCount = 1 then //to reduce conflict with ConnectCount Restoring in StartTransaction
      for i := 0 to FConnections.Count - 1 do
        FConnections[i].InternalDisconnect; // Decrease ConnectCount
  end
  else
    FFailOverSatus := 0;
  FTrStartCount := 0; //Transaction is closing so reset FTrStartCount
  FExplicitlyStarted := False;
  FUnCommitedStatementCount := 0;
  UnPrepareTransaction;
end;

procedure TDATransaction.Restore; //This function restore transaction at failover time
begin
  if FFailOverSatus > 0 then begin
    if not Active then
      FITransaction.StartTransaction;
    FTrStartCount := FFailOverSatus;
  end;
end;

procedure TDATransaction.CloseDataSets;
var
  i,j: Integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    if not FConnections[i].IsMultipleTransactionsSupported then
      continue;

    for j := 0 to FConnections[i].DataSetCount - 1 do
      if FConnections[i].DataSets[j] is TCustomDADataSet then begin
        if TCustomDADataSet(FConnections[i].DataSets[j]).UsedTransaction = Self then
          TCustomDADataSet(FConnections[i].DataSets[j]).ConnectChange(FConnections[i], False);//We can't use SendConectEvent(False)
      end
      else
      if FConnections[i].DataSets[j] is TDAMetaData then begin
        if TDAMetaData(FConnections[i].DataSets[j]).UsedTransaction = Self then
          TDAMetaData(FConnections[i].DataSets[j]).ConnectChange(FConnections[i], False);//We can't use SendConectEvent(False)
      end;
    for j := 0 to TCustomDAConnection(FConnections[i]).FSQLs.Count - 1 do
      if TCustomDASQL(FConnections[i].FSQLs[j]).UsedTransaction = Self then
         TCustomDASQL(TCustomDAConnection(FConnections[i]).FSQLs[j]).ConnectChange(FConnections[i], False);
  end;
end;

function TDATransaction.SQLMonitorClass: TClass;
begin
  Result := nil;
  Assert(False, 'Must be overrided');
end;

function TDATransaction.UsedConnection: TCustomDAConnection;
begin
  if FDefaultConnection <> nil then
    Result := FDefaultConnection
  else
  if FConnections.Count > 0 then
    Result := FConnections[0]
  else
    Result := nil;
end;

procedure TDATransaction.CloseTransaction(Force: boolean = False);
begin
  if Active then
    try
      case FDefaultCloseAction of
        taCommit:
          Commit;
        taRollback:
          Rollback;
      end;
    except
      on E: EDAError do begin
        if not((csDestroying in ComponentState) and E.IsFatalError) then
          if not Force then
            raise
          else
            if FITransaction <> nil then
              FITransaction.Reset;
      end
      else
        if not Force then
          raise;
    end;
end;

procedure TDATransaction.GainTransaction;
var
  StoredTrStartCount: integer;
begin
  Inc(FTrStartCount);
  StoredTrStartCount := FTrStartCount;
  // register using ConnectCount only once
  if (FTrStartCount = 1) and not Active then begin
    // transaction could be active here in non disconnect mode
    StartTransaction;
    FExplicitlyStarted := False;
  end;
  // restore TrStartCount only if sucessfuly started
  // (FTrStartCount will be 0 in case of fatal error Reset)
  FTrStartCount := StoredTrStartCount;
end;

procedure TDATransaction.AutoCommitTransaction(NeedCommit: boolean);
begin
  if NeedCommit and (not FExplicitlyStarted or CanAutoCommitExplicitTransaction) then begin
    if FTrStartCount = 1 then
      Commit
    else
      if FTrStartCount > 1 then
        DoCommitRetaining;
  end
  else
    Inc(FUnCommitedStatementCount);
end;

procedure TDATransaction.ReleaseTransaction;
begin
  if FTrStartCount = 1 then begin
    if FDisconnectedMode then begin
      if FUnCommitedStatementCount = 0 then
        CloseTransaction
      else
        Dec(FTrStartCount);
    end
  end
  else
    if FTrStartCount > 1 then
      Dec(FTrStartCount);
end;

function TDATransaction.CanAutoCommitExplicitTransaction: boolean;
begin
  Result := True;
end;

procedure TDATransaction.PrepareTransaction(CheckOnly: boolean = False);
var
  i, j: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    if not (not CheckOnly and FConnections[i].Options.DisconnectedMode or
      FConnections[i].Connected)
    then
      raise Exception.Create(SConnectionInTransactionNotActive);
  end;

  if not CheckOnly then
    for i := 0 to FConnections.Count - 1 do
      if FConnections[i].Options.DisconnectedMode then
        try
          FConnections[i].InternalConnect;
        except
          //restore Connection.ConnectCount
          for j := 0 to i do
            if FConnections[i].Options.DisconnectedMode and FConnections[j].Connected then
              FConnections[j].InternalDisconnect; //To avoid ConnectClose exception with the [i] connection
          raise;
        end;

  CheckITransaction;

  FDisconnectedMode := True;
  for i := 0 to FConnections.Count - 1 do begin
    FITransaction.AddConnection(FConnections[i].FIConnection);
    FDisconnectedMode := FDisconnectedMode and FConnections[i].Options.DisconnectedMode;
  end;

  FPrepared := not CheckOnly;
end;

procedure TDATransaction.UnPrepareTransaction;
var
  i: integer;
begin
  if not FPrepared then
    exit;

  FPrepared := False;

  FTrStartCount := 0;
  FExplicitlyStarted := False;
  FUnCommitedStatementCount := 0;

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].IsMultipleTransactionsSupported then
      FConnections[i].RestoreAutoCommit;

  for i := 0 to FConnections.Count - 1 do
    FConnections[i].InternalDisconnect; // Decrease ConnectCount
end;

procedure TDATransaction.StartTransaction;
var
  i: integer;
  ReStart: boolean;
  Connection: TCustomDAConnection;
  MessageID: cardinal;
begin
  CheckInactive;

  for i := 0 to FConnections.Count - 1 do begin
    if not FConnections[i].IsMultipleTransactionsSupported and
      FConnections[i].DetectInTransaction
    then
      raise Exception.Create(SMultipleTransactionsNotSupported);
  end;

  PrepareTransaction;

  TDASQLMonitorClass(SQLMonitorClass).TRStart(Self, MessageID, True);

  Connection := nil;
  if FConnections.Count = 1 then begin //Failover allowed only with non-distributed transactions
    Connection := FConnections[0];
    Connection.PushOperation(clTransStart, Connection.IsFailOverAllowed);
  end;

  StartWait;
  try
    repeat
      ReStart := False;
      try
        FITransaction.StartTransaction;
      except
        on E: EFailOver do
          if E.FConnLostCause = clTransStart then begin
            Connection.RestoreAfterFailOver; //Restore all read transactions
            PrepareTransaction;
            ReStart := True  //We should pass clConnectionApplyUpdates FailOver
          end
          else
            raise;
      end;
    until not ReStart;
  finally
    StopWait;
    if Connection <> nil then
      Connection.PopOperation;
    if not Active then  //In case of fatal error during transaction start restore ConnectCount
                        //or even close connection in DisconnectedMode
      for i := 0 to FConnections.Count - 1 do
        TDBAccessUtils.InternalDisconnect(FConnections[i]);
  end;

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].IsMultipleTransactionsSupported then
      FConnections[i].SuppressAutoCommit;

  FTrStartCount := 1;
  FExplicitlyStarted := True;
  TDASQLMonitorClass(SQLMonitorClass).TRStart(Self, MessageID, False);

  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TDATransaction.Commit;
var
  MessageID: cardinal;
begin
  // In ODAC Commit/Rollback can be used without explicit transaction start.
  if not FPrepared then
    PrepareTransaction;

  TDASQLMonitorClass(SQLMonitorClass).TRCommit(Self, MessageID, True);
  StartWait;
  CloseDataSets;
  try
    FITransaction.Commit;
    TDASQLMonitorClass(SQLMonitorClass).TRCommit(Self, MessageID, False);
  finally
    if (FITransaction = nil) or not FITransaction.GetInTransaction then
      UnPrepareTransaction;
    StopWait;
  end;

  if Assigned(FOnCommit) then
    FOnCommit(Self);
end;

procedure TDATransaction.Rollback;
var
  MessageID: cardinal;
begin
  if not FPrepared then
    PrepareTransaction;

  TDASQLMonitorClass(SQLMonitorClass).TRRollback(Self, MessageID, True);
  StartWait;

  CloseDatasets;
  try
    FITransaction.Rollback;
    TDASQLMonitorClass(SQLMonitorClass).TRRollback(Self, MessageID, False);
  finally
    if (FITransaction = nil) or not FITransaction.GetInTransaction then
      UnPrepareTransaction;
    StopWait;
  end;

  if Assigned(FOnRollback) then
    FOnRollback(Self);
end;

procedure TDATransaction.DoCommitRetaining;
var
  MessageID: cardinal;
begin
  CheckActive;
  TDASQLMonitorClass(SQLMonitorClass).TRCommitRetaining(Self, MessageID, True);

  FITransaction.CommitRetaining;
  FUnCommitedStatementCount := 0;

  TDASQLMonitorClass(SQLMonitorClass).TRCommitRetaining(Self, MessageID, False);

  if Assigned(FOnCommitRetaining) then
    FOnCommitRetaining(Self);
end;

procedure TDATransaction.DoRollbackRetaining;
var
  MessageID: cardinal;
begin
  CheckActive;
  TDASQLMonitorClass(SQLMonitorClass).TRRollbackRetaining(Self, MessageID, True);

  FITransaction.RollbackRetaining;
  FUnCommitedStatementCount := 0;

  TDASQLMonitorClass(SQLMonitorClass).TRRollbackRetaining(Self, MessageID, False);

  if Assigned(FOnRollbackRetaining) then
    FOnRollbackRetaining(Self);
end;

procedure TDATransaction.DoSavepoint(const Name: string);
var
  MessageID: cardinal;
begin
  // In ODAC savepoints can be used without explicit transaction start
  if not FPrepared then
    PrepareTransaction(True);

  TDASQLMonitorClass(SQLMonitorClass).TRSavepoint(Self, Name, MessageID, True);

  FITransaction.Savepoint(Name);

  TDASQLMonitorClass(SQLMonitorClass).TRSavepoint(Self, Name, MessageID, False);
end;

procedure TDATransaction.DoReleaseSavepoint(const Name: string);
var
  MessageID: cardinal;
begin
  if not FPrepared then
    PrepareTransaction(True);

  TDASQLMonitorClass(SQLMonitorClass).TRReleaseSavepoint(Self, Name, MessageID, True);

  FITransaction.ReleaseSavepoint(Name);

  TDASQLMonitorClass(SQLMonitorClass).TRReleaseSavepoint(Self, Name, MessageID, False);
end;

procedure TDATransaction.DoRollbackToSavepoint(const Name: string);
var
  MessageID: cardinal;
begin
  if not FPrepared then
    PrepareTransaction(True);

  TDASQLMonitorClass(SQLMonitorClass).TRRollbackToSavepoint(Self, Name, MessageID, True);

  FITransaction.RollbackToSavepoint(Name);

  TDASQLMonitorClass(SQLMonitorClass).TRRollbackToSavepoint(Self, Name, MessageID, False);
end;

function TDATransaction.InternalAddConnection(Connection: TCustomDAConnection): integer;
begin
  if Active then
    if TransactionType = ttMTS then
      FITransaction.AddConnection(Connection.FIConnection)
    else
      CloseTransaction;
  Result := FConnections.IndexOf(Connection);
  if Result = -1 then
    Result := FConnections.Add(Connection);
end;

procedure TDATransaction.InternalRemoveConnection(Connection: TCustomDAConnection);
begin
  if Active and (TransactionType <> ttMTS) then
    CloseTransaction;

  if Connection = FDefaultConnection then
    FDefaultConnection := nil;
  FConnections.Remove(Connection);
  if not FShareTransaction and (Connection <> nil) and (FITransaction <> nil) then
    FITransaction.RemoveConnection(Connection.FIConnection);
end;

function TDATransaction.DoAddConnection(Connection: TCustomDAConnection): integer;
begin
  Result := InternalAddConnection(Connection);

  if Connection <> nil then
    Connection.InternalAddTransaction(Self);
end;

procedure TDATransaction.DoRemoveConnection(Connection: TCustomDAConnection);
begin
  InternalRemoveConnection(Connection);

  if Connection <> nil then
    Connection.InternalRemoveTransaction(Self);
end;

procedure TDATransaction.DoClearConnections;
begin
  while FConnections.Count > 0 do
    DoRemoveConnection(FConnections[0]);
end;

procedure TDATransaction.DoError(E: Exception; var Fail: boolean);
begin
  TDASQLMonitorClass(SQLMonitorClass).DBError(EDAError(E));

  if Assigned(FOnError) then
    FOnError(Self, EDAError(E), Fail);

  if not FInProcessError and EDAError(E).IsFatalError then begin
    FInProcessError := True;
    try
      CloseTransaction(True);
    except // don't raise exception
    end;
    FInProcessError := False;
  end;
end;

procedure TDATransaction.SetDefaultConnection(Value: TCustomDAConnection);
begin
  if Value <> FDefaultConnection then begin
    if FDefaultConnection <> nil then begin
      FDefaultConnection.RemoveFreeNotification(Self);
      DoRemoveConnection(FDefaultConnection);
    end;

    if Value <> nil then begin
      DoAddConnection(Value);
      Value.FreeNotification(Self);
    end;

    FDefaultConnection := Value;
  end;
end;

function TDATransaction.IsInternalTrStored: boolean;
begin
  if FDefaultConnection <> nil then
    Result := FDefaultConnection.FInternalDefTransaction <> Self
  else
    Result := False;
end;

initialization
  SetCursorProc := nil;
  ShowConnectFormProc := nil;
  ShowConnectFormProcFmx := nil;
  FieldTypeInfos := TFieldTypeInfos.Create;

finalization
  FieldTypeInfos.Free;

end.

