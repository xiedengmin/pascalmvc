
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Oracle Classes
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraClassesUni;

{$J+}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Types, SyncObjs, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRTimeStamp, CRAccess, CRParser, CRVio, MemData,
{$IFDEF NEXTGEN}
  Generics.Collections,
{$ENDIF}
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  OraCall, OraNumber, OraDateTime, OraInterval, OraError;
{$ELSE}
  OraCallUni, OraNumberUni, OraDateTimeUni, OraIntervalUni, OraErrorUni;
{$ENDIF}

const
  dtUndefined      = 100;
  dtRowId          = 101;
  dtURowId         = 102;
  dtOraBlob        = 103;
  dtOraClob        = 104;
  dtWideOraClob    = 105;
  dtBFILE          = 106;
  dtCFILE          = 107;
  dtLabel          = 108;  // MLSLABEL
  dtTimeStamp      = 109;
  dtTimeStampTZ    = 110;
  dtTimeStampLTZ   = 111;
  dtIntervalYM     = 112;
  dtIntervalDS     = 113;
  dtNumber         = 114;
  dtNumberFloating = 115; // NUMBER with floating point (without Prec and scale)
  dtBFloat         = 116;
  dtBDouble        = 117;
  dtFixedNChar     = 118;
  dtFixedNWideChar = 119;
  dtNString        = 120;
  dtNWideString    = 121;
  dtNClob          = 122;
  dtAnyData        = 123;

// obsolete
  dtBLOBLocator = dtOraBlob;
  dtCLOBLocator = dtOraClob;

  RowIdSize = 18;
  MaxBlobSize: Integer = 2147483647;
  MaxTransactionIdLength = 64; // Maximum length for TransactionId and BranchQualifier

type
  TOraCursor = class;
  TOraLob = class;
  TOraFile = class;
  TOraTimeStamp = class;
  TOraInterval = class;
  TOraNumber = class;
  TOraParamDesc = class;
  TOCIConnection = class;
  TOCICommand = class;
  TOCIFieldDesc = class;
{$IFNDEF LITE}
  TOCITransaction = class;
{$ENDIF}

{ OraAccess level }

  TTransactionMode = (tmReadOnly, tmReadWrite, tmReadCommitted, tmSerializable);
  TConnectMode = (cmNormal, cmSysOper, cmSysDBA, cmSysASM, cmSysBackup, cmSysDG, cmSysKM);
  TStatementMode = (smUnknown, smAllocated, smPrepared, smCached);

{ TOraParamDesc }

  TOraParamDesc = class (TParamDesc)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TOCICommand;
    FValueData: IntPtr;
    FValueSize: Integer;
    FHasValueLen: Boolean;
    FValueLens: array of ub2;
    FDefIndicator: sb2;
    FIndicator: IntPtr;
    FIndicators: IntPtr;
    FTable: Boolean;
    FActualTableLen: Integer;
    FHandle: IntPtr;
    FBlobPiece: Integer;  // number of piece
    FHasDefault: Boolean;
    FBufferAllocated: Boolean;
    FBufferBinded: Boolean;
    FCanBeOutput: Boolean;
  {$IFDEF LITE}
    FOriginalDataType: Word;
  {$ENDIF}

    function GetOCISvcCtx: TOCISvcCtx;
    function GetOCI8: TOCI8API;
    function GetActualLength: integer;
    function UnicodeIsUTF16: Boolean;
    procedure SetActualLength(Value: integer);
    property ActualLength: integer read GetActualLength write SetActualLength;

  protected
    procedure AllocBuffer; override;
    procedure FreeBuffer; override;

    procedure ValidateParamValue;
    procedure ClearBindData;

    property OCISvcCtx: TOCISvcCtx read GetOCISvcCtx;
    property OCI8: TOCI8API read GetOCI8;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    class function NeedAssignScalarValues: boolean; override;
    class function NeedAssignObjectValues: boolean; override;

    function GetMinDefaultSize: Integer; override;
    function GetMaxStringSize(Connection: TCRConnection): Integer; override;
    procedure SetDataType(Value: word); override;
    procedure SetParamType(Value: TParamDirection); override;
    procedure SetSize(Value: integer); override;
    procedure SetTable(Value: boolean);
    procedure SetHasDefault(Value: boolean);

    // TEMP for describe
    function GetTable: boolean;
    function GetLength: integer;
    function GetHasDefault: boolean;
    function GetCanBeOutput: boolean;
  {$IFDEF LITE}
    function GetOriginalDataType: word;
  {$ENDIF}

    function GetIndicator(Index: integer): smallint; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetIndicator(Index: integer; Value: smallint); {$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure TrimFixedChar;
    procedure TrimWideFixedChar;

  {$IFDEF LITE}
    procedure GetDbxBuffer(BufPtr: IntPtr; BufLen: Integer; out DataSize: Integer);
    procedure SetDbxBuffer(DataPtr: IntPtr; DataSize: Cardinal);
  {$ENDIF}
    function IndicatorPtr: IntPtr;
    procedure SyncIndicator(Connection: TOCIConnection);

    function GetItemAsDateTime(Index: integer): TDateTime;
    procedure SetItemAsDateTime(Index: integer; Value: TDateTime);
    function GetItemAsFloat(Index: integer): double;
    procedure SetItemAsFloat(Index: integer; Value: double);
    function GetItemAsSingle(Index: integer): single;
    procedure SetItemAsSingle(Index: integer; Value: single);
    function GetItemAsCurrency(Index: integer): currency;
    procedure SetItemAsCurrency(Index: integer; Value: currency);
    function GetItemAsInteger(Index: integer): integer;
    procedure SetItemAsInteger(Index: integer; Value: integer; DataType: Word = dtInteger);
    function GetItemAsLargeInt(Index: integer): Int64;
    procedure SetItemAsLargeInt(Index: integer; Value: Int64);
    procedure GetItemAsBcd(Index: integer; var Value: TBCD);
    procedure SetItemAsBcd(Index: integer; const Value: TBCD);
    procedure GetItemAsSQLTimeStamp(Index: integer; var Value: TSQLTimeStamp);
    procedure SetItemAsSQLTimeStamp(Index: integer; const Value: TSQLTimeStamp);
    procedure GetItemAsSQLTimeStampOffset(Index: integer; var Value: TSQLTimeStampOffset);
    procedure SetItemAsSQLTimeStampOffset(Index: integer; const Value: TSQLTimeStampOffset);
    function GetItemAsString(Index: integer): string;
    procedure SetItemAsString(Index: integer; const Value: string);
{$IFDEF NEXTGEN}
  protected
{$ENDIF}
    function GetItemAsAnsiString(Index: integer): AnsiString;
    procedure SetItemAsAnsiString(Index: integer; const Value: AnsiString);
  public
    function GetItemAsWideString(Index: integer): WideString;
    procedure SetItemAsWideString(Index: integer; const Value: WideString);
    function GetItemAsBoolean(Index: integer): boolean;
    procedure SetItemAsBoolean(Index: integer; Value: boolean);

    function GetItemObject(Index: integer): TSharedObject; override;
    procedure SetItemObject(Index: integer; Value: TSharedObject); override;
    function GetItemValue(Index: integer): Variant; override;
    procedure SetItemValue(Index: integer; const Value: Variant); override;

    procedure SetItemBinaryValue(Index: integer; DataPtr: IntPtr; DataSize: Integer);
  {$IFNDEF LITE}
    procedure SetItemEncryptValue(Index: integer; const Value: variant);
  {$ENDIF}

    function GetValue: variant; override;
    procedure SetValue(const Value: variant); override;
    procedure SetValueArr(PValueArr: PVariantArray); override;
    procedure SetObjectArr(PValueArr: PVariantArray); override;

    function GetAsBlobRef: TBlob;
    function GetAsCursor: TOraCursor;
    function GetAsOraBlob: TOraLob;
    function GetAsBFile: TOraFile;
    function GetAsTimeStamp: TOraTimeStamp;
    function GetAsInterval: TOraInterval;
    function GetAsNumber: TOraNumber;

    function IsObjectValue: boolean; override;
    function GetObject: TSharedObject; override;
    procedure SetObject(Value: TSharedObject); override;

    function GetNull: boolean; override;
    procedure SetNull(const Value: boolean); override;
    function GetItemNull(Index: integer): boolean; override;
    procedure SetItemNull(Index: integer; Value: boolean); override;

    property Name: string read GetName write SetName;
    property DataType: word read FDataType write SetDataType;
    property SubDataType: word read FSubDataType write FSubDataType;
    property ParamType: TParamDirection read FParamType write SetParamType;
    property Size: integer read FSize write SetSize;
  end;

{ TOCIConnection }

  TRunMethod = procedure of object;
  TEndMethod = procedure(E: Exception) of object;

{$IFNDEF MSWINDOWS}
  THandle = integer;
{$ENDIF}

  TNlsParamType = (nlsDateLanguage, nlsDateFormat, nlsNumericCharacters, nlsTimeStampFormat,
    nlsTimeStampTZFormat);

  TNlsSessionParam = record
    Name: string;
    Value: string;
    IsUserDefined: boolean;
  end;

  TFailoverCallback = procedure (FailoverState: cardinal; FailoverType: cardinal;
    var Retry: boolean) of object;
  TInfoMessageCallback = procedure(Error: EOraError) of object;

  TConnectionType = (ctDefault, ctOCIPooled{$IFNDEF LITE}{$IFDEF MSWINDOWS}, ctMTSPooled{$ENDIF}{$ENDIF});
  TOptimizerMode = (omDefault, omFirstRows1000, omFirstRows100, omFirstRows10, omFirstRows1, omFirstRows, omAllRows, omChoose, omRule);

  TOCIConnection = class (TCRConnection)
  private
    FOCISvcCtx: TOCISvcCtx;
    FOCICallStyle: TOCICallStyle;
    FOCICallStyleCommand: TOCICallStyle;
    FThreadSafety: boolean;
    FMaxStringSize: word;
    FLastError: integer;
    FConnectMode: TConnectMode;
    FEnableIntegers: boolean;
    FEnableLargeint: boolean;
    FEnableNumbers: boolean;
    FEnableWideOraClob: boolean;
    FInternalName: string;
    FOracleVersionSt: string;
    FOracleVersionFull: string;
    FOracleVersion: word;
    FProxyConnection : TOCIConnection;
    FDisconnectMode: boolean;
    FConnectionTimeout: integer;
    FOCIPoolName: string;
    FStatementCache: boolean;
    FStatementCacheSize: integer;
    FHomeName: string;
    FUnicodeEnvironment: boolean;
    FDirect: boolean;
    FIPVersion: TIPVersion;
    FConnectionType: TConnectionType;
  {$IFNDEF LITE}
    FClientIdentifier: string;
    FOptimizerMode: TOptimizerMode;
    FSchema: string;
    FCachedSchema: string;
    FCachedUser: string;
    FSubscriptionPort: Integer;
  {$ENDIF}
  {$IFNDEF FPC}
    FEnableSQLTimeStamp: boolean;
  {$ELSE}
    FTimeStampAsString: boolean;
  {$ENDIF}
    FIntervalAsString: boolean;
    FUnicodeAsNational: boolean;

    FSmallintPrecision: integer;
    FIntegerPrecision: integer;
    FLargeIntPrecision: integer;
    FFloatPrecision: integer;
    FBCDPrecision, FBCDScale: integer;
    FFmtBCDPrecision, FFmtBCDScale: integer;

  { Charset parameters }
    FCharset: string;
    FCharsetId: word;
    FInternalCharsetId: word; // for Loader
    FCharLength: word;
    FQueryCharLength: boolean;
    FMaxCharLength: word;
    FMinCharLength: word;
    FUseUnicode: boolean;

  { NLS session parameters }
    FNlsParams: array[TNlsParamType] of TNlsSessionParam;

  { OCI73 }
    LDA: PLDA;
    HDA: PHDA;
  { OCI80 }
    hServer  : pOCIServer;
    hSession : pOCISession;
    hOCIAuthInfo : pOCIAuthInfo;
    //hTrans   : pOCITrans;

    FLock: TCriticalSection;
    FTimeoutThread: TThread;
  {$IFDEF MSWINDOWS}
  {$IFNDEF LITE}
    hMTSSvcCtx: pOCISvcCtx;
  {$ENDIF}
  {$ENDIF}

    procedure GetSessionParameters;
    procedure SetNlsParameter(const Name, Value: string);
    function GetMaxStringSize: word;

    function GetOCI7: TOCI7API;
    function GetOCI8: TOCI8API;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    function GetMTS: TMTSAPI;
  {$ENDIF}
  {$ENDIF}
  protected
    FOnFailover: TFailoverCallback;
    FOnInfoMessage: TInfoMessageCallback;

    procedure SetStatementCacheSize(Size: integer);
    procedure SetupConnection;
  {$IFNDEF LITE}
    procedure SetOptimizerMode;
    procedure SetClientIdentifier;
  {$ENDIF}

    procedure RaiseError(const Msg: string); virtual; // for TRIALCALL

  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    procedure MTSCheck(status: sword);
    procedure MTSError(var ErrorCode: sword; UseCallback: boolean);

    procedure Enlist(Transaction: TMTSTransaction); override;
    procedure UnEnlist(Transaction: TMTSTransaction); override;
  {$ENDIF}
  {$ENDIF}

    procedure InitCommandProp(Command: TCRCommand); override;

    property AutoCommit;
    property EnableBCD;
    property EnableFMTBCD;
    property ConvertEOL: boolean read FConvertEOL;
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

    procedure CheckOCI;
    procedure CheckOCI73;
    procedure CheckOCI80;

    procedure Check(Status: sword);
    procedure OraError(FOCICallStyle: TOCICallStyle; ErrorCode: sword; UseCallback: boolean; Component: TObject); virtual;

    function AllocEnvironment: TOCIEnvironment;
    procedure SetConnectionType(ConnectionType: TConnectionType);
    procedure InitSessionParameters;
    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;
    procedure Ping; override;

    function GetOracleVersion: word;
    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;

  {$IFNDEF LITE}
    procedure SetCurrentSchema(SchemaName : string);
    function GetCurrentSchema: string;
    function GetDefaultSchema: string;
    function GetCachedSchema: string;
    function GetCurrentUser: string;
    function GetCachedUser: string;
  {$ENDIF}

    procedure BreakExec;

  { Multi Thread }
    procedure Lock; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Release; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure RunTimeoutThread(Timeout: Integer);
    procedure StopTimeoutThread;
    function RunThread(RunMethod: TRunMethod; EndMethod: TEndMethod): TThread;
    function StopThread(var hThread: TThread{$IFDEF MSWINDOWS}; APeekMessage: boolean = False{$ENDIF}): boolean;

  { OCI73 }
    function GetLDA: PLDA;
    procedure SetLDA(Value: PLDA);

  { OCI80 }
    procedure SetOCISvcCtx(Value: TOCISvcCtx);

    procedure ChangePassword(NewPassword: string);

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    procedure SetNonBlocking(Value: boolean); // nonblocking connection
    function GetOCICallStyle: TOCICallStyle;
    procedure SetOCICallStyle(Value: TOCICallStyle);
    function GetOCICallStyleCommand: TOCICallStyle;
    function GetLastError: integer;
    procedure SetLastError(Value: integer);
    function GetDBKeyList(TableName: string): string;
    procedure GetTableFields(TableName: string; Fields: TStringList);

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    function CheckIsValid: boolean; override;

  {$IFNDEF NODBACCESS}
    procedure ReturnToPool; override;
  {$ENDIF}
    property Direct: boolean read FDirect;
    property OCISvcCtx: TOCISvcCtx read FOCISvcCtx;
    property OCI7: TOCI7API read GetOCI7;
    property OCI8: TOCI8API read GetOCI8;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    property MTS: TMTSAPI read GetMTS;
  {$ENDIF}
  {$ENDIF}
    property UnicodeEnvironment: boolean read FUnicodeEnvironment;
    property ProxyConnection : TOCIConnection read FProxyConnection write FProxyConnection;
    property OCICallStyle: TOCICallStyle read FOCICallStyle;
    property OCICallStyleCommand: TOCICallStyle read FOCICallStyleCommand;

    property OnFailover: TFailoverCallback read FOnFailover write FOnFailover;
    property OnInfoMessage: TInfoMessageCallback read FOnInfoMessage write FOnInfoMessage;
  end;

{ TOraCursor }

  TOraCursor = class (TCRCursor)
  private
    FCDA: PCDA;
    FphOCIStmt: ppOCIStmt;
    FStatementMode : TStatementMode;

    FOCISvcCtx: TOCISvcCtx;
    FOCI8: TOCI8API;
    FOCICallStyle: TOCICallStyle;
    FScrollable: boolean;
    FPrefetchRows: Integer;
    FDefaultPrefetchRows: Integer;

    procedure SetOCISvcCtx(Value: TOCISvcCtx);
    function GetCDA: PCDA;
    function GethOCIStmt: pOCIStmt; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SethOCIStmt(Value: pOCIStmt);
    function GetOCIStmt: pOCIStmt;
    function GetOCIStmtPtr: ppOCIStmt;
    procedure EnablePrefetching;
    procedure DisablePrefetching;
    procedure InitPrefetchRows(Value: integer);
    procedure SetPrefetchRows(Value: integer);
    procedure SetDefaultFetchRows(Value: integer);

    property hOCIStmt: pOCIStmt read GethOCIStmt write SethOCIStmt;
  protected
    FState: TCursorState;

    procedure Check(Status: sword);
    procedure CheckOCI;
    procedure CheckOCI73;
    procedure CheckOCI80;

    procedure Init(AOCISvcCtx: TOCISvcCtx; AOCICallStyle: TOCICallStyle);
    procedure ReleaseOCISvcCtx;

    property OCI8: TOCI8API read FOCI8;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AllocCursor(StatementMode: TStatementMode = smAllocated);
    procedure ReleaseCursor;
    procedure FreeCursor;
    procedure Disconnect; override;

    function CanFetch: boolean; override;

    property CDA: PCDA read GetCDA;
    property OCISvcCtx: TOCISvcCtx read FOCISvcCtx;
    property OCICallStyle: TOCICallStyle read FOCICallStyle;
    property OCIStmt: pOCIStmt read GetOCIStmt;
    property OCIStmtPtr: ppOCIStmt read GetOCIStmtPtr;
    property State: TCursorState read FState;
    property PrefetchRows: integer read FPrefetchRows write SetPrefetchRows;
    property DefaultPrefetchRows: Integer read FDefaultPrefetchRows write SetDefaultFetchRows;
  end;

{$IFDEF MSWINDOWS}
{ TOCIChangeNotification }

  TChangeNotifyEventType = (cneNone, cneStartup, cneShutdown, cneShutdownAny,
    cneDropDB, cneDereg, cneObjChange, cneQueryChange);

  TCustomNotifyChanges = class
  private
    FOCISvcCtx: TOCISvcCtx;

    function GetCount: integer;
  protected
    FItems: array of TObject;
    function CreateItem(ChangeDescriptor: IntPtr): TObject; virtual; abstract;
  public
    constructor Create(AOCISvcCtx: TOCISvcCtx; AOCIColl: pOCIColl);
    destructor Destroy; override;
    property Count: integer read GetCount;
  end;

  TNotifyRowChange = class
  private
    FRowId: string;
    FOperations: TChangeNotifyOperations;
  public
    constructor Create(AOCISvcCtx: TOCISvcCtx; AChangeDescriptor: IntPtr);
    property RowId: string read FRowId;
    property Operations: TChangeNotifyOperations read FOperations;
  end;

  TNotifyRowChanges = class(TCustomNotifyChanges)
  private
    function GetChanges(Index: integer): TNotifyRowChange;
  protected
    function CreateItem(ChangeDescriptor: IntPtr): TObject; override;
  public
    property Changes[Index: integer]: TNotifyRowChange read GetChanges; default;
  end;

  TNotifyTableChange = class
  private
    FTableName: string;
    FOperations: TChangeNotifyOperations;
    FRowChanges: TNotifyRowChanges;
  public
    constructor Create(AOCISvcCtx: TOCISvcCtx; AChangeDescriptor: IntPtr);
    destructor Destroy; override;
    property TableName: string read FTableName;
    property Operations: TChangeNotifyOperations read FOperations;
    property RowChanges: TNotifyRowChanges read FRowChanges;
  end;

  TNotifyTableChanges = class(TCustomNotifyChanges)
  private
    function GetChanges(Index: integer): TNotifyTableChange;
  protected
    function CreateItem(ChangeDescriptor: IntPtr): TObject; override;
  public
    property Changes[Index: integer]: TNotifyTableChange read GetChanges; default;
  end;

  TNotifyChange = class
  private
    FNotifyType: TChangeNotifyEventType;
    FTableChanges: TNotifyTableChanges;
  public
    constructor Create(AOCISvcCtx: TOCISvcCtx; AChangeDescriptor: IntPtr);
    destructor Destroy; override;
    property NotifyType: TChangeNotifyEventType read FNotifyType;
    property TableChanges: TNotifyTableChanges read FTableChanges;
  end;

  TChangeNotifyCallback = procedure(NotifyType: TChangeNotifyEventType;
    TableChanges: TNotifyTableChanges) of object;

  TOCIChangeNotification = class
  private
    FOCISvcCtx: TOCISvcCtx;
    FGCHandle: IntPtr;
    FEnabled: boolean;
    FPersistent: boolean;
    FTimeOut: integer;
    FQueryResultOnly: boolean;
    FOperations: TChangeNotifyDMLOperations;
    FOnChange: TChangeNotifyCallback;
    hOCISubscription: pOCISubscription;

    function GetGCHandle: IntPtr;
    function GetOCI8: TOCI8API;
    procedure SetOCISvcCtx(Value: TOCISvcCtx);
    procedure SetEnabled(Value: boolean);
    function CallbackChangeNotify(pCtx: IntPtr; pSubscrHp: pOCISubscription;
      pPayload: IntPtr; iPayloadLen: ub4; pDescriptor: IntPtr; iMode: ub4): sword;
  protected
    procedure ReleaseOCISvcCtx;

    property GCHandle: IntPtr read GetGCHandle;
    property OCI8: TOCI8API read GetOCI8;
    property OCISvcCtx: TOCISvcCtx read FOCISvcCtx;
  public
    constructor Create;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean;
    function GetProp(Prop: integer; var Value: variant): boolean;

    function GetSubscriptionHandle(Connection: TOCIConnection): pOCISubscription;
    procedure Register(Connection: TOCIConnection);
    procedure Unregister(Connection: TOCIConnection);
    function IsActive: boolean;

    property OnChange: TChangeNotifyCallback read FOnChange write FOnChange;
  end;
{$ENDIF}

{ TOCICommand }

  TOCICommand = class (TCRCommand)
  private
    FOCISvcCtx: TOCISvcCtx;
    FOCICallStyle: TOCICallStyle;
    FCommandTimeout: integer;
    FCursor: TOraCursor;
    FCursorRef: TOraCursor;
    FResultSets: TList;
    FNonBlocking: boolean;
    FSQLType: word;
    FLastSQLType: word;
    FRowsProcessed: integer;
    FFetchedRows: integer;
    FErrorOffset: word;
    FFieldsAsString: boolean;
    FCacheLobs: boolean;
    FStoreRowId: boolean;
    FRowId: string;
    FRawAsString: boolean;
    FNumberAsString: boolean;
    FUseDefaultDataTypes: boolean;
    FProcNamedParams: boolean;

    FSmallintPrecision: integer;
    FIntegerPrecision: integer;
    FLargeIntPrecision: integer;
    FFloatPrecision: integer;
    FBCDPrecision, FBCDScale: integer;
    FFmtBCDPrecision, FFmtBCDScale: integer;

    FOpenNext: boolean;
    FForceUnprepare: boolean;
    FGCHandle: IntPtr;
    FTemporaryLobUpdate: boolean;
    FPrefetchLobSize: Integer;
    FStatementCache: boolean;
  {$IFDEF MSWINDOWS}
    FChangeNotification: TOCIChangeNotification;
  {$ENDIF}
    FCheckParamHasDefault: boolean;
    FForceSPInit: boolean;
    FNullBuf: IntPtr;

    FLock: TCriticalSection;
  {$IFDEF MSWINDOWS}
    hExecThread: TThread;
  {$ENDIF}

    function GetGCHandle: IntPtr;
    function GetOCI7: TOCI7API; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetOCI8: TOCI8API; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function RemoveCRSymbols(SQLText: string; var ErrorOffset: integer): string;

  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TOCIConnection;

    procedure DoExecute;
    procedure EndExecute(E: Exception);

    procedure RaiseError(const Msg: string); virtual; // for TRIALCALL

    procedure AllocOCISvcCtx;
    procedure ReleaseOCISvcCtx;

    procedure CheckOCI;
    procedure CheckOCI73;
    procedure CheckOCI80;

    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckSession;

    procedure Check(Status: sword);

    function GetSmallintPrecision: integer;
    function GetIntegerPrecision: integer;
    function GetLargeintPrecision: integer;
    function GetFloatPrecision: integer;
    function GetBCDPrecision: integer;
    function GetBCDScale: integer;
    function GetFmtBCDPrecision: integer;
    function GetFmtBCDScale: integer;

    procedure DetectDataType(DBType: Word; DBLength: Integer; DBScale: SmallInt;
      ObjectType: TObjectType; LongStrings: boolean; out DataType: Word);

    function DescribeStoredProc(const objnam: string; ovrld: pub2; pos: pub2; level: pub2;
      argnam: IntPtr; arnlen: pub2; dtype: pub2; defsup: pub1; mode: pub1;
      dtsiz: pub4; prec: psb2; scale: psb2; radix: pub1;
      spare: pub4; var arrsiz: ub4): sword;
    procedure DescribeObjectParams(const Name: string; Overload: integer; ObjectParams: TStringList);
    procedure DescribeObjectParamType(const TypeName: string; Param: TOraParamDesc);

  { OCI73 }
    function GetOraType7(DataType: integer; SubDataType: integer{ = 0}): integer;
    function InternalFetch7(Rows: word): word;
    function InternalFetchPiece7: integer;
    procedure InitProcParams7(const Name: string; Overload: integer);

  { OCI80 }
    function GetInternalType8(DBType: Word): Integer;
    function GetOraType8(DataType: integer; SubDataType: integer; IsDefine: Boolean): integer;
    function InternalFetch8(Rows: word; Orientation: integer; Offset: integer): word;
    function InternalExecuteFetch8(Rows: word): word;
    function InternalFetchPiece8(Orientation: integer; Offset: integer): integer;
    procedure InitProcParams8(Name: string; Overload: integer);

    function CallbackInBind(Bind: pOCIBind; Iter: ub4; Index: ub4; var Buf: IntPtr;
      var BufLen: ub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;
    function CallbackOutBind(Bind: pOCIBind; Iter: ub4; Index: ub4; var Buf: IntPtr;
      var BufLen: pub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;

    procedure SetArrayLength(Value: integer);
    function GetActive: boolean;

    procedure CreateBatchCommand; override;
    function NeedBatchSavepoint: boolean; override;
    procedure InternalExecuteBatch(Iters, Offset: integer); override;

    property Params;
    property GCHandle: IntPtr read GetGCHandle;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    class function GetParamDescClass: TParamDescClass; override;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    function GetOraType(DataType: integer; SubDataType: integer; IsDefine: boolean): integer;

    procedure InternalOpen;
    procedure InternalParse;
    procedure InternalPrepare;

    function ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string; override;
    procedure ParseSQLType; override;
    function IsValidBatchSQL: boolean; override;
    procedure InitProcParams(const Name: string; Overload: integer);
    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; override;
    function ForceCreateSPParams: boolean; override;

    class function IsAllowedArrayType(DataType: Word): boolean; override;
    function IsLobPrefetchAllowed(IsNational, IsUnicode: Boolean): Boolean;
    function NeedBindParam(Param: TOraParamDesc): boolean;
    procedure BindParam(Param: TOraParamDesc);
    function InternalExecute(Mode: integer; Rows: Integer = 0): sword;
    procedure Exec;
    procedure DefineData(Field: TOCIFieldDesc; DataPtr, IndPtr, LenPtr: IntPtr; BufSkip, IndSkip, LenSkip: integer);
    procedure DefineDynamic7(Field: TOCIFieldDesc; Buf: IntPtr; Ind: psb2);
    procedure DefineDynamic8(Field: TOCIFieldDesc; Owner: IntPtr; Proc: IntPtr; CharsetId: Integer);

    function InternalFetch(Rows: word; Orientation: integer = OCI_FETCH_NEXT; Offset: integer = 0): word;
    function InternalFetchPiece(Orientation: integer = OCI_FETCH_NEXT; Offset: integer = 0): integer;
    procedure InternalCancel;
    procedure InternalClose;
    procedure Finish;
    procedure Close; override;

    procedure GetPI(var Handle: pOCIHandle; var Piece: byte; var Buf: IntPtr;
      var Iteration: cardinal; var Index: cardinal; var Mode: TParamDirection);
    procedure SetPI(Handle: pOCIHandle; HType: cardinal; Piece: byte; Buf: IntPtr;
      var BufLen: cardinal; Ind: psb2);

    function NativeCursor: boolean;
    function RowsReturn: boolean;
    procedure CheckRowsReturn;
    procedure InitResultSets;
    procedure ReleaseResultSets;

  { Params }
    function AddParam: TParamDesc; override;
    procedure BindParams;
    //procedure DisconnectParams;
    function GetParam(Index: integer): TOraParamDesc;

    procedure BreakExec; override;
    procedure HardBreak;

    procedure Lock;
    procedure Release;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;

    procedure Execute; override;

    procedure SetConnection(Value: TCRConnection); override;
    function GetCursor: TCRCursor; override;
    procedure SetCursor(Value: TCRCursor); override;
    function GetNextCursor: TOraCursor;
    procedure SetOCICallStyle(Value: TOCICallStyle);
    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;
    function GetSQLType: integer;
    function GetRowId: string;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    property EnableBCD;
    property EnableFMTBCD;

    property OCICallStyle: TOCICallStyle read FOCICallStyle;
    property OCISvcCtx: TOCISvcCtx read FOCISvcCtx;
    property OCI7: TOCI7API read GetOCI7;
    property OCI8: TOCI8API read GetOCI8;

  {$IFDEF MSWINDOWS}
    property ChangeNotification: TOCIChangeNotification read FChangeNotification write FChangeNotification;
  {$ENDIF}
  end;

{ TOCISQLInfo }

  TOCISQLInfo = class(TSQLInfo)
  protected
    function NextCharQuotesNeed(Ch: Char; IdCase: TIdentCase): boolean; override;
    function HasAsLexem: boolean; override;
    procedure ParseExtTableInfo(Parser: TSQLParser; var CodeLexem: integer; var StLex: string; var Name: string); override;
  public
    function IdentCase: TIdentCase; override;

    function NormalizeName(const Value: string; const LeftQ: Char; const RightQ: Char; QuoteNames: boolean = False; UnQuoteNames: boolean = False): string; overload; override;
    // for normal C++Builder header
    function NormalizeName(const Value: string; QuoteNames: boolean = False; UnQuoteNames: boolean = False): string; overload; override;

    procedure SplitObjectName(const Name: string; out Info: TSQLObjectInfo); override;

    class function ParseSPName(FullName: string; var Name: string; var Overload: integer): boolean;
    class function GetFinalSPName(Name: string; Overload: integer): string;
  end;

  TOCIFieldDesc = class(TCRFieldDesc)
  private
    FFetchBufferOffset: integer;
    FFetchBufferLenOffset: integer;
    FIsConverted: boolean;
    FIsBlobSubType: boolean;
  protected
    procedure SetDataType(Value: Word); override;
    procedure SetSubDataType(Value: Word); override;
  public
    constructor Create(RecordSetClass: TRecordSetClass); override;

    property FetchBufferOffset: integer read FFetchBufferOffset;
    property FetchBufferLenOffset: integer read FFetchBufferLenOffset;
    property IsBlobSubType: boolean read FIsBlobSubType;
    property IsConverted: boolean read FIsConverted;
  end;

{ TOCIRecordSet }

  TModifyAction = procedure of object;

  TOCIRecordSet = class (TCRRecordSet)
  private
    FOCISvcCtx: TOCISvcCtx;

    FAutoClose: boolean;
    FDeferredLobRead: boolean;
    hExecFetchThread: TThread;
    hFetchAllThread: TThread;
    FFetchCursor: TOraCursor;
    FPieceFetch: boolean;
    FFetchItems: IntPtr;  // for callback fetch
    // for backward fetch
    FFetchAbsolute: boolean;
    FFetchStart: integer;
    FFetchEnd: integer;
    FGCHandle: IntPtr;
    FHasConvertedFields: boolean;
  {$IFDEF MSWINDOWS}
    hEvent: TEvent;
  {$ENDIF}
    FFetching: boolean;
    FHasObjectFields: boolean;
    FTempFilterText: string;
    FDisableInitFields: boolean;
    FExpandedFields: TFieldDescs;
    FHideRowId: Boolean;

    //PreCached FConection properties
    FDisconnectedMode: boolean;
    FUseUnicode: boolean;
    FCharLength: integer;

    procedure AllocOCISvcCtx;
    procedure ReleaseOCISvcCtx;

    function HasCursorParams: boolean;
    function HasResultSets: boolean;
    procedure InitFetchCursor;

    function FetchArray(FetchBack: boolean = False): boolean;
    function FetchPiece(FetchBack: boolean = False): boolean;

    function GetNonBlocking: boolean;
    function GetGCHandle: IntPtr;
    function GetDisconnectedMode: boolean;
    function GetUseUnicode: boolean;
    function GetCharLength: integer;

    procedure Check(Status: Word);
    function GetOCI7: TOCI7API; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetOCI8: TOCI8API; {$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    FCommand: TOCICommand;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TOCIConnection;  // for perf

    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;

  { Open/Close }
    function NeedInitFieldsOnPrepare: boolean; override;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

  { Fields }
    function ExtFieldsInfoIsInternal: boolean; override;
    procedure CreateFieldDescs; override;
    procedure FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); override;
  {$IFNDEF LITE}
    procedure RequestFieldsInfo(Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo); override;
  {$ENDIF}
    function InternalCompareFieldValue(ValuePtr: IntPtr; ValueSize: Word; ValueType: Word; DataBuf: IntPtr; DataLen: Word; FieldType: Word; HasParent: boolean; IsFixed: boolean; const Options: TCompareOptions): integer; override;

    function GetFieldDesc(FieldNo: integer; var Field: TOCIFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
  { OCI70 }
    function GetFieldDesc7(FieldNo: integer; var Field: TOCIFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
  { OCI80 }
    function GetFieldDesc8(FieldNo: integer; var Field: TOCIFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
    function GetIndicatorItemSize: Integer; override;
    function GetEOF: boolean; override;

  { Filter/Find/Locate/Sorting }
    procedure SetFilterText(const Value: string); override;

  { Fetch }
    procedure AllocFetchBuffer; override;
    procedure FreeFetchBuffer; override;
    procedure ProcessFetchedBlock(Block: PBlockHeader; FetchBack: boolean); override;

    procedure InitBlock(Block: PBlockHeader); override;
    procedure SwapBlocks(Block1, Block2: PBlockHeader);
    procedure InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean); override;
    function CanFetchBack: boolean; override;
    function InternalFetch(FetchBack: boolean = False): boolean; override;
    function NeedInitFieldsOnFetch: boolean; override;
    function NeedUnPrepareAfterFetch: boolean; override;
    procedure DoBeforeFetch(out Cancel: boolean); override;
    procedure DoAfterFetch; override;

    function IsSupportedDataType(DataType: word): boolean; override;
    function IsSpecificType(const Field: TFieldDesc; var DataType: word; var DataTypeName: string; var Len, Scale: integer): boolean; override;

  { Items }
    procedure FreeAllItems;

  { Edit }
    procedure DoExecFetch;
    procedure EndExecFetch(E: Exception);
    procedure DoFetchAll;
    procedure DoFetchAllPulse;
    procedure EndFetchAll(E: Exception);

    function CallbackDefine(Define: pOCIDefine; Iter: cardinal; var Buf: IntPtr;
      var BufLen: pub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;

    property GCHandle: IntPtr read GetGCHandle;
    //PreCached FConection properties
    property DisconnectedMode: boolean read GetDisconnectedMode;
    property UseUnicode: boolean read GetUseUnicode;
    property CharLength: integer read GetCharLength;

    property OCISvcCtx: TOCISvcCtx read FOCISvcCtx;
    property OCI7: TOCI7API read GetOCI7;
    property OCI8: TOCI8API read GetOCI8;
  public
    constructor Create; override;
    destructor Destroy; override;

  { Open/Close }
    function IsFullReopen: boolean; override;
    procedure Reopen; override;
    procedure SetCommandType;
    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override; // Execute command
    procedure Disconnect; override;

  { Fetch }
    procedure ExecFetch(DisableInitFields: boolean); override;
    procedure FetchAll; override;
    procedure WaitForFetch; override;
    function RowsReturn: boolean; override;

  { Fields }
    class function GetBufferSize(DataType: Word; LengthInChars: integer): integer; override;
    procedure DetectIdentityField; override;
    function GetFieldDescType: TFieldDescClass; override;
    procedure DetectFieldType(const FieldName: string; DBType: Word; DBLength: Integer; DBScale: SmallInt;
      ObjectType: TObjectType; CharUsed: Boolean; out DataType, SubDataType: Word; out Length, Scale: Integer; out Fixed: boolean);

    procedure SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean); override;
    function GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; override;

    class procedure GetDateFromBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat); override;
    class procedure PutDateToBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat); override;

    class function IsBlobDataType(DataType: word): boolean; override;
    class function IsObjectDataType(DataType: word): boolean; override;
    class function IsSharedObjectDataType(DataType: word): boolean; override;
    class function IsComplexDataType(DataType: word): boolean; override;
    class function IsConvertedFieldType(DataType: word): boolean;

    procedure GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean); override;
    procedure GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType: Word; SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean); override;
    procedure PutDataAsVariant(DataBuf: IntPtr; DataLenPtr: PWord; DataType: Word; Length, Scale: Word; HasParent: boolean; const Value: variant; IsDatabaseValue: boolean); override;

    function FieldListDependsOnParams: boolean; override;

  {$IFNDEF LITE}
  { Encryption}
    function GetDecryptDataType(DataType: Word): Word; override;
    function IsEncryptableDataType(DataType: Word): boolean; override;
  {$ENDIF}

  { Records }
    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc); override;

    procedure SortItems; override;
    procedure FilterUpdated; override;

  { Navigation }
    procedure SetToBegin; override;
    procedure SetToEnd; override;

  { BookMarks }
    procedure GetBookmark(Bookmark: PRecBookmark); override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;
    function GetBlockFetchPos(Block: PBlockHeader): integer;
    function GetItemFetchPos(Item: PItemHeader): integer;

  { Blobs }
    procedure SetConnection(Value: TCRConnection); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    property ExpandedFields: TFieldDescs read FExpandedFields;
  end;

{ TOraLob }

  TLobType = (ltBlob, ltClob, ltNClob);

  TOraLob = class (TCompressedBlob)
  private
    FOCISvcCtx: TOCISvcCtx;
    phLobLocator: ppOCILobLocator;
    FNativeHandle: boolean;
    FCached: boolean;
    FCharsetId: integer;
    FCharsetForm: integer;
    FLobType: TLobType;
    FCharLength: byte;

    function GetOCILobLocator: pOCILobLocator;
    function GethOCILobLocator: pOCIStmt;
    procedure SethOCILobLocator(Value: pOCIStmt);
    procedure SetOCILobLocator(Value: pOCILobLocator);
    function GetOCILobLocatorPtr: ppOCILobLocator;
    procedure SetCached(const Value: boolean);
    procedure SetOCISvcCtx(const Value: TOCISvcCtx);
    function GetOCI8: TOCI8API;
  protected
    FNeedReadLob: boolean;

    function CreateClone: TBlob; override;

    procedure Check(Status: sword);
    procedure CheckValue; override;
    function GetSize: Cardinal; override;
    function GetSizeAnsi: Cardinal; override;
    function GetSizeUni: Cardinal; override;
    procedure CheckAlloc;
    procedure CheckSession;
    procedure CheckInit;
    procedure CheckCharSetForm;
    function CharSize: Byte; virtual;
    procedure ReadLob(var SharedPiece: PPieceHeader; PrefetchLobSize: Integer); overload;

    property OCI8: TOCI8API read GetOCI8;
    property hLobLocator: pOCILobLocator read GethOCILobLocator write SethOCILobLocator;
  public
    constructor Create(AOCISvcCtx: TOCISvcCtx);
    destructor Destroy; override;

    procedure AllocLob; virtual;
    procedure FreeBlob; override;
    procedure FreeLob;
    procedure Disconnect; override;

    procedure Init;
    procedure CreateTemporary(LobType: TLobType);
    procedure FreeTemporary;
    function IsTemporary: LongBool;
    function IsInit: boolean;

    function LengthLob: Cardinal;

    procedure EnableBuffering;
    procedure DisableBuffering;

    procedure ReadLob; overload;
    procedure WriteLob;

    function Read(Position, Count: cardinal; Dest: IntPtr): cardinal; override;
    procedure Write(Position, Count: cardinal; Source: IntPtr); override;
    procedure Clear; override;
    procedure Truncate(NewSize: cardinal); override;

    property OCISvcCtx: TOCISvcCtx read FOCISvcCtx write SetOCISvcCtx;

    property OCILobLocator: pOCILobLocator read GetOCILobLocator write SetOCILobLocator;
    property OCILobLocatorPtr: ppOCILobLocator read GetOCILobLocatorPtr;
    property Cached: boolean read FCached write SetCached;
    property LobType: TLobType read FLobType write FLobType;
  end;

{ TOraFile }

  TOraFile = class (TOraLob)
  private
    FNeedRollback: boolean;
    FRollbackFileDir: string;
    FRollbackFileName: string;

    function GetFileDir: string;
    procedure SetFileDir(const Value: string);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetFileDirAndName(const FileDir, FileName: string);

  protected
    CanRollback: boolean;

    procedure CheckValue; override;
    function CharSize: Byte; override;

    procedure SaveToRollback; override;
  public
    destructor Destroy; override;

    procedure AllocLob; override;
    procedure FreeBlob; override;

    procedure Open;
    procedure Close;
    procedure EnableRollback;

    procedure Commit; override;
    procedure Cancel; override;

    procedure Refresh;

    function IsOpen: boolean;

    function Exists: boolean;

    property FileDir: string read GetFileDir write SetFileDir;
    property FileName: string read GetFileName write SetFileName;
  end;

  THandleType = (htLocal, htNative, htShared);

{ TOraTimeStamp }

  TOraTimeStamp = class (TSharedObject)
  private
    FEnvironment: TOCIEnvironment;
    FOCIDateTimePtr: ppOCIDateTime;
    FLocalDateTime: TOCIDateTime;
    FIndicatorPtr: pOCIInd;
    FIndicator: OCIInd;
    FDescriptorType: Cardinal;
    FPrecision: byte;
    FFormat: string;
    FHandleType: THandleType;

    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetTimeZone: string;
    function GethOCIDateTime: pOCIDateTime;
    procedure SethOCIDateTime(Value: pOCIDateTime);
    function GetOCIDateTime: pOCIDateTime;
    procedure SetOCIDateTime(const Value: pOCIDateTime);
    procedure SetDescriptorType(const Value: cardinal);
    function GetOCIDateTimePtr: ppOCIDateTime;
  {$IFNDEF FPC}
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
    function  GetAsSQLTimeStamp: TSQLTimeStamp;
  {$ENDIF}
    function GetLocalDateTime: TOCIDateTime;
    procedure CheckValid;
  protected
    procedure Check(Res: sword);

    procedure Init(ADataType: word);
    procedure AllocDateTime;
    procedure FreeDateTime;
    function ToLocalDateTime: TOCIDateTime;
    procedure FromLocalDateTime(Value: TOCIDateTime);

    procedure SetFormat(const AFormat: string);

    property hOCIDateTime: pOCIDateTime read GethOCIDateTime write SethOCIDateTime;
    property LocalDateTime: TOCIDateTime read GetLocalDateTime;
  public
    constructor Create(ADataType: word); overload;
    constructor Create(AOCIEnvironment: TOCIEnvironment; ADataType: word); overload;
    constructor Create(AOCISvcCtx: TOCISvcCtx; ADataType: word); overload;
    destructor Destroy; override;

    procedure SetEnvironment(AEnvironment: TOCIEnvironment);
    procedure Disconnect; override;

    procedure Assign(Source: TOraTimeStamp);
    procedure AssignTo(Dest: TOraTimeStamp);
    function Compare(Dest: TOraTimeStamp): integer;

    function GetIsNull: boolean;
    procedure SetIsNull(Value: boolean);

    procedure Construct(Year: smallint; Month, Day, Hour, Min, Sec: byte;
      FSec: cardinal; TimeZone: string);

    procedure GetDate(var Year: smallint; var Month, Day: byte);
    procedure SetDate(Year: smallint; Month, Day: byte);

    procedure GetTime(var Hour, Min, Sec: byte; var FSec: cardinal);
    procedure SetTime(Hour, Min, Sec: byte; FSec: cardinal);

    procedure GetTimeZoneOffset(var TZHour, TZMin: shortint);
    procedure SetTimeZoneOffset(TZHour, TZMin: shortint);

    property DescriptorType: cardinal read FDescriptorType write SetDescriptorType;
    property OCIDateTime: pOCIDateTime read GetOCIDateTime write SetOCIDateTime;
    property OCIDateTimePtr: ppOCIDateTime read GetOCIDateTimePtr;
    property IndicatorPtr: pOCIInd read FIndicatorPtr;
    property Format: string read FFormat write SetFormat;
    property Precision: byte read FPrecision write FPrecision;
    property TimeZone: string read GetTimeZone;
    property AsString: string read GetAsString write SetAsString;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  {$IFNDEF FPC}
    property AsTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
  {$ENDIF}
    property IsNull: boolean read GetIsNull write SetIsNull;
  end;

  TOraInterval = class (TSharedObject)
  private
    FEnvironment: TOCIEnvironment;
    FOCIIntervalPtr: ppOCIInterval;
    FLocalInterval: TOCIInterval;
    FIndicatorPtr: pOCIInd;
    FIndicator: OCIInd;
    FDescriptorType: cardinal;
    FHandleType: THandleType;
    FFracPrecision: byte;
    FLeadPrecision: byte;

    procedure InitInterval;
    procedure CheckValid;
    function GetAsString: string;
    function GethOCIInterval: pOCIInterval;
    procedure SethOCIInterval(Value: pOCIInterval);
    function GetOCIInterval: pOCIInterval;
    function GetOCIIntervalPtr: ppOCIInterval;
    procedure SetAsString(const Value: string);
    procedure SetDescriptorType(const Value: cardinal);
    procedure SetOCIInterval(const Value: pOCIInterval);
    function GetLocalInterval: TOCIInterval;
  protected
    procedure Check(Res: sword);

    procedure Init(ADataType: word);
    procedure AllocInterval;
    procedure FreeInterval;
    function ToLocalInterval: TOCIInterval;
    procedure FromLocalInterval(Value: TOCIInterval);

    property hOCIInterval: pOCIInterval read GethOCIInterval write SethOCIInterval;
    property LocalInterval: TOCIInterval read GetLocalInterval;
  public
    constructor Create(ADataType: word); overload;
    constructor Create(AOCIEnvironment: TOCIEnvironment; ADataType: word); overload;
    constructor Create(AOCISvcCtx: TOCISvcCtx; ADataType: word); overload;
    destructor Destroy; override;

    procedure SetEnvironment(AEnvironment: TOCIEnvironment);
    procedure Disconnect; override;

    procedure Assign(Source: TOraInterval);
    procedure AssignTo(Dest: TOraInterval);
    function Compare(Dest: TOraInterval): integer;

    function GetIsNull: boolean;
    procedure SetIsNull(Value: boolean);

    procedure GetYearMonth(var Year, Month: integer);
    procedure SetYearMonth(Year, Month: integer);

    procedure GetDaySecond(var Day, Hour, Min, Sec, FSec: integer);
    procedure SetDaySecond(Day, Hour, Min, Sec, FSec: integer);

    property DescriptorType: cardinal read FDescriptorType write SetDescriptorType;
    property OCIInterval: pOCIInterval read GetOCIInterval write SetOCIInterval;
    property OCIIntervalPtr: ppOCIInterval read GetOCIIntervalPtr;
    property IndicatorPtr: pOCIInd read FIndicatorPtr;
    property LeadPrecision: byte read FLeadPrecision write FLeadPrecision;
    property FracPrecision: byte read FFracPrecision write FFracPrecision;
    property AsString: string read GetAsString write SetAsString;
    property IsNull: boolean read GetIsNull write SetIsNull;
  end;

  TOraNumber = class (TSharedObject)
  private
    FEnvironment: TOCIEnvironment;
    FOCINumberPtr: pOCINumber;
    FIndicatorPtr: pOCIInd;
    FIndicator: OCIInd;
    FHandleType: THandleType;

    procedure SetOCINumberPtr(Value: pOCINumber);
    function GetOCINumber: OCINumber;
    procedure SetOCINumber(Value: OCINumber);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsInteger: integer;
    procedure SetAsInteger(Value: integer);
    function GetAsLargeInt: int64;
    procedure SetAsLargeInt(Value: int64);
    function GetAsFloat: double;
    procedure SetAsFloat(Value: double);
    function GetIsNull: boolean;
    procedure SetIsNull(Value: boolean);
    function GetAsBCD: TBCD;
    procedure SetAsBCD(const Value: TBCD);
  protected
    procedure Init;
    procedure Check(Res: sword);
  public
    constructor Create; overload;
    constructor Create(AOCIEnvironment: TOCIEnvironment); overload;
    constructor Create(AOCISvcCtx: TOCISvcCtx); overload;
    destructor Destroy; override;

    procedure SetEnvironment(AEnvironment: TOCIEnvironment);
    procedure Disconnect; override;

    procedure Assign(Source: TOraNumber);
    procedure AssignTo(Dest: TOraNumber);
    function Compare(Dest: TOraNumber): integer;

    property OCINumber: OCINumber read GetOCINumber write SetOCINumber;
    property OCINumberPtr: pOCINumber read FOCINumberPtr write SetOCINumberPtr;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsLargeInt: Int64 read GetAsLargeInt write SetAsLargeInt;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property IsNull: boolean read GetIsNull write SetIsNull;
    property AsBCD: TBCD read GetAsBCD write SetAsBCD;
  end;

{ TOCITransaction }

  TOraTransactionState = (tsInactive, tsActive, tsPrepared, tsFinished);

{$IFNDEF LITE}
  TTransactionLink = record
    BranchQualifier: TBytes;
    State: TOraTransactionState;
    OCITrans: pOCITrans;
  end;
{$ENDIF}

  TOCITransaction = class (TCRTransaction)
  private
    FLocalTransactionId: string;
  {$IFNDEF LITE}
    FTransactionName: string;
    FRollbackSegment: string;
    FTransactionLinks: array of TTransactionLink;
    FInactiveTimeOut: integer;
    FResumeTimeOut: integer;
    FXID: IntPtr;
    FTransactionId: TBytes;
    FResume: boolean;
  {$ENDIF}

  protected
  {$IFNDEF LITE}
    procedure WriteTransactionId;
    procedure WriteBranchQualifier(TransactionLink: TTransactionLink);
    procedure FreeTransaction;
  {$ENDIF}

    function LocalTransactionId(CreateTransaction: boolean = False): string;
    procedure StartTransactionLocal;
    procedure CommitLocal;
    procedure RollbackLocal;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Check(Status: sword; Connection: TOCIConnection);
    procedure OraError(OCISvcCtx: TOCISvcCtx; var ErrorCode: sword; UseCallback: boolean);

  {$IFNDEF LITE}
    procedure SetTransactionId(TransactionId: TBytes);
    procedure SetBranchQualifier(Index: integer; BranchQualifier: TBytes);
  {$ENDIF}

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    function DetectInTransaction(CanActivate: boolean): boolean; override;
    procedure AssignConnect(Source: TCRTransaction); override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure Savepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;
  {$IFNDEF LITE}
    procedure Detach;
  {$ENDIF}
  end;

{$IFNDEF LITE}

{ TOCIMetaData }

  TOCIMetaData = class (TCRMetaData)
  protected
    procedure AddOrderBy(var OrderByClause: string; const Value: string);

    function GetTypesForSQL(const ObjectTypes: string; AllTypes: array of string): string;
    function CreateRecordSet: TCRRecordSet; override;

    function InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData; override;
    procedure InternalGetMetaDataKindsList(List: TStringList); override;
    procedure InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string); override;
    function GetTables(Restrictions: TStrings): TData; override;
    function GetColumns(Restrictions: TStrings): TData; override;
    function GetProcedures(Restrictions: TStrings): TData; override;
    function GetProcedureParameters(Restrictions: TStrings): TData; override;
    function GetIndexes(Restrictions: TStrings): TData; override;
    function GetIndexColumns(Restrictions: TStrings): TData; override;
    function GetConstraints(Restrictions: TStrings): TData; override;
    function GetConstraintColumns(Restrictions: TStrings): TData; override;

//    function GetDataTypes(Restrictions: TStrings): TData; override;
    function GetUsers(Restrictions: TStrings): TData; override;
    function GetUDTs(Restrictions: TStrings): TData; override;
    function GetPackages(Restrictions: TStrings): TData; override;
    function GetSequences(Restrictions: TStrings): TData;
    function GetSynonyms(Restrictions: TStrings): TData;
  end;

{ TOCILoader }

  TOCILoaderColumn = class (TCRLoaderColumn)
  private
    FColumnType: integer;
    FDateFormat: string;
    FIsNational: boolean;
    FIsLob: boolean;
    FOffset: integer;
  protected
    procedure VerifySize;
  public
    constructor Create; override;

    procedure UpdateDataType(Value: word); override;

    property ColumnType: integer read FColumnType write FColumnType;
    property DateFormat: string read FDateFormat write FDateFormat;
    property IsNational: boolean read FIsNational write FIsNational;
    property IsLob: boolean read FIsLob write FIsLob;
    property Offset: integer read FOffset write FOffset;
  end;

  _TDPErrorAction = (_dpAbort, _dpFail, _dpIgnore);
  _TDPErrorEvent = procedure(E: Exception; Col, Row: integer; var Action: _TDPErrorAction) of object;

  TOCILoader = class(TCRLoader)
  private
    FConnection: TOCIConnection;
    FIsDirectMode: boolean;
    FOnError: _TDPErrorEvent;
    FDML: TOCICommand;

    hDirPathCtx: pOCIDirPathCtx;
    hColumnArray: pOCIDirPathColArray;
    hStream: pOCIDirPathStream;

    FBufNumRows: integer;  // count of Rows in ColumnArray
    FBufNumCols: word;     // count of Cols in ColumnArray
    FRowSize: word;      // size of Row in buffer
    FBuffer: IntPtr;
    FLobBuffer: TList;

    function GetOCISvcCtx: TOCISvcCtx;
    function GetOCI7: TOCI7API;
    function GetOCI8: TOCI8API;
    procedure Check(Status: Word);
  protected
    procedure SetConnection(Value: TCRConnection); override;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); override;
    function ConverToUnicode(Column: TOCILoaderColumn): boolean;
    procedure CheckLobBuffer;
    procedure ClearLobBuffer;
    procedure FreeLobBuffer;

    property OCISvcCtx: TOCISvcCtx read GetOCISvcCtx;
    property OCI7: TOCI7API read GetOCI7;
    property OCI8: TOCI8API read GetOCI8;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    class function GetColumnClass: TCRLoaderColumnClass; override;
    procedure Prepare; override;
    procedure Reset; override;
    procedure PutColumnData(Col, Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;

    property OnError: _TDPErrorEvent read FOnError write FOnError;
  end;

{ TOCIAlerter }

  _TEventType = (_etAlert, _etPipe);
  TMessageType = (mtNone, mtNumber, mtString, mtDate);

  TAlertEvent = class
  public
    Name: string;
    Message: string;
  end;

  TOCIAlerterTimeoutCallback = procedure(var Continue: boolean) of object;

  TOCIAlerterListenThread = class;

  TOCIAlerter = class(TCRAlerter)
  private
    FGCHandle: IntPtr;
    FListenConnection: TOCIConnection;
    FWaitCommand: TOCICommand;
    FListenThread: TOCIAlerterListenThread;
    FTimeOut: integer;
    FOnTimeOut: TOCIAlerterTimeoutCallback;
    FInterval: integer;
    FEventType: _TEventType;
    FSelfMessage: string;
    FRegistered: boolean;
    FAutoCommit: boolean;
    FSelfEvents: boolean;
  {$IFDEF MSWINDOWS}
    FResponseEvent: TEvent;
    FIntervalEvent: TEvent;
  {$ELSE}
    FLastEvent: TAlertEvent;
  {$ENDIF}

    function GetGCHandle: IntPtr;
    function GetOCISvcCtx: TOCISvcCtx;
    procedure RegisterEvents;
    procedure RemoveEvents;
    procedure SendAlertMessage(const EventName, Message: string);
    function ProcessWaitResult: boolean;
    function ProcessMessage: boolean;
    function ProcessError(Status: integer): boolean;
  {$IFDEF MSWINDOWS}
    procedure DoOnEvent(const EnentName, Message: string);
  {$ELSE}
    procedure DoOnEvent;
    procedure CallEvent(Event: TAlertEvent);
  {$ENDIF}
    procedure DoOnTimeout(var Continue: boolean);
  {$IFDEF MSWINDOWS}
    procedure DoOnError(E: Exception);
  {$ENDIF}

  protected
    property GCHandle: IntPtr read GetGCHandle;
    property OCISvcCtx: TOCISvcCtx read GetOCISvcCtx;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SendEvent(const EventName, Message: string); override;
    procedure Start; override;
    procedure Stop; override;

    // pipe methods
    procedure PackMessage(const Item: variant);
    function UnpackMessage(var Item: variant): boolean;
    function NextItemType: TMessageType;
    procedure SendPipeMessage(const Name: string);
    procedure PurgePipe;

    property OnTimeOut: TOCIAlerterTimeoutCallback read FOnTimeOut write FOnTimeOut;
  end;

  TOCIAlerterListenThread = class(TThread)
  private
    FAlerter: TOCIAlerter;

  protected
    procedure Execute; override;

  public
    constructor Create(Alerter: TOCIAlerter);
    procedure Terminate;
  end;

{$ENDIF}

{ TOraClassesUtils }

  TOraClassesUtils = class
  public
    class procedure InternalUnPrepare(Obj: TOCIRecordSet);
  end;

const
  DefValConnectMode = cmNormal;

var
  OCISQLInfo: TOCISQLInfo;

  SmallintPrecision: integer = 0;
  IntegerPrecision: integer = 9;
  LargeIntPrecision: integer = 0;
  FloatPrecision: integer   = 15;
  BCDPrecision: string      = '14,4';
  FmtBCDPrecision: string   = '39,39';
  EnableWideOraClob: boolean = True;
  RemoveCRInStringLiterals: boolean = False;
  UseMaxDataSize: boolean = True;
  NumberAsInteger: boolean = False;
  UseOCI7ProcDesc: boolean = False;
  ForceProcNamedParams: boolean = False;
  UniqueEnvironments: boolean = False;

  function OraDateToDateTime(Buf: IntPtr): TDateTime;
  function OraDateToTimeStamp(Buf: IntPtr): TTimeStamp;
  function OraDateToMSecs(Buf: IntPtr): double;
  procedure DateTimeToOraDate(DateTime: TDateTime; Buf: IntPtr);
  procedure MSecsToOraDate(MSecs: double; Buf: IntPtr);
  procedure OraTimeStampToSQLTimeStamp(OraTimeStamp: TOraTimeStamp; var SQLTimeStamp: TSQLTimeStamp);
  procedure SQLTimeStampToOraTimeStamp(OraTimeStamp: TOraTimeStamp; const SQLTimeStamp: TSQLTimeStamp);
  procedure OraTimeStampToSQLTimeStampOffset(OraTimeStamp: TOraTimeStamp; var SQLTimeStampOffset: TSQLTimeStampOffset);
  procedure SQLTimeStampOffsetToOraTimeStamp(OraTimeStamp: TOraTimeStamp; const SQLTimeStampOffset: TSQLTimeStampOffset);

  procedure ShareOraTimeStamp(OraTimeStamp: TOraTimeStamp);
  procedure ShareOraInterval(OraInterval: TOraInterval);

  procedure ParseConnectString(const ConnectString: string;
    var Username, Password, Server: string; var ConnectMode: TConnectMode);

{$IFDEF MSWINDOWS}
  procedure AllocODACWnd;
{$ENDIF}

implementation


uses
  Math, RTLConsts, Variants,
{$IFDEF PROF}
  OraProf,
{$ENDIF}
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  ComObj,
{$ENDIF}
{$ENDIF}
{$IFDEF NET}
  {$IFNDEF UNIDACPRO}OraNet,{$ELSE}OraNetUni,{$ENDIF}
{$ENDIF}
  CRProps, CRFunctions, MemUtils, DAConsts,
{$IFNDEF UNIDACPRO}
  OraProps, OraConsts, OraDataTypeMap, OraObjects, OraParser;
{$ELSE}
  OraPropsUni, OraConstsUni, OraDataTypeMapUni, OraObjectsUni, OraParserUni;
{$ENDIF}

const
  WM_ENDTHREAD       = $400;
  WM_AFTERFETCH      = $404;
  WM_CHANGENOTIFY    = $405;
  WM_ALERTER_EVENT   = $406;
  WM_ALERTER_TIMEOUT = $407;
  WM_ALERTER_STOPED  = $408;

  msTerminate = '__ODAC_Terminate_Event__';
  msBlank = '__ODAC_Blank_Message__';

type
  TArr = array [0..100] of byte;  // DEBUG TEMP
  PArr = ^TArr;
  TArrC = array [0..100] of char;  // DEBUG TEMP
  PArrC = ^TArrC;


{ TTimeoutThread }

  TTimeoutThread = class (TThread)
  private
    FConnection: TOCIConnection;
    FTimeout: Integer;

    FLock: TCriticalSection;
    FPauseEvent: TEvent;
    FEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(Connection: TOCIConnection);
    destructor Destroy; override;

    procedure Run(Timeout: Integer);
    procedure Stop;
    procedure Release;
  end;

{$IFDEF MSWINDOWS}

{ TExecThread }

  TExecThread =  class(TThread)
  private
    FRunMethod : TRunMethod;
    FEndMethod : TEndMethod;
    FhWindow :HWND;
    FException: Exception;
    FGCHandle: IntPtr;
  protected
    procedure Execute; override;
  public
    constructor Create(RunMethod: TRunMethod; EndMethod: TEndMethod; hODACWindow: HWND; CreateSuspended: Boolean);
    destructor Destroy; override;
  end;

{$ENDIF}

{ TConnectionList }

  TConnectionList = class (TThreadList)
  private
    function GetCount: integer;
    function GetConnection(i: integer): TOCIConnection;
  public
    procedure DisconnectAll;

    property Count: integer read GetCount;
    property Items[i: integer]: TOCIConnection read GetConnection; default;
  end;

var
  OCIConnections: TConnectionList;
{$IFNDEF MSWINDOWS}
  hLockConnect: TCriticalSection;
{$ENDIF}
{$IFDEF MSWINDOWS}
  hODACWindow: HWND = 0;
{$ENDIF}

  OCICallbackDefinePtr: IntPtr;
  OCICallbackInBindPtr: IntPtr;
  OCICallbackOutBindPtr: IntPtr;
  OCICallbackFailoverPtr: IntPtr;
{$IFDEF MSWINDOWS}
  OCICallbackChangeNotifyPtr: IntPtr;
{$ENDIF}

function Shift(Value: cardinal): cardinal;
begin
  Result := Value;
  if Result <> 0 then
    if (Result and $FF) = 0 then  // while do
      Result := Result shr 8;
end;

function Reverse2(Value: word): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := byte(Value shr 8);
  Result[1] := byte(Value);
end;

function Reverse4(Value: cardinal): TBytes;
begin
  SetLength(Result, 4);
  Result[0] := byte(Value shr 24);
  Result[1] := byte(Value shr 16);
  Result[2] := byte(Value shr 8);
  Result[3] := byte(Value);
end;

// Converts Count bytes from memory pointed by Bytes to 64 base string. Starting
// digit (6-bit chunk) may be shifted by -4, -2, 0 or 2 bits. Missing bits
// assumed to be zero.
// Bytes are converted in the following way (example for Shift = 0):
// 0 byte   1 byte   hi    lo
// 00000100|00000000|01000001|01000011|...
// ------++ ++++---- --++++++ ------++ +++
//  B(1)   A(0)   B(1)  B(1)   Q(16)
function BytesTo64BaseString(Bytes: TBytes; Count: integer; Shift: integer): string;
const
  Map = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i, RestBits: cardinal;
  CurByte, Digit, NextDigit: byte;
begin
  Result :='';
  RestBits := 2 - Shift;
  NextDigit := $FF;

  for i := 0 to Count - 1 do begin
    CurByte := Bytes[i];

    Digit := CurByte shr RestBits;
    if NextDigit <> $FF then
      Digit := Digit or NextDigit;

    Result := Result + Map[Digit + 1];

    NextDigit := (CurByte and ($FF shr (8 - RestBits))) shl (6 - RestBits);

    if RestBits = 6 then begin
      Result := Result + Map[NextDigit + 1];
      NextDigit := $FF;
      RestBits := 2;
    end
    else
      RestBits := RestBits + 2;
  end;

  if NextDigit <> $FF then
    Result := Result + Map[NextDigit + 1];
end;

function RowId7ToString(RowId: PRowId7): string;
var
  Buf: TBytes;
begin
  Buf := nil;
  if (RowId.rd.rcs4 = 0) then begin // obj num
  // restricted (Oracle 7)
    Result :=
      IntToHex(Shift(RowId.rcs7), 8) + '.' +
      IntToHex(Shift(RowId.rcs8), 4) + '.' +  // use 2 byte
      IntToHex(RowId.rd.rcs5, 4)
  end
  else begin
  // extended (Oracle 8 and higher)
    Buf := Reverse4(RowId.rd.rcs4);
    Result := BytesTo64BaseString(Buf, 4, -4);

    Buf := Reverse2(RowId.rd.rcs5);
    Result := Result + BytesTo64BaseString(Buf, 2, -2);

    Buf := Reverse4(Shift(RowId.rcs7));
    Result := Result + BytesTo64BaseString(Buf, 4, -4);

    Buf := Reverse2(Shift(RowId.rcs8)); // use 3 byte
    Result := Result + BytesTo64BaseString(Buf, 2, -2);
  end;
end;

function RowId8ToString(RowId: PRowId8): string;
var
  Buf: TBytes;
begin
  Buf := nil;
  if (RowId.ridobjnum = 0) then
  // restricted (Oracle 7)
    Result :=
      IntToHex(Shift(RowId.ridblocknum), 8) + '.' +
      IntToHex(Shift(RowId.ridslotnum), 4) + '.' +
      IntToHex(RowId.ridfilenum, 4)
  else begin
  // extended (Oracle 8 and higher)
    Buf := Reverse4(RowId.ridobjnum);
    Result := BytesTo64BaseString(Buf, 4, -4);

    Buf := Reverse2(RowId.ridfilenum);
    Result := Result + BytesTo64BaseString(Buf, 2, -2);

    Buf := Reverse4(RowId.ridblocknum);
    Result := Result + BytesTo64BaseString(Buf, 4, -4);

    Buf := Reverse2(RowId.ridslotnum);
    Result := Result + BytesTo64BaseString(Buf, 2, -2);
  end;
end;

function RowId81ToString(RowIdPtr: PRowId81): string;
var
  Bytes: TBytes;
begin
  if RowIdPtr.ridobjnum = 0 then begin
  // restricted (Oracle 7)
    Result :=
      IntToHex(BitConverter.ToInt32(Reverse4(RowIdPtr.ridblocknum), 0), 8) + '.' +
      IntToHex(BitConverter.ToInt16(Reverse2(RowIdPtr.ridslotnum), 0), 4) + '.' +
      IntToHex(BitConverter.ToInt16(Reverse2(RowIdPtr.ridfilenum), 0), 4)
  end
  else begin
  // extended (Oracle 8 and higher)
    SetLength(Bytes, 4);
    Marshal.Copy(PtrOffset(RowIdPtr, 1{TRowId81.ridobjnum}), Bytes, 0, 4);
    Result := BytesTo64BaseString(Bytes, 4, -4);
    Marshal.Copy(PtrOffset(RowIdPtr, 5{TRowId81.ridfilenum}), Bytes, 0, 2);
    Result := Result + BytesTo64BaseString(Bytes, 2, -2);
    Marshal.Copy(PtrOffset(RowIdPtr, 7{TRowId81.ridblocknum}), Bytes, 0, 4);
    Result := Result + BytesTo64BaseString(Bytes, 4, -4);
    Marshal.Copy(PtrOffset(RowIdPtr, 11{TRowId81.ridslotnum}), Bytes, 0, 2);
    Result := Result + BytesTo64BaseString(Bytes, 2, -2);
  end;
end;

function URowIdToString(RowIdPtr: PRowId81; Length: integer): string;
var
  Bytes: TBytes;
begin
  SetLength(Bytes, Length - 1);
  Marshal.Copy(PtrOffset(RowIdPtr, 1), Bytes, 0, Length - 1);
  Result := '*' + BytesTo64BaseString(Bytes, Length - 1, 0);
end;

{ Data convertion }

function OraDateToDateTime(Buf: IntPtr): TDateTime;
begin
  Result := TimeStampToDateTime(OraDateToTimeStamp(Buf));
end;

function OraDateToTimeStamp(Buf: IntPtr): TTimeStamp;
const
{$IFNDEF VER7P}
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
{$ENDIF}
  MonthDays: array [Boolean, 1..12] of Word =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
  TotalMonthDays: array [Boolean, 1..12] of Word =
    ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
     (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));
var
  I: Integer;
  Year: Word;
  Month: Word;
  Day: Word;
  Hour: Word;
  Min: Word;
  Sec: Word;
  LeapYear: boolean;
begin
  Year := Abs((PByteArray(Buf)^[0] - 100) * 100 + PByteArray(Buf)^[1] - 100);
  Month := PByteArray(Buf)^[2];
  Day := PByteArray(Buf)^[3];
  Hour := PByteArray(Buf)^[4] - 1;
  Min := PByteArray(Buf)^[5] - 1;
  Sec := PByteArray(Buf)^[6] - 1;

  LeapYear := IsLeapYear(Year);
  if (Year >= 1)  and (Year <= 9999) and
     (Month >= 1) and (Month <= 12) and
     (Day >= 1)   and (Day <= MonthDays[LeapYear, Month])
  then begin
    Day := Day + TotalMonthDays[LeapYear, Month];
    I := Year - 1;
    Result.Date := I * 365 + I div 4 - I div 100 + I div 400 + Day;
  end
  else
    raise EConvertError.CreateRes(@SDateEncodeError);

  if (Hour < HoursPerDay) and
     (Min  < MinsPerHour) and
     (Sec  < SecsPerMin)
  then begin
    Result.Time := (Hour * (MinsPerHour * SecsPerMin * MSecsPerSec)) +
                   (Min * SecsPerMin * MSecsPerSec) +
                   (Sec * MSecsPerSec);
  end
  else
    raise EConvertError.CreateRes(@STimeEncodeError);
end;

function OraDateToMSecs(Buf: IntPtr): double;
begin
  Result := TimeStampToMSecs(OraDateToTimeStamp(Buf));
end;

procedure DateTimeToOraDate(DateTime: TDateTime; Buf: IntPtr);
var
  Year: Word;
  Month: Word;
  Day: Word;
  Hour: Word;
  Min: Word;
  Sec: Word;
  MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Min, Sec, MSec);

  PByteArray(Buf)^[0] := Year div 100 + 100;
  PByteArray(Buf)^[1] := Year mod 100 + 100;
  PByteArray(Buf)^[2] := Month;
  PByteArray(Buf)^[3] := Day;
  PByteArray(Buf)^[4] := Hour + 1;
  PByteArray(Buf)^[5] := Min + 1;
  PByteArray(Buf)^[6] := Sec + 1;
end;

procedure MSecsToOraDate(MSecs: double; Buf: IntPtr);
begin
  DateTimeToOraDate(MemUtils.TimeStampToDateTime(MSecsToTimeStamp(Trunc(MSecs))), Buf);
end;

procedure OraTimeStampToSQLTimeStamp(OraTimeStamp: TOraTimeStamp; var SQLTimeStamp: TSQLTimeStamp);
var
  Year: SmallInt;
  Month, Day, Hour, Min, Sec: byte;
  FSec: cardinal;
begin
  if OraTimeStamp.IsNull then
    SQLTimeStamp := NullSqlTimeStamp
  else begin
    OraTimeStamp.GetDate(Year, Month, Day);
    SQLTimeStamp.Year := Year;
    SQLTimeStamp.Month := Month;
    SQLTimeStamp.Day := Day;

    OraTimeStamp.GetTime(Hour, Min, Sec, FSec);
    SQLTimeStamp.Hour := Hour;
    SQLTimeStamp.Minute := Min;
    SQLTimeStamp.Second := Sec;
    SQLTimeStamp.Fractions := FSec div 1000000;
  end;
end;

procedure SQLTimeStampToOraTimeStamp(OraTimeStamp: TOraTimeStamp; const SQLTimeStamp: TSQLTimeStamp);
begin
  if (SQLTimeStamp.Year = 0) and (SQLTimeStamp.Month = 0) and (SQLTimeStamp.Day = 0)
    and (SQLTimeStamp.Hour = 0) and (SQLTimeStamp.Minute = 0) and (SQLTimeStamp.Second = 0)
    and (SQLTimeStamp.Fractions = 0)
  then
    OraTimeStamp.IsNull := True
  else
    OraTimeStamp.Construct(
      SQLTimeStamp.Year, SQLTimeStamp.Month, SQLTimeStamp.Day,
      SQLTimeStamp.Hour, SQLTimeStamp.Minute, SQLTimeStamp.Second,
      SQLTimeStamp.Fractions * 1000000,
      '');
end;

procedure OraTimeStampToSQLTimeStampOffset(OraTimeStamp: TOraTimeStamp; var SQLTimeStampOffset: TSQLTimeStampOffset);
var
  Year: SmallInt;
  Month, Day, Hour, Min, Sec: byte;
  FSec: cardinal;
  TZHour, TZMin: shortint;
begin
  if OraTimeStamp.IsNull then
    SQLTimeStampOffset := NullSQLTimeStampOffset
  else begin
    OraTimeStamp.GetDate(Year, Month, Day);
    SQLTimeStampOffset.Year := Year;
    SQLTimeStampOffset.Month := Month;
    SQLTimeStampOffset.Day := Day;

    OraTimeStamp.GetTime(Hour, Min, Sec, FSec);
    SQLTimeStampOffset.Hour := Hour;
    SQLTimeStampOffset.Minute := Min;
    SQLTimeStampOffset.Second := Sec;
    SQLTimeStampOffset.Fractions := FSec div 1000000;

    OraTimeStamp.GetTimeZoneOffset(TZHour, TZMin);
    SQLTimeStampOffset.TimeZoneHour := TZHour;
    SQLTimeStampOffset.TimeZoneMinute := TZMin;
  end;
end;

procedure SQLTimeStampOffsetToOraTimeStamp(OraTimeStamp: TOraTimeStamp; const SQLTimeStampOffset: TSQLTimeStampOffset);
var
  tz: string;
begin
  if (SQLTimeStampOffset.Year = 0) and (SQLTimeStampOffset.Month = 0) and (SQLTimeStampOffset.Day = 0)
    and (SQLTimeStampOffset.Hour = 0) and (SQLTimeStampOffset.Minute = 0) and (SQLTimeStampOffset.Second = 0)
    and (SQLTimeStampOffset.Fractions = 0)
  then
    OraTimeStamp.IsNull := True
  else begin
    tz := IntToStr(SQLTimeStampOffset.TimeZoneMinute);
    if Length(tz) < 2 then
      tz := '0' + tz;
    if SQLTimeStampOffset.TimeZoneHour < 0 then
      tz := IntToStr(SQLTimeStampOffset.TimeZoneHour) + ':' + tz
    else
      tz := '+' + IntToStr(SQLTimeStampOffset.TimeZoneHour) + ':' + tz;
    OraTimeStamp.Construct(
      SQLTimeStampOffset.Year, SQLTimeStampOffset.Month, SQLTimeStampOffset.Day,
      SQLTimeStampOffset.Hour, SQLTimeStampOffset.Minute, SQLTimeStampOffset.Second,
      SQLTimeStampOffset.Fractions * 1000000,
      tz);
  end;
end;

procedure ShareOraTimeStamp(OraTimeStamp: TOraTimeStamp);
begin
  if OraTimeStamp.FHandleType = htNative then
    OraTimeStamp.FHandleType := htShared;
end;

procedure ShareOraInterval(OraInterval: TOraInterval);
begin
  if OraInterval.FHandleType = htNative then
    OraInterval.FHandleType := htShared;
end;

procedure GetPrecAndScale(const Value: string; out Precision, Scale: integer);
var
  P: integer;
begin
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

{ TConnectionList }

function TConnectionList.GetCount: integer;
var
  List: TList;
begin
  List := LockList;
  try
    Result := List.Count;
  finally
    UnlockList;
  end;
end;

function TConnectionList.GetConnection(i: integer): TOCIConnection;
var
  List: TList;
begin
  List := LockList;
  try
    Result := TOCIConnection(List[i]);
  finally
    UnlockList;
  end;
end;

procedure TConnectionList.DisconnectAll;
var
  i: integer;
begin
  LockList;
  try
    for i := 0 to Count - 1 do
    try
      Items[i].Disconnect;
    except
    end;
  finally
    UnlockList;
  end;
end;

{ TOCIConnection }

constructor TOCIConnection.Create;
begin
  inherited Create;

  FIPVersion := ivIPv4;
  FAutoCommit := True;
  FThreadSafety := True;
  FOCICallStyle := None;
  FEnableIntegers := True;
  FEnableWideOraClob := EnableWideOraClob;
  FCharLength := 0;
  FQueryCharLength := FCharLength = 0;
  FInternalName := '';

  FSmallintPrecision := SmallintPrecision;
  FIntegerPrecision := IntegerPrecision;
  FLargeIntPrecision := LargeIntPrecision;
  FFloatPrecision := FloatPrecision;
  GetPrecAndScale(BCDPrecision, FBCDPrecision, FBCDScale);
  GetPrecAndScale(FmtBCDPrecision, FFmtBCDPrecision, FFmtBCDScale);
  FUnicodeAsNational := OCIUnicodeAsNational;

  FNlsParams[nlsDateLanguage].Name := 'NLS_DATE_LANGUAGE';
  FNlsParams[nlsDateFormat].Name := 'NLS_DATE_FORMAT';
  FNlsParams[nlsTimeStampFormat].Name := 'NLS_TIMESTAMP_FORMAT';
  FNlsParams[nlsTimeStampTZFormat].Name := 'NLS_TIMESTAMP_TZ_FORMAT';
  FNlsParams[nlsNumericCharacters].Name := 'NLS_NUMERIC_CHARACTERS';

  FLock := TCriticalSection.Create;

  OCIConnections.Add(Self);
end;

destructor TOCIConnection.Destroy;
begin
  Disconnect;

  // calling Halt in OnCreate event call finalization before Destroy
  if OCIConnections <> nil then
    OCIConnections.Remove(Self);

  if FTimeoutThread <> nil then begin
    TTimeoutThread(FTimeoutThread).Release;
    FTimeoutThread := nil;
  end;

  FLock.Free;

  inherited;
end;

function TOCIConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TOCICommand;
end;

function TOCIConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TOCIRecordSet;
end;

function TOCIConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TOCITransaction;
end;

{$IFNDEF LITE}

function TOCIConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TOCILoader;
end;

function TOCIConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TOCIMetaData;
end;

class function TOCIConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TOCICommand.GetMapRulesClass;
end;

function TOCIConnection.CreateObject(DataType: Word; Transaction: TCRTransaction): TDBObject;
begin
  case DataType of
    dtObject:
      Result := TOraObject.Create(nil);
    dtReference:
      Result := TOraRef.Create(nil);
    dtArray:
      Result := TOraArray.Create(nil);
    dtTable:
      Result := TOraNestTable.Create(nil);
    dtXML:
      Result := TOraXML.Create(nil);
    dtAnyData:
      Result := TOraAnyData.Create(nil);
    else
      raise Exception.Create('Invalid object type');
  end;

  TOraObject(Result).OCISvcCtx := self.OCISvcCtx;
end;

{$ENDIF}

procedure TOCIConnection.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure TOCIConnection.CheckOCI;
begin
  if not ((FOCICallStyle = OCI73) or (FOCICallStyle = OCI80)) then
    RaiseError(SCheckOCI);
end;

procedure TOCIConnection.CheckOCI73;
begin
  if not (FOCICallStyle = OCI73) then
    RaiseError(SCheckOCI73);
end;

procedure TOCIConnection.CheckOCI80;
begin
  if not (FOCICallStyle = OCI80) then
    RaiseError(SCheckOCI80);
end;

procedure TOCIConnection.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    OraError(FOCICallStyle, Status, True, Component);
end;

procedure TOCIConnection.OraError(FOCICallStyle: TOCICallStyle;
  ErrorCode: sword; UseCallback: boolean; Component: TObject);
const
  MsgLen = 512;
var
  MsgBuf: IntPtr;
  Msg: string;
  Code: integer;
  Fail, NeedFreeError: boolean;
  Error: EOraError;
begin
  Code := 0;
  if FOCICallStyle = OCI73 then begin
    Code := ErrorCode;
    if Code = -9 then
      Code := -1;
    MsgBuf := Marshal.AllocHGlobal(MsgLen + 1);
    try
      OCI7.oerhms(LDA, Code, MsgBuf, MsgLen);
      Msg := string(Marshal.PtrToStringAnsi(MsgBuf));
    finally
      Marshal.FreeHGlobal(MsgBuf);
    end;
    Code := Abs(Code);
  end
  else if FOCICallStyle = OCI80 then
    Code := OCI8.GetOraError(ErrorCode, OCISvcCtx, Msg)
  else
    CheckOCI;

  FLastError := Code;

  NeedFreeError := True;
  Error := EOraError.Create(Code, Msg);
  try
  {$IFNDEF NODBACCESS}
    Error.Component := Component;
  {$ENDIF}

    if (FOCICallStyle = OCI80) and (ErrorCode = OCI_SUCCESS_WITH_INFO)
      and (Code <> 24344) // except compilation error
    then begin
      if Assigned(FOnInfoMessage) then
        FOnInfoMessage(Error);
      exit;
    end;

    Fail := True;
    if UseCallback then
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

function OCICallbackFailover(svchp: IntPtr; envhp: IntPtr; fo_ctx: IntPtr; fo_type: ub4; fo_event: ub4): sb4; cdecl;
var
  OCIConnection: TOCIConnection;
  Retry: boolean;
  i: TNlsParamType;
begin
  OCIConnection := TOCIConnection(GetGCHandleTarget(fo_ctx));
  Assert(OCIConnection <> nil);
  Result := 0;
  if fo_event = OCI_FO_END then begin
      // apply user NLS settings
    with OCIConnection do begin
      for i := Low(FNlsParams) to High(FNlsParams) do
        if FNlsParams[i].IsUserDefined then
          SetNlsParameter(FNlsParams[i].Name, FNlsParams[i].Value);
    end;
  end;

  if Assigned(OCIConnection.FOnFailover) then begin
    Retry := False;
    OCIConnection.FOnFailover(fo_event, fo_type, Retry);
    if Retry then
      Result := OCI_FO_RETRY;
  end;
end;

function TOCIConnection.AllocEnvironment: TOCIEnvironment;
begin
{$IFDEF NET}
  if FDirect then
    Result := OracleHomes.Direct.AllocEnvironment(FThreadSafety and UniqueEnvironments, FUnicodeEnvironment, 0)
  else
{$ENDIF}
    Result := OracleHomes.GetHome(FHomeName).AllocEnvironment(FThreadSafety and UniqueEnvironments and (FConnectionType <> ctOCIPooled), FUnicodeEnvironment, {$IFNDEF LITE}FSubscriptionPort{$ELSE}0{$ENDIF});
end;

procedure TOCIConnection.SetConnectionType(ConnectionType: TConnectionType);
begin
  Assert(not FConnected);
  FConnectionType := ConnectionType;
end;

procedure TOCIConnection.SetStatementCacheSize(Size: integer);
begin
  if FStatementCache and
     (OCI73 in OCISvcCtx.Home.PossibleOCICallStyles) and (FOCICallStyle = OCI80) and
     (OCISvcCtx.Home.OCIVersion > 9200)
  then begin
    CheckOCI80;
    Check(OCI8.OCIAttrSet2(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, Size, 4, OCI_ATTR_STMTCACHESIZE, OCISvcCtx.hOCIError));
  end; //TODO: else Raise Error Statemet caching supported sice version 9.2
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
procedure TOCIConnection.MTSCheck(Status: sword);
begin
  if Status <> ORAMTSERR_NOERROR then
    MTSError(Status, True);
end;

procedure TOCIConnection.MTSError(var ErrorCode: sword; UseCallback: boolean);
var
  Msg: string;
  Fail: boolean;
begin
  ErrorCode := OCI8.GetOraError(ErrorCode, FOCISvcCtx, Msg);

  try
    raise EOraError.Create(ErrorCode, Msg);
  except
    on E: EOraError do begin
      Fail := True;
      if UseCallback then
        DoError(E, Fail);
      if Fail then
        raise
      else
        Abort;
    end;
  end;
end;

procedure TOCIConnection.Enlist(Transaction: TMTSTransaction);
var
  Value: variant;
begin
  Transaction.GetProp(prTransactionReadOnly, Value);
  if Value then
    raise Exception.Create(SReadOnlyNotSupportedWithMTS);

  Transaction.GetProp(prIsolationLevel, Value);
  if TCRIsolationLevel(Value) <> ilReadCommitted then
    raise Exception.Create(SIsolationLevelNotSupportedWithMTS);

  Assert(hMTSSvcCtx = nil);

  if FConnectionType = ctMTSPooled then begin
    hMTSSvcCtx := OCISvcCtx.hOCISvcCtx;
    MTSCheck(MTS.OraMTSSvcEnlist(hMTSSvcCtx, OCISvcCtx.hOCIError, Transaction.MTSTransaction, ORAMTS_ENFLG_DEFAULT));
  end
  else begin
    MTSCheck(MTS.OraMTSEnlCtxGet(PAnsiChar(AnsiString(FUserName)), PAnsiChar(AnsiString(FPassword)),
      PAnsiChar(AnsiString(FServer)), OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, OCI_DEFAULT, hMTSSvcCtx));
    MTSCheck(MTS.OraMTSJoinTxn(hMTSSvcCtx, Transaction.MTSTransaction));
  end;
end;

procedure TOCIConnection.UnEnlist(Transaction: TMTSTransaction);
begin
  if hMTSSvcCtx = nil then
    exit;

  if FConnectionType = ctMTSPooled then
    MTSCheck(MTS.OraMTSSvcEnlist(hMTSSvcCtx, OCISvcCtx.hOCIError, nil, ORAMTS_ENFLG_DEFAULT))
  else begin
    MTSCheck(MTS.OraMTSJoinTxn(hMTSSvcCtx, nil));
    MTSCheck(MTS.OraMTSEnlCtxRel(hMTSSvcCtx));
  end;

  hMTSSvcCtx := nil;
end;
{$ENDIF}
{$ENDIF}

procedure TOCIConnection.InitCommandProp(Command: TCRCommand);
begin
  Command.SetProp(prAutoCommit, False); // stataments that are executed with this command don't need to be commited
end;

procedure TOCIConnection.SetupConnection;
begin
  FOracleVersionFull := '';
  FOracleVersionSt := '';
  FOracleVersion := 0;

  if (FOCICallStyle = None) or not (FOCICallStyle in OCISvcCtx.Home.PossibleOCICallStyles) then
    FOCICallStyle := OCISvcCtx.Home.OCICallStyle;

  if FProxyConnection <> nil then
    CheckOCI80;

  FOCICallStyleCommand := FOCICallStyle;

{$IFDEF NET}
{$IFNDEF LITE}
  FSSLOptions.Key := StringReplace(FSSLOptions.Key, '\', '/', [rfReplaceAll]);
  FSSLOptions.Cert := StringReplace(FSSLOptions.Cert, '\', '/', [rfReplaceAll]);
  FSSLOptions.CA := StringReplace(FSSLOptions.CA, '\', '/', [rfReplaceAll]);
{$ENDIF}
{$ENDIF}
end;

procedure TOCIConnection.InitSessionParameters;
begin
  // get session parameters if necessary
{$IFNDEF IS_UTF8}
  if (FCharset <> '') or (FCharLength = 0) then
{$ENDIF}
    GetSessionParameters;
end;

procedure TOCIConnection.Connect(const ConnectString: string);
var
  Credt: ub4;
  Mode: ub4;
  i: TNlsParamType;
  Failover: TOCIFoCbkStruct;
  Handle: IntPtr;
  Res, Size: Integer;
{$IFDEF NET}
  CharsetId: Integer; //ub2
  IPVersion: integer;
{$ENDIF}
  p: IntPtr;


  function GetOraConnectMode(MTS: boolean): integer;
  begin
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    if MTS then
      case FConnectMode of
        cmSYSOPER:
          Result := ORAMTS_CFLG_SYSOPRLOGN;
        cmSYSDBA:
          Result := ORAMTS_CFLG_SYSDBALOGN;
      else
        Result := OCI_DEFAULT;
      end
    else
  {$ENDIF}
  {$ENDIF}
      case FConnectMode of
        cmSYSOPER:
          Result := OCI_SYSOPER;
        cmSYSDBA:
          Result := OCI_SYSDBA;
        cmSysASM:
          Result := OCI_SYSASM;
        cmSysBackup:
          Result := OCI_SYSBKP;
        cmSysDG:
          Result := OCI_SYSDGD;
        cmSysKM:
          Result := OCI_SYSKMT;
      else
        Result := OCI_DEFAULT;
      end;
  end;

  procedure ConnectDefaultOCI80;
  begin
    if OCISvcCtx.UnicodeEnv and not FUseUnicode then
      RaiseError(SUseUnicodeRequired);

    OCISvcCtx.AllocSvcCtxHandle;

    //For child proxy session we don't need Server handle
    if (FProxyConnection = nil) and (hServer = nil) then
      Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hServer, OCI_HTYPE_SERVER, 0, nil));
    if hSession = nil then
      Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hSession, OCI_HTYPE_SESSION, 0, nil));

  {$IFNDEF LITE}
    if (FClientIdentifier <> '') and (OCISvcCtx.Home.OCIVersion >= 9000) then begin
      p := StringToHGlobalOCI(FClientIdentifier, Size, OCISvcCtx.UnicodeEnv);
      Res := OCI8.OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size,
        OCI_ATTR_CLIENT_IDENTIFIER, OCISvcCtx.hOCIError);
      FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
      Check(Res);
    end;
  {$ENDIF}

    try
      if FProxyConnection = nil then begin
      {$IFNDEF MSWINDOWS}
        hLockConnect.Enter;
        try
      {$ENDIF}
          p := StringToHGlobalOCI(FServer, Size, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIServerAttach(hServer, OCISvcCtx.hOCIError, p, Size, OCI_DEFAULT);
          FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
          Check(Res);
      {$IFNDEF MSWINDOWS}
        finally
          hLockConnect.Leave;
        end;
      {$ENDIF}
      end;

      if (OCI73 in OCISvcCtx.Home.PossibleOCICallStyles) and (FProxyConnection = nil) then begin
        p := StringToHGlobalOCI(FInternalName, Size, OCISvcCtx.UnicodeEnv);
        Res := OCI8.OCIAttrSet1(hServer, OCI_HTYPE_SERVER, p, Size, OCI_ATTR_INTERNAL_NAME, OCISvcCtx.hOCIError);
        FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
        Check(Res);
      end;
      try
      // Set the server context in the service context
        if FProxyConnection = nil then
          Check(OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, hServer, 0, OCI_ATTR_SERVER, OCISvcCtx.hOCIError))
        else
          Check(OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, FProxyConnection.hServer, 0, OCI_ATTR_SERVER, OCISvcCtx.hOCIError));

        if FProxyConnection <> nil then begin
          p := StringToHGlobalOCI(FUsername, Size, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size, OCI_ATTR_USERNAME, OCISvcCtx.hOCIError);
          FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
          Check(Res);

          if FPassword <> '' then begin
            p := StringToHGlobalOCI(FPassword, Size, OCISvcCtx.UnicodeEnv);
            Res := OCI8.OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size, OCI_ATTR_PASSWORD, OCISvcCtx.hOCIError);
            FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
            Check(Res);
          end;

          Check(OCI8.OCIAttrSet1(hSession, OCI_HTYPE_SESSION, FProxyConnection.hSession, 0, OCI_ATTR_PROXY_CREDENTIALS, OCISvcCtx.hOCIError));
          Credt := OCI_CRED_PROXY;
        end
        else
          if FUsername <> '' then begin
          // Set username and password attribute in user session handle
            p := StringToHGlobalOCI(FUsername, Size, OCISvcCtx.UnicodeEnv);
            Res := OCI8.OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size, OCI_ATTR_USERNAME, OCISvcCtx.hOCIError);
            FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
            Check(Res);

            p := StringToHGlobalOCI(FPassword, Size, OCISvcCtx.UnicodeEnv);
            Res := OCI8.OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size, OCI_ATTR_PASSWORD, OCISvcCtx.hOCIError);
            FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
            Check(Res);

            Credt := OCI_CRED_RDBMS;
          end
          else
            Credt := OCI_CRED_EXT;

        Mode := GetOraConnectMode(False);

        ///Version > 9200
        if FStatementCache and
           (OCI73 in OCISvcCtx.Home.PossibleOCICallStyles) and (FOCICallStyle = OCI80) and
           (OCISvcCtx.Home.OCIVersion > 9200)
        then
          Mode := Mode or OCI_STMT_CACHE;

      {$IFDEF NET}
        // Direct mode connection attributes
        if FDirect then begin
          Check(OCI8.OCIAttrSet2(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, FConnectionTimeout, 0, OCI_ATTR_CONNECTION_TIMEOUT, OCISvcCtx.hOCIError));

          if FUseUnicode then begin
            CharsetId := Integer(OCI_UTF16ID);
            Check(OCI8.OCIAttrSet2(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, CharsetId, 0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));
          end
          else if FCharset <> '' then begin
            p := StringToHGlobalOCI(FCharset, Size, OCISvcCtx.UnicodeEnv);
            Res := OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, p, Size, OCI_ATTR_CHARSET, OCISvcCtx.hOCIError);
            FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
            Check(Res);
        {$IFDEF IS_UTF8}
          end
          else begin
            CharsetId := FCharsetId;
            Check(OCI8.OCIAttrSet2(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, CharsetId, 0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));
        {$ENDIF}
          end;

          IPVersion := Integer(FIPVersion);
          Res := OCI8.OCIAttrSet2(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, IPVersion, 0, OCI_ATTR_IP_VERSION, OCISvcCtx.hOCIError);
          Check(Res);

        {$IFNDEF LITE}
          p := AllocGCHandle(FIOHandler);
          Res := OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, p, Size, OCI_ATTR_IOHANDLER, OCISvcCtx.hOCIError);
          FreeGCHandle(p);
          Check(Res);

          p := AllocGCHandle(FHttpOptions);
          Res := OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, p, Size, OCI_ATTR_HTTP_OPTIONS, OCISvcCtx.hOCIError);
          FreeGCHandle(p);
          Check(Res);

          p := AllocGCHandle(FProxyOptions);
          Res := OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, p, Size, OCI_ATTR_HTTP_PROXY_OPTIONS, OCISvcCtx.hOCIError);
          FreeGCHandle(p);
          Check(Res);

          p := AllocGCHandle(FSSLOptions);
          Res := OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, p, Size, OCI_ATTR_SSL_OPTIONS, OCISvcCtx.hOCIError);
          FreeGCHandle(p);
          Check(Res);

        {$ENDIF}
        end;
      {$ENDIF}

      {$IFNDEF MSWINDOWS}
        hLockConnect.Enter;
        try
      {$ENDIF}
          Check(OCI8.OCISessionBegin(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hSession, Credt, Mode));
      {$IFNDEF MSWINDOWS}
        finally
          hLockConnect.Leave;
        end;
      {$ENDIF}

        // Set the user session in the service context
        Check(OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, hSession, 0, OCI_ATTR_SESSION, OCISvcCtx.hOCIError));

        if OCI73 in OCISvcCtx.Home.PossibleOCICallStyles then
          if GetOracleVersion < 8000 then
            FOCICallStyleCommand := OCI73;
      except
        if hServer <> nil then
          Check(OCI8.OCIServerDetach(hServer, OCISvcCtx.hOCIError, OCI_DEFAULT));
        raise;
      end;
    except
      on EFailOver do begin
        // do not free handles
      end
      else begin
        if hSession <> nil then
          OCI8.OCIHandleFree(hSession, OCI_HTYPE_SESSION);
        if hServer <> nil then
          OCI8.OCIHandleFree(hServer, OCI_HTYPE_SERVER);
        OCISvcCtx.FreeSvcCtxHandle;
        hSession := nil;
        hServer := nil;
      end;

      raise;
    end;
  end;

  procedure ConnectOCIPooled;
  begin
    CheckOCI80;
    FOCIPoolName := ConnectString;

    if OCISvcCtx.UnicodeEnv and not FUseUnicode then
      RaiseError(SUseUnicodeRequired);

    if hOCIAuthInfo = nil then
      Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hOCIAuthInfo, OCI_HTYPE_AUTHINFO, 0, nil));

    try
      p := StringToHGlobalOCI(FUserName, Size, OCISvcCtx.UnicodeEnv);
      Res := OCI8.OCIAttrSet1(hOCIAuthInfo, OCI_HTYPE_AUTHINFO, p, Size, OCI_ATTR_USERNAME, OCISvcCtx.hOCIError);
      FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
      Check(Res);

      p := StringToHGlobalOCI(FPassword, Size, OCISvcCtx.UnicodeEnv);
      Res := OCI8.OCIAttrSet1(hOCIAuthInfo, OCI_HTYPE_AUTHINFO, p, Size, OCI_ATTR_PASSWORD, OCISvcCtx.hOCIError);
      FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
      Check(Res);

      Mode := OCI_SESSGET_SPOOL;
      if FStatementCache and
         (OCI73 in OCISvcCtx.Home.PossibleOCICallStyles) and (FOCICallStyle = OCI80) and
         (OCISvcCtx.Home.OCIVersion > 9200)
      then
        Mode := Mode or OCI_SESSGET_STMTCACHE;

      OCISvcCtx.AllocPooledSvcCtxHandle(FOCIPoolName, hOCIAuthInfo);
    except
      if hOCIAuthInfo <> nil then
        Check(OCI8.OCIHandleFree(hOCIAuthInfo, OCI_HTYPE_AUTHINFO));

      raise;
    end;
  end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  procedure ConnectMTSPooled;
  begin
    OCISvcCtx.AllocMTSPooledSvcCtxHandle(FServer, FUserName, FPassword);

    if OCISvcCtx.UnicodeEnv and not FUseUnicode then begin
      Disconnect;
      RaiseError(SUseUnicodeRequired);
    end;
  end;
{$ENDIF}
{$ENDIF}

var
  Environment: TOCIEnvironment;
begin
  if FConnected then
    Exit;

  Environment := AllocEnvironment;
  FOCISvcCtx := TOCISvcCtx.Create(Environment, FUseUnicode);
{$IFNDEF AUTOREFCOUNT}
  FOCISvcCtx.AddRef;
{$ENDIF}

  try
    SetupConnection;

    if FOCICallStyle = OCI73 then begin
    // Allocate LDA, HDA
      if LDA = nil then
        New(LDA);
      if HDA = nil then
        New(HDA);
      FillChar(HDA, HDA_SIZE, 0);

      try
        Check(OCI7.olog(LDA, HDA, PAnsiChar(AnsiString(FUsername)), -1,
          PAnsiChar(AnsiString(FPassword)), -1,
          PAnsiChar(AnsiString(FServer)), -1, OCI_LM_DEF));

        if FThreadSafety and not OCI7.GetThreadSafety then
          OCI7.SetThreadSafety(True);
      except
        Dispose(HDA);
        Dispose(LDA);
        LDA := nil;
        HDA := nil;
        raise;
      end;
    end
    else
    if FOCICallStyle = OCI80 then begin
      case FConnectionType of
        ctDefault:
          ConnectDefaultOCI80;
        ctOCIPooled:
          ConnectOCIPooled;
      {$IFNDEF LITE}
      {$IFDEF MSWINDOWS}
        ctMTSPooled:
          ConnectMTSPooled;
      {$ENDIF}
      {$ENDIF}
      end;
    end
    else
      CheckOCI;

    FConnected := True;

    SetStatementCacheSize(FStatementCacheSize);

    // FailOver callback for child ProxyConnection
    // implemented using base ProxyConnection failover events
    if (FProxyConnection = nil) and (OCI73 in OCISvcCtx.Home.PossibleOCICallStyles) and
      (FOCICallStyle = OCI80) and (FConnectionType = ctDefault) and
      (GetOracleVersion >= 8000)
    then begin
      Failover.fo_ctx := AllocGCHandle(Self, False);
      Failover.callback_function := OCICallbackFailoverPtr;
      Handle := AllocGCHandle(@Failover, True);
      try
        Check(OCI8.OCIAttrSet1(hServer, OCI_HTYPE_SERVER, GetAddrOfPinnedObject(Handle), 0, OCI_ATTR_FOCBK, OCISvcCtx.hOCIError));
      finally
        FreeGCHandle(Handle);
      end;
    end;

    // apply user NLS settings
    for i := Low(FNlsParams) to High(FNlsParams) do
      if FNlsParams[i].IsUserDefined then
        SetNlsParameter(FNlsParams[i].Name, FNlsParams[i].Value);

    InitSessionParameters;

  {$IFNDEF LITE}
    SetOptimizerMode;
    if (FClientIdentifier <> '') and (OCISvcCtx.Home.OCIVersion < 9000) then
      SetClientIdentifier;
    if FSchema <> '' then
      SetCurrentSchema(FSchema);
  {$ENDIF}

    inherited;
  except
    if FConnected then
      Disconnect
    else
    {$IFNDEF AUTOREFCOUNT}
      FOCISvcCtx.ReleaseRef;
    {$ENDIF}
      FOCISvcCtx := nil;

    raise;
  end;
end;

procedure TOCIConnection.Disconnect;
var
  i: TNlsParamType;
begin
  if not FConnected then
    Exit;

  FConnected := False;

  if OCISvcCtx <> nil then
  try
    if FOCICallStyle = OCI73 then begin
      try
        if FNativeConnection then
          Check(OCI7.ologof(LDA));
      finally
      // Free LDA, HDA
        if HDA <> nil then
          Dispose(HDA);
        if (IntPtr(LDA) <> nil) and FNativeConnection then
          Dispose(LDA);
        HDA := nil;
        LDA := nil;
      end;
    end
    else
    if FOCICallStyle = OCI80 then begin
      StopTimeoutThread;

      try
        case FConnectionType of
          ctDefault: begin
            if FNativeConnection then begin
              // We should leave ObjectTypes in DisconnectMode to access Objects
              // Types will be destroyed only when there are no more references on them
              if ObjectTypes <> nil then
                ObjectTypes.ClearTypes(OCISvcCtx, FDisconnectMode);
              if OCISvcCtx.hOCIError <> nil then
                Check(OCI8.OCISessionEnd(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hSession, OCI_DEFAULT));
              if hServer <> nil then
                Check(OCI8.OCIServerDetach(hServer, OCISvcCtx.hOCIError, OCI_DEFAULT));
              OCISvcCtx.FreeSvcCtxHandle;
            end;
          end;
          ctOCIPooled: begin
            Check(OCI8.OCISessionRelease(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, nil, 0, OCI_DEFAULT));
            OCISvcCtx.FreeSvcCtxHandle;
          end;
        {$IFNDEF LITE}
        {$IFDEF MSWINDOWS}
          ctMTSPooled: begin
            MTSCheck(MTS.OraMTSSvcRel(OCISvcCtx.hOCISvcCtx));
            OCISvcCtx.FreeSvcCtxHandle;
          end;
        {$ENDIF}
        {$ENDIF}
        end;
      finally
      // Free handles
        if hOCIAuthInfo <> nil then
          OCI8.OCIHandleFree(hOCIAuthInfo, OCI_HTYPE_AUTHINFO);
        if hSession <> nil then
          OCI8.OCIHandleFree(hSession, OCI_HTYPE_SESSION);
        if hServer <> nil then
          OCI8.OCIHandleFree(hServer, OCI_HTYPE_SERVER);
      {$IFDEF LOCAL_ERROR_HANDLE}
        if hOCIError <> nil then
          OCI8.OCIHandleFree(hOCIError, OCI_HTYPE_ERROR);
      {$ENDIF}
        hOCIAuthInfo := nil;
        hSession := nil;
        hServer := nil;

        if (IntPtr(LDA) <> nil) and FNativeConnection then begin  // For convert to LDA
          Dispose(LDA);
          LDA := nil;
        end;
      end;
    end
    else
      CheckOCI;

  finally
    if FOCISvcCtx <> nil then begin
    {$IFNDEF AUTOREFCOUNT}
      FOCISvcCtx.ReleaseRef;
    {$ENDIF}
      FOCISvcCtx := nil;
    end;

    FNativeConnection := True;
    FOracleVersionFull := '';
    FOracleVersionSt := '';
    FOracleVersion := 0;
  {$IFNDEF LITE}
    FCachedSchema := '';
    FCachedUser := '';
  {$ENDIF}

    // reset session parameters
    FMaxStringSize := 0;
    FCharsetId := 0;
    FInternalCharsetId := 0;
    if FQueryCharLength then
      FCharLength := 0;
    for i := Low(FNlsParams) to High(FNlsParams) do
      if not FNlsParams[i].IsUserDefined then
        FNlsParams[i].Value := '';
  end;
end;

procedure TOCIConnection.Ping;
var
  Command: TCRCommand;
begin
  if (OCISvcCtx.Home.PossibleOCICallStyles <> [OCI80]) and
     (OCISvcCtx.Home.OCIVersion >= 10200) and (GetOracleVersion >= 10200)
  then
    Check(OCI8.OCIPing(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, OCI_DEFAULT))
  else begin
    Command := GetCommand;
    try
      Command.SetSQL('Begin Null; End;');
      Command.Execute;
    finally
      ReleaseCommand(Command);
    end;
  end;
end;

function TOCIConnection.GetOracleVersion: word;
begin
  if FOracleVersion = 0 then
    FOracleVersion := VersionStrToWord(GetServerVersion);
  Result := FOracleVersion;
end;

function TOCIConnection.GetServerVersion: string;

  // Extracts Oracle version number from string returned by OCIServerVersion.
  function GetVersionNumber(RawVersion: string): string;
  var
    Ind, Start: integer;
  begin
    Result := '';
    Ind := Pos('.', RawVersion);
    if Ind > 1 then begin
      Start := Ind - 1;
      if (Start > 1) and (RawVersion[Start - 1] >= '0') and (RawVersion[Start - 1] <= '9') then //10.0
        Start := Start - 1;
      Ind := Pos(' ', Copy(RawVersion, Start, Length(RawVersion) - (Start - 1)) );
      if Ind > 0 then
        Result := Copy(RawVersion, Start, Ind - 1);
    end;
  end;

begin
  if FOracleVersionSt = '' then begin
    if FOracleVersionFull = '' then
      GetServerVersionFull;
    if FOracleVersionSt = '' then
      FOracleVersionSt := GetVersionNumber(FOracleVersionFull);
  end;

  Result := FOracleVersionSt;
end;

function TOCIConnection.GetServerVersionFull: string;
var
  VersionFull: IntPtr;
  BufSize: integer;
  Command: TCRCommand;
  Param: TParamDesc;
begin
  if FOracleVersionFull = '' then begin
    if not {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCILite then begin
      try
        if FOCICallStyle = OCI73 then begin
          Command := GetCommand;
          try
            Command.SetSQL(
              'begin SELECT Product, Version INTO :Product, :Version ' +
              'FROM SYS.PRODUCT_COMPONENT_VERSION ' +
              'WHERE Upper(Product) LIKE ''%ORACLE%''; end;');

            Param := Command.Params[0];
            Param.SetDataType(dtString);
            Param.SetParamType(pdOutput);
            Param.SetSize(65);

            Param := Command.Params[1];
            Param.SetDataType(dtString);
            Param.SetParamType(pdOutput);
            Param.SetSize(65);

            Command.Execute;

            FOracleVersionSt := Command.Params[1].GetValue;
            FOracleVersionFull := Command.Params[0].GetValue + FOracleVersionSt;
          finally
            ReleaseCommand(Command);
          end;
        end
        else begin
          BufSize := 255 * SizeOfCharOCI(OCISvcCtx.UnicodeEnv);
          VersionFull := Marshal.AllocHGlobal(BufSize);
          try
            Check(OCI8.OCIServerVersion(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, VersionFull,
              BufSize, OCI_HTYPE_SVCCTX)); // ORA-0 on Lite
            FOracleVersionFull := PtrToStringOCI(VersionFull, OCISvcCtx.UnicodeEnv);
          finally
            Marshal.FreeHGlobal(VersionFull);
          end;
        end;
      except
        on E: EOraError do
          if E.ErrorCode <> 6550 then
            raise;
      end;
    end
    else
      FOracleVersionFull := OCISvcCtx.Home.OCIVersionSt + ' Lite';
  end;

  Result := FOracleVersionFull;
end;

function TOCIConnection.GetClientVersion: string;
begin
  Result := OCISvcCtx.Home.OCIVersionSt;
end;

{$IFNDEF LITE}
procedure TOCIConnection.SetCurrentSchema(SchemaName: string);
var
  Command: TCRCommand;
begin
  if {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCILite then
    raise Exception.Create(SOLiteSchemaNotSupported);
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);
  if SchemaName = '' then
    SchemaName := GetDefaultSchema;

  Command := GetCommand;
  try
    Command.SetSQL('ALTER SESSION SET CURRENT_SCHEMA = ' + SchemaName);
    try
      Command.Execute;
    except
      on E: EOraError do
        if E.ErrorCode = 1435 then
          raise EOraError.Create(E.ErrorCode,'Schema does not exist'+#$A, Self)
        else
          raise;
    end;
  finally
    ReleaseCommand(Command);
  end;

  FSchema := SchemaName;
  FCachedSchema := '';
end;

function TOCIConnection.GetCurrentSchema: string;
var
  Command: TCRCommand;
  Param: TParamDesc;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if (GetOracleVersion < 8100) or ({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCILite) then
    if FSchema = '' then
      Result := GetDefaultSchema
    else
      Result := FSchema
  else begin
    Command := GetCommand;
    try
      Command.SetSQL(
        'begin' +
        '  :Result := SYS_CONTEXT (''USERENV'', ''CURRENT_SCHEMA'');' +
        'end;');

      Param := Command.Params[0];
      Param.SetDataType(dtString);
      Param.SetParamType(pdOutput);
      Param.SetSize(50);

      Command.Execute;

      Result := OCISQLInfo.QuoteIfNeed(Param.GetValue);
    finally
      ReleaseCommand(Command);
    end;
  end;
end;

function TOCIConnection.GetDefaultSchema: string;
begin
  if (FConnectMode in [cmSysOper, cmSysDBA, cmSysBackup, cmSysDG, cmSysKM]) then
    Result := 'SYS'
  else
    Result := OCISQLInfo.NormalizeName(FUserName, False, True);
end;

function TOCIConnection.GetCachedSchema: string;
begin
  if FCachedSchema = '' then
    if FSchema <> '' then
      FCachedSchema := OCISQLInfo.NormalizeName(FSchema, False, True)
    else
      FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;

function TOCIConnection.GetCurrentUser: string;
var
  Command: TCRCommand;
  Param: TParamDesc;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if GetOracleVersion < 8100 then
    Result := FUsername
  else begin
    Command := GetCommand;
    try
      Command.SetSQL(
        'begin' +
        '  :Result := SYS_CONTEXT (''USERENV'', ''SESSION_USER'');' +
        'end;');

      Param := Command.Params[0];
      Param.SetDataType(dtString);
      Param.SetParamType(pdOutput);
      Param.SetSize(50);

      Command.Execute;

      Result := OCISQLInfo.QuoteIfNeed(Param.GetValue);
    finally
      ReleaseCommand(Command);
    end;
  end;
end;

function TOCIConnection.GetCachedUser: string;
begin
  if FCachedUser = '' then
    FCachedUser := GetCurrentUser;

  Result := FCachedUser;
end;

{$ENDIF}

function TOCIConnection.GetDBKeyList(TableName: string): string;
const
  DBKeyListSQL =
    'DECLARE ' +
    '  OLD_INDEX_OWNER VARCHAR2(128) := NULL;' +
    '  OLD_INDEX_NAME VARCHAR2(128) := NULL;' +
    'BEGIN' +
    '  :KEY_LIST := NULL;' +
    '  IF :TABLE_OWNER IS NULL THEN' +
    '    :TABLE_OWNER := SYS_CONTEXT (''USERENV'', ''CURRENT_SCHEMA'');' +
    '  END IF;' +
    '  FOR cc IN (' +
    '    SELECT /*+ RULE */' +
    '      cc.COLUMN_NAME' +
    '    FROM' +
    '      ALL_CONSTRAINTS c, ALL_CONS_COLUMNS cc' +
    '    WHERE' +
    '      c.OWNER = :TABLE_OWNER AND' +
    '      c.TABLE_NAME = :TABLE_NAME AND' +
    '      c.CONSTRAINT_TYPE = ''P'' AND' +
    '      c.OWNER = cc.OWNER AND' +
    '      c.CONSTRAINT_NAME = cc.CONSTRAINT_NAME' +
    '    ORDER BY cc.POSITION' +
    '  )' +
    '  LOOP' +
    '    IF :KEY_LIST IS NULL THEN' +
    '      :KEY_LIST :=  cc.COLUMN_NAME;' +
    '    ELSE' +
    '      :KEY_LIST := :KEY_LIST || '','' || cc.COLUMN_NAME;' +
    '    END IF;' +
    '  END LOOP;' +
    '  IF :KEY_LIST IS NOT NULL THEN' +
    '    RETURN;' +
    '  END IF;' +
    '  FOR ic IN (' +
    '    SELECT /*+ RULE */' +
    '      ic.INDEX_OWNER,' +
    '      ic.INDEX_NAME,' +
    '      ic.COLUMN_NAME' +
    '    FROM' +
    '      ALL_INDEXES i, ALL_IND_COLUMNS ic' +
    '    WHERE' +
    '      i.TABLE_OWNER = :TABLE_OWNER AND' +
    '      i.TABLE_NAME = :TABLE_NAME AND' +
    '      i.UNIQUENESS = ''UNIQUE'' AND' +
    '      i.OWNER = ic.INDEX_OWNER AND' +
    '      i.INDEX_NAME = ic.INDEX_NAME' +
    '    ORDER BY DECODE(IC.INDEX_OWNER, :TABLE_OWNER, 1, ''PUBLIC'', 2, 3),' +
    '             IC.INDEX_OWNER, IC.TABLE_OWNER, IC.TABLE_NAME, IC.INDEX_NAME, IC.COLUMN_POSITION' +
    '  )' +
    '  LOOP' +
    '    IF (OLD_INDEX_OWNER IS NULL) AND (OLD_INDEX_NAME IS NULL) THEN' +
    '      :KEY_LIST := ic.COLUMN_NAME;' +
    '    ELSE' +
    '      IF (OLD_INDEX_OWNER != ic.INDEX_OWNER) OR (OLD_INDEX_NAME != ic.INDEX_NAME) THEN' +
    '        EXIT;' +
    '      END IF;' +
    '      :KEY_LIST := :KEY_LIST || '','' || ic.COLUMN_NAME;' +
    '    END IF;' +
    '    OLD_INDEX_OWNER := ic.INDEX_OWNER;' +
    '    OLD_INDEX_NAME := ic.INDEX_NAME;' +
    '  END LOOP;' +
    'END;';
var
  Info: TSQLObjectInfo;
  SchemaName: string;
  Command: TCRCommand;
  Param: TParamDesc;
begin
  Command := GetCommand;
  try
    Command.SetSQL(DBKeyListSQL);

    Command.SQLInfo.SplitObjectName(TableName, Info);
    if Info.Schema = '' then
    {$IFNDEF LITE}
      // do not call GetCachedSchema to avoid additional query to server
      SchemaName := Command.SQLInfo.NormalizeName(FCachedSchema, False, True)
    {$ELSE}
      SchemaName := ''
    {$ENDIF}
    else
      SchemaName := Command.SQLInfo.NormalizeName(Info.Schema, False, True);
    TableName := Command.SQLInfo.NormalizeName(Info.Name, False, True);

    Param := Command.Params[0];
    Param.SetDataType(dtString);
    Param.SetParamType(pdOutput);
    Param.SetSize(1025);
    Param.SetNull(True);

    Param := Command.Params[1];
    Param.SetDataType(dtString);
    Param.SetParamType(pdInputOutput);
    Param.SetSize(129);
    Param.SetValue(SchemaName);

    Param := Command.Params[2];
    Param.SetDataType(dtString);
    Param.SetParamType(pdInput);
    Param.SetSize(129);
    Param.SetValue(TableName);

    Command.Execute;

  {$IFNDEF LITE}
    if SchemaName = '' then begin
      Param := Command.Params[1];
      if not Param.GetNull then
        FCachedSchema := VarToStr(Param.GetValue);
    end;
  {$ENDIF}

    Param := Command.Params[0];
    if not Param.GetNull then
      Result := VarToStr(Param.GetValue)
    else
      Result := '';
  finally
    ReleaseCommand(Command);
  end;
end;

procedure TOCIConnection.GetTableFields(TableName: string; Fields: TStringList);
var
  Handle: IntPtr;
  hDescribe: pOCIDescribe;
  b1: Integer; //byte
  hParam, hColList, hCol: pOCIParam;
  NumCols: ub2;
  i, Len, BufSize: Integer;
  ValueInt: Integer;
  Ptr, ValuePtr, StrPtr: IntPtr;
  ObjType: byte;
  pName, DbLink: string;
begin
  Fields.Clear;
  if FOCICallStyleCommand = OCI73 then
    Exit
  else if (FOCICallStyleCommand = OCI80) and (GetOracleVersion = 8050) then
    Exit
  else
    CheckOCI;

  Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
  try
    b1 := 1;
    Check(OCI8.OCIAttrSet2(hDescribe, OCI_HTYPE_DESCRIBE, b1, 0, OCI_ATTR_DESC_PUBLIC, OCISvcCtx.hOCIError));
    Ptr := Marshal.AllocHGlobal(sizeof(integer));
    try
      DbLink := '';
      while True do begin
        Handle := StringToHGlobalOCI(TableName, BufSize, OCISvcCtx.UnicodeEnv);
        try
          try
            Check(OCI8.OCIDescribeAny(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, Handle, BufSize,
              OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, hDescribe));
          except
            on EOraError do // OCIDescribeAny fails on Oracle 8 if TableName contains db link
              Exit;
          end;
        finally
          FreeStringOCI(Handle, OCISvcCtx.UnicodeEnv);
        end;

        ValuePtr := @hParam;
        Check(OCI8.OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil, OCI_ATTR_PARAM, OCISvcCtx.hOCIError));

        Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PTYPE, OCISvcCtx.hOCIError));
        ObjType := Byte(ValueInt);

        case ObjType of
          OCI_PTYPE_TABLE, OCI_PTYPE_VIEW: begin
          end;
          OCI_PTYPE_SYN: begin
          // Describe synonyms
            {if d2 > 0 then
              TableName := '.' + Copy(TableName, d2 + 1, Length(TableName))
            else}
              TableName := '';

            StrPtr := nil;
            ValuePtr := @StrPtr;
            Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr,  OCI_ATTR_NAME, OCISvcCtx.hOCIError));

            Len := Cardinal(Marshal.ReadInt32(Ptr));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));
            TableName := pName + TableName;

            StrPtr := nil;
            ValuePtr := @StrPtr;
            Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_SCHEMA_NAME, OCISvcCtx.hOCIError));

            Len := Cardinal(Marshal.ReadInt32(Ptr));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));
            if pName <> '' then  // can be NULL for synonyms with db link
              TableName := pName + '.' + TableName;

            StrPtr := nil;
            ValuePtr := @StrPtr;
            Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_LINK, OCISvcCtx.hOCIError));

            Len := Cardinal(Marshal.ReadInt32(Ptr));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));
            if pName <> '' then begin
              if DbLink <> '' then
                Exit; // Cannot use two db links
              DbLink := '@' + pName;
            end;
            TableName := TableName + DbLink;

            Continue;
          end;
        else
          RaiseOraError(4043, Format(SObjectNotExist, [TableName]));
        end;

        Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_NUM_COLS, OCISvcCtx.hOCIError));
        NumCols := ub2(ValueInt);

        ValuePtr := @hColList;
        Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil, OCI_ATTR_LIST_COLUMNS, OCISvcCtx.hOCIError));

        for i := 1 to NumCols do begin
          Check(OCI8.OCIParamGet(hColList, OCI_DTYPE_PARAM, OCISvcCtx.hOCIError, hCol, i));  // hProc
          try
            StrPtr := nil;
            ValuePtr := @StrPtr;
            Check(OCI8.OCIAttrGet1(hCol, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_NAME, OCISvcCtx.hOCIError));

            Len := Marshal.ReadInt32(Ptr);
            Fields.Add(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));
          finally
            // Free memory after OCIParamGet
            OCI8.OCIDescriptorFree(hCol, OCI_DTYPE_PARAM);
          end;
        end;

        break;
      end;
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  finally
    Check(OCI8.OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE));
  end;
end;

procedure TOCIConnection.GetSessionParameters;

  function GetDefaultCodePage: Integer;
  begin
  {$IFDEF NEXTGEN}
    Result := Encoding.Default.CodePage;
  {$ELSE}
  {$IFNDEF MSWINDOWS}
    Result := 0;
  {$ELSE}
  {$IFDEF VER12P}
    Result:= DefaultSystemCodePage;
  {$ELSE}
  {$IFDEF FPC}
    Result := 65001;
  {$ELSE}
    Result := sb4(GetACP());
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  end;

var
  i: integer;
  ClientCharset: string;
  CharsetIdNeeded: boolean;
  SQLText: string;
  Command: TCRCommand;
  Param: TOraParamDesc;
  Value: variant;
begin
  FInternalCharsetId := 0;

  if FConnected and not OCILite then begin
    Command := GetCommand;
    try
      if FCharset <> '' then
        ClientCharset := FCharset
      else if Direct then
        FInternalCharsetId := GetCodePageCharset(GetDefaultCodePage, GetOracleVersion)
      else if (GetOracleVersion < 11000) or (OCISvcCtx.Home.OCIVersion < 11000) then begin
        ClientCharset := FOCISvcCtx.Environment.Home.NLSLang;
        i := Pos('.', ClientCharset);
        if i > 0 then
          ClientCharset := copy(ClientCharset, i + 1, Length(ClientCharset) - i)
      end;

      CharsetIdNeeded :=
        (FInternalCharsetId = 0) and
        (GetOracleVersion >= 8000) and
        ((ClientCharset <> '') or ((FOracleVersion >= 11000) and (OCISvcCtx.Home.OCIVersion >= 11000)));

      SQLText :=
        'declare' +
        '  cursor NlsParamsCursor is' +
        '    SELECT * FROM nls_session_parameters;' +
        'begin' +
           // AL32UTF8 can has one char length = 4 bytes
        '  SELECT Nvl(Lengthb(Chr(16777216)), Nvl(Lengthb(Chr(65536)), Nvl(Lengthb(Chr(256)), 1))), Nvl(Lengthb(Chr(1)), 1)' +
        '    INTO :MaxCharLength, :MinCharLength FROM sys.dual;' +
        '  for NlsRecord in NlsParamsCursor loop' +
        '    if NlsRecord.parameter = ''NLS_DATE_LANGUAGE'' then' +
        '      :NlsDateLanguage := NlsRecord.value;' +
        '    elsif NlsRecord.parameter = ''NLS_DATE_FORMAT'' then' +
        '      :NlsDateFormat := NlsRecord.value;'+
        '    elsif NlsRecord.parameter = ''NLS_NUMERIC_CHARACTERS'' then' +
        '      :NlsNumericCharacters := NlsRecord.value;';
      if GetOracleVersion >= 8000 then
        SQLText := SQLText +
          '    elsif NlsRecord.parameter = ''NLS_TIMESTAMP_FORMAT'' then' +
          '      :NlsTimeStampFormat := NlsRecord.value;' +
          '    elsif NlsRecord.parameter = ''NLS_TIMESTAMP_TZ_FORMAT'' then' +
          '      :NlsTimeStampTZFormat := NlsRecord.value;';
      SQLText := SQLText +
        '    end if;' +
        '  end loop;';
      if CharsetIdNeeded then
        if ClientCharset <> '' then
          SQLText := SQLText +  '  SELECT NLS_CHARSET_ID(''' + ClientCharset + ''') INTO :CharsetId FROM SYS.DUAL;'
        else if (FOracleVersion >= 11000) and (OCISvcCtx.Home.OCIVersion >= 11000) then
          SQLText := SQLText +  '  SELECT NLS_CHARSET_ID(CLIENT_CHARSET) INTO :CharsetId FROM V$SESSION_CONNECT_INFO WHERE SID = SYS_CONTEXT(''USERENV'', ''SID'') AND ROWNUM <= 1;'
        else
          SQLText := SQLText +  '  SELECT 0 INTO :CharsetId FROM SYS.DUAL;';

      SQLText := SQLText + 'end;';

      Command.SetSQL(SQLText);

      // setup params
      for i := 0 to Command.Params.Count - 1 do begin
        Param := TOraParamDesc(Command.Params[i]);
        Param.SetDataType(dtString);
        Param.SetParamType(pdOutput);
        Param.SetSize(40);
      end;
      Command.Params[0].SetDataType(dtInteger);
      Command.Params[1].SetDataType(dtInteger);

      if CharsetIdNeeded then
        Command.Params[7].SetDataType(dtInteger);

      try
        // execute
        Command.Execute;

        Value := Command.Params[0].GetValue;
        if VarIsNull(Value) then
          FMaxCharLength := 1
        else
          FMaxCharLength := Value;
        Value := Command.Params[1].GetValue;
        if VarIsNull(Value) then
          FMinCharLength := 1
        else
          FMinCharLength := Value;

        FNlsParams[nlsDateLanguage].Value := VarToStr(Command.Params[2].GetValue);
        FNlsParams[nlsDateFormat].Value := VarToStr(Command.Params[3].GetValue);
        FNlsParams[nlsNumericCharacters].Value := VarToStr(Command.Params[4].GetValue);

        if GetOracleVersion >= 8000 then begin
          FNlsParams[nlsTimeStampFormat].Value := VarToStr(Command.Params[5].GetValue);
          FNlsParams[nlsTimeStampTZFormat].Value := VarToStr(Command.Params[6].GetValue);
        end;

        if CharsetIdNeeded then begin
          Value := Command.Params[7].GetValue;
          if VarIsNull(Value) then // for UnicodeEnvironment
            FInternalCharsetId := GetCodePageCharset(GetDefaultCodePage, GetOracleVersion)
          else
            FInternalCharsetId := Value;
        end;

        if FCharset <> '' then begin
          FCharsetId := FInternalCharsetId
      {$IFDEF IS_UTF8}
        end
        else begin
          FInternalCharsetId := GetUTF8Charset(GetOracleVersion);
          FCharsetId := FInternalCharsetId;
      {$ENDIF}
        end;

        if FQueryCharLength then
          if FInternalCharsetId > 0 then
            FCharLength := GetMaxCharLength(FInternalCharsetId)
          else
            FCharLength := FMaxCharLength;

      except
        // hide exceptions
        FCharsetId := 0;
        FInternalCharsetId := 0;
      end;
    finally
      ReleaseCommand(Command);
    end;
  end;
end;

procedure TOCIConnection.SetNlsParameter(const Name, Value: string);
var
  Command: TCRCommand;
begin
  if FConnected then begin
    Command := GetCommand;
    try
      Command.SetSQL('ALTER SESSION SET '+ Name + '=' + '''' + Value + '''');
      Command.Execute;
    finally
      ReleaseCommand(Command);
    end;
  end;
end;

function TOCIConnection.GetMaxStringSize: word;
begin
  if (FMaxStringSize = 0) and FConnected then
    if FOCICallStyle = OCI73 then
      FMaxStringSize := 2000
    else if GetOracleVersion div 1000 = 7 then
      FMaxStringSize := 2000
    else
      FMaxStringSize := 4000;

  Result := FMaxStringSize;
end;

function TOCIConnection.GetOCI7: TOCI7API;
begin
  Result := OCISvcCtx.OCI7;
end;

function TOCIConnection.GetOCI8: TOCI8API;
begin
  Result := OCISvcCtx.OCI8;
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
function TOCIConnection.GetMTS: TMTSAPI;
begin
  Result := OCISvcCtx.MTS;
end;
{$ENDIF}
{$ENDIF}

procedure TOCIConnection.BreakExec;
begin
  if FConnected then
    if FOCICallStyle = OCI73 then begin
      Check(OCI7.obreak(LDA));
    //  if Res <> BREAKED then
    end
    else
    if FOCICallStyle = OCI80 then begin
      Check(OCI8.OCIBreak(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError));
    end
    else
      CheckOCI;
end;

procedure TOCIConnection.ChangePassword(NewPassword: string);
var
  Environment: TOCIEnvironment;
  OCISvcCtx: TOCISvcCtx;
  hServer: pOCIServer;
  hSession: pOCISession;
  CharsetId: Integer;
  Res, BufSize, Size, Size2, Size3: integer;
  p, p2, p3: IntPtr;
begin
  if FConnected then begin
    OCISvcCtx := FOCISvcCtx;
    hServer := Self.hServer;
    hSession := Self.hSession;
  end
  else begin
    Environment := AllocEnvironment;
    OCISvcCtx := TOCISvcCtx.Create(Environment, FUseUnicode);
  {$IFNDEF AUTOREFCOUNT}
    OCISvcCtx.AddRef;
  {$ENDIF}
  end;

  try
    if not FConnected then begin
      OCISvcCtx.AllocSvcCtxHandle;
      OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hServer, OCI_HTYPE_SERVER, 0, nil), OCISvcCtx);
      OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hSession, OCI_HTYPE_SESSION, 0, nil), OCISvcCtx);

      p := StringToHGlobalOCI(FServer, BufSize, OCISvcCtx.UnicodeEnv);
      Res := OCISvcCtx.OCI8.OCIServerAttach(hServer, OCISvcCtx.hOCIError, p, BufSize, OCI_DEFAULT);
      FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
      OCISvcCtx.OCI8.Check(Res, OCISvcCtx);
    end;

    try
      if not FConnected then begin
        OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, hServer, 0, OCI_ATTR_SERVER, OCISvcCtx.hOCIError), OCISvcCtx);
        OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, hSession, 0, OCI_ATTR_SESSION, OCISvcCtx.hOCIError), OCISvcCtx);

        if OCISvcCtx.Home.PossibleOCICallStyles = [OCI80] then begin
          OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIAttrSet2(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, FConnectionTimeout, 0, OCI_ATTR_CONNECTION_TIMEOUT, OCISvcCtx.hOCIError), OCISvcCtx);
          if FUseUnicode then begin
            CharsetId := Integer(OCI_UTF16ID);
            OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIAttrSet2(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, CharsetId, 0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError), OCISvcCtx);
          end
          else
            if FCharset <> '' then begin
              p := StringToHGlobalOCI(FCharset, Size, OCISvcCtx.UnicodeEnv);
              Res := OCISvcCtx.OCI8.OCIAttrSet1(OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX, p, Size, OCI_ATTR_CHARSET, OCISvcCtx.hOCIError);
              FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
              OCISvcCtx.OCI8.Check(Res, OCISvcCtx);
            end;
        end;
      end;

      p := StringToHGlobalOCI(FUsername, BufSize, OCISvcCtx.UnicodeEnv);
      p2 := StringToHGlobalOCI(FPassword, Size2, OCISvcCtx.UnicodeEnv);
      p3 := StringToHGlobalOCI(NewPassword, Size3, OCISvcCtx.UnicodeEnv);
      Res := OCISvcCtx.OCI8.OCIPasswordChange(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, p, BufSize, p2, Size2, p3, Size3, OCI_AUTH);
      FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
      FreeStringOCI(p2, OCISvcCtx.UnicodeEnv);
      FreeStringOCI(p3, OCISvcCtx.UnicodeEnv);
      OCISvcCtx.OCI8.Check(Res, OCISvcCtx);

      if not FConnected then
        OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCISessionEnd(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hSession, OCI_DEFAULT), OCISvcCtx);
    finally
      if not FConnected then
        OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIServerDetach(hServer, OCISvcCtx.hOCIError, OCI_DEFAULT), OCISvcCtx);
    end;
  finally
    if not FConnected then begin
      OCISvcCtx.OCI8.OCIHandleFree(hSession, OCI_HTYPE_SESSION);
      OCISvcCtx.OCI8.OCIHandleFree(hServer, OCI_HTYPE_SERVER);
    {$IFNDEF AUTOREFCOUNT}
      OCISvcCtx.ReleaseRef;
    {$ENDIF}
    end;
  end;
end;

function TOCIConnection.GetLDA: PLDA;
begin
  if FOCICallStyle = OCI73 then
    Result := LDA
  else if FOCICallStyle = OCI80 then begin
    if IntPtr(LDA) = nil then
      New(LDA);

 // BUG with OCI 8.1.5 raise access violation on first time when uses OCI 8.0.x initialization
    OCISvcCtx.GetLDA(LDA);
    Result := LDA;
  end
  else begin
    CheckOCI;
    Result := nil;
  end;
//  Check(OCIAttrGet(hSvcCtx, OCI_HTYPE_SVCCTX, @Value, nil, OCI_ATTR_IN_V8_MODE, hOCIError));
end;

procedure TOCIConnection.SetLDA(Value: PLDA);
begin
  if IntPtr(Value) <> IntPtr(LDA) then begin
    Disconnect;

    LDA := Value;

    FNativeConnection := IntPtr(LDA) = nil;
    FConnected := IntPtr(LDA) <> nil;

    FOCICallStyleCommand := FOCICallStyle;

    if FOCICallStyle = OCI80 then begin
      if OCISvcCtx.hOCISvcCtx = nil then
        OCISvcCtx.AllocSvcCtxHandle;
      OCISvcCtx.SetLDA(LDA);
    end;
  end;
end;

procedure TOCIConnection.SetOCISvcCtx(Value: TOCISvcCtx);
begin
  if Value <> OCISvcCtx then begin
    Disconnect;

    FOCISvcCtx := Value;
  {$IFNDEF AUTOREFCOUNT}
    FOCISvcCtx.AddRef;
  {$ENDIF}

    FNativeConnection := FOCISvcCtx = nil;
    FConnected := FOCISvcCtx.hOCISvcCtx <> nil;

    FOCICallStyleCommand := FOCICallStyle;
  end;
end;

procedure TOCIConnection.Assign(Source: TCRConnection);
var
  Src: TOCIConnection;
begin
  inherited;

  Src := TOCIConnection(Source);
  FThreadSafety := Src.FThreadSafety;
  FConnectMode := Src.FConnectMode;
  FEnableIntegers := Src.FEnableIntegers;
  FEnableLargeint := Src.FEnableLargeint;
  FEnableNumbers := Src.FEnableNumbers;
  FEnableWideOraClob := Src.FEnableWideOraClob;
//  FInternalName := Src.FInternalName;
  FConnectionTimeout := Src.FConnectionTimeout;
  FStatementCache := Src.FStatementCache;
  FStatementCacheSize := Src.FStatementCacheSize;
  FHomeName := Src.FHomeName;
{$IFDEF NET}
  FDirect := Src.FDirect;
{$ENDIF}
  FIPVersion := Src.FIPVersion;
  FConnectionType := Src.FConnectionType;
{$IFNDEF LITE}
  FClientIdentifier := Src.FClientIdentifier;
  FOptimizerMode := Src.FOptimizerMode;
  FSchema := Src.FSchema;
{$ENDIF}
{$IFNDEF FPC}
  FEnableSQLTimeStamp := Src.FEnableSQLTimeStamp;
{$ELSE}
  FTimeStampAsString := Src.FTimeStampAsString;
{$ENDIF}
  FIntervalAsString := Src.FIntervalAsString;
  FSmallintPrecision := Src.FSmallintPrecision;
  FIntegerPrecision := Src.FIntegerPrecision;
  FLargeIntPrecision := Src.FLargeIntPrecision;
  FFloatPrecision := Src.FFloatPrecision;
  FBCDPrecision := Src.FBCDPrecision;
  FBCDScale := Src.FBCDScale;
  FFmtBCDPrecision := Src.FFmtBCDPrecision;
  FFmtBCDScale := Src.FFmtBCDScale;
  FCharset := Src.FCharset;
  FCharLength := Src.FCharLength;
  FQueryCharLength := Src.FQueryCharLength;
  FUseUnicode := Src.FUseUnicode;
  FNlsParams := Src.FNlsParams;
  FUnicodeAsNational := Src.FUnicodeAsNational;
end;

procedure TOCIConnection.AssignConnect(Source: TCRConnection);
var
  Src: TOCIConnection;
begin
  if Source <> Self then begin
    Disconnect;

    if Source <> nil then begin
      Src := TOCIConnection(Source);

      FOCICallStyle := Src.FOCICallStyle;
      if FOCICallStyle = OCI73 then begin
        SetOCISvcCtx(TOCIConnection(Source).OCISvcCtx);
        SetLDA(Src.GetLDA);
      end
      else if FOCICallStyle = OCI80 then
        SetOCISvcCtx(TOCIConnection(Source).OCISvcCtx)
      else
        CheckOCI;

      Assign(Src);

    {$IFNDEF LITE}
      FCachedSchema := Src.FCachedSchema;
      FCachedUser := Src.FCachedUser;
    {$ENDIF}
      FCharsetId := Src.FCharsetId;
      FMaxStringSize := Src.FMaxStringSize;

      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
    end
    else begin
      SetLDA(nil);
      SetOCISvcCtx(nil);
    end;
  end;
end;

{ Multi Thread }

procedure TOCIConnection.Lock;
begin
  if FThreadSafety then
    FLock.Enter;
end;

procedure TOCIConnection.Release;
begin
  if FThreadSafety then
    FLock.Leave;
end;

procedure TOCIConnection.RunTimeoutThread(Timeout: Integer);
begin
  if FOCICallStyle <> OCI80 then
    Exit;

  if FTimeoutThread = nil then
    FTimeoutThread := TTimeoutThread.Create(Self);

  TTimeoutThread(FTimeoutThread).Run(Timeout);
end;

procedure TOCIConnection.StopTimeoutThread;
begin
  if FTimeoutThread <> nil then
    TTimeoutThread(FTimeoutThread).Stop;
end;

{ TTimeoutThread }

constructor TTimeoutThread.Create(Connection: TOCIConnection);
begin
  inherited Create(False);

  FConnection := Connection;

  FLock := TCriticalSection.Create;
  FPauseEvent := TEvent.Create(nil, True, False, '');
  FEvent := TEvent.Create(nil, True, False, '');

  FreeOnTerminate := True;
end;

destructor TTimeoutThread.Destroy;
begin
  FEvent.Free;
  FPauseEvent.Free;
  FLock.Free;

  inherited;
end;

procedure TTimeoutThread.Run(Timeout: Integer);
begin
  FLock.Enter;
  try
    FTimeout := Timeout;
    FEvent.ResetEvent;
    FPauseEvent.SetEvent;
  finally
    FLock.Leave;
  end;
end;

procedure TTimeoutThread.Stop;
begin
  FLock.Enter;
  try
    FPauseEvent.ResetEvent;
    FEvent.SetEvent;
  finally
    FLock.Leave;
  end;
end;

procedure TTimeoutThread.Release;
begin
  FLock.Enter;
  try
    Terminate;
    Run(0);
  finally
    FLock.Leave;
  end;
end;

procedure TTimeoutThread.Execute;
var
  OCISvcCtx: TOCISvcCtx;
begin
  while True do
    if FPauseEvent.WaitFor(INFINITE) = wrSignaled then begin
      if FEvent.WaitFor(FTimeout * 1000) = wrTimeout then begin
        FLock.Enter;
        try
          if Terminated then
            Break;

          if FPauseEvent.WaitFor(0) <> wrSignaled then
            Continue;

          OCISvcCtx := FConnection.OCISvcCtx;
          OCISvcCtx.OCI8.OCIBreak(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError);
          FPauseEvent.ResetEvent;
        finally
          FLock.Leave;
        end;
      end;
    end;
end;

{$IFDEF MSWINDOWS}

function WndProc(hWindow: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  ExecThread: TExecThread;
  Recordset: TOCIRecordSet;
  ChangeNotification: TOCIChangeNotification;
  NotifyChange: TNotifyChange;
{$IFNDEF LITE}
  Alerter: TOCIAlerter;
  Event: TAlertEvent;
  E: Exception;
{$ENDIF}
begin
  Result := 0;
  try
    case Msg of
      WM_CREATE:
        Result := 0;
      WM_ENDTHREAD: begin
        ExecThread := TExecThread(GetGCHandleTarget(IntPtr(wParam)));
        try
          if Assigned(ExecThread.FEndMethod) then
            ExecThread.FEndMethod(ExecThread.FException);
          if ExecThread.FException <> nil then
            if ExecThread.FException is EOraError then
              raise EOraError.Create(EOraError(ExecThread.FException).ErrorCode, ExecThread.FException.Message)
            else
              raise Exception.Create(ExecThread.FException.Message);
        finally
          ExecThread.Terminate;
          ExecThread.WaitFor;
          ExecThread.Free;
        end;
      end;
      WM_AFTERFETCH: begin
        RecordSet := TOCIRecordSet(GetGCHandleTarget(IntPtr(wParam)));
        Recordset.OnAfterFetch();
      end;
      WM_CHANGENOTIFY: begin
        ChangeNotification := TOCIChangeNotification(GetGCHandleTarget(IntPtr(wParam)));
        NotifyChange := TNotifyChange(GetGCHandleTarget(IntPtr(lParam)));
        FreeGCHandle(IntPtr(lParam));
        try
          ChangeNotification.FOnChange(NotifyChange.NotifyType, NotifyChange.TableChanges);
        finally
          NotifyChange.Free;
        end;
      end;
    {$IFNDEF LITE}
      WM_ALERTER_EVENT: begin
        Alerter := TOCIAlerter(GetGCHandleTarget(IntPtr(wParam)));
        Event := TAlertEvent(GetGCHandleTarget(IntPtr(lParam)));
        FreeGCHandle(IntPtr(lParam));
        try
          Alerter.DoOnEvent(Event.Name, Event.Message);
        finally
          Event.Free;
          Alerter.FResponseEvent.SetEvent;
        end;
      end;
      WM_ALERTER_TIMEOUT: begin
        Alerter := TOCIAlerter(GetGCHandleTarget(IntPtr(wParam)));
        try
          Alerter.DoOnTimeOut(Boolean(Pointer(lParam)^));
        finally
          Alerter.FResponseEvent.SetEvent;
        end;
      end;
      WM_ALERTER_STOPED: begin
        Alerter := TOCIAlerter(GetGCHandleTarget(IntPtr(wParam)));
        Alerter.Stop;

        if lParam <> 0 then begin
          E := Exception(GetGCHandleTarget(IntPtr(lParam)));
          FreeGCHandle(IntPtr(lParam));
          try
            Alerter.DoOnError(E);
          finally
            E.Free;
          end;
        end;
      end;
    {$ENDIF}
    else
      Result := DefWindowProc(hWindow, Msg, wParam, lParam);
    end;
  except
  {$IFDEF LITE}
    on E: Exception do
      MessageBox(HInstance, PChar(E.Message), 'Error', MB_OK);
  {$ELSE}
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(nil);
  {$ENDIF}
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
    lpszClassName: 'ODACUtilWnd'
  );

procedure DoAllocODACWnd;
var
  TempClass: TWndClass;
  ClassRegistered: boolean;
  WndProcPtr: IntPtr;
begin
  if hODACWindow = 0 then begin
    WndClass.hInstance := HInstance;
    ClassRegistered := Windows.GetClassInfo(HInstance, WndClass.lpszClassName,
      TempClass);
    WndProcPtr := @{$IFNDEF UNIDACPRO}OraClasses{$ELSE}OraClassesUni{$ENDIF}.WndProc;

    if not ClassRegistered or ({$IFDEF FPC}@{$ENDIF}TempClass.lpfnWndProc <> WndProcPtr) then begin
      if ClassRegistered then
        Windows.UnregisterClass(WndClass.lpszClassName, HInstance);

      hODACWindow := Windows.RegisterClass(WndClass);
    end;

    hODACWindow := Windows.CreateWindowEx(WS_EX_TOOLWINDOW, 'ODACUtilWnd',
      '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
    Assert(hODACWindow > 0);
  {$IFDEF CPU64}
    Windows.SetWindowLongPtr(hODACWindow, GWL_WNDPROC, NativeInt(WndProcPtr));
  {$ELSE}
    Windows.SetWindowLong(hODACWindow, GWL_WNDPROC, Integer(WndProcPtr));
  {$ENDIF}
  end;
end;

procedure AllocODACWnd;
begin
  if IsMainThread then
    DoAllocODACWnd
  else
    SynchronizeWithMainThread(DoAllocODACWnd);
end;

constructor TExecThread.Create(RunMethod: TRunMethod; EndMethod: TEndMethod; hODACWindow: HWND; CreateSuspended: Boolean);
begin
  FRunMethod := RunMethod;
  FEndMethod := EndMethod;
  FhWindow := hODACWindow;

  inherited Create(CreateSuspended);
end;

destructor TExecThread.Destroy;
begin
  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);
  FException.Free;

  inherited;
end;

procedure TExecThread.Execute;
begin
  FGCHandle := AllocGCHandle(Self, False);
  try
    TRunMethod(FRunMethod);
    FException := nil;
  except
    on E: Exception do begin
      FException := E;
      AcquireExceptionObject;
    end;
  end;
  PostMessage(FhWindow, WM_ENDTHREAD, WPARAM(FGCHandle), 0);
end;

{$ENDIF}

function TOCIConnection.RunThread(RunMethod: TRunMethod; EndMethod: TEndMethod): TThread;
begin
{$IFDEF MSWINDOWS}
  if hODACWindow = 0 then
    AllocODACWnd;

  Result := TExecThread.Create(RunMethod, EndMethod, hODACWindow, False);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TOCIConnection.StopThread(var hThread: TThread{$IFDEF MSWINDOWS}; APeekMessage: boolean = False{$ENDIF}): boolean;
{$IFDEF MSWINDOWS}
var
  AMsg: TMSG;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := TerminateThread(hThread.Handle, 0);
  if Result = True then
    hThread.Free;
  // PeekMessage is needed to avoid AV when TOCICommand or TOCIRecordSet is
  // destroying and thread is active.
  if APeekMessage then
    PeekMessage(AMsg, hODACWindow, WM_ENDTHREAD, WM_ENDTHREAD, PM_REMOVE);
{$ELSE}
  Result := True;
{$ENDIF}
end;

procedure TOCIConnection.SetNonBlocking(Value: boolean);
begin
  if FConnected then begin
    if (FOCICallStyle = OCI73) or (FOCICallStyle = OCI80) then begin
      if Value then
        Check(OCI7.onbset(GetLDA))
      else
        Check(OCI7.onbclr(GetLDA));
    end
   {else
    if FOCICallStyle = OCI80 then begin
      Check(OCIAttrSet(hSvcCtx, OCI_HTYPE_SVCCTX, @Temp, 0, OCI_ATTR_NONBLOCKING_MODE, hOCIError));
      Check(OCIAttrGet(hSvcCtx, OCI_HTYPE_SVCCTX, @Temp, nil, OCI_ATTR_NONBLOCKING_MODE, hOCIError));
    end}
    else
      CheckOCI;
  end;
end;

function TOCIConnection.GetOCICallStyle: TOCICallStyle;
begin
  Result := FOCICallStyle;
end;

procedure TOCIConnection.SetOCICallStyle(Value: TOCICallStyle);
begin
  if Value <> FOCICallStyle then begin
    Disconnect;
    FOCICallStyle := Value;
  end;
end;

function TOCIConnection.GetOCICallStyleCommand: TOCICallStyle;
begin
  Result := FOCICallStyleCommand;
end;

function TOCIConnection.GetLastError: integer;
begin
  Result := FLastError;
end;

procedure TOCIConnection.SetLastError(Value: integer);
begin
  FLastError := Value;
end;

{ OCI73 }

{procedure TOCIConnection.SetAutoCommit(Value: boolean);
begin
  CheckOCI73;

  if Connected then
    if Value then
      Check(ocon(LDA))
    else
      Check(ocof(LDA));
end;}

function TOCIConnection.SetProp(Prop: integer; const Value: variant): boolean;
var
  S: string;
  OldCharset: string;
  OldCharsetId: word;

  procedure SetNlsParam(ParamType: TNlsParamType);
  begin
    S := Value;
    if FNlsParams[ParamType].Value <> S then begin
      FNlsParams[ParamType].Value := S;
      FNlsParams[ParamType].IsUserDefined := S <> '';
      if FNlsParams[ParamType].IsUserDefined then
        SetNlsParameter(FNlsParams[ParamType].Name, FNlsParams[ParamType].Value);
    end;
  end;

begin
  Result := True;
  case Prop of
    prThreadSafety: begin
      FThreadSafety := Boolean(Value);
    {$IFNDEF FPC}
      IsMultiThread := FThreadSafety;  // switch multithread memory managemant
    {$ENDIF}
    end;
    prConnectMode:
      FConnectMode := TConnectMode(Value);
    prEnableIntegers:
      FEnableIntegers := Boolean(Value);
    prEnableLargeint:
      FEnableLargeint := Boolean(Value);
    prEnableNumbers:
      FEnableNumbers := Boolean(Value);
    prEnableWideOraClob:
      FEnableWideOraClob := Boolean(Value);
    prInternalName:
      FInternalName := Value;
    prCharLength:
      if (FQueryCharLength and (Word(Value) <> 0))
        or (FCharLength <> Word(Value))
      then begin
        FQueryCharLength := Word(Value) = 0;
        FCharLength := Word(Value);
        FMaxStringSize := 0;
        if FCharLength = 0 then
          if FConnected then
            GetSessionParameters;
      end;
    prCharset: begin
      S := Value;
      if FCharset <> S then begin
        OldCharset := FCharset;
        OldCharsetId := FCharsetId;
        FCharset := S;
        try
          if FConnected then
            GetSessionParameters;
        except
          FCharset := OldCharset;
          FCharsetId := OldCharsetId;
          raise;
        end;
      end;
    end;
    prUseUnicode:
      FUseUnicode := Boolean(Value);
    prUnicodeEnvironment: begin
      if FUnicodeEnvironment <> Value then begin
        Disconnect;
        FUnicodeEnvironment := Value;
      end;
    end;
    prDateLanguage:
      SetNlsParam(nlsDateLanguage);
    prDateFormat:
      SetNlsParam(nlsDateFormat);
    prTimeStampFormat:
      SetNlsParam(nlsTimeStampFormat);
    prTimeStampTZFormat:
      SetNlsParam(nlsTimeStampTZFormat);
    prNumericCharacters:
      SetNlsParam(nlsNumericCharacters);
    prDisconnectMode:
      FDisconnectMode := Boolean(Value);
    prConnectionTimeOut:
      FConnectionTimeout := Value;
    prStatementCache:
      FStatementCache := Boolean(Value);
    prStatementCacheSize: begin
      FStatementCacheSize := Value;
      if GetConnected then
        SetStatementCacheSize(FStatementCacheSize);
    end;
    prDirect: begin
    {$IFDEF NET}
      FDirect := Value;
      if FDirect then
        SetOCICallStyle(OCI80);
    {$ELSE}
      if Value = True then
        raise Exception.Create(SDirectModeNotSupported);
    {$ENDIF}
    end;
    prHomeName: begin
      if FHomeName <> Value then begin
        Disconnect;
        FHomeName := Value;
      end;
    end;
    prIPVersion:
      FIPVersion := Value;
  {$IFNDEF LITE}
    prClientIdentifier: begin
      if Value <> FClientIdentifier then begin
        FClientIdentifier := Value;
        if GetConnected then
          SetClientIdentifier;
      end;
    end;
    prOptimizerMode: begin
      if TOptimizerMode(Value) <> FOptimizerMode then begin
        FOptimizerMode := TOptimizerMode(Value);
        if GetConnected then
          SetOptimizerMode;
      end;
    end;
    prSchema: begin
      if Value <> FSchema then begin
        FSchema := Value;
        if GetConnected then
          SetCurrentSchema(FSchema);
      end;
    end;
    prSubscriptionPort: begin
      if FSubscriptionPort <> Value then begin
        Disconnect;
        FSubscriptionPort := Value;
      end;
    end;
  {$ENDIF}
    prUseOCI7:
    {$IFDEF NET}
      if FDirect then
        SetOCICallStyle(OCI80)
      else
    {$ENDIF}
      begin
        if Boolean(Value) then
          SetOCICallStyle(OCI73)
        else if OCISvcCtx <> nil then
          SetOCICallStyle(OCISvcCtx.Home.OCICallStyle)
        else
          SetOCICallStyle(None);
      end;
  {$IFNDEF FPC}
    prEnableSQLTimeStamp:
      FEnableSQLTimeStamp := Value;
  {$ELSE}
    prTimeStampAsString:
      FTimeStampAsString := Value;
  {$ENDIF}
    prIntervalAsString:
      FIntervalAsString := Value;
    prSmallintPrecision:
      FSmallintPrecision := Value;
    prIntegerPrecision:
      FIntegerPrecision := Value;
    prFloatPrecision:
      FFloatPrecision := Value;
    prLargeintPrecision:
      FLargeIntPrecision := Value;
    prBCDPrecision:
      GetPrecAndScale(Value, FBCDPrecision, FBCDScale);
    prFmtBCDPrecision:
      GetPrecAndScale(Value, FFmtBCDPrecision, FFmtBCDScale);
    prUnicodeAsNational:
      FUnicodeAsNational := Value;

    prSSLServerCertDN:
      FSSLOptions.ServerCertDN := Value;

  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCIConnection.GetProp(Prop: integer; out Value: variant): boolean;

  procedure GetNlsParam(ParamType: TNlsParamType);
  begin
    if FNlsParams[ParamType].Value = '' then
      GetSessionParameters;
    Value := FNlsParams[ParamType].Value;
  end;

begin
  Result := True;
  case Prop of
    prMaxStringSize:
      Value := GetMaxStringSize;
    prCharLength:
      if FQueryCharLength then
        Value := 0
      else
        Value := FCharLength;
    prCharset:
      Value := FCharset;
    prUseUnicode:
      Value := FUseUnicode;
    prUnicodeEnvironment:
      Value := FUnicodeEnvironment;
    prDateFormat:
      GetNlsParam(nlsDateFormat);
    prDateLanguage:
      GetNlsParam(nlsDateLanguage);
    prTimeStampFormat:
      GetNlsParam(nlsTimeStampFormat);
    prTimeStampTZFormat:
      GetNlsParam(nlsTimeStampTZFormat);
    prNumericCharacters:
      GetNlsParam(nlsNumericCharacters);
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prDirect:
    {$IFDEF NET}
      Value := FDirect;
    {$ELSE}
      Value := False;
    {$ENDIF}
    prHomeName:
      Value := FHomeName;
    prIPVersion:
      Value := FIPVersion;
    prConnectMode:
      Value := Variant(FConnectMode);
  {$IFNDEF LITE}
    prSchema:
      Value := FSchema;
  {$ENDIF}

    prSSLServerCertDN:
      Value := FSSLOptions.ServerCertDN;

  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TOCIConnection.CheckIsValid: boolean;
begin
  if not FIsValid then
    Result := False
  else begin
    FIsValid := True;
    try
      if (OCISvcCtx.Home.PossibleOCICallStyles <> [OCI80]) and
        (OCISvcCtx.Home.OCIVersion >= 10200) and (FOracleVersion >= 10200)
      then
        Check(OCI8.OCIPing(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, OCI_DEFAULT))
      else
        FInternalTransaction.Commit;
    except
      FIsValid := False;
    end;
    Result := FIsValid;
  end;
end;

{$IFNDEF NODBACCESS}
procedure TOCIConnection.ReturnToPool;
begin
  Assert(FPool <> nil);
  FOnFailover := nil;
  inherited;
end;
{$ENDIF}

{$IFNDEF LITE}
procedure TOCIConnection.SetOptimizerMode;
var
  s: string;
  Mode: TOptimizerMode;
  Command: TCRCommand;
begin
  if (FOptimizerMode <> omDefault) then begin
    Mode := FOptimizerMode;
    if (GetOracleVersion < 9000) and (Mode in [omFirstRows1000..omFirstRows1]) then
      Mode := omFirstRows;
    case Mode of
      omFirstRows1000:    s := 'FIRST_ROWS_1000';
      omFirstRows100:     s := 'FIRST_ROWS_100';
      omFirstRows10:      s := 'FIRST_ROWS_10';
      omFirstRows1:       s := 'FIRST_ROWS_1';
      omFirstRows:        s := 'FIRST_ROWS';
      omAllRows:          s := 'ALL_ROWS';
      omChoose:           s := 'CHOOSE';
      omRule:             s := 'RULE';
    end;

    Command := GetCommand;
    try
      Command.SetSQL('ALTER SESSION SET OPTIMIZER_MODE = ' + s);
      Command.Execute;
    finally
      ReleaseCommand(Command);
    end;
  end;
end;

// used to set ClientIdentifier on active connection
procedure TOCIConnection.SetClientIdentifier;
var
  s: string;
  Command: TCRCommand;
begin
  s := TrimLeft(FClientIdentifier);
  if (Length(s) > 0) and (s[1] = ':') then
    RaiseError(SClientIdentifierFirstCharShouldNotBeColon);

  if GetOracleVersion < 9000 then
    Exit;

  Command := GetCommand;
  try
    Command.SetSQL('BEGIN DBMS_SESSION.SET_IDENTIFIER(:a); END;');
    Command.Params[0].SetDataType(dtString);
    Command.Params[0].SetParamType(pdInput);
    Command.Params[0].SetSize(Length(FClientIdentifier));
    Command.Params[0].Value := FClientIdentifier;
    try
      Command.Execute;
    except
      on E: EOraError do begin
        if EOraError(E).ErrorCode = 6550 then
          raise EOraError.Create( EOraError(E).ErrorCode,
            'Please execute GRANT EXECUTE ON DBMS_SESSION TO ' +
            FUsername + ' as SYS' + DALineSeparator + DALineSeparator + EOraError(E).Message);
        raise;
      end;
    end;
  finally
    ReleaseCommand(Command);
  end;
end;
{$ENDIF}

{ TOraCursor }

constructor TOraCursor.Create;
begin
  inherited Create;

  FState := csInactive;
  FphOCIStmt := Marshal.AllocHGlobal(sizeof(IntPtr));
  FphOCIStmt^ := nil;
  FStatementMode := smUnknown;
end;

destructor TOraCursor.Destroy;
begin
  FreeCursor;
  Marshal.FreeHGlobal(FphOCIStmt);

  inherited;
end;

procedure TOraCursor.SetOCISvcCtx(Value: TOCISvcCtx);
begin
  if FOCISvcCtx <> Value then begin
    ReleaseOCISvcCtx;

    FOCISvcCtx := Value;
    if FOCISvcCtx <> nil then begin
    {$IFNDEF AUTOREFCOUNT}
      FOCISvcCtx.AddRef;
    {$ENDIF}
      FOCI8 := FOCISvcCtx.OCI8
    end;
  end;
end;

procedure TOraCursor.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    OCI8.DoOraError(Status, OCISvcCtx);
end;

procedure TOraCursor.CheckOCI;
begin
  if not ((FOCICallStyle = OCI73) or (FOCICallStyle = OCI80)) then
    RaiseError(SCheckOCI);
end;

procedure TOraCursor.CheckOCI73;
begin
  if not (FOCICallStyle = OCI73) then
    RaiseError(SCheckOCI73);
end;

procedure TOraCursor.CheckOCI80;
begin
  if not (FOCICallStyle = OCI80) then
    RaiseError(SCheckOCI80);
end;

procedure TOraCursor.Init(AOCISvcCtx: TOCISvcCtx; AOCICallStyle: TOCICallStyle);
begin
  if (FOCISvcCtx <> AOCISvcCtx) or (FOCICallStyle <> AOCICallStyle) then begin
    FreeCursor;

    SetOCISvcCtx(AOCISvcCtx);
    FOCICallStyle := AOCICallStyle;
  end;
end;

procedure TOraCursor.ReleaseOCISvcCtx;
begin
  if FOCISvcCtx <> nil then begin
  {$IFNDEF AUTOREFCOUNT}
    FOCISvcCtx.ReleaseRef;
  {$ENDIF}
    FOCISvcCtx := nil;
    FOCI8 := nil;
  end;
end;

procedure TOraCursor.AllocCursor(StatementMode : TStatementMode = smAllocated);

  function NeedCursorHandle: Boolean;
  begin
    Result := {$IFDEF NET}OCISvcCtx.Home.Direct or{$ENDIF} (OCISvcCtx.Home.OCIVersion < 12000);
  end;

  procedure AllocCursorHandle;
  var
    OCIStmt: pOCIStmt;
  begin
    Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, OCIStmt, OCI_HTYPE_STMT, 0, nil));
    hOCIStmt := OCIStmt;
    FStatementMode := smAllocated;
  end;

begin
  if FOCICallStyle = None then
    FOCICallStyle := OCISvcCtx.Home.OCICallStyle;

  if FOCICallStyle = OCI73 then begin
    if IntPtr(FCDA) = nil then begin
      GetMem(FCDA, CDA_SIZE);
      FillChar(FCDA, CDA_SIZE, 0);
    end;
  end
  else
  if FOCICallStyle = OCI80 then begin
    if hOCIStmt = nil then begin
      if StatementMode = smAllocated then
        AllocCursorHandle
      else if (StatementMode <> smCached) and NeedCursorHandle then
        AllocCursorHandle
      else
        FStatementMode := StatementMode;
    end;
  end
  else
    CheckOCI;
end;

procedure TOraCursor.ReleaseCursor;
begin
  if hOCIStmt <> nil then begin
    if (OCISvcCtx.hOCISvcCtx <> nil) {$IFDEF NET}or OCISvcCtx.Home.Direct{$ENDIF} then
      case FStatementMode of
        smAllocated:
          Check(OCI8.OCIHandleFree(hOCIStmt, OCI_HTYPE_STMT));
        smPrepared:
          Check(OCI8.OCIStmtRelease(hOCIStmt, OCISvcCtx.hOCIError, nil, 0, OCI_DEFAULT));
        smCached:
          Check(OCI8.OCIStmtRelease(hOCIStmt, OCISvcCtx.hOCIError, nil, 0, OCI_STRLS_CACHE_DELETE));
      end;
    hOCIStmt := nil;
    FStatementMode := smUnknown;
  end;
end;

procedure TOraCursor.FreeCursor;
begin
  if IntPtr(FCDA) <> nil then begin
    CheckOCI73;
    FreeMem(FCDA);
    FCDA := nil;
  end;

  if hOCIStmt <> nil then begin
    CheckOCI80;
    ReleaseCursor;
  end;

  ReleaseOCISvcCtx;
end;

procedure TOraCursor.Disconnect;
begin
  FreeCursor;
end;

function TOraCursor.CanFetch: boolean;
begin
  Result := (State >= csExecuted) and (State < csFetched);
end;

function TOraCursor.GetCDA: PCDA;
begin
  CheckOCI73;

  if IntPtr(FCDA) = nil then
    AllocCursor;

  Result := FCDA;
end;

function TOraCursor.GethOCIStmt: pOCIStmt;
begin
  Result := pOCIStmt(FphOCIStmt^);
end;

procedure TOraCursor.SethOCIStmt(Value: pOCIStmt);
begin
  pOCIStmt(FphOCIStmt^) := Value;
  if pOCIStmt(FphOCIStmt^) <> nil then
    EnablePrefetching;
end;

function TOraCursor.GetOCIStmt: pOCIStmt;
begin
  CheckOCI80;

  if hOCIStmt = nil then
    AllocCursor;

  Result := hOCIStmt;
end;

function TOraCursor.GetOCIStmtPtr: ppOCIStmt;
begin
  CheckOCI80;

  if hOCIStmt = nil then
    AllocCursor;

  Result := FphOCIStmt;
end;

procedure TOraCursor.EnablePrefetching;
const
  MaxAutoPrefetch = 100;
begin
  if FPrefetchRows < 0 then
    DisablePrefetching
  else if FPrefetchRows = 0 then
    if FDefaultPrefetchRows > MaxAutoPrefetch then
      InitPrefetchRows(MaxAutoPrefetch + 1)
    else if FDefaultPrefetchRows > 0 then
      InitPrefetchRows(FDefaultPrefetchRows + 1)
    else
      InitPrefetchRows(1)
  else
    InitPrefetchRows(FPrefetchRows);
end;

procedure TOraCursor.DisablePrefetching;
begin
  // Turn off prefetching
  InitPrefetchRows(0);
end;

procedure TOraCursor.InitPrefetchRows(Value: integer);
begin
  if (OCISvcCtx = nil) or (hOCIStmt = nil) then
    Exit;

  if OCISvcCtx.Home.OCIVersion >= 9000 then begin
    Check(OCI8.OCIAttrSet2(hOCIStmt, OCI_HTYPE_STMT, Value, 0, OCI_ATTR_PREFETCH_ROWS, OCISvcCtx.hOCIError));
    if Value = 0 then
      Check(OCI8.OCIAttrSet2(hOCIStmt, OCI_HTYPE_STMT, Value, 0, OCI_ATTR_PREFETCH_MEMORY, OCISvcCtx.hOCIError)); // Disable prefetching
  end;
end;

procedure TOraCursor.SetPrefetchRows(Value: integer);
begin
  if FPrefetchRows <> Value then begin
    FPrefetchRows := Value;
    EnablePrefetching;
  end;
end;

procedure TOraCursor.SetDefaultFetchRows(Value: integer);
begin
  if FDefaultPrefetchRows <> Value then begin
    FDefaultPrefetchRows := Value;
    EnablePrefetching;
  end;
end;

{ TOraParam }

constructor TOraParamDesc.Create;
begin
  inherited Create;

  FDefIndicator := -1;
  FIndicator := @FDefIndicator;
  FArraySize := 1;
  FCanBeOutput := True;
end;

destructor TOraParamDesc.Destroy;
begin
  inherited;
end;

{ Memory management }

function TOraParamDesc.GetOCISvcCtx: TOCISvcCtx;
begin
  Result := FOwner.OCISvcCtx;
  if Result = nil then
    Result := FOwner.FConnection.OCISvcCtx;
end;

function TOraParamDesc.GetOCI8: TOCI8API;
begin
  Result := OCISvcCtx.OCI8;
end;

function TOraParamDesc.GetActualLength: integer;
begin
  Result := FActualTableLen;
end;

function TOraParamDesc.UnicodeIsUTF16: Boolean;
begin
  if OCISvcCtx.Home.Direct or (OCISvcCtx.Home.OCIVersion >= 12000) then
    Result := True
  else
    Result := False;
end;

procedure TOraParamDesc.SetActualLength(Value: integer);
begin
  FActualTableLen := Value;
end;

procedure TOraParamDesc.AllocBuffer;

  function CreateLob(LobDataType: integer): TOraLob;
  begin
    Result := TOraLob.Create(OCISvcCtx);
    if LobDataType in [dtBlob, dtOraBlob] then
      Result.LobType := ltBlob
    else if LobDataType in [dtMemo, dtOraClob] then
      Result.LobType := ltClob
    else if LobDataType in [dtWideMemo, dtWideOraClob, dtNClob] then
      Result.LobType := ltNClob;
  end;

var
  i: integer;
  Obj: TSharedObject;
  PValueArr: PVariantArray;
  ValuePtr: PVarData;
begin
  if FBufferAllocated or (FDataType = dtUnknown) then
    Exit;

  case FDataType of
    dtString, dtFixedChar:
      if FSize >= 1 then
        FValueSize := FSize + 1
      else
        FValueSize := 2;
    dtWideString, dtFixedWideChar:
      if FSize >= 1 then
        FValueSize := (FSize + 1) * SizeOf(WideChar)
      else
        FValueSize := 4;
    dtInt8, dtUInt8:
      FValueSize := sizeof(Byte);
    dtSmallint, dtWord:
      FValueSize := sizeof(SmallInt);
    dtInteger, dtUint32:
      FValueSize := sizeof(Integer);
    dtSingle:
      FValueSize := sizeof(Single);
    dtFloat, dtCurrency:
      FValueSize := sizeof(Double);
    dtExtended:
      FValueSize := sizeof(Extended);
    dtDateTime, dtDate, dtTime:
      FValueSize := 7;
    dtBoolean:
      FValueSize := 4; // as int
    dtCursor:
      FValueSize := 0;
    dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob, dtBFile, dtCFile:
      if FArraySize > 1 then
        FValueSize := SizeOf(pOCILobLocator)
      else
        FValueSize := 0;
    dtObject, dtReference, dtXML, dtAnyData, dtArray, dtTable:
      FValueSize := 0;
    dtLabel:
      FValueSize := 0;
    dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
      if FArraySize > 1 then
        FValueSize := SizeOf(pOCIDateTime)
      else
        FValueSize := 0;
    dtIntervalYM, dtIntervalDS:
      if FArraySize > 1 then
        FValueSize := SizeOf(pOCIInterval)
      else
        FValueSize := 0;
    dtBytes, dtVarBytes:
      FValueSize := FSize;
    dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64:
      if FArraySize > 1 then
        FValueSize := OCI_NUMBER_SIZE
      else
        FValueSize := 0;
  else
    Assert(False, SUnknownDataType);
  end;

  if FValueSize > 0 then
    FValueData := Marshal.AllocHGlobal(FValueSize * FArraySize);

  if FArraySize > 1 then begin
    if FHasValueLen then
      SetLength(FValueLens, FArraySize);

    FIndicators := Marshal.AllocHGlobal(sizeof(sb2) * FArraySize);
    System.FillChar(FIndicators^, sizeof(sb2) * FArraySize, $FF);
    FIndicator := FIndicators;

    // do not recreate array if assigned from DA level
    if (TVarData(FData).VType <> varValueArrayRef) and
       (FDataType in [dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
                      dtIntervalYM, dtIntervalDS,
                      dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64,
                      dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob])
    then begin
      New(PValueArr);
      SetLength(PValueArr^, FArraySize);
      TVarData(FData).VType := varObjectArrayRef;
      TVarData(FData).VPointer := PValueArr;
    end;
  end
  else begin
    if FHasValueLen then
      SetLength(FValueLens, 1);
    psb2(FIndicator)^ := -1;
  end;

  FBufferAllocated := True;

  if FDataType in [dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob] then  begin
    if FArraySize > 1 then begin
      PValueArr := TVarData(FData).VPointer;
      for i := 0 to FArraySize - 1 do begin
        ValuePtr := @PValueArr^[i];
        if (ValuePtr.VType <> varSharedObject) or (ValuePtr.VPointer = nil) then begin
          Obj := CreateLob(FDataType);
          ValuePtr.VType := varSharedObject;
          ValuePtr.VPointer := Obj;
        end;
      end;
    end
    else begin
      Obj := CreateLob(FDataType);
      try
        SetObject(Obj);
      finally
        Obj.Release;
      end;
    end;
  end
  else if FDataType in [dtBCD, dtFMTBCD, dtInt64, dtUInt64, dtSQLTimeStamp
                        ]
  then begin
    if FArraySize > 1 then begin
      PValueArr := TVarData(FData).VPointer;
      for i := 0 to FArraySize - 1 do begin
        ValuePtr := @PValueArr^[i];
        if (ValuePtr.VType <> varSharedObject) or (ValuePtr.VPointer = nil) then begin
          case FDataType of
            dtSQLTimeStamp:
              Obj := TOraTimeStamp.Create(OCISvcCtx, FDataType);
            dtIntervalYM,
            dtIntervalDS:
              Obj := TOraInterval.Create(OCISvcCtx, FDataType)
            else
              Obj := TOraNumber.Create(OCISvcCtx);
          end;
          ValuePtr.VType := varSharedObject;
          ValuePtr.VPointer := Obj;
        end;
      end;
    end
    else begin
      case FDataType of
        dtSQLTimeStamp:
          Obj := TOraTimeStamp.Create(OCISvcCtx, FDataType);
        dtIntervalYM,
        dtIntervalDS:
          Obj := TOraInterval.Create(OCISvcCtx, FDataType)
        else
          Obj := TOraNumber.Create(OCISvcCtx);
      end;
      try
        SetObject(Obj);
      finally
        Obj.Release;
      end;
    end;
  end;
end;

procedure TOraParamDesc.FreeBuffer;

  procedure FreeItems;
  var
    i: integer;
    PValueArr: PVariantArray;
    ValuePtr: PVarData;
    Obj: TSharedObject;
  begin
    PValueArr := TVarData(FData).VPointer;

    for i := 0 to Length(PValueArr^) - 1 do begin
      ValuePtr := @PValueArr^[i];
      if (ValuePtr.VType = varSharedObject) and (ValuePtr.VPointer <> nil) then begin
        Obj := TSharedObject(ValuePtr.VPointer);
        Obj.Free;
      end;
    end;

    TVarData(FData).VType := varEmpty;
    Dispose(PValueArr);
  end;

var
  Obj: TSharedObject;
begin
  if not FBufferAllocated then
    Exit;

  if FArraySize > 1 then
    if TVarData(FData).VType = varObjectArrayRef then
      FreeItems
    else
      TVarData(FData).VType := varEmpty;

  if FValueData <> nil then begin
    Marshal.FreeHGlobal(FValueData);
    FValueData := nil;
  end;

  SetLength(FValueLens, 0);

  if FIndicators <> nil then begin
    Marshal.FreeHGlobal(FIndicators);
    FIndicators := nil;
  end;
  FIndicator := @FDefIndicator;

  Obj := GetObject;
  if Obj <> nil then begin
    Obj.Free;
  {$IFNDEF FPC}
    FData := Unassigned;
  {$ELSE}
    FillByte(FData, SizeOf(TVarRec), 0);
  {$ENDIF}
  end;

  ClearBindData;

  FBufferAllocated := False;
end;

procedure TOraParamDesc.ClearBindData;
begin
  FHandle := nil;
  FBufferBinded := False;
end;

procedure TOraParamDesc.TrimFixedChar;
var
  i: integer;
  pValue: IntPtr;
  Ptr: PByte;
begin
  i := 0;
  pValue := FValueData;
  while True do begin
    Ptr := PtrOffset(pValue, FValueLens[i] - 1);
    while NativeUInt(Ptr) >= NativeUInt(pValue) do
      case Ptr^ of
        $00:
          Dec(Ptr);
        $20: begin
          Ptr^ := 0;
          Dec(Ptr);
        end;
        else
          Break;
      end;
    FValueLens[i] := PtrSubstract(Ptr, pValue) + 1;
    Inc(i);
    if i >= FArraySize then
      Exit;
    pValue := PtrOffset(pValue, FValueSize);
  end;
end;

procedure TOraParamDesc.TrimWideFixedChar;
var
  i: integer;
  pValue: IntPtr;
  Ptr: PWord;
begin
  i := 0;
  pValue := FValueData;
  if UnicodeIsUTF16 then
    while True do begin
      Ptr := PtrOffset(pValue, (FValueLens[i] - 1) * 2);
      while NativeUInt(Ptr) >= NativeUInt(pValue) do
        case Ptr^ of
          $0000:
            Dec(Ptr);
          $0020: begin
            Ptr^ := 0;
            Dec(Ptr);
          end;
          else
            Break;
        end;
      FValueLens[i] := (PtrSubstract(Ptr, pValue) shr 1) + 1;
      Inc(i);
      if i >= FArraySize then
        Exit;
      pValue := PtrOffset(pValue, FValueSize);
    end
  else
    while True do begin
      Ptr := PtrOffset(pValue, FValueLens[i] - 2);
      while NativeUInt(Ptr) >= NativeUInt(pValue) do
        case Ptr^ of
          $0000:
            Dec(Ptr);
          $2000: begin
            Ptr^ := 0;
            Dec(Ptr);
          end;
          else
            Break;
        end;
      FValueLens[i] := PtrSubstract(Ptr, pValue) + 2;
      Inc(i);
      if i >= FArraySize then
        Exit;
      pValue := PtrOffset(pValue, FValueSize);
    end;
end;

{$IFDEF LITE}

procedure TOraParamDesc.GetDbxBuffer(BufPtr: IntPtr; BufLen: Integer; out DataSize: Integer);
begin
  DataSize := 0;

  case GetDataType of
    dtString, dtFixedChar{, dtMemo}: begin
      DataSize := FValueLens[0];
      if DataSize > BufLen then
        DataSize := BufLen;
      Move(FValueData^, BufPtr^, DataSize);
      Marshal.WriteByte(BufPtr, DataSize, 0);
    end;
    dtWideString, dtFixedWideChar{, dtWideMemo}: begin
      DataSize := FValueLens[0];
      if DataSize > BufLen then
        DataSize := BufLen;
      Move(FValueData^, BufPtr^, DataSize * 2);
      Marshal.WriteUInt16(BufPtr, DataSize * 2, 0);
    end;
    dtBytes, dtVarBytes: begin
      DataSize := FValueLens[0];
      if DataSize > BufLen then
        DataSize := BufLen;
      Move(FValueData^, BufPtr^, DataSize);
    end;
    dtInt64, dtUInt64:
      Int64(BufPtr^) := TOraNumber(GetItemObject(0)).AsLargeInt;
    dtBCD:
      Currency(BufPtr^) := TOraNumber(GetItemObject(0)).AsFloat;
    dtFMTBCD:
      TBCD(BufPtr^) := TOraNumber(GetItemObject(0)).AsBCD;
    else
      Move(FValueData^, BufPtr^, FValueSize);
  end;
end;

procedure TOraParamDesc.SetDbxBuffer(DataPtr: IntPtr; DataSize: Cardinal);
var
  Len: Integer;
begin
  if GetDataType in [dtOraBlob, dtOraClob, dtWideOraClob, dtCursor, dtBFile, dtCFile,
                     dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
                     dtIntervalYM, dtIntervalDS,
                     dtNumber]
  then
    RaiseError(SParamIsNotStorable);

  case GetDataType of
    dtString, dtFixedChar{, dtMemo}: begin
      Len := Integer(DataSize) - 1; // zero terminator
      if Len > FSize then begin
        if Len > $FFFE then
          raise Exception.Create('Parameter ''' + FName + ''': value  is too long')
        else
          SetSize(Len);
        AllocBuffer;
      end;
      Move(DataPtr^, FValueData^, Len);
      FValueLens[0] := Word(Len);
    end;
    dtWideString, dtFixedWideChar{, dtWideMemo}: begin
      Len := (Integer(DataSize) shr 1) - 1; // zero terminator
      if Len > FSize then begin
        if Len > $7FFF then
          raise Exception.Create('Parameter ''' + FName + ''': value  is too long')
        else
          SetSize(Len);
        AllocBuffer;
      end;
      Move(DataPtr^, FValueData^, Len * 2);
      FValueLens[0] := Word(Len);
    end;
    dtInt64, dtUInt64:
      TOraNumber(GetItemObject(0)).AsLargeInt := Int64(DataPtr^);
    dtBCD:
      TOraNumber(GetItemObject(0)).AsFloat := Currency(DataPtr^);
    dtFMTBCD:
      TOraNumber(GetItemObject(0)).AsBCD := TBCD(DataPtr^);
    else
      Move(DataPtr^, FValueData^, FValueSize);
  end;
end;

{$ENDIF}

function TOraParamDesc.IndicatorPtr: IntPtr;
begin
  Result := FIndicator;
end;

class function TOraParamDesc.NeedAssignScalarValues: boolean;
begin
  Result := True;
end;

class function TOraParamDesc.NeedAssignObjectValues: boolean;
begin
  Result := True;
end;

function TOraParamDesc.GetMinDefaultSize: Integer;
begin
  Result := 1333;
end;

function TOraParamDesc.GetMaxStringSize(Connection: TCRConnection): Integer;
var
  OCIConnection: TOCIConnection;
begin
  OCIConnection := TOCIConnection(Connection);
  Result := OCIConnection.GetMaxStringSize;

  // old Oracle clients bug with Table parameters in UnicodeEnvironment
  if FTable and
     OCIConnection.UnicodeEnvironment and
     not OCIConnection.OCISvcCtx.Home.Direct and
     (OCIConnection.OCISvcCtx.Home.OCIVersion < 10200)
  then
    Result := 1999;
end;

procedure TOraParamDesc.SetDataType(Value: word);
begin
  if Value <> FDataType then begin
    FreeBuffer;

    if (Value in [dtString, dtFixedChar, dtMemo]) and
       (FOwner <> nil) and (FOwner.FConnection <> nil) and
       (OCISvcCtx.UnicodeEnv)
    then begin
    {$IFDEF LITE}
      FOriginalDataType := Value;
    {$ENDIF}
      case Value of
        dtString:
          FDataType := dtWideString;
        dtFixedChar:
          FDataType := dtFixedWideChar;
        dtMemo:
          FDataType := dtWideMemo;
      end;
    end
    else
      FDataType := Value;

    FHasValueLen := TOCIRecordSet.HasValueLen(FDataType);
  end;
end;

procedure TOraParamDesc.SetParamType(Value: TParamDirection);
var
  i: integer;
begin
  case Value of
    pdResult: begin
      for i := 0 to FOwner.Params.Count - 1 do
        if FOwner.Params[i].GetParamType = pdResult then
          FOwner.Params[i].SetParamType(pdOutput);
      inherited;
    end;
    pdOutput: begin
      if FParamType = pdResult then
        Exit
      else
        inherited;
    end;
    else
      inherited;
  end;
end;

procedure TOraParamDesc.SetSize(Value: integer);
begin
  if Value <> FSize then begin
    if FHasValueLen then
      FreeBuffer;

    FSize := Value;
  end;
end;

procedure TOraParamDesc.ValidateParamValue;
var
  i: Integer;
  StrArr: TStringArray;
begin
  // Oracle 8.0.5 and lower doesn't support AL16UTF16 encoding
  if OCISvcCtx.Home.OCIVersion < 9000 then
    if FDataType in [dtWideString, dtFixedWideChar] then
      if FBufferAllocated then
        if FArraySize > 1 then begin
          SetLength(StrArr, FArraySize);
          for i := 0 to FArraySize - 1 do
            StrArr[i] := string(GetItemAsWideString(i));
          FreeBuffer;
          if FDataType = dtFixedWideChar then
            SetDataType(dtFixedChar)
          else
            SetDataType(dtString);
          for i := 0 to FArraySize - 1 do
            SetItemAsAnsiString(i, AnsiString(StrArr[i]));
        end
        else begin
          SetLength(StrArr, 1);
          StrArr[0] := string(GetItemAsWideString(0));
          FreeBuffer;
          SetDataType(dtString);
          SetItemAsAnsiString(0, AnsiString(StrArr[0]));
        end
      else
        if FDataType = dtFixedWideChar then
          SetDataType(dtFixedChar)
        else
          SetDataType(dtString);
end;

procedure TOraParamDesc.SyncIndicator(Connection: TOCIConnection);
var
  Ind: sb2;
  Index: integer;
  PValueArr: PVariantArray;
  ts: TOraTimeStamp;
  int: TOraInterval;
  num: TOraNumber;
  lob: TOraLob;
  blob: TBlob;
begin
  if FArraySize > 1 then begin
    case FDataType of
      dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
      dtIntervalYM, dtIntervalDS,
      dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64,
      dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob: begin
        PValueArr := TVarData(FData).VPointer;

        for Index := 0 to FArraySize - 1 do
          case FDataType of
            dtOraBlob, dtOraClob, dtWideOraClob, dtNClob: begin
              lob := TOraLob(GetItemObject(Index));
              if lob <> nil then begin
                lob.OCISvcCtx := Connection.OCISvcCtx;
                CopyBuffer(lob.OCILobLocatorPtr, PtrOffset(FValueData, Index * FValueSize), FValueSize);
                if not lob.IsNull then
                  SetIndicator(Index, OCI_IND_NOTNULL)
                else
                  SetIndicator(Index, OCI_IND_NULL);
              end
              else
                SetIndicator(Index, OCI_IND_NULL);
            end;
            dtBlob, dtMemo, dtWideMemo: begin
              blob := TBlob(GetItemObject(Index));
              if blob <> nil then begin
                if not blob.IsNull then
                  SetIndicator(Index, OCI_IND_NOTNULL)
                else
                  SetIndicator(Index, OCI_IND_NULL);
              end
              else
                SetIndicator(Index, OCI_IND_NULL);
            end;
            dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
              ts := TOraTimeStamp(GetItemObject(Index));
              if (ts = nil) and (ParamType in [pdOutput, pdInputOutput, pdResult]) then begin
                ts := TOraTimeStamp.Create(OCISvcCtx, FDataType);
                PValueArr^[Index] := ts.ToVariant;
              end;
              if ts <> nil then begin
                ts.SetEnvironment(Connection.OCISvcCtx.Environment);
                Marshal.WriteIntPtr(FValueData, Index * FValueSize, ts.OCIDateTime);
                SetIndicator(Index, ts.FIndicatorPtr^);
              end
              else
                SetIndicator(Index, OCI_IND_NULL);
            end;
            dtIntervalYM, dtIntervalDS: begin
              int := TOraInterval(GetItemObject(Index));
              if (int = nil) and (ParamType in [pdOutput, pdInputOutput, pdResult]) then begin
                int := TOraInterval.Create(OCISvcCtx, FDataType);
                PValueArr^[Index] := int.ToVariant;
               end;
              if int <> nil then begin
                int.SetEnvironment(Connection.OCISvcCtx.Environment);
                Marshal.WriteIntPtr(FValueData, Index * FValueSize, int.OCIInterval);
                SetIndicator(Index, int.FIndicatorPtr^);
              end
              else
                SetIndicator(Index, OCI_IND_NULL);
            end;
            dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64: begin
              num := TOraNumber(GetItemObject(Index));
              if (num = nil) and (ParamType in [pdOutput, pdInputOutput, pdResult]) then begin
                num := TOraNumber.Create(OCISvcCtx);
                PValueArr^[Index] := num.ToVariant;
              end;
              if num <> nil then begin
                num.SetEnvironment(Connection.OCISvcCtx.Environment);
                CopyBuffer(num.OCINumberPtr, PtrOffset(FValueData, Index * FValueSize), FValueSize);
                if FDataType <> dtNumber then
                // FValue is freed on FreeBuffer.
                // For dtNumber param OCINumber buffer must exists after FreeBuffer
                // because OraNumber is used in TOraParam.ParamObject.
                  num.OCINumberPtr := PtrOffset(FValueData, Index * FValueSize);
                SetIndicator(Index, num.FIndicatorPtr^);
              end
              else
                SetIndicator(Index, OCI_IND_NULL);
            end;
          end;
      end;
    end;
  end
  else begin
    case FDataType of
      dtBlob, dtMemo, dtWideMemo,
      dtOraBlob, dtOraClob, dtWideOraClob,
      dtBFile, dtCFile,
      dtObject, dtReference, dtXML, dtAnyData, dtArray, dtTable, dtCursor,
      dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
      dtIntervalYM, dtIntervalDS,
      dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64: begin
        if TVarData(FData).VPointer = nil then
          Ind := -1
        else
          case FDataType of
            dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob:
              if TBlob(TVarData(FData).VPointer).IsNull then
                Ind := OCI_IND_NULL
              else
                Ind := OCI_IND_NOTNULL;
            dtBFile, dtCFile:
              if (TOraFile(TVarData(FData).VPointer).FileDir = '') and (TOraFile(TVarData(FData).VPointer).FileName = '') then
                Ind := OCI_IND_NULL
              else
                Ind := OCI_IND_NOTNULL;
            dtObject, dtReference, dtXML, dtArray, dtTable:
              if TOraObject(TVarData(FData).VPointer).Instance = nil then
                Ind := OCI_IND_NULL
              else
                Ind := OCI_IND_NOTNULL;
            dtAnyData:
              if TOraAnyData(TVarData(FData).VPointer).IsNull then
                Ind := OCI_IND_NULL
              else
                Ind := OCI_IND_NOTNULL;
            dtCursor:
              Ind := OCI_IND_NOTNULL;
          else
            Exit;
          end;
      end
    else
      if (FValueData = nil) then
        Ind := OCI_IND_NULL
      else
        Exit;
    end;

    SetIndicator(OCI_IND_NOTNULL, Ind)
  end;
end;

function TOraParamDesc.GetIndicator(Index: integer): smallint;
begin
  case FDataType of
    dtCursor:
      Result := 0; // strict for binding
    else begin
      CheckIndex(Index);
      Result := Marshal.ReadInt16(FIndicator, Index * sizeof(sb2));
    end;
  end;
end;

procedure TOraParamDesc.SetIndicator(Index: integer; Value: smallint);
begin
  CheckIndex(Index);
  Marshal.WriteInt16(FIndicator, Index * sizeof(sb2), Value);
end;

procedure TOraParamDesc.SetTable(Value: boolean);
begin
  FTable := Value;
end;

procedure TOraParamDesc.SetHasDefault(Value: boolean);
begin
  FHasDefault := Value;
end;

function TOraParamDesc.GetTable: boolean;
begin
  Result := FTable;
end;

function TOraParamDesc.GetLength: integer;
begin
  Result := FArraySize;
end;

function TOraParamDesc.GetHasDefault: boolean;
begin
  Result := FHasDefault;
end;

function TOraParamDesc.GetCanBeOutput: boolean;
begin
  Result := FCanBeOutput;
end;

{$IFDEF LITE}
function TOraParamDesc.GetOriginalDataType: word;
begin
  Result := FOriginalDataType;
end;
{$ENDIF}

function TOraParamDesc.GetItemAsDateTime(Index: integer): TDateTime;
var
  VPtr: IntPtr;
  OraTS: TOraTimeStamp;
begin
  if not GetItemNull(Index) then
    case FDataType of
      dtDate, dtTime, dtDateTime: begin
        VPtr := PtrOffset(FValueData, Index * FValueSize);
        Result := OraDateToDateTime(VPtr);
      end;
      dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
        if GetLength > 1 then
          OraTS := TOraTimeStamp(GetItemObject(Index))
        else
          OraTS := GetAsTimeStamp;
        Result := OraTS.AsDateTime;
      end;
      else
        Result := 0;
    end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsDateTime(Index: integer; Value: TDateTime);
var
  VPtr: IntPtr;
begin
  CheckIndex(Index);
  FDataType := dtDateTime;
  AllocBuffer;
  VPtr := PtrOffset(FValueData, Index * FValueSize);
  DateTimeToOraDate(Value, VPtr);
  SetItemNull(Index, False);
end;

function TOraParamDesc.GetItemAsFloat(Index: integer): double;
begin
  if not GetItemNull(Index) and (FDataType in [dtFloat, dtCurrency]) then begin
    Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FValueData, Index * FValueSize));
  end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsFloat(Index: integer; Value: double);
begin
  CheckIndex(Index);
  FDataType := dtFloat;
  AllocBuffer;

  Marshal.WriteInt64(FValueData, Index * FValueSize, BitConverter.DoubleToInt64Bits(Value));
  SetItemNull(Index, False);
end;

function TOraParamDesc.GetItemAsSingle(Index: integer): single;
begin
  if not GetItemNull(Index) and (FDataType = dtSingle) then begin
    Result := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(FValueData, Index * FValueSize));
  end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsSingle(Index: integer; Value: single);
begin
  CheckIndex(Index);
  FDataType := dtSingle;
  AllocBuffer;

  Marshal.WriteInt32(FValueData, Index * FValueSize, CRBitConverter.SingleToInt32Bits(Value));
  SetItemNull(Index, False);
end;

function TOraParamDesc.GetItemAsInteger(Index: integer): integer;
begin
  case FDataType of
    dtInt8:
      Result := ShortInt(Marshal.ReadByte(FValueData, Index * FValueSize));
    dtUInt8:
      Result := Byte(Marshal.ReadByte(FValueData, Index * FValueSize));
    dtInt16:
      Result := SmallInt(Marshal.ReadInt16(FValueData, Index * FValueSize));
    dtWord:
      Result := Word(Marshal.ReadInt16(FValueData, Index * FValueSize));
    dtInteger:
      Result := Integer(Marshal.ReadInt32(FValueData, Index * FValueSize));
    dtUInt32:
      Result := Cardinal(Marshal.ReadInt32(FValueData, Index * FValueSize));
    else
      Result := 0;
  end;
end;

procedure TOraParamDesc.SetItemAsInteger(Index: integer; Value: integer; DataType: Word = dtInteger);
begin
  CheckIndex(Index);

  FDataType := DataType;
  AllocBuffer;

  case FDataType of
    dtInt8, dtUInt8:
      Marshal.WriteByte(FValueData, Index * FValueSize, Byte(Value));
    dtInt16, dtWord:
      Marshal.WriteInt16(FValueData, Index * FValueSize, SmallInt(Value));
    dtInteger, dtUInt32:
      Marshal.WriteInt32(FValueData, Index * FValueSize, Integer(Value));
    else
      Assert(False);
  end;

  SetItemNull(Index, False);
end;

function TOraParamDesc.GetItemAsCurrency(Index: integer): currency;
begin
  if not GetItemNull(Index) and (FDataType = dtBCD) then begin
    if GetLength > 1 then
      Result := StrToCurr(TOraNumber(GetItemObject(Index)).AsString)
    else
      Result := StrToCurr(GetAsNumber.AsString);
  end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsCurrency(Index: integer; Value: currency);
var
  OraNumber: TOraNumber;
begin
  CheckIndex(Index);
  FDataType := dtBCD;
  AllocBuffer;

  if GetLength > 1 then
    OraNumber := TOraNumber(GetItemObject(Index))
  else
    OraNumber := GetAsNumber;

  Assert(OraNumber <> nil);
  OraNumber.AsString := CurrToStr(Value);
end;

function TOraParamDesc.GetItemAsLargeInt(Index: integer): Int64;
begin
  if not GetItemNull(Index) and (FDataType = dtLargeint) then begin
    if GetLength > 1 then
      Result := TOraNumber(GetItemObject(Index)).AsLargeInt
    else
      Result := GetAsNumber.AsLargeInt;
  end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsLargeInt(Index: integer; Value: Int64);
var
  OraNumber: TOraNumber;
begin
  CheckIndex(Index);
  FDataType := dtLargeint;
  AllocBuffer;

  if GetLength > 1 then
    OraNumber := TOraNumber(GetItemObject(Index))
  else
    OraNumber := GetAsNumber;

  Assert(OraNumber <> nil);
  OraNumber.AsLargeInt := Value;
end;

procedure TOraParamDesc.GetItemAsBcd(Index: integer; var Value: TBCD);
begin
  if not GetItemNull(Index) and (FDataType = dtFMTBCD) then begin
    if GetLength > 1 then
      Value := TOraNumber(GetItemObject(Index)).AsBCD
    else
      Value := GetAsNumber.AsBCD;
  end
  else
    Value := NullBcd;
end;

procedure TOraParamDesc.SetItemAsBcd(Index: integer; const Value: TBCD);
var
  OraNumber: TOraNumber;
begin
  CheckIndex(Index);
  FDataType := dtFMTBCD;
  AllocBuffer;

  if GetLength > 1 then
    OraNumber := TOraNumber(GetItemObject(Index))
  else
    OraNumber := GetAsNumber;

  Assert(OraNumber <> nil);
  OraNumber.AsBCD := Value;
end;

procedure TOraParamDesc.GetItemAsSQLTimeStamp(Index: integer; var Value: TSQLTimeStamp);
var
  VPtr: IntPtr;
  OraTS: TOraTimeStamp;
begin
  if not GetItemNull(Index) then
    case FDataType of
      dtDateTime: begin
        VPtr := PtrOffset(FValueData, Index * FValueSize);
        Value := DateTimeToSQLTimeStamp(OraDateToDateTime(VPtr));
      end;
      dtSQLTimeStamp, dtSQLTimeStampOffset, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
        if GetLength > 1 then
          OraTS := TOraTimeStamp(GetItemObject(Index))
        else
          OraTS := GetAsTimeStamp;
        OraTimeStampToSQLTimeStamp(OraTS, Value);
      end;
      else
        Value := NullSqlTimeStamp;
    end
  else
    Value := NullSqlTimeStamp;
end;

procedure TOraParamDesc.SetItemAsSQLTimeStamp(Index: integer; const Value: TSQLTimeStamp);
var
  OraTS: TOraTimeStamp;
begin
  CheckIndex(Index);
  if not (FDataType in [dtSQLTimeStamp, dtSQLTimeStampOffset, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ]) then
    FDataType := dtSQLTimeStamp;
  AllocBuffer;

  if GetLength > 1 then
    OraTS := TOraTimeStamp(GetItemObject(Index))
  else
    OraTS := GetAsTimeStamp;

  Assert(OraTS <> nil);
  SQLTimeStampToOraTimeStamp(OraTS, Value);
end;

procedure TOraParamDesc.GetItemAsSQLTimeStampOffset(Index: integer; var Value: TSQLTimeStampOffset);
var
  VPtr: IntPtr;
  OraTS: TOraTimeStamp;
begin
  if not GetItemNull(Index) then
    case FDataType of
      dtDateTime: begin
        VPtr := PtrOffset(FValueData, Index * FValueSize);
        Value := DateTimeToSQLTimeStampOffset(OraDateToDateTime(VPtr));
      end;
      dtSQLTimeStamp, dtSQLTimeStampOffset, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
        if GetLength > 1 then
          OraTS := TOraTimeStamp(GetItemObject(Index))
        else
          OraTS := GetAsTimeStamp;
        OraTimeStampToSQLTimeStampOffset(OraTS, Value);
      end;
      else
        Value := NullSQLTimeStampOffset;
    end
  else
    Value := NullSQLTimeStampOffset;
end;

procedure TOraParamDesc.SetItemAsSQLTimeStampOffset(Index: integer; const Value: TSQLTimeStampOffset);
var
  OraTS: TOraTimeStamp;
begin
  CheckIndex(Index);
  if not (FDataType in [dtSQLTimeStamp, dtSQLTimeStampOffset, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ]) then
    FDataType := dtSQLTimeStampOffset;
  AllocBuffer;

  if GetLength > 1 then
    OraTS := TOraTimeStamp(GetItemObject(Index))
  else
    OraTS := GetAsTimeStamp;

  Assert(OraTS <> nil);
  SQLTimeStampOffsetToOraTimeStamp(OraTS, Value);
end;

function TOraParamDesc.GetItemAsString(Index: integer): string;
begin
{$IFDEF IS_UNICODE}
  Result := GetItemAsWideString(Index);
{$ELSE}
  Result := GetItemAsAnsiString(Index);
{$ENDIF}
end;

procedure TOraParamDesc.SetItemAsString(Index: integer; const Value: string);
begin
{$IFDEF IS_UNICODE}
  SetItemAsWideString(Index, Value);
{$ELSE}
  SetItemAsAnsiString(Index, Value);
{$ENDIF}
end;

function TOraParamDesc.GetItemAsAnsiString(Index: integer): AnsiString;
var
  pData: IntPtr;
  pTempBuf: IntPtr;
  Len: integer;
begin
  if GetItemNull(Index) then begin
    Result := '';
    Exit;
  end;

  case FDataType of
    dtWideString, dtFixedWideChar:
      Result := AnsiString(GetItemAsWideString(Index));
    dtString, dtFixedChar: begin
      pData := PtrOffset(FValueData, Index * FValueSize);
      Len := FValueLens[Index];
      if FConvertEOL then begin
        pTempBuf := Marshal.AllocHGlobal(Len * 2);
        try
          Len := AddCRString(pData, Len, pTempBuf, Len * 2);
          Result := Marshal.PtrToStringAnsi(pTempBuf, Len);
        finally
          Marshal.FreeHGlobal(pTempBuf);
        end;
      end
      else
        Result := Marshal.PtrToStringAnsi(pData, Len);
    end;
  else
    Result := '';
  end;
end;

procedure TOraParamDesc.SetItemAsAnsiString(Index: integer; const Value: AnsiString);
var
  pData: IntPtr;
  Len: integer;
begin
  CheckIndex(Index);
  if FDataType <> dtFixedChar then
    FDataType := dtString;

  Len := LengthA(Value);
  if Len > FSize then
    if Len > $FFFE then
      raise Exception.Create('Parameter ''' + FName + ''': value  is too long')
    else
      SetSize(Len);
  AllocBuffer;

  if Len > 0 then begin
    pData := PtrOffset(FValueData, Index * FValueSize);
    if FConvertEOL then
      Len := RemoveCRString(PAnsiChar(Value), Len, pData, Len)
    else
      CopyBuffer(PAnsiChar(Value), pData, Len);
    Marshal.WriteByte(pData, Len, 0);

    FValueLens[Index] := Word(Len);
    SetItemNull(Index, False);
  end
  else begin
    FValueLens[Index] := 0;
    SetItemNull(Index, True);
  end;
end;

function TOraParamDesc.GetItemAsWideString(Index: integer): WideString;
var
  pData: IntPtr;
  pTempBuf: IntPtr;
  Len : integer;
begin
  case FDataType of
    dtString, dtFixedChar:
      Result := WideString(GetItemAsAnsiString(Index));
    dtWideString, dtFixedWideChar: begin
      pData := PtrOffset(FValueData, Index * FValueSize);
      Len := FValueLens[Index];
      if Len > 0 then
        if UnicodeIsUTF16 then
          if FConvertEOL then begin
            pTempBuf := Marshal.AllocHGlobal(Len shl 2); // Len * 4
            try
              Len := AddCRUnicode(pData, Len, pTempBuf, Len * 2);
              Result := Marshal.PtrToStringUni(pTempBuf, Len);
            finally
              Marshal.FreeHGlobal(pTempBuf);
            end;
          end
          else
            Result := Marshal.PtrToStringUni(pData, Len)
        else
          if FConvertEOL then begin
            pTempBuf := Marshal.AllocHGlobal(Len * 2);
            try
              Len := AddCRBigEndian(pData, Len shr 1, pTempBuf, Len);
              SetLength(Result, Len shr 1);
              CRFunctions.ConvertBigEndianBuffer(pTempBuf, PWideChar(Result), Len);
            finally
              Marshal.FreeHGlobal(pTempBuf);
            end;
          end
          else begin
            SetLength(Result, Len shr 1);
            CRFunctions.ConvertBigEndianBuffer(pData, PWideChar(Result), Len);
          end
      else
        Result := '';
    end
  else
    Result := '';
  end;
end;

procedure TOraParamDesc.SetItemAsWideString(Index: integer; const Value: WideString);
var
  pData: IntPtr;
  Len: integer;
begin
  CheckIndex(Index);
  if FDataType <> dtFixedWideChar then
    FDataType := dtWideString;

  Len := Length(Value);
  if Len > FSize then
    if Len > $7FFF then
      raise Exception.Create('Parameter ''' + FName + ''': value  is too long')
    else
      SetSize(Len);
  AllocBuffer;

  if Len > 0 then begin
    pData := PtrOffset(FValueData, Index * FValueSize);
    if UnicodeIsUTF16 then begin
      if FConvertEOL then
        Len := RemoveCRUnicode(PWideChar(Value), Len, pData, Len)
      else
        Move(Value[1], pData^, Len * 2);
      Marshal.WriteInt16(pData, Len * 2, 0);
      FValueLens[Index] := Word(Len);
    end
    else begin
      if FConvertEOL then begin
        Len := RemoveCRUnicode(PWideChar(Value), Len, pData, Len);
        Len := Len * 2;
        CRFunctions.ConvertBigEndianBuffer(pData, Len);
      end
      else begin
        Len := Len * 2;
        CRFunctions.ConvertBigEndianBuffer(PWideChar(Value), pData, Len);
      end;
      Marshal.WriteInt16(pData, Len, 0);
      FValueLens[Index] := Word(Len);
    end;

    SetItemNull(Index, False);
  end
  else begin
    FValueLens[Index] := 0;
    SetItemNull(Index, True);
  end;
end;

function TOraParamDesc.GetItemAsBoolean(Index: integer): boolean;
begin
  if not GetItemNull(Index) and (FDataType = dtBoolean) then begin
    Result := Boolean(Marshal.ReadInt32(FValueData, Index * FValueSize));
  end
  else
    Result := False;
end;

procedure TOraParamDesc.SetItemAsBoolean(Index: integer; Value: boolean);
begin
  CheckIndex(Index);
  FDataType := dtBoolean;
  AllocBuffer;
  Marshal.WriteInt32(FValueData, Index * FValueSize, Integer(Value));
  SetItemNull(Index, False);
end;

function TOraParamDesc.GetItemObject(Index: integer): TSharedObject;
var
  ItemData: Variant;
  PValueArr: PVariantArray;
begin
  CheckIndex(Index);

  if FArraySize > 1 then begin
    PValueArr := TVarData(FData).VPointer;
    ItemData := PValueArr^[Index];
  end
  else begin
    Assert(Index = 0);
    ItemData := FData;
  end;

  Result := TSharedObject.FromVariant(ItemData);
end;

procedure TOraParamDesc.SetItemObject(Index: integer; Value: TSharedObject);
var
  OldValue: TSharedObject;
  PValueArr: PVariantArray;
begin
  CheckIndex(Index);
  AllocBuffer;

  OldValue := GetItemObject(Index);
  if Value = OldValue then
    Exit;

  // Decrease RefCount for existing object
  if OldValue <> nil then
    OldValue.Release;

  if Value <> nil then begin
    if FArraySize > 1 then begin
      PValueArr := TVarData(FData).VPointer;
      PValueArr^[Index] := Value.ToVariant;
    end
    else begin
      Assert(Index = 0);
      FData := Value.ToVariant;
    end;
    // Increase RefCount for Value
    Value.AddRef;
  end
  else begin
    if FArraySize > 1 then begin
      PValueArr := TVarData(FData).VPointer;
      PValueArr^[Index] := Null;
    end
    else begin
      Assert(Index = 0);
      FData := Null;
    end;
  end;
end;

function TOraParamDesc.GetItemValue(Index: integer): variant;
var
  Len: word;
  Bcd: TBcd;
  TS: TSQLTimeStamp;
  TSO: TSQLTimeStampOffset;
begin
  CheckIndex(Index);

  if GetItemNull(Index) then begin
    Result := Null;
    Exit;
  end;

  case FDataType of
    dtBoolean:
      Result := GetItemAsBoolean(Index);
    dtString, dtFixedChar:
      Result := GetItemAsAnsiString(Index);
    dtWideString, dtFixedWideChar:
      Result := GetItemAsWideString(Index);
    dtInt8:
      Result := ShortInt(GetItemAsInteger(Index));
    dtUInt8:
      Result := Byte(GetItemAsInteger(Index));
    dtInt16:
      Result := SmallInt(GetItemAsInteger(Index));
    dtWord:
      Result := Word(GetItemAsInteger(Index));
    dtInteger:
      Result := GetItemAsInteger(Index);
    dtUInt32:
      Result := Cardinal(GetItemAsInteger(Index));
    dtFloat, dtCurrency:
      Result := GetItemAsFloat(Index);
    dtSingle:
      Result := GetItemAsSingle(Index);
    dtBCD:
      Result := GetItemAsCurrency(Index);
    dtInt64:
      Result := GetItemAsLargeInt(Index);
    dtUInt64:
      Result := UInt64(GetItemAsLargeInt(Index));
    dtFMTBCD: begin
      GetItemAsBcd(Index, Bcd);
      VarFMTBcdCreate(Result, Bcd);
    end;
    dtSQLTimeStamp: begin
      GetItemAsSQLTimeStamp(Index, TS);
      VarSQLTimeStampCreate(Result, TS);
    end;
    dtSQLTimeStampOffset: begin
      GetItemAsSQLTimeStampOffset(Index, TSO);
      VarSQLTimeStampOffsetCreate(Result, TSO);
    end;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
      Result := GetItemAsDateTime(Index);
    dtDate, dtTime, dtDateTime:
      Result := GetItemAsDateTime(Index);
    dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob:
      if TBlob(TVarData(FData).VPointer).IsUnicode then
        Result := TBlob(TVarData(FData).VPointer).AsWideString
      else
        Result := TBlob(TVarData(FData).VPointer).AsString;
    dtBytes, dtVarBytes: begin
      Len := FValueLens[Index];
      Result := VarArrayCreate([0, Len - 1], varByte);
      if Len > 0 then
        Move(PtrOffset(FValueData, Index * FValueSize)^, TVarData(Result).VArray.Data^, Len);
    end;
    dtUnknown:
      Result := Unassigned;
    else
      Result := Null;
  end;
end;

procedure TOraParamDesc.SetItemValue(Index: integer; const Value: variant);
var
  sa: AnsiString;
  sw: WideString;
  lw: Cardinal;
  i64: int64;
  Buf: TBytes;
  SafeArray: PVarArray;
  ItemObject: TSharedObject;
begin
  CheckIndex(Index);

  if TVarData(Value).VType in [varNull, varEmpty] then
    SetItemNull(Index, True)
  else begin
    if FDataType = dtUnknown then
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varShortInt:
          SetDataType(dtInteger);
        varLongWord, varInt64:
          SetDataType(dtLargeint);
        varSingle:
          SetDataType(dtSingle);
        varDouble, varCurrency:
          SetDataType(dtFloat);
        varDate:
          SetDataType(dtDateTime);
        varString: begin
          SetDataType(dtString);
          sa := AnsiString(Value);
          SetSize(LengthA(sa));
        end;
      else
        if VarIsStr(Value) then begin
          SetDataType(dtWideString);
          sw := WideString(Value);
          SetSize(Length(sw));
        end
        else
          raise EConvertError.Create(SUnknownDataType);
      end;

    AllocBuffer;
    case FDataType of
      dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob: begin
        case VarType(Value) of
          varSharedObject: begin
            ItemObject := TSharedObject.FromVariant(Value);
            if ItemObject = nil then
              SetItemNull(Index, True)
            else
              SetItemObject(Index, ItemObject);
          end;
          varString:
            TOraLob(GetItemObject(Index)).AsString := string(Value);
        {$IFDEF VER12P}varUString,{$ENDIF}
          varOleStr:
            TOraLob(GetItemObject(Index)).AsWideString := WideString(Value);
          varByte + varArray: begin begin
            SafeArray := VarArrayAsPSafeArray(Value);
            TOraLob(GetItemObject(Index)).Write(0, SafeArray.Bounds[0].ElementCount, SafeArray.Data);
          end;
          end;
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      end;
      dtBytes, dtVarBytes: begin
      {$IFNDEF VER9P}
        System.SetLength(Buf, 0); // to avoid warning
      {$ENDIF}
        if VarIsStr(Value) then begin
          Buf := Encoding.Default.GetBytes(VarToStr(Value));
          SetItemBinaryValue(Index, Buf, Length(Buf));
        end
        else begin
          SafeArray := VarArrayAsPSafeArray(Value);
          SetItemBinaryValue(Index, SafeArray.Data, SafeArray.Bounds[0].ElementCount);
        end;
      end;
      dtString, dtFixedChar:
        SetItemAsAnsiString(Index, AnsiString(Value));
      dtWideString, dtFixedWideChar:
        SetItemAsWideString(Index, WideString(Value));
      dtUInt8, dtWord, dtUInt32:
        case VarType(Value) of
          varByte, varSmallint, varInteger,
          varShortInt, varWord, varLongWord, varInt64, {$IFDEF VER12P}varUInt64,{$ENDIF}
          varSingle, varDouble, varCurrency: begin
            lw := Value; // to avoid Range Check error
            SetItemAsInteger(Index, Integer(lw), FDataType);
          end;
        else
          if VarIsStr(Value) then begin
            lw := StrToInt(Value);
            SetItemAsInteger(Index, Integer(lw), FDataType)
          end
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtInt8, dtSmallint, dtInteger:
        case VarType(Value) of
          varByte, varSmallint, varInteger,
          varShortInt, varWord, varLongWord, varInt64, {$IFDEF VER12P}varUInt64, {$ENDIF}
          varSingle, varDouble, varCurrency:
            SetItemAsInteger(Index, Integer(Value), FDataType);
        else
          if VarIsStr(Value) then
            SetItemAsInteger(Index, StrToInt(Value), FDataType)
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtSingle, dtFloat, dtCurrency:
        case VarType(Value) of
          varWord, varLongWord, varShortInt, varInt64,
          varSmallint, varInteger, varByte,
          varSingle, varDouble, varCurrency:
          if FDataType = dtSingle then
            SetItemAsSingle(Index, Value)
          else
            SetItemAsFloat(Index, Value);
        else
          if VarIsStr(Value) then
            if FDataType = dtSingle then
              SetItemAsSingle(Index, StrToFloat(Value))
            else
              SetItemAsFloat(Index, StrToFloat(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtBCD:
        case VarType(Value) of
          varWord, varLongWord, varShortInt, varInt64,
          varSmallint, varInteger, varByte,
          varSingle, varDouble, varCurrency:
            SetItemAsCurrency(Index, Value);
        else
          if VarIsStr(Value) then
            SetItemAsCurrency(Index, StrToCurr(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtInt64, dtUInt64:
        case VarType(Value) of
          varWord, varLongWord, varShortInt, varInt64,
          varSmallint, varInteger, varByte,
          varSingle, varDouble, varCurrency:
            SetItemAsLargeInt(Index, Value);
        else
          if VarIsStr(Value) then
            SetItemAsLargeInt(Index, StrToInt64(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtFMTBCD:
        // VarToBcd works correctly only for VarFMTBcd
        case VarType(Value) of
          varSmallint, varInteger, varByte, varWord, varShortInt:
            SetItemAsBcd(Index, IntegerToBcd(Value));
          varLongWord, varInt64: begin
            i64 := Value;
            SetItemAsBcd(Index, StrToBcd(IntToStr(i64)));
          end;
          varSingle, varDouble, varCurrency:
            SetItemAsBcd(Index, DoubleToBcd(Value));
        else
          if VarIsStr(Value) then
            SetItemAsBcd(Index, StrToBcd(Value))
          else
          if VarType(Value) = VarFMTBcd then
            SetItemAsBcd(Index, VarToBcd(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtDateTime, dtDate, dtTime:
        case VarType(Value) of
          varDate:
            SetItemAsDateTime(Index, Value);
          varDouble, varSingle, varCurrency:
            SetItemAsDateTime(Index, Double(Value));
        else
          if VarIsStr(Value) then
            SetItemAsDateTime(Index, StrToDateTime(Value))
          else if VarIsSQLTimeStamp(Value) then
            SetItemAsDateTime(Index, SQLTimeStampToDateTime(VarToSQLTimeStamp(Value)))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtBoolean:
        case VarType(Value) of
          varBoolean:
            SetItemAsBoolean(Index, Value);
          varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64,
          varSingle, varDouble, varCurrency:
            SetItemAsBoolean(Index, Value = 0);
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtSQLTimeStamp, dtSQLTimeStampOffset,
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
      dtIntervalYM, dtIntervalDS, dtNumber:
        case TVarData(Value).VType of
          varSharedObject : begin
            ItemObject := TSharedObject.FromVariant(Value);
            SetItemObject(Index, ItemObject);
          end;
          varDate: begin
            ItemObject := GetItemObject(Index);
            if ItemObject = nil then begin
              ItemObject := TOraTimeStamp.Create(OCISvcCtx, FDataType);
              try
                SetItemObject(Index, ItemObject);
              finally
                ItemObject.Release;
              end;
            end;
            TOraTimeStamp(ItemObject).AsDateTime := Value;
          end;
          else
            if VarIsSQLTimeStamp(Value) then begin
              ItemObject := GetItemObject(Index);
              if ItemObject = nil then begin
                ItemObject := TOraTimeStamp.Create(OCISvcCtx, FDataType);
                try
                  SetItemObject(Index, ItemObject);
                finally
                  ItemObject.Release;
                end;
              end;
              SQLTimeStampToOraTimeStamp(TOraTimeStamp(ItemObject), VarToSQLTimeStamp(Value));
            end
            else if VarIsSQLTimeStampOffset(Value) then begin
              ItemObject := GetItemObject(Index);
               if ItemObject = nil then begin
                 ItemObject := TOraTimeStamp.Create(OCISvcCtx, FDataType);
                 try
                   SetItemObject(Index, ItemObject);
                 finally
                   ItemObject.Release;
                 end;
               end;
               SQLTimeStampOffsetToOraTimeStamp(TOraTimeStamp(ItemObject), VarToSQLTimeStampOffset(Value));
            end
            else if VarIsStr(Value) then begin
              ItemObject := GetItemObject(Index);
              if ItemObject = nil then begin
                ItemObject := TOraTimeStamp.Create(OCISvcCtx, FDataType);
                try
                  SetItemObject(Index, ItemObject);
                finally
                  ItemObject.Release;
                end;
              end;
              if FDataType = dtTimeStampTZ then
                SetItemAsSQLTimeStampOffset(Index, StrToSQLTimeStampOffset(Value))
              else
                SetItemAsSQLTimeStamp(Index, StrToSQLTimeStamp(Value));
            end
            else
              raise EConvertError.Create(SCannotConvertType);
        end;
    else
      raise EConvertError.Create(SCannotConvertType);
    end;
  end;
end;

procedure TOraParamDesc.SetItemBinaryValue(Index: integer; DataPtr: IntPtr; DataSize: Integer);
begin
  if DataSize > FSize then
    if DataSize > $FFFE then
      raise Exception.Create('Parameter ''' + FName + ''': value  is too long')
    else
      SetSize(DataSize);
  AllocBuffer;

  if DataSize > 0 then begin
    Move(DataPtr^, PtrOffset(FValueData, Index * FValueSize)^, DataSize);
    FValueLens[Index] := DataSize;
    SetItemNull(Index, False);
  end
  else begin
    FValueLens[Index] := 0;
    SetItemNull(Index, True);
  end;
end;

{$IFNDEF LITE}

procedure TOraParamDesc.SetItemEncryptValue(Index: integer; const Value: variant);
var
  EncryptDataType: word;
begin
  if Assigned(FEncryptor) then begin
    case FDataType of
      dtFixedChar:
        EncryptDataType := dtString;
      dtFixedWideChar:
        EncryptDataType := dtWideString;
      dtFixedNChar:
        EncryptDataType := dtString;
      dtFixedNWideChar:
        EncryptDataType := dtWideString;
      dtNString:
        EncryptDataType := dtString;
      dtNWideString:
        EncryptDataType := dtWideString;
      else
        EncryptDataType := FDataType;
    end;

    SetItemValue(Index, EncryptValue(EncryptDataType, Value))
  end
  else
    SetItemValue(Index, Value);
end;

{$ENDIF}

function TOraParamDesc.GetValue: variant;
var
  i: integer;
begin
  if FArraySize > 1 then begin
    Result := VarArrayCreate([0, FArraySize - 1], varVariant);
    for i := 0 to FArraySize - 1 do
      Result[i] := GetItemValue(i);
  end
  else
    Result := GetItemValue(0);
end;

procedure TOraParamDesc.SetValue(const Value: variant);
var
  i: integer;
  ArraySize: Integer;
begin
  if (FDataType in [dtBytes, dtVarBytes]) and
     ((VarType(Value) = varArray or varByte) or VarIsStr(Value))
  then
  {$IFNDEF LITE}
    SetItemEncryptValue(0, Value)
  {$ELSE}
    SetItemValue(0, Value)
  {$ENDIF}
  else if TVarData(Value).VType = varValueArrayRef then
    SetValueArr(TVarData(Value).VPointer)
  else if VarIsArray(Value) then begin
    ArraySize := VarArrayHighBound(Value, 1) + 1;
    SetArraySize(ArraySize);
    for i := 0 to ArraySize - 1 do
    {$IFNDEF LITE}
      SetItemEncryptValue(i, Value);
    {$ELSE}
      SetItemValue(i, Value);
    {$ENDIF}
  end
  else
  {$IFNDEF LITE}
    SetItemEncryptValue(0, Value);
  {$ELSE}
    SetItemValue(0, Value);
  {$ENDIF}
end;

procedure TOraParamDesc.SetValueArr(PValueArr: PVariantArray);
var
  i: integer;
  ArraySize: Integer;
begin
  ArraySize := System.Length(PValueArr^);
  SetArraySize(ArraySize);
  for i := 0 to ArraySize - 1 do
  {$IFNDEF LITE}
    SetItemEncryptValue(i, PValueArr^[i]);
  {$ELSE}
    SetItemValue(i, PValueArr^[i]);
  {$ENDIF}
end;

procedure TOraParamDesc.SetObjectArr(PValueArr: PVariantArray);
var
  ArraySize: Integer;
begin
  ArraySize := System.Length(PValueArr^);
  SetArraySize(ArraySize);
  TVarData(FData).VType := varValueArrayRef;
  TVarData(FData).VPointer := PValueArr;
  AllocBuffer;
end;


function TOraParamDesc.IsObjectValue: boolean;
begin
  Result := FDataType in [dtNumber,
                          dtTimeStamp,
                          dtTimeStampTZ,
                          dtTimeStampLTZ,
                          dtIntervalYM,
                          dtIntervalDS,
                          dtMemo,
                          dtWideMemo,
                          dtBlob,
                          dtOraBlob,
                          dtOraClob,
                          dtWideOraClob,
                          dtNClob];
end;

function TOraParamDesc.GetObject: TSharedObject;
begin
  if VarType(FData) = varSharedObject then
    Result := TSharedObject.FromVariant(FData)
  else
    Result := nil;
end;

procedure TOraParamDesc.SetObject(Value: TSharedObject);
{$IFNDEF LITE}
var
  EncryptDataType: word;
{$ENDIF}
begin
  if Value = GetObject then
    Exit;

  FreeBuffer;

{$IFNDEF LITE}
  if Assigned(FEncryptor) then begin
    case FDataType of
      dtOraBlob:
        EncryptDataType := dtBlob;
      dtOraClob:
        EncryptDataType := dtMemo;
      dtWideOraClob, dtNClob:
        EncryptDataType := dtWideMemo;
      else
        EncryptDataType := FDataType;
    end;

    Value := EncryptObject(EncryptDataType, Value);
  end;
{$ENDIF}

  FData := Value.ToVariant;

  if Value <> nil then begin
    Value.AddRef;
    case FDataType of
      dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
        FIndicator := TOraTimeStamp(Value).FIndicatorPtr;
      dtIntervalYM, dtIntervalDS:
        FIndicator := TOraInterval(Value).FIndicatorPtr;
      dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64:
        FIndicator := TOraNumber(Value).FIndicatorPtr;
    end;
  end;

  FBufferAllocated := True;
end;

function TOraParamDesc.GetAsBlobRef: TBlob;
begin
  Result := TBlob(TVarData(FData).VPointer);
end;

function TOraParamDesc.GetAsCursor: TOraCursor;
begin
  Result := TOraCursor(TVarData(FData).VPointer);
end;

function TOraParamDesc.GetAsOraBlob: TOraLob;
begin
  Result := TOraLob(TVarData(FData).VPointer);
end;

function TOraParamDesc.GetAsBFile: TOraFile;
begin
  Result := TOraFile(TVarData(FData).VPointer);
end;

function TOraParamDesc.GetAsTimeStamp: TOraTimeStamp;
begin
  Result := TOraTimeStamp(TVarData(FData).VPointer);
end;

function TOraParamDesc.GetAsInterval: TOraInterval;
begin
  Result := TOraInterval(TVarData(FData).VPointer);
end;

function TOraParamDesc.GetAsNumber: TOraNumber;
begin
  Result := TOraNumber(TVarData(FData).VPointer);
end;

function TOraParamDesc.GetNull: boolean;
begin
  Result := GetItemNull(0);
end;

procedure TOraParamDesc.SetNull(const Value: boolean);
begin
  SetItemNull(0, Value);
end;

function TOraParamDesc.GetItemNull(Index: integer): boolean;
var
  Obj: TSharedObject;
begin
  case FDataType of
    dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
      Obj := GetItemObject(Index);
      if Obj <> nil then
        Result := TOraTimeStamp(Obj).IsNull
      else
        Result := True;
    end;
    dtIntervalYM, dtIntervalDS: begin
      Obj := GetItemObject(Index);
      if Obj <> nil then
        Result := TOraInterval(Obj).IsNull
      else
        Result := True;
    end;
    dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64: begin
      Obj := GetItemObject(Index);
      if Obj <> nil then
        Result := TOraNumber(Obj).IsNull
      else
        Result := True;
    end;
    dtObject, dtReference, dtXML, dtArray, dtTable: begin
      Obj := GetItemObject(Index);
      if Obj <> nil then
        Result := TOraObject(Obj).IsNull
      else
        Result := True;
    end;
    dtAnyData: begin
      Obj := GetItemObject(Index);
      if Obj <> nil then
        Result := TOraAnyData(Obj).IsNull
      else
        Result := True;
    end;
    else
      Result := GetIndicator(Index) < 0;
  end;
end;

procedure TOraParamDesc.SetItemNull(Index: integer; Value: boolean);
var
  Obj: TSharedObject;
begin
  AllocBuffer;

  if Value then begin
    SetIndicator(Index, -1);

    case FDataType of
      dtString, dtFixedChar: begin
        Marshal.WriteByte(FValueData, Index * FValueSize, 0); // zero terminator
        FValueLens[Index] := 0;
      end;
      dtWideString, dtFixedWideChar: begin
        Marshal.WriteUInt16(FValueData, Index * FValueSize, 0); // zero terminator
        FValueLens[Index] := 0;
      end;
      dtBytes, dtVarBytes:
        FValueLens[Index] := 0;
      dtInteger:
        Marshal.WriteInt32(FValueData, Index * FValueSize, 0);
      dtSingle:
        Marshal.WriteInt32(FValueData, Index * FValueSize, 0);
      dtFloat:
        Marshal.WriteInt64(FValueData, Index * FValueSize, 0);
      dtDate, dtTime, dtDateTime:
        FillChar(PtrOffset(FValueData, Index * FValueSize), FValueSize, 0);
      dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
        Obj := GetItemObject(Index);
        if Obj <> nil then
          TOraTimeStamp(Obj).IsNull := True;
      end;
      dtIntervalYM, dtIntervalDS: begin
        Obj := GetItemObject(Index);
        if Obj <> nil then
          TOraInterval(Obj).IsNull := True;
      end;
      dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64: begin
        Obj := GetItemObject(Index);
        if Obj <> nil then
          TOraNumber(Obj).IsNull := True
      end;
      dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob: begin
        Obj := GetItemObject(Index);
        if Obj <> nil then
          TBlob(Obj).Clear;
      end;
      dtObject, dtReference, dtXML, dtAnyData, dtArray, dtTable: begin
        Obj := GetItemObject(Index);
        if Obj <> nil then
          TOraObject(Obj).IsNull := True;
      end;
    end;
  end
  else
    SetIndicator(Index, 0);
end;

{$IFDEF MSWINDOWS}

{ TCustomNotifyChanges }

constructor TCustomNotifyChanges.Create(AOCISvcCtx: TOCISvcCtx; AOCIColl: pOCIColl);
var
  Count, i, Exists: integer;
  Ind, Elem: IntPtr;
begin
  inherited Create;

  FOCISvcCtx := AOCISvcCtx;

  FOCISvcCtx.OCI8.Check(FOCISvcCtx.OCI8.OCICollSize(FOCISvcCtx.hOCIEnv, FOCISvcCtx.hOCIError, AOCIColl, Count), FOCISvcCtx);

  SetLength(FItems, Count);

  for i := 0 to Count - 1 do begin
    FOCISvcCtx.OCI8.Check(FOCISvcCtx.OCI8.OCICollGetElem(FOCISvcCtx.hOCIEnv, FOCISvcCtx.hOCIError, AOCIColl, i, Exists, Elem, Ind), FOCISvcCtx);

    FItems[i] := CreateItem(Marshal.ReadIntPtr(Elem));
  end;
end;

destructor TCustomNotifyChanges.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FItems) - 1 do
    FItems[i].Free;

  inherited Destroy;
end;

function TCustomNotifyChanges.GetCount: integer;
begin
  Result := Length(FItems);
end;

{ TNotifyRowChange }

constructor TNotifyRowChange.Create(AOCISvcCtx: TOCISvcCtx; AChangeDescriptor: IntPtr);
var
  Len, Flags: integer;
  Ptr, ValuePtr, StrPtr: IntPtr;
begin
  inherited Create;

  StrPtr := nil;
  ValuePtr := @StrPtr;
  Ptr := @Len;
  AOCISvcCtx.OCI8.Check(AOCISvcCtx.OCI8.OCIAttrGet1(AChangeDescriptor, OCI_DTYPE_ROW_CHDES, ValuePtr, Ptr,
    OCI_ATTR_CHDES_ROW_ROWID, AOCISvcCtx.hOCIError), AOCISvcCtx);
  FRowId := PtrToStringOCI(StrPtr, Len, AOCISvcCtx.UnicodeEnv);

  AOCISvcCtx.OCI8.Check(AOCISvcCtx.OCI8.OCIAttrGet2(AChangeDescriptor, OCI_DTYPE_ROW_CHDES, Flags, nil,
    OCI_ATTR_CHDES_ROW_OPFLAGS, AOCISvcCtx.hOCIError), AOCISvcCtx);

  FOperations := [];
  if Flags and OCI_OPCODE_INSERT <> 0 then
    Include(FOperations, cnoInsert);
  if Flags and OCI_OPCODE_UPDATE <> 0 then
    Include(FOperations, cnoUpdate);
  if Flags and OCI_OPCODE_DELETE <> 0 then
    Include(FOperations, cnoDelete);
end;

{ TNotifyRowChanges }

function TNotifyRowChanges.CreateItem(ChangeDescriptor: IntPtr): TObject;
begin
  Result := TNotifyRowChange.Create(FOCISvcCtx, ChangeDescriptor);
end;

function TNotifyRowChanges.GetChanges(Index: integer): TNotifyRowChange;
begin
  Result := TNotifyRowChange(FItems[Index]);
end;

{ TNotifyTableChange }

constructor TNotifyTableChange.Create(AOCISvcCtx: TOCISvcCtx; AChangeDescriptor: IntPtr);
var
  Len, Flags: integer;
  OCIRowChanges: pOCIColl;
  Ptr, ValuePtr, StrPtr: IntPtr;
begin
  inherited Create;

  StrPtr := nil;
  ValuePtr := @StrPtr;
  Ptr := @Len;
  AOCISvcCtx.OCI8.Check(AOCISvcCtx.OCI8.OCIAttrGet1(AChangeDescriptor, OCI_DTYPE_TABLE_CHDES, ValuePtr, Ptr,
    OCI_ATTR_CHDES_TABLE_NAME, AOCISvcCtx.hOCIError), AOCISvcCtx);
  FTableName := PtrToStringOCI(StrPtr, Len, AOCISvcCtx.UnicodeEnv);

  AOCISvcCtx.OCI8.Check(AOCISvcCtx.OCI8.OCIAttrGet2(AChangeDescriptor, OCI_DTYPE_TABLE_CHDES, Flags, nil,
    OCI_ATTR_CHDES_TABLE_OPFLAGS, AOCISvcCtx.hOCIError), AOCISvcCtx);

  FOperations := [];
  if Flags and OCI_OPCODE_INSERT <> 0 then
    Include(FOperations, cnoInsert);
  if Flags and OCI_OPCODE_UPDATE <> 0 then
    Include(FOperations, cnoUpdate);
  if Flags and OCI_OPCODE_DELETE <> 0 then
    Include(FOperations, cnoDelete);
  if Flags and OCI_OPCODE_ALLROWS <> 0 then
    Include(FOperations, cnoAllRows);
  if Flags and OCI_OPCODE_ALTER <> 0 then
    Include(FOperations, cnoAlter);
  if Flags and OCI_OPCODE_DROP <> 0 then
    Include(FOperations, cnoDrop);

  if not (cnoAllRows in FOperations) then begin
    AOCISvcCtx.OCI8.Check(AOCISvcCtx.OCI8.OCIAttrGet1(AChangeDescriptor, OCI_DTYPE_TABLE_CHDES, @OCIRowChanges, nil,
      OCI_ATTR_CHDES_TABLE_ROW_CHANGES, AOCISvcCtx.hOCIError), AOCISvcCtx);

    FRowChanges := TNotifyRowChanges.Create(AOCISvcCtx, OCIRowChanges);
  end
  else
    FRowChanges := nil;
end;

destructor TNotifyTableChange.Destroy;
begin
  FRowChanges.Free;

  inherited Destroy;
end;

{ TNotifyTableChanges }

function TNotifyTableChanges.CreateItem(ChangeDescriptor: IntPtr): TObject;
begin
  Result := TNotifyTableChange.Create(FOCISvcCtx, ChangeDescriptor);
end;

function TNotifyTableChanges.GetChanges(Index: integer): TNotifyTableChange;
begin
  Result := TNotifyTableChange(FItems[Index]);
end;

{ TNotifyChange }

constructor TNotifyChange.Create(AOCISvcCtx: TOCISvcCtx; AChangeDescriptor: IntPtr);
var
  OCINotifyType: integer;
  OCITableChanges: pOCIColl;
begin
  inherited Create;

  // Get the Notification Type
  AOCISvcCtx.OCI8.Check(AOCISvcCtx.OCI8.OCIAttrGet2(AChangeDescriptor, OCI_DTYPE_CHDES, OCINotifyType, nil,
    OCI_ATTR_CHDES_NFYTYPE, AOCISvcCtx.hOCIError), AOCISvcCtx);
  case OCINotifyType of
    OCI_EVENT_NONE:
      FNotifyType := cneNone;
    OCI_EVENT_STARTUP:
      FNotifyType := cneStartup;
    OCI_EVENT_SHUTDOWN:
      FNotifyType := cneShutdown;
    OCI_EVENT_SHUTDOWN_ANY:
      FNotifyType := cneShutdownAny;
    OCI_EVENT_DROP_DB:
      FNotifyType := cneDropDB;
    OCI_EVENT_DEREG:
      FNotifyType := cneDereg;
    OCI_EVENT_OBJCHANGE:
      FNotifyType := cneObjChange;
    OCI_EVENT_QUERYCHANGE:
      FNotifyType := cneQueryChange;
  else
    Assert(False);
    FNotifyType := cneNone;
  end;

  if OCINotifyType in [OCI_EVENT_OBJCHANGE, OCI_EVENT_QUERYCHANGE] then begin
    AOCISvcCtx.OCI8.Check(AOCISvcCtx.OCI8.OCIAttrGet1(AChangeDescriptor, OCI_DTYPE_CHDES, @OCITableChanges, nil,
      OCI_ATTR_CHDES_TABLE_CHANGES, AOCISvcCtx.hOCIError), AOCISvcCtx);

    FTableChanges := TNotifyTableChanges.Create(AOCISvcCtx, OCITableChanges);
  end
  else
    FTableChanges := nil;
end;

destructor TNotifyChange.Destroy;
begin
  FTableChanges.Free;

  inherited Destroy;
end;

{ TOCIChangeNotification }

constructor TOCIChangeNotification.Create;
begin
  inherited Create;

  FOCISvcCtx := nil;
  FEnabled := True;
  FQueryResultOnly := False;
  FOperations := [cnoInsert, cnoUpdate, cnoDelete];
end;

destructor TOCIChangeNotification.Destroy;
begin
  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

function TOCIChangeNotification.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

function TOCIChangeNotification.GetOCI8: TOCI8API;
begin
  Result := FOCISvcCtx.OCI8
end;

procedure TOCIChangeNotification.SetOCISvcCtx(Value: TOCISvcCtx);
begin
  if FOCISvcCtx <> Value then begin
    ReleaseOCISvcCtx;

    FOCISvcCtx := Value;
    if FOCISvcCtx <> nil then
      FOCISvcCtx.AddRef;
  end;
end;

procedure TOCIChangeNotification.ReleaseOCISvcCtx;
begin
  if FOCISvcCtx <> nil then begin
    FOCISvcCtx.ReleaseRef;
    FOCISvcCtx := nil;
  end;
end;

procedure TOCIChangeNotification.Register(Connection: TOCIConnection);
var
  hOCISubscr: pOCISubscription;
  AttrValue: integer;
begin
  SetOCISvcCtx(Connection.OCISvcCtx);
  try
  {$IFDEF NET}
    if OCISvcCtx.Home.Direct then
      RaiseError(SChangeNotifyNotSupportedWithDirect);
  {$ENDIF}

    if OCISvcCtx.Home.OCIVersion < 10200 then
      RaiseError(SChangeNotifyOCIVersion);

    if Connection.GetOracleVersion < 10200 then
      RaiseError(SChangeNotifyServerVersion);

    OCI8.Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hOCISubscr, OCI_HTYPE_SUBSCRIPTION, 0, nil), FOCISvcCtx);
    try
      AttrValue := OCI_SUBSCR_NAMESPACE_DBCHANGE;
      Connection.OCI8.Check(OCI8.OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, SizeOf(ub4),
        OCI_ATTR_SUBSCR_NAMESPACE, OCISvcCtx.hOCIError), FOCISvcCtx);

      Connection.OCI8.Check(OCI8.OCIAttrSet1(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, OCICallbackChangeNotifyPtr,  0,
        OCI_ATTR_SUBSCR_CALLBACK, OCISvcCtx.hOCIError), FOCISvcCtx);

      AttrValue := 1;
      Connection.OCI8.Check(OCI8.OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, SizeOf(ub4),
        OCI_ATTR_CHNF_ROWIDS, OCISvcCtx.hOCIError), FOCISvcCtx);

      if FOperations = [] then
        raise Exception.Create(SChangeNotifyOperationsCannotBeEmpty);
      AttrValue := 0;
      if FOperations <> [cnoInsert, cnoUpdate, cnoDelete] then begin // default
        if cnoInsert in FOperations then
          AttrValue := OCI_OPCODE_INSERT;
        if cnoUpdate in FOperations then
          AttrValue := AttrValue or OCI_OPCODE_UPDATE;
        if cnoUpdate in FOperations then
          AttrValue := AttrValue or OCI_OPCODE_DELETE;
      end;
      OCI8.Check(OCI8.OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, 0,
        OCI_ATTR_CHNF_OPERATIONS, OCISvcCtx.hOCIError), FOCISvcCtx);

      OCI8.Check(OCI8.OCIAttrSet1(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, GCHandle, 0,
        OCI_ATTR_SUBSCR_CTX, FOCISvcCtx.hOCIError), FOCISvcCtx);

      OCI8.Check(OCI8.OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, FTimeout, 0,
        OCI_ATTR_SUBSCR_TIMEOUT, OCISvcCtx.hOCIError), FOCISvcCtx);

      if FQueryResultOnly then
        AttrValue := OCI_SUBSCR_CQ_QOS_QUERY
      else
        AttrValue := 0;
      OCI8.Check(OCI8.OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, 0,
        OCI_ATTR_SUBSCR_CQ_QOSFLAGS, OCISvcCtx.hOCIError), FOCISvcCtx);

      if FPersistent then
        AttrValue := OCI_SUBSCR_QOS_RELIABLE
      else
        AttrValue := 0;
      OCI8.Check(OCI8.OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, 0,
        OCI_ATTR_SUBSCR_QOSFLAGS, OCISvcCtx.hOCIError), FOCISvcCtx);

      if hODACWindow = 0 then
        AllocODACWnd;

      OCI8.Check(OCI8.OCISubscriptionRegister(OCISvcCtx.hOCISvcCtx, hOCISubscr, 1,
        OCISvcCtx.hOCIError, OCI_DEFAULT), FOCISvcCtx);
    except
      OCI8.OCIHandleFree(hOCISubscr, OCI_HTYPE_SUBSCRIPTION);
      raise;
    end;

    hOCISubscription := hOCISubscr;
  except
    ReleaseOCISvcCtx;
    raise;
  end;
end;

procedure TOCIChangeNotification.Unregister(Connection: TOCIConnection);
begin
  if hOCISubscription <> nil then
  try
    Connection.OCI8.OCISubscriptionUnRegister(OCISvcCtx.hOCISvcCtx, hOCISubscription,
      OCISvcCtx.hOCIError, OCI_DEFAULT);

    Connection.OCI8.OCIHandleFree(hOCISubscription, OCI_HTYPE_SUBSCRIPTION);

    hOCISubscription := nil;
  finally
    ReleaseOCISvcCtx;
  end;
end;

function TOCIChangeNotification.GetSubscriptionHandle(Connection: TOCIConnection): pOCISubscription;
begin
  if hOCISubscription = nil then
    Register(Connection);
  Result := hOCISubscription;
end;

function TOCIChangeNotification.IsActive: boolean;
begin
  Result := hOCISubscription <> nil;
end;

procedure TOCIChangeNotification.SetEnabled(Value: boolean);
begin
  if hOCISubscription <> nil then begin
    if FEnabled and not Value then
      OCI8.Check(OCI8.OCISubscriptionDisable(hOCISubscription, OCISvcCtx.hOCIError, OCI_DEFAULT), FOCISvcCtx);
    if not FEnabled and Value then
      OCI8.Check(OCI8.OCISubscriptionEnable(hOCISubscription, OCISvcCtx.hOCIError, OCI_DEFAULT), FOCISvcCtx);
  end;
  FEnabled := Value;
end;

function OCICallbackChangeNotify(pCtx: IntPtr; pSubscrHp: pOCISubscription;
  pPayload: IntPtr; iPayloadLen: ub4; pDescriptor: IntPtr; iMode: ub4): sword; cdecl;
begin
  Result := TOCIChangeNotification(GetGCHandleTarget(pCtx)).CallbackChangeNotify(pCtx,
    pSubscrHp, pPayload, iPayloadLen, pDescriptor, iMode);
end;

function TOCIChangeNotification.CallbackChangeNotify(pCtx: IntPtr; pSubscrHp: pOCISubscription;
  pPayload: IntPtr; iPayloadLen: ub4; pDescriptor: IntPtr; iMode: ub4): sword;
var
  NotifyChange: TNotifyChange;
begin
  NotifyChange := TNotifyChange.Create(FOCISvcCtx, pDescriptor);

  if NotifyChange.NotifyType = cneDereg then begin
    OCI8.OCIHandleFree(hOCISubscription, OCI_HTYPE_SUBSCRIPTION);
    hOCISubscription := nil;
  end;

  if Assigned(FOnChange) then
    PostMessage(hODACWindow, WM_CHANGENOTIFY, WPARAM(GCHandle),
      LPARAM(AllocGCHandle(NotifyChange)));

  Result := OCI_SUCCESS;
end;

function TOCIChangeNotification.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prEnabled:
      SetEnabled(Boolean(Value));
    prTimeout:
      FTimeout := Value;
    prPersistent:
      FPersistent := Boolean(Value);
    prQueryResultOnly:
      FQueryResultOnly := Boolean(Value);
    prOperations:
      FOperations := TChangeNotifyDMLOperations(Byte(Value));
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TOCIChangeNotification.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;
{$ENDIF}

{ TOCICommand }

constructor TOCICommand.Create;
begin
  inherited Create;

  FOCICallStyle := None;
  FCursor := TOraCursor.Create;
  FCursorRef := FCursor;
  SetCursorState(csInactive);
  FAutoCommit := True;
  FSQLType := SQL_UNKNOWN;
  FCacheLobs := True;
  FCheckParamHasDefault := True;

  FSmallintPrecision := -1;
  FIntegerPrecision := -1;
  FLargeIntPrecision := -1;
  FFloatPrecision := -1;
  FBCDPrecision := -1;
  FBCDScale := 0;
  FFmtBCDPrecision := -1;
  FFmtBCDScale := 0;
{$IFDEF FPC}
  FRawAsString := True;
{$ENDIF}

  FLock := TCriticalSection.Create;
end;

destructor TOCICommand.Destroy;
begin
{$IFDEF MSWINDOWS}
  if hExecThread <> nil then begin
    if FConnection <> nil then
      FConnection.StopThread(hExecThread, True);
  end
  else
{$ENDIF}
    if GetCursorState > csInactive then
      Close;

  FLock.Free;

  Assert(FCursorRef <> nil);

  if (FCursorRef <> FCursor) then
    FCursorRef.Free;

  FCursor.Free;

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  if FNullBuf <> nil then
    Marshal.FreeHGlobal(FNullBuf);

  inherited;
end;

procedure TOCICommand.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure TOCICommand.AllocOCISvcCtx;
begin
  if FOCISvcCtx <> FConnection.OCISvcCtx then begin
    ReleaseOCISvcCtx;

    FOCISvcCtx := FConnection.OCISvcCtx;
  {$IFNDEF AUTOREFCOUNT}
    if FOCISvcCtx <> nil then
      FOCISvcCtx.AddRef;
  {$ENDIF}
  end;
end;

procedure TOCICommand.ReleaseOCISvcCtx;
begin
  if FOCISvcCtx <> nil then begin
  {$IFNDEF AUTOREFCOUNT}
    FOCISvcCtx.ReleaseRef;
  {$ENDIF}
    FOCISvcCtx := nil;
  end;
end;

procedure TOCICommand.CheckOCI;
begin
  if not ((FOCICallStyle = OCI73) or (FOCICallStyle = OCI80)) then
    RaiseError(SCheckOCI);
end;

procedure TOCICommand.CheckOCI73;
begin
  if not (FOCICallStyle = OCI73) then
    RaiseError(SCheckOCI73);
end;

procedure TOCICommand.CheckOCI80;
begin
  if not (FOCICallStyle = OCI80) then
    RaiseError(SCheckOCI80);
end;

procedure TOCICommand.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    FConnection.OraError(FOCICallStyle, Status, not (FNonBlocking and (FExecuting
      or (GetCursorState in [csFetching, csFetchingAll]))), Component)
end;

procedure TOCICommand.CheckSession;
begin
  if FConnection = nil then
    RaiseError(SSessionNotDefined);
  if not FConnection.GetConnected then
    RaiseError(SSessionNotConnected);
end;

procedure TOCICommand.CheckActive;
begin
  if GetCursorState = csInactive then
    RaiseError(SCursorNotOpened);
end;

procedure TOCICommand.CheckInactive;
begin
  if GetCursorState <> csInactive then
    RaiseError(SCursorOpened);
end;

function TOCICommand.GetSmallintPrecision: integer;
begin
  if FSmallintPrecision <> -1 then
    Result := FSmallintPrecision
  else
    Result := FConnection.FSmallintPrecision;
end;

function TOCICommand.GetIntegerPrecision: integer;
begin
  if FIntegerPrecision <> -1 then
    Result := FIntegerPrecision
  else
    Result := FConnection.FIntegerPrecision;
end;

function TOCICommand.GetLargeintPrecision: integer;
const
  DefaultLargeIntPrecision = 18;
begin
  if FLargeintPrecision <> -1 then
    Result := FLargeintPrecision
  else
    Result := FConnection.FLargeintPrecision;

  if (Result = 0) and FConnection.FEnableLargeint then
    Result := DefaultLargeIntPrecision;
end;

function TOCICommand.GetFloatPrecision: integer;
begin
  if FFloatPrecision <> -1 then
    Result := FFloatPrecision
  else
    Result := FConnection.FFloatPrecision;
end;

function TOCICommand.GetBCDPrecision: integer;
begin
  if FBCDPrecision <> -1 then
    Result := FBCDPrecision
  else
    Result := FConnection.FBCDPrecision;
end;

function TOCICommand.GetBCDScale: integer;
begin
  if FBCDPrecision <> -1 then
    Result := FBCDScale
  else
    Result := FConnection.FBCDScale;
end;

function TOCICommand.GetFmtBCDPrecision: integer;
begin
  if FFmtBCDPrecision <> -1 then
    Result := FFmtBCDPrecision
  else
    Result := FConnection.FFmtBCDPrecision;
end;

function TOCICommand.GetFmtBCDScale: integer;
begin
  if FFmtBCDPrecision <> -1 then
    Result := FFmtBCDScale
  else
    Result := FConnection.FFmtBCDScale;
end;

function TOCICommand.GetOraType7(DataType: integer; SubDataType: integer): integer;
var
  OraType: integer;
begin
  case DataType of
    dtBoolean:
      OraType := SQLT_INT;
    dtString, dtWideString:
      if SubDataType in [dtFixedChar, dtFixedWideChar] then
        OraType := CHAR_TYPE
      else
        OraType := STRING_TYPE;  // return with terminator
    dtInteger:
      OraType := INTEGER_TYPE;
    dtFloat:
      OraType := FLOAT_TYPE;
    dtDateTime:
      OraType := DATE_TYPE;
    dtFixedChar, dtFixedWideChar:
      OraType := CHAR_TYPE;
    dtMemo, dtWideMemo:
      if SubDataType in [dtString, dtWideString] then
        OraType := STRING_TYPE  // return with terminator
      else if SubDataType in [dtFixedChar, dtFixedWideChar] then
        OraType := CHAR_TYPE
      else
        OraType := LONG_TYPE;
    dtBlob:
      OraType := LONGRAW_TYPE;
    dtRowId:
      OraType := ROWID_TYPE;
    dtCursor:
      OraType := CURSOR_TYPE;
    dtExtString, dtExtWideString:
      if SubDataType in [dtFixedChar, dtFixedWideChar] then
        OraType := CHAR_TYPE
      else
        OraType := STRING_TYPE;
    dtVarBytes, dtExtVarBytes:
      OraType := RAW_TYPE;
    dtLabel:
      OraType := SQLT_OSL;
  else
    RaiseError(SDataTypeNotSupported);
    OraType := 0;
  end;
  Result := OraType;
end;

function TOCICommand.GetInternalType8(DBType: Word): Integer;
begin
  case DBType of
    oraChar:
      Result := dtFixedChar;
    oraNChar:
      Result := dtFixedNChar;
    oraVarchar2:
      Result := dtString;
    oraNVarchar2:
      Result := dtNString;
    oraInteger:
      Result := dtInteger;
  {$IFNDEF UNIDACPRO}
    OraDataTypeMap.oraNumber:
  {$ELSE}
    OraDataTypeMapUni.oraNumber:
  {$ENDIF}
      Result := dtNumber;
    oraDate:
      Result := dtDateTime;
    oraRowID:
      Result := dtRowId;
    oraURowID:
      Result := dtURowId;
    oraRaw:
      Result := dtVarBytes;
    oraLong:
      Result := dtMemo;
    oraLongRaw:
      Result := dtBlob;
    oraClob:
      Result := dtOraClob;
    oraNClob:
      Result := dtNClob;
    oraBlob:
      Result := dtOraBlob;
    oraBFile:
      Result := dtBFILE;
    oraCFile:
      Result := dtCFILE;
    oraCursor:
      Result := dtCursor;
    oraObject:
      Result := dtObject;
    oraReference:
      Result := dtReference;
    oraXML:
      Result := dtXML;
    oraAnyData:
      Result := dtAnyData;
    oraLabel:
      Result := dtLabel;
    oraTimeStamp:
      Result := dtTimeStamp;
    oraTimeStampWithTimeZone:
      Result := dtTimeStampTZ;
    oraTimeStampWithLocalTimeZone:
      Result := dtTimeStampLTZ;
    oraIntervalYM:
      Result := dtIntervalYM;
    oraIntervalDS:
      Result := dtIntervalDS;
    oraFloat:
      Result := dtFloat;
    oraBinaryFloat:
      Result := dtBFloat;
    oraBinaryDouble:
      Result := dtBDouble;
    oraUndefined:
      Result := dtUndefined;
    else
      Result := dtUnknown;
  end;
end;

function TOCICommand.GetOraType8(DataType: integer; SubDataType: integer; IsDefine: boolean): integer;
var
  OraType: integer;
begin
  case DataType of
    dtString, dtExtString:
      if SubDataType in [dtFixedChar, dtFixedNChar] then
        OraType := SQLT_AFC
      else
        OraType := SQLT_CHR;
    dtWideString, dtExtWideString:
      if SubDataType in [dtFixedWideChar, dtFixedNWideChar] then
        OraType := SQLT_AFC
      else
        OraType := SQLT_CHR;
    dtFixedChar, dtFixedNChar, dtFixedWideChar, dtFixedNWideChar:
      OraType := SQLT_AFC;
    dtInt8, dtInt16, dtInteger:
      OraType := SQLT_INT;
    dtUInt8, dtWord , dtUInt32:
      OraType := SQLT_UIN;
    dtFloat, dtCurrency:
      if (FConnection.GetOracleVersion >= 10000) and
         ((OCISvcCtx.Home.OCIVersion >= 10000) or OCISvcCtx.Home.Direct)
      then
        case SubDataType of
          dtBFloat, dtBDouble:
            OraType := SQLT_BDOUBLE;
          dtNumber, dtNumberFloating:
            if OCISvcCtx.Home.Direct then
              OraType := SQLT_FLT
            else if IsDefine then
              OraType := SQLT_BDOUBLE
            else
              OraType := SQLT_FLT
          else
            OraType := SQLT_FLT;
        end
      else
        OraType := SQLT_FLT;
    dtSingle:
      if (FConnection.GetOracleVersion >= 10000) and
         ((OCISvcCtx.Home.OCIVersion >= 10000) or OCISvcCtx.Home.Direct)
      then
        case SubDataType of
          dtBFloat, dtBDouble:
            OraType := SQLT_BFLOAT;
          dtNumber, dtNumberFloating:
            if OCISvcCtx.Home.Direct then
              OraType := SQLT_BFLOAT
            else
              OraType := SQLT_BFLOAT;
          else
            OraType := SQLT_BFLOAT;
        end
      else
        OraType := SQLT_FLT;
    dtExtended:
      OraType := SQLT_VNU;
    dtNumber, dtInt64, dtUInt64, dtBCD, dtFMTBCD:
      OraType := SQLT_VNU;
    dtDateTime, dtDate, dtTime:
      OraType := SQLT_DAT;
    dtMemo, dtWideMemo:
      if SubDataType in [dtString, dtNString, dtWideString, dtNWideString] then
        OraType := SQLT_CHR
      else if SubDataType in [dtFixedChar, dtFixedNChar, dtFixedWideChar, dtFixedNWideChar] then
        OraType := SQLT_AFC
      else
        OraType := SQLT_LNG;
    dtBytes, dtVarBytes, dtExtVarBytes:
      OraType := SQLT_BIN;
    dtBlob:
      OraType := SQLT_LBI;
    dtOraBlob:
      OraType := SQLT_BLOB;
    dtOraClob, dtWideOraClob:
      OraType := SQLT_CLOB;
    dtBFILE:
      OraType := SQLT_BFILE;
    dtCFILE:
      OraType := SQLT_CFILE;
    dtRowId:
      OraType := SQLT_RDD;
    dtCursor:
      OraType := SQLT_RSET;
    dtObject:
      OraType := SQLT_NTY;
    dtReference:
      OraType := SQLT_REF;
    dtXML: begin
      if OCISvcCtx.Home.OCIVersion < 9200 then
        RaiseError(SDataTypeNotSupported);
      OraType := SQLT_NTY;
    end;
    dtAnyData: begin
      if OCISvcCtx.Home.OCIVersion < 9200 then
        RaiseError(SDataTypeNotSupported);
      OraType := SQLT_NTY;
    end;
    dtArray:
      OraType := SQLT_NTY;
    dtTable:
      OraType := SQLT_NTY;
    dtBoolean:
      OraType := SQLT_INT;
    dtLabel:
      OraType := SQLT_OSL;
    dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
    dtIntervalYM, dtIntervalDS: begin
      if OCISvcCtx.Home.OCIVersion < 9000 then
        RaiseError(SDataTypeNotSupported);
      OraType := 0; // anti warning
      case DataType of
        dtSQLTimeStamp, dtTimeStamp:
          OraType := SQLT_TIMESTAMP;
        dtTimeStampTZ:
          OraType := SQLT_TIMESTAMP_TZ;
        dtTimeStampLTZ:
          OraType := SQLT_TIMESTAMP;
        dtIntervalYM:
          OraType := SQLT_INTERVAL_YM;
        dtIntervalDS:
          OraType := SQLT_INTERVAL_DS;
      end;
    end;
    dtUndefined:
      OraType := SQLT_CHR;
  else
    RaiseError(SDataTypeNotSupported);
    OraType := 0;
  end;
  Result := OraType;
end;

function TOCICommand.GetOraType(DataType: integer; SubDataType: integer; IsDefine: boolean): integer;
begin
  if FOCICallStyle = OCI73 then begin
    Result := GetOraType7(DataType, SubDataType);
  end
  else
  if FOCICallStyle = OCI80 then begin
    Result := GetOraType8(DataType, SubDataType, IsDefine);
  end
  else begin
    Result := 0;
    CheckOCI;
  end;
end;

procedure TOCICommand.InternalOpen;
var
  Res: sword;
begin
  CheckOCI73;
  CheckSession;
  AllocOCISvcCtx;

  Lock;
  try // For Busy
    FConnection.Lock;
    try
      Res := OCI7.oopen(FCursorRef.CDA, FConnection.GetLDA, nil, -1, -1, nil, -1);
    finally
      FConnection.Release;
    end;
    Check(Res);

    SetCursorState(csOpen);
  finally
    Release;
  end;
end;

function TOCICommand.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

function TOCICommand.GetOCI7: TOCI7API;
begin
  Result := OCISvcCtx.OCI7;
end;

function TOCICommand.GetOCI8: TOCI8API;
begin
  Result := OCISvcCtx.OCI8;
end;

/// RemoveCRSymbols is used to remove #13 symbol from string. It is needed for
/// pl/sql code to be executed without runtime errors. If ErrorOffset <> -1 is
/// Result may contain invalid value but ErrorOffset is recalculated to
/// reflect error correctly.
function TOCICommand.RemoveCRSymbols(SQLText: string;
  var ErrorOffset: integer): string;
const
  WrappedConstL: string = 'wrapped';
  WrappedConstU: string = 'WRAPPED';
var
  i, j, starti, Len: integer;
  InlineComment: boolean;
  NestedComment: boolean;
  StringLiteral: boolean;
  AlternativeQuoting: boolean;
  QuoteDelimiter: Char;
  IsQuoted: boolean;
  IsWrapped: boolean;
  WrappedIndex: integer;
begin
  Result := '';

  if SQLText = '' then
    Exit;

  Len := Length(SQLText);
  Result := '';
  j := 1;
  i := 1;
  starti := 1;
  WrappedIndex := 1;
  IsWrapped := False;
  InlineComment := False;
  NestedComment := False;
  StringLiteral := False;
  IsQuoted := False;
  AlternativeQuoting := False;
  QuoteDelimiter := #0;

  while i < Len do begin
    if (ErrorOffset <> -1) and (ErrorOffset = j - 1) then begin
      ErrorOffset := i - 1;
      Exit;
    end;
    case SQLText[i] of
      #13:
        if not StringLiteral or RemoveCRInStringLiterals then begin
          Result := Result + Copy(SQLText, starti, i - starti) + #10;
          if SQLText[i + 1] = #10 then
            starti := i + 2
          else begin
            starti := i + 1;
            j := j + 1;
          end;
          i := i + 1;
          Continue;
        end;
      #10:
        InlineComment := False;
      '-':
        if not (NestedComment or StringLiteral or IsQuoted) and (SQLText[i + 1] = '-') then
          InlineComment := True;
      '/':
        if not (InlineComment or StringLiteral or IsQuoted) and (SQLText[i + 1] = '*') then
          NestedComment := True;
      '"':
        if not (StringLiteral or InlineComment or NestedComment or IsWrapped) then
          IsQuoted := not IsQuoted;
      '''': begin
        // to avoid bug if comment contains single apostrophe
        if not (InlineComment or NestedComment or IsQuoted or IsWrapped) then begin
          if not StringLiteral then begin
            if (i > 1) and ((SQLText[i - 1] = 'q') or (SQLText[i - 1] = 'Q')) then begin
              AlternativeQuoting := True;
              QuoteDelimiter := SQLText[i + 1];
              case QuoteDelimiter of
                '[': QuoteDelimiter := ']';
                '{': QuoteDelimiter := '}';
                '<': QuoteDelimiter := '>';
                '(': QuoteDelimiter := ')';
              end;
              j := j + 1;
              i := i + 1;
            end
            else
              AlternativeQuoting := False;
            StringLiteral := True;
          end
          else begin
            if AlternativeQuoting then begin
              if (i > 1) and (SQLText[i - 1] = QuoteDelimiter) then
                StringLiteral := False;
            end
            else
              StringLiteral := False;
          end;
        end;
      end;
      '*':
        if not (InlineComment or StringLiteral or IsQuoted) and NestedComment and (SQLText[i + 1] = '/') then begin
          NestedComment := False;
          // in case of such code as "select /*all fields*/* from dept"
          j := j + 1;
          i := i + 1;
        end;
    end;
    //to avoid bug with processing wrapped packages
    if not (IsWrapped or NestedComment or StringLiteral or IsQuoted) then begin
      if ((SQLText[i] = WrappedConstU[WrappedIndex]) or (SQLText[i] = WrappedConstL[WrappedIndex]))
        and (i > 1) and ((WrappedIndex > 1) or (SQLText[i - 1] = #10) or (SQLText[i - 1] = ' ')
        or (SQLText[i - 1] = '/'))
      then
        inc(WrappedIndex)
      else
        WrappedIndex := 1;

      if (WrappedIndex = 8) then begin
        if (i < Len) and ((SQLText[i + 1] = #13) or (SQLText[i + 1] = #10)
          or (SQLText[i + 1] = ' ') or (SQLText[i + 1] = '/'))
        then
          IsWrapped := True;
        WrappedIndex := 1;
      end;
    end
    else
      WrappedIndex := 1;

    j := j + 1;
    i := i + 1;
  end;
  Result := Result + Copy(SQLText, starti, i - starti + 1);
  if (ErrorOffset <> -1) and (ErrorOffset = j - 1) then begin
    ErrorOffset := i - 1;
  end;
end;

procedure TOCICommand.InternalParse;
var
  Res: sword;
  SQLText: string;
begin
  CheckOCI73;
  CheckActive;

// for PL-SQL
  Res := -1;
  SQLText := RemoveCRSymbols(TrimRight(FSQL), Res);

  Lock;
  try   // For Busy
    FConnection.Lock;
    try
      Res := OCI7.oparse(FCursorRef.CDA, PAnsiChar(AnsiString(SQLText)), Length(SQLText) + 1, //-1,
        OCI_PARSE_NODEFER, OCI_LANG_V7);  // DEFER ???
    finally
      FConnection.Release;
    end;

    try
      // check for compilation errors
      if (Res = OCI_SUCCESS) and (FCursorRef.CDA.wrn and 33 = 33) then
        FConnection.FLastError := 24344;

      Check(Res);

      FSQLType := FCursorRef.CDA.ft;
      FLastSQLtype := FSQLType;

      SetCursorState(csParsed);
    finally
      FErrorOffset := FCursorRef.CDA.peo;
    end;
  finally
    Release;
  end;
end;

procedure TOCICommand.InternalPrepare;

  procedure PrepareSQL(SQLText: string);
  var
    OCIStmt: pOCIStmt;
    Res: sword;
    Size: integer;
    p: IntPtr;
  begin
    Res := -1;

    // for PL-SQL
    SQLText := RemoveCRSymbols(SQLText, Res);
    if SQLText = '' then
      SQLText := ' ';

    if OCISvcCtx.UnicodeEnv and (OCISvcCtx.Home.OCIVersion >= 10200) and
      (OCISvcCtx.Home.OCIVersion < 11000) and (SQLText[Length(SQLText)] = 'n')
    then
      SQLText := SQLText + ' ';

    FConnection.Lock;
    try
      p := StringToHGlobalOCI(SQLText, Size, OCISvcCtx.UnicodeEnv);
      try
        if FCursorRef.FStatementMode = smCached then begin
          if OCISvcCtx.Home.Direct then
            RaiseError(SStmtCacheNotSupportedWithDirect);

          FCursorRef.ReleaseCursor;
          Res := OCI8.OCIStmtPrepare2(OCISvcCtx.hOCISvcCtx, OCIStmt, OCISvcCtx.hOCIError, p, Size, nil, 0, OCI_NTV_SYNTAX, OCI_DEFAULT);
          FCursorRef.hOCIStmt := OCIStmt;
          FCursorRef.FStatementMode := smCached;
        end
        else begin
          if FConnection.FDirect then
            Res := OCI8.OCIStmtPrepare3(OCISvcCtx.hOCISvcCtx, FCursorRef.OCIStmt, OCISvcCtx.hOCIError, p, Size, OCI_NTV_SYNTAX, OCI_DEFAULT)
          else if OCISvcCtx.Home.OCIVersion >= 12000 then begin
            FCursorRef.ReleaseCursor;
            Res := OCI8.OCIStmtPrepare2(OCISvcCtx.hOCISvcCtx, OCIStmt, OCISvcCtx.hOCIError, p, Size, nil, 0, OCI_NTV_SYNTAX, OCI_DEFAULT);
            FCursorRef.hOCIStmt := OCIStmt;
            if FConnection.FStatementCache then
              FCursorRef.FStatementMode := smCached
            else
              FCursorRef.FStatementMode := smPrepared;
          end
          else
            Res := OCI8.OCIStmtPrepare(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, p, Size, OCI_NTV_SYNTAX, OCI_DEFAULT);
        end;
      finally
        FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
      end;
    finally
      FConnection.Release;
    end;
    Check(Res);
  end;

var
  i: integer;
  SQLText: string;
  StmtType: word;
  StmtTypeInt32: Integer;
{$IFDEF MSWINDOWS}
  OCISubscr: pOCISubscription;
{$ENDIF}
begin
  CheckOCI80;
  CheckSession;
  AllocOCISvcCtx;

  for i := 0 to Params.Count - 1 do
    TOraParamDesc(Params[i]).ClearBindData;

  Lock;
  try   // For Busy
    SQLText := TrimRight(FSQL);

    PrepareSQL(SQLText);

    Check(OCI8.OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, StmtTypeInt32, nil, OCI_ATTR_STMT_TYPE, OCISvcCtx.hOCIError));
    StmtType := Word(StmtTypeInt32);

    case StmtType of
      OCI_STMT_SELECT:
        FSQLType := SQL_SELECT;
      OCI_STMT_UPDATE:
        FSQLType := SQL_UPDATE;
      OCI_STMT_DELETE:
        FSQLType := SQL_DELETE;
      OCI_STMT_INSERT:
        FSQLType := SQL_INSERT;
      OCI_STMT_BEGIN, OCI_STMT_DECLARE:
        FSQLType := SQL_PLSQL;
      14,15: // explain 805 - 14, 816 - 15
        FSQLType:= SQL_EXPLAIN;
    else
      FSQLType := SQL_UNKNOWN;
    end;
    FLastSQLType := FSQLType;

    // remove trailing ";"
    if not FOCISvcCtx.Home.Direct and
       (FSQLType in [SQL_SELECT, SQL_INSERT, SQL_UPDATE, SQL_DELETE]) and
       (Length(SQLText) > 0) and (SQLText[Length(SQLText)] = ';')
    then begin
      if FCursorRef.FStatementMode = smCached then begin
        Check(OCI8.OCIStmtRelease(FCursorRef.hOCIStmt, OCISvcCtx.hOCIError, nil, 0, OCI_DEFAULT));
        FCursorRef.hOCIStmt := nil;
      end;
      SQLText[Length(SQLText)] := ' ';
      PrepareSQL(SQLText);
    end;

  {$IFDEF MSWINDOWS}
    if (FChangeNotification <> nil) and (FChangeNotification.FEnabled) then begin
      OCISubscr := FChangeNotification.GetSubscriptionHandle(FConnection);

      Check(OCI8.OCIAttrSet1(FCursorRef.OCIStmt, OCI_HTYPE_STMT, OCISubscr, 0,
        OCI_ATTR_CHNF_REGHANDLE, OCISvcCtx.hOCIError));
    end;
  {$ENDIF}

    SetCursorState(csPrepared);
  finally
    Release;
  end;
end;

procedure TOCICommand.Prepare;
var
  StatementMode : TStatementMode;
begin
  CheckSession;
  if GetCursorState <> csInactive then
    Exit;

  AllocOCISvcCtx;

  try
    FOCICallStyle := FConnection.OCICallStyleCommand; // see InitProcParams and SetCursor too
    FCursorRef.Init(OCISvcCtx, OCICallStyle);

    FRowsProcessed := 0;
    FErrorOffset := 0;

    if FConnection.FStatementCache and FStatementCache then
      StatementMode := smCached
    else
      StatementMode := smUnknown;

    FCursorRef.AllocCursor(StatementMode);
    try
      if FOCICallStyle = OCI73 then begin
        InternalOpen;
        try
          InternalParse;
        except
          InternalClose;
          raise;
        end;
      end
      else
      if FOCICallStyle = OCI80 then
        InternalPrepare
      else
        CheckOCI;
    except
      FCursorRef.FreeCursor;
      raise;
    end;
  except
    ReleaseOCISvcCtx;
    raise;
  end;

  inherited;
end;

procedure TOCICommand.Unprepare;
begin
  Close;

  inherited;
end;

class function TOCICommand.IsAllowedArrayType(DataType: Word): boolean;
begin
  Result := (DataType in [dtOraBlob, dtOraClob, dtWideOraClob, dtNClob,
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS,
    dtNumber, dtNumberFloating, dtBFloat, dtBDouble,
    dtFixedNChar, dtFixedNWideChar, dtNString, dtNWideString]) or
    inherited IsAllowedArrayType(DataType);
end;

function TOCICommand.IsLobPrefetchAllowed(IsNational, IsUnicode: Boolean): Boolean;
begin
  Result := (FConnection.GetOracleVersion >= 11000) and
            (FOCISvcCtx.Home.Direct or (OCISvcCtx.Home.OCIVersion >= 12000) or
            // Oracle 11 clent has bug with prefetch NCLOB when UseUnicode=False
            ((OCISvcCtx.Home.OCIVersion >= 11000) and (not IsNational or IsUnicode)));
end;

function TOCICommand.NeedBindParam(Param: TOraParamDesc): boolean;
begin
  Result := Param.GetIsBound or (FCheckParamHasDefault and not Param.FHasDefault) or
    (Param.ParamType <> pdInput);
end;

function OCICallbackInBind(ictxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
  var bufp: IntPtr; var alen: ub4; var piece: ub1; var indp: IntPtr): sb4; cdecl; forward;

function OCICallbackOutBind(octxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
  var bufp: IntPtr; var alen: pub4; var piece: ub1; var indp: IntPtr; var rcodep: pub2): sb4; cdecl; forward;

procedure TOCICommand.BindParam(Param: TOraParamDesc);
var
  OraType: word;
  hBind: pOCIBind;
  ValuePtr: IntPtr;
  LenPtr: pub2;
  StrPtr: IntPtr;
  BufferSize: integer;
  BufferSkip: integer;
  ParamName: string;
  Obj: TOraObject;
  Blob: TBlob;
  OraLob: TOraLob;
  CharsetId: Integer; //sb2
  CharsetForm : Integer;
  MaxDataSize: Integer;
  BindDynamicForLong: Boolean;
  Res, StrSize: integer;
begin
  if not NeedBindParam(Param) then
    Exit;

  Param.ValidateParamValue;

  Param.AllocBuffer;

  if Param.DataType = dtUnknown then
    if Param.GetNull then
      OraType := VARCHAR2_TYPE  // WAR
    else
      raise Exception.Create(Format(SUnknownParamDataType, [Param.Name]))
  else
    OraType := GetOraType(Param.DataType, Param.SubDataType, False);

  Param.SyncIndicator(FConnection);
  BindDynamicForLong := False;

  ParamName :=  Param.Name;
  BufferSize := Param.FValueSize;
  BufferSkip := BufferSize;
  ValuePtr := Param.FValueData;
  if Param.FHasValueLen then
    LenPtr := @Param.FValueLens[0]
  else
    LenPtr := nil;

  if not Param.FTable then
    case Param.DataType of
      dtBytes, dtVarBytes:
        if Param.ParamType in [pdOutput, pdResult] then
          if Param.FArraySize > 1 then
            System.FillChar(Param.FIndicators^, SizeOf(sb2) * Param.FArraySize, $FF)
          else
            psb2(Param.FIndicator)^ := -1;
      dtString, dtFixedChar: begin
        if Param.ParamType in [pdOutput, pdResult] then
          if Param.FArraySize > 1 then
            System.FillChar(Param.FIndicators^, SizeOf(sb2) * Param.FArraySize, $FF)
          else
            psb2(Param.FIndicator)^ := -1;
        Dec(BufferSize);
      end;
      dtWideString, dtFixedWideChar: begin
        if Param.ParamType in [pdOutput, pdResult] then
          if Param.FArraySize > 1 then
            System.FillChar(Param.FIndicators^, SizeOf(sb2) * Param.FArraySize, $FF)
          else
            psb2(Param.FIndicator)^ := -1;
        Dec(BufferSize, 2);
      end;
      dtBlob, dtMemo, dtWideMemo: begin
        if Param.ParamType in [pdUnknown, pdInput] then begin
          if Param.FArraySize <= 1 then begin
            BufferSize := Param.GetAsBlobRef.Size; // anti ORA-01026
            if BufferSize = 0 then
              Inc(BufferSize);
          end
          else
            BufferSize := MaxBlobSize;
        end
        else begin
          BufferSize := MaxBlobSize;
          if Param.FArraySize <= 1 then
            if Param.ParamType in [pdOutput, pdResult] then
              Param.GetAsBlobRef.Clear;  // prevent write OUT data
        end;
        BindDynamicForLong := True;
      end;
    end;

  if FOCICallStyle = OCI73 then begin
    case Param.DataType of
      dtCursor: begin
        Assert(Param.GetAsCursor <> nil);
        Param.GetAsCursor.Init(OCISvcCtx, OCICallStyle);
        Param.GetAsCursor.AllocCursor;
        ValuePtr := Param.GetAsCursor.CDA;
      end;
      dtUnknown:  begin
        if FNullBuf = nil then
          FNullBuf := Marshal.AllocHGlobal(sizeof(IntPtr));
        Marshal.WriteIntPtr(FNullBuf, nil);
        ValuePtr := FNullBuf;
        BufferSize := 1;
      end;
    end;

    if not Param.FTable then begin
      if not (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) then
        if LenPtr <> nil then
          Check(OCI7.obndra(FCursorRef.CDA, PAnsiChar(AnsiString(ParamName)), -1, ValuePtr, BufferSize,
            OraType, -1, Param.IndicatorPtr, LenPtr, nil, 0, nil, nil, -1, -1))
        else
          Check(OCI7.obndrv(FCursorRef.CDA, PAnsiChar(AnsiString(ParamName)), -1, ValuePtr, BufferSize,
            OraType, -1, Param.IndicatorPtr, nil, -1, -1))
      else
      { Piecewise }
        Check(OCI7.obindps(FCursorRef.CDA, 0, PAnsiChar(AnsiString(ParamName)), -1, nil, MaxBlobSize,
          OraType, 0, Param.IndicatorPtr, nil, nil, 0, 0, 0, 0, 0, nil, nil, 0, 0));

      Param.ActualLength := 1;
    end
    else begin
    { Table }
      Param.ActualLength := Param.FArraySize;
      Check(OCI7.obindps(FCursorRef.CDA, 1, PAnsiChar(AnsiString(ParamName)), -1, ValuePtr, BufferSize,
        OraType, 0, Param.IndicatorPtr, nil, nil, BufferSize, sizeof(sb2),
        0, 0, Param.FArraySize, @Param.FActualTableLen, nil, 0, 0));
    end;
  end
  else
  if FOCICallStyle = OCI80 then begin
    case Param.DataType of
      dtCursor: begin
        Assert(Param.GetAsCursor <> nil);
        Param.GetAsCursor.Init(OCISvcCtx, OCICallStyle);
        Param.GetAsCursor.AllocCursor;
        ValuePtr := Param.GetAsCursor.OCIStmtPtr;
      end;
      dtOraBlob, dtOraClob, dtWideOraClob, dtNClob: begin
        if Param.GetLength <= 1 then begin
          OraLob := Param.GetAsOraBlob;
          OraLob.OCISvcCtx := FConnection.OCISvcCtx;
          OraLob.AllocLob;
          ValuePtr := OraLob.OCILobLocatorPtr;
        end;
      end;
      dtBFile, dtCFile: begin
        Param.GetAsBFile.OCISvcCtx := FConnection.OCISvcCtx;
        Param.GetAsBFile.AllocLob;
        ValuePtr := Param.GetAsBFile.OCILobLocatorPtr;
      end;
      dtObject, dtXML, dtAnyData, dtArray, dtTable: begin
        Obj := TOraObject(Param.GetObject);
        if Obj <> nil then begin
          Obj.AllocObject(OCISvcCtx);
          if not Obj.IsNull then
            Obj.WriteLobs;
        end;
        ValuePtr := nil;
      end;
      dtReference: begin
        ValuePtr := nil;
        //ValuePtr := Param.AsRef.OCIRefPtr;
        //RefPtr := nil;
        //ValuePtr := @RefPtr;//Param.AsRef.OCIRefPtr;
      end;
      dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64: begin
        if Param.GetLength <= 1 then begin
          BufferSize := OCI_NUMBER_SIZE;
          Param.GetAsNumber.SetEnvironment(OCISvcCtx.Environment);
          ValuePtr := Param.GetAsNumber.OCINumberPtr;
        end;
      end;
      dtUnknown: begin
        if FNullBuf = nil then
          FNullBuf := Marshal.AllocHGlobal(sizeof(IntPtr));
        Marshal.WriteIntPtr(FNullBuf, nil);
        ValuePtr := FNullBuf;
        BufferSize := 1;
      end;
      else
        if OCISvcCtx.Home.OCIVersion >= 9000 then begin
          case Param.DataType of
            dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
              if Param.GetLength <= 1 then
                with Param.GetAsTimeStamp do begin
                  BufferSize := sizeof(IntPtr);
                  case Param.DataType of
                    dtSQLTimeStamp, dtTimeStamp:
                      DescriptorType := OCI_DTYPE_TIMESTAMP;
                    dtTimeStampTZ:
                      DescriptorType := OCI_DTYPE_TIMESTAMP_TZ;
                    dtTimeStampLTZ:
                      DescriptorType := OCI_DTYPE_TIMESTAMP;
                  end;
                  SetEnvironment(OCISvcCtx.Environment);
                  AllocDateTime;
                  ValuePtr := GetOCIDateTimePtr;
                end;
            dtIntervalYM, dtIntervalDS:
              if Param.GetLength <= 1 then
                with Param.GetAsInterval do begin
                  BufferSize := sizeof(IntPtr);
                  if Param.DataType = dtIntervalYM then
                    DescriptorType := OCI_DTYPE_INTERVAL_YM
                  else
                    DescriptorType := OCI_DTYPE_INTERVAL_DS;
                  SetEnvironment(OCISvcCtx.Environment);
                  AllocInterval;
                  ValuePtr := GetOCIIntervalPtr;
                end;
          end;
        end;
    end;

    if (Param.FHandle <> nil) and Param.FBufferBinded and not BindDynamicForLong then
      Exit;

    if not Param.FTable then begin
      if (FBatchIters = 1) and (FBatchOffset = 0) then begin
        if not BindDynamicForLong then begin
          StrPtr := StringToHGlobalOCI(ParamName, StrSize, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIBindByName(FCursorRef.OCIStmt, hBind, OCISvcCtx.hOCIError, StrPtr, StrSize,
            ValuePtr, BufferSize, OraType,
            Param.IndicatorPtr, LenPtr, nil, 0, nil, OCI_DEFAULT);
          FreeStringOCI(StrPtr, OCISvcCtx.UnicodeEnv);
          Check(Res);

          case Param.DataType of
            dtObject, dtArray, dtTable: begin
              Obj := TOraObject(Param.GetObject);
              if Obj <> nil then
                Check(OCI8.OCIBindObject(hBind, OCISvcCtx.hOCIError, Obj.ObjectType.TDO, Obj.InstancePtr, nil, Obj.IndicatorPtr, nil));
            end;
            dtXML, dtAnyData: begin
              Obj := TOraObject(Param.GetObject);
              if Obj <> nil then begin
                if not OCISvcCtx.Home.Direct then
                  // if XML is NULL then Instance and Indicator must be nil (for OUT parameters especially)
                  if Obj.IsNull then
                    Obj.FreeObject;

                Check(OCI8.OCIBindObject(hBind, OCISvcCtx.hOCIError, Obj.ObjectType.TDO, Obj.InstancePtr, nil, Obj.IndicatorPtr, nil));
              end;
            end;
            dtReference: begin
              Obj := TOraObject(Param.GetObject);
              if Obj <> nil then
                Check(OCI8.OCIBindObject(hBind, OCISvcCtx.hOCIError, Obj.ObjectType.TDO, IntPtr(TOraRef(Obj).OCIRefPtr), nil, nil, nil));
            end;
          end;
        end
        else begin
          StrPtr := StringToHGlobalOCI(ParamName, StrSize, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIBindByName(FCursorRef.OCIStmt, hBind, OCISvcCtx.hOCIError, StrPtr, StrSize,
            nil, BufferSize, OraType, Param.IndicatorPtr,
            nil, nil, 0, nil, OCI_DATA_AT_EXEC);
          FreeStringOCI(StrPtr, OCISvcCtx.UnicodeEnv);
          Check(Res);

          // Dynamic blob
          Check(OCI8.OCIBindDynamic(hBind, OCISvcCtx.hOCIError, GCHandle, OCICallbackInBindPtr, GCHandle, OCICallbackOutBindPtr));
        end;

        Param.ActualLength := 1;
      end
      else begin
      // DML Array
        if FBatchIters + FBatchOffset > Param.FArraySize then
          RaiseError(Format(SArrayParam, [ParamName, FBatchIters]));

        if not BindDynamicForLong then begin
          StrPtr := StringToHGlobalOCI(ParamName, StrSize, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIBindByName(FCursorRef.OCIStmt, hBind, OCISvcCtx.hOCIError, StrPtr, StrSize,
            ValuePtr, BufferSize, OraType,
            Param.IndicatorPtr, LenPtr, nil, 0, nil, OCI_DEFAULT);
          FreeStringOCI(StrPtr, OCISvcCtx.UnicodeEnv);
          Check(Res);
        end
        else begin
          StrPtr := StringToHGlobalOCI(ParamName, StrSize, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIBindByName(FCursorRef.OCIStmt, hBind, OCISvcCtx.hOCIError, StrPtr, StrSize,
            nil, BufferSize, OraType, Param.IndicatorPtr,
            nil, nil, 0, nil, OCI_DATA_AT_EXEC);
          FreeStringOCI(StrPtr, OCISvcCtx.UnicodeEnv);
          Check(Res);

          // Dynamic blob
          Check(OCI8.OCIBindDynamic(hBind, OCISvcCtx.hOCIError, GCHandle, OCICallbackInBindPtr, GCHandle, OCICallbackOutBindPtr));
        end;

        Check(OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, BufferSkip, sizeof(sb2), sizeof(sb2), 0));
      end;
    end
    else begin
    { Table }
    // PL/SQL table
      Param.ActualLength := Param.FArraySize;
      StrPtr := StringToHGlobalOCI(ParamName, StrSize, OCISvcCtx.UnicodeEnv);
      Res := OCI8.OCIBindByName(FCursorRef.OCIStmt, hBind, OCISvcCtx.hOCIError, StrPtr, StrSize,
        ValuePtr, BufferSize, OraType,
        Param.IndicatorPtr, LenPtr, nil, Param.FArraySize, @Param.FActualTableLen, OCI_DEFAULT);
      FreeStringOCI(StrPtr, OCISvcCtx.UnicodeEnv);
      Check(Res);

      Check(OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, BufferSkip, sizeof(sb2), sizeof(sb2), 0));
    end;

    Param.FHandle := hBind;
    // for params with dtUnknown data type buffer is not allocated
    if Param.FBufferAllocated then
      Param.FBufferBinded := True;

    case Param.DataType of
      dtString, dtFixedChar:
        CharsetId := Integer(FConnection.FCharsetId);
      dtWideString, dtFixedWideChar:
        if Param.UnicodeIsUTF16 then
          CharsetId := Integer(OCI_UTF16ID)
        else
          CharsetId := Integer(OCI_AL16UTF16ID);
      dtBlob, dtMemo, dtWideMemo:
        if Param.FArraySize > 1 then begin
          Blob := TBlob(Param.GetItemObject(0));
          if (Blob <> nil) and (Blob.IsUnicode) then
            CharsetId := Integer(OCI_UTF16ID)
          else
            CharsetId := Integer(FConnection.FCharsetId)
        end
        else if Param.GetAsBlobRef.IsUnicode then
          CharsetId := Integer(OCI_UTF16ID)
        else
        {$IFDEF IS_UTF8_EXCLUDE_MEMO}
          CharsetId := 0;
        {$ELSE}
          CharsetId := Integer(FConnection.FCharsetId);
        {$ENDIF}
    else
      CharsetId := 0;
    end;

    if Param.GetNational or
       ((CharsetId = OCI_UTF16ID) and FConnection.FUnicodeAsNational and (OCISvcCtx.Home.OCIVersion >= 9000)) then
    begin
      CharsetForm := SQLCS_NCHAR;
      Check(OCI8.OCIAttrSet2(hBind, OCI_HTYPE_BIND, CharsetForm, 0, OCI_ATTR_CHARSET_FORM, OCISvcCtx.hOCIError));
    end;
    if Integer(CharsetId) > 0 then
      Check(OCI8.OCIAttrSet2(hBind, OCI_HTYPE_BIND, CharsetId, 0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));
    if UseMaxDataSize and not (OCISvcCtx.Home.PossibleOCICallStyles = [OCI80]) and
      (Param.DataType in [dtString, dtFixedChar, dtWideString, dtFixedWideChar])
    then begin
      MaxDataSize := Param.FValueSize - 1;
      if Param.DataType in [dtWideString, dtFixedWideChar] then
        MaxDataSize := MaxDataSize shr 1;

      if (Param.ParamType in [pdUnknown, pdInput]) and not Param.FTable then begin //(((Param.ParamType = pdUnknown) and (Param.FLen > 0)) or
                                                                     // Unknown param is binded as Input/OutPut
                                                                     // cr8800
        MaxDataSize := 65535; // FConnection.GetMaxStringSize * FConnection.FCharLength;

        if Param.FValueSize * 3 < MaxDataSize then
          MaxDataSize := Param.FValueSize * 3;

        if (Param.DataType in [dtWideString, dtFixedWideChar]) then begin
          if (MaxDataSize < 4) then
           MaxDataSize := 4;
        end
        else
          if MaxDataSize < 2 then
            MaxDataSize := 2;
      end;

      Check(OCI8.OCIAttrSet2(hBind, OCI_HTYPE_BIND, MaxDataSize, 0, OCI_ATTR_MAXDATA_SIZE, OCISvcCtx.hOCIError));
      if (OCISvcCtx.Home.OCIVersion >= 9000) and (FConnection.GetOracleVersion >= 9200) and (Param.ParamType = pdInput) and (Param.FValueSize > 0) and
         ((FConnection.GetOracleVersion < 9206) or (FConnection.GetOracleVersion > 10000)) then //Oracle 9.2.0.6 Patchset Bug
      begin
        MaxDataSize := Integer(Param.FValueSize);
        Check(OCI8.OCIAttrSet2(hBind, OCI_HTYPE_BIND, MaxDataSize, 0, OCI_ATTR_MAXCHAR_SIZE, OCISvcCtx.hOCIError));
      end;
    end;
  end
  else
    CheckOCI;
end;

function TOCICommand.InternalExecute(Mode: integer; Rows: Integer = 0): sword;
var
  OldCursorState: TCursorState;
  Res: sword;
  Res1: sword;
  Iters: ub4;
  IterOffset: ub4;
  i: integer;
  ValueInt: Integer;
  OCIStmt: pOCIStmt;
begin
  CheckActive;

  Res := OCI_SUCCESS;

  Lock;
  try  // For Busy
    OldCursorState := GetCursorState;
    SetCursorState(csExecuting);

    OCIStmt := nil;
    try
      try
        if FOCICallStyle = OCI73 then begin
          if Mode = OCI_DEFAULT then begin
            FConnection.Lock;
            try
              Res := OCI7.oexn(FCursorRef.CDA, FBatchIters + FBatchOffset, FBatchOffset);
            finally
              FConnection.Release;
            end;

            if (Res <> OCI_SUCCESS) and
               (Res <> OCI_STILL_IS_PIECE) and
               (Res <> OCI_STILL_IS_PIECE1) and
               (Res <> OCI_BLOCKED)
            then begin
              FErrorOffset := FCursorRef.CDA.peo;
              Check(Res);
            end;

            if Res = OCI_SUCCESS then begin
              FRowsProcessed := FCursorRef.CDA.rpc;
              if (FBatchIters > 1) or (FBatchOffset > 0) then
                FParamsProcessed := FBatchIters;
            end;
          end;
        end
        else
        if FOCICallStyle = OCI80 then begin
          if FSQLType = SQL_SELECT then begin
            Iters := Rows;
            IterOffset := 0;
          end
          else begin
            Iters := FBatchIters + FBatchOffset;
            IterOffset := FBatchOffset;
          end;

          OCIStmt := FCursorRef.OCIStmt;
          FConnection.Lock;
          try
            if FCommandTimeout > 0 then
              FConnection.RunTimeoutThread(FCommandTimeout);
            try
              Res := OCI8.OCIStmtExecute(OCISvcCtx.hOCISvcCtx, OCIStmt, OCISvcCtx.hOCIError, Iters, IterOffset, nil, nil, Mode);
            finally
              if FCommandTimeout > 0 then
                FConnection.StopTimeoutThread;
            end;
          finally
            FConnection.Release;
          end;

          if Mode <> OCI_DESCRIBE_ONLY then begin
            Check(OCI8.OCIAttrGet2(OCIStmt, OCI_HTYPE_STMT, FRowsProcessed, nil, OCI_ATTR_ROW_COUNT, OCISvcCtx.hOCIError));
            if (FBatchIters > 1) or (FBatchOffset > 0) then
              FParamsProcessed := FBatchIters;
          end;

          if FSQLType = SQL_UNKNOWN then begin
            Check(OCI8.OCIAttrGet2(OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, OCI_ATTR_SQLFNCODE, OCISvcCtx.hOCIError));
            FSQLType := Word(ValueInt);
            FLastSQLType := FSQLType;
          end;

          if (Res <> OCI_SUCCESS) and
             (Res <> OCI_NEED_DATA) and
             ((FSQLType <> SQL_SELECT) or (Res <> OCI_NO_DATA))
          then begin
            Res1 := OCI8.GetOraError(Res, OCISvcCtx);
            if Res1 <> Abs(OCI_BLOCKED) then
              Check(Res)
            else
              Res := Res1;
          end;
        end
        else
          CheckOCI;
      except
        if GetCursorState <> csInactive then  // on lost connection
          SetCursorState(OldCursorState);
        raise;
      end;

      if Mode <> OCI_DESCRIBE_ONLY then begin
        SetCursorState(csExecuted);

        for i := 0 to FParams.Count - 1 do
          if TOraParamDesc(FParams[i]).DataType = dtCursor then
            TOraParamDesc(FParams[i]).GetAsCursor.FState := csExecuted;
      end
      else
        SetCursorState(OldCursorState);

    finally
      if (GetCursorState <> csInactive) and // on lost connection
         (FOCICallStyle = OCI80) and (OCISvcCtx.Home.OCIVersion >= 8050)
      then begin
        if OCISvcCtx.Home.OCIVersion >= 8100 then
          Check(OCISvcCtx.OCI8.OCIAttrGet2(OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, OCI_ATTR_PARSE_ERROR_OFFSET, OCISvcCtx.hOCIError))
        else
          Check(OCISvcCtx.OCI8.OCIAttrGet2(OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, $80, OCISvcCtx.hOCIError));
        FErrorOffset := Word(ValueInt);
      end;
    end;
  finally
    Release;
  end;

  FFetchedRows := 0;
  Result := Res;
end;

procedure TOCICommand.Exec;

  procedure Warning(Msg: string);
  begin
  {$IFDEF MSWINDOWS}
    MessageBox(0{HInstance}, PChar(Msg), 'Warning', MB_OK or MB_ICONWARNING);
  {$ENDIF}
  end;

  procedure CreateLobTemporary(Param: TOraParamDesc; OraLob: TOraLob);
  var
    LobType: TLobType;
  begin
    if Param.DataType = dtOraBlob then
      LobType := ltBlob
    else if Param.GetNational then
      LobType := ltNClob
    else
      LobType := ltClob;

    OraLob.OCISvcCtx := FConnection.OCISvcCtx;
    {$IFDEF HAVE_COMPRESS}
    if (Param.DataType = dtOraBlob) and (FCompressBlob in [cbServer, cbClientServer]) then
      OraLob.Compressed := True
    else
      OraLob.Compressed := False;
    {$ENDIF}
    OraLob.CreateTemporary(LobType);
    OraLob.WriteLob;
  end;

  procedure SyncIndicators(Param: TOraParamDesc; ArraySize: Integer);
  var
    i: integer;
    Obj: TSharedObject;
  begin
    for i := 0 to ArraySize - 1 do begin
      case Param.DataType of
        dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
          Obj := Param.GetItemObject(i);
          if Obj <> nil then
            TOraTimeStamp(Obj).FIndicatorPtr^ := Marshal.ReadInt16(Param.FIndicator, i * sizeof(sb2));
        end;
        dtIntervalYM, dtIntervalDS: begin
          Obj := Param.GetItemObject(i);
          if Obj <> nil then
            TOraInterval(Obj).FIndicatorPtr^ := Marshal.ReadInt16(Param.FIndicator, i * sizeof(sb2));
        end;
        dtNumber, dtBCD, dtFMTBCD, dtInt64, dtUInt64: begin
          Obj := Param.GetItemObject(i);
          if Obj <> nil then begin
            TOraNumber(Obj).FIndicatorPtr^ := Marshal.ReadInt16(Param.FIndicator, i * sizeof(sb2));
            if Param.DataType = dtNumber then
              CopyBuffer(PtrOffset(Param.FValueData, i * Param.FValueSize), TOraNumber(Obj).OCINumberPtr, Param.FValueSize);
          end;
        end;
      end;
    end;
  end;

  procedure SyncInOraLobs(Param: TOraParamDesc; ArraySize: Integer; IsTemporary: Boolean);
  var
    i: integer;
    OraLob: TOraLob;
  begin
    if (Param.ParamType = pdUnknown) or
       (Param.ParamType = pdInputOutput) and not FTemporaryLobUpdate
    then
      RaiseError(SNeedParamType);

    for i := 0 to ArraySize - 1 do begin
      OraLob := Param.GetItemObject(i) as TOraLob;
      OraLob.Cached := FCacheLobs or (Param.ParamType = pdInput);
      if OraLob.Cached and (OraLob.IsTemporary = IsTemporary) then
        if not Param.GetNull then begin
        {$IFDEF HAVE_COMPRESS}
          if (Param.DataType = dtOraBlob) and (FCompressBlob in [cbServer, cbClientServer]) then
            OraLob.Compressed := True
          else
            OraLob.Compressed := False;
        {$ENDIF}
          OraLob.WriteLob;
        end;
    end;
  end;

  procedure SyncOutOraLobs(Param: TOraParamDesc; ArraySize: Integer);
  var
    i: integer;
    OraLob: TOraLob;
  begin
    if (Param.ParamType = pdUnknown) or
       (Param.ParamType = pdInputOutput) and not FTemporaryLobUpdate
    then
      RaiseError(SNeedParamType);

    for i := 0 to ArraySize - 1 do begin
      OraLob := Param.GetItemObject(i) as TOraLob;
      OraLob.Cached := FCacheLobs;
      if OraLob.Cached then
        if not Param.GetNull then begin
          OraLob.ReadLob;
        {$IFDEF HAVE_COMPRESS}
          if (Param.DataType = dtOraBlob) and (FCompressBlob in [cbClient, cbClientServer]) then
            OraLob.Compressed := True
          else
            OraLob.Compressed := False;
        {$ENDIF}
        end
        else begin
          OraLob.FreeBlob;
          OraLob.Clear;
        end;
    end;
  end;

  procedure SyncOraObjects(Param: TOraParamDesc; ArraySize: Integer);
  var
    i: Integer;
    OraObj: TOraObject;
  begin
    for i := 0 to ArraySize - 1 do begin
      OraObj := Param.GetItemObject(i) as TOraObject;
      if (OraObj <> nil) and (not OraObj.IsNull) then
        OraObj.ReadLobs;
    end;
  end;

  procedure SyncOraXML(Param: TOraParamDesc; ArraySize: Integer);
  var
    i: Integer;
    OraXML: TOraXML;
  begin
    for i := 0 to ArraySize - 1 do begin
      OraXML := Param.GetItemObject(i) as TOraXML;
      if OraXML <> nil then
        OraXML.ReadXML;
    end;
  end;

const
  BufSize = 100;
var
  i: integer;
  Res: sword;
  PieceStatus: ub1;
  Blob: TBlob;
  Piece: PPieceHeader;
  Ptr: IntPtr;
  BufLen: ub4;
  Iteration: ub4;
  Index: ub4;
  NeedData: integer;
  IsData: integer;
  hBind: pOCIHandle;
  Buf: IntPtr;
  Param: TOraParamDesc;
  Mode: TParamDirection;
  CommitOnSuccess: boolean;
  OraLob: TOraLob;
  ArraySize: Integer;
begin
  CommitOnSuccess :=
    (FOCICallStyle = OCI80) and
    (FSQLType <> SQL_SELECT) and
    FAutoCommit and
    FConnection.AutoCommit;

  for i := 0 to FParams.Count - 1 do begin
    Param := TOraParamDesc(FParams[i]);
    if Param.DataType in [dtOraBlob, dtOraClob, dtWideOraClob, dtNClob] then begin
      if not FTemporaryLobUpdate then
        CommitOnSuccess := False;

      if Param.FArraySize > 1 then
        ArraySize := Param.FArraySize
      else
        ArraySize := 1;

      if FConnection.OCISvcCtx.Home.Direct then
        for Index := 0 to ArraySize - 1 do begin
          OraLob := TOraLob(Param.GetItemObject(Index));
          OraLob.FCharLength := FConnection.FCharLength;
          if not OraLob.IsUnicode and FConnection.FUseUnicode and (FConnection.FCharsetId = 0) then
             OraLob.FCharsetId := FConnection.FInternalCharsetId
           else
          {$IFDEF IS_UTF8_EXCLUDE_MEMO}
            OraLob.FCharsetId := 0;
          {$ELSE}
            OraLob.FCharsetId := FConnection.FCharsetId;
          {$ENDIF}
        end;

      if Param.ParamType in [pdUnknown, pdInput, pdInputOutput] then
        if FTemporaryLobUpdate then begin
          // standard parameter
          if ArraySize <= 1 then begin
            OraLob := Param.GetAsOraBlob;
            if (OraLob <> nil) and not OraLob.IsNull then
              CreateLobTemporary(Param, OraLob);
          end
          // DML array
          else begin
            for Index := 0 to ArraySize - 1 do begin
              OraLob := TOraLob(Param.GetItemObject(Index));
              if (OraLob <> nil) and not OraLob.IsNull then
                CreateLobTemporary(Param, OraLob);
            end;
            // update array of OCILobLocator that were recreated by CreateLobTemporary
            Param.SyncIndicator(FConnection);
          end;
        end
        else
          SyncInOraLobs(Param, ArraySize, True);
      end;
    end;

  if (FSQLType = SQL_SELECT) and (FCursorRef <> nil) and (FCursorRef.FScrollable) then
    Res := InternalExecute(OCI_SCROLLABLE_CURSOR)
  else if CommitOnSuccess then
    Res := InternalExecute(OCI_COMMIT_ON_SUCCESS)
  else
    Res := InternalExecute(OCI_DEFAULT);

  // remember ROWID value for updates without explicit prepare
  if FStoreRowId then
    FRowId := GetRowId
  else
    FRowId := '';

  // Init Implisit ResultSets
  InitResultSets;

  // Reading/Writing Piecewise data
  if Res <> OCI_SUCCESS then begin
    Buf := Marshal.AllocHGlobal(BufSize + 1);
    try
      if FOCICallStyle = OCI80 then begin
        GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);
        repeat
          for i := 0 to FParams.Count - 1 do
            if TOraParamDesc(FParams[i]).FHandle = hBind then
              break;
          Assert(i < FParams.Count);

          Blob := TOraParamDesc(FParams[i]).GetAsBlobRef;

          if Mode = pdInput then begin
            Piece := Blob.FirstPiece;
            if IntPtr(Piece) <> nil then
              repeat
                BufLen := Piece.Used;
                if IntPtr(Piece.Next) = nil then
                  PieceStatus := OCI_LAST_PIECE;

                SetPI(hBind, OCI_HTYPE_BIND, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, nil);

                Res := InternalExecute(OCI_DEFAULT);

                GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

                Piece := Piece.Next;
              until (Res = OCI_SUCCESS) or (PieceStatus = OCI_FIRST_PIECE) or {WAR} (PieceStatus = OCI_ONE_PIECE)
            else begin
              BufLen := 0;
              SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, Buf, BufLen, nil);
              Res := InternalExecute(OCI_DEFAULT);
              GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);
            end;
          end
          else
          if Mode in [pdOutput, pdResult] then begin
            Blob.Clear;
            repeat
              BufLen := Blob.PieceSize;
              Blob.AllocPiece(Piece, BufLen);

              SetPI(hBind, OCI_HTYPE_BIND, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, nil);

              Res := InternalExecute(OCI_DEFAULT);

              Piece.Used := BufLen;
              if BufLen < Cardinal(Blob.PieceSize) then
                Blob.ReallocPiece(Piece, BufLen);
              Blob.AppendPiece(Piece);

              GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);
            until (Res = OCI_SUCCESS) or (PieceStatus = OCI_FIRST_PIECE) or (PieceStatus = OCI_ONE_PIECE);
          end;
        until Res = OCI_SUCCESS;
      end
      else
      if FOCICallStyle = OCI73 then begin
        NeedData := OCI_STILL_IS_PIECE1;
        IsData := OCI_STILL_IS_PIECE;

        // IN params
        for i := 0 to FParams.Count - 1 do begin
          Param := TOraParamDesc(FParams[i]);
          if (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) and
             (Param.ParamType in [pdInput, pdInputOutput]) and not Param.GetNull// and (FOCICallStyle = OCI73) or {for OCI73}
            //((GetParam(i).ParamType in [pdInput, pdInputOutput]) and{or} not GetParam(i).IsNull) and (FOCICallStyle = OCI80))    {for OCI80}
          then begin
            if Res <> NeedData then begin
              Warning(STooManyInputParams);
              break;
            end;

            GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

            Blob := Param.GetAsBlobRef;
            Assert(Blob <> nil);

            Piece := Blob.FirstPiece;
            if IntPtr(Piece) <> nil then
              repeat
                BufLen := Piece.Used;
                if IntPtr(Piece.Next) = nil then
                  PieceStatus := OCI_LAST_PIECE;

                SetPI(hBind, OCI_HTYPE_BIND, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, nil);

                Res := InternalExecute(OCI_DEFAULT);

                GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

                Piece := Piece.Next;
              until (Res = OCI_SUCCESS) or (PieceStatus = OCI_FIRST_PIECE) or {WAR} (PieceStatus = OCI_ONE_PIECE)
            else begin
              BufLen := 0;
              SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, Buf, BufLen, nil);
              Res := InternalExecute(OCI_DEFAULT);
            end;
          end;
        end;

        if (Res = NeedData) then begin
          Warning(SNotEnoughInputParams);
          while Res = NeedData do begin
            BufLen := 0;
            SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, Buf, BufLen, nil);
            Res := InternalExecute(OCI_DEFAULT);
          end;
        end;

        // OUT params
        for i := 0 to FParams.Count - 1 do begin
          Param := TOraParamDesc(FParams[i]);
          if (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) and
             (Param.ParamType in [pdInputOutput, pdOutput, pdResult])
          then begin
            if Res <> IsData then begin
              Warning(STooNanyOutputParams);
              break;
            end;

            GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

            Blob := Param.GetAsBlobRef;
            Assert(Blob <> nil);

            Blob.Clear;

            repeat
              BufLen := Blob.PieceSize;
              Blob.AllocPiece(Piece, BufLen);

              SetPI(hBind, OCI_HTYPE_BIND, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, nil);

              Res := InternalExecute(OCI_DEFAULT);

              Piece.Used := BufLen;
              if BufLen < Cardinal(Blob.PieceSize) then
                Blob.ReallocPiece(Piece, BufLen);
              Blob.AppendPiece(Piece);

              GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

            until (Res = OCI_SUCCESS) or (PieceStatus = OCI_FIRST_PIECE) or (PieceStatus = OCI_ONE_PIECE);
          end;
        end;

        if Res = IsData then begin
          Warning(SNotEnoughOutputParams);
          while Res = IsData do begin
            BufLen := BufSize;
            GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode); // ???
            SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, Buf, BufLen, nil);
            Res := InternalExecute(OCI_DEFAULT);
          end;
        end;

        // OCI73 bug - damage data of last piece
        BufLen := 0;
        SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, nil, BufLen, nil);
      end
      else begin
        CheckOCI;
      end;
    finally
      Marshal.FreeHGlobal(Buf);
    end;
  end;

  // Reading/Writing LOBs, Updating DML Array params
  if (Res = OCI_SUCCESS) and (FOCICallStyle = OCI80) and (FRowsProcessed > 0) then
    for i := 0 to FParams.Count - 1 do begin
      Param := TOraParamDesc(FParams[i]);
      if Param.FArraySize > 1 then
        ArraySize := Param.FArraySize
      else
        ArraySize := 1;

      if (Param.DataType = dtFixedChar) or (Param.SubDataType in [dtFixedChar, dtFixedNChar]) then begin
        if FTrimFixedChar and (Param.ParamType in [pdInputOutput, pdOutput, pdResult]) then
          Param.TrimFixedChar;
      end
      else if (Param.DataType = dtFixedWideChar) or (Param.SubDataType in [dtFixedWideChar, dtFixedNWideChar]) then begin
        if FTrimFixedChar and (Param.ParamType in [pdInputOutput, pdOutput, pdResult]) then
          Param.TrimWideFixedChar;
      end
      else if Param.DataType in [dtOraBlob, dtOraClob, dtWideOraClob, dtNClob] then begin
        if not FTemporaryLobUpdate and (Param.ParamType in [pdUnknown, pdInput, pdInputOutput]) then
          SyncInOraLobs(Param, ArraySize, False);
        if Param.ParamType in [pdUnknown, pdInputOutput, pdOutput, pdResult] then
          SyncOutOraLobs(Param, ArraySize);
      end
      else if Param.DataType in [dtObject, dtAnyData, dtArray, dtTable] then begin
        if Param.ParamType in [pdInputOutput, pdOutput, pdResult] then
          SyncOraObjects(Param, ArraySize);
      end
      else if Param.DataType = dtXML then begin
        if Param.ParamType in [pdInputOutput, pdOutput, pdResult] then
          SyncOraXML(Param, ArraySize);
      end
      else if (ArraySize > 1) and (Param.ParamType in [pdUnknown, pdInputOutput, pdOutput, pdResult]) then
        SyncIndicators(Param, ArraySize);
    end;

  // AutoCommit
  if (Res = OCI_SUCCESS) and
     (FSQLType <> SQL_SELECT) and
     FAutoCommit and
     FConnection.AutoCommit and
     not CommitOnSuccess
  then
    FConnection.GetInternalTransaction.Commit;
end;

function TOCICommand.CallbackInBind(Bind: pOCIBind; Iter: ub4; Index: ub4;
  var Buf: IntPtr; var BufLen: ub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;
var
  i: word;
  Param: TOraParamDesc;
  Blob: TBlob;
  Piece: PPieceHeader;
begin
  for i := 0 to FParams.Count - 1 do
    if TOraParamDesc(FParams[i]).FHandle = Bind then
      break;
  Assert(i < FParams.Count);
  Param := TOraParamDesc(FParams[i]);
  Blob := Param.GetItemObject(Iter) as TBlob;

  Piece := Blob.FirstPiece;
  if PieceStatus = OCI_FIRST_PIECE then
    Param.FBlobPiece := 0
  else begin
    i := 0;
    while (IntPtr(Piece) <> nil) and (i < Param.FBlobPiece) do begin
      Piece := Piece.Next;
      Inc(i);
    end;
  end;

  if IntPtr(Piece) <> nil then begin
    Buf := PieceData(Piece);
    BufLen := Piece.Used;
    Ind := PtrOffset(Param.IndicatorPtr, Iter * sizeof(sb2));
    if IntPtr(Piece.Next) = nil then
      PieceStatus := OCI_LAST_PIECE
    else
      if Param.FBlobPiece = 0 then
        PieceStatus := OCI_FIRST_PIECE
      else
        PieceStatus := OCI_NEXT_PIECE;
    Inc(Param.FBlobPiece);
  end
  else begin
    Buf := nil;
    BufLen := 0;
    Ind := nil;
    PieceStatus := OCI_LAST_PIECE;
  end;

  Result := OCI_CONTINUE;
end;

function TOCICommand.CallbackOutBind(Bind: pOCIBind; Iter: ub4; Index: ub4;
  var Buf: IntPtr; var BufLen: pub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;
var
  i: integer;
  Blob: TBlob;
  Piece: PPieceHeader;
  Len: cardinal;
begin
  for i := 0 to FParams.Count - 1 do
    if TOraParamDesc(FParams[i]).FHandle = Bind then
      break;
  Assert(i < FParams.Count);

  Blob := TOraParamDesc(FParams[i]).GetAsBlobRef;

  if PieceStatus = OCI_ONE_PIECE then
    Blob.Clear;

  Len := Blob.PieceSize;
  Blob.AllocPiece(Piece, Len);
  Blob.AppendPiece(Piece);

  Buf := PieceData(Piece);

  Piece.Used := Len;
  BufLen := PieceUsedPtr(Piece);

  Ind := TOraParamDesc(FParams[i]).IndicatorPtr;

  PieceStatus := OCI_NEXT_PIECE;

  Result := OCI_CONTINUE;
end;

function OCICallbackInBind(ictxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
  var bufp: IntPtr; var alen: ub4; var piece: ub1; var indp: IntPtr): sb4; cdecl;
begin
  Result := TOCICommand(GetGCHandleTarget(ictxp)).CallbackInBind(bindp, iter, index, bufp, alen,
    piece, indp);
end;

function OCICallbackOutBind(octxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
  var bufp: IntPtr; var alen: pub4; var piece: ub1; var indp: IntPtr; var rcodep: pub2): sb4; cdecl;
begin
  Result := TOCICommand(GetGCHandleTarget(octxp)).CallbackOutBind(bindp, iter, index, bufp, alen,
    piece, indp);
  rcodep := nil;
end;

procedure TOCICommand.DefineData(Field: TOCIFieldDesc; DataPtr, IndPtr, LenPtr: IntPtr; BufSkip, IndSkip, LenSkip: integer);
const
  PrefetchLobLength: WordBool = True;
var
  OraType: word;
  hDefine: pOCIDefine;
  ValuePtr: IntPtr;
  Size: Cardinal;
  CharsetId: Integer;//sb2
  CharsetForm : Integer;
begin
  OraType := GetOraType(Field.DataType, Field.SubDataType, True);

  if (Field.DataType in [dtWideString, dtExtWideString]) or
     (Field.SubDataType in [dtWideString, dtNWideString, dtFixedWideChar, dtFixedNWideChar])
  then begin// in future use DataSize
    Size := Field.Length * 2;
    if not (Field.SubDataType in [dtFixedWideChar, dtFixedNWideChar]) or (Size = 0) then
      Size := Size + 2;
  end
  else
  if (Field.DataType in [dtString, dtExtString]) or
     (Field.SubDataType in [dtString, dtNString, dtFixedChar, dtFixedNChar])
  then  begin// in future use DataSize
    Size := Field.Length;
    if not (Field.SubDataType in [dtFixedChar, dtFixedNChar]) or (Size = 0) then
      Size := Size + 1;
  end
  else
    case Field.DataType of
      dtExtVarBytes:
        Size := Field.Length;
      dtNumber, dtInt64, dtUInt64, dtBCD, dtFMTBCD:
        Size := OCI_NUMBER_SIZE;
    else
      Size := Field.Size;
    end;

  if FOCICallStyle = OCI73 then begin
    case Field.DataType of
      dtVarBytes, dtExtVarBytes: begin
          LenPtr := DataPtr;
          LenSkip := BufSkip;
          ValuePtr := PtrOffset(DataPtr, 2);
      end;
      else
        ValuePtr := DataPtr;
    end;

    if (BufSkip > 0) or (IndSkip > 0) then
      Check(OCI7.odefinps(FCursorRef.CDA, 1, Field.ActualFieldNo, ValuePtr, Size, OraType, 0, IndPtr, nil, 0, 0, LenPtr, pub2(0), BufSkip, IndSkip, LenSkip, 0))
    else
      Check(OCI7.odefin(FCursorRef.CDA, Field.ActualFieldNo, ValuePtr, Size, OraType, -1, IndPtr, nil, -1, -1, LenPtr, pub2(0))); // not Array
  end
  else
  if FOCICallStyle = OCI80 then begin
    hDefine := nil;

    if Field.DataType in [dtObject, dtReference, dtXML, dtAnyData, dtArray, dtTable] then
      ValuePtr := nil
    else
      ValuePtr := DataPtr;

    if Field.DataType in [dtXML, dtAnyData] then
      IndPtr := nil;

    Check(OCI8.OCIDefineByPos(FCursorRef.OCIStmt, hDefine, OCISvcCtx.hOCIError, Field.ActualFieldNo,
      ValuePtr, Size, OraType, IndPtr, LenPtr, nil, OCI_DEFAULT));

    CharsetId := 0;
    if (Field.DataType in [dtWideString, dtExtWideString]) or
       (Field.DataType in [dtBlob, dtWideMemo]) and
       (Field.SubDataType in [dtWideString, dtNWideString, dtFixedWideChar, dtFixedNWideChar])
    then
      CharsetId := Integer(OCI_UTF16ID)
    else
    if (Field.DataType in [dtString, dtExtString]) or
       (Field.DataType in [dtBlob, dtMemo]) and
       (Field.SubDataType in [dtString, dtNString, dtFixedChar, dtFixedNChar])
    then begin
      CharsetId := Integer(FConnection.FCharsetId);
      if (CharsetId = 0) and FConnection.UnicodeEnvironment then
        CharsetId := FConnection.FInternalCharsetId;
    end;

    if Field.SubDataType in [dtNString, dtNWideString, dtFixedNChar, dtFixedNWideChar, dtNClob] then begin
      CharsetForm := SQLCS_NCHAR;
      Check(OCI8.OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, CharsetForm, 0, OCI_ATTR_CHARSET_FORM, OCISvcCtx.hOCIError));
    end;

    if CharsetId > 0 then
      Check(OCI8.OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, CharsetId, 0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));

    case Field.DataType of
      dtOraBlob, dtOraClob, dtWideOraClob:
        // Prefetch LOB is supported in Oracle 11 or higher
        if IsLobPrefetchAllowed(Field.IsNational, Field.DataType = dtWideOraClob) then begin
          Check(OCI8.OCIAttrSet1(hDefine, OCI_HTYPE_DEFINE, @PrefetchLobLength, 0, OCI_ATTR_LOBPREFETCH_LENGTH, OCISvcCtx.hOCIError));
          Check(OCI8.OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, FPrefetchLobSize, 0, OCI_ATTR_LOBPREFETCH_SIZE, OCISvcCtx.hOCIError));
        end;
      dtObject, dtXML, dtAnyData, dtArray, dtTable:
        Check(OCI8.OCIDefineObject(hDefine, OCISvcCtx.hOCIError, TOraType(Field.ObjectType).TDO, DataPtr, nil, nil {IntPtr(Ind)}, nil));
      dtReference:
        Check(OCI8.OCIDefineObject(hDefine, OCISvcCtx.hOCIError, TOraType(Field.ObjectType).TDO, DataPtr, nil, nil , nil));
    end;

    if (BufSkip > 0) or (IndSkip > 0) then
      Check(OCI8.OCIDefineArrayOfStruct(hDefine, OCISvcCtx.hOCIError, BufSkip, IndSkip, LenSkip, 0));

    Field.Handle := hDefine;
  end
  else
    CheckOCI;
end;

procedure TOCICommand.DefineDynamic7(Field: TOCIFieldDesc; Buf: IntPtr; Ind: psb2);
var
  OraType: word;
begin
  CheckOCI73;

  OraType := GetOraType(Field.DataType, Field.SubDataType, True);

  Check(OCI7.odefinps(FCursorRef.CDA, 0, Field.ActualFieldNo, Buf, MaxBlobSize, OraType, 0,
    Ind, nil, 0, 0, pub2(0), pub2(0), 0, 0, 0, 0));
end;

procedure TOCICommand.DefineDynamic8(Field: TOCIFieldDesc; {Buf: IntPtr; Ind: psb2;}
  Owner: IntPtr; Proc: IntPtr; CharsetId: integer);
var
  OraType: word;
  hDefine: pOCIDefine;
begin
  CheckOCI80;

  OraType := GetOraType(Field.DataType, Field.SubDataType, True);

  hDefine := nil;
  Check(OCI8.OCIDefineByPos(FCursorRef.OCIStmt, hDefine, OCISvcCtx.hOCIError, Field.ActualFieldNo,
    nil, MaxBlobSize, OraType, nil, nil, nil, OCI_DYNAMIC_FETCH));

  Check(OCI8.OCIDefineDynamic(hDefine, OCISvcCtx.hOCIError, Owner, Proc));

  if CharsetId > 0 then
    Check(OCI8.OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, CharsetId, 0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));

  Field.Handle := hDefine;
end;

function TOCICommand.InternalFetch7(Rows: word): word;
var
  Res: sword;
begin
//  In PtrCDA^.rpc  problem with cursor ???
  FConnection.Lock;
  try
    Res := OCI7.ofen(FCursorRef.CDA, Rows);
  finally
    FConnection.Release;
  end;

  if FCursorRef.CDA.rpc > Cardinal(FFetchedRows) then
    Result := FCursorRef.CDA.rpc - Cardinal(FFetchedRows)
  else begin
    Result := 0;
    Res := OCI_NO_DATA_FOUND;
  end;
  FFetchedRows := FCursorRef.CDA.rpc;

  if Res <> 0 then begin
    if Res = OCI_NO_DATA_FOUND then begin
      InternalCancel;
    end
    else
      Check(Res);
  end;
end;

function TOCICommand.InternalFetch8(Rows: word; Orientation: integer; Offset: integer): word;
var
  Res: sword;
  RowsProc: integer;
begin
  FConnection.Lock;
  try
    if FCommandTimeout > 0 then
      FConnection.RunTimeoutThread(FCommandTimeout);
    try
      if Orientation = OCI_FETCH_NEXT then
        Res := OCI8.OCIStmtFetch(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, Rows, OCI_FETCH_NEXT, OCI_DEFAULT)
      else
        Res := OCI8.OCIStmtFetch2(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, Rows, Orientation, Offset, OCI_DEFAULT);
    finally
      if FCommandTimeout > 0 then
        FConnection.StopTimeoutThread;
    end;
  finally
    FConnection.Release;
  end;

  Check(OCI8.OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, RowsProc, nil, OCI_ATTR_ROW_COUNT, OCISvcCtx.hOCIError));
  if RowsProc > FFetchedRows then
    Result := RowsProc - FFetchedRows
  else
    Result := 0; // bug with 'select level from dual connect by level<=1000' under Direct mode
  FFetchedRows := RowsProc;

  if Res = OCI_NO_DATA then begin
    if not FCursorRef.FScrollable then
      InternalCancel
  end
  else
    try
      Check(Res);
    except
      Close;
      raise;
    end;
end;

function TOCICommand.InternalExecuteFetch8(Rows: word): word;
var
  Res: sword;
  RowsProc: Integer;
begin
  Res := InternalExecute(OCI_DEFAULT, Rows);

  if Res <> 0 then begin
    Check(OCI8.OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, RowsProc, nil, OCI_ATTR_ROW_COUNT, OCISvcCtx.hOCIError));
    Result := RowsProc - FFetchedRows;
    FFetchedRows := RowsProc;

    if Res = OCI_NO_DATA then
      if not FCursorRef.FScrollable then
        InternalCancel;
  end
  else begin
    Check(OCI8.OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, FFetchedRows, nil, OCI_ATTR_ROW_COUNT, OCISvcCtx.hOCIError));
    Result := Rows;
  end;
end;

function TOCICommand.InternalFetch(Rows: word; Orientation: integer = OCI_FETCH_NEXT; Offset: integer = 0): word;
var
  FetchingAll: boolean;
begin
  FetchingAll := GetCursorState = csFetchingAll;
  CheckActive;

  if FOCICallStyle = OCI73 then
    Result := InternalFetch7(Rows)
  else if FOCICallStyle = OCI80 then begin
    if (GetCursorState < csExecuting) then begin
      FetchingAll := GetCursorState = csExecuteFetchAll;
      Result := InternalExecuteFetch8(Rows);
    end
    else
      Result:= InternalFetch8(Rows, Orientation, Offset);
  end
  else begin
    Result := 0;
    CheckOCI;
  end;

  if GetCursorState < csFetching then
    if FetchingAll then
      SetCursorState(csFetchingAll)
    else
      SetCursorState(csFetching);
end;

// For piecewise
function TOCICommand.InternalFetchPiece7: integer;
var
  Res: sword;
begin
  FConnection.Lock;
  try
    Res := OCI7.ofen(FCursorRef.CDA, 1);
  finally
    FConnection.Release;
  end;

  if Res <> 0 then begin
    if Res = OCI_NO_DATA_FOUND then begin
      InternalCancel;
    end
    else
      if Res <> OCI_STILL_IS_PIECE then
        Check(Res);
  end;

  Result := Res;
end;

function TOCICommand.InternalFetchPiece8(Orientation: integer; Offset: integer): integer;
var
  Res: sword;
begin
  FConnection.Lock;
  try
    if FCommandTimeout > 0 then
      FConnection.RunTimeoutThread(FCommandTimeout);
    try
      if Orientation = OCI_FETCH_NEXT then
        Res := OCI8.OCIStmtFetch(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, 1, OCI_FETCH_NEXT, OCI_DEFAULT)
      else
        Res := OCI8.OCIStmtFetch2(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, 1, Orientation, Offset, OCI_DEFAULT);
    finally
      if FCommandTimeout > 0 then
        FConnection.StopTimeoutThread;
    end;
  finally
    FConnection.Release;
  end;

  if Res <> 0 then begin
    if Res = OCI_NO_DATA then // OCI_SUCCESS_WITH_INFO
      if FCursorRef.FScrollable then
      else
        InternalCancel
    else
      if Res <> OCI_NEED_DATA then
        Check(Res);
  end;

  Result := Res;
end;

function TOCICommand.InternalFetchPiece(Orientation: integer = OCI_FETCH_NEXT; Offset: integer = 0): integer;
begin
  CheckActive;

  if FOCICallStyle = OCI73 then begin
    Result := InternalFetchPiece7;
  end
  else
  if FOCICallStyle = OCI80 then begin
    Result := InternalFetchPiece8(Orientation, Offset);
  end
  else begin
    Result := 0;
    CheckOCI;
  end;

  if GetCursorState < csFetching then
    SetCursorState(csFetching);
end;

procedure TOCICommand.InternalCancel;
var
  Res: sword;
begin
  CheckActive;
  try
    if FOCICallStyle = OCI73 then begin
      FConnection.Lock;
      try
        Res := OCI7.ocan(FCursorRef.CDA);
      finally
        FConnection.Release;
      end;
      Check(Res);
    end
    else if FOCICallStyle = OCI80 then begin
//      FConnection.Lock;
//      try
//        Res := OCI8.OCIStmtFetch(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, 0, OCI_FETCH_NEXT, OCI_DEFAULT);
//      finally
//        FConnection.Release;
//      end;
//      Check(Res);
    end
    else
      CheckOCI;
  finally
    SetCursorState(csFetched);
  end;
end;

procedure TOCICommand.InternalClose;
var
  Res: sword;
begin
  CheckOCI73;
  CheckActive;

  Lock;
  try // For Busy
    FConnection.Lock;
    try
      Res := OCI7.oclose(FCursorRef.CDA);
    finally
      FConnection.Release;
    end;
    Check(Res);
  finally
    Release;
    SetCursorState(csInactive);
  end;
end;

procedure TOCICommand.Finish;
begin
  CheckActive;

  try
    if FOCICallStyle = OCI73 then begin
      try
        InternalClose;
      finally
        FCursorRef.FreeCursor;
      end;
    end
    else
    if FOCICallStyle = OCI80 then begin
      Lock;
      try
        FConnection.Lock;
        try
          // Free statement handle
          FCursorRef.FreeCursor;
        finally
          FConnection.Release;
        end;
      finally
        Release;
      end;
    end
    else
      CheckOCI;
  finally
    SetCursorState(csInactive);
    FSQLType := SQL_UNKNOWN;
  end;
end;

procedure TOCICommand.Close;
begin
  // Free implicit result sets
  ReleaseResultSets;

  Finish;

  ReleaseOCISvcCtx;

  inherited;
end;

function TOCICommand.NativeCursor: boolean;
begin
  Result := FCursorRef = FCursor;
end;

function TOCICommand.RowsReturn: boolean;
begin
  Result := (FSQLType = SQL_SELECT) or
    (FCursorRef <> FCursor) and (FCursorRef.State >= csExecuted);
end;

// Check return SQL statement rows
procedure TOCICommand.CheckRowsReturn;
begin
  if not RowsReturn then
    RaiseError(SNotRows);
end;

procedure TOCICommand.InitResultSets;
var
  i: integer;
  Count: Integer;
  StatementType: ub4;
  ResultSet: TOraCursor;
begin
  if not OCISvcCtx.Home.Direct and (OCISvcCtx.Home.OCIVersion < 12000) then
    Exit;

  ReleaseResultSets;

  Check(OCI8.OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, Count, nil, OCI_ATTR_IMPLICIT_RESULT_COUNT, OCISvcCtx.hOCIError));
  if Count = 0 then
    Exit;

  FResultSets := TList.Create;
  for i := 0 to Count - 1 do begin
    ResultSet := TOraCursor.Create;
    try
      ResultSet.Init(OCISvcCtx, FCursorRef.OCICallStyle);
//      ResultSet.AllocCursor(True);
      if OCISvcCtx.Home.Direct then
        Check(OCI8.OCIStmtGetNextResult(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, ResultSet.FphOCIStmt, StatementType, i))
      else
        Check(OCI8.OCIStmtGetNextResult(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, ResultSet.FphOCIStmt, StatementType, OCI_DEFAULT));
      if StatementType = OCI_RESULT_TYPE_SELECT then begin
        ResultSet.FState := csExecuted;
        FResultSets.Add(ResultSet);
      end
      else
        raise Exception.Create('Unknown result set rtype.');
    except
      ResultSet.Free;
      raise;
    end
  end;
end;

procedure TOCICommand.ReleaseResultSets;
var
  i: integer;
begin
  if FResultSets = nil then
    Exit;

  for i := 0 to FResultSets.Count - 1 do
    TOraCursor(FResultSets[i]).Free;
  FreeAndNil(FResultSets);
end;

procedure TOCICommand.GetPI(var Handle: pOCIHandle; var Piece: byte;
  var Buf: IntPtr; var Iteration: cardinal; var Index: cardinal; var Mode: TParamDirection);
var
  Res: sword;
  HType: ub4;
  IOMode: ub1;
  hIteration, hIndex, hPiece: IntPtr;
  hHType: IntPtr;
  hIOMode: IntPtr;
begin
  if FOCICallStyle = OCI73 then begin
    FConnection.Lock;
    try
      Res := OCI7.ogetpi(FCursorRef.CDA, Piece, Buf, Iteration, Index);
    finally
      FConnection.Release;
    end;
    Check(Res);

    Handle := nil;
    HType := 0;
    Mode := pdUnknown;
  end
  else
  if FOCICallStyle = OCI80 then begin
    FConnection.Lock;
    try
      hIteration := @Iteration;
      hIndex := @Index;
      hPiece := @Piece;
      hHType := @HType;
      hIOMode := @IOMode;
      Res := OCI8.OCIStmtGetPieceInfo(FCursorRef.OCIStmt, OCISvcCtx.hOCIError, Handle, hHType,
        hIOMode, hIteration, hIndex, hPiece);
    finally
      FConnection.Release;
    end;
    Check(Res);

    case IOMode of
      OCI_PARAM_IN:
        Mode := pdInput;
      OCI_PARAM_OUT:
        Mode := pdOutput;
    else
      Mode := pdUnknown;
    end;
  end
  else
    CheckOCI;
end;

procedure TOCICommand.SetPI(Handle: pOCIHandle; HType: cardinal; Piece: byte;
  Buf: IntPtr; var BufLen: cardinal; Ind: psb2);
var
  Res: sword;
  HGlobal: IntPtr;
begin
  if FOCICallStyle = OCI73 then begin
    FConnection.Lock;
    try
      Res := OCI7.osetpi(FCursorRef.CDA, Piece, Buf, BufLen);
    finally
      FConnection.Release;
    end;
    Check(Res);
  end
  else
  if FOCICallStyle = OCI80 then begin
    FConnection.Lock;
    try
      HGlobal := @BufLen;
      Res := OCI8.OCIStmtSetPieceInfo(Handle, HType, OCISvcCtx.hOCIError, Buf, HGlobal, Piece, Ind, nil);
    finally
      FConnection.Release;
    end;

    Check(Res);
  end
  else
    CheckOCI;
end;

procedure TOCICommand.DetectDataType(DBType: Word; DBLength: Integer; DBScale: SmallInt;
  ObjectType: TObjectType; LongStrings: boolean; out DataType: Word);
begin
  case DBType of
    oraChar,
    oraVarchar2,
    oraNChar,
    oraNVarChar2: begin
      if LongStrings or (DBLength <= 255) then
        DataType := dtString
      else
        DataType := dtMemo;

      if FConnection.FUseUnicode then begin
        if DataType = dtString then
          DataType := dtWideString
        else if DataType = dtMemo then
          DataType := dtWideMemo;
      end;
    end;
  {$IFNDEF UNIDACPRO}
    OraDataTypeMap.oraNumber:
  {$ELSE}
    OraDataTypeMapUni.oraNumber:
  {$ENDIF}
      if FFieldsAsString or FNumberAsString then
        if FConnection.FUseUnicode then
          DataType := dtWideString
        else
          DataType := dtString
      else begin
        if (FUseDefaultDataTypes or FConnection.FEnableIntegers) and (DBScale = 0) and
           (DBLength <= GetSmallintPrecision)
        then
          DataType := dtSmallint
        else if (FUseDefaultDataTypes or FConnection.FEnableIntegers) and (DBScale = 0) and
                (DBLength <= GetIntegerPrecision)
        then
          DataType := dtInteger
        else if (FUseDefaultDataTypes or FConnection.FEnableIntegers) and (DBScale = 0) and
                (DBLength <= GetLargeIntPrecision)
        then
          DataType := dtLargeint
        else if not FUseDefaultDataTypes and (FConnection.EnableBCD or EnableBCD) and
                (DBScale >= 0) and (DBLength <= GetBCDPrecision) and (DBScale <= GetBCDScale)
        then
          DataType := dtBCD
        else if DBLength <= GetFloatPrecision then
          DataType := dtFloat
        else if not FUseDefaultDataTypes and (FConnection.EnableFMTBCD or EnableFMTBCD) and
                (DBScale >= 0) and (DBLength <= GetFmtBCDPrecision) and (DBScale <= GetFmtBCDScale)
        then
          DataType := dtFMTBCD
        else if not FUseDefaultDataTypes and FConnection.FEnableNumbers then
          DataType := dtNumber
        else
          DataType := dtFloat;
      end;
    oraDate:
      if FFieldsAsString then
        if FConnection.FUseUnicode then
          DataType := dtWideString
        else
          DataType := dtString
      else
        DataType := dtDateTime;
    oraRowID: // ROWID for Oracle 7
      if FConnection.FUseUnicode then
        DataType := dtWideString
      else
        DataType := dtString;
    oraURowID: // ROWID is bound as string
      if FConnection.FUseUnicode then
        DataType := dtWideString
      else
        DataType := dtString;
    oraRaw:
      if FFieldsAsString or FRawAsString then
        if FConnection.FUseUnicode then
          DataType := dtWideString
        else
          DataType := dtString
      else
        DataType := dtVarBytes;
    oraLong:
      if FConnection.FUseUnicode then
        DataType := dtWideMemo
      else
        DataType := dtMemo;
    oraLongRaw:
      DataType := dtBlob;
    oraClob, oraNClob: begin
      if FConnection.FUseUnicode and FConnection.FEnableWideOraClob then
        DataType := dtWideOraClob
      else
        DataType := dtOraClob;
    end;
    oraBlob:
      DataType := dtOraBlob;
    oraBFile:
      DataType := dtBFILE;
    oraCFile:
      DataType := dtCFILE;
    oraCursor:
      DataType := dtCursor;
    oraObject: begin
      if (OCISvcCtx.Home.OCIVersion < 9200) and
         (TOraType(ObjectType).DataType in [dtXML, dtAnyData])
      then
        RaiseError(SDataTypeNotSupported);
      DataType := TOraType(ObjectType).DataType;
    end;
    oraXML: begin
      if OCISvcCtx.Home.OCIVersion < 9200 then
        RaiseError(SDataTypeNotSupported);
      DataType := dtXML;
    end;
    oraAnyData: begin
      if OCISvcCtx.Home.OCIVersion < 9200 then
        RaiseError(SDataTypeNotSupported);
      DataType := dtAnyData;
    end;
    oraReference: begin
      if (OCISvcCtx.Home.OCIVersion < 9200) and
         (TOraType(ObjectType).DataType in [dtXML, dtAnyData])
      then
        RaiseError(SDataTypeNotSupported);
      DataType := dtReference;
    end;
    oraLabel:
      DataType := dtLabel;
    oraTimeStamp, oraTimeStampWithTimeZone, oraTimeStampWithLocalTimeZone,
    oraIntervalYM, oraIntervalDS:
    begin
      if OCISvcCtx.Home.OCIVersion < 9000 then
        RaiseError(SDataTypeNotSupported);

      if FFieldsAsString
        or FConnection.FIntervalAsString and (DBType in [oraIntervalYM, oraIntervalDS])
      {$IFDEF FPC}
        or FConnection.FTimeStampAsString and (DBType in [oraTimeStamp, oraTimeStampWithTimeZone, oraTimeStampWithLocalTimeZone])
      {$ENDIF}
      then
        if FConnection.FUseUnicode then
          DataType := dtWideString
        else
          DataType := dtString
      else begin
        case DBType of
          oraIntervalYM:
            DataType := dtIntervalYM;
          oraIntervalDS:
            DataType := dtIntervalDS;
          else begin
          {$IFNDEF FPC}
            if FConnection.FEnableSQLTimeStamp then
              DataType := dtSQLTimeStamp
            else
          {$ENDIF}
              case DBType of
                oraTimeStamp:
                  DataType := dtTimeStamp;
                oraTimeStampWithTimeZone:
                  DataType := dtTimeStampTZ;
                oraTimeStampWithLocalTimeZone:
                  DataType := dtTimeStampLTZ;
              end;
          end;
        end;
      end;
    end;
    oraUndefined:
      DataType := dtUndefined;
    oraFloat:
      DataType := dtFloat;
    oraBinaryFloat:
    //{$IFDEF VER14P}
    //  DataType := dtSingle;
    //{$ELSE}
      DataType := dtFloat;
    //{$ENDIF}
    oraBinaryDouble:
      DataType := dtFloat;
    else
      RaiseError(SDataTypeNotSupported);
  end;
end;

/// DescSP performs low-level call of the SYS.DBMS_DESCRIBE.DESCRIBE_PROCEDURE
/// to retrieve procedure params. Should be used in InitProcParams7 instead of
/// odessp in Direct mode.
{$IFNDEF VER7P}
{$O-}
{$ENDIF}
function TOCICommand.DescribeStoredProc(const objnam: string; ovrld: pub2; pos: pub2; level: pub2;
  argnam: IntPtr; arnlen: pub2; dtype: pub2; defsup: pub1; mode: pub1;
  dtsiz: pub4; prec: psb2; scale: psb2; radix: pub1;
  spare: pub4; var arrsiz: ub4): sword;
var
  hOCIStmt: pOCIStmt;
  SQLText: string;
  hBind: pOCIBind;
  i, Size: integer;
  Indicator: IntPtr;
  p, PObjName, PArrSize: IntPtr;
  //ValuePtr: Integer;
begin
  {Result := OCIAttrGet2(FConnection.GetSvcCtx, OCI_HTYPE_SVCCTX, ValuePtr, nil, OCI_ATTR_ENV, hOCIError);
  if Result <> OCI_SUCCESS then
    Exit;}

  /// alloc statement handle
  Result := OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hOCIStmt, OCI_HTYPE_STMT, 0, nil);
  if Result <> OCI_SUCCESS then
    Exit;

  try
    /// prepare
    SQLText :=
      'begin' +
      ' sys.dbms_describe.describe_procedure(:object_name, null, null, :overload,' +
      ' :position, :level, :argument, :datatype, :default, :in_out, :length,' +
      ' :precision, :scale, :radix, :spare);' +
      'end;';
    p := StringToHGlobalOCI(SQLText, Size, OCISvcCtx.UnicodeEnv);
    try
      Result := OCI8.OCIStmtPrepare(hOCIStmt, OCISvcCtx.hOCIError, p, Size, OCI_NTV_SYNTAX, OCI_DEFAULT);
    finally
      FreeStringOCI(p, OCISvcCtx.UnicodeEnv);
    end;

    if Result <> OCI_SUCCESS then
      Exit;

    /// bind object_name param
    PObjName := StringToHGlobalOCI(objnam, Size, False);
    try
      Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 1, PObjName, Size + 1,
        SQLT_STR, nil, nil, nil, 0, nil, OCI_DEFAULT);
      if Result <> OCI_SUCCESS then
        Exit;

      /// bind overload param
      PArrSize := @arrsiz;
      Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 2, ovrld,
        sizeof(ub2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);

      if Result <> OCI_SUCCESS then
        Exit;
      Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub2), 0, 0, 0);
      if Result <> OCI_SUCCESS then
        Exit;

      /// bind position param
      Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 3, pos,
        sizeof(ub2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
      if Result <> OCI_SUCCESS then
        Exit;
      Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub2), 0, 0, 0);
      if Result <> OCI_SUCCESS then
        Exit;

    /// bind level param
      Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 4, level,
        sizeof(ub2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
      if Result <> OCI_SUCCESS then
        Exit;
      Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub2), 0, 0, 0);
      if Result <> OCI_SUCCESS then
        Exit;

      /// bind argument param
      for i := 0 to Marshal.ReadInt32(PArrSize) - 1 do
        Marshal.WriteInt16(arnlen, sizeof(ub2) * i + 30);

      /// indicator is needed to allow NULL arument name values (i.e Result)
      Indicator := Marshal.AllocHGlobal(2 * Marshal.ReadInt32(PArrSize));
      try
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 5, argnam,
          31, SQLT_CHR, Indicator, arnlen, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, 30, sizeof(sb2), sizeof(ub2), 0);
        if Result <> OCI_SUCCESS then
          Exit;

        /// bind datatype param
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 6, dtype,
          sizeof(ub2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub2), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

        /// bind default param
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 7, defsup,
          sizeof(ub1), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub1), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

        /// bind in_out param
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 8, mode,
          sizeof(ub1), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub1), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

        /// bind length param
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 9, dtsiz,
          sizeof(ub4), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub4), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

        /// bind precision param
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 10, prec,
          sizeof(sb2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(sb2), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind scale param
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 11, scale,
          sizeof(sb2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(sb2), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

        /// bind radix param
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 12, radix,
          sizeof(ub1), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub1), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

        /// bind spare param
        Result := OCI8.OCIBindByPos(hOCIStmt, hBind, OCISvcCtx.hOCIError, 13, spare,
          sizeof(ub4), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCI8.OCIBindArrayOfStruct(hBind, OCISvcCtx.hOCIError, sizeof(ub4), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

        /// execute statement
        Result := OCI8.OCIStmtExecute(OCISvcCtx.hOCISvcCtx, hOCIStmt, OCISvcCtx.hOCIError, 1, 0, nil, nil, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
      finally
        Marshal.FreeHGlobal(Indicator);
      end;
    finally
      FreeStringOCI(PObjName, False);
    end;
  finally
    /// free statement handle
    OCI8.OCIHandleFree(hOCIStmt, OCI_HTYPE_STMT);
  end;
end;

procedure TOCICommand.DescribeObjectParams(const Name: string; Overload: integer; ObjectParams: TStringList);

  function GetDescribeSQL(const OwnerName, PackageName, ProcName: string): string;
  begin
    if OwnerName = '' then
      Result := 'select ARGUMENT_NAME, TYPE_OWNER, TYPE_NAME ' +
                '  from USER_ARGUMENTS ' +
                '  where OBJECT_NAME = ''' + ProcName + ''' and '
    else
      Result := 'select ARGUMENT_NAME, TYPE_OWNER, TYPE_NAME ' +
                '  from ALL_ARGUMENTS ' +
                '  where OWNER = ''' + OwnerName + ''' and ' +
                '        OBJECT_NAME = ''' + ProcName + ''' and ';

    if PackageName <> '' then
      Result := Result +
                '        PACKAGE_NAME = ''' + PackageName + ''' and ';

    Result := Result +
              '        nvl(OVERLOAD, 1) = ' + IntToStr(Overload) + ' and ' +
              '        DATA_LEVEL=0 ' +
              '  order by POSITION'
   end;

var
  i: integer;
  str: string;
  OwnerName: string;
  PackageName: string;
  ProcName: string;
  Info: TSQLObjectInfo;
  RecordSet: TOCIRecordSet;
  ArgNameField: TFieldDesc;
  TypeOwner: TFieldDesc;
  TypeNameField: TFieldDesc;
  RecBuf: IntPtr;
  v: Variant;
begin
  OCISQLInfo.SplitObjectName(Name, Info);
  if Info.Schema <> '' then begin
    PackageName := Info.Schema;
    ProcName := Info.Name;
    OCISQLInfo.SplitObjectName(ProcName, Info);
    if Info.Schema <> '' then begin
      OwnerName := PackageName;
      PackageName := Info.Schema;
      ProcName := Info.Name;
    end;
  end
  else
    ProcName := Name;

  RecordSet := TOCIRecordSet.Create;
  try
    RecordSet.SetConnection(FConnection);
    RecordSet.FCommand.SQL := GetDescribeSQL(OwnerName, PackageName, ProcName);
    RecordSet.Open;
    RecordSet.FetchAll;

    if (RecordSet.RecordCount = 0) and (OwnerName = '') and (PackageName <> '') then begin
      OwnerName := PackageName;

      RecordSet.Close;
      RecordSet.FCommand.SQL := GetDescribeSQL(OwnerName, '', ProcName);
      RecordSet.Open;
      RecordSet.FetchAll;
    end;

    ArgNameField := RecordSet.Fields[0];
    TypeOwner := RecordSet.Fields[1];
    TypeNameField := RecordSet.Fields[2];
    RecordSet.AllocRecBuf(RecBuf);
    try
      while True do begin
        RecordSet.GetNextRecord(RecBuf);
        if RecordSet.Eof then
          Break;

        RecordSet.GetFieldAsVariant(ArgNameField, RecBuf, v);
        str := VarToStr(v);
        for i := 0 to ObjectParams.Count - 1 do
          if ObjectParams.Strings[i] = str then begin
            RecordSet.GetFieldAsVariant(TypeNameField, RecBuf, v);
            str := VarToStr(v);
            RecordSet.GetFieldAsVariant(TypeOwner, RecBuf, v);
            if not VarIsNull(v) then
              str := VarToStr(v) + '.' + str;
            DescribeObjectParamType(str, TOraParamDesc(ObjectParams.Objects[i]));
            break;
          end;
      end;
    finally
      RecordSet.FreeRecBuf(RecBuf);
    end;

    RecordSet.Close;
  finally
    RecordSet.Free;
  end;
end;

procedure TOCICommand.DescribeObjectParamType(const TypeName: string; Param: TOraParamDesc);
var
  ObjectType: TOraType;
  OraObject: TOraObject;
begin
  ObjectType := nil;
  if ObjectTypes <> nil then
    ObjectType := TOraType(ObjectTypes.FindType(OCISvcCtx, TypeName));
  if ObjectType = nil then
    ObjectType := TOraType.Create(OCISvcCtx, TypeName)
  else
    ObjectType.AddRef;

  try
    case ObjectType.DataType of
      dtXml: begin
        Param.DataType := dtXml;
        OraObject := TOraXML.Create(OCISvcCtx, ObjectType);
        try
          Param.SetObject(OraObject); // increments ref count to 2
        finally
          OraObject.Free;             // decrements ref count to 1
        end;
      end;
      dtAnyData: begin
        Param.DataType := dtAnyData;
        OraObject := TOraAnyData.Create(OCISvcCtx, ObjectType);
        try
          Param.SetObject(OraObject); // increments ref count to 2
        finally
          OraObject.Free;             // decrements ref count to 1
        end;
      end;
      else begin
        Param.DataType := dtObject;//TOraType(Field.ObjectType).DataType;
        OraObject := TOraObject.Create(ObjectType);
        try
          Param.SetObject(OraObject); // increments ref count to 2
        finally
          OraObject.Free;             // decrements ref count to 1
        end;
      end;
    end;
  finally
    ObjectType.Release;
  end;
end;

procedure TOCICommand.InitProcParams7(const Name: string; Overload: integer);
const
  MaxParams = 150;

  DBMS_XML           = 58;
  DBMS_OBJECT        = 121;
  DBMS_TIME          = 178;
  DBMS_TIME_TZ       = 179;
  DBMS_TIMESTAMP     = 180;
  DBMS_TIMESTAMP_TZ  = 181;
  DBMS_TIMESTAMP_LTZ = 231;
var
  ovrld: IntPtr;
  pos: IntPtr;
  level: IntPtr;
  argnm: IntPtr;
  arnlen: IntPtr;
  dtype: IntPtr;
  defsup: IntPtr;
  mode: IntPtr;
  dtsize: IntPtr;
  prec: IntPtr;
  scale: IntPtr;
  radix: IntPtr;
  spare: IntPtr;
  arrsize: ub4;
  i: word;
  Param: TOraParamDesc;
  Table: boolean;
  TableLength: Integer;
  OraType: integer;
  OraPrec: Integer;
  OraScale: Integer;
  Res: sword;
//  Msg: array [0..512] of char;
  ActualParamName: string;
  ParamName: string;
  ParamIndex: integer;
  ParamOverload: integer;
  ObjectParams: TStringlist;
begin
  arrsize := MaxParams;

  ovrld := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  pos := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  level := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  argnm := Marshal.AllocHGlobal( (MaxParams + 1) * 30);
  arnlen := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  dtype := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  defsup := Marshal.AllocHGlobal( MaxParams + 1 );
  mode := Marshal.AllocHGlobal( MaxParams + 1 );
  dtsize := Marshal.AllocHGlobal( (MaxParams + 1) * 4);
  prec := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  scale := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  radix := Marshal.AllocHGlobal( MaxParams + 1 );
  spare := Marshal.AllocHGlobal( (MaxParams + 1) * 4);

  ObjectParams := TStringlist.Create;
  try
    if OCI73 in OCISvcCtx.Home.PossibleOCICallStyles then
      Res := OCI7.odessp(FConnection.GetLDA, PAnsiChar(AnsiString(Name)), NativeUInt(-1), nil, 0, nil, 0, ovrld,
        pos, level, argnm, arnlen, dtype, defsup, mode, dtsize, prec,
        scale, radix, spare, arrsize)
    else
      Res := DescribeStoredProc(Name, ovrld,
        pos, level, argnm, arnlen, dtype, defsup, mode, dtsize,
        prec, scale, radix, spare, arrsize);

    if Res <> 0 then begin  // for using with OCI8 and UseOCI7ProcDesc = True
      if OCI73 in OCISvcCtx.Home.PossibleOCICallStyles then
        FConnection.OraError(OCI73, Res, True, Component)
      else
        FConnection.OraError(OCI80, Res, True, Component);
  //      oerhms(FConnection.GetLDA, Res, Msg, 512);
  //      raise EOraError.Create(Abs(Res), Msg);
    end;

    Assert(arrsize < MaxParams);

    FParams.Clear;

    Param := nil;
    Table := False;
    TableLength := 0;

    // Overload 0 and 1 is equal
    if Overload = 0 then
      Overload := 1;

    i := 0;
    while i < arrsize do begin
      ParamOverload := Marshal.ReadInt16(ovrld, i * 2);
      // Overload 0 and 1 is equal
      if ParamOverload = 0 then
        ParamOverload := 1;
      if (Marshal.ReadInt16(dtype, i * 2) <> UNKNOWN_TYPE) and (ParamOverload = Overload) then begin
        if Marshal.ReadInt16(level, i * 2) = 0 then begin
          Param := TOraParamDesc(AddParam);

          if Marshal.ReadInt16(pos, i * 2) = 0 then begin
            Param.Name := 'RESULT';

            Param.ParamType := pdResult;
          end
          else begin
            ActualParamName := string(Marshal.PtrToStringAnsi(PtrOffset(argnm, i * 30), Marshal.ReadInt16(arnlen, i * 2)));
            ParamName := StringReplace(ActualParamName, ' ' , '_', [rfReplaceAll]);
            if FParams.FindParam(ParamName) <> nil then begin
              ParamIndex := 1;
              while FParams.FindParam(ParamName + '_' + IntToStr(ParamIndex)) <> nil do
                Inc(ParamIndex);
              ParamName := ParamName + '_' + IntToStr(ParamIndex);
            end;
            Param.Name := ParamName;

            case Marshal.ReadByte(mode, i) of
              0: Param.ParamType := pdInput;
              1: Param.ParamType := pdOutput;
              2: Param.ParamType := pdInputOutput;
            end;

            Param.SetHasDefault(Boolean(Marshal.ReadByte(defsup, i)));
          end;
        end;

        Assert(Param <> nil);

        if (Marshal.ReadInt16(level, i * 2) = 0) or Table then begin
          OraType := Marshal.ReadInt16(dtype, i * 2);
          case OraType of
            VARCHAR2_TYPE: begin
            {$IFNDEF LITE}
              if FConnection.FUseUnicode then
                Param.DataType := dtWideString
              else
            {$ENDIF}
                Param.DataType := dtString;
              if Marshal.ReadInt32(dtsize, i * 4) > 0 then
                Param.Size := Marshal.ReadInt32(dtsize, i * 4);
            end;
            CHAR_TYPE: begin
            {$IFNDEF LITE}
              if FConnection.FUseUnicode then
                Param.DataType := dtFixedWideChar
              else
            {$ENDIF}
                Param.DataType := dtFixedChar;
              if Marshal.ReadInt32(dtsize, i * 4) > 0 then
                Param.Size := Marshal.ReadInt32(dtsize, i * 4);
            end;
            NUMBER_TYPE: begin
              OraPrec := Marshal.ReadInt16(prec, i * 2);
              OraScale := Marshal.ReadInt16(scale, i * 2);

              if OraPrec = 0 then begin // NUMBER without Prec and Scale
                OraPrec := 39;
                if OraScale <= 0 then
                  OraScale := 39;
                Param.SubDataType := dtNumberFloating;
              end
              else
                Param.SubDataType := dtNumber;

              if FConnection.FEnableIntegers and (OraScale = 0) and (OraPrec <= GetIntegerPrecision) then
                Param.DataType := dtInteger
              else
                Param.DataType := dtFloat;
            end;
            INTEGER_TYPE: begin
              Param.DataType := dtInteger;
              Param.SubDataType := dtInteger;
            end;
            DATE_TYPE,
            DBMS_TIME, DBMS_TIME_TZ:
              Param.DataType := dtDateTime;
            ROWID_TYPE: begin
              Param.DataType := dtString;
              Param.Size := RowIdSize + 1;
            end;
            RAW_TYPE: begin
              Param.DataType := dtVarBytes;
              if Marshal.ReadInt32(dtsize, i * 4) > 0 then
                Param.Size := Marshal.ReadInt32(dtsize, i * 4);
            end;
            LONG_TYPE:
            {$IFNDEF LITE}
              if FConnection.FUseUnicode then
                Param.DataType := dtWideMemo
              else
            {$ENDIF}
                Param.DataType := dtMemo;
            LONGRAW_TYPE:
              Param.DataType := dtBlob;
            CURSOR_TYPE:
              Param.DataType := dtCursor;
            SQLT_TAB: begin
              Table := True;
              TableLength := 1;
            end;
            SQLT_BOL:
              Param.DataType := dtBoolean;
            SQLT_REC: begin
              //Rec := True;
            end;
          // Oracle8 types
            SQLT_IBFLOAT: begin
              Param.DataType := dtFloat;
              Param.SubDataType := dtBFloat;
            end;
            SQLT_IBDOUBLE: begin
              Param.DataType := dtFloat;
              Param.SubDataType := dtBDouble;
            end;
            SQLT_CLOB:
              Param.DataType := dtOraClob;
            SQLT_BLOB:
              Param.DataType := dtOraBlob;
            SQLT_BFILEE:
              Param.DataType := dtBFILE;
            SQLT_CFILEE:
              Param.DataType := dtCFILE;
            SQLT_RSET:
              Param.DataType := dtCursor;
            DBMS_OBJECT, SQLT_NTY: begin
              Param.DataType := dtObject;
              ObjectParams.AddObject(ActualParamName, Param);
            end;
            DBMS_XML: begin
              Param.DataType := dtXML;
              ObjectParams.AddObject(ActualParamName, Param);
            end;
            SQLT_REF: begin
              Param.DataType := dtReference;
              ObjectParams.AddObject(ActualParamName, Param);
            end;
            SQLT_NCO: begin
              Param.DataType := dtTable;
              ObjectParams.AddObject(ActualParamName, Param);
            end;
            SQLT_VARRAY: begin
              Param.DataType := dtArray;
              ObjectParams.AddObject(ActualParamName, Param);
            end;
          // Oracle9 types
            SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ,
            DBMS_TIMESTAMP, DBMS_TIMESTAMP_TZ, DBMS_TIMESTAMP_LTZ: begin
            {$IFNDEF FPC}
              if FConnection.FEnableSQLTimeStamp then
                Param.DataType := dtSQLTimeStamp
            {$ELSE}
              if FConnection.FTimeStampAsString then
                Param.DataType := dtDateTime
            {$ENDIF}
              else
                case OraType of
                  SQLT_TIMESTAMP, DBMS_TIMESTAMP:
                    Param.DataType := dtTimeStamp;
                  SQLT_TIMESTAMP_TZ, DBMS_TIMESTAMP_TZ:
                    Param.DataType := dtTimeStampTZ;
                  SQLT_TIMESTAMP_LTZ, DBMS_TIMESTAMP_LTZ:
                    Param.DataType := dtTimeStampLTZ;
                end;
            end;
            SQLT_INTERVAL_YM:
              Param.DataType := dtIntervalYM;
            SQLT_INTERVAL_DS:
              Param.DataType := dtIntervalDS;
          else
            RaiseError(SUnsupportedDataType + ' [' + IntToStr(Marshal.ReadInt16(dtype, i * 2)) + ']')
          end;
        end;

        if Marshal.ReadInt16(level, i * 2) > 0 then
          if Table then begin
          // for PL/SQL table
            Param.FTable := Table;
            Param.FArraySize := TableLength;
            Table := False;
            TableLength := 0;
          end;
        if (Param.FValueData = nil) and not Table then
          Param.AllocBuffer;
      end;
      Inc(i);
    end;

    if ObjectParams.Count > 0 then
      DescribeObjectParams(Name, Overload, ObjectParams);
  finally
    Marshal.FreeHGlobal(argnm);
    Marshal.FreeHGlobal(ovrld);
    Marshal.FreeHGlobal(pos);
    Marshal.FreeHGlobal(level);
    Marshal.FreeHGlobal(arnlen);
    Marshal.FreeHGlobal(dtype);
    Marshal.FreeHGlobal(defsup);
    Marshal.FreeHGlobal(mode);
    Marshal.FreeHGlobal(dtsize);
    Marshal.FreeHGlobal(prec);
    Marshal.FreeHGlobal(scale);
    Marshal.FreeHGlobal(radix);
    Marshal.FreeHGlobal(spare);

    ObjectParams.Free;
  end;
end;

procedure TOCICommand.InitProcParams8(Name: string; Overload: integer);

  function DescribeSynonym(const SynonymName: string): string;
  var
    Handle: IntPtr;
    hDescribe: pOCIDescribe;
    b1: Integer; //byte
    hParam: pOCIParam;
    Len: Integer;
    Ptr, ValuePtr, StrPtr: IntPtr;
    ValueInt: Integer;
    ObjType: byte;
    Res, Size: integer;
  begin
    Result := SynonymName;
    Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
    try
      b1 := 1;
      Check(OCI8.OCIAttrSet2(hDescribe, OCI_HTYPE_DESCRIBE, b1, 0, OCI_ATTR_DESC_PUBLIC, OCISvcCtx.hOCIError));
      Ptr := Marshal.AllocHGlobal(sizeof(integer));
      try
        while True do begin
          Handle := StringToHGlobalOCI(Result, Size, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIDescribeAny(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, Handle, Size,
            OCI_OTYPE_NAME, OCI_DEFAULT, OCI_PTYPE_UNK, hDescribe);
          FreeStringOCI(Handle, OCISvcCtx.UnicodeEnv);
          Check(Res);

          ValuePtr := @hParam;
          Check(OCI8.OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil, OCI_ATTR_PARAM, OCISvcCtx.hOCIError));

          Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PTYPE, OCISvcCtx.hOCIError));
          ObjType := Byte(ValueInt);

          case ObjType of
            OCI_PTYPE_TYPE:
              Break;
            OCI_PTYPE_SYN: begin
              StrPtr := nil;
              ValuePtr := @StrPtr;
              Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_NAME, OCISvcCtx.hOCIError));

              Len := Cardinal(Marshal.ReadInt32(Ptr));
              Result := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

              StrPtr := nil;
              ValuePtr := @StrPtr;
              Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_SCHEMA_NAME, OCISvcCtx.hOCIError));

              Len := Cardinal(Marshal.ReadInt32(Ptr));
              if Len > 0 then
                Result := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv)) + '.' + Result;
            end
          else
            RaiseOraError(4043, Format(SObjectNotExist, [Name]));
          end;
        end;
      finally
        Marshal.FreeHGlobal(Ptr);
      end;
    finally
      Check(OCI8.OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE));
    end;
  end;

  procedure GetDelimiterPositions(const Value: string; out DotPos1, DotPos2, DogPos: Integer);
  var
    i: Integer;
    InQuote: boolean;
  begin
    DotPos1 :=0;
    DotPos2 :=0;
    DogPos := 0;

    InQuote := False;
    for i := 1 to Length(Value) do
      if Value[i] = '"' then
        InQuote := not InQuote
      else if not InQuote then
        if Value[i] = '.' then begin
          if DotPos1 = 0 then
            DotPos1 := i
          else if DotPos2 = 0 then
            DotPos2 := i;
        end
        else if Value[i] = '@' then begin
          DogPos := i;
          Exit;
        end;
  end;

var
  Res: sword;
  i, BufSize: integer;
  Param: TOraParamDesc;
  hDescribe: pOCIDescribe;
  hParam: pOCIParam;
  hArgList: pOCIParam;
  hArgList1: pOCIParam; // arguments of level 1
  hArg: pOCIParam;
  hProcList: pOCIParam;
  ArgCount: word;
  pName: string;
  Len: cardinal;
  OraType: word;
  Size: Integer;
  Scale: shortint;
  Prec: word; // ??? byte
  Mode: byte;
  ProcCount: word;
  ListType: byte;
  lOverload: word;
  d1,d2,d3: integer;
  St,St1, DBLinkName: string;
  ObjType: byte;
  b1: Integer; //byte
  iOffset: integer;
  Handle: IntPtr;
  ValuePtr, StrPtr: IntPtr;
  ValueInt: Integer;
  PLen: IntPtr;
  TypeName, SchemaName: string;
  IsEqual: boolean;
  ParamName: string;
  ParamIndex: Integer;
begin
  Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
  Check(OCI8.OCIAttrSet2(hDescribe, OCI_HTYPE_DESCRIBE, b1, 0, OCI_ATTR_DESC_PUBLIC, OCISvcCtx.hOCIError));
  try
    if Overload = 1 then
      Overload := 0;
    lOverload := 0;

    PLen  := Marshal.AllocHGlobal(sizeof(Integer));
    try
      while True do begin
        GetDelimiterPositions(Name, d1, d2, d3);

        if d3 > 0 then
          DBLinkName := AnsiUpperCase(Copy(Name, d3, Length(Name)))
        else
          DBLinkName := '';

      { Get the describe handle }
        if d2 > 0 then
          St := Copy(Name, 1, d2 - 1) + DBLinkName
        else
          St := Name;

        Handle := StringToHGlobalOCI(St, BufSize, OCISvcCtx.UnicodeEnv);
        Res := OCI8.OCIDescribeAny(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, Handle, BufSize,
          OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, hDescribe);
        FreeStringOCI(Handle, OCISvcCtx.UnicodeEnv);

        if (Res <> 0) and (d1 > 0) and (d2 = 0) then begin
        // for <package>.<proc>
          St := Copy(Name, 1, d1 - 1) + DBLinkName;
          Handle := StringToHGlobalOCI(St, BufSize, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIDescribeAny(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, Handle, BufSize,
            OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, hDescribe);
          FreeStringOCI(Handle, OCISvcCtx.UnicodeEnv);
          Check(Res);
        end
        else
          Check(Res);

        if d2 = 0 then
          d2 := d1;

        ValuePtr := @hParam;
        Check(OCI8.OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil, OCI_ATTR_PARAM, OCISvcCtx.hOCIError));

        Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PTYPE, OCISvcCtx.hOCIError));
        ObjType := Byte(ValueInt);

        case ObjType of
          OCI_PTYPE_PROC, OCI_PTYPE_FUNC: begin

          end;
          OCI_PTYPE_PKG, OCI_PTYPE_TYPE : begin
          // Describe package proc
          // Describe object methods

          { Get the parameter handle }
            ValuePtr := @hParam;
            Check(OCI8.OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil,  OCI_ATTR_PARAM, OCISvcCtx.hOCIError));

            if ObjType = OCI_PTYPE_PKG then begin
            { Get the subprogram list }
              ValuePtr := @hProcList;
              Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil,  OCI_ATTR_LIST_SUBPROGRAMS, OCISvcCtx.hOCIError));
              iOffset := 0;
            end
            else begin
            { Get the method list }
              ValuePtr := @hProcList;
              Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil,  OCI_ATTR_LIST_TYPE_METHODS, OCISvcCtx.hOCIError));
              iOffset := 1;
            end;

          { Get the number of procs }
            Check(OCI8.OCIAttrGet2(hProcList, OCI_DTYPE_PARAM, ValueInt, nil,
              OCI_ATTR_NUM_PARAMS, OCISvcCtx.hOCIError));
            ProcCount := Word(ValueInt);

            St := Copy(Name, d2 + 1, Length(Name));
            i := iOffset;
            lOverload := 0;
            while i < ProcCount + iOffset do begin
            { Get the parameter handle }
              Check(OCI8.OCIParamGet(hProcList, OCI_DTYPE_PARAM, OCISvcCtx.hOCIError, hParam, i));  // hProc

              try
                StrPtr := nil;
                ValuePtr := @StrPtr;
                Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, PLen, OCI_ATTR_NAME, OCISvcCtx.hOCIError));

                Len := Cardinal(Marshal.ReadInt32(PLen));
                St1 := PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv) + DBLinkName;

                if OCISQLInfo.IsQuoted(St) then
                  IsEqual := OCISQLInfo.UnQuote(St) = St1
                else
                  IsEqual := AnsiUpperCase(St) = St1;

                if IsEqual then begin
                  if Overload > 0 then
                    Inc(lOverload);
                  if lOverload = Overload then
                    break;
                end;

              finally
                // free memory after OCIParamGet
                OCI8.OCIDescriptorFree(hParam, OCI_DTYPE_PARAM);
              end;

              Inc(i);
            end;

            if i = ProcCount + iOffset then
              RaiseOraError(4043, Format(SObjectNotExist, [Name]));
          end;
          OCI_PTYPE_SYN: begin
          // Describe synonyms
            if d2 > 0 then
              Name := '.' + Copy(Name, d2 + 1, Length(Name))
            else
              Name := '';

            StrPtr := nil;
            ValuePtr := @StrPtr;
            Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, PLen,  OCI_ATTR_NAME, OCISvcCtx.hOCIError));

            Len := Cardinal(Marshal.ReadInt32(PLen));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));
            Name := pName + Name;

            StrPtr := nil;
            ValuePtr := @StrPtr;
            Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, PLen,  OCI_ATTR_SCHEMA_NAME, OCISvcCtx.hOCIError));

            Len := Cardinal(Marshal.ReadInt32(PLen));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));
            Name := pName + '.' + Name;

            Continue;
          end;
          //OCI_PTYPE_SCHEMA:
        else
          RaiseOraError(4043, Format(SObjectNotExist, [St]));
        end;

        { Get the arg list }
        ValuePtr := @hArgList;
        Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil, OCI_ATTR_LIST_ARGUMENTS, OCISvcCtx.hOCIError));

        { Get the number of arguments }
        Check(OCI8.OCIAttrGet2(hArgList, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_NUM_PARAMS, OCISvcCtx.hOCIError));
        ArgCount := Word(ValueInt);

        ListType := 0;
        if (OCISvcCtx.Home.OCIVersion < 8100) then begin
          if (ObjType = OCI_PTYPE_FUNC)
          /// Test param at zero position for package subprogram. It won't return
          /// error in case of function.
            or ((ObjType = OCI_PTYPE_PKG)
            and (OCI8.OCIParamGet(hArgList, OCI_DTYPE_PARAM, OCISvcCtx.hOCIError, hArg, 0) = 0))
          then
            ListType := OCI_LTYPE_ARG_FUNC
        end
        else begin
        /// Get the type of subproram for 8.1.5 and above. In 8.0.x client attribute
        /// OCI_ATTR_LTYPE is not supported.
          Check(OCI8.OCIAttrGet2(hArgList, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_LTYPE, OCISvcCtx.hOCIError));
          ListType := Byte(ValueInt);
        end;

        FParams.Clear;

        Param := nil;
        hArgList1 := nil;

        if ListType in [OCI_LTYPE_ARG_FUNC, OCI_LTYPE_TYPE_ARG_FUNC] then begin // IsFunc
          i := 0;
          if ListType in [OCI_LTYPE_ARG_FUNC] then
            Dec(ArgCount);
        end
        else
          // OCI_LTYPE_ARG_PROC, OCI_LTYPE_TYPE_ARG_PROC
          i := 1;

        while i <= ArgCount do begin
          if hArgList1 = nil then
            Check(OCI8.OCIParamGet(hArgList, OCI_DTYPE_PARAM, OCISvcCtx.hOCIError, hArg, i))
          else
            Check(OCI8.OCIParamGet(hArgList1, OCI_DTYPE_PARAM, OCISvcCtx.hOCIError, hArg, 1));

          try
            Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PTYPE, OCISvcCtx.hOCIError));
            ObjType := Byte(ValueInt);

            //Check(OCIAttrGet(hArg, OCI_DTYPE_PARAM, @Level, nil, OCI_ATTR_LEVEL, OCISvcCtx.hOCIError));
            Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_TYPE, OCISvcCtx.hOCIError));
            OraType := Word(ValueInt);

            if (OraType <> SQLT_UNK) and (lOverload = Overload) then begin
              if hArgList1 = nil then begin
                if i > 0 then begin
                  StrPtr := nil;
                  ValuePtr := @StrPtr;
                  Check(OCI8.OCIAttrGet1(hArg, OCI_DTYPE_PARAM, ValuePtr, PLen, OCI_ATTR_NAME, OCISvcCtx.hOCIError));

                  Len := Cardinal(Marshal.ReadInt32(PLen));
                  ParamName := PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv);
                  ParamName := StringReplace(ParamName, ' ' , '_', [rfReplaceAll]);
                  if FParams.FindParam(ParamName) <> nil then begin
                    ParamIndex := 1;
                    while FParams.FindParam(ParamName + '_' + IntToStr(ParamIndex)) <> nil do
                      Inc(ParamIndex);
                    ParamName := ParamName + '_' + IntToStr(ParamIndex);
                  end;
                end;
                Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_IOMODE, OCISvcCtx.hOCIError));
                Mode := Byte(ValueInt);

                Param := TOraParamDesc(AddParam);
                if i > 0 then begin
                  Param.Name := ParamName;

                  case Mode of
                    0: Param.ParamType := pdInput;
                    1: Param.ParamType := pdOutput;
                    2: Param.ParamType := pdInputOutput;
                  end;

                  Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_HAS_DEFAULT, OCISvcCtx.hOCIError));
                  Param.SetHasDefault(Boolean(ValueInt));
                end
                else begin
                  Param.Name := 'RESULT';
                  Param.ParamType := pdResult;
                end;
              end;

              Assert(Param <> nil);

              Size := 0;
              Scale := 0;
              Prec := 0;
              if //(OraType <> SQLT_NTY) and
                 (ObjType = OCI_PTYPE_ARG) then begin  // for Self param of methods
                Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_SIZE, OCISvcCtx.hOCIError));
                Size := ValueInt;
                Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_SCALE, OCISvcCtx.hOCIError));
                Scale := sb1(ValueInt);
                Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PRECISION, OCISvcCtx.hOCIError));
                Prec := Word(ValueInt);
              end;
              // Use default value OCI_ATTR_HAS_DEFAULT

              case OraType of
                SQLT_CHR: begin
                  if FConnection.FUseUnicode then
                    Param.DataType := dtWideString
                  else
                    Param.DataType := dtString;
                  if Size > 0 then
                    Param.Size := Size;
                  Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, OCISvcCtx.hOCIError));
                  Param.SetNational(ub1(ValueInt) = SQLCS_NCHAR);
                end;
                SQLT_AFC: begin
                  if FConnection.FUseUnicode then
                    Param.DataType := dtFixedWideChar
                  else
                    Param.DataType := dtFixedChar;
                  if Size > 0 then
                    Param.Size := Size;
                  Check(OCI8.OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, OCISvcCtx.hOCIError));
                  Param.SetNational(ub1(ValueInt) = SQLCS_NCHAR);
                end;
                SQLT_INT: begin
                  Param.DataType := dtInteger;
                  Param.SubDataType := dtInteger;
                end;
                SQLT_NUM: begin
                  if (Scale = -127) and (Prec > 0) then begin // FLOAT
                    Prec := Ceil(Scale * 0.30103);
                    Param.SubDataType := dtFloat;
                  end
                  else if Prec = 0 then begin // NUMBER without Prec and Scale
                    Prec := 39;
                    if Scale <= 0 then
                      Scale := 39;
                    Param.SubDataType := dtNumberFloating;
                  end
                  else
                    Param.SubDataType := dtNumber;

                  if FConnection.FEnableIntegers and (Scale = 0) and (Prec <= GetSmallintPrecision) then
                    Param.DataType := dtSmallint
                  else if FConnection.FEnableIntegers and (Scale = 0) and (Prec <= GetIntegerPrecision) then
                    Param.DataType := dtInteger
                  else if FConnection.FEnableIntegers and (Scale = 0) and (Prec <= GetLargeintPrecision) then
                    Param.DataType := dtLargeint
                  else if (FConnection.EnableBCD or FEnableBCD) and
                          (Scale >= 0) and (Prec <= GetBCDPrecision) and (Scale <= GetBCDScale)
                  then
                    Param.DataType := dtBCD
                  else if Prec <= GetFloatPrecision then
                    Param.DataType := dtFloat
                  else if (FConnection.EnableFMTBCD or FEnableFMTBCD) and
                          (Scale >= 0) and (Prec <= GetFmtBCDPrecision) and (Scale <= GetFmtBCDScale)
                  then
                    Param.DataType := dtFMTBCD
                  else if FConnection.FEnableNumbers then
                    Param.DataType := dtNumber
                  else
                    Param.DataType := dtFloat;
                end;
                SQLT_IBFLOAT: begin
                  Param.DataType := dtFloat;
                  Param.SubDataType := dtBFloat;
                end;
                SQLT_IBDOUBLE: begin
                  Param.DataType := dtFloat;
                  Param.SubDataType := dtBDouble;
                end;
                SQLT_DAT:
                  Param.DataType := dtDateTime;
                SQLT_RID,SQLT_RDD : begin
                {$IFNDEF LITE}
                  if FConnection.FUseUnicode then
                    Param.DataType := dtWideString // dtRowId;
                  else
                {$ENDIF}
                    Param.DataType := dtString; // dtRowId;
                  Param.Size := RowIdSize + 1; // for terminator
                end;
                SQLT_BIN: begin
                  Param.DataType := dtVarBytes;
                  if Size > 0 then
                    Param.Size := Size;
                end;
                SQLT_LNG:
                {$IFNDEF LITE}
                  if FConnection.FUseUnicode then
                    Param.DataType := dtWideMemo
                  else
                {$ENDIF}
                    Param.DataType := dtMemo;
                SQLT_LBI:
                  Param.DataType := dtBlob;
                SQLT_CUR, SQLT_RSET: begin
                  Param.DataType := dtCursor;
                  //Param.ParamType := pdOutput;
                end;
                SQLT_TAB: begin
                  ValuePtr := @hArgList1;
                  Check(OCI8.OCIAttrGet1(hArg, OCI_DTYPE_PARAM, ValuePtr, nil, OCI_ATTR_LIST_ARGUMENTS, OCISvcCtx.hOCIError));

                  Param.FTable := True;
                  Param.FArraySize := 1;
                  if hArgList1 <> nil then
                    Continue;
                end;
                SQLT_BOL:
                  Param.DataType := dtBoolean;
                SQLT_REC: begin
                  //Rec:= True;
                end;
              // Oracle8 types
                SQLT_CLOB:
                  Param.DataType := dtOraClob;
                SQLT_BLOB:
                  Param.DataType := dtOraBlob;
                SQLT_BFILEE:
                  Param.DataType := dtBFILE;
                SQLT_CFILEE:
                  Param.DataType := dtCFILE;
                SQLT_NTY: begin
                  StrPtr := nil;
                  ValuePtr := @StrPtr;
                  Check(OCISvcCtx.OCI8.OCIAttrGet1(hArg, OCI_DTYPE_PARAM, ValuePtr, PLen, OCI_ATTR_TYPE_NAME, OCISvcCtx.hOCIError));

                  Len := Marshal.ReadInt32(PLen);
                  TypeName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

                  StrPtr := nil;
                  ValuePtr := @StrPtr;
                  Check(OCISvcCtx.OCI8.OCIAttrGet1(hArg, OCI_DTYPE_PARAM, ValuePtr, PLen, OCI_ATTR_SCHEMA_NAME, OCISvcCtx.hOCIError));

                  Len := Marshal.ReadInt32(PLen);
                  SchemaName := PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv);

                  if SchemaName <> 'PUBLIC' then // Oracle 10
                    TypeName := SchemaName + '.' + TypeName;

                  TypeName := DescribeSynonym(TypeName);
                  DescribeObjectParamType(TypeName, Param);
                end;
                SQLT_REF:
                  Param.DataType := dtReference;
                SQLT_NCO:
                  Param.DataType := dtTable;
                SQLT_VARRAY:
                  Param.DataType := dtArray;
              // Oracle9 types
                SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ: begin
                {$IFNDEF FPC}
                  if FConnection.FEnableSQLTimeStamp then
                    Param.DataType := dtSQLTimeStamp
                {$ELSE}
                  if FConnection.FTimeStampAsString then
                    Param.DataType := dtDateTime
                {$ENDIF}
                  else
                    case OraType of
                      SQLT_TIMESTAMP:
                        Param.DataType := dtTimeStamp;
                      SQLT_TIMESTAMP_TZ:
                        Param.DataType := dtTimeStampTZ;
                      SQLT_TIMESTAMP_LTZ:
                        Param.DataType := dtTimeStampLTZ;
                    end;
                end;
                SQLT_INTERVAL_YM:
                  Param.DataType := dtIntervalYM;
                SQLT_INTERVAL_DS:
                  Param.DataType := dtIntervalDS;
              else
                RaiseError(SUnsupportedDataType + ' [' + IntToStr(OraType) + ']')
              end;
              if Param.FValueData = nil then
                Param.AllocBuffer;
            end;

            hArgList1 := nil;
            Inc(i);

          finally
            // free memory after OCIParamGet
            OCISvcCtx.OCI8.OCIDescriptorFree(hArg, OCI_DTYPE_PARAM);
          end;

        end;

        break;
      end;
    finally
      Marshal.FreeHGlobal(PLen);
    end;

  finally
    Check(OCISvcCtx.OCI8.OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE));
  end;
end;

procedure TOCICommand.InitProcParams(const Name: string; Overload: integer);
begin
  CheckSession;
  AllocOCISvcCtx;

  try
    FOCICallStyle := FConnection.FOCICallStyleCommand;

    if FOCICallStyle = OCI73 then
      InitProcParams7(Name, Overload)
    else if FOCICallStyle = OCI80 then begin
      /// Oracle Server 8.0.5 has a bug with OCI80 call style after recompiling.
      /// Oracle 8.0.5 has a bug with OCI80 call style??? - Not detected.
      /// Oracle 8.0.4 has bug with OCI73 call style (cannot describe more than 10
      /// parameters).
      if UseOCI7ProcDesc or (FConnection.GetOracleVersion = 8050) then
        InitProcParams7(Name, Overload)
      else
        InitProcParams8(Name, Overload);
    end
    else
      CheckOCI;
  finally
    ReleaseOCISvcCtx;
  end;
end;

procedure TOCICommand.DoExecute;
var
  OldPrepared: boolean;
begin
  OldPrepared := GetPrepared;
  if not OldPrepared then
    Prepare;
  try
    if FSQLType <> SQL_EXPLAIN then  // avoid binding with EXPLAIN PLAN
      BindParams;

    Exec;

  {$IFDEF AUTOTEST}
    inherited Execute;
  {$ENDIF}
  finally
    if (not OldPrepared or FForceUnprepare) and (GetCursorState <> csInactive) then
      Unprepare;
    FForceUnprepare := False;

    FBatchIters := 1;
    FBatchOffset := 0;
  end;
end;

procedure TOCICommand.Execute;
var
  E: Exception;
begin
  if GetCursorState = csExecuting then
    Exit;

  Executing := True;
  if FNonBlocking then begin
    if not FConnection.FThreadSafety then begin
      E := Exception.Create(SNeedThreadSafety);
      EndExecute(E);
      raise E;
    end;

  {$IFDEF MSWINDOWS}
    Assert(hExecThread = nil);
    hExecThread := FConnection.RunThread(DoExecute, EndExecute);
  {$ENDIF}
  end
  else begin
    try
      DoExecute;
    except
      on E: Exception do begin
        EndExecute(E);
        raise;
      end;
    end;
    EndExecute(nil);
  end;
end;

procedure TOCICommand.BreakExec;
begin
  if GetCursorState <> csInactive then
    FConnection.BreakExec;
end;

procedure TOCICommand.HardBreak;
{$IFDEF MSWINDOWS}
var
  E: Exception;
  Res: DWORD;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if GetCursorState <> csInactive {= csExecuting} then begin
    FConnection.BreakExec;

    Res := WaitForSingleObject(hExecThread.Handle, 3000);

    case Res of  // timeout 3 sec
      WAIT_TIMEOUT: begin
        if hExecThread <> nil then
          FConnection.StopThread(hExecThread);
        E := Exception.Create('HardBreak');
        try
          if GetCursorState = csExecuting then
            Finish;
        finally
          EndExecute(E);
          E.Free;
        end;
      end;
    end;
  end;
{$ENDIF}
end;

procedure TOCICommand.EndExecute(E: Exception);
var
  Fail: boolean;
begin
{$IFDEF MSWINDOWS}
  if hExecThread <> nil then begin
//    FConnection.StopThread(hExecThread);
    hExecThread := nil;
  end
  else if FNonBlocking then
    Exit;
{$ENDIF}

  Executing := False;  // Here for reexecute in AfterExecute

  if Assigned(FAfterExecute) then
    FAfterExecute(E = nil);

  if FNonBlocking and (E is EOraError) then begin
    Fail := True;
    FConnection.DoError(EOraError(E), Fail);
    if not Fail then
      Abort;
  end;
end;

{ Params }

function TOCICommand.AddParam: TParamDesc;
begin
  Result := inherited AddParam;
  TOraParamDesc(Result).FOwner := Self;
end;

class function TOCICommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TOCISQLInfo;
end;

class function TOCICommand.GetParserClass: TSQLParserClass;
begin
  Result := TOraParser;
end;

class function TOCICommand.GetParamDescClass: TParamDescClass;
begin
  Result := TOraParamDesc;
end;

{$IFNDEF LITE}

class function TOCICommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TOraMapRules;
end;

{$ENDIF}


function TOCICommand.ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string;
var
  Param: TOraParamDesc;
  Parser: TOraParser;
  Code: integer;
  St: string;
  SQLType: word;
  IsReturning: boolean;
  AllParams: TStringList;
begin
  if RenamePrefix <> '' then
    Result := inherited ParseSQL(SQL, nil, RenamePrefix)
  else
    Result := SQL;

  FParamsInfo.Clear;
  if not FScanParams or (Params = nil) then
    exit;

  FParsedSQLType := qtUnknown;


  Params.Clear;
  Parser := TOraParser.Create(Result);
  Parser.DecSeparator := '.';
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  AllParams := TStringList.Create;
  AllParams.Sorted := True;
  try
    SQLType := 0;
    IsReturning := False;
    Parser.ToBegin;
    repeat
      Code := Parser.GetNext(St);
      while (Code <> lcEnd) and
            (Code <> lxColon)
      do begin // ':'
        if (Code = lxCREATE) or (Code = lxTRIGGER) then begin
          Code := lcEnd;
          break;
        end
        else if SQLType = 0 then
          case Code of
            lxSELECT, lxWITH: begin
              SQLType := OCI_STMT_SELECT;
              FParsedSQLType := qtSelect;
            end;
            lxINSERT: begin
              SQLType := OCI_STMT_INSERT;
              FParsedSQLType := qtInsert;
            end;
            lxUPDATE: begin
              SQLType := OCI_STMT_UPDATE;
              FParsedSQLType := qtUpdate;
            end;
            lxDELETE: begin
              SQLType := OCI_STMT_DELETE;
              FParsedSQLType := qtDelete;
            end;
            lxBEGIN:
              SQLType := OCI_STMT_BEGIN;
            lxDECLARE:
              SQLType := OCI_STMT_DECLARE;
            lxEXPLAIN:
              SQLType := OCI_STMT_EXPLAIN;
          end
        else // if SQLType is already detected
          case Code of
            lxRETURNING:
              IsReturning := True;
            lxFROM, lxSET, lxWHERE:
              IsReturning := False;
          end;
        Code := Parser.GetNext(St);
      end;

      if (Code = lxColon)  then begin
          Code := Parser.GetNext(St);

        if (Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst) then begin // PL/SQL reserved words is allowed
          if AllParams.IndexOf(St) < 0 then begin
            Param := TOraParamDesc(AddParam);
            Param.FOwner := Self;
            Param.Name := St;
            AllParams.Add(St);

            if SQLType in [OCI_STMT_INSERT, OCI_STMT_UPDATE] then
              if not IsReturning then
                Param.FCanBeOutput := False;
          end
          else begin
            if (SQLType in [OCI_STMT_INSERT, OCI_STMT_UPDATE]) and IsReturning then begin
              Param := TOraParamDesc(Params.FindParam(St));
              if Param <> nil then
                Param.FCanBeOutput := True;
            end;
          end;
        end;
      end;
    until Code = lcEnd;


  finally
    AllParams.Free;
    Parser.Free;
  end;
end;

procedure TOCICommand.ParseSQLType;
var
  Parser: TSQLParser;
  Code: integer;
begin
  if FParsedSQLType <> qtUnparsed then
    Exit;

  FParsedSQLType := qtUnknown;

  Parser := TOraParser.Create(FSQL);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;

    // omit begin brackets
    repeat
      Code := Parser.GetNextToken;
    until Code <> lxLeftBracket;

    // Detect statement type
    case Code of
      lxSELECT, lxWITH:
        FParsedSQLType := qtSelect;
      lxINSERT:
        FParsedSQLType := qtInsert;
      lxUPDATE:
        FParsedSQLType := qtUpdate;
      lxDELETE:
        FParsedSQLType := qtDelete;
    end;
  finally
    Parser.Free;
  end;
end;

function TOCICommand.IsValidBatchSQL: boolean;
begin
    Result := True;
end;

procedure TOCICommand.BindParams;
var
  i: integer;
begin
  FConnection.Lock;
  try
    for i := 0 to FParams.Count - 1 do
      BindParam(TOraParamDesc(FParams[i]));
  finally
    FConnection.Release;
  end;

  SetCursorState(csBound);
end;

{procedure TOCICommand.DisconnectParams;
var
  Param: TOraParamDesc;
  i: integer;
begin
  for i := 0 to FParams.Count - 1 do begin
    Param := TOraParamDesc(FParams[i]);
    case Param.FDataType of
      dtOraBlob, dtOraClob, dtWideOracClob:
        Param.GetAsOraBlob.FreeLob;
      dtObject, dtArray:
        TOraObject(Param.GetObject).FreeObject;
    end;
  end;
end;}

function TOCICommand.GetParam(Index: integer): TOraParamDesc;
begin
  Result := TOraParamDesc(FParams[Index]);
end;

{ Strored Proc }

function TOCICommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;
const
  STab = '  ';
var
  SPName: string;
  SPOverload: integer;
  PassByName: boolean;
  St, DeclSt, OutSt: string;
  i: integer;
  Added: boolean;
  ResParamDesc, ParamDesc: TOraParamDesc;

  function PrepareVariable(AParamDesc: TOraParamDesc): string;
  begin
    case AParamDesc.DataType of
      dtBoolean: begin
        DeclSt := DeclSt + STab + 'v_' + AParamDesc.Name + ' boolean';
        if AParamDesc.ParamType in [pdUnknown, pdInput, pdInputOutput] then begin
          DeclSt := DeclSt + ' := sys.DIUTIL.INT_TO_BOOL(:' + AParamDesc.Name + ');' + DALineSeparator;
        end else
          DeclSt := DeclSt + ';' + DALineSeparator;
        if AParamDesc.ParamType in [pdUnknown, pdInputOutput, pdOutput, pdResult] then begin
          OutSt := OutSt + STab + ':' + AParamDesc.Name + ' := sys.DIUTIL.BOOL_TO_INT(v_' + AParamDesc.Name + ');' + DALineSeparator;
        end;
        Result := 'v_' + AParamDesc.Name;
      end;
      else
        Result := ':' + AParamDesc.Name;
    end;
    if (AParamDesc.ParamType <> pdResult) and PassByName then
      Result := AParamDesc.Name + ' => ' + Result;
  end;

  function FindResultParam: TOraParamDesc;
  var
    i: integer;
  begin
    Result := nil;

    for i := 0 to FParams.Count - 1 do
      if (FParams[i] <> nil) then
        if TOraParamDesc(FParams[i]).ParamType = pdResult then begin
          Result := TOraParamDesc(FParams[i]);
          break;
        end;
  end;

begin
  TOCISQLInfo.ParseSPName(Name, SPName, SPOverload);

  if NeedDescribe then begin
    InitProcParams(SPName, SPOverload);
    FForceSPInit := False;
  end;

  ResParamDesc := FindResultParam;

  PassByName := ForceProcNamedParams or FProcNamedParams;
  Added := False;
  for i := FParams.Count - 1 downto 0 do begin
    ParamDesc := TOraParamDesc(FParams[i]);
    if not NeedBindParam(ParamDesc) then begin
      if Added then begin
        PassByName := True;
        break;
      end;
    end
    else
      if ParamDesc <> ResParamDesc then
        Added := True;
  end;

  St := 'begin' + DALineSeparator;
  St := St + STab;

  DeclSt := '';
  OutSt := '';

  if ResParamDesc <> nil then begin
    St := St + PrepareVariable(ResParamDesc) + ' := ';
  end;
  St := St + SPName;

  Added := False;
  for i := 0 to FParams.Count - 1 do begin
    ParamDesc := TOraParamDesc(FParams[i]);
    if (ParamDesc <> ResParamDesc) and NeedBindParam(ParamDesc) then begin
      if not Added then
        St := St + '('
      else
        St := St + ', ';
      St := St + PrepareVariable(ParamDesc);
      Added := True;
    end;
  end;

  if Added then
    St := St + ')';

  St := St + ';' + DALineSeparator;
  if DeclSt <> '' then
    St := 'declare' + DALineSeparator + DeclSt + St;
  if OutSt <> '' then
    St := St + OutSt;
  St := St + 'end;';

  Result := St;
  FSQL := St;
  FUserSQL := Result;
end;

function TOCICommand.ForceCreateSPParams: boolean;
begin
  Result := not (FProcNamedParams or ForceProcNamedParams);
end;

procedure TOCICommand.Lock;
begin
  if FNonBlocking then
    FLock.Enter;
end;

procedure TOCICommand.Release;
begin
  if FNonBlocking then
    FLock.Leave;
end;

procedure TOCICommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
  {$IFDEF MSWINDOWS}
    if hExecThread <> nil then
      FConnection.StopThread(hExecThread, True);
  {$ENDIF}
    if GetActive then
      Close;

    inherited;

    FConnection := TOCIConnection(Value);
  end;
end;

function TOCICommand.GetCursorState: TCursorState;
begin
//  WAR Blocked
  Result := FCursorRef.State;
end;

procedure TOCICommand.SetCursorState(Value: TCursorState);
begin
  if FNonBlocking then begin
    Lock;
    FCursorRef.FState := Value;
    Release;
  end
  else
    FCursorRef.FState := Value;
end;

function TOCICommand.GetCursor: TCRCursor;
begin
  Result := FCursorRef;
end;

procedure TOCICommand.SetCursor(Value: TCRCursor);
begin
  if Value <> FCursorRef then begin
    if (FCursorRef <> nil) and (FCursorRef <> FCursor) then
      FCursorRef.Release;

    if (Value <> nil) and (Value <> FCursor) then begin
      FCursorRef := Value as TOraCursor;
      FCursorRef.AddRef;

      if (FCursorRef.OCISvcCtx <> nil) and (FOCISvcCtx = nil) then begin
        FOCISvcCtx :=  FCursorRef.OCISvcCtx;
      {$IFNDEF AUTOREFCOUNT}
        FOCISvcCtx.AddRef;
      {$ENDIF}
      end;
      FOCICallStyle := FCursorRef.FOCICallStyle;

      FFetchedRows := 0;
    end
    else
      FCursorRef := FCursor;
  end;
end;

function TOCICommand.GetNextCursor: TOraCursor;
var
  i: integer;
  Param: TOraParamDesc;
  Cursor: TOraCursor;
  Found: boolean;
begin
  Found := False;

  if FResultSets <> nil then
    for i := 0 to FResultSets.Count - 1 do begin
      Cursor := FResultSets[i];
      if not Found then begin
        if Cursor = FCursorRef then
          Found := True;
      end
      else if Cursor.CanFetch then begin
        Result := Cursor;
        Exit;
      end;
    end;

  for i := 0 to FParams.Count - 1 do begin
    Param := TOraParamDesc(FParams[i]);
    if Param.GetDataType = dtCursor then begin
      Cursor := Param.GetAsCursor;
      if not Found then begin
        if Cursor = FCursorRef then
          Found := True;
      end
      else if Cursor.CanFetch then begin
        Result := Cursor;
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

function TOCICommand.GetRowId: string;
type
  TBuf = array [0..35] of byte;
  PBuf = ^TBuf;
var
  hRowId: pOCIRowid;
  Ptr: IntPtr;
  RowId81Ptr: pRowId81;
  Res: sword;
  PLen: IntPtr;
  Len: ub2;
begin
  if GetCursorState = csInactive then begin
    Result := FRowId;
    Exit;
  end;

  PLen := Marshal.AllocHGlobal(sizeof(Integer));
  try
    if FOCICallStyle = OCI73 then begin
      Result := RowId7ToString(@FCursorRef.CDA.rid)
    end
    else
    if FOCICallStyle = OCI80 then begin
      Result := '';
      Check(OCISvcCtx.OCI8.OCIDescriptorAlloc(OCISvcCtx.hOCIEnv, Ptr, OCI_DTYPE_ROWID, 0, nil));
      Res := OCISvcCtx.OCI8.OCIAttrGet1(FCursorRef.OCIStmt, OCI_HTYPE_STMT, Ptr, PLen, OCI_ATTR_ROWID, OCISvcCtx.hOCIError);
      hRowId := Ptr;
      try
        if Res <> OCI_NO_DATA then begin
          Check(Res);
          if OCISvcCtx.Home.OCIVersion >= 9000 then begin
            Len := 4000;
            Ptr := Marshal.AllocHGlobal(Len);
            try
              Check(OCISvcCtx.OCI8.OCIRowidToChar(hRowId, Ptr, Len, OCISvcCtx.hOCIError));
              Result := string(Marshal.PtrToStringAnsi(Ptr, Len));
            finally
              Marshal.FreeHGlobal(Ptr);
            end;
          end
          else
          if OCISvcCtx.Home.OCIVersion >= 8100 then begin
            RowId81Ptr := pOCIRowid81(IntPtr(hRowId)).RowId;
            if IntPtr(RowId81Ptr) <> nil then begin
              if RowId81Ptr.filler = 2 then begin// UROWID
                Result := URowIdToString(RowId81Ptr, hRowId.RowId.ridfilenum)
              end
              else
                Result := RowId81ToString(RowId81Ptr);
            end;
          end
          else
            Result := RowId8ToString(@hRowId.RowId);
        end;
      finally
        if not IsLibrary then
        /// We are forced to disable OCIDescriptorFree call in library because of
        /// strange bug on second DML execute with it under Windows XP,
        /// Windows 98 SE. Now memory will leak. :(
          Check(OCISvcCtx.OCI8.OCIDescriptorFree(hRowId, OCI_DTYPE_ROWID));
      end;
    end
    else
      CheckOCI;
  finally
    Marshal.FreeHGlobal(PLen);
  end;
end;

function TOCICommand.GetSQLType: integer;
begin
  Result := FLastSQLType;
end;

function TOCICommand.GetActive: boolean;
begin
  Result := GetCursorState <> csInactive;
end;

procedure TOCICommand.CreateBatchCommand;
begin
  // Empty
end;

function TOCICommand.NeedBatchSavepoint: boolean;
begin
  Result := FAutoCommit;
end;

procedure TOCICommand.InternalExecuteBatch(Iters, Offset: integer);
begin
  FBatchIters := Iters;
  FBatchOffset := Offset;
  FParamsProcessed := 0;
  Execute;
end;

function TOCICommand.GetPrepared: boolean;
begin
  Result := GetCursorState >= csPrepared;
end;

procedure TOCICommand.SetOCICallStyle(Value: TOCICallStyle);
begin
  if Value <> FOCICallStyle then begin
    if GetActive then
      Close;

    FOCICallStyle := Value;
    FCursorRef.FOCICallStyle := FOCICallStyle;
  end;
end;

procedure TOCICommand.SetArrayLength(Value: integer);
var
  i : integer;
begin
  for i := 0 to FParams.Count - 1 do
    TOraParamDesc(FParams[i]).FArraySize := Value;
end;

function TOCICommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommandTimeout := Value;
    prNonBlocking:
  {$IFDEF MSWINDOWS}
      FNonBlocking := Boolean(Value);
  {$ELSE}
      FNonBlocking := False;  // doesn't support non blocking
  {$ENDIF}
    prFieldsAsString:
      FFieldsAsString := Boolean(Value);
    prCacheLobs:
      FCacheLobs := Boolean(Value);
    prScrollableCursor:
      FCursorRef.FScrollable := Boolean(Value);
    prStoreRowId:
      FStoreRowId := Boolean(Value);
    prRawAsString:
      FRawAsString := Boolean(Value);
    prNumberAsString:
      FNumberAsString := Boolean(Value);
    prSmallintPrecision:
      FSmallintPrecision := Value;
    prIntegerPrecision:
      FIntegerPrecision := Value;
    prFloatPrecision:
      FFloatPrecision := Value;
    prLargeintPrecision:
      FLargeIntPrecision := Value;
    prBCDPrecision:
      GetPrecAndScale(Value, FBCDPrecision, FBCDScale);
    prFmtBCDPrecision:
      GetPrecAndScale(Value, FFmtBCDPrecision, FFmtBCDScale);
    prTemporaryLobUpdate:
      FTemporaryLobUpdate := Boolean(Value);
    prPrefetchLobSize:
      FPrefetchLobSize := Value;
    prStatementCache:
      FStatementCache := Boolean(Value);
    prPrefetchRows:
      FCursor.PrefetchRows := Value;
    prCheckParamHasDefault:
      FCheckParamHasDefault := Value;
    prUseDefaultDataTypes:
      FUseDefaultDataTypes := Value;
    prProcNamedParams:
      FProcNamedParams := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCICommand.GetProp(Prop: integer; out Value: variant): boolean;
var
  Buf: integer;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      Value := FCommandTimeout;
    prNonBlocking:
  {$IFDEF MSWINDOWS}
      Value := FNonBlocking;
  {$ELSE}
      Value := False;  // doesn't support non blocking
  {$ENDIF}
    prRowsProcessed:
      if FFetchedRows > 0 then
        Value := FFetchedRows
      else
        Value := FRowsProcessed;
    prSQLType:
      Value := FSQLType;
    prErrorOffset: begin
      Buf := FErrorOffset;
      RemoveCRSymbols(TrimRight(FSQL), Buf);
      Value := Buf;
    end;
    prStoreRowId:
      Value := FStoreRowId;
    prTemporaryLobUpdate:
      Value := FTemporaryLobUpdate;
    prPrefetchLobSize:
      Value := FPrefetchLobSize;
    prFieldsAsString:
      Value := FFieldsAsString;
    prCacheLobs:
      Value := FCacheLobs;
    prRawAsString:
      Value := FRawAsString;
    prNumberAsString:
      Value := FNumberAsString;
    prProcNamedParams:
      Value := FProcNamedParams;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

{ TOCISQLInfo }

function TOCISQLInfo.NextCharQuotesNeed(Ch: Char; IdCase: TIdentCase): boolean;
begin
  case IdCase of
    icUpper:
      case Ch of
        'A'..'Z', '_', '0'..'9', '$', '#': Result := False;
      else
        Result := True;
      end;
    icLower:
      case Ch of
        'a'..'z', '_', '0'..'9', '$', '#': Result := False;
      else
        Result := True;
      end;
  else
    case Ch of
      'a'..'z', 'A'..'Z', '_', '0'..'9', '$', '#': Result := False;
    else
      Result := True;
    end;
  end;
end;

function TOCISQLInfo.HasAsLexem: boolean;
begin
  Result := False;
end;

procedure TOCISQLInfo.ParseExtTableInfo(Parser: TSQLParser; var CodeLexem: integer; var StLex: string; var Name: string);
var
  PriorLexem: integer;
  PriorStLex: string;
begin
  if StLex = '@' then begin
    CodeLexem := Parser.GetNext(StLex);
    if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) then begin
      Name := Name + '@';
      repeat
        PriorLexem := CodeLexem;
        PriorStLex := StLex;
        Name := Name + StLex;
        CodeLexem := Parser.GetNext(StLex);
        if (StLex = '.') and
          ((PriorLexem = lcIdent) or (PriorLexem >= lxSQLFirst))
        then
          continue;
        if (PriorStLex = '.') and
          ((CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst))
        then
          continue;
        break;
      until False;
    end;
  end;

  if CodeLexem = lxPARTITION then begin
    CodeLexem := Parser.GetNext(StLex);
    if CodeLexem = lxLeftBracket then
      if Parser.ToLexem(lxRightBracket) <> lcEnd then
        CodeLexem := Parser.GetNext(StLex);
  end;
end;

function TOCISQLInfo.IdentCase: TIdentCase;
begin
  Result := icUpper;
end;

function TOCISQLInfo.NormalizeName(const Value: string; const LeftQ: Char; const RightQ: Char; QuoteNames: boolean = False; UnQuoteNames: boolean = False): string;

  function GetDelimiterPosition: Integer;
  var
    i: Integer;
    InQuote: boolean;
  begin
    InQuote := False;
    for i := 1 to Length(Value) do
      if Value[i] = '"' then
        InQuote := not InQuote
      else if not InQuote and ((Value[i] = '.') or  (Value[i] = '@')) then begin
        Result := i;
        Exit;
      end;
    Result := 0;
  end;

  function NormalizeStr(const Str: string): string;
  begin
    if not IsQuoted(Str) and not QuoteNames then
      Result := AnsiUpperCase(Str)
    else
      Result := Str;

    if not UnQuoteNames and (QuoteNames or QuotesNeeded(Result)) then
      Result := Quote(Result, LeftQ, RightQ)
    else
      Result := UnQuote(Result);
  end;

var
  i: integer;
begin
  if Value = '' then begin
    Result := '';
    Exit;
  end;

  i := GetDelimiterPosition;
  if i > 0 then
    Result := NormalizeStr(Copy(Value, 1, i - 1)) +
              Value[i] +
              NormalizeName(Copy(Value, i + 1, Length(Value) - i), LeftQ, RightQ, QuoteNames, UnQuoteNames)
  else
    Result := NormalizeStr(Value);
end;

function TOCISQLInfo.NormalizeName(const Value: string; QuoteNames: boolean = False; UnQuoteNames: boolean = False): string;
begin
  Result := NormalizeName(Value, LeftQuote, RightQuote, QuoteNames, UnQuoteNames);
end;

procedure TOCISQLInfo.SplitObjectName(const Name: string; out Info: TSQLObjectInfo);
var
  i, p, Len: integer;
  InQuote: boolean;
begin
  Info.Name := '';
  Info.Schema := '';
  Info.Catalog := '';
  Info.DBLink := '';

  Len := Length(Name);
  p := 1;
  InQuote := False;

  for i := 1 to Len + 1 do begin
    if (i <= Len) and (Name[i] = '"') then
      InQuote := not InQuote;

    if (i = Len + 1) or (not InQuote and (Name[i] = '.')) then begin
      if p > 1 then
        Info.Schema := Info.Name;
      Info.Name := Copy(Name, p, i - p);
      p := i + 1;
    end;

    if (i <= Len) and not InQuote and (Name[i] = '@') then begin
      if p > 1 then
        Info.Schema := Info.Name;
      Info.Name := Copy(Name, p, i - p);
      Info.DBLink := Copy(Name, i + 1, MaxInt);
      break;
    end;
  end;
end;

class function TOCISQLInfo.ParseSPName(FullName: string; var Name: string; var Overload: integer): boolean;
var
  i: integer;
  str_index: integer;
begin
  Result := True;

  str_index := 0;
  for i := Length(FullName) downto 1 do begin
    if (FullName[i] = '"') or (FullName[i] = '.') then begin
      break;
    end;
    if FullName[i] = ':' then begin
      if str_index = 0 then
        str_index := i
      // if more then one ":"
      else begin
        Result := False;
        break;
      end;
    end;
  end;

  if Result and (str_index > 0) then
    if TryStrToInt(Trim(copy(FullName, str_index + 1, MaxInt)), Overload) then
      Name := Trim(copy(FullName, 1, str_index - 1))
    else begin
      Result := False;
      Name := FullName;
      Overload := 0;
    end
  else begin
    Name := FullName;
    Overload := 0;
  end;
end;

class function TOCISQLInfo.GetFinalSPName(Name: string; Overload: integer): string;
var
  SPOverload: integer;
begin
  if (Name <> '') and (Overload > 0) then begin
    if ParseSPName(Name, Result, SPOverload) then
      Result := Result + ':' + IntToStr(Overload)
    else
      Result := Name + ':' + IntToStr(Overload);
  end
  else
    Result := Name;
end;

{ TOCIFieldDesc }

constructor TOCIFieldDesc.Create(RecordSetClass: TRecordSetClass);
begin
  inherited;

  FFetchBufferOffset := -1;
  FFetchBufferLenOffset := -1;
end;

procedure TOCIFieldDesc.SetDataType(Value: Word);
begin
  inherited;
  FIsConverted := TOCIRecordSet.IsConvertedFieldType(Value)
end;

procedure TOCIFieldDesc.SetSubDataType(Value: Word);
begin
  inherited;
  FIsBlobSubType := RecordSetClass.IsBlobDataType(Value);
  FIsNational := Value in [dtNString, dtNWideString, dtFixedNChar, dtFixedNWideChar, dtNClob];
end;

{ TOCIRecordSet }

constructor TOCIRecordSet.Create;
begin
  inherited Create;

  FRequireEmptyStrToNull := True;

  FFetchRows := 25;
{$IFDEF MSWINDOWS}
  hEvent := TEvent.Create(nil, True, True, '');
{$ENDIF}
  FHasObjectFields := False;
  FHideRowId := True;

  FExpandedFields := TFieldDescs.Create;
end;

destructor TOCIRecordSet.Destroy;
begin
  Close;

{$IFDEF MSWINDOWS}
  if hExecFetchThread <> nil then
    FConnection.StopThread(hExecFetchThread, True);
  if hFetchAllThread <> nil then
    FConnection.StopThread(hFetchAllThread, True);
  hEvent.Free;
{$ENDIF}

  FreeFetchBuffer;

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  FExpandedFields.Free;

  inherited;
end;

procedure TOCIRecordSet.AllocOCISvcCtx;
begin
  if FOCISvcCtx <> FConnection.OCISvcCtx then begin
    ReleaseOCISvcCtx;

    FOCISvcCtx := FConnection.OCISvcCtx;
  {$IFNDEF AUTOREFCOUNT}
    if FOCISvcCtx <> nil then
      FOCISvcCtx.AddRef;
  {$ENDIF}
  end;
end;

procedure TOCIRecordSet.ReleaseOCISvcCtx;
begin
  if FOCISvcCtx <> nil then begin
  {$IFNDEF AUTOREFCOUNT}
    FOCISvcCtx.ReleaseRef;
  {$ENDIF}
    FOCISvcCtx := nil;
  end;
end;

procedure TOCIRecordSet.CreateCommand;
begin
  SetCommand(TOCICommand.Create);
end;

procedure TOCIRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TOCICommand(Value);
  if FCommand <> nil then
    FConnection := FCommand.FConnection;
end;

function TOCIRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TOCIFieldDesc;
end;

procedure TOCIRecordSet.DetectFieldType(const FieldName: string; DBType: Word; DBLength: Integer; DBScale: SmallInt;
  ObjectType: TObjectType; CharUsed: Boolean; out DataType, SubDataType: Word; out Length, Scale: Integer; out Fixed: boolean);

  function GetNlsFieldDesc(NlsType: TNlsParamType): Integer;
  var
    Str: string;
  begin
    if FConnection.FNlsParams[NlsType].Value = '' then
      FConnection.GetSessionParameters;

    Str := UpperCase(FConnection.FNlsParams[NlsType].Value);

    Result := System.Length(Str);
    if Pos('MONTH', Str) > 0 then
      Result := Result + 7
    else if Pos('MON', Str) > 0 then
      Result := Result + 3;
    if Pos('DAY', Str) > 0 then
      Result := Result + 9
    else if Pos('DY', Str) > 0 then
      Result := Result + 3;
    if Pos('DDSPTH', Str) > 0 then
      Result := Result + 14;
    if (NlsType in [nlsTimeStampFormat, nlsTimeStampTZFormat]) and (Pos('SSXFF', Str) > 0) then
      Result := Result + 4;
  end;

{$IFNDEF LITE}
var
  FetchConverter: TFetchConverter;
{$ENDIF}
begin
  // set default SubDataType
  SubDataType := FCommand.GetInternalType8(DBType);

  // Fixed
  Fixed := (DBType in [oraChar, oraNChar]);

  Length := 0;
  Scale := 0;

  if (DBType = {$IFNDEF UNIDACPRO}OraDataTypeMap.{$ELSE}OraDataTypeMapUni.{$ENDIF}oraNumber) and
     (DBLength = 39) and (DBScale = 39)
  then
    SubDataType := dtNumberFloating;

{$IFNDEF LITE}
  FetchConverter := GetMapFetchConverter(FieldName, DBType, DBLength, DBScale);
  if FetchConverter <> nil then
    DataType := FetchConverter.InternalDataType
  else
{$ENDIF}
    FCommand.DetectDataType(DBType, DBLength, DBScale, ObjectType, FLongStrings, DataType);

  if DataType in [dtWideString, dtWideMemo] then
    case SubDataType of
      dtString:
        SubDataType := dtWideString;
      dtNString:
        SubDataType := dtNWideString;
      dtFixedChar:
        SubDataType := dtFixedWideChar;
      dtFixedNChar:
        SubDataType := dtFixedNWideChar;
    end;

  if DBType in [oraNChar, oraNVarchar2, oraNClob] then begin
    if DataType = dtString then
      case SubDataType of
        dtString:
          SubDataType := dtNString;
        dtFixedChar:
          SubDataType := dtFixedNChar;
      end
    else if DataType = dtWideString then
      case SubDataType of
        dtWideString:
          SubDataType := dtNWideString;
        dtFixedWideChar:
          SubDataType := dtFixedNWideChar
      end
    else if DataType in [dtMemo, dtWideMemo, dtOraClob, dtWideOraClob] then
      case SubDataType of
        dtOraClob:
          SubDataType := dtNClob;
        dtWideOraClob:
          SubDataType := dtNClob;
      end;
  end;

  case SubDataType of
    dtString, dtNString, dtFixedChar, dtFixedNChar,
    dtWideString, dtNWideString, dtFixedWideChar, dtFixedNWideChar:
    begin
      Length := DBLength;

      if CharUsed and (SubDataType in [dtString, dtNString,  dtFixedChar, dtFixedNChar]) then begin
        // for NVARCAHR and FCharLength = 1 or 0 - each char has size 2 bytes
        // in non Unicode mode and CharLength > 1 size of field = length in chars * size of char in bytes
        if FConnection.FCharLength > 1 then
          Length := DBLength * FConnection.FCharLength;
      end;

      if FConnection.ConvertEOL then
        Length := (Length * 3) shr 1; // data can grow on ConvertEOL
    end;
    dtNumber, dtNumberFloating, dtFloat, dtBFloat, dtBDouble: begin
      if SubDataType in [dtBFloat, dtBDouble] then begin
        Length := 38;
        Scale := 127;
      end
      else begin
        Length := DBLength;
        Scale := DBScale;
      end;

      if DataType = dtString then begin // Number as string
        if DBLength = 0 then
          // max scale = 130
          Length := 132
        else if DBScale > 39 then // FLOAT
          Length := DBScale + 2
        else // NUMBER
          Length := DBlength + 2;
      end
      else if DataType = dtBCD then begin
        if Length > MaxBcdPrecision then
          Length := MaxBcdPrecision;
        if Scale > MaxBcdScale then
          Scale := MaxBcdScale;
      end
      else if DataType = dtFMTBCD then begin
        if Length > MaxFMTBcdDigits then
          Length := MaxFMTBcdDigits;
        if Scale > Length then // if length was reduced
          Scale := Length;
      end
    end;
    dtRowId:
      Length := RowIdSize;
    dtURowId:  // ROWID is bound as string
      if DBLength > SizeOf(IntPtr) then // UROWID has its actual datasize
        Length := Ceil(DBLength * 1.34) // for correct conversion into 64 base string
      else
        Length := RowIdSize;
    dtVarBytes, dtExtVarBytes:
      if DataType = dtWideString then
        Length := DBLength * 2
      else if DataType = dtString then
        Length := DBLength * 2
      else
        Length := DBLength;
    dtDateTime: begin
      if DataType in [dtString, dtExtString, dtWideString, dtExtWideString] then
        Length := GetNlsFieldDesc(nlsDateFormat);
    end;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
    dtIntervalYM, dtIntervalDS: begin
      if not (DataType in [dtString, dtExtString, dtWideString, dtExtWideString]) then begin
        Length := DBLength;
        if DataType <> dtIntervalYM then
          Scale := DBScale;
      end
      else // FieldsAsString := True
        case SubDataType of
          dtTimeStamp, dtTimeStampLTZ:
            Length := GetNlsFieldDesc(nlsTimeStampFormat);
          dtTimeStampTZ:
            Length := GetNlsFieldDesc(nlsTimeStampTZFormat);
          dtIntervalYM:
            Length := DBLength + 4; // interval year to month form is +Prec-MM
          dtIntervalDS:
            Length := DBLength + DBScale + 11; // interval day to second form is +Prec HH:MM:SS.Scale
        end;
    end;
  end;

  if not FFlatBuffers and (Length >= FlatBufferLimit) then
    if DataType = dtString then
      DataType := dtExtString
    else if DataType = dtWideString then
      DataType := dtExtWideString
    else if DataType = dtVarBytes then
      DataType := dtExtVarBytes;

end;

{ Open /Close }

function TOCIRecordSet.NeedInitFieldsOnPrepare: boolean;
begin
  Result := inherited NeedInitFieldsOnPrepare or HasCursorParams;
end;

procedure TOCIRecordSet.InternalPrepare;
begin
  if FCommand.NativeCursor then
    FCommand.Prepare;

  InitFetchCursor;

  SetCommandType;
end;

procedure TOCIRecordSet.InternalUnPrepare;
begin
  if FCommand.FOpenNext then
    Exit;

  if not FCommand.NativeCursor then begin
    try
      if FCommand.GetCursorState <> csInactive then
        FCommand.Close;
    finally
      FCommand.SetCursor(nil);
      FFetchCursor.FreeCursor; // ignore ref count
    end;
  end;

  if FCommand.GetCursorState <> csInactive then
    FCommand.Close;

  // CommandType & FCommand.FSQLType should be reset in any case
  // (even if FCommand.GetCursorState = csInactive)
  FCommand.CommandType := ctUnknown;
  FCommand.FSQLType := SQL_UNKNOWN;
end;

procedure TOCIRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  AllocOCISvcCtx;
  try
    inherited;
  except
    ReleaseOCISvcCtx;
    raise;
  end;
end;

procedure TOCIRecordSet.InternalClose;
{$IFDEF MSWINDOWS}
var
  AMsg: TMSG;
{$ENDIF}
begin
  if (hExecFetchThread <> nil) or (hFetchAllThread <> nil) then begin
    if FCommand.Executing then // doesn't break if not Executing
      FCommand.BreakExec;
  {$IFDEF MSWINDOWS}
    // Additional WaitForSingleObject is set to wait for event to pulse on case
    // if hExecFetchThread is in FetchArray or in FetchPiece function. This was
    // made to ensure that this thread will wait for for final SetEvent in
    // DoExecFetch procedure.
    WaitForSingleObject(THandle(hEvent.Handle), INFINITE);
    WaitForSingleObject(THandle(hEvent.Handle), INFINITE);
    while hExecFetchThread <> nil do begin
      if PeekMessage(AMsg, hODACWindow, 0, 0, PM_REMOVE) then
        DispatchMessage(AMsg);
    end;
  {$ENDIF}
  end;

  if GetNonBlocking and (hFetchAllThread <> nil) then begin
    FWaitForFetchBreak := True;
    while FFetching do
      Sleep(0);
  {$IFDEF MSWINDOWS}
    if PeekMessage(AMsg, hODACWindow, WM_ENDTHREAD, WM_ENDTHREAD, PM_REMOVE) then
      DispatchMessage(AMsg);
  {$ENDIF}
  end;

  if not FCommand.NativeCursor then begin
    if FCommand.GetCursorState <> csInactive then
      if FCommand.FCursor.State <> csInactive then
        FCommand.Finish
      else
        FCommand.Close;
    FFetchCursor.FreeCursor; // ignore ref count
    FCommand.SetCursor(nil);
  end;

  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  inherited;

  FreeFetchBuffer;
  if not Prepared then
    InternalUnprepare;

  ReleaseOCISvcCtx;
end;

procedure TOCIRecordSet.SetCommandType;
begin
  case FCommand.FSQLType of
    SQL_UNKNOWN:
      FCommand.CommandType := ctUnknown;
    SQL_SELECT:
      FCommand.CommandType := ctCursor;
  else
    if FCommand.RowsReturn or (FCommand.FSQLType = SQL_PLSQL) and (FFetchCursor <> nil) then
      FCommand.CommandType := ctCursor
    else
      FCommand.CommandType := ctStatement;
  end;
end;

function TOCIRecordSet.CanFetchBack: boolean;
begin
  Result := (FCommand.FCursorRef <> nil) and FCommand.FCursorRef.FScrollable;
end;

{ Edit }

procedure TOCIRecordSet.DoExecFetch;
var
  i: integer;
  OldExecuted: boolean;
  CanExecuteFetch: boolean;
  Field: TFieldDesc;
begin
{$IFDEF MSWINDOWS}
  if (hExecFetchThread <> nil) or (hFetchAllThread <> nil) then
    PulseEvent(THandle(hEvent.Handle));
{$ENDIF}
  try
    CanExecuteFetch := (FCommand.GetCursorState = csPrepared) and (FCommand.FSQLType = SQL_SELECT) and
      (FCommand.FOCICallStyle = OCI80) and not (OCISvcCtx.Home.PossibleOCICallStyles = [OCI80]);
    OldExecuted := (FCommand.GetCursorState = csExecuted);

    if FCommand.NativeCursor and not OldExecuted then begin
      if not Prepared then
        InternalPrepare;
      FCommand.BindParams;
      if not CanExecuteFetch then
        FCommand.Exec; //if DataSet is prepared(desribeonly) then execute and fetch can be combined
    end;

    if not FDisableInitFields and
      (not Prepared or NeedInitFieldsOnFetch)
    then
      CheckFieldDescs // FetchCursor will be init inside InitFields
    else
      InitFetchCursor; // FetchCursor must be init in any case

    if not RowsReturn then
      RaiseError(SNotRows);

    if CanFetchBack then begin
      OCISvcCtx.Home.CheckOCI90;
      FPieceFetch := True;
      BlockMan.DefaultItemCount := FFetchRows;
    end
    else begin
      FPieceFetch := False;
      if FCommand.FOCICallStyle = OCI73 then // piece fetch is used with OCI7 only
        for i := 0 to FFields.Count - 1 do begin
          Field := FFields[i];
          if (Field.DataType in [dtBlob, dtMemo, dtWideMemo]) and (Field.Length = 0)
          then begin
            FPieceFetch := True;
            break;
          end;
        end;
    end;

    FCommand.SetCursor(FFetchCursor);

    if GetNonBlocking then begin
      Fetch;
      CurrentItem := FirstItem;
      FBOF := IntPtr(FirstItem) = nil;
    end;
  finally
{$IFDEF MSWINDOWS}
    if (hExecFetchThread <> nil) or (hFetchAllThread <> nil) then
      hEvent.SetEvent;
{$ENDIF}
  end;
end;

procedure TOCIRecordSet.EndExecFetch(E: Exception);
var
  Fail: boolean;
begin
{$IFDEF MSWINDOWS}
  if hExecFetchThread <> nil then begin
//    FConnection.StopThread(hExecFetchThread);
    hExecFetchThread := nil;
  end
  else if (FCommand = nil) or (GetNonBlocking and (E = nil)) then
    Exit;
{$ENDIF}

  try
    if Assigned(FAfterExecFetch) then //moved here to start exactly after execute
      FAfterExecFetch(E = nil);

    if (E = nil) and
      (FFetchAll or {$IFNDEF LITE}(FSmartFetchState <> sfNone) or{$ENDIF} (GetNonBlocking and (IndexFieldCount > 0)))
    then
      if FCommand.GetCursorState < csFetched then
        FetchAll
      else
        if GetNonBlocking and (IndexFieldCount > 0) then
          SortItems;
    if (E = nil) and GetNonBlocking and (FTempFilterText <> '') then begin
      inherited SetFilterText(FTempFilterText);
      FTempFilterText := '';
      inherited FilterUpdated;
      if Assigned(FOnReopen) then
        FOnReopen;
    end;

    if FFetchAll and GetNonBlocking and (FCommand.GetCursorState <> csFetchingAll) then
      StopWait; //Reset screen cursor
  finally
    FCommand.Executing := False;
  end;

  if FCommand.FNonBlocking and (E is EOraError) then begin
    Fail := True;
    FConnection.DoError(EOraError(E), Fail);
    if not Fail then
      Abort;
  end;
end;

procedure TOCIRecordSet.ExecCommand(Iters: integer = 1; Offset: integer = 0); // Execute command
var
  NeedPrepare: boolean;
begin
  NeedPrepare := (FCommand.CommandType <> ctCursor) and not Prepared;

  if NeedPrepare then
    FCommand.Prepare;

  try
    inherited;

    InitFetchCursor;

    SetCommandType;

    FCommand.FForceUnprepare := FCommand.FNonBlocking and (FCommand.CommandType <> ctCursor);
  finally // for Unprepare on Exception
    if not FCommand.FNonBlocking and (FCommand.CommandType <> ctCursor) and NeedPrepare
      and FCommand.GetPrepared then //Disconnect mode collision (Unprepare occurs on Disconnect (AfterExecute))
      FCommand.Unprepare;
  end;
end;

procedure TOCIRecordSet.ExecFetch(DisableInitFields: boolean);
begin
  FCommand.Executing := True;
  if FCommand.FNonBlocking then begin
    if not FConnection.FThreadSafety then
      RaiseError(SNeedThreadSafety);

    // InitFields here for valid DataSet.CreateFieldDefs
    if not DisableInitFields then
    try
      CheckFieldDescs;
    {$IFNDEF LITE}
      InitExtFieldsInfo;
    {$ENDIF}
      SetCommandType;
    except
      on E: Exception do begin
        EndExecFetch(E);
        raise;
      end;
    end;
    FilterFunc := nil;
    inherited SetFilterText('');
  {$IFDEF MSWINDOWS}
    StringHeap.ThreadSafety := True;
    hEvent.ResetEvent;
    FDisableInitFields := True;
    hExecFetchThread := FConnection.RunThread(DoExecFetch, EndExecFetch);
  {$ENDIF}
  end
  else begin
    StringHeap.ThreadSafety := False;
    try
      FDisableInitFields := DisableInitFields;
      DoExecFetch;
    except
      on E: Exception do begin
        EndExecFetch(E);
        raise;
      end;
    end;
    EndExecFetch(nil);
  end;
end;

function TOCIRecordSet.IsFullReopen: boolean;
begin
  Result := inherited IsFullReopen or not FCommand.NativeCursor;
end;

procedure TOCIRecordSet.Reopen;
{$IFDEF MSWINDOWS}
var
  Msg: TMsg;
{$ENDIF}
begin
  if not IsFullReopen then begin
  {$IFDEF MSWINDOWS}
    if GetNonBlocking then begin
      while (hExecFetchThread <> nil) do
        PeekMessage(Msg, hODACWindow, 0, 0, PM_REMOVE);

      while (GetNonBlocking) and (hFetchAllThread <> nil) do begin
        FWaitForFetchBreak := True;
        PeekMessage(Msg, hODACWindow, 0, 0, PM_REMOVE);
      end;
    end;
  {$ENDIF}
  end
  else begin
    if FCommand.FSQLType = SQL_UNKNOWN then
      RaiseError(SReopenNotAllowed);
  end;

  inherited;
end;

procedure TOCIRecordSet.Disconnect;
var
  i: Integer;
  Item: PItemHeader;
  RecBuf: IntPtr;
  Field: TFieldDesc;
begin
  GetDisconnectedMode;
  GetUseUnicode;
  GetCharLength;

  if HasBlobFields then begin
    Item := FirstItem;
    while IntPtr(Item) <> nil do begin
      RecBuf := PtrOffset(Item, sizeof(TItemHeader));
      for i := 0 to Fields.Count - 1 do begin
        Field := Fields[i];
        if Field.IsBlob then
          TSharedObject(InternalGetObject(Field, RecBuf)).Disconnect;
      end;
      Item := Item.Next;
    end;
  end;

  inherited;
end;

{ Fields}

function TOCIRecordSet.GetIndicatorItemSize: Integer;
begin
  Result := sizeof(sb2);
end;

function TOCIRecordSet.HasCursorParams: boolean;
var
  i: integer;
begin
  Result := false;

  for i := 0 to FCommand.Params.Count - 1 do
    if FCommand.GetParam(i).DataType = dtCursor then begin
      Result := true;
      exit;
    end;
end;

function TOCIRecordSet.HasResultSets: boolean;
begin
  Result := (FCommand.FResultSets <> nil) and (FCommand.FResultSets.Count > 0);
end;

procedure TOCIRecordSet.InitFetchCursor;

  function IsValidCursor(Cursor: TOraCursor): Boolean;
  var
    State: integer;
  begin
    if (Cursor = nil) or (Cursor.hOCIStmt = nil) then
      Result := False
    else begin
      AllocOCISvcCtx;
      if OCISvcCtx.Home.OCIVersion >= 9100 then // supported starting with 9.1
        Check(OCI8.OCIAttrGet2(Cursor.OCIStmt, OCI_HTYPE_STMT, State, nil, OCI_ATTR_STMT_STATE, OCISvcCtx.hOCIError))
      else
        State := OCI_STMT_STATE_EXECUTED;
      Result := (State = OCI_STMT_STATE_EXECUTED);
    end;
  end;

var
  i: integer;
  Cursor: TOraCursor;
begin
  FFetchCursor := FCommand.FCursorRef;

  if FCommand.NativeCursor and (FCommand.FSQLType = SQL_PLSQL) then begin
    FFetchCursor := nil;

    if FCommand.FResultSets <> nil then
      for i := 0 to FCommand.FResultSets.Count - 1 do begin
        Cursor := FCommand.FResultSets[i];
        if IsValidCursor(Cursor) then begin
          FFetchCursor := Cursor;
          Break;
        end;
      end;

    if FFetchCursor = nil then
      for i := 0 to FCommand.Params.Count - 1 do
        if FCommand.GetParam(i).DataType = dtCursor then begin
          Cursor := FCommand.GetParam(i).GetAsCursor;
          if IsValidCursor(Cursor) then begin
            FFetchCursor := Cursor;
            Break;
          end;
        end;

    // redefine command type - depend on cursor: canbe executed or not
    if FFetchCursor = nil then
      FCommand.CommandType := ctStatement
    else
      FCommand.CommandType := ctCursor;
  end;
end;

class function TOCIRecordSet.GetBufferSize(DataType: Word; LengthInChars: integer): integer;
begin
  case DataType of
    dtDateTime:
      Result := 7;
    dtUndefined:
      Result := LengthInChars;
  else
    Result := inherited GetBufferSize(DataType, LengthInChars);
  end;
end;

procedure TOCIRecordSet.DetectIdentityField;
var
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  FIdentityField := nil;

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if CompareText(FieldDesc.ActualName, 'ROWID') = 0 then
      if FieldDesc.TableInfo = UpdatingTableInfo then begin
        FIdentityField := FieldDesc;
        Exit;
      end;
  end;
end;

function TOCIRecordSet.ExtFieldsInfoIsInternal: boolean;
begin
  Result := False;
end;

procedure TOCIRecordSet.CreateFieldDescs;

  function IsPrefetchAllowed: boolean;
  begin
    if FHasObjectFields or HasFields([dtBlob, dtMemo, dtWideMemo]) then
      Result := False
    else
      Result := True;
  end;

var
  i: integer;
  Field: TOCIFieldDesc;
  OldCursorState: TCursorState;
  OldCursor: TOraCursor;
  OldOCISvcCtx: TOCISvcCtx;
  DescribeExecute: boolean;
begin
  OldCursorState := FCommand.GetCursorState;
  OldCursor := FCommand.FCursorRef;
  OldOCISvcCtx := FOCISvcCtx;

  DescribeExecute := False;

  try
    FOCISvcCtx := FConnection.OCISvcCtx;

    if FCommand.NativeCursor then begin
      if FCommand.GetCursorState = csInactive then
        FCommand.Prepare;

      InitFetchCursor;

      if FCommand.FSQLType = SQL_SELECT then begin
        if (FCommand.FOCICallStyle = OCI80) and (FCommand.GetCursorState < csExecuted) then begin
          if FCommand.GetCursorState < csBound then  // to correct describe type of expression
            FCommand.BindParams;
          FCommand.InternalExecute(OCI_DESCRIBE_ONLY);
        end;
      end
      else if FCommand.FSQLType = SQL_PLSQL then begin
        if FCommand.GetCursorState < csBound then
          FCommand.BindParams;

        if FCommand.GetCursorState < csExecuted then begin
          FCommand.Exec; // need for describe
          DescribeExecute := True;
          InitFetchCursor; // exit if all returned cursors are not in executed state
        end;

        if FCommand.CommandType <> ctCursor then
          Exit;

        FCommand.SetCursor(FFetchCursor);
      end
      else
        Exit;
    end
    else begin
      InitFetchCursor;

      FCommand.CheckActive;
    end;

    FHasConvertedFields := False;

    FConnection.Lock;
    try
      i := 1;
      while True do begin
        Field := TOCIFieldDesc(CreateFieldDesc);
        try
          if GetFieldDesc(i, Field, FLongStrings, FFlatBuffers) then begin
            Field.ActualName := Field.Name;
            Field.FieldNo := FFields.Count + 1;
            FFields.Add(Field);
            Inc(i);

            FHasConvertedFields := FHasConvertedFields or TOCIFieldDesc(Field).IsConverted;

            if FConnection.FDisconnectMode then
              if Field.DataType in [dtObject, dtXML, dtAnyData, dtReference, dtArray, dtTable] then
                Field.ReadOnly := True;
            if Field.DataType in [dtObject, dtArray] then
              InitObjectFields(Field.ObjectType, Field);
          end
          else begin
            Field.Free;
            break;
          end;
        except
          Field.Free;
          raise;
        end;
      end;
    finally
      FConnection.Release;
    end;
  {$IFNDEF LITE}
    FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, FTablesInfo);
    FTablesInfo.CaseSensitive := True;
  {$ENDIF}
    FHasObjectFields := HasFields([dtObject, dtXML, dtAnyData, dtReference, dtArray, dtTable]);

    if FFetchCursor <> nil then
      if IsPrefetchAllowed then
        FFetchCursor.DefaultPrefetchRows := FFetchRows
      else
        FFetchCursor.DefaultPrefetchRows := 0;
  finally
    FOCISvcCtx := OldOCISvcCtx;

    if not FCommand.NativeCursor then
      if FCommand.FCursorRef <> OldCursor then
        FCommand.SetCursor(OldCursor);

    if DescribeExecute then
      for i := 0 to FCommand.Params.Count - 1 do
        if FCommand.GetParam(i).DataType = dtCursor then
          FCommand.GetParam(i).GetAsCursor.FreeCursor;

    if (OldCursorState = csInactive) and (FCommand.GetCursorState <> csInactive) then
      FCommand.Close
    else
      FCommand.SetCursorState(OldCursorState);
  end;
end;

procedure TOCIRecordSet.FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean);
var
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  inherited FillDataFieldDescs(DataFieldDescs, ForceUseAllKeyFields);

  for i := 0 to FExpandedFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FExpandedFields[i]);
    if (FieldDesc <> nil) and (FieldDesc.ParentField = nil) and not FieldDesc.ReadOnly then begin
      SetLength(DataFieldDescs, Length(DataFieldDescs) + 1);
      DataFieldDescs[High(DataFieldDescs)] := FieldDesc;
    end;
  end;
end;

function TOCIRecordSet.GetFieldDesc(FieldNo: integer; var Field: TOCIFieldDesc;
  LongString: boolean; FlatBuffer: boolean): boolean;
begin
  if FCommand.FOCICallStyle = OCI73 then begin
    Result := GetFieldDesc7(FieldNo, Field, LongString, FlatBuffer);
  end
  else
  if FCommand.FOCICallStyle = OCI80 then begin
    Result := GetFieldDesc8(FieldNo, Field, LongString, FlatBuffer);
  end
  else begin
    Result := False;
    OCISvcCtx.Home.CheckOCI;
  end;
end;

function TOCIRecordSet.GetFieldDesc7(FieldNo: integer; var Field: TOCIFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
const
  MaxFieldNameSize = 50;
var
  dbsize: sb4;
  dbtype: sb2;
  cbuf: AnsiString;
  Ptr: IntPtr;
  cbufl: sb4;
  dsize: sb4;
  prec: sb2;
  scale: sb2;
  nullok: sb2;
  Res: sword;
  Len: Integer;
  Str: string;
begin
  cbufl := MaxFieldNameSize;

  Ptr := Marshal.AllocHGlobal(MaxFieldNameSize + 1);
  try
    Res := OCI7.odescr(FCommand.FCursorRef.CDA, FieldNo, dbsize, dbtype, Ptr,
      cbufl, dsize, prec, scale, nullok);
    if Res = 0 then
      cbuf := Marshal.PtrToStringAnsi(Ptr, cbufl);
  finally
    Marshal.FreeHGlobal(Ptr);
  end;

  if Res = 0 then begin
    Field.Name := string(cbuf);
    Field.ActualFieldNo := FieldNo;
    Field.Length := 0;
    Field.Scale := 0;
    Field.SubDataType := dtUnknown;

    case dbtype of
      VARCHAR2_TYPE, CHAR_TYPE: begin
        if (dbsize <= 255) or LongString then begin  // for IDSBase compatibility
          if (dbsize >= FlatBufferLimit) and not FlatBuffer then begin
            Field.DataType := dtExtString;
            Field.Size := SizeOf(Word) {Size} + sizeof(IntPtr);
          end
          else begin
            Field.DataType := dtString;
            Field.Size := SizeOf(Word) {Size} + dbsize + 1;  // for terminator
          end;
        end
        else begin
          Field.DataType := dtMemo;
          Field.SubDataType := dtString;
          Field.Size := sizeof(IntPtr);
        end;
        if dbtype = CHAR_TYPE then
          Field.SubDataType := dtFixedChar;
        Field.Length := dbsize; // !is used to differ from Memo
        Field.Fixed := dbtype = CHAR_TYPE;
      end;
      NUMBER_TYPE: begin
        if prec = 0 then begin// NUMBER without Prec and Scale
          prec := 39;
          if scale <= 0 then
            scale := 39;
          Field.SubDataType := dtNumberFloating;
        end
        else
          Field.SubDataType := dtNumber;

        Field.Length := prec;
        Field.Scale := Abs(scale);

        if FCommand.FFieldsAsString or FCommand.FNumberAsString then begin
          Field.DataType := dtString;
          Field.Size := SizeOf(Word) {Size} + Field.Length + 3;
        end
        else if FConnection.FEnableIntegers and (scale = 0) and (prec <= FCommand.GetIntegerPrecision) then begin
          Field.DataType := dtInteger;
          Field.Size := sizeof(Integer);
        end
        else begin
          Field.DataType := dtFloat;
          Field.Size := sizeof(Double);
        end;
      end;
      DATE_TYPE: begin
        if FCommand.FFieldsAsString then begin
          Str := UpperCase(FConnection.FNlsParams[nlsDateFormat].Value);
          Len := Length(Str);
          if Pos('MONTH', Str) > 0 then
            Len := Len + 7;
          if Pos('DAY', Str) > 0 then
            Len := Len + 9;
          Field.DataType := dtString;
          Field.Size := SizeOf(Word) {Size} + Len + 1;
          Field.Length := Len;
        end
        else begin
          Field.DataType := dtDateTime;
          Field.Size := 7;
        end;
      end;
      ROWID_TYPE: begin
        Field.DataType := dtString; //dtRowId;  //WAR
        Field.SubDataType := dtRowId;
        Field.Size := SizeOf(Word) {Size} + RowIdSize + 1; // for terminator //dbsize;
        Field.Length := RowIdSize;
      end;
      RAW_TYPE:
        if FCommand.FFieldsAsString or FCommand.FRawAsString then begin
          Field.DataType := dtString;
          Field.Size := SizeOf(Word) {Size} + dbsize * 2 + 1;  // for terminator and heximal represent
          Field.Length := dbsize * 2;
        end
        else begin
          Field.DataType := dtVarBytes;
          Field.Size := dbsize + SizeOf(Word);
          Field.Length := dbsize;
        end;
      LONG_TYPE: begin
        Field.DataType := dtMemo;
        Field.Size := sizeof(IntPtr);
      end;
      LONGRAW_TYPE: begin
        Field.DataType := dtBlob;
        Field.Size := sizeof(IntPtr);
      end;
      SQLT_OSL: begin
        Field.DataType := dtLabel;
        Field.Size := dbSize;
      end;
    else
      RaiseError(SDataTypeNotSupported);
      Field.DataType := dtUnknown;
    end;

    Field.Required := nullok = 0;
    Result := True;
  end
  else begin
    if Res <> OCI_VAR_NOT_IN_LIST then
      Check(Res);
    Result := False;
  end;
end;

function TOCIRecordSet.GetFieldDesc8(FieldNo: integer; var Field: TOCIFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;

  function GetNlsFieldDesc(NlsType: TNlsParamType): Integer;
  var
    Str: string;
  begin
    if FConnection.FNlsParams[NlsType].Value = '' then
      FConnection.GetSessionParameters;

    Str := UpperCase(FConnection.FNlsParams[NlsType].Value);

    Result := Length(Str);
    if Pos('MONTH', Str) > 0 then
      Result := Result + 7
    else if Pos('MON', Str) > 0 then
      Result := Result + 3;
    if Pos('DAY', Str) > 0 then
      Result := Result + 9
    else if Pos('DY', Str) > 0 then
      Result := Result + 3;
    if Pos('DDSPTH', Str) > 0 then
      Result := Result + 14;
    if (NlsType in [nlsTimeStampFormat, nlsTimeStampTZFormat]) and (Pos('SSXFF', Str) > 0) then
      Result := Result + 4;
  end;

var
  hParam: pOCIParam;
  Len: integer;
  Name: string;
  Res: sword;
  OraType: sb2;
  IsNull: ub1;
  TypeName: string;
  SchemaName: string;
  Count: integer;
  ValueInt: Integer;
  Ptr, ValuePtr, StrPtr: IntPtr;
  NationalCharset : boolean;
  CharUsed: boolean;
  DataType: Word;
  SubDataType: Word;
  Length: Integer;
  Scale: Integer;
  Fixed: boolean;
begin
  Result := False;

  Ptr := Marshal.AllocHGlobal(sizeof(integer));
  try
    Check(OCI8.OCIAttrGet2(FCommand.FCursorRef.OCIStmt, OCI_HTYPE_STMT, ValueInt, nil,
      OCI_ATTR_PARAM_COUNT, OCISvcCtx.hOCIError));
    Count := Integer(ValueInt);

    if FieldNo > Count then
      Exit;

    hParam := nil;
    Res := OCI8.OCIParamGet(FCommand.FCursorRef.OCIStmt, OCI_HTYPE_STMT, OCISvcCtx.hOCIError, hParam, FieldNo);

    (*if hParam = nil {Res = OCI_NO_DATA} then  //WAR  ORACLE BAG
      Exit;*)

    try
      Check(Res);

      StrPtr := nil;
      ValuePtr := @StrPtr;
      Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_NAME, OCISvcCtx.hOCIError));

      Len := Marshal.ReadInt32(Ptr);
      Name := PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv);

      Field.Name := Name;
      Field.ActualFieldNo := FieldNo;
      Field.SubDataType := dtUnknown;

      // Oracle Type
      Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_TYPE, OCISvcCtx.hOCIError));
      OraType := sb2(ValueInt);

      // National
      if OraType in [SQLT_CHR, SQLT_AFC, SQLT_CLOB] then begin
        Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, OCISvcCtx.hOCIError));
        NationalCharset := ub1(ValueInt) = SQLCS_NCHAR;
      end
      else
        NationalCharset := False;

      // CharUsed
      CharUsed := False;

      // Length, Precision and Scale
      case OraType of
        SQLT_CHR, SQLT_AFC:  begin
          if OCISvcCtx.Home.OCIVersion >= 9000 then begin
            Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHAR_SIZE, OCISvcCtx.hOCIError));
            Field.DBLength := sb2(ValueInt);
            if Field.DBLength > 0 then
              CharUsed := True
          end;

          if not CharUsed then begin
            Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_SIZE, OCISvcCtx.hOCIError));
            Field.DBLength := ValueInt;
          end;
        end;
        SQLT_NUM: begin
          Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PRECISION, OCISvcCtx.hOCIError));
          Field.DBLength := sb2(ValueInt);

          Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_SCALE, OCISvcCtx.hOCIError));
          Field.DBScale := sb1(ValueInt);

          if (Field.DBScale = -127) and (Field.DBLength > 0) then begin // FLOAT
            Field.DBLength := Ceil(Field.DBLength * 0.30103);
            OraType := SQLT_FLT;
          end
          else if Field.DBLength = 0 then begin // NUMBER without Prec and Scale
            Field.DBLength := 39;
            if Field.DBScale <= 0 then
              Field.DBScale := 39;
          end;

          Field.DBScale := Abs(Field.DBScale);
        end;
        SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ: begin
          Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_FSPRECISION, OCISvcCtx.hOCIError));
          Field.DBScale := sb2(ValueInt);
        end;
        SQLT_INTERVAL_YM, SQLT_INTERVAL_DS: begin
          Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PRECISION, OCISvcCtx.hOCIError));
          Field.DBLength := sb2(ValueInt);

          if OraType = SQLT_INTERVAL_DS then begin
            Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_SCALE, OCISvcCtx.hOCIError));
            Field.DBScale := sb1(ValueInt);
          end
          else
            Field.DBScale := 0;
        end;
        else begin
          Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_SIZE, OCISvcCtx.hOCIError));
          Field.DBLength := ValueInt;
        end;
      end;

      // Type Name for Oracle objects and references
      if OraType in [SQLT_NTY, SQLT_REF] then begin
        StrPtr := nil;
        ValuePtr := @StrPtr;
        Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_TYPE_NAME, OCISvcCtx.hOCIError));

        Len := Marshal.ReadInt32(Ptr);
        TypeName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

        StrPtr := nil;
        ValuePtr := @StrPtr;
        Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_SCHEMA_NAME, OCISvcCtx.hOCIError));

        Len := Marshal.ReadInt32(Ptr);
        SchemaName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

        if ObjectTypes <> nil then
          Field.ObjectType := ObjectTypes.FindType(OCISvcCtx, SchemaName + '.' + TypeName)
        else
          Field.ObjectType := nil;

        if Field.ObjectType = nil then begin
          Field.ObjectType := TOraType.Create(OCISvcCtx, SchemaName + '.' + TypeName);
          Field.ObjectType.Release;
        end;
      end;

      if Field.ObjectType <> nil then
        Field.DBType := TOraConverterManager.GetDBType(OraType, TOraType(Field.ObjectType).DataType, NationalCharset)
      else
        Field.DBType := TOraConverterManager.GetDBType(OraType, 0, NationalCharset);

      DetectFieldType(Field.Name, Field.DBType, Field.DBLength, Field.DBScale, Field.ObjectType, CharUsed,
        DataType, SubDataType, Length, Scale, Fixed);

      Field.DataType := DataType;
      Field.SubDataType := SubDataType;
      Field.Length := Length;
      Field.Scale := Scale;
      Field.Fixed := Fixed;

      Field.Size := GetBufferSize(Field.DataType, Field.Length);

      Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_IS_NULL, OCISvcCtx.hOCIError));
      IsNull := Byte(ValueInt);

    finally
      // free memory after OCIParamGet
      OCI8.OCIDescriptorFree(hParam, OCI_DTYPE_PARAM);
    end;

  finally
    Marshal.FreeHGlobal(Ptr);
  end;

  Field.Required := (IsNull = 0) and (Field.Name <> 'ROWID');
  Result := True;
end;

class procedure TOCIRecordSet.GetDateFromBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat);
var
  DateTime: TDateTime;
begin
  if HasParent then begin
    inherited;
    exit;
  end;

  case Format of
    dfMSecs: begin
      try
        Marshal.WriteInt64(Date, BitConverter.DoubleToInt64Bits(OraDateToMSecs(Buf)));
      except
        on E: EConvertError do
          // 1 day - to prevent error on converting to TDateTime
          Marshal.WriteInt64(Date, BitConverter.DoubleToInt64Bits(MSecsPerDay));
      end;
    end;
    dfDateTime: begin
      try
        DateTime := OraDateToDateTime(Buf);
        Marshal.WriteInt64(Date, BitConverter.DoubleToInt64Bits(DateTime));
      except
        on E: EConvertError do
          Marshal.WriteInt64(Date, 0);
      end;
    end;
    dfDate: begin
      try
        DateTime := OraDateToDateTime(Buf);
        Marshal.WriteInt32(Date, DateTimeToTimeStamp(DateTime).Date);
      except
        on E: EConvertError do
          Marshal.WriteInt32(Date, DateDelta);
      end;
    end;
    dfTime: begin
      try
        DateTime := OraDateToDateTime(Buf);
        Marshal.WriteInt32(Date, DateTimeToTimeStamp(DateTime).Time);
      except
        on E: EConvertError do
          Marshal.WriteInt32(Date, 0);
      end;
    end;
  end;
end;

class procedure TOCIRecordSet.PutDateToBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat);
var
  Ts: TTimeStamp;
begin
  if HasParent then begin
    inherited;
    exit;
  end;

  case Format of
    dfMSecs:
      MSecsToOraDate(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Date)), Buf);
    dfDateTime:
      DateTimeToOraDate(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Date)), Buf);

    dfDate: begin
      Ts.Date := Marshal.ReadInt32(Date);
      Ts.Time := 0;
      DateTimeToOraDate(MemUtils.TimeStampToDateTime(Ts), Buf);
    end;
    dfTime: begin
      Ts.Date := DateDelta;
      Ts.Time := Marshal.ReadInt32(Date);
      DateTimeToOraDate(MemUtils.TimeStampToDateTime(Ts), Buf);
    end;
  end;
end;

function TOCIRecordSet.GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
begin
{$IFNDEF LITE}
  CheckFetched(RecBuf, Field);
{$ENDIF}

  if not Field.HasParent then
    case Field.DataType of
      dtObject, dtXML, dtAnyData, dtReference, dtArray, dtTable:
        // indicator in record buffer is not synchronized with object state
        Result := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset))).IsNull;
      dtNumber:
        Result := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset))).IsNull;
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
        Result := TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset))).IsNull;
      dtIntervalYM, dtIntervalDS:
        Result := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset))).IsNull;
    else
      if Field.FieldDescKind <> fdkCalculated then
        Result := Marshal.ReadInt16(RecBuf, FDataSize + (Field.FieldNo - 1) * sizeof(sb2)) = -1
      else
        Result := Marshal.ReadInt16(RecBuf, FRecordSize + FCalcDataSize + (Field.ActualFieldNo - 1) * sizeof(sb2)) = -1;
    end
  else
    Result := GetChildFieldIsNull(Field, RecBuf);

  if Result then
    Result := GetNullByBlob(Field, RecBuf);
end;

procedure TOCIRecordSet.SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
var
  Blob: TBlob;
  Ind: OCIInd;
  OraTimeStamp: TOraTimeStamp;
  OraInterval: TOraInterval;
  OraNumber: TOraNumber;
  OraObject: TOraObject;
  ObjPtr: IntPtr;
begin
  if not Field.HasParent then begin
    if Value then
      Ind := -1
    else
      Ind := 0;

    if Field.FieldDescKind <> fdkCalculated then
      Marshal.WriteInt16(RecBuf, FDataSize + (Field.FieldNo - 1) * SizeOf(sb2), Ind)
    else
      Marshal.WriteInt16(RecBuf, FRecordSize + FCalcDataSize + (Field.ActualFieldNo - 1) * SizeOf(sb2), Ind);

    if Value and Field.IsBlob then begin // clear Blob value
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
      if Blob <> nil then
        Blob.Clear;
    end;

    case Field.DataType of
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
        OraTimeStamp := TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
        if OraTimeStamp <> nil then
          OraTimeStamp.FIndicatorPtr^ := Ind;
      end;
      dtIntervalYM, dtIntervalDS: begin
        OraInterval := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
        if OraInterval <> nil then
          OraInterval.FIndicatorPtr^ := Ind;
      end;
      dtNumber: begin
        ObjPtr := Marshal.ReadIntPtr(RecBuf, Field.DataOffset);
        OraNumber := TOraNumber(GetGCHandleTarget(ObjPtr));
        if OraNumber <> nil then
          OraNumber.FIndicatorPtr^ := Ind;
      end;
      dtObject, dtArray, dtTable: begin
        OraObject := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
        if OraObject <> nil then
          OraObject.IsNull := Value;
      end;
    end;
  end
  else
    PutChildField(Field, RecBuf, nil, 0);

{$IFNDEF LITE}
  SetEncrypted(Field, RecBuf, not Value);
{$ENDIF}
end;

class function TOCIRecordSet.IsBlobDataType(DataType: word): boolean;
begin
  Result := DataType in [dtBlob, dtMemo, dtWideMemo,
                         dtOraBlob, dtOraClob, dtWideOraClob, dtNClob,
                         dtBFile, dtCFile];
end;

class function TOCIRecordSet.IsObjectDataType(DataType: word): boolean;
begin
  Result := DataType in [dtObject, dtXML, dtAnyData, dtReference, dtArray, dtTable];
end;

class function TOCIRecordSet.IsSharedObjectDataType(DataType: word): boolean;
begin
  Result := IsBlobDataType(DataType) or IsObjectDataType(DataType) or
            (DataType in [dtCursor, dtNumber,
                          dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
                          dtIntervalYM, dtIntervalDS]);
end;

class function TOCIRecordSet.IsComplexDataType(DataType: word): boolean;
begin
  case DataType of
    dtExtString, dtExtWideString, dtExtVarBytes:
      Result := True;
  else
    Result := IsSharedObjectDataType(DataType);
  end;
end;

class function TOCIRecordSet.IsConvertedFieldType(DataType: word): boolean;
begin
  case DataType of
    dtInt64, dtUInt64, dtBCD, dtFMTBCD, dtSQLTimeStamp:
      Result := True;
  else
    Result := False;
  end;
end;

{ Records }

procedure TOCIRecordSet.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  DataBuf: IntPtr;
  Lob: TOraLob;
{$IFNDEF ONLY_UTF8_MEMO}
  Blob: TBlob;
{$ENDIF}
  BFile: TOraFile;
  Cursor: TOraCursor;
  OraObject: TOraObject;
  OraRef: TOraRef;
  OraXml: TOraXml;
  OraAnyData: TOraAnyData;
  OraArray: TOraArray;
  OraNestTable: TOraNestTable;
  OraTimeStamp: TOraTimeStamp;
  OraInterval: TOraInterval;
begin
  DataBuf := PtrOffset(RecBuf, Field.DataOffset);
  case Field.DataType of
    dtBlob, dtMemo, dtWideMemo: begin
      inherited;

    {$IFNDEF ONLY_UTF8_MEMO}
      if (FCommand.FOCICallStyle = OCI80) and
         (Field.SubDataType in [dtString, dtNString, dtWideString, dtNWideString,
                                dtFixedChar, dtFixedNChar, dtFixedWideChar, dtFixedNWideChar])
      then begin
        Blob := TBlob(GetGCHandleTarget(PIntPtr(DataBuf)^));
        Blob.IsUnicode := UseUnicode;
      end;
    {$ENDIF}
    end;
    dtOraBlob, dtOraClob, dtWideOraClob: begin
      Lob := TOraLob.Create(OCISvcCtx);
      if Field.DataType <> dtOraBlob then begin
        if Field.SubDataType = dtNClob then
          Lob.LobType := ltNClob
        else
          Lob.LobType := ltClob;
      {$IFNDEF ONLY_UTF8_MEMO}
        Lob.IsUnicode := (Field.DataType in [dtWideMemo, dtWideOraClob]) or UseUnicode;
      {$ENDIF}
      {$IFDEF IS_UTF8_EXCLUDE_MEMO}
        Lob.FCharsetId := 0;
      {$ELSE}
        Lob.FCharsetId := FConnection.FCharsetId;
      {$ENDIF}
        Lob.FCharLength := CharLength;
      end
      else
        Lob.LobType := ltBlob;
      Lob.AllocLob;
      Lob.Cached := FCommand.FCacheLobs;
      Lob.EnableRollback;
      Marshal.WriteIntPtr(DataBuf, Lob.GCHandle);
    end;
    dtBFile, dtCFile: begin
      if (FConnection <> nil) and FConnection.GetConnected then
        BFile := TOraFile.Create(OCISvcCtx)
      else
        BFile := TOraFile.Create(OCISvcCtx);
      BFile.AllocLob;
      BFile.Cached := FCommand.FCacheLobs;
      BFile.EnableRollback;
      Marshal.WriteIntPtr(DataBuf, BFile.GCHandle);
    end;
    dtCursor: begin
      Cursor := TOraCursor.Create;
      Cursor.Init(OCISvcCtx, FCommand.OCICallStyle);
      Marshal.WriteIntPtr(DataBuf, Cursor.GCHandle);
    end;
    dtObject: begin
      if (FConnection = nil) or not FConnection.GetConnected then
        RaiseError('Unsupported Disconnected mode type');
      OraObject := TOraObject.Create(TOraType(Field.ObjectType));
      OraObject.NativeInstance := True;
      Marshal.WriteIntPtr(DataBuf, OraObject.GCHandle);
    end;
    dtReference: begin
      if (FConnection = nil) or not FConnection.GetConnected then
        RaiseError('Unsupported Disconnected mode type');
      OraRef := TOraRef.Create(TOraType(Field.ObjectType));
      Marshal.WriteIntPtr(DataBuf, OraRef.GCHandle);
    end;
    dtXML: begin
      if (FConnection = nil) or not FConnection.GetConnected then
        RaiseError('Unsupported Disconnected mode type');
      OraXml := TOraXML.Create(OCISvcCtx, TOraType(Field.ObjectType));
      if ((OCISvcCtx.Home.OCIVersion > 9203) and (OCISvcCtx.Home.OCIVersion < 9206)) then    //Bug with memory
        OraXml.AllocObject;            //leak with 9204 client
      OraXml.NativeInstance := True;
      Marshal.WriteIntPtr(DataBuf, OraXml.GCHandle);
    end;
    dtAnyData: begin
      if (FConnection = nil) or not FConnection.GetConnected then
        RaiseError('Unsupported Disconnected mode type');
      OraAnyData := TOraAnyData.Create(OCISvcCtx, TOraType(Field.ObjectType));
      OraAnyData.NativeInstance := True;
      Marshal.WriteIntPtr(DataBuf, OraAnyData.GCHandle);
    end;
    dtArray: begin
      if (FConnection = nil) or not FConnection.GetConnected then
        RaiseError('Unsupported Disconnected mode type');
      OraArray := TOraArray.Create(TOraType(Field.ObjectType));
      OraArray.AllocObject(OCISvcCtx); // ??? Optim?
      Marshal.WriteIntPtr(DataBuf, OraArray.GCHandle);
    end;
    dtTable: begin
      if (FConnection = nil) or not FConnection.GetConnected then
        RaiseError('Unsupported Disconnected mode type');
      OraNestTable := TOraNestTable.Create(TOraType(Field.ObjectType));
      OraNestTable.AllocObject(OCISvcCtx); // ??? Optim?
      Marshal.WriteIntPtr(DataBuf, OraNestTable.GCHandle);
    end;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
      OraTimeStamp := TOraTimeStamp.Create(OCISvcCtx, Field.DataType);
      OraTimeStamp.Precision := Field.Scale;
      Marshal.WriteIntPtr(DataBuf, OraTimeStamp.GCHandle);
    end;
    dtIntervalYM, dtIntervalDS: begin
      OraInterval := TOraInterval.Create(OCISvcCtx, Field.DataType);
      OraInterval.LeadPrecision := Field.Length;
      OraInterval.FracPrecision := Field.Scale;
      Marshal.WriteIntPtr(DataBuf, OraInterval.GCHandle);
    end;
    dtNumber:
      Marshal.WriteIntPtr(DataBuf, TOraNumber.Create(OCISvcCtx).GCHandle);
    else
      inherited;
  end;
end;

procedure TOCIRecordSet.FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  ObjPtr: IntPtr;
  SharedObject: TSharedObject;
begin
  case Field.DataType of
    dtOraBlob, dtOraClob, dtWideOraClob,
    dtBFile, dtCFile,
    dtCursor,
    dtObject, dtReference, dtXML, dtAnyData, dtArray, dtTable,
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS,
    dtNumber: begin
      ObjPtr := Marshal.ReadIntPtr(RecBuf, Field.DataOffset);
      if ObjPtr <> nil then begin
        SharedObject := TSharedObject(GetGCHandleTarget(ObjPtr));
        if (SharedObject <> nil) and (SharedObject.RefCount = 1) then begin
          SharedObject.Free;
          Marshal.WriteIntPtr(RecBuf, Field.DataOffset, nil);
        end
        else
          SharedObject.Free;
      end;
    end;
  else
    inherited;
  end;
end;

procedure TOCIRecordSet.CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc);
var
  ValueOffset: Integer;
  SrcPtr: IntPtr;
  DestPtr: IntPtr;
  SrcObject: TObject;
  DestObject: TObject;
begin
  ValueOffset := Field.DataOffset;

  case Field.DataType of
    dtObject, dtReference, dtXML, dtAnyData, dtArray, dtTable: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      SrcObject := GetGCHandleTarget(SrcPtr);
      DestObject := GetGCHandleTarget(DestPtr);
      TOraObject(DestObject).Assign(TOraObject(SrcObject));
      if DisconnectedMode then
        TOraObject(DestObject).CacheObject;
    end;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      SrcObject := GetGCHandleTarget(SrcPtr);
      DestObject := GetGCHandleTarget(DestPtr);
      TOraTimeStamp(SrcObject).AssignTo(TOraTimeStamp(DestObject));
    end;
    dtIntervalYM, dtIntervalDS: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      SrcObject := GetGCHandleTarget(SrcPtr);
      DestObject := GetGCHandleTarget(DestPtr);
      TOraInterval(SrcObject).AssignTo(TOraInterval(DestObject));
    end;
    dtNumber: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      SrcObject := GetGCHandleTarget(SrcPtr);
      DestObject := GetGCHandleTarget(DestPtr);
      TOraNumber(SrcObject).AssignTo(TOraNumber(DestObject));
    end
  else
    inherited;
  end;
end;

{$IFNDEF LITE}

procedure TOCIRecordSet.RequestFieldsInfo(Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo);

  function AddDBLink(DBLink: string): string;
  begin
    if DBLink <> '' then
      Result := '@' + DBLink;
  end;

  function Locate(Query: TOCIRecordSet; RecBuf: IntPtr; const FieldNames: array of string; const Values: array of string): boolean;
  var
    i: Integer;
    v: variant;
    Fields: TList;
  begin
    Result := False;

    Fields := TList.Create;
    try
      for i := 0 to Length(FieldNames) - 1 do
        Fields.Add(Query.FieldByName(FieldNames[i]));

      Query.SetToBegin;
      while True do begin
        Query.GetNextRecord(RecBuf);
        if Query.Eof then
          break;

        Result := True;
        for i := 0 to Fields.Count - 1 do begin
          Query.GetFieldAsVariant(Fields[i], RecBuf, v);
          if not SameText(VarToStr(v), Values[i]) then begin
             Result := False;
            break;
          end;
        end;

        if Result then
          Exit;
      end;

    finally
      Fields.Free;
    end;
  end;

  function GetDefExpr(Query: TOCIRecordSet; RecBuf: IntPtr): string;
  var
    v: Variant;
    i: integer;
    Value: string;
    DataType: string;
    DeqValue: string;
  begin
    Query.GetFieldAsVariant(Query.Fields[3], RecBuf, v);
    Value := VarToStr(v);

    if DefaultExpressionOldBehavior then begin
      Result := TrimRight(Value);
      DeqValue := AnsiDequotedStr(Result, '''');
      if (Result <> DeqValue) and (AnsiQuotedStr(DeqValue, '''') = Result) then // '1' + '2' should not be dequoted
        Result := DeqValue;

      Query.GetFieldAsVariant(Query.Fields[4], RecBuf, v);
      DataType := VarToStr(v);

      if (DataType = 'NUMBER') and ({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.') then begin
        i := Pos('.', Result);
        if i > 0 then
          Result[i] := Char({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
      end;
    end
    else
      Result := VarToStr(Value);
  end;

  procedure GetFieldsInfoServer(Query: TOCIRecordSet; var NeedDescribeSynonyms: boolean);
  var
    v: variant;
    RecBuf: IntPtr;
    SQL, Filter: string;
    CurTableName, FieldTableName, CurOwner, FieldOwner, DBLink: string;
    j, p: integer;
    ColumnInfo, NewColumnInfo: TCRColumnInfo;
    Located, MoreDBLinks: boolean;
  begin
    NeedDescribeSynonyms := False;
    for j := 0 to High(Tables) do
      Tables[j].Flag := Tables[j].Flag and not 2; // 2 - info is queried
    repeat
      Filter := '';
      MoreDBLinks := False;
      for j := 0 to High(Tables) do
        if (Tables[j].Flag and 1 <> 0) and (Tables[j].Flag and 2 = 0) then begin
          if Filter = '' then
            DBLink := Tables[j].DBLink
          else begin
            if Tables[j].DBLink <> DBLink then begin
              MoreDBLinks := True;
              continue;
            end;
            Tables[j].Flag := Tables[j].Flag or 2;
            Filter := Filter + ' OR';
          end;
          Filter := Filter + ' (table_name = ''' + Tables[j].Name + '''' +
            ' and owner = ''' + Tables[j].Schema + ''')';
        end;

      if Filter = '' then
        exit;

      SQL := 'SELECT owner, table_name, column_name';
      if FDefaultValues then
        SQL := SQL + ', data_default, data_type';
      SQL := SQL + ' FROM sys.all_tab_columns' + AddDBLink(DBLink) +
        ' WHERE ' + Filter +
        ' ORDER BY owner, table_name, column_id';

      Query.Close;
      Query.SetSQL(SQL);
      try
        Query.Open;
      except
        //Probably no DBLink
      end;

      Query.AllocRecBuf(RecBuf);
      try
        p := 0;
        while p < Columns.Count do begin
          ColumnInfo := Columns[p];
          if (ColumnInfo.TableIndex = -1) and (ColumnInfo.Table <> '') and
            not FUpdTableIsArtificial or
            (FFieldOrigins = foNone) and (ColumnInfo.TableIndex <> -1)
            and (ColumnInfo.TableIndex <> FUpdatingTableInfoIdx)
          then
            ColumnInfo.Described := True;

          if ColumnInfo.Described then begin
            Inc(p);
            continue;
          end;

          if ColumnInfo.Name = '*' then begin
            if ColumnInfo.TableIndex <> -1 then begin
              FieldTableName := Tables[ColumnInfo.TableIndex].Name;
              FieldOwner := Tables[ColumnInfo.TableIndex].Schema;
            end;
            NewColumnInfo := nil;
            Query.SetToBegin;
            while True do begin
              Query.GetNextRecord(RecBuf);
              if Query.Eof then
                break;

              Query.GetFieldAsVariant(Query.Fields[0], RecBuf, v);
              CurOwner := VarToStr(v);

              Query.GetFieldAsVariant(Query.Fields[1], RecBuf, v);
              CurTableName := VarToStr(v);

              if (ColumnInfo.TableIndex = -1) or
                (CurOwner = FieldOwner) and (CurTableName = FieldTableName)
              then begin
                Query.GetFieldAsVariant(Query.Fields[2], RecBuf, v);

                NewColumnInfo := TCRColumnInfo.Create;
                try
                  NewColumnInfo.Used := False;
                  NewColumnInfo.Described := True;
                  NewColumnInfo.Table := ColumnInfo.Table;
                  NewColumnInfo.TableIndex := ColumnInfo.TableIndex;
                  NewColumnInfo.Name := VarToStr(v);
                  NewColumnInfo.Alias := NewColumnInfo.Name;
                except
                  NewColumnInfo.Free;
                  raise;
                end;

                Inc(p);
                Columns.Insert(p, NewColumnInfo);

                if NewColumnInfo.TableIndex = -1 then begin
                  for j := 0 to High(Tables) do
                    if (Tables[j].Schema = CurOwner) and (Tables[j].Name = CurTableName) then begin
                      NewColumnInfo.TableIndex := j;
                      break;
                    end;
                end;
                if NewColumnInfo.TableIndex <> -1 then
                  Tables[NewColumnInfo.TableIndex].Flag := 0;

                if FDefaultValues and (NewColumnInfo.TableIndex = FUpdatingTableInfoIdx) then
                  NewColumnInfo.Expr := GetDefExpr(Query, RecBuf);
              end
              else
                if (NewColumnInfo <> nil) and (ColumnInfo.TableIndex <> -1) then
                  break;
            end;
            if (NewColumnInfo <> nil) and (ColumnInfo.TableIndex <> -1) then
              ColumnInfo.Described := True;
          end
          else
            if (ColumnInfo.Name <> '') and (ColumnInfo.Name <> 'ROWID') then begin
              if ColumnInfo.TableIndex <> -1 then begin
                if FDefaultValues and (ColumnInfo.TableIndex = FUpdatingTableInfoIdx) then
                  Located := Locate(Query, RecBuf, ['owner', 'table_name', 'column_name'],
                                    [Tables[ColumnInfo.TableIndex].Schema, Tables[ColumnInfo.TableIndex].Name, ColumnInfo.Name])
                else begin
                  ColumnInfo.Described := True;
                  Located := False;
                end;
              end
              else
                Located := Locate(Query, RecBuf, ['column_name'], [ColumnInfo.Name]);

              if Located then begin
                ColumnInfo.Described := True;
                if FDefaultValues then
                  ColumnInfo.Expr := GetDefExpr(Query, RecBuf);

                if ColumnInfo.TableIndex = -1 then begin
                  Query.GetFieldAsVariant(Query.Fields[0], RecBuf, v);
                  CurOwner := VarToStr(v);

                  Query.GetFieldAsVariant(Query.Fields[1], RecBuf, v);
                  CurTableName := VarToStr(v);

                  for j := 0 to High(Tables) do
                    if (Tables[j].Schema = CurOwner) and (Tables[j].Name = CurTableName) then begin
                      ColumnInfo.TableIndex := j;
                      break;
                    end;
                end;
                if ColumnInfo.TableIndex <> -1 then
                  Tables[ColumnInfo.TableIndex].Flag := 0;
              end;
            end
            else
              ColumnInfo.Described := True;

          inc(p);
        end;

        for j := 0 to High(Tables) do
          if (Tables[j].Flag and 1 <> 0) and (Tables[j].DBLink = DBLink) then begin
            if Locate(Query, RecBuf, ['owner', 'table_name'], [Tables[j].Schema, Tables[j].Name]) then
              Tables[j].Flag := 0
            else
              NeedDescribeSynonyms := True;
          end;

        Query.Close;
      finally
        Query.FreeRecBuf(RecBuf);
      end;
    until not MoreDBLinks;

    if NeedDescribeSynonyms then begin
      NeedDescribeSynonyms := False;
      for j := 0 to Columns.Count - 1 do
        if not Columns[j].Described then begin
          NeedDescribeSynonyms := True;
          break;
        end;
    end;
  end;

  procedure GetDBLinkSchema(Query: TOCIRecordSet);
  var
    i: integer;
    v: variant;
    RecBuf: IntPtr;
    SQL: string;
  begin
    SQL := '';

    for i := 0 to High(Tables) do
      if (Tables[i].Flag and 1 <> 0) and
        (Tables[i].DBLink <> '') and (Tables[i].Schema = '')
      then
        SQL := SQL + ' AND db_link = ''' + Tables[i].DBLink + '''';

    if SQL = '' then
      Exit;

    SQL := 'SELECT username, db_link FROM sys.all_db_links' +
           ' WHERE owner IN (''' + FConnection.GetCachedSchema + ''', ''PUBLIC'') ' +
           SQL +
           ' ORDER BY decode(owner, ''PUBLIC'', 1, 0) ';

    Query.Close;
    Query.SetSQL(SQL);
    Query.Open;

    Query.AllocRecBuf(RecBuf);
    try
      for i := 0 to High(Tables) do
        if (Tables[i].Flag and 1 <> 0) and
          (Tables[i].DBLink <> '') and (Tables[i].Schema = '')
        then
          if Locate(Query, RecBuf, ['db_link'], [Tables[i].DBLink]) then begin
            Query.GetFieldAsVariant(Query.Fields[0], RecBuf, v);
            Tables[i].Schema := VarToStr(v);
          end;

      Query.Close;
    finally
      Query.FreeRecBuf(RecBuf);
    end;

    for i := 0 to High(Tables) do
      if (Tables[i].Flag and 1 <> 0) and
        (Tables[i].DBLink <> '') and
        ((Tables[i].Schema = '') or (Tables[i].Schema = 'CURRENT_USER'))
      then
        Tables[i].Schema := FConnection.GetCachedUser;
  end;

  procedure DescribeSynonyms(Query: TOCIRecordSet);
  var
    j: integer;
    v: Variant;
    RecBuf: IntPtr;
    SQL, Filter, DBLink: string;
    MoreDBLinks: boolean;
  begin
    for j := 0 to High(Tables) do
      Tables[j].Flag := Tables[j].Flag and not 2;
    repeat
      Filter := '';
      MoreDBLinks := False;
      for j := 0 to High(Tables) do
        if (Tables[j].Flag and 1 <> 0) and (Tables[j].Flag and 2 = 0) then begin
          if Filter = '' then
            DBLink := Tables[j].DBLink
          else begin
            if Tables[j].DBLink <> DBLink then begin
              MoreDBLinks := True;
              continue;
            end;
            Tables[j].Flag := Tables[j].Flag or 2;
            Filter := Filter + ' OR';
          end;
          Filter := Filter + ' (synonym_name = ''' + Tables[j].Name + '''';
          if Tables[j].Flag and 8 = 0 then
            Filter := Filter + ' AND owner IN (''' + Tables[j].Schema + ''',''PUBLIC'') )'
          else
            Filter := Filter + ' AND owner = ''' + Tables[j].Schema + ''')';
        end;

      if Filter = '' then
        Exit;

      SQL := 'SELECT synonym_name, table_owner, table_name, db_link ' +
        'FROM sys.all_synonyms' + AddDBLink(DBLink) +
        ' WHERE ' + Filter +
        ' ORDER BY decode(owner, ''PUBLIC'', 1, 0)';

      Query.Close;
      Query.SetSQL(SQL);
      Query.Open;

      Query.AllocRecBuf(RecBuf);
      try
        for j := 0 to High(Tables) do
          if (Tables[j].Flag and 1 <> 0) and (Tables[j].DBLink = DBLink) then
            if Locate(Query, RecBuf, ['synonym_name'], [Tables[j].Name]) then begin
              Query.GetFieldAsVariant(Query.Fields[3], RecBuf, v);
              if (DBLink <> '') and (VarToStr(v) <> '') then
                Tables[j].Flag := 0
              else begin
                Tables[j].DBLink := VarToStr(v);
                Tables[j].Synonym := Tables[j].Name;

                Query.GetFieldAsVariant(Query.Fields[1], RecBuf, v);
                Tables[j].Schema := VarToStr(v);

                Query.GetFieldAsVariant(Query.Fields[2], RecBuf, v);
                Tables[j].Name := VarToStr(v);

                Tables[j].Flag := Tables[j].Flag or 8;
              end
            end
            else
              Tables[j].Flag := 0;

        Query.Close;
      finally
        Query.FreeRecBuf(RecBuf);
      end;
    until not MoreDBLinks;

    GetDBLinkSchema(Query);
  end;

var
  i: integer;
  NeedDescribeSynonyms: boolean;
  Query: TOCIRecordSet;
begin
  Query := TOCIRecordSet.Create;
  try
    Query.SetConnection(FConnection);

    for i := 0 to High(Tables) do begin
      if (FFieldOrigins <> foNone) or (i = FUpdatingTableInfoIdx) then
        Tables[i].Flag := 1
      else
        Tables[i].Flag := 0; // don't query fields for this table

      if Tables[i].Schema = '' then begin
        if Tables[i].DBLink <> '' then
          GetDBLinkSchema(Query)
        else
          Tables[i].Schema := FConnection.GetCachedSchema;
        end;
    end;

    repeat
      GetFieldsInfoServer(Query, NeedDescribeSynonyms);

      if NeedDescribeSynonyms then
        DescribeSynonyms(Query);
    until not NeedDescribeSynonyms;
  finally
    Query.Free;
  end;
end;

{$ENDIF}

function TOCIRecordSet.InternalCompareFieldValue(ValuePtr: IntPtr; ValueSize: Word; ValueType: Word; DataBuf: IntPtr; DataLen: Word; FieldType: Word; HasParent: boolean; IsFixed: boolean; const Options: TCompareOptions): integer;
var
  OraNumber: TOraNumber;
  OraTimeStamp: TOraTimeStamp;
  OraInterval: TOraInterval;
begin
  Result := 0;
  case FieldType of
    dtNumber:
      if ValueType in [dtFloat, dtSingle, dtInt32, dtString, dtLargeint] then begin
        OraNumber := TOraNumber.Create(OCISvcCtx);
        try
          case ValueType of
            dtFloat:
              OraNumber.AsFloat := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
            dtSingle:
              OraNumber.AsFloat := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(ValuePtr));
            dtInt32:
              OraNumber.AsInteger := Marshal.ReadInt32(ValuePtr);
            dtString:
              OraNumber.AsString := string(Marshal.PtrToStringAnsi(ValuePtr));
            dtLargeint:
              OraNumber.AsLargeInt := Marshal.ReadInt64(ValuePtr);
          end;
          Result := OraNumber.Compare(TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf))));
        finally
          OraNumber.Free;
        end;
      end
      else
      if ValueType = dtNumber then begin
        OraNumber := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        Result := OraNumber.Compare(TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf))));
      end
      else
        Assert(False);
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: // TODO: SQLTimeStamp support
      if ValueType in [dtDateTime, dtString] then begin
        OraTimeStamp := TOraTimeStamp.Create(OCISvcCtx, FieldType);
        try
          case ValueType of
            dtDateTime:
              OraTimeStamp.AsDateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
            dtString:
              OraTimeStamp.AsString := string(Marshal.PtrToStringAnsi(ValuePtr));
          end;
          Result := OraTimeStamp.Compare(TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf))));
        finally
          OraTimeStamp.Free;
        end;
      end
      else
      if ValueType in [dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ] then begin
        OraTimeStamp := TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        Result := OraTimeStamp.Compare(TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf))));
      end
      else
        Assert(False);
    dtIntervalYM, dtIntervalDS:
      if ValueType = dtString then begin
        OraInterval := TOraInterval.Create(OCISvcCtx, FieldType);
        try
          OraInterval.AsString := string(Marshal.PtrToStringAnsi(ValuePtr));
          Result := OraInterval.Compare(TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf))));
        finally
          OraInterval.Free;
        end;
      end
      else
      if ValueType in [dtIntervalYM, dtIntervalDS] then begin
        OraInterval := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        Result := OraInterval.Compare(TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf))));
      end
      else
        Assert(False);
  else
    Result := inherited InternalCompareFieldValue(ValuePtr, ValueSize, ValueType, DataBuf, DataLen, FieldType, HasParent, IsFixed, Options);
  end;
end;

function TOCIRecordSet.GetEOF: boolean;
begin
  if (IntPtr(CurrentItem )= nil) or (IntPtr(CurrentItem.Next) = nil) then
    if (hExecFetchThread <> nil) or (hFetchAllThread <> nil) then begin
    {$IFDEF MSWINDOWS}
      WaitForSingleObject(THandle(hEvent.Handle), INFINITE);
      if FirstItem = nil then
    {$ENDIF}
      begin
        Result := True;
        Exit;
      end;
    end;

  Result := inherited GetEOF;
end;

procedure TOCIRecordSet.SortItems;
begin
  // SortItems can be called during the command is executing or fetching
  if not GetNonBlocking or (FCommand.GetCursorState = csFetched) or
    ((FCommand.GetCursorState = csInactive) and not FCommand.Executing) // for AutoClose
  then
    inherited;
end;

procedure TOCIRecordSet.SetFilterText(const Value: string);
begin
  if not GetNonBlocking or (FCommand.GetCursorState = csFetched) or
    ((FCommand.GetCursorState = csInactive) and not FCommand.Executing) // for AutoClose
  then
    inherited
  else
    FTempFilterText := Value;
end;

procedure TOCIRecordSet.FilterUpdated;
begin
  // FilterUpdated can be called during the command is executing or fetching
  if not GetNonBlocking or (FCommand.GetCursorState = csFetched) or
    ((FCommand.GetCursorState = csInactive) and not FCommand.Executing) // for AutoClose
  then
    inherited;
end;

function TOCIRecordSet.CallbackDefine(Define: pOCIDefine; Iter: cardinal; var Buf: IntPtr;
  var BufLen: pub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;
var
  i: word;
  Blob: TBlob;
  Piece: PPieceHeader;
  Len: cardinal;
begin
  i := 0;
  while (i < FFields.Count) and (FFields[i].Handle <> Define) do
    Inc(i);

  Assert(i < FFields.Count);

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(FFetchItems,
    (RecordSize + sizeof(TItemHeader)) * Integer(Iter) + sizeof(TItemHeader) + FFields[i].DataOffset)));

  Len := Blob.PieceSize;
  Blob.AllocPiece(Piece, Len);
  Blob.AppendPiece(Piece);

  Buf := PieceData(Piece);
  Piece.Used := Len;
  BufLen := PieceUsedPtr(Piece);

  Ind := PtrOffset(FFetchItems,
    (RecordSize + sizeof(TItemHeader)) * Integer(Iter) + sizeof(TItemHeader) + DataSize + i * sizeof(sb2));

  if (OCISvcCtx.Home.OCIVersion >= 8160) and (OCISvcCtx.Home.OCIVersion < 9000) then begin
  // WAR fix Oracle bug that increments IntPtr to indicator; occurs on 8.1.6,8.1.7
    Ind := PtrOffset(Ind, - Integer(Iter) * sizeof(sb2));
  end;

  PieceStatus := OCI_NEXT_PIECE;

  Result := OCI_CONTINUE;
end;

function OCICallbackDefine(octxp: IntPtr; defnp: pOCIDefine; iter: ub4; var bufp: IntPtr;
  var alenp: pub4; var piece: ub1; var indp: IntPtr; var rcodep: pub2): sb4; cdecl;
begin
  Result := TOCIRecordSet(GetGCHandleTarget(octxp)).CallbackDefine(defnp, iter, bufp,
    alenp, piece, indp);
  rcodep := nil;
end;

function TOCIRecordSet.NeedInitFieldsOnFetch: boolean;
begin
  Result := not FCommand.NativeCursor or
            inherited NeedInitFieldsOnFetch;
end;

function TOCIRecordSet.NeedUnPrepareAfterFetch: boolean;
begin
  Result := FAutoClose;
end;

function TOCIRecordSet.InternalFetch(FetchBack: boolean = False): boolean;
begin
  if FPieceFetch or {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCILite then  // Lite don't supports array fetch
    Result := FetchPiece(FetchBack)
  else
    Result := FetchArray(FetchBack);
end;

procedure TOCIRecordSet.DoBeforeFetch(out Cancel: boolean);
begin
  Cancel := FWaitForFetchBreak;

  if Assigned(FOnBeforeFetch) then
    FOnBeforeFetch(Cancel);

  if Cancel then begin
    if FAutoClose then
      FCommand.InternalCancel
    else
      // reset cursor state for FetchAll
      if (FCommand.GetCursorState = csFetchingAll) then
        FCommand.SetCursorState(csFetching);
  end;
end;

procedure TOCIRecordSet.DoAfterFetch;
{$IFDEF MSWINDOWS}
var
  InThread: boolean;
{$ENDIF}
begin
  if Assigned(FOnAfterFetch) then begin
  {$IFDEF MSWINDOWS}
    InThread := (hFetchAllThread <> nil) and (hFetchAllThread.ThreadID = GetCurrentThreadId) or
                (hExecFetchThread <> nil) and (hExecFetchThread.ThreadID = GetCurrentThreadId);

    if InThread then begin
      if not FFetchAll or (FCommand.GetCursorState = csFetched) then
        PostMessage(hODACWindow, WM_AFTERFETCH, WPARAM(GCHandle), 0);
    end
    else
  {$ENDIF}
      FOnAfterFetch();
  end;
end;

function TOCIRecordSet.IsSupportedDataType(DataType: word): boolean;
begin
  Result := DataType in [dtString .. dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob,
    {$IFNDEF FPC}dtSQLTimeStamp,{$ENDIF} dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS,
    dtNumber, dtXML];
end;

function TOCIRecordSet.IsSpecificType(const Field: TFieldDesc; var DataType: word; var DataTypeName: string; var Len, Scale: integer): boolean;
begin
  Result := True;

  case Field.DataType of
    dtOraBlob: begin
      DataType := dtBlob;
      DataTypeName := 'BLOB';
      Len := -1;
      Scale := -1;
    end;
    dtOraClob,
    dtXML: begin
      DataType := dtMemo;
      DataTypeName := 'MEMO';
      Len := -1;
      Scale := -1;
    end;
    dtWideOraClob: begin
      DataType := dtWideMemo;
      DataTypeName := 'WIDEMEMO';
      Len := -1;
      Scale := -1;
    end;
  {$IFNDEF FPC}
    dtSQLTimeStamp: begin
      DataType := dtSQLTimeStamp;
      DataTypeName := 'TIMESTAMP';
      Len := -1;
      Scale := -1;
    end;
  {$ENDIF}
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
    {$IFNDEF FPC}
      DataType := dtSQLTimeStamp;
      DataTypeName := 'TIMESTAMP';
    {$ELSE}
      DataType := dtDateTime;
      DataTypeName := 'DATETIME';
    {$ENDIF}
      Len := -1;
      Scale := -1;
    end;
    dtIntervalYM, dtIntervalDS: begin
      DataType := dtString;
      DataTypeName := 'VARCHAR';
      Len := 255;
      Scale := -1;
    end;
    dtNumber: begin
      DataType := dtFloat;
      DataTypeName := 'DOUBLE';
      Len := -1;
      Scale := -1;
    end;
  else
    Result := inherited IsSpecificType(Field, DataType, DataTypeName, Len, Scale);
  end
end;

procedure TOCIRecordSet.FreeAllItems;
var
  Item: PItemHeader;
begin
  while IntPtr(FirstItem) <> nil do begin
    Item := FirstItem;
    FirstItem := Item.Next;
    if Item.Flag = flUsed then begin
      if FPieceFetch then
        BlockMan.FreeItem(Item);
    end;
  end;
  LastItem := nil;
end;

{ Fetch }

procedure TOCIRecordSet.AllocFetchBuffer;
var
  i, j: integer;
  FieldDesc: TOCIFieldDesc;
  FieldPtr: IntPtr;
  OraTimeStamp: TOraTimeStamp;
begin
  if FFetchBuffer <> nil then
    FreeFetchBuffer;

  FFetchBufferSize := 0;
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TOCIFieldDesc(FFields[i]);

    if not FieldDesc.HasParent and
       (FieldDesc.IsComplex or FieldDesc.IsConverted) and
       (FieldDesc.FieldDescKind = fdkData) and (FieldDesc.ActualFieldNo > -1)
    then begin
      FieldDesc.FFetchBufferOffset := FFetchBufferSize;

      case FieldDesc.DataType of
        dtExtString: begin
          Inc(FFetchBufferSize, FieldDesc.Length + 1); // for terminator
          FFetchBufferSize := (FFetchBufferSize + 1) and $FFFFFFFE; // align to 2
        end;
        dtMemo: begin
          if FieldDesc.IsBlobSubType then
            Inc(FFetchBufferSize, sizeof(IntPtr))
          else // String as Memo or LongStrings option
            Inc(FFetchBufferSize, FieldDesc.Length + 1); // for terminator
          FFetchBufferSize := (FFetchBufferSize + 1) and $FFFFFFFE; // align to 2
          FieldDesc.FFetchBufferLenOffset := FFetchBufferSize;
          Inc(FFetchBufferSize, sizeof(sb2));
        end;
        dtExtWideString:
          Inc(FFetchBufferSize, FieldDesc.Length * 2 + 2); // for terminator
        dtWideMemo: begin
          if FieldDesc.IsBlobSubType then
            Inc(FFetchBufferSize, sizeof(IntPtr))
          else // String as Memo or LongStrings option
            Inc(FFetchBufferSize, FieldDesc.Length * 2 + 2); // for terminator
          FieldDesc.FFetchBufferLenOffset := FFetchBufferSize;
          Inc(FFetchBufferSize, sizeof(sb2));
        end;
        dtExtVarBytes: begin
          Inc(FFetchBufferSize, FieldDesc.Length);
          FFetchBufferSize := (FFetchBufferSize + 1) and $FFFFFFFE; // align to 2
        end;
        dtNumber, dtInt64, dtUInt64, dtBCD, dtFMTBCD:
          Inc(FFetchBufferSize, OCI_NUMBER_SIZE);
        dtXML:
          Inc(FFetchBufferSize, 2 * sizeof(IntPtr));
        dtReference:
          Inc(FFetchBufferSize, sizeof(IntPtr));
        dtSQLTimeStamp:
          // Calculated Fields has SubDataType = dtUnknown
          if FieldDesc.SubDataType <> dtUnknown then
            Inc(FFetchBufferSize, 2 * sizeof(IntPtr))
          else
            FieldDesc.FFetchBufferOffset := -1;
        else
          Inc(FFetchBufferSize, sizeof(IntPtr));
      end;
    end
    else begin
      FieldDesc.FFetchBufferOffset := -1;
      FieldDesc.FFetchBufferLenOffset := -1;
    end;
  end;

  if FFetchBufferSize > 0 then begin
    FFetchBuffer := Marshal.AllocHGlobal(FFetchBufferSize * FFetchRows);

    for i := 0 to FFields.Count - 1 do begin
      FieldDesc := TOCIFieldDesc(FFields[i]);
      if (FieldDesc.DataType = dtSQLTimeStamp) and (FieldDesc.FFetchBufferOffset >= 0) then begin
        FieldPtr := PtrOffset(FFetchBuffer, FieldDesc.FFetchBufferOffset);
        for j := 0 to FFetchRows - 1 do begin
          OraTimeStamp := TOraTimeStamp.Create(OCISvcCtx, FieldDesc.DataType);
          OraTimeStamp.Precision := FieldDesc.Scale;
          Marshal.WriteIntPtr(FieldPtr, SizeOf(IntPtr), OraTimeStamp.GCHandle);
          Marshal.WriteIntPtr(FieldPtr, OraTimeStamp.OCIDateTime);

          FieldPtr := PtrOffset(FieldPtr, FFetchBufferSize);
        end;
      end;
    end;
  end;
end;

procedure TOCIRecordSet.FreeFetchBuffer;
var
  i, j: Integer;
  FieldDesc: TOCIFieldDesc;
  FieldPtr: IntPtr;
  ObjPtr: IntPtr;
  Obj: TSharedObject;
begin
  if FFetchBuffer <> nil then begin
    for i := 0 to FFields.Count - 1 do begin
      FieldDesc := TOCIFieldDesc(FFields[i]);
      if (FieldDesc.DataType = dtSQLTimeStamp) and (FieldDesc.FFetchBufferOffset >= 0) then begin
        FieldPtr := PtrOffset(FFetchBuffer, FieldDesc.FFetchBufferOffset);
        for j := 0 to FFetchRows - 1 do begin
          ObjPtr := Marshal.ReadIntPtr(FieldPtr, sizeof(IntPtr));
          if ObjPtr <> nil then begin
            Obj := TSharedObject(GetGCHandleTarget(ObjPtr));
            Obj.Free;
            Marshal.WriteIntPtr(FieldPtr, nil);
            Marshal.WriteIntPtr(FieldPtr, sizeof(IntPtr), nil);
          end;

          FieldPtr := PtrOffset(FieldPtr, FFetchBufferSize);
        end;
      end;
    end;
  end;

  inherited;
end;

procedure TOCIRecordSet.ProcessFetchedBlock(Block: PBlockHeader; FetchBack: boolean);
var
  i, j: integer;
  Item: IntPtr;
  BufferItem: IntPtr;
  DataPtr: IntPtr;
  BufferPtr: IntPtr;
  IndPtr: pOCIInd;
  DataLenPtr: PWord;
  ObjPtr: IntPtr;
  Piece: PPieceHeader;
  Len: Integer;
  ValueSize: Integer;
  FieldDesc: TOCIFieldDesc;
  RefFieldCount: Integer;
  ItemSize: Integer;
  Blob: TBlob;
  OraLob: TOraLob;
  OraFile: TOraFile;
  OraRef: TOraRef;
  OraNumber: TOraNumber;
  OraTimeStamp: TOraTimeStamp;
  OraInterval: TOraInterval;
  OraObject: TOraObject;
  OraXML: TOraXML;
  OraAnyData: TOraAnyData;
  SharedNumber: TOraNumber;
  SharedPiece: PPieceHeader;
  Curr: currency;
  i64: int64;
  BcdValue: TBCD;
  SQLTS: TSQLTimeStamp;
begin
  SharedNumber := nil;
  SharedPiece := nil;
  try
    Item := PtrOffset(Block, sizeof(TBlockHeader) + sizeof(TItemHeader));
    ItemSize := RecordSize + sizeof(TItemHeader);
    BufferItem := FFetchBuffer;

    for i := 0 to FLastRowsObtained - 1 do begin
      RefFieldCount := 0;
      for j := 0 to FFields.Count - 1 do begin
        FieldDesc := TOCIFieldDesc(FFields[j]);

        if not FieldDesc.HasParent and
           (FieldDesc.FieldDescKind = fdkData) and
           (FieldDesc.ActualFieldNo > -1) {KeyOnly SmartFetchState}
        then begin
            DataPtr := PtrOffset(Item, FieldDesc.DataOffset);

          if FieldDesc.FetchBufferLenOffset >= 0 then
            DataLenPtr := PtrOffset(BufferItem, FieldDesc.FetchBufferLenOffset)
          else if FieldDesc.HasValueLen and not FieldDesc.HasParent then
            DataLenPtr := PtrOffset(Item, FieldDesc.Offset)
          else
            DataLenPtr := nil;

          IndPtr := PtrOffset(Item, DataSize + j * sizeof(sb2));

          if FieldDesc.IsConverted or
             FieldDesc.IsComplex
          then begin
            ObjPtr := Marshal.ReadIntPtr(DataPtr);
            BufferPtr := PtrOffset(BufferItem, FieldDesc.FetchBufferOffset);

            case FieldDesc.DataType of
              dtReference: begin
                OraRef := TOraRef(GetGCHandleTarget(ObjPtr));
                OraRef.Indicator := IndPtr;
                Marshal.WriteInt16(OraRef.Indicator, Marshal.ReadInt16(FFetchBuffer, FFetchRows * FFetchBufferSize +
                                                     RefFieldCount * FFetchRows * SizeOf(SB2)), i * SizeOf(SB2));
                OraRef.OCIRef := Marshal.ReadIntPtr(BufferPtr);

                if not OraRef.IsNull then
                  OraRef.Pin;

                if FConnection.FDisconnectMode then
                  TOraRef(GetGCHandleTarget(ObjPtr)).CacheObject;

                Inc(RefFieldCount)
              end;
              dtMemo: begin
                Blob := TBlob(GetGCHandleTarget(ObjPtr));
                if FieldDesc.SubDataType in [dtString, dtNString, dtFixedChar, dtFixedNChar] then begin
                  // String as Memo or LongStrings option
                  ValueSize := DataLenPtr^;
                  if ValueSize > 0 then begin
                    Blob.AllocPiece(Piece, ValueSize); // for term
                    Blob.AppendPiece(Piece);
                    CopyBuffer(BufferPtr, PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader)), ValueSize);
                    Blob.FirstPiece.Used := ValueSize;
                  end;
                end
                else
                  Blob.Compress;
              end;
              dtWideMemo: begin
                Blob := TBlob(GetGCHandleTarget(ObjPtr));
                if FieldDesc.SubDataType in [dtWideString, dtNWideString, dtFixedWideChar, dtFixedNWideChar] then begin
                  // String as Memo or LongStrings option
                  ValueSize := Integer(DataLenPtr^) * sizeof(WideChar);
                  if ValueSize > 0 then begin
                    Blob.AllocPiece(Piece, ValueSize); // for term
                    Blob.AppendPiece(Piece);
                    CopyBuffer(BufferPtr, PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader)), ValueSize);
                    Blob.FirstPiece.Used := ValueSize;
                  end;
                end
                else
                  Blob.Compress;
              end;
              dtBlob:
                if not (FieldDesc.SubDataType in [dtString, dtNString, dtWideString, dtNWideString,
                                                  dtFixedChar, dtFixedNChar, dtFixedWideChar, dtFixedNWideChar])
                then begin
                  Blob := TBlob(GetGCHandleTarget(ObjPtr));
                  Blob.Compress;
                end;
              dtOraBlob, dtOraClob, dtWideOraClob: begin
                OraLob := TOraLob(GetGCHandleTarget(ObjPtr));
                if Marshal.ReadInt16(IndPtr) = OCI_IND_NULL then
                  OraLob.Clear
                else begin
                  OraLob.FNeedReadLob := True;
                  if not FDeferredLobRead and FCommand.FCacheLobs then begin
                    if IntPtr(SharedPiece) = nil then
                      TBlob.AllocPiece(SharedPiece, DefaultPieceSize);
                    if FCommand.IsLobPrefetchAllowed(OraLob.LobType = ltNClob, OraLob.IsUnicode) then
                      OraLob.ReadLob(SharedPiece, FCommand.FPrefetchLobSize)
                    else
                      OraLob.ReadLob(SharedPiece, -1);
                  {$IFDEF HAVE_COMPRESS}
                    if (FieldDesc.DataType = dtOraBlob) and
                       (FCommand.FCompressBlob in [cbClient, cbClientServer])
                    then
                      OraLob.Compressed := True
                    else
                      OraLob.Compressed := False;
                  {$ENDIF}
                  end
                  else
                    OraLob.FData.Write(0, 0, nil); // set is not null
                end;
              end;
              dtBFile, dtCFile: begin
                OraFile := TOraFile(GetGCHandleTarget(ObjPtr));
                if Marshal.ReadInt16(IndPtr) = OCI_IND_NULL then
                  OraFile.Clear
                else begin
                  OraFile.FNeedReadLob := True;
                  if not FDeferredLobRead and FCommand.FCacheLobs then begin
                    if OraFile.Exists then begin
                      OraFile.Open;
                      try
                        OraFile.ReadLob;
                      finally
                        OraFile.Close;
                      end;
                    end;
                  end
                  else
                    OraFile.FData.Write(0, 0, nil); // set is not null
                end;
              end;
              dtExtString: begin
                Len := DataLenPtr^;
                if FieldDesc.Fixed and TrimFixedChar then begin
                  PIntPtr(DataPtr)^ := StringHeap.AllocTrimmedStr(BufferPtr, Len);
                  DataLenPtr^ := Word(Len);
                end
                else
                  PIntPtr(DataPtr)^ := StringHeap.AllocStr(BufferPtr, Len);
              end;
              dtExtWideString: begin
                Len := DataLenPtr^;
                if FieldDesc.Fixed and TrimFixedChar then begin
                  PIntPtr(DataPtr)^ := StringHeap.AllocTrimmedWideStr(BufferPtr, Len);
                  DataLenPtr^ := Word(Len);
                end
                else
                  PIntPtr(DataPtr)^ := StringHeap.AllocWideStr(BufferPtr, Len);
              end;
              dtExtVarBytes: begin
                Len := DataLenPtr^;
                PIntPtr(DataPtr)^ := StringHeap.NewBuf(Len);
                DataPtr := PIntPtr(DataPtr)^;
                Move(BufferPtr^, DataPtr^, Len);
              end;
              dtNumber: begin
                OraNumber := TOraNumber(GetGCHandleTarget(ObjPtr));
                CopyBuffer(BufferPtr, OraNumber.OCINumberPtr, OCI_NUMBER_SIZE);
                OraNumber.FIndicatorPtr^ := IndPtr^;
              end;
              dtLargeint: begin
                // Calculated Fields has SubDataType = dtUnknown
                if FieldDesc.SubDataType <> dtUnknown then begin
                  if SharedNumber = nil then
                    SharedNumber := TOraNumber.Create(OCISvcCtx);

                  SharedNumber.OCINumberPtr := BufferPtr;
                  SharedNumber.FIndicatorPtr^ := IndPtr^;
                  Marshal.WriteInt64(DataPtr, SharedNumber.AsLargeInt);
                end;
              end;
              dtBCD: begin
                // Calculated Fields has SubDataType = dtUnknown
                if FieldDesc.SubDataType <> dtUnknown then begin
                  if SharedNumber = nil then
                    SharedNumber := TOraNumber.Create(OCISvcCtx);

                  SharedNumber.OCINumberPtr := BufferPtr;
                  SharedNumber.FIndicatorPtr^ := IndPtr^;
                  if SharedNumber.IsNull then
                    Curr := 0
                  else
                    Curr := StrToCurr(SharedNumber.AsString);
                  i64 := PInt64(@Curr)^;
                  Marshal.WriteInt64(DataPtr, i64);
                end;
              end;
              dtFMTBCD: begin
                // Calculated Fields has SubDataType = dtUnknown
                if FieldDesc.SubDataType <> dtUnknown then begin
                  if SharedNumber = nil then
                    SharedNumber := TOraNumber.Create(OCISvcCtx);

                  SharedNumber.OCINumberPtr := BufferPtr;
                  SharedNumber.FIndicatorPtr^ := IndPtr^;
                  BcdValue := SharedNumber.AsBCD;
                  PBcd(DataPtr)^ := BcdValue;
                end;
              end;
              dtSQLTimeStamp: begin
                // Calculated Fields has SubDataType = dtUnknown
                if FieldDesc.SubDataType <> dtUnknown then begin
                  ObjPtr := Marshal.ReadIntPtr(BufferPtr, sizeof(IntPtr));
                  OraTimeStamp := TOraTimeStamp(GetGCHandleTarget(ObjPtr));
                  OraTimeStamp.FIndicatorPtr^ := IndPtr^;
                  OraTimeStampToSQLTimeStamp(OraTimeStamp, SQLTS);
                  PSQLTimeStamp(DataPtr)^ := SQLTS;
                end;
              end;
              dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
                OraTimeStamp := TOraTimeStamp(GetGCHandleTarget(ObjPtr));
                OraTimeStamp.FIndicatorPtr^ := IndPtr^;
              end;
              dtIntervalYM, dtIntervalDS: begin
                OraInterval := TOraInterval(GetGCHandleTarget(ObjPtr));
                OraInterval.FIndicatorPtr^ := IndPtr^;
              end;
              dtObject: begin
                OraObject := TOraObject(GetGCHandleTarget(ObjPtr));
                OraObject.Instance := Marshal.ReadIntPtr(BufferPtr);
                if FConnection.FDisconnectMode then
                  OraObject.CacheObject;
              end;
              dtXML: begin
                OraXML := TOraXML(GetGCHandleTarget(ObjPtr));
                OraXML.Instance := Marshal.ReadIntPtr(BufferPtr);
                //OraXML.Indicator := Marshal.ReadIntPtr(BufferPtr, sizeof(IntPtr));
                OraXML.ReadXML;
                if FConnection.FDisconnectMode then
                  OraXML.CacheObject;
              end;
              dtAnyData: begin
                OraAnyData := TOraAnyData(GetGCHandleTarget(ObjPtr));
                OraAnyData.Instance := Marshal.ReadIntPtr(BufferPtr);
                if FConnection.FDisconnectMode then
                  OraAnyData.CacheObject;
              end;
              dtArray, dtTable: begin
                if FConnection.FDisconnectMode then begin
                  OraObject := TOraObject(GetGCHandleTarget(ObjPtr));
                  OraObject.CacheObject;
                end;
              end;
            end;
          end
          else
            // Oracle doesn't return Null terminator for CHAR
            case FieldDesc.DataType of
              dtString: begin
                Len := DataLenPtr^;
                Marshal.WriteByte(DataPtr, Len, 0);
              end;
              dtWideString: begin
                Len := DataLenPtr^;
                ValueSize := Len * SizeOf(WideChar);
                Marshal.WriteInt16(DataPtr, ValueSize, 0);
              end;
            end;

        end;
      end;

      Item := PtrOffset(Item, ItemSize);
      BufferItem := PtrOffset(BufferItem, FFetchBufferSize);
    end;
  finally
    SharedNumber.Free;
    if IntPtr(SharedPiece) <> nil then
      Marshal.FreeHGlobal(SharedPiece);
  end;
end;

procedure TOCIRecordSet.InitBlock(Block: PBlockHeader);
var
  i,j: integer;
  RecBuf: IntPtr;
  ObjPtr: IntPtr;
  FieldPtr: IntPtr;
  FieldDesc: TOCIFieldDesc;
  ItemSize: Integer;
begin
  // Create complex filds
  if not (HasComplexFields or FHasConvertedFields) then
    Exit;

  ItemSize := RecordSize + sizeof(TItemHeader);
  for i := 0 to Block.ItemCount - 1 do begin
    RecBuf := PtrOffset(Block, SizeOf(TBlockHeader) + i * ItemSize + SizeOf(TItemHeader));
    for j := 0 to FFields.Count - 1 do begin
      FieldDesc := TOCIFieldDesc(FFields[j]);
      if not FieldDesc.HasParent and
         FieldDesc.IsComplex and
         (FieldDesc.FieldDescKind = fdkData)
      then begin
        if 
           (FieldDesc.ActualFieldNo > -1) {KeyOnly SmartFetchState}
        then begin
          CreateComplexField(RecBuf, FieldDesc);
          ObjPtr := Marshal.ReadIntPtr(RecBuf, FieldDesc.DataOffset);
          FieldPtr := PtrOffset(FFetchBuffer, i * FFetchBufferSize + FieldDesc.FetchBufferOffset);
          case FieldDesc.DataType of
            dtCursor: begin
              TOraCursor(GetGCHandleTarget(ObjPtr)).FState := csExecuted;
              Marshal.WriteIntPtr(FieldPtr, TOraCursor(GetGCHandleTarget(ObjPtr)).OCIStmt);
            end;
            dtOraBlob, dtOraClob, dtWideOraClob, dtBFile, dtCFile:
              Marshal.WriteIntPtr(FieldPtr, TOraLob(GetGCHandleTarget(ObjPtr)).OCILobLocator);
            dtObject, dtArray, dtTable:
              Marshal.WriteIntPtr(FieldPtr, TOraObject(GetGCHandleTarget(ObjPtr)).Instance);
            dtXML: begin
              Marshal.WriteIntPtr(FieldPtr, TOraXML(GetGCHandleTarget(ObjPtr)).Instance);
              Marshal.WriteIntPtr(FieldPtr, sizeof(IntPtr), nil);
            end;
            dtAnyData:
              Marshal.WriteIntPtr(FieldPtr, TOraAnyData(GetGCHandleTarget(ObjPtr)).Instance);
            dtReference:
              Marshal.WriteIntPtr(FieldPtr, TOraRef(GetGCHandleTarget(ObjPtr)).OCIRef);
            dtMemo:
              if FieldDesc.SubDataType in [dtString, dtNString, dtFixedChar, dtFixedNChar] then // String as memo
                Marshal.WriteByte(FieldPtr, 0);
            dtWideMemo:
              if FieldDesc.SubDataType in [dtWideString, dtNWideString, dtFixedWideChar, dtFixedNWideChar] then // WideString as memo
                Marshal.WriteInt16(FieldPtr, 0);
            dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
              Marshal.WriteIntPtr(FieldPtr, TOraTimeStamp(GetGCHandleTarget(ObjPtr)).OCIDateTime);
            dtIntervalYM, dtIntervalDS:
              Marshal.WriteIntPtr(FieldPtr, TOraInterval(GetGCHandleTarget(ObjPtr)).OCIInterval);
          end;
        end
        else
          Marshal.WriteIntPtr(RecBuf, FieldDesc.DataOffset, nil)
      end;
    end;
  end;
end;

procedure TOCIRecordSet.SwapBlocks(Block1, Block2: PBlockHeader);
begin
  Block1.Prev := nil;
  Block1.Next := Block2;
  Block2.Next := nil;
  Block2.Prev := Block1;
end;

procedure TOCIRecordSet.InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean);
begin
  if Filtered and not (FFetchAll or (GetNonBlocking and (IndexFieldCount > 0))) then begin
    if FetchBack then
      InitFetchedItems(PtrOffset(Block, sizeof(TBlockHeader) + (RowsObtained - 1) * (sizeof(TItemHeader) + RecordSize)), FNoCountData, FetchBack)
    else
      InitFetchedItems(PtrOffset(Block, sizeof(TBlockHeader)), FNoCountData, FetchBack);
  end
  else
    if not (FetchBack or FNoCountData) then
      Inc(FRecordCount, RowsObtained);

{$IFDEF MSWINDOWS}
  // in the thread for FetchAll only
  if hFetchAllThread <> nil then
    PulseEvent(THandle(hEvent.Handle));
{$ENDIF}
end;

function TOCIRecordSet.FetchArray(FetchBack: boolean = False): boolean;
var
  i: integer;
  pRec, pInd: IntPtr;
  RowsRequested: word;
  Block: PBlockHeader;
  TempBlock: PBlockHeader;
  ItemSize: integer;
  NewBlock: boolean;
  BufferPtr: IntPtr;
  RefFieldCount: integer;
  BufSkip: integer;
  IndSkip: integer;
  LenSkip: Integer;
  ObjPtr: IntPtr;
  IndPtr: IntPtr;
  LenPtr: IntPtr;
  OldFirstItem: PItemHeader;
  OldLastItem: PItemHeader;
  OldFetchStart: integer;
  OldFetchEnd: integer;
  CharsetID: word;
  FieldDesc: TOCIFieldDesc;
{$IFNDEF LITE}
  StandAloneBlock: boolean;
{$ENDIF}
begin
  Result := False;

  if FetchBack and (FFetchStart = 0) and not FEOF or not FetchBack and
    (FFetchEnd = FCommand.FFetchedRows) and FNoCountData and not FBOF
  then
    Exit;

  FCommand.Lock;
  try // For Busy
    // for close in time fetch all
    if FCommand.GetCursorState < csBound then // For Non Blocking
      Exit;

    ItemSize := RecordSize + sizeof(TItemHeader);

  {$IFNDEF LITE}
    StandAloneBlock := FSmartFetchState = sfDataByKey;
  {$ENDIF}
    NewBlock := {$IFNDEF LITE}(StandAloneBlock and (FSmartFetchBlock = nil)) or{$ENDIF}
                (BlockMan.FirstBlock = nil) or
                ({$IFNDEF LITE}not StandAloneBlock and{$ENDIF}
                 not FUniDirectional and
                 not (CanFetchBack and ((BlockMan.FirstBlock.Next <> nil) or FBOF or FEOF)));

    if NewBlock then begin
      BlockMan.AllocBlock(Block, FFetchRows {$IFNDEF LITE},StandAloneBlock{$ENDIF});

    {$IFNDEF LITE}
      if StandAloneBlock then
        FSmartFetchBlock := Block;
    {$ENDIF}

      if FetchBack then begin
        SwapBlocks(Block.Next, Block);
        BlockMan.FirstBlock := Block.Prev;
      end;

      if HasComplexFields or FHasConvertedFields then
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

      Block := FSmartFetchBlock;
    end
  {$ENDIF}
    else begin
      if BlockMan.FirstBlock.Next <> nil then begin // more then one block
        NewBlock := True; // for redefine data
        if FEOF or FBOF then
          Block := BlockMan.FirstBlock
        else begin
          // BlockMan.FirstBlock.Next always present FFetchStart position
          TempBlock := BlockMan.FirstBlock;
          Block := TempBlock.Next;
          if FetchBack then begin
            // if more then one block used
            if not ((IntPtr(FirstItem) <> nil) and (IntPtr(LastItem) <> nil) and (FirstItem.Block = LastItem.Block)) then begin
              SwapBlocks(Block, TempBlock);
              BlockMan.FirstBlock := Block;
              if IntPtr(LastItem) <> nil then begin
                LastItem := PtrOffset(Block, sizeof(TBlockHeader) + (Block.UsedItems - 1) * ItemSize);
                Dec(FFetchEnd, TempBlock.UsedItems);
              end;
              Block := TempBlock;
            end;
          end
          else begin
            SwapBlocks(Block, TempBlock);
            BlockMan.FirstBlock := Block;
            if (IntPtr(FirstItem) <> nil) and ((IntPtr(LastItem) = nil) or (IntPtr(FirstItem.Block) <> IntPtr(LastItem.Block))) then begin
              FirstItem := PtrOffset(TempBlock, sizeof(TBlockHeader));
              Inc(FFetchStart, Block.UsedItems);
            end;
          end;
        end;

        ClearBlock(Block);
      end
      else begin
        Block := BlockMan.FirstBlock;
        // Refresh block: drop values of blobs
        ClearBlock(Block);

      end;
    end;
    InitBlock(Block);

    if CanFetchBack then begin
      if FBOF then begin
        FFetchStart := FFetchEnd;
        LastItem := nil;
      end;
      if FEOF then begin
        FFetchEnd := FFetchStart;
        FirstItem := nil;
      end;
    end;

    // remeber first item and last item on case of exception
    OldFirstItem := FirstItem;
    OldLastItem := LastItem;
    OldFetchStart := FFetchStart;
    OldFetchEnd := FFetchEnd;
    try     // For free memory
      if {$IFNDEF LITE}StandAloneBlock or{$ENDIF}
         NewBlock
      then begin
        pRec := PtrOffset(Block, sizeof(TBlockHeader) + sizeof(TItemHeader));
        pInd := PtrOffset(pRec, DataSize);

        FConnection.Lock;
        try  // For FConnection.Busy
        { DefineData }
          RefFieldCount := 0;
          for i := 0 to FFields.Count - 1 do begin
            FieldDesc := TOCIFieldDesc(FFields[i]);
            if not FieldDesc.HasParent and
               (FieldDesc.FieldDescKind = fdkData) and
               (FieldDesc.ActualFieldNo > -1) {KeyOnly SmartFetchState}
            then
              if 
                 (FieldDesc.DataType in [dtBlob, dtMemo, dtWideMemo]) and
                 (not (FieldDesc.SubDataType in [dtString, dtNString, dtWideString, dtNWideString,
                                                 dtFixedChar, dtFixedNChar, dtFixedWideChar, dtFixedNWideChar]))
              then begin
                ObjPtr := Marshal.ReadIntPtr(pRec, FieldDesc.DataOffset);

                if TBlob(GetGCHandleTarget(ObjPtr)).IsUnicode then
                  CharsetId := OCI_UTF16ID
                else begin
                {$IFDEF IS_UTF8_EXCLUDE_MEMO}
                  CharsetId := 0;
                {$ELSE}
                  CharsetId := FConnection.FCharsetId;
                  if (CharsetId = 0) and FConnection.UnicodeEnvironment then
                    CharsetId := FConnection.FInternalCharsetId;
                {$ENDIF}
                end;

                FCommand.DefineDynamic8(FieldDesc, GCHandle, OCICallbackDefinePtr, CharsetId);
              end
              else 
              begin
                if FieldDesc.FFetchBufferOffset >= 0 then begin
                  BufferPtr := PtrOffset(FFetchBuffer, FieldDesc.FFetchBufferOffset);
                  BufSkip := FFetchBufferSize;
                end
                else begin
                  BufferPtr := PtrOffset(pRec, FieldDesc.DataOffset);
                  BufSkip := ItemSize;
                end;

                IndPtr := PtrOffset(pInd, i * sizeof(sb2));
                IndSkip := ItemSize;

                if FieldDesc.FFetchBufferLenOffset >= 0 then begin
                  LenPtr := PtrOffset(FFetchBuffer, FieldDesc.FFetchBufferLenOffset);
                  LenSkip := FFetchBufferSize;
                end
                else if FieldDesc.HasValueLen then begin
                  LenPtr := PtrOffset(pRec, FieldDesc.Offset);
                  LenSkip := ItemSize;
                end
                else begin
                  LenPtr := nil;
                  LenSkip := 0;
                end;

                // special IndPtr definition
                case FieldDesc.DataType of
                  dtObject, dtAnyData, dtArray, dtTable: begin
                    Marshal.WriteInt16(pInd, i * sizeof(sb2), 0);
                    IndPtr := nil; // WAR Oracle bug - indicator skip is ignored for Objects
                  end;
                  dtXML: begin
                    IndPtr := PtrOffset(BufferPtr, sizeof(IntPtr));
                    IndSkip := FFetchBufferSize;
                  end;
//                  dtAnyData: begin
//                    IndPtr := PtrOffset(BufferPtr, sizeof(IntPtr));
//                    IndSkip := FFetchBufferSize;
//                  end;
                  dtReference: begin
                    IndPtr := PtrOffset(FFetchBuffer, FFetchRows * FFetchBufferSize + RefFieldCount * FFetchRows * SizeOf(SB2));
                    IndSkip := SizeOf(SB2); // WAR Oracle bug - indicator skip is ignored for REFs
                    inc(RefFieldCount);
                  end;
                end;

                FCommand.DefineData(FieldDesc, BufferPtr, IndPtr, LenPtr, BufSkip, IndSkip, LenSkip);
              end;
          end;
        finally
          FConnection.Release;
        end;
      end;

      FFetchItems := PtrOffset(Block, sizeof(TBlockHeader));

      if FetchBack then begin
        if FFetchStart < FFetchRows then
          RowsRequested := FFetchStart
        else
          RowsRequested := FFetchRows;
        FLastRowsObtained := FCommand.InternalFetch(RowsRequested, OCI_FETCH_ABSOLUTE, FFetchStart - RowsRequested + 1);
        Dec(FFetchStart, FLastRowsObtained);
      end
      else begin
        if FNoCountData and (FCommand.FFetchedRows > FFetchEnd) then begin
          RowsRequested := FCommand.FFetchedRows - FFetchEnd;
          if RowsRequested > FFetchRows then
            RowsRequested := FFetchRows;
        end
        else
          RowsRequested := FFetchRows;
        if FFetchAbsolute then
          FLastRowsObtained := FCommand.InternalFetch(RowsRequested, OCI_FETCH_ABSOLUTE, FFetchEnd + 1)
        else
          FLastRowsObtained := FCommand.InternalFetch(RowsRequested);
        Inc(FFetchEnd, FLastRowsObtained);
      end;

      FFetchAbsolute := FetchBack;

      Result := FLastRowsObtained > 0;

      if Result then begin
        ProcessFetchedBlock(Block, FetchBack);
        CreateBlockStruct(Block, FLastRowsObtained, FetchBack {$IFNDEF LITE},StandAloneBlock{$ENDIF});

        if HasComplexFields or FHasConvertedFields then
          if FCommand.GetCursorState in [csFetched, csInactive] then
          {$IFNDEF LITE}
            if not StandAloneBlock then
          {$ENDIF}
              FreeFetchBuffer;
      {$IFNDEF LITE}
        if StandAloneBlock then
          FCommand.InternalCancel;
      {$ENDIF}
      end // if Result then begin
      else begin
        if not CanFetchBack then begin
          if HasComplexFields or FHasConvertedFields then
          {$IFNDEF LITE}
            if not StandAloneBlock then
          {$ENDIF}
              FreeFetchBuffer;

          if NewBlock then begin
            ClearBlock(Block);
          {$IFNDEF LITE}
            if not StandAloneBlock then
          {$ENDIF}
              BlockMan.FreeBlock(Block {$IFNDEF LITE},StandAloneBlock{$ENDIF});
          end;
        end else
          FirstItem.Prev := nil;
      end;

    except
      if NewBlock then begin
        ClearBlock(Block); // !!! Possible AV on fatal error
        // BlockMan.FirstBlock = nil means that dataset was closed after some
        // fatal error and all blocks are already freed.
        if IntPtr(BlockMan.FirstBlock) <> nil then begin
          BlockMan.FreeBlock(Block {$IFNDEF LITE},StandAloneBlock{$ENDIF});
          // restore first and last items
          FirstItem := OldFirstItem;
          LastItem := OldLastItem;
          if IntPtr(FirstItem) <> nil then
            FirstItem.Prev := nil;
          if IntPtr(LastItem) <> nil then
            LastItem.Next := nil;
          // restore fetch origins
          FFetchStart := OldFetchStart;
          FFetchEnd := OldFetchEnd;
        end;
      end;
      raise;
    end;  // try
  finally
    FCommand.Release;
  end;
end;

var
  GlobalBuf: IntPtr = nil; // static as odefinps writes data to it

function TOCIRecordSet.FetchPiece(FetchBack: boolean = False): boolean;
var
  i: word;
  pRec, pInd: IntPtr;
  Res: integer;
  Item: PItemHeader;
  FirstItemOrder: Integer;
  LastItemOrder: Integer;
  PieceStatus: ub1;
  Blob: TBlob;
  Piece: PPieceHeader;
  Ptr: IntPtr;
  BufLen: ub4;
  Iteration: ub4;
  Index: ub4;
  NeedData: integer;
  NoData: integer;
  hDefine: pOCIHandle;
  BufferPtr: IntPtr;
  LenPtr: PWord;
  OraFile: TOraFile;
  OraRef: TOraRef;
  ObjPtr: IntPtr;
  Mode: TParamDirection;
  IndPtr: IntPtr;
  CharsetId: word;
  FieldDesc: TOCIFieldDesc;
begin
  Result := False;

  FCommand.Lock;
  try  // For Busy
    if FCommand.GetCursorState < csBound then // For Non Blocking
      Exit;

    if FUniDirectional and (IntPtr(FirstItem) <> nil) then begin
      Item := FirstItem;
      FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True);

      FirstItem := nil;
      LastItem := nil;
      CurrentItem := nil;
    end
    else begin
      if CanFetchBack and (IntPtr(BlockMan.FirstFree) = nil) and (IntPtr(BlockMan.FirstBlock) <> nil) then begin
        if FetchBack then begin
          if IntPtr(LastItem) <> nil then begin
            Item := LastItem.Prev;
            FreeComplexFields(PtrOffset(LastItem, sizeof(TItemHeader)), True);
            BlockMan.FreeItem(LastItem);
            LastItem := Item;
            if IntPtr(Item) <> nil then
              Item.Next := nil;
            Dec(FFetchEnd);
          end
        end
        else begin
          if IntPtr(FirstItem) <> nil then begin
            Item := FirstItem.Next;
            FreeComplexFields(PtrOffset(FirstItem, sizeof(TItemHeader)), True);
            BlockMan.FreeItem(FirstItem);
            FirstItem := Item;
            if IntPtr(Item) <> nil then
              Item.Prev := nil;
            Inc(FFetchStart);
          end;
        end;
      end;
      BlockMan.AllocItem(Item);
    end;

    if CanFetchBack then begin
      if FBOF then begin
        FFetchStart := FFetchEnd;
        LastItem := nil;
      end;
      if FEOF then begin
        FFetchEnd := FFetchStart;
        FirstItem := nil;
      end;
    end;

    pRec := PtrOffset(Item, sizeof(TItemHeader));
    CreateComplexFields(pRec, True);
    try     // For free memory
      pInd := PtrOffset(pRec, DataSize);
      BufLen := MaxBlobSize;
      FConnection.Lock;
      try  // For FConnection.Busy
           // Picewise fetch as one operation

      { DefineData } // OPTIM - Once
        for i := 0 to FFields.Count - 1 do begin
          FieldDesc := TOCIFieldDesc(FFields[i]);
          if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then begin
            BufferPtr := PtrOffset(pRec, FieldDesc.DataOffset);
            if FieldDesc.HasValueLen then
              LenPtr := PtrOffset(pRec, FieldDesc.Offset)
            else
              LenPtr := nil;

            if FieldDesc.DataType = dtXML then begin
              ObjPtr := Marshal.ReadIntPtr(BufferPtr);
              IndPtr := TOraObject(GetGCHandleTarget(ObjPtr)).IndicatorPtr;
              Marshal.WriteIntPtr(IndPtr, nil);
            end
            else
              IndPtr := PtrOffset(pInd, i * sizeof(sb2));

            case FieldDesc.DataType of
              dtCursor: begin
                ObjPtr := Marshal.ReadIntPtr(BufferPtr);
                TOraCursor(GetGCHandleTarget(ObjPtr)).FState := csExecuted;
                BufferPtr := TOraCursor(GetGCHandleTarget(ObjPtr)).OCIStmtPtr;
              end;
              dtOraBlob, dtOraClob, dtWideOraClob, dtBFile, dtCFile:
                BufferPtr := TOraLob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).OCILobLocatorPtr;
              dtObject, dtXML, dtAnyData, dtArray, dtTable:
                BufferPtr := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).InstancePtr;
              dtReference:
                BufferPtr := TOraRef(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).OCIRefPtr;
              dtMemo:
                if FieldDesc.SubDataType in [dtString, dtNString, dtFixedChar, dtFixedNChar] then begin
                  TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).AllocPiece(Piece, FieldDesc.Length + 1); // for term
                  TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).AppendPiece(Piece);
                  BufferPtr := PtrOffset(Piece, sizeof(TPieceHeader));
                end;
              dtWideMemo:
                if FieldDesc.SubDataType in [dtWideString, dtNWideString, dtFixedWideChar, dtFixedNWideChar] then begin
                  TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).AllocPiece(Piece, FieldDesc.Length * 2 + sizeof(WideChar)); // for term
                  TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).AppendPiece(Piece);
                  BufferPtr := PtrOffset(Piece, sizeof(TPieceHeader));
                end;
              dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
                BufferPtr := TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).OCIDateTimePtr;
              dtIntervalYM, dtIntervalDS:
                BufferPtr := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).OCIIntervalPtr;
              dtExtString: begin
                LenPtr^ := 0;
                PIntPtr(BufferPtr)^ := StringHeap.NewBuf(Integer(FieldDesc.Length) + 1);
                BufferPtr := PIntPtr(BufferPtr)^
              end;
              dtExtWideString: begin
                LenPtr^ := 0;
                PIntPtr(BufferPtr)^ := StringHeap.NewBuf((Integer(FieldDesc.Length) + 1) * SizeOf(WideChar));
                BufferPtr := PIntPtr(BufferPtr)^;
              end;
              dtExtVarBytes: begin
                LenPtr^ := 0;
                PIntPtr(BufferPtr)^ := StringHeap.NewBuf(FieldDesc.Length);
                BufferPtr := PIntPtr(BufferPtr)^;
              end;
              dtNumber: begin
                ObjPtr := Marshal.ReadIntPtr(BufferPtr);
                BufferPtr := TOraNumber(GetGCHandleTarget(ObjPtr)).OCINumberPtr;
              end;
            end;

            if (FieldDesc.DataType in [dtBlob, dtMemo, dtWideMemo]) and
               (not (FieldDesc.SubDataType in [dtString, dtNString, dtWideString, dtNWideString,
                                               dtFixedChar, dtFixedNChar, dtFixedWideChar, dtFixedNWideChar]))
            then
              if FCommand.FOCICallStyle = OCI73 then
                FCommand.DefineDynamic7(FieldDesc, nil, IndPtr)
              else begin
                ObjPtr := Marshal.ReadIntPtr(BufferPtr);
                if TBlob(GetGCHandleTarget(ObjPtr)).IsUnicode then
                  CharsetId := OCI_UTF16ID
                else
                  CharsetId := FConnection.FCharsetId;
                FCommand.DefineDynamic8(FieldDesc, GCHandle, OCICallbackDefinePtr, CharsetId);
              end
            else
              FCommand.DefineData(FieldDesc, BufferPtr, IndPtr, LenPtr, 0, 0, 0);
          end;
        end;

        FFetchItems := Item;

        if FetchBack then
          Res := FCommand.InternalFetchPiece(OCI_FETCH_ABSOLUTE, FFetchStart)
        else begin
          if FFetchAbsolute then
            Res := FCommand.InternalFetchPiece(OCI_FETCH_ABSOLUTE, FFetchEnd + 1)
          else
            Res := FCommand.InternalFetchPiece;
        end;

        FFetchAbsolute := FetchBack;

        if FCommand.FOCICallStyle = OCI73 then begin
          NeedData := OCI_STILL_IS_PIECE;
          NoData := OCI_NO_DATA_FOUND;
        end
        else
        if FCommand.FOCICallStyle = OCI80 then begin
          NeedData := OCI_NEED_DATA;
          NoData := OCI_NO_DATA;
        end
        else begin
          FConnection.CheckOCI;
          NeedData := 0;
          NoData := 0;
        end;

        if Res = NeedData then begin
          if GlobalBuf = nil then
            GlobalBuf := Marshal.AllocHGlobal(sizeof(IntPtr));
          Marshal.WriteIntPtr(GlobalBuf, nil);
          for i := 0 to FFields.Count - 1 do begin
            FieldDesc := TOCIFieldDesc(FFields[i]);
            if FieldDesc.FieldDescKind <> fdkData then
              continue;
            if (FieldDesc.DataType in [dtBlob, dtMemo, dtWideMemo]) and
               (not (FieldDesc.SubDataType in [dtString, dtNString, dtWideString, dtNWideString,
                                               dtFixedChar, dtFixedNChar, dtFixedWideChar, dtFixedNWideChar]))
            then begin
              if Res <> NeedData then
                Assert(False);            // DEBUG

              FCommand.GetPI(hDefine, PieceStatus, Ptr, Iteration, Index, Mode);

              Blob := nil; // anti warning
              if (PieceStatus = OCI_FIRST_PIECE) or (PieceStatus = OCI_ONE_PIECE) then begin
                ObjPtr := Marshal.ReadIntPtr(pRec, FieldDesc.DataOffset);
                Blob := TBlob(GetGCHandleTarget(ObjPtr));
              end
              else
                Assert(False); // DEBUG

              repeat
                BufLen := Blob.PieceSize;
                Blob.AllocPiece(Piece, BufLen);

                FCommand.SetPI(hDefine, OCI_HTYPE_DEFINE, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)),
                  BufLen, PtrOffset(pInd, i * sizeof(sb2)));

                Res := FCommand.InternalFetchPiece;

                if BufLen > 0 then begin
                  Piece.Used := BufLen;
                  if BufLen < Cardinal(Blob.PieceSize) then
                    Blob.ReallocPiece(Piece, BufLen);
                  Blob.AppendPiece(Piece);
                end
                else
                  Blob.FreePiece(Piece);

                FCommand.GetPI(hDefine, PieceStatus, Ptr, Iteration, Index, Mode);

                if PieceStatus = OCI_FIRST_PIECE then begin
                  BufLen := sizeof(integer);
                  FCommand.SetPI(hDefine, OCI_HTYPE_DEFINE, PieceStatus, GlobalBuf, BufLen, nil);  // ???
                end;
              until (Res = 0) or (Res = NoData) or (PieceStatus = OCI_FIRST_PIECE);
            end
//            else
//            if FieldDesc.DataType = dtExtString then begin
//              ObjPtr := PtrOffset(pRec, FieldDesc.DataOffset);
//              Ptr := Marshal.ReadIntPtr(ObjPtr);
//              Marshal.WriteIntPtr(ObjPtr, StringHeap.ReAllocStr(Ptr, FieldDesc.Fixed and TrimFixedChar));
//            end
//            else
//            if FieldDesc.DataType = dtExtWideString then begin
//              ObjPtr := PtrOffset(pRec, FieldDesc.DataOffset);
//              Ptr := Marshal.ReadIntPtr(ObjPtr);
//              Marshal.WriteIntPtr(ObjPtr, StringHeap.ReAllocStr(Ptr, FieldDesc.Fixed and TrimFixedChar));
//            end
//            else
//            if FieldDesc.DataType = dtExtVarBytes then begin
//              Ptr := PtrOffset(pRec, FieldDesc.DataOffset);
//              ATemp := StringHeap.NewBuf(Marshal.ReadInt16(Marshal.ReadIntPtr(Ptr)) + SizeOf(Word));
//              CopyBuffer(Marshal.ReadIntPtr(Ptr), ATemp, Marshal.ReadInt16(Marshal.ReadIntPtr(Ptr)) + SizeOf(Word));
//              StringHeap.DisposeBuf(Marshal.ReadIntPtr(Ptr));
//              Marshal.WriteIntPtr(Ptr, ATemp);
//            end;
          end;
          Marshal.WriteInt32(GlobalBuf, $FFFFFF); // DEBUG
        end // if Res = NeedData then begin
        else
          if (FCommand.FOCICallStyle = OCI73) and (Res <> NoData) then
            Assert(False); // DEBUG
      finally
        FConnection.Release;
      end;

      Result := Res = 0;

      if Result then begin
        // Prepare comlex field
        if HasComplexFields then
          for i := 0 to FFields.Count - 1 do begin
            FieldDesc := TOCIFieldDesc(FFields[i]);
            if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then begin
              ObjPtr := Marshal.ReadIntPtr(pRec, FieldDesc.DataOffset);
              case FieldDesc.DataType of
                dtBlob, dtMemo:
                  if FieldDesc.SubDataType in [dtString, dtNString, dtFixedChar, dtFixedNChar] then
                    TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece.Used :=
                      StrLen(PAChar(PtrOffset(TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece, sizeof(TPieceHeader))))
                  else
                    TBlob(GetGCHandleTarget(ObjPtr)).Compress;
                dtWideMemo:
                  if FieldDesc.SubDataType in [dtWideString, dtNWideString, dtFixedWideChar, dtFixedNWideChar] then
                    TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece.Used :=
                      StrLenW(PWChar(PtrOffset(TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece, sizeof(TPieceHeader)))) * sizeof(WideChar)
                  else
                    TBlob(GetGCHandleTarget(ObjPtr)).Compress;
                dtOraBlob, dtOraClob, dtWideOraClob: begin
                  TOraLob(GetGCHandleTarget(ObjPtr)).FNeedReadLob := True;
                  if not FDeferredLobRead and FCommand.FCacheLobs then
                    TOraLob(GetGCHandleTarget(ObjPtr)).ReadLob;
                {$IFDEF HAVE_COMPRESS}
                  if (FieldDesc.DataType = dtOraBlob) and
                    (FCommand.FCompressBlob in [cbClient, cbClientServer])
                  then
                    TOraLob(GetGCHandleTarget(ObjPtr)).Compressed := True
                  else
                    TOraLob(GetGCHandleTarget(ObjPtr)).Compressed := False;
                {$ENDIF}
                end;
                dtBFile, dtCFile: begin
                  TOraFile(GetGCHandleTarget(ObjPtr)).FNeedReadLob := True;
                  if not FDeferredLobRead and FCommand.FCacheLobs then begin
                    OraFile := TOraFile(GetGCHandleTarget(ObjPtr));
                    if OraFile.Exists then begin
                      OraFile.Open;
                      try
                        OraFile.ReadLob;
                      finally
                        OraFile.Close;
                      end;
                    end;
                  end;
                end;
                dtReference: begin
                  OraRef := TOraRef(GetGCHandleTarget(ObjPtr));
                  if OraRef.OCIRef <> nil then
                    OraRef.Pin;
                end;
                dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
                  TOraTimeStamp(GetGCHandleTarget(ObjPtr)).FIndicatorPtr^ := Marshal.ReadInt16(pRec, DataSize + i * sizeof(sb2));
                dtIntervalYM, dtIntervalDS:
                  TOraInterval(GetGCHandleTarget(ObjPtr)).FIndicatorPtr^ := Marshal.ReadInt16(pRec, DataSize + i * sizeof(sb2));
                dtNumber:
                  TOraNumber(GetGCHandleTarget(ObjPtr)).FIndicatorPtr^ := Marshal.ReadInt16(pRec, DataSize + i * sizeof(sb2));
                dtXML:
                  TOraXML(GetGCHandleTarget(ObjPtr)).ReadXML;
              end;
            end;
          end;

        // Create Items
        if IntPtr(FirstItem) <> nil then
          FirstItemOrder := FirstItem.Order
        else
          FirstItemOrder := 0;
        if IntPtr(LastItem) <> nil then
          LastItemOrder := LastItem.Order
        else
          LastItemOrder := 0;

        Item.Flag := flUsed;
        InitItem(Item);

        if FetchBack then begin
          if IntPtr(LastItem) = nil then
            LastItem := Item;
          if IntPtr(FirstItem) <> nil then
            FirstItem.Order := FirstItemOrder;

          Item.Prev := nil;
          Item.Next := FirstItem;

          if IntPtr(FirstItem) <> nil then begin
            FirstItem.Prev := Item;
            Item.Order := FirstItem.Order - 1;
          end
          else
            Item.Order := FRecordCount;

          FirstItem := Item;
          Dec(FFetchStart);
        end
        else begin
          if IntPtr(FirstItem) = nil then
            FirstItem := Item;
          if IntPtr(LastItem) <> nil then
            LastItem.Order := LastItemOrder;

          Item.Prev := LastItem;
          Item.Next := nil;

          if IntPtr(LastItem) <> nil then begin
            LastItem.Next := Item;
            Item.Order := LastItem.Order + 1;
          end
          else
            Item.Order := 1;

          LastItem := Item;

          Inc(FRecordCount);
          Inc(FRowsFetched);
          Inc(FFetchEnd);
        end;

        UpdateCachedBuffer(Item, Item);
        // update fetched rows for correct TDataSet.RecordCount calculation
        FCommand.FFetchedRows := FRowsFetched;

      {$IFDEF MSWINDOWS}
        // in the thread only
        if (hExecFetchThread <> nil) or (hFetchAllThread <> nil) then
          PulseEvent(THandle(hEvent.Handle));
      {$ENDIF}
      end // if Result then begin
      else begin
        FreeComplexFields(pRec, True);
        BlockMan.FreeItem(Item);
      end;
    except
      FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True); // !!! Possible AV on fatal error
      // BlockMan.FirstBlock = nil means that dataset was closed after some
      // fatal error and all blocks are already freed.
      if IntPtr(BlockMan.FirstBlock) <> nil then
        BlockMan.FreeItem(Item);
      raise;
    end;
  finally
    FCommand.Release;
  end;
end;

procedure TOCIRecordSet.DoFetchAll;
const
  MaxAutoFetchRows: Integer = 800;
var
  OldFetchRows: Integer;
begin
  try
    FWaitForFetchBreak := False;
    FFetching := True;
    FLastRowsObtained := 0;
    OldFetchRows := FFetchRows;
    try
      if CanFetchBack then begin
        SetToEnd;
        Fetch(True);
      end
      else
        while True do begin
          if FWaitForFetchBreak then begin
            FCommand.InternalCancel;
            break;
          end;

          // FetchAll performance optimization
          if not FUniDirectional and
             {$IFNDEF LITE}(FSmartFetchState = sfNone) and {$ENDIF}
             (FFetchRows < MaxAutoFetchRows) and
             (FLastRowsObtained = FFetchRows)
          then begin
            FreeFetchBuffer;
            FFetchRows := FFetchRows * 2;
            if FFetchRows > MaxAutoFetchRows then
              FFetchRows := MaxAutoFetchRows;
          end;

          if not Fetch then
            break;
        end;
    finally
      FFetching := False;
      if OldFetchRows <> FFetchRows then begin
        FreeFetchBuffer;
        FFetchRows := OldFetchRows;
      end;
    end;
  except
    // earlier we used to restore old cursor state but it does not lead to
    // any positive results
    FCommand.SetCursorState(csInactive);
    raise;
  end;
end;

procedure TOCIRecordSet.DoFetchAllPulse;
begin
{$IFDEF MSWINDOWS}
  PulseEvent(THandle(hEvent.Handle));
  try
{$ENDIF}
    DoFetchAll;
{$IFDEF MSWINDOWS}
  finally
    hEvent.SetEvent;
  end;
{$ENDIF}
end;

procedure TOCIRecordSet.EndFetchAll(E: Exception);
var
  Fail: boolean;
begin
{$IFDEF MSWINDOWS}
  if hFetchAllThread <> nil then begin
//    FConnection.StopThread(hFetchAllThread);
    hFetchAllThread := nil;
  end
  else
    if (FCommand = nil) or GetNonBlocking then Exit;
{$ENDIF}
  if GetNonBlocking and (IndexFieldCount > 0) then
    SortItems;

  if Assigned(FAfterFetchAll) then
    FAfterFetchAll(E = nil);

  if FCommand.FNonBlocking and (E is EOraError) then begin
    Fail := True;
    FConnection.DoError(EOraError(E), Fail);
    if not Fail then
      Abort;
  end;
end;

procedure TOCIRecordSet.FetchAll;
begin
  if (FCommand.GetCursorState < csFetchingAll) and FCommand.GetActive then begin
    if (FCommand.GetCursorState < csExecuting) then
      FCommand.SetCursorState(csExecuteFetchAll)
    else
      FCommand.SetCursorState(csFetchingAll);

    FWaitForFetchBreak := False;

    if FCommand.FNonBlocking then begin
      if not FConnection.FThreadSafety then
        raise Exception.Create(SNeedThreadSafety);
    {$IFDEF MSWINDOWS}
      hEvent.ResetEvent;
      hFetchAllThread := FConnection.RunThread(DoFetchAllPulse, EndFetchAll);
    {$ENDIF}
    end
    else
      DoFetchAll;
  end;
end;

procedure TOCIRecordSet.WaitForFetch;
{$IFDEF MSWINDOWS}
var
  AMsg: TMSG;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if (IntPtr(CurrentItem)= nil) or (IntPtr(CurrentItem.Next) = nil) then begin
    WaitForSingleObject(THandle(hEvent.Handle), INFINITE);
    while hExecFetchThread <> nil do begin
      if PeekMessage(AMsg, hODACWindow, WM_ENDTHREAD, WM_ENDTHREAD, PM_REMOVE) then
        DispatchMessage(AMsg);
      if PeekMessage(AMsg, hODACWindow, WM_AFTERFETCH, WM_AFTERFETCH, PM_REMOVE) then
        DispatchMessage(AMsg);
    end;
  end;
{$ENDIF}
end;

function TOCIRecordSet.RowsReturn: boolean;
begin
  if FCommand.CommandType <> ctUnknown then
    Result := inherited RowsReturn
  else                              //we need to know this info even if CommandType is not set(TCustomDADataSet.DoAfterExecute)
    Result := FCommand.RowsReturn;
end;

{$IFNDEF LITE}
function TOCIRecordSet.IsEncryptableDataType(DataType: Word): boolean;
begin
  Result := (DataType in [dtOraBlob, dtOraClob, dtWideOraClob]) or
    inherited IsEncryptableDataType(DataType);
end;

function TOCIRecordSet.GetDecryptDataType(DataType: Word): Word;
begin
  case DataType of
    dtOraBlob:
      Result := dtBlob;
    dtOraClob:
      Result := dtMemo;
    dtWideOraClob, dtNClob:
      Result := dtWideMemo;
    else
      Result := inherited GetDecryptDataType(DataType);
  end;
end;
{$ENDIF}

{ Navigation }

procedure TOCIRecordSet.SetToBegin;
begin
  inherited;

  if CanFetchBack and (FFetchStart <> 0) then begin
    FFetchAbsolute := True;
//    FirstItem := nil;
    FreeAllItems;
    FFetchEnd := 0;
  end;
end;

procedure TOCIRecordSet.SetToEnd;
begin
  StartWait;
  try
    if GetNonBlocking and FFetchAll then begin
      while FCommand.GetCursorState = csFetchingAll do sleep(0);
    end
    else
      if FCommand.GetCursorState < csFetched then
        if CanFetchBack then begin
          if not ((FFetchEnd = FCommand.FFetchedRows) and FNoCountData) then begin
            FCommand.InternalFetch(1, OCI_FETCH_LAST);
            FNoCountData := True;
  //          LastItem := nil;
            FreeAllItems;
            FFetchStart := FCommand.FFetchedRows;
          end;
        end
        else begin
          FetchAll;
          while FCommand.GetCursorState = csFetchingAll do sleep(0);
        end;
  finally
    StopWait;
  end;

  inherited;
end;

procedure TOCIRecordSet.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TOCIConnection(Value);
end;

function TOCIRecordSet.GetNonBlocking: boolean;
begin
  Result := FCommand.FNonBlocking;
end;

function TOCIRecordSet.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

function TOCIRecordSet.GetDisconnectedMode: boolean;
begin
  if FConnection <> nil then
    FDisconnectedMode := FConnection.DisconnectedMode;
  Result := FDisconnectedMode;
end;

function TOCIRecordSet.GetUseUnicode: boolean;
begin
  if FConnection <> nil then
    FUseUnicode := FConnection.FUseUnicode;
  Result := FUseUnicode;
end;

function TOCIRecordSet.GetCharLength: integer;
begin
  if FConnection <> nil then
    FCharLength := FConnection.FCharLength;
  Result := FCharLength;
end;

procedure TOCIRecordSet.Check(Status: Word);
begin
  OCI8.Check(Status, OCISvcCtx)
end;

function TOCIRecordSet.GetOCI7: TOCI7API;
begin
  Result := FOCISvcCtx.OCI7;
end;

function TOCIRecordSet.GetOCI8: TOCI8API;
begin
  Result := FOCISvcCtx.OCI8;
end;

function TOCIRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout: begin
      Assert(FCommand <> nil);
      FCommand.FCommandTimeout := Value;
    end;
    prAutoClose:
      FAutoClose := Boolean(Value);
    prDeferredLobRead:
      FDeferredLobRead := Boolean(Value);
    prFieldsAsString:
      FCommand.FFieldsAsString := Boolean(Value);
    prHideRowId:
      FHideRowId := Boolean(Value);
    prOpenNext:
      FCommand.FOpenNext := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCIRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout: begin
      Assert(FCommand <> nil);
      Value := FCommand.FCommandTimeout;
    end;
    prAutoClose:
      Value := FAutoClose;
    prDeferredLobRead:
      Value := FDeferredLobRead;
    prHasObjectFields:
      Value := FHasObjectFields;
    prHideRowId:
      Value := FHideRowId;
    prOpenNext:
      Value := FCommand.FOpenNext;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TOCIRecordSet.GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean);
var
  DataSize: Integer;
begin
  case Field.DataType of
    dtFixedChar:
      if NeedConvertEOL then
        DataLen := AddCRString(DataBuf, DataLen, Dest, Field.Length)
      else begin
        Move(DataBuf^, Dest^, DataLen);
        Marshal.WriteByte(Dest, DataLen, 0);
      end;
    dtFixedWideChar:
      if NeedConvertEOL then
        DataLen := AddCRUnicode(DataBuf, DataLen, Dest, Field.Length)
      else begin
        DataSize := DataLen * SizeOf(WideChar);
        Move(DataBuf^, Dest^, DataSize);
        Marshal.WriteInt16(Dest, DataSize, 0);
      end
  else
    inherited GetFieldData(Field, DataBuf, DataLen, Dest, NeedConvert);
  end;
end;

procedure TOCIRecordSet.GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType: Word; SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean);
var
  ObjPtr: IntPtr;
begin
  ObjPtr := Marshal.ReadIntPtr(DataBuf);
  case DataType of
    dtOraBlob:
      inherited GetDataAsVariant(DataBuf, DataLen, dtBlob, SubDataType, HasParent, IsFixed, Value, UseRollback);
    dtOraClob:
      inherited GetDataAsVariant(DataBuf, DataLen, dtMemo, SubDataType, HasParent, IsFixed, Value, UseRollback);
    dtWideOraClob:
      inherited GetDataAsVariant(DataBuf, DataLen, dtWideMemo, SubDataType, HasParent, IsFixed, Value, UseRollback);
  {$IFNDEF FPC}
    dtSQLTimeStamp:
      Value := VarSQLTimeStampCreate(PSQLTimeStamp(DataBuf)^);
  {$ENDIF}
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
      Value := TOraTimeStamp(GetGCHandleTarget(ObjPtr)).AsDateTime;
    dtIntervalYM, dtIntervalDS:
      Value := TOraInterval(GetGCHandleTarget(ObjPtr)).AsString;
    dtNumber:
      Value := TOraNumber(GetGCHandleTarget(ObjPtr)).AsFloat;
    dtXML:
      Value := TOraXML(GetGCHandleTarget(ObjPtr)).AsString;
    dtAnyData:
      Value := TOraAnyData(GetGCHandleTarget(ObjPtr)).AsVariant;
  else
    inherited;
  end
end;

procedure TOCIRecordSet.PutDataAsVariant(DataBuf: IntPtr; DataLenPtr: PWord; DataType: Word; Length, Scale: Word; HasParent: boolean; const Value: variant; IsDatabaseValue: boolean);
var
  ObjPtr: IntPtr;
begin
  case DataType of
    dtNumber: begin
      ObjPtr := Marshal.ReadIntPtr(DataBuf);
      case VarType(Value) of
        varSmallint, varInteger, varByte:
          TOraNumber(GetGCHandleTarget(ObjPtr)).AsInteger := Value;
        varSingle, varDouble, varCurrency:
          TOraNumber(GetGCHandleTarget(ObjPtr)).AsString := Value;
      else
        if VarIsStr(Value) then
          TOraNumber(GetGCHandleTarget(ObjPtr)).AsString := Value
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    end;
  else
    inherited;
  end
end;

function TOCIRecordSet.FieldListDependsOnParams: boolean;
begin
  Result := (FCommand <> nil) and
            (FCommand.FSQLType = SQL_PLSQL) and
            (HasCursorParams or HasResultSets);
end;

procedure TOCIRecordSet.GetBookmark(Bookmark: PRecBookmark);
var
  ItemSize: integer;
begin
  if CanFetchBack then
//    with Bookmark^ do
    begin
      Bookmark.Item := CurrentItem;
      if IntPtr(CurrentItem) <> nil then begin
        if FPieceFetch then
          Bookmark.Order := GetItemFetchPos(CurrentItem)
        else begin
          Bookmark.RefreshIteration := GetBlockFetchPos(CurrentItem.Block);
          ItemSize := RecordSize + sizeof(TItemHeader);
          Bookmark.Order := (PtrSubstract(CurrentItem, CurrentItem.Block) - SizeOf(TBlockHeader)) div ItemSize;
        end;
      end;
    end
  else
    inherited;
end;

procedure TOCIRecordSet.SetToBookmark(Bookmark: PRecBookmark);
var
  AFetchStart: integer;
  i: integer;

  function SetCurrentItem(Block: PBlockHeader; ABlockFetchStart: integer): boolean;
  var
    ItemSize: integer;
  begin
    if IntPtr(Block) <> nil then
      Result := (AFetchStart >= ABlockFetchStart) and (AFetchStart < ABlockFetchStart + Block.UsedItems)
    else
      Result := False;
    if Result then begin
      ItemSize := RecordSize + sizeof(TItemHeader);
      CurrentItem := PtrOffset(Block, SizeOf(TBlockHeader) +
        (AFetchStart - ABlockFetchStart) * ItemSize);
      Assert(CurrentItem.Flag = flUsed);
    end
  end;

begin
  if CanFetchBack then
    if (IntPtr(Bookmark) <> nil) and (IntPtr(Bookmark.Item) <> nil) then begin
      FBOF := False;
      FEOF := False;
      if FPieceFetch then begin
        if (Bookmark.Item.Flag = flUsed) and (Bookmark.Order = GetItemFetchPos(Bookmark.Item)) then
          CurrentItem := Bookmark.Item
        else begin
          if (Bookmark.Order >= FFetchStart - (BlockMan.DefaultItemCount shr 1)) and
             (Bookmark.Order < FFetchEnd + (BlockMan.DefaultItemCount shr 1))
          then begin
            if Bookmark.Order < FFetchStart then begin
              for i := FFetchStart + 1 downto Bookmark.Order do
                Fetch(True);
              CurrentItem := FirstItem;
            end
            else
            if Bookmark.Order > FFetchEnd then begin
              for i := FFetchEnd to Bookmark.Order do
                Fetch;
              CurrentItem := LastItem;
            end
          end
          else begin
            FreeAllItems;
            FFetchAbsolute := True;
            FFetchStart := Bookmark.Order;
            FFetchEnd := Bookmark.Order;
            FirstItem := nil;
            LastItem := nil;
            Fetch;
            CurrentItem := FirstItem;
          end
        end
      end
      else begin
        if (Bookmark.Item.Flag = flUsed) and (Bookmark.RefreshIteration = GetBlockFetchPos(Bookmark.Item.Block)) then
          CurrentItem := Bookmark.Item
        else begin
          AFetchStart := Bookmark.RefreshIteration + Bookmark.Order;
          if (AFetchStart >= FFetchStart - FFetchRows) and (AFetchStart < FFetchEnd + FFetchRows) then begin
            if AFetchStart < FFetchStart then
              Fetch(True)
            else
            if AFetchStart > FFetchEnd then
              Fetch;

            if not SetCurrentItem(BlockMan.FirstBlock, FFetchEnd - BlockMan.FirstBlock.UsedItems) then
              SetCurrentItem(BlockMan.FirstBlock.Next, FFetchStart);
          end
          else begin
            FreeAllItems;
            FFetchAbsolute := True;
            FFetchStart := AFetchStart;
            FFetchEnd := AFetchStart;
            FirstItem := nil;
            LastItem := nil;
            Fetch;
            CurrentItem := FirstItem;
          end;
          if IntPtr(FirstItem) <> nil then
            FirstItem.Prev := nil;  // remove cycle link
          if IntPtr(LastItem) <> nil then
            LastItem.Next := nil;
        end;
      end;
    end
    else
      CurrentItem := nil
  else
    inherited;
end;

function TOCIRecordSet.GetBlockFetchPos(Block: PBlockHeader): integer;
begin
  if Block = BlockMan.FirstBlock then
    Result := FFetchEnd - BlockMan.FirstBlock.UsedItems
  else
    Result := FFetchStart
end;

function TOCIRecordSet.GetItemFetchPos(Item: PItemHeader): integer;
begin
  Result := FFetchStart;
  while (IntPtr(Item) <> nil) and (IntPtr(Item) <> IntPtr(FirstItem)) do begin
    Item := Item.Prev;
    Inc(Result);
  end;
end;

{ TOraLob }

constructor TOraLob.Create(AOCISvcCtx: TOCISvcCtx);
begin
  inherited Create;

  FOCISvcCtx := AOCISvcCtx;
  phLobLocator := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(phLobLocator, nil);
  FCached := True;
  FCharsetID := 0;
  FCharsetForm := 0;
end;

destructor TOraLob.Destroy;
begin
  FreeLob;
  Marshal.FreeHGlobal(phLobLocator);

  inherited;
end;

procedure TOraLob.SetOCISvcCtx(const Value: TOCISvcCtx);
begin
  FOCISvcCtx := Value;
end;

function TOraLob.GetOCI8: TOCI8API;
begin
  Result := FOCISvcCtx.OCI8;
end;

procedure TOraLob.AllocLob;
var
  LOBLocator: pOCILOBLocator;
begin
  if hLOBLocator = nil then begin
    LOBLocator := hLOBLocator;
    Check(OCI8.OCIDescriptorAlloc(OCISvcCtx.hOCIEnv, LOBLocator, OCI_DTYPE_LOB, 0, nil));
    hLOBLocator := LOBLocator;
    FNativeHandle := True;
  end;
end;

procedure TOraLob.FreeLob;
begin
  FreeBlob;
end;

procedure TOraLob.Disconnect;
begin
  FreeBlob;
end;

function TOraLob.CreateClone: TBlob;
begin
  Result := TOraLob.Create(FOCISvcCtx);

  TOraLob(Result).LobType := LobType;
  TOraLob(Result).FCharLength := FCharLength;
  TOraLob(Result).FCharsetId := FCharsetId;
  TOraLob(Result).FCharsetForm := FCharsetForm;
  TOraLob(Result).IsUnicode := IsUnicode;
end;

procedure TOraLob.FreeBlob;
begin
  if IntPtr(hLOBLocator) <> nil then
    if FNativeHandle then
      if IsTemporary and IsInit then
        Check(OCI8.OCILobFreeTemporary(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator));
  if IntPtr(hLOBLocator) <> nil then begin
    if FNativeHandle then
      Check(OCI8.OCIDescriptorFree(hLOBLocator, OCI_DTYPE_LOB));
    hLOBLocator := nil;
    FCharsetForm := 0;
    FNativeHandle := False;
  end;
end;

procedure TOraLob.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    FOCISvcCtx.OCI8.DoOraError(Status, FOCISvcCtx);
end;

procedure TOraLob.CheckValue;
begin
  if FNeedReadLob then
    ReadLob;
end;

function TOraLob.GetSize: Cardinal;
begin
  if FNeedReadLob and (FCharLength > 1) and not FIsUnicode then
    ReadLob; // for multibyte charset LengthLob * CharSize is incorrect

  if FNeedReadLob then begin
    Result := LengthLob * CharSize;
    // reset FNeedReadLob to avoid multiple LengthLob calls on empty LOB
    if Result = 0 then
      FNeedReadLob := False;
  end
  else
    Result := inherited GetSize;
end;

function TOraLob.GetSizeAnsi: Cardinal;
begin
  Assert(FIsUnicode);
  if FNeedReadLob then begin
    if not SysLocale.FarEast then begin
      Result := LengthLob;
      // reset FNeedReadLob to avoid multiple LengthLob calls on empty LOB
      if Result = 0 then
        FNeedReadLob := False;
      Exit;
    end;
    ReadLob;
  end;

  Result := inherited GetSizeAnsi;
end;

function TOraLob.GetSizeUni: Cardinal;
begin
  Assert(not FIsUnicode);
  if FNeedReadLob then begin
    if not SysLocale.FarEast then begin
      Result := LengthLob;
      // reset FNeedReadLob to avoid multiple LengthLob calls on empty LOB
      if Result = 0 then
        FNeedReadLob := False;
      Exit;
    end;
    ReadLob;
  end;

  Result := inherited GetSizeUni;
end;

procedure TOraLob.CheckAlloc;
begin
  if hLOBLocator = nil then
    RaiseError(SLobNotAllocatted);
end;

procedure TOraLob.CheckSession;
begin
  if FOCISvcCtx = nil then
    RaiseError(SSvcCtxNotDefined);
end;

procedure TOraLob.CheckInit;
begin
  if not IsInit then
    RaiseError(SLobNotInited);
end;

procedure TOraLob.Init;
var
  Value: Integer;
begin
  Value := 0;
  Check(OCI8.OCIAttrSet2(hLOBLocator, OCI_DTYPE_LOB, Value, 0, OCI_ATTR_LOBEMPTY, OCISvcCtx.hOCIError));
end;

procedure TOraLob.CreateTemporary(LobType: TLobType);

//  // OCILobCreateTemporary can not create NCLOB locator
//  procedure PLSQLCreateTemporary;
//  var
//    Connection: TOCIConnection;
//    Command: TOCICommand;
//    TempLob: TOraLob;
//  begin
//    Connection := TOCIConnection.Create;
//    try
//      Connection.SetOCISvcCtx(FOCISvcCtx);
//      Command := TOCICommand.Create;
//      try
//        Command.SetConnection(Connection);
//        Command.SetProp(prAutoCommit, False);
//        Command.SetSQL('begin dbms_lob.createtemporary(:TempClob, False); end;');
//        with Command.Params[0] as TOraParamDesc do begin
//          SetDataType(dtOraClob);
//          SetParamType(pdInput);
//          SetNational(True);
//          TempLob := TOraLob.Create(OCISvcCtx);
//          SetObject(TempLob);
//          TempLob.Release;
//        end;
//        Command.Execute();
//
//        OCILobLocator := TempLob.hLobLocator;
//        TempLob.FNativeHandle := False;
//        FNativeHandle := True;
//      finally
//        Command.Free;
//      end;
//    finally
//      Connection.Free;
//    end;
//  end;

var
  OCIlobType: byte;
  CharsetForm: Integer;
begin
  OCISvcCtx.Home.CheckOCI81;
  FreeLob;

  FLobType := LobType;
  case FLobType of
    ltBlob: begin
      OCIlobType := OCI_TEMP_BLOB;
      FCharsetForm := SQLCS_IMPLICIT
    end;
    ltClob: begin
      OCIlobType := OCI_TEMP_CLOB;
      FCharsetForm := SQLCS_IMPLICIT;
    {$IFNDEF ONLY_UTF8_MEMO}
      if OCISvcCtx.UnicodeEnv then
        IsUnicode := True;
    {$ENDIF}
    end;
    ltNClob: begin
      OCIlobType := OCI_TEMP_CLOB;
      FCharsetForm := SQLCS_NCHAR;
    {$IFNDEF ONLY_UTF8_MEMO}
      if OCISvcCtx.UnicodeEnv then
        IsUnicode := True;
    {$ENDIF}
    end;
  else
    raise Exception.Create('Invalid LOB type.');
  end;

  if OCISvcCtx.Home.OCIVersion >= 9000 then
    CharsetForm := FCharsetForm
  else
    CharsetForm := OCI_DEFAULT;

  AllocLob;
  Check(OCI8.OCILobCreateTemporary(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, 
    OCI_DEFAULT, CharsetForm, OCIlobType, tbool(FCached), OCI_DURATION_SESSION));
end;

procedure TOraLob.FreeTemporary;
begin
  FreeBlob;
end;

function TOraLob.IsTemporary: LongBool;
begin
  // OCILobIsTemporary is supported in Oracle 8.1.7
  if OCISvcCtx.Home.OCIVersion >= 8100 then
    Check(OCI8.OCILobIsTemporary(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, hLOBLocator, Result))
  else
    Result := False;
end;

function TOraLob.IsInit: boolean;
var
  Res: tbool;
begin
  CheckAlloc;
  Check(OCI8.OCILobLocatorIsInit(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, hLOBLocator, Res));
  Result := Res <> 0;
end;

function TOraLob.LengthLob: Cardinal;
var
  Res: Cardinal;
begin
  CheckAlloc;
  CheckSession;
  if IsInit then begin
    Check(OCI8.OCILobGetLength(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, Res));
    Result := Res;
  end
  else
    Result := 0;
end;

procedure TOraLob.CheckCharSetForm;
var
  CharsetForm: ub1;
begin
  if FCharsetForm = 0 then begin
    Check(OCI8.OCILobCharSetForm(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, hLOBLocator, CharsetForm));
    FCharsetForm := CharsetForm;
    if CharsetForm = SQLCS_NCHAR then
      FLobType := ltNClob;
  end;
end;

function TOraLob.CharSize: Byte;
begin
  if FIsUnicode then
    Result := 2
  else
    Result := 1;
end;

procedure TOraLob.ReadLob;
var
  SharedPiece: PPieceHeader;
begin
  SharedPiece := nil;
  try
    ReadLob(SharedPiece, -1);
  finally
    if IntPtr(SharedPiece) <> nil then
      Marshal.FreeHGlobal(SharedPiece);
  end;
end;

procedure TOraLob.ReadLob(var SharedPiece: PPieceHeader; PrefetchLobSize: Integer);
var
  Piece: PPieceHeader;
  BufLen: cardinal;
  Buflen_Bytes: Int64;
  Buflen_Chars: Int64;
  Res: integer;
  CharsetId: word;
  PieceState: Byte;
  PrefetchedLength: Integer;
  CurPieceSize: Integer;
begin
  FNeedReadLob := False;

  CheckAlloc;
  CheckSession;

  FData.Truncate(0);
  PieceState := OCI_FIRST_PIECE;
  if IsInit then begin
    CheckCharSetForm;
    if FIsUnicode then
      CharsetId := OCI_UTF16ID
    else
      CharsetId := FCharsetId;

    // if PrefetchLobSize < 0 then parameters and prefetch is not allowed
    if PrefetchLobSize >= 0 then
      PrefetchedLength := LengthLob // get prefetched length
    else if OCISvcCtx.Home.Direct then
      PrefetchedLength := LengthLob
    else
      PrefetchedLength := 0;

    CurPieceSize := LargePieceSize;
    if PrefetchedLength < CurPieceSize then begin
      CurPieceSize := PieceSize;
      if PrefetchedLength > CurPieceSize then
        CurPieceSize := PrefetchedLength;
    end;

    repeat
      if IntPtr(SharedPiece) = nil then
        AllocPiece(Piece, CurPieceSize)
      else if CurPieceSize > SharedPiece.Size then
        AllocPiece(Piece, CurPieceSize)
      else
        Piece := SharedPiece;

      try
        if (OCISvcCtx.Home.OCIVersion >= 10000) then begin
          // Lob prefetch workaround for Oracle 11g and higher
          if (OCISvcCtx.Home.OCIVersion >= 11000) and
             (PrefetchLobSize > 0) and
             (PieceState = OCI_FIRST_PIECE)
          then begin
            if PrefetchedLength <= PrefetchLobSize then begin
              Buflen_Bytes := PrefetchedLength;
              Buflen_Chars := PrefetchedLength;
            end
            else begin
              Buflen_Bytes := $FFFFFFFF;
              Buflen_Chars := 0;
            end;
          end
          else begin
            Buflen_Bytes := $FFFFFFFF;
            Buflen_Chars := 0;
          end;

          Res := OCI8.OCILobRead2(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, Buflen_Bytes, Buflen_Chars, 1,
            PtrOffset(Piece, Sizeof(TPieceHeader)), CurPieceSize, PieceState, nil, nil, CharsetId, FCharsetForm);
          PieceState := OCI_NEXT_PIECE;
          Piece.Used :=  Buflen_Bytes;

          BufLen := Buflen_Bytes;
        end
        else begin
          BufLen := 0;
          Res := OCI8.OCILobRead(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, BufLen, 1,
            PtrOffset(Piece, Sizeof(TPieceHeader)), CurPieceSize, nil, nil, CharsetId, FCharsetForm);
          if FLobType = ltBlob then
            Piece.Used := BufLen
          else
            Piece.Used := BufLen * CharSize;
        end;

        if Res <> OCI_NEED_DATA then
          Check(Res);
      except
        if IntPtr(Piece) <> IntPtr(SharedPiece) then
          FreePiece(Piece);
        raise;
      end;

      if BufLen = 0 then begin  // 804 server and any another client
        if IntPtr(Piece) <> IntPtr(SharedPiece) then
          FreePiece(Piece);
        FNeedReadLob := False;

        if FirstPiece = nil then
          FData.Write(0, 0, nil); // set is not null

        Exit;
      end;

      if Piece = SharedPiece then begin
        if SharedPiece.Used < SharedPiece.Size shr 1 then begin
          AllocPiece(Piece, SharedPiece.Used);
          CopyBuffer(PtrOffset(SharedPiece, SizeOf(TPieceHeader)), PtrOffset(Piece, SizeOf(TPieceHeader)), SharedPiece.Used);
          Piece.Used := SharedPiece.Used;
        end
        else
          SharedPiece := nil;
      end
      else if Piece.Used < Piece.Size shr 1 then
        CompressPiece(Piece);

      AppendPiece(Piece);
    until Res = OCI_SUCCESS;
  end;

  FNeedReadLob := False;
  FCached := True;
end;

procedure TOraLob.WriteLob;

  function AllocBrokenBuffer: IntPtr;
  var
    Piece: PPieceHeader;
    MaxPieceSize: Integer;
  begin
    MaxPieceSize := 0;
    Piece := FirstPiece;
    while Piece <> nil do begin
      if Piece.Used > MaxPieceSize then
        MaxPieceSize := Piece.Used;
      Piece := Piece.Next;
    end;

    if MaxPieceSize > 0 then
      Result := Marshal.AllocHGlobal(MaxPieceSize + 5)
    else
      Result := nil;
  end;

var
  Piece: PPieceHeader;
  BufLen: cardinal;
  Offset: cardinal;
  CharsetId: word;
  CharCount: ub4;
  BufPtr: IntPtr;
  BrokenLen: Cardinal;
  BrokenBuf: IntPtr;
begin
  CheckAlloc;
  CheckSession;
  CheckInit;
  CheckCharSetForm;

  if not IsTemporary then
    Check(OCI8.OCILobTrim(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, 0));

  Defrag($F000); // System.Classes.TStream.CopyFrom : MaxBufSize = $F000
  Piece := FirstPiece;
  Offset := 1;
  BrokenLen := 0;
  BrokenBuf := nil;
  try
    while IntPtr(Piece) <> nil do begin
      if BrokenLen = 0 then begin
        BufPtr := PtrOffset(Piece, Sizeof(TPieceHeader));
        BufLen := Piece.Used;
      end
      else begin
        BufPtr := BrokenBuf;
        Move(PtrOffset(Piece, Sizeof(TPieceHeader))^, PtrOffset(BufPtr, BrokenLen)^, Piece.Used);
        BufLen := Cardinal(Piece.Used) + BrokenLen;
      end;

      if FIsUnicode then begin
        CharsetId := OCI_UTF16ID;

        if FLobType <> ltBlob then
          CharCount := BufLen div CharSize
        else
          CharCount := BufLen;
      end
      else begin
        if (FLobType <> ltBlob) and OCISvcCtx.UnicodeEnv then
          raise Exception.Create(SClobMustBeUnicode);

        CharsetId := FCharsetId;

        // UTF8
        if (FLobType <> ltBlob) and (CharsetId >= 870) and (CharsetId <= 873) then begin
          BrokenLen := DetectUtf8LastBrockenChar(BufPtr, BufLen);
          BufLen := BufLen - BrokenLen;
        end;

        CharCount := BufLen;
      end;

      Check(OCI8.OCILobWrite(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, CharCount, Offset,
        BufPtr, BufLen, OCI_ONE_PIECE, nil, nil, CharsetId, FCharsetForm));

      if BrokenLen > 0 then begin
        if BrokenBuf = nil then
          BrokenBuf := AllocBrokenBuffer;
        Move(PtrOffset(BufPtr, BufLen)^, BrokenBuf^, BrokenLen);
      end;

      Piece := Piece.Next;
      Inc(Offset, CharCount);
    end;
  finally
    if BrokenBuf <> nil then
      Marshal.FreeHGlobal(BrokenBuf);
  end;
end;

function TOraLob.Read(Position, Count: cardinal; Dest: IntPtr): cardinal;
var
  BytesRead: cardinal;
  BytesRead64: Int64;
  CharsRead: Int64;
  Res: integer;
  CharsetId: word;
begin
  Result := 0;
  if not Cached then begin
    CheckAlloc;
    CheckSession;
    if IsInit then begin
      BytesRead := Count; // to avoid OCI_NEED_DATA error
      CheckCharSetForm;

      if BytesRead = 0 then
        BytesRead := LengthLob;

      if Count = 0 then
        Count := Size;

      if FIsUnicode then
        CharsetId := OCI_UTF16ID
      else
        CharsetId := 0;

      if (OCISvcCtx.Home.OCIVersion >= 10000) then begin
        BytesRead64 := BytesRead;
        CharsRead := BytesRead;

        Res := OCI8.OCILobRead2(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, BytesRead64, CharsRead, Position + 1,
          Dest, Count, OCI_FIRST_PIECE, nil, nil, CharsetId, FCharsetForm);
        BytesRead := CharsRead;
        Result := BytesRead64;
      end
      else begin
        Res := OCI8.OCILobRead(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, BytesRead, Position + 1,
          Dest, Count, nil, nil, CharsetId, FCharsetForm);
        Result := BytesRead * CharSize;
      end;

      if Res <> OCI_NEED_DATA then
        Check(Res);
    end;
  end
  else
    Result := inherited Read(Position, Count, Dest);
end;

procedure TOraLob.Write(Position, Count: cardinal; Source: IntPtr);
begin
  Cached := True;

  inherited Write(Position, Count, Source);
end;

procedure TOraLob.Clear;
begin
  FNeedReadLob := False;

  inherited Clear;
end;

procedure TOraLob.Truncate(NewSize: Cardinal);
begin
  if NewSize = 0 then
    FNeedReadLob := False
  else
    CheckValue;

  inherited Truncate(NewSize);
end;

{procedure TLOBLocator.TrimLob(Size: Integer);
begin
  CheckAlloc;

  EnableBuffering;
  Check(OCI8.OCILobTrim(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator, Size));
  DisableBuffering;
end;}

procedure TOraLob.EnableBuffering;
begin
  CheckAlloc;
  CheckSession;

  Check(OCI8.OCILobEnableBuffering(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator));
end;

procedure TOraLob.DisableBuffering;
begin
  CheckAlloc;
  CheckSession;

  Check(OCI8.OCILobDisableBuffering(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLOBLocator));
end;

function TOraLob.GethOCILobLocator: pOCIStmt;
begin
  Result := Marshal.ReadIntPtr(phLobLocator);
end;

procedure TOraLob.SethOCILobLocator(Value: pOCIStmt);
begin
  Marshal.WriteIntPtr(phLobLocator, Value);
end;

function TOraLob.GetOCILobLocator: pOCILobLocator;
begin
  if hLobLocator = nil then
    AllocLob;

  Result := hLobLocator;
end;

procedure TOraLob.SetOCILobLocator(Value: pOCILobLocator);
begin
  FreeLob;
  hLobLocator := Value;
  if IntPtr(hLobLocator) <> nil then
    FNativeHandle := False;
end;

function TOraLob.GetOCILobLocatorPtr: ppOCILobLocator;
begin
  if hLobLocator = nil then
    AllocLob;

  Result := phLobLocator;
end;

procedure TOraLob.SetCached(const Value: boolean);
begin
  if FCached <> Value then begin
    if not Value and not IsNull then begin
      FData.Truncate(0);
      FNeedReadLob := True;
    end;
    FCached := Value;
  end;
end;

{ TOraFile }

destructor TOraFile.Destroy;
begin
{  if IsOpen then
    Close;}

  inherited;
end;

procedure TOraFile.AllocLob;
var
  LOBLocator: pOCILOBLocator;
begin
  if hLOBLocator = nil then begin
    LOBLocator := hLOBLocator;
    Check(OCI8.OCIDescriptorAlloc(OCISvcCtx.hOCIEnv, LOBLocator, OCI_DTYPE_FILE, 0, nil));
    hLOBLocator := LOBLocator;
    FNativeHandle := True;
  end;
end;

procedure TOraFile.FreeBlob;
begin
  if IntPtr(hLOBLocator) <> nil then begin
    if FNativeHandle then
      Check(OCI8.OCIDescriptorFree(hLOBLocator, OCI_DTYPE_FILE));
    hLOBLocator := nil;
    FNativeHandle := False;
  end;
end;

procedure TOraFile.Open;
begin
  CheckAlloc;
  CheckSession;

  if IsInit then
    Check(OCI8.OCILobFileOpen(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLobLocator, OCI_FILE_READONLY));
end;

procedure TOraFile.Close;
begin
  CheckAlloc;
  CheckSession;

  if IsInit then
    Check(OCI8.OCILobFileClose(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLobLocator));
end;

procedure TOraFile.EnableRollback;
begin
  FNeedRollback := True;
end;

procedure TOraFile.SaveToRollback;
begin
  FRollbackFileDir := GetFileDir;
  FRollbackFileName := GetFileName;

  CanRollback := True;
end;

procedure TOraFile.Commit;
begin
  CanRollback := False;
  FNeedRollback := False;
end;

procedure TOraFile.Cancel;
begin
  if CanRollback then begin
    FileName := FRollbackFileName;
    FileDir := FRollbackFileDir;
    if Cached then
      Refresh
    else if not IsNull then begin
      FData.Truncate(0);
      FNeedReadLob := True;
    end;
  end;
  CanRollback := False;
  FNeedRollback := False;
end;

procedure TOraFile.Refresh;
begin
  CheckAlloc;
  CheckSession;

  if not IsNull then begin
    FData.Truncate(0);

    if Exists then begin
      Open;
      try
        ReadLob;
      finally
        Close;
      end;
    end
    else
      Clear;
  end;
end;

function TOraFile.IsOpen: boolean;
var
  Res: tbool;
begin
  Result := False;
  if (IntPtr(hLobLocator) <> nil) and IsInit then begin
    CheckSession;
    Check(OCI8.OCILobFileIsOpen(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLobLocator, Res));
    Result := Res <> 0;
  end;
end;

function TOraFile.Exists: boolean;
var
  Val: tbool;
  Res: sword;
begin
  Result := False;
  if (IntPtr(hLobLocator) <> nil) and IsInit then begin
    CheckSession;

    if FileName = '' then
      Exit;

    Res := OCI8.OCILobFileExists(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, hLobLocator, Val);
    if Res = OCI_SUCCESS then
      Result := Val <> 0
    else
      if OCI8.GetOraError(Res, FOCISvcCtx) <> 22285 then  // dir or file not exist
        OCI8.DoOraError(Res, FOCISvcCtx);
  end;
end;

function TOraFile.GetFileDir: string;
var
  Len: ub2;
  hLen, Handle: IntPtr;
begin
  if (hLobLocator <> nil) and IsInit then begin
    Len := 255 * SizeOfCharOCI(OCISvcCtx.UnicodeEnv);
    Handle := Marshal.AllocHGlobal(Len);
    try
      hLen := @Len;
      Check(OCI8.OCILobFileGetName(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, hLobLocator, Handle, hLen, nil, nil));
      Result := PtrToStringOCI(Handle, Len, OCISvcCtx.UnicodeEnv);
    finally
      Marshal.FreeHGlobal(Handle);
    end;
    if Result = #0 then
      Result := '';
  end
  else
    Result := '';
end;

procedure TOraFile.SetFileDir(const Value: string);
begin
  SetFileDirAndName(AnsiUpperCase(Value), FileName);
end;

function TOraFile.GetFileName: string;
var
  Len: ub2;
  hLen: IntPtr;
  Handle: IntPtr;
begin
  if (IntPtr(hLobLocator) <> nil) and IsInit then begin
    Len := 255 * SizeOfCharOCI(OCISvcCtx.UnicodeEnv);
    Handle := Marshal.AllocHGlobal(Len);
    try
      hLen := @Len;
      Check(OCI8.OCILobFileGetName(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, hLobLocator, nil, nil, Handle, hLen));
      Result := PtrToStringOCI(Handle, Len, OCISvcCtx.UnicodeEnv);
    finally
      Marshal.FreeHGlobal(Handle);
    end;
    if Result = #0 then
      Result := '';
  end
  else
    Result := '';
end;

procedure TOraFile.SetFileName(const Value: string);
begin
  SetFileDirAndName(FileDir, Value);
end;

procedure TOraFile.SetFileDirAndName(const FileDir, FileName: string);
var
  DSize, NSize, Res: integer;
  p1, p2: IntPtr;
begin
  CheckAlloc;

  if FNeedRollback and not CanRollback then
    SaveToRollback;

  p1 := StringToHGlobalOCI(FileDir, DSize, OCISvcCtx.UnicodeEnv);
  if DSize = 0 then
    DSize := SizeOfCharOCI(OCISvcCtx.UnicodeEnv);

  p2 := StringToHGlobalOCI(FileName, NSize, OCISvcCtx.UnicodeEnv);
  if NSize = 0 then
    NSize := SizeOfCharOCI(OCISvcCtx.UnicodeEnv);

  try
    Res := OCI8.OCILobFileSetName(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, phLobLocator, p1, DSize, p2, NSize);
    Check(Res);
  finally
    FreeStringOCI(p1, OCISvcCtx.UnicodeEnv);
    FreeStringOCI(p2, OCISvcCtx.UnicodeEnv);
  end;
end;

procedure TOraFile.CheckValue;
begin
  if FNeedReadLob then
    if Exists then begin
      Open;
      try
        ReadLob;
      finally
        Close;
      end;
    end
    else
      Clear;
end;

function TOraFile.CharSize: Byte;
begin
  Result := 1;
end;

{ TOraTimeStamp }

constructor TOraTimeStamp.Create(ADataType: word);
begin
  inherited Create;

  FEnvironment := nil;
  FHandleType := htLocal;

  Init(ADataType);
end;

constructor TOraTimeStamp.Create(AOCIEnvironment: TOCIEnvironment; ADataType: word);
begin
  inherited Create;

  FEnvironment := AOCIEnvironment;
  if FEnvironment = nil then
    FHandleType := htLocal
  else begin
    FHandleType := htNative;
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.AddRef;
  {$ENDIF}
  end;

  Init(ADataType);
end;

constructor TOraTimeStamp.Create(AOCISvcCtx: TOCISvcCtx; ADataType: word);
begin
  inherited Create;

  if AOCISvcCtx <> nil then
    FEnvironment := AOCISvcCtx.Environment
  else
    FEnvironment := nil;
  if FEnvironment = nil then
    FHandleType := htLocal
  else begin
    FHandleType := htNative;
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.AddRef;
  {$ENDIF}
  end;

  Init(ADataType);
end;

destructor TOraTimeStamp.Destroy;
begin
  FreeDateTime;
  Marshal.FreeHGlobal(FOCIDateTimePtr);

  if FEnvironment <> nil then begin
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.ReleaseRef;
  {$ENDIF}
    FEnvironment := nil;
  end;

  inherited;
end;

procedure TOraTimeStamp.Init(ADataType: word);
begin
  FOCIDateTimePtr := Marshal.AllocHGlobal(sizeof(IntPtr));
  FOCIDateTimePtr^ := nil;
  FIndicatorPtr := @FIndicator;
  FIndicatorPtr^ := OCI_IND_NULL;
  FPrecision := 6;
  hOCIDateTime := nil;

  case ADataType of
    dtSQLTimeStamp, dtTimeStamp:
      FDescriptorType := OCI_DTYPE_TIMESTAMP;
    dtTimeStampTZ:
      FDescriptorType := OCI_DTYPE_TIMESTAMP_TZ;
    dtTimeStampLTZ:
      FDescriptorType := OCI_DTYPE_TIMESTAMP;
  else
    Assert(False);
  end;
end;

procedure TOraTimeStamp.SetEnvironment(AEnvironment: TOCIEnvironment);
var
  TempIsNull: boolean;
  TempValue: TOCIDateTime;
begin
  if FEnvironment <> AEnvironment then begin
    TempIsNull := GetIsNull;
    if not TempIsNull then
      TempValue := ToLocalDateTime
    else
      TempValue := nil;

    try
      FreeDateTime;

    {$IFNDEF AUTOREFCOUNT}
      if FEnvironment <> nil then
        FEnvironment.ReleaseRef;
    {$ENDIF}
      FEnvironment := AEnvironment;
      if FEnvironment <> nil then begin
        FHandleType := htNative;
      {$IFNDEF AUTOREFCOUNT}
        FEnvironment.AddRef;
      {$ENDIF}
      end
      else
        FHandleType := htLocal;

      SetIsNull(TempIsNull);
      if not TempIsNull then
        FromLocalDateTime(TempValue);
    finally
      TempValue.Free
    end;
  end;
end;

procedure TOraTimeStamp.AllocDateTime;
var
  OCIDateTime: pOCIDateTime;
begin
  if FHandleType = htLocal then begin
    if FLocalDateTime = nil then
      FLocalDateTime := TOCIDateTime.Create(FDescriptorType);
  end
  else if hOCIDateTime = nil then begin
    OCIDateTime := hOCIDateTime;
    Check(FEnvironment.Home.OCI8.OCIDescriptorAlloc(FEnvironment.hOCIEnv, OCIDateTime, FDescriptorType, 0, nil));
    hOCIDateTime := OCIDateTime;
    FHandleType := htNative;
  end;
end;

procedure TOraTimeStamp.FreeDateTime;
begin
  if FHandleType = htLocal then
    FreeAndNil(FLocalDateTime)
  else if IntPtr(hOCIDateTime) <> nil then begin
    if FHandleType = htNative then
      Check(FEnvironment.Home.OCI8.OCIDescriptorFree(hOCIDateTime, FDescriptorType));
    hOCIDateTime := nil;
    FIndicatorPtr^ := OCI_IND_NULL;
    FHandleType := htNative;
  end;
end;

function TOraTimeStamp.ToLocalDateTime: TOCIDateTime;
var
  Year: Smallint;
  Month, Day: Byte;
  Hour, Min, Sec: byte;
  FSec: cardinal;
  TZ: string;
begin
  Result := TOCIDateTime.Create(FDescriptorType);

  GetDate(Year, Month, Day);
  GetTime(Hour, Min, Sec, FSec);
  if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then
    TZ := GetTimeZone
  else
    TZ := '';

  Result.Construct(Year, Month, Day, Hour, Min, Sec, FSec, TZ);
end;

procedure TOraTimeStamp.FromLocalDateTime(Value: TOCIDateTime);
var
  Year: Smallint;
  Month, Day: Byte;
  Hour, Min, Sec: byte;
  FSec: cardinal;
  TZ: string;
begin
  Value.GetDate(Year, Month, Day);
  Value.GetTime(Hour, Min, Sec, FSec);
  if Value.DescriptorType = OCI_DTYPE_TIMESTAMP_TZ then
    TZ := Value.GetTimeZoneName
  else
    TZ := '';

  Construct(Year, Month, Day, Hour, Min, Sec, FSec, TZ);
end;

procedure TOraTimeStamp.Disconnect;
begin
  SetEnvironment(nil);
end;

function TOraTimeStamp.GetIsNull: boolean;
begin
  Result := (FIndicatorPtr^ = OCI_IND_NULL);
end;

procedure TOraTimeStamp.SetIsNull(Value: boolean);
begin
  if Value then
    FIndicatorPtr^ := OCI_IND_NULL
  else
    FIndicatorPtr^ := OCI_IND_NOTNULL;
end;

procedure TOraTimeStamp.Assign(Source: TOraTimeStamp);
begin
  Source.AssignTo(Self);
end;

procedure TOraTimeStamp.AssignTo(Dest: TOraTimeStamp);

  procedure LocalAssign;
  var
    Temp: TOCIDateTime;
  begin
    Temp := ToLocalDateTime;
    try
      Dest.FromLocalDateTime(Temp);
    finally
      Temp.Free;
    end;
  end;

begin
  if (Dest.DescriptorType <> OCI_DTYPE_TIMESTAMP_LTZ) and (DescriptorType <> OCI_DTYPE_TIMESTAMP) then
    Dest.DescriptorType := DescriptorType;

  Dest.AllocDateTime;
  Dest.Format := Format;
  Dest.Precision := Precision;
  Dest.FIndicatorPtr^ := FIndicatorPtr^;

  if not IsNull then
    if (Dest.FEnvironment = nil) and (FEnvironment = nil) then
      Dest.LocalDateTime.Assign(LocalDateTime)
    else if Dest.FEnvironment = FEnvironment then
      Check(FEnvironment.Home.OCI8.OCIDateTimeAssign(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIDateTime, Dest.OCIDateTime))
    else
      LocalAssign;
end;

function TOraTimeStamp.Compare(Dest: TOraTimeStamp): integer;

  function LocalCompare: Integer;
  var
    DateTime1, DateTime2: TOCIDateTime;
  begin
    DateTime1 := ToLocalDateTime;
    try
      DateTime2 := Dest.ToLocalDateTime;
      try
        Result := DateTime1.Compare(DateTime2);
      finally
        DateTime2.Free;
      end;
    finally
      DateTime1.Free;
    end;
  end;

begin
  if (Dest.FEnvironment = nil) and (FEnvironment = nil) then
    if Dest.IsNull or IsNull then
      raise Exception.Create(SDatetimesIncomparable)
    else
      Result := LocalDateTime.Compare(Dest.LocalDateTime)
  else if Dest.FEnvironment = FEnvironment then
    Check(FEnvironment.Home.OCI8.OCIDateTimeCompare(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIDateTime, Dest.OCIDateTime, Result))
  else
    if Dest.IsNull or IsNull then
      raise Exception.Create(SDatetimesIncomparable)
    else
      Result := LocalCompare;
end;

procedure TOraTimeStamp.SetDescriptorType(const Value: cardinal);
begin
  if FDescriptorType <> Value then begin
    FreeDateTime;
    FDescriptorType := Value;
  end;
end;

function TOraTimeStamp.GethOCIDateTime: pOCIDateTime;
begin
  Result := FOCIDateTimePtr^;
end;

procedure TOraTimeStamp.SethOCIDateTime(Value: pOCIDateTime);
begin
  FOCIDateTimePtr^ := Value;
end;

function TOraTimeStamp.GetOCIDateTime: pOCIDateTime;
begin
  AllocDateTime;
  Result := hOCIDateTime;
end;

procedure TOraTimeStamp.SetOCIDateTime(const Value: pOCIDateTime);
begin
  FreeDateTime;
  hOCIDateTime := Value;
  if IntPtr(Value) <> nil then begin
    FHandleType := htShared;
    IsNull := False;
  end;
end;

function TOraTimeStamp.GetOCIDateTimePtr: ppOCIDateTime;
begin
  AllocDateTime;
  Result := FOCIDateTimePtr;
end;

function TOraTimeStamp.GetLocalDateTime: TOCIDateTime;
begin
  AllocDateTime;
  Result := FLocalDateTime;
end;

procedure TOraTimeStamp.Construct(Year: smallint; Month, Day, Hour, Min,
  Sec: byte; FSec: cardinal; TimeZone: string);
var
  Res, BufSize: integer;
  p: IntPtr;
begin
  if FHandleType = htLocal then
    LocalDateTime.Construct(Year, Month, Day, Hour, Min, Sec, FSec, TimeZone)
  else begin
    if TimeZone <> '' then
      p := StringToHGlobalOCI(TimeZone, BufSize, FEnvironment.UnicodeEnv)
    else begin
      p := nil; // PChar('') means UTC
      BufSize := 0;
    end;
    Res := FEnvironment.Home.OCI8.OCIDateTimeConstruct(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIDateTime, Year, Month,
      Day, Hour, Min, Sec, FSec, p, BufSize);
    FreeStringOCI(p, FEnvironment.UnicodeEnv);
    Check(Res);
  end;

  FIndicatorPtr^ := OCI_IND_NOTNULL;
end;

function TOraTimeStamp.GetAsString: string;
var
  BufSize: ub4;
  FmtSize: integer;
  Ptr, FmtPtr: IntPtr;
begin
  if IsNull then begin // to avoid error on OCI function call
    Result := '';
    Exit;
  end;

  if FFormat = '' then
    SetFormat('');

  if FHandleType = htLocal then
    Result := LocalDateTime.ToString(format, Precision)
  else begin
    BufSize := 255 * SizeOfCharOCI(FEnvironment.UnicodeEnv);
    Ptr := Marshal.AllocHGlobal(BufSize);
    FmtPtr := StringToHGlobalOCI(FFormat, FmtSize, FEnvironment.UnicodeEnv);
    try
      try
        Check(FEnvironment.Home.OCI8.OCIDateTimeToText(FEnvironment.hOCIEnv, FEnvironment.hOCIError,
          OCIDateTime, FmtPtr, FmtSize, Precision, nil, 0, BufSize, Ptr));
        Result := PtrToStringOCI(Ptr, BufSize, FEnvironment.UnicodeEnv);
      except
        // Timestamp value is corrupted by a trigger and RETURNING INTO
        on E: EOraError do
          if E.ErrorCode = 1877 then
            Result := ''
          else
            raise;
        on E: EConvertError do // for Direct option
          Result := '';
      end;
    finally
      Marshal.FreeHGlobal(Ptr);
      FreeStringOCI(FmtPtr, FEnvironment.UnicodeEnv);
    end;
  end;
end;

procedure TOraTimeStamp.SetAsString(const Value: string);
var
  hTmp: pOCIDateTime;
  Buf: pOCIDateTime;
  p1, p2: IntPtr;
  VSize, FSize: integer;
begin
  // We need to backup timestamp value because OCIDateTimeFromText may corrupt
  // it if input string is wrong format.
  if Value = '' then begin
    FIndicatorPtr^ := OCI_IND_NULL;
    Exit;
  end;

  if FFormat = '' then
    SetFormat('');

  if FHandleType = htLocal then
    LocalDateTime.Parse(Value, FFormat)
  else begin
    hTmp := hOCIDateTime;
    hOCIDateTime := nil;
    p1 := StringToHGlobalOCI(Value, VSize, FEnvironment.UnicodeEnv);
    p2 := StringToHGlobalOCI(FFormat, FSize, FEnvironment.UnicodeEnv);
    try
      try
        Check(FEnvironment.Home.OCI8.OCIDateTimeFromText(FEnvironment.hOCIEnv, FEnvironment.hOCIError,
          p1, VSize, p2, FSize, nil, 0, OCIDateTime));
      except
        // restore valid timestamp value
        Buf := hTmp;
        hTmp := hOCIDateTime;
        hOCIDateTime := Buf;
        raise;
      end;
    finally
      FreeStringOCI(p1, FEnvironment.UnicodeEnv);
      FreeStringOCI(p2, FEnvironment.UnicodeEnv);
      if hTmp <> nil then
        FEnvironment.Home.OCI8.OCIDescriptorFree(hTmp, FDescriptorType);
    end;
  end;

  FIndicatorPtr^ := OCI_IND_NOTNULL;
end;

procedure TOraTimeStamp.GetDate(var Year: smallint; var Month, Day: byte);
begin
  if FHandleType = htLocal then
    LocalDateTime.GetDate(Year, Month, Day)
  else
    Check(FEnvironment.Home.OCI8.OCIDateTimeGetDate(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIDateTime, Year, Month, Day));
end;

procedure TOraTimeStamp.SetDate(Year: smallint; Month, Day: byte);
var
  Hour, Min, Sec: byte;
  FSec: cardinal;
  TZ: string;
begin
  GetTime(Hour, Min, Sec, FSec);

  if DescriptorType <> OCI_DTYPE_TIMESTAMP then
    // suppress exception raised when timezone is absent
    try
      TZ := TimeZone;
    except
      on EOraError do;
    end;

  Construct(Year, Month, Day, Hour, Min, Sec, FSec, TZ);
end;

procedure TOraTimeStamp.GetTime(var Hour, Min, Sec: byte; var FSec: cardinal);
begin
  if FHandleType = htLocal then
    LocalDateTime.GetTime(Hour, Min, Sec, FSec)
  else
    Check(FEnvironment.Home.OCI8.OCIDateTimeGetTime(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIDateTime, Hour, Min, Sec, FSec));
end;

procedure TOraTimeStamp.SetTime(Hour, Min, Sec: byte; FSec: cardinal);
var
  Year: smallint;
  Month, Day: byte;
  TZ: string;
begin
  GetDate(Year, Month, Day);
  if DescriptorType <> OCI_DTYPE_TIMESTAMP then
    TZ := TimeZone;
  Construct(Year, Month, Day, Hour, Min, Sec, FSec, TZ);
end;

procedure TOraTimeStamp.GetTimeZoneOffset(var TZHour, TZMin: shortint);
begin
  if FHandleType = htLocal then
    LocalDateTime.GetTimeZoneOffset(TZHour, TZMin)
  else
    Check(FEnvironment.Home.OCI8.OCIDateTimeGetTimeZoneOffset(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIDateTime, TZHour, TZMin));
end;

procedure TOraTimeStamp.SetTimeZoneOffset(TZHour, TZMin: shortint);
var
  Year: smallint;
  Month, Day, Hour, Min, Sec: byte;
  FSec: cardinal;
  TZ: string;
begin
  GetDate(Year, Month, Day);
  GetTime(Hour, Min, Sec, FSec);

  TZ := '';

  if TZHour >= 0 then
    TZ := TZ + '+';
  TZ := TZ + IntToStr(TZHour) + ':';

  if tzMin >= 0 then
    TZ := TZ + '+';

  TZ := TZ + IntToStr(TZMin);

  Construct(Year, Month, Day, Hour, Min, Sec, FSec, TZ);

  CheckValid;
end;

function TOraTimeStamp.GetTimeZone: string;
var
  BufSize: cardinal;
  Ptr: IntPtr;
begin
  if FHandleType = htLocal then
    Result := LocalDateTime.GetTimeZoneName
  else begin
    BufSize := 255;
    Ptr := Marshal.AllocHGlobal(BufSize);
    try
      Check(FEnvironment.Home.OCI8.OCIDateTimeGetTimeZoneName(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIDateTime, Ptr, BufSize));
      Result := string(Marshal.PtrToStringAnsi(Ptr, BufSize));
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end;
end;

procedure TOraTimeStamp.CheckValid;
var
  Valid: cardinal;
begin
  if FHandleType = htLocal then
    Valid := LocalDateTime.Check
  else begin
    Assert(hOCIDateTime <> nil);
    Valid := 0;
    Check(FEnvironment.Home.OCI8.OCIDateTimeCheck(FEnvironment.hOCIEnv, FEnvironment.hOCIError, hOCIDateTime, Valid));
  end;

  if Valid <> 0 then
    raise Exception.Create(SInvalidTimeStamp);
end;

procedure TOraTimeStamp.Check(Res: sword);
begin
  TOraError.Check(
    @FEnvironment.Home.OCI8.OCIErrorGet,
    Res,
    FEnvironment.UnicodeEnv,
    FEnvironment.hOCIError
  );
end;

procedure TOraTimeStamp.SetFormat(const AFormat: string);
begin
  if AFormat = '' then
    if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then
      FFormat := DefaultOCITimeStampWithTZFormat(Precision)
    else
      FFormat := DefaultOCITimeStampFormat(Precision)
  else
    FFormat := AFormat;
end;

function TOraTimeStamp.GetAsDateTime: TDateTime;
var
  Year: smallint;
  Month, Day, Hour, Min, Sec: byte;
  FSec: cardinal;
  Date, Time: TDateTime;
begin
  if IsNull then begin // to avoid error on OCI function call
    Result := 0;
    Exit;
  end;

  GetDate(Year, Month, Day);
  GetTime(Hour, Min, Sec, FSec);
  Date := EncodeDate(Year, Month, Day);

  Time := EncodeTime(Hour, Min, Sec, FSec div 1000000);
  if Date < 0 then
    Result := Date - Time
  else
    Result := Date + Time;
end;

procedure TOraTimeStamp.SetAsDateTime(Value: TDateTime);
var
  Year, Month, Day, Hour, Min, Sec, FSec: word;
begin
  DecodeDate(Value, Year, Month, Day);
  DecodeTime(Value, Hour, Min, Sec, FSec);
  Construct(Year, Month, Day, Hour, Min, Sec, FSec * 1000000, '');
end;

{$IFNDEF FPC}
procedure TOraTimeStamp.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  Construct(Value.Year, Value.Month, Value.Day, Value.Hour, Value.Minute, Value.Second, Value.Fractions, '');
end;

function  TOraTimeStamp.GetAsSQLTimeStamp: TSQLTimeStamp;
var
  Year: smallint;
  Month, Day, Hour, Min, Sec: byte;
  FSec: cardinal;
begin
  if IsNull then begin
    Result := NullSQLTimeStamp;
    Exit;
  end;

  GetDate(Year, Month, Day);
  GetTime(Hour, Min, Sec, FSec);

  Result.Year := Year;
  Result.Month := Month;
  Result.Day := Day;
  Result.Hour := Hour;
  Result.Minute := Min;
  Result.Second := Sec;
  Result.Fractions := FSec;
end;
{$ENDIF}

{ TOraInterval }

constructor TOraInterval.Create(ADataType: word);
begin
  inherited Create;

  FEnvironment := nil;
  FHandleType := htLocal;

  Init(ADataType);
end;

constructor TOraInterval.Create(AOCIEnvironment: TOCIEnvironment; ADataType: word);
begin
  inherited Create;

  FEnvironment := AOCIEnvironment;
  if FEnvironment = nil then
    FHandleType := htLocal
  else begin
    FHandleType := htNative;
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.AddRef;
  {$ENDIF}
  end;

  Init(ADataType);
end;

constructor TOraInterval.Create(AOCISvcCtx: TOCISvcCtx; ADataType: word);
begin
  inherited Create;

  if AOCISvcCtx <> nil then
    FEnvironment := AOCISvcCtx.Environment
  else
    FEnvironment := nil;
  if FEnvironment = nil then
    FHandleType := htLocal
  else begin
    FHandleType := htNative;
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.AddRef;
  {$ENDIF}
  end;

  Init(ADataType);
end;

destructor TOraInterval.Destroy;
begin
  FreeInterval;
  Marshal.FreeHGlobal(FOCIIntervalPtr);

  if FEnvironment <> nil then begin
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.ReleaseRef;
  {$ENDIF}
    FEnvironment := nil;
  end;

  inherited;
end;

procedure TOraInterval.Init(ADataType: word);
begin
  FOCIIntervalPtr := Marshal.AllocHGlobal(sizeof(IntPtr));
  FOCIIntervalPtr^ := nil;
  FIndicatorPtr := @FIndicator;
  FIndicatorPtr^ := OCI_IND_NULL;
  hOCIInterval := nil;
  FLeadPrecision := 2;
  FFracPrecision := 6;

  case ADataType of
    dtIntervalDS:
      FDescriptorType := OCI_DTYPE_INTERVAL_DS;
    dtIntervalYM:
      FDescriptorType := OCI_DTYPE_INTERVAL_YM;
  else
    Assert(False);
  end;
end;

procedure TOraInterval.AllocInterval;
var
 Interval: pOCIInterval;
begin
  if FHandleType = htLocal then begin
    if FLocalInterval = nil then
      case FDescriptorType of
        OCI_DTYPE_INTERVAL_DS:
          FLocalInterval := TOCIIntervalDS.Create;
        OCI_DTYPE_INTERVAL_YM:
          FLocalInterval := TOCIIntervalYM.Create;
      else
        Assert(False);
      end;
  end
  else if hOCIInterval = nil then begin
    Interval := nil;
    Check(FEnvironment.Home.OCI8.OCIDescriptorAlloc(FEnvironment.hOCIEnv, Interval, FDescriptorType, 0, nil));
    hOCIInterval := Interval;
    FHandleType := htNative
  end;
end;

procedure TOraInterval.FreeInterval;
begin
  if FHandleType = htLocal then
    FreeAndNil(FLocalInterval)
  else if hOCIInterval <> nil then begin
    if FHandleType = htNative then
      Check(FEnvironment.Home.OCI8.OCIDescriptorFree(hOCIInterval, FDescriptorType));
    hOCIInterval := nil;
    FIndicatorPtr^ := OCI_IND_NULL;
    FHandleType := htNative;
  end;
end;

function TOraInterval.ToLocalInterval: TOCIInterval;
var
  Year, Month: integer;
  Day, Hour, Min, Sec, FSec: integer;
begin
  case FDescriptorType of
    OCI_DTYPE_INTERVAL_DS: begin
      Result := TOCIIntervalDS.Create;
      if not IsNull then begin
        GetDaySecond(Day, Hour, Min, Sec, FSec);
        TOCIIntervalDS(Result).SetDaySecond(Day, Hour, Min, Sec, FSec);
      end;
    end;
    OCI_DTYPE_INTERVAL_YM: begin
      Result := TOCIIntervalYM.Create;
      if not IsNull then begin
        GetYearMonth(Year, Month);
        TOCIIntervalYM(Result).SetYearMonth(Year, Month);
      end;
    end
  else
    Result := nil;
    Assert(False);
  end;
end;

procedure TOraInterval.FromLocalInterval(Value: TOCIInterval);
var
  Year, Month: integer;
  Day, Hour, Min, Sec, FSec: integer;
begin
  case FDescriptorType of
    OCI_DTYPE_INTERVAL_DS: begin
      if not IsNull then begin
        (Value as TOCIIntervalDS).GetDaySecond(Day, Hour, Min, Sec, FSec);
        SetDaySecond(Day, Hour, Min, Sec, FSec);
      end;
    end;
    OCI_DTYPE_INTERVAL_YM: begin
      if not IsNull then begin
        (Value as TOCIIntervalYM).GetYearMonth(Year, Month);
        SetYearMonth(Year, Month);
      end;
    end
  else
    Assert(False);
  end;
end;

procedure TOraInterval.Check(Res: sword);
begin
  TOraError.Check(
    @FEnvironment.Home.OCI8.OCIErrorGet,
    Res,
    FEnvironment.UnicodeEnv,
    FEnvironment.hOCIError
  );
end;

procedure TOraInterval.SetEnvironment(AEnvironment: TOCIEnvironment);
var
  TempIsNull: boolean;
  TempValue: TOCIInterval;
begin
  if FEnvironment <> AEnvironment then begin
    TempIsNull := GetIsNull;
    if not TempIsNull then
      TempValue := ToLocalInterval
    else
      TempValue := nil;

    try
      FreeInterval;

    {$IFNDEF AUTOREFCOUNT}
      if FEnvironment <> nil then
        FEnvironment.ReleaseRef;
    {$ENDIF}
      FEnvironment := AEnvironment;
      if FEnvironment <> nil then begin
        FHandleType := htNative;
      {$IFNDEF AUTOREFCOUNT}
        FEnvironment.AddRef;
      {$ENDIF}
      end
      else
        FHandleType := htLocal;

      SetIsNull(TempIsNull);
      if not TempIsNull then
        FromLocalInterval(TempValue);
    finally
      TempValue.Free;
    end;
  end;
end;

procedure TOraInterval.Disconnect;
begin
  SetEnvironment(nil);
end;

procedure TOraInterval.Assign(Source: TOraInterval);
begin
  Source.AssignTo(Self);
end;

procedure TOraInterval.AssignTo(Dest: TOraInterval);

  procedure LocalAssign;
  var
    Temp: TOCIInterval;
  begin
    Temp := ToLocalInterval;
    try
      Dest.FromLocalInterval(Temp);
    finally
      Temp.Free;
    end;
  end;

begin
  Dest.DescriptorType := DescriptorType;
  Dest.AllocInterval;
  Dest.LeadPrecision := LeadPrecision;
  Dest.FracPrecision := FracPrecision;
  Dest.FIndicatorPtr^ := FIndicatorPtr^;

  if not IsNull then
    if (Dest.FEnvironment = nil) and (FEnvironment = nil) then
      Dest.LocalInterval.Assign(LocalInterval)
    else if Dest.FEnvironment = FEnvironment then
      Check(FEnvironment.Home.OCI8.OCIIntervalAssign(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIInterval, Dest.OCIInterval))
    else
      LocalAssign;
end;

function TOraInterval.Compare(Dest: TOraInterval): integer;

  function LocalCompare: Integer;
  var
    Interval1, Interval2: TOCIInterval;
  begin
    Interval1 := ToLocalInterval;
    try
      Interval2 := Dest.ToLocalInterval;
      try
        Result := Interval1.Compare(Interval2);
      finally
        Interval2.Free;
      end;
    finally
      Interval1.Free;
    end;
  end;

begin
  if (Dest.FEnvironment = nil) and (FEnvironment = nil) then
    if Dest.IsNull or IsNull then
      raise Exception.Create(SIntervalsIncomparable)
    else
      Result := LocalInterval.Compare(Dest.LocalInterval)
  else if Dest.FEnvironment = FEnvironment then
    Check(FEnvironment.Home.OCI8.OCIIntervalCompare(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIInterval, Dest.OCIInterval, Result))
  else
    if Dest.IsNull or IsNull then
      raise Exception.Create(SIntervalsIncomparable)
    else
      Result := LocalCompare;
end;

function TOraInterval.GetIsNull: boolean;
begin
  Result := (FIndicatorPtr^ = OCI_IND_NULL);
end;

procedure TOraInterval.SetIsNull(Value: boolean);
begin
  if Value then
    FIndicatorPtr^ := OCI_IND_NULL
  else
    FIndicatorPtr^ := OCI_IND_NOTNULL;
end;


procedure TOraInterval.SetDescriptorType(const Value: cardinal);
begin
  if FDescriptorType <> Value then begin
    FreeInterval;
    FDescriptorType := Value;
  end;
end;

function TOraInterval.GethOCIInterval: pOCIInterval;
begin
  Result := FOCIIntervalPtr^;
end;

procedure TOraInterval.SethOCIInterval(Value: pOCIInterval);
begin
  FOCIIntervalPtr^ := Value;
end;

function TOraInterval.GetOCIInterval: pOCIInterval;
begin
  AllocInterval;
  Result := hOCIInterval;
end;

procedure TOraInterval.SetOCIInterval(const Value: pOCIInterval);
begin
  FreeInterval;
  hOCIInterval := Value;
  if Value <> nil then begin
    FHandleType := htShared;
    IsNull := False;
  end;
end;

function TOraInterval.GetLocalInterval: TOCIInterval;
begin
  AllocInterval;
  Result := FLocalInterval;
end;

function TOraInterval.GetOCIIntervalPtr: ppOCIInterval;
begin
  AllocInterval;
  Result := FOCIIntervalPtr;
end;

procedure TOraInterval.CheckValid;
var
  Valid: cardinal;
begin
  if FHandleType = htLocal then
    Valid := LocalInterval.Check
  else begin
    Assert(hOCIInterval <> nil);
    Valid := 0;
    Check(FEnvironment.Home.OCI8.OCIIntervalCheck(FEnvironment.hOCIEnv, FEnvironment.hOCIError, hOCIInterval, Valid));
  end;

  if Valid <> 0 then
    raise Exception.Create(SInvalidInterval);
end;

function TOraInterval.GetAsString: string;
var
  Ptr: IntPtr;
  BufSize: NativeUInt;
begin
  if IsNull then begin // to avoid error on OCI function call
    Result := '';
    Exit;
  end;

  if FHandleType = htLocal then
    Result := LocalInterval.ToString(FLeadPrecision, FFracPrecision)
  else begin
    BufSize := 255 * SizeOfCharOCI(FEnvironment.UnicodeEnv);
    Ptr := Marshal.AllocHGlobal(BufSize);
    try
      Check(FEnvironment.Home.OCI8.OCIIntervalToText(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIInterval, LeadPrecision,
        FracPrecision, Ptr, BufSize, BufSize));
      Result := PtrToStringOCI(Ptr, BufSize, FEnvironment.UnicodeEnv);
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end;
end;

procedure TOraInterval.SetAsString(const Value: string);
var
  p: IntPtr;
  Res, Size: integer;
begin
  if Value = '' then begin
    FIndicatorPtr^ := OCI_IND_NULL;
    Exit;
  end;

  if FHandleType = htLocal then
    LocalInterval.Parse(Value)
  else begin
    p := StringToHGlobalOCI(Value, Size, FEnvironment.UnicodeEnv);
    Res := FEnvironment.Home.OCI8.OCIIntervalFromText(FEnvironment.hOCIEnv, FEnvironment.hOCIError,
      p, Size, OCIInterval);
    FreeStringOCI(p, FEnvironment.UnicodeEnv);
    Check(Res);
  end;

  CheckValid;
  FIndicatorPtr^ := OCI_IND_NOTNULL;
end;

procedure TOraInterval.GetDaySecond(var Day, Hour, Min, Sec, FSec: integer);
begin
  if FHandleType = htLocal then
    (LocalInterval as TOCIIntervalDS).GetDaySecond(Day, Hour, Min, Sec, FSec)
  else
    Check(FEnvironment.Home.OCI8.OCIIntervalGetDaySecond(FEnvironment.hOCIEnv, FEnvironment.hOCIError, Day, Hour, Min, Sec, FSec, OCIInterval));
end;

procedure TOraInterval.InitInterval;
var
  i: int64;
  pNum: IntPtr;
begin
  if FHandleType = htLocal then begin
    // Empty
  end
  else begin
    // Need to correctly initialize OCIInterval before SetDaySecond or SetYearMonth call.
    i := 0;
    pNum := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
    try
      Check(FEnvironment.Home.OCI8.OCINumberFromInt(FEnvironment.hOCIError, i, SizeOf(i), OCI_NUMBER_UNSIGNED, pNum));
      Check(FEnvironment.Home.OCI8.OCIIntervalFromNumber(FEnvironment.hOCIEnv, FEnvironment.hOCIError, OCIInterval, pNum));
    finally
      Marshal.FreeHGlobal(pNum);
    end;
  end;
end;

procedure TOraInterval.SetDaySecond(Day, Hour, Min, Sec, FSec: integer);
begin
  if FHandleType = htLocal then
    (LocalInterval as TOCIIntervalDS).SetDaySecond(Day, Hour, Min, Sec, FSec)
  else begin
    InitInterval;
    Check(FEnvironment.Home.OCI8.OCIIntervalSetDaySecond(FEnvironment.hOCIEnv, FEnvironment.hOCIError, Day, Hour, Min, Sec, FSec, OCIInterval));
  end;

  FIndicatorPtr^ := OCI_IND_NOTNULL;
end;

procedure TOraInterval.GetYearMonth(var Year, Month: integer);
begin
  if FHandleType = htLocal then
    (LocalInterval as TOCIIntervalYM).GetYearMonth(Year, Month)
  else
    Check(FEnvironment.Home.OCI8.OCIIntervalGetYearMonth(FEnvironment.hOCIEnv, FEnvironment.hOCIError, Year, Month, OCIInterval));
end;

procedure TOraInterval.SetYearMonth(Year, Month: integer);
begin
  if FHandleType = htLocal then
    (LocalInterval as TOCIIntervalYM).SetYearMonth(Year, Month)
  else begin
    InitInterval;
    Check(FEnvironment.Home.OCI8.OCIIntervalSetYearMonth(FEnvironment.hOCIEnv, FEnvironment.hOCIError, Year, Month, OCIInterval));
  end;

  FIndicatorPtr^ := OCI_IND_NOTNULL;
end;

{ TOraNumber }

constructor TOraNumber.Create;
begin
  inherited Create;

  FEnvironment := nil;
  FHandleType := htLocal;

  Init;
end;

constructor TOraNumber.Create(AOCIEnvironment: TOCIEnvironment);
begin
  inherited Create;

  FEnvironment := AOCIEnvironment;

  if FEnvironment = nil then
    FHandleType := htLocal
  else begin
    FHandleType := htNative;
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.AddRef;
  {$ENDIF}
  end;

  Init;
end;

constructor TOraNumber.Create(AOCISvcCtx: TOCISvcCtx);
begin
  inherited Create;

  if AOCISvcCtx <> nil then
    FEnvironment := AOCISvcCtx.Environment
  else
    FEnvironment := nil;

  if FEnvironment = nil then
    FHandleType := htLocal
  else begin
    FHandleType := htNative;
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.AddRef;
  {$ENDIF}
  end;

  Init;
end;

destructor TOraNumber.Destroy;
begin
  if FHandleType <> htShared then //To support SetOCINumberPtr assignation
    Marshal.FreeHGlobal(FOCINumberPtr);

  if FHandleType <> htLocal then begin
  {$IFNDEF AUTOREFCOUNT}
    FEnvironment.ReleaseRef;
  {$ENDIF}
    FEnvironment := nil;
  end;

  inherited;
end;

procedure TOraNumber.Init;
begin
  FOCINumberPtr := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
  FillChar(FOCINumberPtr, OCI_NUMBER_SIZE, 0);
  FIndicatorPtr := @FIndicator;
  FIndicatorPtr^ := OCI_IND_NULL;
end;

procedure TOraNumber.SetEnvironment(AEnvironment: TOCIEnvironment);
begin
  if FEnvironment <> AEnvironment then begin
  {$IFNDEF AUTOREFCOUNT}
    if FEnvironment <> nil then
      FEnvironment.ReleaseRef;
  {$ENDIF}
    FEnvironment := AEnvironment;
    if FEnvironment <> nil then begin
      FHandleType := htNative;
    {$IFNDEF AUTOREFCOUNT}
      FEnvironment.AddRef;
    {$ENDIF}
    end
    else
      FHandleType := htLocal;
  end;
end;

procedure TOraNumber.Disconnect;
begin
  SetEnvironment(nil);
end;

procedure TOraNumber.Assign(Source: TOraNumber);
begin
  Source.AssignTo(Self);
end;

procedure TOraNumber.AssignTo(Dest: TOraNumber);
begin
  Dest.IsNull := IsNull;

  if not IsNull then
    if (Dest.FEnvironment = nil) and (FEnvironment = nil) then
      CopyBuffer(FOCINumberPtr, Dest.FOCINumberPtr, OCI_NUMBER_SIZE)
    else if Dest.FEnvironment = FEnvironment then
      Check(FEnvironment.Home.OCI8.OCINumberAssign(FEnvironment.hOCIError, OCINumberPtr, Dest.OCINumberPtr))
    else
      CopyBuffer(FOCINumberPtr, Dest.FOCINumberPtr, OCI_NUMBER_SIZE);
end;

function TOraNumber.Compare(Dest: TOraNumber): integer;

  function LocalCompare: Integer;
  var
    Number1, Number2: TBytes;
  begin
    SetLength(Number1, PByte(FOCINumberPtr)^);
    Marshal.Copy(PtrOffset(FOCINumberPtr, 1), Number1, 0, Length(Number1));
    SetLength(Number2, PByte(Dest.FOCINumberPtr)^);
    Marshal.Copy(PtrOffset(Dest.FOCINumberPtr, 1), Number2, 0, Length(Number2));
    Result := TOCINumber.Compare(Number1, Number2);
  end;

begin
  if (Dest.FEnvironment = nil) and (FEnvironment = nil) then
    Result := LocalCompare
  else if Dest.FEnvironment = FEnvironment then
    Check(FEnvironment.Home.OCI8.OCINumberCmp(FEnvironment.hOCIError, OCINumberPtr, Dest.OCINumberPtr, Result))
  else
    Result := LocalCompare;
end;

procedure TOraNumber.SetOCINumberPtr(Value: pOCINumber);
begin
  if Value <> FOCINumberPtr then begin
    if FHandleType <> htShared then
      Marshal.FreeHGlobal(FOCINumberPtr);
    FHandleType := htShared;
    FOCINumberPtr := Value;
  end;
end;

const
  MAX_NUMBER_TEXT_SIZE = 64;
  SNlsParams = 'NLS_NUMERIC_CHARACTERS=''%s%s''';

function TOraNumber.GetOCINumber: OCINumber;
begin
  Move(FOCINumberPtr^, Result.OCINumberPart[0], OCI_NUMBER_SIZE);
end;

procedure TOraNumber.SetOCINumber(Value: OCINumber);
begin
  Move(Value.OCINumberPart[0], FOCINumberPtr^, OCI_NUMBER_SIZE);
  IsNull := False;
end;

function TOraNumber.GetAsString: string;
var
  BufSize: cardinal;
  NlsParams: string;
  Ptr, pF, pN: IntPtr;
  FSize, NSize: integer;
  b: byte;
  Num: TBytes;
begin
  Result := '';
  if IsNull then
    Exit;

  if FHandleType <> htLocal then
    if FEnvironment.Home.OCIVersion < 8100 then begin
      Result := FormatFloat('0.#', GetAsFloat);
      Exit;
    end;

  b := Marshal.ReadByte(OCINumberPtr);
  if b = 1 then begin
    b := Marshal.ReadByte(OCINumberPtr, 1);
    if b = $00 then begin
      Result := '-~';
      exit;
    end;
  end
  else if b = 2 then begin
    b := Marshal.ReadByte(OCINumberPtr, 1);
    if b = $FF then begin
      b := Marshal.ReadByte(OCINumberPtr, 2);
      if b = $65 then begin
        Result := '~';
        exit;
      end;
    end;
  end;

  if FHandleType = htLocal then begin
    SetLength(Num, PByte(FOCINumberPtr)^);
    Marshal.Copy(PtrOffset(FOCINumberPtr, 1), Num, 0, Length(Num));
    Result := TOCINumber.ToString(Num, 'TM9')
  end
  else begin
    NlsParams := Format(SNlsParams, [{$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator]);

    BufSize := MAX_NUMBER_TEXT_SIZE * SizeOfCharOCI(FEnvironment.UnicodeEnv);
    Ptr := Marshal.AllocHGlobal(BufSize);
    pF := StringToHGlobalOCI('TM9', FSize, FEnvironment.UnicodeEnv);
    pN := StringToHGlobalOCI(NlsParams, NSize, FEnvironment.UnicodeEnv);
    try
      Check(FEnvironment.Home.OCI8.OCINumberToText(FEnvironment.hOCIError, OCINumberPtr, pF, FSize, pN, NSize, BufSize, Ptr));
      Result := PtrToStringOCI(Ptr, BufSize, FEnvironment.UnicodeEnv);
    finally
      Marshal.FreeHGlobal(Ptr);
      FreeStringOCI(pF, FEnvironment.UnicodeEnv);
      FreeStringOCI(pN, FEnvironment.UnicodeEnv);
    end;
  end;

  if Length(Result) > 0 then begin
    if (Result[1] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) then
      Result := '0' + Result
    else
      if (Result[1] = '-') and (Result[2] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) then
        Result := '-0' + Copy(Result, 2, MaxInt)
  end;
end;

procedure TOraNumber.SetAsString(const Value: string);
var
  FormatStr: string;
  NlsParams: string;

  function GetFormatString: string;
  const
    SScientificFormat = '9D99999999999999999999999999999999999999999EEEE';
  var
    i: integer;
    Signed: boolean;
    FractionalPart: boolean;
    HasDigits: boolean;
  begin
    if Length(Value) > 0 then begin
      SetLength(Result, Length(Value));

      Signed := (Value[1] = '+') or (Value[1] = '-');
      FractionalPart := False;
      HasDigits := False;

      if Signed then begin
        Result[1] := 'S';
        i := 2;
      end
      else
        i := 1;

      while i <= Length(Value) do begin
        if (Value[i] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) and not FractionalPart then begin
          Result[i] := 'D';
          FractionalPart := True;
        end
        else
        if (Value[i] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator) and HasDigits and not FractionalPart then
          Result[i] := 'G'
        else
        if (Value[i] = 'e') or (Value[i] = 'E') then begin
          if Signed then
            Result := 'S' + SScientificFormat
          else
            Result := SScientificFormat;
          Exit;
        end
        else begin
          Result[i] := '9';
          HasDigits := True;
        end;
        Inc(i);
      end;
      if Length(Result) >= MAX_NUMBER_TEXT_SIZE then
        if Signed then
          SetLength(Result, MAX_NUMBER_TEXT_SIZE)
        else
          SetLength(Result, MAX_NUMBER_TEXT_SIZE - 1);
    end
    else
      Result := '';
  end;

var
  pV, pF, pN: IntPtr;
  Res, VSize, FSize, NSize: integer;
  Num: TBytes;
begin
  if Value = '' then begin
    FIndicatorPtr^ := OCI_IND_NULL;
    Exit;
  end;

  if Value = '-~' then begin
    Marshal.WriteByte(OCINumberPtr, 1);
    Marshal.WriteByte(OCINumberPtr, 1, $00);
    IsNull := False;
    exit;
  end
  else if Value = '~' then begin
    Marshal.WriteByte(OCINumberPtr, 2);
    Marshal.WriteByte(OCINumberPtr, 1, $FF);
    Marshal.WriteByte(OCINumberPtr, 2, $65);
    IsNull := False;
    exit;
  end;

  FillChar(OCINumberPtr, sizeof(OCI_NUMBER_SIZE), 0);
  FormatStr := GetFormatString;
  if FHandleType = htLocal then begin
    TOCINumber.Parse(Value, FormatStr, Num);
    PByte(FOCINumberPtr)^ := Length(Num);
    Marshal.Copy(Num, 0, PtrOffset(FOCINumberPtr, 1), Length(Num));
  end
  else begin
    NlsParams := Format(SNlsParams, [{$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ThousandSeparator]);

    pV := StringToHGlobalOCI(Value, VSize, FEnvironment.UnicodeEnv);
    pF := StringToHGlobalOCI(FormatStr, FSize, FEnvironment.UnicodeEnv);
    pN := StringToHGlobalOCI(NlsParams, NSize, FEnvironment.UnicodeEnv);
    Res := FEnvironment.Home.OCI8.OCINumberFromText(FEnvironment.hOCIError, pV, VSize, pF, FSize, pN, NSize, OCINumberPtr);
    FreeStringOCI(pV, FEnvironment.UnicodeEnv);
    FreeStringOCI(pF, FEnvironment.UnicodeEnv);
    FreeStringOCI(pN, FEnvironment.UnicodeEnv);
    Check(Res);
  end;

  IsNull := False;
end;

function TOraNumber.GetAsInteger: integer;
begin
  Result := Integer(GetAsLargeInt);
end;

procedure TOraNumber.SetAsInteger(Value: integer);
begin
  SetAsLargeInt(Value);
end;

function TOraNumber.GetAsLargeInt: int64;
var
  Num: TBytes;
begin
  if IsNull then
    Result := 0
  else if FHandleType = htLocal then begin
    SetLength(Num, PByte(FOCINumberPtr)^);
    Marshal.Copy(PtrOffset(FOCINumberPtr, 1), Num, 0, Length(Num));
    Result := TOCINumber.ToInt(Num, sizeof(int64), OCI_NUMBER_SIGNED);
  end
  else
    Check(FEnvironment.Home.OCI8.OCINumberToInt(FEnvironment.hOCIError, OCINumberPtr, sizeof(int64), OCI_NUMBER_SIGNED, Result));
end;

procedure TOraNumber.SetAsLargeInt(Value: int64);
var
  Num: TBytes;
begin
{$IFNDEF VER9P}
  SetLength(Num, 0); // anti-warning Delphi 6
{$ENDIF}

  if FHandleType = htLocal then begin
    Num := TOCINumber.FromInt(Value, sizeof(int64), OCI_NUMBER_SIGNED);
    PByte(FOCINumberPtr)^ := Length(Num);
    Marshal.Copy(Num, 0, PtrOffset(FOCINumberPtr, 1), Length(Num));
  end
  else
    Check(FEnvironment.Home.OCI8.OCINumberFromInt(FEnvironment.hOCIError, Value, sizeof(int64), OCI_NUMBER_SIGNED, OCINumberPtr));

  IsNull := False;
end;

function TOraNumber.GetAsFloat: double;
var
  Num: TBytes;
begin
  if IsNull then
    Result := 0
  else if FHandleType = htLocal then begin
    SetLength(Num, PByte(FOCINumberPtr)^);
    Marshal.Copy(PtrOffset(FOCINumberPtr, 1), Num, 0, Length(Num));
    Result := TOCINumber.ToReal(Num, sizeof(double));
  end
  else
    Check(FEnvironment.Home.OCI8.OCINumberToReal(FEnvironment.hOCIError, OCINumberPtr, sizeof(double), Result));
end;

procedure TOraNumber.SetAsFloat(Value: double);
var
  Num: TBytes;
begin
{$IFNDEF VER9P}
  SetLength(Num, 0); // anti-warning Delphi 6
{$ENDIF}

  if FHandleType = htLocal then begin
    Num := TOCINumber.FromReal(Value, sizeof(double));
    PByte(FOCINumberPtr)^ := Length(Num);
    Marshal.Copy(Num, 0, PtrOffset(FOCINumberPtr, 1), Length(Num));
  end
  else
    Check(FEnvironment.Home.OCI8.OCINumberFromReal(FEnvironment.hOCIError, Value, sizeof(double), OCINumberPtr));

  IsNull := False;
end;

function TOraNumber.GetAsBCD: TBCD;
begin
  if IsNull then
    Result := NullBcd
  else
    Result := TOCINumber.ToBCD(PtrOffset(OCINumberPtr, 1), PByte(OCINumberPtr)^);
end;

procedure TOraNumber.SetAsBCD(const Value: TBCD);
var
  Num: TBytes;
begin
  Num := TOCINumber.FromBCD(Value);
  PByte(FOCINumberPtr)^ := Length(Num);
  Marshal.Copy(Num, 0, PtrOffset(FOCINumberPtr, 1), Length(Num));
  IsNull := False;
end;

function TOraNumber.GetIsNull: boolean;
begin
  Result := (FIndicatorPtr^ = OCI_IND_NULL);
end;

procedure TOraNumber.SetIsNull(Value: boolean);
begin
  if Value then
    FIndicatorPtr^ := OCI_IND_NULL
  else
    FIndicatorPtr^ := OCI_IND_NOTNULL;
end;

procedure TOraNumber.Check(Res: sword);
begin
  TOraError.Check(
    @FEnvironment.Home.OCI8.OCIErrorGet,
    Res,
    FEnvironment.UnicodeEnv,
    FEnvironment.hOCIError
  );
end;

{TOCITransaction}

constructor TOCITransaction.Create;
begin
  inherited;
end;

destructor TOCITransaction.Destroy;
begin
{$IFNDEF LITE}
  if FXID <> nil then
    Marshal.FreeHGlobal(FXID);
{$ENDIF}

  inherited;
end;

procedure TOCITransaction.Check(Status: sword; Connection: TOCIConnection);
begin
  if Status <> OCI_SUCCESS then
      Connection.OraError(Connection.OCICallStyle, Status, True, Component);
end;

procedure TOCITransaction.OraError(OCISvcCtx: TOCISvcCtx; var ErrorCode: sword; UseCallback: boolean);
var
  Msg: string;
  Code: sword;
  Fail: boolean;
begin
  Code := OCISvcCtx.OCI8.GetOraError(ErrorCode, OCISvcCtx, Msg);
  if (ErrorCode = OCI_SUCCESS_WITH_INFO) and (Code <> 24344) then  // except compilation error
    Exit;
  ErrorCode := Code;

  try
    raise EOraError.Create(ErrorCode, Msg);
  except
    on E: EOraError do begin
      Fail := True;
      if UseCallback then
        if Assigned(FOnError) then
          FOnError(E, Fail);
      if Fail then
        raise
      else
        Abort;
    end;
  end;
end;

function TOCITransaction.SetProp(Prop: integer; const Value: variant): boolean;
begin
{$IFNDEF LITE}
  Result := True;
  case Prop of
    prInactiveTimeout:
      FInactiveTimeout := Value;
    prResumeTimeout:
      FResumeTimeout := Value;
    prTransactionName:
      FTransactionName := Value;
    prTransactionResume:
      FResume := Value;
    prRollbackSegment:
      FRollbackSegment := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
{$ELSE}
  Result := inherited SetProp(Prop, Value);
{$ENDIF}
end;

function TOCITransaction.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited SetProp(Prop, Value);
end;

{$IFNDEF LITE}
procedure TOCITransaction.SetTransactionId(TransactionId: TBytes);
begin
  FTransactionId := Copy(TransactionId, 0, Length(TransactionId));
end;

procedure TOCITransaction.SetBranchQualifier(Index: integer; BranchQualifier: TBytes);
begin
  if Length(FTransactionLinks) <= Index then
    SetLength(FTransactionLinks, Index + 1);
  FTransactionLinks[Index].BranchQualifier := Copy(BranchQualifier, 0, Length(BranchQualifier));
end;

procedure TOCITransaction.WriteTransactionId;
var
  i, GtridLen: integer;
begin
  GtridLen := Length(FTransactionId);
  if GtridLen > MaxTransactionIdLength then
    raise Exception.CreateFmt(STransactionIdTooLong, ['TransactionId']);

  if FXID = nil then
    FXID := Marshal.AllocHGlobal(XID_SIZE);

  Marshal.WriteInt32(FXID, 0, 0);        // FormatID
  Marshal.WriteInt32(FXID, 4, GtridLen); // Gtrid_length
  for i := 0 to GtridLen - 1 do
    Marshal.WriteByte(FXID, 12 + i, FTransactionId[i]);
end;

procedure TOCITransaction.WriteBranchQualifier(TransactionLink: TTransactionLink);
var
  BranchQualifier: TBytes;
  i, GtridLen, BqualLen: integer;
begin
  BranchQualifier := TransactionLink.BranchQualifier;
  BqualLen := Length(BranchQualifier);
  if BqualLen > MaxTransactionIdLength then
    raise Exception.CreateFmt(STransactionIdTooLong, ['BranchQualifier']);

  Marshal.WriteInt32(FXID, 8, BqualLen); // Bqual_length
  GtridLen := Length(FTransactionId);
  for i := 0 to BqualLen - 1 do
    Marshal.WriteByte(FXID, 12 + GtridLen + i, BranchQualifier[i]);
end;

procedure TOCITransaction.FreeTransaction;
var
  i: integer;
  Connection: TOCIConnection;
begin
  for i := 0 to FConnections.Count - 1 do begin
    Connection := TOCIConnection(FConnections[i]);
    if FTransactionLinks[i].State = tsFinished then begin
      Check(Connection.OCI8.OCIAttrSet1(Connection.OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX,
        nil, 0, OCI_ATTR_TRANS, Connection.OCISvcCtx.hOCIError), Connection);
      Check(Connection.OCI8.OCIHandleFree(FTransactionLinks[i].OCITrans, OCI_HTYPE_TRANS), Connection);
      FTransactionLinks[i].State := tsInactive;
    end;
  end;
  FActive := False;
end;
{$ENDIF}

procedure TOCITransaction.StartTransactionLocal;
{$IFNDEF LITE}
var
  S: string;
  Connection: TOCIConnection;
  Command: TCRCommand;
{$ENDIF}
begin
  CommitLocal;

{$IFNDEF LITE}
  Connection := TOCIConnection(FConnections[0]);
  Command := Connection.GetCommand;
  try
    if FRollbackSegment <> '' then begin
      S := 'SET TRANSACTION USE ROLLBACK SEGMENT '+ FRollbackSegment;
      if FTransactionName <> '' then
        S := S + ' NAME ''' + FTransactionName + '''';
      Command.SetSQL('begin ' + S + '; end;');
      Command.Execute;
    end;

    if FReadOnly then begin
      S := 'SET TRANSACTION READ ONLY';
      if FTransactionName <> '' then
        S := S + ' NAME ''' + FTransactionName + '''';
      Command.SetSQL('begin ' + S + '; end;');
      Command.Execute;
    end
    else
    if FIsolationLevel = ilSnapshot then begin
      S := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
      if FTransactionName <> '' then
        S := S + ' NAME ''' + FTransactionName + '''';
      Command.SetSQL('begin ' + S + '; end;');
      Command.Execute;
    end
    else
    if FIsolationLevel <> ilReadCommitted then
      RaiseError(SUnsupportedIsolationLevel);

    if (FIsolationLevel = ilReadCommitted) and (FRollbackSegment = '') and
      (FTransactionName <> '')
    then begin
      Command.SetSQL('begin SET TRANSACTION NAME ''' + FTransactionName + '''; end;');
      Command.Execute;
    end;
  finally
    Connection.ReleaseCommand(Command);
  end;
{$ENDIF}

  FLocalTransactionId := LocalTransactionId(True); //Server side transaction starts here
  FActive := True;
  FNativeTransaction := True;
end;

procedure TOCITransaction.CommitLocal;
var
  Res: sword;
  Connection: TOCIConnection;
begin
  FLocalTransactionId := '';
  FActive := False;
  if FNativeTransaction then begin
    Connection := TOCIConnection(FConnections[0]);
    if Connection.OCICallStyle = OCI73 then begin
      Connection.Lock;
      try
        Res := Connection.OCI7.ocom(Connection.LDA);
      finally
        Connection.Release;
      end;
      Check(Res, Connection);
    end
    else if Connection.OCICallStyle = OCI80 then begin
      Connection.Lock;
      try
        Res := Connection.OCI8.OCITransCommit(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, OCI_DEFAULT);
      finally
        Connection.Release;
      end;
      Check(Res, Connection);
    end
    else
      Connection.OCISvcCtx.Home.CheckOCI;
  end;
  FNativeTransaction := True;
end;

procedure TOCITransaction.RollbackLocal;
var
  Connection: TOCIConnection;
begin
  Connection := TOCIConnection(FConnections[0]);
  FLocalTransactionId := '';
  FActive := False;
  if FNativeTransaction then begin
    if Connection.OCICallStyle = OCI73 then begin
      Check(Connection.OCI7.orol(Connection.LDA), Connection);
    end
    else
    if Connection.OCICallStyle = OCI80 then begin
      Check(Connection.OCI8.OCITransRollback(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, OCI_DEFAULT), Connection);
    end
    else
      Connection.OCISvcCtx.Home.CheckOCI;
  end;
  FNativeTransaction := True;
end;

procedure TOCITransaction.Savepoint(const Name: string);
var
  i: integer;
  Connection: TCRConnection;
  Command: TCRCommand;
begin
  for i := 0 to FConnections.Count - 1 do begin
    Connection := FConnections[i];
    Command := Connection.GetCommand;
    try
      Command.SetSQL('SAVEPOINT ' + Name);
      Command.Execute;
    finally
      Connection.ReleaseCommand(Command);
    end;
  end;
end;

procedure TOCITransaction.RollbackToSavepoint(const Name: string);
var
  i: integer;
  Connection: TCRConnection;
  Command: TCRCommand;
begin
  for i := 0 to FConnections.Count - 1 do begin
    Connection := FConnections[i];
    Command := Connection.GetCommand;
    try
      Command.SetSQL('ROLLBACK TO SAVEPOINT ' + Name);
      Command.Execute;
    finally
      Connection.ReleaseCommand(Command);
    end;
  end;
end;

function TOCITransaction.DetectInTransaction(CanActivate: boolean): boolean;
var
  NewID: string;
begin
  if not FActive then begin
    if CanActivate then begin
      FLocalTransactionId := LocalTransactionId;
      FActive := FLocalTransactionId <> '';
    end;
  end
  else
  if FLocalTransactionId <> '' then begin
    NewID := LocalTransactionId;
    FActive := NewID = FLocalTransactionId;
    if not FActive then begin
      // transaction was implicitly ended (by server-side logic)
      FLocalTransactionId := '';
    end;
  end;
  Result := FActive;
end;

procedure TOCITransaction.AssignConnect(Source: TCRTransaction);
begin
  inherited;

  FLocalTransactionId := TOCITransaction(Source).FLocalTransactionId;
end;

function TOCITransaction.LocalTransactionId(CreateTransaction: boolean = False): string;
var
  SqlLine: string;
  Connection: TCRConnection;
  Command: TCRCommand;
  Param: TParamDesc;
begin
  //we couldn't use here SQL couse of recursive calls in AfterExecute in DisconnectedMode
  Connection := FConnections[0];

  Command := Connection.GetCommand;
  try
    SqlLine := '  :result := sys.dbms_transaction.local_transaction_id';
    if CreateTransaction then
      SqlLine := SqlLine + '(true)'; //call local_transaction_id(true) - to create transaction if there is no any
    SqlLine := SqlLine + '; ';

    Command.SetSQL('begin' + SqlLine + 'end;');
    Param := Command.Params[0];
    Param.SetDataType(dtString);
    Param. SetParamType(pdOutput);
    Param.SetSize(4000);
    Command.Execute;

    if not TOraParamDesc(Param).GetNull then
      Result := TOraParamDesc(Param).GetItemAsString(0)
    else
      Result := '';
  finally
    Connection.ReleaseCommand(Command);
  end;
end;

procedure TOCITransaction.StartTransaction;
{$IFNDEF LITE}
var
  i, Size, Res: integer;
  TransactionFlags: integer;
  Connection: TOCIConnection;
  p: IntPtr;
{$ENDIF}
begin
{$IFNDEF LITE}
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  if (FConnections.Count > 1) and (FTransactionName = '') and (FTransactionId = nil) then
    raise Exception.Create(SCannotStartTransactionWithoutId);

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].GetConnected then
      raise Exception.Create(SConnectionInTransactionNotActive);

  if (FConnections.Count = 1) and (FTransactionId = nil) then begin
{$ENDIF}
    StartTransactionLocal;
{$IFNDEF LITE}
    exit;
  end;

  CheckInactive;

  SetLength(FTransactionLinks, FConnections.Count);

  if FTransactionId <> nil then
    WriteTransactionId;

  if FReadOnly then begin
    TransactionFlags := OCI_TRANS_READONLY;
  end
  else
    case FIsolationLevel of
      ilReadCommitted:
        TransactionFlags := OCI_TRANS_READWRITE;
      ilSnapshot:
        TransactionFlags := OCI_TRANS_SERIALIZABLE;
    else
      raise Exception.Create(SUnsupportedIsolationLevel);
    end;

  FActive := True;
  FLocalTransactionId := '';
  try
    for i := 0 to FConnections.Count - 1 do begin
      Connection := TOCIConnection(FConnections[i]);

      if (Connection.OCISvcCtx.Home.OCICallStyle = OCI80) and not (OCI73 in Connection.OCISvcCtx.Home.PossibleOCICallStyles) then
        raise Exception.Create(STransactionNotSupportedWithDirect);

      Connection.OCISvcCtx.Home.CheckOCI80;

      with FTransactionLinks[i] do begin
        Check(Connection.OCI8.OCIHandleAlloc(Connection.OCISvcCtx.hOCIEnv, OCITrans, OCI_HTYPE_TRANS, 0, nil), Connection);
        try
          if FTransactionId <> nil then begin
            WriteBranchQualifier(FTransactionLinks[i]);
            Check(Connection.OCI8.OCIAttrSet1(OCITrans, OCI_HTYPE_TRANS,
              FXID, XID_SIZE, OCI_ATTR_XID, Connection.OCISvcCtx.hOCIError), Connection);
          end
          else
            if FTransactionName <> '' then begin
              p := StringToHGlobalOCI(FTransactionName, Size, Connection.OCISvcCtx.UnicodeEnv);
              Res := Connection.OCI8.OCIAttrSet1(OCITrans, OCI_HTYPE_TRANS, p, Size, OCI_ATTR_TRANS_NAME, Connection.OCISvcCtx.hOCIError);
              FreeStringOCI(p, Connection.OCISvcCtx.UnicodeEnv);
              Check(Res, Connection);
            end;

            Check(Connection.OCI8.OCIAttrSet1(Connection.OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX,
              OCITrans, 0, OCI_ATTR_TRANS, Connection.OCISvcCtx.hOCIError), Connection);

          if FResume then
            Check(Connection.OCI8.OCITransStart(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, FResumeTimeOut,
              TransactionFlags or OCI_TRANS_RESUME), Connection)
          else
            Check(Connection.OCI8.OCITransStart(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, FInactiveTimeOut,
              TransactionFlags or OCI_TRANS_NEW), Connection);
        except
          Check(Connection.OCI8.OCIAttrSet1(Connection.OCISvcCtx.hOCISvcCtx, OCI_HTYPE_SVCCTX,
            nil, 0, OCI_ATTR_TRANS, Connection.OCISvcCtx.hOCIError), Connection);

          Check(Connection.OCI8.OCIHandleFree(OCITrans, OCI_HTYPE_TRANS), Connection);
          raise;
        end;
        State := tsActive;
      end;
    end;
  except // detach or rollback successfully started branches
    if FResume then
      Detach
    else
      Rollback;
    raise;
  end;
{$ENDIF}
end;

procedure TOCITransaction.Commit;
{$IFNDEF LITE}
var
  i, Status: integer;
  Connection: TOCIConnection;
{$ENDIF}
begin
{$IFNDEF LITE}
  if (FConnections.Count = 1) and (FTransactionId = nil) then begin
{$ENDIF}
    CommitLocal;
{$IFNDEF LITE}
    exit;
  end;

  CheckActive;

  if FConnections.Count > 1 then begin
    for i := 0 to FConnections.Count - 1 do begin
      Connection := TOCIConnection(FConnections[i]);
      if FTransactionLinks[i].State = tsActive then begin
        Status := Connection.OCI8.OCITransPrepare(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, OCI_DEFAULT);
        case Status of
          OCI_SUCCESS:
            FTransactionLinks[i].State := tsPrepared;
          OCI_SUCCESS_WITH_INFO:
            FTransactionLinks[i].State := tsFinished;
          else begin
            RollBack;
            Check(Status, Connection);
          end;
        end;
      end;
    end;
    for i := 0 to FConnections.Count - 1 do begin
      Connection := TOCIConnection(FConnections[i]);
      if FTransactionLinks[i].State = tsPrepared then begin
        Check(Connection.OCI8.OCITransCommit(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, OCI_TRANS_TWOPHASE), Connection);
        FTransactionLinks[i].State := tsFinished;
      end;
    end;
  end
  else begin
    Connection := TOCIConnection(FConnections[0]);
    if FTransactionLinks[0].State = tsActive then begin
      Check(Connection.OCI8.OCITransCommit(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, OCI_DEFAULT), Connection);
      FTransactionLinks[0].State := tsFinished;
    end;
  end;

  FreeTransaction;
{$ENDIF}
end;

procedure TOCITransaction.Rollback;
{$IFNDEF LITE}
var
  i: integer;
  Connection: TOCIConnection;
{$ENDIF}
begin
{$IFNDEF LITE}
  if (FConnections.Count = 1) and (FTransactionId = nil) then begin
{$ENDIF}
    RollbackLocal;
{$IFNDEF LITE}
    exit;
  end;

  CheckActive;

  for i := 0 to FConnections.Count - 1 do begin
    Connection := TOCIConnection(FConnections[i]);
    if FTransactionLinks[i].State in [tsActive, tsPrepared] then begin
      Check(Connection.OCI8.OCITransRollback(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, OCI_DEFAULT), Connection);
      FTransactionLinks[i].State := tsFinished;
    end;
  end;

  FreeTransaction;
{$ENDIF}
end;

{$IFNDEF LITE}
procedure TOCITransaction.Detach;
var
  Connection: TOCIConnection;
  i: integer;
begin
  CheckActive;

  for i := 0 to FConnections.Count - 1 do begin
    Connection := TOCIConnection(FConnections[i]);
    if FTransactionLinks[i].State = tsActive then begin
      Check(Connection.OCI8.OCITransDetach(Connection.OCISvcCtx.hOCISvcCtx, Connection.OCISvcCtx.hOCIError, OCI_DEFAULT), Connection);
      FTransactionLinks[i].State := tsFinished;
    end;
  end;
  FreeTransaction;
end;
{$ENDIF}

{$IFNDEF LITE}

{ TOCIMetaData }

const
  fmtOrderByOwner = 'DECODE(%s, %s, 0, ''PUBLIC'', 1, 2)';

procedure TOCIMetaData.AddOrderBy(var OrderByClause: string; const Value: string);
begin
  if Value <> '' then begin
    if OrderByClause <> '' then
      OrderByClause := OrderByClause + ', ' + Value
    else
      OrderByClause := Value;
  end;
end;

function TOCIMetaData.GetTypesForSQL(const ObjectTypes: string; AllTypes: array of string): string;
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

function TOCIMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TOCIRecordSet.Create;
  Result.GetCommand.SetProp(prIntegerPrecision, 39);
  Result.GetCommand.SetProp(prUseDefaultDataTypes, True);
end;

function TOCIMetaData.InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData;
begin
  if MetaDataKind = 'sequences' then
    Result := GetSequences(Restrictions)
  else if MetaDataKind = 'synonyms' then
    Result := GetSynonyms(Restrictions)
  else
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions);
end;

procedure TOCIMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  inherited;

  List.Add('DataTypes');
  List.Add('Users');
  List.Add('UserDefinedTypes');
  List.Add('Packages');
  List.Add('Sequences');
  List.Add('Synonyms');

  List.Sort;
end;

procedure TOCIMetaData.InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string);
begin
  List.Clear;

  if MetaDataKind = 'procedures' then begin
    List.Add('PROCEDURE_CATALOG');
    List.Add('PROCEDURE_SCHEMA');
    List.Add('PROCEDURE_PACKAGE');
    List.Add('PROCEDURE_NAME');
    List.Add('PROCEDURE_TYPE');
  end
  else
  if MetaDataKind = 'procedureparameters' then begin
    List.Add('PROCEDURE_CATALOG');
    List.Add('PROCEDURE_SCHEMA');
    List.Add('PROCEDURE_PACKAGE');
    List.Add('PROCEDURE_NAME');
    List.Add('PARAMETER_NAME');
  end
  else
  if MetaDataKind = 'packages' then begin
    List.Add('PACKAGE_CATALOG');
    List.Add('PACKAGE_SCHEMA');
    List.Add('PACKAGE_NAME');
    List.Add('CURRENT_SCHEMA');
  end
  else
  if MetaDataKind = 'users' then begin
    List.Add('USER_NAME');
  end
  else
  if MetaDataKind = 'userdefinedtypes' then begin
    List.Add('TYPE_CATALOG');
    List.Add('TYPE_SCHEMA');
    List.Add('TYPE_NAME');
  end
  else
  if MetaDataKind = 'sequences' then begin
    List.Add('SEQUENCE_CATALOG');
    List.Add('SEQUENCE_SCHEMA');
    List.Add('SEQUENCE_NAME');
  end
  else if MetaDataKind = 'synonyms' then begin
    List.Add('SYNONYM_CATALOG');
    List.Add('SYNONYM_SCHEMA');
    List.Add('SYNONYM_NAME');
  end
  else
    inherited;
end;

function TOCIMetaData.GetTables(Restrictions: TStrings): TData;
const
  fmtGetTablesSQL = 'SELECT ' +
    ''''' TABLE_CATALOG, OWNER TABLE_SCHEMA, OBJECT_NAME TABLE_NAME, ' +
    'OBJECT_TYPE TABLE_TYPE, CREATED, LAST_DDL_TIME ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE IN (%s) ' +
    'ORDER BY %s';
  fmtOrderBy = 'OWNER, OBJECT_NAME ';
var
  WhereClause, OrderByClause, Schema, TableName, TableTypes, QuotedTypes, Scope, CachedSchema: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'OBJECT_NAME', TableName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  QuotedTypes := GetTypesForSQL(TableTypes, ['TABLE', 'VIEW', 'SYNONYM']);

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['TABLE_SCHEMA', AnsiQuotedStr(CachedSchema, '''')]));

  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetTablesSQL, [WhereClause, QuotedTypes, OrderByClause]));

  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetColumns(Restrictions: TStrings): TData;
const
  fmtGetColumnsSQL = 'SELECT ' +
    ''''' TABLE_CATALOG, OWNER TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, COLUMN_ID POSITION, ' +
    'DATA_TYPE, DATA_LENGTH, DATA_PRECISION, DATA_SCALE, ' +
    'DECODE(NULLABLE, ''Y'', 1, 0) NULLABLE, DATA_TYPE_MOD, DATA_TYPE_OWNER ' +
    'FROM SYS.ALL_TAB_COLUMNS %s ORDER BY %s ';
  fmtOrderBy = 'OWNER, TABLE_NAME, COLUMN_ID ';
var
  WhereClause, OrderByClause, Schema, TableName, ColumnName, Scope, CachedSchema: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'COLUMN_NAME', ColumnName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['TABLE_SCHEMA', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetColumnsSQL, [WhereClause, OrderByClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetProcedures(Restrictions: TStrings): TData;
const
  fmtGetProceduresSQL = 'SELECT ' +
    ''''' PROCEDURE_CATALOG, OWNER PROCEDURE_SCHEMA, OBJECT_NAME PROCEDURE_NAME, ' +
    'OBJECT_TYPE PROCEDURE_TYPE, '''' PROCEDURE_PACKAGE, CREATED, LAST_DDL_TIME, STATUS, NULL OVERLOAD ' +
    'FROM SYS.ALL_OBJECTS WHERE %s OBJECT_TYPE IN (%s) ';
  fmtGetPackageProceduresSQL = 'SELECT ' +
    ''''' PROCEDURE_CATALOG, OWNER PROCEDURE_SCHEMA, ' +
    'OBJECT_NAME PROCEDURE_NAME, ' +
    'DECODE(POSITION, 0, ''FUNCTION'', ''PROCEDURE'') PROCEDURE_TYPE, ' +
    'PACKAGE_NAME PROCEDURE_PACKAGE, ' +
    'TO_DATE(NULL) CREATED, TO_DATE(NULL) LAST_DDL_TIME, '''' STATUS, ' +
    'OVERLOAD ' +
    'FROM ( ' +
    'SELECT OWNER, PACKAGE_NAME, OBJECT_NAME, OVERLOAD, MIN(POSITION) POSITION ' +
    'FROM SYS.ALL_ARGUMENTS WHERE %s DATA_LEVEL = 0 ' +
    'GROUP BY OWNER, PACKAGE_NAME, OBJECT_NAME, OVERLOAD ' +
    ') %s';
  fmtOrderBy = 'PROCEDURE_SCHEMA, PROCEDURE_PACKAGE, PROCEDURE_NAME';
var
  WhereClause, OrderByClause, Schema, Package, ProcName, ProcTypes, QuotedTypes, Str, SQL, Scope, CachedSchema: string;
  BoolTypes: TBooleanArray;
begin
  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  Package := Trim(Restrictions.Values['PROCEDURE_PACKAGE']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ProcTypes := Trim(Restrictions.Values['PROCEDURE_TYPE']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'OBJECT_NAME', ProcName);

  BoolTypes := nil;

  if (Package = '') or (Package = '""') then begin
    Str := WhereClause;
    if Str <> '' then
      Str := Str + ' AND ';

    QuotedTypes := GetTypesForSQL(ProcTypes, ['PROCEDURE', 'FUNCTION']);

    SQL := Format(fmtGetProceduresSQL, [Str, QuotedTypes]);
  end;

  if (Package = '') or (Package <> '""') then begin
    if Package <> '' then
      AddWhere(WhereClause, 'PACKAGE_NAME', Package)
    else begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      WhereClause := WhereClause + 'PACKAGE_NAME is not NULL';
    end;
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';

    BoolTypes := ParseTypes(ProcTypes, ['PROCEDURE', 'FUNCTION']);
    if BoolTypes[0] or BoolTypes[1] then begin
      if not (BoolTypes[0] and BoolTypes[1]) then begin
        if BoolTypes[0] then
          Str := '1'
        else
          Str := '0';
        QuotedTypes := 'WHERE POSITION = ' + Str;
      end
      else
        QuotedTypes := '';
    end
    else
      QuotedTypes := 'WHERE 1 = 0';

    if SQL <> '' then
      SQL := SQL + DALineSeparator + 'UNION ALL' + DALineSeparator;
    SQL := SQL + Format(fmtGetPackageProceduresSQL, [WhereClause, QuotedTypes]);
  end;

  SQL := 'SELECT * FROM ( ' + SQL + DALineSeparator + ') ORDER BY ';

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['PROCEDURE_SCHEMA', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  SQL := SQL + DALineSeparator + OrderByClause;

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
const
  fmtGetProcedureParametersSQL = 'SELECT ' +
    ''''' PROCEDURE_CATALOG, OWNER PROCEDURE_SCHEMA, OBJECT_NAME PROCEDURE_NAME, ' +
    'ARGUMENT_NAME PARAMETER_NAME, POSITION, IN_OUT DIRECTION, ' +
    'DATA_TYPE, DATA_LENGTH, DATA_PRECISION, DATA_SCALE, ' +
    'TYPE_OWNER, TYPE_NAME, TYPE_SUBNAME, TYPE_LINK ' +
    'FROM SYS.ALL_ARGUMENTS ' +
    'WHERE %s AND DATA_LEVEL = 0 AND (ARGUMENT_NAME IS NOT NULL OR POSITION = 0) ' +
    'ORDER BY %s';
  fmtOrderBy = 'OWNER, OBJECT_NAME, OVERLOAD, POSITION';
var
  WhereClause, OrderByClause, Schema, Package, ProcName, ParamName, Scope, CachedSchema: string;
begin
  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  Package := Trim(Restrictions.Values['PROCEDURE_PACKAGE']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ParamName := Trim(Restrictions.Values['PARAMETER_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'PACKAGE_NAME', Package, True);
  AddWhere(WhereClause, 'OBJECT_NAME', ProcName);
  AddWhere(WhereClause, 'ARGUMENT_NAME', ParamName);

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['PROCEDURE_SCHEMA', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetProcedureParametersSQL, [WhereClause, OrderByClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetIndexes(Restrictions: TStrings): TData;
const
  fmtGetIndexesSQL =
    'SELECT '''' TABLE_CATALOG, ' +
    '       TABLE_OWNER TABLE_SCHEMA, ' +
    '       TABLE_NAME, ' +
    '       '''' INDEX_CATALOG, ' +
    '       OWNER INDEX_SCHEMA, ' +
    '       INDEX_NAME, ' +
    '       DECODE(UNIQUENESS, ''UNIQUE'', 1, 0) "UNIQUE", ' +
    '       INDEX_TYPE, ' +
    '       TO_DATE(NULL) CREATED, ' +
    '       TO_DATE(NULL) LAST_DDL_TIME, ' +
    '       STATUS ' +
    'FROM SYS.ALL_INDEXES %s ' +
    'ORDER BY %s';
  fmtOrderBy = 'OWNER,  ' +
    '         TABLE_OWNER, ' +
    '         TABLE_NAME, ' +
    '         INDEX_NAME';
var
  WhereClause, OrderByClause, IndexSchema, TableName, TableSchema, IndexName, Scope, CachedSchema: string;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexSchema := Trim(Restrictions.Values['INDEX_SCHEMA']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', IndexSchema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'TABLE_OWNER', TableSchema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'INDEX_NAME', IndexName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (IndexSchema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['INDEX_SCHEMA', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetIndexesSQL, [WhereClause, OrderByClause]));
  FRecordSet.Open;

  Result := FRecordSet;
end;

function TOCIMetaData.GetIndexColumns(Restrictions: TStrings): TData;
const
  fmtGetIndexColumnsSQL = 'SELECT ' +
    ''''' TABLE_CATALOG, IC.TABLE_OWNER TABLE_SCHEMA, IC.TABLE_NAME, ' +
    ''''' INDEX_CATALOG, IC.INDEX_OWNER INDEX_SCHEMA, IC.INDEX_NAME, ' +
    'IC.COLUMN_NAME, IC.COLUMN_POSITION, %s DESCENDING ' +
    'FROM SYS.ALL_IND_COLUMNS IC ' +
    '%s ORDER BY %s ';
  fmtOrderBy = 'INDEX_OWNER, TABLE_OWNER, TABLE_NAME, INDEX_NAME, COLUMN_POSITION';

  // Index for Primary Key must be first, but for Oracle 805 it is impossible
  fmtSQLWithIndexTypeOld = 'SELECT ' +
    ''''' TABLE_CATALOG, IC.TABLE_OWNER TABLE_SCHEMA, IC.TABLE_NAME, ' +
    ''''' INDEX_CATALOG, IC.INDEX_OWNER INDEX_SCHEMA, IC.INDEX_NAME, ' +
    'IC.COLUMN_NAME, IC.COLUMN_POSITION, %s DESCENDING ' +
    'FROM SYS.ALL_IND_COLUMNS IC, SYS.ALL_INDEXES I ' +
    'WHERE %s ' +
    '      I.OWNER = IC.INDEX_OWNER AND I.INDEX_NAME = IC.INDEX_NAME ' +
    'ORDER BY %s ';
  fmtOrderByOld = 'IC.INDEX_OWNER, IC.TABLE_OWNER, IC.TABLE_NAME, IC.INDEX_NAME, IC.COLUMN_POSITION';
  // Index for Primary Key must be first
  fmtSQLWithIndexTypeForORA9 = 'SELECT ' +
    ''''' TABLE_CATALOG, IC.TABLE_OWNER TABLE_SCHEMA, IC.TABLE_NAME, ' +
    ''''' INDEX_CATALOG, IC.INDEX_OWNER INDEX_SCHEMA, IC.INDEX_NAME, ' +
    'IC.COLUMN_NAME, IC.COLUMN_POSITION, %s DESCENDING ' +
    'FROM SYS.ALL_IND_COLUMNS IC, SYS.ALL_INDEXES I, SYS.ALL_CONSTRAINTS C ' +
    'WHERE %s ' +
    '      I.OWNER = IC.INDEX_OWNER AND I.INDEX_NAME = IC.INDEX_NAME AND ' +
    '      C.TABLE_NAME (+) = IC.TABLE_NAME AND C.INDEX_NAME (+) = IC.INDEX_NAME AND C.OWNER (+)= IC.TABLE_OWNER ' +
    'ORDER BY %s ';
  fmtOrderByForORA9 = 'DECODE(C.CONSTRAINT_TYPE, ''P'', 1, ''U'', 2, 3), IC.INDEX_OWNER, IC.TABLE_OWNER, IC.TABLE_NAME, IC.INDEX_NAME, IC.COLUMN_POSITION';

  DescOld = '0';
  DescForORA9 = 'DECODE(IC.DESCEND, ''ASC'', 0, 1)';
var
  WhereClause, OrderByClause, IndexSchema, TableName, TableSchema, IndexName, Uniqueness, Scope, CachedSchema: string;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexSchema := Trim(Restrictions.Values['INDEX_SCHEMA']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  Uniqueness := Trim(Restrictions.Values['UNIQUE']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'INDEX_OWNER', IndexSchema)
  else
    AddWhere(WhereClause, 'IC.INDEX_OWNER', CachedSchema);
  AddWhere(WhereClause, 'IC.TABLE_OWNER', TableSchema);
  AddWhere(WhereClause, 'IC.TABLE_NAME', TableName);
  AddWhere(WhereClause, 'IC.INDEX_NAME', IndexName);

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (IndexSchema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['IC.INDEX_OWNER', AnsiQuotedStr(CachedSchema, '''')]));

  if (Uniqueness = '0') or (Uniqueness = '1') then begin
    if Uniqueness = '0' then
      Uniqueness := 'NONUNIQUE'
    else
      Uniqueness := 'UNIQUE';
    AddWhere(WhereClause, 'I.UNIQUENESS', Uniqueness);
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    if TOCIConnection(FRecordSet.GetCommand.GetConnection).GetOracleVersion < 9000 then begin
      AddOrderBy(OrderByClause, fmtOrderByOld);
      FRecordSet.SetSQL(Format(fmtSQLWithIndexTypeOld, [DescOld, WhereClause, OrderByClause]));
    end
    else begin
      AddOrderBy(OrderByClause, fmtOrderByForORA9);
      FRecordSet.SetSQL(Format(fmtSQLWithIndexTypeForORA9, [DescForORA9, WhereClause, OrderByClause]));
    end;
  end
  else begin
    if WhereClause <> '' then
      WhereClause := 'WHERE ' + WhereClause;
    AddOrderBy(OrderByClause, fmtOrderBy);
    FRecordSet.SetSQL(Format(fmtGetIndexColumnsSQL, [DescForORA9, WhereClause, OrderByClause]));
  end;

  FRecordSet.Open;

  Result := FRecordSet;
end;

function TOCIMetaData.GetConstraints(Restrictions: TStrings): TData;
const
  fmtGetConstraintsSQLStart = 'SELECT ' +
    ''''' TABLE_CATALOG, OWNER TABLE_SCHEMA, TABLE_NAME, CONSTRAINT_NAME, ' +
    'DECODE(CONSTRAINT_TYPE, ''C'', ''CHECK'', ''P'', ''PRIMARY KEY'', ''U'', ''UNIQUE'', ' +
    '''R'', ''FOREIGN KEY'', ''UNKNOWN'') CONSTRAINT_TYPE, ';

  fmtGetConstraintsSQLforORA9 = ''''' INDEX_CATALOG, INDEX_OWNER INDEX_SCHEMA, INDEX_NAME, ';

  fmtGetConstraintsSQLEnd = 'R_OWNER REFERRED_TABLE_OWNER, R_CONSTRAINT_NAME REFERRED_CONSTRAINT_NAME, ' +
    'DECODE(STATUS, ''ENABLED'', 1, 0) ENABLED ' +
    'FROM SYS.ALL_CONSTRAINTS ' +
    '%s ORDER BY %s ';

  fmtOrderBy = 'OWNER, TABLE_NAME, CONSTRAINT_NAME';
var
  WhereClause, OrderByClause, Schema, TableName, ConstraintName, Types, TypesFilter, Scope, CachedSchema: string;
  fmtGetConstraintsSQL: string;
  BoolTypes: TBooleanArray;
  i: integer;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := Trim(Restrictions.Values['CONSTRAINT_TYPE']);

  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'CONSTRAINT_NAME', ConstraintName);

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
            0: TypesFilter := TypesFilter + '''C''';
            1: TypesFilter := TypesFilter + '''P''';
            2: TypesFilter := TypesFilter + '''U''';
            3: TypesFilter := TypesFilter + '''R''';
          end;
        end;
      end;
      if TypesFilter = '' then
        TypesFilter := '0 = 1'
      else begin
        TypesFilter := 'CONSTRAINT_TYPE IN (' + TypesFilter + ')';
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

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['OWNER', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  if TOCIConnection(FRecordSet.GetCommand.GetConnection).GetOracleVersion >= 9000 then
    fmtGetConstraintsSQL := fmtGetConstraintsSQLStart + fmtGetConstraintsSQLforORA9 + fmtGetConstraintsSQLEnd
  else
    fmtGetConstraintsSQL := fmtGetConstraintsSQLStart + fmtGetConstraintsSQLEnd;

  FRecordSet.SetSQL(Format(fmtGetConstraintsSQL, [WhereClause, OrderByClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetConstraintColumns(Restrictions: TStrings): TData;
const
  fmtGetConstraintColumnsSQL = 'SELECT DISTINCT ' +
    ''''' TABLE_CATALOG, AC.OWNER TABLE_SCHEMA, AC.TABLE_NAME TABLE_NAME, AC.CONSTRAINT_NAME CONSTRAINT_NAME, ' +
    'ACC.COLUMN_NAME COLUMN_NAME, ACC.POSITION  POSITION ' +
    'FROM SYS.ALL_CONSTRAINTS AC, SYS.ALL_CONS_COLUMNS ACC ' +
    'WHERE AC.CONSTRAINT_NAME = ACC.CONSTRAINT_NAME ' +
    '%s ORDER BY %s ';

  fmtOrderBy = 'AC.OWNER, AC.TABLE_NAME, AC.CONSTRAINT_NAME';
var
  WhereClause, OrderByClause, Schema, TableName, ConstraintName, ColumnName, Scope, CachedSchema: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then begin
    AddWhere(WhereClause, 'AC.OWNER', Schema);
    AddWhere(WhereClause, 'ACC.OWNER', Schema);
  end
  else begin
    AddWhere(WhereClause, 'AC.OWNER', CachedSchema);
    AddWhere(WhereClause, 'ACC.OWNER', CachedSchema);
  end;
  AddWhere(WhereClause, 'AC.TABLE_NAME', TableName);
  AddWhere(WhereClause, 'AC.CONSTRAINT_NAME', ConstraintName);
  AddWhere(WhereClause, 'AC.COLUMN_NAME', ColumnName);

  if WhereClause <> '' then
    WhereClause := 'AND ' + WhereClause;

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['AC.OWNER', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetConstraintColumnsSQL, [WhereClause, OrderByClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

{function TOCIMetaData.GetDataTypes(Restrictions: TStrings): TData;
const
  fmtGetDataTypesSQL = 'SELECT TYPE_NAME FROM SYS.ALL_TYPES ' +
  'WHERE OWNER IS NULL ' +
  'ORDER BY TYPE_NAME';
begin
  FRecordSet.SetSQL(fmtGetDataTypesSQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;}

function TOCIMetaData.GetUsers(Restrictions: TStrings): TData;
const
  fmtGetUsersSQL = 'SELECT ' +
    'USERNAME USER_NAME, CREATED ' +
    'FROM SYS.ALL_USERS ' +
    '%s ORDER BY USERNAME';
var
  WhereClause, UserName: string;
begin
  UserName := Trim(Restrictions.Values['USER_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'USERNAME', UserName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(Format(fmtGetUsersSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetPackages(Restrictions: TStrings): TData;
const
  fmtGetPackagesSQL = 'SELECT ' +
    ''''' PACKAGE_CATALOG, OWNER PACKAGE_SCHEMA, OBJECT_NAME PACKAGE_NAME, ' +
    'CREATED, LAST_DDL_TIME, STATUS ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE = ''PACKAGE'' ' +
    'ORDER BY %s ';
  fmtOrderBy = 'OWNER, OBJECT_NAME';

var
  WhereClause, OrderByClause, Schema, PackageName, Scope, CachedSchema: string;
begin
  Schema := Trim(Restrictions.Values['PACKAGE_SCHEMA']);
  PackageName := Trim(Restrictions.Values['PACKAGE_NAME']);
  Scope := Trim(Restrictions.Values['SCOPE']);
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;
  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);

  AddWhere(WhereClause, 'OBJECT_NAME', PackageName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['OWNER', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetPackagesSQL, [WhereClause, OrderByClause]));

  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetUDTs(Restrictions: TStrings): TData;
const
  fmtGetUDTsSQL = 'SELECT ' +
    ''''' TYPE_CATALOG, OWNER TYPE_SCHEMA, OBJECT_NAME TYPE_NAME, ' +
    'CREATED, LAST_DDL_TIME, STATUS ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE = ''TYPE'' ORDER BY %s ';
  fmtOrderBy = 'OWNER, OBJECT_NAME';
var
  WhereClause, OrderByClause, Schema, TypeName, Scope, CachedSchema: string;
begin
  Schema := Trim(Restrictions.Values['TYPE_SCHEMA']);
  TypeName := Trim(Restrictions.Values['TYPE_NAME']);
  Scope := Trim(Restrictions.Values['SCOPE']);
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'OBJECT_NAME', TypeName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['OWNER', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetUDTsSQL, [WhereClause, OrderByClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetSequences(Restrictions: TStrings): TData;
const
  fmtGetSequencesSQL = 'SELECT ' +
    ''''' SEQUENCE_CATALOG, OWNER SEQUENCE_SCHEMA, OBJECT_NAME SEQUENCE_NAME, ' +
    'CREATED, LAST_DDL_TIME ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE IN (''SEQUENCE'') ORDER BY %s ';
  fmtOrderBy = 'OWNER, OBJECT_NAME ';
var
  WhereClause, OrderByClause, Schema, ObjectName, Scope, CachedSchema: string;
begin
  Schema := Trim(Restrictions.Values['SEQUENCE_SCHEMA']);
  ObjectName := Trim(Restrictions.Values['SEQUENCE_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'OBJECT_NAME', ObjectName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['OWNER', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetSequencesSQL, [WhereClause, OrderByClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetSynonyms(Restrictions: TStrings): TData;
const
  fmtGetSynonymsSQL = 'SELECT ' +
    ''''' SYNONYM_CATALOG, OWNER SYNONYM_SCHEMA, OBJECT_NAME SYNONYM_NAME, ' +
    'CREATED, LAST_DDL_TIME ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE IN (''SYNONYM'') ORDER BY %s ';
  fmtOrderBy = 'OWNER, OBJECT_NAME ';
var
  WhereClause, OrderByClause, Schema, ObjectName, Scope, CachedSchema: string;
begin
  Schema := Trim(Restrictions.Values['SYNONYM_SCHEMA']);
  ObjectName := Trim(Restrictions.Values['SYNONYM_NAME']);
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  CachedSchema := TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema;

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', CachedSchema);
  AddWhere(WhereClause, 'OBJECT_NAME', ObjectName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  OrderByClause := '';
  if (Scope <> 'LOCAL') and (Schema = '') then
    AddOrderBy(OrderByClause, Format(fmtOrderByOwner, ['OWNER', AnsiQuotedStr(CachedSchema, '''')]));
  AddOrderBy(OrderByClause, fmtOrderBy);

  FRecordSet.SetSQL(Format(fmtGetSynonymsSQL, [WhereClause, OrderByClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

{ TOCILoader }

const
  DPBufferSize = $10000*2; // 64K*2

// RESET, [[GET_RECORD, FIELD_SET]+, DO_CONVERT, DO_LOAD]+, RESET

constructor TOCILoaderColumn.Create;
begin
  inherited;
  FIsNational := false;
  FIsLob := false;
end;

procedure TOCILoaderColumn.VerifySize;
begin
  case DataType of
    dtString, dtWideString, dtFixedChar, dtFixedWideChar, dtMemo, dtWideMemo, dtGuid, dtTimeStamp: begin
      if Size = 0 then
        Size := 20;
    end;
    dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32: begin
      if Size < sizeof(Integer) then
        Size := sizeof(Integer);
    end;
    dtFloat, dtCurrency, dtBCD, dtFMTBCD: begin
      if Size < sizeof(Double) then
        Size := sizeof(Double);
    end;
    dtDate, dtTime, dtDateTime: begin
      if Size < 7 then
        Size := 7;
    end;
    dtBlob, dtOraBlob: begin
      if Size < 4 then
        Size := 4;
    end;
    dtOraClob, dtWideOraClob, dtNClob: begin
      if Size < 4 then
        Size := 4;
    end;
  else begin
      if Size = 0 then
        Size := 20;
    end;
  end;
end;

procedure TOCILoaderColumn.UpdateDataType(Value: word);
begin
  // do not change DateTime types
  if (Value    in [dtDate, dtTime, dtDateTime,
                   dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ]) and
     (DataType in [dtDate, dtTime, dtDateTime,
                   dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ])
 then
   exit
 else
   inherited UpdateDataType(Value);
end;

{ TOCILoader }

constructor TOCILoader.Create;
begin
  inherited;

  FDML := TOCICommand.Create;
  FDML.SetProp(prAutoCommit, True);
  FDML.SetProp(prTemporaryLobUpdate, True);
  hDirPathCtx := nil;
  hColumnArray := nil;
  hStream := nil;
  FBuffer := nil;
  FLobBuffer := nil;
  FIsDirectMode := True;
  FSkipReadOnlyFieldDescs := False;
end;

destructor TOCILoader.Destroy;
begin
  FDML.Free;

  inherited;
end;

function TOCILoader.GetOCISvcCtx: TOCISvcCtx;
begin
  Result := FConnection.OCISvcCtx;
end;

function TOCILoader.GetOCI7: TOCI7API;
begin
  Result := FConnection.OCI7;
end;

function TOCILoader.GetOCI8: TOCI8API;
begin
  Result := FConnection.OCI8;
end;

procedure TOCILoader.Check(Status: Word);
begin
  OCISvcCtx.OCI8.Check(Status, FConnection.OCISvcCtx);
end;

function TOCILoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirectPath:
      FIsDirectMode := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCILoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirectPath:
      Value := FIsDirectMode;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

procedure TOCILoader.Prepare;
var
  StrTableName: string;
  SchemaName: string;
  i: integer;
  Column: TOCILoaderColumn;
  ColumnCount: integer; //sb2
  hColumnList: pOCIParam;
  hColumn: pOCIParam;
  ParamType: byte;
  D1: integer; //byte;
  D2: integer; //word;
  BufSize: Integer;
  Res: sword;
  Ptr, ValuePtr: IntPtr;
  ValueInt: Integer;
  ValSize: integer;
  St: string;
  CharsetID: integer;
  Info: TSQLObjectInfo;
begin
  inherited;

  if FIsDirectMode then begin
  // Direct path
    if OCISvcCtx.Home.PossibleOCICallStyles = [OCI80] then
      RaiseError(SDirectPathLoadingNotSupportedWithDirect);
    OCISvcCtx.Home.CheckOCI81;
    FConnection.CheckOCI80;

    if hDirPathCtx = nil then begin
      Res := OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, pOCIHandle(hDirPathCtx), OCI_HTYPE_DIRPATH_CTX, 0, nil); // raises OCI_NO_DATA if OCIThreaded
      if Res <> OCI_SUCCESS then begin
        hDirPathCtx := nil;
        Check(Res);
      end;
    end;

    TableName := OCISQLInfo.NormalizeName(TableName, QuoteNames);
    OCISQLInfo.SplitObjectName(TableName, Info);
    StrTableName := Info.Name;
    SchemaName := Info.Schema;
    if SchemaName = '' then
      SchemaName := FConnection.GetCachedSchema;

    Ptr := StringToHGlobalOCI(SchemaName, ValSize, OCISvcCtx.UnicodeEnv);
    Res := OCI8.OCIAttrSet1(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, Ptr, ValSize,
      OCI_ATTR_SCHEMA_NAME, OCISvcCtx.hOCIError);
    FreeStringOCI(Ptr, OCISvcCtx.UnicodeEnv);
    Check(Res);

    Ptr := StringToHGlobalOCI(StrTableName, ValSize, OCISvcCtx.UnicodeEnv);
    Res := OCI8.OCIAttrSet1(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, Ptr, ValSize,
      OCI_ATTR_NAME, OCISvcCtx.hOCIError);
    FreeStringOCI(Ptr, OCISvcCtx.UnicodeEnv);
    Check(Res);

  // Columns
    ColumnCount := Columns.Count;
    Check(OCI8.OCIAttrSet2(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, ColumnCount,
      0, OCI_ATTR_NUM_COLS, OCISvcCtx.hOCIError));

    ValuePtr := @hColumnList;
    Check(OCI8.OCIAttrGet1(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, ValuePtr, nil,
      OCI_ATTR_LIST_COLUMNS, OCISvcCtx.hOCIError));

    Check(OCI8.OCIAttrGet2(hColumnList, OCI_DTYPE_PARAM, ValueInt, nil,
      OCI_ATTR_PTYPE, OCISvcCtx.hOCIError));
    ParamType := Byte(ValueInt);

    Assert(ParamType = OCI_PTYPE_LIST);

    FRowSize := 0;
    for i := 0 to ColumnCount - 1 do begin
      Check(OCI8.OCIParamGet(hColumnList, OCI_DTYPE_PARAM, OCISvcCtx.hOCIError, hColumn, i + 1));

      try
        Ptr := StringToHGlobalOCI(Columns[i].Name, ValSize, OCISvcCtx.UnicodeEnv);
        Res := OCI8.OCIAttrSet1(hColumn, OCI_DTYPE_PARAM, Ptr, ValSize,
          OCI_ATTR_NAME, OCISvcCtx.hOCIError);
        FreeStringOCI(Ptr, OCISvcCtx.UnicodeEnv);
        Check(Res);

        Column := TOCILoaderColumn(Columns[i]);
        case Column.DataType of
          dtInt8, dtInt16, dtInt32: begin
            Column.ColumnType := SQLT_INT;
            Column.DataSize := SizeOf(Integer);
          end;
          dtUInt8, dtUInt16, dtUInt32: begin
            Column.ColumnType := SQLT_UIN;
            Column.DataSize := SizeOf(Integer);
          end;
          dtFloat, dtCurrency, dtBCD, dtFMTBcd, dtNumber, dtInt64: begin
            Column.ColumnType := SQLT_FLT;
            Column.DataSize := SizeOf(Double);
          end;
          dtDate, dtTime, dtDateTime: begin
            Column.ColumnType := SQLT_DAT;
            Column.DataSize := 8;
          end;
          dtSQLTimeStamp: begin
            case Column.SubDataType of
              dtTimeStamp: begin
                Column.ColumnType := SQLT_TIMESTAMP;
                Column.DataSize := 20;
              end;
              dtTimeStampTZ: begin
                Column.ColumnType := SQLT_TIMESTAMP_TZ;
                Column.DataSize := 20;
              end;
              dtTimeStampLTZ: begin
                Column.ColumnType := SQLT_TIMESTAMP_TZ;
                Column.DataSize := 20;
              end;
              else begin
                Column.ColumnType := SQLT_TIMESTAMP;
                Column.DataSize := 20;
              end;
            end;
          end;
          dtTimeStamp: begin
            Column.ColumnType := SQLT_TIMESTAMP;
            Column.DataSize := 20;
          end;
          dtTimeStampTZ: begin
            Column.ColumnType := SQLT_TIMESTAMP_TZ;
            Column.DataSize := 20;
          end;
          dtTimeStampLTZ: begin
            Column.ColumnType := SQLT_TIMESTAMP_TZ;
            Column.DataSize := 20;
          end;
          dtBlob, dtOraBlob: begin
            Column.ColumnType := SQLT_CHR;
            Column.DataSize := 4;
            Column.IsLob := true;
          end;
          dtMemo, dtWideMemo, dtOraClob, dtWideOraClob, dtNClob: begin
            Column.ColumnType := SQLT_CHR;
            Column.DataSize := 4;
            Column.IsLob := true;
          end;
        else
          Column.ColumnType := SQLT_CHR;
          if Column.DataSize = 0 then
            Column.DataSize := 20;
        end;

        ValueInt := Column.ColumnType;
        Check(OCI8.OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, ValueInt,
          0, OCI_ATTR_DATA_TYPE, OCISvcCtx.hOCIError));

        if ConverToUnicode(TOCILoaderColumn(Columns[i])) then begin
          CharsetID := OCI_UTF16ID;
          Check(OCI8.OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, CharsetID,
            0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));
        end
        else if FConnection.FUnicodeEnvironment and (FConnection.FInternalCharsetId <> 0) then begin
          CharsetID := FConnection.FInternalCharsetId;
          Check(OCI8.OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, CharsetID,
            0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));
        end;

        ValueInt := Column.DataSize;
        Check(OCI8.OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, ValueInt,
          0, OCI_ATTR_DATA_SIZE, OCISvcCtx.hOCIError));

        if Column.Precision > 0 then begin
          D1 := Integer(Column.Precision);
          Check(OCI8.OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, D1,
            0, OCI_ATTR_PRECISION, OCISvcCtx.hOCIError));
        end;

        if Column.Scale > 0 then begin
          D1 := Integer(Column.Scale);
          Check(OCI8.OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, D1,
            0, OCI_ATTR_SCALE, OCISvcCtx.hOCIError));
        end;

        // date format
        if Column.DateFormat <> '' then begin
          Ptr := StringToHGlobalOCI(Column.DateFormat, ValSize, OCISvcCtx.UnicodeEnv);
          Res := OCI8.OCIAttrSet1(hColumn, OCI_DTYPE_PARAM, Ptr, ValSize,
            OCI_ATTR_DATEFORMAT, OCISvcCtx.hOCIError);
          FreeStringOCI(Ptr, OCISvcCtx.UnicodeEnv);
          Check(Res);
        end;

        if Column.Scale > 0 then begin
          // Set char set for float fields
          D2 := 1; // US7ASCII
          Check(OCI8.OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, D2, 0,
            OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));
        end;

        Column.Offset := FRowSize;
        Inc(FRowSize, Column.DataSize);

      finally
        // free memory for OCIParamGet
        OCI8.OCIDescriptorFree(hColumn, OCI_DTYPE_PARAM);
      end;
    end;

    BufSize := Integer(DPBufferSize);
    Check(OCI8.OCIAttrSet2(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, BufSize,
        0, OCI_ATTR_BUF_SIZE, OCISvcCtx.hOCIError));
    try
      Check(OCI8.OCIDirPathPrepare(hDirPathCtx, OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError));
    except
      Check(OCI8.OCIHandleFree(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX));
      hDirPathCtx := nil;
      raise;
    end;

    if hColumnArray = nil then
      Check(OCI8.OCIHandleAlloc(hDirPathCtx, pOCIHandle(hColumnArray), OCI_HTYPE_DIRPATH_COLUMN_ARRAY, 0, nil));
    if hStream = nil then
      Check(OCI8.OCIHandleAlloc(hDirPathCtx, pOCIHandle(hStream), OCI_HTYPE_DIRPATH_STREAM, 0, nil));

    Check(OCI8.OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY, ValueInt, nil,
      OCI_ATTR_NUM_ROWS, OCISvcCtx.hOCIError));
    FBufNumRows := Integer(ValueInt);

    Check(OCI8.OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY, ValueInt, nil,
      OCI_ATTR_NUM_COLS, OCISvcCtx.hOCIError));
    FBufNumCols := Word(ValueInt);

    FBuffer := Marshal.AllocHGlobal(FBufNumRows * FRowSize);
    CheckLobBuffer;

    Reset;
  end
  else begin
  // DML
    FDML.SetConnection(FConnection);
    St := 'INSERT INTO ' + TableName + '(';
    for i := 0 to Columns.Count - 1 do begin
      if i > 0 then
        St := St + ', ';
      St := St + OCISQLInfo.NormalizeName(Columns[i].Name, QuoteNames);
    end;
    St := St + ') VALUES (';
    for i := 0 to Columns.Count - 1 do begin
      if i > 0 then
        St := St + ', ';
      St := St + ':' + OCISQLInfo.NormalizeName(Columns[i].Name, QuoteNames);
    end;
    St := St + ')';

    FDML.SetSQL(St);

    // Set params type
    FBufNumRows := 200;
    for i := 0 to Columns.Count - 1 do begin
      Column := TOCILoaderColumn(Columns[i]);
      FDML.Params[i].SetDataType(Column.DataType);
      FDML.Params[i].SetParamType(pdInput);
      if Column.DataType in [dtBlob, dtMemo, dtWideMemo, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob] then
        FDML.Params[i].SetSize(0)
      else
        FDML.Params[i].SetSize(Column.Size);
      TOraParamDesc(FDML.Params[i]).SetArraySize(FBufNumRows);
    end;

    FDML.Prepare;
  end;
end;

procedure TOCILoader.Finish;
var
  i: Integer;
begin
  if FIsDirectMode then begin
  // Direct
    Reset;

    if hDirPathCtx <> nil then begin
      Check(OCI8.OCIDirPathFinish(hDirPathCtx, OCISvcCtx.hOCIError));

      Check(OCI8.OCIHandleFree(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY));
      Check(OCI8.OCIHandleFree(hStream, OCI_HTYPE_DIRPATH_STREAM));
      Check(OCI8.OCIHandleFree(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX));

      hColumnArray := nil;
      hStream := nil;
      hDirPathCtx := nil;
    end;

    Marshal.FreeHGlobal(FBuffer);
    FBuffer := nil;
    FreeLobBuffer;
  end
  else begin
  // DML
    FDML.UnPrepare;
    for i := 0 to FDML.Params.Count - 1 do
      TOraParamDesc(FDML.Params[i]).SetArraySize(0);
  end;

  inherited;
end;

procedure TOCILoader.Reset;
var
  i: Integer;
begin
  inherited;

  for i := 0 to FColumns.Count - 1 do
    TOCILoaderColumn(FColumns[i]).VerifySize;

  if hColumnArray <> nil then
    Check(OCI8.OCIDirPathColArrayReset(hColumnArray, OCISvcCtx.hOCIError));
  if hStream <> nil then
    Check(OCI8.OCIDirPathStreamReset(hStream, OCISvcCtx.hOCIError));
end;

procedure TOCILoader.PutColumnData(Col: integer; Row: integer; const Value: variant);

  procedure Error(Column: TOCILoaderColumn);
  begin
    raise EConvertError.Create('Cannot convert data for ' + Column.Name + ' column');
  end;

  procedure PutColumnStr(const Str: String; const IsUnicode: boolean; const ColumnSize: Integer; var Size: Integer; var Ptr: IntPtr);
  var
    AStr: AnsiString;
    WStr: WideString;
    SrcPtr: IntPtr;
  begin
    if IsUnicode then begin
      WStr := WideString(Str);
      Size := Length(WStr);
      if (ColumnSize >= 0) and (Size > ColumnSize) then
        Size := ColumnSize;
      Size := Size * 2;
      SrcPtr := Marshal.StringToHGlobalUni(WStr);
    end
    else begin
      AStr := AnsiString(Str);
      Size := LengthA(AStr);
      if (ColumnSize >= 0) and (Size > ColumnSize) then
        Size := ColumnSize;
      SrcPtr := Marshal.StringToHGlobalAnsi(AStr);
    end;

    try
      if ColumnSize < 0 then
        Ptr := Marshal.AllocHGlobal(Size);
      CopyBuffer(SrcPtr, Ptr, Size);
    finally
      FreeString(SrcPtr);
    end;
  end;

var
  i: integer;
  Ptr: IntPtr;
  Size: integer;
  SafeArray: PVarArray;
  Column: TOCILoaderColumn;
  ParamDesc: TOraParamDesc;
  Flag: byte;
  OldDecimalSeparator: Char;
  NumericCharacters: variant;
  ObjRef: TSharedObject;
begin
  if Row > FLoadedRows + FBufNumRows then
    DoLoad;

  inherited;

  if Row <= FLoadedRows then
    RaiseError('Invalid row number');

  if FIsDirectMode then begin
  // Direct
    Column := TOCILoaderColumn(Columns[Col]);
    Ptr := PtrOffset(FBuffer, (Row - FLoadedRows - 1) * FRowSize + Column.FOffset);
    Flag := OCI_DIRPATH_COL_COMPLETE;
    Size := Column.DataSize;

    if VarIsNull(Value) or VarIsEmpty(Value) then
      Flag := OCI_DIRPATH_COL_NULL
    else if Column.FIsLob then begin
      if VarIsStr(Value) then
        PutColumnStr(Value, ConverToUnicode(Column), -1, Size,  Ptr)
      else if VarType(Value) = varByte + varArray then begin
        SafeArray := VarArrayAsPSafeArray(Value);
        Size := SafeArray.Bounds[0].ElementCount;
        if Size = 0 then
          Ptr := nil
        else begin
          Ptr := Marshal.AllocHGlobal(Size);
          Move(SafeArray.Data^, Ptr^, Size);
        end;
      end
      else if VarType(Value) = varSharedObject then begin
        ObjRef := TSharedObject.FromVariant(Value);
        Size := TOraLob(ObjRef).Size;
        if Size = 0 then
          Ptr := nil
        else if TOraLob(ObjRef).LobType = ltBlob then begin
          Ptr := Marshal.AllocHGlobal(Size);
          TOraLob(ObjRef).Read(0, Size, Ptr);
        end
        else
          PutColumnStr(TOraLob(ObjRef).AsString, ConverToUnicode(Column), -1, Size,  Ptr);
      end
      else
        Error(Column);

      FLobBuffer.Add(Ptr);
    end
    else
      case Column.FColumnType of
        SQLT_CHR: begin
          case VarType(Value) of
            varInteger,varByte,varSmallint,varShortInt,varWord,varLongWord:
              PutColumnStr(IntToStr(Value), ConverToUnicode(Column), Column.Size, Size, Ptr);
            varDouble,varSingle,varCurrency: begin
              OldDecimalSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
              FConnection.GetProp(prNumericCharacters, NumericCharacters);
              {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := VarToStr(NumericCharacters)[1];
              try
                {if (Column.Precision > 0) and (Column.Scale > 0) then
                  Str := FloatToStrF(Value, ffFixed, Column.Precision, Column.Scale)
                else
                  Str := FloatToStr(Value);
                }
                PutColumnStr(FloatToStr(Value), ConverToUnicode(Column), Column.Size, Size, Ptr);
              finally
                {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
              end;
            end;
            varDate:
              if Column.DateFormat <> '' then
                 PutColumnStr(FormatDateTime(Column.DateFormat, Value), ConverToUnicode(Column), Column.Size, Size, Ptr)
               else
                 PutColumnStr(DateToStr(Value), ConverToUnicode(Column), Column.Size, Size, Ptr);
          else
            if VarIsStr(Value) then
              PutColumnStr(Value, ConverToUnicode(Column), Column.Size, Size, Ptr)
            else
              Error(Column);
          end;
        end;
        SQLT_INT, SQLT_UIN:
          case VarType(Value) of
            varInteger,varDouble,varCurrency,varSingle,varByte,varSmallint,varShortInt,varWord,varLongWord:
              Marshal.WriteInt32(Ptr, Value);
          else
            if VarIsStr(Value) then
              Marshal.WriteInt32(Ptr, StrToInt(Value))
            else
              Error(Column);
          end;
        SQLT_FLT:
          case VarType(Value) of
            varByte, varShortInt, varSmallint, varWord, varInteger, varLongWord, varInt64,
            varSingle, varDouble, varCurrency:
              Marshal.WriteInt64(Ptr, BitConverter.DoubleToInt64Bits(Value));
          else
            if VarIsStr(Value) then
              Marshal.WriteInt64(Ptr, BitConverter.DoubleToInt64Bits(StrToFloat(Value)))
            else
              Error(Column);
          end;
        SQLT_DAT:
          case VarType(Value) of
            varDate: begin
              DateTimeToOraDate(Value, Ptr);
              Size := 7;
            end
          else
            Error(Column);
          end;
        SQLT_TIMESTAMP,
        SQLT_TIMESTAMP_TZ,
        SQLT_TIMESTAMP_LTZ:
          case VarType(Value) of
            varDate:
              PutColumnStr(FormatDateTime('dd.mm.yyyy hh:nn:ss', Value), ConverToUnicode(Column), 20, Size, Ptr);
          else
          {$IFNDEF FPC}
            if VarIsSQLTimeStamp(Value) then
              PutColumnStr(SQLTimeStampToStr('dd.mm.yyyy hh:nn:ss', VarToSQLTimeStamp(Value)), ConverToUnicode(Column), 20, Size, Ptr)
            else
          {$ENDIF}
              Error(Column);
          end;
      else
        Error(Column);
      end;

    Check(OCI8.OCIDirPathColArrayEntrySet(hColumnArray, OCISvcCtx.hOCIError, Row - FLoadedRows - 1,
      Col, Ptr, Size, Flag));
  end
  else begin
    // DML
    i := (Row - 1) mod FBufNumRows;
    ParamDesc := TOraParamDesc(FDML.Params[Col]);
    ParamDesc.AllocBuffer;
  {$IFNDEF ONLY_UTF8_MEMO}
    if ParamDesc.GetDataType in [dtOraClob, dtWideOraClob, dtNClob] then
      TOraLob(ParamDesc.GetItemObject(i)).IsUnicode := FConnection.FUseUnicode;
  {$ENDIF}
    ParamDesc.SetNational(TOCILoaderColumn(Columns[Col]).IsNational);
    ParamDesc.SetItemValue(i, Value);
  end;
end;

procedure TOCILoader.DoLoad;
var
  Res: sword;
  ArrRowCount: integer; // count of rows in array
  RowOffset: integer;
  PieceRows: integer; // count of rows in piece
  RowCount: integer;
  Action: _TDPErrorAction;
  ColCount: word;
  ValueInt: Integer;
begin
  if FIsDirectMode then begin
    /// Loading via Oracle direct path interface
    RowOffset := 0;
    ColCount := 0;
    ArrRowCount := FLastRow - FLoadedRows + 1;
    Action := _dpFail;
    repeat
      Res := OCI8.OCIDirPathColArrayToStream(hColumnArray, hDirPathCtx, hStream,
        OCISvcCtx.hOCIError, ArrRowCount, RowOffset);

      Check(OCI8.OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY, ValueInt, nil,
        OCI_ATTR_ROW_COUNT, OCISvcCtx.hOCIError));
      RowCount := Integer(ValueInt);

      if OCISvcCtx.Home.OCIVersion >= 8170 then // Difference of 8.1.7
        PieceRows := RowCount
      else
        PieceRows := RowCount - RowOffset;

      if Res <> OCI_SUCCESS then begin
      /// Stream buffer is not large enough to contain all of the column array
      /// data (OCI_CONTINUE), partial column was encountered (OCI_NEED_DATA)
      /// or convert error ocured (OCI_ERROR).
        if OCISvcCtx.Home.OCIVersion >= 8170 then // Difference of 8.1.7
          Inc(RowOffset, RowCount)
        else
          RowOffset := RowCount;

        if Res = OCI_ERROR then begin
          /// process convert error
          try
            Check(OCI8.OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
              ValueInt, nil, OCI_ATTR_COL_COUNT, OCISvcCtx.hOCIError));
            ColCount := Word(ValueInt);
            Check(Res);
          except
            on E: Exception do begin
              Action := _dpFail;
              if Assigned(FOnError) then
                FOnError(E, ColCount, FLoadedRows + RowCount, Action);
              case Action of
                _dpFail:
                  raise;
                _dpAbort:
                  Break;
              end;
            end;
          end;
          /// Skip row which caused error
          Inc(PieceRows);
          Inc(RowOffset);
        end;
      end;

      Res := OCI8.OCIDirPathLoadStream(hDirPathCtx, hStream, OCISvcCtx.hOCIError);

      if Res = OCI_ERROR then begin
        /// process load error
        ColCount := 0;
        try
          Check(OCI8.OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
            ValueInt, nil, OCI_ATTR_ROW_COUNT, OCISvcCtx.hOCIError));
          RowCount := Integer(ValueInt);
          Check(OCI8.OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
            ValueInt, nil, OCI_ATTR_COL_COUNT, OCISvcCtx.hOCIError));
          ColCount := Word(ValueInt);
          Check(Res);
        except
          on E: Exception do begin
            Action := _dpFail;
            if Assigned(FOnError) then
              FOnError(E, ColCount, FLoadedRows + RowCount, Action);
            case Action of
              _dpFail:
                raise;
              _dpAbort:
                Break;
            end;
          end;
        end;
      end;
      /// reset stream
      Check(OCI8.OCIDirPathStreamReset(hStream, OCISvcCtx.hOCIError));
      Inc(FLoadedRows, PieceRows);
    until FLoadedRows >= FLastRow + 1;

    if Action <> _dpAbort then
      Assert(FLoadedRows = FLastRow + 1);

    /// reset column array
    Check(OCI8.OCIDirPathColArrayReset(hColumnArray, OCISvcCtx.hOCIError));
    ClearLobBuffer;

    if Action = _dpAbort then
      Abort;
  end
  else begin
    // Loading via DML array
    ArrRowCount := FLastRow - FLoadedRows + 1;
    FDML.FBatchIters := ArrRowCount;
    FDML.FBatchOffset := 0;
    FDML.Execute;
    Inc(FLoadedRows, ArrRowCount);
  end;
end;

class function TOCILoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TOCILoaderColumn;
end;

procedure TOCILoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TOCIConnection(Value);
end;

procedure TOCILoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  inherited;

  with TOCILoaderColumn(Column) do begin
    case FieldDesc.DataType of
      dtNumber: begin
        Size := 0;
        Precision := FieldDesc.Length;
        Scale := FieldDesc.Scale;
      end;
      dtTime: begin
        FDateFormat := 'hh24:mi:ss';
      end;
      dtDate: begin
        FDateFormat := 'dd.mm.yyyy';
      end;
      dtDateTime:
        FDateFormat := 'dd.mm.yyyy hh24:mi:ss';
      dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
        FDateFormat := 'dd.mm.yyyy hh24:mi:ss';
    end;

    FIsNational := TOCIFieldDesc(FieldDesc).IsNational;
  end;
end;

function TOCILoader.ConverToUnicode(Column: TOCILoaderColumn): boolean;
begin
  // Loader allows to use Unicode for Oracle 11 client and higher
  // or Oracle 10 with UnicodeEnvironment
  Result := FConnection.FUseUnicode and
            (Column.DataType in [dtString, dtWideString,
                                 dtFixedChar, dtFixedWideChar,
                                 dtOraClob, dtWideOraClob, dtNClob]) and
            (FConnection.FOCISvcCtx.Home.OCIVersion >= 11000);
end;

procedure TOCILoader.CheckLobBuffer;
begin
  if FLobBuffer = nil then
    FLobBuffer := TList.Create;
end;

procedure TOCILoader.ClearLobBuffer;
var
  i: integer;
begin
  if FLobBuffer <> nil then begin
    for i := 0 to FLobBuffer.Count - 1 do
      Marshal.FreeHGlobal(IntPtr(FLobBuffer.Items[i]));
    FLobBuffer.Clear;
  end;
end;

procedure TOCILoader.FreeLobBuffer;
begin
  if FLobBuffer <> nil then begin
    ClearLobBuffer;
    FLobBuffer.Free;
    FLobBuffer := nil;
  end;
end;

{ TOCIAlerter }

constructor TOCIAlerter.Create;
begin
  inherited;

  FListenConnection := TOCIConnection.Create;
  FWaitCommand := TOCICommand(FListenConnection.CreateCommand);
{$IFDEF MSWINDOWS}
  FResponseEvent := TEvent.Create(nil, True, False, '');
  FIntervalEvent := TEvent.Create(nil, True, False, '');
{$ENDIF}

  FTimeOut := -1;
  FInterval := 0;
  FEventType := _etAlert;
  FSelfMessage := msBlank;
  FAutoCommit := True;
  FSelfEvents := True;
end;

destructor TOCIAlerter.Destroy;
begin
  Stop;

  FWaitCommand.Free;
  FListenConnection.Free;
{$IFDEF MSWINDOWS}
  FResponseEvent.Free;
  FIntervalEvent.Free;
{$ENDIF}

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

function TOCIAlerter.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      FAutoCommit := Value;
    prAlerterTimeout:
      FTimeOut := Value;
    prAlerterInterval:
      FInterval := Value;
    prAlerterEventType:
      FEventType := _TEventType(Value);
    prAlerterSelfEvents:
      FSelfEvents := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCIAlerter.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      Value := FAutoCommit;
    prAlerterTimeout:
      Value := FTimeOut;
    prAlerterInterval:
      Value := FInterval;
    prAlerterEventType:
      Value := Variant(FEventType);
    prAlerterSelfEvents:
      Value := FSelfEvents;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TOCIAlerter.SendEvent(const EventName, Message: string);
begin
  case FEventType of
    _etAlert: begin
      SendAlertMessage(EventName, Message);
   end;
    _etPipe: begin
      PackMessage(Message);
      SendPipeMessage(EventName);
    end;
  end;
end;

procedure TOCIAlerter.Start;
var
  Name: string;
  i: integer;
  SQL: string;
begin
  if FActive then
    exit;

  Assert(FEventNames.Count > 0);

{$IFDEF MSWINDOWS}
  AllocODACWnd;
{$ENDIF}

  FListenConnection.Assign(TOCIConnection(FConnection));
  FListenConnection.OnError := nil;
  FListenConnection.Connect('');

  if (FEventType = _etAlert) and not FRegistered then
    RegisterEvents;

  case FEventType of
    _etAlert: begin
      SQL := '';
      if OCISvcCtx.Home.OCIVersion div 10 = 805 then begin  // fix Oracle 8.0.5 bug that unregister events, need to check server version
        for i := 0 to FEventNames.Count - 1 do begin
          Name := Trim(FEventNames[i]);
          SQL := SQL + '  DBMS_ALERT.Register(''' + Name + ''');' + DALineSeparator;
        end;
      end;

      FWaitCommand.SetSQL('begin ' + SQL + 'DBMS_ALERT.WaitAny(:Name, :Message, :Status, :TimeOut); end;');
      with FWaitCommand.Params[0] do begin
        SetParamType(pdOutput);
        SetSize(TOCIConnection(FConnection).GetMaxStringSize);
        if TOCIConnection(FConnection).FUseUnicode then
          SetDataType(dtWideString)
        else
          SetDataType(dtString);
      end;
      with FWaitCommand.Params[1] do begin
        SetParamType(pdOutput);
        SetSize(TOCIConnection(FConnection).GetMaxStringSize);
        if TOCIConnection(FConnection).FUseUnicode then
          SetDataType(dtWideString)
        else
          SetDataType(dtString);
      end;
      with FWaitCommand.Params[2] do begin
        SetParamType(pdOutput);
        SetDataType(dtInteger);
      end;
      with FWaitCommand.Params[3] do begin
        SetParamType(pdInput);
        SetDataType(dtInteger);
        if FTimeOut >= 0 then
          Value := FTimeOut
        else
          Value := 86400000; // 1000 days
      end;
    end;
    _etPipe: begin
      FWaitCommand.SetSQL('begin :Status := DBMS_PIPE.Receive_Message(:Name, :TimeOut); end;');

      with FWaitCommand.Params[0] do begin
        SetParamType(pdOutput);
        SetDataType(dtInteger);
      end;

      // get first pipe from list
      Name := FEventNames[0];

      with FWaitCommand.Params[1] do begin
        SetParamType(pdInput);
        if TOCIConnection(FConnection).FUseUnicode then begin
          SetDataType(dtWideString);
          SetSize(Length(WideString(Name)));
        end
        else begin
          SetDataType(dtString);
          SetSize(Length(AnsiString(Name)));
        end;
        Value := Name;
      end;
      with FWaitCommand.Params[2] do begin
        SetParamType(pdInput);
        SetDataType(dtInteger);
        if FTimeOut >= 0 then
          Value := FTimeOut
        else
          Value := 86400000; // 1000 days
      end;
    end;
  end;

  FActive := True;
  try
    FWaitCommand.Prepare;

    FListenThread := TOCIAlerterListenThread.Create(Self);
  except
    FActive := False;
    raise;
  end;
end;

procedure TOCIAlerter.Stop;
begin
  if FActive then begin
    FActive := False;

    try
      FListenThread.Terminate;
    finally
      FListenThread.WaitFor;
      FreeAndNil(FListenThread);
    end;

    try
      try
        FWaitCommand.UnPrepare;
        if FEventType = _etAlert then
          RemoveEvents;
      finally
        FListenConnection.Disconnect;
      end;
    except
    end;
  end;
end;

procedure TOCIAlerter.PackMessage(const Item: variant);
var
  Command: TCRCommand;
begin
  if (FEventType = _etPipe) and (Item <> Null) then begin
    Command := TOCIConnection(FConnection).GetCommand;
    try
      Command.SetSQL('begin DBMS_PIPE.Pack_Message(:Item); end;');

      with Command.Params[0] do begin
        SetParamType(pdInput);
        SetDataType(dtUnknown);
        Value := Item;
      end;

      Command.Execute;
    finally
      TOCIConnection(FConnection).ReleaseCommand(Command);
    end;
  end;
end;

function TOCIAlerter.UnpackMessage(var Item: variant): boolean;
var
  Command: TCRCommand;
  DataType: word;
  Size: integer;
begin
  Result := False;
  Size := 0;
  case NextItemType of
    mtNone: begin
      Item := Null;
      Result := True;
      Exit;
    end;
    mtNumber:
      DataType := dtFloat;
    mtString: begin
      if TOCIConnection(FListenConnection).FUseUnicode then
        DataType := dtWideString
      else
        DataType := dtString;
      Size := TOCIConnection(FListenConnection).GetMaxStringSize;
    end;
    mtDate:
      DataType := dtDateTime;
  else
    DataType := dtString;
  end;

  Command := TOCIConnection(FListenConnection).GetCommand;
  try
    Command.SetSQL('begin DBMS_PIPE.Unpack_Message(:Item); end;');

    with Command.Params[0] do begin
      SetParamType(pdOutput);
      SetDataType(DataType);
      SetSize(Size);
    end;

    Command.Execute;

    Item := Command.Params[0].GetValue;
  finally
    TOCIConnection(FListenConnection).ReleaseCommand(Command);
  end;
end;

function TOCIAlerter.NextItemType: TMessageType;
var
  Command: TCRCommand;
  Res: integer;
begin
  Command := TOCIConnection(FListenConnection).GetCommand;
  try
    Command.SetSQL('begin :Result := DBMS_PIPE.Next_Item_Type; end;');

    with Command.Params[0] do begin
      SetParamType(pdOutput);
      SetDataType(dtInteger);
    end;

    Command.Execute;

    Res := Command.Params[0].GetValue;
    case Res of
      0: Result := mtNone;
      6: Result := mtNumber;
      9: Result := mtString;
      12: Result := mtDate;
    else
      Result := mtNone;
    end;
  finally
    TOCIConnection(FListenConnection).ReleaseCommand(Command);
  end;
end;

procedure TOCIAlerter.SendPipeMessage(const Name: string);
var
  Command: TCRCommand;
  Res: integer;
  AName: string;
begin
  Assert(FEventNames.Count > 0);

  AName := Name;
  if AName = '' then
    AName := FEventNames[0];

  Command := TOCIConnection(FConnection).GetCommand;
  try
    Command.SetSQL('begin :Result := DBMS_PIPE.Send_Message(:Name); end;');

    with Command.Params[0] do begin
      SetParamType(pdOutput);
      SetDataType(dtInteger);
    end;
    with Command.Params[1] do begin
      SetParamType(pdInput);
      if TOCIConnection(FConnection).FUseUnicode then begin
        SetDataType(dtWideString);
        SetSize(Length(WideString(AName)));
      end
      else begin
        SetDataType(dtString);
        SetSize(Length(AnsiString(AName)));
      end;
      Value := AName;
    end;

    Command.Execute;

    Res := Command.Params[0].GetValue;
    if Res <> 0 then
      raise Exception.Create(SAlerterSendFailed);
  finally
    TOCIConnection(FConnection).ReleaseCommand(Command);
  end;
end;

procedure TOCIAlerter.PurgePipe;
var
  Command: TCRCommand;
  AName: string;
begin
  Assert(FEventNames.Count > 0);

  AName := FEventNames[0];

  Command := TOCIConnection(FConnection).GetCommand;
  try
    Command.SetSQL('begin :Result := DBMS_PIPE.Purge(:Name); end;');

    with Command.Params[0] do begin
      SetParamType(pdOutput);
      SetDataType(dtInteger);
    end;
    with Command.Params[1] do begin
      SetParamType(pdInput);
      if TOCIConnection(FConnection).FUseUnicode then begin
        SetDataType(dtWideString);
        SetSize(Length(WideString(AName)));
      end
      else begin
        SetDataType(dtString);
        SetSize(Length(AnsiString(AName)));
      end;
      Value := AName;
    end;

    Command.Execute;
  finally
    TOCIConnection(FConnection).ReleaseCommand(Command);
  end;
end;

function TOCIAlerter.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

function TOCIAlerter.GetOCISvcCtx: TOCISvcCtx;
begin
  Result := TOCIConnection(FConnection).OCISvcCtx;
end;

procedure TOCIAlerter.RegisterEvents;
var
  Name: string;
  i: integer;
  SQL: string;
  Command: TCRCommand;
begin
  Assert(FEventNames.Count > 0);

  if FEventType = _etAlert then begin
    SQL := '';
    for i := 0 to FEventNames.Count - 1 do begin
      Name := Trim(FEventNames[i]);
      SQL := SQL + '  DBMS_ALERT.Register(''' + Name + ''');' + DALineSeparator;
    end;

    Command := TOCIConnection(FListenConnection).GetCommand;
    try
      Command.SetSQL('begin' + DALineSeparator + SQL + 'end;');
      try
        Command.Execute;
      except
        on E: EOraError do begin
          if E.ErrorCode = 6550 then
            raise EOraError.Create(E.ErrorCode,
              'Please execute GRANT EXECUTE ON DBMS_ALERT TO ' +
              FConnection.GetUsername + ' as SYS' + DALineSeparator + DALineSeparator + E.Message);
          raise;
        end;
      end;
    finally
      TOCIConnection(FListenConnection).ReleaseCommand(Command);
    end;
  end;

  FRegistered := True;
end;

procedure TOCIAlerter.RemoveEvents;
var
  Name: string;
  i: integer;
  SQL: string;
  Command: TCRCommand;
begin
  Assert(FEventNames.Count > 0);

  FRegistered := False;
  if FEventType = _etAlert then begin
    SQL := '';
    for i := 0 to FEventNames.Count - 1 do begin
      Name := Trim(FEventNames[i]);
      SQL := SQL + '  DBMS_ALERT.Remove(''' + Name + ''');' + DALineSeparator;
    end;

    Command := TOCIConnection(FListenConnection).GetCommand;
    try
      Command.SetSQL('begin' + DALineSeparator + SQL + 'end;');
      Command.Execute;
    finally
      TOCIConnection(FListenConnection).ReleaseCommand(Command);
    end;
  end;
end;

procedure TOCIAlerter.SendAlertMessage(const EventName, Message: string);
var
  Command: TCRCommand;
begin
  if not FSelfEvents then
    FSelfMessage := Message;
  Command := TOCIConnection(FConnection).GetCommand;
  try
    Command.SetSQL('begin DBMS_ALERT.Signal(:Name, :Message); end;');

    with Command.Params[0] do begin
      SetParamType(pdInput);
      if TOCIConnection(FConnection).FUseUnicode then begin
        SetDataType(dtWideString);
        SetSize(Length(WideString(EventName)));
      end
      else begin
        SetDataType(dtString);
        SetSize(Length(AnsiString(EventName)));
      end;
      Value := EventName;
    end;

    with Command.Params[1] do begin
      SetParamType(pdInput);
      if TOCIConnection(FConnection).FUseUnicode then begin
        SetDataType(dtWideString);
        SetSize(Length(WideString(Message)));
      end
      else begin
        SetDataType(dtString);
        SetSize(Length(AnsiString(Message)));
      end;
      Value := Message;
    end;

    Command.Execute;
  finally
    TOCIConnection(FConnection).ReleaseCommand(Command);
  end;

  if FAutoCommit and TOCIConnection(FConnection).AutoCommit then
    FConnection.GetInternalTransaction.Commit;
end;

function TOCIAlerter.ProcessWaitResult: boolean;
var
  Status: integer;
begin
  Status := FWaitCommand.Params.ParamByName('Status').GetValue;
  if Status = 0 then begin
    While not ProcessMessage do;
    Result := True;
  end
  else
    Result := ProcessError(Status);
end;

function TOCIAlerter.ProcessMessage: boolean;
var
  Event: TAlertEvent;
  EventName: string;
  EventMessage: string;
  Item: variant;
begin
  Result := True;

  case FEventType of
    _etAlert: begin
      EventName := VarToStr(FWaitCommand.Params.ParamByName('Name').GetValue);
      EventMessage := VarToStr(FWaitCommand.Params.ParamByName('Message').GetValue);
    end;
    _etPipe: begin
      Result := UnpackMessage(Item);
      if Result then
        Exit
      else begin
        EventName := FEventNames[0];
        EventMessage := VarToStr(Item);
      end;
    end;
  end;

  if EventMessage = FSelfMessage then begin
    FSelfMessage := msBlank;
  end
  else
  if EventMessage = msTerminate then begin
    // Empty
  end
  else begin
    Event := TAlertEvent.Create;
    try
      Event.Name := EventName;
      Event.Message := EventMessage;

    {$IFDEF MSWINDOWS}
      FResponseEvent.ResetEvent;
      PostMessage(hODACWindow, WM_ALERTER_EVENT, WPARAM(GCHandle),
        LPARAM(AllocGCHandle(Event)));
      FResponseEvent.WaitFor(INFINITE);
    {$ELSE}
      try
        CallEvent(Event);
      finally
        Event.Free;
      end;
    {$ENDIF}
    except
      Event.Free;
      raise;
    end;
  end;
end;

function TOCIAlerter.ProcessError(Status: integer): boolean;
begin
  if Status = 1 then begin
    if FTimeOut >= 0 then begin
      Result := False;
    {$IFDEF MSWINDOWS}
      FResponseEvent.ResetEvent;
      PostMessage(hODACWindow, WM_ALERTER_TIMEOUT, WPARAM(GCHandle),
        LPARAM(@Result));
      FResponseEvent.WaitFor(INFINITE);
    {$ELSE}
      DoOnTimeout(Result);
    {$ENDIF}
    end
    else
      Result := True;
  end
  else
    raise Exception.CreateFmt(SAlerterReceiveFailed, [Status]);
end;

{$IFDEF MSWINDOWS}
procedure TOCIAlerter.DoOnEvent(const EnentName, Message: string);
begin
  if Assigned(OnEvent) then
    OnEvent(EnentName, Message);
end;
{$ELSE}
procedure TOCIAlerter.DoOnEvent;
begin
  Assert(Assigned(OnEvent) and (FLastEvent <> nil));
  OnEvent(FLastEvent.Name, FLastEvent.Message);
end;

procedure TOCIAlerter.CallEvent(Event: TAlertEvent);
begin
  if Assigned(OnEvent) then begin
    FLastEvent := Event;
    try
      FListenThread.Synchronize(DoOnEvent);
    finally
      FLastEvent := nil;
    end;
  end;
end;
{$ENDIF}

procedure TOCIAlerter.DoOnTimeout(var Continue: boolean);
begin
  if Assigned(OnTimeout) then
    OnTimeout(Continue);
end;

{$IFDEF MSWINDOWS}
procedure TOCIAlerter.DoOnError(E: Exception);
begin
  if Assigned(OnError) then
    OnError(E);
end;
{$ENDIF}

{ TOCIAlerterListenThread }

constructor TOCIAlerterListenThread.Create(Alerter: TOCIAlerter);
begin
  inherited Create(True);

  FAlerter := Alerter;
{$IFNDEF FPC}
  Resume;
{$ELSE}
  Start;
{$ENDIF}
end;

procedure TOCIAlerterListenThread.Terminate;
var
  Event: string;
begin
  inherited Terminate;

{$IFDEF MSWINDOWS}
  FAlerter.FResponseEvent.SetEvent;
  FAlerter.FIntervalEvent.SetEvent;
{$ENDIF}

  Event := FAlerter.EventNames[0];
  FAlerter.SendEvent(Event, msTerminate);
  if (FAlerter.FEventType = _etAlert) and
    not (FAlerter.FAutoCommit and TOCIConnection(FAlerter.Connection).AutoCommit)
  then
    FAlerter.Connection.GetInternalTransaction.Commit;

{$IFDEF MSWINDOWS}
  if WaitForSingleObject(Handle, 1000) = WAIT_TIMEOUT	then
    FAlerter.FListenConnection.BreakExec;
{$ENDIF}
end;

procedure TOCIAlerterListenThread.Execute;
var
  Continue: boolean;
begin
  try
    repeat
      FAlerter.FWaitCommand.Execute;

      if not Terminated then begin
        Continue := FAlerter.ProcessWaitResult;

        if not Continue and not Terminated then begin
          if FAlerter.FInterval > 0 then
          {$IFDEF MSWINDOWS}
          begin
            FAlerter.FIntervalEvent.ResetEvent;
            FAlerter.FIntervalEvent.WaitFor(FAlerter.FInterval * 1000);
            Continue := not Terminated;
          end
          else
            PostMessage(hODACWindow, WM_ALERTER_STOPED, WPARAM(FAlerter.GCHandle), 0);
          {$ELSE}
          begin
            Sleep(FAlerter.FInterval * 1000);
            Continue := not Terminated;
          end;
          {$ENDIF}
        end;
      end
      else
        Continue := False;

    until not Continue;
  except
  {$IFDEF MSWINDOWS}
    on E: Exception do begin
      if not Terminated then begin
        AcquireExceptionObject;
        PostMessage(hODACWindow, WM_ALERTER_STOPED, WPARAM(FAlerter.GCHandle),
          LPARAM(AllocGCHandle(E)));
      end;
    end;
  {$ENDIF}
  end;
end;

{$ENDIF}

{ TOraClassesUtils }

class procedure TOraClassesUtils.InternalUnPrepare(Obj: TOCIRecordSet);
begin
  Obj.InternalUnPrepare;
end;

{$IFDEF FPC}
function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;
{$IFDEF MSWINDOWS}
var
  L: Integer;
  Buffer: array[0..255] of Char;
begin
  L := GetLocaleInfo(Locale, LocaleType, Buffer, SizeOf(Buffer));
  if L > 0 then SetString(Result, Buffer, L - 1) else Result := Default;
end;
{$ENDIF}
{$IFNDEF MSWINDOWS}
begin
  Result := Default;
end;
{$ENDIF}
{$ENDIF}

procedure ParseConnectString(const ConnectString: string;
  var Username, Password, Server: string; var ConnectMode: TConnectMode);
var
  i, p, Mode: integer;
  s: string;
  c: Char;
  Quoted: boolean;
begin
  Username := '';
  Password := '';
  Server := '';
  ConnectMode := cmNormal;

  // 1. bypass blanks
  // 2. go to the end of name
  // 3. go to delimiter ('/', '@') and detect next mode

  Mode := 0;
  i := 1;
  while True do begin
    // 1.
    while (i <= Length(ConnectString)) and (ConnectString[i] = ' ') do
      Inc(i);
    if i > Length(ConnectString) then
      exit;

    // 2.
    p := i;
    Quoted := ConnectString[i] = '"';
    if Quoted then begin
      Inc(i);
      while i <= Length(ConnectString) do begin
        c := ConnectString[i];
        Inc(i);
        if c = '"' then
          break;
      end;
    end
    else begin
      while i <= Length(ConnectString) do begin
        case ConnectString[i] of
          ' ', '"':
            break;
          '/', '@':
            if Mode < 2 then
              // Server can contain any characters except space and "
              break;
        end;
        Inc(i);
      end
    end;

    s := Copy(ConnectString, p, i - p);
    case Mode of
      0:
        Username := s;
      1:
        Password := OCISQLInfo.UnQuote(s); // Password is case-insensitive; Direct mode doen't support quotes
      2:
        Server := OCISQLInfo.UnQuote(s); // Quoted server name is not supported by OCI
      3: begin
        s := UpperCase(s);
        if s = 'SYSDBA' then
          ConnectMode := cmSysDBA
        else
        if s = 'SYSOPER' then
          ConnectMode := cmSysOper
        else
        if s = 'SYSASM' then
          ConnectMode := cmSysASM
        else
        if s = 'SYSBACKUP' then
          ConnectMode := cmSysBackup
        else
        if s = 'SYSDG' then
          ConnectMode := cmSysDG
        else
        if s = 'SYSKM' then
          ConnectMode := cmSysKM
        else
          RaiseError(SInvalidConnectString);
      end;
    end;

    // 3.
    p := i;
    while (i <= Length(ConnectString)) and (ConnectString[i] = ' ') do
      Inc(i);
    if i > Length(ConnectString) then
      exit;
    if (Mode < 1) and (ConnectString[i] = '/') then
      Mode := 1
    else
    if (Mode < 2) and (ConnectString[i] = '@') then
      Mode := 2
    else
    // at least one space must be before and after AS
    if (Mode < 3) and (i > p) and (UpperCase(Copy(ConnectString, i, 3)) = 'AS ') then
      Mode := 3
    else
      exit;
    if Mode = 3 then
      Inc(i, 3)
    else
      Inc(i);
  end;
end;

initialization

{$IFNDEF MSWINDOWS}
  hLockConnect := TCriticalSection.Create;
{$ENDIF}
  OCISQLInfo := TOCISQLInfo.Create(TOraParser);
  OCIConnections := TConnectionList.Create;

  OCICallbackDefinePtr := @OCICallbackDefine;
  OCICallbackInBindPtr := @OCICallbackInBind;
  OCICallbackOutBindPtr := @OCICallbackOutBind;
  OCICallbackFailoverPtr := @OCICallbackFailover;
{$IFDEF MSWINDOWS}
  OCICallbackChangeNotifyPtr := @OCICallbackChangeNotify;
{$ENDIF}

finalization
  if OCIConnections <> nil then begin
    OCIConnections.DisconnectAll;
    OCIConnections.Free;
    OCIConnections := nil;
  end;

  if GlobalBuf <> nil then
    Marshal.FreeHGlobal(GlobalBuf);
{$IFNDEF MSWINDOWS}
  hLockConnect.Free;
{$ENDIF}
  OCISQLInfo.Free;
{$IFDEF MSWINDOWS}
  if hODACWindow <> 0 then
    DestroyWindow(hODACWindow);
{$ENDIF}

end.
