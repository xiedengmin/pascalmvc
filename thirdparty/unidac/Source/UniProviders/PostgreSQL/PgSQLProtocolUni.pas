
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I PgDac.inc}
unit PgSQLProtocolUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, Types, SyncObjs,
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
  CRFunctions, CLRClasses,
  CRTypes, CRVio, CRAccess,
{$IFDEF MSWINDOWS}
  CRSspi,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  PgSQLNet, PgCall, PgError;
{$ELSE}
  PgSQLNetUni, PgCallUni, PgErrorUni;
{$ENDIF}

const
 { PostgreSQL protocol messages }

  PG_MSG_ASCIIROW = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('D');
  PG_MSG_AUTHENTICATION = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('R');
  PG_MSG_PASSWORD = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('p');

  //Authentication request types
  PG_MSG_AUTHENTICATION_OK = 0;
  PG_MSG_AUTHENTICATION_KERBEROSV4 = 1;
  PG_MSG_AUTHENTICATION_KERBEROSV5 = 2;
  PG_MSG_AUTHENTICATION_CLEARTEXTPASSWORD = 3;
  PG_MSG_AUTHENTICATION_CRYPTPASSWORD = 4;
  PG_MSG_AUTHENTICATION_MD5PASSWORD = 5;
  PG_MSG_AUTHENTICATION_SCMCREDENTIAL = 6;
  PG_MSG_AUTHENTICATION_SSPI = 9;
  PG_MSG_AUTHENTICATION_SCRAM = 10;

  PG_MSG_BACKENDKEYDATA = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('K');
  PG_MSG_BINARYROW = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('B');

  PG_MSG_COMPLETED_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('C');
  PG_MSG_CURSOR_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('P');
  PG_MSG_EMPTY_QUERY_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('I');
  PG_MSG_ERROR_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('E');
  PG_MSG_FUNCTION_CALL = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('F');
  PG_MSG_FUNCTION_RESULT_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('V');

  PG_MSG_FUNCTION_RESULT_NONEMPTY_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('G');
  PG_MSG_FUNCTION_RESULT_VOID_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('0');
  PG_MSG_NOTICE_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('N');
  PG_MSG_NOTIFICATION_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('A');

  PG_MSG_QUERY = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('Q');
  PG_MSG_READY_FOR_QUERY = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('Z');
  PG_MSG_ROW_DESCRIPTION = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('T');
  PG_MSG_TERMINATE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('X');

  PG_MSG_COPY_DATA = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('d');
  PG_MSG_COPY_DONE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('c');
  PG_MSG_COPY_FAIL = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('f');
  PG_MSG_COPYIN_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('G');
  PG_MSG_COPYOUT_RESPONSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('H');

  PG_MSG_FUNCTION_RESULT = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('V');

  // Protocol 3.0 only
  PG_MSG_PARAMETER_STATUS = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('S');
  PG_MSG_PARSE_COMPLETE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('1');
  PG_MSG_BIND_COMPLETE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('2');
  PG_MSG_CLOSE_COMPLETE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('3');
  PG_MSG_PARAMETER_DESCRIPTION = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('t');
  PG_MSG_PORTAL_SUSPENDED = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('s');
  PG_MSG_NODATA = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('n');

  PG_SSL_REQUEST_CODE = 80877103;
  PG_MSG_SSL_RESPONSE_OK = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('S');

  PG_SSPI_REQUEST_CODE = $70;
  PG_MSG_SSPI_CONTINUE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('R');

  PG_MSG_SCRAM_CONTINUE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('R');

  PG_COMMAND_HOLD = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('H');
  PG_COMMAND_SYNC = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('S');
  PG_COMMAND_CLOSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('C');
  PG_COMMAND_PARSE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('P');
  PG_COMMAND_DESCRIBE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('D');
  PG_COMMAND_BIND = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('B');
  PG_COMMAND_EXECUTE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('E');
  PG_COMMAND_PORTAL = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('P');
  PG_COMMAND_STATEMENT = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('S');

  PG_ERROR_FIELD_SEVERITY = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('S');
  PG_ERROR_FIELD_CODE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('C');
  PG_ERROR_FIELD_MESSAGE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('M');
  PG_ERROR_FIELD_DETAILS = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('D');
  PG_ERROR_FIELD_HINT = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('H');
  PG_ERROR_FIELD_POSITION = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('P');
  PG_ERROR_FIELD_LINE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('L');
  PG_ERROR_FIELD_WHERE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('W');
  PG_ERROR_FIELD_FILE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('F');
  PG_ERROR_FIELD_ROUTINE = {$IFDEF NEXTGEN}AnsiChar{$ENDIF}('R');

type
  TPgSQLProtocol = class;
  TPgSQLProtocolClass = class of TPgSQLProtocol;
  TPgSQLReadStream = class;
  TPgSQLReader = class;

  TPgProtocolState = (psNone, psError, psAuthenticating, psAuthenticated, psBackendParemeters, psIsReadyForQuery,
    psStmtPreparing, psStmtBinding, psStmtExecuting, psStmtFetching, psFunctionExecuting, psFunctionExecuted,
    psCopyIn);

  TPgProtocolNoticeEvent = procedure(Errors: TPgErrors) of object;
  TPgProtocolNotificationEvent = procedure(const Name: string; const PID: integer; const PayLoad: string) of object;

  TBindParamProc = procedure(Dest: TPgSQLNet; ParamNo: integer; var ItemDesc: TPgSQLItemDesc) of object;
  TFetchFieldProc = procedure(Source: TPgBufferProvider; Size: integer; FetchBlock: IntPtr; RowNo, FieldNo: integer) of object;


{ TPgSQLStatement }

  TPgSQLStatement = class
  private
    FSimpleQueryExecute: boolean;

    FStatementID,
    FStatementName,
    FPortalName: string;

    FParsedSQLType: TParsedSQLType;
    FStmtType: string;

    FLastInsertOID: Int64;
    FRowsAffected: integer;

    FFetchAll: boolean;
    FFetchSize: integer;
    FPrefetchSize: Integer;
    FPrepared: boolean;
    FWithHold: boolean;
    FNoData: boolean;

    FParamsCount: Integer;
    FParams: TPgSQLItemDescs;

    FFieldsCount: Integer;
    FFields: TPgSQLItemDescs;
    FFieldNulls: TBytes;

    FFinalSQL: string;
    FParamTypes: TIntegerDynArray;

    FReadStream: TPgSQLReadStream;
  public
    constructor Create;
    destructor Destroy; override;

    property FetchAll: boolean read FFetchAll write FFetchAll;
    property FetchRows: Integer read FFetchSize write FFetchSize;
    property PrefetchRows: Integer read FPrefetchSize write FPrefetchSize;
    property Prepared: boolean read FPrepared write FPrepared;
    property WithHold: boolean read FWithHold write FWithHold;

    property ReadStream: TPgSQLReadStream read FReadStream;
  end;

  PPgSQLStatement = IntPtr;

{ TPgSQLResultSet }   

  TPgSQLResultSet = class
  private
    FProtocol: TPgSQLProtocol;

    FValues: TStrings;
    FRecordsCount: integer;
    FFieldsCount: integer;

    procedure FetchFieldProc(Source: TPgBufferProvider; Size: integer; FetchBlock: IntPtr;
      Row, FieldNo: integer);
    function GetValue(FieldNo, RecordNo: integer): string;
  public
    constructor Create(Protocol: TPgSQLProtocol);
    destructor Destroy; override;

    procedure Open(const SQL: string);

    property RecordsCount: integer read FRecordsCount;
    property Value[FieldNo, RecordNo: integer]: string read GetValue;
  end;

{ TPgSQLLargeObjectsAPI }  

  TPgSQLLargeObjectsAPI = class
    lo_creat: OID;
    lo_create: OID;
    lo_open: OID;
    lo_write: OID;
    lo_read: OID;
    lo_lseek: OID;
    lo_tell: OID;
    lo_close: OID;
    lo_unlink: OID;
  end;

  TPgSQLFuncDataType = (fdtInteger, fdtBuffer);

  TPgSQLFuncParam = record
    IsNull: boolean;
    Size: integer;
    case DataType: TPgSQLFuncDataType of
      fdtInteger: (VInteger: integer);
      fdtBuffer: (VPointer: IntPtr);
  end;

{$HPPEMIT 'struct TPgSQLFuncParam;'}

  TPgSQLFuncParams = array of TPgSQLFuncParam;

  _TSSLMode = (_smDisable, _smRequire, _smPrefer, _smAllow, _smVerifyCA, _smVerifyFull);

  TPgSQLCriticalSection = class(TCriticalSection);

{ TPgSQLProtocol }

  TPgSQLProtocol = class
  private
    FServer: string;
    FPort: integer;
    FConnectionTimeout: integer;
    FKey: Integer;
    FProcessID: Integer;
    FServerDefaults: TStrings;
    FMessagesEncoding: {$IFDEF NEXTGEN}TEncoding{$ELSE}Encoding{$ENDIF};

    FServerVersionFull: string;
    FServerVersion: string;
    FMajorServerVersion: integer;
    FMinorServerVersion: integer;
    FIsRedshiftServer: boolean;

    FStmtCounter: Int64;
    FOnNotice: TPgProtocolNoticeEvent;
    FOnNotification: TPgProtocolNotificationEvent;
    FLargeObjectsAPI: TPgSQLLargeObjectsAPI;
    FTerminated: boolean;
    FIPVersion: TIPVersion;
    FMessagesCharset: string;

    FReader: TPgSQLReader;
    FBufferProvider: TPgNetBufferProvider;

    procedure CheckLargeObjectsAPI;
    procedure SetMessagesCharset(const Value: string);
  protected
    FLock: TPgSQLCriticalSection;
    FNet: TPgSQLNet;
    FProtocolState: TPgProtocolState;
    FActiveStmt: TPgSQLStatement;
    FHasMoreResultSets: boolean;
    FActiveError: EPgError;
    FActiveNotices: TPgErrors;
    FImmediateNotices: boolean;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FIOHandler: TCRIOHandler;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FHttpOptions: THttpOptions;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FProxyOptions: TProxyOptions;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FSSLOptions: TSSLOptions;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FSSHOptions: TSSHOptions;
    FSSLMode: _TSSLMode;

    function CreateVio(): TCRVio;
    procedure TryToInitiateSSL(Net: TPgSQLNet);
    procedure SetProtocolState(Value: TPgProtocolState; ActiveStmt: TPgSQLStatement = nil);

    function GenerateStmtID: string;
    function GenerateFetchCall(const Cursor: string; FetchAll: boolean; FetchSize: Integer): string;

    // server requesting functions
    procedure PutMsgStart(MsgType: AnsiChar); virtual;
    procedure PutMsgEnd; virtual;
    procedure Flush; virtual;

    // server response processing functions
    function ProcessMessage(Response: AnsiChar): Boolean; virtual;
    function GetSize: integer; virtual; abstract;

    // error control
    procedure RaiseError(E: EPgError); overload;
    procedure RaiseError(const Msg: string); overload;
    function ReadError(IsError: Boolean; UseUnicode: Boolean = False): EPgError; virtual; abstract;

    // connection control functions
    procedure StartConnection(const NeedSSL: boolean; const Database, UserName, ApplicationName: string);
    procedure SendStartUpMsg(const Database, UserName, ApplicationName: string); virtual; abstract;
    procedure Authenticate(const UserName, Password: string);
    procedure AuthenticateClearText(const Password: string); virtual; abstract;
    procedure AuthenticateCrypt(const Password: string); virtual; abstract;
    function CalMD5PasswordHash(const UserName, Password: string; const Salt: TBytes): TBytes;
    procedure AuthenticateMD5(const UserName, Password: string); virtual; abstract;
    procedure AuthenticateScram(const Mechanizm, UserName, Password: string); virtual; abstract;
  {$IFDEF MSWINDOWS}
    procedure AuthenticateSSPI; virtual; abstract;
  {$ENDIF}

    // server parameters
    procedure RequestServerVersion;

    // command execution functions
    procedure InternalExecuteStmt(Stmt: TPgSQLStatement; const SQL: string; ProcessResponse: boolean = True);
    procedure ReadColDescs; virtual; abstract;
    procedure ReadNotifications; virtual; abstract;
    procedure FetchRow(Stmt: TPgSQLStatement; RowNo: integer; FetchBlock: IntPtr;
      FetchFieldProc: TFetchFieldProc); virtual; abstract;
    procedure PreFetchRow(Stmt: TPgSQLStatement; ReadStream: TPgSQLReadStream); virtual; abstract;
    procedure ReadCompleteStatus; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Init;
    procedure Finalize;
    procedure Reset;

    function Lock: TPgSQLCriticalSection;
    procedure Unlock;
    function IsReaderSupported: boolean; virtual;

    // connection constrol functions
    procedure SetSSLOptions(const SSLMode: _TSSLMode; IOHandler: TCRIOHandler;
      HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
      SSLOptions: TSSLOptions);
    procedure Connect(const Server: string; Port, ConnectionTimeout: integer;
      const Database, UserName, Password, ApplicationName: string; IPVersion: TIPVersion = ivIPv4);
    procedure Disconnect;
    function GetBackendPID: integer;
    procedure SetImmediateNotices(const Value: boolean);
    procedure ProcessMessageQueue(RequiredResponse: AnsiChar = {$IFNDEF NEXTGEN}#0{$ELSE}0{$ENDIF}; WhileEqual: Boolean = False; Wait: boolean = False);
    procedure TerminateMessageLoop;

    // command execution functions
    procedure CloseStmt(Stmt: TPgSQLStatement); virtual;
    // simple query execution protocol
    procedure ExecuteStmt(Stmt: TPgSQLStatement; const SQL: string; ParsedSQLType: TParsedSQLType); virtual;
    // binary query execution protocol
    procedure PrepareStmt(Stmt: TPgSQLStatement; const SQL: string;
      ParsedSQLType: TParsedSQLType; const ParamTypes: TIntegerDynArray); virtual;
    procedure UnPrepareStmt(Stmt: TPgSQLStatement); virtual;
    procedure DescribeParams(Stmt: TPgSQLStatement; var Params: TPgSQLItemDescs);
    procedure BindPreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc); virtual;
    procedure ExecutePreparedStmt(Stmt: TPgSQLStatement); virtual;
    procedure BindExecutePreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc; IsCursor: boolean); virtual;

    procedure RequestCancel;
    procedure Ping(ProcessResponse: boolean = True); virtual; abstract;

    procedure DescribeFields(Stmt: TPgSQLStatement; var Fields: TPgSQLItemDescs);
    function FetchStmt(Stmt: TPgSQLStatement; Rows, PrefetchedRows: integer; FetchBlock: IntPtr; FetchFieldProc: TFetchFieldProc): integer; virtual;

    function LastInsertOID(Stmt: TPgSQLStatement): Int64;
    function GetRowsAffected(Stmt: TPgSQLStatement): integer;
    procedure SetRowsAffected(Stmt: TPgSQLStatement; Value: integer);
    function StatementType(Stmt: TPgSQLStatement): string;
    function RowsReturn(Stmt: TPgSQLStatement): boolean;
    function NoData(Stmt: TPgSQLStatement): boolean;
    function HasMoreResultSets: boolean;
    function IsVoidFunc(Stmt: TPgSQLStatement): boolean;
    function GetServerParameter(const Name: string): string;

    // server function call
    procedure CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
      var OutParam: TPgSQLFuncParam); virtual; abstract;
    procedure CallServerFuncInt(FuncOID: OID; InParams: array of integer;
      var OutParam: integer);

    // large objects protocol
    function lo_create(Mode: integer): OID;
    function lo_open(ObjOID: OID; Mode: integer): integer;
    function lo_write(ObjHandle: integer; Buffer: IntPtr; Count: integer): integer;
    function lo_read(ObjHandle: integer; Buffer: IntPtr; Count: integer): integer;
    function lo_lseek(ObjHandle: integer; Offset: integer; Origin: integer): integer;
    function lo_tell(ObjHandle: integer): integer;
    function lo_close(ObjHandle: integer): integer;
    function lo_unlink(ObjOID: OID): integer;

    // COPY command support
    procedure BeginCopyDataBlock(out Net: TPgSQLNet); virtual;
    procedure EndCopyDataBlock; virtual;
    procedure PutCopyEnd; virtual;

    procedure SetTimeout(Value: integer);
    procedure SetUseUnicode(Value: boolean);

    property Net: TPgSQLNet read FNet;
    property ProtocolState: TPgProtocolState read FProtocolState;
    property ServerVersionFull: string read FServerVersionFull;
    property ServerVersion: string read FServerVersion;
    property MajorServerVersion: integer read FMajorServerVersion;
    property MinorServerVersion: integer read FMinorServerVersion;
    property MessagesCharset: string read FMessagesCharset write SetMessagesCharset;
    property IsRedshiftServer: boolean read FIsRedshiftServer;
    property Reader: TPgSQLReader read FReader;
    property OnNotice: TPgProtocolNoticeEvent read FOnNotice write FOnNotice;
    property OnNotification: TPgProtocolNotificationEvent read FOnNotification write FOnNotification;
  end;

  TPgSQLProtocol20 = class(TPgSQLProtocol)
  protected
    function ProcessMessage(Response: AnsiChar): Boolean; override;
    function GetSize: integer; override;

    procedure SendStartUpMsg(const Database, UserName, ApplicationName: string); override;
    procedure AuthenticateClearText(const Password: string); override;
    procedure AuthenticateCrypt(const Password: string); override;
    procedure AuthenticateMD5(const UserName, Password: string); override;
    procedure AuthenticateScram(const Mechanizm, UserName, Password: string); override;
  {$IFDEF MSWINDOWS}
    procedure AuthenticateSSPI; override;
  {$ENDIF}

    function ReadError(IsError: Boolean; UseUnicode: Boolean = False): EPgError; override;

    procedure ReadColDescs; override;
    procedure ReadNotifications; override;
    procedure FetchRow(Stmt: TPgSQLStatement; RowNo: integer; FetchBlock: IntPtr;
      FetchFieldProc: TFetchFieldProc); override;
    procedure PreFetchRow(Stmt: TPgSQLStatement; ReadStream: TPgSQLReadStream); override;
  public
    function IsReaderSupported: boolean; override;

    procedure CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
      var OutParam: TPgSQLFuncParam); override;

    procedure Ping(ProcessResponse: boolean = True); override;

    // COPY command support
    procedure EndCopyDataBlock; override;
    procedure PutCopyEnd; override;
  end;

  TPgSQLProtocol30 = class(TPgSQLProtocol)
  private
    FtxnStatus: AnsiChar;
  {$IFDEF MSWINDOWS}
    FNTLMAuth: TCRNTLMAuth;
  {$ENDIF}

    function GetPortalName(const StmtID: string; ParsedSQLType: TParsedSQLType): string;
    function GetStatementName(const StmtID: string): string;

    procedure ParseCommand(const StatementName: string; const SQL: string; const ParamTypes: TIntegerDynArray);
    procedure DescribeCommand(const StatementName: string);
    procedure BindCommand(const StatementName, PortalName: string; Fields: TPgSQLItemDescs; Params: TPgSQLItemDescs; ParamsCount: Integer; BindParamProc: TBindParamProc);
    procedure ExecuteCommand(const PortalName: string; FetchSize: integer);

    procedure ClosePortalCommand(const PortalName: string);
    procedure CloseStatementCommand(const StatementName: string);

    procedure HoldCommand;
    procedure SyncCommand;
  protected
    procedure PutMsgStart(MsgType: AnsiChar); override;
    procedure PutMsgEnd; override;
    function ProcessMessage(Response: AnsiChar): Boolean; override;
    function GetSize: integer; override;

    function ReadError(IsError: Boolean; UseUnicode: Boolean = False): EPgError; override;

    procedure SendStartUpMsg(const Database, Username, ApplicationName: string); override;
    procedure AuthenticateClearText(const Password: string); override;
    procedure AuthenticateCrypt(const Password: string); override;
    procedure AuthenticateMD5(const UserName, Password: string); override;
    function CalcSHA256Hash(const Data: TBytes): TBytes;
    function CalcHMAC(const Key, Msg: TBytes): TBytes;
    function CalcPBKDF2(const Password, Salt: TBytes; Iterations, KeyLength: Cardinal): TBytes;
    procedure AuthenticateScram(const Mechanizm, UserName, Password: string); override;
  {$IFDEF MSWINDOWS}
    procedure AuthenticateSSPI; override;
  {$ENDIF}

    procedure ReadParamDescs;
    procedure ReadColDescs; override;
    procedure ReadNotifications; override;
    procedure FetchRow(Stmt: TPgSQLStatement; RowNo: integer; FetchBlock: IntPtr;
      FetchFieldProc: TFetchFieldProc); override;
    procedure PreFetchRow(Stmt: TPgSQLStatement; ReadStream: TPgSQLReadStream); override;
    procedure ReadCopyInResponse;

    procedure Hold;
    procedure Sync;
  public
    constructor Create; override;
    destructor Destroy; override;

    function IsReaderSupported: boolean; override;

    procedure CloseStmt(Stmt: TPgSQLStatement); override;

    procedure PrepareStmt(Stmt: TPgSQLStatement; const SQL: string;
      ParsedSQLType: TParsedSQLType; const ParamTypes: TIntegerDynArray); override;
    procedure UnPrepareStmt(Stmt: TPgSQLStatement); override;
    procedure BindPreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc); override;
    procedure ExecutePreparedStmt(Stmt: TPgSQLStatement); override;
    procedure BindExecutePreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc; IsCursor: boolean); override;
    function FetchStmt(Stmt: TPgSQLStatement; Rows, PrefetchedRows: integer; FetchBlock: IntPtr;
      FetchFieldProc: TFetchFieldProc): integer; override;

    procedure Ping(ProcessResponse: boolean = True); override;

    procedure CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
      var OutParam: TPgSQLFuncParam); override;

    // COPY command support
    procedure BeginCopyDataBlock(out Net: TPgSQLNet); override;
    procedure EndCopyDataBlock; override;
    procedure PutCopyEnd; override;
  end;

  BytesBuilder = class
  protected
    FBuffer: TBytes;
    FActualLength: Integer;

    procedure SetActualLength(Value: Integer);
  public
    constructor Create(Capacity: Integer);
    procedure Append(Value: TBytes); overload;
    procedure Append(Value: byte); overload;
    function ToBytes: TBytes;

    property Length: Integer read FActualLength write SetActualLength;
  end;

  TPgStreamPageData = array[0..65535 - SizeOf(Integer)] of Byte;
  PPgStreamPageData = ^TPgStreamPageData;

  TPgStreamPage = record
    Size: Integer;
    Data: TPgStreamPageData;
  end;
  PPgStreamPage = ^TPgStreamPage;

  TPgStreamBufferProvider = class;

  TPgSQLReadStream = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FProtocol: TPgSQLProtocol;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FStmt: TPgSQLStatement;
    FPages: TList;
    FListLock: TCriticalSection;
    FBufferProvider: TPgStreamBufferProvider;

    FUseUnicode: boolean;

    FCurrentPageData: PPgStreamPageData;
    FCurrentPageSize: Integer;
    FCurrentPageIndex: Integer;
    FCurrentPosition: Integer;
    FFreePrevPages: boolean;
    FIsLastRow: boolean;
  protected
    procedure ReadData(Buffer: IntPtr; Count: Integer);
    procedure WriteData(DataPtr: IntPtr; DataSize: Integer);
    procedure WriteNetData(Protocol: TPgSQLProtocol; DataSize: Integer);

    procedure SetIsLastRow(Value: boolean);
    procedure FetchRow(RowNo: integer; FetchBlock: IntPtr; FetchFieldProc: TFetchFieldProc);
    procedure GetRow;
  public
    constructor Create(Stmt: TPgSQLStatement);
    destructor Destroy; override;

    procedure Clear;
    procedure Reset(Protocol: TPgSQLProtocol);

    function FetchStmt(Protocol: TPgSQLProtocol; Rows: integer; FetchBlock: IntPtr; FetchFieldProc: TFetchFieldProc): integer;
    function PrefetchStmt(Protocol: TPgSQLProtocol): integer;
    function ReadNextRow(Protocol: TPgSQLProtocol): boolean;
    function HasData: boolean;

    property UseUnicode: boolean read FUseUnicode write FUseUnicode;
    property IsLastRow: boolean read FIsLastRow write SetIsLastRow;
  end;

  TPgSQLReadThread = class(TThread)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FProtocol: TPgSQLProtocol;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FStream: TPgSQLReadStream;
    FLock: TPgSQLCriticalSection;
    FDataReadEvent: TEvent;
    FPauseEvent: TEvent;
    FStopEvent: TEvent;
    FLastError: Exception;

    function GetIsStopped: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Protocol: TPgSQLProtocol; Lock: TPgSQLCriticalSection; DataReadEvent: TEvent);
    destructor Destroy; override;

    procedure CheckLastError;

    procedure Run(Stream: TPgSQLReadStream);
    procedure Stop;

    property IsStopped: boolean read GetIsStopped;
  end;

  TPgSQLReader = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FProtocol: TPgSQLProtocol;
    FLock: TPgSQLCriticalSection;
    FDataReadEvent: TEvent;
    FReadThread: TPgSQLReadThread;
  public
    constructor Create(Protocol: TPgSQLProtocol);
    destructor Destroy; override;

    procedure CheckLastError;

    procedure Run(Stream: TPgSQLReadStream);
    procedure Stop;
    function IsStopped: boolean;
    procedure WaitForStop;

    procedure ReadRow(Protocol: TPgSQLProtocol; Stream: TPgSQLReadStream);
  end;

  TPgStreamBufferProvider = class(TPgBufferProvider)
  private
    FStream: TPgSQLReadStream;

    function ReadInt32NoSwap: integer;
  public
    constructor Create(const Stream: TPgSQLReadStream);

    procedure ReadBytes(Buffer: TValueArr; Offset, Count: integer); override;
    function ReadByte: Byte; override;
    function ReadWord: Word; override;
    function ReadInt16: Smallint; override;
    function ReadInt32: integer; override;
    function ReadInt64: Int64; override;
    function ReadSingle: Single; override;
    function ReadDouble: Double; override;
    function ReadAnsiChar: AnsiChar; override;
    function ReadAnsiString: AnsiString; overload; override;
    function ReadAnsiString(Count: integer): AnsiString; overload; override;
    function ReadAnsiString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer; overload; override;
    function ReadWideString: WideString; overload; override;
    function ReadWideString(Count: integer): WideString; overload; override;
    function ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer; overload; override;
    function ReadString(Count: integer): string; override;
    function ReadStringAsBytes(var Count: integer): TBytes; override;
  end;
const
  _PgSSLModeNames: array[_TSSLMode] of string = ('smDisable', 'smRequire', 'smPrefer', 'smAllow',
    'smVerifyCA', 'smVerifyFull');

function EncodingToCodePage(EncodingName: string): Cardinal;
{$IFDEF MSWINDOWS}
function DetectEncoding: string;
{$ENDIF}

{$IFDEF DEBUG_MULTITHREAD}
function GetLockInfo(const Lock: TPgSQLCriticalSection): string;
{$ENDIF}

implementation

uses
  Variants,
{$IFDEF HAVE_OPENSSL}
  CRVioTcpSSL,
{$ENDIF}
{$IFNDEF LITE}
  CRVioHttp, CRHttp,
{$ENDIF}
  CRVioSocket, CRVioTcp, DAConsts,
  CRHash, CRBase64,
{$IFNDEF UNIDACPRO}
  PgConsts, PgCrypt, PgClasses;
{$ELSE}
  PgConstsUni, PgCryptUni, PgClassesUni;
{$ENDIF}

function EncodingToCodePage(EncodingName: string): Cardinal;
var
  i: integer;
begin
  Result := 0;

  if EncodingName = '' then
      Exit;

  for i := 0 to CodePagesCount - 1 do begin
    if SameText(EncodingName, CodePages[i].Name) then begin
      Result := CodePages[i].CodePage;
      Break;
    end;
  end;
end;

function CodePageToEncoding(CodePage: Cardinal): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to CodePagesCount - 1 do begin
    if CodePage = CodePages[i].CodePage then begin
      Result := CodePages[i].Name;
      Break;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
function DetectEncoding: string;
var
  cp: word;
begin
  cp := GetACP;
  Result := CodePageToEncoding(cp);
end;
{$ENDIF}

{$IFDEF DEBUG_MULTITHREAD}
function GetLockInfo(const Lock: TPgSQLCriticalSection): string;
var
  b: integer;
  i, n: integer;
begin
  b := Lock.FSection.LockCount;
  i := b and 1;
  n := (-1 - b) shr 2;
  Result := 'Owner = #' + IntToStr(Lock.FSection.OwningThread) + ' locked = ' + BoolToStr(not boolean(i), True) + ' count = ' + IntToStr(n) + ' recursion = ' + IntToStr(Lock.FSection.RecursionCount);// + ' semaphore = ' + IntToStr(Lock.FSection.LockSemaphore);
end;
{$ENDIF}

function StringToSeverity(const Str: string): TPgSeverity;
begin
  if Str = 'ERROR' then
    Result := {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sError
  else
  if Str = 'FATAL' then
    Result := sFatal
  else
  if Str = 'PANIC' then
    Result := sPanic
  else
  if Str = 'WARNING' then
    Result := sWarning
  else
  if Str = 'NOTICE' then
    Result := sNotice
  else
  if Str = 'DEBUG' then
    Result := sDebug
  else
  if Str = 'INFO' then
    Result := sInfo
  else
  if Str = 'LOG' then
    Result := sLog
  else
    Result := {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sUnknown;
end;

function ErrorCodeToSeverity(const Str: string): TPgSeverity;
begin
  if Str = '28000' then
    Result := sFatal
  else
    Result := {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sUnknown;
end;

procedure SplitErrorMsg(var Msg: string; var Severity: TPgSeverity);
var
  Idx: integer;
  StrSev: string;
begin
  Idx := Pos(':', Msg);
  if Idx > 0 then begin
    StrSev := Trim(Copy(Msg, 1, Idx - 1));
    Msg := Trim(Copy(Msg, Idx + 1, Length(Msg) - Idx));
    Severity := StringToSeverity(StrSev);
  end
  else begin
    Msg := Trim(Msg);
    Severity := {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sError;
  end;
end;

{ TPgSQLStatement }

constructor TPgSQLStatement.Create;
begin
  inherited;

  FNoData := True;
  FFetchAll := True;
  FPrepared := False;

  FReadStream := TPgSQLReadStream.Create(Self);
end;

destructor TPgSQLStatement.Destroy;
begin
  FReadStream.Free;

  inherited;
end;

{ TPgSQLResultSet }

constructor TPgSQLResultSet.Create(Protocol: TPgSQLProtocol);
begin
  inherited Create;

  FProtocol := Protocol;
  FValues := TStringList.Create;
end;

destructor TPgSQLResultSet.Destroy;
begin
  FValues.Free;

  inherited;
end;

procedure TPgSQLResultSet.FetchFieldProc(Source: TPgBufferProvider; Size: integer;
  FetchBlock: IntPtr; Row, FieldNo: integer);
begin
  if Size = 0 then
    FValues.Add('')
  else
    FValues.Add(Source.ReadString(Size));
end;

function TPgSQLResultSet.GetValue(FieldNo, RecordNo: integer): string;
begin
  Result := FValues[RecordNo * FFieldsCount + FieldNo];
end;

procedure TPgSQLResultSet.Open(const SQL: string);
var
  Stmt: TPgSQLStatement;
begin
  FValues.Clear;
  FRecordsCount := 0;

  Stmt := TPgSQLStatement.Create;
  try
    FProtocol.InternalExecuteStmt(Stmt, SQL);

    FFieldsCount := Stmt.FFieldsCount;

    while not FProtocol.NoData(Stmt) do begin
      FValues.Capacity := FValues.Capacity + 25;
      Inc(FRecordsCount, FProtocol.FetchStmt(Stmt, 25, 0, nil, FetchFieldProc));
    end;
  finally
    FProtocol.CloseStmt(Stmt);
    Stmt.Free;
  end;
end;
  
{ TPgSQLProtocol }

constructor TPgSQLProtocol.Create;
begin
  inherited Create;

  FServerDefaults := TStringList.Create;
  FLock := TPgSQLCriticalSection.Create;

  FMessagesEncoding := nil;

  Init;
end;

destructor TPgSQLProtocol.Destroy;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' before destroy PROTOCOL $' + IntToHex(Integer(Self), 8)));
{$ENDIF}

  Finalize;
  FServerDefaults.Free;
  FLargeObjectsAPI.Free;
  FreeAndNil(FLock);

  inherited;
end;

procedure TPgSQLProtocol.Init;
begin
  FNet := TPgSQLNet.Create;
  FReader := TPgSQLReader.Create(Self);
  FBufferProvider := TPgNetBufferProvider.Create(FNet);
end;

procedure TPgSQLProtocol.Finalize;
begin
  FNet.Free;
  FReader.Free;
  FBufferProvider.Free;
end;

procedure TPgSQLProtocol.Reset;
begin
  Finalize;
  Init;
end;

function TPgSQLProtocol.Lock: TPgSQLCriticalSection;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Protocol $' + IntToHex(Integer(Self), 8) + ' acquire PROTOCOL_LOCK ' + GetLockInfo(FLock)));
{$ENDIF}
  FLock.Enter;
  FReader.WaitForStop;
  Result := FLock;
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Protocol $' + IntToHex(Integer(Self), 8) + ' PROTOCOL_LOCK gained ' + GetLockInfo(FLock)));
{$ENDIF}
end;

procedure TPgSQLProtocol.Unlock;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Protocol $' + IntToHex(Integer(Self), 8) + ' release PROTOCOL_LOCK ' + GetLockInfo(FLock)));
{$ENDIF}
  FLock.Leave;
{$IFDEF DEBUG_MULTITHREAD}
  if FLock.FSection.RecursionCount >= 0 then
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Protocol $' + IntToHex(Integer(Self), 8) + ' PROTOCOL_LOCK released ' + GetLockInfo(FLock)))
  else
    OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' Protocol $' + IntToHex(Integer(Self), 8) + ' ERROR releasing PROTOCOL_LOCK = ' + GetLockInfo(FLock)))
{$ENDIF}
end;

function TPgSQLProtocol.IsReaderSupported: boolean;
begin
  Result := False;
end;

procedure TPgSQLProtocol.SetSSLOptions(const SSLMode: _TSSLMode; IOHandler: TCRIOHandler;
  HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
  SSLOptions: TSSLOptions);
begin
  FSSLMode := SSLMode;
  FIOHandler := IOHandler;

  FHttpOptions := HttpOptions;
  FProxyOptions := ProxyOptions;
  FSSLOptions := SSLOptions;
end;

procedure TPgSQLProtocol.Connect(const Server: string; Port, ConnectionTimeout: integer;
  const Database, UserName, Password, ApplicationName: string; IPVersion: TIPVersion);
var
  useSSL: boolean;
  appName, Msg: string;
begin
  FServer := Server;
  FPort := Port;
  FConnectionTimeout := ConnectionTimeout;
  FIPVersion := IPVersion;

  useSSL := False;
  appName := ApplicationName;

  while True do begin
    StartConnection(useSSL, Database, UserName, appName);

    try
      Authenticate(UserName, Password);
    except
      on E: Exception do begin
        if (not useSSL) and
          (FSSLMode = _smAllow) and 
          (not(E is EPgError) or ((E is EPgError) and (EPgError(E).Severity = sFatal)))
        then begin
          // repeat connection with SSL
          useSSL := True;
          continue;
        end
        else if E.Message <> '' then
          raise
        else begin
          Msg := SServerClosedTheConnection;
          if Database = '' then
            Msg := Msg + '. ' + STryToSpecifyDatabase;
          if (E is EPgError) then
            raise EPgError.Create(sFatal, StrToInt(EPgError(E).ErrorCode), Msg)
          else
            raise Exception.Create(Msg);
        end;
      end;
    end;

    if FProtocolState <> psAuthenticated then
      Exit;

    // Get backend cancel key data and server parameters
    try
      ProcessMessageQueue(PG_MSG_BACKENDKEYDATA, False);
    except
      on E: EPgError do begin
        if (EPgError(E).Severity = sFatal) and
           (EPgError(E).ErrorCode = '42704')
        then begin
          // repeat connection without ApplicationName
          // PostgreSQL before ver 9.0 doesn't support ApplicationName
          appName := '';
          continue;
        end
        else begin
          if FActiveError <> nil then
            raise EPgError.Create(FActiveError.Severity, FActiveError.Message)
          else
            raise;
        end;
      end
    end;

    // Check whether server is ready for query
    // if not this method throws exception
    ProcessMessageQueue(PG_MSG_NOTICE_RESPONSE, True);

    break;
  end;

  RequestServerVersion;
end;

procedure TPgSQLProtocol.Disconnect;
begin
  FNet.WriteAnsiChar(PG_MSG_TERMINATE);
  FNet.WriteInt32(4);
  try
    Flush;
  except
    on EPgError do ;
  end;
  FNet.Vio := nil;

end;

function TPgSQLProtocol.GetBackendPID: integer;
begin
  Result := FProcessID;
end;

procedure TPgSQLProtocol.SetImmediateNotices(const Value: boolean);
begin
  FImmediateNotices := Value;
end;

procedure TPgSQLProtocol.CloseStmt(Stmt: TPgSQLStatement);
var
  ClStmt: TPgSQLStatement;
begin
  try
    if Stmt.FStatementID <> '' then begin
      ClStmt := TPgSQLStatement.Create;
      try
        InternalExecuteStmt(ClStmt, 'CLOSE ' + Stmt.FStatementID);
      finally
        ClStmt.Free;
      end;
    end;
  except
  end;
end;

procedure TPgSQLProtocol.ExecuteStmt(Stmt: TPgSQLStatement; const SQL: string;
  ParsedSQLType: TParsedSQLType);
var
  FinalSQL: string;
begin
  FinalSQL := '';

  Stmt.FStatementID := '';
  
  if (ParsedSQLType = qtCursor) then
    Stmt.FStatementID := SQL
  else if (ParsedSQLType = qtSelect) and (not Stmt.FFetchAll) then begin
    Stmt.FParsedSQLType := ParsedSQLType;
    Stmt.FStatementID := GenerateStmtID;
    FinalSQL := 'DECLARE ' + Stmt.FStatementID + ' CURSOR ';
    if Stmt.FWithHold then
      FinalSQL := FinalSQL + 'WITH HOLD ';
    FinalSQL := FinalSQL + 'FOR ' + SQL + #13#10';';
  end;

  if Stmt.FStatementID <> '' then
    FinalSQL := FinalSQL + GenerateFetchCall(Stmt.FStatementID, Stmt.FFetchAll, Stmt.FFetchSize + Stmt.FPrefetchSize)
  else
    FinalSQL := SQL;

  InternalExecuteStmt(Stmt, FinalSQL);
end;

procedure TPgSQLProtocol.PrepareStmt(Stmt: TPgSQLStatement; const SQL: string;
  ParsedSQLType: TParsedSQLType; const ParamTypes: TIntegerDynArray);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.UnPrepareStmt(Stmt: TPgSQLStatement);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.DescribeParams(Stmt: TPgSQLStatement; var Params: TPgSQLItemDescs);
begin
  Assert(Stmt <> nil);
  Params := Stmt.FParams;
end;

procedure TPgSQLProtocol.BindPreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.ExecutePreparedStmt(Stmt: TPgSQLStatement);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.BindExecutePreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc; IsCursor: boolean);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.RequestCancel;
const
  CANCEL_REQUEST_CODE = 1234 shl 16 or 5678;
var
  Net: TPgSQLNet;
begin
  Net := TPgSQLNet.Create;
  try
    Net.Vio := CreateVio;
    if (FSSLMode in [_smRequire, _smPrefer, _smVerifyCA, _smVerifyFull]) then
      TryToInitiateSSL(Net);

    // packet size
    Net.WriteInt32(16);
    // protocol version
    Net.WriteInt32(CANCEL_REQUEST_CODE);
    Net.WriteInt32(FProcessID);
    Net.WriteInt32(FKey);
    Net.FlushSend;
  finally
    Net.Free;
  end;
end;

procedure TPgSQLProtocol.DescribeFields(Stmt: TPgSQLStatement; var Fields: TPgSQLItemDescs);
begin
  Fields := Stmt.FFields;
end;

function TPgSQLProtocol.FetchStmt(Stmt: TPgSQLStatement; Rows, PrefetchedRows: integer; FetchBlock: IntPtr;
  FetchFieldProc: TFetchFieldProc): integer;
begin
  Result := PrefetchedRows;
  FHasMoreResultSets := False;

  if FProtocolState <> psStmtFetching then begin
    if Stmt.FFetchAll then
      Exit;

    if Stmt.FSimpleQueryExecute then
      InternalExecuteStmt(Stmt, GenerateFetchCall(Stmt.FStatementID, Stmt.FFetchAll, Stmt.FFetchSize + Stmt.FPrefetchSize))
    else
      ExecutePreparedStmt(Stmt);
  end;

  while (Result < Rows) and (FProtocolState = psStmtFetching) do begin
    Assert(FActiveStmt = Stmt);

    FetchRow(Stmt, Result, FetchBlock, FetchFieldProc);

    Inc(Result);
    ProcessMessageQueue;
    if FHasMoreResultSets then
      Break;
  end;

  if Stmt.FFetchAll then
    Stmt.FNoData := (FProtocolState <> psStmtFetching) or FHasMoreResultSets
  else
    Stmt.FNoData := Result <> Rows;
end;

function TPgSQLProtocol.LastInsertOID(Stmt: TPgSQLStatement): Int64;
begin
  Result := Stmt.FLastInsertOID;
end;

function TPgSQLProtocol.GetRowsAffected(Stmt: TPgSQLStatement): integer;
begin
  Result := Stmt.FRowsAffected;
end;

procedure TPgSQLProtocol.SetRowsAffected(Stmt: TPgSQLStatement; Value: integer);
begin
  Stmt.FRowsAffected := Value;
end;

function TPgSQLProtocol.StatementType(Stmt: TPgSQLStatement): string;
begin
  Result := Stmt.FStmtType;
end;

function TPgSQLProtocol.RowsReturn(Stmt: TPgSQLStatement): boolean;
begin
  Result := Stmt.FFieldsCount > 0;
end;

function TPgSQLProtocol.NoData(Stmt: TPgSQLStatement): boolean;
begin
  Result := Stmt.FNoData;
end;

function TPgSQLProtocol.HasMoreResultSets: boolean;
begin
  Result := FHasMoreResultSets;
end;

function TPgSQLProtocol.IsVoidFunc(Stmt: TPgSQLStatement): boolean;
begin
  Result := (Stmt.FFieldsCount = 1) and (Stmt.FFields[0].TypeOID = SQL_PG_VOID);
end;

function TPgSQLProtocol.GetServerParameter(const Name: string): string;
begin
  Result := FServerDefaults.Values[Name];
end;

procedure TPgSQLProtocol.CallServerFuncInt(FuncOID: OID; InParams: array of integer;
  var OutParam: integer);
var
  i: integer;
  InParamRecs: TPgSQLFuncParams;
  OutParamRec: TPgSQLFuncParam;
begin
  SetLength(InParamRecs, Length(InParams));
  for i := 0 to Length(InParams) - 1 do
    if VarIsNull(InParams[i]) then
      InParamRecs[i].IsNull := True
    else begin
      InParamRecs[i].IsNull := False;    
      case VarType(InParams[i]) of
        varInteger: begin
          InParamRecs[i].Size := 4;
          InParamRecs[i].DataType := fdtInteger;
          InParamRecs[i].VInteger := InParams[i]; 
        end;  
      else
        raise Exception.Create('');
      end;
    end;

  OutParamRec.DataType := fdtInteger;    

  CallServerFunc(FuncOID, InParamRecs, OutParamRec);
  
  OutParam := OutParamRec.VInteger;
end;

function TPgSQLProtocol.lo_create(Mode: integer): OID;
begin
  CheckLargeObjectsApi;

  CallServerFuncInt(FLargeObjectsAPI.lo_creat, [Mode], Result);
end;

function TPgSQLProtocol.lo_open(ObjOID: OID; Mode: integer): integer;
begin
  CheckLargeObjectsApi;
  
  CallServerFuncInt(FLargeObjectsAPI.lo_open, [ObjOID, Mode], Result);
end;

function TPgSQLProtocol.lo_write(ObjHandle: integer; Buffer: IntPtr; Count: integer): integer;
var
  WriteParams: TPgSQLFuncParams;
  ResultParam: TPgSQLFuncParam;
begin
  CheckLargeObjectsAPI;

  SetLength(WriteParams, 2);
  WriteParams[0].IsNull := False;
  WriteParams[0].DataType := fdtInteger;
  WriteParams[0].VInteger := ObjHandle;

  WriteParams[1].IsNull := False;
  WriteParams[1].DataType := fdtBuffer;
  WriteParams[1].Size := Count;
  WriteParams[1].VPointer := Buffer;

  ResultParam.DataType := fdtInteger;

  CallServerFunc(FLargeObjectsAPI.lo_write, WriteParams, ResultParam);
  Result := ResultParam.VInteger;
end;

function TPgSQLProtocol.lo_read(ObjHandle: integer; Buffer: IntPtr; Count: integer): integer;
var
  ReadParams: TPgSQLFuncParams; 
  ResultParam: TPgSQLFuncParam;
begin
  CheckLargeObjectsAPI;

  SetLength(ReadParams, 2);

  ReadParams[0].IsNull := False;
  ReadParams[0].DataType := fdtInteger;
  ReadParams[0].VInteger := ObjHandle;

  ReadParams[1].IsNull := False;
  ReadParams[1].DataType := fdtInteger;
  ReadParams[1].VInteger := Count;

  ResultParam.DataType := fdtBuffer;
  ResultParam.VPointer := Buffer;

  CallServerFunc(FLargeObjectsAPI.lo_read, ReadParams, ResultParam);

  Result := ResultParam.Size;
end;

function TPgSQLProtocol.lo_lseek(ObjHandle: integer; Offset: integer; Origin: integer): integer;
begin
  CheckLargeObjectsAPI;

  CallServerFuncInt(FLargeObjectsAPI.lo_lseek, [ObjHandle, Offset, Origin], Result);
end;

function TPgSQLProtocol.lo_tell(ObjHandle: integer): integer;
begin
  CheckLargeObjectsAPI;

  CallServerFuncInt(FLargeObjectsAPI.lo_tell, ObjHandle, Result);
end;

function TPgSQLProtocol.lo_close(ObjHandle: integer): integer;
begin
  CheckLargeObjectsAPI;

  CallServerFuncInt(FLargeObjectsAPI.lo_close, [ObjHandle], Result);
end;

function TPgSQLProtocol.lo_unlink(ObjOID: OID): integer;
begin
  CheckLargeObjectsAPI;

  CallServerFuncInt(FLargeObjectsAPI.lo_unlink, [ObjOID], Result);
end;

procedure TPgSQLProtocol.BeginCopyDataBlock(out Net: TPgSQLNet);
begin
  Assert(FProtocolState = psCopyIn);
  Net := FNet;
end;

procedure TPgSQLProtocol.EndCopyDataBlock;
begin
  Assert(FProtocolState = psCopyIn);
end;

procedure TPgSQLProtocol.PutCopyEnd;
begin
  Assert(FProtocolState = psCopyIn);
end;

procedure TPgSQLProtocol.SetTimeout(Value: integer);
begin
  Assert(FNet <> nil);
  Assert(FNet.Vio <> nil);

  FNet.Vio.Timeout := Value;
end;

procedure TPgSQLProtocol.SetUseUnicode(Value: boolean);
begin
  FNet.UseUnicode := Value;
end;

function TPgSQLProtocol.CreateVio(): TCRVio;
var
  Err: string;
  ErrCode: integer;
{$IFDEF HAVE_OPENSSL}
  SSLCACert, SSLCert, SSLKey: string;
{$ENDIF}
begin
  if (FIOHandler <> nil) and (FIOHandler.HandlerType <> 'ssl') then
    Result := TCRVioHandler.Create(FServer, FPort, FIOHandler, FHttpOptions, FProxyOptions,
      FSSLOptions, FSSHOptions, FIPVersion)
  else
  if FSSLMode = _smDisable then begin
{$IFNDEF LITE}
  if (FHttpOptions <> nil) and FHttpOptions.Enabled then begin
    Result := TCRVioHttp.Create(FIOHandler,
      FHttpOptions, FProxyOptions, FServer, FPort, FIPVersion)
  end
  else
{$ENDIF}
    Result := TCRVioTCP.Create(FProxyOptions, '', FServer, FPort, FIPVersion);
  end
  else
  if FIOHandler <> nil then
    Result := TCRVioHandler.Create(FServer, FPort, FIOHandler, FHttpOptions, FProxyOptions,
      FSSLOptions, FSSHOptions, FIPVersion)
  else begin
  {$IFNDEF LITE}
    if (FHttpOptions <> nil) and FHttpOptions.Enabled then
      raise EPgError.Create({$IFDEF FPC}TPgSeverity.{$ENDIF}sError, 'Can''t create SSL connection via HTTP')
    else
  {$ENDIF}
  {$IFDEF HAVE_OPENSSL}
    if FSSLMode in [_smVerifyCA, _smVerifyFull]  then
      raise EPgError.Create({$IFDEF FPC}TPgSeverity.{$ENDIF}sError, Format(SSSLModeNotSuppoted, [_PgSSLModeNames[FSSLMode]]));

    SSLCACert := StringReplace(FSSLOptions.CA, '\', '/', [rfReplaceAll]);
    SSLCert := StringReplace(FSSLOptions.Cert, '\', '/', [rfReplaceAll]);
    SSLKey := StringReplace(FSSLOptions.Key, '\', '/', [rfReplaceAll]);
    Result := TCRVioTcpSSL.Create(FServer, FPort,
      SSLKey, SSLCert, SSLCACert, '', FSSLOptions.Cipher, FIPVersion);
  {$ELSE}
    raise EPgError.Create({$IFDEF FPC}TPgSeverity.{$ENDIF}sError, 'Can''t create SSL connection');
  {$ENDIF}
  end;
  try
    Result.Timeout := FConnectionTimeout;
    Result.Connect;
  except
    on E: Exception do begin
      Err := Result.LastError;
      ErrCode := Result.LastErrorCode;
      Result.Free;
      if E is SocketException then
        raise EPgError.Create(sFatal, ErrCode, Err)
      else
        raise;
    end;
  end;
end;

procedure TPgSQLProtocol.TryToInitiateSSL(Net: TPgSQLNet);
var
  SSLAllowed: boolean;
begin
  // InitiateSSL
  Net.WriteInt32(8);
  Net.WriteInt32(PG_SSL_REQUEST_CODE);
  Net.FlushSend;

  SSLAllowed := Net.ReadAnsiChar = PG_MSG_SSL_RESPONSE_OK;

  if not SSLAllowed and (FSSLMode in [_smRequire, _smVerifyCA, _smVerifyFull]) then
    raise EPgError.Create(sFatal, 'SSL connection is not allowed');

  if SSLAllowed then begin
    if Net.Vio is TCRVioHandler then
      TCRVioHandler(Net.Vio).IsSecure := True
    else
  {$IFDEF HAVE_OPENSSL}
    if Net.Vio is TCRVioTcpSSL then
      TCRVioTcpSSL(Net.Vio).IsSecure := True
    else
  {$ENDIF}
      Assert(False);
  end;
end;

procedure TPgSQLProtocol.SetProtocolState(Value: TPgProtocolState; ActiveStmt: TPgSQLStatement = nil);
begin
  FProtocolState := Value;

  if FProtocolState in [psStmtPreparing, psStmtExecuting, psIsReadyForQuery]
  then
    FActiveStmt := ActiveStmt;

  if FProtocolState = psStmtFetching then
    FActiveStmt.FNoData := False;
end;

function TPgSQLProtocol.GenerateStmtID: string;
begin
  Result := 'ST' + IntToStr(NativeUInt(Self)) + IntToStr(FStmtCounter);
  if FStmtCounter = High(Int64) then
    FStmtCounter := 0
  else
    Inc(FStmtCounter);
end;

function TPgSQLProtocol.GenerateFetchCall(const Cursor: string; FetchAll: boolean; FetchSize: Integer): string;
begin
  if FetchAll then
    Result := 'FETCH ALL FROM ' + Cursor
  else
    Result := 'FETCH FORWARD ' + IntToStr(FetchSize) + ' FROM ' + Cursor;
end;

procedure TPgSQLProtocol.PutMsgStart(MsgType: AnsiChar);
begin
  FNet.WriteAnsiChar(MsgType);
end;

procedure TPgSQLProtocol.PutMsgEnd;
begin
end;

procedure TPgSQLProtocol.Flush;
begin
  FNet.FlushSend;
end;

function TPgSQLProtocol.ProcessMessage(Response: AnsiChar): Boolean;
var
  ParamName: string;
  ParamValue: string;
begin
  Result := True;
  case Response of
    PG_MSG_BACKENDKEYDATA: begin
      FProcessID := FNet.ReadInt32;
      FKey := FNet.ReadInt32;
    end;
    PG_MSG_ERROR_RESPONSE: begin
      FActiveError := ReadError(True, True);
      Result := not FActiveError.IsFatalError;
    end;
    PG_MSG_NOTICE_RESPONSE: begin
      if FActiveNotices = nil then
        FActiveNotices := TPgErrors.Create;
      FActiveNotices.Add(ReadError(True, True));

      if FImmediateNotices and Assigned(FOnNotice) then begin
        FOnNotice(FActiveNotices);
        FActiveNotices.Clear;
      end;
    end;
    PG_MSG_NOTIFICATION_RESPONSE:
      ReadNotifications;
    PG_MSG_PARAMETER_STATUS: begin
      ParamName := FNet.ReadString(-1);
      ParamValue := FNet.ReadString(-1);
      FServerDefaults.Values[ParamName] := ParamValue;
    end;
    PG_MSG_READY_FOR_QUERY: begin
      SetProtocolState(psIsReadyForQuery);
      Result := False;
    end;
    PG_MSG_ROW_DESCRIPTION: begin
      FHasMoreResultSets := FProtocolState = psStmtFetching;
      ReadColDescs;
      Result := FActiveStmt.FSimpleQueryExecute;
    end;
    PG_MSG_ASCIIROW: begin
      SetProtocolState(psStmtFetching);
      Result := False;
    end;
    PG_MSG_COMPLETED_RESPONSE: begin
      ReadCompleteStatus;
    end;
    PG_MSG_NODATA: begin
      FNet.ReadInt32;
      Result := False;
    end;
    PG_MSG_EMPTY_QUERY_RESPONSE: begin
      FActiveError := EPgError.Create({$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sError, SEmptySQLStatement);
//      Result := False;
    end;
    PG_MSG_PORTAL_SUSPENDED: begin
      FNet.ReadInt32;
      SetProtocolState(psIsReadyForQuery);
      Result := False;
    end;
    PG_MSG_COPYIN_RESPONSE: begin
      SetProtocolState(psCopyIn);
      Result := False;
    end;
    PG_MSG_FUNCTION_RESULT: begin
      SetProtocolState(psFunctionExecuted);
      Result := False;
    end;
  else
    RaiseError(sUnexpectedServerResponse + ' : ' + Char(Response));
  end;
end;

procedure TPgSQLProtocol.ProcessMessageQueue(RequiredResponse: AnsiChar; WhileEqual: Boolean; Wait: boolean);
var
  Response: AnsiChar;
begin
  Assert(FActiveError = nil);
  Assert(FActiveNotices = nil);

  // if connection is lost
  if FNet.Vio = nil then
    Exit;

  Response := {$IFNDEF NEXTGEN}#255{$ELSE}255{$ENDIF};
  if WhileEqual then
    Response := RequiredResponse;

  while Wait or ((Response = RequiredResponse) = WhileEqual) do begin
    if Wait then begin
      if FNet.GetReadBufferAvailable < 1 then
        FNet.Vio.WaitForData;
      if FTerminated then begin
        Wait := False;
        FTerminated := False;
      end;
    end;
    Response := FNet.ReadAnsiChar;
    if not ProcessMessage(Response) and not Wait then
      Break;
  end;

  if FActiveNotices <> nil then
    try
      if not FImmediateNotices and Assigned(FOnNotice) then
        FOnNotice(FActiveNotices);
    finally
      FActiveNotices.Free;
      FActiveNotices := nil;
    end;

  if FActiveError <> nil then
    try
      RaiseError(FActiveError);
    finally
      FActiveError := nil;
    end;
end;

procedure TPgSQLProtocol.TerminateMessageLoop;
begin
  FTerminated := True;
  try
    Ping(False); // to unlock thread waiting for data in socket
  except
    // if Connection is lost then Ping raise exception
  end;
end;

procedure TPgSQLProtocol.RaiseError(E: EPgError);
begin
  raise E;
end;

procedure TPgSQLProtocol.RaiseError(const Msg: string);
begin
  RaiseError(EPgError.Create(sFatal, Msg));
end;

procedure TPgSQLProtocol.StartConnection(const NeedSSL: boolean;
  const Database, UserName, ApplicationName: string);
begin
  FNet.Vio := CreateVio();
  if ((FSSLMode in [_smRequire, _smPrefer, _smVerifyCA, _smVerifyFull]) or NeedSSL) then
    TryToInitiateSSL(FNet);

  SendStartUpMsg(Database, UserName, ApplicationName);
end;

procedure TPgSQLProtocol.Authenticate(const UserName, Password: string);
var
  Response: AnsiChar;
  AuthMethod: Integer;
  AuthMechanizm: string;
begin
  AuthMethod := -1;

  SetProtocolState(psAuthenticating);

  while AuthMethod <> 0 do begin
    Response := FNet.ReadAnsiChar;

    if Response = PG_MSG_ERROR_RESPONSE then
      RaiseError(ReadError(True, False));

    if Response <> PG_MSG_AUTHENTICATION then
      RaiseError(SUnexpectedServerResponse);

    GetSize;

    AuthMethod := FNet.ReadInt32;
    case AuthMethod of
      PG_MSG_AUTHENTICATION_OK:
        SetProtocolState(psAuthenticated);
      PG_MSG_AUTHENTICATION_CLEARTEXTPASSWORD:
        AuthenticateClearText(Password);
      PG_MSG_AUTHENTICATION_CRYPTPASSWORD:
        // !!!This authentication method is allowed only for users, that was created with UNENCRYPTED password
        AuthenticateCrypt(Password);
      PG_MSG_AUTHENTICATION_MD5PASSWORD:
        AuthenticateMD5(UserName, Password);
      PG_MSG_AUTHENTICATION_KERBEROSV4, PG_MSG_AUTHENTICATION_KERBEROSV5,
      PG_MSG_AUTHENTICATION_SCMCREDENTIAL:
        RaiseError(SUnknownAuthMethod);
      PG_MSG_AUTHENTICATION_SSPI: begin
      {$IFDEF MSWINDOWS}
        AuthenticateSSPI;
      {$ELSE}
        RaiseError(SUnknownAuthMethod);
      {$ENDIF}
      end;
      PG_MSG_AUTHENTICATION_SCRAM: begin
        AuthMechanizm := FNet.ReadString(-1);
        AuthenticateScram(AuthMechanizm, UserName, Password);
      end
    else
      RaiseError(SUnknownAuthMethod);
    end;
  end;
end;

function TPgSQLProtocol.CalMD5PasswordHash(const UserName, Password: string; const Salt: TBytes): TBytes;
var
  i: Integer;
  Hash: TBytes;
  UserNameBuf, PasswordBuf: TBytes;
  UserWithPwd, UserWithSalt: TBytes;
  MD5: TMD5;
  MD5Buf: TBytes;
  MD5str: StringBuilder;
begin
  MD5 := TMD5.Create;
  try
    UserNameBuf := Encoding.UTF8.GetBytes(UserName);
    PasswordBuf := Encoding.UTF8.GetBytes(Password);
    SetLength(UserWithPwd, Length(UserNameBuf) + Length(PasswordBuf));
    Buffer.BlockCopy(PasswordBuf, 0, UserWithPwd, 0, Length(PasswordBuf));
    Buffer.BlockCopy(UserNameBuf, 0, UserWithPwd, Length(PasswordBuf), Length(UserNameBuf));
    Hash := MD5.ComputeHash(UserWithPwd);

    MD5str := StringBuilder.Create(Length(Hash) * 2);
    try
      for i := 0 to Length(Hash) - 1 do
        MD5str.Append(LowerCase(IntToHex(Hash[i], 2)));

      MD5Buf := Encoding.Default.GetBytes(MD5str.ToString);
      SetLength(UserWithSalt, Length(MD5Buf) + Length(Salt));
      if Length(MD5Buf) > 0 then
        Move(MD5Buf[0], UserWithSalt[0], Length(MD5Buf));
      if Length(Salt) > 0 then
        Move(Salt[0], UserWithSalt[Length(MD5Buf)], Length(Salt));
      Hash := MD5.ComputeHash(UserWithSalt);

      MD5str.Length := 0;
      MD5str.Append('md5');
      for i := 0 to Length(Hash) - 1 do
        MD5str.Append(LowerCase(IntToHex(Hash[i], 2)));

      Result := Encoding.Default.GetBytes(MD5str.ToString);
    finally
      MD5str.Free;
    end;
  finally
    MD5.Free;
  end;
end;

procedure TPgSQLProtocol.RequestServerVersion;
const
  Digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
var
  RecordSet: TPgSQLResultSet;
  Pos: integer;
  i: integer;
begin
  RecordSet := TPgSQLResultSet.Create(Self);
  try
    RecordSet.Open('SELECT VERSION()');
    Assert(RecordSet.RecordsCount = 1, 'Unable to get server version');

    FServerVersionFull := RecordSet.Value[0, 0];

    Pos := 0;
    for i := 1 to Length(FServerVersionFull) do begin
      if (FServerVersionFull[i] = ' ') or (FServerVersionFull[i] = ',') then
        if Pos = 0 then
          Pos := i
        else begin
          FServerVersion := Copy(FServerVersionFull, Pos + 1, i - Pos - 1);
          Break;
        end;
    end;

    Pos := 1;
    for i := 1 to Length(FServerVersion) do
      if not CharInSet(FServerVersion[i], Digits) then
        if Pos = 1 then begin
          FMajorServerVersion := StrToIntDef(string(Copy(FServerVersion, Pos, i - Pos)), 0);
          Pos := i + 1;
          while (Pos <= Length(FServerVersion)) and not CharInSet(FServerVersion[Pos], Digits) do
            Inc(Pos);
        end
        else begin
          FMinorServerVersion := StrToIntDef(string(Copy(FServerVersion, Pos, i - Pos)), 0);
          Break;
        end;

    FIsRedshiftServer := System.Pos('Redshift', FServerVersionFull) > 0;
  finally
    RecordSet.Free;
  end;
end;

procedure TPgSQLProtocol.InternalExecuteStmt(Stmt: TPgSQLStatement; const SQL: string; ProcessResponse: boolean = True);
begin
  SetProtocolState(psStmtExecuting, Stmt);

  Stmt.FSimpleQueryExecute := True;

  // Send query to the server
  PutMsgStart(PG_MSG_QUERY);
  FNet.WriteString(SQL);
  PutMsgEnd;

  Flush;

  if ProcessResponse then
    ProcessMessageQueue;
end;

procedure TPgSQLProtocol.ReadCompleteStatus;
var
  StatusStr: string;

  FirstSep: integer;
  SecondSep: integer;

  TypeStr: string;
  i: integer;
  ValCode,
  RowsAffected: Integer;
begin
  GetSize;

  if FProtocolState = psFunctionExecuting then
    Exit;

  StatusStr := FNet.ReadString(-1);

  FirstSep := 0;
  SecondSep := 0;
  for i := 1 to Length(StatusStr) do
    if (StatusStr[i] = ' ') then
      if FirstSep = 0 then
        FirstSep := i
      else
      if SecondSep = 0 then begin
        SecondSep := i;
        Break;
      end;

  if FirstSep = 0 then
    FirstSep := Length(StatusStr) + 1;

  TypeStr := Copy(StatusStr, 1, FirstSep - 1);

  FActiveStmt.FStmtType := TypeStr;

  if CompareText(TypeStr, 'INSERT') = 0 then begin
    Val(Copy(StatusStr, FirstSep + 1, i - FirstSep - 1), FActiveStmt.FLastInsertOID, ValCode);
    FirstSep := SecondSep;
  end
  else
    FActiveStmt.FLastInsertOID := 0;

  if (CompareText(TypeStr, 'INSERT') = 0) or
    (CompareText(TypeStr, 'UPDATE') = 0) or
    (CompareText(TypeStr, 'DELETE') = 0) or
    (CompareText(TypeStr, 'SELECT') = 0)
  then begin
    Val(Copy(StatusStr, FirstSep + 1, Length(StatusStr) - 1), RowsAffected, ValCode);
    Inc(FActiveStmt.FRowsAffected, RowsAffected);
  end
  else
    FActiveStmt.FRowsAffected := 0;
{$IFDEF FPC}
  if ValCode > 0 then ; // Lazarus warning
{$ENDIF}
end;

procedure TPgSQLProtocol.CheckLargeObjectsApi;
var
  SQL: string;
  ResultSet: TPgSQLResultSet;
  i: integer;
  FuncName: string;
  FuncOID: integer;  
begin
  if FLargeObjectsAPI = nil then
    FLargeObjectsAPI := TPgSQLLargeObjectsAPI.Create
  else  
    Exit;

  Assert((MajorServerVersion > 7) or ((MajorServerVersion = 7) and (MinorServerVersion >= 3)));

  SQL := 'select proname, oid from pg_catalog.pg_proc where proname in (' +
    '''lo_open'',''lo_close'', ''lo_creat'',''lo_create'', ''lo_unlink'', ''lo_lseek'',' +
    '''lo_tell'',''loread'',''lowrite'') and pronamespace = (select oid from pg_catalog.' +
    'pg_namespace where nspname = ''pg_catalog'')';

  ResultSet := TPgSQLResultSet.Create(Self);
  try
    ResultSet.Open(SQL);

      for i := 0 to ResultSet.RecordsCount - 1 do begin
        FuncName := ResultSet.Value[0, i];
        FuncOID := StrToInt(string(ResultSet.Value[1, i]));

        with FLargeObjectsAPI do
          if FuncName = 'lo_open' then
            lo_open := FuncOID
          else
          if FuncName = 'lo_close' then
            lo_close := FuncOID
          else
          if FuncName = 'lo_creat' then
            lo_creat := FuncOID
          else
          if FuncName = 'lo_create' then
            lo_create := FuncOID
          else
          if FuncName = 'lo_unlink' then
            lo_unlink := FuncOID
          else
          if FuncName = 'lo_lseek' then
            lo_lseek := FuncOID
          else
          if FuncName = 'lo_tell' then
            lo_tell := FuncOID
          else
          if FuncName = 'loread' then
            lo_read := FuncOID
          else
          if FuncName = 'lowrite' then
            lo_write := FuncOID;
      end;
  finally
    ResultSet.Free;
  end;
end;

procedure TPgSQLProtocol.SetMessagesCharset(const Value: string);
begin
  if FMessagesCharset <> Value then begin
    FMessagesCharset := Value;
    FMessagesEncoding := nil;
  end;
end;

{ TPgSQLProtocol20 }

function TPgSQLProtocol20.IsReaderSupported: boolean;
begin
  Result := True;
end;

procedure TPgSQLProtocol20.CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
  var OutParam: TPgSQLFuncParam);
var
  i: integer;
  FuncExecResult: AnsiChar;
  ParamsCount: integer;
begin
  SetProtocolState(psFunctionExecuting);
  ParamsCount := Length(InParams);

  PutMsgStart(PG_MSG_FUNCTION_CALL);
  FNet.WriteAnsiString(' ');        // dummy string
  FNet.WriteInt32(FuncOID);     // function ID
  FNet.WriteInt32(ParamsCount); // # of format codes

  for i := 0 to ParamsCount - 1 do
    if InParams[i].IsNull then
      FNet.WriteInt32(-1)
    else
      case InParams[i].DataType of
        fdtInteger: begin
          FNet.WriteInt32(4);
          FNet.WriteInt32(InParams[i].VInteger);
        end;
        fdtBuffer: begin
          FNet.WriteInt32(InParams[i].Size);
          FNet.WriteBytes(InParams[i].VPointer, 0, InParams[i].Size);
        end;
      end;  

  PutMsgEnd;

  Flush;

  ProcessMessageQueue();

  Assert(FProtocolState = psFunctionExecuted);

  while True do begin
    FuncExecResult := FNet.ReadAnsiChar;
    case FuncExecResult of
      PG_MSG_FUNCTION_RESULT_NONEMPTY_RESPONSE: begin
        OutParam.Size :=  FNet.ReadInt32;
        case OutParam.DataType of
          fdtInteger:
            OutParam.VInteger := FNet.ReadInt32;
          fdtBuffer:
            FNet.ReadBytes(OutParam.VPointer, 0, OutParam.Size);
        end;
      end;
      PG_MSG_FUNCTION_RESULT_VOID_RESPONSE: begin
        FProtocolState := psIsReadyForQuery;
        Break;
      end;
    else
      RaiseError(SUnexpectedServerResponse);
    end;
  end;
  ProcessMessageQueue;
end;

procedure TPgSQLProtocol20.Ping(ProcessResponse: boolean = True);
var
  Stmt: TPgSQLStatement;
begin
  Stmt := TPgSQLStatement.Create;
  try
    InternalExecuteStmt(Stmt, '', ProcessResponse);
  finally
    Stmt.Free;
  end;
end;

procedure TPgSQLProtocol20.EndCopyDataBlock;
begin
  inherited;

  Flush;
end;

procedure TPgSQLProtocol20.PutCopyEnd;
begin
  inherited;

  Flush;
  ProcessMessageQueue;
end;

function TPgSQLProtocol20.ProcessMessage(Response: AnsiChar): Boolean;
begin
  Result := True;
  case Response of
    PG_MSG_EMPTY_QUERY_RESPONSE: begin
      FNet.ReadWord;
      Result := False;
    end;
    PG_MSG_CURSOR_RESPONSE:
      FNet.ReadAnsiString;
  else
    Result := inherited ProcessMessage(Response);
  end;
end;

function TPgSQLProtocol20.GetSize: integer;
begin
  Result := 0;
end;

procedure TPgSQLProtocol20.SendStartUpMsg(const Database, UserName, ApplicationName: string);

  procedure WriteString(const Value: string; Len: integer);
  var
    TmpBuf: TBytes;
    TmpBufLen: integer;
  begin
    TmpBuf := Encoding.UTF8.GetBytes(Value);
    TmpBufLen := Length(TmpBuf);
    SetLength(TmpBuf, Len);
    while TmpBufLen < Length(TmpBuf) do begin
      TmpBuf[TmpBufLen] := 0;
      Inc(TmpBufLen);
    end;
    FNet.WriteBytes(TmpBuf);
  end;

const
  ProtocolVersion20 = $20000;
begin
  // Startup packet size
  FNet.EnterSizeBlock;
  // Protocol version
  FNet.WriteInt32(ProtocolVersion20);
  WriteString(Database, 64);
  WriteString(UserName, 32);
  // Arguments
  WriteString('', 64);
  // Unused
  WriteString('', 64);
  // Optional tty
  WriteString('', 64);
  FNet.LeaveSizeBlock(True);

  Flush;
end;

procedure TPgSQLProtocol20.AuthenticateClearText(const Password: string);
begin
  FNet.EnterSizeBlock;
  FNet.WriteStringAsBytes(Encoding.UTF8.GetBytes(Password));
  FNet.LeaveSizeBlock(True);
  Flush;
end;

procedure TPgSQLProtocol20.AuthenticateCrypt(const Password: string);
var
  Salt,
  Crypted: TBytes;
begin
  SetLength(Salt, 2);
  FNet.ReadBytes(TValueArr(Salt), 0, 2);
  Crypted := TUnixCrypt.Crypt(Salt, Encoding.UTF8.GetBytes(Password));

  FNet.EnterSizeBlock;
  if Length(Crypted) > 0 then
    FNet.WriteBytes(Crypted);
  FNet.WriteByte(0);
  FNet.LeaveSizeBlock(True);
  Flush;
end;

procedure TPgSQLProtocol20.AuthenticateMD5(const UserName, Password: string);
var
  Salt: TBytes;
  PasswordHash: TBytes;
begin
  SetLength(Salt, 4);
  FNet.ReadBytes(TValueArr(Salt), 0, 4);
  PasswordHash := CalMD5PasswordHash(UserName, Password, Salt);

  FNet.EnterSizeBlock;
  FNet.WriteStringAsBytes(PasswordHash);
  FNet.LeaveSizeBlock(True);
  Flush;
end;

procedure TPgSQLProtocol20.AuthenticateScram(const Mechanizm, UserName, Password: string);
begin
  RaiseError(SUnknownAuthMethod);
end;

{$IFDEF MSWINDOWS}
procedure TPgSQLProtocol20.AuthenticateSSPI;
begin
  RaiseError(SUnknownAuthMethod);
end;
{$ENDIF}

function TPgSQLProtocol20.ReadError(IsError: Boolean; UseUnicode: Boolean): EPgError;
var
  Buf: TBytes;
  Msg: string;
  Size: Integer;
  Severity: TPgSeverity;
  OldUseUnicode: boolean;
begin
  OldUseUnicode := FNet.UseUnicode;

  try
    Size := -1;
    if FMessagesEncoding = nil then
        if FNet.UseUnicode and UseUnicode then
          FMessagesEncoding := Encoding.UTF8
        else begin
          if FMessagesCharset <> '' then
            FMessagesEncoding := Encoding.GetEncoding(EncodingToCodePage(FMessagesCharset))
        {$IFDEF MSWINDOWS}
          else
            FMessagesEncoding := Encoding.GetEncoding(EncodingToCodePage(DetectEncoding))
        {$ENDIF}
          ;

          if FMessagesEncoding = nil then
            FMessagesEncoding := Encoding.Default;
        end;
    Buf := FNet.ReadStringAsBytes(Size);
    Msg := FMessagesEncoding.GetString(Buf, 0, Size);

    SplitErrorMsg(Msg, Severity);

    Result := EPgError.Create(Severity, Msg);
    if IsError then
      SetProtocolState(psError);
  finally
    FNet.UseUnicode := OldUseUnicode;
  end;
end;

procedure TPgSQLProtocol20.ReadColDescs;
var
  i: Integer;
begin
  Assert(FActiveStmt <> nil);

  FActiveStmt.FFieldsCount := FNet.ReadWord;
  SetLength(FActiveStmt.FFieldNulls, (FActiveStmt.FFieldsCount + 7) div 8);

  SetLength(FActiveStmt.FFIelds, FActiveStmt.FFieldsCount);
  for i := 0 to FActiveStmt.FFieldsCount - 1 do
    with FActiveStmt.FFields[i] do begin
      FieldName := FNet.ReadString(-1);
      TypeOid := FNet.ReadInt32;
      TypeSize := FNet.ReadInt16;
      TypeModifier := FNet.ReadInt32;
    end;
end;

procedure TPgSQLProtocol20.FetchRow(Stmt: TPgSQLStatement; RowNo: integer;
  FetchBlock: IntPtr; FetchFieldProc: TFetchFieldProc);
var
  i: integer;
  Size: integer;
  IsNull: boolean;
  BytePos: integer;
  BitPos: integer;
begin
  FNet.ReadBytes(TValueArr(Stmt.FFieldNulls), 0, Length(Stmt.FFieldNulls));

  BytePos := 0;
  BitPos := 7;
  for i := 0 to Stmt.FFieldsCount - 1 do begin
    IsNull := Stmt.FFieldNulls[BytePos] shr BitPos and 1 = 0;
    Dec(BitPos);
    if BitPos = -1 then begin
      Inc(BytePos);
      BitPos := 7;
    end;

    if IsNull then
      Size := -1
    else
      Size := FNet.ReadInt32 - 4;

    if @FetchFieldProc <> nil then
      FetchFieldProc(FBufferProvider, Size, FetchBlock, RowNo, i)
    else if Size > 0 then
      FNet.ReadStringAsBytes(Size);
  end;
end;

procedure TPgSQLProtocol20.PreFetchRow(Stmt: TPgSQLStatement; ReadStream: TPgSQLReadStream);
var
  i: integer;
  Size: integer;
  SwapedValue: Integer;
  IsNull: boolean;
  BytePos: integer;
  BitPos: integer;
  Buf: TBytes; // TODO !!!!
begin
{$IFNDEF VER9P}
  SetLength(Buf, 0); // anti-warning Delphi 6
{$ENDIF}

  FNet.ReadBytes(TValueArr(Stmt.FFieldNulls), 0, Length(Stmt.FFieldNulls));
  BytePos := 0;
  BitPos := 7;
  for i := 0 to Stmt.FFieldsCount - 1 do begin
    IsNull := Stmt.FFieldNulls[BytePos] shr BitPos and 1 = 0;
    Dec(BitPos);
    if BitPos = -1 then begin
      Inc(BytePos);
      BitPos := 7;
    end;

    if IsNull then begin
      Size := -1;
      ReadStream.WriteData(@Size, 4);
    end
    else begin
      Size := FNet.ReadInt32 - 4;
      Buf := FNet.ReadStringAsBytes(Size);
      SwapedValue := FastSwap(Size);
      ReadStream.WriteData(@SwapedValue, 4);
      if Size > 0 then
        ReadStream.WriteData(TValueArr(Buf), Size);
    end;
  end;
end;

procedure TPgSQLProtocol20.ReadNotifications;
var
  PID: integer;
  Name: string;
begin
  PID := FNet.ReadInt32;
  Name := FNet.ReadString(-1);
  if Assigned(FOnNotification) then
    FOnNotification(Name, PID, '');
end;

{ TPgSQLProtocol30 }

constructor TPgSQLProtocol30.Create;
begin
  inherited;

{$IFDEF MSWINDOWS}
  FNTLMAuth := TCRNTLMAuth.Create;
{$ENDIF}
end;

destructor TPgSQLProtocol30.Destroy;
begin
{$IFDEF MSWINDOWS}
  FNTLMAuth.Free;
{$ENDIF}

  inherited;
end;

function TPgSQLProtocol30.IsReaderSupported: boolean;
begin
  Result := True;
end;

function TPgSQLProtocol30.GetPortalName(const StmtID: string; ParsedSQLType: TParsedSQLType): string;
begin
  if not (ParsedSQLType in [qtInsert, qtUpdate, qtDelete]) then
    Result := 'PORTAL' + StmtID
  else
    Result := '';
end;

function TPgSQLProtocol30.GetStatementName(const StmtID: string): string;
begin
  Result := 'PRSTMT' + StmtID;
end;

procedure TPgSQLProtocol30.HoldCommand;
begin
  FNet.WriteAnsiChar(PG_COMMAND_HOLD);
  FNet.WriteInt32(4);
end;

procedure TPgSQLProtocol30.SyncCommand;
begin
  FNet.WriteAnsiChar(PG_COMMAND_SYNC);
  FNet.WriteInt32(4);
end;

procedure TPgSQLProtocol30.ClosePortalCommand(const PortalName: string);
begin
  PutMsgStart(PG_COMMAND_CLOSE);
  FNet.WriteAnsiChar(PG_COMMAND_PORTAL);
  FNet.WriteString(PortalName);
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.CloseStatementCommand(const StatementName: string);
begin
  PutMsgStart(PG_COMMAND_CLOSE);
  FNet.WriteAnsiChar(PG_COMMAND_STATEMENT);
  FNet.WriteString(StatementName);
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.ParseCommand(const StatementName: string; const SQL: string; const ParamTypes: TIntegerDynArray);
var
  i: integer;
begin
  // Parse statemenet
  PutMsgStart(PG_COMMAND_PARSE);
  FNet.WriteString(StatementName);
  FNet.WriteString(SQL);
  FNet.WriteInt16(Length(ParamTypes)); // number of datatypes for paremeters
  for i := 0 to Length(ParamTypes) - 1 do
    FNet.WriteInt32(ParamTypes[i]);
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.DescribeCommand(const StatementName: string);
begin
  // Describe statement
  PutMsgStart(PG_COMMAND_DESCRIBE);
  FNet.WriteAnsiChar(PG_COMMAND_STATEMENT);
  FNet.WriteString(StatementName);
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.BindCommand(const StatementName, PortalName: string; Fields: TPgSQLItemDescs; Params: TPgSQLItemDescs; ParamsCount: Integer; BindParamProc: TBindParamProc);
var
  i: integer;

  procedure SendFormatCodes(ItemDescs: TPgSQLItemDescs);
  var
    i: integer;
    IsItemsValid: boolean;
  begin
    IsItemsValid := True;
    for i := 0 to Length(ItemDescs) - 1 do
      if ItemDescs[i].FormatCode = 0 then
        IsItemsValid := False;

    if IsItemsValid then begin
      FNet.WriteInt16(1);
      FNet.WriteInt16(1);
    end
    else begin
      FNet.WriteWord(Length(ItemDescs));
      for i := 0 to Length(ItemDescs) - 1 do
        FNet.WriteInt16(ItemDescs[i].FormatCode);
    end;
  end;

begin
  PutMsgStart(PG_COMMAND_BIND);

  FNet.WriteString(PortalName);
  FNet.WriteString(StatementName);

  // params
  SendFormatCodes(Params);
  FNet.WriteWord(ParamsCount);
  try
    for i := 0 to ParamsCount - 1 do
      BindParamProc(FNet, i, Params[i]);
  except
    PutMsgEnd;
    FNet.ClearWriteBuffer;
    raise;
  end;

  // fields
  SendFormatCodes(Fields);

  PutMsgEnd;
end;

procedure TPgSQLProtocol30.ExecuteCommand(const PortalName: string; FetchSize: integer);
begin
  PutMsgStart(PG_COMMAND_EXECUTE);
  FNet.WriteString(PortalName);
  FNet.WriteInt32(FetchSize);
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.CloseStmt(Stmt: TPgSQLStatement);
begin
  if Stmt.FSimpleQueryExecute then
    inherited;
end;

procedure TPgSQLProtocol30.PrepareStmt(Stmt: TPgSQLStatement; const SQL: string;
  ParsedSQLType: TParsedSQLType; const ParamTypes: TIntegerDynArray);
begin
  Assert(Stmt.FStatementID = '');
  SetProtocolState(psStmtPreparing, Stmt);

  if ParsedSQLType = qtCursor then
    Stmt.FFinalSQL := 'FETCH ALL FROM ' + SQL
  else
    Stmt.FFinalSQL := SQL;

  Stmt.FParamTypes := ParamTypes;
  Stmt.FParsedSQLType := ParsedSQLType;
  Stmt.FStatementID := GenerateStmtID;
  Stmt.FStatementName := GetStatementName(Stmt.FStatementID);
  Stmt.FPortalName := GetPortalName(Stmt.FStatementID, ParsedSQLType);
  try
    ParseCommand(Stmt.FStatementName, Stmt.FFinalSQL, Stmt.FParamTypes);
    DescribeCommand(Stmt.FStatementName);

    if ParsedSQLType in [qtSelect, qtCursor, qtSelectProc] then begin
      ClosePortalCommand(Stmt.FPortalName);
      CloseStatementCommand(Stmt.FStatementName);
    end;

    Sync;

    ProcessMessageQueue;
    ProcessMessageQueue;
  except
    CloseStmt(Stmt);
    raise;
  end;

  SetProtocolState(psIsReadyForQuery);
end;

procedure TPgSQLProtocol30.UnPrepareStmt(Stmt: TPgSQLStatement);
begin
  try
    if not Stmt.FSimpleQueryExecute and (Stmt.FStatementID <> '') then begin
      ClosePortalCommand(Stmt.FPortalName);
      CloseStatementCommand(Stmt.FStatementName);

      Sync;

      Stmt.FStatementID := '';
      Stmt.FStatementName := '';
      Stmt.FPortalName := '';
    end;
  except
  end;
end;

procedure TPgSQLProtocol30.BindPreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc);
begin
  // we should close portal before new bind
  // portal can be not close if statement was open in transaction
  // if we close not exist portal - we don't get any errors
  ClosePortalCommand(Stmt.FPortalName);

  BindCommand(Stmt.FStatementName, Stmt.FPortalName, Stmt.FFields, Stmt.FParams, Stmt.FParamsCount, BindParamProc);
  Sync;
  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.ExecutePreparedStmt(Stmt: TPgSQLStatement);
begin
  SetProtocolState(psStmtExecuting, Stmt);

  ExecuteCommand(Stmt.FPortalName, Stmt.FFetchSize + Stmt.FPrefetchSize);
  Sync;
end;

procedure TPgSQLProtocol30.BindExecutePreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc; IsCursor: boolean);
var
  SavedFields: TPgSQLItemDescs;
begin
  // to preserve changes that were made in TPgSQLCommand.DescribeFields
  SavedFields := Stmt.FFields;
  try
    // we should close portal before new bind
    // portal can be not close if statement was open in transaction
    // if we close not exist portal - we don't get any errors
    if not IsCursor and (Stmt.FPortalName <> '') then
      ClosePortalCommand(Stmt.FPortalName);

    // Send Bind and Execute in the same Net packet increase performance highly !!!
    SetProtocolState(psStmtExecuting, Stmt);

    if IsCursor then begin
      Stmt.FStatementID := GenerateStmtID;
      Stmt.FStatementName := GetStatementName(Stmt.FStatementID);
      Stmt.FPortalName := GetPortalName(Stmt.FStatementID, Stmt.FParsedSQLType);
      ParseCommand(Stmt.FStatementName, Stmt.FFinalSQL, Stmt.FParamTypes);
      DescribeCommand(Stmt.FStatementName);
    end;

    BindCommand(Stmt.FStatementName, Stmt.FPortalName, Stmt.FFields, Stmt.FParams, Stmt.FParamsCount, BindParamProc);
    ExecuteCommand(Stmt.FPortalName, Stmt.FFetchSize + Stmt.FPrefetchSize);

    Sync;                  // when IsCursor: response for Parse (PG_MSG_PARSE_COMPLETE)
    if IsCursor then begin
      ProcessMessageQueue; // when IsCursor: responses for Desribe (PG_MSG_PARAMETER_DESCRIPTION and PG_MSG_ROW_DESCRIPTION)
      ProcessMessageQueue; // when IsCursor: response for Bind (PG_MSG_BIND_COMPLETE)
    end;
    ProcessMessageQueue;   // when IsCursor: response for Execute (PG_MSG_ASCIIROW)
  finally
    if IsCursor then
      Stmt.FFields := SavedFields;
  end;
end;

function TPgSQLProtocol30.FetchStmt(Stmt: TPgSQLStatement; Rows, PrefetchedRows: integer;
  FetchBlock: IntPtr; FetchFieldProc: TFetchFieldProc): integer;
begin
  Result := inherited FetchStmt(Stmt, Rows, PrefetchedRows, FetchBlock, FetchFieldProc);

  if Stmt.FNoData and (Stmt.FStatementID <> '') then begin
    ClosePortalCommand(Stmt.FPortalName);
    CloseStatementCommand(Stmt.FStatementName);
    Sync;

    Stmt.FStatementID := '';
    Stmt.FStatementName := '';
    Stmt.FPortalName := '';
  end;
end;

procedure TPgSQLProtocol30.CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
  var OutParam: TPgSQLFuncParam);
var
  i: integer;
  ParamsCount: integer;
begin
  SetProtocolState(psFunctionExecuting);
  ParamsCount := Length(InParams);

  PutMsgStart(PG_MSG_FUNCTION_CALL);

  FNet.WriteInt32(FuncOID);
  FNet.WriteInt16(1); //# of format codes
  FNet.WriteInt16(1); //format code: BINARY
  FNet.WriteInt16(ParamsCount);

  for i := 0 to ParamsCount - 1 do
    if InParams[i].IsNull then
      FNet.WriteInt32(-1)
    else
      case InParams[i].DataType of
        fdtInteger: begin
          FNet.WriteInt32(4);
          FNet.WriteInt32(InParams[i].VInteger);
        end;
        fdtBuffer: begin
          FNet.WriteInt32(InParams[i].Size);
          FNet.WriteBytes(InParams[i].VPointer, 0, InParams[i].Size);
        end;
      end;  

  FNet.WriteInt16(1); // result format code: BINARY
  PutMsgEnd;

  Hold;

  Assert(FProtocolState = psFunctionExecuted);

  FNet.ReadInt32; // packet length
  OutParam.Size :=  FNet.ReadInt32;
  case OutParam.DataType of
    fdtInteger: begin
      case OutParam.Size of
        2:
          OutParam.VInteger := Integer(FNet.ReadWord);
        4:
          OutParam.VInteger := FNet.ReadInt32;
      end;
    end;
    fdtBuffer:
      FNet.ReadBytes(OutParam.VPointer, 0, OutParam.Size);
  end;

  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.BeginCopyDataBlock(out Net: TPgSQLNet);
begin
  inherited BeginCopyDataBlock(Net);

  PutMsgStart(PG_MSG_COPY_DATA);
end;

procedure TPgSQLProtocol30.EndCopyDataBlock;
begin
  inherited;

  PutMsgEnd;
  Flush;
end;

procedure TPgSQLProtocol30.PutCopyEnd;
begin
  inherited;

  FNet.WriteAnsiChar(PG_MSG_COPY_DONE);
  FNet.WriteInt32(4);
  Flush;

  Sync;
end;

procedure TPgSQLProtocol30.PutMsgStart(MsgType: AnsiChar);
begin
  inherited;

  FNet.EnterSizeBlock;
end;

procedure TPgSQLProtocol30.PutMsgEnd;
begin
  FNet.LeaveSizeBlock(True);
end;

function TPgSQLProtocol30.ProcessMessage(Response: AnsiChar): Boolean;
begin
  Result := True;
  case Response of
    PG_MSG_BACKENDKEYDATA, PG_MSG_PARAMETER_STATUS, PG_MSG_EMPTY_QUERY_RESPONSE: begin
      FNet.ReadInt32;
      Result := inherited ProcessMessage(Response);
    end;
    PG_MSG_PARAMETER_DESCRIPTION:
      ReadParamDescs;
    PG_MSG_PARSE_COMPLETE: begin
      FNet.ReadInt32;
      Result := False;
    end;
    PG_MSG_BIND_COMPLETE: begin
      FNet.ReadInt32;
      Result := False;
    end;
    PG_MSG_CLOSE_COMPLETE:
      FNet.ReadInt32;
    PG_MSG_READY_FOR_QUERY: begin
      FNet.ReadInt32; // skip size
      FTxnStatus := FNet.ReadAnsiChar;
      Result := inherited ProcessMessage(Response);
    end;
    PG_MSG_COPYIN_RESPONSE: begin
      ReadCopyInResponse;
      Result := inherited ProcessMessage(Response);
    end;
    PG_MSG_PORTAL_SUSPENDED: begin
      if FActiveStmt.FSimpleQueryExecute then
        Result := inherited ProcessMessage(Response)
      else begin
        inherited ProcessMessage(Response);
        Result := True;
      end;
    end;
    PG_MSG_SSPI_CONTINUE:
      Result := True;
  else
    Result := inherited ProcessMessage(Response);
  end;
end;

function TPgSQLProtocol30.GetSize: integer;
begin
  Result := FNet.ReadInt32;
end;

function TPgSQLProtocol30.ReadError(IsError: Boolean; UseUnicode: Boolean): EPgError;
var
  Size: Integer;
  ParamChar: AnsiChar;
  ParamBuf: TBytes;
  ParamValue: string;

  Severity: TPgSeverity;
  Msg: string;

  ErrorCode: string;
  DetailMsg: string;
  Hint: string;
  Position: Integer;
  LineNumber: Integer;
  CallStack: string;
  FileName: string;
  Proc: string;
begin
{$IFNDEF VER9P}
  SetLength(ParamBuf, 0);
{$ENDIF}
  Size := GetSize;
  if Size = 1178686529 then begin
    Msg := 'FATA' + FNet.ReadString(-1);

    SplitErrorMsg(Msg, Severity);

    Result := EPgError.Create(Severity, Msg);
  end
  else begin
    Position := 0;
    LineNumber := 0;

    while True do begin
      ParamChar := FNet.ReadAnsiChar;
      if ParamChar = {$IFNDEF NEXTGEN}#0{$ELSE}0{$ENDIF} then
        Break;

      Size := -1;
      if FMessagesEncoding = nil then
          if FNet.UseUnicode and UseUnicode then
            FMessagesEncoding := Encoding.UTF8
          else begin
            if FMessagesCharset <> '' then
              FMessagesEncoding := Encoding.GetEncoding(EncodingToCodePage(FMessagesCharset))
          {$IFDEF MSWINDOWS}
            else
              FMessagesEncoding := Encoding.GetEncoding(EncodingToCodePage(DetectEncoding))
          {$ENDIF}
            ;

            if FMessagesEncoding = nil then
              FMessagesEncoding := Encoding.Default;
          end;
      ParamBuf := FNet.ReadStringAsBytes(Size);
      ParamValue := FMessagesEncoding.GetString(ParamBuf, 0, Size);
      case ParamChar of
        PG_ERROR_FIELD_SEVERITY:
          Severity := StringToSeverity(ParamValue);
        PG_ERROR_FIELD_CODE: begin
          ErrorCode := ParamValue;
          if Severity = {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sUnknown then
            Severity := ErrorCodeToSeverity(ErrorCode);
        end;
        PG_ERROR_FIELD_MESSAGE:
          Msg := ParamValue;
        PG_ERROR_FIELD_DETAILS:
          DetailMsg := ParamValue;
        PG_ERROR_FIELD_HINT:
          Hint := ParamValue;
        PG_ERROR_FIELD_POSITION:
          Position := StrToInt(ParamValue);
        PG_ERROR_FIELD_LINE:
          LineNumber := StrToInt(ParamValue);
        PG_ERROR_FIELD_WHERE:
          CallStack := ParamValue;
        PG_ERROR_FIELD_FILE:
          FileName := ParamValue;
        PG_ERROR_FIELD_ROUTINE:
          Proc := ParamValue;
      end;
    end;

    if Severity = {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sUnknown then
      Severity := {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sError;

    Result := EPgError.Create(Severity, ErrorCode, Msg, DetailMsg,
      Hint, CallStack, FileName, Proc, Position, LineNumber);
  end;

  if IsError then
    SetProtocolState(psError);
end;

procedure TPgSQLProtocol30.SendStartUpMsg(const Database, Username, ApplicationName: string);
const
  ProtocolVersion30 = $30000;
var
  User: string;
{$IFDEF MSWINDOWS}
  GetUserNameExProc: TGetUserNameEx;
  Buf: array[0..255]of Char;
  n: Cardinal;
  Domain: string;
{$ENDIF}
begin
  User := UserName;
{$IFDEF MSWINDOWS}
  if (User = '') and (hSecure32Lib <> 0) then begin
    GetUserNameExProc := GetProcAddress(hSecure32Lib, {$IFDEF UNICODE}'GetUserNameExW'{$ELSE}'GetUserNameExA'{$ENDIF});
    if Assigned(GetUserNameExProc) then begin
      n := SizeOf(Buf);
      if GetUserNameExProc(2{NameSamCompatible}, Buf, n) then begin
        User := string(Buf);
        n := pos('\', User);
        if n > 0 then begin
          Domain := Copy(User, 1, n - 1);
          Delete(User, 1, n);
          User := User + '@' + Domain;
        end;
      end;
    end;
  end;
{$ENDIF}

  FNet.EnterSizeBlock;
  FNet.WriteInt32(protocolVersion30);
  FNet.WriteStringAsBytes(Encoding.UTF8.GetBytes('user'));
  FNet.WriteStringAsBytes(Encoding.UTF8.GetBytes(User));
  FNet.WriteStringAsBytes(Encoding.UTF8.GetBytes('database'));
  FNet.WriteStringAsBytes(Encoding.UTF8.GetBytes(Database));
  // ApplicationName was supported in PostgreSQL 9.0
  if ApplicationName <> '' then begin
    FNet.WriteStringAsBytes(Encoding.UTF8.GetBytes('application_name'));
    FNet.WriteStringAsBytes(Encoding.UTF8.GetBytes(ApplicationName));
  end;
  FNet.WriteByte(0);
  FNet.LeaveSizeBlock(True);
  Flush;
end;

procedure TPgSQLProtocol30.AuthenticateClearText(const Password: string);
begin
  PutMsgStart(PG_MSG_PASSWORD);
  FNet.WriteStringAsBytes(Encoding.UTF8.GetBytes(Password));
  PutMsgEnd;

  Flush;
end;

procedure TPgSQLProtocol30.AuthenticateCrypt(const Password: string);
var
  Salt,
  Crypted: TBytes;
begin
  SetLength(Salt, 2);
  FNet.ReadBytes(TValueArr(Salt), 0, 2);
  Crypted := TUnixCrypt.Crypt(Salt, Encoding.UTF8.GetBytes(Password));

  PutMsgStart(PG_MSG_PASSWORD);
  if Length(Crypted) > 0 then
    FNet.WriteBytes(Crypted);
  FNet.WriteByte(0);
  PutMsgEnd;
  Flush;
end;

procedure TPgSQLProtocol30.AuthenticateMD5(const UserName, Password: string);
var
  Salt: TBytes;
  PasswordHash: TBytes;
begin
  SetLength(Salt, 4);
  FNet.ReadBytes(TValueArr(Salt), 0, 4);
  PasswordHash := CalMD5PasswordHash(UserName, Password, Salt);

  PutMsgStart(PG_MSG_PASSWORD);
  FNet.WriteBytes(PasswordHash);
  FNet.WriteByte(0);
  PutMsgEnd;
  Flush;
end;

function TPgSQLProtocol30.CalcSHA256Hash(const Data: TBytes): TBytes;
var
  Hash: THash_SHA2_256;
begin
  Hash := THash_SHA2_256.Create;
  try
    Result := Hash.ComputeHash(Data);
  finally
    Hash.Free;
  end;
end;

function TPgSQLProtocol30.CalcHMAC(const Key, Msg: TBytes): TBytes;
const
  BlockSize = 64;
  HashLen = 32;
var
  KeyHash, IKeyPad, OKeyPad: TBytes;
  KeyLen, MsgLen, i: integer;
begin
  KeyLen := Length(Key);

  if KeyLen > BlockSize then begin
    KeyHash := CalcSHA256Hash(Key);
    KeyLen := HashLen;
  end
  else
    KeyHash := Key;

  MsgLen := Length(Msg);
  SetLength(IKeyPad, BlockSize + MsgLen);
  SetLength(OKeyPad, BlockSize + HashLen);
  for i := 0 to KeyLen - 1 do begin
    IKeyPad[i] := byte(KeyHash[i] xor $36);
    OKeyPad[i] := byte(KeyHash[i] xor $5C);
  end;
  for i := KeyLen to BlockSize - 1 do begin
    IKeyPad[i] := $36;
    OKeyPad[i] := $5C;
  end;

  Buffer.BlockCopy(Msg, 0, IKeyPad, BlockSize, MsgLen);
  IKeyPad := CalcSHA256Hash(IKeyPad);
  Buffer.BlockCopy(IKeyPad, 0, OKeyPad, BlockSize, HashLen);
  Result := CalcSHA256Hash(OKeyPad);
end;

function TPgSQLProtocol30.CalcPBKDF2(const Password, Salt: TBytes; Iterations, KeyLength: Cardinal): TBytes;
const
  HashLen = 32;
var
  BlockCount,
  BlockNumber,
  SaltLen, i, j, k: Cardinal;
  SaltBlock, Block: TBytes;
begin
  BlockCount := KeyLength div HashLen;
  if (KeyLength mod HashLen) <> 0 then Inc(BlockCount);
  SetLength(Result, BlockCount * HashLen);

  SaltLen := Length(Salt);
  SetLength(SaltBlock, SaltLen + 4);
  Buffer.BlockCopy(Salt, 0, SaltBlock, 0, SaltLen);

  i := 0;
  BlockNumber := 0;
  SetLength(Block, 0);
  while (i < KeyLength) do begin
    inc(BlockNumber);

    SaltBlock[SaltLen] := byte(BlockNumber shr 24);
    SaltBlock[SaltLen + 1] := byte(BlockNumber shr 16);
    SaltBlock[SaltLen + 2] := byte(BlockNumber shr 8);
    SaltBlock[SaltLen + 3] := byte(BlockNumber);
    Block := CalcHMAC(Password, SaltBlock);

    Buffer.BlockCopy(Block, 0, Result, 0, HashLen);
    for j := 0 to Iterations - 2 do begin
      Block := CalcHMAC(Password, Block);
      for k := 0 to HashLen - 1 do
        Byte(Result[i + k]) := Byte(Result[i + k]) xor Byte(Block[k]);
    end;

    i := i + HashLen;
  end;

  SetLength(Result, KeyLength);
end;

procedure TPgSQLProtocol30.AuthenticateScram(const Mechanizm, UserName, Password: string);
var
  g: TGUID;
  n, i, j, Iterations: integer;
  ClientFirstMessage,
  ClientAuthData,
  ServerFirstMessage,
  ServerNonce,
  Salt,
  SaltedPassword,
  ClientKey,
  StoredKey,
  AuthMessage,
  ClientSignature,
  ClientFinalMessage,
  ServerFinalMessage,
  ServerKey,
  ServerSignature: TBytes;
  ClientNonce: string;
  Response: AnsiChar;
  Builder: BytesBuilder;
begin
  CreateGUID(g);
  ClientNonce := GUIDToString(g);
  ClientNonce := Copy(StringReplace(ClientNonce, '-', '', [rfReplaceAll]), 2, 24);

  ClientFirstMessage := Encoding.ANSI.GetBytes('n=,r=' + ClientNonce);
  Builder := BytesBuilder.Create(1024);
  try
    Builder.Append(Encoding.ANSI.GetBytes(Mechanizm));
    Builder.Append(0);
    Builder.Append(0);
    Builder.Append(0);
    Builder.Append(0);
    Builder.Append(Encoding.ANSI.GetBytes(' n,,'));
    Builder.Append(ClientFirstMessage);
    ClientAuthData := Builder.ToBytes;
  finally
    Builder.Free;
  end;

  FNet.WriteAnsiChar(PG_MSG_PASSWORD);
  FNet.WriteInt32(Length(ClientAuthData) + SizeOf(integer));
  FNet.WriteBytes(ClientAuthData);
  Flush;

  Response := {$IFDEF NEXTGEN}AnsiChar{$ENDIF}(#0);
  if FNet.GetReadBufferAvailable > 0 then
    Response := FNet.ReadAnsiChar;
  if Response = PG_MSG_SCRAM_CONTINUE then
    ProcessMessage(Response)
  else
    ProcessMessageQueue(PG_MSG_SCRAM_CONTINUE);

  n := GetSize - 2 * SizeOf(integer);
  FNet.ReadInt32; // ???  integer = 11
  SetLength(ServerFirstMessage, n);
  FNet.ReadBytes(@ServerFirstMessage[0], 0, n);

  i := 0;
  while i < n do begin
    case ServerFirstMessage[i] of
      Ord('r'): begin
        Inc(i, 2);
        j := i;
        while (i < n) and (ServerFirstMessage[i] <> Ord(',')) do
          Inc(i);
        SetLength(ServerNonce, i - j);
        Buffer.BlockCopy(ServerFirstMessage, j, ServerNonce, 0, i - j);
        Inc(i);
      end;
      Ord('s'): begin
        Inc(i, 2);
        j := i;
        while (i < n) and (ServerFirstMessage[i] <> Ord(',')) do
          Inc(i);
        SetLength(Salt, i - j);
        Buffer.BlockCopy(ServerFirstMessage, j, Salt, 0, i - j);
        Inc(i);
      end;
      Ord('i'): begin
        Inc(i, 2);
        TryStrToInt(Encoding.ANSI.GetString(ServerFirstMessage, i, n - i), Iterations);
        Break;
      end;
    else
      raise Exception.Create(sUnexpectedServerResponse);
    end;
  end;

  if (Length(ServerNonce) = 0) or (Length(Salt) = 0) or (Iterations = 0) then
    raise Exception.Create(SAuthenticationFailed);

  SaltedPassword := CalcPBKDF2(Encoding.UTF8.GetBytes(Password), TBase64.Decode(Salt), Iterations, 32);
  ClientKey := CalcHMAC(SaltedPassword, Encoding.ANSI.GetBytes('Client Key'));
  StoredKey := CalcSHA256Hash(ClientKey);

  Builder := BytesBuilder.Create(1024);
  try
    Builder.Append(ClientFirstMessage);
    Builder.Append(Encoding.ANSI.GetBytes(','));
    Builder.Append(ServerFirstMessage);
    Builder.Append(Encoding.ANSI.GetBytes(',c=biws,r='));
    Builder.Append(ServerNonce);
    AuthMessage := Builder.ToBytes;
  finally
    Builder.Free;
  end;

  ClientSignature := CalcHMAC(StoredKey, AuthMessage);
  for i := 0 to 31 do
    ClientKey[i] := byte(ClientKey[i] xor ClientSignature[i]);

  Builder := BytesBuilder.Create(1024);
  try
    Builder.Append(Encoding.ANSI.GetBytes('c=biws,r='));
    Builder.Append(ServerNonce);
    Builder.Append(Encoding.ANSI.GetBytes(',p='));
    Builder.Append(TBase64.Encode(ClientKey));
    ClientFinalMessage := Builder.ToBytes;
  finally
    Builder.Free;
  end;

  FNet.WriteAnsiChar(PG_MSG_PASSWORD);
  FNet.WriteInt32(Length(ClientFinalMessage) + SizeOf(integer));
  FNet.WriteBytes(ClientFinalMessage);
  Flush;

  Response := {$IFDEF NEXTGEN}AnsiChar{$ENDIF}(#0);
  if FNet.GetReadBufferAvailable > 0 then
    Response := FNet.ReadAnsiChar;
  if Response = PG_MSG_SCRAM_CONTINUE then
    ProcessMessage(Response)
  else
    ProcessMessageQueue(PG_MSG_SCRAM_CONTINUE);

  n := GetSize - 2 * SizeOf(integer);
  FNet.ReadInt32; // ???  integer = 12
  SetLength(ServerFinalMessage, n);
  FNet.ReadBytes(@ServerFinalMessage[0], 0, n);

  ServerKey := CalcHMAC(SaltedPassword, Encoding.ANSI.GetBytes('Server Key'));
  ServerSignature := CalcHMAC(ServerKey, AuthMessage);

  if Encoding.ANSI.GetString(ServerFinalMessage) <> ('v=' + Encoding.ANSI.GetString(TBase64.Encode(ServerSignature))) then
    raise Exception.Create(SAuthenticationFailed);
end;

{$IFDEF MSWINDOWS}
procedure TPgSQLProtocol30.AuthenticateSSPI;
var
  ClientAuthData,
  ServerAuthData: TBytes;
  n,
  LastError: integer;
begin
{$IFNDEF VER9P}
  SetLength(ServerAuthData, 0); // anti-warning Delphi 6
{$ENDIF}
  LastError := FNTLMAuth.Initialize;
  if LastError <> 0 then
    raise Exception.Create(SysErrorMessage(LastError));

  LastError := FNTLMAuth.StartAuthentication(ClientAuthData);
  if LastError <> 0 then
    raise Exception.Create(SysErrorMessage(LastError));

  FNet.WriteByte(PG_SSPI_REQUEST_CODE);
  FNet.WriteInt32(Length(ClientAuthData) + SizeOf(integer));
  FNet.WriteBytes(ClientAuthData);
  Flush;
  ProcessMessageQueue(PG_MSG_SSPI_CONTINUE);

  n := GetSize - 2 * SizeOf(integer);
  FNet.ReadInt32; // ???  integer = 8
  SetLength(ServerAuthData, n);
  FNet.ReadBytes(@ServerAuthData[0], 0, n);

  LastError := FNTLMAuth.FinishAuthentication(ServerAuthData, ClientAuthData);
  if LastError <> 0 then
    raise Exception.Create(SysErrorMessage(LastError));

  FNet.WriteByte(PG_SSPI_REQUEST_CODE);
  FNet.WriteInt32(Length(ClientAuthData) + SizeOf(integer));
  FNet.WriteBytes(ClientAuthData);
  Flush;
end;
{$ENDIF}

procedure TPgSQLProtocol30.ReadParamDescs;
var
  i: Integer;
begin
  Assert(FActiveStmt <> nil);
  FNet.ReadInt32; // skip size

  FActiveStmt.FParamsCount := FNet.ReadWord;
  SetLength(FActiveStmt.FParams, FActiveStmt.FParamsCount);

  for i := 0 to FActiveStmt.FParamsCount - 1 do
    FActiveStmt.FParams[i].TypeOid := FNet.ReadInt32;
end;

procedure TPgSQLProtocol30.ReadColDescs;
var
  i: Integer;
begin
  Assert(FActiveStmt <> nil);
  FNet.ReadInt32; // skip size

  FActiveStmt.FFieldsCount := FNet.ReadWord;
  SetLength(FActiveStmt.FFields, FActiveStmt.FFieldsCount);
  for i := 0 to FActiveStmt.FFieldsCount - 1 do
    with FActiveStmt.FFields[i] do begin
      FieldName := FNet.ReadString(-1);
      TableOid := FNet.ReadInt32;
      TableCol := FNet.ReadInt16;
      TypeOid := FNet.ReadInt32;
      TypeSize := FNet.ReadInt16;
      TypeModifier := FNet.ReadInt32;
      FormatCode := FNet.ReadInt16;
    end;
end;

procedure TPgSQLProtocol30.FetchRow(Stmt: TPgSQLStatement; RowNo: integer; FetchBlock: IntPtr;
  FetchFieldProc: TFetchFieldProc);
var
  i: integer;
  Size: integer;
begin
  Size := GetSize;

  if @FetchFieldProc = nil then begin
    FNet.FlushReceive(Size - 4);
    Exit;
  end;

  FNet.ReadWord; // fields count

  for i := 0 to Stmt.FFieldsCount - 1 do begin
    Size := FNet.ReadInt32;

    FetchFieldProc(FBufferProvider, Size, FetchBlock, RowNo, i);
  end;
end;

procedure TPgSQLProtocol30.PreFetchRow(Stmt: TPgSQLStatement; ReadStream: TPgSQLReadStream);
var
  Size: integer;
begin
  Size := GetSize;
  FNet.ReadWord; // fields count
  ReadStream.WriteNetData(Self, Size - 6);
end;

procedure TPgSQLProtocol30.ReadNotifications;
var
  PID: integer;
  Name,
  PayLoad: string;
begin
  FNet.ReadInt32;
  PID := FNet.ReadInt32;
  Name := FNet.ReadString(-1);
  PayLoad := FNet.ReadString(-1); // supported since PostgreSQL 9.0 (before is empty always)
  if Assigned(FOnNotification) then
    FOnNotification(Name, PID, PayLoad);
end;

procedure TPgSQLProtocol30.ReadCopyInResponse;
var
  ColCount, i: integer;
begin
  FNet.ReadInt32;
  FNet.ReadByte; // binaryTuples
  ColCount := FNet.ReadInt16;
  for i := 0 to ColCount - 1 do
    FNet.ReadInt16; // fformat
end;

procedure TPgSQLProtocol30.Hold;
begin
  HoldCommand;

  Flush;

  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.Sync;
begin
  SyncCommand;
  Flush;

  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.Ping(ProcessResponse: boolean = True);
begin
  SyncCommand;
  Flush;

  if ProcessResponse then
    ProcessMessageQueue;
end;

{ BytesBuilder }

constructor BytesBuilder.Create(Capacity: Integer);
begin
  inherited Create;

  FActualLength := 0;
  SetLength(FBuffer, Capacity);
end;

procedure BytesBuilder.SetActualLength(Value: Integer);
var
  l: Integer;
begin
  l := System.Length(FBuffer);
  if l - FActualLength < Value then
    SetLength(FBuffer, FActualLength + Value + l);
  FActualLength := Value;
end;

procedure BytesBuilder.Append(Value: TBytes);
var
  l, ls: Integer;
begin
  ls := System.Length(value);
  if ls = 0 then
    Exit;

  l := System.Length(FBuffer);
  if l - FActualLength < ls then
    SetLength(FBuffer, FActualLength + ls + l);
  Move(Value[0], FBuffer[FActualLength], ls);
  Inc(FActualLength, ls);
end;

procedure BytesBuilder.Append(Value: byte);
var
  l: Integer;
begin
  l := System.Length(FBuffer);
  if l - FActualLength < 1 then
    SetLength(FBuffer, FActualLength + l + 1);
  Move((@Value)^, FBuffer[FActualLength], 1);
  Inc(FActualLength);
end;

function BytesBuilder.ToBytes: TBytes;
begin
  SetLength(Result, FActualLength);
  if FActualLength > 0 then
    Move(FBuffer[0], Result[0], FActualLength);
end;

{ TPgSQLMemoryStream }

constructor TPgSQLReadStream.Create(Stmt: TPgSQLStatement);
var
  Page: PPgStreamPage;
begin
  inherited Create;

  FStmt := Stmt;

  FBufferProvider := TPgStreamBufferProvider.Create(Self);

  FListLock := TCriticalSection.Create;
  FPages := TList.Create;
  New(Page);
  Page.Size := 0;
  FPages.Add(Page);
end;

destructor TPgSQLReadStream.Destroy;
begin
  Clear;

  FListLock.Enter;
  try
    Dispose(PPgStreamPage(FPages[0]));
    FPages.Free;
  finally
    FListLock.Leave;
  end;
  FListLock.Free;


  FBufferProvider.Free;

  inherited;
end;

procedure TPgSQLReadStream.ReadData(Buffer: IntPtr; Count: Integer);
var
  Size: Integer;
begin
  if FCurrentPosition >= FCurrentPageSize then
    GetRow;

  while True do begin
    Size := FCurrentPageSize - FCurrentPosition;
    if Size = 0 then begin
      Sleep(0);
      GetRow;
    end
    else begin
      if Size > Count then
        Size := Count;
      case Size of
        1:
          Byte(Buffer^) := FCurrentPageData[FCurrentPosition];
        2:
          Word(Buffer^) := PWord(@FCurrentPageData[FCurrentPosition])^;
        4:
          Cardinal(Buffer^) := PCardinal(@FCurrentPageData[FCurrentPosition])^;
      {$IFDEF CPU64}
        8:
          UInt64(Buffer^) := PUInt64(@FCurrentPageData[FCurrentPosition])^;
      {$ENDIF}
        else
          Move(FCurrentPageData[FCurrentPosition], Buffer^, Size);
      end;
      FCurrentPosition := FCurrentPosition + Size;
      if Count > Size then begin
        Buffer := PtrOffset(Buffer, Size);
        Count := Count - Size;
      end
      else
        Exit;
    end;
  end;
end;

procedure TPgSQLReadStream.WriteData(DataPtr: IntPtr; DataSize: Integer);
var
  Page: PPgStreamPage;
  PageSize: Integer;
  Size: Integer;
begin
  FListLock.Enter;
  try
    Page := PPgStreamPage(FPages[FPages.Count - 1]);
    PageSize := PPgStreamPage(Page).Size;
  finally
    FListLock.Leave;
  end;

  while True do begin
    Size := SizeOf(TPgStreamPageData) - PageSize;
    if Size <= 0 then begin
      New(Page);
      if DataSize <= SizeOf(TPgStreamPageData) then
        Size := DataSize
      else
        Size := SizeOf(TPgStreamPageData);

      Move(DataPtr^, Page.Data[0], Size);
      PageSize := Size;

      FListLock.Enter;
      try
        Page.Size := PageSize;
        FPages.Add(Page);
      finally
        FListLock.Leave;
      end;
    end
    else begin
      if DataSize <= Size then
        Size := DataSize;

      Move(DataPtr^, Page.Data[PageSize], Size);
      PageSize := PageSize + Size;

      FListLock.Enter;
      try
        Page.Size := PageSize;
      finally
        FListLock.Leave;
      end;
    end;

    if Size < DataSize then begin
      DataPtr := PtrOffset(DataPtr, Size);
      DataSize := DataSize - Size;
    end
    else
      Exit;
  end;
end;

procedure TPgSQLReadStream.WriteNetData(Protocol: TPgSQLProtocol; DataSize: Integer);
var
  Page: PPgStreamPage;
  PageSize: integer;
  Size: integer;
begin
  FListLock.Enter;
  try
    Page := PPgStreamPage(FPages[FPages.Count - 1]);
    PageSize := PPgStreamPage(Page).Size;
  finally
    FListLock.Leave;
  end;

  while True do begin
    Size := SizeOf(TPgStreamPageData) - PageSize;
    if Size <= 0 then begin
      New(Page);
      if DataSize <= SizeOf(TPgStreamPageData) then
        Size := DataSize
      else
        Size := SizeOf(TPgStreamPageData);

      Protocol.FNet.ReadBytes(@Page.Data[0], 0, Size);
      PageSize := Size;

      FListLock.Enter;
      try
        Page.Size := PageSize;
        FPages.Add(Page);
      finally
        FListLock.Leave;
      end;
    end
    else if DataSize <= Size then begin
      Size := DataSize;

      Protocol.FNet.ReadBytes(@Page.Data[PageSize], 0, Size);
      PageSize := PageSize + Size;

      FListLock.Enter;
      try
        Page.Size := PageSize;
      finally
        FListLock.Leave;
      end;
    end
    else begin
      Protocol.FNet.ReadBytes(@Page.Data[PageSize], 0, Size);
      PageSize := PageSize + Size;

      FListLock.Enter;
      try
        Page.Size := PageSize;
      finally
        FListLock.Leave;
      end;
    end;

    if Size < DataSize then
      DataSize := DataSize - Size
    else
      Exit;
  end;
end;

procedure TPgSQLReadStream.SetIsLastRow(Value: boolean);
begin
  FIsLastRow := Value;
end;

procedure TPgSQLReadStream.FetchRow(RowNo: integer; FetchBlock: IntPtr; FetchFieldProc: TFetchFieldProc);
var
  i: integer;
  Size: integer;
begin
  if @FetchFieldProc = nil then
    Exit;

  try
    for i := 0 to FStmt.FFieldsCount - 1 do begin
      Size := FBufferProvider.ReadInt32;
      FetchFieldProc(FBufferProvider, Size, FetchBlock, RowNo, i);
    end;
  except
    on e: Exception do
      raise;
  end;
end;

procedure TPgSQLReadStream.GetRow;
var
  Page: PPgStreamPage;
  PageSize: Integer;
  PageCount: Integer;
begin
  FListLock.Enter;
  try
    Page := PPgStreamPage(FPages[FCurrentPageIndex]);
    PageSize := Page.Size;
    PageCount := FPages.Count;
  finally
    FListLock.Leave;
  end;

  if FCurrentPageSize < PageSize then
    FCurrentPageSize := PageSize
  else if PageSize < SizeOf(TPgStreamPageData) then begin
    FProtocol.Reader.ReadRow(FProtocol, Self);

    FListLock.Enter;
    try
      FCurrentPageSize := Page.Size;
    finally
      FListLock.Leave;
    end;
  end
  else if PageSize >= SizeOf(TPgStreamPageData) then begin
    if FCurrentPageIndex + 1 >= PageCount then
      FProtocol.Reader.ReadRow(FProtocol, Self);

    FListLock.Enter;
    try
      // check if new page was read
      if FCurrentPageIndex + 1 >= FPages.Count then
        Exit;

      if FFreePrevPages and (FCurrentPageIndex > 0) then begin
        // Free page
        Page := PPgStreamPage(FPages[FCurrentPageIndex]);
        Marshal.FreeHGlobal(Page);
        FPages[FCurrentPageIndex] := nil;
      end;
      Inc(FCurrentPageIndex);
      Page := PPgStreamPage(FPages[FCurrentPageIndex]);
      FCurrentPageData := @Page.Data;
      FCurrentPageSize := Page.Size;
      FCurrentPosition := 0;
    finally
      FListLock.Leave;
    end;
  end;
end;

procedure TPgSQLReadStream.Clear;
var
  i: Integer;
  Page: PPgStreamPage;
begin
  FListLock.Enter;
  try
    for i := 1 to FPages.Count - 1 do begin
      Page := FPages[i];
      if Page <> nil then
        Marshal.FreeHGlobal(Page);
    end;
    FPages.Count := 1;

    Page := PPgStreamPage(FPages[0]);
    Page.Size := 0;

    FCurrentPageData := @Page.Data;
    FCurrentPageSize := 0;
    FCurrentPageIndex := 0;
    FCurrentPosition := 0;
  finally
    FListLock.Leave
  end;
end;

procedure TPgSQLReadStream.Reset(Protocol: TPgSQLProtocol);
begin
  FIsLastRow := False;

  if Protocol <> nil then
    Protocol.Reader.Stop
  else
    Exit;

  Clear;
end;

function TPgSQLReadStream.FetchStmt(Protocol: TPgSQLProtocol; Rows: integer; FetchBlock: IntPtr; FetchFieldProc: TFetchFieldProc): integer;
begin
  FProtocol := Protocol;
  try
    Result := 0;

    while Result < Rows do
      // always check LastRow before HasData !!!
      if IsLastRow then
        if HasData then begin
          FetchRow(Result, FetchBlock, FetchFieldProc);
          Inc(Result);
        end
        else
          Break
      else
        if HasData then begin
          FetchRow(Result, FetchBlock, FetchFieldProc);
          Inc(Result);
        end
        else
          Sleep(0); // wait for read data from socket
  finally
    FProtocol := nil;
  end;
end;

function TPgSQLReadStream.PrefetchStmt(Protocol: TPgSQLProtocol): integer;
begin
  Result := 0;

  while True do
    if ReadNextRow(Protocol) then
      Inc(Result)
    else
      Break;
end;

function TPgSQLReadStream.ReadNextRow(Protocol: TPgSQLProtocol): boolean;
begin
  if FIsLastRow then begin
    Result := False;
    Exit;
  end;

  if Protocol.FProtocolState <> psStmtFetching then begin
    SetIsLastRow(True);
    Result := False;
    Exit;
  end;

  Result := True;

  Protocol.PreFetchRow(FStmt, Self);

  Protocol.ProcessMessageQueue;
  if Protocol.FProtocolState <> psStmtFetching then
    SetIsLastRow(True);
end;

function TPgSQLReadStream.HasData: boolean;
var
  Page: PPgStreamPage;
  PageSize: Integer;
begin
  if FCurrentPosition < FCurrentPageSize then
    Result := True
  else begin
    FListLock.Enter;
    try
      if FCurrentPageIndex + 1 < FPages.Count then
        Result := True
      else begin
        Page := PPgStreamPage(FPages[FCurrentPageIndex]);
        PageSize := Page.Size;
        if FCurrentPosition < PageSize then
          Result := True
        else
          Result := False;
      end;
    finally
      FListLock.Leave
    end;
  end;
end;

{ TPgSQLReadThread }

constructor TPgSQLReadThread.Create(Protocol: TPgSQLProtocol; Lock: TPgSQLCriticalSection; DataReadEvent: TEvent);
begin
  inherited Create(False);

  FProtocol := Protocol;
  FLock := Lock;
  FDataReadEvent := DataReadEvent;

  FPauseEvent := TEvent.Create(nil, True, False, '');
  FStopEvent := TEvent.Create(nil, True, False, '');

  FreeOnTerminate := True;
end;

destructor TPgSQLReadThread.Destroy;
begin
  FPauseEvent.Free;
  FStopEvent.Free;
  FDataReadEvent.Free;
  FLock.Free;

  inherited;
end;

function TPgSQLReadThread.GetIsStopped: Boolean;
begin
  Result := FPauseEvent.WaitFor(0) <> wrSignaled;
end;

procedure TPgSQLReadThread.Execute;
var
  NoData: boolean;
begin
  while True do begin
    if FPauseEvent.WaitFor(INFINITE) = wrSignaled then begin
      FLock.Enter;
      try
        if Terminated then
          Break;

        NoData := True;
        if FStream <> nil then
          try
            NoData := not FStream.ReadNextRow(FProtocol);
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

        if NoData then
          Stop;
      finally
        FDataReadEvent.SetEvent;
        FLock.Leave;
      end;
    end;
  end;
end;

procedure TPgSQLReadThread.CheckLastError;
begin
  if FLastError = nil then
    Exit;

  try
    raise FLastError;
  finally
    FLastError := nil;
  end;
end;

procedure TPgSQLReadThread.Run(Stream: TPgSQLReadStream);
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' ReadThread $' + IntToHex(Integer(Self), 8) + ' RUN'));
{$ENDIF}
  FStream := Stream;

  FPauseEvent.SetEvent;
  FStopEvent.ResetEvent;
end;

procedure TPgSQLReadThread.Stop;
begin
{$IFDEF DEBUG_MULTITHREAD}
  OutputDebugString(PChar('#' + IntToStr(GetCurrentThreadId) + ' ReadThread $' + IntToHex(Integer(Self), 8) + ' STOP'));
{$ENDIF}

  FPauseEvent.ResetEvent;
  FStopEvent.SetEvent;

  FStream := nil;
end;

{ TPgSQLReader }

constructor TPgSQLReader.Create(Protocol: TPgSQLProtocol);
begin
  inherited Create;

  FProtocol := Protocol;

  FLock := TPgSQLCriticalSection.Create;
  FDataReadEvent := TEvent.Create(nil, False, False, '');
end;

destructor TPgSQLReader.Destroy;
begin
  if FReadThread <> nil then begin
    FLock.Enter;
    try
      FReadThread.Terminate;
      FReadThread.Run(nil);
      FReadThread := nil;
    finally
      FDataReadEvent.SetEvent;
      FLock.Leave;
    end;
  end
  else begin
    FDataReadEvent.Free;
    FLock.Free;
  end;

  inherited;
end;

procedure TPgSQLReader.CheckLastError;
begin
  FLock.Enter;
  try
    if FReadThread <> nil then
      FReadThread.CheckLastError;
  finally
    FLock.Leave;
  end;
end;

procedure TPgSQLReader.Run(Stream: TPgSQLReadStream);
begin
  FLock.Enter;
  try
    Stream.FIsLastRow := False;
    if FReadThread = nil then
      FReadThread := TPgSQLReadThread.Create(FProtocol, FLock, FDataReadEvent);
    FReadThread.Run(Stream);
  finally
    FDataReadEvent.ResetEvent;
    FLock.Leave;
  end;
end;

procedure TPgSQLReader.Stop;
begin
  FLock.Enter;
  try
    if FReadThread <> nil then
      FReadThread.Stop;
  finally
    FDataReadEvent.SetEvent;
    FLock.Leave;
  end;
end;

function TPgSQLReader.IsStopped: boolean;
begin
  Result := (FReadThread = nil) or FReadThread.IsStopped;
end;

procedure TPgSQLReader.WaitForStop;
begin
  if (FReadThread <> nil) and not FReadThread.IsStopped then
    FReadThread.FStopEvent.WaitFor(INFINITE);
end;

procedure TPgSQLReader.ReadRow(Protocol: TPgSQLProtocol; Stream: TPgSQLReadStream);
begin
{$IFDEF FPC}
  if FLock.TryEnter then
{$ELSE}{$IFDEF VER10P}
  if FLock.TryEnter then
{$ELSE}
  if TryEnterCriticalSection(TPgSQLCriticalSection(FLock).FSection) then
{$ENDIF}{$ENDIF}
    try
      CheckLastError;

      if (FReadThread = nil) or FReadThread.IsStopped then
        Stream.ReadNextRow(Protocol);
    finally
      FLock.Leave
    end
  else
    FDataReadEvent.WaitFor(INFINITE);
end;

{ TPgStreamBuferProvider }

constructor TPgStreamBufferProvider.Create(const Stream: TPgSQLReadStream);
begin
  inherited Create;

  FStream := Stream;
end;

function TPgStreamBufferProvider.ReadInt32NoSwap: integer;
begin
  FStream.ReadData(@Result, SizeOf(Integer));
end;

procedure TPgStreamBufferProvider.ReadBytes(Buffer: TValueArr; Offset, Count: integer);
begin
  if Count > 0 then
    FStream.ReadData(PtrOffset(Buffer, Offset), Count);
end;

function TPgStreamBufferProvider.ReadByte: Byte;
begin
  if FStream.FCurrentPageSize - FStream.FCurrentPosition >= SizeOf(Byte) then begin
    Result := FStream.FCurrentPageData[FStream.FCurrentPosition];
    Inc(FStream.FCurrentPosition);
  end
  else
    FStream.ReadData(@Result, SizeOf(Byte));
end;

function TPgStreamBufferProvider.ReadWord: Word;
begin
  FStream.ReadData(@Result, SizeOf(Word));
  Result := FastSwap(Result);
end;

function TPgStreamBufferProvider.ReadInt16: Smallint;
begin
  FStream.ReadData(@Result, SizeOf(SmallInt));
  Result := FastSwap(Result);
end;

function TPgStreamBufferProvider.ReadInt32: integer;
begin
  Result := FastSwap(ReadInt32NoSwap);
end;

function TPgStreamBufferProvider.ReadInt64: Int64;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 8);
  ReadBytes(TValueArr(Buf), 0, 8);

  for i := 0 to 3 do begin
     Buf[i] := Buf[i] xor Buf[7 - i];
     Buf[7 - i] := Buf[i] xor Buf[7 - i];
     Buf[i] := Buf[i] xor Buf[7 - i];
  end;

  Result := BitConverter.ToInt64(Buf, 0);
end;

function TPgStreamBufferProvider.ReadSingle: Single;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 4);
  ReadBytes(TValueArr(Buf), 0, 4);

  for i := 0 to 1 do begin
     Buf[i] := Buf[i] xor Buf[3 - i];
     Buf[3 - i] := Buf[i] xor Buf[3 - i];
     Buf[i] := Buf[i] xor Buf[3 - i];
  end;

  Result := BitConverter.ToSingle(Buf, 0);
end;

function TPgStreamBufferProvider.ReadDouble: Double;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 8);
  ReadBytes(TValueArr(Buf), 0, 8);

  for i := 0 to 3 do begin
     Buf[i] := Buf[i] xor Buf[7 - i];
     Buf[7 - i] := Buf[i] xor Buf[7 - i];
     Buf[i] := Buf[i] xor Buf[7 - i];
  end;

  Result := BitConverter.ToDouble(Buf, 0);
end;

function TPgStreamBufferProvider.ReadAnsiChar: AnsiChar;
begin
  Result := AnsiChar(ReadByte);
end;

function TPgStreamBufferProvider.ReadAnsiString: AnsiString;
begin
  Result := ReadAnsiString(-1);
end;

function TPgStreamBufferProvider.ReadAnsiString(Count: integer): AnsiString;
var
  Buf: TBytes;
begin
  Buf := ReadStringAsBytes(Count);
  if Count > 0 then
    if FStream.FUseUnicode then
    {$IFDEF NEXTGEN}
      Result := AnsiString(Encoding.UTF8.GetString(Buf, 0, Count))
    {$ELSE}
      Result := Encoding.UTF8.GetAnsiString(Buf, 0, Count)
    {$ENDIF}
    else
      Result := Marshal.PtrToStringAnsi(Buf, Count)
  else
    Result := '';
end;

function TPgStreamBufferProvider.ReadAnsiString(Buffer: IntPtr; Count: integer; AddNull: boolean): integer;
var
  sa: AnsiString;
begin
  if FStream.FUseUnicode then begin
    sa := ReadAnsiString(Count);
    Result := Length(sa);
    if Result > 0 then
      Move(sa[1]{$IFDEF NEXTGEN}^{$ENDIF}, Buffer^, Result);
  end
  else begin
    ReadBytes(Buffer, 0, Count);
    Result := Count;
  end;

  if AddNull then
    Marshal.WriteByte(Buffer, Result, 0);
end;

function TPgStreamBufferProvider.ReadWideString: WideString;
begin
  Result := ReadWideString(-1);
end;

function TPgStreamBufferProvider.ReadWideString(Count: integer): WideString;
var
  Buf: TBytes;
begin
  Buf := ReadStringAsBytes(Count);
  if Count > 0 then begin
    if FStream.FUseUnicode then
      Result := Encoding.UTF8.{$IFDEF IS_UNICODE}GetString{$ELSE}GetWideString{$ENDIF}(Buf, 0, Count)
    else
      Result := Encoding.Default.{$IFDEF IS_UNICODE}GetString{$ELSE}GetWideString{$ENDIF}(Buf, 0, Count);
  end
  else
    Result := '';
end;

function TPgStreamBufferProvider.ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean): integer;
var
  s: WideString;
begin
  s := ReadWideString(Count);
  Result := Length(s) * sizeof(WideChar);
  if s <> '' then
    Move(s[1], Buffer^, Result);

  if AddNull then
    Marshal.WriteInt16(Buffer, Result, 0);
end;

function TPgStreamBufferProvider.ReadString(Count: integer): string;
var
  Buf: TBytes;
begin
  Buf := ReadStringAsBytes(Count);
  if Count > 0 then
    if FStream.FUseUnicode then
      Result := Encoding.UTF8.GetString(Buf, 0, Count)
    else
      Result := Encoding.Default.GetString(Buf, 0, Count)
  else
    Result := '';
end;

function TPgStreamBufferProvider.ReadStringAsBytes(var Count: integer): TBytes;
var
  Len: integer;
  b: byte;
  Stream: MemoryStream;
begin
  Len := 0;

  if Count < 0 then
    Stream := MemoryStream.Create(512)
  else
    Stream := MemoryStream.Create(Count);
  try
    if Count <> 0 then begin
      repeat
        b := ReadByte;
        Inc(Len);
        Stream.Write(PAnsiChar(@b), 0, 1);
      until (b = 0) or (Len = Count);
    end;

    Result := Stream.ToArray;
    Count := Stream.Position;
  finally
    Stream.Free;
  end;
end;

end.

