/// Database Framework Direct Oracle Connnection via OCI
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.oracle;

{
  *****************************************************************************

   Oracle Database Access via the OCI High-Performance Library
    -  TSqlDBOracleConnection* and TSqlDBOracleStatement Classes

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.perf,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql;


{ ************ TSqlDBOracleConnection* and TSqlDBOracleStatement Classes }

type
  /// will implement properties shared by native Oracle Client Interface connections
  TSqlDBOracleConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    fRowsPrefetchSize: integer;
    fBlobPrefetchSize: integer;
    fStatementCacheSize: integer;
    fInternalBufferSize: integer;
    fEnvironmentInitializationMode: integer;
    fOnPasswordChanged: TNotifyEvent;
    fOnPasswordExpired: TOnPasswordExpired;
    fUseWallet: boolean;
    fIgnoreORA01453OnStartTransaction: boolean;
    function GetClientVersion: RawUtf8;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
    procedure PasswordChanged(const ANewPassword: RawUtf8);
  public
    /// initialize the connection properties
    // - we don't need a database name parameter for Oracle connection: only
    // aServerName is to be set
    // - you may specify the TNSName in aServerName, or a connection string
    // like '//host[:port]/[service_name]', e.g. '//sales-server:1523/sales'
    // - connection is opened globaly as UTF-8, to match the internal encoding
    // of our units; but CHAR / NVARCHAR2 fields will use the Oracle charset
    // as retrieved from the opened connection (to avoid any conversion error)
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8); override;
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBOracleConnection instance
    function NewConnection: TSqlDBConnection; override;
    /// extract the TNS listener name from a Oracle full connection string
    // - e.g. ExtractTnsName('1.2.3.4:1521/dbname') returns 'dbname'
    class function ExtractTnsName(const aServerName: RawUtf8): RawUtf8;
    /// determine if the SQL statement can be cached
    // - always returns false, to force server-side caching only on this driver
    function IsCachable(P: PUtf8Char): boolean; override;
    function SqlLimitClause(AStmt: TSelectStatement): TSqlDBDefinitionLimitClause; override;
  published
    /// returns the Client version e.g. 'oci.dll rev. 11.2.0.1'
    property ClientVersion: RawUtf8
      read GetClientVersion;
    /// the OCI initialization mode used for the connection
    // - equals OCI_EVENTS or OCI_THREADED by default, since will likely be
    // used in a multi-threaded context (even if this class is inheriting from
    // TSqlDBConnectionPropertiesThreadSafe), and  OCI_EVENTS is needed to support
    // Oracle RAC Connection Load Balancing
    // - can be tuned depending on the configuration or the Oracle version
    property EnvironmentInitializationMode: integer
      read fEnvironmentInitializationMode write fEnvironmentInitializationMode;
    /// the size (in bytes) of the internal buffer used to retrieve rows in statements
    // - default is 128 KB, which gives very good results
    property InternalBufferSize: integer
      read fInternalBufferSize write fInternalBufferSize;
    /// the size (in bytes) of rows data prefecth at OCI driver level
    // - is set to 128 KB by default, but may be changed for tuned performance
    property RowsPrefetchSize: integer
      read fRowsPrefetchSize write fRowsPrefetchSize;
    /// the size (in bytes) of LOB prefecth
    // - is set to 4096 (4 KB) by default, but may be changed for tuned performance
    property BlobPrefetchSize: integer
      read fBlobPrefetchSize write fBlobPrefetchSize;
    /// Password Expired event
    property OnPasswordExpired: TOnPasswordExpired
      read FOnPasswordExpired write FOnPasswordExpired;
    /// Password changed event
    property OnPasswordChanged: TNotifyEvent
      read FOnPasswordChanged write FOnPasswordChanged;
    /// the number of prepared statements cached by OCI on the Client side
    // - is set to 30 by default
    // - only used if UseCache=true
    property StatementCacheSize: integer
      read fStatementCacheSize write fStatementCacheSize;
    /// use the Secure External Password Store for Password Credentials
    // - see Oracle documentation
    // http://docs.oracle.com/cd/B28359_01/network.111/b28531/authentication.htm#DBSEG97906
    property UseWallet: boolean
      read fUseWallet write fUseWallet;
    /// When we execute a SELECT statement across a database link, a transaction lock is placed
    // on the undo segments (transaction is implicity started).
    // Setting this options to true allow to ignore ORA-01453 during
    // TSqlDBOracleConnection.StartTransaction call.
    // - see Oracle documentation
    // http://docs.oracle.com/cd/B28359_01/server.111/b28310/ds_appdev002.htm
    property IgnoreORA01453OnStartTransaction: boolean
      read fIgnoreORA01453OnStartTransaction write fIgnoreORA01453OnStartTransaction;
  end;

  /// implements a direct connection to the native Oracle Client Interface (OCI)
  TSqlDBOracleConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fEnv: pointer;
    fError: pointer;
    fServer: pointer;
    fContext: pointer;
    fSession: pointer;
    fTrans: pointer;
    fOCICharSet: cardinal;
    fType_numList: pointer;
    fType_strList: pointer;
    // match DB charset for CHAR/NVARCHAR2, nil for OCI_UTF8/OCI_AL32UTF8
    fAnsiConvert: TSynAnsiConvert;
    procedure OCISTRToUtf8(P: PAnsiChar; var result: RawUtf8;
      ColumnDBCharSet,ColumnDBForm: cardinal);
    {$ifndef UNICODE}
    procedure OCISTRToAnsiString(P: PAnsiChar; var result: AnsiString;
      ColumnDBCharSet,ColumnDBForm: cardinal);
    {$endif UNICODE}
  public
    /// prepare a connection to a specified Oracle database server
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified Oracle database server
    // - should raise an Exception on error
    // - the connection will be globaly opened with UTF-8 encoding; for CHAR /
    // NVARCHAR2 fields, the DB charset encoding will be retrieved from the
    // server, to avoid any truncation during data retrieval
    // - BlobPrefetchSize, RowsPrefetchSize and StatementCacheSize field values
    // of the associated properties will be used to tune the opened connection
    procedure Connect; override;
    /// stop connection to the specified Oracle database server
    // - should raise an Exception on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// initialize a new SQL query statement for the given connection
    // - if UseCache=true, this overridden implementation will use server-side
    // Oracle statement cache - in this case, StatementCacheSize will define
    // how many statements are to be cached - not that IsCachable() has been
    // overriden to return false, so statement cache on client side is disabled
    // - the caller should free the instance after use
    function NewStatement: TSqlDBStatement; override;
    /// begin a Transaction for this connection
    // - current implementation do not support nested transaction with those
    // methods: exception will be raised in such case
    // - by default, TSqlDBOracleStatement works in AutoCommit mode, unless
    // StartTransaction is called
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// allows to change the password of the current connected user
    // - will first launch the OnPasswordExpired event to retrieve the new
    // password, then change it and call OnPasswordChanged event on success
    function PasswordChange: boolean; override;
  end;

  /// implements a statement via the native Oracle Client Interface (OCI)
  // - those statements can be prepared on the client side, but by default we
  // enabled the OCI-side statement cache, not to reinvent the wheel this time
  // - note that bound OUT ftUtf8 parameters will need to be pre-allocated
  // before calling - e.g. via BindTextU(RawUtf8OfChar(3000),paramOut)
  // - you can also bind an TInt64DynArray or TRawUtf8DynArray as parameter to
  // be assigned later as an OCI_OBJECT so that you may write such statements:
  // ! var arr: TInt64DynArray = [1, 2, 3];
  // ! Query := TSqlDBOracleConnectionProperties.NewThreadSafeStatementPrepared(
  // !   'select * from table where table.id in '+
  // !     '(select column_value from table(cast(? as SYS.ODCINUMBERLIST)))');
  // ! Query.BindArray(1,arr);
  // ! Query.ExecutePrepared;
  // (use SYS.ODCIVARCHAR2LIST type cast for TRawUtf8DynArray values)
  TSqlDBOracleStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fStatement: pointer;
    fError: pointer;
    fPreparedParamsCount: integer;
    fRowCount: cardinal;
    fRowBufferCount: cardinal;
    fRowFetched: cardinal;
    fRowFetchedCurrent: cardinal;
    fRowFetchedEnded: boolean;
    fRowBuffer: TByteDynArray;
    fBoundCursor: array of pointer;
    fInternalBufferSize: cardinal;
    // warning: shall be 32 bits aligned!
    fTimeElapsed: TPrecisionTimer;
    fUseServerSideStatementCache: boolean;
    function DateTimeToDescriptor(aDateTime: TDateTime): pointer;
    procedure FreeHandles(AfterError: boolean);
    procedure FetchTest(Status: integer);
    /// Col=0...fColumnCount-1
    function GetCol(Col: integer; out Column: PSqlDBColumnProperty): pointer;
    // called by Prepare and CreateFromExistingStatement
    procedure SetColumnsForPreparedStatement;
    // called by Step and CreateFromExistingStatement
    procedure FetchRows;
  public
    /// create an OCI statement instance, from an existing OCI connection
    // - the Execute method can be called once per TSqlDBOracleStatement instance,
    // but you can use the Prepare once followed by several  ExecutePrepared methods
    // - if the supplied connection is not of TOleDBConnection type, will raise
    //   an exception
    constructor Create(aConnection: TSqlDBConnection); override;
    /// initialize the class from an existing OCI statement (and connection)
    // - to be called e.g. by ColumnCursor() for SQLT_RSET kind of column
    constructor CreateFromExistingStatement(aConnection: TSqlDBConnection; aStatement: pointer);
    /// release all associated memory and OCI handles
    destructor Destroy; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBOracle on any error
    // - if aSql requires a trailing ';', you should end it with ';;' e.g. for
    // $ DB.ExecuteNoResult(
    // $  'CREATE OR REPLACE FUNCTION ORA_POC(MAIN_TABLE IN VARCHAR2, REC_COUNT IN NUMBER, BATCH_SIZE IN NUMBER) RETURN VARCHAR2' +
    // $  ' AS LANGUAGE JAVA' +
    // $  ' NAME ''OraMain.selectTable(java.lang.String, int, int) return java.lang.String'';;', []);
    procedure Prepare(const aSql: RawUtf8; ExpectResults: boolean = false); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - raise an ESqlDBOracle on any error
    procedure ExecutePrepared; override;

    /// After a statement has been prepared via Prepare() + ExecutePrepared() or
    // Execute(), this method must be called one or more times to evaluate it
    // - you shall call this method before calling any Column*() methods
    // - return TRUE on success, with data ready to be retrieved by Column*()
    // - return FALSE if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - access the first or next row of data from the SQL Statement result:
    // if SeekFirst is TRUE, will put the cursor on the first row of results,
    // otherwise, it will fetch one row of data, to be called within a loop
    // - raise an ESqlDBOracle on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// finalize the OCI cursor resources - not implemented yet
    procedure ReleaseRows; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    // - should retrieve directly the 64 bit Currency content, to avoid
    // any rounding/conversion error from floating-point types
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUtf8(Col: integer): RawUtf8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    // - ColumnBlob() will return the binary content of the field is was not ftBlob,
    // e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
    // or a direct mapping of the RawUnicode
    function ColumnBlob(Col: integer): RawByteString; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    // - this function will return the BLOB content as a TBytes
    // - this default virtual method will call ColumnBlob()
    function ColumnBlobBytes(Col: integer): TBytes; override;
    /// read a blob Column into the Stream parameter
    procedure ColumnBlobToStream(Col: integer; Stream: TStream); override;
    /// write a blob Column into the Stream parameter
    // - expected to be used with 'SELECT .. FOR UPDATE' locking statements
    procedure ColumnBlobFromStream(Col: integer; Stream: TStream); override;
    /// return a Column as a variant
    // - this implementation will retrieve the data with no temporary variable
    // (since TQuery calls this method a lot, we tried to optimize it)
    // - a ftUtf8 content will be mapped into a generic WideString variant
    // for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
    // since Delphi 2009: you may not loose any data during charset conversion
    // - a ftBlob content will be mapped into a TBlobData AnsiString variant
    function ColumnToVariant(Col: integer; var Value: Variant): TSqlDBFieldType; override;
    /// return a Column as a TSqlVar value, first Col is 0
    // - the specified Temp variable will be used for temporary storage of
    // ftUtf8/ftBlob values
    // - this implementation will retrieve the data with no temporary variable,
    // and handling ftCurrency/NUMBER(22,0) as fast as possible, directly from
    // the memory buffers returned by OCI: it will ensure best performance
    // possible when called from TOrmVirtualTableCursorExternal.Column method
    // as defined in mormot.orm.sql unit (i.e. mORMot external DB access)
    procedure ColumnToSqlVar(Col: integer; var Value: TSqlVar;
      var Temp: RawByteString); override;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - fast overridden implementation with no temporary variable (about 20%
    // faster when run over high number of data rows)
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure ColumnsToJson(WR: TResultsWriter); override;
    /// return a special CURSOR Column content as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but Oracle usually use
    // such structures to get data from strored procedures
    // - such columns are mapped as ftUtf8, with the rows converted to JSON
    // - this overridden method will allow direct access to the data rows
    function ColumnCursor(Col: integer): ISqlDBRows; override;

    /// bind a special CURSOR parameter to be returned as a mormot.db.sql result set
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - such parameters are mapped as ftUnknown, and is always of paramOut type
    // - use BoundCursor() method to retrieve the corresponding ISqlDBRows after
    // execution of the statement
    // - this overridden method will prepare direct access to the data rows
    procedure BindCursor(Param: integer); override;
    /// return a special CURSOR parameter content as a mormot.db.sql result set
    // - this method is not about a column, but a parameter defined with
    // BindCursor() before method execution
    // - Cursors are not handled internally by mORMot, but some databases (e.g.
    // Oracle) usually use such structures to get data from strored procedures
    // - this method allow direct access to the data rows after execution
    // - this overridden method will allow direct access to the data rows
    function BoundCursor(Param: integer): ISqlDBRows; override;

    /// returns the number of rows updated by the execution of this statement
    function UpdateCount: integer; override;
  end;



implementation

uses
  mormot.db.raw.oracle; // defines raw OCI library API


{ ************ TSqlDBOracleConnection* and TSqlDBOracleStatement Classes }

{ TSqlDBOracleConnectionProperties }

class function TSqlDBOracleConnectionProperties.ExtractTnsName(
  const aServerName: RawUtf8): RawUtf8;
var
  i: integer;
begin
  i := PosExChar('/', aServerName);
  if i = 0 then
    result := aServerName
  else
    result := copy(aServerName, i + 1, 100);
end;

function TSqlDBOracleConnectionProperties.IsCachable(P: PUtf8Char): boolean;
begin
  result := false; // no client-side cache, only server-side
end;

constructor TSqlDBOracleConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  fDbms := dOracle;
  fBatchSendingAbilities := [cCreate, cUpdate, cDelete]; // array DML feature
  fBatchMaxSentAtOnce := 10000;  // iters <= 32767 for better performance
  inherited Create(aServerName, '', aUserID, aPassWord);
  if OCI = nil then
    OracleLibraryInitialize;
  fBlobPrefetchSize := 4096;
  fRowsPrefetchSize := 128 * 1024;
  fStatementCacheSize := 30; // default is 20
  fInternalBufferSize := 128 * 1024; // 128 KB
  fEnvironmentInitializationMode := OCI_EVENTS or OCI_THREADED or OCI_OBJECT;
end;

function TSqlDBOracleConnectionProperties.GetClientVersion: RawUtf8;
begin
  result := OCI.ClientRevision;
end;

procedure TSqlDBOracleConnectionProperties.GetForeignKeys;
begin
  with Execute('select b.owner||''.''||b.table_name||''.''||b.column_name col,' +
    '  c.owner||''.''||c.table_name||''.''||c.column_name ref' +
    '  from all_cons_columns b, all_cons_columns c, all_constraints a' +
    ' where b.constraint_name=a.constraint_name and a.owner=b.owner ' +
    'and b.position=c.position and c.constraint_name=a.r_constraint_name ' +
    'and c.owner=a.r_owner and a.constraint_type = ''R''', []) do
    while Step do
      fForeignKeys.Add(ColumnUtf8(0), ColumnUtf8(1));
end;

function TSqlDBOracleConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBOracleConnection.Create(self);
  TSqlDBOracleConnection(result).InternalProcess(speCreated);
end;

procedure TSqlDBOracleConnectionProperties.PasswordChanged(
  const ANewPassword: RawUtf8);
begin
  SynDBLog.Add.Log(sllDB, 'PasswordChanged method called', self);
  fPassWord := ANewPassword;
  if Assigned(FOnPasswordChanged) then
    FOnPasswordChanged(Self);
end;

function TSqlDBOracleConnectionProperties.SqlLimitClause(
  AStmt: TSelectStatement): TSqlDBDefinitionLimitClause;
begin
  if AStmt.OrderByField <> nil then
  begin
    result.Position := posOuter;
    result.InsertFmt := 'select * from (%) where rownum<=%';
  end
  else
    result := inherited SqlLimitClause(AStmt);
end;


{ TSqlDBOracleConnection }

procedure TSqlDBOracleConnection.Commit;
begin
  inherited Commit;
  if fTrans = nil then
    raise ESqlDBOracle.CreateUtf8('Invalid %.Commit call', [self]);
  try
    OCI.Check(self, nil,
      OCI.TransCommit(fContext, fError, OCI_DEFAULT),
      fError);
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBOracleConnection.Connect;
var
  log: ISynLog;
  Props: TSqlDBOracleConnectionProperties;
  mode: ub4;
  msg: RawUtf8;
  r: sword;
const
  type_owner_name: RawUtf8 = 'SYS';
  type_NymberListName: RawUtf8 = 'ODCINUMBERLIST';
  type_Varchar2ListName: RawUtf8 = 'ODCIVARCHAR2LIST';
  type_Credential: array[boolean] of integer = (
    OCI_CRED_RDBMS, OCI_CRED_EXT);
begin
  log := SynDBLog.Enter(self, 'Connect');
  Disconnect; // force fTrans=fError=fServer=fContext=nil
  Props := Properties as TSqlDBOracleConnectionProperties;
  with OCI do
  try
    if fEnv = nil then
    begin
      // will use UTF-8 encoding by default, in a multi-threaded context
      // OCI_EVENTS is needed to support Oracle RAC Connection Load Balancing
      r := EnvNlsCreate(fEnv, Props.EnvironmentInitializationMode, nil, nil, nil,
        nil, 0, nil, OCI_CHARSET_UTF8, OCI_CHARSET_UTF8);
      if r <> OCI_SUCCESS then
        raise ESqlDBOracle.CreateUtf8('OCIEnvNlsCreate fails with code %', [r]);
    end;
    HandleAlloc(fEnv, fError, OCI_HTYPE_ERROR);
    HandleAlloc(fEnv, fServer, OCI_HTYPE_SERVER);
    HandleAlloc(fEnv, fContext, OCI_HTYPE_SVCCTX);
    Check(self, nil,
      ServerAttach(fServer, fError,
        pointer(Props.ServerName), length(Props.ServerName), 0),
      fError);
    // we don't catch all errors here, since Client may ignore unhandled ATTR
    AttrSet(fContext, OCI_HTYPE_SVCCTX, fServer, 0, OCI_ATTR_SERVER, fError);
    HandleAlloc(fEnv, fSession, OCI_HTYPE_SESSION);
    AttrSet(fSession, OCI_HTYPE_SESSION, pointer(Props.UserID), length(Props.UserID),
      OCI_ATTR_USERNAME, fError);
    AttrSet(fSession, OCI_HTYPE_SESSION, pointer(Props.Password), length(Props.Password),
      OCI_ATTR_PASSWORD, fError);
    AttrSet(fSession, OCI_HTYPE_SESSION, @Props.fBlobPrefetchSize, 0,
      OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE, fError);
    AttrSet(fContext, OCI_HTYPE_SVCCTX, fSession, 0, OCI_ATTR_SESSION, fError);
    HandleAlloc(fEnv, fTrans, OCI_HTYPE_TRANS);
    AttrSet(fContext, OCI_HTYPE_SVCCTX, fTrans, 0, OCI_ATTR_TRANS, fError);
    if Props.UseCache then
    begin
      AttrSet(fContext, OCI_HTYPE_SVCCTX, @Props.fStatementCacheSize, 0,
        OCI_ATTR_STMTCACHESIZE, fError);
      mode := OCI_STMT_CACHE;
    end
    else
      mode := OCI_DEFAULT;
    if Props.UserID = 'SYS' then
      mode := mode or OCI_SYSDBA;
    CheckSession(self, nil,
      SessionBegin(fContext, fError, fSession,
        type_Credential[Props.UseWallet], mode),
      fError);
    Check(self, nil,
      TypeByName(fEnv, fError, fContext,
        Pointer(type_owner_name), length(type_owner_name),
        Pointer(type_NymberListName), length(type_NymberListName),
        nil, 0, OCI_DURATION_SESSION, OCI_TYPEGET_HEADER, fType_numList),
      fError);
    Check(self, nil,
      TypeByName(fEnv, fError, fContext,
        Pointer(type_owner_name), length(type_owner_name),
        Pointer(type_Varchar2ListName), length(type_Varchar2ListName),
        nil, 0, OCI_DURATION_SESSION, OCI_TYPEGET_HEADER, fType_strList),
      fError);
    if fOCICharSet = 0 then
    begin
      // retrieve the charset to be used for inlined CHAR / VARCHAR2 fields
      with NewStatement do
      try
        try
          Execute(
            'SELECT NLS_CHARSET_ID(PROPERTY_VALUE) FROM DATABASE_PROPERTIES' +
            ' WHERE PROPERTY_NAME=''NLS_CHARACTERSET''', true);
          if Step then
            fOCICharSet := ColumnInt(0)
          else
            fOCICharSet := CodePageToCharSetID(fEnv, 0); // from NLS_LANG
        except
          // on error, retrieve default from NLS_LANG
          fOCICharSet := CodePageToCharSetID(fEnv, 0);
        end;
      finally
        Free;
      end;
      fAnsiConvert := TSynAnsiConvert.Engine(CharSetIDToCodePage(fOCICharSet));
    end;
    if Props.UseWallet then
      msg := 'using Oracle Wallet'
    else
      msg := 'as ' + Props.UserID;
    if log <> nil then
      log.log(sllInfo, 'Connected to % % with %, codepage % (%/%)', [Props.ServerName,
        msg, Props.ClientVersion, fAnsiConvert.CodePage, fOCICharSet,
        OracleCharSetName(fOCICharSet)], self);
    with NewStatement do
    try
      // ORM will send date/time as ISO-8601 text -> force encoding
      Execute('ALTER SESSION SET NLS_DATE_FORMAT=''YYYY-MM-DD-HH24:MI:SS''', false);
    finally
      Free;
    end;
    with NewStatement do
    try
      // currency content is returned as SQLT_STR -> force '.' decimal separator
      Execute('alter session set NLS_NUMERIC_CHARACTERS = ". "', false);
    finally
      Free;
    end;
    //Check(TransStart(fContext,fError,0,OCI_DEFAULT),fError);
    inherited Connect; // notify any re-connection
  except
    on E: Exception do
    begin
      if log <> nil then
        log.log(sllError, E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

constructor TSqlDBOracleConnection.Create(aProperties: TSqlDBConnectionProperties);
var
  {%H-}log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'Create');
  if not aProperties.InheritsFrom(TSqlDBOracleConnectionProperties) then
    raise ESqlDBOracle.CreateUtf8('Invalid %.Create(%)', [self, aProperties]);
  inherited Create(aProperties);
end;

destructor TSqlDBOracleConnection.Destroy;
begin
  inherited Destroy;
  if (OCI <> nil) and
     (fEnv <> nil) then
    OCI.HandleFree(fEnv, OCI_HTYPE_ENV);
end;

procedure TSqlDBOracleConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if (fError <> nil) and
       (OCI <> nil) then
      with SynDBLog.Enter(self, 'Disconnect'), OCI do
      begin
        if fTrans <> nil then
        begin
        // close any opened session
          HandleFree(fTrans, OCI_HTYPE_TRANS);
          fTrans := nil;
          Check(self, nil,
            SessionEnd(fContext, fError, fSession, OCI_DEFAULT),
            fError, false, sllError);
          Check(self, nil,
            ServerDetach(fServer, fError, OCI_DEFAULT),
            fError, false, sllError);
        end;
        HandleFree(fSession, OCI_HTYPE_SESSION);
        HandleFree(fContext, OCI_HTYPE_SVCCTX);
        HandleFree(fServer, OCI_HTYPE_SERVER);
        HandleFree(fError, OCI_HTYPE_ERROR);
        fSession := nil;
        fContext := nil;
        fServer := nil;
        fError := nil;
      end;
  end;
end;

function TSqlDBOracleConnection.IsConnected: boolean;
begin
  result := fTrans <> nil;
end;

function TSqlDBOracleConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBOracleStatement.Create(self);
  if fProperties.UseCache then // client-side cache is disabled in this unit
    TSqlDBOracleStatement(result).fUseServerSideStatementCache := true;
end;

function TSqlDBOracleConnection.PasswordChange: boolean;
var
  password: RawUtf8;
begin
  result := False;
  if Properties is TSqlDBOracleConnectionProperties then
    if Assigned(TSqlDBOracleConnectionProperties(Properties).OnPasswordExpired) then
    begin
      password := Properties.PassWord;
      if TSqlDBOracleConnectionProperties(Properties).OnPasswordExpired(Self,
        password) then
        OCI.Check(Self, nil,
          OCI.PasswordChange(fContext, fError,
            pointer(Properties.UserID), Length(Properties.UserID),
            Pointer(Properties.PassWord), Length(Properties.PassWord),
            Pointer(password), Length(password),
            OCI_DEFAULT or OCI_AUTH),
          fError);
      TSqlDBOracleConnectionProperties(Properties).PasswordChanged(password);
      result := True;
    end;
end;

procedure TSqlDBOracleConnection.Rollback;
begin
  inherited;
  if fTrans = nil then
    raise ESqlDBOracle.CreateUtf8('Invalid %.RollBack call', [self]);
  OCI.Check(self, nil,
    OCI.TransRollback(fContext, fError, OCI_DEFAULT),
    fError);
end;

procedure TSqlDBOracleConnection.StartTransaction;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'StartTransaction');
  if TransactionCount > 0 then
    raise ESqlDBOracle.CreateUtf8('Invalid %.StartTransaction: nested ' +
      'transactions are not supported by the Oracle driver', [self]);
  try
    inherited StartTransaction;
    if fTrans = nil then
      raise ESqlDBOracle.CreateUtf8('Invalid %.StartTransaction call', [self]);
    // Oracle creates implicit transactions, and we'll handle AutoCommit in
    // TSqlDBOracleStatement.ExecutePrepared if TransactionCount=0
    OCI.Check(self, nil,
      OCI.TransStart(fContext, fError, 0, OCI_DEFAULT),
      fError);
  except
    on E: Exception do
    begin
      if (Properties as TSqlDBOracleConnectionProperties).
          IgnoreORA01453OnStartTransaction and
         (Pos('ORA-01453', E.Message) > 0) then
      begin
        if log <> nil then
          log.Log(sllWarning, 'It seems that we use DBLink, and Oracle ' +
            'implicitly started transaction. ORA-01453 ignored');
      end
      else
      begin
        if fTransactionCount > 0 then
          dec(fTransactionCount);
        raise;
      end;
    end;
  end;
end;

procedure TSqlDBOracleConnection.OCISTRToUtf8(P: PAnsiChar; var result: RawUtf8;
  ColumnDBCharSet, ColumnDBForm: cardinal);
var
  L: integer;
begin
  L := StrLen(P);
  if (L = 0) or
     (ColumnDBCharSet = OCI_AL32UTF8) or
     (ColumnDBCharSet = OCI_UTF8) or
     (ColumnDBForm = SQLCS_NCHAR) then
    FastSetString(result, P, L)
  else
    result := fAnsiConvert.AnsiBufferToRawUtf8(P, L);
end;

{$ifndef UNICODE}
procedure TSqlDBOracleConnection.OCISTRToAnsiString(P: PAnsiChar;
  var result: AnsiString; ColumnDBCharSet, ColumnDBForm: cardinal);
var
  L: integer;
begin
  L := StrLen(P);
  if (L = 0) or
     ((ColumnDBCharSet <> OCI_AL32UTF8) and
      (ColumnDBCharSet <> OCI_UTF8) and
      (ColumnDBForm <> SQLCS_NCHAR) and
      (fAnsiConvert.CodePage = CurrentAnsiConvert.CodePage)) then
    SetString(result, P, L)
  else
    result := CurrentAnsiConvert.AnsiToAnsi(fAnsiConvert, P, L);
end;
{$endif UNICODE}


{ TSqlDBOracleStatement }

function TSqlDBOracleStatement.ColumnBlob(Col: integer): RawByteString;
var
  C: PSqlDBColumnProperty;
  V: PPOCIDescriptor;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := ''
  else if C^.ColumnType = ftBlob then
    if C^.ColumnValueInlined then
      FastSetRawByteString(result, V, C^.ColumnValueDBSize)
    else        // conversion from POCILobLocator
      with TSqlDBOracleConnection(Connection) do
        OCI.BlobFromDescriptor(self, fContext, fError, V^, result)
  else      // need conversion to destination type
    ColumnToTypedValue(Col, ftBlob, result);
end;

function TSqlDBOracleStatement.ColumnBlobBytes(Col: integer): TBytes;
var
  C: PSqlDBColumnProperty;
  V: PPOCIDescriptor;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := nil
  else if C^.ColumnType = ftBlob then
    if C^.ColumnValueInlined then
    begin
      SetLength(result, C^.ColumnValueDBSize);
      MoveFast(V^, pointer(result)^, C^.ColumnValueDBSize);
    end
    else        // conversion from POCILobLocator
      with TSqlDBOracleConnection(Connection) do
        OCI.BlobFromDescriptor(self, fContext, fError, V^, result)
  else      // need conversion to destination type
    result := inherited ColumnBlobBytes(Col);
end;

procedure TSqlDBOracleStatement.ColumnBlobToStream(Col: integer; Stream: TStream);
var
  C: PSqlDBColumnProperty;
  V: PPOCIDescriptor;
begin
  V := GetCol(Col, C);
  if V <> nil then // column is NULL
    if C^.ColumnType = ftBlob then
      if C^.ColumnValueInlined then
        Stream.WriteBuffer(V^, C^.ColumnValueDBSize)
      else        // conversion from POCILobLocator
        with TSqlDBOracleConnection(Connection) do
          OCI.BlobFromDescriptorToStream(self, fContext, fError, V^, Stream);
end;

procedure TSqlDBOracleStatement.ColumnBlobFromStream(Col: integer; Stream: TStream);
var
  C: PSqlDBColumnProperty;
  V: PPOCIDescriptor;
begin
  V := GetCol(Col, C);
  if V <> nil then
  begin
    // V=nil means column is NULL
    if C^.ColumnType = ftBlob then
      if C^.ColumnValueInlined then
        raise ESqlDBOracle.CreateUtf8(
          '%.ColumnBlobFromStream(ColumnValueInlined) not supported', [self])
      else        // conversion from POCILobLocator
        with TSqlDBOracleConnection(Connection) do
          OCI.BlobToDescriptorFromStream(self, fContext, fError, V^, Stream);
  end
  else
    raise ESqlDBOracle.CreateUtf8('Unexpected %.ColumnBlobFromStream(null): ' +
      'use EMPTY_BLOB() to initialize it', [self]);
end;

function TSqlDBOracleStatement.ColumnCurrency(Col: integer): currency;
var
  C: PSqlDBColumnProperty;
  V: PUtf8Char;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := 0
  else if C^.ColumnType = ftCurrency then  // encoded as SQLT_STR
    PInt64(@result)^ := StrToCurr64(V)
  else
    ColumnToTypedValue(Col, ftCurrency, result);
end;

function TSqlDBOracleStatement.ColumnDateTime(Col: integer): TDateTime;
var
  C: PSqlDBColumnProperty;
  V: POracleDate;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := 0
  else if C^.ColumnType = ftDate then
    if C^.ColumnValueDBType = SQLT_DAT then
      // types match -> fast direct retrieval
      result := V^.ToDateTime
    else
      // convert from SQLT_INTERVAL_YM/SQLT_INTERVAL_DS text
      IntervalTextToDateTimeVar(pointer(V), result)
  else
    // need conversion to destination type
    ColumnToTypedValue(Col, ftDate, result);
end;

function TSqlDBOracleStatement.ColumnDouble(Col: integer): double;
var
  C: PSqlDBColumnProperty;
  V: pointer;
  curr: currency;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := 0
  else
    case C^.ColumnType of // optimized for ToDataSet() in SynDBVCL.pas
      ftDouble:
        result := unaligned(PDouble(V)^);
      ftInt64:
        result := PInt64(V)^;
      ftCurrency:
        begin
          PInt64(@curr)^ := StrToCurr64(V); // handle '.5' - not GetExtended()
          result := curr;
        end;
    else
      // need conversion to destination type
      ColumnToTypedValue(Col, ftDouble, result);
    end;
end;

function TSqlDBOracleStatement.ColumnInt(Col: integer): Int64;
var
  C: PSqlDBColumnProperty;
  V: pointer;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := 0
  else
    case C^.ColumnType of
      ftInt64:
        if C^.ColumnValueDBType = SQLT_INT then
          result := PInt64(V)^
        else
          SetInt64(V, result{%H-});
      ftCurrency:
        SetInt64(V, result); // encoded as SQLT_STR
    else
      ColumnToTypedValue(Col, ftInt64, result);
    end;
end;

function TSqlDBOracleStatement.ColumnNull(Col: integer): boolean;
var
  C: PSqlDBColumnProperty;
begin
  result := GetCol(Col, C) = nil;
end;

procedure TSqlDBOracleStatement.ColumnsToJson(WR: TResultsWriter);
var
  V: pointer;
  col, indicator: integer;
  tmp: array[0..31] of AnsiChar;
  U: RawUtf8;
begin
  // dedicated version to avoid as much memory allocation than possible
  if (not Assigned(fStatement)) or
     (CurrentRow <= 0) then
    raise ESqlDBOracle.CreateUtf8('%.ColumnsToJson() with no prior Step', [self]);
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do // fast direct conversion from OleDB buffer
    with fColumns[col] do
    begin
      if WR.Expand then
        WR.AddFieldName(ColumnName); // add '"ColumnName":'
      indicator := PSmallIntArray(fRowBuffer)[cardinal(col) * fRowCount +
        fRowFetchedCurrent];
      if (indicator = -1) or
         (ColumnType = ftNull) then // ftNull for SQLT_RSET
        WR.AddNull
      else
      begin
        if indicator <> 0 then
          LogTruncatedColumn(self, fColumns[col]);
        V := @fRowBuffer[ColumnAttr + fRowFetchedCurrent * ColumnValueDBSize];
        case ColumnType of
          ftInt64:
            if ColumnValueDBType = SQLT_INT then
              WR.Add(PInt64(V)^)
            else
              WR.AddNoJsonEscape(V); // already as SQLT_STR
          ftDouble:
            WR.AddDouble(unaligned(PDouble(V)^));
          ftCurrency:
            WR.AddFloatStr(V); // already as SQLT_STR
          ftDate:
            if ColumnValueDBType = SQLT_DAT then
              WR.AddNoJsonEscape(@tmp, POracleDate(V)^.ToIso8601(tmp{%H-}))
            else
            begin
              WR.Add('"');  // SQLT_INTERVAL_YM/SQLT_INTERVAL_DS
              WR.AddDateTime(IntervalTextToDateTime(V));
              WR.Add('"');
            end;
          ftUtf8:
            begin
              WR.Add('"');
              with TSqlDBOracleConnection(Connection) do
                if ColumnValueInlined then
                  OCISTRToUtf8(V, U, ColumnValueDBCharSet, ColumnValueDBForm)
                else
                  OCI.ClobFromDescriptor(self, fContext, fError,
                    PPOCIDescriptor(V)^, ColumnValueDBForm, U, false);
              WR.AddJsonEscape(pointer(U));
              WR.Add('"');
            end;
          ftBlob:
            if fForceBlobAsNull then
              WR.AddNull
            else if ColumnValueInlined then
              WR.WrBase64(V, ColumnValueDBSize, true)
            else
            begin
              with TSqlDBOracleConnection(Connection) do
                OCI.BlobFromDescriptor(self, fContext, fError,
                  PPOCIDescriptor(V)^, RawByteString(U));
              WR.WrBase64(Pointer(U), length(U), true);
            end;
        else
          assert(false);
        end;
      end;
      WR.AddComma;
    end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

procedure TSqlDBOracleStatement.ColumnToSqlVar(Col: integer; var Value: TSqlVar;
  var Temp: RawByteString);
var
  C: PSqlDBColumnProperty;
  V: pointer;
  NoDecimal: boolean;
begin
  // dedicated version to avoid as much memory allocation than possible
  Value.Options := [];
  V := GetCol(Col, C);
  if V = nil then
    Value.VType := ftNull
  else
    Value.VType := C^.ColumnType;
  case Value.VType of
    ftNull:
      ; // do nothing
    ftInt64:
      if C^.ColumnValueDBType = SQLT_INT then
        Value.VInt64 := PInt64(V)^
      else
        SetInt64(V, Value.VInt64);  // encoded as SQLT_STR
    ftCurrency:
      begin
        Value.VInt64 := StrToCurr64(V, @NoDecimal); // encoded as SQLT_STR
        if NoDecimal then
          Value.VType := ftInt64; // encoded e.g. from SQLT_NUM as NUMBER(22,0)
      end;
    ftDouble:
      Value.VInt64 := PInt64(V)^; // copy 64 bit content
    ftDate:
      if C^.ColumnValueDBType = SQLT_DAT then // types match -> fast direct retrieval
        Value.VDateTime := POracleDate(V)^.ToDateTime
      else
        Value.VDateTime := IntervalTextToDateTime(V);
    ftUtf8:
      begin
        with TSqlDBOracleConnection(Connection) do
          if C^.ColumnValueInlined then
            OCISTRToUtf8(V, RawUtf8(Temp), C^.ColumnValueDBCharSet, C^.ColumnValueDBForm)
          else
            OCI.ClobFromDescriptor(self, fContext, fError, PPOCIDescriptor(V)^,
              C^.ColumnValueDBForm, RawUtf8(Temp), false);
        Value.VText := pointer(Temp);
      end;
    ftBlob:
      if fForceBlobAsNull then
      begin
        Value.VBlob := nil;
        Value.VBlobLen := 0;
        Value.VType := ftNull;
      end
      else
      begin
        if C^.ColumnValueInlined then
          FastSetRawByteString(Temp, V, C^.ColumnValueDBSize)
        else
          with TSqlDBOracleConnection(Connection) do
            OCI.BlobFromDescriptor(self, fContext, fError,
              PPOCIDescriptor(V)^, Temp);
        Value.VBlob := pointer(Temp);
        Value.VBlobLen := length(Temp);
      end;
  else
    raise ESqlDBOracle.CreateUtf8('%.ColumnToSqlVar: unexpected VType=%',
      [self, ord(Value.VType)]);
  end;
end;

function TSqlDBOracleStatement.ColumnToVariant(Col: integer;
  var Value: Variant): TSqlDBFieldType;
var
  C: PSqlDBColumnProperty;
  V: pointer;
  tmp: RawUtf8;
  NoDecimal: boolean;
begin
  // dedicated version to avoid as much memory allocation than possible
  V := GetCol(Col, C);
  if V = nil then
    result := ftNull
  else
    result := C^.ColumnType;
  VarClear(Value);
  with TVarData(Value) do
  begin
    VType := MAP_FIELDTYPE2VARTYPE[result];
    case result of
      ftNull:
        ; // do nothing
      ftInt64:
        if C^.ColumnValueDBType = SQLT_INT then
          VInt64 := PInt64(V)^
        else
          SetInt64(V, VInt64);  // encoded as SQLT_STR
      ftCurrency:
        begin
          VInt64 := StrToCurr64(V, @NoDecimal); // encoded as SQLT_STR
          if NoDecimal then
          begin
            VType := varInt64; // encoded e.g. from SQLT_NUM as NUMBER(22,0)
            result := ftInt64;
          end;
        end;
      ftDouble:
        VInt64 := PInt64(V)^; // copy 64 bit content
      ftDate:
        if C^.ColumnValueDBType = SQLT_DAT then
          VDate := POracleDate(V)^.ToDateTime
        else // direct retrieval
          IntervalTextToDateTimeVar(V, VDate); // from SQLT_INTERVAL_* text
      ftUtf8:
        begin
          // see TSqlDBStatement.ColumnToVariant() for reference
          VAny := nil;
          with TSqlDBOracleConnection(Connection) do
            if C^.ColumnValueInlined then
              {$ifndef UNICODE}
              if not Connection.Properties.VariantStringAsWideString then
              begin
                VType := varString;
                OCISTRToAnsiString(V, AnsiString(VAny),
                  C^.ColumnValueDBCharSet, C^.ColumnValueDBForm);
                exit;
              end
              else
              {$endif UNICODE}
                OCISTRToUtf8(V, tmp, C^.ColumnValueDBCharSet, C^.ColumnValueDBForm)
            else
              OCI.ClobFromDescriptor(self, fContext, fError,
                PPOCIDescriptor(V)^, C^.ColumnValueDBForm, tmp);
        {$ifndef UNICODE}
          if not Connection.Properties.VariantStringAsWideString then
          begin
            VType := varString;
            AnsiString(VAny) := CurrentAnsiConvert.Utf8ToAnsi(tmp);
          end
          else
        {$endif UNICODE}
            Utf8ToSynUnicode(tmp, SynUnicode(VAny));
        end;
      ftBlob:
        begin
          VAny := nil;
          if C^.ColumnValueInlined then
            FastSetRawByteString(RawByteString(VAny), V, C^.ColumnValueDBSize)
          else
            with TSqlDBOracleConnection(Connection) do
              OCI.BlobFromDescriptor(self, fContext, fError,
                PPOCIDescriptor(V)^, RawByteString(VAny));
        end;
    else
      raise ESqlDBOracle.CreateUtf8('%.ColumnToVariant: unexpected % type',
        [self, ord(result)]);
    end;
  end;
end;

function TSqlDBOracleStatement.ColumnUtf8(Col: integer): RawUtf8;
var
  C: PSqlDBColumnProperty;
  V: PAnsiChar;
begin
  V := GetCol(Col, C);
  if V = nil then // column is NULL
    result := ''
  else if C^.ColumnType = ftUtf8 then
    with TSqlDBOracleConnection(Connection) do
      if C^.ColumnValueInlined then
        // conversion from SQLT_STR (null-terminated string)
        OCISTRToUtf8(V, result, C^.ColumnValueDBCharSet, C^.ColumnValueDBForm)
      else        // conversion from POCILobLocator
        OCI.ClobFromDescriptor(self, fContext, fError, PPOCIDescriptor(V)^,
          C^.ColumnValueDBForm, result)
  else
    // need conversion to destination type
    ColumnToTypedValue(Col, ftUtf8, result);
end;

function TSqlDBOracleStatement.ColumnCursor(Col: integer): ISqlDBRows;
var
  C: PSqlDBColumnProperty;
  V: PAnsiChar;
begin
  result := nil;
  V := GetCol(Col, C);
  if V <> nil then // column is NULL
    if C^.ColumnValueDBType = SQLT_RSET then
    begin
      result := TSqlDBOracleStatement.CreateFromExistingStatement(Connection,
        PPointer(V)^);
      PPointer(V)^ := nil; // caller will release the POCIStmt instance with its ISqlDBRows
    end
    else
      result := inherited ColumnCursor(Col); // will raise an exception
end;

procedure TSqlDBOracleStatement.BindCursor(Param: integer);
begin
  CheckParam(Param, ftUnknown, paramOut); // ftUnknown+paramOut indicate SQLT_RSET
end;

function TSqlDBOracleStatement.BoundCursor(Param: integer): ISqlDBRows;
begin
  dec(Param);
  if (cardinal(Param) >= cardinal(length(fBoundCursor))) or
     (fBoundCursor[Param] = nil) then
    raise ESqlDBOracle.CreateUtf8('%.BoundCursor: no BindCursor() on Param #%',
      [self, Param + 1]);
  result := TSqlDBOracleStatement.CreateFromExistingStatement(
    Connection, fBoundCursor[Param]);
  fBoundCursor[Param] := nil;
end;

constructor TSqlDBOracleStatement.Create(aConnection: TSqlDBConnection);
begin
  if not aConnection.InheritsFrom(TSqlDBOracleConnection) then
    raise ESqlDBOracle.CreateUtf8('Invalid %.Create(%) call',
      [self, aConnection]);
  inherited Create(aConnection);
  fInternalBufferSize :=
    TSqlDBOracleConnectionProperties(aConnection.Properties).InternalBufferSize;
  if fInternalBufferSize < 16384 then // default is 128 KB
    fInternalBufferSize := 16384; // minimal value
end;

destructor TSqlDBOracleStatement.Destroy;
begin
  try
    fTimeElapsed.Resume;
    FreeHandles(false);
    {$ifndef SYNDB_SILENCE}
    SynDBLog.Add.Log(sllDB, 'Destroy: stats = % row(s) in %',
      [TotalRowsRetrieved, fTimeElapsed.Stop], self);
    {$endif}
  finally
    inherited;
  end;
end;

constructor TSqlDBOracleStatement.CreateFromExistingStatement(aConnection:
  TSqlDBConnection; aStatement: pointer);
begin
  Create(aConnection);
  fTimeElapsed.Resume;
  try
    fStatement := aStatement;
    try
      fExpectResults := true;
      SetColumnsForPreparedStatement;
      FetchRows;
      if fRowFetched = 0 then
        fCurrentRow := -1
      else // no data row available
        fCurrentRow := 0; // mark cursor on the first row
    except
      on E: Exception do
      begin
        fStatement := nil; // do not release the statement in constructor
        FreeHandles(True);
        raise;
      end;
    end;
  finally
    fTimeElapsed.Pause;
  end;
end;

procedure TSqlDBOracleStatement.FetchRows;
var
  status: integer;
begin
  fRowFetched := 0;
  status := OCI.StmtFetch(
    fStatement, fError, fRowCount, OCI_FETCH_NEXT, OCI_DEFAULT);
  case status of
    OCI_SUCCESS:
      fRowFetched := fRowCount; // all rows successfully retrieved
    OCI_NO_DATA:
      begin
        OCI.AttrGet(fStatement, OCI_HTYPE_STMT, @fRowFetched, nil,
          OCI_ATTR_ROWS_FETCHED, fError);
        fRowFetchedEnded := true;
      end;
  else
    OCI.Check(nil, self, status, fError); // will raise error
  end;
  fRowFetchedCurrent := 0;
end;

procedure UnQuoteSqlString(S, D: PUtf8Char; SLen: integer);
begin
  // internal method, tuned for our OCI process
  if S = nil then
    D^ := #0
  else if S^ <> '''' then
    MoveFast(S^, D^, SLen + 1) // +1 to include #0
  else
  begin
    inc(S);
    repeat
      if S[0] = '''' then
        if S[1] = '''' then
          inc(S)
        else
          break;
      D^ := S^;
      inc(S);
      inc(D);
    until S^ = #0;
    D^ := #0; // include trailing #0
  end;
end;

const
  /// 32 MB of data sent at once sounds enough
  MAX_INLINED_PARAM_SIZE = 32 * 1024 * 1024;

procedure TSqlDBOracleStatement.ExecutePrepared;
var
  i, j: PtrInt;
  Env: POCIEnv;
  Context: POCISvcCtx;
  param: PSqlDBParam;
  Type_List: POCIType;
  oData: pointer;
  oDataDAT: ^TOracleDateArray absolute oData;
  oDataINT: ^TInt64Array absolute oData;
  oDataSTR: PUtf8Char;
  oLength: integer;
  oBind: POCIBind;
  oIndicator: array of sb2;
  aIndicator: array of array of sb2;
  oOCIDateTime: POCIDateTime;
  Status, L: integer;
  mode: cardinal;
  Int32: set of 0..127;
  ociArrays: array of POCIArray;
  ociArraysCount: byte;
  num_val: OCINumber;
  tmp: RawUtf8;
  str_val: POCIString;
  {$ifdef FPC_64}
  wasStringHacked: TByteDynArray;
  {$endif FPC_64}
label
  txt;
begin
  if fStatement = nil then
    raise ESqlDBOracle.CreateUtf8(
      '%.ExecutePrepared without previous Prepare', [self]);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  SqlLogBegin(sllSQL);
  try
    ociArraysCount := 0;
    Env := (Connection as TSqlDBOracleConnection).fEnv;
    Context := TSqlDBOracleConnection(Connection).fContext;
    Status := OCI_ERROR;
    try
      fRowFetchedEnded := false;
      // 1. bind parameters
      if fPreparedParamsCount <> fParamCount then
        raise ESqlDBOracle.CreateUtf8(
          '%.ExecutePrepared expected % bound parameters, got %',
          [self, fPreparedParamsCount, fParamCount]);
      if not fExpectResults then
        fRowCount := 1; // to avoid ORA-24333 error
      if fParamCount > 0 then
        if (fParamsArrayCount > 0) and
           not fExpectResults then
        begin
          // 1.1. Array DML binding
          SetLength(aIndicator, fParamCount);
          for i := 0 to fParamCount - 1 do
            with fParams[i] do
            begin
              if VArray = nil then
                raise ESqlDBOracle.CreateUtf8(
                  '%.ExecutePrepared: Parameter #% should be an array',
                  [self, i]);
              if VInt64 <> fParamsArrayCount then
                raise ESqlDBOracle.CreateUtf8(
                  '%.ExecutePrepared: Parameter #% expected array count %, got %',
                  [self, i, fParamsArrayCount, VInt64]);
              SetLength(aIndicator[i], fParamsArrayCount);
              VDBType := SQLT_STR;
              oLength := 23; // max size for ftInt64/ftDouble/ftCurrency
              case VType of
                ftDate:
                  begin
                    VDBType := SQLT_DAT;
                    FastSetRawByteString(
                      VData, nil, fParamsArrayCount * SizeOf(TOracleDate));
                    oData := pointer(VData);
                    oLength := SizeOf(TOracleDate);
                  end;
                ftInt64:
                  if OCI.SupportsInt64Params then
                  begin
                    // starting with 11.2, OCI supports NUMBER conversion to/from Int64
                    VDBType := SQLT_INT;
                    FastSetRawByteString(
                      VData, nil, fParamsArrayCount * SizeOf(Int64));
                    oData := pointer(VData);
                    oLength := SizeOf(Int64);
                  end;
                  // prior to 11.2, we will stay with the default SQLT_STR type
                ftUtf8:
                  oLength := 7; // minimal aligned length
                ftBlob:
                  begin
                    VDBTYPE := SQLT_LVB;
                    oLength := 7; // minimal aligned length
                  end;
              end;
              for j := 0 to fParamsArrayCount - 1 do
                if VArray[j] = 'null' then // bind null (ftUtf8 should be '"null"')
                  aIndicator[i][j] := -1
                else
                begin
                  if VDBType = SQLT_INT then
                    SetInt64(pointer(Varray[j]), oDataINT^[j])
                  else
                    case VType of
                      ftUtf8,
                      ftDate:
                        begin
                          L := length(VArray[j]) - 2; // -2 since quotes will be removed
                          if VType = ftDate then
                            if L <= 0 then
                              oDataDAT^[j].From(0)
                            else
                              oDataDAT^[j].From(PUtf8Char(pointer(VArray[j])) + 1, L)
                          else if L > oLength then
                            if L * fParamsArrayCount > MAX_INLINED_PARAM_SIZE then
                              raise ESqlDBOracle.CreateUtf8(
                                '%.ExecutePrepared: Array parameter #% STR too big',
                                [self, i + 1])
                            else
                              oLength := L;
                        end;
                      ftBlob:
                        begin
                          L := length(VArray[j]) + SizeOf(integer);
                          if L * fParamsArrayCount > MAX_INLINED_PARAM_SIZE then
                            raise ESqlDBOracle.CreateUtf8(
                              '%.ExecutePrepared: Array parameter #% BLOB too big',
                              [self, i + 1])
                          else if L > oLength then
                            oLength := L;
                        end;
                    end;
                end;
              case VDBType of
                SQLT_STR:
                  begin
                    inc(oLength); // space for trailing #0
                    FastSetRawByteString(VData, nil, oLength * fParamsArrayCount);
                    oData := Pointer(VData); // in-place quote removal in text
                    oDataSTR := oData;
                    for j := 0 to fParamsArrayCount - 1 do
                    begin
                      UnQuoteSqlString(pointer(VArray[j]), oDataSTR, length(VArray[j]));
                      inc(oDataSTR, oLength);
                    end;
                  end;
                SQLT_LVB:
                  begin
                    FastSetRawByteString(VData, nil, oLength * fParamsArrayCount);
                    oData := Pointer(VData);
                    oDataSTR := oData;
                    for j := 0 to fParamsArrayCount - 1 do
                    begin
                      {$ifdef FPC}
                      PInteger(oDataSTR)^ := length(VArray[j]);
                      MoveFast(Pointer(VArray[j])^, oDataSTR[4], length(VArray[j]));
                      {$else} // Delphi has 32-bit length, just as OCI expects
                      MoveFast(Pointer(PtrInt(VArray[j]) - 4)^, oDataSTR^,
                        length(VArray[j]) + 4);
                      {$endif FPC}
                      inc(oDataSTR, oLength);
                    end;
                  end;
              end;
              oBind := nil;
              OCI.Check(nil, self,
                OCI.BindByPos(fStatement, oBind, fError,
                  i + 1, oData, oLength, VDBType, pointer(aIndicator[i]),
                  nil, nil, 0, nil, OCI_DEFAULT),
                fError);
            end;
          fRowCount := fParamsArrayCount; // set iters count for OCI.StmtExecute()
        end
        else
        begin
          // 1.2. One row DML optimized binding
          FillcharFast(Int32, SizeOf(Int32), 0);
          SetLength(oIndicator, fParamCount);
          SetLength(ociArrays, fParamCount);
          for i := 0 to fParamCount - 1 do
            if Length(fParams[i].VArray) > 0 then
            begin
              // 1.2.1. Bind an array as one object
              param := @fParams[i];
              case param.VType of
                ftInt64:
                  Type_List := TSqlDBOracleConnection(Connection).fType_numList;
                ftUtf8:
                  Type_List := TSqlDBOracleConnection(Connection).fType_strList;
              else
                Type_List := nil;
              end;
              if Type_List = nil then
                raise ESqlDBOracle.CreateUtf8(
                  '%.ExecutePrepared: Unsupported array parameter type #%',
                  [self, i + 1]);
              ociArrays[ociArraysCount] := nil;
              OCI.Check(nil, self,
                OCI.ObjectNew(Env, fError, Context, OCI_TYPECODE_VARRAY,
                  Type_List, nil, OCI_DURATION_SESSION, True,
                  ociArrays[ociArraysCount]),
                fError);
              inc(ociArraysCount);
              FastSetRawByteString(
                param.VData, nil, Length(param.VArray) * SizeOf(Int64));
              oData := pointer(param.VData);
              for j := 0 to Length(param.VArray) - 1 do
                case param.VType of
                  ftInt64:
                    begin
                      SetInt64(pointer(param.Varray[j]), oDataINT^[j]);
                      OCI.Check(nil, self,
                        OCI.NumberFromInt(fError, @oDataINT[j],
                          SizeOf(Int64), OCI_NUMBER_SIGNED, num_val),
                        fError);
                      OCI.Check(nil, self,
                        OCI.CollAppend(Env, fError, @num_val,
                          nil, ociArrays[ociArraysCount - 1]),
                        fError);
                    end;
                  ftUtf8:
                    begin
                      str_val := nil;
                      UnQuoteSqlStringVar(pointer(param.VArray[j]), tmp);
                      OCI.Check(nil, self,
                        OCI.StringAssignText(Env, fError,
                          pointer(tmp), length(tmp), str_val),
                        fError);
                      OCI.Check(nil, self,
                        OCI.CollAppend(Env, fError, str_val,
                          nil, ociArrays[ociArraysCount - 1]),
                        fError);
                    end;
                end;
              oBind := nil;
              OCI.Check(nil, self,
                OCI.BindByPos(fStatement, oBind, fError, i + 1, nil, 0,
                  SQLT_NTY, nil, nil, nil, 0, nil, OCI_DEFAULT),
                fError);
              OCI.BindObject(oBind, fError, Type_List,
                ociArrays[ociArraysCount - 1], nil, nil, nil);
            end
            else        // 1.2.2. Bind one simple parameter value
              with fParams[i] do
              begin
                if VType = ftNull then
                begin
                  oIndicator[i] := -1; // assign a NULL to the column, ignoring input value
                  oLength := 0;
                  oData := nil;
                  VDBType := SQLT_STR;
                end
                else
                begin
                  oLength := SizeOf(Int64);
                  oData := @VInt64;
                  case VType of
                    ftUnknown:
                      begin
                        if VInOut = paramIn then
                          raise ESqlDBOracle.CreateUtf8(
                            '%.ExecutePrepared: Unexpected IN cursor parameter #%',
                            [self, i + 1]);
                        VDBType := SQLT_RSET;
                        with OCI do
                          Check(nil, self,
                            HandleAlloc(Env, PPointer(oData)^,
                              OCI_HTYPE_STMT, 0, nil),
                            fError);
                        oLength := SizeOf(pointer);
                      end;
                    ftInt64:
                      if OCI.SupportsInt64Params then
                        // starting with 11.2, OCI supports NUMBER conversion to/from Int64
                        VDBType := SQLT_INT
                      else
                      // before 11.2, we will use either SQLT_INT, SQLT_STR or SQLT_FLT
                      if VInOut = paramIn then
                        if (VInt64 > low(integer)) and
                           (VInt64 < high(integer)) then
                        begin
                          // map to 32 bit will always work
                          VDBType := SQLT_INT;
                          Include(Int32, i);
                          oLength := SizeOf(integer); // truncate to 32 bit integer value
                        end
                        else
                        begin
                          VData := Int64ToUtf8(VInt64);      // (SQLT_VNU did not work)
                          goto txt; // IN huge integers will be managed as text
                        end
                      else
                      begin
                        VDBType := SQLT_FLT; // OUT values will be converted as double
                        unaligned(PDouble(oData)^) := VInt64;
                      end;
                    ftDouble:
                      VDBType := SQLT_FLT;
                    ftCurrency:
                      if VInOut = paramIn then
                      begin
                        VData := Curr64ToStr(VInt64);
                        goto txt; // input-only currency values will be managed as text
                      end
                      else
                      begin
                        VDBType := SQLT_FLT; // OUT values will be converted as double
                        unaligned(PDouble(oData)^) := PCurrency(oData)^;
                      end;
                    ftDate:
                      if VInOut = paramIn then
                      begin
                        VDBType := SQLT_TIMESTAMP; // SQLT_DAT is wrong within WHERE clause
                        oOCIDateTime := DateTimeToDescriptor(PDateTime(@VInt64)^);
                        FastSetRawByteString(VData, @oOCIDateTime, SizeOf(oOCIDateTime));
                        oData := pointer(VData);
                        oLength := SizeOf(oOCIDateTime);
                      end
                      else
                      begin
                        VDBType := SQLT_DAT;  // will work for OUT parameters
                        POracleDate(@VInt64)^.From(PDateTime(@VInt64)^);
                      end;
                    ftUtf8:
                      begin
txt:                    VDBType := SQLT_STR; // use STR external data type (SQLT_LVC fails)
                        oLength := Length(VData) + 1; // include #0
                        if oLength = 1 then // '' will just map one #0
                          oData := @VData
                        else
                          oData := pointer(VData);
                        // for OUT param, input text shall be pre-allocated
                      end;
                    ftBlob:
                      if VInOut <> paramIn then
                        raise ESqlDBOracle.CreateUtf8(
                          '%.ExecutePrepared: Unexpected OUT blob parameter #%',
                          [self, i + 1])
                      else
                      begin
                        oLength := Length(VData);
                        if oLength < 2000 then
                        begin
                          VDBTYPE := SQLT_BIN;
                          oData := pointer(VData);
                        end
                        else
                        begin
                          VDBTYPE := SQLT_LVB; // layout: raw data prepended by int32 len
                          {$ifdef FPC_64}
                          // in case of FPC+CPU64 TSqlDBParam.VData is a RawByteString and
                          // length is stored as SizeInt = Int64 (not int32) -> patch
                          // (no patch needed for Delphi, in which len is always longint)
                          if Length(VData) > MaxInt then
                            raise ESqlDBOracle.CreateUtf8('%.ExecutePrepared: % ' +
                              'blob length exceeds max size for parameter #%',
                              [self, KB(oLength), i + 1]);
                          UniqueString(VData); // for thread-safety
                          PInteger(PtrInt(VData) - SizeOf(integer))^ := oLength;
                          if {%H-}wasStringHacked = nil then
                            SetLength(wasStringHacked, fParamCount shr 3 + 1);
                          SetBitPtr(pointer(wasStringHacked), i); // for unpatching below
                          {$endif FPC_64}
                          oData := Pointer(PtrInt(VData) - SizeOf(integer));
                          Inc(oLength, SizeOf(integer));
                        end;
                      end;
                  else
                    raise ESqlDBOracle.CreateUtf8(
                      '%.ExecutePrepared: Invalid parameter #% type=%',
                      [self, i + 1, ord(VType)]);
                  end;
                end;
                oBind := nil;
                OCI.Check(nil, self,
                  OCI.BindByPos(fStatement, oBind, fError,
                    i + 1, oData, oLength, VDBType, @oIndicator[i], nil, nil,
                    0, nil, OCI_DEFAULT),
                  fError);
              end;
        end;
      // 2. retrieve column information (if not already done)
      if fExpectResults and
         (fColumn.Count = 0) then
        // We move this after params binding to prevent "ORA-00932: inconsistent
        // datatypes" during call to StmtExecute with OCI_DESCRIBE_ONLY.
        // Because if called here sometimes it breaks the Oracle shared pool and
        // only `ALTER system flush shared_pool` seems to fix the DB state
        SetColumnsForPreparedStatement;
      // 3. execute prepared statement and dispatch data in row buffers
      if (fColumnCount = 0) and
         (Connection.TransactionCount = 0) then
        // for INSERT/UPDATE/DELETE without a transaction: AutoCommit after execution
        mode := OCI_COMMIT_ON_SUCCESS
      else        // for SELECT or inside a transaction: wait for an explicit COMMIT
        mode := OCI_DEFAULT;
      Status := OCI.StmtExecute(TSqlDBOracleConnection(Connection).fContext,
        fStatement, fError, fRowCount, 0, nil, nil, mode);
      // 4. check execution error, and retrieve data result range
      FetchTest(Status); // error + set fRowCount+fCurrentRow+fRowFetchedCurrent
      Status := OCI_SUCCESS; // mark OK for fBoundCursor[] below
    finally
      {$ifdef FPC_64}
      if wasStringHacked <> nil then // restore patched strings length ASAP
        for i := 0 to fParamCount - 1 do
          if GetBitPtr(pointer(wasStringHacked), i) then
            PInteger(PtrInt(fParams[i].VData) - SizeOf(integer))^ := 0;
      {$endif FPC_64}
      for i := 0 to ociArraysCount - 1 do
        OCI.Check(nil, self,
          OCI.ObjectFree(Env, fError, ociArrays[i], OCI_OBJECTFREE_FORCE),
          fError, false, sllError);
      // 3. release and/or retrieve OUT bound parameters
      if fParamsArrayCount > 0 then
        for i := 0 to fParamCount - 1 do
          fParams[i].VData := ''
      else
        for i := 0 to fParamCount - 1 do
          with fParams[i] do
            case VType of
              ftUnknown:
                if VInOut = paramOut then
                  if Status = OCI_SUCCESS then
                  begin
                    SetLength(fBoundCursor, fParamCount);
                    fBoundCursor[i] := PPointer(@VInt64)^; // available via BoundCursor()
                  end
                  else
                  // on error, release bound statement resource
                  if OCI.HandleFree(
                       PPointer(@VInt64)^, OCI_HTYPE_STMT) <> OCI_SUCCESS then
                    SynDBLog.Add.Log(sllError,
                      'ExecutePrepared: HandleFree(SQLT_RSET)', self);
              ftInt64:
                if VDBType = SQLT_FLT then // retrieve OUT integer parameter
                  VInt64 := trunc(unaligned(PDouble(@VInt64)^));
              ftCurrency:
                if VDBType = SQLT_FLT then // retrieve OUT currency parameter
                  PCurrency(@VInt64)^ := unaligned(PDouble(@VInt64)^);
              ftDate:
                case VDBType of
                  SQLT_DAT: // retrieve OUT date parameter
                    PDateTime(@VInt64)^ := POracleDate(@VInt64)^.ToDateTime;
                  SQLT_TIMESTAMP:
                    begin
                      // release OCIDateTime resource
                      oOCIDateTime := PPointer(VData)^;
                      if OCI.DescriptorFree(
                           oOCIDateTime, OCI_DTYPE_TIMESTAMP) <> OCI_SUCCESS then
                        SynDBLog.Add.Log(sllError,
                          'ExecutePrepared: DescriptorFree(OCI_DTYPE_TIMESTAMP)', self);
                      VData := '';
                    end;
                end;
              ftUtf8:
                if VInOut <> paramIn then
                  // retrieve OUT text parameter - is #0 terminated -> StrLen()
                  SetLength(VData, StrLen(pointer(VData)));
            end;
    end;
  finally
    fTimeElapsed.FromExternalMicroSeconds(SqlLogEnd);
  end;
end;

procedure TSqlDBOracleStatement.FetchTest(Status: integer);
begin
  fRowFetched := 0;
  case Status of
    OCI_SUCCESS,
    OCI_SUCCESS_WITH_INFO:
      begin
        if fColumnCount <> 0 then
          fRowFetched := fRowCount;
        if Status = OCI_SUCCESS_WITH_INFO then
          OCI.Check(nil, self, Status, fError, false, sllWarning);
      end;
    OCI_NO_DATA:
      begin
        assert(fColumnCount <> 0);
        OCI.AttrGet(fStatement, OCI_HTYPE_STMT, @fRowFetched, nil,
          OCI_ATTR_ROWS_FETCHED, fError);
        fRowFetchedEnded := true;
      end;
  else
    OCI.Check(nil, self, Status, fError); // will raise error
  end;
  if fRowFetched = 0 then
  begin
    fRowCount := 0;
    fCurrentRow := -1; // no data
  end
  else
  begin
    fCurrentRow := 0; // mark cursor on the first row
    fRowFetchedCurrent := 0;
  end;
end;

function TSqlDBOracleStatement.DateTimeToDescriptor(aDateTime: TDateTime): pointer;
var
  HH, MM, SS, MS, Y, M, D: word;
  env: pointer;
begin
  env := (Connection as TSqlDBOracleConnection).fEnv;
  OCI.Check(nil, self,
    OCI.DescriptorAlloc(env, result, OCI_DTYPE_TIMESTAMP, 0, nil),
    fError);
  DecodeDate(aDateTime, Y, M, D);
  if Frac(aDateTime) = 0 then
  begin
    HH := 0;
    MM := 0;
    SS := 0;
  end
  else
    DecodeTime(aDateTime, HH, MM, SS, MS);
  OCI.Check(nil, nil,
    OCI.DateTimeConstruct(env, fError, result, Y, M, D, HH, MM, SS, 0, nil, 0),
    fError);
end;

procedure TSqlDBOracleStatement.ReleaseRows;
begin
  // not implemented yet
  inherited ReleaseRows;
end;

procedure TSqlDBOracleStatement.FreeHandles(AfterError: boolean);
const // see http://gcov.php.net/PHP_5_3/lcov_html/ext/oci8/oci8_statement.c.gcov.php
  RELEASE_MODE: array[boolean] of integer = (
    OCI_DEFAULT, OCI_STMTCACHE_DELETE);
var
  i, j: integer;
  P: PPointer;
begin
  if self = nil then
    exit; // avoid GPF
  if fRowBuffer <> nil then
    for i := 0 to fColumnCount - 1 do
      with fColumns[i] do
        if not ColumnValueInlined then
        begin
          P := @fRowBuffer[ColumnAttr]; // first POCILobLocator/POCIStmt item
          for j := 1 to fRowBufferCount do
          begin
            if P^ <> nil then
            begin
              case ColumnValueDBType of
                SQLT_CLOB,
                SQLT_BLOB:
                  if OCI.DescriptorFree(P^, OCI_DTYPE_LOB) <> OCI_SUCCESS then
                    SynDBLog.Add.Log(sllError,
                      'FreeHandles: Invalid OCI_DTYPE_LOB', self);
                SQLT_RSET:
                  if OCI.HandleFree(P^, OCI_HTYPE_STMT) <> OCI_SUCCESS then
                    SynDBLog.Add.Log(sllError, 'FreeHandles: Invalid SQLT_RSET', self);
              else
                raise ESqlDBOracle.CreateUtf8(
                  '%.FreeHandles: Wrong % type for inlined column %',
                  [self, ColumnValueDBType, ColumnName]);
              end;
              P^ := nil;
            end;
            inc(P);
          end;
        end;
  if fBoundCursor <> nil then
  begin
    for i := 0 to high(fBoundCursor) do
      if fBoundCursor[i] <> nil then
        OCI.HandleFree(fBoundCursor[i], OCI_HTYPE_STMT);
    fBoundCursor := nil;
  end;
  if fStatement <> nil then
  begin
    if fUseServerSideStatementCache then
      OCI.Check(nil, self,
        OCI.StmtRelease(fStatement, fError, nil, 0, RELEASE_MODE[AfterError]),
      fError)
    else
      OCI.HandleFree(fStatement, OCI_HTYPE_STMT);
    fStatement := nil;
  end;
  if fError <> nil then
  begin
    OCI.HandleFree(fError, OCI_HTYPE_ERROR);
    fError := nil;
  end;
  if fRowBuffer <> nil then
    SetLength(fRowBuffer, 0); // release internal buffer memory
  if fColumnCount > 0 then
    fColumn.Clear;
end;

function TSqlDBOracleStatement.GetCol(Col: integer; out Column:
  PSqlDBColumnProperty): pointer;
begin
  CheckCol(Col); // check Col value  against fColumnCount
  if (not Assigned(fStatement)) or
     (fColumnCount = 0) or
     (fRowCount = 0) or
     (fRowBuffer = nil) then
    raise ESqlDBOracle.CreateUtf8('%.Column*() with no prior Execute', [self]);
  if CurrentRow <= 0 then
    raise ESqlDBOracle.CreateUtf8('%.Column*() with no prior Step', [self]);
  Column := @fColumns[Col];
  result := @fRowBuffer[Column^.ColumnAttr + fRowFetchedCurrent * Column^.ColumnValueDBSize];
  case PSmallIntArray(fRowBuffer)[cardinal(Col) * fRowCount + fRowFetchedCurrent] of
    // 0:OK, >0:untruncated length, -1:NULL, -2:truncated (length>32KB)
    -1:
      result := nil; // NULL
    0:
      exit; // OK
  else
    LogTruncatedColumn(self, Column^);
  end;
end;

function TSqlDBOracleStatement.UpdateCount: integer;
begin
  result := 0;
  if fStatement <> nil then
    OCI.AttrGet(fStatement, OCI_HTYPE_STMT, @result, nil,
      OCI_ATTR_ROW_COUNT, fError);
end;

procedure TSqlDBOracleStatement.SetColumnsForPreparedStatement;
var
  aName: RawUtf8;
  Env: POCIEnv;
  i, j: integer;
  oHandle: POCIHandle;
  oDefine: POCIDefine;
  oName: PAnsiChar;
  oNameLen, oScale, oCharSet: integer;
  ColCount, RowSize: cardinal;
  StatementType, oType, oSize: ub2;
  Prefetch: ub4;
  ColumnLongTypes: set of (hasLOB, hasLONG, hasCURS);
  PP: PPointer;
  Indicators: PAnsiChar;
begin
  Env := (Connection as TSqlDBOracleConnection).fEnv;
  with OCI do
  begin
    // 1. ensure fStatement is SELECT
    if fError = nil then
      HandleAlloc(Env, fError, OCI_HTYPE_ERROR);
    AttrGet(fStatement, OCI_HTYPE_STMT, @StatementType, nil, OCI_ATTR_STMT_TYPE, fError);
    if fExpectResults <> (StatementType = OCI_STMT_SELECT) then
      raise ESqlDBOracle.CreateUtf8(
        '%.SetColumnsForPreparedStatement called with ' +
        'ExpectResults=%, whereas StatementType=%',
         [self, ord(fExpectResults), StatementType]);
    if not fExpectResults then
    begin
      fRowCount := 1; // iters=1 by default
      exit; // no row data expected -> leave fColumnCount=0
    end;
    // 2. retrieve rows column types
    Check(nil, self,
      StmtExecute(TSqlDBOracleConnection(Connection).fContext,
        fStatement, fError, 1, 0, nil, nil, OCI_DESCRIBE_ONLY),
      fError);
    ColCount := 0;
    AttrGet(fStatement, OCI_HTYPE_STMT, @ColCount, nil, OCI_ATTR_PARAM_COUNT, fError);
    RowSize := ColCount * SizeOf(sb2); // space for indicators
    ColumnLongTypes := [];
    fColumn.Capacity := ColCount;
    for i := 1 to ColCount do
    begin
      oHandle := nil;
      ParamGet(fStatement, OCI_HTYPE_STMT, fError, oHandle, i);
      AttrGet(oHandle, OCI_DTYPE_PARAM, @oName, @oNameLen, OCI_ATTR_NAME, fError);
      if oNameLen = 0 then
        aName := 'col_' + Int32ToUtf8(i)
      else
        FastSetString(aName, oName, oNameLen);
      AttrGet(oHandle, OCI_DTYPE_PARAM, @oType, nil, OCI_ATTR_DATA_TYPE, fError);
      AttrGet(oHandle, OCI_DTYPE_PARAM, @oSize, nil, OCI_ATTR_DATA_SIZE, fError);
      with PSqlDBColumnProperty(fColumn.AddAndMakeUniqueName(aName))^ do
      begin
        ColumnValueDBSize := oSize;
        ColumnValueInlined := true;
        case oType of
          SQLT_CHR,
          SQLT_VCS,
          SQLT_AFC,
          SQLT_AVC,
          SQLT_STR,
          SQLT_VST,
          SQLT_NTY:
            begin
              ColumnType := ftUtf8;
              ColumnValueDBType := SQLT_STR; // null-terminated string
              inc(ColumnValueDBSize); // must include ending #0
            end;
          SQLT_LNG:
            begin
              ColumnValueDBSize := 32768; // will be truncated at 32 KB
              ColumnType := ftUtf8;
              ColumnValueDBType := SQLT_STR; // null-terminated string
              include(ColumnLongTypes, hasLONG);
            end;
          SQLT_LVC,
          SQLT_CLOB:
            begin
              ColumnType := ftUtf8;
              ColumnValueInlined := false;
              ColumnValueDBType := SQLT_CLOB;
              ColumnValueDBSize := SizeOf(POCILobLocator);
              include(ColumnLongTypes, hasLOB);
            end;
          SQLT_RID,
          SQLT_RDD:
            begin
              ColumnType := ftUtf8;
              ColumnValueDBType := SQLT_STR; // null-terminated string
              ColumnValueDBSize := 24; // 24 will fit 8 bytes alignment
            end;
          SQLT_VNU,
          SQLT_FLT,
          SQLT_BFLOAT,
          SQLT_BDOUBLE,
          SQLT_IBFLOAT,
          SQLT_IBDOUBLE:
            begin
              ColumnType := ftDouble;
              ColumnValueDBType := SQLT_BDOUBLE;
              ColumnValueDBSize := SizeOf(Double);
            end;
          SQLT_NUM:
            begin
              oScale := 5; // OCI_ATTR_PRECISION is always 38 (on Oracle 11g) :(
              AttrGet(oHandle, OCI_DTYPE_PARAM, @oScale, nil, OCI_ATTR_SCALE, fError);
              ColumnValueDBSize := SizeOf(Double);
              case oScale of
               {0: if (major_version>11) or
                      ((major_version=11) and
                       (minor_version>1)) then
               begin
                 // starting with 11.2, OCI supports NUMBER conversion into Int64
                 ColumnType := ftInt64;
                 ColumnValueDBType := SQLT_INT;
               end
               else
               begin
                 // we'll work out with null-terminated string
                 ColumnType := ftCurrency;
                 ColumnValueDBType := SQLT_STR;
                 ColumnValueDBSize := 24;
               end;}
               // we found out that a computed column is returned with Scale=0
               // even if it is numeric (OCI 11.2 bug) -> so SQLT_INT won't work
               // in fact, SQLT_STR will make JSON creation faster (already ASCII)
                0..4:
                  begin
                    ColumnType := ftCurrency;      // will guess type from results
                    ColumnValueDBType := SQLT_STR; // use null-terminated string
                    ColumnValueDBSize := 24;
                  end
              else
                begin
                  ColumnType := ftDouble;
                  ColumnValueDBType := SQLT_BDOUBLE;
                end;
              end;
            end;
          SQLT_INT,
          _SQLT_PLI,
          SQLT_UIN:
            begin
              ColumnType := ftInt64;
              ColumnValueDBType := SQLT_INT;
              ColumnValueDBSize := SizeOf(Int64);
            end;
          SQLT_DAT,
          SQLT_DATE,
          SQLT_TIME,
          SQLT_TIME_TZ,
          SQLT_TIMESTAMP,
          SQLT_TIMESTAMP_TZ,
          SQLT_TIMESTAMP_LTZ:
            begin
              ColumnType := ftDate;
              ColumnValueDBType := SQLT_DAT;
              ColumnValueDBSize := SizeOf(TOracleDate);
            end;
          SQLT_INTERVAL_YM,
          SQLT_INTERVAL_DS:
            begin
              ColumnType := ftDate;
              ColumnValueDBType := SQLT_STR; // null-terminated string
              ColumnValueDBSize := 24; // 24 will fit 8 bytes alignment
            end;
          SQLT_BIN:
            begin
              if fForceBlobAsNull then
                ColumnType := ftNull
              else
                ColumnType := ftBlob;
              ColumnValueDBType := SQLT_BIN;
            end;
          SQLT_LBI,
          SQLT_BLOB,
          SQLT_LVB:
            begin
              ColumnType := ftBlob;
              ColumnValueInlined := false;
              ColumnValueDBType := SQLT_BLOB;
              ColumnValueDBSize := SizeOf(POCILobLocator);
              if fForceBlobAsNull then
                ColumnType := ftNull
              else
                include(ColumnLongTypes, hasLOB);
            end;
          SQLT_RSET,
          SQLT_CUR:
            begin
              ColumnType := ftNull;
              ColumnValueInlined := false;
              ColumnValueDBType := SQLT_RSET;
              ColumnValueDBSize := SizeOf(POCIStmt);
              include(ColumnLongTypes, hasCURS);
            end;
        else
          raise ESqlDBOracle.CreateUtf8('% - Column [%]: unknown type %',
            [self, ColumnName, oType]);
        end;
        inc(RowSize, ColumnValueDBSize);
        if ColumnType = ftUtf8 then
        begin
          Check(nil, self,
            AttrGet(oHandle, OCI_DTYPE_PARAM, @ColumnValueDBForm, nil,
              OCI_ATTR_CHARSET_FORM, fError),
            fError);
          Check(nil, self,
            AttrGet(oHandle, OCI_DTYPE_PARAM, @ColumnValueDBCharSet, nil,
              OCI_ATTR_CHARSET_ID, fError),
            fError);
          case ColumnValueDBForm of
            SQLCS_IMPLICIT:
              begin
                oCharSet := TSqlDBOracleConnection(Connection).fOCICharSet;
                if ColumnValueDBCharSet = SQLCS_IMPLICIT then
                  ColumnValueDBCharSet := oCharSet
                else if (ColumnValueDBCharSet <> oCharSet) and
                        not SimilarCharSet(ColumnValueDBCharSet, oCharSet) then
                  // log a warning, but use the connection-level code page
                  SynDBLog.Add.Log(sllWarning, 'Column [%] has % (%) charset - ' +
                    'expected % (%) -> possible data loss', [ColumnName,
                    ColumnValueDBCharSet, OracleCharSetName(ColumnValueDBCharSet),
                    oCharSet, OracleCharSetName(oCharSet)], self);
              end;
            SQLCS_NCHAR:
              // NVARCHAR2 -> set max UTF-8 bytes from chars
              if ColumnValueInlined then
              begin
                inc(RowSize, ColumnValueDBSize * 2);
                ColumnValueDBSize := ColumnValueDBSize * 3;
              end;
          end;
        end;
      end;
      // avoid memory leak for cached statement
      if DescriptorFree(oHandle, OCI_DTYPE_PARAM) <> OCI_SUCCESS then
        SynDBLog.Add.Log(sllError, 'Invalid DescriptorFree(OCI_DTYPE_PARAM)', self);
    end;
    assert(fColumn.Count = integer(ColCount));
    // 3. Dispatch data in row buffer
    assert(fRowBuffer = nil);
    fRowCount := (fInternalBufferSize - ColCount shl 4) div RowSize;
    if fRowCount = 0 then
    begin
      // reserve space for at least one row of data
      fInternalBufferSize := RowSize + ColCount shl 4;
      fRowCount := 1;
    end
    else if (TSqlDBOracleConnectionProperties(Connection.Properties).
               RowsPrefetchSize > 1024) and
            (ColumnLongTypes = []) then
    begin
      // prefetching if no LOB nor LONG column(s)
      Prefetch := 0; // set prefetch by Memory, not by row count
      Check(nil, self,
        AttrSet(fStatement, OCI_HTYPE_STMT, @Prefetch, 0,
          OCI_ATTR_PREFETCH_ROWS, fError),
        fError);
      Prefetch := TSqlDBOracleConnectionProperties(Connection.Properties).RowsPrefetchSize;
      Check(nil, self,
        AttrSet(fStatement, OCI_HTYPE_STMT, @Prefetch, 0,
          OCI_ATTR_PREFETCH_MEMORY, fError),
        fError);
    end;
    Setlength(fRowBuffer, fInternalBufferSize);
    assert(fRowCount > 0);
    if ((hasLOB in ColumnLongTypes) or
        (hasCURS in ColumnLongTypes)) and
       (fRowCount > 100) then
      fRowCount := 100; // do not create too much POCILobLocator items
    fRowBufferCount := fRowCount; // fRowCount may be set to 0: avoid leaking
    // fRowBuffer[] contains Indicators[] + Col0[] + Col1[] + Col2[]...
    Indicators := pointer(fRowBuffer);
    RowSize := fRowBufferCount * ColCount * SizeOf(sb2);
    for i := 0 to ColCount - 1 do
      with fColumns[i] do
      begin
        RowSize := ((RowSize - 1) shr 3 + 1) shl 3; // 8 bytes Col*[] alignment
        ColumnAttr := RowSize;
        if not ColumnValueInlined then
        begin
          PP := @fRowBuffer[RowSize]; // first POCILobLocator item
          for j := 1 to fRowBufferCount do
          begin
            case ColumnValueDBType of
              SQLT_CLOB,
              SQLT_BLOB:
                Check(nil, self,
                  DescriptorAlloc(Env, PP^, OCI_DTYPE_LOB, 0, nil),
                  fError);
              SQLT_RSET:
                Check(nil, self,
                  HandleAlloc(Env, PP^, OCI_HTYPE_STMT, 0, nil),
                  fError);
            else
              raise ESqlDBOracle.CreateUtf8('%: Wrong % type for %',
                [self, ColumnValueDBType, ColumnName]);
            end;
            inc(PP);
          end;
        end;
        oDefine := nil;
        Check(nil, self,
          DefineByPos(fStatement, oDefine, fError, i + 1,
            @fRowBuffer[RowSize], ColumnValueDBSize,
            ColumnValueDBType, Indicators, nil, nil, OCI_DEFAULT),
          fError);
        case ColumnType of
          ftCurrency: // currency content is returned as SQLT_STR
            Check(nil, self,
              AttrSet(oDefine, OCI_HTYPE_DEFINE,
                @OCI_CHARSET_WIN1252, 0, OCI_ATTR_CHARSET_ID, fError),
              fError);
          ftUtf8:
            case ColumnValueDBForm of
              SQLCS_IMPLICIT: // force CHAR + VARCHAR2 inlined fields charset
                // -> a conversion into UTF-8 would probably truncate the inlined result
                Check(nil, self,
                  AttrSet(oDefine, OCI_HTYPE_DEFINE,
                    @ColumnValueDBCharSet, 0, OCI_ATTR_CHARSET_ID, fError),
                  fError);
              SQLCS_NCHAR: // NVARCHAR2 + NCLOB will be retrieved directly as UTF-8 content
                Check(nil, self,
                  AttrSet(oDefine, OCI_HTYPE_DEFINE,
                    @OCI_CHARSET_UTF8, 0, OCI_ATTR_CHARSET_ID, fError),
                  fError);
            end;
        end;
        inc(RowSize, fRowBufferCount * ColumnValueDBSize);
        inc(Indicators, fRowBufferCount * SizeOf(sb2));
      end;
    assert(PtrUInt(Indicators - pointer(fRowBuffer)) = fRowBufferCount *
      ColCount * SizeOf(sb2));
    assert(RowSize <= fInternalBufferSize);
  end;
end;

procedure TSqlDBOracleStatement.Prepare(const aSql: RawUtf8; ExpectResults: boolean);
var
  env: POCIEnv;
  L: PtrInt;
begin
  SqlLogBegin(sllDB);
  try
    try
      if (fStatement <> nil) or
         (fColumnCount > 0) then
        raise ESqlDBOracle.CreateUtf8(
          '%.Prepare should be called only once', [self]);
      // 1. process SQL
      inherited Prepare(aSql, ExpectResults); // set fSql + Connect if necessary
      fPreparedParamsCount := ReplaceParamsByNumbers(aSql, fSqlPrepared, ':', true);
      L := Length(fSqlPrepared);
      while (L > 0) and
            (fSqlPrepared[L] <= ' ') do // trim right
        dec(L);
      // allow one trailing ';' by writing ';;' or allows 'END;' at the end of a statement
      if (L > 5) and
         (fSqlPrepared[L] = ';') and
         not (IdemPChar(@fSqlPrepared[L - 3], 'END') and
              (fSqlPrepared[L - 4] <= 'A')) then
        dec(L);
      if L <> Length(fSqlPrepared) then
        SetLength(fSqlPrepared, L); // trim trailing spaces or ';' if needed
      // 2. prepare statement
      env := (Connection as TSqlDBOracleConnection).fEnv;
      with OCI do
      begin
        HandleAlloc(env, fError, OCI_HTYPE_ERROR);
        if fUseServerSideStatementCache then
        begin
          if StmtPrepare2(TSqlDBOracleConnection(Connection).fContext,
             fStatement, fError, pointer(fSqlPrepared), length(fSqlPrepared), nil,
             0, OCI_NTV_SYNTAX, OCI_PREP2_CACHE_SEARCHONLY) = OCI_SUCCESS then
            fCacheIndex := 1
          else
            Check(nil, self,
              StmtPrepare2(TSqlDBOracleConnection(Connection).fContext,
                fStatement, fError, pointer(fSqlPrepared), length(fSqlPrepared),
                nil, 0, OCI_NTV_SYNTAX, OCI_DEFAULT),
              fError);
        end
        else
        begin
          HandleAlloc(env, fStatement, OCI_HTYPE_STMT);
          Check(nil, self,
            StmtPrepare(fStatement, fError,
              pointer(fSqlPrepared), length(fSqlPrepared),
              OCI_NTV_SYNTAX, OCI_DEFAULT),
            fError);
        end;
      end;
      // note: if SetColumnsForPreparedStatement is called here, we randomly got
      // "ORA-00932 : inconsistent datatypes" error -> moved to ExecutePrepared
    except
      on E: Exception do
      begin
        FreeHandles(True);
        raise;
      end;
    end;
  finally
    fTimeElapsed.FromExternalMicroSeconds(SqlLogEnd(' cache=%', [fCacheIndex]));
  end;
end;

function TSqlDBOracleStatement.Step(SeekFirst: boolean): boolean;
var
  sav, status: integer;
begin
  if not Assigned(fStatement) then
    raise ESqlDBOracle.CreateUtf8(
      '%.Execute should be called before Step', [self]);
  result := false;
  if (fCurrentRow < 0) or
     (fRowCount = 0) then
    exit; // no data available at all
  sav := fCurrentRow;
  fCurrentRow := -1;
  if fColumnCount = 0 then
    exit; // no row available at all (e.g. for SQL UPDATE) -> return false
  if sav <> 0 then
  begin
    // ignore if just retrieved ROW #1
    if SeekFirst then
    begin
      fTimeElapsed.Resume;
      try
       { if OCI.major_version < 9 then
          raise ESqlDBOracle.CreateUtf8('OCI % does not support OCI_FETCH_FIRST',
            [OCI.ClientRevision]); }
        status := OCI.StmtFetch(fStatement, fError, fRowCount,
          OCI_FETCH_FIRST, OCI_DEFAULT);
        FetchTest(status); // error + set fRowCount+fRowFetchedCurrent
        if fCurrentRow < 0 then // should not happen
          raise ESqlDBOracle.Create('OCI_FETCH_FIRST did not reset cursor');
      finally
        fTimeElapsed.Pause;
      end;
    end
    else
    begin
      // ensure we have some data in fRowBuffer[] for this row
      inc(fRowFetchedCurrent);
      if fRowFetchedCurrent >= fRowFetched then
      begin
        // reached end of buffer
        if fRowFetchedEnded then
          exit; // no more data
        fTimeElapsed.Resume;
        try
          FetchRows;
          if fRowFetched = 0 then
            exit; // no more row available -> return false + fCurrentRow=-1
        finally
          fTimeElapsed.Pause;
        end;
      end;
    end;
  end;
  fCurrentRow := sav + 1;
  inc(fTotalRowsRetrieved);
  result := true; // mark data available in fRowSetData
end;


initialization
  TSqlDBOracleConnectionProperties.RegisterClassNameForDefinition;

end.

