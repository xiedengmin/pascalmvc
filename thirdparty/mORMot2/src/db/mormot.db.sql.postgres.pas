/// Database Framework Direct PostgreSQL Connnection via libpq
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.sql.postgres;

{
  *****************************************************************************

   Direct PostgreSQL Client Access using the libpq Library
    -  TSqlDBPostgreConnection* and TSqlDBPostgreStatement Classes

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


{ ************ TSqlDBPostgreConnection* and TSqlDBPostgreStatement Classes }

type
  /// connection properties which will implement an internal Thread-Safe
  // connection pool
  TSqlDBPostgresConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  private
    fOids: TWordDynArray; // O(n) search in L1 cache - use SSE2 on FPC x86_64
    fOidsFieldTypes: TSqlDBFieldTypeDynArray;
    fOidsCount: integer;
  protected
    procedure GetForeignKeys; override;
    /// fill mapping of standard OID
    // - at runtime mapping can be defined using Oid2FieldType() method
    // - OIDs defined in DB can be retrieved using query
    //  "select oid, typname from pg_type where typtype = 'b' order by oid"
    procedure FillOidMapping; virtual;
  public
    /// initialize the properties
    // - raise an exception in case libpg is not thead-safe
    // - aDatabaseName can be a Connection URI - see
    // https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
    // - if aDatabaseName contains connection URI with password we recommend to repeat password
    // in aPassword parameter to prevent logging it (see TSqlDBConnectionProperties.DatabaseNameSafe)
    // - better to use environment variables and postgres config file for connection parameters
    constructor Create(
      const aServerName, aDatabaseName, aUserID, aPassword: RawUtf8); override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBPostgresConnection instance
    function NewConnection: TSqlDBConnection; override;
    /// add or replace mapping of OID into TSqlDBFieldType
    // - in case mapping for OID is not defined, returns ftUtf8
    function Oid2FieldType(cOID: cardinal): TSqlDBFieldType;
      {$ifdef HASINLINE}inline;{$endif}
    /// add new (or override existed) OID to FieldType mapping
    procedure MapOid(cOid: cardinal; fieldType: TSqlDBFieldType);
  end;


  /// implements a connection via the libpq access layer
  TSqlDBPostgresConnection = class(TSqlDBConnectionThreadSafe)
  protected
    // prepared statement names = SHA-256 of its SQL
    fPrepared: THash256DynArray; // O(n) fast search in L1 cache
    fPreparedCount: integer;
    // the associated low-level provider connection
    fPGConn: pointer;
    // fServerSettings: set of (ssByteAasHex);
    // maintain fPrepared[] hash list to identify already cached
    // - returns statement index in prepared cache array
    function PrepareCached(const aSql: RawUtf8; aParamCount: integer;
      out aName: RawUtf8): integer;
    /// direct execution of SQL statement what do not returns a result
    // - statement should not contains parameters
    // - raise an ESqlDBPostgres on error
    procedure DirectExecSql(const SQL: RawUtf8); overload;
    /// direct execution of SQL statement what do not returns a result
    // - overloaded method to return a single value e.g. from a SELECT
    procedure DirectExecSql(const SQL: RawUtf8; out Value: RawUtf8); overload;
    /// query the pg_settings table for a given setting
    function GetServerSetting(const Name: RawUtf8): RawUtf8;
  public
    /// connect to the specified server
    // - should raise an ESqlDBPostgres on error
    procedure Connect; override;
    /// stop connection to the specified PostgreSQL database server
    // - should raise an ESqlDBPostgres on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSqlDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// direct access to the associated PPGconn connection
    property Direct: pointer
      read fPGConn;
    /// how many prepared statements are currently cached for this connection
    property PreparedCount: integer
      read fPreparedCount;
  end;


  /// implements a statement via a Postgres database connection
  TSqlDBPostgresStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fPreparedStmtName: RawUtf8; // = SHA-256 of the SQL
    fPreparedParamsCount: integer;
    fRes: pointer;
    fResStatus: integer;
    // pointers to query parameters; initialized by Prepare, filled in Executeprepared
    fPGParams: TPointerDynArray;
    // 0 - text, 1 - binary; initialized by Prepare, filled in Executeprepared
    fPGParamFormats: TIntegerDynArray;
    // non zero for binary params
    fPGparamLengths: TIntegerDynArray;
    /// define the result columns name and content
    procedure BindColumns;
    /// raise an exception if Col is out of range according to fColumnCount
    // or rowset is not initialized
    procedure CheckColAndRowset(const Col: integer);
  public
    /// finalize the statement for a given connection
    destructor Destroy; override;
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBPostgres on any error
    procedure Prepare(const aSql: RawUtf8; ExpectResults: boolean = False); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also handle bound array of values (if any)
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESqlDBPostgres on any error
    procedure ExecutePrepared; override;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESqlDBPostgres on any error
    procedure Reset; override;

    /// Access the next or first row of data from the SQL Statement result
    // - return true on success, with data ready to be retrieved by Column*() methods
    // - return false if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - if SeekFirst is TRUE, will put the cursor on the first row of results
    // - raise an ESqlDBPostgres on any error
    function Step(SeekFirst: boolean = False): boolean; override;
    /// clear(fRes) when ISqlDBStatement is back in cache
    procedure ReleaseRows; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): int64; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUtf8(Col: integer): RawUtf8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; override;
    /// append all columns values of the current Row to a JSON stream
    // - overriden method to avoid temporary memory allocation or conversion
    procedure ColumnsToJson(WR: TResultsWriter); override;
    /// how many parameters founded during prepare stage
    property PreparedParamsCount: integer
      read fPreparedParamsCount;
  end;


implementation

uses
  mormot.crypt.core,  // libpq requires named prepared statements = use SHA-256
  mormot.db.raw.postgres; // raw libpq library API access


{ ************ TSqlDBPostgreConnection* and TSqlDBPostgreStatement Classes }

{ TSqlDBPostgresConnection }

function TSqlDBPostgresConnection.PrepareCached(
  const aSql: RawUtf8; aParamCount: integer; out aName: RawUtf8): integer;
var
  dig: TSha256Digest;
begin
  dig := Sha256Digest(aSql);
  aName := Sha256DigestToString(dig);
  result := Hash256Index(pointer(fPrepared), fPreparedCount, @dig);
  if result >= 0 then
    exit; // already prepared -> we will just give the statement name to PQ
  PQ.Check(fPGConn,
    PQ.Prepare(fPGConn, pointer(aName), pointer(aSql), aParamCount, nil));
  result := fPreparedCount;
  inc(fPreparedCount);
  if result = length(fPrepared) then
    SetLength(fPrepared, result + 32);
  fPrepared[result] := dig;
end;

procedure TSqlDBPostgresConnection.DirectExecSql(const SQL: RawUtf8);
begin
  PQ.Check(fPGConn,
    PQ.Exec(fPGConn, pointer(SQL)));
end;

procedure TSqlDBPostgresConnection.DirectExecSql(
  const SQL: RawUtf8; out Value: RawUtf8);
var
  res: PPGresult;
begin
  res := PQ.Exec(fPGConn, pointer(SQL));
  PQ.Check(fPGConn, res, nil, {andclear=}false);
  PQ.GetRawUtf8(res, 0, 0, Value);
  PQ.Clear(res);
end;

function TSqlDBPostgresConnection.GetServerSetting(const Name: RawUtf8): RawUtf8;
var
  sql: RawUtf8;
begin
  FormatUtf8('select setting from pg_settings where name=''%''', [Name], sql);
  DirectExecSql(sql, result);
end;

// our conversion is faster than PQUnescapeByteA - which requires libpq 8.3+
//  and calls malloc()
// https://github.com/postgres/postgres/blob/master/src/interfaces/libpq/fe-exec.c

// checking \x for hexadecimal encoding is what UnescapeByteA() does
// -> no need to ask server settings
// note: bytea_output is HEX by default (at least since PostgreSQL 9.0)

function BlobInPlaceDecode(P: PAnsiChar; PLen: integer): integer;
begin
  if (P = nil) or
     (PLen <= 0) then
    result := 0
  else
  if PWord(P)^ = ord('\') + ord('x') shl 8 then {ssByteAasHex in fServerSettings}
  begin
    result := (PLen - 2) shr 1; // skip trailing \x and compute number of bytes
    if result > 0 then
      HexToBinFast(P + 2, PByte(P), result); // in-place conversion
  end
  else
    // oldest PostgreSQL versions may stil use octal encoding (unlikely)
    result := OctToBin(P, pointer(P)); // in-place conversion
end;

procedure SynLogNoticeProcessor({%H-}arg: Pointer; message: PUtf8Char); cdecl;
begin
  SynDBLog.Add.Log(sllTrace, 'PGINFO: %', [message], TObject(arg));
end;

procedure DummyNoticeProcessor({%H-}arg: Pointer; message: PUtf8Char); cdecl;
begin
end;

procedure TSqlDBPostgresConnection.Connect;
var
  log: ISynLog;
  host, port: RawUtf8;
begin
  log := SynDBLog.Enter(self, 'Connect');
  Disconnect; // force fTrans=fError=fServer=fContext=nil
  try
    Split(Properties.ServerName, ':', host, port);
    fPGConn := PQ.SetDBLogin(pointer(host), pointer(port), nil, nil,
      pointer(Properties.DatabaseName), pointer(Properties.UserID),
      pointer(Properties.PassWord));
    if PQ.Status(fPGConn) = CONNECTION_BAD then
      raise ESqlDBPostgres.CreateUtf8('Connection to database % failed [%]',
        [Properties.DatabaseNameSafe, PQ.ErrorMessage(fPGConn)]);
    // if GetServerSetting('bytea_output') = 'HEX' then
    //   include(fServerSettings, ssByteAasHex);
    if log <> nil then
    begin
      PQ.SetNoticeProcessor(fPGConn, SynLogNoticeProcessor, pointer(self));
      log.Log(sllDB, 'Connected to % % using % v%', [fProperties.ServerName,
        fProperties.DatabaseNameSafe, PQ.LibraryPath, PQ.LibVersion], self);
    end
    else
      // to ensure no performance drop due to notice to console
      PQ.SetNoticeProcessor(fPGConn, DummyNoticeProcessor, nil);
    inherited Connect; // notify any re-connection
  except
    on E: Exception do
    begin
      if log <> nil then
        log.Log(sllError, 'Connect: % on %',
          [E, Properties.DatabaseNameSafe], self);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSqlDBPostgresConnection.Disconnect;
begin
  try
    inherited Disconnect;
  finally
    // any prepared statements will be released with this connection
    fPrepared := nil;
    fPreparedCount := 0;
    // let PG driver finish the connection
    if fPGConn <> nil then
    begin
      PQ.Finish(fPGConn);
      fPGConn := nil;
    end;
  end;
end;

function TSqlDBPostgresConnection.IsConnected: boolean;
begin
  result := (fPGConn <> nil);
end;

function TSqlDBPostgresConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBPostgresStatement.Create(self);
end;

procedure TSqlDBPostgresConnection.StartTransaction;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'StartTransaction');
  if TransactionCount > 0 then
    raise ESqlDBPostgres.CreateUtf8('Invalid %.StartTransaction: nested ' +
      'transactions are not supported by the Postgres - use SAVEPOINT instead',
      [self]);
  try
    inherited StartTransaction;
    DirectExecSql('START TRANSACTION');
  except
    on E: Exception do
    begin
      if log <> nil then
        log.Log(sllError, 'StartTransaction: % on %',
          [E, Properties.DatabaseNameSafe], self);
      if fTransactionCount > 0 then
        Dec(fTransactionCount);
      raise;
    end;
  end;
end;

procedure TSqlDBPostgresConnection.Commit;
begin
  inherited Commit;
  try
    DirectExecSql('COMMIT');
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBPostgresConnection.Rollback;
begin
  inherited;
  DirectExecSql('ROLLBACK');
end;


{ TSqlDBPostgresConnectionProperties }

procedure TSqlDBPostgresConnectionProperties.GetForeignKeys;
begin
  // TODO - how to get field we reference to? (currently consider this is "ID")
  with Execute('SELECT' + '  ct.conname as foreign_key_name, ' +
      '  case when ct.condeferred then 1 else 0 end AS is_disabled, ' +
      '  (SELECT tc.relname from pg_class tc where tc.oid = ct.conrelid) || ''.'' || ' +
      '     (SELECT a.attname FROM pg_attribute a WHERE a.attnum = ct.conkey[1] AND a.attrelid = ct.conrelid) as from_ref, ' +
      '  (SELECT tc.relname from pg_class tc where tc.oid = ct.confrelid) || ''.id'' as referenced_object ' +
      'FROM  pg_constraint ct WHERE contype = ''f''', []) do
    while Step do
      fForeignKeys.Add(ColumnUtf8(2), ColumnUtf8(3));
end;

procedure TSqlDBPostgresConnectionProperties.FillOidMapping;
begin
  // see pg_type.h (most used first)
  MapOid(INT4OID, ftInt64);
  MapOid(INT8OID, ftInt64);
  MapOid(TEXTOID, ftUtf8);
  MapOid(FLOAT8OID, ftDouble);
  MapOid(TIMESTAMPOID, ftDate);
  MapOid(BYTEAOID, ftBlob);
  MapOid(NUMERICOID, ftCurrency);// our ORM uses NUMERIC(19,4) for currency
  MapOid(BOOLOID, ftInt64);
  MapOid(INT2OID, ftInt64);
  MapOid(CASHOID, ftCurrency);
  MapOid(TIMESTAMPTZOID, ftDate);
  MapOid(ABSTIMEOID, ftDate);
  MapOid(DATEOID, ftDate);
  MapOid(TIMEOID, ftDate);
  MapOid(TIMETZOID, ftDate);
  MapOid(REGPROCOID, ftInt64);
  MapOid(OIDOID, ftInt64);
  MapOid(FLOAT4OID, ftDouble);
  // note: any other unregistered OID will be handled as ftUtf8 to keep the data
end;

constructor TSqlDBPostgresConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassword: RawUtf8);
begin
  PostgresLibraryInitialize; // raise an ESqlDBPostgres on loading failure
  if PQ.IsThreadSafe <> 1 then
    raise ESqlDBPostgres.Create('libpq should be compiled in threadsafe mode');
  fDbms := dPostgreSQL;
  FillOidMapping;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  // JsonDecodedPrepareToSql will detect cPostgreBulkArray and set
  // DecodedFieldTypesToUnnest -> fast bulk insert/delete/update
  fBatchSendingAbilities := [cCreate, cDelete, cUpdate, cPostgreBulkArray];
  // disable MultiInsert SQL and rely on cPostgreBulkArray process for cCreate
  fOnBatchInsert := nil; // see TRestStorageExternal.InternalBatchStop
end;

function TSqlDBPostgresConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBPostgresConnection.Create(self);
  TSqlDBPostgresConnection(result).InternalProcess(speCreated);
end;

function TSqlDBPostgresConnectionProperties.Oid2FieldType(
  cOID: cardinal): TSqlDBFieldType;
var
  i: PtrInt;
begin
  if cOID <= 65535 then
  begin
    // fast brute force search within L1 CPU cache (use SSE2 asm on Intel/AMD)
    i := WordScanIndex(pointer(fOids), fOidsCount, cOID);
    if i >= 0 then
      result := fOidsFieldTypes[i]
    else
      result := ftUtf8;
  end
  else
    result := ftUtf8;
end;

procedure TSqlDBPostgresConnectionProperties.MapOid(cOid: cardinal;
  fieldType: TSqlDBFieldType);
var
  i: PtrInt;
begin
  if cOID > 65535 then
    raise ESqlDBPostgres.CreateUtf8('Out of range %.MapOid(%)', [self, cOID]);
  i := WordScanIndex(pointer(fOids), fOidsCount, cOID);
  if i < 0 then
  begin
    i := FOidsCount;
    inc(FOidsCount);
    if i = length(FOids) then
    begin
      SetLength(fOids, i + 32);
      SetLength(fOidsFieldTypes, i + 32);
    end;
    fOids[i] := cOid;
  end;
  fOidsFieldTypes[i] := fieldType // set or replace
end;

procedure TSqlDBPostgresStatement.BindColumns;
var
  nCols, c: integer;
  cName: RawUtf8;
begin
  fColumn.Clear;
  fColumn.ForceReHash;
  nCols := PQ.nfields(fRes);
  fColumn.Capacity := nCols;
  for c := 0 to nCols - 1 do
  begin
    cName := PQ.fname(fRes, c);
    with PSqlDBColumnProperty(fColumn.AddAndMakeUniqueName(cName))^ do
    begin
      ColumnAttr := PQ.ftype(fRes, c);
      ColumnType := TSqlDBPostgresConnectionProperties(Connection.Properties).
        Oid2FieldType(ColumnAttr);
    end;
  end;
end;

procedure TSqlDBPostgresStatement.CheckColAndRowset(const Col: integer);
begin
  CheckCol(Col);
  if (fRes = nil) or
     (fResStatus <> PGRES_TUPLES_OK) then
    raise ESqlDBPostgres.CreateUtf8(
      '%.Execute not called before Column*', [self]);
end;

destructor TSqlDBPostgresStatement.Destroy;
begin
  try
    Reset; // close result if any
  finally
    inherited;
  end;
end;

// see https://www.postgresql.org/docs/9.3/libpq-exec.html

const
  PREP_SQL: array[0..5] of PAnsiChar = (
    'SELECT',
    'INSERT',
    'UPDATE',
    'DELETE',
    'VALUES',
    nil);

procedure TSqlDBPostgresStatement.Prepare(
  const aSql: RawUtf8; ExpectResults: boolean);
begin
  SqlLogBegin(sllDB);
  if aSql = '' then
    raise ESqlDBPostgres.CreateUtf8('%.Prepare: empty statement', [self]);
  inherited Prepare(aSql, ExpectResults); // will strip last ;
  fPreparedParamsCount := ReplaceParamsByNumbers(fSql, fSqlPrepared, '$');
  if (fPreparedParamsCount > 0) and
     (IdemPPChar(pointer(fSqlPrepared), @PREP_SQL) >= 0) then
  begin
    // preparable statement will be cached by Sha256(SQL) name on server side
    fCacheIndex := TSqlDBPostgresConnection(fConnection).PrepareCached(
      fSqlPrepared, fPreparedParamsCount, fPreparedStmtName);
    SqlLogEnd(' name=% cache=%', [fPreparedStmtName, fCacheIndex]);
  end
  else
    SqlLogEnd;
  SetLength(fPGParams, fPreparedParamsCount);
  SetLength(fPGParamFormats, fPreparedParamsCount);
  SetLength(fPGparamLengths, fPreparedParamsCount);
end;

procedure TSqlDBPostgresStatement.ExecutePrepared;
var
  i: PtrInt;
  p: PSqlDBParam;
  c: TSqlDBPostgresConnection;
begin
  SqlLogBegin(sllSQL);
  if fSqlPrepared = '' then
    raise ESqlDBPostgres.CreateUtf8(
      '%.ExecutePrepared: Statement not prepared', [self]);
  if fParamCount <> fPreparedParamsCount then
    raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: Query expects % ' +
      'parameters but % bound', [self, fPreparedParamsCount, fParamCount]);
  inherited ExecutePrepared;
  for i := 0 to fParamCount - 1 do // set parameters as expected by PostgreSQL
  begin
    // mark parameter as textual by default, with no blob len
    fPGParamFormats[i] := 0;
    fPGparamLengths[i] := 0;
    // convert parameter value as text stored in p^.VData
    p := @fParams[i];
    if p^.VArray <> nil then
    begin
      if not (p^.VType in [
           ftInt64,
           ftDouble,
           ftCurrency,
           ftDate,
           ftUtf8]) then
        raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: Invalid array ' +
          'type % on bound parameter #%', [Self, ToText(p^.VType)^, i]);
      p^.VData := BoundArrayToJsonArray(p^.VArray);
    end
    else
    begin
      case p^.VType of
        ftNull:
          p^.VData := '';
        ftInt64:
          // use SwapEndian + binary ?
          Int64ToUtf8(p^.VInt64, RawUtf8(p^.VData));
        ftCurrency:
          Curr64ToStr(p^.VInt64, RawUtf8(p^.VData));
        ftDouble:
          DoubleToStr(PDouble(@p^.VInt64)^, RawUtf8(p^.VData));
        ftDate:
          // Postgres expects space instead of T in ISO-8601 expanded format
          p^.VData := DateTimeToIso8601(
            PDateTime(@p^.VInt64)^, true, ' ', fForceDateWithMS);
        ftUtf8:
          ; // text already in p^.VData
        ftBlob:
        begin
          fPGParamFormats[i] := 1; // binary
          fPGparamLengths[i] := length(p^.VData);
        end;
        else
          raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: cannot bind ' +
            'parameter #% of type %', [self, i, ToText(p^.VType)^]);
      end;
    end;
    fPGParams[i] := pointer(p^.VData);
  end;
  c := TSqlDBPostgresConnection(Connection);
  if fPreparedStmtName <> '' then
    fRes := PQ.ExecPrepared(c.fPGConn, pointer(fPreparedStmtName),
      fPreparedParamsCount, pointer(fPGParams), pointer(fPGparamLengths),
      pointer(fPGParamFormats), PGFMT_TEXT)
  else if fPreparedParamsCount = 0 then
    // PQexec handles multiple SQL commands
    fRes := PQ.Exec(c.fPGConn, pointer(fSqlPrepared)) else
    fRes := PQ.ExecParams(c.fPGConn, pointer(fSqlPrepared),
      fPreparedParamsCount, nil, pointer(fPGParams), pointer(fPGparamLengths),
      pointer(fPGParamFormats), PGFMT_TEXT);
  PQ.Check(c.fPGConn, fRes, @fRes, {forceClean=}false);
  fResStatus := PQ.ResultStatus(fRes);
  if fExpectResults then
  begin
    if fResStatus <> PGRES_TUPLES_OK then
    begin
      // paranoid check
      PQ.Clear(fRes);
      fRes := nil;
      raise ESqlDBPostgres.CreateUtf8('%.ExecutePrepared: result expected ' +
        'but statement did not return tuples', [self]);
    end;
    fTotalRowsRetrieved := PQ.ntuples(fRes);
    fCurrentRow := -1;
    if fColumn.Count = 0 then // if columns exist then statement is already cached
      BindColumns;
    SqlLogEnd(' rows=%', [fTotalRowsRetrieved]);
  end
  else
    SqlLogEnd;
end;

function TSqlDBPostgresStatement.UpdateCount: integer;
begin
  result := GetCardinalDef(PQ.cmdTuples(fRes), 0);
end;

procedure TSqlDBPostgresStatement.Reset;
begin
  ReleaseRows;
  fResStatus := PGRES_EMPTY_QUERY;
  inherited Reset;
end;

function TSqlDBPostgresStatement.Step(SeekFirst: boolean): boolean;
begin
  if (fRes = nil) or
     (fResStatus <> PGRES_TUPLES_OK) then
    raise ESqlDBPostgres.CreateUtf8(
      '%.Execute should be called before Step', [self]);
  if SeekFirst then
    fCurrentRow := -1;
  result := fCurrentRow + 1 < fTotalRowsRetrieved;
  if not result then
    exit;
  inc(fCurrentRow);
end;

procedure TSqlDBPostgresStatement.ReleaseRows;
begin
  if fRes <> nil then
  begin
    PQ.clear(fRes);
    fRes := nil;
  end;
  inherited ReleaseRows;
end;

function TSqlDBPostgresStatement.ColumnInt(Col: integer): int64;
begin
  CheckColAndRowset(Col);
  result := GetInt64(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSqlDBPostgresStatement.ColumnNull(Col: integer): boolean;
begin
  CheckColAndRowset(Col);
  result := (PQ.GetIsNull(fRes, fCurrentRow, Col) = 1);
end;

function TSqlDBPostgresStatement.ColumnDouble(Col: integer): double;
begin
  CheckColAndRowset(Col);
  result := GetExtended(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSqlDBPostgresStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  CheckColAndRowset(Col);
  Iso8601ToDateTimePUtf8CharVar(PQ.GetValue(fRes, fCurrentRow, Col),
    PQ.GetLength(fRes, fCurrentRow, Col), result);
end;

function TSqlDBPostgresStatement.ColumnCurrency(Col: integer): currency;
begin
  CheckColAndRowset(Col);
  PInt64(@result)^ := StrToCurr64(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSqlDBPostgresStatement.ColumnUtf8(Col: integer): RawUtf8;
begin
  CheckColAndRowset(Col);
  PQ.GetRawUtf8(fRes, fCurrentRow, Col, result);
end;

function TSqlDBPostgresStatement.ColumnBlob(Col: integer): RawByteString;
var
  P: pointer;
begin
  // PGFMT_TEXT was used -> need to convert into binary
  CheckColAndRowset(Col);
  P := PQ.GetValue(fRes, fCurrentRow, Col);
  FastSetRawByteString(result, P,
    BlobInPlaceDecode(P, PQ.GetLength(fRes, fCurrentRow, col)));
end;

procedure TSqlDBPostgresStatement.ColumnsToJson(WR: TResultsWriter);
var
  col: integer;
  P: pointer;
begin
  if (fRes = nil) or
     (fResStatus <> PGRES_TUPLES_OK) or
     (fCurrentRow < 0) then
    raise ESqlDBPostgres.CreateUtf8('%.ColumnToJson unexpected', [self]);
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do
  with fColumns[col] do
  begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    if PQ.GetIsNull(fRes, fCurrentRow, col) = 1 then
      WR.AddNull
    else
    begin
      P := PQ.GetValue(fRes, fCurrentRow, col);
      case ColumnType of
        ftNull:
          WR.AddNull;
        ftInt64,
        ftDouble,
        ftCurrency:
          WR.AddNoJsonEscape(P, PQ.GetLength(fRes, fCurrentRow, col));
        ftUtf8:
          if (ColumnAttr = JSONOID) or
             (ColumnAttr = JSONBOID) then
            WR.AddNoJsonEscape(P, PQ.GetLength(fRes, fCurrentRow, col))
          else
          begin
            WR.Add('"');
            WR.AddJsonEscape(P);
            WR.Add('"');
          end;
        ftDate:
          begin
            WR.Add('"');
            if (PQ.GetLength(fRes, fCurrentRow, col) > 10) and
               (PAnsiChar(P)[10] = ' ') then
              PAnsiChar(P)[10] := 'T'; // ensure strict ISO-8601 encoding
            WR.AddJsonEscape(P);
            WR.Add('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            WR.AddNull
          else
            WR.WrBase64(P, BlobInPlaceDecode(P,
              PQ.GetLength(fRes, fCurrentRow, col)), {withmagic=}true);
        else
          raise ESqlDBPostgres.CreateUtf8('%.ColumnsToJson: %?',
            [self, ToText(ColumnType)^]);
      end;
    end;
    WR.AddComma;
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;


initialization
  TSqlDBPostgresConnectionProperties.RegisterClassNameForDefinition;

end.

