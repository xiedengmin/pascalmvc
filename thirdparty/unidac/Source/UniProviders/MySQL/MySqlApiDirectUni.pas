
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MySqlApiDirectUni;

interface

uses
  Variants, Types, SyncObjs, SysUtils,
  CLRClasses, CRTypes, CRAccess, CRVio,
{$IFNDEF UNIDACPRO}
  MyCall, MySqlApi, MySqlBind, MySqlResultSet,
  MySqlSession, MySqlStmt;
{$ELSE}
  MyCallUni, MySqlApiUni, MySqlBindUni, MySqlResultSetUni,
  MySqlSessionUni, MySqlStmtUni;
{$ENDIF}

type
  TMySQLAPIDirect = class (TMySQLAPI)
  protected
    FQueryBufferCS: TCriticalSection;

  public
    constructor Create;
    destructor Destroy; override;

    //function mysql_num_rows(pres: PMYSQL_RES): Int64; override;
    function mysql_num_fields(pres: PMYSQL_RES): cardinal; override;
    //function mysql_eof(pres: PMYSQL_RES): byte; override;
    //function mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: cardinal): PMYSQL_FIELD; override;
    //function mysql_fetch_fields(pres: PMYSQL_RES): PMYSQL_FIELDS; override;
    //function mysql_row_tell(pres: PMYSQL_RES): PMYSQL_ROWS; override;
    //function mysql_field_tell(pres: PMYSQL_RES): cardinal; override;

    function mysql_field_count(pmysql: PMYSQL_CON): cardinal; override;
    function mysql_affected_rows(pmysql: PMYSQL_CON): Int64; override;
    function mysql_insert_id(pmysql: PMYSQL_CON): Int64; override;
    function mysql_warning_count(pmysql: PMYSQL_CON): cardinal; override;
    function mysql_errno(pmysql: PMYSQL_CON): cardinal; override;
    function mysql_error(pmysql: PMYSQL_CON): string; override;
    function mysql_isservererror(pmysql: PMYSQL_CON): boolean; override;
    function mysql_info(pmysql: PMYSQL_CON): string; override;
    function mysql_thread_id(pmysql: PMYSQL_CON): cardinal; override;
    function mysql_character_set_name(pmysql: PMYSQL_CON): string; override;

    function mysql_init(pmysql: PMYSQL_CON): PMYSQL_CON; override;
    function mysql_ssl_set(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: string): integer; override;
    //function mysql_connect(pmysql: PMYSQL_CON; const host, user, passwd: PChar): PMYSQL_CON; override;
    //function mysql_change_user(pmysql: PMYSQL_CON; const user, passwd, db: PChar): byte; override;
    function mysql_real_connect(pmysql: PMYSQL_CON; const host, user, passwd, db: string; port: cardinal; const unix_socket: string; clientflag: cardinal): PMYSQL_CON; override;
    procedure mysql_close(pmysql: PMYSQL_CON); override;
    function mysql_select_db(pmysql: PMYSQL_CON; const db: string): integer; override;
    //function mysql_query(pmysql: PMYSQL_CON; const q: PChar): integer; override;
    //function mysql_send_query(pmysql: PMYSQL_CON; const q: PChar; length: cardinal): integer; override;
    //function mysql_read_query_result(pmysql: PMYSQL_CON): integer; override;
    function mysql_real_query(pmysql: PMYSQL_CON; const q: PAnsiChar; _length: cardinal): integer; override;
    //function mysql_create_db(pmysql: PMYSQL_CON; const DB: PChar): integer; override;
    //function mysql_drop_db(pmysql: PMYSQL_CON; const DB: PChar): integer; override;
    //function mysql_shutdown(pmysql: PMYSQL_CON): integer; override;
    //function mysql_dump_debug_info(pmysql: PMYSQL_CON): integer; override;
    //function mysql_refresh(pmysql: PMYSQL_CON; refresh_options: cardinal): integer; override;
    function mysql_kill(pmysql: PMYSQL_CON; pid: cardinal): integer; override;
    function mysql_ping(pmysql: PMYSQL_CON): integer; override;
    //function mysql_stat(pmysql: PMYSQL_CON): string; override;
    function mysql_get_server_info(pmysql: PMYSQL_CON): string; override;
    function mysql_get_client_info: string; override;
    function mysql_get_host_info(pmysql: PMYSQL_CON): string; override;
    //function mysql_get_proto_info(pmysql: PMYSQL_CON): cardinal; override;
    //function mysql_list_dbs(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES; override;
    //function mysql_list_tables(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES; override;
    //function mysql_list_fields(pmysql: PMYSQL_CON; const table, wild: PChar): PMYSQL_RES; override;
    //function mysql_list_processes(pmysql: PMYSQL_CON): PMYSQL_RES; override;
    //function mysql_store_result(pmysql: PMYSQL_CON): PMYSQL_RES; override;
    function mysql_use_result(pmysql: PMYSQL_CON): PMYSQL_RES; override;
    function mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; var arg: integer): integer; overload; override;
    function mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; const arg: string): integer; overload; override;
    procedure mysql_free_result(pres: PMYSQL_RES); override;
    //function mysql_data_seek: procedure(pres: PMYSQL_RES; offset: Int64); override;
    //function mysql_row_seek(pres: PMYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET; override;
    //function mysql_field_seek(pres: PMYSQL_RES; offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; override;
    function mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW; override;
    function mysql_fetch_lengths(pres: PMYSQL_RES): PMYSQL_LENGTHS; override;
    //function mysql_fetch_field(pres: PMYSQL_RES): PMYSQL_FIELD; override;
    //function mysql_escape_string(_to: PChar; const from: PChar; from_length: cardinal): cardinal; override;
    //function mysql_real_escape_string(pmysql: PMYSQL_CON; _to: PChar; const from: PChar; length: cardinal): cardinal; override;
    //function mysql_debug: procedure(const debug: PChar); override;
    //function mysql_odbc_escape_string(pmysql: PMYSQL_CON; _to: PChar; to_length: cardinal; const from: PChar; from_length: cardinal; param: pointer; extend_buffer: extend_buffer_func): string; override;
    //myodbc_remove_escape: procedure(pmysql: PMYSQL_CON; name: PChar); override;
    //function mysql_thread_safe: function: cardinal; override;

    function mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer; override;
    procedure mysql_server_end; override;

  //  function function mysql_reload(pmysql: PMySQL): integer; override;

  // C API Prepared Statements functions
    function mysql_stmt_init(pmysql: PMYSQL_CON): PMYSQL_STMT; override;
    function mysql_stmt_prepare(pstmt: PMYSQL_STMT; const q: PAnsiChar; _length: cardinal): integer; override;
    //function mysql_stmt_execute(pstmt: PMYSQL_STMT): integer; override;
    function mysql_stmt_param_count(pstmt: PMYSQL_STMT): cardinal; override;
    //function mysql_stmt_bind_param(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND): byte; override;
    //function mysql_stmt_bind_result(pstmt: PMYSQL_STMT; pbnd: PMYSQL_BIND): byte; override;
    function mysql_stmt_field_count(pstmt: PMYSQL_STMT): cardinal; override;
    function mysql_stmt_close(pstmt: PMYSQL_STMT): byte; override;
    function mysql_stmt_free_result(pstmt: PMYSQL_STMT): byte; override;
    function mysql_stmt_errno(pstmt: PMYSQL_STMT): cardinal; override;
    function mysql_stmt_error(pstmt: PMYSQL_STMT): string; override;
    function mysql_stmt_isservererror(pstmt: PMYSQL_STMT): boolean; override;
    //function mysql_commit(pmysql: PMYSQL_CON): byte; override;
    //function mysql_rollback(pmysql: PMYSQL_CON): byte; override;
    //function mysql_autocommit(pmysql: PMYSQL_CON; auto_mode: byte): byte; override;
    function mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer; override;
    function _mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; var bnds: TMYSQL_BINDS; column: cardinal; offset: cardinal): integer; override;
    function mysql_stmt_send_long_data(pstmt: PMYSQL_STMT; param_number: cardinal; const data: PAnsiChar; length: cardinal): byte; override;
    function mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; override;
    function mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; override;
    function mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): Int64; override;
    //function mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer; override;
    function mysql_more_results(pmysql: PMYSQL_CON): byte; override;
    function mysql_next_result(pmysql: PMYSQL_CON): integer; override;

    procedure _mysql_fetch_lengths(pres: PMYSQL_RES; const Lens: TLenArr); override;
    function _mysql_fetch_value_is_null(prow: PMYSQL_ROW; fieldnr: integer): boolean; override;
    function _mysql_fetch_value_ptr(pres: PMYSQL_RES; prow: PMYSQL_ROW; fieldnr: cardinal): IntPtr; override;
    procedure _mysql_fetch_value_to_buff(prow: PMYSQL_ROW; fieldnr: cardinal; const Buff: TBytes; Off: integer; Len: integer); override;
    procedure _mysql_fetch_value_to_buff(prow: PMYSQL_ROW; fieldnr: cardinal; pBuff: IntPtr; Len: integer); override;
    procedure _mysql_fetch_value_to_str(prow: PMYSQL_ROW; fieldnr: cardinal; pBuff: IntPtr; Len: integer); override;
    function _mysql_fetch_value_arr(prow: PMYSQL_ROW; fieldnr: cardinal; out Off: integer; Len: integer): TValueArr; override;
    function _mysql_fetch_value_str(prow: PMYSQL_ROW; fieldnr: cardinal): AnsiString; override;

    function _mysql_read_row(pres: PMYSQL_RES; Index: integer): PMYSQL_ROW; override;
    function _mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: cardinal; const Unicode: boolean): TMYSQL_FIELD; override;
    function _mysql_fetch_options(pres: PMYSQL_RES; option: TMySqlFecthOption; arg: integer): integer; override;

    function _mysql_stmt_execute(pstmt: PMYSQL_STMT; Command: TCRCommand; const Unicode: boolean): integer; override;
    function _mysql_stmt_fetch(pstmt: PMYSQL_STMT; out row: PMYSQL_ROW): integer; override;
    function _mysql_stmt_bind_result(pstmt: PMYSQL_STMT; const bnds: TMYSQL_BINDS): byte; override;
    procedure _mysql_stmt_fetch_lengths(prow: PMYSQL_ROW; const Lens: TLenArr); override;
    function _mysql_stmt_fetch_value_ptr(pstmt: PMYSQL_STMT; fieldnr: cardinal): IntPtr; override;

    procedure CheckMySQLLib; override;
    procedure SetIOHandler(pmysql: PMYSQL_CON; IOHandler: TCRIOHandler);
    procedure SetOptions(pmysql: PMYSQL_CON; HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
      SSLOptions: TSSLOptions);
    procedure SetIPVersion(pmysql: PMYSQL_CON; IPVersion: TIPVersion);
    procedure SetOnDialogAuthPlugin(pmysql: PMYSQL_CON; OnDialogAuthPlugin: TMyDialogAuthPluginEvent);
    procedure SetIsUTF8(pmysql: PMYSQL_CON; Value: boolean);
  end;

var
  MyAPIDirect: TMySQLAPIDirect;

implementation

uses
  DAConsts, CRFunctions, MemUtils,
{$IFNDEF LITE}
  CRConnectionPool,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  MySqlErrors;
{$ELSE}
  MySqlErrorsUni;
{$ENDIF}

{ TMySQLAPIDirect }

constructor TMySQLAPIDirect.Create;
begin
  inherited;
  FQueryBufferCS := TCriticalSection.Create;
end;

destructor TMySQLAPIDirect.Destroy;
begin
  FQueryBufferCS.Free;
  inherited;
end;

function TMySQLAPIDirect.mysql_num_fields(pres: PMYSQL_RES): cardinal;
begin
  Assert(pres <> nil);
  Result := TMySqlResultSet(pres).FieldCount;
end;

function TMySQLAPIDirect.mysql_field_count(pmysql: PMYSQL_CON): cardinal;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).fieldCount;
end;

function TMySQLAPIDirect.mysql_affected_rows(pmysql: PMYSQL_CON): Int64;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).affectedRows;
end;

function TMySQLAPIDirect.mysql_insert_id(pmysql: PMYSQL_CON): Int64;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).insertId;
end;

function TMySQLAPIDirect.mysql_warning_count(pmysql: PMYSQL_CON): cardinal;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).warningCount;
end;

function TMySQLAPIDirect.mysql_errno(pmysql: PMYSQL_CON): cardinal;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).errno;
end;

function TMySQLAPIDirect.mysql_error(pmysql: PMYSQL_CON): string;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).error;
end;

function TMySQLAPIDirect.mysql_isservererror(pmysql: PMYSQL_CON): boolean;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).IsServerError;
end;

function TMySQLAPIDirect.mysql_info(pmysql: PMYSQL_CON): string;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).info;
end;

function TMySQLAPIDirect.mysql_thread_id(pmysql: PMYSQL_CON): cardinal;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).threadId;
end;

function TMySQLAPIDirect.mysql_character_set_name(pmysql: PMYSQL_CON): string;
begin
  Result := '';
  Assert(pmysql <> nil);
  raise NotSupportedException.Create;
end;

function TMySQLAPIDirect.mysql_init(pmysql: PMYSQL_CON): PMYSQL_CON;
begin
  Result := TMySqlSession.Create;
end;

function TMySQLAPIDirect.mysql_ssl_set(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: string): integer;
var
  mysql: TMySqlSession;
begin
  Result := 0;
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.protocolType := MYSQL_PROTOCOL_SSL;
end;

function TMySQLAPIDirect.mysql_real_connect(pmysql: PMYSQL_CON; const host, user, passwd, db: string; port: cardinal; const unix_socket: string; clientflag: cardinal): PMYSQL_CON;
var
  mysql: TMySqlSession;
begin
  Result := pmysql;
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    mysql.Connect(host, user, passwd, db, port, unix_socket, clientflag);
  except
    on E: EMySqlException do begin
      Result := nil;
      TMySqlSession(pmysql).errno := E.ErrorCode;
      TMySqlSession(pmysql).error := E.Message;
      TMySqlSession(pmysql).IsServerError := E.IsServerError;
    end;
  end;
end;

procedure TMySQLAPIDirect.mysql_close(pmysql: PMYSQL_CON);
var
  mysql: TMySqlSession;
begin
  if pmysql = nil then
    Exit;

  mysql := TMySqlSession(pmysql);
  try
    mysql.Close;
    if mysql.ResultSet <> nil then
      TMySqlResultSet(mysql.ResultSet).Session := nil;
  except
    on E: Exception do begin
      // silent
    end;
  end;

  try
    mysql.Free;
  except
    on E: EMySqlException do begin
      // silent
    end;
  end;
end;

function TMySQLAPIDirect.mysql_select_db(pmysql: PMYSQL_CON; const db: string): integer;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    mysql.SelectDb(db);
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_real_query(pmysql: PMYSQL_CON; const q: PAnsiChar; _length: cardinal): integer;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    FQueryBufferCS.Acquire;
    try
      mysql.SimpleCommand(scQuery, q, _length, true);
    finally
      FQueryBufferCS.Release;
    end;

    mysql.ReadQueryResult;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_kill(pmysql: PMYSQL_CON; pid: cardinal): integer;
  procedure int4store(buff: TBytes; _pos, val: integer);
  begin
    buff[_pos] := Byte(val and $FF);
    buff[_pos+1] := Byte((val shr 8) and $FF);
    buff[_pos+2] := Byte((val shr 16) and $FF);
    buff[_pos+3] := Byte(val shr 24);
  end;

var
  mysql: TMySqlSession;
  buff: TBytes;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    SetLength(buff, 4);
    int4store(buff, 0, pid);
    mysql.SimpleCommand(scProcessKill, buff, 4, false);
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_ping(pmysql: PMYSQL_CON): integer;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    mysql.Ping;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_get_server_info(pmysql: PMYSQL_CON): string;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    Result := mysql.serverVersion;
  except
    on E: EMySqlException do begin
      Result := '';
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_get_client_info: string;
begin
  Result := MYSQL_CLIENT_VERSION;
end;

function TMySQLAPIDirect.mysql_get_host_info(pmysql: PMYSQL_CON): string;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    Result := mysql.hostInfo;
  except
    on E: EMySqlException do begin
      Result := '';
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_use_result(pmysql: PMYSQL_CON): PMYSQL_RES;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  try
    if Length(mysql.fields) = 0 then begin
      Result := nil;
      Exit;
    end;

    if mysql.status <> msGetResult then
      raise EMySqlException.Create(CR_COMMANDS_OUT_OF_SYNC, [], 'mysql.status = ' + IntToStr(Integer(mysql.status)));

    Result := TMySqlResultSet.Create(mysql);
    mysql.ResultSet := Result;
    mysql.fields := nil;
    mysql.status := msUseResult;
  except
    on E: EMySqlException do begin
      Result := nil;
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; var arg: integer): integer;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    case option of
      MYSQL_OPT_PROTOCOL:
        mysql.protocolType := MYSQL_PROTOCOL_TYPE(arg);
      MYSQL_OPT_NAMED_PIPE:
        mysql.protocolType := MYSQL_PROTOCOL_PIPE;
      MYSQL_OPT_CONNECT_TIMEOUT:
        mysql.connectTimeout := arg;
      MYSQL_OPT_READ_TIMEOUT, MYSQL_OPT_WRITE_TIMEOUT:
        mysql.commandTimeout := arg;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; const arg: string): integer;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    case option of
      MYSQL_SET_CHARSET_NAME:
        mysql.charset := arg;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

procedure TMySQLAPIDirect.mysql_free_result(pres: PMYSQL_RES);
var
  res: TMySqlResultSet;
begin
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);

  try
    if res.Session <> nil then
      res.Session.ResultSet := nil;
    res.ClearBuffer;
  except
    on E: EMySqlException do;
  end;

  res.Free;
end;

function TMySQLAPIDirect.mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW;
var
  res: TMySqlResultSet;
  row: TMySqlBinds;
begin
  Result := nil;
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);
  Assert(res.Session <> nil);
  res.Session.errno := 0;
  // res.Session.error := ''; // performance optimization

  try
    row := res.Binds;
    if res.Fetch(row) then
      Result := row;
  except
    on E: EMySqlException do begin
      res.Session.errno := E.ErrorCode;
      res.Session.error := E.Message;
      res.Session.IsServerError := E.IsServerError;
      if res.Session.serverStatus and SERVER_MORE_RESULTS_EXISTS = 0 then
        res.Session.status := msReady;
    end;
  end;
end;

function TMySQLAPIDirect._mysql_read_row(pres: PMYSQL_RES; Index: integer): PMYSQL_ROW;
var
  res: TMySqlResultSet;
  row: TMySqlBinds;
begin
  Result := nil;
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);
  Assert(res.Session <> nil);
  res.Session.errno := 0;
  // res.Session.error := ''; // performance optimization

  try
    row := res.Binds;
    if res.ReadRow(row, Index) then
      Result := row;
  except
    on E: EMySqlException do begin
      res.Session.errno := E.ErrorCode;
      res.Session.error := E.Message;
      res.Session.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_fetch_lengths(pres: PMYSQL_RES): PMYSQL_LENGTHS;
begin
  Assert(False);
  Result := nil;
end;

function TMySQLAPIDirect.mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer;
begin
  Assert(False);
  Result := 0;
end;

procedure TMySQLAPIDirect.mysql_server_end;
begin
  Assert(False);
end;

// C API Prepared Statements functions
function TMySQLAPIDirect.mysql_stmt_init(pmysql: PMYSQL_CON): PMYSQL_STMT;
begin
  Result := TMySqlStmt.Create(TMySqlSession(pmysql));
end;

function TMySQLAPIDirect.mysql_stmt_prepare(pstmt: PMYSQL_STMT; const q: PAnsiChar; _length: cardinal): integer;
var
  stmt: TMySqlStmt;
begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    FQueryBufferCS.Acquire;
    try
      stmt.Prepare(q, _length);
    finally
      FQueryBufferCS.Release;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := E.Message;
      stmt.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_stmt_param_count(pstmt: PMYSQL_STMT): cardinal;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).ParamCount;
end;

(*function TMySQLAPIDirect.mysql_stmt_bind_param(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND): byte;
var
  stmt: TMySqlStmt;
  bind: TMySqlBinds;
  i: integer;

begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';
  bind := nil;

  try
    try
      bind := TMySqlBinds.Create(stmt.ParamCount);
      for i := 0 to stmt.ParamCount - 1 do begin
        //bind.Binds[i].IsNull := bnd.is_null;
        bind.Binds[i].Offset := bnd.offset;
        bind.Binds[i].Length := bnd.buffer_length;
        bind.Binds[i].Index := i;
        bind.Binds[i]._Type := bnd.buffer_type;

        bnd := PMYSQL_BIND(Integer(bnd) + SizeOf(MYSQL_BIND));
      end;

      stmt.BindParams(bind);
    except
      bind.Free;
      raise;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := E.Message;
    end;
  end;
end;
*)

function TMySQLAPIDirect._mysql_stmt_bind_result(pstmt: PMYSQL_STMT; const bnds: TMYSQL_BINDS): byte;
var
  stmt: TMySqlStmt;
begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    stmt.BindResult();
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := E.Message;
      stmt.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_stmt_field_count(pstmt: PMYSQL_STMT): cardinal;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).FieldCount;
end;

function TMySQLAPIDirect.mysql_stmt_close(pstmt: PMYSQL_STMT): byte;
begin
  TMySqlStmt(pstmt).Free;
  Result := 0;
end;

function TMySQLAPIDirect.mysql_stmt_free_result(pstmt: PMYSQL_STMT): byte;
begin
  TMySqlStmt(pstmt).FreeResult;
  Result := 0;
end;

function TMySQLAPIDirect.mysql_stmt_errno(pstmt: PMYSQL_STMT): cardinal;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).errno;
end;

function TMySQLAPIDirect.mysql_stmt_error(pstmt: PMYSQL_STMT): string;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).error;
end;

function TMySQLAPIDirect.mysql_stmt_isservererror(pstmt: PMYSQL_STMT): boolean;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).IsServerError ;
end;

function TMySQLAPIDirect.mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer;
begin
  Assert(False);
  Result := 0;
end;

function TMySQLAPIDirect._mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; var bnds: TMYSQL_BINDS; column: cardinal; offset: cardinal): integer;
begin
  Assert(False);
  Result := 0;
end;

function TMySQLAPIDirect.mysql_stmt_send_long_data(pstmt: PMYSQL_STMT; param_number: cardinal; const data: PAnsiChar; length: cardinal): byte;
var
  stmt: TMySqlStmt;
begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    stmt.SendLongData(param_number, data, length);
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := E.Message;
      stmt.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).Result;
end;

function TMySQLAPIDirect.mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  Assert(False);
  Result := nil;
end;

function TMySQLAPIDirect.mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): Int64;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).AffectedRows;
end;

{function TMySQLAPIDirect.mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer;
begin
  Assert(False);
  Result := 0;
end;}

function TMySQLAPIDirect.mysql_more_results(pmysql: PMYSQL_CON): byte;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  if mysql.serverStatus and SERVER_MORE_RESULTS_EXISTS <> 0 then
    Result := 1
  else
    Result := 0;
end;

function TMySQLAPIDirect.mysql_next_result(pmysql: PMYSQL_CON): integer;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    if mysql.status <> msReady then
      raise EMySqlException.Create(CR_COMMANDS_OUT_OF_SYNC, [], 'mysql.status = ' + IntToStr(Integer(mysql.status)));

    mysql.affectedRows := -1;
    if mysql.serverStatus and SERVER_MORE_RESULTS_EXISTS <> 0 then begin
      mysql.ReadQueryResult;
      Result := 0;
    end
    else
      Result := -1; // No more results
    except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := E.Message;
      mysql.IsServerError := E.IsServerError;
    end;
  end;
end;

procedure TMySQLAPIDirect._mysql_fetch_lengths(pres: PMYSQL_RES; const Lens: TLenArr);
var
  res: TMySqlResultSet;
  Binds: TMySqlBinds;
  i: integer;
begin
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);

  Assert(Length(Lens) >= res.FieldCount);
  Binds := res.Binds;
  for i := 0 to res.FieldCount - 1 do
    Lens[i] := Binds.Items[i].Length;
end;

function TMySQLAPIDirect._mysql_fetch_value_ptr(pres: PMYSQL_RES; prow: PMYSQL_ROW; fieldnr: cardinal): IntPtr;
var
  res: TMySqlResultSet;
  row: TMySQLBinds;
  _pos: integer;
begin
  Assert(pres <> nil);
  Assert(prow <> nil);

  Result := nil;
  row := TMySqlBinds(prow);
  _pos := row.Items[fieldnr].Offset;
  if _pos = 0 then
    Exit;

  res := TMySqlResultSet(pres);
  Result := PtrOffset(GetAddrOfPinnedObject(res.GCHandleData), _pos);
end;

procedure TMySQLAPIDirect._mysql_fetch_value_to_buff(prow: PMYSQL_ROW; fieldnr: cardinal; const Buff: TBytes; Off: integer; Len: integer);
var
  row: TMySqlBinds; 
  _pos: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  _pos := row.Items[fieldnr].Offset;
  if (_pos = 0) or (Len = 0) then
    Exit;

  CopyBuffer(@row.Buffer[_pos], @Buff[Off], Len);
end;

procedure TMySQLAPIDirect._mysql_fetch_value_to_buff(prow: PMYSQL_ROW; fieldnr: cardinal; pBuff: IntPtr; Len: integer);
var
  row: TMySqlBinds;
  _pos: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  _pos := row.Items[fieldnr].Offset;
  if (_pos = 0) or (Len = 0) then
    Exit;

  CopyBuffer(row.Buffer + _pos, pBuff, Len);
end;

procedure TMySQLAPIDirect._mysql_fetch_value_to_str(prow: PMYSQL_ROW; fieldnr: cardinal; pBuff: IntPtr; Len: integer);
var
  row: TMySqlBinds;
  _pos: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  _pos := row.Items[fieldnr].Offset;
  if (_pos = 0) or (Len = 0) then begin
    Marshal.WriteByte(pBuff, Len, 0);
    Exit;
  end;

  CopyBuffer(row.Buffer + _pos, pBuff, Len);
  Marshal.WriteByte(pBuff, Len, 0);
end;

function TMySQLAPIDirect._mysql_fetch_value_is_null(prow: PMYSQL_ROW; fieldnr: integer): boolean;
var
  row: TMySqlBinds;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  Assert(fieldnr < Length(row.Items));
  Result := row.Items[fieldnr].IsNull;
end;

function TMySQLAPIDirect._mysql_fetch_value_arr(prow: PMYSQL_ROW; fieldnr: cardinal; out Off: integer; Len: integer): TValueArr;
var
  row: TMySqlBinds;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);

  Result := @row.Buffer[0];
  Off := row.Items[fieldnr].Offset;
end;

function TMySQLAPIDirect._mysql_fetch_value_str(prow: PMYSQL_ROW; fieldnr: cardinal): AnsiString;
var
  row: TMySqlBinds;
  bind: TMySqlBind;
  _pos, len: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  bind := row.Items[fieldnr];

  _pos := bind.Offset;
  len := bind.Length;
  Result := Marshal.PtrToStringAnsi(@row.Buffer[_pos], len);
end;

function TMySQLAPIDirect._mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: cardinal; const Unicode: boolean): TMYSQL_FIELD;
var
  res: TMySqlResultSet;
  Field: TMySqlFieldInfo;
  n: integer;
begin
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);
  Assert(fieldnr < cardinal(res.FieldCount));
  Assert(res.Fields <> nil);
  Assert(fieldnr < cardinal(Length(res.Fields)));
  Field := res.Fields[fieldnr];

  Result.Name := Field.Name;
  if Field.OrgName <> '' then
    Result.OrgName := Field.OrgName
  else
    Result.OrgName := Field.Name; // Server version 3.23
  Result.Table := Field.Table;
  Result.OrgTable := Field.OrgTable;

  Result.Flags := cardinal(Field.Flags);
  Result.MyType := Field.MyType;
  Result.Decimals := Field.Scale;
  Result.CharsetNr := Field.CharsetNr;
  Result.LengthInBytes := Field.Length;

(*
  c_char VARCHAR(100) CHARACTER SET cp1251 COLLATE cp1251_general_ci
  -----------
           SELECT c_char   Field.Length = 100
  prepared SELECT c_char   Field.Length = 100
           SELECT ''abc''  Field.Length = 9
           SELECT N''abc'' Field.Length = 9
  prepared SELECT ''abc''  Field.Length = 9
*)       

  if Unicode and (Result.Table <> '') and not res.Session.protocol41 then
    case Field.MyType of
      FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, FIELD_TYPE_VARCHAR, FIELD_TYPE_ENUM, FIELD_TYPE_SET, FIELD_TYPE_JSON:
      begin // to prevent F2084 Internal Error: C6662 in BDS 2005
        n := Result.LengthInBytes * MaxUTF8CharLen;
        Result.LengthInBytes := n;
      end;
    end;
end;

function TMySQLAPIDirect._mysql_fetch_options(pres: PMYSQL_RES; option: TMySqlFecthOption; arg: integer): integer;
var
  res: TMySqlResultSet;
begin
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);

  case option of
    foKeepRowCount: begin
      if res.Session.net.RowCount <> arg then
        res.Session.net.RowCount := arg;
      Result := 0;
    end;
  else
    Assert(False);
    Result := -1;
  end;
end;

function TMySQLAPIDirect._mysql_stmt_execute(pstmt: PMYSQL_STMT; Command: TCRCommand; const Unicode: boolean): integer;
var
  stmt: TMySqlStmt;
begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    stmt.Execute(Command, Unicode);
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := E.Message;
      stmt.IsServerError := E.IsServerError;
    end;
  end;
end;

function TMySQLAPIDirect._mysql_stmt_fetch(pstmt: PMYSQL_STMT; out row: PMYSQL_ROW): integer;
var
  stmt: TMySqlStmt;
  binds: TMySqlBinds;
begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    Result := stmt.Fetch(binds);
    row := binds;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := E.Message;
      stmt.IsServerError := E.IsServerError;
      if stmt.Session.serverStatus and SERVER_MORE_RESULTS_EXISTS = 0 then
        stmt.Session.status := msReady;
    end;
  end;
end;

procedure TMySQLAPIDirect._mysql_stmt_fetch_lengths(prow: PMYSQL_ROW; const Lens: TLenArr);
var
  row: TMySQLBinds;
  i: integer;
begin
  row := TMySqlBinds(prow);

  Assert(Length(Lens) >= Length(row.Items));
  for i := 0 to Length(row.Items) - 1 do
    Lens[i] := row.Items[i].Length;
end;

function TMySQLAPIDirect._mysql_stmt_fetch_value_ptr(pstmt: PMYSQL_STMT; fieldnr: cardinal): IntPtr;
var
  stmt: TMySqlStmt;
  _pos: integer;
begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);

  Result := nil;
  _pos := stmt.Result.Binds.Items[fieldnr].Offset;
  if _pos = 0 then
    Exit;

  Result := PtrOffset(GetAddrOfPinnedObject(stmt.Result.GCHandleData), _pos);
end;

procedure TMySQLAPIDirect.CheckMySQLLib;
begin
  FClientStructVer := cvDirect;
  FClientVer := mysql_get_client_info;
end;

procedure TMySQLAPIDirect.SetIOHandler(pmysql: PMYSQL_CON; IOHandler: TCRIOHandler);
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.IOHandler := IOHandler;
end;

procedure TMySQLAPIDirect.SetOptions(pmysql: PMYSQL_CON; HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
  SSLOptions: TSSLOptions);
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.HttpOptions := HttpOptions;
  mysql.ProxyOptions := ProxyOptions;
  mysql.SSLOptions := SSLOptions;
end;

procedure TMySQLAPIDirect.SetIPVersion(pmysql: PMYSQL_CON; IPVersion: TIPVersion);
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.IPVersion := IPVersion;
end;

procedure TMySQLAPIDirect.SetOnDialogAuthPlugin(pmysql: PMYSQL_CON; OnDialogAuthPlugin: TMyDialogAuthPluginEvent);
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.OnDialogAuthPlugin := OnDialogAuthPlugin;
end;

procedure TMySQLAPIDirect.SetIsUTF8(pmysql: PMYSQL_CON; Value: boolean);
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.SetIsUTF8(Value);
end;

initialization
  MyAPIDirect := TMySQLAPIDirect.Create;

finalization
{$IFNDEF LITE}
  ClearPoolManager;
{$ENDIF}
  MyAPIDirect.Free;

end.
