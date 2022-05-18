
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoCallUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFDEF POSIX}
  Posix.Dlfcn,
{$ENDIF}
{$IFDEF UNIX}
  dl,
{$ENDIF}
  CRTypes, CRJson,
{$IFNDEF UNIDACPRO}
  MongoConsts, MongoJson;
{$ELSE}
  MongoConstsUni, MongoJsonUni;
{$ENDIF}

const
  DACProductName = 'MongoDAC';

  MongoDLLName = {$IFDEF MSWINDOWS}
                   'libmongoc-1.0.dll';
                 {$ELSE}{$IFDEF MACOS}
                   'libmongoc-1.0.0.dylib';
                 {$ELSE}
                   'libmongoc-1.0.so.0';
                 {$ENDIF}
                 {$ENDIF}

  BSONDLLName = {$IFDEF MSWINDOWS}
                  'libbson-1.0.dll';
                {$ELSE}{$IFDEF MACOS}
                  'libbson-1.0.0.dylib';
                {$ELSE}
                  'libbson-1.0.so.0';
                {$ENDIF}
                {$ENDIF}

  MONGOC_QUERY_NONE              = 0;
  MONGOC_QUERY_TAILABLE_CURSOR   = 2;
  MONGOC_QUERY_SLAVE_OK          = 4;
  MONGOC_QUERY_OPLOG_REPLAY      = 8;
  MONGOC_QUERY_NO_CURSOR_TIMEOUT = 16;
  MONGOC_QUERY_AWAIT_DATA        = 32;
  MONGOC_QUERY_EXHAUST           = 64;
  MONGOC_QUERY_PARTIAL           = 128;

  MONGOC_UPDATE_NONE             = 0;
  MONGOC_UPDATE_UPSERT           = 1;
  MONGOC_UPDATE_MULTI_UPDATE     = 2;

  MONGOC_INSERT_NONE             = 0;

  MONGOC_REMOVE_NONE             = 0;
  MONGOC_REMOVE_SINGLE_REMOVE    = 1;

  MONGOC_WRITE_CONCERN_W_UNACKNOWLEDGED = 0;
  MONGOC_WRITE_CONCERN_W_DEFAULT = 1;

  BSON_DECIMAL128_STRING = 43;

type
  uint8_t = byte;
  puint8_t = ^uint8_t;
  uint32_t = Cardinal;
  size_t = NativeUInt;
  psize_t = ^size_t;
  ssize_t = NativeInt;
  int32_t = integer;
  int64_t = Int64;

  pmongoc_client_t = IntPtr;
  pmongoc_database_t = IntPtr;
  pmongoc_collection_t = IntPtr;
  pmongoc_cursor_t = IntPtr;
  pmongoc_read_prefs_t = IntPtr;
  pmongoc_write_concern_t = IntPtr;

  mongoc_query_flags_t = Cardinal;
  mongoc_update_flags_t = Cardinal;
  mongoc_remove_flags_t = Cardinal;

  pbson_t = ^bson_t;
  ppbson_t = ^pbson_t;
  bson_t = record
    flags: uint32_t;
    len: uint32_t;
    padding: array [0..119] of byte;
  end;

  pbson_error_t = ^bson_error_t;
  bson_error_t = record
    domain: Cardinal;
    code: Cardinal;
    message: array[0..503] of byte;
  end;

  pbson_oid_t = ^bson_oid_t;
  bson_oid_t = record
   bytes: TJSONOid;
  end;

  pbson_context_t = IntPtr;

  pbson_iter_t = ^bson_iter_t;
  bson_iter_t = record
    raw: PByte;         // The raw buffer being iterated.
    len: Cardinal;      // The length of raw.
    off: Cardinal;      // The offset within the buffer.
    _type: Cardinal;    // The offset of the type byte.
    key: Cardinal;      // The offset of the key byte.
    d1: Cardinal;       // The offset of the first data byte.
    d2: Cardinal;       // The offset of the second data byte.
    d3: Cardinal;       // The offset of the third data byte.
    d4: Cardinal;       // The offset of the fourth data byte.
    next_off: Cardinal; // The offset of the next field.
    err_off: Cardinal;  // The offset of the error.
    padding: array [0 .. 83] of Byte // bson_value_t   value;    // Internal value for various state.
  end;

  // client

  Tmongoc_init = procedure; cdecl;
  Tmongoc_cleanup = procedure; cdecl;

  Tmongoc_client_new = function(
    uri_string: IntPtr
  ): pmongoc_client_t; cdecl;

  Tmongoc_client_destroy = procedure(
    client: pmongoc_client_t
  ); cdecl;

  Tmongoc_client_get_server_status = function(
    client: pmongoc_client_t;
    read_prefs: pmongoc_read_prefs_t;
    reply: pbson_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tmongoc_get_version = function: PAnsiChar; cdecl;

  Tmongoc_client_command_simple = function(
    client: pmongoc_client_t;
    db_name: PAnsiChar;
    command: pbson_t;
    read_prefs: pmongoc_read_prefs_t;
    reply: pbson_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tmongoc_client_command = function(
    client: pmongoc_client_t;
    db_name: PAnsiChar;
    flags: mongoc_query_flags_t;
    skip,
    limit,
    batch_size: uint32_t;
    query,
    fields: pbson_t;
    read_prefs: pmongoc_read_prefs_t
  ): pmongoc_cursor_t; cdecl;

  Tmongoc_client_find_databases = function(
    client: pmongoc_client_t;
    error: pbson_error_t
  ): pmongoc_cursor_t; cdecl;

  Tmongoc_client_get_database = function(
    client: pmongoc_client_t;
    name: IntPtr
  ): pmongoc_database_t; cdecl;

  Tmongoc_client_get_collection = function(
    client: pmongoc_client_t;
    db: PAnsiChar;
    collection: PAnsiChar
  ): pmongoc_collection_t; cdecl;

  // database

   Tmongoc_database_destroy = procedure(
     database: pmongoc_database_t
   ); cdecl;

  Tmongoc_database_command = function(
    database: pmongoc_database_t;
    flags: mongoc_query_flags_t;
    skip,
    limit,
    batch_size: uint32_t;
    command,
    fields: pbson_t;
    read_prefs: pmongoc_read_prefs_t
    ): pmongoc_cursor_t; cdecl;

  Tmongoc_database_command_simple = function(
    database: pmongoc_database_t;
    command: pbson_t;
    read_prefs: pmongoc_read_prefs_t;
    reply: pbson_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tmongoc_database_get_collection = function(
    database: pmongoc_database_t;
    const name: PAnsiChar
  ): pmongoc_collection_t; cdecl;

  Tmongoc_database_find_collections = function(
    database: pmongoc_database_t;
    const filter: pbson_t;
    error: pbson_error_t
  ): pmongoc_cursor_t; cdecl;

  // write concern

  Tmongoc_write_concern_new = function: pmongoc_write_concern_t; cdecl;
  Tmongoc_write_concern_set_w = procedure(
    write_concern: pmongoc_write_concern_t;
    w: int32_t
  ); cdecl;
  Tmongoc_write_concern_destroy = procedure(
    write_concern: pmongoc_write_concern_t
  ); cdecl;

  // collection

  Tmongoc_collection_destroy = procedure(
    collection: pmongoc_collection_t
  ); cdecl;

  Tmongoc_collection_command = function(
    collection: pmongoc_collection_t;
    flags: mongoc_query_flags_t;
    skip,
    limit,
    batch_size: uint32_t;
    command,
    fields: pbson_t;
    read_prefs: pmongoc_read_prefs_t
  ): pmongoc_cursor_t; cdecl;

  Tmongoc_collection_find = function(
    collection: pmongoc_collection_t;
    flags: mongoc_query_flags_t;
    skip,
    limit,
    batch_size: uint32_t;
    query,
    fields: pbson_t;
    read_prefs: pmongoc_read_prefs_t
  ): pmongoc_cursor_t; cdecl;

  Tmongoc_collection_update = function(
    collection: pmongoc_collection_t;
    flags: mongoc_update_flags_t;
    selector,
    update: pbson_t;
    write_concern: pmongoc_write_concern_t;
    error: Pbson_error_t
  ): boolean; cdecl;

  Tmongoc_collection_insert = function(
    collection: pmongoc_collection_t;
    flags: mongoc_remove_flags_t;
    document: pbson_t;
    write_concern: pmongoc_write_concern_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tmongoc_collection_save = function(
    collection: pmongoc_collection_t;
    document: pbson_t;
    write_concern: pmongoc_write_concern_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tmongoc_collection_remove = function(
    collection: pmongoc_collection_t;
    flags: mongoc_remove_flags_t;
    selector: pbson_t;
    write_concern: pmongoc_write_concern_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tmongoc_collection_find_indexes = function(
    collection: pmongoc_collection_t;
    error: pbson_error_t
  ): pmongoc_cursor_t; cdecl;

  Tmongoc_collection_drop = function(
    collection: pmongoc_collection_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tmongoc_collection_get_last_error = function(
    collection: pmongoc_collection_t
  ): pbson_t;

  // cursor

  Tmongoc_cursor_get_id = function(
    cursor: pmongoc_cursor_t
  ): int64_t; cdecl;

  Tmongoc_cursor_is_alive = function(
    cursor: pmongoc_cursor_t
  ): boolean; cdecl;

  Tmongoc_cursor_more = function(
    cursor: pmongoc_cursor_t
  ): boolean; cdecl;

  Tmongoc_cursor_current = function(
    cursor: pmongoc_cursor_t
  ): pbson_t; cdecl;

  Tmongoc_cursor_next = function(
    cursor: pmongoc_cursor_t;
    bson: ppbson_t
  ): boolean; cdecl;

  Tmongoc_cursor_error = function(
    cursor: pmongoc_cursor_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tmongoc_cursor_destroy = procedure(
    cursor: pmongoc_cursor_t
  ); cdecl;

  // bson

  Tbson_new = function: pbson_t; cdecl;

  Tbson_new_from_json = function(
    data: PAnsiChar;
    len: ssize_t;
    error: pbson_error_t
  ): pbson_t; cdecl;

  Tbson_init = procedure(
    b: pbson_t
  ); cdecl;

  Tbson_init_static = function(
    b: pbson_t;
    data: IntPtr;
    len: size_t
  ): boolean; cdecl;

  Tbson_init_from_json = function(
    bson: pbson_t;
    data: PAnsiChar;
    len: size_t;
    error: pbson_error_t
  ): boolean; cdecl;

  Tbson_destroy = procedure(
    bson: pbson_t
  ); cdecl;

  Tbson_get_data = function(
    bson: pbson_t
  ): IntPtr; cdecl;

  Tbson_as_json = function(
    bson: pbson_t;
    length: psize_t
  ): IntPtr; cdecl;

  Tbson_decimal128_to_string = procedure(
    const dec: PBinaryDecimal128;
    str: PAnsiChar
  ); cdecl;

  Tbson_decimal128_from_string = function(
    const str: PAnsiChar;
    dec: PBinaryDecimal128
  ): boolean; cdecl;

  // oid

  Tbson_oid_init = procedure(
    oid: pbson_oid_t;
    context: pbson_context_t
  ); cdecl;

  // visitor

  Tvisit_before = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_after = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_corrupt = procedure(
    const iter: pbson_iter_t;
    data: IntPtr
  ); cdecl;

  Tvisit_double = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_double: Double;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_utf8 = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_utf8_len: size_t;
    v_utf8: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_document = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    const v_document: pbson_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_array = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    const v_array: pbson_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_binary = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_subtype: TJSONBinarySubtype;
    v_binary_len: size_t;
    v_binary: puint8_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_undefined = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_oid = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    const v_oid: pbson_oid_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_bool = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_bool: boolean;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_date_time = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    msec_since_epoch: int64_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_null = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_regex = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    const v_regex: PAnsiChar;
    const v_options: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_dbpointer = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_collection_len: size_t;
    const v_collection: PAnsiChar;
    const v_oid: pbson_oid_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_code = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_code_len: size_t;
    const v_code: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_symbol = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_symbol_len: size_t;
    const v_symbol: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_codewscope = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_code_len: size_t;
    const v_code: PAnsiChar;
    const v_scope: pbson_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_int32 = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_int32: int32_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_timestamp = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_timestamp: uint32_t;
    v_increment: uint32_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_int64 = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    v_int64: int64_t;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_maxkey = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  Tvisit_minkey = function(
    const iter: pbson_iter_t;
    const key: PAnsiChar;
    data: IntPtr
  ): boolean; cdecl;

  pbson_visitor_t = ^bson_visitor_t;
  bson_visitor_t = record
    visit_before: Tvisit_before;
    visit_after: Tvisit_after;
    visit_corrupt: Tvisit_corrupt;
    visit_double: Tvisit_double;
    visit_utf8: Tvisit_utf8;
    visit_document: Tvisit_document;
    visit_array: Tvisit_array;
    visit_binary: Tvisit_binary;
    visit_undefined: Tvisit_undefined;
    visit_oid: Tvisit_oid;
    visit_bool: Tvisit_bool;
    visit_date_time: Tvisit_date_time;
    visit_null: Tvisit_null;
    visit_regex: Tvisit_regex;
    visit_dbpointer: Tvisit_dbpointer;
    visit_code: Tvisit_code;
    visit_symbol: Tvisit_symbol;
    visit_codewscope: Tvisit_codewscope;
    visit_int32: Tvisit_int32;
    visit_timestamp: Tvisit_timestamp;
    visit_int64: Tvisit_int64;
    visit_maxkey: Tvisit_maxkey;
    visit_minkey: Tvisit_minkey;
    padding: array[0..8] of IntPtr;
  end;

  // iterator

  Tbson_iter_init = function(
    iter: pbson_iter_t;
    bson: pbson_t
  ): boolean; cdecl;

  Tbson_iter_next = function(
    iter: pbson_iter_t
  ): boolean; cdecl;

  TMongoAPI = class
  private
  {$IFDEF MSWINDOWS}
    FClientLibHandle,
    FBSONLibHandle: HMODULE;
  {$ENDIF}
  {$IFDEF POSIX}
    FClientLibHandle,
    FBSONLibHandle: NativeUInt;
  {$ENDIF}
  {$IFDEF UNIX}
    FClientLibHandle,
    FBSONLibHandle: IntPtr;
  {$ENDIF}

    FClientLibrary,
    FBSONLibrary: string;
    FInitialized: boolean;

    procedure LoadLibraries(const LibraryName, BSONLibraryName: string);
    procedure UnLoadLibraries;

  public
    Deserializer: TJSONDeserializer;
    CommandDeserializer: TCommandDeserializer;
    Serializer: TCommandSerializer;

    mongoc_init: Tmongoc_init;
    mongoc_cleanup: Tmongoc_cleanup;

    mongoc_client_new: Tmongoc_client_new;
    mongoc_client_destroy: Tmongoc_client_destroy;
    mongoc_client_get_server_status: Tmongoc_client_get_server_status;
    mongoc_get_version: Tmongoc_get_version;
    mongoc_client_command_simple: Tmongoc_client_command_simple;
    mongoc_client_command: Tmongoc_client_command;
    mongoc_client_find_databases: Tmongoc_client_find_databases;
    mongoc_client_get_database: Tmongoc_client_get_database;

    mongoc_database_destroy: Tmongoc_database_destroy;
    mongoc_database_command_simple: Tmongoc_database_command_simple;
    mongoc_database_command: Tmongoc_database_command;
    mongoc_database_get_collection: Tmongoc_database_get_collection;
    mongoc_database_find_collections: Tmongoc_database_find_collections;

    mongoc_write_concern_new: Tmongoc_write_concern_new;
    mongoc_write_concern_set_w: Tmongoc_write_concern_set_w;
    mongoc_write_concern_destroy: Tmongoc_write_concern_destroy;

    mongoc_collection_destroy: Tmongoc_collection_destroy;
    mongoc_collection_command: Tmongoc_collection_command;
    mongoc_collection_find: Tmongoc_collection_find;
    mongoc_collection_update: Tmongoc_collection_update;
    mongoc_collection_insert: Tmongoc_collection_insert;
    mongoc_collection_save: Tmongoc_collection_save;
    mongoc_collection_remove: Tmongoc_collection_remove;
    mongoc_collection_find_indexes: Tmongoc_collection_find_indexes;
    mongoc_collection_drop: Tmongoc_collection_drop;
    mongoc_collection_get_last_error: Tmongoc_collection_get_last_error;

    mongoc_cursor_get_id: Tmongoc_cursor_get_id;
    mongoc_cursor_is_alive: Tmongoc_cursor_is_alive;
    mongoc_cursor_more: Tmongoc_cursor_more;
    mongoc_cursor_current: Tmongoc_cursor_current;
    mongoc_cursor_next: Tmongoc_cursor_next;
    mongoc_cursor_error: Tmongoc_cursor_error;
    mongoc_cursor_destroy: Tmongoc_cursor_destroy;

    bson_new: Tbson_new;
    bson_new_from_json: Tbson_new_from_json;
    bson_init: Tbson_init;
    bson_init_static: Tbson_init_static;
    bson_init_from_json: Tbson_init_from_json;

    bson_destroy: Tbson_destroy;

    bson_get_data: Tbson_get_data;

    bson_as_json: Tbson_as_json;

    bson_decimal128_to_string: Tbson_decimal128_to_string;
    bson_decimal128_from_string: Tbson_decimal128_from_string;

    bson_oid_init: Tbson_oid_init;

    bson_iter_init: Tbson_iter_init;
    bson_iter_next: Tbson_iter_next;

    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure UnInitialize;
    procedure Assign(const Source: TMongoAPI);

    function GetProc(const Name: string; NotLinkPtr: IntPtr; const BSON: boolean = False): IntPtr; overload;
    function GetProc(const Name: string; const BSON: boolean = False): IntPtr; overload;

    property ClientLibrary: string read FClientLibrary write FClientLibrary;
    property BSONLibrary: string read FBSONLibrary write FBSONLibrary;
    property Initialized: boolean read FInitialized;
  end;

var
  LockInit: TCriticalSection;

procedure NotLink;

implementation

procedure NotLink;
begin
  raise Exception.Create(SFunctionNotLinked);
end;

procedure InitFunctions(const API: TMongoAPI);
begin
  API.mongoc_init := API.GetProc('mongoc_init');
  API.mongoc_cleanup := API.GetProc('mongoc_cleanup');

  API.mongoc_client_new := API.GetProc('mongoc_client_new');
  API.mongoc_client_destroy := API.GetProc('mongoc_client_destroy');
  API.mongoc_client_get_server_status := API.GetProc('mongoc_client_get_server_status');
  API.mongoc_get_version := API.GetProc('mongoc_get_version');
  API.mongoc_client_command_simple := API.GetProc('mongoc_client_command_simple');
  API.mongoc_client_command := API.GetProc('mongoc_client_command');
  API.mongoc_client_find_databases := API.GetProc('mongoc_client_find_databases');
  API.mongoc_client_get_database := API.GetProc('mongoc_client_get_database');

  API.mongoc_database_destroy := API.GetProc('mongoc_database_destroy');
  API.mongoc_database_command_simple := API.GetProc('mongoc_database_command_simple');
  API.mongoc_database_command := API.GetProc('mongoc_database_command');
  API.mongoc_database_get_collection := API.GetProc('mongoc_database_get_collection');
  API.mongoc_database_find_collections := API.GetProc('mongoc_database_find_collections');

  API.mongoc_write_concern_new := API.GetProc('mongoc_write_concern_new');
  API.mongoc_write_concern_set_w := API.GetProc('mongoc_write_concern_set_w');
  API.mongoc_write_concern_destroy := API.GetProc('mongoc_write_concern_destroy');

  API.mongoc_collection_destroy := API.GetProc('mongoc_collection_destroy');
  API.mongoc_collection_command := API.GetProc('mongoc_collection_command');
  API.mongoc_collection_find := API.GetProc('mongoc_collection_find');
  API.mongoc_collection_update := API.GetProc('mongoc_collection_update');
  API.mongoc_collection_insert := API.GetProc('mongoc_collection_insert');
  API.mongoc_collection_save := API.GetProc('mongoc_collection_save');
  API.mongoc_collection_remove := API.GetProc('mongoc_collection_remove');
  API.mongoc_collection_find_indexes := API.GetProc('mongoc_collection_find_indexes');
  API.mongoc_collection_drop := API.GetProc('mongoc_collection_drop');
  API.mongoc_collection_get_last_error := API.GetProc('mongoc_collection_get_last_error');

  API.mongoc_cursor_get_id := API.GetProc('mongoc_cursor_get_id');
  API.mongoc_cursor_is_alive := API.GetProc('mongoc_cursor_is_alive');
  API.mongoc_cursor_more := API.GetProc('mongoc_cursor_more');
  API.mongoc_cursor_current := API.GetProc('mongoc_cursor_current');
  API.mongoc_cursor_next := API.GetProc('mongoc_cursor_next');
  API.mongoc_cursor_error := API.GetProc('mongoc_cursor_error');
  API.mongoc_cursor_destroy := API.GetProc('mongoc_cursor_destroy');

  API.bson_new := API.GetProc('bson_new', True);
  API.bson_new_from_json := API.GetProc('bson_new_from_json', True);
  API.bson_init := API.GetProc('bson_init', True);
  API.bson_init_static := API.GetProc('bson_init_static', True);
  API.bson_init_from_json := API.GetProc('bson_init_from_json', True);
  API.bson_destroy := API.GetProc('bson_destroy', True);
  API.bson_get_data := API.GetProc('bson_get_data', True);
  API.bson_as_json := API.GetProc('bson_as_json', True);

  API.bson_decimal128_to_string := API.GetProc('bson_decimal128_to_string', True);
  API.bson_decimal128_from_string := API.GetProc('bson_decimal128_from_string', True);

  API.bson_oid_init := API.GetProc('bson_oid_init', True);

  API.bson_iter_init := API.GetProc('bson_iter_init', True);
  API.bson_iter_next := API.GetProc('bson_iter_next', True);
end;

{ TMongoAPI }

constructor TMongoAPI.Create;
begin
  inherited;

{$IFDEF UNIX}
  FClientLibHandle := nil;
  FBSONLibHandle := nil;
{$ELSE}
  FClientLibHandle := 0;
  FBSONLibHandle := 0;
{$ENDIF}

  FInitialized := False;

  Deserializer := TJSONDeserializer.Create;
  CommandDeserializer := TCommandDeserializer.Create;
  Serializer := TCommandSerializer.Create;
end;

destructor TMongoAPI.Destroy;
begin
  if FInitialized then
    UnInitialize;

  Deserializer.Free;
  CommandDeserializer.Free;
  Serializer.Free;

  inherited;
end;

procedure TMongoAPI.LoadLibraries(const LibraryName, BSONLibraryName: string);
begin
{$IFDEF MSWINDOWS}
  FClientLibHandle := LoadLibraryEx(PChar(LibraryName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  FBSONLibHandle := LoadLibraryEx(PChar(BSONLibraryName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
{$ELSE}
  FClientLibHandle := dlopen(PAnsiChar(AnsiString(LibraryName)), RTLD_LAZY);
  FBSONLibHandle := dlopen(PAnsiChar(AnsiString(BSONLibraryName)), RTLD_LAZY);
{$ENDIF}
end;

procedure TMongoAPI.UnLoadLibraries;
begin
{$IFDEF MSWINDOWS}
  FreeLibrary(FClientLibHandle);
  FClientLibHandle := 0;
  if FBSONLibHandle <> 0 then
    FreeLibrary(FBSONLibHandle);
  FBSONLibHandle := 0;
{$ENDIF}
{$IFDEF POSIX}
  dlclose(FClientLibHandle);
  FClientLibHandle := 0;
  if FBSONLibHandle <> 0 then
    dlclose(FBSONLibHandle);
  FBSONLibHandle := 0;
{$ENDIF}
{$IFDEF UNIX}
  dlclose(FClientLibHandle);
  FClientLibHandle := nil;
  if FBSONLibHandle <> nil then
    dlclose(FBSONLibHandle);
  FBSONLibHandle := nil;
{$ENDIF}
end;

procedure TMongoAPI.Initialize;
var
  LibraryPath,
  BSONPath: string;
begin
  if Assigned(LockInit) then
    LockInit.Enter;

  try
    if FInitialized then
      Exit;

    LibraryPath := FClientLibrary;
    BSONPath := FBSONLibrary;

    if LibraryPath = '' then
      LibraryPath := MongoDLLName;
    if BSONPath = '' then
      BSONPath := BSONDLLName;

    LoadLibraries(LibraryPath, BSONPath);


    if FClientLibHandle = {$IFNDEF UNIX}0{$ELSE}nil{$ENDIF} then
      raise Exception.Create('Cannot load client library: ' + LibraryPath{$IFNDEF UNIX} + ' (' + SysErrorMessage(GetLastError) + ')'{$ENDIF});
    if FBSONLibHandle = {$IFNDEF UNIX}0{$ELSE}nil{$ENDIF} then
      raise Exception.Create('Cannot load BSON library: ' + BSONPath{$IFNDEF UNIX} + ' (' + SysErrorMessage(GetLastError) + ')'{$ENDIF});

    InitFunctions(Self);
    mongoc_init;

    FInitialized := True;
  finally
    if Assigned(LockInit) then
      LockInit.Leave;
  end;
end;

procedure TMongoAPI.UnInitialize;
begin
  if Assigned(LockInit) then
    LockInit.Enter;
  try
    if not FInitialized then
      Exit;

    if FClientLibHandle <> {$IFNDEF UNIX}0{$ELSE}nil{$ENDIF} then
      UnLoadLibraries;
  finally
    if Assigned(LockInit) then
      LockInit.Leave;
  end;
end;

procedure TMongoAPI.Assign(const Source: TMongoAPI);
begin
  mongoc_init := Source.mongoc_init;
  mongoc_cleanup := Source.mongoc_cleanup;

  mongoc_client_new := Source.mongoc_client_new;
  mongoc_client_destroy := Source.mongoc_client_destroy;
  mongoc_client_get_server_status := Source.mongoc_client_get_server_status;
  mongoc_get_version := Source.mongoc_get_version;
  mongoc_client_command_simple := Source.mongoc_client_command_simple;
  mongoc_client_command := Source.mongoc_client_command;
  mongoc_client_find_databases := Source.mongoc_client_find_databases;
  mongoc_client_get_database := Source.mongoc_client_get_database;

  mongoc_database_destroy := Source.mongoc_database_destroy;
  mongoc_database_command_simple := Source.mongoc_database_command_simple;
  mongoc_database_command := Source.mongoc_database_command;
  mongoc_database_get_collection := Source.mongoc_database_get_collection;

  mongoc_write_concern_new := Source.mongoc_write_concern_new;
  mongoc_write_concern_set_w := Source.mongoc_write_concern_set_w;
  mongoc_write_concern_destroy := Source.mongoc_write_concern_destroy;

  mongoc_collection_destroy := Source.mongoc_collection_destroy;
  mongoc_collection_command := Source.mongoc_collection_command;
  mongoc_collection_find := Source.mongoc_collection_find;
  mongoc_collection_update := Source.mongoc_collection_update;
  mongoc_collection_insert := Source.mongoc_collection_insert;
  mongoc_collection_save := Source.mongoc_collection_save;
  mongoc_collection_remove := Source.mongoc_collection_remove;
  mongoc_collection_find_indexes := Source.mongoc_collection_find_indexes;
  mongoc_collection_drop := Source.mongoc_collection_drop;
  mongoc_collection_get_last_error := Source.mongoc_collection_get_last_error;

  mongoc_cursor_get_id := Source.mongoc_cursor_get_id;
  mongoc_cursor_is_alive := Source.mongoc_cursor_is_alive;
  mongoc_cursor_more := Source.mongoc_cursor_more;
  mongoc_cursor_current := Source.mongoc_cursor_current;
  mongoc_cursor_next := Source.mongoc_cursor_next;
  mongoc_cursor_error := Source.mongoc_cursor_error;
  mongoc_cursor_destroy := Source.mongoc_cursor_destroy;

  bson_new := Source.bson_new;
  bson_new_from_json := Source.bson_new_from_json;
  bson_init := Source.bson_init;
  bson_init_static := Source.bson_init_static;
  bson_init_from_json := Source.bson_init_from_json;
  bson_destroy := Source.bson_destroy;
  bson_get_data := Source.bson_get_data;
  bson_as_json := Source.bson_as_json;

  bson_oid_init := Source.bson_oid_init;

  bson_iter_init := Source.bson_iter_init;
  bson_iter_next := Source.bson_iter_next;
end;

function TMongoAPI.GetProc(const Name: string; NotLinkPtr: IntPtr; const BSON: boolean = False): IntPtr;
begin
  if not BSON then
  {$IFDEF MSWINDOWS}
    Result := GetProcAddress(FClientLibHandle, PChar(Name))
  {$ELSE}
    Result := dlsym(FClientLibHandle, PAnsiChar(AnsiString(Name)))
  {$ENDIF}
  else
  {$IFDEF MSWINDOWS}
    Result := GetProcAddress(FBSONLibHandle, PChar(Name));
  {$ELSE}
    Result := dlsym(FBSONLibHandle, PAnsiChar(AnsiString(Name)));
  {$ENDIF}
  if Result = nil then
    Result := NotLinkPtr;
end;

function TMongoAPI.GetProc(const Name: string; const BSON: boolean = False): IntPtr;
begin
  Result := GetProc(Name, @NotLink, BSON and (FBSONLibHandle <> {$IFNDEF UNIX}0{$ELSE}nil{$ENDIF}));
end;

initialization
  LockInit := TCriticalSection.Create;

finalization
  FreeAndNil(LockInit);

end.
