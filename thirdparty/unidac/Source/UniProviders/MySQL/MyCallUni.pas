
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright � 1998-2021 Devart. All right reserved.
//  MySQL Interface
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MyCallUni;

interface

{$A+} // for TMYSQL_FIELD3 {$ALIGN 4}

uses
  DateUtils, SysUtils,
  CLRClasses, MemUtils, CRTypes, CRFunctions,
  {$IFNDEF UNIDACPRO}MyConsts{$ELSE}MyConstsUni{$ENDIF};


const
  LOCAL_HOST              = 'localhost';
  LOCAL_HOST_NAMEDPIPE    = '.';
  CLIENT_NET_READ_TIMEOUT = 365*24*3600;

const
  NOT_NULL_FLAG          = $1;       // Field can't be NULL
  PRI_KEY_FLAG           = $2;       // Field is part of a primary key
  UNIQUE_KEY_FLAG        = $4;       // Field is part of a unique key
  MULTIPLE_KEY_FLAG      = $8;       // Field is part of a key
  BLOB_FLAG              = $10;      // Field is a blob
  UNSIGNED_FLAG          = $20;      // Field is unsigned
  ZEROFILL_FLAG          = $40;      // Field is zerofill
  BINARY_FLAG            = $80;      // Field is binary

  // The following are only sent to new clients

  ENUM_FLAG              = $100;     // Field is an enum
  AUTO_INCREMENT_FLAG    = $200;     // Field is a autoincrement field
  TIMESTAMP_FLAG         = $400;     // Field is a timestamp
  SET_FLAG               = $800;     // Field is a set
  ON_UPDATE_NOW_FLAG     = $2000;    // Field is set to NOW on UPDATE.
  NUM_FLAG               = $8000;    // Field is num (for clients)
  PART_KEY_FLAG          = $4000;    // Intern; Part of some key
  GROUP_FLAG             = $8000;    // Intern: Group field
  UNIQUE_FLAG            = $10000;   // Intern: Used by sql_yacc
{
  REFRESH_GRANT          = 1;        // Refresh grant tables
  REFRESH_LOG            = 2;        // Start on new log file
  REFRESH_TABLES         = 4;        // Close all tables
  REFRESH_HOSTS          = 8;        // Flush host cache
  REFRESH_STATUS         = 16;       // Flush status variables
  REFRESH_THREADS        = 32;       // Flush status variables
  REFRESH_SLAVE          = 64;       // Reset master info and restart slave thread
  REFRESH_MASTER         = 128;      // Remove all bin logs in the index
                                     // and truncate the index

  // The following can't be set with mysql_refresh()

  REFRESH_READ_LOCK      = 16384;    // Lock tables for read
  REFRESH_FAST           = 32768;    // Intern flag
}
  CLIENT_LONG_PASSWORD     = $1;     // new more secure passwords
  CLIENT_FOUND_ROWS        = $2;     // Found instead of affected rows
  CLIENT_LONG_FLAG         = $4;     // Get all column flags
  CLIENT_CONNECT_WITH_DB   = $8;     // One can specify db on connect
  CLIENT_NO_SCHEMA         = $10;    // Don't allow database.table.column
  CLIENT_COMPRESS          = $20;    // Can use compression protcol
  CLIENT_ODBC              = $40;    // Odbc client
  CLIENT_LOCAL_FILES       = $80;    // Can use LOAD DATA LOCAL
  CLIENT_IGNORE_SPACE      = $100;   // Ignore spaces before '('
  CLIENT_PROTOCOL_41       = $200;   // Support the mysql_change_user()
  CLIENT_INTERACTIVE       = $400;   // This is an interactive client
  CLIENT_SSL               = $800;   // Switch to SSL after handshake
  CLIENT_IGNORE_SIGPIPE    = $1000;  // IGNORE sigpipes
  CLIENT_TRANSACTIONS      = $2000;  // Client knows about transactions
  CLIENT_RESERVED          = $4000;  // Old flag for 4.1 protocol (4.1.9)
  CLIENT_SECURE_CONNECTION = $8000;  // New 4.1 authentication
  CLIENT_MULTI_STATEMENTS  = $10000; // Enable/disable multi-stmt support
  CLIENT_MULTI_RESULTS     = $20000; // Enable/disable multi-results
  CLIENT_PLUGIN_AUTH       = $80000; // Supports authentication plugins
  CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA = $200000; //Understands length-encoded integer for auth response data in Protocol::HandshakeResponse41.

  CLIENT_CAPABILITIES      = (CLIENT_LONG_PASSWORD or CLIENT_LONG_FLAG or CLIENT_LOCAL_FILES or CLIENT_TRANSACTIONS or CLIENT_PROTOCOL_41 or CLIENT_SECURE_CONNECTION or CLIENT_MULTI_STATEMENTS or CLIENT_MULTI_RESULTS);

  SERVER_STATUS_IN_TRANS = 1;        // Transaction has started
  SERVER_STATUS_AUTOCOMMIT = 2;      // Server in auto_commit mode
  SERVER_STATUS_MORE_RESULTS = 4;    // More results on server
  SERVER_MORE_RESULTS_EXISTS = 8;    // Multi query - next query exists
  SERVER_QUERY_NO_GOOD_INDEX_USED = 16;
  SERVER_QUERY_NO_INDEX_USED = 32;

  MYSQL_ERRMSG_SIZE = 200;
  NET_READ_TIMEOUT  = 30;      // Timeout on read
  NET_WRITE_TIMEOUT = 60;      // Timeout on write
  NET_WAIT_TIMEOUT  = 8*60*60; // Wait for new query

const
  packet_error: cardinal   = $FFFFFFFF;

  MYSQL_TYPE_DECIMAL       = 0;
  MYSQL_TYPE_TINY          = 1;
  MYSQL_TYPE_SHORT         = 2;
  MYSQL_TYPE_LONG          = 3;
  MYSQL_TYPE_FLOAT         = 4;
  MYSQL_TYPE_DOUBLE        = 5;
  MYSQL_TYPE_NULL          = 6;
  MYSQL_TYPE_TIMESTAMP     = 7;
  MYSQL_TYPE_LONGLONG      = 8;
  MYSQL_TYPE_INT24         = 9;
  MYSQL_TYPE_DATE          = 10;
  MYSQL_TYPE_TIME          = 11;
  MYSQL_TYPE_DATETIME      = 12;
  MYSQL_TYPE_YEAR          = 13;
  MYSQL_TYPE_NEWDATE       = 14;
  MYSQL_TYPE_VARCHAR       = 15;
  MYSQL_TYPE_BIT           = 16;
  MYSQL_TYPE_JSON          = 245; //binary_log_types.h
  MYSQL_TYPE_NEWDECIMAL    = 246;
  MYSQL_TYPE_ENUM          = 247;
  MYSQL_TYPE_SET           = 248;
  MYSQL_TYPE_TINY_BLOB     = 249;
  MYSQL_TYPE_MEDIUM_BLOB   = 250;
  MYSQL_TYPE_LONG_BLOB     = 251;
  MYSQL_TYPE_BLOB          = 252;
  MYSQL_TYPE_VAR_STRING    = 253;
  MYSQL_TYPE_STRING        = 254;
  MYSQL_TYPE_GEOMETRY      = 255;

type
  TMySqlFieldType = MYSQL_TYPE_DECIMAL..MYSQL_TYPE_GEOMETRY; // enum_field_types

const
  FIELD_TYPE_DECIMAL       = MYSQL_TYPE_DECIMAL;
  FIELD_TYPE_TINY          = MYSQL_TYPE_TINY;
  FIELD_TYPE_SHORT         = MYSQL_TYPE_SHORT;
  FIELD_TYPE_LONG          = MYSQL_TYPE_LONG;
  FIELD_TYPE_FLOAT         = MYSQL_TYPE_FLOAT;
  FIELD_TYPE_DOUBLE        = MYSQL_TYPE_DOUBLE;
  FIELD_TYPE_NULL          = MYSQL_TYPE_NULL;
  FIELD_TYPE_TIMESTAMP     = MYSQL_TYPE_TIMESTAMP;
  FIELD_TYPE_LONGLONG      = MYSQL_TYPE_LONGLONG;
  FIELD_TYPE_INT24         = MYSQL_TYPE_INT24;
  FIELD_TYPE_DATE          = MYSQL_TYPE_DATE;
  FIELD_TYPE_TIME          = MYSQL_TYPE_TIME;
  FIELD_TYPE_DATETIME      = MYSQL_TYPE_DATETIME;
  FIELD_TYPE_YEAR          = MYSQL_TYPE_YEAR;
  FIELD_TYPE_NEWDATE       = MYSQL_TYPE_NEWDATE;
  FIELD_TYPE_VARCHAR       = MYSQL_TYPE_VARCHAR;
  FIELD_TYPE_BIT           = MYSQL_TYPE_BIT;
  FIELD_TYPE_JSON          = MYSQL_TYPE_JSON;
  FIELD_TYPE_NEWDECIMAL    = MYSQL_TYPE_NEWDECIMAL;
  FIELD_TYPE_ENUM          = MYSQL_TYPE_ENUM;
  FIELD_TYPE_SET           = MYSQL_TYPE_SET;
  FIELD_TYPE_TINY_BLOB     = MYSQL_TYPE_TINY_BLOB;
  FIELD_TYPE_MEDIUM_BLOB   = MYSQL_TYPE_MEDIUM_BLOB;
  FIELD_TYPE_LONG_BLOB     = MYSQL_TYPE_LONG_BLOB;
  FIELD_TYPE_BLOB          = MYSQL_TYPE_BLOB;
  FIELD_TYPE_VAR_STRING    = MYSQL_TYPE_VAR_STRING;
  FIELD_TYPE_STRING        = MYSQL_TYPE_STRING;
  FIELD_TYPE_CHAR          = MYSQL_TYPE_TINY;
  FIELD_TYPE_INTERVAL      = MYSQL_TYPE_ENUM;
  FIELD_TYPE_GEOMETRY      = MYSQL_TYPE_GEOMETRY;

  NUMBER_TYPES = [FIELD_TYPE_DECIMAL, FIELD_TYPE_TINY, FIELD_TYPE_SHORT,
    FIELD_TYPE_LONG, FIELD_TYPE_FLOAT,  FIELD_TYPE_DOUBLE, FIELD_TYPE_TIMESTAMP,
    FIELD_TYPE_LONGLONG, FIELD_TYPE_INT24, FIELD_TYPE_DATE, FIELD_TYPE_TIME,
    FIELD_TYPE_DATETIME, FIELD_TYPE_YEAR, FIELD_TYPE_NEWDATE,
    FIELD_TYPE_NEWDECIMAL];

// errmsg.h

  CLIENT_ERRMAP = 2; (* Errormap used by my_error() *)
  CR_UNKNOWN_ERROR = 2000;
  CR_SOCKET_CREATE_ERROR = 2001;
  CR_CONNECTION_ERROR = 2002;
  CR_CONN_HOST_ERROR = 2003;
  CR_IPSOCK_ERROR = 2004;
  CR_UNKNOWN_HOST = 2005;
  CR_SERVER_GONE_ERROR = 2006;
  CR_VERSION_ERROR = 2007;
  CR_OUT_OF_MEMORY = 2008;
  CR_WRONG_HOST_INFO = 2009;
  CR_LOCALHOST_CONNECTION = 2010;
  CR_TCP_CONNECTION = 2011;
  CR_SERVER_HANDSHAKE_ERR = 2012;
  CR_SERVER_LOST = 2013;
  CR_COMMANDS_OUT_OF_SYNC = 2014;
  CR_NAMEDPIPE_CONNECTION = 2015;
  CR_NAMEDPIPEWAIT_ERROR = 2016;
  CR_NAMEDPIPEOPEN_ERROR = 2017;
  CR_NAMEDPIPESETSTATE_ERROR = 2018;
  CR_CANT_READ_CHARSET = 2019;
  CR_NET_PACKET_TOO_LARGE = 2020;

  { new 4.1 error codes }
  NULL_POINTER  = 2028;
  NO_PREPARE_STMT = 2029;
  NOT_ALL_PARAMS_BOUND = 2030;
  DATA_TRUNCATED = 2031;
  NO_PARAMETERS_EXISTS = 2032;
  INVALID_PARAMETER_NO = 2033;
  INVALID_BUFFER_USE = 2034;
  UNSUPPORTED_PARAM_TYPE = 2035;
  
  CONN_UNKNOW_PROTOCOL   = 2046;
  
// mysqld_error.h

  ER_HASHCHK = 1000;
  ER_NISAMCHK = 1001;
  ER_NO = 1002;
  ER_YES = 1003;
  ER_CANT_CREATE_FILE = 1004;
  ER_CANT_CREATE_TABLE = 1005;
  ER_CANT_CREATE_DB = 1006;
  ER_DB_CREATE_EXISTS = 1007;
  ER_DB_DROP_EXISTS = 1008;
  ER_DB_DROP_DELETE = 1009;
  ER_DB_DROP_RMDIR = 1010; 
  ER_CANT_DELETE_FILE = 1011; 
  ER_CANT_FIND_SYSTEM_REC = 1012;
  ER_CANT_GET_STAT = 1013; 
  ER_CANT_GET_WD = 1014;
  ER_CANT_LOCK = 1015;
  ER_CANT_OPEN_FILE = 1016;
  ER_FILE_NOT_FOUND = 1017;
  ER_CANT_READ_DIR = 1018; 
  ER_CANT_SET_WD = 1019;
  ER_CHECKREAD = 1020; 
  ER_DISK_FULL = 1021;
  ER_DUP_KEY = 1022; 
  ER_ERROR_ON_CLOSE = 1023; 
  ER_ERROR_ON_READ = 1024; 
  ER_ERROR_ON_RENAME = 1025;
  ER_ERROR_ON_WRITE = 1026;
  ER_FILE_USED = 1027; 
  ER_FILSORT_ABORT = 1028; 
  ER_FORM_NOT_FOUND = 1029;
  ER_GET_ERRNO = 1030;
  ER_ILLEGAL_HA = 1031;
  ER_KEY_NOT_FOUND = 1032; 
  ER_NOT_FORM_FILE = 1033;
  ER_NOT_KEYFILE = 1034;
  ER_OLD_KEYFILE = 1035;
  ER_OPEN_AS_READONLY = 1036;
  ER_OUTOFMEMORY = 1037; 
  ER_OUT_OF_SORTMEMORY = 1038;
  ER_UNEXPECTED_EOF = 1039;
  ER_CON_COUNT_ERROR = 1040; 
  ER_OUT_OF_RESOURCES = 1041; 
  ER_BAD_HOST_ERROR = 1042; 
  ER_HANDSHAKE_ERROR = 1043; 
  ER_DBACCESS_DENIED_ERROR = 1044; 
  ER_ACCESS_DENIED_ERROR = 1045;
  ER_NO_DB_ERROR = 1046; 
  ER_UNKNOWN_COM_ERROR = 1047;
  ER_BAD_NULL_ERROR = 1048; 
  ER_BAD_DB_ERROR = 1049;
  ER_TABLE_EXISTS_ERROR = 1050;
  ER_BAD_TABLE_ERROR = 1051;
  ER_NON_UNIQ_ERROR = 1052;
  ER_SERVER_SHUTDOWN = 1053;
  ER_BAD_FIELD_ERROR = 1054;
  ER_WRONG_FIELD_WITH_GROUP = 1055;
  ER_WRONG_GROUP_FIELD = 1056; 
  ER_WRONG_SUM_SELECT = 1057; 
  ER_WRONG_VALUE_COUNT = 1058; 
  ER_TOO_LONG_IDENT = 1059;
  ER_DUP_FIELDNAME = 1060; 
  ER_DUP_KEYNAME = 1061; 
  ER_DUP_ENTRY = 1062;
  ER_WRONG_FIELD_SPEC = 1063; 
  ER_PARSE_ERROR = 1064; 
  ER_EMPTY_QUERY = 1065; 
  ER_NONUNIQ_TABLE = 1066;
  ER_INVALID_DEFAULT = 1067; 
  ER_MULTIPLE_PRI_KEY = 1068; 
  ER_TOO_MANY_KEYS = 1069;
  ER_TOO_MANY_KEY_PARTS = 1070;
  ER_TOO_LONG_KEY = 1071;
  ER_KEY_COLUMN_DOES_NOT_EXITS = 1072; 
  ER_BLOB_USED_AS_KEY = 1073;
  ER_TOO_BIG_FIELDLENGTH = 1074; 
  ER_WRONG_AUTO_KEY = 1075; 
  ER_READY = 1076; 
  ER_NORMAL_SHUTDOWN = 1077; 
  ER_GOT_SIGNAL = 1078;
  ER_SHUTDOWN_COMPLETE = 1079;
  ER_FORCING_CLOSE = 1080; 
  ER_IPSOCK_ERROR = 1081;
  ER_NO_SUCH_INDEX = 1082; 
  ER_WRONG_FIELD_TERMINATORS = 1083;
  ER_BLOBS_AND_NO_TERMINATED = 1084; 
  ER_TEXTFILE_NOT_READABLE = 1085;
  ER_FILE_EXISTS_ERROR = 1086; 
  ER_LOAD_INFO = 1087;
  ER_ALTER_INFO = 1088;
  ER_WRONG_SUB_KEY = 1089;
  ER_CANT_REMOVE_ALL_FIELDS = 1090; 
  ER_CANT_DROP_FIELD_OR_KEY = 1091; 
  ER_INSERT_INFO = 1092;
  ER_INSERT_TABLE_USED = 1093;
  ER_NO_SUCH_THREAD = 1094;
  ER_KILL_DENIED_ERROR = 1095; 
  ER_NO_TABLES_USED = 1096; 
  ER_TOO_BIG_SET = 1097; 
  ER_NO_UNIQUE_LOGFILE = 1098;
  ER_TABLE_NOT_LOCKED_FOR_WRITE = 1099;
  ER_TABLE_NOT_LOCKED = 1100; 
  ER_BLOB_CANT_HAVE_DEFAULT = 1101; 
  ER_WRONG_DB_NAME = 1102;
  ER_WRONG_TABLE_NAME = 1103;
  ER_TOO_BIG_SELECT = 1104;
  ER_UNKNOWN_ERROR = 1105;
  ER_UNKNOWN_PROCEDURE = 1106; 
  ER_WRONG_PARAMCOUNT_TO_PROCEDURE = 1107;
  ER_WRONG_PARAMETERS_TO_PROCEDURE = 1108;
  ER_UNKNOWN_TABLE = 1109;
  ER_FIELD_SPECIFIED_TWICE = 1110; 
  ER_INVALID_GROUP_FUNC_USE = 1111;
  ER_UNSUPPORTED_EXTENSION = 1112; 
  ER_TABLE_MUST_HAVE_COLUMNS = 1113; 
  ER_RECORD_FILE_FULL = 1114; 
  ER_UNKNOWN_CHARACTER_SET = 1115; 
  ER_TOO_MANY_TABLES = 1116;
  ER_TOO_MANY_FIELDS = 1117; 
  ER_TOO_BIG_ROWSIZE = 1118; 
  ER_STACK_OVERRUN = 1119;
  ER_WRONG_OUTER_JOIN = 1120;
  ER_NULL_COLUMN_IN_INDEX = 1121; 
  ER_CANT_FIND_UDF = 1122; 
  ER_CANT_INITIALIZE_UDF = 1123;
  ER_UDF_NO_PATHS = 1124;
  ER_UDF_EXISTS = 1125; 
  ER_CANT_OPEN_LIBRARY = 1126;
  ER_CANT_FIND_DL_ENTRY = 1127; 
  ER_FUNCTION_NOT_DEFINED = 1128;
  ER_HOST_IS_BLOCKED = 1129;
  ER_HOST_NOT_PRIVILEGED = 1130;
  ER_PASSWORD_ANONYMOUS_USER = 1131;
  ER_PASSWORD_NOT_ALLOWED = 1132;
  ER_PASSWORD_NO_MATCH = 1133;
  ER_UPDATE_INFO = 1134; 
  ER_CANT_CREATE_THREAD = 1135; 
  ER_WRONG_VALUE_COUNT_ON_ROW = 1136; 
  ER_CANT_REOPEN_TABLE = 1137;
  ER_INVALID_USE_OF_NULL = 1138; 
  ER_REGEXP_ERROR = 1139;
  ER_MIX_OF_GROUP_FUNC_AND_FIELDS = 1140; 
  ER_NONEXISTING_GRANT = 1141;
  ER_TABLEACCESS_DENIED_ERROR = 1142; 
  ER_COLUMNACCESS_DENIED_ERROR = 1143; 
  ER_ILLEGAL_GRANT_FOR_TABLE = 1144;
  ER_GRANT_WRONG_HOST_OR_USER = 1145;
  ER_NO_SUCH_TABLE = 1146; 
  ER_NONEXISTING_TABLE_GRANT = 1147;
  ER_NOT_ALLOWED_COMMAND = 1148;
  ER_SYNTAX_ERROR = 1149;
  ER_DELAYED_CANT_CHANGE_LOCK = 1150; 
  ER_TOO_MANY_DELAYED_THREADS = 1151; 
  ER_ABORTING_CONNECTION = 1152; 
  ER_NET_PACKET_TOO_LARGE = 1153;
  ER_NET_READ_ERROR_FROM_PIPE = 1154; 
  ER_NET_FCNTL_ERROR = 1155; 
  ER_NET_PACKETS_OUT_OF_ORDER = 1156; 
  ER_NET_UNCOMPRESS_ERROR = 1157; 
  ER_NET_READ_ERROR = 1158; 
  ER_NET_READ_INTERRUPTED = 1159;
  ER_NET_ERROR_ON_WRITE = 1160;
  ER_NET_WRITE_INTERRUPTED = 1161; 
  ER_TOO_LONG_STRING = 1162; 
  ER_TABLE_CANT_HANDLE_BLOB = 1163; 
  ER_TABLE_CANT_HANDLE_AUTO_INCREMENT = 1164; 
  ER_DELAYED_INSERT_TABLE_LOCKED = 1165;
  ER_WRONG_COLUMN_NAME = 1166; 
  ER_WRONG_KEY_COLUMN = 1167; 
  ER_WRONG_MRG_TABLE = 1168; 
  ER_DUP_UNIQUE = 1169;
  ER_BLOB_KEY_WITHOUT_LENGTH = 1170;
  ER_PRIMARY_CANT_HAVE_NULL = 1171;
  ER_TOO_MANY_ROWS = 1172;
  ER_REQUIRES_PRIMARY_KEY = 1173; 
  ER_NO_RAID_COMPILED = 1174; 
  ER_UPDATE_WITHOUT_KEY_IN_SAFE_MODE = 1175;
  ER_KEY_DOES_NOT_EXITS = 1176; 
  ER_CHECK_NO_SUCH_TABLE = 1177;
  ER_CHECK_NOT_IMPLEMENTED = 1178; 
  ER_CANT_DO_THIS_DURING_AN_TRANSACTION = 1179;
  ER_ERROR_DURING_COMMIT = 1180; 
  ER_ERROR_DURING_ROLLBACK = 1181; 
  ER_ERROR_DURING_FLUSH_LOGS = 1182;
  ER_ERROR_DURING_CHECKPOINT = 1183; 
  ER_NEW_ABORTING_CONNECTION = 1184; 
  ER_DUMP_NOT_IMPLEMENTED = 1185; 
  ER_FLUSH_MASTER_BINLOG_CLOSED = 1186;
  ER_INDEX_REBUILD = 1187;
  ER_MASTER = 1188;
  ER_MASTER_NET_READ = 1189;
  ER_MASTER_NET_WRITE = 1190; 
  ER_FT_MATCHING_KEY_NOT_FOUND = 1191;
  ER_LOCK_OR_ACTIVE_TRANSACTION = 1192;
  ER_UNKNOWN_SYSTEM_VARIABLE = 1193; 
  ER_CRASHED_ON_USAGE = 1194; 
  ER_CRASHED_ON_REPAIR = 1195;
  ER_WARNING_NOT_COMPLETE_ROLLBACK = 1196;
  ER_TRANS_CACHE_FULL = 1197;
  ER_SLAVE_MUST_STOP = 1198;
  ER_SLAVE_NOT_RUNNING = 1199;
  ER_BAD_SLAVE = 1200;
  ER_MASTER_INFO = 1201;
  ER_SLAVE_THREAD = 1202;
  ER_TOO_MANY_USER_CONNECTIONS = 1203;
  ER_SET_CONSTANTS_ONLY = 1204;
  ER_LOCK_WAIT_TIMEOUT = 1205;
  ER_LOCK_TABLE_FULL = 1206;
  ER_READ_ONLY_TRANSACTION = 1207;
  ER_DROP_DB_WITH_READ_LOCK = 1208;
  ER_CREATE_DB_WITH_READ_LOCK = 1209;
  ER_WRONG_ARGUMENTS = 1210;
  ER_NO_PERMISSION_TO_CREATE_USER = 1211;
  ER_UNION_TABLES_IN_DIFFERENT_DIR = 1212;
  ER_LOCK_DEADLOCK = 1213;
  ER_TABLE_CANT_HANDLE_FULLTEXT = 1214;
  ER_CANNOT_ADD_FOREIGN = 1215;
  ER_NO_REFERENCED_ROW = 1216;
  ER_ROW_IS_REFERENCED = 1217;
  ER_SP_DOES_NOT_EXIST = 1305;
  ER_TRG_DOES_NOT_EXIST = 1360;

  ER_ERROR_MESSAGES = 218;

const
  PROTOCOL_VERSION              = 10;
  MYSQL_CLIENT_VERSION          = '8.0.0 Direct';
  MYSQL_SERVER_SUFFIX           = '';
  FRM_VER                       = 6;
  MYSQL_VERSION_ID              = 32351;
  MYSQL_UNIX_ADDR               = '/tmp/mysql.sock';
  MYSQL_PORT                    = 3306;
  NULL_LENGTH                   = -1;
  MYSQL_NO_DATA                 = 100;
  MYSQL_DATA_TRUNCATED          = 101;

type
  TMyDialogAuthPluginEvent = procedure(const Question: string; out Answer: string) of object;

type
{$IFDEF NEXTGEN}
  TIntPtrObj = TObject;
{$ELSE}
  TIntPtrObj = IntPtr;
{$ENDIF}

  PMYSQL_CON = TIntPtrObj;
  PMYSQL_RES = TIntPtrObj;
  PMYSQL_STMT = TIntPtrObj;
  PMYSQL_FIELD = TIntPtrObj;
  PMYSQL_ROW = TIntPtrObj;
  PMYSQL_LENGTHS = TIntPtrObj;

  TClientStructVer = ({$IFDEF HAVE_DIRECT}cvDirect, {$ENDIF}cv3, cv4, cv410, cv411); // Version of MYSQL_FIELD

  // MYSQL field struct for MyDAC
  TMYSQL_FIELD = record
    Name: AnsiString; // Name or Alias
    OrgName: AnsiString; // Original name
    Table: AnsiString;
    OrgTable: AnsiString;
    Flags: cardinal;
    MyType: TMySqlFieldType;
    LengthInBytes: cardinal; // If Unicode then LengthInChars = LengthInBytes div MaxUTF8CharLen
    CharsetNr: integer;
    Decimals: cardinal;
  end;

  // MYSQL struct for MySQL v 3.23
  TMYSQL_FIELD3 = record
    name: PAnsiChar;          // Name of column
    table: PAnsiChar;         // Table of column if column was a field
    def: IntPtr;              // Default value (set by mysql_list_fields)
    mytype: TMySqlFieldType;  // Type of field. Se mysql_com.h for types
    length: cardinal;         // Width of column
    max_length: cardinal;     // Max width of selected set
    flags: cardinal;          // Div flags
    decimals: cardinal;       // Number of decimals in field
  end;

  // MYSQL struct for MySQL v 4.0
  TMYSQL_FIELD4 = packed record
    name: PAnsiChar;          // Name of column
    table: PAnsiChar;         // Table of column if column was a field
    org_table: PAnsiChar;	    // Org table name if table was an alias
    db: IntPtr;      			    // Database for table
    def: IntPtr;              // Default value (set by mysql_list_fields)
    length: cardinal;         // Width of column
    max_length: cardinal;     // Max width of selected set
    flags: cardinal;          // Div flags
    decimals: cardinal;       // Number of decimals in field
    mytype: TMySqlFieldType;  // Type of field. Se mysql_com.h for types
  end;

  // MYSQL struct for MySQL v 4.1
  TMYSQL_FIELD410 = packed record
    name: PAnsiChar;          // Name of column
    org_name: PAnsiChar;      // Original column name, if an alias
    table: PAnsiChar;         // Table of column if column was a field
    org_table: PAnsiChar;     // Org table name if table was an alias
    db: IntPtr;      			    // Database for table
    def: IntPtr;              // Default value (set by mysql_list_fields)
    length: cardinal;         // Width of column
    max_length: cardinal;     // Max width of selected set
    name_length: cardinal;
    org_name_length: cardinal;
    table_length: cardinal;
    org_table_length: cardinal;
    db_length: cardinal;
    def_length: cardinal;
    flags: cardinal;          // Div flags
    decimals: cardinal;       // Number of decimals in field
    charsetnr: cardinal;      // Character set
    mytype: TMySqlFieldType;  // Type of field. Se mysql_com.h for types
  end;

  // MYSQL struct for MySQL v 4.1.1
  TMYSQL_FIELD411 = packed record
    name: PAnsiChar;          // Name of column
    org_name: PAnsiChar;      // Original column name, if an alias
    table: PAnsiChar;         // Table of column if column was a field
    org_table: PAnsiChar;     // Org table name if table was an alias
    db: IntPtr;               // Database for table
    catalog: IntPtr;          // Catalog for table
    def: IntPtr;              // Default value (set by mysql_list_fields)
    length: cardinal;         // Width of column
    max_length: cardinal;     // Max width of selected set
    name_length: cardinal;
    org_name_length: cardinal;
    table_length: cardinal;
    org_table_length: cardinal;
    db_length: cardinal;
    catalog_length: cardinal;
    def_length: cardinal;
    flags: cardinal;          // Div flags
    decimals: cardinal;       // Number of decimals in field
    charsetnr: cardinal;      // Character set
    mytype: TMySqlFieldType;  // Type of field. Se mysql_com.h for types
  end;

  PMYSQL_FIELD3 = ^TMYSQL_FIELD3;
  PMYSQL_FIELD4 = ^TMYSQL_FIELD4;
  PMYSQL_FIELD410 = ^TMYSQL_FIELD410;
  PMYSQL_FIELD411 = ^TMYSQL_FIELD411;

type
  MYSQL_FIELD_OFFSET = cardinal;  // offset to current field

type
  TMySqlOption = ( // mysql_option
    MYSQL_OPT_CONNECT_TIMEOUT, MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE, MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE, MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME,
    MYSQL_OPT_LOCAL_INFILE,

    // MySQL Server 4.1 Specific
    MYSQL_OPT_PROTOCOL, MYSQL_SHARED_MEMORY_BASE_NAME, MYSQL_OPT_READ_TIMEOUT,
    MYSQL_OPT_WRITE_TIMEOUT, MYSQL_OPT_USE_RESULT,
    MYSQL_OPT_USE_REMOTE_CONNECTION, MYSQL_OPT_USE_EMBEDDED_CONNECTION,
    MYSQL_OPT_GUESS_CONNECTION, MYSQL_SET_CLIENT_IP, MYSQL_SECURE_AUTH
  );

// C API Prepared Statements DataTypes
type
  PNET = pointer;
  PMYSQL_BIND = IntPtr;
  PUInt = ^cardinal;

type
  MYSQL_BIND = record // Copied from MySQL 4.1.7
    length: PUInt;             (* output length pointer *)
    is_null: PByte;            (* Pointer to null indicator *)
    buffer: IntPtr;            (* buffer to get/put data *)
    buffer_type: TMySqlFieldType;  (* buffer type *)
    buffer_length: cardinal;   (* buffer length, must be set for str/binary *)

    (* Following are for internal use. Set by mysql_stmt_bind_param *)
    inter_buffer: IntPtr;      (* for the current data position *)
    offset: integer;           (* offset position for char/binary fetch *)
    internal_length: integer;  (* Used if length is 0 *)
    param_number: cardinal;    (* For null count and error messages *)
    pack_length: cardinal;     (* Internal length for packed data *)
    is_unsigned: byte;         (* set if integer type is unsigned *)
    long_data_used: byte;      (* If used with mysql_send_long_data *)
    internal_is_null: byte;    (* Used if is_null is 0 *)
    store_param_func: IntPtr;
    fetch_result: IntPtr;
    skip_result: IntPtr;
  end;
  TMYSQL_BINDS = array of MYSQL_BIND;

  MYSQL_BIND503 = record // Copied from MySQL 5.0.3
    length: PUInt;             (* output length pointer *)
    is_null: PByte;            (* Pointer to null indicator *)
    buffer: IntPtr;            (* buffer to get/put data *)
    error: PByte;
    buffer_type: TMySqlFieldType;  (* buffer type *)
    buffer_length: cardinal;   (* buffer length, must be set for str/binary *)
    
    (* Following are for internal use. Set by mysql_stmt_bind_param *)
    row_ptr: IntPtr;           (* for the current data position *)
    offset: integer;           (* offset position for char/binary fetch *)
    length_value: integer;     (* Used if length is 0 *)
    param_number: cardinal;    (* For null count and error messages *)
    pack_length: cardinal;     (* Internal length for packed data *)
    error_value: byte;         (* used if error is 0 *)
    is_unsigned: byte;         (* set if integer type is unsigned *)
    long_data_used: byte;      (* If used with mysql_send_long_data *)
    is_null_value: byte;       (* Used if is_null is 0 *)
    store_param_func: IntPtr;
    fetch_result: IntPtr;
    skip_result: IntPtr;
  end;
  TMYSQL_BINDS503 = array of MYSQL_BIND503;

type
  MYSQL_PROTOCOL_TYPE = (
    MYSQL_PROTOCOL_DEFAULT,
    MYSQL_PROTOCOL_TCP,
    MYSQL_PROTOCOL_SOCKET,
    MYSQL_PROTOCOL_PIPE,
    MYSQL_PROTOCOL_MEMORY,
    MYSQL_PROTOCOL_SSL
  );

const
  MYSQL_TIMESTAMP_NONE = -2;
  MYSQL_TIMESTAMP_ERROR = -1;
  MYSQL_TIMESTAMP_DATE = 0;
  MYSQL_TIMESTAMP_DATETIME = 1;
  MYSQL_TIMESTAMP_TIME = 2;

const
  ORDINARY_QUESTION = 2;
  LAST_QUESTION = 3;
  PASSWORD_QUESTION = 4;
  LAST_PASSWORD = 5;

type
  PMYSQL_TIME = ^MYSQL_TIME;
  MYSQL_TIME = record
    year, month, day, hour, minute, second: cardinal;
    second_part: cardinal;
    neg: byte;
    time_type: integer;
  end;

  function DateTimeToMYSQL_TIME(const dt: TDateTime): MYSQL_TIME;
  procedure DecodeVersion(const Version: string; out Major, Minor, Release: integer; out IsMariaDB: boolean);

const
  DACProductName = 'MyDAC';

type
  TMySqlFieldInfo = record
    Name: AnsiString;           (* Name of column *)
    OrgName: AnsiString;        (* Original column name, if an alias *)
    Table: AnsiString;          (* Table of column if column was a field *)
    OrgTable: AnsiString;       (* Org table name if table was an alias *)
    Database: AnsiString;       (* Database for table *)
    DefaultValue: AnsiString;   (* Default value *)
    Length: cardinal;            (* Width of column *)
    Scale: integer;             (* Number of decimals in field *)
    MaxLength: integer;         (* Max width of selected set *)
    Flags: integer;             (* Div flags *)
    CharsetNr: integer;         (* Character set *)
    MyType: TMySqlFieldType;    (* MySql type of field. *)
  end;
  TMySqlFieldInfos = array of TMySqlFieldInfo;

function MySqlFieldInfo(
  const name: AnsiString;
  const orgName: AnsiString;
  const table: AnsiString;
  const orgTable: AnsiString;
  const database: AnsiString;
  const defaultValue: AnsiString;
  const length: cardinal;
  const scale: integer;
  const maxLength: integer;
  const flags: integer;
  const charsetnr: integer;
  const mytype: TMySqlFieldType): TMySqlFieldInfo;

function IsNumField(const FieldInfo: TMySqlFieldInfo): boolean;
function IsNotNull(const FieldInfo: TMySqlFieldInfo): boolean;
function IsPrimaryKey(const FieldInfo: TMySqlFieldInfo): boolean;
function IsUniqueKey(const FieldInfo: TMySqlFieldInfo): boolean;
function IsMultipleKey(const FieldInfo: TMySqlFieldInfo): boolean;
function IsUnsigned(const FieldInfo: TMySqlFieldInfo): boolean;
function IsZeroFill(const FieldInfo: TMySqlFieldInfo): boolean;
function IsBinary(const FieldInfo: TMySqlFieldInfo): boolean;
function IsAutoIncrement(const FieldInfo: TMySqlFieldInfo): boolean;

(*type
  TCharsetInfo = record
    number: integer;
    csname: string;
    name: string;
    _encoding: Encoding;
  end;

function CharsetInfo(
  const number: integer;
  const csname: string;
  const name: string;
  const encoding: string): TCharsetInfo;*)

type
  TServerCommand = (
    scSleep,
    scQuit,
    scInitDb,
    scQuery,
    scFieldList,
    scCreateDb,
    scDropDb,
    scRefresh,
    scShutdown,
    scStatistics,
    scProcessInfo,
    scConnect,
    scProcessKill,
    scDebug,
    scPing,
    scTime,
    scDelayedInsert,
    scChangeUser,
    scBunLogDump,
    scTableDump,
    scConnectOut,
    scRegisterSlave,
    scPrepare,
    scExecute,
    scLongData,
    scCloseStmt
  );

  TPrepStmtState = (
    psUnknown,
    psPrepare,
    psExecute
  );

  TMySqlStatus = (
    msReady,
    msGetResult,
    msUseResult
  );

type
  TCharset = record
    CharsetID: word;
    CharSetName: string;
    CharSetWidth: integer;
  end;

const
  CharSet: array [0..39] of TCharset = (
    (CharsetID: 1;  CharSetName: 'big5';     CharSetWidth: 2),
    (CharsetID: 3;  CharSetName: 'dec8';     CharSetWidth: 1),
    (CharsetID: 4;  CharSetName: 'cp850';    CharSetWidth: 1),
    (CharsetID: 6;  CharSetName: 'hp8';      CharSetWidth: 1),
    (CharsetID: 7;  CharSetName: 'koi8r';    CharSetWidth: 1),
    (CharsetID: 8;  CharSetName: 'latin1';   CharSetWidth: 1),
    (CharsetID: 9;  CharSetName: 'latin2';   CharSetWidth: 1),
    (CharsetID: 10; CharSetName: 'swe7';     CharSetWidth: 1),
    (CharsetID: 11; CharSetName: 'ascii';    CharSetWidth: 1),
    (CharsetID: 12; CharSetName: 'ujis';     CharSetWidth: 3),
    (CharsetID: 13; CharSetName: 'sjis';     CharSetWidth: 2),
    (CharsetID: 16; CharSetName: 'hebrew';   CharSetWidth: 1),
    (CharsetID: 18; CharSetName: 'tis620';   CharSetWidth: 1),
    (CharsetID: 19; CharSetName: 'euckr';    CharSetWidth: 2),
    (CharsetID: 22; CharSetName: 'koi8u';    CharSetWidth: 1),
    (CharsetID: 24; CharSetName: 'gb2312';   CharSetWidth: 2),
    (CharsetID: 25; CharSetName: 'greek';    CharSetWidth: 1),
    (CharsetID: 26; CharSetName: 'cp1250';   CharSetWidth: 1),
    (CharsetID: 28; CharSetName: 'gbk';      CharSetWidth: 2),
    (CharsetID: 30; CharSetName: 'latin5';   CharSetWidth: 1),
    (CharsetID: 32; CharSetName: 'armscii8'; CharSetWidth: 1),
    (CharsetID: 33; CharSetName: 'utf8';     CharSetWidth: 3),
    (CharsetID: 35; CharSetName: 'ucs2';     CharSetWidth: 2),
    (CharsetID: 36; CharSetName: 'cp866';    CharSetWidth: 1),
    (CharsetID: 37; CharSetName: 'keybcs2';  CharSetWidth: 1),
    (CharsetID: 38; CharSetName: 'macce';    CharSetWidth: 1),
    (CharsetID: 39; CharSetName: 'macroman'; CharSetWidth: 1),
    (CharsetID: 40; CharSetName: 'cp852';    CharSetWidth: 1),
    (CharsetID: 41; CharSetName: 'latin7';   CharSetWidth: 1),
    (CharsetID: 45; CharSetName: 'utf8mb4';  CharSetWidth: 4),
    (CharsetID: 51; CharSetName: 'cp1251';   CharSetWidth: 1),
    (CharsetID: 54; CharSetName: 'utf16';    CharSetWidth: 4),
    (CharsetID: 56; CharSetName: 'utf16le';  CharSetWidth: 4),
    (CharsetID: 57; CharSetName: 'cp1256';   CharSetWidth: 1),
    (CharsetID: 59; CharSetName: 'cp1257';   CharSetWidth: 1),
    (CharsetID: 60; CharSetName: 'utf32';    CharSetWidth: 4),
    (CharsetID: 63; CharSetName: 'binary';   CharSetWidth: 1),
    (CharsetID: 92; CharSetName: 'geostd8';  CharSetWidth: 1),
    (CharsetID: 95; CharSetName: 'cp932';    CharSetWidth: 2),
    (CharsetID: 97; CharSetName: 'eucjpms '; CharSetWidth: 3)
  );

function GetCharSetID(CharSetName: string): integer;
function GetCharSetName(CharSetID: integer): string;
function GetCharSetWidth(CharSetName: string): integer;


implementation

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  DAConsts;


function DateTimeToMYSQL_TIME(const dt: TDateTime): MYSQL_TIME;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(dt, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  Result.year := AYear;
  Result.month := AMonth;
  Result.day := ADay;
  Result.hour := AHour;
  Result.minute := AMinute;
  Result.second := ASecond;
  Result.second_part := AMilliSecond * 1000;
  Result.neg := Byte(False);
  Result.time_type := MYSQL_TIMESTAMP_DATETIME;
end;

procedure DecodeVersion(const Version: string; out Major, Minor, Release: integer; out IsMariaDB: boolean);
var
  i, j: integer;
  VersionStr: string;
begin
//for MariaDB 5.5.5. Example 5.5.5-10.2.7-MariaDB
  i := Pos('5.5.5-', Version);
  if (i > 0) and CharInSet(Version[Length('5.5.5-') + 1], ['0'..'9'])then
    VersionStr := Copy(Version, Length('5.5.5-') + 1, Length(Version) - 1)
  else
    VersionStr := Version;


  i := Pos('.', VersionStr);
  Major := StrToInt(Copy(VersionStr, 1, i - 1));

  j := i + 1;
  while (j <= Length(VersionStr))
    and (VersionStr[j] >= '0')
    and (VersionStr[j] <= '9') do
    Inc(j);
  Minor := StrToInt(Copy(VersionStr, i + 1, j - i - 1));

  i := j;
  j := i + 1;
  while (j <= Length(VersionStr))
    and (VersionStr[j] >= '0')
    and (VersionStr[j] <= '9') do
    Inc(j);
  Release := StrToInt(Copy(VersionStr, i + 1, j - i - 1));

  IsMariaDB := Pos('MariaDB', VersionStr) > 0;
end;

function MySqlFieldInfo(
  const name: AnsiString;
  const orgName: AnsiString;
  const table: AnsiString;
  const orgTable: AnsiString;
  const database: AnsiString;
  const defaultValue: AnsiString;
  const length: cardinal;
  const scale: integer;
  const maxLength: integer;
  const flags: integer;
  const charsetnr: integer;
  const mytype: TMySqlFieldType): TMySqlFieldInfo;
begin
  Result.Name := name;
  Result.OrgName := orgName;
  Result.Table := table;
  Result.OrgTable := orgTable;
  Result.Database := database;
  Result.DefaultValue := defaultValue;
  Result.Length := length;
  Result.Scale := scale;
  Result.MaxLength := maxLength;
  Result.Flags := flags;
  Result.CharsetNr := charsetnr;
  Result.MyType := mytype;
  if IsNumField(Result) then
    Result.Flags := Flags or NUM_FLAG;
end;

function IsNumField(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := ((FieldInfo.MyType <= FIELD_TYPE_INT24) and
    ((FieldInfo.MyType <> FIELD_TYPE_TIMESTAMP) or (FieldInfo.Length = 14) or (FieldInfo.Length = 8)) or
    (FieldInfo.MyType = FIELD_TYPE_YEAR));
end;

function IsNotNull(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := (FieldInfo.Flags and NOT_NULL_FLAG) <> 0;
end;

function IsPrimaryKey(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := (FieldInfo.Flags and PRI_KEY_FLAG) <> 0;
end;

function IsUniqueKey(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := (FieldInfo.Flags and UNIQUE_KEY_FLAG) <> 0;
end;

function IsMultipleKey(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := (FieldInfo.Flags and MULTIPLE_KEY_FLAG ) <> 0;
end;

function IsUnsigned(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := (FieldInfo.Flags and UNSIGNED_FLAG ) <> 0;
end;

function IsZeroFill(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := (FieldInfo.Flags and ZEROFILL_FLAG ) <> 0;
end;

function IsBinary(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := (FieldInfo.Flags and BINARY_FLAG ) <> 0;
end;

function IsAutoIncrement(const FieldInfo: TMySqlFieldInfo): boolean;
begin
  Result := (FieldInfo.Flags and AUTO_INCREMENT_FLAG ) <> 0;
end;

(*
function CharsetInfo(
  const number: integer;
  const csname: string;
  const name: string;
  const encoding: string): TCharsetInfo;
begin
  Result.number := number;
  Result.csname := csname;
  Result.name := name;
  Result._encoding := CLRClasses.Encoding.GetEncoding(encoding);
end;*)

function GetCharSetID(CharSetName: string): integer;
var
  i: integer;
begin
  Result := 0;
  if CharSetName = '' then
    Exit;

  for i := 0 to Length(CharSet) - 1 do
    if CharSet[i].CharSetName = CharSetName then begin
      Result := CharSet[i].CharsetID;
      Exit;
    end;
end;

function GetCharSetName(CharSetID: integer): string;
var
  i: integer;
begin
  Result := '';
  if CharSetID = 0 then
    Exit;

  for i := 0 to Length(CharSet) - 1 do
    if CharSet[i].CharSetID = CharSetID then begin
      Result := CharSet[i].CharSetName;
      Exit;
    end;
end;

function GetCharSetWidth(CharSetName: string): integer;
var
  i: integer;
begin
  Result := 1;
  if CharSetName = '' then
    Exit;

  for i := 0 to Length(CharSet) - 1 do
    if CharSet[i].CharSetName = CharSetName then begin
      Result := CharSet[i].CharSetWidth;
      Exit;
    end;
end;

initialization
{$IFNDEF CPU64}
  Assert(SizeOf(TMYSQL_FIELD3) = 32);
{$ENDIF}


finalization

end.