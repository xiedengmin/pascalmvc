
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I VirtualQuery.inc}
{$I LiteDac.inc}
unit LiteCallVirtual;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry, 
{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFDEF POSIX}
  Posix.Dlfcn,
{$ENDIF}
{$IFDEF UNIX}
  dl,
{$ENDIF}
  CLRClasses,
  CRTypes,
  DAConsts,
{$IFDEF VIRTUAL_QUERY}
  LiteConstsVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
  LiteConsts;
{$ELSE}
  LiteConstsUni;
{$ENDIF}
{$ENDIF}


const
{$IFNDEF VIRTUAL_QUERY}
  DACProductName = 'LiteDAC';
{$ELSE}
  DACProductName = 'VirtualDAC';
{$ENDIF}

  SQLiteDLLName = {$IFDEF MSWINDOWS}
                    'sqlite3.dll';
                  {$ELSE}
                    {$IFDEF MACOS}
                    'libsqlite3.dylib';
                    {$ELSE}
                    {$IFDEF ANDROID}
                    'libsqlite.so';
                    {$ELSE}
                    'libsqlite3.so';
                    {$ENDIF}
                    {$ENDIF}
                  {$ENDIF}

const
  // Result Codes
  SQLITE_OK         =  0;   // Successful result
  SQLITE_ERROR      =  1;   // SQL error or missing database
  SQLITE_INTERNAL   =  2;   // Internal logic error in SQLite
  SQLITE_PERM       =  3;   // Access permission denied
  SQLITE_ABORT      =  4;   // Callback routine requested an abort
  SQLITE_BUSY       =  5;   // The database file is locked
  SQLITE_LOCKED     =  6;   // A table in the database is locked
  SQLITE_NOMEM      =  7;   // A malloc() failed
  SQLITE_READONLY   =  8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT  =  9;   // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR      = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT    = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND   = 12;   // NOT USED. Table or record not found
  SQLITE_FULL       = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN   = 14;   // Unable to open the database file
  SQLITE_PROTOCOL   = 15;   // NOT USED. Database lock protocol error
  SQLITE_EMPTY      = 16;   // Database is empty
  SQLITE_SCHEMA     = 17;   // The database schema changed
  SQLITE_TOOBIG     = 18;   // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT = 19;   // Abort due to constraint violation
  SQLITE_MISMATCH   = 20;   // Data type mismatch
  SQLITE_MISUSE     = 21;   // Library used incorrectly
  SQLITE_NOLFS      = 22;   // Uses OS features not supported on host
  SQLITE_AUTH       = 23;   // Authorization denied
  SQLITE_FORMAT     = 24;   // Auxiliary database format error
  SQLITE_RANGE      = 25;   // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB     = 26;   // File opened that is not a database file
  SQLITE_ROW        = 100;  // sqlite3_step() has another row ready
  SQLITE_DONE       = 101;  // sqlite3_step() has finished executing

  // extended error codes

  SQLITE_BUSY_RECOVERY           = SQLITE_BUSY + 256;        // 261
  SQLITE_LOCKED_SHAREDCACHE      = SQLITE_LOCKED + 256;      // 262
  SQLITE_READONLY_RECOVERY       = SQLITE_READONLY + 256;    // 264
  SQLITE_IOERR_READ              = SQLITE_IOERR + 256;       // 266
  SQLITE_CORRUPT_VTAB            = SQLITE_CORRUPT + 256;     // 267
  SQLITE_CANTOPEN_NOTEMPDIR      = SQLITE_CANTOPEN + 256;    // 270
  SQLITE_ABORT_ROLLBACK          = SQLITE_ABORT + 2*256;     // 516
  SQLITE_BUSY_SNAPSHOT           = SQLITE_BUSY + 2*256;      // 517
  SQLITE_READONLY_CANTLOCK       = SQLITE_READONLY + 2*256;  // 520
  SQLITE_CANTOPEN_ISDIR          = SQLITE_CANTOPEN + 2*256;  // 526
  SQLITE_IOERR_SHORT_READ        = SQLITE_IOERR + 2*256;     // 522
  SQLITE_IOERR_WRITE             = SQLITE_IOERR + 3*256;     // 778
  SQLITE_IOERR_FSYNC             = SQLITE_IOERR + 4*256;     // 1034
  SQLITE_IOERR_DIR_FSYNC         = SQLITE_IOERR + 5*256;     // 1290
  SQLITE_IOERR_TRUNCATE          = SQLITE_IOERR + 6*256;     // 1546
  SQLITE_IOERR_FSTAT             = SQLITE_IOERR + 7*256;     // 1802
  SQLITE_IOERR_UNLOCK            = SQLITE_IOERR + 8*256;     // 2058
  SQLITE_IOERR_RDLOCK            = SQLITE_IOERR + 9*256;     // 2314
  SQLITE_IOERR_DELETE            = SQLITE_IOERR + 10*256;    // 2570
  SQLITE_IOERR_BLOCKED           = SQLITE_IOERR + 11*256;    // 2826
  SQLITE_IOERR_NOMEM             = SQLITE_IOERR + 12*256;    // 3082
  SQLITE_IOERR_ACCESS            = SQLITE_IOERR + 13*256;    // 3338
  SQLITE_IOERR_CHECKRESERVEDLOCK = SQLITE_IOERR + 14*256;    // 3594
  SQLITE_IOERR_LOCK              = SQLITE_IOERR + 15*256;    // 3850
  SQLITE_IOERR_CLOSE             = SQLITE_IOERR + 16*256;    // 4106
  SQLITE_IOERR_DIR_CLOSE         = SQLITE_IOERR + 17*256;    // 4362
  SQLITE_IOERR_SHMOPEN           = SQLITE_IOERR + 18*256;    // 4618
  SQLITE_IOERR_SHMSIZE           = SQLITE_IOERR + 19*256;    // 4874
  SQLITE_IOERR_SHMLOCK           = SQLITE_IOERR + 20*256;    // 5130
  SQLITE_IOERR_SHMMAP            = SQLITE_IOERR + 21*256;    // 5386
  SQLITE_IOERR_SEEK              = SQLITE_IOERR + 22*256;    // 5642
  SQLITE_IOERR_DELETE_NOENT      = SQLITE_IOERR + 23*256;    // 5898

  // Fundamental Datatypes
  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  // Special destructor types
  SQLITE_STATIC: IntPtr    = IntPtr(0);
  SQLITE_TRANSIENT: IntPtr = IntPtr(-1);

  SQLITE_UTF8       = 1;
  SQLITE_UTF16LE    = 2;
  SQLITE_UTF16BE    = 3;
  SQLITE_UTF16      = 4;    // Use native byte order
  SQLITE_ANY        = 5;

  SQLITE_OPEN_READONLY         = $00000001;
  SQLITE_OPEN_READWRITE        = $00000002;
  SQLITE_OPEN_CREATE           = $00000004;
  SQLITE_OPEN_DELETEONCLOSE    = $00000008;
  SQLITE_OPEN_EXCLUSIVE        = $00000010;
  SQLITE_OPEN_AUTOPROXY        = $00000020;
  SQLITE_OPEN_URI              = $00000040;
  SQLITE_OPEN_MEMORY           = $00000080;
  SQLITE_OPEN_MAIN_DB          = $00000100;
  SQLITE_OPEN_TEMP_DB          = $00000200;
  SQLITE_OPEN_TRANSIENT_DB     = $00000400;
  SQLITE_OPEN_MAIN_JOURNAL     = $00000800;
  SQLITE_OPEN_TEMP_JOURNAL     = $00001000;
  SQLITE_OPEN_SUBJOURNAL       = $00002000;
  SQLITE_OPEN_MASTER_JOURNAL   = $00004000;
  SQLITE_OPEN_NOMUTEX          = $00008000;
  SQLITE_OPEN_FULLMUTEX        = $00010000;
  SQLITE_OPEN_SHAREDCACHE      = $00020000;
  SQLITE_OPEN_PRIVATECACHE     = $00040000;
  SQLITE_OPEN_WAL              = $00080000;

  SQLITE_IOCAP_ATOMIC                 = $00000001;
  SQLITE_IOCAP_ATOMIC512              = $00000002;
  SQLITE_IOCAP_ATOMIC1K               = $00000004;
  SQLITE_IOCAP_ATOMIC2K               = $00000008;
  SQLITE_IOCAP_ATOMIC4K               = $00000010;
  SQLITE_IOCAP_ATOMIC8K               = $00000020;
  SQLITE_IOCAP_ATOMIC16K              = $00000040;
  SQLITE_IOCAP_ATOMIC32K              = $00000080;
  SQLITE_IOCAP_ATOMIC64K              = $00000100;
  SQLITE_IOCAP_SAFE_APPEND            = $00000200;
  SQLITE_IOCAP_SEQUENTIAL             = $00000400;
  SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN  = $00000800;
  SQLITE_IOCAP_POWERSAFE_OVERWRITE    = $00001000;
  SQLITE_IOCAP_IMMUTABLE              = $00002000;

  SQLITE_FCNTL_LOCKSTATE              = 1;
  SQLITE_GET_LOCKPROXYFILE            = 2;
  SQLITE_SET_LOCKPROXYFILE            = 3;
  SQLITE_LAST_ERRNO                   = 4;
  SQLITE_FCNTL_SIZE_HINT              = 5;
  SQLITE_FCNTL_CHUNK_SIZE             = 6;
  SQLITE_FCNTL_FILE_POINTER           = 7;
  SQLITE_FCNTL_SYNC_OMITTED           = 8;
  SQLITE_FCNTL_WIN32_AV_RETRY         = 9;
  SQLITE_FCNTL_PERSIST_WAL            = 10;
  SQLITE_FCNTL_OVERWRITE              = 11;
  SQLITE_FCNTL_VFSNAME                = 12;
  SQLITE_FCNTL_POWERSAFE_OVERWRITE    = 13;
  SQLITE_FCNTL_PRAGMA                 = 14;
  SQLITE_FCNTL_BUSYHANDLER            = 15;
  SQLITE_FCNTL_TEMPFILENAME           = 16;
  SQLITE_FCNTL_MMAP_SIZE              = 18;
  SQLITE_FCNTL_TRACE                  = 19;
  SQLITE_FCNTL_HAS_MOVED              = 20;
  SQLITE_FCNTL_SYNC                   = 21;
  SQLITE_FCNTL_COMMIT_PHASETWO        = 22;
  SQLITE_FCNTL_WIN32_SET_HANDLE       = 23;

  UNIXFILE_EXCL        = $01;
  UNIXFILE_RDONLY      = $02;
  UNIXFILE_PERSIST_WAL = $04;
  UNIXFILE_DIRSYNC     = $08;
  UNIXFILE_PSOW        = $10;
  UNIXFILE_DELETE      = $20;
  UNIXFILE_URI         = $40;
  UNIXFILE_NOLOCK      = $80;
  UNIXFILE_WARNED      = $100;

  SQLITE_ACCESS_EXISTS    = 0;
  SQLITE_ACCESS_READWRITE = 1;
  SQLITE_ACCESS_READ      = 2;

  NO_LOCK         = 0;
  SHARED_LOCK     = 1;
  RESERVED_LOCK   = 2;
  PENDING_LOCK    = 3;
  EXCLUSIVE_LOCK  = 4;

  PENDING_BYTE  = $40000000;
  RESERVED_BYTE = PENDING_BYTE + 1;

  SQLITE_DEFAULT_SECTOR_SIZE = 4096;
  SQLITE_ANDROID_SECTOR_SIZE = SQLITE_DEFAULT_SECTOR_SIZE;

  SQLITE_SHM_UNLOCK    = 1;
  SQLITE_SHM_LOCK      = 2;
  SQLITE_SHM_SHARED    = 4;
  SQLITE_SHM_EXCLUSIVE = 8;

  //for sqlite3_limit
  SQLITE_LIMIT_LENGTH              = 0;
  SQLITE_LIMIT_SQL_LENGTH          = 1;
  SQLITE_LIMIT_COLUMN              = 2;
  SQLITE_LIMIT_EXPR_DEPTH          = 3;
  SQLITE_LIMIT_COMPOUND_SELECT     = 4;
  SQLITE_LIMIT_VDBE_OP             = 5;
  SQLITE_LIMIT_FUNCTION_ARG        = 6;
  SQLITE_LIMIT_ATTACHED            = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER     = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH       = 10;
  SQLITE_LIMIT_WORKER_THREADS      = 11;

  DEF_LIMIT_EXPR_DEPTH = 1000;

  SQLITE_VTAB_CONSTRAINT_SUPPORT = 1;

  SQLITE_INDEX_CONSTRAINT_EQ     = 2;
  SQLITE_INDEX_CONSTRAINT_GT     = 4;
  SQLITE_INDEX_CONSTRAINT_LE     = 8;
  SQLITE_INDEX_CONSTRAINT_LT     = 16;
  SQLITE_INDEX_CONSTRAINT_GE     = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH  = 64;
  SQLITE_INDEX_CONSTRAINT_LIKE   = 65;
  SQLITE_INDEX_CONSTRAINT_GLOB   = 66;
  SQLITE_INDEX_CONSTRAINT_REGEXP = 67;
  SQLITE_INDEX_SCAN_UNIQUE       = 1;

  CP_SJIS         = 932;
  CP_GBK          = 936;
  CP_UHC          = 949;
  CP_BIG5         = 950;
  CP_JOHAB        = 1361;
  CP_EUC_JIS_2004 = 20932;
  CP_EUC_JP       = 51932;
  CP_EUC_CN       = 51936;
  CP_EUC_KR       = 51949;
  CP_EUC_TW       = 51950;
  CP_GB18030      = 54936;
  CP_UTF_8        = 65001;

type
  u8                  = byte;
  i8                  = ShortInt;
  u16                 = word;
  u32                 = Cardinal;
  i16                 = SmallInt;
  i64                 = Int64;

{$IFDEF LINUX_FPC}
  pInt = pointer;
{$ENDIF}
{$IFDEF DARWIN_FPC}
  pInt = pointer;
{$ENDIF}

  pFuncDef = ^TFuncDef;
  TFuncDef = record
    nArg: i16;            // Number of arguments.  -1 means unlimited */
    iPrefEnc: u8;         // Preferred text encoding (SQLITE_UTF8, 16LE, 16BE) */
    flags: u8;            // Some combination of SQLITE_FUNC_* */
    pUserData: IntPtr;    // User data parameter */
  end;

  pFuncDef3250 = ^TFuncDef3250;
  TFuncDef3250 = packed record
    nArg: i8;             // Number of arguments.  -1 means unlimited */
    padding1: u8;
    padding2: u8;
    padding3: u8;
    funcFlags: u32;       // Some combination of SQLITE_FUNC_* */
    pUserData: IntPtr;    // User data parameter */
  end;

  pSQLite3Db = ^TSQLite3Db;
  TSQLite3Db = record
    zName: IntPtr;    // Name of this database */
    pBt: IntPtr;      // The B*Tree structure for this database file */
    safety_level: u8; // How aggressive at syncing data to disk */
    pSchema: IntPtr;  // Pointer to database schema (possibly shared) */
  end;

  TSQLite3 = record
    pVfs: IntPtr;      // OS Interface */
    pVdbe: IntPtr;     // List of active virtual machines */
    pDfltColl: IntPtr; // The default collating sequence (BINARY) */
    mutex: IntPtr;     // Connection mutex */
    aDb: IntPtr;       // All backends */
    nDb: integer;      // Number of backends currently in use */
  end;

  TSQLite3Context = record
    pFunc: pFuncDef;         // Pointer to function information */
  end;

  TSQLite3Context387 = record
    pOut: IntPtr;            // The return value is stored here */
    pFunc: pFuncDef;         // Pointer to function information */
  end;

  pSQLite3 = IntPtr;
  pSQLite3Stmt = IntPtr;
  pSQLite3Context = IntPtr;
  pSQLite3Value = IntPtr;
  pSQLite3Backup = IntPtr;

  pContext = ^TSQLite3Context;
  pContext387 = ^TSQLite3Context387;

  TCallBackLiteCollation = function (
    pUserData: IntPtr;
    StrSize1: Integer; const pStr1: IntPtr;
    StrSize2: Integer; const pStr2: IntPtr
  ): Integer; cdecl;

  TCallBackLiteFunction = procedure (
    Context: pContext;
    ParamCount: Integer;
    pData: IntPtr
  ); cdecl;

  TCallBackLiteFunction387 = procedure (
    Context: pContext387;
    ParamCount: Integer;
    pData: IntPtr
  ); cdecl;

{$IFDEF CUSTOM_VFS}
  psqlite3_file = ^sqlite3_file;
  psqlite3_customfile = ^sqlite3_customfile;
  psqlite3_io_methods = ^sqlite3_io_methods;
  psqlite3_vfs = ^sqlite3_vfs;
  psqlite3_shm_node = ^sqlite3_shm_node;
  psqlite3_shm = ^sqlite3_shm;
  ppsqlite3_shm = ^psqlite3_shm;

  sqlite3_file = record
    pMethods: psqlite3_io_methods;
  end;

  sqlite3_customfile = record
    Methods: psqlite3_io_methods;
    Vfs: psqlite3_vfs;
    Name: string;
    Stream: TStream;
    LockType: byte;
    DeleteOnClose: boolean;
    LastErrno: integer;
    FileType: integer;
    Shm: psqlite3_shm;
    ShmNode: psqlite3_shm_node;
  end;

  sqlite3_io_methods = record
    iVersion: Integer;
    xClose: function (pFile: psqlite3_file): Integer; cdecl;
    xRead: function (pFile: psqlite3_file; buf: Pointer; iAmt: Integer; iOfst: i64): Integer; cdecl;
    xWrite: function (pFile: psqlite3_file; bug: Pointer; iAmt: Integer; iOfst: i64): Integer; cdecl;
    xTruncate: function (pFile: psqlite3_file; size: i64): Integer; cdecl;
    xSync: function (pFile: psqlite3_file; flags: Integer): Integer; cdecl;
    xFileSize: function (pFile: psqlite3_file; var pSize: i64): Integer; cdecl;
    xLock: function (pFile: psqlite3_file; mode: Integer): Integer; cdecl;
    xUnlock: function (pFile: psqlite3_file; mode: Integer): Integer; cdecl;
    xCheckReservedLock: function (pFile: psqlite3_file; var pResOut: Integer): Integer; cdecl;
    xFileControl: function (pFile: psqlite3_file; op: Integer; pArg: Pointer): Integer; cdecl;
    xSectorSize: function (pFile: psqlite3_file): Integer; cdecl;
    xDeviceCharacteristics: function (pFile: psqlite3_file): Integer; cdecl;
    xShmMap: function (pFile: psqlite3_file; iPg: Integer; pgsz: Integer; i: Integer; p: PPointer): Integer; cdecl;
    xShmLock: function (pFile: psqlite3_file; offset: Integer; n: Integer; flags: Integer): Integer; cdecl;
    xShmBarrier: procedure (pFile: psqlite3_file); cdecl;
    xShmUnmap: function (pFile: psqlite3_file; deleteFlag: Integer): Integer; cdecl;
    xFetch: function (pFile: sqlite3_file; iOfst: i64; iAmt: Integer; pp: PPointer): Integer; cdecl;
    xUnfetch: function (pFile: sqlite3_file; iOfst: i64; p: Pointer): Integer; cdecl;
  end;

  sqlite3_vfs = record
    iVersion: Integer;
    szOsFile: Integer;
    mxPathname: Integer;
    pNext: psqlite3_vfs;
    zName: PAnsiChar;
    pAppData: Pointer;
    xOpen: function (pVfs: psqlite3_vfs; zName: PAnsiChar; pFile: psqlite3_file; flags: Integer; var pOutFlags: Integer): Integer; cdecl;
    xDelete: function (pVfs: psqlite3_vfs; zName: PAnsiChar; syncDir: Integer): Integer; cdecl;
    xAccess: function (pVfs: psqlite3_vfs; zName: PAnsiChar; flags: Integer; var pResOut: Integer): Integer; cdecl;
    xFullPathname: function (pVfs: psqlite3_vfs; zName: PAnsiChar; nOut: Integer; zOut: PAnsiChar): Integer; cdecl;
    xDlOpen: function (pVfs: psqlite3_vfs; zFilename: PAnsiChar): Pointer; cdecl;
    xDlError: procedure (pVfs: psqlite3_vfs; nByte: Integer; zErrMsg: PAnsiChar); cdecl;
    xDlSym: function (pVfs: psqlite3_vfs; ptr: Pointer; zSymbol: PAnsiChar): Pointer; cdecl;
    xDlClose: procedure (pVfs: psqlite3_vfs; ptr: Pointer); cdecl;
    xRandomness: function (pVfs: psqlite3_vfs; nBuf: Integer; zOut: PAnsiChar): Integer; cdecl;
    xSleep: function (pVfs: psqlite3_vfs; microseconds: Integer): Integer; cdecl;
    xCurrentTime: function (pVfs: psqlite3_vfs; var tm: double): Integer; cdecl;
    xGetLastError: function (pVfs: psqlite3_vfs; l: Integer; v: PAnsiChar): Integer; cdecl;
    xCurrentTimeInt64: function (pVfs: psqlite3_vfs; var tm: i64): Integer; cdecl;
    xSetSystemCall: function (pVfs: psqlite3_vfs; zName: PAnsiChar; p: pointer): Integer; cdecl;
    xGetSystemCall: function (pVfs: psqlite3_vfs; zName: PAnsiChar): pointer; cdecl;
    xNextSystemCall: function (pVfs: psqlite3_vfs; zName: PAnsiChar): PAnsiChar; cdecl;
  end;

  sqlite3_shm_node = record
//    unixInodeInfo *pInode;
//    sqlite3_mutex *mutex;
//    char *zFilename;
//    int h;
    szRegion: integer;
    nRegion: u16;
    isReadonly: u8;
    apRegion: array of pointer;
    nRef: integer;
    pFirst: psqlite3_shm;
  end;

  sqlite3_shm = record
    pShmNode: psqlite3_shm_node;
    pNext: psqlite3_shm;
    hasMutex: u8;
    id: u8;
    sharedMask: u16;
    exclMask: u16;
  end;
{$ENDIF}

{$IFDEF CODEC}
  ptm = ^stm;
  stm = packed record
    tm_sec: integer;
    tm_min: integer;
    tm_hour: integer;
    tm_mday: integer;
    tm_mon: integer;
    tm_year: integer;
    tm_wday: integer;
    tm_yday: integer;
    tm_isdst: integer;
  end;

  psqlite3_codec = ^sqlite3_codec;

  sqlite3_codec = record
    xKeyV2: function (pSQLite: pSQLite3; dbName, key: PAnsiChar; size: Integer): integer; cdecl;
    xRekeyV2: function (pSQLite: pSQLite3; dbName, newkey: PAnsiChar; size: Integer): integer; cdecl;
    xActivateSee: procedure (zPassPhrase: PAnsiChar); cdecl;
    xCodecAttach: function (pSQLite: pSQLite3; nDb: integer; key: PAnsiChar; size: Integer; Algorithm: TLiteEncryptionAlgorithm = DefaultEncryptionAlgorithm): integer; cdecl;
    xCodecGetKey: procedure (pSQLite: pSQLite3; nDb: integer; var key: pointer; var size: {$IFDEF POSIX}pointer{$ELSE}pInt{$ENDIF}); cdecl;
    xCustomPragma: function(pSQLite: pSQLite3; nDb: Integer; Left: PAnsiChar; Right: PAnsiChar): integer; cdecl;
  {$IFDEF USE_CUSTOM_LOCALTIME}
    xCustomLocaltime: function(timer: pointer): ptm; cdecl;
  {$ENDIF}
  end;
{$ENDIF}

  // functions
  Tsqlite3_malloc = function(
    n: integer
  ): IntPtr; cdecl;

  Tsqlite3_open = function(
    filename: PAnsiChar;    // Database filename (UTF-8)
    out ppDb: pSQLite3      // OUT: SQLite db handle
  ): Integer; cdecl;

  Tsqlite3_open16 = function(
    filename: PWideChar;    // Database filename (UTF-16)
    out ppDb: pSQLite3      // OUT: SQLite db handle
  ): Integer; cdecl;

  Tsqlite3_open_v2 = function(
    filename: PAnsiChar;    // Database filename (UTF-8)
    out ppDb: pSQLite3;     // OUT: SQLite db handle
    flags: Integer;         // Flags
    zVfs: PAnsiChar         // Name of VFS module to use
  ): Integer; cdecl;

  Tsqlite3_close = function(
    pDb: pSQLite3
  ): Integer; cdecl;

  Tsqlite3_errcode = function(
    pDb: pSQLite3
  ): Integer; cdecl;

  Tsqlite3_extended_errcode = function(
    pDb: pSQLite3
  ): Integer; cdecl;

  Tsqlite3_extended_result_codes = function(
    pDb: pSQLite3;
    onoff: Integer
  ): Integer; cdecl;

  Tsqlite3_errmsg = function(
    pDb: pSQLite3
  ): PAnsiChar; cdecl;

  Tsqlite3_last_insert_rowid = function(
    pDb: pSQLite3
  ): Int64; cdecl;

  Tsqlite3_changes = function(
    pDb: pSQLite3
  ): Integer; cdecl;

  Tsqlite3_prepare_v2 = function(
    db: pSQLite3;           // Database handle
    zSql: PAnsiChar;        // SQL statement, UTF-8 encoded
    nByte: Integer;         // Maximum length of zSql in bytes.
    out ppStmt: pSQLite3Stmt;  // OUT: Statement handle
    out pzTail: IntPtr          // OUT: Pointer to unused portion of zSql
  ): Integer; cdecl;

  Tsqlite3_step = function(
    pStmt: pSQLite3Stmt
  ): Integer; cdecl;

  Tsqlite3_reset = function(
    pStmt: pSQLite3Stmt
  ): Integer; cdecl;

  Tsqlite3_finalize = function(
    pStmt: pSQLite3Stmt
  ): Integer; cdecl;

  Tsqlite3_column_count = function(
    pStmt: pSQLite3Stmt
  ): Integer; cdecl;

  Tsqlite3_column_type = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): Integer; cdecl;

  Tsqlite3_column_name = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): PAnsiChar; cdecl;

  Tsqlite3_column_origin_name = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): PAnsiChar; cdecl;

  Tsqlite3_column_table_name = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): PAnsiChar; cdecl;

  Tsqlite3_column_database_name = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): PAnsiChar; cdecl;

  Tsqlite3_column_decltype = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): PAnsiChar; cdecl;

  Tsqlite3_table_column_metadata = function(
    pDb: pSQLite3;
    zDbName: IntPtr;
    zTableName: IntPtr;
    zColumnName: IntPtr;
    out pzDataType: IntPtr;
    out pzCollSeq: IntPtr;
    out pNotNull: Integer;
    out pPrimaryKey: Integer;
    out pAutoinc: Integer
  ): Integer; cdecl;

  Tsqlite3_column_blob = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): IntPtr; cdecl;

  Tsqlite3_column_bytes = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): Integer; cdecl;

  Tsqlite3_column_double = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): double; cdecl;

  Tsqlite3_column_int = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): Integer; cdecl;

  Tsqlite3_column_int64 = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): int64; cdecl;

  Tsqlite3_column_text = function(
    pStmt: pSQLite3Stmt; iCol: Integer
  ): PAnsiChar; cdecl;

  Tsqlite3_bind_parameter_count = function(
    pStmt: pSQLite3Stmt
  ): Integer; cdecl;

  Tsqlite3_bind_parameter_name = function(
    pStmt: pSQLite3Stmt; Index: integer
  ): PAnsiChar; cdecl;

  Tsqlite3_bind_blob = function(
    pStmt: pSQLite3Stmt; Index: Integer; pBlob: IntPtr; Size: Integer; pDestrType: IntPtr
  ): Integer; cdecl;

  Tsqlite3_bind_zeroblob = function(
    pStmt: pSQLite3Stmt; Index: Integer; Size: Integer
  ): Integer; cdecl;

  Tsqlite3_bind_double = function(
    pStmt: pSQLite3Stmt; Index: Integer; Value: Double
  ): Integer; cdecl;

  Tsqlite3_bind_int = function(
    pStmt: pSQLite3Stmt; Index: Integer; Value: Integer
  ): Integer; cdecl;

  Tsqlite3_bind_int64 = function(
    pStmt: pSQLite3Stmt; Index: Integer; Value: Int64
  ): Integer; cdecl;

  Tsqlite3_bind_null = function(
    pStmt: pSQLite3Stmt; Index: Integer
  ): Integer; cdecl;

  Tsqlite3_bind_text = function(
    pStmt: pSQLite3Stmt; Index: Integer; Value: PAnsiChar; Size: Integer; pDestrType: IntPtr
  ): Integer; cdecl;

  Tsqlite3_result_blob = procedure(
    pContext: pSQLite3Context; pBlob: IntPtr; Size: Integer; pDestrType: IntPtr
  ); cdecl;

  Tsqlite3_result_double = procedure(
    pContext: pSQLite3Context; Value: Double
  ); cdecl;

  Tsqlite3_result_error = procedure(
    pContext: pSQLite3Context; pMsg: IntPtr; Size: Integer
  ); cdecl;

  Tsqlite3_result_error16 = procedure(
    pContext: pSQLite3Context; pMsg: IntPtr; Size: Integer
  ); cdecl;

  Tsqlite3_result_error_toobig = procedure(
    pContext: pSQLite3Context
  ); cdecl;

  Tsqlite3_result_error_nomem = procedure(
    pContext: pSQLite3Context
  ); cdecl;

  Tsqlite3_result_error_code = procedure(
    pContext: pSQLite3Context; ErrorCode: Integer
  ); cdecl;

  Tsqlite3_result_int = procedure(
    pContext: pSQLite3Context; Value: Integer
  ); cdecl;

  Tsqlite3_result_int64 = procedure(
    pContext: pSQLite3Context; Value: Int64
  ); cdecl;

  Tsqlite3_result_null = procedure(
    pContext: pSQLite3Context
  ); cdecl;

  Tsqlite3_result_text = procedure(
    pContext: pSQLite3Context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); cdecl;

  Tsqlite3_result_text16 = procedure(
    pContext: pSQLite3Context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); cdecl;

  Tsqlite3_result_text16le = procedure(
    pContext: pSQLite3Context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); cdecl;

  Tsqlite3_result_text16be = procedure(
    pContext: pSQLite3Context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); cdecl;

  Tsqlite3_result_value = procedure(
    pContext: pSQLite3Context; Value: pSQLite3Value
  ); cdecl;

  Tsqlite3_result_zeroblob = procedure(
    pContext: pSQLite3Context; Size: Integer
  ); cdecl;

  Tsqlite3_value_blob = function(
    pValue: IntPtr
  ): IntPtr; cdecl;

  Tsqlite3_value_bytes = function(
    pValue: IntPtr
  ): Integer; cdecl;

  Tsqlite3_value_bytes16 = function(
    pValue: IntPtr
  ): Integer; cdecl;

  Tsqlite3_value_double = function(
    pValue: IntPtr
  ): Double; cdecl;

  Tsqlite3_value_int = function(
    pValue: IntPtr
  ): Integer; cdecl;

  Tsqlite3_value_int64 = function(
    pValue: IntPtr
  ): Int64; cdecl;

  Tsqlite3_value_text = function(
    pValue: IntPtr
  ): IntPtr; cdecl;

  Tsqlite3_value_text16 = function(
    pValue: IntPtr
  ): IntPtr; cdecl;

  Tsqlite3_value_type = function(
    pValue: IntPtr
  ): Integer; cdecl;

  Tsqlite3_user_data = function(
    pContext: pSQLite3Context
  ): IntPtr; cdecl;

  Tsqlite3_libversion = function(
  ): PAnsiChar; cdecl;

  Tsqlite3_libversion_number = function(
  ): Integer; cdecl;

  Tsqlite3_create_collation = function(
    pSQLite: pSQLite3; zName: PAnsiChar; eTextRep: Integer; userData: IntPtr; func: IntPtr
  ): Integer; cdecl;

  Tsqlite3_create_collation16 = function(
    pSQLite: pSQLite3; zName: PWideChar; eTextRep: Integer; userData: IntPtr; func: IntPtr
  ): Integer; cdecl;

  Tsqlite3_create_function = function(
    pSQLite: pSQLite3; zFunctionName: PAnsiChar; nArg: Integer; eTextRep: Integer; pApp: IntPtr; xFunc: IntPtr; xStep: IntPtr; xFinal: IntPtr
  ): Integer; cdecl;

  Tsqlite3_create_function16 = function(
    pSQLite: pSQLite3; zFunctionName: PWideChar; nArg: Integer; eTextRep: Integer; pApp: IntPtr; xFunc: IntPtr; xStep: IntPtr; xFinal: IntPtr
  ): Integer; cdecl;

  Tsqlite3_create_function_v2 = function(
    pSQLite: pSQLite3; zFunctionName: PAnsiChar; nArg: Integer; eTextRep: Integer; pApp: IntPtr; xFunc: IntPtr; xStep: IntPtr; xFinal: IntPtr; xDestroy: IntPtr
  ): Integer; cdecl;

  Tsqlite3_overload_function = function(
    pSQLite: pSQLite3; zFunctionName: PAnsiChar; nArg: Integer
  ): Integer; cdecl;

  Tsqlite3_create_module = function(
    pSQLite: pSQLite3; zName: IntPtr; p: IntPtr; pClientData: IntPtr
  ): Integer; cdecl;

  Tsqlite3_create_module_v2 = function(
    pSQLite: pSQLite3; zName: IntPtr; p: IntPtr; pClientData: IntPtr; xDestroy: IntPtr
  ): Integer; cdecl;

  Tsqlite3_declare_vtab = function(
    pSQLite: pSQLite3; const zCreateTable: IntPtr
  ): Integer; cdecl;

  Tsqlite3_vtab_config = function(
    pSQLite: pSQLite3; op: Integer; flag: Integer
  ): Integer; cdecl;

  Tsqlite3_enable_shared_cache = function(
    Value: Integer
  ): Integer; cdecl;

  Tsqlite3_enable_load_extension = function(
    pSQLite: pSQLite3; OnOff: Integer
  ): Integer; cdecl;

  Tsqlite3_busy_timeout = function(
    pSQLite: pSQLite3; MilliSeconds: Integer
  ): Integer; cdecl;

  Tsqlite3_busy_handler = function(
    pSQLite: pSQLite3; func: IntPtr; userData: IntPtr
  ): Integer; cdecl;

  Tsqlite3_key = function(
    pSQLite: pSQLite3; key: PAnsiChar; size: Integer
  ): Integer; cdecl;

  Tsqlite3_key_v2 = function(
    pSQLite: pSQLite3; zDbName, key: PAnsiChar; size: Integer
  ): Integer; cdecl;

  Tsqlite3_rekey = function(
    pSQLite: pSQLite3; newkey: PAnsiChar; size: Integer
  ): Integer; cdecl;

  Tsqlite3_rekey_v2 = function(
    pSQLite: pSQLite3; zDbName, newkey: PAnsiChar; size: Integer
  ): Integer; cdecl;

  Tsqlite3_backup_init = function(
    pDest: pSQLite3; zDestName: PAnsiChar;
    pSource: pSQLite3; zSourceName: PAnsiChar
  ): pSQLite3Backup; cdecl;

  Tsqlite3_backup_step = function(
    p: pSQLite3Backup; nPage: Integer
  ): Integer; cdecl;

  Tsqlite3_backup_finish = function(
    p: pSQLite3Backup
  ): Integer; cdecl;

  Tsqlite3_backup_remaining = function(
    p: pSQLite3Backup
  ): Integer; cdecl;

  Tsqlite3_backup_pagecount = function(
    p: pSQLite3Backup
  ): Integer; cdecl;

  Tsqlite3_db_release_memory = function(
    pDb: pSQLite3
  ): Integer; cdecl;

  Tsqlite3_db_readonly = function(
    pDb: pSQLite3; zDbName: PAnsiChar
  ): Integer; cdecl;

  Tsqlite3_limit = function(
    pDb: pSQLite3;
    id: integer;
    newVal: Integer
  ): Integer; cdecl;

  Tsqlite3_uri_parameter = function(
    zFilename: PAnsiChar;
    zParam: PAnsiChar
  ): PAnsiChar; cdecl;

  Tsqlite3_interrupt = procedure(
    pDb: pSQLite3
  ); cdecl;

  TCustomSQLite3API = class
  private
  {$IFDEF MSWINDOWS}
    hLiteLib: HMODULE;
  {$ENDIF}
  {$IFDEF POSIX}
    hLiteLib: NativeUInt;
  {$ENDIF}
  {$IFDEF UNIX}
    hLiteLib: IntPtr;
  {$ENDIF}

    FClientLibrary: string;
    FDirect: boolean;
    FInitialized: Boolean;

    procedure LoadClientLibrary;
    procedure UnLoadClientLibrary;

  public
    sqlite3_malloc: Tsqlite3_malloc;
    sqlite3_open: Tsqlite3_open;
    sqlite3_open16: Tsqlite3_open16;
    sqlite3_open_v2: Tsqlite3_open_v2;
    sqlite3_close: Tsqlite3_close;
    sqlite3_errcode: Tsqlite3_errcode;
    sqlite3_extended_errcode: Tsqlite3_extended_errcode;
    sqlite3_extended_result_codes: Tsqlite3_extended_result_codes;
    sqlite3_errmsg: Tsqlite3_errmsg;
    sqlite3_last_insert_rowid: Tsqlite3_last_insert_rowid;
    sqlite3_changes: Tsqlite3_changes;
    sqlite3_prepare_v2: Tsqlite3_prepare_v2;
    sqlite3_step: Tsqlite3_step;
    sqlite3_reset: Tsqlite3_reset;
    sqlite3_finalize: Tsqlite3_finalize;
    sqlite3_column_count: Tsqlite3_column_count;
    sqlite3_column_type: Tsqlite3_column_type;
    sqlite3_column_name: Tsqlite3_column_name;
    sqlite3_column_origin_name: Tsqlite3_column_origin_name;
    sqlite3_column_table_name: Tsqlite3_column_table_name;
    sqlite3_column_database_name: Tsqlite3_column_database_name;
    sqlite3_column_decltype: Tsqlite3_column_decltype;
    sqlite3_table_column_metadata: Tsqlite3_table_column_metadata;
    sqlite3_column_blob: Tsqlite3_column_blob;
    sqlite3_column_bytes: Tsqlite3_column_bytes;
    sqlite3_column_double: Tsqlite3_column_double;
    sqlite3_column_int: Tsqlite3_column_int;
    sqlite3_column_int64: Tsqlite3_column_int64;
    sqlite3_column_text: Tsqlite3_column_text;
    sqlite3_bind_parameter_count: Tsqlite3_bind_parameter_count;
    sqlite3_bind_parameter_name: Tsqlite3_bind_parameter_name;
    sqlite3_bind_blob: Tsqlite3_bind_blob;
    sqlite3_bind_zeroblob: Tsqlite3_bind_zeroblob;
    sqlite3_bind_double: Tsqlite3_bind_double;
    sqlite3_bind_int: Tsqlite3_bind_int;
    sqlite3_bind_int64: Tsqlite3_bind_int64;
    sqlite3_bind_null: Tsqlite3_bind_null;
    sqlite3_bind_text: Tsqlite3_bind_text;
    sqlite3_result_blob: Tsqlite3_result_blob;
    sqlite3_result_double: Tsqlite3_result_double;
    sqlite3_result_error: Tsqlite3_result_error;
    sqlite3_result_error16: Tsqlite3_result_error16;
    sqlite3_result_error_toobig: Tsqlite3_result_error_toobig;
    sqlite3_result_error_nomem: Tsqlite3_result_error_nomem;
    sqlite3_result_error_code: Tsqlite3_result_error_code;
    sqlite3_result_int: Tsqlite3_result_int;
    sqlite3_result_int64: Tsqlite3_result_int64;
    sqlite3_result_null: Tsqlite3_result_null;
    sqlite3_result_text: Tsqlite3_result_text;
    sqlite3_result_text16: Tsqlite3_result_text16;
    sqlite3_result_text16le: Tsqlite3_result_text16le;
    sqlite3_result_text16be: Tsqlite3_result_text16be;
    sqlite3_result_value: Tsqlite3_result_value;
    sqlite3_result_zeroblob: Tsqlite3_result_zeroblob;
    sqlite3_value_blob: Tsqlite3_value_blob;
    sqlite3_value_bytes: Tsqlite3_value_bytes;
    sqlite3_value_bytes16: Tsqlite3_value_bytes16;
    sqlite3_value_double: Tsqlite3_value_double;
    sqlite3_value_int: Tsqlite3_value_int;
    sqlite3_value_int64: Tsqlite3_value_int64;
    sqlite3_value_text: Tsqlite3_value_text;
    sqlite3_value_text16: Tsqlite3_value_text16;
    sqlite3_value_type: Tsqlite3_value_type;
    sqlite3_user_data: Tsqlite3_user_data;
    sqlite3_libversion: Tsqlite3_libversion;
    sqlite3_libversion_number: Tsqlite3_libversion_number;
    sqlite3_create_collation: Tsqlite3_create_collation;
    sqlite3_create_collation16: Tsqlite3_create_collation16;
    sqlite3_create_function: Tsqlite3_create_function;
    sqlite3_create_function16: Tsqlite3_create_function16;
    sqlite3_overload_function: Tsqlite3_overload_function;
    sqlite3_create_module: Tsqlite3_create_module;
    sqlite3_create_module_v2: Tsqlite3_create_module_v2;
    sqlite3_declare_vtab: Tsqlite3_declare_vtab;
    sqlite3_vtab_config: Tsqlite3_vtab_config;
    sqlite3_enable_shared_cache: Tsqlite3_enable_shared_cache;
    sqlite3_enable_load_extension: Tsqlite3_enable_load_extension;
    sqlite3_busy_timeout: Tsqlite3_busy_timeout;
    sqlite3_busy_handler: Tsqlite3_busy_handler;
    sqlite3_key: Tsqlite3_key;
    sqlite3_key_v2: Tsqlite3_key_v2;
    sqlite3_rekey: Tsqlite3_rekey;
    sqlite3_rekey_v2: Tsqlite3_rekey_v2;
    sqlite3_backup_init: Tsqlite3_backup_init;
    sqlite3_backup_step: Tsqlite3_backup_step;
    sqlite3_backup_finish: Tsqlite3_backup_finish;
    sqlite3_backup_remaining: Tsqlite3_backup_remaining;
    sqlite3_backup_pagecount: Tsqlite3_backup_pagecount;
    sqlite3_db_release_memory: Tsqlite3_db_release_memory;
    sqlite3_db_readonly: Tsqlite3_db_readonly;
    sqlite3_limit: Tsqlite3_limit;
    sqlite3_uri_parameter: Tsqlite3_uri_parameter;
    sqlite3_interrupt: Tsqlite3_interrupt;

    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure UnInitialize;

    procedure GetLiteErrorCode(SQLite: pSQLite3; var ErrorCode: integer);
    procedure GetLiteErrorMsg(SQLite: pSQLite3; var ErrorMsg: string);
    procedure GetPredefinedErrorMsg(ErrorCode: integer; var ErrorMsg: string);
    function IsMetaDataAPIAvailable: boolean;

    function GetProc(const Name: string; NotLinkPtr: IntPtr): IntPtr; overload;
    function GetProc(const Name: string): IntPtr; overload;

    procedure Assign(Source: TCustomSQLite3API); virtual;

    property ClientLibrary: string read FClientLibrary write FClientLibrary;
    property Initialized: boolean read FInitialized;
    property Direct: boolean read FDirect write FDirect;
  end;

  TSQLite3API = class (TCustomSQLite3API)
  public
    SQLite: pSQLite3;

    procedure Assign(Source: TCustomSQLite3API); override;

    procedure GetLiteErrorCode(var ErrorCode: integer);
    procedure GetLiteErrorMsg(var ErrorMsg: string);
  end;

var
  LockInit: TCriticalSection;

implementation

uses
{$IFNDEF NOSTATIC}
{$IFDEF VIRTUAL_QUERY}
  LiteStaticCallVirtual,
{$ELSE}
{$IFNDEF UNIDACPRO}
  LiteStaticCall,
{$ELSE}
  LiteStaticCallUni,
{$ENDIF}
{$ENDIF}
{$ENDIF}
  CRFunctions;


procedure NotLink;
begin
  raise Exception.Create(SFunctionNotLinked);
end;

procedure NotLinkEncryption;
begin
  raise Exception.Create('SQLite DLL that you are using does not support SQLite database encryption. ' + #13 +
                         'Please download a new DLL or recompile the existing one with encryption support. ');
end;

procedure InitFunctions(const API: TCustomSQLite3API);
begin
  API.sqlite3_malloc := API.GetProc('sqlite3_malloc');
  API.sqlite3_open := API.GetProc('sqlite3_open');
  API.sqlite3_open16 := API.GetProc('sqlite3_open16');
  API.sqlite3_open_v2 := API.GetProc('sqlite3_open_v2');
  API.sqlite3_close := API.GetProc('sqlite3_close');
  API.sqlite3_errcode := API.GetProc('sqlite3_errcode');
  API.sqlite3_extended_errcode := API.GetProc('sqlite3_extended_errcode');
  API.sqlite3_extended_result_codes := API.GetProc('sqlite3_extended_result_codes');
  API.sqlite3_errmsg := API.GetProc('sqlite3_errmsg');
  API.sqlite3_last_insert_rowid := API.GetProc('sqlite3_last_insert_rowid');
  API.sqlite3_changes := API.GetProc('sqlite3_changes');
  API.sqlite3_prepare_v2 := API.GetProc('sqlite3_prepare_v2');
  API.sqlite3_step := API.GetProc('sqlite3_step');
  API.sqlite3_reset := API.GetProc('sqlite3_reset');
  API.sqlite3_finalize := API.GetProc('sqlite3_finalize');
  API.sqlite3_column_count := API.GetProc('sqlite3_column_count');
  API.sqlite3_column_type := API.GetProc('sqlite3_column_type');
  API.sqlite3_column_name := API.GetProc('sqlite3_column_name');
  API.sqlite3_column_origin_name := API.GetProc('sqlite3_column_origin_name');
  API.sqlite3_column_table_name := API.GetProc('sqlite3_column_table_name');
  API.sqlite3_column_database_name := API.GetProc('sqlite3_column_database_name');
  API.sqlite3_column_decltype := API.GetProc('sqlite3_column_decltype');
  API.sqlite3_table_column_metadata := API.GetProc('sqlite3_table_column_metadata');
  API.sqlite3_column_blob := API.GetProc('sqlite3_column_blob');
  API.sqlite3_column_bytes := API.GetProc('sqlite3_column_bytes');
  API.sqlite3_column_double := API.GetProc('sqlite3_column_double');
  API.sqlite3_column_int := API.GetProc('sqlite3_column_int');
  API.sqlite3_column_int64 := API.GetProc('sqlite3_column_int64');
  API.sqlite3_column_text := API.GetProc('sqlite3_column_text');
  API.sqlite3_bind_parameter_count := API.GetProc('sqlite3_bind_parameter_count');
  API.sqlite3_bind_parameter_name := API.GetProc('sqlite3_bind_parameter_name');
  API.sqlite3_bind_blob := API.GetProc('sqlite3_bind_blob');
  API.sqlite3_bind_zeroblob := API.GetProc('sqlite3_bind_zeroblob');
  API.sqlite3_bind_double := API.GetProc('sqlite3_bind_double');
  API.sqlite3_bind_int := API.GetProc('sqlite3_bind_int');
  API.sqlite3_bind_int64 := API.GetProc('sqlite3_bind_int64');
  API.sqlite3_bind_null := API.GetProc('sqlite3_bind_null');
  API.sqlite3_bind_text := API.GetProc('sqlite3_bind_text');
  API.sqlite3_result_blob := API.GetProc('sqlite3_result_blob');
  API.sqlite3_result_double := API.GetProc('sqlite3_result_double');
  API.sqlite3_result_error := API.GetProc('sqlite3_result_error');
  API.sqlite3_result_error16 := API.GetProc('sqlite3_result_error16');
  API.sqlite3_result_error_toobig := API.GetProc('sqlite3_result_error_toobig');
  API.sqlite3_result_error_nomem := API.GetProc('sqlite3_result_error_nomem');
  API.sqlite3_result_error_code := API.GetProc('sqlite3_result_error_code');
  API.sqlite3_result_int := API.GetProc('sqlite3_result_int');
  API.sqlite3_result_int64 := API.GetProc('sqlite3_result_int64');
  API.sqlite3_result_null := API.GetProc('sqlite3_result_null');
  API.sqlite3_result_text := API.GetProc('sqlite3_result_text');
  API.sqlite3_result_text16 := API.GetProc('sqlite3_result_text16');
  API.sqlite3_result_text16le := API.GetProc('sqlite3_result_text16le');
  API.sqlite3_result_text16be := API.GetProc('sqlite3_result_text16be');
  API.sqlite3_result_value := API.GetProc('sqlite3_result_value');
  API.sqlite3_result_zeroblob := API.GetProc('sqlite3_result_zeroblob');
  API.sqlite3_value_blob := API.GetProc('sqlite3_value_blob');;
  API.sqlite3_value_bytes := API.GetProc('sqlite3_value_bytes');
  API.sqlite3_value_bytes16 := API.GetProc('sqlite3_value_bytes16');
  API.sqlite3_value_double := API.GetProc('sqlite3_value_double');
  API.sqlite3_value_int := API.GetProc('sqlite3_value_int');
  API.sqlite3_value_int64 := API.GetProc('sqlite3_value_int64');
  API.sqlite3_value_text := API.GetProc('sqlite3_value_text');
  API.sqlite3_value_text16 := API.GetProc('sqlite3_value_text16');
  API.sqlite3_value_type := API.GetProc('sqlite3_value_type');
  API.sqlite3_user_data := API.GetProc('sqlite3_user_data');
  API.sqlite3_libversion := API.GetProc('sqlite3_libversion');
  API.sqlite3_libversion_number := API.GetProc('sqlite3_libversion_number');
  API.sqlite3_create_collation := API.GetProc('sqlite3_create_collation');
  API.sqlite3_create_collation16 := API.GetProc('sqlite3_create_collation16');
  API.sqlite3_create_function := API.GetProc('sqlite3_create_function');
  API.sqlite3_create_function16 := API.GetProc('sqlite3_create_function16');
  API.sqlite3_overload_function := API.GetProc('sqlite3_overload_function');
  API.sqlite3_create_module := API.GetProc('sqlite3_create_module');
  API.sqlite3_create_module_v2 := API.GetProc('sqlite3_create_module_v2');
  API.sqlite3_declare_vtab := API.GetProc('sqlite3_declare_vtab');
  API.sqlite3_vtab_config := API.GetProc('sqlite3_vtab_config');
  API.sqlite3_enable_shared_cache := API.GetProc('sqlite3_enable_shared_cache');
  API.sqlite3_enable_load_extension := API.GetProc('sqlite3_enable_load_extension');
  API.sqlite3_busy_timeout := API.GetProc('sqlite3_busy_timeout');
  API.sqlite3_busy_handler := API.GetProc('sqlite3_busy_handler');
  API.sqlite3_key := API.GetProc('sqlite3_key', @NotLinkEncryption);
  API.sqlite3_key_v2 := API.GetProc('sqlite3_key_v2', @NotLinkEncryption);
  API.sqlite3_rekey := API.GetProc('sqlite3_rekey', @NotLinkEncryption);
  API.sqlite3_rekey_v2 := API.GetProc('sqlite3_rekey_v2', @NotLinkEncryption);
  API.sqlite3_backup_init := API.GetProc('sqlite3_backup_init');
  API.sqlite3_backup_step := API.GetProc('sqlite3_backup_step');
  API.sqlite3_backup_finish := API.GetProc('sqlite3_backup_finish');
  API.sqlite3_backup_remaining := API.GetProc('sqlite3_backup_remaining');
  API.sqlite3_backup_pagecount := API.GetProc('sqlite3_backup_pagecount');
  API.sqlite3_db_release_memory := API.GetProc('sqlite3_db_release_memory');
  API.sqlite3_db_readonly := API.GetProc('sqlite3_db_readonly');
  API.sqlite3_limit := API.GetProc('sqlite3_limit');
  API.sqlite3_uri_parameter := API.GetProc('sqlite3_uri_parameter');
  API.sqlite3_interrupt := API.GetProc('sqlite3_interrupt');
end;

{ TCustomSQLite3API }

procedure TCustomSQLite3API.Assign(Source: TCustomSQLite3API);
begin
  sqlite3_malloc := Source.sqlite3_malloc;
  sqlite3_open := Source.sqlite3_open;
  sqlite3_open16 := Source.sqlite3_open16;
  sqlite3_open_v2 := Source.sqlite3_open_v2;
  sqlite3_close := Source.sqlite3_close;
  sqlite3_errcode := Source.sqlite3_errcode;
  sqlite3_extended_errcode := Source.sqlite3_extended_errcode;
  sqlite3_extended_result_codes := Source.sqlite3_extended_result_codes;
  sqlite3_errmsg := Source.sqlite3_errmsg;
  sqlite3_last_insert_rowid := Source.sqlite3_last_insert_rowid;
  sqlite3_changes := Source.sqlite3_changes;
  sqlite3_prepare_v2 := Source.sqlite3_prepare_v2;
  sqlite3_step := Source.sqlite3_step;
  sqlite3_reset := Source.sqlite3_reset;
  sqlite3_finalize := Source.sqlite3_finalize;
  sqlite3_column_count := Source.sqlite3_column_count;
  sqlite3_column_type := Source.sqlite3_column_type;
  sqlite3_column_name := Source.sqlite3_column_name;
  sqlite3_column_origin_name := Source.sqlite3_column_origin_name;
  sqlite3_column_table_name := Source.sqlite3_column_table_name;
  sqlite3_column_database_name := Source.sqlite3_column_database_name;
  sqlite3_column_decltype := Source.sqlite3_column_decltype;
  sqlite3_table_column_metadata := Source.sqlite3_table_column_metadata;
  sqlite3_column_blob := Source.sqlite3_column_blob;
  sqlite3_column_bytes := Source.sqlite3_column_bytes;
  sqlite3_column_double := Source.sqlite3_column_double;
  sqlite3_column_int := Source.sqlite3_column_int;
  sqlite3_column_int64 := Source.sqlite3_column_int64;
  sqlite3_column_text := Source.sqlite3_column_text;
  sqlite3_bind_parameter_count := Source.sqlite3_bind_parameter_count;
  sqlite3_bind_parameter_name := Source.sqlite3_bind_parameter_name;
  sqlite3_bind_blob := Source.sqlite3_bind_blob;
  sqlite3_bind_zeroblob := Source.sqlite3_bind_zeroblob;
  sqlite3_bind_double := Source.sqlite3_bind_double;
  sqlite3_bind_int := Source.sqlite3_bind_int;
  sqlite3_bind_int64 := Source.sqlite3_bind_int64;
  sqlite3_bind_null := Source.sqlite3_bind_null;
  sqlite3_bind_text := Source.sqlite3_bind_text;
  sqlite3_result_blob := Source.sqlite3_result_blob;
  sqlite3_result_double := Source.sqlite3_result_double;
  sqlite3_result_error := Source.sqlite3_result_error;
  sqlite3_result_error16 := Source.sqlite3_result_error16;
  sqlite3_result_error_toobig := Source.sqlite3_result_error_toobig;
  sqlite3_result_error_nomem := Source.sqlite3_result_error_nomem;
  sqlite3_result_error_code := Source.sqlite3_result_error_code;
  sqlite3_result_int := Source.sqlite3_result_int;
  sqlite3_result_int64 := Source.sqlite3_result_int64;
  sqlite3_result_null := Source.sqlite3_result_null;
  sqlite3_result_text := Source.sqlite3_result_text;
  sqlite3_result_text16 := Source.sqlite3_result_text16;
  sqlite3_result_text16le := Source.sqlite3_result_text16le;
  sqlite3_result_text16be := Source.sqlite3_result_text16be;
  sqlite3_result_value := Source.sqlite3_result_value;
  sqlite3_result_zeroblob := Source.sqlite3_result_zeroblob;
  sqlite3_value_blob := Source.sqlite3_value_blob;
  sqlite3_value_bytes := Source.sqlite3_value_bytes;
  sqlite3_value_bytes16 := Source.sqlite3_value_bytes16;
  sqlite3_value_double := Source.sqlite3_value_double;
  sqlite3_value_int := Source.sqlite3_value_int;
  sqlite3_value_int64 := Source.sqlite3_value_int64;
  sqlite3_value_text := Source.sqlite3_value_text;
  sqlite3_value_text16 := Source.sqlite3_value_text16;
  sqlite3_value_type := Source.sqlite3_value_type;
  sqlite3_user_data := Source.sqlite3_user_data;
  sqlite3_libversion := Source.sqlite3_libversion;
  sqlite3_libversion_number := Source.sqlite3_libversion_number;
  sqlite3_create_collation := Source.sqlite3_create_collation;
  sqlite3_create_collation16 := Source.sqlite3_create_collation16;
  sqlite3_create_function := Source.sqlite3_create_function;
  sqlite3_create_function16 := Source.sqlite3_create_function16;
  sqlite3_overload_function := Source.sqlite3_overload_function;
  sqlite3_create_module := Source.sqlite3_create_module;
  sqlite3_create_module_v2 := Source.sqlite3_create_module_v2;
  sqlite3_declare_vtab := Source.sqlite3_declare_vtab;
  sqlite3_vtab_config := Source.sqlite3_vtab_config;
  sqlite3_enable_shared_cache := Source.sqlite3_enable_shared_cache;
  sqlite3_enable_load_extension := Source.sqlite3_enable_load_extension;
  sqlite3_busy_timeout:= Source.sqlite3_busy_timeout;
  sqlite3_busy_handler:= Source.sqlite3_busy_handler;
{$IFDEF CODEC}
  sqlite3_key := Source.sqlite3_key;
  sqlite3_key_v2 := Source.sqlite3_key_v2;
  sqlite3_rekey := Source.sqlite3_rekey;
  sqlite3_rekey_v2 := Source.sqlite3_rekey_v2;
{$ENDIF}
  sqlite3_backup_init := Source.sqlite3_backup_init;
  sqlite3_backup_step := Source.sqlite3_backup_step;
  sqlite3_backup_finish := Source.sqlite3_backup_finish;
  sqlite3_backup_remaining := Source.sqlite3_backup_remaining;
  sqlite3_backup_pagecount := Source.sqlite3_backup_pagecount;
  sqlite3_db_release_memory := Source.sqlite3_db_release_memory;
  sqlite3_db_readonly := Source.sqlite3_db_readonly;
  sqlite3_limit := Source.sqlite3_limit;
  sqlite3_uri_parameter := Source.sqlite3_uri_parameter;
  sqlite3_interrupt := Source.sqlite3_interrupt;
end;

constructor TCustomSQLite3API.Create;
begin
  inherited;

{$IFDEF UNIX}
  hLiteLib := nil;
{$ELSE}
  hLiteLib := 0;
{$ENDIF}

  sqlite3_malloc := @NotLink;
  sqlite3_open := @NotLink;
  sqlite3_open16 := @NotLink;
  sqlite3_open_v2 := @NotLink;
  sqlite3_close := @NotLink;
  sqlite3_errcode := @NotLink;
  sqlite3_extended_errcode := @NotLink;
  sqlite3_extended_result_codes := @NotLink;
  sqlite3_errmsg := @NotLink;
  sqlite3_last_insert_rowid := @NotLink;
  sqlite3_changes := @NotLink;
  sqlite3_prepare_v2 := @NotLink;
  sqlite3_step := @NotLink;
  sqlite3_reset := @NotLink;
  sqlite3_finalize := @NotLink;
  sqlite3_column_count := @NotLink;
  sqlite3_column_type := @NotLink;
  sqlite3_column_name := @NotLink;
  sqlite3_column_origin_name := @NotLink;
  sqlite3_column_table_name := @NotLink;
  sqlite3_column_database_name := @NotLink;
  sqlite3_column_decltype := @NotLink;
  sqlite3_table_column_metadata := @NotLink;
  sqlite3_column_blob := @NotLink;
  sqlite3_column_bytes := @NotLink;
  sqlite3_column_double := @NotLink;
  sqlite3_column_int := @NotLink;
  sqlite3_column_int64 := @NotLink;
  sqlite3_column_text := @NotLink;
  sqlite3_bind_parameter_count := @NotLink;
  sqlite3_bind_parameter_name := @NotLink;
  sqlite3_bind_blob := @NotLink;
  sqlite3_bind_zeroblob := @NotLink;
  sqlite3_bind_double := @NotLink;
  sqlite3_bind_int := @NotLink;
  sqlite3_bind_int64 := @NotLink;
  sqlite3_bind_null := @NotLink;
  sqlite3_bind_text := @NotLink;
  sqlite3_result_blob := @NotLink;
  sqlite3_result_double := @NotLink;
  sqlite3_result_error := @NotLink;
  sqlite3_result_error16 := @NotLink;
  sqlite3_result_error_toobig := @NotLink;
  sqlite3_result_error_nomem := @NotLink;
  sqlite3_result_error_code := @NotLink;
  sqlite3_result_int := @NotLink;
  sqlite3_result_int64 := @NotLink;
  sqlite3_result_null := @NotLink;
  sqlite3_result_text := @NotLink;
  sqlite3_result_text16 := @NotLink;
  sqlite3_result_text16le := @NotLink;
  sqlite3_result_text16be := @NotLink;
  sqlite3_result_value := @NotLink;
  sqlite3_result_zeroblob := @NotLink;
  sqlite3_value_blob := @NotLink;
  sqlite3_value_bytes := @NotLink;
  sqlite3_value_bytes16 := @NotLink;
  sqlite3_value_double := @NotLink;
  sqlite3_value_int := @NotLink;
  sqlite3_value_int64 := @NotLink;
  sqlite3_value_text := @NotLink;
  sqlite3_value_text16 := @NotLink;
  sqlite3_value_type := @NotLink;
  sqlite3_user_data := @NotLink;
  sqlite3_libversion := @NotLink;
  sqlite3_libversion_number := @NotLink;
  sqlite3_create_collation := @NotLink;
  sqlite3_create_collation16 := @NotLink;
  sqlite3_create_function := @NotLink;
  sqlite3_create_function16 := @NotLink;
  sqlite3_overload_function := @NotLink;
  sqlite3_create_module := @NotLink;
  sqlite3_create_module_v2 := @NotLink;
  sqlite3_declare_vtab := @NotLink;
  sqlite3_vtab_config := @NotLink;
  sqlite3_enable_shared_cache := @NotLink;
  sqlite3_enable_load_extension := @NotLink;
  sqlite3_busy_timeout := @NotLink;
  sqlite3_busy_handler := @NotLink;
  sqlite3_key := @NotLinkEncryption;
  sqlite3_key_v2 := @NotLinkEncryption;
  sqlite3_rekey := @NotLinkEncryption;
  sqlite3_rekey_v2 := @NotLinkEncryption;
  sqlite3_backup_init := @NotLink;
  sqlite3_backup_step := @NotLink;
  sqlite3_backup_finish := @NotLink;
  sqlite3_backup_remaining := @NotLink;
  sqlite3_backup_pagecount := @NotLink;
  sqlite3_db_release_memory := @NotLink;
  sqlite3_db_readonly := @NotLink;
  sqlite3_limit := @NotLink;
  sqlite3_uri_parameter := @NotLink;
  sqlite3_interrupt := @NotLink;

  FInitialized := False;
  FDirect := False;
end;

destructor TCustomSQLite3API.Destroy;
begin
  if FInitialized then
    UnInitialize;
  inherited;
end;

procedure TCustomSQLite3API.GetLiteErrorCode(SQLite: pSQLite3; var ErrorCode: integer);
begin
  if (SQLite <> nil) and (@sqlite3_extended_errcode <> @NotLink) then
    ErrorCode := sqlite3_extended_errcode(SQLite)
  else
  // for CheckExtended
    ErrorCode := 0;
end;

procedure TCustomSQLite3API.GetLiteErrorMsg(SQLite: pSQLite3; var ErrorMsg: string);
begin
  ErrorMsg := string(CRFunctions.UTF8Decode(Marshal.PtrToStringAnsi(sqlite3_errmsg(SQLite))));
end;

procedure TCustomSQLite3API.GetPredefinedErrorMsg(ErrorCode: integer;
  var ErrorMsg: string);
begin
  case ErrorCode of
    SQLITE_OK:         ErrorMsg := 'not an error';
    SQLITE_ERROR:      ErrorMsg := 'SQL error or missing database';
    SQLITE_INTERNAL:   ErrorMsg := 'Internal logic error in SQLite';
    SQLITE_PERM:       ErrorMsg := 'Access permission denied';
    SQLITE_ABORT:      ErrorMsg := 'Callback routine requested an abort';
    SQLITE_BUSY:       ErrorMsg := 'The database file is locked';
    SQLITE_LOCKED:     ErrorMsg := 'A table in the database is locked';
    SQLITE_NOMEM:      ErrorMsg := 'A malloc() failed';
    SQLITE_READONLY:   ErrorMsg := 'Attempt to write a readonly database';
    SQLITE_INTERRUPT:  ErrorMsg := 'Operation terminated by sqlite3_interrupt()';
    SQLITE_IOERR:      ErrorMsg := 'Some kind of disk I/O error occurred';
    SQLITE_CORRUPT:    ErrorMsg := 'The database disk image is malformed';
    SQLITE_NOTFOUND:   ErrorMsg := 'NOT USED. Table or record not found';
    SQLITE_FULL:       ErrorMsg := 'Insertion failed because database is full';
    SQLITE_CANTOPEN:   ErrorMsg := 'Unable to open the database file';
    SQLITE_PROTOCOL:   ErrorMsg := 'NOT USED. Database lock protocol error';
    SQLITE_EMPTY:      ErrorMsg := 'Database is empty ';
    SQLITE_SCHEMA:     ErrorMsg := 'The database schema changed';
    SQLITE_TOOBIG:     ErrorMsg := 'String or BLOB exceeds size limit';
    SQLITE_CONSTRAINT: ErrorMsg := 'Abort due to constraint violation';
    SQLITE_MISMATCH:   ErrorMsg := 'Data type mismatch';
    SQLITE_MISUSE:     ErrorMsg := 'Library used incorrectly';
    SQLITE_NOLFS:      ErrorMsg := 'Uses OS features not supported on host';
    SQLITE_AUTH:       ErrorMsg := 'Authorization denied';
    SQLITE_FORMAT:     ErrorMsg := 'Auxiliary database format error';
    SQLITE_RANGE:      ErrorMsg := '2nd parameter to sqlite3_bind out of range';
    SQLITE_NOTADB:     ErrorMsg := 'File opened that is not a database file';
    SQLITE_ROW:        ErrorMsg := 'sqlite3_step() has another row ready';
    SQLITE_DONE:       ErrorMsg := 'sqlite3_step() has finished executing';

    SQLITE_IOERR_READ              :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_READ';
    SQLITE_IOERR_SHORT_READ        :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_SHORT_READ';
    SQLITE_IOERR_WRITE             :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_WRITE';
    SQLITE_IOERR_FSYNC             :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_FSYNC';
    SQLITE_IOERR_DIR_FSYNC         :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_DIR_FSYNC';
    SQLITE_IOERR_TRUNCATE          :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_TRUNCATE';
    SQLITE_IOERR_FSTAT             :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_FSTAT';
    SQLITE_IOERR_UNLOCK            :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_UNLOCK';
    SQLITE_IOERR_RDLOCK            :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_RDLOCK';
    SQLITE_IOERR_DELETE            :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_DELETE';
    SQLITE_IOERR_BLOCKED           :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_BLOCKED';
    SQLITE_IOERR_NOMEM             :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_NOMEM';
    SQLITE_IOERR_ACCESS            :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_ACCESS';
    SQLITE_IOERR_CHECKRESERVEDLOCK :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_CHECKRESERVEDLOCK';
    SQLITE_IOERR_LOCK              :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_LOCK';
    SQLITE_IOERR_CLOSE             :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_CLOSE';
    SQLITE_IOERR_DIR_CLOSE         :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_DIR_CLOSE';
    SQLITE_IOERR_SHMOPEN           :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_SHMOPEN';
    SQLITE_IOERR_SHMSIZE           :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_SHMSIZE';
    SQLITE_IOERR_SHMLOCK           :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_SHMLOCK';
    SQLITE_IOERR_SHMMAP            :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_SHMMAP';
    SQLITE_IOERR_SEEK              :ErrorMsg := 'SQLite extended error: SQLITE_IOERR_SEEK';
    SQLITE_LOCKED_SHAREDCACHE      :ErrorMsg := 'SQLite extended error: SQLITE_LOCKED_SHAREDCACHE';
    SQLITE_BUSY_RECOVERY           :ErrorMsg := 'SQLite extended error: SQLITE_BUSY_RECOVERY';
    SQLITE_CANTOPEN_NOTEMPDIR      :ErrorMsg := 'SQLite extended error: SQLITE_CANTOPEN_NOTEMPDIR';
    SQLITE_CANTOPEN_ISDIR          :ErrorMsg := 'SQLite extended error: SQLITE_CANTOPEN_ISDIR';
    SQLITE_CORRUPT_VTAB            :ErrorMsg := 'SQLite extended error: SQLITE_CORRUPT_VTAB';
    SQLITE_READONLY_RECOVERY       :ErrorMsg := 'SQLite extended error: SQLITE_READONLY_RECOVERY';
    SQLITE_READONLY_CANTLOCK       :ErrorMsg := 'SQLite extended error: SQLITE_READONLY_CANTLOCK';
    SQLITE_ABORT_ROLLBACK          :ErrorMsg := 'SQLite extended error: SQLITE_ABORT_ROLLBACK';
  else
    ErrorMsg := 'Unknown error';
  end;
end;

procedure TCustomSQLite3API.Initialize;
begin
  if Assigned(LockInit) then
    LockInit.Enter;

  try
    if FInitialized then
      Exit;

    if FDirect then begin
    {$IFNDEF NOSTATIC}
      if not InitStaticFunction(pointer(Self)) then
        raise Exception.Create(SDirectIsDisabled);
    {$ENDIF}
    end
    else begin
      if FClientLibrary = '' then
        FClientLibrary := SQLiteDLLName;

      LoadClientLibrary;

      if NativeUInt(hLiteLib) = 0 then
        raise Exception.Create('Cannot load client library: ' + FClientLibrary);

      InitFunctions(Self);

    end;

    FInitialized := True;

  finally
    if Assigned(LockInit) then
      LockInit.Leave;
  end;
end;

function TCustomSQLite3API.IsMetaDataAPIAvailable: boolean;
begin
  Result := (@sqlite3_column_origin_name <> @NotLink) and
            (@sqlite3_column_table_name <> @NotLink) and
            (@sqlite3_column_database_name <> @NotLink);
end;

procedure TCustomSQLite3API.UnInitialize;
begin
  if Assigned(LockInit) then
    LockInit.Enter;
  try
    if not FInitialized then
      Exit;

    FInitialized := False;

    if not FDirect then begin
{$IFDEF UNIX}
      if hLiteLib <> nil then
{$ELSE}
      if hLiteLib <> 0 then
{$ENDIF}
      UnloadClientLibrary;

    end;

  finally
    if Assigned(LockInit) then
      LockInit.Leave;
  end;
end;

procedure TCustomSQLite3API.LoadClientLibrary;
begin
{$IFDEF MSWINDOWS}
  hLiteLib := LoadLibraryEx(PChar(FClientLibrary), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
{$ELSE}
  hLiteLib := dlopen(PAnsiChar(AnsiString(FClientLibrary)), RTLD_LAZY);
{$ENDIF}
end;

procedure TCustomSQLite3API.UnloadClientLibrary;
begin
{$IFDEF MSWINDOWS}
  FreeLibrary(hLiteLib);
  hLiteLib := 0;
{$ENDIF}
{$IFDEF POSIX}
  dlclose(hLiteLib);
  hLiteLib := 0;
{$ENDIF}
{$IFDEF UNIX}
  dlclose(hLiteLib);
  hLiteLib := nil;
{$ENDIF}

end;

function TCustomSQLite3API.GetProc(const Name: string; NotLinkPtr: IntPtr): IntPtr;
begin
{$IFDEF MSWINDOWS}
  Result := GetProcAddress(hLiteLib, PChar(Name));
{$ELSE}
  Result := dlsym(hLiteLib, PAnsiChar(AnsiString(Name)));
{$ENDIF}
  if Result = nil then
    Result := NotLinkPtr;
end;

function TCustomSQLite3API.GetProc(const Name: string): IntPtr;
begin
  Result := GetProc(Name, @NotLink);
end;

{ TSQLite3API }

procedure TSQLite3API.Assign(Source: TCustomSQLite3API);
begin
  if Source is TSQLite3API then
    SQLite := TSQLite3API(Source).SQLite;

  inherited;
end;

procedure TSQLite3API.GetLiteErrorCode(var ErrorCode: integer);
begin
  if (SQLite <> nil) and (@sqlite3_extended_errcode <> @NotLink) then
    ErrorCode := sqlite3_extended_errcode(SQLite)
  else
  // for CheckExtended
    ErrorCode := 0;
end;

procedure TSQLite3API.GetLiteErrorMsg(var ErrorMsg: string);
begin
  ErrorMsg := string(CRFunctions.UTF8Decode(Marshal.PtrToStringAnsi(sqlite3_errmsg(SQLite))));
end;

initialization
  LockInit := TCriticalSection.Create;

finalization
  FreeAndNil(LockInit);

end.
