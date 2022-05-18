// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteCallVirtual.pas' rev: 35.00 (Windows)

#ifndef LitecallvirtualHPP
#define LitecallvirtualHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <DAConsts.hpp>
#include <LiteConstsVirtual.hpp>

//-- user supplied -----------------------------------------------------------

namespace Litecallvirtual
{
//-- forward type declarations -----------------------------------------------
struct TFuncDef;
struct TFuncDef3250;
struct TSQLite3Db;
struct TSQLite3;
struct TSQLite3Context;
struct TSQLite3Context387;
struct stm;
struct sqlite3_codec;
class DELPHICLASS TCustomSQLite3API;
class DELPHICLASS TSQLite3API;
//-- type declarations -------------------------------------------------------
typedef System::Byte u8;

typedef System::Int8 i8;

typedef System::Word u16;

typedef unsigned u32;

typedef short i16;

typedef __int64 i64;

typedef TFuncDef *pFuncDef;

struct DECLSPEC_DRECORD TFuncDef
{
public:
	short nArg;
	System::Byte iPrefEnc;
	System::Byte flags;
	void *pUserData;
};


typedef TFuncDef3250 *pFuncDef3250;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TFuncDef3250
{
public:
	System::Int8 nArg;
	System::Byte padding1;
	System::Byte padding2;
	System::Byte padding3;
	unsigned funcFlags;
	void *pUserData;
};
#pragma pack(pop)


typedef TSQLite3Db *pSQLite3Db;

struct DECLSPEC_DRECORD TSQLite3Db
{
public:
	void *zName;
	void *pBt;
	System::Byte safety_level;
	void *pSchema;
};


struct DECLSPEC_DRECORD TSQLite3
{
public:
	void *pVfs;
	void *pVdbe;
	void *pDfltColl;
	void *mutex;
	void *aDb;
	int nDb;
};


struct DECLSPEC_DRECORD TSQLite3Context
{
public:
	TFuncDef *pFunc;
};


struct DECLSPEC_DRECORD TSQLite3Context387
{
public:
	void *pOut;
	TFuncDef *pFunc;
};


typedef void * pSQLite3;

typedef void * pSQLite3Stmt;

typedef void * pSQLite3Context;

typedef void * pSQLite3Value;

typedef void * pSQLite3Backup;

typedef TSQLite3Context *pContext;

typedef TSQLite3Context387 *pContext387;

typedef int __cdecl (*TCallBackLiteCollation)(void * pUserData, int StrSize1, const void * pStr1, int StrSize2, const void * pStr2);

typedef void __cdecl (*TCallBackLiteFunction)(pContext Context, int ParamCount, void * pData);

typedef void __cdecl (*TCallBackLiteFunction387)(pContext387 Context, int ParamCount, void * pData);

typedef stm *ptm;

#pragma pack(push,1)
struct DECLSPEC_DRECORD stm
{
public:
	int tm_sec;
	int tm_min;
	int tm_hour;
	int tm_mday;
	int tm_mon;
	int tm_year;
	int tm_wday;
	int tm_yday;
	int tm_isdst;
};
#pragma pack(pop)


typedef sqlite3_codec *psqlite3_codec;

struct DECLSPEC_DRECORD sqlite3_codec
{
public:
	int __cdecl (*xKeyV2)(void * pSQLite, char * dbName, char * key, int size);
	int __cdecl (*xRekeyV2)(void * pSQLite, char * dbName, char * newkey, int size);
	void __cdecl (*xActivateSee)(char * zPassPhrase);
	int __cdecl (*xCodecAttach)(void * pSQLite, int nDb, char * key, int size, Liteconstsvirtual::TLiteEncryptionAlgorithm Algorithm/* = (Liteconstsvirtual::TLiteEncryptionAlgorithm)(0x7)*/);
	void __cdecl (*xCodecGetKey)(void * pSQLite, int nDb, void * &key, PINT &size);
	int __cdecl (*xCustomPragma)(void * pSQLite, int nDb, char * Left, char * Right);
};


typedef void * __cdecl (*Tsqlite3_malloc)(int n);

typedef int __cdecl (*Tsqlite3_open)(char * filename, /* out */ void * &ppDb);

typedef int __cdecl (*Tsqlite3_open16)(System::WideChar * filename, /* out */ void * &ppDb);

typedef int __cdecl (*Tsqlite3_open_v2)(char * filename, /* out */ void * &ppDb, int flags, char * zVfs);

typedef int __cdecl (*Tsqlite3_close)(void * pDb);

typedef int __cdecl (*Tsqlite3_errcode)(void * pDb);

typedef int __cdecl (*Tsqlite3_extended_errcode)(void * pDb);

typedef int __cdecl (*Tsqlite3_extended_result_codes)(void * pDb, int onoff);

typedef char * __cdecl (*Tsqlite3_errmsg)(void * pDb);

typedef __int64 __cdecl (*Tsqlite3_last_insert_rowid)(void * pDb);

typedef int __cdecl (*Tsqlite3_changes)(void * pDb);

typedef int __cdecl (*Tsqlite3_prepare_v2)(void * db, char * zSql, int nByte, /* out */ void * &ppStmt, /* out */ void * &pzTail);

typedef int __cdecl (*Tsqlite3_step)(void * pStmt);

typedef int __cdecl (*Tsqlite3_reset)(void * pStmt);

typedef int __cdecl (*Tsqlite3_finalize)(void * pStmt);

typedef int __cdecl (*Tsqlite3_column_count)(void * pStmt);

typedef int __cdecl (*Tsqlite3_column_type)(void * pStmt, int iCol);

typedef char * __cdecl (*Tsqlite3_column_name)(void * pStmt, int iCol);

typedef char * __cdecl (*Tsqlite3_column_origin_name)(void * pStmt, int iCol);

typedef char * __cdecl (*Tsqlite3_column_table_name)(void * pStmt, int iCol);

typedef char * __cdecl (*Tsqlite3_column_database_name)(void * pStmt, int iCol);

typedef char * __cdecl (*Tsqlite3_column_decltype)(void * pStmt, int iCol);

typedef int __cdecl (*Tsqlite3_table_column_metadata)(void * pDb, void * zDbName, void * zTableName, void * zColumnName, /* out */ void * &pzDataType, /* out */ void * &pzCollSeq, /* out */ int &pNotNull, /* out */ int &pPrimaryKey, /* out */ int &pAutoinc);

typedef void * __cdecl (*Tsqlite3_column_blob)(void * pStmt, int iCol);

typedef int __cdecl (*Tsqlite3_column_bytes)(void * pStmt, int iCol);

typedef double __cdecl (*Tsqlite3_column_double)(void * pStmt, int iCol);

typedef int __cdecl (*Tsqlite3_column_int)(void * pStmt, int iCol);

typedef __int64 __cdecl (*Tsqlite3_column_int64)(void * pStmt, int iCol);

typedef char * __cdecl (*Tsqlite3_column_text)(void * pStmt, int iCol);

typedef int __cdecl (*Tsqlite3_bind_parameter_count)(void * pStmt);

typedef char * __cdecl (*Tsqlite3_bind_parameter_name)(void * pStmt, int Index);

typedef int __cdecl (*Tsqlite3_bind_blob)(void * pStmt, int Index, void * pBlob, int Size, void * pDestrType);

typedef int __cdecl (*Tsqlite3_bind_zeroblob)(void * pStmt, int Index, int Size);

typedef int __cdecl (*Tsqlite3_bind_double)(void * pStmt, int Index, double Value);

typedef int __cdecl (*Tsqlite3_bind_int)(void * pStmt, int Index, int Value);

typedef int __cdecl (*Tsqlite3_bind_int64)(void * pStmt, int Index, __int64 Value);

typedef int __cdecl (*Tsqlite3_bind_null)(void * pStmt, int Index);

typedef int __cdecl (*Tsqlite3_bind_text)(void * pStmt, int Index, char * Value, int Size, void * pDestrType);

typedef void __cdecl (*Tsqlite3_result_blob)(void * pContext, void * pBlob, int Size, void * pDestrType);

typedef void __cdecl (*Tsqlite3_result_double)(void * pContext, double Value);

typedef void __cdecl (*Tsqlite3_result_error)(void * pContext, void * pMsg, int Size);

typedef void __cdecl (*Tsqlite3_result_error16)(void * pContext, void * pMsg, int Size);

typedef void __cdecl (*Tsqlite3_result_error_toobig)(void * pContext);

typedef void __cdecl (*Tsqlite3_result_error_nomem)(void * pContext);

typedef void __cdecl (*Tsqlite3_result_error_code)(void * pContext, int ErrorCode);

typedef void __cdecl (*Tsqlite3_result_int)(void * pContext, int Value);

typedef void __cdecl (*Tsqlite3_result_int64)(void * pContext, __int64 Value);

typedef void __cdecl (*Tsqlite3_result_null)(void * pContext);

typedef void __cdecl (*Tsqlite3_result_text)(void * pContext, void * pStr, int Size, void * pDestrType);

typedef void __cdecl (*Tsqlite3_result_text16)(void * pContext, void * pStr, int Size, void * pDestrType);

typedef void __cdecl (*Tsqlite3_result_text16le)(void * pContext, void * pStr, int Size, void * pDestrType);

typedef void __cdecl (*Tsqlite3_result_text16be)(void * pContext, void * pStr, int Size, void * pDestrType);

typedef void __cdecl (*Tsqlite3_result_value)(void * pContext, void * Value);

typedef void __cdecl (*Tsqlite3_result_zeroblob)(void * pContext, int Size);

typedef void * __cdecl (*Tsqlite3_value_blob)(void * pValue);

typedef int __cdecl (*Tsqlite3_value_bytes)(void * pValue);

typedef int __cdecl (*Tsqlite3_value_bytes16)(void * pValue);

typedef double __cdecl (*Tsqlite3_value_double)(void * pValue);

typedef int __cdecl (*Tsqlite3_value_int)(void * pValue);

typedef __int64 __cdecl (*Tsqlite3_value_int64)(void * pValue);

typedef void * __cdecl (*Tsqlite3_value_text)(void * pValue);

typedef void * __cdecl (*Tsqlite3_value_text16)(void * pValue);

typedef int __cdecl (*Tsqlite3_value_type)(void * pValue);

typedef void * __cdecl (*Tsqlite3_user_data)(void * pContext);

typedef char * __cdecl (*Tsqlite3_libversion)(void);

typedef int __cdecl (*Tsqlite3_libversion_number)(void);

typedef int __cdecl (*Tsqlite3_create_collation)(void * pSQLite, char * zName, int eTextRep, void * userData, void * func);

typedef int __cdecl (*Tsqlite3_create_collation16)(void * pSQLite, System::WideChar * zName, int eTextRep, void * userData, void * func);

typedef int __cdecl (*Tsqlite3_create_function)(void * pSQLite, char * zFunctionName, int nArg, int eTextRep, void * pApp, void * xFunc, void * xStep, void * xFinal);

typedef int __cdecl (*Tsqlite3_create_function16)(void * pSQLite, System::WideChar * zFunctionName, int nArg, int eTextRep, void * pApp, void * xFunc, void * xStep, void * xFinal);

typedef int __cdecl (*Tsqlite3_create_function_v2)(void * pSQLite, char * zFunctionName, int nArg, int eTextRep, void * pApp, void * xFunc, void * xStep, void * xFinal, void * xDestroy);

typedef int __cdecl (*Tsqlite3_overload_function)(void * pSQLite, char * zFunctionName, int nArg);

typedef int __cdecl (*Tsqlite3_create_module)(void * pSQLite, void * zName, void * p, void * pClientData);

typedef int __cdecl (*Tsqlite3_create_module_v2)(void * pSQLite, void * zName, void * p, void * pClientData, void * xDestroy);

typedef int __cdecl (*Tsqlite3_declare_vtab)(void * pSQLite, const void * zCreateTable);

typedef int __cdecl (*Tsqlite3_vtab_config)(void * pSQLite, int op, int flag);

typedef int __cdecl (*Tsqlite3_enable_shared_cache)(int Value);

typedef int __cdecl (*Tsqlite3_enable_load_extension)(void * pSQLite, int OnOff);

typedef int __cdecl (*Tsqlite3_busy_timeout)(void * pSQLite, int MilliSeconds);

typedef int __cdecl (*Tsqlite3_busy_handler)(void * pSQLite, void * func, void * userData);

typedef int __cdecl (*Tsqlite3_key)(void * pSQLite, char * key, int size);

typedef int __cdecl (*Tsqlite3_key_v2)(void * pSQLite, char * zDbName, char * key, int size);

typedef int __cdecl (*Tsqlite3_rekey)(void * pSQLite, char * newkey, int size);

typedef int __cdecl (*Tsqlite3_rekey_v2)(void * pSQLite, char * zDbName, char * newkey, int size);

typedef void * __cdecl (*Tsqlite3_backup_init)(void * pDest, char * zDestName, void * pSource, char * zSourceName);

typedef int __cdecl (*Tsqlite3_backup_step)(void * p, int nPage);

typedef int __cdecl (*Tsqlite3_backup_finish)(void * p);

typedef int __cdecl (*Tsqlite3_backup_remaining)(void * p);

typedef int __cdecl (*Tsqlite3_backup_pagecount)(void * p);

typedef int __cdecl (*Tsqlite3_db_release_memory)(void * pDb);

typedef int __cdecl (*Tsqlite3_db_readonly)(void * pDb, char * zDbName);

typedef int __cdecl (*Tsqlite3_limit)(void * pDb, int id, int newVal);

typedef char * __cdecl (*Tsqlite3_uri_parameter)(char * zFilename, char * zParam);

typedef void __cdecl (*Tsqlite3_interrupt)(void * pDb);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomSQLite3API : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	NativeUInt hLiteLib;
	System::UnicodeString FClientLibrary;
	bool FDirect;
	bool FInitialized;
	void __fastcall LoadClientLibrary();
	void __fastcall UnLoadClientLibrary();
	
public:
	Tsqlite3_malloc sqlite3_malloc;
	Tsqlite3_open sqlite3_open;
	Tsqlite3_open16 sqlite3_open16;
	Tsqlite3_open_v2 sqlite3_open_v2;
	Tsqlite3_close sqlite3_close;
	Tsqlite3_errcode sqlite3_errcode;
	Tsqlite3_extended_errcode sqlite3_extended_errcode;
	Tsqlite3_extended_result_codes sqlite3_extended_result_codes;
	Tsqlite3_errmsg sqlite3_errmsg;
	Tsqlite3_last_insert_rowid sqlite3_last_insert_rowid;
	Tsqlite3_changes sqlite3_changes;
	Tsqlite3_prepare_v2 sqlite3_prepare_v2;
	Tsqlite3_step sqlite3_step;
	Tsqlite3_reset sqlite3_reset;
	Tsqlite3_finalize sqlite3_finalize;
	Tsqlite3_column_count sqlite3_column_count;
	Tsqlite3_column_type sqlite3_column_type;
	Tsqlite3_column_name sqlite3_column_name;
	Tsqlite3_column_origin_name sqlite3_column_origin_name;
	Tsqlite3_column_table_name sqlite3_column_table_name;
	Tsqlite3_column_database_name sqlite3_column_database_name;
	Tsqlite3_column_decltype sqlite3_column_decltype;
	Tsqlite3_table_column_metadata sqlite3_table_column_metadata;
	Tsqlite3_column_blob sqlite3_column_blob;
	Tsqlite3_column_bytes sqlite3_column_bytes;
	Tsqlite3_column_double sqlite3_column_double;
	Tsqlite3_column_int sqlite3_column_int;
	Tsqlite3_column_int64 sqlite3_column_int64;
	Tsqlite3_column_text sqlite3_column_text;
	Tsqlite3_bind_parameter_count sqlite3_bind_parameter_count;
	Tsqlite3_bind_parameter_name sqlite3_bind_parameter_name;
	Tsqlite3_bind_blob sqlite3_bind_blob;
	Tsqlite3_bind_zeroblob sqlite3_bind_zeroblob;
	Tsqlite3_bind_double sqlite3_bind_double;
	Tsqlite3_bind_int sqlite3_bind_int;
	Tsqlite3_bind_int64 sqlite3_bind_int64;
	Tsqlite3_bind_null sqlite3_bind_null;
	Tsqlite3_bind_text sqlite3_bind_text;
	Tsqlite3_result_blob sqlite3_result_blob;
	Tsqlite3_result_double sqlite3_result_double;
	Tsqlite3_result_error sqlite3_result_error;
	Tsqlite3_result_error16 sqlite3_result_error16;
	Tsqlite3_result_error_toobig sqlite3_result_error_toobig;
	Tsqlite3_result_error_nomem sqlite3_result_error_nomem;
	Tsqlite3_result_error_code sqlite3_result_error_code;
	Tsqlite3_result_int sqlite3_result_int;
	Tsqlite3_result_int64 sqlite3_result_int64;
	Tsqlite3_result_null sqlite3_result_null;
	Tsqlite3_result_text sqlite3_result_text;
	Tsqlite3_result_text16 sqlite3_result_text16;
	Tsqlite3_result_text16le sqlite3_result_text16le;
	Tsqlite3_result_text16be sqlite3_result_text16be;
	Tsqlite3_result_value sqlite3_result_value;
	Tsqlite3_result_zeroblob sqlite3_result_zeroblob;
	Tsqlite3_value_blob sqlite3_value_blob;
	Tsqlite3_value_bytes sqlite3_value_bytes;
	Tsqlite3_value_bytes16 sqlite3_value_bytes16;
	Tsqlite3_value_double sqlite3_value_double;
	Tsqlite3_value_int sqlite3_value_int;
	Tsqlite3_value_int64 sqlite3_value_int64;
	Tsqlite3_value_text sqlite3_value_text;
	Tsqlite3_value_text16 sqlite3_value_text16;
	Tsqlite3_value_type sqlite3_value_type;
	Tsqlite3_user_data sqlite3_user_data;
	Tsqlite3_libversion sqlite3_libversion;
	Tsqlite3_libversion_number sqlite3_libversion_number;
	Tsqlite3_create_collation sqlite3_create_collation;
	Tsqlite3_create_collation16 sqlite3_create_collation16;
	Tsqlite3_create_function sqlite3_create_function;
	Tsqlite3_create_function16 sqlite3_create_function16;
	Tsqlite3_overload_function sqlite3_overload_function;
	Tsqlite3_create_module sqlite3_create_module;
	Tsqlite3_create_module_v2 sqlite3_create_module_v2;
	Tsqlite3_declare_vtab sqlite3_declare_vtab;
	Tsqlite3_vtab_config sqlite3_vtab_config;
	Tsqlite3_enable_shared_cache sqlite3_enable_shared_cache;
	Tsqlite3_enable_load_extension sqlite3_enable_load_extension;
	Tsqlite3_busy_timeout sqlite3_busy_timeout;
	Tsqlite3_busy_handler sqlite3_busy_handler;
	Tsqlite3_key sqlite3_key;
	Tsqlite3_key_v2 sqlite3_key_v2;
	Tsqlite3_rekey sqlite3_rekey;
	Tsqlite3_rekey_v2 sqlite3_rekey_v2;
	Tsqlite3_backup_init sqlite3_backup_init;
	Tsqlite3_backup_step sqlite3_backup_step;
	Tsqlite3_backup_finish sqlite3_backup_finish;
	Tsqlite3_backup_remaining sqlite3_backup_remaining;
	Tsqlite3_backup_pagecount sqlite3_backup_pagecount;
	Tsqlite3_db_release_memory sqlite3_db_release_memory;
	Tsqlite3_db_readonly sqlite3_db_readonly;
	Tsqlite3_limit sqlite3_limit;
	Tsqlite3_uri_parameter sqlite3_uri_parameter;
	Tsqlite3_interrupt sqlite3_interrupt;
	__fastcall TCustomSQLite3API();
	__fastcall virtual ~TCustomSQLite3API();
	void __fastcall Initialize();
	void __fastcall UnInitialize();
	void __fastcall GetLiteErrorCode(void * SQLite, int &ErrorCode);
	void __fastcall GetLiteErrorMsg(void * SQLite, System::UnicodeString &ErrorMsg);
	void __fastcall GetPredefinedErrorMsg(int ErrorCode, System::UnicodeString &ErrorMsg);
	bool __fastcall IsMetaDataAPIAvailable();
	void * __fastcall GetProc(const System::UnicodeString Name, void * NotLinkPtr)/* overload */;
	void * __fastcall GetProc(const System::UnicodeString Name)/* overload */;
	virtual void __fastcall Assign(TCustomSQLite3API* Source);
	__property System::UnicodeString ClientLibrary = {read=FClientLibrary, write=FClientLibrary};
	__property bool Initialized = {read=FInitialized, nodefault};
	__property bool Direct = {read=FDirect, write=FDirect, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLite3API : public TCustomSQLite3API
{
	typedef TCustomSQLite3API inherited;
	
public:
	void *SQLite;
	virtual void __fastcall Assign(TCustomSQLite3API* Source);
	HIDESBASE void __fastcall GetLiteErrorCode(int &ErrorCode);
	HIDESBASE void __fastcall GetLiteErrorMsg(System::UnicodeString &ErrorMsg);
public:
	/* TCustomSQLite3API.Create */ inline __fastcall TSQLite3API() : TCustomSQLite3API() { }
	/* TCustomSQLite3API.Destroy */ inline __fastcall virtual ~TSQLite3API() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define DACProductName L"VirtualDAC"
#define SQLiteDLLName L"sqlite3.dll"
static const System::Int8 SQLITE_OK = System::Int8(0x0);
static const System::Int8 SQLITE_ERROR = System::Int8(0x1);
static const System::Int8 SQLITE_INTERNAL = System::Int8(0x2);
static const System::Int8 SQLITE_PERM = System::Int8(0x3);
static const System::Int8 SQLITE_ABORT = System::Int8(0x4);
static const System::Int8 SQLITE_BUSY = System::Int8(0x5);
static const System::Int8 SQLITE_LOCKED = System::Int8(0x6);
static const System::Int8 SQLITE_NOMEM = System::Int8(0x7);
static const System::Int8 SQLITE_READONLY = System::Int8(0x8);
static const System::Int8 SQLITE_INTERRUPT = System::Int8(0x9);
static const System::Int8 SQLITE_IOERR = System::Int8(0xa);
static const System::Int8 SQLITE_CORRUPT = System::Int8(0xb);
static const System::Int8 SQLITE_NOTFOUND = System::Int8(0xc);
static const System::Int8 SQLITE_FULL = System::Int8(0xd);
static const System::Int8 SQLITE_CANTOPEN = System::Int8(0xe);
static const System::Int8 SQLITE_PROTOCOL = System::Int8(0xf);
static const System::Int8 SQLITE_EMPTY = System::Int8(0x10);
static const System::Int8 SQLITE_SCHEMA = System::Int8(0x11);
static const System::Int8 SQLITE_TOOBIG = System::Int8(0x12);
static const System::Int8 SQLITE_CONSTRAINT = System::Int8(0x13);
static const System::Int8 SQLITE_MISMATCH = System::Int8(0x14);
static const System::Int8 SQLITE_MISUSE = System::Int8(0x15);
static const System::Int8 SQLITE_NOLFS = System::Int8(0x16);
static const System::Int8 SQLITE_AUTH = System::Int8(0x17);
static const System::Int8 SQLITE_FORMAT = System::Int8(0x18);
static const System::Int8 SQLITE_RANGE = System::Int8(0x19);
static const System::Int8 SQLITE_NOTADB = System::Int8(0x1a);
static const System::Int8 SQLITE_ROW = System::Int8(0x64);
static const System::Int8 SQLITE_DONE = System::Int8(0x65);
static const System::Word SQLITE_BUSY_RECOVERY = System::Word(0x105);
static const System::Word SQLITE_LOCKED_SHAREDCACHE = System::Word(0x106);
static const System::Word SQLITE_READONLY_RECOVERY = System::Word(0x108);
static const System::Word SQLITE_IOERR_READ = System::Word(0x10a);
static const System::Word SQLITE_CORRUPT_VTAB = System::Word(0x10b);
static const System::Word SQLITE_CANTOPEN_NOTEMPDIR = System::Word(0x10e);
static const System::Word SQLITE_ABORT_ROLLBACK = System::Word(0x204);
static const System::Word SQLITE_BUSY_SNAPSHOT = System::Word(0x205);
static const System::Word SQLITE_READONLY_CANTLOCK = System::Word(0x208);
static const System::Word SQLITE_CANTOPEN_ISDIR = System::Word(0x20e);
static const System::Word SQLITE_IOERR_SHORT_READ = System::Word(0x20a);
static const System::Word SQLITE_IOERR_WRITE = System::Word(0x30a);
static const System::Word SQLITE_IOERR_FSYNC = System::Word(0x40a);
static const System::Word SQLITE_IOERR_DIR_FSYNC = System::Word(0x50a);
static const System::Word SQLITE_IOERR_TRUNCATE = System::Word(0x60a);
static const System::Word SQLITE_IOERR_FSTAT = System::Word(0x70a);
static const System::Word SQLITE_IOERR_UNLOCK = System::Word(0x80a);
static const System::Word SQLITE_IOERR_RDLOCK = System::Word(0x90a);
static const System::Word SQLITE_IOERR_DELETE = System::Word(0xa0a);
static const System::Word SQLITE_IOERR_BLOCKED = System::Word(0xb0a);
static const System::Word SQLITE_IOERR_NOMEM = System::Word(0xc0a);
static const System::Word SQLITE_IOERR_ACCESS = System::Word(0xd0a);
static const System::Word SQLITE_IOERR_CHECKRESERVEDLOCK = System::Word(0xe0a);
static const System::Word SQLITE_IOERR_LOCK = System::Word(0xf0a);
static const System::Word SQLITE_IOERR_CLOSE = System::Word(0x100a);
static const System::Word SQLITE_IOERR_DIR_CLOSE = System::Word(0x110a);
static const System::Word SQLITE_IOERR_SHMOPEN = System::Word(0x120a);
static const System::Word SQLITE_IOERR_SHMSIZE = System::Word(0x130a);
static const System::Word SQLITE_IOERR_SHMLOCK = System::Word(0x140a);
static const System::Word SQLITE_IOERR_SHMMAP = System::Word(0x150a);
static const System::Word SQLITE_IOERR_SEEK = System::Word(0x160a);
static const System::Word SQLITE_IOERR_DELETE_NOENT = System::Word(0x170a);
static const System::Int8 SQLITE_INTEGER = System::Int8(0x1);
static const System::Int8 SQLITE_FLOAT = System::Int8(0x2);
static const System::Int8 SQLITE_TEXT = System::Int8(0x3);
static const System::Int8 SQLITE_BLOB = System::Int8(0x4);
static const System::Int8 SQLITE_NULL = System::Int8(0x5);
extern DELPHI_PACKAGE void *SQLITE_STATIC;
extern DELPHI_PACKAGE void *SQLITE_TRANSIENT;
static const System::Int8 SQLITE_UTF8 = System::Int8(0x1);
static const System::Int8 SQLITE_UTF16LE = System::Int8(0x2);
static const System::Int8 SQLITE_UTF16BE = System::Int8(0x3);
static const System::Int8 SQLITE_UTF16 = System::Int8(0x4);
static const System::Int8 SQLITE_ANY = System::Int8(0x5);
static const System::Int8 SQLITE_OPEN_READONLY = System::Int8(0x1);
static const System::Int8 SQLITE_OPEN_READWRITE = System::Int8(0x2);
static const System::Int8 SQLITE_OPEN_CREATE = System::Int8(0x4);
static const System::Int8 SQLITE_OPEN_DELETEONCLOSE = System::Int8(0x8);
static const System::Int8 SQLITE_OPEN_EXCLUSIVE = System::Int8(0x10);
static const System::Int8 SQLITE_OPEN_AUTOPROXY = System::Int8(0x20);
static const System::Int8 SQLITE_OPEN_URI = System::Int8(0x40);
static const System::Byte SQLITE_OPEN_MEMORY = System::Byte(0x80);
static const System::Word SQLITE_OPEN_MAIN_DB = System::Word(0x100);
static const System::Word SQLITE_OPEN_TEMP_DB = System::Word(0x200);
static const System::Word SQLITE_OPEN_TRANSIENT_DB = System::Word(0x400);
static const System::Word SQLITE_OPEN_MAIN_JOURNAL = System::Word(0x800);
static const System::Word SQLITE_OPEN_TEMP_JOURNAL = System::Word(0x1000);
static const System::Word SQLITE_OPEN_SUBJOURNAL = System::Word(0x2000);
static const System::Word SQLITE_OPEN_MASTER_JOURNAL = System::Word(0x4000);
static const System::Word SQLITE_OPEN_NOMUTEX = System::Word(0x8000);
static const int SQLITE_OPEN_FULLMUTEX = int(0x10000);
static const int SQLITE_OPEN_SHAREDCACHE = int(0x20000);
static const int SQLITE_OPEN_PRIVATECACHE = int(0x40000);
static const int SQLITE_OPEN_WAL = int(0x80000);
static const System::Int8 SQLITE_IOCAP_ATOMIC = System::Int8(0x1);
static const System::Int8 SQLITE_IOCAP_ATOMIC512 = System::Int8(0x2);
static const System::Int8 SQLITE_IOCAP_ATOMIC1K = System::Int8(0x4);
static const System::Int8 SQLITE_IOCAP_ATOMIC2K = System::Int8(0x8);
static const System::Int8 SQLITE_IOCAP_ATOMIC4K = System::Int8(0x10);
static const System::Int8 SQLITE_IOCAP_ATOMIC8K = System::Int8(0x20);
static const System::Int8 SQLITE_IOCAP_ATOMIC16K = System::Int8(0x40);
static const System::Byte SQLITE_IOCAP_ATOMIC32K = System::Byte(0x80);
static const System::Word SQLITE_IOCAP_ATOMIC64K = System::Word(0x100);
static const System::Word SQLITE_IOCAP_SAFE_APPEND = System::Word(0x200);
static const System::Word SQLITE_IOCAP_SEQUENTIAL = System::Word(0x400);
static const System::Word SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN = System::Word(0x800);
static const System::Word SQLITE_IOCAP_POWERSAFE_OVERWRITE = System::Word(0x1000);
static const System::Word SQLITE_IOCAP_IMMUTABLE = System::Word(0x2000);
static const System::Int8 SQLITE_FCNTL_LOCKSTATE = System::Int8(0x1);
static const System::Int8 SQLITE_GET_LOCKPROXYFILE = System::Int8(0x2);
static const System::Int8 SQLITE_SET_LOCKPROXYFILE = System::Int8(0x3);
static const System::Int8 SQLITE_LAST_ERRNO = System::Int8(0x4);
static const System::Int8 SQLITE_FCNTL_SIZE_HINT = System::Int8(0x5);
static const System::Int8 SQLITE_FCNTL_CHUNK_SIZE = System::Int8(0x6);
static const System::Int8 SQLITE_FCNTL_FILE_POINTER = System::Int8(0x7);
static const System::Int8 SQLITE_FCNTL_SYNC_OMITTED = System::Int8(0x8);
static const System::Int8 SQLITE_FCNTL_WIN32_AV_RETRY = System::Int8(0x9);
static const System::Int8 SQLITE_FCNTL_PERSIST_WAL = System::Int8(0xa);
static const System::Int8 SQLITE_FCNTL_OVERWRITE = System::Int8(0xb);
static const System::Int8 SQLITE_FCNTL_VFSNAME = System::Int8(0xc);
static const System::Int8 SQLITE_FCNTL_POWERSAFE_OVERWRITE = System::Int8(0xd);
static const System::Int8 SQLITE_FCNTL_PRAGMA = System::Int8(0xe);
static const System::Int8 SQLITE_FCNTL_BUSYHANDLER = System::Int8(0xf);
static const System::Int8 SQLITE_FCNTL_TEMPFILENAME = System::Int8(0x10);
static const System::Int8 SQLITE_FCNTL_MMAP_SIZE = System::Int8(0x12);
static const System::Int8 SQLITE_FCNTL_TRACE = System::Int8(0x13);
static const System::Int8 SQLITE_FCNTL_HAS_MOVED = System::Int8(0x14);
static const System::Int8 SQLITE_FCNTL_SYNC = System::Int8(0x15);
static const System::Int8 SQLITE_FCNTL_COMMIT_PHASETWO = System::Int8(0x16);
static const System::Int8 SQLITE_FCNTL_WIN32_SET_HANDLE = System::Int8(0x17);
static const System::Int8 UNIXFILE_EXCL = System::Int8(0x1);
static const System::Int8 UNIXFILE_RDONLY = System::Int8(0x2);
static const System::Int8 UNIXFILE_PERSIST_WAL = System::Int8(0x4);
static const System::Int8 UNIXFILE_DIRSYNC = System::Int8(0x8);
static const System::Int8 UNIXFILE_PSOW = System::Int8(0x10);
static const System::Int8 UNIXFILE_DELETE = System::Int8(0x20);
static const System::Int8 UNIXFILE_URI = System::Int8(0x40);
static const System::Byte UNIXFILE_NOLOCK = System::Byte(0x80);
static const System::Word UNIXFILE_WARNED = System::Word(0x100);
static const System::Int8 SQLITE_ACCESS_EXISTS = System::Int8(0x0);
static const System::Int8 SQLITE_ACCESS_READWRITE = System::Int8(0x1);
static const System::Int8 SQLITE_ACCESS_READ = System::Int8(0x2);
static const System::Int8 NO_LOCK = System::Int8(0x0);
static const System::Int8 SHARED_LOCK = System::Int8(0x1);
static const System::Int8 RESERVED_LOCK = System::Int8(0x2);
static const System::Int8 PENDING_LOCK = System::Int8(0x3);
static const System::Int8 EXCLUSIVE_LOCK = System::Int8(0x4);
static const int PENDING_BYTE = int(0x40000000);
static const int RESERVED_BYTE = int(0x40000001);
static const System::Word SQLITE_DEFAULT_SECTOR_SIZE = System::Word(0x1000);
static const System::Word SQLITE_ANDROID_SECTOR_SIZE = System::Word(0x1000);
static const System::Int8 SQLITE_SHM_UNLOCK = System::Int8(0x1);
static const System::Int8 SQLITE_SHM_LOCK = System::Int8(0x2);
static const System::Int8 SQLITE_SHM_SHARED = System::Int8(0x4);
static const System::Int8 SQLITE_SHM_EXCLUSIVE = System::Int8(0x8);
static const System::Int8 SQLITE_LIMIT_LENGTH = System::Int8(0x0);
static const System::Int8 SQLITE_LIMIT_SQL_LENGTH = System::Int8(0x1);
static const System::Int8 SQLITE_LIMIT_COLUMN = System::Int8(0x2);
static const System::Int8 SQLITE_LIMIT_EXPR_DEPTH = System::Int8(0x3);
static const System::Int8 SQLITE_LIMIT_COMPOUND_SELECT = System::Int8(0x4);
static const System::Int8 SQLITE_LIMIT_VDBE_OP = System::Int8(0x5);
static const System::Int8 SQLITE_LIMIT_FUNCTION_ARG = System::Int8(0x6);
static const System::Int8 SQLITE_LIMIT_ATTACHED = System::Int8(0x7);
static const System::Int8 SQLITE_LIMIT_LIKE_PATTERN_LENGTH = System::Int8(0x8);
static const System::Int8 SQLITE_LIMIT_VARIABLE_NUMBER = System::Int8(0x9);
static const System::Int8 SQLITE_LIMIT_TRIGGER_DEPTH = System::Int8(0xa);
static const System::Int8 SQLITE_LIMIT_WORKER_THREADS = System::Int8(0xb);
static const System::Word DEF_LIMIT_EXPR_DEPTH = System::Word(0x3e8);
static const System::Int8 SQLITE_VTAB_CONSTRAINT_SUPPORT = System::Int8(0x1);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_EQ = System::Int8(0x2);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_GT = System::Int8(0x4);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_LE = System::Int8(0x8);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_LT = System::Int8(0x10);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_GE = System::Int8(0x20);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_MATCH = System::Int8(0x40);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_LIKE = System::Int8(0x41);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_GLOB = System::Int8(0x42);
static const System::Int8 SQLITE_INDEX_CONSTRAINT_REGEXP = System::Int8(0x43);
static const System::Int8 SQLITE_INDEX_SCAN_UNIQUE = System::Int8(0x1);
static const System::Word CP_SJIS = System::Word(0x3a4);
static const System::Word CP_GBK = System::Word(0x3a8);
static const System::Word CP_UHC = System::Word(0x3b5);
static const System::Word CP_BIG5 = System::Word(0x3b6);
static const System::Word CP_JOHAB = System::Word(0x551);
static const System::Word CP_EUC_JIS_2004 = System::Word(0x51c4);
static const System::Word CP_EUC_JP = System::Word(0xcadc);
static const System::Word CP_EUC_CN = System::Word(0xcae0);
static const System::Word CP_EUC_KR = System::Word(0xcaed);
static const System::Word CP_EUC_TW = System::Word(0xcaee);
static const System::Word CP_GB18030 = System::Word(0xd698);
static const System::Word CP_UTF_8 = System::Word(0xfde9);
extern DELPHI_PACKAGE System::Syncobjs::TCriticalSection* LockInit;
}	/* namespace Litecallvirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITECALLVIRTUAL)
using namespace Litecallvirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LitecallvirtualHPP
