// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteStaticVirtual.pas' rev: 35.00 (Windows)

#ifndef LitestaticvirtualHPP
#define LitestaticvirtualHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Crtl.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CREncryption.hpp>
#include <MemData.hpp>
#include <MemUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.DateUtils.hpp>
#include <LiteConstsVirtual.hpp>
#include <LiteCallVirtual.hpp>

//-- user supplied -----------------------------------------------------------

namespace Litestaticvirtual
{
//-- forward type declarations -----------------------------------------------
struct OQQQOCCOQ0;
class DELPHICLASS OC0COCCOQ0;
//-- type declarations -------------------------------------------------------
typedef NativeUInt OC0QOCCOQ0;

typedef int O0OQOCCOQ0;

typedef void * OOOQOCCOQ0;

typedef void * OQOQOCCOQ0;

typedef int OCOQOCCOQ0;

typedef int *O0QQOCCOQ0;

typedef OQQQOCCOQ0 *OOQQOCCOQ0;

struct DECLSPEC_DRECORD OQQQOCCOQ0
{
public:
	char *OCQQOCCOQ0;
	void *O0CQOCCOQ0;
	System::Byte OOCQOCCOQ0;
	System::Byte OQCQOCCOQ0;
	void *OCCQOCCOQ0;
};


typedef System::DynamicArray<OOQQOCCOQ0> O00COCCOQ0;

typedef O00COCCOQ0 *OO0COCCOQ0;

typedef OC0COCCOQ0* *OQ0COCCOQ0;

#pragma pack(push,4)
class PASCALIMPLEMENTATION OC0COCCOQ0 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int O0OCOCCOQ0;
	void *OOOCOCCOQ0;
	void *OQOCOCCOQ0;
	Crencryption::TCREncryptor* OCOCOCCOQ0;
	Crencryption::TCREncryptor* O0QCOCCOQ0;
	System::AnsiString OOQCOCCOQ0;
	System::AnsiString OQQCOCCOQ0;
	Crencryption::TCREncryptionAlgorithm OCQCOCCOQ0;
	void __fastcall O0CCOCCOQ0(int OOCCOCCOQ0);
	void __fastcall OQCCOCCOQ0(System::AnsiString OCCCOCCOQ0);
	void __fastcall O000OCCOQ0(System::AnsiString OO00OCCOQ0);
	
public:
	__fastcall OC0COCCOQ0(System::AnsiString OC00OCCOQ0, System::AnsiString O0O0OCCOQ0, Crencryption::TCREncryptionAlgorithm OOO0OCCOQ0);
	__fastcall virtual ~OC0COCCOQ0();
	__property Crencryption::TCREncryptionAlgorithm OCO0OCCOQ0 = {read=OCQCOCCOQ0, nodefault};
	__property System::AnsiString O0Q0OCCOQ0 = {read=OOQCOCCOQ0, write=OQCCOCCOQ0};
	__property System::AnsiString OOQ0OCCOQ0 = {read=OQQCOCCOQ0, write=O000OCCOQ0};
	__property Crencryption::TCREncryptor* OQQ0OCCOQ0 = {read=OCOCOCCOQ0};
	__property Crencryption::TCREncryptor* OCQ0OCCOQ0 = {read=O0QCOCCOQ0};
	__property void * O0C0OCCOQ0 = {read=OOOCOCCOQ0};
	__property void * OOC0OCCOQ0 = {read=OQOCOCCOQ0};
	__property int OQC0OCCOQ0 = {read=O0OCOCCOQ0, write=O0CCOCCOQ0, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int _fltused;
static const System::Int8 O00QOCCOQ0 = System::Int8(0x16);
static const System::Int8 OO0QOCCOQ0 = System::Int8(0x10);
static const System::Int8 OQ0QOCCOQ0 = System::Int8(0x64);
extern DELPHI_PACKAGE System::Word __turbofloat;
extern DELPHI_PACKAGE System::Syncobjs::TCriticalSection* OOC00QCOQ0;
extern DELPHI_PACKAGE int __fastcall OC0OOCCOQ0(void);
extern DELPHI_PACKAGE void __fastcall DoInitStaticFunction(Litecallvirtual::TSQLite3API* const OQ0OOCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_key(void * OQ0QOQCOQ0, char * OC0QOQCOQ0, int O0OQOQCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_key_v2(void * OOOQOQCOQ0, char * OQOQOQCOQ0, char * OCOQOQCOQ0, int O0QQOQCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_rekey(void * OCQQOQCOQ0, char * O0CQOQCOQ0, int OOCQOQCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_rekey_v2(void * OQCQOQCOQ0, char * OCCQOQCOQ0, char * O00COQCOQ0, int OO0COQCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_activate_see(char * OQ00OQCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3CustomPragma(void * OC00OQCOQ0, int O0O0OQCOQ0, char * OOO0OQCOQ0, char * OQO0OQCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3CodecAttach(void * O0OCOQCOQ0, int OOOCOQCOQ0, char * OQOCOQCOQ0, int OCOCOQCOQ0, Liteconstsvirtual::TLiteEncryptionAlgorithm O0QCOQCOQ0 = (Liteconstsvirtual::TLiteEncryptionAlgorithm)(0x7));
extern DELPHI_PACKAGE void __cdecl sqlite3CodecGetKey(void * OOCCOQCOQ0, int OQCCOQCOQ0, void * &OCCCOQCOQ0, PINT &O000OQCOQ0);
extern DELPHI_PACKAGE void __fastcall Sqlite3CodecSetKey(void * OOQ0OQCOQ0, int OQQ0OQCOQ0, char * OCQ0OQCOQ0, bool O0C0OQCOQ0, Liteconstsvirtual::TLiteEncryptionAlgorithm OOC0OQCOQ0 = (Liteconstsvirtual::TLiteEncryptionAlgorithm)(0x7));
extern DELPHI_PACKAGE __int64 __cdecl _ftol(void);
extern DELPHI_PACKAGE unsigned __int64 __cdecl _ftoul(void);
extern DELPHI_PACKAGE void * __cdecl strrchr(const void * OOQQ0QCOQ0, int OQQQ0QCOQ0);
extern DELPHI_PACKAGE int __cdecl strcspn(const void * OQCQ0QCOQ0, const void * OCCQ0QCOQ0);
extern DELPHI_PACKAGE void * __cdecl malloc(NativeUInt OQ0C0QCOQ0);
extern DELPHI_PACKAGE void * __cdecl realloc(void * OC0C0QCOQ0, NativeUInt O0OC0QCOQ0);
extern DELPHI_PACKAGE void __cdecl memcpy(void * OOOC0QCOQ0, const void * OQOC0QCOQ0, NativeUInt OCOC0QCOQ0);
extern DELPHI_PACKAGE void __cdecl free(void * O0QC0QCOQ0);
extern DELPHI_PACKAGE void * __cdecl memset(void * OOQC0QCOQ0, int OQQC0QCOQ0, NativeUInt OCQC0QCOQ0);
extern DELPHI_PACKAGE void __cdecl memmove(void * O0CC0QCOQ0, const void * OOCC0QCOQ0, NativeUInt OQCC0QCOQ0);
extern DELPHI_PACKAGE int __cdecl memcmp(void * OCCC0QCOQ0, void * O0000QCOQ0, NativeUInt OO000QCOQ0);
extern DELPHI_PACKAGE int __cdecl strlen(void * OQ000QCOQ0);
extern DELPHI_PACKAGE void __fastcall _lldiv(void);
extern DELPHI_PACKAGE void __fastcall _lludiv(void);
extern DELPHI_PACKAGE void __fastcall _llmod(void);
extern DELPHI_PACKAGE void __fastcall _llmul(void);
extern DELPHI_PACKAGE void __fastcall _llumod(void);
extern DELPHI_PACKAGE void __fastcall _llshl(void);
extern DELPHI_PACKAGE void __fastcall _llshr(void);
extern DELPHI_PACKAGE void __fastcall _llushr(void);
extern DELPHI_PACKAGE int __cdecl strncmp(System::PByte OC000QCOQ0, System::PByte O0O00QCOQ0, NativeUInt OOO00QCOQ0);
extern DELPHI_PACKAGE int __cdecl strcmp(System::PByte OQO00QCOQ0, System::PByte OCO00QCOQ0);
extern DELPHI_PACKAGE Litecallvirtual::ptm __fastcall localtime(void * O0Q00QCOQ0);
extern DELPHI_PACKAGE void * __cdecl sqlite3_malloc(int O0OOOCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_enable_shared_cache(int O0Q0QCCOQ0);
extern DELPHI_PACKAGE void * __cdecl sqlite3_backup_init(void * O00OQCCOQ0, char * OO0OQCCOQ0, void * OQ0OQCCOQ0, char * OC0OQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_backup_step(void * O0OOQCCOQ0, int OOOOQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_backup_finish(void * OQOOQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_backup_remaining(void * OCOOQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_backup_pagecount(void * O0QOQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_finalize(void * O0CQ0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_reset(void * OCQQ0CCOQ0);
extern DELPHI_PACKAGE void * __cdecl sqlite3_value_blob(void * OC0OCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_value_bytes(void * O0OOCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_value_bytes16(void * OOOOCCCOQ0);
extern DELPHI_PACKAGE double __cdecl sqlite3_value_double(void * OQOOCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_value_int(void * OCOOCCCOQ0);
extern DELPHI_PACKAGE __int64 __cdecl sqlite3_value_int64(void * O0QOCCCOQ0);
extern DELPHI_PACKAGE void * __cdecl sqlite3_value_text(void * OOQOCCCOQ0);
extern DELPHI_PACKAGE void * __cdecl sqlite3_value_text16(void * OQQOCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_value_type(void * OCQOCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_blob(void * OQQQCCCOQ0, void * OCQQCCCOQ0, int O0CQCCCOQ0, void * OOCQCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_double(void * OQCQCCCOQ0, double OCCQCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_error(void * O00CCCCOQ0, void * OO0CCCCOQ0, int OQ0CCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_error16(void * OC0CCCCOQ0, void * O0OCCCCOQ0, int OOOCCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_int(void * OQQCCCCOQ0, int OCQCCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_int64(void * O0CCCCCOQ0, __int64 OOCCCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_null(void * OQCCCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_text(void * OCCCCCCOQ0, void * O000CCCOQ0, int OO00CCCOQ0, void * OQ00CCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_text16(void * OC00CCCOQ0, void * O0O0CCCOQ0, int OOO0CCCOQ0, void * OQO0CCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_text16be(void * OCQ0CCCOQ0, void * O0C0CCCOQ0, int OOC0CCCOQ0, void * OQC0CCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_text16le(void * OCO0CCCOQ0, void * O0Q0CCCOQ0, int OOQ0CCCOQ0, void * OQQ0CCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_value(void * OCC0CCCOQ0, void * O00OCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_zeroblob(void * OO0OCCCOQ0, int OQ0OCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_error_code(void * O0QCCCCOQ0, int OOQCCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_error_toobig(void * OQOCCCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_result_error_nomem(void * OCOCCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_step(void * OQQQ0CCOQ0);
extern DELPHI_PACKAGE void * __cdecl sqlite3_user_data(void * O0COCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_column_count(void * OOCQ0CCOQ0);
extern DELPHI_PACKAGE void * __cdecl sqlite3_column_blob(void * OC000CCOQ0, int O0O00CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_column_bytes(void * OOO00CCOQ0, int OQO00CCOQ0);
extern DELPHI_PACKAGE double __cdecl sqlite3_column_double(void * OCO00CCOQ0, int O0Q00CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_column_int(void * OOQ00CCOQ0, int OQQ00CCOQ0);
extern DELPHI_PACKAGE __int64 __cdecl sqlite3_column_int64(void * OCQ00CCOQ0, int O0C00CCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_column_text(void * OOC00CCOQ0, int OQC00CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_column_type(void * OQCQ0CCOQ0, int OCCQ0CCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_column_name(void * O00C0CCOQ0, int OO0C0CCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_column_decltype(void * O0QC0CCOQ0, int OOQC0CCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_column_database_name(void * OQOC0CCOQ0, int OCOC0CCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_column_table_name(void * O0OC0CCOQ0, int OOOC0CCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_column_origin_name(void * OQ0C0CCOQ0, int OC0C0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_bind_blob(void * OQ0O0CCOQ0, int OC0O0CCOQ0, void * O0OO0CCOQ0, int OOOO0CCOQ0, void * OQOO0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_bind_double(void * OQQO0CCOQ0, int OCQO0CCOQ0, double O0CO0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_bind_int(void * OOCO0CCOQ0, int OQCO0CCOQ0, int OCCO0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_bind_int64(void * O00QCCCOQ0, int OO0QCCCOQ0, __int64 OQ0QCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_bind_null(void * OC0QCCCOQ0, int O0OQCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_bind_text(void * OOOQCCCOQ0, int OQOQCCCOQ0, char * OCOQCCCOQ0, int O0QQCCCOQ0, void * OOQQCCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_bind_zeroblob(void * OCOO0CCOQ0, int O0QO0CCOQ0, int OOQO0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_bind_parameter_count(void * OCC00CCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_bind_parameter_name(void * O00O0CCOQ0, int OO0O0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_enable_load_extension(void * OOQ0QCCOQ0, int OQQ0QCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_prepare_v2(void * OOOQ0CCOQ0, char * OQOQ0CCOQ0, int OCOQ0CCOQ0, /* out */ void * &O0QQ0CCOQ0, /* out */ void * &OOQQ0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_create_module(void * OQQCQCCOQ0, void * OCQCQCCOQ0, void * O0CCQCCOQ0, void * OOCCQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_create_module_v2(void * OQCCQCCOQ0, void * OCCCQCCOQ0, void * O000QCCOQ0, void * OO00QCCOQ0, void * OQ00QCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_declare_vtab(void * OC00QCCOQ0, const void * O0O0QCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_vtab_config(void * OOO0QCCOQ0, int OQO0QCCOQ0, int OCO0QCCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_libversion(void);
extern DELPHI_PACKAGE int __cdecl sqlite3_libversion_number(void);
extern DELPHI_PACKAGE int __cdecl sqlite3_db_release_memory(void * OOQOQCCOQ0);
extern DELPHI_PACKAGE __int64 __cdecl sqlite3_last_insert_rowid(void * OC0Q0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_changes(void * O0OQ0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_close(void * OOCOOCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_busy_handler(void * OOC0QCCOQ0, void * OQC0QCCOQ0, void * OCC0QCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_busy_timeout(void * OCQ0QCCOQ0, int O0C0QCCOQ0);
extern DELPHI_PACKAGE void __cdecl sqlite3_interrupt(void * OO0QOQCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_create_function(void * OCOQQCCOQ0, char * O0QQQCCOQ0, int OOQQQCCOQ0, int OQQQQCCOQ0, void * OCQQQCCOQ0, void * O0CQQCCOQ0, void * OOCQQCCOQ0, void * OQCQQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_create_function16(void * OCCQQCCOQ0, System::WideChar * O00CQCCOQ0, int OO0CQCCOQ0, int OQ0CQCCOQ0, void * OC0CQCCOQ0, void * O0OCQCCOQ0, void * OOOCQCCOQ0, void * OQOCQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_overload_function(void * OCOCQCCOQ0, char * O0QCQCCOQ0, int OOQCQCCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_errmsg(void * OQCOOCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_errcode(void * OCCOOCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_extended_errcode(void * O00Q0CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_limit(void * O0COQCCOQ0, int OOCOQCCOQ0, int OQCOQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_open(char * OOOOOCCOQ0, /* out */ void * &OQOOOCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_open_v2(char * OOQOOCCOQ0, /* out */ void * &OQQOOCCOQ0, int OCQOOCCOQ0, char * O0COOCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_open16(System::WideChar * OCOOOCCOQ0, /* out */ void * &O0QOOCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_create_collation(void * OOCOCCCOQ0, char * OQCOCCCOQ0, int OCCOCCCOQ0, void * O00QQCCOQ0, void * OO0QQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_create_collation16(void * OQ0QQCCOQ0, System::WideChar * OC0QQCCOQ0, int O0OQQCCOQ0, void * OOOQQCCOQ0, void * OQOQQCCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_table_column_metadata(void * OQQC0CCOQ0, void * OCQC0CCOQ0, void * O0CC0CCOQ0, void * OOCC0CCOQ0, /* out */ void * &OQCC0CCOQ0, /* out */ void * &OCCC0CCOQ0, /* out */ int &O0000CCOQ0, /* out */ int &OO000CCOQ0, /* out */ int &OQ000CCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_extended_result_codes(void * OO0Q0CCOQ0, int OQ0Q0CCOQ0);
extern DELPHI_PACKAGE char * __cdecl sqlite3_uri_parameter(char * OCCOQCCOQ0, char * O00QOQCOQ0);
extern DELPHI_PACKAGE int __cdecl sqlite3_db_readonly(void * OQQOQCCOQ0, char * OCQOQCCOQ0);
extern DELPHI_PACKAGE OOQQOCCOQ0 __cdecl lite_sqlite3GetBackend(void * O0OOOQCOQ0, int OOOOOQCOQ0);
extern DELPHI_PACKAGE void * __cdecl lite_sqlite3GetBtreePager(void * OOCOOQCOQ0);
extern DELPHI_PACKAGE int __cdecl lite_sqlite3BtreeGetPageSize(void * OQOOOQCOQ0);
extern DELPHI_PACKAGE int __cdecl lite_sqlite3BtreeBeginTrans(void * OCOOOQCOQ0, int O0QOOQCOQ0, System::PInteger OOQOOQCOQ0);
extern DELPHI_PACKAGE void * __cdecl lite_sqlite3PagerGetCodec(void * OQCOOQCOQ0);
extern DELPHI_PACKAGE void __cdecl lite_sqlite3PagerSetCodec(void * OCC0OQCOQ0, void * O00OOQCOQ0, void * OO0OOQCOQ0, void * OQ0OOQCOQ0, void * OC0OOQCOQ0);
extern DELPHI_PACKAGE void __cdecl lite_sqlite3PagerPagecount(void * OCCOOQCOQ0, System::PInteger O00Q0QCOQ0);
extern DELPHI_PACKAGE int __cdecl lite_sqlite3PagerIsMjPgno(void * OO0Q0QCOQ0, unsigned OQ0Q0QCOQ0);
extern DELPHI_PACKAGE int __cdecl lite_sqlite3PagerGet(void * OC0Q0QCOQ0, unsigned O0OQ0QCOQ0, void * OOOQ0QCOQ0, int OQOQ0QCOQ0);
extern DELPHI_PACKAGE int __cdecl lite_sqlite3PagerWrite(void * OCOQ0QCOQ0);
extern DELPHI_PACKAGE void __cdecl lite_sqlite3PagerUnref(void * O0QQ0QCOQ0);
extern DELPHI_PACKAGE int __cdecl lite_sqlite3BtreeCommit(void * OQQOOQCOQ0);
extern DELPHI_PACKAGE int __cdecl lite_sqlite3BtreeRollback(void * OCQOOQCOQ0, int O0COOQCOQ0);
}	/* namespace Litestaticvirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITESTATICVIRTUAL)
using namespace Litestaticvirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LitestaticvirtualHPP
