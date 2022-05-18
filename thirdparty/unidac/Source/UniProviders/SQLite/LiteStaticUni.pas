//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$IFNDEF VIRTUAL_QUERY}
{$I LiteDac.inc}
unit LiteStaticUni;
{$ENDIF}
interface
{$IFNDEF NOSTATIC}
uses
{$IFDEF MSWINDOWS}
Windows,{$IFDEF VER16P}System.Win.Crtl,{$ENDIF}
{$ENDIF}
Classes,
{$IFDEF CUSTOM_VFS}
{$IFNDEF POSIX_VFS} System.IOUtils,{$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
Posix.Unistd,
{$ENDIF}
{$IFDEF POSIX_VFS}
Posix.Base,Posix.SysTypes,
{$ENDIF}
{$IFDEF LINUX_FPC}
cthreads,
{$ENDIF}
SysUtils,
CLRClasses,
CRTypes,
{$IFDEF CODEC}
CRFunctions,
CREncryption,
MemData,
MemUtils,
SyncObjs,
{$ENDIF}
{$IFNDEF LITEDLL}
DateUtils,
{$ENDIF}
{$IFDEF VIRTUAL_QUERY}
LiteConstsVirtual,LiteCallVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
LiteConsts,LiteCall;
{$ELSE}
LiteConstsUni,LiteCallUni;
{$ENDIF}
{$ENDIF}
var
_fltused:Integer=0;
{$IFNDEF LITEDLL}
{$IFNDEF FPC}
{$IFDEF NAMED_EXTERNAL}
const
{$IFDEF ANDROID}
{$IFNDEF VER26P}
sqlite3o='sqlite3android32.o';
{$ELSE}
{$IFDEF ANDROID32}
sqlite3o='sqlite3android32.o';
{$ENDIF}
{$IFDEF ANDROID64}
sqlite3o='sqlite3android64.o';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF IOS}
{$IFNDEF VER22P}
sqlite3o='sqlite3ios32.o';
{$ELSE}
{$IFDEF IOS32}
sqlite3o='sqlite3ios32.o';
{$ENDIF}
{$IFDEF IOS64}
sqlite3o='sqlite3ios64.o';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
sqlite3o='sqlite3linux64.o';
{$ENDIF}
{$IFDEF MACOS64}
{$IFNDEF CPUARM64}
sqlite3o='sqlite3osx64.o';
{$ENDIF}
{$ENDIF}
{$ELSE}
{$IFDEF WIN32}
{$L 'sqlite3\sqlite3win32.o'}
{$ENDIF}
{$IFDEF WIN64}
{$L 'sqlite3\sqlite3win64.o'}
{$ENDIF}
{$IFDEF POSIX_VFS}
{$L 'sqlite3\sqlite3osx32.o'}
{$ENDIF}
{$ENDIF}
{$ELSE}
{$IFDEF WIN32}
{$L 'sqlite3fpc32.o'}
{$ENDIF}
{$IFDEF WIN64}
{$L 'sqlite3fpc64.o'}
{$ENDIF}
{$IFDEF MACOS}
{$IFDEF CPUX86}
{$linklib libgcc.a}
{$L 'sqlite3fpcosx32.o'}
{$ELSE}
{$L 'sqlite3fpcosx64.o'}
{$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
{$IFDEF CPUX86}
{$linklib libgcc.a}
{$L 'sqlite3fpclinux32.o'}
{$ELSE}
{$L 'sqlite3fpclinux64.o'}
{$ENDIF}
{$ENDIF}
{$IFNDEF UNIX}
{$linklib libgcc.a}
{$linklib libmsvcrt.a}
{$linklib libkernel32.a}
{$ENDIF}
{$ENDIF}
{$ELSE}
const
{$IFDEF WIN32}
sqlite3o='sqlite3dac.obj';
PU='_';
{$ENDIF}
{$IFDEF WIN64}
sqlite3o='sqlite3dac.o';
PU='';
{$ENDIF}
{$ENDIF}
{$IFDEF CODEC}
const
O00QOCCOQ0=22;
OO0QOCCOQ0=16;
OQ0QOCCOQ0=100;
type
OC0QOCCOQ0=NativeUInt;
O0OQOCCOQ0=integer;
OOOQOCCOQ0=IntPtr;
OQOQOCCOQ0=IntPtr;
OCOQOCCOQ0=integer;
O0QQOCCOQ0=^OCOQOCCOQ0;
OOQQOCCOQ0=^OQQQOCCOQ0;
OQQQOCCOQ0=record
OCQQOCCOQ0:PAnsiChar;
O0CQOCCOQ0:IntPtr;
OOCQOCCOQ0:byte;
OQCQOCCOQ0:byte;
OCCQOCCOQ0:IntPtr;
end;
O00COCCOQ0=array of OOQQOCCOQ0;
OO0COCCOQ0=^O00COCCOQ0;
OQ0COCCOQ0=^OC0COCCOQ0;
OC0COCCOQ0=class
private
O0OCOCCOQ0:integer;
OOOCOCCOQ0,
OQOCOCCOQ0:IntPtr;
OCOCOCCOQ0:TCREncryptor;
O0QCOCCOQ0:TCREncryptor;
OOQCOCCOQ0:AnsiString;
OQQCOCCOQ0:AnsiString;
OCQCOCCOQ0:TCREncryptionAlgorithm;
procedure O0CCOCCOQ0(OOCCOCCOQ0:integer);
procedure OQCCOCCOQ0(OCCCOCCOQ0:AnsiString);
procedure O000OCCOQ0(OO00OCCOQ0:AnsiString);
public
constructor Create(OC00OCCOQ0,O0O0OCCOQ0:AnsiString;OOO0OCCOQ0:TCREncryptionAlgorithm);
destructor Destroy;override;
property OCO0OCCOQ0:TCREncryptionAlgorithm read OCQCOCCOQ0;
property O0Q0OCCOQ0:AnsiString read OOQCOCCOQ0 write OQCCOCCOQ0;
property OOQ0OCCOQ0:AnsiString read OQQCOCCOQ0 write O000OCCOQ0;
property OQQ0OCCOQ0:TCREncryptor read OCOCOCCOQ0;
property OCQ0OCCOQ0:TCREncryptor read O0QCOCCOQ0;
property O0C0OCCOQ0:IntPtr read OOOCOCCOQ0;
property OOC0OCCOQ0:IntPtr read OQOCOCCOQ0;
property OQC0OCCOQ0:integer read O0OCOCCOQ0 write O0CCOCCOQ0;
end;
{$ENDIF CODEC}
{$IFNDEF NOIMPLEMENT}
type
OCC0OCCOQ0=function(O00OOCCOQ0,OO0OOCCOQ0:pointer):integer;cdecl;
{$ENDIF}
procedure DoInitStaticFunction(const OQ0OOCCOQ0:TSQLite3API);
function OC0OOCCOQ0:integer;
function sqlite3_malloc(
O0OOOCCOQ0:integer
):IntPtr;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_malloc'{$ENDIF};
function sqlite3_open(
OOOOOCCOQ0:PAnsiChar;
out OQOOOCCOQ0:pSQLite3
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_open'{$ENDIF};
function sqlite3_open16(
OCOOOCCOQ0:PWideChar;
out O0QOOCCOQ0:pSQLite3
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_open16'{$ENDIF};
function sqlite3_open_v2(
OOQOOCCOQ0:PAnsiChar;
out OQQOOCCOQ0:pSQLite3;
OCQOOCCOQ0:integer;
O0COOCCOQ0:PAnsiChar
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_open_v2'{$ENDIF};
function sqlite3_close(
OOCOOCCOQ0:pSQLite3
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_close'{$ENDIF};
function sqlite3_errmsg(
OQCOOCCOQ0:pSQLite3
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_errmsg'{$ENDIF};
function sqlite3_errcode(
OCCOOCCOQ0:pSQLite3
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_errcode'{$ENDIF};
function sqlite3_extended_errcode(
O00Q0CCOQ0:pSQLite3
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_extended_errcode'{$ENDIF};
function sqlite3_extended_result_codes(
OO0Q0CCOQ0:pSQLite3;
OQ0Q0CCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_extended_result_codes'{$ENDIF};
function sqlite3_last_insert_rowid(
OC0Q0CCOQ0:pSQLite3
):int64;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_last_insert_rowid'{$ENDIF};
function sqlite3_changes(
O0OQ0CCOQ0:pSQLite3
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_changes'{$ENDIF};
function sqlite3_prepare_v2(
OOOQ0CCOQ0:pSQLite3;
OQOQ0CCOQ0:PAnsiChar;
OCOQ0CCOQ0:integer;
out O0QQ0CCOQ0:pSQLite3Stmt;
out OOQQ0CCOQ0:IntPtr
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_prepare_v2'{$ENDIF};
function sqlite3_step(
OQQQ0CCOQ0:pSQLite3Stmt
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_step'{$ENDIF};
function sqlite3_reset(
OCQQ0CCOQ0:pSQLite3Stmt
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_reset'{$ENDIF};
function sqlite3_finalize(
O0CQ0CCOQ0:pSQLite3Stmt
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_finalize'{$ENDIF};
function sqlite3_column_count(
OOCQ0CCOQ0:pSQLite3Stmt
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_count'{$ENDIF};
function sqlite3_column_type(
OQCQ0CCOQ0:pSQLite3Stmt;OCCQ0CCOQ0:integer
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_type'{$ENDIF};
function sqlite3_column_name(
O00C0CCOQ0:pSQLite3Stmt;OO0C0CCOQ0:integer
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_name'{$ENDIF};
function sqlite3_column_origin_name(
OQ0C0CCOQ0:pSQLite3Stmt;OC0C0CCOQ0:integer
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_origin_name'{$ENDIF};
function sqlite3_column_table_name(
O0OC0CCOQ0:pSQLite3Stmt;OOOC0CCOQ0:integer
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_table_name'{$ENDIF};
function sqlite3_column_database_name(
OQOC0CCOQ0:pSQLite3Stmt;OCOC0CCOQ0:integer
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_database_name'{$ENDIF};
function sqlite3_column_decltype(
O0QC0CCOQ0:pSQLite3Stmt;OOQC0CCOQ0:integer
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_decltype'{$ENDIF};
function sqlite3_table_column_metadata(
OQQC0CCOQ0:pSQLite3;
OCQC0CCOQ0:IntPtr;
O0CC0CCOQ0:IntPtr;
OOCC0CCOQ0:IntPtr;
out OQCC0CCOQ0:IntPtr;
out OCCC0CCOQ0:IntPtr;
out O0000CCOQ0:Integer;
out OO000CCOQ0:Integer;
out OQ000CCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_table_column_metadata'{$ENDIF};
function sqlite3_column_blob(
OC000CCOQ0:pSQLite3Stmt;O0O00CCOQ0:integer
):IntPtr;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_blob'{$ENDIF};
function sqlite3_column_bytes(
OOO00CCOQ0:pSQLite3Stmt;OQO00CCOQ0:integer
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_bytes'{$ENDIF};
function sqlite3_column_double(
OCO00CCOQ0:pSQLite3Stmt;O0Q00CCOQ0:integer
):double;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_double'{$ENDIF};
function sqlite3_column_int(
OOQ00CCOQ0:pSQLite3Stmt;OQQ00CCOQ0:integer
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_int'{$ENDIF};
function sqlite3_column_int64(
OCQ00CCOQ0:pSQLite3Stmt;O0C00CCOQ0:integer
):int64;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_int64'{$ENDIF};
function sqlite3_column_text(
OOC00CCOQ0:pSQLite3Stmt;OQC00CCOQ0:integer
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_column_text'{$ENDIF};
function sqlite3_bind_parameter_count(
OCC00CCOQ0:pSQLite3Stmt
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_parameter_count'{$ENDIF};
function sqlite3_bind_parameter_name(
O00O0CCOQ0:pSQLite3Stmt;OO0O0CCOQ0:integer
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_parameter_name'{$ENDIF};
function sqlite3_bind_blob(
OQ0O0CCOQ0:pSQLite3Stmt;OC0O0CCOQ0:integer;O0OO0CCOQ0:IntPtr;OOOO0CCOQ0:integer;OQOO0CCOQ0:IntPtr
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_blob'{$ENDIF};
function sqlite3_bind_zeroblob(
OCOO0CCOQ0:pSQLite3Stmt;O0QO0CCOQ0:integer;OOQO0CCOQ0:integer
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_zeroblob'{$ENDIF};
function sqlite3_bind_double(
OQQO0CCOQ0:pSQLite3Stmt;OCQO0CCOQ0:integer;O0CO0CCOQ0:double
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_double'{$ENDIF};
function sqlite3_bind_int(
OOCO0CCOQ0:pSQLite3Stmt;OQCO0CCOQ0:integer;OCCO0CCOQ0:integer
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_int'{$ENDIF};
function sqlite3_bind_int64(
O00QCCCOQ0:pSQLite3Stmt;OO0QCCCOQ0:integer;OQ0QCCCOQ0:int64
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_int64'{$ENDIF};
function sqlite3_bind_null(
OC0QCCCOQ0:pSQLite3Stmt;O0OQCCCOQ0:integer
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_null'{$ENDIF};
function sqlite3_bind_text(
OOOQCCCOQ0:pSQLite3Stmt;OQOQCCCOQ0:integer;OCOQCCCOQ0:PAnsiChar;O0QQCCCOQ0:integer;OOQQCCCOQ0:IntPtr
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_bind_text'{$ENDIF};
procedure sqlite3_result_blob(
OQQQCCCOQ0:pSQLite3Context;OCQQCCCOQ0:IntPtr;O0CQCCCOQ0:Integer;OOCQCCCOQ0:IntPtr
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_blob'{$ENDIF};
procedure sqlite3_result_double(
OQCQCCCOQ0:pSQLite3Context;OCCQCCCOQ0:Double
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_double'{$ENDIF};
procedure sqlite3_result_error(
O00CCCCOQ0:pSQLite3Context;OO0CCCCOQ0:IntPtr;OQ0CCCCOQ0:Integer
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_error'{$ENDIF};
procedure sqlite3_result_error16(
OC0CCCCOQ0:pSQLite3Context;O0OCCCCOQ0:IntPtr;OOOCCCCOQ0:Integer
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_error16'{$ENDIF};
procedure sqlite3_result_error_toobig(
OQOCCCCOQ0:pSQLite3Context
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_error_toobig'{$ENDIF};
procedure sqlite3_result_error_nomem(
OCOCCCCOQ0:pSQLite3Context
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_error_nomem'{$ENDIF};
procedure sqlite3_result_error_code(
O0QCCCCOQ0:pSQLite3Context;OOQCCCCOQ0:Integer
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_error_code'{$ENDIF};
procedure sqlite3_result_int(
OQQCCCCOQ0:pSQLite3Context;OCQCCCCOQ0:Integer
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_int'{$ENDIF};
procedure sqlite3_result_int64(
O0CCCCCOQ0:pSQLite3Context;OOCCCCCOQ0:Int64
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_int64'{$ENDIF};
procedure sqlite3_result_null(
OQCCCCCOQ0:pSQLite3Context
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_null'{$ENDIF};
procedure sqlite3_result_text(
OCCCCCCOQ0:pSQLite3Context;O000CCCOQ0:IntPtr;OO00CCCOQ0:Integer;OQ00CCCOQ0:IntPtr
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_text'{$ENDIF};
procedure sqlite3_result_text16(
OC00CCCOQ0:pSQLite3Context;O0O0CCCOQ0:IntPtr;OOO0CCCOQ0:Integer;OQO0CCCOQ0:IntPtr
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_text16'{$ENDIF};
procedure sqlite3_result_text16le(
OCO0CCCOQ0:pSQLite3Context;O0Q0CCCOQ0:IntPtr;OOQ0CCCOQ0:Integer;OQQ0CCCOQ0:IntPtr
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_text16le'{$ENDIF};
procedure sqlite3_result_text16be(
OCQ0CCCOQ0:pSQLite3Context;O0C0CCCOQ0:IntPtr;OOC0CCCOQ0:Integer;OQC0CCCOQ0:IntPtr
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_text16be'{$ENDIF};
procedure sqlite3_result_value(
OCC0CCCOQ0:pSQLite3Context;O00OCCCOQ0:pSQLite3Value
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_value'{$ENDIF};
procedure sqlite3_result_zeroblob(
OO0OCCCOQ0:pSQLite3Context;OQ0OCCCOQ0:Integer
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_result_zeroblob'{$ENDIF};
function sqlite3_value_blob(
OC0OCCCOQ0:IntPtr
):IntPtr;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_blob'{$ENDIF};
function sqlite3_value_bytes(
O0OOCCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_bytes'{$ENDIF};
function sqlite3_value_bytes16(
OOOOCCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_bytes16'{$ENDIF};
function sqlite3_value_double(
OQOOCCCOQ0:IntPtr
):Double;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_double'{$ENDIF};
function sqlite3_value_int(
OCOOCCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_int'{$ENDIF};
function sqlite3_value_int64(
O0QOCCCOQ0:IntPtr
):Int64;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_int64'{$ENDIF};
function sqlite3_value_text(
OOQOCCCOQ0:IntPtr
):IntPtr;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_text'{$ENDIF};
function sqlite3_value_text16(
OQQOCCCOQ0:IntPtr
):IntPtr;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_text16'{$ENDIF};
function sqlite3_value_type(
OCQOCCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_value_type'{$ENDIF};
function sqlite3_user_data(
O0COCCCOQ0:pSQLite3Context
):IntPtr;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_user_data'{$ENDIF};
function sqlite3_libversion(
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_libversion'{$ENDIF};
function sqlite3_libversion_number(
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_libversion_number'{$ENDIF};
function sqlite3_create_collation(
OOCOCCCOQ0:pSQLite3;OQCOCCCOQ0:PAnsiChar;OCCOCCCOQ0:Integer;O00QQCCOQ0:IntPtr;OO0QQCCOQ0:IntPtr
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_create_collation'{$ENDIF};
function sqlite3_create_collation16(
OQ0QQCCOQ0:pSQLite3;OC0QQCCOQ0:PWideChar;O0OQQCCOQ0:Integer;OOOQQCCOQ0:IntPtr;OQOQQCCOQ0:IntPtr
):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_create_collation16'{$ENDIF};
function sqlite3_create_function(
OCOQQCCOQ0:pSQLite3;O0QQQCCOQ0:PAnsiChar;OOQQQCCOQ0:Integer;OQQQQCCOQ0:Integer;OCQQQCCOQ0:IntPtr;O0CQQCCOQ0:IntPtr;OOCQQCCOQ0:IntPtr;OQCQQCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_create_function'{$ENDIF};
function sqlite3_create_function16(
OCCQQCCOQ0:pSQLite3;O00CQCCOQ0:PWideChar;OO0CQCCOQ0:Integer;OQ0CQCCOQ0:Integer;OC0CQCCOQ0:IntPtr;O0OCQCCOQ0:IntPtr;OOOCQCCOQ0:IntPtr;OQOCQCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_create_function16'{$ENDIF};
function sqlite3_overload_function(
OCOCQCCOQ0:pSQLite3;O0QCQCCOQ0:PAnsiChar;OOQCQCCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_overload_function'{$ENDIF};
function sqlite3_create_module(
OQQCQCCOQ0:pSQLite3;OCQCQCCOQ0:IntPtr;O0CCQCCOQ0:IntPtr;OOCCQCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_create_module'{$ENDIF};
function sqlite3_create_module_v2(
OQCCQCCOQ0:pSQLite3;OCCCQCCOQ0:IntPtr;O000QCCOQ0:IntPtr;OO00QCCOQ0:IntPtr;OQ00QCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_create_module_v2'{$ENDIF};
function sqlite3_declare_vtab(
OC00QCCOQ0:pSQLite3;const O0O0QCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_declare_vtab'{$ENDIF};
function sqlite3_vtab_config(
OOO0QCCOQ0:pSQLite3;OQO0QCCOQ0:Integer;OCO0QCCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_vtab_config'{$ENDIF};
function sqlite3_enable_shared_cache(
O0Q0QCCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_enable_shared_cache'{$ENDIF};
function sqlite3_enable_load_extension(
OOQ0QCCOQ0:pSQLite3;OQQ0QCCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_enable_load_extension'{$ENDIF};
function sqlite3_busy_timeout(
OCQ0QCCOQ0:pSQLite3;O0C0QCCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_busy_timeout'{$ENDIF};
function sqlite3_busy_handler(
OOC0QCCOQ0:pSQLite3;OQC0QCCOQ0:IntPtr;OCC0QCCOQ0:IntPtr
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_busy_handler'{$ENDIF};
function sqlite3_backup_init(
O00OQCCOQ0:pSQLite3;OO0OQCCOQ0:PAnsiChar;
OQ0OQCCOQ0:pSQLite3;OC0OQCCOQ0:PAnsiChar
):pSQLite3Backup;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_backup_init'{$ENDIF};
function sqlite3_backup_step(
O0OOQCCOQ0:pSQLite3Backup;OOOOQCCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_backup_step'{$ENDIF};
function sqlite3_backup_finish(
OQOOQCCOQ0:pSQLite3Backup
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_backup_finish'{$ENDIF};
function sqlite3_backup_remaining(
OCOOQCCOQ0:pSQLite3Backup
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_backup_remaining'{$ENDIF};
function sqlite3_backup_pagecount(
O0QOQCCOQ0:pSQLite3Backup
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_backup_pagecount'{$ENDIF};
function sqlite3_db_release_memory(
OOQOQCCOQ0:pSQLite3
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_db_release_memory'{$ENDIF};
function sqlite3_db_readonly(
OQQOQCCOQ0:pSQLite3;OCQOQCCOQ0:PAnsiChar
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_db_readonly'{$ENDIF};
function sqlite3_limit(
O0COQCCOQ0:pSQLite3;
OOCOQCCOQ0:integer;
OQCOQCCOQ0:Integer
):Integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_limit'{$ENDIF};
function sqlite3_uri_parameter(
OCCOQCCOQ0:PAnsiChar;
O00QOQCOQ0:PAnsiChar
):PAnsiChar;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_uri_parameter'{$ENDIF};
procedure sqlite3_interrupt(
OO0QOQCOQ0:pSQLite3
);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3_interrupt'{$ENDIF};
{$IFDEF CODEC}
function sqlite3_key(
OQ0QOQCOQ0:pSQLite3;OC0QOQCOQ0:PAnsiChar;O0OQOQCOQ0:Integer
):Integer;cdecl;
function sqlite3_key_v2(
OOOQOQCOQ0:pSQLite3;OQOQOQCOQ0,OCOQOQCOQ0:PAnsiChar;O0QQOQCOQ0:Integer
):Integer;cdecl;
function sqlite3_rekey(
OCQQOQCOQ0:pSQLite3;O0CQOQCOQ0:PAnsiChar;OOCQOQCOQ0:Integer
):Integer;cdecl;
function sqlite3_rekey_v2(
OQCQOQCOQ0:pSQLite3;OCCQOQCOQ0,O00COQCOQ0:PAnsiChar;OO0COQCOQ0:Integer
):Integer;cdecl;
function sqlite3CodecAttach(
O0OCOQCOQ0:pSQLite3;OOOCOQCOQ0:integer;OQOCOQCOQ0:PAnsiChar;OCOCOQCOQ0:Integer;O0QCOQCOQ0:TLiteEncryptionAlgorithm=DefaultEncryptionAlgorithm
):integer;cdecl;
procedure sqlite3CodecGetKey(
OOCCOQCOQ0:pSQLite3;OQCCOQCOQ0:integer;var OCCCOQCOQ0:pointer;var O000OQCOQ0:{$IFDEF POSIX}pointer{$ELSE}pInt{$ENDIF}
);cdecl;
procedure sqlite3_activate_see(
OQ00OQCOQ0:PAnsiChar
);cdecl;
{$IFNDEF LINUX_VFS}
function sqlite3CustomPragma(
OC00OQCOQ0:pSQLite3;O0O0OQCOQ0:Integer;OOO0OQCOQ0:PAnsiChar;OQO0OQCOQ0:PAnsiChar
):integer;cdecl;
{$ENDIF}
procedure Sqlite3CodecSetKey(OOQ0OQCOQ0:pSQLite3;OQQ0OQCOQ0:integer;OCQ0OQCOQ0:PAnsiChar;O0C0OQCOQ0:boolean;OOC0OQCOQ0:TLiteEncryptionAlgorithm=leDefault);
procedure lite_sqlite3PagerSetCodec(OCC0OQCOQ0:pointer;O00OOQCOQ0:pointer;OO0OOQCOQ0:pointer;OQ0OOQCOQ0:pointer;OC0OOQCOQ0:pointer);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3PagerSetCodec'{$ENDIF};
function lite_sqlite3GetBackend(O0OOOQCOQ0:pSQLite3;OOOOOQCOQ0:integer):OOQQOCCOQ0;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3GetBackend'{$ENDIF};
function lite_sqlite3BtreeGetPageSize(OQOOOQCOQ0:OQOQOCCOQ0):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3BtreeGetPageSize'{$ENDIF};
function lite_sqlite3BtreeBeginTrans(OCOOOQCOQ0:OQOQOCCOQ0;O0QOOQCOQ0:integer;OOQOOQCOQ0:PInteger):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3BtreeBeginTrans'{$ENDIF};
function lite_sqlite3BtreeCommit(OQQOOQCOQ0:OQOQOCCOQ0):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3BtreeCommit'{$ENDIF};
function lite_sqlite3BtreeRollback(OCQOOQCOQ0:OQOQOCCOQ0;O0COOQCOQ0:integer):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3BtreeRollback'{$ENDIF};
function lite_sqlite3GetBtreePager(OOCOOQCOQ0:OQOQOCCOQ0):OOOQOCCOQ0;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3GetBtreePager'{$ENDIF};
function lite_sqlite3PagerGetCodec(OQCOOQCOQ0:OOOQOCCOQ0):IntPtr;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3PagerGetCodec'{$ENDIF};
procedure lite_sqlite3PagerPagecount(OCCOOQCOQ0:OOOQOCCOQ0;O00Q0QCOQ0:PInteger);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3PagerPagecount'{$ENDIF};
function lite_sqlite3PagerIsMjPgno(OO0Q0QCOQ0:OOOQOCCOQ0;OQ0Q0QCOQ0:Cardinal):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3PagerIsMjPgno'{$ENDIF};
function lite_sqlite3PagerGet(OC0Q0QCOQ0:OOOQOCCOQ0;O0OQ0QCOQ0:Cardinal;OOOQ0QCOQ0:IntPtr;OQOQ0QCOQ0:integer):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3PagerGet'{$ENDIF};
function lite_sqlite3PagerWrite(OCOQ0QCOQ0:IntPtr):integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3PagerWrite'{$ENDIF};
procedure lite_sqlite3PagerUnref(O0QQ0QCOQ0:IntPtr);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'lite_sqlite3PagerUnref'{$ENDIF};
{$ENDIF DEF CODEC}
{$IFDEF CUSTOM_VFS}
function sqlite3_initialize():integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF};
function sqlite3_shutdown():integer;cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF};
procedure sqlite3_randomness(O0CQQQCOQ0:integer;OOCQQQCOQ0:pointer);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF};
{$IFNDEF WIN_VFS}
procedure sqlite3InitVfsFunctions(OQCQQQCOQ0:psqlite3_vfs);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF};
{$ENDIF}
{$IFDEF SET_CODEC}
procedure sqlite3InitCodecFunctions(OCCQQQCOQ0:psqlite3_codec);cdecl;external{$IFDEF NAMED_EXTERNAL} sqlite3o{$ENDIF};
{$ENDIF DEF SET_CODEC}
{$ENDIF DEF CUSTOM_VFS}
{$IFNDEF FPC}
{$IFNDEF MOBILE_VFS}
{$IFNDEF LINUX_VFS}
{$IFNDEF MACOS64}
function _ftol:Int64;cdecl;
function _ftoul:UInt64;cdecl;
{$ENDIF NDEF MACOS64}
{$ENDIF NDEF LINUX_VFS}
{$ENDIF NDEF MOBILE_VFS}
{$ENDIF NDEF FPC}
function strrchr(const OOQQ0QCOQ0:IntPtr;OQQQ0QCOQ0:integer):pointer;cdecl;
function strcspn(const OQCQ0QCOQ0,OCCQ0QCOQ0:IntPtr):integer;cdecl;
{$IFNDEF WIN64}
function malloc(OQ0C0QCOQ0:OC0QOCCOQ0):pointer;cdecl;
function realloc(OC0C0QCOQ0:pointer;O0OC0QCOQ0:OC0QOCCOQ0):pointer;cdecl;
procedure memcpy(OOOC0QCOQ0:pointer;const OQOC0QCOQ0:pointer;OCOC0QCOQ0:OC0QOCCOQ0);cdecl;
{$IFNDEF MOBILE_VFS}
procedure free(O0QC0QCOQ0:pointer);cdecl;
function memset(OOQC0QCOQ0:pointer;OQQC0QCOQ0:integer;OCQC0QCOQ0:OC0QOCCOQ0):pointer;cdecl;
procedure memmove(O0CC0QCOQ0:pointer;const OOCC0QCOQ0:pointer;OQCC0QCOQ0:OC0QOCCOQ0);cdecl;
function memcmp(OCCC0QCOQ0,O0000QCOQ0:pointer;OO000QCOQ0:OC0QOCCOQ0):integer;cdecl;
function strlen(OQ000QCOQ0:pointer):integer;cdecl;
{$IFNDEF LINUX_VFS}
{$IFNDEF FPC}
{$IFNDEF MACOS64}
procedure _lldiv;
procedure _lludiv;
procedure _llmod;
procedure _llmul;
procedure _llumod;
procedure _llshl;
procedure _llshr;
procedure _llushr;
{$ENDIF NDEF MACOS64}
{$ENDIF NDEF FPC}
{$ENDIF NDEF LINUX_VFS}
{$ENDIF NDEF MOBILE_VFS}
{$ENDIF NDEF WIN64}
{$IFDEF FPC}
{$IFDEF WIN64}
function malloc(OQ0C0QCOQ0:size_t):pointer;cdecl;
function realloc(OC0C0QCOQ0:pointer;O0OC0QCOQ0:size_t):pointer;cdecl;
procedure memcpy(OOOC0QCOQ0:pointer;const OQOC0QCOQ0:pointer;OCOC0QCOQ0:size_t);cdecl;
{$ENDIF}
{$ENDIF}
function strncmp(OC000QCOQ0,O0O00QCOQ0:PByte;OOO00QCOQ0:OC0QOCCOQ0):integer;cdecl;
function strcmp(OQO00QCOQ0,OCO00QCOQ0:PByte):integer;cdecl;
{$IFNDEF MOBILE_VFS}
function localtime(O0Q00QCOQ0:pointer):ptm;
{$IFDEF WIN64}
function rand_s(O0OQQQCOQ0:pointer):O0OQOCCOQ0;
{$ENDIF DEF WIN64}
{$IFNDEF NOIMPLEMENT}
procedure qsort(OOQ00QCOQ0:PByte;OQQ00QCOQ0,OCQ00QCOQ0:OC0QOCCOQ0;O0C00QCOQ0:OCC0OCCOQ0);cdecl;
{$ENDIF}
{$ENDIF NDEF MOBILE_VFS}
{$IFDEF WIN64}
procedure __chkstk;cdecl;
procedure __faststorefence;cdecl;
{$IFNDEF VER18P}
procedure __imp_InitializeCriticalSection(var OOOQQQCOQ0:TRTLCriticalSection);cdecl;
procedure __imp_DeleteCriticalSection(var OQOQQQCOQ0:TRTLCriticalSection);cdecl;
procedure __imp_EnterCriticalSection(var OCOQQQCOQ0:TRTLCriticalSection);cdecl;
procedure __imp_LeaveCriticalSection(var O0QQQQCOQ0:TRTLCriticalSection);cdecl;
function __imp_CloseHandle(OOQQQQCOQ0:THandle):BOOL;cdecl;
function __imp_GetCurrentThreadId:DWORD;cdecl;
{$ENDIF NDEF VER18P}
{$ENDIF DEF WIN64}
var
__turbofloat:word;
{$IFDEF CODEC}
{$IFNDEF NOCODECSYNC}
OOC00QCOQ0:TCriticalSection;
{$ENDIF}
{$ENDIF DEF CODEC}
{$IFDEF CUSTOM_VFS}
O00CQQCOQ0:sqlite3_vfs;
OO0CQQCOQ0:sqlite3_io_methods;
{$IFDEF SET_CODEC}
OQ0CQQCOQ0:sqlite3_codec;
{$ENDIF DEF SET_CODEC}
function OC0CQQCOQ0:string;
{$ENDIF DEF CUSTOM_VFS}
{$IFDEF CODEC_CALLBACK}
var
sqlite3CodecCallbackRec:sqlite3_codec;
procedure sqlite3InitCodecCallback(structCodec:pointer);cdecl;external {$IFDEF NAMED_EXTERNAL}sqlite3o name {$IFNDEF LINUX_VFS}PU+{$ENDIF}'sqlite3InitCodecCallback'{$ENDIF}{$IFDEF LITEDLL} sqlite3o name PU+'sqlite3InitCodecCallback'{$ENDIF};
{$ENDIF}
{$ENDIF}
implementation
{$IFNDEF NOSTATIC}
{$IFDEF CUSTOM_VFS}
uses
System.RTLConsts
{$IFNDEF WIN_VFS}
,Posix.SysTime
,Posix.Dlfcn
{$ENDIF NDEF WIN_VFS}
;
{$IFNDEF WIN_VFS}
const
{$IFDEF ANDROID_VFS}
O0OCQQCOQ0='/system/lib/libc.so';
OOOCQQCOQ0='';
{$ELSE}
{$IFDEF LINUX_VFS}
O0OCQQCOQ0='libc.so.6';
{$ELSE}
O0OCQQCOQ0='/usr/lib/libc.dylib';
{$IFNDEF IOS_VFS}
OOOCQQCOQ0='$INODE64';
{$ELSE}
OOOCQQCOQ0='';
{$ENDIF}
{$ENDIF}
{$ENDIF DEF ANDROID_VFS}
OQOCQQCOQ0=$0000F000;
OCOCQQCOQ0=$0000A000;
O0QCQQCOQ0=$00004000;
OOQCQQCOQ0=$00008000;
OQQCQQCOQ0=$0;
OCQCQQCOQ0=$2;
{$IFDEF ANDROID_VFS}
O0CCQQCOQ0=$40;
OOCCQQCOQ0=$200;
OQCCQQCOQ0=$80;
{$ELSE}
{$IFDEF LINUX_VFS}
O0CCQQCOQ0=$40;
OOCCQQCOQ0=$200;
OQCCQQCOQ0=$80;
{$ELSE}
O0CCQQCOQ0=$200;
OOCCQQCOQ0=$400;
OQCCQQCOQ0=$800;
{$ENDIF DEF LINUX_VFS}
{$ENDIF DEF ANDROID_VFS}
{$IFDEF ANDROID_VFS}
OCCCQQCOQ0=$0100;
O000QQCOQ0=$0080;
OO00QQCOQ0=$0020;
OQ00QQCOQ0=$0010;
OC00QQCOQ0=$0004;
O0O0QQCOQ0=$0002;
{$ELSE}
OCCCQQCOQ0=$00000100;
O000QQCOQ0=$00000080;
OO00QQCOQ0=$00000020;
OQ00QQCOQ0=$00000010;
OC00QQCOQ0=$00000004;
O0O0QQCOQ0=$00000002;
{$ENDIF DEF ANDROID_VFS}
const
OOO0QQCOQ0=OCCCQQCOQ0 or O000QQCOQ0 or OO00QQCOQ0 or OQ00QQCOQ0 or OC00QQCOQ0 or O0O0QQCOQ0;
OQO0QQCOQ0=13;
OCO0QQCOQ0=16;
O0Q0QQCOQ0=1;
OOQ0QQCOQ0=2;
type
OQQ0QQCOQ0=UInt32;
OCQ0QQCOQ0=UInt32;
{$IFDEF ANDROID_VFS}
O0C0QQCOQ0=UInt32;
OOC0QQCOQ0=Word;
OQC0QQCOQ0=Word;
OCC0QQCOQ0=Longint;
O00OQQCOQ0=UInt32;
OO0OQQCOQ0=UInt32;
{$IFDEF CPU64BITS}
ssize_t=Int64;
{$ELSE}
OQ0OQQCOQ0=Integer;
{$ENDIF}
{$ELSE}
O0C0QQCOQ0=Int32;
OOC0QQCOQ0=UInt16;
OQC0QQCOQ0=Uint16;
OCC0QQCOQ0=Int64;
O00OQQCOQ0=Int64;
OO0OQQCOQ0=Int32;
OQ0OQQCOQ0=LongInt;
{$ENDIF DEF ANDROID_VFS}
OC0OQQCOQ0=record
O0OOQQCOQ0:O0C0QQCOQ0;
OOOOQQCOQ0:OOC0QQCOQ0;
OQOOQQCOQ0:OQC0QQCOQ0;
OCOOQQCOQ0:UInt64;
O0QOQQCOQ0:OQQ0QQCOQ0;
OOQOQQCOQ0:OCQ0QQCOQ0;
OQQOQQCOQ0:O0C0QQCOQ0;
OCQOQQCOQ0:time_t;
O0COQQCOQ0:LongInt;
OOCOQQCOQ0:time_t;
OQCOQQCOQ0:LongInt;
OCCOQQCOQ0:time_t;
O00QOOCOQ0:LongInt;
OO0QOOCOQ0:time_t;
OQ0QOOCOQ0:LongInt;
OC0QOOCOQ0:OCC0QQCOQ0;
O0OQOOCOQ0:O00OQQCOQ0;
OOOQOOCOQ0:OO0OQQCOQ0;
OQOQOOCOQ0:UInt32;
OCOQOOCOQ0:UInt32;
O0QQOOCOQ0:Int32;
OOQQOOCOQ0:Int64;
OQQQOOCOQ0:Int64;
end;
{$IFDEF CUSTOM_FILESTREAM}
{$IFNDEF LINUX_VFS}
OCOQQOCOQ0=function(O0QQQOCOQ0:PAnsiChar;var OOQQQOCOQ0:OC0OQQCOQ0):Integer;cdecl;
{$ELSE}
OCOQQOCOQ0=function(O0OCO0COQ0:integer;O0QQQOCOQ0:PAnsiChar;var OOQQQOCOQ0:OC0OQQCOQ0):Integer;cdecl;
{$ENDIF}
OQQQQOCOQ0=function(OCQQQOCOQ0:PAnsiChar;O0CQQOCOQ0:Integer;OOCQQOCOQ0:Integer):Integer;cdecl;
OQCQQOCOQ0=function(OCCQQOCOQ0:Integer;O00CQOCOQ0:OCC0QQCOQ0;OO0CQOCOQ0:Integer):off_t;cdecl;
OQ0CQOCOQ0=function(OC0CQOCOQ0:Integer;O0OCQOCOQ0:Pointer;OOOCQOCOQ0:OC0QOCCOQ0):ssize_t;cdecl;
OQOCQOCOQ0=function(OCOCQOCOQ0:Integer;O0QCQOCOQ0:Pointer;OOQCQOCOQ0:OC0QOCCOQ0):ssize_t;cdecl;
OQQCQOCOQ0=function(OCQCQOCOQ0:PAnsiChar):Integer;cdecl;
O0CCQOCOQ0=function(OOCCQOCOQ0:integer):Integer;cdecl;
OQCCQOCOQ0=function(OCCCQOCOQ0:integer;O000QOCOQ0:OCC0QQCOQ0):integer;cdecl;
{$ENDIF}
{$IFDEF CUSTOM_FILESTREAM}
var
OO00QOCOQ0:NativeUInt;
{$IFNDEF LINUX_VFS}
OQ00QOCOQ0:OCOQQOCOQ0;
OC00QOCOQ0:OCOQQOCOQ0;
{$ENDIF}
O0O0QOCOQ0:OQQQQOCOQ0;
OOO0QOCOQ0:OQCQQOCOQ0;
OQO0QOCOQ0:OQ0CQOCOQ0;
OCO0QOCOQ0:OQOCQOCOQ0;
O0Q0QOCOQ0:OQQCQOCOQ0;
OOQ0QOCOQ0:O0CCQOCOQ0;
OQQ0QOCOQ0:OQCCQOCOQ0;
{$ENDIF}
const
OCQQOOCOQ0=$80000000;
O0CQOOCOQ0=$40000000;
OOCQOOCOQ0=1;
OQCQOOCOQ0=3;
OCCQOOCOQ0=4;
{$ENDIF NDEF WIN_VFS}
type
O00COOCOQ0=class(TFileStream)
{$IFDEF CUSTOM_FILESTREAM}
protected
procedure SetSize(const O0C0QOCOQ0:Int64);override;
public
constructor Create(const OQC0QOCOQ0:string;OCC0QOCOQ0,O00OQOCOQ0:LongWord);
destructor Destroy;override;
function Seek(const OC0OQOCOQ0:Int64;O0OOQOCOQ0:TSeekOrigin):Int64;override;
function Read(var OQOOQOCOQ0;OCOOQOCOQ0:Longint):Longint;override;
function Write(const OOQOQOCOQ0;OQQOQOCOQ0:Longint):Longint;override;
{$ENDIF}
end;
{$ENDIF DEF CUSTOM_VFS}
function OC0OOCCOQ0:integer;
begin
raise Exception.Create('SQLite database encryption and decryption are not supported with TLiteConnection.Options.Direct = True');
{$IFDEF FPC}
Result:=0;
{$ENDIF}
end;
procedure DoInitStaticFunction(const OQ0OOCCOQ0:TSQLite3API);
begin
OQ0OOCCOQ0.sqlite3_malloc:=sqlite3_malloc;
OQ0OOCCOQ0.sqlite3_open:=sqlite3_open;
OQ0OOCCOQ0.sqlite3_open16:=sqlite3_open16;
OQ0OOCCOQ0.sqlite3_open_v2:=sqlite3_open_v2;
OQ0OOCCOQ0.sqlite3_close:=sqlite3_close;
OQ0OOCCOQ0.sqlite3_errcode:=sqlite3_errcode;
OQ0OOCCOQ0.sqlite3_extended_errcode:=sqlite3_extended_errcode;
OQ0OOCCOQ0.sqlite3_extended_result_codes:=sqlite3_extended_result_codes;
OQ0OOCCOQ0.sqlite3_errmsg:=sqlite3_errmsg;
OQ0OOCCOQ0.sqlite3_last_insert_rowid:=sqlite3_last_insert_rowid;
OQ0OOCCOQ0.sqlite3_changes:=sqlite3_changes;
OQ0OOCCOQ0.sqlite3_prepare_v2:=sqlite3_prepare_v2;
OQ0OOCCOQ0.sqlite3_step:=sqlite3_step;
OQ0OOCCOQ0.sqlite3_reset:=sqlite3_reset;
OQ0OOCCOQ0.sqlite3_finalize:=sqlite3_finalize;
OQ0OOCCOQ0.sqlite3_column_count:=sqlite3_column_count;
OQ0OOCCOQ0.sqlite3_column_type:=sqlite3_column_type;
OQ0OOCCOQ0.sqlite3_column_name:=sqlite3_column_name;
OQ0OOCCOQ0.sqlite3_column_origin_name:=sqlite3_column_origin_name;
OQ0OOCCOQ0.sqlite3_column_table_name:=sqlite3_column_table_name;
OQ0OOCCOQ0.sqlite3_column_database_name:=sqlite3_column_database_name;
OQ0OOCCOQ0.sqlite3_column_decltype:=sqlite3_column_decltype;
OQ0OOCCOQ0.sqlite3_table_column_metadata:=sqlite3_table_column_metadata;
OQ0OOCCOQ0.sqlite3_column_blob:=sqlite3_column_blob;
OQ0OOCCOQ0.sqlite3_column_bytes:=sqlite3_column_bytes;
OQ0OOCCOQ0.sqlite3_column_double:=sqlite3_column_double;
OQ0OOCCOQ0.sqlite3_column_int:=sqlite3_column_int;
OQ0OOCCOQ0.sqlite3_column_int64:=sqlite3_column_int64;
OQ0OOCCOQ0.sqlite3_column_text:=sqlite3_column_text;
OQ0OOCCOQ0.sqlite3_bind_parameter_count:=sqlite3_bind_parameter_count;
OQ0OOCCOQ0.sqlite3_bind_parameter_name:=sqlite3_bind_parameter_name;
OQ0OOCCOQ0.sqlite3_bind_blob:=sqlite3_bind_blob;
OQ0OOCCOQ0.sqlite3_bind_zeroblob:=sqlite3_bind_zeroblob;
OQ0OOCCOQ0.sqlite3_bind_double:=sqlite3_bind_double;
OQ0OOCCOQ0.sqlite3_bind_int:=sqlite3_bind_int;
OQ0OOCCOQ0.sqlite3_bind_int64:=sqlite3_bind_int64;
OQ0OOCCOQ0.sqlite3_bind_null:=sqlite3_bind_null;
OQ0OOCCOQ0.sqlite3_bind_text:=sqlite3_bind_text;
OQ0OOCCOQ0.sqlite3_result_blob:=sqlite3_result_blob;
OQ0OOCCOQ0.sqlite3_result_double:=sqlite3_result_double;
OQ0OOCCOQ0.sqlite3_result_error:=sqlite3_result_error;
OQ0OOCCOQ0.sqlite3_result_error16:=sqlite3_result_error16;
OQ0OOCCOQ0.sqlite3_result_error_toobig:=sqlite3_result_error_toobig;
OQ0OOCCOQ0.sqlite3_result_error_nomem:=sqlite3_result_error_nomem;
OQ0OOCCOQ0.sqlite3_result_error_code:=sqlite3_result_error_code;
OQ0OOCCOQ0.sqlite3_result_int:=sqlite3_result_int;
OQ0OOCCOQ0.sqlite3_result_int64:=sqlite3_result_int64;
OQ0OOCCOQ0.sqlite3_result_null:=sqlite3_result_null;
OQ0OOCCOQ0.sqlite3_result_text:=sqlite3_result_text;
OQ0OOCCOQ0.sqlite3_result_text16:=sqlite3_result_text16;
OQ0OOCCOQ0.sqlite3_result_text16le:=sqlite3_result_text16le;
OQ0OOCCOQ0.sqlite3_result_text16be:=sqlite3_result_text16be;
OQ0OOCCOQ0.sqlite3_result_value:=sqlite3_result_value;
OQ0OOCCOQ0.sqlite3_result_zeroblob:=sqlite3_result_zeroblob;
OQ0OOCCOQ0.sqlite3_value_blob:=sqlite3_value_blob;
OQ0OOCCOQ0.sqlite3_value_bytes:=sqlite3_value_bytes;
OQ0OOCCOQ0.sqlite3_value_bytes16:=sqlite3_value_bytes16;
OQ0OOCCOQ0.sqlite3_value_double:=sqlite3_value_double;
OQ0OOCCOQ0.sqlite3_value_int:=sqlite3_value_int;
OQ0OOCCOQ0.sqlite3_value_int64:=sqlite3_value_int64;
OQ0OOCCOQ0.sqlite3_value_text:=sqlite3_value_text;
OQ0OOCCOQ0.sqlite3_value_text16:=sqlite3_value_text16;
OQ0OOCCOQ0.sqlite3_value_type:=sqlite3_value_type;
OQ0OOCCOQ0.sqlite3_user_data:=sqlite3_user_data;
OQ0OOCCOQ0.sqlite3_libversion:=sqlite3_libversion;
OQ0OOCCOQ0.sqlite3_libversion_number:=sqlite3_libversion_number;
OQ0OOCCOQ0.sqlite3_create_collation:=sqlite3_create_collation;
OQ0OOCCOQ0.sqlite3_create_collation16:=sqlite3_create_collation16;
OQ0OOCCOQ0.sqlite3_create_function:=sqlite3_create_function;
OQ0OOCCOQ0.sqlite3_create_function16:=sqlite3_create_function16;
OQ0OOCCOQ0.sqlite3_overload_function:=sqlite3_overload_function;
OQ0OOCCOQ0.sqlite3_create_module:=sqlite3_create_module;
OQ0OOCCOQ0.sqlite3_create_module_v2:=sqlite3_create_module_v2;
OQ0OOCCOQ0.sqlite3_declare_vtab:=sqlite3_declare_vtab;
OQ0OOCCOQ0.sqlite3_vtab_config:=sqlite3_vtab_config;
OQ0OOCCOQ0.sqlite3_enable_shared_cache:=sqlite3_enable_shared_cache;
OQ0OOCCOQ0.sqlite3_enable_load_extension:=sqlite3_enable_load_extension;
OQ0OOCCOQ0.sqlite3_busy_timeout:=sqlite3_busy_timeout;
OQ0OOCCOQ0.sqlite3_busy_handler:=sqlite3_busy_handler;
{$IFDEF CODEC}
OQ0OOCCOQ0.sqlite3_key:=sqlite3_key;
OQ0OOCCOQ0.sqlite3_key_v2:=sqlite3_key_v2;
OQ0OOCCOQ0.sqlite3_rekey:=sqlite3_rekey;
OQ0OOCCOQ0.sqlite3_rekey_v2:=sqlite3_rekey_v2;
{$ENDIF}
OQ0OOCCOQ0.sqlite3_backup_init:=sqlite3_backup_init;
OQ0OOCCOQ0.sqlite3_backup_step:=sqlite3_backup_step;
OQ0OOCCOQ0.sqlite3_backup_finish:=sqlite3_backup_finish;
OQ0OOCCOQ0.sqlite3_backup_remaining:=sqlite3_backup_remaining;
OQ0OOCCOQ0.sqlite3_backup_pagecount:=sqlite3_backup_pagecount;
OQ0OOCCOQ0.sqlite3_db_release_memory:=sqlite3_db_release_memory;
OQ0OOCCOQ0.sqlite3_db_readonly:=sqlite3_db_readonly;
OQ0OOCCOQ0.sqlite3_limit:=sqlite3_limit;
OQ0OOCCOQ0.sqlite3_uri_parameter:=sqlite3_uri_parameter;
OQ0OOCCOQ0.sqlite3_interrupt:=sqlite3_interrupt;
end;
{$IFDEF CODEC}
constructor OC0COCCOQ0.Create(OC00OCCOQ0,O0O0OCCOQ0:AnsiString;OOO0OCCOQ0:TCREncryptionAlgorithm);
begin
inherited Create;
OCQCOCCOQ0:=OOO0OCCOQ0;
O0Q0OCCOQ0:=OC00OCCOQ0;
OOQ0OCCOQ0:=O0O0OCCOQ0;
end;
destructor OC0COCCOQ0.Destroy;
begin
if Assigned(OCOCOCCOQ0)then
FreeAndNil(OCOCOCCOQ0);
if Assigned(O0QCOCCOQ0)then
FreeAndNil(O0QCOCCOQ0);
if O0OCOCCOQ0>0 then begin
FreeMem(OOOCOCCOQ0,O0OCOCCOQ0);
FreeMem(OQOCOCCOQ0,O0OCOCCOQ0);
end;
inherited;
end;
procedure OC0COCCOQ0.O0CCOCCOQ0(OOCCOCCOQ0:integer);
begin
if OOCCOCCOQ0<>O0OCOCCOQ0 then begin
O0OCOCCOQ0:=OOCCOCCOQ0;
if OOOCOCCOQ0<>nil then
OOOCOCCOQ0:=realloc(OOOCOCCOQ0,O0OCOCCOQ0)
else
OOOCOCCOQ0:=malloc(O0OCOCCOQ0);
if OQOCOCCOQ0<>nil then
OQOCOCCOQ0:=realloc(OQOCOCCOQ0,O0OCOCCOQ0)
else
OQOCOCCOQ0:=malloc(O0OCOCCOQ0);
end;
end;
procedure OC0COCCOQ0.OQCCOCCOQ0(OCCCOCCOQ0:AnsiString);
begin
if not Assigned(OCOCOCCOQ0)then begin
OCOCOCCOQ0:=TCREncryptor.Create(nil);
OCOCOCCOQ0.DataHeader:=ehNone;
OCOCOCCOQ0.EncryptionAlgorithm:=OCQCOCCOQ0;
end;
OOQCOCCOQ0:=OCCCOCCOQ0;
end;
procedure OC0COCCOQ0.O000OCCOQ0(OO00OCCOQ0:AnsiString);
begin
if not Assigned(O0QCOCCOQ0)then begin
O0QCOCCOQ0:=TCREncryptor.Create(nil);
O0QCOCCOQ0.DataHeader:=ehNone;
O0QCOCCOQ0.EncryptionAlgorithm:=OCQCOCCOQ0;
end;
OQQCOCCOQ0:=OO00OCCOQ0;
end;
function OQC00QCOQ0(OCC00QCOQ0:pSQLite3;O00O0QCOQ0:integer):OOQQOCCOQ0;
begin
Result:=lite_sqlite3GetBackend(OCC00QCOQ0,O00O0QCOQ0);
end;
function OO0O0QCOQ0(OQ0O0QCOQ0:pSQLite3;OC0O0QCOQ0:integer):OOOQOCCOQ0;
var
O0OO0QCOQ0:OOQQOCCOQ0;
begin
Result:=nil;
O0OO0QCOQ0:=OQC00QCOQ0(OQ0O0QCOQ0,OC0O0QCOQ0);
if O0OO0QCOQ0<>nil then
Result:=lite_sqlite3GetBtreePager(O0OO0QCOQ0^.O0CQOCCOQ0);
end;
function OOOO0QCOQ0(OQOO0QCOQ0:pSQLite3;OCOO0QCOQ0:integer):IntPtr;
var
O0QO0QCOQ0:OOOQOCCOQ0;
begin
Result:=nil;
O0QO0QCOQ0:=OO0O0QCOQ0(OQOO0QCOQ0,OCOO0QCOQ0);
if O0QO0QCOQ0<>nil then
Result:=lite_sqlite3PagerGetCodec(O0QO0QCOQ0);
end;
function OOQO0QCOQ0(OQQO0QCOQ0:pSQLite3;OCQO0QCOQ0:integer;O0CO0QCOQ0:PAnsiChar;OOCO0QCOQ0:integer):integer;
var
OQCO0QCOQ0:IntPtr;
OCCO0QCOQ0:TLiteEncryptionAlgorithm;
begin
OCCO0QCOQ0:=leDefault;
{$IFDEF LITEDLL}
{$IFDEF FIXED_ENCRYPTION}
lAlgorithm:=leAES256;
{$ENDIF}
{$ENDIF}
OQCO0QCOQ0:=OOOO0QCOQ0(OQQO0QCOQ0,OCQO0QCOQ0);
if OQCO0QCOQ0<>nil then
OCCO0QCOQ0:=TLiteEncryptionAlgorithm(OC0COCCOQ0(OQCO0QCOQ0).OCQCOCCOQ0);
Sqlite3CodecSetKey(OQQO0QCOQ0,OCQO0QCOQ0,O0CO0QCOQ0,True,OCCO0QCOQ0);
Sqlite3CodecSetKey(OQQO0QCOQ0,OCQO0QCOQ0,O0CO0QCOQ0,False,OCCO0QCOQ0);
Result:=SQLITE_OK;
end;
function O00QCQCOQ0(OO0QCQCOQ0:pSQLite3;OQ0QCQCOQ0:integer;OC0QCQCOQ0:PAnsiChar;O0OQCQCOQ0:Integer):integer;
var
OOOQCQCOQ0:OOQQOCCOQ0;
OQOQCQCOQ0:OOOQOCCOQ0;
OCOQCQCOQ0:IntPtr;
O0QQCQCOQ0:integer;
OOQQCQCOQ0:integer;
OQQQCQCOQ0:integer;
OCQQCQCOQ0:IntPtr;
begin
Result:=SQLITE_ERROR;
{$IFNDEF NOCODECSYNC}
OOC00QCOQ0.Acquire;
try
{$ENDIF}
OOOQCQCOQ0:=OQC00QCOQ0(OO0QCQCOQ0,OQ0QCQCOQ0);
if OOOQCQCOQ0<>nil then begin
OQOQCQCOQ0:=OO0O0QCOQ0(OO0QCQCOQ0,OQ0QCQCOQ0);
if OQOQCQCOQ0<>nil then begin
OCOQCQCOQ0:=OOOO0QCOQ0(OO0QCQCOQ0,OQ0QCQCOQ0);
if OCOQCQCOQ0=nil then begin
sqlite3CodecAttach(OO0QCQCOQ0,OQ0QCQCOQ0,nil,0);
OCOQCQCOQ0:=OOOO0QCOQ0(OO0QCQCOQ0,OQ0QCQCOQ0);
end;
if OCOQCQCOQ0=nil then
Exit;
OC0COCCOQ0(OCOQCQCOQ0).OOQ0OCCOQ0:=AnsiString(OC0QCQCOQ0);
O0QQCQCOQ0:=lite_sqlite3BtreeBeginTrans(OOOQCQCOQ0^.O0CQOCCOQ0,1,nil);
if O0QQCQCOQ0=SQLITE_OK then begin
lite_sqlite3PagerPagecount(OQOQCQCOQ0,@OOQQCQCOQ0);
OQQQCQCOQ0:=1;
while(O0QQCQCOQ0=SQLITE_OK)and(OQQQCQCOQ0<=OOQQCQCOQ0)do begin
if lite_sqlite3PagerIsMjPgno(OQOQCQCOQ0,OQQQCQCOQ0)=0 then begin
O0QQCQCOQ0:=lite_sqlite3PagerGet(OQOQCQCOQ0,OQQQCQCOQ0,@OCQQCQCOQ0,0);
if O0QQCQCOQ0=SQLITE_OK then begin
O0QQCQCOQ0:=lite_sqlite3PagerWrite(OCQQCQCOQ0);
if O0QQCQCOQ0=SQLITE_OK then
lite_sqlite3PagerUnref(OCQQCQCOQ0);
end;
end;
Inc(OQQQCQCOQ0);
end;
if O0QQCQCOQ0=SQLITE_OK then begin
O0QQCQCOQ0:=lite_sqlite3BtreeCommit(OOOQCQCOQ0^.O0CQOCCOQ0);
if O0QQCQCOQ0=SQLITE_OK then begin
OC0COCCOQ0(OCOQCQCOQ0).O0Q0OCCOQ0:=AnsiString(OC0QCQCOQ0);
if OC0QCQCOQ0='' then
lite_sqlite3PagerSetCodec(OQOQCQCOQ0,nil,nil,nil,nil);
end;
end
else
lite_sqlite3BtreeRollback(OOOQCQCOQ0^.O0CQOCCOQ0,SQLITE_ERROR);
Result:=O0QQCQCOQ0;
end;
end;
end;
{$IFNDEF NOCODECSYNC}
finally
OOC00QCOQ0.Release;
end;
{$ENDIF}
end;
function sqlite3_key(OQ0QOQCOQ0:pSQLite3;OC0QOQCOQ0:PAnsiChar;O0OQOQCOQ0:Integer):integer;
begin
Result:=OOQO0QCOQ0(OQ0QOQCOQ0,0,OC0QOQCOQ0,O0OQOQCOQ0);
end;
function sqlite3_key_v2(OOOQOQCOQ0:pSQLite3;OQOQOQCOQ0,OCOQOQCOQ0:PAnsiChar;O0QQOQCOQ0:Integer):integer;
var
OOQQOQCOQ0,OQQQOQCOQ0:integer;
begin
OOQQOQCOQ0:=-1;
if(OQOQOQCOQ0='main')or(OQOQOQCOQ0='')or(OQOQOQCOQ0=nil)then
OOQQOQCOQ0:=0
else
for OQQQOQCOQ0:=0 to TSQLite3(OOOQOQCOQ0^).nDb-1 do
if PAnsiChar(pSqlite3Db(PtrOffset(TSQLite3(OOOQOQCOQ0^).aDb,OQQQOQCOQ0*SizeOf(TSqlite3Db)))^.zName)=OQOQOQCOQ0 then begin
OOQQOQCOQ0:=OQQQOQCOQ0;
Break;
end;
if OOQQOQCOQ0>=0 then
Result:=OOQO0QCOQ0(OOOQOQCOQ0,OOQQOQCOQ0,OCOQOQCOQ0,O0QQOQCOQ0)
else
Result:=SQLITE_ERROR;
end;
function sqlite3_rekey(OCQQOQCOQ0:pSQLite3;O0CQOQCOQ0:PAnsiChar;OOCQOQCOQ0:Integer):Integer;
begin
Result:=O00QCQCOQ0(OCQQOQCOQ0,0,O0CQOQCOQ0,OOCQOQCOQ0);
end;
function sqlite3_rekey_v2(OQCQOQCOQ0:pSQLite3;OCCQOQCOQ0,O00COQCOQ0:PAnsiChar;OO0COQCOQ0:Integer):Integer;
var
OQ0COQCOQ0,OC0COQCOQ0:integer;
begin
OQ0COQCOQ0:=-1;
if(OCCQOQCOQ0='main')or(OCCQOQCOQ0='')or(OCCQOQCOQ0=nil)then
OQ0COQCOQ0:=0
else
for OC0COQCOQ0:=0 to TSQLite3(OQCQOQCOQ0^).nDb-1 do
if PAnsiChar(pSqlite3Db(PtrOffset(TSQLite3(OQCQOQCOQ0^).aDb,OC0COQCOQ0*SizeOf(TSqlite3Db)))^.zName)=OCCQOQCOQ0 then begin
OQ0COQCOQ0:=OC0COQCOQ0;
Break;
end;
if OQ0COQCOQ0>=0 then
Result:=O00QCQCOQ0(OQCQOQCOQ0,OQ0COQCOQ0,O00COQCOQ0,OO0COQCOQ0)
else
Result:=SQLITE_ERROR;
end;
procedure sqlite3_activate_see;
begin
end;
function sqlite3CustomPragma(OC00OQCOQ0:pSQLite3;O0O0OQCOQ0:Integer;OOO0OQCOQ0:PAnsiChar;OQO0OQCOQ0:PAnsiChar):integer;
var
OCO0OQCOQ0:string;
O0Q0OQCOQ0:TLiteEncryptionAlgorithm;
begin
if UpperCase(string(OOO0OQCOQ0))='ENCRYPTION' then begin
OCO0OQCOQ0:=UpperCase(string(OQO0OQCOQ0));
if OCO0OQCOQ0='TRIPLEDES' then
O0Q0OQCOQ0:=leTripleDES
else if OCO0OQCOQ0='BLOWFISH' then
O0Q0OQCOQ0:=leBlowfish
else if OCO0OQCOQ0='AES128' then
O0Q0OQCOQ0:=leAES128
else if OCO0OQCOQ0='AES192' then
O0Q0OQCOQ0:=leAES192
else if OCO0OQCOQ0='AES256' then
O0Q0OQCOQ0:=leAES256
else if OCO0OQCOQ0='CAST128' then
O0Q0OQCOQ0:=leCast128
else if OCO0OQCOQ0='RC4' then
O0Q0OQCOQ0:=leRC4
else
O0Q0OQCOQ0:=leDefault;
Sqlite3CodecSetKey(OC00OQCOQ0,O0O0OQCOQ0,'',True,O0Q0OQCOQ0);
Sqlite3CodecSetKey(OC00OQCOQ0,O0O0OQCOQ0,'',False,O0Q0OQCOQ0);
end;
Result:=SQLITE_OK;
end;
function O0CQCQCOQ0(const OOCQCQCOQ0:IntPtr;OQCQCQCOQ0:pointer;OCCQCQCOQ0:u32;O00CCQCOQ0:integer):pointer;cdecl;
var
OO0CCQCOQ0:cardinal;
OQ0CCQCOQ0:Integer;
OC0CCQCOQ0:TBytes;
O0OCCQCOQ0:TBytesEncryptionMethod;
OOOCCQCOQ0:TDecryptionMethod;
OQOCCQCOQ0:TCRDecryptedDataType;
begin
{$IFNDEF NOCODECSYNC}
OOC00QCOQ0.Acquire;
try
{$ENDIF}
OQ0CCQCOQ0:=0;
if OCCQCQCOQ0=1 then
OQ0CCQCOQ0:=OQ0QOCCOQ0;
OO0CCQCOQ0:=OC0COCCOQ0(OOCQCQCOQ0).OQC0OCCOQ0-OQ0CCQCOQ0;
case O00CCQCOQ0 of
3:begin
if Assigned(OC0COCCOQ0(OOCQCQCOQ0).OQQ0OCCOQ0)then begin
if string(OC0COCCOQ0(OOCQCQCOQ0).O0Q0OCCOQ0)<>'' then begin
memcpy(OC0COCCOQ0(OOCQCQCOQ0).O0C0OCCOQ0,OQCQCQCOQ0,OC0COCCOQ0(OOCQCQCOQ0).OQC0OCCOQ0);
if OC0COCCOQ0(OOCQCQCOQ0).OQQ0OCCOQ0.Password<>string(OC0COCCOQ0(OOCQCQCOQ0).O0Q0OCCOQ0)then
OC0COCCOQ0(OOCQCQCOQ0).OQQ0OCCOQ0.Password:=string(OC0COCCOQ0(OOCQCQCOQ0).O0Q0OCCOQ0);
OOOCCQCOQ0:=TCREncryptorUtils.Decryptor(OC0COCCOQ0(OOCQCQCOQ0).OQQ0OCCOQ0);
OOOCCQCOQ0(PtrOffset(OC0COCCOQ0(OOCQCQCOQ0).O0C0OCCOQ0,OQ0CCQCOQ0),dtBytes,OO0CCQCOQ0,OQOCCQCOQ0);
if OQOCCQCOQ0<>ddtError then begin
if(OCCQCQCOQ0=1)and(Cardinal(PtrOffset(OC0COCCOQ0(OOCQCQCOQ0).O0C0OCCOQ0,21)^)and$00FFFFFF<>$00202040)then begin
OQ0CCQCOQ0:=OO0QOCCOQ0;
OO0CCQCOQ0:=OC0COCCOQ0(OOCQCQCOQ0).OQC0OCCOQ0-OQ0CCQCOQ0;
memcpy(OC0COCCOQ0(OOCQCQCOQ0).O0C0OCCOQ0,OQCQCQCOQ0,OC0COCCOQ0(OOCQCQCOQ0).OQC0OCCOQ0);
OOOCCQCOQ0(PtrOffset(OC0COCCOQ0(OOCQCQCOQ0).O0C0OCCOQ0,OQ0CCQCOQ0),dtBytes,OO0CCQCOQ0,OQOCCQCOQ0);
if OQOCCQCOQ0=ddtError then begin
Result:=nil;
Exit;
end;
end;
memcpy(OQCQCQCOQ0,OC0COCCOQ0(OOCQCQCOQ0).O0C0OCCOQ0,OC0COCCOQ0(OOCQCQCOQ0).OQC0OCCOQ0);
Result:=OQCQCQCOQ0;
end
else
Result:=nil;
end
else
Result:=OQCQCQCOQ0;
end
else
Result:=OQCQCQCOQ0;
end;
6,7:begin
if Assigned(OC0COCCOQ0(OOCQCQCOQ0).OCQ0OCCOQ0)then begin
if string(OC0COCCOQ0(OOCQCQCOQ0).OOQ0OCCOQ0)<>'' then begin
memcpy(OC0COCCOQ0(OOCQCQCOQ0).OOC0OCCOQ0,OQCQCQCOQ0,OC0COCCOQ0(OOCQCQCOQ0).OQC0OCCOQ0);
if OC0COCCOQ0(OOCQCQCOQ0).OCQ0OCCOQ0.Password<>string(OC0COCCOQ0(OOCQCQCOQ0).OOQ0OCCOQ0)then
OC0COCCOQ0(OOCQCQCOQ0).OCQ0OCCOQ0.Password:=string(OC0COCCOQ0(OOCQCQCOQ0).OOQ0OCCOQ0);
O0OCCQCOQ0:=TCREncryptorUtils.BytesEncryptor(OC0COCCOQ0(OOCQCQCOQ0).OCQ0OCCOQ0);
OC0CCQCOQ0:=O0OCCQCOQ0(PtrOffset(OC0COCCOQ0(OOCQCQCOQ0).OOC0OCCOQ0,OQ0CCQCOQ0),OO0CCQCOQ0);
if OO0CCQCOQ0>0 then begin
memcpy(PtrOffset(OC0COCCOQ0(OOCQCQCOQ0).OOC0OCCOQ0,OQ0CCQCOQ0),pointer((@OC0CCQCOQ0)^),OO0CCQCOQ0);
Result:=OC0COCCOQ0(OOCQCQCOQ0).OOC0OCCOQ0;
end
else
Result:=nil;
end
else
Result:=OQCQCQCOQ0;
end
else
Result:=OQCQCQCOQ0;
end;
else
Result:=OQCQCQCOQ0;
end;
{$IFNDEF NOCODECSYNC}
finally
OOC00QCOQ0.Release;
end;
{$ENDIF}
end;
procedure OCOCCQCOQ0(O0QCCQCOQ0:IntPtr;OOQCCQCOQ0,OQQCCQCOQ0:Integer);cdecl;
begin
if O0QCCQCOQ0=nil then
Exit;
OC0COCCOQ0(O0QCCQCOQ0).OQC0OCCOQ0:=OOQCCQCOQ0;
end;
procedure OCQCCQCOQ0(O0CCCQCOQ0:IntPtr);cdecl;
begin
{$IFNDEF NEXTGEN}
if O0CCCQCOQ0<>nil then
OC0COCCOQ0(O0CCCQCOQ0).Free;
{$ENDIF}
end;
function sqlite3CodecAttach(O0OCOQCOQ0:pSQLite3;OOOCOQCOQ0:integer;OQOCOQCOQ0:PAnsiChar;OCOCOQCOQ0:Integer;O0QCOQCOQ0:TLiteEncryptionAlgorithm=DefaultEncryptionAlgorithm):integer;
var
OOQCOQCOQ0:OOOQOCCOQ0;
OQQCOQCOQ0:IntPtr;
OCQCOQCOQ0:TCREncryptionAlgorithm;
O0CCOQCOQ0:AnsiString;
begin
{$IFDEF LITEDLL}
{$IFDEF FIXED_ENCRYPTION}
Algorithm:=leAES256;
{$ENDIF}
{$ENDIF}
Result:=SQLITE_ERROR;
OCQCOQCOQ0:=TCREncryptionAlgorithm(O0QCOQCOQ0);
if OOOCOQCOQ0>1 then begin
OQQCOQCOQ0:=OOOO0QCOQ0(O0OCOQCOQ0,0);
if OQQCOQCOQ0<>nil then
OCQCOQCOQ0:=OC0COCCOQ0(OQQCOQCOQ0).OCQCOCCOQ0;
end;
if not(OCQCOQCOQ0 in[eaTripleDES..eaRC4])then begin
Result:=SQLITE_OK;
Exit;
end;
{$IFNDEF NOCODECSYNC}
OOC00QCOQ0.Acquire;
try
{$ENDIF}
OQQCOQCOQ0:=OOOO0QCOQ0(O0OCOQCOQ0,OOOCOQCOQ0);
if OQQCOQCOQ0<>nil then
Exit;
OOQCOQCOQ0:=OO0O0QCOQ0(O0OCOQCOQ0,OOOCOQCOQ0);
O0CCOQCOQ0:=AnsiString(CRFunctions.Utf8Decode(AnsiString(OQOCOQCOQ0)));
OQQCOQCOQ0:=OC0COCCOQ0.Create(O0CCOQCOQ0,O0CCOQCOQ0,OCQCOQCOQ0);
lite_sqlite3PagerSetCodec(OOQCOQCOQ0,@O0CQCQCOQ0,@OCOCCQCOQ0,@OCQCCQCOQ0,OQQCOQCOQ0);
Result:=SQLITE_OK;
{$IFNDEF NOCODECSYNC}
finally
OOC00QCOQ0.Release;
end;
{$ENDIF}
end;
procedure sqlite3CodecGetKey(OOCCOQCOQ0:pSQLite3;OQCCOQCOQ0:integer;var OCCCOQCOQ0:pointer;var O000OQCOQ0:{$IFDEF POSIX}pointer{$ELSE}pInt{$ENDIF});
var
OO00OQCOQ0:IntPtr;
begin
OCCCOQCOQ0:=nil;
O000OQCOQ0:=nil;
OO00OQCOQ0:=OOOO0QCOQ0(OOCCOQCOQ0,OQCCOQCOQ0);
if OO00OQCOQ0=nil then
Exit;
PAnsiChar(OCCCOQCOQ0):=PAnsiChar(OC0COCCOQ0(OO00OQCOQ0).O0Q0OCCOQ0);
end;
procedure Sqlite3CodecSetKey(OOQ0OQCOQ0:pSQLite3;OQQ0OQCOQ0:integer;OCQ0OQCOQ0:PAnsiChar;O0C0OQCOQ0:boolean;OOC0OQCOQ0:TLiteEncryptionAlgorithm=leDefault);
var
OQC0OQCOQ0:IntPtr;
begin
if OOC0OQCOQ0=leDefault then
Exit;
OQC0OQCOQ0:=OOOO0QCOQ0(OOQ0OQCOQ0,OQQ0OQCOQ0);
if OQC0OQCOQ0=nil then begin
sqlite3CodecAttach(OOQ0OQCOQ0,OQQ0OQCOQ0,'',0,OOC0OQCOQ0);
OQC0OQCOQ0:=OOOO0QCOQ0(OOQ0OQCOQ0,OQQ0OQCOQ0);
end;
if O0C0OQCOQ0 then
OC0COCCOQ0(OQC0OQCOQ0).O0Q0OCCOQ0:=AnsiString(OCQ0OQCOQ0)
else
OC0COCCOQ0(OQC0OQCOQ0).OOQ0OCCOQ0:=AnsiString(OCQ0OQCOQ0);
end;
{$ENDIF DEF CODEC}
{$IFNDEF FPC}
{$IFNDEF MOBILE_VFS}
{$IFNDEF LINUX_VFS}
{$IFNDEF MACOS64}
function _ftol:Int64;
asm
jmp System.@Trunc
end;
function _ftoul:UInt64;
asm
jmp System.@Trunc
end;
{$ENDIF NDEF MACOS64}
{$ENDIF NDEF LINUX_VFS}
{$ENDIF NDEF MOBILE_VFS}
{$ENDIF NDEF FPC}
function strrchr(const OOQQ0QCOQ0:IntPtr;OQQQ0QCOQ0:integer):IntPtr;
var
OCQQ0QCOQ0:integer;
O0CQ0QCOQ0:IntPtr;
OOCQ0QCOQ0:boolean;
begin
O0CQ0QCOQ0:=OOQQ0QCOQ0;
Result:=nil;
if OQQQ0QCOQ0>32767 then
OCQQ0QCOQ0:=4
else if OQQQ0QCOQ0>255 then
OCQQ0QCOQ0:=2
else
OCQQ0QCOQ0:=1;
while byte(O0CQ0QCOQ0^)<>0 do
O0CQ0QCOQ0:=PtrOffset(O0CQ0QCOQ0,OCQQ0QCOQ0);
if OQQQ0QCOQ0=0 then begin
Result:=O0CQ0QCOQ0;
Exit;
end;
repeat
if OCQQ0QCOQ0=1 then
OOCQ0QCOQ0:=byte(O0CQ0QCOQ0^)=byte(OQQQ0QCOQ0)
else if OCQQ0QCOQ0=2 then
OOCQ0QCOQ0:=Word(O0CQ0QCOQ0^)=word(OQQQ0QCOQ0)
else
OOCQ0QCOQ0:=integer(O0CQ0QCOQ0^)=OQQQ0QCOQ0;
if OOCQ0QCOQ0 then begin
Result:=O0CQ0QCOQ0;
Exit;
end;
O0CQ0QCOQ0:=PtrOffset(O0CQ0QCOQ0,-1*OCQQ0QCOQ0);
until O0CQ0QCOQ0=OOQQ0QCOQ0;
end;
function strcspn(const OQCQ0QCOQ0,OCCQ0QCOQ0:IntPtr):integer;
var
O00C0QCOQ0,OO0C0QCOQ0:IntPtr;
begin
O00C0QCOQ0:=OQCQ0QCOQ0;
while byte(O00C0QCOQ0^)<>0 do begin
OO0C0QCOQ0:=OCCQ0QCOQ0;
while byte(OO0C0QCOQ0^)<>0 do begin
if byte(O00C0QCOQ0^)=byte(OO0C0QCOQ0^)then begin
Result:=PtrSubstract(O00C0QCOQ0,OQCQ0QCOQ0);
Exit;
end;
OO0C0QCOQ0:=PtrOffset(OO0C0QCOQ0,1);
end;
O00C0QCOQ0:=PtrOffset(O00C0QCOQ0,1);
end;
Result:=PtrSubstract(O00C0QCOQ0,OQCQ0QCOQ0);
end;
{$IFNDEF WIN64}
function malloc(OQ0C0QCOQ0:OC0QOCCOQ0):pointer;
begin
GetMem(Result,OQ0C0QCOQ0);
end;
function realloc(OC0C0QCOQ0:pointer;O0OC0QCOQ0:OC0QOCCOQ0):pointer;
begin
Result:=OC0C0QCOQ0;
ReallocMem(Result,O0OC0QCOQ0);
end;
procedure memcpy(OOOC0QCOQ0:pointer;const OQOC0QCOQ0:pointer;OCOC0QCOQ0:OC0QOCCOQ0);
begin
Move(OQOC0QCOQ0^,OOOC0QCOQ0^,OCOC0QCOQ0);
end;
{$IFNDEF MOBILE_VFS}
procedure free(O0QC0QCOQ0:pointer);
begin
FreeMem(O0QC0QCOQ0);
end;
function memset(OOQC0QCOQ0:pointer;OQQC0QCOQ0:integer;OCQC0QCOQ0:OC0QOCCOQ0):pointer;
begin
Result:=OOQC0QCOQ0;
FillChar(OOQC0QCOQ0,OCQC0QCOQ0,OQQC0QCOQ0);
end;
procedure memmove(O0CC0QCOQ0:pointer;const OOCC0QCOQ0:pointer;OQCC0QCOQ0:OC0QOCCOQ0);
begin
Move(OOCC0QCOQ0^,O0CC0QCOQ0^,OQCC0QCOQ0);
end;
function memcmp(OCCC0QCOQ0,O0000QCOQ0:pointer;OO000QCOQ0:OC0QOCCOQ0):integer;
begin
if(OCCC0QCOQ0<>O0000QCOQ0)and(OO000QCOQ0<>0)then
if OCCC0QCOQ0<>nil then
if O0000QCOQ0<>nil then begin
repeat
if PByte(OCCC0QCOQ0)^<>PByte(O0000QCOQ0)^then begin
Result:=PByte(OCCC0QCOQ0)^-PByte(O0000QCOQ0)^;
Exit;
end;
Dec(OO000QCOQ0);
Inc(PByte(OCCC0QCOQ0));
Inc(PByte(O0000QCOQ0));
until OO000QCOQ0=0;
Result:=0;
end
else
Result:=1
else
Result:=-1
else
Result:=0;
end;
function strlen(OQ000QCOQ0:pointer):integer;
begin
Result:=length(PAnsiChar(OQ000QCOQ0));
end;
{$IFNDEF LINUX_VFS}
{$IFNDEF FPC}
{$IFNDEF MACOS64}
procedure _lldiv;
asm
jmp System.@_lldiv
end;
procedure _lludiv;
asm
jmp System.@_lludiv
end;
procedure _llmod;
asm
jmp System.@_llmod
end;
procedure _llmul;
asm
jmp System.@_llmul
end;
procedure _llumod;
asm
jmp System.@_llumod
end;
procedure _llshl;
asm
jmp System.@_llshl
end;
procedure _llshr;
asm
and cl,$3F
cmp cl,32
jl @__llshr@below32
mov eax,edx
cdq
sar eax,cl
ret
@__llshr@below32:
shrd eax,edx,cl
sar edx,cl
ret
end;
procedure _llushr;
asm
and cl,$3F
cmp cl,32
jl @__llushr@below32
mov eax,edx
xor edx,edx
shr eax,cl
ret
@__llushr@below32:
shrd eax,edx,cl
shr edx,cl
ret
end;
{$ENDIF NDEF MACOS64}
{$ENDIF NDEF FPC}
{$ENDIF NDEF LINUX_VFS}
{$ENDIF NDEF MOBILE_VFS}
{$ENDIF NDEF WIN64}
{$IFDEF FPC}
{$IFDEF WIN64}
function malloc(OQ0C0QCOQ0:size_t):pointer;
begin
GetMem(Result,OQ0C0QCOQ0);
end;
function realloc(OC0C0QCOQ0:pointer;O0OC0QCOQ0:size_t):pointer;
begin
Result:=OC0C0QCOQ0;
ReallocMem(Result,O0OC0QCOQ0);
end;
procedure memcpy(OOOC0QCOQ0:pointer;const OQOC0QCOQ0:pointer;OCOC0QCOQ0:size_t);
begin
Move(OQOC0QCOQ0^,OOOC0QCOQ0^,OCOC0QCOQ0);
end;
{$ENDIF}
{$ENDIF}
function strncmp(OC000QCOQ0,O0O00QCOQ0:PByte;OOO00QCOQ0:OC0QOCCOQ0):integer;
begin
while OOO00QCOQ0>0 do begin
if(OC000QCOQ0^<>O0O00QCOQ0^)or(byte(O0O00QCOQ0^)=0)then begin
Result:=integer(OC000QCOQ0^)-integer(O0O00QCOQ0^);
Exit;
end;
Inc(OC000QCOQ0);
Inc(O0O00QCOQ0);
Dec(OOO00QCOQ0);
end;
Result:=0;
end;
function strcmp(OQO00QCOQ0,OCO00QCOQ0:PByte):integer;
begin
while(OQO00QCOQ0^=OCO00QCOQ0^)and(byte(OCO00QCOQ0^)<>0)do begin
Inc(OQO00QCOQ0);
Inc(OCO00QCOQ0);
end;
Result:=integer(OQO00QCOQ0^)-Integer(OCO00QCOQ0^);
end;
{$IFNDEF MOBILE_VFS}
const
OOCCCQCOQ0:TDateTime=25569.0;
function OQCCCQCOQ0(OCCCCQCOQ0:integer):TDateTime;
begin
Result:=(OCCCCQCOQ0/86400)+OOCCCQCOQ0;
end;
{$IFDEF LITEDLL}
function EncodeDate(Year,Month,Day:Word):TDateTime;
begin
if not TryEncodeDate(Year,Month,Day,Result)then
raise Exception.Create('Invalid date');
end;
function YearOf(const AValue:TDateTime):Word;
var
LMonth,LDay:Word;
begin
DecodeDate(AValue,Result,LMonth,LDay);
end;
function StartOfTheYear(const AValue:TDateTime):TDateTime;
begin
Result:=EncodeDate(YearOf(AValue),1,1);
end;
function DayOfTheYear(const AValue:TDateTime):Word;
begin
Result:=Trunc(AValue)-Trunc(StartOfTheYear(AValue))+1;
end;
{$ENDIF}
procedure O000CQCOQ0(OO00CQCOQ0:pointer;const OQ00CQCOQ0:ptm;OC00CQCOQ0:boolean);
{$IFNDEF POSIX_VFS}
{$IFNDEF FPC}
var
O0O0CQCOQ0:TDateTime;
OOO0CQCOQ0,LocalTime:TSystemTime;
OQO0CQCOQ0:TTimeZoneInformation;
OCO0CQCOQ0:DWORD;
O0Q0CQCOQ0:integer;
OOQ0CQCOQ0:Int64;
{$ENDIF}
{$ENDIF}
begin
{$IFNDEF POSIX_VFS}
{$IFNDEF FPC}
if OO00CQCOQ0<>nil then begin
if OC00CQCOQ0 then begin
OOQ0CQCOQ0:=Int64(OO00CQCOQ0^);
O0Q0CQCOQ0:=integer(OOQ0CQCOQ0);
end
else
O0Q0CQCOQ0:=integer(OO00CQCOQ0^);
O0O0CQCOQ0:=OQCCCQCOQ0(O0Q0CQCOQ0);
end
else
O0O0CQCOQ0:=Now;
DateTimeToSystemTime(O0O0CQCOQ0,OOO0CQCOQ0);
if OO00CQCOQ0<>nil then begin
OCO0CQCOQ0:=GetTimeZoneInformation(OQO0CQCOQ0);
case OCO0CQCOQ0 of
TIME_ZONE_ID_STANDARD:
OQ00CQCOQ0^.tm_isdst:=integer(OQO0CQCOQ0.StandardDate.wMonth<>0);
TIME_ZONE_ID_DAYLIGHT:
OQ00CQCOQ0^.tm_isdst:=1;
else
OQ00CQCOQ0^.tm_isdst:=-1;
end;
SystemTimeToTzSpecificLocalTime(@OQO0CQCOQ0,OOO0CQCOQ0,LocalTime);
O0O0CQCOQ0:=SystemTimeToDateTime(LocalTime);
end
else
LocalTime:=OOO0CQCOQ0;
OQ00CQCOQ0^.tm_sec:=LocalTime.wSecond;
OQ00CQCOQ0^.tm_min:=LocalTime.wMinute;
OQ00CQCOQ0^.tm_hour:=LocalTime.wHour;
OQ00CQCOQ0^.tm_mday:=LocalTime.wDay;
OQ00CQCOQ0^.tm_mon:=LocalTime.wMonth-1;
OQ00CQCOQ0^.tm_year:=LocalTime.wYear-1900;
OQ00CQCOQ0^.tm_wday:=LocalTime.wDayOfWeek;
OQ00CQCOQ0^.tm_yday:=DayOfTheYear(O0O0CQCOQ0)-1;
{$ENDIF}
{$ENDIF}
end;
function localtime(O0Q00QCOQ0:pointer):ptm;
begin
Result:=malloc(SizeOf(stm));
Result^.tm_sec:=0;
Result^.tm_min:=0;
Result^.tm_hour:=0;
Result^.tm_mday:=0;
Result^.tm_mon:=0;
Result^.tm_year:=0;
Result^.tm_wday:=0;
Result^.tm_yday:=0;
Result^.tm_isdst:=0;
O000CQCOQ0(O0Q00QCOQ0,Result,False);
end;
{$IFDEF WIN64}
function _localtime64_s(OQQQQQCOQ0:pointer;OCQQQQCOQ0:pointer):integer;
begin
O000CQCOQ0(OCQQQQCOQ0,ptm(OQQQQQCOQ0),True);
Result:=0;
end;
function rand_s(O0OQQQCOQ0:pointer):O0OQOCCOQ0;
begin
if O0OQQQCOQ0=nil then begin
Result:=O00QOCCOQ0;
Exit;
end;
PUInt(O0OQQQCOQ0)^:=Random(MaxInt);
Result:=0;
end;
{$ENDIF DEF WIN64}
{$IFNDEF NOIMPLEMENT}
procedure OQQ0CQCOQ0(OCQ0CQCOQ0:PByte;O0C0CQCOQ0:OC0QOCCOQ0;OOC0CQCOQ0,OQC0CQCOQ0:OC0QOCCOQ0;OCC0CQCOQ0:OCC0OCCOQ0);
procedure O00OCQCOQ0(OO0OCQCOQ0,OQ0OCQCOQ0:PByte;OC0OCQCOQ0:OC0QOCCOQ0);
var
O0OOCQCOQ0:byte;
OOOOCQCOQ0:OC0QOCCOQ0;
begin
for OOOOCQCOQ0:=1 to OC0OCQCOQ0 do begin
O0OOCQCOQ0:=OO0OCQCOQ0^;
OO0OCQCOQ0^:=OQ0OCQCOQ0^;
OQ0OCQCOQ0^:=O0OOCQCOQ0;
Inc(OO0OCQCOQ0);
Inc(OQ0OCQCOQ0);
end;
end;
var
OQOOCQCOQ0,OCOOCQCOQ0,O0QOCQCOQ0:OC0QOCCOQ0;
OOQOCQCOQ0,OQQOCQCOQ0:PByte;
begin
repeat
OQOOCQCOQ0:=OOC0CQCOQ0;
OCOOCQCOQ0:=OQC0CQCOQ0;
O0QOCQCOQ0:=(OOC0CQCOQ0+OQC0CQCOQ0)shr 1;
repeat
OOQOCQCOQ0:=PByte(NativeUInt(OCQ0CQCOQ0)+O0QOCQCOQ0*O0C0CQCOQ0);
OQQOCQCOQ0:=PByte(NativeUInt(OCQ0CQCOQ0)+OQOOCQCOQ0*O0C0CQCOQ0);
while OCC0CQCOQ0(OQQOCQCOQ0,OOQOCQCOQ0)<0 do begin
Inc(OQOOCQCOQ0);
Inc(OQQOCQCOQ0,O0C0CQCOQ0);
end;
OQQOCQCOQ0:=PByte(NativeUInt(OCQ0CQCOQ0)+OCOOCQCOQ0*O0C0CQCOQ0);
while OCC0CQCOQ0(OQQOCQCOQ0,OOQOCQCOQ0)>0 do begin
Dec(OCOOCQCOQ0);
Dec(OQQOCQCOQ0,O0C0CQCOQ0);
end;
if OQOOCQCOQ0<=OCOOCQCOQ0 then begin
O00OCQCOQ0(PByte(NativeUInt(OCQ0CQCOQ0)+OQOOCQCOQ0*O0C0CQCOQ0),PByte(NativeUInt(OCQ0CQCOQ0)+OCOOCQCOQ0*O0C0CQCOQ0),O0C0CQCOQ0);
if O0QOCQCOQ0=OQOOCQCOQ0 then
O0QOCQCOQ0:=OCOOCQCOQ0
else if O0QOCQCOQ0=OCOOCQCOQ0 then
O0QOCQCOQ0:=OQOOCQCOQ0;
Inc(OQOOCQCOQ0);
Dec(OCOOCQCOQ0);
end;
until OQOOCQCOQ0>OCOOCQCOQ0;
if OOC0CQCOQ0<OCOOCQCOQ0 then
OQQ0CQCOQ0(OCQ0CQCOQ0,O0C0CQCOQ0,OOC0CQCOQ0,OCOOCQCOQ0,OCC0CQCOQ0);
OOC0CQCOQ0:=OQOOCQCOQ0;
until OQOOCQCOQ0>=OQC0CQCOQ0;
end;
procedure qsort(OOQ00QCOQ0:PByte;OQQ00QCOQ0,OCQ00QCOQ0:OC0QOCCOQ0;O0C00QCOQ0:OCC0OCCOQ0);
begin
if(OQQ00QCOQ0>1)and(OCQ00QCOQ0>0)then
OQQ0CQCOQ0(OOQ00QCOQ0,OCQ00QCOQ0,0,OQQ00QCOQ0-1,O0C00QCOQ0);
end;
{$ENDIF}
{$ENDIF NDEF MOBILE_VFS}
{$IFDEF WIN64}
procedure __chkstk;cdecl;
begin
end;
procedure __faststorefence;cdecl;
begin
end;
{$IFNDEF VER18P}
procedure __imp_InitializeCriticalSection(var OOOQQQCOQ0:TRTLCriticalSection);
begin
InitializeCriticalSection(OOOQQQCOQ0);
end;
procedure __imp_DeleteCriticalSection(var OQOQQQCOQ0:TRTLCriticalSection);
begin
DeleteCriticalSection(OQOQQQCOQ0);
end;
procedure __imp_EnterCriticalSection(var OCOQQQCOQ0:TRTLCriticalSection);
begin
EnterCriticalSection(OCOQQQCOQ0);
end;
procedure __imp_LeaveCriticalSection(var O0QQQQCOQ0:TRTLCriticalSection);
begin
LeaveCriticalSection(O0QQQQCOQ0);
end;
function __imp_CloseHandle(OOQQQQCOQ0:THandle):BOOL;
begin
Result:=CloseHandle(OOQQQQCOQ0);
end;
function __imp_GetCurrentThreadId:DWORD;
begin
Result:=GetCurrentThreadId;
end;
{$ENDIF NDEF VER18P}
{$ENDIF DEF WIN64}
{$IFNDEF CUSTOM_VFS}
{$IFNDEF LINUX_FPC}
{$IFNDEF DARWIN_FPC}
function _beginthreadex(OCQOCQCOQ0:Pointer;O0COCQCOQ0:OC0QOCCOQ0;OOCOCQCOQ0:TFNThreadStartRoutine;
OQCOCQCOQ0:Pointer;OCCOCQCOQ0:DWORD;var O00QQQCOQ0:DWORD):THandle;
begin
Result:=CreateThread(OCQOCQCOQ0,O0COCQCOQ0,OOCOCQCOQ0,OQCOCQCOQ0,OCCOCQCOQ0,O00QQQCOQ0);
end;
procedure _endthreadex(OO0QQQCOQ0:Integer);
begin
ExitThread(OO0QQQCOQ0);
end;
{$ENDIF NDEF DARWIN_FPC}
{$ENDIF NDEF LINUX_FPC}
{$ENDIF NDEF CUSTOM_VFS}
{$IFDEF CUSTOM_VFS}
function OC0CQQCOQ0:string;
begin
{$IFDEF MOBILE_VFS}
Result:=GetHomePath+PathDelim{$IFNDEF ANDROID_VFS}+'Documents'+PathDelim{$ENDIF};
{$ENDIF DEF MOBILE_VFS}
{$IFDEF POSIX_VFS}
Result:=GetHomePath+PathDelim+'Documents'+PathDelim;
{$ENDIF DEF POSIX_VFS}
{$IFDEF WIN_VFS}
Result:=GetHomePath+PathDelim;
{$ENDIF}
end;
{$IFDEF CUSTOM_FILESTREAM}
function OCQOQOCOQ0(const O0COQOCOQ0:string):boolean;
{$IFDEF LINUX_VFS}
var
OOOCO0COQ0:Integer;
begin
Result:=False;
OOOCO0COQ0:=Integer(O0O0QOCOQ0(PAnsiChar(CRFunctions.UTF8Encode(O0COQOCOQ0)),OQQCQQCOQ0,OCCCQQCOQ0 or OO00QQCOQ0));
if OOOCO0COQ0<>-1 then begin
OOQ0QOCOQ0(OOOCO0COQ0);
Result:=True;
Exit;
end;
end;
{$ELSE}
var
OOCOQOCOQ0:OC0OQQCOQ0;
begin
if OC00QOCOQ0(PAnsiChar(CRFunctions.UTF8Encode(O0COQOCOQ0)),OOCOQOCOQ0)=0 then begin
if(OOCOQOCOQ0.OOOOQQCOQ0 and OQOCQQCOQ0)=OCOCQQCOQ0 then begin
if OQ00QOCOQ0(PAnsiChar(CRFunctions.UTF8Encode(O0COQOCOQ0)),OOCOQOCOQ0)=0 then
Result:=(OOCOQOCOQ0.OOOOQQCOQ0 and OQOCQQCOQ0)<>O0QCQQCOQ0
else
Result:=False;
end
else
Result:=(OOCOQOCOQ0.OOOOQQCOQ0 and OQOCQQCOQ0)<>O0QCQQCOQ0;
end
else
Result:=False;
end;
{$ENDIF}
function OQCOQOCOQ0(const OCCOQOCOQ0:string;O00QO0COQ0,OO0QO0COQ0:LongWord):THandle;
var
OQ0QO0COQ0,OC0QO0COQ0:Integer;
begin
Result:=INVALID_HANDLE_VALUE;
if OO0QO0COQ0=OCQQOOCOQ0 then
OC0QO0COQ0:=OQQCQQCOQ0 or O0CCQQCOQ0
else
OC0QO0COQ0:=OCQCQQCOQ0 or O0CCQQCOQ0;
if O00QO0COQ0=OOCQOOCOQ0 then
OC0QO0COQ0:=OC0QO0COQ0 or OQCCQQCOQ0;
OQ0QO0COQ0:=Integer(O0O0QOCOQ0(PAnsiChar(CRFunctions.UTF8Encode(OCCOQOCOQ0)),OC0QO0COQ0,OOO0QQCOQ0));
if OQ0QO0COQ0<>-1 then
Result:=OQ0QO0COQ0;
end;
function O0OQO0COQ0(const OOOQO0COQ0:string;OQOQO0COQ0:LongWord):THandle;
var
OCOQO0COQ0,O0QQO0COQ0:Integer;
begin
Result:=INVALID_HANDLE_VALUE;
if OQOQO0COQ0=OCQQOOCOQ0 then
O0QQO0COQ0:=OQQCQQCOQ0
else
O0QQO0COQ0:=OCQCQQCOQ0;
OCOQO0COQ0:=O0O0QOCOQ0(PAnsiChar(CRFunctions.UTF8Encode(OOOQO0COQ0)),O0QQO0COQ0,OOO0QQCOQ0);
if OCOQO0COQ0<>-1 then
Result:=OCOQO0COQ0;
end;
function OOQQO0COQ0(const OQQQO0COQ0:string):{$IFDEF WIN_VFS}TFileAttributes{$ELSE}integer{$ENDIF};
{$IFNDEF LINUX_VFS}
var
OCQQO0COQ0:OC0OQQCOQ0;
O0CQO0COQ0:OC0OQQCOQ0;
OOCQO0COQ0:string;
OQCQO0COQ0:Integer;
OCCQO0COQ0:Pointer;
{$ENDIF}
begin
{$IFDEF LINUX_VFS}
Result:=0;
{$ELSE}
Result:=faInvalid;
OCCQO0COQ0:=PAnsiChar(CRFunctions.UTF8Encode(OQQQO0COQ0));
if OQ00QOCOQ0(OCCQO0COQ0,OCQQO0COQ0)=0 then begin
Result:=0;
if(OCQQO0COQ0.OOOOQQCOQ0 and OQOCQQCOQ0)=O0QCQQCOQ0 then
Result:=faDirectory
else if((OCQQO0COQ0.OOOOQQCOQ0 and OQOCQQCOQ0)<>OOQCQQCOQ0)and((OCQQO0COQ0.OOOOQQCOQ0 and OQOCQQCOQ0)=OCOCQQCOQ0)then begin
Result:=Result or faSymLink;
if(OQ00QOCOQ0(OCCQO0COQ0,O0CQO0COQ0)=0)and((O0CQO0COQ0.OOOOQQCOQ0 and OQOCQQCOQ0)=O0QCQQCOQ0)then
Result:=Result or faDirectory;
end;
OOCQO0COQ0:=ExtractFilename(OQQQO0COQ0);
OQCQO0COQ0:=Length(OOCQO0COQ0);
if(OQCQO0COQ0>1)and(OOCQO0COQ0[1]='.')and(OOCQO0COQ0[2]<>#0)then begin
if(OQCQO0COQ0>3)and not((OOCQO0COQ0[2]='.')and(OOCQO0COQ0[3]=#0))then
Result:=Result or faHidden;
end;
end;
{$ENDIF}
end;
procedure O00CO0COQ0(const OO0CO0COQ0:string);
var
OQ0CO0COQ0:Cardinal;
begin
if O0Q0QOCOQ0(PAnsiChar(CRFunctions.UTF8Encode(OO0CO0COQ0)))=-1 then
OQ0CO0COQ0:=GetLastError()
else
OQ0CO0COQ0:=0;
if(OQ0CO0COQ0=OQO0QQCOQ0)or(OQ0CO0COQ0=OCO0QQCOQ0)or(OQ0CO0COQ0=O0Q0QQCOQ0)or(OQ0CO0COQ0=OOQ0QQCOQ0)then
raise EInOutError.Create(SysErrorMessage(OQ0CO0COQ0));
end;
constructor O00COOCOQ0.Create(const OQC0QOCOQ0:string;OCC0QOCOQ0,O00OQOCOQ0:LongWord);
begin
if(OCC0QOCOQ0<>OQCQOOCOQ0)then begin
inherited Create(OQCOQOCOQ0(OQC0QOCOQ0,OCC0QOCOQ0,O00OQOCOQ0));
if FHandle=INVALID_HANDLE_VALUE then
raise EFCreateError.CreateResFmt(@SFCreateErrorEx,[ExpandFileName(OQC0QOCOQ0),SysErrorMessage(GetLastError)]);
end
else begin
inherited Create(O0OQO0COQ0(OQC0QOCOQ0,O00OQOCOQ0));
if FHandle=INVALID_HANDLE_VALUE then
raise EFOpenError.CreateResFmt(@SFOpenErrorEx,[ExpandFileName(OQC0QOCOQ0),SysErrorMessage(GetLastError)]);
end;
end;
destructor O00COOCOQ0.Destroy;
begin
if FHandle<>INVALID_HANDLE_VALUE then begin
OOQ0QOCOQ0(FHandle);
FHandle:=INVALID_HANDLE_VALUE;
end;
inherited Destroy;
end;
procedure O00COOCOQ0.SetSize(const O0C0QOCOQ0:Int64);
begin
Seek(O0C0QOCOQ0,soBeginning);
{$IFDEF MSWINDOWS}
Win32Check(SetEndOfFile(FHandle));
{$ELSE}
{$IFDEF POSIX}
if OQQ0QOCOQ0(FHandle,Position)=-1 then
raise EStreamError(sStreamSetSize);
{$ENDIF}
{$ENDIF}
end;
function O00COOCOQ0.Seek(const OC0OQOCOQ0:Int64;O0OOQOCOQ0:TSeekOrigin):Int64;
begin
Result:=OOO0QOCOQ0(Handle,OC0OQOCOQ0,Ord(O0OOQOCOQ0));
end;
function O00COOCOQ0.Read(var OQOOQOCOQ0;OCOOQOCOQ0:Longint):Longint;
begin
Result:=OQO0QOCOQ0(FHandle,@OQOOQOCOQ0,OCOOQOCOQ0);
if Result=-1 then Result:=0;
end;
function O00COOCOQ0.Write(const OOQOQOCOQ0;OQQOQOCOQ0:Longint):Longint;
begin
Result:=OCO0QOCOQ0(Handle,@OOQOQOCOQ0,OQQOQOCOQ0);
if Result=-1 then Result:=0;
end;
{$ENDIF DEF CUSTOM_FILESTREAM}
function OO0COOCOQ0:string;
const
OQ0COOCOQ0:string='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'+
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'+
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'+
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'+
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
OC0COOCOQ0,O0OCOOCOQ0:integer;
OOOCOOCOQ0:array[0..14]of byte;
begin
Result:=OC0CQQCOQ0;
OC0COOCOQ0:=Length(Result);
SetLength(Result,OC0COOCOQ0+15);
sqlite3_randomness(15,@OOOCOOCOQ0);
for O0OCOOCOQ0:=0 to 14 do
Result[OC0COOCOQ0+O0OCOOCOQ0+1]:=OQ0COOCOQ0[ord(OOOCOOCOQ0[O0OCOOCOQ0])+1];
Result:=Result+'.tmp';
end;
function OQOCOOCOQ0(OCOCOOCOQ0:psqlite3_vfs;const O0QCOOCOQ0:PAnsiChar;OOQCOOCOQ0:psqlite3_customfile;OQQCOOCOQ0:Integer;OCQCOOCOQ0:pInteger):Integer;cdecl;
var
O0CCOOCOQ0,
OOCCOOCOQ0,
OQCCOOCOQ0,
OCCCOOCOQ0:boolean;
O000OOCOQ0:string;
OO00OOCOQ0:Word;
{$IFDEF CUSTOM_FILESTREAM}
OC0CO0COQ0:LongWord;
{$ENDIF}
begin
Result:=SQLITE_OK;
O0CCOOCOQ0:=(OQQCOOCOQ0 and SQLITE_OPEN_EXCLUSIVE)>0;
OOCCOOCOQ0:=(OQQCOOCOQ0 and SQLITE_OPEN_DELETEONCLOSE)>0;
OQCCOOCOQ0:=(OQQCOOCOQ0 and SQLITE_OPEN_CREATE)>0;
OCCCOOCOQ0:=(OQQCOOCOQ0 and SQLITE_OPEN_READWRITE)>0;
try
FillChar(OOQCOOCOQ0,SizeOf(sqlite3_customfile),0);
OOQCOOCOQ0^.FileType:=OQQCOOCOQ0 and$FFFFFF00;
O000OOCOQ0:=Trim(string(O0QCOOCOQ0));
if O000OOCOQ0='' then
O000OOCOQ0:=OO0COOCOQ0;
{$IFNDEF CUSTOM_FILESTREAM}
if O0CCOOCOQ0 then
OO00OOCOQ0:=fmCreate
else if OQCCOOCOQ0 then begin
if FileExists(O000OOCOQ0)then
OO00OOCOQ0:=fmOpenReadWrite
else
OO00OOCOQ0:=fmCreate;
end
else
OO00OOCOQ0:=fmOpenReadWrite;
{$ELSE}
if O0CCOOCOQ0 then
OO00OOCOQ0:=OOCQOOCOQ0
else if OQCCOOCOQ0 then begin
if OCQOQOCOQ0(O000OOCOQ0)then
OO00OOCOQ0:=OQCQOOCOQ0
else
OO00OOCOQ0:=OCCQOOCOQ0;
end
else
OO00OOCOQ0:=OQCQOOCOQ0;
if OCCCOOCOQ0 then
OC0CO0COQ0:=OCQQOOCOQ0 or O0CQOOCOQ0
else
OC0CO0COQ0:=OCQQOOCOQ0;
{$ENDIF}
try
OOQCOOCOQ0^.Stream:=O00COOCOQ0.Create(O000OOCOQ0,OO00OOCOQ0{$IFDEF CUSTOM_FILESTREAM},OC0CO0COQ0{$ENDIF});
if O00COOCOQ0(OOQCOOCOQ0^.Stream).Handle=INVALID_HANDLE_VALUE then begin
OOQCOOCOQ0^.LastErrno:=GetLastError;
Result:=SQLITE_CANTOPEN;
end;
except
on E:exception do begin
OOQCOOCOQ0^.LastErrno:=GetLastError;
Result:=SQLITE_CANTOPEN;
Exit;
end;
end;
if OCQCOOCOQ0<>nil then
if OCCCOOCOQ0 then
OCQCOOCOQ0^:=SQLITE_OPEN_READWRITE
else
OCQCOOCOQ0^:=SQLITE_OPEN_READONLY;
OOQCOOCOQ0^.LastErrno:=0;
OOQCOOCOQ0^.Vfs:=OCOCOOCOQ0;
OOQCOOCOQ0^.Name:=O000OOCOQ0;
OOQCOOCOQ0^.LockType:=NO_LOCK;
OOQCOOCOQ0^.DeleteOnClose:=OOCCOOCOQ0;
OOQCOOCOQ0^.Methods:=@OO0CQQCOQ0;
except
end;
end;
function OQ00OOCOQ0(OC00OOCOQ0:psqlite3_vfs;O0O0OOCOQ0:PAnsiChar;OOO0OOCOQ0:Integer):Integer;cdecl;
var
OQO0OOCOQ0:string;
OCO0OOCOQ0:integer;
begin
Result:=SQLITE_OK;
try
OQO0OOCOQ0:=string(O0O0OOCOQ0);
if OQO0OOCOQ0='' then begin
Result:=SQLITE_IOERR_NOMEM;
Exit;
end;
try
if not {$IFNDEF CUSTOM_FILESTREAM}FileExists(OQO0OOCOQ0){$ELSE}OCQOQOCOQ0(OQO0OOCOQ0){$ENDIF} then begin
Result:=SQLITE_IOERR_DELETE_NOENT;
Exit;
end
else
OCO0OOCOQ0:={$IFNDEF CUSTOM_FILESTREAM}FileGetAttr(OQO0OOCOQ0){$ELSE}OOQQO0COQ0(OQO0OOCOQ0){$ENDIF};
except
Result:=SQLITE_ERROR;
Exit;
end;
if(OCO0OOCOQ0 and faDirectory)>0 then begin
Result:=SQLITE_ERROR;
Exit;
end;
try
{$IFNDEF CUSTOM_FILESTREAM}DeleteFile(OQO0OOCOQ0){$ELSE}O00CO0COQ0(OQO0OOCOQ0){$ENDIF};
except
on E:Exception do begin
Result:=SQLITE_ERROR;
end;
end;
except
end;
end;
function O0Q0OOCOQ0(OOQ0OOCOQ0:psqlite3_vfs;OQQ0OOCOQ0:PAnsiChar;OCQ0OOCOQ0:Integer;var O0C0OOCOQ0:integer):Integer;cdecl;
var
OOC0OOCOQ0:string;
OQC0OOCOQ0:integer;
OCC0OOCOQ0:boolean;
begin
Result:=SQLITE_OK;
O0C0OOCOQ0:=0;
OCC0OOCOQ0:=False;
try
OQC0OOCOQ0:=faInvalid;
OOC0OOCOQ0:=string(OQQ0OOCOQ0);
if OOC0OOCOQ0='' then begin
Result:=SQLITE_IOERR_NOMEM;
Exit;
end;
if(OCQ0OOCOQ0=SQLITE_ACCESS_EXISTS)and not {$IFNDEF CUSTOM_FILESTREAM}FileExists(OOC0OOCOQ0){$ELSE}OCQOQOCOQ0(OOC0OOCOQ0){$ENDIF} then begin
OCC0OOCOQ0:=True;
end
else
OQC0OOCOQ0:={$IFNDEF CUSTOM_FILESTREAM}FileGetAttr(OOC0OOCOQ0){$ELSE}OOQQO0COQ0(OOC0OOCOQ0){$ENDIF};
case OCQ0OOCOQ0 of
SQLITE_ACCESS_READ,
SQLITE_ACCESS_EXISTS:
O0C0OOCOQ0:=integer(not OCC0OOCOQ0);
SQLITE_ACCESS_READWRITE:
O0C0OOCOQ0:=integer((not OCC0OOCOQ0)and(
(OQC0OOCOQ0 and faReadOnly)=0
));
end;
except
end;
end;
function O00OOOCOQ0(OO0OOOCOQ0:psqlite3_vfs;const OQ0OOOCOQ0:PAnsiChar;OC0OOOCOQ0:Integer;O0OOOOCOQ0:PAnsiChar):Integer;cdecl;
var
OOOOOOCOQ0:AnsiString;
OQOOOOCOQ0:integer;
begin
Result:=SQLITE_ERROR;
try
OOOOOOCOQ0:=AnsiString(OQ0OOOCOQ0);
if(OOOOOOCOQ0<>'')and {$IFNDEF WIN_VFS}(byte(OQ0OOOCOQ0^)<>ord('/')){$ELSE}(ExtractFilePath(string(tmpName))=''){$ENDIF} then
OOOOOOCOQ0:=AnsiString(OC0CQQCOQ0+string(OOOOOOCOQ0));
OQOOOOCOQ0:=LengthA(OOOOOOCOQ0);
if OQOOOOCOQ0>OC0OOOCOQ0 then
OQOOOOCOQ0:=OC0OOOCOQ0;
FillChar(O0OOOOCOQ0,OC0OOOCOQ0,0);
CopyBuffer({$IFNDEF NEXTGEN}pointer{$ENDIF}(OOOOOOCOQ0){$IFDEF NEXTGEN}.Ptr{$ENDIF},O0OOOOCOQ0,OQOOOOCOQ0);
Result:=SQLITE_OK;
except
end;
end;
function OCOOOOCOQ0(O0QOOOCOQ0:psqlite3_vfs;OOQOOOCOQ0:PAnsiChar):Pointer;cdecl;
begin
Result:=nil;
end;
procedure OQQOOOCOQ0(OCQOOOCOQ0:psqlite3_vfs;O0COOOCOQ0:Integer;OOCOOOCOQ0:PAnsiChar);cdecl;
begin
end;
function OQCOOOCOQ0(OCCOOOCOQ0:psqlite3_vfs;O00Q0OCOQ0:Pointer;OO0Q0OCOQ0:PAnsiChar):Pointer;cdecl;
begin
Result:=nil;
end;
procedure OQ0Q0OCOQ0(OC0Q0OCOQ0:psqlite3_vfs;O0OQ0OCOQ0:Pointer);cdecl;
begin
end;
function OOOQ0OCOQ0(OQOQ0OCOQ0:psqlite3_vfs;OCOQ0OCOQ0:Integer;O0QQ0OCOQ0:pointer):Integer;cdecl;
var
OOQQ0OCOQ0,OQQQ0OCOQ0:integer;
OCQQ0OCOQ0:Cardinal;
O0CQ0OCOQ0:TDateTime;
OOCQ0OCOQ0:pointer;
begin
OOQQ0OCOQ0:=0;
OOCQ0OCOQ0:=O0QQ0OCOQ0;
while True do begin
OQQQ0OCOQ0:=SizeOf(Cardinal);
if OQQQ0OCOQ0<=(OCOQ0OCOQ0-OOQQ0OCOQ0)then begin
OCQQ0OCOQ0:=GetTickCount;
Move(OCQQ0OCOQ0,OOCQ0OCOQ0^,OQQQ0OCOQ0);
OOQQ0OCOQ0:=OOQQ0OCOQ0+OQQQ0OCOQ0;
OOCQ0OCOQ0:=PtrOffset(OOCQ0OCOQ0,OQQQ0OCOQ0);
end
else
Break;
OQQQ0OCOQ0:=SizeOf(TDateTime);
if OQQQ0OCOQ0<=(OCOQ0OCOQ0-OOQQ0OCOQ0)then begin
O0CQ0OCOQ0:=now;
Move(O0CQ0OCOQ0,OOCQ0OCOQ0^,OQQQ0OCOQ0);
OOQQ0OCOQ0:=OOQQ0OCOQ0+OQQQ0OCOQ0;
OOCQ0OCOQ0:=PtrOffset(OOCQ0OCOQ0,OQQQ0OCOQ0);
end
else
Break;
end;
Result:=OOQQ0OCOQ0;
end;
function OQCQ0OCOQ0(OCCQ0OCOQ0:psqlite3_vfs;O00C0OCOQ0:Integer):Integer;cdecl;
begin
Result:=SQLITE_OK;
end;
function OO0C0OCOQ0(OQ0C0OCOQ0:psqlite3_vfs;var OC0C0OCOQ0:double):Integer;cdecl;
begin
Result:=SQLITE_OK;
end;
function O0OC0OCOQ0(OOOC0OCOQ0:psqlite3_vfs;OQOC0OCOQ0:Integer;OCOC0OCOQ0:PAnsiChar):Integer;cdecl;
begin
Result:=SQLITE_OK;
end;
function O0QC0OCOQ0(OOQC0OCOQ0:psqlite3_vfs;var OQQC0OCOQ0:i64):Integer;cdecl;
{$IFNDEF WIN_VFS}
const
OCQC0OCOQ0:Int64=24405875*Int64(8640000);
var
O0CC0OCOQ0:timeval;
{$ENDIF}
begin
{$IFNDEF WIN_VFS}
gettimeofday(O0CC0OCOQ0,nil);
OQQC0OCOQ0:=OCQC0OCOQ0+1000*Int64(O0CC0OCOQ0.tv_sec)+trunc(O0CC0OCOQ0.tv_usec/1000);
{$ENDIF}
Result:=SQLITE_OK;
end;
function OOCC0OCOQ0(OQCC0OCOQ0:psqlite3_vfs;OCCC0OCOQ0:PAnsiChar;O0000OCOQ0:pointer):Integer;cdecl;
begin
Result:=SQLITE_OK;
end;
function OO000OCOQ0(OQ000OCOQ0:psqlite3_vfs;OC000OCOQ0:PAnsiChar):pointer;cdecl;
begin
Result:=nil;
end;
function O0O00OCOQ0(OOO00OCOQ0:psqlite3_vfs;OQO00OCOQ0:PAnsiChar):PAnsiChar;cdecl;
begin
Result:=nil;
end;
function OCO00OCOQ0(O0Q00OCOQ0:psqlite3_customfile):Integer;cdecl;
var
OOQ00OCOQ0:string;
begin
try
if O0Q00OCOQ0^.DeleteOnClose then
OOQ00OCOQ0:=O0Q00OCOQ0^.Name
else
OOQ00OCOQ0:='';
O0Q00OCOQ0^.Stream.Free;
O0Q00OCOQ0^.Stream:=nil;
if OOQ00OCOQ0<>'' then
OQ00OOCOQ0(nil,PAnsiChar(AnsiString(OOQ00OCOQ0)),0);
Result:=SQLITE_OK;
except
on E:Exception do begin
Result:=SQLITE_IOERR_CLOSE;
end;
end;
end;
function OQQ00OCOQ0(OCQ00OCOQ0:psqlite3_customfile;const O0C00OCOQ0:pointer;OOC00OCOQ0:Integer;OQC00OCOQ0:i64):Integer;cdecl;
var
OCC00OCOQ0:O00COOCOQ0;
O00O0OCOQ0:integer;
begin
Result:=SQLITE_IOERR;
try
OCC00OCOQ0:=O00COOCOQ0(OCQ00OCOQ0^.Stream);
if OQC00OCOQ0<>OCC00OCOQ0.Position then begin
if OCC00OCOQ0.Seek(OQC00OCOQ0,soFromBeginning)<0 then begin
Result:=SQLITE_IOERR_SEEK;
Exit;
end
else begin
end;
end;
try
O00O0OCOQ0:=OCC00OCOQ0.Read(O0C00OCOQ0^,OOC00OCOQ0);
except
on E:Exception do begin
O00O0OCOQ0:=-1;
end;
end;
if O00O0OCOQ0=OOC00OCOQ0 then
Result:=SQLITE_OK
else if O00O0OCOQ0<0 then begin
Result:=SQLITE_IOERR_READ;
OCQ00OCOQ0^.LastErrno:=GetLastError;
end
else begin
OCQ00OCOQ0^.LastErrno:=0;
FillChar(PtrOffset(O0C00OCOQ0,O00O0OCOQ0),OOC00OCOQ0-O00O0OCOQ0,0);
Result:=SQLITE_IOERR_SHORT_READ;
end;
except
end;
end;
function OO0O0OCOQ0(OQ0O0OCOQ0:psqlite3_customfile;const OC0O0OCOQ0:pointer;O0OO0OCOQ0:Integer;OOOO0OCOQ0:i64):Integer;cdecl;
var
OQOO0OCOQ0:O00COOCOQ0;
OCOO0OCOQ0:pointer;
O0QO0OCOQ0:integer;
OOQO0OCOQ0:integer;
begin
Result:=SQLITE_IOERR_WRITE;
try
OQOO0OCOQ0:=O00COOCOQ0(OQ0O0OCOQ0^.Stream);
if(OOOO0OCOQ0>OQOO0OCOQ0.Size)
and((OQ0O0OCOQ0^.FileType=SQLITE_OPEN_MEMORY)
or(OQ0O0OCOQ0^.FileType=SQLITE_OPEN_MAIN_DB)
or(OQ0O0OCOQ0^.FileType=SQLITE_OPEN_TEMP_DB)
or(OQ0O0OCOQ0^.FileType=SQLITE_OPEN_TRANSIENT_DB)
or(OQ0O0OCOQ0^.FileType=SQLITE_OPEN_MAIN_JOURNAL)
or(OQ0O0OCOQ0^.FileType=SQLITE_OPEN_TEMP_JOURNAL)
or(OQ0O0OCOQ0^.FileType=SQLITE_OPEN_SUBJOURNAL)
or(OQ0O0OCOQ0^.FileType=SQLITE_OPEN_MASTER_JOURNAL))then begin
OQOO0OCOQ0.Size:=OOOO0OCOQ0;
end;
if OOOO0OCOQ0>OQOO0OCOQ0.Size then begin
Result:=SQLITE_IOERR_SEEK;
Exit;
end;
if(OOOO0OCOQ0<>OQOO0OCOQ0.Position)and(OQOO0OCOQ0.Seek(OOOO0OCOQ0,soFromBeginning)<0)then begin
Result:=SQLITE_IOERR_SEEK;
Exit;
end
else begin
end;
OCOO0OCOQ0:=OC0O0OCOQ0;
O0QO0OCOQ0:=O0OO0OCOQ0;
while O0QO0OCOQ0>0 do begin
OOQO0OCOQ0:=OQOO0OCOQ0.Write(OCOO0OCOQ0^,O0QO0OCOQ0);
if((OOQO0OCOQ0=0)or(OOQO0OCOQ0>O0QO0OCOQ0))then begin
Result:=SQLITE_IOERR_WRITE;
OQ0O0OCOQ0^.LastErrno:=GetLastError;
Exit;
end;
O0QO0OCOQ0:=O0QO0OCOQ0-OOQO0OCOQ0;
OCOO0OCOQ0:=PtrOffset(OCOO0OCOQ0,OOQO0OCOQ0);
end;
Result:=SQLITE_OK;
except
end;
end;
function OQQO0OCOQ0(OCQO0OCOQ0:psqlite3_customfile;O0CO0OCOQ0:i64):Integer;cdecl;
var
OOCO0OCOQ0:O00COOCOQ0;
begin
Result:=SQLITE_IOERR_WRITE;
try
OOCO0OCOQ0:=O00COOCOQ0(OCQO0OCOQ0^.Stream);
OOCO0OCOQ0.Size:=O0CO0OCOQ0;
Result:=SQLITE_OK;
except
end;
end;
function OQCO0OCOQ0(OCCO0OCOQ0:psqlite3_customfile;O00QCOCOQ0:Integer):Integer;cdecl;
begin
Result:=SQLITE_OK;
end;
function OO0QCOCOQ0(OQ0QCOCOQ0:psqlite3_customfile;OC0QCOCOQ0:pointer):Integer;cdecl;
begin
Result:=SQLITE_ERROR;
try
i64(OC0QCOCOQ0^):=OQ0QCOCOQ0^.Stream.Size;
Result:=SQLITE_OK;
except
end;
end;
function O0OQCOCOQ0(OOOQCOCOQ0:psqlite3_customfile;OQOQCOCOQ0:Integer):Integer;cdecl;
var
OCOQCOCOQ0,O0QQCOCOQ0:byte;
begin
Result:=SQLITE_OK;
try
OCOQCOCOQ0:=OOOQCOCOQ0^.LockType;
if(OCOQCOCOQ0>=OQOQCOCOQ0)then
Exit;
O0QQCOCOQ0:=OOOQCOCOQ0^.LockType;
if(OQOQCOCOQ0=SHARED_LOCK)then
if OCOQCOCOQ0=NO_LOCK then
O0QQCOCOQ0:=SHARED_LOCK
else
Result:=SQLITE_IOERR_LOCK;
if(OQOQCOCOQ0=RESERVED_LOCK)then
if OCOQCOCOQ0=SHARED_LOCK then
O0QQCOCOQ0:=RESERVED_LOCK
else
Result:=SQLITE_IOERR_LOCK;
if(OQOQCOCOQ0=EXCLUSIVE_LOCK)then
if OCOQCOCOQ0>=SHARED_LOCK then
O0QQCOCOQ0:=EXCLUSIVE_LOCK
else
Result:=SQLITE_IOERR_LOCK;
if Result=SQLITE_OK then
OOOQCOCOQ0^.LockType:=O0QQCOCOQ0;
except
on E:Exception do begin
Result:=SQLITE_IOERR_LOCK;
end;
end;
end;
function OOQQCOCOQ0(OQQQCOCOQ0:psqlite3_customfile;OCQQCOCOQ0:Integer):Integer;cdecl;
begin
Result:=SQLITE_OK;
OQQQCOCOQ0^.LockType:=OCQQCOCOQ0;
end;
function O0CQCOCOQ0(OOCQCOCOQ0:psqlite3_customfile;OQCQCOCOQ0:pInteger):Integer;cdecl;
begin
Result:=SQLITE_IOERR_LOCK;
try
if(OOCQCOCOQ0^.LockType>=RESERVED_LOCK)then
OQCQCOCOQ0^:=1
else
OQCQCOCOQ0^:=0;
Result:=SQLITE_OK;
except
end;
end;
procedure OCCQCOCOQ0(O00CCOCOQ0:psqlite3_customfile;OO0CCOCOQ0:byte;OQ0CCOCOQ0:pInteger);cdecl;
begin
end;
function OC0CCOCOQ0(O0OCCOCOQ0:psqlite3_customfile;OOOCCOCOQ0:Integer;OQOCCOCOQ0:pointer):Integer;cdecl;
var
OCOCCOCOQ0:TBytes;
O0QCCOCOQ0:O00COOCOQ0;
OOQCCOCOQ0:i64;
begin
Result:=SQLITE_NOTFOUND;
try
case OOOCCOCOQ0 of
SQLITE_FCNTL_LOCKSTATE:begin
Byte(OQOCCOCOQ0^):=O0OCCOCOQ0^.LockType;
Result:=SQLITE_OK;
end;
SQLITE_LAST_ERRNO:begin
integer(OQOCCOCOQ0^):=O0OCCOCOQ0^.LastErrno;
Result:=SQLITE_OK;
end;
SQLITE_FCNTL_CHUNK_SIZE:begin
Result:=SQLITE_OK;
end;
SQLITE_FCNTL_SIZE_HINT:begin
O0QCCOCOQ0:=O00COOCOQ0(O0OCCOCOQ0^.Stream);
OOQCCOCOQ0:=i64(OQOCCOCOQ0^);
O0QCCOCOQ0.Size:=OOQCCOCOQ0;
Result:=SQLITE_OK;
end;
{$IFDEF LINUX_VFS}
SQLITE_FCNTL_SYNC:begin
Result:=SQLITE_OK;
end;
{$ENDIF}
SQLITE_FCNTL_PERSIST_WAL:begin
OCCQCOCOQ0(O0OCCOCOQ0,UNIXFILE_PERSIST_WAL,OQOCCOCOQ0);
Result:=SQLITE_OK;
end;
SQLITE_FCNTL_POWERSAFE_OVERWRITE:begin
OCCQCOCOQ0(O0OCCOCOQ0,UNIXFILE_PSOW,OQOCCOCOQ0);
Result:=SQLITE_OK;
end;
SQLITE_FCNTL_VFSNAME:begin
PAnsiChar(OQOCCOCOQ0^):=O0OCCOCOQ0^.Vfs^.zName;
Result:=SQLITE_OK;
end;
SQLITE_FCNTL_TEMPFILENAME:begin
OCOCCOCOQ0:=Encoding.UTF8.GetBytes(OO0COOCOQ0);
PAnsiChar(OQOCCOCOQ0^):=malloc(O0OCCOCOQ0^.Vfs^.mxPathname);
FillChar(PAnsiChar(OQOCCOCOQ0^),O0OCCOCOQ0^.Vfs^.mxPathname,0);
StrCopy(PAnsiChar(OQOCCOCOQ0^),@(OCOCCOCOQ0[0]));
Result:=SQLITE_OK;
end;
SQLITE_FCNTL_HAS_MOVED:begin
integer(OQOCCOCOQ0^):=0;
Result:=SQLITE_OK;
end;
SQLITE_FCNTL_MMAP_SIZE:begin
int64(OQOCCOCOQ0^):=0;
Result:=SQLITE_OK;
end;
end;
except
end;
end;
function OQQCCOCOQ0(OCQCCOCOQ0:psqlite3_customfile):Integer;cdecl;
begin
Result:=SQLITE_ANDROID_SECTOR_SIZE;
end;
function O0CCCOCOQ0(OOCCCOCOQ0:psqlite3_customfile):Integer;cdecl;
begin
Result:=SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN;
end;
function OQCCCOCOQ0(OCCCCOCOQ0:psqlite3_customfile):integer;
var
O000COCOQ0:psqlite3_shm;
OO00COCOQ0:psqlite3_shm_node;
begin
Result:=SQLITE_IOERR_NOMEM;
try
GetMem(O000COCOQ0,SizeOf(sqlite3_shm));
FillChar(O000COCOQ0,SizeOf(sqlite3_shm),0);
OO00COCOQ0:=OCCCCOCOQ0^.ShmNode;
if OO00COCOQ0=nil then begin
GetMem(OO00COCOQ0,SizeOf(sqlite3_shm_node));
FillChar(OO00COCOQ0,SizeOf(sqlite3_shm_node),0);
end;
O000COCOQ0^.pShmNode:=OO00COCOQ0;
inc(OO00COCOQ0^.nRef);
O000COCOQ0^.pNext:=OO00COCOQ0^.pFirst;
OO00COCOQ0^.pFirst:=O000COCOQ0;
OCCCCOCOQ0^.Shm:=O000COCOQ0;
OCCCCOCOQ0^.ShmNode:=OO00COCOQ0;
Result:=SQLITE_OK;
except
end;
end;
function OQ00COCOQ0(OC00COCOQ0:psqlite3_customfile;O0O0COCOQ0:Integer;OOO0COCOQ0:Integer;OQO0COCOQ0:Integer;OCO0COCOQ0:PPointer):Integer;cdecl;
var
O0Q0COCOQ0:psqlite3_shm;
OOQ0COCOQ0:psqlite3_shm_node;
OQQ0COCOQ0:integer;
OCQ0COCOQ0:pointer;
begin
Result:=SQLITE_OK;
try
if OC00COCOQ0^.Shm=nil then begin
Result:=OQCCCOCOQ0(OC00COCOQ0);
if Result<>SQLITE_OK then
Exit;
end;
O0Q0COCOQ0:=OC00COCOQ0^.Shm;
OOQ0COCOQ0:=O0Q0COCOQ0^.pShmNode;
OQQ0COCOQ0:=O0O0COCOQ0+1;
if OOQ0COCOQ0^.nRegion<OQQ0COCOQ0 then begin
OOQ0COCOQ0^.szRegion:=OOO0COCOQ0;
SetLength(OOQ0COCOQ0^.apRegion,OQQ0COCOQ0);
while OOQ0COCOQ0^.nRegion<OQQ0COCOQ0 do begin
GetMem(OCQ0COCOQ0,OOO0COCOQ0);
FillChar(OCQ0COCOQ0,OOO0COCOQ0,0);
OOQ0COCOQ0^.apRegion[OOQ0COCOQ0^.nRegion]:=OCQ0COCOQ0;
inc(OOQ0COCOQ0^.nRegion);
end;
end;
OCO0COCOQ0^:=OOQ0COCOQ0^.apRegion[O0O0COCOQ0];
except
end;
end;
function O0C0COCOQ0(OOC0COCOQ0:psqlite3_customfile;OQC0COCOQ0:Integer;OCC0COCOQ0:Integer;O00OCOCOQ0:Integer):Integer;cdecl;
var
OO0OCOCOQ0,
OQ0OCOCOQ0:psqlite3_shm;
OC0OCOCOQ0:psqlite3_shm_node;
O0OOCOCOQ0:u16;
begin
Result:=SQLITE_OK;
try
OO0OCOCOQ0:=OOC0COCOQ0^.Shm;
if OO0OCOCOQ0=nil then
Exit;
OC0OCOCOQ0:=OO0OCOCOQ0^.pShmNode;
O0OOCOCOQ0:=(1 shl(OQC0COCOQ0+OCC0COCOQ0))-(1 shl OQC0COCOQ0);
if(O00OCOCOQ0 and SQLITE_SHM_UNLOCK)>0 then begin
OQ0OCOCOQ0:=OC0OCOCOQ0^.pFirst;
while OQ0OCOCOQ0<>nil do begin
if OQ0OCOCOQ0<>OO0OCOCOQ0 then begin
end;
OQ0OCOCOQ0:=OQ0OCOCOQ0^.pNext;
end;
OO0OCOCOQ0^.exclMask:=OO0OCOCOQ0^.exclMask and u16(not O0OOCOCOQ0);
OO0OCOCOQ0^.sharedMask:=OO0OCOCOQ0^.sharedMask and u16(not O0OOCOCOQ0);
end
else if(O00OCOCOQ0 and SQLITE_SHM_SHARED)>0 then begin
OQ0OCOCOQ0:=OC0OCOCOQ0^.pFirst;
while OQ0OCOCOQ0<>nil do begin
if(OQ0OCOCOQ0^.exclMask and O0OOCOCOQ0)<>0 then begin
Result:=SQLITE_BUSY;
Exit;
end;
OQ0OCOCOQ0:=OQ0OCOCOQ0^.pNext;
end;
OO0OCOCOQ0^.sharedMask:=OO0OCOCOQ0^.sharedMask or O0OOCOCOQ0;
end
else begin
OQ0OCOCOQ0:=OC0OCOCOQ0^.pFirst;
while OQ0OCOCOQ0<>nil do begin
if((OQ0OCOCOQ0^.exclMask and O0OOCOCOQ0)<>0)or((OQ0OCOCOQ0^.sharedMask and O0OOCOCOQ0)<>0)then begin
Result:=SQLITE_BUSY;
Exit;
end;
OQ0OCOCOQ0:=OQ0OCOCOQ0^.pNext;
OO0OCOCOQ0^.exclMask:=OO0OCOCOQ0^.exclMask or O0OOCOCOQ0;
end;
end;
except
end;
end;
procedure OOOOCOCOQ0(OQOOCOCOQ0:psqlite3_file);cdecl;
begin
end;
function OCOOCOCOQ0(O0QOCOCOQ0:psqlite3_customfile;OOQOCOCOQ0:Integer):Integer;cdecl;
var
OQQOCOCOQ0:psqlite3_shm;
OCQOCOCOQ0:ppsqlite3_shm;
O0COCOCOQ0:psqlite3_shm_node;
OOCOCOCOQ0:integer;
begin
Result:=SQLITE_OK;
try
OQQOCOCOQ0:=O0QOCOCOQ0^.Shm;
if OQQOCOCOQ0=nil then
Exit;
O0COCOCOQ0:=OQQOCOCOQ0^.pShmNode;
OCQOCOCOQ0:=@O0COCOCOQ0^.pFirst;
while OCQOCOCOQ0^<>OQQOCOCOQ0 do
OCQOCOCOQ0^:=OCQOCOCOQ0^.pNext;
FreeMem(OQQOCOCOQ0);
O0QOCOCOQ0^.Shm:=nil;
dec(O0COCOCOQ0^.nRef);
if O0COCOCOQ0^.nRef<=0 then begin
if O0QOCOCOQ0^.ShmNode=O0COCOCOQ0 then
O0QOCOCOQ0^.ShmNode:=nil;
for OOCOCOCOQ0:=0 to O0COCOCOQ0^.nRegion-1 do
FreeMem(O0COCOCOQ0^.apRegion[OOCOCOCOQ0],O0COCOCOQ0^.szRegion);
SetLength(O0COCOCOQ0^.apRegion,0);
FreeMem(O0COCOCOQ0);
end;
except
end;
end;
function OQCOCOCOQ0(OCCOCOCOQ0:sqlite3_file;O00QQOCOQ0:i64;OO0QQOCOQ0:Integer;OQ0QQOCOQ0:PPointer):Integer;cdecl;
begin
Result:=SQLITE_OK;
end;
function OC0QQOCOQ0(O0OQQOCOQ0:sqlite3_file;OOOQQOCOQ0:i64;OQOQQOCOQ0:Pointer):Integer;cdecl;
begin
Result:=SQLITE_OK;
end;
{$ENDIF DEF CUSTOM_VFS}
initialization
{$IFDEF CODEC}
{$IFNDEF NOCODECSYNC}
OOC00QCOQ0:=TCriticalSection.Create;
{$ENDIF}
{$ENDIF CODEC}
{$IFDEF CUSTOM_VFS}
OO0CQQCOQ0.iVersion:=3;
OO0CQQCOQ0.xClose:=@OCO00OCOQ0;
OO0CQQCOQ0.xRead:=@OQQ00OCOQ0;
OO0CQQCOQ0.xWrite:=@OO0O0OCOQ0;
OO0CQQCOQ0.xTruncate:=@OQQO0OCOQ0;
OO0CQQCOQ0.xSync:=@OQCO0OCOQ0;
OO0CQQCOQ0.xFileSize:=@OO0QCOCOQ0;
OO0CQQCOQ0.xLock:=@O0OQCOCOQ0;
OO0CQQCOQ0.xUnlock:=@OOQQCOCOQ0;
OO0CQQCOQ0.xCheckReservedLock:=@O0CQCOCOQ0;
OO0CQQCOQ0.xFileControl:=@OC0CCOCOQ0;
OO0CQQCOQ0.xSectorSize:=@OQQCCOCOQ0;
OO0CQQCOQ0.xDeviceCharacteristics:=@O0CCCOCOQ0;
OO0CQQCOQ0.xShmMap:=@OQ00COCOQ0;
OO0CQQCOQ0.xShmLock:=@O0C0COCOQ0;
OO0CQQCOQ0.xShmBarrier:=@OOOOCOCOQ0;
OO0CQQCOQ0.xShmUnmap:=@OCOOCOCOQ0;
OO0CQQCOQ0.xFetch:=@OQCOCOCOQ0;
OO0CQQCOQ0.xUnfetch:=@OC0QQOCOQ0;
O00CQQCOQ0.iVersion:=3;
O00CQQCOQ0.szOsFile:=SizeOf(sqlite3_customfile);
O00CQQCOQ0.mxPathname:=MAX_PATH;
O00CQQCOQ0.pNext:=nil;
O00CQQCOQ0.zName:='';
O00CQQCOQ0.pAppData:=nil;
O00CQQCOQ0.xOpen:=@OQOCOOCOQ0;
O00CQQCOQ0.xDelete:=@OQ00OOCOQ0;
O00CQQCOQ0.xAccess:=@O0Q0OOCOQ0;
O00CQQCOQ0.xFullPathname:=@O00OOOCOQ0;
O00CQQCOQ0.xDlOpen:=@OCOOOOCOQ0;
O00CQQCOQ0.xDlError:=@OQQOOOCOQ0;
O00CQQCOQ0.xDlSym:=@OQCOOOCOQ0;
O00CQQCOQ0.xDlClose:=@OQ0Q0OCOQ0;
O00CQQCOQ0.xRandomness:=@OOOQ0OCOQ0;
O00CQQCOQ0.xSleep:=@OQCQ0OCOQ0;
O00CQQCOQ0.xCurrentTime:=nil;
O00CQQCOQ0.xGetLastError:=@O0OC0OCOQ0;
O00CQQCOQ0.xCurrentTimeInt64:=@O0QC0OCOQ0;
O00CQQCOQ0.xSetSystemCall:=@OOCC0OCOQ0;
O00CQQCOQ0.xGetSystemCall:=@OO000OCOQ0;
O00CQQCOQ0.xNextSystemCall:=@O0O00OCOQ0;
{$IFNDEF WIN_VFS}
sqlite3InitVfsFunctions(@O00CQQCOQ0);
{$ENDIF}
{$IFDEF SET_CODEC}
OQ0CQQCOQ0.xKeyV2:=@sqlite3_key_v2;
OQ0CQQCOQ0.xRekeyV2:=@sqlite3_rekey_v2;
OQ0CQQCOQ0.xActivateSee:=@sqlite3_activate_see;
OQ0CQQCOQ0.xCodecAttach:=@sqlite3CodecAttach;
OQ0CQQCOQ0.xCodecGetKey:=@sqlite3CodecGetKey;
OQ0CQQCOQ0.xCustomPragma:=@sqlite3CustomPragma;
sqlite3InitCodecFunctions(@OQ0CQQCOQ0);
{$ENDIF DEF SET_CODEC}
{$IFDEF CUSTOM_FILESTREAM}
OO00QOCOQ0:=dlopen(O0OCQQCOQ0,RTLD_LAZY);
{$IFNDEF LINUX_VFS}
OQ00QOCOQ0:=dlsym(OO00QOCOQ0,'stat'+OOOCQQCOQ0);
OC00QOCOQ0:=dlsym(OO00QOCOQ0,'stat'+OOOCQQCOQ0);
{$ENDIF}
O0O0QOCOQ0:=dlsym(OO00QOCOQ0,'open');
OOO0QOCOQ0:=dlsym(OO00QOCOQ0,'lseek');
OQO0QOCOQ0:=dlsym(OO00QOCOQ0,'read');
OCO0QOCOQ0:=dlsym(OO00QOCOQ0,'write');
O0Q0QOCOQ0:=dlsym(OO00QOCOQ0,'unlink');
OOQ0QOCOQ0:=dlsym(OO00QOCOQ0,'close');
OQQ0QOCOQ0:=dlsym(OO00QOCOQ0,'ftruncate');
{$ENDIF}
sqlite3_initialize;
{$ENDIF DEF CUSTOM_VFS}
{$IFDEF CODEC_CALLBACK}
sqlite3CodecCallbackRec.xKeyV2:=@sqlite3_key_v2;
sqlite3CodecCallbackRec.xRekeyV2:=@sqlite3_rekey_v2;
sqlite3CodecCallbackRec.xActivateSee:=@sqlite3_activate_see;
sqlite3CodecCallbackRec.xCodecAttach:=@sqlite3CodecAttach;
sqlite3CodecCallbackRec.xCodecGetKey:=@sqlite3CodecGetKey;
sqlite3CodecCallbackRec.xCustomPragma:=@sqlite3CustomPragma;
{$IFDEF USE_CUSTOM_LOCALTIME}
sqlite3CodecCallbackRec.xCustomLocaltime:=@localtime;
{$ENDIF}
sqlite3InitCodecCallback(@sqlite3CodecCallbackRec);
{$ENDIF}
finalization
{$IFDEF CODEC}
{$IFNDEF NOCODECSYNC}
FreeAndNil(OOC00QCOQ0);
{$ENDIF}
{$ENDIF DEF CODEC}
{$IFDEF CUSTOM_VFS}
sqlite3_shutdown;
{$IFDEF CUSTOM_FILESTREAM}
dlclose(OO00QOCOQ0);
{$ENDIF}
{$ENDIF DEF CUSTOM_VFS}
{$ENDIF NDEF NOSTATIC}
end.
