
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Oracle Call Interface
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraCallUni;

{$O-}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry, {$IFNDEF LITE}MTSCall,{$ENDIF}
{$ENDIF}
  SysUtils, CLasses, SyncObjs, {$IFDEF VER6P}Types,{$ENDIF}
{$IFDEF CLR}
  System.IO, System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes, MemUtils, DAConsts;

const
{ external data types }
  SQLT_UNK            = 0;        // UNKNOWN
  SQLT_CHR            = 1;        // (ORANET TYPE) character string
  SQLT_NUM            = 2;        // (ORANET TYPE) oracle numeric
  SQLT_INT            = 3;        // (ORANET TYPE) integer
  SQLT_FLT            = 4;        // (ORANET TYPE) Floating point number
  SQLT_STR            = 5;        // zero terminated string
  SQLT_VNU            = 6;        // NUM with preceding length byte
  SQLT_PDN            = 7;        // (ORANET TYPE) Packed Decimal Numeric
  SQLT_LNG            = 8;        // long
  SQLT_VCS            = 9;        // Variable character string
  SQLT_NON            = 10;       // Null/empty PCC Descriptor entry
  SQLT_RID            = 11;       // rowid
  SQLT_DAT            = 12;       // date in oracle format
  SQLT_VBI            = 15;       // binary in VCS format
  SQLT_BFLOAT         = 21;       // Native Binary float
  SQLT_BDOUBLE        = 22;       // Native binary double
  SQLT_BIN            = 23;       // binary data(DTYBIN)
  SQLT_LBI            = 24;       // long binary
  SQLT_UND            = 25;       // UNDEFINED
  SQLT_OPAQUE         = 58;       // internal XML
  SQLT_UIN            = 68;       // unsigned integer
  SQLT_SLS            = 91;       // Display sign leading separate
  SQLT_LVC            = 94;       // Longer longs (char)
  SQLT_LVB            = 95;       // Longer long binary
  SQLT_AFC            = 96;       // Ansi fixed char
  SQLT_AVC            = 97;       // Ansi Var char
  SQLT_IBFLOAT        = 100;      // binary float canonical
  SQLT_IBDOUBLE       = 101;      // binary double canonical
  SQLT_CUR            = 102;      // cursor  type
  SQLT_RDD            = 104;      // rowid descriptor
  SQLT_LAB            = 105;      // label type
  SQLT_OSL            = 106;      // oslabel type
  SQLT_NTY            = 108;      // named object type
  SQLT_REF            = 110;      // ref type
  SQLT_CLOB           = 112;      // character lob
  SQLT_BLOB           = 113;      // binary lob
  SQLT_BFILEE         = 114;      // binary file lob
  SQLT_CFILEE         = 115;      // character file lob
  SQLT_RSET           = 116;      // result set type
  SQLT_NCO            = 122;      // named collection type (varray or nested table) table
  SQLT_VARRAY         = 123;      // ---- // ----                                   varray
  SQLT_VST            = 155;      // OCIString type
  SQLT_ODT            = 156;      // OCIDate type
  SQLT_FILE           = SQLT_BFILEE; // binary file lob
  SQLT_CFILE          = SQLT_CFILEE;
  SQLT_BFILE          = SQLT_BFILEE;
  SQLT_DATE           = 184;      // ANSI Date
  SQLT_TIME           = 185;      // TIME
  SQLT_TIME_TZ        = 186;      // TIME WITH TIME ZONE
  SQLT_TIMESTAMP      = 187;      // TIMESTAMP
  SQLT_TIMESTAMP_TZ   = 188;      // TIMESTAMP WITH TIME ZONE
  SQLT_INTERVAL_YM    = 189;      // INTERVAL YEAR TO MONTH
  SQLT_INTERVAL_DS    = 190;      // INTERVAL DAY TO SECOND
  SQLT_URID           = 208;      // UROWID
  SQLT_TIMESTAMP_LTZ  = 232;      // TIMESTAMP WITH LOCAL TZ
  SQLT_REC            = 250;      // PLSQL record
  SQLT_TAB            = 251;      // PLSQL indexed table
  SQLT_BOL            = 252;      // PLSQL boolean

type

  //TChangeNotify* type declarations moved here because of the cbuilder bug

  TChangeNotifyOperation = (cnoInsert, cnoUpdate, cnoDelete, cnoAllRows, cnoAlter, cnoDrop);
  {$NODEFINE TChangeNotifyOperation}

  TChangeNotifyDMLOperation = cnoInsert..cnoDelete;
  {$NODEFINE TChangeNotifyDMLOperation}

  TChangeNotifyOperations = set of TChangeNotifyOperation;
  {$NODEFINE TChangeNotifyOperations}

  TChangeNotifyDMLOperations = set of TChangeNotifyDMLOperation;
  {$NODEFINE TChangeNotifyDMLOperations}

  {$IFNDEF UNIDACPRO}
  (*$HPPEMIT 'namespace Oracall {'*)
  {$ELSE}
  (*$HPPEMIT 'namespace Oracalluni {'*)
  {$ENDIF}
  (*$HPPEMIT '  #pragma option push -b-'*)
  (*$HPPEMIT '  enum TChangeNotifyOperation { cnoInsert, cnoUpdate, cnoDelete, cnoAllRows, cnoAlter, cnoDrop };'*)
  (*$HPPEMIT '  typedef Set<TChangeNotifyOperation, cnoInsert, cnoDrop> TChangeNotifyOperations;'*)
  (*$HPPEMIT '  typedef TChangeNotifyOperation TChangeNotifyDMLOperation;'*)
  (*$HPPEMIT '  typedef Set<TChangeNotifyDMLOperation, cnoInsert, cnoDelete> TChangeNotifyDMLOperations;'*)
  (*$HPPEMIT '  #pragma option pop'*)
  (*$HPPEMIT '}'*)//namespace Oraclasses

  eword = integer;
  uword = cardinal;
  sword = integer;

  eb1   = shortint;
  ub1   = byte;
  sb1   = shortint;
{$IFDEF CLR}
  pub1  = IntPtr;
  psb1  = IntPtr;
  ppub1 = IntPtr;
  pppub1 = IntPtr;

  pub2  = IntPtr;
  psb2  = IntPtr;
  ppub2 = IntPtr;

  psb4  = IntPtr;
  pub4  = IntPtr;
  ppub4 = IntPtr;

  pub8  = IntPtr;
  ppub8 = IntPtr;

  pbool = IntPtr;

  PAnsiChar = string;
{$ELSE}
  pub1  = ^ub1;
  psb1  = ^sb1;
  ppub1 = ^pub1;
  pppub1 = ^ppub1;

  pub2  = ^ub2;
  psb2  = ^sb2;
  ppub2 = ^pub2;

  psb4  = ^sb4;
  pub4  = ^ub4;
  ppub4 = ^pub4;

  pub8  = ^ub8;
  ppub8 = ^pub8;

  PPointer = ^IntPtr;
  pbool = ^tbool;
{$ENDIF}

  eb2   = smallint;
  ub2   = word;
  sb2   = smallint;

  eb4   = integer;
  ub4   = cardinal;
  sb4   = integer;

  ub8   = int64;

  tbool = integer;

{$IFDEF VER5}
  PPChar   = ^PChar;
{$ENDIF}

// ORACLE 7.3 specific

const
  CDA_SIZE = 64;
  HDA_SIZE = 256;

// internal/external datatype codes

  UNKNOWN_TYPE  = 0;
  VARCHAR2_TYPE = 1;
  NUMBER_TYPE   = 2;
  INTEGER_TYPE  = 3;
  FLOAT_TYPE    = 4;
  STRING_TYPE   = 5;
  LONG_TYPE     = 8;
  ROWID_TYPE    = 11;
  DATE_TYPE     = 12;
  RAW_TYPE      = 23;
  LONGRAW_TYPE  = 24;
  CHAR_TYPE     = 96;
  CHARZ_TYPE    = 97;
  CURSOR_TYPE   = 102;

// ORACLE error codes

  OCI_VAR_NOT_IN_LIST     = -303;  //1007;
  OCI_NO_DATA_FOUND       = 4;     //1403;
  OCI_NULL_VALUE_RETURNED = 1405;
  OCI_BLOCKED             = -3123;
  OCI_CONNECTION_BUSY     = -3127;
// Session error
  OCI_SESSION_KILLED      = 28;
  OCI_NOT_LOGGEDON        = 1012;
  OCI_EOF_COMMUNICATION   = 3113;
  OCI_NOT_CONNECTED       = 3114;
  OCI_NO_INTERFACE        = 3121;

  OCI_STILL_IS_PIECE      = -3130;
  OCI_STILL_IS_PIECE1     = -3129;
  OCI_BREAKED             = -1013;

// SQL function codes

  SQL_UNKNOWN = 0;
  SQL_CREATE_TABLE = 1;
  SQL_SET_ROLE = 2;
  SQL_INSERT  = 3;
  SQL_SELECT  = 4;
  SQL_UPDATE  = 5;
  SQL_DROP_ROLE = 6;
  SQL_DROP_VIEW = 7;
  SQL_DROP_TABLE = 8;
  SQL_DELETE  = 9;
  SQL_CREATE_VIEW = 10;
  SQL_DROP_USER = 11;
  SQL_CREATE_ROLE = 12;
  SQL_CREATE_SEQUENCE = 13;
  SQL_ALTER_SEQUENCE = 14;
  SQL_DROP_SEQUENCE = 16;
  SQL_CREATE_SCHEMA = 17;
  SQL_CREATE_CLUSTER = 18;
  SQL_CREATE_USER = 19;
  SQL_CREATE_INDEX = 20;
  SQL_DROP_INDEX = 21;
  SQL_DROP_CLUSTER = 22;
  SQL_VALIDATE_INDEX = 23;
  SQL_CREATE_PROCEDURE = 24;
  SQL_ALTER_PROCEDURE = 25;
  SQL_ALTER_TABLE = 26;
  SQL_EXPLAIN = 27;
  SQL_GRANT = 28;
  SQL_REVOKE = 29;
  SQL_CREATE_SYNONYM = 30;
  SQL_DROP_SYNONYM = 31;
  SQL_ALTER_SYSTEM_SWITCH = 32;
  SQL_SET_TRANSACTION = 33;
  SQL_PLSQL = 34;
  SQL_LOCK = 35;
  SQL_RENAME = 37;
  SQL_COMMENT = 38;
  SQL_AUDIT = 39;
  SQL_NOAUDIT = 40;
  SQL_ALTER_INDEX = 41;
  SQL_CREATE_EXTERNAL_DATABASE = 42;
  SQL_DROP_EXTERNAL_DATABASE = 43;
  SQL_CREATE_DATABASE = 44;
  SQL_ALTER_DATABASE = 45;
  SQL_CREATE_ROLLBACK_SEGMENT = 46;
  SQL_ALTER_ROLLBACK_SEGMENT = 47;
  SQL_DROP_ROLLBACK_SEGMENT = 48;
  SQL_CREATE_TABLESPACE = 49;
  SQL_ALTER_TABLESPACE = 50;
  SQL_DROP_TABLESPACE = 51;
  SQL_ALTER_SESSION = 52;
  SQL_ALTER_USER = 53;
  SQL_COMMIT  = 54;
  SQL_ROLLBACK = 55;
  SQL_SAVEPOINT = 56;
  SQL_CREATE_CONTROL_FILE = 57;
  SQL_ALTER_TRACING = 58;
  SQL_CREATE_TRIGGER = 59;
  SQL_ALTER_TRIGGER = 60;
  SQL_DROP_TRIGGER = 61;
  SQL_ANALYZE_TABLE = 62;
  SQL_ANALYZE_INDEX = 63;
  SQL_ANALYZE_CLUSTER = 64;
  SQL_CREATE_PROFILE = 65;
  SQL_DROP_PROFILE = 66;
  SQL_ALTER_PROFILE = 67;
  SQL_DROP_PROCEDURE = 68;
  SQL_ALTER_RESOURCE_COST = 70;
  SQL_CREATE_SNAPSHOT_LOG = 71;
  SQL_ALTER_SNAPSHOT_LOG = 72;
  SQL_DROP_SNAPSHOT_LOG = 73;
  SQL_CREATE_SNAPSHOT = 74;
  SQL_ALTER_SNAPSHOT = 75;
  SQL_DROP_SNAPSHOT = 76;
  SQL_CREATE_TYPE = 77;
  SQL_DROP_TYPE = 78;
  SQL_ALTER_ROLE = 79;
  SQL_ALTER_TYPE = 80;
  SQL_CREATE_TYPE_BODY = 81;
  SQL_ALTER_TYPE_BODY = 82;
  SQL_DROP_TYPE_BODY = 83;
  SQL_DROP_LIBRARY = 84;
  SQL_TRUNCATE_TABLE = 85;
  SQL_TRUNCATE_CLUSTER = 86;
  SQL_CREATE_BITMAPFILE = 87;
  SQL_ALTER_VIEW = 88;
  SQL_DROP_BITMAPFILE = 89;
  SQL_SET_CONSTRAINTS = 90;
  SQL_CREATE_FUNCTION = 91;
  SQL_ALTER_FUNCTION = 92;
  SQL_DROP_FUNCTION = 93;
  SQL_CREATE_PACKAGE = 94;
  SQL_ALTER_PACKAGE = 95;
  SQL_DROP_PACKAGE = 96;
  SQL_CREATE_PACKAGE_BODY = 97;
  SQL_ALTER_PACKAGE_BODY = 98;
  SQL_DROP_PACKAGE_BODY = 99;
  SQL_CREATE_DIRECTORY = 157;
  SQL_DROP_DIRECTORY = 158;
  SQL_CREATE_LIBRARY = 159;
  SQL_CREATE_JAVA = 160;
  SQL_ALTER_JAVA = 161;
  SQL_DROP_JAVA = 162;
  SQL_CREATE_OPERATOR = 163;
  SQL_CREATE_INDEXTYPE = 164;
  SQL_DROP_INDEXTYPE = 165;
  SQL_ALTER_INDEXTYPE = 166;
  SQL_DROP_OPERATOR = 167;
  SQL_ASSOCIATE_STATISTICS = 168;
  SQL_DISASSOCIATE_STATISTICS = 169;
  SQL_CALL_METHOD = 170;
  SQL_CREATE_SUMMARY = 171;
  SQL_ALTER_SUMMARY = 172;
  SQL_DROP_SUMMARY = 173;
  SQL_CREATE_DIMENSION = 174;
  SQL_ALTER_DIMENSION = 175;
  SQL_DROP_DIMENSION = 176;
  SQL_CREATE_CONTEXT = 177;
  SQL_DROP_CONTEXT = 178;
  SQL_ALTER_OUTLINE = 179;
  SQL_CREATE_OUTLINE = 180;
  SQL_DROP_OUTLINE = 181;
  SQL_UPDATE_INDEXES = 182;
  SQL_ALTER_OPERATOR = 183;
  SQL_ALTER_SYNONYM = 192;
  SQL_PURGE_USER_RECYCLEBIN = 197;
  SQL_PURGE_DBA_RECYCLEBIN = 198;
  SQL_PURGE_TABLESAPCE = 199;
  SQL_PURGE_TABLE = 200;
  SQL_PURGE_INDEX = 201;
  SQL_UNDROP_OBJECT = 202;
  SQL_FLASHBACK_DATABASE = 204;
  SQL_FLASHBACK_TABLE = 205;
  SQL_CREATE_RESTORE_POINT = 206;
  SQL_DROP_RESTORE_POINT = 207;
  SQL_PROXY_AUTHENTICATION_ONLY = 208;
  SQL_DECLARE_REWRITE_EQUIVALENCE = 209;
  SQL_ALTER_REWRITE_EQUIVALENCE = 210;
  SQL_DROP_REWRITE_EQUIVALENCE = 211;

  FC_OOPEN = 14;

{ OCI Environment Modes for opinit call }

  OCI_EV_DEF = 0;                    // default single-threaded environment
  OCI_EV_TSF = 1;                    // thread-safe environment

{ OCI Logon Modes for olog call }

  OCI_LM_DEF = 0;                                   // default login
  OCI_LM_NBL = 1;                                   // non-blocking logon

{ Piece Definitions }

  OCI_ONE_PIECE   = 0;                 // there or this is the only piece
  OCI_FIRST_PIECE = 1;                 // the first of many pieces
  OCI_NEXT_PIECE  = 2;                 // the next of many pieces
  OCI_LAST_PIECE  = 3;                 // the last piece of this column

{ for parse }

  OCI_PARSE_NODEFER = 0;
  OCI_PARSE_DEFER = 1;
  OCI_LANG_V6 = 0;
  OCI_LANG_NORM = 1;
  OCI_LANG_V7 = 2;

{ CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information }
  SQLCS_IMPLICIT  = 1; //* for CHAR, VARCHAR2, CLOB w/o a specified set */
  SQLCS_NCHAR     = 2; //* for NCHAR, NCHAR VARYING, NCLOB */
  SQLCS_EXPLICIT  = 3; //* for CHAR, etc, with "CHARACTER SET ..." syntax */
  SQLCS_FLEXIBLE  = 4; //* for PL/SQL "flexible" parameters */
  SQLCS_LIT_NULL  = 5; //* for typecheck of NULL and empty_clob() lits */

type
{$IFDEF CLR}
  PTRD = record
  private
    Ptr: IntPtr;
    function GetUB1Property(Index: Integer): ub1;
    procedure SetUB1Property(Index: Integer; Value: ub1);
    function GetUB2Property(Index: Integer): ub2;
    procedure SetUB2Property(Index: Integer; Value: ub2);
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);

  public
    property rcs4 : ub4 index 0 read GetUB4Property write SetUB4Property;
    property rcs5 : ub2 index 4 read GetUB2Property write SetUB2Property;
    property rcs6 : ub1 index 6 read GetUB1Property write SetUB1Property;

    class operator Implicit(AValue: IntPtr): PTRD;
    class operator Implicit(AValue: PTRD): IntPtr;
  end;
{$ENDIF}
  TRD = packed record
    rcs4 : ub4;   // obj num
    rcs5 : ub2;   // file num
    rcs6 : ub1;
  end;
{$IFNDEF CLR}
  PTRD = ^TRD;
{$ENDIF}

{ rowid structure }

{$IFDEF CLR}
  PRowId7 = record
  private
    Ptr: IntPtr;
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);
    function GetTRDProperty(Index: Integer): PTRD;

  public
    property rd   : PTRD index 0 read GetTRDProperty;
    property rcs7 : ub4 index 7 read GetUB4Property write SetUB4Property;
    property rcs8 : ub4 index 11 read GetUB4Property write SetUB4Property;

    class operator Implicit(AValue: IntPtr): PRowId7;
    class operator Implicit(AValue: PRowId7): IntPtr;
  end;
{$ENDIF}
 TRowId7 = packed record
    rd   : TRD;
    rcs7 : ub4;   // block num
    rcs8 : ub4;   // for Oracle7 - ub2;   // slot num
  end;
type
{$IFNDEF CLR}
  PRowId7 = ^TRowId7;
{$ENDIF}

{ The cda_head struct is strictly PRIVATE.  It is used
   internally only. Do not use this struct in OCI programs. }

  TCDAHead = packed record
    v2_rc : sb2;
    ft    : ub2;
    rpc   : ub4;
    peo   : ub2;
    fc    : ub1;
    rcs1  : ub1;
    rc    : ub2;
    wrn   : ub1;
    rcs2  : ub1;
    rcs3  : sword;
    rid   : TRowId7;
    ose   : sword;
    chk   : ub1;
    rcsp  : IntPtr;
  end;

const
{$IFDEF CLR}
  SizeOfTCDAHead = 44;
{$ELSE}
  SizeOfTCDAHead = sizeof(TCDAHead);
{$ENDIF}

type
{ the real CDA, padded to CDA_SIZE bytes in size }

  TCDA = packed record
    v2_rc : sb2;         { V2 return code }
    ft    : ub2;         { SQL function type }
    rpc   : ub4;         { rows processed count }
    peo   : ub2;         { parse error offset }
    fc    : ub1;         { OCI function code }
    rcs1  : ub1;         { filler area }
    rc    : ub2;         { V7 return code }
    wrn   : ub1;         { warning flags }
    rcs2  : ub1;         { reserved }
    rcs3  : sword;       { reserved }
    rid   : TRowId7;     { rowid }
    ose   : sword;       { OSD dependent error }
    chk   : ub1;
    rcsp  : IntPtr;     { pointer to reserved area }
    rcs9  : array [0 .. CDA_SIZE - SizeOfTCDAHead - 1] of ub1; { filler }
  end;
{$IFDEF CLR}
  PCDA = packed record
  private
    Ptr: IntPtr;

    function GetSB2Property(Index: Integer): sb2;
    procedure SetSB2Property(Index: Integer; Value: sb2);
    function GetSB4Property(Index: Integer): sb4;
    procedure SetSB4Property(Index: Integer; Value: sb4);
    function GetUB1Property(Index: Integer): ub1;
    procedure SetUB1Property(Index: Integer; Value: ub1);
    function GetUB2Property(Index: Integer): ub2;
    procedure SetUB2Property(Index: Integer; Value: ub2);
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);
    function GetIntPtrProperty(Index: Integer): IntPtr;
    procedure SetIntPtrProperty(Index: Integer; Value: IntPtr);
    function GetTRowId7Property(Index: Integer): PRowId7;
    procedure SetArrayProperty(Index: Integer; ArrayInd: integer; Value: ub1);
    function GetArrayProperty(Index: Integer; ArrayInd: integer): ub1;

  public
    { V2 return code }
    property v2_rc: sb2 index 0 read GetSB2Property write SetSB2Property;
    { SQL function type }
    property ft: ub2 index 2 read GetUB2Property write SetUB2Property;
    { rows processed count }
    property rpc: ub4 index 4 read GetUB4Property write SetUB4Property;
    { parse error offset }
    property peo: ub2 index 8 read GetUB2Property write SetUB2Property;
    { OCI function code }
    property fc: ub1 index 10 read GetUB1Property write SetUB1Property;
    { filler area }
    property rcs1: ub1 index 11 read GetUB1Property write SetUB1Property;
    { V7 return code }
    property rc: ub2 index 12 read GetUB2Property write SetUB2Property;
    { warning flags }
    property wrn: ub1 index 14 read GetUB1Property write SetUB1Property;
    { reserved }
    property rcs2: ub1 index 15 read GetUB1Property write SetUB1Property;
    { reserved }
    property rcs3: sb4 index 16 read GetSB4Property write SetSB4Property;
    { rowid }
    property rid: PRowId7 index 20 read GetTRowId7Property;
    { OSD dependent error }
    property ose: sb4 index 35 read GetSB4Property write SetSB4Property;
    property chk: ub1 index 39 read GetUB1Property write SetUB1Property;
    { pointer to reserved area }
    property rcsp: IntPtr index 40 read GetIntPtrProperty write SetIntPtrProperty;
    { filler }
    property rcs9[i: integer]: ub1  index 44 read GetArrayProperty write SetArrayProperty;

    class operator Implicit(AValue: IntPtr): PCDA;
    class operator Implicit(AValue: PCDA): IntPtr;
  end;
{$ELSE}
  PCDA = ^TCDA;
{$ENDIF}

{ the logon data area (LDA) is the same shape as the CDA }

  TLDA = TCDA;
  PLDA = PCDA;

{ host data area }

  THDA = packed array [1 .. HDA_SIZE] of byte;
{$IFNDEF CLR}
  PHDA = ^THDA;
{$ELSE}
  PHDA = IntPtr;
{$ENDIF}

{ Declare the OCI functions }

  _obindps = function (cursor: IntPtr; opcode: ub1; sqlvar: PAnsiChar; sqlvl: sb4;
               pvctx: pub1; progvl: sb4; ftype: sword; scale: sword; indp: psb2;
               alen: pub2; arcode: pub2; pv_skip: sb4; ind_skip: sb4; alen_skip: sb4;
               rc_skip: sb4; maxsiz: ub4; cursiz: pub4; fmt: IntPtr; fmtl: sb4;
               fmtt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _obreak = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ocan = function (cursor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oclose = function (cursor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ocof = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ocom = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ocon = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _odefinps = function (cursor: IntPtr; opcode: ub1; pos: sword; bufctx: IntPtr;
                bufl: sb4; ftype: sword; scale: sword; indp: psb2; fmt: IntPtr;
                fmtl: sb4; fmtt: sword; rlen: pub2; rcode: pub2; pv_skip: sb4;
                ind_skip: sb4; alen_skip: sb4; rc_skip: sb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _odessp = function (lda: IntPtr; objnam: PAnsiChar; onlen: NativeUInt; rsv1: pub1;
              rsv1ln: NativeUInt; rsv2: pub1; rsv2ln: NativeUInt; ovrld: pub2; pos: pub2;
              level: pub2; argnam: IntPtr; arnlen: pub2; dtype: pub2; defsup: pub1;
              mode: pub1; dtsiz: pub4; prec: psb2; scale: psb2; radix: pub1;
              spare: pub4; var arrsiz: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _odescr = function (cursor: IntPtr; pos: sword; var dbsize: sb4; var dbtype: sb2;
              cbuf: IntPtr; var cbufl: sb4; var dsize: sb4; var prec: sb2; var scale: sb2;
              var nullok: sb2): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oerhms = function (lda: IntPtr; rcode: sb2; buf: IntPtr; bufsiz: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oermsg = function (rcode: sb2; buf: PAnsiChar): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oexec = function (cursor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oexfet = function (cursor: IntPtr; nrows: ub4; cancel: sword; exact: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oexn = function (cursor: IntPtr; iters: sword; rowoff: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ofen = function (cursor: IntPtr; nrows: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ofetch = function (cursor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oflng = function (cursor: IntPtr; pos: sword; buf: pub1; bufl: sb4; dtype: sword;
             retl: pub4; offset: sb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ogetpi = function (cursor: IntPtr; var piecep: ub1; var ctxpp: IntPtr; var iterp: ub4;
              var indexp: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oopt = function (cursor: IntPtr; rbopt: sword; waitopt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _opinit = function (mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _olog = function (lda: IntPtr; hda: PHDA; uid: PAnsiChar; uidl: sword; pswd: PAnsiChar;
            pswdl: sword; conn: PAnsiChar; connl: sword; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ologof = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oopen = function (cursor: IntPtr; lda: IntPtr; dbn: IntPtr; dbnl: sword; arsize: sword;
             uid: IntPtr; uidl: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _oparse = function (cursor: IntPtr; sqlstm: PAnsiChar; sqllen: sb4; defflg: sword;
              lngflg: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _orol = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _osetpi = function (cursor: IntPtr; piece: ub1; bufp: IntPtr; var lenp: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _sqlld2 = procedure (lda: IntPtr; cname: PAnsiChar; cnlen: psb4); {$IFNDEF CLR} cdecl; {$ENDIF}

  _sqllda = procedure (lda: IntPtr); {$IFNDEF CLR} cdecl; {$ENDIF}

  _onbset = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _onbtst = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _onbclr = function (lda: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _ognfd  = function(lda: IntPtr; fdp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  { OBSOLETE DEFINE CALLS }

  _obndra = function (cursor: IntPtr; sqlvar: PAnsiChar; sqlvl: sword;
                 progv: pub1; progvl: sword; ftype: sword; scale: sword;
                 indp: psb2; alen: pub2; arcode: pub2; maxsiz: ub4;
                 cursiz: pub4; fmt: IntPtr; fmtl: sword; fmtt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _obndrn = function (cursor: IntPtr; sqlvn: sword; progv: pub1; progvl: sword;
              ftype: sword; scale: sword; indp: psb2; fmt: PAnsiChar; fmtl: sword;
              fmtt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _obndrv = function (cursor: IntPtr; sqlvar: PAnsiChar; sqlvl: sword; progv: IntPtr;
              progvl: sword; ftype: sword; scale: sword; indp: psb2; fmt: IntPtr;
              fmtl: sword; fmtt: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _odefin = function (cursor: IntPtr; pos: sword; buf: IntPtr; bufl: sword; ftype: sword;
              scale: sword; indp: psb2; fmt: IntPtr; fmtl: sword; fmtt: sword;
              rlen: pub2; rcode: pub2): sword; {$IFNDEF CLR} cdecl; {$ENDIF}



// ORACLE 8.0 specific

const
{ Modes }

  OCI_DEFAULT              = $00;   // the default value for parameters and attributes
  OCI_THREADED             = $01;   // the application is in threaded environment
  OCI_OBJECT               = $02;   // the application is in object environment
  OCI_EVENTS               = $04;   // the application is enabled for events
  OCI_ENV_NO_MUTEX         = $08;   // the environment handle will not be
  OCI_SHARED               = $10;   // the application is in shared mode
  OCI_RESERVED2            = $20;
  OCI_NO_UCB               = $40;   // No user callback called during init
  OCI_NO_MUTEX             = $80;   // protected by a mutex internally
  OCI_SHARED_EXT           = $100;  // Used for shared forms
  OCI_CACHE                = $200;  // Used by iCache
  OCI_NO_CACHE             = $400;  // Turn off iCache mode, used by iCache
  OCI_UTF16                = $4000; // mode for all UTF16 metadata
  OCI_NEW_LENGTH_SEMANTICS      = $00020000; // Adopt new length semantics
  OCI_NCHAR_LITERAL_REPLACE_ON  = $00400000;
  OCI_NCHAR_LITERAL_REPLACE_OFF = $00800000;

  OCI_SESSGET_SPOOL        = $01;   // enables OCI session pooling
  OCI_SESSGET_STMTCACHE    = $04;   // Use statement cache

  OCI_SPC_REINITIALIZE     = $01;   // Reinitialize the session pool
  OCI_SPC_HOMOGENEOUS      = $02;   // Session pool is homogeneneous
  OCI_SPC_STMTCACHE        = $04;   // Session pool has stmt cache

{ Handle Types }
                                  // handle types range from 1 - 49 */
  OCI_HTYPE_FIRST          = 1;    // start value of handle type
  OCI_HTYPE_ENV            = 1;    // environment handle
  OCI_HTYPE_ERROR          = 2;    // error handle
  OCI_HTYPE_SVCCTX         = 3;    // service handle
  OCI_HTYPE_STMT           = 4;    // statement handle
  OCI_HTYPE_BIND           = 5;    // bind handle
  OCI_HTYPE_DEFINE         = 6;    // define handle
  OCI_HTYPE_DESCRIBE       = 7;    // describe handle
  OCI_HTYPE_SERVER         = 8;    // server handle
  OCI_HTYPE_SESSION        = 9;    // authentication handle
  OCI_HTYPE_AUTHINFO       = OCI_HTYPE_SESSION;
  OCI_HTYPE_TRANS          = 10;   // transaction handle
  OCI_HTYPE_COMPLEXOBJECT  = 11;   // complex object retrieval handle
  OCI_HTYPE_SECURITY       = 12;   // security handle
  OCI_HTYPE_SUBSCRIPTION   = 13;   // subscription handle
  OCI_HTYPE_DIRPATH_CTX    = 14;   // direct path context
  OCI_HTYPE_DIRPATH_COLUMN_ARRAY = 15;  // direct path column array
  OCI_HTYPE_DIRPATH_STREAM = 16;   // direct path stream
  OCI_HTYPE_PROC           = 17;   // process handle
  OCI_HTYPE_LAST           = 17;   // last value of a handle type
  OCI_HTYPE_SPOOL          = 27;   // session pool handle

{ Descriptor Types }
                                    // descriptor values range from 50 - 255
  OCI_DTYPE_FIRST             = 50; // start value of descriptor type
  OCI_DTYPE_LOB               = 50; // lob  locator
  OCI_DTYPE_SNAP              = 51; // snapshot descriptor
  OCI_DTYPE_RSET              = 52; // result set descriptor
  OCI_DTYPE_PARAM             = 53; // a parameter descriptor obtained from ocigparm
  OCI_DTYPE_ROWID             = 54; // rowid descriptor
  OCI_DTYPE_COMPLEXOBJECTCOMP = 55; // complex object retrieval descriptor
  OCI_DTYPE_FILE              = 56; // File Lob locator
  OCI_DTYPE_AQENQ_OPTIONS     = 57; // enqueue options
  OCI_DTYPE_AQDEQ_OPTIONS     = 58; // dequeue options
  OCI_DTYPE_AQMSG_PROPERTIES  = 59; // message properties
  OCI_DTYPE_AQAGENT           = 60; // aq agent
  OCI_DTYPE_INTERVAL_YM       = 62; // Interval year month
  OCI_DTYPE_INTERVAL_DS       = 63; // Interval day second
  OCI_DTYPE_AQNFY_DESCRIPTOR  = 64; // AQ notify descriptor
  OCI_DTYPE_DATE              = 65; // Date
  OCI_DTYPE_TIME              = 66; // Time
  OCI_DTYPE_TIME_TZ           = 67; // Time with timezone
  OCI_DTYPE_TIMESTAMP         = 68; // Timestamp
  OCI_DTYPE_TIMESTAMP_TZ      = 69; // Timestamp with timezone
  OCI_DTYPE_TIMESTAMP_LTZ     = 70; // Timestamp with local tz
  OCI_DTYPE_UCB               = 71; // user callback descriptor
  OCI_DTYPE_SRVDN             = 72; // server DN list descriptor
  OCI_DTYPE_SIGNATURE         = 73; // signature
  OCI_DTYPE_XML_STREAM        = 74; // reserved for internal use
  OCI_DTYPE_AQLIS_OPTIONS     = 75; // AQ listen options
  OCI_DTYPE_AQLIS_MSG_PROPERTIES = 76; // AQ listen msg props
  OCI_DTYPE_CHDES             = 77; // Top level change notification desc
  OCI_DTYPE_TABLE_CHDES       = 78; // Table change descriptor
  OCI_DTYPE_ROW_CHDES         = 79; // Row change descriptor

{ Object Ptr Types }

  OCI_OTYPE_NAME           = 1;    // object name
  OCI_OTYPE_REF            = 2;    // REF to TDO
  OCI_OTYPE_PTR            = 3;    // PTR to TDO

{ Attribute Types }

  OCI_ATTR_FNCODE          = 1;
  OCI_ATTR_OBJECT          = 2;
  OCI_ATTR_NONBLOCKING_MODE  = 3;
  OCI_ATTR_SQLCODE         = 4;
  OCI_ATTR_ENV             = 5;
  OCI_ATTR_SERVER          = 6;
  OCI_ATTR_SESSION         = 7;
  OCI_ATTR_TRANS           = 8;
  OCI_ATTR_ROW_COUNT       = 9;
  OCI_ATTR_SQLFNCODE       = 10;
  OCI_ATTR_PREFETCH_ROWS   = 11;
  OCI_ATTR_NESTED_PREFETCH_ROWS  = 12;
  OCI_ATTR_PREFETCH_MEMORY = 13;
  OCI_ATTR_NESTED_PREFETCH_MEMORY  = 14;
  OCI_ATTR_CHAR_COUNT      = 15;    // this specifies the bind and define size in characters
  OCI_ATTR_PDSCL           = 16;
  OCI_ATTR_FSPRECISION     = OCI_ATTR_PDSCL;
  OCI_ATTR_PDFMT           = 17;
  OCI_ATTR_PARAM_COUNT     = 18;
  OCI_ATTR_ROWID           = 19;
  OCI_ATTR_CHARSET         = 20;
  OCI_ATTR_NCHAR           = 21;
  OCI_ATTR_USERNAME        = 22;    // username attribute
  OCI_ATTR_PASSWORD        = 23;    // password attribute
  OCI_ATTR_STMT_TYPE       = 24;    // statement type
  OCI_ATTR_INTERNAL_NAME   = 25;
  OCI_ATTR_EXTERNAL_NAME   = 26;
  OCI_ATTR_XID             = 27;
  OCI_ATTR_TRANS_LOCK      = 28;
  OCI_ATTR_TRANS_NAME      = 29;
  OCI_ATTR_HEAPALLOC       = 30;    // memory allocated on the heap
  OCI_ATTR_CHARSET_ID      = 31;    // Character Set ID
  OCI_ATTR_CHARSET_FORM    = 32;    // Character Set Form
  OCI_ATTR_MAXDATA_SIZE    = 33;    // Maximumsize of data on the server
  OCI_ATTR_CACHE_OPT_SIZE  = 34;    // object cache optimal size
  OCI_ATTR_CACHE_MAX_SIZE  = 35;    // object cache maximum size percentage
  OCI_ATTR_PINOPTION       = 36;    // object cache default pin option
  OCI_ATTR_ALLOC_DURATION  = 37;    // object cache default allocation duration
  OCI_ATTR_PIN_DURATION    = 38;    // object cache default pin duration
  OCI_ATTR_FDO             = 39;    // Format Descriptor object attribute
  OCI_ATTR_POSTPROCESSING_CALLBACK = 40;   // Callback to process outbind data
  OCI_ATTR_POSTPROCESSING_CONTEXT  = 41;  // Callback context to process outbind data
  OCI_ATTR_ROWS_RETURNED   = 42;    // Number of rows returned in current iter - for Bind handles
  OCI_ATTR_FOCBK           = 43;    // Failover Callback attribute
  OCI_ATTR_IN_V8_MODE      = 44;    // is the server/service context in V8 mode
  OCI_ATTR_LOBEMPTY        = 45;
  OCI_ATTR_SESSLANG        = 46;    // session language handle

{ AQ Attribute Types }
{ Enqueue Options }

  OCI_ATTR_VISIBILITY      = 47;    // visibility
  OCI_ATTR_RELATIVE_MSGID  = 48;    // relative message id
  OCI_ATTR_SEQUENCE_DEVIATION = 49; // sequence deviation

{ Dequeue Options }

  OCI_ATTR_CONSUMER_NAME   = 50;    // consumer name
  OCI_ATTR_DEQ_MODE        = 51;    // dequeue mode
  OCI_ATTR_NAVIGATION      = 52;    // navigation
  OCI_ATTR_WAIT            = 53;    // wait
  OCI_ATTR_DEQ_MSGID       = 54;    // dequeue message id

{ Message Properties }

  OCI_ATTR_PRIORITY        = 55;    // priority
  OCI_ATTR_DELAY           = 56;    // delay
  OCI_ATTR_EXPIRATION      = 57;    // expiration
  OCI_ATTR_CORRELATION     = 58;    // correlation id
  OCI_ATTR_ATTEMPTS        = 59;    // # of attempts
  OCI_ATTR_RECIPIENT_LIST  = 60;    // recipient list
  OCI_ATTR_EXCEPTION_QUEUE = 61;    // exception queue name
  OCI_ATTR_ENQ_TIME        = 62;    // enqueue time (only OCIAttrGet)
  OCI_ATTR_MSG_STATE       = 63;    // message state (only OCIAttrGet)

{ AQ Agent }

  OCI_ATTR_AGENT_NAME      = 64;    // agent name
  OCI_ATTR_AGENT_ADDRESS   = 65;    // agent address
  OCI_ATTR_AGENT_PROTOCOL  = 66;    // agent protocol

  OCI_ATTR_SENDER_ID       = 68;    // sender id
  OCI_ATTR_ORIGINAL_MSGID  = 69;    // original message id

  OCI_ATTR_QUEUE_NAME      = 70;    // queue name
  OCI_ATTR_NFY_MSGID       = 71;    // message id
  OCI_ATTR_MSG_PROP        = 72;    // message properties

  OCI_ATTR_NUM_DML_ERRORS  = 73;    // num of errs in array DML
  OCI_ATTR_DML_ROW_OFFSET  = 74;    // row offset in the array

  OCI_ATTR_DATEFORMAT      = 75;    // default date format string
  OCI_ATTR_BUF_ADDR        = 76;    // buffer address
  OCI_ATTR_BUF_SIZE        = 77;    // buffer size
  OCI_ATTR_DIRPATH_MODE    = 78;    // mode of direct path operation
  OCI_ATTR_DIRPATH_NOLOG   = 79;    // nologging option
  OCI_ATTR_DIRPATH_PARALLEL = 80;   // parallel (temp seg) option
  OCI_ATTR_NUM_ROWS        = 81;    // number of rows in column array
   // NOTE that OCI_ATTR_NUM_COLS is a column array attribute too.

  OCI_ATTR_COL_COUNT       = 82;    // columns of column array processed so far.
  OCI_ATTR_STREAM_OFFSET   = 83;    // str off of last row processed
  OCI_ATTR_SHARED_HEAPALLOC = 84;   // Shared Heap Allocation Size

  OCI_ATTR_SERVER_GROUP    = 85;    // server group name

  OCI_ATTR_MIGSESSION      = 86;    // migratable session attribute

  OCI_ATTR_NOCACHE         = 87;    // Temporary LOBs

  OCI_ATTR_MEMPOOL_SIZE    = 88;    // Pool Size
  OCI_ATTR_MEMPOOL_INSTNAME = 89;   // Instance name
  OCI_ATTR_MEMPOOL_APPNAME = 90;    // Application name
  OCI_ATTR_MEMPOOL_HOMENAME = 91;   // Home Directory name
  OCI_ATTR_MEMPOOL_MODEL   = 92;    // Pool Model (proc,thrd,both)
  OCI_ATTR_MODES           = 93;    // Modes

  OCI_ATTR_SUBSCR_NAME     = 94;    // name of subscription
  OCI_ATTR_SUBSCR_CALLBACK = 95;    // associated callback
  OCI_ATTR_SUBSCR_CTX      = 96;    // associated callback context
  OCI_ATTR_SUBSCR_PAYLOAD  = 97;    // associated payload
  OCI_ATTR_SUBSCR_NAMESPACE = 98;   // associated namespace

  OCI_ATTR_PROXY_CREDENTIALS = 99;  // Proxy user credentials
  OCI_ATTR_INITIAL_CLIENT_ROLES = 100;// Initial client role list

{ Parameter Attribute Types }

  OCI_ATTR_UNK             = 101;   // unknown attribute
  OCI_ATTR_NUM_COLS        = 102;   // number of columns
  OCI_ATTR_LIST_COLUMNS    = 103;   // parameter of the column list
  OCI_ATTR_RDBA            = 104;   // DBA of the segment header
  OCI_ATTR_CLUSTERED       = 105;   // whether the table is clustered
  OCI_ATTR_PARTITIONED     = 106;   // whether the table is partitioned
  OCI_ATTR_INDEX_ONLY      = 107;   // whether the table is index only
  OCI_ATTR_LIST_ARGUMENTS  = 108;   // parameter of the argument list
  OCI_ATTR_LIST_SUBPROGRAMS = 109;  // parameter of the subprogram list
  OCI_ATTR_REF_TDO         = 110;   // REF to the type descriptor
  OCI_ATTR_LINK            = 111;   // the database link name
  OCI_ATTR_MIN             = 112;   // minimum value
  OCI_ATTR_MAX             = 113;   // maximum value
  OCI_ATTR_INCR            = 114;   // increment value
  OCI_ATTR_CACHE           = 115;   // number of sequence numbers cached
  OCI_ATTR_ORDER           = 116;   // whether the sequence is ordered
  OCI_ATTR_HW_MARK         = 117;   // high-water mark
  OCI_ATTR_TYPE_SCHEMA     = 118;   // type's schema name
  OCI_ATTR_TIMESTAMP       = 119;   // timestamp of the object
  OCI_ATTR_NUM_ATTRS       = 120;   // number of sttributes
  OCI_ATTR_NUM_PARAMS      = 121;   // number of parameters
  OCI_ATTR_OBJID           = 122;   // object id for a table or view
  OCI_ATTR_PTYPE           = 123;   // type of info described by
  OCI_ATTR_PARAM           = 124;   // parameter descriptor
  OCI_ATTR_OVERLOAD_ID     = 125;   // overload ID for funcs and procs
  OCI_ATTR_TABLESPACE      = 126;   // table name space
  OCI_ATTR_TDO             = 127;   // TDO of a type
  OCI_ATTR_LTYPE           = 128;   // list type
  OCI_ATTR_PARSE_ERROR_OFFSET = 129;  // Parse Error offset
  OCI_ATTR_IS_TEMPORARY    = 130;   // whether table is temporary
  OCI_ATTR_IS_TYPED        = 131;   // whether table is typed
  OCI_ATTR_DURATION        = 132;   // duration of temporary table
  OCI_ATTR_IS_INVOKER_RIGHTS = 133; // is invoker rights
  OCI_ATTR_OBJ_NAME        = 134;   // top level schema obj name
  OCI_ATTR_OBJ_SCHEMA      = 135;   // schema name
  OCI_ATTR_OBJ_ID          = 136;   // top level schema object id

  OCI_ATTR_MAXCHAR_SIZE     = 163;  // max char size of data
  OCI_ATTR_CURRENT_POSITION = 164;  // for scrollable result sets

  OCI_ATTR_STMTCACHESIZE    = 176;  // size of the stm cache
  OCI_ATTR_STMT_STATE       = 182;
  OCI_ATTR_ENV_UTF16        = 209;  // is env in utf16 mode?

{ notification subscription }

  OCI_SUBSCR_CQ_QOS_QUERY          = 1;     // Query level granularity is required
  OCI_SUBSCR_CQ_QOS_BEST_EFFORT    = 2;     // Best effort filtering is acceptable

  OCI_ATTR_SUBSCR_QOSFLAGS         = 225;   // QOS flags
  OCI_ATTR_SUBSCR_PAYLOADCBK       = 226;   // Payload callback
  OCI_ATTR_SUBSCR_TIMEOUT          = 227;   // Timeout
  OCI_ATTR_SUBSCR_NAMESPACE_CTX    = 228;   // Namespace context
  OCI_ATTR_SUBSCR_CQ_QOSFLAGS      = 229;   // Notification-specific QOS flag

  OCI_ATTR_CLIENT_IDENTIFIER       = 278;   // User identifier in the session handle

  OCI_ATTR_IS_XMLTYPE              = 315;   // Is the type an XML type?
  OCI_ATTR_XMLSCHEMA_NAME          = 316;   // Name of XML Schema
  OCI_ATTR_XMLELEMENT_NAME         = 317;   // Name of XML Element
  OCI_ATTR_XMLSQLTYPSCH_NAME       = 318;   // SQL type's schema for XML Ele
  OCI_ATTR_XMLSQLTYPE_NAME         = 319;   // Name of SQL type for XML Ele
  OCI_ATTR_XMLTYPE_STORED_OBJ      = 320;   // XML type stored as object?

  OCI_ATTR_TRANSACTION_NO          = 365;   // AQ enq txn number

{ port no attribute in subscription handle }

  OCI_ATTR_SUBSCR_PORTNO           = 390;   // port no to listen

{ DB Change Notification reg handle attributes }

  OCI_ATTR_CHNF_TABLENAMES         = 401;   // out: array of table names
  OCI_ATTR_CHNF_ROWIDS             = 402;   // in: rowids needed
  OCI_ATTR_CHNF_OPERATIONS         = 403;   // in: notification operation filter
  OCI_ATTR_CHNF_CHANGELAG          = 404;   // txn lag between notifications

{ DB Change: Notification Descriptor attributes }

  OCI_ATTR_CHDES_DBNAME            = 405;   // source database
  OCI_ATTR_CHDES_NFYTYPE           = 406;   // notification type flags
  OCI_ATTR_CHDES_XID               = 407;   // XID  of the transaction
  OCI_ATTR_CHDES_TABLE_CHANGES     = 408;   // array of table chg descriptors

  OCI_ATTR_CHDES_TABLE_NAME        = 409;   // table name
  OCI_ATTR_CHDES_TABLE_OPFLAGS     = 410;   // table operation flags
  OCI_ATTR_CHDES_TABLE_ROW_CHANGES = 411;   // array of changed rows
  OCI_ATTR_CHDES_ROW_ROWID         = 412;   // rowid of changed row
  OCI_ATTR_CHDES_ROW_OPFLAGS       = 413;   // row operation flags

{ Statement handle attribute for db change notification }
  OCI_ATTR_CHNF_REGHANDLE          = 414;   // IN: subscription handle

{ Attributes for LOB prefetch }
  OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE = 438;  // default prefetch size
  OCI_ATTR_LOBPREFETCH_SIZE         = 439;
  OCI_ATTR_LOBPREFETCH_LENGTH       = 440;

  OCI_ATTR_MSG_DELIVERY_MODE       = 407;   // msg delivery mode

  OCI_ATTR_IMPLICIT_RESULT_COUNT   = 463;   // Statement handle attribute

{ Specific for Net = true}
  OCI_ATTR_CONNECTION_TIMEOUT      = 10001;
  OCI_ATTR_IP_VERSION              = 10002;
  OCI_ATTR_IOHANDLER               = 10004;
  OCI_ATTR_SSL_OPTIONS             = 10005;
  OCI_ATTR_SSH_OPTIONS             = 10006;
  OCI_ATTR_HTTP_OPTIONS            = 10007;
  OCI_ATTR_HTTP_PROXY_OPTIONS      = 10008;

{ Result set type }

  OCI_RESULT_TYPE_SELECT = 1;

{ DB Change: Event types }

  OCI_EVENT_NONE                   = 0;     // None
  OCI_EVENT_STARTUP                = 1;     // Startup database
  OCI_EVENT_SHUTDOWN               = 2;     // Shutdown database
  OCI_EVENT_SHUTDOWN_ANY           = 3;     // Startup instance
  OCI_EVENT_DROP_DB                = 4;     // Drop database
  OCI_EVENT_DEREG                  = 5;     // Subscription deregistered
  OCI_EVENT_OBJCHANGE              = 6;     // Object change notification
  OCI_EVENT_QUERYCHANGE            = 7;     // Query change notification

{ DB Change: Operation types }

  OCI_OPCODE_ALLROWS               = $1;    // all rows invalidated
  OCI_OPCODE_ALLOPS                = $0;    // interested in all operations
  OCI_OPCODE_INSERT                = $2;    // INSERT
  OCI_OPCODE_UPDATE                = $4;    // UPDATE
  OCI_OPCODE_DELETE                = $8;    // DELETE
  OCI_OPCODE_ALTER                 = $10;   // ALTER
  OCI_OPCODE_DROP                  = $20;   // DROP TABLE
  OCI_OPCODE_UNKNOWN               = $40;   // GENERIC/ UNKNOWN

{ Supported QOS values for notification registrations }

  OCI_SUBSCR_QOS_RELIABLE          = $1;    // reliable
  OCI_SUBSCR_QOS_PAYLOAD           = $2;    // payload delivery
  OCI_SUBSCR_QOS_REPLICATE         = $4;    // replicate to director
  OCI_SUBSCR_QOS_SECURE            = $8;    // secure payload delivery
  OCI_SUBSCR_QOS_PURGE_ON_NTFN     = $10;   // purge on first ntfn
  OCI_SUBSCR_QOS_MULTICBK          = $20;   // multi instance callback

{ Temporary attribute value for UCS2/UTF16 character set ID }

  OCI_AL24UTFFSSID            = 870;
  OCI_AL24UTF8ID              = 871;
  OCI_UTFEID                  = 872;
  OCI_AL32UTF8ID              = 873;
  OCI_UCS2ID                  = 1000; // UCS2 charset ID
  OCI_UTF16ID                 = 1000; // UTF16 charset ID
  OCI_AL16UTF16ID             = 2000;

{ Supported Namespaces }

  OCI_SUBSCR_NAMESPACE_ANONYMOUS   = 0;     // Anonymous Namespace
  OCI_SUBSCR_NAMESPACE_AQ          = 1;     // Advanced Queues
  OCI_SUBSCR_NAMESPACE_DBCHANGE    = 2;     // Change notification

{ Credential Types }

  OCI_CRED_RDBMS           = 1;     // database username/password
  OCI_CRED_EXT             = 2;     // externally provided credentials
  OCI_CRED_PROXY           = 3;     // proxy authentication
{ Error Return Values }

  OCI_SUCCESS              = 0;       // maps to SQL_SUCCESS of SAG CLI
  OCI_SUCCESS_WITH_INFO    = 1;       // maps to SQL_SUCCESS_WITH_INFO
  OCI_NO_DATA              = 100;     // maps to SQL_NO_DATA
  OCI_ERROR                = -1;      // maps to SQL_ERROR
  OCI_INVALID_HANDLE       = -2;      // maps to SQL_INVALID_HANDLE
  OCI_NEED_DATA            = 99;      // maps to SQL_NEED_DATA
  OCI_STILL_EXECUTING      = -3123;   // OCI would block error
  OCI_CONTINUE             = -24200;  // Continue with the body of the OCI function

{ Parsing Syntax Types }

  OCI_NTV_SYNTAX           = 1;       // Use what so ever is the native lang of server
  OCI_V7_SYNTAX            = 2;       // V7 language
  OCI_V8_SYNTAX            = 3;       // V8 language

{ Scrollable Cursor Options }

  OCI_FETCH_NEXT           = $02;
  OCI_FETCH_FIRST          = $04;
  OCI_FETCH_LAST           = $08;
  OCI_FETCH_PRIOR          = $10;
  OCI_FETCH_ABSOLUTE       = $20;
  OCI_FETCH_RELATIVE       = $40;

{ Bind and Define Options }

  OCI_SB2_IND_PTR          = $01;
  OCI_DATA_AT_EXEC         = $02;
  OCI_DYNAMIC_FETCH        = $02;
  OCI_PIECEWISE            = $04;

{ Statement States }

  OCI_STMT_STATE_INITIALIZED  = $01;
  OCI_STMT_STATE_EXECUTED     = $02;
  OCI_STMT_STATE_END_OF_FETCH = $03;

{ Execution Modes }
  OCI_DEFAULT_MODE           = $00;
  OCI_BATCH_MODE             = $01;
  OCI_EXACT_FETCH            = $02;
  OCI_KEEP_FETCH_STATE       = $04;
  OCI_SCROLLABLE_CURSOR      = $08;
  OCI_DESCRIBE_ONLY          = $10;
  OCI_COMMIT_ON_SUCCESS      = $20;
  OCI_NON_BLOCKING           = $40;
  OCI_BATCH_ERRORS           = $80;
  OCI_PARSE_ONLY             = $100;
  OCI_SHOW_DML_WARNINGS      = $400;
  OCI_RETURN_ROW_COUNT_ARRAY = $100000; // OCI_RETURN_ROW_COUNT_ARRAY : Per Iter DML Row Count mode

{ Authentication Modes }
  OCI_MIGRATE              = $0001;   // migratable auth context
  OCI_SYSDBA               = $0002;   // for SYSDBA authorization
  OCI_SYSOPER              = $0004;   // for SYSOPER authorization
  OCI_PRELIM_AUTH          = $0008;   // for preliminary authorization
  OCI_STMT_CACHE           = $0040;   // enable OCI Stmt Caching
  OCI_SYSASM               = $8000;   // for SYSASM authorization
  OCI_SYSBKP               = $20000;   // for SYSBACKUP authorization Oracle 12c
  OCI_SYSDGD               = $40000;   // for SYSDG authorization Oracle 12c
  OCI_SYSKMT               = $80000;   // for SYSKM authorization Oracle 12c

{ Release Modes }
  OCI_STRLS_CACHE_DELETE   = $0010;

{ Piece Information }
  OCI_PARAM_IN             = $01;
  OCI_PARAM_OUT            = $02;

{ Transaction Start Flags }

  OCI_TRANS_OLD            = $00000000;
  OCI_TRANS_NEW            = $00000001;
  OCI_TRANS_JOIN           = $00000002;
  OCI_TRANS_RESUME         = $00000004;
  OCI_TRANS_STARTMASK      = $000000ff;
  OCI_TRANS_READONLY       = $00000100;
  OCI_TRANS_READWRITE      = $00000200;
  OCI_TRANS_SERIALIZABLE   = $00000400;
  OCI_TRANS_ISOLMASK       = $0000ff00;
  OCI_TRANS_LOOSE          = $00010000;
  OCI_TRANS_TIGHT          = $00020000;
  OCI_TRANS_TYPEMASK       = $000f0000;
  OCI_TRANS_NOMIGRATE      = $00100000;


{ Transaction End Flags }
  OCI_TRANS_TWOPHASE       = $01000000;

{ Visibility flags }
  OCI_ENQ_IMMEDIATE        = 1;
  OCI_ENQ_ON_COMMIT        = 2;

{ Dequeue mode flags }
  OCI_DEQ_BROWSE           = 1;
  OCI_DEQ_LOCKED           = 2;
  OCI_DEQ_REMOVE           = 3;

{ Dequeue navigation flags }
  OCI_DEQ_FIRST_MSG        = 1;
  OCI_DEQ_NEXT_MSG         = 3;
  OCI_DEQ_NEXT_TRANSACTION = 2;

{ Message states }
  OCI_MSG_WAITING          = 1;
  OCI_MSG_READY            = 0;
  OCI_MSG_PROCESSED        = 2;
  OCI_MSG_EXPIRED          = 3;

{ Sequence deviation }
  OCI_ENQ_BEFORE           = 2;
  OCI_ENQ_TOP              = 3;

{ Visibility flags }
  OCI_DEQ_IMMEDIATE        = 1;
  OCI_DEQ_ON_COMMIT        = 2;

{ Wait }
  OCI_DEQ_WAIT_FOREVER     = -1;
  OCI_DEQ_NO_WAIT          = 0;

{ Delay }
  OCI_MSG_NO_DELAY         = 0;

{ Expiration }
  OCI_MSG_NO_EXPIRATION    = -1;

  OCI_MSG_PERSISTENT_OR_BUFFERED = 3;
  OCI_MSG_BUFFERED               = 2;
  OCI_MSG_PERSISTENT             = 1;

{ END AQ Constants }

{ Object Types }

  OCI_OTYPE_UNK            = 0;
  OCI_OTYPE_TABLE          = 1;
  OCI_OTYPE_VIEW           = 2;
  OCI_OTYPE_SYN            = 3;
  OCI_OTYPE_PROC           = 4;
  OCI_OTYPE_FUNC           = 5;
  OCI_OTYPE_PKG            = 6;
  OCI_OTYPE_STMT           = 7;


 OCI_ATTR_CHAR_USED        = 285;  // char length semantics
 OCI_ATTR_CHAR_SIZE        = 286;  // char length

{ Describe Handle Parameter Attributes }
// Attributes common to Columns and Stored Procs
  OCI_ATTR_DATA_SIZE       = 1;
  OCI_ATTR_DATA_TYPE       = 2;
  OCI_ATTR_DISP_SIZE       = 3;
  OCI_ATTR_NAME            = 4;
  OCI_ATTR_PRECISION       = 5;
  OCI_ATTR_SCALE           = 6;
  OCI_ATTR_IS_NULL         = 7;
  OCI_ATTR_TYPE_NAME       = 8;
  OCI_ATTR_SCHEMA_NAME     = 9;
  OCI_ATTR_SUB_NAME        = 10;
  OCI_ATTR_POSITION        = 11;

{ complex object retrieval parameter attributes }
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE        = 50;
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL  = 51;
  OCI_ATTR_COMPLEXOBJECT_LEVEL           = 52;
  OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE  = 53;

{ Only Columns }
  OCI_ATTR_DISP_NAME       = 100;

{ Only Stored Procs }
  OCI_ATTR_OVERLOAD        = 210;
  OCI_ATTR_LEVEL           = 211;
  OCI_ATTR_HAS_DEFAULT     = 212;
  OCI_ATTR_IOMODE          = 213;
  OCI_ATTR_RADIX           = 214;
  OCI_ATTR_NUM_ARGS        = 215;

{ only user-defined Type's }
  OCI_ATTR_TYPECODE             = 216;
  OCI_ATTR_COLLECTION_TYPECODE  = 217;
  OCI_ATTR_VERSION              = 218;
  OCI_ATTR_IS_INCOMPLETE_TYPE   = 219;
  OCI_ATTR_IS_SYSTEM_TYPE       = 220;
  OCI_ATTR_IS_PREDEFINED_TYPE   = 221;
  OCI_ATTR_IS_TRANSIENT_TYPE    = 222;
  OCI_ATTR_IS_SYSTEM_GENERATED_TYPE = 223;
  OCI_ATTR_HAS_NESTED_TABLE     = 224;
  OCI_ATTR_HAS_LOB              = 225;
  OCI_ATTR_HAS_FILE             = 226;
  OCI_ATTR_COLLECTION_ELEMENT   = 227;
  OCI_ATTR_NUM_TYPE_ATTRS       = 228;
  OCI_ATTR_LIST_TYPE_ATTRS      = 229;
  OCI_ATTR_NUM_TYPE_METHODS     = 230;
  OCI_ATTR_LIST_TYPE_METHODS    = 231;
  OCI_ATTR_MAP_METHOD           = 232;
  OCI_ATTR_ORDER_METHOD         = 233;
  OCI_ATTR_NUM_ELEMS            = 234;
  OCI_ATTR_IS_SUBTYPE           = 258;
  OCI_ATTR_SUPERTYPE_SCHEMA_NAME = 259;
  OCI_ATTR_SUPERTYPE_NAME       = 260;
  OCI_ATTR_LIST_OBJECTS         = 261;
  OCI_ATTR_NCHARSET_ID          = 262;
  OCI_ATTR_LIST_SCHEMAS         = 263;
  OCI_ATTR_MAX_PROC_LEN         = 264;
  OCI_ATTR_MAX_COLUMN_LEN       = 265;
  OCI_ATTR_CURSOR_COMMIT_BEHAVIOR = 266;
  OCI_ATTR_MAX_CATALOG_NAMELEN  = 267;
  OCI_ATTR_CATALOG_LOCATION     = 268;
  OCI_ATTR_SAVEPOINT_SUPPORT    = 269;
  OCI_ATTR_NOWAIT_SUPPORT       = 270;
  OCI_ATTR_AUTOCOMMIT_DDL       = 271;
  OCI_ATTR_LOCKING_MODE         = 272;

{ for inheritance - part 2 }
  OCI_ATTR_IS_FINAL_TYPE        = 279;
  OCI_ATTR_IS_INSTANTIABLE_TYPE = 280;
  OCI_ATTR_IS_FINAL_METHOD      = 281;     //* is final method ? */
  OCI_ATTR_IS_INSTANTIABLE_METHOD = 282;   //* is instantiable method ? */
  OCI_ATTR_IS_OVERRIDING_METHOD = 283;     //* is overriding method ? */

{ only collection element }
  OCI_ATTR_NUM_ELEMENTS         = 234;

{ only type methods }
  OCI_ATTR_ENCAPSULATION   = 235;
  OCI_ATTR_IS_SELFISH      = 236;
  OCI_ATTR_IS_VIRTUAL      = 237;
  OCI_ATTR_IS_INLINE       = 238;
  OCI_ATTR_IS_CONSTANT     = 239;
  OCI_ATTR_HAS_RESULT      = 240;
  OCI_ATTR_IS_CONSTRUCTOR  = 241;
  OCI_ATTR_IS_DESTRUCTOR   = 242;
  OCI_ATTR_IS_OPERATOR     = 243;
  OCI_ATTR_IS_MAP          = 244;
  OCI_ATTR_IS_ORDER        = 245;
  OCI_ATTR_IS_RNDS         = 246;
  OCI_ATTR_IS_RNPS         = 247;
  OCI_ATTR_IS_WNDS         = 248;
  OCI_ATTR_IS_WNPS         = 249;
{ describing public objects }
  OCI_ATTR_DESC_PUBLIC     = 250;

{ ocicpw Modes }
  OCI_AUTH                 = $08;      // Change the password but donot login

{ Other Constants }
  OCI_MAX_FNS              = 100;      // max number of OCI Functions
  OCI_SQLSTATE_SIZE        = 5;
  OCI_ERROR_MAXMSG_SIZE    = 1024;
  //OCI_LOBMAXSIZE           = MINUB4MAXVAL;
  OCI_ROWID_LEN            = 23;

{ Fail Over Events }
  OCI_FO_END               = $00000001;
  OCI_FO_ABORT             = $00000002;
  OCI_FO_REAUTH            = $00000004;
  OCI_FO_BEGIN             = $00000008;
  OCI_FO_ERROR             = $00000010;

{ Fail Over Callback Return Codes }
  OCI_FO_RETRY              = 25410;


{ Function Codes }
{ Fail Over Types }
  OCI_FO_NONE              = $00000001;
  OCI_FO_SESSION           = $00000002;
  OCI_FO_SELECT            = $00000004;
  OCI_FO_TXNAL             = $00000008;

{ FILE open modes }
  OCI_FILE_READONLY        = 1;      // readonly mode open for FILE types

{ LOB Buffering Flush Flags }
  OCI_LOB_BUFFER_FREE      = 1;
  OCI_LOB_BUFFER_NOFREE    = 2;

{ LOB types }
  OCI_TEMP_BLOB            = 1;      // LOB type - BLOB
  OCI_TEMP_CLOB            = 2;      // LOB type - CLOB
  OCI_TEMP_NCLOB           = 3;      // LOB type - NCLOB ???

{ OCI Statement Types }

  OCI_STMT_UNKNOWN         = 0;
  OCI_STMT_SELECT          = 1;      // Select statement
  OCI_STMT_UPDATE          = 2;      // Update statement
  OCI_STMT_DELETE          = 3;      // Delete statement
  OCI_STMT_INSERT          = 4;      // Insert Statement
  OCI_STMT_CREATE          = 5;      // Create statement
  OCI_STMT_DROP            = 6;      // Drop statement
  OCI_STMT_ALTER           = 7;      // Alter statement
  OCI_STMT_BEGIN           = 8;      // Begin ... (pl/sql statement)
  OCI_STMT_DECLARE         = 9;      // Declare .. (pl/sql statement )
  OCI_STMT_EXPLAIN         = 10;     // Explain plan

{ OCI Parameter Types }

  OCI_PTYPE_UNK            = 0;      // unknown
  OCI_PTYPE_TABLE          = 1;      // table
  OCI_PTYPE_VIEW           = 2;      // view
  OCI_PTYPE_PROC           = 3;      // procedure
  OCI_PTYPE_FUNC           = 4;      // function
  OCI_PTYPE_PKG            = 5;      // package
  OCI_PTYPE_TYPE           = 6;      // user-defined type
  OCI_PTYPE_SYN            = 7;      // synonym
  OCI_PTYPE_SEQ            = 8;      // sequence
  OCI_PTYPE_COL            = 9;      // column
  OCI_PTYPE_ARG            = 10;     // argument
  OCI_PTYPE_LIST           = 11;     // list
  OCI_PTYPE_TYPE_ATTR      = 12;     // user-defined type's attribute
  OCI_PTYPE_TYPE_COLL      = 13;     // collection type's element
  OCI_PTYPE_TYPE_METHOD    = 14;     // user-defined type's method
  OCI_PTYPE_TYPE_ARG       = 15;     // user-defined type method's argument
  OCI_PTYPE_TYPE_RESULT    = 16;     // user-defined type method's result

{ OCI List Types }

  OCI_LTYPE_UNK            = 0;      // unknown
  OCI_LTYPE_COLUMN         = 1;      // column list
  OCI_LTYPE_ARG_PROC       = 2;      // procedure argument list
  OCI_LTYPE_ARG_FUNC       = 3;      // function argument list
  OCI_LTYPE_SUBPRG         = 4;      // subprogram list
  OCI_LTYPE_TYPE_ATTR      = 5;      // type attribute
  OCI_LTYPE_TYPE_METHOD    = 6;      // type method
  OCI_LTYPE_TYPE_ARG_PROC  = 7;      // type method w/o result argument list
  OCI_LTYPE_TYPE_ARG_FUNC  = 8;      // type method w/result argument list

{ TYPE CODE }

  OCI_TYPECODE_DATE        = SQLT_DAT;    // SQL DATE  OTS DATE
  OCI_TYPECODE_SIGNED8     = 27;          // SQL SIGNED INTEGER(8)  OTS SINT8
  OCI_TYPECODE_SIGNED16    = 28;          // SQL SIGNED INTEGER(16)  OTS SINT16
  OCI_TYPECODE_SIGNED32    = 29;          // SQL SIGNED INTEGER(32)  OTS SINT32
  OCI_TYPECODE_REAL        = 21;          // SQL REAL  OTS SQL_REAL
  OCI_TYPECODE_DOUBLE      = 22;          // SQL DOUBLE PRECISION  OTS SQL_DOUBLE
  OCI_TYPECODE_FLOAT       = SQLT_FLT;    // SQL FLOAT(P)  OTS FLOAT(P)
  OCI_TYPECODE_NUMBER      = SQLT_NUM;    // SQL NUMBER(P S)  OTS NUMBER(P S)
  OCI_TYPECODE_DECIMAL     = SQLT_PDN;    // SQL DECIMAL(P S)  OTS DECIMAL(P S)
  OCI_TYPECODE_UNSIGNED8   = SQLT_BIN;    // SQL UNSIGNED INTEGER(8)  OTS UINT8
  OCI_TYPECODE_UNSIGNED16  = 25;          // SQL UNSIGNED INTEGER(16)  OTS UINT16
  OCI_TYPECODE_UNSIGNED32  = 26;          // SQL UNSIGNED INTEGER(32)  OTS UINT32
  OCI_TYPECODE_OCTET       = 245;         // SQL ???  OTS OCTET
  OCI_TYPECODE_SMALLINT    = 246;         // SQL SMALLINT  OTS SMALLINT
  OCI_TYPECODE_INTEGER     = SQLT_INT;    // SQL INTEGER  OTS INTEGER
  OCI_TYPECODE_RAW         = SQLT_LVB;    // SQL RAW(N)  OTS RAW(N)
  OCI_TYPECODE_PTR         = 32;          // SQL POINTER  OTS POINTER
  OCI_TYPECODE_VARCHAR2    = SQLT_VCS;    // SQL VARCHAR2(N)  OTS SQL_VARCHAR2(N)
  OCI_TYPECODE_CHAR        = SQLT_AFC;    // SQL CHAR(N)  OTS SQL_CHAR(N)
  OCI_TYPECODE_VARCHAR     = SQLT_CHR;    // SQL VARCHAR(N)  OTS SQL_VARCHAR(N)
  OCI_TYPECODE_MLSLABEL    = SQLT_LAB;    // OTS MLSLABEL
  OCI_TYPECODE_VARRAY      = 247;         // SQL VARRAY  OTS PAGED VARRAY
  OCI_TYPECODE_TABLE       = 248;         // SQL TABLE  OTS MULTISET
  OCI_TYPECODE_OBJECT      = SQLT_NTY;    // SQL/OTS NAMED OBJECT TYPE
  OCI_TYPECODE_REF         = SQLT_REF;    // SQL/OTS OBJECT REFERENCE
  OCI_TYPECODE_NAMEDCOLLECTION = SQLT_NCO;// SQL/OTS NAMED COLLECTION TYPE
  OCI_TYPECODE_OPAQUE      = SQLT_OPAQUE;
  OCI_TYPECODE_BLOB        = SQLT_BLOB;   // SQL/OTS BINARY LARGE OBJECT
  OCI_TYPECODE_CLOB        = SQLT_CLOB;   // SQL/OTS CHARACTER LARGE OBJECT
  OCI_TYPECODE_BFILE       = SQLT_BFILE;  // SQL/OTS BINARY FILE OBJECT
  OCI_TYPECODE_CFILE       = SQLT_CFILE;  // SQL/OTS CHARACTER FILE OBJECT
  OCI_TYPECODE_TIME        = SQLT_TIME;
  OCI_TYPECODE_TIME_TZ     = SQLT_TIME_TZ;
  OCI_TYPECODE_TIMESTAMP   = SQLT_TIMESTAMP;
  OCI_TYPECODE_TIMESTAMP_TZ = SQLT_TIMESTAMP_TZ;
  OCI_TYPECODE_TIMESTAMP_LTZ = SQLT_TIMESTAMP_LTZ;
  OCI_TYPECODE_INTERVAL_YM = SQLT_INTERVAL_YM;
  OCI_TYPECODE_INTERVAL_DS = SQLT_INTERVAL_DS;
  OCI_TYPECODE_BDOUBLE     = SQLT_IBDOUBLE;
  OCI_TYPECODE_BFLOAT      = SQLT_IBFLOAT;
  OCI_TYPECODE_UROWID      = SQLT_RDD;
  OCI_TYPECODE_NCHAR       = 286;
  OCI_TYPECODE_NVARCHAR2   = 287;
  OCI_TYPECODE_NCLOB       = 288;

  OCI_TYPECODE_OTMFIRST    = 228;         // first Open Type Manager typecode
  OCI_TYPECODE_OTMLAST     = 320;         // last OTM typecode
  OCI_TYPECODE_SYSFIRST    = 228;         // first OTM system type (internal)
  OCI_TYPECODE_SYSLAST     = 235;         // last OTM system type (internal)

  // the following are PL/SQL-only internal. They should not be used
  OCI_TYPECODE_ITABLE      = SQLT_TAB;    // PLSQL indexed table
  OCI_TYPECODE_RECORD      = SQLT_REC;    // PLSQL record
  OCI_TYPECODE_BOOLEAN     = SQLT_BOL;    // PLSQL boolean

{ OBJECT INDICATOR }

  OCI_IND_NOTNULL          = 0;      // not NULL
  OCI_IND_NULL             = -1;     // NULL
  OCI_IND_BADNULL          = -2;     // BAD NULL
  OCI_IND_NOTNULLABLE      = -3;     // not NULLable
  OCI_IND_UNCHANGED        = 3105;   // indicator was not changed

{ OBJECT PIN OPTION }

  // 0 = uninitialized
  OCI_PIN_DEFAULT          = 1;      // default pin option
  OCI_PIN_ANY              = 3;      // pin any copy of the object
  OCI_PIN_RECENT           = 4;      // pin recent copy of the object
  OCI_PIN_LATEST           = 5;      // pin latest copy of the object

{ OBJECT LOCK OPTION }

  // 0 = uninitialized
  OCI_LOCK_NONE            = 1;      // null (same as no lock)
  OCI_LOCK_X               = 2;      // exclusive lock

{ OBJECT MODIFYING OPTION }

  // 0 = uninitialized
  OCI_MARK_DEFAULT         = 1;      // default (the same as OCI_MARK_NONE)
  OCI_MARK_NONE            = OCI_MARK_DEFAULT;   // object has not been modified
  OCI_MARK_UPDATE          = 2;      // object is to be updated

{ OCIDuration Types }
  OCI_DURATION_INVALID     = $FFFF;                  //Invalid duration

  OCI_DURATION_BEGIN         = 10;                      //beginning sequence of duration
  OCI_DURATION_SESSION       = OCI_DURATION_BEGIN;
  OCI_DURATION_TRANS         = OCI_DURATION_BEGIN + 1;
  OCI_DURATION_CALL          = OCI_DURATION_BEGIN + 2;
  OCI_DURATION_STATEMENT     = OCI_DURATION_BEGIN + 3;
  OCI_DURATION_CALLOUT       = OCI_DURATION_BEGIN + 4;
  OCI_DURATION_NULL          = OCI_DURATION_BEGIN - 1;
  OCI_DURATION_DEFAULT       = OCI_DURATION_BEGIN - 2;
  OCI_DURATION_USER_CALLBACK = OCI_DURATION_BEGIN - 3;
  OCI_DURATION_NEXT          = OCI_DURATION_BEGIN - 4;   //next special duration
  OCI_DURATION_PROCESS       = OCI_DURATION_BEGIN - 5;
  OCI_DURATION_LAST          = OCI_DURATION_CALLOUT;     //last of predefined durations

{ OBJECT FREE OPTION }

  OCI_OBJECTFREE_FORCE     = $0001;
  OCI_OBJECTFREE_NONULL    = $0002;

{ OBJECT LIFETIME }

  // 0 = uninitialized
  OCI_OBJECT_PERSISTENT    = 1;     // persistent object
  OCI_OBJECT_TRANSIENT     = 2;     // transient object
  OCI_OBJECT_VALUE         = 3;     // value object

{ OBJECT MARK STATUS }

  OCI_OBJECT_NEW           = $0001;   // new object
  OCI_OBJECT_DELETED       = $0002;   // object marked deleted
  OCI_OBJECT_UPDATED       = $0004;   // object marked updated

{ OCITypeGetOpt Types }

  OCI_TYPEGET_HEADER       = 0;
  OCI_TYPEGET_ALL          = 1;

{ NUMBER TYPE SIZE }

  OCI_NUMBER_SIZE          = 22;

{ OCINumberToInt }

  OCI_NUMBER_UNSIGNED      = 0;       // Unsigned type -- ubX
  OCI_NUMBER_SIGNED        = 2;       // Signed type -- sbX

{ OCIInterval }
  OCI_INTER_INVALID_DAY        = $1;
  OCI_INTER_DAY_BELOW_VALID    = $2;
  OCI_INTER_INVALID_MONTH      = $4;
  OCI_INTER_MONTH_BELOW_VALID  = $8;
  OCI_INTER_INVALID_YEAR       = $10;
  OCI_INTER_YEAR_BELOW_VALID   = $20;
  OCI_INTER_INVALID_HOUR       = $40;
  OCI_INTER_HOUR_BELOW_VALID   = $80;
  OCI_INTER_INVALID_MINUTE     = $100;
  OCI_INTER_MINUTE_BELOW_VALID = $200;
  OCI_INTER_INVALID_SECOND     = $400;
  OCI_INTER_SECOND_BELOW_VALID = $800;
  OCI_INTER_INVALID_FRACSEC    = $1000;
  OCI_INTER_FRACSEC_BELOW_VALID= $2000;

{ OCIDateTime }
  OCI_DT_INVALID_DAY           = $1;
  OCI_DT_DAY_BELOW_VALID       = $2;
  OCI_DT_INVALID_MONTH         = $4;
  OCI_DT_MONTH_BELOW_VALID     = $8;
  OCI_DT_INVALID_YEAR          = $10;
  OCI_DT_YEAR_BELOW_VALID      = $20;
  OCI_DT_INVALID_HOUR          = $40;
  OCI_DT_HOUR_BELOW_VALID      = $80;
  OCI_DT_INVALID_MINUTE        = $100;
  OCI_DT_MINUTE_BELOW_VALID    = $200;
  OCI_DT_INVALID_SECOND        = $400;
  OCI_DT_SECOND_BELOW_VALID    = $800;
  OCI_DT_DAY_MISSING_FROM_1582 = $1000;
  OCI_DT_YEAR_ZERO             = $2000;
  OCI_DT_INVALID_TIMEZONE      = $4000;
  OCI_DT_INVALID_FORMAT        = $8000;

{ XML}
  OCI_XMLTYPE_CREATE_OCISTRING  = 1;
  OCI_XMLTYPE_CREATE_CLOB       = 2;
  OCI_XMLTYPE_CREATE_BLOB       = 3;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}

{ Connection flags }

  ORAMTS_CFLG_ALLDEFAULT = $00;  // default flags
  ORAMTS_CFLG_NOIMPLICIT = $01;  // don't do implicit enlistment
  ORAMTS_CFLG_UNIQUESRVR = $02;  // need a separate Net8 connect
  ORAMTS_CFLG_SYSDBALOGN = $04;  // logon as a SYSDBA
  ORAMTS_CFLG_SYSOPRLOGN = $10;  // logon as a SYSOPER
  ORAMTS_CFLG_PRELIMAUTH = $20;  // preliminary internal login

  ORAMTS_ENFLG_DEFAULT = 0;      // default flags
  ORAMTS_ENFLG_RESUMTX = 1;      // resume a detached transact.
  ORAMTS_ENFLG_DETCHTX = 2;      // detached from the transact.


{ Error codes reported by the OraMTS<> functions }

  ORAMTSERR_NOERROR      = 0;          // success code

  ORAMTSERR_NOMTXDISPEN  = 1001;       // no MTXDM.DLL available
  ORAMTSERR_DSPCREAFAIL  = 1002;       // failure to create dispen
  ORAMTSERR_DSPMAXSESSN  = 1003;       // exceeded max sessions
  ORAMTSERR_DSPINVLSVCC  = 1004;       // invalid OCI Svc ctx
  ORAMTSERR_DSPNODBIDEN  = 1005;       // can't create new dbiden

  ORAMTSERR_NOSERVEROBJ  = 2001;       // unable to alloc a server

  ORAMTSERR_INVALIDSRVR  = 3001;       // invalid server object
  ORAMTSERR_FAILEDATTCH  = 3002;       // failed attach to Oracle
  ORAMTSERR_FAILEDDETCH  = 3003;       // failed detach from db
  ORAMTSERR_FAILEDTRANS  = 3004;       // failed to start trans.
  ORAMTSERR_SETATTRIBUT  = 3005;       // OCI set attrib failed
  ORAMTSERR_CONNXBROKEN  = 3006;       // conn to Oracle broken
  ORAMTSERR_NOTATTACHED  = 3007;       // not attached to Oracle
  ORAMTSERR_ALDYATTACHD  = 3008;       // alrdy attached to Oracle

  ORAMTSERR_INVALIDSESS  = 4001;       // invalid session object
  ORAMTSERR_FAILEDLOGON  = 4002;       // failed logon to Oracle
  ORAMTSERR_FAILEDLOGOF  = 4003;       // failed logoff from db
  ORAMTSERR_TRANSEXISTS  = 4004;       // no transaction beneath
  ORAMTSERR_LOGONEXISTS  = 4005;       // already logged on to db
  ORAMTSERR_NOTLOGGEDON  = 4006;       // not logged on to Oracle

  ORAMTSERR_RPCINVLCTXT  = 5001;       // RPC context is invalid
  ORAMTSERR_RPCCOMMUERR  = 5002;       // generic communic. error
  ORAMTSERR_RPCALRDYCON  = 5003;       // endpoint already connect
  ORAMTSERR_RPCNOTCONNE  = 5004;       // endpoint not connected
  ORAMTSERR_RPCPROTVIOL  = 5005;       // protocol violation
  ORAMTSERR_RPCACCPTIMO  = 5006;       // timeout accepting conn.
  ORAMTSERR_RPCILLEGOPC  = 5007;       // invalid RPC opcode
  ORAMTSERR_RPCBADINCNO  = 5008;       // mismatched incarnation#
  ORAMTSERR_RPCCONNTIMO  = 5009;       // client connect timeout
  ORAMTSERR_RPCSENDTIMO  = 5010;       // synch. send timeout
  ORAMTSERR_RPCRECVTIMO  = 5011;       // synch. receive timedout
  ORAMTSERR_RPCCONRESET  = 5012;       // connection reset by peer

  ORAMTSERR_INVALIDARGU  = 6001;       // invalid args to function
  ORAMTSERR_INVALIDOBJE  = 6002;       // an object was invalid
  ORAMTSERR_ILLEGALOPER  = 6003;       // illegal operation
  ORAMTSERR_ALLOCMEMORY  = 6004;       // memory allocation error
  ORAMTSERR_ERRORSYNCHR  = 6005;       // synchr. object error
  ORAMTSERR_NOORAPROXY   = 6006;       // no Oracle Proxy server
  ORAMTSERR_ALRDYENLIST  = 6007;       // session already enlisted
  ORAMTSERR_NOTENLISTED  = 6008;       // session is not enlisted
  ORAMTSERR_TYPMANENLIS  = 6009;       // illeg on manuenlst sess
  ORAMTSERR_TYPAUTENLIS  = 6010;       // illeg on autoenlst sess
  ORAMTSERR_TRANSDETACH  = 6011;       // error detaching trans.
  ORAMTSERR_OCIHNDLALLC  = 6012;       // OCI handle alloc error
  ORAMTSERR_OCIHNDLRELS  = 6013;       // OCI handle dealloc error
  ORAMTSERR_TRANSEXPORT  = 6014;       // error exporting trans.
  ORAMTSERR_OSCREDSFAIL  = 6105;       // error getting NT creds
  ORAMTSERR_ISONOSUPPORT = 6108;       // txn iso-level not supported
  ORAMTSERR_MIXEDTXNISO  = 6109;       // differ iso lvls for same txn
{$ENDIF}
{$ENDIF}

{ Handles and descriptors for direct path operations (OCIDirPath*) }
type
{$IFDEF CLR}
  pOCIDirPathCtx      = IntPtr;
  pOCIDirPathColArray = IntPtr;
  pOCIDirPathStream   = IntPtr;
  pOCIDirPathDesc     = IntPtr;
{$ELSE}
  OCIDirPathCtx      = record end;    // context
  OCIDirPathColArray = record end;    // column array
  OCIDirPathStream   = record end;    // stream
  OCIDirPathDesc     = record end;    // direct path descriptor

  pOCIDirPathCtx      = ^OCIDirPathCtx;
  pOCIDirPathColArray = ^OCIDirPathColArray;
  pOCIDirPathStream   = ^OCIDirPathStream;
  pOCIDirPathDesc     = ^OCIDirPathDesc;
{$ENDIF}

{ Defines for Direct Path Options }
const
  // values for OCI_ATTR_DIRPATH_MODE attribute
  OCI_DIRPATH_LOAD         = 1;       // direct path load operation
  OCI_DIRPATH_UNLOAD       = 2;       // direct path unload operation
  OCI_DIRPATH_CONVERT      = 3;       // direct path convert only operation

  // values for OCI_ATTR_STATE attribute of OCIDirPathCtx
  OCI_DIRPATH_NORMAL       = 1;       // can accept rows, last row complete
  OCI_DIRPATH_PARTIAL      = 2;       // last row was partial
  OCI_DIRPATH_NOT_PREPARED = 3;       // direct path context is not prepared

  // values for cflg argument to OCIDirpathColArrayEntrySet
  OCI_DIRPATH_COL_COMPLETE = 0;       // column data is complete
  OCI_DIRPATH_COL_NULL     = 1;       // column is null
  OCI_DIRPATH_COL_PARTIAL  = 2;       // column data is partial

type

  TRowId8 = packed record
    ridobjnum   : ub4; // data obj#--this field is unused in restricted ROWIDs
    ridfilenum  : ub2;
    filler      : ub2; // ub1; oracle bag
    ridblocknum : ub4;
    ridslotnum  : ub2;
  end;
{$IFDEF CLR}
  PRowId8 = packed record
  private
    Ptr: IntPtr;
    function GetUB2Property(Index: Integer): ub2;
    procedure SetUB2Property(Index: Integer; Value: ub2);
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);

  public
    property ridobjnum   : ub4 index 0 read GetUB4Property write SetUB4Property;
    property ridfilenum  : ub2 index 4 read GetUB2Property write SetUB2Property;
    property filler      : ub2 index 6 read GetUB2Property write SetUB2Property;
    property ridblocknum : ub4 index 8 read GetUB4Property write SetUB4Property;
    property ridslotnum  : ub2 index 12 read GetUB2Property write SetUB2Property;

    class operator Implicit(AValue: IntPtr): PRowId8;
    class operator Implicit(AValue: PRowId8): IntPtr;
  end;
{$ELSE}
  PRowid8 = ^TRowid8;
{$ENDIF}

const
  sizeof_TRowId8 = 14;

type
  TRowId81 = packed record
    filler      : ub1; // 0 //unknown
    ridobjnum   : ub4; // 1 //data obj#--this field is unused in restricted ROWIDs
    ridfilenum  : ub2; // 5
    ridblocknum : ub4; // 7
    ridslotnum  : ub2; // 11
  end;
{$IFDEF CLR}
  PRowid81 = packed record
  private
    Ptr: IntPtr;
    function GetUB1Property(Index: Integer): ub1;
    procedure SetUB1Property(Index: Integer; Value: ub1);
    function GetUB2Property(Index: Integer): ub2;
    procedure SetUB2Property(Index: Integer; Value: ub2);
    function GetUB4Property(Index: Integer): ub4;
    procedure SetUB4Property(Index: Integer; Value: ub4);

  public
    property filler      : ub1 index 0 read GetUB1Property write SetUB1Property;
    property ridobjnum   : ub4 index 1 read GetUB4Property write SetUB4Property;
    property ridfilenum  : ub2 index 5 read GetUB2Property write SetUB2Property;
    property ridblocknum : ub4 index 7 read GetUB4Property write SetUB4Property;
    property ridslotnum  : ub2 index 11 read GetUB2Property write SetUB2Property;

    class operator Implicit(AValue: IntPtr): PRowid81;
    class operator Implicit(AValue: PRowid81): IntPtr;
  end;
{$ELSE}
  PRowid81 = ^TRowid81;
{$ENDIF}

const
  sizeof_TRowId81 = 13;

type
{ OCI descriptor types }
  OCIRowid = packed record
    Unknown : array [0..7] of byte;
    RowId   : TRowId8;
  end;
{$IFDEF CLR}
  POCIRowid = packed record
  private
    Ptr: IntPtr;
    function GetPRowId8Property(Index: Integer): PRowId8;

  public
    property RowId   : PRowId8 index 8 read GetPRowId8Property;

    class operator Implicit(AValue: IntPtr): POCIRowid;
    class operator Implicit(AValue: POCIRowid): IntPtr;
  end;
{$ELSE}
  POCIRowid = ^OCIRowid;
{$ENDIF}

const
  sizeof_OCIRowid = 8 + sizeof_TRowId8;

type
  OCIRowid81 = packed record  // for Oracle 8i
    Unknown : array [0..7] of byte;
    RowId   : PRowId81;
  end;
{$IFDEF CLR}
  POCIRowid81 = packed record  // for Oracle 8i
  private
    Ptr: IntPtr;
    function GetPRowId81Property(Index: Integer): PRowId81;
    procedure SetPRowId81Property(Index: Integer; Value: PRowId81);

  public
    property RowId : PRowId81 index 8 read GetPRowId81Property write SetPRowId81Property;

    class operator Implicit(AValue: IntPtr): pOCIRowid81;
    class operator Implicit(AValue: pOCIRowid81): IntPtr;
  end;
{$ELSE}
  POCIRowid81 = ^OCIRowid81;
{$ENDIF}
const
  sizeof_OCIRowid81 = 12;


type
{ OTS types }
  OCINumber = record
    OCINumberPart: array[0..OCI_NUMBER_SIZE -1] of byte;
  end;

  OCITime = packed record
    OCITimeHH: ub1;         // hours; range is 0 <= hours <=23
    OCITimeMI: ub1;         // minutes; range is 0 <= minutes <= 59
    OCITimeSS: ub1;         // seconds; range is 0 <= seconds <= 59
  end;

  OCIDate = packed record
    OCIDateYYYY: sb2;       // gregorian year; range is -4712 <= year <= 9999
    OCIDateMM: ub1;         // month; range is 1 <= month < 12
    OCIDateDD: ub1;         // day; range is 1 <= day <= 31
    OCIDateTime: OCITime;   // time
  end;

  TOCICallbackFailover = function (svchp: IntPtr; envhp: IntPtr; fo_ctx: IntPtr; fo_type: ub4; fo_event: ub4): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}

  TOCIFoCbkStruct = packed record
    callback_function: IntPtr;
    fo_ctx: IntPtr;
  end;

{ OCI handle types }
  pOCIBind     = IntPtr;
  pOCIDefine   = IntPtr;
  pOCIDescribe = IntPtr;
  pOCIEnv      = IntPtr;
  pOCIError    = IntPtr;
  pOCIServer   = IntPtr;
  pOCISession  = IntPtr;
  pOCIStmt     = IntPtr;
  pOCISvcCtx   = IntPtr;
  pOCITrans    = IntPtr;
  pOCIComplexObject = IntPtr;
  pOCISPool    = IntPtr;
  pOCIAuthInfo = IntPtr;

  pOCIParam       = IntPtr;
  pOCISnapshot    = IntPtr;
  pOCILobLocator  = IntPtr;
  pOCIDateTime    = IntPtr;
  pOCIInterval    = IntPtr;
  pOCIType        = IntPtr;
  ppOCIString     = IntPtr;

  pOCINumber   = IntPtr;
  pOCIDate     = IntPtr;
  pOCIString   = IntPtr;
  pOCIRaw      = IntPtr;
  pOCIRef      = IntPtr;
  pOCIColl     = IntPtr;
  pOCIArray    = IntPtr;
  pOCITable    = IntPtr;

  pOCIExtProcContext = IntPtr;

  pOCISubscription = IntPtr;

{$IFDEF CLR}

  pOCIHandle   = IntPtr;
  pOCIDescriptor  = IntPtr;

  ppOCIStmt    = IntPtr;
  ppOCIParam      = IntPtr;
  ppOCILobLocator = IntPtr;
  ppOCIDateTime   = IntPtr;
  ppOCIInterval   = IntPtr;
  ppOCIDescriptor = IntPtr;
  ppOCIRef     = IntPtr;
  ppOCIType    = IntPtr;

{$ELSE}

  ppOCIEnv     = ^pOCIEnv;
  ppOCIBind    = ^pOCIBind;
  ppOCIDefine  = ^pOCIDefine;
  ppOCIStmt    = ^pOCIStmt;
  ppOCISvcCtx  = ^pOCISvcCtx;

  pOCIHandle   = IntPtr;
  ppOCIHandle  = ^pOCIHandle;

{ OCI descriptor types }

  OCIParam      = integer;  // OCI_DTYPE_PARAM
  OCISnapshot   = integer;  // OCI_DTYPE_SNAP
  OCILobLocator = integer;  // OCI_DTYPE_LOB, OCI_DTYPE_FILE
  OCIDateTime   = integer;  // OCI_DTYPE_TIMESTAMP, OCI_DTYPE_TIMESTAMP_TZ,
                            // OCI_DTYPE_TIMESTAMP_LTZ
  OCIInterval   = integer;  // OCI_DTYPE_INTERVAL_YM, OCI_DTYPE_INTERVAL_DS

  pOCIDescriptor  = IntPtr;

  ppOCIParam      = ^pOCIParam;
  ppOCILobLocator = ^pOCILobLocator;
  ppOCIDescriptor = ^pOCIDescriptor;
  ppOCIDateTime   = ^pOCIDateTime;
  ppOCIInterval   = ^pOCIInterval;

{ OTS types }

  OCIType = integer;

  OCIString = packed record
  end;

  OCIRaw = packed record
  end;

  OCIRef = packed record
  end;

  OCIColl = packed record
  end;

  OCIArray = packed record
  end;

  OCITable = packed record
  end;

  OCIIter = packed record
  end;

  ppOCIType    = ^pOCIType;
  ppOCIRef     = ^pOCIRef;

{$ENDIF}

  tenum = Integer;
  OCIDuration = ub2;
  OCIInd = sb2;
  OCILockOpt = tenum;
  OCIMarkOpt = tenum;
  OCIObjectEvent = tenum;
  OCIObjectProperty = tenum;
  OCIPinOpt = tenum;
  OCIRefreshOpt = tenum;
  OCITypeCode = ub2;
  OCITypeEncap = tenum;
  OCITypeGetOpt = tenum;
  OCITypeMethodFlag = tenum;
  OCITypeParamMode = tenum;
  OCIObjectPropId = ub1;
  OCIObjectLifetime = tenum;
  OCIObjectMarkstatus = uword;

{$IFDEF CLR}
  pOCIInd = IntPtr;
{$ELSE}
  pOCIInd = ^OCIInd;
{$ENDIF}

{$IFDEF CLR}
  [Serializable, AttributeUsage (AttributeTargets.Delegate)]
  CallConvCdeclAttribute = class (Attribute)
  end;
{$ENDIF}

  TXID = packed record
    FormatID: integer;
    Gtrid_length: integer; // value from 1 through 64
    Bqual_length: integer; // value from 1 through 64
    Data: array [0 .. 127] of byte;
  end;

const
  XID_SIZE = 140; // SizeOf(TXID) incorrect in CLR

type
  {$IFDEF CLR}[CallConvCdecl]{$ENDIF}
  TOCISubscriptionNotify = function(pCtx: IntPtr; pSubscrHp: pOCISubscription;
    pPayload: IntPtr; iPayloadLen: ub4; pDescriptor: IntPtr; iMode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAttrGet1 = function(trgthndlp: IntPtr; trghndltyp: ub4; attributep: IntPtr;
    sizep: pub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAttrGet2 = function(trgthndlp: IntPtr; trghndltyp: ub4; var attributep: Integer;
    sizep: pub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAttrSet1 = function (trgthndlp: IntPtr; trghndltyp: ub4; attributep: IntPtr;
    size: ub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  //_OCIAttrSet3 = function (trgthndlp: IntPtr; trghndltyp: ub4; attributep: PChar;
  //  size: ub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAttrSet2 = function (trgthndlp: IntPtr; trghndltyp: ub4; var attributep: Integer;
    size: ub4; attrtype: ub4; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindArrayOfStruct = function (bindp: pOCIBind; errhp: pOCIError; pvskip: ub4;
    indskip: ub4; alskip: ub4; rcskip: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindByName = function (stmtp: pOCIStmt; var bindpp: pOCIBind; errhp: pOCIError;
    placeholder: IntPtr; placeh_len: sb4; valuep: IntPtr; value_sz: sb4;
    dty: ub2; indp: IntPtr; alenp: pub2; rcodep: pub2; maxarr_len: ub4;
    curelep: pub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindByPos = function (stmtp: pOCIStmt; var bindpp: pOCIBind; errhp: pOCIError;
    position: ub4; valuep: IntPtr; value_sz: sb4; dty: ub2;
    indp: IntPtr; alenp: pub2; rcodep: pub2; maxarr_len: ub4;
    curelep: pub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  {$IFDEF CLR}[CallConvCdecl]{$ENDIF}
  TOCICallbackInBind = function (ictxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
    var bufpp: IntPtr; var alenp: ub4; var piecep: ub1; var indpp: pOCIInd): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}

  {$IFDEF CLR}[CallConvCdecl]{$ENDIF}
  TOCICallbackOutBind = function (octxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
    var bufpp: IntPtr; var alenpp: pub4; var piecep: ub1; var indpp: pOCIInd;
    var rcodepp: pub2): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindDynamic = function (bindp: pOCIBind; errhp: pOCIError; ictxp: IntPtr;
    icbfp: IntPtr; octxp: IntPtr; ocbfp: IntPtr): sword;  {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBindObject = function (bindp: pOCIBind; errhp: pOCIError; const otype: pOCIType;
    pgvpp: IntPtr; pvszsp: pub4; indpp: pOCIInd; indszp: pub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIBreak = function (hndlp: pOCIHandle; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDefineArrayOfStruct = function (defnp: pOCIDefine; errhp: pOCIError;
    pvskip: ub4; indskip: ub4; rlskip: ub4; rcskip: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDefineByPos = function (stmtp: pOCIStmt; var defnpp: pOCIDefine; errhp: pOCIError;
    position: ub4; valuep: IntPtr; value_sz: sb4; dty: ub2;
    indp: IntPtr; rlenp: pub2; rcodep: pub2; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  {$IFDEF CLR}[CallConvCdecl]{$ENDIF}
  TOCICallbackDefine = function (octxp: IntPtr; defnp: pOCIDefine; iter: ub4; var bufpp: IntPtr;
    var alenpp: pub4; var piecep: ub1; var indpp: pOCIInd; var rcodep: pub2): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDefineDynamic = function (defnp: pOCIDefine; errhp: pOCIError; octxp: IntPtr;
    ocbfp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDefineObject = function (defnp: pOCIDefine; errhp: pOCIError; const otype: pOCIType;
    pgvpp: IntPtr; pvszsp: pub4; indpp: pOCIInd; indszp: pub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDescribeAny = function (svchp: pOCISvcCtx; errhp: pOCIError; objptr: IntPtr;
    objnm_len: ub4; objptr_typ: ub1; info_level: ub1; objtyp: ub1;  // WAR objtyp: ub1
    dschp: pOCIDescribe): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDescriptorAlloc = function (parenth: IntPtr; var descpp: pOCIDescriptor;
    dtype: ub4; xtramem_sz: NativeUInt; usrmempp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDescriptorFree = function (descp: pOCIDescriptor; dtype: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIEnvInit = function (var envhpp: pOCIEnv; mode: ub4; xtramemsz: NativeUInt; usrmempp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIErrorGet = function (hndlp: pOCIHandle; recordno: ub4; sqlstate: IntPtr;
    var errcodep: sb4; bufp: IntPtr; bufsiz: ub4; htype: ub4 ): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIHandleAlloc = function (parenth: IntPtr; var hndlpp: pOCIHandle; htype: ub4;
    xtramem_sz: NativeUInt; usrmempp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIHandleFree = function (hndlp: pOCIHandle; htype: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIInitialize = function (mode: ub4; ctxp: IntPtr; malocfp: IntPtr;
    ralocfp: IntPtr; mfreefp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILdaToSvcCtx = function (var svchpp: pOCISvcCtx; errhp: pOCIError; ldap: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIParamGet = function (hndlp: pOCIHandle; htype: ub4; errhp: pOCIError;
    var parmdpp: pOCIParam; pos: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPasswordChange = function (svchp: pOCISvcCtx; errhp: pOCIError; const user_name: IntPtr;
    usernm_len: ub4; const opasswd: IntPtr; opasswd_len: ub4;
    const npasswd: IntPtr; npasswd_len: sb4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIReset = function (hndlp: pOCIHandle; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIServerAttach = function (srvhp: pOCIServer; errhp: pOCIError; dblink: IntPtr;
    dblink_len: sb4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIServerDetach = function (srvhp: pOCIServer; errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIServerVersion = function (hndlp: IntPtr; errhp: pOCIError; bufp: IntPtr; bufsz: ub4; hndltype: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionBegin = function (svchp: pOCISvcCtx; errhp: pOCIError; usrhp: pOCISession;
    credt: ub4; mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionEnd = function (svchp: pOCISvcCtx; errhp: pOCIError; usrhp: pOCISession;
    mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionGet = function(envhp: pOCIEnv; errhp: pOCIError; var svchp: pOCISvcCtx;
    authhp: pOCIAuthInfo; poolName: IntPtr; poolName_len: ub4;
    tagInfo: IntPtr; tagInfo_len: ub4; var retTagInfo: IntPtr;
    var retTagInfo_len: ub4; var found: longbool; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionRelease = function(svchp: pOCISvcCtx; errhp: pOCIError; tag: IntPtr;
    tag_len: ub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionPoolCreate = function (envhp: pOCIEnv; errhp: pOCIError; spoolhp: pOCISpool;
    var poolName: IntPtr; var poolNameLen: ub4; connStr: IntPtr; connStrLen: ub4;
    sessMin: ub4; sessMax: ub4; sessIncr: ub4; userid: IntPtr; useridLen: ub4;
    password: IntPtr; passwordLen: ub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISessionPoolDestroy = function(spoolhp: pOCISPool; errhp: pOCIError;
    mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransStart = function (svchp: pOCISvcCtx; errhp: pOCIError; timeout: word;
    flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransRollback = function (svchp:pOCISvcCtx; errhp:pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransCommit = function (svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransDetach = function (svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransPrepare = function (svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITransForget = function (svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtExecute = function (svchp: pOCISvcCtx; stmtp: pOCIStmt; errhp: pOCIError;
    iters: ub4; rowoff: ub4; snap_in: pOCISnapshot; snap_out: pOCISnapshot;
    mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtFetch = function (stmtp: pOCIStmt; errhp: pOCIError; nrows: ub4; orientation: ub2;
    mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtGetPieceInfo = function (stmtp: pOCIStmt; errhp: pOCIError; var hndlpp: pOCIHandle;
    htypep: pub4; in_outp: pub1; iterp: pub4; idxp: pub4;
    piecep: pub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtPrepare = function (stmtp: pOCIStmt; errhp: pOCIError; stmt: IntPtr;
    stmt_len: ub4; language: ub4; mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtPrepare2 = function(svchp: pOCISvcCtx; var stmtp: pOCIStmt; errhp: pOCIError;
    stmt: IntPtr; stmt_len: ub4; key: IntPtr; key_len: ub4; language: ub4;
    mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  // Direct mode only
  _OCIStmtPrepare3 = function (svchp: pOCISvcCtx; stmtp: pOCIStmt; errhp: pOCIError;
    stmt: IntPtr; stmt_len: ub4; language: ub4; mode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtRelease = function(stmtp: pOCIStmt; errhp: pOCIError; key: IntPtr; key_len: ub4;
    mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtGetNextResult = function(stmtp: pOCIStmt; errhp: pOCIError;
    ppresult: ppOCIStmt; out rtype: ub4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStmtSetPieceInfo = function (hndlp: pOCIHandle; htype: ub4; errhp: pOCIError;
    const bufp: IntPtr; alenp: pub4; piece: ub1;
    const indp: IntPtr; rcodep: pub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISvcCtxToLda = function (srvhp: pOCISvcCtx; errhp: pOCIError; ldap: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

{ LOB supports }

  _OCILobAppend = function (svchp: pOCISvcCtx; errhp: pOCIError; dst_locp: pOCILobLocator;
    src_locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobAssign = function (envhp: pOCIEnv; errhp: pOCIError;
    const src_locp: pOCILobLocator; var dst_locpp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}


  _OCILobCharSetForm = function (envhp: pOCIEnv; errhp: pOCIError;
    const locp: pOCILobLocator; var csfrm: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobCharSetId = function (envhp: pOCIEnv; errhp: pOCIError;
    const locp: pOCILobLocator; csid: pub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobCopy = function (svchp: pOCISvcCtx; errhp: pOCIError; dst_locp: pOCILobLocator;
    src_locp: pOCILobLocator; amount: ub4; dst_offset: ub4;
    src_offset: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobOpen = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator; mode: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobClose = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobIsOpen = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator; var flag: LongBool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobCreateTemporary = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    csid: ub2; csfrm: ub1; lobtype: ub1; cache: tbool; duration: OCIDuration): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFreeTemporary = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobIsTemporary = function (envhp: pOCIEnv; errhp: pOCIError; locp: pOCILobLocator;
    var is_temporary: LongBool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobDisableBuffering = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobEnableBuffering = function (svchp: pOCISvcCtx; errhp: pOCIError;
    locp: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobErase = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    amount: pub4; offset: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileClose = function (svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileExists = function (svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
    var flag: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileGetName = function (envhp: pOCIEnv; errhp: pOCIError; const filep: pOCILobLocator;
    dir_alias: IntPtr; d_length: pub2; filename: IntPtr; f_length: pub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileIsOpen = function (svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
    var flag: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileOpen = function (svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
    mode: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFileSetName = function (envhp: pOCIEnv; errhp: pOCIError; filepp: ppOCILobLocator;
    const dir_alias: IntPtr; d_length: ub2; const filename: IntPtr;
    f_length: ub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobFlushBuffer = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    flag: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobGetLength = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    var lenp: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobIsEqual = function (envhp: pOCIEnv; const x: pOCILobLocator; const y: pOCILobLocator;
    is_equal: pbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobLoadFromFile = function (svchp: pOCISvcCtx; errhp: pOCIError;
    dst_locp: pOCILobLocator; src_locp: pOCILobLocator;
    amount: ub4; dst_offset: ub4; src_offset: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobLocatorIsInit = function (envhp: pOCIEnv; errhp: pOCIError; const locp: pOCILobLocator;
    var is_initialized: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobRead = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    var amtp: ub4; offset: ub4; bufp: IntPtr; bufl: ub4; ctxp: IntPtr;
    cbfp: IntPtr; csid: ub2; csfrm: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobRead2 = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    var byte_amtp: ub8; var char_amtp: ub8; offset: ub8; bufp: IntPtr; bufl: ub8;
    piece: ub1; ctxp: IntPtr; cbfp: IntPtr; csid: ub2; csfrm: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobTrim = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    newlen: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCILobWrite = function (svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
    var amtp: ub4; offset: ub4; bufp: IntPtr; bufl: ub4; piece: ub1;
    ctxp: IntPtr; cbfp: IntPtr; csid: ub2; csfrm: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

{ Objects supports }

  TGetFlushRef = function (context: IntPtr; last: pub1): pOCIRef; cdecl;

  _OCICacheFlush = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    context: IntPtr; get: TGetFlushRef; var ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICacheFree = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  TGetRefreshRef = function (context: IntPtr): pOCIRef; cdecl;

  _OCICacheRefresh = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    option: OCIRefreshOpt; context: IntPtr; get: TGetRefreshRef;
    var ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICacheUnmark = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICacheUnpin = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword;{$IFNDEF CLR} cdecl; {$ENDIF}


  _OCIObjectCopy = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    source: IntPtr; null_source: IntPtr; target: IntPtr;
    null_target: IntPtr; tdo: pOCIType; duration: OCIDuration;
    option: ub1): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectExists = function (env: pOCIEnv; err: pOCIError; ins: IntPtr; var exist: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectFlush = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectFree = function (env: pOCIEnv; err: pOCIError; instance: IntPtr; flags: ub2): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  // Direct mode only
  _OCIObjectPtr = function (env: pOCIEnv; err: pOCIError; instance: IntPtr): IntPtr;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetAttr = function (env: pOCIEnv; err: pOCIError; instance: IntPtr;
    null_struct: IntPtr; tdo: pOCIType; namesp: IntPtr;
    const lengths: pub4; const name_count: ub4; const indexes: pub4;
    const index_count: ub4; attr_null_status: pOCIInd;
    attr_null_structp: IntPtr; attr_valuep: IntPtr; attr_tdop: ppOCIType): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetInd = function (env: pOCIEnv; err: pOCIError; instance: IntPtr; null_structp: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetObjectRef = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr;
    object_ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetProperty = function (env: pOCIEnv; err: pOCIError; const obj: IntPtr;
    propertyId: OCIObjectPropId; prop: OCIObjectPropId; size: pub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectGetTypeRef = function (env: pOCIEnv; err: pOCIError; instance: IntPtr;
    type_ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectIsDirty = function (env: pOCIEnv; err: pOCIError; ins: IntPtr; var dirty: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectIsLocked = function (env: pOCIEnv; err: pOCIError; ins: IntPtr; var lock: tbool): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectLock = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectMarkDelete = function (env: pOCIEnv; err: pOCIError; instance: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectMarkDeleteByRef = function (env: pOCIEnv; err: pOCIError; object_ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectMarkUpdate = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectNew = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    typecode: OCITypeCode; tdo: pOCIType; table: IntPtr;
    duration: OCIDuration; value: tbool; var instance: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectPin = function (env: pOCIEnv; err: pOCIError; object_ref: pOCIRef;
    corhdl: pOCIComplexObject; pin_option: OCIPinOpt; pin_duration: OCIDuration;
    lock_option: OCILockOpt; pobjectp: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectPin2 = function (env: pOCIEnv; const svc: pOCISvcCtx; err: pOCIError; object_ref: pOCIRef;
    corhdl: pOCIComplexObject; pin_option: OCIPinOpt; pin_duration: OCIDuration;
    lock_option: OCILockOpt; pobjectp: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectPinCountReset = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectPinTable = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    const schema_name: IntPtr; s_n_length: ub4; const object_name: IntPtr;
    o_n_length: ub4; not_used: IntPtr; pin_duration: OCIDuration;
    var pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectRefresh = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectSetAttr = function (env: pOCIEnv; err: pOCIError; instance: IntPtr;
    null_struct: IntPtr; tdo: pOCIType; namesp: IntPtr;
    const lengths: pub4; const name_count: ub4; const indexes: pub4;
    const index_count: ub4; const null_status: OCIInd;
    const attr_null_struct: IntPtr; const attr_value: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectUnmark = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectUnmarkByRef = function (env: pOCIEnv; err: pOCIError; ref: pOCIRef): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIObjectUnpin = function (env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITypeByName = function (env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
    const schema_name: IntPtr; s_length: ub4; const type_name: IntPtr;
    t_length: ub4; version_name: IntPtr; v_length: ub4;
    pin_duration: OCIDuration; get_option: OCITypeGetOpt; var tdo: pOCIType): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITypeByRef = function (env: pOCIEnv; err: pOCIError; const type_ref: pOCIRef;
    pin_duration: OCIDuration; get_option: OCITypeGetOpt; tdo: pOCIType): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

{ OTS types }

  _OCICollAppend = function (env: pOCIEnv; err: pOCIError; const elem: IntPtr;
    const elemind: IntPtr; coll: pOCIColl): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollAssign = function (env: pOCIEnv; err: pOCIError; const rhs: pOCIColl;
    lhs: pOCIColl): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollAssignElem = function (env: pOCIEnv; err: pOCIError; index: sb4;
    const elem: IntPtr; const elemind: IntPtr; coll: pOCIColl): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollGetElem = function (env: pOCIEnv; err: pOCIError; const coll: pOCIColl;
    index: sb4; var exists: tbool; var elem: IntPtr; var elemind: IntPtr): sword;{$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollMax = function (env: pOCIEnv; const coll: pOCIColl): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollSize = function (env: pOCIEnv; err: pOCIError; const coll: pOCIColl; var size: sb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCICollTrim = function (env: pOCIEnv; err: pOCIError; trim_num: sb4; coll: pOCIColl): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateAssign = function (err: pOCIError; const from: pOCIDate; todate: pOCIDate): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateFromText = function (err: pOCIError; date_str: IntPtr; d_str_length: ub4;
    const fmt: IntPtr; fmt_length: ub1; const lang_name: IntPtr;
    lang_length: ub4; date: pOCIDate): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateGetDate = function (const date: pOCIDate; year: psb2; month: pub1; day: pub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateGetTime = function (const date: pOCIDate; hour: pub1; min: pub1; sec: pub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateSetDate = function (date: pOCIDate; year: sb2; month: ub1; day: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateSetTime = function (date: pOCIDate; hour: ub1; min: ub1; sec: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateToText = function (err: pOCIError; const date: pOCIDate; const fmt: IntPtr;
    fmt_length: ub1; const lang_name: IntPtr; lang_length: ub4;
    buf_size: pub4; buf: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberAssign = function (err: pOCIError; const from: pOCINumber; tonum: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberCmp = function ( err: pOCIError; const number1: pOCINumber;
    const number2: pOCINumber; var result: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberFromInt = function (err: pOCIError; var inum: int64;
    inum_length: uword; inum_s_flag: uword; number: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberFromReal = function (err: pOCIError; var rnum: double; rnum_length: uword;
    number: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberFromText = function (err: pOCIError; const str: IntPtr; str_length: ub4;
    const fmt: IntPtr; fmt_length: ub4; const nls_params: IntPtr;
    nls_p_length: ub4; number: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberToInt = function (err: pOCIError; number: pOCINumber; rsl_length: uword;
    rsl_flag: uword; var rsl: int64): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberToReal = function (err: pOCIError; const number: pOCINumber; rsl_length: uword;
    var rsl: double): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCINumberToText = function (err: pOCIError; number: pOCINumber;
    const fmt: IntPtr; fmt_length: ub4; const nls_params: IntPtr;
    nls_p_length: ub4; var buf_size: ub4; buf: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefAssign = function (env: pOCIEnv; err: pOCIError; const source: pOCIRef;
    var target: pOCIRef): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefClear = function (env: pOCIEnv; ref: pOCIRef): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefIsEqual = function (env: pOCIEnv; const x: pOCIRef; const y: pOCIRef): tbool; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefIsNull = function (env: pOCIEnv; const ref: pOCIRef): tbool; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIRefToHex = function (env: pOCIEnv; err: pOCIError; const ref: pOCIRef;
    hex: IntPtr; var hex_length: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringAllocSize = function (env: pOCIEnv; err: pOCIError; const vs: pOCIString; allocsize: pub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringAssign = function (env: pOCIEnv; err: pOCIError; const rhs: pOCIString;
    lhs: ppOCIString): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringAssignText = function (env: pOCIEnv; err: pOCIError; const rhs: IntPtr;
    rhs_len: ub4; lhs: ppOCIString): sword; {$IFNDEF CLR} cdecl; {$ENDIF}// Oracle documentation bag rhs_len: ub2;

  _OCIStringPtr = function (env: pOCIEnv; const vs: pOCIString): IntPtr; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringResize = function (env: pOCIEnv; err: pOCIError; new_size: ub4; str: ppOCIString): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIStringSize = function (env: pOCIEnv; const vs: pOCIString): ub4; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableDelete = function (env: pOCIEnv; err: pOCIError; index: sb4; tbl: pOCITable): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableExists = function (env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
    index: sb4; exists: pbool): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableFirst = function (env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
    index: psb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableLast = function (env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
    index: psb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableNext = function (env: pOCIEnv; err: pOCIError; index: sb4; const tbl: pOCITable;
    var next_index: sb4; var exists: tbool): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITablePrev = function (env: pOCIEnv; err: pOCIError; index: sb4; const tbl: pOCITable;
    prev_index: psb4; exists: pbool): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCITableSize = function (env: pOCIEnv; err: pOCIError; const tbl: pOCITable; var size: sb4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ OCI81 }

  _OCIEnvCreate = function (var envhpp: pOCIEnv; mode: ub4; const ctxp: IntPtr;
    const malocfp: IntPtr; const ralocfp: IntPtr;
    const mfreefp: IntPtr; xtramemsz: NativeUInt; usrmempp: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ OCI90 }

  _OCIEnvNlsCreate = function (var envhpp: pOCIEnv; mode: ub4; const ctxp: IntPtr;
    const malocfp: IntPtr; const ralocfp: IntPtr;
    const mfreefp: IntPtr; xtramemsz: NativeUInt; usrmempp: IntPtr; charset: ub2; ncharset: ub2): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Direct path load interface support }

  _OCIDirPathAbort = function (dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayEntryGet = function (dpca: pOCIDirPathColArray; errhp: pOCIError;
    rownum: ub4; colIdx: ub2; var cvalpp: pub1; clenp: ub4;
    cflgp: pub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayEntrySet = function (dpca: pOCIDirPathColArray; errhp: pOCIError;
    rownum: ub4; colIdx: ub2; cvalp: pub1; clen: ub4;
    cflg: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayRowGet = function (dpca: pOCIDirPathColArray; errhp: pOCIError;
    rownum: ub4; var cvalppp: pub1; var clenpp: pub4;
    cflgpp: ppub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayReset = function (dpca: pOCIDirPathColArray; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathColArrayToStream = function (dpca: pOCIDirPathColArray; const dpctx: pOCIDirPathCtx;
    dpstr: pOCIDirPathStream; errhp: pOCIError; rowcnt: ub4;
    rowoff: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathFinish = function (dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathLoadStream = function (dpctx: pOCIDirPathCtx; dpstr: pOCIDirPathStream;
    errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathPrepare = function (dpctx: pOCIDirPathCtx; svchp: pOCISvcCtx;
    errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDirPathStreamReset = function (dpstr: pOCIDirPathStream; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ OCI9 }

{ Timestamp and interval types support }

  _OCIDateTimeConstruct = function (hndl: IntPtr; err: pOCIError;
    datetime: pOCIDateTime; year: sb2; month, day, hour, min, sec: ub1; fsec: ub4;
    timezone: IntPtr; timezone_length: integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeCheck = function (hndl: IntPtr; err: pOCIError; date: pOCIDateTime;
		var valid: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeFromText = function (hndl: IntPtr; err: pOCIError;
    date_str: IntPtr; d_str_length: integer; fmt: IntPtr; fmt_length: ub1; lang_name: IntPtr;
    lang_length: integer; date: pOCIDateTime): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeToText = function (hndl: IntPtr; err: pOCIError; date: pOCIDateTime;
    fmt: IntPtr; fmt_length, fsprec: ub1; lang_name: IntPtr; lang_length: integer;
    var buf_size: ub4; buf: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeGetDate = function (hndl: IntPtr; err: pOCIError;
    date: pOCIDateTime; var year: sb2; var month: ub1; var day: ub1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeGetTime = function (hndl: IntPtr; err: pOCIError;
    datetime: pOCIDateTime; var hour: ub1; var minute: ub1; var sec: ub1;
    var fsec: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeGetTimeZoneOffset = function (hndl: IntPtr; err: pOCIError;
    datetime: pOCIDateTime; var hour: sb1; var minute: sb1): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeGetTimeZoneName = function (hndl: IntPtr; err: pOCIError;
    datetime: pOCIDateTime; buf: pub1; var buflen: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeAssign = function (hndl: IntPtr; err: pOCIError; src: pOCIDateTime;
    dst: pOCIDateTime): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDateTimeCompare = function (hndl: IntPtr; err: pOCIError; const date1: pOCIDateTime;
    const date2: pOCIDateTime; var result: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalFromText = function (hndl: IntPtr; err: pOCIError; inpstr: IntPtr;
		str_len: integer; result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalToText = function (hndl: IntPtr; err: pOCIError; inter: pOCIInterval;
    lfprec, fsprec: ub1; buffer: IntPtr; buflen: NativeUInt;
    var resultlen: NativeUInt): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalCheck = function (hndl: IntPtr; err: pOCIError; interval: pOCIInterval;
		var valid: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalAssign = function (hndl: IntPtr; err: pOCIError; ininter: pOCIInterval;
		outinter: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalCompare = function (hndl: IntPtr; err: pOCIError; inter1: pOCIInterval;
    inter2: pOCIInterval; var result: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalSetYearMonth = function (hndl: IntPtr; err: pOCIError; yr, mnth: sb4;
    result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalGetYearMonth = function (hndl: IntPtr; err: pOCIError; var yr: sb4; var mnth: sb4;
    result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalSetDaySecond = function (hndl: IntPtr; err: pOCIError; dy, hr,
    mm, ss, fsec: sb4; result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalGetDaySecond = function (hndl: IntPtr; err: pOCIError; var dy: sb4; var hr: sb4;
    var mm: sb4; var ss: sb4; var fsec: sb4; result: pOCIInterval): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIIntervalFromNumber = function (hndl: IntPtr; err: pOCIError; interval: pOCIInterval;
    number: pOCINumber): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Scrollable cursors }

  _OCIStmtFetch2 = function (stmtp: pOCIStmt; errhp: pOCIError; nrows: ub4; orientation: ub2;
    scrollOffset: sb4; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ OCI10 }
  _OCIClientVersion = function (var major_version, minor_version, update_num, patch_num, port_update_num: sword): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPing = function (svchp: pOCISvcCtx; errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ XMLType supports }

  pOCIXMLType = IntPtr;
  pOCIDOMDocument = IntPtr;
  ppOCIXMLType = IntPtr;
  ppOCIDOMDocument = IntPtr;
{$IFNDEF CLR}
  OCIXMLType = record end;
  OCIDOMDocument = record end;
{$ENDIF}


  _OCIXMLTypeNew = function (svchp: pOCISvcCtx; errhp: pOCIError; dur: OCIDuration;
    elname: PAnsiChar; elname_Len: ub4; schemaURL: PAnsiChar;
    schemaURL_Len: ub4; var retInstance: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeCreateFromSrc = function (svchp: pOCISvcCtx; errhp: pOCIError; dur: OCIDuration;
    src_type: ub1; src_ptr: IntPtr; ind: sb4; var retInstance: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeExtract = function (errhp: pOCIError; doc: pOCIXMLType; dur: OCIDuration;
    xpathexpr: PAnsiChar; xpathexpr_Len: ub4; nsmap: PAnsiChar; nsmap_Len: ub4;
    var retDoc: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeTransform = function (errhp: pOCIError; dur: OCIDuration;
    doc: pOCIXMLType; xsldoc: pOCIXMLType;
    var retDoc: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeExists = function (errhp: pOCIError; doc: pOCIXMLType;
    xpathexpr: PAnsiChar; xpathexpr_Len: ub4; nsmap: PAnsiChar; nsmap_Len: ub4;
    var retval: Cardinal): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeIsSchemaBased = function (errhp: pOCIError;
    doc: pOCIXMLType; var retval: Cardinal): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeGetSchema = function (errhp: pOCIError; doc: pOCIXMLType;
    var schemadoc: pOCIXMLType; var schemaURL: IntPtr; var schemaURL_Len: ub4;
    var rootelem: IntPtr; var rootelem_Len: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeValidate = function (errhp: pOCIError; doc: pOCIXMLType;
    schemaURL: PAnsiChar; schemaURL_Len: ub4; var retval: Cardinal): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeGetDOM = function (errhp: pOCIError; doc: pOCIXMLType; dur: OCIDuration;
    var retDom: pOCIDOMDocument): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIXMLTypeGetFromDOM = function (errhp: pOCIError; domdoc: pOCIDOMDocument;
    var retXMLType: pOCIXMLType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIDOMFree = function (errhp: pOCIError; domdoc: pOCIDOMDocument): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPStreamFromXMLType = function(errhp: pOCIError; phOCIDescriptor: IntPtr; pobject: IntPtr; res: integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPStreamRead = function(errhp: pOCIError; phOCIDescriptor: IntPtr; pStr: IntPtr; var Len: int64; res: integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIPStreamClose = function(errhp: pOCIError; phOCIDescriptor: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ AnyData support }

  pOCIAnyData = IntPtr;

  _OCIAnyDataAccess = function (svchp: pOCISvcCtx; errhp: pOCIError; sdata: pOCIAnyData;
    typecode: OCITypeCode; tdo: pOCIType; null_ind: IntPtr;
    out data_value: IntPtr; out length: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAnyDataConvert = function (svchp: pOCISvcCtx; errhp: pOCIError; typecode: OCITypeCode;
    tdo: pOCIType; duration: OCIDuration; null_ind: IntPtr; data_value: IntPtr; length: ub4;
    sdata: IntPtr): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIAnyDataGetType = function (svchp: pOCISvcCtx; errhp: pOCIError; data: pOCIAnyData;
    out typecode: OCITypeCode; out tdo: pOCIType): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Misc }

  _OCIRowidToChar = function (rowidDesc: IntPtr; outbfp: IntPtr;
    var outbflp: ub2; errhp: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Oracle External procedures support }

  _OCIExtProcGetEnv = function(with_context: pOCIExtProcContext;
    var envh: pOCIEnv; var svch: pOCISvcCtx; var errh: pOCIError): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIExtProcAllocCallMemory = function(with_context: pOCIExtProcContext;
    amount: cardinal): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCIExtProcRaiseExcpWithMsg = function(with_context: pOCIExtProcContext;
    errnum: Integer; errmsg: PAnsiChar; msglen: Integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{ Publish - subscribe support }

  _OCISubscriptionRegister = function(svchp: pOCISvcCtx; var subscrhpp: pOCISubscription;
    count: ub2; errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISubscriptionUnRegister = function(svchp: pOCISvcCtx; subscrhp: pOCISubscription;
    errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISubscriptionEnable = function(subscrhp: pOCISubscription;
    errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OCISubscriptionDisable = function(subscrhp: pOCISubscription;
    errhp: pOCIError; mode: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  { MTS support }
  _OraMTSSvcGet = function(lpUName: PAnsiChar; lpPsswd: PAnsiChar; lpDbnam: PAnsiChar; var pOCISvc: pOCISvcCtx;
                   var pOCIEnv: pOCIEnv; ConFlg: ub4): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSSvcRel = function(OCISvc: pOCISvcCtx): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSJoinTxn = function(svchp: pOCISvcCtx; lpTrans: ICRTransactionSC): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSEnlCtxGet = function(lpUName: PAnsiChar; lpPsswd: PAnsiChar; lpDbnam: PAnsiChar;
                               pOCISvc: pOCISvcCtx; errhp: pOCIError; dwFlags: ub4;
                               var pCtxt: pOCISvcCtx): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSEnlCtxRel = function(pCtxt: pOCISvcCtx): sword; {$IFNDEF CLR} cdecl; {$ENDIF}

  _OraMTSSvcEnlist = function(OCISvc: pOCISvcCtx; OCIErr: pOCIError; lpTrans: ICRTransactionSC;
                       dwFlags: Integer): sword; {$IFNDEF CLR} cdecl; {$ENDIF}
{$ENDIF}
{$ENDIF}

  TOCICallStyle = (None, OCI73, OCI80);
  TOCICallStyleSet = set of TOCICallStyle;

  TOracleHome = class;
  TOracleHomes = class;
  TOCISvcCtx = class;
  TOCIEnvironment = class;
  TEnvironmentClass = class of TOCIEnvironment;

  TOCIAPI = class
  private
    hInitLock: TCriticalSection;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FHome: TOracleHome;
  protected
    FInited: boolean;

  {$IFDEF MSWINDOWS}
    function GetProc(const Name: string): FARPROC;
    function GetOraClientProc(const Name: string): FARPROC;
    class function CheckProc(OCILib: HMODULE; const Name: string): boolean;
  {$ELSE}
    function GetProc(const Name: string): IntPtr;
    function GetOraClientProc(const Name: string): IntPtr;
    {$IFDEF POSIX}
    class function CheckProc(OCILib: NativeUInt; const Name: string): boolean;
    {$ELSE}
    class function CheckProc(OCILib: IntPtr; const Name: string): boolean;
    {$ENDIF}
  {$ENDIF}
  public
    constructor Create(AHome: TOracleHome); virtual;
    destructor Destroy; override;

    procedure Init; virtual; abstract;
    procedure Release; virtual; abstract;

    property Home: TOracleHome read FHome;
    property Inited: boolean read FInited;
  end;

  TOCI7API = class(TOCIAPI)
  public
    obindps  : _obindps;
    obndra   : _obndra;
    obndrn   : _obndrn;
    obndrv   : _obndrv;
    obreak   : _obreak;
    ocan     : _ocan;
    oclose   : _oclose;
    ocof     : _ocof;
    ocom     : _ocom;
    ocon     : _ocon;
    odefin   : _odefin;
    odefinps : _odefinps;
    odescr   : _odescr;
    odessp   : _odessp;
    oerhms   : _oerhms;
    oermsg   : _oermsg;
    oexec    : _oexec;
    oexfet   : _oexfet;
    oexn     : _oexn;
    ofen     : _ofen;
    ofetch   : _ofetch;
    oflng    : _oflng;
    ognfd    : _ognfd;
    olog     : _olog;
    ologof   : _ologof;
    onbclr   : _onbclr;
    onbset   : _onbset;
    onbtst   : _onbtst;
    oopt     : _oopt;
    oopen    : _oopen;
    oparse   : _oparse;
    opinit   : _opinit;
    orol     : _orol;
    ogetpi   : _ogetpi;
    osetpi   : _osetpi;
    sqllda   : _sqllda;
    sqlld2   : _sqlld2;

    procedure Init; override;
    procedure Release; override;

    function GetThreadSafety: boolean;
    procedure SetThreadSafety(Value: boolean);
  end;

  TOCI8API = class(TOCIAPI)
  public
    OCIAttrGet1            : _OCIAttrGet1;
    OCIAttrGet2            : _OCIAttrGet2;
    OCIAttrSet1            : _OCIAttrSet1;
    //OCIAttrSet3            : _OCIAttrSet3;
    OCIAttrSet2            : _OCIAttrSet2;
    OCIBindArrayOfStruct   : _OCIBindArrayOfStruct;
    OCIBindByName          : _OCIBindByName;
    OCIBindByPos           : _OCIBindByPos;
    OCIBindDynamic         : _OCIBindDynamic;
    OCIBindObject          : _OCIBindObject;
    OCIBreak               : _OCIBreak;
    OCIDefineArrayOfStruct : _OCIDefineArrayOfStruct;
    OCIDefineByPos         : _OCIDefineByPos;
    OCIDefineDynamic       : _OCIDefineDynamic;
    OCIDefineObject        : _OCIDefineObject;
    OCIDescribeAny         : _OCIDescribeAny;
    OCIDescriptorAlloc     : _OCIDescriptorAlloc;
    OCIDescriptorFree      : _OCIDescriptorFree;
    OCIEnvInit             : _OCIEnvInit;
    OCIErrorGet            : _OCIErrorGet;
    OCIHandleAlloc         : _OCIHandleAlloc;
    OCIHandleFree          : _OCIHandleFree;
    OCIInitialize          : _OCIInitialize;
    OCILdaToSvcCtx         : _OCILdaToSvcCtx;
    OCIParamGet            : _OCIParamGet;
    OCIPasswordChange      : _OCIPasswordChange;
    OCIReset               : _OCIReset;
    OCIServerAttach        : _OCIServerAttach;
    OCIServerDetach        : _OCIServerDetach;
    OCIServerVersion       : _OCIServerVersion;
    OCISessionBegin        : _OCISessionBegin;
    OCISessionEnd          : _OCISessionEnd;
    OCISessionGet          : _OCISessionGet;
    OCISessionRelease      : _OCISessionRelease;
    OCISessionPoolCreate   : _OCISessionPoolCreate;
    OCISessionPoolDestroy  : _OCISessionPoolDestroy;
    OCITransStart          : _OCITransStart;
    OCITransRollback       : _OCITransRollback;
    OCITransCommit         : _OCITransCommit;
    OCITransDetach         : _OCITransDetach;
    OCITransPrepare        : _OCITransPrepare;
    OCITransForget         : _OCITransForget;
    OCIStmtExecute         : _OCIStmtExecute;
    OCIStmtFetch           : _OCIStmtFetch;
    OCIStmtFetch2          : _OCIStmtFetch2;
    OCIStmtGetPieceInfo    : _OCIStmtGetPieceInfo;
    OCIStmtPrepare         : _OCIStmtPrepare;
    OCIStmtPrepare2        : _OCIStmtPrepare2;
    OCIStmtPrepare3        : _OCIStmtPrepare3; // Direct mode only
    OCIStmtRelease         : _OCIStmtRelease;
    OCIStmtGetNextResult   : _OCIStmtGetNextResult;
    OCIStmtSetPieceInfo    : _OCIStmtSetPieceInfo;
    OCISvcCtxToLda         : _OCISvcCtxToLda;

    OCILobAppend           : _OCILobAppend;
    OCILobAssign           : _OCILobAssign;
    OCILobCharSetForm      : _OCILobCharSetForm;
    OCILobCharSetId        : _OCILobCharSetId;
    OCILobCopy             : _OCILobCopy;
    OCILobOpen             : _OCILobOpen;
    OCILobClose            : _OCILobClose;
    OCILobIsOpen           : _OCILobIsOpen;
    OCILobCreateTemporary  : _OCILobCreateTemporary;
    OCILobFreeTemporary    : _OCILobFreeTemporary;
    OCILobIsTemporary      : _OCILobIsTemporary;
    OCILobDisableBuffering : _OCILobDisableBuffering;
    OCILobEnableBuffering  : _OCILobEnableBuffering;
    OCILobErase            : _OCILobErase;
    OCILobFileClose        : _OCILobFileClose;
    OCILobFileExists       : _OCILobFileExists;
    OCILobFileGetName      : _OCILobFileGetName;
    OCILobFileIsOpen       : _OCILobFileIsOpen;
    OCILobFileOpen         : _OCILobFileOpen;
    OCILobFileSetName      : _OCILobFileSetName;
    OCILobFlushBuffer      : _OCILobFlushBuffer;
    OCILobGetLength        : _OCILobGetLength;
    OCILobIsEqual          : _OCILobIsEqual;
    OCILobLoadFromFile     : _OCILobLoadFromFile;
    OCILobLocatorIsInit    : _OCILobLocatorIsInit;
    OCILobRead             : _OCILobRead;
    OCILobRead2            : _OCILobRead2;
    OCILobTrim             : _OCILobTrim;
    OCILobWrite            : _OCILobWrite;

    OCICacheFlush          : _OCICacheFlush;
    OCICacheFree           : _OCICacheFree;
    OCICacheRefresh        : _OCICacheRefresh;
    OCICacheUnmark         : _OCICacheUnmark;
    OCICacheUnpin          : _OCICacheUnpin;

    OCIObjectCopy          : _OCIObjectCopy;
    OCIObjectExists        : _OCIObjectExists;
    OCIObjectFlush         : _OCIObjectFlush;
    OCIObjectFree          : _OCIObjectFree;
    OCIObjectPtr           : _OCIObjectPtr; // Direct mode only
    OCIObjectGetAttr       : _OCIObjectGetAttr;
    OCIObjectGetInd        : _OCIObjectGetInd;
    OCIObjectGetObjectRef  : _OCIObjectGetObjectRef;
    OCIObjectGetProperty   : _OCIObjectGetProperty;
    OCIObjectGetTypeRef    : _OCIObjectGetTypeRef;
    OCIObjectIsDirty       : _OCIObjectIsDirty;
    OCIObjectIsLocked      : _OCIObjectIsLocked;
    OCIObjectLock          : _OCIObjectLock;
    OCIObjectMarkDelete    : _OCIObjectMarkDelete;
    OCIObjectMarkDeleteByRef : _OCIObjectMarkDeleteByRef;
    OCIObjectMarkUpdate    : _OCIObjectMarkUpdate;
    OCIObjectNew           : _OCIObjectNew;
    OCIObjectPin           : _OCIObjectPin;
    OCIObjectPin2          : _OCIObjectPin2; // Direct mode only
    OCIObjectPinCountReset : _OCIObjectPinCountReset;
    OCIObjectPinTable      : _OCIObjectPinTable;
    OCIObjectRefresh       : _OCIObjectRefresh;
    OCIObjectSetAttr       : _OCIObjectSetAttr;
    OCIObjectUnmark        : _OCIObjectUnmark;
    OCIObjectUnmarkByRef   : _OCIObjectUnmarkByRef;
    OCIObjectUnpin         : _OCIObjectUnpin;
    OCITypeByName          : _OCITypeByName;
    OCITypeByRef           : _OCITypeByRef;

    OCICollAppend          : _OCICollAppend;
    OCICollAssign          : _OCICollAssign;
    OCICollAssignElem      : _OCICollAssignElem;
    OCICollGetElem         : _OCICollGetElem;
    OCICollMax             : _OCICollMax;
    OCICollSize            : _OCICollSize;
    OCICollTrim            : _OCICollTrim;
    OCIDateAssign          : _OCIDateAssign;
    OCIDateFromText        : _OCIDateFromText;
    OCIDateGetDate         : _OCIDateGetDate;
    OCIDateGetTime         : _OCIDateGetTime;
    OCIDateSetDate         : _OCIDateSetDate;
    OCIDateSetTime         : _OCIDateSetTime;
    OCIDateToText          : _OCIDateToText;
    OCINumberAssign        : _OCINumberAssign;
    OCINumberCmp           : _OCINumberCmp;
    OCINumberFromInt       : _OCINumberFromInt;
    OCINumberFromReal      : _OCINumberFromReal;
    OCINumberFromText      : _OCINumberFromText;
    OCINumberToInt         : _OCINumberToInt;
    OCINumberToReal        : _OCINumberToReal;
    OCINumberToText        : _OCINumberToText;
    OCIRefAssign           : _OCIRefAssign;
    OCIRefClear            : _OCIRefClear;
    OCIRefIsEqual          : _OCIRefIsEqual;
    OCIRefIsNull           : _OCIRefIsNull;
    OCIRefToHex            : _OCIRefToHex;
    OCIStringAllocSize     : _OCIStringAllocSize;
    OCIStringAssign        : _OCIStringAssign;
    OCIStringAssignText    : _OCIStringAssignText;
    OCIStringPtr           : _OCIStringPtr;
    OCIStringResize        : _OCIStringResize;
    OCIStringSize          : _OCIStringSize;
    OCITableDelete         : _OCITableDelete;
    OCITableExists         : _OCITableExists;
    OCITableFirst          : _OCITableFirst;
    OCITableLast           : _OCITableLast;
    OCITableNext           : _OCITableNext;
    OCITablePrev           : _OCITablePrev;
    OCITableSize           : _OCITableSize;

    OCIEnvCreate               : _OCIEnvCreate;
    OCIEnvNlsCreate            : _OCIEnvNlsCreate;

    OCIDirPathAbort            : _OCIDirPathAbort;
    OCIDirPathColArrayEntryGet : _OCIDirPathColArrayEntryGet;
    OCIDirPathColArrayEntrySet : _OCIDirPathColArrayEntrySet;
    OCIDirPathColArrayRowGet   : _OCIDirPathColArrayRowGet;
    OCIDirPathColArrayReset    : _OCIDirPathColArrayReset;
    OCIDirPathColArrayToStream : _OCIDirPathColArrayToStream;
    OCIDirPathFinish           : _OCIDirPathFinish;
    OCIDirPathLoadStream       : _OCIDirPathLoadStream;
    OCIDirPathPrepare          : _OCIDirPathPrepare;
    OCIDirPathStreamReset      : _OCIDirPathStreamReset;

    OCIDateTimeConstruct          : _OCIDateTimeConstruct;
    OCIDateTimeCheck              : _OCIDateTimeCheck;
    OCIDateTimeFromText           : _OCIDateTimeFromText;
    OCIDateTimeToText             : _OCIDateTimeToText;
    OCIDateTimeGetDate            : _OCIDateTimeGetDate;
    OCIDateTimeGetTime            : _OCIDateTimeGetTime;
    OCIDateTimeGetTimeZoneOffset  : _OCIDateTimeGetTimeZoneOffset;
    OCIDateTimeGetTimeZoneName    : _OCIDateTimeGetTimeZoneName;
    OCIDateTimeAssign             : _OCIDateTimeAssign;
    OCIDateTimeCompare            : _OCIDateTimeCompare;
    OCIIntervalFromText           : _OCIIntervalFromText;
    OCIIntervalToText             : _OCIIntervalToText;
    OCIIntervalCheck              : _OCIIntervalCheck;
    OCIIntervalAssign             : _OCIIntervalAssign;
    OCIIntervalCompare            : _OCIIntervalCompare;
    OCIIntervalSetYearMonth       : _OCIIntervalSetYearMonth;
    OCIIntervalGetYearMonth       : _OCIIntervalGetYearMonth;
    OCIIntervalSetDaySecond       : _OCIIntervalSetDaySecond;
    OCIIntervalGetDaySecond       : _OCIIntervalGetDaySecond;
    OCIIntervalFromNumber         : _OCIIntervalFromNumber;

    OCIPing                       : _OCIPing;

    OCIXMLTypeNew                 : _OCIXMLTypeNew;
    OCIXMLTypeCreateFromSrc       : _OCIXMLTypeCreateFromSrc;
    OCIXMLTypeExtract             : _OCIXMLTypeExtract;
    OCIXMLTypeTransform           : _OCIXMLTypeTransform;
    OCIXMLTypeExists              : _OCIXMLTypeExists;
    OCIXMLTypeIsSchemaBased       : _OCIXMLTypeIsSchemaBased;
    OCIXMLTypeGetSchema           : _OCIXMLTypeGetSchema;
    OCIXMLTypeValidate            : _OCIXMLTypeValidate;
    OCIXMLTypeGetDOM              : _OCIXMLTypeGetDOM;
    OCIXMLTypeGetFromDOM          : _OCIXMLTypeGetFromDOM;
    OCIDOMFree                    : _OCIDOMFree;

    OCIAnyDataAccess              : _OCIAnyDataAccess;
    OCIAnyDataConvert             : _OCIAnyDataConvert;
    OCIAnyDataGetType             : _OCIAnyDataGetType;

    OCIPStreamFromXMLType         : _OCIPStreamFromXMLType;
    OCIPStreamRead                : _OCIPStreamRead;
    OCIPStreamClose               : _OCIPStreamClose;

    OCIRowidToChar                : _OCIRowidToChar;

    OCIExtProcGetEnv              : _OCIExtProcGetEnv;
    OCIExtProcAllocCallMemory     : _OCIExtProcAllocCallMemory;
    OCIExtProcRaiseExcpWithMsg    : _OCIExtProcRaiseExcpWithMsg;

    OCISubscriptionRegister       : _OCISubscriptionRegister;
    OCISubscriptionUnRegister     : _OCISubscriptionUnRegister;
    OCISubscriptionEnable         : _OCISubscriptionEnable;
    OCISubscriptionDisable        : _OCISubscriptionDisable;

    procedure Init; override;
    procedure Release; override;

    procedure DoOraError(ErrorCode: sword; OCISvcCtx: TOCISvcCtx); overload;
    procedure DoOraError(ErrorCode: sword; UnicodeEnv: boolean; hOCIError: pOCIError); overload;
    function GetOraError(ErrorCode: sword; OCISvcCtx: TOCISvcCtx; var ErrorMsg: string): sword; overload;
    function GetOraError(ErrorCode: sword; OCISvcCtx: TOCISvcCtx): sword; overload;
    procedure Check(Status: sword; OCISvcCtx: TOCISvcCtx); overload;
    procedure Check(Status: sword; UnicodeEnv: boolean; hOCIError: pOCIError); overload;

    function GetHeapAlloc(OCISvcCtx: TOCISvcCtx): integer;
    function GetSharedHeapAlloc(OCISvcCtx: TOCISvcCtx): integer;

    function IsUnicodeEnv(hOCIEnv: pOCIEnv; hOCIError: pOCIError): boolean;
  end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  TMTSAPI = class (TOCIAPI)
  private
  {$IFNDEF CLR}
    hMTSLib: HMODULE;
  {$ENDIF}
  public
    OraMTSSvcGet                  : _OraMTSSvcGet;
    OraMTSSvcRel                  : _OraMTSSvcRel;
    OraMTSJoinTxn                 : _OraMTSJoinTxn;
    OraMTSEnlCtxGet               : _OraMTSEnlCtxGet;
    OraMTSEnlCtxRel               : _OraMTSEnlCtxRel;
    OraMTSSvcEnlist               : _OraMTSSvcEnlist;

    procedure Init; override;
    procedure Release; override;
  end;
{$ENDIF}
{$ENDIF}

  TDirectAPI = class (TOCI8API)
  public
    procedure Init; override;
  end;

  TOracleHome = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TOracleHomes;

  {$IFDEF MSWINDOWS}
    FOCILib: HMODULE;
    FOCIClientLib: HMODULE;
  {$ENDIF}
  {$IFDEF POSIX}
    FOCILib: NativeUInt;
    FOCIClientLib: NativeUInt;
  {$ENDIF}
  {$IFDEF UNIX}
    FOCILib: IntPtr;
    FOCIClientLib: IntPtr;
  {$ENDIF}

    FOCI7: TOCI7API;
    FOCI8: TOCI8API;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    FMTS: TMTSAPI;
  {$ENDIF}
  {$ENDIF}

    FEnvironments: TCRObjectList;

    function GetDirect: boolean;
    function GetEnvironment(Index: Integer): TOCIEnvironment;
    function GetEnvironmentCount: Integer;

    function GetOCI7: TOCI7API;
    function GetOCI8: TOCI8API;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    function GetMTS: TMTSAPI;
  {$ENDIF}
  {$ENDIF}
  protected
    FName: string;
    FPath: string;
    FTNSPath: string;
    FNLSLang: string;
    FOCIDLL: string;
    FOCIClientDLL : string;
    FOCIVersionSt  : string;
    FOCIVersion    : word;
    FOCICallStyle  : TOCICallStyle;
    FOCILite: boolean;
    FPossibleOCICallStyles : TOCICallStyleSet;

    function GetEnvironmentClass: TEnvironmentClass; virtual;

    function GetInited: boolean; virtual;
    function CreateOCI8API: TOCI8API; virtual;
  {$IFDEF MSWINDOWS}
    function GetFileVersion(FileName: string): string;
  {$ENDIF}
    procedure DetectOCIClientVersion;

    procedure AddEnvironment(Environment: TOCIEnvironment);
    procedure RemoveEnvironment(Environment: TOCIEnvironment);
    procedure ClearEnvironments;
    function FindEnvironment(Unique: boolean; UnicodeEnv: boolean; SubscriptionPort: Integer): TOCIEnvironment; overload;
    function FindEnvironment(hOCIEnv: pOCIEnv): TOCIEnvironment; overload;
    function CreateEnvironment(Unique: boolean; UnicodeEnv: boolean; SubscriptionPort: Integer): TOCIEnvironment; overload;
    function CreateEnvironment(hOCIEnv: pOCIEnv): TOCIEnvironment; overload;

    procedure LoadOCI;
    procedure FreeOCI;

  {$IFDEF MSWINDOWS}
    property OCILib: HMODULE read FOCILib;
    property OCIClientLib: HMODULE read FOCIClientLib;
  {$ENDIF}
  {$IFDEF POSIX}
    property OCILib: NativeUInt read FOCILib;
    property OCIClientLib: NativeUInt read FOCIClientLib;
  {$ENDIF}
  {$IFDEF UNIX}
    property OCILib: IntPtr read FOCILib;
    property OCIClientLib: IntPtr read FOCIClientLib;
  {$ENDIF}
  public
    constructor Create(AOwner: TOracleHomes); virtual;
    destructor Destroy; override;

    procedure Init; virtual;
    procedure Release; virtual;

    function AllocEnvironment(Unique: boolean; UnicodeEnv: boolean; SubscriptionPort: Integer): TOCIEnvironment; overload;
    function AllocEnvironment(hOCIEnv: pOCIEnv): TOCIEnvironment; overload;

    procedure CheckOCI;
    procedure CheckOCI73;
    procedure CheckOCI80;
    procedure CheckOCI81;
    procedure CheckOCI90;

    property Owner: TOracleHomes read FOwner;
    property Direct: boolean read GetDirect;
    property Inited: boolean read GetInited;
    property Name: string read FName;
    property Path: string read FPath;
    property TNSPath: string read FTNSPath;
    property NLSLang: string read FNLSLang;
    property OCIDLL: string read FOCIDLL;
    property OCIClientDLL: string read FOCIClientDLL;
    property OCIVersionSt: string read FOCIVersionSt;
    property OCIVersion: word read FOCIVersion;
    property OCICallStyle: TOCICallStyle read FOCICallStyle;
    property PossibleOCICallStyles : TOCICallStyleSet read FPossibleOCICallStyles;

    property OCI7: TOCI7API read GetOCI7;
    property OCI8: TOCI8API read GetOCI8;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    property MTS: TMTSAPI read GetMTS;
  {$ENDIF}
  {$ENDIF}

    property Environments[Index: Integer]: TOCIEnvironment read GetEnvironment;
    property EnvironmentCount: Integer read GetEnvironmentCount;
  end;

  TDirectHome = class (TOracleHome)
  protected
    function GetEnvironmentClass: TEnvironmentClass; override;

    function GetInited: boolean; override;
    function CreateOCI8API: TOCI8API; override;
  public
    constructor Create(AOwner: TOracleHomes); override;

    procedure Init; override;
  end;

  TOracleHomes = class
  private
    hInitLock: TCriticalSection;

    FInited: boolean;
    FInDestroying: boolean;
    FHomes: TCRObjectList;
    FDirect: TOracleHome;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDefault: TOracleHome;

    function GetCount: Integer;
    function GetOracleHome(Index: integer): TOracleHome;
    function GetDirect: TOracleHome;
    function GetDefault: TOracleHome;
    procedure SetDefault(Value: TOracleHome);
  protected
    function Add(const Name, HomePath, OCIDLL, TNSPath, NLSLang: string; IsLite: boolean): TOracleHome;
    procedure Clear;

    procedure OCINotFound(const Path: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure Release;

    function AddHome(const Path: string): TOracleHome; overload;
    function AddHome(const Name, Path: string): TOracleHome; overload;
    function AddHome(const Name, Path, TNSPath: string; IsLite: boolean = False): TOracleHome; overload;
    function AddHome(const Name, Path, TNSPath, NLSLang: string; IsLite: boolean = False): TOracleHome; overload;

    function FindHome(const HomeName: string): TOracleHome;
    function GetHome(const HomeName: string): TOracleHome;

    property Inited: boolean read FInited;
    property Homes[Index: integer]: TOracleHome read GetOracleHome; default;
    property Count: Integer read GetCount;
    property Direct: TOracleHome read GetDirect;
    property Default: TOracleHome read GetDefault write SetDefault;
  end;

  TOCIEnvironment = class
  private
    hInitLock: TCriticalSection;

    FHome: TOracleHome;
    FUnique: boolean;
    FUnicodeEnv: boolean;
    FSubscriptionPort: Integer;

    FhOCIEnv: pOCIEnv;
    FhOCIError: pOCIError;
    FNativeHandle: boolean;

    FOCISvcCtxs: TList;

  {$IFNDEF AUTOREFCOUNT}
    FRefCount: Integer;
  {$ENDIF}
  protected
    FInited: boolean;

    procedure AddOCISvcCtx(Value: TOCISvcCtx);
    procedure RemoveOCISvcCtx(Value: TOCISvcCtx);
    procedure ClearOCISvcCtxs;

    procedure OCIError(Res: sword);
    procedure SetSubscriptionPort(Port: Integer);
  public
    constructor Create(AHome: TOracleHome; AUnique: boolean; AUnicodeEnv: boolean; ASubscriptionPort: Integer); overload;
    constructor Create(AHome: TOracleHome; hOCIEnv: pOCIEnv); overload;
    destructor Destroy; override;

  {$IFNDEF AUTOREFCOUNT}
    procedure AddRef;
    procedure ReleaseRef;
  {$ENDIF}

    procedure Init; virtual;
    procedure Release; virtual;

    function AllocErrorHandle: pOCIError;
    procedure FreeErrorHandle(hOCIError: pOCIError);

    function AllocSvcCtxHandle: pOCISvcCtx;
    function AllocPooledSvcCtxHandle(const PoolName: string; hOCIAuthInfo: pOCIAuthInfo): pOCISvcCtx;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    function AllocMTSPooledSvcCtxHandle(const Server, UserName, Password: string): pOCISvcCtx;
  {$ENDIF}
  {$ENDIF}
    procedure FreeSvcCtxHandle(hOCISvcCtx: pOCISvcCtx);

    property Home: TOracleHome read FHome;
    property Inited: boolean read FInited;
    property hOCIEnv: pOCIEnv read FhOCIEnv;
    property Unique: boolean read FUnique;
    property UnicodeEnv: boolean read FUnicodeEnv;
    property SubscriptionPort: Integer read FSubscriptionPort;

    property hOCIError: pOCIError read FhOCIError;
  end;

  TDirectEnvironment = class (TOCIEnvironment)
  public
    procedure Init; override;
  end;

  TOCISvcCtx = class
  private
    FEnvironment: TOCIEnvironment;

    FHome: TOracleHome;
    FOCI7: TOCI7API;
    FOCI8: TOCI8API;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    FMTS: TMTSAPI;
  {$ENDIF}
  {$ENDIF}

    FhOCIEnv: pOCIEnv;
    FhOCISvcCtx: pOCISvcCtx;
    FhOCIError: pOCIError;
    FNativeHandle: boolean;

    FUnicodeEnv: boolean;
    FUseUnicode: boolean;

  {$IFNDEF AUTOREFCOUNT}
    FRefCount: Integer;
  {$ENDIF}

  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    function GetMTS: TMTSAPI;
  {$ENDIF}
  {$ENDIF}
    procedure ReleaseEnvironment;
  protected
    procedure ResetEnvironment;
    procedure SetEnvironment(Value: TOCIEnvironment);
  public
    constructor Create(AEnvironment: TOCIEnvironment; AUseUnicode: boolean); overload;
    constructor Create(AEnvironment: TOCIEnvironment; AhOCISvcCtx: pOCISvcCtx; AUseUnicode: boolean); overload;
    destructor Destroy; override;

  {$IFNDEF AUTOREFCOUNT}
    procedure AddRef;
    procedure ReleaseRef;
  {$ENDIF}

    procedure AllocSvcCtxHandle;
    procedure AllocPooledSvcCtxHandle(const PoolName: string; hOCIAuthInfo: pOCIAuthInfo);
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    procedure AllocMTSPooledSvcCtxHandle(const Server, UserName, Password: string);
  {$ENDIF}
  {$ENDIF}
    procedure FreeSvcCtxHandle;
    procedure Release;

    procedure GetLDA(Value: PLDA);
    procedure SetLDA(Value: PLDA);

    property Environment: TOCIEnvironment read FEnvironment;
    property Home: TOracleHome read FHome;
    property OCI7: TOCI7API read FOCI7;
    property OCI8: TOCI8API read FOCI8;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    property MTS: TMTSAPI read GetMTS;
  {$ENDIF}
  {$ENDIF}

    property hOCIEnv: pOCIEnv read FhOCIEnv;
    property hOCIError: pOCIError read FhOCIError;
    property hOCISvcCtx: pOCISvcCtx read FhOCISvcCtx;

    property UnicodeEnv: boolean read FUnicodeEnv;
    property UseUnicode: boolean read FUseUnicode;
  end;

  TDirectServerInfo = class
  private
    FFullInfo: boolean;
    FProtocol: string;
    FHost: string;
    FPort: string;
    FSID: string;
    FServiceName: string;

    function GetProtocol: string;
    function GetHost: string;
    function GetPort: string;
    function GetSID: string;
    function GetServiceName: string;
  public
    constructor Create;

    procedure Clear; virtual;

    function GetServerInfo: string;
    procedure SetServerInfo(const Value: string);

    property FullInfo: boolean read FFullInfo write FFullInfo;

    property Protocol: string read GetProtocol write FProtocol;
    property Host: string read GetHost write FHost;
    property Port: string read GetPort write FPort;
    property SID: string read GetSID write FSID;
    property ServiceName: string read GetServiceName write FServiceName;
  end;

  EOCIInitError = class(Exception);

  function StringToHGlobalOCI(const S: string; var Size: integer; UnicodeEnv: boolean): IntPtr;
  procedure FreeStringOCI(P: IntPtr; UnicodeEnv: boolean);
  function PtrToStringOCI(P: IntPtr; UnicodeEnv: boolean): string; overload;
  function PtrToStringOCI(P: IntPtr; Size: integer; UnicodeEnv: boolean): string; overload;
  function SizeOfCharOCI(UnicodeEnv: boolean): integer;

  function DefaultOCIDateFormat: string;
  function DefaultOCITimeFormat(Precision: Byte = 6): string;
  function DefaultOCITimeStampFormat(Precision: Byte = 6): string;
  function DefaultOCITimeStampWithTZFormat(Precision: Byte = 6): string;

  function VersionStrToWord(VersionSt: string): word;
  function GetMaxCharLength(Charset: word): Byte;
  function GetFixedCharLength(Charset: word): Byte;
  function GetUTF8Charset(OracleVersion: Word): Integer;
  function GetCodePageCharset(CodePage: Integer; OracleVersion: Word): Word;

{$IFDEF CLR}
const
  OCIDLL = 'oci.dll';
  MTSDLL = 'oramts.dll';
{$ENDIF}
var
  OracleHomes: TOracleHomes;
  { SubscriptionPort is depricated}
  SubscriptionPort: integer;
  ObjectVersion : boolean;
  OCI7ThreadSafety  : boolean;
  { OCIUnicode is depricated}
  OCIUnicode    : boolean;
  OCIThreaded   : boolean;  // OCI_THREADED for OCI80
  OCIMutexed    : boolean;  // OCI_ENV_NO_MUTEX for OCI81 (works with OCI81 only)
  OCIShared     : boolean;  // OCI_SHARED for OCI81
  OCIEvents     : boolean;  // OCI_EVENTS for OCI81
  OCIEventsVersion: word;
  OCIUnicodeAsNational: boolean = False; // bind all Unicode params as NVarchar data type
  OCILite       : boolean;  // Oracle Lite
  OCINCharLiteralReplace: boolean = True;

const
  DACProductName = 'ODAC';

implementation

uses
{$IFDEF CLR}
  {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
  Posix.Dlfcn,
{$ENDIF}
{$IFDEF UNIX}
  dl,
{$ENDIF}
  CRFunctions,
{$IFDEF NET}
  {$IFNDEF UNIDACPRO}OraNet,{$ELSE}OraNetUni,{$ENDIF}
{$ENDIF}
{$IFNDEF UNIDACPRO}
  OraError, OraConsts;
{$ELSE}
  OraErrorUni, OraConstsUni;
{$ENDIF}

const
  DefaultProtocol = 'TCP';
  DefaultHost = 'localhost';
  DefaultPort = '1521';
  DefaultSID = 'orcl';

var
  hHomeLock: TCriticalSection;
  hEnvLock: TCriticalSection;
  hSvcCtxLock: TCriticalSection;


function StringToHGlobalOCI(const S: string; var Size: integer; UnicodeEnv: boolean): IntPtr;
var
{$IFDEF IS_UNICODE}
  sa: AnsiString;
{$ELSE}
  sw: WideString;
{$ENDIF}
begin
  if UnicodeEnv then begin
  {$IFNDEF IS_UNICODE}
    sw := WideString(S);
    Size := Length(sw) * SizeOf(WideChar);
    GetMem(Result, Size + 2);
    Move(PWideChar(sw)^, Result^, Size + 2);
  {$ELSE}
    Size := Length(S) * SizeOf(WideChar);
    Result := PWideChar(S);
  {$ENDIF}
  end
  else begin
  {$IFDEF IS_UNICODE}
    sa := AnsiString(S);
    Size := LengthA(sa);
    GetMem(Result, Size + 1);
    Move(PAnsiChar(sa)^, Result^, Size + 1);
  {$ELSE}
    Size := Length(S);
    Result := PAnsiChar(S);
  {$ENDIF}
  end;
end;

procedure FreeStringOCI(P: IntPtr; UnicodeEnv: boolean);
begin
  if {$IFDEF IS_UNICODE} not {$ENDIF} UnicodeEnv then
    FreeMem(P);
end;

function PtrToStringOCI(P: IntPtr; UnicodeEnv: boolean): string;
begin
  if UnicodeEnv then
    Result := string(Marshal.PtrToStringUni(P))
  else
    Result := string(Marshal.PtrToStringAnsi(P));
end;

function PtrToStringOCI(P: IntPtr; Size: integer; UnicodeEnv: boolean): string;
begin
  if UnicodeEnv then
    Result := string(Marshal.PtrToStringUni(P, Size shr 1))
  else
    Result := string(Marshal.PtrToStringAnsi(P, Size));
end;

function SizeOfCharOCI(UnicodeEnv: boolean): integer;
begin
  if UnicodeEnv then
    Result := 2
  else
    Result := 1;
end;

function VersionStrToWord(VersionSt: string): word;
var
  S: string;
  PosInd: integer;
  i: integer;
begin
  Result := 0;
  S := Trim(VersionSt);
  for i := 1 to 4 do begin
    if S = '' then
      Result := Result * 10
    else begin
      PosInd := Pos('.', S);
      if PosInd > 0 then begin
        Result := Result * 10 + StrToIntDef(Copy(S, 1, PosInd - 1), 0);
        S := Copy(S, PosInd + 1, Length(S) - PosInd);
      end
      else begin
        Result := Result * 10 + StrToIntDef(S, 0);
        S := '';
      end;
    end;
  end;
end;

function GetMaxCharLength(Charset: Word): Byte;
begin
  if Charset < 800 then
    Result := 1
  else
    Result := 2;

  // exceptions
  case Charset of
    830, // JA16EUC
    831: // JA16EUCYEN
      Result := 3;
    854, // ZHS32GB18030
    860, // ZHT32EUC
    861, // ZHT32SOPS
    863, // ZHT32TRIS
    1860, // ZHT32EUCFIXED
    1863: // ZHT32TRISFIXED
      Result := 4;
    OCI_UTF16ID,      // CHARSET_UTF16
    OCI_AL16UTF16ID:  // CHARSET_AL16UTF16
      Result := 2;
    OCI_AL24UTFFSSID, // CHARSET_AL24UTFFSS
    OCI_AL24UTF8ID:   // CHARSET_AL24UTF8
      Result := 3;
    OCI_UTFEID,       // CHARSET_UTFE
    OCI_AL32UTF8ID:   // CHARSET_AL32UTF8
      Result := 4;
  end;
end;

function GetFixedCharLength(Charset: word): Byte;
begin
  if Charset < 1000 then
    Result := 1
  else if (Charset = 1860) or (Charset = 1863) then // ZHT32EUCFIXED, ZHT32TRISFIXED
    Result := 4
  else
    Result := 2;
end;

function GetUTF8Charset(OracleVersion: Word): Integer;
begin
  if OracleVersion >= 9000 then
    Result := OCI_AL32UTF8ID
  else if OracleVersion >= 8000 then
    Result := OCI_AL24UTF8ID
  else if OracleVersion >= 7000 then
    Result := OCI_AL24UTFFSSID
  else
    Result := 0;
end;

function GetCodePageCharset(CodePage: Integer; OracleVersion: Word): Word;
begin
  case CodePage of
    1256:    Result := 560; //AR8MSWIN
    1250:    Result := 170; //EE8MSWIN
    1251:    Result := 171; //CL8MSWIN
    1252:    Result := 178; //WE8MSWIN
    1253:    Result := 174; //EL8MSWIN
    1254:    Result := 177; //TR8MSWIN
    1255:    Result := 175; //IW8MSWIN
    1257:    Result := 179; //BLT8MSWIN
    1258:    Result := 45;  //VN8MSWIN
    923:     Result := 172; //ET8MSWIN
    921:     Result := 176; //LT8MSWIN
    949:     Result := 846; //KO16MSWIN949
    932:     Result := 832; //JA16SJIS
    936:     Result := 852; //ZHS16GBK
    950:     Result := 867; //ZHT16MSWIN950
    20127:   Result := 1;   // "US-ASCII"
    437:     Result := 4;   // "CP437"
    37:      Result := 5;   // "CP037"
    500:     Result := 6;   // "CP500"
    1140:    Result := 7;   // "CP01140"
    20285:   Result := 8;   // "CP285"
    1146:    Result := 9;   // "CP01146"
    850:     Result := 10;  // "CP850"
    20106:   Result := 11;  // "DIN_66003"
    20107:   Result := 15;  // "SEN_850200_B"
    1148:    Result := $1b; // "CP01148"
    858:     Result := $1c; // "CP00858"
    28591:   Result := $1f; // "ISO-8859-1"
    28592:   Result := $20; // "ISO-8859-2"
    28593:   Result := $21; // "ISO-8859-3"
    28594:   Result := $22; // "ISO-8859-4"
    28595:   Result := $23; // "ISO-8859-5"
    28596:   Result := $24; // "ISO-8859-6"
    28597:   Result := $25; // "ISO-8859-7"
    38598:   Result := $26; // "ISO-8859-8-I"
    28599:   Result := $27; // "ISO-8859-9"
    874:     Result := $29; // "TH8TISASCII"
    28605:   Result := $2e; // "ISO-8859-15"
    28603:   Result := $2f; // "ISO-8859-13"
    21866:   Result := $33; // "KOI8-U"
    20420:   Result := 70;  // "CP420"
    20424:   Result := $5c; // "CP424"
    1026:    Result := $5d; // "CP1026"
    20871:   Result := $5e; // "CP871"
    20284:   Result := $5f; // "CP284"
    1145:    Result := $62; // "CP01145"
    20924:   Result := $65; // "CP00924"
    852:     Result := 150; // "CP852"
    866:     Result := $98; // "CP866"
    862:     Result := $9a; // "CP862"
    855:     Result := $9b; // "CP855"
    857:     Result := $9c; // "CP857"
    860:     Result := 160; // "CP860"
    861:     Result := $a1; // "CP861"
    20273:   Result := 180; // "CP273"
    20280:   Result := $b5; // "CP280"
    20277:   Result := $b6; // "IBM277"
    20278:   Result := $b7; // "CP278"
    870:     Result := $b8; // "CP870"
    20297:   Result := $ba; // "CP297"
    1141:    Result := $bd; // "CP01141"
    865:     Result := 190; // "CP865"
    20866:   Result := $c4; // "KOI8-R"
    1142:    Result := $c6; // "CP01142"
    1143:    Result := $c7; // "CP01143"
    1144:    Result := 200; // "CP01144"
    20108:   Result := $cc; // "NS_4551-1"
    1147:    Result := $d0; // "CP01147"
    20423:   Result := $147;// "CP423"
    51932:   Result := $33d;// "EUC-JP"
    51949:   Result := 840; // "EUC-KR"
    863:     Result := 390; // "CP863"
    10000:   Result := $15f;// "MACINTOSH"
    869:     Result := $181;// "CP869"
    54936:   Result := $356;// "GB18030"
    // Oracle 7.3 and 8.0.5 doesn't support AL32UTF8
    65001:   Result := GetUTF8Charset(OracleVersion); // "UTF-8"
    1200:    Result := $3e8;// "UTF-16"
    1201:    Result := $7d0;// "UTF-16BE"
    else     Result := 0;
  end;
end;

{$IFDEF CLR}

{PTRD}

function PTRD.GetUB1Property(Index: Integer): ub1;
begin
  Result := Marshal.ReadByte(Ptr, Index);
end;

procedure PTRD.SetUB1Property(Index: Integer; Value: ub1);
begin
  Marshal.WriteByte(Ptr, Index, Value);
end;

function PTRD.GetUB2Property(Index: Integer): ub2;
begin
  Result := ub2(Marshal.ReadInt16(Ptr, Index));
end;

procedure PTRD.SetUB2Property(Index: Integer; Value: ub2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PTRD.GetUB4Property(Index: Integer): ub4;
begin
  Result := ub4(Marshal.ReadInt32(Ptr, Index));
end;

procedure PTRD.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

class operator PTRD.Implicit(AValue: IntPtr): PTRD;
begin
  Result.Ptr := AValue;
end;

class operator PTRD.Implicit(AValue: PTRD): IntPtr;
begin
  Result := AValue.Ptr;
end;

{PRowId7}

function PRowId7.GetUB4Property(Index: Integer): ub4;
begin
  Result := Marshal.ReadInt32(Ptr, Index);
end;

procedure PRowId7.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PRowId7.GetTRDProperty(Index: Integer): PTRD;
begin
  Result.Ptr := IntPtr(Integer(Ptr) + Index);
end;

class operator PRowId7.Implicit(AValue: IntPtr): PRowId7;
begin
  Result.Ptr := AValue;
end;

class operator PRowId7.Implicit(AValue: PRowId7): IntPtr;
begin
  Result := AValue.Ptr;
end;

{PCDA}

function PCDA.GetSB2Property(Index: Integer): sb2;
begin
  Result := Marshal.ReadInt16(Ptr, Index);
end;

procedure PCDA.SetSB2Property(Index: Integer; Value: sb2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PCDA.GetSB4Property(Index: Integer): sb4;
begin
  Result := Marshal.ReadInt32(Ptr, Index);
end;

procedure PCDA.SetSB4Property(Index: Integer; Value: sb4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

function PCDA.GetUB1Property(Index: Integer): ub1;
begin
  Result := Marshal.ReadByte(Ptr, Index);
end;

procedure PCDA.SetUB1Property(Index: Integer; Value: ub1);
begin
  Marshal.WriteByte(Ptr, Index, Value);
end;

function PCDA.GetUB2Property(Index: Integer): ub2;
begin
  Result := ub2(Marshal.ReadInt16(Ptr, Index));
end;

procedure PCDA.SetUB2Property(Index: Integer; Value: ub2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PCDA.GetUB4Property(Index: Integer): ub4;
begin
  Result := ub4(Marshal.ReadInt32(Ptr, Index));
end;

procedure PCDA.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

function PCDA.GetIntPtrProperty(Index: Integer): IntPtr;
begin
  Result := Marshal.ReadIntPtr(Ptr, Index);
end;

procedure PCDA.SetIntPtrProperty(Index: Integer; Value: IntPtr);
begin
  Marshal.WriteIntPtr(Ptr, Index, Value);
end;

function PCDA.GetTRowId7Property(Index: Integer): PRowId7;
begin
  Result.Ptr := IntPtr(Integer(Ptr) + Index);
end;

procedure PCDA.SetArrayProperty(Index: Integer; ArrayInd: integer; Value: ub1);
begin
  Marshal.WriteByte(Ptr, Index + ArrayInd, Value);
end;

function PCDA.GetArrayProperty(Index: Integer; ArrayInd: integer): ub1;
begin
  Result := Marshal.ReadByte(Ptr, Index + ArrayInd);
end;

class operator PCDA.Implicit(AValue: IntPtr): PCDA;
begin
  Result.Ptr := AValue;
end;

class operator PCDA.Implicit(AValue: PCDA): IntPtr;
begin
  Result := AValue.Ptr;
end;

{PRowId8}

function PRowId8.GetUB2Property(Index: Integer): ub2;
begin
  Result := ub2(Marshal.ReadInt16(Ptr, Index));
end;

procedure PRowId8.SetUB2Property(Index: Integer; Value: ub2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PRowId8.GetUB4Property(Index: Integer): ub4;
begin
  Result := ub4(Marshal.ReadInt32(Ptr, Index));
end;

procedure PRowId8.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

class operator PRowId8.Implicit(AValue: IntPtr): PRowId8;
begin
  Result.Ptr := AValue;
end;

class operator PRowId8.Implicit(AValue: PRowId8): IntPtr;
begin
  Result := AValue.Ptr;
end;

{PRowid81}

function PRowid81.GetUB1Property(Index: Integer): ub1;
begin
  Result := Marshal.ReadByte(Ptr, Index);
end;

procedure PRowid81.SetUB1Property(Index: Integer; Value: ub1);
begin
  Marshal.WriteByte(Ptr, Index, Value);
end;

function PRowid81.GetUB2Property(Index: Integer): ub2;
begin
  Result := ub2(Marshal.ReadInt16(Ptr, Index));
end;

procedure PRowid81.SetUB2Property(Index: Integer; Value: ub2);
begin
  Marshal.WriteInt16(Ptr, Index, Value);
end;

function PRowid81.GetUB4Property(Index: Integer): ub4;
begin
  Result := ub4(Marshal.ReadInt32(Ptr, Index));
end;

procedure PRowid81.SetUB4Property(Index: Integer; Value: ub4);
begin
  Marshal.WriteInt32(Ptr, Index, Value);
end;

class operator PRowid81.Implicit(AValue: IntPtr): PRowid81;
begin
  Result.Ptr := AValue;
end;

class operator PRowid81.Implicit(AValue: PRowid81): IntPtr;
begin
  Result := AValue.Ptr;
end;

{POCIRowid}

function POCIRowid.GetPRowId8Property(Index: Integer): PRowId8;
begin
  Result := IntPtr(Integer(Ptr) + Index);
end;

class operator POCIRowid.Implicit(AValue: IntPtr): POCIRowid;
begin
  Result.Ptr := AValue;
end;

class operator POCIRowid.Implicit(AValue: POCIRowid): IntPtr;
begin
  Result := AValue.Ptr;
end;

{POCIRowid81}

function POCIRowid81.GetPRowId81Property(Index: Integer): PRowId81;
begin
  Result := Marshal.ReadIntPtr(Ptr, Index);
end;

procedure POCIRowid81.SetPRowId81Property(Index: Integer; Value: PRowId81);
begin
  Marshal.WriteIntPtr(Ptr, Index, Value);
end;

class operator POCIRowid81.Implicit(AValue: IntPtr): pOCIRowid81;
begin
  Result.Ptr := AValue;
end;

class operator POCIRowid81.Implicit(AValue: pOCIRowid81): IntPtr;
begin
  Result := AValue.Ptr;
end;

{$ENDIF}

function NotLink: sword;
begin
{$IFDEF FPC}
  Result := 0; // FPC anti-warning
{$ENDIF}
  raise Exception.Create('OCI function is not linked');
end;

{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
function GetEnvironmentPathVariable: string;
begin
  Result := GetEnvironmentVariable('PATH');
end;
{$ENDIF}
{$ENDIF}

{ TOCIAPI }

constructor TOCIAPI.Create(AHome: TOracleHome);
begin
  inherited Create;

  hInitLock := TCriticalSection.Create;

  FHome := AHome;

  Release;
end;

destructor TOCIAPI.Destroy;
begin
  Release;

  hInitLock.Free;

  inherited;
end;

{$IFDEF MSWINDOWS}
function TOCIAPI.GetProc(const Name: string): FARPROC;
begin
  if Home.OCILib <> 0 then
    Result := GetProcAddress(Home.OCILib, PChar(Name))
  else
    Result := nil;

  if Result = nil then
    Result := @NotLink;
end;

function TOCIAPI.GetOraClientProc(const Name: string): FARPROC;
begin
  if Home.OCIClientLib <> 0 then
    Result := GetProcAddress(Home.OCIClientLib, PChar(Name))
  else if Home.OCILib <> 0 then
    Result := GetProcAddress(Home.OCILib, PChar(Name))
  else
    Result := nil;

  if Result = nil then
    Result := @NotLink;
end;

class function TOCIAPI.CheckProc(OCILib: HMODULE; const Name: string): boolean;
begin
  if OCILib <> 0 then
    Result := GetProcAddress(OCILib, PChar(Name)) <> nil
  else
    Result := False;
end;
{$ENDIF}

{$IFNDEF MSWINDOWS}
function TOCIAPI.GetProc(const Name: string): IntPtr;
begin
  if NativeUInt(Home.OCILib) <> 0 then
    Result := dlsym(Home.OCILib, PAnsiChar(AnsiString(Name)))
  else
    Result := nil;

  if Result = nil then
    Result := @NotLink;
end;

function TOCIAPI.GetOraClientProc(const Name: string): IntPtr;
begin
  Result := @NotLink;
end;

{$IFDEF POSIX}
class function TOCIAPI.CheckProc(OCILib: NativeUInt; const Name: string): boolean;
{$ELSE}
class function TOCIAPI.CheckProc(OCILib: IntPtr; const Name: string): boolean;
{$ENDIF}
begin
  if NativeUInt(OCILib) <> 0 then
    Result := dlsym(OCILib, PAnsiChar(AnsiString(Name))) <> nil
  else
    Result := False;
end;
{$ENDIF}

{ TOCI7API }

procedure TOCI7API.Init;
begin
  hInitLock.Enter;
  try
    if Inited then
      Exit;

    if OCI73 in Home.PossibleOCICallStyles then begin
    {$IFDEF CLR}
      obindps  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obindps;
      obndra   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obndra ;
      obndrn   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obndrn ;
      obndrv   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obndrv ;
      obreak   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.obreak ;
      ocan     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ocan   ;
      oclose   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oclose ;
      ocof     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ocof   ;
      ocom     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ocom   ;
      ocon     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ocon   ;
      odefin   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.odefin ;
      odefinps := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.odefinps ;
      odescr   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.odescr ;
      odessp   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.odessp ;
      oerhms   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oerhms ;
      oermsg   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oermsg ;
      oexec    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oexec  ;
      oexfet   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oexfet ;
      oexn     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oexn   ;
      ofen     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ofen   ;
      ofetch   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ofetch ;
      oflng    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oflng  ;
      ognfd    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ognfd  ;
      olog     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.olog   ;
      ologof   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ologof ;
      onbclr   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.onbclr ;
      onbset   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.onbset ;
      onbtst   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.onbtst ;
      oopen    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oopen  ;
      oopt     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oopt   ;
      oparse   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.oparse ;
      opinit   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.opinit ;
      orol     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.orol   ;
      ogetpi   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.ogetpi ;
      osetpi   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.osetpi ;
      sqllda   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.sqllda ;
      sqlld2   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.sqlld2 ;
    {$ELSE}
      obindps  := GetProc('obindps');
      obndra   := GetProc('obndra');
      obndrn   := GetProc('obndrn');
      obndrv   := GetProc('obndrv');
      obreak   := GetProc('obreak');
      ocan     := GetProc('ocan');
      oclose   := GetProc('oclose');
      ocof     := GetProc('ocof');
      ocom     := GetProc('ocom');
      ocon     := GetProc('ocon');
      odefin   := GetProc('odefin');
      odefinps := GetProc('odefinps');
      odescr   := GetProc('odescr');
      odessp   := GetProc('odessp');
      oerhms   := GetProc('oerhms');
      oermsg   := GetProc('oermsg');
      oexec    := GetProc('oexec');
      oexfet   := GetProc('oexfet');
      oexn     := GetProc('oexn');
      ofen     := GetProc('ofen');
      ofetch   := GetProc('ofetch');
      oflng    := GetProc('oflng');
      ognfd    := GetProc('ognfd');
      olog     := GetProc('olog');
      ologof   := GetProc('ologof');
      onbclr   := GetProc('onbclr');
      onbset   := GetProc('onbset');
      onbtst   := GetProc('onbtst');
      oopen    := GetProc('oopen');
      oopt     := GetProc('oopt');
      oparse   := GetProc('oparse');
      opinit   := GetProc('opinit');
      orol     := GetProc('orol');
      ogetpi   := GetProc('ogetpi');
      osetpi   := GetProc('osetpi');
      sqllda   := GetProc('sqllda');
      sqlld2   := GetProc('sqlld2');
    {$ENDIF}
    end;
  finally
    hInitLock.Leave;
  end;
end;

procedure TOCI7API.Release;
begin
  hInitLock.Enter;
  try
    FInited := False;

  {$IFNDEF CLR}
    obindps  := @NotLink;
    obndra   := @NotLink;
    obndrn   := @NotLink;
    obndrv   := @NotLink;
    obreak   := @NotLink;
    ocan     := @NotLink;
    oclose   := @NotLink;
    ocof     := @NotLink;
    ocom     := @NotLink;
    ocon     := @NotLink;
    odefin   := @NotLink;
    odefinps := @NotLink;
    odescr   := @NotLink;
    odessp   := @NotLink;
    oerhms   := @NotLink;
    oermsg   := @NotLink;
    oexec    := @NotLink;
    oexfet   := @NotLink;
    oexn     := @NotLink;
    ofen     := @NotLink;
    ofetch   := @NotLink;
    oflng    := @NotLink;
    ognfd    := @NotLink;
    olog     := @NotLink;
    ologof   := @NotLink;
    onbclr   := @NotLink;
    onbset   := @NotLink;
    onbtst   := @NotLink;
    oopen    := @NotLink;
    oopt     := @NotLink;
    opinit   := @NotLink;
    oparse   := @NotLink;
    orol     := @NotLink;
    ogetpi   := @NotLink;
    osetpi   := @NotLink;
    sqllda   := nil;
    sqlld2   := nil;
  {$ENDIF}
  finally
    hInitLock.Leave;
  end;
end;

function TOCI7API.GetThreadSafety: boolean;
begin
  Result := OCI7ThreadSafety
end;

procedure TOCI7API.SetThreadSafety(Value: boolean);
begin
  if OCI73 in Home.PossibleOCICallStyles then begin
    OCI7ThreadSafety := Value;
  {  if ThreadSafety then
      opinit(OCI_EV_TSF)
    else}
      opinit(OCI_EV_DEF); // for nonblock break
  end;
end;

{ TOCI8API }

procedure TOCI8API.Init;
begin
  hInitLock.Enter;
  try
    if Inited then
      Exit;

    if OCI80 in Home.PossibleOCICallStyles then begin
    {$IFDEF CLR}
      OCIAttrGet1            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrGet1;
      OCIAttrGet2            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrGet2;
      OCIAttrSet1            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrSet1;
      OCIAttrSet2            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrSet2;
      //OCIAttrSet3            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAttrSet3;
      OCIBindArrayOfStruct   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindArrayOfStruct;
      OCIBindByName          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindByName;
      OCIBindByPos           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindByPos;
      OCIBindDynamic         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindDynamic;
      OCIBindObject          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBindObject;
      OCIBreak               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIBreak;
      OCIDefineArrayOfStruct := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDefineArrayOfStruct;
      OCIDefineByPos         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDefineByPos;
      OCIDefineDynamic       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDefineDynamic;
      OCIDefineObject        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDefineObject;
      OCIDescribeAny         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDescribeAny;
      OCIDescriptorAlloc     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDescriptorAlloc;
      OCIDescriptorFree      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDescriptorFree;
      OCIEnvInit             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIEnvInit;
      OCIErrorGet            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIErrorGet;
      OCIHandleAlloc         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIHandleAlloc;
      OCIHandleFree          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIHandleFree;
      OCIInitialize          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIInitialize;
      OCILdaToSvcCtx         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILdaToSvcCtx;
      OCIParamGet            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIParamGet;
      OCIPasswordChange      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPasswordChange;
      OCIReset               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIReset;
      OCIServerAttach        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIServerAttach;
      OCIServerDetach        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIServerDetach;
      OCIServerVersion       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIServerVersion;
      OCISessionBegin        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionBegin;
      OCISessionEnd          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionEnd;
      OCISessionGet          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionGet;
      OCISessionRelease      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionRelease;
      OCISessionPoolCreate   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionPoolCreate;
      OCISessionPoolDestroy  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISessionPoolDestroy;
      OCITransStart          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransStart;
      OCITransRollback       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransRollback;
      OCITransCommit         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransCommit;
      OCITransDetach         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransDetach;
      OCITransPrepare        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransPrepare;
      OCITransForget         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransForget;
      OCIStmtExecute         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtExecute;
      OCIStmtFetch           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtFetch;
      OCIStmtFetch2          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtFetch2;
      OCIStmtGetPieceInfo    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtGetPieceInfo;
      OCIStmtPrepare         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtPrepare;
      OCIStmtPrepare2        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtPrepare2;
      OCIStmtPrepare3        := @NotLink;
      OCIStmtRelease         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtRelease;
      OCIStmtGetNextResult   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtGetNextResult;
      OCIStmtSetPieceInfo    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStmtSetPieceInfo;
      OCISvcCtxToLda         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISvcCtxToLda;
      OCITransCommit         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransCommit;
      OCITransRollback       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITransRollback;

      OCILobAppend           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobAppend;
      OCILobAssign           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobAssign;
      OCILobCharSetForm      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobCharSetForm;
      OCILobCharSetId        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobCharSetId;
      OCILobCopy             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobCopy;
      OCILobOpen             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobOpen;
      OCILobClose            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobClose;
      OCILobIsOpen           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobIsOpen;
      OCILobCreateTemporary  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobCreateTemporary;
      OCILobFreeTemporary    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFreeTemporary;
      OCILobIsTemporary      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobIsTemporary;
      OCILobDisableBuffering := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobDisableBuffering;
      OCILobEnableBuffering  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobEnableBuffering;
      OCILobErase            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobErase;
      OCILobFileClose        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileClose;
      OCILobFileExists       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileExists;
      OCILobFileGetName      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileGetName;
      OCILobFileIsOpen       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileIsOpen;
      OCILobFileOpen         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileOpen;
      OCILobFileSetName      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFileSetName;
      OCILobFlushBuffer      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobFlushBuffer;
      OCILobGetLength        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobGetLength;
      OCILobIsEqual          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobIsEqual;
      OCILobLoadFromFile     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobLoadFromFile;
      OCILobLocatorIsInit    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobLocatorIsInit;
      OCILobRead             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobRead;
      OCILobTrim             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobTrim;
      OCILobWrite            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobWrite;

      OCISubscriptionRegister   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISubscriptionRegister;
      OCISubscriptionUnRegister := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISubscriptionUnRegister;
      OCISubscriptionEnable     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISubscriptionEnable;
      OCISubscriptionDisable    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCISubscriptionDisable;

      if ObjectVersion then begin
        OCICacheFlush          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheFlush;
        OCICacheFree           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheFree;
        OCICacheRefresh        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheRefresh;
        OCICacheUnmark         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheUnmark;
        OCICacheUnpin          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICacheUnpin;

        OCIObjectCopy          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectCopy;
        OCIObjectExists        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectExists;
        OCIObjectFlush         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectFlush;
        OCIObjectFree          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectFree;
        OCIObjectPtr           := @NotLink;
        OCIObjectGetAttr       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetAttr;
        OCIObjectGetInd        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetInd;
        OCIObjectGetObjectRef  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetObjectRef;
        OCIObjectGetProperty   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetProperty;
        OCIObjectGetTypeRef    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectGetTypeRef;
        OCIObjectIsDirty       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectIsDirty;
        OCIObjectIsLocked      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectIsLocked;
        OCIObjectLock          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectLock;
        OCIObjectMarkDelete    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectMarkDelete;
        OCIObjectMarkDeleteByRef :={$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectMarkDeleteByRef;
        OCIObjectMarkUpdate    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectMarkUpdate;
        OCIObjectNew           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectNew;
        OCIObjectPin           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectPin;
        OCIObjectPin2          := @NotLink;
        OCIObjectPinCountReset := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectPinCountReset;
        OCIObjectPinTable      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectPinTable;
        OCIObjectRefresh       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectRefresh;
        OCIObjectSetAttr       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectSetAttr;
        OCIObjectUnmark        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectUnmark;
        OCIObjectUnmarkByRef   := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectUnmarkByRef;
        OCIObjectUnpin         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIObjectUnpin;
        OCITypeByName          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITypeByName;
        OCITypeByRef           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITypeByRef;

        OCICollAppend          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollAppend;
        OCICollAssign          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollAssign;
        OCICollAssignElem      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollAssignElem;
        OCICollGetElem         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollGetElem;
        OCICollMax             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollMax;
        OCICollSize            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollSize;
        OCICollTrim            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCICollTrim;
        OCIDateAssign          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateAssign;
        OCIDateFromText        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateFromText;
        OCIDateGetDate         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateGetDate;
        OCIDateGetTime         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateGetTime;
        OCIDateSetDate         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateSetDate;
        OCIDateSetTime         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateSetTime;
        OCIDateToText          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateToText;
        OCINumberAssign        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberAssign;
        OCINumberCmp           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberCmp;
        OCINumberFromInt       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberFromInt;
        OCINumberFromReal      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberFromReal;
        OCINumberFromText      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberFromText;
        OCINumberToInt         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberToInt;
        OCINumberToReal        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberToReal;
        OCINumberToText        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCINumberToText;
        OCIRefAssign           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefAssign;
        OCIRefClear            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefClear;
        OCIRefIsEqual          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefIsEqual;
        OCIRefIsNull           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefIsNull;
        OCIRefToHex            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRefToHex;
        OCIStringAllocSize     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringAllocSize;
        OCIStringAssign        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringAssign;
        OCIStringAssignText    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringAssignText;
        OCIStringPtr           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringPtr;
        OCIStringResize        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringResize;
        OCIStringSize          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIStringSize;
        OCITableDelete         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableDelete;
        OCITableExists         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableExists;
        OCITableFirst          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableFirst;
        OCITableLast           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableLast;
        OCITableNext           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableNext;
        OCITablePrev           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITablePrev;
        OCITableSize           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCITableSize;
      end; // ObjectVersion
    {$ELSE}
      OCIAttrGet1            := GetProc('OCIAttrGet');
      OCIAttrGet2            := _OCIAttrGet2(OCIAttrGet1);
      OCIAttrSet1            := GetProc('OCIAttrSet');
      OCIAttrSet2            := _OCIAttrSet2(OCIAttrSet1);
      //OCIAttrSet3            := GetProc('OCIAttrSet');
      OCIBindArrayOfStruct   := GetProc('OCIBindArrayOfStruct');
      OCIBindByName          := GetProc('OCIBindByName');
      OCIBindByPos           := GetProc('OCIBindByPos');
      OCIBindDynamic         := GetProc('OCIBindDynamic');
      OCIBindObject          := GetProc('OCIBindObject');
      OCIBreak               := GetProc('OCIBreak');
      OCIDefineArrayOfStruct := GetProc('OCIDefineArrayOfStruct');
      OCIDefineByPos         := GetProc('OCIDefineByPos');
      OCIDefineDynamic       := GetProc('OCIDefineDynamic');
      OCIDefineObject        := GetProc('OCIDefineObject');
      OCIDescribeAny         := GetProc('OCIDescribeAny');
      OCIDescriptorAlloc     := GetProc('OCIDescriptorAlloc');
      OCIDescriptorFree      := GetProc('OCIDescriptorFree');
      OCIEnvInit             := GetProc('OCIEnvInit');
      OCIErrorGet            := GetProc('OCIErrorGet');
      OCIHandleAlloc         := GetProc('OCIHandleAlloc');
      OCIHandleFree          := GetProc('OCIHandleFree');
      OCIInitialize          := GetProc('OCIInitialize');
      OCILdaToSvcCtx         := GetProc('OCILdaToSvcCtx');
      OCIParamGet            := GetProc('OCIParamGet');
      OCIPasswordChange      := GetProc('OCIPasswordChange');
      OCIReset               := GetProc('OCIReset');
      OCIServerAttach        := GetProc('OCIServerAttach');
      OCIServerDetach        := GetProc('OCIServerDetach');
      OCIServerVersion       := GetProc('OCIServerVersion');
      OCISessionBegin        := GetProc('OCISessionBegin');
      OCISessionEnd          := GetProc('OCISessionEnd');
      OCISessionGet          := GetProc('OCISessionGet');
      OCISessionRelease      := GetProc('OCISessionRelease');
      OCISessionPoolCreate   := GetProc('OCISessionPoolCreate');
      OCISessionPoolDestroy  := GetProc('OCISessionPoolDestroy');
      OCITransStart          := GetProc('OCITransStart');
      OCITransRollback       := GetProc('OCITransRollback');
      OCITransCommit         := GetProc('OCITransCommit');
      OCITransDetach         := GetProc('OCITransDetach');
      OCITransPrepare        := GetProc('OCITransPrepare');
      OCITransForget         := GetProc('OCITransForget');
      OCIStmtExecute         := GetProc('OCIStmtExecute');
      OCIStmtFetch           := GetProc('OCIStmtFetch');
      OCIStmtFetch2          := GetProc('OCIStmtFetch2');
      OCIStmtGetPieceInfo    := GetProc('OCIStmtGetPieceInfo');
      OCIStmtPrepare         := GetProc('OCIStmtPrepare');
      OCIStmtSetPieceInfo    := GetProc('OCIStmtSetPieceInfo');
      OCISvcCtxToLda         := GetProc('OCISvcCtxToLda');

      OCILobAppend           := GetProc('OCILobAppend');
      OCILobAssign           := GetProc('OCILobAssign');
      OCILobCharSetForm      := GetProc('OCILobCharSetForm');
      OCILobCharSetId        := GetProc('OCILobCharSetId');
      OCILobCopy             := GetProc('OCILobCopy');
      OCILobOpen             := GetProc('OCILobOpen');
      OCILobClose            := GetProc('OCILobClose');
      OCILobIsOpen           := GetProc('OCILobIsOpen');
      OCILobCreateTemporary  := GetProc('OCILobCreateTemporary');
      OCILobFreeTemporary    := GetProc('OCILobFreeTemporary');
      OCILobIsTemporary      := GetProc('OCILobIsTemporary');
      OCILobDisableBuffering := GetProc('OCILobDisableBuffering');
      OCILobEnableBuffering  := GetProc('OCILobEnableBuffering');
      OCILobErase            := GetProc('OCILobErase');
      OCILobFileClose        := GetProc('OCILobFileClose');
      OCILobFileExists       := GetProc('OCILobFileExists');
      OCILobFileGetName      := GetProc('OCILobFileGetName');
      OCILobFileIsOpen       := GetProc('OCILobFileIsOpen');
      OCILobFileOpen         := GetProc('OCILobFileOpen');
      OCILobFileSetName      := GetProc('OCILobFileSetName');
      OCILobFlushBuffer      := GetProc('OCILobFlushBuffer');
      OCILobGetLength        := GetProc('OCILobGetLength');
      OCILobIsEqual          := GetProc('OCILobIsEqual');
      OCILobLoadFromFile     := GetProc('OCILobLoadFromFile');
      OCILobLocatorIsInit    := GetProc('OCILobLocatorIsInit');
      OCILobRead             := GetProc('OCILobRead');
      OCILobTrim             := GetProc('OCILobTrim');
      OCILobWrite            := GetProc('OCILobWrite');

      OCIExtProcGetEnv       := GetProc('ociepgoe');
      OCIExtProcAllocCallMemory := GetProc('ociepacm');
      OCIExtProcRaiseExcpWithMsg := GetProc('ociepmsg');

      OCISubscriptionRegister   := GetProc('OCISubscriptionRegister');
      OCISubscriptionUnRegister := GetProc('OCISubscriptionUnRegister');
      OCISubscriptionEnable     := GetProc('OCISubscriptionEnable');
      OCISubscriptionDisable    := GetProc('OCISubscriptionDisable');

      if ObjectVersion then begin
        OCICacheFlush          := GetProc('OCICacheFlush');
        OCICacheFree           := GetProc('OCICacheFree');
        OCICacheRefresh        := GetProc('OCICacheRefresh');
        OCICacheUnmark         := GetProc('OCICacheUnmark');
        OCICacheUnpin          := GetProc('OCICacheUnpin');

        OCIObjectCopy          := GetProc('OCIObjectCopy');
        OCIObjectExists        := GetProc('OCIObjectExists');
        OCIObjectFlush         := GetProc('OCIObjectFlush');
        OCIObjectFree          := GetProc('OCIObjectFree');
        OCIObjectPtr           := @NotLink;
        OCIObjectGetAttr       := GetProc('OCIObjectGetAttr');
        OCIObjectGetInd        := GetProc('OCIObjectGetInd');
        OCIObjectGetObjectRef  := GetProc('OCIObjectGetObjectRef');
        OCIObjectGetProperty   := GetProc('OCIObjectGetProperty');
        OCIObjectGetTypeRef    := GetProc('OCIObjectGetTypeRef');
        OCIObjectIsDirty       := GetProc('OCIObjectIsDirty');
        OCIObjectIsLocked      := GetProc('OCIObjectIsLocked');
        OCIObjectLock          := GetProc('OCIObjectLock');
        OCIObjectMarkDelete    := GetProc('OCIObjectMarkDelete');
        OCIObjectMarkDeleteByRef := GetProc('OCIObjectMarkDeleteByRef');
        OCIObjectMarkUpdate    := GetProc('OCIObjectMarkUpdate');
        OCIObjectNew           := GetProc('OCIObjectNew');
        OCIObjectPin           := GetProc('OCIObjectPin');
        OCIObjectPin2          := @NotLink;
        OCIObjectPinCountReset := GetProc('OCIObjectPinCountReset');
        OCIObjectPinTable      := GetProc('OCIObjectPinTable');
        OCIObjectRefresh       := GetProc('OCIObjectRefresh');
        OCIObjectSetAttr       := GetProc('OCIObjectSetAttr');
        OCIObjectUnmark        := GetProc('OCIObjectUnmark');
        OCIObjectUnmarkByRef   := GetProc('OCIObjectUnmarkByRef');
        OCIObjectUnpin         := GetProc('OCIObjectUnpin');
        OCITypeByName          := GetProc('OCITypeByName');
        OCITypeByRef           := GetProc('OCITypeByRef');

        OCICollAppend          := GetProc('OCICollAppend');
        OCICollAssign          := GetProc('OCICollAssign');
        OCICollAssignElem      := GetProc('OCICollAssignElem');
        OCICollGetElem         := GetProc('OCICollGetElem');
        OCICollMax             := GetProc('OCICollMax');
        OCICollSize            := GetProc('OCICollSize');
        OCICollTrim            := GetProc('OCICollTrim');
        OCIDateAssign          := GetProc('OCIDateAssign');
        OCIDateFromText        := GetProc('OCIDateFromText');
        OCIDateGetDate         := GetProc('OCIDateGetDate');
        OCIDateGetTime         := GetProc('OCIDateGetTime');
        OCIDateSetDate         := GetProc('OCIDateSetDate');
        OCIDateSetTime         := GetProc('OCIDateSetTime');
        OCIDateToText          := GetProc('OCIDateToText');
        OCINumberAssign        := GetProc('OCINumberAssign');
        OCINumberCmp           := GetProc('OCINumberCmp');
        OCINumberFromInt       := GetProc('OCINumberFromInt');
        OCINumberFromReal      := GetProc('OCINumberFromReal');
        OCINumberFromText      := GetProc('OCINumberFromText');
        OCINumberToInt         := GetProc('OCINumberToInt');
        OCINumberToReal        := GetProc('OCINumberToReal');
        OCINumberToText        := GetProc('OCINumberToText');
        OCIRefAssign           := GetProc('OCIRefAssign');
        OCIRefClear            := GetProc('OCIRefClear');
        OCIRefIsEqual          := GetProc('OCIRefIsEqual');
        OCIRefIsNull           := GetProc('OCIRefIsNull');
        OCIRefToHex            := GetProc('OCIRefToHex');
        OCIStringAllocSize     := GetProc('OCIStringAllocSize');
        OCIStringAssign        := GetProc('OCIStringAssign');
        OCIStringAssignText    := GetProc('OCIStringAssignText');
        OCIStringPtr           := GetProc('OCIStringPtr');
        OCIStringResize        := GetProc('OCIStringResize');
        OCIStringSize          := GetProc('OCIStringSize');
        OCITableDelete         := GetProc('OCITableDelete');
        OCITableExists         := GetProc('OCITableExists');
        OCITableFirst          := GetProc('OCITableFirst');
        OCITableLast           := GetProc('OCITableLast');
        OCITableNext           := GetProc('OCITableNext');
        OCITablePrev           := GetProc('OCITablePrev');
        OCITableSize           := GetProc('OCITableSize');
      end; // ObjectVersion
    {$ENDIF}
    end;

    if Home.OCIVersion >= 8100 then begin
    {$IFDEF CLR}
      OCIEnvCreate                := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIEnvCreate;

      OCIDirPathAbort             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathAbort;
      OCIDirPathColArrayEntryGet  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayEntryGet;
      OCIDirPathColArrayEntrySet  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayEntrySet;
      OCIDirPathColArrayRowGet    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayRowGet;
      OCIDirPathColArrayReset     := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayReset;
      OCIDirPathColArrayToStream  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathColArrayToStream;
      OCIDirPathFinish            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathFinish;
      OCIDirPathLoadStream        := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathLoadStream;
      OCIDirPathPrepare           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathPrepare;
      OCIDirPathStreamReset       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDirPathStreamReset;
    {$ELSE}
      OCIEnvCreate                := GetProc('OCIEnvCreate');

      OCIDirPathAbort             := GetProc('OCIDirPathAbort');
      OCIDirPathColArrayEntryGet  := GetProc('OCIDirPathColArrayEntryGet');
      OCIDirPathColArrayEntrySet  := GetProc('OCIDirPathColArrayEntrySet');
      OCIDirPathColArrayRowGet    := GetProc('OCIDirPathColArrayRowGet');
      OCIDirPathColArrayReset     := GetProc('OCIDirPathColArrayReset');
      OCIDirPathColArrayToStream  := GetProc('OCIDirPathColArrayToStream');
      OCIDirPathFinish            := GetProc('OCIDirPathFinish');
      OCIDirPathLoadStream        := GetProc('OCIDirPathLoadStream');
      OCIDirPathPrepare           := GetProc('OCIDirPathPrepare');
      OCIDirPathStreamReset       := GetProc('OCIDirPathStreamReset');
    {$ENDIF}
    end;

    if Home.OCIVersion >= 9000 then begin
    {$IFDEF CLR}
      OCIEnvNlsCreate               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIEnvNlsCreate;

      OCIDateTimeConstruct          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeConstruct;
      OCIDateTimeCheck              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeCheck;
      OCIDateTimeFromText           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeFromText;
      OCIDateTimeToText             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeToText;
      OCIDateTimeGetDate            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeGetDate;
      OCIDateTimeGetTime            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeGetTime;
      OCIDateTimeGetTimeZoneOffset  := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeGetTimeZoneOffset;
      OCIDateTimeGetTimeZoneName    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeGetTimeZoneName;
      OCIDateTimeAssign             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeAssign;
      OCIDateTimeCompare            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDateTimeCompare;
      OCIIntervalFromText           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalFromText;
      OCIIntervalToText             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalToText;
      OCIIntervalCheck              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalCheck;
      OCIIntervalAssign             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalAssign;
      OCIIntervalCompare            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalCompare;
      OCIIntervalSetYearMonth       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalSetYearMonth;
      OCIIntervalGetYearMonth       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalGetYearMonth;
      OCIIntervalSetDaySecond       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalSetDaySecond;
      OCIIntervalGetDaySecond       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalGetDaySecond;
      OCIIntervalFromNumber         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIIntervalFromNumber;

      OCIPing                       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPing;

      OCIXMLTypeNew                 := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeNew;
      OCIXMLTypeCreateFromSrc       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeCreateFromSrc;
      OCIXMLTypeExtract             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeExtract;
      OCIXMLTypeTransform           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeTransform;
      OCIXMLTypeExists              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeExists;
      OCIXMLTypeIsSchemaBased       := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeIsSchemaBased;
      OCIXMLTypeGetSchema           := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeGetSchema;
      OCIXMLTypeValidate            := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeValidate;

      OCIXMLTypeGetDOM              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeGetDOM;
      OCIXMLTypeGetFromDOM          := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIXMLTypeGetFromDOM;
      OCIDOMFree                    := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIDOMFree;

      OCIAnyDataAccess              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAnyDataAccess;
      OCIAnyDataConvert             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAnyDataConvert;
      OCIAnyDataGetType             := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAnyDataGetType;
      OCIAnyDataIsNull              := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAnyDataIsNull;
      OCIAnyDataTypeCodeToSqlt      := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIAnyDataTypeCodeToSqlt;

      OCIRowidToChar                := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIRowidToChar;

      if Home.OCIVersion > 10000 then begin
        OCIPStreamFromXMLType         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamFromXMLType10;
        OCIPStreamRead                := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamRead10;
        OCIPStreamClose               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamClose10;
      end
      else begin
        OCIPStreamFromXMLType         := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamFromXMLType;
        OCIPStreamRead                := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamRead;
        OCIPStreamClose               := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIPStreamClose;
      end;
    {$ELSE}
      OCIEnvNlsCreate               := GetProc('OCIEnvNlsCreate');

      OCIDateTimeConstruct          := GetProc('OCIDateTimeConstruct');
      OCIDateTimeCheck              := GetProc('OCIDateTimeCheck');
      OCIDateTimeFromText           := GetProc('OCIDateTimeFromText');
      OCIDateTimeToText             := GetProc('OCIDateTimeToText');
      OCIDateTimeGetDate            := GetProc('OCIDateTimeGetDate');
      OCIDateTimeGetTime            := GetProc('OCIDateTimeGetTime');
      OCIDateTimeGetTimeZoneOffset  := GetProc('OCIDateTimeGetTimeZoneOffset');
      OCIDateTimeGetTimeZoneName    := GetProc('OCIDateTimeGetTimeZoneName');
      OCIDateTimeAssign             := GetProc('OCIDateTimeAssign');
      OCIDateTimeCompare            := GetProc('OCIDateTimeCompare');
      OCIIntervalFromText           := GetProc('OCIIntervalFromText');
      OCIIntervalToText             := GetProc('OCIIntervalToText');
      OCIIntervalCheck              := GetProc('OCIIntervalCheck');
      OCIIntervalAssign             := GetProc('OCIIntervalAssign');
      OCIIntervalCompare            := GetProc('OCIIntervalCompare');
      OCIIntervalSetYearMonth       := GetProc('OCIIntervalSetYearMonth');
      OCIIntervalGetYearMonth       := GetProc('OCIIntervalGetYearMonth');
      OCIIntervalSetDaySecond       := GetProc('OCIIntervalSetDaySecond');
      OCIIntervalGetDaySecond       := GetProc('OCIIntervalGetDaySecond');
      OCIIntervalFromNumber         := GetProc('OCIIntervalFromNumber');

      OCIPing                       := GetProc('OCIPing');

      OCIXMLTypeNew                 := GetProc('OCIXMLTypeNew');
      OCIXMLTypeCreateFromSrc       := GetProc('OCIXMLTypeCreateFromSrc');
      OCIXMLTypeExtract             := GetProc('OCIXMLTypeExtract');
      OCIXMLTypeTransform           := GetProc('OCIXMLTypeTransform');
      OCIXMLTypeExists              := GetProc('OCIXMLTypeExists');
      OCIXMLTypeIsSchemaBased       := GetProc('OCIXMLTypeIsSchemaBased');
      OCIXMLTypeGetSchema           := GetProc('OCIXMLTypeGetSchema');
      OCIXMLTypeValidate            := GetProc('OCIXMLTypeValidate');
      OCIXMLTypeGetDOM              := GetProc('OCIXMLTypeGetDOM');
      OCIXMLTypeGetFromDOM          := GetProc('OCIXMLTypeGetFromDOM');
      OCIDOMFree                    := GetProc('OCIDOMFree');

      OCIAnyDataAccess              := GetProc('OCIAnyDataAccess');
      OCIAnyDataConvert             := GetProc('OCIAnyDataConvert');
      OCIAnyDataGetType             := GetProc('OCIAnyDataGetType');

      OCIRowidToChar                := GetProc('OCIRowidToChar');

      OCIPStreamFromXMLType         := GetOraClientProc('OCIPStreamFromXMLType');
      OCIPStreamRead                := GetOraClientProc('OCIPStreamRead');
      OCIPStreamClose               := GetOraClientProc('OCIPStreamClose');

      OCIStmtPrepare2               := GetProc('OCIStmtPrepare2');
      OCIStmtRelease                := GetProc('OCIStmtRelease');
      OCIStmtGetNextResult          := GetProc('OCIStmtGetNextResult');
    {$ENDIF}

      OCIStmtPrepare3               := @NotLink;
    end;

    if Home.OCIVersion >= 10000 then begin
    {$IFDEF CLR}
      OCILobRead2 := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCILobRead2;
    {$ELSE}
      OCILobRead2 := GetProc('OCILobRead2');
    {$ENDIF}
    end;

    FInited := True;
  finally
    hInitLock.Leave;
  end;
end;

procedure TOCI8API.Release;
begin
  hInitLock.Enter;
  try
    FInited := False;

  {$IFNDEF CLR}
    OCIAttrGet1            := @NotLink;
    OCIAttrGet2            := @NotLink;
    OCIAttrSet1            := @NotLink;
    OCIAttrSet2            := @NotLink;
    //OCIAttrSet3            := @NotLink;
    OCIBindArrayOfStruct   := @NotLink;
    OCIBindByName          := @NotLink;
    OCIBindByPos           := @NotLink;
    OCIBindDynamic         := @NotLink;
    OCIBindObject          := @NotLink;
    OCIBreak               := @NotLink;
    OCIDefineArrayOfStruct := @NotLink;
    OCIDefineByPos         := @NotLink;
    OCIDefineDynamic       := @NotLink;
    OCIDefineObject        := @NotLink;
    OCIDescribeAny         := @NotLink;
    OCIDescriptorAlloc     := @NotLink;
    OCIDescriptorFree      := @NotLink;
    OCIEnvCreate           := @NotLink;
    OCIEnvNlsCreate        := @NotLink;
    OCIEnvInit             := @NotLink;
    OCIErrorGet            := @NotLink;
    OCIHandleAlloc         := @NotLink;
    OCIHandleFree          := @NotLink;
    OCIInitialize          := @NotLink;
    OCILdaToSvcCtx         := @NotLink;
    OCIParamGet            := @NotLink;
    OCIPasswordChange      := @NotLink;
    OCIReset               := @NotLink;
    OCIServerAttach        := @NotLink;
    OCIServerDetach        := @NotLink;
    OCIServerVersion       := @NotLink;
    OCISessionBegin        := @NotLink;
    OCISessionEnd          := @NotLink;
    OCISessionGet          := @NotLink;
    OCISessionRelease      := @NotLink;
    OCISessionPoolCreate   := @NotLink;
    OCISessionPoolDestroy  := @NotLink;
    OCITransStart          := @NotLink;
    OCITransRollback       := @NotLink;
    OCITransCommit         := @NotLink;
    OCITransDetach         := @NotLink;
    OCITransPrepare        := @NotLink;
    OCITransForget         := @NotLink;
    OCIStmtExecute         := @NotLink;
    OCIStmtFetch           := @NotLink;
    OCIStmtFetch2          := @NotLink;
    OCIStmtGetPieceInfo    := @NotLink;
    OCIStmtPrepare         := @NotLink;
    OCIStmtPrepare2        := @NotLink;
    OCIStmtPrepare3        := @NotLink;
    OCIStmtRelease         := @NotLink;
    OCIStmtGetNextResult   := @NotLink;
    OCIStmtSetPieceInfo    := @NotLink;
    OCISvcCtxToLda         := @NotLink;

    OCILobAppend           := @NotLink;
    OCILobAssign           := @NotLink;
    OCILobCharSetForm      := @NotLink;
    OCILobCharSetId        := @NotLink;
    OCILobCopy             := @NotLink;
    OCILobOpen             := @NotLink;
    OCILobClose            := @NotLink;
    OCILobIsOpen           := @NotLink;
    OCILobCreateTemporary  := @NotLink;
    OCILobFreeTemporary    := @NotLink;
    OCILobIsTemporary      := @NotLink;
    OCILobDisableBuffering := @NotLink;
    OCILobEnableBuffering  := @NotLink;
    OCILobErase            := @NotLink;
    OCILobFileClose        := @NotLink;
    OCILobFileExists       := @NotLink;
    OCILobFileGetName      := @NotLink;
    OCILobFileIsOpen       := @NotLink;
    OCILobFileOpen         := @NotLink;
    OCILobFileSetName      := @NotLink;
    OCILobFlushBuffer      := @NotLink;
    OCILobGetLength        := @NotLink;
    OCILobIsEqual          := @NotLink;
    OCILobLoadFromFile     := @NotLink;
    OCILobLocatorIsInit    := @NotLink;
    OCILobRead             := @NotLink;
    OCILobRead2            := @NotLink;
    OCILobTrim             := @NotLink;
    OCILobWrite            := @NotLink;

    OCICacheFlush          := @NotLink;
    OCICacheFree           := @NotLink;
    OCICacheRefresh        := @NotLink;
    OCICacheUnmark         := @NotLink;
    OCICacheUnpin          := @NotLink;

    OCIObjectCopy          := @NotLink;
    OCIObjectExists        := @NotLink;
    OCIObjectFlush         := @NotLink;
    OCIObjectFree          := @NotLink;
    OCIObjectPtr           := @NotLink;
    OCIObjectGetAttr       := @NotLink;
    OCIObjectGetInd        := @NotLink;
    OCIObjectGetObjectRef  := @NotLink;
    OCIObjectGetProperty   := @NotLink;
    OCIObjectGetTypeRef    := @NotLink;
    OCIObjectIsDirty       := @NotLink;
    OCIObjectIsLocked      := @NotLink;
    OCIObjectLock          := @NotLink;
    OCIObjectMarkDelete    := @NotLink;
    OCIObjectMarkDeleteByRef := @NotLink;
    OCIObjectMarkUpdate    := @NotLink;
    OCIObjectNew           := @NotLink;
    OCIObjectPin           := @NotLink;
    OCIObjectPin2          := @NotLink;
    OCIObjectPinCountReset := @NotLink;
    OCIObjectPinTable      := @NotLink;
    OCIObjectRefresh       := @NotLink;
    OCIObjectSetAttr       := @NotLink;
    OCIObjectUnmark        := @NotLink;
    OCIObjectUnmarkByRef   := @NotLink;
    OCIObjectUnpin         := @NotLink;
    OCITypeByName          := @NotLink;
    OCITypeByRef           := @NotLink;

    OCICollAppend          := @NotLink;
    OCICollAssign          := @NotLink;
    OCICollAssignElem      := @NotLink;
    OCICollGetElem         := @NotLink;
    OCICollMax             := @NotLink;
    OCICollSize            := @NotLink;
    OCICollTrim            := @NotLink;
    OCIDateAssign          := @NotLink;
    OCIDateFromText        := @NotLink;
    OCIDateGetDate         := @NotLink;
    OCIDateGetTime         := @NotLink;
    OCIDateSetDate         := @NotLink;
    OCIDateSetTime         := @NotLink;
    OCIDateToText          := @NotLink;
    OCINumberAssign        := @NotLink;
    OCINumberCmp           := @NotLink;
    OCINumberFromInt       := @NotLink;
    OCINumberFromReal      := @NotLink;
    OCINumberFromText      := @NotLink;
    OCINumberToInt         := @NotLink;
    OCINumberToReal        := @NotLink;
    OCINumberToText        := @NotLink;
    OCIRefAssign           := @NotLink;
    OCIRefClear            := @NotLink;
    OCIRefIsEqual          := @NotLink;
    OCIRefIsNull           := @NotLink;
    OCIRefToHex            := @NotLink;
    OCIStringAllocSize     := @NotLink;
    OCIStringAssign        := @NotLink;
    OCIStringAssignText    := @NotLink;
    OCIStringPtr           := @NotLink;
    OCIStringResize        := @NotLink;
    OCIStringSize          := @NotLink;
    OCITableDelete         := @NotLink;
    OCITableExists         := @NotLink;
    OCITableFirst          := @NotLink;
    OCITableLast           := @NotLink;
    OCITableNext           := @NotLink;
    OCITablePrev           := @NotLink;
    OCITableSize           := @NotLink;

    OCIDirPathAbort            := @NotLink;
    OCIDirPathColArrayEntryGet := @NotLink;
    OCIDirPathColArrayEntrySet := @NotLink;
    OCIDirPathColArrayRowGet   := @NotLink;
    OCIDirPathColArrayReset    := @NotLink;
    OCIDirPathColArrayToStream := @NotLink;
    OCIDirPathFinish           := @NotLink;
    OCIDirPathLoadStream       := @NotLink;
    OCIDirPathPrepare          := @NotLink;
    OCIDirPathStreamReset      := @NotLink;

    OCIExtProcGetEnv           := @NotLink;
    OCIExtProcAllocCallMemory  := @NotLink;
    OCIExtProcRaiseExcpWithMsg := @NotLink;

    OCISubscriptionRegister    := @NotLink;
    OCISubscriptionUnRegister  := @NotLink;
    OCISubscriptionEnable      := @NotLink;
    OCISubscriptionDisable     := @NotLink;

  // Oracle 9
    OCIDateTimeConstruct          := @NotLink;
    OCIDateTimeFromText           := @NotLink;
    OCIDateTimeToText             := @NotLink;
    OCIDateTimeGetDate            := @NotLink;
    OCIDateTimeGetTime            := @NotLink;
    OCIDateTimeGetTimeZoneOffset  := @NotLink;
    OCIDateTimeGetTimeZoneName    := @NotLink;
    OCIDateTimeAssign             := @NotLink;
    OCIDateTimeCompare            := @NotLink;
    OCIIntervalFromText           := @NotLink;
    OCIIntervalToText             := @NotLink;
    OCIIntervalCheck              := @NotLink;
    OCIIntervalAssign             := @NotLink;
    OCIIntervalCompare            := @NotLink;
    OCIIntervalSetYearMonth       := @NotLink;
    OCIIntervalGetYearMonth       := @NotLink;
    OCIIntervalSetDaySecond       := @NotLink;
    OCIIntervalGetDaySecond       := @NotLink;
    OCIIntervalFromNumber         := @NotLink;

    OCIRowidToChar                := @NotLink;
    OCIPing                       := @NotLink;

    OCIXMLTypeNew                 := @NotLink;
    OCIXMLTypeCreateFromSrc       := @NotLink;
    OCIXMLTypeExtract             := @NotLink;
    OCIXMLTypeTransform           := @NotLink;
    OCIXMLTypeExists              := @NotLink;
    OCIXMLTypeIsSchemaBased       := @NotLink;
    OCIXMLTypeGetSchema           := @NotLink;
    OCIXMLTypeValidate            := @NotLink;
    OCIXMLTypeGetDOM              := @NotLink;
    OCIXMLTypeGetFromDOM          := @NotLink;
    OCIDOMFree                    := @NotLink;

    OCIAnyDataAccess              := @NotLink;
    OCIAnyDataConvert             := @NotLink;
    OCIAnyDataGetType             := @NotLink;
  {$ENDIF}
  finally
    hInitLock.Leave;
  end;
end;

procedure TOCI8API.DoOraError(ErrorCode: sword; OCISvcCtx: TOCISvcCtx);
begin
  DoOraError(ErrorCode, OCISvcCtx.UnicodeEnv, OCISvcCtx.hOCIError);
end;

procedure TOCI8API.DoOraError(ErrorCode: sword; UnicodeEnv: boolean; hOCIError: pOCIError);
begin
  TOraError.DoOraError(
    @Self.OCIErrorGet,
    ErrorCode,
    UnicodeEnv,
    hOCIError
  );
end;

function TOCI8API.GetOraError(ErrorCode: sword; OCISvcCtx: TOCISvcCtx; var ErrorMsg: string): sword;
begin
  Result := TOraError.GetOraError(
    @Self.OCIErrorGet,
    ErrorCode,
    OCISvcCtx.UnicodeEnv,
    OCISvcCtx.hOCIError,
    ErrorMsg
  );
end;

function TOCI8API.GetOraError(ErrorCode: sword; OCISvcCtx: TOCISvcCtx): sword;
var
  Msg: string;
begin
  Result := TOraError.GetOraError(
    @Self.OCIErrorGet,
    ErrorCode,
    OCISvcCtx.UnicodeEnv,
    OCISvcCtx.hOCIError,
    Msg
  );
end;

procedure TOCI8API.Check(Status: sword; OCISvcCtx: TOCISvcCtx);
begin
  if Status <> OCI_SUCCESS then
    DoOraError(Status, OCISvcCtx);
end;

procedure TOCI8API.Check(Status: sword; UnicodeEnv: boolean; hOCIError: pOCIError);
begin
  if Status <> OCI_SUCCESS then
    DoOraError(Status, UnicodeEnv, hOCIError);
end;

function TOCI8API.GetHeapAlloc(OCISvcCtx: TOCISvcCtx): integer;
var
  ValuePtr: Integer;
begin
  Check(OCIAttrGet2(OCISvcCtx.hOCIEnv, OCI_HTYPE_ENV, ValuePtr, nil, OCI_ATTR_HEAPALLOC, OCISvcCtx.hOCIError), OCISvcCtx);
  Result := Integer(ValuePtr);
end;

function TOCI8API.GetSharedHeapAlloc(OCISvcCtx: TOCISvcCtx): integer;
var
  ValuePtr: Integer;
begin
  Check(OCIAttrGet2(OCISvcCtx.hOCIEnv, OCI_HTYPE_ENV, ValuePtr, nil, OCI_ATTR_SHARED_HEAPALLOC, OCISvcCtx.hOCIError), OCISvcCtx);
  Result := Integer(ValuePtr);
end;

function OCINumberFromBCD__(const Value: TBytes; RealLength: uword): TBytes;
var
  i, exponent: Integer;
  IsFirstDigit, IsFirstDigitBCD, IsPositive: Boolean;
  NumberInd, BCDInd: Integer;
  b: byte;
  bytesCount: integer;
  OldPrecision, Precision, Places: integer;
begin
  if (Value[0] = 0) or (Value[0] > 64) or ((Value[1] and $40) > 0) then begin  // Zero
    SetLength(Result, 1);
    Result[0] := Byte(-128);
    Exit;
  end;
  Precision := Value[0];
  IsPositive := (Value[1] and $80) = 0;
  Places := Value[1] and $3f;

  // last nulls
  i := ((Precision + 1) shr 1) + 1;
  repeat
   if Value[i] <> 0 then begin
     if (Precision and 1) = 0 then begin
       if (Value[(Precision shr 1) + 1] and $0f) = 0 then begin //last digit is null
         Dec(Precision);
         Dec(Places);
       end;
     end
     else begin
       if (Value[(Precision shr 1) + 1] and $f0) = 0 then begin //first digit is null
         Dec(Precision);
         Dec(Places);
       end;
     end;
     break;
   end;
   Dec(Precision, 2);
   Dec(Places, 2);
   Dec(i);
  until i <= 1;

  BCDInd := 2;
  IsFirstDigitBCD := true;
  // first nulls
  OldPrecision := ((Precision + 1) shr 1) + 1;
  for i := 2 to OldPrecision do begin
   if Value[i] <> 0 then begin
     if (Value[i] and $f0) = 0 then begin
       IsFirstDigitBCD := false;
       Dec(Precision);
     end;
     break;
   end;
   Inc(BCDInd);
   Dec(Precision, 2);
  end;

  if Precision = 0 then begin
    SetLength(Result, 1);
    Result[0] := Byte(-128);
    Exit;
  end;

  IsFirstDigit := ((Precision - Places) and 1) = 0;

  if Precision - Places >= 0 then
    exponent := ((Precision - Places + 1) shr 1) - 1
  else
    exponent := ((Precision - Places) shr 1) - 1;

  bytesCount := (Precision + 1 + ((Precision - Places) and 1)) shr 1;

  if IsPositive then begin  // positive
    SetLength(Result, bytesCount + 1);
    Result[0] := exponent + 128 + 65;
  end
  else begin
    if bytesCount < 20 then begin
      SetLength(Result, bytesCount + 2);
      Result[bytesCount + 1] := 102;
    end
    else
      SetLength(Result, bytesCount + 1);
    Result[0] := not(exponent + 128 + 65);
  end;

  NumberInd := 1;
  Result[1] := 0;
  for i := 1 to Precision do begin
    if IsFirstDigitBCD then
      b := (Value[BCDInd] and $f0) shr 4
    else begin
      b := Value[BCDInd] and $0f;
      Inc(BCDInd);
    end;
    IsFirstDigitBCD := not IsFirstDigitBCD;

    if IsFirstDigit then
      Result[NumberInd] := b * 10
    else begin
      Result[NumberInd] := Result[NumberInd] + b + 1;
      Inc(NumberInd);
    end;
    IsFirstDigit := not IsFirstDigit;
  end;
  {if Result <> 0 then
    raise Exception.Create(Format('''%s'' is not a valid Oracle Number value', [str]));}
  if not IsFirstDigit then
    Result[NumberInd] := Result[NumberInd] + 1;

  if not IsPositive then begin  // positive
    for i := 1 to bytesCount do
      Result[i] := 102 - Result[i];
  end;
end;

function DefaultOCIDateFormat: string;
var
  P, StrLen: integer;
  Str: string;
  Buf: integer;
begin
  Result := '';
  P := 1;
  Str := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
  StrLen := Length(Str);
  while P <= StrLen do begin
    case Str[P] of
      'd','D': begin
        Inc(P);
        if (P <= StrLen) and ((Str[P] = 'd') or (Str[P] = 'D')) then begin
          Inc(P);
          if (P <= StrLen) and ((Str[P] = 'd') or (Str[P] = 'D')) then begin
            Inc(P);
            if (P <= StrLen) and ((Str[P] = 'd') or (Str[P] = 'D')) then begin
              Inc(P);
              Result := Result + 'fmDAYfm';
            end else
              Result := Result + 'DY';
          end else
            Result := Result + 'DD';
        end else
          Result := Result + 'fmDDfm';
      end;
      'm','M': begin
        Inc(P);
        if (P <= StrLen) and ((Str[P] = 'm') or (Str[P] = 'M')) then begin
          Inc(P);
          if (P <= StrLen) and ((Str[P] = 'm') or (Str[P] = 'M')) then begin
            Inc(P);
            if (P <= StrLen) and ((Str[P] = 'm') or (Str[P] = 'M')) then begin
              Inc(P);
              Result := Result + 'MONTH';
            end else
              Result := Result + 'MON';
          end else
            Result := Result + 'MM';
        end else
          Result := Result + 'fmMMfm';
      end;
      'g','G': begin
        Inc(P);
        Result := Result + 'E';
      end;
      '''','"': begin
        Buf := P;
        Inc(P);
        P := Pos(Str[Buf], Copy(Str, P, StrLen - P + 1));
        if P <> 0 then begin
          Inc(P, Buf + 1);
          Str[Buf] := '"';
          Str[P - 1] := '"';
          Result := Result + Copy(Str, Buf, P - Buf)
        end else begin
          P := StrLen + 1;
          Result := Result + Copy(Str, Buf, P - Buf) + '"';
        end;
      end;
      '/': begin // ShortDateFormat always contains "/" instead of DateSeparator
        Inc(P);
        Result := Result + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator;
      end;
    else
      Result := Result + UpperCase(Str[P]);
      Inc(P);
    end;
  end;
end;

function DefaultOCITimeFormat(Precision: Byte = 6): string;
var
  P, J, StrLen: integer;
  Str: string;
  Buf: integer;
  Temp: string;
begin
  Result := '';
  P := 1;
  Str := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
  StrLen := Length(Str);
  while P <= StrLen do begin
    case Str[P] of
      't','T': begin
        Inc(P);
        while (P <= StrLen) and ((Str[P] = 't') or (Str[P] = 'T')) do
          Inc(P);
        Result := Result + 'AM';
      end;
      'h','H': begin
        Inc(P);
        J := StrLen;
        Temp := '24';
        while J > P do begin
          case Str[J] of
            't': begin
              Temp := '12';
              break;
            end;
            'a', 'A': begin
              if (UpperCase(Copy(Str, J, 3)) = 'A/P') or
                 (UpperCase(copy(Str, J, 4)) = 'AMPM') or
                 (UpperCase(copy(Str, J, 5)) = 'AM/PM')
              then begin
                Temp := '12';
                break;
              end;
            end;
          end;
          Dec(J);
        end;
        if (P <= StrLen) and ((Str[P] = 'h') or (Str[P] = 'H')) then begin
          Inc(P);
          Result := Result + 'HH' + Temp;
        end else
          Result := Result + 'fmHH'+ Temp +'fm';
      end;
      'n','N','m','M': begin
        Inc(P);
        if (P <= StrLen) and ((Str[P] = 'n') or (Str[P] = 'N') or (Str[P] = 'm') or (Str[P] = 'M')) then begin
          Inc(P);
          Result := Result + 'MI';
        end else
          Result := Result + 'fmMIfm';
      end;
      's','S': begin
        Inc(P);
        if (Precision > 0) and (AnsiPos('Z', UpperCase(Str)) = 0) then
          Temp := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator + 'FF' // + IntToStr(Precision); does not work on Oracle 9.0.1
        else
          Temp := '';
        if (P <= StrLen) and ((Str[P] = 's') or (Str[P] = 'S')) then begin
          Inc(P);
          Result := Result + 'SS' + Temp;
        end else
          Result := Result + 'fmSSfm' + Temp;
      end;
      'z','Z': begin
        Result := Result + 'FF';
        Inc(P);
        while (P <= StrLen) and ((Str[P] = 'z') or (Str[P] = 'Z')) do
          Inc(P);
      end;
      '''','"': begin
        Buf := P;
        Inc(P);
        P := Pos(Str[Buf], Copy(Str, P, StrLen - P + 1));
        if P <> 0 then begin
          Inc(P, Buf + 1);
          Str[Buf] := '"';
          Str[P - 1] := '"';
          Result := Result + Copy(Str, Buf, P - Buf)
        end else begin
          P := StrLen + 1;
          Result := Result + Copy(Str, Buf, P - Buf) + '"';
        end;
      end;
      ':': begin // LongTimeFormat always contains ":" instead of TimeSeparator
        Inc(P);
        Result := Result + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator;
      end;
    else
      if UpperCase(Copy(Str, P, 3)) = 'A/P' then begin
        Result := Result + 'AM';
        Inc(P, 3);
      end
      else if UpperCase(Copy(Str, P, 4)) = 'AMPM' then begin
        Result := Result + 'AM';
        Inc(P, 4);
      end
      else if UpperCase(Copy(Str, P, 5)) = 'AM/PM' then begin
        Result := Result + 'AM';
        Inc(P, 5);
      end
      else begin
        Result := Result + UpperCase(Str[P]);
        Inc(P);
      end;
    end;
  end;
end;

function DefaultOCITimeStampFormat(Precision: Byte = 6): string;
begin
  Result := DefaultOCIDateFormat + ' ' + DefaultOCITimeFormat(Precision);
end;

function DefaultOCITimeStampWithTZFormat(Precision: Byte = 6): string;
begin
  Result := DefaultOCITimeStampFormat + ' TZH:TZM';
end;

function TOCI8API.IsUnicodeEnv(hOCIEnv: pOCIEnv; hOCIError: pOCIError): boolean;
var
  BVal: ub1;
  BPtr: IntPtr;
begin
  if Home.OCIVersion < 9000 then begin
    Result := False;
    exit;
  end;

  BPtr := @BVal;
  TOraError.Check(
    @OCIErrorGet,
    OCIAttrGet1(hOCIEnv, OCI_HTYPE_ENV, BPtr, nil, OCI_ATTR_ENV_UTF16, hOCIError),
    False,
    hOCIError
  );
  Result := BVal <> 0;
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}

{ TMTSAPI }

procedure TMTSAPI.Init;
{$IFNDEF CLR}
var
  MTSName: string;
{$ENDIF}
begin
  hInitLock.Enter;
  try
    if Inited then
      Exit;

  {$IFDEF NET}
    if Home.Direct then
      Exit;
  {$ENDIF}

  {$IFNDEF CLR}
    {oramts}
    MTSName := ExtractFileDir(Home.OCIDLL) + '\oramts.dll';

    hMTSLib := LoadLibraryEx(PChar(MTSName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
    if hMTSLib <> 0 then begin
      OraMTSSvcGet := GetProcAddress(hMTSLib, PChar('OraMTSSvcGet'));
      OraMTSSvcRel := GetProcAddress(hMTSLib, PChar('OraMTSSvcRel'));
      OraMTSEnlCtxGet := GetProcAddress(hMTSLib, PChar('OraMTSEnlCtxGet'));
      OraMTSJoinTxn := GetProcAddress(hMTSLib, PChar('OraMTSJoinTxn'));
      OraMTSEnlCtxRel := GetProcAddress(hMTSLib, PChar('OraMTSEnlCtxRel'));
      OraMTSSvcEnlist := GetProcAddress(hMTSLib, PChar('OraMTSSvcEnlist'));
    end
    else
      raise Exception.Create('Cannot load MTS: ' + MTSName);

  {$ELSE}
      OraMTSSvcGet := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSSvcGet;
      OraMTSSvcRel := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSSvcRel;
      OraMTSEnlCtxGet := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSEnlCtxGet;
      OraMTSJoinTxn := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSJoinTxn;
      OraMTSEnlCtxRel := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSEnlCtxRel;
      OraMTSSvcEnlist := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OraMTSSvcEnlist;
  {$ENDIF}

    InitMSDTC;

    FInited := True;
  finally
    hInitLock.Leave;
  end;
end;

procedure TMTSAPI.Release;
begin
  hInitLock.Enter;
  try
  {$IFNDEF CLR}
    OraMTSSvcGet := @NotLink;
    OraMTSSvcRel := @NotLink;
    OraMTSEnlCtxGet := @NotLink;
    OraMTSJoinTxn := @NotLink;
    OraMTSEnlCtxRel := @NotLink;
    OraMTSSvcEnlist := @NotLink;
  {$ENDIF}

    if Inited then begin
      FInited := False;

    {$IFNDEF CLR}
      if hMTSLib <> 0 then begin
        FreeLibrary(hMTSLib);
        hMTSLib := 0;
      end;

      FreeMSDTC;
    {$ENDIF}
    end;
  finally
    hInitLock.Leave;
  end;
end;

{$ENDIF}
{$ENDIF}

{ TDirectAPI }

procedure TDirectAPI.Init;
begin
{$IFDEF NET}
  OCIAttrGet1            := @OCIAttrGet_;
  OCIAttrGet2            := @OCIAttrGet2_;
  OCIAttrSet1            := @OCIAttrSet_;
//  OCIAttrSet3            := @OCIAttrSet3_;
  OCIAttrSet2            := @OCIAttrSet2_;
  OCIBindArrayOfStruct   := @OCIBindArrayOfStruct_;
  OCIBindByName          := @OCIBindByName_;
  OCIBindDynamic         := @OCIBindDynamic_;
  OCIBindByPos           := @OCIBindByPos_;
  OCIBreak               := @OCIBreak_;
  OCIDefineArrayOfStruct := @OCIDefineArrayOfStruct_;
  OCIDefineByPos         := @OCIDefineByPos_;
  OCIDefineDynamic       := @OCIDefineDynamic_;
  OCIDescriptorAlloc     := @OCIDescriptorAlloc_;
  OCIDescriptorFree      := @OCIDescriptorFree_;
  OCIEnvCreate           := @OCIEnvCreate_;
  OCIErrorGet            := @OCIErrorGet_;
  OCIHandleAlloc         := @OCIHandleAlloc_;
  OCIHandleFree          := @OCIHandleFree_;
  OCIParamGet            := @OCIParamGet_;
  OCIPasswordChange      := @OCIPasswordChange_;
  OCIServerAttach        := @OCIServerAttach_;
  OCIServerDetach        := @OCIServerDetach_;
  OCIServerVersion       := @OCIServerVersion_;
  OCISessionBegin        := @OCISessionBegin_;
  OCISessionEnd          := @OCISessionEnd_;
  OCIStmtExecute         := @OCIStmtExecute_;
  OCIStmtFetch           := @OCIStmtFetch_;
  OCIStmtPrepare         := @OCIStmtPrepare_;
  OCIStmtPrepare2        := @OCIStmtPrepare2_;
  OCIStmtPrepare3        := @OCIStmtPrepare3_;
  OCIStmtGetNextResult   := @OCIStmtGetNextResult_;
  OCITransCommit         := @OCITransCommit_;
  OCITransRollback       := @OCITransRollback_;
  OCILobCharSetForm      := @OCILobCharSetForm_;
  OCILobGetLength        := @OCILobGetLength_;
  OCILobLocatorIsInit    := @OCILobLocatorIsInit_;
  OCILobRead             := @OCILobRead_;
  OCILobTrim             := @OCILobTrim_;
  OCILobWrite            := @OCILobWrite_;
  OCILobCreateTemporary  := @OCILobCreateTemporary_;
  OCILobFreeTemporary    := @OCILobFreeTemporary_;
  OCILobIsTemporary      := @OCILobIsTemporary_;
{$IFNDEF CLR}
  OCITypeByName          := @NotLink;
  OCIBindObject          := @NotLink;
  OCIDefineObject        := @NotLink;
  OCIEnvInit             := @NotLink;
  OCIInitialize          := @NotLink;
  OCILdaToSvcCtx         := @NotLink;
  OCIStmtFetch2          := @NotLink;
  OCIStmtGetPieceInfo    := @NotLink;
  OCIStmtSetPieceInfo    := @NotLink;
  OCISvcCtxToLda         := @NotLink;
  OCILobAppend           := @NotLink;
  OCILobAssign           := @NotLink;
  OCILobCharSetId        := @NotLink;
  OCILobCopy             := @NotLink;
  OCILobDisableBuffering := @NotLink;
  OCILobEnableBuffering  := @NotLink;
  OCILobErase            := @NotLink;
  OCILobFileClose        := OCILobFileClose_;
  OCILobFileExists       := OCILobFileExists_;
  OCILobFileGetName      := OCILobFileGetName_;
  OCILobFileIsOpen       := OCILobFileIsOpen_;
  OCILobFileOpen         := OCILobFileOpen_;
  OCILobFileSetName      := OCILobFileSetName_;
  OCILobFlushBuffer      := @NotLink;
  OCILobIsEqual          := @NotLink;
  OCILobLoadFromFile     := @NotLink;

  OCIXMLTypeExtract       := @NotLink;
  OCIXMLTypeTransform     := @NotLink;
  OCIXMLTypeExists        := @NotLink;
  OCIXMLTypeValidate      := @NotLink;
  OCIXMLTypeIsSchemaBased := @NotLink;
  OCIXMLTypeGetSchema     := @NotLink;
{$ENDIF}

  OCINumberAssign        := @OCINumberAssign_;
  OCINumberCmp           := @OCINumberCmp_;
  OCINumberFromInt       := @OCINumberFromInt_;
  OCINumberFromReal      := @OCINumberFromReal_;
  OCINumberFromText      := @OCINumberFromText_;
  OCINumberToInt         := @OCINumberToInt_;
  OCINumberToReal        := @OCINumberToReal_;
  OCINumberToText        := @OCINumberToText_;

  OCIDateTimeAssign            := @OCIDateTimeAssign_;
  OCIDateTimeCheck             := @OCIDateTimeCheck_;
  OCIDateTimeCompare           := @OCIDateTimeCompare_;
  OCIDateTimeConstruct         := @OCIDateTimeConstruct_;
  OCIDateTimeFromText          := @OCIDateTimeFromText_;
  OCIDateTimeToText            := @OCIDateTimeToText_;
  OCIDateTimeGetDate           := @OCIDateTimeGetDate_;
  OCIDateTimeGetTime           := @OCIDateTimeGetTime_;
  OCIDateTimeGetTimeZoneOffset := @OCIDateTimeGetTimeZoneOffset_;
  OCIDateTimeGetTimeZoneName   := @OCIDateTimeGetTimeZoneName_;

  OCIIntervalAssign            := @OCIIntervalAssign_;
  OCIIntervalCompare           := @OCIIntervalCompare_;
  OCIIntervalCheck             := @OCIIntervalCheck_;
  OCIIntervalFromText          := @OCIIntervalFromText_;
  OCIIntervalToText            := @OCIIntervalToText_;
  OCIIntervalSetYearMonth      := @OCIIntervalSetYearMonth_;
  OCIIntervalGetYearMonth      := @OCIIntervalGetYearMonth_;
  OCIIntervalSetDaySecond      := @OCIIntervalSetDaySecond_;
  OCIIntervalGetDaySecond      := @OCIIntervalGetDaySecond_;
  OCIIntervalFromNumber        := @OCIIntervalFromNumber_;

  OCIRowidToChar               := @OCIRowidToChar_;

  OCIStringAllocSize           := @OCIStringAllocSize_;
  OCIStringAssign              := @OCIStringAssign_;
  OCIStringAssignText          := @OCIStringAssignText_;
  OCIStringPtr                 := @OCIStringPtr_;
  OCIStringResize              := @OCIStringResize_;
  OCIStringSize                := @OCIStringSize_;

  OCIDescribeAny         := @OCIDescribeAny_;
  OCITypeByName          := @OCITypeByName_;

  OCIObjectNew           := @OCIObjectNew_;
  OCIObjectCopy          := @OCIObjectCopy_;
  OCIObjectFree          := @OCIObjectFree_;
  OCIObjectPtr           := @OCIObjectPtr_;
  OCIObjectGetInd        := @OCIObjectGetInd_;
  OCIObjectSetAttr       := @OCIObjectSetAttr_;
  OCIObjectGetAttr       := @OCIObjectGetAttr_;
  OCIDefineObject        := @OCIDefineObject_;
  OCIBindObject          := @OCIBindObject_;

  OCIRefIsNull           := @OCIRefIsNull_;
  OCIObjectPin2          := @OCIObjectPin2_;

  OCICollSize            := @OCICollSize_;
  OCICollTrim            := @OCICollTrim_;
  OCICollAppend          := @OCICollAppend_;
  OCICollAssign          := @OCICollAssign_;
  OCICollAssignElem      := @OCICollAssignElem_;
  OCICollGetElem         := @OCICollGetElem_;

  OCITableSize           := @OCITableSize_;
  OCITableNext           := @OCITableNext_;
  OCITableDelete         := @OCITableDelete_;

  OCIXMLTypeNew           := @OCIXMLTypeNew_;
  OCIXMLTypeCreateFromSrc := @OCIXMLTypeCreateFromSrc_;
  OCIPStreamFromXMLType   := @OCIPStreamFromXMLType_;
  OCIPStreamRead          := @OCIPStreamRead_;
  OCIPStreamClose         := @OCIPStreamClose_;

  OCIAnyDataAccess        := @OCIAnyDataAccess_;
  OCIAnyDataConvert       := @OCIAnyDataConvert_;
  OCIAnyDataGetType       := @OCIAnyDataGetType_;

  FInited := True;
{$ELSE}
  RaiseError(SDirectModeNotSupported);
{$ENDIF}
end;

{ TOracleHome }

constructor TOracleHome.Create(AOwner: TOracleHomes);
begin
  inherited Create;

  FOwner := AOwner;
  FOCI7 := TOCI7API.Create(self);
  FOCI8 := CreateOCI8API;
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  FMTS:= TMTSAPI.Create(self);
{$ENDIF}
{$ENDIF}

  FEnvironments := TCRObjectList.Create;

  Release;
end;

destructor TOracleHome.Destroy;
begin
  Release;

  FEnvironments.Free;

  FreeAndNil(FOCI7);
  FreeAndNil(FOCI8);
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  FreeAndNil(FMTS);
{$ENDIF}
{$ENDIF}

  inherited;
end;

function TOracleHome.GetEnvironmentClass: TEnvironmentClass;
begin
  Result := TOCIEnvironment;
end;

function TOracleHome.GetDirect: boolean;
begin
  Result := FOwner.FDirect = Self;
end;

function TOracleHome.GetEnvironment(Index: Integer): TOCIEnvironment;
begin
  Result := TOCIEnvironment(FEnvironments[Index]);
end;

function TOracleHome.GetEnvironmentCount: Integer;
begin
  Result := FEnvironments.Count;
end;

function TOracleHome.GetOCI7: TOCI7API;
begin
  if OCI73 in PossibleOCICallStyles then begin
    FOCI7.Init;
    Result := FOCI7;
  end
  else
    Result := nil;
end;

function TOracleHome.GetOCI8: TOCI8API;
begin
  if OCI80 in PossibleOCICallStyles then begin
    FOCI8.Init;
    Result := FOCI8;
  end
  else
    Result := nil;
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
function TOracleHome.GetMTS: TMTSAPI;
begin
  if OCI80 in PossibleOCICallStyles then begin
    FOCI8.Init;
    FMTS.Init;
    Result := FMTS;
  end
  else
    Result := nil;
end;
{$ENDIF}
{$ENDIF}

function TOracleHome.GetInited: boolean;
begin
{$IFDEF MSWINDOWS}
  Result := FOCILib <> 0;
{$ENDIF}
{$IFDEF POSIX}
  Result := FOCILib <> 0;
{$ENDIF}
{$IFDEF UNIX}
  Result := FOCILib <> nil;
{$ENDIF}
end;

function TOracleHome.CreateOCI8API: TOCI8API;
begin
  Result := TOCI8API.Create(Self);
end;

{$IFDEF MSWINDOWS}
function TOracleHome.GetFileVersion(FileName: string): string;
var
  VersionData: TBytes;
  VersionSt: IntPtr;
  Len: DWORD;
  Handle: DWORD;
  CharSetC: cardinal;
  CharSet: string;
begin
  Result := '';
  FOCIVersion := 0;

  Len := GetFileVersionInfoSize(PChar(FileName), Handle);
  SetLength(VersionData, Len);
  if GetFileVersionInfo(PChar(FileName), Handle, Len, VersionData) then begin
    if VerQueryValueA(VersionData, '\VarFileInfo\Translation', VersionSt, Len) then begin
    {$IFDEF CLR}
      CharSetC := Marshal.ReadInt32(VersionSt);
    {$ELSE}
      Move(VersionSt^, CharSetC, 4);
    {$ENDIF}
      CharSet := IntToHex(CharSetC, 8);
      CharSet := Copy(CharSet, 5, 4) + Copy(CharSet, 1, 4);
    end
    else
      CharSet := '040904B0';

    if VerQueryValueA(VersionData, PAnsiChar(AnsiString('\StringFileInfo\' + CharSet + '\FileVersion')), VersionSt, Len) then
      Result := string(Marshal.PtrToStringAnsi(VersionSt));
  end;
end;
{$ENDIF}

procedure TOracleHome.DetectOCIClientVersion;
var
  i: integer;
  VersionAr: array[0..4] of sword;
  OCIClientVersion: _OCIClientVersion;
begin
  FOCIVersionSt := '';
  FOCIVersion := 0;

  if OCILib = {$IFNDEF UNIX}0{$ELSE}nil{$ENDIF} then
    raise Exception.Create('OCI is not loaded.');

{$IFDEF CLR}
  OCIClientVersion := {$IFNDEF UNIDACPRO}OraCallCLR{$ELSE}OraCallCLRUni{$ENDIF}.OCIClientVersion;
{$ELSE}
  OCIClientVersion := FOCI8.GetProc('OCIClientVersion');
{$ENDIF}

{$IFDEF CLR}
  if GetProcAddress(OCILib, 'OCIClientVersion') <> nil then begin
{$ELSE}
  if @OCIClientVersion <> @NotLink then begin
{$ENDIF}
    OCIClientVersion(VersionAr[0], VersionAr[1], VersionAr[2], VersionAr[3], VersionAr[4]);
    // OCI function OCIClientVersion was added in Oracle 10.x.x
    if (VersionAr[0] >= 10) and (VersionAr[0] <= 20) then
      for i := 0 to High(VersionAr) do begin
        if i > 0 then
          FOCIVersionSt := FOCIVersionSt + '.';
        FOCIVersionSt := FOCIVersionSt + IntToStr(VersionAr[i]);
      end;
  end;

{$IFDEF MSWINDOWS}
  if FOCIVersionSt = '' then
    FOCIVersionSt := GetFileVersion(OCIDLL);
{$ENDIF}

  if FOCIVersionSt = '' then
    if Inited then begin
      if TOCIAPI.CheckProc(OCILib, 'OCIArrayDescriptorFree') then
        FOCIVersionSt := '11.1.0.0.0'
      else
      if TOCIAPI.CheckProc(OCILib, 'OCIClientVersion') then
        FOCIVersionSt := '10.2.0.0.0'
      else
      if TOCIAPI.CheckProc(OCILib, 'OCILobWrite2') then
        FOCIVersionSt := '10.1.0.0.0'
      else
      if TOCIAPI.CheckProc(OCILib, 'OCISessionPoolCreate') then
        FOCIVersionSt := '9.2.0.0.0'
      else
      if TOCIAPI.CheckProc(OCILib, 'OCIStmtFetch2') then
        FOCIVersionSt := '9.0.0.0.0'
      else
      if TOCIAPI.CheckProc(OCILib, 'OCIEnvCreate') then
        FOCIVersionSt := '8.1.0.0.0'
      else
        FOCIVersionSt := '8.0.0.0.0';
    end
    else
      FOCIVersionSt := '8.0.0.0.0';

  FOCIVersion := VersionStrToWord(FOCIVersionSt);

  if LowerCase(ExtractFileName(OCIDLL)) = 'ociw32.dll' then begin
    Include(FPossibleOCICallStyles, OCI73);
    FOCICallStyle := OCI73;
    Exit;
  end;

  if OCIVersionSt = '' then
    FOwner.OCINotFound(FOCIDLL);

  if OCIVersion div 100 = 73 then begin
    Include(FPossibleOCICallStyles, OCI73);
    FOCICallStyle := OCI73;
  end
  else if OCIVersion >= 8000 then begin
    Include(FPossibleOCICallStyles, OCI73);
    Include(FPossibleOCICallStyles, OCI80);
    if OCICallStyle = None then
      FOCICallStyle := OCI80;
  end
  else if OracleHomes.Default <> nil then
    raise Exception.Create('OCI version ' + OracleHomes.Default.OCIVersionSt + ' is not supported')
  else
    raise Exception.Create('OCI version is unknown');
end;

procedure TOracleHome.AddEnvironment(Environment: TOCIEnvironment);
begin
  FEnvironments.Add(Environment);
end;

procedure TOracleHome.RemoveEnvironment(Environment: TOCIEnvironment);
begin
  FEnvironments.Remove(Environment);
end;

procedure TOracleHome.ClearEnvironments;
var
  i: Integer;
  Environment: TOCIEnvironment;
begin
  // try to release Environments
  for i := FEnvironments.Count - 1 downto 0 do begin
    Environment := Environments[i];
    Environment.Release;
  end;

  for i := FEnvironments.Count - 1 downto 0 do
    FEnvironments.Delete(i);
end;

function TOracleHome.FindEnvironment(Unique: boolean; UnicodeEnv: boolean; SubscriptionPort: Integer): TOCIEnvironment;
var
  i: integer;
begin
  for i := 0  to EnvironmentCount - 1 do begin
    Result := Environments[i];
    if (Result.Unique = Unique) and
       (Result.UnicodeEnv = UnicodeEnv) and
       (Result.SubscriptionPort = SubscriptionPort)
    then
      exit;
  end;

  Result := nil;
end;

function TOracleHome.FindEnvironment(hOCIEnv: pOCIEnv): TOCIEnvironment;
var
  i: integer;
begin
  for i := 0  to EnvironmentCount - 1 do
    if Environments[i].hOCIEnv = hOCIEnv then begin
      Result := Environments[i];
      exit;
    end;

  Result := nil;
end;

function TOracleHome.CreateEnvironment(Unique: boolean; UnicodeEnv: boolean; SubscriptionPort: Integer): TOCIEnvironment;
begin
  Init;

  Result := GetEnvironmentClass.Create(Self, Unique, UnicodeEnv, SubscriptionPort);
  FEnvironments.Add(Result);
end;

function TOracleHome.CreateEnvironment(hOCIEnv: pOCIEnv): TOCIEnvironment;
begin
  Init;

  Result := GetEnvironmentClass.Create(Self, hOCIEnv);
  FEnvironments.Add(Result);
end;

procedure TOracleHome.LoadOCI;
begin
{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
//  if (Length(OracleHomes) > 1) and (OracleHomePath <> '') then begin // {(OCIVersion = OCI81)}
//    St := GetEnvironmentPathVariable;
//    St := ExtractFileDir(OCIDLL) + ';' + St;
//    SetEnvironmentVariable('PATH', PChar(St));
//  end;
{$ENDIF}
  if (FOCIClientLib = 0) and (FOCIClientDLL <> '') then begin
    if Path <> '' then
      FOCIClientLib := LoadLibrary(PChar(FOCIClientDLL))
    else
      FOCIClientLib := LoadLibraryEx(PChar(FOCIClientDLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  end;

  if FOCILib = 0 then begin
    if Path <> '' then
      FOCILib := LoadLibrary(PChar(FOCIDLL))
    else
      FOCILib := LoadLibraryEx(PChar(FOCIDLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  end;
{$ENDIF}
{$IFDEF POSIX}
  if FOCILib = 0 then
    FOCILib := dlopen(PAnsiChar(AnsiString(FOCIDLL)), RTLD_LAZY);
{$ENDIF}
{$IFDEF UNIX}
  if FOCILib = nil then
    FOCILib := dlopen(PAnsiChar(AnsiString(FOCIDLL)), RTLD_LAZY);
{$ENDIF}

{$IFDEF MSWINDOWS}
  if FOCILib = 0 then begin
{$ENDIF}
{$IFDEF POSIX}
  if FOCILib = 0 then begin
{$ENDIF}
{$IFDEF UNIX}
  if FOCILib = nil then begin
{$ENDIF}
    raise Exception.Create('Cannot load OCI: ' + FOCIDLL);
  end;
end;

procedure TOracleHome.FreeOCI;
begin
{$IFDEF MSWINDOWS}
//  if (OracleHomes.Count > 1) and (FPath <> '') then begin
//    // remove added home from PATH variable
//    Env := GetEnvironmentPathVariable;
//    Home := ExtractFileDir(FOCIDLL) + ';';
//    Ind := Pos(Home, Env);
//    if Ind > 0 then begin
//      Env := Copy(Env, 1, Ind - 1) + Copy(Env, Ind + Length(Home),
//        Length(Env) - Length(Home) - Ind + 1);
//      SetEnvironmentVariable('PATH', PChar(Env));
//    end;
//  end;

  if FOCILib <> 0 then begin
    FreeLibrary(FOCILib);  // Returns False on Win95 in DLL
    FOCILib := 0;
  end;

  if FOCIClientLib > 0 then begin
    FreeLibrary(FOCIClientLib);  // Returns False on Win95 in DLL
    FOCIClientLib := 0;
  end;
{$ENDIF}
{$IFDEF POSIX}
  if FOCILib <> 0 then begin
    //dlclose(FOCILib); // BUG: SIGSEGV on close application
    FOCILib := 0;
  end;
  if FOCIClientLib <> 0 then begin
    //dlclose(FOCIClientLib); // BUG: SIGSEGV on close application
    FOCIClientLib := 0;
  end;
{$ENDIF}
{$IFDEF UNIX}
  if FOCILib <> nil then begin
    //dlclose(FOCILib); // BUG: SIGSEGV on close application
    FOCILib := nil;
  end;
  if FOCIClientLib <> nil then begin
    //dlclose(FOCIClientLib); // BUG: SIGSEGV on close application
    FOCIClientLib := nil;
  end;
{$ENDIF}
end;

procedure TOracleHome.Init;
var
  Str: string;
  OldStr: string;
  SearchRec: TSearchRec;

  {hOraClient: HMODULE;
  FileName: string;
  Len: integer;}
begin
  hHomeLock.Enter;
  try
    if Inited then
      Exit;

    // Find DLL
    if (FOCIDLL = '') and (Path <> '') then
      if FindFirst(Path + '\BIN\' + 'oraclient*.dll', faAnyFile, SearchRec) = 0 then begin
        FOCIDLL := Path + '\BIN\' + 'oci.dll';
        FOCIClientDLL := Path + '\BIN\' + SearchRec.Name;
        SysUtils.FindClose(SearchRec);
      end
      else if FindFirst(Path + '\' + 'oraclient*.dll', faAnyFile, SearchRec) = 0 then begin
        FOCIDLL := Path + '\' + 'oci.dll';
        FOCIClientDLL := Path + '\' + SearchRec.Name;
        SysUtils.FindClose(SearchRec);
      end
      else if FindFirst(Path + '\BIN\' + 'oraociei*.dll', faAnyFile, SearchRec) = 0 then begin
        FOCIDLL := Path + '\BIN\' + 'oci.dll';
        FOCIClientDLL := Path + '\BIN\' + SearchRec.Name;
        SysUtils.FindClose(SearchRec);
      end
      else if FindFirst(Path + '\' + 'oraociei*.dll', faAnyFile, SearchRec) = 0 then begin
        FOCIDLL := Path + '\' + 'oci.dll';
        FOCIClientDLL := Path + '\' + SearchRec.Name;
        SysUtils.FindClose(SearchRec);
      end
      else if FindFirst(Path + '\BIN\' + 'oci.dll', faAnyFile, SearchRec) = 0 then begin
        FOCIDLL := Path + '\BIN\' + SearchRec.Name;
        SysUtils.FindClose(SearchRec);
      end
      else if FindFirst(Path + '\' + 'oci.dll', faAnyFile, SearchRec) = 0 then begin
        FOCIDLL := Path + '\' + SearchRec.Name;
        SysUtils.FindClose(SearchRec);
      end
      else if FindFirst(Path + '\BIN\' + 'ora*.dll', faAnyFile, SearchRec) = 0 then begin
        repeat
          OldStr := '';
          Str := Copy(ChangeFileExt(SearchRec.Name, ''), 4, 6);
          if (Str[1] >= '0') and (Str[1] <= '9') then
            if Str > OldStr then begin
              FOCIDLL := Path + '\BIN\' + SearchRec.Name;
              OldStr := Str;
            end;
        until FindNext(SearchRec) <> 0;
        SysUtils.FindClose(SearchRec);
      end;

    if OCIDLL = '' then
      if Name = '' then
      {$IFDEF MSWINDOWS}
        FOCIDLL := 'oci.dll' // try to search in PATH for default home
      {$ELSE}{$IFDEF MACOS}
        FOCIDLL := 'libociei.dylib'
      {$ELSE}
        FOCIDLL := 'libclntsh.so'
      {$ENDIF}{$ENDIF}
      else
        FOwner.OCINotFound(Path);

    LoadOCI;

    DetectOCIClientVersion;
  finally
    hHomeLock.Leave;
  end;
end;

procedure TOracleHome.Release;
begin
  hHomeLock.Enter;
  try
  //{$IFNDEF CLR}
  //  if not System.IsLibrary then  // WAR access violation on free DLL with ODAC
  //{$ENDIF}
      ClearEnvironments;

  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    if FMTS <> nil then
      FMTS.Release;
  {$ENDIF}
  {$ENDIF}
    if FOCI8 <> nil then
      FOCI8.Release;
    if FOCI7 <> nil then
      FOCI7.Release;

    FreeOCI;

    FOCIDLL := '';
    FOCIClientDLL := '';

    FOCIVersionSt := '';
    FOCIVersion := 0;
    FOCICallStyle := None;
    FPossibleOCICallStyles := [];
  finally
    hHomeLock.Leave;
  end;
end;

function TOracleHome.AllocEnvironment(Unique: boolean; UnicodeEnv: boolean; SubscriptionPort: Integer): TOCIEnvironment;
begin
  hEnvLock.Enter;
  try
    if Unique then
      Result := CreateEnvironment(Unique, UnicodeEnv, SubscriptionPort)
    else
    begin
      Result := FindEnvironment(Unique, UnicodeEnv, SubscriptionPort);
      if Result = nil then
        Result := CreateEnvironment(Unique, UnicodeEnv, SubscriptionPort);
    end;
  finally
    hEnvLock.Leave;
  end;
end;

function TOracleHome.AllocEnvironment(hOCIEnv: pOCIEnv): TOCIEnvironment;
begin
  hEnvLock.Enter;
  try
    Result := FindEnvironment(hOCIEnv);
    if Result = nil then
      Result := CreateEnvironment(hOCIEnv);
  finally
    hEnvLock.Leave;
  end;
end;

procedure TOracleHome.CheckOCI;
begin
  if not ((OCICallStyle = OCI73) or (OCICallStyle = OCI80)) then
    RaiseError(SCheckOCI);
end;

procedure TOracleHome.CheckOCI73;
begin
  if not (OCICallStyle = OCI73) then
    RaiseError(SCheckOCI73);
end;

procedure TOracleHome.CheckOCI80;
begin
  if not (OCICallStyle = OCI80) then
    RaiseError(SCheckOCI80);
end;

procedure TOracleHome.CheckOCI81;
begin
  if OCIVersion < 8100 then
    RaiseError(SCheckOCI81);
end;

procedure TOracleHome.CheckOCI90;
begin
  if OCIVersion < 9000 then
    RaiseError(SCheckOCI90);
end;

{ TDirectHome }

constructor TDirectHome.Create(AOwner: TOracleHomes);
begin
  inherited;

  FName := 'Direct';
end;

function TDirectHome.GetEnvironmentClass: TEnvironmentClass;
begin
  Result := TDirectEnvironment;
end;

function TDirectHome.GetInited: boolean;
begin
  Result := (FOCICallStyle <> None) and (FPossibleOCICallStyles <> []);
end;

function TDirectHome.CreateOCI8API: TOCI8API;
begin
  Result := TDirectAPI.Create(Self);
end;

procedure TDirectHome.Init;
begin
{$IFDEF NET}
  FOCIVersion := 9200;
  FOCICallStyle := OCI80;
  FPossibleOCICallStyles := [OCI80];
{$ELSE}
  RaiseError(SDirectModeNotSupported);
{$ENDIF}
end;

{ TOracleHomes }

constructor TOracleHomes.Create;
begin
  inherited;

  hInitLock := TCriticalSection.Create;

  FInited := False;
  FInDestroying := False;
  FHomes := TCRObjectList.Create;
end;

destructor TOracleHomes.Destroy;
begin
  FInDestroying := True;
  Release;

{$IFDEF NET}
  FDirect.Free;
{$ENDIF}
  FHomes.Free;

  hInitLock.Free;

  inherited;
end;

function TOracleHomes.GetOracleHome(Index: integer): TOracleHome;
begin
  Init;

  Result := TOracleHome(FHomes[Index]);
end;

function TOracleHomes.GetCount: Integer;
begin
  Init;

  Result := FHomes.Count;
end;

function TOracleHomes.GetDirect: TOracleHome;
begin
  if FDirect = nil then
  {$IFDEF NET}
    FDirect := TDirectHome.Create(Self);
  {$ELSE}
    RaiseError(SDirectModeNotSupported);
  {$ENDIF}

  Result := FDirect;
end;

function TOracleHomes.GetDefault: TOracleHome;
begin
  Init;

  Result := FDefault;
end;

procedure TOracleHomes.SetDefault(Value: TOracleHome);
var
  i: Integer;
begin
  Init;

  for i := 0 to FHomes.Count - 1 do
    if FHomes[i] = Value then begin
      FDefault := Value;
      Exit;
    end;

  raise Exception.Create('Invalid OracleHome');
end;

function TOracleHomes.Add(const Name, HomePath, OCIDLL, TNSPath, NLSLang: string; IsLite: boolean): TOracleHome;

  function NormalizePath(const Path: string; RemoveBin: boolean): string;
  begin
    if Path <> '' then begin
      if Path[Length(Path)] = '\' then
        Result := Copy(Path, 1, Length(Path) - 1)
      else
        Result := Path;

      if RemoveBin then
        if AnsiUpperCase(Copy(Result, Length(Result) - 3, 4)) = '\BIN' then
          SetLength(Result,  Length(Result) - 4);
    end
    else
      Result := '';
  end;

begin
  Result := TOracleHome.Create(Self);
  try
    Result.FName := Name;
    Result.FPath := NormalizePath(HomePath, True);
    Result.FTNSPath := NormalizePath(TNSPath, False);
    Result.FNLSLang := NLSLang;
    Result.FOCILite := IsLite;
    Result.FOCIDLL := OCIDLL;
    FHomes.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TOracleHomes.Clear;
begin
  FHomes.Clear;
  FInited := False;
end;

procedure TOracleHomes.OCINotFound(const Path: string);
var
  St: string;
begin
  St := Path;
  if St <> '' then
    raise Exception.Create('OCI is not found: ' + St)
  else
    raise Exception.Create('OCI is not found');
end;

procedure TOracleHomes.Init;
{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
type
  OralceClientType = (ctOracle7, ctOracle8, ctInstant);

  function CheckOciDll(const FileName: string): boolean;
  begin
    Result := FileExists(FileName) and
      (DetectLibraryMachineType(FileName) in {$IFDEF WIN64}[mtAMD64, mtIA64]{$ELSE}[mtI386]{$ENDIF});
  end;

  function CheckHomePath(Path: string): boolean;
  begin
    Result := CheckOciDll(Path + '\oci.dll') or
              CheckOciDll(Path + '\bin\' + 'oci.dll') or
              CheckOciDll(Path + '\ociw32.dll') or
              CheckOciDll(Path + '\bin\' + 'ociw32.dll');
  end;

  procedure AddToOracleHomes(const Path, Key: string; ClientType: OralceClientType);
  var
    i: integer;
    RegKey: string;
    TempStr: string;
    hOracleReg: HKEY;
    Home: TOracleHome;
    HomeName: array [0..100] of char;
    HomePath: array [0..255] of char;
    TNSPath: array [0..255] of char;
    NLSLang: array [0..255] of char;
    Lite: array [0..10] of char;
    IsLite: boolean;
    Len: UINT;
    Success: boolean;
  begin
    RegKey := Path;
    if Key <> '' then
      RegKey := RegKey + '\' + Key;

    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(RegKey), 0, KEY_READ, hOracleReg) = ERROR_SUCCESS then
    try
      if ClientType = ctOracle7 then begin
        StrCopy(HomeName, PChar(Key));
        Success := True;
      end
      else if ClientType = ctOracle8 then begin
        Len := Length(HomeName) * SizeOf(Char);
        Success := RegQueryValueEx(hOracleReg, 'ORACLE_HOME_NAME', nil, nil, @HomeName, @Len) = ERROR_SUCCESS;
      end
      else if ClientType = ctInstant then begin
        HomeName := 'Instant';
        Success := True;
      end
      else
        raise Exception.Create('Unknown Oracle client type.');

      if Success then begin
        Len := Length(HomePath) * SizeOf(Char);
        if ClientType = ctInstant then
          Success := RegQueryValueEx(hOracleReg, 'OCIDLL', nil, nil, @HomePath, @Len) = ERROR_SUCCESS
        else
          Success := RegQueryValueEx(hOracleReg, 'ORACLE_HOME', nil, nil, @HomePath, @Len) = ERROR_SUCCESS;

        if Success then begin
          Home := nil;

          for i := 0 to FHomes.Count - 1 do begin
            Home := TOracleHome(FHomes[i]);
            if AnsiStrIComp(PChar(Home.Name), HomeName) = 0 then
              break
            else
              Home := nil;
          end;

          if Home = nil then begin
            // detect OCILite
            IsLite := False;
            Len := Length(Lite) * SizeOf(Char);
            if RegQueryValueEx(hOracleReg, 'OLITE', nil, nil, @Lite, @Len) = ERROR_SUCCESS then
              if StrLIComp(Lite, 'YES', 3) = 0 then
                IsLite := True;

            if FileExists('.\tnsnames.ora') then
              TNSPath := '.'
            else begin
              Len := Length(TNSPath) * SizeOf(Char);
              if RegQueryValueEx(hOracleReg, 'TNS_ADMIN', nil, nil, @TNSPath, @Len) <> ERROR_SUCCESS then begin
                TempStr := GetEnvironmentVariable('TNS_ADMIN');
                if TempStr <> '' then
                  Move(TempStr[1], TNSPath, Length(TempStr) * SizeOf(Char))
                else
                  TNSPath[0] := #0; // clear value;
              end;
            end;

            Len := Length(NLSLang) * SizeOf(Char);
            if RegQueryValueEx(hOracleReg, 'NLS_LANG', nil, nil, @NLSLang, @Len) <> ERROR_SUCCESS then begin
              TempStr := GetEnvironmentVariable('NLS_LANG');
              if TempStr <> '' then
                Move(TempStr[1], NLSLang, Length(TempStr) * SizeOf(Char))
              else
                NLSLang[0] := #0; // clear value;
            end;

            if (ClientType = ctInstant) and (ExtractFileExt(HomePath) <> '') then
              Home := OracleHomes.Add(HomeName, ExtractFilePath(HomePath), ExtractFileName(HomePath), TNSPath, NLSLang, IsLite)
            else
              Home := OracleHomes.Add(HomeName, HomePath, '', TNSPath, NLSLang, IsLite);
          end;

          // Default home
          if (FDefault = nil) and (Home <> nil) and (Key = '') and (ClientType <> ctInstant) and CheckHomePath(Home.Path) then
            FDefault := Home;
        end;
      end;
    finally
      RegCloseKey(hOracleReg);
    end;
  end;

  procedure AddInstantClients;
  var
    iPos, i: integer;
    PathStr, Str: string;
    Home: TOracleHome;
    InstantCount: integer;
  begin
    InstantCount := 0;
    PathStr := GetEnvironmentPathVariable;
    while True do begin
      iPos := Pos(';', PathStr);
      if iPos > 0 then begin
        Str := Trim(Copy(PathStr, 1, iPos - 1));
        PathStr := Copy(PathStr, iPos + 1, Length(PathStr));;
        if Str = '' then
          continue;
      end
      else begin
        Str := Trim(PathStr);
        PathStr := '';
        if Str = '' then
          break;
      end;

      if Str[Length(Str)] <> '\' then
        Str := Str + '\';

      Home := nil;

      for i := 0 to FHomes.Count - 1 do begin
        Home := TOracleHome(FHomes[i]);

        if (AnsiStrIComp(PChar(Home.Path + '\'), PChar(Str)) = 0) or
           (AnsiStrIComp(PChar(Home.Path + '\bin\'), PChar(Str)) = 0)
        then begin
          if (FDefault = nil) and CheckHomePath(TOracleHome(FHomes[i]).Path) then
            FDefault := Home;
          break;
        end
        else
          Home := nil;
      end;

      if (Home = nil) and CheckHomePath(Str) then begin
        Inc(InstantCount);
        Home := OracleHomes.Add('InstantClient' + IntToStr(InstantCount), Str, '', '', '', True);
        if FDefault = nil then
          FDefault := Home;
      end;
    end;
  end;
{$ENDIF}

{$IFNDEF CLR}
const
  BufLen = 100;
  sOra7HomeKey = 'SOFTWARE\ORACLE\ORACLE_HOMES';
  sOra8HomeKey = 'SOFTWARE\ORACLE';
var
  i: integer;
  hOracleReg: HKEY;
  Len: UINT;
  Key: string;
  NumSubKeys: Integer;
  MaxSubKeyLen: Integer;
  Home: TOracleHome;
{$ENDIF}
begin
  hInitLock.Enter;
  try
    if Inited then
      Exit;

  {$IFNDEF CLR}
    hOracleReg := 0;
    try
      // Read home information
      OracleHomes.Clear;

      if CheckHomePath('.') then
        FDefault := OracleHomes.Add('', '.', '', '', '', False);

      // find Oracle 7 homes
      if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(sOra7HomeKey),
        0, KEY_READ, hOracleReg) = ERROR_SUCCESS
      then begin
        if RegQueryInfoKey(hOracleReg, nil, nil, nil, @NumSubKeys,
          @MaxSubKeyLen, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS
        then begin
          for i := 0 to NumSubKeys - 1 do begin
            Len := MaxSubKeyLen + 1;
            SetLength(Key, Len);
            RegEnumKeyEx(hOracleReg, i, PChar(Key), Len, nil, nil, nil, nil);
            SetLength(Key, Len);
            AddToOracleHomes(sOra7HomeKey, Key, ctOracle7);
          end;
        end;
        RegCloseKey(hOracleReg);
      end;

      // find 10g homes
      if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(sOra8HomeKey),
        0, KEY_READ, hOracleReg) = ERROR_SUCCESS
      then begin
        if RegQueryInfoKey(hOracleReg, nil, nil, nil, @NumSubKeys,
          @MaxSubKeyLen, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS
        then begin
          for i := 0 to NumSubKeys - 1 do begin
            Len := MaxSubKeyLen + 1;
            SetLength(Key, Len);
            RegEnumKeyEx(hOracleReg, i, PChar(Key), Len, nil, nil, nil, nil);
            if (AnsiStrLIComp(Pchar(Key), 'HOME', 4) = 0) or (AnsiStrLIComp(Pchar(Key), 'KEY_', 4) = 0) then begin
              Setlength(Key, Len);
              AddToOracleHomes(sOra8HomeKey, Key, ctOracle8);
            end;
          end;
        end;
      end;

      // Find instant clients
      AddInstantClients;

      // find home in SOFTWARE\ORACLE
      AddToOracleHomes(sOra8HomeKey, '', ctOracle8);

      // read OCIDLL registry variable
      AddToOracleHomes(sOra8HomeKey, '', ctInstant);
    finally
      if hOracleReg <> 0 then
        RegCloseKey(hOracleReg);
    end;

    // If default Home is not specified then set it
    if FDefault = nil then
      for i := 0 to FHomes.Count - 1 do begin
        Home := TOracleHome(FHomes[i]);
        if CheckHomePath(Home.Path) then begin
          FDefault := Home;
          break;
        end;
      end;
  {$ELSE}
    Add('', '', '', '', False);
    FDefault := TOracleHome(FHomes[0]);
  {$ENDIF}

    FInited := True;
  finally
    hInitLock.Leave;
  end;
end;
{$ELSE}
begin
  Add(
    '',
    GetEnvironmentVariable('ORACLE_HOME'),
    GetEnvironmentVariable('OCIDLL'),
    GetEnvironmentVariable('TNS_ADMIN'),
    GetEnvironmentVariable('NLS_LANG'),
    False
  );
  FDefault := TOracleHome(FHomes[0]);

  FInited := True;
end;
{$ENDIF}

procedure TOracleHomes.Release;
var
  i: Integer;
begin
  hInitLock.Enter;
  try
    // try to release all homes before destroy
    for i := FHomes.Count - 1 downto 0 do
      TOracleHome(FHomes[i]).Release;
  {$IFDEF NET}
    if FDirect <> nil then
      FDirect.Release;
  {$ENDIF}
    FDefault := nil;

    Clear;
  finally
    hInitLock.Leave;
  end;
end;

function TOracleHomes.AddHome(const Path: string): TOracleHome;
begin
  Init;

  Result := Add('', Path, '', '', '', False);
end;

function TOracleHomes.AddHome(const Name, Path: string): TOracleHome;
begin
  Init;

  Result := Add(Name, Path, '', '', '', False);
end;

function TOracleHomes.AddHome(const Name, Path, TNSPath: string; IsLite: boolean): TOracleHome;
begin
  Init;

  Result := Add(Name, Path, '', TNSPath, '', IsLite);
end;

function TOracleHomes.AddHome(const Name, Path, TNSPath, NLSLang: string; IsLite: boolean = False): TOracleHome;
begin
  Init;

  Result := Add(Name, Path, '', TNSPath, NLSLang, IsLite);
end;

function TOracleHomes.FindHome(const HomeName: string): TOracleHome;
{$IFNDEF CLR}
var
  i: Integer;
{$ENDIF}
begin
  Init;

{$IFNDEF CLR}
  Result := nil;

  if HomeName <> '' then begin
    for i := 0 to Count - 1 do
      if AnsiStrComp(PChar(Homes[i].Name), PChar(HomeName)) = 0 then begin
        Result := Homes[i];
        exit;
      end;
  end
  else
{$ENDIF}
    Result := Default
end;

function TOracleHomes.GetHome(const HomeName: string): TOracleHome;
begin
  Result := FindHome(HomeName);
  if Result = nil then
    raise Exception.Create('Invalid Oracle Home: ' + HomeName);
end;

{ TDirectServerInfo }

constructor TDirectServerInfo.Create;
begin
  inherited;

  Clear;
  FFullInfo := False;
end;

function TDirectServerInfo.GetProtocol: string;
begin
  if FProtocol <> '' then
    Result := FProtocol
  else if FFullInfo then
    Result := DefaultProtocol
  else
    Result := '';
end;

function TDirectServerInfo.GetHost: string;
begin
  if FHost <> '' then
    Result := FHost
  else if FFullInfo then
    Result := DefaultHost
  else
    Result := '';
end;

function TDirectServerInfo.GetPort: string;
begin
  if FPort <> '' then
    Result := FPort
  else if FFullInfo then
    Result := DefaultPort
  else
    Result := '';
end;

function TDirectServerInfo.GetSID: string;
begin
  if FSID <> '' then
    Result := FSID
  else if FFullInfo  and (FServiceName = '') then
    Result := DefaultSID
  else
    Result := '';
end;

function TDirectServerInfo.GetServiceName: string;
begin
  if FServiceName <> '' then
    Result := FServiceName
  else
    Result := '';
end;

procedure TDirectServerInfo.Clear;
begin
  FHost := '';
  FPort := '';
  FSID := '';
  FServiceName := '';
end;

function TDirectServerInfo.GetServerInfo: string;
begin
  if (FSID <> '') and (FServiceName <> '') then
    raise Exception.Create('SID and Service Name cannot be defined at the same time');

  if FSID <> '' then
    Result := ':' + FSID
  else if FServiceName <> '' then
    Result := '/' + FServiceName
  else if FFullInfo then
    Result := ':' + DefaultSID
  else
    Result := '';

  if FPort <> '' then
    Result := ':' + FPort + Result
  else if Result <> '' then
    Result := ':' + DefaultPort + Result;

  if FHost <> '' then
    Result := FHost + Result
  else if Result <> '' then
    Result := DefaultHost + Result;

  if FProtocol <> '' then
    Result := FProtocol + '://' + Result
  else if FFullInfo then
    Result := DefaultProtocol + '://' + Result;
end;

procedure TDirectServerInfo.SetServerInfo(const Value: string);
var
  i, j, l, k: integer;
  Str: string;
  StrArray: array of string;
begin
  Clear;

  k := Pos('://', Value);
  if k > 0 then begin
    FProtocol := Copy(Value, 1, k - 1);
    k := k + 2;
  end
  else
    FProtocol := '';

  SetLength(StrArray, 0);

  // Parse server name back to front
  i := Length(Value);
  l := i;
  while i > k do begin
    if CharInSet(Value[i], [':', '/']) then begin
      j := Length(StrArray);
      SetLength(StrArray, j + 1);
      if Value[i] = '/' then
        StrArray[j] := copy(Value, i, l - i + 1)
      else
        StrArray[j] := copy(Value, i + 1, l - i);
      l := i - 1;
      if Length(StrArray) = 2 then
        break;
    end;

    Dec(i);
  end;

  j := Length(StrArray);
  SetLength(StrArray, j + 1);
  StrArray[j] := copy(Value, k + 1, l - k);

  if Length(StrArray) > 3 then
    raise Exception.Create('Invalid server name')
  else begin
    if (Length(StrArray) > 0) and
       (Length(StrArray) < 3) and
       (Length(StrArray[0]) > 0) and
       (StrArray[0][1] = '/')
    then begin
      SetLEngth(StrArray, 3);
      StrArray[2] := StrArray[1];
      StrArray[1] := '';
    end;

    if Length(StrArray) > 1 then begin
      // Swap first and last array elements
      Str := StrArray[0];
      StrArray[0] := StrArray[Length(StrArray) - 1];
      StrArray[Length(StrArray) - 1] := Str;
    end;
  end;

  if Length(StrArray) > 0 then begin
    Str := StrArray[0];
    if Str <> '' then
      FHost := StrArray[0];
  end;

  if Length(StrArray) > 1 then begin
    Str := StrArray[1];
    if Str <> '' then
      FPort := Str;
  end;

  if Length(StrArray) > 2 then begin
    Str := StrArray[2];
    if Str <> '' then begin
      i := Pos('=', Str);
      if i = 0 then
        if Str[1] = '/' then
          ServiceName := copy(Str, 2, Length(Str) - 1)
        else
          SID := Str
      else begin
        if UpperCase(copy(Str, 1, 4)) = 'SID=' then
          SID := copy(Str, 5, Length(Str) - 4)
        else if UpperCase(copy(Str, 1, 3)) = 'SN=' then
          ServiceName := copy(Str, 4, Length(Str) - 3)
        else
          RaiseError('Invalid SID or Service Name');
      end;
    end;
  end;
end;

{ TOCIEnvironment }

constructor TOCIEnvironment.Create(AHome: TOracleHome; AUnique: boolean; AUnicodeEnv: boolean; ASubscriptionPort: Integer);
begin
  inherited Create;

  hInitLock := TCriticalSection.Create;

  FHome := AHome;
  FUnique := AUnique;
  FUnicodeEnv := AUnicodeEnv;
  FSubscriptionPort := ASubscriptionPort;
  FInited := False;
  FhOCIEnv := nil;
  FNativeHandle := True;

  FOCISvcCtxs := TList.Create;
end;

constructor TOCIEnvironment.Create(AHome: TOracleHome; hOCIEnv: pOCIEnv);
var
  hOCIError: pOCIError;
begin
  inherited Create;

  hInitLock := TCriticalSection.Create;

  FHome := AHome;
  FhOCIEnv := hOCIEnv;
  FNativeHandle := False;

  hOCIError := AllocErrorHandle;
  try
    FUnicodeEnv := FHome.OCI8.IsUnicodeEnv(hOCIEnv, hOCIError);
  finally
    FreeErrorHandle(hOCIError);
  end;

  FUnique := False;

  FOCISvcCtxs := TList.Create;
end;

destructor TOCIEnvironment.Destroy;
begin
  Release;

  FOCISvcCtxs.Free;

  hInitLock.Free;

  inherited;
end;

procedure TOCIEnvironment.AddOCISvcCtx(Value: TOCISvcCtx);
begin
  hSvcCtxLock.Enter;
  try
  {$IFNDEF AUTOREFCOUNT}
    AddRef;
  {$ENDIF}
    FOCISvcCtxs.Add(Value);
  finally
    hSvcCtxLock.Leave;
  end;
end;

procedure TOCIEnvironment.RemoveOCISvcCtx(Value: TOCISvcCtx);
begin
  hSvcCtxLock.Enter;
  try
    FOCISvcCtxs.Remove(Value);

  {$IFNDEF AUTOREFCOUNT}
    ReleaseRef;
  {$ELSE}
    if FUnique and (FOCISvcCtxs.Count = 0) and (FHome <> nil) then
      FHome.RemoveEnvironment(Self);
  {$ENDIF}

  finally
    hSvcCtxLock.Leave;
  end;
end;

procedure TOCIEnvironment.ClearOCISvcCtxs;
var
  i: integer;
begin
  hSvcCtxLock.Enter;
  try
    for i := FOCISvcCtxs.Count - 1 downto 0 do
      TOCISvcCtx(FOCISvcCtxs[i]).Release;
  finally
    hSvcCtxLock.Leave;
  end;
end;

procedure TOCIEnvironment.OCIError(Res: sword);
begin
  raise EOCIInitError.CreateFmt(SCannotInitOCI, [Res]);
end;

procedure TOCIEnvironment.SetSubscriptionPort(Port: Integer);
begin
  if (Port <> 0) and (Home.OCIVersion >= 10200) then begin
    TOraError.Check(
      @Home.OCI8.OCIErrorGet,
      Home.OCI8.OCIAttrSet2(hOCIEnv, OCI_HTYPE_ENV, Port, 0, OCI_ATTR_SUBSCR_PORTNO, hOCIError),
      UnicodeEnv,
      hOCIError
    );
    FSubscriptionPort := Port;
  end;
end;

{$IFNDEF AUTOREFCOUNT}
procedure TOCIEnvironment.AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

procedure TOCIEnvironment.ReleaseRef;
begin
  if InterlockedDecrement(FRefCount) = 0 then
    if FHome = nil then
      Free
    else if FUnique then
      FHome.RemoveEnvironment(Self);
end;
{$ENDIF}

procedure TOCIEnvironment.Init;
var
  InitMode: integer;
  EnvInitMode: integer;
  Res: sword;
begin
  hInitLock.Enter;
  try
    if FInited then
      Exit;

    if FNativeHandle then begin
      if OCI80 in Home.PossibleOCICallStyles then begin
        // WAR problem with OCI_THREADED on HandelFree(hStmt... with Oracle 8.0.4
        //                  and create direct path handle
        InitMode := OCI_DEFAULT or OCI_OBJECT;
        if UnicodeEnv and (Home.OCIVersion >= 9000) then begin
          InitMode := InitMode or OCI_UTF16;
          if (Home.OCIVersion >= 10000) then
            if OCINCharLiteralReplace then
              InitMode := InitMode or OCI_NCHAR_LITERAL_REPLACE_ON;
        end;
        if OCIThreaded and (Home.OCIVersion > 8040) then // supports from OCI 8.0.5
          InitMode := InitMode or OCI_THREADED;
        if OCIShared and (Home.OCIVersion >= 8140) then  // supports from OCI 8.1.5
          InitMode := InitMode or OCI_SHARED;
        if OCIEvents and (Home.OCIVersion >= 8140) and (Home.OCIVersion >= OCIEventsVersion) then
          InitMode := InitMode or OCI_EVENTS;

        if Home.OCIVersion >= 8100 then begin
        // Oracle 8.1.5
          if not OCIMutexed and OCIThreaded {$IFDEF LITE} or (Home.OCIVersion < 9000) {$ENDIF}then
            InitMode := InitMode or OCI_NO_MUTEX;

        {$IFDEF IS_UTF8}
          if (Home.OCIVersion >= 9000) and (OCI_UTF16 and InitMode = 0) then
            Res := Home.OCI8.OCIEnvNlsCreate(FhOCIEnv, InitMode, nil, nil, nil, nil, 0, nil, GetUTF8Charset(Home.OCIVersion), OCI_UTF16ID)
          else
        {$ENDIF}
            Res := Home.OCI8.OCIEnvCreate(FhOCIEnv, InitMode, nil, nil, nil, nil, 0, nil);
          if Res <> OCI_SUCCESS then
            OCIError(Res);
        end
        else begin
        // Oracle 8.0.x
          EnvInitMode := OCI_DEFAULT;
          if {$IFDEF LITE} not OCIMutexed and{$ENDIF} OCIThreaded and (Home.OCIVersion <= 8140) then // supports from OCI 8.1.5
            EnvInitMode := EnvInitMode or OCI_ENV_NO_MUTEX;

          Res := Home.OCI8.OCIInitialize(InitMode, nil, nil, nil, nil);
          if Res <> OCI_SUCCESS then
            OCIError(Res);

          Res := Home.OCI8.OCIEnvInit(FhOCIEnv, EnvInitMode, 0, nil);
          if Res <> OCI_SUCCESS then
            OCIError(Res);
        end;

        FhOCIError := AllocErrorHandle;

        if FNativeHandle then
          SetSubscriptionPort(SubscriptionPort);
      end;

      if OCI73 in Home.PossibleOCICallStyles then
        if OCIThreaded then
          Home.OCI7.SetThreadSafety(True);
    end
    else
      FhOCIError := AllocErrorHandle;

    FInited := True;
  finally
    hInitLock.Leave;
  end;
end;

procedure TOCIEnvironment.Release;
begin
  hInitLock.Enter;
  try
    if not Inited then
      Exit;

    ClearOCISvcCtxs;

  {$IFNDEF AUTOREFCOUNT}
    if FRefCount > 0 then
      if not Home.Owner.FInDestroying then
        raise Exception.Create('OCI Environment cannot be released, because it is being used');
  {$ENDIF}

    FreeErrorHandle(FhOCIError);
    FhOCIError := nil;

    if OCI80 in Home.PossibleOCICallStyles then begin
      if FNativeHandle then
        Home.OCI8.OCIHandleFree(FhOCIEnv, OCI_HTYPE_ENV);
      FhOCIEnv := nil;
    end;

    FInited := False;
  finally
    hInitLock.Leave;
  end;
end;

function TOCIEnvironment.AllocErrorHandle: pOCIError;
var
  Res: sword;
begin
  Res := Home.OCI8.OCIHandleAlloc(FhOCIEnv, Result, OCI_HTYPE_ERROR, 0, nil);
  if Res <> OCI_SUCCESS then
    OCIError(Res);
end;

procedure TOCIEnvironment.FreeErrorHandle(hOCIError: pOCIError);
begin
  if hOCIError <> nil then
    Home.OCI8.OCIHandleFree(hOCIError, OCI_HTYPE_ERROR);
end;

function TOCIEnvironment.AllocSvcCtxHandle: pOCISvcCtx;
var
  Res: sword;
begin
  Res := Home.OCI8.OCIHandleAlloc(hOCIEnv, Result, OCI_HTYPE_SVCCTX, 0, nil);
  Home.OCI8.Check(Res, UnicodeEnv, hOCIError);
end;

function TOCIEnvironment.AllocPooledSvcCtxHandle(const PoolName: string; hOCIAuthInfo: pOCIAuthInfo): pOCISvcCtx;
var
  p: IntPtr;
  Res: Integer;
  Size: Integer;
  retTagInfo: IntPtr;
  retTagInfo_len: Cardinal;
  found: LongBool;
begin
  p := StringToHGlobalOCI(PoolName, Size, UnicodeEnv);
  Res := Home.OCI8.OCISessionGet(hOCIEnv, hOCIError, Result, hOCIAuthInfo, p, Size, nil, 0, RetTagInfo,
    retTagInfo_len, found, OCI_SESSGET_SPOOL);
  FreeStringOCI(p, UnicodeEnv);
  Home.OCI8.Check(Res, UnicodeEnv, hOCIError);
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
function TOCIEnvironment.AllocMTSPooledSvcCtxHandle(const Server, UserName, Password: string): pOCISvcCtx;
var
  Res: Integer;
begin
  Res := Home.MTS.OraMTSSvcGet(PAnsiChar(AnsiString(UserName)), PAnsiChar(AnsiString(Password)),
      PAnsiChar(AnsiString(Server)), Result, FhOCIEnv, ORAMTS_CFLG_NOIMPLICIT);
  Home.OCI8.Check(Res, UnicodeEnv, hOCIError);
end;
{$ENDIF}
{$ENDIF}

procedure TOCIEnvironment.FreeSvcCtxHandle(hOCISvcCtx: pOCISvcCtx);
begin
  Home.OCI8.OCIHandleFree(hOCISvcCtx, OCI_HTYPE_SVCCTX);
end;

{ TDirectEnvironment }

procedure TDirectEnvironment.Init;
{$IFDEF NET}
var
  InitMode: integer;
{$ENDIF}
begin
{$IFDEF NET}
  hInitLock.Enter;
  try
    if FInited then
      Exit;

    InitMode := OCI_DEFAULT or OCI_OBJECT;
    if UnicodeEnv then begin
      InitMode := InitMode or OCI_UTF16;
      if OCINCharLiteralReplace then
        InitMode := InitMode or OCI_NCHAR_LITERAL_REPLACE_ON;
    end;
    if OCIThreaded then
      InitMode := InitMode or OCI_THREADED;
    if OCIShared then
      InitMode := InitMode or OCI_SHARED;
    if OCIEvents then
      InitMode := InitMode or OCI_EVENTS;

    if not OCIMutexed and OCIThreaded then
      InitMode := InitMode or OCI_ENV_NO_MUTEX;

    if Home.OCI8.OCIEnvCreate(FhOCIEnv, InitMode, nil, nil, nil, nil, 0, nil) <> OCI_SUCCESS then
      RaiseError('Can''t initialize Direct mode');

    FhOCIError := AllocErrorHandle;

    FInited := True;
  finally
    hInitLock.Leave;
  end;
{$ELSE}
  RaiseError(SDirectModeNotSupported);
{$ENDIF}
end;

{ TOCISvcCtx }

constructor TOCISvcCtx.Create(AEnvironment: TOCIEnvironment; AUseUnicode: boolean);
begin
  inherited Create;

  SetEnvironment(AEnvironment);
  FUseUnicode := AUseUnicode;
  FNativeHandle := True;
end;

constructor TOCISvcCtx.Create(AEnvironment: TOCIEnvironment; AhOCISvcCtx: pOCISvcCtx; AUseUnicode: boolean);
begin
  inherited Create;

  SetEnvironment(AEnvironment);
  FUseUnicode := AUseUnicode;
  FNativeHandle := False;

  FhOCISvcCtx := AhOCISvcCtx;
end;

destructor TOCISvcCtx.Destroy;
begin
  FreeSvcCtxHandle;
  ResetEnvironment;

  inherited;
end;

{$IFNDEF AUTOREFCOUNT}
procedure TOCISvcCtx.AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

procedure TOCISvcCtx.ReleaseRef;
begin
  if InterlockedDecrement(FRefCount) = 0 then
    Free;
end;
{$ENDIF}

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
function TOCISvcCtx.GetMTS: TMTSAPI;
begin
  if FMTS = nil then
    FMTS := Home.MTS;

  Result := FMTS;
end;
{$ENDIF}
{$ENDIF}

procedure TOCISvcCtx.ReleaseEnvironment;
begin
  if FEnvironment <> nil then begin
    FEnvironment.FreeErrorHandle(FhOCIError);

    FHome := nil;
    FOCI7 := nil;
    FOCI8 := nil;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    FMTS:= nil;
  {$ENDIF}
  {$ENDIF}

    FhOCIEnv := nil;
    FhOCIError := nil;

    FUnicodeEnv := False;

    FEnvironment.RemoveOCISvcCtx(Self);
    FEnvironment := nil;
  end;
end;

procedure TOCISvcCtx.ResetEnvironment;
begin
  SetEnvironment(nil);
end;

procedure TOCISvcCtx.SetEnvironment(Value: TOCIEnvironment);
begin
  ReleaseEnvironment;

  if (Value <> nil) and (not Value.Inited) then
    Value.Init;

  FEnvironment := Value;
  if Value <> nil then begin
    FEnvironment.AddOCISvcCtx(Self);

    FHome := Environment.Home;
    FOCI7 := Environment.Home.OCI7;
    FOCI8 := Environment.Home.OCI8;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    FMTS:= nil;
  {$ENDIF}
  {$ENDIF}

    FhOCIEnv := Environment.hOCIEnv;
    FhOCIError := FEnvironment.AllocErrorHandle;

    FUnicodeEnv := Environment.UnicodeEnv;
  end;
end;

procedure TOCISvcCtx.AllocSvcCtxHandle;
begin
  Assert(FhOCISvcCtx = nil);

  FhOCISvcCtx := Environment.AllocSvcCtxHandle;
end;

procedure TOCISvcCtx.AllocPooledSvcCtxHandle(const PoolName: string; hOCIAuthInfo: pOCIAuthInfo);
begin
  Assert(FhOCISvcCtx = nil);

  FhOCISvcCtx := Environment.AllocPooledSvcCtxHandle(PoolName, hOCIAuthInfo);
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
procedure TOCISvcCtx.AllocMTSPooledSvcCtxHandle(const Server, UserName, Password: string);
begin
  Assert(FhOCISvcCtx = nil);

  FhOCISvcCtx := Environment.AllocMTSPooledSvcCtxHandle(Server, UserName, Password);
end;
{$ENDIF}
{$ENDIF}

procedure TOCISvcCtx.FreeSvcCtxHandle;
begin
  if FhOCISvcCtx <> nil then begin
    if FNativeHandle then
      Environment.FreeSvcCtxHandle(FhOCISvcCtx);
    FhOCISvcCtx := nil;
  end;
end;

procedure TOCISvcCtx.Release;
begin
  if FRefCount > 0 then
    if not Home.Owner.FInDestroying then
      raise Exception.Create('OCISvcCtx cannot be released, because it is being used')
    else begin
      FreeSvcCtxHandle;
      SetEnvironment(nil);
    end;
end;

procedure TOCISvcCtx.GetLDA(Value: PLDA);
begin
  OCI8.Check(OCI8.OCISvcCtxToLda(hOCISvcCtx, hOCIError, Value), Self);
end;

procedure TOCISvcCtx.SetLDA(Value: PLDA);
begin
  OCI8.Check(OCI8.OCILdaToSvcCtx(FhOCISvcCtx, hOCIError, Value), Self);
end;

initialization

  hHomeLock := TCriticalSection.Create;
  hEnvLock := TCriticalSection.Create;
  hSvcCtxLock := TCriticalSection.Create;

  OracleHomes := TOracleHomes.Create;

  ObjectVersion := True;
  OCIUnicode := False;
  SubscriptionPort := 0;
  OCIThreaded := True;
  OCIMutexed := True;
  OCIShared := False;
  OCIEvents := True; // need for using subscription
  OCIEventsVersion := 10200; // for change notification

finalization

  OracleHomes.Free;

  hHomeLock.Free;
  hEnvLock.Free;
  hSvcCtxLock.Free;

end.

