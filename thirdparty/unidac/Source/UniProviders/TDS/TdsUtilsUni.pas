//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsUtilsUni;
interface
uses
{$IFDEF MSWINDOWS}
Windows,Messages,
{$ENDIF}
{$IFDEF POSIX}
Posix.SysSocket,
{$ENDIF}
{$IFDEF NEXTGEN}
Generics.Collections,
{$ENDIF}
{$IFDEF HAVE_COMPRESS_INTERNAL}
ZLib,ZLibConst,
{$ENDIF}
SysUtils,Classes,SyncObjs,
{$IFNDEF FPC}
{$IFNDEF MSWINDOWS}
System.IOUtils,
{$ENDIF}
{$ENDIF}
CLRClasses,CRTypes,CRFunctions,CRVio,CRVioTcp,CRBigInteger,
{$IFNDEF UNIDACPRO}
TdsSSLConsts;
{$ELSE}
TdsSSLConstsUni;
{$ENDIF}
{$UNDEF TRIAL}
{$UNDEF TRIAL_BPLCALL}
type
OO00COQOQ0=(
OOOCCOQOQ0,
OQOCCOQOQ0,
OCOCCOQOQ0,
O0QCCOQOQ0,
OOQCCOQOQ0,
OQQCCOQOQ0,
OCQCCOQOQ0,
O0CCCOQOQ0,
OOCCCOQOQ0,
OQCCCOQOQ0,
OCCCCOQOQ0,
O000COQOQ0
);
TScSymmetricAlgorithms=set of OO00COQOQ0;
OCQ0COQOQ0=(
OQ00COQOQ0,
OC00COQOQ0,
O0O0COQOQ0,
OOO0COQOQ0,
OQO0COQOQ0,
OCO0COQOQ0,
O0Q0COQOQ0,
OOQ0COQOQ0,
OQQ0COQOQ0
);
OOOOCOQOQ0=(O0C0COQOQ0,OOC0COQOQ0,OQC0COQOQ0,OCC0COQOQ0,O00OCOQOQ0,OO0OCOQOQ0,OQ0OCOQOQ0,OC0OCOQOQ0,O0OOCOQOQ0);
TScHashAlgorithms=set of OOOOCOQOQ0;
O0COCOQOQ0=(OQOOCOQOQ0,OCOOCOQOQ0,O0QOCOQOQ0,OOQOCOQOQ0,OQQOCOQOQ0,OCQOCOQOQ0);
TScHMACAlgorithms=set of O0COCOQOQ0;
OOOQQOQOQ0=(
OOCOCOQOQ0,OQCOCOQOQ0,
OCCOCOQOQ0,O00QQOQOQ0,
OO0QQOQOQ0,OQ0QQOQOQ0,OC0QQOQOQ0,
O0OQQOQOQ0);
TScKeyExchangeAlgorithms=set of OOOQQOQOQ0;
OOQQQOQOQ0=(OQOQQOQOQ0,OCOQQOQOQ0,O0QQQOQOQ0);
TScAsymmetricAlgorithms=set of OOQQQOQOQ0;
OQCCQOQOQ0=(OQQQQOQOQ0,OCQQQOQOQ0,O0CQQOQOQ0,
OOCQQOQOQ0,OQCQQOQOQ0,OCCQQOQOQ0,
O00CQOQOQ0,OO0CQOQOQ0,
OQ0CQOQOQ0,OC0CQOQOQ0,O0OCQOQOQ0,
OOOCQOQOQ0,OQOCQOQOQ0,OCOCQOQOQ0,O0QCQOQOQ0,
OOQCQOQOQ0,OQQCQOQOQ0,OCQCQOQOQ0,O0CCQOQOQ0,OOCCQOQOQ0);
OOQOQOQOQ0=(
OCCCQOQOQ0,
O000QOQOQ0,OO00QOQOQ0,OQ00QOQOQ0,
OC00QOQOQ0,O0O0QOQOQ0,OOO0QOQOQ0,OQO0QOQOQ0,
OCO0QOQOQ0,O0Q0QOQOQ0,OOQ0QOQOQ0,OQQ0QOQOQ0,
OCQ0QOQOQ0,O0C0QOQOQ0,OOC0QOQOQ0,
OQC0QOQOQ0,OCC0QOQOQ0,O00OQOQOQ0,OO0OQOQOQ0,OQ0OQOQOQ0,
OC0OQOQOQ0,O0OOQOQOQ0,OOOOQOQOQ0,OQOOQOQOQ0,
OCOOQOQOQ0,O0QOQOQOQ0
);
OOCOQOQOQ0=(OQQOQOQOQ0,OCQOQOQOQ0,O0COQOQOQ0);
TScCompressionAlgorithms=set of OOCOQOQOQ0;
OO0QO0QOQ0=(OQCOQOQOQ0,OCCOQOQOQ0,O00QO0QOQ0);
OQ0QO0QOQ0=array of OO0QO0QOQ0;
OCOQO0QOQ0=(OC0QO0QOQ0,O0OQO0QOQ0,OOOQO0QOQ0,OQOQO0QOQ0);
TScSSHChannelPermissions=set of OCOQO0QOQ0;
O0CQO0QOQ0=(O0QQO0QOQ0,OOQQO0QOQ0,OQQQO0QOQ0,OCQQO0QOQ0);
O00CO0QOQ0=(OOCQO0QOQ0,OQCQO0QOQ0,OCCQO0QOQ0);
type
EScError=class(Exception)
protected
FErrorCode:TScErrorCode;
public
constructor Create(const OO0CO0QOQ0:string);overload;
constructor Create(OQ0CO0QOQ0:TScErrorCode);overload;
constructor CreateFmt(const OC0CO0QOQ0:string;const O0OCO0QOQ0:array of const;OOOCO0QOQ0:TScErrorCode);
function Clone:EScError;
property ErrorCode:TScErrorCode read FErrorCode;
end;
OQOCO0QOQ0=class(TCollectionItem)
protected
function OCOCO0QOQ0:string;virtual;
procedure O0QCO0QOQ0(const OOQCO0QOQ0:string);virtual;
public
property OQQCO0QOQ0:string read OCOCO0QOQ0 write O0QCO0QOQ0;
end;
OCQCO0QOQ0=class of OQOCO0QOQ0;
O0CCO0QOQ0=class(TOwnedCollection)
private
OOCCO0QOQ0:TNotifyEvent;
function OQCCO0QOQ0:string;
procedure OO00O0QOQ0(const OOQCO0QOQ0:string);
protected
procedure Update(OQO0O0QOQ0:TCollectionItem);override;
public
procedure Assign(O0Q0O0QOQ0:TPersistent);override;
public
constructor Create(OQQ0O0QOQ0:TPersistent;OCQ0O0QOQ0:OCQCO0QOQ0);
property O0C0O0QOQ0:TNotifyEvent read OOCCO0QOQ0 write OOCCO0QOQ0;
property OOC0O0QOQ0:string read OQCCO0QOQ0 write OO00O0QOQ0;
end;
OQC0O0QOQ0=record
OCC0O0QOQ0:TStream;
O00OO0QOQ0:Int64;
OO0OO0QOQ0:Int64;
end;
OQ0OO0QOQ0=class
private
OC0OO0QOQ0:TStream;
O0OOO0QOQ0:Int64;
OOOOO0QOQ0:Int64;
public
constructor Create(OCOOO0QOQ0:TStream;O0QOO0QOQ0,OOQOO0QOQ0:Int64);
procedure OQQOO0QOQ0(OCQOO0QOQ0:TStream;O0COO0QOQ0,OOCOO0QOQ0:Int64);
procedure OQCOO0QOQ0(O0Q0O0QOQ0:OQ0OO0QOQ0);overload;
procedure OQCOO0QOQ0(const O0Q0O0QOQ0:OQC0O0QOQ0);overload;
property OCCOO0QOQ0:TStream read OC0OO0QOQ0;
property O00Q00QOQ0:Int64 read O0OOO0QOQ0;
property OO0Q00QOQ0:Int64 read OOOOO0QOQ0;
end;
OQ0Q00QOQ0=class
protected
procedure OC0Q00QOQ0(O0OQ00QOQ0:OQ0Q00QOQ0);
function OQOQ00QOQ0:OQ0Q00QOQ0;virtual;abstract;
public
procedure OCOQ00QOQ0(O0Q0O0QOQ0:OQ0Q00QOQ0);virtual;abstract;
end;
O0QQ00QOQ0=class of OQ0Q00QOQ0;
OOQQ00QOQ0=class
private
OQQQ00QOQ0:TCRObjectList;
function OCQQ00QOQ0:integer;
function O0CQ00QOQ0(OOCQ00QOQ0:integer):OQ0Q00QOQ0;
procedure OQCQ00QOQ0(OCCQ00QOQ0:integer;O00C00QOQ0:OQ0Q00QOQ0);
protected
procedure OO0C00QOQ0(OQ0C00QOQ0:OQ0Q00QOQ0);
function OC0C00QOQ0:O0QQ00QOQ0;virtual;
public
constructor Create;
destructor Destroy;override;
procedure OQOC00QOQ0(O0Q0O0QOQ0:OOQQ00QOQ0);virtual;
function OOQC00QOQ0(OQQC00QOQ0:OQ0Q00QOQ0):integer;
procedure OCQC00QOQ0;
procedure O0CC00QOQ0(OOCC00QOQ0:integer);
function OQCC00QOQ0(OCCC00QOQ0:OQ0Q00QOQ0):integer;
procedure O00000QOQ0(OO0000QOQ0:integer;OQ0000QOQ0:OQ0Q00QOQ0);
function OC0000QOQ0(O0O000QOQ0:OQ0Q00QOQ0):integer;
property OOO000QOQ0:integer read OCQQ00QOQ0;
property OQO000QOQ0[Index:integer]:OQ0Q00QOQ0 read O0CQ00QOQ0 write OQCQ00QOQ0;
end;
OCO000QOQ0=procedure(O0Q000QOQ0:TStream;const OOQ000QOQ0:TBytes;OQQ000QOQ0:integer)of object;
OCQ000QOQ0=function(O0C000QOQ0:TStream;var OOC000QOQ0:TBytes;OQC000QOQ0:integer):integer of object;
OCC000QOQ0=procedure(O00O00QOQ0:TObject;var OO0O00QOQ0:boolean)of object;
OQ0O00QOQ0=procedure(OC0O00QOQ0:TObject;out O0OO00QOQ0,OOOO00QOQ0:TBytes)of object;
OQOO00QOQ0=procedure(OCOO00QOQ0:TObject;const O0QO00QOQ0:TBytes;out OOQO00QOQ0:TBytes)of object;
OQQO00QOQ0=procedure(OCQO00QOQ0:TObject)of object;
O0CO00QOQ0=procedure(OOCO00QOQ0:TObject;OQCO00QOQ0:Exception)of object;
OCCO00QOQ0=procedure(O00QC0QOQ0:TObject;OO0QC0QOQ0:boolean)of object;
OQ0QC0QOQ0=procedure(OC0QC0QOQ0:TObject;const O0OQC0QOQ0:string)of object;
OOOQC0QOQ0=procedure(OQOQC0QOQ0:TObject;const OCOQC0QOQ0:string)of object;
O0QQC0QOQ0=procedure(OOQQC0QOQ0:TObject;const OQQQC0QOQ0:string)of object;
OCQQC0QOQ0=interface
['{94EC0E52-388C-49DA-A1C6-3DEF7B27C117}']
procedure O0CQC0QOQ0(const OOCQC0QOQ0:TValueArr;OQCQC0QOQ0,OCCQC0QOQ0:cardinal;
const O00CC0QOQ0:TValueArr;OO0CC0QOQ0:cardinal;var OQ0CC0QOQ0:cardinal;OC0CC0QOQ0:boolean=True);
function O0OCC0QOQ0(const OOOCC0QOQ0:TValueArr;var OQOCC0QOQ0,OCOCC0QOQ0:cardinal;
const O0QCC0QOQ0:TValueArr;OOQCC0QOQ0:cardinal;var OQQCC0QOQ0:cardinal):boolean;
procedure OCQCC0QOQ0(O0CCC0QOQ0:TStream;OOCCC0QOQ0:OCO000QOQ0);
procedure OQCCC0QOQ0(OCCCC0QOQ0:TStream;O000C0QOQ0:OCQ000QOQ0);
end;
OO00C0QOQ0=class(TInterfacedObject,OCQQC0QOQ0)
private
{$IFDEF HAVE_COMPRESS_INTERNAL}
OO0O00OCQ0:TZStreamRec;
OQ0O00OCQ0:TZStreamRec;
OC0O00OCQ0:Boolean;
O0OO00OCQ0:Boolean;
OOOO00OCQ0:TBytes;
OQOO00OCQ0:TBytes;
procedure OCOO00OCQ0;
procedure O0QO00OCQ0;
function OOQO00OCQ0(OQQO00OCQ0:Integer):Integer;
function OCQO00OCQ0(O0CO00OCQ0:Integer):Integer;
{$ENDIF}
public
constructor Create;
destructor Destroy;override;
procedure O0CQC0QOQ0(const OOCQC0QOQ0:TValueArr;OQCQC0QOQ0,OCCQC0QOQ0:cardinal;
const O00CC0QOQ0:TValueArr;OO0CC0QOQ0:cardinal;var OQ0CC0QOQ0:cardinal;OC0CC0QOQ0:boolean=True);
function O0OCC0QOQ0(const OOOCC0QOQ0:TValueArr;var OQOCC0QOQ0,OCOCC0QOQ0:cardinal;
const O0QCC0QOQ0:TValueArr;OOQCC0QOQ0:cardinal;var OQQCC0QOQ0:cardinal):boolean;
procedure OCQCC0QOQ0(O0CCC0QOQ0:TStream;OOCCC0QOQ0:OCO000QOQ0);
procedure OQCCC0QOQ0(OCCCC0QOQ0:TStream;O000C0QOQ0:OCQ000QOQ0);
end;
O0O0C0QOQ0=class
private
OOO0C0QOQ0:TCriticalSection;
OQO0C0QOQ0:TEvent;
OCO0C0QOQ0:integer;
O0Q0C0QOQ0:integer;
OOQ0C0QOQ0:TThreadID;
public
constructor Create;
destructor Destroy;override;
function OOC0C0QOQ0(OQC0C0QOQ0:cardinal=INFINITE):boolean;
procedure OQ0OC0QOQ0;
property OC0OC0QOQ0:integer read OCO0C0QOQ0;
end;
O0OOC0QOQ0=class
public
class function OOOOC0QOQ0(OQOOC0QOQ0:TStream;out OCOOC0QOQ0:string;O0QOC0QOQ0:integer=-1;
OOQOC0QOQ0:Encoding=nil):boolean;
class procedure OC0QQ0QOQ0(O0OQQ0QOQ0:TStream;const OOOQQ0QOQ0:string);
end;
OCOQQ0QOQ0=procedure(O0QQQ0QOQ0:TObject;const OOQQQ0QOQ0:string)of object;
OQQQQ0QOQ0=procedure(OCQQQ0QOQ0:TObject;const O0CQQ0QOQ0:string;OOCQQ0QOQ0:Exception)of object;
OQCQQ0QOQ0=class
private
OCCQQ0QOQ0:TCriticalSection;
O00CQ0QOQ0:OCOQQ0QOQ0;
OO0CQ0QOQ0:OCOQQ0QOQ0;
OQ0CQ0QOQ0:OCOQQ0QOQ0;
OC0CQ0QOQ0:OQQQQ0QOQ0;
protected
procedure O0OCQ0QOQ0(const OOOCQ0QOQ0:string);virtual;
procedure OQOCQ0QOQ0(const OCOCQ0QOQ0:string);virtual;
procedure O0QCQ0QOQ0(const OOQCQ0QOQ0:string);virtual;
procedure OQQCQ0QOQ0(const OCQCQ0QOQ0:string;O0CCQ0QOQ0:Exception);virtual;
public
constructor Create;
destructor Destroy;override;
property OCCCQ0QOQ0:OCOQQ0QOQ0 read O00CQ0QOQ0 write O00CQ0QOQ0;
property O000Q0QOQ0:OCOQQ0QOQ0 read OO0CQ0QOQ0 write OO0CQ0QOQ0;
property OO00Q0QOQ0:OCOQQ0QOQ0 read OQ0CQ0QOQ0 write OQ0CQ0QOQ0;
property OQ00Q0QOQ0:OQQQQ0QOQ0 read OC0CQ0QOQ0 write OC0CQ0QOQ0;
end;
OC00Q0QOQ0=class
public
class procedure O0O0Q0QOQ0(OOO0Q0QOQ0:OQCQQ0QOQ0;const OOOCQ0QOQ0:string);overload;
class procedure O0O0Q0QOQ0(OOO0Q0QOQ0:OQCQQ0QOQ0;const OOOCQ0QOQ0:string;const OQO0Q0QOQ0:array of const);overload;
class procedure OCO0Q0QOQ0(O0Q0Q0QOQ0:OQCQQ0QOQ0;const OCOCQ0QOQ0:string);overload;
class procedure OCO0Q0QOQ0(O0Q0Q0QOQ0:OQCQQ0QOQ0;const OCOCQ0QOQ0:string;const OOQ0Q0QOQ0:array of const);overload;
class procedure OQQ0Q0QOQ0(OCQ0Q0QOQ0:OQCQQ0QOQ0;const OOQCQ0QOQ0:string);overload;
class procedure OQQ0Q0QOQ0(OCQ0Q0QOQ0:OQCQQ0QOQ0;const OOQCQ0QOQ0:string;const O0C0Q0QOQ0:array of const);overload;
class procedure OOC0Q0QOQ0(OQC0Q0QOQ0:OQCQQ0QOQ0;const OCQCQ0QOQ0:string;O0CCQ0QOQ0:Exception);overload;
class procedure OOC0Q0QOQ0(OQC0Q0QOQ0:OQCQQ0QOQ0;const OCQCQ0QOQ0:string;const OCC0Q0QOQ0:array of const;O0CCQ0QOQ0:Exception);overload;
end;
function O00OQ0QOQ0(const OO0OQ0QOQ0:string):Encoding;
function O0OOQ0QOQ0(const OOOOQ0QOQ0:string;const OQOOQ0QOQ0:Char):TStringList;overload;
procedure O0OOQ0QOQ0(OCOOQ0QOQ0:TStrings;const OOOOQ0QOQ0:string;const OQOOQ0QOQ0:Char;O0QOQ0QOQ0:Boolean);overload;
function OOCOQ0QOQ0(const OQCOQ0QOQ0:string):string;
function O00QOCOOQ0(const OO0QOCOOQ0:TBytes):string;overload;
function O00QOCOOQ0(const OO0QOCOOQ0:TBytes;const O0OQOCOOQ0:string):string;overload;
function OOOQOCOOQ0(const OQOQOCOOQ0:string):TBytes;
function OCQQOCOOQ0(const O0CQOCOOQ0:Char):Boolean;
function OOCQOCOOQ0(const OQCQOCOOQ0:string):string;
function OCCQOCOOQ0(const O00COCOOQ0:string):string;
function OQ0COCOOQ0(const OC0COCOOQ0,O0OCOCOOQ0:string;OOOCOCOOQ0:integer=1):integer;
function OQQCOCOOQ0(const OCQCOCOOQ0,O0CCOCOOQ0:string;OOCCOCOOQ0:integer=1):integer;
function OC00OCOOQ0(const O0O0OCOOQ0,OOO0OCOOQ0:string):boolean;
function OCO0OCOOQ0:cardinal;
function OQC0OCOOQ0:TDateTime;
function OQ0OOCOOQ0(OC0OOCOOQ0:string=''):string;
procedure O0QOOCOOQ0(OOQOOCOOQ0:TCriticalSection);
procedure OQQOOCOOQ0(OCQOOCOOQ0:TCriticalSection);
function O0COOCOOQ0(OOCOOCOOQ0:Exception):Exception;
function OQCOOCOOQ0:integer;
{$IFDEF VER16P}
{$IFNDEF MSWINDOWS}
function GetCurrentThreadID:TThreadID;
{$ENDIF}
{$ENDIF}
{$IFNDEF VER16P}
function GetHomePath:string;
{$ENDIF}
{$IFDEF POSIX}
type
TSockAddr=sockaddr;
{$EXTERNALSYM TSockAddr}
{$ENDIF}
{$IFNDEF VER7P}
const
HoursPerDay=24;
MinsPerHour=60;
SecsPerMin=60;
UnixDateDelta=25569;
{$ENDIF}
var
OCCOOCOOQ0:OQCQQ0QOQ0;
var
O00Q0COOQ0,OO0Q0COOQ0,OQ0Q0COOQ0,OC0Q0COOQ0,
O0OQ0COOQ0,OOOQ0COOQ0,OQOQ0COOQ0:TBigInteger;
const
OCOQ0COOQ0=32*1024;
const
O0QQ0COOQ0='SecureBridge';
implementation
uses
StrUtils,Types,Math,
{$IFDEF MSWINDOWS}
{$IFDEF WIN32_64}SysConst,Registry,{$ENDIF}
WinSock;
{$ENDIF}
{$IFDEF POSIX}
Posix.SysTime,Posix.Time,Posix.UTime,
Posix.NetinetIn,Posix.Errno;
{$ENDIF}
{$IFDEF UNIX}
Sockets,baseunix,unix,unixutil,netdb;
{$ENDIF}
resourcestring
SInvalidMask='''%s'' is an invalid mask at (%d)';
const
OOQQ0COOQ0:packed array[0..255]of array[1..2]of Char=
('00','01','02','03','04','05','06','07','08','09','0a','0b','0c','0d','0e','0f',
'10','11','12','13','14','15','16','17','18','19','1a','1b','1c','1d','1e','1f',
'20','21','22','23','24','25','26','27','28','29','2a','2b','2c','2d','2e','2f',
'30','31','32','33','34','35','36','37','38','39','3a','3b','3c','3d','3e','3f',
'40','41','42','43','44','45','46','47','48','49','4a','4b','4c','4d','4e','4f',
'50','51','52','53','54','55','56','57','58','59','5a','5b','5c','5d','5e','5f',
'60','61','62','63','64','65','66','67','68','69','6a','6b','6c','6d','6e','6f',
'70','71','72','73','74','75','76','77','78','79','7a','7b','7c','7d','7e','7f',
'80','81','82','83','84','85','86','87','88','89','8a','8b','8c','8d','8e','8f',
'90','91','92','93','94','95','96','97','98','99','9a','9b','9c','9d','9e','9f',
'a0','a1','a2','a3','a4','a5','a6','a7','a8','a9','aa','ab','ac','ad','ae','af',
'b0','b1','b2','b3','b4','b5','b6','b7','b8','b9','ba','bb','bc','bd','be','bf',
'c0','c1','c2','c3','c4','c5','c6','c7','c8','c9','ca','cb','cc','cd','ce','cf',
'd0','d1','d2','d3','d4','d5','d6','d7','d8','d9','da','db','dc','dd','de','df',
'e0','e1','e2','e3','e4','e5','e6','e7','e8','e9','ea','eb','ec','ed','ee','ef',
'f0','f1','f2','f3','f4','f5','f6','f7','f8','f9','fa','fb','fc','fd','fe','ff');
{$IFDEF VER16P}
{$IFNDEF MSWINDOWS}
function GetCurrentThreadID:TThreadID;
begin
Result:=TThread.CurrentThread.ThreadID;
end;
{$ENDIF}
{$ENDIF}
{$IFNDEF VER16P}
function GetHomePath:string;
begin
Result:=GetEnvironmentVariable('HOME');
end;
{$ENDIF}
function OQCOOCOOQ0:integer;
begin
{$IFDEF MSWINDOWS}
Result:=WSAGetLastError;
{$ELSE}
Result:=errno;
{$ENDIF}
end;
type
TScCharsetSpec=record
Name:string;
CodePage:cardinal;
end;
const
OQQQ0COOQ0:array[0..404]of TScCharsetSpec=(
(Name:'';CodePage:0),
(Name:'US-ASCII';CodePage:20127),
(Name:'ANSI_X3.4-1968';CodePage:20127),
(Name:'iso-ir-6';CodePage:20127),
(Name:'ANSI_X3.4-1986';CodePage:20127),
(Name:'ISO_646.irv:1991';CodePage:20127),
(Name:'ASCII';CodePage:20127),
(Name:'ISO646-US';CodePage:20127),
(Name:'us';CodePage:20127),
(Name:'IBM367';CodePage:20127),
(Name:'cp367';CodePage:20127),
(Name:'csASCII';CodePage:20127),
(Name:'KS_C_5601-1987';CodePage:949),
(Name:'iso-ir-149';CodePage:949),
(Name:'KS_C_5601-1989';CodePage:949),
(Name:'KSC_5601';CodePage:949),
(Name:'korean';CodePage:949),
(Name:'csKSC56011987';CodePage:949),
(Name:'ISO-2022-KR';CodePage:50225),
(Name:'csISO2022KR';CodePage:50225),
(Name:'EUC-KR';CodePage:51949),
(Name:'csEUCKR';CodePage:51949),
(Name:'ISO-2022-JP';CodePage:50220),
(Name:'csISO2022JP';CodePage:50221),
(Name:'GB_2312-80';CodePage:936),
(Name:'iso-ir-58';CodePage:936),
(Name:'chinese';CodePage:936),
(Name:'csISO58GB231280';CodePage:936),
(Name:'ISO-8859-1';CodePage:28591),
(Name:'ISO_8859-1:1987';CodePage:28591),
(Name:'iso-ir-100';CodePage:28591),
(Name:'ISO_8859-1';CodePage:28591),
(Name:'latin1';CodePage:28591),
(Name:'l1';CodePage:28591),
(Name:'IBM819';CodePage:28591),
(Name:'CP819';CodePage:28591),
(Name:'csISOLatin1';CodePage:28591),
(Name:'ISO-8859-2';CodePage:28592),
(Name:'ISO_8859-2:1987';CodePage:28592),
(Name:'iso-ir-101';CodePage:28592),
(Name:'ISO_8859-2';CodePage:28592),
(Name:'latin2';CodePage:28592),
(Name:'l2';CodePage:28592),
(Name:'csISOLatin2';CodePage:28592),
(Name:'ISO-8859-3';CodePage:28593),
(Name:'ISO_8859-3:1988';CodePage:28593),
(Name:'iso-ir-109';CodePage:28593),
(Name:'ISO_8859-3';CodePage:28593),
(Name:'latin3';CodePage:28593),
(Name:'l3';CodePage:28593),
(Name:'csISOLatin3';CodePage:28593),
(Name:'ISO-8859-4';CodePage:28594),
(Name:'ISO_8859-4:1988';CodePage:28594),
(Name:'iso-ir-110';CodePage:28594),
(Name:'ISO_8859-4';CodePage:28594),
(Name:'latin4';CodePage:28594),
(Name:'l4';CodePage:28594),
(Name:'csISOLatin4';CodePage:28594),
(Name:'ISO-8859-6';CodePage:28596),
(Name:'ISO_8859-6:1987';CodePage:708),
(Name:'iso-ir-127';CodePage:708),
(Name:'ISO_8859-6';CodePage:708),
(Name:'ECMA-114';CodePage:708),
(Name:'ASMO-708';CodePage:708),
(Name:'arabic';CodePage:708),
(Name:'csISOLatinArabic';CodePage:708),
(Name:'ISO-8859-7';CodePage:28597),
(Name:'ISO_8859-7:1987';CodePage:28597),
(Name:'iso-ir-126';CodePage:28597),
(Name:'ISO_8859-7';CodePage:28597),
(Name:'ELOT_928';CodePage:28597),
(Name:'ECMA-118';CodePage:28597),
(Name:'greek';CodePage:28597),
(Name:'greek8';CodePage:28597),
(Name:'csISOLatinGreek';CodePage:28597),
(Name:'ISO-8859-8';CodePage:28598),
(Name:'ISO_8859-8:1988';CodePage:28598),
(Name:'iso-ir-138';CodePage:28598),
(Name:'ISO_8859-8';CodePage:28598),
(Name:'hebrew';CodePage:28598),
(Name:'csISOLatinHebrew';CodePage:28598),
(Name:'ISO-8859-8-I';CodePage:38598),
(Name:'ISO_8859-8-I';CodePage:38598),
(Name:'csISO88598I';CodePage:38598),
(Name:'ISO-8859-5';CodePage:28595),
(Name:'ISO_8859-5:1988';CodePage:28595),
(Name:'iso-ir-144';CodePage:28595),
(Name:'ISO_8859-5';CodePage:28595),
(Name:'cyrillic';CodePage:28595),
(Name:'csISOLatinCyrillic';CodePage:28595),
(Name:'ISO-8859-9';CodePage:28599),
(Name:'ISO_8859-9:1989';CodePage:28599),
(Name:'iso-ir-148';CodePage:28599),
(Name:'ISO_8859-9';CodePage:28599),
(Name:'latin5';CodePage:28599),
(Name:'l5';CodePage:28599),
(Name:'csISOLatin5';CodePage:28599),
(Name:'macintosh';CodePage:10000),
(Name:'mac';CodePage:10000),
(Name:'csMacintosh';CodePage:10000),
(Name:'IBM037';CodePage:37),
(Name:'cp037';CodePage:37),
(Name:'ebcdic-cp-us';CodePage:37),
(Name:'ebcdic-cp-ca';CodePage:37),
(Name:'ebcdic-cp-wt';CodePage:37),
(Name:'ebcdic-cp-nl';CodePage:37),
(Name:'csIBM037';CodePage:37),
(Name:'IBM273';CodePage:20273),
(Name:'CP273';CodePage:20273),
(Name:'csIBM273';CodePage:20273),
(Name:'IBM277';CodePage:20277),
(Name:'EBCDIC-CP-DK';CodePage:20277),
(Name:'EBCDIC-CP-NO';CodePage:20277),
(Name:'csIBM277';CodePage:20277),
(Name:'IBM278';CodePage:20278),
(Name:'CP278';CodePage:20278),
(Name:'ebcdic-cp-fi';CodePage:20278),
(Name:'ebcdic-cp-se';CodePage:20278),
(Name:'csIBM278';CodePage:20278),
(Name:'IBM280';CodePage:20280),
(Name:'CP280';CodePage:20280),
(Name:'ebcdic-cp-it';CodePage:20280),
(Name:'csIBM280';CodePage:20280),
(Name:'IBM284';CodePage:20284),
(Name:'CP284';CodePage:20284),
(Name:'ebcdic-cp-es';CodePage:20284),
(Name:'csIBM284';CodePage:20284),
(Name:'IBM285';CodePage:20285),
(Name:'CP285';CodePage:20285),
(Name:'ebcdic-cp-gb';CodePage:20285),
(Name:'csIBM285';CodePage:20285),
(Name:'IBM290';CodePage:20290),
(Name:'cp290';CodePage:20290),
(Name:'EBCDIC-JP-kana';CodePage:20290),
(Name:'csIBM290';CodePage:20290),
(Name:'IBM297';CodePage:20297),
(Name:'cp297';CodePage:20297),
(Name:'ebcdic-cp-fr';CodePage:20297),
(Name:'csIBM297';CodePage:20297),
(Name:'IBM420';CodePage:20420),
(Name:'cp420';CodePage:20420),
(Name:'ebcdic-cp-ar1';CodePage:20420),
(Name:'csIBM420';CodePage:20420),
(Name:'IBM423';CodePage:20423),
(Name:'cp423';CodePage:20423),
(Name:'ebcdic-cp-gr';CodePage:20423),
(Name:'csIBM423';CodePage:20423),
(Name:'IBM424';CodePage:20424),
(Name:'cp424';CodePage:20424),
(Name:'ebcdic-cp-he';CodePage:20424),
(Name:'csIBM424';CodePage:20424),
(Name:'IBM437';CodePage:437),
(Name:'cp437';CodePage:437),
(Name:'437';CodePage:437),
(Name:'csPC8CodePage437';CodePage:437),
(Name:'IBM500';CodePage:500),
(Name:'CP500';CodePage:500),
(Name:'ebcdic-cp-be';CodePage:500),
(Name:'ebcdic-cp-ch';CodePage:500),
(Name:'csIBM500';CodePage:500),
(Name:'IBM775';CodePage:775),
(Name:'cp775';CodePage:775),
(Name:'csPC775Baltic';CodePage:775),
(Name:'IBM850';CodePage:850),
(Name:'cp850';CodePage:850),
(Name:'850';CodePage:850),
(Name:'csPC850Multilingual';CodePage:850),
(Name:'IBM852';CodePage:852),
(Name:'cp852';CodePage:852),
(Name:'852';CodePage:852),
(Name:'csPCp852';CodePage:852),
(Name:'IBM855';CodePage:855),
(Name:'cp855';CodePage:855),
(Name:'855';CodePage:855),
(Name:'csIBM855';CodePage:855),
(Name:'IBM857';CodePage:857),
(Name:'cp857';CodePage:857),
(Name:'857';CodePage:857),
(Name:'csIBM857';CodePage:857),
(Name:'IBM860';CodePage:860),
(Name:'cp860';CodePage:860),
(Name:'860';CodePage:860),
(Name:'csIBM860';CodePage:860),
(Name:'IBM861';CodePage:861),
(Name:'cp861';CodePage:861),
(Name:'861';CodePage:861),
(Name:'cp-is';CodePage:861),
(Name:'csIBM861';CodePage:861),
(Name:'IBM863';CodePage:863),
(Name:'cp863';CodePage:863),
(Name:'863';CodePage:863),
(Name:'csIBM863';CodePage:863),
(Name:'IBM864';CodePage:864),
(Name:'cp864';CodePage:864),
(Name:'csIBM864';CodePage:864),
(Name:'IBM865';CodePage:865),
(Name:'cp865';CodePage:865),
(Name:'865';CodePage:865),
(Name:'csIBM865';CodePage:865),
(Name:'IBM866';CodePage:866),
(Name:'cp866';CodePage:866),
(Name:'866';CodePage:866),
(Name:'csIBM866';CodePage:866),
(Name:'IBM869';CodePage:869),
(Name:'cp869';CodePage:869),
(Name:'869';CodePage:869),
(Name:'cp-gr';CodePage:869),
(Name:'csIBM869';CodePage:869),
(Name:'IBM870';CodePage:870),
(Name:'CP870';CodePage:870),
(Name:'ebcdic-cp-roece';CodePage:870),
(Name:'ebcdic-cp-yu';CodePage:870),
(Name:'csIBM870';CodePage:870),
(Name:'IBM871';CodePage:20871),
(Name:'CP871';CodePage:20871),
(Name:'ebcdic-cp-is';CodePage:20871),
(Name:'csIBM871';CodePage:20871),
(Name:'IBM880';CodePage:20880),
(Name:'cp880';CodePage:20880),
(Name:'EBCDIC-Cyrillic';CodePage:20880),
(Name:'csIBM880';CodePage:20880),
(Name:'IBM905';CodePage:20905),
(Name:'CP905';CodePage:20905),
(Name:'ebcdic-cp-tr';CodePage:20905),
(Name:'csIBM905';CodePage:20905),
(Name:'IBM1026';CodePage:1026),
(Name:'CP1026';CodePage:1026),
(Name:'csIBM1026';CodePage:1026),
(Name:'KOI8-R';CodePage:20866),
(Name:'csKOI8R';CodePage:20866),
(Name:'KOI8-U';CodePage:21866),
(Name:'IBM00858';CodePage:858),
(Name:'CCSID00858';CodePage:858),
(Name:'CP00858';CodePage:858),
(Name:'PC-Multilingual-850+euro';CodePage:858),
(Name:'IBM00924';CodePage:20924),
(Name:'CCSID00924';CodePage:20924),
(Name:'CP00924';CodePage:20924),
(Name:'ebcdic-Latin9--euro';CodePage:20924),
(Name:'IBM01140';CodePage:1140),
(Name:'CCSID01140';CodePage:1140),
(Name:'CP01140';CodePage:1140),
(Name:'ebcdic-us-37+euro';CodePage:1140),
(Name:'IBM01141';CodePage:1141),
(Name:'CCSID01141';CodePage:1141),
(Name:'CP01141';CodePage:1141),
(Name:'ebcdic-de-273+euro';CodePage:1141),
(Name:'IBM01142';CodePage:1142),
(Name:'CCSID01142';CodePage:1142),
(Name:'CP01142';CodePage:1142),
(Name:'ebcdic-dk-277+euro';CodePage:1142),
(Name:'ebcdic-no-277+euro';CodePage:1142),
(Name:'IBM01143';CodePage:1143),
(Name:'CCSID01143';CodePage:1143),
(Name:'CP01143';CodePage:1143),
(Name:'ebcdic-fi-278+euro';CodePage:1143),
(Name:'ebcdic-se-278+euro';CodePage:1143),
(Name:'IBM01144';CodePage:1144),
(Name:'CCSID01144';CodePage:1144),
(Name:'CP01144';CodePage:1144),
(Name:'ebcdic-it-280+euro';CodePage:1144),
(Name:'IBM01145';CodePage:1145),
(Name:'CCSID01145';CodePage:1145),
(Name:'CP01145';CodePage:1145),
(Name:'ebcdic-es-284+euro';CodePage:1145),
(Name:'IBM01146';CodePage:1146),
(Name:'CCSID01146';CodePage:1146),
(Name:'CP01146';CodePage:1146),
(Name:'ebcdic-gb-285+euro';CodePage:1146),
(Name:'IBM01147';CodePage:1147),
(Name:'CCSID01147';CodePage:1147),
(Name:'CP01147';CodePage:1147),
(Name:'ebcdic-fr-297+euro';CodePage:1147),
(Name:'IBM01148';CodePage:1148),
(Name:'CCSID01148';CodePage:1148),
(Name:'CP01148';CodePage:1148),
(Name:'ebcdic-international-500+euro';CodePage:1148),
(Name:'IBM01149';CodePage:1149),
(Name:'CCSID01149';CodePage:1149),
(Name:'CP01149';CodePage:1149),
(Name:'ebcdic-is-871+euro';CodePage:1149),
(Name:'UTF-16BE';CodePage:1201),
(Name:'UTF-16LE';CodePage:1200),
(Name:'UTF-16';CodePage:1200),
(Name:'UTF-32';CodePage:12000),
(Name:'UTF-32BE';CodePage:12001),
(Name:'UTF-32LE';CodePage:12000),
(Name:'UTF-8';CodePage:65001),
(Name:'ISO-8859-13';CodePage:28603),
(Name:'ISO-8859-15';CodePage:28605),
(Name:'ISO_8859-15';CodePage:28605),
(Name:'Latin-9';CodePage:28605),
(Name:'GBK';CodePage:936),
(Name:'CP936';CodePage:936),
(Name:'MS936';CodePage:936),
(Name:'windows-936';CodePage:936),
(Name:'GB18030';CodePage:54936),
(Name:'Shift_JIS';CodePage:932),
(Name:'MS_Kanji';CodePage:932),
(Name:'csShiftJIS';CodePage:932),
(Name:'EUC-JP';CodePage:20932),
(Name:'Extended_UNIX_Code_Packed_Format_for_Japanese';CodePage:20932),
(Name:'csEUCPkdFmtJapanese';CodePage:20932),
(Name:'DOS-862';CodePage:862),
(Name:'windows-874';CodePage:874),
(Name:'cp875';CodePage:875),
(Name:'IBM01047';CodePage:1047),
(Name:'unicodeFFFE';CodePage:1201),
(Name:'Johab';CodePage:1361),
(Name:'x-mac-japanese';CodePage:10001),
(Name:'x-mac-chinesetrad';CodePage:10002),
(Name:'x-mac-korean';CodePage:10003),
(Name:'x-mac-arabic';CodePage:10004),
(Name:'x-mac-hebrew';CodePage:10005),
(Name:'x-mac-greek';CodePage:10006),
(Name:'x-mac-cyrillic';CodePage:10007),
(Name:'x-mac-chinesesimp';CodePage:10008),
(Name:'x-mac-romanian';CodePage:10010),
(Name:'x-mac-ukrainian';CodePage:10017),
(Name:'x-mac-thai';CodePage:10021),
(Name:'x-mac-ce';CodePage:10029),
(Name:'x-mac-icelandic';CodePage:10079),
(Name:'x-mac-turkish';CodePage:10081),
(Name:'x-mac-croatian';CodePage:10082),
(Name:'x-Chinese-CNS';CodePage:20000),
(Name:'x-cp20001';CodePage:20001),
(Name:'x-Chinese-Eten';CodePage:20002),
(Name:'x-cp20003';CodePage:20003),
(Name:'x-cp20004';CodePage:20004),
(Name:'x-cp20005';CodePage:20005),
(Name:'x-IA5';CodePage:20105),
(Name:'x-IA5-German';CodePage:20106),
(Name:'x-IA5-Swedish';CodePage:20107),
(Name:'x-IA5-Norwegian';CodePage:20108),
(Name:'x-cp20261';CodePage:20261),
(Name:'x-cp20269';CodePage:20269),
(Name:'x-EBCDIC-KoreanExtended';CodePage:20833),
(Name:'x-cp20936';CodePage:20936),
(Name:'x-cp20949';CodePage:20949),
(Name:'cp1025';CodePage:21025),
(Name:'x-Europa';CodePage:29001),
(Name:'x-cp50227';CodePage:50227),
(Name:'EUC-CN';CodePage:51936),
(Name:'x-iscii-de';CodePage:57002),
(Name:'x-iscii-be';CodePage:57003),
(Name:'x-iscii-ta';CodePage:57004),
(Name:'x-iscii-te';CodePage:57005),
(Name:'x-iscii-as';CodePage:57006),
(Name:'x-iscii-or';CodePage:57007),
(Name:'x-iscii-ka';CodePage:57008),
(Name:'x-iscii-ma';CodePage:57009),
(Name:'x-iscii-gu';CodePage:57010),
(Name:'x-iscii-pa';CodePage:57011),
(Name:'x-EBCDIC-Arabic';CodePage:20420),
(Name:'x-EBCDIC-CyrillicRussian';CodePage:20880),
(Name:'x-EBCDIC-CyrillicSerbianBulgarian';CodePage:21025),
(Name:'x-EBCDIC-DenmarkNorway';CodePage:20277),
(Name:'x-ebcdic-denmarknorway-euro';CodePage:1142),
(Name:'x-EBCDIC-FinlandSweden';CodePage:20278),
(Name:'x-ebcdic-finlandsweden-euro';CodePage:1143),
(Name:'X-EBCDIC-France';CodePage:1143),
(Name:'x-ebcdic-france-euro';CodePage:1147),
(Name:'x-EBCDIC-Germany';CodePage:20273),
(Name:'x-ebcdic-germany-euro';CodePage:1141),
(Name:'x-EBCDIC-GreekModern';CodePage:875),
(Name:'x-EBCDIC-Greek';CodePage:20423),
(Name:'x-EBCDIC-Hebrew';CodePage:20424),
(Name:'x-EBCDIC-Icelandic';CodePage:20871),
(Name:'x-ebcdic-icelandic-euro';CodePage:1149),
(Name:'x-ebcdic-international-euro';CodePage:1148),
(Name:'x-EBCDIC-Italy';CodePage:20280),
(Name:'x-ebcdic-italy-euro';CodePage:1144),
(Name:'x-EBCDIC-JapaneseAndKana';CodePage:50930),
(Name:'x-EBCDIC-JapaneseAndJapaneseLatin';CodePage:50939),
(Name:'x-EBCDIC-JapaneseAndUSCanada';CodePage:50931),
(Name:'x-EBCDIC-JapaneseKatakana';CodePage:20290),
(Name:'x-EBCDIC-KoreanAndKoreanExtended';CodePage:50933),
(Name:'x-EBCDIC-SimplifiedChinese';CodePage:50935),
(Name:'X-EBCDIC-Spain';CodePage:20284),
(Name:'x-ebcdic-spain-euro';CodePage:1145),
(Name:'x-EBCDIC-Thai';CodePage:20838),
(Name:'x-EBCDIC-TraditionalChinese';CodePage:50937),
(Name:'x-EBCDIC-Turkish';CodePage:20905),
(Name:'x-EBCDIC-UK';CodePage:20285),
(Name:'x-ebcdic-uk-euro';CodePage:1146),
(Name:'x-ebcdic-cp-us-euro';CodePage:1140),
(Name:'UTF-7';CodePage:65000),
(Name:'IBM-Thai';CodePage:20838),
(Name:'csIBMThai';CodePage:20838),
(Name:'GB2312';CodePage:936),
(Name:'csGB2312';CodePage:936),
(Name:'Big5';CodePage:950),
(Name:'csBig5';CodePage:950),
(Name:'HZ-GB-2312';CodePage:52936),
(Name:'windows-1250';CodePage:1250),
(Name:'windows-1251';CodePage:1251),
(Name:'windows-1252';CodePage:1252),
(Name:'windows-1253';CodePage:1253),
(Name:'windows-1254';CodePage:1254),
(Name:'windows-1255';CodePage:1255),
(Name:'windows-1256';CodePage:1256),
(Name:'windows-1257';CodePage:1257),
(Name:'windows-1258';CodePage:1258),
(Name:'DOS-720';CodePage:720),
(Name:'ibm737';CodePage:737)
);
function O00OQ0QOQ0(const OO0OQ0QOQ0:string):Encoding;
var
OQ0OQ0QOQ0:cardinal;
OC0OQ0QOQ0:integer;
begin
Result:=nil;
if OO0OQ0QOQ0<>'' then begin
OQ0OQ0QOQ0:=0;
for OC0OQ0QOQ0:=Low(OQQQ0COOQ0)to High(OQQQ0COOQ0)do begin
if AnsiSameText(OQQQ0COOQ0[OC0OQ0QOQ0].Name,OO0OQ0QOQ0)then begin
OQ0OQ0QOQ0:=OQQQ0COOQ0[OC0OQ0QOQ0].CodePage;
Break;
end;
end;
if OQ0OQ0QOQ0<>0 then
Result:={$IFDEF NEXTGEN}Encoding{$ENDIF}(Encoding.GetEncoding(OQ0OQ0QOQ0));
end;
if not Assigned(Result)then
Result:=Encoding.Default;
end;
function O0COOCOOQ0(OOCOOCOOQ0:Exception):Exception;
begin
Result:=nil;
if OOCOOCOOQ0<>nil then
if OOCOOCOOQ0.ClassName=EScError.ClassName then
Result:=EScError(OOCOOCOOQ0).Clone
else
if OOCOOCOOQ0.ClassName=SocketException.ClassName then
Result:=SocketException.Create(OOCOOCOOQ0.Message,SocketException(OOCOOCOOQ0).ErrorCode)
else
if OOCOOCOOQ0.ClassName=ArgumentException.ClassName then
Result:=ArgumentException.Create(OOCOOCOOQ0.Message)
else
if OOCOOCOOQ0.ClassName=InvalidDataException.ClassName then
Result:=InvalidDataException.Create(OOCOOCOOQ0.Message)
else
if OOCOOCOOQ0.ClassName=InvalidOperationException.ClassName then
Result:=InvalidOperationException.Create(OOCOOCOOQ0.Message)
else
if OOCOOCOOQ0.ClassName=JSONException.ClassName then
Result:=JSONException.Create(OOCOOCOOQ0.Message)
else
if OOCOOCOOQ0.ClassName=OperationCanceledException.ClassName then
Result:=OperationCanceledException.Create(OOCOOCOOQ0.Message)
else
Result:=Exception.Create(OOCOOCOOQ0.Message);
end;
procedure O0QOOCOOQ0(OOQOOCOOQ0:TCriticalSection);
begin
OOQOOCOOQ0.Enter;
end;
procedure OQQOOCOOQ0(OCQOOCOOQ0:TCriticalSection);
begin
OCQOOCOOQ0.Leave;
end;
function O0OOQ0QOQ0(const OOOOQ0QOQ0:string;const OQOOQ0QOQ0:Char):TStringList;
begin
Result:=TStringList.Create;
O0OOQ0QOQ0(Result,OOOOQ0QOQ0,OQOOQ0QOQ0,False);
end;
procedure O0OOQ0QOQ0(OCOOQ0QOQ0:TStrings;const OOOOQ0QOQ0:string;const OQOOQ0QOQ0:Char;O0QOQ0QOQ0:Boolean);
var
OOQOQ0QOQ0,OQQOQ0QOQ0:string;
OCQOQ0QOQ0,O0COQ0QOQ0:integer;
begin
if OCOOQ0QOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OCOOQ0QOQ0.Clear;
if O0QOQ0QOQ0 then begin
OOQOQ0QOQ0:=Trim(OOOOQ0QOQ0);
if OOQOQ0QOQ0='' then
Exit;
end
else
OOQOQ0QOQ0:=OOOOQ0QOQ0;
OCQOQ0QOQ0:=1;
for O0COQ0QOQ0:=1 to Length(OOQOQ0QOQ0)do begin
if OOQOQ0QOQ0[O0COQ0QOQ0]=OQOOQ0QOQ0 then begin
if O0QOQ0QOQ0 then
OQQOQ0QOQ0:=Trim(Copy(OOQOQ0QOQ0,OCQOQ0QOQ0,O0COQ0QOQ0-OCQOQ0QOQ0))
else
OQQOQ0QOQ0:=Copy(OOQOQ0QOQ0,OCQOQ0QOQ0,O0COQ0QOQ0-OCQOQ0QOQ0);
if OQQOQ0QOQ0<>'' then
OCOOQ0QOQ0.Add(OQQOQ0QOQ0);
OCQOQ0QOQ0:=O0COQ0QOQ0+1;
end;
end;
if O0QOQ0QOQ0 then
OQQOQ0QOQ0:=Trim(Copy(OOQOQ0QOQ0,OCQOQ0QOQ0,Length(OOOOQ0QOQ0)))
else
OQQOQ0QOQ0:=Copy(OOQOQ0QOQ0,OCQOQ0QOQ0,Length(OOOOQ0QOQ0));
if OQQOQ0QOQ0<>'' then
OCOOQ0QOQ0.Add(OQQOQ0QOQ0);
end;
function OOCOQ0QOQ0(const OQCOQ0QOQ0:string):string;
var
OCCOQ0QOQ0:integer;
begin
Result:=OQCOQ0QOQ0;
OCCOQ0QOQ0:=Pos(' ',Result);
while OCCOQ0QOQ0>0 do begin
Result:=Copy(Result,1,OCCOQ0QOQ0-1)+Copy(Result,OCCOQ0QOQ0+1,Length(Result));
OCCOQ0QOQ0:=Pos(' ',Result);
end;
end;
function O00QOCOOQ0(const OO0QOCOOQ0:TBytes):string;
var
OQ0QOCOOQ0,OC0QOCOOQ0:integer;
begin
SetLength(Result,Length(OO0QOCOOQ0)*2);
OC0QOCOOQ0:=1;
for OQ0QOCOOQ0:=0 to Length(OO0QOCOOQ0)-1 do begin
Result[OC0QOCOOQ0]:=OOQQ0COOQ0[OO0QOCOOQ0[OQ0QOCOOQ0]][1];
Result[OC0QOCOOQ0+1]:=OOQQ0COOQ0[OO0QOCOOQ0[OQ0QOCOOQ0]][2];
Inc(OC0QOCOOQ0,2);
end;
end;
function O00QOCOOQ0(const OO0QOCOOQ0:TBytes;const O0OQOCOOQ0:string):string;
var
OQ0QOCOOQ0,OC0QOCOOQ0:integer;
begin
if Length(OO0QOCOOQ0)=0 then begin
Result:='';
Exit;
end;
SetLength(Result,Length(OO0QOCOOQ0)*3-1);
Result[1]:=OOQQ0COOQ0[OO0QOCOOQ0[0]][1];
Result[2]:=OOQQ0COOQ0[OO0QOCOOQ0[0]][2];
OC0QOCOOQ0:=3;
for OQ0QOCOOQ0:=1 to Length(OO0QOCOOQ0)-1 do begin
Result[OC0QOCOOQ0]:=O0OQOCOOQ0[1];
Result[OC0QOCOOQ0+1]:=OOQQ0COOQ0[OO0QOCOOQ0[OQ0QOCOOQ0]][1];
Result[OC0QOCOOQ0+2]:=OOQQ0COOQ0[OO0QOCOOQ0[OQ0QOCOOQ0]][2];
Inc(OC0QOCOOQ0,3);
end;
end;
function OOOQOCOOQ0(const OQOQOCOOQ0:string):TBytes;
const
OCOQOCOOQ0:array['0'..'f']of Byte=
(0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,
0,10,11,12,13,14,15,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,10,11,12,13,14,15);
var
O0QQOCOOQ0,OOQQOCOOQ0:integer;
OQQQOCOOQ0:integer;
begin
if(Length(OQOQOCOOQ0)and 1)=1 then begin
OQQQOCOOQ0:=(Length(OQOQOCOOQ0)shr 1)+1;
SetLength(Result,OQQQOCOOQ0);
Result[0]:=OCOQOCOOQ0[OQOQOCOOQ0[1]];
O0QQOCOOQ0:=1;
OOQQOCOOQ0:=2;
end
else begin
OQQQOCOOQ0:=Length(OQOQOCOOQ0)shr 1;
SetLength(Result,OQQQOCOOQ0);
O0QQOCOOQ0:=0;
OOQQOCOOQ0:=1;
end;
while O0QQOCOOQ0<OQQQOCOOQ0 do begin
if not CharInSet(OQOQOCOOQ0[OOQQOCOOQ0],['0'..'f'])or not CharInSet(OQOQOCOOQ0[OOQQOCOOQ0+1],['0'..'f'])then
Break;
Result[O0QQOCOOQ0]:=(OCOQOCOOQ0[OQOQOCOOQ0[OOQQOCOOQ0]]shl 4)+OCOQOCOOQ0[OQOQOCOOQ0[OOQQOCOOQ0+1]];
Inc(O0QQOCOOQ0);
Inc(OOQQOCOOQ0,2);
end;
end;
function OCQQOCOOQ0(const O0CQOCOOQ0:Char):Boolean;
begin
Result:=((O0CQOCOOQ0>='a')and(O0CQOCOOQ0<='z'))or((O0CQOCOOQ0>='A')and(O0CQOCOOQ0<='Z'));
end;
function OOCQOCOOQ0(const OQCQOCOOQ0:string):string;
begin
Result:=OQCQOCOOQ0;
if Result='' then
Exit;
if Result[1]<>'<' then
Result:='<'+Result;
if Result[Length(Result)]<>'>' then
Result:=Result+'>';
end;
function OCCQOCOOQ0(const O00COCOOQ0:string):string;
var
OO0COCOOQ0:integer;
begin
Result:=O00COCOOQ0;
if(Result='')or(Result[1]<>'"')then
Exit;
for OO0COCOOQ0:=2 to Length(Result)do begin
if Result[OO0COCOOQ0]='"' then begin
Result:=Copy(O00COCOOQ0,2,OO0COCOOQ0-2);
Exit;
end;
end;
System.Delete(Result,1,1);
end;
function OQ0COCOOQ0(const OC0COCOOQ0,O0OCOCOOQ0:string;OOOCOCOOQ0:integer=1):integer;
var
OQOCOCOOQ0,OCOCOCOOQ0:integer;
O0QCOCOOQ0,OOQCOCOOQ0:integer;
begin
Result:=0;
if OC0COCOOQ0='' then
Exit;
OCOCOCOOQ0:=Max(Length(O0OCOCOOQ0)-OOOCOCOOQ0+1,0);
if OCOCOCOOQ0=0 then
Exit;
OQOCOCOOQ0:=Length(OC0COCOOQ0);
for O0QCOCOOQ0:=0 to OCOCOCOOQ0-1 do begin
for OOQCOCOOQ0:=1 to OQOCOCOOQ0 do begin
if O0OCOCOOQ0[OOOCOCOOQ0]=OC0COCOOQ0[OOQCOCOOQ0]then begin
Result:=OOOCOCOOQ0;
Exit;
end;
end;
Inc(OOOCOCOOQ0);
end;
end;
function OQQCOCOOQ0(const OCQCOCOOQ0,O0CCOCOOQ0:string;OOCCOCOOQ0:integer=1):integer;
var
OQCCOCOOQ0,OCCCOCOOQ0:integer;
O000OCOOQ0,OO00OCOOQ0:integer;
OQ00OCOOQ0:boolean;
begin
Result:=0;
OCCCOCOOQ0:=Max(Length(O0CCOCOOQ0)-OOCCOCOOQ0+1,0);
if OCCCOCOOQ0=0 then
Exit;
if OCQCOCOOQ0='' then begin
Result:=OOCCOCOOQ0;
Exit;
end;
OQCCOCOOQ0:=Length(OCQCOCOOQ0);
for O000OCOOQ0:=0 to OCCCOCOOQ0-1 do begin
OQ00OCOOQ0:=False;
for OO00OCOOQ0:=1 to OQCCOCOOQ0 do begin
if O0CCOCOOQ0[OOCCOCOOQ0]=OCQCOCOOQ0[OO00OCOOQ0]then begin
OQ00OCOOQ0:=True;
Break;
end;
end;
if not OQ00OCOOQ0 then begin
Result:=OOCCOCOOQ0;
Exit;
end;
Inc(OOCCOCOOQ0);
end;
end;
function OQC0OCOOQ0:TDateTime;
{$IFDEF MSWINDOWS}
var
OCC0OCOOQ0:TTimeZoneInformation;
O00OOCOOQ0:integer;
OO0OOCOOQ0:integer;
{$ENDIF}
{$IFDEF POSIX}
var
O0QQQ0OCQ0:time_t;
OOQQQ0OCQ0:timeval;
OQQQQ0OCQ0:tm;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
O00OOCOOQ0:=GetTimeZoneInformation(OCC0OCOOQ0);
OO0OOCOOQ0:=OCC0OCOOQ0.Bias+(O00OOCOOQ0-1)*OCC0OCOOQ0.DaylightBias;
Result:=EncodeTime(Abs(OO0OOCOOQ0)div 60,Abs(OO0OOCOOQ0)mod 60,0,0);
if OO0OOCOOQ0>0 then
Result:=0.0-Result;
{$ENDIF}
{$IFDEF POSIX}
gettimeofday(OOQQQ0OCQ0,nil);
O0QQQ0OCQ0:=OOQQQ0OCQ0.tv_sec;
localtime_r(O0QQQ0OCQ0,OQQQQ0OCQ0);
Result:=OQQQQ0OCQ0.tm_gmtoff/(60*60*24);
{$ENDIF}
{$IFDEF UNIX}
Result:=Tzseconds/(60*60*24);
{$ENDIF}
end;
function OCO0OCOOQ0:cardinal;
var
O0Q0OCOOQ0:TDateTime;
OOQ0OCOOQ0,OQQ0OCOOQ0,OCQ0OCOOQ0,O0C0OCOOQ0:Word;
OOC0OCOOQ0:integer;
begin
O0Q0OCOOQ0:=Now-UnixDateDelta;
OOC0OCOOQ0:=integer(Trunc(O0Q0OCOOQ0));
DecodeTime(O0Q0OCOOQ0,OOQ0OCOOQ0,OQQ0OCOOQ0,OCQ0OCOOQ0,O0C0OCOOQ0);
Result:=OCQ0OCOOQ0+OQQ0OCOOQ0*SecsPerMin+OOQ0OCOOQ0*MinsPerHour*SecsPerMin+OOC0OCOOQ0*SecsPerDay;
end;
function OQ0OOCOOQ0(OC0OOCOOQ0:string=''):string;
const
O0OOOCOOQ0='SBridge';
{$IFNDEF FPC}
var
OOOOOCOOQ0:string;
OQOOOCOOQ0:UInt64;
{$IFDEF MSWINDOWS}
OCOOOCOOQ0:integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF FPC}
Result:=GetTempFileName(OC0OOCOOQ0,O0OOOCOOQ0);
{$ELSE}
{$IFDEF MSWINDOWS}
if OC0OOCOOQ0='' then begin
SetLength(OC0OOCOOQ0,MAX_PATH);
OCOOOCOOQ0:=GetTempPath(MAX_PATH,PChar(OC0OOCOOQ0));
if OCOOOCOOQ0>0 then begin
SetLength(OC0OOCOOQ0,OCOOOCOOQ0);
OC0OOCOOQ0:=IncludeTrailingPathDelimiter(OC0OOCOOQ0);
end
else
Result:='';
end;
{$ELSE}
if OC0OOCOOQ0='' then
OC0OOCOOQ0:=System.IOUtils.TPath.GetTempPath;
{$ENDIF}
if(OC0OOCOOQ0<>'')and DirectoryExists(OC0OOCOOQ0)then
OOOOOCOOQ0:=IncludeTrailingPathDelimiter(OC0OOCOOQ0)+O0OOOCOOQ0
else
OOOOOCOOQ0:=O0OOOCOOQ0;
OQOOOCOOQ0:=UInt64({$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF});
repeat
Result:=OOOOOCOOQ0+IntToHex(OQOOOCOOQ0,8)+{$IFNDEF UNIX}'.tmp'{$ENDIF};
if not FileExists(Result)then
Break;
Inc(OQOOOCOOQ0);
until False;
{$ENDIF}
end;
constructor EScError.Create(const OO0CO0QOQ0:string);
begin
inherited Create(OO0CO0QOQ0);
FErrorCode:=seInternalError;
end;
constructor EScError.Create(OQ0CO0QOQ0:TScErrorCode);
begin
inherited Create(ScErrorMessages[OQ0CO0QOQ0]);
FErrorCode:=OQ0CO0QOQ0;
end;
constructor EScError.CreateFmt(const OC0CO0QOQ0:string;const O0OCO0QOQ0:array of const;OOOCO0QOQ0:TScErrorCode);
begin
inherited CreateFmt(OC0CO0QOQ0,O0OCO0QOQ0);
FErrorCode:=OOOCO0QOQ0;
end;
function EScError.Clone:EScError;
begin
Result:=EScError.Create(Self.Message);
Result.FErrorCode:=Self.FErrorCode;
end;
function OQOCO0QOQ0.OCOCO0QOQ0:string;
begin
Result:='';
end;
procedure OQOCO0QOQ0.O0QCO0QOQ0(const OOQCO0QOQ0:string);
begin
end;
constructor O0CCO0QOQ0.Create(OQQ0O0QOQ0:TPersistent;OCQ0O0QOQ0:OCQCO0QOQ0);
begin
inherited Create(OQQ0O0QOQ0,OCQ0O0QOQ0);
end;
procedure O0CCO0QOQ0.Update(OQO0O0QOQ0:TCollectionItem);
begin
if Assigned(O0C0O0QOQ0)then O0C0O0QOQ0(Self);
end;
procedure O0CCO0QOQ0.Assign(O0Q0O0QOQ0:TPersistent);
begin
inherited Assign(O0Q0O0QOQ0);
end;
function O0CCO0QOQ0.OQCCO0QOQ0:string;
var
OCCCO0QOQ0:StringBuilder;
O000O0QOQ0:Integer;
begin
OCCCO0QOQ0:=StringBuilder.Create;
try
for O000O0QOQ0:=0 to Count-1 do begin
if O000O0QOQ0>0 then
OCCCO0QOQ0.Append(',');
OCCCO0QOQ0.Append(OQOCO0QOQ0(Items[O000O0QOQ0]).OQQCO0QOQ0);
end;
Result:=OCCCO0QOQ0.ToString;
finally
OCCCO0QOQ0.Free;
end;
end;
procedure O0CCO0QOQ0.OO00O0QOQ0(const OOQCO0QOQ0:string);
var
OQ00O0QOQ0,OC00O0QOQ0:Integer;
O0O0O0QOQ0:Integer;
begin
if OOC0O0QOQ0<>OOQCO0QOQ0 then begin
Clear;
O0O0O0QOQ0:=Length(TrimRight(OOQCO0QOQ0));
if O0O0O0QOQ0=0 then
Exit;
OC00O0QOQ0:=0;
while OC00O0QOQ0<O0O0O0QOQ0 do begin
OQ00O0QOQ0:=OC00O0QOQ0+1;
OC00O0QOQ0:=PosEx(',',OOQCO0QOQ0,OQ00O0QOQ0);
if OC00O0QOQ0=0 then
OC00O0QOQ0:=O0O0O0QOQ0+1;
OQOCO0QOQ0(Add).OQQCO0QOQ0:=Trim(Copy(OOQCO0QOQ0,OQ00O0QOQ0,OC00O0QOQ0-OQ00O0QOQ0));
end;
end;
end;
constructor OQ0OO0QOQ0.Create(OCOOO0QOQ0:TStream;O0QOO0QOQ0,OOQOO0QOQ0:Int64);
begin
inherited Create;
OQQOO0QOQ0(OCOOO0QOQ0,O0QOO0QOQ0,OOQOO0QOQ0);
end;
procedure OQ0OO0QOQ0.OQQOO0QOQ0(OCQOO0QOQ0:TStream;O0COO0QOQ0,OOCOO0QOQ0:Int64);
begin
OC0OO0QOQ0:=OCQOO0QOQ0;
O0OOO0QOQ0:=O0COO0QOQ0;
OOOOO0QOQ0:=OOCOO0QOQ0;
end;
procedure OQ0OO0QOQ0.OQCOO0QOQ0(O0Q0O0QOQ0:OQ0OO0QOQ0);
begin
if O0Q0O0QOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OC0OO0QOQ0:=O0Q0O0QOQ0.OC0OO0QOQ0;
O0OOO0QOQ0:=O0Q0O0QOQ0.O0OOO0QOQ0;
OOOOO0QOQ0:=O0Q0O0QOQ0.OOOOO0QOQ0;
end;
procedure OQ0OO0QOQ0.OQCOO0QOQ0(const O0Q0O0QOQ0:OQC0O0QOQ0);
begin
OC0OO0QOQ0:=O0Q0O0QOQ0.OCC0O0QOQ0;
O0OOO0QOQ0:=O0Q0O0QOQ0.O00OO0QOQ0;
OOOOO0QOQ0:=O0Q0O0QOQ0.OO0OO0QOQ0;
end;
class function O0OOC0QOQ0.OOOOC0QOQ0(OQOOC0QOQ0:TStream;out OCOOC0QOQ0:string;O0QOC0QOQ0:integer=-1;
OOQOC0QOQ0:Encoding=nil):boolean;
const
OQQOC0QOQ0=4096;
var
OCQOC0QOQ0,O0COC0QOQ0,OOCOC0QOQ0:integer;
OQCOC0QOQ0:TBytes;
OCCOC0QOQ0,O00QQ0QOQ0:NativeInt;
OO0QQ0QOQ0:boolean;
OQ0QQ0QOQ0:integer;
begin
Assert(OQOOC0QOQ0<>nil);
if O0QOC0QOQ0<0 then
O0QOC0QOQ0:=MaxInt;
OCCOC0QOQ0:=OQOOC0QOQ0.Position;
O00QQ0QOQ0:=OQOOC0QOQ0.Size;
if OCCOC0QOQ0>=O00QQ0QOQ0 then begin
OCOOC0QOQ0:='';
Result:=False;
Exit;
end;
SetLength(OQCOC0QOQ0,0);
OO0QQ0QOQ0:=False;
O0COC0QOQ0:=0;
OCQOC0QOQ0:=0;
while OCCOC0QOQ0<O00QQ0QOQ0 do begin
if(O0COC0QOQ0+OQQOC0QOQ0)<Length(OQCOC0QOQ0)then
SetLength(OQCOC0QOQ0,O0COC0QOQ0+OQQOC0QOQ0);
OOCOC0QOQ0:=OQOOC0QOQ0.Read(OQCOC0QOQ0[O0COC0QOQ0],Min(OQQOC0QOQ0,O0QOC0QOQ0));
if OOCOC0QOQ0<=0 then
Break;
OCQOC0QOQ0:=O0COC0QOQ0+OOCOC0QOQ0;
OQ0QQ0QOQ0:=0;
while OQ0QQ0QOQ0<OOCOC0QOQ0 do begin
case OQCOC0QOQ0[OQ0QQ0QOQ0+O0COC0QOQ0]of
10:begin
OO0QQ0QOQ0:=True;
OCQOC0QOQ0:=OQ0QQ0QOQ0+O0COC0QOQ0;
OOCOC0QOQ0:=OQ0QQ0QOQ0+1;
Break;
end;
13:begin
OO0QQ0QOQ0:=True;
OCQOC0QOQ0:=OQ0QQ0QOQ0+O0COC0QOQ0;
Inc(OQ0QQ0QOQ0);
if(OQ0QQ0QOQ0<OOCOC0QOQ0)and(OQCOC0QOQ0[OQ0QQ0QOQ0+O0COC0QOQ0]=10)then
OOCOC0QOQ0:=OQ0QQ0QOQ0+1
else
OOCOC0QOQ0:=OQ0QQ0QOQ0;
Break;
end;
end;
Inc(OQ0QQ0QOQ0);
end;
Inc(OCCOC0QOQ0,OOCOC0QOQ0);
Inc(O0COC0QOQ0,OOCOC0QOQ0);
Dec(O0QOC0QOQ0,OOCOC0QOQ0);
if OO0QQ0QOQ0 then
Break;
end;
OQOOC0QOQ0.Position:=OCCOC0QOQ0;
if OOQOC0QOQ0<>nil then
OCOOC0QOQ0:=OOQOC0QOQ0.GetString(OQCOC0QOQ0,0,OCQOC0QOQ0)
else
OCOOC0QOQ0:=Encoding.Default.GetString(OQCOC0QOQ0,0,OCQOC0QOQ0);
Result:=True;
end;
class procedure O0OOC0QOQ0.OC0QQ0QOQ0(O0OQQ0QOQ0:TStream;const OOOQQ0QOQ0:string);
var
OQOQQ0QOQ0:Byte;
begin
if OOOQQ0QOQ0<>'' then
O0OQQ0QOQ0.WriteBuffer(Encoding.Default.GetBytes(OOOQQ0QOQ0)[0],Length(OOOQQ0QOQ0));
OQOQQ0QOQ0:=$0A;
O0OQQ0QOQ0.WriteBuffer(OQOQQ0QOQ0,1);
end;
procedure OQ0Q00QOQ0.OC0Q00QOQ0(O0OQ00QOQ0:OQ0Q00QOQ0);
var
OOOQ00QOQ0:string;
begin
if O0OQ00QOQ0<>nil then
OOOQ00QOQ0:=O0OQ00QOQ0.ClassName
else
OOOQ00QOQ0:='nil';
raise EScError.CreateFmt(SAssignError,[OOOQ00QOQ0,ClassName],seAssignError);
end;
constructor OOQQ00QOQ0.Create;
begin
inherited;
OQQQ00QOQ0:=TCRObjectList.Create;
end;
destructor OOQQ00QOQ0.Destroy;
begin
OQQQ00QOQ0.Free;
inherited;
end;
procedure OOQQ00QOQ0.OQOC00QOQ0(O0Q0O0QOQ0:OOQQ00QOQ0);
var
OCOC00QOQ0:OQ0Q00QOQ0;
O0QC00QOQ0:integer;
begin
if O0Q0O0QOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
if Self=O0Q0O0QOQ0 then
Exit;
OQQQ00QOQ0.Clear;
for O0QC00QOQ0:=0 to O0Q0O0QOQ0.OQQQ00QOQ0.Count-1 do begin
if O0Q0O0QOQ0.OQQQ00QOQ0[O0QC00QOQ0]<>nil then begin
OCOC00QOQ0:=OQ0Q00QOQ0(O0Q0O0QOQ0.OQQQ00QOQ0[O0QC00QOQ0]).OQOQ00QOQ0;
OQQQ00QOQ0.Add(OCOC00QOQ0);
end;
end;
end;
function OOQQ00QOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OQ0Q00QOQ0;
end;
procedure OOQQ00QOQ0.OO0C00QOQ0(OQ0C00QOQ0:OQ0Q00QOQ0);
begin
if not(OQ0C00QOQ0 is OC0C00QOQ0)then
raise EScError.CreateFmt(SInvalidObjectClass,[OC0C00QOQ0.ClassName,OQ0C00QOQ0.ClassName],seInvalidObjectClass);
end;
function OOQQ00QOQ0.OOQC00QOQ0(OQQC00QOQ0:OQ0Q00QOQ0):integer;
begin
OO0C00QOQ0(OQQC00QOQ0);
Result:=OQQQ00QOQ0.Add(OQQC00QOQ0);
end;
procedure OOQQ00QOQ0.OCQC00QOQ0;
begin
OQQQ00QOQ0.Clear;
end;
procedure OOQQ00QOQ0.O0CC00QOQ0(OOCC00QOQ0:integer);
begin
OQQQ00QOQ0.Delete(OOCC00QOQ0);
end;
function OOQQ00QOQ0.OQCC00QOQ0(OCCC00QOQ0:OQ0Q00QOQ0):integer;
begin
Result:=OQQQ00QOQ0.IndexOf(OCCC00QOQ0);
end;
procedure OOQQ00QOQ0.O00000QOQ0(OO0000QOQ0:integer;OQ0000QOQ0:OQ0Q00QOQ0);
begin
OO0C00QOQ0(OQ0000QOQ0);
OQQQ00QOQ0.Insert(OO0000QOQ0,OQ0000QOQ0);
end;
function OOQQ00QOQ0.OC0000QOQ0(O0O000QOQ0:OQ0Q00QOQ0):integer;
begin
Result:=OQQQ00QOQ0.Remove(O0O000QOQ0);
end;
function OOQQ00QOQ0.OCQQ00QOQ0:integer;
begin
Result:=OQQQ00QOQ0.Count;
end;
function OOQQ00QOQ0.O0CQ00QOQ0(OOCQ00QOQ0:integer):OQ0Q00QOQ0;
begin
Result:=OQ0Q00QOQ0(OQQQ00QOQ0.Items[OOCQ00QOQ0]);
end;
procedure OOQQ00QOQ0.OQCQ00QOQ0(OCCQ00QOQ0:integer;O00C00QOQ0:OQ0Q00QOQ0);
begin
OO0C00QOQ0(O00C00QOQ0);
OQQQ00QOQ0.Items[OCCQ00QOQ0]:=O00C00QOQ0;
end;
constructor OO00C0QOQ0.Create;
begin
inherited;
{$IFDEF HAVE_COMPRESS_INTERNAL}
SetLength(OOOO00OCQ0,OCOQ0COOQ0);
SetLength(OQOO00OCQ0,OCOQ0COOQ0+1024);
{$ENDIF}
end;
destructor OO00C0QOQ0.Destroy;
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
if OC0O00OCQ0 then
deflateEnd(OO0O00OCQ0);
if O0OO00OCQ0 then
inflateEnd(OQ0O00OCQ0);
{$ENDIF}
inherited;
end;
{$IFDEF HAVE_COMPRESS_INTERNAL}
procedure OO00C0QOQ0.OCOO00OCQ0;
begin
FillChar(OO0O00OCQ0,sizeof(OO0O00OCQ0),0);
OO0O00OCQ0.zalloc:=zlibAllocMem;
OO0O00OCQ0.zfree:=zlibFreeMem;
OOQO00OCQ0(deflateInit_(OO0O00OCQ0,Z_DEFAULT_COMPRESSION,ZLIB_VERSION,sizeof(OO0O00OCQ0)));
OC0O00OCQ0:=True;
end;
procedure OO00C0QOQ0.O0QO00OCQ0;
begin
FillChar(OQ0O00OCQ0,sizeof(OQ0O00OCQ0),0);
OQ0O00OCQ0.zalloc:=zlibAllocMem;
OQ0O00OCQ0.zfree:=zlibFreeMem;
OCQO00OCQ0(inflateInit_(OQ0O00OCQ0,ZLIB_VERSION,sizeof(OQ0O00OCQ0)));
O0OO00OCQ0:=True;
end;
function OO00C0QOQ0.OOQO00OCQ0(OQQO00OCQ0:Integer):Integer;
begin
Result:=OQQO00OCQ0;
if OQQO00OCQ0<0 then
raise {$IFDEF VER12P}EZCompressionError{$ELSE}ECompressionError{$ENDIF}.Create(sError);
end;
function OO00C0QOQ0.OCQO00OCQ0(O0CO00OCQ0:Integer):Integer;
begin
Result:=O0CO00OCQ0;
if O0CO00OCQ0<0 then
raise {$IFDEF VER12P}EZDecompressionError{$ELSE}EDecompressionError{$ENDIF}.Create(sError);
end;
{$ENDIF}
procedure OO00C0QOQ0.O0CQC0QOQ0(const OOCQC0QOQ0:TValueArr;OQCQC0QOQ0,OCCQC0QOQ0:cardinal;
const O00CC0QOQ0:TValueArr;OO0CC0QOQ0:cardinal;var OQ0CC0QOQ0:cardinal;OC0CC0QOQ0:boolean=True);
{$IFDEF HAVE_COMPRESS_INTERNAL}
var
OOCO00OCQ0:integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
if not OC0O00OCQ0 then
OCOO00OCQ0;
OO0O00OCQ0.next_in:=PtrOffset(OOCQC0QOQ0,OQCQC0QOQ0);
OO0O00OCQ0.avail_in:=OCCQC0QOQ0;
OO0O00OCQ0.next_out:=PtrOffset(O00CC0QOQ0,OO0CC0QOQ0);
OO0O00OCQ0.avail_out:=OQ0CC0QOQ0;
OO0O00OCQ0.total_out:=0;
try
if OC0CC0QOQ0 then
OOCO00OCQ0:=OOQO00OCQ0(deflate(OO0O00OCQ0,Z_PARTIAL_FLUSH))
else
OOCO00OCQ0:=OOQO00OCQ0(deflate(OO0O00OCQ0,Z_NO_FLUSH));
if((OOCO00OCQ0<>Z_STREAM_END)and(OOCO00OCQ0<>Z_OK))or(OO0O00OCQ0.avail_in>0)then
raise EZlibError.CreateRes(@sTargetBufferTooSmall);
OQ0CC0QOQ0:=OO0O00OCQ0.total_out;
except
OOQO00OCQ0(deflateEnd(OO0O00OCQ0));
OCOO00OCQ0;
raise;
end;
{$ELSE}
OQ0CC0QOQ0:=0;
{$ENDIF}
end;
function OO00C0QOQ0.O0OCC0QOQ0(const OOOCC0QOQ0:TValueArr;var OQOCC0QOQ0,OCOCC0QOQ0:cardinal;
const O0QCC0QOQ0:TValueArr;OOQCC0QOQ0:cardinal;var OQQCC0QOQ0:cardinal):boolean;
{$IFDEF HAVE_COMPRESS_INTERNAL}
var
OQCO00OCQ0:integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
if not O0OO00OCQ0 then
O0QO00OCQ0;
OQ0O00OCQ0.next_in:=PtrOffset(OOOCC0QOQ0,OQOCC0QOQ0);
OQ0O00OCQ0.avail_in:=OCOCC0QOQ0;
OQ0O00OCQ0.next_out:=PtrOffset(O0QCC0QOQ0,OOQCC0QOQ0);
OQ0O00OCQ0.avail_out:=OQQCC0QOQ0;
OQ0O00OCQ0.total_out:=0;
try
OQCO00OCQ0:=OCQO00OCQ0(inflate(OQ0O00OCQ0,Z_PARTIAL_FLUSH));
if((OQCO00OCQ0<>Z_STREAM_END)and(OQCO00OCQ0<>Z_OK))then
raise EZlibError.CreateRes(@sTargetBufferTooSmall);
OQOCC0QOQ0:=OQOCC0QOQ0+OCOCC0QOQ0-cardinal(OQ0O00OCQ0.avail_in);
OCOCC0QOQ0:=OQ0O00OCQ0.avail_in;
OQQCC0QOQ0:=OQ0O00OCQ0.total_out;
Result:=OQ0O00OCQ0.avail_in=0;
except
OCQO00OCQ0(inflateEnd(OQ0O00OCQ0));
O0QO00OCQ0;
raise;
end;
{$ELSE}
Result:=True;
OQQCC0QOQ0:=0;
{$ENDIF}
end;
procedure OO00C0QOQ0.OCQCC0QOQ0(O0CCC0QOQ0:TStream;OOCCC0QOQ0:OCO000QOQ0);
{$IFDEF HAVE_COMPRESS_INTERNAL}
var
OCCO00OCQ0:TZStreamRec;
O00QC0OCQ0:integer;
OO0QC0OCQ0,OQ0QC0OCQ0:integer;
OC0QC0OCQ0:integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
FillChar(OCCO00OCQ0,sizeof(OCCO00OCQ0),0);
OCCO00OCQ0.zalloc:=zlibAllocMem;
OCCO00OCQ0.zfree:=zlibFreeMem;
OOQO00OCQ0(deflateInit_(OCCO00OCQ0,Z_DEFAULT_COMPRESSION,ZLIB_VERSION,sizeof(OCCO00OCQ0)));
OO0QC0OCQ0:=0;
O00QC0OCQ0:=O0CCC0QOQ0.Read(OOOO00OCQ0,Length(OOOO00OCQ0));
while O00QC0OCQ0>0 do begin
OCCO00OCQ0.next_in:=@OOOO00OCQ0[0];
OCCO00OCQ0.avail_in:=O00QC0OCQ0;
repeat
OCCO00OCQ0.next_out:=@OQOO00OCQ0[OO0QC0OCQ0];
OCCO00OCQ0.avail_out:=Length(OQOO00OCQ0)-OO0QC0OCQ0;
OOQO00OCQ0(deflate(OCCO00OCQ0,Z_NO_FLUSH));
OQ0QC0OCQ0:=Length(OQOO00OCQ0)-integer(OCCO00OCQ0.avail_out);
if OQ0QC0OCQ0>=1024 then begin
OOCCC0QOQ0(O0CCC0QOQ0,OQOO00OCQ0,OQ0QC0OCQ0);
OO0QC0OCQ0:=0;
end
else
OO0QC0OCQ0:=OQ0QC0OCQ0;
until(OCCO00OCQ0.avail_in=0)and(OCCO00OCQ0.avail_out>0);
O00QC0OCQ0:=O0CCC0QOQ0.Read(OOOO00OCQ0,Length(OOOO00OCQ0));
end;
repeat
OCCO00OCQ0.next_out:=@OQOO00OCQ0[OO0QC0OCQ0];
OCCO00OCQ0.avail_out:=Length(OQOO00OCQ0)-OO0QC0OCQ0;
OC0QC0OCQ0:=OOQO00OCQ0(deflate(OCCO00OCQ0,Z_FINISH));
OQ0QC0OCQ0:=Length(OQOO00OCQ0)-integer(OCCO00OCQ0.avail_out);
if OQ0QC0OCQ0>0 then begin
OOCCC0QOQ0(O0CCC0QOQ0,OQOO00OCQ0,OQ0QC0OCQ0);
OO0QC0OCQ0:=0;
end;
until(OC0QC0OCQ0=Z_STREAM_END)and(OCCO00OCQ0.avail_out>0);
OOQO00OCQ0(deflateEnd(OCCO00OCQ0));
{$ELSE}
raise Exception.Create('Operation is not implemented');
{$ENDIF}
end;
procedure OO00C0QOQ0.OQCCC0QOQ0(OCCCC0QOQ0:TStream;O000C0QOQ0:OCQ000QOQ0);
{$IFDEF HAVE_COMPRESS_INTERNAL}
var
O0OQC0OCQ0:TZStreamRec;
OOOQC0OCQ0,OQOQC0OCQ0:integer;
OCOQC0OCQ0:integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
FillChar(O0OQC0OCQ0,sizeof(O0OQC0OCQ0),0);
O0OQC0OCQ0.zalloc:=zlibAllocMem;
O0OQC0OCQ0.zfree:=zlibFreeMem;
OCQO00OCQ0(inflateInit_(O0OQC0OCQ0,ZLIB_VERSION,sizeof(O0OQC0OCQ0)));
OOOQC0OCQ0:=O000C0QOQ0(OCCCC0QOQ0,OOOO00OCQ0,Length(OOOO00OCQ0));
while OOOQC0OCQ0>0 do begin
O0OQC0OCQ0.next_in:=@OOOO00OCQ0[0];
O0OQC0OCQ0.avail_in:=OOOQC0OCQ0;
repeat
O0OQC0OCQ0.next_out:=@OQOO00OCQ0[0];
O0OQC0OCQ0.avail_out:=Length(OQOO00OCQ0);
OCQO00OCQ0(inflate(O0OQC0OCQ0,Z_NO_FLUSH));
OQOQC0OCQ0:=Length(OQOO00OCQ0)-integer(O0OQC0OCQ0.avail_out);
if OQOQC0OCQ0<>0 then
OCCCC0QOQ0.Write(OQOO00OCQ0,OQOQC0OCQ0);
until(O0OQC0OCQ0.avail_in=0)and(O0OQC0OCQ0.avail_out>0);
OOOQC0OCQ0:=O000C0QOQ0(OCCCC0QOQ0,OOOO00OCQ0,Length(OOOO00OCQ0));
end;
repeat
O0OQC0OCQ0.next_out:=@OQOO00OCQ0[0];
O0OQC0OCQ0.avail_out:=Length(OQOO00OCQ0);
OCOQC0OCQ0:=inflate(O0OQC0OCQ0,Z_FINISH);
if OCOQC0OCQ0<>Z_BUF_ERROR then
OCOQC0OCQ0:=OCQO00OCQ0(OCOQC0OCQ0);
OQOQC0OCQ0:=Length(OQOO00OCQ0)-integer(O0OQC0OCQ0.avail_out);
if OQOQC0OCQ0<>0 then
OCCCC0QOQ0.Write(OQOO00OCQ0,OQOQC0OCQ0);
until((OCOQC0OCQ0=Z_STREAM_END)and(O0OQC0OCQ0.avail_out>0))or(OCOQC0OCQ0=Z_BUF_ERROR);
OCQO00OCQ0(inflateEnd(O0OQC0OCQ0));
{$ELSE}
raise Exception.Create('Operation is not implemented');
{$ENDIF}
end;
constructor O0O0C0QOQ0.Create;
const
OCQ0C0QOQ0=1;
begin
inherited;
OOO0C0QOQ0:=TCriticalSection.Create;
OQO0C0QOQ0:=CreateEvent;
OCO0C0QOQ0:=OCQ0C0QOQ0;
O0Q0C0QOQ0:=0;
OOQ0C0QOQ0:={$IFDEF DARWIN}nil{$ELSE}0{$ENDIF};
end;
destructor O0O0C0QOQ0.Destroy;
begin
OOO0C0QOQ0.Free;
OQO0C0QOQ0.Free;
inherited;
end;
function O0O0C0QOQ0.OOC0C0QOQ0(OQC0C0QOQ0:cardinal=INFINITE):boolean;
var
OCC0C0QOQ0,O00OC0QOQ0:{$IFDEF FPC}UInt64{$ELSE}cardinal{$ENDIF};
OO0OC0QOQ0:TThreadID;
begin
OOO0C0QOQ0.Enter;
try
OO0OC0QOQ0:=GetCurrentThreadId;
if OOQ0C0QOQ0=OO0OC0QOQ0 then begin
Inc(O0Q0C0QOQ0);
Result:=True;
Exit;
end;
Result:=OCO0C0QOQ0>0;
if Result then begin
OOQ0C0QOQ0:=OO0OC0QOQ0;
Inc(O0Q0C0QOQ0);
Dec(OCO0C0QOQ0);
Exit;
end;
OQO0C0QOQ0.ResetEvent;
finally
OOO0C0QOQ0.Leave;
end;
while OQC0C0QOQ0>0 do begin
O00OC0QOQ0:={$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF};
OQO0C0QOQ0.WaitFor(OQC0C0QOQ0);
OOO0C0QOQ0.Enter;
try
Result:=OCO0C0QOQ0>0;
if Result then begin
OOQ0C0QOQ0:=OO0OC0QOQ0;
Inc(O0Q0C0QOQ0);
Dec(OCO0C0QOQ0);
Exit;
end;
OQO0C0QOQ0.ResetEvent;
finally
OOO0C0QOQ0.Leave;
end;
OCC0C0QOQ0:=GetTickInterval(O00OC0QOQ0,{$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF});
if OQC0C0QOQ0>OCC0C0QOQ0 then
OQC0C0QOQ0:=OQC0C0QOQ0-OCC0C0QOQ0
else
OQC0C0QOQ0:=0;
end;
end;
procedure O0O0C0QOQ0.OQ0OC0QOQ0;
begin
OOO0C0QOQ0.Enter;
try
Dec(O0Q0C0QOQ0);
if O0Q0C0QOQ0=0 then begin
Inc(OCO0C0QOQ0);
OOQ0C0QOQ0:={$IFDEF DARWIN}nil{$ELSE}0{$ENDIF};
OQO0C0QOQ0.SetEvent;
end;
finally
OOO0C0QOQ0.Leave;
end;
end;
class procedure OC00Q0QOQ0.O0O0Q0QOQ0(OOO0Q0QOQ0:OQCQQ0QOQ0;const OOOCQ0QOQ0:string);
begin
if Assigned(OOO0Q0QOQ0)then
OOO0Q0QOQ0.O0OCQ0QOQ0(OOOCQ0QOQ0);
end;
class procedure OC00Q0QOQ0.O0O0Q0QOQ0(OOO0Q0QOQ0:OQCQQ0QOQ0;const OOOCQ0QOQ0:string;const OQO0Q0QOQ0:array of const);
begin
if Assigned(OOO0Q0QOQ0)then
OOO0Q0QOQ0.O0OCQ0QOQ0(Format(OOOCQ0QOQ0,OQO0Q0QOQ0));
end;
class procedure OC00Q0QOQ0.OCO0Q0QOQ0(O0Q0Q0QOQ0:OQCQQ0QOQ0;const OCOCQ0QOQ0:string);
begin
if Assigned(O0Q0Q0QOQ0)then
O0Q0Q0QOQ0.OQOCQ0QOQ0(OCOCQ0QOQ0);
end;
class procedure OC00Q0QOQ0.OCO0Q0QOQ0(O0Q0Q0QOQ0:OQCQQ0QOQ0;const OCOCQ0QOQ0:string;const OOQ0Q0QOQ0:array of const);
begin
if Assigned(O0Q0Q0QOQ0)then
O0Q0Q0QOQ0.OQOCQ0QOQ0(Format(OCOCQ0QOQ0,OOQ0Q0QOQ0));
end;
class procedure OC00Q0QOQ0.OQQ0Q0QOQ0(OCQ0Q0QOQ0:OQCQQ0QOQ0;const OOQCQ0QOQ0:string);
begin
if Assigned(OCQ0Q0QOQ0)then
OCQ0Q0QOQ0.O0QCQ0QOQ0(OOQCQ0QOQ0);
end;
class procedure OC00Q0QOQ0.OQQ0Q0QOQ0(OCQ0Q0QOQ0:OQCQQ0QOQ0;const OOQCQ0QOQ0:string;const O0C0Q0QOQ0:array of const);
begin
if Assigned(OCQ0Q0QOQ0)then
OCQ0Q0QOQ0.O0QCQ0QOQ0(Format(OOQCQ0QOQ0,O0C0Q0QOQ0));
end;
class procedure OC00Q0QOQ0.OOC0Q0QOQ0(OQC0Q0QOQ0:OQCQQ0QOQ0;const OCQCQ0QOQ0:string;O0CCQ0QOQ0:Exception);
begin
if Assigned(OQC0Q0QOQ0)then
if O0CCQ0QOQ0<>nil then
OQC0Q0QOQ0.OQQCQ0QOQ0(OCQCQ0QOQ0+#13#10+O0CCQ0QOQ0.Message,O0CCQ0QOQ0)
else
OQC0Q0QOQ0.OQQCQ0QOQ0(OCQCQ0QOQ0,nil);
end;
class procedure OC00Q0QOQ0.OOC0Q0QOQ0(OQC0Q0QOQ0:OQCQQ0QOQ0;const OCQCQ0QOQ0:string;const OCC0Q0QOQ0:array of const;O0CCQ0QOQ0:Exception);
begin
if Assigned(OQC0Q0QOQ0)then
if O0CCQ0QOQ0<>nil then
OQC0Q0QOQ0.OQQCQ0QOQ0(Format(OCQCQ0QOQ0,OCC0Q0QOQ0)+#13#10+O0CCQ0QOQ0.Message,O0CCQ0QOQ0)
else
OQC0Q0QOQ0.OQQCQ0QOQ0(Format(OCQCQ0QOQ0,OCC0Q0QOQ0),nil);
end;
constructor OQCQQ0QOQ0.Create;
begin
inherited;
OCCQQ0QOQ0:=TCriticalSection.Create;
end;
destructor OQCQQ0QOQ0.Destroy;
begin
OCCQQ0QOQ0.Free;
inherited;
end;
procedure OQCQQ0QOQ0.O0OCQ0QOQ0(const OOOCQ0QOQ0:string);
begin
OCCQQ0QOQ0.Enter;
try
if Assigned(O00CQ0QOQ0)then
O00CQ0QOQ0(Self,OOOCQ0QOQ0);
finally
OCCQQ0QOQ0.Leave;
end;
end;
procedure OQCQQ0QOQ0.OQOCQ0QOQ0(const OCOCQ0QOQ0:string);
begin
OCCQQ0QOQ0.Enter;
try
if Assigned(OO0CQ0QOQ0)then
OO0CQ0QOQ0(Self,OCOCQ0QOQ0);
finally
OCCQQ0QOQ0.Leave;
end;
end;
procedure OQCQQ0QOQ0.O0QCQ0QOQ0(const OOQCQ0QOQ0:string);
begin
OCCQQ0QOQ0.Enter;
try
if Assigned(OQ0CQ0QOQ0)then
OQ0CQ0QOQ0(Self,OOQCQ0QOQ0);
finally
OCCQQ0QOQ0.Leave;
end;
end;
procedure OQCQQ0QOQ0.OQQCQ0QOQ0(const OCQCQ0QOQ0:string;O0CCQ0QOQ0:Exception);
begin
OCCQQ0QOQ0.Enter;
try
if Assigned(OC0CQ0QOQ0)then
OC0CQ0QOQ0(Self,OCQCQ0QOQ0,O0CCQ0QOQ0);
finally
OCCQQ0QOQ0.Leave;
end;
end;
type
{$IFDEF VER12P}
{$WARN WIDECHAR_REDUCED OFF}
{$ENDIF}
TMaskSet=set of Char;
OCQQ0COOQ0=^TMaskSet;
TMaskStates=(msLiteral,msAny,msSet,msMBCSLiteral);
O0CQ0COOQ0=record
OOCQ0COOQ0:Boolean;
case OQCQ0COOQ0:TMaskStates of
msLiteral:(OCCQ0COOQ0:Char);
msAny:();
msSet:(
O00C0COOQ0:Boolean;
OO0C0COOQ0:OCQQ0COOQ0);
msMBCSLiteral:(OQ0C0COOQ0,OC0C0COOQ0:Char);
end;
O0OC0COOQ0=class
private
OOOC0COOQ0:array of O0CQ0COOQ0;
protected
function OQOC0COOQ0(const OCOC0COOQ0:string):Integer;
procedure O0Q00COOQ0;
function OQQ00COOQ0(const OCQ00COOQ0:string):Boolean;
public
constructor Create(const OQCO0COOQ0:string);
destructor Destroy;override;
function OO0QCCOOQ0(const OQ0QCCOOQ0:string):Boolean;
end;
const
OC0QCCOOQ0=30;
function O0OC0COOQ0.OQOC0COOQ0(const OCOC0COOQ0:string):Integer;
var
O0QC0COOQ0:Integer;
OOQC0COOQ0:Boolean;
OQQC0COOQ0:Char;
OCQC0COOQ0,O0CC0COOQ0:Char;
OOCC0COOQ0:PChar;
OQCC0COOQ0:Boolean;
OCCC0COOQ0:TMaskSet;
O0000COOQ0:Integer;
procedure OO000COOQ0;
begin
raise Exception.CreateResFmt(@SInvalidMask,[OCOC0COOQ0,OOCC0COOQ0-PChar(OCOC0COOQ0)+1]);
end;
procedure OQ000COOQ0;
begin
OOQC0COOQ0:=False;
OQCC0COOQ0:=False;
OCCC0COOQ0:=[];
end;
procedure OC000COOQ0(O0O00COOQ0:TMaskStates);
begin
if O0QC0COOQ0<=High(OOOC0COOQ0)then
begin
if OOQC0COOQ0 then
begin
Inc(O0000COOQ0);
if O0000COOQ0>OC0QCCOOQ0 then OO000COOQ0;
end;
OOOC0COOQ0[O0QC0COOQ0].OOCQ0COOQ0:=OOQC0COOQ0;
OOOC0COOQ0[O0QC0COOQ0].OQCQ0COOQ0:=O0O00COOQ0;
case O0O00COOQ0 of
msLiteral:OOOC0COOQ0[O0QC0COOQ0].OCCQ0COOQ0:=UpCase(OQQC0COOQ0);
msSet:
begin
OOOC0COOQ0[O0QC0COOQ0].O00C0COOQ0:=OQCC0COOQ0;
New(OOOC0COOQ0[O0QC0COOQ0].OO0C0COOQ0);
OOOC0COOQ0[O0QC0COOQ0].OO0C0COOQ0^:=OCCC0COOQ0;
end;
msMBCSLiteral:
begin
OOOC0COOQ0[O0QC0COOQ0].OQ0C0COOQ0:=OCQC0COOQ0;
OOOC0COOQ0[O0QC0COOQ0].OC0C0COOQ0:=O0CC0COOQ0;
end;
end;
end;
Inc(O0QC0COOQ0);
OQ000COOQ0;
end;
procedure OOO00COOQ0;
var
OQO00COOQ0:Char;
OCO00COOQ0:Char;
begin
Inc(OOCC0COOQ0);
if OOCC0COOQ0^='!' then
begin
OQCC0COOQ0:=True;
Inc(OOCC0COOQ0);
end;
OQO00COOQ0:=#0;
while not(OOCC0COOQ0^in[#0,']'])do
begin
if {$IFDEF VER14P}IsLeadChar(OOCC0COOQ0^){$ELSE}CharInSet(OOCC0COOQ0^,LeadBytes){$ENDIF} then
Inc(OOCC0COOQ0)
else
case OOCC0COOQ0^of
'-':
if OQO00COOQ0=#0 then OO000COOQ0
else
begin
Inc(OOCC0COOQ0);
for OCO00COOQ0:=OQO00COOQ0 to UpCase(OOCC0COOQ0^)do
OCCC0COOQ0:=OCCC0COOQ0+[OCO00COOQ0];
end;
else
OQO00COOQ0:=UpCase(OOCC0COOQ0^);
OCCC0COOQ0:=OCCC0COOQ0+[OQO00COOQ0];
end;
Inc(OOCC0COOQ0);
end;
if(OOCC0COOQ0^<>']')or(OCCC0COOQ0=[])then OO000COOQ0;
OC000COOQ0(msSet);
end;
begin
OOCC0COOQ0:=PChar(OCOC0COOQ0);
O0QC0COOQ0:=0;
O0000COOQ0:=0;
OQ000COOQ0;
while OOCC0COOQ0^<>#0 do
begin
case OOCC0COOQ0^of
'*':OOQC0COOQ0:=True;
'?':if not OOQC0COOQ0 then OC000COOQ0(msAny);
'[':OOO00COOQ0;
else
if {$IFDEF VER14P}IsLeadChar(OOCC0COOQ0^){$ELSE}CharInSet(OOCC0COOQ0^,LeadBytes){$ENDIF} then
begin
OCQC0COOQ0:=OOCC0COOQ0^;
Inc(OOCC0COOQ0);
O0CC0COOQ0:=OOCC0COOQ0^;
OC000COOQ0(msMBCSLiteral);
end
else
begin
OQQC0COOQ0:=OOCC0COOQ0^;
OC000COOQ0(msLiteral);
end;
end;
Inc(OOCC0COOQ0);
end;
OQQC0COOQ0:=#0;
OC000COOQ0(msLiteral);
Result:=O0QC0COOQ0;
end;
function O0OC0COOQ0.OQQ00COOQ0(const OCQ00COOQ0:string):Boolean;
type
O0C00COOQ0=record
OOC00COOQ0:PChar;
OQC00COOQ0:Integer;
end;
var
OCC00COOQ0:Integer;
O00O0COOQ0:array of O0C00COOQ0;
OO0O0COOQ0:Integer;
OQ0O0COOQ0:PChar;
procedure OC0O0COOQ0(O0OO0COOQ0:PChar;OOOO0COOQ0:Integer);
begin
O00O0COOQ0[OCC00COOQ0].OOC00COOQ0:=O0OO0COOQ0;
O00O0COOQ0[OCC00COOQ0].OQC00COOQ0:=OOOO0COOQ0;
Inc(OCC00COOQ0);
end;
function OQOO0COOQ0(var OCOO0COOQ0:PChar;var O0QO0COOQ0:Integer):Boolean;
begin
if OCC00COOQ0=0 then
Result:=False
else
begin
Dec(OCC00COOQ0);
OCOO0COOQ0:=O00O0COOQ0[OCC00COOQ0].OOC00COOQ0;
O0QO0COOQ0:=O00O0COOQ0[OCC00COOQ0].OQC00COOQ0;
Result:=True;
end;
end;
function OOQO0COOQ0(OQQO0COOQ0:PChar;OCQO0COOQ0:Integer):Boolean;
var
O0CO0COOQ0:Integer;
begin
Result:=False;
for O0CO0COOQ0:=OCQO0COOQ0 to High(OOOC0COOQ0)do
begin
if OOOC0COOQ0[O0CO0COOQ0].OOCQ0COOQ0 then
begin
case OOOC0COOQ0[O0CO0COOQ0].OQCQ0COOQ0 of
msLiteral:
while(OQQO0COOQ0^<>#0)and(UpCase(OQQO0COOQ0^)<>OOOC0COOQ0[O0CO0COOQ0].OCCQ0COOQ0)do Inc(OQQO0COOQ0);
msSet:
while(OQQO0COOQ0^<>#0)and not(OOOC0COOQ0[O0CO0COOQ0].O00C0COOQ0 xor(UpCase(OQQO0COOQ0^)in OOOC0COOQ0[O0CO0COOQ0].OO0C0COOQ0^))do Inc(OQQO0COOQ0);
msMBCSLiteral:
while(OQQO0COOQ0^<>#0)do
begin
if(OQQO0COOQ0^<>OOOC0COOQ0[O0CO0COOQ0].OQ0C0COOQ0)then Inc(OQQO0COOQ0,2)
else
begin
Inc(OQQO0COOQ0);
if(OQQO0COOQ0^=OOOC0COOQ0[O0CO0COOQ0].OC0C0COOQ0)then Break;
Inc(OQQO0COOQ0);
end;
end;
end;
if OQQO0COOQ0^<>#0 then
OC0O0COOQ0(@OQQO0COOQ0[1],O0CO0COOQ0);
end;
case OOOC0COOQ0[O0CO0COOQ0].OQCQ0COOQ0 of
msLiteral:if UpCase(OQQO0COOQ0^)<>OOOC0COOQ0[O0CO0COOQ0].OCCQ0COOQ0 then Exit;
msSet:if not(OOOC0COOQ0[O0CO0COOQ0].O00C0COOQ0 xor(UpCase(OQQO0COOQ0^)in OOOC0COOQ0[O0CO0COOQ0].OO0C0COOQ0^))then Exit;
msMBCSLiteral:
begin
if OQQO0COOQ0^<>OOOC0COOQ0[O0CO0COOQ0].OQ0C0COOQ0 then Exit;
Inc(OQQO0COOQ0);
if OQQO0COOQ0^<>OOOC0COOQ0[O0CO0COOQ0].OC0C0COOQ0 then Exit;
end;
msAny:
if OQQO0COOQ0^=#0 then
begin
Result:=False;
Exit;
end;
end;
Inc(OQQO0COOQ0);
end;
Result:=True;
end;
begin
SetLength(O00O0COOQ0,OC0QCCOOQ0);
Result:=True;
OCC00COOQ0:=0;
OQ0O0COOQ0:=PChar(OCQ00COOQ0);
OO0O0COOQ0:=Low(OOOC0COOQ0);
repeat
if OOQO0COOQ0(OQ0O0COOQ0,OO0O0COOQ0)then Exit;
until not OQOO0COOQ0(OQ0O0COOQ0,OO0O0COOQ0);
Result:=False;
end;
procedure O0OC0COOQ0.O0Q00COOQ0;
var
OOQ00COOQ0:Integer;
begin
for OOQ00COOQ0:=Low(OOOC0COOQ0)to High(OOOC0COOQ0)do
if OOOC0COOQ0[OOQ00COOQ0].OQCQ0COOQ0=msSet then Dispose(OOOC0COOQ0[OOQ00COOQ0].OO0C0COOQ0);
end;
constructor O0OC0COOQ0.Create(const OQCO0COOQ0:string);
var
OCCO0COOQ0:Integer;
begin
inherited Create;
SetLength(OOOC0COOQ0,1);
OCCO0COOQ0:=OQOC0COOQ0(OQCO0COOQ0);
O0Q00COOQ0;
SetLength(OOOC0COOQ0,OCCO0COOQ0);
OQOC0COOQ0(OQCO0COOQ0);
end;
destructor O0OC0COOQ0.Destroy;
begin
O0Q00COOQ0;
SetLength(OOOC0COOQ0,0);
inherited;
end;
function O0OC0COOQ0.OO0QCCOOQ0(const OQ0QCCOOQ0:string):Boolean;
begin
Result:=OQQ00COOQ0(OQ0QCCOOQ0);
end;
function OC00OCOOQ0(const O0O0OCOOQ0,OOO0OCOOQ0:string):Boolean;
var
OQO0OCOOQ0:O0OC0COOQ0;
begin
OQO0OCOOQ0:=O0OC0COOQ0.Create(OOO0OCOOQ0);
try
Result:=OQO0OCOOQ0.OO0QCCOOQ0(O0O0OCOOQ0);
finally
OQO0OCOOQ0.Free;
end;
end;
procedure O0OQCCOOQ0;
var
OOOQCCOOQ0:StringBuilder;
begin
OOOQCCOOQ0:=StringBuilder.Create;
try
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
OOOQCCOOQ0.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
OOOQCCOOQ0.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
OOOQCCOOQ0.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
OOOQCCOOQ0.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381');
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFF');
O00Q0COOQ0:=TBigInteger.Create(string(OOOQCCOOQ0.ToString),16);
finally
OOOQCCOOQ0.Free;
end;
OOOQCCOOQ0:=StringBuilder.Create;
try
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
OOOQCCOOQ0.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
OOOQCCOOQ0.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
OOOQCCOOQ0.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
OOOQCCOOQ0.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
OOOQCCOOQ0.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
OOOQCCOOQ0.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
OOOQCCOOQ0.Append('670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF');
OO0Q0COOQ0:=TBigInteger.Create(string(OOOQCCOOQ0.ToString),16);
finally
OOOQCCOOQ0.Free;
end;
OOOQCCOOQ0:=StringBuilder.Create;
try
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
OOOQCCOOQ0.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
OOOQCCOOQ0.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
OOOQCCOOQ0.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
OOOQCCOOQ0.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
OOOQCCOOQ0.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
OOOQCCOOQ0.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
OOOQCCOOQ0.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B');
OOOQCCOOQ0.Append('E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9');
OOOQCCOOQ0.Append('DE2BCBF6955817183995497CEA956AE515D2261898FA0510');
OOOQCCOOQ0.Append('15728E5A8AACAA68FFFFFFFFFFFFFFFF');
OQ0Q0COOQ0:=TBigInteger.Create(string(OOOQCCOOQ0.ToString),16);
finally
OOOQCCOOQ0.Free;
end;
OOOQCCOOQ0:=StringBuilder.Create;
try
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
OOOQCCOOQ0.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
OOOQCCOOQ0.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
OOOQCCOOQ0.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
OOOQCCOOQ0.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
OOOQCCOOQ0.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
OOOQCCOOQ0.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
OOOQCCOOQ0.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B');
OOOQCCOOQ0.Append('E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9');
OOOQCCOOQ0.Append('DE2BCBF6955817183995497CEA956AE515D2261898FA0510');
OOOQCCOOQ0.Append('15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64');
OOOQCCOOQ0.Append('ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7');
OOOQCCOOQ0.Append('ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B');
OOOQCCOOQ0.Append('F12FFA06D98A0864D87602733EC86A64521F2B18177B200C');
OOOQCCOOQ0.Append('BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31');
OOOQCCOOQ0.Append('43DB5BFCE0FD108E4B82D120A93AD2CAFFFFFFFFFFFFFFFF');
OC0Q0COOQ0:=TBigInteger.Create(string(OOOQCCOOQ0.ToString),16);
finally
OOOQCCOOQ0.Free;
end;
OOOQCCOOQ0:=StringBuilder.Create;
try
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
OOOQCCOOQ0.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
OOOQCCOOQ0.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
OOOQCCOOQ0.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
OOOQCCOOQ0.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
OOOQCCOOQ0.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
OOOQCCOOQ0.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
OOOQCCOOQ0.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B');
OOOQCCOOQ0.Append('E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9');
OOOQCCOOQ0.Append('DE2BCBF6955817183995497CEA956AE515D2261898FA0510');
OOOQCCOOQ0.Append('15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64');
OOOQCCOOQ0.Append('ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7');
OOOQCCOOQ0.Append('ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B');
OOOQCCOOQ0.Append('F12FFA06D98A0864D87602733EC86A64521F2B18177B200C');
OOOQCCOOQ0.Append('BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31');
OOOQCCOOQ0.Append('43DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D7');
OOOQCCOOQ0.Append('88719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA');
OOOQCCOOQ0.Append('2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6');
OOOQCCOOQ0.Append('287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED');
OOOQCCOOQ0.Append('1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA9');
OOOQCCOOQ0.Append('93B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934063199');
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFF');
O0OQ0COOQ0:=TBigInteger.Create(string(OOOQCCOOQ0.ToString),16);
finally
OOOQCCOOQ0.Free;
end;
OOOQCCOOQ0:=StringBuilder.Create;
try
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E08');
OOOQCCOOQ0.Append('8A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B');
OOOQCCOOQ0.Append('302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9');
OOOQCCOOQ0.Append('A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE6');
OOOQCCOOQ0.Append('49286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8');
OOOQCCOOQ0.Append('FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D');
OOOQCCOOQ0.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C');
OOOQCCOOQ0.Append('180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF695581718');
OOOQCCOOQ0.Append('3995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D');
OOOQCCOOQ0.Append('04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7D');
OOOQCCOOQ0.Append('B3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D226');
OOOQCCOOQ0.Append('1AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200C');
OOOQCCOOQ0.Append('BBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFC');
OOOQCCOOQ0.Append('E0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B26');
OOOQCCOOQ0.Append('99C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB');
OOOQCCOOQ0.Append('04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2');
OOOQCCOOQ0.Append('233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127');
OOOQCCOOQ0.Append('D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934028492');
OOOQCCOOQ0.Append('36C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BDF8FF9406');
OOOQCCOOQ0.Append('AD9E530EE5DB382F413001AEB06A53ED9027D831179727B0865A8918');
OOOQCCOOQ0.Append('DA3EDBEBCF9B14ED44CE6CBACED4BB1BDB7F1447E6CC254B33205151');
OOOQCCOOQ0.Append('2BD7AF426FB8F401378CD2BF5983CA01C64B92ECF032EA15D1721D03');
OOOQCCOOQ0.Append('F482D7CE6E74FEF6D55E702F46980C82B5A84031900B1C9E59E7C97F');
OOOQCCOOQ0.Append('BEC7E8F323A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AA');
OOOQCCOOQ0.Append('CC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE32806A1D58B');
OOOQCCOOQ0.Append('B7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55CDA56C9EC2EF29632');
OOOQCCOOQ0.Append('387FE8D76E3C0468043E8F663F4860EE12BF2D5B0B7474D6E694F91E');
OOOQCCOOQ0.Append('6DCC4024FFFFFFFFFFFFFFFF');
OOOQ0COOQ0:=TBigInteger.Create(string(OOOQCCOOQ0.ToString),16);
finally
OOOQCCOOQ0.Free;
end;
OOOQCCOOQ0:=StringBuilder.Create;
try
OOOQCCOOQ0.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
OOOQCCOOQ0.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
OOOQCCOOQ0.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
OOOQCCOOQ0.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
OOOQCCOOQ0.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
OOOQCCOOQ0.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
OOOQCCOOQ0.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
OOOQCCOOQ0.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B');
OOOQCCOOQ0.Append('E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9');
OOOQCCOOQ0.Append('DE2BCBF6955817183995497CEA956AE515D2261898FA0510');
OOOQCCOOQ0.Append('15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64');
OOOQCCOOQ0.Append('ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7');
OOOQCCOOQ0.Append('ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B');
OOOQCCOOQ0.Append('F12FFA06D98A0864D87602733EC86A64521F2B18177B200C');
OOOQCCOOQ0.Append('BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31');
OOOQCCOOQ0.Append('43DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D7');
OOOQCCOOQ0.Append('88719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA');
OOOQCCOOQ0.Append('2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6');
OOOQCCOOQ0.Append('287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED');
OOOQCCOOQ0.Append('1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA9');
OOOQCCOOQ0.Append('93B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934028492');
OOOQCCOOQ0.Append('36C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BD');
OOOQCCOOQ0.Append('F8FF9406AD9E530EE5DB382F413001AEB06A53ED9027D831');
OOOQCCOOQ0.Append('179727B0865A8918DA3EDBEBCF9B14ED44CE6CBACED4BB1B');
OOOQCCOOQ0.Append('DB7F1447E6CC254B332051512BD7AF426FB8F401378CD2BF');
OOOQCCOOQ0.Append('5983CA01C64B92ECF032EA15D1721D03F482D7CE6E74FEF6');
OOOQCCOOQ0.Append('D55E702F46980C82B5A84031900B1C9E59E7C97FBEC7E8F3');
OOOQCCOOQ0.Append('23A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AA');
OOOQCCOOQ0.Append('CC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE328');
OOOQCCOOQ0.Append('06A1D58BB7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55C');
OOOQCCOOQ0.Append('DA56C9EC2EF29632387FE8D76E3C0468043E8F663F4860EE');
OOOQCCOOQ0.Append('12BF2D5B0B7474D6E694F91E6DBE115974A3926F12FEE5E4');
OOOQCCOOQ0.Append('38777CB6A932DF8CD8BEC4D073B931BA3BC832B68D9DD300');
OOOQCCOOQ0.Append('741FA7BF8AFC47ED2576F6936BA424663AAB639C5AE4F568');
OOOQCCOOQ0.Append('3423B4742BF1C978238F16CBE39D652DE3FDB8BEFC848AD9');
OOOQCCOOQ0.Append('22222E04A4037C0713EB57A81A23F0C73473FC646CEA306B');
OOOQCCOOQ0.Append('4BCBC8862F8385DDFA9D4B7FA2C087E879683303ED5BDD3A');
OOOQCCOOQ0.Append('062B3CF5B3A278A66D2A13F83F44F82DDF310EE074AB6A36');
OOOQCCOOQ0.Append('4597E899A0255DC164F31CC50846851DF9AB48195DED7EA1');
OOOQCCOOQ0.Append('B1D510BD7EE74D73FAF36BC31ECFA268359046F4EB879F92');
OOOQCCOOQ0.Append('4009438B481C6CD7889A002ED5EE382BC9190DA6FC026E47');
OOOQCCOOQ0.Append('9558E4475677E9AA9E3050E2765694DFC81F56E880B96E71');
OOOQCCOOQ0.Append('60C980DD98EDD3DFFFFFFFFFFFFFFFFF');
OQOQ0COOQ0:=TBigInteger.Create(string(OOOQCCOOQ0.ToString),16);
finally
OOOQCCOOQ0.Free;
end;
end;
initialization
OCCOOCOOQ0:=OQCQQ0QOQ0.Create;
O0OQCCOOQ0;
finalization
OCCOOCOOQ0.Free;
O00Q0COOQ0.Free;
OO0Q0COOQ0.Free;
OQ0Q0COOQ0.Free;
OC0Q0COOQ0.Free;
O0OQ0COOQ0.Free;
OOOQ0COOQ0.Free;
OQOQ0COOQ0.Free;
end.
