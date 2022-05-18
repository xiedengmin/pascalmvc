//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsBridgeUni;
interface
uses
{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
{$IFNDEF BCB}
{$NOINCLUDE winsock}
{$HPPEMIT '#include <winsock2.h>'}
{$ENDIF}
{$ENDIF}
WinSock,
Windows,Registry,
{$ENDIF}
{$IFDEF VER16P}System.Types,{$ELSE}Types,{$ENDIF}
{$IFDEF POSIX}
Posix.SysSocket,Posix.Unistd,Posix.Stdio,
{$ENDIF}
{$IFDEF UNIX}
Sockets,
{$ENDIF}
{$IFDEF NEXTGEN}Generics.Collections,{$ELSE}Contnrs,{$ENDIF}
Classes,SysUtils,SyncObjs,
CLRClasses,CRTypes,CRFunctions,
CRHashAlgorithm,CRSymmetricAlgorithm,CRHMAC,CRCipher,CRRNG,CRBigInteger,
{$IFNDEF UNIDACPRO}
TdsSSLConsts,TdsUtils,
TdsASN1,TdsOids,TdsCertificateConsts,TdsCertificateExts,TdsEC25519;
{$ELSE}
TdsSSLConstsUni,TdsUtilsUni,
TdsASN1Uni,TdsOidsUni,TdsCertificateConstsUni,TdsCertificateExtsUni,TdsEC25519Uni;
{$ENDIF}
{$IFNDEF DAC}
{$I SecureBridgeVer.inc}
{$ENDIF}
{$IFDEF FPC}
{$IFDEF MSWINDOWS}
const
HKEY_CURRENT_USER=NativeUint($80000001);
{$ENDIF}
{$ENDIF}
const
OCCQ0O0OQ0='.';
O00C0O0OQ0='key';
OO0C0O0OQ0='usr';
OQ0C0O0OQ0='crt';
OC0C0O0OQ0='crl';
type
OCOC0O0OQ0=(O0OC0O0OQ0,OOOC0O0OQ0,OQOC0O0OQ0);
type
O0QC0O0OQ0=record
OOQC0O0OQ0:TBigInteger;
OQQC0O0OQ0:TBigInteger;
OCQC0O0OQ0:TBigInteger;
O0CC0O0OQ0:TBigInteger;
end;
OOCC0O0OQ0=record
OQCC0O0OQ0:TBigInteger;
OCCC0O0OQ0:TBigInteger;
O0000O0OQ0:TBigInteger;
OO000O0OQ0:TBigInteger;
OQ000O0OQ0:TBigInteger;
end;
OC000O0OQ0=record
O0O00O0OQ0:TBigInteger;
OOO00O0OQ0:TBigInteger;
OQO00O0OQ0:TBigInteger;
OCO00O0OQ0:TBigInteger;
O0Q00O0OQ0:TBigInteger;
OOQ00O0OQ0:TBigInteger;
OQQ00O0OQ0:TBigInteger;
OCQ00O0OQ0:TBigInteger;
end;
O0C00O0OQ0=record
OOC00O0OQ0:cardinal;
OQC00O0OQ0:cardinal;
OCC00O0OQ0:cardinal;
O00O0O0OQ0:cardinal;
OO0O0O0OQ0:cardinal;
OQ0O0O0OQ0:cardinal;
OC0O0O0OQ0:cardinal;
O0OO0O0OQ0:cardinal;
OOOO0O0OQ0:cardinal;
OQOO0O0OQ0:cardinal;
OCOO0O0OQ0:cardinal;
O0QO0O0OQ0:cardinal;
OOQO0O0OQ0:cardinal;
OQQO0O0OQ0:cardinal;
OCQO0O0OQ0:cardinal;
O0CO0O0OQ0:cardinal;
OOCO0O0OQ0:cardinal;
OQCO0O0OQ0:cardinal;
end;
OCCO0O0OQ0=record
O00QCO0OQ0:cardinal;
OO0QCO0OQ0:cardinal;
OQ0QCO0OQ0:cardinal;
OC0QCO0OQ0:cardinal;
O0OQCO0OQ0:cardinal;
OOOQCO0OQ0:cardinal;
OQOQCO0OQ0:cardinal;
OCOQCO0OQ0:cardinal;
O0QQCO0OQ0:cardinal;
OOQQCO0OQ0:cardinal;
OQQQCO0OQ0:cardinal;
OCQQCO0OQ0:cardinal;
O0CQCO0OQ0:cardinal;
OOCQCO0OQ0:cardinal;
OQCQCO0OQ0:cardinal;
OCCQCO0OQ0:cardinal;
O00CCO0OQ0:cardinal;
end;
OO0CCO0OQ0=class;
O0QOCO0OQ0=class of OO0CCO0OQ0;
TScECParameters=record
ECName:OOQOQOQOQ0;
CryptographyClass:O0QOCO0OQ0;
Size:integer;
p:string;
a,b:string;
Gx,Gy:string;
n:string;
OID:string;
SSHName:string;
end;
T32BytesField=class(TPersistent)
private
OOQOCO0OQ0:OCQCQC0OQ0;
public
constructor Create;overload;
constructor Create(const OQQOCO0OQ0:TBytes);overload;
procedure Assign(O000CO0OQ0:TPersistent);override;
function Equal(OCQOCO0OQ0:T32BytesField):boolean;
function GetBytes:TBytes;
end;
O0COCO0OQ0=class of OOCOCO0OQ0;
OOCOCO0OQ0=class
public
procedure OQCOCO0OQ0(O000CO0OQ0:OOCOCO0OQ0);virtual;abstract;
function Equals(OCCOCO0OQ0:OOCOCO0OQ0):boolean;reintroduce;virtual;abstract;
end;
O00QQO0OQ0=class(OOCOCO0OQ0)
public
OO0QQO0OQ0,OQ0QQO0OQ0,OC0QQO0OQ0:TBigInteger;
destructor Destroy;override;
procedure OQCOCO0OQ0(O000CO0OQ0:OOCOCO0OQ0);override;
function Equals(OCCOCO0OQ0:OOCOCO0OQ0):boolean;reintroduce;override;
end;
OOOQQO0OQ0=class(OOCOCO0OQ0)
public
OQOQQO0OQ0:OCQCQC0OQ0;
procedure OQCOCO0OQ0(O000CO0OQ0:OOCOCO0OQ0);override;
function Equals(OCCOCO0OQ0:OOCOCO0OQ0):boolean;reintroduce;override;
end;
OCOQQO0OQ0=record
O0QQQO0OQ0:OOQOQOQOQ0;
OOQQQO0OQ0:OO0CCO0OQ0;
OQQQQO0OQ0:OOCOCO0OQ0;
OCQQQO0OQ0:TPersistent;
end;
OO0CCO0OQ0=class
protected
OQ0CCO0OQ0:boolean;
OC0CCO0OQ0:integer;
O0OCCO0OQ0:boolean;
function OOOCCO0OQ0(OQOCCO0OQ0:TBigInteger):TBytes;
procedure OOQCCO0OQ0(var OQQCCO0OQ0:OCOQQO0OQ0;OCQCCO0OQ0:IScRandom=nil);virtual;abstract;
function O0CCCO0OQ0:integer;virtual;abstract;
public
constructor Create;overload;virtual;
constructor Create(const OQCCCO0OQ0:TScECParameters);overload;virtual;
procedure OCCCCO0OQ0(O000CO0OQ0:OO0CCO0OQ0);virtual;
function Equals(OO00CO0OQ0:OO0CCO0OQ0):boolean;reintroduce;virtual;abstract;
function OQ00CO0OQ0:O0COCO0OQ0;virtual;abstract;
function OC00CO0OQ0:TPersistentClass;virtual;abstract;
function O0O0CO0OQ0(OOO0CO0OQ0:OOCOCO0OQ0;OQO0CO0OQ0:TObject):OOCOCO0OQ0;virtual;abstract;
function OCO0CO0OQ0(const O0Q0CO0OQ0:TBytes;OOQ0CO0OQ0,OQQ0CO0OQ0:integer):OOCOCO0OQ0;virtual;abstract;
function OCQ0CO0OQ0(O0C0CO0OQ0:OOCOCO0OQ0):TBytes;virtual;abstract;
function OOC0CO0OQ0(const OQC0CO0OQ0:TBytes;OCC0CO0OQ0:TObject;O00OCO0OQ0:OOCOCO0OQ0):TBytes;virtual;abstract;
function OO0OCO0OQ0(const OQ0OCO0OQ0,OC0OCO0OQ0:TBytes;O0OOCO0OQ0:OOCOCO0OQ0):boolean;virtual;abstract;
property OOOOCO0OQ0:integer read OC0CCO0OQ0;
property OQOOCO0OQ0:integer read O0CCCO0OQ0;
property OCOOCO0OQ0:boolean read O0OCCO0OQ0 write O0OCCO0OQ0;
end;
O0CQQO0OQ0=class(OO0CCO0OQ0)
private
OOCQQO0OQ0:TBigInteger;
OQCQQO0OQ0:TBigInteger;
OCCQQO0OQ0:TBigInteger;
O00CQO0OQ0:TBigInteger;
OO0CQO0OQ0:O00QQO0OQ0;
OQ0CQO0OQ0:TBytes;
OC0CQO0OQ0:integer;
class function O0OCQO0OQ0(OOOCQO0OQ0:TBigInteger):TIntArr;
procedure OQCCQO0OQ0(OCCCQO0OQ0:O00QQO0OQ0;O000QO0OQ0:O00QQO0OQ0);
procedure OO00QO0OQ0(OQ00QO0OQ0:O00QQO0OQ0);
protected
function OC00QO0OQ0(O0O0QO0OQ0:IScRandom=nil):TBigInteger;
procedure OOQCCO0OQ0(var OQQCCO0OQ0:OCOQQO0OQ0;OCQCCO0OQ0:IScRandom=nil);override;
function O0CCCO0OQ0:integer;override;
procedure OQO0QO0OQ0;virtual;abstract;
procedure OCO0QO0OQ0(O0Q0QO0OQ0:O00QQO0OQ0);virtual;abstract;
procedure OOQ0QO0OQ0(OQQ0QO0OQ0,OCQ0QO0OQ0:O00QQO0OQ0;O0C0QO0OQ0:O00QQO0OQ0);virtual;abstract;
procedure OOC0QO0OQ0(OQC0QO0OQ0:O00QQO0OQ0;OCC0QO0OQ0:O00QQO0OQ0);virtual;abstract;
procedure O00OQO0OQ0(OO0OQO0OQ0:O00QQO0OQ0);virtual;abstract;
function OQ0OQO0OQ0:boolean;virtual;
function OC0OQO0OQ0(O0OOQO0OQ0:O00QQO0OQ0):boolean;virtual;abstract;
public
constructor Create;overload;override;
constructor Create(const OQCCCO0OQ0:TScECParameters);overload;override;
destructor Destroy;override;
procedure OCCCCO0OQ0(O000CO0OQ0:OO0CCO0OQ0);override;
function Equals(OO00CO0OQ0:OO0CCO0OQ0):boolean;reintroduce;override;
function OQ00CO0OQ0:O0COCO0OQ0;override;
function OC00CO0OQ0:TPersistentClass;override;
function O0O0CO0OQ0(OOO0CO0OQ0:OOCOCO0OQ0;OQO0CO0OQ0:TObject):OOCOCO0OQ0;override;
function OCO0CO0OQ0(const O0Q0CO0OQ0:TBytes;OOQ0CO0OQ0,OQQ0CO0OQ0:integer):OOCOCO0OQ0;override;
function OCQ0CO0OQ0(O0C0CO0OQ0:OOCOCO0OQ0):TBytes;override;
function OOC0CO0OQ0(const OQC0CO0OQ0:TBytes;OCC0CO0OQ0:TObject;O00OCO0OQ0:OOCOCO0OQ0):TBytes;override;
function OO0OCO0OQ0(const OQ0OCO0OQ0,OC0OCO0OQ0:TBytes;O0OOCO0OQ0:OOCOCO0OQ0):boolean;override;
end;
O0QCO00OQ0=class(O0CQQO0OQ0)
protected
procedure OQO0QO0OQ0;override;
procedure OCO0QO0OQ0(O0Q0QO0OQ0:O00QQO0OQ0);override;
procedure OOQ0QO0OQ0(OQQ0QO0OQ0,OCQ0QO0OQ0:O00QQO0OQ0;O0C0QO0OQ0:O00QQO0OQ0);override;
procedure OOC0QO0OQ0(OQC0QO0OQ0:O00QQO0OQ0;OCC0QO0OQ0:O00QQO0OQ0);override;
procedure O00OQO0OQ0(OO0OQO0OQ0:O00QQO0OQ0);override;
function OC0OQO0OQ0(O0OOQO0OQ0:O00QQO0OQ0):boolean;override;
end;
OQOOO00OQ0=class(O0CQQO0OQ0)
private
OCOOO00OQ0:integer;
procedure O0QOO00OQ0(OOQOO00OQ0,OQQOO00OQ0:O00QQO0OQ0;OCQOO00OQ0:O00QQO0OQ0);
procedure OOOQ000OQ0(OQOQ000OQ0:O00QQO0OQ0;OCOQ000OQ0:O00QQO0OQ0);
procedure O0QQ000OQ0(OOQQ000OQ0,OQQQ000OQ0:O00QQO0OQ0;OCQQ000OQ0:O00QQO0OQ0);
procedure OQOC000OQ0(OCOC000OQ0:O00QQO0OQ0;O0QC000OQ0:O00QQO0OQ0);
protected
procedure OQO0QO0OQ0;override;
procedure OCO0QO0OQ0(O0Q0QO0OQ0:O00QQO0OQ0);override;
procedure OOQ0QO0OQ0(OQQ0QO0OQ0,OCQ0QO0OQ0:O00QQO0OQ0;O0C0QO0OQ0:O00QQO0OQ0);override;
procedure OOC0QO0OQ0(OQC0QO0OQ0:O00QQO0OQ0;OCC0QO0OQ0:O00QQO0OQ0);override;
procedure O00OQO0OQ0(OO0OQO0OQ0:O00QQO0OQ0);override;
function OQ0OQO0OQ0:boolean;override;
function OC0OQO0OQ0(O0OOQO0OQ0:O00QQO0OQ0):boolean;override;
end;
OO00000OQ0=(OQCC000OQ0,OCCC000OQ0,O000000OQ0);
OQ00000OQ0=class(OO0CCO0OQ0)
protected
OC00000OQ0:OO00000OQ0;
function O0O0000OQ0(OOO0000OQ0:T32BytesField):OOOQQO0OQ0;
procedure OOQCCO0OQ0(var OQQCCO0OQ0:OCOQQO0OQ0;OCQCCO0OQ0:IScRandom=nil);override;
function O0CCCO0OQ0:integer;override;
public
constructor Create;overload;override;
constructor Create(const OQCCCO0OQ0:TScECParameters);overload;override;
procedure OCCCCO0OQ0(O000CO0OQ0:OO0CCO0OQ0);override;
function Equals(OO00CO0OQ0:OO0CCO0OQ0):boolean;reintroduce;override;
function OQ00CO0OQ0:O0COCO0OQ0;override;
function OC00CO0OQ0:TPersistentClass;override;
function O0O0CO0OQ0(OOO0CO0OQ0:OOCOCO0OQ0;OQO0CO0OQ0:TObject):OOCOCO0OQ0;override;
function OCO0CO0OQ0(const O0Q0CO0OQ0:TBytes;OOQ0CO0OQ0,OQQ0CO0OQ0:integer):OOCOCO0OQ0;override;
function OCQ0CO0OQ0(O0C0CO0OQ0:OOCOCO0OQ0):TBytes;override;
function OOC0CO0OQ0(const OCO0000OQ0:TBytes;OCC0CO0OQ0:TObject;O00OCO0OQ0:OOCOCO0OQ0):TBytes;override;
function OO0OCO0OQ0(const O0Q0000OQ0,OC0OCO0OQ0:TBytes;O0OOCO0OQ0:OOCOCO0OQ0):boolean;override;
end;
OOQ0000OQ0=class
private
class function OQQ0000OQ0(const OCQ0000OQ0,O0C0000OQ0:TBytes):TBytes;
class function O00O000OQ0(const OO0O000OQ0,OQ0O000OQ0:TBytes;OC0O000OQ0:integer):TBytes;
public
class function OQOO000OQ0(const OCOO000OQ0:OOOOCOQOQ0;
const O0QO000OQ0:string;const OOQO000OQ0:TBytes;
OQQO000OQ0:integer;OCQO000OQ0:integer;O0CO000OQ0:boolean):TBytes;
class function OQOQC00OQ0(const OCOQC00OQ0:string;const O0QQC00OQ0:TBytes;
OOQQC00OQ0:integer;OQQQC00OQ0:integer):TBytes;
class function O0OCC00OQ0(const OOOCC00OQ0:OOOOCOQOQ0;
const OQOCC00OQ0:string;const OCOCC00OQ0:TBytes;
O0QCC00OQ0:OCOC0O0OQ0;OOQCC00OQ0:integer;
OQQCC00OQ0:integer):TBytes;
class function OQ0OC00OQ0(const OC0OC00OQ0:string;const O0OOC00OQ0:TBytes;OOOOC00OQ0:integer;OQOOC00OQ0:integer):TBytes;
class function OO0QQ00OQ0(const OQ0QQ00OQ0:string;const OC0QQ00OQ0:TBytes;
const O0OQQ00OQ0:string):TSymmetricAlgorithm;
end;
type
OQOCQ00OQ0=(O00CQ00OQ0,OO0CQ00OQ0,OQ0CQ00OQ0,OC0CQ00OQ0,O0OCQ00OQ0,OOOCQ00OQ0);
OCQCQ00OQ0=(OCOCQ00OQ0,O0QCQ00OQ0,OOQCQ00OQ0,OQQCQ00OQ0);
OQCCQ00OQ0=(O0CCQ00OQ0,OOCCQ00OQ0);
OCCCQ00OQ0=class;
OC0Q0CC0Q0=class of OCCCQ00OQ0;
O0OQ0CC0Q0=class;
OCCC0CC0Q0=class of O0OQ0CC0Q0;
O0000CC0Q0=class;
OQCCCCC0Q0=class;
OOC0CCC0Q0=class;
O0COCCC0Q0=class;
OQCQQCC0Q0=class;
OOC0QCC0Q0=class;
TScCertificate=class;
OCQOCQC0Q0=class;
O0OQ0CC0Q0=class(TPersistent)
protected
{$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
OOOQ0CC0Q0:O0000CC0Q0;
OQOQ0CC0Q0:TCriticalSection;
OCOQ0CC0Q0:string;
O0QQ0CC0Q0:boolean;
OOQQ0CC0Q0:boolean;
procedure OQQQ0CC0Q0(const OCQQ0CC0Q0:boolean);
procedure O0CQ0CC0Q0(const OOCQ0CC0Q0:string);virtual;
procedure OQCQ0CC0Q0(OCCQ0CC0Q0:O0000CC0Q0);virtual;
procedure OO0C0CC0Q0;virtual;
procedure OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);virtual;
procedure O0OC0CC0Q0;virtual;
function OOOC0CC0Q0:O0OQ0CC0Q0;
procedure OQOC0CC0Q0;
property OCOC0CC0Q0:string read OCOQ0CC0Q0 write O0CQ0CC0Q0;
public
constructor Create(OOQC0CC0Q0:O0000CC0Q0=nil);virtual;
destructor Destroy;override;
procedure Assign(O000CO0OQ0:TPersistent);override;
function Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;reintroduce;virtual;
property OOCC0CC0Q0:O0000CC0Q0 read OOOQ0CC0Q0 write OQCQ0CC0Q0;
property OQCC0CC0Q0:boolean read O0QQ0CC0Q0 write OQQQ0CC0Q0;
end;
O0CQOOC0Q0=class(O0OQ0CC0Q0)
private
OOCQOOC0Q0:TList;
OQCQOOC0Q0:OOQQQOQOQ0;
OCCQOOC0Q0:OC0Q0OOOQ0;
O00COOC0Q0:OCOC0OOOQ0;
function OO0COOC0Q0:OQCCCCC0Q0;
procedure OQ0COOC0Q0(OC0COOC0Q0:OQCCCCC0Q0);
procedure O0OCOOC0Q0;
procedure OOOCOOC0Q0;
class procedure OQOCOOC0Q0(OCOCOOC0Q0,O0QCOOC0Q0:integer;
OOQCOOC0Q0:IScRandom;var OQQCOOC0Q0,OCQCOOC0Q0:TBigInteger);
class function OOC0OOC0Q0(OQC0OOC0Q0,OCC0OOC0Q0:TBigInteger;
O00OOOC0Q0:IScRandom):TBigInteger;
procedure OCOOOOC0Q0(const O0QOOOC0Q0:integer;OOQOOOC0Q0:IScRandom);
procedure OQCOOOC0Q0(const OCCOOOC0Q0:integer;O00Q0OC0Q0:IScRandom);
procedure OOQQ0OC0Q0;
procedure OQQQ0OC0Q0;
function O00C0OC0Q0(const OO0C0OC0Q0:TBytes):TBytes;
function O0QC0OC0Q0(const OOQC0OC0Q0:TBytes):TBytes;
procedure O0CC0OC0Q0(const OOCC0OC0Q0:OOOOCOQOQ0;out OQCC0OC0Q0:PByteArray;out OCCC0OC0Q0:integer);
procedure O0000OC0Q0(OO000OC0Q0:TStringList;const OQ000OC0Q0:string;out OC000OC0Q0:string);
function OOOO0OC0Q0(OQOO0OC0Q0:TStringList;const OCOO0OC0Q0:string;out O0QO0OC0Q0:string):TBytes;
function OOCO0OC0Q0(OQCO0OC0Q0:TStringList;const OCCO0OC0Q0:string;const O00QCOC0Q0:string;out OO0QCOC0Q0:string):TBytes;
procedure OCQQCOC0Q0(O0CQCOC0Q0:TStream;const OOCQCOC0Q0:string;
OQCQCOC0Q0:OO00COQOQ0;var OCCQCOC0Q0:TBytes);
function O0QCCOC0Q0(const OOQCCOC0Q0:string;out OQQCCOC0Q0:string):TBytes;
procedure OQCCCOC0Q0(OCCCCOC0Q0:TStream;const O000COC0Q0:TBytes;const OO00COC0Q0:string);
function O0O0COC0Q0(const OOO0COC0Q0:TBytes;out OQO0COC0Q0:OOQQQOQOQ0;out OCO0COC0Q0:boolean):boolean;
procedure OOQ0COC0Q0(const OQQ0COC0Q0:TBytes;OCQ0COC0Q0:OOQQQOQOQ0;O0C0COC0Q0:boolean);
procedure O00OCOC0Q0(const OO0OCOC0Q0:TBytes);
procedure OQOOCOC0Q0(const OCOOCOC0Q0:TBytes;const O0QOCOC0Q0:string);
procedure OCOQQOC0Q0(const O0QQQOC0Q0:TBytes);
procedure OCCQQOC0Q0(const O00CQOC0Q0:TBytes;const OO0CQOC0Q0:string);
procedure OCOCQOC0Q0(const O0QCQOC0Q0:TBytes;const OOQCQOC0Q0:string);
procedure OC00QOC0Q0(const O0O0QOC0Q0:TBytes);
procedure OOO0QOC0Q0(const OQO0QOC0Q0,OCO0QOC0Q0:TBytes);
procedure OCQ0QOC0Q0(const O0C0QOC0Q0:TBytes;OOC0QOC0Q0:OO00000OQ0);
procedure O00OQOC0Q0(const OO0OQOC0Q0:TBytes;OQ0OQOC0Q0:OO00000OQ0);
procedure O0OOQOC0Q0(const OOOOQOC0Q0:TBytes);overload;
procedure O0OOQOC0Q0(OCOOQOC0Q0:OC0QOQOOQ0);overload;
function OC0QO0C0Q0:TBytes;
function OOOQO0C0Q0:TBytes;
function OCCQO0C0Q0(O00CO0C0Q0:boolean):TBytes;
function OQ0CO0C0Q0:TBytes;
function O0OCO0C0Q0:TBytes;
function OCOCO0C0Q0(const O0QCO0C0Q0:string;OOQCO0C0Q0:OO00COQOQ0):TBytes;
function OOO0O0C0Q0(const OQO0O0C0Q0:string):TBytes;
function OCC0O0C0Q0:TBytes;
procedure O00OO0C0Q0(const OO0OO0C0Q0:TBytes);
function OOCOO0C0Q0(OQCOO0C0Q0:cardinal):O0C00O0OQ0;
function OQ0Q00C0Q0(OC0Q00C0Q0:cardinal):OCCO0O0OQ0;
procedure OCOQ00C0Q0(const O0QQ00C0Q0:TBytes;OOQQ00C0Q0:boolean);
procedure OCQQ00C0Q0(const O0CQ00C0Q0,OOCQ00C0Q0:TBytes);
procedure OCCQ00C0Q0(const O00C00C0Q0,OO0C00C0Q0:TBytes);
procedure OQ0C00C0Q0(const OC0C00C0Q0:TBytes;O0OC00C0Q0:OO00000OQ0);
{$IFDEF MSWINDOWS}
function OOOC00C0Q0:TBytes;
function OCQC00C0Q0:TBytes;
{$ENDIF}
class function OO0000C0Q0(const OQ0000C0Q0:TBytes;OC0000C0Q0:integer;O0O000C0Q0:OOOOCOQOQ0):TBytes;
function O0C000C0Q0(const OOC000C0Q0:TBytes;OQC000C0Q0:OQ0Q0OOOQ0):TBytes;
function O0CO00C0Q0(const OOCO00C0Q0:TBytes;OQCO00C0Q0:OOOOCOQOQ0;OCCO00C0Q0:OQ0Q0OOOQ0):TBytes;
function OO0CC0C0Q0(const OQ0CC0C0Q0,OC0CC0C0Q0:TBytes;O0OCC0C0Q0:OOOOCOQOQ0;OOOCC0C0Q0:OQ0Q0OOOQ0):boolean;
function OQO0C0C0Q0(const OCO0C0C0Q0:TBytes;O0Q0C0C0Q0:OQ0Q0OOOQ0):TBytes;
function O0OOC0C0Q0(const OOOOC0C0Q0:TBytes):TBytes;
function OOCOC0C0Q0(const OQCOC0C0Q0,OCCOC0C0Q0:TBytes):boolean;
function OQQQQ0C0Q0(const OCQQQ0C0Q0:TBytes;O0CQQ0C0Q0:OOOOCOQOQ0;OOCQQ0C0Q0:OQ0Q0OOOQ0):TBytes;
function OCCQQ0C0Q0(const O00CQ0C0Q0,OO0CQ0C0Q0:TBytes;OQ0CQ0C0Q0:OOOOCOQOQ0;OC0CQ0C0Q0:OQ0Q0OOOQ0):boolean;
function OOOCQ0C0Q0(const OQOCQ0C0Q0:TBytes):TBytes;
function OCOCQ0C0Q0(const O0QCQ0C0Q0,OOQCQ0C0Q0:TBytes):boolean;
class function OQQCQ0C0Q0(const OCQCQ0C0Q0,O0CCQ0C0Q0:OOCC0O0OQ0;OOCCQ0C0Q0:boolean):boolean;
class function OQCCQ0C0Q0(const OCCCQ0C0Q0,O000Q0C0Q0:OC000O0OQ0;OO00Q0C0Q0:boolean):boolean;
class function OQ00Q0C0Q0(const OC00Q0C0Q0,O0O0Q0C0Q0:OCOQQO0OQ0;OOO0Q0C0Q0:boolean):boolean;
function OQO0Q0C0Q0:OOQQQOQOQ0;
function OCO0Q0C0Q0:integer;
function O0Q0Q0C0Q0:integer;
function OOQ0Q0C0Q0:boolean;
function OQQ0Q0C0Q0:TBytes;
procedure OCQ0Q0C0Q0(const O0C0Q0C0Q0:TBytes);
procedure OOC0Q0C0Q0(OQC0Q0C0Q0:OC0Q0OOOQ0);
procedure OCC0Q0C0Q0(O00OQ0C0Q0:OCOC0OOOQ0);
protected
OO0OQ0C0Q0:OOCC0O0OQ0;
OQ0OQ0C0Q0:OC000O0OQ0;
OC0OQ0C0Q0:OCOQQO0OQ0;
procedure O0OOQ0C0Q0(OOOOQ0C0Q0:TObject);
procedure OQOOQ0C0Q0(OCOOQ0C0Q0:TObject);
procedure OQCQ0CC0Q0(OCCQ0CC0Q0:O0000CC0Q0);override;
procedure OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);override;
procedure O0OC0CC0Q0;override;
procedure OCCOQ0C0Q0(const O00QOCQ0Q0:TBytes;
const OO0QOCQ0Q0:OCQCQ00OQ0;const OQ0QOCQ0Q0:OOQQQOQOQ0;const OC0QOCQ0Q0:boolean);
function O0OQOCQ0Q0:TBytes;
procedure OOOQOCQ0Q0(const OQOQOCQ0Q0:TBytes);
procedure OO0C0CC0Q0;override;
public
constructor Create(OOQC0CC0Q0:O0000CC0Q0=nil);override;
destructor Destroy;override;
procedure OOQQOCQ0Q0(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string;out OQQQOCQ0Q0:string);overload;
procedure OOQQOCQ0Q0(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string='');overload;
procedure OOQQOCQ0Q0(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string;out OQQQOCQ0Q0:string);overload;
procedure OOQQOCQ0Q0(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string='');overload;
procedure O0OCOCQ0Q0(const OQ0CCQC0Q0:string;const OOOCOCQ0Q0:boolean;const OQOCOCQ0Q0:string;
const OCOCOCQ0Q0:OO00COQOQ0=OOOCCOQOQ0;const O0QCOCQ0Q0:OQOCQ00OQ0=O00CQ00OQ0;const OOQCOCQ0Q0:string='');overload;
procedure O0OCOCQ0Q0(O0OCCQC0Q0:TStream;const OOOCOCQ0Q0:boolean;const OQOCOCQ0Q0:string;
const OCOCOCQ0Q0:OO00COQOQ0=OOOCCOQOQ0;const O0QCOCQ0Q0:OQOCQ00OQ0=O00CQ00OQ0;const OOQCOCQ0Q0:string='');overload;
procedure OQCCOCQ0Q0(const OCCCOCQ0Q0:OOQQQOQOQ0;const O000OCQ0Q0:integer;OO00OCQ0Q0:IScRandom=nil);
procedure OQ00OCQ0Q0(const OC00OCQ0Q0:OOQOQOQOQ0;O0O0OCQ0Q0:IScRandom=nil);
procedure OOO0OCQ0Q0(const OQQQCQC0Q0:OOOOCOQOQ0;out OCQQCQC0Q0:TBytes);overload;
procedure OOO0OCQ0Q0(const OQQQCQC0Q0:OOOOCOQOQ0;out OCQQCQC0Q0:string);overload;
function Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;reintroduce;override;
function O0Q0OCQ0Q0(const OOOCCQC0Q0:TBytes;OQOCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OCOCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):TBytes;
function OCQ0OCQ0Q0(const OQC0CO0OQ0:TBytes;O0QCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OOQCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):TBytes;
function O0C0OCQ0Q0(const OQQCCQC0Q0,OCQCCQC0Q0:TBytes;O0CCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OOCCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):boolean;
function OCC0OCQ0Q0(const OQCCCQC0Q0,OCCCCQC0Q0:TBytes;O000CQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OO00CQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):boolean;
function O00OOCQ0Q0(const OQ00CQC0Q0:TBytes;OO0OOCQ0Q0:OQ0Q0OOOQ0=OQCOOOOOQ0):TBytes;
function OC0OOCQ0Q0(const OC00CQC0Q0:TBytes;O0OOOCQ0Q0:OQ0Q0OOOQ0=OQCOOOOOQ0):TBytes;
property OQOOOCQ0Q0:OOQQQOQOQ0 read OQO0Q0C0Q0;
property OCOOOCQ0Q0:integer read O0Q0Q0C0Q0;
property O0QOOCQ0Q0:boolean read OOQ0Q0C0Q0;
property OOQOOCQ0Q0:string read OCOQ0CC0Q0 write O0CQ0CC0Q0;
property OQQOOCQ0Q0:OQCCCCC0Q0 read OO0COOC0Q0 write OQ0COOC0Q0;
property OCQOOCQ0Q0:OCOQQO0OQ0 read OC0OQ0C0Q0;
property O0COOCQ0Q0:OOCC0O0OQ0 read OO0OQ0C0Q0;
property OOCOOCQ0Q0:OC000O0OQ0 read OQ0OQ0C0Q0;
property OQCOOCQ0Q0:OC0Q0OOOQ0 read OCCQOOC0Q0 write OOC0Q0C0Q0;
property OCCOOCQ0Q0:OCOC0OOOQ0 read O00COOC0Q0 write OCC0Q0C0Q0;
end;
O0000CC0Q0=class
protected
{$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
OO000CC0Q0:OCCCQ00OQ0;
OQ000CC0Q0:TCriticalSection;
OC000CC0Q0:TStringList;
function O0O00CC0Q0:integer;
procedure OOO00CC0Q0;
function OQO00CC0Q0:OCCC0CC0Q0;virtual;abstract;
function OCO00CC0Q0:string;virtual;abstract;
procedure O0Q00CC0Q0(OOQ00CC0Q0:O0OQ0CC0Q0);
procedure OQQ00CC0Q0(OCQ00CC0Q0:O0OQ0CC0Q0);
procedure O0C00CC0Q0(OOC00CC0Q0:O0OQ0CC0Q0);
procedure OQC00CC0Q0(OCC00CC0Q0:O0OQ0CC0Q0;const O00O0CC0Q0:string);
procedure OO0O0CC0Q0(OQ0O0CC0Q0:O0OQ0CC0Q0;const OC0O0CC0Q0:string);
procedure O0OO0CC0Q0(const OOOO0CC0Q0:string);
function OQOO0CC0Q0(OCOO0CC0Q0:integer):O0OQ0CC0Q0;
procedure O0QO0CC0Q0(OOQO0CC0Q0:integer;OQQO0CC0Q0:O0OQ0CC0Q0);
procedure OCQO0CC0Q0(O0CO0CC0Q0:O0OQ0CC0Q0;const OOCO0CC0Q0:string);
function OCCO0CC0Q0(const O00QCCC0Q0:string):O0OQ0CC0Q0;
function OQ0QCCC0Q0(const OC0QCCC0Q0:string):O0OQ0CC0Q0;
procedure O0OQCCC0Q0(OOOQCCC0Q0:TStrings);
function OCOQCCC0Q0(O0QQCCC0Q0:O0OQ0CC0Q0):boolean;
property OQQQCCC0Q0[Index:integer]:O0OQ0CC0Q0 read OQOO0CC0Q0 write O0QO0CC0Q0;
public
constructor Create(O0CQCCC0Q0:OCCCQ00OQ0);virtual;
destructor Destroy;override;
procedure OQCQCCC0Q0(OCCQCCC0Q0:O0OQ0CC0Q0);
function OO0CCCC0Q0(OQ0CCCC0Q0:O0OQ0CC0Q0):integer;
procedure OC0CCCC0Q0(O0OCCCC0Q0:O0OQ0CC0Q0);
procedure OQOCCCC0Q0;
procedure O0QCCCC0Q0;
procedure OCQCCCC0Q0;virtual;
property O0CCCCC0Q0:integer read O0O00CC0Q0;
property OOCCCCC0Q0:OCCCQ00OQ0 read OO000CC0Q0;
end;
OQCCCCC0Q0=class(O0000CC0Q0)
protected
function OCCCCCC0Q0(O000CCC0Q0:integer):O0CQOOC0Q0;
procedure OO00CCC0Q0(OQ00CCC0Q0:integer;OC00CCC0Q0:O0CQOOC0Q0);
function OQO00CC0Q0:OCCC0CC0Q0;override;
function OCO00CC0Q0:string;override;
public
procedure O0O0CCC0Q0(const OOO0CCC0Q0:string);
function OQO0CCC0Q0(const OCO0CCC0Q0:string):O0CQOOC0Q0;
function O0Q0CCC0Q0(const OOQ0CCC0Q0:string):O0CQOOC0Q0;
procedure OQQ0CCC0Q0(OCQ0CCC0Q0:TStrings);
property O0C0CCC0Q0[Index:integer]:O0CQOOC0Q0 read OCCCCCC0Q0 write OO00CCC0Q0;default;
end;
OCCCQ00OQ0=class(TComponent)
private
O000Q00OQ0:TCriticalSection;
OO00Q00OQ0:OQCCCCC0Q0;
OQ00Q00OQ0:OOC0CCC0Q0;
OC00Q00OQ0:O0COCCC0Q0;
O0O0Q00OQ0:OQCQQCC0Q0;
class function OOO0Q00OQ0(const OQO0Q00OQ0:OO00COQOQ0;const OCO0Q00OQ0:string):TSymmetricAlgorithm;
protected
O0C0Q00OQ0:boolean;
OOC0Q00OQ0:boolean;
procedure OQC0Q00OQ0(const OCC0Q00OQ0:Boolean);
function O00OQ00OQ0:OQCCCCC0Q0;virtual;
function OO0OQ00OQ0:OOC0CCC0Q0;virtual;
function OQ0OQ00OQ0:O0COCCC0Q0;virtual;
function OC0OQ00OQ0:OQCQQCC0Q0;virtual;
procedure O0OOQ00OQ0;virtual;
procedure OOOOQ00OQ0;
procedure OQOOQ00OQ0(OCOOQ00OQ0:O0OQ0CC0Q0);
procedure O0QOQ00OQ0(OOQOQ00OQ0:O0OQ0CC0Q0);virtual;abstract;
procedure OQQOQ00OQ0(OCQOQ00OQ0:O0OQ0CC0Q0);
procedure O0COQ00OQ0(OOCOQ00OQ0:O0OQ0CC0Q0);virtual;abstract;
procedure OQCOQ00OQ0(OCCOQ00OQ0:O0OQ0CC0Q0);
procedure O00QOCC0Q0(OO0QOCC0Q0:O0OQ0CC0Q0);virtual;abstract;
procedure OQ0QOCC0Q0(OC0QOCC0Q0:O0OQ0CC0Q0;const O0OQOCC0Q0:string;OOOQOCC0Q0:boolean=True);
procedure OQOQOCC0Q0(OCOQOCC0Q0:O0OQ0CC0Q0;const O0QQOCC0Q0:string;OOQQOCC0Q0:boolean);virtual;abstract;
procedure OQQQOCC0Q0(const OCQQOCC0Q0:OCCC0CC0Q0;O0CQOCC0Q0:TStrings);
procedure OOCQOCC0Q0(const OQCQOCC0Q0:OCCC0CC0Q0;OCCQOCC0Q0:TStrings);virtual;abstract;
procedure O00COCC0Q0;virtual;abstract;
procedure OO0COCC0Q0;virtual;
class procedure OQ0COCC0Q0(OC0COCC0Q0:TStream;O0OCOCC0Q0:O0OQ0CC0Q0;const OOOCOCC0Q0:OO00COQOQ0;const OQOCOCC0Q0:string);
class procedure O000OCC0Q0(OO00OCC0Q0:TStream;OQ00OCC0Q0:O0OQ0CC0Q0;const OC00OCC0Q0:OO00COQOQ0;const O0O0OCC0Q0:string);
public
constructor Create(OQC0OCC0Q0:TComponent);override;
destructor Destroy;override;
procedure DeleteStorage;
procedure O00OOCC0Q0(const OO0OOCC0Q0:string;const OQ0OOCC0Q0:string='');overload;
procedure O00OOCC0Q0(OC0OOCC0Q0:TStream;const OQ0OOCC0Q0:string='');overload;
procedure O00OOCC0Q0(O0OOOCC0Q0:OOC0QCC0Q0);overload;
property OOCOOCC0Q0:OQCCCCC0Q0 read O00OQ00OQ0;
property OQCOOCC0Q0:OOC0CCC0Q0 read OO0OQ00OQ0;
property OCCOOCC0Q0:O0COCCC0Q0 read OQ0OQ00OQ0;
property O00Q0CC0Q0:OQCQQCC0Q0 read OC0OQ00OQ0;
published
property OO0Q0CC0Q0:boolean read O0C0Q00OQ0 write OQC0Q00OQ0 default False;
property OQ0Q0CC0Q0:boolean read OOC0Q00OQ0 write OOC0Q00OQ0 default True;
end;
{$IFDEF VER16P}
[ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
O00Q0CQ0Q0=class(OCCCQ00OQ0)
protected
procedure O0QOQ00OQ0(OOQOQ00OQ0:O0OQ0CC0Q0);override;
procedure O0COQ00OQ0(OOCOQ00OQ0:O0OQ0CC0Q0);override;
procedure O00QOCC0Q0(OO0QOCC0Q0:O0OQ0CC0Q0);override;
procedure OQOQOCC0Q0(OCOQOCC0Q0:O0OQ0CC0Q0;const O0QQOCC0Q0:string;OOQQOCC0Q0:boolean);override;
procedure OOCQOCC0Q0(const OQCQOCC0Q0:OCCC0CC0Q0;OCCQOCC0Q0:TStrings);override;
procedure O00COCC0Q0;override;
end;
{$IFDEF VER16P}
[ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
OO0Q0CQ0Q0=class(OCCCQ00OQ0)
private
OQ0Q0CQ0Q0:string;
OC0Q0CQ0Q0:OO00COQOQ0;
O0OQ0CQ0Q0:string;
OOOQ0CQ0Q0:string;
OQOQ0CQ0Q0:string;
OCOQ0CQ0Q0:string;
O0QQ0CQ0Q0:string;
procedure OOQQ0CQ0Q0(const OQQQ0CQ0Q0:string);
procedure OCQQ0CQ0Q0(const O0CQ0CQ0Q0:string);
procedure OOCQ0CQ0Q0(const OQCQ0CQ0Q0:string);
procedure OCCQ0CQ0Q0(const O00C0CQ0Q0:string);
procedure OO0C0CQ0Q0(const OQ0C0CQ0Q0:string);
function OC0C0CQ0Q0:string;
function O0OC0CQ0Q0(OOOC0CQ0Q0:OCCC0CC0Q0):string;
procedure OQOC0CQ0Q0(OCOC0CQ0Q0:O0OQ0CC0Q0;out O0QC0CQ0Q0,OOQC0CQ0Q0:string);
function OQQC0CQ0Q0(OCQC0CQ0Q0:O0OQ0CC0Q0;O0CC0CQ0Q0:boolean=False):string;
protected
procedure AssignTo(OO000CQ0Q0:TPersistent);override;
procedure DefineProperties(OC000CQ0Q0:TFiler);override;
procedure O0O00CQ0Q0(OOO00CQ0Q0:TReader);
procedure OQO00CQ0Q0(OCO00CQ0Q0:TWriter);
procedure O0Q00CQ0Q0(OOQ00CQ0Q0:TReader);
procedure OQQ00CQ0Q0(OCQ00CQ0Q0:TWriter);
procedure O0C00CQ0Q0(OOC00CQ0Q0:TReader);
procedure OQC00CQ0Q0(OCC00CQ0Q0:TWriter);
procedure O00O0CQ0Q0(OO0O0CQ0Q0:TReader);
procedure OQ0O0CQ0Q0(OC0O0CQ0Q0:TWriter);
procedure O0OO0CQ0Q0(OOOO0CQ0Q0:TReader);
procedure OQOO0CQ0Q0(OCOO0CQ0Q0:TWriter);
procedure O0QOQ00OQ0(OOQOQ00OQ0:O0OQ0CC0Q0);override;
procedure O0COQ00OQ0(OOCOQ00OQ0:O0OQ0CC0Q0);override;
procedure O00QOCC0Q0(OO0QOCC0Q0:O0OQ0CC0Q0);override;
procedure OQOQOCC0Q0(OCOQOCC0Q0:O0OQ0CC0Q0;const O0QQOCC0Q0:string;OOQQOCC0Q0:boolean);override;
procedure OOCQOCC0Q0(const OQCQOCC0Q0:OCCC0CC0Q0;OCCQOCC0Q0:TStrings);override;
procedure O00COCC0Q0;override;
public
constructor Create(OQC0OCC0Q0:TComponent);override;
procedure OOQQCCQ0Q0(const OQQQCCQ0Q0:string);
published
property OCOCCCQ0Q0:OO00COQOQ0 read OC0Q0CQ0Q0 write OC0Q0CQ0Q0 default OOOCCOQOQ0;
property O0QCCCQ0Q0:string read OQ0Q0CQ0Q0 write OQ0Q0CQ0Q0;
property OOQCCCQ0Q0:string read O0OQ0CQ0Q0 write OOQQ0CQ0Q0 stored False;
property OQQCCCQ0Q0:string read OOOQ0CQ0Q0 write OCQQ0CQ0Q0 stored False;
property OCQCCCQ0Q0:string read OQOQ0CQ0Q0 write OOCQ0CQ0Q0 stored False;
property O0CCCCQ0Q0:string read OCOQ0CQ0Q0 write OCCQ0CQ0Q0 stored False;
property OOCCCCQ0Q0:string read O0QQ0CQ0Q0 write OO0C0CQ0Q0 stored False;
end;
{$IFDEF MSWINDOWS}
{$IFDEF VER16P}
[ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
OQCCCCQ0Q0=class(OCCCQ00OQ0)
private
OCCCCCQ0Q0:OO00COQOQ0;
O000CCQ0Q0:string;
OO00CCQ0Q0:string;
OQ00CCQ0Q0:TRegistry;
procedure OC00CCQ0Q0(const O0O0CCQ0Q0:string);
procedure OOO0CCQ0Q0(const OQO0CCQ0Q0:NativeUint);
function OCO0CCQ0Q0:NativeUint;
protected
procedure AssignTo(OO000CQ0Q0:TPersistent);override;
procedure DefineProperties(OC000CQ0Q0:TFiler);override;
procedure OQQ0CCQ0Q0(OCQ0CCQ0Q0:TReader);
procedure O0C0CCQ0Q0(OOC0CCQ0Q0:TWriter);
function OQC0CCQ0Q0(OCC0CCQ0Q0:OCCC0CC0Q0):string;
procedure O00OCCQ0Q0(OO0OCCQ0Q0:O0OQ0CC0Q0;out OQ0OCCQ0Q0,OC0OCCQ0Q0:string);
procedure O0QOQ00OQ0(OOQOQ00OQ0:O0OQ0CC0Q0);override;
procedure O0COQ00OQ0(OOCOQ00OQ0:O0OQ0CC0Q0);override;
procedure O00QOCC0Q0(OO0QOCC0Q0:O0OQ0CC0Q0);override;
procedure OQOQOCC0Q0(OCOQOCC0Q0:O0OQ0CC0Q0;const O0QQOCC0Q0:string;OOQQOCC0Q0:boolean);override;
procedure OOCQOCC0Q0(const OQCQOCC0Q0:OCCC0CC0Q0;OCCQOCC0Q0:TStrings);override;
procedure O00COCC0Q0;override;
public
constructor Create(OQC0OCC0Q0:TComponent);override;
destructor Destroy;override;
procedure OQCOCCQ0Q0(const OQQQCCQ0Q0:string);
published
property OC0QQCQ0Q0:OO00COQOQ0 read OCCCCCQ0Q0 write OCCCCCQ0Q0 default OOOCCOQOQ0;
property O0OQQCQ0Q0:string read O000CCQ0Q0 write O000CCQ0Q0;
{$HPPEMIT '#ifdef KeyPath'}
{$HPPEMIT '#undef KeyPath'}
{$HPPEMIT '#endif'}
property OOOQQCQ0Q0:string read OO00CCQ0Q0 write OC00CCQ0Q0 stored False;
property OQOQQCQ0Q0:NativeUint read OCO0CCQ0Q0 write OOO0CCQ0Q0 default HKEY_CURRENT_USER;
end;
{$ENDIF}
OQQQQCQ0Q0=(OCOQQCQ0Q0,O0QQQCQ0Q0,OOQQQCQ0Q0);
TScUserAuthentications=set of OQQQQCQ0Q0;
OCQQQCQ0Q0=class(O0OQ0CC0Q0)
private
O0CQQCQ0Q0:string;
OOCQQCQ0Q0:string;
OQCQQCQ0Q0:string;
OCCQQCQ0Q0:TScSSHChannelPermissions;
O00CQCQ0Q0:TScUserAuthentications;
OO0CQCQ0Q0:string;
OQ0CQCQ0Q0:string;
OC0CQCQ0Q0:O0CQOOC0Q0;
function O0OCQCQ0Q0:OOC0CCC0Q0;
procedure OOOCQCQ0Q0(OQOCQCQ0Q0:OOC0CCC0Q0);
procedure OCOCQCQ0Q0(const O0QCQCQ0Q0:string);
procedure OOQCQCQ0Q0(const OQQCQCQ0Q0:string);
procedure OCQCQCQ0Q0(const O0CCQCQ0Q0:string);
procedure OOCCQCQ0Q0(const OQCCQCQ0Q0:TScSSHChannelPermissions);
procedure OCCCQCQ0Q0(const O000QCQ0Q0:string);
procedure OQ00QCQ0Q0(const OC00QCQ0Q0:TScUserAuthentications);
procedure O0O0QCQ0Q0(OC00CCC0Q0:O0CQOOC0Q0);
procedure OOO0QCQ0Q0;
protected
procedure OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);override;
procedure O0CQ0CC0Q0(const OOCQ0CC0Q0:string);override;
procedure OQO0QCQ0Q0(const OCO0QCQ0Q0:TBytes);
function OQQ0QCQ0Q0:TBytes;
procedure OQC0QCQ0Q0(const OCC0QCQ0Q0:TBytes);
function OQOOQCQ0Q0:TBytes;
public
constructor Create(OOQC0CC0Q0:O0000CC0Q0=nil);override;
destructor Destroy;override;
function Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;reintroduce;override;
property OCQOQCQ0Q0:string read OCOQ0CC0Q0 write O0CQ0CC0Q0;
property O0COQCQ0Q0:string read O0CQQCQ0Q0 write OCOCQCQ0Q0;
property OOCOQCQ0Q0:string read OOCQQCQ0Q0 write OOQCQCQ0Q0;
property OQCOQCQ0Q0:TScSSHChannelPermissions read OCCQQCQ0Q0 write OOCCQCQ0Q0;
property OCCOQCQ0Q0:string read OQCQQCQ0Q0 write OCQCQCQ0Q0;
property O00QOQQ0Q0:TScUserAuthentications read O00CQCQ0Q0 write OQ00QCQ0Q0;
property OO0QOQQ0Q0:string read OQ0CQCQ0Q0;
property OQ0QOQQ0Q0:string read OO0CQCQ0Q0 write OCCCQCQ0Q0;
property OC0QOQQ0Q0:O0CQOOC0Q0 read OC0CQCQ0Q0 write O0O0QCQ0Q0;
property O0OQOQQ0Q0:OOC0CCC0Q0 read O0OCQCQ0Q0 write OOOCQCQ0Q0;
end;
OOC0CCC0Q0=class(O0000CC0Q0)
protected
function OQC0CCC0Q0(OCC0CCC0Q0:integer):OCQQQCQ0Q0;
procedure O00OCCC0Q0(OO0OCCC0Q0:integer;OQ0OCCC0Q0:OCQQQCQ0Q0);
function OQO00CC0Q0:OCCC0CC0Q0;override;
function OCO00CC0Q0:string;override;
public
procedure OC0OCCC0Q0(const O0OOCCC0Q0:string);
function OOOOCCC0Q0(const OQOOCCC0Q0:string):OCQQQCQ0Q0;
function OCOOCCC0Q0(const O0QOCCC0Q0:string):OCQQQCQ0Q0;
procedure OOQOCCC0Q0(OQQOCCC0Q0:TStrings);
property OCQOCCC0Q0[Index:integer]:OCQQQCQ0Q0 read OQC0CCC0Q0 write O00OCCC0Q0;default;
end;
TScCertificateStatus=(
csValid,
csOtherError,csExpired,
csInsecureSignature,csForbiddenSignature,csInvalidPolicies,
csUnknownCriticalExtension,
csCRLNotFound,csCRLIsNotValid,csCertificateIsRevoked,
csInvalidBasicConstraints,csInvalidKeyUsage,csIssuerNotEqualSubject,
csInvalidSubjectName,csUntrustedRoot,csInvalidSignature
);
TScCertificateStatusSet=set of TScCertificateStatus;
OOOQOQQ0Q0=procedure(OQOQOQQ0Q0:TObject;
OCOQOQQ0Q0:OCOC00OOQ0;O0QQOQQ0Q0:boolean;out OOQQOQQ0Q0:OCQOCQC0Q0)of object;
OQQQOQQ0Q0=procedure(OCQQOQQ0Q0:IntPtr)of object;
TScCertificate=class(O0OQ0CC0Q0)
private
OOQOOQC0Q0:OQCOCQOOQ0;
OQQOOQC0Q0:TBytes;
OCQOOQC0Q0:IntPtr;
O0COOQC0Q0:OQOCO0OOQ0;
OOCOOQC0Q0:OQOCO0OOQ0;
OQCOOQC0Q0:OQ0O0C0OQ0;
OCCOOQC0Q0:OQ0COC0OQ0;
O00Q0QC0Q0:O0CQOOC0Q0;
OO0Q0QC0Q0:O0CQOOC0Q0;
OQ0Q0QC0Q0:OOOCCOOOQ0;
OC0Q0QC0Q0:OQ0OQ0OOQ0;
O0OQ0QC0Q0:OQQQOQQ0Q0;
OOOQ0QC0Q0:integer;
function OQOQ0QC0Q0:O0COCCC0Q0;
procedure OCOQ0QC0Q0(O0QQ0QC0Q0:O0COCCC0Q0);
class function OOQQ0QC0Q0(const OQQQ0QC0Q0,OCQQ0QC0Q0:string):string;
function O00C0QC0Q0:string;
function OO0C0QC0Q0:string;
procedure OQ0C0QC0Q0;
procedure OC0C0QC0Q0;
class function O0OC0QC0Q0(OOOC0QC0Q0:OQOCO0OOQ0):string;
function OCOC0QC0Q0:OQOCO0OOQ0;
function O0QC0QC0Q0:OQOCO0OOQ0;
function OOQC0QC0Q0:TDateTime;
function OQQC0QC0Q0:TDateTime;
function OCQC0QC0Q0:string;
function O0CC0QC0Q0:OOOCCOOOQ0;
function OOCC0QC0Q0:TBytes;
function OQCC0QC0Q0:integer;
procedure OCCC0QC0Q0;
function O0000QC0Q0:OQ0O0C0OQ0;
function OO000QC0Q0:string;
function O0O00QC0Q0:O0CQOOC0Q0;
function OCO00QC0Q0:boolean;
protected
procedure O0Q00QC0Q0(OOQ00QC0Q0:IntPtr;OQQ00QC0Q0:OQQQOQQ0Q0);
procedure OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);override;
procedure O0OC0CC0Q0;override;
procedure OCQ00QC0Q0;
class procedure O0C00QC0Q0(OOC00QC0Q0:OOOCCOOOQ0;
out OQC00QC0Q0:OOOOCOQOQ0;out OCC00QC0Q0:OQ0Q0OOOQ0);
function O00O0QC0Q0(OO0O0QC0Q0:TScCertificate):boolean;
function OQOO0QC0Q0(OCOO0QC0Q0,O0QO0QC0Q0:OQQOC0OOQ0):boolean;
procedure OOCO0QC0Q0(OQCO0QC0Q0:OOOQOQQ0Q0;OCCO0QC0Q0:OCOC00OOQ0;
O00QCQC0Q0:OCQOCQC0Q0;OO0QCQC0Q0:TScCertificate;var OQ0QCQC0Q0:TScCertificateStatusSet);
procedure OOOQCQC0Q0;
public
constructor Create(OOQC0CC0Q0:O0000CC0Q0=nil);override;
procedure SetRawData(OQOQCQC0Q0:IntPtr;OCOQCQC0Q0:OQQQOQQ0Q0;const O0QQCQC0Q0:TBytes);
function GetRawData:TBytes;
function Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;reintroduce;override;
procedure GetFingerprint(const OQQQCQC0Q0:OOOOCOQOQ0;out OCQQCQC0Q0:TBytes);overload;
procedure GetFingerprint(const OQQQCQC0Q0:OOOOCOQOQ0;out OCQQCQC0Q0:string);overload;
procedure ImportFrom(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string='');overload;
procedure ImportFrom(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string='');overload;
procedure ExportTo(const OQ0CCQC0Q0:string;const OC0CCQC0Q0:OQCCQ00OQ0=OOCCQ00OQ0);overload;
procedure ExportTo(O0OCCQC0Q0:TStream;const OC0CCQC0Q0:OQCCQ00OQ0=OOCCQ00OQ0);overload;
function Sign(const OOOCCQC0Q0:TBytes;OQOCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OCOCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):TBytes;
function SignHash(const OQC0CO0OQ0:TBytes;O0QCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OOQCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):TBytes;
function VerifySign(const OQQCCQC0Q0,OCQCCQC0Q0:TBytes;O0CCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OOCCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):boolean;
function VerifyHashSign(const OQCCCQC0Q0,OCCCCQC0Q0:TBytes;O000CQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OO00CQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):boolean;
function Encrypt(const OQ00CQC0Q0:TBytes):TBytes;
function Decrypt(const OC00CQC0Q0:TBytes):TBytes;
procedure VerifyCertificateChain(O0O0CQC0Q0:OOOQOQQ0Q0;OOO0CQC0Q0:TScCertificate;var OQO0CQC0Q0:TScCertificateStatusSet);
procedure VerifyCRLRevocation(OCC0CQC0Q0:OOOQOQQ0Q0;O00OCQC0Q0:TScCertificate;var OO0OCQC0Q0:TScCertificateStatusSet);
property Issuer:string read O00C0QC0Q0;
property IssuerName:OQOCO0OOQ0 read OCOC0QC0Q0;
property Subject:string read OO0C0QC0Q0;
property SubjectName:OQOCO0OOQ0 read O0QC0QC0Q0;
property NotAfter:TDateTime read OOQC0QC0Q0;
property NotBefore:TDateTime read OQQC0QC0Q0;
property SerialNumber:string read OCQC0QC0Q0;
property SignatureAlgorithm:OOOCCOOOQ0 read O0CC0QC0Q0;
property Signature:TBytes read OOCC0QC0Q0;
property Version:integer read OQCC0QC0Q0;
property Extensions:OQ0O0C0OQ0 read O0000QC0Q0;
property SubjectKeyIdentifier:string read OO000QC0Q0;
property CRLReason:OQ0OQ0OOQ0 read OC0Q0QC0Q0;
property IsSelfSigned:boolean read OCO00QC0Q0;
property Handle:IntPtr read OCQOOQC0Q0;
property Key:O0CQOOC0Q0 read O0O00QC0Q0;
property CertName:string read OCOQ0CC0Q0 write O0CQ0CC0Q0;
property CertificateList:O0COCCC0Q0 read OQOQ0QC0Q0 write OCOQ0QC0Q0;
end;
O0COCCC0Q0=class(O0000CC0Q0)
protected
function OOCOCCC0Q0(OQCOCCC0Q0:integer):TScCertificate;
procedure OCCOCCC0Q0(O00QQCC0Q0:integer;OO0QQCC0Q0:TScCertificate);
function OQO00CC0Q0:OCCC0CC0Q0;override;
function OCO00CC0Q0:string;override;
public
procedure OQ0QQCC0Q0(const OC0QQCC0Q0:string);
function O0OQQCC0Q0(const OOOQQCC0Q0:string):TScCertificate;
function OQOQQCC0Q0(const OCOQQCC0Q0:string):TScCertificate;
procedure O0QQQCC0Q0(OOQQQCC0Q0:TStrings);
function OQQQQCC0Q0(OCQQQCC0Q0:OQOCO0OOQ0):TScCertificate;
property OOCQQCC0Q0[Index:integer]:TScCertificate read OOCOCCC0Q0 write OCCOCCC0Q0;default;
end;
TScCertificateChain=class
public
class procedure VerifyChain(O0CQOQQ0Q0:OOOQOQQ0Q0;OOCQOQQ0Q0:TCRList;out OQCQOQQ0Q0:TScCertificateStatusSet);
end;
O00COQQ0Q0=class(OQ0Q00QOQ0)
private
OO0COQQ0Q0:string;
OQ0COQQ0Q0:TDateTime;
OC0COQQ0Q0:OQ0O0C0OQ0;
O0OCOQQ0Q0:TBytes;
function OOOCOQQ0Q0:OQ0O0C0OQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create;
destructor Destroy;override;
procedure OCOQ00QOQ0(O000CO0OQ0:OQ0Q00QOQ0);override;
property OQQCOQQ0Q0:string read OO0COQQ0Q0;
property OCQCOQQ0Q0:TDateTime read OQ0COQQ0Q0;
property O0CCOQQ0Q0:OQ0O0C0OQ0 read OOOCOQQ0Q0;
end;
OOCCOQQ0Q0=class(OOQQ00QOQ0)
private
function OQCCOQQ0Q0(OCCCOQQ0Q0:integer):O00COQQ0Q0;
procedure O000OQQ0Q0(OO00OQQ0Q0:integer;OQ00OQQ0Q0:O00COQQ0Q0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property OC00OQQ0Q0[Index:integer]:O00COQQ0Q0 read OQCCOQQ0Q0 write O000OQQ0Q0;default;
end;
OCQOCQC0Q0=class(O0OQ0CC0Q0)
private
O0COCQC0Q0:OQCOCQOOQ0;
OOCOCQC0Q0:TBytes;
OQCOCQC0Q0:boolean;
OCCOCQC0Q0:OOOCCOOOQ0;
O00QQQC0Q0:OQOCO0OOQ0;
OO0QQQC0Q0:OQ0O0C0OQ0;
OQ0QQQC0Q0:OOCCOQQ0Q0;
OC0QQQC0Q0:TStringList;
function O0OQQQC0Q0:OQCQQCC0Q0;
procedure OOOQQQC0Q0(OQOQQQC0Q0:OQCQQCC0Q0);
function OCOQQQC0Q0:integer;
function O0QQQQC0Q0:TBytes;
function OOQQQQC0Q0:OOOCCOOOQ0;
function OQQQQQC0Q0:string;
function OCQQQQC0Q0:OQOCO0OOQ0;
function O0CQQQC0Q0:TDateTime;
function OOCQQQC0Q0:TDateTime;
function OQCQQQC0Q0:OQ0O0C0OQ0;
function OCCQQQC0Q0:OOCCOQQ0Q0;
procedure O00CQQC0Q0;
procedure OO0CQQC0Q0;
protected
procedure OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);override;
procedure O0OC0CC0Q0;override;
function OCOCQQC0Q0(OO0O0QC0Q0:TScCertificate):boolean;
procedure O0QCQQC0Q0(const O0QQCQC0Q0:TBytes);
public
constructor Create(OOQC0CC0Q0:O0000CC0Q0=nil);override;
function Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;reintroduce;override;
function OQQCQQC0Q0(OCQCQQC0Q0:TScCertificate;O0CCQQC0Q0:OC0OQ0OOQ0):boolean;overload;
function OQQCQQC0Q0(OOO0QQC0Q0:OCQOCQC0Q0):boolean;overload;
function OOQ0QQC0Q0(OQQ0QQC0Q0:TScCertificate;out OCQ0QQC0Q0:OQ0OQ0OOQ0):boolean;
procedure OQOOQQC0Q0(OCOOQQC0Q0:TScCertificate;var O0QOQQC0Q0:TScCertificateStatusSet);
procedure OQQOQQC0Q0(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string='');overload;
procedure OQQOQQC0Q0(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string='');overload;
procedure OCQOQQC0Q0(const OQ0CCQC0Q0:string;const OC0CCQC0Q0:OQCCQ00OQ0=OOCCQ00OQ0);overload;
procedure OCQOQQC0Q0(O0OCCQC0Q0:TStream;const OC0CCQC0Q0:OQCCQ00OQ0=OOCCQ00OQ0);overload;
class function O0COQQC0Q0(const OOCOQQC0Q0:string):OCQOCQC0Q0;
property OO0QOOC0Q0:integer read OCOQQQC0Q0;
property OQ0QOOC0Q0:TBytes read O0QQQQC0Q0;
property OC0QOOC0Q0:OOOCCOOOQ0 read OOQQQQC0Q0;
property O0OQOOC0Q0:string read OQQQQQC0Q0;
property OOOQOOC0Q0:OQOCO0OOQ0 read OCQQQQC0Q0;
property OQOQOOC0Q0:TDateTime read O0CQQQC0Q0;
property OCOQOOC0Q0:TDateTime read OOCQQQC0Q0;
property O0QQOOC0Q0:OQ0O0C0OQ0 read OQCQQQC0Q0;
property OOQQOOC0Q0:OOCCOQQ0Q0 read OCCQQQC0Q0;
property OQQQOOC0Q0:string read OCOQ0CC0Q0 write O0CQ0CC0Q0;
property OCQQOOC0Q0:OQCQQCC0Q0 read O0OQQQC0Q0 write OOOQQQC0Q0;
end;
OQCQQCC0Q0=class(O0000CC0Q0)
protected
function OCCQQCC0Q0(O00CQCC0Q0:integer):OCQOCQC0Q0;
procedure OO0CQCC0Q0(OQ0CQCC0Q0:integer;OC0CQCC0Q0:OCQOCQC0Q0);
function OQO00CC0Q0:OCCC0CC0Q0;override;
function OCO00CC0Q0:string;override;
public
class procedure O0OCQCC0Q0(OOOCQCC0Q0:TObject;OQOCQCC0Q0:OCCCQ00OQ0;
OCOCQCC0Q0:boolean;O0QCQCC0Q0:OOOQOQQ0Q0;
OOQCQCC0Q0:OCOC00OOQ0;OQQCQCC0Q0:boolean;out OCQCQCC0Q0:OCQOCQC0Q0);
class function OO00QCC0Q0(const OQ00QCC0Q0:string):string;
procedure O0O0QCC0Q0(const OOO0QCC0Q0:string);
function OQO0QCC0Q0(const OCO0QCC0Q0:string):OCQOCQC0Q0;
function O0Q0QCC0Q0(const OOQ0QCC0Q0:string):OCQOCQC0Q0;
procedure OQQ0QCC0Q0(OCQ0QCC0Q0:TStrings);
property O0C0QCC0Q0[Index:integer]:OCQOCQC0Q0 read OCCQQCC0Q0 write OO0CQCC0Q0;default;
end;
O0O0OQQ0Q0=class
private
OOO0OQQ0Q0:O0OQ0CC0Q0;
OQO0OQQ0Q0:string;
OCO0OQQ0Q0:TBytes;
O0Q0OQQ0Q0:string;
OOQ0OQQ0Q0:TBytes;
public
destructor Destroy;override;
property OCQ0OQQ0Q0:O0OQ0CC0Q0 read OOO0OQQ0Q0;
property O0C0OQQ0Q0:string read OQO0OQQ0Q0;
property OOC0OQQ0Q0:TBytes read OCO0OQQ0Q0;
property OQC0OQQ0Q0:string read O0Q0OQQ0Q0;
property OCC0OQQ0Q0:TBytes read OOQ0OQQ0Q0;
end;
OOC0QCC0Q0=class
private
OQC0QCC0Q0:TCRList;
OCC0QCC0Q0:TCRList;
O00OQCC0Q0:TCRList;
OO0OQCC0Q0:TCRList;
OQ0OQCC0Q0:TScCertificate;
OC0OQCC0Q0:TScCertificate;
procedure O0OOQCC0Q0(const OOOOQCC0Q0:TBytes;const OQOOQCC0Q0:string);
public
constructor Create;
destructor Destroy;override;
procedure OQQQOQC0Q0;
class function OOCQOQC0Q0(OQCQOQC0Q0:TStream):string;
procedure O0QCOQC0Q0(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string='');overload;
procedure O0QCOQC0Q0(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string='');overload;
property OC0OOQC0Q0:TScCertificate read OQ0OQCC0Q0 write OQ0OQCC0Q0;
property O0OOOQC0Q0:TScCertificate read OC0OQCC0Q0 write OC0OQCC0Q0;
property OOOOOQC0Q0:TCRList read OQC0QCC0Q0;
property OQOOOQC0Q0:TCRList read OCC0QCC0Q0;
property OCOOOQC0Q0:TCRList read O00OQCC0Q0;
property O0QOOQC0Q0:TCRList read OO0OQCC0Q0;
end;
O00OOQQ0Q0=class
public
class procedure OO0OOQQ0Q0(OQ0OOQQ0Q0:O0CQOOC0Q0;const OC0OOQQ0Q0:TBytes);
class function O0OOOQQ0Q0(OOOOOQQ0Q0:O0CQOOC0Q0):TBytes;
class procedure OQOOOQQ0Q0(OCOOOQQ0Q0:O0CQOOC0Q0;const O0QOOQQ0Q0:OC000O0OQ0);
class procedure OOQOOQQ0Q0(OQQOOQQ0Q0:O0CQOOC0Q0;const O00QOCQ0Q0:TBytes;
const OO0QOCQ0Q0:OCQCQ00OQ0;const OQ0QOCQ0Q0:OOQQQOQOQ0;const OC0QOCQ0Q0:boolean);
class function OCQOOQQ0Q0(O0COOQQ0Q0:O0CQOOC0Q0):TBytes;
class procedure OOCOOQQ0Q0(OQCOOQQ0Q0:O0CQOOC0Q0;const OQOQOCQ0Q0:TBytes);
end;
OCCOOQQ0Q0=class
public
class procedure O00Q0QQ0Q0(OO0Q0QQ0Q0:TScCertificate;OOQ00QC0Q0:IntPtr;OQQ00QC0Q0:OQQQOQQ0Q0);
end;
OQ0Q0QQ0Q0=procedure(OC0Q0QQ0Q0:TObject;
O0OQ0QQ0Q0:O0CQOOC0Q0;
var OOOQ0QQ0Q0:boolean
)of object;
OQOQ0QQ0Q0=procedure(OCOQ0QQ0Q0:TObject;
O0QQ0QQ0Q0:TScCertificate;OOQQ0QQ0Q0:TCRList;
var OQQQ0QQ0Q0:TScCertificateStatusSet
)of object;
const
OCQQ0QQ0Q0=64;
O0CQ0QQ0Q0=48;
const
OOCQ0QQ0Q0:array[TScCertificateStatus]of TScErrorCode=(
seSuccess,
seCertificateNotValid,seCertNotValidityPeriod,
seInsecureSignature,seForbiddenSignature,seInvalidPolicies,
seUnknownCriticalExtension,
seCRLNotFound,seCRLIsNotValid,seCertificateIsRevoked,
seCertCANotValid,seCertKeyUsageNotValid,seIssuerNotEqualSubject,
seInvalidSubjectName,seCertNotTrusted,seCertSignatureNotValid
);
var
OQCQ0QQ0Q0:string;
OCCQ0QQ0Q0:IScRandom;
O00C0QQ0Q0:OCCCQ00OQ0=nil;
implementation
uses
StrUtils,Math,
CRBase64,CRHash,CRDECUtil,CRCryptoTransformIntf,
CRHttp,
{$IFNDEF UNIDACPRO}
TdsReaderWriter,TdsAlgorithmSupport,TdsCipherSuites;
{$ELSE}
TdsReaderWriterUni,TdsAlgorithmSupportUni,TdsCipherSuitesUni;
{$ENDIF}
const
OO0C0QQ0Q0=1;
OQ0C0QQ0Q0=11;
OC0C0QQ0Q0=1;
O0OC0QQ0Q0=2;
OOOC0QQ0Q0=3;
OQOC0QQ0Q0=4;
OCOC0QQ0Q0='1';
O0QC0QQ0Q0='2';
OOQC0QQ0Q0='\SOFTWARE\Devart\SecureBridge\';
OQQC0QQ0Q0='SSHPERMS';
OCQC0QQ0Q0='EXTDATA';
O0CC0QQ0Q0='_tmp';
OOCC0QQ0Q0='\Keys';
OQCC0QQ0Q0='\Users';
OCCC0QQ0Q0='\Certificates';
O0000QQ0Q0='\CRLs';
const
OO000QQ0Q0:array[0..3]of OCCC0CC0Q0=(
O0CQOOC0Q0,OCQQQCQ0Q0,TScCertificate,OCQOCQC0Q0
);
const
OQ000QQ0Q0:array[OCOQO0QOQ0]of integer=(
1,
2,
4,
8
);
const
OC000QQ0Q0=4;
type
O0O00QQ0Q0=class
private
OOO00QQ0Q0:array of cardinal;
public
constructor Create(OCO00QQ0Q0:integer);
function OQC00QQ0Q0:cardinal;
function OC0O0QQ0Q0(O0OO0QQ0Q0:integer):integer;
end;
const
OQOO0QQ0Q0:array[0..255]of integer=(
0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,
2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,1,2,
2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,
4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,1,2,2,3,
2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,
4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,2,3,3,4,3,4,
4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,3,4,4,5,4,5,5,
6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8
);
constructor O0O00QQ0Q0.Create(OCO00QQ0Q0:integer);
var
O0Q00QQ0Q0,OOQ00QQ0Q0,OQQ00QQ0Q0:integer;
OCQ00QQ0Q0,O0C00QQ0Q0,OOC00QQ0Q0:integer;
begin
inherited Create;
if OCO00QQ0Q0<4 then
OCO00QQ0Q0:=4;
O0Q00QQ0Q0:=(OCO00QQ0Q0-3)div(32*2);
SetLength(OOO00QQ0Q0,O0Q00QQ0Q0);
OOQ00QQ0Q0:=O0Q00QQ0Q0*32;
OQQ00QQ0Q0:=Round(sqrt(OOQ00QQ0Q0))+1;
for OCQ00QQ0Q0:=0 to OQQ00QQ0Q0-1 do begin
if(OOO00QQ0Q0[OCQ00QQ0Q0 div 32]and(1 shl(OCQ00QQ0Q0 and(32-1))))=0 then begin
OOC00QQ0Q0:=3+OCQ00QQ0Q0*2;
O0C00QQ0Q0:=OCQ00QQ0Q0+OOC00QQ0Q0;
while O0C00QQ0Q0<OOQ00QQ0Q0 do begin
OOO00QQ0Q0[O0C00QQ0Q0 div 32]:=OOO00QQ0Q0[O0C00QQ0Q0 div 32]or(cardinal(1)shl(O0C00QQ0Q0 and(32-1)));
O0C00QQ0Q0:=O0C00QQ0Q0+OOC00QQ0Q0;
end;
end;
end;
end;
function O0O00QQ0Q0.OQC00QQ0Q0:cardinal;
var
OCC00QQ0Q0,O00O0QQ0Q0,OO0O0QQ0Q0:cardinal;
OQ0O0QQ0Q0:integer;
begin
OO0O0QQ0Q0:=2;
for OQ0O0QQ0Q0:=0 to Length(OOO00QQ0Q0)-1 do begin
O00O0QQ0Q0:=OOO00QQ0Q0[OQ0O0QQ0Q0];
OCC00QQ0Q0:=0;
while O00O0QQ0Q0<>0 do begin
OCC00QQ0Q0:=OCC00QQ0Q0+cardinal(OQOO0QQ0Q0[O00O0QQ0Q0 and$FF]);
O00O0QQ0Q0:=O00O0QQ0Q0 shr 8;
end;
OO0O0QQ0Q0:=OO0O0QQ0Q0+(32-OCC00QQ0Q0);
end;
Result:=OO0O0QQ0Q0;
end;
function O0O00QQ0Q0.OC0O0QQ0Q0(O0OO0QQ0Q0:integer):integer;
var
OOOO0QQ0Q0:integer;
begin
OOOO0QQ0Q0:=((O0OO0QQ0Q0-3)div 2)+1;
case O0OO0QQ0Q0 of
0:Result:=2;
1:Result:=2;
2:Result:=3;
else
while True do begin
if(OOOO0QQ0Q0 div 32)>=Length(OOO00QQ0Q0)then begin
Result:=0;
Exit;
end;
if(OOO00QQ0Q0[OOOO0QQ0Q0 div 32]and(1 shl(OOOO0QQ0Q0 and(32-1))))=0 then begin
Result:=OOOO0QQ0Q0*2+3;
Exit;
end;
Inc(OOOO0QQ0Q0);
end;
end;
end;
constructor T32BytesField.Create;
begin
inherited;
end;
constructor T32BytesField.Create(const OQQOCO0OQ0:TBytes);
begin
inherited Create;
if Length(OQQOCO0OQ0)>=sizeof(OCQCQC0OQ0)then
Move(OQQOCO0OQ0[0],OOQOCO0OQ0[0],sizeof(OCQCQC0OQ0));
end;
procedure T32BytesField.Assign(O000CO0OQ0:TPersistent);
begin
if IsClass(O000CO0OQ0,T32BytesField)then begin
Move(T32BytesField(O000CO0OQ0).OOQOCO0OQ0[0],OOQOCO0OQ0[0],sizeof(OCQCQC0OQ0));
end
else
inherited Assign(O000CO0OQ0);
end;
function T32BytesField.Equal(OCQOCO0OQ0:T32BytesField):boolean;
begin
Result:=MemCompare(@OOQOCO0OQ0[0],@OCQOCO0OQ0.OOQOCO0OQ0[0],sizeof(OCQCQC0OQ0))=0;
end;
function T32BytesField.GetBytes:TBytes;
begin
SetLength(Result,sizeof(OCQCQC0OQ0));
Move(OOQOCO0OQ0[0],Result[0],sizeof(OCQCQC0OQ0));
end;
destructor O00QQO0OQ0.Destroy;
begin
OO0QQO0OQ0.Free;
OQ0QQO0OQ0.Free;
OC0QQO0OQ0.Free;
inherited;
end;
procedure O00QQO0OQ0.OQCOCO0OQ0(O000CO0OQ0:OOCOCO0OQ0);
begin
if IsClass(O000CO0OQ0,O00QQO0OQ0)then begin
FreeAndNil(OO0QQO0OQ0);
FreeAndNil(OQ0QQO0OQ0);
FreeAndNil(OC0QQO0OQ0);
if O00QQO0OQ0(O000CO0OQ0).OO0QQO0OQ0<>nil then
OO0QQO0OQ0:=TBigInteger.Create(O00QQO0OQ0(O000CO0OQ0).OO0QQO0OQ0);
if O00QQO0OQ0(O000CO0OQ0).OQ0QQO0OQ0<>nil then
OQ0QQO0OQ0:=TBigInteger.Create(O00QQO0OQ0(O000CO0OQ0).OQ0QQO0OQ0);
if O00QQO0OQ0(O000CO0OQ0).OC0QQO0OQ0<>nil then
OC0QQO0OQ0:=TBigInteger.Create(O00QQO0OQ0(O000CO0OQ0).OC0QQO0OQ0);
end
else
if O000CO0OQ0<>nil then
raise EConvertError.CreateFmt(SAssignError,[O000CO0OQ0.ClassName,ClassName]);
end;
function O00QQO0OQ0.Equals(OCCOCO0OQ0:OOCOCO0OQ0):boolean;
begin
Result:=IsClass(OCCOCO0OQ0,O00QQO0OQ0)and
((OO0QQO0OQ0=nil)or OO0QQO0OQ0.Equal(O00QQO0OQ0(OCCOCO0OQ0).OO0QQO0OQ0))and
((OQ0QQO0OQ0=nil)or OQ0QQO0OQ0.Equal(O00QQO0OQ0(OCCOCO0OQ0).OQ0QQO0OQ0))and
((OC0QQO0OQ0=nil)or OC0QQO0OQ0.Equal(O00QQO0OQ0(OCCOCO0OQ0).OC0QQO0OQ0));
end;
procedure OOOQQO0OQ0.OQCOCO0OQ0(O000CO0OQ0:OOCOCO0OQ0);
begin
if IsClass(O000CO0OQ0,OOOQQO0OQ0)then begin
Move(OOOQQO0OQ0(O000CO0OQ0).OQOQQO0OQ0[0],OQOQQO0OQ0[0],sizeof(OCQCQC0OQ0));
end
else
if O000CO0OQ0<>nil then
raise EConvertError.CreateFmt(SAssignError,[O000CO0OQ0.ClassName,ClassName]);
end;
function OOOQQO0OQ0.Equals(OCCOCO0OQ0:OOCOCO0OQ0):boolean;
begin
Result:=IsClass(OCCOCO0OQ0,OOOQQO0OQ0)and
(MemCompare(@OQOQQO0OQ0[0],@OOOQQO0OQ0(OCCOCO0OQ0).OQOQQO0OQ0[0],sizeof(OCQCQC0OQ0))=0);
end;
constructor OO0CCO0OQ0.Create;
begin
inherited Create;
OQ0CCO0OQ0:=False;
end;
constructor OO0CCO0OQ0.Create(const OQCCCO0OQ0:TScECParameters);
begin
inherited Create;
OQ0CCO0OQ0:=True;
OC0CCO0OQ0:=OQCCCO0OQ0.Size;
end;
procedure OO0CCO0OQ0.OCCCCO0OQ0(O000CO0OQ0:OO0CCO0OQ0);
begin
if O000CO0OQ0=nil then
Exit;
if ClassType<>O000CO0OQ0.ClassType then
raise EConvertError.CreateFmt(SAssignError,[O000CO0OQ0.ClassName,ClassName]);
OQ0CCO0OQ0:=O000CO0OQ0.OQ0CCO0OQ0;
OC0CCO0OQ0:=O000CO0OQ0.OC0CCO0OQ0;
end;
function OO0CCO0OQ0.OOOCCO0OQ0(OQOCCO0OQ0:TBigInteger):TBytes;
var
OCOCCO0OQ0:string;
O0QCCO0OQ0:integer;
begin
OCOCCO0OQ0:=OQOCCO0OQ0.ToString(16);
Result:=OOOQOCOOQ0(OCOCCO0OQ0);
O0QCCO0OQ0:=Length(Result);
if O0QCCO0OQ0<OC0CCO0OQ0 then begin
SetLength(Result,OC0CCO0OQ0);
Move(Result[0],Result[OC0CCO0OQ0-O0QCCO0OQ0],O0QCCO0OQ0);
FillChar(Result[0],OC0CCO0OQ0-O0QCCO0OQ0,0);
end;
end;
constructor O0CQQO0OQ0.Create;
begin
inherited Create;
end;
constructor O0CQQO0OQ0.Create(const OQCCCO0OQ0:TScECParameters);
begin
inherited Create(OQCCCO0OQ0);
OOCQQO0OQ0:=TBigInteger.Create(OQCCCO0OQ0.p,16);
OQCQQO0OQ0:=TBigInteger.Create(OQCCCO0OQ0.a,16);
OCCQQO0OQ0:=TBigInteger.Create(OQCCCO0OQ0.b,16);
O00CQO0OQ0:=TBigInteger.Create(OQCCCO0OQ0.n,16);
OO0CQO0OQ0:=O00QQO0OQ0.Create;
OO0CQO0OQ0.OO0QQO0OQ0:=TBigInteger.Create(OQCCCO0OQ0.Gx,16);
OO0CQO0OQ0.OQ0QQO0OQ0:=TBigInteger.Create(OQCCCO0OQ0.Gy,16);
OO0CQO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(1);
OQO0QO0OQ0;
end;
destructor O0CQQO0OQ0.Destroy;
begin
OOCQQO0OQ0.Free;
OQCQQO0OQ0.Free;
OCCQQO0OQ0.Free;
O00CQO0OQ0.Free;
OO0CQO0OQ0.Free;
inherited;
end;
function O0CQQO0OQ0.Equals(OO00CO0OQ0:OO0CCO0OQ0):boolean;
begin
Result:=(ClassType=OO00CO0OQ0.ClassType)and
(OC0CCO0OQ0=OO00CO0OQ0.OC0CCO0OQ0)and
OOCQQO0OQ0.Equal(O0CQQO0OQ0(OO00CO0OQ0).OOCQQO0OQ0)and
OQCQQO0OQ0.Equal(O0CQQO0OQ0(OO00CO0OQ0).OQCQQO0OQ0)and
OCCQQO0OQ0.Equal(O0CQQO0OQ0(OO00CO0OQ0).OCCQQO0OQ0)and
O00CQO0OQ0.Equal(O0CQQO0OQ0(OO00CO0OQ0).O00CQO0OQ0)and
OO0CQO0OQ0.Equals(O0CQQO0OQ0(OO00CO0OQ0).OO0CQO0OQ0);
end;
procedure O0CQQO0OQ0.OCCCCO0OQ0(O000CO0OQ0:OO0CCO0OQ0);
var
OQOOQO0OQ0:O0CQQO0OQ0;
begin
if O000CO0OQ0=nil then
Exit;
inherited OCCCCO0OQ0(O000CO0OQ0);
OQOOQO0OQ0:=O0CQQO0OQ0(O000CO0OQ0);
OQ0CQO0OQ0:=OQOOQO0OQ0.OQ0CQO0OQ0;
OC0CQO0OQ0:=OQOOQO0OQ0.OC0CQO0OQ0;
FreeAndNil(OOCQQO0OQ0);
if OQOOQO0OQ0.OOCQQO0OQ0<>nil then
OOCQQO0OQ0:=TBigInteger.Create(OQOOQO0OQ0.OOCQQO0OQ0);
FreeAndNil(OQCQQO0OQ0);
if OQOOQO0OQ0.OQCQQO0OQ0<>nil then
OQCQQO0OQ0:=TBigInteger.Create(OQOOQO0OQ0.OQCQQO0OQ0);
FreeAndNil(OCCQQO0OQ0);
if OQOOQO0OQ0.OCCQQO0OQ0<>nil then
OCCQQO0OQ0:=TBigInteger.Create(OQOOQO0OQ0.OCCQQO0OQ0);
FreeAndNil(O00CQO0OQ0);
if OQOOQO0OQ0.O00CQO0OQ0<>nil then
O00CQO0OQ0:=TBigInteger.Create(OQOOQO0OQ0.O00CQO0OQ0);
FreeAndNil(OO0CQO0OQ0);
if OQOOQO0OQ0.OO0CQO0OQ0<>nil then begin
OO0CQO0OQ0:=O00QQO0OQ0.Create;
OO0CQO0OQ0.OQCOCO0OQ0(OQOOQO0OQ0.OO0CQO0OQ0);
end;
OQO0QO0OQ0;
end;
function O0CQQO0OQ0.OQ00CO0OQ0:O0COCO0OQ0;
begin
Result:=O00QQO0OQ0;
end;
function O0CQQO0OQ0.OC00CO0OQ0:TPersistentClass;
begin
Result:=TBigInteger;
end;
function O0CQQO0OQ0.O0CCCO0OQ0:integer;
begin
Result:=OOCQQO0OQ0.BitCount;
end;
function O0CQQO0OQ0.OC00QO0OQ0(O0O0QO0OQ0:IScRandom=nil):TBigInteger;
var
OOO0QO0OQ0:TBytes;
begin
if O0O0QO0OQ0=nil then
O0O0QO0OQ0:=OCCQ0QQ0Q0;
if O0O0QO0OQ0=nil then
raise Exception.Create(SInternalError);
SetLength(OOO0QO0OQ0,OC0CCO0OQ0);
O0O0QO0OQ0.Random(OOO0QO0OQ0,0,Length(OOO0QO0OQ0));
Result:=TBigInteger.Create(OOO0QO0OQ0);
try
if Result.GreaterOrEqual(O00CQO0OQ0)and(OOO0QO0OQ0[0]>1)then begin
OOO0QO0OQ0[0]:=1;
FreeAndNil(Result);
Result:=TBigInteger.Create(OOO0QO0OQ0);
end;
if Result.GreaterOrEqual(O00CQO0OQ0)then begin
OOO0QO0OQ0[0]:=0;
OOO0QO0OQ0[1]:=1;
FreeAndNil(Result);
Result:=TBigInteger.Create(OOO0QO0OQ0);
end;
FillChar(OOO0QO0OQ0[0],Length(OOO0QO0OQ0),0);
except
Result.Free;
raise;
end;
end;
procedure O0CQQO0OQ0.OOQCCO0OQ0(var OQQCCO0OQ0:OCOQQO0OQ0;OCQCCO0OQ0:IScRandom=nil);
begin
OQQCCO0OQ0.OCQQQO0OQ0:=OC00QO0OQ0(OCQCCO0OQ0);
OQQCCO0OQ0.OQQQQO0OQ0:=O0O0CO0OQ0(OO0CQO0OQ0,OQQCCO0OQ0.OCQQQO0OQ0);
if not OC0OQO0OQ0(O00QQO0OQ0(OQQCCO0OQ0.OQQQQO0OQ0))then
raise EScError.Create(seWrongEllipticCurvesParameters);
end;
class function O0CQQO0OQ0.O0OCQO0OQ0(OOOCQO0OQ0:TBigInteger):TIntArr;
var
OQOCQO0OQ0:integer;
OCOCQO0OQ0,O0QCQO0OQ0,OOQCQO0OQ0:integer;
OQQCQO0OQ0,OCQCQO0OQ0:integer;
O0CCQO0OQ0:cardinal;
OOCCQO0OQ0:integer;
begin
OCOCQO0OQ0:=1 shl OC000QQ0Q0;
O0QCQO0OQ0:=OCOCQO0OQ0 shl 1;
O0CCQO0OQ0:=O0QCQO0OQ0-1;
if OOOCQO0OQ0.IsNegative then
OQQCQO0OQ0:=-1
else
OQQCQO0OQ0:=1;
OOQCQO0OQ0:=OOOCQO0OQ0.BitCount;
SetLength(Result,OOQCQO0OQ0+1);
OQOCQO0OQ0:=OOOCQO0OQ0.IntValue and O0CCQO0OQ0;
OOCCQO0OQ0:=0;
while(OQOCQO0OQ0<>0)or((OOCCQO0OQ0+OC000QQ0Q0+1)<OOQCQO0OQ0)do begin
OCQCQO0OQ0:=0;
if(OQOCQO0OQ0 and 1)<>0 then begin
if(OQOCQO0OQ0 and OCOCQO0OQ0)<>0 then begin
OCQCQO0OQ0:=OQOCQO0OQ0-O0QCQO0OQ0;
if(OOCCQO0OQ0+OC000QQ0Q0+1)>=OOQCQO0OQ0 then begin
OCQCQO0OQ0:=OQOCQO0OQ0 and(O0CCQO0OQ0 shr 1);
end;
end
else
OCQCQO0OQ0:=OQOCQO0OQ0;
if(OCQCQO0OQ0<=-OCOCQO0OQ0)or(OCQCQO0OQ0>=OCOCQO0OQ0)or((OCQCQO0OQ0 and 1)=0)then
raise EScError.Create(sewNAFError);
OQOCQO0OQ0:=OQOCQO0OQ0-OCQCQO0OQ0;
if(OQOCQO0OQ0<>0)and(OQOCQO0OQ0<>O0QCQO0OQ0)and(OQOCQO0OQ0<>OCOCQO0OQ0)then
raise EScError.Create(sewNAFError);
end;
if OOCCQO0OQ0>=(OOQCQO0OQ0+1)then
raise EScError.Create(sewNAFError);
Result[OOCCQO0OQ0]:=OQQCQO0OQ0*OCQCQO0OQ0;
Inc(OOCCQO0OQ0);
OQOCQO0OQ0:=OQOCQO0OQ0 shr 1;
OQOCQO0OQ0:=OQOCQO0OQ0+(OCOCQO0OQ0*OOOCQO0OQ0.GetBit(OOCCQO0OQ0+OC000QQ0Q0));
if OQOCQO0OQ0>O0QCQO0OQ0 then
raise EScError.Create(sewNAFError);
end;
SetLength(Result,OOCCQO0OQ0);
end;
function O0CQQO0OQ0.O0O0CO0OQ0(OOO0CO0OQ0:OOCOCO0OQ0;OQO0CO0OQ0:TObject):OOCOCO0OQ0;
var
OCOOQO0OQ0:TIntArr;
O0QOQO0OQ0:integer;
OOQOQO0OQ0:array of O00QQO0OQ0;
OQQOQO0OQ0:O00QQO0OQ0;
OCQOQO0OQ0,O0COQO0OQ0,OOCOQO0OQ0:boolean;
OQCOQO0OQ0:integer;
begin
if not IsClass(OOO0CO0OQ0,O00QQO0OQ0)then
raise EScError.Create(seInvalidInputArgs);
if not IsClass(OQO0CO0OQ0,TBigInteger)then
raise EScError.Create(seInvalidInputArgs);
if O00QQO0OQ0(OOO0CO0OQ0).OC0QQO0OQ0.BitCount<>1 then
raise EScError.Create(seInvalidInputArgs);
OQQOQO0OQ0:=O00QQO0OQ0.Create;
try
OCOOQO0OQ0:=O0OCQO0OQ0(TBigInteger(OQO0CO0OQ0));
SetLength(OOQOQO0OQ0,1 shl(OC000QQ0Q0-1));
for OQCOQO0OQ0:=0 to Length(OOQOQO0OQ0)-1 do
OOQOQO0OQ0[OQCOQO0OQ0]:=O00QQO0OQ0.Create;
OQCCQO0OQ0(O00QQO0OQ0(OOO0CO0OQ0),OOQOQO0OQ0[0]);
OOC0QO0OQ0(OOQOQO0OQ0[0],OQQOQO0OQ0);
for OQCOQO0OQ0:=1 to Length(OOQOQO0OQ0)-1 do begin
OOQ0QO0OQ0(OQQOQO0OQ0,OOQOQO0OQ0[OQCOQO0OQ0-1],OOQOQO0OQ0[OQCOQO0OQ0]);
if OQ0OQO0OQ0 then
O00OQO0OQ0(OOQOQO0OQ0[OQCOQO0OQ0]);
end;
OCQOQO0OQ0:=True;
O0COQO0OQ0:=False;
for OQCOQO0OQ0:=Length(OCOOQO0OQ0)-1 downto 0 do begin
if not OCQOQO0OQ0 then
OOC0QO0OQ0(OQQOQO0OQ0,OQQOQO0OQ0);
O0QOQO0OQ0:=OCOOQO0OQ0[OQCOQO0OQ0];
if O0QOQO0OQ0<>0 then begin
OOCOQO0OQ0:=O0QOQO0OQ0<0;
if OOCOQO0OQ0 then
O0QOQO0OQ0:=-O0QOQO0OQ0;
if OOCOQO0OQ0<>O0COQO0OQ0 then begin
if not OCQOQO0OQ0 then
OCO0QO0OQ0(OQQOQO0OQ0);
O0COQO0OQ0:=not O0COQO0OQ0;
end;
if OCQOQO0OQ0 then begin
OQCCQO0OQ0(OOQOQO0OQ0[O0QOQO0OQ0 shr 1],OQQOQO0OQ0);
OCQOQO0OQ0:=False;
end
else
OOQ0QO0OQ0(OQQOQO0OQ0,OOQOQO0OQ0[O0QOQO0OQ0 shr 1],OQQOQO0OQ0);
end;
end;
if OCQOQO0OQ0 then
OO00QO0OQ0(OQQOQO0OQ0)
else
if O0COQO0OQ0 then
OCO0QO0OQ0(OQQOQO0OQ0);
O00OQO0OQ0(OQQOQO0OQ0);
Result:=OQQOQO0OQ0;
OQQOQO0OQ0:=nil;
finally
for OQCOQO0OQ0:=0 to Length(OOQOQO0OQ0)-1 do
OOQOQO0OQ0[OQCOQO0OQ0].Free;
OQQOQO0OQ0.Free;
end;
end;
function O0CQQO0OQ0.OQ0OQO0OQ0:boolean;
begin
Result:=False;
end;
procedure O0CQQO0OQ0.OO00QO0OQ0(OQ00QO0OQ0:O00QQO0OQ0);
begin
if OQ00QO0OQ0.OC0QQO0OQ0<>nil then
OQ00QO0OQ0.OC0QQO0OQ0.SetToZero
else
OQ00QO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(0);
end;
procedure O0CQQO0OQ0.OQCCQO0OQ0(OCCCQO0OQ0:O00QQO0OQ0;O000QO0OQ0:O00QQO0OQ0);
begin
if OCCCQO0OQ0=O000QO0OQ0 then
Exit;
FreeAndNil(O000QO0OQ0.OO0QQO0OQ0);
FreeAndNil(O000QO0OQ0.OQ0QQO0OQ0);
FreeAndNil(O000QO0OQ0.OC0QQO0OQ0);
O000QO0OQ0.OO0QQO0OQ0:=TBigInteger.Create(OCCCQO0OQ0.OO0QQO0OQ0);
O000QO0OQ0.OQ0QQO0OQ0:=TBigInteger.Create(OCCCQO0OQ0.OQ0QQO0OQ0);
O000QO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(OCCCQO0OQ0.OC0QQO0OQ0);
end;
function O0CQQO0OQ0.OCO0CO0OQ0(const O0Q0CO0OQ0:TBytes;OOQ0CO0OQ0,OQQ0CO0OQ0:integer):OOCOCO0OQ0;
var
OCCOQO0OQ0:O00QQO0OQ0;
begin
OCCOQO0OQ0:=O00QQO0OQ0.Create;
Result:=OCCOQO0OQ0;
try
if(OQQ0CO0OQ0=1)and(O0Q0CO0OQ0[OOQ0CO0OQ0]=0)then begin
OCCOQO0OQ0.OO0QQO0OQ0:=TBigInteger.Create(0);
OCCOQO0OQ0.OQ0QQO0OQ0:=TBigInteger.Create(0);
OCCOQO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(0);
Exit;
end;
if OQQ0CO0OQ0<>(1+OC0CCO0OQ0*2)then
raise EScError.Create(seWrongECPointFormat);
if O0Q0CO0OQ0[OOQ0CO0OQ0]<>4 then
raise EScError.Create(seWrongECPointFormat);
OCCOQO0OQ0.OO0QQO0OQ0:=TBigInteger.Create(O0Q0CO0OQ0,1+OOQ0CO0OQ0,OC0CCO0OQ0);
OCCOQO0OQ0.OQ0QQO0OQ0:=TBigInteger.Create(O0Q0CO0OQ0,1+OOQ0CO0OQ0+OC0CCO0OQ0,OC0CCO0OQ0);
if(OCCOQO0OQ0.OO0QQO0OQ0.BitCount=0)and(OCCOQO0OQ0.OQ0QQO0OQ0.BitCount=0)then
OCCOQO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(0)
else
OCCOQO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(1);
if not OC0OQO0OQ0(OCCOQO0OQ0)then
raise EScError.Create(seWrongECPointFormat);
except
OCCOQO0OQ0.Free;
raise;
end;
end;
function O0CQQO0OQ0.OCQ0CO0OQ0(O0C0CO0OQ0:OOCOCO0OQ0):TBytes;
var
O00QO00OQ0:TBytes;
begin
if not IsClass(O0C0CO0OQ0,O00QQO0OQ0)then
raise EScError.Create(seInvalidInputArgs);
if O00QQO0OQ0(O0C0CO0OQ0).OC0QQO0OQ0.BitCount=0 then begin
SetLength(Result,1);
Result[0]:=0;
Exit;
end;
SetLength(Result,1+OC0CCO0OQ0*2);
SetLength(O00QO00OQ0,0);
Result[0]:=4;
O00QO00OQ0:=O00QQO0OQ0(O0C0CO0OQ0).OO0QQO0OQ0.GetBytes(OC0CCO0OQ0);
Move(O00QO00OQ0[0],Result[1],Length(O00QO00OQ0));
O00QO00OQ0:=O00QQO0OQ0(O0C0CO0OQ0).OQ0QQO0OQ0.GetBytes(OC0CCO0OQ0);
Move(O00QO00OQ0[0],Result[1+OC0CCO0OQ0],Length(O00QO00OQ0));
end;
function O0CQQO0OQ0.OOC0CO0OQ0(const OQC0CO0OQ0:TBytes;OCC0CO0OQ0:TObject;O00OCO0OQ0:OOCOCO0OQ0):TBytes;
var
OO0QO00OQ0:TBigInteger;
OQ0QO00OQ0,OC0QO00OQ0,O0OQO00OQ0,OOOQO00OQ0,OQOQO00OQ0,OCOQO00OQ0:TBigInteger;
O0QQO00OQ0:O00QQO0OQ0;
OOQQO00OQ0:OQCOCQOOQ0;
OQQQO00OQ0:OC0QCOOCQ0;
begin
if not IsClass(OCC0CO0OQ0,TBigInteger)then
raise EScError.Create(seInvalidInputArgs);
OC0QO00OQ0:=nil;
OQ0QO00OQ0:=nil;
O0OQO00OQ0:=nil;
OOOQO00OQ0:=nil;
OQOQO00OQ0:=nil;
OCOQO00OQ0:=nil;
if Length(OQC0CO0OQ0)>OC0CCO0OQ0 then
OO0QO00OQ0:=TBigInteger.Create(OQC0CO0OQ0,0,OC0CCO0OQ0)
else
OO0QO00OQ0:=TBigInteger.Create(OQC0CO0OQ0);
try
repeat
repeat
FreeAndNil(OC0QO00OQ0);
FreeAndNil(OQ0QO00OQ0);
OC0QO00OQ0:=OC00QO0OQ0;
O0QQO00OQ0:=O00QQO0OQ0(O0O0CO0OQ0(OO0CQO0OQ0,OC0QO00OQ0));
try
OQ0QO00OQ0:=O0QQO00OQ0.OO0QQO0OQ0.Mod_(O00CQO0OQ0);
finally
FreeAndNil(O0QQO00OQ0);
end;
until OQ0QO00OQ0.BitCount<>0;
FreeAndNil(OOOQO00OQ0);
FreeAndNil(OQOQO00OQ0);
FreeAndNil(O0OQO00OQ0);
OOOQO00OQ0:=TBigInteger(OCC0CO0OQ0).Mul(OQ0QO00OQ0);
OCOQO00OQ0:=OOOQO00OQ0.Mod_(O00CQO0OQ0);
OQOQO00OQ0:=OCOQO00OQ0.Add(OO0QO00OQ0);
FreeAndNil(OCOQO00OQ0);
OCOQO00OQ0:=OC0QO00OQ0;
OC0QO00OQ0:=OC0QO00OQ0.ModInverse(O00CQO0OQ0);
FreeAndNil(OCOQO00OQ0);
OCOQO00OQ0:=OC0QO00OQ0.Mul(OQOQO00OQ0);
O0OQO00OQ0:=OCOQO00OQ0.Mod_(O00CQO0OQ0);
FreeAndNil(OCOQO00OQ0);
until O0OQO00OQ0.BitCount<>0;
if OCOOCO0OQ0 then begin
OQQQO00OQ0:=OC0QCOOCQ0.Create;
try
OQQQO00OQ0.O0QO0OOCQ0(OQ0QO00OQ0.GetBytes);
OQQQO00OQ0.O0QO0OOCQ0(O0OQO00OQ0.GetBytes);
Result:=OQQQO00OQ0.OOCC0OOCQ0;
finally
OQQQO00OQ0.Free;
end;
end
else begin
OOQQO00OQ0:=OQCOCQOOQ0.Create;
try
if not OOQQO00OQ0.OCQCOOOOQ0(OOQQCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OOQQO00OQ0['R'].OCQQ0QOOQ0:=OQ0QO00OQ0.GetBytes;
OOQQO00OQ0['S'].OCQQ0QOOQ0:=O0OQO00OQ0.GetBytes;
Result:=OOQQO00OQ0.OOQCOOOOQ0;
finally
OOQQO00OQ0.Free;
end;
end;
finally
OO0QO00OQ0.Free;
OC0QO00OQ0.Free;
OQ0QO00OQ0.Free;
O0OQO00OQ0.Free;
OOOQO00OQ0.Free;
OQOQO00OQ0.Free;
OCOQO00OQ0.Free;
end;
end;
function O0CQQO0OQ0.OO0OCO0OQ0(const OQ0OCO0OQ0,OC0OCO0OQ0:TBytes;O0OOCO0OQ0:OOCOCO0OQ0):boolean;
var
OCQQO00OQ0:OQCOCQOOQ0;
O0CQO00OQ0:TBigInteger;
OOCQO00OQ0,OQCQO00OQ0,OCCQO00OQ0,O00CO00OQ0,OO0CO00OQ0,OQ0CO00OQ0,OC0CO00OQ0:TBigInteger;
O0OCO00OQ0,OOOCO00OQ0,OQOCO00OQ0:O00QQO0OQ0;
OCOCO00OQ0:OCCO0OOCQ0;
begin
Result:=False;
OOCQO00OQ0:=nil;
OQCQO00OQ0:=nil;
OCCQO00OQ0:=nil;
O00CO00OQ0:=nil;
OO0CO00OQ0:=nil;
OQ0CO00OQ0:=nil;
OC0CO00OQ0:=nil;
OOOCO00OQ0:=nil;
OQOCO00OQ0:=nil;
O0OCO00OQ0:=nil;
if Length(OQ0OCO0OQ0)>OC0CCO0OQ0 then
O0CQO00OQ0:=TBigInteger.Create(OQ0OCO0OQ0,0,OC0CCO0OQ0)
else
O0CQO00OQ0:=TBigInteger.Create(OQ0OCO0OQ0);
try
if OCOOCO0OQ0 then begin
OCOCO00OQ0:=OCCO0OOCQ0.Create(OC0OCO0OQ0);
try
OOCQO00OQ0:=OCOCO00OQ0.OOCQ0OOCQ0;
OQCQO00OQ0:=OCOCO00OQ0.OOCQ0OOCQ0;
finally
OCOCO00OQ0.Free;
end;
end
else begin
OCQQO00OQ0:=OQCOCQOOQ0.Create;
try
if not OCQQO00OQ0.OCQCOOOOQ0(OOQQCCOOQ0,OC0OCO0OQ0)then
raise EScError.Create(seWrongDataFormat);
OOCQO00OQ0:=TBigInteger.Create(OCQQO00OQ0['R'].OCQQ0QOOQ0);
OQCQO00OQ0:=TBigInteger.Create(OCQQO00OQ0['S'].OCQQ0QOOQ0);
finally
OCQQO00OQ0.Free;
end;
end;
if OOCQO00OQ0.GreaterOrEqual(O00CQO0OQ0)or OOCQO00OQ0.IsNegative or(OOCQO00OQ0.BitCount=0)or
OQCQO00OQ0.GreaterOrEqual(O00CQO0OQ0)or OQCQO00OQ0.IsNegative or(OQCQO00OQ0.BitCount=0)then
Exit;
OCCQO00OQ0:=OQCQO00OQ0.ModInverse(O00CQO0OQ0);
OC0CO00OQ0:=O0CQO00OQ0.Mul(OCCQO00OQ0);
O00CO00OQ0:=OC0CO00OQ0.Mod_(O00CQO0OQ0);
FreeAndNil(OC0CO00OQ0);
OC0CO00OQ0:=OOCQO00OQ0.Mul(OCCQO00OQ0);
OO0CO00OQ0:=OC0CO00OQ0.Mod_(O00CQO0OQ0);
FreeAndNil(OC0CO00OQ0);
OOOCO00OQ0:=O00QQO0OQ0(O0O0CO0OQ0(OO0CQO0OQ0,O00CO00OQ0));
if OOOCO00OQ0.OC0QQO0OQ0.BitCount=0 then
Exit;
OQOCO00OQ0:=O00QQO0OQ0(O0O0CO0OQ0(O0OOCO0OQ0,OO0CO00OQ0));
if OQOCO00OQ0.OC0QQO0OQ0.BitCount=0 then
Exit;
O0OCO00OQ0:=O00QQO0OQ0.Create;
OOQ0QO0OQ0(OOOCO00OQ0,OQOCO00OQ0,O0OCO00OQ0);
if(O0OCO00OQ0.OC0QQO0OQ0=nil)or(O0OCO00OQ0.OC0QQO0OQ0.BitCount=0)then
Exit;
O00OQO0OQ0(O0OCO00OQ0);
OQ0CO00OQ0:=O0OCO00OQ0.OO0QQO0OQ0.Mod_(O00CQO0OQ0);
Result:=OQ0CO00OQ0.Equal(OOCQO00OQ0);
finally
O0CQO00OQ0.Free;
OOCQO00OQ0.Free;
OQCQO00OQ0.Free;
OCCQO00OQ0.Free;
O00CO00OQ0.Free;
OO0CO00OQ0.Free;
OQ0CO00OQ0.Free;
OC0CO00OQ0.Free;
OOOCO00OQ0.Free;
OQOCO00OQ0.Free;
O0OCO00OQ0.Free;
end;
end;
procedure O0QCO00OQ0.OQO0QO0OQ0;
begin
if OOCQQO0OQ0=nil then
raise EScError.Create(seECCryptographyNotInitialized);
OOCQQO0OQ0.PrepareForBarrettReduction;
end;
procedure O0QCO00OQ0.OCO0QO0OQ0(O0Q0QO0OQ0:O00QQO0OQ0);
var
OOQCO00OQ0:TBigInteger;
begin
	if(O0Q0QO0OQ0.OQ0QQO0OQ0.BitCount=0)or(O0Q0QO0OQ0.OC0QQO0OQ0.BitCount=0)then
Exit;
OOQCO00OQ0:=O0Q0QO0OQ0.OQ0QQO0OQ0;
O0Q0QO0OQ0.OQ0QQO0OQ0:=OOCQQO0OQ0.Minus(O0Q0QO0OQ0.OQ0QQO0OQ0);
OOQCO00OQ0.Free;
end;
procedure O0QCO00OQ0.OOQ0QO0OQ0(OQQ0QO0OQ0,OCQ0QO0OQ0:O00QQO0OQ0;O0C0QO0OQ0:O00QQO0OQ0);
var
OQQCO00OQ0:boolean;
OCQCO00OQ0,O0CCO00OQ0,OOCCO00OQ0,OQCCO00OQ0,OCCCO00OQ0,O000O00OQ0,OO00O00OQ0,OQ00O00OQ0,OC00O00OQ0,O0O0O00OQ0,OOO0O00OQ0:TBigInteger;
begin
OQQCO00OQ0:=(OQQ0QO0OQ0=OCQ0QO0OQ0)or OQQ0QO0OQ0.Equals(OCQ0QO0OQ0);
if OQQCO00OQ0 then begin
OOC0QO0OQ0(OQQ0QO0OQ0,O0C0QO0OQ0);
Exit;
end;
if OQQ0QO0OQ0.OC0QQO0OQ0.BitCount=0 then begin
OQCCQO0OQ0(OCQ0QO0OQ0,O0C0QO0OQ0);
Exit;
end;
if OCQ0QO0OQ0.OC0QQO0OQ0.BitCount=0 then begin
OQCCQO0OQ0(OQQ0QO0OQ0,O0C0QO0OQ0);
Exit;
end;
OCQCO00OQ0:=nil;
O0CCO00OQ0:=nil;
OOCCO00OQ0:=nil;
OQCCO00OQ0:=nil;
OCCCO00OQ0:=nil;
O000O00OQ0:=nil;
OO00O00OQ0:=nil;
OQ00O00OQ0:=nil;
OC00O00OQ0:=nil;
O0O0O00OQ0:=nil;
OOO0O00OQ0:=nil;
try
if OCQ0QO0OQ0.OC0QQO0OQ0.BitCount=1 then begin
OOCCO00OQ0:=TBigInteger.Create(OQQ0QO0OQ0.OO0QQO0OQ0);
OQCCO00OQ0:=TBigInteger.Create(OQQ0QO0OQ0.OQ0QQO0OQ0);
end
else begin
FreeAndNil(O0CCO00OQ0);
O0CCO00OQ0:=OCQ0QO0OQ0.OC0QQO0OQ0.ModMul(OCQ0QO0OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(OOCCO00OQ0);
OOCCO00OQ0:=OQQ0QO0OQ0.OO0QQO0OQ0.ModMul(O0CCO00OQ0,OOCQQO0OQ0);
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=O0CCO00OQ0.ModMul(OCQ0QO0OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(OQCCO00OQ0);
OQCCO00OQ0:=OQQ0QO0OQ0.OQ0QQO0OQ0.ModMul(O0CCO00OQ0,OOCQQO0OQ0);
end;
if OQQ0QO0OQ0.OC0QQO0OQ0.BitCount=1 then begin
OCCCO00OQ0:=TBigInteger.Create(OCQ0QO0OQ0.OO0QQO0OQ0);
O000O00OQ0:=TBigInteger.Create(OCQ0QO0OQ0.OQ0QQO0OQ0);
end
else begin
FreeAndNil(O0CCO00OQ0);
O0CCO00OQ0:=OQQ0QO0OQ0.OC0QQO0OQ0.ModMul(OQQ0QO0OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(OCCCO00OQ0);
OCCCO00OQ0:=OCQ0QO0OQ0.OO0QQO0OQ0.ModMul(O0CCO00OQ0,OOCQQO0OQ0);
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=O0CCO00OQ0.ModMul(OQQ0QO0OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(O000O00OQ0);
O000O00OQ0:=OCQ0QO0OQ0.OQ0QQO0OQ0.ModMul(O0CCO00OQ0,OOCQQO0OQ0);
end;
FreeAndNil(OO00O00OQ0);
OO00O00OQ0:=OOCCO00OQ0.Minus(OCCCO00OQ0);
if OO00O00OQ0.IsNegative then begin
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=OO00O00OQ0;
OO00O00OQ0:=OO00O00OQ0.Add(OOCQQO0OQ0);
end;
FreeAndNil(OC00O00OQ0);
OC00O00OQ0:=OQCCO00OQ0.Minus(O000O00OQ0);
if OC00O00OQ0.IsNegative then begin
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=OC00O00OQ0;
OC00O00OQ0:=OC00O00OQ0.Add(OOCQQO0OQ0);
end;
if OO00O00OQ0.BitCount=0 then begin
if OC00O00OQ0.BitCount=0 then
OOC0QO0OQ0(OQQ0QO0OQ0,O0C0QO0OQ0)
else
OO00QO0OQ0(O0C0QO0OQ0);
Exit;
end;
O0O0O00OQ0:=OOCCO00OQ0.Add(OCCCO00OQ0);
OOO0O00OQ0:=OQCCO00OQ0.Add(O000O00OQ0);
if(OQQ0QO0OQ0.OC0QQO0OQ0.BitCount=1)and(OCQ0QO0OQ0.OC0QQO0OQ0.BitCount=1)then begin
FreeAndNil(O0C0QO0OQ0.OC0QQO0OQ0);
O0C0QO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(OO00O00OQ0);
end
else begin
FreeAndNil(O0CCO00OQ0);
if OQQ0QO0OQ0.OC0QQO0OQ0.BitCount=1 then
O0CCO00OQ0:=TBigInteger.Create(OCQ0QO0OQ0.OC0QQO0OQ0)
else
if OCQ0QO0OQ0.OC0QQO0OQ0.BitCount=1 then
O0CCO00OQ0:=TBigInteger.Create(OQQ0QO0OQ0.OC0QQO0OQ0)
else
O0CCO00OQ0:=OQQ0QO0OQ0.OC0QQO0OQ0.ModMul(OCQ0QO0OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(O0C0QO0OQ0.OC0QQO0OQ0);
O0C0QO0OQ0.OC0QQO0OQ0:=O0CCO00OQ0.ModMul(OO00O00OQ0,OOCQQO0OQ0);
end;
FreeAndNil(O0CCO00OQ0);
O0CCO00OQ0:=OC00O00OQ0.ModMul(OC00O00OQ0,OOCQQO0OQ0);
OQ00O00OQ0:=OO00O00OQ0.ModMul(OO00O00OQ0,OOCQQO0OQ0);
FreeAndNil(OCCCO00OQ0);
OCCCO00OQ0:=O0O0O00OQ0.ModMul(OQ00O00OQ0,OOCQQO0OQ0);
FreeAndNil(O0C0QO0OQ0.OO0QQO0OQ0);
O0C0QO0OQ0.OO0QQO0OQ0:=O0CCO00OQ0.Minus(OCCCO00OQ0);
if O0C0QO0OQ0.OO0QQO0OQ0.IsNegative then begin
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0C0QO0OQ0.OO0QQO0OQ0;
O0C0QO0OQ0.OO0QQO0OQ0:=O0C0QO0OQ0.OO0QQO0OQ0.Add(OOCQQO0OQ0);
end;
FreeAndNil(O0CCO00OQ0);
O0CCO00OQ0:=O0C0QO0OQ0.OO0QQO0OQ0.Shl_(1);
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=O0CCO00OQ0.BarrettReduction(OOCQQO0OQ0);
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=OCCCO00OQ0.Minus(O0CCO00OQ0);
if O0CCO00OQ0.IsNegative then begin
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=O0CCO00OQ0.Add(OOCQQO0OQ0);
end;
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=O0CCO00OQ0.ModMul(OC00O00OQ0,OOCQQO0OQ0);
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=OO00O00OQ0;
OO00O00OQ0:=OQ00O00OQ0.ModMul(OO00O00OQ0,OOCQQO0OQ0);
FreeAndNil(OOCCO00OQ0);
OOCCO00OQ0:=OOO0O00OQ0.ModMul(OO00O00OQ0,OOCQQO0OQ0);
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=O0CCO00OQ0.Minus(OOCCO00OQ0);
if O0CCO00OQ0.IsNegative then begin
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=O0CCO00OQ0.Add(OOCQQO0OQ0);
end;
if O0CCO00OQ0.IsOdd then begin
FreeAndNil(OCQCO00OQ0);
OCQCO00OQ0:=O0CCO00OQ0;
O0CCO00OQ0:=O0CCO00OQ0.Add(OOCQQO0OQ0);
end;
FreeAndNil(O0C0QO0OQ0.OQ0QQO0OQ0);
O0C0QO0OQ0.OQ0QQO0OQ0:=O0CCO00OQ0.Shr_(1);
finally
O0CCO00OQ0.Free;
OOCCO00OQ0.Free;
OQCCO00OQ0.Free;
OCCCO00OQ0.Free;
O000O00OQ0.Free;
OO00O00OQ0.Free;
OQ00O00OQ0.Free;
OC00O00OQ0.Free;
O0O0O00OQ0.Free;
OOO0O00OQ0.Free;
OCQCO00OQ0.Free;
end;
end;
procedure O0QCO00OQ0.OOC0QO0OQ0(OQC0QO0OQ0:O00QQO0OQ0;OCC0QO0OQ0:O00QQO0OQ0);
var
OQO0O00OQ0,OCO0O00OQ0,O0Q0O00OQ0,OOQ0O00OQ0,OQQ0O00OQ0:TBigInteger;
begin
if OQC0QO0OQ0.OC0QQO0OQ0.BitCount=0 then begin
OO00QO0OQ0(OCC0QO0OQ0);
Exit;
end;
OQO0O00OQ0:=nil;
OCO0O00OQ0:=nil;
O0Q0O00OQ0:=nil;
OOQ0O00OQ0:=nil;
OQQ0O00OQ0:=nil;
try
if OQC0QO0OQ0.OC0QQO0OQ0.BitCount=1 then begin
FreeAndNil(OCO0O00OQ0);
OCO0O00OQ0:=OQC0QO0OQ0.OO0QQO0OQ0.ModMul(OQC0QO0OQ0.OO0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(O0Q0O00OQ0);
O0Q0O00OQ0:=OCO0O00OQ0.Shl_(1);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OCO0O00OQ0;
OCO0O00OQ0:=OCO0O00OQ0.Add(O0Q0O00OQ0);
FreeAndNil(O0Q0O00OQ0);
O0Q0O00OQ0:=OCO0O00OQ0.Add(OQCQQO0OQ0);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=O0Q0O00OQ0;
O0Q0O00OQ0:=O0Q0O00OQ0.BarrettReduction(OOCQQO0OQ0);
end
else begin
FreeAndNil(OCO0O00OQ0);
OCO0O00OQ0:=OQC0QO0OQ0.OO0QQO0OQ0.ModMul(OQC0QO0OQ0.OO0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(O0Q0O00OQ0);
O0Q0O00OQ0:=OCO0O00OQ0.Shl_(1);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OCO0O00OQ0;
OCO0O00OQ0:=OCO0O00OQ0.Add(O0Q0O00OQ0);
FreeAndNil(O0Q0O00OQ0);
O0Q0O00OQ0:=OQC0QO0OQ0.OC0QQO0OQ0.ModMul(OQC0QO0OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=O0Q0O00OQ0;
O0Q0O00OQ0:=O0Q0O00OQ0.ModMul(O0Q0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=O0Q0O00OQ0;
O0Q0O00OQ0:=O0Q0O00OQ0.ModMul(OQCQQO0OQ0,OOCQQO0OQ0);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=O0Q0O00OQ0;
O0Q0O00OQ0:=O0Q0O00OQ0.Add(OCO0O00OQ0);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=O0Q0O00OQ0;
O0Q0O00OQ0:=O0Q0O00OQ0.BarrettReduction(OOCQQO0OQ0);
end;
FreeAndNil(OCO0O00OQ0);
if OQC0QO0OQ0.OC0QQO0OQ0.BitCount=1 then
OCO0O00OQ0:=TBigInteger.Create(OQC0QO0OQ0.OQ0QQO0OQ0)
else
OCO0O00OQ0:=OQC0QO0OQ0.OQ0QQO0OQ0.ModMul(OQC0QO0OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(OCC0QO0OQ0.OC0QQO0OQ0);
OCC0QO0OQ0.OC0QQO0OQ0:=OCO0O00OQ0.Shl_(1);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OCC0QO0OQ0.OC0QQO0OQ0;
OCC0QO0OQ0.OC0QQO0OQ0:=OCC0QO0OQ0.OC0QQO0OQ0.BarrettReduction(OOCQQO0OQ0);
OQQ0O00OQ0:=OQC0QO0OQ0.OQ0QQO0OQ0.ModMul(OQC0QO0OQ0.OQ0QQO0OQ0,OOCQQO0OQ0);
OOQ0O00OQ0:=OQC0QO0OQ0.OO0QQO0OQ0.ModMul(OQQ0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OOQ0O00OQ0;
OOQ0O00OQ0:=OOQ0O00OQ0.Shl_(2);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OOQ0O00OQ0;
OOQ0O00OQ0:=OOQ0O00OQ0.BarrettReduction(OOCQQO0OQ0);
FreeAndNil(OCO0O00OQ0);
OCO0O00OQ0:=OOQ0O00OQ0.Shl_(1);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=O0Q0O00OQ0.ModMul(O0Q0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OCC0QO0OQ0.OO0QQO0OQ0);
OCC0QO0OQ0.OO0QQO0OQ0:=OQO0O00OQ0.Minus(OCO0O00OQ0);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OCC0QO0OQ0.OO0QQO0OQ0;
OCC0QO0OQ0.OO0QQO0OQ0:=OCC0QO0OQ0.OO0QQO0OQ0.Mod_(OOCQQO0OQ0);
FreeAndNil(OCO0O00OQ0);
OCO0O00OQ0:=OQQ0O00OQ0.ModMul(OQQ0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OQQ0O00OQ0);
OQQ0O00OQ0:=OCO0O00OQ0.Shl_(3);
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OQQ0O00OQ0;
OQQ0O00OQ0:=OQQ0O00OQ0.BarrettReduction(OOCQQO0OQ0);
FreeAndNil(OCO0O00OQ0);
OCO0O00OQ0:=OOQ0O00OQ0.Minus(OCC0QO0OQ0.OO0QQO0OQ0);
if OCO0O00OQ0.IsNegative then begin
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OCO0O00OQ0;
OCO0O00OQ0:=OCO0O00OQ0.Add(OOCQQO0OQ0);
end;
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=O0Q0O00OQ0.ModMul(OCO0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OCC0QO0OQ0.OQ0QQO0OQ0);
OCC0QO0OQ0.OQ0QQO0OQ0:=OQO0O00OQ0.Minus(OQQ0O00OQ0);
if OCC0QO0OQ0.OQ0QQO0OQ0.IsNegative then begin
FreeAndNil(OQO0O00OQ0);
OQO0O00OQ0:=OCC0QO0OQ0.OQ0QQO0OQ0;
OCC0QO0OQ0.OQ0QQO0OQ0:=OCC0QO0OQ0.OQ0QQO0OQ0.Add(OOCQQO0OQ0);
end;
finally
OCO0O00OQ0.Free;
O0Q0O00OQ0.Free;
OOQ0O00OQ0.Free;
OQQ0O00OQ0.Free;
OQO0O00OQ0.Free;
end;
end;
function O0QCO00OQ0.OC0OQO0OQ0(O0OOQO0OQ0:O00QQO0OQ0):boolean;
var
OCC0O00OQ0:TBigInteger;
O00OO00OQ0,OO0OO00OQ0,OQ0OO00OQ0,OC0OO00OQ0,O0OOO00OQ0,OOOOO00OQ0:TBigInteger;
begin
Assert(OOCQQO0OQ0<>nil);
Assert(OQCQQO0OQ0<>nil);
Assert(OCCQQO0OQ0<>nil);
Assert(O0OOQO0OQ0.OO0QQO0OQ0<>nil);
Assert(O0OOQO0OQ0.OQ0QQO0OQ0<>nil);
O00OO00OQ0:=nil;
OO0OO00OQ0:=nil;
OQ0OO00OQ0:=nil;
OC0OO00OQ0:=nil;
O0OOO00OQ0:=nil;
OOOOO00OQ0:=nil;
OCC0O00OQ0:=TBigInteger.Create(3);
try
O00OO00OQ0:=O0OOQO0OQ0.OO0QQO0OQ0.ModPow(OCC0O00OQ0,OOCQQO0OQ0);
OO0OO00OQ0:=OQCQQO0OQ0.ModMul(O0OOQO0OQ0.OO0QQO0OQ0,OOCQQO0OQ0);
OQ0OO00OQ0:=O00OO00OQ0.Add(OO0OO00OQ0);
OC0OO00OQ0:=OQ0OO00OQ0.Add(OCCQQO0OQ0);
O0OOO00OQ0:=OC0OO00OQ0.Mod_(OOCQQO0OQ0);
OOOOO00OQ0:=O0OOQO0OQ0.OQ0QQO0OQ0.ModMul(O0OOQO0OQ0.OQ0QQO0OQ0,OOCQQO0OQ0);
Result:=O0OOO00OQ0.Equal(OOOOO00OQ0);
finally
OCC0O00OQ0.Free;
O00OO00OQ0.Free;
OO0OO00OQ0.Free;
OQ0OO00OQ0.Free;
OC0OO00OQ0.Free;
O0OOO00OQ0.Free;
OOOOO00OQ0.Free;
end;
end;
procedure O0QCO00OQ0.O00OQO0OQ0(OO0OQO0OQ0:O00QQO0OQ0);
var
OCQ0O00OQ0,O0C0O00OQ0,OOC0O00OQ0,OQC0O00OQ0:TBigInteger;
begin
if(OO0OQO0OQ0.OC0QQO0OQ0=nil)or(OO0OQO0OQ0.OC0QQO0OQ0.BitCount=0)then
raise EScError.Create(sePointInfinitive);
if OO0OQO0OQ0.OC0QQO0OQ0.BitCount=1 then
Exit;
O0C0O00OQ0:=nil;
OOC0O00OQ0:=nil;
OQC0O00OQ0:=nil;
OCQ0O00OQ0:=OO0OQO0OQ0.OC0QQO0OQ0.ModInverse(OOCQQO0OQ0);
try
O0C0O00OQ0:=OCQ0O00OQ0.ModMul(OCQ0O00OQ0,OOCQQO0OQ0);
OQC0O00OQ0:=OO0OQO0OQ0.OO0QQO0OQ0;
OO0OQO0OQ0.OO0QQO0OQ0:=OO0OQO0OQ0.OO0QQO0OQ0.ModMul(O0C0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OQC0O00OQ0);
OOC0O00OQ0:=O0C0O00OQ0.ModMul(OCQ0O00OQ0,OOCQQO0OQ0);
OQC0O00OQ0:=OO0OQO0OQ0.OQ0QQO0OQ0;
OO0OQO0OQ0.OQ0QQO0OQ0:=OO0OQO0OQ0.OQ0QQO0OQ0.ModMul(OOC0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OQC0O00OQ0);
FreeAndNil(OO0OQO0OQ0.OC0QQO0OQ0);
OO0OQO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(1);
finally
OCQ0O00OQ0.Free;
O0C0O00OQ0.Free;
OOC0O00OQ0.Free;
OQC0O00OQ0.Free;
end;
end;
procedure OQOOO00OQ0.OQO0QO0OQ0;
begin
if(OOCQQO0OQ0=nil)or(OQCQQO0OQ0=nil)then
raise EScError.Create(seECCryptographyNotInitialized);
OOCQQO0OQ0.PrepareForGF2mCalc;
OCOOO00OQ0:=OQCQQO0OQ0.BitCount;
end;
procedure OQOOO00OQ0.OCO0QO0OQ0(O0Q0QO0OQ0:O00QQO0OQ0);
begin
if O0Q0QO0OQ0.OC0QQO0OQ0.BitCount=0 then
Exit;
if O0Q0QO0OQ0.OC0QQO0OQ0.BitCount>1 then
O00OQO0OQ0(O0Q0QO0OQ0);
O0Q0QO0OQ0.OQ0QQO0OQ0.XorSelf(O0Q0QO0OQ0.OO0QQO0OQ0);
end;
procedure OQOOO00OQ0.OOQ0QO0OQ0(OQQ0QO0OQ0,OCQ0QO0OQ0:O00QQO0OQ0;O0C0QO0OQ0:O00QQO0OQ0);
begin
if(OCOOO00OQ0=0)or(OCOOO00OQ0=1)then
O0QQ000OQ0(OQQ0QO0OQ0,OCQ0QO0OQ0,O0C0QO0OQ0)
else
O0QOO00OQ0(OQQ0QO0OQ0,OCQ0QO0OQ0,O0C0QO0OQ0);
end;
procedure OQOOO00OQ0.OOC0QO0OQ0(OQC0QO0OQ0:O00QQO0OQ0;OCC0QO0OQ0:O00QQO0OQ0);
begin
if(OCOOO00OQ0=0)or(OCOOO00OQ0=1)then
OQOC000OQ0(OQC0QO0OQ0,OCC0QO0OQ0)
else
OOOQ000OQ0(OQC0QO0OQ0,OCC0QO0OQ0);
end;
procedure OQOOO00OQ0.O0QOO00OQ0(OOQOO00OQ0,OQQOO00OQ0:O00QQO0OQ0;OCQOO00OQ0:O00QQO0OQ0);
var
O0COO00OQ0,OOCOO00OQ0,OQCOO00OQ0,OCCOO00OQ0,O00Q000OQ0,OO0Q000OQ0,OQ0Q000OQ0,OC0Q000OQ0,O0OQ000OQ0:TBigInteger;
begin
if OOQOO00OQ0.OC0QQO0OQ0.BitCount=0 then begin
OQCCQO0OQ0(OQQOO00OQ0,OCQOO00OQ0);
Exit;
end;
if OQQOO00OQ0.OC0QQO0OQ0.BitCount=0 then begin
OQCCQO0OQ0(OOQOO00OQ0,OCQOO00OQ0);
Exit;
end;
OQCOO00OQ0:=nil;
OCCOO00OQ0:=nil;
O00Q000OQ0:=nil;
OO0Q000OQ0:=nil;
OQ0Q000OQ0:=nil;
OC0Q000OQ0:=nil;
O0OQ000OQ0:=nil;
O0COO00OQ0:=TBigInteger.Create(OOQOO00OQ0.OO0QQO0OQ0);
OOCOO00OQ0:=TBigInteger.Create(OOQOO00OQ0.OQ0QQO0OQ0);
try
if not O0COO00OQ0.Equal(OQQOO00OQ0.OO0QQO0OQ0)then begin
OQCOO00OQ0:=O0COO00OQ0.Xor_(OQQOO00OQ0.OO0QQO0OQ0);
OCCOO00OQ0:=OOCOO00OQ0.Xor_(OQQOO00OQ0.OQ0QQO0OQ0);
OC0Q000OQ0:=OCCOO00OQ0.ModDiv_GF2m(OQCOO00OQ0,OOCQQO0OQ0);
OO0Q000OQ0:=OC0Q000OQ0.ModSqr_GF2m(OOCQQO0OQ0);
OO0Q000OQ0.XorSelf(OC0Q000OQ0);
OO0Q000OQ0.XorSelf(OQCQQO0OQ0);
FreeAndNil(OCQOO00OQ0.OO0QQO0OQ0);
OCQOO00OQ0.OO0QQO0OQ0:=OO0Q000OQ0.Xor_(OQCOO00OQ0);
OQ0Q000OQ0:=O0COO00OQ0.Xor_(OCQOO00OQ0.OO0QQO0OQ0);
O0OQ000OQ0:=OQ0Q000OQ0;
OQ0Q000OQ0:=OQ0Q000OQ0.ModMul_GF2m(OC0Q000OQ0,OOCQQO0OQ0);
FreeAndNil(O0OQ000OQ0);
OQ0Q000OQ0.XorSelf(OCQOO00OQ0.OO0QQO0OQ0);
FreeAndNil(OCQOO00OQ0.OQ0QQO0OQ0);
OCQOO00OQ0.OQ0QQO0OQ0:=OQ0Q000OQ0.Xor_(OOCOO00OQ0);
end
else begin
if not OOCOO00OQ0.Equal(OQQOO00OQ0.OQ0QQO0OQ0)or(OQQOO00OQ0.OO0QQO0OQ0.BitCount=0)then begin
OO00QO0OQ0(OCQOO00OQ0);
Exit;
end;
OC0Q000OQ0:=OOCOO00OQ0.ModDiv_GF2m(O0COO00OQ0,OOCQQO0OQ0);
OC0Q000OQ0.XorSelf(O0COO00OQ0);
OO0Q000OQ0:=OC0Q000OQ0.ModSqr_GF2m(OOCQQO0OQ0);
OO0Q000OQ0.XorSelf(OC0Q000OQ0);
FreeAndNil(OCQOO00OQ0.OO0QQO0OQ0);
OCQOO00OQ0.OO0QQO0OQ0:=OO0Q000OQ0.Xor_(OQCQQO0OQ0);
OQ0Q000OQ0:=OCQOO00OQ0.OO0QQO0OQ0.ModMul_GF2m(OC0Q000OQ0,OOCQQO0OQ0);
OQ0Q000OQ0.XorSelf(OCQOO00OQ0.OO0QQO0OQ0);
O00Q000OQ0:=O0COO00OQ0.ModSqr_GF2m(OOCQQO0OQ0);
FreeAndNil(OCQOO00OQ0.OQ0QQO0OQ0);
OCQOO00OQ0.OQ0QQO0OQ0:=OQ0Q000OQ0.Xor_(O00Q000OQ0);
end;
FreeAndNil(OCQOO00OQ0.OC0QQO0OQ0);
OCQOO00OQ0.OC0QQO0OQ0:=TBigInteger.Create(1);
finally
O0COO00OQ0.Free;
OOCOO00OQ0.Free;
OQCOO00OQ0.Free;
OCCOO00OQ0.Free;
O00Q000OQ0.Free;
OO0Q000OQ0.Free;
OQ0Q000OQ0.Free;
OC0Q000OQ0.Free;
O0OQ000OQ0.Free;
end;
end;
procedure OQOOO00OQ0.OOOQ000OQ0(OQOQ000OQ0:O00QQO0OQ0;OCOQ000OQ0:O00QQO0OQ0);
begin
O0QOO00OQ0(OQOQ000OQ0,OQOQ000OQ0,OCOQ000OQ0);
end;
procedure OQOOO00OQ0.O0QQ000OQ0(OOQQ000OQ0,OQQQ000OQ0:O00QQO0OQ0;OCQQ000OQ0:O00QQO0OQ0);
var
O0CQ000OQ0,OOCQ000OQ0,OQCQ000OQ0,OCCQ000OQ0,O00C000OQ0,OO0C000OQ0,OQ0C000OQ0,OC0C000OQ0,O0OC000OQ0,OOOC000OQ0:TBigInteger;
begin
if OOQQ000OQ0.OC0QQO0OQ0.BitCount=0 then begin
OQCCQO0OQ0(OQQQ000OQ0,OCQQ000OQ0);
Exit;
end;
if OQQQ000OQ0.OC0QQO0OQ0.BitCount=0 then begin
OQCCQO0OQ0(OOQQ000OQ0,OCQQ000OQ0);
Exit;
end;
if OQQQ000OQ0.OC0QQO0OQ0.BitCount<>1 then
raise EScError.Create(seInvalidInputArgs);
O0CQ000OQ0:=nil;
OOCQ000OQ0:=nil;
OQCQ000OQ0:=nil;
OCCQ000OQ0:=nil;
O00C000OQ0:=nil;
OO0C000OQ0:=nil;
OQ0C000OQ0:=nil;
OC0C000OQ0:=nil;
O0OC000OQ0:=nil;
OOOC000OQ0:=nil;
try
OQCQ000OQ0:=OOQQ000OQ0.OC0QQO0OQ0.ModMul_GF2m(OQQQ000OQ0.OO0QQO0OQ0,OOCQQO0OQ0);
OO0C000OQ0:=OOQQ000OQ0.OO0QQO0OQ0.Xor_(OQCQ000OQ0);
FreeAndNil(OQCQ000OQ0);
OQCQ000OQ0:=OOQQ000OQ0.OC0QQO0OQ0.ModSqr_GF2m(OOCQQO0OQ0);
OQ0C000OQ0:=OOQQ000OQ0.OC0QQO0OQ0.ModMul_GF2m(OO0C000OQ0,OOCQQO0OQ0);
OCCQ000OQ0:=OQCQ000OQ0.ModMul_GF2m(OQQQ000OQ0.OQ0QQO0OQ0,OOCQQO0OQ0);
O00C000OQ0:=OOQQ000OQ0.OQ0QQO0OQ0.Xor_(OCCQ000OQ0);
if OO0C000OQ0.BitCount=0 then begin
if O00C000OQ0.BitCount=0 then
OQOC000OQ0(OQQQ000OQ0,OCQQ000OQ0)
else
OO00QO0OQ0(OCQQ000OQ0);
Exit;
end;
O0CQ000OQ0:=TBigInteger.Create(OQQQ000OQ0.OO0QQO0OQ0);
OOCQ000OQ0:=TBigInteger.Create(OQQQ000OQ0.OQ0QQO0OQ0);
FreeAndNil(OCQQ000OQ0.OC0QQO0OQ0);
OCQQ000OQ0.OC0QQO0OQ0:=OQ0C000OQ0.ModSqr_GF2m(OOCQQO0OQ0);
O0OC000OQ0:=OQ0C000OQ0.ModMul_GF2m(O00C000OQ0,OOCQQO0OQ0);
if OQCQQO0OQ0.BitCount=1 then
OQ0C000OQ0.XorSelf(OQCQ000OQ0);
FreeAndNil(OQCQ000OQ0);
OQCQ000OQ0:=OO0C000OQ0.ModSqr_GF2m(OOCQQO0OQ0);
OC0C000OQ0:=OQCQ000OQ0.ModMul_GF2m(OQ0C000OQ0,OOCQQO0OQ0);
FreeAndNil(OQCQ000OQ0);
OQCQ000OQ0:=O00C000OQ0.ModSqr_GF2m(OOCQQO0OQ0);
FreeAndNil(OCQQ000OQ0.OO0QQO0OQ0);
OCQQ000OQ0.OO0QQO0OQ0:=OC0C000OQ0.Xor_(OQCQ000OQ0);
OCQQ000OQ0.OO0QQO0OQ0.XorSelf(O0OC000OQ0);
FreeAndNil(OQCQ000OQ0);
OQCQ000OQ0:=O0CQ000OQ0.ModMul_GF2m(OCQQ000OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
OQCQ000OQ0.XorSelf(OCQQ000OQ0.OO0QQO0OQ0);
O0OC000OQ0.XorSelf(OCQQ000OQ0.OC0QQO0OQ0);
FreeAndNil(OCQQ000OQ0.OQ0QQO0OQ0);
OCQQ000OQ0.OQ0QQO0OQ0:=O0OC000OQ0.ModMul_GF2m(OQCQ000OQ0,OOCQQO0OQ0);
FreeAndNil(OQCQ000OQ0);
OQCQ000OQ0:=O0CQ000OQ0.Xor_(OOCQ000OQ0);
OOOC000OQ0:=OCQQ000OQ0.OC0QQO0OQ0.ModSqr_GF2m(OOCQQO0OQ0);
FreeAndNil(OCCQ000OQ0);
OCCQ000OQ0:=OOOC000OQ0.ModMul_GF2m(OQCQ000OQ0,OOCQQO0OQ0);
OCQQ000OQ0.OQ0QQO0OQ0.XorSelf(OCCQ000OQ0);
finally
O0CQ000OQ0.Free;
OOCQ000OQ0.Free;
OQCQ000OQ0.Free;
OCCQ000OQ0.Free;
O00C000OQ0.Free;
OO0C000OQ0.Free;
OQ0C000OQ0.Free;
OC0C000OQ0.Free;
O0OC000OQ0.Free;
OOOC000OQ0.Free;
end;
end;
procedure OQOOO00OQ0.OQOC000OQ0(OCOC000OQ0:O00QQO0OQ0;O0QC000OQ0:O00QQO0OQ0);
var
OOQC000OQ0,OQQC000OQ0,OCQC000OQ0:TBigInteger;
begin
if OCOC000OQ0.OC0QQO0OQ0.BitCount=0 then begin
OO00QO0OQ0(O0QC000OQ0);
Exit;
end;
OOQC000OQ0:=nil;
OQQC000OQ0:=nil;
OCQC000OQ0:=nil;
try
OOQC000OQ0:=OCOC000OQ0.OC0QQO0OQ0.ModSqr_GF2m(OOCQQO0OQ0);
OQQC000OQ0:=OCOC000OQ0.OO0QQO0OQ0.ModSqr_GF2m(OOCQQO0OQ0);
FreeAndNil(O0QC000OQ0.OC0QQO0OQ0);
O0QC000OQ0.OC0QQO0OQ0:=OOQC000OQ0.ModMul_GF2m(OQQC000OQ0,OOCQQO0OQ0);
OCQC000OQ0:=OQQC000OQ0.ModSqr_GF2m(OOCQQO0OQ0);
FreeAndNil(OQQC000OQ0);
OQQC000OQ0:=OOQC000OQ0;
OOQC000OQ0:=OOQC000OQ0.ModSqr_GF2m(OOCQQO0OQ0);
FreeAndNil(OQQC000OQ0);
OQQC000OQ0:=OOQC000OQ0.ModMul_GF2m(OCCQQO0OQ0,OOCQQO0OQ0);
FreeAndNil(O0QC000OQ0.OO0QQO0OQ0);
O0QC000OQ0.OO0QQO0OQ0:=OCQC000OQ0.Xor_(OQQC000OQ0);
FreeAndNil(OOQC000OQ0);
OOQC000OQ0:=OCOC000OQ0.OQ0QQO0OQ0.ModSqr_GF2m(OOCQQO0OQ0);
if OQCQQO0OQ0.BitCount=1 then
OOQC000OQ0.XorSelf(O0QC000OQ0.OC0QQO0OQ0);
OOQC000OQ0.XorSelf(OQQC000OQ0);
FreeAndNil(OCQC000OQ0);
OCQC000OQ0:=O0QC000OQ0.OO0QQO0OQ0.ModMul_GF2m(OOQC000OQ0,OOCQQO0OQ0);
FreeAndNil(OOQC000OQ0);
OOQC000OQ0:=OQQC000OQ0.ModMul_GF2m(O0QC000OQ0.OC0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(O0QC000OQ0.OQ0QQO0OQ0);
O0QC000OQ0.OQ0QQO0OQ0:=OCQC000OQ0.Xor_(OOQC000OQ0);
finally
OOQC000OQ0.Free;
OQQC000OQ0.Free;
OCQC000OQ0.Free;
end;
end;
function OQOOO00OQ0.OC0OQO0OQ0(O0OOQO0OQ0:O00QQO0OQ0):boolean;
var
OOOOO00OQ0,O0CC000OQ0,OOCC000OQ0:TBigInteger;
begin
Assert(OOCQQO0OQ0<>nil);
Assert(OQCQQO0OQ0<>nil);
Assert(OCCQQO0OQ0<>nil);
Assert(O0OOQO0OQ0.OO0QQO0OQ0<>nil);
Assert(O0OOQO0OQ0.OQ0QQO0OQ0<>nil);
O0CC000OQ0:=nil;
OOOOO00OQ0:=nil;
OOCC000OQ0:=nil;
try
O0CC000OQ0:=O0OOQO0OQ0.OO0QQO0OQ0.Xor_(OQCQQO0OQ0);
OOCC000OQ0:=O0CC000OQ0;
O0CC000OQ0:=O0CC000OQ0.ModMul_GF2m(O0OOQO0OQ0.OO0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(OOCC000OQ0);
O0CC000OQ0.XorSelf(O0OOQO0OQ0.OQ0QQO0OQ0);
OOCC000OQ0:=O0CC000OQ0;
O0CC000OQ0:=O0CC000OQ0.ModMul_GF2m(O0OOQO0OQ0.OO0QQO0OQ0,OOCQQO0OQ0);
FreeAndNil(OOCC000OQ0);
O0CC000OQ0.XorSelf(OCCQQO0OQ0);
OOOOO00OQ0:=O0OOQO0OQ0.OQ0QQO0OQ0.ModSqr_GF2m(OOCQQO0OQ0);
O0CC000OQ0.XorSelf(OOOOO00OQ0);
Result:=O0CC000OQ0.BitCount=0;
finally
O0CC000OQ0.Free;
OOOOO00OQ0.Free;
OOCC000OQ0.Free;
end;
end;
procedure OQOOO00OQ0.O00OQO0OQ0(OO0OQO0OQ0:O00QQO0OQ0);
var
OCQ0O00OQ0,O0C0O00OQ0,OQC0O00OQ0:TBigInteger;
begin
if(OO0OQO0OQ0.OC0QQO0OQ0=nil)or(OO0OQO0OQ0.OC0QQO0OQ0.BitCount=0)then
raise EScError.Create(sePointInfinitive);
if OO0OQO0OQ0.OC0QQO0OQ0.BitCount=1 then
Exit;
O0C0O00OQ0:=nil;
OQC0O00OQ0:=nil;
OCQ0O00OQ0:=OO0OQO0OQ0.OC0QQO0OQ0.ModInv_GF2m(OOCQQO0OQ0);
try
OQC0O00OQ0:=OO0OQO0OQ0.OO0QQO0OQ0;
OO0OQO0OQ0.OO0QQO0OQ0:=OO0OQO0OQ0.OO0QQO0OQ0.ModMul_GF2m(OCQ0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OQC0O00OQ0);
O0C0O00OQ0:=OCQ0O00OQ0.ModMul_GF2m(OCQ0O00OQ0,OOCQQO0OQ0);
OQC0O00OQ0:=OO0OQO0OQ0.OQ0QQO0OQ0;
OO0OQO0OQ0.OQ0QQO0OQ0:=OO0OQO0OQ0.OQ0QQO0OQ0.ModMul_GF2m(O0C0O00OQ0,OOCQQO0OQ0);
FreeAndNil(OQC0O00OQ0);
FreeAndNil(OO0OQO0OQ0.OC0QQO0OQ0);
OO0OQO0OQ0.OC0QQO0OQ0:=TBigInteger.Create(1);
finally
OCQ0O00OQ0.Free;
O0C0O00OQ0.Free;
OQC0O00OQ0.Free;
end;
end;
function OQOOO00OQ0.OQ0OQO0OQ0:boolean;
begin
Result:=True;
end;
constructor OQ00000OQ0.Create;
begin
inherited Create;
OC0CCO0OQ0:=32;
OC00000OQ0:=OQCC000OQ0;
end;
constructor OQ00000OQ0.Create(const OQCCCO0OQ0:TScECParameters);
begin
Create;
end;
function OQ00000OQ0.Equals(OO00CO0OQ0:OO0CCO0OQ0):boolean;
begin
Result:=(ClassType=OO00CO0OQ0.ClassType)and
(OC0CCO0OQ0=OO00CO0OQ0.OC0CCO0OQ0);
end;
procedure OQ00000OQ0.OCCCCO0OQ0(O000CO0OQ0:OO0CCO0OQ0);
begin
if O000CO0OQ0=nil then
Exit;
inherited OCCCCO0OQ0(O000CO0OQ0);
end;
function OQ00000OQ0.OQ00CO0OQ0:O0COCO0OQ0;
begin
Result:=OOOQQO0OQ0;
end;
function OQ00000OQ0.OC00CO0OQ0:TPersistentClass;
begin
Result:=T32BytesField;
end;
function OQ00000OQ0.O0CCCO0OQ0:integer;
begin
Result:=OC0CCO0OQ0*8;
end;
procedure OQ00000OQ0.OOQCCO0OQ0(var OQQCCO0OQ0:OCOQQO0OQ0;OCQCCO0OQ0:IScRandom=nil);
begin
if OCQCCO0OQ0=nil then
OCQCCO0OQ0:=OCCQ0QQ0Q0;
if OCQCCO0OQ0=nil then
raise Exception.Create(SInternalError);
OQQCCO0OQ0.OCQQQO0OQ0:=T32BytesField.Create;
OCQCCO0OQ0.Random(@T32BytesField(OQQCCO0OQ0.OCQQQO0OQ0).OOQOCO0OQ0[0],sizeof(OCQCQC0OQ0));
OQQCCO0OQ0.OQQQQO0OQ0:=O0O0000OQ0(T32BytesField(OQQCCO0OQ0.OCQQQO0OQ0));
end;
function OQ00000OQ0.O0O0000OQ0(OOO0000OQ0:T32BytesField):OOOQQO0OQ0;
const
OQO0000OQ0:OCQCQC0OQ0=(9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
begin
Result:=OOOQQO0OQ0.Create;
try
case OC00000OQ0 of
OQCC000OQ0:
O0CCQC0OQ0(Result.OQOQQO0OQ0,OOO0000OQ0.OOQOCO0OQ0,OQO0000OQ0);
OCCC000OQ0:
OCQ0QC0OQ0(OOO0000OQ0.OOQOCO0OQ0,Result.OQOQQO0OQ0);
else
raise EScError.Create(sePreHashTypeNotSupported);
end;
except
Result.Free;
raise;
end;
end;
function OQ00000OQ0.O0O0CO0OQ0(OOO0CO0OQ0:OOCOCO0OQ0;OQO0CO0OQ0:TObject):OOCOCO0OQ0;
begin
if not IsClass(OOO0CO0OQ0,OOOQQO0OQ0)or not IsClass(OQO0CO0OQ0,T32BytesField)then
raise EScError.Create(seInvalidInputArgs);
Result:=OOOQQO0OQ0.Create;
try
O0CCQC0OQ0(OOOQQO0OQ0(Result).OQOQQO0OQ0,T32BytesField(OQO0CO0OQ0).OOQOCO0OQ0,OOOQQO0OQ0(OOO0CO0OQ0).OQOQQO0OQ0);
except
Result.Free;
raise;
end;
end;
function OQ00000OQ0.OCO0CO0OQ0(const O0Q0CO0OQ0:TBytes;OOQ0CO0OQ0,OQQ0CO0OQ0:integer):OOCOCO0OQ0;
begin
if OQQ0CO0OQ0<>OC0CCO0OQ0 then
raise EScError.Create(seWrongECPointFormat);
Result:=OOOQQO0OQ0.Create;
try
Move(O0Q0CO0OQ0[OOQ0CO0OQ0],OOOQQO0OQ0(Result).OQOQQO0OQ0[0],OQQ0CO0OQ0);
except
Result.Free;
raise;
end;
end;
function OQ00000OQ0.OCQ0CO0OQ0(O0C0CO0OQ0:OOCOCO0OQ0):TBytes;
begin
if not IsClass(O0C0CO0OQ0,OOOQQO0OQ0)then
raise EScError.Create(seInvalidInputArgs);
SetLength(Result,OC0CCO0OQ0);
Move(OOOQQO0OQ0(O0C0CO0OQ0).OQOQQO0OQ0[0],Result[0],OC0CCO0OQ0);
end;
function OQ00000OQ0.OOC0CO0OQ0(const OCO0000OQ0:TBytes;OCC0CO0OQ0:TObject;O00OCO0OQ0:OOCOCO0OQ0):TBytes;
begin
if not IsClass(O00OCO0OQ0,OOOQQO0OQ0)or not IsClass(OCC0CO0OQ0,T32BytesField)then
raise EScError.Create(seInvalidInputArgs);
Result:=OO0OQC0OQ0(OCO0000OQ0,T32BytesField(OCC0CO0OQ0).OOQOCO0OQ0,OOOQQO0OQ0(O00OCO0OQ0).OQOQQO0OQ0);
end;
function OQ00000OQ0.OO0OCO0OQ0(const O0Q0000OQ0,OC0OCO0OQ0:TBytes;O0OOCO0OQ0:OOCOCO0OQ0):boolean;
begin
if not IsClass(O0OOCO0OQ0,OOOQQO0OQ0)then
raise EScError.Create(seInvalidInputArgs);
Result:=OCQOQC0OQ0(O0Q0000OQ0,OC0OCO0OQ0,OOOQQO0OQ0(O0OOCO0OQ0).OQOQQO0OQ0);
end;
class function OOQ0000OQ0.OQOO000OQ0(const OCOO000OQ0:OOOOCOQOQ0;
const O0QO000OQ0:string;const OOQO000OQ0:TBytes;
OQQO000OQ0:integer;OCQO000OQ0:integer;O0CO000OQ0:boolean):TBytes;
var
OOCO000OQ0:THashAlgorithm;
OQCO000OQ0,OCCO000OQ0,O00QC00OQ0,OO0QC00OQ0:TBytes;
OQ0QC00OQ0,OC0QC00OQ0,O0OQC00OQ0:integer;
OOOQC00OQ0:integer;
begin
OOCO000OQ0:=OOCQCC0OQ0.OOOCCC0OQ0(OCOO000OQ0);
try
OQCO000OQ0:=Encoding.UTF8.GetBytes(O0QO000OQ0);
OQ0QC00OQ0:=Min(Length(OOQO000OQ0),8);
SetLength(OCCO000OQ0,Length(OQCO000OQ0)+OQ0QC00OQ0);
if Length(OQCO000OQ0)>0 then
Move(OQCO000OQ0[0],OCCO000OQ0[0],Length(OQCO000OQ0));
if OQ0QC00OQ0>0 then
Move(OOQO000OQ0[0],OCCO000OQ0[Length(OQCO000OQ0)],OQ0QC00OQ0);
OC0QC00OQ0:=OOCO000OQ0.HashSize;
SetLength(Result,OQQO000OQ0);
O00QC00OQ0:=OCCO000OQ0;
for OOOQC00OQ0:=0 to OCQO000OQ0-1 do
O00QC00OQ0:=OOCO000OQ0.ComputeHash(O00QC00OQ0);
Move(O00QC00OQ0[0],Result[0],Min(OQQO000OQ0,OC0QC00OQ0));
if OC0QC00OQ0<OQQO000OQ0 then begin
SetLength(OO0QC00OQ0,Length(OCCO000OQ0)+OC0QC00OQ0);
if Length(OCCO000OQ0)>0 then
if O0CO000OQ0 then
Move(OCCO000OQ0[0],OO0QC00OQ0[0],Length(OCCO000OQ0))
else
Move(OCCO000OQ0[0],OO0QC00OQ0[OC0QC00OQ0],Length(OCCO000OQ0));
O0OQC00OQ0:=OC0QC00OQ0;
while O0OQC00OQ0<OQQO000OQ0 do begin
if O0CO000OQ0 then
Move(O00QC00OQ0[0],OO0QC00OQ0[Length(OCCO000OQ0)],OC0QC00OQ0)
else
Move(O00QC00OQ0[0],OO0QC00OQ0[0],OC0QC00OQ0);
O00QC00OQ0:=OO0QC00OQ0;
for OOOQC00OQ0:=0 to OCQO000OQ0-1 do
O00QC00OQ0:=OOCO000OQ0.ComputeHash(O00QC00OQ0);
Move(O00QC00OQ0[0],Result[O0OQC00OQ0],Min(OQQO000OQ0-O0OQC00OQ0,OC0QC00OQ0));
O0OQC00OQ0:=O0OQC00OQ0+OC0QC00OQ0;
end;
end;
finally
OOCO000OQ0.Free;
end;
if Length(OCCO000OQ0)>0 then
FillChar(OCCO000OQ0[0],Length(OCCO000OQ0),0);
if Length(OQCO000OQ0)>0 then
FillChar(OQCO000OQ0[0],Length(OQCO000OQ0),0);
FillChar(O00QC00OQ0[0],Length(O00QC00OQ0),0);
end;
class function OOQ0000OQ0.OQOQC00OQ0(const OCOQC00OQ0:string;const O0QQC00OQ0:TBytes;
OOQQC00OQ0:integer;OQQQC00OQ0:integer):TBytes;
var
OCQQC00OQ0:THMAC;
O0CQC00OQ0,OOCQC00OQ0:TBytes;
OQCQC00OQ0,OCCQC00OQ0,O00CC00OQ0:integer;
OO0CC00OQ0:cardinal;
OQ0CC00OQ0,OC0CC00OQ0:integer;
begin
OQCQC00OQ0:=Length(O0QQC00OQ0);
SetLength(O0CQC00OQ0,OQCQC00OQ0+4);
Buffer.BlockCopy(O0QQC00OQ0,0,O0CQC00OQ0,0,OQCQC00OQ0);
SetLength(Result,OOQQC00OQ0);
SetLength(OOCQC00OQ0,0);
O00CC00OQ0:=0;
OCQQC00OQ0:=THMAC.Create(THash_SHA1,Encoding.UTF8.GetBytes(OCOQC00OQ0));
try
OO0CC00OQ0:=1;
OCCQC00OQ0:=OCQQC00OQ0.HashSize;
while OOQQC00OQ0>0 do begin
if OOQQC00OQ0<OCCQC00OQ0 then
OCCQC00OQ0:=OOQQC00OQ0;
O0CQC00OQ0[OQCQC00OQ0]:=byte(OO0CC00OQ0 shr 24);
O0CQC00OQ0[OQCQC00OQ0+1]:=byte(OO0CC00OQ0 shr 16);
O0CQC00OQ0[OQCQC00OQ0+2]:=byte(OO0CC00OQ0 shr 8);
O0CQC00OQ0[OQCQC00OQ0+3]:=byte(OO0CC00OQ0);
OOCQC00OQ0:=OCQQC00OQ0.ComputeHash(O0CQC00OQ0);
Buffer.BlockCopy(OOCQC00OQ0,0,Result,O00CC00OQ0,OCCQC00OQ0);
for OQ0CC00OQ0:=1 to OQQQC00OQ0-1 do begin
OOCQC00OQ0:=OCQQC00OQ0.ComputeHash(OOCQC00OQ0);
for OC0CC00OQ0:=0 to OCCQC00OQ0-1 do
Result[O00CC00OQ0+OC0CC00OQ0]:=Result[O00CC00OQ0+OC0CC00OQ0]xor OOCQC00OQ0[OC0CC00OQ0];
end;
OOQQC00OQ0:=OOQQC00OQ0-OCCQC00OQ0;
O00CC00OQ0:=O00CC00OQ0+OCCQC00OQ0;
Inc(OO0CC00OQ0);
end;
finally
OCQQC00OQ0.Free;
end;
end;
class function OOQ0000OQ0.O0OCC00OQ0(const OOOCC00OQ0:OOOOCOQOQ0;
const OQOCC00OQ0:string;const OCOCC00OQ0:TBytes;
O0QCC00OQ0:OCOC0O0OQ0;OOQCC00OQ0:integer;
OQQCC00OQ0:integer):TBytes;
var
OCQCC00OQ0:THashAlgorithm;
O0CCC00OQ0,OOCCC00OQ0:integer;
OQCCC00OQ0:byte;
OCCCC00OQ0:TBytes;
O000C00OQ0,OO00C00OQ0:TBytes;
OQ00C00OQ0,OC00C00OQ0,O0O0C00OQ0,OOO0C00OQ0,OQO0C00OQ0:integer;
OCO0C00OQ0,O0Q0C00OQ0:integer;
OOQ0C00OQ0,OQQ0C00OQ0,OCQ0C00OQ0:TBytes;
O0C0C00OQ0,OOC0C00OQ0,OQC0C00OQ0,OCC0C00OQ0:TBigInteger;
O00OC00OQ0,OO0OC00OQ0:integer;
begin
OCQCC00OQ0:=OOCQCC0OQ0.OOOCCC0OQ0(OOOCC00OQ0);
try
O0CCC00OQ0:=OCQCC00OQ0.HashSize;
if O0CCC00OQ0>32 then
OOCCC00OQ0:=128
else
OOCCC00OQ0:=64;
case O0QCC00OQ0 of
O0OC0O0OQ0:
OQCCC00OQ0:=1;
OOOC0O0OQ0:
OQCCC00OQ0:=2;
OQOC0O0OQ0:
OQCCC00OQ0:=3;
else
raise ArgumentException.Create('KeyPurpose');
end;
{$IFNDEF VER10P}
SetLength(OCQ0C00OQ0,0);
SetLength(OOQ0C00OQ0,0);
SetLength(O000C00OQ0,0);
{$ENDIF}
SetLength(OCCCC00OQ0,OOCCC00OQ0);
FillChar(OCCCC00OQ0[0],Length(OCCCC00OQ0),OQCCC00OQ0);
OC00C00OQ0:=Length(OCOCC00OQ0);
OOO0C00OQ0:=((OC00C00OQ0+OOCCC00OQ0-1)div OOCCC00OQ0)*OOCCC00OQ0;
O000C00OQ0:=Encoding.BigEndianUnicode.GetBytes(OQOCC00OQ0);
OQ00C00OQ0:=Length(O000C00OQ0);
if OQ00C00OQ0>0 then begin
Inc(OQ00C00OQ0,2);
O0O0C00OQ0:=((OQ00C00OQ0+OOCCC00OQ0-1)div OOCCC00OQ0)*OOCCC00OQ0;
end
else
O0O0C00OQ0:=0;
OQO0C00OQ0:=OOO0C00OQ0+O0O0C00OQ0;
SetLength(OO00C00OQ0,OQO0C00OQ0);
OCO0C00OQ0:=0;
if OC00C00OQ0>0 then begin
while(OCO0C00OQ0+OC00C00OQ0)<=OOO0C00OQ0 do begin
Move(OCOCC00OQ0[0],OO00C00OQ0[OCO0C00OQ0],OC00C00OQ0);
Inc(OCO0C00OQ0,OC00C00OQ0);
end;
if OOO0C00OQ0-OCO0C00OQ0>0 then
Move(OCOCC00OQ0[0],OO00C00OQ0[OCO0C00OQ0],OOO0C00OQ0-OCO0C00OQ0);
OCO0C00OQ0:=OOO0C00OQ0;
end;
if OQ00C00OQ0>0 then begin
while(OCO0C00OQ0+OQ00C00OQ0)<=OQO0C00OQ0 do begin
Move(O000C00OQ0[0],OO00C00OQ0[OCO0C00OQ0],Length(O000C00OQ0));
Inc(OCO0C00OQ0,OQ00C00OQ0);
end;
if OQO0C00OQ0-OCO0C00OQ0>0 then
Move(O000C00OQ0[0],OO00C00OQ0[OCO0C00OQ0],OQO0C00OQ0-OCO0C00OQ0);
end;
SetLength(OQQ0C00OQ0,OOCCC00OQ0);
SetLength(Result,OQQCC00OQ0);
O0Q0C00OQ0:=0;
while True do begin
OCQCC00OQ0.TransformBlock(OCCCC00OQ0,0,OOCCC00OQ0);
OCQCC00OQ0.TransformFinalBlock(OO00C00OQ0,0,OQO0C00OQ0);
OOQ0C00OQ0:=OCQCC00OQ0.Hash;
OCQCC00OQ0.Initialize;
for O00OC00OQ0:=0 to OOQCC00OQ0-2 do
OOQ0C00OQ0:=OCQCC00OQ0.ComputeHash(OOQ0C00OQ0);
Move(OOQ0C00OQ0[0],Result[O0Q0C00OQ0],Min(OQQCC00OQ0,O0CCC00OQ0));
if O0CCC00OQ0>=OQQCC00OQ0 then
Break;
Dec(OQQCC00OQ0,O0CCC00OQ0);
Inc(O0Q0C00OQ0,O0CCC00OQ0);
OCO0C00OQ0:=0;
while(OCO0C00OQ0+O0CCC00OQ0)<=OOCCC00OQ0 do begin
Move(OOQ0C00OQ0[0],OQQ0C00OQ0[OCO0C00OQ0],O0CCC00OQ0);
Inc(OCO0C00OQ0,O0CCC00OQ0);
end;
if OOCCC00OQ0-OCO0C00OQ0>0 then
Move(OOQ0C00OQ0[0],OQQ0C00OQ0[OCO0C00OQ0],OOCCC00OQ0-OCO0C00OQ0);
OOC0C00OQ0:=nil;
OQC0C00OQ0:=nil;
OCC0C00OQ0:=nil;
O0C0C00OQ0:=TBigInteger.Create(OQQ0C00OQ0);
try
OOC0C00OQ0:=O0C0C00OQ0.Add(1);
OO0OC00OQ0:=0;
while OO0OC00OQ0<OQO0C00OQ0 do begin
FreeAndNil(OQC0C00OQ0);
OQC0C00OQ0:=TBigInteger.Create(OO00C00OQ0,OO0OC00OQ0,OOCCC00OQ0);
FreeAndNil(OCC0C00OQ0);
OCC0C00OQ0:=OQC0C00OQ0.Add(OOC0C00OQ0);
OCQ0C00OQ0:=OCC0C00OQ0.GetBytes;
if Length(OCQ0C00OQ0)>OOCCC00OQ0 then
Move(OCQ0C00OQ0[1],OO00C00OQ0[OO0OC00OQ0],OOCCC00OQ0)
else
if Length(OCQ0C00OQ0)<OOCCC00OQ0 then begin
FillChar(OO00C00OQ0[OO0OC00OQ0],OOCCC00OQ0-Length(OCQ0C00OQ0),0);
Move(OCQ0C00OQ0[0],OO00C00OQ0[OO0OC00OQ0+OOCCC00OQ0-Length(OCQ0C00OQ0)],Length(OCQ0C00OQ0));
end
else
Move(OCQ0C00OQ0[0],OO00C00OQ0[OO0OC00OQ0],OOCCC00OQ0);
Inc(OO0OC00OQ0,OOCCC00OQ0);
end;
finally
O0C0C00OQ0.Free;
OOC0C00OQ0.Free;
OQC0C00OQ0.Free;
OCC0C00OQ0.Free;
end;
end;
finally
OCQCC00OQ0.Free;
end;
if Length(O000C00OQ0)>0 then
FillChar(O000C00OQ0[0],Length(O000C00OQ0),0);
if Length(OO00C00OQ0)>0 then
FillChar(OO00C00OQ0[0],Length(OO00C00OQ0),0);
end;
class function OOQ0000OQ0.OQQ0000OQ0(const OCQ0000OQ0,O0C0000OQ0:TBytes):TBytes;
const
OOC0000OQ0:array[0..31]of byte=
($63,$79,$78,$4F,$6D,$6F,$72,$68,$63,$69,$74,$61,$77,$6F,$6C,$42,
$68,$73,$69,$66,$74,$61,$77,$53,$61,$6E,$79,$44,$65,$74,$69,$6D);
var
OQC0000OQ0:TCipher_BlowfishLE;
OCC0000OQ0:integer;
begin
OQC0000OQ0:=TCipher_BlowfishLE.Create;
try
OQC0000OQ0.Mode:=cmECB;
OQC0000OQ0.InitEx(OCQ0000OQ0,O0C0000OQ0);
for OCC0000OQ0:=1 to 64 do begin
OQC0000OQ0.InitEx(O0C0000OQ0);
OQC0000OQ0.InitEx(OCQ0000OQ0);
end;
SetLength(Result,Length(OOC0000OQ0));
Move(OOC0000OQ0[0],Result[0],Length(OOC0000OQ0));
for OCC0000OQ0:=1 to 64 do
OQC0000OQ0.EncodeBuffer(@Result[0],0,Length(Result),@Result[0],0);
finally
OQC0000OQ0.Free;
end;
end;
class function OOQ0000OQ0.O00O000OQ0(const OO0O000OQ0,OQ0O000OQ0:TBytes;OC0O000OQ0:integer):TBytes;
var
O0OO000OQ0:THashAlgorithm;
OOOO000OQ0:array[0..3]of byte;
begin
O0OO000OQ0:=THash_SHA2_512.Create;
try
if OC0O000OQ0<>0 then begin
O0OO000OQ0.TransformBlock(OQ0O000OQ0,0,Length(OQ0O000OQ0));
PutIntBE(OC0O000OQ0,TValueArr(@OOOO000OQ0),0);
O0OO000OQ0.TransformFinalBlock(TValueArr(@OOOO000OQ0),4);
end
else
O0OO000OQ0.ComputeHash(OQ0O000OQ0);
Result:=OQQ0000OQ0(OO0O000OQ0,O0OO000OQ0.Hash);
finally
O0OO000OQ0.Free;
end;
end;
class function OOQ0000OQ0.OQ0OC00OQ0(const OC0OC00OQ0:string;const O0OOC00OQ0:TBytes;OOOOC00OQ0:integer;OQOOC00OQ0:integer):TBytes;
var
OCOOC00OQ0:THashAlgorithm;
O0QOC00OQ0,OOQOC00OQ0,OQQOC00OQ0,OCQOC00OQ0:TBytes;
O0COC00OQ0,OOCOC00OQ0,OQCOC00OQ0:integer;
OCCOC00OQ0,O00QQ00OQ0:integer;
begin
OCOOC00OQ0:=THash_SHA2_512.Create;
try
OCOOC00OQ0.ComputeHash(Encoding.UTF8.GetBytes(OC0OC00OQ0));
SetLength(O0QOC00OQ0,Length(OCOOC00OQ0.Hash));
Buffer.BlockCopy(OCOOC00OQ0.Hash,0,O0QOC00OQ0,0,Length(OCOOC00OQ0.Hash));
finally
OCOOC00OQ0.Free;
end;
SetLength(Result,OQOOC00OQ0);
SetLength(OCQOC00OQ0,0);
SetLength(OQQOC00OQ0,0);
SetLength(OOQOC00OQ0,32);
O0COC00OQ0:=(OQOOC00OQ0+31)div 32;
for OOCOC00OQ0:=0 to O0COC00OQ0-1 do begin
FillChar(OOQOC00OQ0[0],32,0);
OQQOC00OQ0:=O0OOC00OQ0;
for OQCOC00OQ0:=0 to OOOOC00OQ0-1 do begin
if OQCOC00OQ0=0 then
OCQOC00OQ0:=O00O000OQ0(O0QOC00OQ0,OQQOC00OQ0,OOCOC00OQ0+1)
else
OCQOC00OQ0:=O00O000OQ0(O0QOC00OQ0,OQQOC00OQ0,0);
OQQOC00OQ0:=OCQOC00OQ0;
for OCCOC00OQ0:=0 to 31 do
OOQOC00OQ0[OCCOC00OQ0]:=OOQOC00OQ0[OCCOC00OQ0]xor OCQOC00OQ0[OCCOC00OQ0];
end;
OCCOC00OQ0:=OOCOC00OQ0;
O00QQ00OQ0:=0;
while OCCOC00OQ0<OQOOC00OQ0 do begin
Result[OCCOC00OQ0]:=OOQOC00OQ0[O00QQ00OQ0];
Inc(OCCOC00OQ0,O0COC00OQ0);
Inc(O00QQ00OQ0);
end
end;
FillChar(O0QOC00OQ0[0],Length(O0QOC00OQ0),0);
end;
class function OOQ0000OQ0.OO0QQ00OQ0(const OQ0QQ00OQ0:string;
const OC0QQ00OQ0:TBytes;const O0OQQ00OQ0:string):TSymmetricAlgorithm;
var
OOOQQ00OQ0:OQCOCQOOQ0;
OQOQQ00OQ0,OCOQQ00OQ0,O0QQQ00OQ0:TBytes;
OOQQQ00OQ0:integer;
OQQQQ00OQ0:OO00COQOQ0;
OCQQQ00OQ0:OCQ0COQOQ0;
O0CQQ00OQ0:TCipherMode;
OOCQQ00OQ0,OQCQQ00OQ0:integer;
OCCQQ00OQ0:OOOOCOQOQ0;
begin
Result:=nil;
SetLength(OQOQQ00OQ0,0);
SetLength(OCOQQ00OQ0,0);
SetLength(O0QQQ00OQ0,0);
OOOQQ00OQ0:=OQCOCQOOQ0.Create;
try
if OQ0QQ00OQ0=OID_PBES2 then begin
if not OOOQQ00OQ0.OCQCOOOOQ0(OQOOCCOOQ0,OC0QQ00OQ0)then
Exit;
OCOQQ00OQ0:=OOOQQ00OQ0['Salt'].OQOQ0QOOQ0;
OOQQQ00OQ0:=OOOQQ00OQ0['Iters'].OQQQ0QOOQ0;
O0QQQ00OQ0:=OOOQQ00OQ0['IV'].OQOQ0QOOQ0;
OQQQQ00OQ0:=OOCQCC0OQ0.OCOCQC0OQ0(OOOQQ00OQ0['Algo'].O0CQ0QOOQ0);
OQOQQ00OQ0:=OQOQC00OQ0(O0OQQ00OQ0,OCOQQ00OQ0,OOCQCC0OQ0.OCCCCC0OQ0(OQQQQ00OQ0),OOQQQ00OQ0);
Result:=OOCQCC0OQ0.OQCQCC0OQ0(OQQQQ00OQ0,OQOQQ00OQ0,O0QQQ00OQ0);
end
else
if Copy(OQ0QQ00OQ0,1,Length(OID_PKCS5))=OID_PKCS5 then begin
if OQ0QQ00OQ0=OID_pbeWithMD2AndDES_CBC then begin
OCCQQ00OQ0:=O0OOCOQOQ0;
OCQQQ00OQ0:=OQ00COQOQ0;
end
else
if OQ0QQ00OQ0=OID_pbeWithMD2AndRC2_CBC then begin
OCCQQ00OQ0:=O0OOCOQOQ0;
OCQQQ00OQ0:=OOQ0COQOQ0;
end
else
if OQ0QQ00OQ0=OID_pbeWithMD5AndDES_CBC then begin
OCCQQ00OQ0:=OQ0OCOQOQ0;
OCQQQ00OQ0:=OQ00COQOQ0;
end
else
if OQ0QQ00OQ0=OID_pbeWithMD5AndRC2_CBC then begin
OCCQQ00OQ0:=OQ0OCOQOQ0;
OCQQQ00OQ0:=OOQ0COQOQ0;
end
else
if OQ0QQ00OQ0=OID_pbeWithSHA1AndDES_CBC then begin
OCCQQ00OQ0:=OOC0COQOQ0;
OCQQQ00OQ0:=OQ00COQOQ0;
end
else
if OQ0QQ00OQ0=OID_pbeWithSHA1AndRC2_CBC then begin
OCCQQ00OQ0:=OOC0COQOQ0;
OCQQQ00OQ0:=OOQ0COQOQ0;
end
else
Exit;
if not OOOQQ00OQ0.OCQCOOOOQ0(OCOOCCOOQ0,OC0QQ00OQ0)then
Exit;
OCOQQ00OQ0:=OOOQQ00OQ0['Salt'].OQOQ0QOOQ0;
OOQQQ00OQ0:=OOOQQ00OQ0['Iters'].OQQQ0QOOQ0;
OQOQQ00OQ0:=OQOO000OQ0(OCCQQ00OQ0,O0OQQ00OQ0,OCOQQ00OQ0,16,OOQQQ00OQ0,False);
SetLength(O0QQQ00OQ0,8);
Move(OQOQQ00OQ0[8],O0QQQ00OQ0[0],8);
SetLength(OQOQQ00OQ0,8);
Result:=OOCQCC0OQ0.OQ0CCC0OQ0(OCQQQ00OQ0,cmCBC);
Result.Key:=OQOQQ00OQ0;
Result.IV:=O0QQQ00OQ0;
end
else
if Copy(OQ0QQ00OQ0,1,Length(OID_PKCS12_PBE_IDS))=OID_PKCS12_PBE_IDS then begin
if OQ0QQ00OQ0=OID_pbeWithSHAAnd128BitRC4 then begin
OCQQQ00OQ0:=OQQ0COQOQ0;
O0CQQ00OQ0:=cmECB;
OOCQQ00OQ0:=16;
OQCQQ00OQ0:=0;
end
else
if OQ0QQ00OQ0=OID_pbeWithSHAAnd40BitRC4 then begin
OCQQQ00OQ0:=OQQ0COQOQ0;
O0CQQ00OQ0:=cmECB;
OOCQQ00OQ0:=5;
OQCQQ00OQ0:=0;
end
else
if OQ0QQ00OQ0=OID_pbeWithSHAAnd3KeyTripleDES_CBC then begin
OCQQQ00OQ0:=OC00COQOQ0;
O0CQQ00OQ0:=cmCBC;
OOCQQ00OQ0:=24;
OQCQQ00OQ0:=8;
end
else
if OQ0QQ00OQ0=OID_pbeWithSHAAnd2KeyTripleDES_CBC then begin
OCQQQ00OQ0:=OC00COQOQ0;
O0CQQ00OQ0:=cmCBC;
OOCQQ00OQ0:=16;
OQCQQ00OQ0:=8;
end
else
if OQ0QQ00OQ0=OID_pbeWithSHAAnd128BitRC2_CBC then begin
OCQQQ00OQ0:=OOQ0COQOQ0;
O0CQQ00OQ0:=cmCBC;
OOCQQ00OQ0:=16;
OQCQQ00OQ0:=8;
end
else
if OQ0QQ00OQ0=OID_pbeWithSHAAnd40BitRC2_CBC then begin
OCQQQ00OQ0:=OOQ0COQOQ0;
O0CQQ00OQ0:=cmCBC;
OOCQQ00OQ0:=5;
OQCQQ00OQ0:=8;
end
else
Exit;
if not OOOQQ00OQ0.OCQCOOOOQ0(O0QOCCOOQ0,OC0QQ00OQ0)then
Exit;
OCOQQ00OQ0:=OOOQQ00OQ0['Salt'].OQOQ0QOOQ0;
OOQQQ00OQ0:=OOOQQ00OQ0['Iters'].OQQQ0QOOQ0;
OQOQQ00OQ0:=O0OCC00OQ0(OOC0COQOQ0,O0OQQ00OQ0,OCOQQ00OQ0,O0OC0O0OQ0,OOQQQ00OQ0,OOCQQ00OQ0);
if OQCQQ00OQ0>0 then
O0QQQ00OQ0:=O0OCC00OQ0(OOC0COQOQ0,O0OQQ00OQ0,OCOQQ00OQ0,OOOC0O0OQ0,OOQQQ00OQ0,OQCQQ00OQ0);
Result:=OOCQCC0OQ0.OQ0CCC0OQ0(OCQQQ00OQ0,O0CQQ00OQ0);
Result.Key:=OQOQQ00OQ0;
Result.IV:=O0QQQ00OQ0;
end;
if Length(OQOQQ00OQ0)>0 then
FillChar(OQOQQ00OQ0[0],Length(OQOQQ00OQ0),0);
finally
OOOQQ00OQ0.Free;
end;
end;
procedure OCOO0QQ0Q0(O0QO0QQ0Q0:TStream;const OOQO0QQ0Q0:TBytes;OQQO0QQ0Q0:integer;OCQO0QQ0Q0:Boolean);
procedure O0CO0QQ0Q0(const OOCO0QQ0Q0:TBytes;OQCO0QQ0Q0,OCCO0QQ0Q0:integer);
begin
if(OCCO0QQ0Q0<>0)and(O0QO0QQ0Q0.Write(OOCO0QQ0Q0[OQCO0QQ0Q0],OCCO0QQ0Q0)<>OCCO0QQ0Q0)then
raise EWriteError.Create('Stream write error');
end;
const
O00QCQQ0Q0:byte=10;
OO0QCQQ0Q0:byte=92;
var
OQ0QCQQ0Q0:integer;
begin
OQ0QCQQ0Q0:=0;
while OQ0QCQQ0Q0<Length(OOQO0QQ0Q0)do begin
if OQQO0QQ0Q0>=(Length(OOQO0QQ0Q0)-OQ0QCQQ0Q0)then begin
O0CO0QQ0Q0(OOQO0QQ0Q0,OQ0QCQQ0Q0,Length(OOQO0QQ0Q0)-OQ0QCQQ0Q0);
end
else
if OCQO0QQ0Q0 then begin
O0CO0QQ0Q0(OOQO0QQ0Q0,OQ0QCQQ0Q0,OQQO0QQ0Q0-1);
O0QO0QQ0Q0.WriteBuffer(OO0QCQQ0Q0,1);
Dec(OQ0QCQQ0Q0);
end
else begin
O0CO0QQ0Q0(OOQO0QQ0Q0,OQ0QCQQ0Q0,OQQO0QQ0Q0);
end;
O0QO0QQ0Q0.WriteBuffer(O00QCQQ0Q0,1);
OQ0QCQQ0Q0:=OQ0QCQQ0Q0+OQQO0QQ0Q0;
end;
end;
constructor O0CQOOC0Q0.Create(OOQC0CC0Q0:O0000CC0Q0=nil);
begin
inherited Create(OOQC0CC0Q0);
OCCQOOC0Q0:=OC0Q0OOOQ0.Create;
O00COOC0Q0:=OCOC0OOOQ0.Create;
OOCQOOC0Q0:=TList.Create;
OQCQOOC0Q0:=OCOQQOQOQ0;
end;
destructor O0CQOOC0Q0.Destroy;
begin
OCCQOOC0Q0.Free;
O00COOC0Q0.Free;
inherited;
OOCQOOC0Q0.Free;
end;
procedure O0CQOOC0Q0.O0OCOOC0Q0;
begin
raise EScError.Create(seBrokenKey);
end;
procedure O0CQOOC0Q0.OOOCOOC0Q0;
begin
if not O0QQ0CC0Q0 then
O0OCOOC0Q0;
end;
procedure O0CQOOC0Q0.O0OOQ0C0Q0(OOOOQ0C0Q0:TObject);
begin
if IsClass(OOOOQ0C0Q0,OCQQQCQ0Q0)or IsClass(OOOOQ0C0Q0,TScCertificate)then
OOCQOOC0Q0.Add(OOOOQ0C0Q0);
end;
procedure O0CQOOC0Q0.OQOOQ0C0Q0(OCOOQ0C0Q0:TObject);
begin
OOCQOOC0Q0.Remove(OCOOQ0C0Q0);
end;
procedure O0CQOOC0Q0.OO0C0CC0Q0;
var
OCOQOCQ0Q0:TObject;
O0QQOCQ0Q0:integer;
begin
inherited;
for O0QQOCQ0Q0:=0 to OOCQOOC0Q0.Count-1 do begin
OCOQOCQ0Q0:=OOCQOOC0Q0[O0QQOCQ0Q0];
if IsClass(OCOQOCQ0Q0,OCQQQCQ0Q0)then
OCQQQCQ0Q0(OCOQOCQ0Q0).OO0C0CC0Q0
else
if IsClass(OCOQOCQ0Q0,TScCertificate)then
TScCertificate(OCOQOCQ0Q0).OCQ00QC0Q0;
end;
end;
procedure O0CQOOC0Q0.O0OC0CC0Q0;
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
FreeAndNil(OO0OQ0C0Q0.OQCC0O0OQ0);
FreeAndNil(OO0OQ0C0Q0.OCCC0O0OQ0);
FreeAndNil(OO0OQ0C0Q0.O0000O0OQ0);
FreeAndNil(OO0OQ0C0Q0.OO000O0OQ0);
FreeAndNil(OO0OQ0C0Q0.OQ000O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.O0O00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OOO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OQO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OCO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.O0Q00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OOQ00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OQQ00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OCQ00O0OQ0);
FreeAndNil(OC0OQ0C0Q0.OOQQQO0OQ0);
FreeAndNil(OC0OQ0C0Q0.OQQQQO0OQ0);
FreeAndNil(OC0OQ0C0Q0.OCQQQO0OQ0);
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
procedure O0CQOOC0Q0.OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);
procedure OOQOQ0C0Q0(const OQQOQ0C0Q0:OOCC0O0OQ0);
begin
if OQQOQ0C0Q0.OCCC0O0OQ0<>nil then
OO0OQ0C0Q0.OCCC0O0OQ0:=TBigInteger.Create(OQQOQ0C0Q0.OCCC0O0OQ0);
if OQQOQ0C0Q0.O0000O0OQ0<>nil then
OO0OQ0C0Q0.O0000O0OQ0:=TBigInteger.Create(OQQOQ0C0Q0.O0000O0OQ0);
if OQQOQ0C0Q0.OO000O0OQ0<>nil then
OO0OQ0C0Q0.OO000O0OQ0:=TBigInteger.Create(OQQOQ0C0Q0.OO000O0OQ0);
if OQQOQ0C0Q0.OQ000O0OQ0<>nil then
OO0OQ0C0Q0.OQ000O0OQ0:=TBigInteger.Create(OQQOQ0C0Q0.OQ000O0OQ0);
if OQQOQ0C0Q0.OQCC0O0OQ0<>nil then
OO0OQ0C0Q0.OQCC0O0OQ0:=TBigInteger.Create(OQQOQ0C0Q0.OQCC0O0OQ0);
end;
procedure OCQOQ0C0Q0(const O0COQ0C0Q0:OC000O0OQ0);
begin
if O0COQ0C0Q0.OQQ00O0OQ0<>nil then
OQ0OQ0C0Q0.OQQ00O0OQ0:=TBigInteger.Create(O0COQ0C0Q0.OQQ00O0OQ0);
if O0COQ0C0Q0.OCQ00O0OQ0<>nil then
OQ0OQ0C0Q0.OCQ00O0OQ0:=TBigInteger.Create(O0COQ0C0Q0.OCQ00O0OQ0);
if O0COQ0C0Q0.O0O00O0OQ0<>nil then
OQ0OQ0C0Q0.O0O00O0OQ0:=TBigInteger.Create(O0COQ0C0Q0.O0O00O0OQ0);
if O0COQ0C0Q0.OOO00O0OQ0<>nil then
OQ0OQ0C0Q0.OOO00O0OQ0:=TBigInteger.Create(O0COQ0C0Q0.OOO00O0OQ0);
if O0COQ0C0Q0.OQO00O0OQ0<>nil then
OQ0OQ0C0Q0.OQO00O0OQ0:=TBigInteger.Create(O0COQ0C0Q0.OQO00O0OQ0);
if O0COQ0C0Q0.OCO00O0OQ0<>nil then
OQ0OQ0C0Q0.OCO00O0OQ0:=TBigInteger.Create(O0COQ0C0Q0.OCO00O0OQ0);
if O0COQ0C0Q0.O0Q00O0OQ0<>nil then
OQ0OQ0C0Q0.O0Q00O0OQ0:=TBigInteger.Create(O0COQ0C0Q0.O0Q00O0OQ0);
if O0COQ0C0Q0.OOQ00O0OQ0<>nil then
OQ0OQ0C0Q0.OOQ00O0OQ0:=TBigInteger.Create(O0COQ0C0Q0.OOQ00O0OQ0);
end;
procedure OOCOQ0C0Q0(const OQCOQ0C0Q0:OCOQQO0OQ0);
begin
OC0OQ0C0Q0.O0QQQO0OQ0:=OQCOQ0C0Q0.O0QQQO0OQ0;
if OQCOQ0C0Q0.OOQQQO0OQ0<>nil then begin
OC0OQ0C0Q0.OOQQQO0OQ0:=O0QOCO0OQ0(OQCOQ0C0Q0.OOQQQO0OQ0.ClassType).Create;
OC0OQ0C0Q0.OOQQQO0OQ0.OCCCCO0OQ0(OQCOQ0C0Q0.OOQQQO0OQ0);
if OQCOQ0C0Q0.OQQQQO0OQ0<>nil then begin
OC0OQ0C0Q0.OQQQQO0OQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OQ00CO0OQ0.Create;
OC0OQ0C0Q0.OQQQQO0OQ0.OQCOCO0OQ0(OQCOQ0C0Q0.OQQQQO0OQ0);
end;
if OQCOQ0C0Q0.OCQQQO0OQ0<>nil then begin
OC0OQ0C0Q0.OCQQQO0OQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OC00CO0OQ0.Create;
OC0OQ0C0Q0.OCQQQO0OQ0.Assign(OQCOQ0C0Q0.OCQQQO0OQ0);
end;
end;
end;
begin
if not IsClass(OC0C0CC0Q0,O0CQOOC0Q0)then
raise EScError.CreateFmt(SInvalidObjectClass,[O0CQOOC0Q0.ClassName,OC0C0CC0Q0.ClassName],seInvalidObjectClass);
O0OC0CC0Q0;
OQCQOOC0Q0:=O0CQOOC0Q0(OC0C0CC0Q0).OQCQOOC0Q0;
case OQCQOOC0Q0 of
OCOQQOQOQ0:
OCQOQ0C0Q0(O0CQOOC0Q0(OC0C0CC0Q0).OQ0OQ0C0Q0);
OQOQQOQOQ0:
OOQOQ0C0Q0(O0CQOOC0Q0(OC0C0CC0Q0).OO0OQ0C0Q0);
O0QQQOQOQ0:
OOCOQ0C0Q0(O0CQOOC0Q0(OC0C0CC0Q0).OC0OQ0C0Q0);
else
Assert(False);
end;
O0QQ0CC0Q0:=OC0C0CC0Q0.O0QQ0CC0Q0;
end;
procedure O0CQOOC0Q0.OQCOOOC0Q0(const OCCOOOC0Q0:integer;O00Q0OC0Q0:IScRandom);
var
OO0Q0OC0Q0,OQ0Q0OC0Q0:TBigInteger;
OC0Q0OC0Q0,O0OQ0OC0Q0,OOOQ0OC0Q0,OQOQ0OC0Q0,OCOQ0OC0Q0,O0QQ0OC0Q0:TBigInteger;
begin
if O00Q0OC0Q0=nil then
O00Q0OC0Q0:=OCCQ0QQ0Q0;
if O00Q0OC0Q0=nil then
raise Exception.Create(SInternalError);
O0OQ0OC0Q0:=nil;
OOOQ0OC0Q0:=nil;
OQ0Q0OC0Q0:=nil;
OQOQ0OC0Q0:=nil;
OCOQ0OC0Q0:=nil;
O0QQ0OC0Q0:=nil;
OO0Q0OC0Q0:=TBigInteger.Create(1);
OC0Q0OC0Q0:=TBigInteger.Create(65537);
try
while True do begin
while True do begin
FreeAndNil(O0OQ0OC0Q0);
FreeAndNil(OOOQ0OC0Q0);
O0OQ0OC0Q0:=TBigInteger.GenPseudoPrime(OCCOOOC0Q0 shr 1,64,O00Q0OC0Q0);
OOOQ0OC0Q0:=TBigInteger.GenPseudoPrime(OCCOOOC0Q0 shr 1,64,O00Q0OC0Q0);
FreeAndNil(OQ0Q0OC0Q0);
OQ0Q0OC0Q0:=O0OQ0OC0Q0.gcd(OOOQ0OC0Q0);
if OQ0Q0OC0Q0.Equal(OO0Q0OC0Q0)then
Break;
end;
FreeAndNil(OQOQ0OC0Q0);
FreeAndNil(OCOQ0OC0Q0);
FreeAndNil(O0QQ0OC0Q0);
OQOQ0OC0Q0:=O0OQ0OC0Q0.Minus(OO0Q0OC0Q0);
OCOQ0OC0Q0:=OOOQ0OC0Q0.Minus(OO0Q0OC0Q0);
O0QQ0OC0Q0:=OQOQ0OC0Q0.Mul(OCOQ0OC0Q0);
FreeAndNil(OQ0Q0OC0Q0);
OQ0Q0OC0Q0:=OC0Q0OC0Q0.gcd(O0QQ0OC0Q0);
if OQ0Q0OC0Q0.Equal(OO0Q0OC0Q0)then
break;
end;
OQ0OQ0C0Q0.OQQ00O0OQ0:=TBigInteger.Create(OC0Q0OC0Q0);
OQ0OQ0C0Q0.OCQ00O0OQ0:=O0OQ0OC0Q0.Mul(OOOQ0OC0Q0);
OQ0OQ0C0Q0.O0O00O0OQ0:=OC0Q0OC0Q0.ModInverse(O0QQ0OC0Q0);
OQ0OQ0C0Q0.OCO00O0OQ0:=OOOQ0OC0Q0.ModInverse(O0OQ0OC0Q0);
OQ0OQ0C0Q0.O0Q00O0OQ0:=OQ0OQ0C0Q0.O0O00O0OQ0.Mod_(OQOQ0OC0Q0);
OQ0OQ0C0Q0.OOQ00O0OQ0:=OQ0OQ0C0Q0.O0O00O0OQ0.Mod_(OCOQ0OC0Q0);
OQ0OQ0C0Q0.OOO00O0OQ0:=O0OQ0OC0Q0;
O0OQ0OC0Q0:=nil;
OQ0OQ0C0Q0.OQO00O0OQ0:=OOOQ0OC0Q0;
OOOQ0OC0Q0:=nil;
OQCQOOC0Q0:=OCOQQOQOQ0;
O0QQ0CC0Q0:=True;
finally
OO0Q0OC0Q0.Free;
O0OQ0OC0Q0.Free;
OOOQ0OC0Q0.Free;
OQ0Q0OC0Q0.Free;
OQOQ0OC0Q0.Free;
OCOQ0OC0Q0.Free;
O0QQ0OC0Q0.Free;
OC0Q0OC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OQQQ0OC0Q0;
var
OCQQ0OC0Q0:TBigInteger;
O0CQ0OC0Q0,OOCQ0OC0Q0:TBigInteger;
OQCQ0OC0Q0,OCCQ0OC0Q0:TBigInteger;
begin
OQCQ0OC0Q0:=OQ0OQ0C0Q0.OQO00O0OQ0.ModInverse(OQ0OQ0C0Q0.OOO00O0OQ0);
try
if not OQCQ0OC0Q0.Equal(OQ0OQ0C0Q0.OCO00O0OQ0)then begin
OQCQ0OC0Q0.Free;
OQCQ0OC0Q0:=OQ0OQ0C0Q0.OOO00O0OQ0.ModInverse(OQ0OQ0C0Q0.OQO00O0OQ0);
if not OQCQ0OC0Q0.Equal(OQ0OQ0C0Q0.OCO00O0OQ0)then
O0OCOOC0Q0;
OCCQ0OC0Q0:=OQ0OQ0C0Q0.OQO00O0OQ0;
OQ0OQ0C0Q0.OQO00O0OQ0:=OQ0OQ0C0Q0.OOO00O0OQ0;
OQ0OQ0C0Q0.OOO00O0OQ0:=OCCQ0OC0Q0;
end;
finally
OQCQ0OC0Q0.Free;
end;
O0CQ0OC0Q0:=nil;
OOCQ0OC0Q0:=nil;
OCQQ0OC0Q0:=TBigInteger.Create(1);
try
O0CQ0OC0Q0:=OQ0OQ0C0Q0.OOO00O0OQ0.Minus(OCQQ0OC0Q0);
OOCQ0OC0Q0:=OQ0OQ0C0Q0.OQO00O0OQ0.Minus(OCQQ0OC0Q0);
OQ0OQ0C0Q0.O0Q00O0OQ0:=OQ0OQ0C0Q0.O0O00O0OQ0.Mod_(O0CQ0OC0Q0);
OQ0OQ0C0Q0.OOQ00O0OQ0:=OQ0OQ0C0Q0.O0O00O0OQ0.Mod_(OOCQ0OC0Q0);
finally
OCQQ0OC0Q0.Free;
O0CQ0OC0Q0.Free;
OOCQ0OC0Q0.Free;
end;
end;
class procedure O0CQOOC0Q0.OQOCOOC0Q0(OCOCOOC0Q0,O0QCOOC0Q0:integer;
OOQCOOC0Q0:IScRandom;var OQQCOOC0Q0,OCQCOOC0Q0:TBigInteger);
var
O0CCOOC0Q0,OOCCOOC0Q0,OQCCOOC0Q0,OCCCOOC0Q0,O000OOC0Q0,OO00OOC0Q0:TBigInteger;
OQ00OOC0Q0:O0O00QQ0Q0;
OC00OOC0Q0,O0O0OOC0Q0,OOO0OOC0Q0:array of Int64;
OQO0OOC0Q0,OCO0OOC0Q0:Int64;
O0Q0OOC0Q0:cardinal;
OOQ0OOC0Q0:boolean;
OQQ0OOC0Q0,OCQ0OOC0Q0,O0C0OOC0Q0:integer;
begin
OQ00OOC0Q0:=O0O00QQ0Q0.Create(16000);
try
O0Q0OOC0Q0:=OQ00OOC0Q0.OQC00QQ0Q0-1;
SetLength(OOO0OOC0Q0,O0Q0OOC0Q0);
SetLength(OC00OOC0Q0,O0Q0OOC0Q0);
SetLength(O0O0OOC0Q0,O0Q0OOC0Q0);
OQQ0OOC0Q0:=0;
O0C0OOC0Q0:=2;
while O0C0OOC0Q0<>0 do begin
OOO0OOC0Q0[OQQ0OOC0Q0]:=O0C0OOC0Q0;
O0C0OOC0Q0:=OQ00OOC0Q0.OC0O0QQ0Q0(O0C0OOC0Q0);
Inc(OQQ0OOC0Q0);
end;
finally
OQ00OOC0Q0.Free;
end;
OOCCOOC0Q0:=nil;
OQCCOOC0Q0:=nil;
OCCCOOC0Q0:=nil;
O0CCOOC0Q0:=TBigInteger.Create(1);
try
OCQCOOC0Q0:=TBigInteger.GenPseudoPrime(O0QCOOC0Q0,50,OOQCOOC0Q0);
for OQQ0OOC0Q0:=0 to O0Q0OOC0Q0-1 do begin
O000OOC0Q0:=OCQCOOC0Q0.Mod_(OOO0OOC0Q0[OQQ0OOC0Q0]);
try
OC00OOC0Q0[OQQ0OOC0Q0]:=({$IFDEF VER25P}Int64{$ENDIF}(O000OOC0Q0.LongValue)*2)mod OOO0OOC0Q0[OQQ0OOC0Q0];
finally
O000OOC0Q0.Free;
end;
end;
while True do begin
OOCCOOC0Q0:=TBigInteger.Create;
OOCCOOC0Q0.GenRandomBits(OCOCOOC0Q0,OOQCOOC0Q0);
OOCCOOC0Q0.SetBit(OCOCOOC0Q0-1);
OQCCOOC0Q0:=OCQCOOC0Q0.Shl_(1);
OCCCOOC0Q0:=OOCCOOC0Q0.Mod_(OQCCOOC0Q0);
if not OOCCOOC0Q0.Equal(OCCCOOC0Q0)then begin
OO00OOC0Q0:=OOCCOOC0Q0;
OOCCOOC0Q0:=OOCCOOC0Q0.Minus(OCCCOOC0Q0);
OO00OOC0Q0.Free;
end;
OO00OOC0Q0:=OOCCOOC0Q0;
OOCCOOC0Q0:=OOCCOOC0Q0.Add(O0CCOOC0Q0);
OO00OOC0Q0.Free;
FreeAndNil(OQCCOOC0Q0);
FreeAndNil(OCCCOOC0Q0);
if OOCCOOC0Q0.BitCount<=(OCOCOOC0Q0-1)then
Continue;
for OCQ0OOC0Q0:=0 to O0Q0OOC0Q0-1 do begin
O000OOC0Q0:=OOCCOOC0Q0.Mod_(OOO0OOC0Q0[OCQ0OOC0Q0]);
try
O0O0OOC0Q0[OCQ0OOC0Q0]:=O000OOC0Q0.LongValue;
finally
O000OOC0Q0.Free;
end;
end;
OCCCOOC0Q0:=OCQCOOC0Q0.Shl_(1);
OQQ0OOC0Q0:=0;
while OQQ0OOC0Q0<(1 shl 24)do begin
OOQ0OOC0Q0:=True;
for OCQ0OOC0Q0:=1 to O0Q0OOC0Q0-1 do begin
OQO0OOC0Q0:=OOO0OOC0Q0[OCQ0OOC0Q0];
OCO0OOC0Q0:=O0O0OOC0Q0[OCQ0OOC0Q0];
if OCO0OOC0Q0>=OQO0OOC0Q0 then
OCO0OOC0Q0:=OCO0OOC0Q0-OQO0OOC0Q0;
if OCO0OOC0Q0=0 then
OOQ0OOC0Q0:=False;
O0O0OOC0Q0[OCQ0OOC0Q0]:=OCO0OOC0Q0+OC00OOC0Q0[OCQ0OOC0Q0];
end;
if not OOQ0OOC0Q0 then begin
Inc(OQQ0OOC0Q0);
Continue;
end;
OQCCOOC0Q0:=OCCCOOC0Q0.Mul(OQQ0OOC0Q0);
FreeAndNil(OQQCOOC0Q0);
OQQCOOC0Q0:=OOCCOOC0Q0.Add(OQCCOOC0Q0);
FreeAndNil(OQCCOOC0Q0);
if OQQCOOC0Q0.BitCount>OCOCOOC0Q0 then begin
Inc(OQQ0OOC0Q0);
Continue;
end;
if OQQCOOC0Q0.IsProbablePrime(50)then
break;
Inc(OQQ0OOC0Q0);
end;
FreeAndNil(OCCCOOC0Q0);
FreeAndNil(OOCCOOC0Q0);
if OQQ0OOC0Q0<(1 shl 24)then
break;
end;
finally
O0CCOOC0Q0.Free;
OOCCOOC0Q0.Free;
OQCCOOC0Q0.Free;
OCCCOOC0Q0.Free;
end;
end;
class function O0CQOOC0Q0.OOC0OOC0Q0(OQC0OOC0Q0,OCC0OOC0Q0:TBigInteger;
O00OOOC0Q0:IScRandom):TBigInteger;
var
OO0OOOC0Q0,OQ0OOOC0Q0,OC0OOOC0Q0,O0OOOOC0Q0,OOOOOOC0Q0:TBigInteger;
OQOOOOC0Q0:TBigInteger;
begin
OQ0OOOC0Q0:=nil;
OC0OOOC0Q0:=nil;
OQOOOOC0Q0:=nil;
OO0OOOC0Q0:=TBigInteger.Create(1);
try
OQ0OOOC0Q0:=OCC0OOC0Q0.Minus(OO0OOOC0Q0);
OC0OOOC0Q0:=OQ0OOOC0Q0.Mod_(OQC0OOC0Q0);
if OC0OOOC0Q0.LongValue<>0 then begin
Result:=TBigInteger.Create(0);
Exit;
end;
FreeAndNil(OC0OOOC0Q0);
OC0OOOC0Q0:=OQ0OOOC0Q0.Div_(OQC0OOC0Q0);
while True do begin
OOOOOOC0Q0:=nil;
O0OOOOC0Q0:=TBigInteger.Create;
try
O0OOOOC0Q0.GenRandomBits(OCC0OOC0Q0.BitCount,O00OOOC0Q0);
OOOOOOC0Q0:=O0OOOOC0Q0.Mod_(OCC0OOC0Q0);
FreeAndNil(OQOOOOC0Q0);
OQOOOOC0Q0:=OOOOOOC0Q0.ModPow(OC0OOOC0Q0,OCC0OOC0Q0);
finally
O0OOOOC0Q0.Free;
OOOOOOC0Q0.Free;
end;
if OQOOOOC0Q0.Greater(OO0OOOC0Q0)then
break;
end;
FreeAndNil(OQ0OOOC0Q0);
OQ0OOOC0Q0:=OQOOOOC0Q0.ModPow(OQC0OOC0Q0,OCC0OOC0Q0);
if OQ0OOOC0Q0.NotEqual(OO0OOOC0Q0)then begin
Result:=TBigInteger.Create(0);
Exit;
end;
Result:=OQOOOOC0Q0;
OQOOOOC0Q0:=nil;
finally
OO0OOOC0Q0.Free;
OQ0OOOC0Q0.Free;
OC0OOOC0Q0.Free;
OQOOOOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OCOOOOC0Q0(const O0QOOOC0Q0:integer;OOQOOOC0Q0:IScRandom);
var
OQQOOOC0Q0,OCQOOOC0Q0,O0COOOC0Q0,OOCOOOC0Q0:TBigInteger;
begin
if OOQOOOC0Q0=nil then
OOQOOOC0Q0:=OCCQ0QQ0Q0;
if OOQOOOC0Q0=nil then
raise Exception.Create(SInternalError);
OCQOOOC0Q0:=nil;
O0COOOC0Q0:=nil;
OOCOOOC0Q0:=nil;
OQQOOOC0Q0:=TBigInteger.Create(1);
try
OQOCOOC0Q0(O0QOOOC0Q0,160,OOQOOOC0Q0,OCQOOOC0Q0,O0COOOC0Q0);
OO0OQ0C0Q0.OCCC0O0OQ0:=OOC0OOC0Q0(O0COOOC0Q0,OCQOOOC0Q0,OOQOOOC0Q0);
OOCOOOC0Q0:=O0COOOC0Q0.Minus(OQQOOOC0Q0);
OO0OQ0C0Q0.OQCC0O0OQ0:=TBigInteger.Create;
repeat
OO0OQ0C0Q0.OQCC0O0OQ0.GenRandomBits(O0COOOC0Q0.BitCount,OOQOOOC0Q0);
until OO0OQ0C0Q0.OQCC0O0OQ0.Greater(OQQOOOC0Q0)and OO0OQ0C0Q0.OQCC0O0OQ0.Less(OOCOOOC0Q0);
OO0OQ0C0Q0.OQ000O0OQ0:=OO0OQ0C0Q0.OCCC0O0OQ0.ModPow(OO0OQ0C0Q0.OQCC0O0OQ0,OCQOOOC0Q0);
OO0OQ0C0Q0.O0000O0OQ0:=OCQOOOC0Q0;
OCQOOOC0Q0:=nil;
OO0OQ0C0Q0.OO000O0OQ0:=O0COOOC0Q0;
O0COOOC0Q0:=nil;
OQCQOOC0Q0:=OQOQQOQOQ0;
O0QQ0CC0Q0:=True;
finally
OQQOOOC0Q0.Free;
OCQOOOC0Q0.Free;
O0COOOC0Q0.Free;
OOCOOOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OOQQ0OC0Q0;
begin
OO0OQ0C0Q0.OQ000O0OQ0:=OO0OQ0C0Q0.OCCC0O0OQ0.ModPow(OO0OQ0C0Q0.OQCC0O0OQ0,OO0OQ0C0Q0.O0000O0OQ0);
end;
procedure O0CQOOC0Q0.OQCCOCQ0Q0(const OCCCOCQ0Q0:OOQQQOQOQ0;const O000OCQ0Q0:integer;OO00OCQ0Q0:IScRandom=nil);
begin
if not(OCCCOCQ0Q0 in[OCOQQOQOQ0,OQOQQOQOQ0])then
raise EScError.Create(seUseGenerateECMethod);
if(O000OCQ0Q0<=0)or((O000OCQ0Q0 mod 128)<>0)then
raise EScError.Create(seBitCountMultiple);
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OQCC0CC0Q0:=False;
case OCCCOCQ0Q0 of
OCOQQOQOQ0:
OQCOOOC0Q0(O000OCQ0Q0,OO00OCQ0Q0);
OQOQQOQOQ0:
OCOOOOC0Q0(O000OCQ0Q0,OO00OCQ0Q0);
else
Assert(False);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
OO0C0CC0Q0;
end;
procedure O0CQOOC0Q0.OQ00OCQ0Q0(const OC00OCQ0Q0:OOQOQOQOQ0;O0O0OCQ0Q0:IScRandom=nil);
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OQCC0CC0Q0:=False;
Assert(OC0OQ0C0Q0.OOQQQO0OQ0=nil);
OC0OQ0C0Q0.O0QQQO0OQ0:=OC00OCQ0Q0;
OC0OQ0C0Q0.OOQQQO0OQ0:=OQ0C00Q0Q0[OC00OCQ0Q0].CryptographyClass.Create(OQ0C00Q0Q0[OC00OCQ0Q0]);
try
OC0OQ0C0Q0.OOQQQO0OQ0.OOQCCO0OQ0(OC0OQ0C0Q0,O0O0OCQ0Q0);
except
O0OC0CC0Q0;
raise;
end;
OQCQOOC0Q0:=O0QQQOQOQ0;
O0QQ0CC0Q0:=True;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
OO0C0CC0Q0;
end;
class function O0CQOOC0Q0.OQQCQ0C0Q0(const OCQCQ0C0Q0,O0CCQ0C0Q0:OOCC0O0OQ0;OOCCQ0C0Q0:boolean):boolean;
begin
Result:=OCQCQ0C0Q0.OCCC0O0OQ0.Equal(O0CCQ0C0Q0.OCCC0O0OQ0)and OCQCQ0C0Q0.O0000O0OQ0.Equal(O0CCQ0C0Q0.O0000O0OQ0)and OCQCQ0C0Q0.OO000O0OQ0.Equal(O0CCQ0C0Q0.OO000O0OQ0)and OCQCQ0C0Q0.OQ000O0OQ0.Equal(O0CCQ0C0Q0.OQ000O0OQ0);
if not OOCCQ0C0Q0 and Result then
Result:=OCQCQ0C0Q0.OQCC0O0OQ0.Equal(O0CCQ0C0Q0.OQCC0O0OQ0);
end;
class function O0CQOOC0Q0.OQCCQ0C0Q0(const OCCCQ0C0Q0,O000Q0C0Q0:OC000O0OQ0;OO00Q0C0Q0:boolean):boolean;
begin
Result:=OCCCQ0C0Q0.OQQ00O0OQ0.Equal(O000Q0C0Q0.OQQ00O0OQ0)and OCCCQ0C0Q0.OCQ00O0OQ0.Equal(O000Q0C0Q0.OCQ00O0OQ0);
if not OO00Q0C0Q0 and Result then
Result:=OCCCQ0C0Q0.O0O00O0OQ0.Equal(O000Q0C0Q0.O0O00O0OQ0)and OCCCQ0C0Q0.OOO00O0OQ0.Equal(O000Q0C0Q0.OOO00O0OQ0)and OCCCQ0C0Q0.OQO00O0OQ0.Equal(O000Q0C0Q0.OQO00O0OQ0)and OCCCQ0C0Q0.OCO00O0OQ0.Equal(O000Q0C0Q0.OCO00O0OQ0);
end;
class function O0CQOOC0Q0.OQ00Q0C0Q0(const OC00Q0C0Q0,O0O0Q0C0Q0:OCOQQO0OQ0;OOO0Q0C0Q0:boolean):boolean;
begin
Result:=(OC00Q0C0Q0.OOQQQO0OQ0<>nil)and(O0O0Q0C0Q0.OOQQQO0OQ0<>nil)and
(OC00Q0C0Q0.OQQQQO0OQ0<>nil)and(O0O0Q0C0Q0.OQQQQO0OQ0<>nil)and
OC00Q0C0Q0.OOQQQO0OQ0.Equals(O0O0Q0C0Q0.OOQQQO0OQ0)and
OC00Q0C0Q0.OQQQQO0OQ0.Equals(O0O0Q0C0Q0.OQQQQO0OQ0);
if not OOO0Q0C0Q0 and Result then begin
Result:=(OC00Q0C0Q0.OCQQQO0OQ0<>nil)and(O0O0Q0C0Q0.OCQQQO0OQ0<>nil)and(OC00Q0C0Q0.OCQQQO0OQ0.ClassType=O0O0Q0C0Q0.OCQQQO0OQ0.ClassType);
if Result then
if IsClass(OC00Q0C0Q0.OCQQQO0OQ0,TBigInteger)then
Result:=TBigInteger(OC00Q0C0Q0.OCQQQO0OQ0).Equal(TBigInteger(O0O0Q0C0Q0.OCQQQO0OQ0))
else
if IsClass(OC00Q0C0Q0.OCQQQO0OQ0,T32BytesField)then
Result:=T32BytesField(OC00Q0C0Q0.OCQQQO0OQ0).Equal(T32BytesField(O0O0Q0C0Q0.OCQQQO0OQ0))
else
Result:=False;
end;
end;
function O0CQOOC0Q0.Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;
var
OCO0OCQ0Q0:O0CQOOC0Q0;
begin
Result:=IsClass(O0CC0CC0Q0,O0CQOOC0Q0);
if not Result then
Exit;
OCO0OCQ0Q0:=O0CQOOC0Q0(O0CC0CC0Q0);
OQOC0CC0Q0;
OCO0OCQ0Q0.OQOC0CC0Q0;
Result:=OQOOOCQ0Q0=OCO0OCQ0Q0.OQOOOCQ0Q0;
if Result then
case OQOOOCQ0Q0 of
OCOQQOQOQ0:
Result:=OQCCQ0C0Q0(OQ0OQ0C0Q0,OCO0OCQ0Q0.OQ0OQ0C0Q0,not(O0QOOCQ0Q0 and OCO0OCQ0Q0.O0QOOCQ0Q0));
OQOQQOQOQ0:
Result:=OQQCQ0C0Q0(OO0OQ0C0Q0,OCO0OCQ0Q0.OO0OQ0C0Q0,not(O0QOOCQ0Q0 and OCO0OCQ0Q0.O0QOOCQ0Q0));
O0QQQOQOQ0:
Result:=OQ00Q0C0Q0(OC0OQ0C0Q0,OCO0OCQ0Q0.OC0OQ0C0Q0,not(O0QOOCQ0Q0 and OCO0OCQ0Q0.O0QOOCQ0Q0));
else
Assert(False);
end;
end;
procedure O0CQOOC0Q0.OOO0OCQ0Q0(const OQQQCQC0Q0:OOOOCOQOQ0;out OCQQCQC0Q0:TBytes);
var
OQO0OCQ0Q0:THashAlgorithm;
begin
OQOC0CC0Q0;
OQO0OCQ0Q0:=OOCQCC0OQ0.OOOCCC0OQ0(OQQQCQC0Q0);
try
OQO0OCQ0Q0.ComputeHash(OQ0CO0C0Q0);
SetLength(OCQQCQC0Q0,Length(OQO0OCQ0Q0.Hash));
Buffer.BlockCopy(OQO0OCQ0Q0.Hash,0,OCQQCQC0Q0,0,Length(OQO0OCQ0Q0.Hash));
finally
OQO0OCQ0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OOO0OCQ0Q0(const OQQQCQC0Q0:OOOOCOQOQ0;out OCQQCQC0Q0:string);
var
OOCQCQC0Q0:TBytes;
begin
OOO0OCQ0Q0(OQQQCQC0Q0,OOCQCQC0Q0);
OCQQCQC0Q0:=O00QOCOOQ0(OOCQCQC0Q0,':');
end;
procedure O0CQOOC0Q0.O0000OC0Q0(OO000OC0Q0:TStringList;const OQ000OC0Q0:string;out OC000OC0Q0:string);
var
O0O00OC0Q0,OOO00OC0Q0,OQO00OC0Q0:Integer;
OCO00OC0Q0:string;
O0Q00OC0Q0,OOQ00OC0Q0:string;
OQQ00OC0Q0:string;
OCQ00OC0Q0,O0C00OC0Q0:string;
OOC00OC0Q0,OQC00OC0Q0,OCC00OC0Q0,O00O0OC0Q0,OO0O0OC0Q0:TBytes;
OQ0O0OC0Q0:THash_SHA1;
OC0O0OC0Q0:TSymmetricAlgorithm;
O0OO0OC0Q0:OCCO0OOCQ0;
begin
OC000OC0Q0:='';
OQQ00OC0Q0:='';
OCQ00OC0Q0:='';
O0C00OC0Q0:='';
SetLength(OOC00OC0Q0,0);
SetLength(OCC00OC0Q0,0);
O0O00OC0Q0:=0;
while O0O00OC0Q0<OO000OC0Q0.Count do begin
OCO00OC0Q0:=Trim(OO000OC0Q0.Strings[O0O00OC0Q0]);
Inc(O0O00OC0Q0);
OOO00OC0Q0:=Pos(':',OCO00OC0Q0);
if OOO00OC0Q0<=0 then
Continue;
O0Q00OC0Q0:=Trim(Copy(OCO00OC0Q0,1,OOO00OC0Q0-1));
OOQ00OC0Q0:=Trim(Copy(OCO00OC0Q0,OOO00OC0Q0+1,Length(OCO00OC0Q0)-OOO00OC0Q0));
if SameText(O0Q00OC0Q0,'PuTTY-User-Key-File-2')then begin
if SameText(OOQ00OC0Q0,DSA_TYPE_HEADER)then
OQCQOOC0Q0:=OQOQQOQOQ0
else
if SameText(OOQ00OC0Q0,RSA_TYPE_HEADER)then
OQCQOOC0Q0:=OCOQQOQOQ0
else
if SameText(OOQ00OC0Q0,ED25519_TYPE_HEADER)or
SameText(LeftStr(OOQ00OC0Q0,Length(ECDSA_TYPE_HEADER)),ECDSA_TYPE_HEADER)then
OQCQOOC0Q0:=O0QQQOQOQ0
else
raise EScError.CreateFmt(SUnknownAlgorithmS,[OOQ00OC0Q0],seUnknownAlgorithm);
end
else
if SameText(O0Q00OC0Q0,'Encryption')then
OQQ00OC0Q0:=OOQ00OC0Q0
else
if SameText(O0Q00OC0Q0,'Comment')then
OC000OC0Q0:=OOQ00OC0Q0
else
if SameText(O0Q00OC0Q0,'Public-Lines')then begin
OQO00OC0Q0:=StrToIntDef(OOQ00OC0Q0,-1);
if OQO00OC0Q0<=0 then
raise EScError.Create(seBrokenKey);
OCQ00OC0Q0:='';
while(OQO00OC0Q0>0)and(O0O00OC0Q0<OO000OC0Q0.Count)do begin
OCQ00OC0Q0:=OCQ00OC0Q0+OO000OC0Q0.Strings[O0O00OC0Q0];
Inc(O0O00OC0Q0);
Dec(OQO00OC0Q0);
end;
if OQO00OC0Q0<>0 then
raise EScError.Create(seBrokenKey);
end
else
if SameText(O0Q00OC0Q0,'Private-Lines')then begin
OQO00OC0Q0:=StrToIntDef(OOQ00OC0Q0,-1);
if OQO00OC0Q0<=0 then
raise EScError.Create(seBrokenKey);
O0C00OC0Q0:='';
while(OQO00OC0Q0>0)and(O0O00OC0Q0<OO000OC0Q0.Count)do begin
O0C00OC0Q0:=O0C00OC0Q0+OO000OC0Q0.Strings[O0O00OC0Q0];
Inc(O0O00OC0Q0);
Dec(OQO00OC0Q0);
end;
if OQO00OC0Q0<>0 then
raise EScError.Create(seBrokenKey);
end;
end;
if OCQ00OC0Q0<>'' then begin
OOC00OC0Q0:=TBase64.Decode(Encoding.Default.GetBytes(OCQ00OC0Q0));
OC00QOC0Q0(OOC00OC0Q0);
end;
OOOCOOC0Q0;
if O0C00OC0Q0<>'' then begin
OOC00OC0Q0:=TBase64.Decode(Encoding.Default.GetBytes(O0C00OC0Q0));
if not SameText(OQQ00OC0Q0,'none')then begin
if OQ000OC0Q0='' then
raise EScError.Create(sePasswordNotSpecified);
OQ0O0OC0Q0:=THash_SHA1.Create;
try
OCC00OC0Q0:=Encoding.Default.GetBytes(OQ000OC0Q0);
SetLength(OQC00OC0Q0,4+Length(OCC00OC0Q0));
SetLength(O00O0OC0Q0,2*OQ0O0OC0Q0.HashSize);
OQC00OC0Q0[0]:=0;
OQC00OC0Q0[1]:=0;
OQC00OC0Q0[2]:=0;
OQC00OC0Q0[3]:=0;
Move(OCC00OC0Q0[0],OQC00OC0Q0[4],Length(OCC00OC0Q0));
OQ0O0OC0Q0.ComputeHash(OQC00OC0Q0);
Move(OQ0O0OC0Q0.Hash[0],O00O0OC0Q0[0],OQ0O0OC0Q0.HashSize);
OQC00OC0Q0[3]:=1;
OQ0O0OC0Q0.ComputeHash(OQC00OC0Q0);
Move(OQ0O0OC0Q0.Hash[0],O00O0OC0Q0[OQ0O0OC0Q0.HashSize],OQ0O0OC0Q0.HashSize);
finally
OQ0O0OC0Q0.Free;
end;
SetLength(O00O0OC0Q0,32);
SetLength(OO0O0OC0Q0,16);
FillChar(OO0O0OC0Q0[0],Length(OO0O0OC0Q0),0);
OC0O0OC0Q0:=OOCQCC0OQ0.OQCQCC0OQ0(OOCQCC0OQ0.OCO0CC0OQ0(OQQ00OC0Q0),O00O0OC0Q0,OO0O0OC0Q0);
OC0O0OC0Q0.CreateDecryptor.TransformBlock(@OOC00OC0Q0[0],0,Length(OOC00OC0Q0),@OOC00OC0Q0[0],0);
end;
O0OO0OC0Q0:=OCCO0OOCQ0.Create(OOC00OC0Q0);
try
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
OQ0OQ0C0Q0.O0O00O0OQ0:=O0OO0OC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.OOO00O0OQ0:=O0OO0OC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.OQO00O0OQ0:=O0OO0OC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.OCO00O0OQ0:=O0OO0OC0Q0.OOCQ0OOCQ0;
OQQQ0OC0Q0;
end;
OQOQQOQOQ0:begin
OO0OQ0C0Q0.OQCC0O0OQ0:=O0OO0OC0Q0.OOCQ0OOCQ0;
end;
O0QQQOQOQ0:begin
if IsClass(OC0OQ0C0Q0.OOQQQO0OQ0,OQ00000OQ0)then begin
OQC00OC0Q0:=O0OO0OC0Q0.OQOQ0OOCQ0;
OC0OQ0C0Q0.OCQQQO0OQ0:=T32BytesField.Create(OQC00OC0Q0);
end
else
OC0OQ0C0Q0.OCQQQO0OQ0:=O0OO0OC0Q0.OOCQ0OOCQ0;
end;
else
raise EScError.CreateFmt(SUnknownAlgorithmS,['EC'],seUnknownAlgorithm);
end;
finally
O0OO0OC0Q0.Free;
end;
end;
O0QQ0CC0Q0:=True;
end;
function O0CQOOC0Q0.OOOO0OC0Q0(OQOO0OC0Q0:TStringList;const OCOO0OC0Q0:string;out O0QO0OC0Q0:string):TBytes;
var
OOQO0OC0Q0:string;
OQQO0OC0Q0:string;
OCQO0OC0Q0:StringBuilder;
O0CO0OC0Q0:integer;
begin
O0QO0OC0Q0:='';
OCQO0OC0Q0:=StringBuilder.Create;
try
O0CO0OC0Q0:=1;
while O0CO0OC0Q0<OQOO0OC0Q0.Count do begin
OOQO0OC0Q0:=Trim(OQOO0OC0Q0[O0CO0OC0Q0]);
if SameText(LeftStr(OOQO0OC0Q0,Length(OCOO0OC0Q0)),OCOO0OC0Q0)then
Break;
if Pos(':',OOQO0OC0Q0)=0 then
OCQO0OC0Q0.Append(OOQO0OC0Q0)
else begin
OQQO0OC0Q0:=OOQO0OC0Q0;
while(O0CO0OC0Q0<OQOO0OC0Q0.Count-1)and(OOQO0OC0Q0[Length(OOQO0OC0Q0)]='\')do begin
Delete(OOQO0OC0Q0,Length(OOQO0OC0Q0),1);
OQQO0OC0Q0:=OQQO0OC0Q0+OOQO0OC0Q0;
Inc(O0CO0OC0Q0);
OOQO0OC0Q0:=OQOO0OC0Q0[O0CO0OC0Q0];
end;
if SameText(LeftStr(OQQO0OC0Q0,Length(COMMENT_LINE)),COMMENT_LINE)then
O0QO0OC0Q0:=Trim(Copy(OOQO0OC0Q0,Length(COMMENT_LINE)+1,Length(OOQO0OC0Q0)));
end;
Inc(O0CO0OC0Q0);
end;
if O0CO0OC0Q0>=OQOO0OC0Q0.Count then
O0OCOOC0Q0;
Result:=TBase64.Decode(Encoding.Default.GetBytes(OCQO0OC0Q0.ToString));
finally
OCQO0OC0Q0.Free;
end;
end;
function O0CQOOC0Q0.OOCO0OC0Q0(OQCO0OC0Q0:TStringList;const OCCO0OC0Q0:string;const O00QCOC0Q0:string;out OO0QCOC0Q0:string):TBytes;
var
OQ0QCOC0Q0,OC0QCOC0Q0,O0OQCOC0Q0:string;
OOOQCOC0Q0,OQOQCOC0Q0:TBytes;
OCOQCOC0Q0:TSymmetricAlgorithm;
O0QQCOC0Q0:OO00COQOQ0;
OOQQCOC0Q0,OQQQCOC0Q0:integer;
begin
if OQCO0OC0Q0.Count>3 then
OQ0QCOC0Q0:=OQCO0OC0Q0[1]
else
OQ0QCOC0Q0:='';
if LeftStr(OQ0QCOC0Q0,Length(PEM_PROC_TYPE_LINE))=PEM_PROC_TYPE_LINE then begin
OQ0QCOC0Q0:=OQCO0OC0Q0[2];
SetLength(OOOQCOC0Q0,0);
SetLength(OQOQCOC0Q0,0);
if LeftStr(OQ0QCOC0Q0,Length(PEM_INFO_LINE))=PEM_INFO_LINE then begin
OQQQCOC0Q0:=Pos(',',OQ0QCOC0Q0);
if OQQQCOC0Q0=0 then
O0OCOOC0Q0;
OC0QCOC0Q0:=Copy(OQ0QCOC0Q0,Length(PEM_INFO_LINE)+1,OQQQCOC0Q0-Length(PEM_INFO_LINE)-1);
O0OQCOC0Q0:=Trim(Copy(OQ0QCOC0Q0,OQQQCOC0Q0+1,Length(OQ0QCOC0Q0)));
SetLength(OQOQCOC0Q0,Length(O0OQCOC0Q0)div 2);
for OOQQCOC0Q0:=0 to Length(OQOQCOC0Q0)-1 do
OQOQCOC0Q0[OOQQCOC0Q0]:=StrToInt('$'+O0OQCOC0Q0[OOQQCOC0Q0*2+1]+O0OQCOC0Q0[OOQQCOC0Q0*2+2]);
end;
if OC0QCOC0Q0=PEM_3DESCBC_ALG then
O0QQCOC0Q0:=OOOCCOQOQ0
else
if OC0QCOC0Q0=PEM_AES128CBC_ALG then
O0QQCOC0Q0:=OCOCCOQOQ0
else
if OC0QCOC0Q0=PEM_AES192CBC_ALG then
O0QQCOC0Q0:=O0QCCOQOQ0
else
if OC0QCOC0Q0=PEM_AES256CBC_ALG then
O0QQCOC0Q0:=OOQCCOQOQ0
else
if OC0QCOC0Q0=PEM_AES128CTR_ALG then
O0QQCOC0Q0:=OOCCCOQOQ0
else
if OC0QCOC0Q0=PEM_AES192CTR_ALG then
O0QQCOC0Q0:=OQCCCOQOQ0
else
if OC0QCOC0Q0=PEM_AES256CTR_ALG then
O0QQCOC0Q0:=OCCCCOQOQ0
else
raise EScError.Create(seCipherNotSupported);
OOOQCOC0Q0:=OOQ0000OQ0.OQOO000OQ0(OQ0OCOQOQ0,OCCO0OC0Q0,OQOQCOC0Q0,OOCQCC0OQ0.OCCCCC0OQ0(O0QQCOC0Q0),1,False);
OCOQCOC0Q0:=OOCQCC0OQ0.OQCQCC0OQ0(O0QQCOC0Q0,OOOQCOC0Q0,OQOQCOC0Q0);
FillChar(OOOQCOC0Q0[0],Length(OOOQCOC0Q0),0);
Result:=OOOO0OC0Q0(OQCO0OC0Q0,O00QCOC0Q0,OO0QCOC0Q0);
OCOQCOC0Q0.CreateDecryptor.TransformBlock(Result,0,Length(Result));
end
else
Result:=OOOO0OC0Q0(OQCO0OC0Q0,O00QCOC0Q0,OO0QCOC0Q0);
end;
procedure O0CQOOC0Q0.OCQQCOC0Q0(O0CQCOC0Q0:TStream;const OOCQCOC0Q0:string;
OQCQCOC0Q0:OO00COQOQ0;var OCCQCOC0Q0:TBytes);
var
O00CCOC0Q0:TBytes;
OO0CCOC0Q0:TSymmetricAlgorithm;
OQ0CCOC0Q0:TBytes;
OC0CCOC0Q0,O0OCCOC0Q0,OOOCCOC0Q0:integer;
OQOCCOC0Q0,OCOCCOC0Q0:string;
begin
case OQCQCOC0Q0 of
OOOCCOQOQ0:
OQOCCOC0Q0:=PEM_3DESCBC_ALG;
OCOCCOQOQ0:
OQOCCOC0Q0:=PEM_AES128CBC_ALG;
O0QCCOQOQ0:
OQOCCOC0Q0:=PEM_AES192CBC_ALG;
OOQCCOQOQ0:
OQOCCOC0Q0:=PEM_AES256CBC_ALG;
OOCCCOQOQ0:
OQOCCOC0Q0:=PEM_AES128CTR_ALG;
OQCCCOQOQ0:
OQOCCOC0Q0:=PEM_AES192CTR_ALG;
OCCCCOQOQ0:
OQOCCOC0Q0:=PEM_AES256CTR_ALG;
else
raise EScError.Create(seCipherNotSupported);
end;
OCOCCOC0Q0:=PEM_INFO_LINE+OQOCCOC0Q0+',';
O0OOC0QOQ0.OC0QQ0QOQ0(O0CQCOC0Q0,PEM_PROC_TYPE_LINE);
O0CQCOC0Q0.WriteBuffer(Encoding.Default.GetBytes(OCOCCOC0Q0)[0],Length(OCOCCOC0Q0));
OC0CCOC0Q0:=Length(OCCQCOC0Q0);
OOOCCOC0Q0:=OOCQCC0OQ0.OO00CC0OQ0(OQCQCOC0Q0);
O0OCCOC0Q0:=OC0CCOC0Q0 mod OOOCCOC0Q0;
if O0OCCOC0Q0>0 then begin
O0OCCOC0Q0:=OOOCCOC0Q0-O0OCCOC0Q0;
if O0OCCOC0Q0>0 then begin
SetLength(OCCQCOC0Q0,OC0CCOC0Q0+O0OCCOC0Q0);
System.FillChar(OCCQCOC0Q0[OC0CCOC0Q0],O0OCCOC0Q0,O0OCCOC0Q0);
end;
end;
SetLength(OQ0CCOC0Q0,OOOCCOC0Q0);
if OCCQ0QQ0Q0=nil then
raise Exception.Create(SInternalError);
OCCQ0QQ0Q0.Random(OQ0CCOC0Q0,0,Length(OQ0CCOC0Q0));
O0OOC0QOQ0.OC0QQ0QOQ0(O0CQCOC0Q0,O00QOCOOQ0(OQ0CCOC0Q0));
O0OOC0QOQ0.OC0QQ0QOQ0(O0CQCOC0Q0,'');
O00CCOC0Q0:=OOQ0000OQ0.OQOO000OQ0(OQ0OCOQOQ0,OOCQCOC0Q0,OQ0CCOC0Q0,OOCQCC0OQ0.OCCCCC0OQ0(OQCQCOC0Q0),1,False);
OO0CCOC0Q0:=OOCQCC0OQ0.OQCQCC0OQ0(OQCQCOC0Q0,O00CCOC0Q0,OQ0CCOC0Q0);
FillChar(O00CCOC0Q0[0],Length(O00CCOC0Q0),0);
OO0CCOC0Q0.CreateEncryptor.TransformBlock(OCCQCOC0Q0,0,Length(OCCQCOC0Q0));
end;
function O0CQOOC0Q0.O0QCCOC0Q0(const OOQCCOC0Q0:string;out OQQCCOC0Q0:string):TBytes;
var
OCQCCOC0Q0:integer;
O0CCCOC0Q0,OOCCCOC0Q0:integer;
begin
OCQCCOC0Q0:=Length(OOQCCOC0Q0);
O0CCCOC0Q0:=1;
while(O0CCCOC0Q0<=OCQCCOC0Q0)and(OOQCCOC0Q0[O0CCCOC0Q0]<>' ')do
Inc(O0CCCOC0Q0);
OOCCCOC0Q0:=O0CCCOC0Q0+1;
while(OOCCCOC0Q0<=OCQCCOC0Q0)and(OOQCCOC0Q0[OOCCCOC0Q0]<>' ')do
Inc(OOCCCOC0Q0);
if OOCCCOC0Q0>OCQCCOC0Q0 then
OQQCCOC0Q0:=''
else
OQQCCOC0Q0:=Copy(OOQCCOC0Q0,OOCCCOC0Q0+1,Length(OOQCCOC0Q0));
Result:=TBase64.Decode(Encoding.Default.GetBytes(Copy(OOQCCOC0Q0,O0CCCOC0Q0+1,OOCCCOC0Q0-O0CCCOC0Q0-1)));
end;
procedure O0CQOOC0Q0.OOQQOCQ0Q0(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string;out OQQQOCQ0Q0:string);
var
OCQQOCQ0Q0:string;
O0CQOCQ0Q0:Int64;
OOCQOCQ0Q0:string;
OQCQOCQ0Q0:TStringList;
OCCQOCQ0Q0,O00COCQ0Q0:TBytes;
OO0COCQ0Q0:integer;
OQ0COCQ0Q0:OOQQQOQOQ0;
OC0COCQ0Q0:boolean;
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OQCC0CC0Q0:=False;
OQQQOCQ0Q0:='';
if OCQCOQC0Q0.Position=OCQCOQC0Q0.Size then
raise EScError.CreateFmt(SBrokenKey+': Stream.Position = Stream.Size',[],seBrokenKey);
O0CQOCQ0Q0:=OCQCOQC0Q0.Position;
SetLength(OCCQOCQ0Q0,Min(OCQCOQC0Q0.Size-OCQCOQC0Q0.Position,1024*1024));
OCQCOQC0Q0.Read(OCCQOCQ0Q0[0],Length(OCCQOCQ0Q0));
OOCQOCQ0Q0:=Encoding.Default.GetString(OCCQOCQ0Q0);
OO0COCQ0Q0:=Pos('-----BEGIN ',OOCQOCQ0Q0);
if OO0COCQ0Q0=0 then
OO0COCQ0Q0:=Pos('---- BEGIN ',OOCQOCQ0Q0);
if OO0COCQ0Q0>1 then
Delete(OOCQOCQ0Q0,1,OO0COCQ0Q0-1);
OQCQOCQ0Q0:=TStringList.Create;
try
try
OQCQOCQ0Q0.Text:=OOCQOCQ0Q0;
if OQCQOCQ0Q0.Count>0 then
OCQQOCQ0Q0:=OQCQOCQ0Q0[0]
else
OCQQOCQ0Q0:='';
if SameText(LeftStr(OCQQOCQ0Q0,Length(PUTTY_KEY_HEADER_2)),PUTTY_KEY_HEADER_2)then begin
O0000OC0Q0(OQCQOCQ0Q0,OQQCOQC0Q0,OQQQOCQ0Q0);
end
else
if SameText(LeftStr(OCQQOCQ0Q0,Length(RSA_TYPE_HEADER)),RSA_TYPE_HEADER)or
SameText(LeftStr(OCQQOCQ0Q0,Length(DSA_TYPE_HEADER)),DSA_TYPE_HEADER)or
SameText(LeftStr(OCQQOCQ0Q0,Length(ECDSA_TYPE_HEADER)),ECDSA_TYPE_HEADER)or
SameText(LeftStr(OCQQOCQ0Q0,Length(ED25519_TYPE_HEADER)),ED25519_TYPE_HEADER)or
SameText(LeftStr(OCQQOCQ0Q0,Length(SCurve25519_Sha256)),SCurve25519_Sha256)then begin
OCCQOCQ0Q0:=O0QCCOC0Q0(OCQQOCQ0Q0,OQQQOCQ0Q0);
O00OCOC0Q0(OCCQOCQ0Q0);
end
else
if OCQQOCQ0Q0=PKCS1_RSA_PRIV_HEADER then begin
OCCQOCQ0Q0:=OOCO0OC0Q0(OQCQOCQ0Q0,OQQCOQC0Q0,PKCS1_RSA_PRIV_FOOTER,OQQQOCQ0Q0);
OOQ0COC0Q0(OCCQOCQ0Q0,OCOQQOQOQ0,False);
end
else
if OCQQOCQ0Q0=PKCS1_RSA_PUB_HEADER then begin
OCCQOCQ0Q0:=OOOO0OC0Q0(OQCQOCQ0Q0,PKCS1_RSA_PUB_FOOTER,OQQQOCQ0Q0);
OOQ0COC0Q0(OCCQOCQ0Q0,OCOQQOQOQ0,True);
end
else
if OCQQOCQ0Q0=PKCS1_DSA_PRIV_HEADER then begin
OCCQOCQ0Q0:=OOCO0OC0Q0(OQCQOCQ0Q0,OQQCOQC0Q0,PKCS1_DSA_PRIV_FOOTER,OQQQOCQ0Q0);
OOQ0COC0Q0(OCCQOCQ0Q0,OQOQQOQOQ0,False);
end
else
if OCQQOCQ0Q0=PKCS1_DSA_PUB_HEADER then begin
OCCQOCQ0Q0:=OOOO0OC0Q0(OQCQOCQ0Q0,PKCS1_DSA_PUB_FOOTER,OQQQOCQ0Q0);
OOQ0COC0Q0(OCCQOCQ0Q0,OQOQQOQOQ0,True);
end
else
if OCQQOCQ0Q0=EC_PRIV_HEADER then begin
OCCQOCQ0Q0:=OOCO0OC0Q0(OQCQOCQ0Q0,OQQCOQC0Q0,EC_PRIV_FOOTER,OQQQOCQ0Q0);
OOQ0COC0Q0(OCCQOCQ0Q0,O0QQQOQOQ0,False);
end
else
if OCQQOCQ0Q0=OPENSSH_PRIV_HEADER then begin
OCCQOCQ0Q0:=OOCO0OC0Q0(OQCQOCQ0Q0,OQQCOQC0Q0,OPENSSH_PRIV_FOOTER,OQQQOCQ0Q0);
OQOOCOC0Q0(OCCQOCQ0Q0,OQQCOQC0Q0);
end
else
if OCQQOCQ0Q0=EC_PARAM_HEADER then begin
SetLength(O00COCQ0Q0,0);
O00COCQ0Q0:=OOOO0OC0Q0(OQCQOCQ0Q0,EC_PARAM_FOOTER,OQQQOCQ0Q0);
while OQCQOCQ0Q0.Count>0 do begin
if SameText(OQCQOCQ0Q0[0],EC_PARAM_FOOTER)then begin
OQCQOCQ0Q0.Delete(0);
Break;
end;
OQCQOCQ0Q0.Delete(0);
end;
if(OQCQOCQ0Q0.Count=0)or(OQCQOCQ0Q0[0]<>EC_PRIV_HEADER)then
O0OCOOC0Q0;
OCCQOCQ0Q0:=OOCO0OC0Q0(OQCQOCQ0Q0,OQQCOQC0Q0,EC_PRIV_FOOTER,OQQQOCQ0Q0);
OOO0QOC0Q0(OCCQOCQ0Q0,O00COCQ0Q0);
end
else
if OCQQOCQ0Q0=PKCS1_PUB_HEADER then begin
OCCQOCQ0Q0:=OOOO0OC0Q0(OQCQOCQ0Q0,PKCS1_PUB_FOOTER,OQQQOCQ0Q0);
OCOQ00C0Q0(OCCQOCQ0Q0,True);
if not O0QQ0CC0Q0 then
OOQ0COC0Q0(OCCQOCQ0Q0,OCOQQOQOQ0,True);
if not O0QQ0CC0Q0 then
OOQ0COC0Q0(OCCQOCQ0Q0,OQOQQOQOQ0,True);
if not O0QQ0CC0Q0 then
OOQ0COC0Q0(OCCQOCQ0Q0,O0QQQOQOQ0,True);
end
else
if OCQQOCQ0Q0=PKCS8_HEADER then begin
OCCQOCQ0Q0:=OOOO0OC0Q0(OQCQOCQ0Q0,PKCS8_FOOTER,OQQQOCQ0Q0);
OCOQQOC0Q0(OCCQOCQ0Q0);
end
else
if OCQQOCQ0Q0=PKCS8_ENC_HEADER then begin
OCCQOCQ0Q0:=OOOO0OC0Q0(OQCQOCQ0Q0,PKCS8_ENC_FOOTER,OQQQOCQ0Q0);
OCCQQOC0Q0(OCCQOCQ0Q0,OQQCOQC0Q0);
end
else
if OCQQOCQ0Q0=IETF_PRIV_HEADER then begin
OCCQOCQ0Q0:=OOOO0OC0Q0(OQCQOCQ0Q0,IETF_PRIV_FOOTER,OQQQOCQ0Q0);
OCOCQOC0Q0(OCCQOCQ0Q0,OQQCOQC0Q0);
end
else
if OCQQOCQ0Q0=IETF_PUB_HEADER then begin
OCCQOCQ0Q0:=OOOO0OC0Q0(OQCQOCQ0Q0,IETF_PUB_FOOTER,OQQQOCQ0Q0);
OC00QOC0Q0(OCCQOCQ0Q0);
end
else begin
if O0O0COC0Q0(OCCQOCQ0Q0,OQ0COCQ0Q0,OC0COCQ0Q0)then
try
OOQ0COC0Q0(OCCQOCQ0Q0,OQ0COCQ0Q0,OC0COCQ0Q0);
except
end;
try
if not O0QQ0CC0Q0 then
OCOQQOC0Q0(OCCQOCQ0Q0);
except
end;
if not O0QQ0CC0Q0 then begin
OCQCOQC0Q0.Position:=O0CQOCQ0Q0;
OCCCQ00OQ0.OQ0COCC0Q0(OCQCOQC0Q0,Self,OOOCCOQOQ0,'');
end;
end;
except
O0OC0CC0Q0;
raise;
end;
finally
OQCQOCQ0Q0.Free;
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
OOOCOOC0Q0;
OO0C0CC0Q0;
end;
procedure O0CQOOC0Q0.OOQQOCQ0Q0(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string='');
var
OQQQOCQ0Q0:string;
begin
OOQQOCQ0Q0(OCQCOQC0Q0,OQQCOQC0Q0,OQQQOCQ0Q0);
end;
procedure O0CQOOC0Q0.OOQQOCQ0Q0(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string;out OQQQOCQ0Q0:string);
var
OCQCOQC0Q0:TFileStream;
begin
OCQCOQC0Q0:=TFileStream.Create(OOQCOQC0Q0,fmOpenRead);
try
OOQQOCQ0Q0(OCQCOQC0Q0,OQQCOQC0Q0,OQQQOCQ0Q0);
finally
OCQCOQC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OOQQOCQ0Q0(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string='');
var
OQQQOCQ0Q0:string;
begin
OOQQOCQ0Q0(OOQCOQC0Q0,OQQCOQC0Q0,OQQQOCQ0Q0);
end;
function O0CQOOC0Q0.O0O0COC0Q0(const OOO0COC0Q0:TBytes;out OQO0COC0Q0:OOQQQOQOQ0;out OCO0COC0Q0:boolean):boolean;
var
O0Q0COC0Q0:OQCOCQOOQ0;
begin
O0Q0COC0Q0:=OQCOCQOOQ0.Create;
try
Result:=True;
if O0Q0COC0Q0.OCQCOOOOQ0(OOCQCCOOQ0,OOO0COC0Q0)then begin
OQO0COC0Q0:=OCOQQOQOQ0;
OCO0COC0Q0:=False;
Exit;
end;
if O0Q0COC0Q0.OCQCOOOOQ0(OCCQCCOOQ0,OOO0COC0Q0)then begin
OQO0COC0Q0:=OQOQQOQOQ0;
OCO0COC0Q0:=False;
Exit;
end;
if O0Q0COC0Q0.OCQCOOOOQ0(OQCQCCOOQ0,OOO0COC0Q0)then begin
OQO0COC0Q0:=OQOQQOQOQ0;
OCO0COC0Q0:=True;
Exit;
end;
if O0Q0COC0Q0.OCQCOOOOQ0(O0CQCCOOQ0,OOO0COC0Q0)then begin
OQO0COC0Q0:=OCOQQOQOQ0;
OCO0COC0Q0:=True;
Exit;
end;
if O0Q0COC0Q0.OCQCOOOOQ0(OQ0CCCOOQ0,OOO0COC0Q0)then begin
OQO0COC0Q0:=O0QQQOQOQ0;
OCO0COC0Q0:=False;
Exit;
end;
if O0Q0COC0Q0.OCQCOOOOQ0(O00CCCOOQ0,OOO0COC0Q0)then begin
OQO0COC0Q0:=O0QQQOQOQ0;
OCO0COC0Q0:=True;
Exit;
end;
Result:=False;
finally
O0Q0COC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OOQ0COC0Q0(const OQQ0COC0Q0:TBytes;OCQ0COC0Q0:OOQQQOQOQ0;O0C0COC0Q0:boolean);
var
OOC0COC0Q0:OQCOCQOOQ0;
OQC0COC0Q0:string;
OCC0COC0Q0:TBytes;
begin
OOC0COC0Q0:=OQCOCQOOQ0.Create;
try
OQCQOOC0Q0:=OCQ0COC0Q0;
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
if O0C0COC0Q0 then begin
if not OOC0COC0Q0.OCQCOOOOQ0(O0CQCCOOQ0,OQQ0COC0Q0)then
Exit;
FreeAndNil(OQ0OQ0C0Q0.O0O00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OOO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OQO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OCO00O0OQ0);
end
else begin
if not OOC0COC0Q0.OCQCOOOOQ0(OOCQCCOOQ0,OQQ0COC0Q0)then
Exit;
OQ0OQ0C0Q0.O0O00O0OQ0:=TBigInteger.Create(OOC0COC0Q0['D'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OOO00O0OQ0:=TBigInteger.Create(OOC0COC0Q0['P'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OQO00O0OQ0:=TBigInteger.Create(OOC0COC0Q0['Q'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OCO00O0OQ0:=TBigInteger.Create(OOC0COC0Q0['U'].OCQQ0QOOQ0);
OQQQ0OC0Q0;
end;
OQ0OQ0C0Q0.OCQ00O0OQ0:=TBigInteger.Create(OOC0COC0Q0['PubMod'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OQQ00O0OQ0:=TBigInteger.Create(OOC0COC0Q0['PubExp'].OCQQ0QOOQ0);
end;
OQOQQOQOQ0:begin
if O0C0COC0Q0 then begin
if not OOC0COC0Q0.OCQCOOOOQ0(OQCQCCOOQ0,OQQ0COC0Q0)then
Exit;
FreeAndNil(OO0OQ0C0Q0.OQCC0O0OQ0);
end
else begin
if not OOC0COC0Q0.OCQCOOOOQ0(OCCQCCOOQ0,OQQ0COC0Q0)then
Exit;
OO0OQ0C0Q0.OQCC0O0OQ0:=TBigInteger.Create(OOC0COC0Q0['X'].OCQQ0QOOQ0);
end;
OO0OQ0C0Q0.O0000O0OQ0:=TBigInteger.Create(OOC0COC0Q0['P'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OO000O0OQ0:=TBigInteger.Create(OOC0COC0Q0['Q'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OCCC0O0OQ0:=TBigInteger.Create(OOC0COC0Q0['G'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OQ000O0OQ0:=TBigInteger.Create(OOC0COC0Q0['Y'].OCQQ0QOOQ0);
end;
O0QQQOQOQ0:begin
if not O0C0COC0Q0 then begin
OOO0QOC0Q0(OQQ0COC0Q0,nil);
if not O0QQ0CC0Q0 then
OCQ0QOC0Q0(OQQ0COC0Q0,OQCC000OQ0);
if not O0QQ0CC0Q0 then
Exit;
end
else begin
if not OOC0COC0Q0.OCQCOOOOQ0(O00CCCOOQ0,OQQ0COC0Q0)then
Exit;
SetLength(OCC0COC0Q0,0);
OQC0COC0Q0:=OOC0COC0Q0['Algorithm']['Algorithm'].OCOQ0QOOQ0;
OCC0COC0Q0:=OOC0COC0Q0['SubjectPublicKey'].OQOQ0QOOQ0;
if OQC0COC0Q0=OID_EC_PUBLIC_KEY then
O0OOQOC0Q0(OOC0COC0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0)
else
if OQC0COC0Q0=OID_EC_PUBLIC_KEY_RESTRICTED then begin
if not OOC0COC0Q0.OCQCOOOOQ0(OCQ0CCOOQ0,OOC0COC0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0)then
raise EScError.Create(seUnknownECParametersFormat);
O0OOQOC0Q0(OOC0COC0Q0['EcDomain'].OQOQ0QOOQ0);
end
else
if OQC0COC0Q0=OID_X25519_PUBLICKEY then
O00OQOC0Q0(OOC0COC0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0,OQCC000OQ0)
else
if OQC0COC0Q0=OID_X25519 then
O00OQOC0Q0(OOC0COC0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0,OQCC000OQ0)
else
if OQC0COC0Q0=OID_Ed25519 then
O00OQOC0Q0(OOC0COC0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0,OCCC000OQ0)
else
raise EScError.Create(seUnknownECParametersFormat);
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
OC0OQ0C0Q0.OQQQQO0OQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCO0CO0OQ0(OCC0COC0Q0,0,Length(OCC0COC0Q0));
end;
end;
else
Assert(False);
end;
O0QQ0CC0Q0:=True;
finally
OOC0COC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.O00OCOC0Q0(const OO0OCOC0Q0:TBytes);
var
OQ0OCOC0Q0:OCCO0OOCQ0;
OC0OCOC0Q0,O0OOCOC0Q0:string;
OOOOCOC0Q0:TBytes;
begin
OQ0OCOC0Q0:=OCCO0OOCQ0.Create(OO0OCOC0Q0);
try
OC0OCOC0Q0:=Encoding.Default.GetString(OQ0OCOC0Q0.OQOQ0OOCQ0);
OQCQOOC0Q0:=OOCQCC0OQ0.OOOOCC0OQ0(OC0OCOC0Q0);
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
OQ0OQ0C0Q0.OQQ00O0OQ0:=OQ0OCOC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.OCQ00O0OQ0:=OQ0OCOC0Q0.OOCQ0OOCQ0;
FreeAndNil(OQ0OQ0C0Q0.O0O00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OOO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OQO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OCO00O0OQ0);
end;
OQOQQOQOQ0:begin
OO0OQ0C0Q0.O0000O0OQ0:=OQ0OCOC0Q0.OOCQ0OOCQ0;
OO0OQ0C0Q0.OO000O0OQ0:=OQ0OCOC0Q0.OOCQ0OOCQ0;
OO0OQ0C0Q0.OCCC0O0OQ0:=OQ0OCOC0Q0.OOCQ0OOCQ0;
OO0OQ0C0Q0.OQ000O0OQ0:=OQ0OCOC0Q0.OOCQ0OOCQ0;
FreeAndNil(OO0OQ0C0Q0.OQCC0O0OQ0);
end;
O0QQQOQOQ0:begin
if SameText(OC0OCOC0Q0,ED25519_TYPE_HEADER)then begin
OC0OQ0C0Q0.O0QQQO0OQ0:=OCCCQOQOQ0;
OC0OQ0C0Q0.OOQQQO0OQ0:=OQ00000OQ0.Create;
OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OC00000OQ0:=OCCC000OQ0;
end
else begin
O0OOCOC0Q0:=Encoding.Default.GetString(OQ0OCOC0Q0.OQOQ0OOCQ0);
OC0OQ0C0Q0.O0QQQO0OQ0:=OC0OO0Q0Q0.OOCQ00Q0Q0(O0OOCOC0Q0);
OC0OQ0C0Q0.OOQQQO0OQ0:=OQ0C00Q0Q0[OC0OQ0C0Q0.O0QQQO0OQ0].CryptographyClass.Create(OQ0C00Q0Q0[OC0OQ0C0Q0.O0QQQO0OQ0]);
end;
SetLength(OOOOCOC0Q0,0);
OOOOCOC0Q0:=OQ0OCOC0Q0.OQOQ0OOCQ0;
OC0OQ0C0Q0.OQQQQO0OQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCO0CO0OQ0(OOOOCOC0Q0,0,Length(OOOOCOC0Q0));
end;
else
Assert(False);
end;
if OQ0OCOC0Q0.OQ0C0OOCQ0<>0 then
O0OCOOC0Q0;
O0QQ0CC0Q0:=True;
finally
OQ0OCOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OQOOCOC0Q0(const OCOOCOC0Q0:TBytes;const O0QOCOC0Q0:string);
var
OOQOCOC0Q0:OCCO0OOCQ0;
OQQOCOC0Q0,OCQOCOC0Q0,O0COCOC0Q0:string;
OOCOCOC0Q0:TBytes;
OQCOCOC0Q0:TBytes;
OCCOCOC0Q0:TBytes;
O00QQOC0Q0,OO0QQOC0Q0:integer;
OQ0QQOC0Q0:string;
OC0QQOC0Q0:TSymmetricAlgorithm;
O0OQQOC0Q0,OOOQQOC0Q0,OQOQQOC0Q0:TBytes;
begin
{$IFNDEF VER9P}
SetLength(OQCOCOC0Q0,0);
SetLength(OOCOCOC0Q0,0);
SetLength(OOOQQOC0Q0,0);
SetLength(OCCOCOC0Q0,0);
{$ENDIF}
OOQOCOC0Q0:=OCCO0OOCQ0.Create(OCOOCOC0Q0);
try
OQQOCOC0Q0:=Encoding.Default.GetString(OOQOCOC0Q0.OCQQ0OOCQ0(Length(OPENSSH_AUTH_MAGIC)));
if not SameText(OQQOCOC0Q0,OPENSSH_AUTH_MAGIC)then
raise EScError.Create(seWrongDataFormat);
OCQOCOC0Q0:=Encoding.Default.GetString(OOQOCOC0Q0.OQOQ0OOCQ0);
O0COCOC0Q0:=Encoding.Default.GetString(OOQOCOC0Q0.OQOQ0OOCQ0);
OOCOCOC0Q0:=OOQOCOC0Q0.OQOQ0OOCQ0;
O00QQOC0Q0:=OOQOCOC0Q0.OC0Q0OOCQ0;
if O00QQOC0Q0<>1 then
raise EScError.Create(seWrongDataFormat);
OOQOCOC0Q0.OQOQ0OOCQ0;
OQCOCOC0Q0:=OOQOCOC0Q0.OQOQ0OOCQ0;
finally
OOQOCOC0Q0.Free;
end;
if SameText(O0COCOC0Q0,'bcrypt')then begin
if O0QOCOC0Q0='' then
raise EScError.Create(sePasswordNotSpecified);
OOQOCOC0Q0:=OCCO0OOCQ0.Create(OOCOCOC0Q0);
try
OOOQQOC0Q0:=OOQOCOC0Q0.OQOQ0OOCQ0;
OO0QQOC0Q0:=OOQOCOC0Q0.OC0Q0OOCQ0;
finally
OOQOCOC0Q0.Free;
end;
O0OQQOC0Q0:=OOQ0000OQ0.OQ0OC00OQ0(O0QOCOC0Q0,OOOQQOC0Q0,OO0QQOC0Q0,32+16);
SetLength(OQOQQOC0Q0,16);
Move(O0OQQOC0Q0[32],OQOQQOC0Q0[0],16);
SetLength(O0OQQOC0Q0,32);
OC0QQOC0Q0:=OOCQCC0OQ0.OQCQCC0OQ0(OOCQCC0OQ0.OCO0CC0OQ0(OCQOCOC0Q0),O0OQQOC0Q0,OQOQQOC0Q0);
OC0QQOC0Q0.CreateDecryptor.TransformBlock(@OQCOCOC0Q0[0],0,Length(OQCOCOC0Q0),@OQCOCOC0Q0[0],0);
end
else
if not SameText(O0COCOC0Q0,'none')then
raise EScError.Create(seUnsupportedKeyEncryption);
OOQOCOC0Q0:=OCCO0OOCQ0.Create(OQCOCOC0Q0);
try
if OOQOCOC0Q0.OC0Q0OOCQ0<>OOQOCOC0Q0.OC0Q0OOCQ0 then
raise EScError.Create(seWrongDataFormat);
OQ0QQOC0Q0:=Encoding.Default.GetString(OOQOCOC0Q0.OQOQ0OOCQ0);
if SameText(OQ0QQOC0Q0,ED25519_TYPE_HEADER)then begin
OQCQOOC0Q0:=O0QQQOQOQ0;
OCCOCOC0Q0:=OOQOCOC0Q0.OQOQ0OOCQ0;
if Length(OCCOCOC0Q0)<>32 then
raise EScError.Create(seWrongDataFormat);
OC0OQ0C0Q0.O0QQQO0OQ0:=OCCCQOQOQ0;
OC0OQ0C0Q0.OOQQQO0OQ0:=OQ00000OQ0.Create;
OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OC00000OQ0:=OCCC000OQ0;
OC0OQ0C0Q0.OQQQQO0OQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCO0CO0OQ0(OCCOCOC0Q0,0,Length(OCCOCOC0Q0));
OCCOCOC0Q0:=OOQOCOC0Q0.OQOQ0OOCQ0;
if Length(OCCOCOC0Q0)<>64 then
raise EScError.Create(seWrongDataFormat);
OC0OQ0C0Q0.OCQQQO0OQ0:=T32BytesField.Create(OCCOCOC0Q0);
end
else
if SameText(OQ0QQOC0Q0,RSA_TYPE_HEADER)then begin
OQCQOOC0Q0:=OCOQQOQOQ0;
OQ0OQ0C0Q0.OCQ00O0OQ0:=OOQOCOC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.OQQ00O0OQ0:=OOQOCOC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.O0O00O0OQ0:=OOQOCOC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.OCO00O0OQ0:=OOQOCOC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.OOO00O0OQ0:=OOQOCOC0Q0.OOCQ0OOCQ0;
OQ0OQ0C0Q0.OQO00O0OQ0:=OOQOCOC0Q0.OOCQ0OOCQ0;
OQQQ0OC0Q0;
end;
O0QQ0CC0Q0:=True;
finally
OOQOCOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OCOQ00C0Q0(const O0QQ00C0Q0:TBytes;OOQQ00C0Q0:boolean);
var
OQQQ00C0Q0:OQCOCQOOQ0;
begin
OQQQ00C0Q0:=OQCOCQOOQ0.Create;
try
if OOQQ00C0Q0 then begin
if OQQQ00C0Q0.OCQCOOOOQ0(O0C0CCOOQ0,O0QQ00C0Q0)then
OQCQOOC0Q0:=OCOQQOQOQ0
else
if OQQQ00C0Q0.OCQCOOOOQ0(OOC0CCOOQ0,O0QQ00C0Q0)then
OQCQOOC0Q0:=OCOQQOQOQ0
else
if OQQQ00C0Q0.OCQCOOOOQ0(OQC0CCOOQ0,O0QQ00C0Q0)then
OQCQOOC0Q0:=OQOQQOQOQ0
else
Exit;
end
else begin
if OQQQ00C0Q0.OCQCOOOOQ0(OCC0CCOOQ0,O0QQ00C0Q0)then
OQCQOOC0Q0:=OCOQQOQOQ0
else
if OQQQ00C0Q0.OCQCOOOOQ0(O00OCCOOQ0,O0QQ00C0Q0)then
OQCQOOC0Q0:=OQOQQOQOQ0
else
Exit;
end;
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
if OOQQ00C0Q0 then begin
FreeAndNil(OQ0OQ0C0Q0.O0O00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OOO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OQO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OCO00O0OQ0);
end
else begin
OQ0OQ0C0Q0.O0O00O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['D'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OOO00O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['P'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OQO00O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['Q'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OCO00O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['U'].OCQQ0QOOQ0);
OQQQ0OC0Q0;
end;
OQ0OQ0C0Q0.OCQ00O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['PubMod'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OQQ00O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['PubExp'].OCQQ0QOOQ0);
end;
OQOQQOQOQ0:begin
OO0OQ0C0Q0.O0000O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['P'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OO000O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['Q'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OCCC0O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['G'].OCQQ0QOOQ0);
if OOQQ00C0Q0 then begin
FreeAndNil(OO0OQ0C0Q0.OQCC0O0OQ0);
OO0OQ0C0Q0.OQ000O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['Y'].OCQQ0QOOQ0);
end
else begin
OO0OQ0C0Q0.OQCC0O0OQ0:=TBigInteger.Create(OQQQ00C0Q0['X'].OCQQ0QOOQ0);
OOQQ0OC0Q0;
end;
end;
O0QQQOQOQ0:
raise EScError.Create(seWrongDataFormat);
else
Assert(False);
end;
O0QQ0CC0Q0:=True;
finally
OQQQ00C0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OCQQ00C0Q0(const O0CQ00C0Q0,OOCQ00C0Q0:TBytes);
var
OQCQ00C0Q0:OQCOCQOOQ0;
begin
OQCQ00C0Q0:=OQCOCQOOQ0.Create;
try
OQCQOOC0Q0:=OQOQQOQOQ0;
if not OQCQ00C0Q0.OCQCOOOOQ0(OO0OCCOOQ0,O0CQ00C0Q0)then
O0OCOOC0Q0;
OO0OQ0C0Q0.OQ000O0OQ0:=TBigInteger.Create(OQCQ00C0Q0['Y'].OCQQ0QOOQ0);
if not OQCQ00C0Q0.OCQCOOOOQ0(OQ0OCCOOQ0,OOCQ00C0Q0)then
O0OCOOC0Q0;
OO0OQ0C0Q0.O0000O0OQ0:=TBigInteger.Create(OQCQ00C0Q0['P'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OO000O0OQ0:=TBigInteger.Create(OQCQ00C0Q0['Q'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OCCC0O0OQ0:=TBigInteger.Create(OQCQ00C0Q0['G'].OCQQ0QOOQ0);
O0QQ0CC0Q0:=True;
finally
OQCQ00C0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OCCQ00C0Q0(const O00C00C0Q0,OO0C00C0Q0:TBytes);
begin
OQCQOOC0Q0:=O0QQQOQOQ0;
O0OOQOC0Q0(OO0C00C0Q0);
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
OC0OQ0C0Q0.OQQQQO0OQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCO0CO0OQ0(O00C00C0Q0,0,Length(O00C00C0Q0));
O0QQ0CC0Q0:=True;
end;
procedure O0CQOOC0Q0.OQ0C00C0Q0(const OC0C00C0Q0:TBytes;O0OC00C0Q0:OO00000OQ0);
begin
OQCQOOC0Q0:=O0QQQOQOQ0;
OC0OQ0C0Q0.O0QQQO0OQ0:=OCCCQOQOQ0;
OC0OQ0C0Q0.OOQQQO0OQ0:=OQ00000OQ0.Create;
OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OC00000OQ0:=O0OC00C0Q0;
OC0OQ0C0Q0.OQQQQO0OQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCO0CO0OQ0(OC0C00C0Q0,0,Length(OC0C00C0Q0));
O0QQ0CC0Q0:=True;
end;
procedure O0CQOOC0Q0.OCOQQOC0Q0(const O0QQQOC0Q0:TBytes);
var
OOQQQOC0Q0:OQCOCQOOQ0;
OQQQQOC0Q0:string;
OCQQQOC0Q0,O0CQQOC0Q0:TBytes;
OOCQQOC0Q0:boolean;
OQCQQOC0Q0:OO00000OQ0;
begin
OOQQQOC0Q0:=OQCOCQOOQ0.Create;
try
if not OOQQQOC0Q0.OCQCOOOOQ0(OC0OCCOOQ0,O0QQQOC0Q0)then
Exit;
OOCQQOC0Q0:=False;
OQCQQOC0Q0:=OQCC000OQ0;
OQQQQOC0Q0:=OOQQQOC0Q0['PrivateKeyAlgorithm']['Algorithm'].OCOQ0QOOQ0;
if OQQQQOC0Q0=OID_RSA_ENCRYPTION then
OQCQOOC0Q0:=OCOQQOQOQ0
else
if OQQQQOC0Q0=OID_DSA_ENCRYPTION then
OQCQOOC0Q0:=OQOQQOQOQ0
else
if OQQQQOC0Q0=OID_EC_PUBLIC_KEY then
OQCQOOC0Q0:=O0QQQOQOQ0
else
if OQQQQOC0Q0=OID_X25519 then begin
OQCQOOC0Q0:=O0QQQOQOQ0;
OOCQQOC0Q0:=True;
OQCQQOC0Q0:=OQCC000OQ0;
end
else
if OQQQQOC0Q0=OID_Ed25519 then begin
OQCQOOC0Q0:=O0QQQOQOQ0;
OOCQQOC0Q0:=True;
OQCQQOC0Q0:=OCCC000OQ0;
end
else
Exit;
SetLength(O0CQQOC0Q0,0);
SetLength(OCQQQOC0Q0,0);
OCQQQOC0Q0:=OOQQQOC0Q0['PrivateKeyAlgorithm']['Parameters'].O0OQ0QOOQ0;
O0CQQOC0Q0:=OOQQQOC0Q0['PrivateKey'].OQOQ0QOOQ0;
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
if not OOQQQOC0Q0.OCQCOOOOQ0(OOCQCCOOQ0,O0CQQOC0Q0)then
Exit;
OQ0OQ0C0Q0.OCQ00O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['PubMod'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OQQ00O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['PubExp'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.O0O00O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['D'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OOO00O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['P'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OQO00O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['Q'].OCQQ0QOOQ0);
OQ0OQ0C0Q0.OCO00O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['U'].OCQQ0QOOQ0);
OQQQ0OC0Q0;
end;
OQOQQOQOQ0:begin
if not OOQQQOC0Q0.OCQCOOOOQ0(OQ0OCCOOQ0,OCQQQOC0Q0)then
Exit;
OO0OQ0C0Q0.O0000O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['P'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OO000O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['Q'].OCQQ0QOOQ0);
OO0OQ0C0Q0.OCCC0O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['G'].OCQQ0QOOQ0);
if not OOQQQOC0Q0.OCQCOOOOQ0(O0OOCCOOQ0,O0CQQOC0Q0)then
Exit;
OO0OQ0C0Q0.OQCC0O0OQ0:=TBigInteger.Create(OOQQQOC0Q0['X'].OCQQ0QOOQ0);
OOQQ0OC0Q0;
end;
O0QQQOQOQ0:begin
if OOCQQOC0Q0 then
OCQ0QOC0Q0(O0CQQOC0Q0,OQCQQOC0Q0)
else
OOO0QOC0Q0(O0CQQOC0Q0,OCQQQOC0Q0);
if not O0QQ0CC0Q0 then
Exit;
end;
else
Assert(False);
end;
O0QQ0CC0Q0:=True;
finally
OOQQQOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OCCQQOC0Q0(const O00CQOC0Q0:TBytes;const OO0CQOC0Q0:string);
var
OQ0CQOC0Q0:OQCOCQOOQ0;
OC0CQOC0Q0:string;
O0OCQOC0Q0:TBytes;
OOOCQOC0Q0:TBytes;
OQOCQOC0Q0:TSymmetricAlgorithm;
begin
SetLength(O0OCQOC0Q0,0);
SetLength(OOOCQOC0Q0,0);
OQ0CQOC0Q0:=OQCOCQOOQ0.Create;
try
if not OQ0CQOC0Q0.OCQCOOOOQ0(OOOOCCOOQ0,O00CQOC0Q0)then
Exit;
OC0CQOC0Q0:=OQ0CQOC0Q0['EncryptionAlgorithm']['Algorithm'].OCOQ0QOOQ0;
OOOCQOC0Q0:=OQ0CQOC0Q0['EncryptedData'].OQOQ0QOOQ0;
O0OCQOC0Q0:=OQ0CQOC0Q0['EncryptionAlgorithm']['Parameters'].O0OQ0QOOQ0;
OQOCQOC0Q0:=OOQ0000OQ0.OO0QQ00OQ0(OC0CQOC0Q0,O0OCQOC0Q0,OO0CQOC0Q0);
if OQOCQOC0Q0=nil then
Exit;
OQOCQOC0Q0.CreateDecryptor.TransformBlock(OOOCQOC0Q0,0,Length(OOOCQOC0Q0));
OCOQQOC0Q0(OOOCQOC0Q0);
finally
OQ0CQOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OCOCQOC0Q0(const O0QCQOC0Q0:TBytes;const OOQCQOC0Q0:string);
var
OQQCQOC0Q0:OCCO0OOCQ0;
OCQCQOC0Q0,O0CCQOC0Q0:TBytes;
OOCCQOC0Q0:string;
OQCCQOC0Q0:integer;
OCCCQOC0Q0:TSymmetricAlgorithm;
O000QOC0Q0:OO00COQOQ0;
OO00QOC0Q0,OQ00QOC0Q0:TBytes;
begin
OQQCQOC0Q0:=OCCO0OOCQ0.Create(O0QCQOC0Q0);
try
OQCCQOC0Q0:=OQQCQOC0Q0.OC0Q0OOCQ0;
if OQCCQOC0Q0<>IETF_MAGIC_VAL then
Exit;
OQQCQOC0Q0.OC0Q0OOCQ0;
SetLength(O0CCQOC0Q0,0);
SetLength(OCQCQOC0Q0,0);
OCQCQOC0Q0:=OQQCQOC0Q0.OQOQ0OOCQ0;
OOCCQOC0Q0:=Encoding.Default.GetString(OCQCQOC0Q0);
if OOCCQOC0Q0=IETF_RSA then
OQCQOOC0Q0:=OCOQQOQOQ0
else
if OOCCQOC0Q0=IETF_DSA then
OQCQOOC0Q0:=OQOQQOQOQ0
else
Exit;
OCQCQOC0Q0:=OQQCQOC0Q0.OQOQ0OOCQ0;
OOCCQOC0Q0:=Encoding.Default.GetString(OCQCQOC0Q0);
O000QOC0Q0:=OOCQCC0OQ0.OCO0CC0OQ0(STripleDES_cbc);
if OOCCQOC0Q0=STripleDES_cbc then begin
SetLength(OQ00QOC0Q0,OOCQCC0OQ0.OO00CC0OQ0(O000QOC0Q0));
FillChar(OQ00QOC0Q0[0],Length(OQ00QOC0Q0),0);
SetLength(OO00QOC0Q0,0);
OO00QOC0Q0:=OOQ0000OQ0.OQOO000OQ0(OQ0OCOQOQ0,OOQCQOC0Q0,nil,OOCQCC0OQ0.OCCCCC0OQ0(O000QOC0Q0),1,True);
OCCCQOC0Q0:=OOCQCC0OQ0.OQCQCC0OQ0(O000QOC0Q0,OO00QOC0Q0,OQ00QOC0Q0);
FillChar(OO00QOC0Q0[0],Length(OO00QOC0Q0),0);
O0CCQOC0Q0:=OQQCQOC0Q0.OQOQ0OOCQ0;
OCCCQOC0Q0.CreateDecryptor.TransformBlock(O0CCQOC0Q0,0,Length(O0CCQOC0Q0));
end
else
if OOCCQOC0Q0=SNone then
O0CCQOC0Q0:=OQQCQOC0Q0.OQOQ0OOCQ0
else
Exit;
finally
OQQCQOC0Q0.Free;
end;
OQQCQOC0Q0:=OCCO0OOCQ0.Create(O0CCQOC0Q0);
try
OQQCQOC0Q0.OC0Q0OOCQ0;
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
OQ0OQ0C0Q0.OQQ00O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OQ0OQ0C0Q0.O0O00O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OQ0OQ0C0Q0.OCQ00O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OQ0OQ0C0Q0.OCO00O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OQ0OQ0C0Q0.OQO00O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OQ0OQ0C0Q0.OOO00O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OQQQ0OC0Q0;
end;
OQOQQOQOQ0:begin
OQQCQOC0Q0.OC0Q0OOCQ0;
OO0OQ0C0Q0.O0000O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OO0OQ0C0Q0.OCCC0O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OO0OQ0C0Q0.OO000O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OO0OQ0C0Q0.OQ000O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
OO0OQ0C0Q0.OQCC0O0OQ0:=OQQCQOC0Q0.O00QCOOCQ0;
end;
O0QQQOQOQ0:
raise EScError.Create(seWrongDataFormat);
else
Assert(False);
end;
finally
OQQCQOC0Q0.Free;
end;
O0QQ0CC0Q0:=True;
end;
procedure O0CQOOC0Q0.OC00QOC0Q0(const O0O0QOC0Q0:TBytes);
begin
O00OCOC0Q0(O0O0QOC0Q0);
end;
procedure O0CQOOC0Q0.OOO0QOC0Q0(const OQO0QOC0Q0,OCO0QOC0Q0:TBytes);
var
O0Q0QOC0Q0:OQCOCQOOQ0;
OOQ0QOC0Q0:OC0QOQOOQ0;
OQQ0QOC0Q0:string;
begin
O0Q0QOC0Q0:=OQCOCQOOQ0.Create;
try
OQCQOOC0Q0:=O0QQQOQOQ0;
if not O0Q0QOC0Q0.OCQCOOOOQ0(OQ0CCCOOQ0,OQO0QOC0Q0)then
Exit;
OOQ0QOC0Q0:=O0Q0QOC0Q0['Params'].OCCOOQOOQ0;
if not OOQ0QOC0Q0.OOOQ0QOOQ0 then
O0OOQOC0Q0(OOQ0QOC0Q0)
else
if Length(OCO0QOC0Q0)>0 then
O0OOQOC0Q0(OCO0QOC0Q0)
else
raise EScError.Create(seUnknownECParametersFormat);
if not(OC0OQ0C0Q0.OOQQQO0OQ0 is O0CQQO0OQ0)then
raise EScError.Create(seUnknownECParametersFormat);
OQQ0QOC0Q0:=O00QOCOOQ0(O0Q0QOC0Q0['PrivateKey'].OQOQ0QOOQ0);
OC0OQ0C0Q0.OCQQQO0OQ0:=TBigInteger.Create(OQQ0QOC0Q0,16);
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
OC0OQ0C0Q0.OQQQQO0OQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.O0O0CO0OQ0(O0CQQO0OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OO0CQO0OQ0,OC0OQ0C0Q0.OCQQQO0OQ0);
O0QQ0CC0Q0:=True;
finally
O0Q0QOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OCQ0QOC0Q0(const O0C0QOC0Q0:TBytes;OOC0QOC0Q0:OO00000OQ0);
var
OQC0QOC0Q0:OQCOCQOOQ0;
OCC0QOC0Q0:TBytes;
begin
OQC0QOC0Q0:=OQCOCQOOQ0.Create;
try
OQCQOOC0Q0:=O0QQQOQOQ0;
if not OQC0QOC0Q0.OCQCOOOOQ0(OO0CCCOOQ0,O0C0QOC0Q0)then
Exit;
SetLength(OCC0QOC0Q0,0);
OCC0QOC0Q0:=OQC0QOC0Q0.O0O0OOOOQ0.OQOQ0QOOQ0;
if Length(OCC0QOC0Q0)<>32 then
Exit;
OC0OQ0C0Q0.O0QQQO0OQ0:=OCCCQOQOQ0;
OC0OQ0C0Q0.OOQQQO0OQ0:=OQ00000OQ0.Create;
OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OC00000OQ0:=OOC0QOC0Q0;
OC0OQ0C0Q0.OCQQQO0OQ0:=T32BytesField.Create(OCC0QOC0Q0);
OC0OQ0C0Q0.OQQQQO0OQ0:=OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).O0O0000OQ0(T32BytesField(OC0OQ0C0Q0.OCQQQO0OQ0));
O0QQ0CC0Q0:=True;
finally
OQC0QOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.O00OQOC0Q0(const OO0OQOC0Q0:TBytes;OQ0OQOC0Q0:OO00000OQ0);
var
OC0OQOC0Q0:OQCOCQOOQ0;
begin
OC0OQ0C0Q0.O0QQQO0OQ0:=OCCCQOQOQ0;
OC0OQ0C0Q0.OOQQQO0OQ0:=OQ00000OQ0.Create;
OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OC00000OQ0:=OQ0OQOC0Q0;
if Length(OO0OQOC0Q0)=0 then
Exit;
OC0OQOC0Q0:=OQCOCQOOQ0.Create;
try
if not OC0OQOC0Q0.OCQCOOOOQ0(OQQQCCOOQ0,OO0OQOC0Q0)then
raise EScError.Create(seUnknownECParametersFormat);
case OC0OQOC0Q0['Parameters'].OQQQ0QOOQ0 of
1:
OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OC00000OQ0:=OQCC000OQ0;
2:
OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OC00000OQ0:=OCCC000OQ0;
else
raise EScError.Create(seUnknownECParametersFormat);
end;
finally
OC0OQOC0Q0.Free;
end;
end;
function O0CQOOC0Q0.OC0QO0C0Q0:TBytes;
var
O0OQO0C0Q0:OQCOCQOOQ0;
begin
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
O0OQO0C0Q0:=OQCOCQOOQ0.Create;
try
if not O0OQO0C0Q0.OCQCOOOOQ0(OQQQCCOOQ0)then
raise EScError.Create(seUnsupportedECParameters);
case OQ00000OQ0(OC0OQ0C0Q0.OOQQQO0OQ0).OC00000OQ0 of
OQCC000OQ0:
O0OQO0C0Q0['Parameters'].OQQQ0QOOQ0:=1;
OCCC000OQ0:
O0OQO0C0Q0['Parameters'].OQQQ0QOOQ0:=2;
else
raise EScError.Create(seUnsupportedECParameters);
end;
Result:=O0OQO0C0Q0.OOQCOOOOQ0;
finally
O0OQO0C0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.O0OOQOC0Q0(const OOOOQOC0Q0:TBytes);
var
OQOOQOC0Q0:OQCOCQOOQ0;
begin
OQOOQOC0Q0:=OQCOCQOOQ0.Create;
try
if not OQOOQOC0Q0.OCQCOOOOQ0(OCQQCCOOQ0,OOOOQOC0Q0)then
raise EScError.Create(seUnknownECParametersFormat);
O0OOQOC0Q0(OQOOQOC0Q0.O0O0OOOOQ0.OCCOOQOOQ0);
finally
OQOOQOC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.O0OOQOC0Q0(OCOOQOC0Q0:OC0QOQOOQ0);
var
O0QOQOC0Q0,OOQOQOC0Q0:OC0QOQOOQ0;
OQQOQOC0Q0,OCQOQOC0Q0:OC0QOQOOQ0;
O0COQOC0Q0:O0CQQO0OQ0;
OOCOQOC0Q0:TBytes;
OQCOQOC0Q0:string;
OCCOQOC0Q0,O00QO0C0Q0,OO0QO0C0Q0,OQ0QO0C0Q0:integer;
begin
Assert(OC0OQ0C0Q0.OOQQQO0OQ0=nil);
if SameText(OCOOQOC0Q0.O00C0QOOQ0,'Specified')then begin
O0QOQOC0Q0:=OCOOQOC0Q0['FieldID'];
OQQOQOC0Q0:=O0QOQOC0Q0['Params'];
if O0QOQOC0Q0['FieldType'].O0CQ0QOOQ0=OID_PRIME_FIELD then begin
if not SameText(OQQOQOC0Q0.O00C0QOOQ0,'PrimeP')then
raise EScError.Create(seUnknownECParametersFormat);
O0COQOC0Q0:=O0QCO00OQ0.Create;
OC0OQ0C0Q0.OOQQQO0OQ0:=O0COQOC0Q0;
OQCOQOC0Q0:=O00QOCOOQ0(OQQOQOC0Q0.OQOQ0QOOQ0);
O0COQOC0Q0.OOCQQO0OQ0:=TBigInteger.Create(OQCOQOC0Q0,16);
end
else
if O0QOQOC0Q0['FieldType'].O0CQ0QOOQ0=OID_CHARACTERISTIC_TWO_FIELD then begin
if not SameText(OQQOQOC0Q0.O00C0QOOQ0,'CharacteristicTwo')then
raise EScError.Create(seUnknownECParametersFormat);
O0COQOC0Q0:=OQOOO00OQ0.Create;
OC0OQ0C0Q0.OOQQQO0OQ0:=O0COQOC0Q0;
OCCOQOC0Q0:=OQQOQOC0Q0['M'].OQQQ0QOOQ0;
O0COQOC0Q0.OOCQQO0OQ0:=TBigInteger.Create(1);
O0COQOC0Q0.OOCQQO0OQ0.SetBit(OCCOQOC0Q0);
OCQOQOC0Q0:=OQQOQOC0Q0['Params'].OCCOOQOOQ0;
if SameText(OCQOQOC0Q0.O00C0QOOQ0,'Trinomial')then begin
O00QO0C0Q0:=OCQOQOC0Q0.OQQQ0QOOQ0;
O0COQOC0Q0.OOCQQO0OQ0.SetBit(O00QO0C0Q0);
end
else
if SameText(OCQOQOC0Q0.O00C0QOOQ0,'Pentanomial')then begin
O00QO0C0Q0:=OCQOQOC0Q0['k1'].OQQQ0QOOQ0;
O0COQOC0Q0.OOCQQO0OQ0.SetBit(O00QO0C0Q0);
OO0QO0C0Q0:=OCQOQOC0Q0['k2'].OQQQ0QOOQ0;
O0COQOC0Q0.OOCQQO0OQ0.SetBit(OO0QO0C0Q0);
OQ0QO0C0Q0:=OCQOQOC0Q0['k3'].OQQQ0QOOQ0;
O0COQOC0Q0.OOCQQO0OQ0.SetBit(OQ0QO0C0Q0);
end
else
raise EScError.Create(seUnknownECParametersFormat);
end
else
raise EScError.Create(seUnknownECParametersFormat);
O0COQOC0Q0.OC0CCO0OQ0:=O0COQOC0Q0.OOCQQO0OQ0.BitCount shr 3;
if(O0COQOC0Q0.OOCQQO0OQ0.BitCount and 7)>0 then
Inc(O0COQOC0Q0.OC0CCO0OQ0);
OOQOQOC0Q0:=OCOOQOC0Q0['Curve'];
OQCOQOC0Q0:=O00QOCOOQ0(OOQOQOC0Q0['A'].OQOQ0QOOQ0);
O0COQOC0Q0.OQCQQO0OQ0:=TBigInteger.Create(OQCOQOC0Q0,16);
OQCOQOC0Q0:=O00QOCOOQ0(OOQOQOC0Q0['B'].OQOQ0QOOQ0);
O0COQOC0Q0.OCCQQO0OQ0:=TBigInteger.Create(OQCOQOC0Q0,16);
O0COQOC0Q0.OQ0CQO0OQ0:=OOQOQOC0Q0['Seed'].OQOQ0QOOQ0;
OQCOQOC0Q0:=O00QOCOOQ0(OCOOQOC0Q0['Order'].OQOQ0QOOQ0);
O0COQOC0Q0.O00CQO0OQ0:=TBigInteger.Create(OQCOQOC0Q0,16);
O0COQOC0Q0.OC0CQO0OQ0:=OCOOQOC0Q0['Cofactor'].OQQQ0QOOQ0;
O0COQOC0Q0.OQO0QO0OQ0;
SetLength(OOCOQOC0Q0,0);
OOCOQOC0Q0:=OCOOQOC0Q0['Base'].OQOQ0QOOQ0;
O0COQOC0Q0.OO0CQO0OQ0:=O00QQO0OQ0(O0COQOC0Q0.OCO0CO0OQ0(OOCOQOC0Q0,0,Length(OOCOQOC0Q0)));
end
else
if SameText(OCOOQOC0Q0.O00C0QOOQ0,'Named')then begin
OC0OQ0C0Q0.O0QQQO0OQ0:=OC0OO0Q0Q0.OOQQ00Q0Q0(OCOOQOC0Q0.O0CQ0QOOQ0);
OC0OQ0C0Q0.OOQQQO0OQ0:=OQ0C00Q0Q0[OC0OQ0C0Q0.O0QQQO0OQ0].CryptographyClass.Create(OQ0C00Q0Q0[OC0OQ0C0Q0.O0QQQO0OQ0]);
end
else
raise EScError.Create(seUnknownECParametersFormat);
end;
function O0CQOOC0Q0.OOOQO0C0Q0:TBytes;
var
OQOQO0C0Q0:OQCOCQOOQ0;
OCOQO0C0Q0,O0QQO0C0Q0,OOQQO0C0Q0:OC0QOQOOQ0;
OQQQO0C0Q0,OCQQO0C0Q0:OC0QOQOOQ0;
O0CQO0C0Q0:O0CQQO0OQ0;
OOCQO0C0Q0:TIntArr;
OQCQO0C0Q0:string;
begin
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
OQOQO0C0Q0:=OQCOCQOOQ0.Create;
try
if not OQOQO0C0Q0.OCQCOOOOQ0(OCQQCCOOQ0)then
raise EScError.Create(seUnknownECParametersFormat);
if not OC0OQ0C0Q0.OOQQQO0OQ0.OQ0CCO0OQ0 then begin
O0CQO0C0Q0:=O0CQQO0OQ0(OC0OQ0C0Q0.OOQQQO0OQ0);
OQOQO0C0Q0.O0O0OOOOQ0.O00Q0QOOQ0('Specified');
OCOQO0C0Q0:=OQOQO0C0Q0['Specified'];
OCOQO0C0Q0['Version'].OQQQ0QOOQ0:=1;
O0QQO0C0Q0:=OCOQO0C0Q0['FieldID'];
if IsClass(OC0OQ0C0Q0.OOQQQO0OQ0,O0QCO00OQ0)then begin
O0QQO0C0Q0['FieldType'].O0CQ0QOOQ0:=OID_PRIME_FIELD;
O0QQO0C0Q0['Params'].O00Q0QOOQ0('PrimeP');
O0QQO0C0Q0['PrimeP'].OQOQ0QOOQ0:=O0CQO0C0Q0.OOOCCO0OQ0(O0CQO0C0Q0.OOCQQO0OQ0);
end
else
if IsClass(OC0OQ0C0Q0.OOQQQO0OQ0,OQOOO00OQ0)then begin
O0QQO0C0Q0['FieldType'].O0CQ0QOOQ0:=OID_CHARACTERISTIC_TWO_FIELD;
O0QQO0C0Q0['Params'].O00Q0QOOQ0('CharacteristicTwo');
OQQQO0C0Q0:=O0QQO0C0Q0['Params'];
SetLength(OOCQO0C0Q0,0);
OOCQO0C0Q0:=O0CQO0C0Q0.OOCQQO0OQ0.GetSetBitsArray;
OQQQO0C0Q0['M'].OQQQ0QOOQ0:=OOCQO0C0Q0[0];
if Length(OOCQO0C0Q0)=3 then begin
OQQQO0C0Q0['Basis'].O0CQ0QOOQ0:=OID_CHARACTERISTIC_TWO_BASIS_2;
OQQQO0C0Q0['Params'].O00Q0QOOQ0('Trinomial');
OCQQO0C0Q0:=OQQQO0C0Q0['Trinomial'];
OCQQO0C0Q0.OQQQ0QOOQ0:=OOCQO0C0Q0[1];
end
else
if Length(OOCQO0C0Q0)=5 then begin
OQQQO0C0Q0['Basis'].O0CQ0QOOQ0:=OID_CHARACTERISTIC_TWO_BASIS_3;
OQQQO0C0Q0['Params'].O00Q0QOOQ0('Pentanomial');
OCQQO0C0Q0:=OQQQO0C0Q0['Pentanomial'];
OCQQO0C0Q0['k1'].OQQQ0QOOQ0:=OOCQO0C0Q0[3];
OCQQO0C0Q0['k2'].OQQQ0QOOQ0:=OOCQO0C0Q0[2];
OCQQO0C0Q0['k3'].OQQQ0QOOQ0:=OOCQO0C0Q0[1];
end
else
raise EScError.Create(seUnsupportedECParameters);
end
else
if IsClass(OC0OQ0C0Q0.OOQQQO0OQ0,OQ00000OQ0)then begin
raise EScError.Create(seUnsupportedECParameters);
end
else
raise EScError.Create(seUnsupportedECParameters);
OOQQO0C0Q0:=OCOQO0C0Q0['Curve'];
OQCQO0C0Q0:=O0CQO0C0Q0.OQCQQO0OQ0.ToString(16);
OOQQO0C0Q0['A'].OQOQ0QOOQ0:=OOOQOCOOQ0(OQCQO0C0Q0);
OQCQO0C0Q0:=O0CQO0C0Q0.OCCQQO0OQ0.ToString(16);
OOQQO0C0Q0['B'].OQOQ0QOOQ0:=OOOQOCOOQ0(OQCQO0C0Q0);
if Length(O0CQO0C0Q0.OQ0CQO0OQ0)>0 then
OOQQO0C0Q0['Seed'].OQOQ0QOOQ0:=O0CQO0C0Q0.OQ0CQO0OQ0;
OQCQO0C0Q0:=O0CQO0C0Q0.O00CQO0OQ0.ToString(16);
OCOQO0C0Q0['Order'].OQOQ0QOOQ0:=OOOQOCOOQ0(OQCQO0C0Q0);
OCOQO0C0Q0['Cofactor'].OQQQ0QOOQ0:=O0CQO0C0Q0.OC0CQO0OQ0;
OCOQO0C0Q0['Base'].OQOQ0QOOQ0:=O0CQO0C0Q0.OCQ0CO0OQ0(O0CQO0C0Q0.OO0CQO0OQ0);
end
else begin
OQOQO0C0Q0.O0O0OOOOQ0.O00Q0QOOQ0('Named');
OQOQO0C0Q0['Named'].O0CQ0QOOQ0:=OC0OO0Q0Q0.OCQQ00Q0Q0(OC0OQ0C0Q0.O0QQQO0OQ0);
end;
Result:=OQOQO0C0Q0.OOQCOOOOQ0;
finally
OQOQO0C0Q0.Free;
end;
end;
function O0CQOOC0Q0.OCCQO0C0Q0(O00CO0C0Q0:boolean):TBytes;
var
OO0CO0C0Q0:OQCOCQOOQ0;
begin
Assert(O0QQ0CC0Q0=True);
OO0CO0C0Q0:=OQCOCQOOQ0.Create;
try
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
if O00CO0C0Q0 then begin
if not OO0CO0C0Q0.OCQCOOOOQ0(O0CQCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
end
else begin
if not OO0CO0C0Q0.OCQCOOOOQ0(OOCQCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OO0CO0C0Q0['Version'].OQQQ0QOOQ0:=0;
OO0CO0C0Q0['D'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.O0O00O0OQ0.GetBytes;
OO0CO0C0Q0['P'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OOO00O0OQ0.GetBytes;
OO0CO0C0Q0['Q'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OQO00O0OQ0.GetBytes;
OO0CO0C0Q0['U'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OCO00O0OQ0.GetBytes;
OO0CO0C0Q0['DP'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.O0Q00O0OQ0.GetBytes;
OO0CO0C0Q0['DQ'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OOQ00O0OQ0.GetBytes;
end;
OO0CO0C0Q0['PubMod'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OCQ00O0OQ0.GetBytes;
OO0CO0C0Q0['PubExp'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OQQ00O0OQ0.GetBytes;
end;
OQOQQOQOQ0:begin
if O00CO0C0Q0 then begin
if not OO0CO0C0Q0.OCQCOOOOQ0(OQCQCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
end
else begin
if not OO0CO0C0Q0.OCQCOOOOQ0(OCCQCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OO0CO0C0Q0['Version'].OQQQ0QOOQ0:=0;
OO0CO0C0Q0['X'].OCQQ0QOOQ0:=OO0OQ0C0Q0.OQCC0O0OQ0.GetBytes;
end;
OO0CO0C0Q0['P'].OCQQ0QOOQ0:=OO0OQ0C0Q0.O0000O0OQ0.GetBytes;
OO0CO0C0Q0['Q'].OCQQ0QOOQ0:=OO0OQ0C0Q0.OO000O0OQ0.GetBytes;
OO0CO0C0Q0['G'].OCQQ0QOOQ0:=OO0OQ0C0Q0.OCCC0O0OQ0.GetBytes;
OO0CO0C0Q0['Y'].OCQQ0QOOQ0:=OO0OQ0C0Q0.OQ000O0OQ0.GetBytes;
end;
O0QQQOQOQ0:begin
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
if O00CO0C0Q0 then begin
if not OO0CO0C0Q0.OCQCOOOOQ0(O00CCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
if IsClass(OC0OQ0C0Q0.OOQQQO0OQ0,OQ00000OQ0)then begin
OO0CO0C0Q0['Algorithm']['Algorithm'].O0CQ0QOOQ0:=OID_X25519_PUBLICKEY;
OO0CO0C0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0:=OC0QO0C0Q0;
OO0CO0C0Q0['SubjectPublicKey'].OQOQ0QOOQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCQ0CO0OQ0(OC0OQ0C0Q0.OQQQQO0OQ0);
end
else begin
OO0CO0C0Q0['Algorithm']['Algorithm'].O0CQ0QOOQ0:=OID_EC_PUBLIC_KEY;
OO0CO0C0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0:=OOOQO0C0Q0;
OO0CO0C0Q0['SubjectPublicKey'].OQOQ0QOOQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCQ0CO0OQ0(OC0OQ0C0Q0.OQQQQO0OQ0);
end;
end
else begin
if IsClass(OC0OQ0C0Q0.OOQQQO0OQ0,OQ00000OQ0)then begin
if not OO0CO0C0Q0.OCQCOOOOQ0(OO0CCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OO0CO0C0Q0.O0O0OOOOQ0.OQOQ0QOOQ0:=T32BytesField(OC0OQ0C0Q0.OCQQQO0OQ0).GetBytes;
end
else begin
if not OO0CO0C0Q0.OCQCOOOOQ0(OQ0CCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OO0CO0C0Q0['Version'].OQQQ0QOOQ0:=1;
OO0CO0C0Q0['PrivateKey'].OQOQ0QOOQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OOOCCO0OQ0(OC0OQ0C0Q0.OCQQQO0OQ0 as TBigInteger);
OO0CO0C0Q0['Params'].O0OQ0QOOQ0:=OOOQO0C0Q0;
OO0CO0C0Q0['PublicKey'].OQOQ0QOOQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCQ0CO0OQ0(OC0OQ0C0Q0.OQQQQO0OQ0);
end;
end;
end;
else
Assert(False);
end;
Result:=OO0CO0C0Q0.OOQCOOOOQ0;
finally
OO0CO0C0Q0.Free;
end;
end;
function O0CQOOC0Q0.O0OCO0C0Q0:TBytes;
var
OOOCO0C0Q0:OQCOCQOOQ0;
OQOCO0C0Q0:OC0QOQOOQ0;
begin
Assert(O0QQ0CC0Q0=True);
OOOCO0C0Q0:=OQCOCQOOQ0.Create;
try
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
if not OOOCO0C0Q0.OCQCOOOOQ0(OC0CCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OOOCO0C0Q0['Version'].OQQQ0QOOQ0:=0;
OOOCO0C0Q0['PrivateKey']['Version'].OQQQ0QOOQ0:=0;
OOOCO0C0Q0['PubMod'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OCQ00O0OQ0.GetBytes;
OOOCO0C0Q0['PubExp'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OQQ00O0OQ0.GetBytes;
OOOCO0C0Q0['D'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.O0O00O0OQ0.GetBytes;
OOOCO0C0Q0['P'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OOO00O0OQ0.GetBytes;
OOOCO0C0Q0['Q'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OQO00O0OQ0.GetBytes;
OOOCO0C0Q0['U'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OCO00O0OQ0.GetBytes;
OOOCO0C0Q0['DP'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.O0Q00O0OQ0.GetBytes;
OOOCO0C0Q0['DQ'].OCQQ0QOOQ0:=OQ0OQ0C0Q0.OOQ00O0OQ0.GetBytes;
end;
OQOQQOQOQ0:begin
if not OOOCO0C0Q0.OCQCOOOOQ0(O0OCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OOOCO0C0Q0['Version'].OQQQ0QOOQ0:=0;
OOOCO0C0Q0['P'].OCQQ0QOOQ0:=OO0OQ0C0Q0.O0000O0OQ0.GetBytes;
OOOCO0C0Q0['Q'].OCQQ0QOOQ0:=OO0OQ0C0Q0.OO000O0OQ0.GetBytes;
OOOCO0C0Q0['G'].OCQQ0QOOQ0:=OO0OQ0C0Q0.OCCC0O0OQ0.GetBytes;
OOOCO0C0Q0['X'].OCQQ0QOOQ0:=OO0OQ0C0Q0.OQCC0O0OQ0.GetBytes;
end;
O0QQQOQOQ0:begin
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
if IsClass(OC0OQ0C0Q0.OOQQQO0OQ0,OQ00000OQ0)then begin
if not OOOCO0C0Q0.OCQCOOOOQ0(OOOCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OOOCO0C0Q0['Version'].OQQQ0QOOQ0:=0;
OOOCO0C0Q0['PrivateKeyPoint'].OQOQ0QOOQ0:=T32BytesField(OC0OQ0C0Q0.OCQQQO0OQ0).GetBytes;
end
else begin
if not OOOCO0C0Q0.OCQCOOOOQ0(OQOCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OOOCO0C0Q0['Version'].OQQQ0QOOQ0:=0;
OOOCO0C0Q0['PrivateKeyAlgorithm']['Parameters'].O0OQ0QOOQ0:=OOOQO0C0Q0;
OQOCO0C0Q0:=OOOCO0C0Q0['PrivateKey'];
OQOCO0C0Q0['Version'].OQQQ0QOOQ0:=1;
OQOCO0C0Q0['PrivateKey'].OQOQ0QOOQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OOOCCO0OQ0(OC0OQ0C0Q0.OCQQQO0OQ0 as TBigInteger);
OQOCO0C0Q0['PublicKey'].OQOQ0QOOQ0:=OC0OQ0C0Q0.OOQQQO0OQ0.OCQ0CO0OQ0(OC0OQ0C0Q0.OQQQQO0OQ0);
end;
end;
else
Assert(False);
end;
Result:=OOOCO0C0Q0.OOQCOOOOQ0;
finally
OOOCO0C0Q0.Free;
end;
end;
function O0CQOOC0Q0.OCOCO0C0Q0(const O0QCO0C0Q0:string;OOQCO0C0Q0:OO00COQOQ0):TBytes;
var
OQQCO0C0Q0:OQCOCQOOQ0;
OCQCO0C0Q0:TBytes;
O0CCO0C0Q0:TBytes;
OOCCO0C0Q0,OQCCO0C0Q0,OCCCO0C0Q0:integer;
O000O0C0Q0,OO00O0C0Q0:TBytes;
OQ00O0C0Q0:Word;
OC00O0C0Q0:string;
O0O0O0C0Q0:TSymmetricAlgorithm;
begin
OCQCO0C0Q0:=O0OCO0C0Q0;
OC00O0C0Q0:=OOCQCC0OQ0.OOQCQC0OQ0(OOQCO0C0Q0);
OOCCO0C0Q0:=Length(OCQCO0C0Q0);
OCCCO0C0Q0:=OOCQCC0OQ0.OO00CC0OQ0(OOQCO0C0Q0);
OQCCO0C0Q0:=OOCCO0C0Q0 mod OCCCO0C0Q0;
if OQCCO0C0Q0>0 then begin
OQCCO0C0Q0:=OCCCO0C0Q0-OQCCO0C0Q0;
if OQCCO0C0Q0>0 then begin
SetLength(OCQCO0C0Q0,OOCCO0C0Q0+OQCCO0C0Q0);
System.FillChar(OCQCO0C0Q0[OOCCO0C0Q0],OQCCO0C0Q0,OQCCO0C0Q0);
end;
end;
if OCCQ0QQ0Q0=nil then
raise Exception.Create(SInternalError);
OQ00O0C0Q0:=2048;
SetLength(O000O0C0Q0,OCCCO0C0Q0);
OCCQ0QQ0Q0.Random(O000O0C0Q0,0,Length(O000O0C0Q0));
SetLength(OO00O0C0Q0,OCCCO0C0Q0);
OCCQ0QQ0Q0.Random(OO00O0C0Q0,0,Length(OO00O0C0Q0));
O0CCO0C0Q0:=OOQ0000OQ0.OQOQC00OQ0(O0QCO0C0Q0,O000O0C0Q0,OOCQCC0OQ0.OCCCCC0OQ0(OOQCO0C0Q0),OQ00O0C0Q0);
O0O0O0C0Q0:=OOCQCC0OQ0.OQCQCC0OQ0(OOQCO0C0Q0,O0CCO0C0Q0,OO00O0C0Q0);
FillChar(O0CCO0C0Q0[0],Length(O0CCO0C0Q0),0);
O0O0O0C0Q0.CreateEncryptor.TransformBlock(OCQCO0C0Q0,0,Length(OCQCO0C0Q0));
OQQCO0C0Q0:=OQCOCQOOQ0.Create;
try
if not OQQCO0C0Q0.OCQCOOOOQ0(OCOCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OQQCO0C0Q0['Algo'].O0CQ0QOOQ0:=OC00O0C0Q0;
OQQCO0C0Q0['Iters'].OQQQ0QOOQ0:=OQ00O0C0Q0;
OQQCO0C0Q0['Salt'].OQOQ0QOOQ0:=O000O0C0Q0;
OQQCO0C0Q0['IV'].OQOQ0QOOQ0:=OO00O0C0Q0;
OQQCO0C0Q0['EncryptedData'].OQOQ0QOOQ0:=OCQCO0C0Q0;
Result:=OQQCO0C0Q0.OOQCOOOOQ0;
finally
OQQCO0C0Q0.Free;
end;
end;
function O0CQOOC0Q0.OOO0O0C0Q0(const OQO0O0C0Q0:string):TBytes;
var
OCO0O0C0Q0:OC0QCOOCQ0;
O0Q0O0C0Q0:integer;
OOQ0O0C0Q0:TBytes;
OQQ0O0C0Q0:TSymmetricAlgorithm;
OCQ0O0C0Q0:OO00COQOQ0;
O0C0O0C0Q0,OOC0O0C0Q0,OQC0O0C0Q0:TBytes;
begin
OCO0O0C0Q0:=OC0QCOOCQ0.Create;
try
OCO0O0C0Q0.OCQ00OOCQ0(0);
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
OCO0O0C0Q0.O0OQCOOCQ0(OQ0OQ0C0Q0.OQQ00O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OQ0OQ0C0Q0.O0O00O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OQ0OQ0C0Q0.OCQ00O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OQ0OQ0C0Q0.OCO00O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OQ0OQ0C0Q0.OQO00O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OQ0OQ0C0Q0.OOO00O0OQ0);
end;
OQOQQOQOQ0:begin
OCO0O0C0Q0.OCQ00OOCQ0(0);
OCO0O0C0Q0.O0OQCOOCQ0(OO0OQ0C0Q0.O0000O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OO0OQ0C0Q0.OCCC0O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OO0OQ0C0Q0.OO000O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OO0OQ0C0Q0.OQ000O0OQ0);
OCO0O0C0Q0.O0OQCOOCQ0(OO0OQ0C0Q0.OQCC0O0OQ0);
end;
O0QQQOQOQ0:
raise EScError.Create(seNotCompatibleFormatWithECKey);
else
Assert(False);
end;
OCQ0O0C0Q0:=OOOCCOQOQ0;
O0Q0O0C0Q0:=0;
if OQO0O0C0Q0<>'' then begin
O0Q0O0C0Q0:=OCO0O0C0Q0.OQCO0OOCQ0 mod OOCQCC0OQ0.OO00CC0OQ0(OCQ0O0C0Q0);
if O0Q0O0C0Q0>0 then begin
O0Q0O0C0Q0:=OOCQCC0OQ0.OO00CC0OQ0(OCQ0O0C0Q0)-O0Q0O0C0Q0;
if O0Q0O0C0Q0>0 then begin
SetLength(OOQ0O0C0Q0,O0Q0O0C0Q0);
System.FillChar(OOQ0O0C0Q0[0],O0Q0O0C0Q0,O0Q0O0C0Q0);
OCO0O0C0Q0.OO000OOCQ0(OOQ0O0C0Q0);
end;
end;
end;
SetLength(OQC0O0C0Q0,0);
OQC0O0C0Q0:=OCO0O0C0Q0.OOCC0OOCQ0;
PutIntBE(Length(OQC0O0C0Q0)-O0Q0O0C0Q0-4,TValueArr(OQC0O0C0Q0),0);
if OQO0O0C0Q0<>'' then begin
SetLength(OOC0O0C0Q0,OOCQCC0OQ0.OO00CC0OQ0(OCQ0O0C0Q0));
FillChar(OOC0O0C0Q0[0],Length(OOC0O0C0Q0),0);
SetLength(O0C0O0C0Q0,0);
O0C0O0C0Q0:=OOQ0000OQ0.OQOO000OQ0(OQ0OCOQOQ0,OQO0O0C0Q0,nil,OOCQCC0OQ0.OCCCCC0OQ0(OCQ0O0C0Q0),1,True);
OQQ0O0C0Q0:=OOCQCC0OQ0.OQCQCC0OQ0(OCQ0O0C0Q0,O0C0O0C0Q0,OOC0O0C0Q0);
FillChar(O0C0O0C0Q0[0],Length(O0C0O0C0Q0),0);
OQQ0O0C0Q0.CreateEncryptor.TransformBlock(OQC0O0C0Q0,0,Length(OQC0O0C0Q0));
end;
OCO0O0C0Q0.O0CC0OOCQ0;
OCO0O0C0Q0.OCQ00OOCQ0(integer(IETF_MAGIC_VAL));
OCO0O0C0Q0.OCQ00OOCQ0(0);
case OQCQOOC0Q0 of
OCOQQOQOQ0:
OCO0O0C0Q0.OCC00OOCQ0(IETF_RSA);
OQOQQOQOQ0:
OCO0O0C0Q0.OCC00OOCQ0(IETF_DSA);
O0QQQOQOQ0:
raise EScError.Create(seNotCompatibleFormatWithECKey);
else
Assert(False);
end;
if OQO0O0C0Q0='' then
OCO0O0C0Q0.OCC00OOCQ0(SNone)
else
OCO0O0C0Q0.OCC00OOCQ0(STripleDES_cbc);
OCO0O0C0Q0.O0OO0OOCQ0(OQC0O0C0Q0);
Result:=OCO0O0C0Q0.OOCC0OOCQ0;
PutIntBE(Length(Result),TValueArr(Result),4);
finally
OCO0O0C0Q0.Free;
end;
end;
function O0CQOOC0Q0.OCC0O0C0Q0:TBytes;
begin
Result:=OQ0CO0C0Q0;
end;
function O0CQOOC0Q0.OQ0CO0C0Q0:TBytes;
var
OC0CO0C0Q0:OC0QCOOCQ0;
begin
OC0CO0C0Q0:=OC0QCOOCQ0.Create;
try
OC0CO0C0Q0.OCC00OOCQ0(OOCQCC0OQ0.OCQ0CC0OQ0(OQCQOOC0Q0,OC0OQ0C0Q0.O0QQQO0OQ0));
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
OC0CO0C0Q0.O0QO0OOCQ0(OQ0OQ0C0Q0.OQQ00O0OQ0.GetBytes);
OC0CO0C0Q0.O0QO0OOCQ0(OQ0OQ0C0Q0.OCQ00O0OQ0.GetBytes);
end;
OQOQQOQOQ0:begin
OC0CO0C0Q0.O0QO0OOCQ0(OO0OQ0C0Q0.O0000O0OQ0.GetBytes);
OC0CO0C0Q0.O0QO0OOCQ0(OO0OQ0C0Q0.OO000O0OQ0.GetBytes);
OC0CO0C0Q0.O0QO0OOCQ0(OO0OQ0C0Q0.OCCC0O0OQ0.GetBytes);
OC0CO0C0Q0.O0QO0OOCQ0(OO0OQ0C0Q0.OQ000O0OQ0.GetBytes);
end;
O0QQQOQOQ0:begin
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
if not IsClass(OC0OQ0C0Q0.OOQQQO0OQ0,OQ00000OQ0)then
OC0CO0C0Q0.OCC00OOCQ0(OC0OO0Q0Q0.OCCQ00Q0Q0(OC0OQ0C0Q0.O0QQQO0OQ0));
OC0CO0C0Q0.O0OO0OOCQ0(OC0OQ0C0Q0.OOQQQO0OQ0.OCQ0CO0OQ0(OC0OQ0C0Q0.OQQQQO0OQ0));
end;
else
Assert(False);
end;
Result:=OC0CO0C0Q0.OOCC0OOCQ0;
finally
OC0CO0C0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.OQCCCOC0Q0(OCCCCOC0Q0:TStream;const O000COC0Q0:TBytes;const OO00COC0Q0:string);
var
OQ00COC0Q0:string;
OC00COC0Q0:TBytes;
begin
OQ00COC0Q0:=OOCQCC0OQ0.OCQ0CC0OQ0(OQCQOOC0Q0,OC0OQ0C0Q0.O0QQQO0OQ0)+' ';
OCCCCOC0Q0.WriteBuffer(Encoding.Default.GetBytes(OQ00COC0Q0)[0],Length(OQ00COC0Q0));
Assert(Length(O000COC0Q0)>0);
OC00COC0Q0:=TBase64.Encode(O000COC0Q0);
OCCCCOC0Q0.WriteBuffer(OC00COC0Q0[0],Length(OC00COC0Q0));
OQ00COC0Q0:=' '+OO00COC0Q0+#10;
OCCCCOC0Q0.WriteBuffer(Encoding.Default.GetBytes(OQ00COC0Q0)[0],Length(OQ00COC0Q0));
end;
const
OC0QCQQ0Q0=$6;
O0OQCQQ0Q0=$7;
OOOQCQQ0Q0=$2000;
OQOQCQQ0Q0=$A000;
OCOQCQQ0Q0=$200;
O0QQCQQ0Q0=$400;
OOQQCQQ0Q0=(OOOQCQQ0Q0 or O0QQCQQ0Q0);
OQQQCQQ0Q0=(OOOQCQQ0Q0 or OCOQCQQ0Q0);
OCQQCQQ0Q0=(OQOQCQQ0Q0 or O0QQCQQ0Q0);
type
O0CQCQQ0Q0=record
OOCQCQQ0Q0:byte;
OQCQCQQ0Q0:byte;
OCCQCQQ0Q0:word;
O00CCQQ0Q0:cardinal;
end;
function O0CQOOC0Q0.OOCOO0C0Q0(OQCOO0C0Q0:cardinal):O0C00O0OQ0;
var
OCCOO0C0Q0,O00Q00C0Q0,OO0Q00C0Q0:cardinal;
begin
OCCOO0C0Q0:=8;
O00Q00C0Q0:=12;
with Result do begin
OOC00O0OQ0:=4;
OQC00O0OQ0:=OQCOO0C0Q0 shr 3;
OCC00O0OQ0:=OQCOO0C0Q0 shr 4;
O00O0O0OQ0:=OQCOO0C0Q0 shr 4;
OO0O0O0OQ0:=OQCOO0C0Q0 shr 4;
OQ0O0O0OQ0:=OQCOO0C0Q0 shr 4;
OC0O0O0OQ0:=OQCOO0C0Q0 shr 4;
O0OO0O0OQ0:=OQCOO0C0Q0 shr 3;
OOCO0O0OQ0:=OCCOO0C0Q0+O00Q00C0Q0+OQC00O0OQ0+OCC00O0OQ0+O00O0O0OQ0+OO0O0O0OQ0+OQ0O0O0OQ0+OC0O0O0OQ0+O0OO0O0OQ0;
OQCO0O0OQ0:=OCCOO0C0Q0+O00Q00C0Q0+OQC00O0OQ0;
OO0Q00C0Q0:=OCCOO0C0Q0;
OOOO0O0OQ0:=OO0Q00C0Q0+8;
OQOO0O0OQ0:=OO0Q00C0Q0+O00Q00C0Q0;
OCOO0O0OQ0:=OQOO0O0OQ0+OQC00O0OQ0;
O0QO0O0OQ0:=OCOO0O0OQ0+OCC00O0OQ0;
OOQO0O0OQ0:=O0QO0O0OQ0+O00O0O0OQ0;
OQQO0O0OQ0:=OOQO0O0OQ0+OO0O0O0OQ0;
OCQO0O0OQ0:=OQQO0O0OQ0+OQ0O0O0OQ0;
O0CO0O0OQ0:=OCQO0O0OQ0+OC0O0O0OQ0;
end;
end;
function O0CQOOC0Q0.OQ0Q00C0Q0(OC0Q00C0Q0:cardinal):OCCO0O0OQ0;
var
O0OQ00C0Q0,OOOQ00C0Q0,OQOQ00C0Q0:cardinal;
begin
O0OQ00C0Q0:=8;
OOOQ00C0Q0:=8;
with Result do begin
O00QCO0OQ0:=OC0Q00C0Q0 shr 3;
OO0QCO0OQ0:=20;
OQ0QCO0OQ0:=OC0Q00C0Q0 shr 3;
OC0QCO0OQ0:=20;
O0OQCO0OQ0:=OC0Q00C0Q0 shr 3;
OOOQCO0OQ0:=4;
OQOQCO0OQ0:=20;
OCCQCO0OQ0:=O0OQ00C0Q0+OOOQ00C0Q0+O00QCO0OQ0+OO0QCO0OQ0+OQ0QCO0OQ0+OC0QCO0OQ0+OOOQCO0OQ0+OQOQCO0OQ0;
O00CCO0OQ0:=O0OQ00C0Q0+OOOQ00C0Q0+O00QCO0OQ0+OO0QCO0OQ0+OQ0QCO0OQ0+O0OQCO0OQ0+OOOQCO0OQ0+OQOQCO0OQ0;
OQOQ00C0Q0:=O0OQ00C0Q0;
OCOQCO0OQ0:=OQOQ00C0Q0+OOOQ00C0Q0;
O0QQCO0OQ0:=OCOQCO0OQ0+O00QCO0OQ0;
OOQQCO0OQ0:=O0QQCO0OQ0+OO0QCO0OQ0;
OQQQCO0OQ0:=OOQQCO0OQ0+OQ0QCO0OQ0;
OCQQCO0OQ0:=OQQQCO0OQ0+OC0QCO0OQ0;
O0CQCO0OQ0:=OQQQCO0OQ0+O0OQCO0OQ0;
OOCQCO0OQ0:=OCQQCO0OQ0+OOOQCO0OQ0;
OQCQCO0OQ0:=O0CQCO0OQ0+OOOQCO0OQ0;
end;
end;
procedure O0CQOOC0Q0.O00OO0C0Q0(const OO0OO0C0Q0:TBytes);
procedure OQ0OO0C0Q0(OC0OO0C0Q0:integer;O0OOO0C0Q0:Boolean);
var
OOOOO0C0Q0:O0C00O0OQ0;
begin
OOOOO0C0Q0:=OOCOO0C0Q0(OC0OO0C0Q0);
OQ0OQ0C0Q0.OCQ00O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOOOO0C0Q0.OQOO0O0OQ0,OOOOO0C0Q0.OQC00O0OQ0));
OQ0OQ0C0Q0.OQQ00O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOOOO0C0Q0.OOOO0O0OQ0,OOOOO0C0Q0.OOC00O0OQ0));
if O0OOO0C0Q0 then begin
OQ0OQ0C0Q0.OOO00O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOOOO0C0Q0.OCOO0O0OQ0,OOOOO0C0Q0.OCC00O0OQ0));
OQ0OQ0C0Q0.OQO00O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOOOO0C0Q0.O0QO0O0OQ0,OOOOO0C0Q0.O00O0O0OQ0));
OQ0OQ0C0Q0.OCO00O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOOOO0C0Q0.OCQO0O0OQ0,OOOOO0C0Q0.OC0O0O0OQ0));
OQ0OQ0C0Q0.O0O00O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOOOO0C0Q0.O0CO0O0OQ0,OOOOO0C0Q0.O0OO0O0OQ0));
OQQQ0OC0Q0;
end
else begin
FreeAndNil(OQ0OQ0C0Q0.O0O00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OOO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OQO00O0OQ0);
FreeAndNil(OQ0OQ0C0Q0.OCO00O0OQ0);
end;
end;
procedure OQOOO0C0Q0(OCOOO0C0Q0:integer;O0QOO0C0Q0:Boolean);
var
OOQOO0C0Q0:OCCO0O0OQ0;
begin
OOQOO0C0Q0:=OQ0Q00C0Q0(OCOOO0C0Q0);
OO0OQ0C0Q0.O0000O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOQOO0C0Q0.OCOQCO0OQ0,OOQOO0C0Q0.O00QCO0OQ0));
OO0OQ0C0Q0.OO000O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOQOO0C0Q0.O0QQCO0OQ0,OOQOO0C0Q0.OO0QCO0OQ0));
OO0OQ0C0Q0.OCCC0O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOQOO0C0Q0.OOQQCO0OQ0,OOQOO0C0Q0.OQ0QCO0OQ0));
if O0QOO0C0Q0 then begin
OO0OQ0C0Q0.OQCC0O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOQOO0C0Q0.OQQQCO0OQ0,OOQOO0C0Q0.OC0QCO0OQ0));
OOQQ0OC0Q0;
end
else begin
OO0OQ0C0Q0.OQ000O0OQ0:=TBigInteger.Create(TBigInteger.GetBigIntegerLE(OO0OO0C0Q0,OOQOO0C0Q0.OQQQCO0OQ0,OOQOO0C0Q0.O0OQCO0OQ0));
FreeAndNil(OO0OQ0C0Q0.OQCC0O0OQ0);
end;
end;
var
OQQOO0C0Q0:O0CQCQQ0Q0;
OCQOO0C0Q0:cardinal;
O0COO0C0Q0:cardinal;
begin
if Length(OO0OO0C0Q0)<16 then
O0OCOOC0Q0;
OQQOO0C0Q0.OOCQCQQ0Q0:=OO0OO0C0Q0[0];
OQQOO0C0Q0.OQCQCQQ0Q0:=OO0OO0C0Q0[1];
OQQOO0C0Q0.OCCQCQQ0Q0:=BitConverter.ToInt16(OO0OO0C0Q0,2);
OQQOO0C0Q0.O00CCQQ0Q0:=BitConverter.ToInt32(OO0OO0C0Q0,4);
if(OQQOO0C0Q0.OOCQCQQ0Q0<>OC0QCQQ0Q0)and(OQQOO0C0Q0.OOCQCQQ0Q0<>O0OQCQQ0Q0)then
raise EScError.Create(seWrongDataFormat);
OCQOO0C0Q0:=BitConverter.ToUInt32(OO0OO0C0Q0,8);
O0COO0C0Q0:=BitConverter.ToUInt32(OO0OO0C0Q0,12);
if(OQQOO0C0Q0.O00CCQQ0Q0=OOQQCQQ0Q0)or(OQQOO0C0Q0.O00CCQQ0Q0=OCQQCQQ0Q0)then begin
if((OQQOO0C0Q0.OOCQCQQ0Q0=OC0QCQQ0Q0)and(OCQOO0C0Q0=$31415352))or
((OQQOO0C0Q0.OOCQCQQ0Q0=O0OQCQQ0Q0)and(OCQOO0C0Q0=$32415352))then
OQCQOOC0Q0:=OCOQQOQOQ0
else
O0OCOOC0Q0;
end
else
if OQQOO0C0Q0.O00CCQQ0Q0=OQQQCQQ0Q0 then begin
if((OQQOO0C0Q0.OOCQCQQ0Q0=OC0QCQQ0Q0)and(OCQOO0C0Q0=$31535344))or
((OQQOO0C0Q0.OOCQCQQ0Q0=O0OQCQQ0Q0)and(OCQOO0C0Q0=$32535344))then
OQCQOOC0Q0:=OQOQQOQOQ0
else
O0OCOOC0Q0;
end
else
raise EScError.Create(seWrongDataFormat);
case OQCQOOC0Q0 of
OCOQQOQOQ0:
OQ0OO0C0Q0(O0COO0C0Q0,OQQOO0C0Q0.OOCQCQQ0Q0=O0OQCQQ0Q0);
OQOQQOQOQ0:
OQOOO0C0Q0(O0COO0C0Q0,OQQOO0C0Q0.OOCQCQQ0Q0=O0OQCQQ0Q0);
O0QQQOQOQ0:
raise EScError.Create(seWrongDataFormat);
else
Assert(False);
end;
O0QQ0CC0Q0:=True;
end;
{$IFDEF MSWINDOWS}
procedure OO0CCQQ0Q0(var OQ0CCQQ0Q0:TBytes;OC0CCQQ0Q0,O0OCCQQ0Q0:integer;const OOOCCQQ0Q0:TBytes);
begin
Buffer.BlockCopy(OOOCCQQ0Q0,0,OQ0CCQQ0Q0,OC0CCQQ0Q0,Length(OOOCCQQ0Q0));
if O0OCCQQ0Q0>Length(OOOCCQQ0Q0)then
FillChar(OQ0CCQQ0Q0[OC0CCQQ0Q0+Length(OOOCCQQ0Q0)],O0OCCQQ0Q0-Length(OOOCCQQ0Q0),0);
end;
function O0CQOOC0Q0.OOOC00C0Q0:TBytes;
var
OQOC00C0Q0:TBytes;
OCOC00C0Q0:cardinal;
O0QC00C0Q0:integer;
OOQC00C0Q0:Byte;
OQQC00C0Q0:O0C00O0OQ0;
begin
if not OQCC0CC0Q0 then
raise EScError.Create(seKeyNotReady);
OCOC00C0Q0:=O0Q0Q0C0Q0;
OQQC00C0Q0:=OOCOO0C0Q0(OCOC00C0Q0);
if O0QOOCQ0Q0 then begin
OOQC00C0Q0:=O0OQCQQ0Q0;
O0QC00C0Q0:=20+(9*(OCOC00C0Q0 shr 4));
OQOC00C0Q0:=Encoding.Default.GetBytes('RSA2');
end
else begin
OOQC00C0Q0:=OC0QCQQ0Q0;
O0QC00C0Q0:=20+(OCOC00C0Q0 shr 3);
OQOC00C0Q0:=Encoding.Default.GetBytes('RSA1');
end;
SetLength(Result,O0QC00C0Q0);
Result[0]:=OOQC00C0Q0;
Result[1]:=2;
Result[2]:=0;
Result[3]:=0;
PutIntLE(OCQQCQQ0Q0,Result,4);
Buffer.BlockCopy(OQOC00C0Q0,0,Result,8,4);
PutIntLE(OCOC00C0Q0,Result,12);
if Length(OQ0OQ0C0Q0.OQQ00O0OQ0.GetBytes)>4 then
raise EScError.Create(seLargeExpLength);
OO0CCQQ0Q0(Result,OQQC00C0Q0.OQOO0O0OQ0,OQQC00C0Q0.OQC00O0OQ0,OQ0OQ0C0Q0.OCQ00O0OQ0.GetBytesLE);
OO0CCQQ0Q0(Result,OQQC00C0Q0.OOOO0O0OQ0,OQQC00C0Q0.OOC00O0OQ0,OQ0OQ0C0Q0.OQQ00O0OQ0.GetBytesLE);
if O0QOOCQ0Q0 then begin
OO0CCQQ0Q0(Result,OQQC00C0Q0.OCOO0O0OQ0,OQQC00C0Q0.OCC00O0OQ0,OQ0OQ0C0Q0.OOO00O0OQ0.GetBytesLE);
OO0CCQQ0Q0(Result,OQQC00C0Q0.O0QO0O0OQ0,OQQC00C0Q0.O00O0O0OQ0,OQ0OQ0C0Q0.OQO00O0OQ0.GetBytesLE);
OO0CCQQ0Q0(Result,OQQC00C0Q0.OOQO0O0OQ0,OQQC00C0Q0.OO0O0O0OQ0,OQ0OQ0C0Q0.O0Q00O0OQ0.GetBytesLE);
OO0CCQQ0Q0(Result,OQQC00C0Q0.OQQO0O0OQ0,OQQC00C0Q0.OQ0O0O0OQ0,OQ0OQ0C0Q0.OOQ00O0OQ0.GetBytesLE);
OO0CCQQ0Q0(Result,OQQC00C0Q0.OCQO0O0OQ0,OQQC00C0Q0.OC0O0O0OQ0,OQ0OQ0C0Q0.OCO00O0OQ0.GetBytesLE);
OO0CCQQ0Q0(Result,OQQC00C0Q0.O0CO0O0OQ0,OQQC00C0Q0.O0OO0O0OQ0,OQ0OQ0C0Q0.O0O00O0OQ0.GetBytesLE);
end;
end;
function O0CQOOC0Q0.OCQC00C0Q0:TBytes;
var
O0CC00C0Q0:TBytes;
OOCC00C0Q0:cardinal;
OQCC00C0Q0:integer;
OCCC00C0Q0:Byte;
O00000C0Q0:OCCO0O0OQ0;
begin
if not OQCC0CC0Q0 then
raise EScError.Create(seKeyNotReady);
OOCC00C0Q0:=O0Q0Q0C0Q0;
O00000C0Q0:=OQ0Q00C0Q0(OOCC00C0Q0);
if O0QOOCQ0Q0 then begin
OCCC00C0Q0:=O0OQCQQ0Q0;
OQCC00C0Q0:=O00000C0Q0.OCCQCO0OQ0;
O0CC00C0Q0:=Encoding.Default.GetBytes('DSS2');
end
else begin
OCCC00C0Q0:=OC0QCQQ0Q0;
OQCC00C0Q0:=O00000C0Q0.O00CCO0OQ0;
O0CC00C0Q0:=Encoding.Default.GetBytes('DSS1');
end;
SetLength(Result,OQCC00C0Q0);
Result[0]:=OCCC00C0Q0;
Result[1]:=2;
Result[2]:=0;
Result[3]:=0;
PutIntLE(OQQQCQQ0Q0,Result,4);
Buffer.BlockCopy(O0CC00C0Q0,0,Result,8,4);
PutIntLE(OOCC00C0Q0,Result,12);
OO0CCQQ0Q0(Result,O00000C0Q0.OCOQCO0OQ0,O00000C0Q0.O00QCO0OQ0,OO0OQ0C0Q0.O0000O0OQ0.GetBytesLE);
OO0CCQQ0Q0(Result,O00000C0Q0.O0QQCO0OQ0,O00000C0Q0.OO0QCO0OQ0,OO0OQ0C0Q0.OO000O0OQ0.GetBytesLE);
OO0CCQQ0Q0(Result,O00000C0Q0.OOQQCO0OQ0,O00000C0Q0.OQ0QCO0OQ0,OO0OQ0C0Q0.OCCC0O0OQ0.GetBytesLE);
if O0QOOCQ0Q0 then begin
OO0CCQQ0Q0(Result,O00000C0Q0.OQQQCO0OQ0,O00000C0Q0.OC0QCO0OQ0,OO0OQ0C0Q0.OQCC0O0OQ0.GetBytesLE);
PutIntLE($FFFFFFFF,Result,O00000C0Q0.OCQQCO0OQ0);
FillChar(Result[O00000C0Q0.OOCQCO0OQ0],O00000C0Q0.OQOQCO0OQ0,0);
end
else begin
OO0CCQQ0Q0(Result,O00000C0Q0.OQQQCO0OQ0,O00000C0Q0.O0OQCO0OQ0,OO0OQ0C0Q0.OQ000O0OQ0.GetBytesLE);
PutIntLE($FFFFFFFF,Result,O00000C0Q0.O0CQCO0OQ0);
FillChar(Result[O00000C0Q0.OQCQCO0OQ0],O00000C0Q0.OQOQCO0OQ0,0);
end;
end;
{$ENDIF MSWINDOWS}
procedure O0CQOOC0Q0.O0OCOCQ0Q0(const OQ0CCQC0Q0:string;const OOOCOCQ0Q0:boolean;const OQOCOCQ0Q0:string;
const OCOCOCQ0Q0:OO00COQOQ0=OOOCCOQOQ0;const O0QCOCQ0Q0:OQOCQ00OQ0=O00CQ00OQ0;const OOQCOCQ0Q0:string='');
var
O0OCCQC0Q0:TFileStream;
begin
O0OCCQC0Q0:=TFileStream.Create(OQ0CCQC0Q0,fmCreate);
try
O0OCOCQ0Q0(O0OCCQC0Q0,OOOCOCQ0Q0,OQOCOCQ0Q0,OCOCOCQ0Q0,O0QCOCQ0Q0,OOQCOCQ0Q0);
finally
O0OCCQC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.O0OCOCQ0Q0(O0OCCQC0Q0:TStream;const OOOCOCQ0Q0:boolean;const OQOCOCQ0Q0:string;
const OCOCOCQ0Q0:OO00COQOQ0=OOOCCOQOQ0;const O0QCOCQ0Q0:OQOCQ00OQ0=O00CQ00OQ0;const OOQCOCQ0Q0:string='');
var
OQQCOCQ0Q0,OCQCOCQ0Q0:string;
O0CCOCQ0Q0:TBytes;
OOCCOCQ0Q0:integer;
begin
if not OQCC0CC0Q0 then
raise EScError.Create(seKeyNotReady);
if OOOCOCQ0Q0 then begin
if not(O0QCOCQ0Q0 in[O00CQ00OQ0,O0OCQ00OQ0,OOOCQ00OQ0])then
raise EScError.Create(seNotCompatibleFormatWithPublicKeyOnly);
if OQOCOCQ0Q0<>'' then
raise EScError.Create(seNotCompatibleFormatWithPassphrase);
end
else
if not O0QOOCQ0Q0 then
raise EScError.Create(seKeyNotPrivate);
if(OQOCOCQ0Q0<>'')and(O0QCOCQ0Q0 in[OO0CQ00OQ0,OQ0CQ00OQ0])then
raise EScError.Create(seNotCompatibleFormatWithPassphrase);
if(OOQCOCQ0Q0<>'')and(O0QCOCQ0Q0<>O0OCQ00OQ0)and not OOOCOCQ0Q0 then
raise EScError.Create(seNotCompatibleFormatWithComment);
SetLength(O0CCOCQ0Q0,0);
case O0QCOCQ0Q0 of
O00CQ00OQ0,OOOCQ00OQ0:begin
if OOOCOCQ0Q0 then begin
if O0QCOCQ0Q0=OOOCQ00OQ0 then begin
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
OQQCOCQ0Q0:=PKCS1_RSA_PUB_HEADER;
OCQCOCQ0Q0:=PKCS1_RSA_PUB_FOOTER;
end;
OQOQQOQOQ0:begin
OQQCOCQ0Q0:=PKCS1_DSA_PUB_HEADER;
OCQCOCQ0Q0:=PKCS1_DSA_PUB_FOOTER;
end;
O0QQQOQOQ0:begin
OQQCOCQ0Q0:=PKCS1_PUB_HEADER;
OCQCOCQ0Q0:=PKCS1_PUB_FOOTER;
end;
else
Assert(False);
end;
O0CCOCQ0Q0:=OCCQO0C0Q0(True);
end
else begin
O0CCOCQ0Q0:=OQ0CO0C0Q0;
OQCCCOC0Q0(O0OCCQC0Q0,O0CCOCQ0Q0,OOQCOCQ0Q0);
Exit;
end;
end
else begin
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
OQQCOCQ0Q0:=PKCS1_RSA_PRIV_HEADER;
OCQCOCQ0Q0:=PKCS1_RSA_PRIV_FOOTER;
end;
OQOQQOQOQ0:begin
OQQCOCQ0Q0:=PKCS1_DSA_PRIV_HEADER;
OCQCOCQ0Q0:=PKCS1_DSA_PRIV_FOOTER;
end;
O0QQQOQOQ0:begin
OQQCOCQ0Q0:=EC_PRIV_HEADER;
OCQCOCQ0Q0:=EC_PRIV_FOOTER;
end;
else
Assert(False);
end;
O0CCOCQ0Q0:=OCCQO0C0Q0(False);
end;
end;
OO0CQ00OQ0,OQ0CQ00OQ0:begin
OQQCOCQ0Q0:=PKCS8_HEADER;
OCQCOCQ0Q0:=PKCS8_FOOTER;
O0CCOCQ0Q0:=O0OCO0C0Q0;
end;
OC0CQ00OQ0:begin
OQQCOCQ0Q0:=PKCS8_ENC_HEADER;
OCQCOCQ0Q0:=PKCS8_ENC_FOOTER;
O0CCOCQ0Q0:=OCOCO0C0Q0(OQOCOCQ0Q0,OCOCOCQ0Q0);
end;
O0OCQ00OQ0:begin
if OOOCOCQ0Q0 then begin
OQQCOCQ0Q0:=IETF_PUB_HEADER;
OCQCOCQ0Q0:=IETF_PUB_FOOTER;
end
else begin
if OQCQOOC0Q0=O0QQQOQOQ0 then
raise EScError.Create(seNotCompatibleFormatWithECKey);
OQQCOCQ0Q0:=IETF_PRIV_HEADER;
OCQCOCQ0Q0:=IETF_PRIV_FOOTER;
end;
if OOOCOCQ0Q0 then
O0CCOCQ0Q0:=OCC0O0C0Q0
else begin
if(OQOCOCQ0Q0<>'')and(OCOCOCQ0Q0<>OOOCCOQOQ0)then
raise EScError.Create(seCipherNotSupported);
O0CCOCQ0Q0:=OOO0O0C0Q0(OQOCOCQ0Q0);
end;
end;
else
Assert(False);
end;
Assert(Length(O0CCOCQ0Q0)>0);
if O0QCOCQ0Q0=OO0CQ00OQ0 then begin
O0OCCQC0Q0.WriteBuffer(O0CCOCQ0Q0[0],Length(O0CCOCQ0Q0));
end
else begin
O0OOC0QOQ0.OC0QQ0QOQ0(O0OCCQC0Q0,OQQCOCQ0Q0);
if(O0QCOCQ0Q0 in[O00CQ00OQ0,OOOCQ00OQ0])and not OOOCOCQ0Q0 and(OQOCOCQ0Q0<>'')then
OCQQCOC0Q0(O0OCCQC0Q0,OQOCOCQ0Q0,OCOCOCQ0Q0,O0CCOCQ0Q0);
if(O0QCOCQ0Q0=O0OCQ00OQ0)and not OOOCOCQ0Q0 then
OOCCOCQ0Q0:=70
else
OOCCOCQ0Q0:=OCQQ0QQ0Q0;
if OOQCOCQ0Q0<>'' then
OCOO0QQ0Q0(O0OCCQC0Q0,Encoding.Default.GetBytes('Comment: '+OOQCOCQ0Q0),OOCCOCQ0Q0,True);
OCOO0QQ0Q0(O0OCCQC0Q0,TBase64.Encode(O0CCOCQ0Q0),OOCCOCQ0Q0,False);
O0OOC0QOQ0.OC0QQ0QOQ0(O0OCCQC0Q0,OCQCOCQ0Q0);
end;
end;
procedure O0CQOOC0Q0.OOOQOCQ0Q0(const OQOQOCQ0Q0:TBytes);
begin
OQCC0CC0Q0:=False;
O00OO0C0Q0(OQOQOCQ0Q0);
end;
procedure O0CQOOC0Q0.OCCOQ0C0Q0(const O00QOCQ0Q0:TBytes;
const OO0QOCQ0Q0:OCQCQ00OQ0;const OQ0QOCQ0Q0:OOQQQOQOQ0;const OC0QOCQ0Q0:boolean);
begin
OQCC0CC0Q0:=False;
case OO0QOCQ0Q0 of
OQQCQ00OQ0:
O00OO0C0Q0(O00QOCQ0Q0);
OCOCQ00OQ0:
OOQ0COC0Q0(O00QOCQ0Q0,OQ0QOCQ0Q0,OC0QOCQ0Q0);
O0QCQ00OQ0:
OCOQQOC0Q0(O00QOCQ0Q0);
{$IFNDEF MSWINDOWS}
OOQCQ00OQ0:
OCOQ00C0Q0(O00QOCQ0Q0,OC0QOCQ0Q0);
{$ENDIF}
else
raise EScError.Create(seWrongDataFormat);
end;
OOOCOOC0Q0;
end;
function O0CQOOC0Q0.O0OQOCQ0Q0:TBytes;
begin
if not OQCC0CC0Q0 then
raise EScError.Create(seKeyNotReady);
SetLength(Result,0);
{$IFDEF MSWINDOWS}
case OQCQOOC0Q0 of
OCOQQOQOQ0:
Result:=OOOC00C0Q0;
OQOQQOQOQ0:
Result:=OCQC00C0Q0;
O0QQQOQOQ0:
raise EScError.Create(seNotCompatibleFormatWithECKey);
else
Assert(False);
end;
{$ELSE}
Result:=OCCQO0C0Q0(not O0QOOCQ0Q0);
{$ENDIF}
end;
class function O0CQOOC0Q0.OO0000C0Q0(const OQ0000C0Q0:TBytes;OC0000C0Q0:integer;O0O000C0Q0:OOOOCOQOQ0):TBytes;
var
OOO000C0Q0:THashAlgorithm;
OQO000C0Q0,OCO000C0Q0,O0Q000C0Q0,OOQ000C0Q0:integer;
OQQ000C0Q0,OCQ000C0Q0:TBytes;
begin
Result:=nil;
OOO000C0Q0:=OOCQCC0OQ0.OOOCCC0OQ0(O0O000C0Q0);
try
O0Q000C0Q0:=Length(OQ0000C0Q0);
SetLength(OQQ000C0Q0,O0Q000C0Q0+4);
Move(OQ0000C0Q0[0],OQQ000C0Q0[0],O0Q000C0Q0);
SetLength(OCQ000C0Q0,0);
OCO000C0Q0:=OOO000C0Q0.HashSize;
OQO000C0Q0:=0;
OOQ000C0Q0:=0;
while OQO000C0Q0<OC0000C0Q0 do begin
PutIntBE(OOQ000C0Q0,TValueArr(OQQ000C0Q0),O0Q000C0Q0);
OCQ000C0Q0:=OOO000C0Q0.ComputeHash(OQQ000C0Q0);
if Result=nil then
Result:=OCQ000C0Q0
else begin
SetLength(Result,OQO000C0Q0+OCO000C0Q0);
Move(OCQ000C0Q0[0],Result[OQO000C0Q0],OCO000C0Q0);
end;
Inc(OQO000C0Q0,OCO000C0Q0);
Inc(OOQ000C0Q0);
end;
SetLength(Result,OC0000C0Q0);
finally
OOO000C0Q0.Free;
end;
end;
function O0CQOOC0Q0.O0C000C0Q0(const OOC000C0Q0:TBytes;OQC000C0Q0:OQ0Q0OOOQ0):TBytes;
var
OCC000C0Q0,O00O00C0Q0:integer;
OO0O00C0Q0,OQ0O00C0Q0,OC0O00C0Q0:integer;
O0OO00C0Q0:TBytes;
OOOO00C0Q0,OQOO00C0Q0:TBytes;
OCOO00C0Q0,O0QO00C0Q0,OOQO00C0Q0,OQQO00C0Q0:TBytes;
OCQO00C0Q0:integer;
begin
if OCCQ0QQ0Q0=nil then
raise Exception.Create(SInternalError);
OO0O00C0Q0:=OCO0Q0C0Q0;
case OQC000C0Q0 of
OQCOOOOOQ0:begin
OCC000C0Q0:=Length(OOC000C0Q0);
O00O00C0Q0:=OO0O00C0Q0-OCC000C0Q0;
if O00O00C0Q0<11 then
raise EScError.CreateFmt(SErrorEncryptingData,[SMessageTooLong],seErrorEncryptingData);
SetLength(Result,OO0O00C0Q0);
Result[0]:=0;
Result[1]:=2;
Result[O00O00C0Q0-1]:=0;
Move(OOC000C0Q0[0],Result[O00O00C0Q0],OCC000C0Q0);
OCCQ0QQ0Q0.Random(@Result[2],O00O00C0Q0-3);
for OCQO00C0Q0:=2 to O00O00C0Q0-2 do
while Result[OCQO00C0Q0]=0 do
OCCQ0QQ0Q0.Random(@Result[OCQO00C0Q0],1);
end;
OCCOOOOOQ0:begin
OCC000C0Q0:=Length(OOC000C0Q0);
O00O00C0Q0:=OO0O00C0Q0-OCC000C0Q0;
if O00O00C0Q0<(2+2*OOCQCC0OQ0.OC00CC0OQ0(O00COOC0Q0.O0O00OOOQ0))then
raise EScError.CreateFmt(SErrorEncryptingData,[SMessageTooLong],seErrorEncryptingData);
SetLength(O0OO00C0Q0,0);
O0OO00C0Q0:=O00COOC0Q0.OQ000OOOQ0;
OQ0O00C0Q0:=Length(O0OO00C0Q0);
OC0O00C0Q0:=OO0O00C0Q0-OQ0O00C0Q0-1;
SetLength(OQQO00C0Q0,OC0O00C0Q0);
Move(O0OO00C0Q0[0],OQQO00C0Q0[0],OQ0O00C0Q0);
FillChar(OQQO00C0Q0[OQ0O00C0Q0],OC0O00C0Q0-OCC000C0Q0-OQ0O00C0Q0-1,0);
OQQO00C0Q0[OC0O00C0Q0-OCC000C0Q0-1]:=1;
Move(OOC000C0Q0[0],OQQO00C0Q0[OC0O00C0Q0-OCC000C0Q0],OCC000C0Q0);
SetLength(OOQO00C0Q0,OQ0O00C0Q0);
OCCQ0QQ0Q0.Random(@OOQO00C0Q0[0],OQ0O00C0Q0);
SetLength(O0QO00C0Q0,0);
O0QO00C0Q0:=OO0000C0Q0(OOQO00C0Q0,OC0O00C0Q0,O00COOC0Q0.OOO00OOOQ0);
SetLength(OQOO00C0Q0,OC0O00C0Q0);
for OCQO00C0Q0:=0 to OC0O00C0Q0-1 do
OQOO00C0Q0[OCQO00C0Q0]:=OQQO00C0Q0[OCQO00C0Q0]xor O0QO00C0Q0[OCQO00C0Q0];
SetLength(OCOO00C0Q0,0);
OCOO00C0Q0:=OO0000C0Q0(OQOO00C0Q0,OQ0O00C0Q0,O00COOC0Q0.OOO00OOOQ0);
SetLength(OOOO00C0Q0,OQ0O00C0Q0);
for OCQO00C0Q0:=0 to OQ0O00C0Q0-1 do
OOOO00C0Q0[OCQO00C0Q0]:=OOQO00C0Q0[OCQO00C0Q0]xor OCOO00C0Q0[OCQO00C0Q0];
SetLength(Result,OO0O00C0Q0);
Result[0]:=0;
Move(OOOO00C0Q0[0],Result[1],OQ0O00C0Q0);
Move(OQOO00C0Q0[0],Result[1+OQ0O00C0Q0],OC0O00C0Q0);
end;
OO0Q0OOOQ0:begin
OCC000C0Q0:=Length(OOC000C0Q0);
if OCC000C0Q0<>OO0O00C0Q0 then
raise EScError.CreateFmt(SErrorEncryptingData,[SInvalidLength],seErrorEncryptingData);
Result:=OOC000C0Q0;
end;
else
raise EScError.Create(seInvalidInputArgs);
end;
end;
function O0CQOOC0Q0.O0CO00C0Q0(const OOCO00C0Q0:TBytes;OQCO00C0Q0:OOOOCOQOQ0;OCCO00C0Q0:OQ0Q0OOOQ0):TBytes;
var
O00QC0C0Q0,OO0QC0C0Q0:integer;
OQ0QC0C0Q0,OC0QC0C0Q0,O0OQC0C0Q0:integer;
OOOQC0C0Q0:integer;
OQOQC0C0Q0:THashAlgorithm;
OCOQC0C0Q0:integer;
O0QQC0C0Q0:PByteArray;
OOQQC0C0Q0,OQQQC0C0Q0:TBytes;
OCQQC0C0Q0:TBytes;
O0CQC0C0Q0,OOCQC0C0Q0,OQCQC0C0Q0:TBytes;
OCCQC0C0Q0:byte;
O00CC0C0Q0:integer;
begin
if OCCQ0QQ0Q0=nil then
raise Exception.Create(SInternalError);
OQ0QC0C0Q0:=OCO0Q0C0Q0;
OC0QC0C0Q0:=Length(OOCO00C0Q0);
case OCCO00C0Q0 of
OOCOOOOOQ0:begin
if OQCO00C0Q0=O0C0COQOQ0 then
OOQQC0C0Q0:=OOCO00C0Q0
else begin
O0CC0OC0Q0(OQCO00C0Q0,O0QQC0C0Q0,OCOQC0C0Q0);
SetLength(OOQQC0C0Q0,OCOQC0C0Q0+OC0QC0C0Q0);
if O0QQC0C0Q0<>nil then
Move(O0QQC0C0Q0^,OOQQC0C0Q0[0],OCOQC0C0Q0);
Move(OOCO00C0Q0[0],OOQQC0C0Q0[OCOQC0C0Q0],OC0QC0C0Q0);
end;
O00QC0C0Q0:=Length(OOQQC0C0Q0);
OO0QC0C0Q0:=OQ0QC0C0Q0-O00QC0C0Q0;
if OO0QC0C0Q0<11 then
raise EScError.CreateFmt(SErrorEncryptingData,[SMessageTooLong],seErrorEncryptingData);
SetLength(Result,OQ0QC0C0Q0);
Result[0]:=0;
Result[1]:=1;
Result[OO0QC0C0Q0-1]:=0;
FillChar(Result[2],OO0QC0C0Q0-3,$FF);
Move(OOQQC0C0Q0[0],Result[OO0QC0C0Q0],O00QC0C0Q0);
end;
O00Q0OOOQ0:begin
OOOQC0C0Q0:=OQ0OQ0C0Q0.OCQ00O0OQ0.BitCount-1;
if OQ0QC0C0Q0<OC0QC0C0Q0+OCCQOOC0Q0.OQOC0OOOQ0+2 then
raise EScError.CreateFmt(SErrorEncryptingData,[SInvalidSaltLength],seErrorEncryptingData);
if OCCQOOC0Q0.OQOC0OOOQ0>0 then begin
SetLength(OOCQC0C0Q0,OCCQOOC0Q0.OQOC0OOOQ0);
OCCQ0QQ0Q0.Random(@OOCQC0C0Q0[0],OCCQOOC0Q0.OQOC0OOOQ0);
end;
SetLength(OOQQC0C0Q0,OC0QC0C0Q0+OCCQOOC0Q0.OQOC0OOOQ0+8);
FillChar(OOQQC0C0Q0[0],8,0);
Move(OOCO00C0Q0[0],OOQQC0C0Q0[8],OC0QC0C0Q0);
if OCCQOOC0Q0.OQOC0OOOQ0>0 then
Move(OOCQC0C0Q0[0],OOQQC0C0Q0[8+OC0QC0C0Q0],OCCQOOC0Q0.OQOC0OOOQ0);
SetLength(OQQQC0C0Q0,0);
OQOQC0C0Q0:=OOCQCC0OQ0.OOOCCC0OQ0(OQCO00C0Q0);
try
OQQQC0C0Q0:=OQOQC0C0Q0.ComputeHash(OOQQC0C0Q0);
finally
OQOQC0C0Q0.Free;
end;
OO0QC0C0Q0:=OQ0QC0C0Q0-OCCQOOC0Q0.OQOC0OOOQ0-OC0QC0C0Q0-2;
O0OQC0C0Q0:=OO0QC0C0Q0+OCCQOOC0Q0.OQOC0OOOQ0+1;
SetLength(OQCQC0C0Q0,O0OQC0C0Q0);
FillChar(OQCQC0C0Q0[0],OO0QC0C0Q0,0);
OQCQC0C0Q0[OO0QC0C0Q0]:=1;
if OCCQOOC0Q0.OQOC0OOOQ0>0 then
Move(OOCQC0C0Q0[0],OQCQC0C0Q0[OO0QC0C0Q0+1],OCCQOOC0Q0.OQOC0OOOQ0);
SetLength(O0CQC0C0Q0,0);
O0CQC0C0Q0:=OO0000C0Q0(OQQQC0C0Q0,O0OQC0C0Q0,OCCQOOC0Q0.OOOC0OOOQ0);
SetLength(OCQQC0C0Q0,O0OQC0C0Q0);
for O00CC0C0Q0:=0 to O0OQC0C0Q0-1 do
OCQQC0C0Q0[O00CC0C0Q0]:=OQCQC0C0Q0[O00CC0C0Q0]xor O0CQC0C0Q0[O00CC0C0Q0];
for O00CC0C0Q0:=0 to(OQ0QC0C0Q0*8-OOOQC0C0Q0)-1 do begin
OCCQC0C0Q0:=$7F shr O00CC0C0Q0;
OCQQC0C0Q0[0]:=OCQQC0C0Q0[0]and OCCQC0C0Q0;
end;
SetLength(Result,OQ0QC0C0Q0);
Move(OCQQC0C0Q0[0],Result[0],O0OQC0C0Q0);
Move(OQQQC0C0Q0[0],Result[O0OQC0C0Q0],OC0QC0C0Q0);
Result[OQ0QC0C0Q0-1]:=$bc;
end;
else
raise EScError.Create(seInvalidInputArgs);
end;
end;
function O0CQOOC0Q0.OO0CC0C0Q0(const OQ0CC0C0Q0,OC0CC0C0Q0:TBytes;O0OCC0C0Q0:OOOOCOQOQ0;OOOCC0C0Q0:OQ0Q0OOOQ0):boolean;
var
OQOCC0C0Q0:integer;
OCOCC0C0Q0:THashAlgorithm;
O0QCC0C0Q0:integer;
OOQCC0C0Q0:PByteArray;
OQQCC0C0Q0,OCQCC0C0Q0,O0CCC0C0Q0:TBytes;
OOCCC0C0Q0:TBytes;
OQCCC0C0Q0,OCCCC0C0Q0:TBytes;
O000C0C0Q0,OO00C0C0Q0,OQ00C0C0Q0:integer;
OC00C0C0Q0:integer;
O0O0C0C0Q0:byte;
OOO0C0C0Q0:integer;
begin
Result:=False;
O000C0C0Q0:=Length(OQ0CC0C0Q0);
OQOCC0C0Q0:=Length(OC0CC0C0Q0);
case OOOCC0C0Q0 of
OOCOOOOOQ0:begin
if(OQOCC0C0Q0<=11)or(OC0CC0C0Q0[0]<>0)or(OC0CC0C0Q0[1]<>1)then
raise EScError.Create(seIPCorruptData);
OOO0C0C0Q0:=2;
while OOO0C0C0Q0<OQOCC0C0Q0 do begin
if OC0CC0C0Q0[OOO0C0C0Q0]=0 then
break;
if OC0CC0C0Q0[OOO0C0C0Q0]<>$FF then
raise EScError.Create(seIPCorruptData);
Inc(OOO0C0C0Q0);
end;
if OOO0C0C0Q0=OQOCC0C0Q0 then
raise EScError.Create(seIPCorruptData);
Inc(OOO0C0C0Q0);
O0CC0OC0Q0(O0OCC0C0Q0,OOQCC0C0Q0,O0QCC0C0Q0);
if(OQOCC0C0Q0-OOO0C0C0Q0)<>(O0QCC0C0Q0+O000C0C0Q0)then
Exit
else begin
if OOQCC0C0Q0<>nil then
if MemCompare(OOQCC0C0Q0,@OC0CC0C0Q0[OOO0C0C0Q0],O0QCC0C0Q0)<>0 then
Exit;
if MemCompare(@OQ0CC0C0Q0[0],@OC0CC0C0Q0[OOO0C0C0Q0+O0QCC0C0Q0],O000C0C0Q0)<>0 then
Exit;
end;
Result:=True;
end;
O00Q0OOOQ0:begin
OC00C0C0Q0:=OQ0OQ0C0Q0.OCQ00O0OQ0.BitCount-1;
if OQOCC0C0Q0<O000C0C0Q0+OCCQOOC0Q0.OQOC0OOOQ0+2 then
raise EScError.Create(seIPCorruptData);
if OC0CC0C0Q0[OQOCC0C0Q0-1]<>$bc then
Exit;
OO00C0C0Q0:=OQOCC0C0Q0-O000C0C0Q0-1;
SetLength(OOCCC0C0Q0,OO00C0C0Q0);
Move(OC0CC0C0Q0[0],OOCCC0C0Q0[0],OO00C0C0Q0);
SetLength(OCQCC0C0Q0,O000C0C0Q0);
Move(OC0CC0C0Q0[OO00C0C0Q0],OCQCC0C0Q0[0],O000C0C0Q0);
for OOO0C0C0Q0:=0 to(OQOCC0C0Q0*8-OC00C0C0Q0)-1 do begin
O0O0C0C0Q0:=128 shr OOO0C0C0Q0;
if(OOCCC0C0Q0[0]and O0O0C0C0Q0)<>0 then
Exit;
end;
SetLength(OQCCC0C0Q0,0);
OQCCC0C0Q0:=OO0000C0Q0(OCQCC0C0Q0,OO00C0C0Q0,OCCQOOC0Q0.OOOC0OOOQ0);
SetLength(OCCCC0C0Q0,OO00C0C0Q0);
for OOO0C0C0Q0:=0 to OO00C0C0Q0-1 do
OCCCC0C0Q0[OOO0C0C0Q0]:=OOCCC0C0Q0[OOO0C0C0Q0]xor OQCCC0C0Q0[OOO0C0C0Q0];
for OOO0C0C0Q0:=0 to(OQOCC0C0Q0*8-OC00C0C0Q0)-1 do begin
O0O0C0C0Q0:=$7F shr OOO0C0C0Q0;
OCCCC0C0Q0[0]:=OCCCC0C0Q0[0]and O0O0C0C0Q0;
end;
OQ00C0C0Q0:=OQOCC0C0Q0-O000C0C0Q0-OCCQOOC0Q0.OQOC0OOOQ0-2;
for OOO0C0C0Q0:=0 to OQ00C0C0Q0-1 do begin
if OCCCC0C0Q0[OOO0C0C0Q0]<>0 then
Exit;
end;
if OCCCC0C0Q0[OQ00C0C0Q0]<>1 then
Exit;
SetLength(OQQCC0C0Q0,O000C0C0Q0+OCCQOOC0Q0.OQOC0OOOQ0+8);
FillChar(OQQCC0C0Q0[0],8,0);
Move(OQ0CC0C0Q0[0],OQQCC0C0Q0[8],O000C0C0Q0);
if OCCQOOC0Q0.OQOC0OOOQ0>0 then
Move(OCCCC0C0Q0[OQ00C0C0Q0+1],OQQCC0C0Q0[8+O000C0C0Q0],OCCQOOC0Q0.OQOC0OOOQ0);
SetLength(O0CCC0C0Q0,0);
OCOCC0C0Q0:=OOCQCC0OQ0.OOOCCC0OQ0(O0OCC0C0Q0);
try
O0CCC0C0Q0:=OCOCC0C0Q0.ComputeHash(OQQCC0C0Q0);
finally
OCOCC0C0Q0.Free;
end;
if MemCompare(@OCQCC0C0Q0[0],@O0CCC0C0Q0[0],O000C0C0Q0)<>0 then
Exit;
Result:=True;
end;
OO0Q0OOOQ0:begin
Result:=(Length(OQ0CC0C0Q0)=OQOCC0C0Q0)and(MemCompare(@OQ0CC0C0Q0[0],@OC0CC0C0Q0[0],OQOCC0C0Q0)=0);
end;
else
raise EScError.Create(seInvalidInputArgs);
end;
end;
function O0CQOOC0Q0.OQO0C0C0Q0(const OCO0C0C0Q0:TBytes;O0Q0C0C0Q0:OQ0Q0OOOQ0):TBytes;
var
OOQ0C0C0Q0:integer;
OQQ0C0C0Q0:TBytes;
OCQ0C0C0Q0,O0C0C0C0Q0:TBytes;
OOC0C0C0Q0,OQC0C0C0Q0,OCC0C0C0Q0,O00OC0C0Q0:TBytes;
OO0OC0C0Q0,OQ0OC0C0Q0:integer;
OC0OC0C0Q0:integer;
begin
OOQ0C0C0Q0:=Length(OCO0C0C0Q0);
case O0Q0C0C0Q0 of
OQCOOOOOQ0:begin
if(OOQ0C0C0Q0<=11)or(OCO0C0C0Q0[0]<>0)or(OCO0C0C0Q0[1]<>2)then
raise EScError.Create(seIPCorruptData);
OC0OC0C0Q0:=2;
while OC0OC0C0Q0<OOQ0C0C0Q0 do begin
if OCO0C0C0Q0[OC0OC0C0Q0]=0 then
break;
Inc(OC0OC0C0Q0);
end;
if OC0OC0C0Q0=OOQ0C0C0Q0 then
raise EScError.Create(seIPCorruptData);
Inc(OC0OC0C0Q0);
SetLength(Result,OOQ0C0C0Q0-OC0OC0C0Q0);
Move(OCO0C0C0Q0[OC0OC0C0Q0],Result[0],Length(Result));
end;
OCCOOOOOQ0:begin
if(OOQ0C0C0Q0<=(2+2*OOCQCC0OQ0.OC00CC0OQ0(O00COOC0Q0.O0O00OOOQ0)))or
(OCO0C0C0Q0[0]<>0)then
raise EScError.Create(seIPCorruptData);
SetLength(OQQ0C0C0Q0,0);
OQQ0C0C0Q0:=O00COOC0Q0.OQ000OOOQ0;
OO0OC0C0Q0:=Length(OQQ0C0C0Q0);
OQ0OC0C0Q0:=OOQ0C0C0Q0-OO0OC0C0Q0-1;
SetLength(OCQ0C0C0Q0,OO0OC0C0Q0);
Move(OCO0C0C0Q0[1],OCQ0C0C0Q0[0],OO0OC0C0Q0);
SetLength(O0C0C0C0Q0,OQ0OC0C0Q0);
Move(OCO0C0C0Q0[1+Length(OCQ0C0C0Q0)],O0C0C0C0Q0[0],OQ0OC0C0Q0);
SetLength(OOC0C0C0Q0,0);
OOC0C0C0Q0:=OO0000C0Q0(O0C0C0C0Q0,OO0OC0C0Q0,O00COOC0Q0.OOO00OOOQ0);
SetLength(OCC0C0C0Q0,OO0OC0C0Q0);
for OC0OC0C0Q0:=0 to OO0OC0C0Q0-1 do
OCC0C0C0Q0[OC0OC0C0Q0]:=OCQ0C0C0Q0[OC0OC0C0Q0]xor OOC0C0C0Q0[OC0OC0C0Q0];
SetLength(OQC0C0C0Q0,0);
OQC0C0C0Q0:=OO0000C0Q0(OCC0C0C0Q0,OQ0OC0C0Q0,O00COOC0Q0.OOO00OOOQ0);
SetLength(O00OC0C0Q0,OQ0OC0C0Q0);
for OC0OC0C0Q0:=0 to OQ0OC0C0Q0-1 do
O00OC0C0Q0[OC0OC0C0Q0]:=O0C0C0C0Q0[OC0OC0C0Q0]xor OQC0C0C0Q0[OC0OC0C0Q0];
if(OO0OC0C0Q0>=OQ0OC0C0Q0)or(MemCompare(@OQQ0C0C0Q0[0],@O00OC0C0Q0[0],OO0OC0C0Q0)<>0)then
raise EScError.Create(seIPCorruptData);
OC0OC0C0Q0:=OO0OC0C0Q0;
while OC0OC0C0Q0<OQ0OC0C0Q0 do begin
if O00OC0C0Q0[OC0OC0C0Q0]=1 then
break;
if O00OC0C0Q0[OC0OC0C0Q0]<>0 then
raise EScError.Create(seIPCorruptData);
Inc(OC0OC0C0Q0);
end;
if OC0OC0C0Q0=OQ0OC0C0Q0 then
raise EScError.Create(seIPCorruptData);
Inc(OC0OC0C0Q0);
SetLength(Result,OQ0OC0C0Q0-OC0OC0C0Q0);
Move(O00OC0C0Q0[OC0OC0C0Q0],Result[0],Length(Result));
end;
OO0Q0OOOQ0:begin
Result:=OCO0C0C0Q0;
end;
else
raise EScError.Create(seInvalidInputArgs);
end;
end;
function O0CQOOC0Q0.O00C0OC0Q0(const OO0C0OC0Q0:TBytes):TBytes;
var
OQ0C0OC0Q0,OC0C0OC0Q0,O0OC0OC0Q0,OOOC0OC0Q0,OQOC0OC0Q0,OCOC0OC0Q0:TBigInteger;
begin
OQOC0OC0Q0:=nil;
OC0C0OC0Q0:=nil;
O0OC0OC0Q0:=nil;
OOOC0OC0Q0:=nil;
OQ0C0OC0Q0:=TBigInteger.Create(OO0C0OC0Q0);
try
OC0C0OC0Q0:=OQ0C0OC0Q0.ModPow(OQ0OQ0C0Q0.O0Q00O0OQ0,OQ0OQ0C0Q0.OOO00O0OQ0);
O0OC0OC0Q0:=OQ0C0OC0Q0.ModPow(OQ0OQ0C0Q0.OOQ00O0OQ0,OQ0OQ0C0Q0.OQO00O0OQ0);
OOOC0OC0Q0:=OC0C0OC0Q0.Minus(O0OC0OC0Q0);
if OOOC0OC0Q0.IsNegative then begin
OCOC0OC0Q0:=OOOC0OC0Q0;
OOOC0OC0Q0:=OOOC0OC0Q0.Add(OQ0OQ0C0Q0.OOO00O0OQ0);
OCOC0OC0Q0.Free;
end;
OQOC0OC0Q0:=OQ0OQ0C0Q0.OCO00O0OQ0.Mul(OOOC0OC0Q0);
OCOC0OC0Q0:=OQOC0OC0Q0;
OQOC0OC0Q0:=OQOC0OC0Q0.Mod_(OQ0OQ0C0Q0.OOO00O0OQ0);
OCOC0OC0Q0.Free;
OCOC0OC0Q0:=OQOC0OC0Q0;
OQOC0OC0Q0:=OQOC0OC0Q0.Mul(OQ0OQ0C0Q0.OQO00O0OQ0);
OCOC0OC0Q0.Free;
OCOC0OC0Q0:=OQOC0OC0Q0;
OQOC0OC0Q0:=OQOC0OC0Q0.Add(O0OC0OC0Q0);
OCOC0OC0Q0.Free;
Result:=OQOC0OC0Q0.GetBytes(OCO0Q0C0Q0);
finally
OQ0C0OC0Q0.Free;
OQOC0OC0Q0.Free;
OC0C0OC0Q0.Free;
O0OC0OC0Q0.Free;
OOOC0OC0Q0.Free;
end;
end;
function O0CQOOC0Q0.O0QC0OC0Q0(const OOQC0OC0Q0:TBytes):TBytes;
var
OQQC0OC0Q0,OCQC0OC0Q0:TBigInteger;
begin
OCQC0OC0Q0:=nil;
OQQC0OC0Q0:=TBigInteger.Create(OOQC0OC0Q0);
try
OCQC0OC0Q0:=OQQC0OC0Q0.ModPow(OQ0OQ0C0Q0.OQQ00O0OQ0,OQ0OQ0C0Q0.OCQ00O0OQ0);
Result:=OCQC0OC0Q0.GetBytes(OCO0Q0C0Q0);
finally
OQQC0OC0Q0.Free;
OCQC0OC0Q0.Free;
end;
end;
procedure O0CQOOC0Q0.O0CC0OC0Q0(const OOCC0OC0Q0:OOOOCOQOQ0;out OQCC0OC0Q0:PByteArray;out OCCC0OC0Q0:integer);
begin
case OOCC0OC0Q0 of
O0C0COQOQ0:begin
OCCC0OC0Q0:=0;
OQCC0OC0Q0:=nil;
end;
O0OOCOQOQ0:begin
OCCC0OC0Q0:=Length(MD2_ASN_ID);
OQCC0OC0Q0:=@MD2_ASN_ID;
end;
OC0OCOQOQ0:begin
OCCC0OC0Q0:=Length(MD4_ASN_ID);
OQCC0OC0Q0:=@MD4_ASN_ID;
end;
OQ0OCOQOQ0:begin
OCCC0OC0Q0:=Length(MD5_ASN_ID);
OQCC0OC0Q0:=@MD5_ASN_ID;
end;
OOC0COQOQ0:begin
OCCC0OC0Q0:=Length(SHA1_ASN_ID);
OQCC0OC0Q0:=@SHA1_ASN_ID;
end;
OQC0COQOQ0:begin
OCCC0OC0Q0:=Length(SHA256_ASN_ID);
OQCC0OC0Q0:=@SHA256_ASN_ID;
end;
OCC0COQOQ0:begin
OCCC0OC0Q0:=Length(SHA512_ASN_ID);
OQCC0OC0Q0:=@SHA512_ASN_ID;
end;
O00OCOQOQ0:begin
OCCC0OC0Q0:=Length(SHA224_ASN_ID);
OQCC0OC0Q0:=@SHA224_ASN_ID;
end;
OO0OCOQOQ0:begin
OCCC0OC0Q0:=Length(SHA384_ASN_ID);
OQCC0OC0Q0:=@SHA384_ASN_ID;
end;
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
end;
function O0CQOOC0Q0.O0OOC0C0Q0(const OOOOC0C0Q0:TBytes):TBytes;
var
OQOOC0C0Q0,OCOOC0C0Q0,O0QOC0C0Q0,OOQOC0C0Q0,OQQOC0C0Q0:TBigInteger;
OCQOC0C0Q0,O0COC0C0Q0:TBytes;
begin
OCOOC0C0Q0:=nil;
O0QOC0C0Q0:=nil;
OOQOC0C0Q0:=nil;
OQOOC0C0Q0:=TBigInteger.Create(OOOOC0C0Q0);
try
OCOOC0C0Q0:=OO0OQ0C0Q0.OQ000O0OQ0.Mod_(OO0OQ0C0Q0.OO000O0OQ0);
OOQOC0C0Q0:=OO0OQ0C0Q0.OQCC0O0OQ0.Mul(OCOOC0C0Q0);
OQQOC0C0Q0:=OOQOC0C0Q0;
OOQOC0C0Q0:=OQOOC0C0Q0.Add(OOQOC0C0Q0);
OQQOC0C0Q0.Free;
O0QOC0C0Q0:=OO0OQ0C0Q0.OQCC0O0OQ0.ModInverse(OO0OQ0C0Q0.OO000O0OQ0);
OQQOC0C0Q0:=O0QOC0C0Q0;
O0QOC0C0Q0:=O0QOC0C0Q0.Mul(OOQOC0C0Q0);
OQQOC0C0Q0.Free;
OQQOC0C0Q0:=O0QOC0C0Q0;
O0QOC0C0Q0:=O0QOC0C0Q0.Mod_(OO0OQ0C0Q0.OO000O0OQ0);
OQQOC0C0Q0.Free;
OCQOC0C0Q0:=OCOOC0C0Q0.GetBytes;
O0COC0C0Q0:=O0QOC0C0Q0.GetBytes;
SetLength(Result,Length(OOOOC0C0Q0)*2);
Move(OCQOC0C0Q0[0],Result[Length(OOOOC0C0Q0)-Length(OCQOC0C0Q0)],Length(OCQOC0C0Q0));
Move(O0COC0C0Q0[0],Result[Length(OOOOC0C0Q0)*2-Length(O0COC0C0Q0)],Length(O0COC0C0Q0));
finally
OQOOC0C0Q0.Free;
OCOOC0C0Q0.Free;
O0QOC0C0Q0.Free;
OOQOC0C0Q0.Free;
end;
end;
function O0CQOOC0Q0.OOCOC0C0Q0(const OQCOC0C0Q0,OCCOC0C0Q0:TBytes):boolean;
var
O00QQ0C0Q0,OO0QQ0C0Q0,OQ0QQ0C0Q0,OC0QQ0C0Q0,O0OQQ0C0Q0,OOOQQ0C0Q0,OQOQQ0C0Q0,OCOQQ0C0Q0:TBigInteger;
O0QQQ0C0Q0,OOQQQ0C0Q0:TBytes;
begin
OO0QQ0C0Q0:=nil;
OQ0QQ0C0Q0:=nil;
OC0QQ0C0Q0:=nil;
O0OQQ0C0Q0:=nil;
OOOQQ0C0Q0:=nil;
OQOQQ0C0Q0:=nil;
O00QQ0C0Q0:=TBigInteger.Create(OQCOC0C0Q0);
try
SetLength(O0QQQ0C0Q0,Length(OCCOC0C0Q0)div 2);
SetLength(OOQQQ0C0Q0,Length(OCCOC0C0Q0)div 2);
Move(OCCOC0C0Q0[0],O0QQQ0C0Q0[0],Length(O0QQQ0C0Q0));
Move(OCCOC0C0Q0[Length(O0QQQ0C0Q0)],OOQQQ0C0Q0[0],Length(OOQQQ0C0Q0));
OO0QQ0C0Q0:=TBigInteger.Create(O0QQQ0C0Q0);
OQ0QQ0C0Q0:=TBigInteger.Create(OOQQQ0C0Q0);
OC0QQ0C0Q0:=OQ0QQ0C0Q0.ModInverse(OO0OQ0C0Q0.OO000O0OQ0);
O0OQQ0C0Q0:=O00QQ0C0Q0.Mul(OC0QQ0C0Q0);
OCOQQ0C0Q0:=O0OQQ0C0Q0;
O0OQQ0C0Q0:=O0OQQ0C0Q0.Mod_(OO0OQ0C0Q0.OO000O0OQ0);
OCOQQ0C0Q0.Free;
OOOQQ0C0Q0:=OO0QQ0C0Q0.Mul(OC0QQ0C0Q0);
OCOQQ0C0Q0:=OOOQQ0C0Q0;
OOOQQ0C0Q0:=OOOQQ0C0Q0.Mod_(OO0OQ0C0Q0.OO000O0OQ0);
OCOQQ0C0Q0.Free;
OCOQQ0C0Q0:=O0OQQ0C0Q0;
O0OQQ0C0Q0:=OO0OQ0C0Q0.OCCC0O0OQ0.modPow(O0OQQ0C0Q0,OO0OQ0C0Q0.O0000O0OQ0);
OCOQQ0C0Q0.Free;
OCOQQ0C0Q0:=OOOQQ0C0Q0;
OOOQQ0C0Q0:=OO0OQ0C0Q0.OQ000O0OQ0.modPow(OOOQQ0C0Q0,OO0OQ0C0Q0.O0000O0OQ0);
OCOQQ0C0Q0.Free;
OQOQQ0C0Q0:=O0OQQ0C0Q0.Mul(OOOQQ0C0Q0);
OCOQQ0C0Q0:=OQOQQ0C0Q0;
OQOQQ0C0Q0:=OQOQQ0C0Q0.Mod_(OO0OQ0C0Q0.O0000O0OQ0);
OCOQQ0C0Q0.Free;
OCOQQ0C0Q0:=OQOQQ0C0Q0;
OQOQQ0C0Q0:=OQOQQ0C0Q0.Mod_(OO0OQ0C0Q0.OO000O0OQ0);
OCOQQ0C0Q0.Free;
Result:=OQOQQ0C0Q0.Equal(OO0QQ0C0Q0);
finally
O00QQ0C0Q0.Free;
OO0QQ0C0Q0.Free;
OQ0QQ0C0Q0.Free;
OC0QQ0C0Q0.Free;
O0OQQ0C0Q0.Free;
OOOQQ0C0Q0.Free;
OQOQQ0C0Q0.Free;
end;
end;
function O0CQOOC0Q0.OQQQQ0C0Q0(const OCQQQ0C0Q0:TBytes;O0CQQ0C0Q0:OOOOCOQOQ0;OOCQQ0C0Q0:OQ0Q0OOOQ0):TBytes;
var
OQCQQ0C0Q0:TBytes;
begin
if not(OOCQQ0C0Q0 in[OOCOOOOOQ0,O00Q0OOOQ0])then
raise EScError.Create(seImproperPaddingMode);
OQCQQ0C0Q0:=O0CO00C0Q0(OCQQQ0C0Q0,O0CQQ0C0Q0,OOCQQ0C0Q0);
Result:=O00C0OC0Q0(OQCQQ0C0Q0);
if not OCCQQ0C0Q0(OCQQQ0C0Q0,Result,O0CQQ0C0Q0,OOCQQ0C0Q0)then
raise EScError.Create(seErrorSigningData);
end;
function O0CQOOC0Q0.OCCQQ0C0Q0(const O00CQ0C0Q0,OO0CQ0C0Q0:TBytes;OQ0CQ0C0Q0:OOOOCOQOQ0;OC0CQ0C0Q0:OQ0Q0OOOQ0):boolean;
var
O0OCQ0C0Q0:TBytes;
begin
if not(OC0CQ0C0Q0 in[OOCOOOOOQ0,O00Q0OOOQ0])then
raise EScError.Create(seImproperPaddingMode);
O0OCQ0C0Q0:=O0QC0OC0Q0(OO0CQ0C0Q0);
try
Result:=OO0CC0C0Q0(O00CQ0C0Q0,O0OCQ0C0Q0,OQ0CQ0C0Q0,OC0CQ0C0Q0);
except
Result:=False;
end;
end;
function O0CQOOC0Q0.OOOCQ0C0Q0(const OQOCQ0C0Q0:TBytes):TBytes;
begin
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
Result:=OC0OQ0C0Q0.OOQQQO0OQ0.OOC0CO0OQ0(OQOCQ0C0Q0,OC0OQ0C0Q0.OCQQQO0OQ0,OC0OQ0C0Q0.OQQQQO0OQ0);
end;
function O0CQOOC0Q0.OCOCQ0C0Q0(const O0QCQ0C0Q0,OOQCQ0C0Q0:TBytes):boolean;
begin
Assert(OC0OQ0C0Q0.OOQQQO0OQ0<>nil);
Result:=OC0OQ0C0Q0.OOQQQO0OQ0.OO0OCO0OQ0(O0QCQ0C0Q0,OOQCQ0C0Q0,OC0OQ0C0Q0.OQQQQO0OQ0);
end;
function O0CQOOC0Q0.O0Q0OCQ0Q0(const OOOCCQC0Q0:TBytes;OQOCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OCOCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):TBytes;
var
OOQ0OCQ0Q0:THashAlgorithm;
OQQ0OCQ0Q0:TBytes;
begin
if OQOCCQC0Q0=O0C0COQOQ0 then
OQQ0OCQ0Q0:=OOOCCQC0Q0
else begin
OOQ0OCQ0Q0:=OOCQCC0OQ0.OOOCCC0OQ0(OQOCCQC0Q0);
try
OQQ0OCQ0Q0:=OOQ0OCQ0Q0.ComputeHash(OOOCCQC0Q0);
finally
OOQ0OCQ0Q0.Free;
end;
end;
Result:=OCQ0OCQ0Q0(OQQ0OCQ0Q0,OQOCCQC0Q0,OCOCCQC0Q0);
end;
function O0CQOOC0Q0.OCQ0OCQ0Q0(const OQC0CO0OQ0:TBytes;O0QCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OOQCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):TBytes;
begin
OQOC0CC0Q0;
if not O0QOOCQ0Q0 then
raise EScError.Create(seCannotSignData);
O0QOOCOOQ0(OQOQ0CC0Q0);
try
case OQCQOOC0Q0 of
OCOQQOQOQ0:
Result:=OQQQQ0C0Q0(OQC0CO0OQ0,O0QCCQC0Q0,OOQCCQC0Q0);
OQOQQOQOQ0:
Result:=O0OOC0C0Q0(OQC0CO0OQ0);
O0QQQOQOQ0:
Result:=OOOCQ0C0Q0(OQC0CO0OQ0);
else
Assert(False);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function O0CQOOC0Q0.O0C0OCQ0Q0(const OQQCCQC0Q0,OCQCCQC0Q0:TBytes;O0CCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OOCCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):boolean;
var
OOC0OCQ0Q0:THashAlgorithm;
OQC0OCQ0Q0:TBytes;
begin
if O0CCCQC0Q0=O0C0COQOQ0 then
OQC0OCQ0Q0:=OQQCCQC0Q0
else begin
OOC0OCQ0Q0:=OOCQCC0OQ0.OOOCCC0OQ0(O0CCCQC0Q0);
try
OQC0OCQ0Q0:=OOC0OCQ0Q0.ComputeHash(OQQCCQC0Q0);
finally
OOC0OCQ0Q0.Free;
end;
end;
Result:=OCC0OCQ0Q0(OQC0OCQ0Q0,OCQCCQC0Q0,O0CCCQC0Q0,OOCCCQC0Q0);
end;
function O0CQOOC0Q0.OCC0OCQ0Q0(const OQCCCQC0Q0,OCCCCQC0Q0:TBytes;O000CQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OO00CQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):boolean;
begin
OQOC0CC0Q0;
O0QOOCOOQ0(OQOQ0CC0Q0);
try
case OQCQOOC0Q0 of
OCOQQOQOQ0:
Result:=OCCQQ0C0Q0(OQCCCQC0Q0,OCCCCQC0Q0,O000CQC0Q0,OO00CQC0Q0);
OQOQQOQOQ0:
Result:=OOCOC0C0Q0(OQCCCQC0Q0,OCCCCQC0Q0);
O0QQQOQOQ0:
Result:=OCOCQ0C0Q0(OQCCCQC0Q0,OCCCCQC0Q0);
else
Result:=False;
Assert(False);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function O0CQOOC0Q0.O00OOCQ0Q0(const OQ00CQC0Q0:TBytes;OO0OOCQ0Q0:OQ0Q0OOOQ0=OQCOOOOOQ0):TBytes;
var
OQ0OOCQ0Q0:TBytes;
begin
OQOC0CC0Q0;
if OQCQOOC0Q0=OQOQQOQOQ0 then
raise EScError.Create(seDSACannotEncrypt);
if OQCQOOC0Q0=O0QQQOQOQ0 then
raise EScError.Create(seECCannotEncrypt);
if not(OO0OOCQ0Q0 in[OO0Q0OOOQ0,OQCOOOOOQ0,OCCOOOOOQ0])then
raise EScError.Create(seImproperPaddingMode);
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OQ0OOCQ0Q0:=O0C000C0Q0(OQ00CQC0Q0,OO0OOCQ0Q0);
Result:=O0QC0OC0Q0(OQ0OOCQ0Q0);
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function O0CQOOC0Q0.OC0OOCQ0Q0(const OC00CQC0Q0:TBytes;O0OOOCQ0Q0:OQ0Q0OOOQ0=OQCOOOOOQ0):TBytes;
var
OOOOOCQ0Q0:TBytes;
begin
OQOC0CC0Q0;
if OQCQOOC0Q0=OQOQQOQOQ0 then
raise EScError.Create(seDSACannotEncrypt);
if OQCQOOC0Q0=O0QQQOQOQ0 then
raise EScError.Create(seECCannotEncrypt);
if not O0QOOCQ0Q0 then
raise EScError.Create(seCannotDecryptData);
if not(O0OOOCQ0Q0 in[OO0Q0OOOQ0,OQCOOOOOQ0,OCCOOOOOQ0])then
raise EScError.Create(seImproperPaddingMode);
if Length(OC00CQC0Q0)<>OCO0Q0C0Q0 then
raise EScError.CreateFmt(SErrorEncryptingData,[SInvalidLength],seErrorEncryptingData);
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OOOOOCQ0Q0:=O00C0OC0Q0(OC00CQC0Q0);
Result:=OQO0C0C0Q0(OOOOOCQ0Q0,O0OOOCQ0Q0);
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function O0CQOOC0Q0.OQQ0Q0C0Q0:TBytes;
begin
OQOC0CC0Q0;
Result:=OQ0CO0C0Q0;
end;
procedure O0CQOOC0Q0.OCQ0Q0C0Q0(const O0C0Q0C0Q0:TBytes);
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OQCC0CC0Q0:=False;
O00OCOC0Q0(O0C0Q0C0Q0);
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
OO0C0CC0Q0;
end;
function O0CQOOC0Q0.OQO0Q0C0Q0:OOQQQOQOQ0;
begin
Result:=OQCQOOC0Q0;
end;
function O0CQOOC0Q0.OCO0Q0C0Q0:integer;
begin
if not OQCC0CC0Q0 then
Result:=0
else
case OQCQOOC0Q0 of
OCOQQOQOQ0:
Result:=((OQ0OQ0C0Q0.OCQ00O0OQ0.BitCount+7)shr 3);
OQOQQOQOQ0:
Result:=((OO0OQ0C0Q0.O0000O0OQ0.BitCount+7)shr 3);
O0QQQOQOQ0:
Result:=OC0OQ0C0Q0.OOQQQO0OQ0.OC0CCO0OQ0;
else
Result:=0;
Assert(False);
end;
end;
function O0CQOOC0Q0.O0Q0Q0C0Q0:integer;
begin
if not OQCC0CC0Q0 then
Result:=0
else
case OQCQOOC0Q0 of
OCOQQOQOQ0:
Result:=((OQ0OQ0C0Q0.OCQ00O0OQ0.BitCount+7)shr 3)shl 3;
OQOQQOQOQ0:
Result:=((OO0OQ0C0Q0.O0000O0OQ0.BitCount+7)shr 3)shl 3;
O0QQQOQOQ0:
Result:=OC0OQ0C0Q0.OOQQQO0OQ0.OQOOCO0OQ0;
else
Result:=0;
Assert(False);
end;
end;
function O0CQOOC0Q0.OOQ0Q0C0Q0:boolean;
begin
if not OQCC0CC0Q0 then
Result:=False
else
case OQCQOOC0Q0 of
OCOQQOQOQ0:begin
Result:=OQ0OQ0C0Q0.O0O00O0OQ0<>nil;
if Result then begin
Assert(OQ0OQ0C0Q0.OOO00O0OQ0<>nil);
Assert(OQ0OQ0C0Q0.OQO00O0OQ0<>nil);
Assert(OQ0OQ0C0Q0.OCO00O0OQ0<>nil);
Assert(OQ0OQ0C0Q0.O0Q00O0OQ0<>nil);
Assert(OQ0OQ0C0Q0.OOQ00O0OQ0<>nil);
end;
end;
OQOQQOQOQ0:begin
Result:=OO0OQ0C0Q0.OQCC0O0OQ0<>nil;
end;
O0QQQOQOQ0:begin
Result:=OC0OQ0C0Q0.OCQQQO0OQ0<>nil;
end;
else
Result:=False;
Assert(False);
end;
end;
procedure O0CQOOC0Q0.OOC0Q0C0Q0(OQC0Q0C0Q0:OC0Q0OOOQ0);
begin
OCCQOOC0Q0.OQQQ0OOOQ0(OQC0Q0C0Q0);
end;
procedure O0CQOOC0Q0.OCC0Q0C0Q0(O00OQ0C0Q0:OCOC0OOOQ0);
begin
O00COOC0Q0.OCCC0OOOQ0(O00OQ0C0Q0);
end;
procedure O0CQOOC0Q0.OQCQ0CC0Q0(OCCQ0CC0Q0:O0000CC0Q0);
var
O0QOQ0C0Q0:integer;
begin
if OOOQ0CC0Q0<>OCCQ0CC0Q0 then
if OOCQOOC0Q0<>nil then begin
for O0QOQ0C0Q0:=0 to OOCQOOC0Q0.Count-1 do begin
if IsClass(TObject(OOCQOOC0Q0[O0QOQ0C0Q0]),TScCertificate)then
raise EScError.Create(seCertStorageCannotBeChanged);
end;
end;
inherited;
end;
function O0CQOOC0Q0.OO0COOC0Q0:OQCCCCC0Q0;
begin
Result:=OQCCCCC0Q0(OOCC0CC0Q0);
end;
procedure O0CQOOC0Q0.OQ0COOC0Q0(OC0COOC0Q0:OQCCCCC0Q0);
begin
OOCC0CC0Q0:=OC0COOC0Q0;
end;
constructor O0OQ0CC0Q0.Create(OOQC0CC0Q0:O0000CC0Q0=nil);
begin
inherited Create;
O0QQ0CC0Q0:=False;
OQOQ0CC0Q0:=TCriticalSection.Create;
OOOQ0CC0Q0:=OOQC0CC0Q0;
if OOOQ0CC0Q0<>nil then
OOOQ0CC0Q0.OC000CC0Q0.AddObject('',Self);
end;
destructor O0OQ0CC0Q0.Destroy;
begin
O0OC0CC0Q0;
OOCC0CC0Q0:=nil;
OQOQ0CC0Q0.Free;
inherited;
end;
procedure O0OQ0CC0Q0.O0OC0CC0Q0;
begin
end;
procedure O0OQ0CC0Q0.OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);
begin
end;
procedure O0OQ0CC0Q0.Assign(O000CO0OQ0:TPersistent);
begin
if IsClass(O000CO0OQ0,ClassType)then begin
OQCC0CC0Q0:=False;
OQ0C0CC0Q0(O0OQ0CC0Q0(O000CO0OQ0));
OO0C0CC0Q0;
end
else
inherited Assign(O000CO0OQ0);
end;
function O0OQ0CC0Q0.Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;
begin
Result:=IsClass(O0CC0CC0Q0,ClassType);
end;
function O0OQ0CC0Q0.OOOC0CC0Q0:O0OQ0CC0Q0;
begin
Result:=OCCC0CC0Q0(ClassType).Create(nil);
Result.OCOQ0CC0Q0:=OCOQ0CC0Q0;
Result.OQ0C0CC0Q0(Self);
end;
procedure O0OQ0CC0Q0.OQOC0CC0Q0;
begin
OQCC0CC0Q0:=True;
if not OQCC0CC0Q0 then
raise EScError.CreateFmt(SObjNotReady,[ClassName],seObjNotReady);
end;
procedure O0OQ0CC0Q0.OQQQ0CC0Q0(const OCQQ0CC0Q0:boolean);
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
if O0QQ0CC0Q0<>OCQQ0CC0Q0 then begin
if OCQQ0CC0Q0 then begin
if OOOQ0CC0Q0=nil then
raise EScError.Create(seStorageNoSet)
else begin
OOOQ0CC0Q0.OQQ00CC0Q0(Self);
if not O0QQ0CC0Q0 then
raise EScError.Create(seStorageNoSet);
end;
end
else begin
O0OC0CC0Q0;
O0QQ0CC0Q0:=False;
end;
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
procedure O0OQ0CC0Q0.OO0C0CC0Q0;
begin
if not OOQQ0CC0Q0 and OQCC0CC0Q0 and(OOCC0CC0Q0<>nil)and(OCOC0CC0Q0<>'')then
OOCC0CC0Q0.O0Q00CC0Q0(Self);
end;
procedure O0OQ0CC0Q0.O0CQ0CC0Q0(const OOCQ0CC0Q0:string);
begin
if OCOQ0CC0Q0<>OOCQ0CC0Q0 then begin
if OOOQ0CC0Q0<>nil then
OOOQ0CC0Q0.OCQO0CC0Q0(Self,OOCQ0CC0Q0);
OCOQ0CC0Q0:=OOCQ0CC0Q0;
end;
end;
procedure O0OQ0CC0Q0.OQCQ0CC0Q0(OCCQ0CC0Q0:O0000CC0Q0);
var
O00C0CC0Q0:integer;
begin
if OOOQ0CC0Q0<>OCCQ0CC0Q0 then begin
if OCCQ0CC0Q0<>nil then
OCCQ0CC0Q0.OQCQCCC0Q0(Self)
else begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
if OOOQ0CC0Q0<>nil then begin
O0QOOCOOQ0(OOOQ0CC0Q0.OQ000CC0Q0);
try
O00C0CC0Q0:=OOOQ0CC0Q0.OO0CCCC0Q0(Self);
if O00C0CC0Q0>-1 then
OOOQ0CC0Q0.OC000CC0Q0.Objects[O00C0CC0Q0]:=nil;
finally
OQQOOCOOQ0(OOOQ0CC0Q0.OQ000CC0Q0);
end;
end;
OOOQ0CC0Q0:=nil;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
end;
end;
constructor O0000CC0Q0.Create(O0CQCCC0Q0:OCCCQ00OQ0);
begin
inherited Create;
OO000CC0Q0:=O0CQCCC0Q0;
OQ000CC0Q0:=TCriticalSection.Create;
OC000CC0Q0:=TStringList.Create;
end;
destructor O0000CC0Q0.Destroy;
begin
OOO00CC0Q0;
OC000CC0Q0.Free;
OQ000CC0Q0.Free;
inherited;
end;
procedure O0000CC0Q0.OOO00CC0Q0;
begin
O0QOOCOOQ0(OQ000CC0Q0);
try
while OC000CC0Q0.Count>0 do begin
{$IFNDEF AUTOREFCOUNT}
OC000CC0Q0.Objects[OC000CC0Q0.Count-1].Free;
{$ELSE}
OC000CC0Q0.Objects[OC000CC0Q0.Count-1]:=nil;
{$ENDIF}
OC000CC0Q0.Delete(OC000CC0Q0.Count-1);
end;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
function O0000CC0Q0.O0O00CC0Q0:integer;
begin
O0QOOCOOQ0(OQ000CC0Q0);
try
Result:=OC000CC0Q0.Count;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
procedure O0000CC0Q0.OQCQCCC0Q0(OCCQCCC0Q0:O0OQ0CC0Q0);
var
O00CCCC0Q0:integer;
begin
if not IsClass(OCCQCCC0Q0,OQO00CC0Q0)then
raise EScError.CreateFmt(SInvalidObjectClass,[OQO00CC0Q0.ClassName,OCCQCCC0Q0.ClassName],seInvalidObjectClass);
O0QOOCOOQ0(OQ000CC0Q0);
try
O0OO0CC0Q0(OCCQCCC0Q0.OCOC0CC0Q0);
OCCQCCC0Q0.OQCC0CC0Q0:=True;
O0Q00CC0Q0(OCCQCCC0Q0);
O0QOOCOOQ0(OCCQCCC0Q0.OQOQ0CC0Q0);
try
if OCCQCCC0Q0.OOOQ0CC0Q0<>nil then begin
O0QOOCOOQ0(OCCQCCC0Q0.OOOQ0CC0Q0.OQ000CC0Q0);
try
O00CCCC0Q0:=OCCQCCC0Q0.OOOQ0CC0Q0.OO0CCCC0Q0(OCCQCCC0Q0);
if O00CCCC0Q0>-1 then
OCCQCCC0Q0.OOOQ0CC0Q0.OC000CC0Q0.Objects[O00CCCC0Q0]:=OCCQCCC0Q0.OOOC0CC0Q0;
finally
OQQOOCOOQ0(OCCQCCC0Q0.OOOQ0CC0Q0.OQ000CC0Q0);
end;
end;
OCCQCCC0Q0.OOOQ0CC0Q0:=Self;
finally
OQQOOCOOQ0(OCCQCCC0Q0.OQOQ0CC0Q0);
end;
OC000CC0Q0.AddObject(OCCQCCC0Q0.OCOC0CC0Q0,OCCQCCC0Q0);
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
procedure O0000CC0Q0.O0OO0CC0Q0(const OOOO0CC0Q0:string);
begin
if(OOOO0CC0Q0='')and(OO000CC0Q0<>nil)then
raise EScError.Create(seObjNameMissing);
if OCCO0CC0Q0(OOOO0CC0Q0)<>nil then
raise EScError.Create(seDuplicateObjName);
end;
function O0000CC0Q0.OQOO0CC0Q0(OCOO0CC0Q0:integer):O0OQ0CC0Q0;
begin
O0QOOCOOQ0(OQ000CC0Q0);
try
Result:=O0OQ0CC0Q0(OC000CC0Q0.Objects[OCOO0CC0Q0]);
if Result=nil then begin
Result:=OQO00CC0Q0.Create(nil);
Result.OOOQ0CC0Q0:=Self;
Result.OCOQ0CC0Q0:=OC000CC0Q0[OCOO0CC0Q0];
OC000CC0Q0.Objects[OCOO0CC0Q0]:=Result;
OQQ00CC0Q0(Result);
end
else
Result.OOOQ0CC0Q0:=Self;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
procedure O0000CC0Q0.O0QO0CC0Q0(OOQO0CC0Q0:integer;OQQO0CC0Q0:O0OQ0CC0Q0);
begin
if not IsClass(OQQO0CC0Q0,OQO00CC0Q0)then
raise EScError.CreateFmt(SInvalidObjectClass,[OQO00CC0Q0.ClassName,OQQO0CC0Q0.ClassName],seInvalidObjectClass);
OQOO0CC0Q0(OOQO0CC0Q0).Assign(OQQO0CC0Q0);
end;
procedure O0000CC0Q0.OCQO0CC0Q0(O0CO0CC0Q0:O0OQ0CC0Q0;const OOCO0CC0Q0:string);
var
OQCO0CC0Q0:integer;
begin
if O0CO0CC0Q0=nil then
raise EScError.Create(seInvalidInputArgs);
O0QOOCOOQ0(OQ000CC0Q0);
try
O0OO0CC0Q0(OOCO0CC0Q0);
OQCO0CC0Q0:=OC000CC0Q0.IndexOfObject(O0CO0CC0Q0);
if OQCO0CC0Q0<0 then begin
if O0CO0CC0Q0.OQCC0CC0Q0 then begin
O0CO0CC0Q0.OCOQ0CC0Q0:=OOCO0CC0Q0;
O0Q00CC0Q0(O0CO0CC0Q0);
end;
OC000CC0Q0.AddObject(OOCO0CC0Q0,O0CO0CC0Q0);
end
else begin
if O0CO0CC0Q0.OCOQ0CC0Q0<>'' then begin
if O0CO0CC0Q0.OQCC0CC0Q0 then
OO0O0CC0Q0(O0CO0CC0Q0,OOCO0CC0Q0)
else
OQC00CC0Q0(O0CO0CC0Q0,OOCO0CC0Q0);
end
else
if O0CO0CC0Q0.OQCC0CC0Q0 then begin
O0CO0CC0Q0.OCOQ0CC0Q0:=OOCO0CC0Q0;
O0Q00CC0Q0(O0CO0CC0Q0);
end;
OC000CC0Q0[OQCO0CC0Q0]:=OOCO0CC0Q0;
end;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
function O0000CC0Q0.OCCO0CC0Q0(const O00QCCC0Q0:string):O0OQ0CC0Q0;
var
OO0QCCC0Q0:integer;
begin
O0QOOCOOQ0(OQ000CC0Q0);
try
OO0QCCC0Q0:=OC000CC0Q0.IndexOf(O00QCCC0Q0);
if OO0QCCC0Q0>=0 then
Result:=OQOO0CC0Q0(OO0QCCC0Q0)
else
Result:=nil;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
function O0000CC0Q0.OQ0QCCC0Q0(const OC0QCCC0Q0:string):O0OQ0CC0Q0;
begin
Result:=OCCO0CC0Q0(OC0QCCC0Q0);
if Result=nil then
raise EScError.CreateFmt(SObjNameNotFound,[OC0QCCC0Q0],seObjectNotFound);
end;
procedure O0000CC0Q0.O0OQCCC0Q0(OOOQCCC0Q0:TStrings);
var
OQOQCCC0Q0:integer;
begin
O0QOOCOOQ0(OQ000CC0Q0);
OOOQCCC0Q0.BeginUpdate;
try
OOOQCCC0Q0.Clear;
for OQOQCCC0Q0:=0 to OC000CC0Q0.Count-1 do
OOOQCCC0Q0.Add(OC000CC0Q0[OQOQCCC0Q0]);
finally
OOOQCCC0Q0.EndUpdate;
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
function O0000CC0Q0.OO0CCCC0Q0(OQ0CCCC0Q0:O0OQ0CC0Q0):integer;
begin
O0QOOCOOQ0(OQ000CC0Q0);
try
Result:=OC000CC0Q0.IndexOfObject(OQ0CCCC0Q0);
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
procedure O0000CC0Q0.OC0CCCC0Q0(O0OCCCC0Q0:O0OQ0CC0Q0);
var
OOOCCCC0Q0:integer;
begin
if O0OCCCC0Q0=nil then
Exit;
O0QOOCOOQ0(OQ000CC0Q0);
try
OOOCCCC0Q0:=OC000CC0Q0.IndexOfObject(O0OCCCC0Q0);
if OOOCCCC0Q0>-1 then begin
O0C00CC0Q0(O0OCCCC0Q0);
OC000CC0Q0.Delete(OOOCCCC0Q0);
end;
O0OCCCC0Q0.OOOQ0CC0Q0:=nil;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
procedure O0000CC0Q0.OQOCCCC0Q0;
var
OCOCCCC0Q0:O0OQ0CC0Q0;
begin
O0QOOCOOQ0(OQ000CC0Q0);
try
while OC000CC0Q0.Count>0 do begin
OCOCCCC0Q0:=OQOO0CC0Q0(OC000CC0Q0.Count-1);
OC0CCCC0Q0(OCOCCCC0Q0);
OCOCCCC0Q0.Free;
end;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
procedure O0000CC0Q0.O0QCCCC0Q0;
var
OOQCCCC0Q0:TStringList;
OQQCCCC0Q0:integer;
begin
if(OO000CC0Q0=nil)or IsClass(OO000CC0Q0,O00Q0CQ0Q0)then
Exit;
OOO00CC0Q0;
OOQCCCC0Q0:=TStringList.Create;
try
OO000CC0Q0.OQQQOCC0Q0(OQO00CC0Q0,OOQCCCC0Q0);
O0QOOCOOQ0(OQ000CC0Q0);
try
for OQQCCCC0Q0:=0 to OOQCCCC0Q0.Count-1 do
OC000CC0Q0.AddObject(OOQCCCC0Q0[OQQCCCC0Q0],OOQCCCC0Q0.Objects[OQQCCCC0Q0]);
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
finally
OOQCCCC0Q0.Free;
end;
end;
function O0000CC0Q0.OCOQCCC0Q0(O0QQCCC0Q0:O0OQ0CC0Q0):boolean;
var
OOQQCCC0Q0:integer;
begin
O0QOOCOOQ0(OQ000CC0Q0);
try
for OOQQCCC0Q0:=0 to OC000CC0Q0.Count-1 do begin
if OQOO0CC0Q0(OOQQCCC0Q0).Equals(O0QQCCC0Q0)then begin
Result:=True;
Exit;
end;
end;
Result:=False;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
procedure O0000CC0Q0.O0Q00CC0Q0(OOQ00CC0Q0:O0OQ0CC0Q0);
begin
if(OO000CC0Q0<>nil)and(OOQ00CC0Q0<>nil)then
OO000CC0Q0.OQQOQ00OQ0(OOQ00CC0Q0);
end;
procedure O0000CC0Q0.OQQ00CC0Q0(OCQ00CC0Q0:O0OQ0CC0Q0);
begin
if(OO000CC0Q0<>nil)and(OCQ00CC0Q0<>nil)then
OO000CC0Q0.OQOOQ00OQ0(OCQ00CC0Q0);
end;
procedure O0000CC0Q0.O0C00CC0Q0(OOC00CC0Q0:O0OQ0CC0Q0);
begin
if(OO000CC0Q0<>nil)and(OOC00CC0Q0<>nil)then
OO000CC0Q0.OQCOQ00OQ0(OOC00CC0Q0);
end;
procedure O0000CC0Q0.OQC00CC0Q0(OCC00CC0Q0:O0OQ0CC0Q0;const O00O0CC0Q0:string);
begin
if(OO000CC0Q0<>nil)and(OCC00CC0Q0<>nil)then
OO000CC0Q0.OQ0QOCC0Q0(OCC00CC0Q0,O00O0CC0Q0,False);
end;
procedure O0000CC0Q0.OO0O0CC0Q0(OQ0O0CC0Q0:O0OQ0CC0Q0;const OC0O0CC0Q0:string);
begin
if(OO000CC0Q0<>nil)and(OQ0O0CC0Q0<>nil)then
OO000CC0Q0.OQ0QOCC0Q0(OQ0O0CC0Q0,OC0O0CC0Q0,True);
end;
procedure O0000CC0Q0.OCQCCCC0Q0;
begin
if OO000CC0Q0<>nil then
OO000CC0Q0.OO0COCC0Q0;
end;
function OQCCCCC0Q0.OCCCCCC0Q0(O000CCC0Q0:integer):O0CQOOC0Q0;
begin
Result:=O0CQOOC0Q0(OQOO0CC0Q0(O000CCC0Q0));
end;
procedure OQCCCCC0Q0.OO00CCC0Q0(OQ00CCC0Q0:integer;OC00CCC0Q0:O0CQOOC0Q0);
begin
O0QO0CC0Q0(OQ00CCC0Q0,OC00CCC0Q0);
end;
procedure OQCCCCC0Q0.O0O0CCC0Q0(const OOO0CCC0Q0:string);
begin
O0OO0CC0Q0(OOO0CCC0Q0);
end;
function OQCCCCC0Q0.OQO0CCC0Q0(const OCO0CCC0Q0:string):O0CQOOC0Q0;
begin
Result:=O0CQOOC0Q0(OCCO0CC0Q0(OCO0CCC0Q0));
end;
function OQCCCCC0Q0.O0Q0CCC0Q0(const OOQ0CCC0Q0:string):O0CQOOC0Q0;
begin
Result:=O0CQOOC0Q0(OQ0QCCC0Q0(OOQ0CCC0Q0));
end;
procedure OQCCCCC0Q0.OQQ0CCC0Q0(OCQ0CCC0Q0:TStrings);
begin
O0OQCCC0Q0(OCQ0CCC0Q0);
end;
function OQCCCCC0Q0.OQO00CC0Q0:OCCC0CC0Q0;
begin
Result:=O0CQOOC0Q0;
end;
function OQCCCCC0Q0.OCO00CC0Q0:string;
begin
Result:='key';
end;
constructor OCCCQ00OQ0.Create(OQC0OCC0Q0:TComponent);
begin
inherited;
OOC0Q00OQ0:=True;
O000Q00OQ0:=TCriticalSection.Create;
end;
destructor OCCCQ00OQ0.Destroy;
begin
O0OOQ00OQ0;
O000Q00OQ0.Free;
inherited;
end;
procedure OCCCQ00OQ0.O0OOQ00OQ0;
begin
O0QOOCOOQ0(O000Q00OQ0);
try
FreeAndNil(OO00Q00OQ0);
FreeAndNil(OQ00Q00OQ0);
FreeAndNil(OC00Q00OQ0);
FreeAndNil(O0O0Q00OQ0);
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
function OCCCQ00OQ0.O00OQ00OQ0:OQCCCCC0Q0;
begin
O0QOOCOOQ0(O000Q00OQ0);
try
if OO00Q00OQ0=nil then begin
OO00Q00OQ0:=OQCCCCC0Q0.Create(Self);
OO00Q00OQ0.O0QCCCC0Q0;
end;
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
Result:=OO00Q00OQ0;
end;
function OCCCQ00OQ0.OO0OQ00OQ0:OOC0CCC0Q0;
begin
O0QOOCOOQ0(O000Q00OQ0);
try
if OQ00Q00OQ0=nil then begin
OQ00Q00OQ0:=OOC0CCC0Q0.Create(Self);
OQ00Q00OQ0.O0QCCCC0Q0;
end;
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
Result:=OQ00Q00OQ0;
end;
function OCCCQ00OQ0.OQ0OQ00OQ0:O0COCCC0Q0;
begin
O0QOOCOOQ0(O000Q00OQ0);
try
if OC00Q00OQ0=nil then begin
OC00Q00OQ0:=O0COCCC0Q0.Create(Self);
OC00Q00OQ0.O0QCCCC0Q0;
end;
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
Result:=OC00Q00OQ0;
end;
function OCCCQ00OQ0.OC0OQ00OQ0:OQCQQCC0Q0;
begin
O0QOOCOOQ0(O000Q00OQ0);
try
if O0O0Q00OQ0=nil then begin
O0O0Q00OQ0:=OQCQQCC0Q0.Create(Self);
O0O0Q00OQ0.O0QCCCC0Q0;
end;
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
Result:=O0O0Q00OQ0;
end;
procedure OCCCQ00OQ0.OOOOQ00OQ0;
begin
if O0C0Q00OQ0 then
raise EScError.Create(seChangingReadOnlyStorage);
end;
procedure OCCCQ00OQ0.OQOOQ00OQ0(OCOOQ00OQ0:O0OQ0CC0Q0);
begin
O0QOOCOOQ0(O000Q00OQ0);
try
O0QOQ00OQ0(OCOOQ00OQ0);
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
procedure OCCCQ00OQ0.OQQOQ00OQ0(OCQOQ00OQ0:O0OQ0CC0Q0);
begin
OOOOQ00OQ0;
O0QOOCOOQ0(O000Q00OQ0);
try
O0COQ00OQ0(OCQOQ00OQ0);
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
procedure OCCCQ00OQ0.OQCOQ00OQ0(OCCOQ00OQ0:O0OQ0CC0Q0);
begin
OOOOQ00OQ0;
O0QOOCOOQ0(O000Q00OQ0);
try
O00QOCC0Q0(OCCOQ00OQ0);
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
procedure OCCCQ00OQ0.OQ0QOCC0Q0(OC0QOCC0Q0:O0OQ0CC0Q0;const O0OQOCC0Q0:string;OOOQOCC0Q0:boolean=True);
begin
OOOOQ00OQ0;
O0QOOCOOQ0(O000Q00OQ0);
try
OQOQOCC0Q0(OC0QOCC0Q0,O0OQOCC0Q0,OOOQOCC0Q0);
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
procedure OCCCQ00OQ0.OQQQOCC0Q0(const OCQQOCC0Q0:OCCC0CC0Q0;O0CQOCC0Q0:TStrings);
begin
O0QOOCOOQ0(O000Q00OQ0);
try
O0CQOCC0Q0.BeginUpdate;
try
O0CQOCC0Q0.Clear;
OOCQOCC0Q0(OCQQOCC0Q0,O0CQOCC0Q0);
finally
O0CQOCC0Q0.EndUpdate;
end;
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
procedure OCCCQ00OQ0.DeleteStorage;
begin
OOOOQ00OQ0;
O0QOOCOOQ0(O000Q00OQ0);
try
O00COCC0Q0;
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
procedure OCCCQ00OQ0.OO0COCC0Q0;
begin
OOOOQ00OQ0;
end;
procedure OCCCQ00OQ0.OQC0Q00OQ0(const OCC0Q00OQ0:Boolean);
begin
if OCC0Q00OQ0<>O0C0Q00OQ0 then begin
O0C0Q00OQ0:=OCC0Q00OQ0;
O0OOQ00OQ0;
end;
end;
class function OCCCQ00OQ0.OOO0Q00OQ0(const OQO0Q00OQ0:OO00COQOQ0;const OCO0Q00OQ0:string):TSymmetricAlgorithm;
var
O0Q0Q00OQ0:THash_SHA1;
OOQ0Q00OQ0,OQQ0Q00OQ0:TBytes;
OCQ0Q00OQ0:integer;
begin
O0Q0Q00OQ0:=THash_SHA1.Create;
try
OOQ0Q00OQ0:=O0Q0Q00OQ0.ComputeHash(Encoding.Default.GetBytes(OCO0Q00OQ0));
finally
O0Q0Q00OQ0.Free;
end;
OCQ0Q00OQ0:=OOCQCC0OQ0.OCCCCC0OQ0(OQO0Q00OQ0);
if OCQ0Q00OQ0<>Length(OOQ0Q00OQ0)then
SetLength(OOQ0Q00OQ0,OCQ0Q00OQ0);
SetLength(OQQ0Q00OQ0,OOCQCC0OQ0.OO00CC0OQ0(OQO0Q00OQ0));
Result:=OOCQCC0OQ0.OQCQCC0OQ0(OQO0Q00OQ0,OOQ0Q00OQ0,OQQ0Q00OQ0);
FillChar(OOQ0Q00OQ0[0],Length(OOQ0Q00OQ0),0);
end;
class procedure OCCCQ00OQ0.OQ0COCC0Q0(OC0COCC0Q0:TStream;O0OCOCC0Q0:O0OQ0CC0Q0;const OOOCOCC0Q0:OO00COQOQ0;const OQOCOCC0Q0:string);
var
OCOCOCC0Q0:THash_SHA1;
O0QCOCC0Q0,OOQCOCC0Q0:TBytes;
OQQCOCC0Q0:TSymmetricAlgorithm;
OCQCOCC0Q0:string;
O0CCOCC0Q0,OOCCOCC0Q0:integer;
OQCCOCC0Q0:OCCO0OOCQ0;
OCCCOCC0Q0:Int64;
begin
if(OC0COCC0Q0.Size-OC0COCC0Q0.Position)<=4 then
raise EScError.Create(seWrongDataFormat);
if O0OCOCC0Q0.OOQQ0CC0Q0 then
Exit;
O0OCOCC0Q0.OOQQ0CC0Q0:=True;
try
if IsClass(O0OCOCC0Q0,TScCertificate)then begin
TScCertificate(O0OCOCC0Q0).ImportFrom(OC0COCC0Q0,OQOCOCC0Q0);
end
else
if IsClass(O0OCOCC0Q0,OCQOCQC0Q0)then begin
OCQOCQC0Q0(O0OCOCC0Q0).OQQOQQC0Q0(OC0COCC0Q0,OQOCOCC0Q0);
end
else begin
OCCCOCC0Q0:=OC0COCC0Q0.Position;
SetLength(O0QCOCC0Q0,OC0COCC0Q0.Size-OC0COCC0Q0.Position);
OC0COCC0Q0.Read(O0QCOCC0Q0[0],Length(O0QCOCC0Q0));
OCQCOCC0Q0:=Encoding.Default.GetString(O0QCOCC0Q0,0,4);
if((OCQCOCC0Q0<>('SBE'+OCOC0QQ0Q0))and(OCQCOCC0Q0<>('SBP'+OCOC0QQ0Q0))and
(OCQCOCC0Q0<>('SBE'+O0QC0QQ0Q0))and(OCQCOCC0Q0<>('SBP'+O0QC0QQ0Q0)))then begin
if IsClass(O0OCOCC0Q0,O0CQOOC0Q0)then begin
OC0COCC0Q0.Position:=OCCCOCC0Q0;
O0CQOOC0Q0(O0OCOCC0Q0).OOQQOCQ0Q0(OC0COCC0Q0,OQOCOCC0Q0);
Exit;
end
else
raise EScError.Create(seWrongDataFormat);
end;
OCOCOCC0Q0:=THash_SHA1.Create;
try
O0CCOCC0Q0:=OCOCOCC0Q0.HashSize;
O0QCOCC0Q0:=TBase64.Decode(O0QCOCC0Q0,4,Length(O0QCOCC0Q0)-4);
if(Length(O0QCOCC0Q0)<=(4+O0CCOCC0Q0))then
raise EScError.Create(seWrongDataFormat);
SetLength(OOQCOCC0Q0,0);
OOCCOCC0Q0:=Length(O0QCOCC0Q0)-O0CCOCC0Q0;
if OCQCOCC0Q0[4]=OCOC0QQ0Q0 then begin
OOQCOCC0Q0:=OCOCOCC0Q0.ComputeHash(TValueArr(O0QCOCC0Q0),0,OOCCOCC0Q0);
if MemCompare(@O0QCOCC0Q0[OOCCOCC0Q0],@OOQCOCC0Q0[0],O0CCOCC0Q0)<>0 then
raise EScError.Create(seWrongDataFormat);
end;
if(Length(OQOCOCC0Q0)>0)and(OCQCOCC0Q0[3]='E')then begin
OQQCOCC0Q0:=OOO0Q00OQ0(OOOCOCC0Q0,OQOCOCC0Q0);
OQQCOCC0Q0.CreateDecryptor.TransformBlock(O0QCOCC0Q0,0,OOCCOCC0Q0);
end;
if OCQCOCC0Q0[4]=O0QC0QQ0Q0 then begin
OOQCOCC0Q0:=OCOCOCC0Q0.ComputeHash(TValueArr(O0QCOCC0Q0),0,OOCCOCC0Q0);
if MemCompare(@O0QCOCC0Q0[OOCCOCC0Q0],@OOQCOCC0Q0[0],O0CCOCC0Q0)<>0 then
raise EScError.Create(seWrongDataFormat);
end;
SetLength(O0QCOCC0Q0,OOCCOCC0Q0);
finally
OCOCOCC0Q0.Free;
end;
if IsClass(O0OCOCC0Q0,O0CQOOC0Q0)then begin
O0OCOCC0Q0.OQCC0CC0Q0:=False;
OQCCOCC0Q0:=OCCO0OOCQ0.Create(O0QCOCC0Q0);
try
if OQCCOCC0Q0.OO0Q0OOCQ0<>OO0C0QQ0Q0 then
raise EScError.Create(seWrongDataFormat);
if OQCCOCC0Q0.O00Q0OOCQ0 then
O0CQOOC0Q0(O0OCOCC0Q0).OCOCQOC0Q0(OQCCOCC0Q0.OQCQ0OOCQ0,'')
else
O0CQOOC0Q0(O0OCOCC0Q0).OC00QOC0Q0(OQCCOCC0Q0.OQCQ0OOCQ0);
O0CQOOC0Q0(O0OCOCC0Q0).OOOCOOC0Q0;
finally
OQCCOCC0Q0.Free;
end;
end
else
if IsClass(O0OCOCC0Q0,OCQQQCQ0Q0)then
OCQQQCQ0Q0(O0OCOCC0Q0).OQC0QCQ0Q0(O0QCOCC0Q0)
else
raise EScError.Create(seInvalidInputArgs);
end;
finally
O0OCOCC0Q0.OOQQ0CC0Q0:=False;
end;
end;
class procedure OCCCQ00OQ0.O000OCC0Q0(OO00OCC0Q0:TStream;OQ00OCC0Q0:O0OQ0CC0Q0;const OC00OCC0Q0:OO00COQOQ0;const O0O0OCC0Q0:string);
var
OOO0OCC0Q0:THash_SHA1;
OQO0OCC0Q0:TSymmetricAlgorithm;
OCO0OCC0Q0,O0Q0OCC0Q0,OOQ0OCC0Q0:TBytes;
OQQ0OCC0Q0,OCQ0OCC0Q0:integer;
O0C0OCC0Q0:TBytes;
begin
if not OQ00OCC0Q0.OQCC0CC0Q0 then
raise EScError.CreateFmt(SObjNotReady,[OQ00OCC0Q0.ClassName],seObjNotReady);
if IsClass(OQ00OCC0Q0,O0CQOOC0Q0)then begin
if O0CQOOC0Q0(OQ00OCC0Q0).O0QOOCQ0Q0 then
O0CQOOC0Q0(OQ00OCC0Q0).O0OCOCQ0Q0(OO00OCC0Q0,False,O0O0OCC0Q0,OC00OCC0Q0,OC0CQ00OQ0)
else
O0CQOOC0Q0(OQ00OCC0Q0).O0OCOCQ0Q0(OO00OCC0Q0,True,'',OC00OCC0Q0,OOOCQ00OQ0);
end
else
if IsClass(OQ00OCC0Q0,TScCertificate)then begin
TScCertificate(OQ00OCC0Q0).ExportTo(OO00OCC0Q0,OOCCQ00OQ0);
end
else
if IsClass(OQ00OCC0Q0,OCQOCQC0Q0)then begin
OCQOCQC0Q0(OQ00OCC0Q0).OCQOQQC0Q0(OO00OCC0Q0,OOCCQ00OQ0);
end
else
if IsClass(OQ00OCC0Q0,OCQQQCQ0Q0)then begin
SetLength(OCO0OCC0Q0,0);
O0Q0OCC0Q0:=OCQQQCQ0Q0(OQ00OCC0Q0).OQOOQCQ0Q0;
OOO0OCC0Q0:=THash_SHA1.Create;
try
OCO0OCC0Q0:=OOO0OCC0Q0.ComputeHash(O0Q0OCC0Q0);
finally
OOO0OCC0Q0.Free;
end;
OQQ0OCC0Q0:=Length(OCO0OCC0Q0);
OCQ0OCC0Q0:=Length(O0Q0OCC0Q0);
SetLength(O0C0OCC0Q0,0);
if Length(O0O0OCC0Q0)>0 then begin
OQO0OCC0Q0:=OOO0Q00OQ0(OC00OCC0Q0,O0O0OCC0Q0);
OQO0OCC0Q0.CreateEncryptor.TransformBlock(O0Q0OCC0Q0,0,OCQ0OCC0Q0);
O0C0OCC0Q0:=Encoding.Default.GetBytes('SBE'+O0QC0QQ0Q0);
end
else
O0C0OCC0Q0:=Encoding.Default.GetBytes('SBP'+O0QC0QQ0Q0);
SetLength(O0Q0OCC0Q0,OCQ0OCC0Q0+OQQ0OCC0Q0);
Buffer.BlockCopy(OCO0OCC0Q0,0,O0Q0OCC0Q0,OCQ0OCC0Q0,OQQ0OCC0Q0);
O0Q0OCC0Q0:=TBase64.Encode(O0Q0OCC0Q0);
SetLength(OOQ0OCC0Q0,Length(O0C0OCC0Q0)+Length(O0Q0OCC0Q0));
Buffer.BlockCopy(O0C0OCC0Q0,0,OOQ0OCC0Q0,0,Length(O0C0OCC0Q0));
Buffer.BlockCopy(O0Q0OCC0Q0,0,OOQ0OCC0Q0,Length(O0C0OCC0Q0),Length(O0Q0OCC0Q0));
OCOO0QQ0Q0(OO00OCC0Q0,OOQ0OCC0Q0,64,False);
end
else
raise EScError.Create(seInvalidInputArgs);
end;
procedure OCCCQ00OQ0.O00OOCC0Q0(const OO0OOCC0Q0:string;const OQ0OOCC0Q0:string='');
var
OC0OOCC0Q0:TFileStream;
begin
OC0OOCC0Q0:=TFileStream.Create(OO0OOCC0Q0,fmOpenRead);
try
O00OOCC0Q0(OC0OOCC0Q0,OQ0OOCC0Q0);
finally
OC0OOCC0Q0.Free;
end;
end;
procedure OCCCQ00OQ0.O00OOCC0Q0(OC0OOCC0Q0:TStream;const OQ0OOCC0Q0:string='');
var
O0OOOCC0Q0:OOC0QCC0Q0;
begin
O0OOOCC0Q0:=OOC0QCC0Q0.Create;
try
O0OOOCC0Q0.O0QCOQC0Q0(OC0OOCC0Q0,OQ0OOCC0Q0);
O00OOCC0Q0(O0OOOCC0Q0);
finally
O0OOOCC0Q0.Free;
end;
end;
procedure OCCCQ00OQ0.O00OOCC0Q0(O0OOOCC0Q0:OOC0QCC0Q0);
procedure OOOOOCC0Q0(OQOOOCC0Q0:TCRList;OCOOOCC0Q0:OCCC0CC0Q0);
var
O0QOOCC0Q0:O0O0OQQ0Q0;
OOQOOCC0Q0:O0000CC0Q0;
OQQOOCC0Q0:string;
OCQOOCC0Q0,O0COOCC0Q0:integer;
begin
if OCOOOCC0Q0=O0CQOOC0Q0 then
OOQOOCC0Q0:=O00OQ00OQ0
else
if OCOOOCC0Q0=TScCertificate then
OOQOOCC0Q0:=OQ0OQ00OQ0
else
if OCOOOCC0Q0=OCQOCQC0Q0 then
OOQOOCC0Q0:=OC0OQ00OQ0
else
raise Exception.Create(SInternalError);
for OCQOOCC0Q0:=0 to OQOOOCC0Q0.Count-1 do begin
O0QOOCC0Q0:=O0O0OQQ0Q0(OQOOOCC0Q0[OCQOOCC0Q0]);
if O0QOOCC0Q0.OCQ0OQQ0Q0=nil then
Continue;
if OOQOOCC0Q0.OCOQCCC0Q0(O0QOOCC0Q0.OCQ0OQQ0Q0)then
Continue;
if O0QOOCC0Q0.O0C0OQQ0Q0<>'' then
OQQOOCC0Q0:=O0QOOCC0Q0.O0C0OQQ0Q0
else
OQQOOCC0Q0:=OOQOOCC0Q0.OCO00CC0Q0+'0';
O0COOCC0Q0:=0;
while OOQOOCC0Q0.OCCO0CC0Q0(OQQOOCC0Q0)<>nil do begin
Inc(O0COOCC0Q0);
OQQOOCC0Q0:=OOQOOCC0Q0.OCO00CC0Q0+IntToStr(O0COOCC0Q0);
end;
O0QOOCC0Q0.OCQ0OQQ0Q0.OCOC0CC0Q0:=Trim(OQQOOCC0Q0);
OOQOOCC0Q0.OQCQCCC0Q0(O0QOOCC0Q0.OCQ0OQQ0Q0);
O0QOOCC0Q0.OOO0OQQ0Q0:=nil;
end;
end;
begin
OOOOOCC0Q0(O0OOOCC0Q0.OOOOOQC0Q0,O0CQOOC0Q0);
OOOOOCC0Q0(O0OOOCC0Q0.OQOOOQC0Q0,TScCertificate);
OOOOOCC0Q0(O0OOOCC0Q0.OCOOOQC0Q0,OCQOCQC0Q0);
end;
procedure O00Q0CQ0Q0.O0QOQ00OQ0(OOQOQ00OQ0:O0OQ0CC0Q0);
begin
raise EScError.Create(seObjectNotFound);
end;
procedure O00Q0CQ0Q0.O0COQ00OQ0(OOCOQ00OQ0:O0OQ0CC0Q0);
begin
end;
procedure O00Q0CQ0Q0.O00QOCC0Q0(OO0QOCC0Q0:O0OQ0CC0Q0);
begin
end;
procedure O00Q0CQ0Q0.OQOQOCC0Q0(OCOQOCC0Q0:O0OQ0CC0Q0;const O0QQOCC0Q0:string;OOQQOCC0Q0:boolean);
begin
end;
procedure O00Q0CQ0Q0.OOCQOCC0Q0(const OQCQOCC0Q0:OCCC0CC0Q0;OCCQOCC0Q0:TStrings);
begin
end;
procedure O00Q0CQ0Q0.O00COCC0Q0;
begin
OOCOOCC0Q0.OQOCCCC0Q0;
OQCOOCC0Q0.OQOCCCC0Q0;
OCCOOCC0Q0.OQOCCCC0Q0;
O00Q0CC0Q0.OQOCCCC0Q0;
end;
constructor OO0Q0CQ0Q0.Create(OQC0OCC0Q0:TComponent);
begin
inherited;
OC0Q0CQ0Q0:=OOOCCOQOQ0;
O0OQ0CQ0Q0:=OCCQ0O0OQ0;
OOOQ0CQ0Q0:=O00C0O0OQ0;
OQOQ0CQ0Q0:=OO0C0O0OQ0;
OCOQ0CQ0Q0:=OQ0C0O0OQ0;
O0QQ0CQ0Q0:=OC0C0O0OQ0;
end;
procedure OO0Q0CQ0Q0.AssignTo(OO000CQ0Q0:TPersistent);
begin
if IsClass(OO000CQ0Q0,OO0Q0CQ0Q0)then begin
OO0Q0CQ0Q0(OO000CQ0Q0).O0C0Q00OQ0:=O0C0Q00OQ0;
OO0Q0CQ0Q0(OO000CQ0Q0).OC0Q0CQ0Q0:=OC0Q0CQ0Q0;
OO0Q0CQ0Q0(OO000CQ0Q0).OQ0Q0CQ0Q0:=OQ0Q0CQ0Q0;
OO0Q0CQ0Q0(OO000CQ0Q0).OOQCCCQ0Q0:=OOQCCCQ0Q0;
OO0Q0CQ0Q0(OO000CQ0Q0).OQQCCCQ0Q0:=OQQCCCQ0Q0;
OO0Q0CQ0Q0(OO000CQ0Q0).OCQCCCQ0Q0:=OCQCCCQ0Q0;
OO0Q0CQ0Q0(OO000CQ0Q0).O0CCCCQ0Q0:=O0CCCCQ0Q0;
OO0Q0CQ0Q0(OO000CQ0Q0).OOCCCCQ0Q0:=OOCCCCQ0Q0;
end
else
inherited;
end;
procedure OO0Q0CQ0Q0.DefineProperties(OC000CQ0Q0:TFiler);
begin
inherited DefineProperties(OC000CQ0Q0);
OC000CQ0Q0.DefineProperty('Path',O0O00CQ0Q0,OQO00CQ0Q0,O0OQ0CQ0Q0<>OCCQ0O0OQ0);
OC000CQ0Q0.DefineProperty('KeyExt',O0Q00CQ0Q0,OQQ00CQ0Q0,OOOQ0CQ0Q0<>O00C0O0OQ0);
OC000CQ0Q0.DefineProperty('UserExt',O0C00CQ0Q0,OQC00CQ0Q0,OQOQ0CQ0Q0<>OO0C0O0OQ0);
OC000CQ0Q0.DefineProperty('CertificateExt',O00O0CQ0Q0,OQ0O0CQ0Q0,OCOQ0CQ0Q0<>OQ0C0O0OQ0);
OC000CQ0Q0.DefineProperty('CRLExt',O0OO0CQ0Q0,OQOO0CQ0Q0,O0QQ0CQ0Q0<>OC0C0O0OQ0);
end;
procedure OO0Q0CQ0Q0.O0O00CQ0Q0(OOO00CQ0Q0:TReader);
begin
O0OQ0CQ0Q0:=OOO00CQ0Q0.ReadString;
end;
procedure OO0Q0CQ0Q0.OQO00CQ0Q0(OCO00CQ0Q0:TWriter);
begin
OCO00CQ0Q0.WriteString(O0OQ0CQ0Q0);
end;
procedure OO0Q0CQ0Q0.O0Q00CQ0Q0(OOQ00CQ0Q0:TReader);
begin
OOOQ0CQ0Q0:=OOQ00CQ0Q0.ReadString;
end;
procedure OO0Q0CQ0Q0.OQQ00CQ0Q0(OCQ00CQ0Q0:TWriter);
begin
OCQ00CQ0Q0.WriteString(OOOQ0CQ0Q0);
end;
procedure OO0Q0CQ0Q0.O0C00CQ0Q0(OOC00CQ0Q0:TReader);
begin
OQOQ0CQ0Q0:=OOC00CQ0Q0.ReadString;
end;
procedure OO0Q0CQ0Q0.OQC00CQ0Q0(OCC00CQ0Q0:TWriter);
begin
OCC00CQ0Q0.WriteString(OQOQ0CQ0Q0);
end;
procedure OO0Q0CQ0Q0.O00O0CQ0Q0(OO0O0CQ0Q0:TReader);
begin
OCOQ0CQ0Q0:=OO0O0CQ0Q0.ReadString;
end;
procedure OO0Q0CQ0Q0.OQ0O0CQ0Q0(OC0O0CQ0Q0:TWriter);
begin
OC0O0CQ0Q0.WriteString(OCOQ0CQ0Q0);
end;
procedure OO0Q0CQ0Q0.O0OO0CQ0Q0(OOOO0CQ0Q0:TReader);
begin
O0QQ0CQ0Q0:=OOOO0CQ0Q0.ReadString;
end;
procedure OO0Q0CQ0Q0.OQOO0CQ0Q0(OCOO0CQ0Q0:TWriter);
begin
OCOO0CQ0Q0.WriteString(O0QQ0CQ0Q0);
end;
function OO0Q0CQ0Q0.O0OC0CQ0Q0(OOOC0CQ0Q0:OCCC0CC0Q0):string;
begin
if OOOC0CQ0Q0=O0CQOOC0Q0 then
Result:=OOOQ0CQ0Q0
else
if OOOC0CQ0Q0=OCQQQCQ0Q0 then
Result:=OQOQ0CQ0Q0
else
if OOOC0CQ0Q0=TScCertificate then
Result:=OCOQ0CQ0Q0
else
if OOOC0CQ0Q0=OCQOCQC0Q0 then
Result:=O0QQ0CQ0Q0
else
Assert(False);
end;
procedure OO0Q0CQ0Q0.OQOC0CQ0Q0(OCOC0CQ0Q0:O0OQ0CC0Q0;out O0QC0CQ0Q0,OOQC0CQ0Q0:string);
begin
O0QC0CQ0Q0:=OCOC0CQ0Q0.OCOC0CC0Q0;
OOQC0CQ0Q0:=O0OC0CQ0Q0(OCCC0CC0Q0(OCOC0CQ0Q0.ClassType));
end;
function OO0Q0CQ0Q0.OQQC0CQ0Q0(OCQC0CQ0Q0:O0OQ0CC0Q0;O0CC0CQ0Q0:boolean=False):string;
var
OOCC0CQ0Q0,OQCC0CQ0Q0,OCCC0CQ0Q0:string;
begin
OQOC0CQ0Q0(OCQC0CQ0Q0,OQCC0CQ0Q0,OCCC0CQ0Q0);
if OQCC0CQ0Q0='' then
raise EScError.Create(seNullName);
OOCC0CQ0Q0:=OC0C0CQ0Q0;
if O0CC0CQ0Q0 then
if not ForceDirectories(OOCC0CQ0Q0)then
RaiseLastOSError;
Assert(OCCC0CQ0Q0<>'');
Result:=IncludeTrailingBackslash(OOCC0CQ0Q0)+OQCC0CQ0Q0+'.'+OCCC0CQ0Q0;
end;
procedure OO0Q0CQ0Q0.O0QOQ00OQ0(OOQOQ00OQ0:O0OQ0CC0Q0);
var
O0QO0CQ0Q0:TFileStream;
begin
O0QO0CQ0Q0:=nil;
try
try
O0QO0CQ0Q0:=TFileStream.Create(OQQC0CQ0Q0(OOQOQ00OQ0),fmOpenRead);
except
on e:EFOpenError do
raise EScError.Create(seObjectNotFound);
end;
if O0QO0CQ0Q0.Size=0 then
raise EScError.Create(seObjectNotFound);
OQ0COCC0Q0(O0QO0CQ0Q0,OOQOQ00OQ0,OCOCCCQ0Q0,O0QCCCQ0Q0);
finally
O0QO0CQ0Q0.Free;
end;
end;
procedure OO0Q0CQ0Q0.O0COQ00OQ0(OOCOQ00OQ0:O0OQ0CC0Q0);
var
OOQO0CQ0Q0:TFileStream;
begin
OOQO0CQ0Q0:=TFileStream.Create(OQQC0CQ0Q0(OOCOQ00OQ0,True),fmCreate);
try
O000OCC0Q0(OOQO0CQ0Q0,OOCOQ00OQ0,OCOCCCQ0Q0,O0QCCCQ0Q0);
finally
OOQO0CQ0Q0.Free;
end;
end;
procedure OO0Q0CQ0Q0.OOCQOCC0Q0(const OQCQOCC0Q0:OCCC0CC0Q0;OCCQOCC0Q0:TStrings);
{$IFDEF FPC}
const
faVolumeId=$00000008;
{$ENDIF}
var
O00QCCQ0Q0:TSearchRec;
OO0QCCQ0Q0:string;
OQ0QCCQ0Q0,OC0QCCQ0Q0:string;
O0OQCCQ0Q0:string;
begin
OO0QCCQ0Q0:=OC0C0CQ0Q0;
if(OO0QCCQ0Q0<>'')and not DirectoryExists(OO0QCCQ0Q0)then
Exit;
OC0QCCQ0Q0:='.'+O0OC0CQ0Q0(OQCQOCC0Q0);
OQ0QCCQ0Q0:='*'+OC0QCCQ0Q0;
try
if FindFirst(IncludeTrailingBackslash(ExpandFileName(IncludeTrailingBackslash(OO0QCCQ0Q0)))+OQ0QCCQ0Q0,faAnyFile,O00QCCQ0Q0)=0 then
repeat
if(O00QCCQ0Q0.Name<>'.')and
(O00QCCQ0Q0.Attr and faVolumeId<>faVolumeId)and
(O00QCCQ0Q0.Attr and faDirectory<>faDirectory)then
if LowerCase(ExtractFileExt(O00QCCQ0Q0.Name))=OC0QCCQ0Q0 then begin
O0OQCCQ0Q0:=ChangeFileExt(O00QCCQ0Q0.Name,'');
if OCCQOCC0Q0.IndexOf(O0OQCCQ0Q0)<0 then
OCCQOCC0Q0.Add(O0OQCCQ0Q0);
end;
until FindNext(O00QCCQ0Q0)<>0;
finally
FindClose(O00QCCQ0Q0);
end;
end;
procedure OO0Q0CQ0Q0.OOQQ0CQ0Q0(const OQQQ0CQ0Q0:string);
begin
if OQQQ0CQ0Q0<>O0OQ0CQ0Q0 then begin
O0OQ0CQ0Q0:=Trim(OQQQ0CQ0Q0);
O0OOQ00OQ0;
end;
end;
procedure OO0Q0CQ0Q0.OCQQ0CQ0Q0(const O0CQ0CQ0Q0:string);
begin
if O0CQ0CQ0Q0<>OOOQ0CQ0Q0 then begin
OOOQ0CQ0Q0:=Trim(O0CQ0CQ0Q0);
O0OOQ00OQ0;
end;
end;
procedure OO0Q0CQ0Q0.OOCQ0CQ0Q0(const OQCQ0CQ0Q0:string);
begin
if OQCQ0CQ0Q0<>OQOQ0CQ0Q0 then begin
OQOQ0CQ0Q0:=Trim(OQCQ0CQ0Q0);
O0OOQ00OQ0;
end;
end;
procedure OO0Q0CQ0Q0.OCCQ0CQ0Q0(const O00C0CQ0Q0:string);
begin
if O00C0CQ0Q0<>OCOQ0CQ0Q0 then begin
OCOQ0CQ0Q0:=Trim(O00C0CQ0Q0);
O0OOQ00OQ0;
end;
end;
procedure OO0Q0CQ0Q0.OO0C0CQ0Q0(const OQ0C0CQ0Q0:string);
begin
if OQ0C0CQ0Q0<>O0QQ0CQ0Q0 then begin
O0QQ0CQ0Q0:=Trim(OQ0C0CQ0Q0);
O0OOQ00OQ0;
end;
end;
function OO0Q0CQ0Q0.OC0C0CQ0Q0:string;
begin
Result:=O0OQ0CQ0Q0;
if
(Result<>'')and(Result[1]='.')and
(OQCQ0QQ0Q0<>'')
then
Result:=IncludeTrailingBackslash(OQCQ0QQ0Q0)+Result;
end;
procedure OO0Q0CQ0Q0.O00QOCC0Q0(OO0QOCC0Q0:O0OQ0CC0Q0);
var
OQQO0CQ0Q0:string;
begin
OQQO0CQ0Q0:=OQQC0CQ0Q0(OO0QOCC0Q0);
if FileExists(OQQO0CQ0Q0)then
if not DeleteFile(OQQO0CQ0Q0)then
RaiseLastOSError;
end;
procedure OO0Q0CQ0Q0.OQOQOCC0Q0(OCOQOCC0Q0:O0OQ0CC0Q0;const O0QQOCC0Q0:string;OOQQOCC0Q0:boolean);
var
OCQO0CQ0Q0:string;
O0CO0CQ0Q0,OOCO0CQ0Q0:string;
OQCO0CQ0Q0,OCCO0CQ0Q0:string;
begin
OQOC0CQ0Q0(OCOQOCC0Q0,O0CO0CQ0Q0,OOCO0CQ0Q0);
if(O0CO0CQ0Q0='')or(O0QQOCC0Q0='')then
raise EScError.Create(seNullName);
Assert(OOCO0CQ0Q0<>'');
OCQO0CQ0Q0:=IncludeTrailingBackslash(OC0C0CQ0Q0);
OQCO0CQ0Q0:=OCQO0CQ0Q0+O0CO0CQ0Q0+'.'+OOCO0CQ0Q0;
if not FileExists(OQCO0CQ0Q0)then begin
if OOQQOCC0Q0 then
raise EScError.Create(seObjectNotFound)
else
Exit;
end;
OCCO0CQ0Q0:=OCQO0CQ0Q0+O0QQOCC0Q0+'.'+OOCO0CQ0Q0;
if FileExists(OCCO0CQ0Q0)then
raise EScError.CreateFmt(SObjectAlreadyExists,[O0QQOCC0Q0],seObjectAlreadyExists);
RenameFile(OQCO0CQ0Q0,OCCO0CQ0Q0);
end;
procedure OO0Q0CQ0Q0.OOQQCCQ0Q0(const OQQQCCQ0Q0:string);
var
OCQQCCQ0Q0:TStringList;
O0CQCCQ0Q0,OOCQCCQ0Q0:string;
procedure OQCQCCQ0Q0(OCCQCCQ0Q0:O0OQ0CC0Q0);
var
O00CCCQ0Q0,OO0CCCQ0Q0:string;
OQ0CCCQ0Q0:string;
begin
OQOOQ00OQ0(OCCQCCQ0Q0);
OQOC0CQ0Q0(OCCQCCQ0Q0,O00CCCQ0Q0,OO0CCCQ0Q0);
OQ0CCCQ0Q0:=O0CQCCQ0Q0+O00CCCQ0Q0+'.'+OO0CCCQ0Q0;
if FileExists(OOCQCCQ0Q0)then
if not DeleteFile(OOCQCCQ0Q0)then
RaiseLastOSError;
RenameFile(OQ0CCCQ0Q0,OOCQCCQ0Q0);
OQ0Q0CQ0Q0:=OQQQCCQ0Q0;
try
OQQOQ00OQ0(OCCQCCQ0Q0);
except
DeleteFile(OQ0CCCQ0Q0);
RenameFile(OOCQCCQ0Q0,OQ0CCCQ0Q0);
raise;
end;
end;
var
OC0CCCQ0Q0:O0CQOOC0Q0;
O0OCCCQ0Q0:OCQQQCQ0Q0;
OOOCCCQ0Q0:string;
OQOCCCQ0Q0:integer;
begin
OOOOQ00OQ0;
if OQ0Q0CQ0Q0=OQQQCCQ0Q0 then
Exit;
O0QOOCOOQ0(O000Q00OQ0);
try
OC0CCCQ0Q0:=nil;
O0OCCCQ0Q0:=nil;
OCQQCCQ0Q0:=TStringList.Create;
try
OOOCCCQ0Q0:=OQ0Q0CQ0Q0;
O0CQCCQ0Q0:=IncludeTrailingBackslash(OC0C0CQ0Q0);
try
OOCQCCQ0Q0:=O0CQCCQ0Q0+O0CC0QQ0Q0+'.obj';
OQOCCCQ0Q0:=0;
while FileExists(OOCQCCQ0Q0)do begin
Inc(OQOCCCQ0Q0);
OOCQCCQ0Q0:=O0CQCCQ0Q0+O0CC0QQ0Q0+IntToStr(OQOCCCQ0Q0)+'.obj';
end;
try
OC0CCCQ0Q0:=O0CQOOC0Q0.Create(nil);
OQQQOCC0Q0(O0CQOOC0Q0,OCQQCCQ0Q0);
for OQOCCCQ0Q0:=0 to OCQQCCQ0Q0.Count-1 do begin
OQ0Q0CQ0Q0:=OOOCCCQ0Q0;
OC0CCCQ0Q0.OOQOOCQ0Q0:=OCQQCCQ0Q0[OQOCCCQ0Q0];
OQCQCCQ0Q0(OC0CCCQ0Q0);
end;
O0OCCCQ0Q0:=OCQQQCQ0Q0.Create(nil);
OQQQOCC0Q0(OCQQQCQ0Q0,OCQQCCQ0Q0);
for OQOCCCQ0Q0:=0 to OCQQCCQ0Q0.Count-1 do begin
OQ0Q0CQ0Q0:=OOOCCCQ0Q0;
O0OCCCQ0Q0.OCQOQCQ0Q0:=OCQQCCQ0Q0[OQOCCCQ0Q0];
OQCQCCQ0Q0(O0OCCCQ0Q0);
end;
finally
DeleteFile(OOCQCCQ0Q0);
end;
except
OQ0Q0CQ0Q0:=OOOCCCQ0Q0;
raise;
end;
OQ0Q0CQ0Q0:=OQQQCCQ0Q0;
finally
OCQQCCQ0Q0.Free;
OC0CCCQ0Q0.Free;
O0OCCCQ0Q0.Free;
end;
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
procedure OO0Q0CQ0Q0.O00COCC0Q0;
var
OOOQCCQ0Q0:TStringList;
OQOQCCQ0Q0:string;
OCOQCCQ0Q0,O0QQCCQ0Q0:integer;
begin
OQOQCCQ0Q0:=IncludeTrailingBackslash(OC0C0CQ0Q0);
OOOQCCQ0Q0:=TStringList.Create;
try
for O0QQCCQ0Q0:=Low(OO000QQ0Q0)to High(OO000QQ0Q0)do begin
OQQQOCC0Q0(OO000QQ0Q0[O0QQCCQ0Q0],OOOQCCQ0Q0);
for OCOQCCQ0Q0:=0 to OOOQCCQ0Q0.Count-1 do
DeleteFile(OQOQCCQ0Q0+OOOQCCQ0Q0[OCOQCCQ0Q0]+'.'+O0OC0CQ0Q0(OO000QQ0Q0[O0QQCCQ0Q0]));
end;
finally
OOOQCCQ0Q0.Free;
end;
RemoveDir(OC0C0CQ0Q0);
O0OOQ00OQ0;
end;
{$IFDEF MSWINDOWS}
constructor OQCCCCQ0Q0.Create(OQC0OCC0Q0:TComponent);
begin
inherited;
OO00CCQ0Q0:=OOQC0QQ0Q0;
OCCCCCQ0Q0:=OOOCCOQOQ0;
OQ00CCQ0Q0:=TRegistry.Create;
end;
destructor OQCCCCQ0Q0.Destroy;
begin
OQ00CCQ0Q0.Free;
inherited;
end;
procedure OQCCCCQ0Q0.AssignTo(OO000CQ0Q0:TPersistent);
begin
if IsClass(OO000CQ0Q0,OQCCCCQ0Q0)then begin
OQCCCCQ0Q0(OO000CQ0Q0).O0C0Q00OQ0:=O0C0Q00OQ0;
OQCCCCQ0Q0(OO000CQ0Q0).OCCCCCQ0Q0:=OCCCCCQ0Q0;
OQCCCCQ0Q0(OO000CQ0Q0).O000CCQ0Q0:=O000CCQ0Q0;
OQCCCCQ0Q0(OO000CQ0Q0).OQOQQCQ0Q0:=OQOQQCQ0Q0;
OQCCCCQ0Q0(OO000CQ0Q0).OOOQQCQ0Q0:=OOOQQCQ0Q0;
end
else
inherited;
end;
procedure OQCCCCQ0Q0.DefineProperties(OC000CQ0Q0:TFiler);
begin
inherited DefineProperties(OC000CQ0Q0);
OC000CQ0Q0.DefineProperty('KeyPath',OQQ0CCQ0Q0,O0C0CCQ0Q0,OOOQQCQ0Q0<>OOQC0QQ0Q0);
end;
procedure OQCCCCQ0Q0.OQQ0CCQ0Q0(OCQ0CCQ0Q0:TReader);
begin
OO00CCQ0Q0:=OCQ0CCQ0Q0.ReadString;
end;
procedure OQCCCCQ0Q0.O0C0CCQ0Q0(OOC0CCQ0Q0:TWriter);
begin
OOC0CCQ0Q0.WriteString(OO00CCQ0Q0);
end;
function OQCCCCQ0Q0.OQC0CCQ0Q0(OCC0CCQ0Q0:OCCC0CC0Q0):string;
begin
if OCC0CCQ0Q0=O0CQOOC0Q0 then
Result:=OO00CCQ0Q0+OOCC0QQ0Q0
else
if OCC0CCQ0Q0=OCQQQCQ0Q0 then
Result:=OO00CCQ0Q0+OQCC0QQ0Q0
else
if OCC0CCQ0Q0=TScCertificate then
Result:=OO00CCQ0Q0+OCCC0QQ0Q0
else
if OCC0CCQ0Q0=OCQOCQC0Q0 then
Result:=OO00CCQ0Q0+O0000QQ0Q0
else
Assert(False);
end;
procedure OQCCCCQ0Q0.O00OCCQ0Q0(OO0OCCQ0Q0:O0OQ0CC0Q0;out OQ0OCCQ0Q0,OC0OCCQ0Q0:string);
begin
OC0OCCQ0Q0:=OO0OCCQ0Q0.OCOC0CC0Q0;
OQ0OCCQ0Q0:=OQC0CCQ0Q0(OCCC0CC0Q0(OO0OCCQ0Q0.ClassType));
end;
procedure OQCCCCQ0Q0.O0QOQ00OQ0(OOQOQ00OQ0:O0OQ0CC0Q0);
var
O0OOCCQ0Q0,OOOOCCQ0Q0:string;
OQOOCCQ0Q0:TBytes;
O0QO0CQ0Q0:TMemoryStream;
begin
O00OCCQ0Q0(OOQOQ00OQ0,O0OOCCQ0Q0,OOOOCCQ0Q0);
if not OQ00CCQ0Q0.OpenKeyReadOnly(O0OOCCQ0Q0)then
raise EScError.Create(seKeyPathNotFound);
try
if OQ00CCQ0Q0.ValueExists(OOOOCCQ0Q0)then begin
if OQ00CCQ0Q0.GetDataType(OOOOCCQ0Q0)=rdString then
OQOOCCQ0Q0:=Encoding.Default.GetBytes(OQ00CCQ0Q0.ReadString(OOOOCCQ0Q0))
else begin
SetLength(OQOOCCQ0Q0,OQ00CCQ0Q0.GetDataSize(OOOOCCQ0Q0));
if Length(OQOOCCQ0Q0)>0 then
OQ00CCQ0Q0.ReadBinaryData(OOOOCCQ0Q0,OQOOCCQ0Q0[0],Length(OQOOCCQ0Q0));
end;
O0QO0CQ0Q0:=TMemoryStream.Create;
try
O0QO0CQ0Q0.WriteBuffer(OQOOCCQ0Q0[0],Length(OQOOCCQ0Q0));
O0QO0CQ0Q0.Position:=0;
OQ0COCC0Q0(O0QO0CQ0Q0,OOQOQ00OQ0,OC0QQCQ0Q0,O0OQQCQ0Q0);
finally
O0QO0CQ0Q0.Free;
end;
end
else
raise EScError.Create(seObjectNotFound);
finally
OQ00CCQ0Q0.CloseKey;
end;
end;
procedure OQCCCCQ0Q0.O0COQ00OQ0(OOCOQ00OQ0:O0OQ0CC0Q0);
var
OCOOCCQ0Q0,O0QOCCQ0Q0:string;
OOQOCCQ0Q0:TBytes;
OOQO0CQ0Q0:TMemoryStream;
begin
O00OCCQ0Q0(OOCOQ00OQ0,OCOOCCQ0Q0,O0QOCCQ0Q0);
OOQO0CQ0Q0:=TMemoryStream.Create;
try
O000OCC0Q0(OOQO0CQ0Q0,OOCOQ00OQ0,OC0QQCQ0Q0,O0OQQCQ0Q0);
OOQO0CQ0Q0.Position:=0;
SetLength(OOQOCCQ0Q0,OOQO0CQ0Q0.Size);
OOQO0CQ0Q0.ReadBuffer(OOQOCCQ0Q0[0],Length(OOQOCCQ0Q0));
finally
OOQO0CQ0Q0.Free;
end;
OQ00CCQ0Q0.Access:=KEY_ALL_ACCESS;
Win32Check(OQ00CCQ0Q0.OpenKey(OCOOCCQ0Q0,True));
try
OQ00CCQ0Q0.WriteBinaryData(O0QOCCQ0Q0,OOQOCCQ0Q0[0],Length(OOQOCCQ0Q0));
finally
OQ00CCQ0Q0.CloseKey;
end;
end;
procedure OQCCCCQ0Q0.O00QOCC0Q0(OO0QOCC0Q0:O0OQ0CC0Q0);
var
OQQOCCQ0Q0,OCQOCCQ0Q0:string;
begin
O00OCCQ0Q0(OO0QOCC0Q0,OQQOCCQ0Q0,OCQOCCQ0Q0);
OQ00CCQ0Q0.Access:=KEY_ALL_ACCESS;
if OQ00CCQ0Q0.OpenKey(OQQOCCQ0Q0,False)then begin
OQ00CCQ0Q0.DeleteValue(OCQOCCQ0Q0);
OQ00CCQ0Q0.CloseKey;
end;
end;
procedure OQCCCCQ0Q0.OQOQOCC0Q0(OCOQOCC0Q0:O0OQ0CC0Q0;const O0QQOCC0Q0:string;OOQQOCC0Q0:boolean);
var
O0CO0CQ0Q0,O0COCCQ0Q0:string;
begin
O00OCCQ0Q0(OCOQOCC0Q0,O0COCCQ0Q0,O0CO0CQ0Q0);
if(O0CO0CQ0Q0='')or(O0QQOCC0Q0='')then
raise EScError.Create(seNullName);
OQ00CCQ0Q0.Access:=KEY_ALL_ACCESS;
if not OQ00CCQ0Q0.OpenKey(O0COCCQ0Q0,False)then begin
if OOQQOCC0Q0 then
raise EScError.Create(seKeyPathNotFound)
else
Exit;
end;
try
if not OQ00CCQ0Q0.ValueExists(O0CO0CQ0Q0)then begin
if OOQQOCC0Q0 then
raise EScError.Create(seObjectNotFound)
else
Exit;
end;
if OQ00CCQ0Q0.ValueExists(O0QQOCC0Q0)then
raise EScError.CreateFmt(SObjectAlreadyExists,[O0QQOCC0Q0],seObjectAlreadyExists);
OQ00CCQ0Q0.RenameValue(O0CO0CQ0Q0,O0QQOCC0Q0);
finally
OQ00CCQ0Q0.CloseKey;
end;
end;
procedure OQCCCCQ0Q0.OQCOCCQ0Q0(const OQQQCCQ0Q0:string);
var
OCCOCCQ0Q0:TRegistry;
OCQQCCQ0Q0:TStringList;
O00QQCQ0Q0:string;
procedure OQCQCCQ0Q0(OCCQCCQ0Q0:O0OQ0CC0Q0);
var
OO0QQCQ0Q0,OQ0QQCQ0Q0:string;
begin
OQOOQ00OQ0(OCCQCCQ0Q0);
O00OCCQ0Q0(OCCQCCQ0Q0,OO0QQCQ0Q0,OQ0QQCQ0Q0);
if OCCOCCQ0Q0.ValueExists(O00QQCQ0Q0)then
OCCOCCQ0Q0.DeleteValue(O00QQCQ0Q0);
OCCOCCQ0Q0.RenameValue(OQ0QQCQ0Q0,O00QQCQ0Q0);
O000CCQ0Q0:=OQQQCCQ0Q0;
try
OQQOQ00OQ0(OCCQCCQ0Q0);
except
OCCOCCQ0Q0.DeleteValue(OQ0QQCQ0Q0);
OCCOCCQ0Q0.RenameValue(O00QQCQ0Q0,OQ0QQCQ0Q0);
raise;
end;
end;
var
OC0CCCQ0Q0:O0CQOOC0Q0;
O0OCCCQ0Q0:OCQQQCQ0Q0;
OOOCCCQ0Q0:string;
OQOCCCQ0Q0:integer;
begin
OOOOQ00OQ0;
if O000CCQ0Q0=OQQQCCQ0Q0 then
Exit;
O0QOOCOOQ0(O000Q00OQ0);
try
OC0CCCQ0Q0:=nil;
O0OCCCQ0Q0:=nil;
OCQQCCQ0Q0:=TStringList.Create;
try
OOOCCCQ0Q0:=O000CCQ0Q0;
try
OC0CCCQ0Q0:=O0CQOOC0Q0.Create(nil);
OQQQOCC0Q0(O0CQOOC0Q0,OCQQCCQ0Q0);
if OCQQCCQ0Q0.Count>0 then begin
OCCOCCQ0Q0:=TRegistry.Create;
try
OCCOCCQ0Q0.RootKey:=OQ00CCQ0Q0.RootKey;
OCCOCCQ0Q0.Access:=KEY_ALL_ACCESS;
if not OCCOCCQ0Q0.OpenKey(OO00CCQ0Q0+OOCC0QQ0Q0,False)then
raise EScError.Create(seKeyPathNotFound);
O00QQCQ0Q0:=O0CC0QQ0Q0;
OQOCCCQ0Q0:=0;
while OCCOCCQ0Q0.ValueExists(O00QQCQ0Q0)do begin
Inc(OQOCCCQ0Q0);
O00QQCQ0Q0:=O0CC0QQ0Q0+IntToStr(OQOCCCQ0Q0);
end;
try
for OQOCCCQ0Q0:=0 to OCQQCCQ0Q0.Count-1 do begin
O000CCQ0Q0:=OOOCCCQ0Q0;
OC0CCCQ0Q0.OOQOOCQ0Q0:=OCQQCCQ0Q0[OQOCCCQ0Q0];
OQCQCCQ0Q0(OC0CCCQ0Q0);
end;
finally
OCCOCCQ0Q0.DeleteValue(O00QQCQ0Q0);
end;
finally
OCCOCCQ0Q0.Free;
end;
end;
O0OCCCQ0Q0:=OCQQQCQ0Q0.Create(nil);
OQQQOCC0Q0(OCQQQCQ0Q0,OCQQCCQ0Q0);
if OCQQCCQ0Q0.Count>0 then begin
OCCOCCQ0Q0:=TRegistry.Create;
try
OCCOCCQ0Q0.RootKey:=OQ00CCQ0Q0.RootKey;
OCCOCCQ0Q0.Access:=KEY_ALL_ACCESS;
if not OCCOCCQ0Q0.OpenKey(OO00CCQ0Q0+OQCC0QQ0Q0,False)then
raise EScError.Create(seKeyPathNotFound);
O00QQCQ0Q0:=O0CC0QQ0Q0;
OQOCCCQ0Q0:=0;
while OCCOCCQ0Q0.ValueExists(O00QQCQ0Q0)do begin
Inc(OQOCCCQ0Q0);
O00QQCQ0Q0:=O0CC0QQ0Q0+IntToStr(OQOCCCQ0Q0);
end;
try
for OQOCCCQ0Q0:=0 to OCQQCCQ0Q0.Count-1 do begin
O000CCQ0Q0:=OOOCCCQ0Q0;
O0OCCCQ0Q0.OCQOQCQ0Q0:=OCQQCCQ0Q0[OQOCCCQ0Q0];
OQCQCCQ0Q0(O0OCCCQ0Q0);
end;
finally
OCCOCCQ0Q0.DeleteValue(O00QQCQ0Q0);
end;
finally
OCCOCCQ0Q0.Free;
end;
end;
except
O000CCQ0Q0:=OOOCCCQ0Q0;
raise;
end;
O000CCQ0Q0:=OQQQCCQ0Q0;
finally
OCQQCCQ0Q0.Free;
OC0CCCQ0Q0.Free;
O0OCCCQ0Q0.Free;
end;
finally
OQQOOCOOQ0(O000Q00OQ0);
end;
end;
procedure OQCCCCQ0Q0.OOCQOCC0Q0(const OQCQOCC0Q0:OCCC0CC0Q0;OCCQOCC0Q0:TStrings);
var
OOCOCCQ0Q0:string;
begin
try
OOCOCCQ0Q0:=OQC0CCQ0Q0(OQCQOCC0Q0);
if OQ00CCQ0Q0.OpenKeyReadOnly(OOCOCCQ0Q0)then
OQ00CCQ0Q0.GetValueNames(OCCQOCC0Q0);
finally
OQ00CCQ0Q0.CloseKey;
end;
end;
procedure OQCCCCQ0Q0.OC00CCQ0Q0(const O0O0CCQ0Q0:string);
begin
if O0O0CCQ0Q0<>OO00CCQ0Q0 then begin
OO00CCQ0Q0:=Trim(O0O0CCQ0Q0);
O0OOQ00OQ0;
end;
end;
procedure OQCCCCQ0Q0.OOO0CCQ0Q0(const OQO0CCQ0Q0:NativeUint);
begin
if OQO0CCQ0Q0<>OQ00CCQ0Q0.RootKey then begin
OQ00CCQ0Q0.RootKey:=OQO0CCQ0Q0;
O0OOQ00OQ0;
end;
end;
function OQCCCCQ0Q0.OCO0CCQ0Q0:NativeUint;
begin
Result:=OQ00CCQ0Q0.RootKey;
end;
procedure OQCCCCQ0Q0.O00COCC0Q0;
var
O0QQCCQ0Q0:integer;
begin
for O0QQCCQ0Q0:=Low(OO000QQ0Q0)to High(OO000QQ0Q0)do
OQ00CCQ0Q0.DeleteKey(OQC0CCQ0Q0(OO000QQ0Q0[O0QQCCQ0Q0]));
O0OOQ00OQ0;
end;
{$ENDIF}
constructor OCQQQCQ0Q0.Create(OOQC0CC0Q0:O0000CC0Q0=nil);
begin
inherited Create(OOQC0CC0Q0);
OCCQQCQ0Q0:=[OC0QO0QOQ0,O0OQO0QOQ0,OOOQO0QOQ0,OQOQO0QOQ0];
OQCQQCQ0Q0:='';
O00CQCQ0Q0:=[];
end;
destructor OCQQQCQ0Q0.Destroy;
begin
OC0CQCQ0Q0.Free;
inherited;
OQ0QOQQ0Q0:='';
end;
function OCQQQCQ0Q0.Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;
var
OQQOQCQ0Q0:OCQQQCQ0Q0;
begin
Result:=IsClass(O0CC0CC0Q0,OCQQQCQ0Q0);
if not Result then
Exit;
OQQOQCQ0Q0:=OCQQQCQ0Q0(O0CC0CC0Q0);
Result:=OCQOQCQ0Q0=OQQOQCQ0Q0.OCQOQCQ0Q0;
end;
procedure OCQQQCQ0Q0.OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);
begin
if not IsClass(OC0C0CC0Q0,OCQQQCQ0Q0)then
raise EScError.CreateFmt(SInvalidObjectClass,[OCQQQCQ0Q0.ClassName,OC0C0CC0Q0.ClassName],seInvalidObjectClass);
O00CQCQ0Q0:=OCQQQCQ0Q0(OC0C0CC0Q0).O00CQCQ0Q0;
O0CQQCQ0Q0:=OCQQQCQ0Q0(OC0C0CC0Q0).O0CQQCQ0Q0;
OOCQQCQ0Q0:=OCQQQCQ0Q0(OC0C0CC0Q0).OOCQQCQ0Q0;
OCCQQCQ0Q0:=OCQQQCQ0Q0(OC0C0CC0Q0).OCCQQCQ0Q0;
OQCQQCQ0Q0:=OCQQQCQ0Q0(OC0C0CC0Q0).OQCQQCQ0Q0;
OO0CQCQ0Q0:=OCQQQCQ0Q0(OC0C0CC0Q0).OO0CQCQ0Q0;
OQ0CQCQ0Q0:=OCQQQCQ0Q0(OC0C0CC0Q0).OQ0CQCQ0Q0;
OOO0QCQ0Q0;
if OC0CQCQ0Q0<>nil then
OC0CQCQ0Q0.Assign(OCQQQCQ0Q0(OC0C0CC0Q0).OC0QOQQ0Q0);
OCQOQCQ0Q0:=OCQQQCQ0Q0(OC0C0CC0Q0).OCQOQCQ0Q0;
end;
procedure OCQQQCQ0Q0.O0CQ0CC0Q0(const OOCQ0CC0Q0:string);
begin
if OCOQ0CC0Q0<>OOCQ0CC0Q0 then begin
inherited;
O0QQ0CC0Q0:=True;
OO0C0CC0Q0;
end;
end;
procedure OCQQQCQ0Q0.OCOCQCQ0Q0(const O0QCQCQ0Q0:string);
begin
if O0CQQCQ0Q0<>O0QCQCQ0Q0 then begin
O0CQQCQ0Q0:=O0QCQCQ0Q0;
OO0C0CC0Q0;
end;
end;
procedure OCQQQCQ0Q0.OOQCQCQ0Q0(const OQQCQCQ0Q0:string);
begin
if OOCQQCQ0Q0<>OQQCQCQ0Q0 then begin
OOCQQCQ0Q0:=OQQCQCQ0Q0;
OO0C0CC0Q0;
end;
end;
procedure OCQQQCQ0Q0.OCQCQCQ0Q0(const O0CCQCQ0Q0:string);
begin
if OQCQQCQ0Q0<>O0CCQCQ0Q0 then begin
OQCQQCQ0Q0:=O0CCQCQ0Q0;
OO0C0CC0Q0;
end;
end;
procedure OCQQQCQ0Q0.OOCCQCQ0Q0(const OQCCQCQ0Q0:TScSSHChannelPermissions);
begin
if OCCQQCQ0Q0<>OQCCQCQ0Q0 then begin
OCCQQCQ0Q0:=OQCCQCQ0Q0;
OO0C0CC0Q0;
end;
end;
procedure OCQQQCQ0Q0.OCCCQCQ0Q0(const O000QCQ0Q0:string);
var
OO00QCQ0Q0:THash_SHA1;
begin
if OO0CQCQ0Q0<>O000QCQ0Q0 then begin
if Length(OO0CQCQ0Q0)>0 then
FillChar(OO0CQCQ0Q0[1],Length(OO0CQCQ0Q0)*sizeof(Char),0);
if Length(OQ0CQCQ0Q0)>0 then
FillChar(OQ0CQCQ0Q0[1],Length(OQ0CQCQ0Q0)*sizeof(Char),0);
OO0CQCQ0Q0:=O000QCQ0Q0;
if O000QCQ0Q0<>'' then begin
OO00QCQ0Q0:=THash_SHA1.Create;
try
OQ0CQCQ0Q0:=O00QOCOOQ0(OO00QCQ0Q0.ComputeHash(Encoding.UTF8.GetBytes(O000QCQ0Q0)));
finally
OO00QCQ0Q0.Free;
end;
end
else
OQ0CQCQ0Q0:='';
OO0C0CC0Q0;
end;
end;
procedure OCQQQCQ0Q0.OOO0QCQ0Q0;
begin
if(OCOQQCQ0Q0 in O00CQCQ0Q0)and(OC0CQCQ0Q0=nil)then begin
OC0CQCQ0Q0:=O0CQOOC0Q0.Create(nil);
OC0CQCQ0Q0.O0OOQ0C0Q0(Self);
end
else
if not(OCOQQCQ0Q0 in O00CQCQ0Q0)and(OC0CQCQ0Q0<>nil)then
FreeAndNil(OC0CQCQ0Q0);
end;
procedure OCQQQCQ0Q0.OQ00QCQ0Q0(const OC00QCQ0Q0:TScUserAuthentications);
begin
if O00CQCQ0Q0<>OC00QCQ0Q0 then begin
O00CQCQ0Q0:=OC00QCQ0Q0;
OOO0QCQ0Q0;
OO0C0CC0Q0;
end;
end;
procedure OCQQQCQ0Q0.O0O0QCQ0Q0(OC00CCC0Q0:O0CQOOC0Q0);
begin
if OC0CQCQ0Q0<>OC00CCC0Q0 then begin
if OC0CQCQ0Q0=nil then
raise EScError.Create(seKeyAuthNotSupported);
OC0CQCQ0Q0.Assign(OC00CCC0Q0);
OO0C0CC0Q0;
end;
end;
procedure OCQQQCQ0Q0.OQO0QCQ0Q0(const OCO0QCQ0Q0:TBytes);
var
O0Q0QCQ0Q0:OCCO0OOCQ0;
OOQ0QCQ0Q0:byte;
begin
OC0QOQQ0Q0.OQCC0CC0Q0:=False;
O0Q0QCQ0Q0:=OCCO0OOCQ0.Create(OCO0QCQ0Q0);
try
OOQ0QCQ0Q0:=O0Q0QCQ0Q0.OO0Q0OOCQ0;
if(OOQ0QCQ0Q0<>OO0C0QQ0Q0)and(OOQ0QCQ0Q0<>OQ0C0QQ0Q0)then
raise EScError.Create(seWrongDataFormat);
if O0Q0QCQ0Q0.O00Q0OOCQ0 then begin
if OOQ0QCQ0Q0=OQ0C0QQ0Q0 then
OC0QOQQ0Q0.OCOQQOC0Q0(O0Q0QCQ0Q0.OQCQ0OOCQ0)
else
OC0QOQQ0Q0.OCOCQOC0Q0(O0Q0QCQ0Q0.OQCQ0OOCQ0,'');
end
else
OC0QOQQ0Q0.OC00QOC0Q0(O0Q0QCQ0Q0.OQCQ0OOCQ0);
finally
O0Q0QCQ0Q0.Free;
end;
OC0QOQQ0Q0.OOOCOOC0Q0;
end;
function OCQQQCQ0Q0.OQQ0QCQ0Q0:TBytes;
var
OCQ0QCQ0Q0:OC0QCOOCQ0;
O0C0QCQ0Q0:TBytes;
OOC0QCQ0Q0:byte;
begin
if not OC0QOQQ0Q0.OQCC0CC0Q0 then
raise EScError.Create(seKeyNotReady);
OOC0QCQ0Q0:=OO0C0QQ0Q0;
if OC0QOQQ0Q0.O0QOOCQ0Q0 then begin
if OC0QOQQ0Q0.OQOOOCQ0Q0=O0QQQOQOQ0 then begin
OOC0QCQ0Q0:=OQ0C0QQ0Q0;
O0C0QCQ0Q0:=OC0QOQQ0Q0.O0OCO0C0Q0;
end
else
O0C0QCQ0Q0:=OC0QOQQ0Q0.OOO0O0C0Q0('');
end
else
O0C0QCQ0Q0:=OC0QOQQ0Q0.OCC0O0C0Q0;
OCQ0QCQ0Q0:=OC0QCOOCQ0.Create;
try
OCQ0QCQ0Q0.OCO00OOCQ0(OOC0QCQ0Q0);
OCQ0QCQ0Q0.OOO00OOCQ0(OC0QOQQ0Q0.O0QOOCQ0Q0);
OCQ0QCQ0Q0.OO000OOCQ0(O0C0QCQ0Q0);
Result:=OCQ0QCQ0Q0.OOCC0OOCQ0;
finally
OCQ0QCQ0Q0.Free;
end;
end;
procedure OCQQQCQ0Q0.OQC0QCQ0Q0(const OCC0QCQ0Q0:TBytes);
var
O00OQCQ0Q0:OCCO0OOCQ0;
OO0OQCQ0Q0:byte;
OQ0OQCQ0Q0:TBytes;
OC0OQCQ0Q0:string;
O0OOQCQ0Q0:OCOQO0QOQ0;
OOOOQCQ0Q0:integer;
begin
SetLength(OQ0OQCQ0Q0,0);
O0QQ0CC0Q0:=False;
O00OQCQ0Q0:=OCCO0OOCQ0.Create(OCC0QCQ0Q0);
try
OO0OQCQ0Q0:=O00OQCQ0Q0.OO0Q0OOCQ0;
if not(OO0OQCQ0Q0 in[OC0C0QQ0Q0,O0OC0QQ0Q0,OOOC0QQ0Q0,OQOC0QQ0Q0])then
raise EScError.Create(seWrongDataFormat);
if OCOQ0CC0Q0<>string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(O00OQCQ0Q0.OQOQ0OOCQ0))then
raise EScError.Create(seWrongDataFormat);
O00CQCQ0Q0:=[];
if O00OQCQ0Q0.O00Q0OOCQ0 then
Include(O00CQCQ0Q0,OCOQQCQ0Q0);
if O00OQCQ0Q0.O00Q0OOCQ0 then
Include(O00CQCQ0Q0,O0QQQCQ0Q0);
if O00OQCQ0Q0.O00Q0OOCQ0 then
Include(O00CQCQ0Q0,OOQQQCQ0Q0);
OOO0QCQ0Q0;
if OO0OQCQ0Q0>=O0OC0QQ0Q0 then
O0CQQCQ0Q0:=string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(O00OQCQ0Q0.OQOQ0OOCQ0));
if OO0OQCQ0Q0>=OOOC0QQ0Q0 then
OOCQQCQ0Q0:=string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(O00OQCQ0Q0.OQOQ0OOCQ0));
OQ0QOQQ0Q0:=string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(O00OQCQ0Q0.OQOQ0OOCQ0));
if OO0OQCQ0Q0>=O0OC0QQ0Q0 then
OQ0CQCQ0Q0:=string(Encoding.Default.GetString(O00OQCQ0Q0.OQOQ0OOCQ0));
if O00OQCQ0Q0.O00Q0OOCQ0 then begin
OQ0OQCQ0Q0:=O00OQCQ0Q0.OQOQ0OOCQ0;
if OCOQQCQ0Q0 in O00QOQQ0Q0 then
OQO0QCQ0Q0(OQ0OQCQ0Q0);
end;
OQCQQCQ0Q0:='';
OCCQQCQ0Q0:=[OC0QO0QOQ0,O0OQO0QOQ0,OOOQO0QOQ0,OQOQO0QOQ0];
while O00OQCQ0Q0.OQ0C0OOCQ0>0 do begin
try
OC0OQCQ0Q0:=string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(O00OQCQ0Q0.OQOQ0OOCQ0));
if SameText(OC0OQCQ0Q0,OQQC0QQ0Q0)then begin
OCCQQCQ0Q0:=[];
OOOOQCQ0Q0:=O00OQCQ0Q0.OC0Q0OOCQ0;
for O0OOQCQ0Q0:=Low(OCOQO0QOQ0)to High(OCOQO0QOQ0)do begin
if(OQ000QQ0Q0[O0OOQCQ0Q0]and OOOOQCQ0Q0)<>0 then
Include(OCCQQCQ0Q0,O0OOQCQ0Q0);
end;
end
else
if SameText(OC0OQCQ0Q0,OCQC0QQ0Q0)then
OQCQQCQ0Q0:=string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(O00OQCQ0Q0.OQOQ0OOCQ0))
except
end;
end;
O0QQ0CC0Q0:=True;
finally
O00OQCQ0Q0.Free;
end;
end;
function OCQQQCQ0Q0.OQOOQCQ0Q0:TBytes;
var
OCOOQCQ0Q0:OC0QCOOCQ0;
O0QOQCQ0Q0:OCOQO0QOQ0;
OOQOQCQ0Q0:integer;
begin
SetLength(Result,0);
OCOOQCQ0Q0:=OC0QCOOCQ0.Create;
try
OCOOQCQ0Q0.OCO00OOCQ0(OQOC0QQ0Q0);
OCOOQCQ0Q0.OO0O0OOCQ0(WideString(OCOQ0CC0Q0));
OCOOQCQ0Q0.OOO00OOCQ0(OCOQQCQ0Q0 in O00CQCQ0Q0);
OCOOQCQ0Q0.OOO00OOCQ0(O0QQQCQ0Q0 in O00CQCQ0Q0);
OCOOQCQ0Q0.OOO00OOCQ0(OOQQQCQ0Q0 in O00CQCQ0Q0);
OCOOQCQ0Q0.OO0O0OOCQ0(WideString(O0CQQCQ0Q0));
OCOOQCQ0Q0.OO0O0OOCQ0(WideString(OOCQQCQ0Q0));
if(OOOQ0CC0Q0<>nil)and(OOOQ0CC0Q0.OOCCCCC0Q0<>nil)and not OOOQ0CC0Q0.OOCCCCC0Q0.OQ0Q0CC0Q0 then
OCOOQCQ0Q0.OCC00OOCQ0('')
else
OCOOQCQ0Q0.OO0O0OOCQ0(WideString(OO0CQCQ0Q0));
OCOOQCQ0Q0.OCC00OOCQ0(OQ0CQCQ0Q0);
if(OCOQQCQ0Q0 in O00CQCQ0Q0)and OC0QOQQ0Q0.OQCC0CC0Q0 then begin
OCOOQCQ0Q0.OOO00OOCQ0(True);
OCOOQCQ0Q0.O0OO0OOCQ0(OQQ0QCQ0Q0);
end
else
OCOOQCQ0Q0.OOO00OOCQ0(False);
OOQOQCQ0Q0:=0;
for O0QOQCQ0Q0:=Low(OCOQO0QOQ0)to High(OCOQO0QOQ0)do begin
if O0QOQCQ0Q0 in OCCQQCQ0Q0 then
Inc(OOQOQCQ0Q0,OQ000QQ0Q0[O0QOQCQ0Q0]);
end;
OCOOQCQ0Q0.OO0O0OOCQ0(WideString(OQQC0QQ0Q0));
OCOOQCQ0Q0.OCQ00OOCQ0(OOQOQCQ0Q0);
OCOOQCQ0Q0.OO0O0OOCQ0(WideString(OCQC0QQ0Q0));
OCOOQCQ0Q0.OO0O0OOCQ0(WideString(OQCQQCQ0Q0));
Result:=OCOOQCQ0Q0.OOCC0OOCQ0;
finally
OCOOQCQ0Q0.Free;
end;
end;
function OCQQQCQ0Q0.O0OCQCQ0Q0:OOC0CCC0Q0;
begin
Result:=OOC0CCC0Q0(OOCC0CC0Q0);
end;
procedure OCQQQCQ0Q0.OOOCQCQ0Q0(OQOCQCQ0Q0:OOC0CCC0Q0);
begin
OOCC0CC0Q0:=OQOCQCQ0Q0;
end;
function OOC0CCC0Q0.OQC0CCC0Q0(OCC0CCC0Q0:integer):OCQQQCQ0Q0;
begin
Result:=OCQQQCQ0Q0(OQOO0CC0Q0(OCC0CCC0Q0));
end;
procedure OOC0CCC0Q0.O00OCCC0Q0(OO0OCCC0Q0:integer;OQ0OCCC0Q0:OCQQQCQ0Q0);
begin
O0QO0CC0Q0(OO0OCCC0Q0,OQ0OCCC0Q0);
end;
procedure OOC0CCC0Q0.OC0OCCC0Q0(const O0OOCCC0Q0:string);
begin
O0OO0CC0Q0(O0OOCCC0Q0);
end;
function OOC0CCC0Q0.OOOOCCC0Q0(const OQOOCCC0Q0:string):OCQQQCQ0Q0;
begin
Result:=OCQQQCQ0Q0(OCCO0CC0Q0(OQOOCCC0Q0));
end;
function OOC0CCC0Q0.OCOOCCC0Q0(const O0QOCCC0Q0:string):OCQQQCQ0Q0;
begin
Result:=OCQQQCQ0Q0(OQ0QCCC0Q0(O0QOCCC0Q0));
end;
procedure OOC0CCC0Q0.OOQOCCC0Q0(OQQOCCC0Q0:TStrings);
begin
O0OQCCC0Q0(OQQOCCC0Q0);
end;
function OOC0CCC0Q0.OQO00CC0Q0:OCCC0CC0Q0;
begin
Result:=OCQQQCQ0Q0;
end;
function OOC0CCC0Q0.OCO00CC0Q0:string;
begin
Result:='usr';
end;
constructor TScCertificate.Create(OOQC0CC0Q0:O0000CC0Q0=nil);
begin
inherited Create(OOQC0CC0Q0);
OOOQ0QC0Q0:=-1;
end;
procedure TScCertificate.O0OC0CC0Q0;
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
O0Q00QC0Q0(nil,nil);
OCCOOQC0Q0:=nil;
FreeAndNil(OOQOOQC0Q0);
FreeAndNil(O0COOQC0Q0);
FreeAndNil(OOCOOQC0Q0);
FreeAndNil(OQCOOQC0Q0);
FreeAndNil(OQ0Q0QC0Q0);
FreeAndNil(O00Q0QC0Q0);
FreeAndNil(OO0Q0QC0Q0);
OOOQ0QC0Q0:=-1;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function TScCertificate.GetRawData:TBytes;
begin
Result:=OQQOOQC0Q0;
end;
procedure TScCertificate.OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);
begin
if not IsClass(OC0C0CC0Q0,TScCertificate)then
raise EScError.CreateFmt(SInvalidObjectClass,[TScCertificate.ClassName,OC0C0CC0Q0.ClassName],seInvalidObjectClass);
SetRawData(nil,nil,TScCertificate(OC0C0CC0Q0).OQQOOQC0Q0);
O0QQ0CC0Q0:=OC0C0CC0Q0.O0QQ0CC0Q0;
end;
class function TScCertificate.OOQQ0QC0Q0(const OQQQ0QC0Q0,OCQQ0QC0Q0:string):string;
var
O0CQ0QC0Q0,OOCQ0QC0Q0:integer;
OQCQ0QC0Q0,OCCQ0QC0Q0:string;
begin
Result:='';
OQCQ0QC0Q0:='-----BEGIN '+OCQQ0QC0Q0+'-----';
O0CQ0QC0Q0:=Pos(OQCQ0QC0Q0,OQQQ0QC0Q0);
if O0CQ0QC0Q0=0 then
Exit;
OCCQ0QC0Q0:='-----END '+OCQQ0QC0Q0+'-----';
OOCQ0QC0Q0:=PosEx(OCCQ0QC0Q0,OQQQ0QC0Q0,O0CQ0QC0Q0);
if OOCQ0QC0Q0=0 then
OOCQ0QC0Q0:=Length(OQQQ0QC0Q0);
O0CQ0QC0Q0:=O0CQ0QC0Q0+Length(OQCQ0QC0Q0);
Result:=Trim(Copy(OQQQ0QC0Q0,O0CQ0QC0Q0,OOCQ0QC0Q0-O0CQ0QC0Q0));
end;
procedure TScCertificate.O0Q00QC0Q0(OOQ00QC0Q0:IntPtr;OQQ00QC0Q0:OQQQOQQ0Q0);
begin
if Assigned(OCQOOQC0Q0)and Assigned(O0OQ0QC0Q0)then
O0OQ0QC0Q0(OCQOOQC0Q0);
OCQOOQC0Q0:=OOQ00QC0Q0;
O0OQ0QC0Q0:=OQQ00QC0Q0;
end;
procedure TScCertificate.SetRawData(OQOQCQC0Q0:IntPtr;OCOQCQC0Q0:OQQQOQQ0Q0;const O0QQCQC0Q0:TBytes);
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OQCC0CC0Q0:=False;
OCQOOQC0Q0:=OQOQCQC0Q0;
O0OQ0QC0Q0:=OCOQCQC0Q0;
OQQOOQC0Q0:=O0QQCQC0Q0;
Assert(OOQOOQC0Q0=nil);
OOQOOQC0Q0:=OQCOCQOOQ0.Create;
try
if(OQQOOQC0Q0=nil)or not OOQOOQC0Q0.OCQCOOOOQ0(O0QQCCOOQ0,OQQOOQC0Q0)then
raise EScError.Create(seWrongCertContext);
except
FreeAndNil(OOQOOQC0Q0);
raise;
end;
O0QQ0CC0Q0:=True;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
class procedure TScCertificate.O0C00QC0Q0(OOC00QC0Q0:OOOCCOOOQ0;
out OQC00QC0Q0:OOOOCOQOQ0;out OCC00QC0Q0:OQ0Q0OOOQ0);
begin
OCC00QC0Q0:=OOC00QC0Q0.OQQCCOOOQ0;
if OCC00QC0Q0=O00Q0OOOQ0 then
OQC00QC0Q0:=OOC00QC0Q0.OCQCCOOOQ0.O0OC0OOOQ0
else
OQC00QC0Q0:=OOC00QC0Q0.OOQCCOOOQ0;
end;
function TScCertificate.O00O0QC0Q0(OO0O0QC0Q0:TScCertificate):boolean;
var
OQ0O0QC0Q0,OC0O0QC0Q0:TBytes;
O0OO0QC0Q0:OOOOCOQOQ0;
OOOO0QC0Q0:OQ0Q0OOOQ0;
begin
Assert(OO0O0QC0Q0<>nil);
O0C00QC0Q0(O0CC0QC0Q0,O0OO0QC0Q0,OOOO0QC0Q0);
OO0O0QC0Q0.Key.OQCOOCQ0Q0.OQQQ0OOOQ0(O0CC0QC0Q0.OCQCCOOOQ0);
OQ0O0QC0Q0:=OOQOOQC0Q0['TBSCertificate'].O0OQ0QOOQ0;
OC0O0QC0Q0:=OOQOOQC0Q0['SignatureValue'].OQOQ0QOOQ0;
Result:=OO0O0QC0Q0.Key.O0C0OCQ0Q0(OQ0O0QC0Q0,OC0O0QC0Q0,O0OO0QC0Q0,OOOO0QC0Q0);
end;
function TScCertificate.OQOO0QC0Q0(OCOO0QC0Q0,O0QO0QC0Q0:OQQOC0OOQ0):boolean;
var
OOQO0QC0Q0:string;
OQQO0QC0Q0:boolean;
OCQO0QC0Q0,O0CO0QC0Q0:integer;
begin
Result:=False;
if(OCOO0QC0Q0=nil)or(O0QO0QC0Q0=nil)then
Exit;
for O0CO0QC0Q0:=0 to O0QO0QC0Q0.OQOQQ0OOQ0.OOO000QOQ0-1 do
if O0QO0QC0Q0.OQOQQ0OOQ0[O0CO0QC0Q0].OO0OC0OOQ0=OID_CE_CERTIFICATE_ANY_POLICY then begin
Result:=True;
Exit;
end;
for OCQO0QC0Q0:=0 to OCOO0QC0Q0.OQOQQ0OOQ0.OOO000QOQ0-1 do begin
OOQO0QC0Q0:=OCOO0QC0Q0.OQOQQ0OOQ0[OCQO0QC0Q0].OO0OC0OOQ0;
OQQO0QC0Q0:=False;
for O0CO0QC0Q0:=0 to O0QO0QC0Q0.OQOQQ0OOQ0.OOO000QOQ0-1 do begin
if OOQO0QC0Q0=O0QO0QC0Q0.OQOQQ0OOQ0[O0CO0QC0Q0].OO0OC0OOQ0 then begin
OQQO0QC0Q0:=True;
break;
end;
end;
if not OQQO0QC0Q0 then
Exit;
end;
Result:=True;
end;
procedure TScCertificate.OOOQCQC0Q0;
begin
if OCCOOQC0Q0=nil then
OCCOOQC0Q0:=OQ0COC0OQ0(Extensions.O0CO0C0OQ0(OQ0COC0OQ0));
end;
procedure TScCertificate.VerifyCertificateChain(O0O0CQC0Q0:OOOQOQQ0Q0;
OOO0CQC0Q0:TScCertificate;var OQO0CQC0Q0:TScCertificateStatusSet);
var
OCO0CQC0Q0:TDateTime;
O0Q0CQC0Q0:OOQ000OOQ0;
OOQ0CQC0Q0:TClass;
OQQ0CQC0Q0:OQQOC0OOQ0;
OCQ0CQC0Q0:boolean;
O0C0CQC0Q0:OOOOCOQOQ0;
OOC0CQC0Q0:OQ0Q0OOOQ0;
OQC0CQC0Q0:integer;
begin
OCO0CQC0Q0:=Now;
if(OCO0CQC0Q0<OQQC0QC0Q0)or(OCO0CQC0Q0>OOQC0QC0Q0)then
Include(OQO0CQC0Q0,csExpired);
if OOO0CQC0Q0=nil then begin
if OCO00QC0Q0 then begin
Exclude(OQO0CQC0Q0,csUntrustedRoot);
Exit;
end;
end
else begin
if not IssuerName.Equals(OOO0CQC0Q0.SubjectName)then
Include(OQO0CQC0Q0,csIssuerNotEqualSubject);
OQQ0CQC0Q0:=OQQOC0OOQ0(Extensions.O0CO0C0OQ0(OQQOC0OOQ0));
if(OQQ0CQC0Q0<>nil)and not OQQ0CQC0Q0.OOOO00OOQ0 then
OQQ0CQC0Q0:=nil;
OCQ0CQC0Q0:=False;
for OQC0CQC0Q0:=0 to OOO0CQC0Q0.Extensions.OOO000QOQ0-1 do begin
O0Q0CQC0Q0:=OOO0CQC0Q0.Extensions[OQC0CQC0Q0];
if IsClass(O0Q0CQC0Q0,OCOO00OOQ0)then begin
OCQ0CQC0Q0:=True;
if not OCOO00OOQ0(O0Q0CQC0Q0).OCCO00OOQ0 then
Include(OQO0CQC0Q0,csInvalidBasicConstraints);
end
else
if IsClass(O0Q0CQC0Q0,O0CQC0OOQ0)then begin
if not(OCOQC0OOQ0 in O0CQC0OOQ0(O0Q0CQC0Q0).OQQCC0OOQ0)then
Include(OQO0CQC0Q0,csInvalidKeyUsage);
end
else
if IsClass(O0Q0CQC0Q0,OQQOC0OOQ0)then begin
if(OQQ0CQC0Q0<>nil)and not OQOO0QC0Q0(OQQ0CQC0Q0,OQQOC0OOQ0(O0Q0CQC0Q0))then
Include(OQO0CQC0Q0,csInvalidPolicies);
OQQ0CQC0Q0:=nil;
end;
end;
if not OCQ0CQC0Q0 then
Include(OQO0CQC0Q0,csInvalidBasicConstraints);
if OQQ0CQC0Q0<>nil then
if not OOO0CQC0Q0.OCO00QC0Q0 and not OOO0CQC0Q0.IssuerName.Equals(OOO0CQC0Q0.SubjectName)then
Include(OQO0CQC0Q0,csInvalidPolicies);
if not O00O0QC0Q0(OOO0CQC0Q0)then
Include(OQO0CQC0Q0,csInvalidSignature);
end;
if Key.OQOOOCQ0Q0=OCOQQOQOQ0 then begin
O0C00QC0Q0(O0CC0QC0Q0,O0C0CQC0Q0,OOC0CQC0Q0);
if O0C0CQC0Q0 in[O0C0COQOQ0,OQ0OCOQOQ0,OC0OCOQOQ0,O0OOCOQOQ0]then
Include(OQO0CQC0Q0,csForbiddenSignature)
else
if O0C0CQC0Q0=OOC0COQOQ0 then
Include(OQO0CQC0Q0,csInsecureSignature);
end;
if Assigned(O0O0CQC0Q0)then
VerifyCRLRevocation(O0O0CQC0Q0,OOO0CQC0Q0,OQO0CQC0Q0);
for OQC0CQC0Q0:=0 to Extensions.OOO000QOQ0-1 do begin
O0Q0CQC0Q0:=Extensions[OQC0CQC0Q0];
OOQ0CQC0Q0:=O0Q0CQC0Q0.ClassType;
if O0Q0CQC0Q0.OOOO00OOQ0 and
((OOQ0CQC0Q0<>OCOO00OOQ0)and
(OOQ0CQC0Q0<>O0CQC0OOQ0)and
(OOQ0CQC0Q0<>OCQCC0OOQ0)and
(OOQ0CQC0Q0<>OO00Q0OOQ0)and
(OOQ0CQC0Q0<>OQQOC0OOQ0)and
(OOQ0CQC0Q0<>OQ0COC0OQ0))
then
Include(OQO0CQC0Q0,csUnknownCriticalExtension);
end;
end;
procedure TScCertificate.VerifyCRLRevocation(OCC0CQC0Q0:OOOQOQQ0Q0;
O00OCQC0Q0:TScCertificate;var OO0OCQC0Q0:TScCertificateStatusSet);
var
OQ0OCQC0Q0,OC0OCQC0Q0:O0CCOC0OQ0;
O0OOCQC0Q0,OOOOCQC0Q0:OC0OQ0OOQ0;
OQOOCQC0Q0,OCOOCQC0Q0:OCQOCQC0Q0;
O0QOCQC0Q0:OQ0OQ0OOQ0;
OOQOCQC0Q0,OQQOCQC0Q0:integer;
begin
if not Assigned(OCC0CQC0Q0)then
raise EScError.Create(seInvalidInputArgs);
OOOQCQC0Q0;
if OCCOOQC0Q0=nil then
Exit;
OQ0OCQC0Q0:=O0CCOC0OQ0(Extensions.O0CO0C0OQ0(O0CCOC0OQ0));
OC0Q0QC0Q0:=OQ0OQ0OOQ0(-1);
OCOOCQC0Q0:=nil;
for OOQOCQC0Q0:=0 to OCCOOQC0Q0.OCQCOC0OQ0.OOO000QOQ0-1 do begin
OC0Q0QC0Q0:=OQ0OQ0OOQ0(-1);
O0OOCQC0Q0:=OCCOOQC0Q0.OCQCOC0OQ0[OOQOCQC0Q0];
if O0OOCQC0Q0.O0QQOC0OQ0.OOO000QOQ0=0 then
Continue;
OQOOCQC0Q0:=nil;
try
OCC0CQC0Q0(Self,O0OOCQC0Q0.O0QQOC0OQ0,False,OQOOCQC0Q0);
except
Continue;
end;
if OQOOCQC0Q0=nil then
Continue;
if Now>OQOOCQC0Q0.OCOQOOC0Q0 then begin
try
OCC0CQC0Q0(Self,O0OOCQC0Q0.O0QQOC0OQ0,True,OQOOCQC0Q0);
except
Continue;
end;
if OQOOCQC0Q0=nil then
Continue;
end;
OCOOCQC0Q0:=OQOOCQC0Q0;
if not OQOOCQC0Q0.OQQCQQC0Q0(Self,O0OOCQC0Q0)then
Include(OO0OCQC0Q0,csCRLIsNotValid);
if(OQ0OCQC0Q0<>nil)and(OQ0OCQC0Q0.OCQCOC0OQ0.OOO000QOQ0>OOQOCQC0Q0)then begin
OOOOCQC0Q0:=OQ0OCQC0Q0.OCQCOC0OQ0[OOQOCQC0Q0];
if OOOOCQC0Q0.O0QQOC0OQ0.OOO000QOQ0=0 then begin
Include(OO0OCQC0Q0,csCRLIsNotValid);
Continue;
end;
OOCO0QC0Q0(OCC0CQC0Q0,OOOOCQC0Q0.O0QQOC0OQ0,OQOOCQC0Q0,O00OCQC0Q0,OO0OCQC0Q0);
end;
OC0OCQC0Q0:=O0CCOC0OQ0(OQOOCQC0Q0.O0QQOOC0Q0.O0CO0C0OQ0(O0CCOC0OQ0));
if OC0OCQC0Q0<>nil then begin
for OQQOCQC0Q0:=0 to OC0OCQC0Q0.OCQCOC0OQ0.OOO000QOQ0-1 do begin
OOOOCQC0Q0:=OC0OCQC0Q0.OCQCOC0OQ0[OQQOCQC0Q0];
if OOOOCQC0Q0.O0QQOC0OQ0.OOO000QOQ0=0 then begin
Include(OO0OCQC0Q0,csCRLIsNotValid);
Continue;
end;
OOCO0QC0Q0(OCC0CQC0Q0,OOOOCQC0Q0.O0QQOC0OQ0,OQOOCQC0Q0,O00OCQC0Q0,OO0OCQC0Q0);
end;
end;
if O00OCQC0Q0<>nil then
OQOOCQC0Q0.OQOOQQC0Q0(O00OCQC0Q0,OO0OCQC0Q0);
if OQOOCQC0Q0.OOQ0QQC0Q0(Self,O0QOCQC0Q0)then
if OC0Q0QC0Q0<>O00OQ0OOQ0 then
OC0Q0QC0Q0:=O0QOCQC0Q0;
if(OC0Q0QC0Q0<>OQ0OQ0OOQ0(-1))and(OC0Q0QC0Q0<>O00OQ0OOQ0)then begin
Include(OO0OCQC0Q0,csCertificateIsRevoked);
Exit;
end;
end;
if(OCOOCQC0Q0=nil)and(OCCOOQC0Q0.OCQCOC0OQ0.OOO000QOQ0>0)then
Include(OO0OCQC0Q0,csCRLNotFound);
end;
procedure TScCertificate.OOCO0QC0Q0(OQCO0QC0Q0:OOOQOQQ0Q0;
OCCO0QC0Q0:OCOC00OOQ0;O00QCQC0Q0:OCQOCQC0Q0;OO0QCQC0Q0:TScCertificate;
var OQ0QCQC0Q0:TScCertificateStatusSet);
var
OC0QCQC0Q0:OCQOCQC0Q0;
O0OQCQC0Q0:OQ0OQ0OOQ0;
begin
if(O00QCQC0Q0=nil)or not Assigned(OQCO0QC0Q0)then
raise EScError.Create(seInvalidInputArgs);
OC0QCQC0Q0:=nil;
try
OQCO0QC0Q0(Self,OCCO0QC0Q0,False,OC0QCQC0Q0);
except
Exit;
end;
if OC0QCQC0Q0=nil then
Exit;
if Now>OC0QCQC0Q0.OCOQOOC0Q0 then begin
try
OQCO0QC0Q0(Self,OCCO0QC0Q0,True,OC0QCQC0Q0);
except
Include(OQ0QCQC0Q0,csCRLNotFound);
Exit;
end;
end;
if OC0QCQC0Q0=nil then begin
Include(OQ0QCQC0Q0,csCRLNotFound);
Exit;
end;
if Now>OC0QCQC0Q0.OCOQOOC0Q0 then
Include(OQ0QCQC0Q0,csCRLIsNotValid);
if not O00QCQC0Q0.OQQCQQC0Q0(OC0QCQC0Q0)then
Include(OQ0QCQC0Q0,csCRLIsNotValid);
if OO0QCQC0Q0<>nil then
OC0QCQC0Q0.OQOOQQC0Q0(OO0QCQC0Q0,OQ0QCQC0Q0);
if OC0QCQC0Q0.OOQ0QQC0Q0(Self,O0OQCQC0Q0)then
if OC0Q0QC0Q0<>O00OQ0OOQ0 then
OC0Q0QC0Q0:=O0OQCQC0Q0;
end;
procedure TScCertificate.ImportFrom(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string='');
var
OCQCOQC0Q0:TFileStream;
begin
OCQCOQC0Q0:=TFileStream.Create(OOQCOQC0Q0,fmOpenRead);
try
ImportFrom(OCQCOQC0Q0,OQQCOQC0Q0);
finally
OCQCOQC0Q0.Free;
end;
end;
procedure TScCertificate.ImportFrom(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string='');
var
OQCQCQC0Q0,OCCQCQC0Q0:TBytes;
O00CCQC0Q0,OO0CCQC0Q0:string;
begin
SetLength(OQCQCQC0Q0,OCQCOQC0Q0.Size-OCQCOQC0Q0.Position);
if Length(OQCQCQC0Q0)>0 then
OCQCOQC0Q0.Read(OQCQCQC0Q0[0],Length(OQCQCQC0Q0));
O00CCQC0Q0:=Encoding.ANSI.GetString(OQCQCQC0Q0);
OO0CCQC0Q0:=OOQQ0QC0Q0(O00CCQC0Q0,'CERTIFICATE');
if OO0CCQC0Q0='' then
OO0CCQC0Q0:=OOQQ0QC0Q0(O00CCQC0Q0,'X509 CERTIFICATE');
if OO0CCQC0Q0='' then
OO0CCQC0Q0:=OOQQ0QC0Q0(O00CCQC0Q0,'TRUSTED CERTIFICATE');
if OO0CCQC0Q0<>'' then
OCCQCQC0Q0:=TBase64.Decode(Encoding.Default.GetBytes(OO0CCQC0Q0))
else
OCCQCQC0Q0:=OQCQCQC0Q0;
SetRawData(nil,nil,OCCQCQC0Q0);
OO0C0CC0Q0;
end;
procedure TScCertificate.ExportTo(const OQ0CCQC0Q0:string;const OC0CCQC0Q0:OQCCQ00OQ0=OOCCQ00OQ0);
var
O0OCCQC0Q0:TFileStream;
begin
O0OCCQC0Q0:=TFileStream.Create(OQ0CCQC0Q0,fmCreate);
try
ExportTo(O0OCCQC0Q0,OC0CCQC0Q0);
finally
O0OCCQC0Q0.Free;
end;
end;
procedure TScCertificate.ExportTo(O0OCCQC0Q0:TStream;const OC0CCQC0Q0:OQCCQ00OQ0=OOCCQ00OQ0);
begin
if not OQCC0CC0Q0 then
raise EScError.Create(seCertificateNotReady);
if OC0CCQC0Q0=OOCCQ00OQ0 then begin
O0OOC0QOQ0.OC0QQ0QOQ0(O0OCCQC0Q0,CERT_HEADER);
OCOO0QQ0Q0(O0OCCQC0Q0,TBase64.Encode(OQQOOQC0Q0),64,False);
O0OOC0QOQ0.OC0QQ0QOQ0(O0OCCQC0Q0,CERT_FOOTER);
end
else
O0OCCQC0Q0.WriteBuffer(OQQOOQC0Q0[0],Length(OQQOOQC0Q0));
end;
function TScCertificate.Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;
var
OOQQCQC0Q0:TScCertificate;
begin
Result:=IsClass(O0CC0CC0Q0,TScCertificate);
if not Result then
Exit;
OOQQCQC0Q0:=TScCertificate(O0CC0CC0Q0);
OQOC0CC0Q0;
OOQQCQC0Q0.OQOC0CC0Q0;
Result:=Length(OQQOOQC0Q0)=Length(OOQQCQC0Q0.OQQOOQC0Q0);
if Result then begin
Result:=MemCompare(@OQQOOQC0Q0[0],@OOQQCQC0Q0.OQQOOQC0Q0[0],Length(OQQOOQC0Q0))=0;
if Result then
Result:=OOQQCQC0Q0.Key.O0QOOCQ0Q0=Key.O0QOOCQ0Q0;
end;
end;
function TScCertificate.Sign(const OOOCCQC0Q0:TBytes;OQOCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OCOCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):TBytes;
begin
OQOC0CC0Q0;
Result:=Key.O0Q0OCQ0Q0(OOOCCQC0Q0,OQOCCQC0Q0,OCOCCQC0Q0);
end;
function TScCertificate.SignHash(const OQC0CO0OQ0:TBytes;O0QCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OOQCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):TBytes;
begin
OQOC0CC0Q0;
Result:=Key.OCQ0OCQ0Q0(OQC0CO0OQ0,O0QCCQC0Q0,OOQCCQC0Q0);
end;
function TScCertificate.VerifySign(const OQQCCQC0Q0,OCQCCQC0Q0:TBytes;O0CCCQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OOCCCQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):boolean;
begin
OQOC0CC0Q0;
Result:=Key.O0C0OCQ0Q0(OQQCCQC0Q0,OCQCCQC0Q0,O0CCCQC0Q0,OOCCCQC0Q0);
end;
function TScCertificate.VerifyHashSign(const OQCCCQC0Q0,OCCCCQC0Q0:TBytes;O000CQC0Q0:OOOOCOQOQ0=OOC0COQOQ0;OO00CQC0Q0:OQ0Q0OOOQ0=OOCOOOOOQ0):boolean;
begin
OQOC0CC0Q0;
Result:=Key.OCC0OCQ0Q0(OQCCCQC0Q0,OCCCCQC0Q0,O000CQC0Q0,OO00CQC0Q0);
end;
function TScCertificate.Encrypt(const OQ00CQC0Q0:TBytes):TBytes;
begin
OQOC0CC0Q0;
Result:=Key.O00OOCQ0Q0(OQ00CQC0Q0);
end;
function TScCertificate.Decrypt(const OC00CQC0Q0:TBytes):TBytes;
begin
OQOC0CC0Q0;
Result:=Key.OC0OOCQ0Q0(OC00CQC0Q0);
end;
procedure TScCertificate.GetFingerprint(const OQQQCQC0Q0:OOOOCOQOQ0;out OCQQCQC0Q0:TBytes);
var
O0CQCQC0Q0:THashAlgorithm;
begin
OQOC0CC0Q0;
O0CQCQC0Q0:=OOCQCC0OQ0.OOOCCC0OQ0(OQQQCQC0Q0);
try
OCQQCQC0Q0:=O0CQCQC0Q0.ComputeHash(OQQOOQC0Q0);
finally
O0CQCQC0Q0.Free;
end;
end;
procedure TScCertificate.GetFingerprint(const OQQQCQC0Q0:OOOOCOQOQ0;out OCQQCQC0Q0:string);
var
OOCQCQC0Q0:TBytes;
begin
GetFingerprint(OQQQCQC0Q0,OOCQCQC0Q0);
OCQQCQC0Q0:=O00QOCOOQ0(OOCQCQC0Q0,':');
end;
procedure TScCertificate.OQ0C0QC0Q0;
begin
OQOC0CC0Q0;
O0QOOCOOQ0(OQOQ0CC0Q0);
try
if O0COOQC0Q0=nil then begin
O0COOQC0Q0:=OQOCO0OOQ0.Create;
O00QCC0OQ0.OO0QCC0OQ0(O0COOQC0Q0,OOQOOQC0Q0['Issuer']);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
procedure TScCertificate.OC0C0QC0Q0;
begin
OQOC0CC0Q0;
O0QOOCOOQ0(OQOQ0CC0Q0);
try
if OOCOOQC0Q0=nil then begin
OOCOOQC0Q0:=OQOCO0OOQ0.Create;
O00QCC0OQ0.OO0QCC0OQ0(OOCOOQC0Q0,OOQOOQC0Q0['Subject']);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function TScCertificate.OCOC0QC0Q0:OQOCO0OOQ0;
begin
OQ0C0QC0Q0;
Result:=O0COOQC0Q0;
end;
function TScCertificate.O0QC0QC0Q0:OQOCO0OOQ0;
begin
OC0C0QC0Q0;
Result:=OOCOOQC0Q0;
end;
class function TScCertificate.O0OC0QC0Q0(OOOC0QC0Q0:OQOCO0OOQ0):string;
var
OQOC0QC0Q0:integer;
begin
if OOOC0QC0Q0.OOCOO0OOQ0=0 then begin
Result:='';
Exit;
end;
OQOC0QC0Q0:=OOOC0QC0Q0.OQ0OO0OOQ0(OID_COMMON_NAME);
if OQOC0QC0Q0<0 then begin
OQOC0QC0Q0:=OOOC0QC0Q0.OQ0OO0OOQ0(OID_ORGANIZATIONAL_UNIT_NAME);
if OQOC0QC0Q0<0 then begin
OQOC0QC0Q0:=OOOC0QC0Q0.OQ0OO0OOQ0(OID_ORGANIZATION_NAME);
if OQOC0QC0Q0<0 then begin
OQOC0QC0Q0:=OOOC0QC0Q0.OQ0OO0OOQ0(OID_RSA_emailAddr);
if OQOC0QC0Q0<0 then
OQOC0QC0Q0:=0;
end;
end;
end;
Result:=OOOC0QC0Q0.OO0Q00OOQ0[OQOC0QC0Q0];
end;
function TScCertificate.O00C0QC0Q0:string;
begin
OQ0C0QC0Q0;
Result:=O0OC0QC0Q0(O0COOQC0Q0);
end;
function TScCertificate.OO0C0QC0Q0:string;
begin
OC0C0QC0Q0;
Result:=O0OC0QC0Q0(OOCOOQC0Q0);
end;
function TScCertificate.OOQC0QC0Q0:TDateTime;
begin
OQOC0CC0Q0;
Result:=OOQOOQC0Q0['NotAfter'].OOCQ0QOOQ0;
end;
function TScCertificate.OQQC0QC0Q0:TDateTime;
begin
OQOC0CC0Q0;
Result:=OOQOOQC0Q0['NotBefore'].OOCQ0QOOQ0;
end;
function TScCertificate.OCQC0QC0Q0:string;
begin
OQOC0CC0Q0;
Result:=OOQOOQC0Q0['SerialNumber'].OCOQ0QOOQ0;
end;
function TScCertificate.O0CC0QC0Q0:OOOCCOOOQ0;
begin
OQOC0CC0Q0;
O0QOOCOOQ0(OQOQ0CC0Q0);
try
if OQ0Q0QC0Q0=nil then begin
OQ0Q0QC0Q0:=OOOCCOOOQ0.Create;
O00QCC0OQ0.OO0QCC0OQ0(OQ0Q0QC0Q0,OOQOOQC0Q0['Signature']);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
Result:=OQ0Q0QC0Q0;
end;
function TScCertificate.OOCC0QC0Q0:TBytes;
begin
OQOC0CC0Q0;
Result:=OOQOOQC0Q0['SignatureValue'].OQOQ0QOOQ0;
end;
function TScCertificate.OQCC0QC0Q0:integer;
begin
OQOC0CC0Q0;
Result:=OOQOOQC0Q0['Version'].OQQQ0QOOQ0+1;
end;
procedure TScCertificate.OCCC0QC0Q0;
begin
OQOC0CC0Q0;
O0QOOCOOQ0(OQOQ0CC0Q0);
try
if OQCOOQC0Q0=nil then begin
OQCOOQC0Q0:=OQ0O0C0OQ0.Create;
O00QCC0OQ0.OO0QCC0OQ0(OQCOOQC0Q0,OOQOOQC0Q0['Extensions']);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function TScCertificate.O0000QC0Q0:OQ0O0C0OQ0;
begin
OCCC0QC0Q0;
Result:=OQCOOQC0Q0;
end;
function TScCertificate.OO000QC0Q0:string;
var
OQ000QC0Q0:OOQ000OOQ0;
OC000QC0Q0:integer;
begin
OCCC0QC0Q0;
Result:='';
for OC000QC0Q0:=0 to OQCOOQC0Q0.OOO000QOQ0-1 do begin
OQ000QC0Q0:=OQCOOQC0Q0[OC000QC0Q0];
if IsClass(OQ000QC0Q0,OCCCC0OOQ0)then begin
Result:=OCCCC0OOQ0(OQ000QC0Q0).OO00C0OOQ0;
Exit;
end;
end;
end;
function TScCertificate.O0O00QC0Q0:O0CQOOC0Q0;
var
OOO00QC0Q0:string;
OQO00QC0Q0:OC0QOQOOQ0;
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OQOC0CC0Q0;
if(O00Q0QC0Q0<>nil)and not O00Q0QC0Q0.OQCC0CC0Q0 then begin
FreeAndNil(O00Q0QC0Q0);
FreeAndNil(OO0Q0QC0Q0);
end;
if O00Q0QC0Q0=nil then begin
O00Q0QC0Q0:=O0CQOOC0Q0.Create(nil);
OO0Q0QC0Q0:=O0CQOOC0Q0.Create(nil);
O00Q0QC0Q0.O0OOQ0C0Q0(Self);
OQO00QC0Q0:=OOQOOQC0Q0['SubjectPublicKeyInfo'];
OOO00QC0Q0:=OQO00QC0Q0['Algorithm']['Algorithm'].O0CQ0QOOQ0;
if(OOO00QC0Q0=OID_RSA_ENCRYPTION)or(OOO00QC0Q0=OID_PKCS1)then
O00Q0QC0Q0.OCCOQ0C0Q0(OQO00QC0Q0['SubjectPublicKey'].OQOQ0QOOQ0,OCOCQ00OQ0,OCOQQOQOQ0,True)
else
if OOO00QC0Q0=OID_DSA_ENCRYPTION then
O00Q0QC0Q0.OCQQ00C0Q0(OQO00QC0Q0['SubjectPublicKey'].OQOQ0QOOQ0,OQO00QC0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0)
else
if OOO00QC0Q0=OID_EC_PUBLIC_KEY then
O00Q0QC0Q0.OCCQ00C0Q0(OQO00QC0Q0['SubjectPublicKey'].OQOQ0QOOQ0,OQO00QC0Q0['Algorithm']['Parameters'].O0OQ0QOOQ0)
else
if OOO00QC0Q0=OID_X25519 then
O00Q0QC0Q0.OQ0C00C0Q0(OQO00QC0Q0['SubjectPublicKey'].OQOQ0QOOQ0,OQCC000OQ0)
else
if OOO00QC0Q0=OID_Ed25519 then
O00Q0QC0Q0.OQ0C00C0Q0(OQO00QC0Q0['SubjectPublicKey'].OQOQ0QOOQ0,OCCC000OQ0)
else
raise EScError.Create(seInvalidPublicKeyAlgorithm);
OO0Q0QC0Q0.Assign(O00Q0QC0Q0);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
Result:=O00Q0QC0Q0;
end;
function TScCertificate.OCO00QC0Q0:boolean;
begin
OQOC0CC0Q0;
if OOOQ0QC0Q0=-1 then begin
try
if IssuerName.Equals(SubjectName)and O00O0QC0Q0(Self)then
OOOQ0QC0Q0:=1
else
OOOQ0QC0Q0:=0;
except
OOOQ0QC0Q0:=0;
end;
end;
Result:=OOOQ0QC0Q0=1;
end;
procedure TScCertificate.OCQ00QC0Q0;
begin
if O00Q0QC0Q0<>nil then begin
try
Assert(OO0Q0QC0Q0<>nil);
if not O00Q0QC0Q0.O0QOOCQ0Q0 then
raise EScError.Create(seCertKeyCannotBeChanged);
if not O00Q0QC0Q0.Equals(OO0Q0QC0Q0)then
raise EScError.Create(seCertKeyCannotBeChanged);
if not OO0Q0QC0Q0.O0QOOCQ0Q0 then
OO0Q0QC0Q0.OQ0C0CC0Q0(O00Q0QC0Q0);
except
O00Q0QC0Q0.OQ0C0CC0Q0(OO0Q0QC0Q0);
raise;
end;
end;
end;
function TScCertificate.OQOQ0QC0Q0:O0COCCC0Q0;
begin
Result:=O0COCCC0Q0(OOCC0CC0Q0);
end;
procedure TScCertificate.OCOQ0QC0Q0(O0QQ0QC0Q0:O0COCCC0Q0);
begin
OOCC0CC0Q0:=O0QQ0QC0Q0;
end;
function O0COCCC0Q0.OOCOCCC0Q0(OQCOCCC0Q0:integer):TScCertificate;
begin
Result:=TScCertificate(OQOO0CC0Q0(OQCOCCC0Q0));
end;
procedure O0COCCC0Q0.OCCOCCC0Q0(O00QQCC0Q0:integer;OO0QQCC0Q0:TScCertificate);
begin
O0QO0CC0Q0(O00QQCC0Q0,OO0QQCC0Q0);
end;
procedure O0COCCC0Q0.OQ0QQCC0Q0(const OC0QQCC0Q0:string);
begin
O0OO0CC0Q0(OC0QQCC0Q0);
end;
function O0COCCC0Q0.O0OQQCC0Q0(const OOOQQCC0Q0:string):TScCertificate;
begin
Result:=TScCertificate(OCCO0CC0Q0(OOOQQCC0Q0));
end;
function O0COCCC0Q0.OQOQQCC0Q0(const OCOQQCC0Q0:string):TScCertificate;
begin
Result:=TScCertificate(OQ0QCCC0Q0(OCOQQCC0Q0));
end;
procedure O0COCCC0Q0.O0QQQCC0Q0(OOQQQCC0Q0:TStrings);
begin
O0OQCCC0Q0(OOQQQCC0Q0);
end;
function O0COCCC0Q0.OQQQQCC0Q0(OCQQQCC0Q0:OQOCO0OOQ0):TScCertificate;
var
O0CQQCC0Q0:integer;
begin
O0QOOCOOQ0(OQ000CC0Q0);
try
for O0CQQCC0Q0:=0 to OC000CC0Q0.Count-1 do begin
Result:=OOCQQCC0Q0[O0CQQCC0Q0];
if OCQQQCC0Q0.Equals(Result.SubjectName)then
Exit;
end;
Result:=nil;
finally
OQQOOCOOQ0(OQ000CC0Q0);
end;
end;
function O0COCCC0Q0.OQO00CC0Q0:OCCC0CC0Q0;
begin
Result:=TScCertificate;
end;
function O0COCCC0Q0.OCO00CC0Q0:string;
begin
Result:='crt';
end;
class procedure TScCertificateChain.VerifyChain(O0CQOQQ0Q0:OOOQOQQ0Q0;
OOCQOQQ0Q0:TCRList;out OQCQOQQ0Q0:TScCertificateStatusSet);
var
OCCQOQQ0Q0:integer;
begin
OQCQOQQ0Q0:=[csUntrustedRoot];
if OOCQOQQ0Q0.Count=0 then
Exit;
for OCCQOQQ0Q0:=0 to OOCQOQQ0Q0.Count-2 do
TScCertificate(OOCQOQQ0Q0[OCCQOQQ0Q0]).VerifyCertificateChain(O0CQOQQ0Q0,TScCertificate(OOCQOQQ0Q0[OCCQOQQ0Q0+1]),OQCQOQQ0Q0);
TScCertificate(OOCQOQQ0Q0[OOCQOQQ0Q0.Count-1]).VerifyCertificateChain(O0CQOQQ0Q0,nil,OQCQOQQ0Q0);
end;
constructor O00COQQ0Q0.Create;
begin
inherited Create;
OC0COQQ0Q0:=OQ0O0C0OQ0.Create;
end;
destructor O00COQQ0Q0.Destroy;
begin
OC0COQQ0Q0.Free;
inherited;
end;
function O00COQQ0Q0.OOOCOQQ0Q0:OQ0O0C0OQ0;
var
OQOCOQQ0Q0:OQCOCQOOQ0;
begin
if Length(O0OCOQQ0Q0)>0 then begin
OQOCOQQ0Q0:=OQCOCQOOQ0.Create;
try
if not OQOCOQQ0Q0.OCQCOOOOQ0(OOCQQCOOQ0,O0OCOQQ0Q0)then
raise EScError.Create(seWrongDataFormat);
O00QCC0OQ0.OO0QCC0OQ0(OC0COQQ0Q0,OQOCOQQ0Q0.O0O0OOOOQ0);
SetLength(O0OCOQQ0Q0,0);
finally
OQOCOQQ0Q0.Free;
end;
end;
Result:=OC0COQQ0Q0;
end;
function O00COQQ0Q0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=O00COQQ0Q0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure O00COQQ0Q0.OCOQ00QOQ0(O000CO0OQ0:OQ0Q00QOQ0);
var
OOQCOQQ0Q0:O00COQQ0Q0;
begin
if(O000CO0OQ0=nil)or not IsClass(O000CO0OQ0,O00COQQ0Q0)then
OC0Q00QOQ0(O000CO0OQ0);
if Self<>O000CO0OQ0 then begin
OOQCOQQ0Q0:=O00COQQ0Q0(O000CO0OQ0);
OO0COQQ0Q0:=OOQCOQQ0Q0.OO0COQQ0Q0;
OQ0COQQ0Q0:=OOQCOQQ0Q0.OQ0COQQ0Q0;
OC0COQQ0Q0.OQOC00QOQ0(OOQCOQQ0Q0.OC0COQQ0Q0);
end;
end;
function OOCCOQQ0Q0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=O00COQQ0Q0;
end;
function OOCCOQQ0Q0.OQCCOQQ0Q0(OCCCOQQ0Q0:integer):O00COQQ0Q0;
begin
Result:=TObject(OQO000QOQ0[OCCCOQQ0Q0])as O00COQQ0Q0;
end;
procedure OOCCOQQ0Q0.O000OQQ0Q0(OO00OQQ0Q0:integer;OQ00OQQ0Q0:O00COQQ0Q0);
begin
OQO000QOQ0[OO00OQQ0Q0]:=OQ00OQQ0Q0;
end;
constructor OCQOCQC0Q0.Create(OOQC0CC0Q0:O0000CC0Q0=nil);
begin
inherited Create(OOQC0CC0Q0);
end;
procedure OCQOCQC0Q0.O0OC0CC0Q0;
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
FreeAndNil(OCCOCQC0Q0);
FreeAndNil(O00QQQC0Q0);
FreeAndNil(OO0QQQC0Q0);
FreeAndNil(OQ0QQQC0Q0);
FreeAndNil(OC0QQQC0Q0);
FreeAndNil(O0COCQC0Q0);
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
procedure OCQOCQC0Q0.OQ0C0CC0Q0(OC0C0CC0Q0:O0OQ0CC0Q0);
begin
if not IsClass(OC0C0CC0Q0,OCQOCQC0Q0)then
raise EScError.CreateFmt(SInvalidObjectClass,[OCQOCQC0Q0.ClassName,OC0C0CC0Q0.ClassName],seInvalidObjectClass);
O0QOOCOOQ0(OQOQ0CC0Q0);
try
Assert(O0COCQC0Q0=nil);
OOCOCQC0Q0:=OCQOCQC0Q0(OC0C0CC0Q0).OOCOCQC0Q0;
try
OCCOCQC0Q0:=OOOCCOOOQ0.Create;
OCCOCQC0Q0.OCOQ00QOQ0(OCQOCQC0Q0(OC0C0CC0Q0).OCCOCQC0Q0);
O00QQQC0Q0:=OQOCO0OOQ0.Create;
O00QQQC0Q0.OCOQ00QOQ0(OCQOCQC0Q0(OC0C0CC0Q0).O00QQQC0Q0);
OQCOCQC0Q0:=True;
O0COCQC0Q0:=OQCOCQOOQ0.Create;
if(OOCOCQC0Q0=nil)or not O0COCQC0Q0.OCQCOOOOQ0(O0CQQCOOQ0,OOCOCQC0Q0)then
raise EScError.Create(seWrongCRLContext);
except
FreeAndNil(O0COCQC0Q0);
FreeAndNil(OCCOCQC0Q0);
FreeAndNil(O00QQQC0Q0);
raise;
end;
O0QQ0CC0Q0:=OC0C0CC0Q0.O0QQ0CC0Q0;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function OCQOCQC0Q0.Equals(O0CC0CC0Q0:O0OQ0CC0Q0):boolean;
var
OOQCQQC0Q0:OCQOCQC0Q0;
begin
Result:=IsClass(O0CC0CC0Q0,OCQOCQC0Q0);
if not Result then
Exit;
OOQCQQC0Q0:=OCQOCQC0Q0(O0CC0CC0Q0);
OQOC0CC0Q0;
OOQCQQC0Q0.OQOC0CC0Q0;
Result:=Length(OOCOCQC0Q0)=Length(OOQCQQC0Q0.OOCOCQC0Q0);
if Result then
Result:=MemCompare(@OOCOCQC0Q0[0],@OOQCQQC0Q0.OOCOCQC0Q0[0],Length(OOCOCQC0Q0))=0;
end;
function OCQOCQC0Q0.OQQCQQC0Q0(OCQCQQC0Q0:TScCertificate;O0CCQQC0Q0:OC0OQ0OOQ0):boolean;
var
OOCCQQC0Q0,OQCCQQC0Q0:OOQQ00OOQ0;
OCCCQQC0Q0:OQCOOC0OQ0;
O000QQC0Q0:OCOO00OOQ0;
OO00QQC0Q0:boolean;
OQ00QQC0Q0,OC00QQC0Q0,O0O0QQC0Q0:integer;
begin
if(OCQCQQC0Q0=nil)or(O0CCQQC0Q0=nil)then
raise EScError.Create(seInvalidInputArgs);
O00CQQC0Q0;
Result:=False;
OCCCQQC0Q0:=OQCOOC0OQ0(OO0QQQC0Q0.O0CO0C0OQ0(OQCOOC0OQ0));
if OCCCQQC0Q0=nil then begin
if not OCQCQQC0Q0.IssuerName.Equals(O00QQQC0Q0)then
Exit;
Result:=True;
Exit;
end;
if OCCCQQC0Q0.O0QQ0C0OQ0 and(O0CCQQC0Q0.OQQQOC0OQ0.OOO000QOQ0>0)then begin
if not O0CCQQC0Q0.OQQQOC0OQ0[0].OQOC00OOQ0.Equals(O00QQQC0Q0)then
Exit;
end
else
if not OCQCQQC0Q0.IssuerName.Equals(O00QQQC0Q0)then
Exit;
if OCCCQQC0Q0.OOQQ0C0OQ0 then begin
O000QQC0Q0:=OCOO00OOQ0(OCQCQQC0Q0.Extensions.O0CO0C0OQ0(OCOO00OOQ0));
if(O000QQC0Q0<>nil)and O000QQC0Q0.OCCO00OOQ0 then
Exit;
end;
if OCCCQQC0Q0.OQQQ0C0OQ0 then begin
O000QQC0Q0:=OCOO00OOQ0(OCQCQQC0Q0.Extensions.O0CO0C0OQ0(OCOO00OOQ0));
if(O000QQC0Q0=nil)or not O000QQC0Q0.OCCO00OOQ0 then
Exit;
end;
if OCCCQQC0Q0.OCQQ0C0OQ0 then
Exit;
OO00QQC0Q0:=False;
for OQ00QQC0Q0:=0 to OCCCQQC0Q0.OQOQ0C0OQ0.OOO000QOQ0-1 do begin
OQCCQQC0Q0:=OCCCQQC0Q0.OQOQ0C0OQ0[OQ00QQC0Q0];
for OC00QQC0Q0:=0 to OCQCQQC0Q0.OCCOOQC0Q0.OCQCOC0OQ0.OOO000QOQ0-1 do begin
O0CCQQC0Q0:=OCQCQQC0Q0.OCCOOQC0Q0.OCQCOC0OQ0[OQ00QQC0Q0];
if O0CCQQC0Q0.O0QQOC0OQ0.OOO000QOQ0>0 then begin
for O0O0QQC0Q0:=0 to O0CCQQC0Q0.O0QQOC0OQ0.OOO000QOQ0-1 do begin
OOCCQQC0Q0:=O0CCQQC0Q0.O0QQOC0OQ0[O0O0QQC0Q0];
if SameText(OQCCQQC0Q0.OOOC00OOQ0,OOCCQQC0Q0.OOOC00OOQ0)then begin
OO00QQC0Q0:=True;
break;
end;
end;
end
else
for O0O0QQC0Q0:=0 to O0CCQQC0Q0.OQQQOC0OQ0.OOO000QOQ0-1 do begin
OOCCQQC0Q0:=O0CCQQC0Q0.OQQQOC0OQ0[O0O0QQC0Q0];
if SameText(OQCCQQC0Q0.OOOC00OOQ0,OOCCQQC0Q0.OOOC00OOQ0)then begin
OO00QQC0Q0:=True;
break;
end;
end;
if OO00QQC0Q0 then
break;
end;
if OO00QQC0Q0 then
break;
end;
if not OO00QQC0Q0 and(OCCCQQC0Q0.OQOQ0C0OQ0.OOO000QOQ0>0)then
Exit;
Result:=True;
end;
function OCQOCQC0Q0.OQQCQQC0Q0(OOO0QQC0Q0:OCQOCQC0Q0):boolean;
var
OCCCQQC0Q0,OQO0QQC0Q0:OQCOOC0OQ0;
OCO0QQC0Q0,O0Q0QQC0Q0:OQ00C0OOQ0;
begin
if OOO0QQC0Q0=nil then
raise EScError.Create(seInvalidInputArgs);
O00CQQC0Q0;
OOO0QQC0Q0.O00CQQC0Q0;
Result:=False;
if not O00QQQC0Q0.Equals(OOO0QQC0Q0.O00QQQC0Q0)then
Exit;
OCCCQQC0Q0:=OQCOOC0OQ0(OO0QQQC0Q0.O0CO0C0OQ0(OQCOOC0OQ0));
OQO0QQC0Q0:=OQCOOC0OQ0(OOO0QQC0Q0.OO0QQQC0Q0.O0CO0C0OQ0(OQCOOC0OQ0));
if(OCCCQQC0Q0<>nil)or(OQO0QQC0Q0<>nil)then begin
if(OCCCQQC0Q0=nil)or(OQO0QQC0Q0=nil)then
Exit;
if not OCCCQQC0Q0.OQOQ0C0OQ0.Equals(OQO0QQC0Q0.OQOQ0C0OQ0)or
(OCCCQQC0Q0.OCOQ0C0OQ0<>OQO0QQC0Q0.OCOQ0C0OQ0)or
(OCCCQQC0Q0.OOQQ0C0OQ0<>OQO0QQC0Q0.OOQQ0C0OQ0)or
(OCCCQQC0Q0.OQQQ0C0OQ0<>OQO0QQC0Q0.OQQQ0C0OQ0)or
(OCCCQQC0Q0.O0QQ0C0OQ0<>OQO0QQC0Q0.O0QQ0C0OQ0)or
(OCCCQQC0Q0.OCQQ0C0OQ0<>OQO0QQC0Q0.OCQQ0C0OQ0)
then
Exit;
end;
OCO0QQC0Q0:=OQ00C0OOQ0(OO0QQQC0Q0.O0CO0C0OQ0(OQ00C0OOQ0));
O0Q0QQC0Q0:=OQ00C0OOQ0(OOO0QQC0Q0.OO0QQQC0Q0.O0CO0C0OQ0(OQ00C0OOQ0));
if(OCO0QQC0Q0<>nil)or(O0Q0QQC0Q0<>nil)then begin
if(OCO0QQC0Q0=nil)or(O0Q0QQC0Q0=nil)then
Exit;
if not SameText(OCO0QQC0Q0.O0Q0C0OOQ0,O0Q0QQC0Q0.O0Q0C0OOQ0)or
not SameText(OCO0QQC0Q0.OOQ0C0OOQ0,O0Q0QQC0Q0.OOQ0C0OOQ0)or
not OCO0QQC0Q0.OQQ0C0OOQ0.Equals(O0Q0QQC0Q0.OQQ0C0OOQ0)
then
Exit;
end;
Result:=True;
end;
function OCQOCQC0Q0.OOQ0QQC0Q0(OQQ0QQC0Q0:TScCertificate;out OCQ0QQC0Q0:OQ0OQ0OOQ0):boolean;
var
O0C0QQC0Q0:OQCOCQOOQ0;
OOC0QQC0Q0:O00COQQ0Q0;
OQC0QQC0Q0,OCC0QQC0Q0:OQ0C0C0OQ0;
O00OQQC0Q0,OO0OQQC0Q0:string;
OQ0OQQC0Q0:O0CQ0C0OQ0;
OC0OQQC0Q0,O0OOQQC0Q0:OC0QOQOOQ0;
OOOOQQC0Q0:integer;
begin
if OQQ0QQC0Q0=nil then
raise EScError.Create(seInvalidInputArgs);
OQOC0CC0Q0;
Result:=False;
OO0OQQC0Q0:=OQQ0QQC0Q0.SerialNumber;
OCQ0QQC0Q0:=OO0OQ0OOQ0;
if OQ0QQQC0Q0<>nil then begin
OQC0QQC0Q0:=nil;
for OOOOQQC0Q0:=0 to OQ0QQQC0Q0.OOO000QOQ0-1 do begin
OOC0QQC0Q0:=OQ0QQQC0Q0[OOOOQQC0Q0];
OCC0QQC0Q0:=OQ0C0C0OQ0(OOC0QQC0Q0.O0CCOQQ0Q0.O0CO0C0OQ0(OQ0C0C0OQ0));
if OCC0QQC0Q0<>nil then
OQC0QQC0Q0:=OCC0QQC0Q0;
if SameText(OOC0QQC0Q0.OQQCOQQ0Q0,OO0OQQC0Q0)then
if OQC0QQC0Q0<>nil then
Result:=OQC0QQC0Q0.O0OC0C0OQ0.OO0000OOQ0(OQQ0QQC0Q0.IssuerName)
else
Result:=True;
if Result then begin
OQ0OQQC0Q0:=O0CQ0C0OQ0(OOC0QQC0Q0.O0CCOQQ0Q0.O0CO0C0OQ0(O0CQ0C0OQ0));
if OQ0OQQC0Q0<>nil then
OCQ0QQC0Q0:=OQ0OQQC0Q0.OQCQ0C0OQ0;
Break;
end;
end;
end
else
if OC0QQQC0Q0<>nil then begin
if OC0QQQC0Q0.IndexOf(OO0OQQC0Q0)>-1 then
Result:=True;
end
else begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
O0C0QQC0Q0:=OQCOCQOOQ0.Create;
try
if not O0C0QQC0Q0.OCQCOOOOQ0(OCQQQCOOQ0,OOCOCQC0Q0)then
raise EScError.Create(seWrongCRLContext);
OC0QQQC0Q0:=TStringList.Create;
OC0OQQC0Q0:=O0C0QQC0Q0['RevokedCertificates'];
for OOOOQQC0Q0:=0 to OC0OQQC0Q0.OQ0C0QOOQ0-1 do begin
O0OOQQC0Q0:=OC0OQQC0Q0.OO0C0QOOQ0[OOOOQQC0Q0];
O00OQQC0Q0:=O0OOQQC0Q0['UserCertificate'].OCOQ0QOOQ0;
OC0QQQC0Q0.Add(O00OQQC0Q0);
if not Result and SameText(O00OQQC0Q0,OO0OQQC0Q0)then
Result:=True;
end;
finally
O0C0QQC0Q0.Free;
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
end;
function OCQOCQC0Q0.OCOCQQC0Q0(OO0O0QC0Q0:TScCertificate):boolean;
var
OQ0O0QC0Q0,OC0O0QC0Q0:TBytes;
O0OO0QC0Q0:OOOOCOQOQ0;
OOOO0QC0Q0:OQ0Q0OOOQ0;
begin
if OO0O0QC0Q0=nil then
raise EScError.Create(seInvalidInputArgs);
TScCertificate.O0C00QC0Q0(OOQQQQC0Q0,O0OO0QC0Q0,OOOO0QC0Q0);
OO0O0QC0Q0.Key.OQCOOCQ0Q0.OQQQ0OOOQ0(OCCOCQC0Q0.OCQCCOOOQ0);
OQ0O0QC0Q0:=O0COCQC0Q0['TBSCertList'].O0OQ0QOOQ0;
OC0O0QC0Q0:=O0COCQC0Q0['SignatureValue'].OQOQ0QOOQ0;
Result:=OO0O0QC0Q0.Key.O0C0OCQ0Q0(OQ0O0QC0Q0,OC0O0QC0Q0,O0OO0QC0Q0,OOOO0QC0Q0);
end;
procedure OCQOCQC0Q0.OQOOQQC0Q0(OCOOQQC0Q0:TScCertificate;var O0QOQQC0Q0:TScCertificateStatusSet);
var
OOQOQQC0Q0:OOQ000OOQ0;
begin
if OCOOQQC0Q0=nil then
raise EScError.Create(seInvalidInputArgs);
OQOC0CC0Q0;
if not O00QQQC0Q0.Equals(OCOOQQC0Q0.SubjectName)then
Include(O0QOQQC0Q0,csIssuerNotEqualSubject);
OOQOQQC0Q0:=OCOOQQC0Q0.Extensions.O0CO0C0OQ0(O0CQC0OOQ0);
if OOQOQQC0Q0<>nil then
if not(O0QQC0OOQ0 in O0CQC0OOQ0(OOQOQQC0Q0).OQQCC0OOQ0)then
Include(O0QOQQC0Q0,csInvalidKeyUsage);
if not OCOCQQC0Q0(OCOOQQC0Q0)then
Include(O0QOQQC0Q0,csInvalidSignature);
end;
procedure OCQOCQC0Q0.OQQOQQC0Q0(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string='');
var
OCQCOQC0Q0:TFileStream;
begin
OCQCOQC0Q0:=TFileStream.Create(OOQCOQC0Q0,fmOpenRead);
try
OQQOQQC0Q0(OCQCOQC0Q0,OQQCOQC0Q0);
finally
OCQCOQC0Q0.Free;
end;
end;
procedure OCQOCQC0Q0.OQQOQQC0Q0(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string='');
var
OQCQCQC0Q0,OCCQCQC0Q0:TBytes;
O00CCQC0Q0,OO0CCQC0Q0:string;
begin
SetLength(OQCQCQC0Q0,OCQCOQC0Q0.Size-OCQCOQC0Q0.Position);
if Length(OQCQCQC0Q0)>0 then
OCQCOQC0Q0.Read(OQCQCQC0Q0[0],Length(OQCQCQC0Q0));
O00CCQC0Q0:=Encoding.ANSI.GetString(OQCQCQC0Q0);
OO0CCQC0Q0:=TScCertificate.OOQQ0QC0Q0(O00CCQC0Q0,'X509 CRL');
if OO0CCQC0Q0='' then
OO0CCQC0Q0:=TScCertificate.OOQQ0QC0Q0(O00CCQC0Q0,'CRL');
if OO0CCQC0Q0<>'' then
OCCQCQC0Q0:=TBase64.Decode(Encoding.Default.GetBytes(OO0CCQC0Q0))
else
OCCQCQC0Q0:=OQCQCQC0Q0;
O0QCQQC0Q0(OCCQCQC0Q0);
OO0C0CC0Q0;
end;
procedure OCQOCQC0Q0.O0QCQQC0Q0(const O0QQCQC0Q0:TBytes);
begin
O0QOOCOOQ0(OQOQ0CC0Q0);
try
OQCC0CC0Q0:=False;
OOCOCQC0Q0:=O0QQCQC0Q0;
OQCOCQC0Q0:=True;
Assert(O0COCQC0Q0=nil);
O0COCQC0Q0:=OQCOCQOOQ0.Create;
try
if(OOCOCQC0Q0=nil)or not O0COCQC0Q0.OCQCOOOOQ0(O0CQQCOOQ0,OOCOCQC0Q0)then
raise EScError.Create(seWrongCRLContext);
OCCOCQC0Q0:=OOOCCOOOQ0.Create;
O00QCC0OQ0.OO0QCC0OQ0(OCCOCQC0Q0,O0COCQC0Q0['SignatureAlgorithm']);
O00QQQC0Q0:=OQOCO0OOQ0.Create;
O00QCC0OQ0.OO0QCC0OQ0(O00QQQC0Q0,O0COCQC0Q0['Issuer']);
except
FreeAndNil(O0COCQC0Q0);
FreeAndNil(OCCOCQC0Q0);
FreeAndNil(O00QQQC0Q0);
raise;
end;
O0QQ0CC0Q0:=True;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
procedure OCQOCQC0Q0.OCQOQQC0Q0(const OQ0CCQC0Q0:string;const OC0CCQC0Q0:OQCCQ00OQ0=OOCCQ00OQ0);
var
O0OCCQC0Q0:TFileStream;
begin
O0OCCQC0Q0:=TFileStream.Create(OQ0CCQC0Q0,fmCreate);
try
OCQOQQC0Q0(O0OCCQC0Q0,OC0CCQC0Q0);
finally
O0OCCQC0Q0.Free;
end;
end;
procedure OCQOCQC0Q0.OCQOQQC0Q0(O0OCCQC0Q0:TStream;const OC0CCQC0Q0:OQCCQ00OQ0=OOCCQ00OQ0);
begin
if not OQCC0CC0Q0 then
raise EScError.Create(seCRLNotReady);
if OC0CCQC0Q0=OOCCQ00OQ0 then begin
O0OOC0QOQ0.OC0QQ0QOQ0(O0OCCQC0Q0,CRL_HEADER);
OCOO0QQ0Q0(O0OCCQC0Q0,TBase64.Encode(OOCOCQC0Q0),64,False);
O0OOC0QOQ0.OC0QQ0QOQ0(O0OCCQC0Q0,CRL_FOOTER);
end
else
O0OCCQC0Q0.WriteBuffer(OOCOCQC0Q0[0],Length(OOCOCQC0Q0));
end;
function OCQOCQC0Q0.OCOQQQC0Q0:integer;
begin
OQOC0CC0Q0;
Result:=O0COCQC0Q0['Version'].OQQQ0QOOQ0+1;
end;
function OCQOCQC0Q0.O0QQQQC0Q0:TBytes;
begin
OQOC0CC0Q0;
Result:=O0COCQC0Q0['SignatureValue'].OQOQ0QOOQ0;
end;
function OCQOCQC0Q0.OOQQQQC0Q0:OOOCCOOOQ0;
begin
OQOC0CC0Q0;
Result:=OCCOCQC0Q0;
end;
function OCQOCQC0Q0.OQQQQQC0Q0:string;
begin
OQOC0CC0Q0;
Result:=TScCertificate.O0OC0QC0Q0(O00QQQC0Q0);
end;
function OCQOCQC0Q0.OCQQQQC0Q0:OQOCO0OOQ0;
begin
OQOC0CC0Q0;
Result:=O00QQQC0Q0;
end;
function OCQOCQC0Q0.O0CQQQC0Q0:TDateTime;
begin
OQOC0CC0Q0;
Result:=O0COCQC0Q0['ThisUpdate'].OOCQ0QOOQ0;
end;
function OCQOCQC0Q0.OOCQQQC0Q0:TDateTime;
begin
OQOC0CC0Q0;
Result:=O0COCQC0Q0['NextUpdate'].OOCQ0QOOQ0;
end;
procedure OCQOCQC0Q0.O00CQQC0Q0;
begin
OQOC0CC0Q0;
O0QOOCOOQ0(OQOQ0CC0Q0);
try
if OO0QQQC0Q0=nil then begin
OO0QQQC0Q0:=OQ0O0C0OQ0.Create;
O00QCC0OQ0.OO0QCC0OQ0(OO0QQQC0Q0,O0COCQC0Q0['CrlExtensions']);
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function OCQOCQC0Q0.OQCQQQC0Q0:OQ0O0C0OQ0;
begin
O00CQQC0Q0;
Result:=OO0QQQC0Q0;
end;
procedure OCQOCQC0Q0.OO0CQQC0Q0;
var
OQ0CQQC0Q0,OC0CQQC0Q0:OC0QOQOOQ0;
O0OCQQC0Q0:O00COQQ0Q0;
OOOCQQC0Q0:OOCCOQQ0Q0;
OQOCQQC0Q0:integer;
begin
OQOC0CC0Q0;
O0QOOCOOQ0(OQOQ0CC0Q0);
try
if OQCOCQC0Q0 then begin
if not O0COCQC0Q0.OCQCOOOOQ0(OCQQQCOOQ0,OOCOCQC0Q0)then
raise EScError.Create(seWrongCRLContext);
OQCOCQC0Q0:=False;
end;
if OQ0QQQC0Q0=nil then begin
OQ0CQQC0Q0:=O0COCQC0Q0['RevokedCertificates'];
OOOCQQC0Q0:=OOCCOQQ0Q0.Create;
for OQOCQQC0Q0:=0 to OQ0CQQC0Q0.OQ0C0QOOQ0-1 do begin
O0OCQQC0Q0:=O00COQQ0Q0.Create;
OOOCQQC0Q0.OOQC00QOQ0(O0OCQQC0Q0);
OC0CQQC0Q0:=OQ0CQQC0Q0.OO0C0QOOQ0[OQOCQQC0Q0];
O0OCQQC0Q0.OO0COQQ0Q0:=OC0CQQC0Q0['UserCertificate'].OCOQ0QOOQ0;
O0OCQQC0Q0.OQ0COQQ0Q0:=OC0CQQC0Q0['RevocationDate'].OOCQ0QOOQ0;
O0OCQQC0Q0.O0OCOQQ0Q0:=OC0CQQC0Q0['CrlEntryExtensions'].O0OQ0QOOQ0;
end;
OQ0QQQC0Q0:=OOOCQQC0Q0;
end;
finally
OQQOOCOOQ0(OQOQ0CC0Q0);
end;
end;
function OCQOCQC0Q0.OCCQQQC0Q0:OOCCOQQ0Q0;
begin
OO0CQQC0Q0;
Result:=OQ0QQQC0Q0;
end;
function OCQOCQC0Q0.O0OQQQC0Q0:OQCQQCC0Q0;
begin
Result:=OQCQQCC0Q0(OOCC0CC0Q0);
end;
procedure OCQOCQC0Q0.OOOQQQC0Q0(OQOQQQC0Q0:OQCQQCC0Q0);
begin
OOCC0CC0Q0:=OQOQQQC0Q0;
end;
class function OCQOCQC0Q0.O0COQQC0Q0(const OOCOQQC0Q0:string):OCQOCQC0Q0;
var
OQCOQQC0Q0:TCRHttpWebRequest;
OCCOQQC0Q0:TCRHttpWebResponse;
O00QOOC0Q0:TMemoryStream;
begin
OCCOQQC0Q0:=nil;
OQCOQQC0Q0:=TCRHttpWebRequest.Create(OOCOQQC0Q0);
try
OCCOQQC0Q0:=OQCOQQC0Q0.GetResponse;
O00QOOC0Q0:=TMemoryStream.Create;
try
OCCOQQC0Q0.ReadToStream(O00QOOC0Q0);
O00QOOC0Q0.Position:=0;
Result:=OCQOCQC0Q0.Create(nil);
try
Result.OQQQOOC0Q0:=OQCQQCC0Q0.OO00QCC0Q0(OOCOQQC0Q0);
Result.OQQOQQC0Q0(O00QOOC0Q0);
except
Result.Free;
raise;
end;
finally
O00QOOC0Q0.Free;
end;
finally
OQCOQQC0Q0.Free;
OCCOQQC0Q0.Free;
end;
end;
class procedure OQCQQCC0Q0.O0OCQCC0Q0(OOOCQCC0Q0:TObject;OQOCQCC0Q0:OCCCQ00OQ0;
OCOCQCC0Q0:boolean;O0QCQCC0Q0:OOOQOQQ0Q0;
OOQCQCC0Q0:OCOC00OOQ0;OQQCQCC0Q0:boolean;out OCQCQCC0Q0:OCQOCQC0Q0);
var
O0CCQCC0Q0,OOCCQCC0Q0:OCQOCQC0Q0;
OQCCQCC0Q0,OCCCQCC0Q0:string;
O000QCC0Q0:integer;
begin
OCQCQCC0Q0:=nil;
OQCCQCC0Q0:='';
for O000QCC0Q0:=0 to OOQCQCC0Q0.OOO000QOQ0-1 do begin
OQCCQCC0Q0:=OOQCQCC0Q0.OCO000OOQ0[O000QCC0Q0].OOOC00OOQ0;
if Copy(OQCCQCC0Q0,1,4)='http' then
Break
else
OQCCQCC0Q0:='';
end;
if OQCCQCC0Q0<>'' then
OCCCQCC0Q0:=OQCQQCC0Q0.OO00QCC0Q0(OQCCQCC0Q0)
else
OCCCQCC0Q0:='';
if OCCCQCC0Q0<>'' then begin
if O00C0QQ0Q0<>nil then
OOCCQCC0Q0:=O00C0QQ0Q0.O00Q0CC0Q0.OQO0QCC0Q0(OCCCQCC0Q0)
else
OOCCQCC0Q0:=nil;
if OQOCQCC0Q0<>nil then
O0CCQCC0Q0:=OQOCQCC0Q0.O00Q0CC0Q0.OQO0QCC0Q0(OCCCQCC0Q0)
else
O0CCQCC0Q0:=nil;
if not OQQCQCC0Q0 then begin
if OOCCQCC0Q0=nil then
OCQCQCC0Q0:=O0CCQCC0Q0
else
if O0CCQCC0Q0=nil then
OCQCQCC0Q0:=OOCCQCC0Q0
else
if OOCCQCC0Q0.OCOQOOC0Q0>O0CCQCC0Q0.OCOQOOC0Q0 then
OCQCQCC0Q0:=OOCCQCC0Q0
else
OCQCQCC0Q0:=O0CCQCC0Q0;
if OCQCQCC0Q0<>nil then
Exit;
end;
try
if OCOCQCC0Q0 then begin
OCQCQCC0Q0:=OCQOCQC0Q0.O0COQQC0Q0(OQCCQCC0Q0);
if(OQOCQCC0Q0<>nil)and(O0CCQCC0Q0<>nil)then begin
OQOCQCC0Q0.O00Q0CC0Q0.OC0CCCC0Q0(O0CCQCC0Q0);
O0CCQCC0Q0.Free;
OQOCQCC0Q0.O00Q0CC0Q0.OQCQCCC0Q0(OCQCQCC0Q0);
end;
if O00C0QQ0Q0<>nil then begin
O00C0QQ0Q0.O00Q0CC0Q0.OC0CCCC0Q0(OOCCQCC0Q0);
OOCCQCC0Q0.Free;
O00C0QQ0Q0.O00Q0CC0Q0.OQCQCCC0Q0(OCQCQCC0Q0);
end;
if OCQCQCC0Q0<>nil then
Exit;
end;
except
end;
end;
if Assigned(O0QCQCC0Q0)then
O0QCQCC0Q0(OOOCQCC0Q0,OOQCQCC0Q0,OQQCQCC0Q0,OCQCQCC0Q0);
end;
class function OQCQQCC0Q0.OO00QCC0Q0(const OQ00QCC0Q0:string):string;
var
OC00QCC0Q0:THash_SHA1;
begin
OC00QCC0Q0:=THash_SHA1.Create;
try
Result:=O00QOCOOQ0(OC00QCC0Q0.ComputeHash(Encoding.UTF8.GetBytes(OQ00QCC0Q0)));
finally
OC00QCC0Q0.Free;
end;
end;
function OQCQQCC0Q0.OCCQQCC0Q0(O00CQCC0Q0:integer):OCQOCQC0Q0;
begin
Result:=OCQOCQC0Q0(OQOO0CC0Q0(O00CQCC0Q0));
end;
procedure OQCQQCC0Q0.OO0CQCC0Q0(OQ0CQCC0Q0:integer;OC0CQCC0Q0:OCQOCQC0Q0);
begin
O0QO0CC0Q0(OQ0CQCC0Q0,OC0CQCC0Q0);
end;
procedure OQCQQCC0Q0.O0O0QCC0Q0(const OOO0QCC0Q0:string);
begin
O0OO0CC0Q0(OOO0QCC0Q0);
end;
function OQCQQCC0Q0.OQO0QCC0Q0(const OCO0QCC0Q0:string):OCQOCQC0Q0;
begin
Result:=OCQOCQC0Q0(OCCO0CC0Q0(OCO0QCC0Q0));
end;
function OQCQQCC0Q0.O0Q0QCC0Q0(const OOQ0QCC0Q0:string):OCQOCQC0Q0;
begin
Result:=OCQOCQC0Q0(OQ0QCCC0Q0(OOQ0QCC0Q0));
end;
procedure OQCQQCC0Q0.OQQ0QCC0Q0(OCQ0QCC0Q0:TStrings);
begin
O0OQCCC0Q0(OCQ0QCC0Q0);
end;
function OQCQQCC0Q0.OQO00CC0Q0:OCCC0CC0Q0;
begin
Result:=OCQOCQC0Q0;
end;
function OQCQQCC0Q0.OCO00CC0Q0:string;
begin
Result:='crl';
end;
destructor O0O0OQQ0Q0.Destroy;
begin
OOO0OQQ0Q0.Free;
inherited;
end;
constructor OOC0QCC0Q0.Create;
begin
inherited;
OQC0QCC0Q0:=TCRList.Create;
OCC0QCC0Q0:=TCRList.Create;
O00OQCC0Q0:=TCRList.Create;
OO0OQCC0Q0:=TCRList.Create;
end;
destructor OOC0QCC0Q0.Destroy;
begin
OQQQOQC0Q0;
OQC0QCC0Q0.Free;
OCC0QCC0Q0.Free;
O00OQCC0Q0.Free;
OO0OQCC0Q0.Free;
inherited;
end;
procedure OOC0QCC0Q0.OQQQOQC0Q0;
procedure OCQQOQC0Q0(O0CQOQC0Q0:TCRList);
begin
while O0CQOQC0Q0.Count>0 do begin
{$IFNDEF AUTOREFCOUNT}
TObject(O0CQOQC0Q0[O0CQOQC0Q0.Count-1]).Free;
{$ELSE}
O0CQOQC0Q0[O0CQOQC0Q0.Count-1]:=nil;
{$ENDIF}
O0CQOQC0Q0.Delete(O0CQOQC0Q0.Count-1);
end;
end;
begin
OCQQOQC0Q0(OQC0QCC0Q0);
OCQQOQC0Q0(OCC0QCC0Q0);
OCQQOQC0Q0(O00OQCC0Q0);
OCQQOQC0Q0(OO0OQCC0Q0);
end;
class function OOC0QCC0Q0.OOCQOQC0Q0(OQCQOQC0Q0:TStream):string;
const
OCCQOQC0Q0:array[0..10]of byte=
($A1,$F8,$4E,$36,0,0,0,6,0,0,0);
O00COQC0Q0:array[0..15]of byte=
(192,52,216,49,28,2,206,248,81,240,20,75,129,237,75,242);
var
OO0COQC0Q0,OQ0COQC0Q0:integer;
OC0COQC0Q0,O0OCOQC0Q0,OOOCOQC0Q0,OQOCOQC0Q0:TBytes;
OCOCOQC0Q0:TSymmetricAlgorithm;
begin
if OQCQOQC0Q0.Size<46 then
raise EScError.Create(sePKCS12DataBroken);
SetLength(OC0COQC0Q0,13);
OQCQOQC0Q0.Read(OC0COQC0Q0[0],13);
if MemCompare(@OC0COQC0Q0[0],@OCCQOQC0Q0[0],3)<>0 then
raise EScError.Create(sePKCS12DataBroken);
if MemCompare(@OC0COQC0Q0[4],@OCCQOQC0Q0[4],7)<>0 then
raise EScError.Create(sePKCS12DataBroken);
if(OC0COQC0Q0[3]<>$36)and(OC0COQC0Q0[3]<>$37)then
raise EScError.Create(sePKCS12DataBroken);
if(OC0COQC0Q0[11]=33)and(OC0COQC0Q0[12]=6)then begin
SetLength(O0OCOQC0Q0,16);
SetLength(OOOCOQC0Q0,16);
SetLength(OQOCOQC0Q0,16);
OQCQOQC0Q0.Read(O0OCOQC0Q0[0],16);
OQCQOQC0Q0.Read(OQOCOQC0Q0[0],16);
Move(O00COQC0Q0[0],OOOCOQC0Q0[0],16);
OCOCOQC0Q0:=TCipher_Rijndael.Create;
OCOCOQC0Q0.Mode:=cmCBC;
OCOCOQC0Q0.Key:=O0OCOQC0Q0;
OCOCOQC0Q0.IV:=OOOCOQC0Q0;
OCOCOQC0Q0.CreateDecryptor.TransformBlock(OQOCOQC0Q0,0,Length(OQOCOQC0Q0));
Result:=Encoding.Default.GetString(OQOCOQC0Q0);
end
else
if(OC0COQC0Q0[11]=65)and(OC0COQC0Q0[12]=53)then begin
SetLength(O0OCOQC0Q0,16);
SetLength(OQOCOQC0Q0,48);
OQCQOQC0Q0.Read(O0OCOQC0Q0[0],16);
OQCQOQC0Q0.Read(OQOCOQC0Q0[0],48);
O0OCOQC0Q0:=OOOQOCOOQ0(Encoding.Default.GetString(O0OCOQC0Q0));
OQOCOQC0Q0:=OOOQOCOOQ0(Encoding.Default.GetString(OQOCOQC0Q0));
OCOCOQC0Q0:=TCipher_1DES.Create;
OCOCOQC0Q0.Mode:=cmCBC;
OCOCOQC0Q0.Key:=O0OCOQC0Q0;
OCOCOQC0Q0.CreateDecryptor.TransformBlock(OQOCOQC0Q0,0,Length(OQOCOQC0Q0));
OQ0COQC0Q0:=OQOCOQC0Q0[Length(OQOCOQC0Q0)-1];
if OQ0COQC0Q0>8 then
raise EScError.Create(sePKCS12DataBroken);
for OO0COQC0Q0:=1 to OQ0COQC0Q0 do
if OQOCOQC0Q0[Length(OQOCOQC0Q0)-OO0COQC0Q0]<>OQ0COQC0Q0 then
raise EScError.Create(sePKCS12DataBroken);
Result:=Encoding.Default.GetString(OQOCOQC0Q0,0,Length(OQOCOQC0Q0)-OQ0COQC0Q0);
end
else
raise EScError.Create(sePKCS12DataBroken);
if OQCQOQC0Q0.Position=OQCQOQC0Q0.Size then
raise EScError.Create(sePKCS12DataBroken);
end;
procedure OOC0QCC0Q0.O0QCOQC0Q0(const OOQCOQC0Q0:string;const OQQCOQC0Q0:string='');
var
OCQCOQC0Q0:TFileStream;
begin
OCQCOQC0Q0:=TFileStream.Create(OOQCOQC0Q0,fmOpenRead);
try
O0QCOQC0Q0(OCQCOQC0Q0,OQQCOQC0Q0);
finally
OCQCOQC0Q0.Free;
end;
end;
procedure OOC0QCC0Q0.O0QCOQC0Q0(OCQCOQC0Q0:TStream;const OQQCOQC0Q0:string='');
var
O0CCOQC0Q0:OQCOCQOOQ0;
OOCCOQC0Q0:OQCOCQOOQ0;
OQCCOQC0Q0,OCCCOQC0Q0:OC0QOQOOQ0;
O000OQC0Q0:OC0QOQOOQ0;
OO00OQC0Q0,OQ00OQC0Q0:OC0QOQOOQ0;
OC00OQC0Q0:string;
O0O0OQC0Q0:string;
OOO0OQC0Q0,OQO0OQC0Q0,OCO0OQC0Q0,O0Q0OQC0Q0:TBytes;
OOQ0OQC0Q0:TBytes;
OQQ0OQC0Q0:O00QCOOOQ0;
OCQ0OQC0Q0:TSymmetricAlgorithm;
O0C0OQC0Q0:integer;
OOC0OQC0Q0:OOOOCOQOQ0;
OQC0OQC0Q0:THMAC;
OCC0OQC0Q0,O00OOQC0Q0:O0O0OQQ0Q0;
OO0OOQC0Q0,OQ0OOQC0Q0:integer;
begin
OQQQOQC0Q0;
OQQ0OQC0Q0:=nil;
OOCCOQC0Q0:=nil;
O0CCOQC0Q0:=OQCOCQOOQ0.Create;
try
if OCQCOQC0Q0.Position=OCQCOQC0Q0.Size then
OCQCOQC0Q0.Position:=0;
if not O0CCOQC0Q0.OCQCOOOOQ0(OC0CQCOOQ0,OCQCOQC0Q0)then
raise EScError.Create(seWrongPKCS12Context);
if O0CCOQC0Q0['Version'].OQQQ0QOOQ0<>3 then
raise EScError.Create(seWrongPKCS12Context);
OQCCOQC0Q0:=O0CCOQC0Q0['AuthSafe'];
O0O0OQC0Q0:=OQCCOQC0Q0['ContentType'].OCOQ0QOOQ0;
if O0O0OQC0Q0=OID_DATA_TYPE then
OOQ0OQC0Q0:=OQCCOQC0Q0['Content'].OQOQ0QOOQ0
else
raise EScError.Create(seWrongPKCS12Context);
O000OQC0Q0:=O0CCOQC0Q0['MacData'];
SetLength(OOO0OQC0Q0,0);
SetLength(OQO0OQC0Q0,0);
if not O000OQC0Q0.OOOQ0QOOQ0 then begin
OC00OQC0Q0:=O000OQC0Q0['DigestAlgorithm']['Algorithm'].OCOQ0QOOQ0;
OOO0OQC0Q0:=O000OQC0Q0['MacSalt'].OQOQ0QOOQ0;
OQO0OQC0Q0:=O000OQC0Q0['Digest'].OQOQ0QOOQ0;
if O000OQC0Q0['Iters'].OOOQ0QOOQ0 then
O0C0OQC0Q0:=1
else
O0C0OQC0Q0:=O000OQC0Q0['Iters'].OQQQ0QOOQ0;
end
else begin
OC00OQC0Q0:='';
O0C0OQC0Q0:=0;
end;
if not O0CCOQC0Q0.OCQCOOOOQ0(O0OCQCOOQ0,OOQ0OQC0Q0)then
raise EScError.Create(seWrongPKCS12Context);
{$IFNDEF VER10P}
SetLength(OOQ0OQC0Q0,0);
SetLength(OCO0OQC0Q0,0);
SetLength(O0Q0OQC0Q0,0);
{$ENDIF}
OCCCOQC0Q0:=O0CCOQC0Q0['AuthSafes'];
if OC00OQC0Q0<>'' then begin
OOC0OQC0Q0:=OOCQCC0OQ0.OOCQQC0OQ0(OC00OQC0Q0);
O0Q0OQC0Q0:=OOQ0000OQ0.O0OCC00OQ0(OOC0OQC0Q0,OQQCOQC0Q0,
OOO0OQC0Q0,OQOC0O0OQ0,O0C0OQC0Q0,OOCQCC0OQ0.OCOCCC0OQ0(OOC0OQC0Q0).GetHashSize);
OQC0OQC0Q0:=THMAC.Create(OOCQCC0OQ0.OCOCCC0OQ0(OOC0OQC0Q0),O0Q0OQC0Q0);
try
OCO0OQC0Q0:=OQC0OQC0Q0.ComputeHash(OCCCOQC0Q0.O0OQ0QOOQ0);
finally
OQC0OQC0Q0.Free;
end;
if(Length(OQO0OQC0Q0)<>Length(OCO0OQC0Q0))or
(MemCompare(@OQO0OQC0Q0[0],@OCO0OQC0Q0[0],Length(OCO0OQC0Q0))<>0)then
raise EScError.Create(sePKCS12DataBroken);
end;
for OO0OOQC0Q0:=0 to OCCCOQC0Q0.OQ0C0QOOQ0-1 do begin
OO00OQC0Q0:=OCCCOQC0Q0.OO0C0QOOQ0[OO0OOQC0Q0];
O0O0OQC0Q0:=OO00OQC0Q0['ContentType'].OCOQ0QOOQ0;
if O0O0OQC0Q0=OID_DATA_TYPE then begin
OOQ0OQC0Q0:=OO00OQC0Q0['Content'].OQOQ0QOOQ0;
end
else
if O0O0OQC0Q0=OID_ENCRYPTED_DATA_TYPE then begin
if OOCCOQC0Q0=nil then
OOCCOQC0Q0:=OQCOCQOOQ0.Create;
if not OOCCOQC0Q0.OCQCOOOOQ0(OOOCQCOOQ0,OO00OQC0Q0['Content'].OQOQ0QOOQ0)then
raise EScError.Create(seWrongPKCS12Context);
OQ00OQC0Q0:=OOCCOQC0Q0['EncryptedContentInfo'];
if OQQ0OQC0Q0=nil then
OQQ0OQC0Q0:=O00QCOOOQ0.Create;
O00QCC0OQ0.OO0QCC0OQ0(OQQ0OQC0Q0,OQ00OQC0Q0['ContentEncryptionAlgorithm']);
OCQ0OQC0Q0:=OOQ0000OQ0.OO0QQ00OQ0(OQQ0OQC0Q0.OC0CCOOOQ0.OCOO0OOOQ0,
OQQ0OQC0Q0.O0OCCOOOQ0,OQQCOQC0Q0);
if OCQ0OQC0Q0=nil then
raise EScError.Create(seWrongPKCS12Context);
O0O0OQC0Q0:=OQ00OQC0Q0['ContentType'].OCOQ0QOOQ0;
OOQ0OQC0Q0:=OQ00OQC0Q0['EncryptedContent'].OQOQ0QOOQ0;
OCQ0OQC0Q0.CreateDecryptor.TransformBlock(OOQ0OQC0Q0,0,Length(OOQ0OQC0Q0));
end
else
raise EScError.Create(seWrongPKCS12Context);
O0OOQCC0Q0(OOQ0OQC0Q0,OQQCOQC0Q0);
end;
finally
O0CCOQC0Q0.Free;
OOCCOQC0Q0.Free;
OQQ0OQC0Q0.Free;
end;
for OO0OOQC0Q0:=0 to OCC0QCC0Q0.Count-1 do begin
O00OOQC0Q0:=O0O0OQQ0Q0(OCC0QCC0Q0[OO0OOQC0Q0]);
for OQ0OOQC0Q0:=0 to OQC0QCC0Q0.Count-1 do begin
OCC0OQC0Q0:=O0O0OQQ0Q0(OQC0QCC0Q0[OQ0OOQC0Q0]);
if(Length(O00OOQC0Q0.OCO0OQQ0Q0)=Length(OCC0OQC0Q0.OCO0OQQ0Q0))and
(MemCompare(@O00OOQC0Q0.OCO0OQQ0Q0[0],@OCC0OQC0Q0.OCO0OQQ0Q0[0],Length(O00OOQC0Q0.OCO0OQQ0Q0))=0)then begin
(O00OOQC0Q0.OCQ0OQQ0Q0 as TScCertificate).Key.Assign(OCC0OQC0Q0.OCQ0OQQ0Q0);
break;
end;
end;
end;
end;
procedure OOC0QCC0Q0.O0OOQCC0Q0(const OOOOQCC0Q0:TBytes;const OQOOQCC0Q0:string);
var
OCOOQCC0Q0:OQCOCQOOQ0;
O0QOQCC0Q0:OQCOCQOOQ0;
OOQOQCC0Q0,OQQOQCC0Q0:OC0QOQOOQ0;
OCQOQCC0Q0,O0COQCC0Q0,OOCOQCC0Q0:OC0QOQOOQ0;
OQCOQCC0Q0,OCCOQCC0Q0,O00QOQC0Q0:string;
OO0QOQC0Q0:TBytes;
OQ0QOQC0Q0:O0O0OQQ0Q0;
OC0QOQC0Q0:O0CQOOC0Q0;
O0OQOQC0Q0:TScCertificate;
OOOQOQC0Q0:OCQOCQC0Q0;
OQOQOQC0Q0,OCOQOQC0Q0:integer;
begin
SetLength(OO0QOQC0Q0,0);
O0QOQCC0Q0:=nil;
OCOOQCC0Q0:=OQCOCQOOQ0.Create;
try
if not OCOOQCC0Q0.OCQCOOOOQ0(OQOCQCOOQ0,OOOOQCC0Q0)then
raise EScError.Create(seWrongPKCS12Context);
OOQOQCC0Q0:=OCOOQCC0Q0['SafeBags'];
for OQOQOQC0Q0:=0 to OOQOQCC0Q0.OQ0C0QOOQ0-1 do begin
OQQOQCC0Q0:=OOQOQCC0Q0.OO0C0QOOQ0[OQOQOQC0Q0];
OQCOQCC0Q0:=OQQOQCC0Q0['BagID'].OCOQ0QOOQ0;
OO0QOQC0Q0:=OQQOQCC0Q0['BagValue'].OQOQ0QOOQ0;
OCQOQCC0Q0:=OQQOQCC0Q0['BagAttributes'];
OQ0QOQC0Q0:=O0O0OQQ0Q0.Create;
try
for OCOQOQC0Q0:=0 to OCQOQCC0Q0.OQ0C0QOOQ0-1 do begin
O0COQCC0Q0:=OCQOQCC0Q0.OO0C0QOOQ0[OCOQOQC0Q0];
OCCOQCC0Q0:=O0COQCC0Q0['AttrId'].OCOQ0QOOQ0;
OOCOQCC0Q0:=O0COQCC0Q0['AttrValues'];
if OOCOQCC0Q0.OQ0C0QOOQ0>0 then begin
if OCCOQCC0Q0=OID_PKCS12_ATTR_FriendlyName then
OQ0QOQC0Q0.OQO0OQQ0Q0:=OOCOQCC0Q0.OO0C0QOOQ0[0]['AttrValue'].OCOQ0QOOQ0
else
if OCCOQCC0Q0=OID_PKCS12_ATTR_LocalKeyID then
OQ0QOQC0Q0.OCO0OQQ0Q0:=OOCOQCC0Q0.OO0C0QOOQ0[0]['AttrValue'].OQOQ0QOOQ0;
end;
end;
if OQCOQCC0Q0=OID_KEY_BAG then begin
OC0QOQC0Q0:=O0CQOOC0Q0.Create;
OQ0QOQC0Q0.OOO0OQQ0Q0:=OC0QOQC0Q0;
OC0QOQC0Q0.OCOQQOC0Q0(OO0QOQC0Q0);
OC0QOQC0Q0.OOOCOOC0Q0;
OQC0QCC0Q0.Add(OQ0QOQC0Q0);
end
else
if OQCOQCC0Q0=OID_PKCS8_ENC_KEY_BAG then begin
OC0QOQC0Q0:=O0CQOOC0Q0.Create;
OQ0QOQC0Q0.OOO0OQQ0Q0:=OC0QOQC0Q0;
OC0QOQC0Q0.OCCQQOC0Q0(OO0QOQC0Q0,OQOOQCC0Q0);
OC0QOQC0Q0.OOOCOOC0Q0;
OQC0QCC0Q0.Add(OQ0QOQC0Q0);
end
else
if OQCOQCC0Q0=OID_CERT_BAG then begin
if O0QOQCC0Q0=nil then
O0QOQCC0Q0:=OQCOCQOOQ0.Create;
if not O0QOQCC0Q0.OCQCOOOOQ0(OCOCQCOOQ0,OO0QOQC0Q0)then
raise EScError.Create(seWrongPKCS12Context);
O0OQOQC0Q0:=TScCertificate.Create;
OQ0QOQC0Q0.OOO0OQQ0Q0:=O0OQOQC0Q0;
O00QOQC0Q0:=O0QOQCC0Q0['TypeID'].OCOQ0QOOQ0;
if O00QOQC0Q0=OID_X509_CERTIFICATE_BAG_TYPE then
O0OQOQC0Q0.SetRawData(nil,nil,O0QOQCC0Q0['Value'].OQOQ0QOOQ0)
else
if O00QOQC0Q0=OID_SDSI_CERTIFICATE_BAG_TYPE then
O0OQOQC0Q0.SetRawData(nil,nil,TBase64.Decode(O0QOQCC0Q0['Value'].OQOQ0QOOQ0))
else
raise EScError.Create(seWrongPKCS12Context);
OCC0QCC0Q0.Add(OQ0QOQC0Q0);
end
else
if OQCOQCC0Q0=OID_CRL_BAG then begin
if O0QOQCC0Q0=nil then
O0QOQCC0Q0:=OQCOCQOOQ0.Create;
if not O0QOQCC0Q0.OCQCOOOOQ0(OCOCQCOOQ0,OO0QOQC0Q0)then
raise EScError.Create(seWrongPKCS12Context);
OOOQOQC0Q0:=OCQOCQC0Q0.Create;
OQ0QOQC0Q0.OOO0OQQ0Q0:=OOOQOQC0Q0;
O00QOQC0Q0:=O0QOQCC0Q0['TypeID'].OCOQ0QOOQ0;
if O00QOQC0Q0=OID_X509CRL_BAG_TYPE then
OOOQOQC0Q0.O0QCQQC0Q0(O0QOQCC0Q0['Value'].OQOQ0QOOQ0)
else
raise EScError.Create(seWrongPKCS12Context);
O00OQCC0Q0.Add(OQ0QOQC0Q0);
end
else
if OQCOQCC0Q0=OID_SECRET_BAG then begin
if O0QOQCC0Q0=nil then
O0QOQCC0Q0:=OQCOCQOOQ0.Create;
if not O0QOQCC0Q0.OCQCOOOOQ0(OCOCQCOOQ0,OO0QOQC0Q0)then
raise EScError.Create(seWrongPKCS12Context);
OQ0QOQC0Q0.O0Q0OQQ0Q0:=O0QOQCC0Q0['TypeID'].OCOQ0QOOQ0;
OQ0QOQC0Q0.OOQ0OQQ0Q0:=O0QOQCC0Q0['Value'].OQOQ0QOOQ0;
OO0OQCC0Q0.Add(OQ0QOQC0Q0);
end
else
if OQCOQCC0Q0=OID_SAFE_CONTENTS_BAG then begin
FreeAndNil(OQ0QOQC0Q0);
O0OOQCC0Q0(OO0QOQC0Q0,OQOOQCC0Q0);
end;
except
OQ0QOQC0Q0.Free;
raise;
end;
end;
finally
OCOOQCC0Q0.Free;
O0QOQCC0Q0.Free;
end;
end;
class procedure O00OOQQ0Q0.OO0OOQQ0Q0(OQ0OOQQ0Q0:O0CQOOC0Q0;const OC0OOQQ0Q0:TBytes);
begin
OQ0OOQQ0Q0.OCQ0Q0C0Q0(OC0OOQQ0Q0);
end;
class function O00OOQQ0Q0.O0OOOQQ0Q0(OOOOOQQ0Q0:O0CQOOC0Q0):TBytes;
begin
Result:=OOOOOQQ0Q0.OQQ0Q0C0Q0;
end;
class procedure O00OOQQ0Q0.OQOOOQQ0Q0(OCOOOQQ0Q0:O0CQOOC0Q0;const O0QOOQQ0Q0:OC000O0OQ0);
begin
OCOOOQQ0Q0.O0OC0CC0Q0;
OCOOOQQ0Q0.OQ0OQ0C0Q0:=O0QOOQQ0Q0;
OCOOOQQ0Q0.OQCQOOC0Q0:=OCOQQOQOQ0;
OCOOOQQ0Q0.O0QQ0CC0Q0:=True;
end;
class procedure O00OOQQ0Q0.OOQOOQQ0Q0(OQQOOQQ0Q0:O0CQOOC0Q0;const O00QOCQ0Q0:TBytes;
const OO0QOCQ0Q0:OCQCQ00OQ0;const OQ0QOCQ0Q0:OOQQQOQOQ0;const OC0QOCQ0Q0:boolean);
begin
OQQOOQQ0Q0.OCCOQ0C0Q0(O00QOCQ0Q0,OO0QOCQ0Q0,OQ0QOCQ0Q0,OC0QOCQ0Q0);
end;
class function O00OOQQ0Q0.OCQOOQQ0Q0(O0COOQQ0Q0:O0CQOOC0Q0):TBytes;
begin
Result:=O0COOQQ0Q0.O0OQOCQ0Q0;
end;
class procedure O00OOQQ0Q0.OOCOOQQ0Q0(OQCOOQQ0Q0:O0CQOOC0Q0;const OQOQOCQ0Q0:TBytes);
begin
OQCOOQQ0Q0.OOOQOCQ0Q0(OQOQOCQ0Q0);
end;
class procedure OCCOOQQ0Q0.O00Q0QQ0Q0(OO0Q0QQ0Q0:TScCertificate;OOQ00QC0Q0:IntPtr;OQQ00QC0Q0:OQQQOQQ0Q0);
begin
OO0Q0QQ0Q0.O0Q00QC0Q0(OOQ00QC0Q0,OQQ00QC0Q0);
end;
initialization
OQCQ0QQ0Q0:=GetCurrentDir;
OCCQ0QQ0Q0:=TScRandomLFSR.Create;
O00C0QQ0Q0:=OO0Q0CQ0Q0.Create(nil);
OO0Q0CQ0Q0(O00C0QQ0Q0).OOQCCCQ0Q0:=GetHomePath+PathDelim {$IFDEF IOS}+'Documents'+PathDelim{$ENDIF}+'CRL';
finalization
OCCQ0QQ0Q0:=nil;
FreeAndNil(O00C0QQ0Q0);
end.
