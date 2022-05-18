//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsCertificateExtsUni;
interface
uses
Classes,SysUtils,
CLRClasses,CRTypes,CRFunctions,
CRHashAlgorithm,
{$IFNDEF UNIDACPRO}
TdsSSLConsts,TdsUtils,
TdsASN1,TdsOids,TdsCertificateConsts;
{$ELSE}
TdsSSLConstsUni,TdsUtilsUni,
TdsASN1Uni,TdsOidsUni,TdsCertificateConstsUni;
{$ENDIF}
type
OQ0Q0OOOQ0=(OOCOOOOOQ0,OQCOOOOOQ0,OCCOOOOOQ0,O00Q0OOOQ0,OO0Q0OOOQ0);
OC0Q0OOOQ0=class
private
O0OQ0OOOQ0:OOOOCOQOQ0;
OOOQ0OOOQ0:string;
OQOQ0OOOQ0:OOOOCOQOQ0;
OCOQ0OOOQ0:integer;
O0QQ0OOOQ0:integer;
public
constructor Create;
procedure OQQQ0OOOQ0(OCQQ0OOOQ0:OC0Q0OOOQ0);
procedure O0CQ0OOOQ0;
function OOCQ0OOOQ0:TBytes;
procedure OCCQ0OOOQ0(const O00C0OOOQ0:TBytes);
property O0OC0OOOQ0:OOOOCOQOQ0 read O0OQ0OOOQ0 write O0OQ0OOOQ0;
property OOOC0OOOQ0:OOOOCOQOQ0 read OQOQ0OOOQ0 write OQOQ0OOOQ0;
property OQOC0OOOQ0:integer read OCOQ0OOOQ0 write OCOQ0OOOQ0;
end;
OCOC0OOOQ0=class
private
O0QC0OOOQ0:OOOOCOQOQ0;
OOQC0OOOQ0:string;
OQQC0OOOQ0:OOOOCOQOQ0;
OCQC0OOOQ0:TBytes;
procedure O0CC0OOOQ0(OOCC0OOOQ0:OOOOCOQOQ0);
public
constructor Create;
procedure OCCC0OOOQ0(OCQQ0OOOQ0:OCOC0OOOQ0);
function O0000OOOQ0:TBytes;
procedure OO000OOOQ0(const O00C0OOOQ0:TBytes);
function OQ000OOOQ0:TBytes;
property O0O00OOOQ0:OOOOCOQOQ0 read O0QC0OOOQ0 write O0CC0OOOQ0;
property OOO00OOOQ0:OOOOCOQOQ0 read OQQC0OOOQ0 write OQQC0OOOQ0;
end;
OQO00OOOQ0=class(OQ0Q00QOQ0)
private
OCO00OOOQ0:string;
O0Q00OOOQ0:string;
OOQ00OOOQ0:TNotifyEvent;
OQQ00OOOQ0:TNotifyEvent;
procedure OCQ00OOOQ0(const O0C00OOOQ0:string);
function OOC00OOOQ0:string;
procedure OQC00OOOQ0(const OCC00OOOQ0:string);
protected
OO0O0OOOQ0:boolean;
procedure OQ0O0OOOQ0(OC0O0OOOQ0:boolean);
procedure O0OO0OOOQ0;
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create;overload;
constructor Create(const OQOO0OOOQ0:string);overload;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
property OCOO0OOOQ0:string read OCO00OOOQ0 write OCQ00OOOQ0;
property O0QO0OOOQ0:string read OOC00OOOQ0 write OQC00OOOQ0;
end;
OOQO0OOOQ0=class(OOQQ00QOQ0)
private
function OQQO0OOOQ0(OCQO0OOOQ0:integer):OQO00OOOQ0;
procedure O0CO0OOOQ0(OOCO0OOOQ0:integer;OQCO0OOOQ0:OQO00OOOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property OCCO0OOOQ0[Index:integer]:OQO00OOOQ0 read OQQO0OOOQ0 write O0CO0OOOQ0;default;
end;
O00QCOOOQ0=class(OQ0Q00QOQ0)
private
OO0QCOOOQ0:OQO00OOOQ0;
OQ0QCOOOQ0:TBytes;
procedure OC0QCOOOQ0(O0OQCOOOQ0:OQO00OOOQ0);
procedure OOOQCOOOQ0(const OQOQCOOOQ0:TBytes);
protected
OCOQCOOOQ0:boolean;
procedure O0QQCOOOQ0(OC0O0OOOQ0:boolean);
procedure OOQQCOOOQ0;
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OQQQCOOOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
procedure OQCQCOOOQ0;virtual;
public
constructor Create;virtual;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
procedure OO0CCOOOQ0;
procedure OQ0CCOOOQ0;
property OC0CCOOOQ0:OQO00OOOQ0 read OO0QCOOOQ0 write OC0QCOOOQ0;
property O0OCCOOOQ0:TBytes read OQ0QCOOOQ0 write OOOQCOOOQ0;
end;
OOOCCOOOQ0=class(O00QCOOOQ0)
private
OQOCCOOOQ0:OOOOCOQOQ0;
OCOCCOOOQ0:OQ0Q0OOOQ0;
O0QCCOOOQ0:OC0Q0OOOQ0;
protected
procedure OQCQCOOOQ0;override;
public
constructor Create;override;
destructor Destroy;override;
property OOQCCOOOQ0:OOOOCOQOQ0 read OQOCCOOOQ0;
property OQQCCOOOQ0:OQ0Q0OOOQ0 read OCOCCOOOQ0;
property OCQCCOOOQ0:OC0Q0OOOQ0 read O0QCCOOOQ0;
end;
O0CCCOOOQ0=class(OOQQ00QOQ0)
private
function OOCCCOOOQ0(OQCCCOOOQ0:integer):O00QCOOOQ0;
procedure OCCCCOOOQ0(O000COOOQ0:integer;OO00COOOQ0:O00QCOOOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property OQ00COOOQ0[Index:integer]:O00QCOOOQ0 read OOCCCOOOQ0 write OCCCCOOOQ0;default;
end;
OC00COOOQ0=class(OQ0Q00QOQ0)
private
O0O0COOOQ0:OQO00OOOQ0;
OOO0COOOQ0:O0OOQCOOQ0;
OQO0COOOQ0:TBytes;
procedure OCO0COOOQ0(O0OQCOOOQ0:OQO00OOOQ0);
function O0Q0COOOQ0:string;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create(const O0C0COOOQ0:string;const OOC0COOOQ0:TBytes;OQC0COOOQ0:O0OOQCOOQ0);overload;
constructor Create(const O0C0COOOQ0,OOC0COOOQ0:string;OQC0COOOQ0:O0OOQCOOQ0);overload;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
function Equals(OO0OCOOOQ0:OC00COOOQ0):boolean;reintroduce;
function OQ0OCOOOQ0:TBytes;
property O0OOCOOOQ0:OQO00OOOQ0 read O0O0COOOQ0 write OCO0COOOQ0;
property OOOOCOOOQ0:O0OOQCOOQ0 read OOO0COOOQ0;
property OQOOCOOOQ0:TBytes read OQO0COOOQ0;
property OCOOCOOOQ0:string read O0Q0COOOQ0;
end;
O0QOCOOOQ0=class(OOQQ00QOQ0)
private
function OOQOCOOOQ0(OQQOCOOOQ0:integer):OC00COOOQ0;
procedure OCQOCOOOQ0(O0COCOOOQ0:integer;OOCOCOOOQ0:OC00COOOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property OQCOCOOOQ0[Index:integer]:OC00COOOQ0 read OOQOCOOOQ0 write OCQOCOOOQ0;default;
end;
OCCOCOOOQ0=class(OQ0Q00QOQ0)
private
O00QQOOOQ0:OQO00OOOQ0;
OO0QQOOOQ0:O0QOCOOOQ0;
procedure OQ0QQOOOQ0(O0OQCOOOQ0:OQO00OOOQ0);
function OC0QQOOOQ0:integer;
function O0OQQOOOQ0(OOOQQOOOQ0:integer):OC00COOOQ0;
procedure OQOQQOOOQ0(OCOQQOOOQ0:integer;O0QQQOOOQ0:OC00COOOQ0);
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOQQQOOOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
public
constructor Create;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
function OO0CQOOOQ0:TBytes;
procedure O0OCQOOOQ0(OOOCQOOOQ0:OC00COOOQ0);overload;
procedure O0OCQOOOQ0(const OQOCQOOOQ0:TBytes;OCOCQOOOQ0:O0OOQCOOQ0);overload;
procedure O0OCQOOOQ0(const OQOCQOOOQ0:string;OCOCQOOOQ0:O0OOQCOOQ0);overload;
procedure O0QCQOOOQ0;
procedure OOQCQOOOQ0(OQQCQOOOQ0:integer);
property OCQCQOOOQ0:OQO00OOOQ0 read O00QQOOOQ0 write OQ0QQOOOQ0;
property O0CCQOOOQ0:integer read OC0QQOOOQ0;
property OOCCQOOOQ0[Index:integer]:OC00COOOQ0 read O0OQQOOOQ0 write OQOQQOOOQ0;
end;
OQCCQOOOQ0=class(OOQQ00QOQ0)
private
function OCCCQOOOQ0(OQQOCOOOQ0:integer):OCCOCOOOQ0;
procedure O000QOOOQ0(O0COCOOOQ0:integer;OOCOCOOOQ0:OCCOCOOOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
function OO00QOOOQ0:O0QCQCOOQ0;virtual;
procedure OQ00QOOOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
public
function O0O0QOOOQ0:TBytes;
property OQO0QOOOQ0[Index:integer]:OCCOCOOOQ0 read OCCCQOOOQ0 write O000QOOOQ0;default;
end;
OCO0QOOOQ0=class(OQ0Q00QOQ0)
private
O0Q0QOOOQ0:O0QOCOOOQ0;
function OOQ0QOOOQ0(OQQ0QOOOQ0:integer):OC00COOOQ0;
function OCQ0QOOOQ0(O0C0QOOOQ0:integer):string;
function OOC0QOOOQ0(const OQC0QOOOQ0:string):string;
function OCC0QOOOQ0(O00OQOOOQ0:integer):string;
function OO0OQOOOQ0:integer;
protected
function OQ0OQOOOQ0(const OC0OQOOOQ0:string):integer;
procedure OQOOQOOOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
function OQCOQOOOQ0:TBytes;
procedure OCCOQOOOQ0(O00QO0OOQ0:OC00COOOQ0);overload;
procedure OCCOQOOOQ0(const OO0QO0OOQ0,OQ0QO0OOQ0:string;OC0QO0OOQ0:O0OOQCOOQ0=OCO0QCOOQ0);overload;
procedure O0OQO0OOQ0;
procedure OOOQO0OOQ0(const OQOQO0OOQ0:string);
procedure O0QQO0OOQ0(const OOQQO0OOQ0:integer);
function Equals(OQQQO0OOQ0:OCO0QOOOQ0):boolean;reintroduce;
function ToString:string;{$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
property OO0CO0OOQ0:integer read OO0OQOOOQ0;
property OQ0CO0OOQ0[Index:integer]:OC00COOOQ0 read OOQ0QOOOQ0;default;
property OC0CO0OOQ0[Index:integer]:string read OCQ0QOOOQ0;
property O0OCO0OOQ0[const OId:string]:string read OOC0QOOOQ0;
property OOOCO0OOQ0[Index:integer]:string read OCC0QOOOQ0;
end;
OQOCO0OOQ0=class(OQ0Q00QOQ0)
private
OCOCO0OOQ0:TCRObjectList;
function O0QCO0OOQ0(OQQ0QOOOQ0:integer):OCO0QOOOQ0;
function OOQCO0OOQ0:integer;
function OQQCO0OOQ0(O0C0QOOOQ0:integer):string;
function OOCCO0OOQ0(const OQC0QOOOQ0:string):string;
function OO00O0OOQ0(O00OQOOOQ0:integer):string;
function O0O0O0OOQ0:integer;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OCO0O0OOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
public
constructor Create;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
procedure OOC0O0OOQ0(const OQC0O0OOQ0:TBytes;OCC0O0OOQ0,O00OO0OOQ0:integer);
function OO0OO0OOQ0:TBytes;
function OQ0OO0OOQ0(const OC0OQOOOQ0:string):integer;
procedure OQOOO0OOQ0(OCOOO0OOQ0:OCO0QOOOQ0);overload;
procedure OQOOO0OOQ0(const OO0QO0OOQ0,OQ0QO0OOQ0:string;OC0QO0OOQ0:O0OOQCOOQ0=OCO0QCOOQ0);overload;
procedure O0QOO0OOQ0;
procedure OOQOO0OOQ0(const OOQQO0OOQ0:integer);
function Equals(OQQQO0OOQ0:OQOCO0OOQ0):boolean;reintroduce;
function ToString:string;{$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
property OOCOO0OOQ0:integer read OOQCO0OOQ0;
property OQCOO0OOQ0[Index:integer]:OCO0QOOOQ0 read O0QCO0OOQ0;default;
property OCCOO0OOQ0[Index:integer]:string read OQQCO0OOQ0;
property O00Q00OOQ0[const OId:string]:string read OOCCO0OOQ0;
property OO0Q00OOQ0[Index:integer]:string read OO00O0OOQ0;
property OQ0Q00OOQ0:integer read O0O0O0OOQ0;
end;
OC0Q00OOQ0=class(OOQQ00QOQ0)
private
function O0OQ00OOQ0(O0C0QOOOQ0:integer):OQOCO0OOQ0;
procedure OOOQ00OOQ0(OQOQ00OOQ0:integer;OCOQ00OOQ0:OQOCO0OOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property O0QQ00OOQ0[Index:integer]:OQOCO0OOQ0 read O0OQ00OOQ0 write OOOQ00OOQ0;default;
end;
OOQQ00OOQ0=class(OQ0Q00QOQ0)
private
OQQQ00OOQ0:string;
OCQQ00OOQ0:string;
O0CQ00OOQ0:OQOCO0OOQ0;
procedure OOCQ00OOQ0(OQCQ00OOQ0:OQOCO0OOQ0);
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OCCQ00OOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
public
constructor Create;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
function Equals(OQ0C00OOQ0:OOQQ00OOQ0):boolean;reintroduce;overload;
function Equals(OC0C00OOQ0:OQOCO0OOQ0):boolean;reintroduce;overload;
function ToString:string;{$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
property O0OC00OOQ0:string read OQQQ00OOQ0 write OQQQ00OOQ0;
property OOOC00OOQ0:string read OCQQ00OOQ0 write OCQQ00OOQ0;
property OQOC00OOQ0:OQOCO0OOQ0 read O0CQ00OOQ0 write OOCQ00OOQ0;
end;
OCOC00OOQ0=class(OOQQ00QOQ0)
private
function O0QC00OOQ0(OOQC00OOQ0:integer):OOQQ00OOQ0;
procedure OQQC00OOQ0(OCQC00OOQ0:integer;O0CC00OOQ0:OOQQ00OOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
procedure OOCC00OOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
public
function Equals(OQQQO0OOQ0:OCOC00OOQ0):boolean;reintroduce;
function OO0000OOQ0(OQ0000OOQ0:OQOCO0OOQ0):boolean;
function O0O000OOQ0(const OOO000OOQ0:string):OOQQ00OOQ0;
property OCO000OOQ0[Index:integer]:OOQQ00OOQ0 read O0QC00OOQ0 write OQQC00OOQ0;default;
function ToString:string;{$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
end;
O0Q000OOQ0=class of OOQ000OOQ0;
OOQ000OOQ0=class(OQ0Q00QOQ0)
protected
OQQ000OOQ0:OQO00OOOQ0;
OCQ000OOQ0:boolean;
O0C000OOQ0:TBytes;
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);virtual;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);virtual;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
function ToString:string;{$IFDEF VER12P}override;{$ELSE}virtual;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
property O0OO00OOQ0:OQO00OOOQ0 read OQQ000OOQ0;
property OOOO00OOQ0:boolean read OCQ000OOQ0;
property OQOO00OOQ0:TBytes read O0C000OOQ0;
end;
OCOO00OOQ0=class(OOQ000OOQ0)
private
O0QO00OOQ0:boolean;
OOQO00OOQ0:boolean;
OQQO00OOQ0:integer;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
function ToString:string;override;
property OCCO00OOQ0:boolean read O0QO00OOQ0 write O0QO00OOQ0;
property O00QC0OOQ0:boolean read OOQO00OOQ0 write OOQO00OOQ0;
property OO0QC0OOQ0:integer read OQQO00OOQ0 write OQQO00OOQ0;
end;
OCQQC0OOQ0=(
OQ0QC0OOQ0,
OC0QC0OOQ0,
O0OQC0OOQ0,
OOOQC0OOQ0,
OQOQC0OOQ0,
OCOQC0OOQ0,
O0QQC0OOQ0,
OOQQC0OOQ0,
OQQQC0OOQ0
);
TScKeyUsageFlags=set of OCQQC0OOQ0;
O0CQC0OOQ0=class(OOQ000OOQ0)
private
OOCQC0OOQ0:TScKeyUsageFlags;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
function ToString:string;override;
property OQQCC0OOQ0:TScKeyUsageFlags read OOCQC0OOQ0 write OOCQC0OOQ0;
end;
OCQCC0OOQ0=class(OOQ000OOQ0)
private
O0CCC0OOQ0:OOQO0OOOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property OQCCC0OOQ0:OOQO0OOOQ0 read O0CCC0OOQ0;
end;
OCCCC0OOQ0=class(OOQ000OOQ0)
private
O000C0OOQ0:string;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
function ToString:string;override;
property OO00C0OOQ0:string read O000C0OOQ0 write O000C0OOQ0;
end;
OQ00C0OOQ0=class(OOQ000OOQ0)
private
OC00C0OOQ0:string;
O0O0C0OOQ0:string;
OOO0C0OOQ0:OCOC00OOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property O0Q0C0OOQ0:string read OC00C0OOQ0 write OC00C0OOQ0;
property OOQ0C0OOQ0:string read O0O0C0OOQ0 write O0O0C0OOQ0;
property OQQ0C0OOQ0:OCOC00OOQ0 read OOO0C0OOQ0;
end;
OCQ0C0OOQ0=record
O0C0C0OOQ0:string;
OOC0C0OOQ0:string;
OQC0C0OOQ0:string;
OCC0C0OOQ0:string;
end;
O00OC0OOQ0=class(OQ0Q00QOQ0)
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
OO0OC0OOQ0:string;
OQ0OC0OOQ0:array of OCQ0C0OOQ0;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
end;
OC0OC0OOQ0=class(OOQQ00QOQ0)
private
function O0OOC0OOQ0(OOOOC0OOQ0:integer):O00OC0OOQ0;
procedure OQOOC0OOQ0(OCOOC0OOQ0:integer;O0QOC0OOQ0:O00OC0OOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property OOQOC0OOQ0[Index:integer]:O00OC0OOQ0 read O0OOC0OOQ0 write OQOOC0OOQ0;default;
end;
OQQOC0OOQ0=class(OOQ000OOQ0)
private
OCQOC0OOQ0:OC0OC0OOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property OQOQQ0OOQ0:OC0OC0OOQ0 read OCQOC0OOQ0;
end;
OCOQQ0OOQ0=class(OQ0Q00QOQ0)
private
O0QQQ0OOQ0:OQO00OOOQ0;
OOQQQ0OOQ0:OQO00OOOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create;overload;
constructor Create(const OCQQQ0OOQ0,O0CQQ0OOQ0:string);overload;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
property OQCQQ0OOQ0:OQO00OOOQ0 read O0QQQ0OOQ0;
property OCCQQ0OOQ0:OQO00OOOQ0 read OOQQQ0OOQ0;
end;
O00CQ0OOQ0=class(OOQQ00QOQ0)
private
function OO0CQ0OOQ0(OQ0CQ0OOQ0:integer):OCOQQ0OOQ0;
procedure OC0CQ0OOQ0(O0OCQ0OOQ0:integer;OOOCQ0OOQ0:OCOQQ0OOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property OQOCQ0OOQ0[Index:integer]:OCOQQ0OOQ0 read OO0CQ0OOQ0 write OC0CQ0OOQ0;default;
end;
OCOCQ0OOQ0=class(OOQ000OOQ0)
private
O0QCQ0OOQ0:O00CQ0OOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property O0CCQ0OOQ0:O00CQ0OOQ0 read O0QCQ0OOQ0;
end;
OOCCQ0OOQ0=class(OOQ000OOQ0)
private
OQCCQ0OOQ0:OCOC00OOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property O000Q0OOQ0:OCOC00OOQ0 read OQCCQ0OOQ0;
end;
OO00Q0OOQ0=class(OOCCQ0OOQ0)
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
end;
OQ00Q0OOQ0=class(OOCCQ0OOQ0)
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
end;
OC00Q0OOQ0=class(OOQ000OOQ0)
private
O0O0Q0OOQ0:OQCCQOOOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property OCO0Q0OOQ0:OQCCQOOOQ0 read O0O0Q0OOQ0;
end;
OQ0OQ0OOQ0=(
O0Q0Q0OOQ0,
OOQ0Q0OOQ0,
OQQ0Q0OOQ0,
OCQ0Q0OOQ0,
O0C0Q0OOQ0,
OOC0Q0OOQ0,
OQC0Q0OOQ0,
OCC0Q0OOQ0,
O00OQ0OOQ0,
OO0OQ0OOQ0
);
TScCRLReasons=set of OQ0OQ0OOQ0;
OC0OQ0OOQ0=class(OQ0Q00QOQ0)
private
O0OOQ0OOQ0:OCOC00OOQ0;
OOOOQ0OOQ0:TScCRLReasons;
OQOOQ0OOQ0:OCOC00OOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
class procedure OCOOQ0OOQ0(O0QOQ0OOQ0:OC0QOQOOQ0;out OOQOQ0OOQ0:TScCRLReasons);
class function OC0QOC0OQ0(const O0OQOC0OQ0:TScCRLReasons):string;
public
constructor Create;overload;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
property O0QQOC0OQ0:OCOC00OOQ0 read O0OOQ0OOQ0;
property OOQQOC0OQ0:TScCRLReasons read OOOOQ0OOQ0;
property OQQQOC0OQ0:OCOC00OOQ0 read OQOOQ0OOQ0;
end;
OCQQOC0OQ0=class(OOQQ00QOQ0)
private
function O0CQOC0OQ0(OOCQOC0OQ0:integer):OC0OQ0OOQ0;
procedure OQCQOC0OQ0(OCCQOC0OQ0:integer;O00COC0OQ0:OC0OQ0OOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property OO0COC0OQ0[Index:integer]:OC0OQ0OOQ0 read O0CQOC0OQ0 write OQCQOC0OQ0;default;
end;
OQ0COC0OQ0=class(OOQ000OOQ0)
private
OC0COC0OQ0:OCQQOC0OQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property OCQCOC0OQ0:OCQQOC0OQ0 read OC0COC0OQ0;
end;
O0CCOC0OQ0=class(OQ0COC0OQ0)
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
end;
OOCCOC0OQ0=class(OQ0Q00QOQ0)
private
OQCCOC0OQ0:OQO00OOOQ0;
OCCCOC0OQ0:OOQQ00OOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
public
constructor Create;overload;
destructor Destroy;override;
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
property OQ00OC0OQ0:OQO00OOOQ0 read OQCCOC0OQ0;
property OC00OC0OQ0:OOQQ00OOQ0 read OCCCOC0OQ0;
end;
O0O0OC0OQ0=class(OOQQ00QOQ0)
private
function OOO0OC0OQ0(OQO0OC0OQ0:integer):OOCCOC0OQ0;
procedure OCO0OC0OQ0(O0Q0OC0OQ0:integer;OOQ0OC0OQ0:OOCCOC0OQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
property OQQ0OC0OQ0[Index:integer]:OOCCOC0OQ0 read OOO0OC0OQ0 write OCO0OC0OQ0;default;
end;
OCQ0OC0OQ0=class(OOQ000OOQ0)
private
O0C0OC0OQ0:O0O0OC0OQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property OO0OOC0OQ0:O0O0OC0OQ0 read O0C0OC0OQ0;
end;
OQ0OOC0OQ0=class(OOQ000OOQ0)
private
OC0OOC0OQ0:O0O0OC0OQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property OCOOOC0OQ0:O0O0OC0OQ0 read OC0OOC0OQ0;
end;
O0QOOC0OQ0=class(OOQ000OOQ0)
private
OOQOOC0OQ0:string;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
function ToString:string;override;
property OQQOOC0OQ0:string read OOQOOC0OQ0;
end;
OCQOOC0OQ0=class(OOQ000OOQ0)
private
O0COOC0OQ0:string;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
function ToString:string;override;
property OOCOOC0OQ0:string read O0COOC0OQ0;
end;
OQCOOC0OQ0=class(OOQ000OOQ0)
private
OCCOOC0OQ0:OCOC00OOQ0;
O00Q0C0OQ0:TScCRLReasons;
OO0Q0C0OQ0:boolean;
OQ0Q0C0OQ0:boolean;
OC0Q0C0OQ0:boolean;
O0OQ0C0OQ0:boolean;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property OQOQ0C0OQ0:OCOC00OOQ0 read OCCOOC0OQ0;
property OCOQ0C0OQ0:TScCRLReasons read O00Q0C0OQ0;
property O0QQ0C0OQ0:boolean read OO0Q0C0OQ0;
property OOQQ0C0OQ0:boolean read OQ0Q0C0OQ0;
property OQQQ0C0OQ0:boolean read OC0Q0C0OQ0;
property OCQQ0C0OQ0:boolean read O0OQ0C0OQ0;
end;
O0CQ0C0OQ0=class(OOQ000OOQ0)
private
OOCQ0C0OQ0:OQ0OQ0OOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
function ToString:string;override;
property OQCQ0C0OQ0:OQ0OQ0OOQ0 read OOCQ0C0OQ0;
end;
OCCQ0C0OQ0=class(OOQ000OOQ0)
private
O00C0C0OQ0:TDateTime;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
function ToString:string;override;
property OO0C0C0OQ0:TDateTime read O00C0C0OQ0;
end;
OQ0C0C0OQ0=class(OOQ000OOQ0)
private
OC0C0C0OQ0:OCOC00OOQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property O0OC0C0OQ0:OCOC00OOQ0 read OC0C0C0OQ0;
end;
OOOC0C0OQ0=class(OQ0Q00QOQ0)
private
OQOC0C0OQ0:integer;
OCOC0C0OQ0:TBytes;
O0QC0C0OQ0:TDateTime;
OOQC0C0OQ0:OOOOCOQOQ0;
OQQC0C0OQ0:OOQQQOQOQ0;
OCQC0C0OQ0:TBytes;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure O0CC0C0OQ0(const OOCC0C0OQ0:TBytes;OQCC0C0OQ0:integer);
public
procedure OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);override;
function ToString:string;{$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
property OO000C0OQ0:integer read OQOC0C0OQ0;
property OQ000C0OQ0:TBytes read OCOC0C0OQ0;
property OC000C0OQ0:TDateTime read O0QC0C0OQ0;
property O0O00C0OQ0:OOOOCOQOQ0 read OOQC0C0OQ0;
property OOO00C0OQ0:OOQQQOQOQ0 read OQQC0C0OQ0;
property OQO00C0OQ0:TBytes read OCQC0C0OQ0;
end;
OCO00C0OQ0=class(OOQQ00QOQ0)
private
function O0Q00C0OQ0(OOQ00C0OQ0:integer):OOOC0C0OQ0;
procedure OQQ00C0OQ0(OCQ00C0OQ0:integer;O0C00C0OQ0:OOOC0C0OQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
public
function ToString:string;{$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
property OOC00C0OQ0[Index:integer]:OOOC0C0OQ0 read O0Q00C0OQ0 write OQQ00C0OQ0;default;
end;
OQC00C0OQ0=class(OOQ000OOQ0)
private
OCC00C0OQ0:OCO00C0OQ0;
protected
function OQOQ00QOQ0:OQ0Q00QOQ0;override;
procedure OOC000OOQ0(const OQC000OOQ0:TBytes);override;
public
constructor Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);override;
destructor Destroy;override;
function ToString:string;override;
property OO0O0C0OQ0:OCO00C0OQ0 read OCC00C0OQ0;
end;
OQ0O0C0OQ0=class(OOQQ00QOQ0)
private
function OC0O0C0OQ0(O0OO0C0OQ0:integer):OOQ000OOQ0;
procedure OOOO0C0OQ0(OQOO0C0OQ0:integer;OCOO0C0OQ0:OOQ000OOQ0);
protected
function OC0C00QOQ0:O0QQ00QOQ0;override;
procedure O0QO0C0OQ0(OCQQCOOOQ0:OC0QOQOOQ0);
public
function O0CO0C0OQ0(OOCO0C0OQ0:O0Q000OOQ0):OOQ000OOQ0;
property OCCO0C0OQ0[Index:integer]:OOQ000OOQ0 read OC0O0C0OQ0 write OOOO0C0OQ0;default;
end;
O00QCC0OQ0=class
public
class procedure OO0QCC0OQ0(OQ0QCC0OQ0:O00QCOOOQ0;OCQQCOOOQ0:OC0QOQOOQ0);overload;
class procedure OO0QCC0OQ0(OQ0QCC0OQ0:OQOCO0OOQ0;OCQQCOOOQ0:OC0QOQOOQ0);overload;
class procedure OO0QCC0OQ0(OQ0QCC0OQ0:OQCCQOOOQ0;OCQQCOOOQ0:OC0QOQOOQ0);overload;
class procedure OO0QCC0OQ0(OQ0QCC0OQ0:OQ0O0C0OQ0;OCQQCOOOQ0:OC0QOQOOQ0);overload;
class procedure OC0QCC0OQ0(O0OQCC0OQ0:OQO00OOOQ0;OOOQCC0OQ0:TNotifyEvent);
class procedure OQOQCC0OQ0(OCOQCC0OQ0:OQO00OOOQ0;O0QQCC0OQ0:TNotifyEvent);
class procedure OOQQCC0OQ0(OQQQCC0OQ0:OQO00OOOQ0;OC0O0OOOQ0:boolean);overload;
class procedure OOQQCC0OQ0(OCQQCC0OQ0:O00QCOOOQ0;OC0O0OOOQ0:boolean);overload;
end;
implementation
uses
CRBase64,CRHash,CRDECUtil,CRCryptoTransformIntf,
CRHttp,
{$IFNDEF UNIDACPRO}
TdsReaderWriter,TdsAlgorithmSupport,TdsCipherSuites;
{$ELSE}
TdsReaderWriterUni,TdsAlgorithmSupportUni,TdsCipherSuitesUni;
{$ENDIF}
const
O0CQCC0OQ0:array[OQ0OQ0OOQ0]of string=(
'Key Compromise',
'CA Compromise',
'Affiliation Changed',
'Superseded',
'Cessation Of Operation',
'Certificate Hold',
'Privilege Withdrawn',
'AA Compromise',
'Remove From CRL',
'Unspecified'
);
constructor OC0Q0OOOQ0.Create;
begin
inherited;
O0CQ0OOOQ0;
end;
procedure OC0Q0OOOQ0.O0CQ0OOOQ0;
begin
O0OQ0OOOQ0:=OOC0COQOQ0;
OOOQ0OOOQ0:=OID_RSA_MGF1;
OQOQ0OOOQ0:=OOC0COQOQ0;
OCOQ0OOOQ0:=20;
O0QQ0OOOQ0:=0;
end;
procedure OC0Q0OOOQ0.OQQQ0OOOQ0(OCQQ0OOOQ0:OC0Q0OOOQ0);
begin
if OCQQ0OOOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
O0OQ0OOOQ0:=OCQQ0OOOQ0.O0OQ0OOOQ0;
OOOQ0OOOQ0:=OCQQ0OOOQ0.OOOQ0OOOQ0;
OQOQ0OOOQ0:=OCQQ0OOOQ0.OQOQ0OOOQ0;
OCOQ0OOOQ0:=OCQQ0OOOQ0.OCOQ0OOOQ0;
O0QQ0OOOQ0:=OCQQ0OOOQ0.O0QQ0OOOQ0;
end;
procedure OC0Q0OOOQ0.OCCQ0OOOQ0(const O00C0OOOQ0:TBytes);
var
OO0C0OOOQ0:OQCOCQOOQ0;
OQ0C0OOOQ0:OC0QOQOOQ0;
OC0C0OOOQ0:string;
begin
if Length(O00C0OOOQ0)=0 then
raise EScError.Create(seInvalidInputArgs);
OO0C0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OO0C0OOOQ0.OCQCOOOOQ0(OOQCCCOOQ0,O00C0OOOQ0)then
raise EScError.Create(seWrongDataFormat);
OC0C0OOOQ0:=OO0C0OOOQ0['HashAlgorithm']['Algorithm'].OCOQ0QOOQ0;
if OC0C0OOOQ0<>'' then
O0OQ0OOOQ0:=OOCQCC0OQ0.OOCQQC0OQ0(OC0C0OOOQ0)
else
O0OQ0OOOQ0:=OOC0COQOQ0;
OQ0C0OOOQ0:=OO0C0OOOQ0['MaskGenAlgorithm']['Algorithm'];
if not OQ0C0OOOQ0.OOOQ0QOOQ0 and(OQ0C0OOOQ0.OCOQ0QOOQ0<>OID_RSA_MGF1)then
raise EScError.Create(seUnknownMGFId);
OC0C0OOOQ0:=OO0C0OOOQ0['MaskGenAlgorithm']['Parameters']['Algorithm'].OCOQ0QOOQ0;
if OC0C0OOOQ0<>'' then
OQOQ0OOOQ0:=OOCQCC0OQ0.OOCQQC0OQ0(OC0C0OOOQ0)
else
OQOQ0OOOQ0:=OOC0COQOQ0;
if not OO0C0OOOQ0['SaltLength'].OOOQ0QOOQ0 then
OCOQ0OOOQ0:=OO0C0OOOQ0['SaltLength'].OQQQ0QOOQ0
else
OCOQ0OOOQ0:=20;
O0QQ0OOOQ0:=OO0C0OOOQ0['TrailerField'].OQQQ0QOOQ0;
finally
OO0C0OOOQ0.Free;
end;
end;
function OC0Q0OOOQ0.OOCQ0OOOQ0:TBytes;
var
OQCQ0OOOQ0:OQCOCQOOQ0;
begin
OQCQ0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OQCQ0OOOQ0.OCQCOOOOQ0(OOQCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
if O0OQ0OOOQ0<>OOC0COQOQ0 then
OQCQ0OOOQ0['HashAlgorithm']['Algorithm'].OCOQ0QOOQ0:=OOCQCC0OQ0.OCCQQC0OQ0(O0OQ0OOOQ0);
if OQOQ0OOOQ0<>OOC0COQOQ0 then begin
OQCQ0OOOQ0['MaskGenAlgorithm']['Algorithm'].OCOQ0QOOQ0:=OOOQ0OOOQ0;
OQCQ0OOOQ0['MaskGenAlgorithm']['Parameters']['Algorithm'].OCOQ0QOOQ0:=OOCQCC0OQ0.OCCQQC0OQ0(OQOQ0OOOQ0);
end;
if OCOQ0OOOQ0<>20 then
OQCQ0OOOQ0['SaltLength'].OQQQ0QOOQ0:=OCOQ0OOOQ0;
if O0QQ0OOOQ0<>0 then
OQCQ0OOOQ0['TrailerField'].OQQQ0QOOQ0:=O0QQ0OOOQ0;
Result:=OQCQ0OOOQ0.OOQCOOOOQ0;
finally
OQCQ0OOOQ0.Free;
end;
end;
constructor OCOC0OOOQ0.Create;
begin
inherited;
O0QC0OOOQ0:=OOC0COQOQ0;
OOQC0OOOQ0:=OID_RSA_MGF1;
OQQC0OOOQ0:=OOC0COQOQ0;
end;
procedure OCOC0OOOQ0.OCCC0OOOQ0(OCQQ0OOOQ0:OCOC0OOOQ0);
begin
if OCQQ0OOOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
O0QC0OOOQ0:=OCQQ0OOOQ0.O0QC0OOOQ0;
OOQC0OOOQ0:=OCQQ0OOOQ0.OOQC0OOOQ0;
OQQC0OOOQ0:=OCQQ0OOOQ0.OQQC0OOOQ0;
end;
procedure OCOC0OOOQ0.OO000OOOQ0(const O00C0OOOQ0:TBytes);
var
OO0C0OOOQ0:OQCOCQOOQ0;
OQ0C0OOOQ0:OC0QOQOOQ0;
OC0C0OOOQ0:string;
begin
if Length(O00C0OOOQ0)=0 then
raise EScError.Create(seInvalidInputArgs);
OO0C0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OO0C0OOOQ0.OCQCOOOOQ0(O0QCCCOOQ0,O00C0OOOQ0)then
raise EScError.Create(seWrongDataFormat);
OC0C0OOOQ0:=OO0C0OOOQ0['HashAlgorithm']['Algorithm'].OCOQ0QOOQ0;
if OC0C0OOOQ0<>'' then
O0QC0OOOQ0:=OOCQCC0OQ0.OOCQQC0OQ0(OC0C0OOOQ0)
else
O0QC0OOOQ0:=OOC0COQOQ0;
OQ0C0OOOQ0:=OO0C0OOOQ0['MaskGenAlgorithm']['Algorithm'];
if not OQ0C0OOOQ0.OOOQ0QOOQ0 and(OQ0C0OOOQ0.OCOQ0QOOQ0<>OID_RSA_MGF1)then
raise EScError.Create(seUnknownMGFId);
OC0C0OOOQ0:=OO0C0OOOQ0['MaskGenAlgorithm']['Parameters']['Algorithm'].OCOQ0QOOQ0;
if OC0C0OOOQ0<>'' then
OQQC0OOOQ0:=OOCQCC0OQ0.OOCQQC0OQ0(OC0C0OOOQ0)
else
OQQC0OOOQ0:=OOC0COQOQ0;
finally
OO0C0OOOQ0.Free;
end;
end;
function OCOC0OOOQ0.O0000OOOQ0:TBytes;
var
OQCQ0OOOQ0:OQCOCQOOQ0;
begin
OQCQ0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OQCQ0OOOQ0.OCQCOOOOQ0(O0QCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
if O0QC0OOOQ0<>OOC0COQOQ0 then
OQCQ0OOOQ0['HashAlgorithm']['Algorithm'].OCOQ0QOOQ0:=OOCQCC0OQ0.OCCQQC0OQ0(O0QC0OOOQ0);
if OQQC0OOOQ0<>OOC0COQOQ0 then begin
OQCQ0OOOQ0['MaskGenAlgorithm']['Algorithm'].OCOQ0QOOQ0:=OOQC0OOOQ0;
OQCQ0OOOQ0['MaskGenAlgorithm']['Parameters']['Algorithm'].OCOQ0QOOQ0:=OOCQCC0OQ0.OCCQQC0OQ0(OQQC0OOOQ0);
end;
Result:=OQCQ0OOOQ0.OOQCOOOOQ0;
finally
OQCQ0OOOQ0.Free;
end;
end;
function OCOC0OOOQ0.OQ000OOOQ0:TBytes;
var
OC000OOOQ0:THashAlgorithm;
begin
if OCQC0OOOQ0=nil then begin
OC000OOOQ0:=OOCQCC0OQ0.OOOCCC0OQ0(O0QC0OOOQ0);
try
OCQC0OOOQ0:=OC000OOOQ0.ComputeHash(nil);
finally
OC000OOOQ0.Free;
end;
end;
Result:=OCQC0OOOQ0;
end;
procedure OCOC0OOOQ0.O0CC0OOOQ0(OOCC0OOOQ0:OOOOCOQOQ0);
begin
if O0QC0OOOQ0<>OOCC0OOOQ0 then begin
O0QC0OOOQ0:=OOCC0OOOQ0;
OCQC0OOOQ0:=nil;
end;
end;
constructor OQO00OOOQ0.Create;
begin
inherited;
end;
constructor OQO00OOOQ0.Create(const OQOO0OOOQ0:string);
begin
inherited Create;
OCQ00OOOQ0(OQOO0OOOQ0);
end;
procedure OQO00OOOQ0.OQ0O0OOOQ0(OC0O0OOOQ0:boolean);
begin
OO0O0OOOQ0:=OC0O0OOOQ0;
end;
procedure OQO00OOOQ0.O0OO0OOOQ0;
begin
if OO0O0OOOQ0 then
raise EScError.Create(seChangingReadOnlyObject);
end;
function OQO00OOOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQO00OOOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OQO00OOOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OQO00OOOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
O0OO0OOOQ0;
if Self<>OCQQ0OOOQ0 then begin
if Assigned(OOQ00OOOQ0)then
OOQ00OOOQ0(Self);
OCO00OOOQ0:=OQO00OOOQ0(OCQQ0OOOQ0).OCO00OOOQ0;
O0Q00OOOQ0:=OQO00OOOQ0(OCQQ0OOOQ0).O0Q00OOOQ0;
if Assigned(OQQ00OOOQ0)then
OQQ00OOOQ0(Self);
end;
end;
procedure OQO00OOOQ0.OCQ00OOOQ0(const O0C00OOOQ0:string);
begin
O0OO0OOOQ0;
if OCO00OOOQ0<>O0C00OOOQ0 then begin
if Assigned(OOQ00OOOQ0)then
OOQ00OOOQ0(Self);
OCO00OOOQ0:=O0C00OOOQ0;
if O0C00OOOQ0='' then
O0Q00OOOQ0:=''
else
O0Q00OOOQ0:=OC0OOOOOQ0(O0C00OOOQ0);
if Assigned(OQQ00OOOQ0)then
OQQ00OOOQ0(Self);
end;
end;
function OQO00OOOQ0.OOC00OOOQ0:string;
begin
if O0Q00OOOQ0<>'' then
Result:=O0Q00OOOQ0
else
Result:=OCO00OOOQ0;
end;
procedure OQO00OOOQ0.OQC00OOOQ0(const OCC00OOOQ0:string);
var
O00O0OOOQ0:string;
begin
O0OO0OOOQ0;
if O0Q00OOOQ0<>OCC00OOOQ0 then begin
if Assigned(OOQ00OOOQ0)then
OOQ00OOOQ0(Self);
O0Q00OOOQ0:=OCC00OOOQ0;
O00O0OOOQ0:=OQOOOOOOQ0(OCC00OOOQ0);
if O00O0OOOQ0<>'' then
OCO00OOOQ0:=O00O0OOOQ0;
if Assigned(OQQ00OOOQ0)then
OQQ00OOOQ0(Self);
end;
end;
function OOQO0OOOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OQO00OOOQ0;
end;
function OOQO0OOOQ0.OQQO0OOOQ0(OCQO0OOOQ0:integer):OQO00OOOQ0;
begin
Result:=TObject(OQO000QOQ0[OCQO0OOOQ0])as OQO00OOOQ0;
end;
procedure OOQO0OOOQ0.O0CO0OOOQ0(OOCO0OOOQ0:integer;OQCO0OOOQ0:OQO00OOOQ0);
begin
OQO000QOQ0[OOCO0OOOQ0]:=OQCO0OOOQ0;
end;
constructor O00QCOOOQ0.Create;
begin
inherited Create;
OO0QCOOOQ0:=OQO00OOOQ0.Create;
end;
destructor O00QCOOOQ0.Destroy;
begin
OO0QCOOOQ0.Free;
inherited;
end;
procedure O00QCOOOQ0.OQCQCOOOQ0;
begin
end;
procedure O00QCOOOQ0.O0QQCOOOQ0(OC0O0OOOQ0:boolean);
begin
OCOQCOOOQ0:=OC0O0OOOQ0;
OO0QCOOOQ0.OQ0O0OOOQ0(OC0O0OOOQ0);
end;
procedure O00QCOOOQ0.OOQQCOOOQ0;
begin
if OCOQCOOOQ0 then
raise EScError.Create(seChangingReadOnlyObject);
end;
function O00QCOOOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=O00QCOOOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure O00QCOOOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,O00QCOOOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
OOQQCOOOQ0;
OO0QCOOOQ0.OCOQ00QOQ0(O00QCOOOQ0(OCQQ0OOOQ0).OO0QCOOOQ0);
OQ0QCOOOQ0:=O00QCOOOQ0(OCQQ0OOOQ0).OQ0QCOOOQ0;
OQCQCOOOQ0;
end;
procedure O00QCOOOQ0.OQ0CCOOOQ0;
begin
OOQQCOOOQ0;
OO0QCOOOQ0.OCOO0OOOQ0:='';
SetLength(OQ0QCOOOQ0,0);
OQCQCOOOQ0;
end;
procedure O00QCOOOQ0.OO0CCOOOQ0;
begin
OOQQCOOOQ0;
OO0QCOOOQ0.OCOO0OOOQ0:=OID_RSA_ENCRYPTION;
SetLength(OQ0QCOOOQ0,2);
OQ0QCOOOQ0[0]:=5;
OQ0QCOOOQ0[1]:=0;
OQCQCOOOQ0;
end;
procedure O00QCOOOQ0.OQQQCOOOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
var
O0CQCOOOQ0:OC0QOQOOQ0;
OOCQCOOOQ0:boolean;
begin
OOCQCOOOQ0:=OCOQCOOOQ0;
O0QQCOOOQ0(False);
try
OO0QCOOOQ0.OCOO0OOOQ0:=OCQQCOOOQ0['Algorithm'].O0CQ0QOOQ0;
O0CQCOOOQ0:=OCQQCOOOQ0['Parameters'];
if O0CQCOOOQ0.OOOQ0QOOQ0 then
SetLength(OQ0QCOOOQ0,0)
else
OQ0QCOOOQ0:=O0CQCOOOQ0.O0OQ0QOOQ0;
OQCQCOOOQ0;
finally
O0QQCOOOQ0(OOCQCOOOQ0);
end;
end;
procedure O00QCOOOQ0.OC0QCOOOQ0(O0OQCOOOQ0:OQO00OOOQ0);
begin
OOQQCOOOQ0;
OO0QCOOOQ0.OCOQ00QOQ0(O0OQCOOOQ0);
OQCQCOOOQ0;
end;
procedure O00QCOOOQ0.OOOQCOOOQ0(const OQOQCOOOQ0:TBytes);
begin
OOQQCOOOQ0;
SetLength(OQ0QCOOOQ0,Length(OQOQCOOOQ0));
if Length(OQOQCOOOQ0)>0 then
Move(OQOQCOOOQ0[0],OQ0QCOOOQ0[0],Length(OQOQCOOOQ0));
OQCQCOOOQ0;
end;
constructor OOOCCOOOQ0.Create;
begin
inherited;
O0QCCOOOQ0:=OC0Q0OOOQ0.Create;
end;
destructor OOOCCOOOQ0.Destroy;
begin
O0QCCOOOQ0.Free;
inherited;
end;
procedure OOOCCOOOQ0.OQCQCOOOQ0;
begin
if(OO0QCOOOQ0.OCOO0OOOQ0=OID_Ed25519)or(OO0QCOOOQ0.OCOO0OOOQ0=OID_X25519)then begin
OQOCCOOOQ0:=O0C0COQOQ0;
OCOCCOOOQ0:=OO0Q0OOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_SHA1_WITH_RSA_ENC then begin
OQOCCOOOQ0:=OOC0COQOQ0;
OCOCCOOOQ0:=OOCOOOOOQ0;
end
else
if(OO0QCOOOQ0.OCOO0OOOQ0=OID_DSA_WITH_SHA1)or(OO0QCOOOQ0.OCOO0OOOQ0=OID_ECDSA_WITH_SHA1)then begin
OQOCCOOOQ0:=OOC0COQOQ0;
OCOCCOOOQ0:=OO0Q0OOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_RSA_PSS_ENCRYPTION then begin
OQOCCOOOQ0:=OOC0COQOQ0;
OCOCCOOOQ0:=O00Q0OOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_SHA256_WITH_RSA_ENC then begin
OQOCCOOOQ0:=OQC0COQOQ0;
OCOCCOOOQ0:=OOCOOOOOQ0;
end
else
if(OO0QCOOOQ0.OCOO0OOOQ0=OID_DSA_WITH_SHA256)or(OO0QCOOOQ0.OCOO0OOOQ0=OID_ECDSA_WITH_SHA256)then begin
OQOCCOOOQ0:=OQC0COQOQ0;
OCOCCOOOQ0:=OO0Q0OOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_SHA224_WITH_RSA_ENC then begin
OQOCCOOOQ0:=O00OCOQOQ0;
OCOCCOOOQ0:=OOCOOOOOQ0;
end
else
if(OO0QCOOOQ0.OCOO0OOOQ0=OID_DSA_WITH_SHA224)or(OO0QCOOOQ0.OCOO0OOOQ0=OID_ECDSA_WITH_SHA224)then begin
OQOCCOOOQ0:=O00OCOQOQ0;
OCOCCOOOQ0:=OO0Q0OOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_SHA512_WITH_RSA_ENC then begin
OQOCCOOOQ0:=OCC0COQOQ0;
OCOCCOOOQ0:=OOCOOOOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_ECDSA_WITH_SHA512 then begin
OQOCCOOOQ0:=OCC0COQOQ0;
OCOCCOOOQ0:=OO0Q0OOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_SHA384_WITH_RSA_ENC then begin
OQOCCOOOQ0:=OO0OCOQOQ0;
OCOCCOOOQ0:=OOCOOOOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_ECDSA_WITH_SHA384 then begin
OQOCCOOOQ0:=OO0OCOQOQ0;
OCOCCOOOQ0:=OO0Q0OOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_MD5_WITH_RSA_ENC then begin
OQOCCOOOQ0:=OQ0OCOQOQ0;
OCOCCOOOQ0:=OOCOOOOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_MD4_WITH_RSA_ENC then begin
OQOCCOOOQ0:=OC0OCOQOQ0;
OCOCCOOOQ0:=OOCOOOOOQ0;
end
else
if OO0QCOOOQ0.OCOO0OOOQ0=OID_MD2_WITH_RSA_ENC then begin
OQOCCOOOQ0:=O0OOCOQOQ0;
OCOCCOOOQ0:=OOCOOOOOQ0;
end
else
raise EScError.Create(seUnknownHashAlgorithm);
if OCOCCOOOQ0=O00Q0OOOQ0 then
O0QCCOOOQ0.OCCQ0OOOQ0(OQ0QCOOOQ0)
else
O0QCCOOOQ0.O0CQ0OOOQ0;
end;
function O0CCCOOOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=O00QCOOOQ0;
end;
function O0CCCOOOQ0.OOCCCOOOQ0(OQCCCOOOQ0:integer):O00QCOOOQ0;
begin
Result:=TObject(OQO000QOQ0[OQCCCOOOQ0])as O00QCOOOQ0;
end;
procedure O0CCCOOOQ0.OCCCCOOOQ0(O000COOOQ0:integer;OO00COOOQ0:O00QCOOOQ0);
begin
OQO000QOQ0[O000COOOQ0]:=OO00COOOQ0;
end;
constructor OC00COOOQ0.Create(const O0C0COOOQ0:string;const OOC0COOOQ0:TBytes;OQC0COOOQ0:O0OOQCOOQ0);
begin
inherited Create;
O0O0COOOQ0:=OQO00OOOQ0.Create(O0C0COOOQ0);
OOO0COOOQ0:=OQC0COOOQ0;
OQO0COOOQ0:=OOC0COOOQ0;
end;
constructor OC00COOOQ0.Create(const O0C0COOOQ0,OOC0COOOQ0:string;OQC0COOOQ0:O0OOQCOOQ0);
var
OCC0COOOQ0:O0COQCOOQ0;
begin
inherited Create;
O0O0COOOQ0:=OQO00OOOQ0.Create(O0C0COOOQ0);
OOO0COOOQ0:=OQC0COOOQ0;
OCC0COOOQ0.OO0QOQOOQ0:=OOO0COOOQ0;
OQCCCQOOQ0.OO00CQOOQ0(OCC0COOOQ0,OOC0COOOQ0);
OQO0COOOQ0:=OCC0COOOQ0.OOCOQCOOQ0;
end;
destructor OC00COOOQ0.Destroy;
begin
O0O0COOOQ0.Free;
inherited;
end;
function OC00COOOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OC00COOOQ0.Create(O0O0COOOQ0.OCOO0OOOQ0,OQO0COOOQ0,OOO0COOOQ0);
end;
procedure OC00COOOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OC00COOOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
O0O0COOOQ0.OCOQ00QOQ0(OC00COOOQ0(OCQQ0OOOQ0).O0O0COOOQ0);
OOO0COOOQ0:=OC00COOOQ0(OCQQ0OOOQ0).OOO0COOOQ0;
OQO0COOOQ0:=OC00COOOQ0(OCQQ0OOOQ0).OQO0COOOQ0;
end;
function OC00COOOQ0.Equals(OO0OCOOOQ0:OC00COOOQ0):boolean;
begin
Result:=(OOO0COOOQ0=OO0OCOOOQ0.OOO0COOOQ0)and
(Length(OQO0COOOQ0)=Length(OO0OCOOOQ0.OQO0COOOQ0))and
(MemCompare(@OQO0COOOQ0[0],@OO0OCOOOQ0.OQO0COOOQ0[0],Length(OQO0COOOQ0))=0);
end;
function OC00COOOQ0.OQ0OCOOOQ0:TBytes;
var
OQCQ0OOOQ0:OQCOCQOOQ0;
OC0OCOOOQ0:OC0QOQOOQ0;
begin
OQCQ0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OQCQ0OOOQ0.OCQCOOOOQ0(OQQCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OQCQ0OOOQ0['AttrType'].O0CQ0QOOQ0:=O0O0COOOQ0.OCOO0OOOQ0;
OC0OCOOOQ0:=OQCQ0OOOQ0['AttrValue'];
OC0OCOOOQ0.OQCQ0QOOQ0:=OOO0COOOQ0;
OC0OCOOOQ0.OQOQ0QOOQ0:=OQO0COOOQ0;
Result:=OQCQ0OOOQ0.OOQCOOOOQ0;
finally
OQCQ0OOOQ0.Free;
end;
end;
function OC00COOOQ0.O0Q0COOOQ0:string;
var
OOQ0COOOQ0:O0COQCOOQ0;
OQQ0COOOQ0:TMemoryStream;
begin
OQQ0COOOQ0:=TMemoryStream.Create;
try
OQQ0COOOQ0.WriteBuffer(OQO0COOOQ0[0],Length(OQO0COOOQ0));
OOQ0COOOQ0.OQCOQCOOQ0:=0;
OOQ0COOOQ0.O00QOQOOQ0:=0;
OOQ0COOOQ0.OCCOQCOOQ0:=Length(OQO0COOOQ0);
OOQ0COOOQ0.OO0QOQOOQ0:=OOO0COOOQ0;
Result:=O0C00QOOQ0.O00O0QOOQ0(OQQ0COOOQ0,OOQ0COOOQ0);
finally
OQQ0COOOQ0.Free;
end;
end;
procedure OC00COOOQ0.OCO0COOOQ0(O0OQCOOOQ0:OQO00OOOQ0);
begin
O0O0COOOQ0.OCOQ00QOQ0(O0OQCOOOQ0);
end;
function O0QOCOOOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OC00COOOQ0;
end;
function O0QOCOOOQ0.OOQOCOOOQ0(OQQOCOOOQ0:integer):OC00COOOQ0;
begin
Result:=TObject(OQO000QOQ0[OQQOCOOOQ0])as OC00COOOQ0;
end;
procedure O0QOCOOOQ0.OCQOCOOOQ0(O0COCOOOQ0:integer;OOCOCOOOQ0:OC00COOOQ0);
begin
if OOCOCOOOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[O0COCOOOQ0]:=OOCOCOOOQ0;
end;
constructor OCCOCOOOQ0.Create;
begin
inherited Create;
OO0QQOOOQ0:=O0QOCOOOQ0.Create;
O00QQOOOQ0:=OQO00OOOQ0.Create;
end;
destructor OCCOCOOOQ0.Destroy;
begin
O0QCQOOOQ0;
O00QQOOOQ0.Free;
OO0QQOOOQ0.Free;
inherited;
end;
procedure OCCOCOOOQ0.OOQQQOOOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
var
OQQQQOOOQ0,OCQQQOOOQ0:OC0QOQOOQ0;
O0CQQOOOQ0:integer;
begin
O0QCQOOOQ0;
O00QQOOOQ0.OCOO0OOOQ0:=OCQQCOOOQ0['Type'].O0CQ0QOOQ0;
OQQQQOOOQ0:=OCQQCOOOQ0['Values'];
for O0CQQOOOQ0:=0 to OQQQQOOOQ0.OQ0C0QOOQ0-1 do begin
OCQQQOOOQ0:=OQQQQOOOQ0.OO0C0QOOQ0[O0CQQOOOQ0]['Value'];
O0OCQOOOQ0(OC00COOOQ0.Create(O00QQOOOQ0.OCOO0OOOQ0,OCQQQOOOQ0.OQOQ0QOOQ0,OCQQQOOOQ0.OQCQ0QOOQ0));
end;
end;
function OCCOCOOOQ0.OO0CQOOOQ0:TBytes;
var
OQCQ0OOOQ0:OQCOCQOOQ0;
OQ0CQOOOQ0,OC0OCOOOQ0:OC0QOQOOQ0;
OC0CQOOOQ0:integer;
begin
OQCQ0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OQCQ0OOOQ0.OCQCOOOOQ0(OCQCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OQCQ0OOOQ0['Type'].O0CQ0QOOQ0:=O00QQOOOQ0.OCOO0OOOQ0;
OQ0CQOOOQ0:=OQCQ0OOOQ0['Values'];
OQ0CQOOOQ0.OQ0C0QOOQ0:=OO0QQOOOQ0.OOO000QOQ0;
for OC0CQOOOQ0:=0 to OO0QQOOOQ0.OOO000QOQ0-1 do begin
OC0OCOOOQ0:=OQ0CQOOOQ0.OO0C0QOOQ0[OC0CQOOOQ0]['Value'];
OC0OCOOOQ0.OQCQ0QOOQ0:=OO0QQOOOQ0[OC0CQOOOQ0].OOOOCOOOQ0;
OC0OCOOOQ0.OQOQ0QOOQ0:=OO0QQOOOQ0[OC0CQOOOQ0].OQOOCOOOQ0;
end;
Result:=OQCQ0OOOQ0.OOQCOOOOQ0;
finally
OQCQ0OOOQ0.Free;
end;
end;
function OCCOCOOOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCCOCOOOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OCCOCOOOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
var
OCCQQOOOQ0:OCCOCOOOQ0;
O00CQOOOQ0:integer;
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OCCOCOOOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
OCCQQOOOQ0:=OCCOCOOOQ0(OCQQ0OOOQ0);
O00QQOOOQ0.OCOQ00QOQ0(OCCQQOOOQ0.O00QQOOOQ0);
OO0QQOOOQ0.OCQC00QOQ0;
for O00CQOOOQ0:=0 to OCCQQOOOQ0.OO0QQOOOQ0.OOO000QOQ0-1 do
OO0QQOOOQ0.OOQC00QOQ0(OCCQQOOOQ0.OO0QQOOOQ0[O00CQOOOQ0].OQOQ00QOQ0);
end;
procedure OCCOCOOOQ0.O0OCQOOOQ0(OOOCQOOOQ0:OC00COOOQ0);
begin
if OOOCQOOOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OO0QQOOOQ0.OOQC00QOQ0(OOOCQOOOQ0);
end;
procedure OCCOCOOOQ0.O0OCQOOOQ0(const OQOCQOOOQ0:TBytes;OCOCQOOOQ0:O0OOQCOOQ0);
begin
O0OCQOOOQ0(OC00COOOQ0.Create(O00QQOOOQ0.OCOO0OOOQ0,OQOCQOOOQ0,OCOCQOOOQ0));
end;
procedure OCCOCOOOQ0.O0OCQOOOQ0(const OQOCQOOOQ0:string;OCOCQOOOQ0:O0OOQCOOQ0);
begin
O0OCQOOOQ0(OC00COOOQ0.Create(O00QQOOOQ0.OCOO0OOOQ0,OQOCQOOOQ0,OCOCQOOOQ0));
end;
procedure OCCOCOOOQ0.O0QCQOOOQ0;
begin
OO0QQOOOQ0.OCQC00QOQ0;
end;
procedure OCCOCOOOQ0.OOQCQOOOQ0(OQQCQOOOQ0:integer);
begin
OO0QQOOOQ0.O0CC00QOQ0(OQQCQOOOQ0);
end;
function OCCOCOOOQ0.OC0QQOOOQ0:integer;
begin
Result:=OO0QQOOOQ0.OOO000QOQ0;
end;
function OCCOCOOOQ0.O0OQQOOOQ0(OOOQQOOOQ0:integer):OC00COOOQ0;
begin
Result:=OO0QQOOOQ0[OOOQQOOOQ0];
end;
procedure OCCOCOOOQ0.OQOQQOOOQ0(OCOQQOOOQ0:integer;O0QQQOOOQ0:OC00COOOQ0);
begin
if O0QQQOOOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OO0QQOOOQ0[OCOQQOOOQ0]:=O0QQQOOOQ0;
end;
procedure OCCOCOOOQ0.OQ0QQOOOQ0(O0OQCOOOQ0:OQO00OOOQ0);
begin
O00QQOOOQ0.OCOQ00QOQ0(O0OQCOOOQ0);
end;
function OQCCQOOOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OCCOCOOOQ0;
end;
function OQCCQOOOQ0.OO00QOOOQ0:O0QCQCOOQ0;
begin
Result:=O00CQCOOQ0;
end;
procedure OQCCQOOOQ0.OQ00QOOOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
var
OC00QOOOQ0:OCCOCOOOQ0;
O0CQQOOOQ0:integer;
begin
OCQC00QOQ0;
for O0CQQOOOQ0:=0 to OCQQCOOOQ0.OQ0C0QOOQ0-1 do begin
OC00QOOOQ0:=OCCOCOOOQ0.Create;
OOQC00QOQ0(OC00QOOOQ0);
OC00QOOOQ0.OOQQQOOOQ0(OCQQCOOOQ0.OO0C0QOOQ0[O0CQQOOOQ0]);
end;
end;
function OQCCQOOOQ0.O0O0QOOOQ0:TBytes;
var
OQCQ0OOOQ0:OQCOCQOOQ0;
OOO0QOOOQ0:OC0QOQOOQ0;
OC0CQOOOQ0:integer;
begin
OQCQ0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OQCQ0OOOQ0.OCQCOOOOQ0(OO00QOOOQ0)then
raise EScError.Create(seWrongDataFormat);
OOO0QOOOQ0:=OQCQ0OOOQ0.O0O0OOOOQ0;
OOO0QOOOQ0.OQ0C0QOOQ0:=OOO000QOQ0;
for OC0CQOOOQ0:=0 to OOO000QOQ0-1 do
OOO0QOOOQ0.OO0C0QOOQ0[OC0CQOOOQ0].OQOQ0QOOQ0:=OQO0QOOOQ0[OC0CQOOOQ0].OO0CQOOOQ0;
Result:=OQCQ0OOOQ0.OOQCOOOOQ0;
finally
OQCQ0OOOQ0.Free;
end;
end;
function OQCCQOOOQ0.OCCCQOOOQ0(OQQOCOOOQ0:integer):OCCOCOOOQ0;
begin
Result:=TObject(OQO000QOQ0[OQQOCOOOQ0])as OCCOCOOOQ0;
end;
procedure OQCCQOOOQ0.O000QOOOQ0(O0COCOOOQ0:integer;OOCOCOOOQ0:OCCOCOOOQ0);
begin
if OOCOCOOOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[O0COCOOOQ0]:=OOCOCOOOQ0;
end;
constructor OCO0QOOOQ0.Create;
begin
inherited;
O0Q0QOOOQ0:=O0QOCOOOQ0.Create;
end;
destructor OCO0QOOOQ0.Destroy;
begin
O0Q0QOOOQ0.Free;
inherited;
end;
function OCO0QOOOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCO0QOOOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OCO0QOOOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
var
OCCQQOOOQ0:OCO0QOOOQ0;
O0COQOOOQ0,OOCOQOOOQ0:OC00COOOQ0;
O00CQOOOQ0:integer;
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OCO0QOOOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
if Self=OCQQ0OOOQ0 then
Exit;
O0Q0QOOOQ0.OCQC00QOQ0;
OCCQQOOOQ0:=OCO0QOOOQ0(OCQQ0OOOQ0);
for O00CQOOOQ0:=0 to OCCQQOOOQ0.O0Q0QOOOQ0.OOO000QOQ0-1 do begin
OOCOQOOOQ0:=OCCQQOOOQ0.O0Q0QOOOQ0[O00CQOOOQ0];
O0COQOOOQ0:=OC00COOOQ0.Create(OOCOQOOOQ0.O0OOCOOOQ0.OCOO0OOOQ0,OOCOQOOOQ0.OQOOCOOOQ0,OOCOQOOOQ0.OOOOCOOOQ0);
O0Q0QOOOQ0.OOQC00QOQ0(O0COQOOOQ0);
end;
end;
procedure OCO0QOOOQ0.OQOOQOOOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
var
OCOOQOOOQ0:OC0QOQOOQ0;
O0QOQOOOQ0:OC00COOOQ0;
OOQOQOOOQ0:string;
O0CQQOOOQ0:integer;
begin
O0Q0QOOOQ0.OCQC00QOQ0;
if OCQQCOOOQ0=nil then
Exit;
for O0CQQOOOQ0:=0 to OCQQCOOOQ0.OQ0C0QOOQ0-1 do begin
OOQOQOOOQ0:=OCQQCOOOQ0.OO0C0QOOQ0[O0CQQOOOQ0]['AttrType'].O0CQ0QOOQ0;
OCOOQOOOQ0:=OCQQCOOOQ0.OO0C0QOOQ0[O0CQQOOOQ0]['AttrValue'];
O0QOQOOOQ0:=OC00COOOQ0.Create(OOQOQOOOQ0,OCOOQOOOQ0.OQOQ0QOOQ0,OCOOQOOOQ0.OQCQ0QOOQ0);
O0Q0QOOOQ0.OOQC00QOQ0(O0QOQOOOQ0);
end;
end;
function OCO0QOOOQ0.OQCOQOOOQ0:TBytes;
var
OQCQ0OOOQ0:OQCOCQOOQ0;
OC0CQOOOQ0:integer;
begin
OQCQ0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OQCQ0OOOQ0.OCQCOOOOQ0(O0CCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OQCQ0OOOQ0.O0O0OOOOQ0.OQ0C0QOOQ0:=O0Q0QOOOQ0.OOO000QOQ0;
for OC0CQOOOQ0:=0 to O0Q0QOOOQ0.OOO000QOQ0-1 do
OQCQ0OOOQ0.O0O0OOOOQ0.OO0C0QOOQ0[OC0CQOOOQ0]['Attr'].OQOQ0QOOQ0:=O0Q0QOOOQ0[OC0CQOOOQ0].OQ0OCOOOQ0;
Result:=OQCQ0OOOQ0.OOQCOOOOQ0;
finally
OQCQ0OOOQ0.Free;
end;
end;
procedure OCO0QOOOQ0.OCCOQOOOQ0(O00QO0OOQ0:OC00COOOQ0);
begin
O0Q0QOOOQ0.OOQC00QOQ0(O00QO0OOQ0);
end;
procedure OCO0QOOOQ0.OCCOQOOOQ0(const OO0QO0OOQ0,OQ0QO0OOQ0:string;
OC0QO0OOQ0:O0OOQCOOQ0=OCO0QCOOQ0);
var
O00QO0OOQ0:OC00COOOQ0;
begin
O00QO0OOQ0:=OC00COOOQ0.Create(OO0QO0OOQ0,OQ0QO0OOQ0,OC0QO0OOQ0);
O0Q0QOOOQ0.OOQC00QOQ0(O00QO0OOQ0);
end;
procedure OCO0QOOOQ0.O0OQO0OOQ0;
begin
O0Q0QOOOQ0.OCQC00QOQ0;
end;
function OCO0QOOOQ0.OQ0OQOOOQ0(const OC0OQOOOQ0:string):integer;
var
O0OOQOOOQ0:OC00COOOQ0;
OOOOQOOOQ0:integer;
begin
for OOOOQOOOQ0:=0 to O0Q0QOOOQ0.OOO000QOQ0-1 do begin
O0OOQOOOQ0:=O0Q0QOOOQ0.OQCOCOOOQ0[OOOOQOOOQ0];
if SameText(O0OOQOOOQ0.O0OOCOOOQ0.OCOO0OOOQ0,OC0OQOOOQ0)or SameText(O0OOQOOOQ0.O0OOCOOOQ0.O0QO0OOOQ0,OC0OQOOOQ0)then begin
Result:=OOOOQOOOQ0;
Exit;
end;
end;
Result:=-1;
end;
procedure OCO0QOOOQ0.OOOQO0OOQ0(const OQOQO0OOQ0:string);
var
OCOQO0OOQ0:integer;
begin
OCOQO0OOQ0:=OQ0OQOOOQ0(OQOQO0OOQ0);
if OCOQO0OOQ0>=0 then
O0Q0QOOOQ0.O0CC00QOQ0(OCOQO0OOQ0);
end;
procedure OCO0QOOOQ0.O0QQO0OOQ0(const OOQQO0OOQ0:integer);
begin
O0Q0QOOOQ0.O0CC00QOQ0(OOQQO0OOQ0);
end;
function OCO0QOOOQ0.OOQ0QOOOQ0(OQQ0QOOOQ0:integer):OC00COOOQ0;
begin
Result:=O0Q0QOOOQ0.OQCOCOOOQ0[OQQ0QOOOQ0];
end;
function OCO0QOOOQ0.OCQ0QOOOQ0(O0C0QOOOQ0:integer):string;
begin
Result:=O0Q0QOOOQ0.OQCOCOOOQ0[O0C0QOOOQ0].O0OOCOOOQ0.O0QO0OOOQ0;
end;
function OCO0QOOOQ0.OCC0QOOOQ0(O00OQOOOQ0:integer):string;
begin
Result:=O0Q0QOOOQ0.OQCOCOOOQ0[O00OQOOOQ0].OCOOCOOOQ0;
end;
function OCO0QOOOQ0.OOC0QOOOQ0(const OQC0QOOOQ0:string):string;
var
OOOQQOOOQ0:integer;
begin
OOOQQOOOQ0:=OQ0OQOOOQ0(OQC0QOOOQ0);
if OOOQQOOOQ0>=0 then
Result:=O0Q0QOOOQ0.OQCOCOOOQ0[OOOQQOOOQ0].OCOOCOOOQ0
else
raise EScError.CreateFmt(SDistinguishedNameNotFound,[OQC0QOOOQ0],seDistinguishedNameNotFound);
end;
function OCO0QOOOQ0.OO0OQOOOQ0:integer;
begin
Result:=O0Q0QOOOQ0.OOO000QOQ0;
end;
function OCO0QOOOQ0.Equals(OQQQO0OOQ0:OCO0QOOOQ0):boolean;
var
OCQQO0OOQ0,O0CQO0OOQ0:OC00COOOQ0;
OOCQO0OOQ0,OQCQO0OOQ0:integer;
begin
Result:=(OQQQO0OOQ0<>nil)and(O0Q0QOOOQ0.OOO000QOQ0=OQQQO0OOQ0.O0Q0QOOOQ0.OOO000QOQ0);
if not Result then
Exit;
for OOCQO0OOQ0:=0 to O0Q0QOOOQ0.OOO000QOQ0-1 do begin
OCQQO0OOQ0:=O0Q0QOOOQ0.OQCOCOOOQ0[OOCQO0OOQ0];
OQCQO0OOQ0:=0;
while OQCQO0OOQ0<OQQQO0OOQ0.O0Q0QOOOQ0.OOO000QOQ0 do begin
O0CQO0OOQ0:=OQQQO0OOQ0.O0Q0QOOOQ0.OQCOCOOOQ0[OQCQO0OOQ0];
if OCQQO0OOQ0.O0OOCOOOQ0.OCOO0OOOQ0=O0CQO0OOQ0.O0OOCOOOQ0.OCOO0OOOQ0 then begin
if not OCQQO0OOQ0.Equals(O0CQO0OOQ0)then begin
Result:=False;
Exit;
end;
break;
end;
Inc(OQCQO0OOQ0);
end;
if OQCQO0OOQ0=OQQQO0OOQ0.O0Q0QOOOQ0.OOO000QOQ0 then begin
Result:=False;
Exit;
end;
end;
end;
function OCO0QOOOQ0.ToString:string;
var
OCCQO0OOQ0:OC00COOOQ0;
O00CO0OOQ0:integer;
begin
Result:='';
for O00CO0OOQ0:=0 to O0Q0QOOOQ0.OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+', ';
OCCQO0OOQ0:=O0Q0QOOOQ0.OQCOCOOOQ0[O00CO0OOQ0];
Result:=Result+OCCQO0OOQ0.O0OOCOOOQ0.O0QO0OOOQ0+'='+OCCQO0OOQ0.OCOOCOOOQ0;
end;
end;
constructor OQOCO0OOQ0.Create;
begin
inherited;
OCOCO0OOQ0:=TCRObjectList.Create;
end;
destructor OQOCO0OOQ0.Destroy;
begin
OCOCO0OOQ0.Free;
inherited;
end;
function OQOCO0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQOCO0OOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OQOCO0OOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
var
O0C0O0OOQ0:OCO0QOOOQ0;
O00CQOOOQ0:integer;
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OQOCO0OOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
if Self=OCQQ0OOOQ0 then
Exit;
OCOCO0OOQ0.Clear;
for O00CQOOOQ0:=0 to OQOCO0OOQ0(OCQQ0OOOQ0).OCOCO0OOQ0.Count-1 do begin
O0C0O0OOQ0:=OCO0QOOOQ0.Create;
OCOCO0OOQ0.Add(O0C0O0OOQ0);
O0C0O0OOQ0.OCOQ00QOQ0(OCO0QOOOQ0(OQOCO0OOQ0(OCQQ0OOOQ0).OCOCO0OOQ0[O00CQOOOQ0]));
end;
end;
procedure OQOCO0OOQ0.OCO0O0OOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
var
O0Q0O0OOQ0:OC0QOQOOQ0;
OOQ0O0OOQ0:OCO0QOOOQ0;
O0CQQOOOQ0:integer;
begin
OCOCO0OOQ0.Clear;
if OCQQCOOOQ0=nil then
Exit;
for O0CQQOOOQ0:=0 to OCQQCOOOQ0.OQ0C0QOOQ0-1 do begin
O0Q0O0OOQ0:=OCQQCOOOQ0.OO0C0QOOQ0[O0CQQOOOQ0]['Attrs'];
OOQ0O0OOQ0:=OCO0QOOOQ0.Create;
OCOCO0OOQ0.Add(OOQ0O0OOQ0);
OOQ0O0OOQ0.OQOOQOOOQ0(O0Q0O0OOQ0);
end;
end;
procedure OQOCO0OOQ0.OOC0O0OOQ0(const OQC0O0OOQ0:TBytes;OCC0O0OOQ0,O00OO0OOQ0:integer);
var
OO0C0OOOQ0:OQCOCQOOQ0;
begin
OO0C0OOOQ0:=OQCOCQOOQ0.Create;
try
if OO0C0OOOQ0.OCQCOOOOQ0(OQQ0CCOOQ0,OQC0O0OOQ0,OCC0O0OOQ0,O00OO0OOQ0)then
OCO0O0OOQ0(OO0C0OOOQ0.OOO0OOOOQ0['Names']);
finally
OO0C0OOOQ0.Free;
end;
end;
function OQOCO0OOQ0.OO0OO0OOQ0:TBytes;
var
OQCQ0OOOQ0:OQCOCQOOQ0;
OC0CQOOOQ0:integer;
begin
OQCQ0OOOQ0:=OQCOCQOOQ0.Create;
try
if not OQCQ0OOOQ0.OCQCOOOOQ0(OOCCCCOOQ0)then
raise EScError.Create(seWrongDataFormat);
OQCQ0OOOQ0.O0O0OOOOQ0.OQ0C0QOOQ0:=OCOCO0OOQ0.Count;
for OC0CQOOOQ0:=0 to OCOCO0OOQ0.Count-1 do
OQCQ0OOOQ0.O0O0OOOOQ0.OO0C0QOOQ0[OC0CQOOOQ0].O0OQ0QOOQ0:=OCO0QOOOQ0(OCOCO0OOQ0[OC0CQOOOQ0]).OQCOQOOOQ0;
Result:=OQCQ0OOOQ0.OOQCOOOOQ0;
finally
OQCQ0OOOQ0.Free;
end;
end;
procedure OQOCO0OOQ0.OQOOO0OOQ0(OCOOO0OOQ0:OCO0QOOOQ0);
begin
OCOCO0OOQ0.Add(OCOOO0OOQ0);
end;
procedure OQOCO0OOQ0.OQOOO0OOQ0(const OO0QO0OOQ0,OQ0QO0OOQ0:string;
OC0QO0OOQ0:O0OOQCOOQ0=OCO0QCOOQ0);
var
OCOOO0OOQ0:OCO0QOOOQ0;
begin
OCOOO0OOQ0:=OCO0QOOOQ0.Create;
OCOCO0OOQ0.Add(OCOOO0OOQ0);
OCOOO0OOQ0.OCCOQOOOQ0(OO0QO0OOQ0,OQ0QO0OOQ0,OC0QO0OOQ0);
end;
procedure OQOCO0OOQ0.O0QOO0OOQ0;
begin
OCOCO0OOQ0.Clear;
end;
procedure OQOCO0OOQ0.OOQOO0OOQ0(const OOQQO0OOQ0:integer);
begin
OCOCO0OOQ0.Delete(OOQQO0OOQ0);
end;
function OQOCO0OOQ0.O0QCO0OOQ0(OQQ0QOOOQ0:integer):OCO0QOOOQ0;
begin
Result:=OCO0QOOOQ0(OCOCO0OOQ0.Items[OQQ0QOOOQ0]);
end;
function OQOCO0OOQ0.OOQCO0OOQ0:integer;
begin
Result:=OCOCO0OOQ0.Count;
end;
function OQOCO0OOQ0.OQ0OO0OOQ0(const OC0OQOOOQ0:string):integer;
var
OC0OO0OOQ0:OCO0QOOOQ0;
O0OOO0OOQ0,OOOOO0OOQ0:integer;
OOOOQOOOQ0:integer;
begin
O0OOO0OOQ0:=0;
for OOOOQOOOQ0:=0 to OCOCO0OOQ0.Count-1 do begin
OC0OO0OOQ0:=OCO0QOOOQ0(OCOCO0OOQ0.Items[OOOOQOOOQ0]);
OOOOO0OOQ0:=OC0OO0OOQ0.OQ0OQOOOQ0(OC0OQOOOQ0);
if OOOOO0OOQ0>=0 then begin
Result:=O0OOO0OOQ0+OOOOO0OOQ0;
Exit;
end;
O0OOO0OOQ0:=O0OOO0OOQ0+OC0OO0OOQ0.OO0CO0OOQ0;
end;
Result:=-1;
end;
function OQOCO0OOQ0.OQQCO0OOQ0(O0C0QOOOQ0:integer):string;
var
OCQCO0OOQ0:OCO0QOOOQ0;
O0CCO0OOQ0:integer;
begin
for O0CCO0OOQ0:=0 to OCOCO0OOQ0.Count-1 do begin
OCQCO0OOQ0:=OCO0QOOOQ0(OCOCO0OOQ0.Items[O0CCO0OOQ0]);
if OCQCO0OOQ0.OO0CO0OOQ0>O0C0QOOOQ0 then begin
Result:=OCQCO0OOQ0.OC0CO0OOQ0[O0C0QOOOQ0];
Exit;
end;
O0C0QOOOQ0:=O0C0QOOOQ0-OCQCO0OOQ0.OO0CO0OOQ0;
end;
raise Exception.CreateFmt(SListIndexError,[O0C0QOOOQ0]);
end;
function OQOCO0OOQ0.OO00O0OOQ0(O00OQOOOQ0:integer):string;
var
OQ00O0OOQ0:OCO0QOOOQ0;
OC00O0OOQ0:integer;
begin
for OC00O0OOQ0:=0 to OCOCO0OOQ0.Count-1 do begin
OQ00O0OOQ0:=OCO0QOOOQ0(OCOCO0OOQ0.Items[OC00O0OOQ0]);
if OQ00O0OOQ0.OO0CO0OOQ0>O00OQOOOQ0 then begin
Result:=OQ00O0OOQ0.OOOCO0OOQ0[O00OQOOOQ0];
Exit;
end;
O00OQOOOQ0:=O00OQOOOQ0-OQ00O0OOQ0.OO0CO0OOQ0;
end;
raise Exception.CreateFmt(SListIndexError,[O00OQOOOQ0]);
end;
function OQOCO0OOQ0.OOCCO0OOQ0(const OQC0QOOOQ0:string):string;
var
OQCCO0OOQ0:OCO0QOOOQ0;
OCCCO0OOQ0:integer;
O000O0OOQ0:integer;
begin
for O000O0OOQ0:=0 to OCOCO0OOQ0.Count-1 do begin
OQCCO0OOQ0:=OCO0QOOOQ0(OCOCO0OOQ0.Items[O000O0OOQ0]);
OCCCO0OOQ0:=OQCCO0OOQ0.OQ0OQOOOQ0(OQC0QOOOQ0);
if OCCCO0OOQ0>=0 then begin
Result:=OQCCO0OOQ0.OOOCO0OOQ0[OCCCO0OOQ0];
Exit;
end;
end;
raise EScError.CreateFmt(SDistinguishedNameNotFound,[OQC0QOOOQ0],seDistinguishedNameNotFound);
end;
function OQOCO0OOQ0.O0O0O0OOQ0:integer;
var
OOO0O0OOQ0:OCO0QOOOQ0;
OQO0O0OOQ0:integer;
begin
Result:=0;
for OQO0O0OOQ0:=0 to OCOCO0OOQ0.Count-1 do begin
OOO0O0OOQ0:=OCO0QOOOQ0(OCOCO0OOQ0.Items[OQO0O0OOQ0]);
Result:=Result+OOO0O0OOQ0.OO0CO0OOQ0;
end;
end;
function OQOCO0OOQ0.Equals(OQQQO0OOQ0:OQOCO0OOQ0):boolean;
var
OQQOO0OOQ0,OCQOO0OOQ0:OCO0QOOOQ0;
OOCQO0OOQ0:integer;
begin
Result:=(OQQQO0OOQ0<>nil)and(OCOCO0OOQ0.Count=OQQQO0OOQ0.OCOCO0OOQ0.Count);
if not Result then
Exit;
for OOCQO0OOQ0:=0 to OCOCO0OOQ0.Count-1 do begin
OQQOO0OOQ0:=OCO0QOOOQ0(OCOCO0OOQ0.Items[OOCQO0OOQ0]);
OCQOO0OOQ0:=OCO0QOOOQ0(OQQQO0OOQ0.OCOCO0OOQ0.Items[OOCQO0OOQ0]);
if not OQQOO0OOQ0.Equals(OCQOO0OOQ0)then begin
Result:=False;
Exit;
end;
end;
end;
function OQOCO0OOQ0.ToString:string;
var
O0COO0OOQ0:OCO0QOOOQ0;
O00CO0OOQ0:integer;
begin
Result:='';
for O00CO0OOQ0:=0 to OCOCO0OOQ0.Count-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+'; ';
O0COO0OOQ0:=OCO0QOOOQ0(OCOCO0OOQ0.Items[O00CO0OOQ0]);
Result:=Result+O0COO0OOQ0.ToString;
end;
end;
function OC0Q00OOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OQOCO0OOQ0;
end;
function OC0Q00OOQ0.O0OQ00OOQ0(O0C0QOOOQ0:integer):OQOCO0OOQ0;
begin
Result:=TObject(OQO000QOQ0[O0C0QOOOQ0])as OQOCO0OOQ0;
end;
procedure OC0Q00OOQ0.OOOQ00OOQ0(OQOQ00OOQ0:integer;OCOQ00OOQ0:OQOCO0OOQ0);
begin
if OCOQ00OOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[OQOQ00OOQ0]:=OCOQ00OOQ0;
end;
constructor OOQQ00OOQ0.Create;
begin
inherited;
O0CQ00OOQ0:=OQOCO0OOQ0.Create;
end;
destructor OOQQ00OOQ0.Destroy;
begin
O0CQ00OOQ0.Free;
inherited;
end;
function OOQQ00OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OOQQ00OOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OOQQ00OOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OOQQ00OOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
OQQQ00OOQ0:=OOQQ00OOQ0(OCQQ0OOOQ0).OQQQ00OOQ0;
OCQQ00OOQ0:=OOQQ00OOQ0(OCQQ0OOOQ0).OCQQ00OOQ0;
O0CQ00OOQ0.OCOQ00QOQ0(OOQQ00OOQ0(OCQQ0OOOQ0).O0CQ00OOQ0);
end;
function OOQQ00OOQ0.Equals(OQ0C00OOQ0:OOQQ00OOQ0):boolean;
begin
if OQ0C00OOQ0=nil then
Result:=False
else
if(OQQQ00OOQ0='DirectoryName')and(OQ0C00OOQ0.OQQQ00OOQ0='DirectoryName')then
Result:=O0CQ00OOQ0.Equals(OQ0C00OOQ0.O0CQ00OOQ0)
else
Result:=SameText(OCQQ00OOQ0,OQ0C00OOQ0.OCQQ00OOQ0);
end;
function OOQQ00OOQ0.Equals(OC0C00OOQ0:OQOCO0OOQ0):boolean;
begin
Result:=(OC0C00OOQ0<>nil)and O0CQ00OOQ0.Equals(OC0C00OOQ0);
end;
procedure OOQQ00OOQ0.OCCQ00OOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
begin
OQQQ00OOQ0:=OCQQCOOOQ0.O00C0QOOQ0;
if SameText(OQQQ00OOQ0,'OtherName')then
OCQQ00OOQ0:=OCQQCOOOQ0['TypeId'].OCOQ0QOOQ0+'='+OCQQCOOOQ0['Value'].OCOQ0QOOQ0
else
if SameText(OQQQ00OOQ0,'DirectoryName')then begin
O0CQ00OOQ0.OCO0O0OOQ0(OCQQCOOOQ0);
OCQQ00OOQ0:=O0CQ00OOQ0.ToString;
end
else
if SameText(OQQQ00OOQ0,'EdiPartyName')then
OCQQ00OOQ0:=OCQQCOOOQ0['NameAssigner'].OCOQ0QOOQ0+'='+OCQQCOOOQ0['PartyName'].OCOQ0QOOQ0
else
OCQQ00OOQ0:=OCQQCOOOQ0.OCOQ0QOOQ0;
end;
function OOQQ00OOQ0.ToString:string;
begin
if O0OC00OOQ0='' then
Result:=OCQQ00OOQ0
else
if O0OC00OOQ0='DirectoryName' then
Result:='Directory Address: '+OCQQ00OOQ0
else
if(O0OC00OOQ0='OtherName')or(O0OC00OOQ0='EdiPartyName')or(O0OC00OOQ0='Relative Name')then
Result:=OQQQ00OOQ0+': '+OCQQ00OOQ0
else
Result:=OQQQ00OOQ0+'='+OCQQ00OOQ0;
end;
procedure OOQQ00OOQ0.OOCQ00OOQ0(OQCQ00OOQ0:OQOCO0OOQ0);
begin
O0CQ00OOQ0.OCOQ00QOQ0(OQCQ00OOQ0);
end;
function OCOC00OOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OOQQ00OOQ0;
end;
function OCOC00OOQ0.O0QC00OOQ0(OOQC00OOQ0:integer):OOQQ00OOQ0;
begin
Result:=TObject(OQO000QOQ0[OOQC00OOQ0])as OOQQ00OOQ0;
end;
procedure OCOC00OOQ0.OQQC00OOQ0(OCQC00OOQ0:integer;O0CC00OOQ0:OOQQ00OOQ0);
begin
if O0CC00OOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[OCQC00OOQ0]:=O0CC00OOQ0;
end;
function OCOC00OOQ0.O0O000OOQ0(const OOO000OOQ0:string):OOQQ00OOQ0;
var
OQO000OOQ0:integer;
begin
for OQO000OOQ0:=0 to OOO000QOQ0-1 do begin
if SameText(OOQQ00OOQ0(OQO000QOQ0[OQO000OOQ0]).O0OC00OOQ0,OOO000OOQ0)then begin
Result:=OOQQ00OOQ0(OQO000QOQ0[OQO000OOQ0]);
Exit;
end;
end;
Result:=nil;
end;
function OCOC00OOQ0.Equals(OQQQO0OOQ0:OCOC00OOQ0):boolean;
var
OCCC00OOQ0,O00000OOQ0:OOQQ00OOQ0;
OOCQO0OOQ0:integer;
begin
Result:=(OQQQO0OOQ0<>nil)and(OOO000QOQ0=OQQQO0OOQ0.OOO000QOQ0);
if not Result then
Exit;
for OOCQO0OOQ0:=0 to OOO000QOQ0-1 do begin
OCCC00OOQ0:=OOQQ00OOQ0(OCO000OOQ0[OOCQO0OOQ0]);
O00000OOQ0:=OOQQ00OOQ0(OQQQO0OOQ0.OCO000OOQ0[OOCQO0OOQ0]);
if not OCCC00OOQ0.Equals(O00000OOQ0)then begin
Result:=False;
Exit;
end;
end;
end;
function OCOC00OOQ0.OO0000OOQ0(OQ0000OOQ0:OQOCO0OOQ0):boolean;
var
OC0000OOQ0:integer;
begin
Result:=False;
if OQ0000OOQ0=nil then
Exit;
for OC0000OOQ0:=0 to OOO000QOQ0-1 do begin
if OCO000OOQ0[OC0000OOQ0].Equals(OQ0000OOQ0)then begin
Result:=True;
Exit;
end;
end;
end;
procedure OCOC00OOQ0.OOCC00OOQ0(OCQQCOOOQ0:OC0QOQOOQ0);
var
OQCC00OOQ0:OOQQ00OOQ0;
O0CQQOOOQ0:integer;
begin
if OCQQCOOOQ0.OOOQ0QOOQ0 then
Exit;
for O0CQQOOOQ0:=0 to OCQQCOOOQ0.OQ0C0QOOQ0-1 do begin
OQCC00OOQ0:=OOQQ00OOQ0.Create;
OOQC00QOQ0(OQCC00OOQ0);
OQCC00OOQ0.OCCQ00OOQ0(OCQQCOOOQ0.OO0C0QOOQ0[O0CQQOOOQ0]['Name']);
end;
end;
function OCOC00OOQ0.ToString:string;
var
O00CO0OOQ0:integer;
begin
Result:='';
for O00CO0OOQ0:=0 to OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+'; ';
Result:=Result+OCO000OOQ0[O00CO0OOQ0].ToString;
end;
end;
constructor OOQ000OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited Create;
OQQ000OOQ0:=OQO00OOOQ0.Create(O00O00OOQ0);
OCQ000OOQ0:=OO0O00OOQ0;
O0C000OOQ0:=OQ0O00OOQ0;
OOC000OOQ0(OQ0O00OOQ0);
end;
destructor OOQ000OOQ0.Destroy;
begin
OQQ000OOQ0.Free;
inherited;
end;
function OOQ000OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OOQ000OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
procedure OOQ000OOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OOQ000OOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
OQQ000OOQ0.OCOQ00QOQ0(OOQ000OOQ0(OCQQ0OOOQ0).OQQ000OOQ0);
OCQ000OOQ0:=OOQ000OOQ0(OCQQ0OOOQ0).OCQ000OOQ0;
O0C000OOQ0:=OOQ000OOQ0(OCQQ0OOOQ0).O0C000OOQ0;
OOC000OOQ0(O0C000OOQ0);
end;
procedure OOQ000OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
begin
end;
function OOQ000OOQ0.ToString:string;
begin
Result:=O00QOCOOQ0(O0C000OOQ0);
end;
constructor OCOO00OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OCOO00OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCOO00OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
procedure OCOO00OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
const
OCQO00OOQ0=$80;
var
O0CO00OOQ0:OQCOCQOOQ0;
OOCO00OOQ0:OC0QOQOOQ0;
OQCO00OOQ0:TBytes;
begin
SetLength(OQCO00OOQ0,0);
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if OQQ000OOQ0.OCOO0OOOQ0=OID_CE_BASIC_CONSTRAINTS then begin
if not O0CO00OOQ0.OCQCOOOOQ0(OOQOCCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOCO00OOQ0:=O0CO00OOQ0['Info'];
OQCO00OOQ0:=OOCO00OOQ0['SubjectType'].OQOQ0QOOQ0;
if Length(OQCO00OOQ0)>0 then
O0QO00OOQ0:=(OQCO00OOQ0[0]and OCQO00OOQ0)<>0
else
O0QO00OOQ0:=False;
OOQO00OOQ0:=not OOCO00OOQ0['Len'].OOOQ0QOOQ0;
OQQO00OOQ0:=OOCO00OOQ0['Len'].OQQQ0QOOQ0;
end
else begin
if not O0CO00OOQ0.OCQCOOOOQ0(OQQOCCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOCO00OOQ0:=O0CO00OOQ0['Info'];
O0QO00OOQ0:=OOCO00OOQ0['CA'].OOQQ0QOOQ0;
OOQO00OOQ0:=not OOCO00OOQ0['Len'].OOOQ0QOOQ0;
OQQO00OOQ0:=OOCO00OOQ0['Len'].OQQQ0QOOQ0;
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OCOO00OOQ0.ToString:string;
begin
Result:='Subject Type=';
if O0QO00OOQ0 then
Result:=Result+'CA'
else
Result:=Result+'End Entity';
Result:=Result+'; Path Length Constraint=';
if not OOQO00OOQ0 then
Result:=Result+'None'
else
Result:=Result+IntToStr(OQQO00OOQ0);
end;
constructor O0CQC0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function O0CQC0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=O0CQC0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
procedure O0CQC0OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
const
OQCQC0OOQ0=$80;
OCCQC0OOQ0=$40;
O00CC0OOQ0=$20;
OO0CC0OOQ0=$10;
OQ0CC0OOQ0=$08;
OC0CC0OOQ0=$04;
O0OCC0OOQ0=$02;
OOOCC0OOQ0=$01;
OQOCC0OOQ0=$80;
var
O0CO00OOQ0:OQCOCQOOQ0;
OCOCC0OOQ0:TBytes;
begin
OOCQC0OOQ0:=[];
SetLength(OCOCC0OOQ0,0);
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OCQOCCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OCOCC0OOQ0:=O0CO00OOQ0['Data'].OQOQ0QOOQ0;
if Length(OCOCC0OOQ0)>0 then begin
if(OCOCC0OOQ0[0]and OQCQC0OOQ0)>0 then
Include(OOCQC0OOQ0,OQ0QC0OOQ0);
if(OCOCC0OOQ0[0]and OCCQC0OOQ0)>0 then
Include(OOCQC0OOQ0,OC0QC0OOQ0);
if(OCOCC0OOQ0[0]and O00CC0OOQ0)>0 then
Include(OOCQC0OOQ0,O0OQC0OOQ0);
if(OCOCC0OOQ0[0]and OO0CC0OOQ0)>0 then
Include(OOCQC0OOQ0,OOOQC0OOQ0);
if(OCOCC0OOQ0[0]and OQ0CC0OOQ0)>0 then
Include(OOCQC0OOQ0,OQOQC0OOQ0);
if(OCOCC0OOQ0[0]and OC0CC0OOQ0)>0 then
Include(OOCQC0OOQ0,OCOQC0OOQ0);
if(OCOCC0OOQ0[0]and O0OCC0OOQ0)>0 then
Include(OOCQC0OOQ0,O0QQC0OOQ0);
if(OCOCC0OOQ0[0]and OOOCC0OOQ0)>0 then
Include(OOCQC0OOQ0,OOQQC0OOQ0);
if(Length(OCOCC0OOQ0)>1)and((OCOCC0OOQ0[1]and OQOCC0OOQ0)>0)then
OOCQC0OOQ0:=OOCQC0OOQ0+[OQQQC0OOQ0];
end;
finally
O0CO00OOQ0.Free;
end;
end;
function O0CQC0OOQ0.ToString:string;
const
O0QCC0OOQ0:array[OCQQC0OOQ0]of string=(
'Digital Signature',
'Non-Repudiation',
'Key Encipherment',
'Data Encipherment',
'Key Agreement',
'Certificate Signing',
'CRL Signing',
'Encipher Only',
'Decipher Only'
);
var
OOQCC0OOQ0:OCQQC0OOQ0;
begin
Result:='';
for OOQCC0OOQ0:=Low(OCQQC0OOQ0)to High(OCQQC0OOQ0)do begin
if OOQCC0OOQ0 in OOCQC0OOQ0 then begin
if Result<>'' then
Result:=Result+', ';
Result:=Result+O0QCC0OOQ0[OOQCC0OOQ0];
end;
end;
end;
constructor OCQCC0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OCQCC0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCQCC0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OCQCC0OOQ0.Destroy;
begin
O0CCC0OOQ0.Free;
inherited;
end;
procedure OCQCC0OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
OOCCC0OOQ0:OC0QOQOOQ0;
OOQOQOOOQ0:OQO00OOOQ0;
O0CQQOOOQ0:integer;
begin
if O0CCC0OOQ0=nil then
O0CCC0OOQ0:=OOQO0OOOQ0.Create
else
O0CCC0OOQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(O0COCCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOCCC0OOQ0:=O0CO00OOQ0['Usage'];
for O0CQQOOOQ0:=0 to OOCCC0OOQ0.OQ0C0QOOQ0-1 do begin
OOQOQOOOQ0:=OQO00OOOQ0.Create(OOCCC0OOQ0.OO0C0QOOQ0[O0CQQOOOQ0]['ID'].O0CQ0QOOQ0);
O0CCC0OOQ0.OOQC00QOQ0(OOQOQOOOQ0);
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OCQCC0OOQ0.ToString:string;
var
O00CO0OOQ0:integer;
begin
Result:='';
if O0CCC0OOQ0=nil then
Exit;
for O00CO0OOQ0:=0 to O0CCC0OOQ0.OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+', ';
Result:=Result+O0CCC0OOQ0.OCCO0OOOQ0[O00CO0OOQ0].O0QO0OOOQ0+' ('+O0CCC0OOQ0.OCCO0OOOQ0[O00CO0OOQ0].OCOO0OOOQ0+')';
end;
end;
constructor OCCCC0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OCCCC0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCCCC0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
procedure OCCCC0OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
begin
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OOCOCCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
O000C0OOQ0:=O00QOCOOQ0(O0CO00OOQ0['Data'].OQOQ0QOOQ0);
finally
O0CO00OOQ0.Free;
end;
end;
function OCCCC0OOQ0.ToString:string;
begin
Result:=O000C0OOQ0;
end;
constructor OQ00C0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OQ00C0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQ00C0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OQ00C0OOQ0.Destroy;
begin
OOO0C0OOQ0.Free;
inherited;
end;
procedure OQ00C0OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
OQO0C0OOQ0:O0QCQCOOQ0;
O0CO00OOQ0:OQCOCQOOQ0;
OCO0C0OOQ0:OC0QOQOOQ0;
OQCC00OOQ0:OOQQ00OOQ0;
O0CQQOOOQ0:integer;
begin
if OOO0C0OOQ0=nil then
OOO0C0OOQ0:=OCOC00OOQ0.Create
else
OOO0C0OOQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if OQQ000OOQ0.OCO00OOOQ0=OID_CE_AUTHORITY_KEY_IDENTIFIER then
OQO0C0OOQ0:=OQCQQCOOQ0
else
OQO0C0OOQ0:=OCCQQCOOQ0;
if not O0CO00OOQ0.OCQCOOOOQ0(OQO0C0OOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OC00C0OOQ0:=O00QOCOOQ0(O0CO00OOQ0['KeyID'].OQOQ0QOOQ0);
O0O0C0OOQ0:=O0CO00OOQ0['SerialNumber'].OCOQ0QOOQ0;
OCO0C0OOQ0:=O0CO00OOQ0['Issuer'];
if OQQ000OOQ0.OCO00OOOQ0=OID_CE_AUTHORITY_KEY_IDENTIFIER then
OOO0C0OOQ0.OOCC00OOQ0(OCO0C0OOQ0)
else begin
for O0CQQOOOQ0:=0 to OCO0C0OOQ0.OQ0C0QOOQ0-1 do begin
OQCC00OOQ0:=OOQQ00OOQ0.Create;
OOO0C0OOQ0.OOQC00QOQ0(OQCC00OOQ0);
OQCC00OOQ0.OQOC00OOQ0.OCO0O0OOQ0(OCO0C0OOQ0.OO0C0QOOQ0[O0CQQOOOQ0]['DirectoryName']);
OQCC00OOQ0.OOOC00OOQ0:=OQCC00OOQ0.OQOC00OOQ0.ToString;
end;
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OQ00C0OOQ0.ToString:string;
begin
if OOO0C0OOQ0=nil then begin
Result:='';
Exit;
end;
Result:='KeyID='+OC00C0OOQ0;
if OOO0C0OOQ0.OOO000QOQ0>0 then begin
Result:=Result+'; '#13#10'Certificate Issuer: '#13#10;
Result:=Result+OOO0C0OOQ0.ToString;
end;
if O0O0C0OOQ0<>'' then
Result:=Result+'; '#13#10'Certificate SerialNumber='+O0O0C0OOQ0;
end;
function O00OC0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=O00OC0OOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure O00OC0OOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
var
O00CQOOOQ0:integer;
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,O00OC0OOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
OO0OC0OOQ0:=O00OC0OOQ0(OCQQ0OOOQ0).OO0OC0OOQ0;
SetLength(OQ0OC0OOQ0,Length(O00OC0OOQ0(OCQQ0OOOQ0).OQ0OC0OOQ0));
for O00CQOOOQ0:=0 to Length(O00OC0OOQ0(OCQQ0OOOQ0).OQ0OC0OOQ0)-1 do
OQ0OC0OOQ0[O00CQOOOQ0]:=O00OC0OOQ0(OCQQ0OOOQ0).OQ0OC0OOQ0[O00CQOOOQ0];
end;
function OC0OC0OOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=O00OC0OOQ0;
end;
function OC0OC0OOQ0.O0OOC0OOQ0(OOOOC0OOQ0:integer):O00OC0OOQ0;
begin
Result:=TObject(OQO000QOQ0[OOOOC0OOQ0])as O00OC0OOQ0;
end;
procedure OC0OC0OOQ0.OQOOC0OOQ0(OCOOC0OOQ0:integer;O0QOC0OOQ0:O00OC0OOQ0);
begin
if O0QOC0OOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[OCOOC0OOQ0]:=O0QOC0OOQ0;
end;
constructor OQQOC0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OQQOC0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQQOC0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OQQOC0OOQ0.Destroy;
begin
OCQOC0OOQ0.Free;
inherited;
end;
procedure OQQOC0OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
O0COC0OOQ0,OOCOC0OOQ0:OC0QOQOOQ0;
OQCOC0OOQ0,OCCOC0OOQ0,O00QQ0OOQ0:OC0QOQOOQ0;
OO0QQ0OOQ0:O00OC0OOQ0;
O0CQQOOOQ0,OQ0QQ0OOQ0:integer;
begin
if OCQOC0OOQ0=nil then
OCQOC0OOQ0:=OC0OC0OOQ0.Create
else
OCQOC0OOQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OQCOCCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
O0COC0OOQ0:=O0CO00OOQ0['Policies'];
for O0CQQOOOQ0:=0 to O0COC0OOQ0.OQ0C0QOOQ0-1 do begin
OO0QQ0OOQ0:=O00OC0OOQ0.Create;
OCQOC0OOQ0.OOQC00QOQ0(OO0QQ0OOQ0);
OOCOC0OOQ0:=O0COC0OOQ0.OO0C0QOOQ0[O0CQQOOOQ0];
OO0QQ0OOQ0.OO0OC0OOQ0:=OOCOC0OOQ0['Identifier'].O0CQ0QOOQ0;
OQCOC0OOQ0:=OOCOC0OOQ0['Qualifiers'];
SetLength(OO0QQ0OOQ0.OQ0OC0OOQ0,OQCOC0OOQ0.OQ0C0QOOQ0);
for OQ0QQ0OOQ0:=0 to OQCOC0OOQ0.OQ0C0QOOQ0-1 do begin
OCCOC0OOQ0:=OQCOC0OOQ0.OO0C0QOOQ0[OQ0QQ0OOQ0];
OO0QQ0OOQ0.OQ0OC0OOQ0[OQ0QQ0OOQ0].O0C0C0OOQ0:=OCCOC0OOQ0['QualifierId'].O0CQ0QOOQ0;
O00QQ0OOQ0:=OCCOC0OOQ0['Qualifier'];
if OO0QQ0OOQ0.OQ0OC0OOQ0[OQ0QQ0OOQ0].O0C0C0OOQ0=OID_POLICY_QUALIFIER_CPS then begin
if O00QQ0OOQ0.O00C0QOOQ0<>'CPSuri' then
raise EScError.Create(seWrongExtensionData);
OO0QQ0OOQ0.OQ0OC0OOQ0[OQ0QQ0OOQ0].OOC0C0OOQ0:=O00QQ0OOQ0.OCOQ0QOOQ0;
end
else
if OO0QQ0OOQ0.OQ0OC0OOQ0[OQ0QQ0OOQ0].O0C0C0OOQ0=OID_POLICY_QUALIFIER_UNOTICE then begin
OO0QQ0OOQ0.OQ0OC0OOQ0[OQ0QQ0OOQ0].OQC0C0OOQ0:=O00QQ0OOQ0['Organization'].OCOQ0QOOQ0;
OO0QQ0OOQ0.OQ0OC0OOQ0[OQ0QQ0OOQ0].OCC0C0OOQ0:=O00QQ0OOQ0['ExplicitText'].OCOQ0QOOQ0;
end
else
raise EScError.Create(seWrongExtensionData);
end;
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OQQOC0OOQ0.ToString:string;
var
OC0QQ0OOQ0:O00OC0OOQ0;
O0OQQ0OOQ0:string;
O00CO0OOQ0,OOOQQ0OOQ0:integer;
begin
Result:='';
if OCQOC0OOQ0=nil then
Exit;
for O00CO0OOQ0:=0 to OCQOC0OOQ0.OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+'; '#13#10;
OC0QQ0OOQ0:=OCQOC0OOQ0[O00CO0OOQ0];
Result:=Result+'['+IntToStr(O00CO0OOQ0+1)+']Certificate Policy: '#13#10;
O0OQQ0OOQ0:=OC0OOOOOQ0(OC0QQ0OOQ0.OO0OC0OOQ0);
if O0OQQ0OOQ0='' then
O0OQQ0OOQ0:=OC0QQ0OOQ0.OO0OC0OOQ0;
Result:=Result+'Policy Identifier='+O0OQQ0OOQ0;
for OOOQQ0OOQ0:=0 to Length(OC0QQ0OOQ0.OQ0OC0OOQ0)-1 do begin
Result:=Result+'; '#13#10;
Result:=Result+'['+IntToStr(O00CO0OOQ0+1)+','+IntToStr(OOOQQ0OOQ0+1)+']Policy Qualifier Info: '#13#10;
O0OQQ0OOQ0:=OC0OOOOOQ0(OC0QQ0OOQ0.OQ0OC0OOQ0[OOOQQ0OOQ0].O0C0C0OOQ0);
if O0OQQ0OOQ0='' then
O0OQQ0OOQ0:=OC0QQ0OOQ0.OQ0OC0OOQ0[OOOQQ0OOQ0].O0C0C0OOQ0;
Result:=Result+'Policy Qualifier Id='+O0OQQ0OOQ0+'; ';
Result:=Result+'Qualifier: ';
if OC0QQ0OOQ0.OQ0OC0OOQ0[OOOQQ0OOQ0].O0C0C0OOQ0=OID_POLICY_QUALIFIER_CPS then
Result:=Result+OC0QQ0OOQ0.OQ0OC0OOQ0[OOOQQ0OOQ0].OOC0C0OOQ0
else
if OC0QQ0OOQ0.OQ0OC0OOQ0[OOOQQ0OOQ0].O0C0C0OOQ0=OID_POLICY_QUALIFIER_UNOTICE then begin
if OC0QQ0OOQ0.OQ0OC0OOQ0[OOOQQ0OOQ0].OQC0C0OOQ0<>'' then
Result:=Result+'Organization='+OC0QQ0OOQ0.OQ0OC0OOQ0[OOOQQ0OOQ0].OQC0C0OOQ0+'; ';
Result:=Result+'Explicit Text='+OC0QQ0OOQ0.OQ0OC0OOQ0[OOOQQ0OOQ0].OCC0C0OOQ0;
end;
end;
end;
end;
constructor OCOQQ0OOQ0.Create;
begin
inherited Create;
O0QQQ0OOQ0:=OQO00OOOQ0.Create;
OOQQQ0OOQ0:=OQO00OOOQ0.Create;
end;
constructor OCOQQ0OOQ0.Create(const OCQQQ0OOQ0,O0CQQ0OOQ0:string);
begin
inherited Create;
O0QQQ0OOQ0:=OQO00OOOQ0.Create(OCQQQ0OOQ0);
OOQQQ0OOQ0:=OQO00OOOQ0.Create(O0CQQ0OOQ0);
end;
destructor OCOQQ0OOQ0.Destroy;
begin
O0QQQ0OOQ0.Free;
OOQQQ0OOQ0.Free;
inherited;
end;
function OCOQQ0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCOQQ0OOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OCOQQ0OOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OCOQQ0OOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
O0QQQ0OOQ0.OCOQ00QOQ0(OCOQQ0OOQ0(OCQQ0OOOQ0).O0QQQ0OOQ0);
OOQQQ0OOQ0.OCOQ00QOQ0(OCOQQ0OOQ0(OCQQ0OOOQ0).OOQQQ0OOQ0);
end;
function O00CQ0OOQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OCOQQ0OOQ0;
end;
function O00CQ0OOQ0.OO0CQ0OOQ0(OQ0CQ0OOQ0:integer):OCOQQ0OOQ0;
begin
Result:=TObject(OQO000QOQ0[OQ0CQ0OOQ0])as OCOQQ0OOQ0;
end;
procedure O00CQ0OOQ0.OC0CQ0OOQ0(O0OCQ0OOQ0:integer;OOOCQ0OOQ0:OCOQQ0OOQ0);
begin
if OOOCQ0OOQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[O0OCQ0OOQ0]:=OOOCQ0OOQ0;
end;
constructor OCOCQ0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OCOCQ0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCOCQ0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OCOCQ0OOQ0.Destroy;
begin
O0QCQ0OOQ0.Free;
inherited;
end;
procedure OCOCQ0OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
OOQCQ0OOQ0,OOCOC0OOQ0:OC0QOQOOQ0;
OQQCQ0OOQ0:OCOQQ0OOQ0;
O0CQQOOOQ0:integer;
begin
if O0QCQ0OOQ0=nil then
O0QCQ0OOQ0:=O00CQ0OOQ0.Create
else
O0QCQ0OOQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OCCOCCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOQCQ0OOQ0:=O0CO00OOQ0['PolicyMappings'];
for O0CQQOOOQ0:=0 to OOQCQ0OOQ0.OQ0C0QOOQ0-1 do begin
OOCOC0OOQ0:=OOQCQ0OOQ0.OO0C0QOOQ0[O0CQQOOOQ0];
OQQCQ0OOQ0:=OCOQQ0OOQ0.Create(OOCOC0OOQ0['IssuerDomainPolicy'].O0CQ0QOOQ0,OOCOC0OOQ0['SubjectDomainPolicy'].O0CQ0QOOQ0);
O0QCQ0OOQ0.OOQC00QOQ0(OQQCQ0OOQ0);
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OCOCQ0OOQ0.ToString:string;
var
OCQCQ0OOQ0:OCOQQ0OOQ0;
O00CO0OOQ0:integer;
begin
Result:='';
if O0QCQ0OOQ0=nil then
Exit;
for O00CO0OOQ0:=0 to O0QCQ0OOQ0.OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+'; '#13#10;
OCQCQ0OOQ0:=O0QCQ0OOQ0[O00CO0OOQ0];
Result:=Result+'Issuer Domain Policy='+OCQCQ0OOQ0.OQCQQ0OOQ0.O0QO0OOOQ0;
Result:=Result+', Subject Domain Policy='+OCQCQ0OOQ0.OCCQQ0OOQ0.O0QO0OOOQ0;
end;
end;
constructor OOCCQ0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OOCCQ0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OOCCQ0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OOCCQ0OOQ0.Destroy;
begin
OQCCQ0OOQ0.Free;
inherited;
end;
procedure OOCCQ0OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
OCCCQ0OOQ0:OC0QOQOOQ0;
begin
if OQCCQ0OOQ0=nil then
OQCCQ0OOQ0:=OCOC00OOQ0.Create
else
OQCCQ0OOQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(O00QQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OCCCQ0OOQ0:=O0CO00OOQ0['GeneralNames'];
OQCCQ0OOQ0.OOCC00OOQ0(OCCCQ0OOQ0);
finally
O0CO00OOQ0.Free;
end;
end;
function OOCCQ0OOQ0.ToString:string;
begin
if OQCCQ0OOQ0=nil then
Result:=''
else
Result:=OQCCQ0OOQ0.ToString;
end;
constructor OO00Q0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OO00Q0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OO00Q0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
constructor OQ00Q0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OQ00Q0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQ00Q0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
constructor OC00Q0OOQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OC00Q0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OC00Q0OOQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OC00Q0OOQ0.Destroy;
begin
O0O0Q0OOQ0.Free;
inherited;
end;
procedure OC00Q0OOQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
OOO0Q0OOQ0:OC0QOQOOQ0;
OC00QOOOQ0:OCCOCOOOQ0;
O0CQQOOOQ0:integer;
begin
if O0O0Q0OOQ0=nil then
O0O0Q0OOQ0:=OQCCQOOOQ0.Create
else
O0O0Q0OOQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OO0QQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOO0Q0OOQ0:=O0CO00OOQ0['Attributes'];
for O0CQQOOOQ0:=0 to OOO0Q0OOQ0.OQ0C0QOOQ0-1 do begin
OC00QOOOQ0:=OCCOCOOOQ0.Create;
O0O0Q0OOQ0.OOQC00QOQ0(OC00QOOOQ0);
OC00QOOOQ0.OOQQQOOOQ0(OOO0Q0OOQ0.OO0C0QOOQ0[O0CQQOOOQ0]);
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OC00Q0OOQ0.ToString:string;
var
OQO0Q0OOQ0:OCCOCOOOQ0;
O00CO0OOQ0,OOOQQ0OOQ0:integer;
begin
Result:='';
if O0O0Q0OOQ0=nil then
Exit;
for O00CO0OOQ0:=0 to O0O0Q0OOQ0.OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+'; '#13#10;
OQO0Q0OOQ0:=O0O0Q0OOQ0[O00CO0OOQ0];
Result:=Result+OQO0Q0OOQ0.OCQCQOOOQ0.O0QO0OOOQ0+': ';
for OOOQQ0OOQ0:=0 to OQO0Q0OOQ0.O0CCQOOOQ0-1 do begin
if OOOQQ0OOQ0>0 then
Result:=Result+', ';
Result:=Result+OQO0Q0OOQ0.OOCCQOOOQ0[OOOQQ0OOQ0].OCOOCOOOQ0;
end;
end;
end;
constructor OC0OQ0OOQ0.Create;
begin
inherited Create;
O0OOQ0OOQ0:=OCOC00OOQ0.Create;
OQOOQ0OOQ0:=OCOC00OOQ0.Create;
end;
destructor OC0OQ0OOQ0.Destroy;
begin
O0OOQ0OOQ0.Free;
OQOOQ0OOQ0.Free;
inherited;
end;
function OC0OQ0OOQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OC0OQ0OOQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OC0OQ0OOQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OC0OQ0OOQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
O0OOQ0OOQ0.OQOC00QOQ0(OC0OQ0OOQ0(OCQQ0OOOQ0).O0OOQ0OOQ0);
OQOOQ0OOQ0.OQOC00QOQ0(OC0OQ0OOQ0(OCQQ0OOOQ0).OQOOQ0OOQ0);
OOOOQ0OOQ0:=OC0OQ0OOQ0(OCQQ0OOOQ0).OOOOQ0OOQ0;
end;
class procedure OC0OQ0OOQ0.OCOOQ0OOQ0(O0QOQ0OOQ0:OC0QOQOOQ0;out OOQOQ0OOQ0:TScCRLReasons);
const
OQQOQ0OOQ0=$40;
OCQOQ0OOQ0=$20;
O0COQ0OOQ0=$10;
OOCOQ0OOQ0=$08;
OQCOQ0OOQ0=$04;
OCCOQ0OOQ0=$02;
O00QOC0OQ0=$01;
OO0QOC0OQ0=$80;
var
OQ0QOC0OQ0:TBytes;
begin
OOQOQ0OOQ0:=[];
if not O0QOQ0OOQ0.OOOQ0QOOQ0 then begin
SetLength(OQ0QOC0OQ0,0);
OQ0QOC0OQ0:=O0QOQ0OOQ0.OQOQ0QOOQ0;
if Length(OQ0QOC0OQ0)>0 then begin
if(OQ0QOC0OQ0[0]and OQQOQ0OOQ0)>0 then
Include(OOQOQ0OOQ0,O0Q0Q0OOQ0);
if(OQ0QOC0OQ0[0]and OCQOQ0OOQ0)>0 then
Include(OOQOQ0OOQ0,OOQ0Q0OOQ0);
if(OQ0QOC0OQ0[0]and O0COQ0OOQ0)>0 then
Include(OOQOQ0OOQ0,OQQ0Q0OOQ0);
if(OQ0QOC0OQ0[0]and OOCOQ0OOQ0)>0 then
Include(OOQOQ0OOQ0,OCQ0Q0OOQ0);
if(OQ0QOC0OQ0[0]and OQCOQ0OOQ0)>0 then
Include(OOQOQ0OOQ0,O0C0Q0OOQ0);
if(OQ0QOC0OQ0[0]and OCCOQ0OOQ0)>0 then
Include(OOQOQ0OOQ0,OOC0Q0OOQ0);
if(OQ0QOC0OQ0[0]and O00QOC0OQ0)>0 then
Include(OOQOQ0OOQ0,OQC0Q0OOQ0);
if(Length(OQ0QOC0OQ0)>1)and((OQ0QOC0OQ0[1]and OO0QOC0OQ0)>0)then
Include(OOQOQ0OOQ0,OCC0Q0OOQ0);
end;
end;
end;
class function OC0OQ0OOQ0.OC0QOC0OQ0(const O0OQOC0OQ0:TScCRLReasons):string;
var
OOOQOC0OQ0:OQ0OQ0OOQ0;
begin
Result:='';
for OOOQOC0OQ0:=Low(OQ0OQ0OOQ0)to High(OQ0OQ0OOQ0)do begin
if OOOQOC0OQ0 in O0OQOC0OQ0 then begin
if Result<>'' then
Result:=Result+', ';
Result:=Result+O0CQCC0OQ0[OOOQOC0OQ0];
end;
end;
end;
function OCQQOC0OQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OC0OQ0OOQ0;
end;
function OCQQOC0OQ0.O0CQOC0OQ0(OOCQOC0OQ0:integer):OC0OQ0OOQ0;
begin
Result:=TObject(OQO000QOQ0[OOCQOC0OQ0])as OC0OQ0OOQ0;
end;
procedure OCQQOC0OQ0.OQCQOC0OQ0(OCCQOC0OQ0:integer;O00COC0OQ0:OC0OQ0OOQ0);
begin
if O00COC0OQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[OCCQOC0OQ0]:=O00COC0OQ0;
end;
constructor OQ0COC0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OQ0COC0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQ0COC0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OQ0COC0OQ0.Destroy;
begin
OC0COC0OQ0.Free;
inherited;
end;
procedure OQ0COC0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
O0OCOC0OQ0:OC0QOQOOQ0;
OOOCOC0OQ0,OQOCOC0OQ0,OCOCOC0OQ0:OC0QOQOOQ0;
O0QCOC0OQ0:OC0OQ0OOQ0;
OQCC00OOQ0:OOQQ00OOQ0;
OOQ0O0OOQ0:OCO0QOOOQ0;
O0CQQOOOQ0:integer;
begin
if OC0COC0OQ0=nil then
OC0COC0OQ0:=OCQQOC0OQ0.Create
else
OC0COC0OQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OQ0QQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
O0OCOC0OQ0:=O0CO00OOQ0['CRLDistributionPoints'];
for O0CQQOOOQ0:=0 to O0OCOC0OQ0.OQ0C0QOOQ0-1 do begin
O0QCOC0OQ0:=OC0OQ0OOQ0.Create;
OC0COC0OQ0.OOQC00QOQ0(O0QCOC0OQ0);
OOOCOC0OQ0:=O0OCOC0OQ0.OO0C0QOOQ0[O0CQQOOOQ0]['DistributionPoint'];
if not OOOCOC0OQ0.OOOQ0QOOQ0 then begin
if OOOCOC0OQ0.O00C0QOOQ0='FullName' then
O0QCOC0OQ0.O0QQOC0OQ0.OOCC00OOQ0(OOOCOC0OQ0)
else
if OOOCOC0OQ0.O00C0QOOQ0='NameRelativeToCRLIssuer' then begin
OQCC00OOQ0:=OOQQ00OOQ0.Create;
O0QCOC0OQ0.O0QQOC0OQ0.OOQC00QOQ0(OQCC00OOQ0);
OQCC00OOQ0.OQQQ00OOQ0:='Relative Name';
OOQ0O0OOQ0:=OCO0QOOOQ0.Create;
try
OOQ0O0OOQ0.OQOOQOOOQ0(OOOCOC0OQ0);
OQCC00OOQ0.OCQQ00OOQ0:=OOQ0O0OOQ0.ToString;
finally
OOQ0O0OOQ0.Free;
end;
end;
end;
OQOCOC0OQ0:=O0OCOC0OQ0.OO0C0QOOQ0[O0CQQOOOQ0]['Reasons'];
OC0OQ0OOQ0.OCOOQ0OOQ0(OQOCOC0OQ0,O0QCOC0OQ0.OOOOQ0OOQ0);
OCOCOC0OQ0:=O0OCOC0OQ0.OO0C0QOOQ0[O0CQQOOOQ0]['CRLIssuer'];
O0QCOC0OQ0.OQQQOC0OQ0.OOCC00OOQ0(OCOCOC0OQ0);
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OQ0COC0OQ0.ToString:string;
var
OOQCOC0OQ0:OC0OQ0OOQ0;
OQQCOC0OQ0:string;
O00CO0OOQ0:integer;
begin
Result:='';
if OC0COC0OQ0=nil then
Exit;
for O00CO0OOQ0:=0 to OC0COC0OQ0.OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+'; '#13#10;
OOQCOC0OQ0:=OC0COC0OQ0[O00CO0OOQ0];
Result:=Result+'['+IntToStr(O00CO0OOQ0+1)+']CRL Distribution Point '#13#10;
OQQCOC0OQ0:=OOQCOC0OQ0.O0QQOC0OQ0.ToString;
if OQQCOC0OQ0<>'' then
Result:=Result+'Distribution Point Name: '#13#10+OQQCOC0OQ0;
OQQCOC0OQ0:=OC0OQ0OOQ0.OC0QOC0OQ0(OOQCOC0OQ0.OOOOQ0OOQ0);
if OQQCOC0OQ0<>'' then
Result:=Result+'; '#13#10'CRL Reasons='+OQQCOC0OQ0;
OQQCOC0OQ0:=OOQCOC0OQ0.OQQQOC0OQ0.ToString;
if OQQCOC0OQ0<>'' then
Result:=Result+'; '#13#10'CRL Issuer: '#13#10+OQQCOC0OQ0;
end;
end;
constructor O0CCOC0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function O0CCOC0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=O0CCOC0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
constructor OOCCOC0OQ0.Create;
begin
inherited Create;
OQCCOC0OQ0:=OQO00OOOQ0.Create;
OCCCOC0OQ0:=OOQQ00OOQ0.Create;
end;
destructor OOCCOC0OQ0.Destroy;
begin
OQCCOC0OQ0.Free;
OCCCOC0OQ0.Free;
inherited;
end;
function OOCCOC0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OOCCOC0OQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OOCCOC0OQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OOCCOC0OQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
OQCCOC0OQ0.OCOQ00QOQ0(OOCCOC0OQ0(OCQQ0OOOQ0).OQCCOC0OQ0);
OCCCOC0OQ0.OCOQ00QOQ0(OOCCOC0OQ0(OCQQ0OOOQ0).OCCCOC0OQ0);
end;
function O0O0OC0OQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OOCCOC0OQ0;
end;
function O0O0OC0OQ0.OOO0OC0OQ0(OQO0OC0OQ0:integer):OOCCOC0OQ0;
begin
Result:=TObject(OQO000QOQ0[OQO0OC0OQ0])as OOCCOC0OQ0;
end;
procedure O0O0OC0OQ0.OCO0OC0OQ0(O0Q0OC0OQ0:integer;OOQ0OC0OQ0:OOCCOC0OQ0);
begin
if OOQ0OC0OQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[O0Q0OC0OQ0]:=OOQ0OC0OQ0;
end;
constructor OCQ0OC0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OCQ0OC0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCQ0OC0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OCQ0OC0OQ0.Destroy;
begin
O0C0OC0OQ0.Free;
inherited;
end;
procedure OCQ0OC0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
OOC0OC0OQ0,OQC0OC0OQ0:OC0QOQOOQ0;
OCC0OC0OQ0:OOCCOC0OQ0;
O0CQQOOOQ0:integer;
begin
if O0C0OC0OQ0=nil then
O0C0OC0OQ0:=O0O0OC0OQ0.Create
else
O0C0OC0OQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OC0QQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOC0OC0OQ0:=O0CO00OOQ0['AuthorityInfoAccess'];
for O0CQQOOOQ0:=0 to OOC0OC0OQ0.OQ0C0QOOQ0-1 do begin
OCC0OC0OQ0:=OOCCOC0OQ0.Create;
O0C0OC0OQ0.OOQC00QOQ0(OCC0OC0OQ0);
OQC0OC0OQ0:=OOC0OC0OQ0.OO0C0QOOQ0[O0CQQOOOQ0]['AccessDescription'];
OCC0OC0OQ0.OQ00OC0OQ0.OCOO0OOOQ0:=OQC0OC0OQ0['AccessMethod'].O0CQ0QOOQ0;
OCC0OC0OQ0.OC00OC0OQ0.OCCQ00OOQ0(OQC0OC0OQ0['AccessLocation']);
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OCQ0OC0OQ0.ToString:string;
var
O00OOC0OQ0:OOCCOC0OQ0;
O00CO0OOQ0:integer;
begin
Result:='';
if O0C0OC0OQ0=nil then
Exit;
for O00CO0OOQ0:=0 to O0C0OC0OQ0.OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+'; '#13#10;
O00OOC0OQ0:=O0C0OC0OQ0[O00CO0OOQ0];
Result:=Result+'['+IntToStr(O00CO0OOQ0+1)+']Authority Info Access '#13#10;
Result:=Result+'Access Method='+O00OOC0OQ0.OQ00OC0OQ0.O0QO0OOOQ0;
Result:=Result+' ('+O00OOC0OQ0.OQ00OC0OQ0.OCOO0OOOQ0+'); '#13#10;
Result:=Result+'Alternative Name: '+O00OOC0OQ0.OC00OC0OQ0.ToString;
end;
end;
constructor OQ0OOC0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OQ0OOC0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQ0OOC0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OQ0OOC0OQ0.Destroy;
begin
OC0OOC0OQ0.Free;
inherited;
end;
procedure OQ0OOC0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
O0OOOC0OQ0,OQC0OC0OQ0:OC0QOQOOQ0;
OOOOOC0OQ0:OOCCOC0OQ0;
O0CQQOOOQ0:integer;
begin
if OC0OOC0OQ0=nil then
OC0OOC0OQ0:=O0O0OC0OQ0.Create
else
OC0OOC0OQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(O0OQQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
O0OOOC0OQ0:=O0CO00OOQ0['SubjectInfoAccess'];
for O0CQQOOOQ0:=0 to O0OOOC0OQ0.OQ0C0QOOQ0-1 do begin
OOOOOC0OQ0:=OOCCOC0OQ0.Create;
OC0OOC0OQ0.OOQC00QOQ0(OOOOOC0OQ0);
OQC0OC0OQ0:=O0OOOC0OQ0.OO0C0QOOQ0[O0CQQOOOQ0]['AccessDescription'];
OOOOOC0OQ0.OQ00OC0OQ0.OCOO0OOOQ0:=OQC0OC0OQ0['AccessMethod'].O0CQ0QOOQ0;
OOOOOC0OQ0.OC00OC0OQ0.OCCQ00OOQ0(OQC0OC0OQ0['AccessLocation']);
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OQ0OOC0OQ0.ToString:string;
var
OQOOOC0OQ0:OOCCOC0OQ0;
O00CO0OOQ0:integer;
begin
Result:='';
if OC0OOC0OQ0=nil then
Exit;
for O00CO0OOQ0:=0 to OC0OOC0OQ0.OOO000QOQ0-1 do begin
if O00CO0OOQ0>0 then
Result:=Result+'; '#13#10;
OQOOOC0OQ0:=OC0OOC0OQ0[O00CO0OOQ0];
Result:=Result+'['+IntToStr(O00CO0OOQ0+1)+']Subject Info Access '#13#10;
Result:=Result+'Access Method='+OQOOOC0OQ0.OQ00OC0OQ0.O0QO0OOOQ0;
Result:=Result+' ('+OQOOOC0OQ0.OQ00OC0OQ0.OCOO0OOOQ0+'); '#13#10;
Result:=Result+'Alternative Name: '+OQOOOC0OQ0.OC00OC0OQ0.ToString;
end;
end;
constructor O0QOOC0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function O0QOOC0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=O0QOOC0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
procedure O0QOOC0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
begin
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OOOQQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOQOOC0OQ0:=O0CO00OOQ0['CRLNumber'].OCOQ0QOOQ0;
finally
O0CO00OOQ0.Free;
end;
end;
function O0QOOC0OQ0.ToString:string;
begin
Result:='CRL Number: '+OOQOOC0OQ0;
end;
constructor OCQOOC0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OCQOOC0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCQOOC0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
procedure OCQOOC0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
begin
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OOOQQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
O0COOC0OQ0:=O0CO00OOQ0['CRLNumber'].OCOQ0QOOQ0;
finally
O0CO00OOQ0.Free;
end;
end;
function OCQOOC0OQ0.ToString:string;
begin
Result:='Base CRL Number: '+O0COOC0OQ0;
end;
constructor OQCOOC0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OQCOOC0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQCOOC0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OQCOOC0OQ0.Destroy;
begin
OCCOOC0OQ0.Free;
inherited;
end;
procedure OQCOOC0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
OOOCOC0OQ0,OOOQ0C0OQ0:OC0QOQOOQ0;
OQCC00OOQ0:OOQQ00OOQ0;
OOQ0O0OOQ0:OCO0QOOOQ0;
begin
if OCCOOC0OQ0=nil then
OCCOOC0OQ0:=OCOC00OOQ0.Create
else
OCCOOC0OQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OQOQQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOOCOC0OQ0:=O0CO00OOQ0['DistributionPoint'];
if not OOOCOC0OQ0.OOOQ0QOOQ0 then begin
if OOOCOC0OQ0.O00C0QOOQ0='FullName' then
OCCOOC0OQ0.OOCC00OOQ0(OOOCOC0OQ0)
else
if OOOCOC0OQ0.O00C0QOOQ0='NameRelativeToCRLIssuer' then begin
OQCC00OOQ0:=OOQQ00OOQ0.Create;
OCCOOC0OQ0.OOQC00QOQ0(OQCC00OOQ0);
OQCC00OOQ0.OQQQ00OOQ0:='Relative Name';
OOQ0O0OOQ0:=OCO0QOOOQ0.Create;
try
OOQ0O0OOQ0.OQOOQOOOQ0(OOOCOC0OQ0);
OQCC00OOQ0.OCQQ00OOQ0:=OOQ0O0OOQ0.ToString;
finally
OOQ0O0OOQ0.Free;
end;
end;
end;
OOOQ0C0OQ0:=O0CO00OOQ0['OnlySomeReasons'];
OC0OQ0OOQ0.OCOOQ0OOQ0(OOOQ0C0OQ0,O00Q0C0OQ0);
OO0Q0C0OQ0:=O0CO00OOQ0['IndirectCRL'].OOQQ0QOOQ0;
OQ0Q0C0OQ0:=O0CO00OOQ0['OnlyContainsUserCerts'].OOQQ0QOOQ0;
OC0Q0C0OQ0:=O0CO00OOQ0['OnlyContainsCACerts'].OOQQ0QOOQ0;
O0OQ0C0OQ0:=O0CO00OOQ0['OnlyContainsAttributeCerts'].OOQQ0QOOQ0;
finally
O0CO00OOQ0.Free;
end;
end;
function OQCOOC0OQ0.ToString:string;
var
OQQCOC0OQ0:string;
begin
Result:='';
if OCCOOC0OQ0=nil then
Exit;
Result:='Issuing CRL Distribution Point '#13#10;
OQQCOC0OQ0:=OCCOOC0OQ0.ToString;
if OQQCOC0OQ0<>'' then
Result:=Result+'Distribution Point Name: '#13#10+OQQCOC0OQ0+#13#10;
Result:=Result+'Only Contains User Certs='+BoolToStr(OQ0Q0C0OQ0,True)+'; '#13#10;
Result:=Result+'Only Contains CA Certs='+BoolToStr(OC0Q0C0OQ0,True)+'; '#13#10;
OQQCOC0OQ0:=OC0OQ0OOQ0.OC0QOC0OQ0(O00Q0C0OQ0);
if OQQCOC0OQ0<>'' then
Result:=Result+'Only Some Reasons='+OQQCOC0OQ0+'; '#13#10;
Result:=Result+'Indirect CRL='+BoolToStr(OO0Q0C0OQ0,True)+'; '#13#10;
Result:=Result+'Only Contains Attribute Certs='+BoolToStr(O0OQ0C0OQ0,True);
end;
constructor O0CQ0C0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function O0CQ0C0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=O0CQ0C0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
procedure O0CQ0C0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
begin
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OCOQQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
case O0CO00OOQ0['CRLReason'].OQQQ0QOOQ0 of
1:
OOCQ0C0OQ0:=O0Q0Q0OOQ0;
2:
OOCQ0C0OQ0:=OOQ0Q0OOQ0;
3:
OOCQ0C0OQ0:=OQQ0Q0OOQ0;
4:
OOCQ0C0OQ0:=OCQ0Q0OOQ0;
5:
OOCQ0C0OQ0:=O0C0Q0OOQ0;
6:
OOCQ0C0OQ0:=OOC0Q0OOQ0;
8:
OOCQ0C0OQ0:=O00OQ0OOQ0;
9:
OOCQ0C0OQ0:=OQC0Q0OOQ0;
10:
OOCQ0C0OQ0:=OCC0Q0OOQ0;
else
OOCQ0C0OQ0:=OO0OQ0OOQ0;
end;
finally
O0CO00OOQ0.Free;
end;
end;
function O0CQ0C0OQ0.ToString:string;
begin
Result:='CRL Reason: '+O0CQCC0OQ0[OOCQ0C0OQ0];
end;
constructor OCCQ0C0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OCCQ0C0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OCCQ0C0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
procedure OCCQ0C0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
begin
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(O0QQQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
O00C0C0OQ0:=O0CO00OOQ0['InvalidityDate'].OOCQ0QOOQ0;
finally
O0CO00OOQ0.Free;
end;
end;
function OCCQ0C0OQ0.ToString:string;
begin
Result:='Invalidity Date: '+DateTimeToStr(O00C0C0OQ0);
end;
constructor OQ0C0C0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OQ0C0C0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQ0C0C0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OQ0C0C0OQ0.Destroy;
begin
OC0C0C0OQ0.Free;
inherited;
end;
procedure OQ0C0C0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
begin
if OC0C0C0OQ0=nil then
OC0C0C0OQ0:=OCOC00OOQ0.Create
else
OC0C0C0OQ0.OCQC00QOQ0;
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OOQQQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OC0C0C0OQ0.OOCC00OOQ0(O0CO00OOQ0['CertificateIssuer']);
finally
O0CO00OOQ0.Free;
end;
end;
function OQ0C0C0OQ0.ToString:string;
begin
Result:='';
if OC0C0C0OQ0=nil then
Exit;
Result:='Certificate Issuer: '#13#10+OC0C0C0OQ0.ToString;
end;
function OOOC0C0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OOOC0C0OQ0.Create;
Result.OCOQ00QOQ0(Self);
end;
procedure OOOC0C0OQ0.OCOQ00QOQ0(OCQQ0OOOQ0:OQ0Q00QOQ0);
begin
if(OCQQ0OOOQ0=nil)or not IsClass(OCQQ0OOOQ0,OOOC0C0OQ0)then
OC0Q00QOQ0(OCQQ0OOOQ0);
OQOC0C0OQ0:=OOOC0C0OQ0(OCQQ0OOOQ0).OQOC0C0OQ0;
SetLength(OCOC0C0OQ0,Length(OOOC0C0OQ0(OCQQ0OOOQ0).OCOC0C0OQ0));
if Length(OCOC0C0OQ0)>0 then
Move(OOOC0C0OQ0(OCQQ0OOOQ0).OCOC0C0OQ0[0],OCOC0C0OQ0[0],Length(OCOC0C0OQ0));
O0QC0C0OQ0:=OOOC0C0OQ0(OCQQ0OOOQ0).O0QC0C0OQ0;
OOQC0C0OQ0:=OOOC0C0OQ0(OCQQ0OOOQ0).OOQC0C0OQ0;
OQQC0C0OQ0:=OOOC0C0OQ0(OCQQ0OOOQ0).OQQC0C0OQ0;
SetLength(OCQC0C0OQ0,Length(OOOC0C0OQ0(OCQQ0OOOQ0).OCQC0C0OQ0));
if Length(OCQC0C0OQ0)>0 then
Move(OOOC0C0OQ0(OCQQ0OOOQ0).OCQC0C0OQ0[0],OCQC0C0OQ0[0],Length(OCQC0C0OQ0));
end;
procedure OOOC0C0OQ0.O0CC0C0OQ0(const OOCC0C0OQ0:TBytes;OQCC0C0OQ0:integer);
var
OCCC0C0OQ0:integer;
O0000C0OQ0:Int64;
begin
OQOC0C0OQ0:=OOCC0C0OQ0[OQCC0C0OQ0];
if OQOC0C0OQ0=0 then begin
if(OQCC0C0OQ0+47)>Length(OOCC0C0OQ0)then
raise EScError.Create(seWrongExtensionData);
SetLength(OCOC0C0OQ0,32);
Move(OOCC0C0OQ0[OQCC0C0OQ0+1],OCOC0C0OQ0[0],32);
Inc(OQCC0C0OQ0,33);
O0000C0OQ0:=GetInt64BE(OOCC0C0OQ0,OQCC0C0OQ0);
O0QC0C0OQ0:=(O0000C0OQ0/MSecsPerDay)+UnixDateDelta+OQC0OCOOQ0;
Inc(OQCC0C0OQ0,8);
OCCC0C0OQ0:=OOCC0C0OQ0[OQCC0C0OQ0]shl 8+OOCC0C0OQ0[OQCC0C0OQ0+1];
Inc(OQCC0C0OQ0,2);
if(OQCC0C0OQ0+OCCC0C0OQ0)>Length(OOCC0C0OQ0)then
raise EScError.Create(seWrongExtensionData);
Inc(OQCC0C0OQ0,OCCC0C0OQ0);
case OOCC0C0OQ0[OQCC0C0OQ0]of
0:
OOQC0C0OQ0:=O0C0COQOQ0;
1:
OOQC0C0OQ0:=OQ0OCOQOQ0;
2:
OOQC0C0OQ0:=OOC0COQOQ0;
3:
OOQC0C0OQ0:=O00OCOQOQ0;
4:
OOQC0C0OQ0:=OQC0COQOQ0;
5:
OOQC0C0OQ0:=OO0OCOQOQ0;
6:
OOQC0C0OQ0:=OCC0COQOQ0;
else
OOQC0C0OQ0:=O0C0COQOQ0;
end;
case OOCC0C0OQ0[OQCC0C0OQ0+1]of
1:
OQQC0C0OQ0:=OCOQQOQOQ0;
2:
OQQC0C0OQ0:=OQOQQOQOQ0;
3:
OQQC0C0OQ0:=O0QQQOQOQ0;
else
OQQC0C0OQ0:=OCOQQOQOQ0;
end;
Inc(OQCC0C0OQ0,2);
OCCC0C0OQ0:=OOCC0C0OQ0[OQCC0C0OQ0]shl 8+OOCC0C0OQ0[OQCC0C0OQ0+1];
Inc(OQCC0C0OQ0,2);
if(OCCC0C0OQ0=0)or((OQCC0C0OQ0+OCCC0C0OQ0)>Length(OOCC0C0OQ0))then
raise EScError.Create(seWrongExtensionData);
SetLength(OCQC0C0OQ0,OCCC0C0OQ0);
Move(OOCC0C0OQ0[OQCC0C0OQ0],OCQC0C0OQ0[0],OCCC0C0OQ0);
end;
end;
function OOOC0C0OQ0.ToString:string;
begin
Result:='v'+IntToStr(OQOC0C0OQ0+1)+#13#10;
Result:=Result+O00QOCOOQ0(OCOC0C0OQ0)+#13#10;
Result:=Result+DateTimeToStr(O0QC0C0OQ0)+#13#10;
Result:=Result+OOCQCC0OQ0.OOCOCC0OQ0(OOQC0C0OQ0)+#13#10;
Result:=Result+OOCQCC0OQ0.OOQOCC0OQ0(OQQC0C0OQ0)+#13#10;
Result:=Result+O00QOCOOQ0(OCQC0C0OQ0)+#13#10;
end;
function OCO00C0OQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OOOC0C0OQ0;
end;
function OCO00C0OQ0.O0Q00C0OQ0(OOQ00C0OQ0:integer):OOOC0C0OQ0;
begin
Result:=TObject(OQO000QOQ0[OOQ00C0OQ0])as OOOC0C0OQ0;
end;
procedure OCO00C0OQ0.OQQ00C0OQ0(OCQ00C0OQ0:integer;O0C00C0OQ0:OOOC0C0OQ0);
begin
if O0C00C0OQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQO000QOQ0[OCQ00C0OQ0]:=O0C00C0OQ0;
end;
function OCO00C0OQ0.ToString:string;
var
O00CO0OOQ0:integer;
begin
Result:='';
for O00CO0OOQ0:=0 to OOO000QOQ0-1 do
Result:=Result+O0Q00C0OQ0(O00CO0OOQ0).ToString+#13#10;
end;
constructor OQC00C0OQ0.Create(const O00O00OOQ0:string;OO0O00OOQ0:boolean;const OQ0O00OOQ0:TBytes);
begin
inherited;
end;
function OQC00C0OQ0.OQOQ00QOQ0:OQ0Q00QOQ0;
begin
Result:=OQC00C0OQ0.Create(OQQ000OOQ0.OCOO0OOOQ0,OCQ000OOQ0,O0C000OOQ0);
end;
destructor OQC00C0OQ0.Destroy;
begin
OCC00C0OQ0.Free;
inherited;
end;
procedure OQC00C0OQ0.OOC000OOQ0(const OQC000OOQ0:TBytes);
var
O0CO00OOQ0:OQCOCQOOQ0;
OOCC0C0OQ0:TBytes;
OQCC0C0OQ0,OCCC0C0OQ0:integer;
O00O0C0OQ0:OOOC0C0OQ0;
begin
if OCC00C0OQ0=nil then
OCC00C0OQ0:=OCO00C0OQ0.Create
else
OCC00C0OQ0.OCQC00QOQ0;
SetLength(OOCC0C0OQ0,0);
O0CO00OOQ0:=OQCOCQOOQ0.Create;
try
if not O0CO00OOQ0.OCQCOOOOQ0(OQQQQCOOQ0,OQC000OOQ0)then
raise EScError.Create(seWrongExtensionData);
OOCC0C0OQ0:=O0CO00OOQ0.O0O0OOOOQ0.OQOQ0QOOQ0;
if Length(OOCC0C0OQ0)<2 then
raise EScError.Create(seWrongExtensionData);
OCCC0C0OQ0:=OOCC0C0OQ0[0]shl 8+OOCC0C0OQ0[1];
if(OCCC0C0OQ0<2)or((OCCC0C0OQ0+2)<>Length(OOCC0C0OQ0))then
raise EScError.Create(seWrongExtensionData);
OQCC0C0OQ0:=2;
while(OQCC0C0OQ0+1)<Length(OOCC0C0OQ0)do begin
OCCC0C0OQ0:=OOCC0C0OQ0[OQCC0C0OQ0]shl 8+OOCC0C0OQ0[OQCC0C0OQ0+1];
Inc(OQCC0C0OQ0,2);
if(OCCC0C0OQ0<1)or((OQCC0C0OQ0+OCCC0C0OQ0)>Length(OOCC0C0OQ0))then
raise EScError.Create(seWrongExtensionData);
O00O0C0OQ0:=OOOC0C0OQ0.Create;
OCC00C0OQ0.OOQC00QOQ0(O00O0C0OQ0);
O00O0C0OQ0.O0CC0C0OQ0(OOCC0C0OQ0,OQCC0C0OQ0);
Inc(OQCC0C0OQ0,OCCC0C0OQ0);
end;
finally
O0CO00OOQ0.Free;
end;
end;
function OQC00C0OQ0.ToString:string;
begin
if OCC00C0OQ0=nil then
Result:=''
else
Result:=OCC00C0OQ0.ToString;
end;
function OQ0O0C0OQ0.OC0C00QOQ0:O0QQ00QOQ0;
begin
Result:=OOQ000OOQ0;
end;
procedure OQ0O0C0OQ0.O0QO0C0OQ0(OCQQCOOOQ0:OC0QOQOOQ0);
var
OOQO0C0OQ0:OC0QOQOOQ0;
OQQO0C0OQ0:OOQ000OOQ0;
OOQOQOOOQ0:string;
OQC000OOQ0:TBytes;
OCQO0C0OQ0:boolean;
O0CQQOOOQ0:integer;
begin
OCQC00QOQ0;
SetLength(OQC000OOQ0,0);
for O0CQQOOOQ0:=0 to OCQQCOOOQ0.OQ0C0QOOQ0-1 do begin
OOQO0C0OQ0:=OCQQCOOOQ0.OO0C0QOOQ0[O0CQQOOOQ0];
OOQOQOOOQ0:=OOQO0C0OQ0['ExtnID'].O0CQ0QOOQ0;
OCQO0C0OQ0:=OOQO0C0OQ0['Critical'].OOQQ0QOOQ0;
OQC000OOQ0:=OOQO0C0OQ0['ExtnValue'].OQOQ0QOOQ0;
OQQO0C0OQ0:=nil;
try
if(OOQOQOOOQ0=OID_CE_BASIC_CONSTRAINTS)or(OOQOQOOOQ0=OID_CE_BASIC_CONSTRAINTS2)then
OQQO0C0OQ0:=OCOO00OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_KEY_USAGE then
OQQO0C0OQ0:=O0CQC0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_EXTENDED_KEY_USAGE then
OQQO0C0OQ0:=OCQCC0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_SUBJECT_KEY_IDENTIFIER then
OQQO0C0OQ0:=OCCCC0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if(OOQOQOOOQ0=OID_CE_AUTHORITY_KEY_IDENTIFIER)or(OOQOQOOOQ0=OID_CE_AUTHORITY_KEY_IDENTIFIER2)then
OQQO0C0OQ0:=OQ00C0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_SUBJECT_ALTERNATIVE_NAME then
OQQO0C0OQ0:=OO00Q0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_ISSUER_ALTERNATIVE_NAME then
OQQO0C0OQ0:=OQ00Q0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_SUBJECT_DIRECTORY_ATTRIBUTES then
OQQO0C0OQ0:=OC00Q0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_CERTIFICATE_POLICIES then
OQQO0C0OQ0:=OQQOC0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_POLICY_MAPPINGS then
OQQO0C0OQ0:=OCOCQ0OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_CRL_DISTRIBUTION_POINTS then
OQQO0C0OQ0:=OQ0COC0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_FRESHEST_CRL_POINTS then
OQQO0C0OQ0:=O0CCOC0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_PE_AUTHORITY_INFO_ACCESS then
OQQO0C0OQ0:=OCQ0OC0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_PE_SUBJECT_INFO_ACCESS then
OQQO0C0OQ0:=OQ0OOC0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_CRL_NUMBER then
OQQO0C0OQ0:=O0QOOC0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_DELTA_CRL_INDICATOR then
OQQO0C0OQ0:=OCQOOC0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_ISSUING_DISTRIBUTION_POINT then
OQQO0C0OQ0:=OQCOOC0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_CRL_REASONS then
OQQO0C0OQ0:=O0CQ0C0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_CRL_INVALIDITY_DATE then
OQQO0C0OQ0:=OCCQ0C0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_CE_CRL_CERTIFICATE_ISSUER then
OQQO0C0OQ0:=OQ0C0C0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
if OOQOQOOOQ0=OID_SIGNED_CERTIFICATE_TIMESTAMP_LIST then
OQQO0C0OQ0:=OQC00C0OQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0)
else
OQQO0C0OQ0:=OOQ000OOQ0.Create(OOQOQOOOQ0,OCQO0C0OQ0,OQC000OOQ0);
OOQC00QOQ0(OQQO0C0OQ0);
except
OQQO0C0OQ0.Free;
raise;
end;
end;
end;
function OQ0O0C0OQ0.O0CO0C0OQ0(OOCO0C0OQ0:O0Q000OOQ0):OOQ000OOQ0;
var
OQCO0C0OQ0:integer;
begin
for OQCO0C0OQ0:=0 to OOO000QOQ0-1 do begin
Result:=TObject(OQO000QOQ0[OQCO0C0OQ0])as OOQ000OOQ0;
if Result.ClassType=OOCO0C0OQ0 then
Exit;
end;
Result:=nil;
end;
function OQ0O0C0OQ0.OC0O0C0OQ0(O0OO0C0OQ0:integer):OOQ000OOQ0;
begin
Result:=TObject(OQO000QOQ0[O0OO0C0OQ0])as OOQ000OOQ0;
end;
procedure OQ0O0C0OQ0.OOOO0C0OQ0(OQOO0C0OQ0:integer;OCOO0C0OQ0:OOQ000OOQ0);
begin
OQO000QOQ0[OQOO0C0OQ0]:=OCOO0C0OQ0;
end;
class procedure O00QCC0OQ0.OO0QCC0OQ0(OQ0QCC0OQ0:O00QCOOOQ0;OCQQCOOOQ0:OC0QOQOOQ0);
begin
OQ0QCC0OQ0.OQQQCOOOQ0(OCQQCOOOQ0);
end;
class procedure O00QCC0OQ0.OO0QCC0OQ0(OQ0QCC0OQ0:OQOCO0OOQ0;OCQQCOOOQ0:OC0QOQOOQ0);
begin
OQ0QCC0OQ0.OCO0O0OOQ0(OCQQCOOOQ0);
end;
class procedure O00QCC0OQ0.OO0QCC0OQ0(OQ0QCC0OQ0:OQCCQOOOQ0;OCQQCOOOQ0:OC0QOQOOQ0);
begin
OQ0QCC0OQ0.OQ00QOOOQ0(OCQQCOOOQ0);
end;
class procedure O00QCC0OQ0.OO0QCC0OQ0(OQ0QCC0OQ0:OQ0O0C0OQ0;OCQQCOOOQ0:OC0QOQOOQ0);
begin
OQ0QCC0OQ0.O0QO0C0OQ0(OCQQCOOOQ0);
end;
class procedure O00QCC0OQ0.OC0QCC0OQ0(O0OQCC0OQ0:OQO00OOOQ0;OOOQCC0OQ0:TNotifyEvent);
begin
O0OQCC0OQ0.OOQ00OOOQ0:=OOOQCC0OQ0;
end;
class procedure O00QCC0OQ0.OQOQCC0OQ0(OCOQCC0OQ0:OQO00OOOQ0;O0QQCC0OQ0:TNotifyEvent);
begin
OCOQCC0OQ0.OQQ00OOOQ0:=O0QQCC0OQ0;
end;
class procedure O00QCC0OQ0.OOQQCC0OQ0(OQQQCC0OQ0:OQO00OOOQ0;OC0O0OOOQ0:boolean);
begin
OQQQCC0OQ0.OQ0O0OOOQ0(OC0O0OOOQ0);
end;
class procedure O00QCC0OQ0.OOQQCC0OQ0(OCQQCC0OQ0:O00QCOOOQ0;OC0O0OOOQ0:boolean);
begin
OCQQCC0OQ0.O0QQCOOOQ0(OC0O0OOOQ0);
end;
end.
