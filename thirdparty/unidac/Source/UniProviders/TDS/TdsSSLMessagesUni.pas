//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright � 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsSSLMessagesUni;
interface
uses
SysUtils,
CLRClasses,CRTypes,
{$IFNDEF UNIDACPRO}
TdsSSLTypes,TdsSSLConsts,TdsUtils;
{$ELSE}
TdsSSLTypesUni,TdsSSLConstsUni,TdsUtilsUni;
{$ENDIF}
const
OCOC00Q0Q0=4;
O0QC00Q0Q0=5;
OOQC00Q0Q0=16384;
OQQC00Q0Q0=OOQC00Q0Q0+2048;
type
OCQC00Q0Q0=class
private
O0CC00Q0Q0:OQQC0OQ0Q0;
OOCC00Q0Q0:TBytes;
OQCC00Q0Q0:integer;
OCCC00Q0Q0:integer;
O00000Q0Q0:integer;
OO0000Q0Q0:integer;
procedure OQ0000Q0Q0;
procedure O0O000Q0Q0(OOO000Q0Q0:integer);
public
constructor Create(const O0Q000Q0Q0:TBytes;OOQ000Q0Q0:integer);overload;
constructor Create;overload;
procedure OCQ000Q0Q0(O0C000Q0Q0:OQQC0OQ0Q0);
procedure OOC000Q0Q0;
procedure OQC000Q0Q0(const OCC000Q0Q0:TBytes);overload;
procedure OQC000Q0Q0(const OCC000Q0Q0:TBytes;OO0O00Q0Q0,O00O00Q0Q0:integer);overload;
procedure OQ0O00Q0Q0(const OC0O00Q0Q0:TBytes);
procedure OOOO00Q0Q0(const OQOO00Q0Q0:TBytes);
procedure O0QO00Q0Q0(const OOQO00Q0Q0:TBytes);
procedure OCQO00Q0Q0(O0CO00Q0Q0:byte);
procedure OOCO00Q0Q0(OQCO00Q0Q0:byte;OCCO00Q0Q0:integer);
procedure O00QC0Q0Q0(OO0QC0Q0Q0:word);
procedure OQ0QC0Q0Q0(OC0QC0Q0Q0:word;O0OQC0Q0Q0:integer);
procedure OOOQC0Q0Q0(OQOQC0Q0Q0:integer);
procedure OCOQC0Q0Q0(O0QQC0Q0Q0:integer;OOQQC0Q0Q0:integer);
procedure OQQQC0Q0Q0(OCQQC0Q0Q0:cardinal);
procedure O0CQC0Q0Q0(const OOCQC0Q0Q0:Int64);
procedure OQCQC0Q0Q0(OCCQC0Q0Q0:integer);
function O00CC0Q0Q0:byte;
function OO0CC0Q0Q0:word;
function OQ0CC0Q0Q0:integer;
function OC0CC0Q0Q0:cardinal;
function O0OCC0Q0Q0:Int64;
function OOOCC0Q0Q0(OQOCC0Q0Q0:integer):TBytes;
procedure OCOCC0Q0Q0(var O0QCC0Q0Q0:TBytes;OOQCC0Q0Q0,OQQCC0Q0Q0:integer);
procedure OCQCC0Q0Q0(O0CCC0Q0Q0:integer);
function OOCCC0Q0Q0:integer;
property OQCCC0Q0Q0:OQQC0OQ0Q0 read O0CC00Q0Q0;
property OCCCC0Q0Q0:TBytes read OOCC00Q0Q0;
property O000C0Q0Q0:integer read OQCC00Q0Q0;
property OO00C0Q0Q0:integer read OCCC00Q0Q0;
property OQ00C0Q0Q0:integer read O00000Q0Q0 write O00000Q0Q0;
end;
OC00C0Q0Q0=class
public
O0O0C0Q0Q0:O0OQ0OQ0Q0;
OOO0C0Q0Q0:OO0CCOQ0Q0;
OQO0C0Q0Q0:TBytes;
OCO0C0Q0Q0:TBytes;
O0Q0C0Q0Q0:integer;
OOQ0C0Q0Q0:integer;
OQQ0C0Q0Q0:integer;
OCQ0C0Q0Q0:integer;
constructor Create;
procedure OOC0C0Q0Q0(OQC0C0Q0Q0:integer);
procedure OCC0C0Q0Q0;
procedure O00OC0Q0Q0(OO0OC0Q0Q0:integer);
end;
OQ0OC0Q0Q0=class
public
OC0OC0Q0Q0:O0OQ0OQ0Q0;
O0OOC0Q0Q0:OO0CCOQ0Q0;
OOOOC0Q0Q0:integer;
OQOOC0Q0Q0:integer;
procedure OCOOC0Q0Q0(O0QOC0Q0Q0:PByteArray;OO0OC0Q0Q0:integer);
end;
implementation
const
OOQOC0Q0Q0=OQQC00Q0Q0;
OQQOC0Q0Q0=8192;
constructor OCQC00Q0Q0.Create(const O0Q000Q0Q0:TBytes;OOQ000Q0Q0:integer);
var
OQQ000Q0Q0:integer;
begin
inherited Create;
OQQ000Q0Q0:=(O0Q000Q0Q0[OOQ000Q0Q0+1]shl 16)+(O0Q000Q0Q0[OOQ000Q0Q0+2]shl 8)+O0Q000Q0Q0[OOQ000Q0Q0+3];
if Length(O0Q000Q0Q0)<OOQ000Q0Q0+OCOC00Q0Q0+OQQ000Q0Q0 then
raise EScError.Create(seInvalidInputArgs);
O0CC00Q0Q0:=O00QO0Q0Q0.OC0QO0Q0Q0(O0Q000Q0Q0[OOQ000Q0Q0]);
OOCC00Q0Q0:=O0Q000Q0Q0;
OQCC00Q0Q0:=OOQ000Q0Q0+OCOC00Q0Q0;
OCCC00Q0Q0:=OOQ000Q0Q0+OCOC00Q0Q0;
O00000Q0Q0:=OOQ000Q0Q0+OCOC00Q0Q0+OQQ000Q0Q0;
end;
constructor OCQC00Q0Q0.Create;
begin
inherited Create;
SetLength(OOCC00Q0Q0,OOQOC0Q0Q0);
OO0000Q0Q0:=OOQOC0Q0Q0;
OQCC00Q0Q0:=OCOC00Q0Q0;
OCCC00Q0Q0:=OCOC00Q0Q0;
O00000Q0Q0:=OCOC00Q0Q0;
end;
procedure OCQC00Q0Q0.OCQ000Q0Q0(O0C000Q0Q0:OQQC0OQ0Q0);
begin
O0CC00Q0Q0:=O0C000Q0Q0;
OQCC00Q0Q0:=OCOC00Q0Q0;
OCCC00Q0Q0:=OCOC00Q0Q0;
O00000Q0Q0:=OCOC00Q0Q0;
end;
procedure OCQC00Q0Q0.OQ0000Q0Q0;
var
OC0000Q0Q0:integer;
begin
OC0000Q0Q0:=O00000Q0Q0-OQCC00Q0Q0;
OOCC00Q0Q0[0]:=OCQC0OQ0Q0[O0CC00Q0Q0];
OOCC00Q0Q0[1]:=byte(OC0000Q0Q0 shr 16);
OOCC00Q0Q0[2]:=byte(OC0000Q0Q0 shr 8);
OOCC00Q0Q0[3]:=byte(OC0000Q0Q0);
end;
procedure OCQC00Q0Q0.OOC000Q0Q0;
begin
OQ0000Q0Q0;
end;
procedure OCQC00Q0Q0.O0O000Q0Q0(OOO000Q0Q0:integer);
var
OQO000Q0Q0:integer;
begin
OQO000Q0Q0:=O00000Q0Q0+OOO000Q0Q0-OO0000Q0Q0;
if OQO000Q0Q0>0 then begin
if OQO000Q0Q0<OQQOC0Q0Q0 then
OQO000Q0Q0:=OQQOC0Q0Q0;
Inc(OO0000Q0Q0,OQO000Q0Q0);
SetLength(OOCC00Q0Q0,OO0000Q0Q0);
end;
end;
procedure OCQC00Q0Q0.OQC000Q0Q0(const OCC000Q0Q0:TBytes);
var
O00O00Q0Q0:integer;
begin
O00O00Q0Q0:=Length(OCC000Q0Q0);
O0O000Q0Q0(O00O00Q0Q0);
if O00O00Q0Q0>0 then begin
Move(OCC000Q0Q0[0],OOCC00Q0Q0[O00000Q0Q0],O00O00Q0Q0);
Inc(O00000Q0Q0,O00O00Q0Q0);
end;
end;
procedure OCQC00Q0Q0.OQC000Q0Q0(const OCC000Q0Q0:TBytes;OO0O00Q0Q0,O00O00Q0Q0:integer);
begin
O0O000Q0Q0(O00O00Q0Q0);
if O00O00Q0Q0>0 then begin
Move(OCC000Q0Q0[OO0O00Q0Q0],OOCC00Q0Q0[O00000Q0Q0],O00O00Q0Q0);
Inc(O00000Q0Q0,O00O00Q0Q0);
end;
end;
procedure OCQC00Q0Q0.OQ0O00Q0Q0(const OC0O00Q0Q0:TBytes);
var
O0OO00Q0Q0:integer;
begin
O0OO00Q0Q0:=byte(Length(OC0O00Q0Q0));
O0O000Q0Q0(O0OO00Q0Q0+1);
OOCC00Q0Q0[O00000Q0Q0]:=O0OO00Q0Q0;
Inc(O00000Q0Q0);
if O0OO00Q0Q0>0 then begin
Move(OC0O00Q0Q0[0],OOCC00Q0Q0[O00000Q0Q0],O0OO00Q0Q0);
Inc(O00000Q0Q0,O0OO00Q0Q0);
end;
end;
procedure OCQC00Q0Q0.OOOO00Q0Q0(const OQOO00Q0Q0:TBytes);
var
OCOO00Q0Q0:integer;
begin
OCOO00Q0Q0:=word(Length(OQOO00Q0Q0));
O0O000Q0Q0(OCOO00Q0Q0+2);
OOCC00Q0Q0[O00000Q0Q0]:=byte(OCOO00Q0Q0 shr 8);
OOCC00Q0Q0[O00000Q0Q0+1]:=byte(OCOO00Q0Q0);
Inc(O00000Q0Q0,2);
if OCOO00Q0Q0>0 then begin
Move(OQOO00Q0Q0[0],OOCC00Q0Q0[O00000Q0Q0],OCOO00Q0Q0);
Inc(O00000Q0Q0,OCOO00Q0Q0);
end;
end;
procedure OCQC00Q0Q0.O0QO00Q0Q0(const OOQO00Q0Q0:TBytes);
var
OQQO00Q0Q0:integer;
begin
OQQO00Q0Q0:=Length(OOQO00Q0Q0)and$FFFFFF;
O0O000Q0Q0(OQQO00Q0Q0+3);
OOCC00Q0Q0[O00000Q0Q0]:=byte(OQQO00Q0Q0 shr 16);
OOCC00Q0Q0[O00000Q0Q0+1]:=byte(OQQO00Q0Q0 shr 8);
OOCC00Q0Q0[O00000Q0Q0+2]:=byte(OQQO00Q0Q0);
Inc(O00000Q0Q0,3);
if OQQO00Q0Q0>0 then begin
Move(OOQO00Q0Q0[0],OOCC00Q0Q0[O00000Q0Q0],OQQO00Q0Q0);
Inc(O00000Q0Q0,OQQO00Q0Q0);
end;
end;
procedure OCQC00Q0Q0.OCQO00Q0Q0(O0CO00Q0Q0:byte);
begin
O0O000Q0Q0(1);
OOCC00Q0Q0[O00000Q0Q0]:=O0CO00Q0Q0;
Inc(O00000Q0Q0);
end;
procedure OCQC00Q0Q0.OOCO00Q0Q0(OQCO00Q0Q0:byte;OCCO00Q0Q0:integer);
begin
if OCCO00Q0Q0>=O00000Q0Q0 then
raise EScError.Create(seInvalidInputArgs);
OOCC00Q0Q0[OCCO00Q0Q0]:=OQCO00Q0Q0;
end;
procedure OCQC00Q0Q0.O00QC0Q0Q0(OO0QC0Q0Q0:word);
begin
O0O000Q0Q0(2);
OOCC00Q0Q0[O00000Q0Q0]:=byte(OO0QC0Q0Q0 shr 8);
OOCC00Q0Q0[O00000Q0Q0+1]:=byte(OO0QC0Q0Q0);
Inc(O00000Q0Q0,2);
end;
procedure OCQC00Q0Q0.OQ0QC0Q0Q0(OC0QC0Q0Q0:word;O0OQC0Q0Q0:integer);
begin
if O0OQC0Q0Q0>=O00000Q0Q0 then
raise EScError.Create(seInvalidInputArgs);
OOCC00Q0Q0[O0OQC0Q0Q0]:=byte(OC0QC0Q0Q0 shr 8);
OOCC00Q0Q0[O0OQC0Q0Q0+1]:=byte(OC0QC0Q0Q0);
end;
procedure OCQC00Q0Q0.OOOQC0Q0Q0(OQOQC0Q0Q0:integer);
begin
O0O000Q0Q0(3);
OOCC00Q0Q0[O00000Q0Q0]:=byte(OQOQC0Q0Q0 shr 16);
OOCC00Q0Q0[O00000Q0Q0+1]:=byte(OQOQC0Q0Q0 shr 8);
OOCC00Q0Q0[O00000Q0Q0+2]:=byte(OQOQC0Q0Q0);
Inc(O00000Q0Q0,3);
end;
procedure OCQC00Q0Q0.OCOQC0Q0Q0(O0QQC0Q0Q0:integer;OOQQC0Q0Q0:integer);
begin
if OOQQC0Q0Q0>=O00000Q0Q0 then
raise EScError.Create(seInvalidInputArgs);
OOCC00Q0Q0[OOQQC0Q0Q0]:=byte(O0QQC0Q0Q0 shr 16);
OOCC00Q0Q0[OOQQC0Q0Q0+1]:=byte(O0QQC0Q0Q0 shr 8);
OOCC00Q0Q0[OOQQC0Q0Q0+2]:=byte(O0QQC0Q0Q0);
end;
procedure OCQC00Q0Q0.OQQQC0Q0Q0(OCQQC0Q0Q0:cardinal);
begin
O0O000Q0Q0(4);
OOCC00Q0Q0[O00000Q0Q0]:=byte(OCQQC0Q0Q0 shr 24);
OOCC00Q0Q0[O00000Q0Q0+1]:=byte(OCQQC0Q0Q0 shr 16);
OOCC00Q0Q0[O00000Q0Q0+2]:=byte(OCQQC0Q0Q0 shr 8);
OOCC00Q0Q0[O00000Q0Q0+3]:=byte(OCQQC0Q0Q0);
Inc(O00000Q0Q0,4);
end;
procedure OCQC00Q0Q0.O0CQC0Q0Q0(const OOCQC0Q0Q0:Int64);
begin
O0O000Q0Q0(8);
OOCC00Q0Q0[O00000Q0Q0]:=byte(OOCQC0Q0Q0 shr 56);
OOCC00Q0Q0[O00000Q0Q0+1]:=byte(OOCQC0Q0Q0 shr 48);
OOCC00Q0Q0[O00000Q0Q0+2]:=byte(OOCQC0Q0Q0 shr 40);
OOCC00Q0Q0[O00000Q0Q0+3]:=byte(OOCQC0Q0Q0 shr 32);
OOCC00Q0Q0[O00000Q0Q0+4]:=byte(OOCQC0Q0Q0 shr 24);
OOCC00Q0Q0[O00000Q0Q0+5]:=byte(OOCQC0Q0Q0 shr 16);
OOCC00Q0Q0[O00000Q0Q0+6]:=byte(OOCQC0Q0Q0 shr 8);
OOCC00Q0Q0[O00000Q0Q0+7]:=byte(OOCQC0Q0Q0);
Inc(O00000Q0Q0,8);
end;
procedure OCQC00Q0Q0.OQCQC0Q0Q0(OCCQC0Q0Q0:integer);
begin
O0O000Q0Q0(OCCQC0Q0Q0);
Inc(O00000Q0Q0,OCCQC0Q0Q0);
end;
function OCQC00Q0Q0.O00CC0Q0Q0:byte;
begin
if OCCC00Q0Q0>=O00000Q0Q0 then
raise EScError.Create(seInvalidMessage);
Result:=OOCC00Q0Q0[OCCC00Q0Q0];
Inc(OCCC00Q0Q0);
end;
function OCQC00Q0Q0.OO0CC0Q0Q0:word;
begin
if(OCCC00Q0Q0+2)>O00000Q0Q0 then
raise EScError.Create(seInvalidMessage);
Result:=(OOCC00Q0Q0[OCCC00Q0Q0]shl 8)+
OOCC00Q0Q0[OCCC00Q0Q0+1];
Inc(OCCC00Q0Q0,2);
end;
function OCQC00Q0Q0.OQ0CC0Q0Q0:integer;
begin
if(OCCC00Q0Q0+3)>O00000Q0Q0 then
raise EScError.Create(seInvalidMessage);
Result:=(OOCC00Q0Q0[OCCC00Q0Q0]shl 16)+
(OOCC00Q0Q0[OCCC00Q0Q0+1]shl 8)+
OOCC00Q0Q0[OCCC00Q0Q0+2];
Inc(OCCC00Q0Q0,3);
end;
function OCQC00Q0Q0.OC0CC0Q0Q0:cardinal;
begin
if(OCCC00Q0Q0+4)>O00000Q0Q0 then
raise EScError.Create(seInvalidMessage);
Result:=(OOCC00Q0Q0[OCCC00Q0Q0]shl 24)+
(OOCC00Q0Q0[OCCC00Q0Q0+1]shl 16)+
(OOCC00Q0Q0[OCCC00Q0Q0+2]shl 8)+
OOCC00Q0Q0[OCCC00Q0Q0+3];
Inc(OCCC00Q0Q0,4);
end;
function OCQC00Q0Q0.O0OCC0Q0Q0:Int64;
begin
if(OCCC00Q0Q0+8)>O00000Q0Q0 then
raise EScError.Create(seInvalidMessage);
Result:=(Int64(OOCC00Q0Q0[OCCC00Q0Q0])shl 56)+
(Int64(OOCC00Q0Q0[OCCC00Q0Q0+1])shl 48)+
(Int64(OOCC00Q0Q0[OCCC00Q0Q0+2])shl 40)+
(Int64(OOCC00Q0Q0[OCCC00Q0Q0+3])shl 32)+
(Int64(OOCC00Q0Q0[OCCC00Q0Q0+4])shl 24)+
(Int64(OOCC00Q0Q0[OCCC00Q0Q0+5])shl 16)+
(Int64(OOCC00Q0Q0[OCCC00Q0Q0+6])shl 8)+
OOCC00Q0Q0[OCCC00Q0Q0+7];
Inc(OCCC00Q0Q0,8);
end;
function OCQC00Q0Q0.OOOCC0Q0Q0(OQOCC0Q0Q0:integer):TBytes;
begin
if((OCCC00Q0Q0+OQOCC0Q0Q0)>O00000Q0Q0)or(OQOCC0Q0Q0<0)then
raise EScError.Create(seInvalidMessage);
SetLength(Result,OQOCC0Q0Q0);
if OQOCC0Q0Q0>0 then
Move(OOCC00Q0Q0[OCCC00Q0Q0],Result[0],OQOCC0Q0Q0);
Inc(OCCC00Q0Q0,OQOCC0Q0Q0);
end;
procedure OCQC00Q0Q0.OCOCC0Q0Q0(var O0QCC0Q0Q0:TBytes;OOQCC0Q0Q0,OQQCC0Q0Q0:integer);
begin
if((OCCC00Q0Q0+OQQCC0Q0Q0)>O00000Q0Q0)or(OQQCC0Q0Q0<0)then
raise EScError.Create(seInvalidMessage);
if OQQCC0Q0Q0>0 then
Move(OOCC00Q0Q0[OCCC00Q0Q0],O0QCC0Q0Q0[OOQCC0Q0Q0],OQQCC0Q0Q0);
Inc(OCCC00Q0Q0,OQQCC0Q0Q0);
end;
procedure OCQC00Q0Q0.OCQCC0Q0Q0(O0CCC0Q0Q0:integer);
begin
if((OCCC00Q0Q0+O0CCC0Q0Q0)>O00000Q0Q0)or(O0CCC0Q0Q0<0)then
raise EScError.Create(seInvalidMessage);
Inc(OCCC00Q0Q0,O0CCC0Q0Q0);
end;
function OCQC00Q0Q0.OOCCC0Q0Q0:integer;
begin
Result:=O00000Q0Q0-OCCC00Q0Q0;
end;
constructor OC00C0Q0Q0.Create;
begin
inherited;
System.SetLength(OQO0C0Q0Q0,OQQC00Q0Q0*3);
O0Q0C0Q0Q0:=0;
OOQ0C0Q0Q0:=0;
end;
procedure OC00C0Q0Q0.OOC0C0Q0Q0(OQC0C0Q0Q0:integer);
begin
if OOQ0C0Q0Q0+OQC0C0Q0Q0>System.Length(OQO0C0Q0Q0)then
System.SetLength(OQO0C0Q0Q0,OOQ0C0Q0Q0+OQC0C0Q0Q0);
end;
procedure OC00C0Q0Q0.OCC0C0Q0Q0;
begin
if O0Q0C0Q0Q0=OOQ0C0Q0Q0 then begin
O0Q0C0Q0Q0:=0;
OOQ0C0Q0Q0:=0;
end
else
if O0Q0C0Q0Q0>=OOQC00Q0Q0 then begin
Move(OQO0C0Q0Q0[O0Q0C0Q0Q0],OQO0C0Q0Q0[0],OOQ0C0Q0Q0-O0Q0C0Q0Q0);
Dec(OOQ0C0Q0Q0,O0Q0C0Q0Q0);
O0Q0C0Q0Q0:=0;
end;
end;
procedure OC00C0Q0Q0.O00OC0Q0Q0(OO0OC0Q0Q0:integer);
begin
OQO0C0Q0Q0[O0Q0C0Q0Q0]:=OOOQ0OQ0Q0[O0O0C0Q0Q0];
OQO0C0Q0Q0[O0Q0C0Q0Q0+1]:=OOO0C0Q0Q0.OQ0CCOQ0Q0;
OQO0C0Q0Q0[O0Q0C0Q0Q0+2]:=OOO0C0Q0Q0.OC0CCOQ0Q0;
OQO0C0Q0Q0[O0Q0C0Q0Q0+3]:=byte(OO0OC0Q0Q0 shr 8);
OQO0C0Q0Q0[O0Q0C0Q0Q0+4]:=byte(OO0OC0Q0Q0);
end;
procedure OQ0OC0Q0Q0.OCOOC0Q0Q0(O0QOC0Q0Q0:PByteArray;OO0OC0Q0Q0:integer);
begin
O0QOC0Q0Q0[0]:=OOOQ0OQ0Q0[OC0OC0Q0Q0];
O0QOC0Q0Q0[1]:=O0OOC0Q0Q0.OQ0CCOQ0Q0;
O0QOC0Q0Q0[2]:=O0OOC0Q0Q0.OC0CCOQ0Q0;
O0QOC0Q0Q0[3]:=byte(OO0OC0Q0Q0 shr 8);
O0QOC0Q0Q0[4]:=byte(OO0OC0Q0Q0);
end;
end.