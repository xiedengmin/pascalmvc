//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsReaderWriterUni;
interface
uses
SysUtils,
CLRClasses,CRTypes,CRBigInteger;
type
OQOOOOOCQ0=type byte;
OCOOOOOCQ0=class
protected
O0QOOOOCQ0:TBytes;
OOQOOOOCQ0:Integer;
OQQOOOOCQ0:Integer;
function OCQOOOOCQ0:Integer;
public
constructor Create(const OOCOOOOCQ0:TBytes);overload;virtual;
constructor Create(const OOCOOOOCQ0:TBytes;OQCOOOOCQ0:Integer);overload;
function OCCOOOOCQ0:Byte;
function O00Q0OOCQ0:Boolean;
function OO0Q0OOCQ0:Byte;
function OQ0Q0OOCQ0:Word;
function OC0Q0OOCQ0:Integer;
function O0OQ0OOCQ0:cardinal;
function OOOQ0OOCQ0:Int64;
function OQOQ0OOCQ0:TBytes;overload;
function OQOQ0OOCQ0(const O0QQ0OOCQ0:TBytes;const OOQQ0OOCQ0,OQQQ0OOCQ0:Integer):Integer;overload;
function OCQQ0OOCQ0(O0CQ0OOCQ0:Integer):TBytes;
function OOCQ0OOCQ0:TBigInteger;
function OQCQ0OOCQ0:TBytes;
procedure OCCQ0OOCQ0;virtual;
property O00C0OOCQ0:TBytes read O0QOOOOCQ0;
property OO0C0OOCQ0:Integer read OOQOOOOCQ0;
property OQ0C0OOCQ0:Integer read OCQOOOOCQ0;
end;
OC0C0OOCQ0=class
protected
O0OC0OOCQ0:TBytes;
OOOC0OOCQ0:Integer;
OQOC0OOCQ0:Integer;
procedure OCOC0OOCQ0(O0QC0OOCQ0:Integer);
public
constructor Create;overload;
constructor Create(OCQC0OOCQ0:Integer);overload;
procedure O0CC0OOCQ0;
function OOCC0OOCQ0:TBytes;
procedure OQCC0OOCQ0(OCCC0OOCQ0:TBigInteger);
procedure OO000OOCQ0(const OQ000OOCQ0:TBytes);overload;
procedure OO000OOCQ0(const OQ000OOCQ0:TValueArr;O0O00OOCQ0,OC000OOCQ0:Integer);overload;
procedure OOO00OOCQ0(OQO00OOCQ0:Boolean);
procedure OCO00OOCQ0(O0Q00OOCQ0:Byte);
procedure OOQ00OOCQ0(OQQ00OOCQ0:Word);
procedure OCQ00OOCQ0(O0C00OOCQ0:Integer);
procedure OOC00OOCQ0(const OQC00OOCQ0:Int64);
procedure OCC00OOCQ0(const O00O0OOCQ0:string);
procedure OO0O0OOCQ0(const OQ0O0OOCQ0:WideString);
procedure O0OO0OOCQ0(const OOOO0OOCQ0:TBytes);overload;
procedure O0OO0OOCQ0(const OOOO0OOCQ0:TValueArr;OQOO0OOCQ0,OCOO0OOCQ0:Integer);overload;
procedure O0QO0OOCQ0(const OOQO0OOCQ0:TBytes);overload;
procedure O0QO0OOCQ0(const OOQO0OOCQ0:TBytes;OCQO0OOCQ0,O0CO0OOCQ0:Integer);overload;
property OOCO0OOCQ0:TBytes read O0OC0OOCQ0;
property OQCO0OOCQ0:Integer read OOOC0OOCQ0;
end;
OCCO0OOCQ0=class(OCOOOOOCQ0)
public
function O00QCOOCQ0:TBigInteger;
function OQ0QCOOCQ0:OQOOOOOCQ0;
end;
OC0QCOOCQ0=class(OC0C0OOCQ0)
public
procedure O0OQCOOCQ0(OOOQCOOCQ0:TBigInteger);
procedure OQOQCOOCQ0(OCOQCOOCQ0:OQOOOOOCQ0);
end;
implementation
uses
{$IFNDEF UNIDACPRO}
TdsUtils,TdsSSLConsts;
{$ELSE}
TdsUtilsUni,TdsSSLConstsUni;
{$ENDIF}
const
O0QQCOOCQ0=1024;
constructor OCOOOOOCQ0.Create(const OOCOOOOCQ0:TBytes);
begin
inherited Create;
O0QOOOOCQ0:=OOCOOOOCQ0;
OQQOOOOCQ0:=Length(O0QOOOOCQ0);
OOQOOOOCQ0:=0;
end;
constructor OCOOOOOCQ0.Create(const OOCOOOOCQ0:TBytes;OQCOOOOCQ0:Integer);
begin
inherited Create;
O0QOOOOCQ0:=OOCOOOOCQ0;
OQQOOOOCQ0:=OQCOOOOCQ0;
OOQOOOOCQ0:=0;
end;
procedure OCOOOOOCQ0.OCCQ0OOCQ0;
begin
OOQOOOOCQ0:=0;
end;
function OCOOOOOCQ0.OCCOOOOCQ0:Byte;
begin
if OOQOOOOCQ0>=OQQOOOOCQ0 then
raise EScError.Create(seUnexpectedEOP);
Result:=O0QOOOOCQ0[OOQOOOOCQ0];
end;
function OCOOOOOCQ0.O00Q0OOCQ0:Boolean;
begin
if OOQOOOOCQ0>=OQQOOOOCQ0 then
raise EScError.Create(seUnexpectedEOP);
if O0QOOOOCQ0[OOQOOOOCQ0]=0 then
Result:=False
else
Result:=True;
Inc(OOQOOOOCQ0);
end;
function OCOOOOOCQ0.OO0Q0OOCQ0:Byte;
begin
if OOQOOOOCQ0>=OQQOOOOCQ0 then
raise EScError.Create(seUnexpectedEOP);
Result:=O0QOOOOCQ0[OOQOOOOCQ0];
Inc(OOQOOOOCQ0);
end;
function OCOOOOOCQ0.OQ0Q0OOCQ0:Word;
begin
if(OOQOOOOCQ0+2)>OQQOOOOCQ0 then
raise EScError.Create(seUnexpectedEOP);
Result:=(Word(O0QOOOOCQ0[OOQOOOOCQ0])shl 8)+
O0QOOOOCQ0[OOQOOOOCQ0+1];
Inc(OOQOOOOCQ0,2);
end;
function OCOOOOOCQ0.OC0Q0OOCQ0:Integer;
begin
if(OOQOOOOCQ0+4)>OQQOOOOCQ0 then
raise EScError.Create(seUnexpectedEOP);
Result:=(integer(O0QOOOOCQ0[OOQOOOOCQ0])shl 24)+
(integer(O0QOOOOCQ0[OOQOOOOCQ0+1])shl 16)+
(integer(O0QOOOOCQ0[OOQOOOOCQ0+2])shl 8)+
integer(O0QOOOOCQ0[OOQOOOOCQ0+3]);
Inc(OOQOOOOCQ0,4);
end;
function OCOOOOOCQ0.O0OQ0OOCQ0:cardinal;
begin
if(OOQOOOOCQ0+4)>OQQOOOOCQ0 then
raise EScError.Create(seUnexpectedEOP);
Result:=(cardinal(O0QOOOOCQ0[OOQOOOOCQ0])shl 24)+
(cardinal(O0QOOOOCQ0[OOQOOOOCQ0+1])shl 16)+
(cardinal(O0QOOOOCQ0[OOQOOOOCQ0+2])shl 8)+
cardinal(O0QOOOOCQ0[OOQOOOOCQ0+3]);
Inc(OOQOOOOCQ0,4);
end;
function OCOOOOOCQ0.OOOQ0OOCQ0:Int64;
begin
if(OOQOOOOCQ0+8)>OQQOOOOCQ0 then
raise EScError.Create(seUnexpectedEOP);
Result:=(Int64(O0QOOOOCQ0[OOQOOOOCQ0])shl 56)+
(Int64(O0QOOOOCQ0[OOQOOOOCQ0+1])shl 48)+
(Int64(O0QOOOOCQ0[OOQOOOOCQ0+2])shl 40)+
(Int64(O0QOOOOCQ0[OOQOOOOCQ0+3])shl 32)+
(Int64(O0QOOOOCQ0[OOQOOOOCQ0+4])shl 24)+
(Int64(O0QOOOOCQ0[OOQOOOOCQ0+5])shl 16)+
(Int64(O0QOOOOCQ0[OOQOOOOCQ0+6])shl 8)+
O0QOOOOCQ0[OOQOOOOCQ0+7];
Inc(OOQOOOOCQ0,8);
end;
function OCOOOOOCQ0.OQOQ0OOCQ0:TBytes;
var
OCOQ0OOCQ0:Integer;
begin
OCOQ0OOCQ0:=OC0Q0OOCQ0;
Result:=OCQQ0OOCQ0(OCOQ0OOCQ0);
end;
function OCOOOOOCQ0.OQOQ0OOCQ0(const O0QQ0OOCQ0:TBytes;const OOQQ0OOCQ0,OQQQ0OOCQ0:Integer):Integer;
begin
Result:=OC0Q0OOCQ0;
if Result>OQQQ0OOCQ0 then
raise EScError.Create(seUnexpectedEOP);
if((OOQOOOOCQ0+Result)>OQQOOOOCQ0)or(Result<0)then
raise EScError.Create(seUnexpectedEOP);
if Result>0 then
Buffer.BlockCopy(O0QOOOOCQ0,OOQOOOOCQ0,O0QQ0OOCQ0,OOQQ0OOCQ0,Result);
Inc(OOQOOOOCQ0,Result);
end;
function OCOOOOOCQ0.OCQQ0OOCQ0(O0CQ0OOCQ0:Integer):TBytes;
begin
if((OOQOOOOCQ0+O0CQ0OOCQ0)>OQQOOOOCQ0)or(O0CQ0OOCQ0<0)then
raise EScError.Create(seUnexpectedEOP);
SetLength(Result,O0CQ0OOCQ0);
if O0CQ0OOCQ0>0 then
Buffer.BlockCopy(O0QOOOOCQ0,OOQOOOOCQ0,Result,0,O0CQ0OOCQ0);
Inc(OOQOOOOCQ0,O0CQ0OOCQ0);
end;
function OCOOOOOCQ0.OOCQ0OOCQ0:TBigInteger;
begin
Result:=TBigInteger.Create(OQOQ0OOCQ0);
end;
function OCOOOOOCQ0.OQCQ0OOCQ0:TBytes;
begin
SetLength(Result,OQQOOOOCQ0-OOQOOOOCQ0);
if Length(Result)>0 then
Buffer.BlockCopy(O0QOOOOCQ0,OOQOOOOCQ0,Result,0,Length(Result));
OOQOOOOCQ0:=OQQOOOOCQ0;
end;
function OCOOOOOCQ0.OCQOOOOCQ0:Integer;
begin
Result:=OQQOOOOCQ0-OOQOOOOCQ0;
end;
constructor OC0C0OOCQ0.Create;
begin
inherited;
SetLength(O0OC0OOCQ0,O0QQCOOCQ0);
OQOC0OOCQ0:=O0QQCOOCQ0;
OOOC0OOCQ0:=0;
end;
constructor OC0C0OOCQ0.Create(OCQC0OOCQ0:Integer);
begin
inherited Create;
SetLength(O0OC0OOCQ0,OCQC0OOCQ0);
OQOC0OOCQ0:=OCQC0OOCQ0;
OOOC0OOCQ0:=0;
end;
procedure OC0C0OOCQ0.OCOC0OOCQ0(O0QC0OOCQ0:Integer);
var
OOQC0OOCQ0:integer;
begin
OOQC0OOCQ0:=OOOC0OOCQ0+O0QC0OOCQ0-OQOC0OOCQ0;
if OOQC0OOCQ0>0 then begin
if OQOC0OOCQ0<O0QQCOOCQ0 then begin
if OOQC0OOCQ0<OQOC0OOCQ0 then
OOQC0OOCQ0:=OQOC0OOCQ0;
end
else begin
if OOQC0OOCQ0<O0QQCOOCQ0 then
OOQC0OOCQ0:=O0QQCOOCQ0;
end;
Inc(OQOC0OOCQ0,OOQC0OOCQ0);
SetLength(O0OC0OOCQ0,OQOC0OOCQ0);
end;
end;
procedure OC0C0OOCQ0.O0CC0OOCQ0;
begin
OOOC0OOCQ0:=0;
end;
function OC0C0OOCQ0.OOCC0OOCQ0:TBytes;
begin
SetLength(Result,OOOC0OOCQ0);
Buffer.BlockCopy(O0OC0OOCQ0,0,Result,0,OOOC0OOCQ0);
end;
procedure OC0C0OOCQ0.OQCC0OOCQ0(OCCC0OOCQ0:TBigInteger);
var
O0000OOCQ0:TBytes;
begin
O0000OOCQ0:=OCCC0OOCQ0.GetBytes;
O0QO0OOCQ0(O0000OOCQ0);
end;
procedure OC0C0OOCQ0.OO000OOCQ0(const OQ000OOCQ0:TBytes);
var
OC000OOCQ0:Integer;
begin
OC000OOCQ0:=Length(OQ000OOCQ0);
OCOC0OOCQ0(OC000OOCQ0);
Move(OQ000OOCQ0[0],O0OC0OOCQ0[OOOC0OOCQ0],OC000OOCQ0);
Inc(OOOC0OOCQ0,OC000OOCQ0);
end;
procedure OC0C0OOCQ0.OO000OOCQ0(const OQ000OOCQ0:TValueArr;O0O00OOCQ0,OC000OOCQ0:Integer);
begin
OCOC0OOCQ0(OC000OOCQ0);
Move(OQ000OOCQ0[O0O00OOCQ0],O0OC0OOCQ0[OOOC0OOCQ0],OC000OOCQ0);
Inc(OOOC0OOCQ0,OC000OOCQ0);
end;
procedure OC0C0OOCQ0.OOO00OOCQ0(OQO00OOCQ0:Boolean);
begin
OCOC0OOCQ0(1);
if OQO00OOCQ0 then
O0OC0OOCQ0[OOOC0OOCQ0]:=1
else
O0OC0OOCQ0[OOOC0OOCQ0]:=0;
Inc(OOOC0OOCQ0);
end;
procedure OC0C0OOCQ0.OCO00OOCQ0(O0Q00OOCQ0:Byte);
begin
OCOC0OOCQ0(1);
O0OC0OOCQ0[OOOC0OOCQ0]:=O0Q00OOCQ0;
Inc(OOOC0OOCQ0);
end;
procedure OC0C0OOCQ0.OOQ00OOCQ0(OQQ00OOCQ0:Word);
begin
OCOC0OOCQ0(2);
O0OC0OOCQ0[OOOC0OOCQ0]:=byte(OQQ00OOCQ0 shr 8);
O0OC0OOCQ0[OOOC0OOCQ0+1]:=byte(OQQ00OOCQ0);
Inc(OOOC0OOCQ0,2);
end;
procedure OC0C0OOCQ0.OCQ00OOCQ0(O0C00OOCQ0:Integer);
begin
OCOC0OOCQ0(4);
O0OC0OOCQ0[OOOC0OOCQ0]:=byte(cardinal(O0C00OOCQ0)shr 24);
O0OC0OOCQ0[OOOC0OOCQ0+1]:=byte(cardinal(O0C00OOCQ0)shr 16);
O0OC0OOCQ0[OOOC0OOCQ0+2]:=byte(cardinal(O0C00OOCQ0)shr 8);
O0OC0OOCQ0[OOOC0OOCQ0+3]:=byte(O0C00OOCQ0);
Inc(OOOC0OOCQ0,4);
end;
procedure OC0C0OOCQ0.OOC00OOCQ0(const OQC00OOCQ0:Int64);
begin
OCOC0OOCQ0(8);
O0OC0OOCQ0[OOOC0OOCQ0]:=byte(OQC00OOCQ0 shr 56);
O0OC0OOCQ0[OOOC0OOCQ0+1]:=byte(OQC00OOCQ0 shr 48);
O0OC0OOCQ0[OOOC0OOCQ0+2]:=byte(OQC00OOCQ0 shr 40);
O0OC0OOCQ0[OOOC0OOCQ0+3]:=byte(OQC00OOCQ0 shr 32);
O0OC0OOCQ0[OOOC0OOCQ0+4]:=byte(OQC00OOCQ0 shr 24);
O0OC0OOCQ0[OOOC0OOCQ0+5]:=byte(OQC00OOCQ0 shr 16);
O0OC0OOCQ0[OOOC0OOCQ0+6]:=byte(OQC00OOCQ0 shr 8);
O0OC0OOCQ0[OOOC0OOCQ0+7]:=byte(OQC00OOCQ0);
Inc(OOOC0OOCQ0,8);
end;
procedure OC0C0OOCQ0.OCC00OOCQ0(const O00O0OOCQ0:string);
begin
OCQ00OOCQ0(Length(O00O0OOCQ0));
if Length(O00O0OOCQ0)>0 then
OO000OOCQ0(Encoding.Default.GetBytes(O00O0OOCQ0));
end;
procedure OC0C0OOCQ0.OO0O0OOCQ0(const OQ0O0OOCQ0:WideString);
var
OC0O0OOCQ0:TBytes;
begin
if Length(OQ0O0OOCQ0)>0 then begin
SetLength(OC0O0OOCQ0,0);
OC0O0OOCQ0:=Encoding.UTF8.GetBytes(OQ0O0OOCQ0);
OCQ00OOCQ0(Length(OC0O0OOCQ0));
if Length(OC0O0OOCQ0)>0 then
OO000OOCQ0(OC0O0OOCQ0);
end
else
OCQ00OOCQ0(0);
end;
procedure OC0C0OOCQ0.O0OO0OOCQ0(const OOOO0OOCQ0:TBytes);
begin
OCQ00OOCQ0(Length(OOOO0OOCQ0));
if Length(OOOO0OOCQ0)>0 then
OO000OOCQ0(OOOO0OOCQ0);
end;
procedure OC0C0OOCQ0.O0OO0OOCQ0(const OOOO0OOCQ0:TValueArr;OQOO0OOCQ0,OCOO0OOCQ0:Integer);
begin
OCQ00OOCQ0(OCOO0OOCQ0);
if OCOO0OOCQ0>0 then
OO000OOCQ0(OOOO0OOCQ0,OQOO0OOCQ0,OCOO0OOCQ0);
end;
procedure OC0C0OOCQ0.O0QO0OOCQ0(const OOQO0OOCQ0:TBytes);
var
OQQO0OOCQ0:Integer;
begin
OQQO0OOCQ0:=Length(OOQO0OOCQ0);
if OQQO0OOCQ0>0 then begin
if OOQO0OOCQ0[0]>=$80 then begin
OCQ00OOCQ0(OQQO0OOCQ0+1);
OCO00OOCQ0(0);
end
else
OCQ00OOCQ0(OQQO0OOCQ0);
OO000OOCQ0(OOQO0OOCQ0);
end
else
OCQ00OOCQ0(0);
end;
procedure OC0C0OOCQ0.O0QO0OOCQ0(const OOQO0OOCQ0:TBytes;OCQO0OOCQ0,O0CO0OOCQ0:Integer);
var
OQQO0OOCQ0:Integer;
begin
OQQO0OOCQ0:=O0CO0OOCQ0;
if(O0CO0OOCQ0>0)and(OOQO0OOCQ0[OCQO0OOCQ0]>=$80)then begin
OCQ00OOCQ0(OQQO0OOCQ0+1);
OCO00OOCQ0(0);
end
else
OCQ00OOCQ0(OQQO0OOCQ0);
if O0CO0OOCQ0>0 then
OO000OOCQ0(TValueArr(OOQO0OOCQ0),OCQO0OOCQ0,O0CO0OOCQ0);
end;
function OCCO0OOCQ0.O00QCOOCQ0:TBigInteger;
var
OO0QCOOCQ0:Integer;
begin
OO0QCOOCQ0:=OC0Q0OOCQ0;
if OO0QCOOCQ0<0 then
raise EScError.Create(seUnexpectedEOP);
Result:=TBigInteger.Create(OCQQ0OOCQ0((OO0QCOOCQ0+7)shr 3));
end;
function OCCO0OOCQ0.OQ0QCOOCQ0:OQOOOOOCQ0;
begin
Result:=OQOOOOOCQ0(OO0Q0OOCQ0);
end;
procedure OC0QCOOCQ0.O0OQCOOCQ0(OOOQCOOCQ0:TBigInteger);
begin
OCQ00OOCQ0(OOOQCOOCQ0.BitCount);
OO000OOCQ0(OOOQCOOCQ0.GetBytes);
end;
procedure OC0QCOOCQ0.OQOQCOOCQ0(OCOQCOOCQ0:OQOOOOOCQ0);
begin
OCO00OOCQ0(OCOQCOOCQ0);
end;
end.
