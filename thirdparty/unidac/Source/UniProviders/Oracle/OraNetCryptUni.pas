//////////////////////////////////////////////////
//  Oracle Data Access Components Net
//  Copyright (c) 1998-2019 Devart. All right reserved.
//  OraNetCrypt (OCI Lite)
//////////////////////////////////////////////////
{$I Odac.inc}
unit OraNetCryptUni;
interface
uses
{$IFDEF UNIX}
cwstring,
{$ENDIF}
Classes,Types,SysUtils,
CRTypes,CRBigInteger,CRCipher,CRHMAC;
type
OC0OOCCOQ0=array of ShortInt;
O0OOOCCOQ0=array of array of Byte;
OOOOOCCOQ0=class
private
OQOOOCCOQ0:TBytes;
OCOOOCCOQ0:TBytes;
O0QOOCCOQ0:Word;
OOQOOCCOQ0:Word;
OQQOOCCOQ0:TBigInteger;
OCQOOCCOQ0:TBigInteger;
class function O0COOCCOQ0:O0OOOCCOQ0;
class function OCCOOCCOQ0:O0OOOCCOQ0;
protected
procedure OQ0Q0CCOQ0(const OC0Q0CCOQ0,O0OQ0CCOQ0:TBytes;OOOQ0CCOQ0,OQOQ0CCOQ0:Word;OCOQ0CCOQ0:Integer);
public
constructor Create(const OQCQ0CCOQ0,OCCQ0CCOQ0:TBytes;O00C0CCOQ0,OO0C0CCOQ0:Word);
destructor Destroy;override;
function OC0C0CCOQ0:TBytes;
function O0QC0CCOQ0(const OOQC0CCOQ0:TBytes;OQQC0CCOQ0:Integer):TBytes;
end;
OOCC0CCOQ0=class(TCipher_1DES)
private
OQCC0CCOQ0:Integer;
protected
procedure Init(const O0000CCOQ0:TBytes;const OO000CCOQ0:PCardinal);override;
public
constructor Create(OC000CCOQ0:Integer);reintroduce;
function O0O00CCOQ0(OOO00CCOQ0:IntPtr;OQO00CCOQ0:Integer):TBytes;
function O0Q00CCOQ0(OOQ00CCOQ0:IntPtr;OQQ00CCOQ0:Integer):TBytes;
end;
O0C00CCOQ0=class(TCipher_3DES)
private
OOC00CCOQ0:Integer;
OQC00CCOQ0:Byte;
protected
procedure Decode(O00O0CCOQ0:PCardinal;OO0O0CCOQ0:Integer;OQ0O0CCOQ0:Integer=0);override;
procedure Encode(O0OO0CCOQ0:PCardinal;OOOO0CCOQ0:Integer;OQOO0CCOQ0:Integer=0);override;
procedure Init(const O0000CCOQ0:TBytes;const OO000CCOQ0:PCardinal);override;
public
constructor Create(OC000CCOQ0:Integer);reintroduce;
procedure OOQO0CCOQ0;
function OCQO0CCOQ0(O0CO0CCOQ0:IntPtr;OOCO0CCOQ0:Integer):Integer;
function OCCO0CCOQ0(O00QCCCOQ0:IntPtr;OO0QCCCOQ0:Integer):Integer;
end;
OC0QCCCOQ0=class(TCipher_RC4)
private
O0OQCCCOQ0:Integer;
OOOQCCCOQ0:TBytes;
OQOQCCCOQ0:Boolean;
OCOQCCCOQ0:TCipher_RC4;
O0QQCCCOQ0:TCipher_RC4;
protected
procedure Init(const O0000CCOQ0:TBytes;const OO000CCOQ0:PCardinal);override;
public
constructor Create(OC000CCOQ0:Integer;const O0CQCCCOQ0:TBytes;OOCQCCCOQ0:Boolean);reintroduce;
destructor Destroy;override;
procedure OCCQCCCOQ0;
function OC0CCCCOQ0(O0CO0CCOQ0:IntPtr;OOCO0CCOQ0:Integer):Integer;
function O0OCCCCOQ0(O00QCCCOQ0:IntPtr;OO0QCCCOQ0:Integer):Integer;
function OOOCCCCOQ0(OQOCCCCOQ0:Integer):TBytes;
function OCOCCCCOQ0(O0QCCCCOQ0:Integer):TBytes;
end;
OOQCCCCOQ0=class(TCipher_Rijndael)
private
OQQCCCCOQ0:Integer;
OCQCCCCOQ0:TBytes;
O0CCCCCOQ0:Boolean;
OOCCCCCOQ0:TCipher_Rijndael;
OQCCCCCOQ0:TCipher_Rijndael;
OCCCCCCOQ0:TBytes;
O000CCCOQ0:TBytes;
OO00CCCOQ0:TBytes;
protected
procedure Init(const O0000CCOQ0:TBytes;const OO000CCOQ0:PCardinal);override;
public
constructor Create(OC000CCOQ0:Integer;const O0CQCCCOQ0:TBytes;OOCQCCCOQ0:Boolean);reintroduce;
destructor Destroy;override;
procedure OCO0CCCOQ0;
function OQQ0CCCOQ0(O0CO0CCOQ0:IntPtr;OOCO0CCOQ0:Integer):Integer;
function OCQ0CCCOQ0(O00QCCCOQ0:IntPtr;OO0QCCCOQ0:Integer):Integer;
function O0C0CCCOQ0(OOC0CCCOQ0:IntPtr;OQC0CCCOQ0:Integer):TBytes;
function O00OCCCOQ0(OO0OCCCOQ0:IntPtr;OQ0OCCCOQ0:Integer):TBytes;
function OQOOCCCOQ0(OQOCCCCOQ0:Integer):TBytes;
function OCOOCCCOQ0(O0QCCCCOQ0:Integer):TBytes;
end;
O0QOCCCOQ0=class
private
OOQOCCCOQ0:THMAC;
OQQOCCCOQ0:TBytes;
OCQOCCCOQ0:Integer;
O0COCCCOQ0:Integer;
OOCOCCCOQ0:TBytes;
OQCOCCCOQ0:Cardinal;
OCCOCCCOQ0:Integer;
O00QQCCOQ0:Integer;
protected
function OO0QQCCOQ0(OQ0QQCCOQ0:Cardinal):TBytes;
function OC0QQCCOQ0:TBytes;
public
constructor Create(const OOQQQCCOQ0:Tbytes;OQQQQCCOQ0:TBytes;OCQQQCCOQ0:Integer);
destructor Destroy;override;
function OOCQQCCOQ0(OQCQQCCOQ0:Integer):TBytes;
end;
function O0OCQCCOQ0(OOOCQCCOQ0:Integer):TBytes;
function OQOCQCCOQ0:Byte;
implementation
uses
CRFunctions,CRSymmetricAlgorithm,CRHash,CRRNG,MemUtils;
var
OCOCQCCOQ0:TScRandomLFSR;
function O0OCQCCOQ0(OOOCQCCOQ0:Integer):TBytes;
begin
if OCOCQCCOQ0=nil then
OCOCQCCOQ0:=TScRandomLFSR.Create;
SetLength(Result,OOOCQCCOQ0);
OCOCQCCOQ0.Random(@Result[0],OOOCQCCOQ0);
end;
function OQOCQCCOQ0:Byte;
begin
if OCOCQCCOQ0=nil then
OCOCQCCOQ0:=TScRandomLFSR.Create;
OCOCQCCOQ0.Random(@Result,1);
end;
constructor OOOOOCCOQ0.Create(const OQCQ0CCOQ0,OCCQ0CCOQ0:TBytes;O00C0CCOQ0,OO0C0CCOQ0:Word);
begin
inherited Create;
if(Length(OQCQ0CCOQ0)<>0)and(Length(OCCQ0CCOQ0)<>0)then begin
OQOOOCCOQ0:=OQCQ0CCOQ0;
OCOOOCCOQ0:=OCCQ0CCOQ0;
OOQOOCCOQ0:=OO0C0CCOQ0;
O0QOOCCOQ0:=O00C0CCOQ0;
end
else
OQ0Q0CCOQ0(OQCQ0CCOQ0,OCCQ0CCOQ0,Length(OQCQ0CCOQ0),Length(OCCQ0CCOQ0),40);
end;
destructor OOOOOCCOQ0.Destroy;
begin
OQQOOCCOQ0.Free;
OCQOOCCOQ0.Free;
inherited;
end;
class function OOOOOCCOQ0.O0COOCCOQ0:O0OOOCCOQ0;
const
OOCOOCCOQ0:array[0..37]of Byte=(
2,83,179,242,166,141,61,187,
106,195,153,9,192,215,4,5,
242,91,130,97,107,122,232,220,
29,123,3,150,53,226,219,239,
67,102,250,208,76,193
);
OQCOOCCOQ0:array[0..63]of Byte=(
130,152,222,73,222,247,9,229,
224,13,176,160,165,156,169,242,
61,246,198,167,233,74,68,163,
225,135,46,245,76,31,161,122,
223,92,242,117,129,237,81,195,
38,238,139,225,4,3,30,103,
80,83,181,124,75,69,111,21,
74,23,86,11,90,21,149,165
);
begin
SetLength(Result,5);
SetLength(Result[0],High(OOCOOCCOQ0));
Move(OOCOOCCOQ0[0],Result[0][0],High(OOCOOCCOQ0));
SetLength(Result[1],High(OQCOOCCOQ0));
Move(OQCOOCCOQ0[0],Result[1][0],High(OQCOOCCOQ0));
SetLength(Result[2],High(OQCOOCCOQ0));
Move(OQCOOCCOQ0[0],Result[2][0],High(OQCOOCCOQ0));
SetLength(Result[3],High(OQCOOCCOQ0));
Move(OQCOOCCOQ0[0],Result[3][0],High(OQCOOCCOQ0));
SetLength(Result[4],High(OQCOOCCOQ0));
Move(OQCOOCCOQ0[0],Result[4][0],High(OQCOOCCOQ0));
end;
class function OOOOOCCOQ0.OCCOOCCOQ0:O0OOOCCOQ0;
const
O00Q0CCOQ0:array[0..37]of Byte=(
12,54,129,183,4,71,3,160,
120,96,81,38,140,234,155,188,
163,62,124,1,171,54,139,34,
117,152,119,102,53,197,128,213,
36,210,80,99,184,243
);
OO0Q0CCOQ0:array[0..63]of Byte=(
220,142,163,27,8,96,105,138,
204,246,209,158,135,14,52,252,
103,197,89,11,78,166,177,60,
213,253,239,21,172,157,95,63,
33,76,220,7,204,135,74,179,
1,215,127,44,67,51,81,60,
222,11,30,206,100,71,118,87,
92,81,204,152,179,254,231,239
);
begin
SetLength(Result,5);
SetLength(Result[0],High(O00Q0CCOQ0));
Move(O00Q0CCOQ0[0],Result[0][0],High(O00Q0CCOQ0));
SetLength(Result[1],High(OO0Q0CCOQ0));
Move(OO0Q0CCOQ0[0],Result[1][0],High(OO0Q0CCOQ0));
SetLength(Result[2],High(OO0Q0CCOQ0));
Move(OO0Q0CCOQ0[0],Result[2][0],High(OO0Q0CCOQ0));
SetLength(Result[3],High(OO0Q0CCOQ0));
Move(OO0Q0CCOQ0[0],Result[3][0],High(OO0Q0CCOQ0));
SetLength(Result[4],High(OO0Q0CCOQ0));
Move(OO0Q0CCOQ0[0],Result[4][0],High(OO0Q0CCOQ0));
end;
procedure OOOOOCCOQ0.OQ0Q0CCOQ0(const OC0Q0CCOQ0,O0OQ0CCOQ0:TBytes;OOOQ0CCOQ0,OQOQ0CCOQ0:Word;OCOQ0CCOQ0:Integer);
const
O0QQ0CCOQ0:array[0..4]of Word=(
40,41,56,128,256
);
OOQQ0CCOQ0:array[0..4]of Word=(
40,64,56,128,256
);
OQQQ0CCOQ0:array[0..4]of Word=(
80,112,112,512,512
);
OCQQ0CCOQ0:array[0..4]of Word=(
300,512,512,512,512
);
var
O0CQ0CCOQ0:Integer;
begin
for O0CQ0CCOQ0:=0 to High(O0QQ0CCOQ0)do
if(OCOQ0CCOQ0>=O0QQ0CCOQ0[O0CQ0CCOQ0])and(OCOQ0CCOQ0<=OOQQ0CCOQ0[O0CQ0CCOQ0])then begin
O0QOOCCOQ0:=OQQQ0CCOQ0[O0CQ0CCOQ0];
OOQOOCCOQ0:=OCQQ0CCOQ0[O0CQ0CCOQ0];
SetLength(OQOOOCCOQ0,(OOQOOCCOQ0+7)div 8);
SetLength(OCOOOCCOQ0,(OOQOOCCOQ0+7)div 8);
if(OOOQ0CCOQ0*8>=OOQOOCCOQ0)and(OQOQ0CCOQ0*8>=OOQOOCCOQ0)then begin
Move(OC0Q0CCOQ0[0],OQOOOCCOQ0[0],Length(OQOOOCCOQ0));
Move(O0OQ0CCOQ0[0],OCOOOCCOQ0[0],Length(OCOOOCCOQ0));
end
else begin
Move(O0COOCCOQ0[O0CQ0CCOQ0][0],OQOOOCCOQ0[0],Length(OQOOOCCOQ0));
Move(OCCOOCCOQ0[O0CQ0CCOQ0][0],OCOOOCCOQ0[0],Length(OCOOOCCOQ0));
end;
break;
end;
end;
function OOOOOCCOQ0.OC0C0CCOQ0:TBytes;
var
O0OC0CCOQ0:TBigInteger;
OOOC0CCOQ0:TBigInteger;
OQOC0CCOQ0:TBytes;
OCOC0CCOQ0:Integer;
begin
OCOC0CCOQ0:=(O0QOOCCOQ0+7)shr 3;
OQOC0CCOQ0:=O0OCQCCOQ0(OCOC0CCOQ0);
OQOC0CCOQ0[0]:=OQOC0CCOQ0[0]and(255 shr OCOC0CCOQ0-8*O0QOOCCOQ0);
OCQOOCCOQ0:=TBigInteger.Create(OQOC0CCOQ0);
OQQOOCCOQ0:=TBigInteger.Create(OCOOOCCOQ0);
O0OC0CCOQ0:=TBigInteger.Create(OQOOOCCOQ0);
try
OOOC0CCOQ0:=O0OC0CCOQ0.ModPow(OCQOOCCOQ0,OQQOOCCOQ0);
try
Result:=OOOC0CCOQ0.GetBytes(64);
finally
OOOC0CCOQ0.Free;
end;
finally
O0OC0CCOQ0.Free;
end;
end;
function OOOOOCCOQ0.O0QC0CCOQ0(const OOQC0CCOQ0:TBytes;OQQC0CCOQ0:Integer):TBytes;
var
OCQC0CCOQ0:TBigInteger;
O0CC0CCOQ0:TBigInteger;
begin
OCQC0CCOQ0:=TBigInteger.Create(OOQC0CCOQ0);
try
O0CC0CCOQ0:=OCQC0CCOQ0.ModPow(OCQOOCCOQ0,OQQOOCCOQ0);
try
Result:=O0CC0CCOQ0.GetBytes(OQQC0CCOQ0);
finally
O0CC0CCOQ0.Free;
end;
finally
OCQC0CCOQ0.Free;
end;
end;
constructor OOCC0CCOQ0.Create(OC000CCOQ0:Integer);
begin
inherited Create;
OQCC0CCOQ0:=OC000CCOQ0;
end;
procedure OOCC0CCOQ0.Init(const O0000CCOQ0:TBytes;const OO000CCOQ0:PCardinal);
begin
if OQCC0CCOQ0>0 then begin
inherited Init(copy(O0000CCOQ0,0,OQCC0CCOQ0),OO000CCOQ0);
FKeySize:=OQCC0CCOQ0;
end
else
inherited Init(O0000CCOQ0,OO000CCOQ0);
end;
function OOCC0CCOQ0.O0O00CCOQ0(OOO00CCOQ0:IntPtr;OQO00CCOQ0:Integer):TBytes;
var
OCO00CCOQ0:TBytes;
begin
SetLength(OCO00CCOQ0,OQO00CCOQ0);
DecodeBuffer(OOO00CCOQ0,@OCO00CCOQ0[0],OQO00CCOQ0);
SetLength(Result,8);
Move(OCO00CCOQ0[Length(OCO00CCOQ0)-8],Result[0],8);
end;
function OOCC0CCOQ0.O0Q00CCOQ0(OOQ00CCOQ0:IntPtr;OQQ00CCOQ0:Integer):TBytes;
var
OCQ00CCOQ0:TBytes;
begin
SetLength(OCQ00CCOQ0,OQQ00CCOQ0);
EncodeBuffer(OOQ00CCOQ0,@OCQ00CCOQ0[0],OQQ00CCOQ0);
SetLength(Result,8);
Move(OCQ00CCOQ0[Length(OCQ00CCOQ0)-8],Result[0],8);
end;
constructor O0C00CCOQ0.Create(OC000CCOQ0:Integer);
begin
inherited Create;
OOC00CCOQ0:=OC000CCOQ0;
end;
procedure O0C00CCOQ0.Decode(O00O0CCOQ0:PCardinal;OO0O0CCOQ0,OQ0O0CCOQ0:Integer);
begin
if OQC00CCOQ0=2 then
inherited Encode(O00O0CCOQ0,OO0O0CCOQ0,OQ0O0CCOQ0)
else
inherited Decode(O00O0CCOQ0,OO0O0CCOQ0,OQ0O0CCOQ0);
end;
procedure O0C00CCOQ0.Encode(O0OO0CCOQ0:PCardinal;OOOO0CCOQ0:Integer;OQOO0CCOQ0:Integer=0);
begin
if OQC00CCOQ0=1 then
inherited Decode(O0OO0CCOQ0,OOOO0CCOQ0,OQOO0CCOQ0)
else
inherited Encode(O0OO0CCOQ0,OOOO0CCOQ0,OQOO0CCOQ0);
end;
procedure O0C00CCOQ0.Init(const O0000CCOQ0:TBytes;const OO000CCOQ0:PCardinal);
begin
if OOC00CCOQ0>0 then begin
inherited Init(copy(O0000CCOQ0,0,OOC00CCOQ0),OO000CCOQ0);
FKeySize:=OOC00CCOQ0;
end
else
inherited Init(O0000CCOQ0,OO000CCOQ0);
OOQO0CCOQ0;
end;
procedure O0C00CCOQ0.OOQO0CCOQ0;
const
OQQO0CCOQ0:array[0..7]of Byte=(
1,35,69,103,137,171,205,239
);
begin
IV:=DynArrayCreate(OQQO0CCOQ0);
OQC00CCOQ0:=0;
end;
function O0C00CCOQ0.OCQO0CCOQ0(O0CO0CCOQ0:IntPtr;OOCO0CCOQ0:Integer):Integer;
var
OQCO0CCOQ0:Byte;
begin
Result:=OOCO0CCOQ0-1;
OQCO0CCOQ0:=PByteArray(O0CO0CCOQ0)[Result];
if OQCO0CCOQ0>8 then begin
Result:=-1;
Exit;
end;
if OQC00CCOQ0=0 then
OQC00CCOQ0:=1;
DecodeBuffer(O0CO0CCOQ0,O0CO0CCOQ0,Result);
Result:=Result-OQCO0CCOQ0+1;
end;
function O0C00CCOQ0.OCCO0CCOQ0(O00QCCCOQ0:IntPtr;OO0QCCCOQ0:Integer):Integer;
var
OQ0QCCCOQ0:Byte;
begin
OQ0QCCCOQ0:=Byte(OO0QCCCOQ0)and$07;
if OQ0QCCCOQ0>0 then begin
OQ0QCCCOQ0:=8-OQ0QCCCOQ0;
FillChar(@PByteArray(O00QCCCOQ0)[OO0QCCCOQ0],OQ0QCCCOQ0,0);
Result:=OO0QCCCOQ0+OQ0QCCCOQ0;
end
else
Result:=OO0QCCCOQ0;
if OQC00CCOQ0=0 then
OQC00CCOQ0:=2;
EncodeBuffer(O00QCCCOQ0,O00QCCCOQ0,Result);
PByteArray(O00QCCCOQ0)[Result]:=OQ0QCCCOQ0+1;
Inc(Result);
end;
constructor OC0QCCCOQ0.Create(OC000CCOQ0:Integer;const O0CQCCCOQ0:TBytes;OOCQCCCOQ0:Boolean);
begin
inherited Create;
O0OQCCCOQ0:=OC000CCOQ0;
OOOQCCCOQ0:=O0CQCCCOQ0;
OQOQCCCOQ0:=OOCQCCCOQ0;
Mode:=cmECB;
OCOQCCCOQ0:=TCipher_RC4.Create;
OCOQCCCOQ0.Mode:=cmECB;
O0QQCCCOQ0:=TCipher_RC4.Create;
O0QQCCCOQ0.Mode:=cmECB;
end;
destructor OC0QCCCOQ0.Destroy;
begin
OCOQCCCOQ0.Free;
O0QQCCCOQ0.Free;
inherited;
end;
procedure OC0QCCCOQ0.Init(const O0000CCOQ0:TBytes;const OO000CCOQ0:PCardinal);
var
OQQQCCCOQ0:TBytes;
begin
SetLength(OQQQCCCOQ0,O0OQCCCOQ0+1+Length(OOOQCCCOQ0));
if O0OQCCCOQ0>0 then
Move(O0000CCOQ0[Length(O0000CCOQ0)-O0OQCCCOQ0],OQQQCCCOQ0[0],O0OQCCCOQ0);
if Length(OOOQCCCOQ0)>0 then
Move(OOOQCCCOQ0[0],OQQQCCCOQ0[O0OQCCCOQ0+1],Length(OOOQCCCOQ0));
if OQOQCCCOQ0 then
OQQQCCCOQ0[O0OQCCCOQ0]:=255
else
OQQQCCCOQ0[O0OQCCCOQ0]:=123;
OCOQCCCOQ0.Key:=OQQQCCCOQ0;
OCCQCCCOQ0;
end;
procedure OC0QCCCOQ0.OCCQCCCOQ0;
const
O00CCCCOQ0:array[0..35]of Byte=(
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
32,32,32,32,32,32
);
var
OO0CCCCOQ0:TBytes;
OQ0CCCCOQ0:TBytes;
begin
if OQOQCCCOQ0 then begin
SetLength(OO0CCCCOQ0,FBlockSize);
SetLength(OQ0CCCCOQ0,O0OQCCCOQ0+1);
OCOQCCCOQ0.EncodeBuffer(@O00CCCCOQ0[0],@OQ0CCCCOQ0[0],O0OQCCCOQ0);
OQ0CCCCOQ0[O0OQCCCOQ0]:=90;
inherited Init(OQ0CCCCOQ0,@OO0CCCCOQ0[0]);
FKeySize:=O0OQCCCOQ0+1;
OQ0CCCCOQ0[O0OQCCCOQ0]:=180;
O0QQCCCOQ0.Key:=OQ0CCCCOQ0;
end
else begin
SetLength(OO0CCCCOQ0,FBlockSize);
SetLength(OQ0CCCCOQ0,O0OQCCCOQ0);
OCOQCCCOQ0.EncodeBuffer(@O00CCCCOQ0[0],@OQ0CCCCOQ0[0],O0OQCCCOQ0);
inherited Init(OQ0CCCCOQ0,@OO0CCCCOQ0[0]);
FKeySize:=O0OQCCCOQ0;
OQ0CCCCOQ0[O0OQCCCOQ0-1]:=OQ0CCCCOQ0[O0OQCCCOQ0-1]xor 170;
O0QQCCCOQ0.Key:=OQ0CCCCOQ0;
end;
end;
function OC0QCCCOQ0.OC0CCCCOQ0(O0CO0CCOQ0:IntPtr;OOCO0CCOQ0:Integer):Integer;
var
OQCO0CCOQ0:Byte;
begin
Result:=OOCO0CCOQ0-1;
OQCO0CCOQ0:=PByteArray(O0CO0CCOQ0)[Result];
if OQCO0CCOQ0<>0 then begin
Result:=-1;
Exit;
end;
O0QQCCCOQ0.DecodeBuffer(O0CO0CCOQ0,O0CO0CCOQ0,Result);
Result:=Result-OQCO0CCOQ0;
end;
function OC0QCCCOQ0.O0OCCCCOQ0(O00QCCCOQ0:IntPtr;OO0QCCCOQ0:Integer):Integer;
begin
EncodeBuffer(O00QCCCOQ0,O00QCCCOQ0,OO0QCCCOQ0);
PByteArray(O00QCCCOQ0)[OO0QCCCOQ0]:=0;
Result:=OO0QCCCOQ0+1;
end;
function OC0QCCCOQ0.OOOCCCCOQ0(OQOCCCCOQ0:Integer):TBytes;
begin
SetLength(Result,OQOCCCCOQ0);
O0QQCCCOQ0.DecodeBuffer(@Result[0],@Result[0],OQOCCCCOQ0);
end;
function OC0QCCCOQ0.OCOCCCCOQ0(O0QCCCCOQ0:Integer):TBytes;
begin
SetLength(Result,O0QCCCCOQ0);
EncodeBuffer(@Result[0],@Result[0],O0QCCCCOQ0);
end;
constructor OOQCCCCOQ0.Create(OC000CCOQ0:Integer;const O0CQCCCOQ0:TBytes;OOCQCCCOQ0:Boolean);
begin
inherited Create;
OQQCCCCOQ0:=OC000CCOQ0;
OCQCCCCOQ0:=O0CQCCCOQ0;
O0CCCCCOQ0:=OOCQCCCOQ0;
if OOCQCCCOQ0 then begin
OOCCCCCOQ0:=TCipher_Rijndael.Create;
OQCCCCCOQ0:=TCipher_Rijndael.Create;
end
else
OQCCCCCOQ0:=nil;
end;
destructor OOQCCCCOQ0.Destroy;
begin
OOCCCCCOQ0.Free;
OQCCCCCOQ0.Free;
inherited;
end;
procedure OOQCCCCOQ0.Init(const O0000CCOQ0:TBytes;const OO000CCOQ0:PCardinal);
var
OC00CCCOQ0:TBytes;
O0O0CCCOQ0:TBytes;
begin
if O0CCCCCOQ0 then begin
SetLength(OC00CCCOQ0,16);
Move(O0000CCOQ0[0],OC00CCCOQ0[0],5);
OC00CCCOQ0[5]:=255;
SetLength(O0O0CCCOQ0,16);
Move(OCQCCCCOQ0[0],O0O0CCCOQ0[0],16);
SetLength(OCCCCCCOQ0,32);
FillChar(@OCCCCCCOQ0[0],32,0);
OOCCCCCOQ0.Key:=OC00CCCOQ0;
OOCCCCCOQ0.IV:=O0O0CCCOQ0;
SetLength(O000CCCOQ0,0);
SetLength(OO00CCCOQ0,0);
OCO0CCCOQ0;
end
else begin
if OQQCCCCOQ0>=0 then
OC00CCCOQ0:=copy(O0000CCOQ0,0,OQQCCCCOQ0)
else
OC00CCCOQ0:=O0000CCOQ0;
inherited Init(OC00CCCOQ0,OO000CCOQ0);
end;
end;
procedure OOQCCCCOQ0.OCO0CCCOQ0;
var
O0Q0CCCOQ0:TBytes;
OOQ0CCCOQ0:TBytes;
begin
if O0CCCCCOQ0 then begin
OOCCCCCOQ0.EncodeBuffer(@OCCCCCCOQ0[0],@OCCCCCCOQ0[0],32);
SetLength(O0Q0CCCOQ0,16);
Move(OCCCCCCOQ0[0],O0Q0CCCOQ0[0],16);
SetLength(OOQ0CCCOQ0,16);
Move(OCCCCCCOQ0[16],OOQ0CCCOQ0[0],16);
OOCCCCCOQ0.Key:=O0Q0CCCOQ0;
OOCCCCCOQ0.IV:=OOQ0CCCOQ0;
O0Q0CCCOQ0[5]:=180;
OQCCCCCOQ0.Key:=O0Q0CCCOQ0;
OQCCCCCOQ0.IV:=OOQ0CCCOQ0;
O0Q0CCCOQ0[5]:=90;
inherited Init(O0Q0CCCOQ0,@OOQ0CCCOQ0[0]);
end;
end;
function OOQCCCCOQ0.OQQ0CCCOQ0(O0CO0CCOQ0:IntPtr;OOCO0CCOQ0:Integer):Integer;
var
OQCO0CCOQ0:Byte;
begin
Result:=OOCO0CCOQ0-1;
OQCO0CCOQ0:=PByteArray(O0CO0CCOQ0)[Result];
if OQCO0CCOQ0>16 then begin
Result:=-1;
Exit;
end;
ClearIV;
DecodeBuffer(O0CO0CCOQ0,O0CO0CCOQ0,Result);
Result:=Result-OQCO0CCOQ0+1;
end;
function OOQCCCCOQ0.OCQ0CCCOQ0(O00QCCCOQ0:IntPtr;OO0QCCCOQ0:Integer):Integer;
var
OQ0QCCCOQ0:Byte;
begin
OQ0QCCCOQ0:=Byte(OO0QCCCOQ0)and$0F;
if OQ0QCCCOQ0>0 then begin
OQ0QCCCOQ0:=16-OQ0QCCCOQ0;
FillChar(@PByteArray(O00QCCCOQ0)[OO0QCCCOQ0],OQ0QCCCOQ0,0);
Result:=OO0QCCCOQ0+OQ0QCCCOQ0;
end
else
Result:=OO0QCCCOQ0;
ClearIV;
EncodeBuffer(O00QCCCOQ0,O00QCCCOQ0,Result);
PByteArray(O00QCCCOQ0)[Result]:=OQ0QCCCOQ0+1;
Inc(Result);
end;
function OOQCCCCOQ0.O0C0CCCOQ0(OOC0CCCOQ0:IntPtr;OQC0CCCOQ0:Integer):TBytes;
var
OCC0CCCOQ0:Integer;
begin
SetLength(Result,OQC0CCCOQ0);
if OQC0CCCOQ0 and$0000000F<>0 then
Exit;
DecodeBuffer(OOC0CCCOQ0,@Result[0],OQC0CCCOQ0);
OCC0CCCOQ0:=Result[OQC0CCCOQ0-1];
if OCC0CCCOQ0<=16 then
SetLength(Result,Length(Result)-OCC0CCCOQ0)
else
SetLength(Result,0);
end;
function OOQCCCCOQ0.O00OCCCOQ0(OO0OCCCOQ0:IntPtr;OQ0OCCCOQ0:Integer):TBytes;
var
OC0OCCCOQ0:Integer;
O0OOCCCOQ0:Integer;
OOOOCCCOQ0:Integer;
begin
O0OOCCCOQ0:=16-(OQ0OCCCOQ0 mod 16);
OOOOCCCOQ0:=OQ0OCCCOQ0+O0OOCCCOQ0;
SetLength(Result,OOOOCCCOQ0);
Move(OO0OCCCOQ0^,Result[0],OQ0OCCCOQ0);
for OC0OCCCOQ0:=OQ0OCCCOQ0 to Length(Result)-1 do
Result[OC0OCCCOQ0]:=O0OOCCCOQ0;
EncodeBuffer(@Result[0],@Result[0],OOOOCCCOQ0);
end;
function OOQCCCCOQ0.OQOOCCCOQ0(OQOCCCCOQ0:Integer):TBytes;
begin
if Length(O000CCCOQ0)<>OQOCCCCOQ0 then
SetLength(O000CCCOQ0,OQOCCCCOQ0);
OQCCCCCOQ0.EncodeBuffer(@O000CCCOQ0[0],@O000CCCOQ0[0],OQOCCCCOQ0);
Result:=O000CCCOQ0;
end;
function OOQCCCCOQ0.OCOOCCCOQ0(O0QCCCCOQ0:Integer):TBytes;
begin
if Length(OO00CCCOQ0)<>O0QCCCCOQ0 then
SetLength(OO00CCCOQ0,O0QCCCCOQ0);
EncodeBuffer(@OO00CCCOQ0[0],@OO00CCCOQ0[0],O0QCCCCOQ0);
Result:=OO00CCCOQ0;
end;
constructor O0QOCCCOQ0.Create(const OOQQQCCOQ0:Tbytes;OQQQQCCOQ0:TBytes;OCQQQCCOQ0:Integer);
begin
inherited Create;
OOQOCCCOQ0:=THMAC.Create(THash_SHA2_512,OOQQQCCOQ0);
OQQOCCCOQ0:=OQQQQCCOQ0;
OCQOCCCOQ0:=OCQQQCCOQ0;
O0COCCCOQ0:=OOQOCCCOQ0.HashSize;
SetLength(OOCOCCCOQ0,O0COCCCOQ0);
OQCOCCCOQ0:=1;
OCCOCCCOQ0:=0;
O00QQCCOQ0:=0;
end;
destructor O0QOCCCOQ0.Destroy;
begin
OOQOCCCOQ0.Free;
inherited;
end;
function O0QOCCCOQ0.OO0QQCCOQ0(OQ0QQCCOQ0:Cardinal):TBytes;
begin
SetLength(Result,4);
Result[3]:=byte(OQ0QQCCOQ0);
Result[2]:=byte(OQ0QQCCOQ0 shr 8);
Result[1]:=byte(OQ0QQCCOQ0 shr 16);
Result[0]:=byte(OQ0QQCCOQ0 shr 24);
end;
function O0QOCCCOQ0.OC0QQCCOQ0:TBytes;
var
O0OQQCCOQ0,OOOQQCCOQ0:Integer;
OQOQQCCOQ0:TBytes;
OCOQQCCOQ0:TBytes;
begin
OQOQQCCOQ0:=OO0QQCCOQ0(OQCOCCCOQ0);
OOQOCCCOQ0.TransformBlock(OQQOCCCOQ0,0,Length(OQQOCCCOQ0));
OOQOCCCOQ0.TransformFinalBlock(OQOQQCCOQ0,0,Length(OQOQQCCOQ0));
OCOQQCCOQ0:=OOQOCCCOQ0.Hash;
OOQOCCCOQ0.Initialize;
Result:=OCOQQCCOQ0;
for O0OQQCCOQ0:=2 to OCQOCCCOQ0 do begin
OCOQQCCOQ0:=OOQOCCCOQ0.ComputeHash(OCOQQCCOQ0);
for OOOQQCCOQ0:=0 to O0COCCCOQ0-1 do
Result[OOOQQCCOQ0]:=Result[OOOQQCCOQ0]xor OCOQQCCOQ0[OOOQQCCOQ0];
end;
Inc(OQCOCCCOQ0);
end;
function O0QOCCCOQ0.OOCQQCCOQ0(OQCQQCCOQ0:Integer):TBytes;
var
OCCQQCCOQ0:Integer;
O00CQCCOQ0:TBytes;
OO0CQCCOQ0:Integer;
OQ0CQCCOQ0:Integer;
OC0CQCCOQ0:Integer;
begin
SetLength(Result,OQCQQCCOQ0);
OCCQQCCOQ0:=0;
OO0CQCCOQ0:=O00QQCCOQ0-OCCOCCCOQ0;
if OO0CQCCOQ0>0 then begin
if OQCQQCCOQ0<OO0CQCCOQ0 then begin
Move(OOCOCCCOQ0[OCCOCCCOQ0],Result[0],OQCQQCCOQ0);
OCCOCCCOQ0:=OCCOCCCOQ0+OQCQQCCOQ0;
Exit;
end;
Move(OOCOCCCOQ0[OCCOCCCOQ0],Result[0],OO0CQCCOQ0);
OCCOCCCOQ0:=0;
O00QQCCOQ0:=0;
OCCQQCCOQ0:=OCCQQCCOQ0+OO0CQCCOQ0;
end;
{$IFNDEF VER9P}
SetLength(O00CQCCOQ0,0);
{$ENDIF}
while OCCQQCCOQ0<OQCQQCCOQ0 do begin
O00CQCCOQ0:=OC0QQCCOQ0;
OQ0CQCCOQ0:=OQCQQCCOQ0-OCCQQCCOQ0;
OC0CQCCOQ0:=O0COCCCOQ0-OQ0CQCCOQ0;
if OC0CQCCOQ0>=0 then begin
Move(O00CQCCOQ0[0],Result[OCCQQCCOQ0],OQ0CQCCOQ0);
if OC0CQCCOQ0>0 then begin
Move(O00CQCCOQ0[OQ0CQCCOQ0],OOCOCCCOQ0[OCCOCCCOQ0],OC0CQCCOQ0);
O00QQCCOQ0:=O00QQCCOQ0+OC0CQCCOQ0;
end;
Exit;
end;
Move(O00CQCCOQ0[0],Result[OCCQQCCOQ0],20);
OCCQQCCOQ0:=OCCQQCCOQ0+O0COCCCOQ0;
end;
end;
initialization
finalization
OCOCQCCOQ0.Free;
end.
