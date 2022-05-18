//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsSSL3HandshakeLayerUni;
interface
uses
SysUtils,
CLRClasses,CRTypes,CRHash,CRHashAlgorithm,CRHMAC,
{$IFNDEF UNIDACPRO}
TdsUtils,TdsSSLTypes,TdsCipherSuites,
TdsLayers,TdsSSLMessages,TdsAlgorithmSupport;
{$ELSE}
TdsUtilsUni,TdsSSLTypesUni,TdsCipherSuitesUni,
TdsLayersUni,TdsSSLMessagesUni,TdsAlgorithmSupportUni;
{$ENDIF}
type
O0C0QQOCQ0=class
private
OOC0QQOCQ0:TBytes;
OQC0QQOCQ0:TBytes;
OCC0QQOCQ0:Integer;
O00OQQOCQ0:TBytes;
OO0OQQOCQ0:THash_MD5;
OQ0OQQOCQ0:THash_SHA1;
OC0OQQOCQ0:Integer;
protected
function O0OOQQOCQ0:TBytes;
public
constructor Create(const O0QOQQOCQ0,OOQOQQOCQ0:TBytes;OQQOQQOCQ0:Boolean);
destructor Destroy;override;
function O0COQQOCQ0(OOCOQQOCQ0:Integer):TBytes;
procedure OCCOQQOCQ0;
end;
O00QOOOCQ0=class(TKeyedHashAlgorithm)
private
OO0QOOOCQ0:THashAlgorithm;
OQ0QOOOCQ0:Boolean;
OC0QOOOCQ0,O0OQOOOCQ0:TBytes;
protected
function Get_HashSize:Integer;override;
procedure HashCore(const OCOQOOOCQ0:TValueArr;O0QQOOOCQ0,OOQQOOOCQ0:Integer);override;
function HashFinal:TBytes;override;
public
constructor Create(OOCQOOOCQ0:OOOOCOQOQ0;const OQCQOOOCQ0:TBytes);reintroduce;
destructor Destroy;override;
procedure Initialize;override;
end;
OQ0COOOCQ0=class(TKeyedHashAlgorithm)
private
OC0COOOCQ0:THashAlgorithm;
O0OCOOOCQ0,OOOCOOOCQ0:TBytes;
protected
function Get_HashSize:Integer;override;
procedure HashCore(const OCOQOOOCQ0:TValueArr;O0QQOOOCQ0,OOQQOOOCQ0:Integer);override;
function HashFinal:TBytes;override;
public
constructor Create(OOCQOOOCQ0:OOOOCOQOQ0;OQQCOOOCQ0:THashAlgorithm;const OQCQOOOCQ0:TBytes);reintroduce;
procedure Initialize;override;
end;
O0CCOOOCQ0=class(OCOO0C00Q0)
public
function O00OCC00Q0(const OOCCOOOCQ0:TCipherDefinition;
const OQCCOOOCQ0:OQQC0Q00Q0):OCQ0O0Q0Q0;override;
function OCCQOQ00Q0:THashAlgorithm;override;
procedure O0COCC00Q0(const OOQ0OOOCQ0,OQQ0OOOCQ0:TBytes;const OCQ0OOOCQ0:array of byte);override;
procedure OOOOQC00Q0(OOC0OOOCQ0:OCQC00Q0Q0);override;
procedure OCOOQC00Q0(const OCC0OOOCQ0:TBytes;O00OOOOCQ0,OO0OOOOCQ0:integer);override;
function O00COQ00Q0:OOCCCQQ0Q0;override;
end;
implementation
uses
CRDECUtil,CRSymmetricAlgorithm,
{$IFNDEF UNIDACPRO}
TdsMD5SHA1CSP,TdsSSLConsts;
{$ELSE}
TdsMD5SHA1CSPUni,TdsSSLConstsUni;
{$ENDIF}
const
O0OOOOOCQ0:array[0..3]of byte=($43,$4C,$4E,$54);
OOOOOOOCQ0:array[0..3]of byte=($53,$52,$56,$52);
constructor O0C0QQOCQ0.Create(const O0QOQQOCQ0,OOQOQQOCQ0:TBytes;OQQOQQOCQ0:Boolean);
begin
inherited Create;
if Length(OOQOQQOCQ0)<>64 then
raise EScError.Create(seInvalidInputArgs);
SetLength(OOC0QQOCQ0,Length(O0QOQQOCQ0));
if Length(O0QOQQOCQ0)>0 then
Buffer.BlockCopy(O0QOQQOCQ0,0,OOC0QQOCQ0,0,Length(O0QOQQOCQ0));
SetLength(O00OQQOCQ0,64);
if OQQOQQOCQ0 then
Buffer.BlockCopy(OOQOQQOCQ0,0,O00OQQOCQ0,0,64)
else begin
Buffer.BlockCopy(OOQOQQOCQ0,0,O00OQQOCQ0,32,32);
Buffer.BlockCopy(OOQOQQOCQ0,32,O00OQQOCQ0,0,32);
end;
OO0OQQOCQ0:=THash_MD5.Create;
OQ0OQQOCQ0:=THash_SHA1.Create;
OCCOQQOCQ0;
end;
destructor O0C0QQOCQ0.Destroy;
begin
OO0OQQOCQ0.Free;
OQ0OQQOCQ0.Free;
FillChar(OOC0QQOCQ0[0],Length(OOC0QQOCQ0),0);
FillChar(OQC0QQOCQ0[0],Length(OQC0QQOCQ0),0);
FillChar(O00OQQOCQ0[0],Length(O00OQQOCQ0),0);
inherited;
end;
function O0C0QQOCQ0.O0OOQQOCQ0:TBytes;
var
OOOOQQOCQ0:TBytes;
OQOOQQOCQ0:Integer;
begin
if OC0OQQOCQ0>26 then
raise EScError.Create(seSsl3PsepdoRandomError);
SetLength(OOOOQQOCQ0,OC0OQQOCQ0);
for OQOOQQOCQ0:=0 to OC0OQQOCQ0-1 do
OOOOQQOCQ0[OQOOQQOCQ0]:=Byte(64+OC0OQQOCQ0);
OQ0OQQOCQ0.TransformBlock(OOOOQQOCQ0,0,Length(OOOOQQOCQ0));
OQ0OQQOCQ0.TransformBlock(OOC0QQOCQ0,0,Length(OOC0QQOCQ0));
OQ0OQQOCQ0.TransformFinalBlock(O00OQQOCQ0,0,Length(O00OQQOCQ0));
OO0OQQOCQ0.TransformBlock(OOC0QQOCQ0,0,Length(OOC0QQOCQ0));
OO0OQQOCQ0.TransformFinalBlock(OQ0OQQOCQ0.Hash,0,20);
Result:=OO0OQQOCQ0.Hash;
OQ0OQQOCQ0.Initialize;
OO0OQQOCQ0.Initialize;
Inc(OC0OQQOCQ0);
end;
function O0C0QQOCQ0.O0COQQOCQ0(OOCOQQOCQ0:Integer):TBytes;
var
OQCOQQOCQ0:Integer;
begin
if OOCOQQOCQ0<0 then
raise EScError.Create(seInvalidInputArgs);
SetLength(Result,OOCOQQOCQ0);
OQCOQQOCQ0:=0;
while OQCOQQOCQ0<Length(Result)do begin
if(OQCOQQOCQ0+Length(OQC0QQOCQ0)-OCC0QQOCQ0)>=OOCOQQOCQ0 then begin
Buffer.BlockCopy(OQC0QQOCQ0,OCC0QQOCQ0,Result,OQCOQQOCQ0,OOCOQQOCQ0-OQCOQQOCQ0);
OCC0QQOCQ0:=OCC0QQOCQ0+OOCOQQOCQ0-OQCOQQOCQ0;
OQCOQQOCQ0:=Length(Result);
end
else begin
Buffer.BlockCopy(OQC0QQOCQ0,OCC0QQOCQ0,Result,OQCOQQOCQ0,Length(OQC0QQOCQ0)-OCC0QQOCQ0);
OQCOQQOCQ0:=OQCOQQOCQ0+Length(OQC0QQOCQ0)-OCC0QQOCQ0;
OQC0QQOCQ0:=O0OOQQOCQ0;
OCC0QQOCQ0:=0;
end;
end;
end;
procedure O0C0QQOCQ0.OCCOQQOCQ0;
begin
OC0OQQOCQ0:=1;
OQC0QQOCQ0:=O0OOQQOCQ0;
OCC0QQOCQ0:=0;
end;
constructor O00QOOOCQ0.Create(OOCQOOOCQ0:OOOOCOQOQ0;const OQCQOOOCQ0:TBytes);
var
OCCQOOOCQ0:Integer;
begin
inherited Create;
if Length(OQCQOOOCQ0)=0 then
raise EScError.Create(seInvalidInputArgs);
if OOCQOOOCQ0=OQ0OCOQOQ0 then begin
OO0QOOOCQ0:=THash_MD5.Create;
OCCQOOOCQ0:=48;
end
else if OOCQOOOCQ0=OOC0COQOQ0 then begin
OO0QOOOCQ0:=THash_SHA1.Create;
OCCQOOOCQ0:=40;
end
else begin
OCCQOOOCQ0:=0;
Assert(False);
end;
SetLength(OC0QOOOCQ0,OCCQOOOCQ0);
SetLength(O0OQOOOCQ0,OCCQOOOCQ0);
FillChar(OC0QOOOCQ0[0],OCCQOOOCQ0,$36);
FillChar(O0OQOOOCQ0[0],OCCQOOOCQ0,$5C);
SetLength(FKeyValue,Length(OQCQOOOCQ0));
Buffer.BlockCopy(OQCQOOOCQ0,0,FKeyValue,0,Length(OQCQOOOCQ0));
Initialize;
end;
destructor O00QOOOCQ0.Destroy;
begin
OO0QOOOCQ0.Free;
inherited;
end;
procedure O00QOOOCQ0.Initialize;
begin
OO0QOOOCQ0.Initialize;
OQ0QOOOCQ0:=False;
FState:=0;
end;
procedure O00QOOOCQ0.HashCore(const OCOQOOOCQ0:TValueArr;O0QQOOOCQ0,OOQQOOOCQ0:Integer);
begin
if not OQ0QOOOCQ0 then begin
OO0QOOOCQ0.TransformBlock(FKeyValue,0,Length(FKeyValue));
OO0QOOOCQ0.TransformBlock(OC0QOOOCQ0,0,Length(OC0QOOOCQ0));
OQ0QOOOCQ0:=True;
end;
OO0QOOOCQ0.TransformBlock(OCOQOOOCQ0,O0QQOOOCQ0,OOQQOOOCQ0,OCOQOOOCQ0,O0QQOOOCQ0);
end;
function O00QOOOCQ0.HashFinal:TBytes;
var
OCQQOOOCQ0:TBytes;
begin
OO0QOOOCQ0.TransformFinalBlock(nil,0,0);
OCQQOOOCQ0:=OO0QOOOCQ0.Hash;
OO0QOOOCQ0.Initialize;
OO0QOOOCQ0.TransformBlock(FKeyValue,0,Length(FKeyValue));
OO0QOOOCQ0.TransformBlock(O0OQOOOCQ0,0,Length(O0OQOOOCQ0));
OO0QOOOCQ0.TransformFinalBlock(OCQQOOOCQ0,0,Length(OCQQOOOCQ0));
OQ0QOOOCQ0:=False;
Result:=OO0QOOOCQ0.Hash;
end;
function O00QOOOCQ0.get_HashSize:Integer;
begin
Result:=OO0QOOOCQ0.HashSize;
end;
constructor OQ0COOOCQ0.Create(OOCQOOOCQ0:OOOOCOQOQ0;OQQCOOOCQ0:THashAlgorithm;const OQCQOOOCQ0:TBytes);
var
OCCQOOOCQ0:Integer;
begin
inherited Create;
if Length(OQCQOOOCQ0)=0 then
raise EScError.Create(seInvalidInputArgs);
if OOCQOOOCQ0=OQ0OCOQOQ0 then
OCCQOOOCQ0:=48
else if OOCQOOOCQ0=OOC0COQOQ0 then
OCCQOOOCQ0:=40
else begin
OCCQOOOCQ0:=0;
Assert(False);
end;
SetLength(O0OCOOOCQ0,OCCQOOOCQ0);
SetLength(OOOCOOOCQ0,OCCQOOOCQ0);
FillChar(O0OCOOOCQ0[0],OCCQOOOCQ0,$36);
FillChar(OOOCOOOCQ0[0],OCCQOOOCQ0,$5C);
OC0COOOCQ0:=OQQCOOOCQ0;
SetLength(FKeyValue,Length(OQCQOOOCQ0));
Buffer.BlockCopy(OQCQOOOCQ0,0,FKeyValue,0,Length(OQCQOOOCQ0));
end;
procedure OQ0COOOCQ0.Initialize;
begin
OC0COOOCQ0.Initialize;
FState:=0;
end;
procedure OQ0COOOCQ0.HashCore(const OCOQOOOCQ0:TValueArr;O0QQOOOCQ0,OOQQOOOCQ0:Integer);
begin
OC0COOOCQ0.TransformBlock(OCOQOOOCQ0,O0QQOOOCQ0,OOQQOOOCQ0,OCOQOOOCQ0,O0QQOOOCQ0);
end;
function OQ0COOOCQ0.HashFinal:TBytes;
var
OCQQOOOCQ0:TBytes;
begin
OC0COOOCQ0.TransformBlock(FKeyValue,0,Length(FKeyValue));
OC0COOOCQ0.TransformFinalBlock(O0OCOOOCQ0,0,Length(O0OCOOOCQ0));
OCQQOOOCQ0:=OC0COOOCQ0.Hash;
OC0COOOCQ0.Initialize;
OC0COOOCQ0.TransformBlock(FKeyValue,0,Length(FKeyValue));
OC0COOOCQ0.TransformBlock(OOOCOOOCQ0,0,Length(OOOCOOOCQ0));
OC0COOOCQ0.TransformFinalBlock(OCQQOOOCQ0,0,Length(OCQQOOOCQ0));
Result:=OC0COOOCQ0.Hash;
end;
function OQ0COOOCQ0.get_HashSize:Integer;
begin
Result:=OC0COOOCQ0.HashSize;
end;
function O0CCOOOCQ0.O00OCC00Q0(const OOCCOOOCQ0:TCipherDefinition;
const OQCCOOOCQ0:OQQC0Q00Q0):OCQ0O0Q0Q0;
var
OCCCOOOCQ0,O000OOOCQ0:TSymmetricAlgorithm;
OO00OOOCQ0,OQ00OOOCQ0,OC00OOOCQ0,O0O0OOOCQ0,OOO0OOOCQ0,OQO0OOOCQ0:TBytes;
OCO0OOOCQ0:O0C0QQOCQ0;
O0Q0OOOCQ0:integer;
begin
Result:=OCQ0O0Q0Q0.Create;
try
OCCCOOOCQ0:=nil;
O000OOOCQ0:=nil;
try
OCCCOOOCQ0:=TSymmetricAlgorithm(OOCCOOOCQ0.CipherAlgorithmClass.Create);
O000OOOCQ0:=TSymmetricAlgorithm(OOCCOOOCQ0.CipherAlgorithmClass.Create);
if OOCCOOOCQ0.CipherMode=cmECB then
O0Q0OOOCQ0:=0
else
O0Q0OOOCQ0:=OCCCOOOCQ0.BlockSize;
OCO0OOOCQ0:=O0C0QQOCQ0.Create(OQCO0C00Q0,O0QO0C00Q0.OOOO0C00Q0,False);
try
OO00OOOCQ0:=OCO0OOOCQ0.O0COQQOCQ0(OOCQCC0OQ0.OC00CC0OQ0(OOCCOOOCQ0.HashAlgorithm));
OQ00OOOCQ0:=OCO0OOOCQ0.O0COQQOCQ0(OOCQCC0OQ0.OC00CC0OQ0(OOCCOOOCQ0.HashAlgorithm));
OC00OOOCQ0:=OCO0OOOCQ0.O0COQQOCQ0(OOCCOOOCQ0.KeyMaterialLength);
O0O0OOOCQ0:=OCO0OOOCQ0.O0COQQOCQ0(OOCCOOOCQ0.KeyMaterialLength);
OOO0OOOCQ0:=OCO0OOOCQ0.O0COQQOCQ0(O0Q0OOOCQ0);
OQO0OOOCQ0:=OCO0OOOCQ0.O0COQQOCQ0(O0Q0OOOCQ0);
finally
OCO0OOOCQ0.Free;
end;
OCCCOOOCQ0.Mode:=OOCCOOOCQ0.CipherMode;
OCCCOOOCQ0.Key:=OC00OOOCQ0;
OCCCOOOCQ0.IV:=OOO0OOOCQ0;
O000OOOCQ0.Mode:=OOCCOOOCQ0.CipherMode;
O000OOOCQ0.Key:=O0O0OOOCQ0;
O000OOOCQ0.IV:=OQO0OOOCQ0;
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
Result.O0C0O0Q0Q0:=OCCCOOOCQ0;
Result.OOC0O0Q0Q0:=O000OOOCQ0;
end
else begin
Result.O0C0O0Q0Q0:=O000OOOCQ0;
Result.OOC0O0Q0Q0:=OCCCOOOCQ0;
end;
except
OCCCOOOCQ0.Free;
O000OOOCQ0.Free;
raise;
end;
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
Result.OQC0O0Q0Q0:=O00QOOOCQ0.Create(OOCCOOOCQ0.HashAlgorithm,OO00OOOCQ0);
Result.OCC0O0Q0Q0:=O00QOOOCQ0.Create(OOCCOOOCQ0.HashAlgorithm,OQ00OOOCQ0);
end
else begin
Result.OQC0O0Q0Q0:=O00QOOOCQ0.Create(OOCCOOOCQ0.HashAlgorithm,OQ00OOOCQ0);
Result.OCC0O0Q0Q0:=O00QOOOCQ0.Create(OOCCOOOCQ0.HashAlgorithm,OO00OOOCQ0);
end;
except
Result.Free;
raise;
end;
FillChar(OO00OOOCQ0[0],Length(OO00OOOCQ0),0);
FillChar(OQ00OOOCQ0[0],Length(OQ00OOOCQ0),0);
FillChar(OC00OOOCQ0[0],Length(OC00OOOCQ0),0);
FillChar(O0O0OOOCQ0[0],Length(O0O0OOOCQ0),0);
if Length(OOO0OOOCQ0)>0 then
FillChar(OOO0OOOCQ0[0],Length(OOO0OOOCQ0),0);
if Length(OQO0OOOCQ0)>0 then
FillChar(OQO0OOOCQ0[0],Length(OQO0OOOCQ0),0);
end;
function O0CCOOOCQ0.OCCQOQ00Q0:THashAlgorithm;
begin
Result:=OCOCO0Q0Q0.Create;
OCOCO0Q0Q0(Result).OOQ0O0Q0Q0:=O0CCCQQ0Q0;
end;
procedure O0CCOOOCQ0.O0COCC00Q0(const OOQ0OOOCQ0,OQQ0OOOCQ0:TBytes;
const OCQ0OOOCQ0:array of byte);
var
O0C0OOOCQ0:O0C0QQOCQ0;
begin
O0C0OOOCQ0:=O0C0QQOCQ0.Create(OOQ0OOOCQ0,OQQ0OOOCQ0,True);
try
OQCO0C00Q0:=O0C0OOOCQ0.O0COQQOCQ0(48);
finally
O0C0OOOCQ0.Free;
end;
end;
procedure O0CCOOOCQ0.OOOOQC00Q0(OOC0OOOCQ0:OCQC00Q0Q0);
var
OQC0OOOCQ0:OCOCO0Q0Q0;
begin
OQC0OOOCQ0:=OCOCO0Q0Q0.Create;
try
OQC0OOOCQ0.OOQ0O0Q0Q0:=O0CCCQQ0Q0;
OQC0OOOCQ0.OQQ0O0Q0Q0:=OQCO0C00Q0;
OQC0OOOCQ0.TransformBlock(OQQO0C00Q0,0,OCQO0C00Q0);
if OOQO0C00Q0=OOOQOOQ0Q0 then
OQC0OOOCQ0.TransformFinalBlock(@O0OOOOOCQ0[0],Length(O0OOOOOCQ0))
else
OQC0OOOCQ0.TransformFinalBlock(@OOOOOOOCQ0[0],Length(OOOOOOOCQ0));
OOC0OOOCQ0.OQC000Q0Q0(OQC0OOOCQ0.Hash);
finally
OQC0OOOCQ0.Free;
end;
end;
procedure O0CCOOOCQ0.OCOOQC00Q0(const OCC0OOOCQ0:TBytes;O00OOOOCQ0,OO0OOOOCQ0:integer);
var
OQ0OOOOCQ0:OCOCO0Q0Q0;
OC0OOOOCQ0:integer;
begin
if OO0OOOOCQ0<>36 then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
OQ0OOOOCQ0:=OCOCO0Q0Q0.Create;
try
OQ0OOOOCQ0.OOQ0O0Q0Q0:=O0CCCQQ0Q0;
OQ0OOOOCQ0.OQQ0O0Q0Q0:=OQCO0C00Q0;
OQ0OOOOCQ0.TransformBlock(OQQO0C00Q0,0,OCQO0C00Q0);
if OOQO0C00Q0=OOOQOOQ0Q0 then
OQ0OOOOCQ0.TransformFinalBlock(@OOOOOOOCQ0[0],Length(OOOOOOOCQ0))
else
OQ0OOOOCQ0.TransformFinalBlock(@O0OOOOOCQ0[0],Length(O0OOOOOCQ0));
for OC0OOOOCQ0:=0 to Length(OQ0OOOOCQ0.Hash)-1 do
if OQ0OOOOCQ0.Hash[OC0OOOOCQ0]<>OCC0OOOCQ0[O00OOOOCQ0+OC0OOOOCQ0]then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seHashVerificationNotCorrespond);
finally
OQ0OOOOCQ0.Free;
end;
end;
function O0CCOOOCQ0.O00COQ00Q0:OOCCCQQ0Q0;
begin
Result:=O0CCCQQ0Q0;
end;
end.
