//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright � 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsTLS1HandshakeLayerUni;
interface
uses
SysUtils,
CLRClasses,CRTypes,CRHash,CRHashAlgorithm,CRHMAC,
{$IFNDEF UNIDACPRO}
TdsUtils,TdsSSLTypes,TdsCipherSuites,TdsBridge,
TdsLayers,TdsSSLMessages,TdsAlgorithmSupport;
{$ELSE}
TdsUtilsUni,TdsSSLTypesUni,TdsCipherSuitesUni,TdsBridgeUni,
TdsLayersUni,TdsSSLMessagesUni,TdsAlgorithmSupportUni;
{$ENDIF}
type
O00O0QOCQ0=class
private
OO0O0QOCQ0:THMAC;
OQ0O0QOCQ0:integer;
OC0O0QOCQ0:TBytes;
O0OO0QOCQ0:TBytes;
OOOO0QOCQ0:integer;
OQOO0QOCQ0:TBytes;
protected
procedure OCOO0QOCQ0(O0QO0QOCQ0:THashAlgorithmClass;const OOQO0QOCQ0,OQQO0QOCQ0:TBytes);
function OCQO0QOCQ0:TBytes;
public
constructor Create(OOCO0QOCQ0:THashAlgorithmClass;const OQCO0QOCQ0,OCCO0QOCQ0:TBytes);
destructor Destroy;override;
function OO0QCQOCQ0(OQ0QCQOCQ0:integer):TBytes;
procedure O0OQCQOCQ0;
end;
OOOQCQOCQ0=class
protected
OQOQCQOCQ0:OOOOCOQOQ0;
procedure OCOQCQOCQ0(const O0QQCQOCQ0:TBytes;const OOQQCQOCQ0:array of byte;const OQQQCQOCQ0:TBytes);virtual;abstract;
public
constructor Create(const O0CQCQOCQ0:TBytes;const OOCQCQOCQ0:array of byte;
const OQCQCQOCQ0:TBytes;OOCO0QOCQ0:OOOOCOQOQ0);
function OCCQCQOCQ0(OQ0QCQOCQ0:integer):TBytes;virtual;abstract;
end;
O00CCQOCQ0=class(OOOQCQOCQ0)
private
OO0CCQOCQ0:O00O0QOCQ0;
OQ0CCQOCQ0:O00O0QOCQ0;
protected
procedure OCOQCQOCQ0(const O0QQCQOCQ0:TBytes;const OOQQCQOCQ0:array of byte;const OQQQCQOCQ0:TBytes);override;
public
destructor Destroy;override;
function OCCQCQOCQ0(OQ0QCQOCQ0:integer):TBytes;override;
end;
OCQCCQOCQ0=class(OOOQCQOCQ0)
private
O0CCCQOCQ0:O00O0QOCQ0;
protected
procedure OCOQCQOCQ0(const O0QQCQOCQ0:TBytes;const OOQQCQOCQ0:array of byte;const OQQQCQOCQ0:TBytes);override;
public
destructor Destroy;override;
function OCCQCQOCQ0(OQ0QCQOCQ0:integer):TBytes;override;
end;
OQCCCQOCQ0=class of OOOQCQOCQ0;
OCCCCQOCQ0=class(OCOO0C00Q0)
protected
function O000CQOCQ0:OQCCCQOCQ0;virtual;abstract;
public
function O00OCC00Q0(const OO00CQOCQ0:TCipherDefinition;
const OQ00CQOCQ0:OQQC0Q00Q0):OCQ0O0Q0Q0;override;
procedure O0COCC00Q0(const OQC0CQOCQ0,OCC0CQOCQ0:TBytes;const O00OCQOCQ0:array of byte);override;
procedure OOOOQC00Q0(OQ0OCQOCQ0:OCQC00Q0Q0);override;
procedure OCOOQC00Q0(const OOOOCQOCQ0:TBytes;OQOOCQOCQ0,OCOOCQOCQ0:integer);override;
end;
O0COCQOCQ0=class(OCCCCQOCQ0)
protected
function O000CQOCQ0:OQCCCQOCQ0;override;
public
function OCCQOQ00Q0:THashAlgorithm;override;
function O00COQ00Q0:OOCCCQQ0Q0;override;
end;
OOCOCQOCQ0=class(OCCCCQOCQ0)
protected
function O000CQOCQ0:OQCCCQOCQ0;override;
public
function OCCQOQ00Q0:THashAlgorithm;override;
function O00COQ00Q0:OOCCCQQ0Q0;override;
end;
OQCOCQOCQ0=class(OCCCCQOCQ0)
private
OCCOCQOCQ0:array of OOOCQQQ0Q0;
protected
function O000CQOCQ0:OQCCCQOCQ0;override;
public
procedure OCCCQC00Q0(O00QQQOCQ0:OCQC00Q0Q0;OO0QQQOCQ0:TScCertificate);override;
procedure OCC0QC00Q0(OOQQQQOCQ0:OCQC00Q0Q0;OQQQQQOCQ0:TScCertificate);override;
procedure O0O0QC00Q0(OC0CQQOCQ0:OCQC00Q0Q0;O0OCQQOCQ0:TScCertificate);override;
procedure OOQ0QC00Q0(OCQCQQOCQ0:OCQC00Q0Q0;
O0CCQQOCQ0:TScCertificate);override;
procedure OQCQQC00Q0(OC00QQOCQ0:OCQC00Q0Q0);override;
procedure O00CQC00Q0(OOO0QQOCQ0:OCQC00Q0Q0);override;
function OCCQOQ00Q0:THashAlgorithm;override;
function O00COQ00Q0:OOCCCQQ0Q0;override;
end;
implementation
uses
CRDECUtil,CRSymmetricAlgorithm,
{$IFNDEF UNIDACPRO}
TdsSSLConsts,TdsMD5SHA1CSP,TdsCertificateExts,TdsSSLExtensions;
{$ELSE}
TdsSSLConstsUni,TdsMD5SHA1CSPUni,TdsCertificateExtsUni,TdsSSLExtensionsUni;
{$ENDIF}
const
O0Q0QQOCQ0:array[0..12]of byte=
(107,101,121,32,101,120,112,97,110,115,105,111,110);
OOQ0QQOCQ0:array[0..14]of byte=
(99,108,105,101,110,116,32,102,105,110,105,115,104,101,100);
OQQ0QQOCQ0:array[0..14]of byte=
(115,101,114,118,101,114,32,102,105,110,105,115,104,101,100);
const
OCQ0QQOCQ0:array[0..12]of OOOCQQQ0Q0=(
OQOQQQQ0Q0,OCOQQQQ0Q0,O0QQQQQ0Q0,
OOQQQQQ0Q0,OQQQQQQ0Q0,OCQQQQQ0Q0,
O0CQQQQ0Q0,OOCQQQQ0Q0,OQCQQQQ0Q0,
OCCQQQQ0Q0,O00CQQQ0Q0,OO0CQQQ0Q0,
OQ0CQQQ0Q0
);
constructor O00O0QOCQ0.Create(OOCO0QOCQ0:THashAlgorithmClass;const OQCO0QOCQ0,OCCO0QOCQ0:TBytes);
begin
inherited Create;
OCOO0QOCQ0(OOCO0QOCQ0,OQCO0QOCQ0,OCCO0QOCQ0);
end;
destructor O00O0QOCQ0.Destroy;
begin
OO0O0QOCQ0.Free;
FillChar(OC0O0QOCQ0[0],Length(OC0O0QOCQ0),0);
FillChar(O0OO0QOCQ0[0],Length(O0OO0QOCQ0),0);
FillChar(OQOO0QOCQ0[0],Length(OQOO0QOCQ0),0);
inherited;
end;
procedure O00O0QOCQ0.OCOO0QOCQ0(O0QO0QOCQ0:THashAlgorithmClass;const OOQO0QOCQ0,OQQO0QOCQ0:TBytes);
begin
if(O0QO0QOCQ0=nil)or(Length(OQQO0QOCQ0)=0)then
raise EScError.Create(seInvalidInputArgs);
OO0O0QOCQ0:=THMAC.Create(O0QO0QOCQ0,OOQO0QOCQ0);
OC0O0QOCQ0:=OQQO0QOCQ0;
OQ0O0QOCQ0:=OO0O0QOCQ0.HashSize;
O0OQCQOCQ0;
end;
procedure O00O0QOCQ0.O0OQCQOCQ0;
begin
OQOO0QOCQ0:=OO0O0QOCQ0.ComputeHash(OC0O0QOCQ0);
O0OO0QOCQ0:=OCQO0QOCQ0;
OOOO0QOCQ0:=0;
end;
function O00O0QOCQ0.OCQO0QOCQ0:TBytes;
begin
OO0O0QOCQ0.TransformBlock(OQOO0QOCQ0,0,OQ0O0QOCQ0);
OO0O0QOCQ0.TransformFinalBlock(OC0O0QOCQ0,0,Length(OC0O0QOCQ0));
Result:=OO0O0QOCQ0.Hash;
OO0O0QOCQ0.Initialize;
OQOO0QOCQ0:=OO0O0QOCQ0.ComputeHash(OQOO0QOCQ0);
end;
function O00O0QOCQ0.OO0QCQOCQ0(OQ0QCQOCQ0:integer):TBytes;
var
OC0QCQOCQ0:integer;
begin
if OQ0QCQOCQ0<0 then
raise EScError.Create(seInvalidInputArgs);
SetLength(Result,OQ0QCQOCQ0);
OC0QCQOCQ0:=0;
while OC0QCQOCQ0<OQ0QCQOCQ0 do begin
if(OC0QCQOCQ0+Length(O0OO0QOCQ0)-OOOO0QOCQ0)>=OQ0QCQOCQ0 then begin
Buffer.BlockCopy(O0OO0QOCQ0,OOOO0QOCQ0,Result,OC0QCQOCQ0,OQ0QCQOCQ0-OC0QCQOCQ0);
OOOO0QOCQ0:=OOOO0QOCQ0+OQ0QCQOCQ0-OC0QCQOCQ0;
OC0QCQOCQ0:=Length(Result);
end
else begin
Buffer.BlockCopy(O0OO0QOCQ0,OOOO0QOCQ0,Result,OC0QCQOCQ0,Length(O0OO0QOCQ0)-OOOO0QOCQ0);
OC0QCQOCQ0:=OC0QCQOCQ0+Length(O0OO0QOCQ0)-OOOO0QOCQ0;
O0OO0QOCQ0:=OCQO0QOCQ0;
OOOO0QOCQ0:=0;
end;
end;
end;
constructor OOOQCQOCQ0.Create(const O0CQCQOCQ0:TBytes;const OOCQCQOCQ0:array of byte;
const OQCQCQOCQ0:TBytes;OOCO0QOCQ0:OOOOCOQOQ0);
begin
inherited Create;
if Length(OOCQCQOCQ0)=0 then
raise EScError.Create(seInvalidInputArgs);
OQOQCQOCQ0:=OOCO0QOCQ0;
OCOQCQOCQ0(O0CQCQOCQ0,OOCQCQOCQ0,OQCQCQOCQ0);
end;
destructor O00CCQOCQ0.Destroy;
begin
OO0CCQOCQ0.Free;
OQ0CCQOCQ0.Free;
inherited;
end;
procedure O00CCQOCQ0.OCOQCQOCQ0(const O0QQCQOCQ0:TBytes;
const OOQQCQOCQ0:array of byte;const OQQQCQOCQ0:TBytes);
var
OC0CCQOCQ0,O0OCCQOCQ0,OOOCCQOCQ0:TBytes;
OQOCCQOCQ0:integer;
begin
SetLength(OC0CCQOCQ0,Length(OOQQCQOCQ0)+Length(OQQQCQOCQ0));
Move(OOQQCQOCQ0[0],OC0CCQOCQ0[0],Length(OOQQCQOCQ0));
Move(OQQQCQOCQ0[0],OC0CCQOCQ0[Length(OOQQCQOCQ0)],Length(OQQQCQOCQ0));
OQOCCQOCQ0:=Length(O0QQCQOCQ0);
if(OQOCCQOCQ0 mod 2)=0 then
OQOCCQOCQ0:=OQOCCQOCQ0 shr 1
else
OQOCCQOCQ0:=(OQOCCQOCQ0 shr 1)+1;
SetLength(O0OCCQOCQ0,OQOCCQOCQ0);
SetLength(OOOCCQOCQ0,OQOCCQOCQ0);
if OQOCCQOCQ0>0 then begin
Buffer.BlockCopy(O0QQCQOCQ0,0,O0OCCQOCQ0,0,OQOCCQOCQ0);
Buffer.BlockCopy(O0QQCQOCQ0,Length(O0QQCQOCQ0)-OQOCCQOCQ0,OOOCCQOCQ0,0,OQOCCQOCQ0);
end;
OO0CCQOCQ0:=O00O0QOCQ0.Create(THash_MD5,O0OCCQOCQ0,OC0CCQOCQ0);
OQ0CCQOCQ0:=O00O0QOCQ0.Create(THash_SHA1,OOOCCQOCQ0,OC0CCQOCQ0);
FillChar(O0OCCQOCQ0[0],Length(O0OCCQOCQ0),0);
FillChar(OOOCCQOCQ0[0],Length(OOOCCQOCQ0),0);
end;
function O00CCQOCQ0.OCCQCQOCQ0(OQ0QCQOCQ0:integer):TBytes;
var
O0QCCQOCQ0,OOQCCQOCQ0:TBytes;
OQQCCQOCQ0:integer;
begin
O0QCCQOCQ0:=OO0CCQOCQ0.OO0QCQOCQ0(OQ0QCQOCQ0);
OOQCCQOCQ0:=OQ0CCQOCQ0.OO0QCQOCQ0(OQ0QCQOCQ0);
SetLength(Result,OQ0QCQOCQ0);
for OQQCCQOCQ0:=0 to Length(Result)-1 do
Result[OQQCCQOCQ0]:=O0QCCQOCQ0[OQQCCQOCQ0]xor OOQCCQOCQ0[OQQCCQOCQ0];
end;
destructor OCQCCQOCQ0.Destroy;
begin
O0CCCQOCQ0.Free;
inherited;
end;
procedure OCQCCQOCQ0.OCOQCQOCQ0(const O0QQCQOCQ0:TBytes;
const OOQQCQOCQ0:array of byte;const OQQQCQOCQ0:TBytes);
var
O0QO0QOCQ0:THashAlgorithmClass;
OC0CCQOCQ0:TBytes;
begin
SetLength(OC0CCQOCQ0,Length(OOQQCQOCQ0)+Length(OQQQCQOCQ0));
Move(OOQQCQOCQ0[0],OC0CCQOCQ0[0],Length(OOQQCQOCQ0));
Move(OQQQCQOCQ0[0],OC0CCQOCQ0[Length(OOQQCQOCQ0)],Length(OQQQCQOCQ0));
if OQOQCQOCQ0=OO0OCOQOQ0 then
O0QO0QOCQ0:=THash_SHA2_384
else
O0QO0QOCQ0:=THash_SHA2_256;
O0CCCQOCQ0:=O00O0QOCQ0.Create(O0QO0QOCQ0,O0QQCQOCQ0,OC0CCQOCQ0);
end;
function OCQCCQOCQ0.OCCQCQOCQ0(OQ0QCQOCQ0:integer):TBytes;
begin
Result:=O0CCCQOCQ0.OO0QCQOCQ0(OQ0QCQOCQ0);
end;
function OCCCCQOCQ0.O00OCC00Q0(const OO00CQOCQ0:TCipherDefinition;
const OQ00CQOCQ0:OQQC0Q00Q0):OCQ0O0Q0Q0;
var
OC00CQOCQ0,O0O0CQOCQ0:TSymmetricAlgorithm;
OOO0CQOCQ0,OQO0CQOCQ0,OCO0CQOCQ0,O0Q0CQOCQ0,OOQ0CQOCQ0,OQQ0CQOCQ0:TBytes;
OCQ0CQOCQ0:TBytes;
O0C0CQOCQ0:OOOQCQOCQ0;
OOC0CQOCQ0:integer;
begin
Result:=OCQ0O0Q0Q0.Create;
try
OC00CQOCQ0:=nil;
O0O0CQOCQ0:=nil;
try
OC00CQOCQ0:=TSymmetricAlgorithm(OO00CQOCQ0.CipherAlgorithmClass.Create);
O0O0CQOCQ0:=TSymmetricAlgorithm(OO00CQOCQ0.CipherAlgorithmClass.Create);
if OO00CQOCQ0.CipherMode=cmECB then
OOC0CQOCQ0:=0
else
if OO00CQOCQ0.CipherMode=cmGCM then
OOC0CQOCQ0:=4
else
OOC0CQOCQ0:=OC00CQOCQ0.BlockSize;
SetLength(OCQ0CQOCQ0,64);
Buffer.BlockCopy(O0QO0C00Q0.OOOO0C00Q0,32,OCQ0CQOCQ0,0,32);
Buffer.BlockCopy(O0QO0C00Q0.OOOO0C00Q0,0,OCQ0CQOCQ0,32,32);
O0C0CQOCQ0:=O000CQOCQ0.Create(OQCO0C00Q0,O0Q0QQOCQ0,OCQ0CQOCQ0,OO00CQOCQ0.HashAlgorithm);
try
if OO00CQOCQ0.CipherMode<>cmGCM then begin
OOO0CQOCQ0:=O0C0CQOCQ0.OCCQCQOCQ0(OOCQCC0OQ0.OC00CC0OQ0(OO00CQOCQ0.HashAlgorithm));
OQO0CQOCQ0:=O0C0CQOCQ0.OCCQCQOCQ0(OOCQCC0OQ0.OC00CC0OQ0(OO00CQOCQ0.HashAlgorithm));
end
else begin
OOO0CQOCQ0:=nil;
OQO0CQOCQ0:=nil;
end;
OCO0CQOCQ0:=O0C0CQOCQ0.OCCQCQOCQ0(OO00CQOCQ0.KeyMaterialLength);
O0Q0CQOCQ0:=O0C0CQOCQ0.OCCQCQOCQ0(OO00CQOCQ0.KeyMaterialLength);
OOQ0CQOCQ0:=O0C0CQOCQ0.OCCQCQOCQ0(OOC0CQOCQ0);
OQQ0CQOCQ0:=O0C0CQOCQ0.OCCQCQOCQ0(OOC0CQOCQ0);
finally
O0C0CQOCQ0.Free;
end;
OC00CQOCQ0.Mode:=OO00CQOCQ0.CipherMode;
O0O0CQOCQ0.Mode:=OO00CQOCQ0.CipherMode;
OC00CQOCQ0.Key:=OCO0CQOCQ0;
O0O0CQOCQ0.Key:=O0Q0CQOCQ0;
if OO00CQOCQ0.CipherMode<>cmGCM then begin
OC00CQOCQ0.IV:=OOQ0CQOCQ0;
O0O0CQOCQ0.IV:=OQQ0CQOCQ0;
end;
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
Result.O0C0O0Q0Q0:=OC00CQOCQ0;
Result.OOC0O0Q0Q0:=O0O0CQOCQ0;
end
else begin
Result.O0C0O0Q0Q0:=O0O0CQOCQ0;
Result.OOC0O0Q0Q0:=OC00CQOCQ0;
end;
except
OC00CQOCQ0.Free;
O0O0CQOCQ0.Free;
raise;
end;
if OO00CQOCQ0.CipherMode<>cmGCM then begin
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
Result.OQC0O0Q0Q0:=THMAC.Create(OOCQCC0OQ0.OCOCCC0OQ0(OO00CQOCQ0.HashAlgorithm),OOO0CQOCQ0);
Result.OCC0O0Q0Q0:=THMAC.Create(OOCQCC0OQ0.OCOCCC0OQ0(OO00CQOCQ0.HashAlgorithm),OQO0CQOCQ0);
end
else begin
Result.OQC0O0Q0Q0:=THMAC.Create(OOCQCC0OQ0.OCOCCC0OQ0(OO00CQOCQ0.HashAlgorithm),OQO0CQOCQ0);
Result.OCC0O0Q0Q0:=THMAC.Create(OOCQCC0OQ0.OCOCCC0OQ0(OO00CQOCQ0.HashAlgorithm),OOO0CQOCQ0);
end;
end
else begin
Result.OQC0O0Q0Q0:=nil;
Result.OCC0O0Q0Q0:=nil;
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
Result.O00OO0Q0Q0:=OOQ0CQOCQ0;
Result.OO0OO0Q0Q0:=OQQ0CQOCQ0;
end
else begin
Result.O00OO0Q0Q0:=OQQ0CQOCQ0;
Result.OO0OO0Q0Q0:=OOQ0CQOCQ0;
end;
OOQ0CQOCQ0:=nil;
OQQ0CQOCQ0:=nil;
end;
except
Result.Free;
raise;
end;
if Length(OOO0CQOCQ0)>0 then
FillChar(OOO0CQOCQ0[0],Length(OOO0CQOCQ0),0);
if Length(OQO0CQOCQ0)>0 then
FillChar(OQO0CQOCQ0[0],Length(OQO0CQOCQ0),0);
FillChar(OCO0CQOCQ0[0],Length(OCO0CQOCQ0),0);
FillChar(O0Q0CQOCQ0[0],Length(O0Q0CQOCQ0),0);
if Length(OOQ0CQOCQ0)>0 then
FillChar(OOQ0CQOCQ0[0],Length(OOQ0CQOCQ0),0);
if Length(OQQ0CQOCQ0)>0 then
FillChar(OQQ0CQOCQ0[0],Length(OQQ0CQOCQ0),0);
if Length(OCQ0CQOCQ0)>0 then
FillChar(OCQ0CQOCQ0[0],Length(OCQ0CQOCQ0),0);
end;
procedure OCCCCQOCQ0.O0COCC00Q0(const OQC0CQOCQ0,OCC0CQOCQ0:TBytes;
const O00OCQOCQ0:array of byte);
var
OO0OCQOCQ0:OOOQCQOCQ0;
begin
OO0OCQOCQ0:=O000CQOCQ0.Create(OQC0CQOCQ0,O00OCQOCQ0,OCC0CQOCQ0,OO0C00Q0Q0[O00QCC00Q0].HashAlgorithm);
try
OQCO0C00Q0:=OO0OCQOCQ0.OCCQCQOCQ0(48);
finally
OO0OCQOCQ0.Free;
end;
end;
procedure OCCCCQOCQ0.OOOOQC00Q0(OQ0OCQOCQ0:OCQC00Q0Q0);
var
OC0OCQOCQ0:OOOQCQOCQ0;
O0OOCQOCQ0:TBytes;
begin
O0OOCQOCQ0:=OQC0CC00Q0;
if OOQO0C00Q0=OOOQOOQ0Q0 then
OC0OCQOCQ0:=O000CQOCQ0.Create(OQCO0C00Q0,OOQ0QQOCQ0,
O0OOCQOCQ0,OO0C00Q0Q0[O00QCC00Q0].HashAlgorithm)
else
OC0OCQOCQ0:=O000CQOCQ0.Create(OQCO0C00Q0,OQQ0QQOCQ0,
O0OOCQOCQ0,OO0C00Q0Q0[O00QCC00Q0].HashAlgorithm);
try
OQ0OCQOCQ0.OQC000Q0Q0(OC0OCQOCQ0.OCCQCQOCQ0(12));
finally
OC0OCQOCQ0.Free;
end;
end;
procedure OCCCCQOCQ0.OCOOQC00Q0(const OOOOCQOCQ0:TBytes;OQOOCQOCQ0,OCOOCQOCQ0:integer);
var
O0QOCQOCQ0:OOOQCQOCQ0;
OOQOCQOCQ0:TBytes;
OQQOCQOCQ0:TBytes;
OCQOCQOCQ0:integer;
begin
if OCOOCQOCQ0<>12 then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
OQQOCQOCQ0:=OQC0CC00Q0;
if OOQO0C00Q0=OOOQOOQ0Q0 then
O0QOCQOCQ0:=O000CQOCQ0.Create(OQCO0C00Q0,OQQ0QQOCQ0,
OQQOCQOCQ0,OO0C00Q0Q0[O00QCC00Q0].HashAlgorithm)
else
O0QOCQOCQ0:=O000CQOCQ0.Create(OQCO0C00Q0,OOQ0QQOCQ0,
OQQOCQOCQ0,OO0C00Q0Q0[O00QCC00Q0].HashAlgorithm);
try
OOQOCQOCQ0:=O0QOCQOCQ0.OCCQCQOCQ0(12);
finally
O0QOCQOCQ0.Free;
end;
for OCQOCQOCQ0:=0 to Length(OOQOCQOCQ0)-1 do
if OOQOCQOCQ0[OCQOCQOCQ0]<>OOOOCQOCQ0[OQOOCQOCQ0+OCQOCQOCQ0]then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seHashVerificationNotCorrespond);
end;
function O0COCQOCQ0.O000CQOCQ0:OQCCCQOCQ0;
begin
Result:=O00CCQOCQ0;
end;
function O0COCQOCQ0.OCCQOQ00Q0:THashAlgorithm;
begin
Result:=OCOCO0Q0Q0.Create;
OCOCO0Q0Q0(Result).OOQ0O0Q0Q0:=O0QCCQQ0Q0;
end;
function O0COCQOCQ0.O00COQ00Q0:OOCCCQQ0Q0;
begin
Result:=O0QCCQQ0Q0;
end;
function OOCOCQOCQ0.O000CQOCQ0:OQCCCQOCQ0;
begin
Result:=O00CCQOCQ0;
end;
function OOCOCQOCQ0.OCCQOQ00Q0:THashAlgorithm;
begin
Result:=OCOCO0Q0Q0.Create;
OCOCO0Q0Q0(Result).OOQ0O0Q0Q0:=O0QCCQQ0Q0;
end;
function OOCOCQOCQ0.O00COQ00Q0:OOCCCQQ0Q0;
begin
Result:=OOQCCQQ0Q0;
end;
procedure OQCOCQOCQ0.OCCCQC00Q0(O00QQQOCQ0:OCQC00Q0Q0;
OO0QQQOCQ0:TScCertificate);
var
OQ0QQQOCQ0:OOOCQQQ0Q0;
OC0QQQOCQ0:OOOOCOQOQ0;
O0OQQQOCQ0:THashAlgorithm;
OOOQQQOCQ0:boolean;
OQOQQQOCQ0,OCOQQQOCQ0:TBytes;
O0QQQQOCQ0:integer;
begin
if OO0QQQOCQ0=nil then
raise EScError.Create(seInvalidInputArgs);
OQ0QQQOCQ0:=OC0CQQQ0Q0;
OOOQQQOCQ0:=False;
for O0QQQQOCQ0:=0 to Length(OCCOCQOCQ0)-1 do begin
if(OO0QQQOCQ0.Key.OQOOOCQ0Q0=OQC0OQO0Q0[OCCOCQOCQ0[O0QQQQOCQ0]].Signature)and
((OQC0OQO0Q0[OCCOCQOCQ0[O0QQQQOCQ0]].KeyPadding={$IFNDEF UNIDACPRO}TdsCertificateExts{$ELSE}TdsCertificateExtsUni{$ENDIF}.OO0Q0OOOQ0)or
(OQC0OQO0Q0[OCCOCQOCQ0[O0QQQQOCQ0]].KeyPadding=OO0QQQOCQ0.SignatureAlgorithm.OQQCCOOOQ0))
then begin
OQ0QQQOCQ0:=OCCOCQOCQ0[O0QQQQOCQ0];
OOOQQQOCQ0:=True;
Break;
end;
end;
if not OOOQQQOCQ0 then
O0QO0C00Q0.OOC00C00Q0(OCO00OQ0Q0,seCertificateNotCorrespondToCertificateRequest);
OC0QQQOCQ0:=OQC0OQO0Q0[OQ0QQQOCQ0].Hash;
if OQC0OQO0Q0[OQ0QQQOCQ0].Padding=O00Q0OOOQ0 then begin
OO0QQQOCQ0.Key.OQCOOCQ0Q0.O0OC0OOOQ0:=OC0QQQOCQ0;
OO0QQQOCQ0.Key.OQCOOCQ0Q0.OOOC0OOOQ0:=OC0QQQOCQ0;
OO0QQQOCQ0.Key.OQCOOCQ0Q0.OQOC0OOOQ0:=OOCQCC0OQ0.OC00CC0OQ0(OC0QQQOCQ0);
end;
O0OQQQOCQ0:=OOCQCC0OQ0.OOOCCC0OQ0(OC0QQQOCQ0);
try
OCOQQQOCQ0:=O0OQQQOCQ0.ComputeHash(TValueArr(OQQO0C00Q0),0,OCQO0C00Q0);
finally
O0OQQQOCQ0.Free;
end;
OQOQQQOCQ0:=OO0QQQOCQ0.SignHash(OCOQQQOCQ0,OC0QQQOCQ0,OQC0OQO0Q0[OQ0QQQOCQ0].Padding);
O00QQQOCQ0.O00QC0Q0Q0(OQQQO0Q0Q0[OQ0QQQOCQ0]);
O00QQQOCQ0.OOOO00Q0Q0(OQOQQQOCQ0);
end;
procedure OQCOCQOCQ0.OCC0QC00Q0(OOQQQQOCQ0:OCQC00Q0Q0;
OQQQQQOCQ0:TScCertificate);
var
OCQQQQOCQ0:OOOCQQQ0Q0;
O0CQQQOCQ0:OOOOCOQOQ0;
OOCQQQOCQ0:THashAlgorithm;
OQCQQQOCQ0,OCCQQQOCQ0:TBytes;
O00CQQOCQ0:integer;
OO0CQQOCQ0:boolean;
OQ0CQQOCQ0:integer;
begin
if OQQQQQOCQ0=nil then
raise EScError.Create(sePeerCertificateNotReceived);
OCQQQQOCQ0:=OC0OO0Q0Q0.OC0Q00Q0Q0(OOQQQQOCQ0.OO0CC0Q0Q0);
if OCQQQQOCQ0=OOOCQQQ0Q0(-1)then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seInvalidSignatureSchemeAlgorithm);
if OQC0OQO0Q0[OCQQQQOCQ0].Signature<>OQQQQQOCQ0.Key.OQOOOCQ0Q0 then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seCertificateNotCorrespondToRequiredSignatureAlgorithms);
OO0CQQOCQ0:=False;
for OQ0CQQOCQ0:=0 to Length(OCQ0QQOCQ0)-1 do begin
if OCQQQQOCQ0=OCQ0QQOCQ0[OQ0CQQOCQ0]then begin
OO0CQQOCQ0:=True;
Break;
end;
end;
if not OO0CQQOCQ0 then
O0QO0C00Q0.OOC00C00Q0(OQ0O0OQ0Q0,seCertificateNotCorrespondToRequiredSignatureAlgorithms);
O0CQQQOCQ0:=OQC0OQO0Q0[OCQQQQOCQ0].Hash;
if OQC0OQO0Q0[OCQQQQOCQ0].Padding=O00Q0OOOQ0 then begin
OQQQQQOCQ0.Key.OQCOOCQ0Q0.O0OC0OOOQ0:=O0CQQQOCQ0;
OQQQQQOCQ0.Key.OQCOOCQ0Q0.OOOC0OOOQ0:=O0CQQQOCQ0;
OQQQQQOCQ0.Key.OQCOOCQ0Q0.OQOC0OOOQ0:=OOCQCC0OQ0.OC00CC0OQ0(O0CQQQOCQ0);
end;
O00CQQOCQ0:=OOQQQQOCQ0.OO0CC0Q0Q0;
if O00CQQOCQ0=0 then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
OCCQQQOCQ0:=OOQQQQOCQ0.OOOCC0Q0Q0(O00CQQOCQ0);
OOCQQQOCQ0:=OOCQCC0OQ0.OOOCCC0OQ0(O0CQQQOCQ0);
try
OQCQQQOCQ0:=OOCQQQOCQ0.ComputeHash(TValueArr(OQQO0C00Q0),0,OCQO0C00Q0);
finally
OOCQQQOCQ0.Free;
end;
if not OQQQQQOCQ0.VerifyHashSign(OQCQQQOCQ0,OCCQQQOCQ0,O0CQQQOCQ0,OQC0OQO0Q0[OCQQQQOCQ0].Padding)then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seHashVerificationNotCorrespond);
end;
procedure OQCOCQOCQ0.O0O0QC00Q0(OC0CQQOCQ0:OCQC00Q0Q0;
O0OCQQOCQ0:TScCertificate);
var
OOOCQQOCQ0:OO00CCO0Q0;
OQOCQQOCQ0,OCOCQQOCQ0:OOOCQQQ0Q0;
O0QCQQOCQ0:THashAlgorithm;
OOQCQQOCQ0:TBytes;
OQQCQQOCQ0:integer;
begin
if O0OCQQOCQ0=nil then
raise EScError.Create(seServerCertificateNotSpecified);
OCOCQQOCQ0:=OQOQQQQ0Q0;
case O0OCQQOCQ0.Key.OQOOOCQ0Q0 of
OCOQQOQOQ0:
OCOCQQOCQ0:=OQOQQQQ0Q0;
O0QQQOQOQ0:
OCOCQQOCQ0:=OOQQQQQ0Q0;
else
O0QO0C00Q0.OOC00C00Q0(OCO00OQ0Q0,seCannotSignData);
end;
OOOCQQOCQ0:=OO00CCO0Q0(O0QO0C00Q0.OQ0O0C00Q0.OQOOOCO0Q0.OOCCQ0Q0Q0(OO00CCO0Q0));
if OOOCQQOCQ0<>nil then begin
for OQQCQQOCQ0:=0 to OOOCQQOCQ0.O00OCCO0Q0-1 do begin
OQOCQQOCQ0:=OOOCQQOCQ0.OO0OCCO0Q0[OQQCQQOCQ0];
if(O0OCQQOCQ0.Key.OQOOOCQ0Q0=OQC0OQO0Q0[OQOCQQOCQ0].Signature)and
(OQC0OQO0Q0[OQOCQQOCQ0].Hash<>OOC0COQOQ0)
then begin
OCOCQQOCQ0:=OQOCQQOCQ0;
Break;
end;
end;
end;
O0QCQQOCQ0:=OOCQCC0OQ0.OOOCCC0OQ0(OQC0OQO0Q0[OCOCQQOCQ0].Hash);
try
O0QCQQOCQ0.TransformBlock(O0QO0C00Q0.OOOO0C00Q0,0,64);
O0QCQQOCQ0.TransformFinalBlock(OC0CQQOCQ0.OCCCC0Q0Q0,OC0CQQOCQ0.O000C0Q0Q0,OC0CQQOCQ0.OQ00C0Q0Q0-OC0CQQOCQ0.O000C0Q0Q0);
OOQCQQOCQ0:=O0OCQQOCQ0.SignHash(O0QCQQOCQ0.Hash,OQC0OQO0Q0[OCOCQQOCQ0].Hash,OQC0OQO0Q0[OCOCQQOCQ0].Padding);
finally
O0QCQQOCQ0.Free;
end;
OC0CQQOCQ0.O00QC0Q0Q0(OQQQO0Q0Q0[OCOCQQOCQ0]);
OC0CQQOCQ0.OOOO00Q0Q0(OOQCQQOCQ0);
end;
procedure OQCOCQOCQ0.OOQ0QC00Q0(OCQCQQOCQ0:OCQC00Q0Q0;
O0CCQQOCQ0:TScCertificate);
var
OOCCQQOCQ0:OOOCQQQ0Q0;
OQCCQQOCQ0:OOOOCOQOQ0;
OCCCQQOCQ0:THashAlgorithm;
O000QQOCQ0:TBytes;
OO00QQOCQ0,OQ00QQOCQ0:integer;
begin
if O0CCQQOCQ0=nil then
raise EScError.Create(seServerCertificateNotReceived);
OO00QQOCQ0:=OCQCQQOCQ0.OO00C0Q0Q0;
OOCCQQOCQ0:=OC0OO0Q0Q0.OC0Q00Q0Q0(OCQCQQOCQ0.OO0CC0Q0Q0);
if OOCCQQOCQ0=OOOCQQQ0Q0(-1)then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seInvalidSignatureSchemeAlgorithm);
if OQC0OQO0Q0[OOCCQQOCQ0].Signature<>O0CCQQOCQ0.Key.OQOOOCQ0Q0 then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seHashVerificationNotCorrespond);
OQ00QQOCQ0:=OCQCQQOCQ0.OO0CC0Q0Q0;
O000QQOCQ0:=OCQCQQOCQ0.OOOCC0Q0Q0(OQ00QQOCQ0);
OQCCQQOCQ0:=OQC0OQO0Q0[OOCCQQOCQ0].Hash;
if OQC0OQO0Q0[OOCCQQOCQ0].Padding=O00Q0OOOQ0 then begin
O0CCQQOCQ0.Key.OQCOOCQ0Q0.O0OC0OOOQ0:=OQCCQQOCQ0;
O0CCQQOCQ0.Key.OQCOOCQ0Q0.OOOC0OOOQ0:=OQCCQQOCQ0;
O0CCQQOCQ0.Key.OQCOOCQ0Q0.OQOC0OOOQ0:=OOCQCC0OQ0.OC00CC0OQ0(OQCCQQOCQ0);
end;
OCCCQQOCQ0:=OOCQCC0OQ0.OOOCCC0OQ0(OQCCQQOCQ0);
try
OCCCQQOCQ0.TransformBlock(O0QO0C00Q0.OOOO0C00Q0,0,64);
OCCCQQOCQ0.TransformFinalBlock(OCQCQQOCQ0.OCCCC0Q0Q0,OCQCQQOCQ0.O000C0Q0Q0,OO00QQOCQ0-OCQCQQOCQ0.O000C0Q0Q0);
if not O0CCQQOCQ0.VerifyHashSign(OCCCQQOCQ0.Hash,O000QQOCQ0,OQCCQQOCQ0,OQC0OQO0Q0[OOCCQQOCQ0].Padding)then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seHashVerificationNotCorrespond);
finally
OCCCQQOCQ0.Free;
end;
end;
procedure OQCOCQOCQ0.OQCQQC00Q0(OC00QQOCQ0:OCQC00Q0Q0);
var
O0O0QQOCQ0:integer;
begin
OC00QQOCQ0.O00QC0Q0Q0(Length(OCQ0QQOCQ0)*2);
for O0O0QQOCQ0:=0 to Length(OCQ0QQOCQ0)-1 do
OC00QQOCQ0.O00QC0Q0Q0(OQQQO0Q0Q0[OCQ0QQOCQ0[O0O0QQOCQ0]]);
end;
procedure OQCOCQOCQ0.O00CQC00Q0(OOO0QQOCQ0:OCQC00Q0Q0);
var
OQO0QQOCQ0:integer;
OCO0QQOCQ0:integer;
begin
OQO0QQOCQ0:=OOO0QQOCQ0.OO0CC0Q0Q0;
if OQO0QQOCQ0>0 then begin
OQO0QQOCQ0:=OQO0QQOCQ0 div 2;
SetLength(OCCOCQOCQ0,OQO0QQOCQ0);
OCO0QQOCQ0:=0;
while OCO0QQOCQ0<OQO0QQOCQ0 do begin
OCCOCQOCQ0[OCO0QQOCQ0]:=OC0OO0Q0Q0.OC0Q00Q0Q0(OOO0QQOCQ0.OO0CC0Q0Q0);
if OCCOCQOCQ0[OCO0QQOCQ0]=OOOCQQQ0Q0(-1)then
Dec(OQO0QQOCQ0)
else
Inc(OCO0QQOCQ0);
end;
SetLength(OCCOCQOCQ0,OQO0QQOCQ0);
end;
end;
function OQCOCQOCQ0.O000CQOCQ0:OQCCCQOCQ0;
begin
Result:=OCQCCQOCQ0;
end;
function OQCOCQOCQ0.OCCQOQ00Q0:THashAlgorithm;
begin
if OO0C00Q0Q0[O00QCC00Q0].HashAlgorithm=OO0OCOQOQ0 then
Result:=THash_SHA2_384.Create
else
Result:=THash_SHA2_256.Create;
end;
function OQCOCQOCQ0.O00COQ00Q0:OOCCCQQ0Q0;
begin
Result:=OQQCCQQ0Q0;
end;
end.