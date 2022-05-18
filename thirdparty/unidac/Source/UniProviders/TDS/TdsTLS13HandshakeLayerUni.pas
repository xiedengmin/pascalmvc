//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright � 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsTLS13HandshakeLayerUni;
interface
uses
{$IFDEF MSWINDOWS}
Windows,
{$ENDIF}
SysUtils,Classes,
CLRClasses,CRTypes,CRFunctions,CRHash,CRHashAlgorithm,CRHMAC,
{$IFNDEF UNIDACPRO}
TdsUtils,TdsSSLTypes,TdsCipherSuites,TdsBridge,TdsCertificateExts,
TdsLayers,TdsSSLMessages,TdsAlgorithmSupport,TdsSSLExtensions;
{$ELSE}
TdsUtilsUni,TdsSSLTypesUni,TdsCipherSuitesUni,TdsBridgeUni,TdsCertificateExtsUni,
TdsLayersUni,TdsSSLMessagesUni,TdsAlgorithmSupportUni,TdsSSLExtensionsUni;
{$ENDIF}
type
OQCOCCOCQ0=class
private
OCCOCCOCQ0:THMAC;
O00QQCOCQ0:TBytes;
OO0QQCOCQ0:TBytes;
OQ0QQCOCQ0:integer;
OC0QQCOCQ0:TBytes;
function O0OQQCOCQ0:TBytes;
public
constructor Create(OQOQQCOCQ0:THashAlgorithmClass;const OCOQQCOCQ0,O0QQQCOCQ0:TBytes);
destructor Destroy;override;
function OQQQQCOCQ0(OCQQQCOCQ0:integer):TBytes;
procedure OOCQQCOCQ0;
end;
OQCQQCOCQ0=class(OCOO0C00Q0)
private
OCCQQCOCQ0:TBytes;
O00CQCOCQ0:TBytes;
OO0CQCOCQ0:TBytes;
OQ0CQCOCQ0:TBytes;
OC0CQCOCQ0:TBytes;
O0OCQCOCQ0:TBytes;
OOOCQCOCQ0:TBytes;
OQOCQCOCQ0:TBytes;
OCOCQCOCQ0:TBytes;
O0QCQCOCQ0:TBytes;
OOQCQCOCQ0:TBytes;
OQQCQCOCQ0:TBytes;
OCQCQCOCQ0:TBytes;
O0CCQCOCQ0:TBytes;
function OOCCQCOCQ0(const OQCCQCOCQ0,OCCCQCOCQ0:TBytes):TBytes;
function OO00QCOCQ0(const OQ00QCOCQ0,OC00QCOCQ0:TBytes;O0O0QCOCQ0:integer):TBytes;
function OQO0QCOCQ0(const OCO0QCOCQ0:TBytes;const O0Q0QCOCQ0:array of byte;
const OOQ0QCOCQ0:TBytes;OQQ0QCOCQ0:integer):TBytes;
function OOC0QCOCQ0:TBytes;
function OCC0QCOCQ0(const O00OQCOCQ0:TBytes;const OO0OQCOCQ0:array of byte;const OQ0OQCOCQ0:TBytes):TBytes;
function OC0OQCOCQ0:integer;
function O0OOQCOCQ0(const OOOOQCOCQ0:TBytes;OQOOQCOCQ0,OCOOQCOCQ0:integer;
const O0QOQCOCQ0,OOQOQCOCQ0:TBytes):TBytes;
public
function O00OCC00Q0(const OQCOQCOCQ0:TCipherDefinition;
const OCCOQCOCQ0:OQQC0Q00Q0):OCQ0O0Q0Q0;override;
procedure OC0OCC00Q0;override;
procedure OCC0CC00Q0;override;
procedure O0OOCC00Q0(const OCOQOQOCQ0:TBytes);override;
procedure OQOOCC00Q0(const OCQQOQOCQ0:TBytes);override;
procedure O0QOCC00Q0;override;
procedure OOQOCC00Q0;override;
procedure OQQOCC00Q0;override;
procedure OCQOCC00Q0;override;
procedure O0COCC00Q0(const O0OCOQOCQ0,OOOCOQOCQ0:TBytes;const OQOCOQOCQ0:array of byte);override;
procedure O00QQC00Q0(OCOCOQOCQ0:OCQC00Q0Q0;O0QCOQOCQ0:OC0Q00OOQ0);override;
function OCOQQC00Q0(OOQCOQOCQ0:OCQC00Q0Q0):OC0Q00OOQ0;override;
procedure OQ0CQC00Q0(O0CCOQOCQ0:OCQC00Q0Q0;OOCCOQOCQ0:TCRList);override;
procedure OOQCQC00Q0(OQ00OQOCQ0:OCQC00Q0Q0;OC00OQOCQ0:TCRList);override;
procedure OCCCQC00Q0(O0Q0OQOCQ0:OCQC00Q0Q0;OOQ0OQOCQ0:TScCertificate);override;
procedure OCC0QC00Q0(OC0OOQOCQ0:OCQC00Q0Q0;O0OOOQOCQ0:TScCertificate);override;
procedure OOOOQC00Q0(O0COOQOCQ0:OCQC00Q0Q0);override;
procedure OCOOQC00Q0(const OO0Q0QOCQ0:TBytes;OQ0Q0QOCQ0,OC0Q0QOCQ0:integer);override;
procedure OCQOQC00Q0(OQQQ0QOCQ0:OCQC00Q0Q0;OCQQ0QOCQ0:OQCCQOQ0Q0);override;
procedure OCCOQC00Q0(OOCQ0QOCQ0:OCQC00Q0Q0;OQCQ0QOCQ0:OQCCQOQ0Q0);override;
procedure OCQQOQ00Q0(OO0C0QOCQ0:OCQC00Q0Q0;const OQ0C0QOCQ0,OC0C0QOCQ0:TBytes);override;
procedure O0OQOQ00Q0(OQOC0QOCQ0:OCQC00Q0Q0;
OCOC0QOCQ0:OCQOC0Q0Q0;O0QC0QOCQ0:OQCCQOQ0Q0;
out OOQC0QOCQ0:integer);override;
function OCCQOQ00Q0:THashAlgorithm;override;
function O0000QOCQ0:THashAlgorithmClass;
function O00COQ00Q0:OOCCCQQ0Q0;override;
end;
implementation
uses
CRDECUtil,CRSymmetricAlgorithm,
{$IFNDEF UNIDACPRO}
TdsSSLConsts,TdsCertificateConsts;
{$ELSE}
TdsSSLConstsUni,TdsCertificateConstsUni;
{$ENDIF}
const
OO000QOCQ0:array[0..5]of byte=(116,108,115,49,51,32);
OQ000QOCQ0:array[0..2]of byte=(107,101,121);
OC000QOCQ0:array[0..1]of byte=(105,118);
O0O00QOCQ0:array[0..10]of byte=(116,114,97,102,102,105,99,32,117,112,100);
OOO00QOCQ0:array[0..7]of byte=(102,105,110,105,115,104,101,100);
OQO00QOCQ0:array[0..9]of byte=(114,101,115,32,98,105,110,100,101,114);
OCO00QOCQ0:array[0..6]of byte=(100,101,114,105,118,101,100);
O0Q00QOCQ0:array[0..11]of byte=(99,32,104,115,32,116,114,97,102,102,105,99);
OOQ00QOCQ0:array[0..11]of byte=(115,32,104,115,32,116,114,97,102,102,105,99);
OQQ00QOCQ0:array[0..11]of byte=(99,32,97,112,32,116,114,97,102,102,105,99);
OCQ00QOCQ0:array[0..11]of byte=(115,32,97,112,32,116,114,97,102,102,105,99);
O0C00QOCQ0:array[0..9]of byte=(114,101,115,32,109,97,115,116,101,114);
OOC00QOCQ0:array[0..9]of byte=(114,101,115,117,109,112,116,105,111,110);
OQC00QOCQ0:array[0..32]of byte=
(84,76,83,32,49,46,51,44,32,99,108,105,101,110,116,32,67,101,
114,116,105,102,105,99,97,116,101,86,101,114,105,102,121);
OCC00QOCQ0:array[0..32]of byte=
(84,76,83,32,49,46,51,44,32,115,101,114,118,101,114,32,67,101,
114,116,105,102,105,99,97,116,101,86,101,114,105,102,121);
constructor OQCOCCOCQ0.Create(OQOQQCOCQ0:THashAlgorithmClass;const OCOQQCOCQ0,O0QQQCOCQ0:TBytes);
begin
inherited Create;
if(OQOQQCOCQ0=nil)or(Length(OCOQQCOCQ0)=0)then
raise EScError.Create(seInvalidInputArgs);
OCCOCCOCQ0:=THMAC.Create(OQOQQCOCQ0,OCOQQCOCQ0);
SetLength(O00QQCOCQ0,Length(O0QQQCOCQ0)+1);
if Length(O0QQQCOCQ0)>0 then
Move(O0QQQCOCQ0[0],O00QQCOCQ0[0],Length(O0QQQCOCQ0));
O00QQCOCQ0[Length(O0QQQCOCQ0)]:=1;
OOCQQCOCQ0;
end;
destructor OQCOCCOCQ0.Destroy;
begin
OCCOCCOCQ0.Free;
FillChar(O00QQCOCQ0[0],Length(O00QQCOCQ0),0);
FillChar(OO0QQCOCQ0[0],Length(OO0QQCOCQ0),0);
FillChar(OC0QQCOCQ0[0],Length(OC0QQCOCQ0),0);
inherited;
end;
procedure OQCOCCOCQ0.OOCQQCOCQ0;
begin
O00QQCOCQ0[Length(O00QQCOCQ0)-1]:=0;
OC0QQCOCQ0:=nil;
OO0QQCOCQ0:=O0OQQCOCQ0;
OQ0QQCOCQ0:=0;
end;
function OQCOCCOCQ0.O0OQQCOCQ0:TBytes;
begin
O00QQCOCQ0[Length(O00QQCOCQ0)-1]:=O00QQCOCQ0[Length(O00QQCOCQ0)-1]+1;
if Length(OC0QQCOCQ0)>0 then
OCCOCCOCQ0.TransformBlock(OC0QQCOCQ0,0,Length(OC0QQCOCQ0));
OCCOCCOCQ0.TransformFinalBlock(O00QQCOCQ0,0,Length(O00QQCOCQ0));
OC0QQCOCQ0:=OCCOCCOCQ0.Hash;
OCCOCCOCQ0.Initialize;
Result:=OC0QQCOCQ0;
end;
function OQCOCCOCQ0.OQQQQCOCQ0(OCQQQCOCQ0:integer):TBytes;
var
O0CQQCOCQ0:integer;
begin
if OCQQQCOCQ0<0 then
raise EScError.Create(seInvalidInputArgs);
SetLength(Result,OCQQQCOCQ0);
O0CQQCOCQ0:=0;
while O0CQQCOCQ0<OCQQQCOCQ0 do begin
if(O0CQQCOCQ0+Length(OO0QQCOCQ0)-OQ0QQCOCQ0)>=OCQQQCOCQ0 then begin
Buffer.BlockCopy(OO0QQCOCQ0,OQ0QQCOCQ0,Result,O0CQQCOCQ0,OCQQQCOCQ0-O0CQQCOCQ0);
OQ0QQCOCQ0:=OQ0QQCOCQ0+OCQQQCOCQ0-O0CQQCOCQ0;
O0CQQCOCQ0:=Length(Result);
end
else begin
Buffer.BlockCopy(OO0QQCOCQ0,OQ0QQCOCQ0,Result,O0CQQCOCQ0,Length(OO0QQCOCQ0)-OQ0QQCOCQ0);
O0CQQCOCQ0:=O0CQQCOCQ0+Length(OO0QQCOCQ0)-OQ0QQCOCQ0;
OO0QQCOCQ0:=O0OQQCOCQ0;
OQ0QQCOCQ0:=0;
end;
end;
end;
function OQCQQCOCQ0.O00OCC00Q0(const OQCOQCOCQ0:TCipherDefinition;
const OCCOQCOCQ0:OQQC0Q00Q0):OCQ0O0Q0Q0;
var
O00QOQOCQ0,OO0QOQOCQ0:TSymmetricAlgorithm;
OQ0QOQOCQ0,OC0QOQOCQ0,O0OQOQOCQ0,OOOQOQOCQ0:TBytes;
begin
if OQCOQCOCQ0.CipherMode<>cmGCM then
raise EScError.Create(seInvalidInputArgs);
{$IFNDEF VER10P}
SetLength(OQ0QOQOCQ0,0);
SetLength(OC0QOQOCQ0,0);
SetLength(O0OQOQOCQ0,0);
SetLength(OOOQOQOCQ0,0);
{$ENDIF}
Result:=OCQ0O0Q0Q0.Create;
try
if(OCCOQCOCQ0=OCOC0Q00Q0)or(OCCOQCOCQ0=OOQC0Q00Q0)then begin
O00QOQOCQ0:=TSymmetricAlgorithm(OQCOQCOCQ0.CipherAlgorithmClass.Create);
try
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
OQ0QOQOCQ0:=OQO0QCOCQ0(O0OCQCOCQ0,OQ000QOCQ0,nil,OQCOQCOCQ0.KeyMaterialLength);
O0OQOQOCQ0:=OQO0QCOCQ0(O0OCQCOCQ0,OC000QOCQ0,nil,12);
end
else begin
OQ0QOQOCQ0:=OQO0QCOCQ0(OOOCQCOCQ0,OQ000QOCQ0,nil,OQCOQCOCQ0.KeyMaterialLength);
O0OQOQOCQ0:=OQO0QCOCQ0(OOOCQCOCQ0,OC000QOCQ0,nil,12);
end;
O00QOQOCQ0.Mode:=OQCOQCOCQ0.CipherMode;
O00QOQOCQ0.Key:=OQ0QOQOCQ0;
except
O00QOQOCQ0.Free;
raise;
end;
Result.O0C0O0Q0Q0:=O00QOQOCQ0;
Result.OQC0O0Q0Q0:=nil;
Result.O00OO0Q0Q0:=O0OQOQOCQ0;
FillChar(OQ0QOQOCQ0[0],Length(OQ0QOQOCQ0),0);
end;
if(OCCOQCOCQ0=O0QC0Q00Q0)or(OCCOQCOCQ0=OOQC0Q00Q0)then begin
OO0QOQOCQ0:=TSymmetricAlgorithm(OQCOQCOCQ0.CipherAlgorithmClass.Create);
try
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
OC0QOQOCQ0:=OQO0QCOCQ0(OOOCQCOCQ0,OQ000QOCQ0,nil,OQCOQCOCQ0.KeyMaterialLength);
OOOQOQOCQ0:=OQO0QCOCQ0(OOOCQCOCQ0,OC000QOCQ0,nil,12);
end
else begin
OC0QOQOCQ0:=OQO0QCOCQ0(O0OCQCOCQ0,OQ000QOCQ0,nil,OQCOQCOCQ0.KeyMaterialLength);
OOOQOQOCQ0:=OQO0QCOCQ0(O0OCQCOCQ0,OC000QOCQ0,nil,12);
end;
OO0QOQOCQ0.Mode:=OQCOQCOCQ0.CipherMode;
OO0QOQOCQ0.Key:=OC0QOQOCQ0;
except
OO0QOQOCQ0.Free;
raise;
end;
Result.OOC0O0Q0Q0:=OO0QOQOCQ0;
Result.OCC0O0Q0Q0:=nil;
Result.OO0OO0Q0Q0:=OOOQOQOCQ0;
FillChar(OC0QOQOCQ0[0],Length(OC0QOQOCQ0),0);
end;
except
Result.Free;
raise;
end;
end;
function OQCQQCOCQ0.OOCCQCOCQ0(const OQCCQCOCQ0,OCCCQCOCQ0:TBytes):TBytes;
var
O000QCOCQ0:THMAC;
begin
O000QCOCQ0:=THMAC.Create(O0000QOCQ0,OQCCQCOCQ0);
try
Result:=O000QCOCQ0.ComputeHash(OCCCQCOCQ0);
finally
O000QCOCQ0.Free;
end;
end;
function OQCQQCOCQ0.OO00QCOCQ0(const OQ00QCOCQ0,OC00QCOCQ0:TBytes;O0O0QCOCQ0:integer):TBytes;
var
OOO0QCOCQ0:OQCOCCOCQ0;
begin
OOO0QCOCQ0:=OQCOCCOCQ0.Create(O0000QOCQ0,OQ00QCOCQ0,OC00QCOCQ0);
try
Result:=OOO0QCOCQ0.OQQQQCOCQ0(O0O0QCOCQ0);
finally
OOO0QCOCQ0.Free;
end;
end;
function OQCQQCOCQ0.OQO0QCOCQ0(const OCO0QCOCQ0:TBytes;
const O0Q0QCOCQ0:array of byte;const OOQ0QCOCQ0:TBytes;OQQ0QCOCQ0:integer):TBytes;
var
OCQ0QCOCQ0:TBytes;
O0C0QCOCQ0:byte;
begin
if Length(OOQ0QCOCQ0)>255 then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidInputArgs);
O0C0QCOCQ0:=byte(6+Length(O0Q0QCOCQ0));
SetLength(OCQ0QCOCQ0,2+1+O0C0QCOCQ0+1+Length(OOQ0QCOCQ0));
OCQ0QCOCQ0[0]:=Byte(OQQ0QCOCQ0 shr 8);
OCQ0QCOCQ0[1]:=Byte(OQQ0QCOCQ0);
OCQ0QCOCQ0[2]:=O0C0QCOCQ0;
Move(OO000QOCQ0[0],OCQ0QCOCQ0[3],6);
Move(O0Q0QCOCQ0[0],OCQ0QCOCQ0[3+6],Length(O0Q0QCOCQ0));
OCQ0QCOCQ0[3+O0C0QCOCQ0]:=Byte(Length(OOQ0QCOCQ0));
if Length(OOQ0QCOCQ0)>0 then
Move(OOQ0QCOCQ0[0],OCQ0QCOCQ0[4+O0C0QCOCQ0],Length(OOQ0QCOCQ0));
Result:=OO00QCOCQ0(OCO0QCOCQ0,OCQ0QCOCQ0,OQQ0QCOCQ0);
end;
function OQCQQCOCQ0.OOC0QCOCQ0:TBytes;
var
OQC0QCOCQ0:THashAlgorithm;
begin
if Length(OCCQQCOCQ0)=0 then begin
OQC0QCOCQ0:=OCCQOQ00Q0;
try
OCCQQCOCQ0:=OQC0QCOCQ0.ComputeHash(nil,0,0);
finally
OQC0QCOCQ0.Free;
end;
end;
Result:=OCCQQCOCQ0;
end;
function OQCQQCOCQ0.OCC0QCOCQ0(const O00OQCOCQ0:TBytes;
const OO0OQCOCQ0:array of byte;const OQ0OQCOCQ0:TBytes):TBytes;
begin
Result:=OQO0QCOCQ0(O00OQCOCQ0,OO0OQCOCQ0,OQ0OQCOCQ0,Length(OQ0OQCOCQ0));
end;
function OQCQQCOCQ0.OC0OQCOCQ0:integer;
begin
Result:=OOCQCC0OQ0.OC00CC0OQ0(OO0C00Q0Q0[O00QCC00Q0].HashAlgorithm);
end;
procedure OQCQQCOCQ0.O0COCC00Q0(const O0OCOQOCQ0,OOOCOQOCQ0:TBytes;const OQOCOQOCQ0:array of byte);
begin
end;
procedure OQCQQCOCQ0.OC0OCC00Q0;
begin
O00CQCOCQ0:=OQC0CC00Q0;
end;
procedure OQCQQCOCQ0.OCC0CC00Q0;
var
OQOQOQOCQ0:OCQC00Q0Q0;
begin
OCQO0C00Q0:=0;
OQOQOQOCQ0:=OCQC00Q0Q0.Create;
try
OQOQOQOCQ0.OCQ000Q0Q0(O0OC0OQ0Q0);
OQOQOQOCQ0.OQC000Q0Q0(O00CQCOCQ0);
O0Q0CC00Q0(OQOQOQOCQ0);
finally
OQOQOQOCQ0.Free;
end;
end;
procedure OQCQQCOCQ0.O0OOCC00Q0(const OCOQOQOCQ0:TBytes);
var
O0QQOQOCQ0:TBytes;
OOQQOQOCQ0:boolean;
OQQQOQOCQ0:TBytes;
begin
OOQQOQOCQ0:=Length(OCOQOQOCQ0)>0;
if OOQQOQOCQ0 then
OQQQOQOCQ0:=OCOQOQOCQ0
else
SetLength(OQQQOQOCQ0,OC0OQCOCQ0);
O0QQOQOCQ0:=OOCCQCOCQ0(nil,OQQQOQOCQ0);
if OOQQOQOCQ0 then begin
OQ0CQCOCQ0:=OCC0QCOCQ0(O0QQOQOCQ0,OQO00QOCQ0,OOC0QCOCQ0);
OC0CQCOCQ0:=OQO0QCOCQ0(OQ0CQCOCQ0,OOO00QOCQ0,nil,OC0OQCOCQ0);
end;
OO0CQCOCQ0:=OCC0QCOCQ0(O0QQOQOCQ0,OCO00QOCQ0,OOC0QCOCQ0);
FillChar(O0QQOQOCQ0[0],Length(O0QQOQOCQ0),0);
end;
procedure OQCQQCOCQ0.OQOOCC00Q0(const OCQQOQOCQ0:TBytes);
var
O0CQOQOCQ0:TBytes;
OOCQOQOCQ0:TBytes;
begin
O0CQOQOCQ0:=OOCCQCOCQ0(OO0CQCOCQ0,OCQQOQOCQ0);
OOCQOQOCQ0:=OQC0CC00Q0;
OQOCQCOCQ0:=OCC0QCOCQ0(O0CQOQOCQ0,O0Q00QOCQ0,OOCQOQOCQ0);
O0OCQCOCQ0:=OQOCQCOCQ0;
OCOCQCOCQ0:=OCC0QCOCQ0(O0CQOQOCQ0,OOQ00QOCQ0,OOCQOQOCQ0);
OOOCQCOCQ0:=OCOCQCOCQ0;
OO0CQCOCQ0:=OCC0QCOCQ0(O0CQOQOCQ0,OCO00QOCQ0,OOC0QCOCQ0);
FillChar(O0CQOQOCQ0[0],Length(O0CQOQOCQ0),0);
end;
procedure OQCQQCOCQ0.O0QOCC00Q0;
var
OQCQOQOCQ0,OCCQOQOCQ0:TBytes;
O00COQOCQ0:TBytes;
begin
SetLength(OQCQOQOCQ0,OC0OQCOCQ0);
OCCQOQOCQ0:=OOCCQCOCQ0(OO0CQCOCQ0,OQCQOQOCQ0);
O00COQOCQ0:=OQC0CC00Q0;
O0QCQCOCQ0:=OCC0QCOCQ0(OCCQOQOCQ0,OQQ00QOCQ0,O00COQOCQ0);
O0OCQCOCQ0:=O0QCQCOCQ0;
OOQCQCOCQ0:=OCC0QCOCQ0(OCCQOQOCQ0,OCQ00QOCQ0,O00COQOCQ0);
OOOCQCOCQ0:=OOQCQCOCQ0;
OO0CQCOCQ0:=OCCQOQOCQ0;
end;
procedure OQCQQCOCQ0.OOQOCC00Q0;
var
OO0COQOCQ0:TBytes;
begin
OO0COQOCQ0:=OQC0CC00Q0;
OQQCQCOCQ0:=OCC0QCOCQ0(OO0CQCOCQ0,O0C00QOCQ0,OO0COQOCQ0);
FillChar(OO0CQCOCQ0[0],Length(OO0CQCOCQ0),0);
SetLength(OO0CQCOCQ0,0);
end;
procedure OQCQQCOCQ0.OQQOCC00Q0;
var
OQ0COQOCQ0:TBytes;
begin
OQ0COQOCQ0:=OQO0QCOCQ0(O0QCQCOCQ0,O0O00QOCQ0,nil,OC0OQCOCQ0);
FillChar(O0QCQCOCQ0[0],Length(O0QCQCOCQ0),0);
O0QCQCOCQ0:=OQ0COQOCQ0;
O0OCQCOCQ0:=OQ0COQOCQ0;
end;
procedure OQCQQCOCQ0.OCQOCC00Q0;
var
OC0COQOCQ0:TBytes;
begin
OC0COQOCQ0:=OQO0QCOCQ0(OOQCQCOCQ0,O0O00QOCQ0,nil,OC0OQCOCQ0);
FillChar(OOQCQCOCQ0[0],Length(OOQCQCOCQ0),0);
OOQCQCOCQ0:=OC0COQOCQ0;
OOOCQCOCQ0:=OC0COQOCQ0;
end;
procedure OQCQQCOCQ0.O00QQC00Q0(OCOCOQOCQ0:OCQC00Q0Q0;
O0QCOQOCQ0:OC0Q00OOQ0);
begin
SetLength(OCQCQCOCQ0,16);
OCCQ0QQ0Q0.Random(OCQCQCOCQ0,0,Length(OCQCQCOCQ0));
OCOCOQOCQ0.OQ0O00Q0Q0(OCQCQCOCQ0);
OCOCOQOCQ0.O00QC0Q0Q0(0);
end;
function OQCQQCOCQ0.OCOQQC00Q0(OOQCOQOCQ0:OCQC00Q0Q0):OC0Q00OOQ0;
var
OQQCOQOCQ0:integer;
OCQCOQOCQ0:OCQOC0Q0Q0;
begin
Result:=nil;
OQQCOQOCQ0:=OOQCOQOCQ0.O00CC0Q0Q0;
O0CCQCOCQ0:=OOQCOQOCQ0.OOOCC0Q0Q0(OQQCOQOCQ0);
OCQCOQOCQ0:=OCQOC0Q0Q0.Create;
try
OCQCOQOCQ0.O0OCQ0Q0Q0(OOQCOQOCQ0,OOQO0C00Q0);
finally
OCQCOQOCQ0.Free;
end;
end;
procedure OQCQQCOCQ0.OQ0CQC00Q0(O0CCOQOCQ0:OCQC00Q0Q0;
OOCCOQOCQ0:TCRList);
var
OQCCOQOCQ0:TScCertificate;
OCCCOQOCQ0:TBytes;
O000OQOCQ0,OO00OQOCQ0:integer;
begin
if OOCCOQOCQ0=nil then begin
O0CCOQOCQ0.OCQO00Q0Q0(0);
O0CCOQOCQ0.OOOQC0Q0Q0(0);
Exit;
end;
O0CCOQOCQ0.OQ0O00Q0Q0(O0CCQCOCQ0);
OO00OQOCQ0:=O0CCOQOCQ0.OQ00C0Q0Q0;
O0CCOQOCQ0.OQCQC0Q0Q0(3);
SetLength(OCCCOQOCQ0,0);
for O000OQOCQ0:=0 to OOCCOQOCQ0.Count-1 do begin
OQCCOQOCQ0:=TScCertificate(OOCCOQOCQ0[O000OQOCQ0]);
if OQCCOQOCQ0=nil then
Continue;
OCCCOQOCQ0:=OQCCOQOCQ0.GetRawData;
O0CCOQOCQ0.O0QO00Q0Q0(OCCCOQOCQ0);
O0CCOQOCQ0.O00QC0Q0Q0(0);
end;
O0CCOQOCQ0.OCOQC0Q0Q0(O0CCOQOCQ0.OQ00C0Q0Q0-OO00OQOCQ0-3,OO00OQOCQ0);
end;
procedure OQCQQCOCQ0.OOQCQC00Q0(OQ00OQOCQ0:OCQC00Q0Q0;
OC00OQOCQ0:TCRList);
var
O0O0OQOCQ0:TMemoryStream;
OOO0OQOCQ0:TScCertificate;
OQO0OQOCQ0:TBytes;
OCO0OQOCQ0:integer;
begin
OCO0OQOCQ0:=OQ00OQOCQ0.O00CC0Q0Q0;
OQO0OQOCQ0:=OQ00OQOCQ0.OOOCC0Q0Q0(OCO0OQOCQ0);
if Length(OQO0OQOCQ0)<>Length(OCQCQCOCQ0)then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
if(Length(OQO0OQOCQ0)>0)and
(MemCompare(@OQO0OQOCQ0[0],@OCQCQCOCQ0[0],Length(OQO0OQOCQ0))<>0)
then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
OCO0OQOCQ0:=OQ00OQOCQ0.OQ0CC0Q0Q0;
if OCO0OQOCQ0>OQ00OQOCQ0.OOCCC0Q0Q0 then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
O0O0OQOCQ0:=TMemoryStream.Create;
try
while OQ00OQOCQ0.OOCCC0Q0Q0>0 do begin
OCO0OQOCQ0:=OQ00OQOCQ0.OQ0CC0Q0Q0;
if OCO0OQOCQ0>OQ00OQOCQ0.OOCCC0Q0Q0 then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
O0O0OQOCQ0.Position:=0;
O0O0OQOCQ0.WriteBuffer(OQ00OQOCQ0.OCCCC0Q0Q0[OQ00OQOCQ0.OO00C0Q0Q0],OCO0OQOCQ0);
OQ00OQOCQ0.OCQCC0Q0Q0(OCO0OQOCQ0);
O0O0OQOCQ0.Size:=OCO0OQOCQ0;
O0O0OQOCQ0.Position:=0;
OOO0OQOCQ0:=TScCertificate.Create(nil);
try
OOO0OQOCQ0.ImportFrom(O0O0OQOCQ0);
OOO0OQOCQ0.CertName:=IntToStr(OC00OQOCQ0.Count);
OC00OQOCQ0.Add(OOO0OQOCQ0);
except
OOO0OQOCQ0.Free;
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
end;
OCO0OQOCQ0:=OQ00OQOCQ0.OO0CC0Q0Q0;
OQ00OQOCQ0.OCQCC0Q0Q0(OCO0OQOCQ0);
end;
finally
O0O0OQOCQ0.Free;
end;
end;
procedure OQCQQCOCQ0.OCCCQC00Q0(O0Q0OQOCQ0:OCQC00Q0Q0;
OOQ0OQOCQ0:TScCertificate);
var
OQQ0OQOCQ0:OO00CCO0Q0;
OCQ0OQOCQ0,O0C0OQOCQ0:OOOCQQQ0Q0;
OOC0OQOCQ0:OOOOCOQOQ0;
OQC0OQOCQ0,OCC0OQOCQ0,O00OOQOCQ0:TBytes;
OO0OOQOCQ0:integer;
OQ0OOQOCQ0:integer;
begin
if OOQ0OQOCQ0=nil then
raise EScError.Create(seInvalidInputArgs);
O0C0OQOCQ0:=OCCQQQQ0Q0;
case OOQ0OQOCQ0.Key.OQOOOCQ0Q0 of
OCOQQOQOQ0:
if OOQ0OQOCQ0.SignatureAlgorithm.OC0CCOOOQ0.OCOO0OOOQ0=OID_RSA_PSS_ENCRYPTION then
O0C0OQOCQ0:=OCCQQQQ0Q0
else
O0C0OQOCQ0:=O0CQQQQ0Q0;
O0QQQOQOQ0:
O0C0OQOCQ0:=OOQQQQQ0Q0;
else
O0QO0C00Q0.OOC00C00Q0(OCO00OQ0Q0,seCannotSignData);
end;
if OOQO0C00Q0=OQOQOOQ0Q0 then begin
OQQ0OQOCQ0:=OO00CCO0Q0(O0QO0C00Q0.OQ0O0C00Q0.OQOOOCO0Q0.OOCCQ0Q0Q0(OO00CCO0Q0));
if OQQ0OQOCQ0<>nil then begin
for OQ0OOQOCQ0:=0 to OQQ0OQOCQ0.O00OCCO0Q0-1 do begin
OCQ0OQOCQ0:=OQQ0OQOCQ0.OO0OCCO0Q0[OQ0OOQOCQ0];
if(OOQ0OQOCQ0.Key.OQOOOCQ0Q0=OQC0OQO0Q0[OCQ0OQOCQ0].Signature)and
(OQC0OQO0Q0[OCQ0OQOCQ0].Hash<>OOC0COQOQ0)
then begin
O0C0OQOCQ0:=OCQ0OQOCQ0;
Break;
end;
end;
end;
end;
O00OOQOCQ0:=OQC0CC00Q0;
OO0OOQOCQ0:=Length(OQC00QOCQ0);
SetLength(OCC0OQOCQ0,64+OO0OOQOCQ0+1+Length(O00OOQOCQ0));
FillChar(OCC0OQOCQ0[0],64,$20);
if OOQO0C00Q0=OOOQOOQ0Q0 then
Move(OQC00QOCQ0[0],OCC0OQOCQ0[64],OO0OOQOCQ0)
else
Move(OCC00QOCQ0[0],OCC0OQOCQ0[64],OO0OOQOCQ0);
Move(O00OOQOCQ0[0],OCC0OQOCQ0[64+OO0OOQOCQ0+1],Length(O00OOQOCQ0));
OOC0OQOCQ0:=OQC0OQO0Q0[O0C0OQOCQ0].Hash;
if OQC0OQO0Q0[O0C0OQOCQ0].Padding=O00Q0OOOQ0 then begin
OOQ0OQOCQ0.Key.OQCOOCQ0Q0.O0OC0OOOQ0:=OOC0OQOCQ0;
OOQ0OQOCQ0.Key.OQCOOCQ0Q0.OOOC0OOOQ0:=OOC0OQOCQ0;
OOQ0OQOCQ0.Key.OQCOOCQ0Q0.OQOC0OOOQ0:=OOCQCC0OQ0.OC00CC0OQ0(OOC0OQOCQ0);
end;
OQC0OQOCQ0:=OOQ0OQOCQ0.Sign(OCC0OQOCQ0,OOC0OQOCQ0,OQC0OQO0Q0[O0C0OQOCQ0].Padding);
O0Q0OQOCQ0.O00QC0Q0Q0(OQQQO0Q0Q0[O0C0OQOCQ0]);
O0Q0OQOCQ0.OOOO00Q0Q0(OQC0OQOCQ0);
end;
procedure OQCQQCOCQ0.OCC0QC00Q0(OC0OOQOCQ0:OCQC00Q0Q0;
O0OOOQOCQ0:TScCertificate);
var
OOOOOQOCQ0:OOOCQQQ0Q0;
OQOOOQOCQ0:OOOOCOQOQ0;
OCOOOQOCQ0,O0QOOQOCQ0,OOQOOQOCQ0:TBytes;
OQQOOQOCQ0,OCQOOQOCQ0:integer;
begin
if O0OOOQOCQ0=nil then
raise EScError.Create(sePeerCertificateNotReceived);
OOOOOQOCQ0:=OC0OO0Q0Q0.OC0Q00Q0Q0(OC0OOQOCQ0.OO0CC0Q0Q0);
if OOOOOQOCQ0=OOOCQQQ0Q0(-1)then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seInvalidSignatureSchemeAlgorithm);
OCQOOQOCQ0:=OC0OOQOCQ0.OO0CC0Q0Q0;
if OCQOOQOCQ0=0 then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
OCOOOQOCQ0:=OC0OOQOCQ0.OOOCC0Q0Q0(OCQOOQOCQ0);
if OQC0OQO0Q0[OOOOOQOCQ0].Signature<>O0OOOQOCQ0.Key.OQOOOCQ0Q0 then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seCertificateNotCorrespondToRequiredSignatureAlgorithms);
if OQC0OQO0Q0[OOOOOQOCQ0].Hash=OOC0COQOQ0 then
O0QO0C00Q0.OOC00C00Q0(OQ0O0OQ0Q0,seInvalidHashAlgorithm);
OOQOOQOCQ0:=OQC0CC00Q0;
OQQOOQOCQ0:=Length(OQC00QOCQ0);
SetLength(O0QOOQOCQ0,64+OQQOOQOCQ0+1+Length(OOQOOQOCQ0));
FillChar(O0QOOQOCQ0[0],64,$20);
if OOQO0C00Q0=OOOQOOQ0Q0 then
Move(OCC00QOCQ0[0],O0QOOQOCQ0[64],OQQOOQOCQ0)
else
Move(OQC00QOCQ0[0],O0QOOQOCQ0[64],OQQOOQOCQ0);
Move(OOQOOQOCQ0[0],O0QOOQOCQ0[64+OQQOOQOCQ0+1],Length(OOQOOQOCQ0));
OQOOOQOCQ0:=OQC0OQO0Q0[OOOOOQOCQ0].Hash;
if OQC0OQO0Q0[OOOOOQOCQ0].Padding=O00Q0OOOQ0 then begin
O0OOOQOCQ0.Key.OQCOOCQ0Q0.O0OC0OOOQ0:=OQOOOQOCQ0;
O0OOOQOCQ0.Key.OQCOOCQ0Q0.OOOC0OOOQ0:=OQOOOQOCQ0;
O0OOOQOCQ0.Key.OQCOOCQ0Q0.OQOC0OOOQ0:=OOCQCC0OQ0.OC00CC0OQ0(OQOOOQOCQ0);
end;
if not O0OOOQOCQ0.VerifySign(O0QOOQOCQ0,OCOOOQOCQ0,OQOOOQOCQ0,OQC0OQO0Q0[OOOOOQOCQ0].Padding)then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seHashVerificationNotCorrespond);
end;
procedure OQCQQCOCQ0.OOOOQC00Q0(O0COOQOCQ0:OCQC00Q0Q0);
var
OOCOOQOCQ0:THMAC;
OQCOOQOCQ0,OCCOOQOCQ0,O00Q0QOCQ0:TBytes;
begin
OQCOOQOCQ0:=OQC0CC00Q0;
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
if Length(OQOCQCOCQ0)>0 then
OCCOOQOCQ0:=OQOCQCOCQ0
else
OCCOOQOCQ0:=O0OCQCOCQ0;
end
else begin
if Length(OCOCQCOCQ0)>0 then
OCCOOQOCQ0:=OCOCQCOCQ0
else
OCCOOQOCQ0:=OOOCQCOCQ0;
end;
O00Q0QOCQ0:=OQO0QCOCQ0(OCCOOQOCQ0,OOO00QOCQ0,nil,Length(OQCOOQOCQ0));
OOCOOQOCQ0:=THMAC.Create(O0000QOCQ0,O00Q0QOCQ0);
try
O0COOQOCQ0.OQC000Q0Q0(OOCOOQOCQ0.ComputeHash(OQCOOQOCQ0));
finally
OOCOOQOCQ0.Free;
end;
if OOQO0C00Q0=OOOQOOQ0Q0 then
SetLength(OQOCQCOCQ0,0)
else
SetLength(OCOCQCOCQ0,0);
end;
procedure OQCQQCOCQ0.OCOOQC00Q0(const OO0Q0QOCQ0:TBytes;OQ0Q0QOCQ0,OC0Q0QOCQ0:integer);
var
O0OQ0QOCQ0:THMAC;
OOOQ0QOCQ0,OQOQ0QOCQ0,OCOQ0QOCQ0,O0QQ0QOCQ0:TBytes;
OOQQ0QOCQ0:integer;
begin
OOOQ0QOCQ0:=OQC0CC00Q0;
if OOQO0C00Q0=OOOQOOQ0Q0 then begin
if Length(OCOCQCOCQ0)>0 then
OQOQ0QOCQ0:=OCOCQCOCQ0
else
OQOQ0QOCQ0:=OOOCQCOCQ0;
end
else begin
if Length(OQOCQCOCQ0)>0 then
OQOQ0QOCQ0:=OQOCQCOCQ0
else
OQOQ0QOCQ0:=O0OCQCOCQ0;
end;
OCOQ0QOCQ0:=OQO0QCOCQ0(OQOQ0QOCQ0,OOO00QOCQ0,nil,Length(OOOQ0QOCQ0));
O0OQ0QOCQ0:=THMAC.Create(O0000QOCQ0,OCOQ0QOCQ0);
try
O0QQ0QOCQ0:=O0OQ0QOCQ0.ComputeHash(OOOQ0QOCQ0);
finally
O0OQ0QOCQ0.Free;
end;
if OOQO0C00Q0=OOOQOOQ0Q0 then
SetLength(OCOCQCOCQ0,0)
else
SetLength(OQOCQCOCQ0,0);
if Length(O0QQ0QOCQ0)<>OC0Q0QOCQ0 then
O0QO0C00Q0.OOC00C00Q0(OQC00OQ0Q0,seInvalidMessage);
for OOQQ0QOCQ0:=0 to OC0Q0QOCQ0-1 do
if O0QQ0QOCQ0[OOQQ0QOCQ0]<>OO0Q0QOCQ0[OQ0Q0QOCQ0+OOQQ0QOCQ0]then
O0QO0C00Q0.OOC00C00Q0(OCC00OQ0Q0,seHashVerificationNotCorrespond);
end;
procedure OQCQQCOCQ0.OCQOQC00Q0(
OQQQ0QOCQ0:OCQC00Q0Q0;OCQQ0QOCQ0:OQCCQOQ0Q0);
var
O0CQ0QOCQ0:OQQ0Q0Q0Q0;
begin
OCQQ0QOCQ0.OCQOQOQ0Q0:=OQQCQCOCQ0;
O0CQ0QOCQ0:=OQQQCC00Q0(OCQQ0QOCQ0);
try
OQQQ0QOCQ0.OQQQC0Q0Q0(O0CQ0QOCQ0.OCQQQOQ0Q0);
OQQQ0QOCQ0.OQQQC0Q0Q0(O0CQ0QOCQ0.O0CQQOQ0Q0);
OQQQ0QOCQ0.OQ0O00Q0Q0(O0CQ0QOCQ0.OOCQQOQ0Q0);
OQQQ0QOCQ0.OOOO00Q0Q0(O0CQ0QOCQ0.OQCQQOQ0Q0);
OQQQ0QOCQ0.O00QC0Q0Q0(0);
finally
O0CQ0QOCQ0.Free;
end;
end;
procedure OQCQQCOCQ0.OCCOQC00Q0(
OOCQ0QOCQ0:OCQC00Q0Q0;OQCQ0QOCQ0:OQCCQOQ0Q0);
var
OCCQ0QOCQ0:OQQ0Q0Q0Q0;
O00C0QOCQ0:integer;
begin
OCCQ0QOCQ0:=OQQ0Q0Q0Q0.Create;
OQCQ0QOCQ0.OQCOQOQ0Q0.OQOCQOQ0Q0(OCCQ0QOCQ0);
OCCQ0QOCQ0.OCQQQOQ0Q0:=OOCQ0QOCQ0.OC0CC0Q0Q0;
OCCQ0QOCQ0.O0CQQOQ0Q0:=OOCQ0QOCQ0.OC0CC0Q0Q0;
O00C0QOCQ0:=OOCQ0QOCQ0.O00CC0Q0Q0;
OCCQ0QOCQ0.OOCQQOQ0Q0:=OOCQ0QOCQ0.OOOCC0Q0Q0(O00C0QOCQ0);
O00C0QOCQ0:=OOCQ0QOCQ0.OO0CC0Q0Q0;
OCCQ0QOCQ0.OQCQQOQ0Q0:=OOCQ0QOCQ0.OOOCC0Q0Q0(O00C0QOCQ0);
OCCQ0QOCQ0.OQC0Q0Q0Q0.O0OCQ0Q0Q0(OOCQ0QOCQ0,OOQO0C00Q0);
OCCQ0QOCQ0.OQQQQOQ0Q0:=OCO0OCOOQ0;
OQCQ0QOCQ0.OCQOQOQ0Q0:=OQQCQCOCQ0;
end;
function OQCQQCOCQ0.O0OOQCOCQ0(const OOOOQCOCQ0:TBytes;OQOOQCOCQ0,OCOOQCOCQ0:integer;
const O0QOQCOCQ0,OOQOQCOCQ0:TBytes):TBytes;
var
OQQOQCOCQ0:THashAlgorithm;
OCQOQCOCQ0:THMAC;
O0COQCOCQ0,OOCOQCOCQ0:TBytes;
begin
O0COQCOCQ0:=OQO0QCOCQ0(O0QOQCOCQ0,OOC00QOCQ0,OOQOQCOCQ0,OC0OQCOCQ0);
O0OOCC00Q0(O0COQCOCQ0);
OQQOQCOCQ0:=OCCQOQ00Q0;
try
OOCOQCOCQ0:=OQQOQCOCQ0.ComputeHash(TValueArr(OOOOQCOCQ0),OQOOQCOCQ0,OCOOQCOCQ0);
finally
OQQOQCOCQ0.Free;
end;
OCQOQCOCQ0:=THMAC.Create(O0000QOCQ0,OC0CQCOCQ0);
try
Result:=OCQOQCOCQ0.ComputeHash(OOCOQCOCQ0);
finally
OCQOQCOCQ0.Free;
end;
end;
procedure OQCQQCOCQ0.OCQQOQ00Q0(OO0C0QOCQ0:OCQC00Q0Q0;
const OQ0C0QOCQ0,OC0C0QOCQ0:TBytes);
var
O0OC0QOCQ0:TBytes;
OOOC0QOCQ0:integer;
begin
OOOC0QOCQ0:=OO0C0QOCQ0.O000C0Q0Q0-OCOC00Q0Q0;
O0OC0QOCQ0:=O0OOQCOCQ0(OO0C0QOCQ0.OCCCC0Q0Q0,OOOC0QOCQ0,
OO0C0QOCQ0.OQ00C0Q0Q0-OC0OQCOCQ0-3-OOOC0QOCQ0,OQ0C0QOCQ0,OC0C0QOCQ0);
Move(O0OC0QOCQ0[0],OO0C0QOCQ0.OCCCC0Q0Q0[OO0C0QOCQ0.OQ00C0Q0Q0-Length(O0OC0QOCQ0)],Length(O0OC0QOCQ0));
end;
procedure OQCQQCOCQ0.O0OQOQ00Q0(OQOC0QOCQ0:OCQC00Q0Q0;
OCOC0QOCQ0:OCQOC0Q0Q0;O0QC0QOCQ0:OQCCQOQ0Q0;
out OOQC0QOCQ0:integer);
var
OQQC0QOCQ0:O00QQOQ0Q0;
OCQC0QOCQ0:OO0QOQO0Q0;
O0CC0QOCQ0:TBytes;
OOCC0QOCQ0:integer;
OQCC0QOCQ0:integer;
OCCC0QOCQ0:integer;
begin
SetLength(O0CC0QOCQ0,0);
OCQC0QOCQ0:=OO0QOQO0Q0(OCOC0QOCQ0.OOCCQ0Q0Q0(OO0QOQO0Q0));
for OCCC0QOCQ0:=0 to OCQC0QOCQ0.OCOCOQO0Q0-1 do begin
OQQC0QOCQ0:=OCQC0QOCQ0.O0QCOQO0Q0[OCCC0QOCQ0];
if not O0OCCC00Q0(OQQC0QOCQ0,O0QC0QOCQ0)then
continue;
O0QO0C00Q0.OC0O0C00Q0.OOOOQOQ0Q0:=O0QC0QOCQ0.OOOOQOQ0Q0;
OOCC0QOCQ0:=OCQC0QOCQ0.OCOCOQO0Q0*(OC0OQCOCQ0+1)+2;
OQCC0QOCQ0:=OQOC0QOCQ0.O000C0Q0Q0-OCOC00Q0Q0;
O0CC0QOCQ0:=O0OOQCOCQ0(OQOC0QOCQ0.OCCCC0Q0Q0,OQCC0QOCQ0,
OQOC0QOCQ0.OO00C0Q0Q0-OOCC0QOCQ0-OQCC0QOCQ0,O0QC0QOCQ0.OCQOQOQ0Q0,O0QC0QOCQ0.OOCOQOQ0Q0);
if(Length(OQQC0QOCQ0.OCCQQOQ0Q0)=Length(O0CC0QOCQ0))and
(MemCompare(@OQQC0QOCQ0.OCCQQOQ0Q0[0],@O0CC0QOCQ0[0],Length(O0CC0QOCQ0))=0)then
begin
OOQC0QOCQ0:=OCCC0QOCQ0;
Exit;
end;
SetLength(OO0CQCOCQ0,0);
end;
OOQC0QOCQ0:=-1;
end;
function OQCQQCOCQ0.OCCQOQ00Q0:THashAlgorithm;
begin
Result:=O0000QOCQ0.Create;
end;
function OQCQQCOCQ0.O0000QOCQ0:THashAlgorithmClass;
begin
if OO0C00Q0Q0[O00QCC00Q0].HashAlgorithm=OO0OCOQOQ0 then
Result:=THash_SHA2_384
else
Result:=THash_SHA2_256;
end;
function OQCQQCOCQ0.O00COQ00Q0:OOCCCQQ0Q0;
begin
Result:=OCQCCQQ0Q0;
end;
end.
