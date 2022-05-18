//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsAlgorithmSupportUni;
interface
uses
SysUtils,
CRTypes,CRSymmetricAlgorithm,CRHashAlgorithm,CRCryptoTransformIntf,
{$IFNDEF UNIDACPRO}
TdsUtils;
{$ELSE}
TdsUtilsUni;
{$ENDIF}
type
OOCQCC0OQ0=class
public
class function OQCQCC0OQ0(OCCQCC0OQ0:OO00COQOQ0;
const O00CCC0OQ0,OO0CCC0OQ0:TBytes):TSymmetricAlgorithm;
class function OQ0CCC0OQ0(OC0CCC0OQ0:OCQ0COQOQ0;
O0OCCC0OQ0:TCipherMode):TSymmetricAlgorithm;
class function OOOCCC0OQ0(OQOCCC0OQ0:OOOOCOQOQ0):THashAlgorithm;
class function OCOCCC0OQ0(O0QCCC0OQ0:OOOOCOQOQ0):THashAlgorithmClass;
class function OOQCCC0OQ0(OQQCCC0OQ0:O0COCOQOQ0;const OCQCCC0OQ0:TBytes):IHashTransform;
class function OOCCCC0OQ0(OQCCCC0OQ0:O0COCOQOQ0):Integer;
class function OCCCCC0OQ0(O000CC0OQ0:OO00COQOQ0):Integer;
class function OO00CC0OQ0(OQ00CC0OQ0:OO00COQOQ0):Integer;
class function OC00CC0OQ0(O0O0CC0OQ0:OOOOCOQOQ0):Integer;
class function OOO0CC0OQ0(OQO0CC0OQ0:OO00COQOQ0):string;
class function OCO0CC0OQ0(const O0Q0CC0OQ0:string):OO00COQOQ0;
class function OOQ0CC0OQ0(OQQ0CC0OQ0:OOQQQOQOQ0):string;
class function OCQ0CC0OQ0(O0C0CC0OQ0:OOQQQOQOQ0;OOC0CC0OQ0:OOQOQOQOQ0):string;
class function OCC0CC0OQ0(O00OCC0OQ0:OOQQQOQOQ0):string;
class function OO0OCC0OQ0(OQ0OCC0OQ0:OOQQQOQOQ0;OC0OCC0OQ0:OOQOQOQOQ0=OCCCQOQOQ0):OOOOCOQOQ0;
class function OOOOCC0OQ0(const OQOOCC0OQ0:string):OOQQQOQOQ0;
class function OCOOCC0OQ0(const O0QOCC0OQ0:string):OOOOCOQOQ0;
class function OOQOCC0OQ0(OQQOCC0OQ0:OOQQQOQOQ0):string;
class function OCQOCC0OQ0(const O0COCC0OQ0:string):OOQQQOQOQ0;
class function OOCOCC0OQ0(OQCOCC0OQ0:OOOOCOQOQ0):string;
class function OCCOCC0OQ0(const O00QQC0OQ0:string):OOOOCOQOQ0;
class function OO0QQC0OQ0(const OQ0QQC0OQ0:string):O0COCOQOQ0;
class function OC0QQC0OQ0(O0OQQC0OQ0:O0COCOQOQ0):string;
class function OOOQQC0OQ0(OQOQQC0OQ0:OOCOQOQOQ0):string;
class function OCOQQC0OQ0(const O0QQQC0OQ0:string):OOCOQOQOQ0;
class function OOQQQC0OQ0(OQQQQC0OQ0:OOOQQOQOQ0):string;
class function OCQQQC0OQ0(const O0CQQC0OQ0:string):OOOQQOQOQ0;
class function OOCQQC0OQ0(const OQCQQC0OQ0:string):OOOOCOQOQ0;
class function OCCQQC0OQ0(const O00CQC0OQ0:OOOOCOQOQ0):string;
class function OO0CQC0OQ0(const OQ0CQC0OQ0:string):OQCCQOQOQ0;
class function OC0CQC0OQ0(const O0OCQC0OQ0:OQCCQOQOQ0):string;
class function OOOCQC0OQ0(const OQOCQC0OQ0:OQCCQOQOQ0):OOOOCOQOQ0;
class function OCOCQC0OQ0(const O0QCQC0OQ0:string):OO00COQOQ0;
class function OOQCQC0OQ0(const OQQCQC0OQ0:OO00COQOQ0):string;
end;
implementation
uses
CRCipher,CRHash,CRHMAC,
{$IFNDEF UNIDACPRO}
TdsSSLConsts,TdsCertificateConsts,TdsCipherSuites;
{$ELSE}
TdsSSLConstsUni,TdsCertificateConstsUni,TdsCipherSuitesUni;
{$ENDIF}
class function OOCQCC0OQ0.OQCQCC0OQ0(OCCQCC0OQ0:OO00COQOQ0;
const O00CCC0OQ0,OO0CCC0OQ0:TBytes):TSymmetricAlgorithm;
begin
case OCCQCC0OQ0 of
OOOCCOQOQ0:begin
Result:=TCipher_3DES.Create;
Result.Mode:=cmCBC;
Result.Key:=O00CCC0OQ0;
Result.IV:=OO0CCC0OQ0;
end;
OQOCCOQOQ0:begin
Result:=TCipher_Blowfish.Create;
Result.Mode:=cmCBC;
Result.Key:=O00CCC0OQ0;
Result.IV:=OO0CCC0OQ0;
end;
OCOCCOQOQ0,O0QCCOQOQ0,OOQCCOQOQ0:begin
Result:=TCipher_Rijndael.Create;
Result.Mode:=cmCBC;
Result.Key:=O00CCC0OQ0;
Result.IV:=OO0CCC0OQ0;
end;
OQQCCOQOQ0:begin
Result:=TCipher_Cast128.Create;
Result.Mode:=cmCBC;
Result.Key:=O00CCC0OQ0;
Result.IV:=OO0CCC0OQ0;
end;
OCQCCOQOQ0:begin
Result:=TCipher_3DES.Create;
Result.Mode:=cmCTR;
Result.Key:=O00CCC0OQ0;
Result.IV:=OO0CCC0OQ0;
end;
O0CCCOQOQ0:begin
Result:=TCipher_Blowfish.Create;
Result.Mode:=cmCTR;
Result.Key:=O00CCC0OQ0;
Result.IV:=OO0CCC0OQ0;
end;
OOCCCOQOQ0,OQCCCOQOQ0,OCCCCOQOQ0:begin
Result:=TCipher_Rijndael.Create;
Result.Mode:=cmCTR;
Result.Key:=O00CCC0OQ0;
Result.IV:=OO0CCC0OQ0;
end;
O000COQOQ0:begin
Result:=TCipher_Cast128.Create;
Result.Mode:=cmCTR;
Result.Key:=O00CCC0OQ0;
Result.IV:=OO0CCC0OQ0;
end;
else
raise EScError.Create(seInvalidCipherAlgorithm);
end;
end;
class function OOCQCC0OQ0.OQ0CCC0OQ0(OC0CCC0OQ0:OCQ0COQOQ0;
O0OCCC0OQ0:TCipherMode):TSymmetricAlgorithm;
begin
case OC0CCC0OQ0 of
OQ00COQOQ0:
Result:=TCipher_1DES.Create;
OC00COQOQ0:
Result:=TCipher_3DES.Create;
O0O0COQOQ0:
Result:=TCipher_Blowfish.Create;
OOO0COQOQ0,OQO0COQOQ0,OCO0COQOQ0:
Result:=TCipher_Rijndael.Create;
O0Q0COQOQ0:
Result:=TCipher_Cast128.Create;
OOQ0COQOQ0:
Result:=TCipher_RC2.Create;
OQQ0COQOQ0:
Result:=TCipher_RC4.Create;
else
raise EScError.Create(seInvalidCipherAlgorithm);
end;
Result.Mode:=O0OCCC0OQ0;
end;
class function OOCQCC0OQ0.OOOCCC0OQ0(OQOCCC0OQ0:OOOOCOQOQ0):THashAlgorithm;
begin
case OQOCCC0OQ0 of
OOC0COQOQ0:
Result:=THash_SHA1.Create;
OQC0COQOQ0:
Result:=THash_SHA2_256.Create;
OCC0COQOQ0:
Result:=THash_SHA2_512.Create;
O00OCOQOQ0:
Result:=THash_SHA2_224.Create;
OO0OCOQOQ0:
Result:=THash_SHA2_384.Create;
O0OOCOQOQ0:
Result:=THash_MD2.Create;
OC0OCOQOQ0:
Result:=THash_MD4.Create;
OQ0OCOQOQ0:
Result:=THash_MD5.Create;
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCOCCC0OQ0(O0QCCC0OQ0:OOOOCOQOQ0):THashAlgorithmClass;
begin
case O0QCCC0OQ0 of
OOC0COQOQ0:
Result:=THash_SHA1;
OQC0COQOQ0:
Result:=THash_SHA2_256;
OCC0COQOQ0:
Result:=THash_SHA2_512;
O00OCOQOQ0:
Result:=THash_SHA2_224;
OO0OCOQOQ0:
Result:=THash_SHA2_384;
O0OOCOQOQ0:
Result:=THash_MD2;
OC0OCOQOQ0:
Result:=THash_MD4;
OQ0OCOQOQ0:
Result:=THash_MD5;
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
end;
class function OOCQCC0OQ0.OOQCCC0OQ0(OQQCCC0OQ0:O0COCOQOQ0;const OCQCCC0OQ0:TBytes):IHashTransform;
var
O0CCCC0OQ0:THashAlgorithmClass;
begin
case OQQCCC0OQ0 of
OQOOCOQOQ0:
O0CCCC0OQ0:=THash_SHA1;
OCOOCOQOQ0:
O0CCCC0OQ0:=THash_SHA2_256;
O0QOCOQOQ0:
O0CCCC0OQ0:=THash_SHA2_512;
OOQOCOQOQ0:
O0CCCC0OQ0:=THash_SHA2_224;
OQQOCOQOQ0:
O0CCCC0OQ0:=THash_SHA2_384;
OCQOCOQOQ0:
O0CCCC0OQ0:=THash_MD5;
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
Result:=THMAC.Create(O0CCCC0OQ0,OCQCCC0OQ0);
end;
class function OOCQCC0OQ0.OOCCCC0OQ0(OQCCCC0OQ0:O0COCOQOQ0):Integer;
begin
case OQCCCC0OQ0 of
OQOOCOQOQ0:
Result:=THash_SHA1.GetHashSize;
OCOOCOQOQ0:
Result:=THash_SHA2_256.GetHashSize;
O0QOCOQOQ0:
Result:=THash_SHA2_512.GetHashSize;
OOQOCOQOQ0:
Result:=THash_SHA2_224.GetHashSize;
OQQOCOQOQ0:
Result:=THash_SHA2_384.GetHashSize;
OCQOCOQOQ0:
Result:=THash_MD5.GetHashSize;
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCCCCC0OQ0(O000CC0OQ0:OO00COQOQ0):Integer;
begin
case O000CC0OQ0 of
OOOCCOQOQ0,OCQCCOQOQ0:
Result:=24;
OQOCCOQOQ0:
Result:=16;
O0CCCOQOQ0:
Result:=32;
OCOCCOQOQ0,OOCCCOQOQ0:
Result:=16;
O0QCCOQOQ0,OQCCCOQOQ0:
Result:=24;
OOQCCOQOQ0,OCCCCOQOQ0:
Result:=32;
OQQCCOQOQ0,O000COQOQ0:
Result:=16;
else
raise EScError.Create(seInvalidCipherAlgorithm);
end;
end;
class function OOCQCC0OQ0.OO00CC0OQ0(OQ00CC0OQ0:OO00COQOQ0):Integer;
begin
case OQ00CC0OQ0 of
OOOCCOQOQ0,OCQCCOQOQ0:
Result:=8;
OQOCCOQOQ0,O0CCCOQOQ0:
Result:=8;
OCOCCOQOQ0,O0QCCOQOQ0,OOQCCOQOQ0:
Result:=16;
OOCCCOQOQ0,OQCCCOQOQ0,OCCCCOQOQ0:
Result:=16;
OQQCCOQOQ0,O000COQOQ0:
Result:=8;
else
raise EScError.Create(seInvalidCipherAlgorithm);
end;
end;
class function OOCQCC0OQ0.OC00CC0OQ0(O0O0CC0OQ0:OOOOCOQOQ0):Integer;
begin
case O0O0CC0OQ0 of
O0C0COQOQ0:
Result:=0;
OOC0COQOQ0:
Result:=THash_SHA1.GetHashSize;
OQC0COQOQ0:
Result:=THash_SHA2_256.GetHashSize;
OCC0COQOQ0:
Result:=THash_SHA2_512.GetHashSize;
O00OCOQOQ0:
Result:=THash_SHA2_224.GetHashSize;
OO0OCOQOQ0:
Result:=THash_SHA2_384.GetHashSize;
OQ0OCOQOQ0:
Result:=THash_MD5.GetHashSize;
OC0OCOQOQ0:
Result:=THash_MD4.GetHashSize;
O0OOCOQOQ0:
Result:=THash_MD2.GetHashSize;
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
end;
class function OOCQCC0OQ0.OOO0CC0OQ0(OQO0CC0OQ0:OO00COQOQ0):string;
begin
case OQO0CC0OQ0 of
OOOCCOQOQ0:
Result:=STripleDES_cbc;
OQOCCOQOQ0:
Result:=SBlowfish_cbc;
OCOCCOQOQ0:
Result:=SAES128_cbc;
O0QCCOQOQ0:
Result:=SAES192_cbc;
OOQCCOQOQ0:
Result:=SAES256_cbc;
OQQCCOQOQ0:
Result:=SCast128_cbc;
OCQCCOQOQ0:
Result:=STripleDES_ctr;
O0CCCOQOQ0:
Result:=SBlowfish_ctr;
OOCCCOQOQ0:
Result:=SAES128_ctr;
OQCCCOQOQ0:
Result:=SAES192_ctr;
OCCCCOQOQ0:
Result:=SAES256_ctr;
O000COQOQ0:
Result:=SCast128_ctr;
else
raise EScError.Create(seInvalidCipherAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCO0CC0OQ0(const O0Q0CC0OQ0:string):OO00COQOQ0;
begin
if O0Q0CC0OQ0=STripleDES_cbc then
Result:=OOOCCOQOQ0
else if O0Q0CC0OQ0=STripleDES_ctr then
Result:=OCQCCOQOQ0
else if O0Q0CC0OQ0=SBlowfish_cbc then
Result:=OQOCCOQOQ0
else if O0Q0CC0OQ0=SBlowfish_ctr then
Result:=O0CCCOQOQ0
else if O0Q0CC0OQ0=SAES128_cbc then
Result:=OCOCCOQOQ0
else if O0Q0CC0OQ0=SAES128_ctr then
Result:=OOCCCOQOQ0
else if O0Q0CC0OQ0=SAES192_cbc then
Result:=O0QCCOQOQ0
else if O0Q0CC0OQ0=SAES192_ctr then
Result:=OQCCCOQOQ0
else if O0Q0CC0OQ0=SAES256_cbc then
Result:=OOQCCOQOQ0
else if O0Q0CC0OQ0=SAES256_ctr then
Result:=OCCCCOQOQ0
else if O0Q0CC0OQ0=SCast128_cbc then
Result:=OQQCCOQOQ0
else if O0Q0CC0OQ0=SCast128_ctr then
Result:=O000COQOQ0
else
raise EScError.Create(seInvalidCipherAlgorithm);
end;
class function OOCQCC0OQ0.OOQ0CC0OQ0(OQQ0CC0OQ0:OOQQQOQOQ0):string;
begin
case OQQ0CC0OQ0 of
OQOQQOQOQ0:
Result:=DSA_TYPE_HEADER;
OCOQQOQOQ0:
Result:=RSA_TYPE_HEADER;
O0QQQOQOQ0:
Result:=ECDSA_TYPE_HEADER;
else
raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCQ0CC0OQ0(O0C0CC0OQ0:OOQQQOQOQ0;OOC0CC0OQ0:OOQOQOQOQ0):string;
var
OQC0CC0OQ0:string;
begin
case O0C0CC0OQ0 of
OQOQQOQOQ0:
Result:=DSA_TYPE_HEADER;
OCOQQOQOQ0:
Result:=RSA_TYPE_HEADER;
O0QQQOQOQ0:begin
OQC0CC0OQ0:=OC0OO0Q0Q0.OCCQ00Q0Q0(OOC0CC0OQ0);
if OQC0CC0OQ0='' then
raise EScError.Create(seUnknownEllipticCurveId);
if OOC0CC0OQ0=OCCCQOQOQ0 then
Result:=OQC0CC0OQ0
else
Result:=ECDSA_SHA2_TYPE_HEADER+OQC0CC0OQ0;
end;
else
raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCC0CC0OQ0(O00OCC0OQ0:OOQQQOQOQ0):string;
begin
case O00OCC0OQ0 of
OQOQQOQOQ0:
Result:=DSA_TYPE_HEADER;
OCOQQOQOQ0:
Result:=RSA_SHA256_TYPE_HEADER+','+RSA_SHA512_TYPE_HEADER+','+RSA_TYPE_HEADER;
O0QQQOQOQ0:
Result:=SECDSA_Sha2_Nistp521+','+SECDSA_Sha2_Nistp384+','+SECDSA_Sha2_Nistp256;
else
raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;
end;
class function OOCQCC0OQ0.OO0OCC0OQ0(OQ0OCC0OQ0:OOQQQOQOQ0;OC0OCC0OQ0:OOQOQOQOQ0=OCCCQOQOQ0):OOOOCOQOQ0;
var
O0OOCC0OQ0:integer;
begin
case OQ0OCC0OQ0 of
OQOQQOQOQ0:
Result:=OOC0COQOQ0;
OCOQQOQOQ0:
Result:=OOC0COQOQ0;
O0QQQOQOQ0:begin
if OC0OCC0OQ0=OCCCQOQOQ0 then
Result:=O0C0COQOQ0
else begin
O0OOCC0OQ0:=OQ0C00Q0Q0[OC0OCC0OQ0].Size shl 3;
if O0OOCC0OQ0>384 then
Result:=OCC0COQOQ0
else
if O0OOCC0OQ0>256 then
Result:=OO0OCOQOQ0
else
Result:=OQC0COQOQ0;
end;
end;
else
raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;
end;
class function OOCQCC0OQ0.OOOOCC0OQ0(const OQOOCC0OQ0:string):OOQQQOQOQ0;
begin
if OQOOCC0OQ0=DSA_TYPE_HEADER then
Result:=OQOQQOQOQ0
else
if(OQOOCC0OQ0=RSA_TYPE_HEADER)or(OQOOCC0OQ0=RSA_SHA256_TYPE_HEADER)or(OQOOCC0OQ0=RSA_SHA512_TYPE_HEADER)then
Result:=OCOQQOQOQ0
else
if(OQOOCC0OQ0=SCurve25519_Sha256)or(OQOOCC0OQ0=ED25519_TYPE_HEADER)then
Result:=O0QQQOQOQ0
else
if Copy(OQOOCC0OQ0,1,Length(ECDSA_TYPE_HEADER))=ECDSA_TYPE_HEADER then
Result:=O0QQQOQOQ0
else
raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;
class function OOCQCC0OQ0.OCOOCC0OQ0(const O0QOCC0OQ0:string):OOOOCOQOQ0;
begin
if(O0QOCC0OQ0=RSA_SHA256_TYPE_HEADER)or(O0QOCC0OQ0=SECDSA_Sha2_Nistp256)then
Result:=OQC0COQOQ0
else
if O0QOCC0OQ0=SECDSA_Sha2_Nistp384 then
Result:=OO0OCOQOQ0
else
if(O0QOCC0OQ0=RSA_SHA512_TYPE_HEADER)or(O0QOCC0OQ0=SECDSA_Sha2_Nistp521)then
Result:=OCC0COQOQ0
else
Result:=OOC0COQOQ0;
end;
class function OOCQCC0OQ0.OOQOCC0OQ0(OQQOCC0OQ0:OOQQQOQOQ0):string;
begin
case OQQOCC0OQ0 of
OQOQQOQOQ0:
Result:='DSA';
OCOQQOQOQ0:
Result:='RSA';
O0QQQOQOQ0:
Result:='EC';
else
raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCQOCC0OQ0(const O0COCC0OQ0:string):OOQQQOQOQ0;
begin
if O0COCC0OQ0='DSA' then
Result:=OQOQQOQOQ0
else if O0COCC0OQ0='RSA' then
Result:=OCOQQOQOQ0
else if O0COCC0OQ0='EC' then
Result:=O0QQQOQOQ0
else
raise EScError.Create(seInvalidPublicKeyAlgorithm);
end;
class function OOCQCC0OQ0.OOCOCC0OQ0(OQCOCC0OQ0:OOOOCOQOQ0):string;
begin
case OQCOCC0OQ0 of
O0C0COQOQ0:
Result:=SHASH_None;
OOC0COQOQ0:
Result:=SHASH_SHA1;
OQC0COQOQ0:
Result:=SHASH_SHA2_256;
OCC0COQOQ0:
Result:=SHASH_SHA2_512;
O00OCOQOQ0:
Result:=SHASH_SHA2_224;
OO0OCOQOQ0:
Result:=SHASH_SHA2_384;
OQ0OCOQOQ0:
Result:=SHASH_MD5;
OC0OCOQOQ0:
Result:=SHASH_MD4;
O0OOCOQOQ0:
Result:=SHASH_MD2;
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCCOCC0OQ0(const O00QQC0OQ0:string):OOOOCOQOQ0;
begin
if O00QQC0OQ0=SHASH_None then
Result:=O0C0COQOQ0
else if O00QQC0OQ0=SHASH_SHA1 then
Result:=OOC0COQOQ0
else if O00QQC0OQ0=SHASH_SHA2_256 then
Result:=OQC0COQOQ0
else if O00QQC0OQ0=SHASH_SHA2_512 then
Result:=OCC0COQOQ0
else if O00QQC0OQ0=SHASH_SHA2_224 then
Result:=O00OCOQOQ0
else if O00QQC0OQ0=SHASH_SHA2_384 then
Result:=OO0OCOQOQ0
else if O00QQC0OQ0=SHASH_MD5 then
Result:=OQ0OCOQOQ0
else if O00QQC0OQ0=SHASH_MD4 then
Result:=OC0OCOQOQ0
else if O00QQC0OQ0=SHASH_MD2 then
Result:=O0OOCOQOQ0
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
class function OOCQCC0OQ0.OO0QQC0OQ0(const OQ0QQC0OQ0:string):O0COCOQOQ0;
begin
if OQ0QQC0OQ0=Shmac_sha1 then
Result:=OQOOCOQOQ0
else
if OQ0QQC0OQ0=Shmac_sha2_256 then
Result:=OCOOCOQOQ0
else
if OQ0QQC0OQ0=Shmac_sha2_224 then
Result:=OOQOCOQOQ0
else
if OQ0QQC0OQ0=Shmac_sha2_512 then
Result:=O0QOCOQOQ0
else
if OQ0QQC0OQ0=Shmac_sha2_384 then
Result:=OQQOCOQOQ0
else
if OQ0QQC0OQ0=Shmac_md5 then
Result:=OCQOCOQOQ0
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
class function OOCQCC0OQ0.OC0QQC0OQ0(O0OQQC0OQ0:O0COCOQOQ0):string;
begin
case O0OQQC0OQ0 of
OQOOCOQOQ0:
Result:=Shmac_sha1;
OCOOCOQOQ0:
Result:=Shmac_sha2_256;
OOQOCOQOQ0:
Result:=Shmac_sha2_224;
O0QOCOQOQ0:
Result:=Shmac_sha2_512;
OQQOCOQOQ0:
Result:=Shmac_sha2_384;
OCQOCOQOQ0:
Result:=Shmac_md5;
else
raise EScError.Create(seInvalidHashAlgorithm);
end;
end;
class function OOCQCC0OQ0.OOOQQC0OQ0(OQOQQC0OQ0:OOCOQOQOQ0):string;
begin
case OQOQQC0OQ0 of
OQQOQOQOQ0:
Result:=SNone;
OCQOQOQOQ0:
Result:=SZLib;
O0COQOQOQ0:
Result:=SZLibOpenSSH;
else
raise EScError.Create(seInvalidCompressionAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCOQQC0OQ0(const O0QQQC0OQ0:string):OOCOQOQOQ0;
begin
if O0QQQC0OQ0=SNone then
Result:=OQQOQOQOQ0
else if O0QQQC0OQ0=SZLib then
Result:=OCQOQOQOQ0
else if O0QQQC0OQ0=SZLibOpenSSH then
Result:=O0COQOQOQ0
else
raise EScError.Create(seInvalidCompressionAlgorithm);
end;
class function OOCQCC0OQ0.OOQQQC0OQ0(OQQQQC0OQ0:OOOQQOQOQ0):string;
begin
case OQQQQC0OQ0 of
OOCOCOQOQ0:
Result:=SDiffieHellmanGroup1;
OQCOCOQOQ0:
Result:=SDiffieHellmanGroup14;
OCCOCOQOQ0:
Result:=SDiffieHellmanExchSHA1;
O00QQOQOQ0:
Result:=SDiffieHellmanExchSHA256;
OO0QQOQOQ0:
Result:=SECDH_Sha2_Nistp256;
OQ0QQOQOQ0:
Result:=SECDH_Sha2_Nistp384;
OC0QQOQOQ0:
Result:=SECDH_Sha2_Nistp521;
O0OQQOQOQ0:
Result:=SCurve25519_Sha256;
else
raise EScError.Create(seInvalidKeyExchangeAlgorithm);
end;
end;
class function OOCQCC0OQ0.OCQQQC0OQ0(const O0CQQC0OQ0:string):OOOQQOQOQ0;
begin
if O0CQQC0OQ0=SDiffieHellmanGroup1 then
Result:=OOCOCOQOQ0
else if O0CQQC0OQ0=SDiffieHellmanGroup14 then
Result:=OQCOCOQOQ0
else if O0CQQC0OQ0=SDiffieHellmanExchSHA1 then
Result:=OCCOCOQOQ0
else if O0CQQC0OQ0=SDiffieHellmanExchSHA256 then
Result:=O00QQOQOQ0
else if O0CQQC0OQ0=SECDH_Sha2_Nistp256 then
Result:=OO0QQOQOQ0
else if O0CQQC0OQ0=SECDH_Sha2_Nistp384 then
Result:=OQ0QQOQOQ0
else if O0CQQC0OQ0=SECDH_Sha2_Nistp521 then
Result:=OC0QQOQOQ0
else if O0CQQC0OQ0=SCurve25519_Sha256 then
Result:=O0OQQOQOQ0
else
raise EScError.Create(seInvalidKeyExchangeAlgorithm);
end;
class function OOCQCC0OQ0.OOCQQC0OQ0(const OQCQQC0OQ0:string):OOOOCOQOQ0;
begin
if OQCQQC0OQ0='' then
Result:=O0C0COQOQ0
else
if OQCQQC0OQ0=OID_SHA1 then
Result:=OOC0COQOQ0
else
if OQCQQC0OQ0=OID_SHA256 then
Result:=OQC0COQOQ0
else
if OQCQQC0OQ0=OID_SHA384 then
Result:=OO0OCOQOQ0
else
if OQCQQC0OQ0=OID_SHA512 then
Result:=OCC0COQOQ0
else
if OQCQQC0OQ0=OID_SHA224 then
Result:=O00OCOQOQ0
else
if OQCQQC0OQ0=OID_MD5 then
Result:=OQ0OCOQOQ0
else
if OQCQQC0OQ0=OID_MD4 then
Result:=OC0OCOQOQ0
else
if OQCQQC0OQ0=OID_MD2 then
Result:=O0OOCOQOQ0
else
raise EScError.Create(seUnknownHashAlgorithm);
end;
class function OOCQCC0OQ0.OCCQQC0OQ0(const O00CQC0OQ0:OOOOCOQOQ0):string;
begin
case O00CQC0OQ0 of
OOC0COQOQ0:
Result:=OID_SHA1;
OQC0COQOQ0:
Result:=OID_SHA256;
OCC0COQOQ0:
Result:=OID_SHA512;
O00OCOQOQ0:
Result:=OID_SHA224;
OO0OCOQOQ0:
Result:=OID_SHA384;
O0OOCOQOQ0:
Result:=OID_MD2;
OC0OCOQOQ0:
Result:=OID_MD4;
OQ0OCOQOQ0:
Result:=OID_MD5;
else
raise EScError.Create(seUnknownHashAlgorithm);
end;
end;
class function OOCQCC0OQ0.OO0CQC0OQ0(const OQ0CQC0OQ0:string):OQCCQOQOQ0;
begin
if OQ0CQC0OQ0='' then
Result:=OQQQQOQOQ0
else
if OQ0CQC0OQ0=OID_RSA_ENCRYPTION then
Result:=OCQQQOQOQ0
else
if OQ0CQC0OQ0=OID_RSA_PSS_ENCRYPTION then
Result:=O0CQQOQOQ0
else
if OQ0CQC0OQ0=OID_SHA1_WITH_RSA_ENC then
Result:=OOCQQOQOQ0
else
if OQ0CQC0OQ0=OID_SHA256_WITH_RSA_ENC then
Result:=OQCQQOQOQ0
else
if OQ0CQC0OQ0=OID_SHA224_WITH_RSA_ENC then
Result:=OCCQQOQOQ0
else
if OQ0CQC0OQ0=OID_SHA512_WITH_RSA_ENC then
Result:=O00CQOQOQ0
else
if OQ0CQC0OQ0=OID_SHA384_WITH_RSA_ENC then
Result:=OO0CQOQOQ0
else
if OQ0CQC0OQ0=OID_MD5_WITH_RSA_ENC then
Result:=OQ0CQOQOQ0
else
if OQ0CQC0OQ0=OID_MD4_WITH_RSA_ENC then
Result:=OC0CQOQOQ0
else
if OQ0CQC0OQ0=OID_MD2_WITH_RSA_ENC then
Result:=O0OCQOQOQ0
else
if OQ0CQC0OQ0=OID_DSA_ENCRYPTION then
Result:=OOOCQOQOQ0
else
if OQ0CQC0OQ0=OID_DSA_WITH_SHA1 then
Result:=OQOCQOQOQ0
else
if OQ0CQC0OQ0=OID_DSA_WITH_SHA256 then
Result:=OCOCQOQOQ0
else
if OQ0CQC0OQ0=OID_DSA_WITH_SHA224 then
Result:=O0QCQOQOQ0
else
if OQ0CQC0OQ0=OID_ECDSA_WITH_SHA1 then
Result:=OOQCQOQOQ0
else
if OQ0CQC0OQ0=OID_ECDSA_WITH_SHA224 then
Result:=OQQCQOQOQ0
else
if OQ0CQC0OQ0=OID_ECDSA_WITH_SHA256 then
Result:=OCQCQOQOQ0
else
if OQ0CQC0OQ0=OID_ECDSA_WITH_SHA384 then
Result:=O0CCQOQOQ0
else
if OQ0CQC0OQ0=OID_ECDSA_WITH_SHA512 then
Result:=OOCCQOQOQ0
else
raise EScError.Create(seUnknownSignatureAlgorithm);
end;
class function OOCQCC0OQ0.OC0CQC0OQ0(const O0OCQC0OQ0:OQCCQOQOQ0):string;
begin
case O0OCQC0OQ0 of
OCQQQOQOQ0:
Result:=OID_RSA_ENCRYPTION;
O0CQQOQOQ0:
Result:=OID_RSA_PSS_ENCRYPTION;
OOCQQOQOQ0:
Result:=OID_SHA1_WITH_RSA_ENC;
OQCQQOQOQ0:
Result:=OID_SHA256_WITH_RSA_ENC;
OCCQQOQOQ0:
Result:=OID_SHA224_WITH_RSA_ENC;
O00CQOQOQ0:
Result:=OID_SHA512_WITH_RSA_ENC;
OO0CQOQOQ0:
Result:=OID_SHA384_WITH_RSA_ENC;
OQ0CQOQOQ0:
Result:=OID_MD5_WITH_RSA_ENC;
OC0CQOQOQ0:
Result:=OID_MD4_WITH_RSA_ENC;
O0OCQOQOQ0:
Result:=OID_MD2_WITH_RSA_ENC;
OOOCQOQOQ0:
Result:=OID_DSA_ENCRYPTION;
OQOCQOQOQ0:
Result:=OID_DSA_WITH_SHA1;
OCOCQOQOQ0:
Result:=OID_DSA_WITH_SHA256;
O0QCQOQOQ0:
Result:=OID_DSA_WITH_SHA224;
else
raise EScError.Create(seUnknownSignatureAlgorithm);
end;
end;
class function OOCQCC0OQ0.OOOCQC0OQ0(const OQOCQC0OQ0:OQCCQOQOQ0):OOOOCOQOQ0;
begin
case OQOCQC0OQ0 of
OCQQQOQOQ0:
Result:=O0C0COQOQ0;
O0CQQOQOQ0:
Result:=O0C0COQOQ0;
OOCQQOQOQ0:
Result:=OOC0COQOQ0;
OQCQQOQOQ0:
Result:=OQC0COQOQ0;
OCCQQOQOQ0:
Result:=O00OCOQOQ0;
O00CQOQOQ0:
Result:=OCC0COQOQ0;
OO0CQOQOQ0:
Result:=OO0OCOQOQ0;
OQ0CQOQOQ0:
Result:=OQ0OCOQOQ0;
OC0CQOQOQ0:
Result:=OC0OCOQOQ0;
O0OCQOQOQ0:
Result:=O0OOCOQOQ0;
OOOCQOQOQ0:
Result:=O0C0COQOQ0;
OQOCQOQOQ0:
Result:=OOC0COQOQ0;
OCOCQOQOQ0:
Result:=OQC0COQOQ0;
O0QCQOQOQ0:
Result:=O00OCOQOQ0;
else
Result:=O0C0COQOQ0;
end;
end;
class function OOCQCC0OQ0.OCOCQC0OQ0(const O0QCQC0OQ0:string):OO00COQOQ0;
begin
if O0QCQC0OQ0=OID_AES256_CBC then
Result:=OOQCCOQOQ0
else
if O0QCQC0OQ0=OID_AES192_CBC then
Result:=O0QCCOQOQ0
else
if O0QCQC0OQ0=OID_AES128_CBC then
Result:=OCOCCOQOQ0
else
if O0QCQC0OQ0=OID_DES_EDE3_CBC then
Result:=OOOCCOQOQ0
else
raise EScError.Create(seCipherNotSupported);
end;
class function OOCQCC0OQ0.OOQCQC0OQ0(const OQQCQC0OQ0:OO00COQOQ0):string;
begin
case OQQCQC0OQ0 of
OOOCCOQOQ0:
Result:=OID_DES_EDE3_CBC;
OCOCCOQOQ0:
Result:=OID_AES128_CBC;
O0QCCOQOQ0:
Result:=OID_AES192_CBC;
OOQCCOQOQ0:
Result:=OID_AES256_CBC;
else
raise EScError.Create(seCipherNotSupported);
end;
end;
end.
