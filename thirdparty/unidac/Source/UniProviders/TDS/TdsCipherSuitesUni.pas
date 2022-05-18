//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright � 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsCipherSuitesUni;
interface
uses
SysUtils,
CRTypes,CRCryptoTransformIntf,CRSymmetricAlgorithm,CRHashAlgorithm,
CRCipher,CRHash,CRHMAC,
{$IFNDEF UNIDACPRO}
TdsSSLConsts,TdsUtils,TdsBridge,TdsCertificateConsts,TdsSSLTypes;
{$ELSE}
TdsSSLConstsUni,TdsUtilsUni,TdsBridgeUni,TdsCertificateConstsUni,TdsSSLTypesUni;
{$ENDIF}
type
TCipherDefinition=record
CipherAlgorithmClass:TSymmetricAlgorithmClass;
CipherMode:TCipherMode;
KeyMaterialLength:integer;
ExpandedKeyLength:integer;
HashAlgorithm:OOOOCOQOQ0;
SupportedProtocols:TScSSLProtocols;
end;
OCQ0O0Q0Q0=class
public
O0C0O0Q0Q0:TSymmetricAlgorithm;
OOC0O0Q0Q0:TSymmetricAlgorithm;
OQC0O0Q0Q0:TKeyedHashAlgorithm;
OCC0O0Q0Q0:TKeyedHashAlgorithm;
O00OO0Q0Q0:TBytes;
OO0OO0Q0Q0:TBytes;
destructor Destroy;override;
end;
OC0OO0Q0Q0=class
public
class function O0OOO0Q0Q0(OOOOO0Q0Q0:word):OC0QQQQ0Q0;
class procedure OOQOO0Q0Q0(OQQOO0Q0Q0:OC0QQQQ0Q0;const OCQOO0Q0Q0:TBytes;O0COO0Q0Q0:integer);
class function OOCOO0Q0Q0(const OQCOO0Q0Q0:OC0QQQQ0Q0):string;
class function OCCOO0Q0Q0(const O00Q00Q0Q0:string):OC0QQQQ0Q0;
class function OC0Q00Q0Q0(O0OQ00Q0Q0:word):OOOCQQQ0Q0;
class function OQOQ00Q0Q0(OCOQ00Q0Q0:word):OOQOQOQOQ0;
class function OOQQ00Q0Q0(const OQQQ00Q0Q0:string):OOQOQOQOQ0;
class function OCQQ00Q0Q0(O0CQ00Q0Q0:OOQOQOQOQ0):string;
class function OOCQ00Q0Q0(const OQCQ00Q0Q0:string):OOQOQOQOQ0;
class function OCCQ00Q0Q0(O00C00Q0Q0:OOQOQOQOQ0):string;
end;
const
OO0C00Q0Q0:array[OC0QQQQ0Q0]of TCipherDefinition=(
(
CipherAlgorithmClass:TSymmetricAlgorithm;CipherMode:cmCBC;
KeyMaterialLength:0;ExpandedKeyLength:0;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[]),
(
CipherAlgorithmClass:TCipher_RC4;CipherMode:cmECB;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQ0OCOQOQ0;
SupportedProtocols:[O0CCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_RC4;CipherMode:cmECB;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_1DES;CipherMode:cmCBC;
KeyMaterialLength:8;ExpandedKeyLength:8;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_3DES;CipherMode:cmCBC;
KeyMaterialLength:24;ExpandedKeyLength:24;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OO0OCOQOQ0;
SupportedProtocols:[OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_1DES;CipherMode:cmCBC;
KeyMaterialLength:8;ExpandedKeyLength:8;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_3DES;CipherMode:cmCBC;
KeyMaterialLength:24;ExpandedKeyLength:24;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OO0OCOQOQ0;
SupportedProtocols:[OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_RC4;CipherMode:cmECB;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_3DES;CipherMode:cmCBC;
KeyMaterialLength:24;ExpandedKeyLength:24;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OO0OCOQOQ0;
SupportedProtocols:[O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OO0OCOQOQ0;
SupportedProtocols:[OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_RC4;CipherMode:cmECB;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_3DES;CipherMode:cmCBC;
KeyMaterialLength:24;ExpandedKeyLength:24;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OOC0COQOQ0;
SupportedProtocols:[O0CCCQQ0Q0,O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmCBC;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OO0OCOQOQ0;
SupportedProtocols:[O0QCCQQ0Q0,OOQCCQQ0Q0,OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OO0OCOQOQ0;
SupportedProtocols:[OQQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:16;ExpandedKeyLength:16;
HashAlgorithm:OQC0COQOQ0;
SupportedProtocols:[OCQCCQQ0Q0]),
(
CipherAlgorithmClass:TCipher_Rijndael;CipherMode:cmGCM;
KeyMaterialLength:32;ExpandedKeyLength:32;
HashAlgorithm:OO0OCOQOQ0;
SupportedProtocols:[OCQCCQQ0Q0])
);
OQ0C00Q0Q0:array[OOQOQOQOQ0]of TScECParameters=(
(ECName:OCCCQOQOQ0;
CryptographyClass:OQ00000OQ0;
Size:32;
p:'7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFED';
a:'76D06';
b:'0';
Gx:'216936D3CD6E53FEC0A4E231FDD6DC5C692CC7609525A7B2C9562D608F25D51A';
Gy:'6666666666666666666666666666666666666666666666666666666666666658';
n:'1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED';
OID:OID_X25519;
SSHName:ED25519_TYPE_HEADER),
(ECName:O000QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:20;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF';
a:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFC';
b:'1C97BEFC54BD7A8B65ACF89F81D4D4ADC565FA45';
Gx:'4A96B5688EF573284664698968C38BB913CBFC82';
Gy:'23A628553168947D59DCC912042351377AC5FB32';
n:'0100000000000000000001F4C8F927AED3CA752257';
OID:OID_secp160r1;
SSHName:''),
(ECName:OO00QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:20;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73';
a:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC70';
b:'B4E134D3FB59EB8BAB57274904664D5AF50388BA';
Gx:'52DCB034293A117E1F4FF11B30F7199D3144CE6D';
Gy:'FEAFFEF2E331F296E071FA0DF9982CFEA7D43F2E';
n:'0100000000000000000000351EE786A818F3A1A16B';
OID:OID_secp160r2;
SSHName:''),
(ECName:OQ00QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:20;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73';
a:'0';
b:'7';
Gx:'3B4C382CE37AA192A4019E763036F4F5DD4D7EBB';
Gy:'938CF935318FDCED6BC28286531733C3F03C4FEE';
n:'0100000000000000000001B8FA16DFAB9ACA16B6B3';
OID:OID_secp160k1;
SSHName:''),
(ECName:OC00QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:24;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF';
a:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC';
b:'64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1';
Gx:'188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012';
Gy:'07192B95FFC8DA78631011ED6B24CDD573F977A11E794811';
n:'FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831';
OID:OID_secp192r1;
SSHName:SSH_NIST_p192),
(ECName:O0O0QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:24;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFEE37';
a:'0';
b:'3';
Gx:'DB4FF10EC057E9AE26B07D0280B7F4341DA5D1B1EAE06C7D';
Gy:'9B2F2F6D9C5628A7844163D015BE86344082AA88D95E2F9D';
n:'FFFFFFFFFFFFFFFFFFFFFFFE26F2FC170F69466A74DEFD8D';
OID:OID_secp192k1;
SSHName:''),
(ECName:OOO0QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:28;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001';
a:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE';
b:'B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4';
Gx:'B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21';
Gy:'BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34';
n:'FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D';
OID:OID_secp224r1;
SSHName:SSH_NIST_p224),
(ECName:OQO0QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:28;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D';
a:'0';
b:'5';
Gx:'A1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C';
Gy:'7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5';
n:'010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7';
OID:OID_secp224k1;
SSHName:''),
(ECName:OCO0QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:32;
p:'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF';
a:'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC';
b:'5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B';
Gx:'6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296';
Gy:'4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5';
n:'FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551';
OID:OID_secp256r1;
SSHName:SSH_NIST_p256),
(ECName:O0Q0QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:32;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F';
a:'0';
b:'7';
Gx:'79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798';
Gy:'483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8';
n:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141';
OID:OID_secp256k1;
SSHName:''),
(ECName:OOQ0QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:48;
p:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF';
a:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC';
b:'B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF';
Gx:'AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7';
Gy:'3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1CE1D7E819D7A431D7C90EA0E5F';
n:'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973';
OID:OID_secp384r1;
SSHName:SSH_NIST_p384),
(ECName:OQQ0QOQOQ0;
CryptographyClass:O0QCO00OQ0;
Size:66;
p:'01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
a:'01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC';
b:'0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00';
Gx:'00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66';
Gy:'011839296A789A3BC0045C8A5FB42C7D1BD998F54449579B446817AFBD17273E662C97EE72995EF42640C550B9013FAD0761353C7086A272C24088BE94769FD16650';
n:'01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409';
OID:OID_secp521r1;
SSHName:SSH_NIST_p521),
(ECName:OCQ0QOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:21;
p:'0800000000000000000000000000000000000000C9';
a:'07B6882CAAEFA84F9554FF8428BD88E246D2782AE2';
b:'0713612DCDDCB40AAB946BDA29CA91F73AF958AFD9';
Gx:'0369979697AB43897789566789567F787A7876A654';
Gy:'00435EDB42EFAFB2989D51FEFCE3C80988F41FF883';
n:'03FFFFFFFFFFFFFFFFFFFF48AAB689C29CA710279B';
OID:OID_sect163r1;
SSHName:''),
(ECName:O0C0QOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:21;
p:'0800000000000000000000000000000000000000C9';
a:'1';
b:'020A601907B8C953CA1481EB10512F78744A3205FD';
Gx:'03F0EBA16286A2D57EA0991168D4994637E8343E36';
Gy:'00D51FBC6C71A0094FA2CDD545B11C5C0C797324F1';
n:'040000000000000000000292FE77E70C12A4234C33';
OID:OID_sect163r2;
SSHName:''),
(ECName:OOC0QOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:21;
p:'0800000000000000000000000000000000000000C9';
a:'1';
b:'1';
Gx:'02FE13C0537BBC11ACAA07D793DE4E6D5E5C94EEE8';
Gy:'0289070FB05D38FF58321F2E800536D538CCDAA3D9';
n:'04000000000000000000020108A2E0CC0D99F8A5EF';
OID:OID_sect163k1;
SSHName:SSH_NIST_k163),
(ECName:OQC0QOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:25;
p:'02000000000000000000000000000000000000000000008001';
a:'0017858FEB7A98975169E171F77B4087DE098AC8A911DF7B01';
b:'00FDFB49BFE6C3A89FACADAA7A1E5BBC7CC1C2E5D831478814';
Gx:'01F481BC5F0FF84A74AD6CDF6FDEF4BF6179625372D8C0C5E1';
Gy:'0025E399F2903712CCF3EA9E3A1AD17FB0B3201B6AF7CE1B05';
n:'01000000000000000000000000C7F34A778F443ACC920EBA49';
OID:OID_sect193r1;
SSHName:''),
(ECName:OCC0QOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:25;
p:'02000000000000000000000000000000000000000000008001';
a:'0163F35A5137C2CE3EA6ED8667190B0BC43ECD69977702709B';
b:'00C9BB9E8927D4D64C377E2AB2856A5B16E3EFB7F61D4316AE';
Gx:'00D9B67D192E0367C803F39E1A7E82CA14A651350AAE617E8F';
Gy:'01CE94335607C304AC29E7DEFBD9CA01F596F927224CDECF6C';
n:'010000000000000000000000015AAB561B005413CCD4EE99D5';
OID:OID_sect193r2;
SSHName:''),
(ECName:O00OQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:30;
p:'020000000000000000000000000000000000000004000000000000000001';
a:'1';
b:'0066647EDE6C332C7F8C0923BB58213B333B20E9CE4281FE115F7D8F90AD';
Gx:'00FAC9DFCBAC8313BB2139F1BB755FEF65BC391F8B36F8F8EB7371FD558B';
Gy:'01006A08A41903350678E58528BEBF8A0BEFF867A7CA36716F7E01F81052';
n:'01000000000000000000000000000013E974E72F8A6922031D2603CFE0D7';
OID:OID_sect233r1;
SSHName:SSH_NIST_b233),
(ECName:OO0OQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:30;
p:'020000000000000000000000000000000000000004000000000000000001';
a:'0';
b:'1';
Gx:'017232BA853A7E731AF129F22FF4149563A419C26BF50A4C9D6EEFAD6126';
Gy:'01DB537DECE819B7F70F555A67C427A8CD9BF18AEB9B56E0C11056FAE6A3';
n:'008000000000000000000000000000069D5BB915BCD46EFB1AD5F173ABDF';
OID:OID_sect233k1;
SSHName:SSH_NIST_k233),
(ECName:OQ0OQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:30;
p:'800000000000000000004000000000000000000000000000000000000001';
a:'0';
b:'1';
Gx:'29A0B6A887A983E9730988A68727A8B2D126C44CC2CC7B2A6555193035DC';
Gy:'76310804F12E549BDB011C103089E73510ACB275FC312A5DC6B76553F0CA';
n:'2000000000000000000000000000005A79FEC67CB6E91F1C1DA800E478A5';
OID:OID_sect239k1;
SSHName:''),
(ECName:OC0OQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:36;
p:'0800000000000000000000000000000000000000000000000000000000000000000010A1';
a:'1';
b:'027B680AC8B8596DA5A4AF8A19A0303FCA97FD7645309FA2A581485AF6263E313B79A2F5';
Gx:'05F939258DB7DD90E1934F8C70B0DFEC2EED25B8557EAC9C80E2E198F8CDBECD86B12053';
Gy:'03676854FE24141CB98FE6D4B20D02B4516FF702350EDDB0826779C813F0DF45BE8112F4';
n:'03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF90399660FC938A90165B042A7CEFADB307';
OID:OID_sect283r1;
SSHName:''),
(ECName:O0OOQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:36;
p:'0800000000000000000000000000000000000000000000000000000000000000000010A1';
a:'0';
b:'1';
Gx:'0503213F78CA44883F1A3B8162F188E553CD265F23C1567A16876913B0C2AC2458492836';
Gy:'01CCDA380F1C9E318D90F95D07E5426FE87E45C0E8184698E45962364E34116177DD2259';
n:'01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9AE2ED07577265DFF7F94451E061E163C61';
OID:OID_sect283k1;
SSHName:SSH_NIST_k283),
(ECName:OOOOQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:52;
p:'02000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001';
a:'1';
b:'0021A5C2C8EE9FEB5C4B9A753B7B476B7FD6422EF1F3DD674761FA99D6AC27C8A9A197B272822F6CD57A55AA4F50AE317B13545F';
Gx:'015D4860D088DDB3496B0C6064756260441CDE4AF1771D4DB01FFE5B34E59703DC255A868A1180515603AEAB60794E54BB7996A7';
Gy:'0061B1CFAB6BE5F32BBFA78324ED106A7636B9C5A7BD198D0158AA4F5488D08F38514F1FDF4B4F40D2181B3681C364BA0273C706';
n:'010000000000000000000000000000000000000000000000000001E2AAD6A612F33307BE5FA47C3C9E052F838164CD37D9A21173';
OID:OID_sect409r1;
SSHName:SSH_NIST_b409),
(ECName:OQOOQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:52;
p:'02000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001';
a:'0';
b:'1';
Gx:'0060F05F658F49C1AD3AB1890F7184210EFD0987E307C84C27ACCFB8F9F67CC2C460189EB5AAAA62EE222EB1B35540CFE9023746';
Gy:'01E369050B7C4E42ACBA1DACBF04299C3460782F918EA427E6325165E9EA10E3DA5F6C42E9C55215AA9CA27A5863EC48D8E0286B';
n:'007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5F83B2D4EA20400EC4557D5ED3E3E7CA5B4B5C83B8E01E5FCF';
OID:OID_sect409k1;
SSHName:SSH_NIST_k409),
(ECName:OCOOQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:72;
p:'080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425';
a:'1';
b:'02F40E7E2221F295DE297117B7F3D62F5C6A97FFCB8CEFF1CD6BA8CE4A9A18AD84FFABBD8EFA59332BE7AD6756A66E294AFD185A78FF12AA520E4DE739BACA0C7FFEFF7F2955727A';
Gx:'0303001D34B856296C16C0D40D3CD7750A93D1D2955FA80AA5F40FC8DB7B2ABDBDE53950F4C0D293CDD711A35B67FB1499AE60038614F1394ABFA3B4C850D927E1E7769C8EEC2D19';
Gy:'037BF27342DA639B6DCCFFFEB73D69D78C6C27A6009CBBCA1980F8533921E8A684423E43BAB08A576291AF8F461BB2A8B3531D2F0485C19B16E2F1516E23DD3C1A4827AF1B8AC15B';
n:'03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE661CE18FF55987308059B186823851EC7DD9CA1161DE93D5174D66E8382E9BB2FE84E47';
OID:OID_sect571r1;
SSHName:''),
(ECName:O0QOQOQOQ0;
CryptographyClass:OQOOO00OQ0;
Size:72;
p:'080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425';
a:'0';
b:'1';
Gx:'026EB7A859923FBC82189631F8103FE4AC9CA2970012D5D46024804801841CA44370958493B205E647DA304DB4CEB08CBBD1BA39494776FB988B47174DCA88C7E2945283A01C8972';
Gy:'0349DC807F4FBF374F4AEADE3BCA95314DD58CEC9F307A54FFC61EFC006D8A2C9D4979C0AC44AEA74FBEBBB9F772AEDCB620B01A7BA7AF1B320430C8591984F601CD4C143EF1C7A3';
n:'020000000000000000000000000000000000000000000000000000000000000000000000131850E1F19A63E4B391A8DB917F4138B630D84BE5D639381E91DEB45CFE778F637C1001';
OID:OID_sect571k1;
SSHName:SSH_NIST_t571)
);
OC0C00Q0Q0:array[OOQCQQQ0Q0]of byte=(
1,
2,
3
);
O0OC00Q0Q0:array[OCCOQQQ0Q0]of word=(
29,
16,17,15,
19,18,21,20,
23,22,24,25,
2,3,1,
4,5,7,6,8,
10,9,12,11,
14,13,
$0100,$0101,$0102,
$0103,$0104
);
implementation
uses
{$IFNDEF UNIDACPRO}
TdsTLS1HandshakeLayer,TdsTLS13HandshakeLayer,TdsSSL3HandshakeLayer;
{$ELSE}
TdsTLS1HandshakeLayerUni,TdsTLS13HandshakeLayerUni,TdsSSL3HandshakeLayerUni;
{$ENDIF}
const
OOOC00Q0Q0:array[OC0QQQQ0Q0]of array[0..1]of byte=(
($00,$00),
($00,$04),
($00,$05),
($00,$09),
($00,$0A),
($00,$2F),
($00,$35),
($00,$3C),
($00,$3D),
($00,$9C),
($00,$9D),
($00,$15),
($00,$16),
($00,$33),
($00,$39),
($00,$67),
($00,$6B),
($00,$9E),
($00,$9F),
($C0,$11),
($C0,$12),
($C0,$13),
($C0,$14),
($C0,$27),
($C0,$28),
($C0,$2F),
($C0,$30),
($C0,$07),
($C0,$08),
($C0,$09),
($C0,$0A),
($C0,$23),
($C0,$24),
($C0,$2B),
($C0,$2C),
($13,$01),
($13,$02)
);
OQOC00Q0Q0:array[OC0QQQQ0Q0]of string=(
OCOCOOQ0Q0,
OOQCOOQ0Q0,
OQQCOOQ0Q0,
OCQCOOQ0Q0,
O0CCOOQ0Q0,
OQCCOOQ0Q0,
OCCCOOQ0Q0,
O000OOQ0Q0,
OO00OOQ0Q0,
OQ00OOQ0Q0,
OC00OOQ0Q0,
O0O0OOQ0Q0,
OOO0OOQ0Q0,
OQO0OOQ0Q0,
OCO0OOQ0Q0,
O0Q0OOQ0Q0,
OOQ0OOQ0Q0,
OQQ0OOQ0Q0,
OCQ0OOQ0Q0,
O0C0OOQ0Q0,
OOC0OOQ0Q0,
OQC0OOQ0Q0,
OCC0OOQ0Q0,
O00OOOQ0Q0,
OO0OOOQ0Q0,
OQ0OOOQ0Q0,
OC0OOOQ0Q0,
O0OOOOQ0Q0,
OOOOOOQ0Q0,
OQOOOOQ0Q0,
OCOOOOQ0Q0,
O0QOOOQ0Q0,
OOQOOOQ0Q0,
OQQOOOQ0Q0,
OCQOOOQ0Q0,
O0COOOQ0Q0,
OOCOOOQ0Q0
);
destructor OCQ0O0Q0Q0.Destroy;
begin
O0C0O0Q0Q0.Free;
OOC0O0Q0Q0.Free;
OQC0O0Q0Q0.Free;
OCC0O0Q0Q0.Free;
inherited;
end;
class function OC0OO0Q0Q0.O0OOO0Q0Q0(OOOOO0Q0Q0:word):OC0QQQQ0Q0;
var
OQOOO0Q0Q0:OC0QQQQ0Q0;
OCOOO0Q0Q0,O0QOO0Q0Q0:byte;
begin
Result:=OQCCCQQ0Q0;
OCOOO0Q0Q0:=OOOOO0Q0Q0 shr 8;
O0QOO0Q0Q0:=byte(OOOOO0Q0Q0);
for OQOOO0Q0Q0:=Low(OC0QQQQ0Q0)to High(OC0QQQQ0Q0)do
if(OCOOO0Q0Q0=OOOC00Q0Q0[OQOOO0Q0Q0][0])and(O0QOO0Q0Q0=OOOC00Q0Q0[OQOOO0Q0Q0][1])then begin
Result:=OQOOO0Q0Q0;
Exit;
end;
end;
class procedure OC0OO0Q0Q0.OOQOO0Q0Q0(OQQOO0Q0Q0:OC0QQQQ0Q0;
const OCQOO0Q0Q0:TBytes;O0COO0Q0Q0:integer);
begin
if Length(OCQOO0Q0Q0)<(O0COO0Q0Q0+2)then
raise EScError.Create(seInvalidInputArgs);
OCQOO0Q0Q0[O0COO0Q0Q0]:=OOOC00Q0Q0[OQQOO0Q0Q0][0];
OCQOO0Q0Q0[O0COO0Q0Q0+1]:=OOOC00Q0Q0[OQQOO0Q0Q0][1];
end;
class function OC0OO0Q0Q0.OOCOO0Q0Q0(const OQCOO0Q0Q0:OC0QQQQ0Q0):string;
begin
Result:=OQOC00Q0Q0[OQCOO0Q0Q0];
end;
class function OC0OO0Q0Q0.OCCOO0Q0Q0(const O00Q00Q0Q0:string):OC0QQQQ0Q0;
var
OO0Q00Q0Q0:string;
OQ0Q00Q0Q0:OC0QQQQ0Q0;
begin
OO0Q00Q0Q0:=UpperCase(StringReplace(O00Q00Q0Q0,'-','_',[rfReplaceAll]));
for OQ0Q00Q0Q0:=Low(OC0QQQQ0Q0)to High(OC0QQQQ0Q0)do
if OO0Q00Q0Q0=OQOC00Q0Q0[OQ0Q00Q0Q0]then begin
Result:=OQ0Q00Q0Q0;
Exit;
end;
raise EScError.Create(seInvalidCipherAlgorithm);
end;
class function OC0OO0Q0Q0.OC0Q00Q0Q0(O0OQ00Q0Q0:word):OOOCQQQ0Q0;
var
OOOQ00Q0Q0:OOOCQQQ0Q0;
begin
for OOOQ00Q0Q0:=Low(OOOCQQQ0Q0)to High(OOOCQQQ0Q0)do
if O0OQ00Q0Q0=OQQQO0Q0Q0[OOOQ00Q0Q0]then begin
Result:=OOOQ00Q0Q0;
Exit;
end;
Result:=OOOCQQQ0Q0(-1);
end;
class function OC0OO0Q0Q0.OQOQ00Q0Q0(OCOQ00Q0Q0:word):OOQOQOQOQ0;
var
O0QQ00Q0Q0:OOQOQOQOQ0;
begin
for O0QQ00Q0Q0:=Low(OOQOQOQOQ0)to High(OOQOQOQOQ0)do
if OCOQ00Q0Q0=O0OC00Q0Q0[OCCOQQQ0Q0(byte(O0QQ00Q0Q0))]then begin
Result:=O0QQ00Q0Q0;
Exit;
end;
raise EScError.Create(seInvalidEllipticCurveName);
end;
class function OC0OO0Q0Q0.OOQQ00Q0Q0(const OQQQ00Q0Q0:string):OOQOQOQOQ0;
begin
for Result:=Low(OOQOQOQOQ0)to High(OOQOQOQOQ0)do
if OQQQ00Q0Q0=OQ0C00Q0Q0[Result].OID then
Exit;
raise EScError.Create(seUnknownEllipticCurveId);
end;
class function OC0OO0Q0Q0.OCQQ00Q0Q0(O0CQ00Q0Q0:OOQOQOQOQ0):string;
begin
Result:=OQ0C00Q0Q0[O0CQ00Q0Q0].OID;
end;
class function OC0OO0Q0Q0.OOCQ00Q0Q0(const OQCQ00Q0Q0:string):OOQOQOQOQ0;
begin
for Result:=Low(OOQOQOQOQ0)to High(OOQOQOQOQ0)do
if OQCQ00Q0Q0=OQ0C00Q0Q0[Result].SSHName then
Exit;
raise EScError.Create(seUnknownEllipticCurveId);
end;
class function OC0OO0Q0Q0.OCCQ00Q0Q0(O00C00Q0Q0:OOQOQOQOQ0):string;
begin
Result:=OQ0C00Q0Q0[O00C00Q0Q0].SSHName;
end;
end.