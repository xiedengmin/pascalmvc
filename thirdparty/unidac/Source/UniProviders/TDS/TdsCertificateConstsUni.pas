
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Tds.inc}
unit TdsCertificateConstsUni;

interface

const
  // OID { 1.3.14.3.2.26 }
  SHA1_ASN_ID: array[0..14] of byte = (
    $30, $21, $30, $09, $06, $05, $2B, $0E, $03, $02, $1A, $05, $00, $04, $14);

  // OID { 2.16.840.1.101.3.4.2.1 }
  SHA256_ASN_ID: array[0..18] of byte = (
    $30, $31, $30, $0D, $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $01, $05, $00, $04, $20);

  // OID { 2.16.840.1.101.3.4.2.2 }
  SHA384_ASN_ID: array[0..18] of byte = (
    $30, $41, $30, $0D, $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $02, $05, $00, $04, $30);

  // OID { 2.16.840.1.101.3.4.2.3 }
  SHA512_ASN_ID: array[0..18] of byte = (
    $30, $51, $30, $0D, $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $03, $05, $00, $04, $40);

  // OID { 2.16.840.1.101.3.4.2.4 }
  SHA224_ASN_ID: array[0..18] of byte = (
    $30, $2D, $30, $0D, $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $04, $05, $00, $04, $1C);

  // OID { 1.2.840.113549.2.2 }
  MD2_ASN_ID: array[0..17] of byte = (
    $30, $20, $30, $0C, $06, $08, $2A, $86, $48, $86, $F7, $0D, $02, $02, $05, $00, $04, $10);

  // OID { 1.2.840.113549.2.4 }
  MD4_ASN_ID: array[0..17] of byte = (
    $30, $20, $30, $0C, $06, $08, $2A, $86, $48, $86, $F7, $0D, $02, $04, $05, $00, $04, $10);

  // OID { 1.2.840.113549.2.5 }
  MD5_ASN_ID: array[0..17] of byte = (
    $30, $20, $30, $0C, $06, $08, $2A, $86, $48, $86, $F7, $0D, $02, $05, $05, $00, $04, $10);

  OID_RSADSI              = '1.2.840.113549';
  OID_PKCS                = OID_RSADSI + '.1';     // 1.2.840.113549.1

  OID_PKCS1               = OID_PKCS + '.1';       // 1.2.840.113549.1.1
  OID_PKCS5               = OID_PKCS + '.5';       // 1.2.840.113549.1.5
  OID_PKCS7               = OID_PKCS + '.7';       // 1.2.840.113549.1.7
  OID_PKCS9               = OID_PKCS + '.9';       // 1.2.840.113549.1.9
  OID_PKCS12              = OID_PKCS + '.12';      // 1.2.840.113549.1.12

  OID_SHA1                = '1.3.14.3.2.26';
  OID_SHA256              = '2.16.840.1.101.3.4.2.1';
  OID_SHA384              = '2.16.840.1.101.3.4.2.2';
  OID_SHA512              = '2.16.840.1.101.3.4.2.3';
  OID_SHA224              = '2.16.840.1.101.3.4.2.4';
  OID_MD2                 = OID_RSADSI + '.2.2';   // 1.2.840.113549.2.2
  OID_MD4                 = OID_RSADSI + '.2.4';   // 1.2.840.113549.2.4
  OID_MD5                 = OID_RSADSI + '.2.5';   // 1.2.840.113549.2.5

  OID_COMMON_NAME         = '2.5.4.3';
  OID_ORGANIZATIONAL_UNIT_NAME = '2.5.4.11';
  OID_ORGANIZATION_NAME   = '2.5.4.10';
  OID_RSA_emailAddr       = OID_PKCS + '.9.1';     // 1.2.840.113549.1.9.1

  OID_AES128_CBC          = '2.16.840.1.101.3.4.1.2';
  OID_AES192_CBC          = '2.16.840.1.101.3.4.1.22';
  OID_AES256_CBC          = '2.16.840.1.101.3.4.1.42';
  OID_DES_EDE3_CBC        = OID_RSADSI + '.3.7';   // 1.2.840.113549

  OID_DSA_ENCRYPTION      = '1.2.840.10040.4.1';
  OID_DSA_WITH_SHA1       = '1.2.840.10040.4.3';
  OID_DSA_WITH_SHA224     = '2.16.840.1.101.3.4.3.1';
  OID_DSA_WITH_SHA256     = '2.16.840.1.101.3.4.3.2';

  OID_RSA_ENCRYPTION      = OID_PKCS1 + '.1';  // 1.2.840.113549.1.1.1
  OID_MD2_WITH_RSA_ENC    = OID_PKCS1 + '.2';  // 1.2.840.113549.1.1.2
  OID_MD4_WITH_RSA_ENC    = OID_PKCS1 + '.3';  // 1.2.840.113549.1.1.3
  OID_MD5_WITH_RSA_ENC    = OID_PKCS1 + '.4';  // 1.2.840.113549.1.1.4
  OID_SHA1_WITH_RSA_ENC   = OID_PKCS1 + '.5';  // 1.2.840.113549.1.1.5
  OID_RSA_MGF1            = OID_PKCS1 + '.8';  // 1.2.840.113549.1.1.8
  OID_RSA_PSS_ENCRYPTION  = OID_PKCS1 + '.10'; // 1.2.840.113549.1.1.10
  OID_SHA256_WITH_RSA_ENC = OID_PKCS1 + '.11'; // 1.2.840.113549.1.1.11
  OID_SHA384_WITH_RSA_ENC = OID_PKCS1 + '.12'; // 1.2.840.113549.1.1.12
  OID_SHA512_WITH_RSA_ENC = OID_PKCS1 + '.13'; // 1.2.840.113549.1.1.13
  OID_SHA224_WITH_RSA_ENC = OID_PKCS1 + '.14'; // 1.2.840.113549.1.1.14

  OID_pbeWithMD2AndDES_CBC  = OID_PKCS5 + '.1';  // 1.2.840.113549.1.5.1
  OID_pbeWithMD2AndRC2_CBC  = OID_PKCS5 + '.4';  // 1.2.840.113549.1.5.4
  OID_pbeWithMD5AndDES_CBC  = OID_PKCS5 + '.3';  // 1.2.840.113549.1.5.3
  OID_pbeWithMD5AndRC2_CBC  = OID_PKCS5 + '.6';  // 1.2.840.113549.1.5.6
  OID_pbeWithSHA1AndDES_CBC = OID_PKCS5 + '.10'; // 1.2.840.113549.1.5.10
  OID_pbeWithSHA1AndRC2_CBC = OID_PKCS5 + '.11'; // 1.2.840.113549.1.5.11

  OID_PBKDF2                = OID_PKCS5 + '.12'; // 1.2.840.113549.1.5.12
  OID_PBES2                 = OID_PKCS5 + '.13'; // 1.2.840.113549.1.5.13

  OID_ANSI_X9_62               = '1.2.840.10045';
  OID_PRIME_FIELD              = OID_ANSI_X9_62 + '.1.1'; // 1.2.840.10045.1.1
  OID_CHARACTERISTIC_TWO_FIELD = OID_ANSI_X9_62 + '.1.2'; // 1.2.840.10045.1.2

  OID_CHARACTERISTIC_TWO_BASIS_1 = OID_CHARACTERISTIC_TWO_FIELD + '.3.1'; // 1.2.840.10045.1.2.3.1
  OID_CHARACTERISTIC_TWO_BASIS_2 = OID_CHARACTERISTIC_TWO_FIELD + '.3.2'; // 1.2.840.10045.1.2.3.2
  OID_CHARACTERISTIC_TWO_BASIS_3 = OID_CHARACTERISTIC_TWO_FIELD + '.3.3'; // 1.2.840.10045.1.2.3.3

  OID_EC_PUBLIC_KEY            = OID_ANSI_X9_62 + '.2.1'; // 1.2.840.10045.2.1
  OID_EC_PUBLIC_KEY_RESTRICTED = OID_ANSI_X9_62 + '.2.2'; // 1.2.840.10045.2.2

  OID_ECDSA_WITH_SHA1   = OID_ANSI_X9_62 + '.4.1';   // 1.2.840.10045.4.1
  OID_ECDSA_WITH_SHA224 = OID_ANSI_X9_62 + '.4.3.1'; // 1.2.840.10045.4.3.1
  OID_ECDSA_WITH_SHA256 = OID_ANSI_X9_62 + '.4.3.2'; // 1.2.840.10045.4.3.2
  OID_ECDSA_WITH_SHA384 = OID_ANSI_X9_62 + '.4.3.3'; // 1.2.840.10045.4.3.3
  OID_ECDSA_WITH_SHA512 = OID_ANSI_X9_62 + '.4.3.4'; // 1.2.840.10045.4.3.4

  OID_secp160k1 = '1.3.132.0.9';
  OID_secp160r1 = '1.3.132.0.8';
  OID_secp160r2 = '1.3.132.0.30';
  OID_secp192k1 = '1.3.132.0.31';
  OID_secp192r1 = OID_ANSI_X9_62 + '.3.1.1'; // 1.2.840.10045.3.1.1
  OID_secp224k1 = '1.3.132.0.32';
  OID_secp224r1 = '1.3.132.0.33';
  OID_secp256k1 = '1.3.132.0.10';
  OID_secp256r1 = OID_ANSI_X9_62 + '.3.1.7'; // 1.2.840.10045.3.1.7
  OID_secp384r1 = '1.3.132.0.34';
  OID_secp521r1 = '1.3.132.0.35';
  OID_sect163k1 = '1.3.132.0.1';
  OID_sect163r1 = '1.3.132.0.2';
  OID_sect163r2 = '1.3.132.0.15';
  OID_sect193r1 = '1.3.132.0.24';
  OID_sect193r2 = '1.3.132.0.25';
  OID_sect233k1 = '1.3.132.0.26';
  OID_sect233r1 = '1.3.132.0.27';
  OID_sect239k1 = '1.3.132.0.3';
  OID_sect283k1 = '1.3.132.0.16';
  OID_sect283r1 = '1.3.132.0.17';
  OID_sect409k1 = '1.3.132.0.36';
  OID_sect409r1 = '1.3.132.0.37';
  OID_sect571k1 = '1.3.132.0.38';
  OID_sect571r1 = '1.3.132.0.39';

  OID_X25519_PUBLICKEY = '1.3.101.100';
  OID_X25519_SIGNATURE = '1.3.101.101';
  OID_X25519           = '1.3.101.110';
  OID_X448             = '1.3.101.111';
  OID_Ed25519          = '1.3.101.112';
  OID_Ed448            = '1.3.101.113';

  OID_CE                              = '2.5.29';
  OID_CE_SUBJECT_DIRECTORY_ATTRIBUTES = OID_CE + '.9';
  OID_CE_BASIC_CONSTRAINTS            = OID_CE + '.10';
  OID_CE_BASIC_CONSTRAINTS2           = OID_CE + '.19';
  OID_CE_SUBJECT_KEY_IDENTIFIER       = OID_CE + '.14';
  OID_CE_KEY_USAGE                    = OID_CE + '.15';
  OID_CE_SUBJECT_ALTERNATIVE_NAME     = OID_CE + '.17';
  OID_CE_ISSUER_ALTERNATIVE_NAME      = OID_CE + '.18';
  OID_CE_CERTIFICATE_POLICIES         = OID_CE + '.32';
  OID_CE_CERTIFICATE_ANY_POLICY       = OID_CE + '.32.0';
  OID_CE_POLICY_MAPPINGS              = OID_CE + '.33';
  OID_CE_AUTHORITY_KEY_IDENTIFIER     = OID_CE + '.35';
  OID_CE_AUTHORITY_KEY_IDENTIFIER2    = OID_CE + '.1';
  OID_CE_EXTENDED_KEY_USAGE           = OID_CE + '.37';
  OID_CE_CRL_DISTRIBUTION_POINTS      = OID_CE + '.31';
  OID_CE_FRESHEST_CRL_POINTS          = OID_CE + '.46';
  OID_CE_CRL_NUMBER                   = OID_CE + '.20';
  OID_CE_DELTA_CRL_INDICATOR          = OID_CE + '.27';
  OID_CE_ISSUING_DISTRIBUTION_POINT   = OID_CE + '.28';
  OID_CE_CRL_REASONS                  = OID_CE + '.21';
  OID_CE_CRL_INVALIDITY_DATE          = OID_CE + '.24';
  OID_CE_CRL_CERTIFICATE_ISSUER       = OID_CE + '.29';

  OID_SIGNED_CERTIFICATE_TIMESTAMP_LIST = '1.3.6.1.4.1.11129.2.4.2';

  OID_PKIX                     = '1.3.6.1.5.5.7';
  OID_PE                       = OID_PKIX + '.1';
  OID_PE_AUTHORITY_INFO_ACCESS = OID_PE + '.1';
  OID_PE_SUBJECT_INFO_ACCESS   = OID_PE + '.11';

  OID_POLICY_QUALIFIER_ID      = OID_PKIX + '.2';
  OID_POLICY_QUALIFIER_CPS     = OID_POLICY_QUALIFIER_ID + '.1';
  OID_POLICY_QUALIFIER_UNOTICE = OID_POLICY_QUALIFIER_ID + '.2';

  OID_KP                       = OID_PKIX + '.3';
  OID_KP_SERVER_AUTH           = OID_KP + '.1';
  OID_KP_CLIENT_AUTH           = OID_KP + '.2';
  OID_KP_CODE_SIGNING          = OID_KP + '.3';
  OID_KP_EMAIL_PROTECTION      = OID_KP + '.4';
  OID_KP_TIME_STAMPING         = OID_KP + '.8';
  OID_KP_OCSP_SIGNING          = OID_KP + '.9';

  OID_DATA_TYPE           = OID_PKCS7 + '.1';  // 1.2.840.113549.1.7.1
  OID_SIGNED_DATA_TYPE    = OID_PKCS7 + '.2';  // 1.2.840.113549.1.7.2
  OID_ENVELOPED_DATA_TYPE = OID_PKCS7 + '.3';  // 1.2.840.113549.1.7.3
  OID_SIGNED_AND_ENVELOPED_DATA_TYPE = OID_PKCS7 + '.4'; // 1.2.840.113549.1.7.4
  OID_DIGESTED_DATA_TYPE  = OID_PKCS7 + '.5';  // 1.2.840.113549.1.7.5
  OID_ENCRYPTED_DATA_TYPE = OID_PKCS7 + '.6';  // 1.2.840.113549.1.7.6

  OID_CONTENT_TYPE        = OID_PKCS9 + '.3';  // 1.2.840.113549.1.9.3
  OID_MESSAGE_DIGEST      = OID_PKCS9 + '.4';  // 1.2.840.113549.1.9.4
  OID_SIGNING_TIME        = OID_PKCS9 + '.5';  // 1.2.840.113549.1.9.5
  OID_COUNTERSIGNATURE    = OID_PKCS9 + '.6';  // 1.2.840.113549.1.9.6
  OID_SMIME_CAPABILITIES  = OID_PKCS9 + '.15'; // 1.2.840.113549.1.9.15

  OID_BAGTYPES            = OID_PKCS12 + '.10.1'; // 1.2.840.113549.1.12.10.1
  OID_KEY_BAG             = OID_BAGTYPES + '.1';  // 1.2.840.113549.1.12.10.1.1
  OID_PKCS8_ENC_KEY_BAG   = OID_BAGTYPES + '.2';  // 1.2.840.113549.1.12.10.1.2
  OID_CERT_BAG            = OID_BAGTYPES + '.3';  // 1.2.840.113549.1.12.10.1.3
  OID_CRL_BAG             = OID_BAGTYPES + '.4';  // 1.2.840.113549.1.12.10.1.4
  OID_SECRET_BAG          = OID_BAGTYPES + '.5';  // 1.2.840.113549.1.12.10.1.5
  OID_SAFE_CONTENTS_BAG   = OID_BAGTYPES + '.6';  // 1.2.840.113549.1.12.10.1.6

  OID_X509_CERTIFICATE_BAG_TYPE = OID_PKCS9 + '.22.1'; // 1.2.840.113549.1.9.22.1
  OID_SDSI_CERTIFICATE_BAG_TYPE = OID_PKCS9 + '.22.2'; // 1.2.840.113549.1.9.22.2
  OID_X509CRL_BAG_TYPE          = OID_PKCS9 + '.23.1'; // 1.2.840.113549.1.9.23.1

  OID_PKCS12_ATTR_FriendlyName  = OID_PKCS9 + '.20';   // 1.2.840.113549.1.9.20
  OID_PKCS12_ATTR_LocalKeyID    = OID_PKCS9 + '.21';   // 1.2.840.113549.1.9.21

  OID_PKCS12_PBE_IDS      = OID_PKCS12 + '.1';                    // 1.2.840.113549.1.12.1
  OID_pbeWithSHAAnd128BitRC4         = OID_PKCS12_PBE_IDS + '.1'; // 1.2.840.113549.1.12.1.1
  OID_pbeWithSHAAnd40BitRC4          = OID_PKCS12_PBE_IDS + '.2'; // 1.2.840.113549.1.12.1.2
  OID_pbeWithSHAAnd3KeyTripleDES_CBC = OID_PKCS12_PBE_IDS + '.3'; // 1.2.840.113549.1.12.1.3
  OID_pbeWithSHAAnd2KeyTripleDES_CBC = OID_PKCS12_PBE_IDS + '.4'; // 1.2.840.113549.1.12.1.4
  OID_pbeWithSHAAnd128BitRC2_CBC     = OID_PKCS12_PBE_IDS + '.5'; // 1.2.840.113549.1.12.1.5
  OID_pbeWithSHAAnd40BitRC2_CBC      = OID_PKCS12_PBE_IDS + '.6'; // 1.2.840.113549.1.12.1.6

  PKCS1_RSA_PRIV_HEADER = '-----BEGIN RSA PRIVATE KEY-----';
  PKCS1_RSA_PRIV_FOOTER = '-----END RSA PRIVATE KEY-----';
  PKCS1_DSA_PRIV_HEADER = '-----BEGIN DSA PRIVATE KEY-----';
  PKCS1_DSA_PRIV_FOOTER = '-----END DSA PRIVATE KEY-----';
  PKCS1_PUB_HEADER      = '-----BEGIN PUBLIC KEY-----';
  PKCS1_PUB_FOOTER      = '-----END PUBLIC KEY-----';
  PKCS1_RSA_PUB_HEADER  = '-----BEGIN RSA PUBLIC KEY-----';
  PKCS1_RSA_PUB_FOOTER  = '-----END RSA PUBLIC KEY-----';
  PKCS1_DSA_PUB_HEADER  = '-----BEGIN DSA PUBLIC KEY-----';
  PKCS1_DSA_PUB_FOOTER  = '-----END DSA PUBLIC KEY-----';
  PKCS8_HEADER          = '-----BEGIN PRIVATE KEY-----';
  PKCS8_FOOTER          = '-----END PRIVATE KEY-----';
  PKCS8_ENC_HEADER      = '-----BEGIN ENCRYPTED PRIVATE KEY-----';
  PKCS8_ENC_FOOTER      = '-----END ENCRYPTED PRIVATE KEY-----';
  IETF_PRIV_HEADER      = '---- BEGIN SSH2 ENCRYPTED PRIVATE KEY ----';
  IETF_PRIV_FOOTER      = '---- END SSH2 ENCRYPTED PRIVATE KEY ----';
  IETF_PUB_HEADER       = '---- BEGIN SSH2 PUBLIC KEY ----';
  IETF_PUB_FOOTER       = '---- END SSH2 PUBLIC KEY ----';
  EC_PARAM_HEADER       = '-----BEGIN EC PARAMETERS-----';
  EC_PARAM_FOOTER       = '-----END EC PARAMETERS-----';
  EC_PRIV_HEADER        = '-----BEGIN EC PRIVATE KEY-----';
  EC_PRIV_FOOTER        = '-----END EC PRIVATE KEY-----';
  OPENSSH_PRIV_HEADER   = '-----BEGIN OPENSSH PRIVATE KEY-----';
  OPENSSH_PRIV_FOOTER   = '-----END OPENSSH PRIVATE KEY-----';
  CERT_HEADER           = '-----BEGIN CERTIFICATE-----';
  CERT_FOOTER           = '-----END CERTIFICATE-----';
  CRL_HEADER            = '-----BEGIN X509 CRL-----';
  CRL_FOOTER            = '-----END X509 CRL-----';
  CMS_HEADER            = '-----BEGIN CMS-----';
  CMS_FOOTER            = '-----END CMS-----';
  CMS_MIME_HEADER       = 'MIME-Version';

  PUTTY_KEY_HEADER_2 = 'PuTTY-User-Key-File-2';

  ALGORITHM_IDENTIFIER_DESC = 'SEQ {"Algorithm" OID, "Parameters" OPTIONAL ANY}';

  PKCS1_MGF_ALGORITHM_DESC =
    'SEQ {"Algorithm" OID, "Parameters" OPTIONAL ' + ALGORITHM_IDENTIFIER_DESC + '}';

  RSAES_OAEP_PARAMS_DESC =
    'SEQ {"HashAlgorithm" OPTIONAL [0] ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"MaskGenAlgorithm" OPTIONAL [1] ' + PKCS1_MGF_ALGORITHM_DESC + ',' +
      '"PSourceAlgorithm" OPTIONAL [2] ANY' +
    '}';

  RSASSA_PSS_PARAMS_DESC =
    'SEQ {"HashAlgorithm" OPTIONAL [0] ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"MaskGenAlgorithm" OPTIONAL [1] ' + PKCS1_MGF_ALGORITHM_DESC + ',' +
      '"SaltLength" OPTIONAL [2] INT,' +
      '"TrailerField" OPTIONAL [3] INT' +
    '}';

  RSA_PRIVATE_KEY_PKCS1_DESC =
    'SEQ {"Version" INT, "PubMod" INT, "PubExp" INT, "D" INT, "P" INT, "Q" INT, "DP" INT, "DQ" INT, "U" INT}';
  RSA_PUBLIC_KEY_PKCS1_DESC =
    'SEQ {"PubMod" INT, "PubExp" INT}';
  DSA_PRIVATE_KEY_PKCS1_DESC =
    'SEQ {"Version" INT, "P" INT, "Q" INT, "G" INT, "Y" INT, "X" INT}';
  DSA_PUBLIC_KEY_PKCS1_DESC =
    'SEQ {"P" INT, "Q" INT, "G" INT, "Y" INT}';
  DSA_TRUNC_PUBLIC_KEY_PKCS1_DESC =
    'SEQ {"P" INT, "Q" INT, "G" INT}';

  DSA_CERT_PUBLIC_KEY_DESC = '"Y" INT';

  RSA_PRIVATE_KEY_OPENSSL_DESC =
    'SEQ {' +
      'SEQ {OID(' + OID_RSA_ENCRYPTION + '), NULL},' +
      'OCTSTR {' + RSA_PRIVATE_KEY_PKCS1_DESC + '}' +
    '}';

  RSA_PUBLIC_KEY_OPENSSL_DESC =
    'SEQ {' +
      'SEQ {OID(' + OID_RSA_ENCRYPTION + '), NULL},' +
      'OCTSTR {SEQ {"Version" INT, "PubMod" INT, "PubExp" INT}}' +
    '}';

  RSA_PUBLIC_KEY_OPENSSL_DESC2 =
    'SEQ {' +
      'SEQ {OID(' + OID_RSA_ENCRYPTION + '), NULL},' +
      'BIT {SEQ {"PubMod" INT, "PubExp" INT}}' +
    '}';

  DSA_PRIVATE_KEY_OPENSSL_DESC =
    'SEQ {' +
      'SEQ {OID(' + OID_DSA_ENCRYPTION + '), ' + DSA_TRUNC_PUBLIC_KEY_PKCS1_DESC + '},' +
      'OCTSTR {"X" INT}'+
    '}';

  DSA_PUBLIC_KEY_OPENSSL_DESC =
    'SEQ {' +
      'SEQ {OID(' + OID_DSA_ENCRYPTION + '), ' + DSA_TRUNC_PUBLIC_KEY_PKCS1_DESC + '}' +
      'BIT {"Y" INT}'+
    '}';

  PKCS8_KEY_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"PrivateKeyAlgorithm" SEQ {"Algorithm" OID, "Parameters" OPTIONAL ANY},' +
      '"PrivateKey" OCTSTR ' +
    '}';

  RSA_PKCS8_KEY_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"PrivateKeyAlgorithm" SEQ {"Algorithm" OID(' + OID_RSA_ENCRYPTION + '), "Parameters" NULL},' +
      '"PrivateKey" OCTSTR {' + RSA_PRIVATE_KEY_PKCS1_DESC + '}' +
    '}';

  DSA_PRIVATE_KEY_PKCS8_DESC = '"X" INT';

  DSA_PKCS8_KEY_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"PrivateKeyAlgorithm" SEQ {"Algorithm" OID(' + OID_DSA_ENCRYPTION + '), "Parameters" ' + DSA_TRUNC_PUBLIC_KEY_PKCS1_DESC + '},' +
      '"PrivateKey" OCTSTR {' + DSA_PRIVATE_KEY_PKCS8_DESC + '}'+
    '}';

  PRIVATE_KEY_PKCS8ENC_DESC =
    'SEQ {' +
      '"EncryptionAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"EncryptedData" OCTSTR' +
    '}';

  PBES1_PARAMS_DESC =
    'SEQ {' +
      '"Salt" OCTSTR,' +
      '"Iters" INT' +
    '}';

  PKCS12_PBE_PARAMS_DESC =
    'SEQ {' +
      '"Salt" OCTSTR,' +
      '"Iters" INT' +
    '}';

  PBES2_PARAMS_DESC =
    'SEQ {' +
      '"KeyDerivationFunc"  SEQ {OID(' + OID_PBKDF2 + '), SEQ {"Salt" OCTSTR, "Iters" INT}},' +
      '"EncryptionScheme" SEQ {"Algo" OID, "IV" OCTSTR}' +
    '}';

  PRIVATE_KEY_PKCS8_PBES2ENC_DESC =
    'SEQ {' +
      '"EncryptionAlgorithm" SEQ {' +
        '"Algorithm" OID(' + OID_PBES2 + '),' +
        '"Parameters" ' + PBES2_PARAMS_DESC +
      '},' +
      '"EncryptedData" OCTSTR' +
    '}';

  // tools.ietf.org/html/draft-ietf-pkix-ecc-pkalgs-01
  // www.secg.org/sec1-v2.pdf
  EC_FIELD_ID_DESC =
    'SEQ {' +
      '"FieldType" OID,' +
      '"Params" CHOICE {' +
        '"PrimeP" INT, ' +
        '"CharacteristicTwo" SEQ {' +
          '"M" INT,' +
          '"Basis" OID,' +
          '"Params" CHOICE {' +
            '"NULL" NULL,' +
            '"Trinomial" INT,' +
            '"Pentanomial" SEQ {' +
              '"k1" INT,' +
              '"k2" INT,' +
              '"k3" INT' +
            '},' +
            '"Other" ANY' +
          '}' +
        '}' +
      '}' +
    '}';

  EC_CURVE_DESC =
    'SEQ {' +
      '"A" OCTSTR,' +
      '"B" OCTSTR,' +
      '"Seed" OPTIONAL BIT,' +
    '}';

  EC_POINT_DESC = 'OCTSTR';

  EC_SPECIFIED_DOMAIN_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"FieldID" ' + EC_FIELD_ID_DESC + ',' +
      '"Curve" ' + EC_CURVE_DESC + ',' +
      '"Base" ' + EC_POINT_DESC + ',' +
      '"Order" INT,' +
      '"Cofactor" OPTIONAL INT,' +
      '"Hash" OPTIONAL ' + ALGORITHM_IDENTIFIER_DESC +
    '}';

  EC_DOMAIN_PARAMS_DESC =
    'CHOICE {' +
      '"Named" OID,' +
      '"Specified" ' + EC_SPECIFIED_DOMAIN_DESC + ',' +
      '"ImplicitCA" NULL' +
    '}';

  EC_PRIVATE_KEY_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"PrivateKey" OCTSTR,' +
      '"Params" OPTIONAL [0] ' + EC_DOMAIN_PARAMS_DESC + ',' +
      '"PublicKey" OPTIONAL [1] BIT' +
    '}';

  EC_PK_RESTRICTIONS_DESC =
    'SEQ {' +
      '"EcDomain" ' + EC_DOMAIN_PARAMS_DESC + ',' +
      '"EccAlgorithms" SEQ[] {' + ALGORITHM_IDENTIFIER_DESC + '}' +
    '}';

  EC_PUBLIC_KEY_DESC =
    'SEQ {' +
      '"Algorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"SubjectPublicKey" BIT' +
    '}';

  X25519_PUBLIC_KEY_PARAMS_DESC =
    '"Parameters" ENUM';

  EC_PKCS8_KEY_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"PrivateKeyAlgorithm" SEQ {"Algorithm" OID(' + OID_EC_PUBLIC_KEY + '), "Parameters" ANY},' +
      '"PrivateKey" OCTSTR {' + EC_PRIVATE_KEY_DESC + '}' +
    '}';

  Ed25519_PKCS8_KEY_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"PrivateKeyAlgorithm" SEQ {"Algorithm" OID(' + OID_Ed25519 + '), "Parameters" ANY},' +
      '"PrivateKey" OCTSTR {"PrivateKeyPoint" ' + EC_POINT_DESC + '}' +
    '}';

  EC_SIGN_DESC = 'SEQ {"R" INT, "S" INT}';

  ATTRIBUTE_DESC = 'SEQ {"Type" OID, "Values" SET[] {"Value" ANY}}';

  ATTRIBUTES_DESC = 'SET[] {' + ATTRIBUTE_DESC + '}';

  ATTR_AND_VALUE_DESC = '"AttrType" OID, "AttrValue" ANY';

  RDN_FORMAT = 'SET[] {"Attr" SEQ {' + ATTR_AND_VALUE_DESC + '}}';

  DISTINGUISHED_NAME_FORMAT = 'SEQ[] {"Attrs" ' + RDN_FORMAT + '}';

  DISTINGUISHED_NAMES = '"Names" ' + DISTINGUISHED_NAME_FORMAT;

  EXTENSIONS_DESC =
    'SEQ[] {SEQ {"ExtnID" OID, "Critical" OPTIONAL BOOL, "ExtnValue" OCTSTR}}';

  CERTIFICATE_DESC =
    'SEQ {' +
      '"TBSCertificate" SEQ {' +
        '"Version" OPTIONAL [0] INT,' +
        '"SerialNumber" INT,' +
        '"Signature" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
        '"Issuer" ' + DISTINGUISHED_NAME_FORMAT + ',' +
        '"Validity" SEQ {"NotBefore" CHOICE {UTCTIME, GENTIME}, "NotAfter" CHOICE {UTCTIME, GENTIME}},' +
        '"Subject" ' + DISTINGUISHED_NAME_FORMAT + ',' +
        '"SubjectPublicKeyInfo" SEQ {"Algorithm" ' + ALGORITHM_IDENTIFIER_DESC + ', "SubjectPublicKey" BIT},' +
        '"IssuerUniqueID" OPTIONAL [1] IMPLICIT BIT,' +
        '"SubjectUniqueID" OPTIONAL [2] IMPLICIT BIT,' +
        '"Extensions" OPTIONAL [3] ' + EXTENSIONS_DESC +
      '},' +
      '"SignatureAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"SignatureValue" BIT' +
    '}';

  GENERAL_NAME_DESC =
    'CHOICE {' +
      '"OtherName" [0] IMPLICIT SEQ {"TypeId" OID, "Value" [0] ANY},' +
      '"Rfc822Name" [1] IMPLICIT IA5STR,' +
      '"DNSName" [2] IMPLICIT IA5STR,' +
      '"X400Address" [3] IMPLICIT SEQ {ANY},' +
      '"DirectoryName" [4] ' + DISTINGUISHED_NAME_FORMAT + ',' +
      '"EdiPartyName" [5] IMPLICIT SEQ {"NameAssigner" OPTIONAL [0] ANY, "PartyName" [1] ANY},' +
      '"URL" [6] IMPLICIT IA5STR,' +
      '"IPAddress" [7] IMPLICIT OCTSTR,' +
      '"RegisteredID" [8] IMPLICIT OID' +
    '}';

  GENERAL_NAMES_DESC =
    'SEQ[] {"Name" ' + GENERAL_NAME_DESC + '}';

  BASIC_CONSTRAINTS_EXTENSION_DESC = '"Info" SEQ {"SubjectType" OPTIONAL BIT, "Len" OPTIONAL INT}';

  BASIC_CONSTRAINTS2_EXTENSION_DESC = '"Info" SEQ {"CA" OPTIONAL BOOL, "Len" OPTIONAL INT}';

  KEY_USAGE_EXTENSION_DESC = '"Data" BIT';

  EXTENDED_KEY_USAGE_EXTENSION_DESC = '"Usage" SEQ[] {"ID" OID}';

  SUBJECT_KEY_ID_EXTENSION_DESC = '"Data" OCTSTR';

  AUTHORITY_KEY_ID_EXTENSION_DESC =
    'SEQ {' +
      '"KeyID" OPTIONAL [0] IMPLICIT OCTSTR,' +
      '"Issuer" OPTIONAL [1] IMPLICIT ' + GENERAL_NAMES_DESC + ',' +
      '"SerialNumber" OPTIONAL [2] IMPLICIT INT' +
    '}';

  AUTHORITY_KEY_ID_EXTENSION_DESC2 =
    'SEQ {' +
      '"KeyID" OPTIONAL [0] IMPLICIT OCTSTR,' +
      '"Issuer" OPTIONAL [1] IMPLICIT SEQ[] {"DirectoryName" ' + DISTINGUISHED_NAME_FORMAT + '},' +
      '"SerialNumber" OPTIONAL [2] IMPLICIT INT' +
    '}';

  DISPLAY_TEXT_DESC =
    'CHOICE {IA5STR, VISIBLESTR, BMPSTR, UTF8STR}';

  POLICY_QUALIFIER_DESC =
    'CHOICE {' +
      '"CPSuri" IA5STR,' +
      '"UserNotice" SEQ {' +
        '"NoticeRef" OPTIONAL SEQ {' +
          '"Organization" ' + DISPLAY_TEXT_DESC + ',' +
          '"NoticeNumbers" SEQ[] {INT}' +
        '}, ' +
        '"ExplicitText" OPTIONAL ' + DISPLAY_TEXT_DESC +
      '}'+
    '}';

  CERTIFICATE_POLICIES_EXTENSION_DESC =
    '"Policies" SEQ[] {SEQ {' +
      '"Identifier" OID,' +
      '"Qualifiers" OPTIONAL SEQ[] {SEQ {' +
        '"QualifierId" OID, ' +
        '"Qualifier" ' + POLICY_QUALIFIER_DESC +
      '}}' +
    '}}';

  POLICY_MAPPINGS_EXTENSION_DESC =
    '"PolicyMappings" SEQ[] {SEQ {' +
      '"IssuerDomainPolicy" OID,' +
      '"SubjectDomainPolicy" OID' +
    '}}';

  ALTERNATIVE_NAME_EXTENSION_DESC =
    '"GeneralNames" ' + GENERAL_NAMES_DESC;

  SUBJECT_DIRECTORY_ATTRIBUTES_EXTENSION_DESC = '"Attributes" ' + ATTRIBUTES_DESC;

  DISTRIBUTION_POINT_NAME_DESC =
    'CHOICE {' +
      '"FullName" [0] IMPLICIT ' + GENERAL_NAMES_DESC + ',' +
      '"NameRelativeToCRLIssuer" [1] IMPLICIT ' + RDN_FORMAT +
    '}';

  CRL_DISTRIBUTION_POINTS_EXTENSION_DESC =
    '"CRLDistributionPoints" SEQ[] {SEQ {' +
      '"DistributionPoint" OPTIONAL [0] ' + DISTRIBUTION_POINT_NAME_DESC + ',' +
      '"Reasons" OPTIONAL [1] IMPLICIT BIT,' +
      '"CRLIssuer" OPTIONAL [2] IMPLICIT ' + GENERAL_NAMES_DESC +
    '}}';

  ACCESS_DESCRIPTION_DESC =
    '"AccessDescription" SEQ {' +
      '"AccessMethod" OID,' +
      '"AccessLocation" ' + GENERAL_NAME_DESC +
    '}';

  AUTHORITY_INFO_ACCESS_EXTENSION_DESC =
    '"AuthorityInfoAccess" SEQ[] { ' + ACCESS_DESCRIPTION_DESC + '}';

  SUBJECT_INFO_ACCESS_EXTENSION_DESC =
    '"SubjectInfoAccess" SEQ[] { ' + ACCESS_DESCRIPTION_DESC + '}';

  SIGNED_CERTIFICATE_TIMESTAMP_LIST_DESC =
    'OCTSTR';

  CRL_NUMBER_EXTENSION_DESC =
    '"CRLNumber" INT';

  CRL_ISSUING_DISTRIBUTION_POINT_EXTENSION_DESC =
    '"IssuingDistributionPoint" SEQ {' +
      '"DistributionPoint" OPTIONAL [0] ' + DISTRIBUTION_POINT_NAME_DESC + ',' +
      '"OnlyContainsUserCerts" OPTIONAL [1] IMPLICIT BOOL,' +
      '"OnlyContainsCACerts" OPTIONAL [2] IMPLICIT BOOL,' +
      '"OnlySomeReasons" OPTIONAL [3] IMPLICIT BIT,' +
      '"IndirectCRL" OPTIONAL [4] IMPLICIT BOOL,' +
      '"OnlyContainsAttributeCerts" OPTIONAL [5] IMPLICIT BOOL,' +
    '}}';

  CRL_REASON_EXTENSION_DESC =
    '"CRLReason" ENUM';

  CRL_INVALIDITY_DATE_EXTENSION_DESC =
    '"InvalidityDate" CHOICE {UTCTIME, GENTIME}';

  CRL_CERTIFICATE_ISSUER_EXTENSION_DESC =
    '"CertificateIssuer" ' + GENERAL_NAMES_DESC;

  TBS_CERTIFICATE_LIST_DESC =
    'SEQ {' +
      '"Version" OPTIONAL INT,' +
      '"Signature" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"Issuer" ' + DISTINGUISHED_NAME_FORMAT + ',' +
      '"ThisUpdate" CHOICE {UTCTIME, GENTIME},' +
      '"NextUpdate" OPTIONAL CHOICE {UTCTIME, GENTIME},' +
      '"RevokedCertificates" OPTIONAL SEQ[] {SEQ {' +
        '"UserCertificate" INT,' +
        '"RevocationDate" CHOICE {UTCTIME, GENTIME},' +
        '"CrlEntryExtensions" OPTIONAL ANY' + // ANY - EXTENSIONS_DESC
      '}},' +
      '"CrlExtensions" OPTIONAL [0] ' + EXTENSIONS_DESC +
    '}';

  PARTIAL_TBS_CERTIFICATE_LIST_DESC =
    'SEQ {' +
      '"Version" OPTIONAL INT,' +
      '"Signature" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"Issuer" ' + DISTINGUISHED_NAME_FORMAT + ',' +
      '"ThisUpdate" CHOICE {UTCTIME, GENTIME},' +
      '"NextUpdate" OPTIONAL CHOICE {UTCTIME, GENTIME},' +
      '"RevokedCertificates" OPTIONAL ANY,' +
      '"CrlExtensions" OPTIONAL [0] ' + EXTENSIONS_DESC +
    '}';

  PARTIAL_CERTIFICATE_LIST_DESC =
    'SEQ {' +
      '"TBSCertList" ' + PARTIAL_TBS_CERTIFICATE_LIST_DESC + ',' +
      '"SignatureAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"SignatureValue" BIT' +
    '}';

  CERTIFICATE_LIST_DESC =
    'SEQ {' +
      '"TBSCertList" ' + TBS_CERTIFICATE_LIST_DESC + ',' +
      '"SignatureAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"SignatureValue" BIT' +
    '}';

// ===========================================================================
// CMS
// ===========================================================================
  CONTENT_INFO_DESC =
    'SEQ {' +
      '"ContentType" OID,' +
      '"Content" [0] ANY' +
    '}';

  ENCAPSULATED_CONTENT_INFO_DESC =
    'SEQ {' +
      '"eContentType" OID,' +
      '"eContent" OPTIONAL [0] OCTSTR' +
    '}';

  SMIME_ATTRIBUTE_DESC = '"Attrs" ARRAY[] {SEQ {"AttrType" OID, "AttrValue" OPTIONAL ANY}}';

  EXTENDED_CERTIFICATE_INFO_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"Certificate" ' + CERTIFICATE_DESC + ',' +
      '"Attributes" ' + ATTRIBUTES_DESC +
    '}';

  EXTENDED_CERTIFICATE_DESC =
    'SEQ {' +
      '"ExtendedCertificateInfo" ' + EXTENDED_CERTIFICATE_INFO_DESC + ',' +
      '"SignatureAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"Signature" BIT' +
    '}';
  ATTRIBUTE_CERTIFICATE_V1_DESC = 'SEQ {ANY}';

  ATTRIBUTE_CERTIFICATE_INFO_DESC = 'ANY';
  ATTRIBUTE_CERTIFICATE_V2_DESC =
    'SEQ {' +
      '"Acinfo" ' + ATTRIBUTE_CERTIFICATE_INFO_DESC + ',' +
      '"SignatureAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"SignatureValue" BIT' +
    '}';

  OTHER_CERTIFICATE_FORMAT_DESC = 'SEQ {"OtherCertFormat" OID, "OtherCert" ANY}';

  CERTIFICATE_SET_DESC =
    'SET[] {' +
      '"CertificateDesc" CHOICE {' +
         '"Certificate" ' + CERTIFICATE_DESC + ',' +
         '"ExtendedCertificate" [0] IMPLICIT ' + EXTENDED_CERTIFICATE_DESC + ',' +
         '"v1AttrCert" [1] IMPLICIT ' + ATTRIBUTE_CERTIFICATE_V1_DESC + ',' +
         '"v2AttrCert" [2] IMPLICIT ' + ATTRIBUTE_CERTIFICATE_V2_DESC + ',' +
         '"Other" [3] IMPLICIT ' + OTHER_CERTIFICATE_FORMAT_DESC +
      '}' +
    '}';

  OTHER_REVOCATION_INFO_FORMAT_DESC = 'SEQ {"OtherRevInfoFormat" OID, "OtherRevInfo" ANY}';

  REVOCATION_INFO_CHOICE_DESC =
    'CHOICE {' +
      '"Crl" ' + CERTIFICATE_LIST_DESC + ', ' +
      '"Other" [1] IMPLICIT ' + OTHER_REVOCATION_INFO_FORMAT_DESC +
    '}';
  REVOCATION_INFO_CHOICES_DESC = 'SET[] {' + REVOCATION_INFO_CHOICE_DESC + '}';

  ISSUER_AND_SERIAL_NUMBER_DESC = 'SEQ {"Issuer" ' + DISTINGUISHED_NAME_FORMAT + ',  "SerialNumber" INT}';
  SUBJECT_KEY_IDENTIFIER_DESC = '[0] IMPLICIT OCTSTR';

  SIGNER_IDENTIFIER_DESC =
    'CHOICE {' +
      '"IssuerAndSerialNumber" ' + ISSUER_AND_SERIAL_NUMBER_DESC + ',' +
      '"SubjectKeyIdentifier" ' + SUBJECT_KEY_IDENTIFIER_DESC +
    '}';

  SIGNED_ATTRS_DESC = '[0] IMPLICIT ' + ATTRIBUTES_DESC;
  UNSIGNED_ATTRS_DESC = '[1] IMPLICIT ' + ATTRIBUTES_DESC;

  SIGNER_INFO_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"Sid" ' + SIGNER_IDENTIFIER_DESC + ',' +
      '"DigestAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"SignedAttrs" OPTIONAL ' + SIGNED_ATTRS_DESC + ',' +
      '"SignatureAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"Signature" OCTSTR,' +
      '"UnsignedAttrs" OPTIONAL ' + UNSIGNED_ATTRS_DESC +
    '}';

  SIGNED_DATA_DESC =
    'SEQ {' +
      '"ContentType" OID,' +
      '"Content" [0] SEQ {' +
        '"Version" INT,' +
        '"DigestAlgorithms" SET[] {' + ALGORITHM_IDENTIFIER_DESC + '},' +
        '"EncapContentInfo" ' + ENCAPSULATED_CONTENT_INFO_DESC + ',' +
        '"Certificates" OPTIONAL [0] IMPLICIT ' + CERTIFICATE_SET_DESC + ',' +
        '"Crls" OPTIONAL [1] IMPLICIT ' + REVOCATION_INFO_CHOICES_DESC + ',' +
        '"SignerInfos" SET[] {' + SIGNER_INFO_DESC + '}' +
      '}' +
    '}';

  ORIGINATOR_INFO_DESC =
    'SEQ {' +
      '"Certificates" OPTIONAL [0] IMPLICIT ' + CERTIFICATE_SET_DESC + ',' +
      '"Crls" OPTIONAL [1] IMPLICIT ' + REVOCATION_INFO_CHOICES_DESC +
    '}';

  RECIPIENT_IDENTIFIER_DESC =
    'CHOICE {' +
      '"IssuerAndSerialNumber" ' + ISSUER_AND_SERIAL_NUMBER_DESC + ',' +
      '"SubjectKeyIdentifier" ' + SUBJECT_KEY_IDENTIFIER_DESC +
    '}';

  KEY_TRANS_RECIPIENT_INFO_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"RecipientIdentifier" ' + RECIPIENT_IDENTIFIER_DESC + ',' +
      '"KeyEncryptionAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"EncryptedKey" OCTSTR' +
    '}';

  PUBLIC_KEY_DESC =
    '[1] SEQ {' +
      '"Algorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"PublicKey" BIT' +
    '}';

  ORIGINATOR_IDENTIFIER_OR_KEY_DESC =
    'CHOICE {' +
      '"IssuerAndSerialNumber" ' + ISSUER_AND_SERIAL_NUMBER_DESC + ',' +
      '"SubjectKeyIdentifier" ' + SUBJECT_KEY_IDENTIFIER_DESC + ',' +
      '"PublicKey" ' + PUBLIC_KEY_DESC +
    '}';

  OTHER_KEY_ATTRIBUTE_DESC =
    'SEQ {' +
      '"KeyAttrId" OID,' +
      '"KeyAttr" OPTIONAL ANY' +
    '}';

  RECIPIENT_KEY_IDENTIFIER_DESC =
    '[0] IMPLICIT SEQ {' +
      '"SubjectKeyIdentifier" OCTSTR,' +
      '"Date" OPTIONAL GENTIME,' +
      '"OtherKeyAttribute" OPTIONAL ' + OTHER_KEY_ATTRIBUTE_DESC +
    '}';

  KEY_AGREE_RECIPIENT_IDENTIFIER_DESC =
    'CHOICE {' +
      '"IssuerAndSerialNumber" ' + ISSUER_AND_SERIAL_NUMBER_DESC + ',' +
      '"KeyId" ' + RECIPIENT_KEY_IDENTIFIER_DESC +
    '}';

  RECIPIENT_ENCRYPTED_KEY_DESC =
    'SEQ {' +
      '"Rid" ' + KEY_AGREE_RECIPIENT_IDENTIFIER_DESC + ',' +
      '"EncryptedKey" OCTSTR' +
    '}';

  RECIPIENT_ENCRYPTED_KEYS_DESC = 'SEQ[] {"RecipientEncryptedKey" ' + RECIPIENT_ENCRYPTED_KEY_DESC + '}';

  KEY_AGREE_RECIPIENT_INFO_DESC =
    '[1] SEQ {' +
      '"Version" INT,' +
      '"Originator" [0] ' + ORIGINATOR_IDENTIFIER_OR_KEY_DESC + ',' +
      '"Ukm" OPTIONAL [1] OCTSTR,' +
      '"KeyEncryptionAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"RecipientEncryptedKeys" ' + RECIPIENT_ENCRYPTED_KEYS_DESC +
    '}';

  KEK_IDENTIFIER_DESC =
    'SEQ {' +
      '"KeyIdentifier" OCTSTR,' +
      '"Date" OPTIONAL GENTIME,' +
      '"OtherKeyAttribute" OPTIONAL ' + OTHER_KEY_ATTRIBUTE_DESC +
    '}';

  KEK_RECIPIENT_INFO_DESC =
    '[2] SEQ {' +
      '"Version" INT,' +
      '"KEKIdentifier" ' + KEK_IDENTIFIER_DESC + ',' +
      '"KeyEncryptionAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"EncryptedKey" OCTSTR' +
    '}';

  PASSWORD_RECIPIENT_INFO_DESC =
    '[3] SEQ {' +
      '"Version" INT,' +
      '"KeyDerivationAlgorithm" OPTIONAL [0] ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"KeyEncryptionAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"EncryptedKey" OCTSTR' +
    '}';

  OTHER_RECIPIENT_INFO_DESC =
    '[4] SEQ {' +
      '"OriType" OID,' +
      '"OriValue" ANY' +
    '}';

  RECIPIENT_INFO_DESC =
    'CHOICE {' +
      '"ktri" ' + KEY_TRANS_RECIPIENT_INFO_DESC + ',' +
      '"kari" ' + KEY_AGREE_RECIPIENT_INFO_DESC + ',' +
      '"kekri" ' + KEK_RECIPIENT_INFO_DESC + ',' +
      '"pwri" ' + PASSWORD_RECIPIENT_INFO_DESC + ',' +
      '"ori" ' + OTHER_RECIPIENT_INFO_DESC +
    '}';

  RECIPIENT_INFOS_DESC = 'SET[] {"RecipientInfo" ' + RECIPIENT_INFO_DESC + '}';

  ENCRYPTED_CONTENT_INFO_DESC =
    'SEQ {' +
      '"ContentType" OID,' +
      '"ContentEncryptionAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"EncryptedContent" OPTIONAL [0] IMPLICIT OCTSTR' +
    '}';

  ENVELOPED_DATA_DESC =
    'SEQ {' +
      '"ContentType" OID,' +
      '"Content" [0] SEQ {' +
        '"Version" INT,' +
        '"OriginatorInfo" OPTIONAL [0] IMPLICIT ' + ORIGINATOR_INFO_DESC + ',' +
        '"RecipientInfos" ' + RECIPIENT_INFOS_DESC + ',' +
        '"EncryptedContentInfo" ' + ENCRYPTED_CONTENT_INFO_DESC + ',' +
        '"UnprotectedAttributes" OPTIONAL [1] IMPLICIT ' + ATTRIBUTES_DESC +
      '}' +
    '}';

  ENCRYPTED_DATA_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"EncryptedContentInfo" ' + ENCRYPTED_CONTENT_INFO_DESC +
    '}';

// ===========================================================================
// PKCS #12
// ===========================================================================
  DIGEST_INFO_DESC =
    'SEQ {' +
      '"DigestAlgorithm" ' + ALGORITHM_IDENTIFIER_DESC + ',' +
      '"Digest" OCTSTR' +
    '}';

  MAC_DATA_DESC =
    'SEQ {' +
      '"Mac" ' + DIGEST_INFO_DESC + ',' +
      '"MacSalt" OCTSTR,' +
      '"Iters" OPTIONAL INT' +
    '}';

  PFX_DESC =
    'SEQ {' +
      '"Version" INT,' +
      '"AuthSafe" ' + CONTENT_INFO_DESC + ',' +
      '"MacData" OPTIONAL ' + MAC_DATA_DESC +
    '}';

  AUTH_SAFE_DESC =
    '"AuthSafes" SEQ[] {"AuthSafei"' + CONTENT_INFO_DESC + '}';

  AUTH_SAFE_DESC_EX =
    'CHOICE {' +
      'OCTSTR {' + AUTH_SAFE_DESC + '}, ' +
      AUTH_SAFE_DESC +
    '}';

  SAFE_BAG_DESC =
    'SEQ {' +
      '"BagID" OID,' +
      '"BagValue" [0] ANY,' +
      '"BagAttributes" OPTIONAL SET[] { SEQ {' +
        '"AttrId" OID,' +
        '"AttrValues" SET[] {"AttrValue" ANY}' +
      '}}' +
    '}';

  SAFE_CONTENTS_DESC =
    '"SafeBags" SEQ[] { ' + SAFE_BAG_DESC + '}';

  SAFE_CONTENTS_DESC_EX =
    'CHOICE {' +
      'OCTSTR {' + SAFE_CONTENTS_DESC + '}, ' +
      SAFE_CONTENTS_DESC +
    '}';

  SAFE_BAG_VALUE_DESC =
    'SEQ {' +
      '"TypeID" OID,' +
      '"Value" [0] CHOICE{OCTSTR, IA5STR, ANY}' +
    '}';

  /// PEM encryption info
  PEM_PROC_TYPE_LINE = 'Proc-Type: 4,ENCRYPTED';
  PEM_INFO_LINE      = 'DEK-Info: ';
  PEM_3DESCBC_ALG    = 'DES-EDE3-CBC';
  PEM_AES128CBC_ALG  = 'AES-128-CBC';
  PEM_AES192CBC_ALG  = 'AES-192-CBC';
  PEM_AES256CBC_ALG  = 'AES-256-CBC';
  PEM_AES128CTR_ALG  = 'AES-128-CTR';
  PEM_AES192CTR_ALG  = 'AES-192-CTR';
  PEM_AES256CTR_ALG  = 'AES-256-CTR';
  COMMENT_LINE       = 'Comment:';

  // IETF info
  IETF_RSA = 'if-modn{sign{rsa-pkcs1-sha1},encrypt{rsa-pkcs1v2-oaep}}';
  IETF_DSA = 'dl-modp{sign{dsa-nist-sha1},dh{plain}}';
  IETF_MAGIC_VAL = $3f6ff9eb;

  // OPENSSH info
  OPENSSH_AUTH_MAGIC = 'openssh-key-v1'#0;

implementation

end.
