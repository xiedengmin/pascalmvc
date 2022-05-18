
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Tds.inc}
unit TdsCryptoAPIIntfUni;

{$ALIGN 8}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  {$IFNDEF ANDROID}{$IFNDEF LINUX}Macapi.CoreFoundation,{$ENDIF}{$ENDIF}
  System.Types, Posix.Dlfcn,
{$ENDIF}
{$IFDEF DARWIN}
  MacTypes, CFBase, CFData, CFArray, CFDictionary, CFNumber, Unix, dl,
{$ENDIF}
  SysUtils,
  CLRClasses, CRTypes;

{$IFDEF MSWINDOWS}
  {$HPPEMIT '#ifdef CryptAcquireContext'}
  {$HPPEMIT '#undef CryptAcquireContext'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#ifdef CryptEnumProviders'}
  {$HPPEMIT '#undef CryptEnumProviders'}
  {$HPPEMIT '#endif'}

type
  PVOID = IntPtr;
  {$NODEFINE PVOID}
  LONG = DWORD;
  {$NODEFINE LONG}

{$IFDEF UNICODE}
  LPAWSTR = PWideChar;
{$ELSE}
  LPAWSTR = PAnsiChar;
{$ENDIF}
  ILPCSTR = LPCSTR;

const
  ADVAPI32 = 'advapi32.dll';
  {$EXTERNALSYM ADVAPI32}
  CRYPT32 = 'crypt32.dll';
  {$EXTERNALSYM CRYPT32}

const
// Provider friendly names
  MS_DEF_PROV_A = 'Microsoft Base Cryptographic Provider v1.0';
  {$EXTERNALSYM MS_DEF_PROV_A}
  MS_DEF_PROV_W = WideString('Microsoft Base Cryptographic Provider v1.0');
  {$EXTERNALSYM MS_DEF_PROV_W}

{$IFDEF UNICODE}
  MS_DEF_PROV = MS_DEF_PROV_W;
{$ELSE}
  MS_DEF_PROV = MS_DEF_PROV_A;
{$ENDIF}
  {$EXTERNALSYM MS_DEF_PROV}

  MS_ENHANCED_PROV_A = 'Microsoft Enhanced Cryptographic Provider v1.0';
  {$EXTERNALSYM MS_ENHANCED_PROV_A}
  MS_ENHANCED_PROV_W = WideString('Microsoft Enhanced Cryptographic Provider v1.0');
  {$EXTERNALSYM MS_ENHANCED_PROV_W}

{$IFDEF UNICODE}
  MS_ENHANCED_PROV = MS_ENHANCED_PROV_W;
{$ELSE}
  MS_ENHANCED_PROV = MS_ENHANCED_PROV_A;
{$ENDIF}
  {$EXTERNALSYM MS_ENHANCED_PROV}

  MS_DEF_RSA_SIG_PROV_A = 'Microsoft RSA Signature Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_RSA_SIG_PROV_A}
  MS_DEF_RSA_SIG_PROV_W = WideString('Microsoft RSA Signature Cryptographic Provider');
  {$EXTERNALSYM MS_DEF_RSA_SIG_PROV_W}

{$IFDEF UNICODE}
  MS_DEF_RSA_SIG_PROV = MS_DEF_RSA_SIG_PROV_W;
{$ELSE}
  MS_DEF_RSA_SIG_PROV = MS_DEF_RSA_SIG_PROV_A;
{$ENDIF}
  {$EXTERNALSYM MS_DEF_RSA_SIG_PROV}

  MS_DEF_RSA_SCHANNEL_PROV_A = 'Microsoft RSA SChannel Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_RSA_SCHANNEL_PROV_A}
  MS_DEF_RSA_SCHANNEL_PROV_W = WideString('Microsoft RSA SChannel Cryptographic Provider');
  {$EXTERNALSYM MS_DEF_RSA_SCHANNEL_PROV_W}

{$IFDEF UNICODE}
  MS_DEF_RSA_SCHANNEL_PROV = MS_DEF_RSA_SCHANNEL_PROV_W;
{$ELSE}
  MS_DEF_RSA_SCHANNEL_PROV = MS_DEF_RSA_SCHANNEL_PROV_A;
{$ENDIF}
  {$EXTERNALSYM MS_DEF_RSA_SCHANNEL_PROV}

  MS_ENHANCED_RSA_SCHANNEL_PROV_A = 'Microsoft Enhanced RSA SChannel Cryptographic Provider';
  {$EXTERNALSYM MS_ENHANCED_RSA_SCHANNEL_PROV_A}
  MS_ENHANCED_RSA_SCHANNEL_PROV_W = WideString('Microsoft Enhanced RSA SChannel Cryptographic Provider');
  {$EXTERNALSYM MS_ENHANCED_RSA_SCHANNEL_PROV_W}

{$IFDEF UNICODE}
  MS_ENHANCED_RSA_SCHANNEL_PROV = MS_ENHANCED_RSA_SCHANNEL_PROV_W;
{$ELSE}
  MS_ENHANCED_RSA_SCHANNEL_PROV = MS_ENHANCED_RSA_SCHANNEL_PROV_A;
{$ENDIF}
  {$EXTERNALSYM MS_ENHANCED_RSA_SCHANNEL_PROV}

  MS_DEF_DSS_PROV_A =  'Microsoft Base DSS Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_DSS_PROV_A}
  MS_DEF_DSS_PROV_W = WideString('Microsoft Base DSS Cryptographic Provider');
  {$EXTERNALSYM MS_DEF_DSS_PROV_W}

{$IFDEF UNICODE}
  MS_DEF_DSS_PROV = MS_DEF_DSS_PROV_W;
{$ELSE}
  MS_DEF_DSS_PROV = MS_DEF_DSS_PROV_A;
{$ENDIF}
  {$EXTERNALSYM MS_DEF_DSS_PROV}

  MS_DEF_DSS_DH_PROV_A = 'Microsoft Base DSS and Diffie-Hellman Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_DSS_DH_PROV_A}
  MS_DEF_DSS_DH_PROV_W = WideString('Microsoft Base DSS and Diffie-Hellman Cryptographic Provider');
  {$EXTERNALSYM MS_DEF_DSS_DH_PROV_W}

{$IFDEF UNICODE}
  MS_DEF_DSS_DH_PROV = MS_DEF_DSS_DH_PROV_W;
{$ELSE}
  MS_DEF_DSS_DH_PROV = MS_DEF_DSS_DH_PROV_A;
{$ENDIF}
  {$EXTERNALSYM MS_DEF_DSS_DH_PROV}

  ALG_CLASS_SIGNATURE = (1 shl 13);
  {$EXTERNALSYM ALG_CLASS_SIGNATURE}
  ALG_TYPE_RSA        = (2 shl 9);
  {$EXTERNALSYM ALG_TYPE_RSA}
  CALG_RSA_SIGN       = (ALG_CLASS_SIGNATURE or ALG_TYPE_RSA);
  {$EXTERNALSYM CALG_RSA_SIGN}

  CRYPT_NEWKEYSET      = $00000008;
  {$EXTERNALSYM CRYPT_NEWKEYSET}
  CRYPT_DELETEKEYSET   = $00000010;
  {$EXTERNALSYM CRYPT_DELETEKEYSET}
  CRYPT_MACHINE_KEYSET = $00000020;
  {$EXTERNALSYM CRYPT_MACHINE_KEYSET}
  CRYPT_SILENT         = $00000040;
  {$EXTERNALSYM CRYPT_SILENT}

  CRYPT_EXPORTABLE     = $00000001;
  {$EXTERNALSYM CRYPT_EXPORTABLE}

  SIMPLEBLOB        = $1;
  {$EXTERNALSYM SIMPLEBLOB}
  PUBLICKEYBLOB     = $6;
  {$EXTERNALSYM PUBLICKEYBLOB}
  PRIVATEKEYBLOB    = $7;
  {$EXTERNALSYM PRIVATEKEYBLOB}
  PLAINTEXTKEYBLOB  = $8;
  {$EXTERNALSYM PLAINTEXTKEYBLOB}
  AT_KEYEXCHANGE    = 1;
  {$EXTERNALSYM AT_KEYEXCHANGE}
  AT_SIGNATURE      = 2;
  {$EXTERNALSYM AT_SIGNATURE}

  PROV_RSA_FULL      = 1;
  {$EXTERNALSYM PROV_RSA_FULL}

  PP_ENUMALGS        = 1;
  {$EXTERNALSYM PP_ENUMALGS}
  PP_ENUMCONTAINERS  = 2;
  {$EXTERNALSYM PP_ENUMCONTAINERS}
  CRYPT_FIRST = 1;
  {$EXTERNALSYM CRYPT_FIRST}

  CERT_RDN_ANY_TYPE         = 0;
  {$EXTERNALSYM CERT_RDN_ANY_TYPE}
  CERT_RDN_ENCODED_BLOB     = 1;
  {$EXTERNALSYM CERT_RDN_ENCODED_BLOB}
  CERT_RDN_OCTET_STRING     = 2;
  {$EXTERNALSYM CERT_RDN_OCTET_STRING}
  CERT_RDN_NUMERIC_STRING   = 3;
  {$EXTERNALSYM CERT_RDN_NUMERIC_STRING}
  CERT_RDN_PRINTABLE_STRING = 4;
  {$EXTERNALSYM CERT_RDN_PRINTABLE_STRING}
  CERT_RDN_TELETEX_STRING   = 5;
  {$EXTERNALSYM CERT_RDN_TELETEX_STRING}
  CERT_RDN_T61_STRING       = 5;
  {$EXTERNALSYM CERT_RDN_T61_STRING}
  CERT_RDN_VIDEOTEX_STRING  = 6;
  {$EXTERNALSYM CERT_RDN_VIDEOTEX_STRING}
  CERT_RDN_IA5_STRING       = 7;
  {$EXTERNALSYM CERT_RDN_IA5_STRING}
  CERT_RDN_GRAPHIC_STRING   = 8;
  {$EXTERNALSYM CERT_RDN_GRAPHIC_STRING}
  CERT_RDN_VISIBLE_STRING   = 9;
  {$EXTERNALSYM CERT_RDN_VISIBLE_STRING}
  CERT_RDN_ISO646_STRING    = 9;
  {$EXTERNALSYM CERT_RDN_ISO646_STRING}
  CERT_RDN_GENERAL_STRING   = 10;
  {$EXTERNALSYM CERT_RDN_GENERAL_STRING}
  CERT_RDN_UNIVERSAL_STRING = 11;
  {$EXTERNALSYM CERT_RDN_UNIVERSAL_STRING}
  CERT_RDN_INT4_STRING      = 11;
  {$EXTERNALSYM CERT_RDN_INT4_STRING}
  CERT_RDN_BMP_STRING       = 12;
  {$EXTERNALSYM CERT_RDN_BMP_STRING}
  CERT_RDN_UNICODE_STRING   = 12;
  {$EXTERNALSYM CERT_RDN_UNICODE_STRING}
  CERT_RDN_UTF8_STRING      = 13;
  {$EXTERNALSYM CERT_RDN_UTF8_STRING}

// Certificate Information Flags
  CERT_INFO_VERSION_FLAG                 = 1;
  {$EXTERNALSYM CERT_INFO_VERSION_FLAG}
  CERT_INFO_SERIAL_NUMBER_FLAG           = 2;
  {$EXTERNALSYM CERT_INFO_SERIAL_NUMBER_FLAG}
  CERT_INFO_SIGNATURE_ALGORITHM_FLAG     = 3;
  {$EXTERNALSYM CERT_INFO_SIGNATURE_ALGORITHM_FLAG}
  CERT_INFO_ISSUER_FLAG                  = 4;
  {$EXTERNALSYM CERT_INFO_ISSUER_FLAG}
  CERT_INFO_NOT_BEFORE_FLAG              = 5;
  {$EXTERNALSYM CERT_INFO_NOT_BEFORE_FLAG}
  CERT_INFO_NOT_AFTER_FLAG               = 6;
  {$EXTERNALSYM CERT_INFO_NOT_AFTER_FLAG}
  CERT_INFO_SUBJECT_FLAG                 = 7;
  {$EXTERNALSYM CERT_INFO_SUBJECT_FLAG}
  CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG = 8;
  {$EXTERNALSYM CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG}
  CERT_INFO_ISSUER_UNIQUE_ID_FLAG        = 9;
  {$EXTERNALSYM CERT_INFO_ISSUER_UNIQUE_ID_FLAG}
  CERT_INFO_SUBJECT_UNIQUE_ID_FLAG       = 10;
  {$EXTERNALSYM CERT_INFO_SUBJECT_UNIQUE_ID_FLAG}
  CERT_INFO_EXTENSION_FLAG               = 11;
  {$EXTERNALSYM CERT_INFO_EXTENSION_FLAG}

// Certificate and Message encoding types
  CRYPT_ASN_ENCODING  = $00000001;
  {$EXTERNALSYM CRYPT_ASN_ENCODING}
  CRYPT_NDR_ENCODING  = $00000002;
  {$EXTERNALSYM CRYPT_NDR_ENCODING}
  X509_ASN_ENCODING   = $00000001;
  {$EXTERNALSYM X509_ASN_ENCODING}
  X509_NDR_ENCODING   = $00000002;
  {$EXTERNALSYM X509_NDR_ENCODING}
  PKCS_7_ASN_ENCODING = $00010000;
  {$EXTERNALSYM PKCS_7_ASN_ENCODING}
  PKCS_7_NDR_ENCODING = $00020000;
  {$EXTERNALSYM PKCS_7_NDR_ENCODING}

const
  CERT_KEY_PROV_HANDLE_PROP_ID        = 1;
  {$EXTERNALSYM CERT_KEY_PROV_HANDLE_PROP_ID}
  CERT_KEY_PROV_INFO_PROP_ID          = 2;
  {$EXTERNALSYM CERT_KEY_PROV_INFO_PROP_ID}
  CERT_SHA1_HASH_PROP_ID              = 3;
  {$EXTERNALSYM CERT_SHA1_HASH_PROP_ID}
  CERT_MD5_HASH_PROP_ID               = 4;
  {$EXTERNALSYM CERT_MD5_HASH_PROP_ID}
  CERT_HASH_PROP_ID                   = CERT_SHA1_HASH_PROP_ID;
  {$EXTERNALSYM CERT_HASH_PROP_ID}
  CERT_FRIENDLY_NAME_PROP_ID          = 11;
  {$EXTERNALSYM CERT_FRIENDLY_NAME_PROP_ID}

  CRYPT_ACQUIRE_COMPARE_KEY_FLAG = $00000004;
  {$EXTERNALSYM CRYPT_ACQUIRE_COMPARE_KEY_FLAG}
  CRYPT_ACQUIRE_SILENT_FLAG = $00000040;
  {$EXTERNALSYM CRYPT_ACQUIRE_SILENT_FLAG}

  CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
  {$EXTERNALSYM CERT_NAME_SIMPLE_DISPLAY_TYPE}
  CERT_NAME_DISABLE_IE4_UTF8_FLAG = $00010000;
  {$EXTERNALSYM CERT_NAME_DISABLE_IE4_UTF8_FLAG}

  CERT_SET_KEY_PROV_HANDLE_PROP_ID = $00000001;
  {$EXTERNALSYM CERT_SET_KEY_PROV_HANDLE_PROP_ID}
  CERT_SET_KEY_CONTEXT_PROP_ID     = $00000001;
  {$EXTERNALSYM CERT_SET_KEY_CONTEXT_PROP_ID}

  CERT_STORE_PROV_MSG    = (ILPCSTR(1));
  {$EXTERNALSYM CERT_STORE_PROV_MSG}
  CERT_STORE_PROV_MEMORY = (ILPCSTR(2));
  {$EXTERNALSYM CERT_STORE_PROV_MEMORY}
  CERT_STORE_PROV_FILE   = (ILPCSTR(3));
  {$EXTERNALSYM CERT_STORE_PROV_FILE}
  CERT_STORE_PROV_REG    = (ILPCSTR(4));
  {$EXTERNALSYM CERT_STORE_PROV_REG}
  CERT_STORE_PROV_FILENAME_A = (ILPCSTR(7));
  {$EXTERNALSYM CERT_STORE_PROV_FILENAME_A}
  CERT_STORE_PROV_FILENAME_W = (ILPCSTR(8));
  {$EXTERNALSYM CERT_STORE_PROV_FILENAME_W}
  CERT_STORE_PROV_FILENAME   =  CERT_STORE_PROV_FILENAME_W;
  {$EXTERNALSYM CERT_STORE_PROV_FILENAME}
  CERT_STORE_PROV_SYSTEM_A   = (ILPCSTR(9));
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_A}
  CERT_STORE_PROV_SYSTEM_W   =  (ILPCSTR(10));
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_W}
  CERT_STORE_PROV_SYSTEM     = CERT_STORE_PROV_SYSTEM_W;
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM}
  CERT_STORE_PROV_SYSTEM_REGISTRY_A = (ILPCSTR(12));
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_REGISTRY_A}
  CERT_STORE_PROV_SYSTEM_REGISTRY_W = (ILPCSTR(13));
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_REGISTRY_W}
  CERT_STORE_PROV_SYSTEM_REGISTRY   = CERT_STORE_PROV_SYSTEM_REGISTRY_W;
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_REGISTRY}
  CERT_STORE_PROV_PHYSICAL_W        = (ILPCSTR(14));
  {$EXTERNALSYM CERT_STORE_PROV_PHYSICAL_W}
  CERT_STORE_PROV_PHYSICAL          = CERT_STORE_PROV_PHYSICAL_W;
  {$EXTERNALSYM CERT_STORE_PROV_PHYSICAL}

  CERT_STORE_SIGNATURE_FLAG     = $00000001;
  {$EXTERNALSYM CERT_STORE_SIGNATURE_FLAG}
  CERT_STORE_TIME_VALIDITY_FLAG = $00000002;
  {$EXTERNALSYM CERT_STORE_TIME_VALIDITY_FLAG}
  CERT_STORE_REVOCATION_FLAG    = $00000004;
  {$EXTERNALSYM CERT_STORE_REVOCATION_FLAG}
  CERT_STORE_NO_CRL_FLAG        = $00010000;
  {$EXTERNALSYM CERT_STORE_NO_CRL_FLAG}
  CERT_STORE_NO_ISSUER_FLAG     = $00020000;
  {$EXTERNALSYM CERT_STORE_NO_ISSUER_FLAG}
  CERT_STORE_DELETE_FLAG        = $00000010;
  {$EXTERNALSYM CERT_STORE_DELETE_FLAG}
  CERT_STORE_READONLY_FLAG      = $00008000;
  {$EXTERNALSYM CERT_STORE_READONLY_FLAG}

  CERT_STORE_SAVE_AS_STORE = 1;
  {$EXTERNALSYM CERT_STORE_SAVE_AS_STORE}
  CERT_STORE_SAVE_TO_FILE  = 1;
  {$EXTERNALSYM CERT_STORE_SAVE_TO_FILE}

  CERT_STORE_ADD_NEW = 1;
  {$EXTERNALSYM CERT_STORE_ADD_NEW}

  CERT_SYSTEM_STORE_LOCATION_SHIFT = 16;
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCATION_SHIFT}
  CERT_SYSTEM_STORE_CURRENT_USER_ID = 1;
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_USER_ID}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ID = 2;
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_ID}
  CERT_SYSTEM_STORE_CURRENT_SERVICE_ID = 4;
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_SERVICE_ID}
  CERT_SYSTEM_STORE_SERVICES_ID = 5;
  {$EXTERNALSYM CERT_SYSTEM_STORE_SERVICES_ID}
  CERT_SYSTEM_STORE_USERS_ID = 6;
  {$EXTERNALSYM CERT_SYSTEM_STORE_USERS_ID}
  CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY_ID = 7;
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY_ID}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY_ID = 8;
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY_ID}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE_ID = 9;
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE_ID}
  CERT_SYSTEM_STORE_CURRENT_USER = (CERT_SYSTEM_STORE_CURRENT_USER_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_USER}
  CERT_SYSTEM_STORE_LOCAL_MACHINE = (CERT_SYSTEM_STORE_LOCAL_MACHINE_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE}
  CERT_SYSTEM_STORE_CURRENT_SERVICE = (CERT_SYSTEM_STORE_CURRENT_SERVICE_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_SERVICE}
  CERT_SYSTEM_STORE_SERVICES = (CERT_SYSTEM_STORE_SERVICES_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  {$EXTERNALSYM CERT_SYSTEM_STORE_SERVICES}
  CERT_SYSTEM_STORE_USERS = (CERT_SYSTEM_STORE_USERS_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  {$EXTERNALSYM CERT_SYSTEM_STORE_USERS}
  CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY = (CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY = (CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE = (CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT);
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE}

  CERT_FILE_STORE_COMMIT_ENABLE_FLAG = $10000;
  {$EXTERNALSYM CERT_FILE_STORE_COMMIT_ENABLE_FLAG}

  CERT_COMPARE_SHIFT = 16;
  {$EXTERNALSYM CERT_COMPARE_SHIFT}
  CERT_COMPARE_SHA1_HASH = 1;
  {$EXTERNALSYM CERT_COMPARE_SHA1_HASH}
  CERT_COMPARE_ATTR = 3;
  {$EXTERNALSYM CERT_COMPARE_ATTR}
  CERT_COMPARE_MD5_HASH  = 4;
  {$EXTERNALSYM CERT_COMPARE_MD5_HASH}
  CERT_COMPARE_NAME_STR_A = 7;
  {$EXTERNALSYM CERT_COMPARE_NAME_STR_A}
  CERT_COMPARE_NAME_STR_W = 8;
  {$EXTERNALSYM CERT_COMPARE_NAME_STR_W}
  CERT_COMPARE_EXISTING = 13;
  {$EXTERNALSYM CERT_COMPARE_EXISTING}

  CERT_FIND_ANY = 0;
  {$EXTERNALSYM CERT_FIND_ANY}
  CERT_FIND_SHA1_HASH = (CERT_COMPARE_SHA1_HASH shl CERT_COMPARE_SHIFT);
  {$EXTERNALSYM CERT_FIND_SHA1_HASH}
  CERT_FIND_MD5_HASH = (CERT_COMPARE_MD5_HASH shl CERT_COMPARE_SHIFT);
  {$EXTERNALSYM CERT_FIND_MD5_HASH}
  CERT_FIND_HASH = (CERT_FIND_SHA1_HASH);
  {$EXTERNALSYM CERT_FIND_HASH}
  CERT_FIND_SUBJECT_STR_A = (CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG);
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR_A}
  CERT_FIND_SUBJECT_STR_W = (CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG);
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR_W}
  CERT_FIND_SUBJECT_STR = CERT_FIND_SUBJECT_STR_W;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR}
  CERT_FIND_SUBJECT_ATTR = (CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG);
  {$EXTERNALSYM CERT_FIND_SUBJECT_ATTR}
  CERT_FIND_EXISTING = (CERT_COMPARE_EXISTING shl CERT_COMPARE_SHIFT);
  {$EXTERNALSYM CERT_FIND_EXISTING}

type
// CRYPTOAPI BLOB definitions
  PCRYPTOAPI_BLOB = ^CRYPTOAPI_BLOB;
  CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
  end;

  PCRYPT_ALGORITHM_IDENTIFIER = ^CRYPT_ALGORITHM_IDENTIFIER;
  CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId: LPSTR;
    Parameters: CRYPTOAPI_BLOB;
  end;

// Attributes making up a Relative Distinguished Name (CERT_RDN)
  PCERT_RDN_ATTR = ^CERT_RDN_ATTR;
  CERT_RDN_ATTR = record
    pszObjId: LPSTR;
    dwValueType: DWORD;
    Value: CRYPTOAPI_BLOB;
  end;

// A CERT_RDN consists of an array of the above attributes
  PCERT_RDN = ^CERT_RDN;
  CERT_RDN = record
    cRDNAttr: DWORD;
    rgRDNAttr: PCERT_RDN_ATTR;
  end;

// In a CRYPT_BIT_BLOB the last byte may contain 0-7 unused bits. Therefore, the
// overall bit length is cbData * 8 - cUnusedBits.
  PCRYPT_BIT_BLOB = ^CRYPT_BIT_BLOB;
  CRYPT_BIT_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
    cUnusedBits: DWORD;
  end;

// Public Key Info
  PCERT_PUBLIC_KEY_INFO = ^CERT_PUBLIC_KEY_INFO;
  CERT_PUBLIC_KEY_INFO = record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey: CRYPT_BIT_BLOB;
  end;

// Information stored in a certificate
  PCERT_INFO = ^CERT_INFO;
  CERT_INFO = record
    dwVersion: DWORD;
    SerialNumber: CRYPTOAPI_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CRYPTOAPI_BLOB;
    NotBefore: TFILETIME;
    NotAfter: TFILETIME;
    Subject: CRYPTOAPI_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId: CRYPT_BIT_BLOB;
    SubjectUniqueId: CRYPT_BIT_BLOB;
    cExtension: DWORD;
    rgExtension: IntPtr;
  end;

// Handle returned by the store provider when opened.
  HCERTSTORE = PVOID;
  PHCERTSTORE = ^HCERTSTORE;

  PCERT_CONTEXT = ^CERT_CONTEXT;
  CERT_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PBYTE;
    cbCertEncoded: DWORD;
    pCertInfo: PCERT_INFO;
    hCertStore: HCERTSTORE;
  end;
  PCCERT_CONTEXT = ^CERT_CONTEXT;

type
  HCRYPTPROV  = NativeUint;
  PHCRYPTPROV = ^HCRYPTPROV;
  HCRYPTKEY   = NativeUint;
  PHCRYPTKEY  = ^HCRYPTKEY;
  HCRYPTHASH  = NativeUint;
  PHCRYPTHASH = ^HCRYPTHASH;

  TCryptAcquireContext = function (var phProv: HCRYPTPROV;
                                   pszContainer: LPAWSTR;
                                   pszProvider: LPAWSTR;
                                   dwProvType: DWORD;
                                   dwFlags: DWORD): BOOL; stdcall;

  TCryptReleaseContext = function (hProv: HCRYPTPROV;
                                   dwFlags: DWORD): BOOL; stdcall;

  TCryptGetProvParam = function (hProv: HCRYPTPROV;
                                 dwParam: DWORD;
                                 pbData: PBYTE;
                             var pdwDataLen: DWORD;
                                 dwFlags: DWORD): BOOL; stdcall;

  TCryptGetUserKey = function (hProv: HCRYPTPROV;
                               dwKeySpec: DWORD;
                           var phUserKey: HCRYPTKEY): BOOL; stdcall;

  TCryptExportKey = function (hKey: HCRYPTKEY;
                              hExpKey: HCRYPTKEY;
                              dwBlobType: DWORD;
                              dwFlags: DWORD;
                              pbData: PBYTE;
                          var dwDataLen: DWORD): BOOL; stdcall;

  TCryptImportKey = function (hProv: HCRYPTPROV;
                        const pbData: TBytes;
                              dwDataLen: DWORD;
                              hPubKey: HCRYPTKEY;
                              dwFlags: DWORD;
                          var hKey: HCRYPTKEY): BOOL; stdcall;

  TCryptDestroyKey = function (hKey: HCRYPTKEY): BOOL; stdcall;

  TCertOpenStore = function (lpszStoreProvider: LPCSTR;
                             dwEncodingType: DWORD;
                             hCryptProv: HCRYPTPROV;
                             dwFlags: DWORD;
                       const pvPara: PVOID): HCERTSTORE; stdcall;

  TCertSaveStore = function (hCertStore: HCERTSTORE;
                             dwEncodingType: DWORD;
                             dwSaveAs: DWORD;
                             dwSaveTo: DWORD;
                             pvSaveToPara: PVOID;
                             dwFlags: DWORD): BOOL; stdcall;

  TCertCloseStore = function (hCertStore: HCERTSTORE; dwFlags: DWORD): BOOL; stdcall;

  TCertFindCertificateInStore = function (hCertStore: HCERTSTORE;
                                          dwCertEncodingType: DWORD;
                                          dwFindFlags: DWORD;
                                          dwFindType: DWORD;
                                    const pvFindPara: PVOID;
                                          pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall;

  TCertDuplicateCertificateContext = function (pCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall;

  TCertCreateCertificateContext = function (dwCertEncodingType: DWORD;
                                            pbCertEncoded: PBYTE;
                                            cbCertEncoded: DWORD): PCCERT_CONTEXT; stdcall;

  TCertFreeCertificateContext = function (pCertContext: PCCERT_CONTEXT): BOOL; stdcall;

  TCertGetCertificateContextProperty = function (pCertContext: PCCERT_CONTEXT;
                                                 dwPropId: DWORD;
                                                 pvData: PVOID;
                                             var pcbData: DWORD): BOOL; stdcall;

  TCertSetCertificateContextProperty = function (pCertContext: PCCERT_CONTEXT;
                                                 dwPropId: DWORD;
                                                 dwFlags: DWORD;
                                                 pvData: PVOID): BOOL; stdcall;

  TCertAddCertificateContextToStore = function (hCertStore: HCERTSTORE;
                                                pCertContext: PCCERT_CONTEXT;
                                                dwAddDisposition: DWORD;
                                            var ppStoreContext: PCCERT_CONTEXT): BOOL; stdcall;

  TCertDeleteCertificateFromStore = function (pCertContext: PCCERT_CONTEXT): BOOL; stdcall;

  TCryptAcquireCertificatePrivateKey = function (pCert: PCCERT_CONTEXT;
                                                 dwFlags: DWORD;
                                                 pvReserved: PVOID;
                                             var phCryptProv: HCRYPTPROV;
                                             var pdwKeySpec: DWORD;
                                             var pfCallerFreeProv: BOOL): BOOL; stdcall;

  TCryptImportPublicKeyInfoEx = function (hCryptProv: HCRYPTPROV;
                                          dwCertEncodingType: DWORD;
                                          pInfo: PCERT_PUBLIC_KEY_INFO;
                                          aiKeyAlg: Cardinal;
                                          dwFlags: DWORD;
                                          pvAuxInfo: PVOID;
                                      var phKey: HCRYPTKEY): BOOL; stdcall;

  TCertGetNameString = function (pCertContext: PCCERT_CONTEXT;
                                 dwType: DWORD;
                                 dwFlags: DWORD;
                                 pvTypePara: PVOID;
                                 pszNameString: LPTSTR;
                                 cchNameString: DWORD): DWORD; stdcall;

  TCertGetIssuerCertificateFromStore = function (hCertStore: HCERTSTORE;
                                                 pSubjectContext: PCCERT_CONTEXT;
                                                 pPrevIssuerContext: PCCERT_CONTEXT;
                                             var pdwFlags: DWORD): PCCERT_CONTEXT; stdcall;

  TCryptEnumProviders = function (dwIndex: DWORD;
                                  pdwReserved: PDWORD;
                                  dwFlags: DWORD;
                              var pdwProvType: DWORD;
                                  pszProvName: LPAWSTR;
                              var pcbProvName: DWORD): BOOL; stdcall;

{$IFNDEF BCB}
  TPFXImportCertStore = function (pPFX: IntPtr;
                                  szPassword: LPCWSTR;
                                  dwFlags: DWORD): HCERTSTORE; stdcall;
{$ENDIF}

var
  CryptAcquireContext: TCryptAcquireContext;
  CryptReleaseContext: TCryptReleaseContext;
  CryptGetProvParam: TCryptGetProvParam;
  CryptGetUserKey: TCryptGetUserKey;
  CryptExportKey: TCryptExportKey;
  CryptImportKey: TCryptImportKey;
  CryptDestroyKey: TCryptDestroyKey;
  CertOpenStore: TCertOpenStore;
  CertSaveStore: TCertSaveStore;
  CertCloseStore: TCertCloseStore;
  CertFindCertificateInStore: TCertFindCertificateInStore;
  CertDuplicateCertificateContext: TCertDuplicateCertificateContext;
  CertCreateCertificateContext: TCertCreateCertificateContext;
  CertFreeCertificateContext: TCertFreeCertificateContext;
  CertGetCertificateContextProperty: TCertGetCertificateContextProperty;
  CertSetCertificateContextProperty: TCertSetCertificateContextProperty;
  CertAddCertificateContextToStore: TCertAddCertificateContextToStore;
  CertDeleteCertificateFromStore: TCertDeleteCertificateFromStore;
  CryptAcquireCertificatePrivateKey: TCryptAcquireCertificatePrivateKey;
  CryptImportPublicKeyInfoEx: TCryptImportPublicKeyInfoEx;
  CertGetNameString: TCertGetNameString;
  CertGetIssuerCertificateFromStore: TCertGetIssuerCertificateFromStore;
  CryptEnumProviders: TCryptEnumProviders;
{$IFNDEF BCB}
  PFXImportCertStore: TPFXImportCertStore;
{$ENDIF}

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
type
  PUInt32 = ^UInt32;
{$ENDIF}

{$IFDEF MACOS}
type
  SecCertificateRef = Pointer;
  SecKeyRef = Pointer;
  SecIdentityRef = Pointer;
  SecKeychainRef = Pointer;
  SecTransformRef = Pointer;
  SecAccessRef = Pointer;
  SecKeychainSearchRef = Pointer;
  SecKeychainItemRef = Pointer;
  SecPolicyRef = Pointer;
  SecTrustRef = Pointer;

  SecTrustResultType = UInt32;
  SecCredentialType = UInt32;
  SecExternalFormat = UInt32;
  PSecExternalFormat = ^SecExternalFormat;
  SecExternalItemType = UInt32;
  PSecExternalItemType = ^SecExternalItemType;
  SecItemImportExportFlags = UInt32;
  SecKeyImportExportFlags = UInt32;
  SecItemClass = FourCharCode;
  PSecItemClass = ^SecItemClass;
  SecKeychainAttrType = OSType;

  CSSM_HEADERVERSION = UInt32;
  CSSM_KEYBLOB_TYPE = UInt32;
  CSSM_KEYBLOB_FORMAT = UInt32;
  CSSM_ALGORITHMS = UInt32;
  CSSM_KEYCLASS = UInt32;
  CSSM_KEYATTR_FLAGS = UInt32;
  CSSM_KEYUSE = UInt32;
  CSSM_ENCRYPT_MODE = UInt32;

const
  CSSM_ALGID_RSA = CSSM_ALGORITHMS(42);
  {$EXTERNALSYM CSSM_ALGID_RSA}
  CSSM_ALGID_DSA = CSSM_ALGORITHMS(43);
  {$EXTERNALSYM CSSM_ALGID_DSA}

  CSSM_KEYBLOB_RAW_FORMAT_NONE    = CSSM_KEYBLOB_FORMAT(0);
  {$EXTERNALSYM CSSM_KEYBLOB_RAW_FORMAT_NONE}
  CSSM_KEYBLOB_RAW_FORMAT_PKCS1   = CSSM_KEYBLOB_FORMAT(1);
  {$EXTERNALSYM CSSM_KEYBLOB_RAW_FORMAT_PKCS1}
  CSSM_KEYBLOB_RAW_FORMAT_PKCS8   = CSSM_KEYBLOB_FORMAT(10);
  {$EXTERNALSYM CSSM_KEYBLOB_RAW_FORMAT_PKCS8}

  CSSM_KEYCLASS_PUBLIC_KEY  = CSSM_KEYCLASS(0);
  {$EXTERNALSYM CSSM_KEYCLASS_PUBLIC_KEY}
  CSSM_KEYCLASS_PRIVATE_KEY = CSSM_KEYCLASS(1);
  {$EXTERNALSYM CSSM_KEYCLASS_PRIVATE_KEY}
  CSSM_KEYCLASS_SESSION_KEY = CSSM_KEYCLASS(2);
  {$EXTERNALSYM CSSM_KEYCLASS_SESSION_KEY}
  CSSM_KEYCLASS_SECRET_PART = CSSM_KEYCLASS(3);
  {$EXTERNALSYM CSSM_KEYCLASS_SECRET_PART}
  CSSM_KEYCLASS_OTHER       = CSSM_KEYCLASS($FFFFFFFF);
  {$EXTERNALSYM CSSM_KEYCLASS_OTHER}

  kSecFormatUnknown        = SecExternalFormat(0);
  {$EXTERNALSYM kSecFormatUnknown}
  kSecFormatOpenSSL        = SecExternalFormat(1);
  {$EXTERNALSYM kSecFormatOpenSSL}
  kSecFormatSSH            = SecExternalFormat(2);
  {$EXTERNALSYM kSecFormatSSH}
  kSecFormatBSAFE          = SecExternalFormat(3);
  {$EXTERNALSYM kSecFormatBSAFE}
  kSecFormatRawKey         = SecExternalFormat(4);
  {$EXTERNALSYM kSecFormatRawKey}
  kSecFormatWrappedPKCS8   = SecExternalFormat(5);
  {$EXTERNALSYM kSecFormatWrappedPKCS8}
  kSecFormatWrappedOpenSSL = SecExternalFormat(6);
  {$EXTERNALSYM kSecFormatWrappedOpenSSL}
  kSecFormatWrappedSSH     = SecExternalFormat(7);
  {$EXTERNALSYM kSecFormatWrappedSSH}
  kSecFormatWrappedLSH     = SecExternalFormat(8);
  {$EXTERNALSYM kSecFormatWrappedLSH}
  kSecFormatX509Cert       = SecExternalFormat(9);
  {$EXTERNALSYM kSecFormatX509Cert}
  kSecFormatPEMSequence    = SecExternalFormat(10);
  {$EXTERNALSYM kSecFormatPEMSequence}
  kSecFormatPKCS7          = SecExternalFormat(11);
  {$EXTERNALSYM kSecFormatPKCS7}
  kSecFormatPKCS12         = SecExternalFormat(12);
  {$EXTERNALSYM kSecFormatPKCS12}
  kSecFormatNetscapeCertSequence = SecExternalFormat(13);
  {$EXTERNALSYM kSecFormatNetscapeCertSequence}
  kSecFormatSSHv2          = SecExternalFormat(14);
  {$EXTERNALSYM kSecFormatSSHv2}

  kSecItemTypeUnknown     = SecExternalItemType(0);
  {$EXTERNALSYM kSecItemTypeUnknown}
  kSecItemTypePrivateKey  = SecExternalItemType(1);
  {$EXTERNALSYM kSecItemTypePrivateKey}
  kSecItemTypePublicKey   = SecExternalItemType(2);
  {$EXTERNALSYM kSecItemTypePublicKey}
  kSecItemTypeSessionKey  = SecExternalItemType(3);
  {$EXTERNALSYM kSecItemTypeSessionKey}
  kSecItemTypeCertificate = SecExternalItemType(4);
  {$EXTERNALSYM kSecItemTypeCertificate}
  kSecItemTypeAggregate   = SecExternalItemType(5);
  {$EXTERNALSYM kSecItemTypeAggregate}

  CSSM_DB_RECORDTYPE_APP_DEFINED_START = $80000000;
  {$EXTERNALSYM CSSM_DB_RECORDTYPE_APP_DEFINED_START}
  CSSM_DB_RECORDTYPE_OPEN_GROUP_START  = $0000000A;
  {$EXTERNALSYM CSSM_DB_RECORDTYPE_OPEN_GROUP_START}
  CSSM_DL_DB_RECORD_X509_CERTIFICATE   = CSSM_DB_RECORDTYPE_APP_DEFINED_START + $1000;
  {$EXTERNALSYM CSSM_DL_DB_RECORD_X509_CERTIFICATE}
  CSSM_DL_DB_RECORD_PUBLIC_KEY         = CSSM_DB_RECORDTYPE_OPEN_GROUP_START + 5;
  {$EXTERNALSYM CSSM_DL_DB_RECORD_PUBLIC_KEY}
  CSSM_DL_DB_RECORD_PRIVATE_KEY        = CSSM_DB_RECORDTYPE_OPEN_GROUP_START + 6;
  {$EXTERNALSYM CSSM_DL_DB_RECORD_PRIVATE_KEY}

  CSSM_DB_ATTRIBUTE_FORMAT_BLOB = 6;
  {$EXTERNALSYM CSSM_DB_ATTRIBUTE_FORMAT_BLOB}

  kSecCertificateItemClass = SecItemClass(CSSM_DL_DB_RECORD_X509_CERTIFICATE);
  {$EXTERNALSYM kSecCertificateItemClass}
  kSecPublicKeyItemClass   = SecItemClass(CSSM_DL_DB_RECORD_PUBLIC_KEY);
  {$EXTERNALSYM kSecPublicKeyItemClass}
  kSecPrivateKeyItemClass  = SecItemClass(CSSM_DL_DB_RECORD_PRIVATE_KEY);
  {$EXTERNALSYM kSecPrivateKeyItemClass}

  kSecKeyKeyClass = 0;
  {$EXTERNALSYM kSecKeyKeyClass}
  kSecKeyPrintName = 1;
  {$EXTERNALSYM kSecKeyPrintName}

  errSecSuccess = OSStatus(0);
  {$EXTERNALSYM errSecItemNotFound}
  errSecItemNotFound = OSStatus(-25300);
  {$EXTERNALSYM errSecItemNotFound}
  errSecDuplicateItem = OSStatus(-25299);
  {$EXTERNALSYM errSecDuplicateItem}
  errSecBufferTooSmall = OSStatus(-25301);
  {$EXTERNALSYM errSecBufferTooSmall}

  kSecTrustResultInvalid = 0;
  {$EXTERNALSYM kSecTrustResultInvalid}
  kSecTrustResultProceed = 1;
  {$EXTERNALSYM kSecTrustResultProceed}
  kSecTrustResultConfirm = 2;
  {$EXTERNALSYM kSecTrustResultConfirm}
  kSecTrustResultDeny = 3;
  {$EXTERNALSYM kSecTrustResultDeny}
  kSecTrustResultUnspecified = 4;
  {$EXTERNALSYM kSecTrustResultUnspecified}
  kSecTrustResultRecoverableTrustFailure = 5;
  {$EXTERNALSYM kSecTrustResultRecoverableTrustFailure}
  kSecTrustResultFatalTrustFailure = 6;
  {$EXTERNALSYM kSecTrustResultFatalTrustFailure}
  kSecTrustResultOtherError = 7;
  {$EXTERNALSYM kSecTrustResultOtherError}

type
  INT_PTR = Pointer;
  {$EXTERNALSYM INT_PTR}
  UINT_PTR = Pointer;
  {$EXTERNALSYM UINT_PTR}
  LONG_PTR = NativeInt;
  {$EXTERNALSYM LONG_PTR}
  ULONG_PTR = NativeUInt;
  {$EXTERNALSYM ULONG_PTR}
  SIZE_T = ULONG_PTR;
  {$EXTERNALSYM SIZE_T}
  INTPTR_T = INT_PTR;
  {$EXTERNALSYM INTPTR_T}
  PVOID = Pointer;
  {$EXTERNALSYM PVOID}

  CSSM_SIZE = SIZE_T;

  CSSM_DATA = record
    Length: CSSM_SIZE; // in bytes
    Data: PByte;
  end;
  CSSM_DATA_PTR = ^CSSM_DATA;

  CSSM_INTPTR = INTPTR_T;
  CSSM_HANDLE = CSSM_INTPTR;
  CSSM_HANDLE_PTR = ^CSSM_HANDLE;
  CSSM_LONG_HANDLE = UInt64;
  CSSM_LONG_HANDLE_PTR = ^CSSM_LONG_HANDLE;
  CSSM_MODULE_HANDLE = CSSM_HANDLE;
  CSSM_MODULE_HANDLE_PTR = ^CSSM_HANDLE;
  CSSM_CC_HANDLE = CSSM_LONG_HANDLE;
  CSSM_CSP_HANDLE = CSSM_MODULE_HANDLE;

  CSSM_RETURN = SInt32;
  CSSM_BOOL = LongBool;

  CSSM_GUID = record
    D1: UInt32;
    D2: UInt16;
    D3: UInt16;
    D4: array[0..7] of UInt8;
  end;
  CSSM_GUID_PTR = ^CSSM_GUID;

  CSSM_DATE = record
    Year: array[1..4] of UInt8;
    Month: array[1..2] of UInt8;
    Day: array[1..2] of UInt8;
  end;
  CSSM_DATE_PTR = ^CSSM_DATE;

  CSSM_KEYHEADER = record
    HeaderVersion: CSSM_HEADERVERSION; // Key header version
    CspId: CSSM_GUID; // GUID of CSP generating the key
    BlobType: CSSM_KEYBLOB_TYPE;
    Format: CSSM_KEYBLOB_FORMAT; // Raw or Reference format
    AlgorithmId: CSSM_ALGORITHMS; // Algorithm ID of key
    KeyClass: CSSM_KEYCLASS; // Public/Private/Secret, etc.
    LogicalKeySizeInBits: UInt32;
    KeyAttr: CSSM_KEYATTR_FLAGS;
    KeyUsage: CSSM_KEYUSE;
    StartDate: CSSM_DATE;
    EndDate: CSSM_DATE;
    WrapAlgorithmId: CSSM_ALGORITHMS;
    WrapMode: CSSM_ENCRYPT_MODE;
    Reserved: UInt32;
  end;

  CSSM_KEY = record
    KeyHeader: CSSM_KEYHEADER;
    KeyData: CSSM_DATA;
  end;
  CSSM_KEY_PTR = ^CSSM_KEY;

  SecKeyImportExportParameters = record
    Version: UInt32;
    Flags: SecKeyImportExportFlags;
    Passphrase: CFTypeRef;
    AlertTitle: CFStringRef;
    AlertPrompt: CFStringRef;
    AccessRef: SecAccessRef;
    KeyUsage: CSSM_KEYUSE;
    KeyAttributes: CSSM_KEYATTR_FLAGS;
  end;
  PSecKeyImportExportParameters = ^SecKeyImportExportParameters;

  SecKeychainAttribute = record
    Tag: SecKeychainAttrType;
    Length: UInt32;
    Data: PVOID;
  end;
  PSecKeychainAttribute = ^SecKeychainAttribute;

  SecKeychainAttributeList = record
    Count: UInt32;
    Attr: PSecKeychainAttribute;
  end;
  PSecKeychainAttributeList = ^SecKeychainAttributeList;
  PPSecKeychainAttributeList = Pointer;

  SecKeychainAttributeInfo = record
    Count: UInt32;
    Tag: PUInt32;
    Format: PUInt32;
  end;
  PSecKeychainAttributeInfo = ^SecKeychainAttributeInfo;

{$IFDEF FPC}
  PCFTypeRef = ^CFTypeRef;
{$ENDIF FPC}
  TCSSM_CSP_CreateDigestContext = function (
    CSPHandle: CSSM_CSP_HANDLE;
    AlgorithmID: CSSM_ALGORITHMS;
    var NewContextHandle: CSSM_CC_HANDLE): CSSM_RETURN; cdecl;

  TCSSM_DeleteContext = function (
    CCHandle: CSSM_CC_HANDLE): CSSM_RETURN; cdecl;

  TSecCopyErrorMessageString = function (
    Status: OSStatus;
    Reserved: Pointer): CFStringRef; cdecl;

  TSecCertificateCopyData = function (
    Certificate: SecCertificateRef): CFDataRef; cdecl;

  TSecCertificateCreateWithData = function (
    Allocator: CFAllocatorRef;
    Data: CFDataRef): SecCertificateRef; cdecl;

  TSecCertificateCopyCommonName = function (
    Certificate: SecCertificateRef;
    var CommonName: CFStringRef): OSStatus; cdecl;

  TSecCertificateCopySubjectSummary = function (
    Certificate: SecCertificateRef): CFStringRef; cdecl;

  TSecCertificateCopyPublicKey = function (
    Certificate: SecCertificateRef;
    var Key: SecKeyRef): OSStatus; cdecl;

  TSecKeyGetCSSMKey = function (
    Key: SecKeyRef;
    var CssmKey: CSSM_KEY_PTR): OSStatus; cdecl;

  TSecIdentityCopyPrivateKey = function (
    IdentityRef: SecIdentityRef;
    var PrivateKeyRef: SecKeyRef): OSStatus; cdecl;

  TSecIdentityCreateWithCertificate = function (
    KeychainOrArray: CFTypeRef;
    CertificateRef: SecCertificateRef;
    var IdentityRef: SecIdentityRef): OSStatus; cdecl;

  TSecKeychainItemImport = function (
    ImportedData: CFDataRef;
    FileNameOrExtension: CFStringRef;
    InputFormat: PSecExternalFormat;
    ItemType: PSecExternalItemType;
    Flags: SecItemImportExportFlags;
    const KeyParams: PSecKeyImportExportParameters;
    ImportKeychain: SecKeychainRef;
    var OutItems: CFArrayRef): OSStatus; cdecl;

  TSecKeychainItemExport = function (
    KeychainItemOrArray: CFTypeRef;
    OutputFormat: SecExternalFormat;
    Flags: SecItemImportExportFlags;
    const KeyParams: PSecKeyImportExportParameters;
    var ExportedData: CFDataRef): OSStatus; cdecl;

  TSecKeychainCopyDefault = function (
    var Keychain: SecKeychainRef): OSStatus; cdecl;

  TSecKeychainOpen = function (
    const PathName: PAnsiChar;
    var Keychain: SecKeychainRef): OSStatus; cdecl;

  TSecKeychainGetPath = function (
    Keychain: SecKeychainRef;
    PathLength: PUInt32;
    PathName: PAnsiChar): OSStatus; cdecl;

  TSecItemCopyMatching = function (
    Query: CFDictionaryRef;
    var Result: CFTypeRef): OSStatus; cdecl;

  TSecItemAdd = function (
    Attributes: CFDictionaryRef;
    Result: PCFTypeRef): OSStatus; cdecl;

  TSecItemUpdate = function (
    Query: CFDictionaryRef;
    AttributesToUpdate: CFDictionaryRef): OSStatus; cdecl;

  TSecKeychainSearchCreateFromAttributes = function (
    KeychainOrArray: CFTypeRef;
    ItemClass: SecItemClass;
    const AttrList: PSecKeychainAttributeList;
    var SearchRef: SecKeychainSearchRef): OSStatus; cdecl;

  TSecKeychainItemCopyAttributesAndData = function (
    ItemRef: SecKeychainItemRef;
    Info: PSecKeychainAttributeInfo;
    ItemClass: PSecItemClass;
    AttrList: PPSecKeychainAttributeList;
    Length: PUInt32;
    OutData: PVOID): OSStatus; cdecl;

  TSecKeychainSearchCopyNext = function (
    SearchRef: SecKeychainSearchRef;
    var ItemRef: SecKeychainItemRef): OSStatus; cdecl;

  TSecCertificateAddToKeychain = function (
    Certificate: SecCertificateRef;
    Keychain: SecKeychainRef): OSStatus; cdecl;

  TSecCertificateGetTypeID = function : CFTypeID; cdecl;
  TSecKeyGetTypeID = function : CFTypeID; cdecl;

  TSecPKCS12Import = function(
    Pkcs12Data: CFDataRef;
    Options: CFDictionaryRef;
    var Items: CFArrayRef): OSStatus; cdecl;

  TSecPolicyCreateBasicX509 = function: SecPolicyRef; cdecl;

  TSecTrustCreateWithCertificates = function(
    Certificates: CFTypeRef;
    Policies: CFTypeRef;
    var Trust: SecTrustRef): OSStatus; cdecl;

  TSecTrustCopyPublicKey = function(
    Trust: SecTrustRef): SecKeyRef; cdecl;

  TSecTrustEvaluate = function(
    Trust: SecTrustRef; var Result: SecTrustResultType): OSStatus; cdecl;

var
  CSSM_CSP_CreateDigestContext: TCSSM_CSP_CreateDigestContext;
  CSSM_DeleteContext: TCSSM_DeleteContext;
  SecCopyErrorMessageString: TSecCopyErrorMessageString;
  SecCertificateCopyData: TSecCertificateCopyData;
  SecCertificateCreateWithData: TSecCertificateCreateWithData;
  SecCertificateCopyCommonName: TSecCertificateCopyCommonName;
  SecCertificateCopySubjectSummary: TSecCertificateCopySubjectSummary;
  SecCertificateCopyPublicKey: TSecCertificateCopyPublicKey;
  SecKeyGetCSSMKey: TSecKeyGetCSSMKey;
  SecIdentityCopyPrivateKey: TSecIdentityCopyPrivateKey;
  SecIdentityCreateWithCertificate: TSecIdentityCreateWithCertificate;
  SecKeychainItemImport: TSecKeychainItemImport;
  SecKeychainItemExport: TSecKeychainItemExport;
  SecKeychainCopyDefault: TSecKeychainCopyDefault;
  SecKeychainOpen: TSecKeychainOpen;
  SecKeychainGetPath: TSecKeychainGetPath;
  SecItemCopyMatching: TSecItemCopyMatching;
  SecItemAdd: TSecItemAdd;
  SecItemUpdate: TSecItemUpdate;
  SecKeychainSearchCreateFromAttributes: TSecKeychainSearchCreateFromAttributes;
  SecKeychainItemCopyAttributesAndData: TSecKeychainItemCopyAttributesAndData;
  SecKeychainSearchCopyNext: TSecKeychainSearchCopyNext;
  SecCertificateAddToKeychain: TSecCertificateAddToKeychain;
  SecCertificateGetTypeID: TSecCertificateGetTypeID;
  SecKeyGetTypeID: TSecKeyGetTypeID;
  SecPolicyCreateBasicX509: TSecPolicyCreateBasicX509;
  SecTrustCreateWithCertificates: TSecTrustCreateWithCertificates;
  SecTrustCopyPublicKey: TSecTrustCopyPublicKey;
  SecTrustEvaluate: TSecTrustEvaluate;

  function kSecClass: CFTypeRef;
  function kSecClassKey: CFTypeRef;
  function kSecValueRef: CFTypeRef;
  function kSecValueData: CFTypeRef;
  function kSecReturnRef: CFTypeRef;
  function kSecReturnData: CFTypeRef;
  function kSecAttrKeyClass: CFTypeRef;
  function kSecAttrKeyClassPublic: CFTypeRef;
  function kSecAttrKeyClassPrivate: CFTypeRef;

  function _Assigned(Proc: Pointer): boolean;

{$ENDIF MACOS}

implementation

{$IFDEF MSWINDOWS}
var
  hAdvapi32Lib: HMODULE = 0;
  hCrypt32Lib: HMODULE = 0;

function NotLink: integer;
begin
  raise Exception.Create('Function is not linked');
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
end;

procedure LoadCryptoLib;
  function GetProc(hLib: HMODULE; Name: string): FARPROC;
  begin
    Result := GetProcAddress(hLib, PChar(Name));
    if Result = nil then
      Result := @NotLink;
  end;
begin
  hAdvapi32Lib := LoadLibrary(PChar(ADVAPI32));
  if hAdvapi32Lib = 0 then
    raise Exception.Create('Cannot load ' + ADVAPI32);

  CryptAcquireContext := GetProc(hAdvapi32Lib, {$IFDEF UNICODE}'CryptAcquireContextW'{$ELSE}'CryptAcquireContextA'{$ENDIF});
  CryptReleaseContext := GetProc(hAdvapi32Lib, 'CryptReleaseContext');
  CryptGetProvParam := GetProc(hAdvapi32Lib, 'CryptGetProvParam');
  CryptGetUserKey := GetProc(hAdvapi32Lib, 'CryptGetUserKey');
  CryptExportKey := GetProc(hAdvapi32Lib, 'CryptExportKey');
  CryptImportKey := GetProc(hAdvapi32Lib, 'CryptImportKey');
  CryptDestroyKey := GetProc(hAdvapi32Lib, 'CryptDestroyKey');
  CryptEnumProviders := GetProc(hAdvapi32Lib, {$IFDEF UNICODE}'CryptEnumProvidersW'{$ELSE}'CryptEnumProvidersA'{$ENDIF});

  hCrypt32Lib := LoadLibrary(PChar(CRYPT32));
  if hCrypt32Lib = 0 then
    raise Exception.Create('Cannot load ' + CRYPT32);

  CertOpenStore := GetProc(hCrypt32Lib, 'CertOpenStore');
  CertSaveStore := GetProc(hCrypt32Lib, 'CertSaveStore');
  CertCloseStore := GetProc(hCrypt32Lib, 'CertCloseStore');
  CertFindCertificateInStore := GetProc(hCrypt32Lib, 'CertFindCertificateInStore');
  CertDuplicateCertificateContext := GetProc(hCrypt32Lib, 'CertDuplicateCertificateContext');
  CertCreateCertificateContext := GetProc(hCrypt32Lib, 'CertCreateCertificateContext');
  CertFreeCertificateContext := GetProc(hCrypt32Lib, 'CertFreeCertificateContext');
  CertGetCertificateContextProperty := GetProc(hCrypt32Lib, 'CertGetCertificateContextProperty');
  CertSetCertificateContextProperty := GetProc(hCrypt32Lib, 'CertSetCertificateContextProperty');
  CertAddCertificateContextToStore := GetProc(hCrypt32Lib, 'CertAddCertificateContextToStore');
  CertDeleteCertificateFromStore := GetProc(hCrypt32Lib, 'CertDeleteCertificateFromStore');
  CryptAcquireCertificatePrivateKey := GetProc(hCrypt32Lib, 'CryptAcquireCertificatePrivateKey');
  CryptImportPublicKeyInfoEx := GetProc(hCrypt32Lib, 'CryptImportPublicKeyInfoEx');
  CertGetNameString := GetProc(hCrypt32Lib, {$IFDEF UNICODE}'CertGetNameStringW'{$ELSE}'CertGetNameStringA'{$ENDIF});
  CertGetIssuerCertificateFromStore := GetProc(hCrypt32Lib, 'CertGetIssuerCertificateFromStore');
{$IFNDEF BCB}
  PFXImportCertStore := GetProc(hCrypt32Lib, 'PFXImportCertStore');
{$ENDIF}
end;

procedure FreeCryptoLib;
begin
  if hAdvapi32Lib > 0 then begin
    FreeLibrary(hAdvapi32Lib);
    hAdvapi32Lib := 0;
  end;

  if hCrypt32Lib > 0 then begin
    FreeLibrary(hCrypt32Lib);
    hCrypt32Lib := 0;
  end;
end;

initialization
  LoadCryptoLib;

finalization
  FreeCryptoLib;
{$ENDIF MSWINDOWS}

{$IFDEF  MACOS}
const
  SECURITYFMX = '/System/Library/Frameworks/Security.framework/Security';
  {$EXTERNALSYM SECURITYFMX}
  KEYCHAINFMX = '/System/Library/Frameworks/Keychain.framework/Keychain';
  {$EXTERNALSYM KEYCHAINFMX}

var
  hSecLib: {$IFDEF POSIX}NativeUInt{$ENDIF}{$IFDEF UNIX}IntPtr{$ENDIF};

function NotLink: integer;
begin
  raise Exception.Create('Function is not linked');
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
end;

function _Assigned(Proc: Pointer): boolean;
begin
  Result := Proc <> @NotLink;
end;

procedure LoadCryptoLib;
  function GetProc(const Name: string): IntPtr;
  begin
    Result := dlsym(hSecLib, PAnsiChar(AnsiString(Name)));
    if Result = nil then
      Result := @NotLink;
  end;

begin
  hSecLib := dlopen(PAnsiChar(AnsiString(SECURITYFMX)), {RTLD_GLOBAL}RTLD_LAZY);
{$IFDEF UNIX}
  if hSecLib = nil then
{$ENDIF}
{$IFDEF POSIX}
  if hSecLib = 0 then
{$ENDIF}
    raise Exception.Create('Cannot load ' + SECURITYFMX);

  CSSM_CSP_CreateDigestContext := GetProc('CSSM_CSP_CreateDigestContext');
  CSSM_DeleteContext := GetProc('CSSM_DeleteContext');
  SecCopyErrorMessageString := GetProc('SecCopyErrorMessageString');
  SecCertificateCopyData := GetProc('SecCertificateCopyData');
  SecCertificateCreateWithData := GetProc('SecCertificateCreateWithData');
  SecCertificateCopyCommonName := GetProc('SecCertificateCopyCommonName');
  SecCertificateCopySubjectSummary := GetProc('SecCertificateCopySubjectSummary');
  SecCertificateCopyPublicKey := GetProc('SecCertificateCopyPublicKey');
  SecKeyGetCSSMKey := GetProc('SecKeyGetCSSMKey');
  SecIdentityCopyPrivateKey := GetProc('SecIdentityCopyPrivateKey');
  SecIdentityCreateWithCertificate := GetProc('SecIdentityCreateWithCertificate');
  SecKeychainItemImport := GetProc('SecKeychainItemImport');
  SecKeychainItemExport := GetProc('SecKeychainItemExport');
  SecKeychainCopyDefault := GetProc('SecKeychainCopyDefault');
  SecKeychainOpen := GetProc('SecKeychainOpen');
  SecKeychainGetPath := GetProc('SecKeychainGetPath');
  SecItemCopyMatching := GetProc('SecItemCopyMatching');
  SecItemAdd := GetProc('SecItemAdd');
  SecItemUpdate := GetProc('SecItemUpdate');
  SecKeychainSearchCreateFromAttributes := GetProc('SecKeychainSearchCreateFromAttributes');
  SecKeychainItemCopyAttributesAndData := GetProc('SecKeychainItemCopyAttributesAndData');
  SecKeychainSearchCopyNext := GetProc('SecKeychainSearchCopyNext');
  SecCertificateAddToKeychain := GetProc('SecCertificateAddToKeychain');
  SecCertificateGetTypeID := GetProc('SecCertificateGetTypeID');
  SecKeyGetTypeID := GetProc('SecKeyGetTypeID');
  SecPolicyCreateBasicX509 := GetProc('SecPolicyCreateBasicX509');
  SecTrustCreateWithCertificates := GetProc('SecTrustCreateWithCertificates');
  SecTrustCopyPublicKey := GetProc('SecTrustCopyPublicKey');
  SecTrustEvaluate := GetProc('SecTrustEvaluate');
end;

procedure FreeCryptoLib;
begin
{$IFDEF UNIX}
  hSecLib := nil;
{$ENDIF}
{$IFDEF POSIX}
  if hSecLib <> 0 then begin
    dlclose(hSecLib);
    hSecLib := 0;
  end;
{$ENDIF}
end;

var
  _kSecClass: Pointer = nil;
  _kSecClassKey: Pointer = nil;
  _kSecValueRef: Pointer = nil;
  _kSecValueData: Pointer = nil;
  _kSecReturnRef: Pointer = nil;
  _kSecReturnData: Pointer = nil;
  _kSecAttrKeyClass: Pointer = nil;
  _kSecAttrKeyClassPublic: Pointer = nil;
  _kSecAttrKeyClassPrivate: Pointer = nil;

function kSecClass: CFTypeRef;
begin
  if _kSecClass = nil then
    _kSecClass := dlsym(hSecLib, PAnsiChar('kSecClass'));
  Result := CFTypeRef(_kSecClass^);
end;

function kSecClassKey: CFTypeRef;
begin
  if _kSecClassKey = nil then
    _kSecClassKey := dlsym(hSecLib, PAnsiChar('kSecClassKey'));
  Result := CFTypeRef(_kSecClassKey^);
end;

function kSecValueRef: CFTypeRef;
begin
  if _kSecValueRef = nil then
    _kSecValueRef := dlsym(hSecLib, PAnsiChar('kSecValueRef'));
  Result := CFTypeRef(_kSecValueRef^);
end;

function kSecValueData: CFTypeRef;
begin
  if _kSecValueData = nil then
    _kSecValueData := dlsym(hSecLib, PAnsiChar('kSecValueData'));
  Result := CFTypeRef(_kSecValueData^);
end;

function kSecReturnRef: CFTypeRef;
begin
  if _kSecReturnRef = nil then
    _kSecReturnRef := dlsym(hSecLib, PAnsiChar('kSecReturnRef'));
  Result := CFTypeRef(_kSecReturnRef^);
end;

function kSecReturnData: CFTypeRef;
begin
  if _kSecReturnData = nil then
    _kSecReturnData := dlsym(hSecLib, PAnsiChar('kSecReturnData'));
  Result := CFTypeRef(_kSecReturnData^);
end;

function kSecAttrKeyClass: CFTypeRef;
begin
  if _kSecAttrKeyClass = nil then
    _kSecAttrKeyClass := dlsym(hSecLib, PAnsiChar('kSecAttrKeyClass'));
  Result := CFTypeRef(_kSecAttrKeyClass^);
end;

function kSecAttrKeyClassPublic: CFTypeRef;
begin
  if _kSecAttrKeyClassPublic = nil then
    _kSecAttrKeyClassPublic := dlsym(hSecLib, PAnsiChar('kSecAttrKeyClassPublic'));
  Result := CFNumberRef(_kSecAttrKeyClassPublic^);
end;

function kSecAttrKeyClassPrivate: CFTypeRef;
begin
  if _kSecAttrKeyClassPrivate = nil then
    _kSecAttrKeyClassPrivate := dlsym(hSecLib, PAnsiChar('kSecAttrKeyClassPrivate'));
  Result := CFTypeRef(_kSecAttrKeyClassPrivate^);
end;

initialization
  LoadCryptoLib;

finalization
  FreeCryptoLib;
{$ENDIF MACOS}

end.
