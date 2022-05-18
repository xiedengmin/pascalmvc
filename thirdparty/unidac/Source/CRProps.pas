
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Core Properties
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRProps;

interface

const
// Props
  prUsername            = 1;  // char*
  prPassword            = 2;  // char*
  prServer              = 3;  // char*
  prConnectionTimeout   = 4;  // integer
  prAutoCommit          = 5;  // bool
  prSQL                 = 6;  // char*
  prScanParams          = 7;  // bool
  prSQLType             = 8;  // integer
  prRowsProcessed       = 9;  // integer

  prUniDirectional      = 10; // bool
  prFetchRows           = 11; // integer
  prFetchAll            = 12; // bool
  prLongStrings         = 14; // bool
  prFlatBuffers         = 15; // bool
  prConvertEOL          = 16; // bool
  prIndexFieldNames     = 17; // char*
{$IFDEF HAVE_COMPRESS}
  prCompressBlobMode    = 18; // TCompressBlobMode
{$ENDIF}
  prDisconnectedMode    = 19; // bool
  prDisableParamScan    = 20; // bool
  prQuoteNames          = 21; // bool

  prIsolationLevel      = 22; // TCRIsolationLevel
  prTransactionReadOnly = 23; // bool
  prDatabase            = 24; // string
  prPort                = 25; // integer
  prMaxStringSize       = 26; // integer
  prEnableBCD           = 27; // bool
  prEnableFmtBCD        = 28; // bool
  prCanReadParams       = 29; // boolean
  prReadOnly            = 30; // boolean
  prIsStoredProc        = 31; // boolean
  prIsSelectParams      = 32; // boolean   // for MySQL stored proc
  prLockFetchAll        = 33; // boolean;
  prDefaultSortType     = 34; // TSortType
  prLastInsertId        = 35; // int64
  prFieldsAsString      = 36; // boolean
  prExtendedFieldsInfo  = 37; // boolean
  prDefaultValues       = 38; // boolean
  prFieldOrigins        = 39; // TFieldOrigins
  prUseDescribeParams   = 40; // boolean
  prRoAfterUpdate       = 41; // boolean
  prEncryptedFields     = 42; // string
  prOpenNext            = 43; // boolean
  prSensibleBCDMapping  = 44; // boolean UniDAC behavior
  prCursorUpdate        = 45; // boolean
  prMultipleConnections = 46; // boolean
  prUuidWithBraces      = 47; // boolean
  prPrefetchRows        = 48; // integer

  // HTTP
  prUseHttp             = 50; // boolean
  prHttpUrl             = 51; // string
  prHttpUsername        = 52; // string
  prHttpPassword        = 53; // string
  prHttpTrustServerCertificate = 54; // boolean

  // Proxy
  prProxyHostname       = 55; // string
  prProxyPort           = 56; // integer
  prProxyUsername       = 57; // string
  prProxyPassword       = 58; // string
  prProxySocksVersion   = 59; // TCRSocksVersion
  prProxyResolveDNS     = 60; // boolean

  // SSL
  prUseSSL              = 61; // boolean
  prSSLCA               = 62; // string
  prSSLCert             = 63; // string
  prSSLKey              = 64; // string
  prSSLCipher           = 65; // string
  prSSLIgnoreServerCertificateValidity    = 66; // boolean
  prSSLIgnoreServerCertificateConstraints = 67; // boolean
  prSSLTrustServerCertificate             = 68; // boolean
  prSSLIgnoreServerCertificateInsecurity  = 69; // boolean

  // SSH
  prUseSSH              = 71; // boolean
  prSSHHostname         = 72; // string
  prSSHPort             = 73; // integer
  prSSHUsername         = 74; // string
  prSSHPassword         = 75; // string
  prSSHClientKey        = 76; // string
  prSSHServerKey        = 77; // string
  prSSHStoragePath      = 78; // string
  prSSHClientKeyPassword = 79; // string

  prIPVersion           = 80; // TCRIPVersion
  prFullRefresh         = 81; // boolean
  prSetFieldsReadOnly   = 82; // boolean
  prKeyFields           = 83; // string
  prUpdatingTable       = 84; // string
  prKeySequence         = 85; // string
  prPrefetchedFields    = 86; // string
  prLiveBlockOnSmartFetch = 87; // boolean
  prParamsProcessed     = 88; // integer
  prCommandTimeout      = 89; // integer
  prUseUnicode          = 90; // boolean
  prInsertAllSetFields  = 91; // boolean
  prNonBlocking         = 92; // boolean;

  prBatchIters          = 93; // integer
  prBatchOffset         = 94; // integer

  // Loader
  prUseBlankValues      = 95; // boolean
  prUTCDates            = 96; // boolean

implementation

end.
