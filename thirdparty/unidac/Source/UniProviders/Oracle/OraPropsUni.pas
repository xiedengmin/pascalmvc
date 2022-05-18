
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Oracle Properties
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraPropsUni;

interface

const
// Props
  prOCIBase           = 1000;

//  prNonBlocking       = prOCIBase + 1;  // moved to common level
  prThreadSafety      = prOCIBase + 2;  // bool
  prAutoClose         = prOCIBase + 3;  // bool
  prErrorOffset       = prOCIBase + 4;  // word
  prDateFormat        = prOCIBase + 6;  // string
  prDeferredLobRead   = prOCIBase + 7;  // bool
  prConnectMode       = prOCIBase + 8;  // enum
  prCharLength        = prOCIBase + 9;  // word
  prCacheLobs         = prOCIBase + 10; // bool
  prEnableIntegers    = prOCIBase + 11; // bool
  prInternalName      = prOCIBase + 12; // string
  prScrollableCursor  = prOCIBase + 13; // bool
  prStoreRowId        = prOCIBase + 14; // bool
  prCharset           = prOCIBase + 15; // word
  prDateLanguage      = prOCIBase + 16; // string
  prTimeStampFormat   = prOCIBase + 17; // string
  prTimeStampTZFormat = prOCIBase + 18; // string
  prRawAsString       = prOCIBase + 19; // bool
  prNumberAsString    = prOCIBase + 20; // bool
  prNumericCharacters = prOCIBase + 21; // string
  prEnableNumbers     = prOCIBase + 22; // bool
//  prUseUnicode        = prOCIBase + 23; // moved to common level
  prIntegerPrecision  = prOCIBase + 24; // word;
  prFloatPrecision    = prOCIBase + 25; // word;
  prTemporaryLobUpdate= prOCIBase + 26; // bool
  prDisconnectMode    = prOCIBase + 27; // bool
  prInactiveTimeout   = prOCIBase + 28; // integer
  prResumeTimeout     = prOCIBase + 29; // integer
  prTransactionName   = prOCIBase + 30; // string
//  prConnectionTimeOut = prOCIBase + 31; // moved to common level
  prHasObjectFields   = prOCIBase + 32; // bool
  prStatementCache    = prOCIBase + 33; // bool
  prStatementCacheSize= prOCIBase + 34; // integer
  prEnabled           = prOCIBase + 35; // bool
  prTimeout           = prOCIBase + 36; // integer
  prPersistent        = prOCIBase + 37; // bool
  prOperations        = prOCIBase + 38; // set
//  prPrefetchRows      = prOCIBase + 39; // // moved to common level
  prTransactionResume = prOCIBase + 40; // bool
  prRollbackSegment   = prOCIBase + 41; // string
  prHomeName          = prOCIBase + 42; // string
  prDirect            = prOCIBase + 43; // bool
  prClientIdentifier  = prOCIBase + 44; // string
  prOptimizerMode     = prOCIBase + 45; // TOptimizerMode
  prUseOCI7           = prOCIBase + 46; // bool
  prSchema            = prOCIBase + 47; // string
{$IFNDEF FPC}
  prEnableSQLTimeStamp = prOCIBase + 48; // bool
{$ELSE}
  prTimeStampAsString  = prOCIBase + 48; // bool
{$ENDIF}
  prIntervalAsString  = prOCIBase + 49; // bool
  prSmallintPrecision = prOCIBase + 50; // integer
  prLargeintPrecision = prOCIBase + 51; // integer
  prBCDPrecision      = prOCIBase + 52; // integer
  prFmtBCDPrecision   = prOCIBase + 53; // integer
  prCheckParamHasDefault = prOCIBase + 54; // bool
//  prUseResultParams      = prOCIBase + 55; // bool
  prUseDefaultDataTypes  = prOCIBase + 56; // bool
  prEnableLargeint       = prOCIBase + 57; // bool
  prUnicodeEnvironment   = prOCIBase + 58; // bool
  prDirectPath           = prOCIBase + 59; // bool
  prEnableWideOraClob    = prOCIBase + 60; // bool
  prAlerterTimeout       = prOCIBase + 61; // bool
  prAlerterInterval      = prOCIBase + 62; // bool
  prAlerterEventType     = prOCIBase + 63; // bool
  prAlerterSelfEvents    = prOCIBase + 64; // bool
  prUnicodeAsNational    = prOCIBase + 65; // bool
  prQueryResultOnly      = prOCIBase + 66; // bool
  prSubscriptionPort     = prOCIBase + 67; // bool
  prProcNamedParams      = prOCIBase + 68; // bool
  prHideRowId            = prOCIBase + 69; // bool
  prPrefetchLobSize      = prOCIBase + 70; // integer

  prSSLServerCertDN      = prOCIBase + 73; // integer

implementation

end.
