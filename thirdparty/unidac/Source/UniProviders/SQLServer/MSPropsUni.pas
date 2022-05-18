
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  SDAC Properties
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I Sdac.inc}
unit MSPropsUni;

interface

const
  // properties
  prOleDBBase           = 1000;
  // connection properties
  prAuthentication      = prOleDBBase + 2; // TMSAuthentication
  // dataset options
  prUniqueRecords       = prOleDBBase + 8;  // boolean
  prCursorType          = prOleDBBase + 9;  // TMSCursorType
  prRequestSQLObjects   = prOleDBBase + 10; // boolean
//  prCommandTimeout      = prOleDBBase + 15; // moved to common level
  prQuotedIdentifier    = prOleDBBase + 16; // boolean
  prLanguage            = prOleDBBase + 17; // string
  prEncrypt             = prOleDBBase + 18; // boolean
  prNetworkLibrary      = prOleDBBase + 19; // string
  prPacketSize          = prOleDBBase + 20; // integer
  prBCDPrecision        = prOleDBBase + 21; // integer
  prBCDScale            = prOleDBBase + 22; // integer
  // if False then unicode fields is supported as TStringsField else as TWideStringField
  prWideStrings         = prOleDBBase + 23; // boolean
  prApplicationName     = prOleDBBase + 24; // string
  prWorkstationID       = prOleDBBase + 25; // string
  prAutoTranslate       = prOleDBBase + 26; // boolean
  prProvider            = prOleDBBase + 28; // string
  prPersistSecurityInfo = prOleDBBase + 30; // boolean
  prInitialFileName     = prOleDBBase + 31; // string
  prMARS                = prOleDBBase + 32; // boolean
//  prMultipleConnections = prOleDBBase + 33; // moved to common level
  // Query notification
  prNotification        = prOleDBBase + 34; // boolean
  prNotificationMessage = prOleDBBase + 35; // string
  prNotificationService = prOleDBBase + 36; // string
  prNotificationTimeout = prOleDBBase + 37; // cardinal
  prDelayedSubsciption  = prOleDBBase + 38; // boolean
//  prNonBlocking         = prOleDBBase + 39; // moved to common level
  prOldPassword         = prOleDBBase + 40; // string
  prMaxDatabaseSize     = prOleDBBase + 41; // integer
  prFailoverPartner     = prOleDBBase + 42; // string
  prOutputStream        = prOleDBBase + 43; // TStream
  prOutputEncoding      = prOleDBBase + 44; // string
  prTrustServerCertificate = prOleDBBase + 45; // boolean
  // Compact Edition specific
  prTempFileDirectory   = prOleDBBase + 50; // string
  prTempFileMaxSize     = prOleDBBase + 51; // integer
  prDefaultLockEscalation = prOleDBBase + 52; // integer
  prDefaultLockTimeout  = prOleDBBase + 53; // integer
  prAutoShrinkThreshold = prOleDBBase + 54; // integer
  prMaxBufferSize       = prOleDBBase + 55; // integer
  prFlushInterval       = prOleDBBase + 56; // integer
  prTransactionCommitMode = prOleDBBase + 57; // TCompactCommitMode
  prLockTimeout         = prOleDBBase + 58; // integer
  prLockEscalation      = prOleDBBase + 59; // integer
  prInitMode            = prOleDBBase + 60; // integer
  prWideMemos           = prOleDBBase + 61; // boolean
  prCompactVersion      = prOleDBBase + 62; // TCompactVersion
  prSetLockTimeout      = prOleDBBase + 64; // boolean UniDAC behavior
  prNativeClientVersion = prOleDBBase + 65; // TNativeClientVersion
  prKeepIdentity        = prOleDBBase + 66;
  prKeepNulls           = prOleDBBase + 67;
  prRowsPerBatch        = prOleDBBase + 68;
  prKilobytesPerBatch   = prOleDBBase + 69;
  prLockTable           = prOleDBBase + 70;
  prCheckConstraints    = prOleDBBase + 71;
  prFireTrigger         = prOleDBBase + 72;
  prDisableMultipleResults = prOleDBBase + 73;
  prBaseTableName       = prOleDBBase + 74;
  prBulkExecuting       = prOleDBBase + 75;
  prTableTypeName       = prOleDBBase + 76;
  prLocaleIdentifier    = prOleDBBase + 77; // cardinal
  prHideSystemUniqueFields = prOleDBBase + 78;
  prForceCreateDatabase = prOleDBBase + 79; // boolean
  prApplicationIntent   = prOleDBBase + 80; // TApplicationIntent
  prMultiSubnetFailover = prOleDBBase + 81;

implementation

end.
