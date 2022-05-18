
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  SQLite Properties
//  Created:            28.05.15
//////////////////////////////////////////////////

{$I VirtualQuery.inc}
{$I LiteDac.inc}
unit LitePropsVirtual;

interface

const
  prLiteBase              = 1000;
//  prUseUnicode          = prLiteBase + 1; // moved to common level
  prClientLibrary         = prLiteBase + 2;
  prDumpData              = prLiteBase + 3;
  prEncryptionKey         = prLiteBase + 4;
  prASCIIDataBase         = prLiteBase + 5;
  prEnableSharedCache     = prLiteBase + 6;
  prBusyTimeout           = prLiteBase + 7;
  prReadUncommitted       = prLiteBase + 8;
{$IFDEF LITE}
  prNewEncryptionKey      = prLiteBase + 9;
{$ENDIF}
  prDefaultCollations     = prLiteBase + 10;
  prDateFormat            = prLiteBase + 11;
  prTimeFormat            = prLiteBase + 12;
  prForeignKeys           = prLiteBase + 13;
  prStaticLibrary         = prLiteBase + 14;
  prForceCreateDatabase   = prLiteBase + 15;
  prEncryptionAlgorithm   = prLiteBase + 16;
  prAutoCommitRowCount    = prLiteBase + 17;
  prNativeDate            = prLiteBase + 18;
  prEnableLoadExtension   = prLiteBase + 19;
  prConnectMode           = prLiteBase + 20;
  prTableCreationMode     = prLiteBase + 21;
  prUnknownAsString       = prLiteBase + 22;
  prAdvancedTypeDetection = prLiteBase + 23;
  prCipherLicense         = prLiteBase + 24;
  prLockingMode           = prLiteBase + 25;
  prSynchronous           = prLiteBase + 26;
  prJournalMode           = prLiteBase + 27;

implementation

end.
