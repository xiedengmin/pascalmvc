
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright (c) 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF VIRTUAL_QUERY}
{$I LiteDac.inc}
unit LiteConstsUni;
{$ENDIF}

interface

type
  TLiteEncryptionAlgorithm = (leTripleDES, leBlowfish, leAES128, leAES192, leAES256, leCast128, leRC4, leDefault);
  TConnectMode = (cmDefault, cmReadWrite, cmReadOnly);
  TLockingMode = (lmNormal, lmExclusive);
  TSynchronous = (smOff, smNormal, smFull, smExtra);
  TJournalMode = (jmDelete, jmTruncate, jmPersist, jmMemory, jmWAL, jmOff);

const
  LiteEncryptionAlgorithm: array[TLiteEncryptionAlgorithm] of string = (
    'TripleDES', 'Blowfish', 'AES128', 'AES192', 'AES256', 'Cast128', 'RC4', 'Default'
  );

  DefaultEncryptionAlgorithm = leDefault;
  DefValDirect = False;
  DefValForceCreateDatabase = False;
  DefValUnknownAsString = False;
  DefValEnableSharedCache = False;
  DefValBusyTimeout = 0;
  DefValConnectMode = cmDefault;
  DefValLockingMode = lmExclusive;
  DefValSynchronous = smOff;
  DefValJournalMode = jmDelete;

resourcestring
  SStoredProcNotSupported =  'SQLite does not support stored procedures';
  SClientLibraryDiffers =    'Connection ClientLibrary or UseStaticLibrary differs from already active connections';
  SIncorrectParamCount =     'Incorrect parameter count. Expected: %d; Actual: %d';
  SIncorrectParameter =      'Incorrect parameter.';

  SInMemoryDB = ':memory:';
  STempDB     = '';

  SDatabaseDoesntExist =    'Database "%s" does not exist';
  //SDecryptionError        = 'Invalid EncryptionKey and/or EncryptionAlgorithm specified'; //not used

  SBHCaption              = '-- %s version: %s'#$D#$A +
                            '-- %s client library: %s'#$D#$A +
                            '-- Script date %s'#$D#$A +
                            '-- ---------------------------------------------------------------------- '#$D#$A +
                            '-- Database: %s'#$D#$A#$D#$A;

  SDBBrowse               = '<Browse...>';

  SDirectNotSupported     = 'Feature is not supported';
  SFunctionNotLinked      = 'SQLite function is not linked';
  SDirectIsDisabled       = 'Direct Mode is disabled';

  SCannotUseAlgorithm       = 'Encryption algorithm can be used only in the Direct mode';
  SDefaultAlgorithm         = 'Encryption algorithm not specified';
  SEncryptionKeyIsEmpty     = 'The EncryptionKey property must not be empty when using encryption';
  SEncryptionKeyMustBeEmpty = 'The EncryptionKey property must be empty when no encryption algorithm specified';
  SCannotEncrypt            = 'The operation cannot be performed because the connection has open data sets';

  SBackupSourceNotSpecified = 'Source connection is not specified';
  SBackupDestinationNotSpecified = 'Destination connection is not specified';
  SBackupConnectionsEqual = 'Source and destination connections are equal';
  SBackupSourceNotConnected = 'Source connection is not connected';
  SBackupDestinationNotConnected = 'Destination connection is not connected';

implementation

end.
