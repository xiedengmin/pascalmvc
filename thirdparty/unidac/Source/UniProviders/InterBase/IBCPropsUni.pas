
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Interbase Properties
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I IbDac.inc}
unit IBCPropsUni;

interface

const
  prGDSBase                  = 1000;

  prCharset                  = prGDSBase + 1;  // string
//  prUseUnicode               = prGDSBase + 2; // moved to common level
  prRole                     = prGDSBase + 3;  // string
  prDBSQLDialect             = prGDSBase + 4;  // integer ReadOnly default 3;
  prSQLDialect               = prGDSBase + 5;  // integer
  prCharLength               = prGDSBase + 6;  // word
  prOptimizedNumerics        = prGDSBase + 7;  // bool for DbxIda
  prEnableMemos              = prGDSBase + 8;  // bool
  prAutoClose                = prGDSBase + 9;  // bool
  prCursor                   = prGDSBase + 10; // string
  prPlan                     = prGDSBase + 11; // string
  prQueryRowsAffected        = prGDSBase + 12; // bool
  prParsedSQLType            = prGDSBase + 13; // word
  prDeferredBlobRead         = prGDSBase + 15; // bool
  prStreamedBlobs            = prGDSBase + 16; // bool
  prCacheBlobs               = prGDSBase + 17; // bool
  prComplexArrayFields       = prGDSBase + 18; // bool
  prCacheArrays              = prGDSBase + 19; // bool
  prDeferredArrayRead        = prGDSBase + 20; // bool
  prBooleanDomainFields      = prGDSBase + 21; // bool
  prClientLibrary            = prGDSBase + 22; // string
  prSimpleNumericMap         = prGDSBase + 23; // bool
  prProtocol                 = prGDSBase + 24; // TIBCProtocol
  prRowsInserted             = prGDSBase + 26;
  prRowsUpdated              = prGDSBase + 27;
  prRowsDeleted              = prGDSBase + 28;
  prEnableLargeint           = prGDSBase + 29;
  prInsertMode               = prGDSBase + 30;
  prRowsPerBatch             = prGDSBase + 31;
  prSetDomainNames           = prGDSBase + 32;
  prTransactionParams        = prGDSBase + 33;
  prTrustedAuthentication    = prGDSBase + 34;
  prNoDBTriggers             = prGDSBase + 35;
  prForceUsingDefaultPort    = prGDSBase + 36; // bool for DbxIda
  prForceUnloadClientLibrary = prGDSBase + 37; // bool for UniDAC
{$IFDEF LITE}
  prWireCompression          = prGDSBase + 38; // bool for DbxIda
{$ENDIF}
  prConnectionParams         = prGDSBase + 40;
  prServerPublicFile         = prGDSBase + 41; // string
  prServerPublicPath         = prGDSBase + 42; // string
  prClientCertFile           = prGDSBase + 43; // string
  prClientPassPhraseFile     = prGDSBase + 44; // string
  prClientPassPhrase         = prGDSBase + 45; // string

implementation

end.
