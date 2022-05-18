
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  MySQL Properties
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MyPropsUni;

interface

const
  // properties
  prMySQLBase       = 1000;

  prNullForZeroDate = prMySQLBase      + 1; // boolean
  prCompress        = prMySQLBase      + 2; // boolean
  prCharset         = prMySQLBase      + 3; // string
//  prMySQLAPI        = prMySQLBase      + 4; // MySQLAPIClient, MySQLAPIEmbedded, MySQLAPIDirect
  prProtocol        = prMySQLBase      + 5; // boolean

  prNumberFieldsAsString = prMySQLBase + 6; // boolean
  prEnableBoolean   = prMySQLBase      + 7; // boolean
  prCreateConnection = prMySQLBase     + 8; // boolean
//  prCommandTimeout  = prMySQLBase      + 10; // moved to common level
//  prUseUnicode      = prMySQLBase      + 12; // moved to common level
  prEmbParams       = prMySQLBase      + 13; // string
//  prCompressBlobMode = prMySQLBase     + 14; // TCompressBlobMode
  prUseHandler      = prMySQLBase      + 16; // boolean
  prEmbedded        = prMySQLBase      + 17; // boolean
  prDirect          = prMySQLBase      + 18; // boolean
  prCheckPrecision  = prMySQLBase      + 19; // boolean
  prOptimizedBigInt = prMySQLBase      + 20; // boolean
  prBinaryAsString  = prMySQLBase      + 21; // boolean
  prCheckBackslashes = prMySQLBase     + 22; // boolean
  prNeedBackslashes = prMySQLBase      + 23; // boolean
  prLock            = prMySQLBase      + 24;
  prDelayed         = prMySQLBase      + 25;
  prRowsPerQuery    = prMySQLBase      + 26;
  prDuplicateKeys   = prMySQLBase      + 27;
  prNullForZeroDelphiDate = prMySQLBase+ 28; // boolean
  prInteractive     = prMySQLBase      + 29; // boolean
  prBaseDir         = prMySQLBase      + 30;
  prDataDir         = prMySQLBase      + 31;
  prTimeStampAsVarChar = prMySQLBase      + 32;

implementation

end.
