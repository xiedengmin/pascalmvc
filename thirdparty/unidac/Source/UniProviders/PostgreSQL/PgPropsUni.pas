
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//  PostgreSQL Properties
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I PgDac.inc}
unit PgPropsUni;

interface

const
  prPgSQLBase              = 1000;

  prSchema                 = prPgSQLBase + 1;
//  prUseUnicode             = prPgSQLBase + 2; // moved to common level
  prCharset                = prPgSQLBase + 3;
  prEnablePgTimeStamps     = prPgSQLBase + 5;
  prIntervalAsString       = prPgSQLBase + 6;
  prEnableGeometrics       = prPgSQLBase + 7;
  prEnableComposites       = prPgSQLBase + 8;
  prSimpleQueryExecute     = prPgSQLBase + 9;
  prOIDAsInt               = prPgSQLBase + 10;
  prCacheBlobs             = prPgSQLBase + 11;
  prDeferredBlobRead       = prPgSQLBase + 12;
//  prCommandTimeout         = prPgSQLBase + 13; // moved to common level
  prCursorAsString         = prPgSQLBase + 14;
  prUnknownAsString        = prPgSQLBase + 15;
  prAutoClose              = prPgSQLBase + 16;
  prProtocolVersion        = prPgSQLBase + 17;
  prSSLMode                = prPgSQLBase + 18;
  prFieldsAsText           = prPgSQLBase + 23;
  prBufferSize             = prPgSQLBase + 25;
  prTextMode               = prPgSQLBase + 26;
  prUseParamTypes          = prPgSQLBase + 27;
  prCursorWithHold         = prPgSQLBase + 28;
  prApplicationName        = prPgSQLBase + 29;
  prBCDPrecision           = prPgSQLBase + 30;
  prEnableDomains          = prPgSQLBase + 31;
  prDistinctParams         = prPgSQLBase + 32;
  prImmediateNotices       = prPgSQLBase + 33;
  prMessagesCharset        = prPgSQLBase + 34;
  prRedshiftConnection     = prPgSQLBase + 35;
  prMultipleConnections    = prPgSQLBase + 36;

implementation

end.
