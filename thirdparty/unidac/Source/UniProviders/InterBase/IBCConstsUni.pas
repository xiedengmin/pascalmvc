
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright (c) 1998-2021 Devart. All right reserved.
//  InterBase constants
//////////////////////////////////////////////////

unit IBCConstsUni;

interface

const
  DefaultDBSQLDialect = 3;
  DefaultIBDACPort = 3050;
  DefaultIBDACSPort = '3050';
  DefaultUniDACSPort = '0';
  DefaultIBDACSProtocol = 'TCP';
  DefaultIBDACServiceName = 'gds_db';

resourcestring

  SCursorNotOpened          = 'Cursor must be opened';
  SCantExecuteNonSPStatement = 'Can''t execute non stored proc statement';
  SCantExecuteNonSelectStatement = 'Can''t execute statement that doesn''t return dataset';
  SDatabaseNameMissing      = 'Database name missing';
  STransAreDifferent        = 'DataSets use different transactions';
  SNotInCachedUpdate        = 'DataSet is not active or not in CachedUpdates mode';
  SBlobNotAllocatted        = 'BLOB is not allocated';
  SCantReadEmptyBlobID      = 'Invalid BLOB ID';
  SCantSetReadPosition      = 'Can''t set BLOB read position';
  SCannotChangeStreamed     = 'Can''t change Streamed mode with non-cached BLOB';
  SCannotDisableBlobCache   = 'Can''t disable BLOB cache';
  SCannotCompressNotCached  = 'Can''t compress non-cached BLOB';
  SParamIsNotStorable       = 'Parameter cannot be stored';
  SSQLInfoError             = 'SQL information exception';
  SWrongSQLType             = 'Unsupported SQL type';
  SEmptySQL                 = 'SQL statement cannot be empty';
  SDPBConstantNotSupported  = 'Unsupported DPB constant (isc_dpb_%s)';
  SDPBConstantUnknown       = 'Unknown DPB constant (%s)';
  STPBConstantNotSupported  = 'Unsupported TPB constant (isc_tpb_%s)';
  STPBConstantUnknown       = 'Unknown TPB constant (%s)';
  SConnClosedOnTrStart      = 'Can''t start transaction with closed connection';
  SCantEndSharedTransaction = 'Can''t finish a shared transaction';
  SEventNameTooLong         = 'Event name is too long. Maximum event name length is %d';
  SSendEventNotSupported    = 'Sending events is supported starting with Firebird 2';
  SClientLibraryDiffers     = 'Connection ClientLibrary differs from already active connections';

  SVarIsNotArray            = 'Variant does not contain an array';
  SVarArrayBounds           = 'Variant array bounds error';
  SInvalidTable             = 'Invalid table name';
  SInvalidColumn            = 'Invalid column name';
  SCannotDisableArrayCache  = 'Can''t disable array cache';

  SArrayDimensionError      = 'Arrays dimesions are different';  
  SArrayLowBoundError       = 'Array low bound out of bounds (%d)';
  SArrayHighBoundError      = 'Array high bound out of bounds (%d)';
  SArrayIndexError          = 'Array index out of bounds (%d)';

  SInvalidDimension         = 'Array dimension is not valid';

  SInvalidArgument          = 'Invalid %s argument';

  SSPBConstantNotSupported  = 'Unsupported SPB constant (isc_spb_%s)';
  SSPBConstantUnknown       = 'Unknown SPB constant (%s)';
  SServerNameMissing        = 'Server name missing';
  SServiceActive            = 'Cann''t perform operation - service is not attached';
  SServiceInActive          = 'Cann''t perform operation - service is attached';
  SQueryParamsError         = 'Query parameters missing or incorrect';
  SStartParamsError         = 'Start parameters missing or incorrect';
  SOutputParsingError       = 'Unexpected Output buffer value';
  SNoVersionInfo            = 'Version information for this server is not retrieved';
  SUseSpecificProcedures    = 'Generic ServiceStart not applicable: use specific procedures to set configuration params';
  SIB75feature              = 'This function is supported only in InterBase starting from version 7.5. To use this functionality, please use InterBase 7.5 or higher.';
  SFBNotSupported           = 'This function is not supported in Firebird.';
  SBackupFileNotSpecified   = 'Backup file is not specified';
  SInvalidParamSize         = 'Size of parameters greater than 65535 is not supported by InterBase and Firebird. Use blob parameters instead.';

  SInvalidCharset           = 'Invalid charset: %s';

implementation

end.
