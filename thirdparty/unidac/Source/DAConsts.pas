
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  DB Access constants
//////////////////////////////////////////////////

{$I Dac.inc}
unit DAConsts;

interface

const

  DALineSeparator = #$D;

  // TStringList line separator
{$IFDEF MSWINDOWS}
  SLLineSeparator = #$D#$A;
{$ELSE}
  SLLineSeparator = #$A;
{$ENDIF}
  Tabulation = #9;
  MaxUTF8CharLen = 3;

// default connection property values
  DefValLoginPrompt = True;
  DefValPooling = False;
  DefValConnectionLifetime = 0;
  DefValMaxPoolSize = 100;
  DefValMinPoolSize = 0;
  DefValValidate = False;
  DefValConnectionTimeout = 15;
  DefValUseUnicode = False;
  DefValProxyPort = 0;

resourcestring
  SUnknownDataType       = 'Unknown data type';
  SDataTypeNotSupported  = 'Data type is not supported';
  SFieldNotFound         = 'Field %s not found';
  SAttributeNotFount     = 'Attribute %s not found';
  SCannotConvertType     = 'Cannot convert type';
  SIllegalFilter         = 'Illegal filter expression';
  SIllegalConstraint     = 'Illegal constraint expression';
  SNeedBlobType          = 'Field is not BLOB';
  SInvalidSharedObject   = 'Invalid SharedObject';
  SInvalidBlob           = 'Invalid BLOB';
  SBlobMustBeCached      = 'Blob must be in cached mode';
  SCachedAlreadyEnabled  = 'Cached is already enabled';
  SKeyFieldsRequired     = 'Key fields required to call FindKey or FindNearest';
  SKeyFieldsReq          = 'Unique key fields required';
  SNoKeyFields           = 'Key fields not found';
  SBadTableInfoName      = 'TableInfo name must be provided';
  SBadStatementType      = 'Bad statement type';
  SBadUpdatingTable      = 'Wrong UpdatingTable value - table %s is unknown';
  SKeyFieldNotFound      = 'Key field ''%s'' not found';
  SNotRows               = 'SQL statement doesn''t return rows';
  SIndexFieldNamesWithUniDirectional = 'IndexFieldNames must be empty when UniDirectional is True';

  SInvalidUnComprBlobSize = 'Invalid uncompressed blob size';
  SInvalidComprBlobSize   = 'Invalid compressed blob size';
  SInvalidComprBlobHeader = 'Invalid compressed blob header';
  SInvalidComprBlobData   = 'Invalid compressed blob data';

  SDataSetIsNotPrepared  = 'DataSet is not prepared';
  SInvalidKeyField       = 'Error in KeyFields string';
  SNotCachedUpdate       = 'Not in cached update mode';
  SUpdateWrongDB         = 'Cannot update, %s is not owned by %s';

  SDataSetMustBeInState             = 'DataSet must be in the %s state';
  SDataSetMustNotHavePendingUpdates = 'DataSet must not have pending updates';

  SConnectionNotDefined   = 'Connection is not defined';
  SConnectionNotConnected = 'Connection is not connected';
  SConnectionOpen         = 'Cannot perform this operation on an opened connection';
  SConnectionClosed       = 'Cannot perform this operation on an closed connection';
  SCannotConnect          = 'Cannot connect to database';
  SMacroNotFound          = 'Macro %s not found';
  SNotInTransaction       = 'Can''t perform operation on inactive transaction';
  SInTransaction          = 'Can''t perform operation on active transaction';
  STransactionNotAssigned = 'Transaction not assigned';
  SUpdateFailed           = 'Update failed. Found %d records';
  SCustomUpdateFailed     = 'Update failed';
  SRefreshFailed          = 'Refresh failed. Found %d records';
  SInvalidFetchRows       = 'FetchRows must be 1..65535';
  SNoCorrespondParam      = 'Not found field corresponding parameter %s';
  SUnknownParamDataType   = 'Unknown data type of parameter %s';
  SRecordChanged          = 'Record was changed by another user';
  SRecordDeleted          = 'Record was deleted by another user';
  SRecordNotFound         = 'Record is not found';
  STableNameNotDefined    = 'TableName must be defined';
  SStoredProcNotDefined   = 'StoredProcName must be defined';
  SConnectionIsClosed     = 'Operation is not allowed when the connection is closed';
  SCannotPerformIfPooling = 'Operation is not allowed when Pooling is True';
  SMaxConnectionsReached  = 'Maximum connections reached in pool';
  SDataSetNotDefined      = 'DataSet must be defined';

  SCannotChangeIsUnicode  = 'Cannot change IsUnicode if Size > 0';
  SColumnNotFound         = 'Column %s not found';
  SNoTimers = 'Not enough timers available';

  sBlobNotCompressed      = 'Blob is not compressed';
  SCompressorNotLinked    = 'Compressor function is not linked';
  SUncompressorNotLinked  = 'Uncompressor function is not linked';

  SUpdateComponentCircularReferences = 'Circular references are not allowed between TCustomDADataset and TCustomDAUpdateSQL objects';
  SUpdateComponentInvalidType        = 'Only %s and %s objects are allowed in update properties';
  SUpdateObjectEmptySQL              = 'SQL property of InsertObject, ModifyObject, DeleteObject or RefreshObject can''t be empty';

  SDateEncodeError        = 'Invalid argument to date encode';
  SInvalidXML             = 'Invalid XML document';

  SWrongTblCount          = 'To execute BackupQuery TableNames must have not more than one table';
  SBackupQueryWrongTableCount = 'Query must contain one table';
  SBHCaption              = '-- %s version: %s'#$D#$A +
                            '-- %s server version: %s'#$D#$A +
                            '-- %s client version: %s'#$D#$A +
                            '-- Script date %s'#$D#$A +
                            '-- ---------------------------------------------------------------------- '#$D#$A +
                            '-- Server: %s'#$D#$A +
                            '-- Database: %s'#$D#$A#$D#$A;
  SBHTableData            = '-- '#$D#$A +
                            '-- Dumping data for table %s'#$D#$A +
                            '-- '#$D#$A#$D#$A;

  SAreYouSureRestore      = 'Are you sure to restore data?';

  SInvalidBatchMove       = 'Invalid batch move parameters';

  SInvalidLexem           = 'Invalid lexem "%s" at position %d in statement:'#$D#$A'%s';

  SEmptySQLStatement      = 'No SQL statement provided';

  SInvalidBlobPosition    = 'Invalid BLOB position';

  SNoConnectionsInTransaction = 'There are no connections in the transaction';
  SMultiConnectionsInTransaction = 'There are more than one connection in the transaction';
  SConnectionInTransactionNotActive = 'One of the connections in the transaction is not active';
  SUnsupportedIsolationLevel = 'Unsupported transaction isolation level';
  SIsolationLevelNotSupportedWithMTS = 'Transaction isolation level is not supported with MTS';
  SReadOnlyTransactionNotSupported = 'Readonly transactions are not supported by the database';
  SOperationNotSupported  = 'Operation is not supported by the database';
  SMultipleTransactionsNotSupported = 'Multiple transactions are not supported by the database';
  SMTSNotSupported = 'MTS transaction coordinator is not supported by the database';

  SVarSubtypeNotSupported = 'Variant subtype is not supported: %d';
  SInvalidParamsArray     = 'Invalid Params array. Valid format: <Name1>,<Value1>[,<Name2>,<Value2>[...]]';
  SUnsupportedMetaDataKind = 'Unsupported metadata kind';
  SRestrictionMustBeSet   = '%s restriction must be set';

  SEventsNotDefined = 'Events names are not defined';
  SMDIsNotMemDataSet = 'Master dataset must be inherited from TMemDataSet';

{$IFDEF FPC}
  SFPCNotSupported = 'This functionality is not supported for Free Pascal for the time being';

  SFieldReadOnly = 'Field ''%s'' can''t be modified';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  SFieldRequired = 'Field ''%s'' must have a value';
  SCircularDataLink = 'Circular datalinks are not allowed';
  sTargetBufferTooSmall = 'ZLib error: target buffer may be too small';
  sError = 'Error';
  SBcdOverflow = 'BCD overflow';
  SNotEditing = 'Dataset not in edit or insert mode';
{$ENDIF}

  SInternalError = 'Internal Error';
  SNumericOverflow = 'Numeric overflow';
  SEInvalidInputArgs = 'Invalid input arguments';
  SEPositiveOverflow = 'Value positive overflow';
  SENegativeUnderflow = 'Value negative underflow';
  SEValueHasNoInverse = 'The BigInteger value has no inverse';
  SEInvalidBigIntegerRadix = 'Radix must be in following range: [2..36]';
  SENotPositiveExponents = 'Only positive exponent is allowed';

  SProtectionCircular     = 'Circular Protection detected, Protection Object is invalid';
  SNotInitialized         = '%s is not initialized. Set Key first';
  SInvalidInputArgs       = 'Invalid input arguments';
  SInvalidIV              = 'Invalid Initialization Vector';
  SInvalidKey             = 'Key is invalid. Set Key or Password first';
  SInvalidKeySize         = 'Length from Encryption key is invalid.'#13#10 +
                            'Keysize for %s must be %d-%d bytes';
  SHashChangedAfterWrite  = 'Hash key can''t be changed after the first write to the stream';
  SGCMModeMustBeUsed      = 'GCM mode must be used';
  SGCMCannotBeUsed        = 'GCM supports only symmetric block ciphers whose block size is 128 bits';
  SEncryptionNotSupported = 'Data type can''t be encrypted: %s';
  SInvalidEncDataSize     = 'Invalid encrypted data size';
  SInvalidEncData         = 'Encrypted data is corrupt';
  SInvalidHash            = 'Encrypted data is corrupt - Invalid hash';
  SEncryptorNotDefined    = 'Encryptor is not defined';
  SEncrFieldsNotDefined   = 'Fields for encryption are not defined';

  SConnectParamNameUnknown   = 'Connection parameter name is unknown: %s';
  SConnectParamNameMissing   = 'Connection parameter name missing';
  SConnectParamValueMissing  = 'Connection parameter value missing';
  SConnectParamInvalidChar   = 'Invalid character ''%s'' in non-quoted connection parameter value';
  SConnectParamAddedTwice    = 'Connection parameter can''t be added twice: %s';
  SInvalidConnectParamValue  = 'Invalid connection parameter or value:'#13#10'  %s=%s';
  SConnectParamInternalError = 'Internal error'#13#10'Connection parameter: %s';

  SLoginParameterServer   = 'SERVER';
  SLoginParameterUsername = 'USERNAME';
  SLoginParameterPassword = 'PASSWORD';

  // Data Type Mapping
  SUnexpectedMapping = 'Unexpected data type mapping error';
  SUnsupportedMapping = 'Unsupported data type mapping: "%s" to "%s"';
  SInvalidMapRuleExpr = 'Invalid data type map rule expression';
  SUnexpectedEndOfMapRuleExpr = 'Unexpected end of data type map rule expression';
  SUnexpectedSymbolInMapRuleExpr = 'Expected "%s" instead of "%s" in data type map rule'{$IFNDEF CB5} + ' expression'{$ENDIF};

  SNotDefinedDBType = 'Database type is not defined';
  SUnsupportedDBType = 'Unsupported database type: %s';
  SInvalidDBLength = 'Length can''t be specified for the "%s" database type';
  SInvalidDBScale = 'Scale can''t be specified for the "%s" database type';

  SNotDefinedFieldType = 'Field type is not defined';
  SUnsupportedFieldType = 'Unsupported field type: %s';
  SInvalidFieldLength = 'Length can''t be specified for the "%s" field type';
  SInvalidFieldScale = 'Scale can''t be specified for the "%s" field type';

  SInvalidDataMapping = 'Value can''t be converted: %s';
  SInvalidIntegerValue = '%s is an invalid integer value';
  SInvalidNumericValue = '%s is an invalid numeric value';
  SInvalidBooleanValue = '%s is an invalid boolean value';
  SInvalidDateTimeValue = '%s is an invalid date or time value';
  SInvalidSQLTimeStampValue = '%s is an invalid SQLTimeStamp value';
  SInvalidGUIDValue = '%s is an invalid GUID value';
  SInvalidIntervalValue = '%s is an invalid Interval value';
  SInvalidBinaryValue = 'Invalid binary value: %s';
  SInvalidBlobValue = 'Invalid blob value: %s';
  SValueOutOfRange = 'Value is out of range: %s';
  SInvalidValueScale = '%s has invalid scale';
  SFractionTruncated = 'Fractional part was truncated: %s';
  SDataTruncated = 'Date or time value was truncated: %s';
  SValueTooLong = 'Value is too long: %s';
  SStringTooLong = 'String value is too long: %s';
  SBinaryTooLong = 'Binary value is too long: %s';
  SParameterIsDmlArray = 'The parameter is a DML array';
  SInvalidIndex = 'Array index out of range';
  SInvalidDmlArrayType = 'Data type is not allowed for a DML array';
  SCannotSetSingleValue = 'Can not set a single value for the parameter of a DML array type';
  SCannotGetSingleValue = 'Can not access a DML array as a single value';
  SParameterTypeDiffers = 'The parameter has different data type';
  SUnknownParameterDataType = 'Unknown parameter data type';

  SInvalidBatchOperation = 'The SQL statement is not allowable for a bulk operation';
  SInvalidBatchParameters = 'Invalid batch parameters count';

  SConditionNotFound = 'Condition %s not found';
  SNoDataToFetch = 'There is no data to fetch';
  SUnknownSmartFetchMode = 'Unknown smart fetch mode';
  SOnlySelectAllowed = 'Only SELECT statement is allowed';
  SSmartFetchIsUnallowed = '%s can''t be called in the SmartFetch mode'; // Execute, Prepare
  SFieldsNotCorresponding = 'Fields of received result set do not correspond to fields of source dataset';
  SRecordsetIsClosed = 'Operation is not allowed when the recordset is closed';
  SIndexFieldNamesWithLiveBlock = 'IndexFieldNames must be empty when LiveBlock is True in SmartFetch mode';

  SInvalidRecordNumber = 'Invalid record number';
  SOpeningWasCanceled = 'Query opening was canceled during data fetching';
  SLocalSortingServerCursor  = 'Local sorting is not compatible with server cursor types';

  SIncorrectConnectionType = 'Incorrect connection type. Expected: %s; Actual: %s';

implementation

end.
