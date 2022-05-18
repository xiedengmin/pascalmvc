
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I PgDac.inc}
unit PgConstsUni;


interface

const
  PgDefValPort = 5432;
  RsDefValPort = 5439;

resourcestring
  // Protocol
  sUnexpectedServerResponse       = 'Unexpected server response';
  sUnknownAuthMethod              = 'Unknown auth method';
  sUnsupportedAuthMethod          = 'Unsupported auth method';
  SStoredProcNotFound             = 'Stored procedure "%s"."%s" not found';
  SInvalidFieldType               = 'Invalid field type';
  SInvalidInputSyntax             = 'Invalid input syntax for type %s';
  SInvalidOutParamDataType        = 'DataType of %s output parameter does not correspond to the type returned by query';
  SRowTypeNotSet                  = 'Row type not set';
  SBlobNeedTransaction            = 'Active transaction is required for operations with large objects';
  SRefCursorNeedTransaction       = 'Active transaction is required for operations with REFCURSOR';
  SFetchAllFalseNeedTransaction   = 'Active transaction is required for using FetchAll = False mode';
  SInvalidParams                  = 'Invalid parameters specified';
  SAuthenticationFailed           = 'Authentication failed';
  SServerClosedTheConnection      = 'The server forcibly closed the connection';
  STryToSpecifyDatabase           = 'Try to specify the database name in the connection settings.';
  SPostreSQLNotSupported          = 'PostgreSQL servers are not supported';
  SRedshiftNotSupported           = 'Redshift servers are not supported';
  SSSLModeNotSuppoted             = 'The SSLMode = %s is not supported when using OpenSSL';

implementation

end.
