
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//  ODBC Properties
//////////////////////////////////////////////////

{$I ODBCDac.inc}
unit ODBCPropsUni;

interface

const
  prODBCBase              = 1000;
//  prUseUnicode            = prODBCBase + 1; moved to common level
//  prConnectionTimeout     = prODBCBase + 2; moved to common level
//  prCommandTimeout        = prODBCBase + 3; moved to common level
  prDSNType               = prODBCBase + 4;
  prDetectFieldsOnPrepare = prODBCBase + 5;
  prDefaultStrParamSize   = prODBCBase + 6;
  prVarBinaryAsBlob       = prODBCBase + 7;
  prLongVarBinaryAsBlob   = prODBCBase + 8;
  prColumnWiseBinding     = prODBCBase + 9;
  prDriverManager         = prODBCBase + 10;
  prUnknownAsString       = prODBCBase + 11;

implementation

end.
