
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoConstsUni;

interface

const
  MnDefValPort = 27017;
  MnDefValServer = 'localhost';

  MaxStringSize = 2048;
  MaxFieldCount = 2000;

resourcestring
  SFunctionNotLinked      = 'MongoDB function is not linked';
  SInvalidURI             = 'Specified URI is not valid';
  SInvalidDatabase        = 'Invalid database name';
  SInvalidCollection      = 'Invalid collection name';
  SSQLEngineNotSupported  = 'SQL engine is not supported';
  SInvalidCommand         = 'Invalid command';
  SIncompatibleData       = 'Incompatible data for field "%s"';
  SIncompatibleType       = 'Incompatible data type for field "%s"';
  SFieldNotFound          = 'Field "%s" not found';
  SFieldCanNotBeGet       = 'Field can not be taken as type %s';
  SInvalidParams          = 'Invalid parameters specified';
  SFeatureNotSupported    = 'Feature is not supported';
  SInvalidDocument        = 'Invalid document';
  SMethodNotApplicable    = 'The method is not applicable here';
  SFieldIsNotArray        = 'Field "%s" is not an array or object';
  SFieldNotHaveChildren   = 'Field "%s" does not have children';
  SCannotGetData          = 'Can not get field';
  SCannotPutData          = 'Can not put field';

implementation

end.
