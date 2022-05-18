{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mongoprovider10; 

interface

uses
  MongoDBUniProvider, MongoClassesUni, MongoDataTypeMapUni, MongoObjectsUni,
  MongoConnectionStringUni, MongoPropsUni, MongoServicesUni, MongoCallUni, MongoConstsUni, MongoErrorUni,
  MongoConnectionPoolUni, MongoJsonUni, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('MongoDBUniProvider', @MongoDBUniProvider.Register); 
end; 

initialization
  RegisterPackage('mongoprovider10', @Register); 
end.
