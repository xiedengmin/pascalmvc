unit oraprovider10; 

interface

uses
  OraCallUni, OraParserUni, OraNumberUni, OraDateTimeUni, OraClassesUni, 
  OraConnectionPoolUni, OraObjectsUni, OraConstsUni, OraErrorUni, 
  OraServicesUni, OraScriptProcessorUni, OraSQLGeneratorUni, OracleUniProvider, 
  LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('OracleUniProvider', @OracleUniProvider.Register); 
end; 

initialization
  RegisterPackage('oraprovider10', @Register); 
end.
