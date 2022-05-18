unit dbfprovider10; 

interface

uses
  DBFUniProvider, DBFConstsUni, DBFStructsUni, DBFEngineUni, DBFClassesUni, DBFConnectionUni, 
  DBFPropsUni, DBFIndexesUni, DBFDataTypeMapUni, DBFServicesUni, DBFParserUni, DBFUtilsUni,
  DBFMemosUni, DBFDBaseUni, DBFFoxProUni, DBFHiPerSixUni, DBFConnectionStringUni, DBFFunctionUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DBFUniProvider', @DBFUniProvider.Register); 
end; 

initialization
  RegisterPackage('DBFprovider10', @Register); 
end.
