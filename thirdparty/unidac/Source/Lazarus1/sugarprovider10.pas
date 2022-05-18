unit sugarprovider10; 

interface

uses
  SugarCRMUniProvider, SugarClassesUni, SugarPropsUni,
  SugarConnectionPoolUni, SugarConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('SugarCRMUniProvider', @SugarCRMUniProvider.Register); 
end; 

initialization
  RegisterPackage('sugarprovider10', @Register); 
end.
