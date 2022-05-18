unit dynamicsprovider10; 

interface

uses
  DynamicsCRMUniProvider, DynamicsClassesUni, DynamicsPropsUni,
  DynamicsConnectionPoolUni, DynamicsConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DynamicsCRMUniProvider', @DynamicsCRMUniProvider.Register); 
end; 

initialization
  RegisterPackage('dynamicsprovider10', @Register); 
end.
