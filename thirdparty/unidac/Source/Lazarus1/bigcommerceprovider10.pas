unit bigcommerceprovider10; 

interface

uses
  BigCommerceUniProvider, BigCommerceClassesUni, BigCommercePropsUni,
  BigCommerceConnectionPoolUni, BigCommerceConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('BigCommerceUniProvider', @BigCommerceUniProvider.Register); 
end; 

initialization
  RegisterPackage('bigcommerceprovider10', @Register); 
end.
