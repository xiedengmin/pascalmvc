unit magentoprovider10; 

interface

uses
  MagentoUniProvider, MagentoClassesUni, MagentoPropsUni,
  MagentoConnectionPoolUni, MagentoConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('MagentoUniProvider', @MagentoUniProvider.Register); 
end; 

initialization
  RegisterPackage('magentoprovider10', @Register); 
end.
