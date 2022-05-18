unit zohoprovider10; 

interface

uses
  ZohoCRMUniProvider, ZohoClassesUni, ZohoPropsUni,
  ZohoConnectionPoolUni, ZohoConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ZohoCRMUniProvider', @ZohoCRMUniProvider.Register); 
end; 

initialization
  RegisterPackage('zohoprovider10', @Register); 
end.
