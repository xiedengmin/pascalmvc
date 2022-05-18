unit salesforceprovider10; 

interface

uses
  SalesforceUniProvider, SalesforceClassesUni, SalesforcePropsUni,
  SalesforceConnectionPoolUni, SalesforceConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('SalesforceUniProvider', @SalesforceUniProvider.Register); 
end; 

initialization
  RegisterPackage('salesforceprovider10', @Register); 
end.
