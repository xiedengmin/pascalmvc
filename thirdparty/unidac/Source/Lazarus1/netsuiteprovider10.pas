unit netsuiteprovider10; 

interface

uses
  NetSuiteUniProvider, NetSuiteClassesUni, NetSuitePropsUni,
  NetSuiteConnectionPoolUni, NetSuiteConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('NetSuiteUniProvider', @NetSuiteUniProvider.Register); 
end; 

initialization
  RegisterPackage('netsuiteprovider10', @Register); 
end.
