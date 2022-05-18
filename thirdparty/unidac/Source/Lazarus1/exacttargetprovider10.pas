unit exacttargetprovider10; 

interface

uses
  SalesforceMCUniProvider, ExactTargetClassesUni, ExactTargetPropsUni,
  ExactTargetConnectionPoolUni, ExactTargetConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('SalesforceMCUniProvider', @SalesforceMCUniProvider.Register); 
end; 

initialization
  RegisterPackage('exacttargetprovider10', @Register); 
end.
