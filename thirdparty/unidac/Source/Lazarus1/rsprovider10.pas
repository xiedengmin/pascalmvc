unit rsprovider10; 

interface

uses
  RedshiftUniProvider,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('RedshiftUniProvider', @RedshiftUniProvider.Register); 
end; 

initialization
  RegisterPackage('rsprovider10', @Register); 
end.
