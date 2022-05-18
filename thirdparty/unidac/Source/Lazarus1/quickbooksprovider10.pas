unit quickbooksprovider10; 

interface

uses
  QuickBooksUniProvider, QuickBooksClassesUni, QuickBooksPropsUni,
  QuickBooksConnectionPoolUni, QuickBooksConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('QuickBooksUniProvider', @QuickBooksUniProvider.Register); 
end; 

initialization
  RegisterPackage('quickbooksprovider10', @Register); 
end.
