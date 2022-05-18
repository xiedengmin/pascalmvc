unit freshbooksprovider10; 

interface

uses
  FreshBooksUniProvider, FreshBooksClassesUni, FreshBooksPropsUni,
  FreshBooksConnectionPoolUni, FreshBooksConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('FreshBooksUniProvider', @FreshBooksUniProvider.Register); 
end; 

initialization
  RegisterPackage('freshbooksprovider10', @Register); 
end.
