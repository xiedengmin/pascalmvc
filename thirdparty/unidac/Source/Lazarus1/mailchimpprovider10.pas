unit mailchimpprovider10; 

interface

uses
  MailChimpUniProvider, MailChimpClassesUni, MailChimpPropsUni,
  MailChimpConnectionPoolUni, MailChimpConnectionStringUni,
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('MailChimpUniProvider', @MailChimpUniProvider.Register); 
end; 

initialization
  RegisterPackage('mailchimpprovider10', @Register); 
end.
