{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pgprovider10;

{$warn 5023 off : no warning about unused units}
interface

uses
  PostgreSQLUniProvider, PgCallUni, PgClassesUni, PgConnectionPoolUni, 
  PgConstsUni, PgPropsUni, PgCryptUni, PgErrorUni, PgObjectsUni, PgParserUni, 
  PgScriptProcessorUni, PgServicesUni, PgServerEnumeratorUni, PgSQLNetUni, 
  PgSQLProtocolUni, PgDataTypeMapUni, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PostgreSQLUniProvider', @PostgreSQLUniProvider.Register);
end;

initialization
  RegisterPackage('pgprovider10', @Register);
end.
