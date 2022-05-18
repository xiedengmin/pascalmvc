{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit liteprovider10;

{$warn 5023 off : no warning about unused units}
interface

uses
  SQLiteUniProvider, LiteCallUni, LiteClassesUni, LiteConstsUni, LitePropsUni, 
  LiteErrorUni, LiteParserUni, LiteServicesUni, LiteCollationUni, 
  LiteFunctionUni, LiteDataTypeMapUni, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SQLiteUniProvider', @SQLiteUniProvider.Register);
end;

initialization
  RegisterPackage('liteprovider10', @Register);
end.
