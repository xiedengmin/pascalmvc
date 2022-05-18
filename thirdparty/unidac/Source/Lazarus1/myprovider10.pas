{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit myprovider10;

{$warn 5023 off : no warning about unused units}
interface

uses
  MySQLUniProvider, MyClassesUni, MyConnectionPoolUni, MyConstsUni, 
  MyPropsUni, MyParserUni, MyScriptProcessorUni, MyServicesUni, 
  MySqlApiDirectUni, MySqlApiUni, MySqlBindUni, MySqlErrorsUni, MySqlNetUni, 
  MySqlResultSetUni, MySqlSessionUni, MySqlStmtUni, MySqlVioPipeUni, 
  MyCallUni, MyDataTypeMapUni, MySQLGeneratorUni, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MySQLUniProvider', @MySQLUniProvider.Register);
end;

initialization
  RegisterPackage('myprovider10', @Register);
end.
