{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dclunidac10;

{$warn 5023 off : no warning about unused units}
interface

uses
  UniUpdateSQLEditor, UniScriptEditor, UniStoredProcEditor, UniTableEditor, 
  UniTableSQLFrame, UniSQLEditor, UniQueryOptionsFrame, UniQueryEditor, 
  UniSPCallFrame, UniParamsFrame, UniSQLOptionsFrame, UniConnectionEditor, 
  UniSpecificOptionsFrame, UniSpecificOptionsEditor, UniMacrosFrame, 
  UniDataTypeMapFrame, UniDesignUtils, UniReg, UniDesign, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UniReg', @UniReg.Register);
  RegisterUnit('UniDesign', @UniDesign.Register);
end;

initialization
  RegisterPackage('dclunidac10', @Register);
end.
