{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dcldac10;

{$warn 5023 off : no warning about unused units}
interface

uses
  HelpUtils, DacReg, VTDesign, DADesign, VTDataEditor, DAConnectionEditor, 
  DATableEditor, DAStoredProcEditor, DAScriptEditor, DADumpEditor, 
  DADumpProgress, DADualListEditor, DAMacrosFrame, CRColFrame, 
  DASQLComponentEditor, DASPCallFrame, DAUpdateSQLFrame, DAUpdateSQLEditor, 
  DASQLGeneratorFrame, DAQueryEditor, DADataEditor, CRDataEditor, 
  DAParamsFrame, DASQLFrame, CRTabEditor, CRFrame, CREditor, CRDesign, 
  CRDesignUtils, DAEditor, DAParamValueEditor, DATableSQLFrame, DADesignUtils, 
  CRValueEdit, CRFldLinks, CRCtrls, DADataTypeMapFrame, DAConditionsFrame, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DacReg', @DacReg.Register);
  RegisterUnit('VTDesign', @VTDesign.Register);
  RegisterUnit('DADesign', @DADesign.Register);
  RegisterUnit('CRDesign', @CRDesign.Register);
end;

initialization
  RegisterPackage('dcldac10', @Register);
end.
