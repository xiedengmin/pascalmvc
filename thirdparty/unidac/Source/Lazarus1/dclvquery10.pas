{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dclvquery10;

interface

uses
  CRDesign, VirtualQueryReg, VirtualQueryDesign, VirtualQueryEditor, VirtualQueryDesignUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CRDesign', @CRDesign.Register);
  RegisterUnit('VirtualQueryDesign', @VirtualQueryDesign.Register);
  RegisterUnit('VirtualQueryReg', @VirtualQueryReg.Register);
end;

initialization
  RegisterPackage('dclvquery10', @Register);
end.
