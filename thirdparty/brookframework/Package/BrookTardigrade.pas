{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit BrookTardigrade;

{$warn 5023 off : no warning about unused units}
interface

uses
  BrookExtra, BrookHandledClasses, BrookHTTPAuthentication, BrookHTTPCookies, 
  BrookHTTPRequest, BrookHTTPResponse, BrookHTTPServer, BrookHTTPUploads, 
  BrookLibraryLoader, BrookLogger, BrookMathExpression, BrookMediaTypes, 
  BrookReader, BrookString, BrookStringMap, BrookURLEntryPoints, 
  BrookURLRouter, BrookUtility, libsagui, Marshalling, Platform, 
  BrookIDEIntegration, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BrookIDEIntegration', @BrookIDEIntegration.Register);
end;

initialization
  RegisterPackage('BrookTardigrade', @Register);
end.
