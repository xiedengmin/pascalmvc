unit CRIOHandlerReg;

{$I SB.inc}

interface

uses
  Classes, CRSSHIOHandler, CRSSLIOHandler, CRSsoStorage;

procedure Register;

implementation

{$R CRIOHandlerDesign.res}
(*
{$IFNDEF CLR}
  {$IFDEF VER9}
    {$R CRIOHandlerDesign9.res}
  {$ELSE}
    {$R CRIOHandlerDesign.res}
  {$ENDIF}
  {$IFDEF VER10P}
    {$R CRIOHandlerDesign10p.res}
  {$ENDIF}
{$ELSE}
  {$R CRIOHandlerDesign.res}
  {$IFDEF VER10P}
    {$R ..\Images\TCRSSHIOHandler.bmp}
    {$R ..\Images\TCRSSHIOHandler16.bmp}
    {$R ..\Images\TCRSSHIOHandler32.bmp}
    {$R ..\Images\TCRSSLIOHandler.bmp}
    {$R ..\Images\TCRSSLIOHandler16.bmp}
    {$R ..\Images\TCRSSLIOHandler32.bmp}
  {$ELSE}
    {$R ..\Images\TCRSSHIOHandler16.bmp}
    {$R ..\Images\TCRSSLIOHandler16.bmp}
  {$ENDIF}
{$ENDIF}
*)

procedure Register;
begin
  RegisterComponents('Data Access', [TCRSSHIOHandler]);
  RegisterComponents('Data Access', [TCRSSLIOHandler]);
  RegisterComponents('Data Access', [TCRSsoFileStorage]);
  RegisterComponents('Data Access', [TCRSsoRegStorage]);
end;

end.

