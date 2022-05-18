{$I Dac.inc}

unit DacReg;

interface

procedure Register;
procedure RegisterCRBatchMove;
{$IFNDEF FPC}
{$IFDEF VER9P}
procedure RegisterSplashScreen(ProductName: string; ProductVersion: string; HProductIco: LongWord; IsTrial: boolean; LicensedType: string);
procedure RegisterAboutBox(ProductName: string; ProductVersion: string; ProductPage:string; AboutDescription: string; HProductIco: LongWord;
                           IsTrial: boolean; LicensedType: string; Edition: string);
procedure UnregisterAboutBox;
{$ENDIF}
{$ENDIF}

implementation

{$IFNDEF FPC}
  {$IFDEF VER9}
    {$R DADesign9.res}
  {$ELSE}
    {$R DADesign.res}
  {$ENDIF}
  {$IFDEF VER10P}
    {$R DADesign10p.res}
  {$ENDIF}
{$ENDIF}

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, CRBatchMove, VirtualTable, VirtualDataSet{$IFNDEF FPC}{$IFDEF VER9P}, ToolsAPI, SysUtils{$ENDIF}{$ENDIF};

var
  CRBatchMoveRegistered: boolean;
{$IFNDEF FPC}
{$IFDEF VER9P}
  AboutBoxServices : IOTAAboutBoxServices = nil;
  AboutBoxIndex : Integer = 0;
{$ENDIF}
{$ENDIF}

procedure Register;
begin
  RegisterClass(TVirtualAutoIncField);
  RegisterComponents('Data Access', [TVirtualTable]);
  RegisterComponents('Data Access', [TVirtualDataSet]);
end;

procedure RegisterCRBatchMove;
begin
  if not CRBatchMoveRegistered then begin
    RegisterComponents('Data Access', [TCRBatchMove]);
    CRBatchMoveRegistered := True;
  end;
end;

{$IFNDEF FPC}
{$IFDEF VER9P}
procedure RegisterSplashScreen(ProductName: string; ProductVersion: string; HProductIco: LongWord; IsTrial: boolean; LicensedType: string);
begin
  SplashScreenServices.AddPluginBitmap(ProductName + ' ' + ProductVersion, HProductIco, IsTrial, LicensedType);
end;

procedure RegisterAboutBox(ProductName: string; ProductVersion: string; ProductPage:string; AboutDescription: string; HProductIco: LongWord;
                           IsTrial: boolean; LicensedType: string; Edition: string);
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(ProductName + ' ' + ProductVersion, AboutDescription,
  HProductIco, IsTrial, LicensedType, Edition);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFDEF FPC}
  {$I DADesign.lrs}
{$ENDIF}
  CRBatchMoveRegistered := False;

{$IFNDEF FPC}
{$IFDEF VER9P}
finalization
  UnregisterAboutBox;
{$ENDIF}
{$ENDIF}

end.
