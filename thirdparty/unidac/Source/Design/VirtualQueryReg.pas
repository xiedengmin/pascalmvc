
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit VirtualQueryReg;

interface

uses
  VirtualQuery;

procedure Register;

implementation

{$IFNDEF FPC}
  {$IFDEF VER9}
    {$R VirtualDesign9.res}
  {$ELSE}
    {$R VirtualDesign.res}
  {$ENDIF}
  {$IFDEF VER10P}
    {$R VirtualDesign10p.res}
  {$ENDIF}
{$ENDIF}

uses
  Classes,
{$IFNDEF FPC}
{$IFDEF VER9P}
  Windows,
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  LResources
{$ELSE}
  DacReg
{$ENDIF}
  ;

{$I VirtualDacVer.inc}

procedure Register;
begin
{$IFNDEF FPC}
{$IFNDEF UNIDACPRO}
{$IFDEF VER9P}
  RegisterSplashScreen('Devart Virtual Data Access Components',
                       VirtualDacVersion,
                       LoadBitmap(HInstance, {$IFDEF VER9}'SPLASHGR'{$ENDIF}
                                             {$IFDEF VER10}'SPLASHBL'{$ENDIF}
                                             {$IFDEF VER11}'SPLASHWH'{$ENDIF}
                                             {$IFDEF VER12}'SPLASHWH'{$ENDIF}
                                             {$IFDEF VER14P}'SPLASHBL'{$ENDIF}),
                       false, 'Licensed'
                      );

  RegisterAboutBox('Devart Virtual Data Access Components',
                   VirtualDacVersion,
                   'http://www.devart.com/virtualdac/',
                   'Devart Virtual Data Access Components' + #13#10 +
                   'Copyright 1998 - 2021 Devart. All rights reserved.' + #13#10 +
                   'Web: www.devart.com/virtualdac/' + #13#10 +
                   'Support: www.devart.com/virtualdac/support.html',
                   LoadBitmap(HInstance, 'ABOUT'),
                   false, 'Licensed'
                   , {$IFDEF PRO}'Standard edition'{$ELSE}'Express edition'{$ENDIF});
{$ENDIF}
{$ENDIF}
{$ENDIF}

  RegisterComponents('Data Access', [TVirtualQuery]);
end;

{$IFDEF FPC}
initialization
  {$I VirtualDesign.lrs}
{$ENDIF}

end.
