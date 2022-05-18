
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit VirtualMenu;

interface

{$IFNDEF UNIDACPRO}
uses
  DAMenu, Windows;

type
  TVirtualMenu = class (TDAProductMenu)
  private
    procedure HomePageItemClick(Sender: TDAMenuClickSender);
    procedure VirtualDacPageItemClick(Sender: TDAMenuClickSender);
    procedure AboutItemClick(Sender: TDAMenuClickSender);
//    procedure DBMonitorItemClick(Sender: TDAMenuClickSender);
//    procedure DBMonitorPageItemClick(Sender: TDAMenuClickSender);
  public
    constructor Create;
    function AddItems(Instance: HINST): boolean; override;
  end;

var
  Menu: TVirtualMenu;
{$ENDIF}

implementation

{$IFNDEF UNIDACPRO}
uses
  SysUtils, Forms, ShellApi, VirtualAbout,// DBMonitorClient,
  HelpUtils;

{$I VirtualDacVer.inc}

resourcestring
  sCRMenuName = 'DevartMenuVirtualDac';
  sVirtualDACMenu = '&VirtualDAC';
  sHelpItemCaption = 'VirtualDAC Help';
  sHelpItemName = 'CRVirtualDacHelpItem';
  sHomePageCaption = 'Devart Home Page';
  sHomePageName = 'CRVirtualDacHomePageItem';
  sFAQCaption = 'VirtualDAC FAQ';
  sFAQName = 'CRVirtualDacFAQItem';
  sVirtualDacPageCaption = 'VirtualDAC Home Page';
  sVirtualDacPageName = 'CRVirtualDacPageItem';
  sAboutItemCaption = 'About VirtualDAC...';
  sAboutItemName = 'CRVirtualDacAboutItemWin32';
//  sDBMonitorItemCaption = 'DBMonitor';
//  sDBMonitorItemName = 'LiteDacDBMonitorItem';
//  sDBMonitorPageCaption = 'Download DBMonitor';
//  sDBMonitorPageName = 'LiteDacDBMonitorPageItem';

{ TVirtualMenu }

constructor TVirtualMenu.Create;
begin
  inherited Create(sCRMenuName, sAboutItemCaption, sAboutItemName,
    sVirtualDACMenu);
  FAboutClickEvent := AboutItemClick;
end;

function TVirtualMenu.AddItems(Instance: HINST): boolean;
var
  Edition: string;
begin
  Result := inherited AddItems(Instance);

  if not Result then
    Exit;

  with SubMenu do begin
//    if HasMonitor then
//      Add(sDBMonitorItemCaption, sDBMonitorItemName, DBMonitorItemClick);

    AddWizards;
    AddSeparator;

    AddHelp(sHelpItemCaption, sHelpItemName, 'VirtualDac', True);
    AddFAQ(sFAQCaption, sFAQName, 'VirtualDac');
    AddSeparator;

    Add(sHomePageCaption, sHomePageName, HomePageItemClick);
    Add(sVirtualDacPageCaption, sVirtualDacPageName, VirtualDacPageItemClick);
//    if not HasMonitor then
//      Add(sDBMonitorPageCaption, sDBMonitorPageName, DBMonitorPageItemClick);
    AddSeparator;
  {$IFDEF Pro}
    Edition := 'Standard';
  {$ELSE}
    Edition := 'Express';
  {$ENDIF}
    AddCheckUpdates('VirtualDAC', VirtualDACVersion, Edition);
    AddAbout;
  end;
end;

procedure TVirtualMenu.HomePageItemClick(Sender: TDAMenuClickSender);
begin
  OpenUrl('http://www.devart.com');
end;

procedure TVirtualMenu.VirtualDacPageItemClick(Sender: TDAMenuClickSender);
begin
  OpenUrl('http://www.devart.com/virtualdac');
end;

procedure TVirtualMenu.AboutItemClick(Sender: TDAMenuClickSender);
begin
  ShowAbout;
end;

//procedure TVirtualMenu.DBMonitorItemClick(Sender: TDAMenuClickSender);
//begin
//  ShellExecute(0, 'open',
//  {$IFDEF CLR}
//    WhereMonitor,
//  {$ELSE}
//    PChar(WhereMonitor),
//  {$ENDIF}
//    '', '', SW_SHOW);
//end;
//
//procedure TVirtualMenu.DBMonitorPageItemClick(Sender: TDAMenuClickSender);
//begin
//  OpenUrl('http://www.devart.com/dbmonitor/dbmon3.exe');
//end;

initialization
  Menu := TVirtualMenu.Create;

finalization
  Menu.Free;
{$ENDIF}

end.
