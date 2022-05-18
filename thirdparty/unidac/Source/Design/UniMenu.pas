
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  UniDAC IDE Menu
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}

unit UniMenu;
{$ENDIF}
interface

uses
  DAMenu, Windows;

type
  TUniMenu = class (TDAProductMenu)
  private
    procedure HomePageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure UniDacPageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure AboutItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});    
    procedure DBMonitorItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure DBMonitorPageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
  public
    constructor Create;
    function AddItems(Instance: HINST): boolean; override;    
  end;

var
  Menu: TUniMenu;

implementation

uses
{$IFDEF CLR}
  WinUtils,
{$ENDIF}
  SysUtils, Forms, ShellApi, UniAbout, DBMonitorClient,
  HelpUtils;

{$I UniDacVer.inc}

resourcestring
  sCRMenuName = 'DevartMenuUniDac';
  sUniDacMenu = '&UniDAC';
  sHelpItemCaption = 'UniDAC Help';
  sHelpItemName = 'CRUniDacHelpItem';
  sHomePageCaption = 'Devart Home Page';
  sHomePageName = 'CRUniDacHomePageItem';
  sFAQCaption = 'UniDAC FAQ';
  sFAQName = 'CRUniDacFAQItem';
  sUniDacPageCaption = 'UniDAC Home Page';
  sUniDacPageName = 'CRUniDacPageItem';
  sAboutItemCaption = 'About UniDAC...';
{$IFDEF CLR}
  sAboutItemName = 'CRUniDacAboutItemCLR';
{$ELSE}
  sAboutItemName = 'CRUniDacAboutItemWin32';
{$ENDIF}
  sDBMonitorItemCaption = 'DBMonitor';
  sDBMonitorItemName = 'UniDacDBMonitorItem';
  sDBMonitorPageCaption = 'Download DBMonitor';
  sDBMonitorPageName = 'UniDacDBMonitorPageItem';

{ TUniMenu }

constructor TUniMenu.Create;
begin
  inherited Create(sCRMenuName, sAboutItemCaption, sAboutItemName,
    sUniDacMenu);
  FAboutClickEvent := AboutItemClick;
end;

function TUniMenu.AddItems(Instance: HINST): boolean;
var
  Edition: string;
begin
  Result := inherited AddItems(Instance);
  if not Result then
    Exit;

  with SubMenu do begin
    if HasMonitor then
      Add(sDBMonitorItemCaption, sDBMonitorItemName, DBMonitorItemClick);

    AddWizards;
    AddSeparator;

    AddHelp(sHelpItemCaption, sHelpItemName, 'UniDac', True);
    AddFAQ(sFAQCaption, sFAQName, 'UniDac');
    AddSeparator;

    Add(sHomePageCaption, sHomePageName, HomePageItemClick);
    Add(sUniDacPageCaption, sUniDacPageName, UniDacPageItemClick);
    if not HasMonitor then
      Add(sDBMonitorPageCaption, sDBMonitorPageName, DBMonitorPageItemClick);
    AddSeparator;

  {$IFDEF EXPRESS}
    Edition := 'Express';
  {$ELSE}
  {$IFDEF STD}
    Edition := 'Standard';
  {$ELSE}
    Edition := 'Professional';
  {$ENDIF}
  {$ENDIF}

    AddCheckUpdates('UniDAC', UniDACVersion, Edition);
    AddAbout;
  end;
end;

procedure TUniMenu.HomePageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  OpenUrl('http://www.devart.com');
end;

procedure TUniMenu.UniDacPageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  OpenUrl('http://www.devart.com/unidac');
end;

procedure TUniMenu.AboutItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  ShowAbout;
end;

procedure TUniMenu.DBMonitorItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  ShellExecute(0, 'open',
  {$IFDEF CLR}
    WhereMonitor,
  {$ELSE}
    PChar(WhereMonitor),
  {$ENDIF}
    '', '', SW_SHOW);
end;

procedure TUniMenu.DBMonitorPageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  OpenUrl('http://www.devart.com/dbmonitor/dbmon3.exe');
end;

initialization
  Menu := TUniMenu.Create;
finalization
  Menu.Free;
end.
