
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright @ 1998-2021 Devart. All right reserved.
//  DAMenu
//////////////////////////////////////////////////

{$IFNDEF DAMENU_HEADER}

{$I Dac.inc}

unit DAMenu;
{$ENDIF}
interface

{$IFDEF VER7P}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

uses
  ToolsAPI, Menus,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Graphics, Classes, SysUtils;

type
  TDAMenu = class;

  TAPIMenuItem = TMenuItem;
  TDAMenuClickSender = TObject;
  TDAMenuClickEvent = procedure (Sender: TDAMenuClickSender) of object;

  TDAMenuItem = class(TCollectionItem)
  protected
    FSubMenu: TDAMenu;
    FCaption: string;
    FName: string;
    FVisible: boolean;
    FRadioItem: boolean;
    FChecked: boolean;
    FNeedDestroyMenuItem: boolean;
    FMenuItem: TAPIMenuItem;

    procedure SetCaption(Value: string);
    procedure SetVisible(Value: boolean);
    procedure SetRadioItem(Value: boolean);
    procedure SetChecked(Value: boolean);
  public
    constructor Create(DAMenu: TDAMenu; MenuItem: TAPIMenuItem; NeedDestroyItem: boolean = False); reintroduce; overload;
    class function CreateMenuItem(DAMenu: TDAMenu; Caption, Name: string; ClickEvent: TDAMenuClickEvent = nil; Index: integer = -1): TDAMenuItem;
    destructor Destroy; override;

    property SubMenu: TDAMenu read FSubMenu;
    property Caption: string read FCaption write SetCaption;
    property Name: string read FName;
    property Visible: boolean read FVisible write SetVisible;
    property RadioItem: boolean read FRadioItem write SetRadioItem;
    property Checked: boolean read FChecked write SetChecked;
  end;

  TDAMenu = class(TCollection)
  protected
    FParentItem: TDAMenuItem;
    FHInstance: HINST;
    FMenuItems: TAPIMenuItem;
    FProjectName: string;
    FSubMenuProcessed: boolean;
    FFAQName: string;
    FWizardPosition: integer;
    FUseCHM: boolean;
  {$IFNDEF LITE}
    procedure HelpItemClick(Sender: TDAMenuClickSender);
    procedure FAQItemClick(Sender: TDAMenuClickSender);
  {$ENDIF}
  public
    constructor Create(
      ParentItem: TDAMenuItem;
      const AProcessSubMenu: boolean = False);
    destructor Destroy; override;

    function AddSeparator: TDAMenuItem;
  {$IFNDEF LITE}
    function AddFAQ(Caption, Name, ProjectName {'Mydac', 'Oda', 'Sdac'}: string): TDAMenuItem;
    function AddHelp(Caption, Name, ProjectName {'Mydac', 'Oda', 'Sdac'}: string; UseCHM: boolean = True): TDAMenuItem;
  {$IFNDEF VER9P}
    procedure AddWizard(Caption, Name: string; ClickEvent: TDAMenuClickEvent);
  {$ENDIF}
  {$ENDIF}
    procedure AddWizards;
    function Add(Caption, Name: string; ClickEvent: TDAMenuClickEvent = nil; Index: integer = -1): TDAMenuItem;
    procedure ProcessSubMenu;
    procedure GetSubMenu;
  end;

  TDAProductMenu = class(TDAMenu)
  protected
    FCRMenuName: string;
    FServerMenuCaption: string;
    FProductMenuCaption: string;
    FAboutCaption: string;
    FAboutName: string;
    FProductName: string;
    FProductEdition: string;
    FProductVersion: string;
    FAboutClickEvent: TDAMenuClickEvent;

    FCRMenu: TDAMenuItem;
    FHasProduct: boolean;

    procedure CheckUpdatesItemClick(Sender: TDAMenuClickSender);

    procedure Prepare;
    function GetSubMenu: TDAMenu;
  {$IFDEF VER9P}
    property SubMenu: TDAMenu read GetSubMenu;
  {$ENDIF}
  public
    constructor Create(const ACRMenuName, AAboutCaption, AAboutName: string;
      const AServerMenuCaption: string; const AProductMenuCaption: string = ''); overload;
    constructor Create; overload;
    function AddItems(Instance: HINST): boolean; virtual;
    function AddAbout: TDAMenuItem;
    function AddCheckUpdates(ProductName, ProductVersion, ProductEdition: string): TDAMenuItem;

    property CRMenuName: string read FCRMenuName;
    property ServerMenuCaption: string read FServerMenuCaption;
    property ProductMenuCaption: string read FProductMenuCaption;
  {$IFNDEF VER9P}
    property SubMenu: TDAMenu read GetSubMenu;
  {$ENDIF}
  end;

function RemoveAmpersands(RawCaption: string): string;

implementation

uses
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  DacVcl,
{$ENDIF}
{$ENDIF}
{$IFNDEF LITE}
  ShellApi,
{$ENDIF}
{$IFNDEF VER6P}
  ActiveX, comobj,
{$ENDIF}
{$IFNDEF LITE}
  HelpUtils,
{$ENDIF}
  Forms, Dialogs, Controls, CRTypes, CRFunctions, Math, CRVersionChecker;

{ TDAMenuItem }

constructor TDAMenuItem.Create(DAMenu: TDAMenu; MenuItem: TAPIMenuItem; NeedDestroyItem: boolean = False);
begin
  inherited Create(DAMenu);

  FMenuItem := MenuItem;
  FNeedDestroyMenuItem := NeedDestroyItem;

  if FMenuItem <> nil then begin
    FName := FMenuItem.Name;
    FVisible := FMenuItem.Visible;
    FChecked := FMenuItem.Checked;
    FCaption := FMenuItem.Caption;
    FRadioItem := FMenuItem.RadioItem;
  end;

  FSubMenu := TDAMenu.Create(Self);
end;

class function TDAMenuItem.CreateMenuItem(DAMenu: TDAMenu; Caption, Name: string; ClickEvent: TDAMenuClickEvent = nil; Index: integer = -1): TDAMenuItem;
var
  Bmp: Graphics.TBitmap;
  MenuItem: TAPIMenuItem;
begin
  if Index = -1 then
    Index := DAMenu.Count;
  MenuItem := TAPIMenuItem.Create(nil);
  MenuItem.Caption := Caption;
  MenuItem.Name := Name;
  MenuItem.OnClick := ClickEvent;
  if (DAMenu <> nil) and (DAMenu.FHInstance <> 0) then begin
    Bmp := Graphics.TBitmap.Create;
    try
      try
        Bmp.LoadFromResourceName(DAMenu.FHInstance, UpperCase(Name));
      except
        Bmp.Free;
        Bmp := nil;
      end;
      if Bmp <> nil then
        MenuItem.ImageIndex := (BorlandIDEServices as INTAServices).AddMasked(Bmp, Bmp.TransparentColor);
    finally
      Bmp.Free;
    end;
  end;
  DAMenu.FMenuItems.Insert(Index, MenuItem);

  Result := TDAMenuItem.Create(DAMenu, MenuItem, True);
end;

destructor TDAMenuItem.Destroy;
begin
  FSubMenu.Free;

  if Assigned(FMenuItem) then begin
    if FNeedDestroyMenuItem then
      FMenuItem.Free
  end;

  inherited Destroy;
end;

procedure TDAMenuItem.SetCaption(Value: string);
begin
  FMenuItem.Caption := Value;
end;

procedure TDAMenuItem.SetVisible(Value: boolean);
begin
  FMenuItem.Visible := Value;
end;

procedure TDAMenuItem.SetRadioItem(Value: boolean);
begin
  FMenuItem.RadioItem := Value;
end;

procedure TDAMenuItem.SetChecked(Value: boolean);
begin
  FMenuItem.Checked := Value;
end;

{ TDAMenu }

constructor TDAMenu.Create(
      ParentItem: TDAMenuItem;
      const AProcessSubMenu: boolean = False);

  procedure MainMenu;
  var
    MainMenu: TMenu;
  begin
    MainMenu := (BorlandIDEServices as INTAServices).GetMainMenu;
    if MainMenu <> nil then
      FMenuItems := MainMenu.Items
    else
      FMenuItems := nil;
  end;

begin
  inherited Create(TDAMenuItem);

  FWizardPosition := -1;

  if ParentItem = nil then
    MainMenu;

  if ParentItem <> nil then
    FMenuItems := ParentItem.FMenuItem;

  if (FMenuItems <> nil) and AProcessSubMenu
  (*{$IFDEF VER9P}and (ParentItem <> nil){$ENDIF}*)  // bug with IOTAMenuItem.Count under CLR (DbxOda)
  then
    ProcessSubMenu;
end;

destructor TDAMenu.Destroy;
begin
  Clear;
  inherited;
end;

{$IFNDEF LITE}
procedure TDAMenu.HelpItemClick(Sender: TDAMenuClickSender);
var
{$IFDEF VER8P}
{$IFNDEF VER22P}
  Prefix,
{$ENDIF}
{$ENDIF}
  HelpFile, JumpID: string;
begin
{$IFDEF VER22P}
  HelpFile := GetHelpFileName(FProjectName, FUseCHM);
  JumpID := 'Overview_' + FProjectName;
{$ELSE}
{$IFDEF VER8P}
{$IFDEF VER10}
  Prefix := 'borland.bds4';
{$ENDIF}
{$IFDEF VER11}
  Prefix := 'borland.bds5';
{$ENDIF}
{$IFDEF VER12}
  Prefix := 'embarcadero.rs2009';
{$ENDIF}
{$IFDEF VER14}
  Prefix := 'embarcadero.rs2010';
{$ENDIF}
{$IFDEF VER15}
  Prefix := 'embarcadero.rs_xe';
{$ENDIF}
{$IFDEF VER16}
  Prefix := 'embarcadero.rs_xe2';
{$ENDIF}
{$IFDEF VER17}
  Prefix := 'embarcadero.rs_xe3';
{$ENDIF}
{$IFDEF VER18}
  Prefix := 'embarcadero.rs_xe4';
{$ENDIF}
{$IFDEF VER19}
  Prefix := 'embarcadero.rs_xe5';
{$ENDIF}
{$IFDEF VER20}
  Prefix := 'embarcadero.rs_xe6';
{$ENDIF}
{$IFDEF VER21}
  Prefix := 'embarcadero.rs_xe7';
{$ENDIF}
  HelpFile := 'ms-help://' + Prefix + '/Devart.' + FProjectName;
  JumpID := Format('ms-help://%s/Devart.%s/%s/overview.htm', [Prefix, FProjectName, FProjectName]);
{$ELSE}
  HelpFile := GetHelpFileName(FProjectName, FUseCHM);
  JumpID := 'Overview_' + FProjectName;
{$ENDIF}
{$ENDIF}
  ShowHelp(HelpFile, JumpID);
end;
{$ENDIF}

function TDAMenu.AddSeparator: TDAMenuItem;
var
  guid: TGuid;
  s, s1: string;
  i, Cnt: integer;
begin
  Result := nil;
  if FMenuItems <> nil then begin
    s := '-';
    Cnt := FMenuItems.Count;

    if FWizardPosition <> -1 then
      Cnt := FWizardPosition - 1;

    if Cnt > 0 then
      s := FMenuItems.Items[Cnt - 1].Caption;

    if s = '-' then
      Exit;
  end;

{$IFDEF VER6P}
  if CreateGuid(guid) <> 0 then
{$ELSE}
  if CoCreateGuid(guid) <> 0 then
{$ENDIF}
    RaiseLastOSError;

  s := GUIDToString(guid);
  s1 := 'Separator';
  for i := 1 to Length(s) do
    case s[i] of
      '0'..'9', 'a'..'z', 'A'..'Z':
         s1 := s1 + s[i];
    end;

  Result := Add('-', s1);
end;

{$IFNDEF LITE}
function TDAMenu.AddFAQ(Caption, Name, ProjectName {'Mydac', 'Oda', 'Sdac'}: string): TDAMenuItem;
var
  i: integer;
begin
  Result := nil;
{$IFDEF VER222P}
  FFAQName := ExtractFilePath(GetHelpFileName(ProjectName, True));
{$ELSE}
{$IFDEF VER8P}
  FFAQName := ExtractFilePath(GetHelpFileName(ProjectName));
{$ELSE}
  FFAQName := ExtractFilePath(GetHelpFileName(ProjectName, True));
{$ENDIF}
{$ENDIF}
  i := Length(FFAQName) - 1;
  while (i > 1) and (FFAQName[i] <> '/') and (FFAQName[i] <> '\') do
    Dec(i);

  if i < 3 then
    Exit;

  FFAQName := Copy(FFAQName, 1, i) + 'FAQ.html';
  if FileExists(FFAQName) then
    Result := Add(Caption, Name, FAQItemClick);
end;

procedure TDAMenu.FAQItemClick(Sender: TDAMenuClickSender);
begin
  ShellExecute(0, 'open', PChar(FFAQName), '', '', SW_SHOW);
end;

function TDAMenu.AddHelp(Caption, Name, ProjectName {'Mydac', 'Odac', 'Sdac'}: string; UseCHM: boolean = True): TDAMenuItem;
var
  HelpFileName: string;
begin
  FProjectName := ProjectName;
  FUseCHM := UseCHM;
  HelpFileName := GetHelpFileName(ProjectName, UseCHM);
  if (ExtractFilePath(HelpFileName) = '') or (FileExists(HelpFileName)) then
    Result := Add(Caption, Name, HelpItemClick)
  else
    Result := nil;
end;

{$IFNDEF VER9P}
procedure TDAMenu.AddWizard(Caption, Name: string; ClickEvent: TDAMenuClickEvent);
begin
  TDAMenuItem.CreateMenuItem(Self, Caption, Name, ClickEvent, -1);  
  if FWizardPosition = -1 then
    FWizardPosition := 0
end;
{$ENDIF}

{$ENDIF}

function TDAMenu.Add(Caption, Name: string; ClickEvent: TDAMenuClickEvent = nil; Index: integer = -1): TDAMenuItem;
begin
  if (Index = -1) and (FWizardPosition <> -1) then begin
    Index := FWizardPosition;
    Inc(FWizardPosition);
  end;
  Result := TDAMenuItem.CreateMenuItem(Self, Caption, Name, ClickEvent, Index);
end;

procedure TDAMenu.ProcessSubMenu;
var
  Skip: boolean;
  i, j: integer;
  MenuItem: TAPIMenuItem;
begin
  if FMenuItems <> nil then begin
    if not FSubMenuProcessed then
      Clear
    else
    if FMenuItems.{$IFDEF MSWINDOWS}Count{$ELSE}ChildCount{$ENDIF} = Count then
      Exit;

    for i := 0 to FMenuItems.Count - 1 do begin
      MenuItem := FMenuItems.Items[i];
      Skip := False;
      if FSubMenuProcessed then
        for j := 0 to Count - 1 do
          if TDAMenuItem(Items[j]).Name = MenuItem.Name then begin
            Skip := True;
            Break;
          end;
      if Skip then
        Continue;     
      TDAMenuItem.Create(Self, MenuItem);
      FWizardPosition := 0;
    end;
    FSubMenuProcessed := True;
  end;
end;

procedure TDAMenu.GetSubMenu;
var
  i: integer;
  MenuItem: TAPIMenuItem;
begin
  if (FMenuItems <> nil) and not FSubMenuProcessed then begin
    Clear;

    for i := 0 to FMenuItems.Count - 1 do begin
      MenuItem := FMenuItems.Items[i];
      TDAMenuItem.Create(Self, MenuItem);
    end;

    FSubMenuProcessed := True;
  end;
end;

procedure TDAMenu.AddWizards;
begin
  FWizardPosition := -1;
end;

{ TDAProductMenu }

constructor TDAProductMenu.Create(const ACRMenuName, AAboutCaption,
  AAboutName: string; const AServerMenuCaption: string;
  const AProductMenuCaption: string = '');
begin
  inherited Create(nil, True);

  FCRMenuName := ACRMenuName;
  FAboutCaption := AAboutCaption;
  FAboutName := AAboutName;
  FServerMenuCaption := AServerMenuCaption;
  FProductMenuCaption := AProductMenuCaption;
{$IFNDEF VER9P}
  Prepare;
{$ENDIF}
end;

constructor TDAProductMenu.Create;
begin
  inherited Create(nil, True);
end;

procedure TDAProductMenu.Prepare;
var
  ThirdParty: TDAMenuItem;
  i: integer;
  UsedCaption: string;
begin
  ThirdParty := nil;
  for i := 0 to Count - 1 do begin
    if TDAMenuItem(Items[i]).Name = FCRMenuName then begin
      FCRMenu := TDAMenuItem(Items[i]);
      SubMenu.ProcessSubMenu;
      if SubMenu.Count > 0 then
        SubMenu.AddSeparator;
      Break;
    end
    else
    if (FProductMenuCaption <> '') and (UpperCase(RemoveAmpersands(TDAMenuItem(Items[i]).Caption)) =
        UpperCase(RemoveAmpersands(FServerMenuCaption))) then
      ThirdParty := TDAMenuItem(Items[i]);
  end;

  FHasProduct := False;

  if ThirdParty = nil then
    UsedCaption := FServerMenuCaption
  else
    UsedCaption := FProductMenuCaption;
  if FCRMenu = nil then
    FCRMenu := Add(UsedCaption, FCRMenuName, nil, Count - 1{$IFDEF VER9P} - 1{$ENDIF})
  else begin
    FCRMenu.Caption := UsedCaption;
  {$IFDEF VER9P}
    for i := 0 to SubMenu.Count - 1 do
      if RemoveAmpersands(TDAMenuItem(SubMenu.Items[i]).Caption) =
        RemoveAmpersands(FAboutCaption) then begin
        FHasProduct := True;
        Break;
      end;
  {$ENDIF}
  end;
end;

procedure TDAProductMenu.CheckUpdatesItemClick(Sender: TDAMenuClickSender);
const
  CannotCheckUpdates = 'Updates checking is unavailable.';
  NoUpdates = 'You are using the latest version of %s.';
  UpdatesAvailable = 'The new %s version %s is available. Do you want to download it?';
var
  LatestVersionStr, UpdatesUrl: string;
  CurrentVersion, LatestVersion: Integer;
begin
  try
    LatestVersionStr := TVersionChecker.GetLatestVersion(FProductName, FProductEdition, FProductVersion);
    CurrentVersion := TVersionChecker.VersionToInt(FProductVersion);
    LatestVersion := TVersionChecker.VersionToInt(LatestVersionStr);
    if LatestVersion > CurrentVersion then begin
      if MessageDlg(Format(UpdatesAvailable, [FProductName, LatestVersionStr]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
        UpdatesUrl := 'https://secure.devart.com';
        OpenUrl(UpdatesUrl);
      end;
    end
    else
      MessageDlg(Format(NoUpdates, [FProductName]), mtInformation, [mbOK], 0);
  except
    MessageDlg(CannotCheckUpdates, mtInformation, [mbOK], 0);
  end;
end;

function TDAProductMenu.AddItems(Instance: HINST): boolean;
begin
  FHInstance := Instance;
  Result := False;
  ProcessSubMenu;
{$IFDEF VER9P}
  Prepare;
{$ENDIF}
  if not FHasProduct then begin
    FHInstance := Instance;
    SubMenu.FHInstance := Instance;
    SubMenu.AddSeparator;
    Result := True;
    FHasProduct := True;
  end
{$IFDEF VER9P}
  else
    AddAbout.Visible := False
{$ENDIF}
end;

function TDAProductMenu.GetSubMenu: TDAMenu;
begin
  Result := FCRMenu.SubMenu;
end;

function TDAProductMenu.AddAbout: TDAMenuItem;
begin
  Result := SubMenu.Add(FAboutCaption, FAboutName, FAboutClickEvent);
end;

function TDAProductMenu.AddCheckUpdates(ProductName, ProductVersion, ProductEdition: string): TDAMenuItem;
begin
  FProductName := ProductName;
  FProductVersion := ProductVersion;
  FProductEdition := ProductEdition;
  Result := SubMenu.Add('Check Updates...', FProductName + 'CheckUpdatesItem', CheckUpdatesItemClick);
end;

function RemoveAmpersands(RawCaption: string): string;
var
  i: integer;
begin
  Result := '';
  i := 1;
  while i <= Length(RawCaption) do begin
    if RawCaption[i] = '&' then begin
      Inc(i);
      if i > Length(RawCaption) then
        Break;
    end;
    Result := Result + RawCaption[i];
    Inc(i);
  end;
end;

end.
