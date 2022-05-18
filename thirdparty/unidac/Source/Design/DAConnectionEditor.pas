
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  ConnectionEditor Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}

unit DAConnectionEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Registry, DacVcl, Buttons,
  {$IFNDEF FPC}Mask,{$ENDIF}
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
{$IFDEF FPC}
  LResources, LCLType,
{$ENDIF}
  CRTypes, DBAccess,
  CRFrame, CREditor, CRTabEditor, CRDesignUtils,
  DAEditor, DADataTypeMapFrame, DADesignUtils;

type
  TDAConnectionEditorForm = class(TCRTabEditorForm)
    shConnect: TTabSheet;
    shDataTypeMapping: TTabSheet;
    Panel: TPanel;
    lbUsername: TLabel;
    lbPassword: TLabel;
    lbServer: TLabel;
    edUsername: TEdit;
  {$IFNDEF FPC}
    edPassword: TMaskEdit;
  {$ELSE}
    edPassword: TEdit;
  {$ENDIF}
    edServer: TComboBox;
    btConnect: TButton;
    btDisconnect: TButton;
    shInfo: TTabSheet;
    shAbout: TTabSheet;
    meInfo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    lbVersion: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbWeb: TLabel;
    lbMail: TLabel;
    lbIDE: TLabel;
    cbLoginPrompt: TCheckBox;
    shRed: TShape;
    shYellow: TShape;
    shGreen: TShape;
    imPeng: TImage;
    lbEdition: TLabel;
    imgLogo: TImage;

    procedure btDisconnectClick(Sender: TObject);
    procedure lbWebClick(Sender: TObject);
    procedure lbMailClick(Sender: TObject);
    procedure cbLoginPromptClick(Sender: TObject);
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure shAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbMailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure edUsernameChange(Sender: TObject);
    procedure edPasswordChange(Sender: TObject);
    procedure edServerChange(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure edServerDropDown(Sender: TObject); virtual;
    procedure edServerKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edServerExit(Sender: TObject);

  protected
    FConnection, FLocalConnection: TCustomDAConnection;
    FInDoInit: boolean;
    FConnectDialog: TCustomConnectDialog;
    FDataMappingFrameFrame: TDADataTypeMapFrame;
    FIsConnected: boolean;
  {$IFDEF MSWINDOWS}
    FRegistry: TRegistry;
  {$ENDIF}
  {$IFDEF DBTOOLS}
    FInExistingChange: boolean;

    function GetExistingConnectionComboBox: TComboBox; virtual;
    procedure ChooseExistingConnection;
    function GetConnectionCondition: string; virtual;
  {$ENDIF}

    procedure GetServerList(List: TStrings); virtual;
  {$IFDEF MSWINDOWS}
    procedure AddServerToList; virtual;
  {$ENDIF}

    procedure ShowState(Yellow: boolean = False); virtual;
    procedure ConnToControls; virtual;
    procedure ControlsToConn; virtual;
    procedure FillInfo(InfoConnection: TCustomDAConnection); virtual;
    procedure PerformConnect; virtual;
    procedure PerformDisconnect; virtual;
    function GetIsConnected: boolean; virtual;
    function GetInfoConnection: TCustomDAConnection; virtual;
    procedure CreateLocalConnection; virtual;
    procedure AssignUsername(const Value: string); virtual;
    procedure AssignPassword(const Value: string); virtual;
    procedure AssignServer(const Value: string); virtual;
    procedure AssignLoginPrompt(Value: boolean); virtual;
    function GetConnectDialogClass: TConnectDialogClass; virtual;
    function GetDataTypeMapFrameClass: TDADataTypeMapFrameClass; virtual;

    procedure Resize; override;

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure DoSave; override;
    procedure DoSaveConnection; virtual;
    procedure DoFinish; override;
    procedure UpdateVersionPosition; virtual;

    function GetFrameByInitProp: TCRFrame; override;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetLocalComponent: TComponent; override;

    procedure DoPageControlChange(Sender: TObject); override;
  public
    constructor Create(Owner: TComponent; CRDesignUtilsClass: TCRDesignUtilsClass); override;
    destructor Destroy; override;

    property IsConnected: boolean read GetIsConnected;
    property Connection: TCustomDAConnection read FConnection;
    property LocalConnection: TCustomDAConnection read FLocalConnection;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}ShellAPI,{$ENDIF}
  HelpUtils,
  {$IFDEF VER6P}Variants, {$ENDIF}
  CRFunctions, MemData;

{$IFNDEF FPC}
{$IFDEF CLR}
{$R DAConnectionEditor.dfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{$I DacVer.inc}

constructor TDAConnectionEditorForm.Create(Owner: TComponent; CRDesignUtilsClass: TCRDesignUtilsClass);
begin
  inherited;

{$IFDEF MSWINDOWS}
  FRegistry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  if not FRegistry.OpenKey('\SOFTWARE\Devart\' + FCRDesignUtilsClass.GetProjectName + '\Connect', True) then
    FreeAndNil(FRegistry);
{$ENDIF}

  FIsConnected := False;
end;

procedure TDAConnectionEditorForm.CreateLocalConnection;
begin
  FLocalConnection := TComponentClass(FConnection.ClassType).Create(nil) as TCustomDAConnection;
  TDBAccessUtils.SetDesigning(FLocalConnection, csDesigning in FConnection.ComponentState);
  FLocalConnection.Assign(FConnection);
  FIsConnected := FConnection.Connected;
end;

destructor TDAConnectionEditorForm.Destroy;
begin
{$IFDEF MSWINDOWS}
  FreeAndNil(FRegistry);
{$ENDIF}

  FConnectDialog.Free;

  inherited;
end;

procedure TDAConnectionEditorForm.Resize;
var
  logoMargin: Integer;
begin
  inherited;

  if imgLogo <> nil then begin
    logoMargin := (shConnect.ClientWidth - Panel.Width - Panel.Left - imgLogo.Width) div 2;
    imgLogo.Left := shConnect.ClientWidth - imgLogo.Width - logoMargin;
    imgLogo.Top := (shConnect.ClientHeight - imgLogo.Height) div 2;

    if meInfo <> nil then begin
      meInfo.Width := Panel.Width;
      meInfo.Height := shInfo.ClientHeight - meInfo.Top * 2;
    end;
  end;

  if Panel <> nil then begin
    btConnect.Top := shConnect.ClientHeight - btConnect.Height - 8;
    btDisconnect.Top := btConnect.Top;

    cbLoginPrompt.Top := btConnect.Top + (btConnect.Height - cbLoginPrompt.Height) div 2;

    shGreen.Top := btConnect.Top + 5;
    shYellow.Top := shGreen.Top;
    shRed.Top := shGreen.Top;

    btDisconnect.Left := Panel.Left + Panel.Width - btDisconnect.Width;
    btConnect.Left := btDisconnect.Left - btConnect.Width - 6;

    Panel.Height := btConnect.Top - 8 - Panel.Top;
  end;
end;

procedure TDAConnectionEditorForm.DoInit;
{$I IdeConsts.inc}
var
  i: integer;
begin
  FInDoInit := True;

  CreateLocalConnection;

//  FCRDesignUtilsClass.SetDesignCreate(FLocalConnection, True);

  FDataMappingFrameFrame := AddTab(GetDataTypeMapFrameClass, shDataTypeMapping) as TDADataTypeMapFrame;

  i := PageControl.Left + PageControl.Width - btCancel.Left - btCancel.Width;
  btCancel.Left := btCancel.Left + i;
  btOk.Left := btOk.Left + i;

  try
    inherited;

    lbVersion.Caption := DACVersion + ' ';
    lbIDE.Caption := 'for ' + IDEInfos[IDEVer].Name;
    UpdateVersionPosition;

  {$IFDEF BETA}
    lbEdition.Caption := 'Beta';
    lbEdition.Font.Color := clRed;
  {$ELSE}
  {$IFDEF RC}
    lbEdition.Caption := 'Release Candidate';
    lbEdition.Font.Color := clGreen;
  {$ELSE}
  {$IFDEF STD}
    lbEdition.Caption := 'Standard Edition';
  {$ELSE}
    lbEdition.Caption := 'Professional Edition';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF DBTOOLS}
    if DADesignUtilsClass.DBToolsAvailable then begin
      GetDBToolsService(DADesignUtilsClass).GetConnections(GetExistingConnectionComboBox.Items, GetConnectionCondition);
      ChooseExistingConnection;
    end;  
  {$ENDIF}

    FConnectDialog := GetConnectDialogClass.Create(nil);
    TDBAccessUtils.SetConnection(FConnectDialog, FLocalConnection);
    TDBAccessUtils.SetUseServerHistory(FConnectDialog, False);

    ConnToControls;

    ShowState;
  finally
    FInDoInit := False;
  end;
end;

procedure TDAConnectionEditorForm.DoActivate;
begin
  inherited;
end;

procedure TDAConnectionEditorForm.DoSave;
var
  OldConnected: boolean;
  OldDebug: boolean;
begin
  OldConnected := IsConnected;
  OldDebug := FConnection.Debug;

  try
    DoSaveConnection;

    FConnection.Debug := False;
    try
      TDBAccessUtils.SetLockLoginPrompt(FConnection, True);
      try
        FConnection.Connected := OldConnected;
      finally
        TDBAccessUtils.SetLockLoginPrompt(FConnection, False);
      end;
    except
    end;
  finally
    FConnection.Debug  := OldDebug;
  end;
end;

procedure TDAConnectionEditorForm.DoSaveConnection;
begin
  FConnection.Username := FLocalConnection.Username;
  FConnection.Password := FLocalConnection.Password;
  FConnection.Server := FLocalConnection.Server;
  FConnection.LoginPrompt := FLocalConnection.LoginPrompt;

  if FDataMappingFrameFrame.Activated and FDataMappingFrameFrame.Modified then begin
    FDataMappingFrameFrame.Finish;
    FConnection.DataTypeMap.Assign(FLocalConnection.DataTypeMap);
  end;
end;

procedure TDAConnectionEditorForm.DoFinish;
begin
  FLocalConnection.Free;
  FLocalConnection := nil;

  inherited;
end;

procedure TDAConnectionEditorForm.UpdateVersionPosition;
begin
  lbIDE.Left := lbVersion.Left + lbVersion.Width;
end;

function TDAConnectionEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'DataTypeMap' then
    Result := FDataMappingFrameFrame
  else
    Result := inherited GetFrameByInitProp;
end;

function TDAConnectionEditorForm.GetComponent: TComponent;
begin
  Result := FConnection;
end;

function TDAConnectionEditorForm.GetLocalComponent: TComponent;
begin
  Result := FLocalConnection;
end;

procedure TDAConnectionEditorForm.DoPageControlChange(Sender: TObject);
begin
  inherited;

  if TPageControl(Sender).ActivePage = shConnect then
    imgLogo.Parent := shConnect
  else if TPageControl(Sender).ActivePage = shInfo then begin
    imgLogo.Parent := shInfo;
    FillInfo(GetInfoConnection);
  end
  else if TPageControl(Sender).ActivePage = shAbout then
    imgLogo.Parent := shAbout;
end;

procedure TDAConnectionEditorForm.SetComponent(Value: TComponent);
begin
  FConnection := Value as TCustomDAConnection;
end;

{$IFDEF DBTOOLS}
function TDAConnectionEditorForm.GetExistingConnectionComboBox: TComboBox;
begin
  Result := nil;
  Assert(False, 'Must be overriden');
end;

procedure TDAConnectionEditorForm.ChooseExistingConnection;
begin
  if not FInExistingChange and DADesignUtilsClass.DBToolsAvailable then
    with GetExistingConnectionComboBox do
      ItemIndex := Items.IndexOf(GetDBToolsService(DADesignUtilsClass).FindConnectionName(FLocalConnection));
end;

function TDAConnectionEditorForm.GetConnectionCondition: string;
begin
  Result := '';
end;
{$ENDIF}

procedure TDAConnectionEditorForm.ConnToControls;
begin
  edUsername.Text := FLocalConnection.Username;
  edPassword.Text := FLocalConnection.Password;
  edServer.Text := FLocalConnection.Server;
  cbLoginPrompt.Checked := FLocalConnection.LoginPrompt;
end;

procedure TDAConnectionEditorForm.ControlsToConn;
begin
  // all parameters are set in controls OnChange event handlers
end;

procedure TDAConnectionEditorForm.ShowState(Yellow: boolean);
begin
  btDisconnect.Enabled := IsConnected;

  shRed.Brush.Color := clBtnFace;
  shYellow.Brush.Color := clBtnFace;
  shGreen.Brush.Color := clBtnFace;

  if Yellow then begin
    shYellow.Brush.Color := clYellow;
    shYellow.Update;
  end
  else
    if IsConnected then begin
      shGreen.Brush.Color := clGreen;
      shYellow.Update;
    end
    else
      shRed.Brush.Color := clRed;
end;

procedure TDAConnectionEditorForm.lbWebClick(Sender: TObject);
begin
  OpenUrl('http://' + lbWeb.Caption);
  lbWeb.Font.Color := $FF0000;
end;

procedure TDAConnectionEditorForm.lbMailClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  MailTo(lbMail.Caption);
  lbMail.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.cbLoginPromptClick(Sender: TObject);
begin
  AssignLoginPrompt(cbLoginPrompt.Checked);
end;

procedure TDAConnectionEditorForm.lbWebMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TDAConnectionEditorForm.lbMailMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbMail.Font.Color := $4080FF;
end;

procedure TDAConnectionEditorForm.shAboutMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
  lbMail.Font.Color := $FF0000;
end;

procedure TDAConnectionEditorForm.edUsernameChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    AssignUsername(edUsername.Text);
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edPasswordChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    AssignPassword(edPassword.Text);
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edServerChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    AssignServer(edServer.Text);
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.btConnectClick(Sender: TObject);
begin
  ShowState(True);
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
  StartWait;
  try
    ControlsToConn;
    PerformConnect;
  {$IFDEF MSWINDOWS}
    if IsConnected then
      AddServerToList;
  {$ENDIF}
  finally
    FIsConnected := False;
    StopWait;
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.btDisconnectClick(Sender: TObject);
begin
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
  try
    PerformDisconnect;
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edServerDropDown(Sender: TObject);
var
  List: TStringList;
begin
{$IFDEF UNIX}
  (Sender as TComboBox).OnGetItems := nil;
  try
{$ENDIF}
  StartWait;
  List := TStringList.Create;
  try
    GetServerList(List);
    AssignStrings(List, edServer.Items);
  finally
    StopWait;
    List.Free;
  end;
{$IFDEF UNIX}
  finally
    (Sender as TComboBox).OnGetItems := edServerDropDown;
  end;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.GetServerList(List: TStrings);
begin
  FConnectDialog.GetServerList(List);
end;

{$IFDEF MSWINDOWS}
procedure TDAConnectionEditorForm.AddServerToList;
begin
  try
    TDBAccessUtils.SaveServerListToRegistry(FConnectDialog);
  finally
    SetCursor(crDefault);
  end;
end;
{$ENDIF}

procedure TDAConnectionEditorForm.FillInfo(InfoConnection: TCustomDAConnection);
var
  OldLoginPrompt: boolean;
begin
  OldLoginPrompt := FLocalConnection.LoginPrompt;
  try
    FLocalConnection.LoginPrompt := False;

    if not IsConnected then
      try
        ShowState(True);
        PerformConnect;
      except
        on E: Exception do
        begin
          // PageControl.ActivePage := shConnect;
          // Application.ShowException(E); - silent exception. Please see CR MyDAC 3443
        end;
      end;
    meInfo.Lines.Clear;
  finally
    FLocalConnection.LoginPrompt := OldLoginPrompt;
    ShowState(False);
  end;
end;

procedure TDAConnectionEditorForm.PerformConnect;
begin
  FLocalConnection.PerformConnect;

  if not FInDoInit then
    Modified := True;
end;

procedure TDAConnectionEditorForm.PerformDisconnect;
begin
  FIsConnected := False;
  FLocalConnection.Disconnect;

  if not FInDoInit then
    Modified := True;
end;

function TDAConnectionEditorForm.GetIsConnected: boolean;
begin
  Result := FIsConnected or FLocalConnection.Connected;
end;

function TDAConnectionEditorForm.GetInfoConnection: TCustomDAConnection;
begin
  if GetIsConnected then
    if LocalConnection.Connected then
      Result := LocalConnection
    else if Connection.Connected then
      Result := Connection
    else
      Result := LocalConnection
  else
    Result := LocalConnection;
end;

procedure TDAConnectionEditorForm.AssignUsername(const Value: string);
begin
  if LocalConnection.Username <> Value then begin
    PerformDisconnect;
    LocalConnection.Username := Value;
    Modified := True;
  end;
end;

procedure TDAConnectionEditorForm.AssignPassword(const Value: string);
begin
  if LocalConnection.Password <> Value then begin
    PerformDisconnect;
    LocalConnection.Password := Value;
    Modified := True;
  end;
end;

procedure TDAConnectionEditorForm.AssignServer(const Value: string);
begin
  if LocalConnection.Server <> Value then begin
    PerformDisconnect;
    LocalConnection.Server := Value;
    Modified := True;
  end;
end;

procedure TDAConnectionEditorForm.AssignLoginPrompt(Value: boolean);
begin
  if LocalConnection.LoginPrompt <> Value then begin
    LocalConnection.LoginPrompt := Value;
    Modified := True;
  end;
end;

function TDAConnectionEditorForm.GetConnectDialogClass: TConnectDialogClass;
begin
  Assert(FConnection <> nil);
  Result := TDBAccessUtils.ConnectDialogClass(FConnection);
end;

function TDAConnectionEditorForm.GetDataTypeMapFrameClass: TDADataTypeMapFrameClass;
begin
  Result := TDADataTypeMapFrame;
end;

procedure TDAConnectionEditorForm.edServerKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    edServerChange(Sender);
end;

procedure TDAConnectionEditorForm.edServerExit(Sender: TObject);
begin
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
end;

end.
