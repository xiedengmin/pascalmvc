{$IFNDEF CLR}

{$I UniDac.inc}

unit UniConnectionEditor;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DacVcl, UniDacVcl, Buttons, Grids,
{$IFNDEF FPC}
  Mask, ValEdit,
{$ENDIF}
{$IFDEF FPC}
  LResources, LCLType,
{$ENDIF}
   CRTypes, DBAccess, Uni,
   CRFrame, CREditor, DAConnectionEditor, DADataTypeMapFrame,
   UniMacrosFrame, UniSpecificOptionsFrame;

type
  TUniConnectionEditorForm = class(TDAConnectionEditorForm)
    lbProvider: TLabel;
    edProvider: TComboBox;
    lbPort: TLabel;
    lbDatabase: TLabel;
    edDatabase: TComboBox;
    shOptions: TTabSheet;
    shMacros: TTabSheet;
    edPort: TEdit;
    procedure edProviderChange(Sender: TObject);
    procedure edDatabaseExit(Sender: TObject);
    procedure edPortExit(Sender: TObject);
    procedure edPortChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure edDatabaseDropDown(Sender: TObject);
    procedure edDatabaseKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure edServerSelect(Sender: TObject);
  protected
    FMacrosFrame: TUniMacrosFrame;
    FOptionsFrame: TUniSpecificOptionsFrame;

    function GetConnection: TUniConnection;
    function GetLocalConnection: TUniConnection;
    function GetDataTypeMapFrameClass: TDADataTypeMapFrameClass; override;

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure DoSaveConnection; override;
    procedure FillInfo(InfoConnection: TCustomDAConnection); override;
    procedure InitSpecificOption(ProviderName: string);
    procedure AssignProvider(const Value: string);
    procedure AssignDataBase(const Value: string);
    procedure AssignPort(const Value: Integer);
  {$IFDEF MSWINDOWS}
    procedure AddServerToList; override;
  {$ENDIF}

    function GetFrameByInitProp: TCRFrame; override;

    procedure ConnToControls; override;
    procedure ControlsToConn; override;
    procedure ShowState(Yellow: boolean = False); override;
    procedure SetupForSpecificOptions;

  {$IFNDEF CLR}
    procedure ReplaceEdit(var Edit: TWinControl);
    procedure SpinEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  {$ENDIF}

  public
    property Connection: TUniConnection read GetConnection;
    property LocalConnection: TUniConnection read GetLocalConnection;
  end;

var
  UniConnectionEditorForm: TUniConnectionEditorForm;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R UniConnectionEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  TypInfo, CRFunctions, UniProvider, UniDataTypeMapFrame;

{$I UniDacVer.inc}

{ TUniSessionEditorForm }

function TUniConnectionEditorForm.GetConnection: TUniConnection;
begin
  Result := FConnection as TUniConnection;
end;

function TUniConnectionEditorForm.GetLocalConnection: TUniConnection;
begin
  Result := FLocalConnection as TUniConnection;
end;

function TUniConnectionEditorForm.GetDataTypeMapFrameClass: TDADataTypeMapFrameClass;
begin
  Result := TUniDataTypeMapFrame;
end;

procedure TUniConnectionEditorForm.DoInit;
{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
var
  WinControl: TWinControl;
{$ENDIF}
{$ENDIF}
begin
  UniProviders.GetProviderNames(edProvider.Items);
{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
  WinControl := edPort;
  ReplaceEdit(WinControl);
{$ENDIF}
{$ENDIF}

  FMacrosFrame := AddTab(TUniMacrosFrame, shMacros) as TUniMacrosFrame;
  FOptionsFrame := AddTab(TUniSpecificOptionsFrame, shOptions) as TUniSpecificOptionsFrame;

  inherited;

{$IFDEF BETA}
  lbEdition.Caption := 'Beta';
  lbEdition.Font.Color := clRed;
{$ELSE}
{$IFDEF RC}
  lbEdition.Caption := 'Release Candidate';
  lbEdition.Font.Color := clGreen;
{$ELSE}
{$IFDEF EXPRESS}
  lbEdition.Caption := 'Express Edition';
{$ELSE}
{$IFDEF STD}
  lbEdition.Caption := 'Standard Edition';
{$ELSE}
  lbEdition.Caption := 'Professional Edition';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

  if (Connection <> nil) and TUniUtils.CanGetProvider(Connection) then
    shMacros.TabVisible := TUniUtils.GetProvider(Connection).IsMacrosSupported;

  FOptionsFrame.InitOptions(LocalConnection.SpecificOptions);
  InitSpecificOption(LocalConnection.ProviderName);

  lbVersion.Caption := UniDACVersion +' ';
  UpdateVersionPosition;
end;

procedure TUniConnectionEditorForm.DoActivate;
var
  Index: integer;
begin
  inherited;

  if PageControl.ActivePage = shConnect then begin
    Index := edProvider.Items.IndexOf(LocalConnection.ProviderName);
    if Index < 0 then
      ActiveControl := edProvider
    else
    if edUsername.Enabled then
      ActiveControl := edUsername
    else
    if edPassword.Enabled then
      ActiveControl := edPassword
    else
    if edServer.Enabled then
      ActiveControl := edServer
    else
    if edDatabase.Enabled then
      ActiveControl := edDatabase;
  end;
end;

procedure TUniConnectionEditorForm.DoSaveConnection;
begin
  inherited;

  Connection.ProviderName := LocalConnection.ProviderName;
  Connection.Database := LocalConnection.Database;
  Connection.Port := LocalConnection.Port;

  if FMacrosFrame.Modified then begin
    FMacrosFrame.Finish;
    Connection.Macros.Assign(LocalConnection.Macros);
  end;

  if FOptionsFrame.Modified then begin
    FOptionsFrame.Finish;
    Connection.SpecificOptions.Assign(LocalConnection.SpecificOptions);
  end;
end;

function TUniConnectionEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else if InitialProperty = 'Macros' then
    Result := FMacrosFrame
  else
    Result := inherited GetFrameByInitProp;
end;

procedure TUniConnectionEditorForm.ConnToControls;
begin
  edProvider.Text := LocalConnection.ProviderName;
  edPort.Text := IntToStr(LocalConnection.Port);
  edDatabase.Text := LocalConnection.Database;

  inherited;
end;

procedure TUniConnectionEditorForm.ControlsToConn;
begin
  AssignDataBase(edDatabase.Text); // OnExit event is not generated on Kylix when dialog is closing
  // all other parameters are set in controls OnChange event handlers
end;

procedure TUniConnectionEditorForm.ShowState(Yellow: boolean = False);
begin
  inherited;

end;

procedure TUniConnectionEditorForm.SetupForSpecificOptions;
begin
  if TUniUtils.CanGetProvider(LocalConnection) then begin
    edUsername.Enabled :=  TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).UsernameEnabled;
    edPassword.Enabled :=  TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).PasswordEnabled;
    edServer.Enabled :=  TUniUtils.GetProvider(LocalConnection).IsServerSupported and
      TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).ServerEnabled;
    edDatabase.Enabled := TUniUtils.GetProvider(LocalConnection).IsDatabaseSupported and
      TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).DatabaseEnabled;
    edPort.Enabled := TUniUtils.GetProvider(LocalConnection).IsPortSupported and
      TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).PortEnabled;
  end
  else begin
    edUsername.Enabled := True;
    edPassword.Enabled := True;
    edServer.Enabled := True;
    edPort.Enabled := False;
    edDatabase.Enabled := False;
  end;

  lbUsername.Enabled := edUsername.Enabled;
  lbPassword.Enabled := edPassword.Enabled;
  lbServer.Enabled := edServer.Enabled;
  lbDatabase.Enabled := edDatabase.Enabled;
  lbPort.Enabled := edPort.Enabled;

  if edUsername.Enabled then
    edUsername.Text := LocalConnection.Username
  else
    edUsername.Text := '';

  if edPassword.Enabled then
    edPassword.Text := LocalConnection.Password
  else
    edPassword.Text := '';

  if edServer.Enabled then
    edServer.Text := LocalConnection.Server
  else
    edServer.Text := '';

  if edDatabase.Enabled then
    edDatabase.Text := LocalConnection.Database
  else
    edDatabase.Text := '';

  if edPort.Enabled then
    edPort.Text := IntToStr(LocalConnection.Port)
  else
    edPort.Text := '';
end;

procedure TUniConnectionEditorForm.FillInfo(InfoConnection: TCustomDAConnection);
var
  UniInfoConnection: TUniConnection;
begin
  UniInfoConnection := TUniConnection(InfoConnection);

  if not UniInfoConnection.Connected then
    meInfo.Lines.Clear
  else begin
    meInfo.Lines.BeginUpdate;
    try
      meInfo.Lines.Text :=
        'Server version: ' + UniInfoConnection.ServerVersionFull + #13#10#13#10 +
        'Client version: ' + UniInfoConnection.ClientVersion + #13#10;
    finally
      meInfo.Lines.EndUpdate;
    end;
  end;
end;

procedure TUniConnectionEditorForm.InitSpecificOption(ProviderName: string);
begin
  SetupForSpecificOptions;
  FOptionsFrame.LoadOptions(LocalConnection.ProviderName, otConnection);
end;

procedure TUniConnectionEditorForm.AssignProvider(const Value: string);
begin
  if LocalConnection.ProviderName <> edProvider.Text then begin
    PerformDisconnect;
    LocalConnection.ProviderName := Trim(edProvider.Text);
    InitSpecificOption(LocalConnection.ProviderName);
    Modified := True;
  end;
end;

procedure TUniConnectionEditorForm.AssignDataBase(const Value: string);
begin
  if LocalConnection.Database <> Value then begin
    PerformDisconnect;
    LocalConnection.Database := Value;
    Modified := True;
  end;
end;

procedure TUniConnectionEditorForm.AssignPort(const Value: Integer);
begin
  if LocalConnection.Port <> Value then begin
    PerformDisconnect;
    LocalConnection.Port := Value;
    edPort.Text := IntToStr(Value);
    Modified := True;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TUniConnectionEditorForm.AddServerToList;
begin
  inherited;

  if TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog)).UseDatabaseHistory then
    TUniConnectDialogUtils.SaveDatabaseListToRegistry(TUniConnectDialog(FConnectDialog));
end;
{$ENDIF}

procedure TUniConnectionEditorForm.edProviderChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    AssignProvider(edProvider.Text);
  finally
    ShowState;
  end;
end;

procedure TUniConnectionEditorForm.edDatabaseExit(Sender: TObject);
begin
  try
    AssignDataBase(edDatabase.Text);
  finally
    ShowState;
  end;
end;

procedure TUniConnectionEditorForm.edDatabaseDropDown(Sender: TObject);
var
  DialogService: TConnectDialogService;
  OldConnected: boolean;
  OldDatabase: string;
  List: TStringList;
begin
{$IFDEF UNIX}
  edDatabase.OnGetItems := nil;
  try
{$ENDIF}
  if not TUniUtils.CanGetProvider(LocalConnection) then
    exit;

  StartWait;
  try
    DialogService := TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(FConnectDialog));
    if DialogService.UseDatabaseHistory then
    {$IFDEF MSWINDOWS}
      TUniConnectDialogUtils.LoadDatabaseListFromRegistry(TUniConnectDialog(FConnectDialog), edDatabase.Items)
    {$ENDIF}
    else begin
      TDBAccessUtils.SetLockLoginPrompt(LocalConnection, True);
      try
        OldConnected := LocalConnection.Connected;
        OldDatabase := LocalConnection.Database;
        if not OldConnected then
          LocalConnection.Database := DialogService.GetDefaultDatabase;
        edDatabase.Items.Clear; // for case when GetDatabaseNames raises an exception
        List := TStringList.Create;
        try
        {$IFDEF DARWIN}
          try
        {$ENDIF}
            LocalConnection.GetDatabaseNames(List);
        {$IFDEF DARWIN}
          except
            on E:Exception do
              ShowMessage(E.Message);
          end;
        {$ENDIF}
          AssignStrings(List, edDatabase.Items);
        finally
          LocalConnection.Connected := OldConnected;
          if not OldConnected then
            LocalConnection.Database := OldDatabase;
          List.Free;
        end;
      finally
        TDBAccessUtils.SetLockLoginPrompt(LocalConnection, False);
      end;

      if edDatabase.Items.Count < 20 then
        edDatabase.DropDownCount := edDatabase.Items.Count
      else
        edDatabase.DropDownCount := 20;
    end;
  finally
    StopWait;
  end;
{$IFDEF UNIX}
  finally
    edDatabase.OnGetItems := edDatabaseDropDown;
  end;
{$ENDIF}
end;

procedure TUniConnectionEditorForm.edDatabaseKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  // Connection.Database is not assigned in OnChange event.
  // We need assign it before default button ('Connect') will act.
  if Key = VK_RETURN then
    edDatabaseExit(Sender);
end;

procedure TUniConnectionEditorForm.edPortExit(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    try
      LocalConnection.Port := StrToInt(edPort.Text);
    except
      PageControl.ActivePage := shConnect;
      edPort.SetFocus;
      edPort.SelectAll;
      raise;
    end;
  finally
    ShowState;
  end;
end;

procedure TUniConnectionEditorForm.edPortChange(Sender: TObject);
begin
  if FInDoInit or (edPort.Text = '') then
    Exit;

  try
    try
      AssignPort(StrToInt(edPort.Text));
    except
      PageControl.ActivePage := shConnect;
      edPort.SetFocus;
      edPort.SelectAll;
      raise;
    end;
  finally
    ShowState;
  end;
end;

{$IFNDEF CLR}
procedure TUniConnectionEditorForm.ReplaceEdit(var Edit: TWinControl);
type
  TSetProc = procedure (Self: TObject; Ptr: pointer);
var
  EditClass: string;
  NewEdit: TWinControl;
  OldName: string;
  TypeInfo: PTypeInfo;
begin
  if GetClass('TSpinEdit') <> nil then
    EditClass := 'TSpinEdit'
  else
{$IFDEF BCB}
  if GetClass('TCSpinEdit') <> nil then
    EditClass := 'TCSpinEdit'
  else
{$ENDIF}
    EditClass := '';

  if EditClass <> '' then begin
    NewEdit := TCustomControl(GetClass(EditClass).NewInstance);
    NewEdit.Create(Edit.Owner);

    with NewEdit do begin
      Parent := Edit.Parent;
      Left := Edit.Left;
      Top := Edit.Top;
      Width := Edit.Width;
      Height := Edit.Height;
      Align := Edit.Align;
      TabOrder := Edit.TabOrder;
      Anchors := Edit.Anchors;
      //Constraints := Edit.Constraints;
      TypeInfo := GetClass(EditClass).ClassInfo;
      HelpContext := Edit.HelpContext;

      if Edit is TEdit then begin
        SetReadOnly(NewEdit, TEdit(Edit).ReadOnly);
        SetOrdProp(NewEdit, 'Color', Longint(TEdit(Edit).Color));
      end;

      OnKeyDown := SpinEditKeyDown;
      if GetPropInfo(Edit.ClassInfo, 'OnChange') <> nil then
        SetMethodProp(NewEdit, GetPropInfo(TypeInfo, 'OnChange'),
          GetMethodProp(Edit, GetPropInfo(Edit.ClassInfo, 'OnChange')));
      SetMethodProp(NewEdit, GetPropInfo(TypeInfo, 'OnExit'),
        GetMethodProp(Edit, GetPropInfo(Edit.ClassInfo, 'OnExit')));
      SetMethodProp(NewEdit, GetPropInfo(TypeInfo, 'OnKeyDown'),
        GetMethodProp(Edit, GetPropInfo(Edit.ClassInfo, 'OnKeyDown')));
      SetMethodProp(NewEdit, GetPropInfo(TypeInfo, 'OnKeyPress'),
        GetMethodProp(Edit, GetPropInfo(Edit.ClassInfo, 'OnKeyPress')));
    end;

    if (Edit.Owner <> nil) and (TForm(Edit.Owner).ActiveControl = Edit) then
      TForm(Edit.Owner).ActiveControl := NewEdit;

    OldName := Edit.Name;
    Edit.Free;
    Edit := TEdit(NewEdit);
    NewEdit.Name := OldName;

    if (EditClass = 'TSpinEdit') {$IFDEF BCB} or (EditClass = 'TCSpinEdit' ){$ENDIF} then begin
      SetOrdProp(NewEdit, 'MaxValue', 65535);
      SetOrdProp(NewEdit, 'MinValue', 0);
    end;
  end;
end;

procedure TUniConnectionEditorForm.SpinEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
    btConnectClick(self);
end;
{$ENDIF}

procedure TUniConnectionEditorForm.PageControlChange(Sender: TObject);
begin
  inherited;

  if PageControl.ActivePage = shConnect then begin
    SetupForSpecificOptions;
    ShowState;
  end;
end;

procedure TUniConnectionEditorForm.FormShow(Sender: TObject);
begin
  inherited;
  TDBAccessUtils.SetLockLoginPrompt(LocalConnection, True);
end;

procedure TUniConnectionEditorForm.edServerSelect(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if assigned(edServer.Items) and assigned(edServer.Items.Objects[edServer.ItemIndex]) then
  begin
    AssignPort(Integer(edServer.Items.Objects[edServer.ItemIndex]));
  end;
{$ENDIF}
  edServerChange(Sender);
end;

end.
