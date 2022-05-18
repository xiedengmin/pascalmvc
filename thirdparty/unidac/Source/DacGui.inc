
{$IFDEF FMX}
type
  TFormClass = class of TForm;
{$ENDIF}

procedure SetCursor(Value: Integer);
function GetScreenCursor: TCursor;
procedure SetScreenCursor(Value: TCursor);
procedure ShowDebugForm(DASQLMonitorClass: TDASQLMonitorClass;
  Component: TComponent; SQL: string; Params: TDAParams; Caption: string);
function ShowConnectForm(ConnectDialog: TCustomConnectDialog): boolean;
procedure StartWait;
procedure StopWait;
function ApplicationTitle: string;

{$IFDEF MSWINDOWS}
function GetHelpFileName(const ProjectName: string; UseCHM: boolean = False): string;
{$ENDIF}

implementation

uses
  CRTypes, CRFunctions;

{$IFDEF MSWINDOWS}
const
  KeyPath = '\SOFTWARE\Devart\DAC\';
{$ENDIF}

var
  SQLDelay: Cardinal = 30;
  SQLArrowCount: integer = 0;
{$IFDEF FMX}
  FInternalCursor: TCursor;
{$ENDIF}

{$IFDEF WIN32_64}
var
  TimerID: word = 0;
  StartTime: Cardinal = 0;

procedure FreeTimer(ForceKill: boolean = False);
begin
  if (TimerID <> 0) and (ForceKill or (GetTickCount - StartTime > SQLDelay)) then begin
    KillTimer(0, TimerID);
    TimerID := 0;
    StartTime := 0;
    SetCursor(crDefault);
  end;
end;

procedure TimerCallBack(hWnd: HWND; Message: Word; TimerID: word;
  SysTime: Integer); stdcall;
begin
  FreeTimer;
end;
{$ELSE}
type
  TCursorTimer = class (TTimer)
  private
    procedure TimerHandler(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Timer: TTimer;

{ TCursorTimer }

constructor TCursorTimer.Create(AOwner: TComponent);
begin
  inherited;

  OnTimer := TimerHandler;
end;

procedure TCursorTimer.TimerHandler(Sender: TObject);
begin
  Enabled := False;
  SetCursor(crDefault);
end;
{$ENDIF}

function GetScreenCursor: TCursor;
begin
{$IFNDEF FMX}
  Result := Screen.Cursor;
{$ELSE}
  Result := FInternalCursor;
{$ENDIF}
end;

procedure SetScreenCursor(Value: TCursor);
{$IFNDEF FMX}
begin
  Screen.Cursor := Value;
end;
{$ELSE}
{$IFDEF VER17P}
var
  CursorService: IFMXCursorService;
begin
  FInternalCursor := Value;
  TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, IInterface(CursorService));
  if CursorService <> nil then
    CursorService.SetCursor(Value);
end;
{$ELSE}
begin
  FInternalCursor := Value;
  Platform.SetCursor(nil, Value);
end;
{$ENDIF}
{$ENDIF}

procedure SetCursor(Value: integer);
begin
  if {$IFNDEF MOBILE}ChangeCursor and IsMainThread{$ELSE}False{$ENDIF} then
    case Value of
      crDefault: begin
        if SQLArrowCount > 0 then begin
          if GetScreenCursor <> crSQLWait then
            Dec(SQLArrowCount);
          if SQLArrowCount > 0 then
            SetScreenCursor(crSQLArrow)
          else
            SetScreenCursor(crDefault);
        end
        else
          SetScreenCursor(crDefault);
      end;
      crSQLWait: begin
      {$IFDEF WIN32_64}
        if TimerID = 0 then
          TimerID := SetTimer(0, 0, SQLDelay, @TimerCallBack);

        if GetScreenCursor <> crSQLWait then
          SetScreenCursor(crSQLWait);

        StartTime := GetTickCount;
      {$ELSE}
        if Timer = nil then begin
          Timer := TCursorTimer.Create(nil);
          Timer.Enabled := False;
        end;
        if not Timer.Enabled then begin
          Timer.Interval := SQLDelay;
          Timer.Enabled := True;
        end;

        if GetScreenCursor <> crSQLWait then
          SetScreenCursor(crSQLWait);
      {$ENDIF}
      end;
      crSQLArrow: begin
        Inc(SQLArrowCount);
        SetScreenCursor(crSQLArrow);
      end;
    end;
end;

type
  TDebugForm = class (TForm)
  public
    constructor CreateNew(Owner: TComponent; Dummy: {$IFDEF FMX}{$IFDEF VER19P}NativeInt{$ELSE}Integer{$ENDIF}{$ELSE}Integer{$ENDIF} = 0); override;

    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure ButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;{$IFDEF FMX} var KeyChar: Char;{$ENDIF} Shift: TShiftState);
  end;

{ TDebugForm }

constructor TDebugForm.CreateNew(Owner: TComponent; Dummy: {$IFDEF FMX}{$IFDEF VER19P}NativeInt{$ELSE}Integer{$ENDIF}{$ELSE}Integer{$ENDIF} = 0);
begin
  inherited CreateNew(Owner {$IFDEF FPC},Dummy{$ENDIF});

{$IFNDEF FMX}
  KeyPreview := True;
  OnKeyPress := FormKeyPress;
{$ENDIF}
end;

procedure TDebugForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) or (Key = #27) then
    Close;
end;

procedure TDebugForm.FormKeyDown(Sender: TObject; var Key: Word;{$IFDEF FMX} var KeyChar: Char;{$ENDIF} Shift: TShiftState);
begin
  if (Sender is TMemo) and (Key = 65) and (Shift = [ssCtrl]) then
    TMemo(Sender).SelectAll;
end;

procedure TDebugForm.ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure ShowDebugForm(DASQLMonitorClass: TDASQLMonitorClass;
  Component: TComponent; SQL: string; Params: TDAParams; Caption: string);
var
  Form: TDebugForm;
  Memo: TMemo;
  St: string;
  Panel: TPanel;
{$IFDEF MSWINDOWS}
  Registry: TRegistry;
{$ENDIF}
begin
  Form := TDebugForm.CreateNew(nil);

{$IFDEF MSWINDOWS}
  Registry := TRegistry.Create;
{$ENDIF}

  try
    St := string(DASQLMonitorClass.GetCaption) + ' Debug: ';
    if Component.Owner <> nil then
      St := St + Component.Owner.Name + '.';
    St := St + Component.Name;
    if Caption <> '' then
      St := St + '  [' + Caption + ']';

    Form.Caption := St;

    Panel := TPanel.Create(Form);
    with Panel do begin
      Parent := Form;
      Align := {$IFDEF FMX}TAlignLayout.{$ENDIF}alTop;
      Height := 24;
    {$IFNDEF FMX}
      BevelInner := bvNone;
      BevelOuter := bvNone;
    {$ENDIF}
    end;

    with TButton.Create(Form) do begin
      Parent := Panel;
      Caption := 'Close';
      Font.Style := [{$IFDEF FMX}TFontStyle.{$ENDIF}fsBold];
      {$IFNDEF FMX}Top{$ELSE}Position.Y{$ENDIF} := 1;
      {$IFNDEF FMX}Left{$ELSE}Position.X{$ENDIF} := 0;
      Height := 22;
      Width := 150;
      OnClick := Form.ButtonClick;
    end;

    with TLabel.Create(Form) do begin
      Parent := Panel;
      Caption := 'Before execution';
      Font.Style := [{$IFDEF FMX}TFontStyle.{$ENDIF}fsBold];
    {$IFNDEF FMX}
      Font.Color := clNavy;
    {$ENDIF}
      {$IFNDEF FMX}Top{$ELSE}Position.Y{$ENDIF} := 5;
      {$IFNDEF FMX}Left{$ELSE}Position.X{$ENDIF} := 175;
    end;

    with TPanel.Create(Form) do begin
      Parent := Form;
      Align := {$IFDEF FMX}TAlignLayout.{$ENDIF}alLeft;
      Width := 15;
    {$IFNDEF FMX}
      BevelInner := bvLowered;
      BevelOuter := bvNone;
    {$ENDIF}
    end;

    Memo := TMemo.Create(Form);
    with Memo do begin
      Parent := Form;
      Align := {$IFDEF FMX}TAlignLayout.{$ENDIF}alClient;
      ReadOnly := True;
      OnKeyDown := Form.FormKeyDown;
    {$IFNDEF FMX}
      Color := clBtnFace;
      Font.Name := 'Courier New';
    {$IFDEF MSWINDOWS}
    {$IFNDEF CLX}
      Font.Charset := GetDefFontCharSet;
    {$ENDIF}
    {$ENDIF}
      ScrollBars := ssBoth;
      WordWrap := False;
    {$ENDIF}
    end;

    Form.ActiveControl := Memo;

    Memo.Lines.Text := SQL + #13#10 + #13#10 + DASQLMonitorClass.GetParams(Params);
    Memo.SelStart := 0;

  {$IFDEF MSWINDOWS}
    if Registry.OpenKey(KeyPath + 'DebugWindow', False) then begin
      if Registry.ValueExists('Left') then
        Form.Left := Registry.ReadInteger('Left');
      if Registry.ValueExists('Top') then
        Form.Top := Registry.ReadInteger('Top');
      if Registry.ValueExists('Width') then
        Form.Width := Registry.ReadInteger('Width');
      if Registry.ValueExists('Height') then
        Form.Height := Registry.ReadInteger('Height');

      Registry.CloseKey;
    end
    else
  {$ENDIF}
    begin
    {$IFNDEF FMX}
      Form.Width := (Screen.Width) div 2;
      Form.Left := (Screen.Width - Form.Width) div 2;
      Form.Top := (Screen.Height - Form.Height) div 2;
    {$ENDIF}
    end;

    Form.ShowModal;

  {$IFDEF MSWINDOWS}
    if Registry.OpenKey(KeyPath + 'DebugWindow', True) then begin
      Registry.WriteInteger('Left', Form.Left);
      Registry.WriteInteger('Top', Form.Top);
      Registry.WriteInteger('Width', Form.Width);
      Registry.WriteInteger('Height', Form.Height);
      Registry.CloseKey;
    end;
  {$ENDIF}

  finally
  {$IFDEF MSWINDOWS}
    Registry.Free;
  {$ENDIF}
    Form.Free;
  end;
end;

type
  _TCustomConnectDialog = class(TCustomConnectDialog)
  end;

function ShowConnectForm(ConnectDialog: TCustomConnectDialog): boolean;
var
  FormClass: TFormClass;
  ConnectForm: TForm;
  PropInfo: PPropInfo;
begin
  if (ConnectDialog.DialogClass = '')
    or (csDesigning in ConnectDialog.ComponentState)
  then
    FormClass := TFormClass(_TCustomConnectDialog(ConnectDialog).DefDialogClass)
  else
    FormClass := TFormClass(FindClass(ConnectDialog.DialogClass));

  ConnectForm := FormClass.Create(nil);
  try
    PropInfo := GetPropInfo(FormClass.ClassInfo, 'ConnectDialog');
    if PropInfo <> nil then
      SetObjectProp(ConnectForm, PropInfo, ConnectDialog);

    Result := (ConnectForm.ShowModal = mrOk) and
      (not TDBAccessUtils.GetNeedConnect(ConnectDialog) or ConnectDialog.Connection.Connected);
  finally
    ConnectForm.Free;
  end;
end;

procedure StartWait;
begin
  SetCursor(crSQLWait);
end;

procedure StopWait;
begin
  SetCursor(crDefault);
end;

function ApplicationTitle: string;
begin
  Result := Application.Title;
end;

{$IFDEF MSWINDOWS}
{$IFDEF VER8P}
function GetHelpFileName(const ProjectName: string; UseCHM: boolean): string;
{$I IdeConsts.inc}
var
  Path: string;
  Ind: integer;

  Registry: TRegistry;
  sl: TStringList;
  i: integer;
  UCProjectName: string;
{$IFDEF VER9P}
  ValName: string;
{$ENDIF}
begin
  Result := '';
  Registry := nil;
  sl := nil;
  try
    Registry := TRegistry.Create;
  {$IFDEF VER9P}
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    Registry.Access := KEY_READ;
    if Registry.OpenKey('Software\Devart\' + ProjectName, False) then begin
      try
        ValName := 'D' + IntToStr(IDEInfos[IDEVer].Version);
        Path := Registry.ReadString(ValName);
        Result := IncludeTrailingPathDelimiter(Path) + 'Doc\' + ProjectName + {$IFNDEF VER22P}'.hxs'{$ELSE}'.chm'{$ENDIF};
        Exit;
      except
        // silent
        Registry.CloseKey;
      end;
    end;
  {$ENDIF}

    Registry.RootKey := HKEY_CURRENT_USER;
    sl := TStringList.Create;
    if not Registry.OpenKey(IDEInfos[IDEVer].RegistryKey + '\Known Assemblies', False) then begin
      Result := ProjectName + {$IFNDEF VER22P}'.hxs'{$ELSE}'.chm'{$ENDIF};
      Exit;
    end;

    Registry.GetValueNames(sl);
    UCProjectName := UpperCase('Devart.' + ProjectName + '.Design');
    for i := 0 to sl.Count - 1 do
      if (Registry.GetDataType(sl[i]) in [rdString, rdExpandString])
        and (UpperCase(Registry.ReadString(sl[i])) = UCProjectName) then begin
        Path := sl[i];
        Break;
      end;

    if Path = '' then begin
      Result := ProjectName + {$IFNDEF VER22P}'.hxs'{$ELSE}'.chm'{$ENDIF};
      Exit;
    end;

  finally
    sl.Free;
    Registry.Free;
  end;

  Ind := LastDelimiter('\', Path);
  Path := Copy(Path, 1, Ind - 1);
  Ind := LastDelimiter('\', Path);
  Path := Copy(Path, 1, Ind);
  Result := Path + 'Doc\' + ProjectName + {$IFNDEF VER22P}'.hxs'{$ELSE}'.chm'{$ENDIF};
end;
{$ELSE}
function GetHelpFileName(const ProjectName: string; UseCHM: boolean = False): string;
var
  Path: string;
{$IFNDEF VER7}
  Ind: integer;
{$ENDIF}
{$IFDEF VER5P}
  Registry: TRegistry;
  ValName: string;
{$ENDIF}
begin
{$IFDEF VER5P}
  if UseCHM then begin
    Result := '';

    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_LOCAL_MACHINE;
      if Registry.OpenKey('Software\Devart\' + ProjectName, False) then begin
        try
        {$IFDEF CB5}
          ValName := 'C5';
        {$ELSE}
        {$IFDEF CB6}
          ValName := 'C6';
        {$ELSE}
        {$IFDEF VER5}
          ValName := 'D5';
        {$ELSE}
        {$IFDEF VER6}
          ValName := 'D6';
        {$ELSE}
        {$IFDEF VER7}
          ValName := 'D7';
        {$ELSE}
          ValName := '';
        {$ENDIF}
        {$ENDIF}
        {$ENDIF}
        {$ENDIF}
        {$ENDIF}

          if ValName <> '' then begin
            Path := Registry.ReadString(ValName);
          {$IFDEF VER5}
            Result := IncludeTrailingBackslash(Path) + 'Doc\' + ProjectName + '.chm';
          {$ELSE}
            Result := IncludeTrailingPathDelimiter(Path) + 'Doc\' + ProjectName + '.chm';
          {$ENDIF}
          end;

          Exit;
        except
          // silent
        end;
      end;
    finally
      Registry.Free;
    end;
  end
  else
{$ENDIF}
  begin
  {$IFDEF VER7}
    Result := ProjectName + '.hlp';
  {$ELSE}
    Path := Application.ExeName;
    Ind := LastDelimiter('\', Path);
    Path := Copy(Path, 1, Ind - 1);
    Ind := LastDelimiter('\', Path);
    Path := Copy(Path, 1, Ind);
    Result := Path + 'help\' + ProjectName + '.hlp';
  {$ENDIF}
  end;
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFDEF WIN32_64}
{$IFNDEF CLX}
{$IFNDEF FMX}
  Screen.Cursors[crSQLArrow] := LoadCursor(HInstance, 'SQLARROW');
{$ENDIF}
{$ENDIF}
{$ELSE}
  Timer := nil;
{$ENDIF}

{$IFDEF FMX}
  if not Assigned(SetCursorProc) then
{$ENDIF}
  SetCursorProc := SetCursor;

{$IFDEF FMX}
  ShowConnectFormProcFmx := ShowConnectForm;
  if not Assigned(ShowConnectFormProc) then
{$ENDIF}
    ShowConnectFormProc := ShowConnectForm;

{$IFDEF FMX}
  if not Assigned(ShowDebugFormProc) then
{$ENDIF}
    ShowDebugFormProc := ShowDebugForm;

{$IFDEF FMX}
  if not Assigned(StartWaitProc) then
{$ENDIF}
    StartWaitProc := StartWait;

{$IFDEF FMX}
  if not Assigned(StopWaitProc) then
{$ENDIF}
    StopWaitProc := StopWait;

{$IFDEF FMX}
  if not Assigned(ApplicationTitleProc) then
{$ENDIF}
    ApplicationTitleProc := ApplicationTitle;

{$IFNDEF VER6P}
  ApplicationHandleException := Application.HandleException;
{$ENDIF};

finalization
{$IFDEF WIN32_64}
{$IFNDEF CLX}
{$IFNDEF FMX}
  DestroyCursor(Screen.Cursors[crSQLArrow]);
{$ENDIF}
{$ENDIF}
  FreeTimer(True);
{$ELSE}
  if Timer <> nil then
    Timer.Free;
{$ENDIF}

end.

