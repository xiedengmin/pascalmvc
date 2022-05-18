
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$DEFINE FMX}
{$I IdeVer.inc}

unit UniConnectFormFmx;

interface

uses
  Classes, SysUtils, 
{$IFDEF MSWINDOWS}
  Winapi.Windows, System.Win.Registry,
{$ENDIF}
  System.UITypes, FMX.Types, FMX.Platform,
  FMX.Forms, FMX.Controls, FMX.Edit, FMX.ListBox, FMX.Memo,
{$IFDEF VER18P}
  FMX.StdCtrls,
{$IFDEF VER21P}
  FMX.ComboEdit,
{$ENDIF}
{$ENDIF}
  CRtypes, DBAccess, UniProvider, Uni;

type
  TUniConnectForm = class(TForm)
    Panel: TPanel;
    lbUsername: TLabel;
    lbPassword: TLabel;
    lbServer: TLabel;
    lbPort: TLabel;
    edUserName: TEdit;
    edPassword: TEdit;
    edServer: TEdit;
    btConnect: TButton;
    btCancel: TButton;
    edProvider: TComboBox;
    lbProvider: TLabel;
    lbDatabase: TLabel;
    edDatabase: TEdit;
    edPort: TEdit;
    procedure btConnectClick(Sender: TObject);
    procedure edProviderChange(Sender: TObject);

  private
    FConnectDialog: TCustomConnectDialog;
    FRetries: integer;
    FRetry: boolean;
    FProviderGot: boolean;
    procedure SetConnectDialog(Value: TCustomConnectDialog);

  protected
    procedure DoInit; virtual;
    procedure DoConnect; virtual;
  {$IFDEF MSWINDOWS}
    procedure AssignPort(const Value: Integer);
  {$ENDIF}

  published
    property ConnectDialog: TCustomConnectDialog read FConnectDialog write SetConnectDialog;
  end;

implementation

{$R *.fmx}

uses
  CRFunctions, UniDacFmx;

{ TUniConnectForm }

procedure TUniConnectForm.DoConnect;
begin
  with FConnectDialog.Connection as TUniConnection do begin
    if edProvider.Selected <> nil then
      ProviderName := edProvider.Selected.Text
    else
      ProviderName := '';
    UserName := edUserName.Text;
    Password := edPassword.Text;
    Server := edServer.Text;

    Server := edServer.Text; // +++

    if edPort.Enabled then
      try
        Port := StrToInt(edPort.Text)
      except
        ActiveControl := edPort;
        raise; // TODO: possibly, raise Exception.Create('Invalid port number'); ???
      end;
    if edDatabase.Enabled then
      Database := edDatabase.Text;
  end;

  try
    FConnectDialog.Connection.PerformConnect(FRetry);
    ModalResult := mrOk;
  except
    on E: EUniError do begin
      Dec(FRetries);
      FRetry := True;
      if FRetries = 0 then
        ModalResult := mrCancel;
// TODO: добавить анализ ошибки и активацию соотв. контрола (например если ошибка в пароле - ActiveControl := edPassword) ???
      raise;
    end
    else
      raise;
  end;
end;

procedure TUniConnectForm.DoInit;
var
  ProviderList: TStringList;
  Index: Integer;
  Provider: TUniProvider;
  ConDialog: {$IFDEF FMX}TUniConnectDialogFmx{$ELSE}TUniConnectDialog{$ENDIF};
  Connection: TUniConnection;
begin
  ConDialog := FConnectDialog as {$IFDEF FMX}TUniConnectDialogFmx{$ELSE}TUniConnectDialog{$ENDIF};
  Connection := ConDialog.Connection;

  FRetry := False;
  FRetries := ConDialog.Retries;
  Caption := ConDialog.Caption;

  // fill the providers list
  if not FProviderGot then begin
    ProviderList := TStringList.Create;
    try
      UniProviders.GetProviderNames(ProviderList);
      edProvider.Items.Clear;
      edProvider.Items.Assign(ProviderList);
      Index := edProvider.Items.IndexOf(Connection.ProviderName);
      if Index <> -1 then begin
        edProvider.ItemIndex := Index;
//        edProvider.Text := edProvider.Items[Index];
        ActiveControl := edUsername;
      end
      else
        ActiveControl := edProvider;
    finally
      ProviderList.Free;
    end;
  end;

  lbUsername.Text := ConDialog.UsernameLabel;
  lbPassword.Text := ConDialog.PasswordLabel;
  lbServer.Text := ConDialog.ServerLabel;
  lbPort.Text := ConDialog.PortLabel;
  lbDatabase.Text := ConDialog.DatabaseLabel;
  lbProvider.Text := ConDialog.ProviderLabel;

  btConnect.Text := ConDialog.ConnectButton;
  btCancel.Text := ConDialog.CancelButton;

  if TUniUtils.CanGetProvider(Connection) then begin
    Provider := TUniUtils.GetProvider(Connection);
    edUsername.Enabled := TUniConnectDialogUtils.GetConnectDialogService(ConDialog).UsernameEnabled;
    edPassword.Enabled := TUniConnectDialogUtils.GetConnectDialogService(ConDialog).PasswordEnabled;
    edServer.Enabled := TUniConnectDialogUtils.GetConnectDialogService(ConDialog).ServerEnabled;
    edDataBase.Enabled := Provider.IsDatabaseSupported and
      TUniConnectDialogUtils.GetConnectDialogService(ConDialog).DatabaseEnabled;
    edPort.Enabled := Provider.IsPortSupported and
      TUniConnectDialogUtils.GetConnectDialogService(ConDialog).PortEnabled;
  end
  else begin
    edUsername.Enabled := True;
    edPassword.Enabled := True;
    edServer.Enabled := True;
    edDataBase.Enabled := False;
    edPort.Enabled := False;
  end;

  if edUsername.Enabled then
    edUsername.Text := Connection.Username
  else
    edUsername.Text := '';

  if edPassword.Enabled then
    edPassword.Text := Connection.Password
  else
    edPassword.Text := '';

  if edServer.Enabled then
    edServer.Text := Connection.Server
  else
    edServer.Text := '';

  if edDataBase.Enabled then
    edDataBase.Text := Connection.Database
  else
    edDataBase.Text := '';

  if edPort.Enabled and (Connection.Port <> -1) then
    edPort.Text := IntToStr(Connection.Port)
  else
    edPort.Text := '';

  lbUsername.Enabled := edUsername.Enabled;
  lbPassword.Enabled := edPassword.Enabled;
  lbServer.Enabled := edServer.Enabled;
  lbDatabase.Enabled := edDatabase.Enabled;
  lbPort.Enabled := edPort.Enabled;

  if (edUsername.Text <> '') and (edPassword.Text = '') and (ActiveControl <> edProvider) then
    ActiveControl := edPassword;
end;

procedure TUniConnectForm.SetConnectDialog(Value: TCustomConnectDialog);
begin
  FConnectDialog := Value;
  FProviderGot := False;
  DoInit;
end;

procedure TUniConnectForm.btConnectClick(Sender: TObject);
begin
  DoConnect;
end;             

procedure TUniConnectForm.edProviderChange(Sender: TObject);
begin
  FProviderGot := True;
  try
    if edProvider.Selected <> nil then
      (FConnectDialog.Connection as TUniConnection).ProviderName := edProvider.Selected.Text
    else
      (FConnectDialog.Connection as TUniConnection).ProviderName := '';
  {$IFDEF MSWINDOWS}
    TUniConnectDialogUtils.ReloadInfoFromRegistry(FConnectDialog as {$IFDEF FMX}TUniConnectDialogFmx{$ELSE}TUniConnectDialog{$ENDIF});
  {$ENDIF}
    DoInit;
  finally
    FProviderGot := False;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TUniConnectForm.AssignPort(const Value: Integer);
begin
  (FConnectDialog.Connection as TUniConnection).Port := Value;
  edPort.Text:= IntToStr(Value);
end;
{$ENDIF}

end.

