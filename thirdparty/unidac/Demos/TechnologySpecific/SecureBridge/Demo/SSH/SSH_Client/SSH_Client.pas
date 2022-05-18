unit SSH_Client;

{$I ..\..\Base\SBDemo.inc}
{$I ..\..\Design\IdeVer.inc}
interface

uses
  Classes, SysUtils, DB,
  {$IFDEF VER16P}
  UITypes,
  {$ENDIF}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, TypInfo,
  DBCtrls, ExtCtrls, Grids, DBGrids, StdCtrls, ToolWin, ComCtrls,
  Buttons, Spin, DemoFrame, MemDS, DBAccess, Uni, UniProvider,
  UniDacVcl, ScBridge, ScSSHClient, ScSSHChannel, CRSSHIOHandler, CRVio,
  OraClassesUni, OraCallUni,
{$IFNDEF CLR}
  OracleUniProvider,
  SQLServerUniProvider,
  InterBaseUniProvider,
  MySQLUniProvider,
  PostgreSQLUniProvider
{$ELSE}
  System.ComponentModel,
  Devart.UniDac.Oracle.OracleUniProvider,
  Devart.UniDac.SQLServer.SQLServerUniProvider,
  Devart.UniDac.InterBase.InterBaseUniProvider,
  Devart.UniDac.MySQL.MySQLUniProvider,
  Devart.UniDac.PostgreSQL.PostgreSQLUniProvider
{$ENDIF}
  ;

type
  TSSHClientFrame = class(TDemoFrame)
    Panel1: TPanel;
    Panel4: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel5: TPanel;
    lbSSHConnection: TLabel;
    lbDBConnection: TLabel;
    lbSSHUserName: TLabel;
    edSSHUserName: TEdit;
    lbSShPassword: TLabel;
    edSSHPassword: TEdit;
    lbPrivateKey: TLabel;
    cbSSHPrivateKey: TComboBox;
    pnPassword: TPanel;
    pnPrivateKey: TPanel;
    rbLocalPF: TRadioButton;
    rbDirect: TRadioButton;
    btConnectDB: TSpeedButton;
    btDisconnectDB: TSpeedButton;
    DBGrid: TDBGrid;
    btConnectSSH: TSpeedButton;
    btDisconnectSSH: TSpeedButton;
    ScSSHClient: TScSSHClient;
    CRSSHIOHandler: TCRSSHIOHandler;
    UniConnection: TUniConnection;
    UniTable: TUniTable;
    DataSource: TDataSource;
    lbSSHServer: TLabel;
    lbSSHPort: TLabel;
    edSSHHost: TEdit;
    edSSHPort: TEdit;
    lbListenPort: TLabel;
    lbDBServer: TLabel;
    edDBHost: TEdit;
    lbDBPort: TLabel;
    lbDBUserName: TLabel;
    edDBUserName: TEdit;
    lbDBPassword: TLabel;
    edDBPassword: TEdit;
    lbDatabase: TLabel;
    lbAuthenticationKind: TLabel;
    rbPassword: TRadioButton;
    rbPublicKey: TRadioButton;
    pnGenerateKey: TPanel;
    btKeyGen: TSpeedButton;
    seDBPort: TSpinEdit;
    ScFileStorage: TScFileStorage;
    cbDBDatabase: TComboBox;
    Panel10: TPanel;
    lbTableName: TLabel;
    cbTableName: TComboBox;
    Panel9: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    DBNavigator: TDBNavigator;
    Panel8: TPanel;
    ScSSHChannel: TScSSHChannel;
    seListenPort: TSpinEdit;
    cbRandomization: TCheckBox;
    lbProvider: TLabel;
    cbProvider: TComboBox;
    lbSID: TLabel;
    cbSID: TCheckBox;
    edDBServiceName: TEdit;
    cbConnectMode: TComboBox;
    lbConnectMode: TLabel;
    lbKeyPath: TLabel;
    edSSHKeyPath: TEdit;
    sbKeyPath: TSpeedButton;    
    OpenDialog: TOpenDialog;
    procedure rbLocalPFClick(Sender: TObject);
    procedure rbDirectClick(Sender: TObject);
    procedure rbPasswordClick(Sender: TObject);
    procedure rbPublicKeyClick(Sender: TObject);
    procedure edSSHUserNameChange(Sender: TObject);
    procedure cbSSHPrivateKeyDropDown(Sender: TObject);
    procedure btConnectDBClick(Sender: TObject);
    procedure btDisconnectDBClick(Sender: TObject);
    procedure btConnectSSHClick(Sender: TObject);
    procedure btDisconnectSSHClick(Sender: TObject);
    procedure ScSSHClientAfterConnect(Sender: TObject);
    procedure ScSSHClientAfterDisconnect(Sender: TObject);
    procedure UniConnectionAfterConnect(Sender: TObject);
    procedure UniConnectionAfterDisconnect(Sender: TObject);
    procedure UniTableAfterClose(DataSet: TDataSet);
    procedure UniTableAfterOpen(DataSet: TDataSet);
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure cbTableNameDropDown(Sender: TObject);
    procedure cbTableNameChange(Sender: TObject);
    procedure edListenPortChange(Sender: TObject);
    procedure cbSSHPrivateKeyChange(Sender: TObject);
    procedure btKeyGenClick(Sender: TObject);
    procedure ScSSHClientServerKeyValidate(Sender: TObject;
      NewServerKey: TScKey; var Accept: Boolean);
    procedure cbDBDatabaseDropDown(Sender: TObject);
    procedure cbDBDatabaseChange(Sender: TObject);
    procedure UniConnectionBeforeConnect(Sender: TObject);
    procedure ScSSHClientBeforeConnect(Sender: TObject);
    procedure cbSIDClick(Sender: TObject);
    procedure cbProviderChange(Sender: TObject);
    procedure sbKeyPathClick(Sender: TObject);
    procedure edSSHKeyPathChange(Sender: TObject);
  private
    procedure CheckRandomize;
    procedure ShowPasswordAuth(pa: boolean);
    procedure ShowSSHButtons;
    procedure ShowDBButtons;
    procedure EnableLPFComponents(Enabled: boolean);
    procedure ChangeDatabaseControl(ProviderName: String);
  {$IFDEF MSWINDOWS}
    function LoadState: boolean;
    function SaveState: boolean;
    function KeyPath: string;
  {$ENDIF}
  public
    destructor Destroy; override;
    procedure DisconnectAll;
    procedure Initialize; override;
    procedure Finalize; override;
  end;

var
  SSHClientFrame: TSSHClientFrame;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
  ScSSHUtils, ScUtils, ScConsts, SSHDacDemoForm;

const
  SSHPrefix = 'ssh://';

destructor TSSHClientFrame.Destroy;
begin
  DisconnectAll;
  inherited;
end;

procedure TSSHClientFrame.Initialize;
begin
  inherited;

  UniProviders.GetProviderNames(cbProvider.Items);

{$IFDEF MSWINDOWS}
  LoadState;
{$ENDIF}

  EnableLPFComponents(rbLocalPF.Checked);
  edSSHHost.Text := ScSSHClient.HostName;
  edSSHPort.Text := IntToStr(ScSSHClient.Port);
  edSSHUserName.Text := ScSSHClient.User;
  edSSHKeyPath.Text := ScFileStorage.Path;
  if cbProvider.ItemIndex = -1 then
    cbProvider.ItemIndex := 0;
  ChangeDatabaseControl(cbProvider.Text);
end;

procedure TSSHClientFrame.Finalize;
begin
{$IFDEF MSWINDOWS}
  SaveState;
{$ENDIF}

  inherited;
end;

procedure TSSHClientFrame.CheckRandomize;
begin
  if not SSHDacForm.Randomized and not cbRandomization.Checked then begin
    SSHDacForm.Randomize;
    if not SSHDacForm.Randomized and not cbRandomization.Checked then
      raise Exception.Create('Data for the random generator has not been generated');
  end;
end;

procedure TSSHClientFrame.DisconnectAll;
begin
  UniConnection.Close;
  ScSSHChannel.Disconnect;
  ScSSHClient.Disconnect;
  ShowSSHButtons;
end;

procedure TSSHClientFrame.ShowSSHButtons;
begin
  btConnectSSH.Enabled := not ScSSHClient.Connected;
  btDisconnectSSH.Enabled := ScSSHClient.Connected;
  btConnectDB.Enabled := ScSSHClient.Connected and not UniConnection.Connected;
end;

procedure TSSHClientFrame.ShowDBButtons;
begin
  btConnectDB.Enabled := not UniConnection.Connected;
  btDisconnectDB.Enabled := UniConnection.Connected;
  btOpen.Enabled := UniConnection.Connected and (cbTableName.Text <> '');
  cbTableName.Enabled := UniConnection.Connected;
end;

procedure TSSHClientFrame.ScSSHClientAfterConnect(Sender: TObject);
begin
  ShowSSHButtons;
end;

procedure TSSHClientFrame.ScSSHClientAfterDisconnect(Sender: TObject);
begin
  ShowSSHButtons;
  ScSSHChannel.Disconnect;
end;

procedure TSSHClientFrame.btConnectSSHClick(Sender: TObject);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    ScSSHClient.Connect;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TSSHClientFrame.btDisconnectSSHClick(Sender: TObject);
begin
  DisconnectAll;
end;

procedure TSSHClientFrame.EnableLPFComponents(Enabled: boolean);
begin
  lbListenPort.Enabled := Enabled;
  seListenPort.Enabled := Enabled;
end;

procedure TSSHClientFrame.cbProviderChange(Sender: TObject);
begin
  UniConnection.Disconnect;
  ScSSHChannel.Disconnect;
  ChangeDatabaseControl(cbProvider.Text);
end;

procedure TSSHClientFrame.ChangeDatabaseControl(ProviderName: String);
var
  ThisIsOracle: Boolean;
begin
  ThisIsOracle := ProviderName = 'Oracle';
  if ThisIsOracle then begin
    lbDBUserName.Top := lbDBServer.Top + 78;
    edDBUserName.Top := lbDBServer.Top + 74;
    lbDBPassword.Top := lbDBServer.Top + 104;
    edDBPassword.Top := lbDBServer.Top + 100;
  end
  else begin
    lbDBUserName.Top := lbDBServer.Top + 52;
    edDBUserName.Top := lbDBServer.Top + 48;
    lbDBPassword.Top := lbDBServer.Top + 78;
    edDBPassword.Top := lbDBServer.Top + 74;
  end;
  lbSID.Visible := ThisIsOracle;
  cbSID.Visible := ThisIsOracle;
  edDBServiceName.Visible := ThisIsOracle;
  lbConnectMode.Visible := ThisIsOracle;
  cbConnectMode.Visible := ThisIsOracle;
  lbDatabase.Visible := Not(ThisIsOracle);
  cbDBDatabase.Visible := Not(ThisIsOracle);
end;

procedure TSSHClientFrame.rbLocalPFClick(Sender: TObject);
begin
  EnableLPFComponents(True);
  UniConnection.Disconnect;
end;

procedure TSSHClientFrame.rbDirectClick(Sender: TObject);
begin
  EnableLPFComponents(False);
  UniConnection.Disconnect;
end;

procedure TSSHClientFrame.edSSHKeyPathChange(Sender: TObject);
begin
  if SysUtils.DirectoryExists(edSSHKeyPath.Text) then begin
    ScFileStorage.Path := edSSHKeyPath.Text;
    ScFileStorage.Keys.GetKeyNames(cbSSHPrivateKey.Items);
    cbSSHPrivateKey.ItemIndex := 0;
  end;
end;

procedure TSSHClientFrame.edListenPortChange(Sender: TObject);
begin
  UniConnection.Disconnect;
  ScSSHChannel.Disconnect;
end;

procedure TSSHClientFrame.ShowPasswordAuth(pa: boolean);
begin
  pnPassword.Visible := pa;
  pnPrivateKey.Visible := not pa;
  if pa then
    cbRandomization.Top := 160
  else
    cbRandomization.Top := 186;
  Repaint;
end;

procedure TSSHClientFrame.rbPasswordClick(Sender: TObject);
begin
  ShowPasswordAuth(True);
  DisconnectAll;
end;

procedure TSSHClientFrame.rbPublicKeyClick(Sender: TObject);
begin
  ShowPasswordAuth(False);
  DisconnectAll;
end;

procedure TSSHClientFrame.edSSHUserNameChange(Sender: TObject);
begin
  DisconnectAll;
end;

procedure TSSHClientFrame.cbSSHPrivateKeyDropDown(Sender: TObject);
begin
  ScFileStorage.Keys.GetKeyNames(cbSSHPrivateKey.Items);
end;

procedure TSSHClientFrame.cbSSHPrivateKeyChange(Sender: TObject);
begin
  DisconnectAll;
end;

procedure TSSHClientFrame.btConnectDBClick(Sender: TObject);
begin
  UniConnection.Connect;
end;

procedure TSSHClientFrame.btDisconnectDBClick(Sender: TObject);
begin
  UniConnection.Disconnect;
end;

procedure TSSHClientFrame.UniConnectionAfterConnect(Sender: TObject);
begin
  ShowDBButtons;
end;

procedure TSSHClientFrame.UniConnectionAfterDisconnect(Sender: TObject);
begin
  ShowDBButtons;
  UniConnection.IOHandler := nil;
  ScSSHChannel.Disconnect;
end;

procedure TSSHClientFrame.UniTableAfterOpen(DataSet: TDataSet);
begin
  btOpen.Enabled := False;
  btClose.Enabled := True;
end;

procedure TSSHClientFrame.UniTableAfterClose(DataSet: TDataSet);
begin
  btOpen.Enabled := not btConnectDB.Enabled and (cbTableName.Text <> '');
  btClose.Enabled := False;
end;

procedure TSSHClientFrame.btOpenClick(Sender: TObject);
begin
  UniTable.Open;
end;

procedure TSSHClientFrame.btCloseClick(Sender: TObject);
begin
  UniTable.Close;
end;

procedure TSSHClientFrame.cbTableNameDropDown(Sender: TObject);
begin
  if UniConnection.Connected then
    UniConnection.GetTableNames(cbTableName.Items)
  else
    cbTableName.Items.Clear;
end;

procedure TSSHClientFrame.cbTableNameChange(Sender: TObject);
begin
  UniTable.TableName := cbTableName.Text;
  btOpen.Enabled := UniConnection.Connected and (cbTableName.Text <> '');
end;

{$IFDEF MSWINDOWS}
function TSSHClientFrame.SaveState: boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    with Registry do begin
      OpenKey(KeyPath + '\' + TSSHClientFrame.ClassName, True);
      WriteBool('SSHPasswordAuth', rbPassword.Checked);
      WriteString('SSHHost', ScSSHClient.HostName);
      WriteInteger('SSHPort', ScSSHClient.Port);
      WriteString('SSHUserName', ScSSHClient.User);
      WriteString('SSHHost', edSSHHost.Text);
      if edSSHPort.Text <> '' then
        WriteInteger('SSHPort', StrToInt(edSSHPort.Text))
      else
        WriteInteger('SSHPort', 22);
      WriteString('SSHKeyPath', ScFileStorage.Path);
      WriteString('SSHKeyExt', ScFileStorage.KeyExt);

      WriteInteger('ListenPort', seListenPort.Value);
      WriteInteger('Provider', cbProvider.ItemIndex);
      WriteString('DBHost', edDBHost.Text);
      WriteInteger('DBPort', seDBPort.Value);
      WriteString('DBUserName', edDBUserName.Text);
      WriteString('DBDatabase', cbDBDatabase.Text);
      WriteString('DBServiceName', edDBServiceName.Text);
      WriteBool('Silent randomization', cbRandomization.Checked);
      WriteBool('Use SID', cbSID.Checked);
      WriteInteger('ConnectMode', cbConnectMode.ItemIndex);
    end;
  finally
    Registry.Free;
  end;

  Result := True;
end;

function TSSHClientFrame.LoadState: boolean;
var
  Registry: TRegistry;
begin
  Result := False;
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    with Registry do begin
      if OpenKey(KeyPath + '\' + TSSHClientFrame.ClassName, False) then begin
        if ValueExists('SSHPasswordAuth') then begin
          rbPassword.Checked := ReadBool('SSHPasswordAuth');
          rbPublicKey.Checked := not(ReadBool('SSHPasswordAuth'));
        end;
        if ValueExists('SSHHost') then
          ScSSHClient.HostName := ReadString('SSHHost');
        if ValueExists('SSHPort') then
          ScSSHClient.Port := ReadInteger('SSHPort');
        if ValueExists('SSHUserName') then
          ScSSHClient.User := ReadString('SSHUserName');
        if ValueExists('SSHKeyPath') then
           ScFileStorage.Path := ReadString('SSHKeyPath');
        if ValueExists('SSHKeyExt') then
           ScFileStorage.KeyExt := ReadString('SSHKeyExt');

        if ValueExists('ListenPort') then
          seListenPort.Value := ReadInteger('ListenPort');
        if ValueExists('Provider') then
          cbProvider.ItemIndex := ReadInteger('Provider');
        if ValueExists('DBHost') then
          edDBHost.Text := ReadString('DBHost');
        if ValueExists('DBPort') then
          seDBPort.Value := ReadInteger('DBPort');
        if ValueExists('DBServiceName') then
          edDBServiceName.Text := ReadString('DBServiceName');
        if ValueExists('DBUserName') then
          edDBUserName.Text := ReadString('DBUserName');
        if ValueExists('Silent randomization') then
          cbRandomization.Checked := ReadBool('Silent randomization');
        if ValueExists('Use SID') then
          cbSID.Checked := ReadBool('Use SID');
        if ValueExists('ConnectMode') then
          cbConnectMode.ItemIndex := ReadInteger('ConnectMode');
        Result := True;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TSSHClientFrame.KeyPath: string;
begin
  Result := '\SOFTWARE\Devart\UniDAC\SecureBridge\Demos\SSH';
end;
{$ENDIF}

procedure TSSHClientFrame.btKeyGenClick(Sender: TObject);
var
  msg: string;
  OldCursor: TCursor;
  Key: TScKey;
  Algorithm: TScAsymmetricAlgorithm;
  BitCount: integer;
begin
  CheckRandomize;

  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    if cbSSHPrivateKey.Text = '' then
      cbSSHPrivateKey.Text := 'client_key';

    Key := ScFileStorage.Keys.FindKey(cbSSHPrivateKey.Text);

    if Key = nil then begin
      Key := TScKey.Create(ScFileStorage.Keys);
      Key.KeyName := cbSSHPrivateKey.Text;
      Algorithm := aaRSA;
      BitCount := 1024;
    end
    else begin
      Key.Ready := True;
      Algorithm := Key.Algorithm;
      BitCount := Key.BitCount;
    end;

    try
      Key.Generate(Algorithm, BitCount);
      Key.ExportTo(Key.KeyName + '.pub', True, '');

      msg := 'The client key file has been generated into the current application directory.'#13#10 +
             'To connect with authentication by key, you should pass the "' + Key.KeyName +
             '.pub" file to the server and set the server to work with this file.';
      MessageDlg(msg, mtInformation, [mbOk], 0);
    except
      on E: Exception do
        MessageDlg('Cannot generate key: ' + E.Message, mtWarning, [mbOk], 0);
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TSSHClientFrame.ScSSHClientServerKeyValidate(Sender: TObject;
  NewServerKey: TScKey; var Accept: Boolean);
var
  Key: TScKey;
  fp, msg: string;
begin
  Key := ScFileStorage.Keys.FindKey(ScSSHClient.HostName);
  if (Key = nil) or not Key.Ready then begin
    NewServerKey.GetFingerPrint(haMD5, fp);
    msg := 'The authenticity of server can not be established.'#13#10 +
           'Fingerprint for the key received from server: ' + fp +'.'#13#10 +
           'Key length: ' + IntToStr(NewServerKey.BitCount) +' bits.'#13#10 +
           'Are you sure you want to continue connecting?';

    if MessageDlg(msg, mtConfirmation, [mbOk, mbCancel], 0) = mrOk then begin
      NewServerKey.KeyName := ScSSHClient.HostName;
      ScFileStorage.Keys.Add(NewServerKey);
      Accept := True;
    end;
  end;
end;

procedure TSSHClientFrame.cbDBDatabaseDropDown(Sender: TObject);
begin
  UniConnection.GetDatabaseNames(cbDBDatabase.Items)
end;

procedure TSSHClientFrame.cbDBDatabaseChange(Sender: TObject);
begin
  UniTable.Close;
  UniConnection.Database := cbDBDatabase.Text;
  cbTableName.Text := '';
end;

procedure TSSHClientFrame.UniConnectionBeforeConnect(Sender: TObject);
var
  ServerInfo: TDirectServerInfo;
begin
  if rbLocalPF.Checked then begin
    UniConnection.IOHandler := nil;

    ScSSHChannel.SourcePort := seListenPort.Value;
    ScSSHChannel.DestPort := seDBPort.Value;
    ScSSHChannel.DestHost := edDBHost.Text;
    ScSSHChannel.Connect;

    UniConnection.Server := 'localhost';
    UniConnection.Port := ScSSHChannel.SourcePort;
  end
  else begin
    UniConnection.IOHandler := CRSSHIOHandler;
    UniConnection.Server := edDBHost.Text;
    UniConnection.Port := seDBPort.Value;
  end;

  UniConnection.ProviderName := cbProvider.Text;
  UniConnection.Username := edDBUserName.Text;
  UniConnection.Password := edDBPassword.Text;
  if UniConnection.ProviderName <> 'Oracle' then
    UniConnection.Database := cbDBDatabase.Text
  else begin
    if Pos(SSHPrefix, edDBHost.Text)=0 then
      if MessageDlg(Format('The host name for the SSH connection must begin with "%s". Should I change the value of the "Server" field?', [SSHPrefix]), mtInformation, [mbYes, mbNo], 0) = mrYes then
        edDBHost.Text := SSHPrefix + edDBHost.Text;
    UniConnection.SpecificOptions.Values['Direct'] := 'True';
    ServerInfo :=  TDirectServerInfo.Create;
    try
      ServerInfo.Host := edDBHost.Text;
      ServerInfo.Port := seDBPort.Text;
      if cbSID.Checked then
        ServerInfo.SID := edDBServiceName.Text
      else
        ServerInfo.ServiceName := edDBServiceName.Text;
      UniConnection.Server := ServerInfo.GetServerInfo;
    finally
      ServerInfo.Free;
    end;
    UniConnection.SpecificOptions.Values['Oracle.ConnectMode'] := GetEnumName(TypeInfo(TConnectMode), cbConnectMode.ItemIndex);
  end;
  if UniConnection.Username = '' then
     raise Exception.Create('Username cannot be empty');
end;

procedure TSSHClientFrame.ScSSHClientBeforeConnect(Sender: TObject);
begin
  CheckRandomize;

  ScSSHClient.HostName := edSSHHost.Text;
  if edSSHPort.Text <> '' then
    ScSSHClient.Port := StrToInt(edSSHPort.Text);
  ScSSHClient.User := edSSHUserName.Text;

  if rbPassword.Checked then begin
    ScSSHClient.Authentication := atPassword;
    ScSSHClient.Password := edSSHPassword.Text;
  end
  else begin
    ScSSHClient.Authentication := atPublicKey;
    ScSSHClient.PrivateKeyName := cbSSHPrivateKey.Text;
    if ScFileStorage.Keys.FindKey(ScSSHClient.PrivateKeyName) = nil then
      raise EScError.Create('Private key can not be empty');

    if (ScSSHClient.User = '') and (cbProvider.Text = 'Oracle') then
      ScSSHClient.User := 'opc';
  end;
end;

procedure TSSHClientFrame.cbSIDClick(Sender: TObject);
begin
  if cbSID.Checked then
    lbSID.Caption := 'SID'
  else
    lbSID.Caption := 'Service Name';
end;

procedure TSSHClientFrame.sbKeyPathClick(Sender: TObject);
begin
  OpenDialog.Filter := 'All files (*.*)|*.*|Private keys (*.ppk)|*.ppk|Private keys (*.ssh)|*.ssh|Public keys (*.pub)|*.pub';
  OpenDialog.Title := 'Select Private Key';
  if OpenDialog.Execute then begin
    ScFileStorage.KeyExt := Copy(ExtractFileExt(OpenDialog.FileName), 2, Length(ExtractFileExt(OpenDialog.FileName)));
    edSSHKeyPath.Text := ExtractFilePath(OpenDialog.FileName);
  end;
end;

end.