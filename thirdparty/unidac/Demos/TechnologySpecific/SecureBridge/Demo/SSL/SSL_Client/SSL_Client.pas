 unit SSL_Client;

{$I ..\..\Base\SBDemo.inc}
interface

uses
  Classes, SysUtils, DB,
  {$IFDEF VER16P}
  UITypes,
  {$ENDIF}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, TypInfo,
  DBCtrls, ExtCtrls, Grids, DBGrids, StdCtrls, ToolWin, ComCtrls,
  Buttons, Spin, DemoFrame, MemDS, DBAccess, Uni, UniProvider,
  UniDacVcl, ScBridge, ScCryptoAPIStorage, CRSSLIOHandler, CRVio,
  CRSsoStorage,
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
  TSSLClientFrame = class(TDemoFrame)
    Panel1: TPanel;
    Panel4: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel5: TPanel;
    pnOracleOptions: TPanel;
    pnAdvancedOptions: TPanel;
    lbSSLConnection: TLabel;
    lbCAcertificate: TLabel;
    lbClientCertificate: TLabel;
    lbClientPrivateKey: TLabel;
    lbStorageKind : TLabel;
    lbWallet: TLabel;
    lbServerCertDN: TLabel;
    edServerCertDN: TEdit;
    lbDBConnection: TLabel;
    lbProvider: TLabel;
    lbDBServer: TLabel;
    lbDBPort: TLabel;
    lbSID: TLabel;
    lbDBUserName: TLabel;
    btConnectDB: TSpeedButton;
    lbDBPassword: TLabel;
    lbDBDatabase: TLabel;
    lbConnectMode: TLabel;
    btDisconnectDB: TSpeedButton;
    DBGrid: TDBGrid;
    UniConnection: TUniConnection;
    UniTable: TUniTable;
    DataSource: TDataSource;
    edDBHost: TEdit;
    edDBUserName: TEdit;
    edDBPassword: TEdit;
    seDBPort: TSpinEdit;
    cbDBDatabase: TComboBox;
    Panel7: TPanel;
    lbTableName: TLabel;
    cbTableName: TComboBox;
    Panel9: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    Panel8: TPanel;
    CRSSLIOHandler: TCRSSLIOHandler;
    ScCryptoAPIStorage: TScCryptoAPIStorage;
    DBNavigator: TDBNavigator;
    Panel3: TPanel;
    edCACertName: TEdit;
    edKeyName: TEdit;
    cbTrustServerCertificate: TCheckBox;
    cbRandomization: TCheckBox;
    sbCACertName: TSpeedButton;
    edCertName: TEdit;
    sbCertName: TSpeedButton;
    sbKeyName: TSpeedButton;
    OpenDialog: TOpenDialog;
    sbWallet: TSpeedButton;
    cbProvider: TComboBox;
    edWallet: TEdit;
    rbWallet: TRadioButton;
    rbCertificate: TRadioButton;
    edDBServiceName: TEdit;
    cbSID: TCheckBox;
    cbConnectMode: TComboBox;
    procedure btConnectDBClick(Sender: TObject);
    procedure btDisconnectDBClick(Sender: TObject);
    procedure UniConnectionAfterConnect(Sender: TObject);
    procedure UniTableAfterClose(DataSet: TDataSet);
    procedure UniTableAfterOpen(DataSet: TDataSet);
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure cbTableNameDropDown(Sender: TObject);
    procedure cbTableNameChange(Sender: TObject);
    procedure cbDBDatabaseDropDown(Sender: TObject);
    procedure cbDBDatabaseChange(Sender: TObject);
    procedure UniConnectionBeforeConnect(Sender: TObject);
    procedure edDBHostChange(Sender: TObject);
    procedure sbCACertNameClick(Sender: TObject);
    procedure sbKeyNameClick(Sender: TObject);
    procedure sbCertNameClick(Sender: TObject);
    procedure sbWalletClick(Sender: TObject);
    procedure rbWalletClick(Sender: TObject);
    procedure rbCertificateClick(Sender: TObject);
    procedure cbSIDClick(Sender: TObject);
    procedure cbProviderChange(Sender: TObject);
  private
    FileStorage: TCRSSOFileStorage;
    procedure CheckRandomize;
    procedure ShowCertControls(Mode: Boolean);
    procedure ChangeDatabaseControl(ProviderName: String);
  {$IFDEF MSWINDOWS}
    procedure GetStoredParameters(ProviderName: String);
    function LoadState: boolean;
    function SaveState: boolean;
    function KeyPath: string;
    function GetDefaultPort(ProviderName: String): Integer;
  {$ENDIF}
  public
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
  end;

var
  SSLClientFrame: TSSLClientFrame;

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
  ScConsts, ScSSHUtils, SSLDacDemoForm,
  MyClassesUni, PgClassesUni;

const
  TCPPrefix = 'tcps://';
  CertFilter = 'All formats |*.pem;*.crt;*.cer|PEM format (*.pem;*.crt)|*.pem;*.crt|DER format (*.cer)|*.cer|All files (*.*)|*.*';
  KeyFilter = 'All formats |*.key;*.ssl;*.pem;*.ietf;*.pub;*.ietfpub|OpenSSL format (*.ssl)|*.ssl|PKCS8 format (*.pem)|*.pem|IETF format (*.ietf)|*.ietf|Public key (*.pub)|*.pub|Public IETF key (*.ietfpub)|*.ietfpub|All files (*.*)|*.*';
  WalletFilter = 'Wallet files(*.sso)|*.sso|All files (*.*)|*.*';

destructor TSSLClientFrame.Destroy;
begin
  UniConnection.Close;
  inherited;
end;

procedure TSSLClientFrame.Initialize;
begin
  inherited;

{$IFDEF MSWINDOWS}
  LoadState;
{$ENDIF}
  FileStorage := TCRSSOFileStorage.Create(Self);
  UniProviders.GetProviderNames(cbProvider.Items);
  if cbProvider.Text = '' then
    cbProvider.ItemIndex := 0;
  cbProviderChange(cbProvider);
  ChangeDatabaseControl(cbProvider.Text);
  ShowCertControls(rbCertificate.Checked or (cbProvider.Text<>'Oracle'));
end;

procedure TSSLClientFrame.Finalize;
begin
{$IFDEF MSWINDOWS}
  SaveState;
{$ENDIF}

  inherited;
end;

procedure TSSLClientFrame.CheckRandomize;
begin
  if not SSLDacForm.Randomized and not cbRandomization.Checked then begin
    SSLDacForm.Randomize;
    if not SSLDacForm.Randomized and not cbRandomization.Checked then
      raise Exception.Create('Data for the random generator has not been generated');
  end;
end;

procedure TSSLClientFrame.btConnectDBClick(Sender: TObject);
begin
  UniConnection.Connect;
end;

procedure TSSLClientFrame.btDisconnectDBClick(Sender: TObject);
begin
  UniConnection.Disconnect;
end;

procedure TSSLClientFrame.edDBHostChange(Sender: TObject);
begin
  UniConnection.Disconnect;
end;

procedure TSSLClientFrame.cbProviderChange(Sender: TObject);
begin
  UniConnection.Disconnect;
  ChangeDatabaseControl(cbProvider.Text);
  GetStoredParameters(cbProvider.Text);
  seDBPort.Value := GetDefaultPort(cbProvider.Text);
  ShowCertControls(rbCertificate.Checked or (cbProvider.Text<>'Oracle'));
end;


procedure TSSLClientFrame.UniConnectionAfterConnect(Sender: TObject);
begin
  btConnectDB.Enabled := not UniConnection.Connected;
  btDisconnectDB.Enabled := UniConnection.Connected;
  btOpen.Enabled := UniConnection.Connected and (cbTableName.Text <> '');
  cbTableName.Enabled := UniConnection.Connected;
end;

procedure TSSLClientFrame.UniTableAfterOpen(DataSet: TDataSet);
begin
  btOpen.Enabled := False;
  btClose.Enabled := True;
end;

procedure TSSLClientFrame.UniTableAfterClose(DataSet: TDataSet);
begin
  btOpen.Enabled := not btConnectDB.Enabled and (cbTableName.Text <> '');
  btClose.Enabled := False;
end;

procedure TSSLClientFrame.btOpenClick(Sender: TObject);
begin
  UniTable.Open;
end;

procedure TSSLClientFrame.btCloseClick(Sender: TObject);
begin
  UniTable.Close;
end;

procedure TSSLClientFrame.cbTableNameDropDown(Sender: TObject);
begin
  if UniConnection.Connected then
    UniConnection.GetTableNames(cbTableName.Items)
  else
    cbTableName.Items.Clear;
end;

procedure TSSLClientFrame.cbTableNameChange(Sender: TObject);
begin
  UniTable.TableName := cbTableName.Text;
  btOpen.Enabled := UniConnection.Connected and (cbTableName.Text <> '');
end;

{$IFDEF MSWINDOWS}
function TSSLClientFrame.SaveState: boolean;
var
  Registry: TRegistry;
  RegistryKey: String;
begin
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    RegistryKey := KeyPath + '\' + TSSLClientFrame.ClassName;
    with Registry do begin
      OpenKey(RegistryKey, True);
      WriteString('Provider', cbProvider.Text);
      WriteString('CACertName', edCACertName.Text);
      WriteString('ClientCertName', edCertName.Text);
      WriteString('CertPrivateKeyName', edKeyName.Text);
      WriteBool('Ñertificate Mode', rbCertificate.Checked);
      WriteBool('Trust Server Certificate', cbTrustServerCertificate.Checked);
      WriteBool('Silent randomization', cbRandomization.Checked);

      RegistryKey := RegistryKey + '\' + cbProvider.Text;
      OpenKey(RegistryKey, True);

      WriteString('DBHost', edDBHost.Text);
      WriteInteger('DBPort', seDBPort.Value);
      WriteString('DBUserName', edDBUserName.Text);
      WriteString('DBDatabase', cbDBDatabase.Text);

      if cbProvider.Text = 'Oracle' then begin
        WriteString('Wallet', edWallet.Text);
        WriteString('ServerCertDN', edServerCertDN.Text);
        WriteString('DBServiceName', edDBServiceName.Text);
        WriteBool('Use SID', cbSID.Checked);
        WriteInteger('ConnectMode', cbConnectMode.ItemIndex);
      end;
    end;
  finally
    Registry.Free;
  end;

  Result := True;
end;

function TSSLClientFrame.LoadState: boolean;
var
  Registry: TRegistry;
  RegistryKey: String;
begin
  Result := False;
  RegistryKey := KeyPath + '\' + TSSLClientFrame.ClassName;
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    with Registry do begin
      if OpenKey(RegistryKey, False) then begin
        if ValueExists('CACertName') then
          edCACertName.Text := ReadString('CACertName');
        if ValueExists('ClientCertName') then
          edCertName.Text := ReadString('ClientCertName');
        if ValueExists('CertPrivateKeyName') then
          edKeyName.Text := ReadString('CertPrivateKeyName');
        if ValueExists('Ñertificate Mode') then begin
          rbCertificate.Checked := ReadBool('Ñertificate Mode');
          rbWallet.Checked := not(rbCertificate.Checked);
        end;
        if ValueExists('Trust Server Ñertificate') then
          cbTrustServerCertificate.Checked := ReadBool('Trust Server Ñertificate');
        if ValueExists('Silent randomization') then
          cbRandomization.Checked := ReadBool('Silent randomization');

        if ValueExists('Provider') then
          cbProvider.Text := ReadString('Provider');

        Result := True;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TSSLClientFrame.KeyPath: string;
begin
  Result := '\SOFTWARE\Devart\UniDAC\SecureBridge\Demos';
end;
{$ENDIF}

procedure TSSLClientFrame.cbDBDatabaseDropDown(Sender: TObject);
begin
  UniConnection.GetDatabaseNames(cbDBDatabase.Items)
end;

procedure TSSLClientFrame.cbDBDatabaseChange(Sender: TObject);
begin
  UniTable.Close;
  UniConnection.Database := cbDBDatabase.Text;
  cbTableName.Text := '';
end;

procedure TSSLClientFrame.UniConnectionBeforeConnect(Sender: TObject);
var
  Cert: TScCertificate;
  ServerInfo: TDirectServerInfo;
begin
  CheckRandomize;
  ScCryptoAPIStorage.Certificates.Clear;

  UniConnection.ProviderName := cbProvider.Text;

  if rbCertificate.Checked or (UniConnection.ProviderName <> 'Oracle') then begin
    CRSSLIOHandler.CACertName := 'cacert';
    Cert := TScCertificate.Create(ScCryptoAPIStorage.Certificates);
    Cert.CertName := CRSSLIOHandler.CACertName;
    Cert.ImportFrom(edCACertName.Text);

    CRSSLIOHandler.CertName := 'clientcert';
    Cert := TScCertificate.Create(ScCryptoAPIStorage.Certificates);
    Cert.CertName := CRSSLIOHandler.CertName;
    Cert.ImportFrom(edCertName.Text);
    Cert.Key.ImportFrom(edKeyName.Text);
  end
  else begin
    CRSSLIOHandler.CACertName := '';
    CRSSLIOHandler.CertName := '';
  end;

  UniConnection.IOHandler := CRSSLIOHandler;
  UniConnection.SpecificOptions.Values['PostgreSQL.SSLMode'] := 'smRequire';
  UniConnection.SpecificOptions.Values['MySQL.Protocol'] := 'mpSSL';

  UniConnection.Server := edDBHost.Text;
  UniConnection.Port := seDBPort.Value;
  UniConnection.Username := edDBUserName.Text;
  UniConnection.Password := edDBPassword.Text;
  if UniConnection.ProviderName <> 'Oracle' then
    UniConnection.Database := cbDBDatabase.Text
  else begin
    if Pos(TCPPrefix, edDBHost.Text)=0 then
      if MessageDlg(Format('The host name for the SSL connection must begin with "%s". Should I change the value of the "Server" field?', [TCPPrefix]), mtInformation, [mbYes, mbNo], 0) = mrYes then
        edDBHost.Text := TCPPrefix + edDBHost.Text;
    UniConnection.SpecificOptions.Values['Oracle.Direct'] := 'True';
    FileStorage.Path := ExtractFilePath(edWallet.Text);
    CRSSLIOHandler.Storage := FileStorage;
    CRSSLIOHandler.SecurityOptions.TrustServerCertificate := cbTrustServerCertificate.Checked;
    UniConnection.SpecificOptions.Values['Oracle.SSLServerCertDN'] := edServerCertDN.Text;
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

procedure TSSLClientFrame.cbSIDClick(Sender: TObject);
begin
  if cbSID.Checked then
    lbSID.Caption := 'SID'
  else
    lbSID.Caption := 'Service Name';
end;

procedure TSSLClientFrame.sbCACertNameClick(Sender: TObject);
begin
  OpenDialog.Filter := CertFilter;
  OpenDialog.Title := 'Import certificate';
  if OpenDialog.Execute then
    edCACertName.Text := OpenDialog.FileName;
end;

procedure TSSLClientFrame.sbCertNameClick(Sender: TObject);
begin
  OpenDialog.Filter := CertFilter;
  OpenDialog.Title := 'Import certificate';
  if OpenDialog.Execute then
    edCertName.Text := OpenDialog.FileName;
end;

procedure TSSLClientFrame.sbKeyNameClick(Sender: TObject);
begin
  OpenDialog.Filter := KeyFilter;
  OpenDialog.Title := 'Import key';
  if OpenDialog.Execute then
    edKeyName.Text := OpenDialog.FileName;
end;

procedure TSSLClientFrame.ShowCertControls(Mode: Boolean);
begin
  edCACertName.Enabled := Mode;
  sbCACertName.Enabled := Mode;
  edCertName.Enabled := Mode;
  sbCertName.Enabled := Mode;
  edKeyName.Enabled := Mode;
  sbKeyName.Enabled := Mode;

  edWallet.Enabled := not(Mode);
  sbWallet.Enabled := not(Mode);
end;

procedure TSSLClientFrame.ChangeDatabaseControl(ProviderName: String);
var
  ThisIsOracle: Boolean;
begin
  ThisIsOracle := ProviderName = 'Oracle';
  if ThisIsOracle then begin
    lbDBUserName.Top := lbDBServer.Top + 78;
    edDBUserName.Top := lbDBServer.Top + 74;
    lbDBPassword.Top := lbDBServer.Top + 104;
    edDBPassword.Top := lbDBServer.Top + 100;
    lbDBDatabase.Top := lbDBServer.Top + 130;
    cbDBDatabase.Top := lbDBServer.Top + 126;
  end
  else begin
    lbDBUserName.Top := lbDBServer.Top + 52;
    edDBUserName.Top := lbDBServer.Top + 48;
    lbDBPassword.Top := lbDBServer.Top + 78;
    edDBPassword.Top := lbDBServer.Top + 74;
    lbDBDatabase.Top := lbDBServer.Top + 104;
    cbDBDatabase.Top := lbDBServer.Top + 100;
  end;
  pnOracleOptions.Visible := ThisIsOracle;
  pnAdvancedOptions.Top := pnOracleOptions.Top + Integer(pnOracleOptions.Visible) * pnOracleOptions.Height;
  lbSID.Visible := ThisIsOracle;
  cbSID.Visible := ThisIsOracle;
  edDBServiceName.Visible := ThisIsOracle;
  lbConnectMode.Visible := ThisIsOracle;
  cbConnectMode.Visible := ThisIsOracle;
  lbDBDatabase.Visible := Not(ThisIsOracle);
  cbDBDatabase.Visible := Not(ThisIsOracle);
end;

procedure TSSLClientFrame.GetStoredParameters(ProviderName: String);
var
  Registry: TRegistry;
begin
  edDBHost.Text := '';
  seDBPort.Value := 0;
  edDBUserName.Text := '';
  cbDBDatabase.Text := '';
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    with Registry do begin
      if OpenKey(KeyPath + '\' + TSSLClientFrame.ClassName + '\' + ProviderName, False) then begin
        if ValueExists('DBHost') then
          edDBHost.Text := ReadString('DBHost');
        if ValueExists('DBPort') then
          seDBPort.Value := ReadInteger('DBPort');
        if seDBPort.Value = 0 then
          seDBPort.Value := GetDefaultPort(ProviderName);
        if ValueExists('DBUserName') then
          edDBUserName.Text := ReadString('DBUserName');
        if ValueExists('DBDatabase') then
          cbDBDatabase.Text := ReadString('DBDatabase');

        if cbProvider.Text = 'Oracle' then begin
          if ValueExists('Wallet') then
            edWallet.Text := ReadString('Wallet');
          if ValueExists('ServerCertDN') then
            edServerCertDN.Text := ReadString('ServerCertDN');
          if ValueExists('DBServiceName') then
            edDBServiceName.Text := ReadString('DBServiceName');
          if ValueExists('Use SID') then
            cbSID.Checked := ReadBool('Use SID');
          if ValueExists('ConnectMode') then
            cbConnectMode.ItemIndex := ReadInteger('ConnectMode');
        end;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TSSLClientFrame.GetDefaultPort(ProviderName: String): Integer;
begin
  Result := 3050;
  if ProviderName = 'MySQL' then
    Result := 3306;
  if ProviderName = 'Oracle' then
    Result := 1522;
  if ProviderName = 'PostgreSQL' then
    Result := 5432;
  if ProviderName = 'SQL Server' then
    Result := 1433;
end;

procedure TSSLClientFrame.rbWalletClick(Sender: TObject);
begin
  ShowCertControls(False or (cbProvider.Text<>'Oracle'));
end;

procedure TSSLClientFrame.rbCertificateClick(Sender: TObject);
begin
  ShowCertControls(True or (cbProvider.Text<>'Oracle'));
end;

procedure TSSLClientFrame.sbWalletClick(Sender: TObject);
begin
  OpenDialog.Filter := WalletFilter;
  OpenDialog.Title := 'Select wallet files';
  if OpenDialog.Execute then
    edWallet.Text := OpenDialog.FileName;
end;

end.