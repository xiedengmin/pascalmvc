unit CRSSLIOHandler;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

//{$I SecureBridgeVer.inc} // uncomment for using SecureBridge versions greater than 9.4.1

interface

uses
  SysUtils, Classes, ScVio,
{$IFNDEF SBRIDGE}
  CRVio, CRTypes, CRFunctions,
{$ELSE}
  ScTypes, ScFunctions, MemUtils,
{$ENDIF}
{$IFDEF MSWINDOWS}
  ScCryptoAPIStorage,
{$ENDIF}
  ScBridge,
{$IFDEF SB_DEMO_VER3}
  ScSSLExtensions,
{$ENDIF}
  ScSSLTypes, ScUtils, ScSSLClient;

{$I SecureBridgeVer.inc}

type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TCRSSLIOHandler = class (TCRIOHandler)
  private
    FCipherSuites: TScSSLCipherSuites;
    FProtocols: TScSSLProtocols;
    FStorage: TScStorage;
    FCertName: string;
    FCACertName: string;
    FOnServerCertificateValidation: {$IFDEF SB_DEMO_VER3}TScRemoteCertificateValidationEvent{$ELSE}TScRemoteCertificateValidation{$ENDIF};
    FClientHelloExtensions: TTLSHelloExtensions;
    FSecurityOptions: TScSSLSecurityOptions;
    FMinDHEKeyLength: integer;
    FCompression: TScCompression;
    FDisableInsertEmptyFragment: boolean;
    FSSLClient: TScSSLClient;

    function GetSecure: boolean;
    function CheckDefaultCipherSuites: boolean;
    procedure SetCipherSuites(Value: TScSSLCipherSuites);
    procedure SetStorage(Value: TScStorage);
    procedure SetSecurityOptions(Value: TScSSLSecurityOptions);
    procedure DoValidateServerCertDN(Sender: TObject;
      ServerCertificate: TScCertificate; CertificateList: TCRList; var Errors: TScCertificateStatusSet);
    procedure DoServerCertificateValidation(Sender: TObject;
      ServerCertificate: TScCertificate; CertificateList: TCRList; var Errors: TScCertificateStatusSet);

  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    class procedure SetIsSecure(Handle: TCRIOHandle; const Value: Boolean); override;
    class function GetIsSecure(Handle: TCRIOHandle): Boolean; override;
    class procedure Renegotiate(Handle: TCRIOHandle); override;
    function GetHandlerType: string; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Connect(const Server: string; const Port: integer;
      HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
      SSLOptions: TSSLOptions; SSHOptions: TSSHOptions;
      IPVersion: TIPVersion = ivIPv4): TCRIOHandle; override;
    procedure Disconnect(Handle: TCRIOHandle); override;

    class function ReadNoWait(Handle: TCRIOHandle; const buffer: TValueArr; offset, count: integer): integer; override;
    class function Read(Handle: TCRIOHandle; const buffer: TValueArr; offset, count: integer): integer; override;
    class function Write(Handle: TCRIOHandle; const buffer: TValueArr; offset, count: integer): integer; override;
    class function WaitForData(Handle: TCRIOHandle; Timeout: integer = -1): boolean; override;

    class function GetTimeout(Handle: TCRIOHandle): integer; override;
    class procedure SetTimeout(Handle: TCRIOHandle; Value: integer); override;

    property IsSecure: boolean read GetSecure;
    property ClientHelloExtensions: TTLSHelloExtensions read FClientHelloExtensions;

  published
    property CipherSuites: TScSSLCipherSuites read FCipherSuites write SetCipherSuites stored CheckDefaultCipherSuites;
    property Protocols: TScSSLProtocols read FProtocols write FProtocols default [spTls1, spTls11, spTls12];

    // MySQL server 5.1.73 uses 512 bit
    property MinDHEKeyLength: integer read FMinDHEKeyLength write FMinDHEKeyLength default 1024;

    property Compession: TScCompression read FCompression write FCompression default csNone;

    property DisableInsertEmptyFragment: boolean read FDisableInsertEmptyFragment write FDisableInsertEmptyFragment default True;
    property SecurityOptions: TScSSLSecurityOptions read FSecurityOptions write SetSecurityOptions;

    property Storage: TScStorage read FStorage write SetStorage;
    property CertName: string read FCertName write FCertName;
    property CACertName: string read FCACertName write FCACertName;

    property OnServerCertificateValidation: {$IFDEF SB_DEMO_VER3}TScRemoteCertificateValidationEvent{$ELSE}TScRemoteCertificateValidation{$ENDIF}
      read FOnServerCertificateValidation write FOnServerCertificateValidation;
  end;

implementation

uses
{$IFNDEF ODBC_DRIVER}
{$IFNDEF SBRIDGE}
  CRAccess,
{$ENDIF}
  CRSsoStorage,
{$ELSE}
  ODBC_SsoStorage,
{$ENDIF}
  ScConsts;

{ TCRSSLIOHandler }

constructor TCRSSLIOHandler.Create(AOwner: TComponent);
begin
  inherited;

  FClientHelloExtensions := TTLSHelloExtensions.Create;

  FCipherSuites := TScSSLCipherSuites.Create(Self{$IFNDEF SB_DEMO_VER3}, TScSSLCipherSuiteItem{$ENDIF});
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_256_GCM_SHA384;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_128_GCM_SHA256;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_256_CBC_SHA;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_128_CBC_SHA;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_256_CBC_SHA384;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_128_CBC_SHA256;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_3DES_168_CBC_SHA;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_256_GCM_SHA384;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_128_GCM_SHA256;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_256_CBC_SHA;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_128_CBC_SHA;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_256_CBC_SHA256;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_128_CBC_SHA256;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_3DES_168_CBC_SHA;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_256_GCM_SHA384;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_128_GCM_SHA256;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_256_CBC_SHA;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_128_CBC_SHA;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_256_CBC_SHA256;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_128_CBC_SHA256;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_3DES_168_CBC_SHA;

  FProtocols := [spTls1, spTls11, spTls12];
  FSecurityOptions := TScSSLSecurityOptions.Create(FClientHelloExtensions);
  FMinDHEKeyLength := 1024;
  FDisableInsertEmptyFragment := True;
  FCompression := csNone;
end;

destructor TCRSSLIOHandler.Destroy;
{$IFNDEF SBRIDGE}
{$IFNDEF ODBC_DRIVER}
var
  con: TCRConnection;
  i: integer;
{$ENDIF}
{$ENDIF}
begin
{$IFNDEF SBRIDGE}
{$IFNDEF ODBC_DRIVER}
  for i := 0 to FList.Count - 1 do begin
    if IsClass(TObject(FList[i]), TCRConnection) then begin
      con := TCRConnection(FList[i]);
      if con.GetConnected then
        con.Disconnect;
    end;
  end;
{$ENDIF}
{$ENDIF}

  FClientHelloExtensions.Free;
  FCipherSuites.Free;
  FSecurityOptions.Free;

  inherited;
end;

function TCRSSLIOHandler.GetHandlerType: string;
begin
  Result := 'ssl';
end;

function TCRSSLIOHandler.Connect(const Server: string; const Port: integer;
  HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
  SSLOptions: TSSLOptions; SSHOptions: TSSHOptions;
  IPVersion: TIPVersion = ivIPv4): TCRIOHandle;

{$IFDEF ODBC_DRIVER}
  function CreateSsoFileStorage(const WalletPath: string; SSLClient: TScSSLClient): TCRSsoFileStorage;
  begin
    Result := TCRSsoFileStorage.Create(SSLClient);
    try
      Result.SetFullWalletPath(WalletPath);
    except
      Result.Free;
      raise;
    end;
  end;

{$IFDEF MSWINDOWS}
  function CreateSsoRegStorage(const WalletReg: string; SSLClient: TScSSLClient): TCRSsoRegStorage;
  begin
    Result := TCRSsoRegStorage.Create(SSLClient);
    try
      Result.SetFullWalletRegPath(WalletReg);
    except
      Result.Free;
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

var
  SSLClient: TScSSLClient;
  Cert: TScCertificate;
begin
  SSLClient := TScSSLClient.Create(nil);
  try
    if HttpOptions <> nil then
      SSLClient.HttpOptions.Assign(HttpOptions);
    if ProxyOptions <> nil then
      SSLClient.ProxyOptions.Assign(ProxyOptions);
    SSLClient.HostName := Server;
    SSLClient.Port := Port;
    SSLClient.CipherSuites := CipherSuites;
    SSLClient.Protocols := Protocols;
    SSLClient.MinDHEKeyLength := MinDHEKeyLength;
    SSLClient.Compression := Compession;
    SSLClient.DisableInsertEmptyFragment := DisableInsertEmptyFragment;
    SSLClient.ClientHelloExtensions.Assign(ClientHelloExtensions);
  {$IFNDEF ODBC_DRIVER}
    SSLClient.SecurityOptions.Assign(SecurityOptions);
    if SSLOptions.ForceUseTrustServerCertificate then
      SSLClient.SecurityOptions.TrustServerCertificate := SSLOptions.TrustServerCertificate; //for VerifyCA, VerifyFull SSL mode PgDAC
  {$ELSE}
    SSLClient.SecurityOptions.IgnoreServerCertificateValidity := SSLOptions.IgnoreServerCertificateValidity;
    SSLClient.SecurityOptions.IgnoreServerCertificateConstraints := SSLOptions.IgnoreServerCertificateConstraints;
    SSLClient.SecurityOptions.IgnoreServerCertificateInsecurity := SSLOptions.IgnoreServerCertificateInsecurity; //RDS
    SSLClient.SecurityOptions.TrustServerCertificate := SSLOptions.TrustServerCertificate;
  {$ENDIF}
  {$IFDEF SB_DEMO_VER2}
    SSLClient.SecurityOptions.TrustSelfSignedCertificate := SSLOptions.TrustSelfSignedCertificate;
  {$ENDIF}
    SSLClient.SecurityOptions.IdentityDNSName := SSLOptions.IdentityDNSName;
    SSLClient.SecurityOptions.ServerCertDN := SSLOptions.ServerCertDN;

  {$IFDEF ODBC_DRIVER}
    if SSLOptions.WalletPath <> '' then
      SSLClient.Storage := CreateSsoFileStorage(SSLOptions.WalletPath, SSLClient)
    else
  {$IFDEF MSWINDOWS}
    if SSLOptions.WalletReg <> '' then
      SSLClient.Storage := CreateSsoRegStorage(SSLOptions.WalletReg, SSLClient)
    else
  {$ENDIF}
  {$ENDIF}
    if Storage <> nil then begin
      SSLClient.Storage := Storage;
      SSLClient.CACertName := CACertName;
      SSLClient.CertName := CertName;
    end
    else begin
      SSLClient.Storage := TScMemoryStorage.Create(SSLClient);
      SSLClient.CACertName := '';
      SSLClient.CertName := '';
    end;

    if (SSLClient.CACertName = '') and (SSLOptions.CA <> '') then begin
      Cert := TScCertificate.Create(SSLClient.Storage.Certificates);
      Cert.CertName := 'cacert' + IntToStr(SSLClient.Storage.Certificates.Count);
      Cert.ImportFrom(SSLOptions.CA);
      SSLClient.CACertName := Cert.CertName;
    end;

    if (SSLClient.CertName = '') and (SSLOptions.Cert <> '') then begin
      Cert := TScCertificate.Create(SSLClient.Storage.Certificates);
      Cert.CertName := 'cert' + IntToStr(SSLClient.Storage.Certificates.Count);
      Cert.ImportFrom(SSLOptions.Cert);
      if SSLOptions.Key <> '' then
        Cert.Key.ImportFrom(SSLOptions.Key);

      SSLClient.CertName := Cert.CertName;
    end;

    if (SSLClient.CACertName <> '') and not SSLClient.SecurityOptions.IgnoreServerCertificateInsecurity then
      SSLClient.SecurityOptions.IgnoreServerCertificateInsecurity := True; //RDS

    SSLClient.IPVersion := ScVio.TIPVersion(IPVersion);
    SSLClient.OnServerCertificateValidation := DoServerCertificateValidation;
    SSLClient.Connect;
  except
    SSLClient.Free;
    raise;
  end;

  FSSLClient := SSLClient;
  Result := SSLClient;
end;

procedure TCRSSLIOHandler.Disconnect(Handle: TCRIOHandle);
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Client.Connected := False;
  FSSLClient := nil;
end;

class procedure TCRSSLIOHandler.Renegotiate(Handle: TCRIOHandle);
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Client.Renegotiate;
end;

class function TCRSSLIOHandler.ReadNoWait(Handle: TCRIOHandle; const buffer: TValueArr; offset, count: integer): integer;
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Result := Client.ReadNoWait(buffer[offset], count);
end;

class function TCRSSLIOHandler.Read(Handle: TCRIOHandle; const buffer: TValueArr; offset, count: integer): integer;
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Result := Client.ReadBuffer(buffer[offset], count);
end;

class function TCRSSLIOHandler.Write(Handle: TCRIOHandle;
  const buffer: TValueArr; offset, count: integer): integer;
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Result := Client.WriteBuffer(buffer[offset], count);
end;

class function TCRSSLIOHandler.WaitForData(Handle: TCRIOHandle; Timeout: integer = -1): boolean;
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Result := Client.WaitForData(Timeout);
end;

class function TCRSSLIOHandler.GetTimeout(Handle: TCRIOHandle): integer;
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Result := Client.Timeout;
end;

class procedure TCRSSLIOHandler.SetTimeout(Handle: TCRIOHandle; Value: integer);
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Client.Timeout := Value;
end;

procedure TCRSSLIOHandler.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FStorage) and (Operation = opRemove) then
    Storage := nil;

  inherited;
end;

class procedure TCRSSLIOHandler.SetIsSecure(Handle: TCRIOHandle; const Value: Boolean);
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Client.IsSecure := Value;
end;

class function TCRSSLIOHandler.GetIsSecure(Handle: TCRIOHandle): Boolean;
var
  Client: TScSSLClient;
begin
  Client := Handle as TScSSLClient;
  Result := Client.IsSecure;
end;

function TCRSSLIOHandler.GetSecure: Boolean;
begin
  if FSSLClient = nil then
    Result := False
  else
    Result := FSSLClient.IsSecure;
end;

function TCRSSLIOHandler.CheckDefaultCipherSuites: boolean;
begin
  Result := not ((FCipherSuites.Count = 21) and
    (((FCipherSuites.Items[0] as TScSSLCipherSuiteItem).CipherAlgorithm = caECDHE_RSA_AES_256_GCM_SHA384) and
     ((FCipherSuites.Items[1] as TScSSLCipherSuiteItem).CipherAlgorithm = caECDHE_RSA_AES_128_GCM_SHA256) and
     ((FCipherSuites.Items[2] as TScSSLCipherSuiteItem).CipherAlgorithm = caECDHE_RSA_AES_256_CBC_SHA) and
     ((FCipherSuites.Items[3] as TScSSLCipherSuiteItem).CipherAlgorithm = caECDHE_RSA_AES_128_CBC_SHA) and
     ((FCipherSuites.Items[4] as TScSSLCipherSuiteItem).CipherAlgorithm = caECDHE_RSA_AES_256_CBC_SHA384) and
     ((FCipherSuites.Items[5] as TScSSLCipherSuiteItem).CipherAlgorithm = caECDHE_RSA_AES_128_CBC_SHA256) and
     ((FCipherSuites.Items[6] as TScSSLCipherSuiteItem).CipherAlgorithm = caECDHE_RSA_3DES_168_CBC_SHA) and
     ((FCipherSuites.Items[7] as TScSSLCipherSuiteItem).CipherAlgorithm = caDHE_RSA_AES_256_GCM_SHA384) and
     ((FCipherSuites.Items[8] as TScSSLCipherSuiteItem).CipherAlgorithm = caDHE_RSA_AES_128_GCM_SHA256) and
     ((FCipherSuites.Items[9] as TScSSLCipherSuiteItem).CipherAlgorithm = caDHE_RSA_AES_256_CBC_SHA) and
     ((FCipherSuites.Items[10] as TScSSLCipherSuiteItem).CipherAlgorithm = caDHE_RSA_AES_128_CBC_SHA) and
     ((FCipherSuites.Items[11] as TScSSLCipherSuiteItem).CipherAlgorithm = caDHE_RSA_AES_256_CBC_SHA256) and
     ((FCipherSuites.Items[12] as TScSSLCipherSuiteItem).CipherAlgorithm = caDHE_RSA_AES_128_CBC_SHA256) and
     ((FCipherSuites.Items[13] as TScSSLCipherSuiteItem).CipherAlgorithm = caDHE_RSA_3DES_168_CBC_SHA) and
     ((FCipherSuites.Items[14] as TScSSLCipherSuiteItem).CipherAlgorithm = caRSA_AES_256_GCM_SHA384) and
     ((FCipherSuites.Items[15] as TScSSLCipherSuiteItem).CipherAlgorithm = caRSA_AES_128_GCM_SHA256) and
     ((FCipherSuites.Items[16] as TScSSLCipherSuiteItem).CipherAlgorithm = caRSA_AES_256_CBC_SHA) and
     ((FCipherSuites.Items[17] as TScSSLCipherSuiteItem).CipherAlgorithm = caRSA_AES_128_CBC_SHA) and
     ((FCipherSuites.Items[18] as TScSSLCipherSuiteItem).CipherAlgorithm = caRSA_AES_256_CBC_SHA256) and
     ((FCipherSuites.Items[19] as TScSSLCipherSuiteItem).CipherAlgorithm = caRSA_AES_128_CBC_SHA256) and
     ((FCipherSuites.Items[20] as TScSSLCipherSuiteItem).CipherAlgorithm = caRSA_3DES_168_CBC_SHA)
    ));
end;

procedure TCRSSLIOHandler.SetCipherSuites(Value: TScSSLCipherSuites);
begin
  if Value <> FCipherSuites then
    FCipherSuites.Assign(Value);
end;

procedure TCRSSLIOHandler.SetStorage(Value: TScStorage);
begin
  if FStorage <> Value then begin
    if FStorage <> nil then
      FStorage.RemoveFreeNotification(Self);

    FStorage := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TCRSSLIOHandler.SetSecurityOptions(Value: TScSSLSecurityOptions);
begin
  FSecurityOptions.Assign(Value);
end;

procedure TCRSSLIOHandler.DoValidateServerCertDN(Sender: TObject;
  ServerCertificate: TScCertificate; CertificateList: TCRList; var Errors: TScCertificateStatusSet);
var
  i: Integer;
  SSLClient: TScSSLClient;
  RemoteCertDN: string;
  ServerCertDN: string;
  RemoteCertDNList: TStringList;
  ServerCertDNList: TStringList;
  RemoteAttr: string;
  ServerAttr: string;
begin
  SSLClient := Sender as TScSSLClient;

  ServerCertDN := SSLClient.SecurityOptions.ServerCertDN;
  if ServerCertDN = '' then
    Exit;

  if ServerCertificate = nil then begin
    Errors := Errors + [csInvalidSubjectName];
    Exit;
  end;

  if CertificateList = nil then begin
    Errors := Errors + [csUnknownCriticalExtension];
    Exit;
  end;

  RemoteCertDNList := TStringList.Create;
  try
    RemoteCertDN := ServerCertificate.SubjectName.ToString;
    RemoteCertDN := StringReplace(RemoteCertDN, ';', #13, [rfReplaceAll]);
    RemoteCertDNList.Text := RemoteCertDN;
    for i := 0 to RemoteCertDNList.Count - 1 do begin
      RemoteAttr := Trim(RemoteCertDNList.Strings[i]);
      RemoteCertDNList.Strings[i] := RemoteAttr;
    end;
    RemoteCertDNList.Sort;

    ServerCertDNList := TStringList.Create;
    try
      ServerCertDN := StringReplace(ServerCertDN, ',', #13, [rfReplaceAll]);
      ServerCertDNList.Text := ServerCertDN;
      for i := 0 to ServerCertDNList.Count - 1 do begin
        ServerAttr := Trim(ServerCertDNList.Strings[i]);
        ServerAttr := StringReplace(ServerAttr, 'ST=', 'S=', []);
        ServerCertDNList.Strings[i] := ServerAttr;
      end;
      ServerCertDNList.Sort;

      if RemoteCertDNList.Count <> ServerCertDNList.Count then begin
        Errors := Errors + [csInvalidSubjectName];
        Exit;
      end;

      for i := 0 to ServerCertDNList.Count - 1 do begin
        RemoteAttr := RemoteCertDNList.Strings[i];
        ServerAttr := ServerCertDNList.Strings[i];
        if RemoteAttr <> ServerAttr then begin
          Errors := Errors + [csInvalidSubjectName];
          Exit;
        end;
      end;
    finally
      ServerCertDNList.Free;
    end;
  finally
    RemoteCertDNList.Free;
  end;
end;

procedure TCRSSLIOHandler.DoServerCertificateValidation(Sender: TObject;
  ServerCertificate: TScCertificate; CertificateList: TCRList; var Errors: TScCertificateStatusSet);
begin
  DoValidateServerCertDN(Sender, ServerCertificate, CertificateList, Errors);

  if Assigned(FOnServerCertificateValidation) then
    FOnServerCertificateValidation(Self, ServerCertificate, CertificateList, Errors);
end;

end.