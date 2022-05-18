
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MySqlSessionUni;

interface

uses
  Types, SysUtils, Classes,
  CLRClasses, CRTypes, CRVio, CRVioTcp, CRHash,
{$IFDEF MSWINDOWS}
  CRSspi,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  MySqlNet, MyCall, MyRSAKey;
{$ELSE}
  MySqlNetUni, MyCallUni, MyRSAKeyUni;
{$ENDIF}

type
  TMyPluginType = (plOldPasword, plNative, plSha2_256, plCachingSha2, plWindows, plGSSAPI, plDialog, plOther);

  TRandStruct = record
    Seed1: Int64;
    Seed2: Int64;
    MaxValue: Int64;
    MaxValueDbl: double;
  end;

  TMySqlSession = class
  private
    FSkipPacket: boolean;
    FErrno: cardinal;
    FError: string;
    FIsServerError: boolean;
    FClientFlag: cardinal;
    FServerLanguage: integer;
    FScrambleBuff: TBytes;
    FAuthPluginName: string;
    FPacketType: TMyPacketType;

    FIsClosed: boolean;
    FIPVersion: TIPVersion;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FResultSet: TObject; // used for FetchAll = False
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FIOHandler: TCRIOHandler;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FHttpOptions: THttpOptions;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FProxyOptions: TProxyOptions;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FSSLOptions: TSSLOptions;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FSSHOptions: TSSHOptions;

  {$IFDEF MSWINDOWS}
    FNTLMAuth: TCRNTLMAuth;
  {$ENDIF}
    FOnDialogAuthPlugin: TMyDialogAuthPluginEvent;

    class function PasswordHashStage1(const Password: string): TBytes;
    class procedure PasswordHashStage2(Salt: TValueArr; SaltOffset: integer; var Hash: TBytes);
    class procedure PasswordXor(Salt: TValueArr; SaltIndex: integer; const Password: TBytes; const Result: TBytes; Length: integer);
    class procedure PasswordXorSha2(const Salt: TBytes; const Password: TBytes; const Result: TBytes);

    procedure Scramble(const Password: string);
    class function MyRandom(var RandSt: TRandStruct): double;
    class procedure RandomInit(var RandSt: TRandStruct; Seed1, Seed2: Int64);
    class procedure HashPassword(const Result: TInt64DynArray; const Password: TBytes);
    procedure Scramble41(const Password: string);
    procedure ScrambleSha2(const Password: string);
    procedure EncryptPasswordSha2(const Password: string; const PublicKey: string; Padding: TPaddingMode);

    function IsProtocol41: boolean;

    procedure NativeAuthenticate(const Password: string);
    procedure OldPasswordAuthenticate(const Password: string; NeedReadBytes: boolean = False);
    procedure SHA256Authenticate(const Password: string);
    procedure CachingSHA2Authenticate(const Password: string);
    procedure PAMAuthenticate(const Password: string);
  {$IFDEF MSWINDOWS}
    procedure SSPIAuthenticate;
  {$ENDIF}
    procedure Authenticate(const Password: string);


    function GetPacketType: TMyPacketType;
    function GetPluginType: TMyPluginType;

    function UsedNativeAuth: boolean;
    function UsedOldPasswordAuth: boolean;
    function UsedSha256Auth: boolean;
    function UsedCachingSha2Auth: boolean;
    function UsedWindowsAuth: boolean;
    function UsedGSSAPIAuth: boolean;
    function UsedDialogAuth: boolean;

    procedure ProcessCachingSha2(const Password: string);
    procedure WriteClearPassword(const Password: string; WriteLen: boolean);
    procedure WritePasswordSha256(const Password: string);
    procedure ProcessSha256(const Password: string);

  public
    net: TMySqlNet;

    // session parameters
    host,
    user,
    password,
    database,
    serverVersion,
    hostInfo,
    info,
    charset: string;

    port,
    connectTimeout,
    commandTimeout,
    threadId,
    serverStatus,
    warningCount: integer;
    status: TMySqlStatus;
    protocolType: MYSQL_PROTOCOL_TYPE;
    serverCapabilities: cardinal;

    // result set
    fields: TMySqlFieldInfos;
    affectedRows: Int64;
    insertId: Int64;
    extraInfo: Int64;
    fieldCount: integer;

    constructor Create;
    destructor Destroy; override;

    procedure Close;
    procedure FreeQuery;
    procedure WriteCommand(command: TServerCommand);
    procedure SimpleCommand(command: TServerCommand; const args: TValueArr; Length: integer; skipCheck: boolean); overload;
    procedure SimpleCommand(command: TServerCommand; const args: TBytes; Length: integer; skipCheck: boolean); overload;
    procedure ReadQueryResult;

    procedure SetIsUTF8(Value: boolean);

    procedure Connect(host, user, passwd, db: string; port: cardinal; const unix_socket: string; clientflag: cardinal);
    procedure SelectDb(const Database: string);
    function Statistics: string;
    procedure Ping;

    property OnDialogAuthPlugin: TMyDialogAuthPluginEvent read FOnDialogAuthPlugin write FOnDialogAuthPlugin;

    property Protocol41: boolean read IsProtocol41;
    property Errno: cardinal read FErrno write FErrno;
    property Error: string read FError write FError;
    property IsServerError: boolean read FIsServerError write FIsServerError;
    property ResultSet: TObject read FResultSet write FResultSet; // used for FetchAll = False
    property SkipPacket: boolean read FSkipPacket write FSkipPacket;

    property IPVersion: TIPVersion read FIPVersion write FIPVersion;
    property IOHandler: TCRIOHandler read FIOHandler write FIOHandler;
    property HttpOptions: THttpOptions read FHttpOptions write FHttpOptions;
    property ProxyOptions: TProxyOptions read FProxyOptions write FProxyOptions;
    property SSLOptions: TSSLOptions read FSSLOptions write FSSLOptions;
    property SSHOptions: TSSHOptions read FSSHOptions write FSSHOptions;
  end;

implementation

uses
  Math,
{$IFDEF MSWINDOWS}
  {$IFNDEF UNIDACPRO}MySqlVioPipe{$ELSE}MySqlVioPipeUni{$ENDIF},
{$ENDIF}
{$IFDEF HAVE_OPENSSL}
  CRVioTcpSSL,
{$ENDIF}
{$IFNDEF LITE}
  CRVioHttp,
{$ENDIF}
  CRFunctions,
{$IFNDEF UNIDACPRO}
  MySqlErrors, MySqlResultSet;
{$ELSE}
  MySqlErrorsUni, MySqlResultSetUni;
{$ENDIF}

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

const
  ScrambleLength = 8;
  Scramble41Length = 20;

constructor TMySqlSession.Create;
begin
  inherited;

  net := TMySqlNet.Create;
  SetLength(FScrambleBuff, Scramble41Length);
{$IFDEF MSWINDOWS}
  FNTLMAuth := TCRNTLMAuth.Create;
{$ENDIF}
end;

destructor TMySqlSession.Destroy;
begin
{$IFDEF MSWINDOWS}
  FNTLMAuth.Free;
{$ENDIF}
  net.Free;
  net := nil;


  inherited;
end;

procedure TMySqlSession.Close;
begin
  if FIsClosed then
    Exit;

  FIsClosed := True;

  // FreeQuery; - optimization for FetchAll = False
  status := msReady;
  try
    SimpleCommand(scQuit, TValueArr(nil), 0, True);
  finally
    net.Close;
  end;
end;

procedure TMySqlSession.FreeQuery;
var
  row0: byte;
  len: integer;
begin
  if (status <> msReady) and (FError = '' {nonsense on error processing}) then begin
    try
      if net = nil then
        Exit;
      if net.Compress then begin
        net.ReceiveRow;
        while (net.Length > 8) or (net.ReadByte <> 254) do
          net.ReceiveRow;
      end
      else begin
        len := net.SkipRow(row0);
        while (len > 8) or (row0 <> 254) do
          len := net.SkipRow(row0);
      end;
    finally
      status := msReady;
      if SkipPacket then begin
        net.ReceiveRow;
        SkipPacket := False;
      end;
    end;
  end;
end;

procedure TMySqlSession.WriteCommand(command: TServerCommand);
begin
  if status <> msReady then
    raise EMySqlException.Create(CR_COMMANDS_OUT_OF_SYNC);

  info := '';
  affectedRows := 0;

  net.NewCommand;
  net.WriteByte(byte(command));
end;


procedure TMySqlSession.SimpleCommand(command: TServerCommand; const args: TValueArr; Length: integer; skipCheck: boolean);
begin
  if net.vio = nil then
    Exit;

  net.vio.ReceiveTimeout := commandTimeout;

  WriteCommand(command);
  if args <> nil then
    net.WriteOrSendBytes(args, 0, Length);
  net.Send;

  if not skipCheck then
    net.ReceiveRow;
end;

procedure TMySqlSession.SimpleCommand(command: TServerCommand; const args: TBytes; Length: integer; skipCheck: boolean);
begin
  SimpleCommand(command, TValueArr(@args[0]), Length, skipCheck);
end;

procedure TMySqlSession.ReadQueryResult;

  procedure SendFileToServer(FileName: string);
    function MY_ALIGN(const A, L: integer): integer;
    begin
      Result := (A + L - 1) and not(L - 1);
    end;
  const
    IO_SIZE = 4096;
  var
    buf: TBytes;
    f: TFileStream;
    c: Int64;
  begin
    f := nil;
    try
      net.Clear;
      f := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
      SetLength(buf, MY_ALIGN(BUFFER_LENGTH - 16, IO_SIZE));

      c := f.Read(buf[0], Length(buf));
      while c > 0 do begin
        net.WriteBytes(buf, 0, c);
        net.Send;
        c := f.Read(buf[0], Length(buf));
      end;
    finally
      f.Free;

      // Send empty packet to mark end of file
      net.SendEmpty;
      net.ReceiveRow;
    end;
  end;

var
  FieldsResult: TMySqlResultSet;
  OldPacketPos: integer;

begin
  serverStatus := SERVER_STATUS_AUTOCOMMIT; // reset previous state
  
  net.ReceiveRow;

  while True do begin
    OldPacketPos := net.Position;
    fieldCount := integer(net.ReadFieldLength);
    if fieldCount = 0 then begin
      affectedRows := net.ReadFieldLength;
      insertId := net.ReadFieldLength;
      if IsProtocol41 or ((serverCapabilities and CLIENT_TRANSACTIONS) <> 0) then
        serverStatus := net.ReadInt16;
      if IsProtocol41 then
        warningCount := net.ReadInt16;
      info := string(net.ReadString);
      Exit;
    end
    else if fieldCount = {$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.NULL_LENGTH then begin // LOAD DATA LOCAL INFILE
      net.Position := OldPacketPos + 1;
      SendFileToServer(string(net.ReadString));
    end
    else
      Break;
  end;
  if (serverStatus and SERVER_STATUS_AUTOCOMMIT) = 0 then
    serverStatus := serverStatus or SERVER_STATUS_IN_TRANS;
  if net.Length > net.Position then
    extraInfo := net.ReadFieldLength; (* Maybe number of rec *)

  if IsProtocol41 then
    FieldsResult := TMySqlResultSet.Create(Self, nil, 7)
  else
    FieldsResult := TMySqlResultSet.Create(Self, nil, 5);
  try
    fields := FieldsResult.ReadFieldsInfo(fieldCount);
    status := msGetResult;
    warningCount := 0;
  finally
    FieldsResult.Free;
  end;
end;

function TMySqlSession.IsProtocol41: boolean;
begin
  Result := ((serverCapabilities and CLIENT_PROTOCOL_41) <> 0) and ((FClientFlag and CLIENT_PROTOCOL_41) <> 0);
end;

function TMySqlSession.GetPacketType: TMyPacketType;
begin
  Result := net.GetPacketType(True);
  if Result = ptAuthSwitchResponse then begin
    net.Position := 1;
    FAuthPluginName := string(net.ReadString);
  end;
end;

function TMySqlSession.GetPluginType: TMyPluginType;
var
  lcAuthPluginName: string;
begin
  Result := plOther;
  if FAuthPluginName <> '' then begin
    lcAuthPluginName := LowerCase(FAuthPluginName);
    if Pos('native', lcAuthPluginName) <> 0 then
      Result := plNative
    else if Pos('old', lcAuthPluginName) <> 0 then
      Result := plOldPasword
    else if Pos('sha256', lcAuthPluginName) <> 0 then
      Result := plSha2_256
    else if Pos('caching_sha2', lcAuthPluginName) <> 0 then
      Result := plCachingSha2
    else if (Pos('windows', lcAuthPluginName) <> 0) then
      Result := plWindows
    else if (Pos('gssapi', lcAuthPluginName) <> 0) then
      Result := plGSSAPI
    else if Pos('dialog', lcAuthPluginName) <> 0 then
      Result := plDialog;
  end;
end;

function TMySqlSession.UsedCachingSha2Auth: boolean;
begin
  Result := GetPluginType = plCachingSha2;
end;

function TMySqlSession.UsedSha256Auth: boolean;
begin
  Result := GetPluginType = plSha2_256;
end;

function TMySqlSession.UsedNativeAuth: boolean;
begin
  Result := GetPluginType = plNative;
end;

function TMySqlSession.UsedOldPasswordAuth: boolean;
begin
  Result := GetPluginType = plOldPasword;
end;

function TMySqlSession.UsedWindowsAuth: boolean;
begin
  Result := GetPluginType = plWindows;
end;

function TMySqlSession.UsedGSSAPIAuth: boolean;
begin
  Result := GetPluginType = plGSSAPI;
end;

function TMySqlSession.UsedDialogAuth: boolean;
begin
  Result := GetPluginType = plDialog;
end;

procedure TMySqlSession.ProcessCachingSha2(const Password: string);
var
  Major, Minor, Release: integer;
  IsMariaDB: boolean;
  PublicKey: string;
begin
  if (net.Length = 2) and (byte(net.Row[1]) = 3) then // Fast authentication
    net.ReceiveRow
  else
  if (net.Length = 2) and (byte(net.Row[1]) = 4) then begin // Full authentication
    if protocolType = MYSQL_PROTOCOL_SSL then begin // send as clear text, since the channel is already encrypted
      net.Clear;
      net.WriteString(Password);
      net.Send;
      net.ReceiveRow;
    end
    else begin
      net.Clear;
      net.WriteByte(2); // we ask server to send public key
      net.Send;
      net.ReceiveRow;
      if GetPacketType = ptAuthMoreData then begin // $01 indicates that the server sent data
        net.Position := net.Position + 1;
        PublicKey := string(net.ReadString);
        net.Clear;
        DecodeVersion(serverVersion, Major, Minor, Release, IsMariaDB);

        if (Major > 8) or ((Major = 8) and (Minor > 0)) or ((Major = 8) and (Minor = 0) and (Release >= 5)) then //  Prior to server 8.0.5 the encryption was done using RSA_PKCS1_PADDING.  With 8.0.5 it is done with RSA_PKCS1_OAEP_PADDING.
          EncryptPasswordSha2(Password, PublicKey, pmOAEP) // Encrypt the Password
        else
          EncryptPasswordSha2(Password, PublicKey, pmPKCS2); // Encrypt the Password
        net.Send;
        net.ReceiveRow;
      end;
    end;
  end;
end;

procedure TMySqlSession.WriteClearPassword(const Password: string; WriteLen: boolean);
var
  PasswordBuff: TBytes;
begin
  PasswordBuff := Encoding.Default.GetBytes(Password + #0);
  if WriteLen then
    net.WriteByte(byte(Length(PasswordBuff)));
  net.WriteBytes(PasswordBuff);
end;

procedure TMySqlSession.WritePasswordSha256(const Password: string);
begin
  if protocolType = MYSQL_PROTOCOL_SSL then // send as clear text, since the channel is already encrypted
    WriteClearPassword(Password, True)
  else if Password = '' then
    net.WriteByte(0)
  else begin
    // we ask server to send public key
    net.WriteByte(1);
    net.WriteByte(1);
  end;
end;

procedure TMySqlSession.ProcessSha256(const Password: string); // works only when (Packet = AuthMoreData) and Sha2_256
var
  PublicKey: string;
begin
  net.Position := net.Position + 1;
  PublicKey := string(net.ReadString);
  net.Clear;
  EncryptPasswordSha2(Password, PublicKey, pmOAEP); // Encrypt the Password
  net.Send;
  net.ReceiveRow;
end;

procedure TMySqlSession.NativeAuthenticate(const Password: string);
var
  PacketLen, PacketPos: integer;
  RowBuffer: TValueArr;
  TmpBuf, PasswordHash: TBytes;
begin
  SetLength(TmpBuf, 0);
  SetLength(PasswordHash, Scramble41Length);
  PacketLen := net.Length;
  PacketPos := net.Position;
  RowBuffer := net.Row;
  if (PacketLen = 24) and (Byte(RowBuffer[PacketPos]) <> 0) then begin
    net.Clear;
    if Byte(RowBuffer[PacketPos]) <> Byte('*') then begin
      TmpBuf := PasswordHashStage1(Password);
      Buffer.BlockCopy(TmpBuf, 0, PasswordHash, 0, Scramble41Length);
      PasswordHashStage2(RowBuffer, PacketPos, PasswordHash);
      PasswordXor(RowBuffer, PacketPos + 4, PasswordHash, FScrambleBuff, Scramble41Length);
      PasswordXor(@FScrambleBuff[0], 0, TmpBuf, TmpBuf, Scramble41Length);
      net.WriteBytes(TmpBuf, 0, Scramble41Length);
    end
    else begin
      PasswordXor(RowBuffer, PacketPos + 4, PasswordHash, FScrambleBuff, Scramble41Length);
      Scramble(Password);
    end;
  end
  else begin
    net.ReadBytes(FScrambleBuff, 0, net.Length - net.Position - 1);
    net.Clear;
    Scramble41(Password);
  end;

  net.Send;
  net.ReceiveRow;
end;

procedure TMySqlSession.OldPasswordAuthenticate(const Password: string; NeedReadBytes: boolean = False);
begin
  if NeedReadBytes then
    net.ReadBytes(FScrambleBuff, 0, net.Length - net.Position - 1);
  net.Clear;
  Scramble(Password);
  net.Send;
  net.ReceiveRow;
end;

procedure TMySqlSession.SHA256Authenticate(const Password: string);
begin
  if Password = '' then begin
    net.Clear;
    net.WriteByte(0);
    net.Send;
    net.ReceiveRow;
  end
  else if protocolType = MYSQL_PROTOCOL_SSL then begin// send as clear text, since the channel is already encrypted
    net.Clear;
    WriteClearPassword(Password, False);
    net.Send;
    net.ReceiveRow;
  end
  else begin
    net.ReadBytes(FScrambleBuff, 0, net.Length - net.Position - 1);
    net.Clear;
    net.WriteByte(1);  // we ask server to send public key
    net.Send;
    net.ReceiveRow;
    FPacketType := GetPacketType;
    if (FPacketType = ptAuthMoreData) and UsedSha256Auth then
      ProcessSha256(Password);
  end;
end;

procedure TMySqlSession.CachingSHA2Authenticate(const Password: string);
begin
  net.ReadBytes(FScrambleBuff, 0, net.Length - net.Position - 1);
  net.Clear;
  ScrambleSha2(Password);
  net.Send;
  net.ReceiveRow;
  FPacketType := GetPacketType;
  if (FPacketType = ptAuthMoreData) and UsedCachingSha2Auth then
    ProcessCachingSha2(Password);
end;

procedure TMySqlSession.PAMAuthenticate(const Password: string);
var
  QuestionType: byte;
  Response, Question: string;
begin
  QuestionType := net.ReadByte;
  repeat
    if (QuestionType <> PASSWORD_QUESTION) and (QuestionType <> LAST_PASSWORD) then
      raise EMySqlException.Create(Format('Authentication plugin %s reported error: %s',
        [FAuthPluginName, 'Unsupported dialog plugin question: ' + IntToStr(QuestionType)]))
    else
      Question := string(net.ReadString);

    if Assigned(OnDialogAuthPlugin) then begin
      FOnDialogAuthPlugin(Question, Response)
    end else
      Response := Password;
    net.Clear;

    WriteClearPassword(Response, False);
    net.Send;
    net.ReceiveRow;
    QuestionType := net.ReadByte;
  until QuestionType = 0;
end;

{$IFDEF MSWINDOWS}
procedure TMySqlSession.SSPIAuthenticate;
var
  ClientAuthData,
  ServerAuthData: TBytes;
  LastError: integer;
begin
  net.ReadString;
{$IFNDEF VER9P}
  SetLength(ServerAuthData, 0); // anti-warning Delphi 6
{$ENDIF}
  LastError := FNTLMAuth.Initialize;
  if LastError <> 0 then
    raise Exception.Create(SysErrorMessage(LastError));

  LastError := FNTLMAuth.StartAuthentication(ClientAuthData);
  if LastError <> 0 then
    raise Exception.Create(SysErrorMessage(LastError));

  net.Clear;
  net.WriteBytes(ClientAuthData);
  net.Send;
  net.ReceiveRow;

  if UsedWindowsAuth then
    net.ReadByte; //only for MySQL

  SetLength(ServerAuthData, net.Length - net.Position);
  net.ReadBytes(ServerAuthData, 0, net.Length - net.Position);
  LastError := FNTLMAuth.FinishAuthentication(ServerAuthData, ClientAuthData);
  if LastError <> 0 then
    raise Exception.Create(SysErrorMessage(LastError));

  net.Clear;
  net.WriteBytes(ClientAuthData);
  net.Send;
  net.ReceiveRow;
end;
{$ENDIF}

procedure TMySqlSession.Authenticate(const Password: string);
begin
  net.ReceiveRow;
  FPacketType := GetPacketType;
  if ((serverCapabilities and CLIENT_SECURE_CONNECTION) <> 0) and (FPacketType <> ptOk) then begin
    case FPacketType of
      ptOldAuthSwitchRequest:
        OldPasswordAuthenticate(Password);
      ptAuthSwitchResponse: begin
        if UsedNativeAuth then
          NativeAuthenticate(Password)
        else if UsedOldPasswordAuth then
          OldPasswordAuthenticate(Password, True)
        else if UsedSha256Auth then
          SHA256Authenticate(Password)
        else if UsedCachingSha2Auth then
          CachingSHA2Authenticate(Password)
        else if UsedDialogAuth then
          PAMAuthenticate(Password)
        else if UsedWindowsAuth or UsedGSSAPIAuth then
        {$IFDEF MSWINDOWS}
          SSPIAuthenticate
        {$ELSE}
          raise EMySqlException.Create(Format('Unsupported auth method: %s', [FAuthPluginName]))
        {$ENDIF}
        else
          raise EMySqlException.Create(Format('Unsupported auth method: %s', [FAuthPluginName]));
      end;
      ptAuthMoreData: begin
        if UsedCachingSha2Auth then
          ProcessCachingSha2(Password)
        else
        if UsedSha256Auth then
          ProcessSha256(Password);
      end;
    else
      raise EMySqlException.Create(Format('Unknown paket type: %s', [IntToStr(Byte(net.Row[0]))]));
    end;
  end;
end;

procedure TMySqlSession.SetIsUTF8(Value: boolean);
begin
  if net <> nil then
    net.IsUTF8 := Value;
end;

procedure TMySqlSession.Connect(host, user, passwd, db: string; port: cardinal; const unix_socket: string; clientflag: cardinal);

  procedure InitHandshakeResponse;
  var
    zeroBytes: TBytes;
    charsetid: integer;
  begin
    net.Clear;
    if (clientflag and CLIENT_PROTOCOL_41) <> 0 then begin
      net.WriteInt32(clientflag);
      net.WriteInt32({$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.MAX_ALLOWED_PACKET);
      charsetid := GetCharSetID(charset);

      if charsetid <> 0 then
        net.WriteByte(charsetid) // used charset
      else
        net.WriteByte(8); // latin1 charset
      SetLength(zeroBytes, 23);
      FillChar(zeroBytes[0], Length(zeroBytes), 0);
      net.WriteBytes(zeroBytes);
    end
    else begin
      net.WriteInt16(clientflag);
      net.WriteInt24({$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.MAX_ALLOWED_PACKET and $ffff);
    end;
  end;

  procedure SwithchToSSLIfNeed;
  begin
    if (clientflag and CLIENT_SSL) <> 0 then begin
      //??? struct st_mysql_options *options= &mysql->options;
      net.Send;
      // Do the SSL layering
      if net.vio is TCRVioHandler then
        TCRVioHandler(net.vio).IsSecure := True
      else
      {$IFDEF HAVE_OPENSSL}
        if net.vio is TCRVioTcpSSL then
          TCRVioTcpSSL(net.vio).IsSecure := True;
      {$ELSE}
        raise EMySqlException.Create(CONN_UNKNOW_PROTOCOL, 'Can''t create SSL connection');
      {$ENDIF}

      InitHandshakeResponse;
    end;
  end;

var
  AuthPluginDataLen, ServerVersionNo: byte;
  PacketLen, PacketPos: integer;
  Msg: string;
begin
  if host = '' then
    host := Self.host;
  if user = '' then
    user := Self.user;
  if passwd = '' then
    passwd := Self.password;
  if db = '' then
    db := Self.database;
  if port = 0 then
    port := Self.port;

  if port = 0 then
    port := MYSQL_PORT;
  if host = '' then
    host := LOCAL_HOST;

  serverStatus := SERVER_STATUS_AUTOCOMMIT;
  if (FIOHandler <> nil) and FSSLOptions.Enabled then begin
    net.vio := TCRVioHandler.Create(host, port, FIOHandler, FHttpOptions, FProxyOptions, FSSLOptions, FSSHOptions, IPVersion);
    hostInfo := host + ' via ' + FIOHandler.ClassName;
  end
  else
  if protocolType = MYSQL_PROTOCOL_SSL then begin
  {$IFNDEF LITE}
    if (FHttpOptions <> nil) and FHttpOptions.Enabled then
      raise EMySqlException.Create(CONN_UNKNOW_PROTOCOL, 'Can''t create SSL connection via HTTP')
    else
  {$ENDIF}
  {$IFDEF HAVE_OPENSSL}
      net.vio := TCRVioTcpSSL.Create(host, port, FSSLOptions.Key, FSSLOptions.Cert, FSSLOptions.CA, '', FSSLOptions.Cipher, IPVersion);
  {$ELSE}
    raise EMySqlException.Create(CONN_UNKNOW_PROTOCOL, 'Can''t create SSL connection');
  {$ENDIF}
    hostInfo := host + ' via SSL';
  end
  else
{$IFDEF MSWINDOWS}
  if protocolType = MYSQL_PROTOCOL_PIPE then begin
    net.vio := TMySqlVioPipe.Create(host, port, unix_socket);
    hostInfo := host + ' via pipes';
  end
  else
{$ENDIF}
{$IFNDEF LITE}
  if (FHttpOptions <> nil) and FHttpOptions.Enabled then begin
    net.vio := TCRVioHttp.Create(FIOHandler, FHttpOptions, FProxyOptions,
      host, port, IPVersion);
    hostInfo := host + ' via HTTP';
  end
  else
{$ENDIF}
  if (protocolType = MYSQL_PROTOCOL_DEFAULT) or (protocolType = MYSQL_PROTOCOL_TCP) then begin
    net.vio := TCRVioTcp.Create(FProxyOptions, '', host, port, IPVersion);
    hostInfo := host + ' via TCP/IP';
  end
  else if net.vio = nil then
    raise EMySqlException.Create(CONN_UNKNOW_PROTOCOL);

  net.vio.Timeout := connectTimeout;
  try
    net.vio.Connect;
  except
    if net.vio is TCRVioHandler then
      raise
    else begin
      Msg := net.vio.LastError;
      raise EMySqlException.Create(CR_CONN_HOST_ERROR, [host, 10061], Msg);
    end
  end;

  net.SkipUTF8Check := True;
  net.ReceiveRow;
  ServerVersionNo := net.ReadByte;
  if ServerVersionNo <> PROTOCOL_VERSION then
    raise EMySqlException.Create(CR_VERSION_ERROR, [ServerVersionNo, PROTOCOL_VERSION], '');

  serverVersion := string(net.ReadString);
  threadId := net.ReadInt32;
  // Scramble is split into two parts because old clients do not understand long scrambles
  net.ReadBytes(FScrambleBuff, 0, ScrambleLength);
  PacketLen := net.Length;
  PacketPos := net.Position;

  net.Position := net.Position + 1;  // filler_1 $00

  if PacketLen > PacketPos + 2 then
    serverCapabilities := net.ReadByte + (word(net.ReadByte) shl 8);

  AuthPluginDataLen := 21;

  if PacketLen >= PacketPos + 18 then begin
    FServerLanguage := net.ReadByte;
    serverStatus := net.ReadInt16;
    // Since 5.5, high bits of server caps are stored after status.
    // Previously, it was part of reserved always $00 13-byte filler.
    serverCapabilities := serverCapabilities + (longword(net.ReadByte) shl 16) + (longword(net.ReadByte) shl 24);
    if (serverCapabilities and CLIENT_PLUGIN_AUTH) <> 0 then
      AuthPluginDataLen := net.ReadByte; // https://dev.mysql.com/doc/internals/en/connection-phase-packets.html#packet-Protocol::AuthSwitchResponse
    net.Position := PacketPos + 18 + 1;
  end;

  // read second part of the long scrambles
  if PacketLen >= PacketPos + 18 + (Max(13, AuthPluginDataLen - 8) - 1) then begin  // -1 for #0
    net.ReadBytes(FScrambleBuff, ScrambleLength, Max(13, AuthPluginDataLen - 8) - 1);
    net.Position := net.Position + 1; {#0}
    FAuthPluginName := 'mysql_native_password';
    if (serverCapabilities and CLIENT_PLUGIN_AUTH) <> 0 then
      FAuthPluginName := string(net.ReadString);
  end;

  Self.host := host;
  Self.port := port;

{$IFNDEF HAVE_COMPRESS_INTERFACE}
  clientflag := clientflag and not CLIENT_COMPRESS;
{$ENDIF}
  if (clientflag and CLIENT_MULTI_STATEMENTS) <> 0 then
    clientflag := clientflag or CLIENT_MULTI_RESULTS;

  clientflag := clientflag or FClientFlag or CLIENT_CAPABILITIES;
  if protocolType = MYSQL_PROTOCOL_SSL then
    clientflag := clientflag or CLIENT_SSL;

  clientflag := clientflag or CLIENT_PLUGIN_AUTH;

  //if db <> '' then
  clientflag := clientflag or CLIENT_CONNECT_WITH_DB;

  (* Remove options that server doesn't support *)
  clientflag := ((clientflag and
    not (CLIENT_COMPRESS or CLIENT_SSL or CLIENT_PROTOCOL_41)) or
    (clientflag and Self.serverCapabilities));
{$IFNDEF HAVE_COMPRESS_INTERFACE}
  clientflag := clientflag and not CLIENT_COMPRESS;
{$ENDIF}
  //clientflag := (clientflag and not (CLIENT_COMPRESS or CLIENT_SSL{ or CLIENT_PROTOCOL_41}) or (clientflag and Self.serverCapabilities));

  if (((Self.serverCapabilities and CLIENT_SSL) <> 0) and
    ((protocolType = MYSQL_PROTOCOL_SSL) or ((clientflag and CLIENT_SSL) <> 0))) then
    clientflag := clientflag or CLIENT_SSL
  else
    clientflag := clientflag and not CLIENT_SSL;

  InitHandshakeResponse;
  SwithchToSSLIfNeed;
  net.SkipUTF8Check := False;

  FClientFlag := clientflag;
  if user <> '' then
    net.WriteString(user)
  else
    net.WriteByte(0);

  if (Self.serverCapabilities and CLIENT_SECURE_CONNECTION) <> 0 then begin
    if passwd <> '' then begin
      if UsedNativeAuth then begin
        net.WriteByte(Scramble41Length);
        Scramble41(passwd);
      end
      else if UsedCachingSha2Auth then begin
        net.WriteByte(32 {Sha256_size});
        ScrambleSha2(passwd);
      end
      else if UsedOldPasswordAuth then begin
        net.WriteByte(ScrambleLength);
        Scramble(passwd);
      end
      else if UsedSha256Auth then
        WritePasswordSha256(passwd)
      else begin
        net.Fill(byte('x'), ScrambleLength);
        net.WriteByte(0);
      end;
    end
    else
      net.WriteByte(0);
  end
  else begin
    if passwd <> '' then
      Scramble(passwd)
    else
      net.WriteByte(0);
  end;

  if {(db <> '') and }((Self.serverCapabilities and CLIENT_CONNECT_WITH_DB) <> 0) then begin
    net.WriteString(db);
    Self.database := db;
  end;
//  net.Length := net.Length - 1;
//  net.Position := net.Length;

  if ((Self.serverCapabilities and CLIENT_PLUGIN_AUTH) <> 0) and (FAuthPluginName <> '') then
    net.WriteString(FAuthPluginName);
  net.Send;

  Authenticate(passwd);
{$IFDEF HAVE_COMPRESS_INTERFACE}
  net.Compress := (clientflag and CLIENT_COMPRESS) <> 0;
{$ENDIF}
end;

class function TMySqlSession.PasswordHashStage1(const Password: string): TBytes;
var
  pwdWithoutSpaces: string;
  sha1: THash_SHA1;
begin
  // Remove spaces
  pwdWithoutSpaces := StringReplace(Password, ' ', '', [rfReplaceAll]);
  pwdWithoutSpaces := StringReplace(pwdWithoutSpaces, #9, '', [rfReplaceAll]);

  sha1 := THash_SHA1.Create;
  try
    Result := sha1.ComputeHash(Encoding.Default.GetBytes(pwdWithoutSpaces));
  finally
    sha1.Free;
  end;
end;

class procedure TMySqlSession.PasswordHashStage2(Salt: TValueArr; SaltOffset: integer; var Hash: TBytes);
var
  buff: TBytes;
  sha1: THash_SHA1;
begin
  SetLength(buff, 24);
  Move((PtrOffset(Salt, SaltOffset))^, buff[0], 4);
  Buffer.BlockCopy(Hash, 0, buff, 4, Scramble41Length);

  sha1 := THash_SHA1.Create;
  try
    Hash := sha1.ComputeHash(buff);
  finally
    sha1.Free;
  end;
end;

class procedure TMySqlSession.PasswordXor(Salt: TValueArr; SaltIndex: integer; const Password: TBytes; const Result: TBytes; Length: integer);
var
  i: integer;
begin
  for i := 0 to Length - 1 do
    Result[i] := Byte(Byte(Salt[SaltIndex + i]) xor Password[i]);
end;

class procedure TMySqlSession.PasswordXorSha2(const Salt: TBytes; const Password: TBytes; const Result: TBytes);
var
  i: integer;
begin
  for i := 0 to Length(Password) - 1 do
    Result[i] := Byte(Password[i] xor Salt[i mod Length(Salt)]);
end;

procedure TMySqlSession.Scramble(const Password: string);
var
  RandSt: TRandStruct;
  HashPass: TInt64DynArray;
  HashMessage: TInt64DynArray;
  messageBuffer: TBytes;
  msg: integer;
  toStart, toIndex: integer;
  extra: byte;
  t: TBytes;
  _to: TValueArr;
  b: byte;
begin
  SetLength(messageBuffer, 8);
  SetLength(HashPass, 2);
  SetLength(HashMessage, 2);
  SetLength(t, 0);

  if Password <> '' then begin
    Buffer.BlockCopy(FScrambleBuff, 0, messageBuffer, 0, 8);
    toStart := net.Position;
    t := Encoding.Default.GetBytes(Password);
    HashPassword(HashPass, t);
    HashPassword(HashMessage, messageBuffer);

    RandomInit(RandSt, HashPass[0] xor HashMessage[0], HashPass[1] xor HashMessage[1]);
    msg := 0;
    while msg < Length(messageBuffer) do begin
      Inc(msg);
      b := byte(Math.Floor(MyRandom(RandSt)*31)+64);
      net.WriteByte(b);
    end;
    extra := byte(Math.Floor(MyRandom(RandSt)*31));
    toIndex := net.Position;
    _to := net.Row;
    while toStart <> toIndex do begin
      Byte(_to[toStart]) := Byte(_to[toStart]) xor extra;
      Inc(toStart);
    end;
  end;
  net.WriteByte(0);
end;

class function TMySqlSession.MyRandom(var RandSt: TRandStruct): double;
var
  d: double;
begin
  RandSt.Seed1 := (RandSt.Seed1 * 3 + RandSt.Seed2) mod RandSt.MaxValue;
  RandSt.Seed2 := (RandSt.Seed1 + RandSt.Seed2 + 33) mod RandSt.MaxValue;
  d := RandSt.Seed1;
  Result := (d / RandSt.MaxValueDbl);
end;

class procedure TMySqlSession.RandomInit(var RandSt: TRandStruct; Seed1, Seed2: Int64);
begin
  (* For mysql 3.21.# *)
  RandSt.MaxValue := $3FFFFFFF;
  RandSt.MaxValueDbl := RandSt.MaxValue;
  RandSt.Seed1 := Seed1 mod RandSt.MaxValue;
  RandSt.Seed2 := Seed2 mod RandSt.MaxValue;
end;

class procedure TMySqlSession.HashPassword(const Result: TInt64DynArray; const Password: TBytes);
var
  nr, add, nr2, tmp: Int64;
  i: integer;
begin
  nr := 1345345333;
  add := 7;
  nr2 := $12345671;
  for i := 0 to Length(Password) - 1 do begin
    if (Password[i] = Byte(' ')) or (Password[i] = Byte(#9)) then
      continue; (* skipp space in Password *)
    tmp := Int64(Password[i]);
    nr := nr xor ((((nr and 63) +add ) * tmp) + (nr shl 8));
    Inc(nr2, (nr2 shl 8) xor nr);
    Inc(add, tmp);
  end;
  Result[0] := nr and ((Int64(1) shl 31) - Int64(1)); (* Don't use sign bit (str2int) *);
  Result[1] := nr2 and ((Int64(1) shl 31) - Int64(1));
end;

procedure TMySqlSession.Scramble41(const Password: string);
var
  sha1: THash_SHA1;
  PasswordBuff, HashStage1, HashStage2, messageBuff, HashStageTo: TBytes;
  i: integer;
begin
  sha1 := THash_SHA1.Create;
  try
    PasswordBuff := Encoding.Default.GetBytes(Password);
    HashStage1 := sha1.ComputeHash(PasswordBuff);
    HashStage2 := sha1.ComputeHash(HashStage1);
    SetLength(messageBuff, Scramble41Length + Scramble41Length);
    Buffer.BlockCopy(FScrambleBuff, 0, messageBuff, 0, Scramble41Length);
    Buffer.BlockCopy(HashStage2, 0, messageBuff, Scramble41Length, Scramble41Length);
    HashStageTo := sha1.ComputeHash(messageBuff);
    for i := 0 to Scramble41Length - 1 do
      HashStageTo[i] := HashStage1[i] xor HashStageTo[i];

    net.WriteBytes(HashStageTo);
  finally
    sha1.Free;
  end;
end;

procedure TMySqlSession.ScrambleSha2(const Password: string);
var
  sha2_256: THash_SHA2_256;
  PasswordBuff, HashStage1, HashStage2, messageBuff, HashStageTo: TBytes;
  i: integer;
begin
  sha2_256 := THash_SHA2_256.Create;
  try
    PasswordBuff := Encoding.Default.GetBytes(Password);
    HashStage1 := sha2_256.ComputeHash(PasswordBuff);
    HashStage2 := sha2_256.ComputeHash(HashStage1);
    SetLength(messageBuff, Scramble41Length + 32);
    Buffer.BlockCopy(HashStage2, 0, messageBuff, 0, 32);
    Buffer.BlockCopy(FScrambleBuff, 0, messageBuff, 32, Scramble41Length);
    HashStageTo := sha2_256.ComputeHash(messageBuff);
    for i := 0 to 31 do
      HashStageTo[i] := HashStage1[i] xor HashStageTo[i];

    net.WriteBytes(HashStageTo);
  finally
    sha2_256.Free;
  end;
end;

procedure TMySqlSession.EncryptPasswordSha2(const Password: string; const PublicKey: string; Padding: TPaddingMode);
var
  PasswordBuff, HashStageTo: TBytes;
  Key: TRSAKey;
  KeyStrings: TStringList;
begin
  PasswordBuff := Encoding.Default.GetBytes(Password + #0);
  SetLength(HashStageTo, Length(PasswordBuff));
  PasswordXorSha2(FScrambleBuff, PasswordBuff, HashStageTo);

  Key := nil;
  KeyStrings := TStringList.Create;
  try
    KeyStrings.Text := PublicKey;
    Key := TRSAKey.Create;
    Key.ImportFrom(KeyStrings);
    net.WriteBytes(Key.Encrypt(HashStageTo, Padding));
  finally
    Key.Free;
    KeyStrings.Free;
  end;
end;

procedure TMySqlSession.SelectDb(const Database: string);
var
  buff: TBytes;
begin
  buff := Encoding.Default.GetBytes(Database);
  SimpleCommand(scInitDb, buff, Length(buff), False);
  Self.database := database;
end;

function TMySqlSession.Statistics: string;
begin
  SimpleCommand(scStatistics, TValueArr(nil), 0, False);
  Result := string(net.ReadString(net.Length));
end;

procedure TMySqlSession.Ping;
begin
  SimpleCommand(scPing, TValueArr(nil), 0, False);
end;

end.
