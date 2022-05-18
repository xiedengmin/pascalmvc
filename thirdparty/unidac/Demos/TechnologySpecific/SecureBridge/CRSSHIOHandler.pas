unit CRSSHIOHandler;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  Types, SysUtils, Classes, ScTypes, MemUtils,
  ScVio, {$IFNDEF SBRIDGE}CRVio, {$ENDIF}
  ScBridge, ScSSHClient, ScSSHChannel;

{$I SecureBridgeVer.inc}

type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TCRSSHIOHandler = class (TCRIOHandler)
  protected
    FClient: TScSSHClient;
    //FChannel: TScSSHChannel;

    procedure CheckClient;
    procedure SetClient(Value: TScSSHClient);
    procedure ConnectChange(Sender: TObject; Connecting: boolean);
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    function GetHandlerType: string; override;

    procedure DoServerKeyValidate(Sender: TObject;
      NewServerKey: TScKey; var Accept: Boolean);
    procedure DoAuthenticationPrompt(Sender: TObject;
      const Name, Instruction: string; const Prompts: TStringDynArray;
      var Responses: TStringDynArray);

  public
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

  published
    property Client: TScSSHClient read FClient write SetClient;
  end;

implementation

uses
  ScConsts, ScUtils, CRAccess, DBAccess, CRFunctions,
  ScSSHUtils;

const
  KEY_GUID = '861690563F1B';

{ TCRSSHIOHandler }

destructor TCRSSHIOHandler.Destroy;
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

  if Assigned(Client) then
    TScClientUtils.UnregisterClient(Client, Self);

  inherited;
end;

function TCRSSHIOHandler.GetHandlerType: string;
begin
  Result := 'ssh';
end;

procedure TCRSSHIOHandler.CheckClient;
begin
  if FClient = nil then
    raise EScError.Create(SClientNotDefined);

  if not Assigned(FClient.OnServerKeyValidate) then
    FClient.OnServerKeyValidate := DoServerKeyValidate;
  if not Assigned(FClient.OnAuthenticationPrompt) then
    FClient.OnAuthenticationPrompt := DoAuthenticationPrompt;

  try
    FClient.Connect;
  except
    on E: EScError do begin
      if (FClient.Authentication = atPassword) and (E.ErrorCode = seAuthenticationFailed) then begin
        FClient.Authentication := atKeyboardInteractive;
        FClient.Connect;
      end
      else
        raise;
    end;
  end;
end;

function TCRSSHIOHandler.Connect(const Server: string; const Port: integer;
  HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
  SSLOptions: TSSLOptions; SSHOptions: TSSHOptions;
  IPVersion: TIPVersion = ivIPv4): TCRIOHandle;
var
  Channel: TScSSHChannel;
begin
  // set default Username for Oracle
  if (SSHOptions <> nil) and (Client.User = '') then
    Client.User := SSHOptions.Username;

  CheckClient;

  Channel := TScSSHChannel.Create(Self);
  try
    Channel.Direct := True;
    Channel.Client := FClient;
    //FClient.Options.IPVersion := IPVersion;  //ODBC
    Channel.DestHost := Server;
    Channel.DestPort := Port;
    Channel.Timeout := FClient.Timeout;

    Channel.Connect;
  except
    Channel.Free;
    raise;
  end;
  Result := Channel;
end;

procedure TCRSSHIOHandler.Disconnect(Handle: TCRIOHandle);
var
  Channel: TScSSHChannel;
begin
  Channel := Handle as TScSSHChannel;
  Channel.Connected := False;
end;

class function TCRSSHIOHandler.ReadNoWait(Handle: TCRIOHandle;
  const buffer: TValueArr; offset, count: integer): integer;
var
  Channel: TScSSHChannel;
begin
  Channel := Handle as TScSSHChannel;
  Result := Channel.ReadNoWait(buffer[offset], count);
end;

class function TCRSSHIOHandler.Read(Handle: TCRIOHandle;
  const buffer: TValueArr; offset, count: integer): integer;
var
  Channel: TScSSHChannel;
begin
  Channel := Handle as TScSSHChannel;
  Result := Channel.ReadBuffer(buffer[offset], count);
end;

class function TCRSSHIOHandler.Write(Handle: TCRIOHandle;
  const buffer: TValueArr; offset, count: integer): integer;
var
  Channel: TScSSHChannel;
begin
  Channel := Handle as TScSSHChannel;
  Result := Channel.WriteBuffer(buffer[offset], count);
end;

class function TCRSSHIOHandler.WaitForData(Handle: TCRIOHandle;
  Timeout: integer = -1): boolean;
var
  Channel: TScSSHChannel;
begin
  Channel := Handle as TScSSHChannel;
  if Timeout = -1 then
    Timeout := MaxInt;
  Result := TScSSHChannelUtils.Readable(Channel, 1, Timeout);
end;

class function TCRSSHIOHandler.GetTimeout(Handle: TCRIOHandle): integer;
var
  Channel: TScSSHChannel;
begin
  Channel := Handle as TScSSHChannel;
  Result := Channel.Timeout;
end;

class procedure TCRSSHIOHandler.SetTimeout(Handle: TCRIOHandle; Value: integer);
var
  Channel: TScSSHChannel;
begin
  Channel := Handle as TScSSHChannel;

  if Value = 0 then
    Value := MaxInt;
  Channel.Timeout := Value;
end;

procedure TCRSSHIOHandler.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FClient) and (Operation = opRemove) then
    Client := nil;

  inherited;
end;

procedure TCRSSHIOHandler.SetClient(Value: TScSSHClient);
begin
  if Value <> FClient then begin
    if FClient <> nil then begin
      TScClientUtils.UnregisterClient(FClient, Self);
      FClient.RemoveFreeNotification(Self);
    end;

    FClient := Value;

    if Value <> nil then begin
      TScClientUtils.RegisterClient(Value, Self, ConnectChange);
      Value.FreeNotification(Self);
    end;
  end;
end;

procedure TCRSSHIOHandler.ConnectChange(Sender: TObject; Connecting: boolean);
var
  i: integer;
  Conn: TCustomDAConnection;
begin
  if not Connecting then
    for i := 0 to FList.Count - 1 do begin
      if IsClass(TObject(FList[i]), TCustomDAConnection) then begin
        Conn := TCustomDAConnection(FList[i]);
        if not Conn.Pooling and not Conn.Options.DisconnectedMode then
          Conn.Disconnect;
      end;
    end;
end;

procedure TCRSSHIOHandler.DoAuthenticationPrompt(Sender: TObject;
  const Name, Instruction: string; const Prompts: TStringDynArray;
  var Responses: TStringDynArray);
begin
  if Length(Responses) > 0 then
    Responses[0] := TScSSHClient(Sender).Password;
end;

procedure TCRSSHIOHandler.DoServerKeyValidate(Sender: TObject;
  NewServerKey: TScKey; var Accept: Boolean);
var
  HostKeyName: string;
  Key: TScKey;
begin
  if TScSSHClient(Sender).HostKeyName = '' then begin
    HostKeyName := TScSSHClient(Sender).HostName + KEY_GUID;
    Key := TScSSHClient(Sender).KeyStorage.Keys.FindKey(HostKeyName);

    if Key = nil then begin
      Key := TScKey.Create(nil);
      try
        Key.Assign(NewServerKey);
        Key.KeyName := HostKeyName;
        TScSSHClient(Sender).KeyStorage.Keys.Add(Key);
      except
        Key.Free;
        raise;
      end;

      Accept := True;
    end
    else
      Accept := Key.Equals(NewServerKey);
  end;
end;

end.

