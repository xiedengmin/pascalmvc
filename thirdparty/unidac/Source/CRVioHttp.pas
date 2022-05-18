//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRVioHttp;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
  CLRClasses, CRTypes, CRFunctions, CRVio, CRHttp;

type
  TTimerThread = class(TThread)
  protected
    FInterval: cardinal;
    FOnTimer: TNotifyEvent;
    FPreviousTimeout: cardinal;
    FEvent: TEvent;

    procedure Execute; override;
  public
    constructor Create(Interval: cardinal; OnTimer: TNotifyEvent);
    destructor Destroy; override;
  end;

  TCRVioHttp = class(TCRVio)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FIOHandler: TCRIOHandler;
    FHttpOptions: THttpOptions;
    FProxyOptions: TProxyOptions;
    FUrl: string;
    FTestUrl: string;
    FPortID: Integer;
    FClosed: Boolean;
    FStartServerScriptRequest: TCRHttpWebRequest;
    FLastReadResponse: TCRHttpWebResponse;
    FConnectionTimeout: integer;
    FSendTimeout: integer;
    FReceiveTimeout: integer;
    FScriptNotificationTime: Integer;
    FExceptionLock: TCriticalSection;
    FThreadException: Exception;
    FIPVersion: TIPVersion;
    FIsConnectedEvent: TEvent;
    FTimerThread: TTimerThread;

    procedure SetIPVersion(const Value: TIPVersion);

    procedure OnScriptNotification(Sender: TObject);
    procedure OnStartServerScriptConnected(Sender: TObject);
    procedure ReStartNotification;
    procedure StartServerScript;
    procedure AbortConnectionScript;
    function CreateRequest(const Url: string; const Command: Char; const Method: string): TCRHttpWebRequest;
    procedure CheckTestConnectionResponse(Response: TCRHttpWebResponse);
    procedure CheckResponseSuccess(Response: TCRHttpWebResponse);
    procedure CheckThreadException;

    function ExecuteGetRequest(const Url: string; const Command: Char; CheckLeaseException: Boolean = True): TCRHttpWebResponse;

  protected
    function GetConnected: boolean; override;
    function GetConnectionTimeout: integer; override;
    procedure SetConnectionTimeout(Value: integer); override;
    function GetSendTimeout: integer; override;
    procedure SetSendTimeout(Value: integer); override;
    function GetReceiveTimeout: integer; override;
    procedure SetReceiveTimeout(Value: integer); override;

  public
    constructor Create(IOHandler: TCRIOHandler; HttpOptions: THttpOptions;
      ProxyOptions: TProxyOptions; const Hostname: string; Port: integer;
      IPVersion: TIPVersion = ivIPv4);
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;

    function ReadNoWait(const buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const buffer: TValueArr; offset, count: integer): integer; override;
    function Write(const buffer: TValueArr; offset, count: integer): integer; override;
    function WaitForData(MillisecondsTimeout: integer = -1): boolean; override;

    property IPVersion: TIPVersion read FIPVersion write SetIPVersion;
  end;

implementation

uses
  CRVioSocket;

resourcestring
  SCannotEstablishConnection = 'Cannot establish HTTP connection';
  SInvalidHttpResponse = 'Invalid HTTP script response on connection';
  SInvalidScriptResponse = 'Invalid script response. Failed to read success flag';
  SScriptReportedError = 'HTTP script reported error';
  SUnknownError = 'HTTP script unknown error: ';

type
  ScriptCommands = (scClose, scRead, scWrite, scSelect, scLease, scConnect, scTest);
  ScriptResponseFlags = (rfSuccess, rfError);

const
  SScriptCommands: array[ScriptCommands] of Char = ('x', 'r', 'w', 's', 'l', 'c', 't');
  SScriptResponseFlags: array[ScriptResponseFlags] of string = ('OK:', 'ER:');

type
  TThreadMethod = procedure of object;

  THttpConnector = class(TThread)
  private
    FFinished: boolean;

  protected
    FExecuteMethod: TThreadMethod;
    FAbortMethod: TThreadMethod;
    procedure Execute; override;
  public
    constructor Create(ExecuteMethod, AbortMethod: TThreadMethod);
    destructor Destroy; override;
  end;

{ TTimerThread }

constructor TTimerThread.Create(Interval: cardinal; OnTimer: TNotifyEvent);
begin
  FInterval := Interval;
  FOnTimer := OnTimer;
  FEvent := CreateEvent;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TTimerThread.Destroy;
begin
  Terminate;
  FEvent.SetEvent;
  inherited;
  FEvent.Free;
end;

procedure TTimerThread.Execute;
var
  WaitTime: cardinal;
begin
  WaitTime := FInterval div 10;

  while not Terminated do begin
    while (GetTickCount - FPreviousTimeout + WaitTime < FInterval) and not Terminated do
      FEvent.WaitFor(WaitTime);

    if not Terminated then
      FOnTimer(nil);
  end;
end;

{ THttpConnector }

constructor THttpConnector.Create(ExecuteMethod, AbortMethod: TThreadMethod);
begin
  FExecuteMethod := ExecuteMethod;
  FAbortMethod := AbortMethod;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor THttpConnector.Destroy;
begin
  try
    if Assigned(FAbortMethod) then
      FAbortMethod();
  except
  end;

  inherited;
end;

procedure THttpConnector.Execute;
begin
  try
    try
      if Assigned(FExecuteMethod) then
        FExecuteMethod();
    except
    end;
  finally
    FFinished := True;
  end;
end;

{ TCRVioHttp }

constructor TCRVioHttp.Create(IOHandler: TCRIOHandler; HttpOptions: THttpOptions;
  ProxyOptions: TProxyOptions; const Hostname: string; Port: integer;
  IPVersion: TIPVersion = ivIPv4);
var
  ConnectionId: string;
  ConnectionUrl: string;
begin
  inherited Create;

  FIOHandler := IOHandler;

  FHttpOptions := THttpOptions.Create;
  if Assigned(HttpOptions) then
    FHttpOptions.Assign(HttpOptions);

  FProxyOptions := TProxyOptions.Create;
  if Assigned(ProxyOptions) then
    FProxyOptions.Assign(ProxyOptions);

  Randomize;
  ConnectionId := IntToStr(Random(1000000)) + '_' + IntToStr(Random(1000000));

  FUrl := Trim(FHttpOptions.Url);
  if (Copy(FUrl, Length(FUrl), 1) <> '/') and (Copy(FUrl, Length(FUrl) - 3, 4) <> '.php') then
    FUrl := FUrl + '/';

  ConnectionUrl := Format(FUrl + '?a=c&s=%s&p=%d&id=%s', [Hostname, Port, ConnectionId]);
  FTestUrl := Format(FUrl + '?a=t&s=%s&p=%d&id=%s', [Hostname, Port, ConnectionId]);
  FPortID := 0;
  FIPVersion := IPVersion;

  FIsConnectedEvent := CreateEvent;
  FStartServerScriptRequest := CreateRequest(ConnectionUrl, SScriptCommands[scConnect], 'GET');
  FStartServerScriptRequest.OnConnected := OnStartServerScriptConnected;
  FConnectionTimeout := DefaultTimeOut;
  FReceiveTimeout := DefaultTimeOut;
  FSendTimeout := 0;
  FScriptNotificationTime := 0;
  FExceptionLock := TCriticalSection.Create;
  FClosed := True;
end;

destructor TCRVioHttp.Destroy;
begin
  inherited;

  FExceptionLock.Free;
  FHttpOptions.Free;
  FProxyOptions.Free;
  FStartServerScriptRequest.Free;
  FIsConnectedEvent.Free;
end;

procedure TCRVioHttp.SetIPVersion(const Value: TIPVersion);
begin
  if not FClosed then begin
    FLastErrorCode := 0;
    FLastError := SCannotChangeTcpVersion;
    raise SocketException.Create(FLastError);
  end
  else
    FIPVersion := Value;
end;

function TCRVioHttp.GetConnectionTimeout: integer;
begin
  Result := FConnectionTimeout;
end;

procedure TCRVioHttp.SetConnectionTimeout(Value: integer);
begin
  FConnectionTimeout := Value;
end;

function TCRVioHttp.GetSendTimeout: integer;
begin
  Result := FSendTimeout;
end;

procedure TCRVioHttp.SetSendTimeout(Value: integer);
begin
  FSendTimeout := Value;
end;

function TCRVioHttp.GetReceiveTimeout: integer;
begin
  Result := FReceiveTimeout;
end;

procedure TCRVioHttp.SetReceiveTimeout(Value: integer);
begin
  if Value = 0 then
    FReceiveTimeout := 1
  else
    FReceiveTimeout := Value;
end;

function TCRVioHttp.GetConnected: boolean;
begin
  Result := not FClosed;
end;

procedure TCRVioHttp.Close;
var
  Response: TCRHttpWebResponse;
begin
  if FClosed then begin
    inherited; // to close inherited TAsyncReceiveThread
    Exit;
  end;

  FEvClose.Acquire;
  try
    if FClosed then begin
      inherited;
      Exit;
    end;
    FClosed := True;
  finally
    FEvClose.Release;
  end;

    try
      Response := ExecuteGetRequest(FUrl, SScriptCommands[scClose], False);
      Response.Free;
    except
    end;

  inherited;

  FreeAndNil(FThreadException);
  FreeAndNil(FTimerThread);
  FreeAndNil(FLastReadResponse);
end;

procedure TCRVioHttp.Connect;
var
  Response: TCRHttpWebResponse;
  HttpConnector: THttpConnector;
  Connected: boolean;
  RetryCount: Integer;
  ConnectionTime: Cardinal;
  TimeoutMSec: Cardinal;
begin
  TryConnect;

  FLastError := '';

  try
    if (Int64(FConnectionTimeout) * 1000) > MaxInt then
      TimeoutMSec := MaxInt
    else
      TimeoutMSec := FConnectionTimeout * 1000;
    RetryCount := 0;
    Connected := False;
    FStartServerScriptRequest.IPVersion := FIPVersion;

    repeat
      FIsConnectedEvent.ResetEvent;
      ConnectionTime := GetTickCount;

      HttpConnector := THttpConnector.Create(StartServerScript, AbortConnectionScript);
      try
        FIsConnectedEvent.WaitFor(Cardinal(-1));

        // test whether connnected script responds
        while (TimeoutMSec = 0) or (GetTickCount - ConnectionTime < TimeoutMSec) do begin
          try
            Response := ExecuteGetRequest(FTestUrl, SScriptCommands[scTest]);
            try
              CheckTestConnectionResponse(Response);
              Connected := True;
              Break;
            finally
              Response.Free;
            end;
          except
          end;

          if HttpConnector.FFinished then begin
            if FStartServerScriptRequest.StatusCode <> scOK then
              FLastError := FStartServerScriptRequest.StatusDescription
            else
              FLastError := '';
            Break;
          end;
        end;
      finally
        HttpConnector.Free;
      end;

      Inc(RetryCount);
    until Connected or (RetryCount >= 3);

  except
    on E: Exception do begin
      FLastError := E.Message;
      raise;
    end;
  end;

  if not Connected then begin
    if FLastError = '' then
      FLastError := SCannotEstablishConnection
    else
      FLastError := SCannotEstablishConnection + ': ' + FLastError;
    raise HttpException.Create(FLastError);
  end;

  FClosed := False;

  Assert(FTimerThread = nil);
  FTimerThread := TTimerThread.Create(FScriptNotificationTime, OnScriptNotification);
  ReStartNotification;
end;

procedure TCRVioHttp.StartServerScript;
begin
  try
    if FConnectionTimeout > 0 then
      FStartServerScriptRequest.ReadWriteTimeout := FConnectionTimeout;
    FStartServerScriptRequest.Method := rmGET;
    FStartServerScriptRequest.GetResponse{$IFNDEF AUTOREFCOUNT}.Free{$ENDIF};
  finally
    FStartServerScriptRequest.Disconnect;
  end;
end;

procedure TCRVioHttp.AbortConnectionScript;
begin
  FStartServerScriptRequest.Abort;
end;

procedure TCRVioHttp.OnStartServerScriptConnected(Sender: TObject);
begin
  FIsConnectedEvent.SetEvent;
end;

function TCRVioHttp.CreateRequest(const Url: string; const Command: Char; const Method: string): TCRHttpWebRequest;
var
  CommandStr: string;
begin
  if (Command = SScriptCommands[scTest]) or (Command = SScriptCommands[scConnect]) then
    CommandStr := ''
  else
    CommandStr := '?a=' + Command;

  Result := TCRHttpWebRequest.Create(Url + CommandStr + '&port=' + IntToStr(FPortID));
  try
    Result.IOHandler := FIOHandler;
    Result.SSLOptions.IgnoreServerCertificateValidity := FHttpOptions.TrustServerCertificate;
    Result.SSLOptions.IgnoreServerCertificateConstraints := FHttpOptions.TrustServerCertificate;
    Result.SSLOptions.TrustServerCertificate := FHttpOptions.TrustServerCertificate;
    Result.IPVersion := FIPVersion;
    Result.ReadWriteTimeout := FReceiveTimeout;

    Result.ContentType := Content_Type_Octet;
    Result.CachePolicy.Level := clNoCacheNoStore;
    Result.Credentials.UserName := FHttpOptions.Username;
    Result.Credentials.Password := FHttpOptions.Password;
    Result.Proxy.Address := FProxyOptions.Hostname;
    Result.Proxy.Port := FProxyOptions.Port;
    Result.Proxy.Credentials.Username := FProxyOptions.Username;
    Result.Proxy.Credentials.Password := FProxyOptions.Password;
  except
    Result.Free;
    raise;
  end;
end;

procedure TCRVioHttp.ReStartNotification;
begin
  if FTimerThread <> nil then
    FTimerThread.FPreviousTimeout := GetTickCount;
end;

procedure TCRVioHttp.OnScriptNotification(Sender: TObject);
var
  Response: TCRHttpWebResponse;
begin
  if FClosed then begin
    if FTimerThread <> nil then
      FTimerThread.Terminate;
    Exit;
  end;

  try
    Response := ExecuteGetRequest(FUrl, SScriptCommands[scLease], False);
    Response.Free;
  except
    on E: SocketException do
      FThreadException := SocketException.Create(E.Message);
    on E: HttpException do
      FThreadException := HttpException.Create(E.Message);
    on E: Exception do
      FThreadException := Exception.Create(E.Message);
  end;

  ReStartNotification;
end;

procedure TCRVioHttp.CheckTestConnectionResponse(Response: TCRHttpWebResponse);

  procedure RaiseInvalidResponseError;
  begin
    raise HttpException.Create(SInvalidHttpResponse);
  end;

var
  Content: TBytes;
  Line: string;
  RequestTime: Integer;
  cnt: Integer;
  p1, p2: Integer;
begin
  SetLength(Content, 64);
  cnt := Response.ReadBuffer(TValueArr(Content), 0, Length(Content));
  if cnt < 4 then
    RaiseInvalidResponseError;

  p1 := 1;
  while p1 < cnt do begin
    if Content[p1] = 10 then
      Break;
    Inc(p1);
  end;

  if p1 >= cnt then
    RaiseInvalidResponseError;

  Line := Encoding.Default.GetString(Content, 0, p1);
  if not TryStrToInt(Trim(Line), FPortID) then
    RaiseInvalidResponseError;

  Inc(p1);
  p2 := p1;
  while p2 < cnt do begin
    if Content[p2] = 10 then
      Break;
    Inc(p2);
  end;

  if p2 >= cnt then
    RaiseInvalidResponseError;

  Line := Encoding.Default.GetString(Content, p1, p2 - p1);
  if not TryStrToInt(Trim(Line), FScriptNotificationTime) then
    RaiseInvalidResponseError;

  if FReceiveTimeout > 0 then
    RequestTime := FReceiveTimeout
  else
    RequestTime := 7;

  // consider maximum request transfer time, notify script before lifetime expires
  if FScriptNotificationTime > 2 * RequestTime then
    FScriptNotificationTime := FScriptNotificationTime - RequestTime
  else
    FScriptNotificationTime := FScriptNotificationTime div 2;

  if FScriptNotificationTime <= 0 then
    FScriptNotificationTime := 1;

  if (Int64(FScriptNotificationTime) * 1000) > MaxInt then
    FScriptNotificationTime := MaxInt
  else
    FScriptNotificationTime := FScriptNotificationTime * 1000;
end;

procedure TCRVioHttp.CheckResponseSuccess(Response: TCRHttpWebResponse);
var
  Flag, AdditionalMessage: string;
  ErrorMessage: TBytes;
  cnt: Integer;
begin
  try
    SetLength(ErrorMessage, 1024);
    cnt := Response.ReadBuffer(TValueArr(ErrorMessage), 0, Length(SScriptResponseFlags[rfSuccess]));
    if cnt < Length(SScriptResponseFlags[rfSuccess]) then
      raise HttpException.Create(SInvalidScriptResponse);

    Flag := Encoding.Default.GetString(ErrorMessage, 0, cnt);
    if AnsiSameText(Flag, SScriptResponseFlags[rfSuccess]) then
      Exit;

    // Error
    if AnsiSameText(Flag, SScriptResponseFlags[rfError]) then
      AdditionalMessage := ''
    else
      AdditionalMessage := SUnknownError + Flag;

    // read error description
    cnt := Response.ReadBuffer(TValueArr(ErrorMessage), 0, Length(ErrorMessage));
    AdditionalMessage := AdditionalMessage + Encoding.Default.GetString(ErrorMessage, 0, cnt);

    if Length(AdditionalMessage) > 0 then
      raise HttpException.Create(AdditionalMessage)
    else
      raise HttpException.Create(SScriptReportedError);
  except
    try
      Close;
    except
    end;

    raise;
  end;
end;

procedure TCRVioHttp.CheckThreadException;
var
  E: Exception;
begin
  FExceptionLock.Enter;
  try
    if FThreadException <> nil then begin
      E := FThreadException;
      FThreadException := nil;
      raise E;
    end;
  finally
    FExceptionLock.Leave;
  end;
end;

function TCRVioHttp.ExecuteGetRequest(const Url: string; const Command: Char;
  CheckLeaseException: Boolean = True): TCRHttpWebResponse;
var
  Request: TCRHttpWebRequest;
begin
  if CheckLeaseException then
    CheckThreadException;

  Request := CreateRequest(Url, Command, 'GET');
  try
    Request.Method := rmGET;
    Result := Request.GetResponse;
    try
      CheckResponseSuccess(Result);
    except
      Result.Free;
      raise;
    end;
  finally
    Request.Free;
  end;
end;

function TCRVioHttp.WriteNoWait(const buffer: TValueArr; offset, count: integer): integer;

  procedure SendPostRequest(buffer: TValueArr; offset, count: integer);
  var
    Request: TCRHttpWebRequest;
    Response: TCRHttpWebResponse;
  begin
    CheckThreadException;

    Response := nil;
    Request := CreateRequest(FUrl, SScriptCommands[scWrite], 'POST');
    try
      Request.Method := rmPOST;
      Request.ContentLength := count;
      Request.WriteBuffer(buffer, offset, count);
      Response := Request.GetResponse;
      CheckResponseSuccess(Response);
      ReStartNotification;
    finally
      Request.Free;
      Response.Free;
    end;
  end;

var
  writeCount: Integer;
begin
  Result := 0;
  if FClosed then
    Exit;

  while count > 0 do begin
    if count > 2 * 1024 * 1024 then
      writeCount := 2 * 1024 * 1024
    else
      writeCount := count;

    try
      FLastError := '';
      SendPostRequest(buffer, offset, writeCount);
      Inc(Result, writeCount);
      Inc(offset, writeCount);
      Dec(count, writeCount);
    except
      on E: Exception do begin
        FLastError := E.Message;
        exit;
      end;
    end
  end;
end;

function TCRVioHttp.Write(const buffer: TValueArr; offset, count: integer): integer;
begin
  Result := WriteNoWait(buffer, offset, count);
end;

function TCRVioHttp.ReadNoWait(const buffer: TValueArr; offset, count: integer): integer;
var
  Response: TCRHttpWebResponse;
begin
  Result := 0;
  if FClosed or (count = 0) then
    Exit;

  Response := nil;
  FLastError := '';
  try
    try
      if FLastReadResponse <> nil then begin
        Response := FLastReadResponse;
        FLastReadResponse := nil;
      end
      else begin
        Response := ExecuteGetRequest(FUrl, SScriptCommands[scRead]);
        ReStartNotification;
      end;

      Result := Response.ReadBuffer(buffer, offset, count);

      if Result = count then begin
        if Response.WaitForData(0) then
          FLastReadResponse := Response;
      end;

      if FLastReadResponse <> nil then
        Response := nil;
    finally
      Response.Free;
    end;

  except
    on E: Exception do
      FLastError := E.Message;
  end
end;

function TCRVioHttp.WaitForData(MillisecondsTimeout: integer = -1): boolean;
var
  Request: TCRHttpWebRequest;
  Response: TCRHttpWebResponse;
  Flag: string;
  ErrorMessage: TBytes;
  cnt: Integer;
begin
  Result := FLastReadResponse <> nil;

  if Result or (MillisecondsTimeout = 0) then
    Exit;

  SetLength(ErrorMessage, Length(SScriptResponseFlags[rfSuccess]));

  while not FClosed do begin
    Response := nil;
    Request := CreateRequest(FUrl, SScriptCommands[scSelect], 'GET');
    try
      try
        Request.ReadWriteTimeout := 15;
        Request.Method := rmGET;
        Response := Request.GetResponse;
      except
        Continue;
      end;

      cnt := Response.ReadBuffer(TValueArr(ErrorMessage), 0, Length(SScriptResponseFlags[rfSuccess]));
      if cnt < Length(SScriptResponseFlags[rfSuccess]) then
        raise HttpException.Create(SInvalidScriptResponse);

      Flag := Encoding.Default.GetString(ErrorMessage, 0, cnt);
      if AnsiSameText(Flag, SScriptResponseFlags[rfSuccess]) then begin
        Result := True;
        Break;
      end;
    finally
      Request.Free;
      Response.Free;
    end;
  end;

  if FClosed then
    Result := True
  else
    ReStartNotification;
end;

end.
