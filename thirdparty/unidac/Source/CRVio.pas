//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRVio;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFDEF PERF_COUNTER}
  Debug,
{$ENDIF}
{$IFDEF VIO_DEBUG}
  Debug,
{$ENDIF}
  CLRClasses, CRTypes;

resourcestring
  SVioError = 'Input/Output error';
  SNotOverriden = 'Not overriden in an inherited class';

{$DEFINE BUFFERED_READ}

{$IFDEF BUFFERED_READ}
const
  // Copied from MySQL 5.0 client for performance reason
  VIO_READ_BUFFER_SIZE = 32768;
  VIO_UNBUFFERED_READ_MIN_SIZE = 2048;
{$ENDIF}

type
  TIPVersion = (ivIPv4, ivIPv6, ivIPBoth);
  TCRSocksVersion = (svNoSocks, svSocks4, svSocks5);

const
  DefValIPVersion = ivIPv4;
  DefValSocksVersion = svNoSocks;
  DefValResolveDNS = True;

type
  SocketException = class(Exception)
  protected
    FErrorCode: integer;
  public
    constructor Create(const Msg: string); overload;
    constructor Create(const Msg: string; ErrorCode: integer{$IFDEF BCB}; AHelpContext: integer = 0{$ENDIF}{$IFDEF VER10P}; AHelpContext: integer = 0{$ENDIF}); overload;
    {$EXTERNALSYM SocketException.Create}
    constructor CreateFmt(const Msg: string; const Args: array of const; ErrorCode: integer);
    {$EXTERNALSYM SocketException.CreateFmt}

    property ErrorCode: integer read FErrorCode;
  end;

  TAsyncThread = class;

  TCRVio = class
  private
    FClosing: boolean;
    FOnClose: TNotifyEvent;
    FReceiveThread: TAsyncThread;
    FNonBlocking: boolean;
    FOnAsyncReceive: TNotifyEvent;

    procedure SetNonBlocking(Value: boolean);
    procedure OnReceive(ar: IScAsyncResult);
    procedure DoOnClose;

  protected
    FEvClose: TCriticalSection;
    FLastError: string;
    FLastErrorCode: integer;

  {$IFDEF BUFFERED_READ}
    FBuffer: TBytes;
    FBufferLen, FBufferPos: integer;
  {$ENDIF}

    function GetTimeout: integer; virtual;
    procedure SetTimeout(Value: integer); virtual;
    function GetConnectionTimeout: integer; virtual;
    procedure SetConnectionTimeout(Value: integer); virtual;
    function GetSendTimeout: integer; virtual;
    procedure SetSendTimeout(Value: integer); virtual;
    function GetReceiveTimeout: integer; virtual;
    procedure SetReceiveTimeout(Value: integer); virtual;

    function GetConnected: boolean; virtual;

    procedure BeforeClosing; virtual;
    procedure InternalClose; virtual;
    function GetAvailable: integer; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    function TryConnect: Boolean; virtual;
    procedure Connect; virtual; abstract;
    procedure Close; virtual;

    function ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer; virtual; abstract;
    function WriteNoWait(const Buffer: TValueArr; Offset, Count: integer): integer; virtual; abstract;
    function Read(const Buffer: TValueArr; Offset, Count: integer): integer; virtual;
    function Write(const Buffer: TValueArr; Offset, Count: integer): integer; virtual;
    function WaitForData(MillisecondsTimeout: integer = -1): boolean; virtual;

    function BeginReceive(var Buffer: TBytes; const Offset, Size: integer; Callback: AsyncCallback; State: TObject): IScAsyncResult; virtual;
    function EndReceive(AsyncResult: IScAsyncResult): integer; virtual;
    procedure StopAsync;

    property Available: integer read GetAvailable;
    property Connected: boolean read GetConnected;
    property ConnectionTimeout: integer read GetConnectionTimeout write SetConnectionTimeout;
    property SendTimeout: integer read GetSendTimeout write SetSendTimeout;
    property ReceiveTimeout: integer read GetReceiveTimeout write SetReceiveTimeout;
    property Timeout: integer read GetTimeout write SetTimeout;
    property LastError: string read FLastError;
    property LastErrorCode: integer read FLastErrorCode;

    property NonBlocking: boolean read FNonBlocking write SetNonBlocking;
    property OnAsyncReceive: TNotifyEvent read FOnAsyncReceive write FOnAsyncReceive;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TCRIOHandle = TObject;
  THttpOptions = class;
  TProxyOptions = class;
  TSSLOptions = class;
  TSSHOptions = class;

  TCRIOHandler = class (TComponent)
  protected
    FList: TCRList;

    procedure RegisterClient(Client: TObject);
    procedure UnRegisterClient(Client: TObject);

    class procedure SetIsSecure(Handle: TCRIOHandle; const Value: Boolean); virtual;
    class function GetIsSecure(Handle: TCRIOHandle): Boolean; virtual;
    class procedure Renegotiate(Handle: TCRIOHandle); virtual;
    function GetHandlerType: string; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Connect(const Server: string; const Port: integer;
      HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
      SSLOptions: TSSLOptions; SSHOptions: TSSHOptions;
      IPVersion: TIPVersion = ivIPv4): TCRIOHandle; virtual; abstract;
    procedure Disconnect(Handle: TCRIOHandle); virtual;

    class function ReadNoWait(Handle: TCRIOHandle; const Buffer: TValueArr; Offset, Count: integer): integer; virtual;
    class function Read(Handle: TCRIOHandle; const Buffer: TValueArr; Offset, Count: integer): integer; virtual;
    class function Write(Handle: TCRIOHandle; const Buffer: TValueArr; Offset, Count: integer): integer; virtual;
    class function WaitForData(Handle: TCRIOHandle; MillisecondsTimeout: integer = -1): boolean; virtual;

    class function GetTimeout(Handle: TCRIOHandle): integer; virtual;
    class procedure SetTimeout(Handle: TCRIOHandle; Value: integer); virtual;
    property HandlerType: string read GetHandlerType;
  end;

  TCRVioHandler = class (TCRVio)
  protected
    FIOHandler: TCRIOHandler;
    FHandle: TCRIOHandle;
    FHostname: string;
    FPort: integer;
    FTimeout: integer;
    FIPVersion: TIPVersion;
    FHttpOptions: THttpOptions;
    FProxyOptions: TProxyOptions;
    FSSLOptions: TSSLOptions;
    FSSHOptions: TSSHOptions;

    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;
    function GetReceiveTimeout: integer; override;
    procedure SetReceiveTimeout(Value: integer); override;
    procedure SetIsSecure(const Value: Boolean);
    function GetIsSecure: Boolean;
  public
    constructor Create(const Hostname: string; const Port: integer;
      IOHandler: TCRIOHandler; HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
      SSLOptions: TSSLOptions; SSHOptions: TSSHOptions;
      IPVersion: TIPVersion = ivIPv4);
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;
    procedure Renegotiate;

    function BeginReceive(var Buffer: TBytes; const Offset, Size: integer; Callback: AsyncCallback; State: TObject): IScAsyncResult; override;
    function EndReceive(AsyncResult: IScAsyncResult): integer; override;

    function ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function WriteNoWait(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function Read(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function Write(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function WaitForData(MillisecondsTimeout: integer = -1): boolean; override;
    property IsSecure: Boolean read GetIsSecure write SetIsSecure;
    property Host: string read FHostName;
    property Port: integer read FPort;
  end;

  TCRIOHandlerUtils = class
    class procedure RegisterClient(Obj: TCRIOHandler; Client: TObject);
    class procedure UnRegisterClient(Obj: TCRIOHandler; Client: TObject);
  end;

  TProxyOptions = class(TPersistent)
  private
    FHostname: string;
    FPort: integer;
    FUsername: string;
    FPassword: string;
    FSocksVersion: TCRSocksVersion;
    FResolveDNS: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    function Equals(ProxyOptions: TProxyOptions): boolean; reintroduce;
  published
    property Hostname: string read FHostname write FHostname;
    property Port: integer read FPort write FPort default 0;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property SocksVersion: TCRSocksVersion read FSocksVersion write FSocksVersion default DefValSocksVersion;
    property ResolveDNS: boolean read FResolveDNS write FResolveDNS default DefValResolveDNS;
  end;

  THttpOptions = class(TPersistent)
  private
    FEnabled: boolean;
    FUrl: string;
    FUsername: string;
    FPassword: string;
    FTrustServerCertificate: boolean;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FProxyOptions: TProxyOptions;

    function GetProxyOptions: TProxyOptions;
    procedure SetProxyOptions(Value: TProxyOptions);

    procedure ReadProxyHostname(Reader: TReader);
    procedure ReadProxyPort(Reader: TReader);
    procedure ReadProxyUsername(Reader: TReader);
    procedure ReadProxyPassword(Reader: TReader);

  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;

  public
    function Equals(HttpOptions: THttpOptions): boolean; reintroduce;

    property ProxyOptions: TProxyOptions read GetProxyOptions write SetProxyOptions;

    property Enabled: boolean read FEnabled write FEnabled default False;

  published
    property Url: string read FUrl write FUrl;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property TrustServerCertificate: boolean read FTrustServerCertificate write FTrustServerCertificate default False;
  end;

  TSSLOptions = class(TPersistent)
  private
    FEnabled: boolean;
    FCA: string;
    FCert: string;
    FKey: string;
    FCipher: string;
    FIgnoreServerCertificateValidity: boolean;
    FIgnoreServerCertificateConstraints: boolean;
    FIgnoreServerCertificateInsecurity: boolean;
    FTrustServerCertificate: boolean;
    FTrustSelfSignedCertificate: boolean;
    FForceUseTrustServerCertificate: boolean;
    FServerCertDN: string;
    FIdentityDNSName: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  public
    function Equals(SslOptions: TSSLOptions): boolean; reintroduce;

    property IdentityDNSName: string read FIdentityDNSName write FIdentityDNSName;
    property ServerCertDN: string read FServerCertDN write FServerCertDN;
    property ForceUseTrustServerCertificate: boolean read FForceUseTrustServerCertificate write FForceUseTrustServerCertificate;
    property TrustSelfSignedCertificate: boolean read FTrustSelfSignedCertificate write FTrustSelfSignedCertificate;
  published
    property Enabled: boolean read FEnabled write FEnabled;
    property CA: string read FCA write FCA;
    property Cert: string read FCert write FCert;
    property Key: string read FKey write FKey;
    property Cipher: string read FCipher write FCipher;
    property IgnoreServerCertificateValidity: boolean read FIgnoreServerCertificateValidity write FIgnoreServerCertificateValidity default False;
    property IgnoreServerCertificateConstraints: boolean read FIgnoreServerCertificateConstraints write FIgnoreServerCertificateConstraints default False;
    property IgnoreServerCertificateInsecurity: boolean read FIgnoreServerCertificateInsecurity write FIgnoreServerCertificateInsecurity default False;
    property TrustServerCertificate: boolean read FTrustServerCertificate write FTrustServerCertificate default False;
  end;

  TSSHOptions = class(TPersistent)
  private
    FEnabled: boolean;
    FHostname: string;
    FPort: integer;
    FUsername: string;
    FPassword: string;
    FClientKey: string;
    FClientKeyPassword: string;
    FServerKey: string;
    FPath: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Enabled: boolean read FEnabled write FEnabled;
    property Hostname: string read FHostname write FHostname;
    property Port: integer read FPort write FPort;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property ClientKey: string read FClientKey write FClientKey;
    property ClientKeyPassword: string read FClientKeyPassword write FClientKeyPassword;
    property ServerKey: string read FServerKey write FServerKey;
    property Path: string read FPath write FPath;
  end;

  TAsyncThread = class(TThread)
  private
    FFinished: boolean;
  protected
    FLock: TCriticalSection;
    FResumedEvent: TEvent;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FVio: TCRVio;
    FCallbackProc: AsyncCallback;
    FBuffer: TBytes;
    FOffset: integer;
    FSize: integer;

  public
    constructor Create(out Addr: TAsyncThread; Vio: TCRVio; CallbackProc: AsyncCallback;
      const Buffer: TBytes; Offset, Size: integer);
    destructor Destroy; override;

    procedure Restart(Vio: TCRVio; CallbackProc: AsyncCallback; const Buffer: TBytes; Offset, Size: integer);
    procedure Pause;
  end;

  TAsyncReceiveThread = class(TAsyncThread)
  protected
    procedure Execute; override;
  end;

{ $DEFINE VIO_DEBUG}
{ $DEFINE VIO_TIME}

{$IFDEF VIO_TIME}
var
  VIO_WriteTime: Int64;
  VIO_ReadTime: Int64;
{$ENDIF}

implementation

uses
{$IFDEF VER17P}
  {$WARN SYMBOL_DEPRECATED OFF}
  Types,
{$ENDIF}
  Math,
  CRFunctions;

{$IFDEF VER16P}
{$IFNDEF MSWINDOWS}
function GetCurrentThreadID: TThreadID;
begin
  Result := TThread.CurrentThread.ThreadID;
end;
{$ENDIF}
{$ENDIF}

{ SocketException }

constructor SocketException.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := 0;
end;

constructor SocketException.Create(const Msg: string; ErrorCode: integer{$IFDEF BCB}; AHelpContext: integer = 0{$ENDIF}{$IFDEF VER10P}; AHelpContext: integer = 0{$ENDIF});
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

constructor SocketException.CreateFmt(const Msg: string; const Args: array of const; ErrorCode: integer);
begin
  inherited CreateFmt(Msg, Args);
  FErrorCode := ErrorCode;
end;

{ TAsyncThread }

constructor TAsyncThread.Create(out Addr: TAsyncThread; Vio: TCRVio; CallbackProc: AsyncCallback;
  const Buffer: TBytes; Offset, Size: integer);
begin
  FLock := TCriticalSection.Create;
  FResumedEvent := CreateEvent;
  Restart(Vio, CallbackProc, Buffer, Offset, Size);

  Addr := Self; /// for non-Windows platforms
  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TAsyncThread.Destroy;
begin
  Terminate;
  FResumedEvent.SetEvent;

  inherited;

  FLock.Free;
  FResumedEvent.Free;
end;

procedure TAsyncThread.Restart(Vio: TCRVio; CallbackProc: AsyncCallback;
  const Buffer: TBytes; Offset, Size: integer);
begin
  FVio := Vio;
  FBuffer := Buffer;
  FOffset := Offset;
  FSize := Size;
  FCallbackProc := CallbackProc;
  FResumedEvent.SetEvent;
end;

procedure TAsyncThread.Pause;
begin
  FResumedEvent.ResetEvent;

  FLock.Acquire; // to make sure, that FCallbackProc will not be called more
  FCallbackProc := nil;
  FLock.Release;
end;

{ TAsyncReceiveThread }

procedure TAsyncReceiveThread.Execute;
begin
  try
    try
      while not Terminated do begin
        if (FResumedEvent.WaitFor($FFFFFFFF) = wrSignaled) and not Terminated then begin
        {$IFDEF ANDROID}
          while not FVio.WaitForData(1000) and not Terminated do ;
          if not Terminated then begin
        {$ELSE}
          if FVio.WaitForData(-1) and not Terminated then begin
        {$ENDIF}
            FLock.Acquire;
            try
              if Assigned(FCallbackProc) then
                FCallbackProc(nil);
            finally
              FLock.Release;
            end;
          end;
        end;
      end;
    finally
    {$IFDEF LINUX}
      FVio.InternalClose;
    {$ENDIF}
      FVio.Close;
    end;
  finally
    FCallbackProc := nil;
    FFinished := True;
  end;
end;

{ TCRVio }

constructor TCRVio.Create;
begin
  inherited;

  FEvClose := TCriticalSection.Create;
end;

destructor TCRVio.Destroy;
begin
  try
    Close;
  except
  end;

  FReceiveThread.Free;
  FEvClose.Free;

  inherited;
end;

function TCRVio.GetConnected: boolean;
begin
  Result := not FClosing;
end;

procedure TCRVio.BeforeClosing;
begin
end;

procedure TCRVio.InternalClose;
begin
end;

procedure TCRVio.Close;
  procedure TerminateThreads;
  begin
    if (FReceiveThread <> nil) and (FReceiveThread.ThreadID <> GetCurrentThreadID) then begin
      FReceiveThread.WaitFor;
      FReceiveThread.Free; // don't use FreeAndNil because of multi-threading
      FReceiveThread := nil;
    end;
  end;

begin
  if FClosing then begin
    TerminateThreads;
    Exit;
  end;

  FEvClose.Acquire;
  if FClosing then begin
    FEvClose.Release;
    TerminateThreads;
    Exit;
  end
  else
    FClosing := True;
  FEvClose.Release;

  try
    BeforeClosing;

    if FReceiveThread <> nil then begin
      FReceiveThread.Terminate;
      FReceiveThread.FResumedEvent.SetEvent;
    {$IFNDEF LINUX}
      InternalClose;
    {$ENDIF}
      if FReceiveThread.ThreadID <> GetCurrentThreadID then begin
        FReceiveThread.WaitFor;
        FReceiveThread.Free; // don't use FreeAndNil because of multi-threading
        FReceiveThread := nil;
      end;
    end
    else
      InternalClose;
  finally
    DoOnClose;
  end;
end;

procedure TCRVio.DoOnClose;
var
  OldOnClose: TNotifyEvent;
begin
  FEvClose.Acquire;
  OldOnClose := FOnClose;
  FOnClose := nil;
  FEvClose.Release;

  if Assigned(OldOnClose) then
    try
      OldOnClose(Self);
    except
    end;
end;

function TCRVio.TryConnect: Boolean;
begin
  FClosing := False;

{$IFDEF BUFFERED_READ}
  FBufferLen := 0;
  FBufferPos := 0;
  SetLength(FBuffer, VIO_READ_BUFFER_SIZE);
{$ENDIF}

  Result := False;
end;

function TCRVio.Read(const Buffer: TValueArr; Offset, Count: integer): integer;
{$IFDEF BUFFERED_READ}
  procedure ReadFromBuffer;
  var
    Cnt: integer;
  begin
    if FBufferPos >= FBufferLen then
      Exit;

    Cnt := Min(FBufferLen - FBufferPos, Count);
    Move(FBuffer[FBufferPos], Buffer[Offset], Cnt);

    Inc(Offset, Cnt);
    Inc(Result, Cnt);
    Dec(Count, Cnt);
    Inc(FBufferPos, Cnt);
    if FBufferPos = FBufferLen then begin
      FBufferPos := 0;
      FBufferLen := 0;
    end;
  end;
{$ENDIF}

var
{$IFDEF VIO_DEBUG}
  o, c, c1,
{$ENDIF}
  ReadCount: integer;
{$IFDEF VIO_TIME}
  tc1, tc2: Int64;
{$ENDIF}
begin
{$IFDEF VIO_DEBUG}
try
  o := Offset;
  c := Count;
  // OFS('+' + Format(ClassName + '.Read (%d, %d). Self = %s', [o, c, IntToHex(integer(Self), 8)]));
  OFS('+' + Format(ClassName + '.Read (%d, %d)', [o, c]));
{$ENDIF}
{$IFDEF PERF_COUNTER}
  PerfCounters[1].Start;
{$ENDIF}
{$IFDEF VIO_TIME}
  QueryPerformanceCounter(tc1);
{$ENDIF}

  Result := 0;
{$IFDEF BUFFERED_READ}
  ReadFromBuffer; // read cached data from FBuffer
  if Count = 0 then // All data readed from FBuffer
    Exit;

  Assert(FBufferPos = 0);
  if Count < VIO_UNBUFFERED_READ_MIN_SIZE then begin
    FBufferLen := ReadNoWait(@FBuffer[0], 0, VIO_READ_BUFFER_SIZE);
    if (FBufferLen = 0) and (LastErrorCode <> 0) then
      Exit;

    ReadFromBuffer; // read cached data from FBuffer
  end;
{$ENDIF}

  while Count > 0 do begin
    ReadCount := ReadNoWait(Buffer, Offset, Count);
    if ReadCount = 0 then
      Break;

    Inc(Result, ReadCount);
    Inc(Offset, ReadCount);
    Dec(Count, ReadCount);
  end;

{$IFDEF VIO_TIME}
  QueryPerformanceCounter(tc2);
  VIO_ReadTime := VIO_ReadTime + tc2 - tc1;
{$ENDIF}
{$IFDEF PERF_COUNTER}
  PerfCounters[1].Stop;
{$ENDIF}
{$IFDEF VIO_DEBUG}
finally
  OFS('-' + Format(ClassName + '.Read (%d, %d) = %d', [o, c, Result]));
  c1 := Result;
  if c1 > 200 then
    c1 := 200;
  OFS(Copy(Buffer, o, c1));
end;
{$ENDIF}
end;

function TCRVio.Write(const Buffer: TValueArr; Offset, Count: integer): integer;
var
  WriteCount: integer;
  RetryCount: integer;
{$IFDEF VIO_DEBUG}
  o, c, c1: integer;
{$ENDIF}
{$IFDEF VIO_TIME}
  tc1, tc2: Int64;
{$ENDIF}
begin
{$IFDEF VIO_DEBUG}
  try
    o := Offset;
    c := Count;
    //OFS('+' + Format(ClassName + '.Write (%d, %d). Self = %s', [o, c, IntToHex(integer(Self), 8)]));
    OFS('+' + Format(ClassName + '.Write (%d, %d)', [o, c]));
    c1 := c;
    if c > 200 then
      c1 := 200;
    OFS(Buffer + o, c1);
{$ENDIF}
{$IFDEF VIO_TIME}
  QueryPerformanceCounter(tc1);
{$ENDIF}
{$IFDEF PERF_COUNTER}
  PerfCounters[1].Start;
{$ENDIF}

  RetryCount := 3;
  Result := 0;

  while Count > 0 do begin
    WriteCount := WriteNoWait(Buffer, Offset, Count);

    if WriteCount <= 0 then begin
      if LastErrorCode <> 0 then
        Exit;

      Dec(RetryCount);
      if RetryCount = 0 then
        Exit;
      Sleep(10); // silent error handling
    end
    else begin
      Inc(Result, WriteCount);
      Inc(Offset, WriteCount);
      Dec(Count, WriteCount);
      RetryCount := 3;
    end;
  end;

{$IFDEF VIO_TIME}
  QueryPerformanceCounter(tc2);
  VIO_WriteTime := VIO_WriteTime + tc2 - tc1;
{$ENDIF}
{$IFDEF PERF_COUNTER}
  PerfCounters[1].Stop;
{$ENDIF}
{$IFDEF VIO_DEBUG}
  finally
    OFS('-' + Format(ClassName + '.Write (%d, %d) = %d', [o, c, Result]));
  end;
{$ENDIF}
end;

function TCRVio.WaitForData(MillisecondsTimeout: integer = -1): boolean;
begin
  Assert(False);
  Result := False;
end;

function TCRVio.GetAvailable: integer;
begin
  if WaitForData(0) then
    Result := 1
  else
    Result := 0;
end;

function TCRVio.BeginReceive(var Buffer: TBytes; const Offset, Size: integer;
  Callback: AsyncCallback; State: TObject): IScAsyncResult;
begin
  if (FReceiveThread <> nil) and Assigned(FReceiveThread.FCallbackProc) then
    raise Exception.Create(SVioError);

  Result := nil;
  if FReceiveThread = nil then
    FReceiveThread := TAsyncReceiveThread.Create(FReceiveThread, Self, Callback, Buffer, Offset, Size)
  else
    FReceiveThread.Restart(Self, Callback, Buffer, Offset, Size);
end;

function TCRVio.EndReceive(AsyncResult: IScAsyncResult): integer;
begin
  if (FReceiveThread = nil) or not Assigned(FReceiveThread.FCallbackProc) then
    raise Exception.Create(SVioError);

  Result := 0;
  if FReceiveThread <> nil then begin
    FReceiveThread.Pause;
    if GetConnected then
      Result := ReadNoWait(TValueArr(FReceiveThread.FBuffer), FReceiveThread.FOffset, FReceiveThread.FSize)
    else
      Result := 0;

    if Result <= 0 then begin
      FReceiveThread.Terminate;
      if (FLastError <> '') and not FClosing then
        raise SocketException.Create(FLastError, FLastErrorCode);
    end;
  end;
end;

procedure TCRVio.StopAsync;
begin
  if FReceiveThread <> nil then begin
    FReceiveThread.Terminate;
    FReceiveThread.FResumedEvent.SetEvent;
  end;
end;

procedure TCRVio.SetNonBlocking(Value: boolean);
begin
  if Value <> FNonBlocking then begin
    FNonBlocking := Value;

    if FNonBlocking then begin
      if FReceiveThread = nil then
        FReceiveThread := TAsyncReceiveThread.Create(FReceiveThread, Self, OnReceive, nil, 0, 0)
      else
        FReceiveThread.Restart(Self, OnReceive, nil, 0, 0);
    end
    else begin
      if FReceiveThread <> nil then
        FReceiveThread.Pause;
    end;
  end
  else
  if not Value and (FReceiveThread <> nil) then
    FReceiveThread.Pause;
end;

procedure TCRVio.OnReceive(ar: IScAsyncResult);
begin
  if Assigned(FOnAsyncReceive) then
    FOnAsyncReceive(Self);
end;

function TCRVio.GetTimeout: integer;
begin
  Result := GetReceiveTimeout;
end;

procedure TCRVio.SetTimeout(Value: integer);
begin
  SetReceiveTimeout(Value);
  SetConnectionTimeout(Value);
end;

function TCRVio.GetConnectionTimeout: integer;
begin
  Result := 0;
end;

procedure TCRVio.SetConnectionTimeout(Value: integer);
begin
end;

function TCRVio.GetSendTimeout: integer;
begin
  Result := 0;
end;

procedure TCRVio.SetSendTimeout(Value: integer);
begin
end;

function TCRVio.GetReceiveTimeout: integer;
begin
  Result := 0;
end;

procedure TCRVio.SetReceiveTimeout(Value: integer);
begin
end;

{ TCRIOHandler }

constructor TCRIOHandler.Create(AOwner: TComponent);
begin
  inherited;

  FList := TCRList.Create;
end;

destructor TCRIOHandler.Destroy;
begin
  inherited;

  FList.Free;
end;

procedure TCRIOHandler.Disconnect(Handle: TCRIOHandle);
begin
  Assert(False, SNotOverriden);
end;

class procedure TCRIOHandler.Renegotiate(Handle: TCRIOHandle);
begin
  Assert(False, SNotOverriden);
end;

class function TCRIOHandler.ReadNoWait(Handle: TCRIOHandle; const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  Assert(False, SNotOverriden);
  Result := 0;
end;

class function TCRIOHandler.Read(Handle: TCRIOHandle; const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  Assert(False, SNotOverriden);
  Result := 0;
end;

class function TCRIOHandler.Write(Handle: TCRIOHandle; const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  Assert(False, SNotOverriden);
  Result := 0;
end;

class function TCRIOHandler.WaitForData(Handle: TCRIOHandle; MillisecondsTimeout: integer = -1): boolean;
begin
  Assert(False, SNotOverriden);
  Result := False;
end;

class function TCRIOHandler.GetTimeout(Handle: TCRIOHandle): integer;
begin
  Assert(False, SNotOverriden);
  Result := 0;
end;

class procedure TCRIOHandler.SetTimeout(Handle: TCRIOHandle; Value: integer);
begin
  Assert(False, SNotOverriden);
end;

procedure TCRIOHandler.RegisterClient(Client: TObject);
begin
  FList.Add(Client);
end;

procedure TCRIOHandler.UnRegisterClient(Client: TObject);
begin
  FList.Remove(Client);
end;

class procedure TCRIOHandler.SetIsSecure(Handle: TCRIOHandle; const Value: Boolean);
begin
end;

class function TCRIOHandler.GetIsSecure(Handle: TCRIOHandle): Boolean;
begin
  Result := False;
end;

function TCRIOHandler.GetHandlerType: string;
begin
  Result := '';
end;

{ TCRVioHandler }

constructor TCRVioHandler.Create(const Hostname: string; const Port: integer;
  IOHandler: TCRIOHandler; HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
  SSLOptions: TSSLOptions; SSHOptions: TSSHOptions;
  IPVersion: TIPVersion = ivIPv4);
begin
  inherited Create;

  FHostname := Hostname;
  FPort := Port;
  FIPVersion := IPVersion;
  FIOHandler := IOHandler;
  Assert(FIOHandler <> nil);

  FHttpOptions := THttpOptions.Create;
  FProxyOptions := TProxyOptions.Create;
  FSSLOptions := TSSLOptions.Create;
  FSSHOptions := TSSHOptions.Create;

  if HttpOptions <> nil then
    FHttpOptions.Assign(HttpOptions);
  if ProxyOptions <> nil then
    FProxyOptions.Assign(ProxyOptions);
  if SSLOptions <> nil then
    FSSLOptions.Assign(SSLOptions);
  if SSHOptions <> nil then
    FSSHOptions.Assign(SSHOptions);
end;

destructor TCRVioHandler.Destroy;
begin
  inherited;
  FHandle.Free;
  FHttpOptions.Free;
  FProxyOptions.Free;
  FSSLOptions.Free;
  FSSHOptions.Free;
end;

function TCRVioHandler.GetTimeout: integer;
begin
  if FHandle <> nil then
    Result := FIOHandler.GetTimeout(FHandle)
  else
    Result := FTimeout;
end;

procedure TCRVioHandler.SetTimeout(Value: integer);
begin
  if FHandle <> nil then
    FIOHandler.SetTimeout(FHandle, Value)
  else
    FTimeout := Value;
end;

function TCRVioHandler.GetReceiveTimeout: integer;
begin
  Result := GetTimeout;
end;

procedure TCRVioHandler.SetReceiveTimeout(Value: integer);
begin
  SetTimeout(Value);
end;

function TCRVioHandler.ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if FHandle <> nil then // To avoid assertion on closing connection
    Result := FIOHandler.ReadNoWait(FHandle, Buffer, Offset, Count)
  else
    Result := 0;
end;

function TCRVioHandler.WriteNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if FHandle <> nil then // To avoid assertion on closing connection
    Result := FIOHandler.Write(FHandle, Buffer, Offset, Count)
  else
    Result := 0;
end;

procedure TCRVioHandler.Connect;
begin
  TryConnect;

  FreeAndNil(FHandle);
  FHandle := FIOHandler.Connect(FHostname, FPort, FHttpOptions, FProxyOptions,
    FSSLOptions, FSSHOptions, FIPVersion);
  if FTimeout > 0 then
    FIOHandler.SetTimeout(FHandle, FTimeout);
end;

procedure TCRVioHandler.Close;
begin
  if FHandle <> nil then // To avoid assertion on closing connection
    FIOHandler.Disconnect(FHandle);
end;

procedure TCRVioHandler.Renegotiate;
begin
  if FHandle <> nil then
    FIOHandler.Renegotiate(FHandle);
end;

function TCRVioHandler.BeginReceive(var Buffer: TBytes; const Offset, Size: integer; Callback: AsyncCallback; State: TObject): IScAsyncResult;
begin
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
  // Async Receive is forbidden
  raise Exception.Create(SVioError);
end;

function TCRVioHandler.EndReceive(AsyncResult: IScAsyncResult): integer;
begin
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
  raise Exception.Create(SVioError);
end;

function TCRVioHandler.Read(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if FHandle <> nil then // To avoid assertion on closing connection
    Result := inherited Read(Buffer, Offset, Count)
  else
    Result := 0;
end;

function TCRVioHandler.Write(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if FHandle <> nil then // To avoid assertion on closing connection
    Result := FIOHandler.Write(FHandle, Buffer, Offset, Count)
  else
    Result := 0;
end;

function TCRVioHandler.WaitForData(MillisecondsTimeout: integer = -1): boolean;
begin
  if FHandle <> nil then // To avoid assertion on closing connection
    Result := FIOHandler.WaitForData(FHandle, MillisecondsTimeout)
  else
    Result := False;
end;

procedure TCRVioHandler.SetIsSecure(const Value: Boolean);
begin
  if FHandle <> nil then
    FIOHandler.SetIsSecure(FHandle, Value);
end;

function TCRVioHandler.GetIsSecure: Boolean;
begin
  if FHandle <> nil then
    Result := FIOHandler.GetIsSecure(FHandle)
  else
    Result := False;
end;

{ TCRIOHandlerUtils }

class procedure TCRIOHandlerUtils.RegisterClient(Obj: TCRIOHandler; Client: TObject);
begin
  Obj.RegisterClient(Client);
end;

class procedure TCRIOHandlerUtils.UnRegisterClient(Obj: TCRIOHandler; Client: TObject);
begin
  Obj.UnRegisterClient(Client);
end;

{ TProxyOptions }

constructor TProxyOptions.Create;
begin
  inherited;

  FSocksVersion := DefValSocksVersion;
  FResolveDNS := DefValResolveDNS;
end;

procedure TProxyOptions.AssignTo(Dest: TPersistent);
begin
  if Dest.ClassName = TProxyOptions.ClassName then begin
    TProxyOptions(Dest).FHostname := FHostname;
    TProxyOptions(Dest).FPort := FPort;
    TProxyOptions(Dest).FUsername := FUsername;
    TProxyOptions(Dest).FPassword := FPassword;
    TProxyOptions(Dest).FSocksVersion := FSocksVersion;
    TProxyOptions(Dest).FResolveDNS := FResolveDNS;
  end
  else
    inherited;
end;

function TProxyOptions.Equals(ProxyOptions: TProxyOptions): boolean;
begin
  Result := False;
  if ProxyOptions <> nil then
    Result :=
      (Hostname = ProxyOptions.Hostname) and
      (Port = ProxyOptions.Port) and
      (Username = ProxyOptions.Username) and
      (Password = ProxyOptions.Password) and
      (SocksVersion = ProxyOptions.SocksVersion);
end;

{ THttpOptions }

procedure THttpOptions.AssignTo(Dest: TPersistent);
begin
  if GetIsClassByName(Dest, THttpOptions.ClassName) then begin
    THttpOptions(Dest).FEnabled := FEnabled;
    THttpOptions(Dest).FUrl := FUrl;
    THttpOptions(Dest).FUsername := FUsername;
    THttpOptions(Dest).FPassword := FPassword;
    THttpOptions(Dest).FTrustServerCertificate := FTrustServerCertificate;

    if (THttpOptions(Dest).FProxyOptions <> nil) and (FProxyOptions <> nil) then
      THttpOptions(Dest).FProxyOptions.Assign(FProxyOptions);
  end
  else
    inherited;
end;

procedure THttpOptions.ReadProxyHostname(Reader: TReader);
begin
  if FProxyOptions <> nil then
    FProxyOptions.Hostname := Reader.ReadString;
end;

procedure THttpOptions.ReadProxyPort(Reader: TReader);
begin
  if FProxyOptions <> nil then
    FProxyOptions.Port := Reader.ReadInteger;
end;

procedure THttpOptions.ReadProxyUsername(Reader: TReader);
begin
  if FProxyOptions <> nil then
    FProxyOptions.Username := Reader.ReadString;
end;

procedure THttpOptions.ReadProxyPassword(Reader: TReader);
begin
  if FProxyOptions <> nil then
    FProxyOptions.Password := Reader.ReadString;
end;

procedure THttpOptions.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('HttpOptions.ProxyOptions.Hostname', ReadProxyHostname, nil, False);
  Filer.DefineProperty('HttpOptions.ProxyOptions.Port', ReadProxyPort, nil, False);
  Filer.DefineProperty('HttpOptions.ProxyOptions.Username', ReadProxyUsername, nil, False);
  Filer.DefineProperty('HttpOptions.ProxyOptions.Password', ReadProxyPassword, nil, False);
end;

function THttpOptions.GetProxyOptions: TProxyOptions;
begin
  if FProxyOptions = nil then
    raise ArgumentException.Create('ProxyOptions');

  Result := FProxyOptions;
end;

procedure THttpOptions.SetProxyOptions(Value: TProxyOptions);
begin
  FProxyOptions := Value;
end;

function THttpOptions.Equals(HttpOptions: THttpOptions): boolean;
begin
  Result := False;
  if HttpOptions <> nil then
    Result :=
      (Url = HttpOptions.Url) and
      (Username = HttpOptions.Username) and
      (Password = HttpOptions.Password) and
      (TrustServerCertificate = HttpOptions.TrustServerCertificate);
end;

{ TSSLOptions }

constructor TSSLOptions.Create;
begin
  inherited;

  FIgnoreServerCertificateValidity := False;
  FIgnoreServerCertificateConstraints := False;
  FIgnoreServerCertificateInsecurity := False;
  FTrustServerCertificate := False;
  FTrustSelfSignedCertificate := False;
  FForceUseTrustServerCertificate := False;
end;

procedure TSSLOptions.AssignTo(Dest: TPersistent);
begin
  if Dest.ClassName = TSSLOptions.ClassName then begin
    TSSLOptions(Dest).FEnabled := FEnabled;
    TSSLOptions(Dest).FCA := FCA;
    TSSLOptions(Dest).FCert := FCert;
    TSSLOptions(Dest).FKey := FKey;
    TSSLOptions(Dest).FCipher := FCipher;

    TSSLOptions(Dest).FIgnoreServerCertificateValidity := FIgnoreServerCertificateValidity;
    TSSLOptions(Dest).FIgnoreServerCertificateConstraints := FIgnoreServerCertificateConstraints;
    TSSLOptions(Dest).FIgnoreServerCertificateInsecurity := FIgnoreServerCertificateInsecurity;
    TSSLOptions(Dest).FTrustServerCertificate := FTrustServerCertificate;
    TSSLOptions(Dest).FTrustSelfSignedCertificate := FTrustSelfSignedCertificate;
    TSSLOptions(Dest).FForceUseTrustServerCertificate := FForceUseTrustServerCertificate;

    TSSLOptions(Dest).FServerCertDN := FServerCertDN;
    TSSLOptions(Dest).FIdentityDNSName := FIdentityDNSName;
  end
  else
    inherited;
end;

function TSSLOptions.Equals(SslOptions: TSSLOptions): boolean;
begin
  Result := False;
  if SslOptions <> nil then
    Result :=
      (SslOptions.FEnabled = FEnabled) and
      (SslOptions.FCA = FCA) and
      (SslOptions.FCert = FCert) and
      (SslOptions.FKey = FKey) and
      (SslOptions.FCipher = FCipher) and

      (SslOptions.FIgnoreServerCertificateValidity = FIgnoreServerCertificateValidity) and
      (SslOptions.FIgnoreServerCertificateConstraints = FIgnoreServerCertificateConstraints) and
      (SslOptions.FIgnoreServerCertificateInsecurity = FIgnoreServerCertificateInsecurity) and
      (SslOptions.FTrustServerCertificate = FTrustServerCertificate) and
      (SslOptions.FTrustSelfSignedCertificate = FTrustSelfSignedCertificate) and
      (SslOptions.FForceUseTrustServerCertificate = FForceUseTrustServerCertificate) and

      (SslOptions.FServerCertDN = FServerCertDN) and
      (SslOptions.FIdentityDNSName = FIdentityDNSName);
end;

{ TSSHOptions }

procedure TSSHOptions.AssignTo(Dest: TPersistent);
begin
  if Dest.ClassName = TSSHOptions.ClassName then begin
    TSSHOptions(Dest).FEnabled := FEnabled;
    TSSHOptions(Dest).FHostname := FHostname;
    TSSHOptions(Dest).FPort := FPort;
    TSSHOptions(Dest).FUsername := FUsername;
    TSSHOptions(Dest).FPassword := FPassword;
    TSSHOptions(Dest).FClientKey := FClientKey;
    TSSHOptions(Dest).FClientKeyPassword := FClientKeyPassword;
    TSSHOptions(Dest).FServerKey := FServerKey;
    TSSHOptions(Dest).FPath := FPath;
  end
  else
    inherited;
end;

end.
