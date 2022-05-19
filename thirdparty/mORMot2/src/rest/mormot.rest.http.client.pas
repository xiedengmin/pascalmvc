/// REpresentation State Tranfer (REST) HTTP Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.http.client;

{
  *****************************************************************************

   Client-Side REST Process over HTTP/WebSockets
    - TRestHttpClientGeneric and TRestHttpClientRequest Parent Classes
    - TRestHttpClientSocket REST Client Class over Sockets
    - TRestHttpClientWebsockets REST Client Class over WebSockets
    - TRestHttpClientWinINet TRestHttpClientWinHttp Windows REST Client Classes
    - TRestHttpClientCurl REST Client Class over LibCurl
    - TRestHttpClient/TRestHttpClients Main Usable Classes

  *****************************************************************************
}

interface

{.$define NOHTTPCLIENTWEBSOCKETS}
{ if defined, TRestHttpClientWebSockets won't be declared
  - will avoid to link mormot.net.ws.* units }

{.$define VERBOSECLIENTLOG}
// if defined, TRestHttpClientSocket will log low-level THttpClientSocket info

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.crypt.core,
  mormot.core.zip,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.search,
  mormot.crypt.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.soa.core,
  mormot.soa.client,
  mormot.db.core,
  mormot.rest.core,
  mormot.rest.client,
  mormot.net.sock,
  mormot.net.http,
  {$ifndef NOHTTPCLIENTWEBSOCKETS}
  mormot.net.ws.core,
  mormot.net.ws.client,
  {$endif NOHTTPCLIENTWEBSOCKETS}
  mormot.net.client;


{ ************ TRestHttpClientGeneric and TRestHttpClientRequest Parent Classes }

type
  ERestHttpClient = class(ERestException);

  /// available compression algorithms for transmission
  // - SynLZ is faster then Deflate, but not standard: use hcSynLZ for Delphi
  // clients, but hcDeflate for AJAX or any HTTP clients
  // - with hcSynLZ, the 440 KB JSON for TTestClientServerAccess._TRestHttpClient
  // is compressed into 106 KB with no speed penalty (it's even a bit faster)
  // whereas hcDeflate with its level set to 1 (fastest), is 25 % slower
  // - here hcDeflate will use in fact gzip content encoding, since deflate
  // is inconsistent between browsers: http://stackoverflow.com/a/9186091
  // - TRestHttpClientGeneric.Compression default property is [hcSynLZ]
  // - deprecated hcSynShaAes used SHA-256/AES-256-CFB to encrypt the content
  // (after SynLZ compression), but has been audited as weak so standard HTTPS
  // is to be used instead
  TRestHttpCompression = (
    hcSynLZ,
    hcDeflate
    {$ifndef PUREMORMOT2}
    , hcSynShaAes
    {$endif PUREMORMOT2}
    );

  /// set of available compressions schemes
  TRestHttpCompressions = set of TRestHttpCompression;

  /// abstract HTTP/1.1 RESTful JSON mORMot Client class
  // - this class, and other inherited classes defined in this unit, are
  // thread-safe, since each of their Uri() method is protected by a giant lock
  TRestHttpClientGeneric = class(TRestClientUri)
  protected
    fKeepAliveMS: cardinal;
    fCompression: TRestHttpCompressions;
    fUriPrefix: RawUtf8;
    fCustomHeader: RawUtf8;
    /// connection parameters as set by Create()
    fServer, fPort: RawUtf8;
    fHttps: boolean;
    fProxyName, fProxyByPass: RawUtf8;
    fSendTimeout, fReceiveTimeout, fConnectTimeout: cardinal;
    fExtendedOptions: THttpRequestExtendedOptions;
    procedure SetCompression(Value: TRestHttpCompressions);
    procedure SetKeepAliveMS(Value: cardinal);
    /// process low-level HTTP/1.1 request
    // - called by InternalUri(), therefore by Uri() public method
    // - returns 200,202,204 if OK, http status error otherwise in result.Lo
    // - returns Server-InternalState in result.Hi
    function InternalRequest(const url, method: RawUtf8;
      var Header, Data, DataType: RawUtf8): Int64Rec; virtual; abstract;
    /// method calling the RESTful server fServer via HTTP/1.1
    // - calls the InternalRequest() protected method
    procedure InternalUri(var Call: TRestUriParams); override;
  public
    /// connect to TRestHttpServer on aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy - note
    // that proxy parameters are currently not available for TRestHttpClientSocket
    // - you can customize the default client timeouts by setting appropriate
    // ConnectTimeout, SendTimeout and ReceiveTimeout parameters (in ms) - if
    // you left the 0 default parameters, it would use global
    // HTTP_DEFAULT_CONNECTTIMEOUT, HTTP_DEFAULT_SENDTIMEOUT and
    // HTTP_DEFAULT_RECEIVETIMEOUT variable values
    // - TRestHttpClientSocket handles aServer='unix:/run/mormotapp.sock' on POSIX
    constructor Create(const aServer, aPort: RawUtf8; aModel: TOrmModel;
      aHttps: boolean = false; const aProxyName: RawUtf8 = '';
      const aProxyByPass: RawUtf8 = ''; aSendTimeout: cardinal = 0;
      aReceiveTimeout: cardinal = 0; aConnectTimeout: cardinal = 0);
       reintroduce; overload; virtual;
    /// connect to TRestHttpServer via 'address:port/root' URI format
    // - if port is not specified, aDefaultPort is used
    // - if root is not specified, aModel.Root is used
    constructor Create(const aServer: TRestServerUriString; aModel: TOrmModel;
      aDefaultPort: integer; aHttps: boolean = false); reintroduce; overload;
    /// initialize REST server instance from a TSynConnectionDefinition
    constructor RegisteredClassCreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition;
      aServerHandleAuthentication: boolean); override;
    /// connnect to a LogView HTTP Server for remote logging
    // - will associate the EchoCustom callback of the log class to this server
    // - the aLogClass.Family will manage this TRestHttpClientGeneric instance
    // life time, until application is closed or Family.EchoRemoteStop is called
    constructor CreateForRemoteLogging(const aServer: RawUtf8;
      aLogClass: TSynLogClass; aPort: Integer = 8091;
      const aRoot: RawUtf8 = 'LogService');
    /// save the TRestHttpClientGeneric properties into a persistent storage object
    // - CreateFrom() will expect Definition.ServerName to store the URI as
    // 'server:port' or 'https://server:port', Definition.User/Password to store
    // the TRestClientUri.SetUser() information, and Definition.DatabaseName
    // to store the extended options as an URL-encoded string
    procedure DefinitionTo(Definition: TSynConnectionDefinition); override;

    /// returns 'Server:Port' current value
    function HostName: RawUtf8;
    /// optional custom HTTP "User Agent:" header value
    property UserAgent: RawUtf8
      read fExtendedOptions.UserAgent write fExtendedOptions.UserAgent;
  published
    /// the Server IP address
    property Server: RawUtf8
      read fServer;
    /// optional URI prefix appended to the REST computed URI
    // - is set from the Server parameter to the Create constructor, e.g.
    // 'hostname/sub/proxy/uri/' will set Server='hostname' and UriPrefix=
    // 'sub/proxy/uri/' which will make 'sub/proxy/uri/root/table/1' call e.g.
    // - could be used e.g. when a reverse proxy is setup with no DNS sub-domain
    // but a per-URI redirection to the actual mormot server
    property UriPrefix: RawUtf8
      read fUriPrefix write fUriPrefix;
    /// optional header transmitted with each REST client request
    // - can be used e.g. to add "Authentication: Bearer xxxxxxxxxx" token
    // using AuthorizationBearer() from mormot.net.http.pas
    property CustomHeader: RawUtf8
      read fCustomHeader write fCustomHeader;
    /// the Server IP port
    property Port: RawUtf8
      read fPort;
    /// the time (in milliseconds) to keep the connection alive with the
    // TRestHttpServer
    // - default is 20000, i.e. 20 seconds
    property KeepAliveMS: cardinal
      read fKeepAliveMS write SetKeepAliveMS;
    /// the compression algorithms usable with this client
    // - equals [hcSynLZ] by default, since our SynLZ algorithm provides a good
    // compression, with very low CPU use on server side
    // - you may include hcDeflate, which will have a better compression ratio,
    // be recognized by all browsers and libraries, but would consume much
    // more CPU resources than hcSynLZ
    // - hcSynShaAes is weak and deprecated, so should not be used on production
    // - for fast and safe communication between stable mORMot nodes, consider
    // using TRestHttpClientWebSockets, leaving hcDeflate for AJAX or non mORMot
    // clients, and hcSynLZ if you expect to have mORMot client(s)
    property Compression: TRestHttpCompressions
      read fCompression write SetCompression;
  end;

  TRestHttpClientGenericClass = class of TRestHttpClientGeneric;


  /// HTTP/1.1 RESTful JSON mORMot Client abstract class using either WinINet,
  // WinHttp or libcurl API
  // - not to be called directly, but via TRestHttpClientWinINet or (even
  // better) TRestHttpClientWinHttp overridden classes under Windows
  TRestHttpClientRequest = class(TRestHttpClientGeneric)
  protected
    fRequest: THttpRequest;
    fRequestClass: THttpRequestClass;
    /// call fWinAPI.Request()
    function InternalRequest(const url, method: RawUtf8;
      var Header, Data, DataType: RawUtf8): Int64Rec; override;
    /// overridden protected method to handle HTTP connection
    function InternalIsOpen: boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    /// set the fWinAPI class
    // - the overridden implementation should set the expected fWinAPIClass
    procedure InternalSetClass; virtual; abstract;
  public
    /// internal class instance used for the connection
    // - will return either a TWinINet, a TWinHttp or a TCurlHttp class instance
    property Request: THttpRequest
      read fRequest;
    /// allows to ignore untrusted SSL certificates
    // - similar to adding a security exception for a domain in the browser
    property IgnoreSSLCertificateErrors: boolean
      read fExtendedOptions.IgnoreSSLCertificateErrors
      write fExtendedOptions.IgnoreSSLCertificateErrors;
    /// optional Authentication Scheme
    property AuthScheme: THttpRequestAuthentication
      read fExtendedOptions.Auth.Scheme
      write fExtendedOptions.Auth.Scheme;
    /// optional User Name for Authentication
    property AuthUserName: SynUnicode
      read fExtendedOptions.Auth.UserName
      write fExtendedOptions.Auth.UserName;
    /// optional Password for Authentication
    property AuthPassword: SynUnicode
      read fExtendedOptions.Auth.Password
      write fExtendedOptions.Auth.Password;
  end;

  /// meta-class of TRestHttpClientRequest types
  TRestHttpClientRequestClass = class of TRestHttpClientRequest;


{ ************ TRestHttpClientSocket REST Client Class over Sockets }

  /// HTTP/1.1 RESTful JSON mORMot Client class using mormot.net.client socket
  // - can use regular HTTP/HTTPS connection, or Unix Domain Sockets supplying
  // aServer='unix:/run/mormotapp.sock' to is Create() constructor
  // - note that, in its current implementation, this class is not thread-safe:
  // you need either to lock its access via a critical section, or initialize
  // one client instance per thread
  TRestHttpClientSocket = class(TRestHttpClientGeneric)
  protected
    /// internal HTTP/1.1 compatible client
    fSocketClass: THttpClientSocketClass;
    /// either THttpClientSocket or THttpClientWebSockets
    fSocket: THttpClientSocket;
    /// call fSocket.Request()
    function InternalRequest(const url, method: RawUtf8;
      var Header, Data, DataType: RawUtf8): Int64Rec; override;
    /// overridden protected method to handle HTTP connection
    function InternalIsOpen: boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
  published
    /// internal HTTP/1.1 compatible client
    // - can be used e.g. to access SendTimeout and ReceiveTimeout properties
    property Socket: THttpClientSocket
      read fSocket;
  end;



{ ************ TRestHttpClientWebsockets REST Client Class over WebSockets }

{$ifndef NOHTTPCLIENTWEBSOCKETS}

  /// HTTP/1.1 RESTful JSON mORMot Client able to upgrade to WebSockets
  // - in addition to TRestHttpClientSocket, this client class is able
  // to upgrade its HTTP connection to the WebSockets protocol, so that the
  // server may be able to notify the client via a callback
  // - the internal Socket class will be in fact a THttpClientWebSockets
  // instance, as defined in the mormot.net.ws.client unit
  TRestHttpClientWebsockets = class(TRestHttpClientSocket)
  protected
    fWebSocketParams: record
      AutoUpgrade: boolean;
      Ajax: boolean;
      BinaryOptions: TWebSocketProtocolBinaryOptions;
      Key: RawUtf8;
    end;
    fOnWebSocketsUpgraded: TOnClientNotify;
    fOnWebSocketsClosed: TNotifyEvent;
    fWebSocketLoopDelay: integer;
    fWebSocketProcessSettings: TWebSocketProcessSettings;
    fUpgradeCount: integer;
    function CallbackRequest(
      Ctxt: THttpServerRequestAbstract): cardinal; virtual;
    procedure InternalOpen; override;
  public
    /// connect to TRestHttpServer on aServer:aPort
    // - this overriden method will handle properly WebSockets settings
    constructor Create(const aServer, aPort: RawUtf8; aModel: TOrmModel;
      aHttps: boolean = false; const aProxyName: RawUtf8 = '';
      const aProxyByPass: RawUtf8 = ''; aSendTimeout: cardinal = 0;
      aReceiveTimeout: cardinal = 0; aConnectTimeout: cardinal = 0); override;
    /// overriden method which will auto-upgrade the WebSockets if needed
    function IsOpen: boolean; override;
    /// upgrade the HTTP client connection to a specified WebSockets protocol
    // - the Model.Root URI will be used for upgrade
    // - if aWebSocketsAjax equals default FALSE, it will use 'synopsebinary'
    // i.e. TWebSocketProtocolBinaryprotocol, with AES-CFB 256 bits encryption
    // if the encryption key text is not '' and optional SynLZ compression
    // - if aWebSocketsAjax is TRUE, it will register the slower and less secure
    // 'synopsejson' mode, i.e. TWebSocketProtocolJson (to be used for AJAX
    // debugging/test purposes only) and aWebSocketsEncryptionKey /
    // aWebSocketsBinaryOptions parameters will be ignored
    // - once upgraded, the client would automatically re-upgrade any new
    // HTTP client link on automatic reconnection, so that use of this class
    // should be not tied to a particular TCP/IP socket - use OnWebsocketsUpgraded
    // event to perform any needed initialization set, e.g. SOA real-time
    // callbacks registration
    // - will return '' on success, or an error message on failure
    function WebSocketsUpgrade(const aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions =
        [pboSynLzCompress]; aRaiseExceptionOnFailure: ESynExceptionClass = nil): RawUtf8;
    /// connect using a specified WebSockets protocol
    // - this method would call WebSocketsUpgrade, then ServerTimestampSynchronize
    // - it therefore expects SetUser() to have been previously called
    function WebSocketsConnect(const aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions =
        [pboSynLzCompress]): RawUtf8;
    /// internal HTTP/1.1 and WebSockets compatible client
    // - you could use its properties after upgrading the connection to WebSockets
    function WebSockets: THttpClientWebSockets;
    /// returns true if the connection is a running WebSockets
    // - may be false even if fSocket<>nil, e.g. when gracefully disconnected
    function WebSocketsConnected: boolean;
    /// will set the HTTP header as expected by THttpClientWebSockets.Request to
    // perform the Callback() query in wscNonBlockWithoutAnswer mode
    procedure CallbackNonBlockingSetHeader(out Header: RawUtf8); override;
    /// used to handle an interface parameter as SOA callback
    function FakeCallbackRegister(Sender: TServiceFactory;
      const Method: TInterfaceMethod; const ParamInfo: TInterfaceMethodArgument;
      ParamValue: Pointer): TRestClientCallbackID; override;
    /// used to finalize an interface parameter as SOA callback
    function FakeCallbackUnregister(Factory: TInterfaceFactory;
      FakeCallbackID: TRestClientCallbackID; Instance: pointer): boolean; override;
    /// this event will be executed just after the HTTP client has been
    // upgraded to the expected WebSockets protocol
    // - supplied Sender parameter will be this TRestHttpClientWebsockets instance
    // - it will be executed the first time, and also on each reconnection
    // occuring when the HTTP-TCP/IP link is re-created, and user re-authenticated
    // - this event handler is the right place to setup link-driven connection,
    // e.g. SOA real-time callbacks registration (using Sender.Services)
    property OnWebSocketsUpgraded: TOnClientNotify
      read fOnWebSocketsUpgraded write fOnWebSocketsUpgraded;
    /// this event handler will be executed when the WebSocket link is destroyed
    // - may happen e.g. after graceful close from the server side, or
    // after DisconnectAfterInvalidHeartbeatCount is reached
    property OnWebSocketsClosed: TNotifyEvent
      read fOnWebSocketsClosed write fOnWebSocketsClosed;
    /// customize the internal REST loop delay
    // - to be defined before WebSocketsUpdate/WebSocketsConnect
    // - will set TWebSocketProcessSettings.LoopDelay value at WebSocketsUpgrade
    // - will override LoopDelay from DefaultWebSocketProcessSettings
    property WebSocketLoopDelay: integer
      read fWebSocketLoopDelay write fWebSocketLoopDelay;
    /// allow to customize the WebSockets processing
    // - apply to all protocols on this client instance
    function Settings: PWebSocketProcessSettings;
      {$ifdef HASINLINE}inline;{$endif}
  published
    /// how many times the connection has been upgraded
    // - reconnections may occur on a weak link - see Settings^.ClientAutoUpgrade
    property UpgradeCount: integer
      read fUpgradeCount;
  end;

{$endif NOHTTPCLIENTWEBSOCKETS}


{ ************ TRestHttpClientWinINet TRestHttpClientWinHttp Windows REST Client Classes }

{$ifdef USEWININET}

  /// HTTP/1.1 RESTful JSON mORMot Client class using WinINet API
  // - this class is 15/20 times slower than TRestHttpClient using
  // mormot.net.client socket on a local machine
  // - this class is able to connect via the secure HTTPS protocol
  // - it will retrieve by default the Internet Explorer proxy settings, and
  // display some error messages or authentification dialog on screen
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinINet API should not be used from a service
  // - is implemented by creating a TWinINet internal class instance
  TRestHttpClientWinINet = class(TRestHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;

  /// HTTP/1.1 RESTful JSON Client class using WinHttp API
  // - has a common behavior as THttpClientSocket() but is sometimes faster over
  // a real network and is able to retrieve the current proxy settings
  // (if available) and handle secure HTTPS connection - so it seems to be used
  // in your client programs: TRestHttpClient will therefore map to this class
  // - WinHttp does not share directly any proxy settings with Internet Explorer.
  // The default WinHttp proxy configuration is set by either
  // proxycfg.exe on Windows XP and Windows Server 2003 or earlier, either
  // netsh.exe on Windows Vista and Windows Server 2008 or later; for instance,
  // you can run "proxycfg -u" or "netsh winhttp import proxy source=ie" to use
  // the current user's proxy settings for Internet Explorer (under 64 bit
  // Vista/Seven, to configure applications using the 32 bit WinHttp settings,
  // call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitly)
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinHttp API can be used from a service or a server
  // - is implemented by creating a TWinHttp internal class instance
  TRestHttpClientWinHttp = class(TRestHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;

{$endif USEWININET}


{ ************ TRestHttpClientCurl REST Client Class over LibCurl }

  {$ifdef USELIBCURL}
  /// HTTP/1.1 RESTful JSON Client class using libculr
  // - will handle HTTP and HTTPS, if OpenSSL or similar libray is available
  TRestHttpClientCurl = class(TRestHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;
  {$endif USELIBCURL}


  { ************ TRestHttpClient/TRestHttpClients Main Usable Classes }

type
  {$ifdef USEWININET}

  /// HTTP/1.1 RESTful JSON default mORMot Client class is WinHttp on Windows
  // - for support of Windows built-in proxy settings for instance
  TRestHttpClient = TRestHttpClientWinHttp;

  /// HTTP/HTTPS RESTful JSON default mORMot Client class is WinHttp on Windows
  TRestHttpsClient = TRestHttpClientWinHttp;

  {$else}

  /// HTTP/1.1 RESTful JSON default mORMot Client class uses sockets on POSIX
  TRestHttpClient = TRestHttpClientSocket;

  {$ifdef USELIBCURL}
  /// HTTP/HTTPS RESTful JSON default mORMot Client class is libcurl
  TRestHttpsClient = TRestHttpClientCurl;
  {$else}
  TRestHttpsClient = TRestHttpClientSocket; // fallback to non-TLS class
  {$endif USELIBCURL}

  {$endif USEWININET}

var
  /// a global hook variable, able to set WebSockets logging to full verbose
  // - checked by TRestHttpClientWebsockets.WebSocketsConnect()
  HttpClientFullWebSocketsLog: boolean;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlRestHttpClientWinSock = TRestHttpClientSocket;
  {$ifndef NOHTTPCLIENTWEBSOCKETS}
  TSqlRestHttpClientWebsockets = TRestHttpClientWebsockets;
  {$endif NOHTTPCLIENTWEBSOCKETS}
  {$ifdef USEWININET}
  TSqlRestHttpClientWinINet = TRestHttpClientWinINet;
  TSqlRestHttpClientWinHttp = TRestHttpClientWinHttp;
  {$endif USEWININET}
  {$ifdef USELIBCURL}
  TSqlRestHttpClientCurl = TRestHttpClientCurl;
  {$endif USELIBCURL}

{$endif PUREMORMOT2}


implementation


{ ************ TRestHttpClientGeneric and TRestHttpClientRequest Parent Classes }

{ TRestHttpClientGeneric }

procedure TRestHttpClientGeneric.InternalUri(var Call: TRestUriParams);
var
  Head, Content, ContentType: RawUtf8;
  P, PBeg: PUtf8Char;
  res: Int64Rec;
  log: ISynLog;
begin
  log := fLogClass.Enter('InternalUri %', [Call.Method], self);
  if IsOpen then
  begin
    Head := Call.InHead;
    Content := Call.InBody;
    ContentType := JSON_CONTENT_TYPE_VAR; // consider JSON by default
    P := pointer(Head);
    while P <> nil do
    begin
      PBeg := P;
      if IdemPChar(PBeg, 'CONTENT-TYPE:') then
      begin
        ContentType := GetNextLine(PBeg + 14, P); // retrieve customized type
        if P = nil then
          // last entry in header
          SetLength(Head, PBeg - pointer(Head))
        else
          system.delete(Head, PBeg - pointer(Head) + 1, P - PBeg);
        TrimSelf(Head);
        break;
      end;
      P := GotoNextLine(P);
    end;
    if Content <> '' then // always favor content type from binary
      ContentType := GetMimeContentTypeFromBuffer(
        pointer(Content), Length(Content), ContentType);
    if fUriPrefix <> '' then
      Call.Url := fUriPrefix + Call.Url;
    if fCustomHeader <> '' then
      if Call.InHead = '' then
        Call.InHead := fCustomHeader
      else
        Call.InHead := Call.InHead + #13#10 + fCustomHeader;
    fSafe.Enter;
    try
      res := InternalRequest(Call.Url, Call.Method, Head, Content, ContentType);
    finally
      fSafe.Leave;
    end;
    Call.OutStatus := res.Lo;
    Call.OutInternalState := res.Hi;
    Call.OutHead := Head;
    Call.OutBody := Content;
  end
  else
    Call.OutStatus := HTTP_NOTIMPLEMENTED; // 501 indicates not socket closed
  if log <> nil then
    with Call do
      log.Log(sllClient, '% % status=% len=% state=%',
        [method, url, OutStatus, length(OutBody), OutInternalState], self);
end;

procedure TRestHttpClientGeneric.SetCompression(Value: TRestHttpCompressions);
begin
  fCompression := Value;
  InternalClose; // force re-create connection at next request
end;

procedure TRestHttpClientGeneric.SetKeepAliveMS(Value: cardinal);
begin
  fKeepAliveMS := Value;
  InternalClose; // force re-create connection at next request
end;

constructor TRestHttpClientGeneric.Create(const aServer, aPort: RawUtf8;
  aModel: TOrmModel; aHttps: boolean; const aProxyName, aProxyByPass: RawUtf8;
  aSendTimeout, aReceiveTimeout, aConnectTimeout: cardinal);
begin
  inherited Create(aModel);
  if IdemPChar(pointer(aServer), 'UNIX:/') then
    fServer := aServer
  else
    Split(aServer, '/', fServer, fUriPrefix);
  fPort := aPort;
  fHttps := aHttps;
  fKeepAliveMS := 20000; // 20 seconds connection keep alive by default
  fCompression := []; // may add hcSynLZ or hcDeflate for AJAX clients
  if aConnectTimeout = 0 then
    fConnectTimeout := HTTP_DEFAULT_CONNECTTIMEOUT
  else
    fConnectTimeout := aConnectTimeout;
  if aSendTimeout = 0 then
    fSendTimeout := HTTP_DEFAULT_SENDTIMEOUT
  else
    fSendTimeout := aSendTimeout;
  if aReceiveTimeout = 0 then
    fReceiveTimeout := HTTP_DEFAULT_RECEIVETIMEOUT
  else
    fReceiveTimeout := aReceiveTimeout;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
end;

constructor TRestHttpClientGeneric.CreateForRemoteLogging(const aServer: RawUtf8;
  aLogClass: TSynLogClass; aPort: Integer; const aRoot: RawUtf8);
var
  aModel: TOrmModel;
begin
  if not Assigned(aLogClass) then
    raise ERestHttpClient.CreateUtf8(
      '%.CreateForRemoteLogging(LogClass=nil)', [self]);
  aModel := TOrmModel.Create([], aRoot);
  Create(aServer, UInt32ToUtf8(aPort), aModel, aPort = 443);
  aModel.Owner := self;
  ServerRemoteLogStart(aLogClass, true);
  fRemoteLogClass.Log(sllTrace,
    'Echoing to remote server http://%/%/RemoteLog:%', [aServer, aRoot, aPort]);
end;

procedure TRestHttpClientGeneric.DefinitionTo(
  Definition: TSynConnectionDefinition);
begin
  if Definition = nil then
    exit;
  inherited DefinitionTo(Definition); // save Kind + User/Password
  if fHttps then
    Definition.ServerName := 'https://';
  Definition.ServerName := FormatUtf8('%%:%',
    [Definition.ServerName, fServer, fPort]);
  Definition.DatabaseName := UrlEncode([
    'IgnoreSSLCertificateErrors', ord(fExtendedOptions.IgnoreSSLCertificateErrors),
    'ConnectTimeout', fConnectTimeout,
    'SendTimeout', fSendTimeout,
    'ReceiveTimeout', fReceiveTimeout,
    'ProxyName', fProxyName,
    'ProxyByPass', fProxyByPass], {TrimLeadingQuestionMark=}true);
end;

constructor TRestHttpClientGeneric.RegisteredClassCreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean);
var
  URI: TUri;
  P: PUtf8Char;
  V: cardinal;
  tmp: RawUtf8;
begin
  URI.From(aDefinition.ServerName);
  Create(URI.Server, URI.Port, aModel, URI.Https);
  P := Pointer(aDefinition.DataBaseName);
  while P <> nil do
  begin
    if UrlDecodeCardinal(P, 'CONNECTTIMEOUT=', V) then
      fConnectTimeout := V
    else if UrlDecodeCardinal(P, 'SENDTIMEOUT=', V) then
      fSendTimeout := V
    else if UrlDecodeCardinal(P, 'RECEIVETIMEOUT=', V) then
      fReceiveTimeout := V
    else if UrlDecodeValue(P, 'PROXYNAME=', tmp) then
      fProxyName := CurrentAnsiConvert.Utf8ToAnsi(tmp)
    else if UrlDecodeValue(P, 'PROXYBYPASS=', tmp) then
      fProxyByPass := CurrentAnsiConvert.Utf8ToAnsi(tmp);
    if UrlDecodeCardinal(P, 'IGNORESSLCERTIFICATEERRORS=', V, @P) then
      fExtendedOptions.IgnoreSSLCertificateErrors := boolean(V);
  end;
  inherited RegisteredClassCreateFrom(aModel, aDefinition, false); // call SetUser()
end;

constructor TRestHttpClientGeneric.Create(const aServer: TRestServerUriString;
  aModel: TOrmModel; aDefaultPort: integer; aHttps: boolean);
var
  uri: TRestServerUri;
begin
  uri.Uri := aServer;
  if uri.Root <> '' then
    aModel.Root := uri.Root;
  if (uri.Port = '') and
     (aDefaultPort <> 0) then
    uri.Port := Int32ToUtf8(aDefaultPort);
  Create(uri.Address, uri.Port, aModel, aHttps);
end;

function TRestHttpClientGeneric.HostName: RawUtf8;
begin
  if fServer <> '' then
    if fPort <> '' then
      result := fServer + ':' + fPort
    else
      result := fServer
  else
    result := '';
end;


{ ************ TRestHttpClientSocket REST Client Class over Sockets }

{ TRestHttpClientSocket }

function TRestHttpClientSocket.InternalIsOpen: boolean;
begin
  result := fSocket <> nil;
end;

procedure TRestHttpClientSocket.InternalOpen;
begin
  if fSocketClass = nil then
    fSocketClass := THttpClientSocket;
  fSocket := fSocketClass.Open(
    fServer, fPort, nlTcp, fConnectTimeout, fHttps);
  {$ifdef VERBOSECLIENTLOG}
  if LogClass <> nil then
    fSocket.OnLog := LogClass.DoLog; // verbose log
  {$endif VERBOSECLIENTLOG}
  // note that first registered algo will be the prefered one
  {$ifndef PUREMORMOT2}
  if hcSynShaAes in Compression then
    // global SHA-256 / AES-256-CFB encryption + SynLZ compression
    fSocket.RegisterCompress(CompressShaAes, {CompressMinSize=}0);
  {$endif PUREMORMOT2}
  if hcSynLz in Compression then
    // SynLZ is very fast and efficient, perfect for a Delphi Client
    fSocket.RegisterCompress(CompressSynLZ);
  if hcDeflate in Compression then
    // standard (slower) AJAX/HTTP gzip compression
    fSocket.RegisterCompress(CompressGZip);
end;

procedure TRestHttpClientSocket.InternalClose;
begin
  if fSocket <> nil then
    InternalLog('InternalClose: fSocket.Free', sllTrace);
  FreeAndNilSafe(fSocket);
end;

function TRestHttpClientSocket.InternalRequest(const url, method: RawUtf8;
  var Header, Data, DataType: RawUtf8): Int64Rec;
begin
  fLogFamily.SynLog.Log(sllTrace, 'InternalRequest % calling %(%).Request',
    [method, fSocket.ClassType, pointer(fSocket)], self);
  result.Lo := fSocket.Request(
    url, method, KeepAliveMS, Header, RawByteString(Data), DataType, false);
  result.Hi := fSocket.Http.ServerInternalState;
  Header := fSocket.Http.Headers;
  Data := fSocket.Http.Content;
  fSocket.Http.Content := ''; // ensure RefCnt=1 to avoid body alloc+copy
end;




{ TRestHttpClientRequest }

function TRestHttpClientRequest.InternalIsOpen: boolean;
begin
  result := fRequest <> nil;
end;

procedure TRestHttpClientRequest.InternalOpen;
begin
  InternalSetClass;
  if fRequestClass = nil then
    raise ERestHttpClient.CreateUtf8('fRequestClass=nil for %', [self]);
  fRequest := fRequestClass.Create(fServer, fPort, fHttps, fProxyName,
    fProxyByPass, fConnectTimeout, fSendTimeout, fReceiveTimeout);
  fRequest.ExtendedOptions := fExtendedOptions;
  // note that first registered algo will be the prefered one
  {$ifndef PUREMORMOT2}
  if hcSynShaAes in Compression then
    // global SHA-256 / AES-256-CFB encryption + SynLZ compression
    fRequest.RegisterCompress(CompressShaAes, 0); // CompressMinSize=0
  {$endif PUREMORMOT2}
  if hcSynLz in Compression then
    // SynLZ is very fast and efficient, perfect for a Delphi Client
    fRequest.RegisterCompress(CompressSynLZ);
  if hcDeflate in Compression then
    // standard (slower) AJAX/HTTP zip/deflate compression
    fRequest.RegisterCompress(CompressGZip);
end;

procedure TRestHttpClientRequest.InternalClose;
begin
  FreeAndNilSafe(fRequest);
end;

function TRestHttpClientRequest.InternalRequest(const url, method: RawUtf8;
  var Header, Data, DataType: RawUtf8): Int64Rec;
var
  OutHeader: RawUtf8;
  OutData: RawByteString;
begin
  if fRequest = nil then
    result.Lo := HTTP_NOTIMPLEMENTED
  else
  begin
    result.Lo := fRequest.Request(url, method, KeepAliveMS, Header,
      RawByteString(Data), DataType, OutHeader, OutData);
    result.Hi := GetCardinal(
      FindNameValue(pointer(OutHeader), 'SERVER-INTERNALSTATE:'));
    Header := OutHeader;
    Data := OutData;
  end;
end;



{ ************ TRestHttpClientWebsockets REST Client Class over WebSockets }

{$ifndef NOHTTPCLIENTWEBSOCKETS}

{ TRestHttpClientWebsockets }

procedure TRestHttpClientWebsockets.InternalOpen;
begin
  if fSocketClass = nil then
    fSocketClass := THttpClientWebSockets;
  inherited InternalOpen;
end;

function TRestHttpClientWebsockets.IsOpen: boolean;

  function NeedConnect: boolean;
  var
    err: RawUtf8;
  begin
    result := inherited IsOpen; // connect and call OnConnected
    if not result then
      exit;
    with fWebSocketParams do
      if AutoUpgrade then
      begin
        err := WebSocketsUpgrade(Key, Ajax, BinaryOptions);
        if err <> '' then
        begin
          if Assigned(fOnConnectionFailed) then
            fOnConnectionFailed(self, nil, nil);
          raise ERestHttpClient.CreateUtf8(
            '%.InternalOpen: WebSocketsUpgrade failed - %', [self, err]);
        end;
      end;
  end;

begin
  result := InternalIsOpen or NeedConnect;
end;

function TRestHttpClientWebsockets.FakeCallbackRegister(Sender: TServiceFactory;
  const Method: TInterfaceMethod; const ParamInfo: TInterfaceMethodArgument;
  ParamValue: Pointer): TRestClientCallbackID;
begin
  if WebSockets = nil then
    raise EServiceException.CreateUtf8('Missing %.WebSocketsUpgrade() call ' +
      'to enable interface parameter callbacks for %.%(%: %)',
      [self, Sender.InterfaceTypeInfo ^.Name, Method.Uri,
       ParamInfo.ParamName^, ParamInfo.ArgTypeName^]);
  if ParamValue = nil then
    result := 0
  else
    result := fFakeCallbacks.DoRegister(ParamValue,
      TInterfaceFactory.Get(ParamInfo.ArgRtti.Info));
end;

function TRestHttpClientWebsockets.FakeCallbackUnregister(
  Factory: TInterfaceFactory; FakeCallbackID: TRestClientCallbackID;
  Instance: pointer): boolean;
var
  body, head, resp: RawUtf8;
begin
  if (FakeCallbackID = 0) or
     not WebSocketsConnected then
  begin
    result := true; // nothing to notify
    exit;
  end;
  if WebSockets = nil then
    raise EServiceException.CreateUtf8('Missing %.WebSocketsUpgrade() call', [self]);
  FormatUtf8('{"%":%}', [Factory.InterfaceTypeInfo^.RawName, FakeCallbackID], body);
  CallbackNonBlockingSetHeader(head); // frames gathering + no wait
  result := CallBack(
    mPOST, 'CacheFlush/_callback_', body, resp, nil, 0, @head) = HTTP_SUCCESS;
end;

function TRestHttpClientWebsockets.CallbackRequest(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  params: TRestUriParams;
begin
  if (Ctxt = nil) or
     ((Ctxt.InContentType <> '') and
      not IdemPropNameU(Ctxt.InContentType, JSON_CONTENT_TYPE)) then
  begin
    result := HTTP_BADREQUEST;
    exit;
  end;
  params.Init(Ctxt.Url, Ctxt.Method, Ctxt.InHeaders, Ctxt.InContent);
  InternalNotificationMethodExecute(params);
  Ctxt.OutContent := params.OutBody;
  Ctxt.OutCustomHeaders := params.OutHead;
  Ctxt.OutContentType := params.OutBodyType;
  result := params.OutStatus;
end;

constructor TRestHttpClientWebsockets.Create(const aServer, aPort: RawUtf8;
  aModel: TOrmModel; aHttps: boolean; const aProxyName, aProxyByPass: RawUtf8;
  aSendTimeout, aReceiveTimeout, aConnectTimeout: cardinal);
begin
  inherited;
  fWebSocketProcessSettings.SetDefaults;
end;

function TRestHttpClientWebsockets.Settings: PWebSocketProcessSettings;
begin
  if self = nil then
    result := nil
  else
    result := @fWebSocketProcessSettings;
end;

function TRestHttpClientWebsockets.WebSocketsConnected: boolean;
begin
  result := (self <> nil) and
            (fSocket <> nil) and
            fSocket.InheritsFrom(THttpClientWebSockets) and
            (THttpClientWebSockets(fSocket).WebSockets.State = wpsRun);
end;

procedure TRestHttpClientWebsockets.CallbackNonBlockingSetHeader(
  out Header: RawUtf8);
begin
  Header := 'Sec-WebSocket-REST: NonBlocking'; // frames gathering + no wait
end;

function TRestHttpClientWebsockets.WebSockets: THttpClientWebSockets;
begin
  if fSocket = nil then
    if not IsOpen then
    begin
      result := nil;
      exit;
    end;
  result := fSocket as THttpClientWebSockets;
  if not Assigned(result.OnCallbackRequestProcess) then
    result.OnCallbackRequestProcess := CallbackRequest;
  if not Assigned(result.OnWebSocketsClosed) then
    result.OnWebSocketsClosed := OnWebSocketsClosed;
  result.Settings^ := fWebSocketProcessSettings;
end;

function TRestHttpClientWebsockets.WebSocketsUpgrade(
  const aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean;
  aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions;
  aRaiseExceptionOnFailure: ESynExceptionClass): RawUtf8;
var
  sockets: THttpClientWebSockets;
  prevconn: THttpServerConnectionID;
  log: ISynLog;
begin
  log := fLogFamily.SynLog.Enter(self, 'WebSocketsUpgrade');
  sockets := WebSockets;
  if sockets = nil then
    result := 'Impossible to connect to the Server'
  else
  begin
    if fWebSocketLoopDelay > 0 then
      sockets.Settings^.LoopDelay := fWebSocketLoopDelay;
    if sockets.WebSockets <> nil then
      prevconn := sockets.WebSockets.ConnectionID
    else
      prevconn := 0;
    result := sockets.WebSocketsUpgrade(
      Model.Root, aWebSocketsEncryptionKey,
      aWebSocketsAjax, aWebSocketsBinaryOptions, nil, fCustomHeader);
    if result = '' then
    begin
      // no error message = success
      with fWebSocketParams do
      begin
        // store parameters for auto-reconnection
        AutoUpgrade := sockets.Settings^.ClientAutoUpgrade;
        Key := aWebSocketsEncryptionKey;
        BinaryOptions := aWebSocketsBinaryOptions;
        Ajax := aWebSocketsAjax;
      end;
      if sockets.Settings^.ClientRestoreCallbacks and
         (prevconn <> 0) then
        // call TServiceContainerServer.FakeCallbackReplaceConnectionID
        if CallBack(mPOST, 'CacheFlush/_replaceconn_',
            Int64ToUtf8(prevconn), result) = HTTP_SUCCESS then
          result := ''; // on error, log result = server response
      if Assigned(fOnWebSocketsUpgraded) then
        fOnWebSocketsUpgraded(self);
      inc(fUpgradeCount);
    end;
  end;
  if log <> nil then
    if result <> '' then
      log.Log(sllWarning, '[%] error upgrading %', [result, sockets], self)
    else
      log.Log(sllHTTP, 'HTTP link upgraded to WebSockets using %',
        [sockets], self);
  if (aRaiseExceptionOnFailure <> nil) and
     (result <> '') then
    raise aRaiseExceptionOnFailure.CreateUtf8('%.WebSocketsUpgrade failed: [%]',
      [self, result]);
end;

function TRestHttpClientWebsockets.WebSocketsConnect(
  const aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean;
  aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions): RawUtf8;
begin
  if WebSockets = nil then
    result := 'WebSockets=nil'
  else
  begin
    if HttpClientFullWebSocketsLog then
      WebSockets.Settings.SetFullLog;
    result := WebSocketsUpgrade(
      aWebSocketsEncryptionKey, aWebSocketsAjax, aWebSocketsBinaryOptions);
    if result = '' then
      if not ServerTimestampSynchronize then
        result := 'ServerTimestampSynchronize';
  end;
  if result <> '' then
    raise ERestHttpClient.CreateUtf8('%.WebSocketsConnect failed on %:%/% -> %',
      [self, Server, Port, Model.Root, result]);
end;

{$endif NOHTTPCLIENTWEBSOCKETS}



{ ************ TRestHttpClientWinINet TRestHttpClientWinHttp Windows REST Client Classes }

{$ifdef USEWININET}

{ TRestHttpClientWinINet }

procedure TRestHttpClientWinINet.InternalSetClass;
begin
  fRequestClass := TWinINet;
end;


{ TRestHttpClientWinHttp }

procedure TRestHttpClientWinHttp.InternalSetClass;
begin
  fRequestClass := TWinHttp;
end;

{$endif USEWININET}



{ ************ TRestHttpClientCurl REST Client Class over LibCurl }

{$ifdef USELIBCURL}

{ TRestHttpClientCurl}

procedure TRestHttpClientCurl.InternalSetClass;
begin
  fRequestClass := TCurlHttp;
end;

{$endif USELIBCURL}

initialization
  TRestHttpClientSocket.RegisterClassNameForDefinition;
  {$ifndef NOHTTPCLIENTWEBSOCKETS}
  TRestHttpClientWebsockets.RegisterClassNameForDefinition;
  {$endif NOHTTPCLIENTWEBSOCKETS}
  {$ifdef USEWININET}
  TRestHttpClientWinINet.RegisterClassNameForDefinition;
  TRestHttpClientWinHttp.RegisterClassNameForDefinition;
  {$endif USEWININET}
  {$ifdef USELIBCURL}
  TRestHttpClientCurl.RegisterClassNameForDefinition;
  {$endif USELIBCURL}

end.

