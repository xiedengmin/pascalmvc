//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRHttp;

interface

uses
{$IFDEF MSWINDOWS}
  ActiveX,
  {$IFNDEF BCB}{$IFNDEF FPC}
    {$NOINCLUDE ActiveX}
  {$ENDIF}{$ENDIF}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs, Types, Math,
  CLRClasses, CRTypes, CRFunctions, CRBase64, CRVio, CRSecureConnection;

type
  //!!! Sync with SRequestMethod
  TScRequestMethod = (rmGET, rmHEAD, rmOPTIONS, rmPOST, rmPUT, rmDELETE, rmTRACE, rmCONNECT, rmPATCH);

  //!!! Sync with HTTP_STATUS_CODES
  TScHttpStatusCode = (
    scAccepted, scAmbiguous, scBadGateway, scBadRequest, scConflict, scContinue,
    scCreated, scExpectationFailed, scForbidden, scFound, scGatewayTimeout,
    scGone, scHttpVersionNotSupported, scInternalServerError, scLengthRequired,
    scMethodNotAllowed, scMoved, scMovedPermanently, scMultipleChoices, scNoContent,
    scNonAuthoritativeInformation, scNotAcceptable, scNotFound, scNotImplemented,
    scNotModified, scOK, scPartialContent, scPaymentRequired, scPreconditionFailed,
    scProxyAuthenticationRequired, scRedirect, scRedirectKeepVerb, scRedirectMethod,
    scRequestedRangeNotSatisfiable, scRequestEntityTooLarge, scRequestTimeout,
    scRequestUriTooLong, scResetContent, scSeeOther, scServiceUnavailable,
    scSwitchingProtocols, scTemporaryRedirect, scUnauthorized,
    scUnsupportedMediaType, scUnused, scUpgradeRequired, scUseProxy, scUnknown
  );

  TScHttpStatusCodes = set of TScHttpStatusCode;

  TScBeforeSendDataEvent = procedure(Sender: TObject; Offset, Count: Int64; var Cancel: boolean) of object;

  TScOnGetNextChunkDataEvent = procedure(Sender: TObject; out Buffer: TValueArr; out Count: Integer) of object;

  TCRHttpWebRequest = class;
  TCRHttpWebResponse = class;

  TScRequestCacheLevel = (clDefault, clNoCacheNoStore, clReload);

  TScRequestCachePolicy = class(TPersistent)
  private
    FLevel: TScRequestCacheLevel;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(RequestCacheLevel: TScRequestCacheLevel = clDefault);

  published
    property Level: TScRequestCacheLevel read FLevel write FLevel default clDefault;
  end;

  TScOnCheckNewStrValueEvent = procedure (const Key, Value: string) of object;

  TScWebHeaderCollection = class(TStrValueStringList)
  private
    SB: StringBuilder;
    FOnCheckNewHeader: TScOnCheckNewStrValueEvent;

    function SaveToString(const Separator: string): string;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF VER12P}override;{$ELSE}{$IFDEF FPC}reintroduce;{$ENDIF}virtual;{$ENDIF}

    property Text: string read GetText write SetText;
    property OnCheckNewHeader: TScOnCheckNewStrValueEvent read FOnCheckNewHeader write FOnCheckNewHeader;
  end;

  TScWebRequestHeaderCollection = class(TScWebHeaderCollection)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCRHttpWebRequest;
  protected
    procedure CheckNewValue(const Key, Value: string); override;
  public
    constructor Create(Owner: TCRHttpWebRequest);

    function ToString: string; override;
  end;

  TScWebResponseHeaderCollection = class(TScWebHeaderCollection)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCRHttpWebResponse;
  public
    constructor Create(Owner: TCRHttpWebResponse);

    function ToString: string; override;
  end;

  TCRHttpWebRequest = class(TInterfacedPersistent, ISequentialStream)
  private
    FAccept: string;
    FCachePolicy: TScRequestCachePolicy;
    FConnection: string;
    FConnectionGroupName: string;
    FContentLength: Int64;
    FContentType: string;
    FCookies: TStringList;
    FCredentials: TScNetworkCredential;
    FDate: TDateTime;
    FExpect: string;
    FFrom: string;
    FHeaders: TScWebHeaderCollection;
    FHost: string;
    FIfModifiedSince: TDateTime;
    FKeepAlive: boolean;
    FMethod: TScRequestMethod;
    FOldMethod: TScRequestMethod;
    FProtocolVersion: TScVersion;
    FProxy: TScWebProxy;
    FRange: string;
    FReadWriteTimeout: integer;
    FReferer: string;
    FRequestUri: string;
    FTransferEncoding: string;
    FUpgrade: string;
    FUserAgent: string;
    FIPVersion: TIPVersion;
    FMaximumAutomaticRedirections: integer;
    FMaximumAutomaticReconnections: integer;

    FContentWrote: Int64;
    FSendChunked: boolean;
    FSendBlockSize: integer;

    FStatusCode: integer;
    FStatusDescription: string;
    FAllowedStatuses: TScHttpStatusCodes;

    FRequestStream: TStream;
    FSendBuffer: TBytes;
    FOnGetNextChunkData: TScOnGetNextChunkDataEvent;
    FOnConnected: TNotifyEvent;
    FOnAuthenticationNeeded: TNotifyEvent;
    FBeforeSendRequest: TNotifyEvent;
    FAfterSendRequest: TNotifyEvent;
    FAfterRetrieveHeaders: TNotifyEvent;
    FBeforeSendData: TScBeforeSendDataEvent;

    FSSLOptions: TSSLOptions;
    FIOHandler: TCRIOHandler;

    procedure ReadHeadersText(Reader: TReader);
    procedure WriteHeadersText(Writer: TWriter);

    function GetIsSecure: boolean;
    function GetStatusCode: TScHttpStatusCode;

    procedure SetAccept(const Value: string);
    procedure SetCachePolicy(Value: TScRequestCachePolicy);
    procedure SetConnection(const Value: string);
    procedure SetConnectionGroupName(const Value: string);
    procedure SetContentLength(const Value: Int64);
    procedure SetContentType(const Value: string);
    procedure SetCookies(Value: TStringList);
    procedure SetCredentials(Value: TScNetworkCredential);
    procedure SetDate(const Value: TDateTime);
    procedure SetExpect(const Value: string);
    procedure SetFrom(const Value: string);
    procedure SetHeaders(Value: TScWebHeaderCollection);
    procedure SetHost(const Value: string);
    procedure SetIfModifiedSince(const Value: TDateTime);
    procedure CheckKeepAliveHeader;
    procedure SetKeepAlive(const Value: boolean);
    procedure SetMethod(const Value: TScRequestMethod);
    procedure SetProtocolVersion(Value: TScVersion);
    procedure SetProxy(Value: TScWebProxy);
    procedure SetRange(const Value: string);
    procedure SetReadWriteTimeout(const Value: integer);
    procedure SetReferer(const Value: string);
    procedure SetRequestUri(const Value: string);
    procedure SetTransferEncoding(const Value: string);
    procedure SetUpgrade(const Value: string);
    procedure SetUserAgent(const Value: string);
    procedure SetIPVersion(const Value: TIPVersion);
    procedure SetMaximumAutomaticRedirections(const Value: integer);
    procedure SetMaximumAutomaticReconnections(const Value: integer);
    procedure SetSendChunked(const Value: boolean);
    procedure SetSendBlockSize(const Value: integer);
    procedure SetSSLOptions(Value: TSSLOptions);
    procedure SetRequestStream(Value: TStream);

    procedure DoBeforeSendRequest;
    procedure DoAfterSendRequest;
    procedure DoAfterRetrieveHeaders;

  protected
    FSecureConnection: TScSecureConnection;

    FAddress: string;
    // URL specifics
    FScheme: string;
    FPort: string;
    FQuery: string;
    FResource: string;
    FParameters: string;
    FPath: string;
    FFragment: string;
    FNetworkLocation: string;
    FPortNo: integer;

    procedure Init;
    procedure GetNextChunkData(out Buffer: TValueArr; out Count: Integer);
    procedure WriteChunk(Buffer: TValueArr; Count: integer; CancellationToken: TScCancellationToken);
    procedure WriteChunkSize(Size: integer);
    procedure InternalWriteData(CancellationToken: TScCancellationToken);

    procedure CheckRequest;
    procedure SendRequest(CancellationToken: TScCancellationToken = nil);
    procedure DefineProperties(Filer: TFiler); override;

    class function DefaultPort(const Scheme: string): string;
    procedure CheckInactive;

  public
  {$IFNDEF FPC}
    {$IFDEF VER22P}
    function Read(pv: IntPtr; cb: FixedUInt; pcbRead: PFixedUInt): HResult; stdcall;
    {$ELSE}
    function Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult; stdcall;
    {$ENDIF}
  {$ELSE}
    function Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT; stdcall;
  {$ENDIF}
  {$IFNDEF FPC}
    {$IFDEF VER22P}
    function Write(pv: IntPtr; cb: FixedUInt; pcbWritten: PFixedUInt): HResult; overload; stdcall;
    {$ELSE}
    function Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult; overload; stdcall;
    {$ENDIF}
  {$ELSE}
    function Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT; overload; stdcall;
  {$ENDIF}

    procedure WriteBuffer(const Buffer: TValueArr; Offset, Count: integer); overload;
    procedure WriteBuffer(const Data: TBytes); overload;
    procedure WriteData(Stream: TStream);
    function ReadBuffer: TBytes;

  public
    constructor Create(const URL: string); 
    destructor Destroy; override;


    procedure Abort;
    procedure Disconnect;
    function GetResponse(CancellationToken: TScCancellationToken = nil): TCRHttpWebResponse;

    procedure SetAllowedStatuses(const Value: TScHttpStatusCodes);

    property Address: string read FAddress;
    property IsSecure: boolean read GetIsSecure;
    property StatusCode: TScHttpStatusCode read GetStatusCode;
    property StatusDescription: string read FStatusDescription;

    property RequestStream: TStream read FRequestStream write SetRequestStream;

    property OnGetNextChunkData: TScOnGetNextChunkDataEvent read FOnGetNextChunkData write FOnGetNextChunkData;

    property IOHandler: TCRIOHandler read FIOHandler write FIOHandler;

  published
    property Accept: string read FAccept write SetAccept;
    property CachePolicy: TScRequestCachePolicy read FCachePolicy write SetCachePolicy;
    property Connection: string read FConnection write SetConnection;
    property ConnectionGroupName: string read FConnectionGroupName write SetConnectionGroupName;
    property ContentLength: Int64 read FContentLength write SetContentLength default -1;
    property ContentType: string read FContentType write SetContentType;
    property Cookies: TStringList read FCookies write SetCookies;
    property Credentials: TScNetworkCredential read FCredentials write SetCredentials;
    property Date: TDateTime read FDate write SetDate;
    property Expect: string read FExpect write SetExpect;
    property From: string read FFrom write SetFrom;
    property Headers: TScWebHeaderCollection read FHeaders write SetHeaders;
    property Host: string read FHost write SetHost;
    property IfModifiedSince: TDateTime read FIfModifiedSince write SetIfModifiedSince;
    property KeepAlive: boolean read FKeepAlive write SetKeepAlive default True;
    property Method: TScRequestMethod read FMethod write SetMethod default rmGET;
    property ProtocolVersion: TScVersion read FProtocolVersion write SetProtocolVersion;
    property Proxy: TScWebProxy read FProxy write SetProxy;
    property Range: string read FRange write SetRange;
    property ReadWriteTimeout: integer read FReadWriteTimeout write SetReadWriteTimeout default 15;
    property Referer: string read FReferer write SetReferer;
    property RequestUri: string read FRequestUri write SetRequestUri;
    property TransferEncoding: string read FTransferEncoding write SetTransferEncoding;
    property Upgrade: string read FUpgrade write SetUpgrade;
    property UserAgent: string read FUserAgent write SetUserAgent;
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion default DefValIPVersion;
    property MaximumAutomaticRedirections: integer read FMaximumAutomaticRedirections write SetMaximumAutomaticRedirections default 50;
    property MaximumAutomaticReconnections: integer read FMaximumAutomaticReconnections write SetMaximumAutomaticReconnections default 10;

    property SendChunked: boolean read FSendChunked write SetSendChunked default False;
    property SendBlockSize: integer read FSendBlockSize write SetSendBlockSize default SEND_BLOCK_SIZE;

    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnAuthenticationNeeded: TNotifyEvent read FOnAuthenticationNeeded write FOnAuthenticationNeeded;
    property BeforeSendRequest: TNotifyEvent read FBeforeSendRequest write FBeforeSendRequest;
    property AfterSendRequest: TNotifyEvent read FAfterSendRequest write FAfterSendRequest;
    property AfterRetrieveHeaders: TNotifyEvent read FAfterRetrieveHeaders write FAfterRetrieveHeaders;
    property BeforeSendData: TScBeforeSendDataEvent read FBeforeSendData write FBeforeSendData;

    property SSLOptions: TSSLOptions read FSSLOptions write SetSSLOptions;
  end;

  TCRHttpWebResponse = class(TInterfacedPersistent, ISequentialStream)
  private
    FCookies: TStringList;
    FHeaders: TScWebHeaderCollection;
    FMethod: TScRequestMethod;
    FProtocolVersion: TScVersion;
    FLocation: string;
    FStatusCode: integer;
    FStatusDescription: string;
    FResponseUri: string;
    FRetryAfter: integer;

    FContentLength: Int64;
    FContentRead: Int64;
    FChunked: boolean;
    FTmpBuf: TBytes;

    FOnProgress: TScOnProgressEvent;

    function GetContentEncoding: string;
    function GetContentType: string;
    function GetLastModified: TDateTime;
    function GetServer: string;
    function GetStatusCode: TScHttpStatusCode;
    function GetIsSecure: boolean;

  protected
    FSecureConnection: TScSecureConnection;

    procedure DoProgress(const Total, Current: Int64);
    function ReadChunk(const Buffer: TValueArr; Offset, Count: integer): integer;
    function InternalRead(const Buffer: TValueArr; Offset, Count: integer): integer;
    procedure SkipAll;

    procedure RetrieveHeaders(CancellationToken: TScCancellationToken = nil);

  public
  {$IFNDEF FPC}
    {$IFDEF VER22P}
    function Read(pv: IntPtr; cb: FixedUInt; pcbRead: PFixedUInt): HResult; overload; stdcall;
    {$ELSE}
    function Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult; overload; stdcall;
    {$ENDIF}
  {$ELSE}
    function Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT; overload; stdcall;
  {$ENDIF}
  {$IFNDEF FPC}
    {$IFDEF VER22P}
    function Write(pv: IntPtr; cb: FixedUInt; pcbWritten: PFixedUInt): HResult; stdcall;
    {$ELSE}
    function Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult; stdcall;
    {$ENDIF}
  {$ELSE}
    function Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT; stdcall;
  {$ENDIF}

    function ReadBuffer(const Buffer: TValueArr; Offset, Count: integer): integer;
    function ReadAsString: string;
    function ReadAsBytes: TBytes;
    function ReadToStream(Stream: TStream): integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Abort;
    function GetResponseHeader(const HeaderName: string): string;
    function WaitForData(MillisecondsTimeout: integer): boolean;

    property IsSecure: boolean read GetIsSecure;
    property ContentEncoding: string read GetContentEncoding;
    property ContentLength: Int64 read FContentLength;
    property ContentType: string read GetContentType;
    property Cookies: TStringList read FCookies;
    property Headers: TScWebHeaderCollection read FHeaders;
    property LastModified: TDateTime read GetLastModified;
    property Method: TScRequestMethod read FMethod;
    property ProtocolVersion: TScVersion read FProtocolVersion;
    property ResponseUri: string read FResponseUri;
    property Server: string read GetServer;
    property StatusCode: TScHttpStatusCode read GetStatusCode;
    property StatusDescription: string read FStatusDescription;

    property OnProgress: TScOnProgressEvent read FOnProgress write FOnProgress;
  end;

  HttpException = class(Exception)
  private
    FStatusCode: integer;
    FServerMessage: string;

    function GetStatusCode: TScHttpStatusCode;
  public
    constructor Create(const Msg: string); overload;
    constructor Create(StatusCode: integer; const Msg: string); overload;
    constructor Create(StatusCode: TScHttpStatusCode; const Msg: string); overload;
    constructor Create(StatusCode: integer; const Msg, ServerMessage: string); overload;

    property Code: integer read FStatusCode;
    property StatusCode: TScHttpStatusCode read GetStatusCode;
    property ServerMessage: string read FServerMessage;
  end;

  TScHttpWebResponseHelper = class
    class function GetSecureConnection(Obj: TCRHttpWebResponse): TScSecureConnection;
    class procedure SetSecureConnection(Obj: TCRHttpWebResponse; Value: TScSecureConnection);
  end;

const
  Content_Type_WWWForm = 'application/x-www-form-urlencoded';
  Content_Type_Octet = 'application/octet-stream';
  Accept_MediaRange1 = 'www/source, text/html, video/mpeg, image/jpeg, image/x-tiff';
  Accept_MediaRange2 = 'image/x-rgb, image/x-xbm, image/gif, */*, application/postscript';
  HttpScheme = 'http:';

implementation

resourcestring
  SRequestExecuted = 'The operation is not allowed when the request is executed';
  SRequestNotExecuted = 'The operation is not allowed when the request is not executed';
  SInvalidMessage = 'Protocol error: message is invalid';
  SInvalidMessageLength = 'Protocol error: message length is invalid';
  SUnknownProtocol = 'Unknown protocol';
  SSetProtectedHeader = 'The ''%s'' header must be modified using the appropriate property or method';
  SCannotSendContent = 'Cannot send a content-body with the %s verb-type';
  SContentLengthNotSet = 'ContentLength must be set for POST or PUT requests';
  SSendChunkedAndTransferEncoding = 'SendChunked must be set when TransferEncoding is set';
  SRequestStreamIsNotAssigned = 'RequestStream must be assigned when SendChunked is set';
  SRequestStreamIsAssigned = 'RequestStream should not be assigned when SendChunked is set';
  SBufferedDataIsWrote = 'Buffered data should not be wrote when SendChunked is set';
  SCanNotWriteDataWhenSendChunked = 'You can not write buffered data when SendChunked is set';
  SInvalidContentBuffer = 'You must write ContentLength bytes to the request buffer before calling GetResponse';
  SHeaderFieldNotFound = '"%s" header field is not found';
  SCollectionReadonly = 'Collection is readonly';
  SDataSendingCanceled = 'Data sending canceled';
  SDataReadingCanceled = 'Data reading canceled';

const
  General_Header_Cache = 'Cache-Control';
  General_Header_Connection = 'Connection';
  General_Header_Date = 'Date';
  General_Header_Pragma = 'Pragma';
  General_Header_Trailer = 'Trailer';
  General_Header_TransferEncoding = 'Transfer-Encoding';
  General_Header_Upgrade = 'Upgrade';
  General_Header_Via = 'Via';
  General_Header_Warning = 'Warning';

  Request_Header_Accept = 'Accept';
  Request_Header_AcceptCharset = 'Accept-Charset';
  Request_Header_AcceptEncoding = 'Accept-Encoding';
  Request_Header_AcceptLanguage = 'Accept-Language';
  Request_Header_Authorization = 'Authorization';
  Request_Header_Expect = 'Expect';
  Request_Header_From = 'From';
  Request_Header_Host = 'Host';
  Request_Header_Port = 'Port';
  Request_Header_IfMatch = 'If-Match';
  Request_Header_IfModifiedSince = 'If-Modified-Since';
  Request_Header_IfNoneMatch = 'If-None-Match';
  Request_Header_IfRange = 'If-Range';
  Request_Header_IfUnmodifiedSince = 'If-Unmodified-Since';
  Request_Header_MaxForwards = 'Max-Forwards';
  Request_Header_ProxyAuthorization = 'Proxy-Authorization';
  Request_Header_Range = 'Range';
  Request_Header_Referer = 'Referer';
  Request_Header_TE = 'TE';
  Request_Header_UserAgent = 'User-Agent';
  Request_Header_Cookie = 'Cookie';
  Request_Header_ProxyConnection = 'Proxy-Connection';

  Response_Header_AcceptRanges = 'Accept-Ranges';
  Response_Header_Age = 'Age';
  Response_Header_ETag = 'ETag';
  Response_Header_Location = 'Location';
  Response_Header_ProxyAuthenticate = 'Proxy-Authenticate';
  Response_Header_RetryAfter = 'Retry-After';
  Response_Header_Server = 'Server';
  Response_Header_Vary = 'Vary';
  Response_Header_WWWAuthenticate = 'WWW-Authenticate';
  Response_Header_SetCookie = 'Set-Cookie';

  Entity_Header_Allow = 'Allow';
  Entity_Header_ContentBase = 'Content-Base';
  Entity_Header_ContentEncoding = 'Content-Encoding';
  Entity_Header_ContentLanguage = 'Content-Language';
  Entity_Header_ContentLength = 'Content-Length';
  Entity_Header_ContentLocation = 'Content-Location';
  Entity_Header_ContentMD5 = 'Content-MD5';
  Entity_Header_ContentRange = 'Content-Range';
  Entity_Header_ContentType = 'Content-Type';
  Entity_Header_ETag = 'ETag';
  Entity_Header_Expires = 'Expires';
  Entity_Header_LastModified = 'Last-Modified';

  SNoCache = 'no-cache';
  SNoStore = 'no-store';
  SKeepAlive = 'keep-alive';
  SClose = 'close';
  SChunked = 'chunked';

  HeaderSeparator = ': ';
  CRLF = #13#10;

  SRequestMethod: array [TScRequestMethod] of string =
    ('GET', 'HEAD', 'OPTIONS', 'POST', 'PUT', 'DELETE', 'TRACE', 'CONNECT', 'PATCH');

  HTTP_STATUS_CODES: array[TScHttpStatusCode] of integer = (
    202{scAccepted}, 300{scAmbiguous}, 502{scBadGateway}, 400{scBadRequest},
    409{scConflict}, 100{scContinue}, 201{scCreated}, 417{scExpectationFailed},
    403{scForbidden}, 302{scFound}, 504{scGatewayTimeout}, 410{scGone},
    505{scHttpVersionNotSupported}, 500{scInternalServerError},
    411{scLengthRequired}, 405{scMethodNotAllowed}, 301{scMoved}, 301{scMovedPermanently},
    300{scMultipleChoices}, 204{scNoContent}, 203{scNonAuthoritativeInformation},
    406{scNotAcceptable}, 404{scNotFound}, 501{scNotImplemented}, 304{scNotModified},
    200{scOK}, 206{scPartialContent}, 402{scPaymentRequired},
    412{scPreconditionFailed}, 407{scProxyAuthenticationRequired}, 302{scRedirect},
    307{scRedirectKeepVerb}, 303{scRedirectMethod}, 416{scRequestedRangeNotSatisfiable},
    413{scRequestEntityTooLarge}, 408{scRequestTimeout}, 414{scRequestUriTooLong},
    205{scResetContent}, 303{scSeeOther}, 503{scServiceUnavailable},
    101{scSwitchingProtocols}, 307{scTemporaryRedirect}, 401{scUnauthorized},
    415{scUnsupportedMediaType}, 306{scUnused}, 426{scUpgradeRequired}, 305{scUseProxy},
    0{scUnknown}
  );

  PortDefault = '80';
  PORTS: array[1..18, 1..2] of string = (
    ('http:',    '80'),
    ('https:',  '443'),
    ('ws:',      '80'),
    ('wss:',    '443'),
    ('ftp:',     '21'),
    ('telnet:',  '23'),
    ('smtp:',    '25'),
    ('whois:',   '43'),
    ('whois++:', '63'),
    ('gopher:',  '70'),
    ('pop3:',   '110'),
    ('nntp:',   '119'),
    ('news:',   '119'),
    ('imap2:',  '143'),
    ('irc:',    '194'),
    ('wais:',   '210'),
    ('imap3:',  '220'),
    ('ldap:',   '389')
  );

function ConvertToStatusCode(Value: integer): TScHttpStatusCode;
begin
  for Result := Low(TScHttpStatusCode) to High(TScHttpStatusCode) do
    if Value = HTTP_STATUS_CODES[Result] then
      Exit;

  Result := scUnknown;
end;

{ TScRequestCachePolicy }

constructor TScRequestCachePolicy.Create(RequestCacheLevel: TScRequestCacheLevel = clDefault);
begin
  inherited Create;

  FLevel := RequestCacheLevel;
end;

procedure TScRequestCachePolicy.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScRequestCachePolicy) then begin
    TScRequestCachePolicy(Dest).FLevel := FLevel;
  end
  else
    inherited;
end;

{ TScWebHeaderCollection }

constructor TScWebHeaderCollection.Create;
begin
  inherited Create;
  SB := StringBuilder.Create(1024);
end;

destructor TScWebHeaderCollection.Destroy;
begin
  SB.Free;
  inherited;
end;

function TScWebHeaderCollection.SaveToString(const Separator: string): string;
var
  i: integer;
begin
  SB.Length := 0;

  for i := 0 to Count - 1 do begin
    SB.Append(Keys[i]);
    SB.Append(Separator);
    SB.Append(Values[i]);
    SB.Append(CRLF);
  end;

  Result := SB.ToString;
end;

function TScWebHeaderCollection.GetText: string;
begin
  Result := SaveToString('=');
end;

procedure TScWebHeaderCollection.SetText(const Value: string);
var
  List: TStringList;
  i: integer;
begin
  Clear;
  List := TStringList.Create;
  try
    List.Text := Value;

    for i := 0 to List.Count - 1 do
      if List.Names[i] <> '' then
        Add(List.Names[i], List.Values[List.Names[i]])
      else
        Add(List[i], '');
  finally
    List.Free;
  end;
end;

function TScWebHeaderCollection.ToString: string;
begin
  Result := SaveToString(HeaderSeparator);
end;

{ TScWebRequestHeaderCollection }

constructor TScWebRequestHeaderCollection.Create(Owner: TCRHttpWebRequest);
begin
  inherited Create;

  FOwner := Owner;
end;

procedure TScWebRequestHeaderCollection.CheckNewValue(const Key, Value: string);
const
  ProtectedHeaders: array [0..18] of string = (
    Request_Header_Accept,
    General_Header_Cache,
    General_Header_Connection,
    Entity_Header_ContentLength,
    Entity_Header_ContentType,
    Request_Header_Cookie,
    General_Header_Date,
    Request_Header_Expect,
    Request_Header_From,
    Request_Header_Host,
    Request_Header_IfModifiedSince,
    Request_Header_Port,
    General_Header_Pragma,
    Request_Header_ProxyConnection,
    Request_Header_Range,
    Request_Header_Referer,
    General_Header_TransferEncoding,
    General_Header_Upgrade,
    Request_Header_UserAgent
  );
var
  i: integer;
begin
  if Assigned(FOnCheckNewHeader) then
    FOnCheckNewHeader(Key, Value)
  else
    for i := Low(ProtectedHeaders) to High(ProtectedHeaders) do
      if AnsiSameText(Key, ProtectedHeaders[i]) then
        raise Exception.CreateFmt(SSetProtectedHeader, [Key]);
end;

function TScWebRequestHeaderCollection.ToString: string;
var
  AuthBuf: TBytes;
  s: string;
  i: integer;
begin
  SetLength(AuthBuf, 0);
  SB.Length := 0;

  if FOwner.Host <> '' then begin
    SB.Append(Request_Header_Host);
    SB.Append(HeaderSeparator);
    SB.Append(FOwner.Host);
    SB.Append(CRLF);
  end
  else begin
    SB.Append(Request_Header_Host);
    SB.Append(HeaderSeparator);
    SB.Append(FOwner.FNetworkLocation + FOwner.FPort);
    SB.Append(CRLF);
  end;

  if FOwner.Proxy.Address <> '' then begin
    if FOwner.KeepAlive then begin
      SB.Append(Request_Header_ProxyConnection);
      SB.Append(HeaderSeparator);
      SB.Append(SKeepAlive);
      SB.Append(CRLF);
    end;

    SB.Append(Request_Header_Port);
    SB.Append(HeaderSeparator);
    SB.Append(IntToStr(FOwner.FPortNo));
    SB.Append(CRLF);

    if (FOwner.Proxy.Credentials.UserName <> '') and (FOwner.Proxy.Credentials.Password <> '') then begin
      s := FOwner.Proxy.Credentials.UserName + ':' + FOwner.Proxy.Credentials.Password;
      AuthBuf := TBase64.Encode(Encoding.Default.GetBytes(s));

      SB.Append(Request_Header_ProxyAuthorization);
      SB.Append(HeaderSeparator);
      SB.Append('Basic ');
      SB.Append(Encoding.Default.GetString(AuthBuf));
      SB.Append(CRLF);
    end;
  end;

  if (FOwner.FMethod <> rmCONNECT) or (FOwner.FOldMethod = rmCONNECT) then begin
    case FOwner.CachePolicy.Level of
      clDefault:
        ;
      clNoCacheNoStore: begin
        SB.Append(General_Header_Cache + HeaderSeparator + SNoStore + ',' + SNoCache + CRLF);
        SB.Append(General_Header_Pragma + HeaderSeparator + SNoCache + CRLF);
      end;
      clReload: begin
        SB.Append(General_Header_Cache + HeaderSeparator + SNoCache + CRLF);
        SB.Append(General_Header_Pragma + HeaderSeparator + SNoCache + CRLF);
      end
    else
      Assert(False);
    end;

    if (FOwner.Credentials.UserName <> '') and (FOwner.Credentials.Password <> '') then begin
      s := FOwner.Credentials.UserName + ':' + FOwner.Credentials.Password;
      AuthBuf := TBase64.Encode(Encoding.Default.GetBytes(s));

      SB.Append(Request_Header_Authorization);
      SB.Append(HeaderSeparator);
      SB.Append('Basic ');
      SB.Append(Encoding.Default.GetString(AuthBuf));
      SB.Append(CRLF);
    end;

    if FOwner.Upgrade <> '' then begin
      SB.Append(General_Header_Upgrade);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.Upgrade);
      SB.Append(CRLF);
    end;


    FOwner.CheckKeepAliveHeader;
    if FOwner.Connection <> '' then begin
      SB.Append(General_Header_Connection);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.Connection);
      SB.Append(CRLF);
    end;

    if FOwner.Date <> 0 then begin
      SB.Append(General_Header_Date);
      SB.Append(HeaderSeparator);
      SB.Append(DateTimeToHttpStr(FOwner.Date));
      SB.Append(CRLF);
    end;

    if FOwner.TransferEncoding <> '' then begin
      SB.Append(General_Header_TransferEncoding);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.TransferEncoding);
      SB.Append(CRLF);
    end;

    if FOwner.Accept <> '' then begin
      SB.Append(Request_Header_Accept);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.Accept);
      SB.Append(CRLF);
    end;

    if FOwner.Expect <> '' then begin
      SB.Append(Request_Header_Expect);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.Expect);
      SB.Append(CRLF);
    end;

    if FOwner.From <> '' then begin
      SB.Append(Request_Header_From);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.From);
      SB.Append(CRLF);
    end;

    if FOwner.IfModifiedSince <> 0 then begin
      SB.Append(Request_Header_IfModifiedSince);
      SB.Append(HeaderSeparator);
      SB.Append(DateTimeToHttpStr(FOwner.IfModifiedSince));
      SB.Append(CRLF);
    end;

    if FOwner.Range <> '' then begin
      SB.Append(Request_Header_Range);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.Range);
      SB.Append(CRLF);
    end;

    if FOwner.Referer <> '' then begin
      SB.Append(Request_Header_Referer);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.Referer);
      SB.Append(CRLF);
    end;

    if FOwner.Cookies.Count > 0 then begin
      SB.Append(Request_Header_Cookie);
      SB.Append(HeaderSeparator);
      SB.Append(FOwner.Cookies.DelimitedText);
      SB.Append(CRLF);
    end;

    case FOwner.Method of
      rmOPTIONS, rmPOST, rmPUT, rmDELETE, rmTRACE, rmPATCH: begin
        if FOwner.ContentType <> '' then begin
          SB.Append(Entity_Header_ContentType);
          SB.Append(HeaderSeparator);
          SB.Append(FOwner.ContentType);
          SB.Append(CRLF);
        end;

        if (FOwner.ContentLength > -1) and not FOwner.SendChunked then begin
          SB.Append(Entity_Header_ContentLength);
          SB.Append(HeaderSeparator);
          SB.Append(IntToStr(FOwner.ContentLength));
          SB.Append(CRLF);
        end;
      end;
    end;

    for i := 0 to Count - 1 do begin
      SB.Append(Keys[i]);
      SB.Append(HeaderSeparator);
      SB.Append(Values[i]);
      SB.Append(CRLF);
    end;
  end;

  if FOwner.UserAgent <> '' then begin
    SB.Append(Request_Header_UserAgent);
    SB.Append(HeaderSeparator);
    SB.Append(FOwner.UserAgent);
    SB.Append(CRLF);
  end;

  Result := SB.ToString;
end;

{ TScWebResponseHeaderCollection }

constructor TScWebResponseHeaderCollection.Create(Owner: TCRHttpWebResponse);
begin
  inherited Create;

  FOwner := Owner;
end;

function TScWebResponseHeaderCollection.ToString: string;
var
  i: integer;
begin
  SB.Length := 0;

  for i := 0 to Count - 1 do begin
    SB.Append(Keys[i]);
    SB.Append(HeaderSeparator);
    SB.Append(Values[i]);
    SB.Append(CRLF);
  end;

  Result := SB.ToString;
end;

{ TScHttpWebRequest }


constructor TCRHttpWebRequest.Create(const URL: string);
begin
  inherited Create;

  Init;
  FRequestUri := Trim(URL);
  FAddress := FRequestUri;
end;

procedure TCRHttpWebRequest.Init;
begin
  FCachePolicy := TScRequestCachePolicy.Create;
  FCredentials := TScNetworkCredential.Create;
  FHeaders := TScWebRequestHeaderCollection.Create(Self);
  FProtocolVersion := TScVersion.Create(1, 1);
  FProxy := TScWebProxy.Create;
  FSSLOptions := TSSLOptions.Create;
  FCookies := TStringList.Create;
{$IFDEF VER11P}
  FCookies.StrictDelimiter := True;
{$ENDIF}
  FCookies.Delimiter := ';';

  FIPVersion := DefValIPVersion;
  FContentLength := -1;
  FKeepAlive := True;
  FReadWriteTimeout := 15;
  FMaximumAutomaticRedirections := 50;
  FMaximumAutomaticReconnections := 10;

  FSendChunked := False;
  FSendBlockSize := SEND_BLOCK_SIZE;

  FAllowedStatuses := [];
  FMethod := rmGET;
end;

destructor TCRHttpWebRequest.Destroy;
begin
  FSecureConnection.Release;

  FCachePolicy.Free;
  FCredentials.Free;
  FHeaders.Free;
  FProtocolVersion.Free;
  FProxy.Free;
  FSSLOptions.Free;
  FCookies.Free;

  inherited;
end;

procedure TCRHttpWebRequest.CheckInactive;
begin
  if FSecureConnection <> nil then
    raise HttpException.CreateRes(@SRequestExecuted);
end;

procedure TCRHttpWebRequest.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('HeadersText', ReadHeadersText, WriteHeadersText, FHeaders.Text <> '');
end;

procedure TCRHttpWebRequest.ReadHeadersText(Reader: TReader);
begin
  FHeaders.Text := Reader.ReadString();
end;

procedure TCRHttpWebRequest.WriteHeadersText(Writer: TWriter);
begin
  Writer.WriteString(FHeaders.Text);
end;

procedure TCRHttpWebRequest.WriteChunk(
  Buffer: TValueArr; Count: integer; CancellationToken: TScCancellationToken);
var
  Cancel: boolean;
begin
  if Assigned(FBeforeSendData) then begin
    Cancel := False;
    FBeforeSendData(Self, FContentWrote, FContentLength, Cancel);
    if Cancel then
      raise OperationCanceledException.Create(SDataSendingCanceled);
  end;

  FSecureConnection.Write(Buffer, 0, Count);
  Inc(FContentWrote, Count);

  if CancellationToken <> nil then
    CancellationToken.ThrowIfCancellationRequested;
end;

procedure TCRHttpWebRequest.WriteChunkSize(Size: integer);
begin
  FSecureConnection.WriteLine(IntToHex(Size, 1));
end;

procedure TCRHttpWebRequest.GetNextChunkData(
  out Buffer: TValueArr; out Count: Integer);
begin
  if FRequestStream <> nil then begin
    if FContentLength = -1 then
      Count := FRequestStream.Read(FSendBuffer[0], Length(FSendBuffer))
    else begin
      Count := FContentLength - FContentWrote;
      if Count > Length(FSendBuffer) then
        Count := Length(FSendBuffer);

      Count := FRequestStream.Read(FSendBuffer[0], Count);
    end;

    Buffer := TValueArr(FSendBuffer);
  end
  else
    FOnGetNextChunkData(Self, Buffer, Count);
end;

procedure TCRHttpWebRequest.InternalWriteData(
  CancellationToken: TScCancellationToken);
var
  Buffer: TValueArr;
  Count: integer;
begin
  FContentWrote := 0;

  if FSendChunked then begin
    if (FRequestStream = nil) and not Assigned(FOnGetNextChunkData) then
      raise HttpException.Create(SRequestStreamIsNotAssigned);

    if FRequestStream <> nil then // else - data will be received from the OnGetNextChunkData event
      if FSendBlockSize > 0 then
        SetLength(FSendBuffer, FSendBlockSize)
      else
      if FContentLength > 0 then
        SetLength(FSendBuffer, FContentLength)
      else
        SetLength(FSendBuffer, SEND_BLOCK_SIZE);

    while (FContentLength = -1) or (FContentWrote < FContentLength) do begin
      GetNextChunkData(Buffer, Count);
      if Count = 0 then
        Break;

      WriteChunkSize(Count);
      WriteChunk(Buffer, Count, CancellationToken);
      FSecureConnection.Write(TValueArr(AnsiString(CRLF)), 0, 2);
    end;

    WriteChunkSize(0);
    FSecureConnection.Write(TValueArr(AnsiString(CRLF)), 0, 2); // empty Headers
  end
  else begin
    while FContentWrote < FContentLength do begin
      Count := FContentLength - FContentWrote;
      if (FSendBlockSize > 0) and (Count > FSendBlockSize) then
        Count := FSendBlockSize;

      WriteChunk(@FSendBuffer[FContentWrote], Count, CancellationToken);
    end;
  end;
end;

procedure TCRHttpWebRequest.CheckRequest;
begin
  if (FMethod in [rmGET, rmHEAD]) and ((FContentLength > -1) or FSendChunked) then
    raise HttpException.CreateFmt(SCannotSendContent, [SRequestMethod[Method]]);

  if (FMethod in [rmPOST, rmPUT]) and (FContentLength = -1) and not FSendChunked then
    raise HttpException.Create(SContentLengthNotSet);

  if FSendChunked then begin
    if (FRequestStream = nil) and not Assigned(FOnGetNextChunkData) then
      raise HttpException.Create(SRequestStreamIsNotAssigned);
    if Length(FSendBuffer) > 0 then
      raise HttpException.Create(SBufferedDataIsWrote);
  end
  else begin
    if FRequestStream <> nil then
      raise HttpException.Create(SRequestStreamIsAssigned);

    if (FContentLength > 0) and (Length(FSendBuffer) < FContentLength) then
      raise HttpException.CreateRes(@SInvalidContentBuffer);

    if TransferEncoding <> '' then
      raise HttpException.Create(SSendChunkedAndTransferEncoding);
  end;
end;

procedure TCRHttpWebRequest.SendRequest(
  CancellationToken: TScCancellationToken = nil);
var
  Selector: string;
  Header: string;
  i: integer;
begin
  if FSendChunked and (FTransferEncoding = '') then
    FTransferEncoding := SChunked;

  if FMethod = rmCONNECT then
    Selector := FNetworkLocation + ':' + IntToStr(FPortNo)
  else
    Selector := FPath + FResource + FParameters + FQuery + FFragment;

  for i := 1 to Length(Selector) do
    if Selector[i] = ' ' then
      Selector[i] := '+';

  Header := SRequestMethod[FMethod] + ' ' + Selector + ' HTTP/' + FProtocolVersion.ToString + CRLF;
  Header := Header + FHeaders.ToString;
  Header := Header + CRLF;

  Assert(FSecureConnection <> nil);
  FSecureConnection.Write(TValueArr(AnsiString(Header)), 0, Length(Header));
  if CancellationToken <> nil then
    CancellationToken.ThrowIfCancellationRequested;

  if FMethod in [rmOPTIONS, rmPOST, rmPUT, rmDELETE, rmTRACE, rmPATCH] then
    InternalWriteData(CancellationToken);
end;

function TCRHttpWebRequest.GetResponse(CancellationToken: TScCancellationToken = nil): TCRHttpWebResponse;

  function StatusIsTimeout(StatusCode: integer): boolean;
  begin
    Result := (StatusCode = 502) or (StatusCode = 503) or (StatusCode = 504);
  end;

  function StatusIsRedirection(StatusCode: integer): boolean;
  begin
    Result := (StatusCode >= 300) and (StatusCode <= 307);
  end;

var
  ConnectionParameters: TScSecureConnectionParameters;
  Hostname: string;
  Port: integer;
  UserName, Password: string;
  ServerMessage: string;
  RetryCount, Redirections: integer;
  IsInternalTimeout: boolean;
  IsSecure: boolean;
  UsePooling: boolean;
begin
  CheckInactive;
  CheckRequest;

  FStatusCode := 0;
  FStatusDescription := '';
  ServerMessage := '';
  RetryCount := 0;
  Redirections := 0;
  IsInternalTimeout := False;

  Result := TCRHttpWebResponse.Create;
  try
    try
      repeat
        DoBeforeSendRequest;

        TScHttpParser.ParseURL(FAddress, FScheme, UserName, Password,
          FNetworkLocation, FPort, FPath, FResource, FParameters, FQuery, FFragment);
        if FScheme = '' then
          FScheme := HttpScheme;

        if (UserName <> '') or (Password <> '') then begin
          FCredentials.UserName := UserName;
          FCredentials.Password := Password;
        end;

        if FPort = '' then
          FPortNo := StrToInt(DefaultPort(FScheme))
        else
          FPortNo := StrToInt(Copy(FPort, 2, Length(FPort)));

        if FProxy.Address = '' then begin
          Hostname := FNetworkLocation;
          Port := FPortNo;
        end
        else begin
          Hostname := FProxy.Address;
          Port := FProxy.Port;
        end;

        IsInternalTimeout := False;
        IsSecure := AnsiSameText(FScheme, 'https:') or AnsiSameText(FScheme, 'wss:');
        FSSLOptions.IdentityDNSName := FNetworkLocation;

        Assert(FSecureConnection = nil);
        UsePooling := (FConnectionGroupName <> '') and FKeepAlive;

        ConnectionParameters := TScSecureConnectionParameters.Create;
        try
          ConnectionParameters.ConnectionGroupName := FConnectionGroupName;
          ConnectionParameters.Hostname := Hostname;
          ConnectionParameters.Port := Port;
          ConnectionParameters.IPVersion := FIPVersion;
          ConnectionParameters.Timeout := FReadWriteTimeout;
          ConnectionParameters.IsSecure := IsSecure;
          ConnectionParameters.SSLOptions.Assign(FSSLOptions);

          ConnectionParameters.IOHandler := FIOHandler;

          if UsePooling then begin
            FSecureConnection := TScConnectionPoolManager.GetConnection(ConnectionParameters);
            FSecureConnection.AddRef;
            FSecureConnection.SetReadWriteTimeout(FReadWriteTimeout);
          end
          else begin
            FSecureConnection := TScSecureConnection.Create;
            FSecureConnection.CreateVio(ConnectionParameters);
          end;
        finally
          ConnectionParameters.Free;
        end;

        Result.FSecureConnection := FSecureConnection;
        Result.FSecureConnection.AddRef;
        Result.FStatusCode := 0;

        if CancellationToken <> nil then
          CancellationToken.ThrowIfCancellationRequested;

        if not FSecureConnection.IsConnected then begin
          FSecureConnection.Connect;
          if CancellationToken <> nil then
            CancellationToken.ThrowIfCancellationRequested;

          if (FProxy.Address <> '') and (FMethod <> rmCONNECT) then begin
            FOldMethod := FMethod;
            try
              FMethod := rmCONNECT;
              SendRequest(CancellationToken);
              Result.FMethod := FMethod;
              Result.RetrieveHeaders(CancellationToken);
            finally
              FMethod := FOldMethod;
            end;
          end;
        end;

        try
          if (Result.FStatusCode = 0) or (Result.FStatusCode = 200) then begin
            Result.FStatusCode := 0;
            if IsSecure then begin
              FSecureConnection.IsSecure := True;
            end;

            FOldMethod := FMethod;

            SendRequest(CancellationToken);

            DoAfterSendRequest;

            if Assigned(FOnConnected) then
              FOnConnected(Self);

            Result.FMethod := FMethod;
            Result.RetrieveHeaders(CancellationToken);
            DoAfterRetrieveHeaders;
          end;
        except
          on SocketException do
            if UsePooling then begin
              IsInternalTimeout := True;
              Result.FStatusCode := 504;
            end
            else
              raise;
        end;

        if StatusIsTimeout(Result.FStatusCode) then begin
          Inc(RetryCount);
          if RetryCount > FMaximumAutomaticReconnections then
            if IsInternalTimeout then
              FSecureConnection.RaiseLastError
            else
              raise HttpException.Create(Result.FStatusCode, Result.FStatusDescription);

          if Result.FStatusCode = 503 then begin
            if (Result.FRetryAfter > 0) and (Result.FRetryAfter <= ReadWriteTimeout) then
              Sleep(Result.FRetryAfter * 1000)
            else
              raise HttpException.Create(Result.FStatusCode, Result.FStatusDescription);
          end
          else
            Sleep(RetryCount * 1000);
        end
        else
        if StatusIsRedirection(Result.FStatusCode) then begin
          Inc(Redirections);
          if (Redirections > FMaximumAutomaticRedirections) or (FAddress = Result.FLocation) or (Result.FLocation = '') then
            raise HttpException.Create(Result.FStatusCode, Result.FStatusDescription)
          else
          if Result.FStatusCode = 305 then
            FProxy.Address := Result.FLocation
          else
            FAddress := Result.FLocation;

          if Result.FCookies.Count > 0 then begin
            FCookies.Clear;
            FCookies.AddStrings(Result.FCookies);
          end;

          if FMethod = rmCONNECT then
            FMethod := rmGET;

          // FURL := 'http://' + Host + '/' + FURL;
          if (FAddress <> '') and (Pos('//', FAddress) = 0) and (FAddress[1] = '/') then
            FAddress := FScheme + '//' + FNetworkLocation + FPort + FAddress;
        end
        else
        if (Result.FStatusCode = 401) and Assigned(FOnAuthenticationNeeded) then
          FOnAuthenticationNeeded(Self)
        else
        if Result.FStatusCode >= 400 then begin
          if (Result.ContentLength > 0) or Result.FChunked then
            ServerMessage := Result.ReadAsString;

          if not (ConvertToStatusCode(Result.FStatusCode) in FAllowedStatuses) then
            raise HttpException.Create(Result.FStatusCode, Result.FStatusDescription, ServerMessage);
        end;

        if StatusIsRedirection(Result.FStatusCode) or StatusIsTimeout(Result.FStatusCode) then begin
          if FSecureConnection <> nil then begin
            FSecureConnection.Disconnect;
            FSecureConnection.Release;
            FSecureConnection := nil;
          end;
          Result.FSecureConnection.Release;
          Result.FSecureConnection := nil;
        end;

      until not StatusIsRedirection(Result.FStatusCode) and not StatusIsTimeout(Result.FStatusCode);
    finally
      if Assigned(FOnConnected) then
        FOnConnected(Self);
    end;

    Result.FResponseUri := FAddress;

  except
    on E: Exception do begin
      FStatusCode := Result.FStatusCode;
      FStatusDescription := Result.FStatusDescription;
      if FStatusDescription = '' then
        FStatusDescription := E.Message;

      if FSecureConnection <> nil then begin
        FSecureConnection.Disconnect;
        FSecureConnection.Release;
        FSecureConnection := nil;
      end;

      Result.Free;

      if not IsInternalTimeout and (FStatusCode <> 0) and (FStatusCode <> 200) then
        raise HttpException.Create(FStatusCode, FStatusDescription, ServerMessage)
      else
        raise;
    end;
  end;
end;


procedure TCRHttpWebRequest.DoBeforeSendRequest;
begin
  if Assigned(FBeforeSendRequest) then
    FBeforeSendRequest(Self);
end;

procedure TCRHttpWebRequest.DoAfterSendRequest;
begin
  if Assigned(FAfterSendRequest) then
    FAfterSendRequest(Self);
end;

procedure TCRHttpWebRequest.DoAfterRetrieveHeaders;
begin
  if Assigned(FAfterRetrieveHeaders) then
    FAfterRetrieveHeaders(Self);
end;

procedure TCRHttpWebRequest.Abort;
begin
  if FSecureConnection <> nil then
    FSecureConnection.Abort;
end;

procedure TCRHttpWebRequest.Disconnect;
begin
  if FSecureConnection <> nil then begin
    FSecureConnection.Disconnect;
    FSecureConnection.Release;
    FSecureConnection := nil;
  end;

  SetLength(FSendBuffer, 0);
end;

{$IFNDEF FPC}
{$IFDEF VER22P}
function TCRHttpWebRequest.Read(pv: IntPtr; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
{$ELSE}
function TCRHttpWebRequest.Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult;
{$ENDIF}
{$ELSE}
function TCRHttpWebRequest.Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT;
{$ENDIF}
begin
  if (FContentLength <> -1) and (FContentLength < cb) then
    cb := FContentLength;
  if Length(FSendBuffer) < integer(cb) then
    cb := Length(FSendBuffer);

  if pcbRead <> nil then
    pcbRead^ := cb;

  if (pv <> nil) and (cb > 0) then
    Move(FSendBuffer[0], pv^, cb);

  Result := S_OK;
end;

function TCRHttpWebRequest.ReadBuffer: TBytes;
var
  cnt: integer;
begin
  cnt := Length(FSendBuffer);
  if (FContentLength <> -1) and (FContentLength < cnt) then
    cnt := FContentLength;

  SetLength(Result, cnt);
  if cnt > 0 then
    Move(FSendBuffer[0], Result[0], cnt);
end;

{$IFNDEF FPC}
{$IFDEF VER22P}
function TCRHttpWebRequest.Write(pv: IntPtr; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
{$ELSE}
function TCRHttpWebRequest.Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult;
{$ENDIF}
{$ELSE}
function TCRHttpWebRequest.Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT;
{$ENDIF}
var
  OldLen: integer;
begin
  if FSendChunked then
    raise HttpException.Create(SCanNotWriteDataWhenSendChunked);

  if pcbWritten <> nil then
    pcbWritten^ := cb;

  if pv <> nil then begin
    OldLen := Length(FSendBuffer);
    SetLength(FSendBuffer, OldLen + integer(cb));
    Move(pv^, FSendBuffer[OldLen], cb);
  end;

  Result := S_OK;
end;

procedure TCRHttpWebRequest.WriteBuffer(const Buffer: TValueArr; Offset, Count: integer);
var
  OldLen: integer;
begin
  if FSendChunked then
    raise HttpException.Create(SCanNotWriteDataWhenSendChunked);

  if Count > 0 then begin
    OldLen := Length(FSendBuffer);
    SetLength(FSendBuffer, OldLen + Count);
    Move(Buffer[Offset], FSendBuffer[OldLen], Count);
  end;
end;

procedure TCRHttpWebRequest.WriteBuffer(const Data: TBytes);
begin
  WriteBuffer(TValueArr(Data), 0, Length(Data));
end;

procedure TCRHttpWebRequest.WriteData(Stream: TStream);
var
  Count: integer;
begin
  if FSendChunked then
    raise HttpException.Create(SCanNotWriteDataWhenSendChunked);

  Count := Stream.Size - Stream.Position;
  SetLength(FSendBuffer, Count);
  if Count > 0 then
    Stream.Read(FSendBuffer[0], Count);
end;

class function TCRHttpWebRequest.DefaultPort(const Scheme: string): string;
var
  i: integer;
begin
  Result := PortDefault;

  if Scheme <> '' then
    for i := Low(PORTS) to High(PORTS) do
      if AnsiSameText(Scheme, PORTS[i, 1]) then begin
        Result := PORTS[i, 2];
        break;
      end;
end;

function TCRHttpWebRequest.GetStatusCode: TScHttpStatusCode;
begin
  Result := ConvertToStatusCode(FStatusCode);
end;

function TCRHttpWebRequest.GetIsSecure: boolean;
begin
  if FSecureConnection <> nil then
    Result := FSecureConnection.IsSecure
  else
    Result := False;
end;


procedure TCRHttpWebRequest.SetAccept(const Value: string);
begin
  CheckInactive;
  FAccept := Value;
end;

procedure TCRHttpWebRequest.SetCachePolicy(Value: TScRequestCachePolicy);
begin
  CheckInactive;
  FCachePolicy.Assign(Value);
end;

procedure TCRHttpWebRequest.SetConnection(const Value: string);
begin
  CheckInactive;
  FConnection := Value;
end;

procedure TCRHttpWebRequest.SetConnectionGroupName(const Value: string);
begin
  CheckInactive;
  FConnectionGroupName := Value;
end;

procedure TCRHttpWebRequest.SetContentLength(const Value: Int64);
begin
  CheckInactive;
  FContentLength := Value;
end;

procedure TCRHttpWebRequest.SetContentType(const Value: string);
begin
  CheckInactive;
  FContentType := Value;
end;

procedure TCRHttpWebRequest.SetCookies(Value: TStringList);
begin
  CheckInactive;
  FCookies.Assign(Value);
end;

procedure TCRHttpWebRequest.SetCredentials(Value: TScNetworkCredential);
begin
  CheckInactive;
  FCredentials.Assign(Value);
end;

procedure TCRHttpWebRequest.SetDate(const Value: TDateTime);
begin
  CheckInactive;
  FDate := Value;
end;

procedure TCRHttpWebRequest.SetExpect(const Value: string);
begin
  CheckInactive;
  FExpect := Value;
end;

procedure TCRHttpWebRequest.SetFrom(const Value: string);
begin
  CheckInactive;
  FFrom := Value;
end;

procedure TCRHttpWebRequest.SetHeaders(Value: TScWebHeaderCollection);
begin
  CheckInactive;
  FHeaders.Assign(Value);
end;

procedure TCRHttpWebRequest.SetHost(const Value: string);
begin
  CheckInactive;
  FHost := Value;
end;

procedure TCRHttpWebRequest.SetIfModifiedSince(const Value: TDateTime);
begin
  CheckInactive;
  FIfModifiedSince := Value;
end;

procedure TCRHttpWebRequest.CheckKeepAliveHeader;
begin
  if FKeepAlive then begin
    if FConnection = '' then
      FConnection := SKeepAlive
    else
      if Pos(SKeepAlive, FConnection) = 0 then
        FConnection := SKeepAlive + ', ' + FConnection;
  end
  else begin
    if FConnection = '' then
      FConnection := SClose
    else
      if Pos(SClose, FConnection) = 0 then
        FConnection := SClose + ', ' + FConnection;
  end;
end;

procedure TCRHttpWebRequest.SetKeepAlive(const Value: boolean);
begin
  if FKeepAlive <> Value then begin
    CheckInactive;
    FKeepAlive := Value;
    CheckKeepAliveHeader;
  end;
end;

procedure TCRHttpWebRequest.SetMethod(const Value: TScRequestMethod);
begin
  CheckInactive;
  FMethod := Value;
end;

procedure TCRHttpWebRequest.SetProtocolVersion(Value: TScVersion);
begin
  CheckInactive;
  FProtocolVersion.Assign(Value);
end;

procedure TCRHttpWebRequest.SetProxy(Value: TScWebProxy);
begin
  CheckInactive;
  FProxy.Assign(Value);
end;

procedure TCRHttpWebRequest.SetRange(const Value: string);
begin
  CheckInactive;
  FRange := Value;
end;

procedure TCRHttpWebRequest.SetReadWriteTimeout(const Value: integer);
begin
  if FReadWriteTimeout <> Value then begin
    CheckInactive;
    FReadWriteTimeout := Value;
    if FSecureConnection <> nil then
      FSecureConnection.SetReadWriteTimeout(Value);
  end;
end;

procedure TCRHttpWebRequest.SetReferer(const Value: string);
begin
  CheckInactive;
  FReferer := Value;
end;

procedure TCRHttpWebRequest.SetRequestUri(const Value: string);
begin
  CheckInactive;
  FRequestUri := Trim(Value);
  FAddress := FRequestUri;
end;

procedure TCRHttpWebRequest.SetTransferEncoding(const Value: string);
begin
  CheckInactive;
  FTransferEncoding := Value;
end;

procedure TCRHttpWebRequest.SetUpgrade(const Value: string);
begin
  CheckInactive;
  FUpgrade := Value;
end;

procedure TCRHttpWebRequest.SetUserAgent(const Value: string);
begin
  CheckInactive;
  FUserAgent := Value;
end;

procedure TCRHttpWebRequest.SetIPVersion(const Value: TIPVersion);
begin
  CheckInactive;
  FIPVersion := Value;
end;

procedure TCRHttpWebRequest.SetMaximumAutomaticRedirections(const Value: integer);
begin
  CheckInactive;
  FMaximumAutomaticRedirections := Value;
end;

procedure TCRHttpWebRequest.SetMaximumAutomaticReconnections(const Value: integer);
begin
  CheckInactive;
  FMaximumAutomaticReconnections := Value;
end;

procedure TCRHttpWebRequest.SetSendChunked(const Value: boolean);
begin
  CheckInactive;
  FSendChunked := Value;
end;

procedure TCRHttpWebRequest.SetSendBlockSize(const Value: integer);
begin
  CheckInactive;
  FSendBlockSize := Value;
end;

procedure TCRHttpWebRequest.SetSSLOptions(Value: TSSLOptions);
begin
  CheckInactive;
  FSSLOptions.Assign(Value);
end;

procedure TCRHttpWebRequest.SetRequestStream(Value: TStream);
begin
  CheckInactive;
  FRequestStream := Value;
end;

procedure TCRHttpWebRequest.SetAllowedStatuses(const Value: TScHttpStatusCodes);
begin
  CheckInactive;
  FAllowedStatuses := Value;
end;

{ TScHttpWebResponse }

constructor TCRHttpWebResponse.Create;
begin
  inherited;

  FHeaders := TScWebResponseHeaderCollection.Create(Self);
  FProtocolVersion := TScVersion.Create;
  FCookies := TStringList.Create;
{$IFDEF VER11P}
  FCookies.StrictDelimiter := True;
{$ENDIF}
  FCookies.Delimiter := ';';

  FContentLength := -1;
  FMethod := rmGET;
end;

destructor TCRHttpWebResponse.Destroy;
begin
  try
    if FSecureConnection <> nil then
      SkipAll;
  except
  end;

  FHeaders.Free;
  FProtocolVersion.Free;
  FCookies.Free;

  if FSecureConnection <> nil then begin
    FSecureConnection.TryReturnToPool;
    FSecureConnection.Release;
    FSecureConnection := nil;
  end;

  inherited;
end;

procedure TCRHttpWebResponse.Abort;
begin
  if FSecureConnection <> nil then
    FSecureConnection.Abort;
end;

{$IFNDEF FPC}
{$IFDEF VER22P}
function TCRHttpWebResponse.Read(pv: IntPtr; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
{$ELSE}
function TCRHttpWebResponse.Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult;
{$ENDIF}
{$ELSE}
function TCRHttpWebResponse.Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT;
{$ENDIF}
var
  cbReadBytes: integer;
begin
  cbReadBytes := InternalRead(pv, 0, cb);
  if pcbRead <> nil then
    Marshal.WriteInt32(pcbRead, cbReadBytes);
  Result := S_OK;
end;

{$IFNDEF FPC}
{$IFDEF VER22P}
function TCRHttpWebResponse.Write(pv: IntPtr; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
{$ELSE}
function TCRHttpWebResponse.Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult;
{$ENDIF}
{$ELSE}
function TCRHttpWebResponse.Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT;
{$ENDIF}
begin
  if pcbWritten <> nil then
    pcbWritten^ := 0;

  Result := S_FALSE;
end;

function TCRHttpWebResponse.ReadBuffer(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  Result := InternalRead(Buffer, Offset, Count);
end;

function TCRHttpWebResponse.ReadAsString: string;
var
  Offset, Count: integer;
begin
  Offset := 0;

  repeat
    if Length(FTmpBuf) - Offset < 1024 then
      SetLength(FTmpBuf, Length(FTmpBuf) + READ_BUFFER_SIZE);

    Count := InternalRead(TValueArr(FTmpBuf), Offset, Length(FTmpBuf) - Offset);
    Inc(Offset, Count);
  until Count <= 0;

  Result := Encoding.UTF8.GetString(FTmpBuf, 0, Offset);
end;

function TCRHttpWebResponse.ReadAsBytes: TBytes;
var
  Offset, Count: integer;
begin
  SetLength(Result, 0);
  Offset := 0;

  repeat
    if Length(Result) - Offset < 1024 then
      SetLength(Result, Length(Result) + READ_BUFFER_SIZE);

    Count := InternalRead(TValueArr(Result), Offset, Length(Result) - Offset);
    Inc(Offset, Count);
  until Count <= 0;

  SetLength(Result, Offset);
end;

function TCRHttpWebResponse.ReadToStream(Stream: TStream): integer;
var
  Count: integer;
begin
  if Length(FTmpBuf) < READ_BUFFER_SIZE then
    SetLength(FTmpBuf, READ_BUFFER_SIZE);

  Result := 0;

  repeat
    Count := InternalRead(TValueArr(FTmpBuf), 0, Length(FTmpBuf));
    Stream.Write(FTmpBuf[0], Count);
    Inc(Result, Count);
  until Count <= 0;
end;

procedure TCRHttpWebResponse.RetrieveHeaders(CancellationToken: TScCancellationToken = nil);
var
  ReplyMsg, Key, Value: string;
  Index, p: integer;
  i: integer;
  CookieArr: TStringArray;
begin
  FSecureConnection.ClearBuffer;
  FStatusCode := 0;
  FStatusDescription := '';
  FRetryAfter := 0;
  FContentLength := -1;
  FContentRead := 0;
  FChunked := False;
  FHeaders.Clear;

  ReplyMsg := FSecureConnection.ReadLine(CancellationToken);
  if UpperCase(Copy(ReplyMsg, 1, 5)) <> 'HTTP/' then
    raise HttpException.CreateRes(@SUnknownProtocol);

  p := Pos(' ', ReplyMsg);
  FProtocolVersion.Parse(Trim(Copy(ReplyMsg, 6, p - 6)));
  Delete(ReplyMsg, 1, p);

  p := Pos(' ', ReplyMsg);
  FStatusCode := StrToIntDef(Copy(ReplyMsg, 1, p - 1), 0);
  Delete(ReplyMsg, 1, p);
  FStatusDescription := ReplyMsg;

  while True do begin
    ReplyMsg := Trim(FSecureConnection.ReadLine(CancellationToken));
    if ReplyMsg = '' then
      Break;

    TScHttpParser.ParseKeyValue(ReplyMsg, Key, Value);
    FHeaders.Add(Key, Value);
  end;

  // Set-Cookie
  FCookies.Clear;
  if FHeaders.Find(Response_Header_SetCookie, Index) then begin
    Value := FHeaders.Values[Index];
    if Value <> '' then begin
      SetLength(CookieArr, 0);
      CookieArr := TScHttpParser.ParseTokens(Value, ';');
      for i := 0 to Length(CookieArr) - 1 do
        FCookies.Add(CookieArr[i]);
    end;
  end;

  // Location
  if FHeaders.Find(Response_Header_Location, Index) then
    FLocation := FHeaders.Values[Index];

  // Retry-After
  if FHeaders.Find(Response_Header_RetryAfter, Index) then
    FRetryAfter := StrToIntDef(FHeaders.Values[Index], 0);

  // Transfer-Encoding
  if FHeaders.Find(General_Header_TransferEncoding, Index) then begin
    if TScHttpParser.FindToken(SChunked, FHeaders.Values[Index], ',') >= 0 then begin
      FChunked := True;
      FContentLength := 0;
    end;
  end
  else
  // Content-Length
  if FHeaders.Find(Entity_Header_ContentLength, Index) then
    FContentLength := StrToInt64Def(FHeaders.Values[Index], 0);
end;

function TCRHttpWebResponse.GetResponseHeader(const HeaderName: string): string;
var
  Idx: integer;
begin
  Idx := FHeaders.IndexOf(HeaderName);
  if Idx = -1 then
    raise Exception.CreateFmt(SHeaderFieldNotFound, [HeaderName])
  else
    Result := FHeaders.Values[Idx];
end;

function TCRHttpWebResponse.WaitForData(MillisecondsTimeout: integer): boolean;
begin
  Result := FChunked or ((FSecureConnection <> nil) and (FSecureConnection.CheckForDataOnSource(-1) > 0{check the data was read}));

  if not Result then
    if FContentLength <> -1 then
      Result := FContentLength > FContentRead
    else
      Result := (FSecureConnection <> nil) and FSecureConnection.WaitForData(MillisecondsTimeout);
end;

function TCRHttpWebResponse.ReadChunk(const Buffer: TValueArr; Offset, Count: integer): integer;
var
  ReadCount: integer;
begin
  Result := 0;
  if (FSecureConnection = nil) or not FSecureConnection.IsValid then
    Exit;

  if FContentLength <> -1 then begin
    if FContentRead >= FContentLength then
      Exit;
    if Count > FContentLength - FContentRead then
      Count := FContentLength - FContentRead;
  end;

  ReadCount := FSecureConnection.Read(Buffer, Offset, Min(Count, FSecureConnection.CheckForDataOnSource(-1){check the data was read}));
  Inc(Offset, ReadCount);
  Dec(Count, ReadCount);
  Result := ReadCount;

  while (Count > 0) and ((Result = 0) or (FContentLength <> -1) or FSecureConnection.WaitForData(0)) do begin
    ReadCount := FSecureConnection.Read(Buffer, Offset, Count);
    if ReadCount <= 0 then
      Break;

    Inc(Offset, ReadCount);
    Dec(Count, ReadCount);
    Inc(Result, ReadCount);
  end;

  Inc(FContentRead, Result);
  DoProgress(FContentLength, FContentRead);
end;

procedure TCRHttpWebResponse.DoProgress(const Total, Current: Int64);
var
  Cancel: boolean;
begin
  if Assigned(FOnProgress) then begin
    Cancel := False;
    FOnProgress(Self, Total, Current, Cancel);
    if Cancel then
      raise OperationCanceledException.Create(SDataReadingCanceled);
  end;
end;

function TCRHttpWebResponse.InternalRead(const Buffer: TValueArr; Offset, Count: integer): integer;

  procedure ReadHeaders;
  var
    ReplyMsg, Key, Value: string;
  begin
    ReplyMsg := FSecureConnection.ReadLine;
    while ReplyMsg <> '' do begin
      TScHttpParser.ParseKeyValue(ReplyMsg, Key, Value);
      FHeaders.Add(Key, Value);
      ReplyMsg := FSecureConnection.ReadLine;
    end;
  end;

  procedure ReadChunkSize;
  var
    ReplyMsg: string;
    ChunkSize: integer;
  begin
    ReplyMsg := FSecureConnection.ReadLine;
    if ReplyMsg = '' then begin
      FChunked := False;
      Exit;
    end;

    ChunkSize := StrToInt('$' + TScHttpParser.NthWord(ReplyMsg, ';', 1));
    if ChunkSize = 0 then
      FChunked := False
    else
      Inc(FContentLength, ChunkSize);
  end;

var
  Read: integer;
  DataCount: integer;
  TmpBuffer: array[0..1] of byte;
begin
  if FChunked then begin
    Result := 0;
    if FSecureConnection = nil then
      Exit;

    if Count > 0 then begin
      if FContentRead = FContentLength then begin
        ReadChunkSize;
        if not FChunked then begin
          ReadHeaders;
          Exit;
        end;
      end;

      Read := ReadChunk(Buffer, Offset, Count);
      Inc(Result, Read);
      Dec(Count, Read);

      if (Count > 0) and (FContentRead < FContentLength) then
        raise HttpException.CreateRes(@SInvalidMessageLength);

      if FContentRead = FContentLength then begin /// read CRLF
        DataCount := FSecureConnection.CheckForDataOnSource(2);
        if DataCount < 2 then begin
          FChunked := False;
          raise HttpException.CreateRes(@SInvalidMessage);
        end;

        FSecureConnection.Read(@TmpBuffer[0], 0, 2);
        if (TmpBuffer[0] <> 13) or (TmpBuffer[1] <> 10) then begin
          FChunked := False;
          raise HttpException.CreateRes(@SInvalidMessage);
        end;
      end;
    end;
  end
  else
    Result := ReadChunk(Buffer, Offset, Count);
end;

procedure TCRHttpWebResponse.SkipAll;
begin
  if FSecureConnection = nil then
    Exit;

  if FContentLength = -1 then
    FSecureConnection.Read(nil, 0, FSecureConnection.CheckForDataOnSource(-1){check the data was read})
  else begin
    InternalRead(nil, 0, $7FFFFFFF);
    while FChunked do
      InternalRead(nil, 0, $7FFFFFFF);
  end;
end;

function TCRHttpWebResponse.GetIsSecure: boolean;
begin
  if FSecureConnection <> nil then
    Result := FSecureConnection.IsSecure
  else
    Result := False;
end;

function TCRHttpWebResponse.GetStatusCode: TScHttpStatusCode;
begin
  Result := ConvertToStatusCode(FStatusCode);
end;

function TCRHttpWebResponse.GetContentEncoding: string;
begin
  FHeaders.TryGetValue(Entity_Header_ContentEncoding, Result);
end;

function TCRHttpWebResponse.GetContentType: string;
begin
  FHeaders.TryGetValue(Entity_Header_ContentType, Result);
end;

function TCRHttpWebResponse.GetLastModified: TDateTime;
var
  Value: string;
begin
  FHeaders.TryGetValue(Entity_Header_LastModified, Value);
  if Value = '' then
    Result := 0
  else
    Result := InternetStrToDateTime(Value);
end;

function TCRHttpWebResponse.GetServer: string;
begin
  FHeaders.TryGetValue(Response_Header_Server, Result);
end;

{ HttpException }

constructor HttpException.Create(const Msg: string);
begin
  inherited Create(Msg);

  FStatusCode := 0;
end;

constructor HttpException.Create(StatusCode: integer; const Msg: string);
begin
  inherited Create(Msg);

  FStatusCode := StatusCode;
end;

constructor HttpException.Create(StatusCode: TScHttpStatusCode; const Msg: string);
begin
  inherited Create(Msg);

  FStatusCode := HTTP_STATUS_CODES[StatusCode];
end;

constructor HttpException.Create(StatusCode: integer; const Msg, ServerMessage: string);
begin
  inherited Create(Msg);

  FStatusCode := StatusCode;
  FServerMessage := ServerMessage;
end;

function HttpException.GetStatusCode: TScHttpStatusCode;
begin
  Result := ConvertToStatusCode(FStatusCode);
end;

{ TScHttpWebResponseHelper }

class function TScHttpWebResponseHelper.GetSecureConnection(Obj: TCRHttpWebResponse): TScSecureConnection;
begin
  Result := Obj.FSecureConnection;
end;

class procedure TScHttpWebResponseHelper.SetSecureConnection(Obj: TCRHttpWebResponse; Value: TScSecureConnection);
begin
  Obj.FSecureConnection := Value;
end;

end.
