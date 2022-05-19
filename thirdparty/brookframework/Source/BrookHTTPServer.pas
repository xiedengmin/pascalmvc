﻿(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2021 Silvio Clecio <silvioprog@gmail.com>
 *
 * Brook framework is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Brook framework; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

{ Contains classes which composes a fast event-driven HTTP(S) server. }

unit BrookHTTPServer;

{$I BrookDefines.inc}

interface

uses
  SysUtils,
  Classes,
  Marshalling,
  libsagui,
  BrookUtility,
  BrookHandledClasses,
  BrookExtra,
  BrookHTTPAuthentication,
  BrookHTTPRequest,
  BrookHTTPResponse;

resourcestring
  { Error message @code('Active server.'). }
  SBrookActiveServer = 'Active server.';
  { Error message @code('Cannot create server handle.'). }
  SBrookCannotCreateServerHandle = 'Cannot create server handle.';
  { Error message @code('TLS is not available.'). }
  SBrookTLSNotAvailable = Concat(
    'TLS is not available. Please download the Sagui library with ',
    'TLS support at: https://github.com/risoflora/libsagui/releases');
  { Error message @code('Private key cannot be empty.'). }
  SBrookEmptyPrivateKey = 'Private key cannot be empty.';
  { Error message @code('Certificate cannot be empty.'). }
  SBrookEmptyCertificate = 'Certificate cannot be empty.';

type
  { Handles exceptions related to HTTP server security. }
  EBrookHTTPServerSecurity = class(Exception);

  { Class which holds the TLS properties for the HTTPS server. }
  TBrookHTTPServerSecurity = class(TPersistent)
  private
    FActive: boolean;
    FPrivateKey: string;
    FPrivatePassword: string;
    FCertificate: string;
    FTrust: string;
    FDHParams: string;
    FPriorities: string;
    function IsActiveStored: boolean;
  public
    { Copies properties from another security source.
      @param(ASource[in] Security source.) }
    procedure Assign(ASource: TPersistent); override;
    { Clears the common TLS properties. }
    procedure Clear; virtual;
    { Validates the common TLS properties. }
    procedure Validate; {$IFNDEF DEBUG}inline;{$ENDIF}
  published
    { Activates the TLS support. }
    property Active: boolean read FActive write FActive stored IsActiveStored;
    { Content of the private key (key.pem) to be used by the HTTPS server. }
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    { Password of the private key. }
    property PrivatePassword: string read FPrivatePassword write FPrivatePassword;
    { Content of the certificate (cert.pem) to be used by the HTTPS server. }
    property Certificate: string read FCertificate write FCertificate;
    { Content of the certificate (ca.pem) to be used by the HTTPS server for
      client authentication. }
    property Trust: string read FTrust write FTrust;
    { Content of the Diffie-Hellman parameters (dh.pem) to be used by the HTTPS
      server for key exchange. }
    property DHParams: string read FDHParams write FDHParams;
    { Content of the cipher algorithm. Default: @code(NORMAL). }
    property Priorities: string read FPriorities write FPriorities;
  end;

  { Event signature used by HTTP server to handle the clients authentication. }
  TBrookHTTPAuthenticateEvent = function(ASender: TObject;
    AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse): boolean of object;

  { Event signature used by HTTP server to handle errors in the clients
    authentication. }
  TBrookHTTPAuthenticateErrorEvent = procedure(ASender: TObject;
    AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
    AResponse: TBrookHTTPResponse; AException: Exception) of object;

  { Event signature used by HTTP server to handle requests. }
  TBrookHTTPRequestEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse) of object;

  { Event signature used by HTTP server to handle error in the requests. }
  TBrookHTTPRequestErrorEvent = procedure(ASender: TObject;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
    AException: Exception) of object;

  { Event signature used by HTTP server to handle client connection events. }
  TBrookHTTPServerClientConnectionEvent = procedure(ASender: TObject;
    const AClient: Pointer; var AClosed: boolean) of object;

  { Handles exceptions related to HTTP server. }
  EBrookHTTPServer = class(Exception);

  { Fast event-driven HTTP(S) server class. }
  TBrookHTTPServer = class(TBrookHandledComponent)
  private
    FHandle: Psg_httpsrv;
    FAuthenticated: boolean;
    FConnectionLimit: cardinal;
    FConnectionTimeout: cardinal;
    FNoFavicon: boolean;
    FPayloadLimit: nativeuint;
    FUploadsLimit: uint64;
    FActive: boolean;
    FPort: uint16;
    FPostBufferSize: nativeuint;
    FThreaded: boolean;
    FStreamedActive: boolean;
    FStreamedAuthenticated: boolean;
    FThreadPoolSize: cardinal;
    FUploadsDir: string;
    FLocker: TBrookLocker;
    FSecurity: TBrookHTTPServerSecurity;
    FOnAuthenticate: TBrookHTTPAuthenticateEvent;
    FOnAuthenticateError: TBrookHTTPAuthenticateErrorEvent;
    FOnRequest: TBrookHTTPRequestEvent;
    FOnRequestError: TBrookHTTPRequestErrorEvent;
    FOnClientConnection: TBrookHTTPServerClientConnectionEvent;
    FOnError: TBrookErrorEvent;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    function GetConnectionLimit: cardinal;
    function GetConnectionTimeout: cardinal;
    function GetPayloadLimit: nativeuint;
    function GetUploadsLimit: uint64;
    function GetPort: uint16;
    function GetPostBufferSize: nativeuint;
    function GetThreaded: boolean;
    function GetThreadPoolSize: cardinal;
    function GetUploadsDir: string;
    function IsActiveStored: boolean;
    function IsAuthenticatedStored: boolean;
    function IsConnectionLimitStored: boolean;
    function IsConnectionTimeoutStored: boolean;
    function IsNoFaviconStored: boolean;
    function IsPayloadLimitStored: boolean;
    function IsUploadsLimitStored: boolean;
    function IsPortStored: boolean;
    function IsPostBufferSizeStored: boolean;
    function IsThreadedStored: boolean;
    function IsThreadPoolSizeStored: boolean;
    function IsUploadsDirStored: boolean;
    procedure SetAuthenticated(AValue: boolean);
    procedure SetConnectionLimit(AValue: cardinal);
    procedure SetConnectionTimeout(AValue: cardinal);
    procedure SetPayloadLimit(AValue: nativeuint);
    procedure SetLocker(AValue: TBrookLocker);
    procedure SetSecurity(AValue: TBrookHTTPServerSecurity);
    procedure SetUploadsLimit(AValue: uint64);
    procedure SetPort(AValue: uint16);
    procedure SetPostBufferSize(AValue: nativeuint);
    procedure SetThreaded(AValue: boolean);
    procedure SetThreadPoolSize(AValue: cardinal);
    procedure SetUploadsDir(const AValue: string);
    procedure InternalCreateServerHandle; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure InternalFreeServerHandle; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure InternalShutdownServer; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure InternalCheckServerOption(Aret: cint);
{$IFNDEF DEBUG}inline;{$ENDIF}
    procedure InternalLibUnloadEvent(ASender: TObject);
  protected
    class function DoAuthenticationCallback(Acls: Pcvoid; Aauth: Psg_httpauth;
      Areq: Psg_httpreq; Ares: Psg_httpres): cbool; cdecl; static;
    class procedure DoRequestCallback(Acls: Pcvoid; Areq: Psg_httpreq;
      Ares: Psg_httpres); cdecl; static;
    class procedure DoClientConnectionCallback(Acls: Pcvoid;
      const Aclient: Pcvoid; Aclosed: Pcbool); cdecl; static;
    class procedure DoErrorCallback(Acls: Pcvoid; const Aerr: Pcchar);
      cdecl; static;
    function CreateLocker: TBrookLocker; virtual;
    function CreateAuthentication(AHandle: Pointer): TBrookHTTPAuthentication;
      virtual;
    function CreateSecurity: TBrookHTTPServerSecurity; virtual;
    function CreateRequest(AHandle: Pointer): TBrookHTTPRequest; virtual;
    function CreateResponse(AHandle: Pointer): TBrookHTTPResponse; virtual;
    function CreateError(const AMessage: string): Exception; virtual;
    procedure HandleAuthenticateError(AAuthentication: TBrookHTTPAuthentication;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse;
      AException: Exception); virtual;
    function HandleAuthenticate(AAuthentication: TBrookHTTPAuthentication;
      ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): boolean; virtual;
    procedure HandleRequestError(ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse; AException: Exception); virtual;
    procedure HandleRequest(ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); virtual;
    procedure HandleClientConnection(ASender: TObject; const AClient: Pointer;
      var AClosed: boolean); virtual;
    procedure HandleError(ASender: TObject; AException: Exception); virtual;
    procedure Loaded; override;
    function GetHandle: Pointer; override;
    function GetMHDHandle: Pointer; virtual;
    procedure DoError(ASender: TObject; AException: Exception); virtual;
    function DoAuthenticate(ASender: TObject;
      AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse): boolean; virtual;
    procedure DoAuthenticateError(ASender: TObject;
      AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse; AException: Exception); virtual;
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); virtual;
    procedure DoRequestError(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse; AException: Exception); virtual;
    procedure DoClientConnection(ASender: TObject; const AClient: Pointer;
      var AClosed: boolean); virtual;
    procedure CheckInactive; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetActive(AValue: boolean); virtual;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
  public
    { Creates an instance of @code(TBrookHTTPServer).
      @param(AOwner[in] Owner component.) }
    constructor Create(AOwner: TComponent); override;
    { Destroys an instance of @code(TBrookHTTPServer). }
    destructor Destroy; override;
    { Starts the HTTP(S) server. }
    procedure Open;
    { Stops the HTTP(S) server. }
    procedure Close;
    { Locks all other requests. }
    procedure Lock; {$IFNDEF DEBUG}inline;{$ENDIF}
    { Unlocks all other requests. }
    procedure Unlock; {$IFNDEF DEBUG}inline;{$ENDIF}
    { Contains the MHD instance. }
    property MHDHandle: Pointer read GetMHDHandle;
  published
    { Activates the HTTP(S) server. }
    property Active: boolean read FActive write SetActive stored IsActiveStored;
    { Enables/disables the basic HTTP authentication. }
    property Authenticated: boolean
      read FAuthenticated write SetAuthenticated stored IsAuthenticatedStored;
    { Port for listening to connections. }
    property Port: uint16 read GetPort write SetPort stored IsPortStored default 0;
    { Enables/disables the threaded model. If @true, the server creates one
      thread per connection. }
    property Threaded: boolean
      read GetThreaded write SetThreaded stored IsThreadedStored default False;
    { Directory to store the uploaded files. }
    property UploadsDir: string
      read GetUploadsDir write SetUploadsDir stored IsUploadsDirStored;
    { Post buffering size. }
    property PostBufferSize: nativeuint read GetPostBufferSize
      write SetPostBufferSize stored IsPostBufferSizeStored default BROOK_POST_BUFFER_SIZE;
    { Total payload limit. Use zero for no limit. }
    property PayloadLimit: nativeuint read GetPayloadLimit
      write SetPayloadLimit stored IsPayloadLimitStored default BROOK_PAYLOAD_LIMIT;
    { Total uploads limit. Use zero for no limit. }
    property UploadsLimit: uint64
      read GetUploadsLimit write SetUploadsLimit stored IsUploadsLimitStored default
      BROOK_UPLOADS_LIMIT;
    { Thread pool size. Size greater than 1 enables the thread pooling. }
    property ThreadPoolSize: cardinal read GetThreadPoolSize
      write SetThreadPoolSize stored IsThreadPoolSizeStored default 0;
    { Inactivity time (in seconds) to a client get time out. }
    property ConnectionTimeout: cardinal read GetConnectionTimeout
      write SetConnectionTimeout stored IsConnectionTimeoutStored default 0;
    { Concurrent connections limit. Use zero for no limit. }
    property ConnectionLimit: cardinal read GetConnectionLimit
      write SetConnectionLimit stored IsConnectionLimitStored default 0;
    { Enables/disables the favicon handling. If @true, it avoids @code(404) errors
      by sending an empty content (@code(204)) if path is @code('/favicon.ico'). }
    property NoFavicon: boolean
      read FNoFavicon write FNoFavicon stored IsNoFaviconStored default False;
    { Allows to lock other requests from accessing a block of code. }
    property Locker: TBrookLocker read FLocker write SetLocker;
    { Holds the TLS properties for the HTTPS server. }
    property Security: TBrookHTTPServerSecurity read FSecurity write SetSecurity;
    { Event triggered when a client requests authentication. }
    property OnAuthenticate: TBrookHTTPAuthenticateEvent
      read FOnAuthenticate write FOnAuthenticate;
    { Event triggered when a client authentication raises errors. }
    property OnAuthenticateError: TBrookHTTPAuthenticateErrorEvent
      read FOnAuthenticateError write FOnAuthenticateError;
    { Event triggered when a client requests a content. }
    property OnRequest: TBrookHTTPRequestEvent read FOnRequest write FOnRequest;
    { Event triggered when a client request raises errors. }
    property OnRequestError: TBrookHTTPRequestErrorEvent
      read FOnRequestError write FOnRequestError;
    { Event triggered when a client connects to or disconnects from the server. }
    property OnClientConnection: TBrookHTTPServerClientConnectionEvent
      read FOnClientConnection write FOnClientConnection;
    { Event triggered when the HTTP server raises errors. }
    property OnError: TBrookErrorEvent read FOnError write FOnError;
    { Event triggered when the HTTP server starts successfully. }
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    { Event triggered when the HTTP server stops successfully. }
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation

{ TBrookHTTPServerSecurity }

procedure TBrookHTTPServerSecurity.Assign(ASource: TPersistent);
var
  VSource: TBrookHTTPServerSecurity;
begin
  if ASource is TBrookHTTPServerSecurity then
  begin
    VSource := ASource as TBrookHTTPServerSecurity;
    FPrivateKey := VSource.PrivateKey;
    FPrivatePassword := VSource.PrivatePassword;
    FCertificate := VSource.Certificate;
    FTrust := VSource.Trust;
    FDHParams := VSource.DHParams;
    FPriorities := VSource.Priorities;
  end
  else
    inherited Assign(ASource);
end;

function TBrookHTTPServerSecurity.IsActiveStored: boolean;
begin
  Result := FActive;
end;

procedure TBrookHTTPServerSecurity.Validate;
begin
  if FPrivateKey.IsEmpty then
    raise EBrookHTTPServerSecurity.Create(SBrookEmptyPrivateKey);
  if FCertificate.IsEmpty then
    raise EBrookHTTPServerSecurity.Create(SBrookEmptyCertificate);
end;

procedure TBrookHTTPServerSecurity.Clear;
begin
  FActive := False;
  FPrivateKey := '';
  FPrivatePassword := '';
  FCertificate := '';
  FTrust := '';
  FDHParams := '';
  FPriorities := '';
end;

{ TBrookHTTPServer }

constructor TBrookHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocker := CreateLocker;
  FSecurity := CreateSecurity;
  SgLib.UnloadEvents.Add(InternalLibUnloadEvent, Self);
  FPostBufferSize := BROOK_POST_BUFFER_SIZE;
  FPayloadLimit := BROOK_PAYLOAD_LIMIT;
  FUploadsLimit := BROOK_UPLOADS_LIMIT;
end;

destructor TBrookHTTPServer.Destroy;
begin
  try
    SetActive(False);
  finally
    FSecurity.Free;
    SgLib.UnloadEvents.Remove(InternalLibUnloadEvent);
    FLocker.Free;
    inherited Destroy;
  end;
end;

procedure TBrookHTTPServer.Lock;
begin
  FLocker.Lock;
end;

procedure TBrookHTTPServer.Unlock;
begin
  FLocker.Unlock;
end;

procedure TBrookHTTPServer.InternalCreateServerHandle;
var
  VACb: sg_httpauth_cb;
begin
  if FAuthenticated then
    VACb := DoAuthenticationCallback
  else
    VACb := nil;
  FHandle := sg_httpsrv_new2(VACb, DoRequestCallback, DoErrorCallback, Self);
  if not Assigned(FHandle) then
    raise EInvalidPointer.Create(SBrookCannotCreateServerHandle);
end;

procedure TBrookHTTPServer.InternalFreeServerHandle;
begin
  sg_httpsrv_free(FHandle);
  FHandle := nil;
end;

procedure TBrookHTTPServer.InternalShutdownServer;
begin
  sg_httpsrv_shutdown(FHandle);
end;

procedure TBrookHTTPServer.InternalCheckServerOption(Aret: cint);
begin
  if Aret <> 0 then
  begin
    InternalFreeServerHandle;
    SgLib.CheckLastError(Aret);
  end;
end;

function TBrookHTTPServer.CreateLocker: TBrookLocker;
begin
  Result := TBrookLocker.Create;
end;

function TBrookHTTPServer.CreateAuthentication(AHandle: Pointer):
TBrookHTTPAuthentication;
begin
  Result := TBrookHTTPAuthentication.Create(AHandle);
end;

function TBrookHTTPServer.CreateSecurity: TBrookHTTPServerSecurity;
begin
  Result := TBrookHTTPServerSecurity.Create;
end;

function TBrookHTTPServer.CreateRequest(AHandle: Pointer): TBrookHTTPRequest;
begin
  Result := TBrookHTTPRequest.Create(AHandle);
end;

function TBrookHTTPServer.CreateResponse(AHandle: Pointer): TBrookHTTPResponse;
begin
  Result := TBrookHTTPResponse.Create(AHandle);
end;

function TBrookHTTPServer.CreateError(const AMessage: string): Exception;
begin
  Result := EBrookHTTPServer.Create(AMessage);
end;

class function TBrookHTTPServer.DoAuthenticationCallback(Acls: Pcvoid;
  Aauth: Psg_httpauth; Areq: Psg_httpreq; Ares: Psg_httpres): cbool; cdecl;
var
  VSrv: TBrookHTTPServer;
  VAuth: TBrookHTTPAuthentication;
  VReq: TBrookHTTPRequest;
  VRes: TBrookHTTPResponse;
begin
  VSrv := Acls;
  VReq := VSrv.CreateRequest(Areq);
  VRes := VSrv.CreateResponse(Ares);
  try
    if VSrv.FNoFavicon and VReq.IsFavicon then
      Exit(True);
    VAuth := VSrv.CreateAuthentication(Aauth);
    try
      VSrv.Lock;
      try
        Result := VSrv.HandleAuthenticate(VAuth, VReq, VRes);
      finally
        VSrv.Unlock;
      end;
    finally
      VAuth.Free;
    end;
  finally
    VRes.Free;
    VReq.Free;
  end;
end;

class procedure TBrookHTTPServer.DoRequestCallback(Acls: Pcvoid;
  Areq: Psg_httpreq; Ares: Psg_httpres); cdecl;
var
  VSrv: TBrookHTTPServer;
  VReq: TBrookHTTPRequest;
  VRes: TBrookHTTPResponse;
begin
  VSrv := Acls;
  VReq := VSrv.CreateRequest(Areq);
  VRes := VSrv.CreateResponse(Ares);
  try
    if VSrv.FNoFavicon and VReq.IsFavicon then
      VRes.SendEmpty
    else
    begin
      VSrv.Lock;
      try
        VSrv.HandleRequest(VReq, VRes);
      finally
        VSrv.Unlock;
      end;
      if VRes.IsEmpty and (not VReq.IsIsolated) then
        VRes.SendEmpty;
    end;
  finally
    VRes.Free;
    VReq.Free;
  end;
end;

class procedure TBrookHTTPServer.DoClientConnectionCallback(Acls: Pcvoid;
  const Aclient: Pcvoid; Aclosed: Pcbool); cdecl;
var
  VSrv: TBrookHTTPServer;
begin
  VSrv := Acls;
  VSrv.Lock;
  try
    VSrv.HandleClientConnection(VSrv, Aclient, PBoolean(Aclosed)^);
  finally
    VSrv.Unlock;
  end;
end;

class procedure TBrookHTTPServer.DoErrorCallback(Acls: Pcvoid;
  const Aerr: Pcchar); cdecl;
var
  VSrv: TBrookHTTPServer;
  VExcept: Exception;
begin
  VSrv := Acls;
  VExcept := VSrv.CreateError(TMarshal.ToString(Aerr));
  try
    VSrv.Lock;
    try
      VSrv.HandleError(VSrv, VExcept);
    finally
      VSrv.Unlock;
    end;
  finally
    VExcept.Free;
  end;
end;

procedure TBrookHTTPServer.CheckInactive;
begin
  if (not (csLoading in ComponentState)) and Active then
    raise EInvalidOpException.Create(SBrookActiveServer);
end;

procedure TBrookHTTPServer.InternalLibUnloadEvent(ASender: TObject);
begin
  if Assigned(ASender) then
    TBrookHTTPServer(ASender).Close;
end;

procedure TBrookHTTPServer.HandleAuthenticateError(
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; AException: Exception);
begin
  AResponse.Reset;
  try
    DoAuthenticateError(Self, AAuthentication, ARequest, AResponse, AException);
  except
    on E: Exception do
      AResponse.Send(E.Message, BROOK_CT_TEXT_PLAIN, 500);
  end;
end;

function TBrookHTTPServer.HandleAuthenticate(AAuthentication: TBrookHTTPAuthentication;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): boolean;
begin
  try
    Result := DoAuthenticate(Self, AAuthentication, ARequest, AResponse);
  except
    on E: Exception do
    begin
      Result := False;
      HandleAuthenticateError(AAuthentication, ARequest, AResponse, E);
    end;
  end;
end;

procedure TBrookHTTPServer.HandleRequestError(ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; AException: Exception);
begin
  AResponse.Reset;
  try
    DoRequestError(Self, ARequest, AResponse, AException);
  except
    on E: Exception do
      AResponse.Send(E.Message, BROOK_CT_TEXT_PLAIN, 500);
  end;
end;

procedure TBrookHTTPServer.HandleRequest(ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  try
    DoRequest(Self, ARequest, AResponse);
  except
    on E: Exception do
      HandleRequestError(ARequest, AResponse, E);
  end;
end;

procedure TBrookHTTPServer.HandleClientConnection(ASender: TObject;
  const AClient: Pointer; var AClosed: boolean);
begin
  DoClientConnection(ASender, AClient, AClosed);
end;

procedure TBrookHTTPServer.HandleError(ASender: TObject; AException: Exception);
begin
  DoError(ASender, AException);
end;

procedure TBrookHTTPServer.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedAuthenticated then
      SetAuthenticated(True);
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr);
    end
    else
      raise;
  end;
end;

function TBrookHTTPServer.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TBrookHTTPServer.GetMHDHandle: Pointer;
begin
  SgLib.Check;
  Result := sg_httpsrv_handle(FHandle);
end;

procedure TBrookHTTPServer.DoError(ASender: TObject; AException: Exception);
begin
  if Assigned(FOnError) then
    FOnError(ASender, AException)
  else
  if Assigned(ApplicationShowException) then
    ApplicationShowException(AException)
  else if Assigned(ApplicationHandleException) then
    ApplicationHandleException(AException)
  else
    ShowException(AException, Pointer(AException));
end;

function TBrookHTTPServer.DoAuthenticate(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse): boolean;
begin
  Result := Assigned(FOnAuthenticate) and FOnAuthenticate(ASender,
    AAuthentication, ARequest, AResponse);
end;

procedure TBrookHTTPServer.DoAuthenticateError(ASender: TObject;
  AAuthentication: TBrookHTTPAuthentication; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse; AException: Exception);
begin
  if Assigned(FOnAuthenticateError) then
    FOnAuthenticateError(ASender, AAuthentication, ARequest, AResponse,
      AException)
  else
    HandleRequestError(ARequest, AResponse, AException);
end;

procedure TBrookHTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  if Assigned(FOnRequest) then
    FOnRequest(ASender, ARequest, AResponse)
  else
    AResponse.SendEmpty;
end;

procedure TBrookHTTPServer.DoRequestError(ASender: TObject;
  ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse; AException: Exception);
begin
  if Assigned(FOnRequestError) then
    FOnRequestError(ASender, ARequest, AResponse, AException)
  else
    AResponse.Send(AException.Message, BROOK_CT_TEXT_PLAIN, 500);
end;

procedure TBrookHTTPServer.DoClientConnection(ASender: TObject;
  const AClient: Pointer; var AClosed: boolean);
begin
  if Assigned(FOnClientConnection) then
    FOnClientConnection(ASender, AClient, AClosed);
end;

procedure TBrookHTTPServer.SetPort(AValue: uint16);
begin
  if not FStreamedActive then
    CheckInactive;
  FPort := AValue;
end;

procedure TBrookHTTPServer.SetPostBufferSize(AValue: nativeuint);
begin
  if not FStreamedActive then
    CheckInactive;
  FPostBufferSize := AValue;
end;

procedure TBrookHTTPServer.SetConnectionLimit(AValue: cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FConnectionLimit := AValue;
end;

procedure TBrookHTTPServer.SetConnectionTimeout(AValue: cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FConnectionTimeout := AValue;
end;

procedure TBrookHTTPServer.SetPayloadLimit(AValue: nativeuint);
begin
  if not FStreamedActive then
    CheckInactive;
  FPayloadLimit := AValue;
end;

procedure TBrookHTTPServer.SetLocker(AValue: TBrookLocker);
begin
  if FLocker = AValue then
    Exit;
  if Assigned(AValue) then
    FLocker.Active := AValue.Active
  else
    FLocker.Active := False;
end;

procedure TBrookHTTPServer.SetSecurity(AValue: TBrookHTTPServerSecurity);
begin
  if FSecurity = AValue then
    Exit;
  if Assigned(AValue) then
    FSecurity.Assign(AValue)
  else
    FSecurity.Clear;
end;

procedure TBrookHTTPServer.SetUploadsLimit(AValue: uint64);
begin
  if not FStreamedActive then
    CheckInactive;
  FUploadsLimit := AValue;
end;

procedure TBrookHTTPServer.SetThreaded(AValue: boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  FThreaded := AValue;
  if FThreaded then
  begin
    System.IsMultiThread := True;
    FLocker.Active := False;
  end;
end;

procedure TBrookHTTPServer.SetThreadPoolSize(AValue: cardinal);
begin
  if not FStreamedActive then
    CheckInactive;
  FThreadPoolSize := AValue;
  if FThreadPoolSize > 0 then
    System.IsMultiThread := True;
end;

procedure TBrookHTTPServer.SetUploadsDir(const AValue: string);
begin
  if not FStreamedActive then
    CheckInactive;
  FUploadsDir := AValue;
end;

function TBrookHTTPServer.IsConnectionLimitStored: boolean;
begin
  Result := FConnectionLimit > 0;
end;

function TBrookHTTPServer.IsConnectionTimeoutStored: boolean;
begin
  Result := FConnectionTimeout > 0;
end;

function TBrookHTTPServer.IsNoFaviconStored: boolean;
begin
  Result := FNoFavicon;
end;

function TBrookHTTPServer.IsPayloadLimitStored: boolean;
begin
  Result := FPayloadLimit <> BROOK_PAYLOAD_LIMIT;
end;

function TBrookHTTPServer.IsUploadsLimitStored: boolean;
begin
  Result := FUploadsLimit <> BROOK_UPLOADS_LIMIT;
end;

function TBrookHTTPServer.IsActiveStored: boolean;
begin
  Result := FActive;
end;

function TBrookHTTPServer.GetPort: uint16;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FPort := sg_httpsrv_port(FHandle);
  end;
  Result := FPort;
end;

function TBrookHTTPServer.GetThreaded: boolean;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FThreaded := sg_httpsrv_is_threaded(FHandle);
  end;
  Result := FThreaded;
end;

function TBrookHTTPServer.GetUploadsDir: string;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FUploadsDir := TMarshal.ToString(sg_httpsrv_upld_dir(FHandle));
  end;
  Result := FUploadsDir;
end;

function TBrookHTTPServer.GetPostBufferSize: nativeuint;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FPostBufferSize := sg_httpsrv_post_buf_size(FHandle);
  end;
  Result := FPostBufferSize;
end;

function TBrookHTTPServer.GetPayloadLimit: nativeuint;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FPayloadLimit := sg_httpsrv_payld_limit(FHandle);
  end;
  Result := FPayloadLimit;
end;

function TBrookHTTPServer.GetUploadsLimit: uint64;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FUploadsLimit := sg_httpsrv_uplds_limit(FHandle);
  end;
  Result := FUploadsLimit;
end;

function TBrookHTTPServer.GetThreadPoolSize: cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FThreadPoolSize := sg_httpsrv_thr_pool_size(FHandle);
  end;
  Result := FThreadPoolSize;
end;

function TBrookHTTPServer.GetConnectionTimeout: cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FConnectionTimeout := sg_httpsrv_con_timeout(FHandle);
  end;
  Result := FConnectionTimeout;
end;

function TBrookHTTPServer.GetConnectionLimit: cardinal;
begin
  if FActive and not (csDesigning in ComponentState) then
  begin
    SgLib.Check;
    FConnectionLimit := sg_httpsrv_con_limit(FHandle);
  end;
  Result := FConnectionLimit;
end;

function TBrookHTTPServer.IsAuthenticatedStored: boolean;
begin
  Result := FAuthenticated;
end;

function TBrookHTTPServer.IsPortStored: boolean;
begin
  Result := FPort <> 0;
end;

function TBrookHTTPServer.IsPostBufferSizeStored: boolean;
begin
  Result := FPostBufferSize <> BROOK_POST_BUFFER_SIZE;
end;

function TBrookHTTPServer.IsThreadedStored: boolean;
begin
  Result := FThreaded;
end;

function TBrookHTTPServer.IsThreadPoolSizeStored: boolean;
begin
  Result := FThreadPoolSize > 0;
end;

function TBrookHTTPServer.IsUploadsDirStored: boolean;
begin
  Result := not FUploadsDir.IsEmpty;
end;

procedure TBrookHTTPServer.SetAuthenticated(AValue: boolean);
begin
  if not FStreamedActive then
    CheckInactive;
  if AValue = FAuthenticated then
    Exit;
  if AValue and (csReading in ComponentState) then
    FStreamedAuthenticated := True;
  FAuthenticated := AValue;
end;

procedure TBrookHTTPServer.SetActive(AValue: boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
      SgLib.Check;
    FActive := AValue;
  end
  else
  if AValue then
  begin
    if csReading in ComponentState then
      FStreamedActive := True
    else
      DoOpen;
  end
  else
    DoClose;
end;

procedure TBrookHTTPServer.DoOpen;
var
  M: TMarshaller;
begin
  if Assigned(FHandle) then
    Exit;
  SgLib.Check;
  InternalCreateServerHandle;
  if not FUploadsDir.IsEmpty then
    InternalCheckServerOption(sg_httpsrv_set_upld_dir(FHandle,
      M.ToCString(FUploadsDir)));
  if FPostBufferSize > 0 then
    InternalCheckServerOption(sg_httpsrv_set_post_buf_size(FHandle,
      FPostBufferSize));
  if FPayloadLimit > 0 then
    InternalCheckServerOption(sg_httpsrv_set_payld_limit(FHandle, FPayloadLimit));
  if FUploadsLimit > 0 then
    InternalCheckServerOption(sg_httpsrv_set_uplds_limit(FHandle, FUploadsLimit));
  if FThreadPoolSize > 0 then
    InternalCheckServerOption(sg_httpsrv_set_thr_pool_size(FHandle,
      FThreadPoolSize));
  if FConnectionTimeout > 0 then
    InternalCheckServerOption(sg_httpsrv_set_con_timeout(FHandle,
      FConnectionTimeout));
  if FConnectionLimit > 0 then
    InternalCheckServerOption(sg_httpsrv_set_con_limit(FHandle, FConnectionLimit));
  InternalCheckServerOption(sg_httpsrv_set_cli_cb(FHandle,
    DoClientConnectionCallback, Self));
  if FSecurity.Active then
  begin
    FSecurity.Validate;
    if not Assigned(sg_httpsrv_tls_listen3) then
      raise ENotSupportedException.Create(SBrookTLSNotAvailable);
    FActive := sg_httpsrv_tls_listen3(FHandle,
      M.ToCNullableString(FSecurity.PrivateKey),
      M.ToCNullableString(FSecurity.PrivatePassword),
      M.ToCNullableString(FSecurity.Certificate),
      M.ToCNullableString(FSecurity.Trust), M.ToCNullableString(FSecurity.DHParams),
      M.ToCNullableString(FSecurity.Priorities), FPort, FThreaded);
  end
  else
    FActive := sg_httpsrv_listen(FHandle, FPort, FThreaded);
  if not FActive then
    InternalFreeServerHandle
  else
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TBrookHTTPServer.DoClose;
begin
  if not Assigned(FHandle) then
    Exit;
  SgLib.Check;
  InternalShutdownServer;
  InternalFreeServerHandle;
  FActive := Assigned(FHandle);
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

procedure TBrookHTTPServer.Open;
begin
  SetActive(True);
end;

procedure TBrookHTTPServer.Close;
begin
  SetActive(False);
end;

end.
