
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CRSSPI
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRSspi;

interface

{$IFDEF MSWINDOWS}
uses
  Classes, SysUtils, Windows,
  CRTypes, CLRClasses;

type
  TSecurityProvider = (spNegotiate, spKerberos, spNTLM);
  TSecurityProviders = set of TSecurityProvider;

  TSecHandle = record
    dwLower: NativeUInt;
    dwUpper: NativeUInt;
  end;
  PSecHandle = ^TSecHandle;

  TSecBuffer = record
    cbBuffer: Cardinal;   // Size of the buffer, in bytes
    BufferType: Cardinal; // Type of the buffer (below)
    pvBuffer: Pointer;    // Pointer to the buffer
  end;
  PSecBuffer = ^TSecBuffer;

  TSecBufferDesc = record
    ulVersion: Cardinal;  // Version number
    cBuffers: Cardinal;   // Number of buffers
    pBuffers: PSecBuffer; // Pointer to array of buffers
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  TSecBufferArray = array of TSecBuffer;

  SecPkgInfo = record
    fCapabilities: Cardinal; // Capability bitmask
    wVersion: Word; // Version of driver
    wRPCID: Word; // ID for RPC Runtime
    cbMaxToken: Cardinal; // Size of authentication token (max)
    Name: PChar; // Text name
    Comment: PChar; // Comment
  end;
  PSecPkgInfo = ^SecPkgInfo;

  TQuerySecurityPackageInfo = function (pszPackageName: PChar; var ppPackageInfo: PSecPkgInfo): Integer; stdcall;

  TAcquireCredentialsHandle = function (pszPrincipal, pszPackage: PChar;
    fCredentialUse: Cardinal; pvLogonId, pAuthData: Pointer;
    pGetKeyFn: Pointer; pvGetKeyArgument: Pointer; phCredential: PSecHandle;
    var ptsExpiry: TTimeStamp): Integer; stdcall;

  TInitializeSecurityContext = function (phCredential: PSecHandle; phContext: PSecHandle;
    pszTargetName: PChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
    pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PSecHandle;
    pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; ptsExpiry: pointer): Integer; stdcall;

  TDeleteSecurityContext = function (phContext: PSecHandle): Integer; stdcall;

  TFreeCredentialsHandle = function (phCredential: PSecHandle): Integer; stdcall;

  TFreeContextBuffer = function (pvContextBuffer: Pointer): Integer; stdcall;

  TGetUserNameEx = function(const NameFormat: Byte; lpNameBuffer: PChar; var lpnSize: Cardinal): boolean; stdcall;

  TCRNTLMAuth = class
  private
    FUsedPackage: TSecurityProvider;
    FMaxMessageLen: Cardinal;
    FCredNegotiate: TSecHandle;
    FSslCtxHandle: TSecHandle;

    FQuerySecurityPackageInfo: TQuerySecurityPackageInfo;
    FAcquireCredentialsHandle: TAcquireCredentialsHandle;
    FFreeContextBuffer: TFreeContextBuffer;
    FInitializeSecurityContext: TInitializeSecurityContext;
    FFreeCredentialsHandle: TFreeCredentialsHandle;
    FDeleteSecurityContext: TDeleteSecurityContext;
    FGetUserNameEx: TGetUserNameEx;
  private
    function GetUsedProtocolName: string;
  protected
    procedure Release;
    function RegisterSecureDllMethods(): Integer; virtual;
    function SecurityProtocolName(Protocol: TSecurityProvider): string;

    property UsedPackage: TSecurityProvider read FUsedPackage write FUsedPackage;
    property MaxMessageLen: Cardinal read FMaxMessageLen write FMaxMessageLen;
    property CredNegotiate: TSecHandle read FCredNegotiate write FCredNegotiate;
    property SslCtxHandle: TSecHandle read FSslCtxHandle write FSslCtxHandle;
    property QuerySecurityPackageInfo: TQuerySecurityPackageInfo read FQuerySecurityPackageInfo write FQuerySecurityPackageInfo;
    property AcquireCredentialsHandle: TAcquireCredentialsHandle read FAcquireCredentialsHandle write FAcquireCredentialsHandle;
    property FreeContextBuffer: TFreeContextBuffer read FFreeContextBuffer write FFreeContextBuffer;
    property InitializeSecurityContext: TInitializeSecurityContext read FInitializeSecurityContext write FInitializeSecurityContext;
    property FreeCredentialsHandle: TFreeCredentialsHandle read FFreeCredentialsHandle write FFreeCredentialsHandle;
    property DeleteSecurityContext: TDeleteSecurityContext read FDeleteSecurityContext write FDeleteSecurityContext;
    property GetUserNameEx: TGetUserNameEx read FGetUserNameEx write FGetUserNameEx;
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize: Integer; virtual;
    function StartAuthentication(out AuthData: TBytes): Integer;
    function FinishAuthentication(const InAuthData: TBytes; out OutAuthData: TBytes): Integer;
    function GetUserName(const NameFormat: Byte; lpNameBuffer: PChar; var lpnSize: Cardinal): Boolean;
  public
    property UsedProtocolName: string read GetUsedProtocolName;
  end;

var
  hSecure32Lib: HMODULE;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

{ TCRNTLMAuth }

constructor TCRNTLMAuth.Create;
begin
  inherited Create;

  FUsedPackage := spNTLM;

  if hSecure32Lib = 0 then
    hSecure32Lib := LoadLibraryEx('secur32.dll', 0, LOAD_WITH_ALTERED_SEARCH_PATH);
end;

destructor TCRNTLMAuth.Destroy;
begin
  Release;

  inherited;
end;

function TCRNTLMAuth.GetUsedProtocolName: string;
begin
  Result := SecurityProtocolName(FUsedPackage);
end;

procedure TCRNTLMAuth.Release;
begin
  if hSecure32Lib = 0 then
    Exit;

  if Assigned(DeleteSecurityContext) then
    DeleteSecurityContext(@FSslCtxHandle);

  if Assigned(FreeCredentialsHandle) then
    FreeCredentialsHandle(@FCredNegotiate);
end;

function TCRNTLMAuth.RegisterSecureDllMethods: Integer;
begin
  Result := 0;

  if hSecure32Lib = 0 then begin
    Result := GetLastError;
    Exit;
  end;

  FQuerySecurityPackageInfo := GetProcAddress(hSecure32Lib, {$IFDEF UNICODE}'QuerySecurityPackageInfoW'{$ELSE}'QuerySecurityPackageInfoA'{$ENDIF});
  if not Assigned(FQuerySecurityPackageInfo) then begin
    Result := GetLastError;
    Exit;
  end;

  FAcquireCredentialsHandle := GetProcAddress(hSecure32Lib, {$IFDEF UNICODE}'AcquireCredentialsHandleW'{$ELSE}'AcquireCredentialsHandleA'{$ENDIF});
  if not Assigned(AcquireCredentialsHandle) then begin
    Result := GetLastError;
    Exit;
  end;

  FFreeContextBuffer := GetProcAddress(hSecure32Lib, 'FreeContextBuffer');
  if not Assigned(FreeContextBuffer) then begin
    Result := GetLastError;
    Exit;
  end;

  FInitializeSecurityContext := GetProcAddress(hSecure32Lib, {$IFDEF UNICODE}'InitializeSecurityContextW'{$ELSE}'InitializeSecurityContextA'{$ENDIF});
  if not Assigned(InitializeSecurityContext) then begin
    Result := GetLastError;
    Exit;
  end;

  FFreeCredentialsHandle := GetProcAddress(hSecure32Lib, 'FreeCredentialsHandle');
  if not Assigned(FreeCredentialsHandle) then begin
    Result := GetLastError;
    Exit;
  end;

  FDeleteSecurityContext := GetProcAddress(hSecure32Lib, 'DeleteSecurityContext');
  if not Assigned(DeleteSecurityContext) then begin
    Result := GetLastError;
    Exit;
  end;

  FGetUserNameEx := GetProcAddress(hSecure32Lib, {$IFDEF UNICODE}'GetUserNameExW'{$ELSE}'GetUserNameExA'{$ENDIF});
  if not Assigned(GetUserNameEx) then begin
    Result := GetLastError;
    Exit;
  end;
end;

function TCRNTLMAuth.SecurityProtocolName(Protocol: TSecurityProvider): string;
begin
  case Protocol of
    spNegotiate: begin
      Result := 'negotiate';
    end;
    spKerberos: begin
      Result := 'kerberos';
    end;
    spNTLM: begin
      Result := 'ntlm';
    end;
  end;
end;

function TCRNTLMAuth.Initialize: Integer;
var
  pkgInfo: PSecPkgInfo;
  tsExpiry: TTimeStamp;
  upn: string;
begin
  Result := RegisterSecureDllMethods();
  if Result <> 0 then
    Exit;

  upn := UsedProtocolName;
  Result := QuerySecurityPackageInfo(PChar(upn), pkgInfo);
  if Result <> 0 then
    Exit;

  FMaxMessageLen := pkgInfo.cbMaxToken;
  FreeContextBuffer(pkgInfo);

  Result := AcquireCredentialsHandle(nil, PChar(upn), $00000003{SECPKG_CRED_BOTH},
    nil, nil, nil, nil, @FCredNegotiate, tsExpiry);
end;

function TCRNTLMAuth.StartAuthentication(out AuthData: TBytes): Integer;
var
  tsExpiry: TTimeStamp;
  InBufferDesc: TSecBufferDesc;
  InSecBuffer: TSecBuffer;
  attrs: Cardinal;
begin
  SetLength(AuthData, FMaxMessageLen);

  InSecBuffer.BufferType := 2{SECBUFFER_TOKEN};
  InSecBuffer.pvBuffer := @AuthData[0];
  InSecBuffer.cbBuffer := Length(AuthData);

  InBufferDesc.ulVersion := 0{SECBUFFER_VERSION};
  InBufferDesc.cBuffers := 1;
  InBufferDesc.pBuffers := @InSecBuffer;

  Result := InitializeSecurityContext(@FCredNegotiate, nil, nil,
    $00000800{ISC_REQ_CONNECTION}, 0, $00000010{SECURITY_NATIVE_DREP},
    nil, 0, @FSslCtxHandle, @InBufferDesc, attrs, @tsExpiry);

  if Result <> $00090312{SEC_I_CONTINUE_NEEDED} then
    Exit;

  SetLength(AuthData, InSecBuffer.cbBuffer);

  Result := 0;
end;

function TCRNTLMAuth.FinishAuthentication(const InAuthData: TBytes; out OutAuthData: TBytes): Integer;
var
  InBufferDesc: TSecBufferDesc;
  InSecBufferArr: TSecBufferArray;
  OutBufferDesc: TSecBufferDesc;
  OutSecBuffer: TSecBuffer;
  attrs: Cardinal;
  tsExpiry: TTimeStamp;
begin
  SetLength(InSecBufferArr, 2);

  InSecBufferArr[0].BufferType := 2{SECBUFFER_TOKEN};
  InSecBufferArr[0].pvBuffer := @InAuthData[0];
  InSecBufferArr[0].cbBuffer := Length(InAuthData);

  InSecBufferArr[1].BufferType := 0{SECBUFFER_EMPTY};
  InSecBufferArr[1].pvBuffer := nil;
  InSecBufferArr[1].cbBuffer := 0;

  InBufferDesc.ulVersion := 0{SECBUFFER_VERSION};
  InBufferDesc.cBuffers := 2;
  InBufferDesc.pBuffers := @InSecBufferArr[0];

  SetLength(OutAuthData, FMaxMessageLen);

  OutSecBuffer.BufferType := 2{SECBUFFER_TOKEN};
  OutSecBuffer.pvBuffer := @OutAuthData[0];
  OutSecBuffer.cbBuffer := Length(OutAuthData);

  OutBufferDesc.ulVersion := 0{SECBUFFER_VERSION};
  OutBufferDesc.cBuffers := 1;
  OutBufferDesc.pBuffers := @OutSecBuffer;

  Result := InitializeSecurityContext(@FCredNegotiate, @FSslCtxHandle, nil,
    $00000800{ISC_REQ_CONNECTION}, 0, $00000010{SECURITY_NATIVE_DREP},
    @InBufferDesc, 0, nil, @OutBufferDesc, attrs, @tsExpiry);

  if Result <> 0 then
    Exit;

  SetLength(OutAuthData, OutSecBuffer.cbBuffer);
end;

function TCRNTLMAuth.GetUserName(const NameFormat: Byte; lpNameBuffer: PChar; var lpnSize: Cardinal): Boolean;
begin
  Result := GetUserNameEx(NameFormat, lpNameBuffer, lpnSize);
end;

initialization
  hSecure32Lib := 0;

finalization
  if hSecure32Lib <> 0 then begin
    FreeLibrary(hSecure32Lib);
    hSecure32Lib := 0;
  end;

{$ENDIF}

end.
