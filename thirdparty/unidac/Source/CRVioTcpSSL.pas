
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRVioTcpSSL;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SyncObjs,
  CLRClasses, CRVio,
  CRTypes, CRVioTcp;

const
  SSL_VERIFY_NONE      = 0;
  SSL_CTRL_SET_TMP_DH  = 3;

  SSL_ERROR_WANT_READ  = 2;
  SSL_ERROR_WANT_WRITE = 3;

type
  st_VioSSLConnectorFd = packed record
    ssl_context_: IntPtr;
    ssl_method_: IntPtr;
  end;

  TCRVioTcpSSL = class(TCRVioTcp)
  protected
    FisSecure: boolean;
    FSSL_key, FSSL_cert, FSSL_ca, FSSL_capath, FSSL_cipher: string;

    Fnewcon: st_VioSSLConnectorFd;
    Fssl_arg: IntPtr;

    procedure new_VioSSLConnectorFd;
    procedure SetisSecure(Value: boolean);

  public
    constructor Create(const hostname: string; const port: integer;
      const SSL_key, SSL_cert, SSL_ca, SSL_capath, SSL_cipher: string; IPVersion: TIPVersion = ivIPv4);
    destructor Destroy; override;
    function ReadNoWait(const buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const buffer: TValueArr; offset, count: integer): integer; override;

    property isSecure: boolean read FisSecure write SetisSecure;
  end;

var
  OpenSSL11_Mode : Boolean;
  SSL_write: function(s: IntPtr; pBuf: IntPtr; len: integer): integer; cdecl;
  SSL_read: function(s: IntPtr; pBuf: IntPtr; len: integer): integer; cdecl;
  SSL_get_error: function(const s: IntPtr; ret_code: integer): integer; cdecl;
  //SSL_shutdown: function(s: IntPtr): integer; cdecl;
  SSL_free: procedure(s: IntPtr); cdecl;

  SSL_load_error_strings: procedure; cdecl;
  TLS_method: function: IntPtr; cdecl;
  SSL_library_init: function: integer; cdecl;

  SSL_CTX_new: function(meth: IntPtr): IntPtr; cdecl;
  SSL_CTX_set_cipher_list: function(actx: IntPtr; const str: PAnsiChar): integer; cdecl;
  SSL_CTX_free: procedure(actx: IntPtr); cdecl;
  SSL_new: function(s: IntPtr): IntPtr; cdecl;
  SSL_clear: function(s: IntPtr): integer; cdecl;
  SSL_SESSION_set_timeout: function(s: IntPtr; t: cardinal): integer; cdecl;
  SSL_get_session: function(s: IntPtr): IntPtr; cdecl;
  SSL_set_fd: function(s: IntPtr; fd: integer): integer; cdecl;
  SSL_set_connect_state: procedure(s: IntPtr); cdecl;
  SSL_do_handshake: function(s: IntPtr): integer; cdecl;
  //SSL_get_peer_certificate: function(s: IntPtr): IntPtr; cdecl;
  //SSL_set_session: function(_to: IntPtr; session: IntPtr): integer; cdecl;
  //SSL_connect: function(s: IntPtr): integer; cdecl;
  //SSL_CIPHER_get_name: function(c: IntPtr): PAnsiChar; cdecl;
  //SSL_get_current_cipher: function(s: IntPtr): IntPtr; cdecl;
  SSL_CTX_set_verify: procedure(actx: IntPtr;mode: integer;acallback: IntPtr); cdecl;
  SSL_CTX_load_verify_locations: function(actx: IntPtr; const CAfile: PAnsiChar; const CApath: PAnsiChar): integer; cdecl;
  SSL_CTX_set_default_verify_paths: function(actx: IntPtr): integer; cdecl;
  SSL_CTX_use_certificate_file: function(actx: IntPtr; const afile: PAnsiChar; atype: integer): integer; cdecl;
  SSL_CTX_use_PrivateKey_file: function(actx: IntPtr; const afile: PAnsiChar; atype: integer): integer; cdecl;
  SSL_CTX_check_private_key: function(const actx: IntPtr): integer; cdecl;
  DH_new: function: IntPtr; cdecl;
  DH_free: function(dh: IntPtr): integer; cdecl;
  OPENSSL_add_all_algorithms_noconf: procedure; cdecl;
  BN_bin2bn: function(const s: IntPtr; len: integer;ret: IntPtr): IntPtr; cdecl;
  X509_get_subject_name: function(a: IntPtr): IntPtr; cdecl;
  X509_NAME_oneline: function(a: IntPtr; buf: PAnsiChar;size: integer): PAnsiChar; cdecl;
  X509_STORE_CTX_get_error_depth: function(actx: IntPtr): integer; cdecl;
  X509_STORE_CTX_get_error: function(actx: IntPtr): integer; cdecl;
  X509_STORE_CTX_get_current_cert: function(actx: IntPtr): IntPtr; cdecl;
  SSL_CTX_ctrl: function(actx: IntPtr; a1: integer; a2: integer; adh: IntPtr): integer; cdecl;
  //X509_verify_cert_error_string: function(n: integer): PAnsiChar; cdecl;
  X509_get_issuer_name: function(a: IntPtr): IntPtr; cdecl;
  //ERR_get_error_line_data: function(const afile: IntPtr; line: IntPtr; const data: IntPtr;flags: IntPtr): integer; cdecl;
  //ERR_error_string: function(e: cardinal; buf: PAnsiChar): PAnsiChar; cdecl;
  //X509_free: function(a: IntPtr); cdecl;

var
{$IFDEF MSWINDOWS}
  LIBEAY32DLL: string;
  SSLEAY32DLL: string;
{$ELSE}
  SSLLIB: string;
{$ENDIF}

var
  ssl_algorithms_added: boolean = False;
  ssl_error_strings_loaded: boolean = False;

function vio_verify_callback(ok: integer; ctx: IntPtr): integer; cdecl;
function vio_set_cert_stuff(ctx: IntPtr; const cert_file: string; key_file: string): integer;
function get_dh512: IntPtr;

procedure InitSSLLib(SilentMode: Boolean = False);
procedure LoadSSLLib(SilentMode: Boolean = False);

var
{$IFDEF MSWINDOWS}
  hlibeay, hssleay: HMODULE;
{$ELSE}
{$IFDEF UNIX}
  hssleay, hlibeay: pointer;
{$ENDIF}
{$IFDEF POSIX}
  hssleay, hlibeay: NativeUInt;
{$ENDIF}
{$ENDIF MSWINDOWS}

implementation

uses
{$IFDEF POSIX}
  Posix.Dlfcn,
{$ENDIF}
{$IFDEF VER12P}{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}{$ENDIF}
{$IFDEF UNIX}
  dl,
{$ENDIF}
  SysUtils;

const
  MaxIOAttempts = 10;

var
{$IFNDEF MSWINDOWS}
{$IFDEF UNIX}
  hlib: pointer;
{$ENDIF}
{$IFDEF POSIX}
  hlib: NativeUInt;
{$ENDIF}
{$ENDIF MSWINDOWS}
  SSLInitLock: TCriticalSection;

constructor TCRVioTcpSSL.Create(const hostname: string; const port: integer;
  const SSL_key, SSL_cert, SSL_ca, SSL_capath, SSL_cipher: string; IPVersion: TIPVersion);
begin
  inherited Create(hostname, port, IPVersion);
  FSSL_key := SSL_key;
  FSSL_cert := SSL_cert;
  FSSL_ca := SSL_ca;
  FSSL_capath := SSL_capath;
  FSSL_cipher := SSL_cipher;

  InitSSLLib;
end;

destructor TCRVioTcpSSL.Destroy;
begin
  if Fnewcon.ssl_context_ <> nil then begin
    SSL_CTX_free(Fnewcon.ssl_context_);
    Fnewcon.ssl_context_ := nil;
  end;

  if Fssl_arg <> nil then begin
    SSL_free(Fssl_arg);
    Fssl_arg := nil;
  end;

  inherited;
end;

procedure TCRVioTcpSSL.new_VioSSLConnectorFd;
{$IFNDEF ANDROID}
var
  // r: int;
  dh: IntPtr;
{$ENDIF}
begin
  Fnewcon.ssl_context_ := nil;
  Fnewcon.ssl_method_ := nil;

  if ((not OpenSSL11_Mode) and (SSL_library_init = 0)) then
    raise Exception.Create('SSL library initialization failed');

  if not ssl_algorithms_added then begin
    ssl_algorithms_added := True;
  {$IFNDEF ANDROID}
    if not OpenSSL11_Mode then
      OpenSSL_add_all_algorithms_noconf();
  {$ENDIF}
  end;
  if not ssl_error_strings_loaded then begin
    ssl_error_strings_loaded := TRUE;
    if not OpenSSL11_Mode then
      SSL_load_error_strings();
  end;

  Fnewcon.ssl_method_ := TLS_method();
  Fnewcon.ssl_context_ := SSL_CTX_new(Fnewcon.ssl_method_);

  Assert(Fnewcon.ssl_context_ <> nil);
  (*
    SSL_CTX_set_options
    SSL_CTX_set_info_callback
   *)
  if Trim(FSSL_cipher) <> '' then
    {r := }SSL_CTX_set_cipher_list(Fnewcon.ssl_context_, PAnsiChar(AnsiString(FSSL_cipher)));
{$IFNDEF ANDROID}
  SSL_CTX_set_verify(Fnewcon.ssl_context_, SSL_VERIFY_NONE, @vio_verify_callback);//???
{$ENDIF}
  Assert(vio_set_cert_stuff(Fnewcon.ssl_context_, FSSL_cert, FSSL_key) <> -1);

  if SSL_CTX_load_verify_locations(Fnewcon.ssl_context_, PAnsiChar(AnsiString(FSSL_ca)), PAnsiChar(AnsiString(FSSL_capath))) = 0 then
    Assert(SSL_CTX_set_default_verify_paths(Fnewcon.ssl_context_) <> 0);

{$IFNDEF ANDROID}
  (* DH stuff *)
  dh := get_dh512();
  Assert(dh <> nil);
  try
    // SSL_CTX_set_tmp_dh(Fnewcon.ssl_context_, dh);
    // #define SSL_CTX_set_tmp_dh(ctx,dh) \
    //   SSL_CTX_ctrl(ctx,SSL_CTRL_SET_TMP_DH,0,(char *)dh)
    SSL_CTX_ctrl(Fnewcon.ssl_context_, SSL_CTRL_SET_TMP_DH, 0, dh);
  finally
    DH_free(dh);
  end;
{$ENDIF}
end;

procedure TCRVioTcpSSL.SetisSecure(Value: boolean);
var
  r, r1, r2: integer;
begin
  Assert(Value = True);
  Assert(Fsd <> -1);
  new_VioSSLConnectorFd;

(*???  net_blocking := vio_is_blocking(vio);
  vio_blocking(vio, 1, &unused);	(* Must be called before reset * )
  vio_reset(vio,VIO_TYPE_SSL,vio->sd,0,FALSE);*)
  Fssl_arg := SSL_new(Fnewcon.ssl_context_);
  try
    Assert(Fssl_arg <> nil, 'SSL_new failure');

    SSL_clear(Fssl_arg);
    SSL_SESSION_set_timeout(SSL_get_session(Fssl_arg), timeout);
    SSL_set_fd(Fssl_arg, FSd);
    SSL_set_connect_state(Fssl_arg);
    r := SSL_do_handshake(Fssl_arg);
    r2 := r;
    r1 := SSL_get_error(Fssl_arg, r2);

    if r <> 1 then
      raise Exception.Create('SSL_do_handshake = ' + IntToStr(r) + #$D#$A + 'SSL_get_error(..., r2) = ' + IntToStr(r1) + #$D#$A + 'r2 = ' + IntToStr(r2));
    FisSecure := True;
  except
    // report_errors();
    SSL_free(Fssl_arg);
    Fssl_arg := nil;
    (*vio_reset(vio, old_type,vio.sd,0,FALSE);
    vio_blocking(vio, net_blocking, &unused);*)
    raise;
  end;
end;

function TCRVioTcpSSL.ReadNoWait(const buffer: TValueArr; offset, count: integer): integer;
var
  pbuffer: IntPtr;
  ErrorCode, Attempt: integer;
begin
  if not isSecure then begin
    Result := inherited ReadNoWait(buffer, offset, count);
    Exit;
  end;

  pbuffer := @buffer[offset];

  Attempt := 1;
  repeat
    Result := SSL_read(Fssl_arg, pbuffer, count);
    if Result < 0 then begin
      ErrorCode := SSL_get_error(Fssl_arg, Result);
      if ErrorCode <> SSL_ERROR_WANT_READ then
        Break;
    end;
    Inc(Attempt);
  until (Result >= 0) or (Attempt > MaxIOAttempts);

  if Result < 0 then
    Result := 0; // silent error handling
end;

function TCRVioTcpSSL.WriteNoWait(const buffer: TValueArr; offset, count: integer): integer;
var
  pbuffer: IntPtr;
  ErrorCode, Attempt: integer;
begin
  if not isSecure then begin
    Result := inherited WriteNoWait(buffer, offset, count);
    Exit;
  end;

  pbuffer := @buffer[offset];

  Attempt := 1;
  repeat
    Result := SSL_write(Fssl_arg, pbuffer, count);
    if Result < 0 then begin
      ErrorCode := SSL_get_error(Fssl_arg, Result);
      if ErrorCode <> SSL_ERROR_WANT_WRITE then
        Break;
    end;
    Inc(Attempt);
  until (Result >= 0) or (Attempt > MaxIOAttempts);

  if Result < 0 then
    Result := 0; // silent error handling
end;

function vio_verify_callback(ok: integer; ctx: IntPtr): integer; cdecl;
type
  dummyctx = record
    pad: array[0..75]of byte;
    error: integer;
    current_cert: IntPtr;
  end;

var
  buf: array[0..255] of AnsiChar;
  err_cert: IntPtr;
  depth, err: integer;
begin
  //get the details
  err_cert := X509_STORE_CTX_get_current_cert(ctx);
  err := X509_STORE_CTX_get_error(ctx);
  depth := X509_STORE_CTX_get_error_depth(ctx);
  X509_NAME_oneline(X509_get_subject_name(err_cert), PAnsiChar(@buf), 256);
  if not (boolean(ok)) then begin
    if depth <= 0 then
      ok := 0
    else
      ok := 1;
  end;
  //some more details about the error
  case err of
    2: //X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
    begin
      X509_NAME_oneline(X509_get_issuer_name(err_cert), PAnsiChar(@buf), 256);
      //showmessage('issuer= '+buf);
    end;
    9, 13: //X509_V_ERR_CERT_NOT_YET_VALID:
           //X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
      //DBUG_PRINT("error", ("notBefore"));
      //*ASN1_TIME_print_fp(stderr,X509_get_notBefore(ctx->current_cert));*/
    ;
    10, 14: //X509_V_ERR_CERT_HAS_EXPIRED:
            //X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
            //DBUG_PRINT("error", ("notAfter error"));
            //*ASN1_TIME_print_fp(stderr,X509_get_notAfter(ctx->current_cert));*/
    ;
  end;
  Result := ok;
end;

////////////////////////////////////////////////////////////////////////////////
// sets the cert stuff
function vio_set_cert_stuff(ctx: IntPtr; const cert_file: string; key_file: string): integer;
begin
  Result := 0;
  if cert_file <> '' then begin //is there anything to set?
    //use it
    if (SSL_CTX_use_certificate_file(ctx, PAnsiChar(AnsiString(cert_file)), 1{SSL_FILETYPE_PEM}) <= 0) then
      Exit;
    if key_file= '' then // do we have any key
      key_file := cert_file;
    //use it
    if SSL_CTX_use_PrivateKey_file(ctx, PAnsiChar(AnsiString(key_file)), 1{SSL_FILETYPE_PEM}) <= 0 then
      Exit;
    //let's check it
    //if SSL_CTX_check_private_key(Marshal.ReadIntPtr(ctx)) <> 0 then
    //  Exit;
    if SSL_CTX_check_private_key(ctx) <> 0 then
      Exit;
  end;
  Result := 1;//no errors
end;

////////////////////////////////////////////////////////////////////////////////
// gets a new dh
function get_dh512: IntPtr;
const
  dh512_g: array[1..1] of byte = ($02);
  dh512_p: array[1..64] of byte =(
    $DA, $58, $3C, $16, $D9, $85, $22, $89, $D0, $E4, $AF, $75,
    $6F, $4C, $CA, $92, $DD, $4B, $E5, $33, $B8, $04, $FB, $0F,
    $ED, $94, $EF, $9C, $8A, $44, $03, $ED, $57, $46, $50, $D3,
    $69, $99, $DB, $29, $D7, $76, $27, $6B, $A2, $D3, $D4, $12,
    $E2, $18, $F4, $DD, $1E, $08, $4C, $F6, $D8, $00, $3E, $7C,
    $47, $74, $E8, $33);

type
  dhdummy = packed record
    pad: integer;
    version: integer;
    p: IntPtr;
    g: IntPtr;
    //the others are skipped
  end;

var
  dh, p, g: IntPtr;
  dh512_gp, dh512_pp: IntPtr;

begin
  Result := nil;

  dh512_pp := @dh512_p;
  dh512_gp := @dh512_g;

  p := BN_bin2bn(dh512_pp, Length(dh512_p), nil); //set p
  g := BN_bin2bn(dh512_gp, Length(dh512_g), nil); //set g

  if (p = nil) or (g = nil) then //any errors?
    Exit;
  dh := DH_new; //grab a dh
  if dh = nil then
    Exit;

  dhdummy(dh^).p := p;
  dhdummy(dh^).g := g;
  Result := dh;
end;

procedure NotLink;
begin
  raise Exception.Create('SSL function is not linked. You should update SSL client library.');
end;

function LoadedSSLLib: boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (hlibeay > 0) and (hssleay > 0);
{$ENDIF}
{$IFDEF UNIX}
  Result := hlib <> nil;
{$ENDIF}
{$IFDEF POSIX}
  Result := hlib <> 0;
{$ENDIF}
end;

procedure LoadSSLLib;
  procedure AssignProc(
  var
  {$IFDEF MSWINDOWS}
    hlib: HMODULE;
  {$ENDIF}
  {$IFDEF UNIX}
    hlib: pointer;
  {$ENDIF}
  {$IFDEF POSIX}
    hlib: NativeUInt;
  {$ENDIF}
    var Proc: pointer; const Name: string);
  begin
  {$IFDEF MSWINDOWS}
    Proc := GetProcAddress(hlib, PChar(Name));
  {$ELSE}
    Proc := dlsym(hlib, PAnsiChar(AnsiString(Name)));
  {$ENDIF}
    if Proc = nil then
      Proc := @NotLink;
  end;
var
  Error: EOSError;
  Msg: string;
{$IFNDEF MSWINDOWS}
  s: string;
{$ELSE}
  SSLEAY32DLLIsEmpty: boolean;
{$ENDIF}
begin
  if LoadedSSLLib then
    Exit;

{$IFDEF MSWINDOWS}
// OpenSSL 1.1 naming
  SSLEAY32DLLIsEmpty := SSLEAY32DLL = '';
  if LIBEAY32DLL = '' then
    LIBEAY32DLL := 'libcrypto-1_1'+{$IFDEF WIN64}'-x64'+{$ENDIF}'.dll';
  if SSLEAY32DLLIsEmpty then
    SSLEAY32DLL := 'libssl-1_1'+{$IFDEF WIN64}'-x64'+{$ENDIF}'.dll';

  hlibeay := LoadLibraryEx(PChar(LIBEAY32DLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  hssleay := LoadLibraryEx(PChar(SSLEAY32DLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if (hssleay = 0) then begin
    if SSLEAY32DLLIsEmpty then begin
      // Fallback to legacy OpenSSL
      LIBEAY32DLL := 'libeay32.dll';
      SSLEAY32DLL := 'ssleay32.dll';
      hlibeay := LoadLibraryEx(PChar(LIBEAY32DLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
      hssleay := LoadLibraryEx(PChar(SSLEAY32DLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
    end;
    if hssleay = 0 then begin
      SSLEAY32DLL := 'libssl32.dll';
      hssleay := LoadLibraryEx(PChar(SSLEAY32DLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
    end;
  end
  else
    OpenSSL11_Mode := True;
{$ELSE}
  if SSLLIB = '' then
  {$IFDEF MACOS}
    SSLLIB := 'libssl.dylib';
  {$ELSE}
    SSLLIB := 'libssl.so';
  {$ENDIF}
  hlib := dlopen(PAnsiChar(AnsiString(SSLLIB)), RTLD_LAZY);
  hssleay := hlib;
  hlibeay := hlib;
{$ENDIF}

  if not LoadedSSLLib then begin
    if not SilentMode then begin
    {$IFDEF MSWINDOWS}
      Msg := 'OpenSSL client library couldn''t be loaded. Please place libeay32.dll and ssleay32.dll (or libssl32.dll) to system folder (included to PATH) or to the folder with executable unit of main program.';
    {$ELSE}
      Msg := 'OpenSSL client library couldn''t be loaded. Please place libssl.' + {$IFDEF MACOS}'dylib '{$ELSE}'so '{$ENDIF} + 'file to system folder (included to LD_LIBRARY_PATH) or to the folder with executable unit of main program.';
      s := string(dlerror);
      if s <> '' then
        Msg := Msg + #$D#$A + s;
    {$ENDIF}
      Error := EOSError.Create(Msg);
    {$IFDEF MSWINDOWS}
      Error.ErrorCode := GetLastError;
    {$ENDIF}
      raise Error;
    end;
    Exit;
  end;

  AssignProc(hssleay, @SSL_write, 'SSL_write');
  AssignProc(hssleay, @SSL_read, 'SSL_read');
  AssignProc(hssleay, @SSL_get_error, 'SSL_get_error');
  AssignProc(hssleay, @SSL_free, 'SSL_free');

  AssignProc(hssleay, @SSL_load_error_strings, 'SSL_load_error_strings');
  AssignProc(hssleay, @TLS_method, 'TLS_client_method');
  if @TLS_method = @NotLink then
    AssignProc(hssleay, @TLS_method, 'SSLv23_client_method');
  AssignProc(hssleay, @SSL_library_init, 'SSL_library_init');
  if @SSL_library_init = @NotLink then
    AssignProc(hssleay, @SSL_library_init, 'OPENSSL_init_ssl');

  AssignProc(hssleay, @SSL_CTX_new, 'SSL_CTX_new');
  AssignProc(hssleay, @SSL_CTX_set_cipher_list, 'SSL_CTX_set_cipher_list');
  AssignProc(hssleay, @SSL_CTX_free, 'SSL_CTX_free');
  AssignProc(hssleay, @SSL_new, 'SSL_new');
  AssignProc(hssleay, @SSL_clear, 'SSL_clear');
  AssignProc(hssleay, @SSL_SESSION_set_timeout, 'SSL_SESSION_set_timeout');
  AssignProc(hssleay, @SSL_get_session, 'SSL_get_session');
  AssignProc(hssleay, @SSL_set_fd, 'SSL_set_fd');
  AssignProc(hssleay, @SSL_set_connect_state, 'SSL_set_connect_state');
  AssignProc(hssleay, @SSL_do_handshake, 'SSL_do_handshake');
  AssignProc(hssleay, @SSL_CTX_set_verify, 'SSL_CTX_set_verify');
  AssignProc(hssleay, @SSL_CTX_load_verify_locations, 'SSL_CTX_load_verify_locations');
  AssignProc(hssleay, @SSL_CTX_set_default_verify_paths, 'SSL_CTX_set_default_verify_paths');
  AssignProc(hssleay, @SSL_CTX_use_certificate_file, 'SSL_CTX_use_certificate_file');
  AssignProc(hssleay, @SSL_CTX_use_PrivateKey_file, 'SSL_CTX_use_PrivateKey_file');
  AssignProc(hssleay, @SSL_CTX_check_private_key, 'SSL_CTX_check_private_key');
  AssignProc(hssleay, @SSL_CTX_ctrl, 'SSL_CTX_ctrl');

  AssignProc(hlibeay, @DH_new, 'DH_new');
  AssignProc(hlibeay, @DH_free, 'DH_free');
  AssignProc(hlibeay, @OPENSSL_add_all_algorithms_noconf, 'OPENSSL_add_all_algorithms_noconf');
  if @OPENSSL_add_all_algorithms_noconf = @NotLink then
    AssignProc(hlibeay, @OPENSSL_add_all_algorithms_noconf, 'OpenSSL_add_all_algorithms');

  AssignProc(hlibeay, @BN_bin2bn, 'BN_bin2bn');
  AssignProc(hlibeay, @X509_get_subject_name, 'X509_get_subject_name');
  AssignProc(hlibeay, @X509_NAME_oneline, 'X509_NAME_oneline');
  AssignProc(hlibeay, @X509_STORE_CTX_get_error_depth, 'X509_STORE_CTX_get_error_depth');
  AssignProc(hlibeay, @X509_STORE_CTX_get_error, 'X509_STORE_CTX_get_error');
  AssignProc(hlibeay, @X509_STORE_CTX_get_current_cert, 'X509_STORE_CTX_get_current_cert');
  AssignProc(hlibeay, @X509_get_issuer_name, 'X509_get_issuer_name');
end;

procedure InitSSLLib;
begin
  SSLInitLock.Enter;
  try
    if not LoadedSSLLib then
      LoadSSLLib(SilentMode);
  finally
    SSLInitLock.Leave;
  end;
end;

initialization
  OpenSSL11_Mode := False;
{$IFDEF MSWINDOWS}
  LIBEAY32DLL := '';
  SSLEAY32DLL := '';
  hlibeay := 0;
  hssleay := 0;
{$ELSE}
  SSLLIB := '';
{$IFDEF POSIX}
  hlib := 0;
{$ELSE}
  hlib := nil;
{$ENDIF}
{$ENDIF}
  SSLInitLock := TCriticalSection.Create;

finalization
{$IFDEF MSWINDOWS}
  if hlibeay <> 0 then begin
    FreeLibrary(hlibeay);
    hlibeay := 0;
  end;
  if hssleay <> 0 then begin
    FreeLibrary(hssleay);
    hssleay := 0;
  end;
{$ENDIF}
{$IFDEF POSIX}
  if hlib <> 0 then begin
    dlclose(hlib);
    hlib := 0;
  end;
{$ENDIF}
{$IFDEF UNIX}
  if hlib <> nil then begin
    dlclose(hlib);
    hlib := nil;
  end;
{$ENDIF}
  SSLInitLock.Free;

end.
