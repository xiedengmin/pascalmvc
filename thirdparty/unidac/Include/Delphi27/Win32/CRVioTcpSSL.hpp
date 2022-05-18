// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVioTcpSSL.pas' rev: 34.00 (Windows)

#ifndef CrviotcpsslHPP
#define CrviotcpsslHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SyncObjs.hpp>
#include <CLRClasses.hpp>
#include <CRVio.hpp>
#include <CRTypes.hpp>
#include <CRVioTcp.hpp>
#include <CRVioSocket.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crviotcpssl
{
//-- forward type declarations -----------------------------------------------
struct st_VioSSLConnectorFd;
class DELPHICLASS TCRVioTcpSSL;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD st_VioSSLConnectorFd
{
public:
	void *ssl_context_;
	void *ssl_method_;
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TCRVioTcpSSL : public Crviotcp::TCRVioTcp
{
	typedef Crviotcp::TCRVioTcp inherited;
	
protected:
	bool FisSecure;
	System::UnicodeString FSSL_key;
	System::UnicodeString FSSL_cert;
	System::UnicodeString FSSL_ca;
	System::UnicodeString FSSL_capath;
	System::UnicodeString FSSL_cipher;
	st_VioSSLConnectorFd Fnewcon;
	void *Fssl_arg;
	void __fastcall new_VioSSLConnectorFd();
	void __fastcall SetisSecure(bool Value);
	
public:
	__fastcall TCRVioTcpSSL(const System::UnicodeString hostname, const int port, const System::UnicodeString SSL_key, const System::UnicodeString SSL_cert, const System::UnicodeString SSL_ca, const System::UnicodeString SSL_capath, const System::UnicodeString SSL_cipher, Crvio::TIPVersion IPVersion);
	__fastcall virtual ~TCRVioTcpSSL();
	virtual int __fastcall ReadNoWait(const char * buffer, int offset, int count);
	virtual int __fastcall WriteNoWait(const char * buffer, int offset, int count);
	__property bool isSecure = {read=FisSecure, write=SetisSecure, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 SSL_VERIFY_NONE = System::Int8(0x0);
static const System::Int8 SSL_CTRL_SET_TMP_DH = System::Int8(0x3);
static const System::Int8 SSL_ERROR_WANT_READ = System::Int8(0x2);
static const System::Int8 SSL_ERROR_WANT_WRITE = System::Int8(0x3);
extern DELPHI_PACKAGE bool OpenSSL11_Mode;
extern DELPHI_PACKAGE int __cdecl (*SSL_write)(void * s, void * pBuf, int len);
extern DELPHI_PACKAGE int __cdecl (*SSL_read)(void * s, void * pBuf, int len);
extern DELPHI_PACKAGE int __cdecl (*SSL_get_error)(const void * s, int ret_code);
extern DELPHI_PACKAGE void __cdecl (*SSL_free)(void * s);
extern DELPHI_PACKAGE void __cdecl (*SSL_load_error_strings)(void);
extern DELPHI_PACKAGE void * __cdecl (*TLS_method)(void);
extern DELPHI_PACKAGE int __cdecl (*SSL_library_init)(void);
extern DELPHI_PACKAGE void * __cdecl (*SSL_CTX_new)(void * meth);
extern DELPHI_PACKAGE int __cdecl (*SSL_CTX_set_cipher_list)(void * actx, const char * str);
extern DELPHI_PACKAGE void __cdecl (*SSL_CTX_free)(void * actx);
extern DELPHI_PACKAGE void * __cdecl (*SSL_new)(void * s);
extern DELPHI_PACKAGE int __cdecl (*SSL_clear)(void * s);
extern DELPHI_PACKAGE int __cdecl (*SSL_SESSION_set_timeout)(void * s, unsigned t);
extern DELPHI_PACKAGE void * __cdecl (*SSL_get_session)(void * s);
extern DELPHI_PACKAGE int __cdecl (*SSL_set_fd)(void * s, int fd);
extern DELPHI_PACKAGE void __cdecl (*SSL_set_connect_state)(void * s);
extern DELPHI_PACKAGE int __cdecl (*SSL_do_handshake)(void * s);
extern DELPHI_PACKAGE void __cdecl (*SSL_CTX_set_verify)(void * actx, int mode, void * acallback);
extern DELPHI_PACKAGE int __cdecl (*SSL_CTX_load_verify_locations)(void * actx, const char * CAfile, const char * CApath);
extern DELPHI_PACKAGE int __cdecl (*SSL_CTX_set_default_verify_paths)(void * actx);
extern DELPHI_PACKAGE int __cdecl (*SSL_CTX_use_certificate_file)(void * actx, const char * afile, int atype);
extern DELPHI_PACKAGE int __cdecl (*SSL_CTX_use_PrivateKey_file)(void * actx, const char * afile, int atype);
extern DELPHI_PACKAGE int __cdecl (*SSL_CTX_check_private_key)(const void * actx);
extern DELPHI_PACKAGE void * __cdecl (*DH_new)(void);
extern DELPHI_PACKAGE int __cdecl (*DH_free)(void * dh);
extern DELPHI_PACKAGE void __cdecl (*OPENSSL_add_all_algorithms_noconf)(void);
extern DELPHI_PACKAGE void * __cdecl (*BN_bin2bn)(const void * s, int len, void * ret);
extern DELPHI_PACKAGE void * __cdecl (*X509_get_subject_name)(void * a);
extern DELPHI_PACKAGE char * __cdecl (*X509_NAME_oneline)(void * a, char * buf, int size);
extern DELPHI_PACKAGE int __cdecl (*X509_STORE_CTX_get_error_depth)(void * actx);
extern DELPHI_PACKAGE int __cdecl (*X509_STORE_CTX_get_error)(void * actx);
extern DELPHI_PACKAGE void * __cdecl (*X509_STORE_CTX_get_current_cert)(void * actx);
extern DELPHI_PACKAGE int __cdecl (*SSL_CTX_ctrl)(void * actx, int a1, int a2, void * adh);
extern DELPHI_PACKAGE void * __cdecl (*X509_get_issuer_name)(void * a);
extern DELPHI_PACKAGE System::UnicodeString LIBEAY32DLL;
extern DELPHI_PACKAGE System::UnicodeString SSLEAY32DLL;
extern DELPHI_PACKAGE bool ssl_algorithms_added;
extern DELPHI_PACKAGE bool ssl_error_strings_loaded;
extern DELPHI_PACKAGE NativeUInt hlibeay;
extern DELPHI_PACKAGE NativeUInt hssleay;
extern DELPHI_PACKAGE int __cdecl vio_verify_callback(int ok, void * ctx);
extern DELPHI_PACKAGE int __fastcall vio_set_cert_stuff(void * ctx, const System::UnicodeString cert_file, System::UnicodeString key_file);
extern DELPHI_PACKAGE void * __fastcall get_dh512(void);
extern DELPHI_PACKAGE void __fastcall LoadSSLLib(bool SilentMode = false);
extern DELPHI_PACKAGE void __fastcall InitSSLLib(bool SilentMode = false);
}	/* namespace Crviotcpssl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVIOTCPSSL)
using namespace Crviotcpssl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrviotcpsslHPP
