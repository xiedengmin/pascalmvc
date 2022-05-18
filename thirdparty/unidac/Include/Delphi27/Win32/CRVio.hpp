// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVio.pas' rev: 34.00 (Windows)

#ifndef CrvioHPP
#define CrvioHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crvio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRVio;
class DELPHICLASS TCRIOHandler;
class DELPHICLASS TCRVioHandler;
class DELPHICLASS TCRIOHandlerUtils;
class DELPHICLASS TProxyOptions;
class DELPHICLASS THttpOptions;
class DELPHICLASS TSSLOptions;
class DELPHICLASS TSSHOptions;
class DELPHICLASS TAsyncThread;
class DELPHICLASS TAsyncReceiveThread;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TIPVersion : unsigned char { ivIPv4, ivIPv6, ivIPBoth };

enum DECLSPEC_DENUM TCRSocksVersion : unsigned char { svNoSocks, svSocks4, svSocks5 };

class PASCALIMPLEMENTATION TCRVio : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FClosing;
	System::Classes::TNotifyEvent FOnClose;
	TAsyncThread* FReceiveThread;
	bool FNonBlocking;
	System::Classes::TNotifyEvent FOnAsyncReceive;
	void __fastcall SetNonBlocking(bool Value);
	void __fastcall OnReceive(Crtypes::_di_IScAsyncResult ar);
	void __fastcall DoOnClose();
	
protected:
	System::Syncobjs::TCriticalSection* FEvClose;
	System::UnicodeString FLastError;
	int FLastErrorCode;
	System::DynamicArray<System::Byte> FBuffer;
	int FBufferLen;
	int FBufferPos;
	virtual int __fastcall GetTimeout();
	virtual void __fastcall SetTimeout(int Value);
	virtual int __fastcall GetConnectionTimeout();
	virtual void __fastcall SetConnectionTimeout(int Value);
	virtual int __fastcall GetSendTimeout();
	virtual void __fastcall SetSendTimeout(int Value);
	virtual int __fastcall GetReceiveTimeout();
	virtual void __fastcall SetReceiveTimeout(int Value);
	virtual bool __fastcall GetConnected();
	virtual void __fastcall BeforeClosing();
	virtual void __fastcall InternalClose();
	virtual int __fastcall GetAvailable();
	
public:
	__fastcall TCRVio();
	__fastcall virtual ~TCRVio();
	virtual bool __fastcall TryConnect();
	virtual void __fastcall Connect() = 0 ;
	virtual void __fastcall Close();
	virtual int __fastcall ReadNoWait(const char * Buffer, int Offset, int Count) = 0 ;
	virtual int __fastcall WriteNoWait(const char * Buffer, int Offset, int Count) = 0 ;
	virtual int __fastcall Read(const char * Buffer, int Offset, int Count);
	virtual int __fastcall Write(const char * Buffer, int Offset, int Count);
	virtual bool __fastcall WaitForData(int MillisecondsTimeout = 0xffffffff);
	virtual Crtypes::_di_IScAsyncResult __fastcall BeginReceive(System::DynamicArray<System::Byte> &Buffer, const int Offset, const int Size, Crtypes::AsyncCallback Callback, System::TObject* State);
	virtual int __fastcall EndReceive(Crtypes::_di_IScAsyncResult AsyncResult);
	void __fastcall StopAsync();
	__property int Available = {read=GetAvailable, nodefault};
	__property bool Connected = {read=GetConnected, nodefault};
	__property int ConnectionTimeout = {read=GetConnectionTimeout, write=SetConnectionTimeout, nodefault};
	__property int SendTimeout = {read=GetSendTimeout, write=SetSendTimeout, nodefault};
	__property int ReceiveTimeout = {read=GetReceiveTimeout, write=SetReceiveTimeout, nodefault};
	__property int Timeout = {read=GetTimeout, write=SetTimeout, nodefault};
	__property System::UnicodeString LastError = {read=FLastError};
	__property int LastErrorCode = {read=FLastErrorCode, nodefault};
	__property bool NonBlocking = {read=FNonBlocking, write=SetNonBlocking, nodefault};
	__property System::Classes::TNotifyEvent OnAsyncReceive = {read=FOnAsyncReceive, write=FOnAsyncReceive};
	__property System::Classes::TNotifyEvent OnClose = {read=FOnClose, write=FOnClose};
};


typedef System::TObject TCRIOHandle;

class PASCALIMPLEMENTATION TCRIOHandler : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	System::Classes::TList* FList;
	void __fastcall RegisterClient(System::TObject* Client);
	void __fastcall UnRegisterClient(System::TObject* Client);
	__classmethod virtual void __fastcall SetIsSecure(System::TObject* Handle, const bool Value);
	__classmethod virtual bool __fastcall GetIsSecure(System::TObject* Handle);
	__classmethod virtual void __fastcall Renegotiate(System::TObject* Handle);
	virtual System::UnicodeString __fastcall GetHandlerType();
	
public:
	__fastcall virtual TCRIOHandler(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCRIOHandler();
	virtual System::TObject* __fastcall Connect(const System::UnicodeString Server, const int Port, THttpOptions* HttpOptions, TProxyOptions* ProxyOptions, TSSLOptions* SSLOptions, TSSHOptions* SSHOptions, TIPVersion IPVersion = (TIPVersion)(0x0)) = 0 ;
	virtual void __fastcall Disconnect(System::TObject* Handle);
	__classmethod virtual int __fastcall ReadNoWait(System::TObject* Handle, const char * Buffer, int Offset, int Count);
	__classmethod virtual int __fastcall Read(System::TObject* Handle, const char * Buffer, int Offset, int Count);
	__classmethod virtual int __fastcall Write(System::TObject* Handle, const char * Buffer, int Offset, int Count);
	__classmethod virtual bool __fastcall WaitForData(System::TObject* Handle, int MillisecondsTimeout = 0xffffffff);
	__classmethod virtual int __fastcall GetTimeout(System::TObject* Handle);
	__classmethod virtual void __fastcall SetTimeout(System::TObject* Handle, int Value);
	__property System::UnicodeString HandlerType = {read=GetHandlerType};
};


class PASCALIMPLEMENTATION TCRVioHandler : public TCRVio
{
	typedef TCRVio inherited;
	
protected:
	TCRIOHandler* FIOHandler;
	System::TObject* FHandle;
	System::UnicodeString FHostname;
	int FPort;
	int FTimeout;
	TIPVersion FIPVersion;
	THttpOptions* FHttpOptions;
	TProxyOptions* FProxyOptions;
	TSSLOptions* FSSLOptions;
	TSSHOptions* FSSHOptions;
	virtual int __fastcall GetTimeout();
	virtual void __fastcall SetTimeout(int Value);
	virtual int __fastcall GetReceiveTimeout();
	virtual void __fastcall SetReceiveTimeout(int Value);
	void __fastcall SetIsSecure(const bool Value);
	bool __fastcall GetIsSecure();
	
public:
	__fastcall TCRVioHandler(const System::UnicodeString Hostname, const int Port, TCRIOHandler* IOHandler, THttpOptions* HttpOptions, TProxyOptions* ProxyOptions, TSSLOptions* SSLOptions, TSSHOptions* SSHOptions, TIPVersion IPVersion);
	__fastcall virtual ~TCRVioHandler();
	virtual void __fastcall Connect();
	virtual void __fastcall Close();
	void __fastcall Renegotiate();
	virtual Crtypes::_di_IScAsyncResult __fastcall BeginReceive(System::DynamicArray<System::Byte> &Buffer, const int Offset, const int Size, Crtypes::AsyncCallback Callback, System::TObject* State);
	virtual int __fastcall EndReceive(Crtypes::_di_IScAsyncResult AsyncResult);
	virtual int __fastcall ReadNoWait(const char * Buffer, int Offset, int Count);
	virtual int __fastcall WriteNoWait(const char * Buffer, int Offset, int Count);
	virtual int __fastcall Read(const char * Buffer, int Offset, int Count);
	virtual int __fastcall Write(const char * Buffer, int Offset, int Count);
	virtual bool __fastcall WaitForData(int MillisecondsTimeout = 0xffffffff);
	__property bool IsSecure = {read=GetIsSecure, write=SetIsSecure, nodefault};
	__property System::UnicodeString Host = {read=FHostname};
	__property int Port = {read=FPort, nodefault};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRIOHandlerUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall RegisterClient(TCRIOHandler* Obj, System::TObject* Client);
	__classmethod void __fastcall UnRegisterClient(TCRIOHandler* Obj, System::TObject* Client);
public:
	/* TObject.Create */ inline __fastcall TCRIOHandlerUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCRIOHandlerUtils() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TProxyOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FHostname;
	int FPort;
	System::UnicodeString FUsername;
	System::UnicodeString FPassword;
	TCRSocksVersion FSocksVersion;
	bool FResolveDNS;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TProxyOptions();
	HIDESBASE bool __fastcall Equals(TProxyOptions* ProxyOptions);
	
__published:
	__property System::UnicodeString Hostname = {read=FHostname, write=FHostname};
	__property int Port = {read=FPort, write=FPort, default=0};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property TCRSocksVersion SocksVersion = {read=FSocksVersion, write=FSocksVersion, default=0};
	__property bool ResolveDNS = {read=FResolveDNS, write=FResolveDNS, default=1};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TProxyOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THttpOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FUrl;
	System::UnicodeString FUsername;
	System::UnicodeString FPassword;
	bool FTrustServerCertificate;
	TProxyOptions* FProxyOptions;
	TProxyOptions* __fastcall GetProxyOptions();
	void __fastcall SetProxyOptions(TProxyOptions* Value);
	void __fastcall ReadProxyHostname(System::Classes::TReader* Reader);
	void __fastcall ReadProxyPort(System::Classes::TReader* Reader);
	void __fastcall ReadProxyUsername(System::Classes::TReader* Reader);
	void __fastcall ReadProxyPassword(System::Classes::TReader* Reader);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	HIDESBASE bool __fastcall Equals(THttpOptions* HttpOptions);
	__property TProxyOptions* ProxyOptions = {read=GetProxyOptions, write=SetProxyOptions};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=0};
	
__published:
	__property System::UnicodeString Url = {read=FUrl, write=FUrl};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property bool TrustServerCertificate = {read=FTrustServerCertificate, write=FTrustServerCertificate, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~THttpOptions() { }
	
public:
	/* TObject.Create */ inline __fastcall THttpOptions() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSSLOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FCA;
	System::UnicodeString FCert;
	System::UnicodeString FKey;
	System::UnicodeString FCipher;
	bool FIgnoreServerCertificateValidity;
	bool FIgnoreServerCertificateConstraints;
	bool FIgnoreServerCertificateInsecurity;
	bool FTrustServerCertificate;
	bool FTrustSelfSignedCertificate;
	bool FForceUseTrustServerCertificate;
	System::UnicodeString FServerCertDN;
	System::UnicodeString FIdentityDNSName;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TSSLOptions();
	HIDESBASE bool __fastcall Equals(TSSLOptions* SslOptions);
	__property System::UnicodeString IdentityDNSName = {read=FIdentityDNSName, write=FIdentityDNSName};
	__property System::UnicodeString ServerCertDN = {read=FServerCertDN, write=FServerCertDN};
	__property bool ForceUseTrustServerCertificate = {read=FForceUseTrustServerCertificate, write=FForceUseTrustServerCertificate, nodefault};
	__property bool TrustSelfSignedCertificate = {read=FTrustSelfSignedCertificate, write=FTrustSelfSignedCertificate, nodefault};
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString CA = {read=FCA, write=FCA};
	__property System::UnicodeString Cert = {read=FCert, write=FCert};
	__property System::UnicodeString Key = {read=FKey, write=FKey};
	__property System::UnicodeString Cipher = {read=FCipher, write=FCipher};
	__property bool IgnoreServerCertificateValidity = {read=FIgnoreServerCertificateValidity, write=FIgnoreServerCertificateValidity, default=0};
	__property bool IgnoreServerCertificateConstraints = {read=FIgnoreServerCertificateConstraints, write=FIgnoreServerCertificateConstraints, default=0};
	__property bool IgnoreServerCertificateInsecurity = {read=FIgnoreServerCertificateInsecurity, write=FIgnoreServerCertificateInsecurity, default=0};
	__property bool TrustServerCertificate = {read=FTrustServerCertificate, write=FTrustServerCertificate, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSSLOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSSHOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FHostname;
	int FPort;
	System::UnicodeString FUsername;
	System::UnicodeString FPassword;
	System::UnicodeString FClientKey;
	System::UnicodeString FClientKeyPassword;
	System::UnicodeString FServerKey;
	System::UnicodeString FPath;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Hostname = {read=FHostname, write=FHostname};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString ClientKey = {read=FClientKey, write=FClientKey};
	__property System::UnicodeString ClientKeyPassword = {read=FClientKeyPassword, write=FClientKeyPassword};
	__property System::UnicodeString ServerKey = {read=FServerKey, write=FServerKey};
	__property System::UnicodeString Path = {read=FPath, write=FPath};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSSHOptions() { }
	
public:
	/* TObject.Create */ inline __fastcall TSSHOptions() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAsyncThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	bool FFinished;
	
protected:
	System::Syncobjs::TCriticalSection* FLock;
	System::Syncobjs::TEvent* FResumedEvent;
	TCRVio* FVio;
	Crtypes::AsyncCallback FCallbackProc;
	System::DynamicArray<System::Byte> FBuffer;
	int FOffset;
	int FSize;
	
public:
	__fastcall TAsyncThread(/* out */ TAsyncThread* &Addr, TCRVio* Vio, Crtypes::AsyncCallback CallbackProc, const System::DynamicArray<System::Byte> Buffer, int Offset, int Size);
	__fastcall virtual ~TAsyncThread();
	void __fastcall Restart(TCRVio* Vio, Crtypes::AsyncCallback CallbackProc, const System::DynamicArray<System::Byte> Buffer, int Offset, int Size);
	void __fastcall Pause();
};


class PASCALIMPLEMENTATION TAsyncReceiveThread : public TAsyncThread
{
	typedef TAsyncThread inherited;
	
protected:
	virtual void __fastcall Execute();
public:
	/* TAsyncThread.Create */ inline __fastcall TAsyncReceiveThread(/* out */ TAsyncThread* &Addr, TCRVio* Vio, Crtypes::AsyncCallback CallbackProc, const System::DynamicArray<System::Byte> Buffer, int Offset, int Size) : TAsyncThread(Addr, Vio, CallbackProc, Buffer, Offset, Size) { }
	/* TAsyncThread.Destroy */ inline __fastcall virtual ~TAsyncReceiveThread() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _SVioError;
#define Crvio_SVioError System::LoadResourceString(&Crvio::_SVioError)
extern DELPHI_PACKAGE System::ResourceString _SNotOverriden;
#define Crvio_SNotOverriden System::LoadResourceString(&Crvio::_SNotOverriden)
static const System::Word VIO_READ_BUFFER_SIZE = System::Word(0x8000);
static const System::Word VIO_UNBUFFERED_READ_MIN_SIZE = System::Word(0x800);
static const TIPVersion DefValIPVersion = (TIPVersion)(0);
static const TCRSocksVersion DefValSocksVersion = (TCRSocksVersion)(0);
static const bool DefValResolveDNS = true;
}	/* namespace Crvio */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVIO)
using namespace Crvio;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrvioHPP
