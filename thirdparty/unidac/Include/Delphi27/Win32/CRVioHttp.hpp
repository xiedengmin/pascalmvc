// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVioHttp.pas' rev: 34.00 (Windows)

#ifndef CrviohttpHPP
#define CrviohttpHPP

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
#include <CRFunctions.hpp>
#include <CRVio.hpp>
#include <CRHttp.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crviohttp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTimerThread;
class DELPHICLASS TCRVioHttp;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TTimerThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
protected:
	unsigned FInterval;
	System::Classes::TNotifyEvent FOnTimer;
	unsigned FPreviousTimeout;
	System::Syncobjs::TEvent* FEvent;
	virtual void __fastcall Execute();
	
public:
	__fastcall TTimerThread(unsigned Interval, System::Classes::TNotifyEvent OnTimer);
	__fastcall virtual ~TTimerThread();
};


class PASCALIMPLEMENTATION TCRVioHttp : public Crvio::TCRVio
{
	typedef Crvio::TCRVio inherited;
	
private:
	Crvio::TCRIOHandler* FIOHandler;
	Crvio::THttpOptions* FHttpOptions;
	Crvio::TProxyOptions* FProxyOptions;
	System::UnicodeString FUrl;
	System::UnicodeString FTestUrl;
	int FPortID;
	bool FClosed;
	Crhttp::TCRHttpWebRequest* FStartServerScriptRequest;
	Crhttp::TCRHttpWebResponse* FLastReadResponse;
	int FConnectionTimeout;
	int FSendTimeout;
	int FReceiveTimeout;
	int FScriptNotificationTime;
	System::Syncobjs::TCriticalSection* FExceptionLock;
	System::Sysutils::Exception* FThreadException;
	Crvio::TIPVersion FIPVersion;
	System::Syncobjs::TEvent* FIsConnectedEvent;
	TTimerThread* FTimerThread;
	void __fastcall SetIPVersion(const Crvio::TIPVersion Value);
	void __fastcall OnScriptNotification(System::TObject* Sender);
	void __fastcall OnStartServerScriptConnected(System::TObject* Sender);
	void __fastcall ReStartNotification();
	void __fastcall StartServerScript();
	void __fastcall AbortConnectionScript();
	Crhttp::TCRHttpWebRequest* __fastcall CreateRequest(const System::UnicodeString Url, const System::WideChar Command, const System::UnicodeString Method);
	void __fastcall CheckTestConnectionResponse(Crhttp::TCRHttpWebResponse* Response);
	void __fastcall CheckResponseSuccess(Crhttp::TCRHttpWebResponse* Response);
	void __fastcall CheckThreadException();
	Crhttp::TCRHttpWebResponse* __fastcall ExecuteGetRequest(const System::UnicodeString Url, const System::WideChar Command, bool CheckLeaseException = true);
	
protected:
	virtual bool __fastcall GetConnected();
	virtual int __fastcall GetConnectionTimeout();
	virtual void __fastcall SetConnectionTimeout(int Value);
	virtual int __fastcall GetSendTimeout();
	virtual void __fastcall SetSendTimeout(int Value);
	virtual int __fastcall GetReceiveTimeout();
	virtual void __fastcall SetReceiveTimeout(int Value);
	
public:
	__fastcall TCRVioHttp(Crvio::TCRIOHandler* IOHandler, Crvio::THttpOptions* HttpOptions, Crvio::TProxyOptions* ProxyOptions, const System::UnicodeString Hostname, int Port, Crvio::TIPVersion IPVersion);
	__fastcall virtual ~TCRVioHttp();
	virtual void __fastcall Connect();
	virtual void __fastcall Close();
	virtual int __fastcall ReadNoWait(const char * buffer, int offset, int count);
	virtual int __fastcall WriteNoWait(const char * buffer, int offset, int count);
	virtual int __fastcall Write(const char * buffer, int offset, int count);
	virtual bool __fastcall WaitForData(int MillisecondsTimeout = 0xffffffff);
	__property Crvio::TIPVersion IPVersion = {read=FIPVersion, write=SetIPVersion, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crviohttp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVIOHTTP)
using namespace Crviohttp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrviohttpHPP
