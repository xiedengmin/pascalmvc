// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVioUdp.pas' rev: 34.00 (Windows)

#ifndef CrvioudpHPP
#define CrvioudpHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRVio.hpp>
#include <CRVioSocket.hpp>

//-- user supplied -----------------------------------------------------------
#include <winsock2.h>

namespace Crvioudp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRVioUdp;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCRVioUdp : public Crviosocket::TCRVioSocket
{
	typedef Crviosocket::TCRVioSocket inherited;
	
private:
	bool FIsBroadcast;
	void *FBroadcastHostAddrInfo;
	void *FHostAddrInfo;
	void * __fastcall CheckHostAddrInfo(bool IsBroadcast);
	int __fastcall InternalReadFrom(const char * Buffer, int Offset, int Count);
	
protected:
	virtual void __fastcall InternalClose();
	
public:
	void __fastcall CreateSocket();
	virtual void __fastcall Connect();
	virtual int __fastcall Read(const char * Buffer, int Offset, int Count);
	int __fastcall ReadFromHost(const char * Buffer, int Offset, int Count, /* out */ bool &FromHost);
	virtual int __fastcall ReadNoWait(const char * buffer, int offset, int count);
	virtual int __fastcall Write(const char * Buffer, int Offset, int Count);
	virtual int __fastcall WriteNoWait(const char * buffer, int offset, int count);
	__property bool IsBroadcast = {read=FIsBroadcast, write=FIsBroadcast, nodefault};
public:
	/* TCRVioSocket.Create */ inline __fastcall virtual TCRVioUdp()/* overload */ : Crviosocket::TCRVioSocket() { }
	/* TCRVioSocket.Create */ inline __fastcall virtual TCRVioUdp(const System::UnicodeString Hostname, int Port, Crvio::TIPVersion IPVersion)/* overload */ : Crviosocket::TCRVioSocket(Hostname, Port, IPVersion) { }
	/* TCRVioSocket.Destroy */ inline __fastcall virtual ~TCRVioUdp() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crvioudp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVIOUDP)
using namespace Crvioudp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrvioudpHPP
