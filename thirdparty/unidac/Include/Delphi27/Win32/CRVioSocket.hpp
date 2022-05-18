// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVioSocket.pas' rev: 34.00 (Windows)

#ifndef CrviosocketHPP
#define CrviosocketHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <System.SyncObjs.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRVio.hpp>

//-- user supplied -----------------------------------------------------------
#include <winsock2.h>
#ifdef SetPort
#undef SetPort
#endif

namespace Crviosocket
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRVioSocket;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCRVioSocket : public Crvio::TCRVio
{
	typedef Crvio::TCRVio inherited;
	
protected:
	System::UnicodeString FBindAddress;
	System::UnicodeString FHostName;
	int FPort;
	int FConnectionTimeout;
	int FSendTimeout;
	int FReceiveTimeout;
	int FSendBuffer;
	int FReceiveBuffer;
	Crvio::TIPVersion FIPVersion;
	System::Syncobjs::TCriticalSection* hLockSd;
	NativeInt FSd;
	bool FAborted;
	void __fastcall SetBindAddress(const System::UnicodeString Value);
	void __fastcall SetHost(const System::UnicodeString Value);
	void __fastcall SetPort(const int Value);
	void __fastcall SetIPVersion(const Crvio::TIPVersion Value);
	virtual int __fastcall GetConnectionTimeout();
	virtual void __fastcall SetConnectionTimeout(int Value);
	virtual int __fastcall GetSendTimeout();
	virtual void __fastcall SetSendTimeout(int Value);
	virtual int __fastcall GetReceiveTimeout();
	virtual void __fastcall SetReceiveTimeout(int Value);
	void __fastcall SetSendBuffer(const int Value);
	void __fastcall SetReceiveBuffer(const int Value);
	virtual bool __fastcall GetConnected();
	virtual void __fastcall BeforeClosing();
	virtual void __fastcall InternalClose();
	virtual void __fastcall Init();
	virtual void __fastcall CloseSocket();
	
public:
	__fastcall virtual TCRVioSocket()/* overload */;
	__fastcall virtual TCRVioSocket(const System::UnicodeString Hostname, int Port, Crvio::TIPVersion IPVersion)/* overload */;
	__fastcall virtual ~TCRVioSocket();
	virtual bool __fastcall WaitForData(int MillisecondsTimeout = 0xffffffff);
	virtual NativeInt __fastcall GetSocket();
	__property int SendBuffer = {read=FSendBuffer, write=SetSendBuffer, nodefault};
	__property int ReceiveBuffer = {read=FReceiveBuffer, write=SetReceiveBuffer, nodefault};
	__property System::UnicodeString BindAddress = {read=FBindAddress, write=SetBindAddress};
	__property System::UnicodeString Host = {read=FHostName, write=SetHost};
	__property int Port = {read=FPort, write=SetPort, nodefault};
	__property Crvio::TIPVersion IPVersion = {read=FIPVersion, write=SetIPVersion, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _SSocketError;
#define Crviosocket_SSocketError System::LoadResourceString(&Crviosocket::_SSocketError)
extern DELPHI_PACKAGE System::ResourceString _SSocketNotConnected;
#define Crviosocket_SSocketNotConnected System::LoadResourceString(&Crviosocket::_SSocketNotConnected)
extern DELPHI_PACKAGE System::ResourceString _SErrorOnDataWaiting;
#define Crviosocket_SErrorOnDataWaiting System::LoadResourceString(&Crviosocket::_SErrorOnDataWaiting)
extern DELPHI_PACKAGE System::ResourceString _SCannotChangeHost;
#define Crviosocket_SCannotChangeHost System::LoadResourceString(&Crviosocket::_SCannotChangeHost)
extern DELPHI_PACKAGE System::ResourceString _SCannotChangePort;
#define Crviosocket_SCannotChangePort System::LoadResourceString(&Crviosocket::_SCannotChangePort)
extern DELPHI_PACKAGE System::ResourceString _SCannotChangeTcpVersion;
#define Crviosocket_SCannotChangeTcpVersion System::LoadResourceString(&Crviosocket::_SCannotChangeTcpVersion)
#define LOCAL_HOST L"localhost"
static const System::Int8 DefaultTimeOut = System::Int8(0x1e);
static const System::Word DefaultBuffer = System::Word(0x2000);
extern DELPHI_PACKAGE WSAData WsaData;
extern DELPHI_PACKAGE System::Syncobjs::TCriticalSection* hLockWsaData;
extern DELPHI_PACKAGE void __fastcall GetIPInterfaces(System::Classes::TStrings* List);
}	/* namespace Crviosocket */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVIOSOCKET)
using namespace Crviosocket;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrviosocketHPP
