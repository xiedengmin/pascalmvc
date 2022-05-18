// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVioTcp.pas' rev: 34.00 (Windows)

#ifndef CrviotcpHPP
#define CrviotcpHPP

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

namespace Crviotcp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRVioTcp;
class DELPHICLASS TConnector;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCRConnectMode : unsigned char { cmBind, cmConnect };

class PASCALIMPLEMENTATION TCRVioTcp : public Crviosocket::TCRVioSocket
{
	typedef Crviosocket::TCRVioSocket inherited;
	
private:
	System::UnicodeString FProviderName;
	Crvio::TProxyOptions* FProxyOptions;
	sockaddr_in *FLocalSockAddr;
	sockaddr_in *FRemoteSockAddr;
	TCRConnectMode FConnectMode;
	Crvio::TCRSocksVersion FSocksVersion;
	bool __fastcall InternalConnect(const System::UnicodeString Host, int Port);
	void __fastcall HttpNegotiate();
	void __fastcall Socks4Negotiate();
	void __fastcall Socks5Negotiate();
	void __fastcall WriteSocks5Address(const System::DynamicArray<System::Byte> Buffer, int &Offset, const System::UnicodeString AHostname, const int APort);
	PSOCKADDR __fastcall GetLocalSockAddr();
	void __fastcall SetLocalSockAddr(PSOCKADDR Value);
	PSOCKADDR __fastcall GetRemoteSockAddr();
	void __fastcall SetRemoteSockAddr(PSOCKADDR Value);
	System::UnicodeString __fastcall GetSockAddrIP(const PSOCKADDR SockAddr);
	int __fastcall GetSockAddrPort(const PSOCKADDR SockAddr);
	
protected:
	virtual void __fastcall Init();
	virtual void __fastcall CloseSocket();
	virtual int __fastcall GetAvailable();
	System::UnicodeString __fastcall ReadLine();
	void __fastcall ProcessWebResponse(const System::UnicodeString Response);
	
public:
	__fastcall TCRVioTcp(Crvio::TProxyOptions* ProxyOptions, const System::UnicodeString ProviderName, const System::UnicodeString Hostname, int Port, Crvio::TIPVersion IPVersion)/* overload */;
	__fastcall virtual ~TCRVioTcp();
	TCRVioTcp* __fastcall CreateNew(NativeInt NewSd, PSOCKADDR From);
	virtual bool __fastcall WaitForConnect(TConnector* AConnector, unsigned AConnectionTimeoutMSec, System::UnicodeString &AErrorDesc);
	virtual bool __fastcall TryConnect();
	virtual void __fastcall Connect();
	bool __fastcall TryBind();
	void __fastcall Bind();
	void __fastcall ShutDown(int how);
	__classmethod int __fastcall GetSockAddrSize(PSOCKADDR SockAddr);
	System::UnicodeString __fastcall GetLocalIP();
	int __fastcall GetLocalPort();
	System::UnicodeString __fastcall GetRemoteIP();
	int __fastcall GetRemotePort();
	void __fastcall Listen(int Backlog);
	void __fastcall Accept(/* out */ NativeInt &NewSd, /* out */ PSOCKADDR &From);
	int __fastcall GetSocketOption(int OptionLevel, int OptionName);
	void __fastcall SetSocketOption(int OptionLevel, int OptionName, int OptionValue);
	virtual int __fastcall ReadNoWait(const char * buffer, int offset, int count);
	virtual int __fastcall WriteNoWait(const char * buffer, int offset, int count);
	virtual int __fastcall Write(const char * buffer, int offset, int count);
	void __fastcall ReadSocksRequest(/* out */ System::UnicodeString &AHostname, /* out */ int &APort);
	void __fastcall WriteSocksResponse(const System::UnicodeString AHostname, const int APort);
	__property PSOCKADDR LocalSockAddr = {read=GetLocalSockAddr};
	__property PSOCKADDR RemoteSockAddr = {read=GetRemoteSockAddr};
	__property System::UnicodeString ProviderName = {read=FProviderName};
	__property Crvio::TProxyOptions* ProxyOptions = {read=FProxyOptions};
public:
	/* TCRVioSocket.Create */ inline __fastcall virtual TCRVioTcp()/* overload */ : Crviosocket::TCRVioSocket() { }
	/* TCRVioSocket.Create */ inline __fastcall virtual TCRVioTcp(const System::UnicodeString Hostname, int Port, Crvio::TIPVersion IPVersion)/* overload */ : Crviosocket::TCRVioSocket(Hostname, Port, IPVersion) { }
	
};


class PASCALIMPLEMENTATION TConnector : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TCRVioTcp* FVio;
	System::UnicodeString FHostname;
	int FPort;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TConnector(const System::UnicodeString Hostname, int Port, TCRVioTcp* Vio);
	__fastcall virtual ~TConnector();
	bool __fastcall Wait(unsigned Timeout);
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _SUnknownSocketError;
#define Crviotcp_SUnknownSocketError System::LoadResourceString(&Crviotcp::_SUnknownSocketError)
extern DELPHI_PACKAGE System::ResourceString _SWSAStartupFailed;
#define Crviotcp_SWSAStartupFailed System::LoadResourceString(&Crviotcp::_SWSAStartupFailed)
extern DELPHI_PACKAGE System::ResourceString _SUnknownHost;
#define Crviotcp_SUnknownHost System::LoadResourceString(&Crviotcp::_SUnknownHost)
extern DELPHI_PACKAGE System::ResourceString _SCannotCreateSocket;
#define Crviotcp_SCannotCreateSocket System::LoadResourceString(&Crviotcp::_SCannotCreateSocket)
extern DELPHI_PACKAGE System::ResourceString _SSocketAborted;
#define Crviotcp_SSocketAborted System::LoadResourceString(&Crviotcp::_SSocketAborted)
extern DELPHI_PACKAGE System::ResourceString _SCannotBind;
#define Crviotcp_SCannotBind System::LoadResourceString(&Crviotcp::_SCannotBind)
extern DELPHI_PACKAGE System::ResourceString _SCannotConnect;
#define Crviotcp_SCannotConnect System::LoadResourceString(&Crviotcp::_SCannotConnect)
extern DELPHI_PACKAGE System::ResourceString _SConnectionTimeout;
#define Crviotcp_SConnectionTimeout System::LoadResourceString(&Crviotcp::_SConnectionTimeout)
extern DELPHI_PACKAGE System::ResourceString _SErrorOnListening;
#define Crviotcp_SErrorOnListening System::LoadResourceString(&Crviotcp::_SErrorOnListening)
extern DELPHI_PACKAGE System::ResourceString _SErrorOnAccepting;
#define Crviotcp_SErrorOnAccepting System::LoadResourceString(&Crviotcp::_SErrorOnAccepting)
extern DELPHI_PACKAGE System::ResourceString _SErrorOnSettingOptions;
#define Crviotcp_SErrorOnSettingOptions System::LoadResourceString(&Crviotcp::_SErrorOnSettingOptions)
extern DELPHI_PACKAGE System::ResourceString _SErrorOnRetrievingOptions;
#define Crviotcp_SErrorOnRetrievingOptions System::LoadResourceString(&Crviotcp::_SErrorOnRetrievingOptions)
extern DELPHI_PACKAGE System::ResourceString _SErrorOnDataReading;
#define Crviotcp_SErrorOnDataReading System::LoadResourceString(&Crviotcp::_SErrorOnDataReading)
extern DELPHI_PACKAGE System::ResourceString _SErrorOnDataWriting;
#define Crviotcp_SErrorOnDataWriting System::LoadResourceString(&Crviotcp::_SErrorOnDataWriting)
extern DELPHI_PACKAGE System::ResourceString _SInvalidDataWriting;
#define Crviotcp_SInvalidDataWriting System::LoadResourceString(&Crviotcp::_SInvalidDataWriting)
extern DELPHI_PACKAGE System::ResourceString _SSocketErrWithErr;
#define Crviotcp_SSocketErrWithErr System::LoadResourceString(&Crviotcp::_SSocketErrWithErr)
extern DELPHI_PACKAGE System::ResourceString _SHttpResponseTooLarge;
#define Crviotcp_SHttpResponseTooLarge System::LoadResourceString(&Crviotcp::_SHttpResponseTooLarge)
extern DELPHI_PACKAGE System::ResourceString _SUnknownHttpVersion;
#define Crviotcp_SUnknownHttpVersion System::LoadResourceString(&Crviotcp::_SUnknownHttpVersion)
extern DELPHI_PACKAGE System::ResourceString _SHttpError;
#define Crviotcp_SHttpError System::LoadResourceString(&Crviotcp::_SHttpError)
extern DELPHI_PACKAGE System::ResourceString _SSocksAccessDenided;
#define Crviotcp_SSocksAccessDenided System::LoadResourceString(&Crviotcp::_SSocksAccessDenided)
extern DELPHI_PACKAGE System::ResourceString _SSocksResponseTooLarge;
#define Crviotcp_SSocksResponseTooLarge System::LoadResourceString(&Crviotcp::_SSocksResponseTooLarge)
extern DELPHI_PACKAGE System::ResourceString _SUnknownSocksAddressType;
#define Crviotcp_SUnknownSocksAddressType System::LoadResourceString(&Crviotcp::_SUnknownSocksAddressType)
extern DELPHI_PACKAGE System::ResourceString _SUnsupportedCommandType;
#define Crviotcp_SUnsupportedCommandType System::LoadResourceString(&Crviotcp::_SUnsupportedCommandType)
extern DELPHI_PACKAGE System::ResourceString _SUnsupportedAuthType;
#define Crviotcp_SUnsupportedAuthType System::LoadResourceString(&Crviotcp::_SUnsupportedAuthType)
extern DELPHI_PACKAGE System::ResourceString _SUnknownSocksVersion;
#define Crviotcp_SUnknownSocksVersion System::LoadResourceString(&Crviotcp::_SUnknownSocksVersion)
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetHostName(void);
}	/* namespace Crviotcp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVIOTCP)
using namespace Crviotcp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrviotcpHPP
