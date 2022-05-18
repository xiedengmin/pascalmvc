// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRSspi.pas' rev: 34.00 (Windows)

#ifndef CrsspiHPP
#define CrsspiHPP

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
#include <CRTypes.hpp>
#include <CLRClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crsspi
{
//-- forward type declarations -----------------------------------------------
struct TSecHandle;
struct TSecBuffer;
struct TSecBufferDesc;
struct SecPkgInfo;
class DELPHICLASS TCRNTLMAuth;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TSecurityProvider : unsigned char { spNegotiate, spKerberos, spNTLM };

typedef System::Set<TSecurityProvider, TSecurityProvider::spNegotiate, TSecurityProvider::spNTLM> TSecurityProviders;

struct DECLSPEC_DRECORD TSecHandle
{
public:
	NativeUInt dwLower;
	NativeUInt dwUpper;
};


typedef TSecHandle *PSecHandle;

struct DECLSPEC_DRECORD TSecBuffer
{
public:
	unsigned cbBuffer;
	unsigned BufferType;
	void *pvBuffer;
};


typedef TSecBuffer *PSecBuffer;

struct DECLSPEC_DRECORD TSecBufferDesc
{
public:
	unsigned ulVersion;
	unsigned cBuffers;
	TSecBuffer *pBuffers;
};


typedef TSecBufferDesc *PSecBufferDesc;

typedef System::DynamicArray<TSecBuffer> TSecBufferArray;

struct DECLSPEC_DRECORD SecPkgInfo
{
public:
	unsigned fCapabilities;
	System::Word wVersion;
	System::Word wRPCID;
	unsigned cbMaxToken;
	System::WideChar *Name;
	System::WideChar *Comment;
};


typedef SecPkgInfo *PSecPkgInfo;

typedef int __stdcall (*TQuerySecurityPackageInfo)(System::WideChar * pszPackageName, PSecPkgInfo &ppPackageInfo);

typedef int __stdcall (*TAcquireCredentialsHandle)(System::WideChar * pszPrincipal, System::WideChar * pszPackage, unsigned fCredentialUse, void * pvLogonId, void * pAuthData, void * pGetKeyFn, void * pvGetKeyArgument, PSecHandle phCredential, System::Sysutils::TTimeStamp &ptsExpiry);

typedef int __stdcall (*TInitializeSecurityContext)(PSecHandle phCredential, PSecHandle phContext, System::WideChar * pszTargetName, unsigned fContextReq, unsigned Reserved1, unsigned TargetDataRep, PSecBufferDesc pInput, unsigned Reserved2, PSecHandle phNewContext, PSecBufferDesc pOutput, unsigned &pfContextAttr, void * ptsExpiry);

typedef int __stdcall (*TDeleteSecurityContext)(PSecHandle phContext);

typedef int __stdcall (*TFreeCredentialsHandle)(PSecHandle phCredential);

typedef int __stdcall (*TFreeContextBuffer)(void * pvContextBuffer);

typedef bool __stdcall (*TGetUserNameEx)(const System::Byte NameFormat, System::WideChar * lpNameBuffer, unsigned &lpnSize);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRNTLMAuth : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSecurityProvider FUsedPackage;
	unsigned FMaxMessageLen;
	TSecHandle FCredNegotiate;
	TSecHandle FSslCtxHandle;
	TQuerySecurityPackageInfo FQuerySecurityPackageInfo;
	TAcquireCredentialsHandle FAcquireCredentialsHandle;
	TFreeContextBuffer FFreeContextBuffer;
	TInitializeSecurityContext FInitializeSecurityContext;
	TFreeCredentialsHandle FFreeCredentialsHandle;
	TDeleteSecurityContext FDeleteSecurityContext;
	TGetUserNameEx FGetUserNameEx;
	System::UnicodeString __fastcall GetUsedProtocolName();
	
protected:
	void __fastcall Release();
	virtual int __fastcall RegisterSecureDllMethods();
	System::UnicodeString __fastcall SecurityProtocolName(TSecurityProvider Protocol);
	__property TSecurityProvider UsedPackage = {read=FUsedPackage, write=FUsedPackage, nodefault};
	__property unsigned MaxMessageLen = {read=FMaxMessageLen, write=FMaxMessageLen, nodefault};
	__property TSecHandle CredNegotiate = {read=FCredNegotiate, write=FCredNegotiate};
	__property TSecHandle SslCtxHandle = {read=FSslCtxHandle, write=FSslCtxHandle};
	__property TQuerySecurityPackageInfo QuerySecurityPackageInfo = {read=FQuerySecurityPackageInfo, write=FQuerySecurityPackageInfo};
	__property TAcquireCredentialsHandle AcquireCredentialsHandle = {read=FAcquireCredentialsHandle, write=FAcquireCredentialsHandle};
	__property TFreeContextBuffer FreeContextBuffer = {read=FFreeContextBuffer, write=FFreeContextBuffer};
	__property TInitializeSecurityContext InitializeSecurityContext = {read=FInitializeSecurityContext, write=FInitializeSecurityContext};
	__property TFreeCredentialsHandle FreeCredentialsHandle = {read=FFreeCredentialsHandle, write=FFreeCredentialsHandle};
	__property TDeleteSecurityContext DeleteSecurityContext = {read=FDeleteSecurityContext, write=FDeleteSecurityContext};
	__property TGetUserNameEx GetUserNameEx = {read=FGetUserNameEx, write=FGetUserNameEx};
	
public:
	__fastcall TCRNTLMAuth();
	__fastcall virtual ~TCRNTLMAuth();
	virtual int __fastcall Initialize();
	int __fastcall StartAuthentication(/* out */ System::DynamicArray<System::Byte> &AuthData);
	int __fastcall FinishAuthentication(const System::DynamicArray<System::Byte> InAuthData, /* out */ System::DynamicArray<System::Byte> &OutAuthData);
	bool __fastcall GetUserName(const System::Byte NameFormat, System::WideChar * lpNameBuffer, unsigned &lpnSize);
	__property System::UnicodeString UsedProtocolName = {read=GetUsedProtocolName};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE NativeUInt hSecure32Lib;
}	/* namespace Crsspi */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRSSPI)
using namespace Crsspi;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrsspiHPP
