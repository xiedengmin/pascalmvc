// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MTSCall.pas' rev: 34.00 (Windows)

#ifndef MtscallHPP
#define MtscallHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mtscall
{
//-- forward type declarations -----------------------------------------------
struct CRBOID;
struct CRXACTOPT;
struct CRXACTTRANSINFO;
__interface DELPHIINTERFACE ICRTransaction;
typedef System::DelphiInterface<ICRTransaction> _di_ICRTransaction;
__interface DELPHIINTERFACE ICRTransactionOptions;
typedef System::DelphiInterface<ICRTransactionOptions> _di_ICRTransactionOptions;
__interface DELPHIINTERFACE ICRTransactionSC;
typedef System::DelphiInterface<ICRTransactionSC> _di_ICRTransactionSC;
__interface DELPHIINTERFACE ICRTransactionOptionsSC;
typedef System::DelphiInterface<ICRTransactionOptionsSC> _di_ICRTransactionOptionsSC;
__interface DELPHIINTERFACE ICRTransactionDispenserSC;
typedef System::DelphiInterface<ICRTransactionDispenserSC> _di_ICRTransactionDispenserSC;
//-- type declarations -------------------------------------------------------
typedef CRBOID *PCRBoid;

#pragma pack(push,1)
struct DECLSPEC_DRECORD CRBOID
{
public:
	System::StaticArray<System::Byte, 16> rgb_;
};
#pragma pack(pop)


typedef CRBOID TCRBoid;

typedef CRXACTOPT *PCRXactOpt;

#pragma pack(push,1)
struct DECLSPEC_DRECORD CRXACTOPT
{
public:
	unsigned ulTimeout;
	System::StaticArray<System::Int8, 40> szDescription;
};
#pragma pack(pop)


typedef CRXACTOPT TCRXActOpt;

typedef CRXACTTRANSINFO *PCRXactTransInfo;

#pragma pack(push,1)
struct DECLSPEC_DRECORD CRXACTTRANSINFO
{
public:
	CRBOID uow;
	int isoLevel;
	unsigned isoFlags;
	unsigned grfTCSupported;
	unsigned grfRMSupported;
	unsigned grfTCSupportedRetaining;
	unsigned grfRMSupportedRetaining;
};
#pragma pack(pop)


typedef CRXACTTRANSINFO TCRXactTransInfo;

__interface  INTERFACE_UUID("{0FB15084-AF41-11CE-BD2B-204C4F4F5020}") ICRTransaction  : public System::IInterface 
{
	virtual HRESULT __stdcall Commit(System::LongBool fRetaining, unsigned grfTC, unsigned grfRM) = 0 ;
	virtual HRESULT __stdcall Abort(PCRBoid pboidReason, System::LongBool fRetaining, System::LongBool fAsync) = 0 ;
	virtual HRESULT __stdcall GetTransactionInfo(/* out */ CRXACTTRANSINFO &pinfo) = 0 ;
};

__interface  INTERFACE_UUID("{3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}") ICRTransactionOptions  : public System::IInterface 
{
	virtual HRESULT __stdcall SetOptions(CRXACTOPT &pOptions) = 0 ;
	virtual HRESULT __stdcall GetOptions(CRXACTOPT &pOptions) = 0 ;
};

__interface  INTERFACE_UUID("{0FB15084-AF41-11CE-BD2B-204C4F4F5020}") ICRTransactionSC  : public System::IInterface 
{
	virtual HRESULT __safecall Commit(System::LongBool fRetaining, unsigned grfTC, unsigned grfRM) = 0 ;
	virtual HRESULT __safecall Abort(PCRBoid pboidReason, System::LongBool fRetaining, System::LongBool fAsync) = 0 ;
	virtual HRESULT __safecall GetTransactionInfo(/* out */ CRXACTTRANSINFO &pinfo) = 0 ;
};

__interface  INTERFACE_UUID("{3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}") ICRTransactionOptionsSC  : public System::IInterface 
{
	virtual HRESULT __safecall SetOptions(CRXACTOPT &pOptions) = 0 ;
	virtual HRESULT __safecall GetOptions(CRXACTOPT &pOptions) = 0 ;
};

__interface  INTERFACE_UUID("{3A6AD9E1-23B9-11CF-AD60-00AA00A74CCD}") ICRTransactionDispenserSC  : public System::IInterface 
{
	virtual HRESULT __safecall GetOptionsObject(/* out */ _di_ICRTransactionOptions &ppOptions) = 0 ;
	virtual HRESULT __safecall BeginTransaction(const System::_di_IInterface punkOuter, int isoLevel, unsigned isoFlags, const _di_ICRTransactionOptions pOptions, /* out */ _di_ICRTransaction &ppTransaction) = 0 ;
};

typedef int __cdecl (*_DtcGetTransactionManagerEx)(char * pszHost, char * pszTmName, const GUID &riid, int grfOptions, void * pvConfigParams, /* out */ _di_ICRTransactionDispenserSC &ppvObject);

//-- var, const, procedure ---------------------------------------------------
static const System::Word ISOLATIONLEVEL_READUNCOMMITTED = System::Word(0x100);
static const System::Word ISOLATIONLEVEL_READCOMMITTED = System::Word(0x1000);
static const int ISOLATIONLEVEL_REPEATABLEREAD = int(0x10000);
static const int ISOLATIONLEVEL_SERIALIZABLE = int(0x100000);
extern DELPHI_PACKAGE GUID IID_ITransactionDispenser;
extern DELPHI_PACKAGE _DtcGetTransactionManagerEx DtcGetTransactionManagerEx;
extern DELPHI_PACKAGE void __fastcall InitMSDTC(void);
extern DELPHI_PACKAGE void __fastcall FreeMSDTC(void);
}	/* namespace Mtscall */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MTSCALL)
using namespace Mtscall;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MtscallHPP
