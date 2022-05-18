// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniAlerter.pas' rev: 34.00 (Windows)

#ifndef UnialerterHPP
#define UnialerterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <Data.DB.hpp>
#include <CRTypes.hpp>
#include <CRAccess.hpp>
#include <MemUtils.hpp>
#include <MemData.hpp>
#include <MemDS.hpp>
#include <DBAccess.hpp>
#include <DAAlerter.hpp>
#include <Uni.hpp>
#include <UniProvider.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unialerter
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniAlerter;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniAlerter : public Daalerter::TDAAlerter
{
	typedef Daalerter::TDAAlerter inherited;
	
private:
	Uni::TSpecificOptionsHolder* FSpecificOptions;
	Uni::TUniConnection* __fastcall GetConnection();
	HIDESBASE void __fastcall SetConnection(Uni::TUniConnection* Value);
	Uni::TUniTransaction* __fastcall GetUniTransaction();
	void __fastcall SetUniTransaction(Uni::TUniTransaction* Value);
	Uni::TSpecificOptionsList* __fastcall GetSpecificOptions();
	void __fastcall SetSpecificOptions(Uni::TSpecificOptionsList* Value);
	
protected:
	bool __fastcall CanGetProvider();
	Uniprovider::TUniProvider* __fastcall GetProvider();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual Craccess::TCRAlerterClass __fastcall GetInternalAlerterClass();
	virtual void __fastcall SetInternalAlerter(Craccess::TCRAlerter* Value);
	virtual void __fastcall BeginConnection(bool WithTransaction = true);
	
public:
	__fastcall virtual TUniAlerter(System::Classes::TComponent* Owner);
	__fastcall virtual ~TUniAlerter();
	
__published:
	__property Uni::TUniConnection* Connection = {read=GetConnection, write=SetConnection};
	__property Uni::TUniTransaction* Transaction = {read=GetUniTransaction, write=SetUniTransaction, stored=IsTransactionStored};
	__property Uni::TSpecificOptionsList* SpecificOptions = {read=GetSpecificOptions, write=SetSpecificOptions};
	__property Events = {default=0};
	__property Active = {default=0};
	__property AutoRegister = {default=0};
	__property OnEvent;
	__property OnError;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unialerter */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIALERTER)
using namespace Unialerter;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnialerterHPP
