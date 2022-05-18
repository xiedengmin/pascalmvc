// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniDump.pas' rev: 34.00 (Windows)

#ifndef UnidumpHPP
#define UnidumpHPP

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
#include <MemData.hpp>
#include <MemDS.hpp>
#include <CRAccess.hpp>
#include <DBAccess.hpp>
#include <DADump.hpp>
#include <DAScript.hpp>
#include <Uni.hpp>
#include <UniProvider.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unidump
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniDump;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniDump : public Dadump::TDADump
{
	typedef Dadump::TDADump inherited;
	
private:
	Uni::TSpecificOptionsHolder* FSpecificOptions;
	Uni::TUniConnection* __fastcall GetConnection();
	HIDESBASE void __fastcall SetConnection(Uni::TUniConnection* Value);
	Uni::TSpecificOptionsList* __fastcall GetSpecificOptions();
	void __fastcall SetSpecificOptions(Uni::TSpecificOptionsList* Value);
	
protected:
	bool __fastcall CanGetProvider();
	Uniprovider::TUniProvider* __fastcall GetProvider();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual Dadump::TDADumpProcessorClass __fastcall GetProcessorClass();
	virtual void __fastcall SetProcessor(Dadump::TDADumpProcessor* Value);
	virtual System::UnicodeString __fastcall GetTableNames();
	virtual void __fastcall SetTableNames(System::UnicodeString Value);
	virtual Dascript::TDAScript* __fastcall CreateScript();
	virtual System::UnicodeString __fastcall GenerateHeader();
	virtual void __fastcall BeginConnection();
	
public:
	__fastcall virtual TUniDump(System::Classes::TComponent* Owner);
	__fastcall virtual ~TUniDump();
	
__published:
	__property Uni::TUniConnection* Connection = {read=GetConnection, write=SetConnection};
	__property Uni::TSpecificOptionsList* SpecificOptions = {read=GetSpecificOptions, write=SetSpecificOptions};
	__property Options;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unidump */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIDUMP)
using namespace Unidump;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnidumpHPP
