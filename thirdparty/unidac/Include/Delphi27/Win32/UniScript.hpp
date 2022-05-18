// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniScript.pas' rev: 34.00 (Windows)

#ifndef UniscriptHPP
#define UniscriptHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>
#include <CRAccess.hpp>
#include <DAScript.hpp>
#include <DBAccess.hpp>
#include <Uni.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniscript
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniScript;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniScript : public Dascript::TDAScript
{
	typedef Dascript::TDAScript inherited;
	
private:
	System::Classes::TStrings* FSpecificOptions;
	HIDESBASE Uni::TUniConnection* __fastcall GetConnection();
	HIDESBASE void __fastcall SetConnection(Uni::TUniConnection* Value);
	HIDESBASE Uni::TUniTransaction* __fastcall GetTransaction();
	HIDESBASE void __fastcall SetTransaction(Uni::TUniTransaction* Value);
	HIDESBASE Uni::TCustomUniDataSet* __fastcall GetDataSet();
	HIDESBASE void __fastcall SetDataSet(Uni::TCustomUniDataSet* Value);
	void __fastcall SetSpecificOptions(System::Classes::TStrings* Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual Dascript::TDAScriptProcessorClass __fastcall GetProcessorClass();
	virtual void __fastcall SetProcessor(Dascript::TDAScriptProcessor* Value);
	virtual Dbaccess::TCustomDASQL* __fastcall CreateCommand();
	
public:
	__fastcall virtual TUniScript(System::Classes::TComponent* Owner);
	__fastcall virtual ~TUniScript();
	
__published:
	__property AutoCommit = {default=0};
	__property ScanParams = {default=1};
	__property Uni::TUniConnection* Connection = {read=GetConnection, write=SetConnection};
	__property Uni::TUniTransaction* Transaction = {read=GetTransaction, write=SetTransaction, stored=IsTransactionStored};
	__property Uni::TCustomUniDataSet* DataSet = {read=GetDataSet, write=SetDataSet};
	__property System::Classes::TStrings* SpecificOptions = {read=FSpecificOptions, write=SetSpecificOptions};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uniscript */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISCRIPT)
using namespace Uniscript;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniscriptHPP
