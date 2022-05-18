// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniSQLMonitor.pas' rev: 34.00 (Windows)

#ifndef UnisqlmonitorHPP
#define UnisqlmonitorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Data.DB.hpp>
#include <CRTypes.hpp>
#include <DBAccess.hpp>
#include <DASQLMonitor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unisqlmonitor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniSQLMonitor;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TUniSQLMonitorClass;

class PASCALIMPLEMENTATION TUniSQLMonitor : public Dasqlmonitor::TCustomDASQLMonitor
{
	typedef Dasqlmonitor::TCustomDASQLMonitor inherited;
	
protected:
	__classmethod virtual Dasqlmonitor::TCustomDASQLMonitor* __fastcall GetMonitor();
	virtual void __fastcall SetMonitor();
	
public:
	__fastcall virtual TUniSQLMonitor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TUniSQLMonitor();
	__classmethod virtual System::UnicodeString __fastcall GetCaption();
	
__published:
	__property Active = {default=1};
	__property Options = {default=15};
	__property DBMonitorOptions;
	__property TraceFlags = {default=1643};
	__property OnSQL;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unisqlmonitor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISQLMONITOR)
using namespace Unisqlmonitor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnisqlmonitorHPP
