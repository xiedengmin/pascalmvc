// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRDesignUtils.pas' rev: 34.00 (Windows)

#ifndef CrdesignutilsHPP
#define CrdesignutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crdesignutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRDesignUtils;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRDesignUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetProjectName();
	__classmethod virtual bool __fastcall GetDesignCreate(System::Classes::TComponent* Obj);
	__classmethod virtual void __fastcall SetDesignCreate(System::Classes::TComponent* Obj, bool Value);
	__classmethod virtual int __fastcall SQLDialect();
	__classmethod virtual bool __fastcall DBToolsAvailable();
public:
	/* TObject.Create */ inline __fastcall TCRDesignUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCRDesignUtils() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TCRDesignUtilsClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crdesignutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRDESIGNUTILS)
using namespace Crdesignutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrdesignutilsHPP
