// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualQueryDesignUtils.pas' rev: 35.00 (Windows)

#ifndef VirtualquerydesignutilsHPP
#define VirtualquerydesignutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Dialogs.hpp>
#include <CRDataTypeMap.hpp>
#include <DBAccess.hpp>
#include <DADesignUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualquerydesignutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVirtualDesignUtils;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualDesignUtils : public Dadesignutils::TDADesignUtils
{
	typedef Dadesignutils::TDADesignUtils inherited;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetProjectName();
	__classmethod virtual Crdatatypemap::TConverterManagerClass __fastcall GetConverterManagerClass();
public:
	/* TObject.Create */ inline __fastcall TVirtualDesignUtils() : Dadesignutils::TDADesignUtils() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualDesignUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Virtualquerydesignutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALQUERYDESIGNUTILS)
using namespace Virtualquerydesignutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualquerydesignutilsHPP
