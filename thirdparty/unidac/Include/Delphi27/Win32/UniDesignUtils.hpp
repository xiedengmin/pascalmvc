// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniDesignUtils.pas' rev: 34.00 (Windows)

#ifndef UnidesignutilsHPP
#define UnidesignutilsHPP

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
#include <DBAccess.hpp>
#include <DADesignUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unidesignutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniDesignUtils;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniDesignUtils : public Dadesignutils::TDADesignUtils
{
	typedef Dadesignutils::TDADesignUtils inherited;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetProjectName();
	__classmethod virtual System::TObject* __fastcall GetConnectionList();
	__classmethod virtual System::UnicodeString __fastcall GetTableName(Dbaccess::TCustomDADataSet* Obj);
	__classmethod virtual void __fastcall SetTableName(Dbaccess::TCustomDADataSet* Obj, const System::UnicodeString Value);
	__classmethod virtual System::UnicodeString __fastcall GetOrderFields(Dbaccess::TCustomDADataSet* Obj);
	__classmethod virtual void __fastcall SetOrderFields(Dbaccess::TCustomDADataSet* Obj, const System::UnicodeString Value);
	__classmethod virtual void __fastcall PrepareSQL(Dbaccess::TCustomDADataSet* Obj);
	__classmethod virtual System::UnicodeString __fastcall GetStoredProcName(Dbaccess::TCustomDADataSet* Obj);
	__classmethod virtual void __fastcall SetStoredProcName(Dbaccess::TCustomDADataSet* Obj, const System::UnicodeString Value);
public:
	/* TObject.Create */ inline __fastcall TUniDesignUtils() : Dadesignutils::TDADesignUtils() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TUniDesignUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unidesignutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIDESIGNUTILS)
using namespace Unidesignutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnidesignutilsHPP
