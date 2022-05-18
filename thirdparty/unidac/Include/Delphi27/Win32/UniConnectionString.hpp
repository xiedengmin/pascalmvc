// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniConnectionString.pas' rev: 34.00 (Windows)

#ifndef UniconnectionstringHPP
#define UniconnectionstringHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>
#include <CRAccess.hpp>
#include <CRVio.hpp>
#include <CRConnectionString.hpp>
#include <UniConsts.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniconnectionstring
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniConnectionStringBuilder;
class DELPHICLASS TUniConnectionDefStringBuilder;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniConnectionStringBuilder : public Crconnectionstring::TCRConnectionStringBuilder
{
	typedef Crconnectionstring::TCRConnectionStringBuilder inherited;
	
protected:
	virtual void __fastcall InitParams();
	virtual void __fastcall SetParamValue(Crconnectionstring::TConnectionStringParam* Param, const System::Variant &Value);
	virtual void __fastcall InitExtStringBuilder();
	
public:
	__fastcall virtual TUniConnectionStringBuilder(Crconnectionstring::TGetConnectionStringParamMethod GetPropMethod, Crconnectionstring::TSetConnectionStringParamMethod SetPropMethod);
	__property ExtStringBuilderClass;
public:
	/* TConnectionStringBuilder.Destroy */ inline __fastcall virtual ~TUniConnectionStringBuilder() { }
	
};


class PASCALIMPLEMENTATION TUniConnectionDefStringBuilder : public Crconnectionstring::TCRConnectionStringBuilder
{
	typedef Crconnectionstring::TCRConnectionStringBuilder inherited;
	
protected:
	virtual void __fastcall InitParams();
public:
	/* TConnectionStringBuilder.Create */ inline __fastcall virtual TUniConnectionDefStringBuilder(Crconnectionstring::TGetConnectionStringParamMethod GetPropMethod, Crconnectionstring::TSetConnectionStringParamMethod SetPropMethod) : Crconnectionstring::TCRConnectionStringBuilder(GetPropMethod, SetPropMethod) { }
	/* TConnectionStringBuilder.Destroy */ inline __fastcall virtual ~TUniConnectionDefStringBuilder() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const short cpProviderName = short(-1001);
}	/* namespace Uniconnectionstring */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNICONNECTIONSTRING)
using namespace Uniconnectionstring;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniconnectionstringHPP
