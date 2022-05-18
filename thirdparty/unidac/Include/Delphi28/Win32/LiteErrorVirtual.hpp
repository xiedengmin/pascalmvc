// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteErrorVirtual.pas' rev: 35.00 (Windows)

#ifndef LiteerrorvirtualHPP
#define LiteerrorvirtualHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <CRTypes.hpp>
#include <Data.DB.hpp>
#include <DBAccess.hpp>
#include <LiteCallVirtual.hpp>

//-- user supplied -----------------------------------------------------------

namespace Liteerrorvirtual
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS ESQLiteError;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION ESQLiteError : public Dbaccess::EDAError
{
	typedef Dbaccess::EDAError inherited;
	
public:
	__fastcall ESQLiteError(int ErrorCode, const System::UnicodeString ErrorMsg);
	__fastcall virtual ~ESQLiteError();
	virtual bool __fastcall IsFatalError();
	virtual bool __fastcall IsKeyViolation();
public:
	/* Exception.CreateFmt */ inline __fastcall ESQLiteError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Dbaccess::EDAError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESQLiteError(NativeUInt Ident)/* overload */ : Dbaccess::EDAError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESQLiteError(System::PResStringRec ResStringRec)/* overload */ : Dbaccess::EDAError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESQLiteError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Dbaccess::EDAError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESQLiteError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Dbaccess::EDAError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESQLiteError(const System::UnicodeString Msg, int AHelpContext) : Dbaccess::EDAError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESQLiteError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Dbaccess::EDAError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESQLiteError(NativeUInt Ident, int AHelpContext)/* overload */ : Dbaccess::EDAError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESQLiteError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Dbaccess::EDAError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESQLiteError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Dbaccess::EDAError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESQLiteError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Dbaccess::EDAError(Ident, Args, Args_High, AHelpContext) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Liteerrorvirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITEERRORVIRTUAL)
using namespace Liteerrorvirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LiteerrorvirtualHPP
