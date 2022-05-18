// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DacReg.pas' rev: 34.00 (Windows)

#ifndef DacregHPP
#define DacregHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dacreg
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
extern DELPHI_PACKAGE void __fastcall RegisterCRBatchMove(void);
extern DELPHI_PACKAGE void __fastcall RegisterSplashScreen(System::UnicodeString ProductName, System::UnicodeString ProductVersion, unsigned HProductIco, bool IsTrial, System::UnicodeString LicensedType);
extern DELPHI_PACKAGE void __fastcall RegisterAboutBox(System::UnicodeString ProductName, System::UnicodeString ProductVersion, System::UnicodeString ProductPage, System::UnicodeString AboutDescription, unsigned HProductIco, bool IsTrial, System::UnicodeString LicensedType, System::UnicodeString Edition);
extern DELPHI_PACKAGE void __fastcall UnregisterAboutBox(void);
}	/* namespace Dacreg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DACREG)
using namespace Dacreg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DacregHPP
