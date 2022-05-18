// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniMenu.pas' rev: 34.00 (Windows)

#ifndef UnimenuHPP
#define UnimenuHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <DAMenu.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unimenu
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniMenu;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniMenu : public Damenu::TDAProductMenu
{
	typedef Damenu::TDAProductMenu inherited;
	
private:
	void __fastcall HomePageItemClick(System::TObject* Sender);
	void __fastcall UniDacPageItemClick(System::TObject* Sender);
	void __fastcall AboutItemClick(System::TObject* Sender);
	void __fastcall DBMonitorItemClick(System::TObject* Sender);
	void __fastcall DBMonitorPageItemClick(System::TObject* Sender);
	
public:
	__fastcall TUniMenu();
	virtual bool __fastcall AddItems(NativeUInt Instance);
public:
	/* TDAMenu.Destroy */ inline __fastcall virtual ~TUniMenu() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TUniMenu* Menu;
}	/* namespace Unimenu */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIMENU)
using namespace Unimenu;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnimenuHPP
