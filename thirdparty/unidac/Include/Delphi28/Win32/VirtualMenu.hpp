// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualMenu.pas' rev: 35.00 (Windows)

#ifndef VirtualmenuHPP
#define VirtualmenuHPP

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

namespace Virtualmenu
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVirtualMenu;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TVirtualMenu : public Damenu::TDAProductMenu
{
	typedef Damenu::TDAProductMenu inherited;
	
private:
	void __fastcall HomePageItemClick(System::TObject* Sender);
	void __fastcall VirtualDacPageItemClick(System::TObject* Sender);
	void __fastcall AboutItemClick(System::TObject* Sender);
	
public:
	__fastcall TVirtualMenu();
	virtual bool __fastcall AddItems(NativeUInt Instance);
public:
	/* TDAMenu.Destroy */ inline __fastcall virtual ~TVirtualMenu() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TVirtualMenu* Menu;
}	/* namespace Virtualmenu */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALMENU)
using namespace Virtualmenu;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualmenuHPP
