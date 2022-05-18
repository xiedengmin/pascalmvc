// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAMacrosFrame.pas' rev: 35.00 (Windows)

#ifndef DamacrosframeHPP
#define DamacrosframeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <DBAccess.hpp>
#include <CRFrame.hpp>
#include <CRColFrame.hpp>
#include <CRTabEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Damacrosframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAMacrosFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAMacrosFrame : public Crcolframe::TCRColFrame
{
	typedef Crcolframe::TCRColFrame inherited;
	
__published:
	Vcl::Stdctrls::TLabel* lbMName;
	Vcl::Stdctrls::TLabel* lbMacroLog;
	Vcl::Stdctrls::TLabel* lbMValue;
	Vcl::Stdctrls::TMemo* meMacroValue;
	Vcl::Stdctrls::TCheckBox* cbMacroActive;
	Vcl::Stdctrls::TLabel* lbActive;
	void __fastcall cbMacroActiveClick(System::TObject* Sender);
	void __fastcall meMacroValueExit(System::TObject* Sender);
	
protected:
	virtual System::Classes::TCollection* __fastcall GetItems();
	Dbaccess::TMacros* __fastcall GetMacros();
	virtual System::UnicodeString __fastcall GetItemName(System::Classes::TCollectionItem* Item);
	virtual void __fastcall ItemToControls(System::Classes::TCollectionItem* Item);
	virtual void __fastcall ControlsToItem(System::Classes::TCollectionItem* Item);
	__property Dbaccess::TMacros* Macros = {read=GetMacros};
public:
	/* TCustomFrame.Create */ inline __fastcall virtual TDAMacrosFrame(System::Classes::TComponent* AOwner) : Crcolframe::TCRColFrame(AOwner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDAMacrosFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAMacrosFrame(HWND ParentWindow) : Crcolframe::TCRColFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Damacrosframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAMACROSFRAME)
using namespace Damacrosframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DamacrosframeHPP
