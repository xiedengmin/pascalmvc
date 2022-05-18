// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRColFrame.pas' rev: 34.00 (Windows)

#ifndef CrcolframeHPP
#define CrcolframeHPP

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
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crcolframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRColFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCRColFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Stdctrls::TListBox* lbItemName;
	Vcl::Extctrls::TPanel* PanelItem;
	void __fastcall lbItemNameClick(System::TObject* Sender);
	
protected:
	int FOldItemIndex;
	bool FInStoreItem;
	bool FInSelectItem;
	virtual System::Classes::TCollection* __fastcall GetItems();
	virtual System::UnicodeString __fastcall GetItemName(System::Classes::TCollectionItem* Item);
	virtual void __fastcall InitItems();
	void __fastcall StoreItem();
	bool __fastcall IsControlEnabled(Vcl::Controls::TControl* Control);
	virtual void __fastcall ItemToControls(System::Classes::TCollectionItem* Item);
	virtual void __fastcall ControlsToItem(System::Classes::TCollectionItem* Item);
	HIDESBASE void __fastcall UpdateControlState(Vcl::Controls::TControl* Control);
	virtual void __fastcall UpdateControlsState();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	__property System::Classes::TCollection* Items = {read=GetItems};
	
public:
	void __fastcall SelectItem();
public:
	/* TCustomFrame.Create */ inline __fastcall virtual TCRColFrame(System::Classes::TComponent* AOwner) : Crframe::TCRFrame(AOwner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TCRColFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCRColFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crcolframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRCOLFRAME)
using namespace Crcolframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrcolframeHPP
