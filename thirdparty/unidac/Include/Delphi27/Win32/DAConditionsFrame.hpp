// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAConditionsFrame.pas' rev: 34.00 (Windows)

#ifndef DaconditionsframeHPP
#define DaconditionsframeHPP

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

namespace Daconditionsframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAConditionsFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAConditionsFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Stdctrls::TLabel* lbCName;
	Vcl::Stdctrls::TListBox* lbItemName;
	Vcl::Extctrls::TPanel* PanelItem;
	Vcl::Stdctrls::TLabel* lbCValue;
	Vcl::Stdctrls::TLabel* lbConditionEnable;
	Vcl::Stdctrls::TMemo* meConditionValue;
	Vcl::Stdctrls::TCheckBox* cbConditionEnable;
	Vcl::Stdctrls::TButton* btnAdd;
	Vcl::Stdctrls::TButton* btnRemove;
	void __fastcall cbConditionEnableClick(System::TObject* Sender);
	void __fastcall meConditionValueExit(System::TObject* Sender);
	void __fastcall btnAddClick(System::TObject* Sender);
	void __fastcall btnRemoveClick(System::TObject* Sender);
	void __fastcall lbItemNameClick(System::TObject* Sender);
	
protected:
	int FOldItemIndex;
	bool FInStoreItem;
	bool FInSelectItem;
	Dbaccess::TDAConditions* __fastcall GetItems();
	Dbaccess::TDAConditions* __fastcall GetConditions();
	System::UnicodeString __fastcall GetItemName(Dbaccess::TDACondition* Item);
	void __fastcall ItemToControls(Dbaccess::TDACondition* Item);
	void __fastcall ControlsToItem(Dbaccess::TDACondition* Item);
	bool __fastcall IsControlEnabled(Vcl::Controls::TControl* Control);
	HIDESBASE void __fastcall UpdateControlState(Vcl::Controls::TControl* Control);
	void __fastcall UpdateControlsState();
	void __fastcall InitItems();
	void __fastcall StoreItem();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	__property Dbaccess::TDAConditions* Conditions = {read=GetConditions};
	__property Dbaccess::TDAConditions* Items = {read=GetItems};
	
public:
	void __fastcall SelectItem();
public:
	/* TCustomFrame.Create */ inline __fastcall virtual TDAConditionsFrame(System::Classes::TComponent* AOwner) : Crframe::TCRFrame(AOwner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDAConditionsFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAConditionsFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daconditionsframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DACONDITIONSFRAME)
using namespace Daconditionsframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaconditionsframeHPP
