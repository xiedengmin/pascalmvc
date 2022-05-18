// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAMenu.pas' rev: 35.00 (Windows)

#ifndef DamenuHPP
#define DamenuHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ToolsAPI.hpp>
#include <Vcl.Menus.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Damenu
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAMenuItem;
class DELPHICLASS TDAMenu;
class DELPHICLASS TDAProductMenu;
//-- type declarations -------------------------------------------------------
typedef Vcl::Menus::TMenuItem TAPIMenuItem;

typedef System::TObject TDAMenuClickSender;

typedef void __fastcall (__closure *TDAMenuClickEvent)(System::TObject* Sender);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAMenuItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
protected:
	TDAMenu* FSubMenu;
	System::UnicodeString FCaption;
	System::UnicodeString FName;
	bool FVisible;
	bool FRadioItem;
	bool FChecked;
	bool FNeedDestroyMenuItem;
	Vcl::Menus::TMenuItem* FMenuItem;
	void __fastcall SetCaption(System::UnicodeString Value);
	void __fastcall SetVisible(bool Value);
	void __fastcall SetRadioItem(bool Value);
	void __fastcall SetChecked(bool Value);
	
public:
	__fastcall TDAMenuItem(TDAMenu* DAMenu, Vcl::Menus::TMenuItem* MenuItem, bool NeedDestroyItem)/* overload */;
	__classmethod TDAMenuItem* __fastcall CreateMenuItem(TDAMenu* DAMenu, System::UnicodeString Caption, System::UnicodeString Name, TDAMenuClickEvent ClickEvent = 0x0, int Index = 0xffffffff);
	__fastcall virtual ~TDAMenuItem();
	__property TDAMenu* SubMenu = {read=FSubMenu};
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property System::UnicodeString Name = {read=FName};
	__property bool Visible = {read=FVisible, write=SetVisible, nodefault};
	__property bool RadioItem = {read=FRadioItem, write=SetRadioItem, nodefault};
	__property bool Checked = {read=FChecked, write=SetChecked, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAMenu : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
protected:
	TDAMenuItem* FParentItem;
	NativeUInt FHInstance;
	Vcl::Menus::TMenuItem* FMenuItems;
	System::UnicodeString FProjectName;
	bool FSubMenuProcessed;
	System::UnicodeString FFAQName;
	int FWizardPosition;
	bool FUseCHM;
	void __fastcall HelpItemClick(System::TObject* Sender);
	void __fastcall FAQItemClick(System::TObject* Sender);
	
public:
	__fastcall TDAMenu(TDAMenuItem* ParentItem, const bool AProcessSubMenu);
	__fastcall virtual ~TDAMenu();
	TDAMenuItem* __fastcall AddSeparator();
	TDAMenuItem* __fastcall AddFAQ(System::UnicodeString Caption, System::UnicodeString Name, System::UnicodeString ProjectName);
	TDAMenuItem* __fastcall AddHelp(System::UnicodeString Caption, System::UnicodeString Name, System::UnicodeString ProjectName, bool UseCHM = true);
	void __fastcall AddWizards();
	HIDESBASE TDAMenuItem* __fastcall Add(System::UnicodeString Caption, System::UnicodeString Name, TDAMenuClickEvent ClickEvent = 0x0, int Index = 0xffffffff);
	void __fastcall ProcessSubMenu();
	void __fastcall GetSubMenu();
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TDAProductMenu : public TDAMenu
{
	typedef TDAMenu inherited;
	
protected:
	System::UnicodeString FCRMenuName;
	System::UnicodeString FServerMenuCaption;
	System::UnicodeString FProductMenuCaption;
	System::UnicodeString FAboutCaption;
	System::UnicodeString FAboutName;
	System::UnicodeString FProductName;
	System::UnicodeString FProductEdition;
	System::UnicodeString FProductVersion;
	TDAMenuClickEvent FAboutClickEvent;
	TDAMenuItem* FCRMenu;
	bool FHasProduct;
	void __fastcall CheckUpdatesItemClick(System::TObject* Sender);
	void __fastcall Prepare();
	HIDESBASE TDAMenu* __fastcall GetSubMenu();
	__property TDAMenu* SubMenu = {read=GetSubMenu};
	
public:
	__fastcall TDAProductMenu(const System::UnicodeString ACRMenuName, const System::UnicodeString AAboutCaption, const System::UnicodeString AAboutName, const System::UnicodeString AServerMenuCaption, const System::UnicodeString AProductMenuCaption)/* overload */;
	__fastcall TDAProductMenu()/* overload */;
	virtual bool __fastcall AddItems(NativeUInt Instance);
	TDAMenuItem* __fastcall AddAbout();
	TDAMenuItem* __fastcall AddCheckUpdates(System::UnicodeString ProductName, System::UnicodeString ProductVersion, System::UnicodeString ProductEdition);
	__property System::UnicodeString CRMenuName = {read=FCRMenuName};
	__property System::UnicodeString ServerMenuCaption = {read=FServerMenuCaption};
	__property System::UnicodeString ProductMenuCaption = {read=FProductMenuCaption};
public:
	/* TDAMenu.Destroy */ inline __fastcall virtual ~TDAProductMenu() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString __fastcall RemoveAmpersands(System::UnicodeString RawCaption);
}	/* namespace Damenu */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAMENU)
using namespace Damenu;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DamenuHPP
