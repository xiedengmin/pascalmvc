// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniConnectionEditor.pas' rev: 35.00 (Windows)

#ifndef UniconnectioneditorHPP
#define UniconnectioneditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <DacVcl.hpp>
#include <UniDacVcl.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.Mask.hpp>
#include <Vcl.ValEdit.hpp>
#include <CRTypes.hpp>
#include <DBAccess.hpp>
#include <Uni.hpp>
#include <CRFrame.hpp>
#include <CREditor.hpp>
#include <DAConnectionEditor.hpp>
#include <DADataTypeMapFrame.hpp>
#include <UniMacrosFrame.hpp>
#include <UniSpecificOptionsFrame.hpp>
#include <CRTabEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniconnectioneditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniConnectionEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniConnectionEditorForm : public Daconnectioneditor::TDAConnectionEditorForm
{
	typedef Daconnectioneditor::TDAConnectionEditorForm inherited;
	
__published:
	Vcl::Stdctrls::TLabel* lbProvider;
	Vcl::Stdctrls::TComboBox* edProvider;
	Vcl::Stdctrls::TLabel* lbPort;
	Vcl::Stdctrls::TLabel* lbDatabase;
	Vcl::Stdctrls::TComboBox* edDatabase;
	Vcl::Comctrls::TTabSheet* shOptions;
	Vcl::Comctrls::TTabSheet* shMacros;
	Vcl::Stdctrls::TEdit* edPort;
	void __fastcall edProviderChange(System::TObject* Sender);
	void __fastcall edDatabaseExit(System::TObject* Sender);
	void __fastcall edPortExit(System::TObject* Sender);
	void __fastcall edPortChange(System::TObject* Sender);
	HIDESBASE void __fastcall PageControlChange(System::TObject* Sender);
	void __fastcall edDatabaseDropDown(System::TObject* Sender);
	void __fastcall edDatabaseKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	HIDESBASE void __fastcall FormShow(System::TObject* Sender);
	void __fastcall edServerSelect(System::TObject* Sender);
	
protected:
	Unimacrosframe::TUniMacrosFrame* FMacrosFrame;
	Unispecificoptionsframe::TUniSpecificOptionsFrame* FOptionsFrame;
	Uni::TUniConnection* __fastcall GetConnection();
	Uni::TUniConnection* __fastcall GetLocalConnection();
	virtual Dadatatypemapframe::TDADataTypeMapFrameClass __fastcall GetDataTypeMapFrameClass();
	virtual void __fastcall DoInit();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoSaveConnection();
	virtual void __fastcall FillInfo(Dbaccess::TCustomDAConnection* InfoConnection);
	void __fastcall InitSpecificOption(System::UnicodeString ProviderName);
	void __fastcall AssignProvider(const System::UnicodeString Value);
	void __fastcall AssignDataBase(const System::UnicodeString Value);
	void __fastcall AssignPort(const int Value);
	virtual void __fastcall AddServerToList();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	virtual void __fastcall ConnToControls();
	virtual void __fastcall ControlsToConn();
	virtual void __fastcall ShowState(bool Yellow = false);
	void __fastcall SetupForSpecificOptions();
	void __fastcall ReplaceEdit(Vcl::Controls::TWinControl* &Edit);
	void __fastcall SpinEditKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	
public:
	__property Uni::TUniConnection* Connection = {read=GetConnection};
	__property Uni::TUniConnection* LocalConnection = {read=GetLocalConnection};
public:
	/* TDAConnectionEditorForm.Create */ inline __fastcall virtual TUniConnectionEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Daconnectioneditor::TDAConnectionEditorForm(Owner, CRDesignUtilsClass) { }
	/* TDAConnectionEditorForm.Destroy */ inline __fastcall virtual ~TUniConnectionEditorForm() { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniConnectionEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Daconnectioneditor::TDAConnectionEditorForm(AOwner, Dummy) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniConnectionEditorForm(HWND ParentWindow) : Daconnectioneditor::TDAConnectionEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TUniConnectionEditorForm* UniConnectionEditorForm;
}	/* namespace Uniconnectioneditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNICONNECTIONEDITOR)
using namespace Uniconnectioneditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniconnectioneditorHPP
