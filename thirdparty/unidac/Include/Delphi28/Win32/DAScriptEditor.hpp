// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAScriptEditor.pas' rev: 35.00 (Windows)

#ifndef DascripteditorHPP
#define DascripteditorHPP

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
#include <Vcl.ComCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <System.SysUtils.hpp>
#include <Data.DB.hpp>
#include <System.Classes.hpp>
#include <CRTypes.hpp>
#include <DBAccess.hpp>
#include <DAScript.hpp>
#include <CREditor.hpp>
#include <CRTabEditor.hpp>
#include <CRFrame.hpp>
#include <DASQLFrame.hpp>
#include <DAMacrosFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dascripteditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAScriptEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAScriptEditorForm : public Crtabeditor::TCRTabEditorForm
{
	typedef Crtabeditor::TCRTabEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shSQL;
	Vcl::Comctrls::TTabSheet* shMacros;
	Vcl::Stdctrls::TButton* btExecute;
	Vcl::Stdctrls::TCheckBox* cbDebug;
	Vcl::Stdctrls::TButton* btOpen;
	Vcl::Stdctrls::TButton* btSave;
	void __fastcall btExecuteClick(System::TObject* Sender);
	void __fastcall cbDebugClick(System::TObject* Sender);
	void __fastcall btOpenClick(System::TObject* Sender);
	void __fastcall btSaveClick(System::TObject* Sender);
	
protected:
	Dasqlframe::TDASQLFrame* FSQLFrame;
	Damacrosframe::TDAMacrosFrame* FMacrosFrame;
	System::Classes::TComponent* FComponent;
	Dascript::TDAScript* FLocalScript;
	Dascript::TAfterStatementExecuteEvent FOldAfterExecute;
	bool FOldDebug;
	void __fastcall AfterExecute(System::TObject* Sender, System::UnicodeString SQL);
	void __fastcall OnError(System::TObject* Sender, System::Sysutils::Exception* E, System::UnicodeString SQL, Dascript::TErrorAction &Action);
	virtual void __fastcall DoInit();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	virtual void __fastcall DoSave();
	Dascript::TDAScript* __fastcall GetScript();
	void __fastcall SetScript(Dascript::TDAScript* Value);
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual System::Classes::TComponent* __fastcall GetLocalComponent();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	
public:
	__property Dascript::TDAScript* Script = {read=GetScript, write=SetScript};
	__property Dasqlframe::TDASQLFrame* SQLFrame = {read=FSQLFrame};
	__property Damacrosframe::TDAMacrosFrame* MacrosFrame = {read=FMacrosFrame};
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TDAScriptEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Crtabeditor::TCRTabEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDAScriptEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Crtabeditor::TCRTabEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDAScriptEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAScriptEditorForm(HWND ParentWindow) : Crtabeditor::TCRTabEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dascripteditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASCRIPTEDITOR)
using namespace Dascripteditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DascripteditorHPP
