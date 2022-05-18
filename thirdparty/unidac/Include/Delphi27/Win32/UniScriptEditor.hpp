// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniScriptEditor.pas' rev: 34.00 (Windows)

#ifndef UniscripteditorHPP
#define UniscripteditorHPP

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
#include <DBAccess.hpp>
#include <MemUtils.hpp>
#include <DAScript.hpp>
#include <CREditor.hpp>
#include <CRTabEditor.hpp>
#include <DAScriptEditor.hpp>
#include <CRFrame.hpp>
#include <UniSQLOptionsFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniscripteditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniScriptEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniScriptEditorForm : public Dascripteditor::TDAScriptEditorForm
{
	typedef Dascripteditor::TDAScriptEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shOptions;
	
protected:
	Unisqloptionsframe::TUniSQLOptionsFrame* FOptionsFrame;
	virtual void __fastcall DoInit();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TUniScriptEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Dascripteditor::TDAScriptEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniScriptEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Dascripteditor::TDAScriptEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TUniScriptEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniScriptEditorForm(HWND ParentWindow) : Dascripteditor::TDAScriptEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uniscripteditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISCRIPTEDITOR)
using namespace Uniscripteditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniscripteditorHPP
