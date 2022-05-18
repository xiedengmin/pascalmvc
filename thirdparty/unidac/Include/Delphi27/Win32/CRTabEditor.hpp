// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRTabEditor.pas' rev: 34.00 (Windows)

#ifndef CrtabeditorHPP
#define CrtabeditorHPP

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
#include <System.Classes.hpp>
#include <CREditor.hpp>
#include <DAEditor.hpp>
#include <CRFrame.hpp>
#include <CRDesignUtils.hpp>
#include <DADesignUtils.hpp>
#include <MemData.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crtabeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRTabEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCRTabEditorForm : public Daeditor::TDAEditorForm
{
	typedef Daeditor::TDAEditorForm inherited;
	
__published:
	Vcl::Comctrls::TPageControl* PageControl;
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall PageControlChange(System::TObject* Sender);
	void __fastcall PageControlChanging(System::TObject* Sender, bool &AllowChange);
	
protected:
	System::Classes::TList* FFramesList;
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall DoInit();
	virtual void __fastcall DoActivate();
	virtual void __fastcall SaveControlData();
	Crframe::TCRFrame* __fastcall AddTab(Crframe::TCRFrameClass FrameClass, Vcl::Comctrls::TTabSheet* Page);
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	virtual bool __fastcall GetModified();
	virtual void __fastcall SetModified(bool Value);
	Crframe::TCRFrame* __fastcall GetActiveFrame();
	virtual void __fastcall DoPageControlChange(System::TObject* Sender);
	virtual void __fastcall DoPageControlChanging(System::TObject* Sender, bool &AllowChange);
	
public:
	__fastcall virtual TCRTabEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass);
	void __fastcall ActivateFrame(Crframe::TCRFrame* Frame);
	__property Crframe::TCRFrame* ActiveFrame = {read=GetActiveFrame};
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCRTabEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Daeditor::TDAEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCRTabEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCRTabEditorForm(HWND ParentWindow) : Daeditor::TDAEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crtabeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRTABEDITOR)
using namespace Crtabeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrtabeditorHPP
