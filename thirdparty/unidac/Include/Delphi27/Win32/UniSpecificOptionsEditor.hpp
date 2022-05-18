// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniSpecificOptionsEditor.pas' rev: 34.00 (Windows)

#ifndef UnispecificoptionseditorHPP
#define UnispecificoptionseditorHPP

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
#include <Vcl.Buttons.hpp>
#include <Vcl.DBCtrls.hpp>
#include <Data.DB.hpp>
#include <DBAccess.hpp>
#include <Uni.hpp>
#include <CREditor.hpp>
#include <CRTabEditor.hpp>
#include <CRFrame.hpp>
#include <UniSQLOptionsFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unispecificoptionseditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniSpecificOptionsEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniSpecificOptionsEditorForm : public Crtabeditor::TCRTabEditorForm
{
	typedef Crtabeditor::TCRTabEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shOptions;
	
protected:
	System::Classes::TComponent* FLocalComponent;
	System::Classes::TComponent* FComponent;
	Unisqloptionsframe::TUniSQLOptionsFrame* FOptionsFrame;
	virtual void __fastcall DoInit();
	virtual void __fastcall DoFinish();
	virtual void __fastcall DoSave();
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual System::Classes::TComponent* __fastcall GetLocalComponent();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TUniSpecificOptionsEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Crtabeditor::TCRTabEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniSpecificOptionsEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Crtabeditor::TCRTabEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TUniSpecificOptionsEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniSpecificOptionsEditorForm(HWND ParentWindow) : Crtabeditor::TCRTabEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unispecificoptionseditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISPECIFICOPTIONSEDITOR)
using namespace Unispecificoptionseditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnispecificoptionseditorHPP
