// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniSQLEditor.pas' rev: 35.00 (Windows)

#ifndef UnisqleditorHPP
#define UnisqleditorHPP

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
#include <DASQLComponentEditor.hpp>
#include <CRFrame.hpp>
#include <DASQLFrame.hpp>
#include <UniSQLOptionsFrame.hpp>
#include <CRTabEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unisqleditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniSQLEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniSQLEditorForm : public Dasqlcomponenteditor::TDASQLEditorForm
{
	typedef Dasqlcomponenteditor::TDASQLEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shOptions;
	Vcl::Stdctrls::TButton* btMacros;
	void __fastcall btMacrosClick(System::TObject* Sender);
	
protected:
	Unisqloptionsframe::TUniSQLOptionsFrame* FOptionsFrame;
	virtual void __fastcall DoInit();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	
public:
	__property SQL;
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TUniSQLEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Dasqlcomponenteditor::TDASQLEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniSQLEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Dasqlcomponenteditor::TDASQLEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TUniSQLEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniSQLEditorForm(HWND ParentWindow) : Dasqlcomponenteditor::TDASQLEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unisqleditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISQLEDITOR)
using namespace Unisqleditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnisqleditorHPP
