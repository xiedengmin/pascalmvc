// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniQueryEditor.pas' rev: 35.00 (Windows)

#ifndef UniqueryeditorHPP
#define UniqueryeditorHPP

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
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <Uni.hpp>
#include <DBAccess.hpp>
#include <CREditor.hpp>
#include <CRTabEditor.hpp>
#include <CRFrame.hpp>
#include <DASQLComponentEditor.hpp>
#include <DAQueryEditor.hpp>
#include <DADataTypeMapFrame.hpp>
#include <UniQueryOptionsFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniqueryeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniQueryEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniQueryEditorForm : public Daqueryeditor::TDAQueryEditorForm
{
	typedef Daqueryeditor::TDAQueryEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shOptions;
	Vcl::Stdctrls::TButton* btMacros;
	void __fastcall btMacrosClick(System::TObject* Sender);
	
protected:
	Uniqueryoptionsframe::TUniQueryOptionsFrame* FOptionsFrame;
	virtual void __fastcall DoInit();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	virtual Dadatatypemapframe::TDADataTypeMapFrameClass __fastcall GetDataTypeMapFrameClass();
	
public:
	__property Query;
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TUniQueryEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Daqueryeditor::TDAQueryEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniQueryEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Daqueryeditor::TDAQueryEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TUniQueryEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniQueryEditorForm(HWND ParentWindow) : Daqueryeditor::TDAQueryEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uniqueryeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIQUERYEDITOR)
using namespace Uniqueryeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniqueryeditorHPP
