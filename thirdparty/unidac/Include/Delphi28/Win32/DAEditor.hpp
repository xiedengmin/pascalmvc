// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAEditor.pas' rev: 35.00 (Windows)

#ifndef DaeditorHPP
#define DaeditorHPP

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
#include <Vcl.DBGrids.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <CREditor.hpp>
#include <DADesignUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAEditorForm : public Creditor::TCREditorForm
{
	typedef Creditor::TCREditorForm inherited;
	
protected:
	Dadesignutils::TDADesignUtilsClass __fastcall GetDADesignUtilsClass();
	
public:
	void __fastcall CheckConnection(System::Classes::TComponent* const Component);
	__property Dadesignutils::TDADesignUtilsClass DADesignUtilsClass = {read=GetDADesignUtilsClass};
public:
	/* TCREditorForm.Create */ inline __fastcall virtual TDAEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Creditor::TCREditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDAEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Creditor::TCREditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDAEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAEditorForm(HWND ParentWindow) : Creditor::TCREditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAEDITOR)
using namespace Daeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaeditorHPP
