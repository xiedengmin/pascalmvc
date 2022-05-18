// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAParamValueEditor.pas' rev: 34.00 (Windows)

#ifndef DaparamvalueeditorHPP
#define DaparamvalueeditorHPP

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
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Data.DB.hpp>
#include <CREditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daparamvalueeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAParamValueEditor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAParamValueEditor : public Creditor::TCREditorForm
{
	typedef Creditor::TCREditorForm inherited;
	
__published:
	Vcl::Stdctrls::TMemo* Memo;
	void __fastcall MemoChange(System::TObject* Sender);
	
protected:
	System::UnicodeString __fastcall GetValue();
	void __fastcall SetValue(System::UnicodeString Value);
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	
public:
	__property System::UnicodeString Value = {read=GetValue, write=SetValue};
public:
	/* TCREditorForm.Create */ inline __fastcall virtual TDAParamValueEditor(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Creditor::TCREditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDAParamValueEditor(System::Classes::TComponent* AOwner, int Dummy) : Creditor::TCREditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDAParamValueEditor() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAParamValueEditor(HWND ParentWindow) : Creditor::TCREditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daparamvalueeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAPARAMVALUEEDITOR)
using namespace Daparamvalueeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaparamvalueeditorHPP
