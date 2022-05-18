// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniTableEditor.pas' rev: 35.00 (Windows)

#ifndef UnitableeditorHPP
#define UnitableeditorHPP

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
#include <Vcl.ComCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <DBAccess.hpp>
#include <Uni.hpp>
#include <CRFrame.hpp>
#include <DATableEditor.hpp>
#include <DATableSQLFrame.hpp>
#include <DADataTypeMapFrame.hpp>
#include <UniTableSQLFrame.hpp>
#include <UniSQLOptionsFrame.hpp>
#include <DAConditionsFrame.hpp>
#include <CRTabEditor.hpp>
#include <CREditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unitableeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniTableEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniTableEditorForm : public Datableeditor::TDATableEditorForm
{
	typedef Datableeditor::TDATableEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shOptions;
	
protected:
	Unisqloptionsframe::TUniSQLOptionsFrame* FOptionsFrame;
	virtual void __fastcall DoInit();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	virtual Datablesqlframe::TDATableSQLFrameClass __fastcall GetSQLFrameClass();
	virtual Dadatatypemapframe::TDADataTypeMapFrameClass __fastcall GetDataTypeMapFrameClass();
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TUniTableEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Datableeditor::TDATableEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniTableEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Datableeditor::TDATableEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TUniTableEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniTableEditorForm(HWND ParentWindow) : Datableeditor::TDATableEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unitableeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNITABLEEDITOR)
using namespace Unitableeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnitableeditorHPP
