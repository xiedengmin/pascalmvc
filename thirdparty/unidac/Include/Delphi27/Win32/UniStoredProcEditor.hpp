// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniStoredProcEditor.pas' rev: 34.00 (Windows)

#ifndef UnistoredproceditorHPP
#define UnistoredproceditorHPP

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
#include <CRFrame.hpp>
#include <DAStoredProcEditor.hpp>
#include <DADataTypeMapFrame.hpp>
#include <Uni.hpp>
#include <UniSQLOptionsFrame.hpp>
#include <DAQueryEditor.hpp>
#include <DASQLComponentEditor.hpp>
#include <CRTabEditor.hpp>
#include <CREditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unistoredproceditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniStoredProcEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniStoredProcEditorForm : public Dastoredproceditor::TDAStoredProcEditorForm
{
	typedef Dastoredproceditor::TDAStoredProcEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shOptions;
	
protected:
	Unisqloptionsframe::TUniSQLOptionsFrame* FOptionsFrame;
	virtual void __fastcall DoInit();
	virtual Dadatatypemapframe::TDADataTypeMapFrameClass __fastcall GetDataTypeMapFrameClass();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	
public:
	__property StoredProc;
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TUniStoredProcEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Dastoredproceditor::TDAStoredProcEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniStoredProcEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Dastoredproceditor::TDAStoredProcEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TUniStoredProcEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniStoredProcEditorForm(HWND ParentWindow) : Dastoredproceditor::TDAStoredProcEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unistoredproceditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISTOREDPROCEDITOR)
using namespace Unistoredproceditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnistoredproceditorHPP
