// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAStoredProcEditor.pas' rev: 35.00 (Windows)

#ifndef DastoredproceditorHPP
#define DastoredproceditorHPP

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
#include <CREditor.hpp>
#include <CRTabEditor.hpp>
#include <DASQLFrame.hpp>
#include <DAParamsFrame.hpp>
#include <DAMacrosFrame.hpp>
#include <DASPCallFrame.hpp>
#include <DASQLComponentEditor.hpp>
#include <DAUpdateSQLFrame.hpp>
#include <DASQLGeneratorFrame.hpp>
#include <DAQueryEditor.hpp>
#include <DADataTypeMapFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dastoredproceditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAStoredProcEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAStoredProcEditorForm : public Daqueryeditor::TDAQueryEditorForm
{
	typedef Daqueryeditor::TDAQueryEditorForm inherited;
	
protected:
	virtual void __fastcall DoInit();
	Dbaccess::TCustomDADataSet* __fastcall GetStoredProc();
	void __fastcall SetStoredProc(Dbaccess::TCustomDADataSet* Value);
	__property Dbaccess::TCustomDADataSet* StoredProc = {read=GetStoredProc, write=SetStoredProc};
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TDAStoredProcEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Daqueryeditor::TDAQueryEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDAStoredProcEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Daqueryeditor::TDAQueryEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDAStoredProcEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAStoredProcEditorForm(HWND ParentWindow) : Daqueryeditor::TDAQueryEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dastoredproceditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASTOREDPROCEDITOR)
using namespace Dastoredproceditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DastoredproceditorHPP
