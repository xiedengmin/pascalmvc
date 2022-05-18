// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DATableEditor.pas' rev: 34.00 (Windows)

#ifndef DatableeditorHPP
#define DatableeditorHPP

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
#include <CRFrame.hpp>
#include <DAEditor.hpp>
#include <CRTabEditor.hpp>
#include <DATableSQLFrame.hpp>
#include <DADataTypeMapFrame.hpp>
#include <DAConditionsFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Datableeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDATableEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDATableEditorForm : public Crtabeditor::TCRTabEditorForm
{
	typedef Crtabeditor::TCRTabEditorForm inherited;
	
__published:
	Vcl::Buttons::TBitBtn* btnDataEditor;
	Vcl::Comctrls::TTabSheet* shSQL;
	Vcl::Comctrls::TTabSheet* shDataTypeMap;
	Vcl::Comctrls::TTabSheet* shConditions;
	void __fastcall btnDataEditorClick(System::TObject* Sender);
	
protected:
	Dbaccess::TCustomDADataSet* FLocalTable;
	Dbaccess::TCustomDADataSet* FTable;
	Datablesqlframe::TDATableSQLFrame* FSQLFrame;
	Dadatatypemapframe::TDADataTypeMapFrame* FDataTypeMapFrame;
	Daconditionsframe::TDAConditionsFrame* FConditionsFrame;
	virtual void __fastcall DoInit();
	virtual void __fastcall DoFinish();
	virtual void __fastcall DoSave();
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual System::Classes::TComponent* __fastcall GetLocalComponent();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	virtual Datablesqlframe::TDATableSQLFrameClass __fastcall GetSQLFrameClass();
	virtual Dadatatypemapframe::TDADataTypeMapFrameClass __fastcall GetDataTypeMapFrameClass();
	
public:
	__property Dbaccess::TCustomDADataSet* Table = {read=FTable, write=FTable};
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TDATableEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Crtabeditor::TCRTabEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDATableEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Crtabeditor::TCRTabEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDATableEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDATableEditorForm(HWND ParentWindow) : Crtabeditor::TCRTabEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Datableeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DATABLEEDITOR)
using namespace Datableeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DatableeditorHPP
