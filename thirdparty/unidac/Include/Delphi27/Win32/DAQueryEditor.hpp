// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAQueryEditor.pas' rev: 34.00 (Windows)

#ifndef DaqueryeditorHPP
#define DaqueryeditorHPP

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
#include <DASQLGenerator.hpp>
#include <CRFrame.hpp>
#include <CREditor.hpp>
#include <CRTabEditor.hpp>
#include <DASQLFrame.hpp>
#include <DAParamsFrame.hpp>
#include <DAMacrosFrame.hpp>
#include <DASPCallFrame.hpp>
#include <DASQLComponentEditor.hpp>
#include <DAUpdateSQLFrame.hpp>
#include <DASQLGeneratorFrame.hpp>
#include <DADataTypeMapFrame.hpp>
#include <DAConditionsFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daqueryeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAQueryEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAQueryEditorForm : public Dasqlcomponenteditor::TDASQLEditorForm
{
	typedef Dasqlcomponenteditor::TDASQLEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shEditSQL;
	Vcl::Comctrls::TTabSheet* shGenerator;
	Vcl::Comctrls::TTabSheet* shDataTypeMap;
	Vcl::Buttons::TBitBtn* btnDataEditor;
	Vcl::Buttons::TBitBtn* btnCodeEditor;
	Vcl::Comctrls::TTabSheet* shConditions;
	void __fastcall btnDataEditorClick(System::TObject* Sender);
	void __fastcall btnCodeEditorClick(System::TObject* Sender);
	
protected:
	Daupdatesqlframe::TDAUpdateSQLFrame* FUpdateSQLFrame;
	Dasqlgeneratorframe::TDASQLGeneratorFrame* FSQLGeneratorFrame;
	Dadatatypemapframe::TDADataTypeMapFrame* FDataTypeMapFrame;
	Daconditionsframe::TDAConditionsFrame* FConditionsFrame;
	virtual void __fastcall DoInit();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoSave();
	Dbaccess::TCustomDADataSet* __fastcall GetQuery();
	void __fastcall SetQuery(Dbaccess::TCustomDADataSet* Value);
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	virtual Dadatatypemapframe::TDADataTypeMapFrameClass __fastcall GetDataTypeMapFrameClass();
	__property Dbaccess::TCustomDADataSet* Query = {read=GetQuery, write=SetQuery};
	
public:
	__property Daupdatesqlframe::TDAUpdateSQLFrame* UpdateSQLFrame = {read=FUpdateSQLFrame};
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TDAQueryEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Dasqlcomponenteditor::TDASQLEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDAQueryEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Dasqlcomponenteditor::TDASQLEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDAQueryEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAQueryEditorForm(HWND ParentWindow) : Dasqlcomponenteditor::TDASQLEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daqueryeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAQUERYEDITOR)
using namespace Daqueryeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaqueryeditorHPP
