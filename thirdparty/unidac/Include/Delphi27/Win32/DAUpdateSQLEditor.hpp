// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAUpdateSQLEditor.pas' rev: 34.00 (Windows)

#ifndef DaupdatesqleditorHPP
#define DaupdatesqleditorHPP

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
#include <System.Classes.hpp>
#include <DBAccess.hpp>
#include <DASQLGenerator.hpp>
#include <CREditor.hpp>
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>
#include <DAUpdateSQLFrame.hpp>
#include <DASQLGeneratorFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daupdatesqleditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAUpdateSQLEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAUpdateSQLEditorForm : public Crtabeditor::TCRTabEditorForm
{
	typedef Crtabeditor::TCRTabEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shEditSQL;
	Vcl::Comctrls::TTabSheet* shGenerator;
	
protected:
	Daupdatesqlframe::TDAUpdateSQLFrame* FUpdateSQLFrame;
	Dasqlgeneratorframe::TDASQLGeneratorFrame* FSQLGeneratorFrame;
	System::Classes::TComponent* FLocalComponent;
	System::Classes::TComponent* FComponent;
	Dbaccess::TCustomDADataSet* FLocalDataSet;
	virtual void __fastcall DoInit();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	virtual void __fastcall DoSave();
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual System::Classes::TComponent* __fastcall GetLocalComponent();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	Dbaccess::TCustomDAUpdateSQL* __fastcall GetUpdateSQL();
	void __fastcall SetUpdateSQL(Dbaccess::TCustomDAUpdateSQL* const Value);
	
public:
	__property Dbaccess::TCustomDAUpdateSQL* UpdateSQL = {read=GetUpdateSQL, write=SetUpdateSQL};
	__property Daupdatesqlframe::TDAUpdateSQLFrame* UpdateSQLFrame = {read=FUpdateSQLFrame};
	__property Dbaccess::TCustomDADataSet* LocalDataSet = {read=FLocalDataSet};
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TDAUpdateSQLEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Crtabeditor::TCRTabEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDAUpdateSQLEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Crtabeditor::TCRTabEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDAUpdateSQLEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAUpdateSQLEditorForm(HWND ParentWindow) : Crtabeditor::TCRTabEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daupdatesqleditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAUPDATESQLEDITOR)
using namespace Daupdatesqleditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaupdatesqleditorHPP
