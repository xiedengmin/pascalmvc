// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRDataEditor.pas' rev: 35.00 (Windows)

#ifndef CrdataeditorHPP
#define CrdataeditorHPP

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
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.DBCtrls.hpp>
#include <Data.DB.hpp>
#include <CREditor.hpp>
#include <MemDS.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crdataeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRDataEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCRDataEditorForm : public Creditor::TCREditorForm
{
	typedef Creditor::TCREditorForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnlToolBar;
	Vcl::Buttons::TSpeedButton* btClose;
	Vcl::Dbctrls::TDBNavigator* DBNavigator;
	Vcl::Dbgrids::TDBGrid* DBGrid;
	Vcl::Comctrls::TStatusBar* StatusBar;
	Data::Db::TDataSource* DataSource;
	void __fastcall DataSourceStateChange(System::TObject* Sender);
	void __fastcall DataSourceDataChange(System::TObject* Sender, Data::Db::TField* Field);
	HIDESBASE void __fastcall FormCloseQuery(System::TObject* Sender, bool &CanClose);
	
protected:
	Memds::TMemDataSet* FDataSet;
	virtual void __fastcall DoInit();
	virtual void __fastcall DoSave();
	virtual void __fastcall DoFinish();
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
public:
	/* TCREditorForm.Create */ inline __fastcall virtual TCRDataEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Creditor::TCREditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCRDataEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Creditor::TCREditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCRDataEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCRDataEditorForm(HWND ParentWindow) : Creditor::TCREditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crdataeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRDATAEDITOR)
using namespace Crdataeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrdataeditorHPP
