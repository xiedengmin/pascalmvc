// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VTDataEditor.pas' rev: 35.00 (Windows)

#ifndef VtdataeditorHPP
#define VtdataeditorHPP

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
#include <CRDataEditor.hpp>
#include <CRDesignUtils.hpp>
#include <VirtualTable.hpp>
#include <CREditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vtdataeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVTDataEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TVTDataEditorForm : public Crdataeditor::TCRDataEditorForm
{
	typedef Crdataeditor::TCRDataEditorForm inherited;
	
__published:
	Vcl::Buttons::TSpeedButton* btSave;
	Vcl::Buttons::TSpeedButton* btClear;
	Vcl::Buttons::TSpeedButton* btLoadFromFile;
	Vcl::Buttons::TSpeedButton* btSaveToFile;
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	void __fastcall btLoadFromFileClick(System::TObject* Sender);
	void __fastcall btSaveToFileClick(System::TObject* Sender);
	void __fastcall btClearClick(System::TObject* Sender);
	HIDESBASE void __fastcall DataSourceDataChange(System::TObject* Sender, Data::Db::TField* Field);
	
private:
	Virtualtable::TVirtualTable* LocalDataSet;
	void __fastcall AfterPost(Data::Db::TDataSet* DataSet);
	void __fastcall AfterDelete(Data::Db::TDataSet* DataSet);
	int __fastcall LoadDefaultExt();
	void __fastcall SaveDefaultExt(int Value);
	
protected:
	virtual void __fastcall DoInit();
	virtual void __fastcall DoSave();
	virtual void __fastcall DoFinish();
public:
	/* TCREditorForm.Create */ inline __fastcall virtual TVTDataEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Crdataeditor::TCRDataEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TVTDataEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Crdataeditor::TCRDataEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TVTDataEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVTDataEditorForm(HWND ParentWindow) : Crdataeditor::TCRDataEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Vtdataeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VTDATAEDITOR)
using namespace Vtdataeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VtdataeditorHPP
