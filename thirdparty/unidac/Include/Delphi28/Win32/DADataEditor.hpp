// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DADataEditor.pas' rev: 35.00 (Windows)

#ifndef DadataeditorHPP
#define DadataeditorHPP

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
#include <DBAccess.hpp>
#include <DASQLGenerator.hpp>
#include <DADesignUtils.hpp>
#include <CREditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dadataeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDADataEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDADataEditorForm : public Crdataeditor::TCRDataEditorForm
{
	typedef Crdataeditor::TCRDataEditorForm inherited;
	
__published:
	Vcl::Buttons::TSpeedButton* btOpen;
	Vcl::Buttons::TSpeedButton* btnExit;
	Vcl::Buttons::TSpeedButton* btSaveToFile;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	void __fastcall btOpenClick(System::TObject* Sender);
	void __fastcall btCloseClick(System::TObject* Sender);
	void __fastcall btSaveToFileClick(System::TObject* Sender);
	
private:
	bool FOldAutoCommit;
	Data::Db::TFilterOptions FOldFilterOptions;
	System::UnicodeString FOldFilterSQL;
	System::UnicodeString FOldSQL;
	bool FOldActive;
	System::UnicodeString FOldFilter;
	bool FOldFiltered;
	
protected:
	virtual void __fastcall DoInit();
	virtual void __fastcall DoSave();
	virtual void __fastcall DoFinish();
public:
	/* TCREditorForm.Create */ inline __fastcall virtual TDADataEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Crdataeditor::TCRDataEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDADataEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Crdataeditor::TCRDataEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDADataEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDADataEditorForm(HWND ParentWindow) : Crdataeditor::TCRDataEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dadataeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DADATAEDITOR)
using namespace Dadataeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DadataeditorHPP
