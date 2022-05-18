// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DADumpEditor.pas' rev: 35.00 (Windows)

#ifndef DadumpeditorHPP
#define DadumpeditorHPP

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
#include <DacVcl.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Data.DB.hpp>
#include <CREditor.hpp>
#include <DAEditor.hpp>
#include <DBAccess.hpp>
#include <DADump.hpp>
#include <DADesignUtils.hpp>
#include <DADualListEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dadumpeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDADumpEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDADumpEditorForm : public Daeditor::TDAEditorForm
{
	typedef Daeditor::TDAEditorForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* ClientPanel;
	Vcl::Extctrls::TPanel* LeftPanel;
	Vcl::Stdctrls::TMemo* meSQL;
	Vcl::Stdctrls::TGroupBox* gbBackupOptions;
	Vcl::Stdctrls::TCheckBox* cbGenerateHeader;
	Vcl::Buttons::TBitBtn* btBackup;
	Vcl::Buttons::TBitBtn* btRestore;
	Vcl::Buttons::TBitBtn* btImport;
	Vcl::Buttons::TBitBtn* btExport;
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	Vcl::Stdctrls::TComboBox* cbTableNames;
	Vcl::Stdctrls::TLabel* lbTableNames;
	void __fastcall btBackupClick(System::TObject* Sender);
	void __fastcall btRestoreClick(System::TObject* Sender);
	void __fastcall meSQLExit(System::TObject* Sender);
	void __fastcall btImportClick(System::TObject* Sender);
	void __fastcall btExportClick(System::TObject* Sender);
	HIDESBASE void __fastcall SaveClick(System::TObject* Sender);
	void __fastcall cbTableNamesChange(System::TObject* Sender);
	void __fastcall cbTableNamesDropDown(System::TObject* Sender);
	
protected:
	Dadump::TDADump* FDump;
	Dadump::TDADump* FLocalDump;
	virtual void __fastcall EditTableNames(System::TObject* Sender);
	virtual void __fastcall DoInit();
	virtual void __fastcall DoSave();
	virtual void __fastcall DoFinish();
	virtual void __fastcall GetButtons();
	virtual void __fastcall SetButtons();
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual System::Classes::TComponent* __fastcall GetLocalComponent();
	
public:
	__property Dadump::TDADump* Dump = {read=FDump, write=FDump};
public:
	/* TCREditorForm.Create */ inline __fastcall virtual TDADumpEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Daeditor::TDAEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDADumpEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Daeditor::TDAEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDADumpEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDADumpEditorForm(HWND ParentWindow) : Daeditor::TDAEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dadumpeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DADUMPEDITOR)
using namespace Dadumpeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DadumpeditorHPP
