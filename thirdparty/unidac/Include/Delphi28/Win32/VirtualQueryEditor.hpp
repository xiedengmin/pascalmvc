// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualQueryEditor.pas' rev: 35.00 (Windows)

#ifndef VirtualqueryeditorHPP
#define VirtualqueryeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <CRAccess.hpp>
#include <DBAccess.hpp>
#include <CRTabEditor.hpp>
#include <DASQLComponentEditor.hpp>
#include <DASQLFrame.hpp>
#include <DAParamsFrame.hpp>
#include <DAMacrosFrame.hpp>
#include <DASQLGeneratorFrame.hpp>
#include <DAUpdateSQLFrame.hpp>
#include <DAQueryEditor.hpp>
#include <CRColFrame.hpp>
#include <CRFrame.hpp>
#include <CREditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualqueryeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVirtualSQLGeneratorFrame;
class DELPHICLASS TVirtualUpdateSQLFrame;
class DELPHICLASS TVirtualParamsFrame;
class DELPHICLASS TVirtualQueryEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TVirtualSQLGeneratorFrame : public Dasqlgeneratorframe::TDASQLGeneratorFrame
{
	typedef Dasqlgeneratorframe::TDASQLGeneratorFrame inherited;
	
protected:
	virtual System::UnicodeString __fastcall GenerateSQLforUpdTable(Craccess::TCRTableInfo* TableInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields, const Dbaccess::TStatementType StatementType, const bool ModifiedFieldsOnly);
	
public:
	__fastcall virtual TVirtualSQLGeneratorFrame(System::Classes::TComponent* AOwner);
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TVirtualSQLGeneratorFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualSQLGeneratorFrame(HWND ParentWindow) : Dasqlgeneratorframe::TDASQLGeneratorFrame(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TVirtualUpdateSQLFrame : public Daupdatesqlframe::TDAUpdateSQLFrame
{
	typedef Daupdatesqlframe::TDAUpdateSQLFrame inherited;
	
protected:
	virtual void __fastcall DoActivate();
public:
	/* TDASQLFrame.Create */ inline __fastcall virtual TVirtualUpdateSQLFrame(System::Classes::TComponent* Owner) : Daupdatesqlframe::TDAUpdateSQLFrame(Owner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TVirtualUpdateSQLFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualUpdateSQLFrame(HWND ParentWindow) : Daupdatesqlframe::TDAUpdateSQLFrame(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TVirtualParamsFrame : public Daparamsframe::TDAParamsFrame
{
	typedef Daparamsframe::TDAParamsFrame inherited;
	
public:
	__fastcall virtual TVirtualParamsFrame(System::Classes::TComponent* AOwner);
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TVirtualParamsFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualParamsFrame(HWND ParentWindow) : Daparamsframe::TDAParamsFrame(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TVirtualQueryEditorForm : public Daqueryeditor::TDAQueryEditorForm
{
	typedef Daqueryeditor::TDAQueryEditorForm inherited;
	
protected:
	virtual void __fastcall DoInit();
	
public:
	__property Query;
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TVirtualQueryEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Daqueryeditor::TDAQueryEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TVirtualQueryEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Daqueryeditor::TDAQueryEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TVirtualQueryEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualQueryEditorForm(HWND ParentWindow) : Daqueryeditor::TDAQueryEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Virtualqueryeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALQUERYEDITOR)
using namespace Virtualqueryeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualqueryeditorHPP
