// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DATableSQLFrame.pas' rev: 35.00 (Windows)

#ifndef DatablesqlframeHPP
#define DatablesqlframeHPP

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
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CRTypes.hpp>
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>
#include <DBAccess.hpp>

//-- user supplied -----------------------------------------------------------

namespace Datablesqlframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDATableSQLFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDATableSQLFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TComboBox* cbTableName;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TEdit* edOrderFields;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TEdit* edFilter;
	Vcl::Stdctrls::TMemo* meSQL;
	void __fastcall cbTableNameDropDown(System::TObject* Sender);
	void __fastcall cbTableNameExit(System::TObject* Sender);
	void __fastcall edOrderFieldsExit(System::TObject* Sender);
	void __fastcall edFilterExit(System::TObject* Sender);
	
protected:
	bool FListGot;
	bool FAllTables;
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	void __fastcall InitSQL();
	Dbaccess::TCustomDADataSet* __fastcall GetLocalTable();
	virtual void __fastcall GetTableNames(Dbaccess::TCustomDAConnection* Connection, System::Classes::TStrings* Items);
	
public:
	__fastcall virtual TDATableSQLFrame(System::Classes::TComponent* Owner);
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDATableSQLFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDATableSQLFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


typedef System::TMetaClass* TDATableSQLFrameClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Datablesqlframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DATABLESQLFRAME)
using namespace Datablesqlframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DatablesqlframeHPP
