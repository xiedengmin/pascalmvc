// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DASQLGeneratorFrame.pas' rev: 34.00 (Windows)

#ifndef DasqlgeneratorframeHPP
#define DasqlgeneratorframeHPP

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
#include <DBAccess.hpp>
#include <CRAccess.hpp>
#include <DASQLGenerator.hpp>
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dasqlgeneratorframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDASQLGeneratorFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDASQLGeneratorFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnSQLGenerator;
	Vcl::Stdctrls::TLabel* lbTableName;
	Vcl::Stdctrls::TLabel* lbKeyFieldsLabel;
	Vcl::Stdctrls::TLabel* lbUpdateFieldsLabel;
	Vcl::Stdctrls::TComboBox* cbTables;
	Vcl::Stdctrls::TButton* btGenerate;
	Vcl::Stdctrls::TListBox* lbKeyFields;
	Vcl::Stdctrls::TListBox* lbUpdateFields;
	Vcl::Stdctrls::TButton* btGetFields;
	Vcl::Stdctrls::TCheckBox* cbInsert;
	Vcl::Stdctrls::TCheckBox* cbUpdate;
	Vcl::Stdctrls::TCheckBox* cbDelete;
	Vcl::Stdctrls::TCheckBox* cbRefresh;
	Vcl::Stdctrls::TCheckBox* cbQuoteFields;
	Vcl::Stdctrls::TCheckBox* cbLock;
	Vcl::Stdctrls::TCheckBox* cbRecCount;
	Vcl::Extctrls::TBevel* bvl;
	void __fastcall pnSQLGeneratorResize(System::TObject* Sender);
	void __fastcall btGetFieldsClick(System::TObject* Sender);
	void __fastcall cbTablesChange(System::TObject* Sender);
	void __fastcall cbIUDRClick(System::TObject* Sender);
	void __fastcall btGenerateClick(System::TObject* Sender);
	void __fastcall lbUpdateFieldsClick(System::TObject* Sender);
	void __fastcall cbTablesDropDown(System::TObject* Sender);
	
protected:
	Dbaccess::TCustomDADataSet* FLocalDataSet;
	System::Classes::TNotifyEvent FOnChange;
	bool FLockUpdateControlsState;
	System::UnicodeString FLastGeneratedSQL;
	System::UnicodeString FLastOriginSQL;
	System::UnicodeString FOldSQL;
	System::UnicodeString FOldUpdatingTable;
	System::StaticArray<Vcl::Stdctrls::TCheckBox*, 11> Fcb;
	bool FPromptlyServerRead;
	bool FParseTableNames;
	bool FActivated;
	virtual void __fastcall GenerateSelectFromAllFields();
	void __fastcall SetFilterSQL(System::UnicodeString Value, bool Open = false);
	bool __fastcall OpenDataSet(bool RaiseException = false);
	void __fastcall InitTables(bool DeleteBadTableNames = false);
	void __fastcall ClearFields();
	void __fastcall GetFields(bool Forced = false);
	virtual void __fastcall UpdateControlsState();
	Dbaccess::TCustomDADataSet* __fastcall GetOriginLocalDataSet();
	virtual Craccess::TCRTablesInfo* __fastcall GetTablesInfo();
	Craccess::TSQLInfo* __fastcall GetSQLInfo();
	virtual System::UnicodeString __fastcall SelectedTableName();
	virtual System::UnicodeString __fastcall NormSelectedTableName();
	System::UnicodeString __fastcall UnqSelectedTableName();
	void __fastcall SetControlEnabled(Vcl::Controls::TControl* Control, bool Value);
	bool __fastcall cbChecked(Dbaccess::TStatementType StatementType);
	__property Dbaccess::TCustomDADataSet* LocalDataSet = {read=FLocalDataSet};
	__property Dbaccess::TCustomDADataSet* OriginLocalDataSet = {read=GetOriginLocalDataSet};
	__property Craccess::TCRTablesInfo* TablesInfo = {read=GetTablesInfo};
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	virtual System::UnicodeString __fastcall GenerateSQLforUpdTable(Craccess::TCRTableInfo* TableInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields, const Dbaccess::TStatementType StatementType, const bool ModifiedFieldsOnly);
	
public:
	__fastcall virtual TDASQLGeneratorFrame(System::Classes::TComponent* AOwner);
	virtual Vcl::Controls::TWinControl* __fastcall ActiveControl();
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDASQLGeneratorFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDASQLGeneratorFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dasqlgeneratorframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASQLGENERATORFRAME)
using namespace Dasqlgeneratorframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DasqlgeneratorframeHPP
