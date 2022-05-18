// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAParamsFrame.pas' rev: 34.00 (Windows)

#ifndef DaparamsframeHPP
#define DaparamsframeHPP

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
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <DBAccess.hpp>
#include <Data.DB.hpp>
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>
#include <CRColFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daparamsframe
{
//-- forward type declarations -----------------------------------------------
struct TDataTypeInfo;
struct TParamTypeInfo;
class DELPHICLASS TDAParamsFrame;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TDataTypeInfo
{
public:
	System::UnicodeString Name;
	Data::Db::TFieldType DataType;
	bool EmptyIsNull;
	bool Editable;
	bool Sizeable;
	System::UnicodeString DefaultValue;
};


typedef System::DynamicArray<TDataTypeInfo> TDataTypeInfos;

struct DECLSPEC_DRECORD TParamTypeInfo
{
public:
	System::UnicodeString Name;
	Data::Db::TParamType ParamType;
};


typedef System::DynamicArray<TParamTypeInfo> TParamTypeInfos;

class PASCALIMPLEMENTATION TDAParamsFrame : public Crcolframe::TCRColFrame
{
	typedef Crcolframe::TCRColFrame inherited;
	
__published:
	Vcl::Stdctrls::TLabel* lbPName;
	Vcl::Stdctrls::TLabel* lbParamLog;
	Vcl::Stdctrls::TLabel* lbPType;
	Vcl::Stdctrls::TComboBox* cbDataType;
	Vcl::Stdctrls::TComboBox* cbParamType;
	Vcl::Stdctrls::TLabel* lbParamType;
	Vcl::Stdctrls::TLabel* lbPValue;
	Vcl::Stdctrls::TEdit* edValue;
	Vcl::Stdctrls::TButton* bEdValue;
	Vcl::Stdctrls::TCheckBox* cbNullValue;
	Vcl::Stdctrls::TLabel* lbNullValue;
	Vcl::Stdctrls::TLabel* lbSize;
	Vcl::Stdctrls::TEdit* edSize;
	void __fastcall cbDataTypeChange(System::TObject* Sender);
	void __fastcall cbParamTypeChange(System::TObject* Sender);
	void __fastcall edValueChange(System::TObject* Sender);
	void __fastcall cbNullValueClick(System::TObject* Sender);
	void __fastcall edSizeChange(System::TObject* Sender);
	void __fastcall bEdValueClick(System::TObject* Sender);
	
protected:
	TDataTypeInfos FDataTypeInfos;
	TParamTypeInfos FParamTypeInfos;
	void __fastcall AddDataType(System::UnicodeString Name, Data::Db::TFieldType DataType, bool EmptyIsNull, bool Editable, bool Sizeable, System::UnicodeString DefaultValue);
	void __fastcall AddParamType(System::UnicodeString Name, Data::Db::TParamType ParamType);
	virtual System::Classes::TCollection* __fastcall GetItems();
	Dbaccess::TDAParams* __fastcall GetParams();
	virtual System::UnicodeString __fastcall GetItemName(System::Classes::TCollectionItem* Item);
	virtual void __fastcall InitItems();
	virtual void __fastcall ItemToControls(System::Classes::TCollectionItem* Item);
	virtual void __fastcall StoreItemValue(System::Classes::TCollectionItem* Item);
	virtual void __fastcall ControlsToItem(System::Classes::TCollectionItem* Item);
	virtual void __fastcall UpdateControlsState();
	__property Dbaccess::TDAParams* Params = {read=GetParams};
public:
	/* TCustomFrame.Create */ inline __fastcall virtual TDAParamsFrame(System::Classes::TComponent* AOwner) : Crcolframe::TCRColFrame(AOwner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDAParamsFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAParamsFrame(HWND ParentWindow) : Crcolframe::TCRColFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daparamsframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAPARAMSFRAME)
using namespace Daparamsframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaparamsframeHPP
