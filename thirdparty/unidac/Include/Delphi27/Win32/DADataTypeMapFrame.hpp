// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DADataTypeMapFrame.pas' rev: 34.00 (Windows)

#ifndef DadatatypemapframeHPP
#define DadatatypemapframeHPP

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
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <CRCtrls.hpp>
#include <System.Variants.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Grids.hpp>
#include <Data.DB.hpp>
#include <CRDataTypeMap.hpp>
#include <DBAccess.hpp>
#include <MemDS.hpp>
#include <CRFrame.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dadatatypemapframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDADataTypeMapFrame;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TDADataTypeMapFrameClass;

class PASCALIMPLEMENTATION TDADataTypeMapFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Stdctrls::TCheckBox* chkIgnoreError;
	Vcl::Stdctrls::TButton* btnAddRule;
	Vcl::Stdctrls::TButton* btnRemoveRule;
	Vcl::Stdctrls::TButton* btnMoveUp;
	Vcl::Stdctrls::TButton* btnMoveDown;
	void __fastcall grdDataTypeMapDrawCell(System::TObject* Sender, int ACol, int ARow, const System::Types::TRect &Rect, Vcl::Grids::TGridDrawState State);
	void __fastcall grdDataTypeMapMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall grdDataTypeMapSetEditText(System::TObject* Sender, int ACol, int ARow, const System::UnicodeString Value);
	void __fastcall btnAddRuleClick(System::TObject* Sender);
	void __fastcall btnRemoveRuleClick(System::TObject* Sender);
	void __fastcall btnMoveUpClick(System::TObject* Sender);
	void __fastcall btnMoveDownClick(System::TObject* Sender);
	
private:
	bool FFieldNameListAllowed;
	Crctrls::TCRStringGrid* FFakeGrid;
	Dbaccess::TFieldTypeInfos* __fastcall GetFieldTypeInfos();
	Dbaccess::TDAMapRules* __fastcall GetRules();
	bool __fastcall GetIgnoreError(int ARow);
	void __fastcall SetIgnoreError(int ARow, bool Value);
	void __fastcall PaintGridTitle(HDC DC);
	int __fastcall GetColumnLeft(int ColumnIndex);
	int __fastcall GetColumnWidth(int ColumnIndex);
	int __fastcall CalculateColumnWidth(int ColumnIndex);
	void __fastcall IsSelectableCell(System::TObject* Sender, int ACol, int ARow, bool &CanSelect);
	bool __fastcall IsEditableCell(int ACol, int ARow);
	bool __fastcall IsComboBox(int ACol, int ARow);
	bool __fastcall IsNumeric(int ACol, int ARow);
	void __fastcall GetPickListItems(int ACol, int ARow, System::Classes::TStrings* Items);
	
protected:
	Crctrls::TCRStringGrid* FDataTypeMapGrid;
	virtual Crdatatypemap::TConverterManagerClass __fastcall GetConverterManagerClass();
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	void __fastcall InitCheckBox();
	void __fastcall InitGrid();
	void __fastcall FillFieldList(System::Classes::TStrings* Items);
	void __fastcall FillDBTypeList(System::Classes::TStrings* Items);
	void __fastcall FillFieldTypeList(System::Classes::TStrings* Items);
	virtual void __fastcall ResizeGrid();
	System::Types::TRect __fastcall GetCheckBoxRect(int ACol, int ARow);
	bool __fastcall IsEmptyRule(int Row);
	void __fastcall ClearRule(int Row);
	void __fastcall LoadRule(int Index, Dbaccess::TDAMapRules* Rules, int RuleIndex);
	void __fastcall LoadRules();
	void __fastcall SaveRule(int Index, Dbaccess::TDAMapRules* Rules);
	void __fastcall SaveRules();
	
public:
	__fastcall virtual TDADataTypeMapFrame(System::Classes::TComponent* AOwner);
	__property bool FieldNameListAllowed = {read=FFieldNameListAllowed, write=FFieldNameListAllowed, nodefault};
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDADataTypeMapFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDADataTypeMapFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 rowTitleCount = System::Int8(0x2);
static const System::Int8 colDBType = System::Int8(0x0);
static const System::Int8 colDBLengthMin = System::Int8(0x1);
static const System::Int8 colDBLengthMax = System::Int8(0x2);
static const System::Int8 colDBScaleMin = System::Int8(0x3);
static const System::Int8 colDBScaleMax = System::Int8(0x4);
static const System::Int8 colFieldName = System::Int8(0x5);
static const System::Int8 colFieldType = System::Int8(0x6);
static const System::Int8 colFieldLength = System::Int8(0x7);
static const System::Int8 colFieldScale = System::Int8(0x8);
static const System::Int8 colIgnoreError = System::Int8(0x9);
static const System::Int8 widthDBType = System::Int8(0x64);
static const System::Int8 widthDBLengthMin = System::Int8(0x1e);
static const System::Int8 widthDBLengthMax = System::Int8(0x1e);
static const System::Int8 widthDBScaleMin = System::Int8(0x1e);
static const System::Int8 widthDBScaleMax = System::Int8(0x1e);
static const System::Int8 widthFieldName = System::Int8(0x64);
static const System::Int8 widthFieldType = System::Int8(0x64);
static const System::Int8 widthFieldLength = System::Int8(0x1e);
static const System::Int8 widthFieldScale = System::Int8(0x1e);
static const System::Int8 widthIgnoreError = System::Int8(0x28);
}	/* namespace Dadatatypemapframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DADATATYPEMAPFRAME)
using namespace Dadatatypemapframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DadatatypemapframeHPP
