// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRGrid.pas' rev: 35.00 (Windows)

#ifndef CrgridHPP
#define CrgridHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.Variants.hpp>
#include <System.UITypes.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.DBGrids.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Forms.hpp>
#include <Data.DB.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crgrid
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRColumnTitle;
class DELPHICLASS TCRColumn;
class DELPHICLASS TCRDBGridColumns;
class DELPHICLASS TCRGridTitleEdit;
class DELPHICLASS TMemoEditorForm;
class DELPHICLASS TSortColInfo;
class DELPHICLASS TCRGridDataLink;
class DELPHICLASS TCRDBGrid;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TSortOrder : unsigned char { soNone, soAsc, soDesc };

enum DECLSPEC_DENUM TSummaryMode : unsigned char { smNone, smSum, smAvr, smMax, smMin, smLabel };

typedef void __fastcall (__closure *TOnMemoClick)(System::TObject* Sender, Vcl::Dbgrids::TColumn* Column);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRColumnTitle : public Vcl::Dbgrids::TColumnTitle
{
	typedef Vcl::Dbgrids::TColumnTitle inherited;
	
private:
	HIDESBASE System::UnicodeString __fastcall GetCaption();
	HIDESBASE bool __fastcall IsCaptionStored();
	
protected:
	HIDESBASE void __fastcall SetCaption(const System::UnicodeString Value);
	
__published:
	__property System::UnicodeString Caption = {read=GetCaption, write=SetCaption, stored=IsCaptionStored};
public:
	/* TColumnTitle.Create */ inline __fastcall TCRColumnTitle(Vcl::Dbgrids::TColumn* Column) : Vcl::Dbgrids::TColumnTitle(Column) { }
	/* TColumnTitle.Destroy */ inline __fastcall virtual ~TCRColumnTitle() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRColumn : public Vcl::Dbgrids::TColumn
{
	typedef Vcl::Dbgrids::TColumn inherited;
	
private:
	int FMinWidth;
	System::UnicodeString FTotalString;
	System::Variant FTotalValue;
	bool FTotalLoaded;
	TSummaryMode FSummaryMode;
	System::Extended FTotalFloat;
	__int64 FTotalInt;
	int FFloatDigits;
	int FFloatPrecision;
	System::Sysutils::TFloatFormat FFloatFormat;
	System::UnicodeString FFilterExpression;
	double FTableSpacePercent;
	TSortOrder __fastcall GetSortOrder();
	void __fastcall SetSortOrder(TSortOrder Value);
	int __fastcall GetSortSequence();
	void __fastcall SetSortSequence(int Value);
	System::UnicodeString __fastcall GetTotalString();
	System::Variant __fastcall GetTotalValue();
	void __fastcall SetSummaryMode(TSummaryMode Value);
	void __fastcall SetFloatDigits(const int Value);
	void __fastcall SetFloatFormat(const System::Sysutils::TFloatFormat Value);
	void __fastcall SetFloatPrecision(const int Value);
	void __fastcall SetFilterExpression(const System::UnicodeString Value);
	HIDESBASE void __fastcall SetWidth(const int Value);
	HIDESBASE int __fastcall GetWidth();
	HIDESBASE void __fastcall SetVisible(bool Value);
	HIDESBASE bool __fastcall GetVisible();
	void __fastcall ResetTotal();
	void __fastcall LoadTotal();
	void __fastcall SetTotal();
	bool __fastcall CanBeSorted();
	TCRColumnTitle* __fastcall GetTitle();
	HIDESBASE void __fastcall SetTitle(TCRColumnTitle* Value);
	
protected:
	virtual Vcl::Dbgrids::TColumnTitle* __fastcall CreateTitle();
	void __fastcall ChangedTitle(bool Rebild);
	System::UnicodeString __fastcall GetFilterExpression(const System::UnicodeString RawFilter);
	
public:
	__fastcall virtual TCRColumn(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::UnicodeString TotalString = {read=GetTotalString, write=FTotalString};
	__property System::Variant TotalValue = {read=GetTotalValue};
	
__published:
	__property int Width = {read=GetWidth, write=SetWidth, nodefault};
	__property bool Visible = {read=GetVisible, write=SetVisible, nodefault};
	__property System::UnicodeString FilterExpression = {read=FFilterExpression, write=SetFilterExpression};
	__property int MinWidth = {read=FMinWidth, write=FMinWidth, default=0};
	__property TSortOrder SortOrder = {read=GetSortOrder, write=SetSortOrder, default=0};
	__property int SortSequence = {read=GetSortSequence, write=SetSortSequence, default=0};
	__property TSummaryMode SummaryMode = {read=FSummaryMode, write=SetSummaryMode, default=0};
	__property System::Sysutils::TFloatFormat FloatFormat = {read=FFloatFormat, write=SetFloatFormat, default=0};
	__property int FloatPrecision = {read=FFloatPrecision, write=SetFloatPrecision, default=0};
	__property int FloatDigits = {read=FFloatDigits, write=SetFloatDigits, default=0};
	__property TCRColumnTitle* Title = {read=GetTitle, write=SetTitle};
public:
	/* TColumn.Destroy */ inline __fastcall virtual ~TCRColumn() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRDBGridColumns : public Vcl::Dbgrids::TDBGridColumns
{
	typedef Vcl::Dbgrids::TDBGridColumns inherited;
	
public:
	TCRColumn* operator[](int Index) { return this->Items[Index]; }
	
private:
	HIDESBASE TCRColumn* __fastcall GetColumn(int Index);
	HIDESBASE void __fastcall SetColumn(int Index, TCRColumn* Value);
	void __fastcall ColumnAdded();
	
public:
	__property TCRColumn* Items[int Index] = {read=GetColumn, write=SetColumn/*, default*/};
public:
	/* TDBGridColumns.Create */ inline __fastcall TCRDBGridColumns(Vcl::Dbgrids::TCustomDBGrid* Grid, Vcl::Dbgrids::TColumnClass ColumnClass) : Vcl::Dbgrids::TDBGridColumns(Grid, ColumnClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TCRDBGridColumns() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRGridTitleEdit : public Vcl::Stdctrls::TCustomStaticText
{
	typedef Vcl::Stdctrls::TCustomStaticText inherited;
	
	
private:
	typedef System::DynamicArray<System::UnicodeString> _TCRGridTitleEdit__1;
	
	
private:
	TCRDBGrid* FCRDBGrid;
	Vcl::Stdctrls::TEdit* FEdit;
	bool FAsFilter;
	Vcl::Dbgrids::TColumn* FActiveColumn;
	_TCRGridTitleEdit__1 FFilterExpressions;
	bool FEditingFilter;
	void __fastcall SetCRDBGrid(TCRDBGrid* const Value);
	void __fastcall FEditKeyPress(System::TObject* Sender, System::WideChar &Key);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Message);
	void __fastcall FEditKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall FEditChange(System::TObject* Sender);
	void __fastcall FEditExit(System::TObject* Sender);
	void __fastcall ProcessEdit();
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
	void __fastcall GotoUpperCell();
	void __fastcall GotoLowerCell();
	void __fastcall GotoNextCell();
	void __fastcall GotoPrevCell();
	void __fastcall SetEditingFilter(const bool Value);
	void __fastcall PostFilter();
	
protected:
	virtual void __fastcall PaintWindow(HDC DC);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall DoExit();
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	
public:
	__fastcall virtual TCRGridTitleEdit(System::Classes::TComponent* AOwner);
	virtual void __fastcall SetFocus();
	void __fastcall ActivateAt(const System::Types::TRect &ARect, Vcl::Dbgrids::TColumn* ActiveColumn, bool AsFilter);
	void __fastcall SetClientRect(const System::Types::TRect &ARect);
	void __fastcall StartEdit();
	void __fastcall StopEdit(bool AcceptChanges);
	__property TCRDBGrid* CRDBGrid = {read=FCRDBGrid, write=SetCRDBGrid};
	__property Vcl::Stdctrls::TEdit* Edit = {read=FEdit};
	__property bool EditingFilter = {read=FEditingFilter, write=SetEditingFilter, nodefault};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCRGridTitleEdit(HWND ParentWindow) : Vcl::Stdctrls::TCustomStaticText(ParentWindow) { }
	/* TWinControl.Destroy */ inline __fastcall virtual ~TCRGridTitleEdit() { }
	
};


class PASCALIMPLEMENTATION TMemoEditorForm : public Vcl::Forms::TCustomForm
{
	typedef Vcl::Forms::TCustomForm inherited;
	
private:
	Vcl::Stdctrls::TMemo* FMemo;
	Vcl::Stdctrls::TButton* FOKBtn;
	Vcl::Stdctrls::TButton* FCancelBtn;
	bool FReadOnly;
	Vcl::Stdctrls::TCheckBox* FCheckBox;
	void __fastcall SetReadOnly(const bool Value);
	void __fastcall MemoKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall CheckBoxClick(System::TObject* Sender);
	
public:
	__fastcall virtual TMemoEditorForm(System::Classes::TComponent* AOwner);
	virtual bool __fastcall CloseQuery();
	__property bool ReadOnly = {read=FReadOnly, write=SetReadOnly, nodefault};
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TMemoEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TCustomForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TMemoEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TMemoEditorForm(HWND ParentWindow) : Vcl::Forms::TCustomForm(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TCRDBGridOptionEx : unsigned char { dgeEnableSort, dgeFilterBar, dgeLocalFilter, dgeLocalSorting, dgeRecordCount, dgeSearchBar, dgeStretch, dgeSummary };

typedef System::Set<TCRDBGridOptionEx, TCRDBGridOptionEx::dgeEnableSort, TCRDBGridOptionEx::dgeSummary> TCRDBGridOptionsEx;

enum DECLSPEC_DENUM Crgrid__6 : unsigned char { geHighlight, geActiveRow, geMultiSelected };

typedef System::Set<Crgrid__6, Crgrid__6::geHighlight, Crgrid__6::geMultiSelected> TGridDrawStateEx;

typedef void __fastcall (__closure *TGetCellParamsEvent)(System::TObject* Sender, Data::Db::TField* Field, Vcl::Graphics::TFont* AFont, System::Uitypes::TColor &Background, Vcl::Grids::TGridDrawState State, TGridDrawStateEx StateEx);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSortColInfo : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int Index;
	bool Desc;
public:
	/* TObject.Create */ inline __fastcall TSortColInfo() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TSortColInfo() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TIndicatorColButton : unsigned char { icbNone, icbMenu, icbFilter, icbSearch };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRGridDataLink : public Vcl::Dbgrids::TGridDataLink
{
	typedef Vcl::Dbgrids::TGridDataLink inherited;
	
protected:
	bool FDataSetChanging;
	virtual void __fastcall DataSetChanged();
public:
	/* TGridDataLink.Create */ inline __fastcall TCRGridDataLink(Vcl::Dbgrids::TCustomDBGrid* AGrid) : Vcl::Dbgrids::TGridDataLink(AGrid) { }
	/* TGridDataLink.Destroy */ inline __fastcall virtual ~TCRGridDataLink() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRDBGrid : public Vcl::Dbgrids::TCustomDBGrid
{
	typedef Vcl::Dbgrids::TCustomDBGrid inherited;
	
private:
	bool FDefaultDrawing;
	TCRDBGridOptionsEx FOptionsEx;
	bool FSoft;
	TGetCellParamsEvent FOnGetCellParams;
	bool FExecSorting;
	bool FExecColAjust;
	System::Classes::TList* FSortInfo;
	bool FActiveRowSelected;
	int FTitleButtonDown;
	bool FTitleBarUp;
	int FOldTitleButtonDown;
	int FCellButtonDown;
	int FCellButtonRow;
	int FCellButtonCol;
	bool FCellButtonPressed;
	System::Types::TRect FCellButtonRect;
	System::Types::TRect FCellButtonBRect;
	int FTotalYOffset;
	TOnMemoClick FOnMemoClick;
	System::WideChar FLevelDelimiterChar;
	TIndicatorColButton FIndicatorColBtnDown;
	TIndicatorColButton FOldIndicatorColBtnDown;
	Vcl::Menus::TPopupMenu* FOptionsMenu;
	Vcl::Menus::TPopupMenu* FOptionsMenuDef;
	TCRGridTitleEdit* CRGridTitleEdit;
	System::Types::TRect FStatusRect;
	bool FFiltered;
	bool FContinueEditingFilter;
	int FMemoWidth;
	int FMemoHeight;
	bool FMemoWordWrap;
	void __fastcall SetOptionsEx(TCRDBGridOptionsEx Value);
	void __fastcall UpdateHeaderHeight();
	HIDESBASE void __fastcall RecordChanged(Data::Db::TField* Field);
	void __fastcall DrawButton(int X, int Y, bool State);
	bool __fastcall IsOnButton(int X, int Y);
	System::Types::TRect __fastcall GetButtonRect(const Vcl::Grids::TGridCoord &Cell);
	void __fastcall SetLevelDelimiterchar(const System::WideChar Value);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TWMSetCursor &Message);
	System::Types::TRect __fastcall CalcSearchBar(Vcl::Dbgrids::TColumn* Column);
	System::Types::TRect __fastcall CalcFilterBar(Vcl::Dbgrids::TColumn* Column);
	bool __fastcall MouseInFilterBar(int X, int Y, Vcl::Dbgrids::TColumn* Column = (Vcl::Dbgrids::TColumn*)(0x0));
	bool __fastcall MouseInFilterEdit(int X, int Y, Vcl::Dbgrids::TColumn* Column = (Vcl::Dbgrids::TColumn*)(0x0));
	bool __fastcall MouseInSortBar(int X, int Y, Vcl::Dbgrids::TColumn* Column = (Vcl::Dbgrids::TColumn*)(0x0));
	bool __fastcall MouseInSortEdit(int X, int Y, Vcl::Dbgrids::TColumn* Column = (Vcl::Dbgrids::TColumn*)(0x0));
	bool __fastcall MouseInLowerstLevel(int X, int Y, Vcl::Dbgrids::TColumn* Column = (Vcl::Dbgrids::TColumn*)(0x0));
	void __fastcall DoOnMemoClick(Vcl::Dbgrids::TColumn* Column);
	void __fastcall DrawTitleBarCell(Vcl::Graphics::TCanvas* Canvas, Vcl::Dbgrids::TColumn* Column, const System::Types::TRect &Rect, System::UnicodeString Text);
	void __fastcall DrawTitleIndicatorCell(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &ARect);
	TIndicatorColButton __fastcall GetIndicatorButton(int X, int Y);
	void __fastcall IndicatorClick(TIndicatorColButton Button, int X, int Y);
	void __fastcall BuildMenu();
	void __fastcall FilteredItemClick(System::TObject* Sender);
	void __fastcall FilterItemClick(System::TObject* Sender);
	void __fastcall SearchItemClick(System::TObject* Sender);
	void __fastcall CalcTableSpacePercent();
	void __fastcall SetFiltered(const bool Value);
	HIDESBASE void __fastcall UpdateRowCount();
	TCRDBGridColumns* __fastcall GetColumns();
	HIDESBASE void __fastcall SetColumns(TCRDBGridColumns* const Value);
	
protected:
	int FHeaderHeight;
	bool FExecSizing;
	virtual System::Types::TRect __fastcall GetClientRect();
	virtual void __fastcall Loaded();
	DYNAMIC Vcl::Dbgrids::TDBGridColumns* __fastcall CreateColumns();
	DYNAMIC Vcl::Dbgrids::TGridDataLink* __fastcall CreateDataLink();
	void __fastcall Reorder();
	TSortColInfo* __fastcall FindSortColInfo(int Index, int &SortNum);
	DYNAMIC void __fastcall ColWidthsChanged();
	DYNAMIC void __fastcall Resize();
	void __fastcall ResizeColumns(int ResizedColumn = 0xffffffff);
	DYNAMIC bool __fastcall EndColumnDrag(int &Origin, int &Destination, const System::Types::TPoint &MousePt);
	DYNAMIC void __fastcall DrawColumnCell(const System::Types::TRect &Rect, int DataCol, Vcl::Dbgrids::TColumn* Column, Vcl::Grids::TGridDrawState State);
	DYNAMIC void __fastcall GetCellProps(Data::Db::TField* Field, Vcl::Graphics::TFont* AFont, System::Uitypes::TColor &Background, Vcl::Grids::TGridDrawState State, TGridDrawStateEx StateEx);
	virtual void __fastcall DrawCell(int ACol, int ARow, const System::Types::TRect &ARect, Vcl::Grids::TGridDrawState AState);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall LinkActive(bool Value);
	virtual void __fastcall Paint();
	void __fastcall ResetTotals();
	void __fastcall LoadTotals();
	virtual bool __fastcall CanEditShow();
	DYNAMIC void __fastcall TopLeftChanged();
	DYNAMIC void __fastcall DoExit();
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Message);
	DYNAMIC void __fastcall TitleClick(Vcl::Dbgrids::TColumn* Column);
	HIDESBASE void __fastcall MoveColRow(int ACol, int ARow, bool MoveAnchor, bool Show);
	HIDESBASE int __fastcall DataToRawColumn(int ACol);
	HIDESBASE void __fastcall InvalidateCol(int ACol);
	HIDESBASE void __fastcall InvalidateRow(int ARow);
	virtual void __fastcall LayoutChanged();
	__property DefaultRowHeight = {default=24};
	__property DataLink;
	
public:
	int __fastcall GetGridSize();
	__fastcall virtual TCRDBGrid(System::Classes::TComponent* Owner);
	HIDESBASE void __fastcall DataChanged();
	__fastcall virtual ~TCRDBGrid();
	void __fastcall ClearSorting();
	void __fastcall ClearFilters();
	void __fastcall ActivateFilterEdit(Vcl::Dbgrids::TColumn* Column);
	void __fastcall ActivateSearchEdit(Vcl::Dbgrids::TColumn* Column);
	__property Canvas;
	__property SelectedRows;
	void __fastcall CalcTitleLevel(int Level, System::Types::TRect &aRect);
	System::Types::TRect __fastcall GetTitleLevel(int Level);
	void __fastcall ApplyFilter();
	void __fastcall AdjustColumns();
	__property Col;
	__property Row;
	__property TopRow;
	__property LeftCol;
	__property Vcl::Menus::TPopupMenu* OptionsMenu = {read=FOptionsMenu, write=FOptionsMenu};
	
__published:
	__property bool DefaultDrawing = {read=FDefaultDrawing, write=FDefaultDrawing, default=1};
	__property System::WideChar LevelDelimiterChar = {read=FLevelDelimiterChar, write=SetLevelDelimiterchar, default=124};
	__property bool Filtered = {read=FFiltered, write=SetFiltered, default=1};
	__property TCRDBGridOptionsEx OptionsEx = {read=FOptionsEx, write=SetOptionsEx, default=29};
	__property TOnMemoClick OnMemoClick = {read=FOnMemoClick, write=FOnMemoClick};
	__property TGetCellParamsEvent OnGetCellParams = {read=FOnGetCellParams, write=FOnGetCellParams};
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property BiDiMode;
	__property BorderStyle = {default=1};
	__property Color = {default=-16777211};
	__property TCRDBGridColumns* Columns = {read=GetColumns, write=SetColumns, stored=false};
	__property Constraints;
	__property Ctl3D;
	__property DataSource;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property FixedColor = {default=-16777201};
	__property Font;
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property Options = {default=27901};
	__property ParentBiDiMode = {default=1};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ReadOnly = {default=0};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TitleFont;
	__property Visible = {default=1};
	__property OnCellClick;
	__property OnColEnter;
	__property OnColExit;
	__property OnColumnMoved;
	__property OnDrawDataCell;
	__property OnDrawColumnCell;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEditButtonClick;
	__property OnEndDock;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDock;
	__property OnStartDrag;
	__property OnTitleClick;
public:
	/* TWinControl.CreateParented */ inline __fastcall TCRDBGrid(HWND ParentWindow) : Vcl::Dbgrids::TCustomDBGrid(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _SFiltered;
#define Crgrid_SFiltered System::LoadResourceString(&Crgrid::_SFiltered)
extern DELPHI_PACKAGE System::ResourceString _SFilterBar;
#define Crgrid_SFilterBar System::LoadResourceString(&Crgrid::_SFilterBar)
extern DELPHI_PACKAGE System::ResourceString _SSearchBar;
#define Crgrid_SSearchBar System::LoadResourceString(&Crgrid::_SSearchBar)
extern DELPHI_PACKAGE System::ResourceString _sWordWrap;
#define Crgrid_sWordWrap System::LoadResourceString(&Crgrid::_sWordWrap)
extern DELPHI_PACKAGE System::ResourceString _SOK;
#define Crgrid_SOK System::LoadResourceString(&Crgrid::_SOK)
extern DELPHI_PACKAGE System::ResourceString _SCancel;
#define Crgrid_SCancel System::LoadResourceString(&Crgrid::_SCancel)
extern DELPHI_PACKAGE System::ResourceString _SClose;
#define Crgrid_SClose System::LoadResourceString(&Crgrid::_SClose)
extern DELPHI_PACKAGE System::ResourceString _fmtModifiedWarning;
#define Crgrid_fmtModifiedWarning System::LoadResourceString(&Crgrid::_fmtModifiedWarning)
}	/* namespace Crgrid */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRGRID)
using namespace Crgrid;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrgridHPP
