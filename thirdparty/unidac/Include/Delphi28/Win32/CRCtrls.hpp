// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRCtrls.pas' rev: 35.00 (Windows)

#ifndef CrctrlsHPP
#define CrctrlsHPP

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
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.Forms.hpp>
#include <System.Types.hpp>
#include <Vcl.Mask.hpp>
#include <Vcl.StdCtrls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crctrls
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRStringGrid;
class DELPHICLASS TCRInplaceEditList;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnPaint)(HDC DC);

typedef bool __fastcall (__closure *TOnCellCheck)(int ACol, int ARow);

class PASCALIMPLEMENTATION TCRStringGrid : public Vcl::Grids::TStringGrid
{
	typedef Vcl::Grids::TStringGrid inherited;
	
private:
	TOnPaint FOnPaint;
	TOnCellCheck FOnEditCell;
	TOnCellCheck FOnComboCell;
	TOnCellCheck FOnNumericCell;
	Vcl::Grids::TOnGetPickListItems FOnGetPickListItems;
	
protected:
	bool __fastcall IsComboBox(int ACol, int ARow);
	bool __fastcall IsNumeric(int ACol, int ARow);
	virtual bool __fastcall CanEditShow();
	virtual Vcl::Grids::TInplaceEdit* __fastcall CreateEditor();
	DYNAMIC Vcl::Grids::TEditStyle __fastcall GetEditStyle(int ACol, int ARow);
	virtual void __fastcall PaintWindow(HDC DC);
	DYNAMIC System::UnicodeString __fastcall GetEditText(int ACol, int ARow);
	DYNAMIC void __fastcall SetEditText(int ACol, int ARow, const System::UnicodeString Value);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	__property Col;
	__property Row;
	
public:
	void __fastcall PaintRectTo(const System::Types::TRect &Rect, HDC DC, int X, int Y)/* overload */;
	__property TOnPaint OnPaint = {read=FOnPaint, write=FOnPaint};
	__property TOnCellCheck OnEditCell = {read=FOnEditCell, write=FOnEditCell};
	__property TOnCellCheck OnComboCell = {read=FOnComboCell, write=FOnComboCell};
	__property TOnCellCheck OnNumericCell = {read=FOnNumericCell, write=FOnNumericCell};
	__property Vcl::Grids::TOnGetPickListItems OnGetPickListItems = {read=FOnGetPickListItems, write=FOnGetPickListItems};
public:
	/* TStringGrid.Create */ inline __fastcall virtual TCRStringGrid(System::Classes::TComponent* AOwner) : Vcl::Grids::TStringGrid(AOwner) { }
	/* TStringGrid.Destroy */ inline __fastcall virtual ~TCRStringGrid() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCRStringGrid(HWND ParentWindow) : Vcl::Grids::TStringGrid(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TCRInplaceEditList : public Vcl::Grids::TInplaceEditList
{
	typedef Vcl::Grids::TInplaceEditList inherited;
	
protected:
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
public:
	/* TInplaceEditList.Create */ inline __fastcall virtual TCRInplaceEditList(System::Classes::TComponent* Owner) : Vcl::Grids::TInplaceEditList(Owner) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCRInplaceEditList(HWND ParentWindow) : Vcl::Grids::TInplaceEditList(ParentWindow) { }
	/* TWinControl.Destroy */ inline __fastcall virtual ~TCRInplaceEditList() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crctrls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRCTRLS)
using namespace Crctrls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrctrlsHPP
