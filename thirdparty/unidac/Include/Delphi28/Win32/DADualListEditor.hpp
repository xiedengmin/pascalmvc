// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DADualListEditor.pas' rev: 35.00 (Windows)

#ifndef DaduallisteditorHPP
#define DaduallisteditorHPP

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
#include <Vcl.Dialogs.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <CRTypes.hpp>
#include <CREditor.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daduallisteditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDADualListEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDADualListEditorForm : public Creditor::TCREditorForm
{
	typedef Creditor::TCREditorForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* Panel2;
	Vcl::Stdctrls::TLabel* SrcLabel;
	Vcl::Stdctrls::TListBox* SrcList;
	Vcl::Extctrls::TPanel* PanelButtons;
	Vcl::Buttons::TSpeedButton* IncludeBtn;
	Vcl::Buttons::TSpeedButton* IncAllBtn;
	Vcl::Buttons::TSpeedButton* ExcludeBtn;
	Vcl::Buttons::TSpeedButton* ExAllBtn;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Stdctrls::TLabel* DstLabel;
	Vcl::Stdctrls::TListBox* DstList;
	Vcl::Extctrls::TPanel* PanelButtons2;
	Vcl::Buttons::TSpeedButton* UpBtn;
	Vcl::Buttons::TSpeedButton* DownBtn;
	void __fastcall IncludeBtnClick(System::TObject* Sender);
	void __fastcall ExcludeBtnClick(System::TObject* Sender);
	void __fastcall IncAllBtnClick(System::TObject* Sender);
	void __fastcall ExcAllBtnClick(System::TObject* Sender);
	void __fastcall DstListDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	void __fastcall DstListDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall SrcListDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	void __fastcall SrcListDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall FormResize(System::TObject* Sender);
	void __fastcall UpBtnClick(System::TObject* Sender);
	void __fastcall DownBtnClick(System::TObject* Sender);
	void __fastcall DstListKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ListClick(System::TObject* Sender);
	
protected:
	virtual void __fastcall DoInit();
	void __fastcall MoveSelected(Vcl::Stdctrls::TListBox* Source, Vcl::Stdctrls::TListBox* Dest, int Index, bool SelectNext);
	void __fastcall MoveAll(Vcl::Stdctrls::TListBox* Source, Vcl::Stdctrls::TListBox* Dest, int Index);
	void __fastcall Move(Vcl::Stdctrls::TListBox* List, int FromIndex, int ToIndex);
	int __fastcall GetFirstSelection(Vcl::Stdctrls::TCustomListBox* List);
	void __fastcall SetButtons();
	virtual System::UnicodeString __fastcall GetSrcLabelCaption();
	virtual void __fastcall GetSrcListItems(System::Classes::TStrings* Items);
	virtual System::UnicodeString __fastcall GetDestLabelCaption();
	virtual void __fastcall GetDstListItems(System::Classes::TStrings* Items);
public:
	/* TCREditorForm.Create */ inline __fastcall virtual TDADualListEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Creditor::TCREditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDADualListEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Creditor::TCREditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDADualListEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDADualListEditorForm(HWND ParentWindow) : Creditor::TCREditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daduallisteditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DADUALLISTEDITOR)
using namespace Daduallisteditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaduallisteditorHPP
