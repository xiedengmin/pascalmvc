// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CREditor.pas' rev: 34.00 (Windows)

#ifndef CreditorHPP
#define CreditorHPP

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
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.DBGrids.hpp>
#include <CRTypes.hpp>
#include <CRDesignUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Creditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCREditorForm;
class DELPHICLASS TDAMemo;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TCREditorClass;

class PASCALIMPLEMENTATION TCREditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* BtnPanel;
	Vcl::Buttons::TBitBtn* btOk;
	Vcl::Buttons::TBitBtn* btCancel;
	Vcl::Extctrls::TImage* imCorner;
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall FormHide(System::TObject* Sender);
	void __fastcall SaveClick(System::TObject* Sender);
	void __fastcall CloseClick(System::TObject* Sender);
	void __fastcall FormCloseQuery(System::TObject* Sender, bool &CanClose);
	void __fastcall FormKeyPress(System::TObject* Sender, System::WideChar &Key);
	
private:
	Vcl::Forms::TPosition FOldPosition;
	bool FConfirmCancel;
	void __fastcall SetConfirmCancel(const bool Value);
	
protected:
	bool FModified;
	System::UnicodeString FolderName;
	Crdesignutils::TCRDesignUtilsClass FCRDesignUtilsClass;
	System::UnicodeString FInitialProperty;
	System::Classes::TPersistent* FSynSQLSyn;
	bool FUseSynEdit;
	virtual bool __fastcall GetModified();
	virtual void __fastcall SetModified(bool Value);
	void __fastcall ReplaceMemos();
	void __fastcall ExtMenuItemClick(System::TObject* Sender);
	void __fastcall ExtMenuPopup(System::TObject* Sender);
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall DoInit();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoSave();
	virtual void __fastcall DoFinish();
	virtual void __fastcall SaveControlData();
	virtual bool __fastcall SaveState();
	virtual bool __fastcall LoadState();
	HIDESBASE virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual System::Classes::TComponent* __fastcall GetLocalComponent();
	System::UnicodeString __fastcall KeyPath();
	__property bool Modified = {read=GetModified, write=SetModified, nodefault};
	
public:
	__fastcall virtual TCREditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass);
	void __fastcall AddMenu(Vcl::Controls::TWinControl* Memo);
	void __fastcall ReplaceMemo(Vcl::Controls::TWinControl* &Memo, bool DrawGutter);
	__property bool ConfirmCancel = {read=FConfirmCancel, write=SetConfirmCancel, nodefault};
	__property Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass = {read=FCRDesignUtilsClass};
	__property System::Classes::TComponent* Component = {read=GetComponent, write=SetComponent};
	__property System::Classes::TComponent* LocalComponent = {read=GetLocalComponent};
	__property System::UnicodeString InitialProperty = {read=FInitialProperty, write=FInitialProperty};
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCREditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCREditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCREditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TDAMemo : public Vcl::Stdctrls::TMemo
{
	typedef Vcl::Stdctrls::TMemo inherited;
	
protected:
	bool BackSpacePressed;
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
public:
	/* TCustomMemo.Create */ inline __fastcall virtual TDAMemo(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TMemo(AOwner) { }
	/* TCustomMemo.Destroy */ inline __fastcall virtual ~TDAMemo() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAMemo(HWND ParentWindow) : Vcl::Stdctrls::TMemo(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall ReplaceControl(Vcl::Controls::TWinControl* &Control, const Vcl::Controls::TWinControlClass NewClass);
extern DELPHI_PACKAGE bool __fastcall ReplaceGridToCRGrid(Vcl::Dbgrids::TCustomDBGrid* &DBGrid);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetMemoText(Vcl::Controls::TWinControl* Memo);
extern DELPHI_PACKAGE void __fastcall SetMemoText(Vcl::Controls::TWinControl* Memo, const System::UnicodeString Value);
extern DELPHI_PACKAGE bool __fastcall GetReadOnly(Vcl::Controls::TWinControl* Memo);
extern DELPHI_PACKAGE void __fastcall SetReadOnly(Vcl::Controls::TWinControl* Memo, bool Value);
extern DELPHI_PACKAGE int __fastcall GetSelStart(Vcl::Controls::TWinControl* Memo);
extern DELPHI_PACKAGE void __fastcall SetSelStart(Vcl::Controls::TWinControl* Memo, int Value);
extern DELPHI_PACKAGE void __fastcall SetSelLength(Vcl::Controls::TWinControl* Memo, int Value);
extern DELPHI_PACKAGE void __fastcall LoadFromFile(Vcl::Controls::TWinControl* Memo, System::UnicodeString FileName);
extern DELPHI_PACKAGE void __fastcall SaveToFile(Vcl::Controls::TWinControl* Memo, System::UnicodeString FileName);
}	/* namespace Creditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CREDITOR)
using namespace Creditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CreditorHPP
