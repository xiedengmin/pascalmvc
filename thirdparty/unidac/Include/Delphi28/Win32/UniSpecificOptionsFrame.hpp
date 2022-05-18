// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniSpecificOptionsFrame.pas' rev: 35.00 (Windows)

#ifndef UnispecificoptionsframeHPP
#define UnispecificoptionsframeHPP

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
#include <Vcl.ComCtrls.hpp>
#include <DacVcl.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.ValEdit.hpp>
#include <CRTypes.hpp>
#include <CRFrame.hpp>
#include <DBAccess.hpp>
#include <UniProvider.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unispecificoptionsframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TOptionsMemo;
class DELPHICLASS TUniSpecificOptionsFrame;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TOptionsType : unsigned char { otConnection, otSQL, otDataSet, otScript, otLoader, otDump, otAlerter, otTransaction };

class PASCALIMPLEMENTATION TOptionsMemo : public Vcl::Stdctrls::TMemo
{
	typedef Vcl::Stdctrls::TMemo inherited;
	
public:
	/* TCustomMemo.Create */ inline __fastcall virtual TOptionsMemo(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TMemo(AOwner) { }
	/* TCustomMemo.Destroy */ inline __fastcall virtual ~TOptionsMemo() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TOptionsMemo(HWND ParentWindow) : Vcl::Stdctrls::TMemo(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TUniSpecificOptionsFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnProvider;
	Vcl::Stdctrls::TLabel* lbProvider;
	Vcl::Stdctrls::TComboBox* edProvider;
	void __fastcall edProviderChange(System::TObject* Sender);
	
protected:
	Vcl::Valedit::TValueListEditor* edOptions;
	System::Classes::TStrings* FEditorList;
	System::UnicodeString FProviderName;
	TOptionsType FOptionsType;
	Uniprovider::TOptionsList* FOptionsList;
	System::Classes::TStrings* FSpecificOptions;
	void __fastcall edOptionsGetPickList(System::TObject* Sender, const System::UnicodeString KeyName, System::Classes::TStrings* Values);
	void __fastcall edOptionsValidate(System::TObject* Sender, int ACol, int ARow, const System::UnicodeString KeyName, const System::UnicodeString KeyValue);
	void __fastcall edOptionsDrawCell(System::TObject* Sender, int ACol, int ARow, const System::Types::TRect &Rect, Vcl::Grids::TGridDrawState State);
	void __fastcall edOptionsSetEditText(System::TObject* Sender, int ACol, int ARow, const System::UnicodeString Value);
	void __fastcall edOptionsExit(System::TObject* Sender);
	void __fastcall edOptionsChanged(System::TObject* Sender);
	bool __fastcall CheckOptionDefault(Uniprovider::TOption* Option, System::UnicodeString &Value);
	virtual void __fastcall DoFinish();
	DYNAMIC void __fastcall Resize();
	
public:
	__fastcall virtual TUniSpecificOptionsFrame(System::Classes::TComponent* Owner);
	void __fastcall InitOptions(System::Classes::TStrings* SpecificOptions);
	void __fastcall SaveOptions();
	void __fastcall LoadOptions(const System::UnicodeString ProviderName, TOptionsType OptionsType);
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TUniSpecificOptionsFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniSpecificOptionsFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unispecificoptionsframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISPECIFICOPTIONSFRAME)
using namespace Unispecificoptionsframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnispecificoptionsframeHPP
