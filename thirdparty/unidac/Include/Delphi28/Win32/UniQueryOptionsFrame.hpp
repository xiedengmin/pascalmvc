// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniQueryOptionsFrame.pas' rev: 35.00 (Windows)

#ifndef UniqueryoptionsframeHPP
#define UniqueryoptionsframeHPP

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
#include <CRFrame.hpp>
#include <Uni.hpp>
#include <UniSQLOptionsFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniqueryoptionsframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniQueryOptionsFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniQueryOptionsFrame : public Unisqloptionsframe::TUniSQLOptionsFrame
{
	typedef Unisqloptionsframe::TUniSQLOptionsFrame inherited;
	
__published:
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TComboBox* cbLock1;
	Vcl::Stdctrls::TLabel* lbUpdatingTable;
	Vcl::Stdctrls::TEdit* edUpdatingTable;
	Vcl::Stdctrls::TLabel* lbKeyFieldsLabel1;
	Vcl::Stdctrls::TEdit* edKeyFields;
	void __fastcall cbLock1Change(System::TObject* Sender);
	void __fastcall edKeyFieldsExit(System::TObject* Sender);
	void __fastcall edUpdatingTableExit(System::TObject* Sender);
	
protected:
	virtual int __fastcall GetOptionsFrameTop();
	Uni::TUniQuery* __fastcall GetLocalQuery();
	virtual void __fastcall DoActivate();
	__property Uni::TUniQuery* LocalQuery = {read=GetLocalQuery};
	
public:
	virtual Vcl::Controls::TWinControl* __fastcall ActiveControl();
public:
	/* TUniSQLOptionsFrame.Create */ inline __fastcall virtual TUniQueryOptionsFrame(System::Classes::TComponent* Owner) : Unisqloptionsframe::TUniSQLOptionsFrame(Owner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TUniQueryOptionsFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniQueryOptionsFrame(HWND ParentWindow) : Unisqloptionsframe::TUniSQLOptionsFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uniqueryoptionsframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIQUERYOPTIONSFRAME)
using namespace Uniqueryoptionsframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniqueryoptionsframeHPP
