// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DASQLFrame.pas' rev: 35.00 (Windows)

#ifndef DasqlframeHPP
#define DasqlframeHPP

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
#include <CRTypes.hpp>
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dasqlframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDASQLFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDASQLFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Stdctrls::TMemo* meSQL;
	Vcl::Extctrls::TPanel* Panel1;
	void __fastcall meSQLExit(System::TObject* Sender);
	
protected:
	System::UnicodeString __fastcall GetSQLText();
	void __fastcall SetSQLText(const System::UnicodeString Value);
	virtual void __fastcall LoadMemo();
	virtual void __fastcall SaveMemo();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	virtual System::Classes::TStrings* __fastcall GetLocalComponentSQL();
	virtual void __fastcall SetLocalComponentSQL(System::Classes::TStrings* Value);
	__property System::UnicodeString SQLText = {read=GetSQLText, write=SetSQLText};
	__property System::Classes::TStrings* LocalComponentSQL = {read=GetLocalComponentSQL, write=SetLocalComponentSQL};
	
public:
	__fastcall virtual TDASQLFrame(System::Classes::TComponent* Owner);
	virtual Vcl::Controls::TWinControl* __fastcall ActiveControl();
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDASQLFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDASQLFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dasqlframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASQLFRAME)
using namespace Dasqlframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DasqlframeHPP
