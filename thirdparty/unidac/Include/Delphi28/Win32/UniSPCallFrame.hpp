// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniSPCallFrame.pas' rev: 35.00 (Windows)

#ifndef UnispcallframeHPP
#define UnispcallframeHPP

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
#include <Vcl.ComCtrls.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <DBAccess.hpp>
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>
#include <DASPCallFrame.hpp>
#include <CREditor.hpp>
#include <DAUpdateSQLFrame.hpp>
#include <DASQLFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unispcallframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniSPCallFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniSPCallFrame : public Daspcallframe::TDASPCallFrame
{
	typedef Daspcallframe::TDASPCallFrame inherited;
	
__published:
	Vcl::Buttons::TSpeedButton* btCreateSQL;
	Vcl::Stdctrls::TCheckBox* cbAllProcs;
	void __fastcall btCreateSQLClick(System::TObject* Sender);
	void __fastcall cbAllProcsClick(System::TObject* Sender);
	
protected:
	virtual bool __fastcall ShowAllProc();
public:
	/* TDASQLFrame.Create */ inline __fastcall virtual TUniSPCallFrame(System::Classes::TComponent* Owner) : Daspcallframe::TDASPCallFrame(Owner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TUniSPCallFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniSPCallFrame(HWND ParentWindow) : Daspcallframe::TDASPCallFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unispcallframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISPCALLFRAME)
using namespace Unispcallframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnispcallframeHPP
