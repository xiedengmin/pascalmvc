// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniSQLOptionsFrame.pas' rev: 35.00 (Windows)

#ifndef UnisqloptionsframeHPP
#define UnisqloptionsframeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <CRTypes.hpp>
#include <CRFrame.hpp>
#include <Uni.hpp>
#include <UniSpecificOptionsFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unisqloptionsframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniSQLOptionsFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniSQLOptionsFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnOptions;
	
protected:
	Unispecificoptionsframe::TUniSpecificOptionsFrame* FOptionsFrame;
	virtual int __fastcall GetOptionsFrameTop();
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	
public:
	__fastcall virtual TUniSQLOptionsFrame(System::Classes::TComponent* Owner);
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TUniSQLOptionsFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniSQLOptionsFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unisqloptionsframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNISQLOPTIONSFRAME)
using namespace Unisqloptionsframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnisqloptionsframeHPP
