// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniParamsFrame.pas' rev: 34.00 (Windows)

#ifndef UniparamsframeHPP
#define UniparamsframeHPP

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
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <Data.DB.hpp>
#include <MemDS.hpp>
#include <DBAccess.hpp>
#include <Uni.hpp>
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>
#include <DAParamsFrame.hpp>
#include <CRColFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniparamsframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniParamsFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniParamsFrame : public Daparamsframe::TDAParamsFrame
{
	typedef Daparamsframe::TDAParamsFrame inherited;
	
__published:
	Vcl::Stdctrls::TCheckBox* cbNational;
	Vcl::Stdctrls::TLabel* lbNational;
	
protected:
	virtual void __fastcall ItemToControls(System::Classes::TCollectionItem* Item);
	virtual void __fastcall ControlsToItem(System::Classes::TCollectionItem* Item);
	virtual void __fastcall UpdateControlsState();
	
public:
	__fastcall virtual TUniParamsFrame(System::Classes::TComponent* AOwner);
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TUniParamsFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniParamsFrame(HWND ParentWindow) : Daparamsframe::TDAParamsFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uniparamsframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIPARAMSFRAME)
using namespace Uniparamsframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniparamsframeHPP
