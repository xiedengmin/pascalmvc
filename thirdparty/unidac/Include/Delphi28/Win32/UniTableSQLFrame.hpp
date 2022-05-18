// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniTableSQLFrame.pas' rev: 35.00 (Windows)

#ifndef UnitablesqlframeHPP
#define UnitablesqlframeHPP

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
#include <CREditor.hpp>
#include <DATableSQLFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unitablesqlframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniTableSQLFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniTableSQLFrame : public Datablesqlframe::TDATableSQLFrame
{
	typedef Datablesqlframe::TDATableSQLFrame inherited;
	
__published:
	Vcl::Stdctrls::TCheckBox* cbAllTables;
	void __fastcall cbAllTablesClick(System::TObject* Sender);
public:
	/* TDATableSQLFrame.Create */ inline __fastcall virtual TUniTableSQLFrame(System::Classes::TComponent* Owner) : Datablesqlframe::TDATableSQLFrame(Owner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TUniTableSQLFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniTableSQLFrame(HWND ParentWindow) : Datablesqlframe::TDATableSQLFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unitablesqlframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNITABLESQLFRAME)
using namespace Unitablesqlframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnitablesqlframeHPP
