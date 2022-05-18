// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniDataTypeMapFrame.pas' rev: 34.00 (Windows)

#ifndef UnidatatypemapframeHPP
#define UnidatatypemapframeHPP

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
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <System.Variants.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Grids.hpp>
#include <Data.DB.hpp>
#include <MemDS.hpp>
#include <CRDataTypeMap.hpp>
#include <DBAccess.hpp>
#include <Uni.hpp>
#include <CRFrame.hpp>
#include <DADataTypeMapFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unidatatypemapframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniDataTypeMapFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniDataTypeMapFrame : public Dadatatypemapframe::TDADataTypeMapFrame
{
	typedef Dadatatypemapframe::TDADataTypeMapFrame inherited;
	
__published:
	Vcl::Stdctrls::TLabel* lbProvider;
	Vcl::Stdctrls::TComboBox* edProvider;
	void __fastcall edProviderChange(System::TObject* Sender);
	
private:
	System::UnicodeString FProviderName;
	Uni::TUniConnection* __fastcall GetConnection();
	
protected:
	virtual void __fastcall DoActivate();
	virtual Crdatatypemap::TConverterManagerClass __fastcall GetConverterManagerClass();
	virtual void __fastcall ResizeGrid();
public:
	/* TDADataTypeMapFrame.Create */ inline __fastcall virtual TUniDataTypeMapFrame(System::Classes::TComponent* AOwner) : Dadatatypemapframe::TDADataTypeMapFrame(AOwner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TUniDataTypeMapFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniDataTypeMapFrame(HWND ParentWindow) : Dadatatypemapframe::TDADataTypeMapFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unidatatypemapframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIDATATYPEMAPFRAME)
using namespace Unidatatypemapframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnidatatypemapframeHPP
