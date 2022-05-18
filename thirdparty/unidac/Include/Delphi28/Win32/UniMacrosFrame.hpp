// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniMacrosFrame.pas' rev: 35.00 (Windows)

#ifndef UnimacrosframeHPP
#define UnimacrosframeHPP

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
#include <Vcl.Buttons.hpp>
#include <System.Variants.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Grids.hpp>
#include <Data.DB.hpp>
#include <CRFrame.hpp>
#include <Uni.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unimacrosframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniMacrosFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniMacrosFrame : public Crframe::TCRFrame
{
	typedef Crframe::TCRFrame inherited;
	
__published:
	Vcl::Buttons::TSpeedButton* UpBtn;
	Vcl::Buttons::TSpeedButton* DownBtn;
	Vcl::Grids::TDrawGrid* sgMacros;
	Vcl::Stdctrls::TButton* btClearAllMacros;
	Vcl::Stdctrls::TButton* btDeleteMacro;
	Vcl::Stdctrls::TButton* btReplaceMacro;
	Vcl::Stdctrls::TButton* btAddMacro;
	Vcl::Stdctrls::TLabel* Label8;
	Vcl::Stdctrls::TLabel* Label7;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TEdit* edMacroValue;
	Vcl::Stdctrls::TEdit* edMacroName;
	Vcl::Stdctrls::TComboBox* edMacroCondition;
	void __fastcall btAddMacroClick(System::TObject* Sender);
	void __fastcall btDeleteMacroClick(System::TObject* Sender);
	void __fastcall UpBtnClick(System::TObject* Sender);
	void __fastcall DownBtnClick(System::TObject* Sender);
	void __fastcall sgMacrosDrawCell(System::TObject* Sender, int ACol, int ARow, const System::Types::TRect &Rect, Vcl::Grids::TGridDrawState State);
	void __fastcall btReplaceMacroClick(System::TObject* Sender);
	void __fastcall btClearAllMacrosClick(System::TObject* Sender);
	void __fastcall sgMacrosSelectCell(System::TObject* Sender, int ACol, int ARow, bool &CanSelect);
	void __fastcall edMacroConditionDropDown(System::TObject* Sender);
	
private:
	Uni::TUniConnection* __fastcall GetConnection();
	Uni::TUniConnection* __fastcall GetLocalConnection();
	
protected:
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	void __fastcall EnableMacroButtons();
	
public:
	__property Uni::TUniConnection* Connection = {read=GetConnection};
	__property Uni::TUniConnection* LocalConnection = {read=GetLocalConnection};
public:
	/* TCustomFrame.Create */ inline __fastcall virtual TUniMacrosFrame(System::Classes::TComponent* AOwner) : Crframe::TCRFrame(AOwner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TUniMacrosFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniMacrosFrame(HWND ParentWindow) : Crframe::TCRFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Unimacrosframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIMACROSFRAME)
using namespace Unimacrosframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnimacrosframeHPP
