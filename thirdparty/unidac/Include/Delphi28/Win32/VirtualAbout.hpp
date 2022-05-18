// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualAbout.pas' rev: 35.00 (Windows)

#ifndef VirtualaboutHPP
#define VirtualaboutHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <HelpUtils.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualabout
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVirtualDacAboutForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TVirtualDacAboutForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TButton* OKBtn;
	Vcl::Extctrls::TImage* Image1;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* lbVersion;
	Vcl::Stdctrls::TLabel* lbIDE;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TLabel* lbMail;
	Vcl::Stdctrls::TLabel* lbWeb;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Stdctrls::TLabel* Label8;
	Vcl::Stdctrls::TLabel* lblDBMonitorVer;
	Vcl::Stdctrls::TLabel* lbForum;
	Vcl::Stdctrls::TLabel* Label10;
	Vcl::Stdctrls::TLabel* lbEdition;
	Vcl::Extctrls::TBevel* Bevel2;
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall lbWebClick(System::TObject* Sender);
	void __fastcall lbMailClick(System::TObject* Sender);
	void __fastcall lbWebMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall lbMailMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall lbForumClick(System::TObject* Sender);
	void __fastcall lbForumMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TVirtualDacAboutForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TVirtualDacAboutForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TVirtualDacAboutForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualDacAboutForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall ShowAbout(void);
}	/* namespace Virtualabout */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALABOUT)
using namespace Virtualabout;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualaboutHPP
