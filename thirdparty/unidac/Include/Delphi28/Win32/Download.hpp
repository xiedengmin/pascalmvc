// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Download.pas' rev: 35.00 (Windows)

#ifndef DownloadHPP
#define DownloadHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.Win.Registry.hpp>

//-- user supplied -----------------------------------------------------------

namespace Download
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDownloadForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDownloadForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TButton* OKBtn;
	Vcl::Stdctrls::TButton* CancelBtn;
	Vcl::Extctrls::TBevel* Bevel1;
	Vcl::Stdctrls::TCheckBox* chkDontAsk;
	Vcl::Extctrls::TImage* Image;
	Vcl::Stdctrls::TLabel* lblQuestion;
	Vcl::Stdctrls::TButton* btHelp;
	Vcl::Stdctrls::TLabel* lblText1;
	Vcl::Stdctrls::TLabel* lblText3;
	Vcl::Stdctrls::TLabel* lblProduct;
	Vcl::Stdctrls::TLabel* lblText2;
	Vcl::Stdctrls::TLabel* lblText3Wrap;
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall OKBtnClick(System::TObject* Sender);
	void __fastcall btHelpClick(System::TObject* Sender);
	void __fastcall FormMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall lblProductMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall lblProductClick(System::TObject* Sender);
	
private:
	bool FProVersionNotice;
	
public:
	__fastcall TDownloadForm(System::Classes::TComponent* AOwner, bool ProVersionNotice)/* overload */;
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDownloadForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDownloadForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDownloadForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SetToolsCheckingParams(const System::UnicodeString ADialogCaptionStr, const System::UnicodeString AToolsNameStr, const System::UnicodeString AProjectVersionStr, const System::UnicodeString AAskIncompatibleStr, const System::UnicodeString AAskNoAddinStr, const System::UnicodeString ARegKeyStr, const System::UnicodeString AHelpProjectStr, const System::UnicodeString AHelpTopicStr, const System::UnicodeString AUrlStr, const System::UnicodeString AUrlExeStr, const System::UnicodeString AAtomName);
extern DELPHI_PACKAGE bool __fastcall NoCheckForTools(bool Incompatible, System::Win::Registry::TRegIniFile* ARegIniFile = (System::Win::Registry::TRegIniFile*)(0x0));
extern DELPHI_PACKAGE void __fastcall CheckForTools(bool Incompatible);
extern DELPHI_PACKAGE bool __fastcall DownloadTools(bool ProVersion);
}	/* namespace Download */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DOWNLOAD)
using namespace Download;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DownloadHPP
