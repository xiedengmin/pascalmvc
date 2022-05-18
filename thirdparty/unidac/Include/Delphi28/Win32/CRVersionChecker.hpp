// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVersionChecker.pas' rev: 35.00 (Windows)

#ifndef CrversioncheckerHPP
#define CrversioncheckerHPP

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
#include <Winapi.WinInet.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.Win.Registry.hpp>
#include <Vcl.Graphics.hpp>
#include <System.StrUtils.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRHttp.hpp>
#include <CLRClasses.hpp>
#include <HelpUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crversionchecker
{
//-- forward type declarations -----------------------------------------------
struct TFileVersion;
struct TDACInfo;
struct TDACVersion;
class DELPHICLASS TCustomVersionChecker;
class DELPHICLASS TVersionChecker;
class DELPHICLASS TVersionCheckerGet;
class DELPHICLASS TVersionCheckerPost;
class DELPHICLASS TCheckUpdatesForm;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TResultCode : unsigned char { rcSuccess, rcFileNotFound, rcIncorrectSecurity, rcProductNotFound, rcInvalidUrl, rcNoPermissions };

struct DECLSPEC_DRECORD TFileVersion
{
public:
	int Major;
	int Minor;
	int Release;
	int Build;
	System::UnicodeString Edition;
};


struct DECLSPEC_DRECORD TDACInfo
{
public:
	System::UnicodeString Name;
	System::UnicodeString Version;
	System::UnicodeString Edition;
};


struct DECLSPEC_DRECORD TDACVersion
{
public:
	System::UnicodeString Name;
	System::UnicodeString CurrentVersion;
	System::UnicodeString NewVersion;
	System::UnicodeString Edition;
};


typedef System::StaticArray<System::UnicodeString, 6> Crversionchecker__1;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomVersionChecker : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	__classmethod void __fastcall ShowDACUpdates();
	__classmethod bool __fastcall NeedCheck();
	__classmethod System::UnicodeString __fastcall GetUrlContent(const System::UnicodeString Url);
	__classmethod System::UnicodeString __fastcall ExtractTextVersion(const System::UnicodeString ResultString);
	__classmethod System::UnicodeString __fastcall ExtractJsonVersion(const System::UnicodeString ResultString);
	
public:
	__classmethod int __fastcall VersionToInt(System::UnicodeString Version);
	__classmethod TFileVersion __fastcall GetFileVersion(System::UnicodeString FileName);
	__classmethod virtual System::UnicodeString __fastcall GetLatestVersion(const System::UnicodeString ProductName, const System::UnicodeString ProductEdition, const System::UnicodeString ProductVersion);
public:
	/* TObject.Create */ inline __fastcall TCustomVersionChecker() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCustomVersionChecker() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVersionChecker : public TCustomVersionChecker
{
	typedef TCustomVersionChecker inherited;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLatestVersion(const System::UnicodeString ProductName, const System::UnicodeString ProductEdition, const System::UnicodeString ProductVersion);
public:
	/* TObject.Create */ inline __fastcall TVersionChecker() : TCustomVersionChecker() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TVersionChecker() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVersionCheckerGet : public TCustomVersionChecker
{
	typedef TCustomVersionChecker inherited;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLatestVersion(const System::UnicodeString ProductName, const System::UnicodeString ProductEdition, const System::UnicodeString ProductVersion);
public:
	/* TObject.Create */ inline __fastcall TVersionCheckerGet() : TCustomVersionChecker() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TVersionCheckerGet() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVersionCheckerPost : public TCustomVersionChecker
{
	typedef TCustomVersionChecker inherited;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLatestVersion(const System::UnicodeString ProductName, const System::UnicodeString ProductEdition, const System::UnicodeString ProductVersion);
public:
	/* TObject.Create */ inline __fastcall TVersionCheckerPost() : TCustomVersionChecker() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TVersionCheckerPost() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCheckUpdatesForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
private:
	Vcl::Extctrls::TPanel* pCaption;
	Vcl::Extctrls::TPanel* pDACs;
	Vcl::Extctrls::TPanel* pControls;
	Vcl::Extctrls::TImage* img;
	Vcl::Stdctrls::TLabel* lbl;
	Vcl::Stdctrls::TButton* bt;
	Vcl::Stdctrls::TCheckBox* cb;
	Vcl::Extctrls::TBevel* bevTop;
	Vcl::Extctrls::TBevel* bevBottom;
	void __fastcall FormClose(System::TObject* Sender, System::Uitypes::TCloseAction &Action);
	void __fastcall ButtonClick(System::TObject* Sender);
	void __fastcall LabelClick(System::TObject* Sender);
	void __fastcall LabelMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall PanelMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TCheckUpdatesForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCheckUpdatesForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCheckUpdatesForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCheckUpdatesForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Crversionchecker__1 ResultErrorMessages;
#define DACUpdatesRegistryKey L"Software\\Devart\\DAC\\CheckUpdates"
}	/* namespace Crversionchecker */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVERSIONCHECKER)
using namespace Crversionchecker;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrversioncheckerHPP
