// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniConnectForm.pas' rev: 35.00 (Windows)

#ifndef UniconnectformHPP
#define UniconnectformHPP

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
#include <DBAccess.hpp>
#include <UniProvider.hpp>
#include <Uni.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniconnectform
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniConnectForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniConnectForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* Panel;
	Vcl::Stdctrls::TLabel* lbUsername;
	Vcl::Stdctrls::TLabel* lbPassword;
	Vcl::Stdctrls::TLabel* lbServer;
	Vcl::Stdctrls::TLabel* lbPort;
	Vcl::Stdctrls::TEdit* edUserName;
	Vcl::Stdctrls::TEdit* edPassword;
	Vcl::Stdctrls::TComboBox* edServer;
	Vcl::Stdctrls::TButton* btConnect;
	Vcl::Stdctrls::TButton* btCancel;
	Vcl::Stdctrls::TComboBox* edProvider;
	Vcl::Stdctrls::TLabel* lbProvider;
	Vcl::Stdctrls::TLabel* lbDatabase;
	Vcl::Stdctrls::TComboBox* edDatabase;
	Vcl::Stdctrls::TEdit* edPort;
	void __fastcall btConnectClick(System::TObject* Sender);
	void __fastcall edProviderChange(System::TObject* Sender);
	void __fastcall edServerDropDown(System::TObject* Sender);
	void __fastcall edDatabaseDropDown(System::TObject* Sender);
	void __fastcall edServerSelect(System::TObject* Sender);
	
private:
	Dbaccess::TCustomConnectDialog* FConnectDialog;
	int FRetries;
	bool FRetry;
	bool FProviderGot;
	void __fastcall SetConnectDialog(Dbaccess::TCustomConnectDialog* Value);
	
protected:
	virtual void __fastcall DoInit();
	virtual void __fastcall DoConnect();
	void __fastcall AssignPort(const int Value);
	
__published:
	__property Dbaccess::TCustomConnectDialog* ConnectDialog = {read=FConnectDialog, write=SetConnectDialog};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TUniConnectForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniConnectForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TUniConnectForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniConnectForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uniconnectform */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNICONNECTFORM)
using namespace Uniconnectform;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniconnectformHPP
