// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAConnectionEditor.pas' rev: 35.00 (Windows)

#ifndef DaconnectioneditorHPP
#define DaconnectioneditorHPP

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
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <System.Win.Registry.hpp>
#include <DacVcl.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Mask.hpp>
#include <CRTypes.hpp>
#include <DBAccess.hpp>
#include <CRFrame.hpp>
#include <CREditor.hpp>
#include <CRTabEditor.hpp>
#include <CRDesignUtils.hpp>
#include <DAEditor.hpp>
#include <DADataTypeMapFrame.hpp>
#include <DADesignUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daconnectioneditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAConnectionEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAConnectionEditorForm : public Crtabeditor::TCRTabEditorForm
{
	typedef Crtabeditor::TCRTabEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shConnect;
	Vcl::Comctrls::TTabSheet* shDataTypeMapping;
	Vcl::Extctrls::TPanel* Panel;
	Vcl::Stdctrls::TLabel* lbUsername;
	Vcl::Stdctrls::TLabel* lbPassword;
	Vcl::Stdctrls::TLabel* lbServer;
	Vcl::Stdctrls::TEdit* edUsername;
	Vcl::Mask::TMaskEdit* edPassword;
	Vcl::Stdctrls::TComboBox* edServer;
	Vcl::Stdctrls::TButton* btConnect;
	Vcl::Stdctrls::TButton* btDisconnect;
	Vcl::Comctrls::TTabSheet* shInfo;
	Vcl::Comctrls::TTabSheet* shAbout;
	Vcl::Stdctrls::TMemo* meInfo;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* lbVersion;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TLabel* lbWeb;
	Vcl::Stdctrls::TLabel* lbMail;
	Vcl::Stdctrls::TLabel* lbIDE;
	Vcl::Stdctrls::TCheckBox* cbLoginPrompt;
	Vcl::Extctrls::TShape* shRed;
	Vcl::Extctrls::TShape* shYellow;
	Vcl::Extctrls::TShape* shGreen;
	Vcl::Extctrls::TImage* imPeng;
	Vcl::Stdctrls::TLabel* lbEdition;
	Vcl::Extctrls::TImage* imgLogo;
	void __fastcall btDisconnectClick(System::TObject* Sender);
	void __fastcall lbWebClick(System::TObject* Sender);
	void __fastcall lbMailClick(System::TObject* Sender);
	void __fastcall cbLoginPromptClick(System::TObject* Sender);
	void __fastcall lbWebMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall shAboutMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall lbMailMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall edUsernameChange(System::TObject* Sender);
	void __fastcall edPasswordChange(System::TObject* Sender);
	void __fastcall edServerChange(System::TObject* Sender);
	void __fastcall btConnectClick(System::TObject* Sender);
	virtual void __fastcall edServerDropDown(System::TObject* Sender);
	void __fastcall edServerKeyUp(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall edServerExit(System::TObject* Sender);
	
protected:
	Dbaccess::TCustomDAConnection* FConnection;
	Dbaccess::TCustomDAConnection* FLocalConnection;
	bool FInDoInit;
	Dbaccess::TCustomConnectDialog* FConnectDialog;
	Dadatatypemapframe::TDADataTypeMapFrame* FDataMappingFrameFrame;
	bool FIsConnected;
	System::Win::Registry::TRegistry* FRegistry;
	virtual void __fastcall GetServerList(System::Classes::TStrings* List);
	virtual void __fastcall AddServerToList();
	virtual void __fastcall ShowState(bool Yellow = false);
	virtual void __fastcall ConnToControls();
	virtual void __fastcall ControlsToConn();
	virtual void __fastcall FillInfo(Dbaccess::TCustomDAConnection* InfoConnection);
	virtual void __fastcall PerformConnect();
	virtual void __fastcall PerformDisconnect();
	virtual bool __fastcall GetIsConnected();
	virtual Dbaccess::TCustomDAConnection* __fastcall GetInfoConnection();
	virtual void __fastcall CreateLocalConnection();
	virtual void __fastcall AssignUsername(const System::UnicodeString Value);
	virtual void __fastcall AssignPassword(const System::UnicodeString Value);
	virtual void __fastcall AssignServer(const System::UnicodeString Value);
	virtual void __fastcall AssignLoginPrompt(bool Value);
	virtual Dbaccess::TConnectDialogClass __fastcall GetConnectDialogClass();
	virtual Dadatatypemapframe::TDADataTypeMapFrameClass __fastcall GetDataTypeMapFrameClass();
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall DoInit();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoSave();
	virtual void __fastcall DoSaveConnection();
	virtual void __fastcall DoFinish();
	virtual void __fastcall UpdateVersionPosition();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual System::Classes::TComponent* __fastcall GetLocalComponent();
	virtual void __fastcall DoPageControlChange(System::TObject* Sender);
	
public:
	__fastcall virtual TDAConnectionEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass);
	__fastcall virtual ~TDAConnectionEditorForm();
	__property bool IsConnected = {read=GetIsConnected, nodefault};
	__property Dbaccess::TCustomDAConnection* Connection = {read=FConnection};
	__property Dbaccess::TCustomDAConnection* LocalConnection = {read=FLocalConnection};
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDAConnectionEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Crtabeditor::TCRTabEditorForm(AOwner, Dummy) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAConnectionEditorForm(HWND ParentWindow) : Crtabeditor::TCRTabEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daconnectioneditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DACONNECTIONEDITOR)
using namespace Daconnectioneditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaconnectioneditorHPP
