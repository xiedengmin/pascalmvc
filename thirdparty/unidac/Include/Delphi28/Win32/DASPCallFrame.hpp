// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DASPCallFrame.pas' rev: 35.00 (Windows)

#ifndef DaspcallframeHPP
#define DaspcallframeHPP

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
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CRTypes.hpp>
#include <DBAccess.hpp>
#include <DASQLGenerator.hpp>
#include <CRFrame.hpp>
#include <CRTabEditor.hpp>
#include <DASQLFrame.hpp>
#include <DAUpdateSQLFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daspcallframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDASPCallFrame;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDASPCallFrameMode : unsigned char { spSQL, spQuery, spSQLSP, spQuerySP };

class PASCALIMPLEMENTATION TDASPCallFrame : public Daupdatesqlframe::TDAUpdateSQLFrame
{
	typedef Daupdatesqlframe::TDAUpdateSQLFrame inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnSQL;
	Vcl::Stdctrls::TLabel* Label14;
	Vcl::Stdctrls::TComboBox* cbStoredProcName;
	Vcl::Stdctrls::TButton* btGenerate;
	Vcl::Extctrls::TPanel* pnSQLSP;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TComboBox* cbStoredProcNameSP;
	void __fastcall btGenerateClick(System::TObject* Sender);
	void __fastcall cbStoredProcNameChange(System::TObject* Sender);
	void __fastcall cbStoredProcNameDropDown(System::TObject* Sender);
	void __fastcall cbStoredProcNameSelect(System::TObject* Sender);
	
protected:
	bool FListGot;
	TDASPCallFrameMode FMode;
	virtual Dbaccess::TStatementTypes __fastcall GetStatementTypes();
	void __fastcall SetMode(TDASPCallFrameMode Value);
	Dbaccess::TCustomDAConnection* __fastcall UsedConnection();
	virtual void __fastcall DoActivate();
	virtual bool __fastcall GetSPIsQuery();
	virtual bool __fastcall ShowAllProc();
	void __fastcall CreateProcedureCall();
	virtual void __fastcall UpdateStoredProcSelection();
	
public:
	virtual System::UnicodeString __fastcall GetSPName();
	virtual void __fastcall SetSPName(const System::UnicodeString Value);
	virtual Vcl::Controls::TWinControl* __fastcall ActiveControl();
	__property TDASPCallFrameMode Mode = {read=FMode, write=SetMode, nodefault};
public:
	/* TDASQLFrame.Create */ inline __fastcall virtual TDASPCallFrame(System::Classes::TComponent* Owner) : Daupdatesqlframe::TDAUpdateSQLFrame(Owner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDASPCallFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDASPCallFrame(HWND ParentWindow) : Daupdatesqlframe::TDAUpdateSQLFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daspcallframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASPCALLFRAME)
using namespace Daspcallframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaspcallframeHPP
