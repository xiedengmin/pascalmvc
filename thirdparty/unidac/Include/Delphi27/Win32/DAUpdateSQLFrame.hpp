// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAUpdateSQLFrame.pas' rev: 34.00 (Windows)

#ifndef DaupdatesqlframeHPP
#define DaupdatesqlframeHPP

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

//-- user supplied -----------------------------------------------------------

namespace Daupdatesqlframe
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAUpdateSQLFrame;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDAUpdateSQLFrame : public Dasqlframe::TDASQLFrame
{
	typedef Dasqlframe::TDASQLFrame inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnlTop;
	Vcl::Buttons::TSpeedButton* btClear;
	Vcl::Stdctrls::TGroupBox* gbStatementType;
	void __fastcall rbClick(System::TObject* Sender);
	void __fastcall btClearClick(System::TObject* Sender);
	void __fastcall meSQLChange(System::TObject* Sender);
	
protected:
	Dbaccess::TStatementType FStatementType;
	bool FStatementTypeSetted;
	System::StaticArray<Vcl::Stdctrls::TRadioButton*, 11> Frb;
	virtual Dbaccess::TStatementTypes __fastcall GetStatementTypes();
	virtual System::Classes::TStrings* __fastcall GetLocalComponentSQL();
	virtual void __fastcall SetLocalComponentSQL(System::Classes::TStrings* Value);
	void __fastcall CreateStatementIndicators();
	void __fastcall RefreshEmptyIndicators();
	void __fastcall SetEmpty(Dbaccess::TStatementType st, bool Empty);
	virtual void __fastcall DoActivate();
	
public:
	void __fastcall SetStatementType(Dbaccess::TStatementType StatementType);
	__property Dbaccess::TStatementType StatementType = {read=FStatementType, nodefault};
public:
	/* TDASQLFrame.Create */ inline __fastcall virtual TDAUpdateSQLFrame(System::Classes::TComponent* Owner) : Dasqlframe::TDASQLFrame(Owner) { }
	
public:
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TDAUpdateSQLFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDAUpdateSQLFrame(HWND ParentWindow) : Dasqlframe::TDASQLFrame(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daupdatesqlframe */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAUPDATESQLFRAME)
using namespace Daupdatesqlframe;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaupdatesqlframeHPP
