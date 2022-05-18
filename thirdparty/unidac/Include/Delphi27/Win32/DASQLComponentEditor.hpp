// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DASQLComponentEditor.pas' rev: 34.00 (Windows)

#ifndef DasqlcomponenteditorHPP
#define DasqlcomponenteditorHPP

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
#include <Vcl.ComCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <System.SysUtils.hpp>
#include <Data.DB.hpp>
#include <System.Classes.hpp>
#include <DBAccess.hpp>
#include <MemUtils.hpp>
#include <CREditor.hpp>
#include <CRTabEditor.hpp>
#include <CRFrame.hpp>
#include <DASQLFrame.hpp>
#include <DAParamsFrame.hpp>
#include <DAMacrosFrame.hpp>
#include <DASPCallFrame.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dasqlcomponenteditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDASQLEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TDASQLEditorForm : public Crtabeditor::TCRTabEditorForm
{
	typedef Crtabeditor::TCRTabEditorForm inherited;
	
__published:
	Vcl::Comctrls::TTabSheet* shSQL;
	Vcl::Comctrls::TTabSheet* shParameters;
	Vcl::Comctrls::TTabSheet* shMacros;
	Vcl::Comctrls::TTabSheet* shGeneratorSPC;
	Vcl::Stdctrls::TButton* btExecute;
	void __fastcall btExecuteClick(System::TObject* Sender);
	
protected:
	Dasqlframe::TDASQLFrame* FSQLFrame;
	Daparamsframe::TDAParamsFrame* FParamsFrame;
	Damacrosframe::TDAMacrosFrame* FMacrosFrame;
	Daspcallframe::TDASPCallFrame* FSPCallFrame;
	System::Classes::TComponent* FLocalComponent;
	System::Classes::TComponent* FComponent;
	Dbaccess::TAfterExecuteEvent FOldAfterExecute;
	void __fastcall AfterExecute(System::TObject* Sender, bool Result);
	virtual void __fastcall DoInit();
	virtual void __fastcall DoActivate();
	virtual void __fastcall DoFinish();
	virtual void __fastcall DoSave();
	virtual void __fastcall DoError(System::Sysutils::Exception* E);
	Dbaccess::TCustomDASQL* __fastcall GetSQL();
	void __fastcall SetSQL(Dbaccess::TCustomDASQL* Value);
	virtual System::Classes::TComponent* __fastcall GetComponent();
	virtual void __fastcall SetComponent(System::Classes::TComponent* Value);
	virtual System::Classes::TComponent* __fastcall GetLocalComponent();
	virtual Crframe::TCRFrame* __fastcall GetFrameByInitProp();
	__property Dbaccess::TCustomDASQL* SQL = {read=GetSQL, write=SetSQL};
	
public:
	__property Dasqlframe::TDASQLFrame* SQLFrame = {read=FSQLFrame};
	__property Daparamsframe::TDAParamsFrame* ParamsFrame = {read=FParamsFrame};
	__property Damacrosframe::TDAMacrosFrame* MacrosFrame = {read=FMacrosFrame};
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TDASQLEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Crtabeditor::TCRTabEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDASQLEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Crtabeditor::TCRTabEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDASQLEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDASQLEditorForm(HWND ParentWindow) : Crtabeditor::TCRTabEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dasqlcomponenteditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASQLCOMPONENTEDITOR)
using namespace Dasqlcomponenteditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DasqlcomponenteditorHPP
