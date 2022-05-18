// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniUpdateSQLEditor.pas' rev: 34.00 (Windows)

#ifndef UniupdatesqleditorHPP
#define UniupdatesqleditorHPP

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
#include <Vcl.ComCtrls.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <DBAccess.hpp>
#include <DAUpdateSQLEditor.hpp>
#include <CRTabEditor.hpp>
#include <CREditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniupdatesqleditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniUpdateSQLEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniUpdateSQLEditorForm : public Daupdatesqleditor::TDAUpdateSQLEditorForm
{
	typedef Daupdatesqleditor::TDAUpdateSQLEditorForm inherited;
	
protected:
	virtual void __fastcall DoInit();
public:
	/* TCRTabEditorForm.Create */ inline __fastcall virtual TUniUpdateSQLEditorForm(System::Classes::TComponent* Owner, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass) : Daupdatesqleditor::TDAUpdateSQLEditorForm(Owner, CRDesignUtilsClass) { }
	
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TUniUpdateSQLEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Daupdatesqleditor::TDAUpdateSQLEditorForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TUniUpdateSQLEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TUniUpdateSQLEditorForm(HWND ParentWindow) : Daupdatesqleditor::TDAUpdateSQLEditorForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uniupdatesqleditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIUPDATESQLEDITOR)
using namespace Uniupdatesqleditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniupdatesqleditorHPP
