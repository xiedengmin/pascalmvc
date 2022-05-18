// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualQueryDesign.pas' rev: 35.00 (Windows)

#ifndef VirtualquerydesignHPP
#define VirtualquerydesignHPP

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
#include <Vcl.Dialogs.hpp>
#include <Data.DB.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <FldLinks.hpp>
#include <ColnEdit.hpp>
#include <CRTypes.hpp>
#include <CRDesign.hpp>
#include <DADesign.hpp>
#include <VirtualQuery.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualquerydesign
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVirtualQueryEditor;
class DELPHICLASS TSourceDataSetProperty;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualQueryEditor : public Dadesign::TDAComponentEditor
{
	typedef Dadesign::TDAComponentEditor inherited;
	
private:
	void __fastcall ShowSourceDataSetsEditor();
	
protected:
	virtual void __fastcall InitVerbs();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TVirtualQueryEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Dadesign::TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualQueryEditor() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TSourceDataSetProperty : public Designeditors::TComponentProperty
{
	typedef Designeditors::TComponentProperty inherited;
	
private:
	System::Classes::TGetStrProc FCheckProc;
	Virtualquery::TCustomVirtualQuery* FQuery;
	void __fastcall CheckComponent(const System::UnicodeString Value);
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TSourceDataSetProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TComponentProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TSourceDataSetProperty() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Virtualquerydesign */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALQUERYDESIGN)
using namespace Virtualquerydesign;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualquerydesignHPP
