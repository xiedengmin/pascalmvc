// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VTDesign.pas' rev: 34.00 (Windows)

#ifndef VtdesignHPP
#define VtdesignHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Data.DB.hpp>
#include <System.TypInfo.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <FldLinks.hpp>
#include <CRTypes.hpp>
#include <CRDesign.hpp>
#include <CRDesignUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Vtdesign
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVTFieldsEditor;
class DELPHICLASS TVTDataSetMasterFieldsEditor;
class DELPHICLASS TVirtualTableEditor;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTFieldsEditor : public Crdesign::TDAFieldsEditor
{
	typedef Crdesign::TDAFieldsEditor inherited;
	
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TVTFieldsEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Crdesign::TDAFieldsEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TVTFieldsEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDataSetMasterFieldsEditor : public Fldlinks::TFieldLinkProperty
{
	typedef Fldlinks::TFieldLinkProperty inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetMasterFields();
	virtual void __fastcall SetMasterFields(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall GetIndexFieldNames();
	virtual void __fastcall SetIndexFieldNames(const System::UnicodeString Value);
public:
	/* TFieldLinkProperty.CreateWith */ inline __fastcall virtual TVTDataSetMasterFieldsEditor(Data::Db::TDataSet* ADataSet) : Fldlinks::TFieldLinkProperty(ADataSet) { }
	
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TVTDataSetMasterFieldsEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Fldlinks::TFieldLinkProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TVTDataSetMasterFieldsEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualTableEditor : public Crdesign::TCRComponentEditor
{
	typedef Crdesign::TCRComponentEditor inherited;
	
private:
	System::Classes::TStrings* Items;
	void __fastcall StrProc(const System::UnicodeString S);
	void __fastcall DataSetsDblClick(System::TObject* Sender);
	void __fastcall ShowVTDataEditor();
	void __fastcall ShowAssignDataSet();
	void __fastcall LoadData();
	void __fastcall SaveData();
	
protected:
	virtual void __fastcall InitVerbs();
	
public:
	virtual void __fastcall Edit();
public:
	/* TCRComponentEditor.Create */ inline __fastcall virtual TVirtualTableEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Crdesign::TCRComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualTableEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Vtdesign */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VTDESIGN)
using namespace Vtdesign;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VtdesignHPP
