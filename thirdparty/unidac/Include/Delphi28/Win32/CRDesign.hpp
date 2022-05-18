// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRDesign.pas' rev: 35.00 (Windows)

#ifndef CrdesignHPP
#define CrdesignHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Win.Registry.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <FldLinks.hpp>
#include <ColnEdit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <Data.DB.hpp>
#include <CRTypes.hpp>
#include <CREditor.hpp>
#include <CRDesignUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crdesign
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAFieldsEditor;
struct TVerb;
class DELPHICLASS TCRComponentEditor;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAFieldsEditor : public Designeditors::TPropertyEditor
{
	typedef Designeditors::TPropertyEditor inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
	virtual void __fastcall Edit();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAFieldsEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDAFieldsEditor() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TVerbMethod)(void);

struct DECLSPEC_DRECORD TVerb
{
public:
	System::UnicodeString Caption;
	TVerbMethod Method;
};


typedef System::DynamicArray<TVerb> TVerbs;

typedef System::TMetaClass* TCRComponentEditorClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRComponentEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
protected:
	Creditor::TCREditorClass FCREditorClass;
	Crdesignutils::TCRDesignUtilsClass FCRDesignUtilsClass;
	TVerbs FVerbs;
	void __fastcall ExecuteDsmAction(const System::UnicodeString ProcName);
	void __fastcall DsmCreateDefaultControl();
	void __fastcall DsmShowInDataSetManager();
	void __fastcall Separator();
	int __fastcall AddVerb(const System::UnicodeString Caption, TVerbMethod Method)/* overload */;
	int __fastcall AddVerb(const System::UnicodeString Caption, Creditor::TCREditorClass CREditorClass, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass)/* overload */;
	virtual void __fastcall InitVerbs();
	virtual void __fastcall ShowEditor()/* overload */;
	virtual void __fastcall ShowEditor(const System::UnicodeString InitialProperty)/* overload */;
	__classmethod virtual void __fastcall ProcessEditorResult(int ModalResult, Creditor::TCREditorForm* CREditor, System::Classes::TComponent* Component, Designintf::_di_IDesigner Designer);
	void __fastcall ShowFieldsEditor();
	virtual void __fastcall ShowDataEditor();
	
public:
	__fastcall virtual TCRComponentEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner);
	virtual int __fastcall GetVerbCount();
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual void __fastcall ExecuteVerb(int Index);
	virtual void __fastcall Edit();
	__classmethod void __fastcall ShowEditorEx(Creditor::TCREditorClass CREditorClass, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass, System::Classes::TComponent* Component, Designintf::_di_IDesigner Designer, System::UnicodeString InitialProperty = System::UnicodeString());
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCRComponentEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall DARegisterComponentEditor(System::Classes::TComponentClass ComponentClass, TCRComponentEditorClass ComponentEditor, Creditor::TCREditorClass CREditorClass, Crdesignutils::TCRDesignUtilsClass CRDesignUtilsClass);
extern DELPHI_PACKAGE bool __fastcall FindComponentEditor(System::TObject* Component, Creditor::TCREditorClass &CREditorClass, Crdesignutils::TCRDesignUtilsClass &CRDesignUtilsClass);
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Crdesign */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRDESIGN)
using namespace Crdesign;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrdesignHPP
