// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DADesign.pas' rev: 34.00 (Windows)

#ifndef DadesignHPP
#define DadesignHPP

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
#include <StrEdit.hpp>
#include <ValueEdit.hpp>
#include <StFilSys.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <CRDesign.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>
#include <DBAccess.hpp>
#include <DAScript.hpp>
#include <DALoader.hpp>
#include <DADump.hpp>
#include <DASQLGenerator.hpp>
#include <CREditor.hpp>
#include <CRDesignUtils.hpp>
#include <DADesignUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dadesign
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStringPropertyUseConnect;
class DELPHICLASS TDAPropertyEditor;
class DELPHICLASS TDAPasswordProperty;
class DELPHICLASS TDATableNameEditor;
class DELPHICLASS TDAUpdatingTableEditor;
class DELPHICLASS TDADatabaseNameEditor;
class DELPHICLASS TDASPNameEditor;
class DELPHICLASS TDAFieldDefsListEditor;
class DELPHICLASS TDAFieldsListEditor;
class DELPHICLASS TDALoaderTableNameEditor;
class DELPHICLASS TDADataSetMasterFieldsEditor;
class DELPHICLASS TVariantEditor;
class DELPHICLASS TDADatasetOrSQLProperty;
class DELPHICLASS TDAUpdateSQLProperty;
class DELPHICLASS TDAMetaDataKindEditor;
class DELPHICLASS TDAMetaDataRestrictionsEditor;
class DELPHICLASS TDAConnectionList;
class DELPHICLASS TDADesignNotification;
class DELPHICLASS TStrEditDlgEx;
class DELPHICLASS TStringListPropertyEx;
class DELPHICLASS TDAComponentEditor;
class DELPHICLASS TDAConnectionEditor;
class DELPHICLASS TDASQLEditor;
class DELPHICLASS TDAScriptEditor;
class DELPHICLASS TDAUpdateSQLEditor;
class DELPHICLASS TDALoaderEditor;
class DELPHICLASS TDASQLMonitorEditor;
class DELPHICLASS TCRDataSourceEditor;
class DELPHICLASS TDesignMacros;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringPropertyUseConnect : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc) = 0 ;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TStringPropertyUseConnect(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TStringPropertyUseConnect() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAPropertyEditor : public Designeditors::TPropertyEditor
{
	typedef Designeditors::TPropertyEditor inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
	virtual void __fastcall Edit();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAPropertyEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDAPropertyEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAPasswordProperty : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
protected:
	bool FActivated;
	
public:
	virtual void __fastcall Initialize();
	virtual void __fastcall Activate();
	virtual System::UnicodeString __fastcall GetValue();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAPasswordProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDAPasswordProperty() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDATableNameEditor : public TStringPropertyUseConnect
{
	typedef TStringPropertyUseConnect inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual bool __fastcall AutoFill();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDATableNameEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TStringPropertyUseConnect(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDATableNameEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAUpdatingTableEditor : public TStringPropertyUseConnect
{
	typedef TStringPropertyUseConnect inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAUpdatingTableEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TStringPropertyUseConnect(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDAUpdatingTableEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADatabaseNameEditor : public TStringPropertyUseConnect
{
	typedef TStringPropertyUseConnect inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual bool __fastcall AutoFill();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDADatabaseNameEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TStringPropertyUseConnect(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDADatabaseNameEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDASPNameEditor : public TStringPropertyUseConnect
{
	typedef TStringPropertyUseConnect inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual bool __fastcall AutoFill();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDASPNameEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TStringPropertyUseConnect(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDASPNameEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAFieldDefsListEditor : public TStringPropertyUseConnect
{
	typedef TStringPropertyUseConnect inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual bool __fastcall AutoFill();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAFieldDefsListEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TStringPropertyUseConnect(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDAFieldDefsListEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAFieldsListEditor : public TStringPropertyUseConnect
{
	typedef TStringPropertyUseConnect inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual bool __fastcall AutoFill();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAFieldsListEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TStringPropertyUseConnect(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDAFieldsListEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDALoaderTableNameEditor : public TStringPropertyUseConnect
{
	typedef TStringPropertyUseConnect inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual bool __fastcall AutoFill();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDALoaderTableNameEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TStringPropertyUseConnect(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDALoaderTableNameEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADataSetMasterFieldsEditor : public Fldlinks::TFieldLinkProperty
{
	typedef Fldlinks::TFieldLinkProperty inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetMasterFields();
	virtual void __fastcall SetMasterFields(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall GetIndexFieldNames();
	virtual void __fastcall SetIndexFieldNames(const System::UnicodeString Value);
public:
	/* TFieldLinkProperty.CreateWith */ inline __fastcall virtual TDADataSetMasterFieldsEditor(Data::Db::TDataSet* ADataSet) : Fldlinks::TFieldLinkProperty(ADataSet) { }
	
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDADataSetMasterFieldsEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Fldlinks::TFieldLinkProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDADataSetMasterFieldsEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVariantEditor : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
	virtual void __fastcall SetValue(const System::UnicodeString Value)/* overload */;
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TVariantEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TVariantEditor() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TDADatasetOrSQLProperty : public Designeditors::TComponentProperty
{
	typedef Designeditors::TComponentProperty inherited;
	
private:
	System::Classes::TGetStrProc FCheckProc;
	void __fastcall CheckComponent(const System::UnicodeString Value);
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDADatasetOrSQLProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TComponentProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDADatasetOrSQLProperty() { }
	
};


class PASCALIMPLEMENTATION TDAUpdateSQLProperty : public Designeditors::TComponentProperty
{
	typedef Designeditors::TComponentProperty inherited;
	
private:
	System::Classes::TGetStrProc FCheckProc;
	void __fastcall CheckComponent(const System::UnicodeString Value);
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAUpdateSQLProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TComponentProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDAUpdateSQLProperty() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAMetaDataKindEditor : public TStringPropertyUseConnect
{
	typedef TStringPropertyUseConnect inherited;
	
protected:
	virtual Dbaccess::TCustomDAConnection* __fastcall InternalGetValues(System::Classes::TGetStrProc Proc);
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAMetaDataKindEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : TStringPropertyUseConnect(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDAMetaDataKindEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAMetaDataRestrictionsEditor : public Stredit::TValueListProperty
{
	typedef Stredit::TValueListProperty inherited;
	
private:
	System::Classes::TStringList* FList;
	
protected:
	virtual System::Classes::TStrings* __fastcall GetStrings();
	virtual void __fastcall SetStrings(System::Classes::TStrings* const Value);
	
public:
	__fastcall virtual ~TDAMetaDataRestrictionsEditor();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TDAMetaDataRestrictionsEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Stredit::TValueListProperty(ADesigner, APropCount) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TCustomDAConnectionClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAConnectionList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	void __fastcall ListBoxDblClick(System::TObject* Sender);
	void __fastcall ListBoxKeyPress(System::TObject* Sender, System::WideChar &Key);
	
protected:
	System::Classes::TStrings* Items;
	Vcl::Forms::TForm* Form;
	void __fastcall StrProc(const System::UnicodeString S);
	virtual TCustomDAConnectionClass __fastcall GetConnectionType() = 0 ;
	
public:
	__fastcall TDAConnectionList();
	__fastcall virtual ~TDAConnectionList();
	Dbaccess::TCustomDAConnection* __fastcall GetConnection(System::Classes::TComponent* Component, Designintf::_di_IDesigner Designer);
};

#pragma pack(pop)

typedef System::TMetaClass* TDAConnectionListClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADesignNotification : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	System::Classes::TPersistent* FItem;
	TDAConnectionList* FConnectionList;
	System::Classes::TStrings* DSItems;
	void __fastcall StrProc(const System::UnicodeString S);
	void __fastcall DSStrProc(const System::UnicodeString S);
	
public:
	virtual void __fastcall ItemDeleted(const Designintf::_di_IDesigner ADesigner, System::Classes::TPersistent* AItem);
	virtual void __fastcall ItemInserted(const Designintf::_di_IDesigner ADesigner, System::Classes::TPersistent* AItem) = 0 ;
	virtual void __fastcall ItemsModified(const Designintf::_di_IDesigner ADesigner);
	virtual void __fastcall SelectionChanged(const Designintf::_di_IDesigner ADesigner, const Designintf::_di_IDesignerSelections ASelection);
	virtual void __fastcall DesignerOpened(const Designintf::_di_IDesigner ADesigner, bool AResurrecting);
	virtual void __fastcall DesignerClosed(const Designintf::_di_IDesigner ADesigner, bool AGoingDormant);
	virtual TDAConnectionList* __fastcall CreateConnectionList() = 0 ;
	virtual System::UnicodeString __fastcall GetConnectionPropertyName() = 0 ;
public:
	/* TObject.Create */ inline __fastcall TDADesignNotification() : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDADesignNotification() { }
	
private:
	void *__IDesignNotification;	// Designintf::IDesignNotification 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {E8C9F739-5601-4ADD-9D95-594132D4CEFD}
	operator Designintf::_di_IDesignNotification()
	{
		Designintf::_di_IDesignNotification intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Designintf::IDesignNotification*(void) { return (Designintf::IDesignNotification*)&__IDesignNotification; }
	#endif
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TStrEditDlgEx : public Stredit::TStrEditDlg
{
	typedef Stredit::TStrEditDlg inherited;
	
protected:
	System::Classes::TStrings* FLines;
	virtual System::Classes::TStrings* __fastcall GetLines();
	virtual void __fastcall SetLines(System::Classes::TStrings* const Value);
	virtual Vcl::Controls::TWinControl* __fastcall GetLinesControl();
	
public:
	virtual int __fastcall ShowModal();
public:
	/* TCustomForm.Create */ inline __fastcall virtual TStrEditDlgEx(System::Classes::TComponent* AOwner) : Stredit::TStrEditDlg(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TStrEditDlgEx(System::Classes::TComponent* AOwner, int Dummy) : Stredit::TStrEditDlg(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TStrEditDlgEx() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TStrEditDlgEx(HWND ParentWindow) : Stredit::TStrEditDlg(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringListPropertyEx : public Stredit::TStringListProperty
{
	typedef Stredit::TStringListProperty inherited;
	
public:
	virtual Stredit::TStrEditDlg* __fastcall EditDialog();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TStringListPropertyEx(const Designintf::_di_IDesigner ADesigner, int APropCount) : Stredit::TStringListProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TStringListPropertyEx() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAComponentEditor : public Crdesign::TCRComponentEditor
{
	typedef Crdesign::TCRComponentEditor inherited;
	
private:
	Dadesignutils::TDADesignUtilsClass __fastcall GetDADesignUtilsClass();
	
protected:
	__classmethod virtual void __fastcall ProcessEditorResult(int ModalResult, Creditor::TCREditorForm* CREditor, System::Classes::TComponent* Component, Designintf::_di_IDesigner Designer);
	virtual void __fastcall ShowDataEditor();
	__property Dadesignutils::TDADesignUtilsClass DADesignUtilsClass = {read=GetDADesignUtilsClass};
	
public:
	__fastcall virtual TDAComponentEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner);
	virtual int __fastcall GetVerbCount();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDAComponentEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAConnectionEditor : public TDAComponentEditor
{
	typedef TDAComponentEditor inherited;
	
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TDAConnectionEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDAConnectionEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDASQLEditor : public TDAComponentEditor
{
	typedef TDAComponentEditor inherited;
	
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TDASQLEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDASQLEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAScriptEditor : public TDAComponentEditor
{
	typedef TDAComponentEditor inherited;
	
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TDAScriptEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDAScriptEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAUpdateSQLEditor : public TDAComponentEditor
{
	typedef TDAComponentEditor inherited;
	
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TDAUpdateSQLEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDAUpdateSQLEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDALoaderEditor : public TDAComponentEditor
{
	typedef TDAComponentEditor inherited;
	
protected:
	virtual void __fastcall InitVerbs();
	void __fastcall ShowColEditor();
	void __fastcall CreateColumns();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TDALoaderEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDALoaderEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDASQLMonitorEditor : public TDAComponentEditor
{
	typedef TDAComponentEditor inherited;
	
protected:
	void __fastcall RunDBMonitor();
	void __fastcall RunSQLMonitor();
	virtual void __fastcall InitVerbs();
	
public:
	virtual void __fastcall Edit();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TDASQLMonitorEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDASQLMonitorEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRDataSourceEditor : public TDAComponentEditor
{
	typedef TDAComponentEditor inherited;
	
private:
	System::Classes::TStrings* Items;
	Designintf::_di_IProperty FFirstProp;
	void __fastcall StrProc(const System::UnicodeString S);
	void __fastcall ConvertToDataSource();
	void __fastcall CheckEdit(const Designintf::_di_IProperty Prop);
	
protected:
	virtual void __fastcall InitVerbs();
	
public:
	__fastcall virtual TCRDataSourceEditor(System::Classes::TComponent* Component, Designintf::_di_IDesigner aDesigner);
	virtual void __fastcall Edit();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCRDataSourceEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDesignMacros : public Dbaccess::TMacros
{
	typedef Dbaccess::TMacros inherited;
	
public:
	HIDESBASE void __fastcall Scan(System::UnicodeString &SQL);
public:
	/* TMacros.Create */ inline __fastcall TDesignMacros(System::Classes::TPersistent* Owner) : Dbaccess::TMacros(Owner) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TDesignMacros() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall ConvertToClass(Designintf::_di_IDesigner Designer, System::Classes::TComponent* Component, System::Classes::TComponentClass NewClass);
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Dadesign */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DADESIGN)
using namespace Dadesign;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DadesignHPP
