// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniDesign.pas' rev: 35.00 (Windows)

#ifndef UnidesignHPP
#define UnidesignHPP

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
#include <System.TypInfo.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <FldLinks.hpp>
#include <ColnEdit.hpp>
#include <UniDacVcl.hpp>
#include <CRFunctions.hpp>
#include <CRTypes.hpp>
#include <DBAccess.hpp>
#include <UniProvider.hpp>
#include <Uni.hpp>
#include <UniScript.hpp>
#include <UniDump.hpp>
#include <UniLoader.hpp>
#include <UniAlerter.hpp>
#include <CRDesign.hpp>
#include <DADesign.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unidesign
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniConnectionList;
class DELPHICLASS TUniDesignNotification;
class DELPHICLASS TUniTransactionProperty;
class DELPHICLASS TUniConnectionEditor;
class DELPHICLASS TUniConnectDialogPropertyEditor;
class DELPHICLASS TUniQueryEditor;
class DELPHICLASS TUniTableEditor;
class DELPHICLASS TUniStoredProcEditor;
class DELPHICLASS TUniSQLEditor;
class DELPHICLASS TUniScriptEditor;
class DELPHICLASS TUniUpdateSQLEditor;
class DELPHICLASS TUniConnectionProviderNameEditor;
class DELPHICLASS TSpecificOptionsEditor;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniConnectionList : public Dadesign::TDAConnectionList
{
	typedef Dadesign::TDAConnectionList inherited;
	
protected:
	virtual Dadesign::TCustomDAConnectionClass __fastcall GetConnectionType();
public:
	/* TDAConnectionList.Create */ inline __fastcall TUniConnectionList() : Dadesign::TDAConnectionList() { }
	/* TDAConnectionList.Destroy */ inline __fastcall virtual ~TUniConnectionList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniDesignNotification : public Dadesign::TDADesignNotification
{
	typedef Dadesign::TDADesignNotification inherited;
	
public:
	virtual void __fastcall ItemInserted(const Designintf::_di_IDesigner ADesigner, System::Classes::TPersistent* AItem);
	virtual Dadesign::TDAConnectionList* __fastcall CreateConnectionList();
	virtual System::UnicodeString __fastcall GetConnectionPropertyName();
public:
	/* TObject.Create */ inline __fastcall TUniDesignNotification() : Dadesign::TDADesignNotification() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TUniDesignNotification() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniTransactionProperty : public Designeditors::TComponentProperty
{
	typedef Designeditors::TComponentProperty inherited;
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
	virtual System::UnicodeString __fastcall GetValue();
	virtual void __fastcall SetValue(const System::UnicodeString Value)/* overload */;
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TUniTransactionProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TComponentProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TUniTransactionProperty() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetValue(const System::WideString Value){ Designeditors::TPropertyEditor::SetValue(Value); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniConnectionEditor : public Dadesign::TDAConnectionEditor
{
	typedef Dadesign::TDAConnectionEditor inherited;
	
protected:
	virtual void __fastcall InitVerbs();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TUniConnectionEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Dadesign::TDAConnectionEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUniConnectionEditor() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TUniConnectDialogPropertyEditor : public Designeditors::TComponentProperty
{
	typedef Designeditors::TComponentProperty inherited;
	
private:
	System::Classes::TGetStrProc FCheckProc;
	void __fastcall CheckComponent(const System::UnicodeString Value);
	
public:
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TUniConnectDialogPropertyEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TComponentProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TUniConnectDialogPropertyEditor() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniQueryEditor : public Dadesign::TDAComponentEditor
{
	typedef Dadesign::TDAComponentEditor inherited;
	
protected:
	virtual void __fastcall InitVerbs();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TUniQueryEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Dadesign::TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUniQueryEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniTableEditor : public Dadesign::TDAComponentEditor
{
	typedef Dadesign::TDAComponentEditor inherited;
	
protected:
	virtual void __fastcall InitVerbs();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TUniTableEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Dadesign::TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUniTableEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniStoredProcEditor : public Dadesign::TDAComponentEditor
{
	typedef Dadesign::TDAComponentEditor inherited;
	
protected:
	virtual void __fastcall InitVerbs();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TUniStoredProcEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Dadesign::TDAComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUniStoredProcEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniSQLEditor : public Dadesign::TDASQLEditor
{
	typedef Dadesign::TDASQLEditor inherited;
	
protected:
	virtual void __fastcall InitVerbs();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TUniSQLEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Dadesign::TDASQLEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUniSQLEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniScriptEditor : public Dadesign::TDAScriptEditor
{
	typedef Dadesign::TDAScriptEditor inherited;
	
protected:
	virtual void __fastcall InitVerbs();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TUniScriptEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Dadesign::TDAScriptEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUniScriptEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniUpdateSQLEditor : public Dadesign::TDAUpdateSQLEditor
{
	typedef Dadesign::TDAUpdateSQLEditor inherited;
	
protected:
	virtual void __fastcall InitVerbs();
public:
	/* TDAComponentEditor.Create */ inline __fastcall virtual TUniUpdateSQLEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Dadesign::TDAUpdateSQLEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TUniUpdateSQLEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniConnectionProviderNameEditor : public Designeditors::TStringProperty
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual void __fastcall GetValues(System::Classes::TGetStrProc Proc);
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TUniConnectionProviderNameEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TUniConnectionProviderNameEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpecificOptionsEditor : public Designeditors::TPropertyEditor
{
	typedef Designeditors::TPropertyEditor inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes();
	virtual System::UnicodeString __fastcall GetValue();
	virtual void __fastcall Edit();
public:
	/* TPropertyEditor.Create */ inline __fastcall virtual TSpecificOptionsEditor(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TSpecificOptionsEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Unidesign */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIDESIGN)
using namespace Unidesign;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnidesignHPP
