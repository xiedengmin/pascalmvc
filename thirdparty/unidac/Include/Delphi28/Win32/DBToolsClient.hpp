// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBToolsClient.pas' rev: 35.00 (Windows)

#ifndef DbtoolsclientHPP
#define DbtoolsclientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <CLRClasses.hpp>
#include <System.VarUtils.hpp>
#include <DesignIntf.hpp>
#include <ToolsAPI.hpp>
#include <Winapi.ActiveX.hpp>
#include <CRTypes.hpp>
#include <CRDesignUtils.hpp>
#include <DADesignUtils.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Data.DB.hpp>
#include <DBAccess.hpp>
#include <DASQLGenerator.hpp>
#include <DBToolsIntf.hpp>
#include <Vcl.ActnMan.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Messages.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dbtoolsclient
{
//-- forward type declarations -----------------------------------------------
struct TParamTypeMap;
class DELPHICLASS TCustomDBToolsService;
class DELPHICLASS TCustomDACSqlEditorFrame;
class DELPHICLASS TDBTools;
class DELPHICLASS TModuleNotifier;
class DELPHICLASS TModuleList;
class DELPHICLASS TCustomSourceNotifier;
class DELPHICLASS TCustomSqlSource;
class DELPHICLASS TSqlSourceList;
class DELPHICLASS TDBToolsDesignNotification;
//-- type declarations -------------------------------------------------------
typedef System::WideChar * TString;

typedef Dbtoolsintf::ConnectionInfo *PConnectionInfo;

typedef Toolsapi::_di_IOTAServices IIDEServices;

typedef Toolsapi::_di_IOTAModuleServices70 IModuleServices;

struct DECLSPEC_DRECORD TParamTypeMap
{
public:
	Data::Db::TFieldType DACType;
	int DBToolsType;
};


enum DECLSPEC_DENUM TDBToolsVerb : unsigned char { dbtEditSql, dbtEditSelectSql, dbtQueryBuilder, dbtFindInDatabaseExplorer, dbtEditDatabaseObject, dbtExecuteSql, dbtDebugSql, dbtRetrieveData, dbtCompile, dbtCompileDebug, dbtEditInsertSql, dbtEditUpdateSql, dbtEditDeleteSql, dbtEditLockSql, dbtEditRefreshSql, dbtEditRecCountSQL };

typedef System::Set<TDBToolsVerb, TDBToolsVerb::dbtEditSql, TDBToolsVerb::dbtEditRecCountSQL> TDBToolsVerbs;

enum DECLSPEC_DENUM TCompareFlag : unsigned char { cfNormal, cfCaseSensitive, cfNone };

typedef System::TMetaClass* TCustomDBToolsServiceClass;

typedef System::TMetaClass* TCustomDACSqlEditorFrameClass;

typedef System::TMetaClass* TCustomSourceNotifierClass;

typedef System::TMetaClass* TCustomSqlSourceClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomDBToolsService : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	Designintf::_di_IDesigner FCurrentDesigner;
	System::Classes::TComponent* FCurrentComponent;
	Dadesignutils::TDADesignUtilsClass FDADesignUtils;
	virtual TCustomDACSqlEditorFrameClass __fastcall GetDACSqlEditorFrameClass() = 0 ;
	virtual TCustomSqlSourceClass __fastcall SqlSourceClass() = 0 ;
	virtual void __fastcall DesignerClosing(System::UnicodeString DesignerName) = 0 ;
	__classmethod virtual System::UnicodeString __fastcall GetNamespace();
	
public:
	__fastcall virtual TCustomDBToolsService(Dadesignutils::TDADesignUtilsClass ADADesignUtils, System::_di_IInterface ASqlService, System::UnicodeString ADefaultConnectionStr);
	Dbaccess::TCustomDAConnection* __fastcall GetConnection(System::Classes::TComponent* const Component);
	virtual void __fastcall FindInDatabaseExplorer() = 0 ;
	virtual void __fastcall EditDatabaseObject() = 0 ;
	virtual void __fastcall ExecuteSql(bool Debug) = 0 ;
	virtual void __fastcall Compile(bool Debug) = 0 ;
	virtual void __fastcall RetrieveData(bool AsDocument) = 0 ;
	virtual void __fastcall EditSql(bool AsQuery, Dbaccess::TStatementType StatementType = (Dbaccess::TStatementType)(0x0)) = 0 ;
	virtual void __fastcall GetConnections(System::Classes::TStrings* NameList, System::UnicodeString Condition = System::UnicodeString()) = 0 ;
	virtual System::UnicodeString __fastcall FindConnectionName(Dbaccess::TCustomDAConnection* AConnection) = 0 ;
	virtual System::Classes::TStringList* __fastcall GetConnectionStrList(System::UnicodeString ConnectionName) = 0 ;
	virtual void __fastcall AddParamTypeMap(Data::Db::TFieldType ADACType, int ADBToolsType) = 0 ;
	virtual void __fastcall PutConnectionParam(const System::UnicodeString ConnectionParam, const TCompareFlag CompareFlag = (TCompareFlag)(0x0)) = 0 ;
	virtual void __fastcall SkipConnectionParams(const int Count) = 0 ;
	__property Dadesignutils::TDADesignUtilsClass DADesignUtils = {read=FDADesignUtils};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCustomDBToolsService() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCustomDACSqlEditorFrame : public Vcl::Extctrls::TPanel
{
	typedef Vcl::Extctrls::TPanel inherited;
	
protected:
	bool FReadOnly;
	System::Classes::TNotifyEvent FOnExit;
	System::Classes::TNotifyEvent FOnChange;
	virtual void __fastcall CheckModified() = 0 ;
	virtual void __fastcall CheckConnectionChange() = 0 ;
	virtual void __fastcall EndInit() = 0 ;
	virtual void __fastcall SetStatementType(const Dbaccess::TStatementType Value) = 0 ;
	HIDESBASE virtual System::UnicodeString __fastcall GetText() = 0 ;
	HIDESBASE virtual void __fastcall SetText(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall SetReadOnly(bool Value) = 0 ;
	
public:
	__fastcall virtual TCustomDACSqlEditorFrame(System::Classes::TComponent* AOwner, System::Classes::TComponent* Component, TCustomDBToolsService* DBToolsService);
	__property System::UnicodeString Text = {read=GetText, write=SetText};
	__property bool ReadOnly = {read=FReadOnly, write=SetReadOnly, nodefault};
	__property Dbaccess::TStatementType StatementType = {write=SetStatementType, nodefault};
	__property System::Classes::TNotifyEvent OnExit = {read=FOnExit, write=FOnExit};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TCustomDACSqlEditorFrame() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomDACSqlEditorFrame(HWND ParentWindow) : Vcl::Extctrls::TPanel(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBTools : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TCustomDBToolsService*> _TDBTools__1;
	
	typedef System::DynamicArray<TCustomDACSqlEditorFrame*> _TDBTools__2;
	
	
private:
	Toolsapi::_di_IOTAServices FIDEServices;
	Toolsapi::_di_IOTAModuleServices70 FModuleServices;
	_TDBTools__1 FDBToolsServices;
	TDBToolsDesignNotification* FDesignNotification;
	Dadesignutils::TDADesignUtilsClass FCurrentDADesignUtils;
	TCustomDBToolsService* FCurrentDBToolsService;
	bool FNeedUninitialize;
	Vcl::Actnman::TActionManager* FMenuManager;
	System::StaticArray<Vcl::Actnlist::TAction*, 16> FMenuActions;
	TCustomDACSqlEditorFrame* FLastDACSqlEditorFrame;
	Vcl::Controls::TWinControl* FLastMemo;
	_TDBTools__2 FDACSqlEditorFrames;
	bool __fastcall MenuItemsAvailable();
	void __fastcall DebugSql(System::TObject* Sender);
	void __fastcall EditDatabaseObject(System::TObject* Sender);
	void __fastcall EditSql(System::TObject* Sender);
	void __fastcall ExecuteSql(System::TObject* Sender);
	void __fastcall Compile(System::TObject* Sender);
	void __fastcall CompileDebug(System::TObject* Sender);
	void __fastcall FindInDatabaseExplorer(System::TObject* Sender);
	void __fastcall QueryBuilder(System::TObject* Sender);
	void __fastcall RetrieveData(System::TObject* Sender);
	void __fastcall EditSelectSql(System::TObject* Sender);
	void __fastcall EditInsertSql(System::TObject* Sender);
	void __fastcall EditUpdateSql(System::TObject* Sender);
	void __fastcall EditDeleteSql(System::TObject* Sender);
	void __fastcall EditRefreshSql(System::TObject* Sender);
	void __fastcall EditLockSql(System::TObject* Sender);
	void __fastcall EditRecCountSql(System::TObject* Sender);
	void __fastcall CreateMenuActions();
	Vcl::Actnlist::TAction* __fastcall GetMenuActions(TDBToolsVerb Index);
	
public:
	__fastcall TDBTools();
	__fastcall virtual ~TDBTools();
	System::TObject* __fastcall CreateDBToolsService(const TCustomDBToolsServiceClass DBToolsServiceClass, const Dadesignutils::TDADesignUtilsClass DADesignUtils, const GUID &ClassID, const System::UnicodeString DefaultConnectionStr, const System::UnicodeString ProviderKey, /* out */ __int64 &ServiceVersion, /* out */ Dadesignutils::TNeedToCheckDbTools &NeedToCheck);
	Dadesignutils::TNeedToCheckDbTools __fastcall CheckDevTools(const GUID &ClassID, const System::UnicodeString ProviderKey, const Dadesignutils::TDADesignUtilsClass DADesignUtils, /* out */ __int64 &ServiceVersion);
	void __fastcall CheckDBToolsChanges(Vcl::Controls::TWinControl* Control);
	void __fastcall ReplaceMemo(Vcl::Stdctrls::TMemo* &Memo, Dadesignutils::TDADesignUtilsClass DADesignUtils, System::Classes::TComponent* Component);
	void __fastcall DesignerClosing(const System::UnicodeString FileName);
	System::UnicodeString __fastcall GetDesignerName(Designintf::_di_IDesigner Designer);
	void __fastcall PrepareMenu(Designintf::_di_IDesigner Designer, System::Classes::TComponent* Component, Dadesignutils::TDADesignUtilsClass DADesignUtils);
	bool __fastcall HasDACSqlEditorFrame(Vcl::Controls::TWinControl* Memo);
	TCustomDACSqlEditorFrame* __fastcall GetDACSqlEditorFrame(Vcl::Controls::TWinControl* Memo);
	void __fastcall CheckConnectionChanges();
	Vcl::Controls::TWinControl* __fastcall GetActiveDACSqlEditorFrame();
	void __fastcall AddFrame(TCustomDACSqlEditorFrame* Value);
	void __fastcall RemoveFrame(TCustomDACSqlEditorFrame* Value);
	__property TDBToolsDesignNotification* DesignNotification = {read=FDesignNotification, write=FDesignNotification};
	__property Toolsapi::_di_IOTAServices IDEServices = {read=FIDEServices};
	__property Toolsapi::_di_IOTAModuleServices70 ModuleServices = {read=FModuleServices};
	__property Vcl::Actnlist::TAction* MenuActions[TDBToolsVerb Index] = {read=GetMenuActions};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TModuleNotifier : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	Toolsapi::_di_IOTAModule FModule;
	bool AllowSaveSwitch;
	int FIndex;
	void __fastcall RemoveNotifier();
	
public:
	__fastcall TModuleNotifier(const Toolsapi::_di_IOTAModule Module);
	__fastcall virtual ~TModuleNotifier();
	void __fastcall AfterSave();
	void __fastcall BeforeSave();
	void __fastcall Destroyed();
	void __fastcall Modified();
	bool __fastcall CheckOverwrite();
	void __fastcall ModuleRenamed(const System::UnicodeString NewName);
	bool __fastcall AllowSave();
	int __fastcall GetOverwriteFileNameCount();
	System::UnicodeString __fastcall GetOverwriteFileName(int Index);
	void __fastcall SetSaveFileName(const System::UnicodeString FileName);
	__property Toolsapi::_di_IOTAModule Module = {read=FModule};
private:
	void *__IOTAModuleNotifier80;	// Toolsapi::IOTAModuleNotifier80 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {6C4714BB-223A-4CDF-A710-429FE8FA0B91}
	operator Toolsapi::_di_IOTAModuleNotifier80()
	{
		Toolsapi::_di_IOTAModuleNotifier80 intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAModuleNotifier80*(void) { return (Toolsapi::IOTAModuleNotifier80*)&__IOTAModuleNotifier80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {F17A7BCE-E07D-11D1-AB0B-00C04FB16FB3}
	operator Toolsapi::_di_IOTAModuleNotifier()
	{
		Toolsapi::_di_IOTAModuleNotifier intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTAModuleNotifier*(void) { return (Toolsapi::IOTAModuleNotifier*)&__IOTAModuleNotifier80; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {F17A7BCF-E07D-11D1-AB0B-00C04FB16FB3}
	operator Toolsapi::_di_IOTANotifier()
	{
		Toolsapi::_di_IOTANotifier intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Toolsapi::IOTANotifier*(void) { return (Toolsapi::IOTANotifier*)&__IOTAModuleNotifier80; }
	#endif
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TModuleList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	HIDESBASE int __fastcall IndexOf(Toolsapi::_di_IOTAModule Module);
	HIDESBASE void __fastcall Add(Toolsapi::_di_IOTAModule Module);
	HIDESBASE void __fastcall Delete(int Index)/* overload */;
	HIDESBASE void __fastcall Delete(Toolsapi::_di_IOTAModule Module)/* overload */;
public:
	/* TList.Destroy */ inline __fastcall virtual ~TModuleList() { }
	
public:
	/* TObject.Create */ inline __fastcall TModuleList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomSourceNotifier : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__fastcall virtual TCustomSourceNotifier();
	virtual void __fastcall OnSqlSourceDeleted() = 0 ;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCustomSourceNotifier() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomSqlSource : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	Designintf::_di_IDesigner FDesigner;
	System::Classes::TComponent* FComponent;
	System::UnicodeString FSqlTextPrefix;
	TCustomSourceNotifier* FSqlSourceNotifier;
	virtual TCustomSourceNotifierClass __fastcall GetSourceNotifierClass() = 0 ;
	virtual void __fastcall FreeSourceNotifier() = 0 ;
	virtual TCustomDBToolsService* __fastcall GetDBToolsService() = 0 ;
	
public:
	__fastcall virtual TCustomSqlSource();
	__fastcall virtual ~TCustomSqlSource();
	virtual void __fastcall CheckChanges() = 0 ;
	virtual void __fastcall CheckRename() = 0 ;
	virtual void __fastcall CheckConnectionChange(bool InternalCheck) = 0 ;
	__property Designintf::_di_IDesigner Designer = {read=FDesigner};
	__property System::UnicodeString SqlTextPrefix = {read=FSqlTextPrefix, write=FSqlTextPrefix};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSqlSourceList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
	
private:
	typedef System::DynamicArray<TCustomSourceNotifier*> _TSqlSourceList__1;
	
	
private:
	_TSqlSourceList__1 FNotifiersToDelete;
	
public:
	HIDESBASE int __fastcall IndexOf(System::Classes::TComponent* Component);
	TCustomSqlSource* __fastcall Find(System::Classes::TComponent* Component)/* overload */;
	TCustomSqlSource* __fastcall Find(System::Classes::TComponent* Component, System::UnicodeString SqlTextPrefix)/* overload */;
	Designintf::_di_IDesigner __fastcall FindDesigner(System::UnicodeString FileName);
	void __fastcall CheckSubordinated(System::Classes::TComponent* AComponent);
	HIDESBASE void __fastcall Delete(int Index, bool WithNotification = false)/* overload */;
	HIDESBASE void __fastcall Delete(System::Classes::TComponent* Component, bool WithNotification = true)/* overload */;
	HIDESBASE void __fastcall Delete(TCustomSqlSource* SqlSource)/* overload */;
	HIDESBASE void __fastcall Add(TCustomSqlSource* SqlSource);
	void __fastcall DeleteDesigner(Designintf::_di_IDesigner Designer);
	void __fastcall CheckDeletedComponents(Designintf::_di_IDesigner Designer);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TSqlSourceList() { }
	
public:
	/* TObject.Create */ inline __fastcall TSqlSourceList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBToolsDesignNotification : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	TSqlSourceList* FSqlSourceList;
	TModuleList* FModuleList;
	System::UnicodeString FDebugStr;
	
public:
	__fastcall TDBToolsDesignNotification();
	__fastcall virtual ~TDBToolsDesignNotification();
	void __fastcall ItemDeleted(const Designintf::_di_IDesigner ADesigner, System::Classes::TPersistent* AItem);
	void __fastcall ItemInserted(const Designintf::_di_IDesigner ADesigner, System::Classes::TPersistent* AItem);
	void __fastcall ItemsModified(const Designintf::_di_IDesigner ADesigner);
	void __fastcall SelectionChanged(const Designintf::_di_IDesigner ADesigner, const Designintf::_di_IDesignerSelections ASelection);
	void __fastcall DesignerOpened(const Designintf::_di_IDesigner ADesigner, bool AResurrecting);
	void __fastcall DesignerClosed(const Designintf::_di_IDesigner ADesigner, bool AGoingDormant);
	__property TSqlSourceList* SqlSourceList = {read=FSqlSourceList};
	__property TModuleList* ModuleList = {read=FModuleList};
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

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TDBTools* DBTools;
extern DELPHI_PACKAGE TCustomDBToolsService* __fastcall GetDBToolsService(Dadesignutils::TDADesignUtilsClass DADesignUtilsClass);
}	/* namespace Dbtoolsclient */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DBTOOLSCLIENT)
using namespace Dbtoolsclient;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DbtoolsclientHPP
