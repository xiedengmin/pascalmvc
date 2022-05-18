// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBForgeClientImp.pas' rev: 34.00 (Windows)

#ifndef DbforgeclientimpHPP
#define DbforgeclientimpHPP

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
#include <DBToolsIntf.hpp>
#include <DBToolsClient.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Messages.hpp>
#include <CRFunctions.hpp>
#include <DASQLGenerator.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dbforgeclientimp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDBForgeService;
class DELPHICLASS TDACSqlEditorFrameF;
class DELPHICLASS TSourceNotifierF;
class DELPHICLASS TSqlSourceF;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBForgeService : public Dbtoolsclient::TCustomDBToolsService
{
	typedef Dbtoolsclient::TCustomDBToolsService inherited;
	
	
private:
	typedef System::DynamicArray<Dbtoolsclient::TCompareFlag> _TDBForgeService__1;
	
	typedef System::DynamicArray<Dbtoolsintf::_di_ISqlEditor> _TDBForgeService__2;
	
	typedef System::DynamicArray<Dbtoolsclient::TParamTypeMap> _TDBForgeService__3;
	
	
protected:
	System::Classes::TStringList* FUsedConnectionStrList;
	_TDBForgeService__1 FUsedConnectionCompareFlags;
	System::Classes::TStringList* FConnectionStrList;
	System::Classes::TStringList* FConnectionsList;
	System::Classes::TStringList* FDefaultConnectionList;
	Dbtoolsintf::_di_IDbToolsService FSqlService;
	_TDBForgeService__2 FSqlEditors;
	_TDBForgeService__3 FParamTypeMaps;
	virtual Dbtoolsclient::TCustomDACSqlEditorFrameClass __fastcall GetDACSqlEditorFrameClass();
	virtual Dbtoolsclient::TCustomSqlSourceClass __fastcall SqlSourceClass();
	Dbtoolsintf::_di_ISqlEditor __fastcall GetSqlEditor();
	void __fastcall ReturnSqlEditor(Dbtoolsintf::_di_ISqlEditor ASqlEditor);
	TSqlSourceF* __fastcall GetSqlSource(System::Classes::TComponent* Component, Designintf::_di_IDesigner Designer, System::UnicodeString SqlTextPrefix = System::UnicodeString(), Dbaccess::TStatementType StatementType = (Dbaccess::TStatementType)(0x0));
	__classmethod void * __fastcall AccessData(const Winapi::Activex::PSafeArray V);
	__classmethod void __fastcall UnaccessData(const Winapi::Activex::PSafeArray V);
	__classmethod int __fastcall DataHigh(const Winapi::Activex::PSafeArray V);
	__classmethod System::UnicodeString __fastcall GetConnectionParamStr(const System::UnicodeString ParamName, const System::UnicodeString ParamValue);
	System::UnicodeString __fastcall GetConnectionValueStr(System::UnicodeString ConnectionName);
	void __fastcall ConnStrToList(System::UnicodeString ConnStr, System::Classes::TStrings* const ConnList);
	void __fastcall CheckConnection(System::Classes::TComponent* const Component);
	void __fastcall BeginConnectionStrGetting(System::Classes::TStringList* const ConnectionStrList);
	virtual void __fastcall DesignerClosing(System::UnicodeString DesignerName);
	__classmethod virtual System::UnicodeString __fastcall GetNamespace();
	
public:
	__fastcall virtual TDBForgeService(Dadesignutils::TDADesignUtilsClass ADADesignUtils, System::_di_IInterface ASqlService, System::UnicodeString ADefaultConnectionStr);
	__fastcall virtual ~TDBForgeService();
	Data::Db::TFieldType __fastcall DBToolsTypeToDataType(int AType, Data::Db::TFieldType OldType);
	int __fastcall DataTypeToDBToolsType(Data::Db::TFieldType AType);
	System::UnicodeString __fastcall GetNativeConnectionString(System::Classes::TComponent* const Component);
	System::WideChar * __fastcall GetConnectionString(System::Classes::TComponent* const Component);
	bool __fastcall GetConnectionStringObjectTypeAndFullName(System::Classes::TComponent* const Component, /* out */ System::WideChar * &ConnectionString, /* out */ System::WideChar * &ObjectType, /* out */ System::WideChar * &FullName);
	virtual void __fastcall GetConnections(System::Classes::TStrings* NameList, System::UnicodeString Condition = System::UnicodeString());
	virtual System::UnicodeString __fastcall FindConnectionName(Dbaccess::TCustomDAConnection* AConnection);
	virtual System::Classes::TStringList* __fastcall GetConnectionStrList(System::UnicodeString ConnectionName);
	virtual void __fastcall FindInDatabaseExplorer();
	virtual void __fastcall EditDatabaseObject();
	virtual void __fastcall ExecuteSql(bool Debug);
	virtual void __fastcall Compile(bool Debug);
	virtual void __fastcall RetrieveData(bool AsDocument);
	virtual void __fastcall EditSql(bool AsQuery, Dbaccess::TStatementType StatementType = (Dbaccess::TStatementType)(0x0));
	virtual void __fastcall AddParamTypeMap(Data::Db::TFieldType ADACType, int ADBToolsType);
	virtual void __fastcall PutConnectionParam(const System::UnicodeString ConnectionParam, const Dbtoolsclient::TCompareFlag CompareFlag = (Dbtoolsclient::TCompareFlag)(0x0));
	virtual void __fastcall SkipConnectionParams(const int Count);
	__property Dbtoolsintf::_di_IDbToolsService SqlService = {read=FSqlService};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TDACSqlEditorFrameF : public Dbtoolsclient::TCustomDACSqlEditorFrame
{
	typedef Dbtoolsclient::TCustomDACSqlEditorFrame inherited;
	
	
private:
	typedef System::StaticArray<Dbtoolsintf::_di_ISqlEditor, 11> _TDACSqlEditorFrameF__1;
	
	
private:
	TDBForgeService* FDBToolsService;
	_TDACSqlEditorFrameF__1 FSqlEditors;
	Dbaccess::TStatementType FStatementType;
	System::Classes::TComponent* FComponent;
	System::UnicodeString FLastConnectionString;
	bool FInInit;
	Dbtoolsintf::_di_ISqlEditor __fastcall GetSqlEditor();
	void __fastcall InternalResize();
	
protected:
	virtual void __fastcall SetStatementType(const Dbaccess::TStatementType Value);
	virtual System::UnicodeString __fastcall GetText();
	virtual void __fastcall SetText(const System::UnicodeString Value);
	virtual void __fastcall SetReadOnly(bool Value);
	DYNAMIC void __fastcall Resize();
	HWND __fastcall GetSqlEditorHandle();
	virtual void __fastcall CheckModified();
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall EndInit();
	virtual void __fastcall CheckConnectionChange();
	
public:
	__fastcall virtual TDACSqlEditorFrameF(System::Classes::TComponent* AOwner, System::Classes::TComponent* Component, Dbtoolsclient::TCustomDBToolsService* DBToolsService);
	__fastcall virtual ~TDACSqlEditorFrameF();
	virtual void __fastcall SetFocus();
	__property Dbtoolsintf::_di_ISqlEditor SqlEditor = {read=GetSqlEditor};
public:
	/* TWinControl.CreateParented */ inline __fastcall TDACSqlEditorFrameF(HWND ParentWindow) : Dbtoolsclient::TCustomDACSqlEditorFrame(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSourceNotifierF : public Dbtoolsclient::TCustomSourceNotifier
{
	typedef Dbtoolsclient::TCustomSourceNotifier inherited;
	
public:
	Dbtoolsintf::_di_ISqlSourceNotifier FSqlSourceNotifier;
	virtual void __fastcall OnSqlSourceDeleted();
public:
	/* TCustomSourceNotifier.Create */ inline __fastcall virtual TSourceNotifierF() : Dbtoolsclient::TCustomSourceNotifier() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSourceNotifierF() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSqlSourceF : public Dbtoolsclient::TCustomSqlSource
{
	typedef Dbtoolsclient::TCustomSqlSource inherited;
	
	
private:
	typedef System::DynamicArray<bool> _TSqlSourceF__1;
	
	
protected:
	int FParameterCount;
	_TSqlSourceF__1 FParameterSetted;
	System::UnicodeString FDesignerName;
	System::UnicodeString FLastName;
	System::UnicodeString FComponentSQL;
	Dbaccess::TStatementType FStatementType;
	TDBForgeService* FDBToolsService;
	Dbaccess::TCustomDAConnection* FLastConnection;
	System::UnicodeString FLastConnectionString;
	System::UnicodeString FID;
	Dbaccess::TDAParams* __fastcall GetParams();
	System::UnicodeString __fastcall GetSqlText();
	void __fastcall SetSqlText(System::UnicodeString Value);
	System::WideChar * __stdcall Get_Name();
	System::WideChar * __stdcall Get_ConnectionString();
	System::WideChar * __stdcall Get_DesignerName();
	System::WideChar * __stdcall Get_Sql();
	void __stdcall Set_Sql(const System::WideChar * Param1);
	void __stdcall GetParameter(const int Index, /* out */ Dbtoolsintf::CommandParameterInfo &Info);
	void __stdcall SetParameter(const int Index, Dbtoolsintf::CommandParameterInfo Info);
	void __stdcall Set_ParameterCount(const int Value);
	int __stdcall Get_ParameterCount();
	System::WideChar * __stdcall Get_ID();
	void __stdcall Close();
	virtual Dbtoolsclient::TCustomSourceNotifierClass __fastcall GetSourceNotifierClass();
	virtual void __fastcall FreeSourceNotifier();
	virtual Dbtoolsclient::TCustomDBToolsService* __fastcall GetDBToolsService();
	__property Dbaccess::TDAParams* Params = {read=GetParams};
	
public:
	__fastcall TSqlSourceF(TDBForgeService* DBToolsService, System::Classes::TComponent* Component, Designintf::_di_IDesigner Designer, Dbaccess::TStatementType StatementType);
	__fastcall virtual ~TSqlSourceF();
	virtual void __fastcall CheckRename();
	virtual void __fastcall CheckConnectionChange(bool InternalCheck);
	virtual void __fastcall CheckChanges();
	__property Dbtoolsclient::TCustomSourceNotifier* SqlSourceNotifier = {read=FSqlSourceNotifier};
private:
	void *__ISqlSource;	// Dbtoolsintf::ISqlSource 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {471AE99C-AE21-491B-89E3-A2C717C81CA8}
	operator Dbtoolsintf::_di_ISqlSource()
	{
		Dbtoolsintf::_di_ISqlSource intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Dbtoolsintf::ISqlSource*(void) { return (Dbtoolsintf::ISqlSource*)&__ISqlSource; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dbforgeclientimp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DBFORGECLIENTIMP)
using namespace Dbforgeclientimp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DbforgeclientimpHPP
