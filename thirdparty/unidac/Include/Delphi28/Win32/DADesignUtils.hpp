// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DADesignUtils.pas' rev: 35.00 (Windows)

#ifndef DadesignutilsHPP
#define DadesignutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CRTypes.hpp>
#include <CRDesignUtils.hpp>
#include <CRDataTypeMap.hpp>
#include <Data.DB.hpp>
#include <DBAccess.hpp>
#include <DASQLGenerator.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dadesignutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDADesignUtils;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TNeedToCheckDbTools : unsigned char { ncNone, ncExpired, ncNoAddin, ncIncompatible };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADesignUtils : public Crdesignutils::TCRDesignUtils
{
	typedef Crdesignutils::TCRDesignUtils inherited;
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetProjectName();
	__classmethod virtual bool __fastcall GetDesignCreate(System::Classes::TComponent* Obj);
	__classmethod virtual void __fastcall SetDesignCreate(System::Classes::TComponent* Obj, bool Value);
	__classmethod virtual System::TObject* __fastcall GetConnectionList();
	__classmethod virtual bool __fastcall HasConnection(System::Classes::TComponent* Obj);
	__classmethod virtual Dbaccess::TCustomDAConnection* __fastcall GetConnection(System::Classes::TComponent* Obj);
	__classmethod virtual void __fastcall SetConnection(System::Classes::TComponent* Obj, Dbaccess::TCustomDAConnection* Value);
	__classmethod virtual Dbaccess::TCustomDAConnection* __fastcall UsedConnection(System::Classes::TComponent* Obj);
	__classmethod virtual System::Classes::TStrings* __fastcall GetSQL(System::Classes::TComponent* Obj, Dbaccess::TStatementType StatementType = (Dbaccess::TStatementType)(0x0));
	__classmethod virtual void __fastcall SetSQL(System::Classes::TComponent* Obj, System::Classes::TStrings* Value, Dbaccess::TStatementType StatementType = (Dbaccess::TStatementType)(0x0))/* overload */;
	__classmethod virtual void __fastcall SetSQL(System::Classes::TComponent* Obj, System::UnicodeString Value, Dbaccess::TStatementType StatementType = (Dbaccess::TStatementType)(0x0))/* overload */;
	__classmethod virtual System::UnicodeString __fastcall GetSQLPropName(System::Classes::TComponent* Obj, Dbaccess::TStatementType StatementType);
	__classmethod virtual Dbaccess::TDAParams* __fastcall GetParams(System::Classes::TComponent* Obj);
	__classmethod virtual void __fastcall SetParams(System::Classes::TComponent* Obj, Dbaccess::TDAParams* Value);
	__classmethod Dbaccess::TMacros* __fastcall GetMacros(System::Classes::TComponent* Obj);
	__classmethod void __fastcall SetMacros(System::Classes::TComponent* Obj, Dbaccess::TMacros* Value);
	__classmethod Dbaccess::TDAConditions* __fastcall GetConditions(System::Classes::TComponent* Obj);
	__classmethod void __fastcall SetConditions(System::Classes::TComponent* Obj, Dbaccess::TDAConditions* Value);
	__classmethod void __fastcall Execute(System::Classes::TComponent* Obj);
	__classmethod Dbaccess::TAfterExecuteEvent __fastcall GetAfterExecute(System::Classes::TComponent* Obj);
	__classmethod void __fastcall SetAfterExecute(System::Classes::TComponent* Obj, Dbaccess::TAfterExecuteEvent Value);
	__classmethod void __fastcall Close(System::Classes::TComponent* Obj);
	__classmethod virtual Dbaccess::TStatementTypes __fastcall GetStatementTypes();
	__classmethod virtual System::UnicodeString __fastcall GetTableName(Dbaccess::TCustomDADataSet* Obj);
	__classmethod virtual void __fastcall SetTableName(Dbaccess::TCustomDADataSet* Obj, const System::UnicodeString Value);
	__classmethod virtual System::UnicodeString __fastcall GetOrderFields(Dbaccess::TCustomDADataSet* Obj);
	__classmethod virtual void __fastcall SetOrderFields(Dbaccess::TCustomDADataSet* Obj, const System::UnicodeString Value);
	__classmethod virtual void __fastcall PrepareSQL(Dbaccess::TCustomDADataSet* Obj);
	__classmethod virtual System::UnicodeString __fastcall GetStoredProcName(Dbaccess::TCustomDADataSet* Obj);
	__classmethod virtual void __fastcall SetStoredProcName(Dbaccess::TCustomDADataSet* Obj, const System::UnicodeString Value);
	__classmethod virtual Crdatatypemap::TConverterManagerClass __fastcall GetConverterManagerClass();
	__classmethod virtual int __fastcall SQLDialect();
	__classmethod virtual bool __fastcall DBToolsAvailable();
	__classmethod virtual System::TObject* __fastcall DBToolsService();
	__classmethod virtual TNeedToCheckDbTools __fastcall NeedToCheckDbTools();
	__classmethod virtual __int64 __fastcall GetDBToolsServiceVersion();
	__classmethod System::UnicodeString __fastcall GetDBToolsServiceVersionStr();
	__classmethod virtual System::UnicodeString __fastcall GetDBToolsMenuCaption();
	__classmethod virtual System::UnicodeString __fastcall GetFullName(System::Classes::TComponent* Obj);
	__classmethod virtual System::UnicodeString __fastcall GetObjectType(System::Classes::TComponent* Obj);
	__classmethod virtual void __fastcall SetDBToolsDownloadParams(bool VerbCheck, bool Incompatible);
	__classmethod bool __fastcall HasParams(System::Classes::TComponent* Obj);
	__classmethod virtual bool __fastcall IsStoredProc(System::Classes::TComponent* Obj);
	__classmethod virtual void __fastcall GetDBToolsConnectionList(Dbaccess::TCustomDAConnection* Connection);
	__classmethod virtual bool __fastcall AreDBToolsUsed();
	__classmethod virtual void __fastcall CheckComponent(System::Classes::TComponent* Component);
public:
	/* TObject.Create */ inline __fastcall TDADesignUtils() : Crdesignutils::TCRDesignUtils() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDADesignUtils() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDADesignUtilsClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dadesignutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DADESIGNUTILS)
using namespace Dadesignutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DadesignutilsHPP
