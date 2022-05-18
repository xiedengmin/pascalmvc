// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVirtualQuery.pas' rev: 35.00 (Windows)

#ifndef CrvirtualqueryHPP
#define CrvirtualqueryHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>
#include <CRAccess.hpp>
#include <CRDataTypeMap.hpp>
#include <MemData.hpp>
#include <CRVirtualData.hpp>
#include <LiteCallVirtual.hpp>
#include <LiteClassesVirtual.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crvirtualquery
{
//-- forward type declarations -----------------------------------------------
struct sqlite3_module;
struct sqlite3_vtab;
struct sqlite3_vtab_cursor;
struct sqlite3_index_constraint;
struct sqlite3_index_orderby;
struct sqlite3_index_constraint_usage;
struct sqlite3_index_info;
class DELPHICLASS TSQLiteVirtualTableCursor;
class DELPHICLASS TSQLiteVirtualTable;
class DELPHICLASS TSQLiteVirtualTables;
class DELPHICLASS TSQLiteModule;
class DELPHICLASS TCRVirtualTransaction;
class DELPHICLASS TCRVirtualMetaData;
class DELPHICLASS TCRVirtualConnection;
class DELPHICLASS TCRVirtualSQLInfo;
class DELPHICLASS TCRVirtualCommand;
class DELPHICLASS TCRVirtualRecordSet;
//-- type declarations -------------------------------------------------------
typedef void * *PIntPtr;

typedef System::DynamicArray<System::UnicodeString> TSQLiteArgs;

enum DECLSPEC_DENUM TTableCreationMode : unsigned char { cmAll, cmOnDemand };

typedef sqlite3_module *psqlite3_module;

typedef sqlite3_vtab *psqlite3_vtab;

typedef sqlite3_vtab_cursor *psqlite3_vtab_cursor;

typedef sqlite3_index_constraint *psqlite3_index_constraint;

typedef sqlite3_index_info *psqlite3_index_info;

typedef sqlite3_index_orderby *psqlite3_index_orderby;

typedef sqlite3_index_constraint_usage *psqlite3_index_constraint_usage;

typedef void * psqlite3_value;

typedef void * *ppsqlite3_value;

typedef void __cdecl (*Tsqlite3_func)(void * pContext, int nargs, ppsqlite3_value args);

struct DECLSPEC_DRECORD sqlite3_module
{
public:
	int iVersion;
	int __cdecl (*xCreate)(void * pSQLite, void * pAux, int argc, PIntPtr argv, psqlite3_vtab &ppVTab, void * &pzErrMsg);
	int __cdecl (*xConnect)(void * pSQLite, void * pAux, int argc, PIntPtr argv, psqlite3_vtab &ppVTab, void * &pzErrMsg);
	int __cdecl (*xBestIndex)(psqlite3_vtab pVTab, psqlite3_index_info pIndexInfo);
	int __cdecl (*xDisconnect)(psqlite3_vtab pVTab);
	int __cdecl (*xDestroy)(psqlite3_vtab pVTab);
	int __cdecl (*xOpen)(psqlite3_vtab pVTab, psqlite3_vtab_cursor &ppCursor);
	int __cdecl (*xClose)(psqlite3_vtab_cursor pCursor);
	int __cdecl (*xFilter)(psqlite3_vtab_cursor pCursor, int idxNum, void * idxStr, int argc, ppsqlite3_value argv);
	int __cdecl (*xNext)(psqlite3_vtab_cursor pCursor);
	int __cdecl (*xEof)(psqlite3_vtab_cursor pCursor);
	int __cdecl (*xColumn)(psqlite3_vtab_cursor pCursor, void * pContext, int index);
	int __cdecl (*xRowid)(psqlite3_vtab_cursor pCursor, __int64 &pRowid);
	int __cdecl (*xUpdate)(psqlite3_vtab pVTab, int argc, ppsqlite3_value &argv, __int64 &pRowid);
	int __cdecl (*xBegin)(psqlite3_vtab pVTab);
	int __cdecl (*xSync)(psqlite3_vtab pVTab);
	int __cdecl (*xCommit)(psqlite3_vtab pVTab);
	int __cdecl (*xRollback)(psqlite3_vtab pVTab);
	int __cdecl (*xFindFunction)(psqlite3_vtab pVTab, int nArg, void * zName, Tsqlite3_func &pxFunc, void * &ppArg);
	int __cdecl (*xRename)(psqlite3_vtab pVTab, void * zNew);
	int __cdecl (*xSavepoint)(psqlite3_vtab pVTab, int n);
	int __cdecl (*xRelease)(psqlite3_vtab pVTab, int r);
	int __cdecl (*xRollbackTo)(psqlite3_vtab pVTab, int r);
};


struct DECLSPEC_DRECORD sqlite3_vtab
{
public:
	sqlite3_module *pModule;
	int nRef;
	void *zErrMsg;
	TSQLiteVirtualTable* pSQLiteVirtualTable;
};


struct DECLSPEC_DRECORD sqlite3_vtab_cursor
{
public:
	sqlite3_vtab *pVtab;
	TSQLiteVirtualTableCursor* pSQLiteVirtualTableCursor;
};


struct DECLSPEC_DRECORD sqlite3_index_constraint
{
public:
	int iColumn;
	System::Byte op;
	System::Byte usable;
	int iTermOffset;
};


struct DECLSPEC_DRECORD sqlite3_index_orderby
{
public:
	int iColumn;
	System::Byte desc;
};


struct DECLSPEC_DRECORD sqlite3_index_constraint_usage
{
public:
	int argvIndex;
	System::Byte omit;
};


struct DECLSPEC_DRECORD sqlite3_index_info
{
public:
	int nConstraint;
	sqlite3_index_constraint *aConstraint;
	int nOrderBy;
	sqlite3_index_orderby *aOrderBy;
	sqlite3_index_constraint_usage *aConstraintUsage;
	int idxNum;
	void *idxStr;
	int needToFreeIdxStr;
	int orderByConsumed;
	double estimatedCost;
	__int64 estimatedRows;
	int idxFlags;
	unsigned __int64 colUsed;
};


class PASCALIMPLEMENTATION TSQLiteVirtualTableCursor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSQLiteVirtualTable* FTable;
	sqlite3_vtab_cursor FInternalCursor;
	Crvirtualdata::TVirtualConstraint FFilter;
	__int64 FBookmark;
	void __fastcall InternalCreateCursor();
	
public:
	__fastcall TSQLiteVirtualTableCursor(TSQLiteVirtualTable* Table);
	int __fastcall Filter(int idxNum, void * idxStr, int argc, ppsqlite3_value argv);
	void __fastcall Open();
	void __fastcall Close();
	void __fastcall Next();
	bool __fastcall Eof();
	int __fastcall GetRowId(__int64 &pRowid);
	int __fastcall GetFieldValue(void * pContext, int index);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSQLiteVirtualTableCursor() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteVirtualTable : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSQLiteModule* FModule;
	sqlite3_vtab FInternalTable;
	System::UnicodeString FSchemaName;
	System::UnicodeString FTableName;
	Crvirtualdata::TVirtualData* FData;
	int FConstraintCount;
	Crvirtualdata::TVirtualConstraints FConstraints;
	Crtypes::TCRObjectList* FCursors;
	void __fastcall InternalCreateTable();
	void __fastcall InternalDropTable();
	void __fastcall ReadValue(const Crvirtualdata::PVirtualValue Value, ppsqlite3_value &ppValue);
	void __fastcall AddLastError(const System::UnicodeString Error);
	int __fastcall StartTransaction();
	int __fastcall CommitTransaction();
	int __fastcall RollbackTransaction();
	int __fastcall BestIndex(psqlite3_index_info pIndexInfo);
	int __fastcall OpenCursor(psqlite3_vtab_cursor &ppCursor);
	int __fastcall CloseCursor(TSQLiteVirtualTableCursor* Cursor);
	int __fastcall Update(int argc, ppsqlite3_value argv, __int64 &pRowid);
	
public:
	__fastcall TSQLiteVirtualTable(TSQLiteModule* Module, const System::UnicodeString SchemaName, const System::UnicodeString TableName, Crvirtualdata::TVirtualData* const Data);
	__fastcall virtual ~TSQLiteVirtualTable();
	__property System::UnicodeString SchemaName = {read=FSchemaName};
	__property System::UnicodeString TableName = {read=FTableName};
	__property Crvirtualdata::TVirtualData* Data = {read=FData};
};

#pragma pack(pop)

typedef System::TMetaClass* TVirtualTableClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteVirtualTables : public Crtypes::TCRObjectList
{
	typedef Crtypes::TCRObjectList inherited;
	
public:
	TSQLiteVirtualTable* __fastcall Find(const System::UnicodeString SchemaName, const System::UnicodeString TableName);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TSQLiteVirtualTables() { }
	
public:
	/* TObject.Create */ inline __fastcall TSQLiteVirtualTables() : Crtypes::TCRObjectList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteModule : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCRVirtualConnection* FConnection;
	TSQLiteVirtualTables* FTables;
	sqlite3_module FInternalModule;
	System::UnicodeString FName;
	void __fastcall InternalCreateModule();
	TSQLiteVirtualTable* __fastcall DoCreateTable(const TSQLiteArgs Args);
	TSQLiteVirtualTable* __fastcall DoConnectTable(const TSQLiteArgs Args);
	int __fastcall CreateTable(bool PerformCreate, void * pSQLite, int argc, PIntPtr argv, psqlite3_vtab &ppVTab, void * &pzErrMsg);
	
public:
	__fastcall TSQLiteModule(TCRVirtualConnection* Connection, const System::UnicodeString ModuleName);
	__fastcall virtual ~TSQLiteModule();
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRVirtualTransaction : public Liteclassesvirtual::TSQLiteTransaction
{
	typedef Liteclassesvirtual::TSQLiteTransaction inherited;
	
public:
	/* TCRTransaction.Create */ inline __fastcall virtual TCRVirtualTransaction() : Liteclassesvirtual::TSQLiteTransaction() { }
	/* TCRTransaction.Destroy */ inline __fastcall virtual ~TCRVirtualTransaction() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRVirtualMetaData : public Liteclassesvirtual::TSQLiteMetaData
{
	typedef Liteclassesvirtual::TSQLiteMetaData inherited;
	
public:
	/* TSQLiteMetaData.Create */ inline __fastcall virtual TCRVirtualMetaData() : Liteclassesvirtual::TSQLiteMetaData() { }
	/* TSQLiteMetaData.Destroy */ inline __fastcall virtual ~TCRVirtualMetaData() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRVirtualConnection : public Liteclassesvirtual::TSQLiteConnection
{
	typedef Liteclassesvirtual::TSQLiteConnection inherited;
	
private:
	System::TObject* FOwner;
	TSQLiteModule* FModule;
	System::Classes::TStringList* FSchemas;
	System::Classes::TStringList* FSpecificTypes;
	TTableCreationMode FTableCreationMode;
	int FTablesCreated;
	bool FLockCreateTables;
	bool FLockDropTables;
	bool __fastcall GetFieldDataType(const System::UnicodeString DataTypeName, System::Word &DataType, int &Len, int &Scale);
	TSQLiteVirtualTables* __fastcall GetTables();
	
protected:
	__classmethod virtual TVirtualTableClass __fastcall GetVirtualTableClass();
	
public:
	__fastcall virtual TCRVirtualConnection()/* overload */;
	__fastcall TCRVirtualConnection(System::TObject* const Owner)/* overload */;
	__fastcall virtual ~TCRVirtualConnection();
	__classmethod virtual Crdatatypemap::TCRMapRulesClass __fastcall GetMapRulesClass();
	virtual Craccess::TCRCommandClass __fastcall GetCommandClass();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, /* out */ System::Variant &Value);
	Crvirtualdata::TVirtualData* __fastcall FindVirtualData(const System::UnicodeString SchemaName, const System::UnicodeString TableName);
	void __fastcall RegisterVirtualData(const System::UnicodeString SchemaName, const System::UnicodeString TableName, Crvirtualdata::TVirtualData* const Reader);
	virtual void __fastcall Connect(const System::UnicodeString ConnectString);
	virtual void __fastcall Disconnect();
	void __fastcall CheckSchema(const System::UnicodeString SchemaName, const System::UnicodeString DatabaseName);
	void __fastcall DoCreateTable(const System::UnicodeString SchemaName, const System::UnicodeString TableName);
	void __fastcall DoDropTable(const System::UnicodeString SchemaName, const System::UnicodeString TableName);
	__property TSQLiteVirtualTables* Tables = {read=GetTables};
	__property System::Classes::TStringList* SpecificTypes = {read=FSpecificTypes};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRVirtualSQLInfo : public Liteclassesvirtual::TSQLiteInfo
{
	typedef Liteclassesvirtual::TSQLiteInfo inherited;
	
private:
	Crparser::TSQLParserClass FParserClass;
	
public:
	__fastcall virtual TCRVirtualSQLInfo(Crparser::TSQLParserClass ParserClass);
	virtual System::WideChar __fastcall LeftQuote();
	virtual System::WideChar __fastcall RightQuote();
	virtual void __fastcall ParseTablesInfo(const System::UnicodeString SQL, Craccess::TCRTablesInfo* TablesInfo);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCRVirtualSQLInfo() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRVirtualCommand : public Liteclassesvirtual::TSQLiteCommand
{
	typedef Liteclassesvirtual::TSQLiteCommand inherited;
	
protected:
	Craccess::TCRTablesInfo* FTablesInfo;
	virtual void __fastcall InternalCreateTablesInfo();
	void __fastcall InternalCreateTables();
	void __fastcall InternalDropTables();
	virtual void __fastcall InternalExecute();
	
public:
	__fastcall virtual TCRVirtualCommand();
	__fastcall virtual ~TCRVirtualCommand();
	__classmethod virtual Craccess::TSQLInfoClass __fastcall GetSQLInfoClass();
	__classmethod virtual Crdatatypemap::TCRMapRulesClass __fastcall GetMapRulesClass();
};


class PASCALIMPLEMENTATION TCRVirtualRecordSet : public Liteclassesvirtual::TSQLiteRecordSet
{
	typedef Liteclassesvirtual::TSQLiteRecordSet inherited;
	
private:
	bool FLockDisableControls;
	
protected:
	virtual void __fastcall CreateCommand();
	void __fastcall InternalCreateTables();
	void __fastcall InternalDropTables();
	virtual void __fastcall InternalPrepare();
	virtual void __fastcall InternalOpen(bool DisableInitFields = false);
	virtual void __fastcall InternalClose();
	virtual void __fastcall DoBeforeFetch(/* out */ bool &Cancel);
	virtual void __fastcall DoAfterFetch();
	virtual System::Word __fastcall GetDBType(const System::UnicodeString SQLTypeName, int &Len, int &Scale)/* overload */;
	virtual System::Word __fastcall GetDBType(int SQLType)/* overload */;
	virtual System::Word __fastcall GetDBUnknown();
	virtual bool __fastcall IsFixedDBType(System::Word DBType);
	virtual System::Word __fastcall GetDataType(System::Word DBType);
	
public:
	virtual void __fastcall Reopen();
	virtual void __fastcall SetSQL(const System::UnicodeString Value);
public:
	/* TSQLiteRecordSet.Create */ inline __fastcall virtual TCRVirtualRecordSet() : Liteclassesvirtual::TSQLiteRecordSet() { }
	/* TSQLiteRecordSet.Destroy */ inline __fastcall virtual ~TCRVirtualRecordSet() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crvirtualquery */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVIRTUALQUERY)
using namespace Crvirtualquery;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrvirtualqueryHPP
