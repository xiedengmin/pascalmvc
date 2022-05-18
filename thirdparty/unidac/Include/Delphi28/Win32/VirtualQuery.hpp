// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualQuery.pas' rev: 35.00 (Windows)

#ifndef VirtualqueryHPP
#define VirtualqueryHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Data.DB.hpp>
#include <CRTypes.hpp>
#include <MemData.hpp>
#include <CRAccess.hpp>
#include <CLRClasses.hpp>
#include <MemDS.hpp>
#include <DAConsts.hpp>
#include <DBAccess.hpp>
#include <DASQLGenerator.hpp>
#include <LiteCallVirtual.hpp>
#include <CRVirtualData.hpp>
#include <CRVirtualQuery.hpp>
#include <CRVirtualConsts.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualquery
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMemDataSetData;
class DELPHICLASS TDataSetFieldAccessor;
class DELPHICLASS TVirtualDataSetIndex;
class DELPHICLASS TDataSetData;
class DELPHICLASS TSourceDataLink;
class DELPHICLASS TDataSetLink;
class DELPHICLASS TDataSetLinks;
class DELPHICLASS TVirtualSQLGenerator;
class DELPHICLASS TVirtualDataSetUpdater;
class DELPHICLASS TVirtualDataSetService;
class DELPHICLASS TVirtualTransaction;
class DELPHICLASS TVirtualConnection;
class DELPHICLASS TVirtualQueryOptions;
class DELPHICLASS TCustomVirtualQuery;
class DELPHICLASS TVirtualQuery;
class DELPHICLASS TVirtualFunctionManager;
class DELPHICLASS TVirtualCollationManager;
//-- type declarations -------------------------------------------------------
typedef System::Variant __fastcall (*TVirtualFunction)(System::Variant *InValues, const int InValues_High);

typedef void __fastcall (__closure *TVirtualMethod)(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);

typedef void __fastcall (__closure *TRegisterFunctionsEvent)(System::TObject* Sender, TVirtualFunctionManager* const FunctionManager);

typedef int __fastcall (*TVirtualCollation)(const System::UnicodeString Str1, const System::UnicodeString Str2);

typedef int __fastcall (__closure *TVirtualCollationMethod)(const System::UnicodeString Str1, const System::UnicodeString Str2);

typedef int __fastcall (*TVirtualAnsiCollation)(const System::AnsiString Str1, const System::AnsiString Str2);

typedef int __fastcall (__closure *TVirtualAnsiCollationMethod)(const System::AnsiString Str1, const System::AnsiString Str2);

typedef int __fastcall (*TVirtualWideCollation)(const System::WideString Str1, const System::WideString Str2);

typedef int __fastcall (__closure *TVirtualWideCollationMethod)(const System::WideString Str1, const System::WideString Str2);

typedef void __fastcall (__closure *TRegisterCollationsEvent)(System::TObject* Sender, TVirtualCollationManager* const CollationManager);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMemDataSetData : public Crvirtualdata::TVirtualMemData
{
	typedef Crvirtualdata::TVirtualMemData inherited;
	
private:
	Memds::TMemDataSet* FMemDataSet;
	
public:
	__fastcall TMemDataSetData(Memds::TMemDataSet* const MemDataSet);
	virtual __int64 __fastcall Open(const Crvirtualdata::PVirtualConstraint Filter);
	virtual void __fastcall EditRecord(const Crvirtualdata::TVirtualValues Values);
	virtual void __fastcall InsertRecord(const Crvirtualdata::TVirtualValues Values);
public:
	/* TVirtualMemData.Destroy */ inline __fastcall virtual ~TMemDataSetData() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataSetFieldAccessor : public Crvirtualdata::TFieldAccessor
{
	typedef Crvirtualdata::TFieldAccessor inherited;
	
public:
	__classmethod virtual __int64 __fastcall AsInteger(const void * Field, const void * Buffer);
	__classmethod virtual double __fastcall AsFloat(const void * Field, const void * Buffer);
	__classmethod virtual System::AnsiString __fastcall AsAnsiString(const void * Field, const void * Buffer);
	__classmethod virtual System::WideString __fastcall AsWideString(const void * Field, const void * Buffer);
public:
	/* TObject.Create */ inline __fastcall TDataSetFieldAccessor() : Crvirtualdata::TFieldAccessor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDataSetFieldAccessor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualDataSetIndex : public Crvirtualdata::TVirtualLocalIndex
{
	typedef Crvirtualdata::TVirtualLocalIndex inherited;
	
protected:
	virtual void * __fastcall GetFieldPtr(const int FieldIndex);
	virtual void * __fastcall GetBuffer(const void * Item);
	virtual int __fastcall InternalCompare(const void * FieldPtr, void * Item1, void * Item2);
	virtual bool __fastcall IsIntegerField(const void * FieldPtr);
	virtual bool __fastcall IsDateTimeField(const void * FieldPtr);
	virtual bool __fastcall IsFloatField(const void * FieldPtr);
	virtual bool __fastcall IsAnsiStringField(const void * FieldPtr);
	virtual bool __fastcall IsWideStringField(const void * FieldPtr);
public:
	/* TVirtualLocalIndex.Create */ inline __fastcall TVirtualDataSetIndex(Crvirtualdata::TVirtualData* const Data, int Capacity) : Crvirtualdata::TVirtualLocalIndex(Data, Capacity) { }
	/* TVirtualLocalIndex.Destroy */ inline __fastcall virtual ~TVirtualDataSetIndex() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataSetData : public Crvirtualdata::TVirtualData
{
	typedef Crvirtualdata::TVirtualData inherited;
	
	
private:
	typedef System::DynamicArray<System::DynamicArray<System::Byte> > _TDataSetData__1;
	
	
private:
	Data::Db::TDataSet* FDataSet;
	_TDataSetData__1 FBookmarks;
	System::DynamicArray<System::Byte> FSavedBookmark;
	int FBookmark;
	int FSavedRecNo;
	bool FOpening;
	bool FNeedDisableControls;
	int __fastcall FindBookmark(const System::DynamicArray<System::Byte> Bookmark);
	
protected:
	__classmethod virtual Crvirtualdata::TVirtualLocalIndexClass __fastcall GetLocalIndexClass();
	virtual Crvirtualdata::TFieldAccessorClass __fastcall GetFieldAccessorClass();
	virtual bool __fastcall GetNextRecord(const Crvirtualdata::PVirtualConstraint Filter);
	virtual void __fastcall InternalGetCurrentRecord();
	virtual void __fastcall InternalNext();
	virtual bool __fastcall InternalEof(const Crvirtualdata::PVirtualConstraint Filter);
	virtual void __fastcall InternalDescribeFields(System::Classes::TStringList* const SpecificTypes);
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const System::Variant &Value)/* overload */;
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const __int64 Value)/* overload */;
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const double Value)/* overload */;
	
public:
	__fastcall TDataSetData(Data::Db::TDataSet* const DataSet);
	__fastcall virtual ~TDataSetData();
	virtual bool __fastcall Active();
	virtual __int64 __fastcall Open(const Crvirtualdata::PVirtualConstraint Filter);
	virtual void __fastcall Close();
	virtual __int64 __fastcall Next(const __int64 Bookmark, const Crvirtualdata::PVirtualConstraint Filter);
	virtual __int64 __fastcall GetBookmark();
	virtual void __fastcall GotoBookmark(const __int64 Bookmark);
	virtual int __fastcall GetRecordCount();
	virtual bool __fastcall GetFieldNull(int FieldIndex);
	virtual System::Variant __fastcall GetFieldValue(int FieldIndex)/* overload */;
	virtual void __fastcall DisableControls(bool SaveRecNo);
	virtual void __fastcall EnableControls();
	virtual void __fastcall EditRecord(const Crvirtualdata::TVirtualValues Values);
	virtual void __fastcall InsertRecord(const Crvirtualdata::TVirtualValues Values);
	virtual void __fastcall DeleteRecord();
	virtual bool __fastcall InTransaction();
	virtual void __fastcall StartTransaction();
	virtual void __fastcall Commit();
	virtual void __fastcall Rollback();
	virtual bool __fastcall IsSimpleFieldType(const int FieldIndex);
	virtual int __fastcall CompareInteger(const void * Field, const void * Buffer, const __int64 Value);
	virtual int __fastcall CompareFloat(const void * Field, const void * Buffer, const double Value);
	virtual int __fastcall CompareAnsiString(const void * Field, const void * Buffer, const System::AnsiString Value);
	virtual int __fastcall CompareWideString(const void * Field, const void * Buffer, const System::WideString Value);
	virtual int __fastcall CompareVariant(const void * Field, const void * Buffer, const System::Variant &Value);
	virtual int __fastcall GetLocalIndex(const Crvirtualdata::PVirtualConstraint Constraint);
	/* Hoisted overloads: */
	
public:
	inline System::Variant __fastcall  GetFieldValue(int FieldIndex, bool &FieldNull){ return Crvirtualdata::TVirtualData::GetFieldValue(FieldIndex, FieldNull); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSourceDataLink : public Data::Db::TDataLink
{
	typedef Data::Db::TDataLink inherited;
	
private:
	TDataSetLink* FOwner;
	
protected:
	virtual void __fastcall ActiveChanged();
	
public:
	__fastcall TSourceDataLink(TDataSetLink* Owner);
public:
	/* TDataLink.Destroy */ inline __fastcall virtual ~TSourceDataLink() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataSetLink : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FSchemaName;
	System::UnicodeString FTableName;
	Data::Db::TDataSource* FDataSource;
	TSourceDataLink* FDataLink;
	Data::Db::TDataSet* __fastcall GetDataSet();
	void __fastcall SetDataSet(Data::Db::TDataSet* const Value);
	System::UnicodeString __fastcall GetSchemaName();
	void __fastcall SetSchemaName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetTableName();
	void __fastcall SetTableName(const System::UnicodeString Value);
	bool __fastcall GetTableNameStored();
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TDataSetLink(System::Classes::TCollection* Collection);
	__fastcall virtual ~TDataSetLink();
	
__published:
	__property System::UnicodeString SchemaName = {read=GetSchemaName, write=SetSchemaName};
	__property System::UnicodeString TableName = {read=GetTableName, write=SetTableName, stored=GetTableNameStored};
	__property Data::Db::TDataSet* DataSet = {read=GetDataSet, write=SetDataSet};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataSetLinks : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TDataSetLink* operator[](int Index) { return this->Items[Index]; }
	
private:
	TCustomVirtualQuery* FOwner;
	HIDESBASE TDataSetLink* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TDataSetLink* Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TDataSetLinks(TCustomVirtualQuery* AOwner);
	__fastcall virtual ~TDataSetLinks();
	HIDESBASE TDataSetLink* __fastcall Add()/* overload */;
	HIDESBASE TDataSetLink* __fastcall Add(Data::Db::TDataSet* DataSet)/* overload */;
	HIDESBASE TDataSetLink* __fastcall Add(Data::Db::TDataSet* DataSet, const System::UnicodeString TableName)/* overload */;
	HIDESBASE TDataSetLink* __fastcall Add(Data::Db::TDataSet* DataSet, const System::UnicodeString SchemaName, const System::UnicodeString TableName)/* overload */;
	__property TDataSetLink* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualSQLGenerator : public Dasqlgenerator::TDASQLGenerator
{
	typedef Dasqlgenerator::TDASQLGenerator inherited;
	
public:
	/* TDASQLGenerator.Create */ inline __fastcall virtual TVirtualSQLGenerator(Dasqlgenerator::TSQLGeneratorServiceClass ServiceClass) : Dasqlgenerator::TDASQLGenerator(ServiceClass) { }
	/* TDASQLGenerator.Destroy */ inline __fastcall virtual ~TVirtualSQLGenerator() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualDataSetUpdater : public Dbaccess::TDADataSetUpdater
{
	typedef Dbaccess::TDADataSetUpdater inherited;
	
protected:
	virtual void __fastcall SetUpdateQueryOptions(const Dbaccess::TStatementType StatementType, const bool IsAutoGeneratedSQL);
	
public:
	virtual bool __fastcall PerformSQL(const System::UnicodeString SQL, const Dbaccess::TStatementTypes StatementTypes);
public:
	/* TDADataSetUpdater.Create */ inline __fastcall virtual TVirtualDataSetUpdater(Memds::TDataSetService* AOwner) : Dbaccess::TDADataSetUpdater(AOwner) { }
	/* TDADataSetUpdater.Destroy */ inline __fastcall virtual ~TVirtualDataSetUpdater() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualDataSetService : public Dbaccess::TDADataSetService
{
	typedef Dbaccess::TDADataSetService inherited;
	
protected:
	virtual void __fastcall CreateDataSetUpdater();
	virtual void __fastcall CreateSQLGenerator();
	virtual bool __fastcall DetectCanModify();
	virtual int __fastcall GetRecCount();
public:
	/* TDADataSetService.Create */ inline __fastcall virtual TVirtualDataSetService(Memds::TMemDataSet* AOwner) : Dbaccess::TDADataSetService(AOwner) { }
	/* TDADataSetService.Destroy */ inline __fastcall virtual ~TVirtualDataSetService() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TVirtualTransaction : public Dbaccess::TDATransaction
{
	typedef Dbaccess::TDATransaction inherited;
	
protected:
	virtual Craccess::TCRTransactionClass __fastcall GetITransactionClass();
public:
	/* TDATransaction.Create */ inline __fastcall virtual TVirtualTransaction(System::Classes::TComponent* AOwner) : Dbaccess::TDATransaction(AOwner) { }
	/* TDATransaction.Destroy */ inline __fastcall virtual ~TVirtualTransaction() { }
	
};


class PASCALIMPLEMENTATION TVirtualConnection : public Dbaccess::TCustomDAConnection
{
	typedef Dbaccess::TCustomDAConnection inherited;
	
private:
	TCustomVirtualQuery* FVirtualQuery;
	
protected:
	virtual Craccess::TCRConnectionClass __fastcall GetIConnectionClass();
	virtual Craccess::TCRRecordSetClass __fastcall GetIRecordSetClass();
	virtual Craccess::TCRTransactionClass __fastcall GetITransactionClass();
	virtual Craccess::TCRMetaDataClass __fastcall GetIMetaDataClass();
	virtual void __fastcall CreateIConnection();
	virtual void __fastcall DoConnect();
	virtual void __fastcall DoDisconnect();
	
public:
	__fastcall virtual TVirtualConnection(System::Classes::TComponent* Owner)/* overload */;
	virtual Dbaccess::TDATransaction* __fastcall CreateTransaction();
	virtual Dbaccess::TCustomDADataSet* __fastcall CreateDataSet(System::Classes::TComponent* AOwner = (System::Classes::TComponent*)(0x0));
	void __fastcall RegisterDataSetLinks();
public:
	/* TCustomDAConnection.Create */ inline __fastcall TVirtualConnection(System::Classes::TComponent* Owner, const System::UnicodeString ConnectString)/* overload */ : Dbaccess::TCustomDAConnection(Owner, ConnectString) { }
	/* TCustomDAConnection.Destroy */ inline __fastcall virtual ~TVirtualConnection() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualQueryOptions : public Dbaccess::TDADataSetOptions
{
	typedef Dbaccess::TDADataSetOptions inherited;
	
private:
	Crvirtualquery::TTableCreationMode FTableCreationMode;
	bool FAutoOpenSources;
	bool FUseUnicode;
	void __fastcall SetTableCreationMode(const Crvirtualquery::TTableCreationMode Value);
	void __fastcall SetAutoOpenSources(const bool Value);
	void __fastcall SetUseUnicode(const bool Value);
	
protected:
	__property Crvirtualquery::TTableCreationMode TableCreationMode = {read=FTableCreationMode, write=SetTableCreationMode, default=0};
	
public:
	__fastcall TVirtualQueryOptions(Dbaccess::TCustomDADataSet* Owner);
	
__published:
	__property bool AutoOpenSources = {read=FAutoOpenSources, write=SetAutoOpenSources, default=0};
	__property bool UseUnicode = {read=FUseUnicode, write=SetUseUnicode, default=0};
	__property FullRefresh = {default=0};
	__property TrimVarChar = {default=0};
	__property SetEmptyStrToNull = {default=0};
	__property ExtendedFieldsInfo = {default=1};
	__property SetFieldsReadOnly = {default=1};
	__property RequiredFields = {default=1};
	__property StrictUpdate = {default=1};
	__property PrepareUpdateSQL = {default=0};
	__property NumberRange = {default=0};
	__property QueryRecCount = {default=0};
	__property AutoPrepare = {default=0};
	__property ReturnParams = {default=0};
	__property TrimFixedChar = {default=1};
	__property LongStrings = {default=1};
	__property RemoveOnRefresh = {default=1};
	__property QuoteNames = {default=0};
	__property DetailDelay = {default=0};
	__property CacheCalcFields = {default=0};
	__property FieldOrigins = {default=0};
	__property UpdateBatchSize = {default=1};
	__property UpdateAllFields = {default=0};
	__property MasterFieldsNullable = {default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TVirtualQueryOptions() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCustomVirtualQuery : public Dbaccess::TCustomDADataSet
{
	typedef Dbaccess::TCustomDADataSet inherited;
	
private:
	TVirtualConnection* FInternalConnection;
	TDataSetLinks* FSourceDataSets;
	TVirtualFunctionManager* FFunctionManager;
	TVirtualCollationManager* FCollationManager;
	bool FStreamedActive;
	bool FLockRegisterLinks;
	bool FOwnConnection;
	TRegisterFunctionsEvent FOnRegisterFunctions;
	TRegisterCollationsEvent FOnRegisterCollations;
	bool __fastcall GetKeepDesignConnected();
	void __fastcall SetKeepDesignConnected(bool Value);
	void __fastcall SetSourceDataSets(TDataSetLinks* const Value);
	TVirtualQueryOptions* __fastcall GetOptions();
	HIDESBASE void __fastcall SetOptions(TVirtualQueryOptions* const Value);
	void __fastcall SetInternalConnection(TVirtualConnection* const Value);
	void __fastcall SetLockRegisterLinks(bool Value);
	void __fastcall LinkActiveChanged(bool Value);
	void __fastcall InternalRegisterLinks();
	
protected:
	virtual Memds::TDataSetServiceClass __fastcall GetDataSetServiceClass();
	virtual Dbaccess::TDADataSetOptions* __fastcall CreateOptions();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall Loaded();
	virtual Dbaccess::TCustomDAConnection* __fastcall UsedConnection();
	virtual void __fastcall SetActive(bool Value);
	virtual void __fastcall Disconnect(bool NeedClose = true);
	virtual void __fastcall InternalExecute(int Iters, int Offset);
	virtual void __fastcall InternalClose();
	virtual void __fastcall BeforeOpenCursor(bool InfoQuery);
	virtual void __fastcall InitFieldDefs();
	
public:
	__fastcall virtual TCustomVirtualQuery(System::Classes::TComponent* Owner);
	__fastcall virtual ~TCustomVirtualQuery();
	virtual void __fastcall Prepare();
	__property bool KeepDesignConnected = {read=GetKeepDesignConnected, write=SetKeepDesignConnected, default=0};
	__property TDataSetLinks* SourceDataSets = {read=FSourceDataSets, write=SetSourceDataSets};
	__property TVirtualQueryOptions* Options = {read=GetOptions, write=SetOptions};
	__property TRegisterFunctionsEvent OnRegisterFunctions = {read=FOnRegisterFunctions, write=FOnRegisterFunctions};
	__property TRegisterCollationsEvent OnRegisterCollations = {read=FOnRegisterCollations, write=FOnRegisterCollations};
};


class PASCALIMPLEMENTATION TVirtualQuery : public TCustomVirtualQuery
{
	typedef TCustomVirtualQuery inherited;
	
__published:
	__property Active = {default=0};
	__property AutoCalcFields = {default=1};
	__property CachedUpdates = {default=0};
	__property Constraints = {stored=IsConstraintsStored};
	__property DataTypeMap;
	__property Options;
	__property Debug = {default=0};
	__property DetailFields = {default=0};
	__property FetchAll = {default=0};
	__property FetchRows = {default=25};
	__property Filter = {default=0};
	__property Filtered = {default=0};
	__property FilterOptions = {default=0};
	__property FilterSQL = {default=0};
	__property IndexFieldNames = {default=0};
	__property KeyFields = {default=0};
	__property LocalUpdate = {default=0};
	__property Macros;
	__property MasterFields = {default=0};
	__property MasterSource;
	__property ParamCheck = {default=1};
	__property Params;
	__property ReadOnly = {default=0};
	__property RefreshOptions = {default=0};
	__property SourceDataSets;
	__property SQL;
	__property SQLDelete;
	__property SQLInsert;
	__property SQLLock;
	__property SQLRefresh;
	__property SQLUpdate;
	__property SQLRecCount;
	__property UniDirectional = {default=0};
	__property UpdatingTable = {default=0};
	__property BeforeExecute;
	__property AfterExecute;
	__property BeforeUpdateExecute;
	__property AfterUpdateExecute;
	__property OnUpdateError;
	__property OnUpdateRecord;
	__property BeforeOpen;
	__property AfterOpen;
	__property BeforeFetch;
	__property AfterFetch;
	__property BeforeClose;
	__property AfterClose;
	__property BeforeInsert;
	__property AfterInsert;
	__property BeforeEdit;
	__property AfterEdit;
	__property BeforePost;
	__property AfterPost;
	__property BeforeCancel;
	__property AfterCancel;
	__property BeforeDelete;
	__property AfterDelete;
	__property BeforeScroll;
	__property AfterScroll;
	__property OnCalcFields;
	__property OnDeleteError;
	__property OnEditError;
	__property OnFilterRecord;
	__property OnNewRecord;
	__property OnPostError;
	__property AfterRefresh;
	__property BeforeRefresh;
	__property OnRegisterFunctions;
	__property OnRegisterCollations;
public:
	/* TCustomVirtualQuery.Create */ inline __fastcall virtual TVirtualQuery(System::Classes::TComponent* Owner) : TCustomVirtualQuery(Owner) { }
	/* TCustomVirtualQuery.Destroy */ inline __fastcall virtual ~TVirtualQuery() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualFunctionManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TVirtualConnection* FConnection;
	void __fastcall UnregisterFunctions();
	
public:
	__fastcall TVirtualFunctionManager(TVirtualConnection* const Connection);
	void __fastcall RegisterFunction(const System::UnicodeString Name, int ParamCount, TVirtualFunction VirtualFunction)/* overload */;
	void __fastcall RegisterFunction(const System::UnicodeString Name, int ParamCount, TVirtualMethod VirtualMethod)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualFunctionManager() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualCollationManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TVirtualConnection* FConnection;
	void __fastcall UnregisterCollations();
	
public:
	__fastcall TVirtualCollationManager(TVirtualConnection* const Connection);
	void __fastcall RegisterCollation(const System::UnicodeString Name, TVirtualCollation VirtualCollation)/* overload */;
	void __fastcall RegisterCollation(const System::UnicodeString Name, TVirtualCollationMethod VirtualCollation)/* overload */;
	void __fastcall UnRegisterCollation(const System::UnicodeString Name);
	void __fastcall RegisterAnsiCollation(const System::UnicodeString Name, TVirtualAnsiCollation VirtualAnsiCollation)/* overload */;
	void __fastcall RegisterAnsiCollation(const System::UnicodeString Name, TVirtualAnsiCollationMethod VirtualAnsiCollation)/* overload */;
	void __fastcall UnRegisterAnsiCollation(const System::UnicodeString Name);
	void __fastcall RegisterWideCollation(const System::UnicodeString Name, TVirtualWideCollation VirtualWideCollation)/* overload */;
	void __fastcall RegisterWideCollation(const System::UnicodeString Name, TVirtualWideCollationMethod VirtualWideCollation)/* overload */;
	void __fastcall UnRegisterWideCollation(const System::UnicodeString Name);
	void __fastcall RegisterDefaultCollations();
	void __fastcall UnRegisterDefaultCollations();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualCollationManager() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Virtualquery */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALQUERY)
using namespace Virtualquery;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualqueryHPP
