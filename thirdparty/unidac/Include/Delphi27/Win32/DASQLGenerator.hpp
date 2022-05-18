// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DASQLGenerator.pas' rev: 34.00 (Windows)

#ifndef DasqlgeneratorHPP
#define DasqlgeneratorHPP

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
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRParser.hpp>
#include <CLRClasses.hpp>
#include <MemData.hpp>
#include <CRAccess.hpp>
#include <DAConsts.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dasqlgenerator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSQLGeneratorService;
class DELPHICLASS TDASQLGenerator;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM _TStatementType : unsigned char { _stQuery, _stInsert, _stUpdate, _stDelete, _stLock, _stRefresh, _stCustom, _stRefreshQuick, _stRefreshCheckDeleted, _stBatchUpdate, _stRecCount };

typedef System::TMetaClass* TSQLGeneratorServiceClass;

typedef System::TMetaClass* TDASQLGeneratorClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLGeneratorService : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__fastcall virtual TSQLGeneratorService();
	virtual void __fastcall RaiseError(const System::UnicodeString Message);
	virtual void * __fastcall GetOldRecBuf() = 0 ;
	virtual void * __fastcall GetNewRecBuf() = 0 ;
	virtual bool __fastcall BlobFieldModified(Craccess::TCRFieldDesc* FieldDesc);
	virtual Memdata::TSharedObject* __fastcall GetFieldObject(Memdata::TFieldDesc* FieldDesc);
	virtual Craccess::TCRCommand* __fastcall GetUpdateCommand();
	virtual System::UnicodeString __fastcall GetDBKeyList(const System::UnicodeString TableName, const System::UnicodeString IndexName);
	virtual bool __fastcall ParamExists(const System::UnicodeString ParamName);
	virtual System::UnicodeString __fastcall BaseSQL() = 0 ;
	virtual System::UnicodeString __fastcall FinalSQL() = 0 ;
	virtual System::UnicodeString __fastcall FilterSQL() = 0 ;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSQLGeneratorService() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDASQLGenerator : public Craccess::TSQLGenerator
{
	typedef Craccess::TSQLGenerator inherited;
	
protected:
	TSQLGeneratorService* FService;
	Craccess::TCRRecordSet* FIRecordSet;
	Craccess::TCRCommand* FICommand;
	Craccess::TCRRecordSet* FIUpdateRecordSet;
	Craccess::TCRTableInfo* FTableInfo;
	System::UnicodeString FKeySequence;
	Clrclasses::WideStringBuilder* FHeaderSB;
	Clrclasses::WideStringBuilder* FFldSB;
	Clrclasses::WideStringBuilder* FMiddleSB;
	Clrclasses::WideStringBuilder* FFldParamSB;
	Clrclasses::WideStringBuilder* FCondSB;
	Clrclasses::WideStringBuilder* FFooterSB;
	void *FOldRecBuf;
	void *FNewRecBuf;
	bool FDesignMode;
	bool FDMLRefresh;
	bool FSubstituteParamName;
	System::UnicodeString FRefreshSQLFields;
	bool FIsUsedIndexNameForFields;
	Craccess::TCRConnection* __fastcall GetIConnection();
	void __fastcall SetIRecordSet(Craccess::TCRRecordSet* Value);
	Craccess::TSQLInfo* __fastcall SQLInfo();
	Crparser::TSQLParserClass __fastcall GetParserClass();
	bool __fastcall GetFullRefresh();
	bool __fastcall GetQuoteNames();
	System::UnicodeString __fastcall QuoteName(const System::UnicodeString AName);
	bool __fastcall CachedUpdates();
	bool __fastcall InCacheProcessing();
	void * __fastcall GetOldRecBuf();
	void * __fastcall GetNewRecBuf();
	virtual void __fastcall Clear();
	System::UnicodeString __fastcall AssembleSB();
	virtual bool __fastcall FieldIsNull(Craccess::TCRFieldDesc* FieldDesc, bool OldValue, Memdata::TData* Data, void * OldRecBuf, void * NewRecBuf)/* overload */;
	virtual bool __fastcall FieldIsNull(Craccess::TCRFieldDesc* FieldDesc, bool OldValue)/* overload */;
	bool __fastcall FieldIsChanged(Craccess::TCRFieldDesc* FieldDesc);
	bool __fastcall BlobModified(Craccess::TCRFieldDesc* FieldDesc, Memdata::TData* Data, void * OldRecBuf, void * NewRecBuf);
	virtual bool __fastcall FieldModified(Craccess::TCRFieldDesc* FieldDesc, Memdata::TData* Data, void * OldRecBuf, void * NewRecBuf)/* overload */;
	virtual bool __fastcall FieldModified(Craccess::TCRFieldDesc* FieldDesc)/* overload */;
	System::UnicodeString __fastcall GetActualFieldName(Craccess::TCRFieldDesc* FieldDesc, bool FullRefresh);
	virtual System::UnicodeString __fastcall GetFullTableName(Craccess::TCRFieldDesc* FieldDesc);
	virtual System::UnicodeString __fastcall GenerateIndexName(const System::UnicodeString Name);
	virtual int __fastcall MaxIdentLength();
	virtual System::WideChar __fastcall ParamPrefix();
	virtual bool __fastcall IsSubstituteParamName();
	virtual System::UnicodeString __fastcall SubstitutedParamName(Craccess::TDAParamsInfo* ParamsInfo);
	System::UnicodeString __fastcall SQLGetFrom(const System::UnicodeString SQLText);
	System::UnicodeString __fastcall SQLGetWhere(const System::UnicodeString SQLText);
	System::UnicodeString __fastcall SQLAddWhere(const System::UnicodeString SQLText, const System::UnicodeString Condition);
	System::UnicodeString __fastcall SQLSetOrderBy(const System::UnicodeString SQLText, const System::UnicodeString Fields);
	virtual void __fastcall AddParam(Craccess::TDAParamsInfo* ParamsInfo, Clrclasses::WideStringBuilder* SB, Craccess::TCRFieldDesc* FieldDesc, const _TStatementType StatementType, Craccess::TParamDirection ParamType, int Index = 0xffffffff, bool Old = false, bool AllowDuplicates = true);
	Craccess::TParamDesc* __fastcall AddParamDesc(Craccess::TDAParamsInfo* ParamsInfo, Clrclasses::WideStringBuilder* SB, Craccess::TCRFieldDesc* FieldDesc, const _TStatementType StatementType, const Craccess::TParamDirection ParamType, int Index = 0xffffffff);
	virtual void __fastcall AddFieldToInsertSQL(Craccess::TDAParamsInfo* ParamsInfo, Craccess::TCRFieldDesc* FieldDesc, const int Index = 0xffffffff);
	virtual void __fastcall AddFieldToUpdateSQL(Craccess::TDAParamsInfo* ParamsInfo, Craccess::TCRFieldDesc* FieldDesc, const bool ModifiedFieldsOnly, const int Index = 0xffffffff);
	virtual void __fastcall AddFieldToRefreshSQL(Craccess::TCRFieldDesc* FieldDesc);
	virtual void __fastcall AddFieldToCondition(Craccess::TDAParamsInfo* ParamsInfo, Clrclasses::WideStringBuilder* SB, Craccess::TCRFieldDesc* FieldDesc, const _TStatementType StatementType, const int Index = 0xffffffff);
	virtual void __fastcall GenerateInsertSQL(Craccess::TDAParamsInfo* ParamsInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields, const bool ModifiedFieldsOnly, const int Index = 0xffffffff);
	virtual void __fastcall GenerateUpdateSQL(Craccess::TDAParamsInfo* ParamsInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields, const bool ModifiedFieldsOnly, const int Index = 0xffffffff);
	virtual void __fastcall GenerateDeleteSQL(Craccess::TDAParamsInfo* ParamsInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields, const int Index = 0xffffffff);
	virtual void __fastcall GenerateLockSQL(Craccess::TDAParamsInfo* ParamsInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields, const int Index = 0xffffffff);
	void __fastcall GenerateFullRefreshSQL();
	virtual void __fastcall GenerateRefreshSQLSelectPart(const Craccess::TKeyAndDataFields &KeyAndDataFields);
	virtual void __fastcall GenerateRefreshSQLFromPart();
	virtual void __fastcall GenerateRefreshSQL(Craccess::TDAParamsInfo* ParamsInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields);
	virtual void __fastcall GenerateRefreshQuickSQL(Craccess::TDAParamsInfo* ParamsInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields);
	virtual void __fastcall GenerateRefreshCheckDeletedSQL(const Craccess::TKeyAndDataFields &KeyAndDataFields);
	virtual void __fastcall GenerateConditions(Craccess::TDAParamsInfo* ParamsInfo, Clrclasses::WideStringBuilder* SB, const _TStatementType StatementType, const Craccess::TKeyAndDataFields &KeyAndDataFields, const int Index = 0xffffffff);
	__property bool FullRefresh = {read=GetFullRefresh, nodefault};
	__property bool QuoteNames = {read=GetQuoteNames, nodefault};
	
public:
	__fastcall virtual TDASQLGenerator(TSQLGeneratorServiceClass ServiceClass);
	__fastcall virtual ~TDASQLGenerator();
	virtual Craccess::TSQLGenerator* __fastcall CloneGenerator();
	System::UnicodeString __fastcall IndexedPrefix();
	virtual int __fastcall DecodeFieldIndex(const System::UnicodeString FieldName);
	int __fastcall GetParamOffset(Craccess::TDAParamInfo* ParamInfo);
	System::UnicodeString __fastcall GenerateSQLforUpdTable(Craccess::TDAParamsInfo* ParamsInfo, Craccess::TCRTableInfo* UpdatingTableInfo, const Craccess::TKeyAndDataFields &KeyAndDataFields, const _TStatementType StatementType, const bool ModifiedFieldsOnly, const int Index = 0xffffffff);
	virtual System::UnicodeString __fastcall GenerateSQL(Craccess::TDAParamsInfo* ParamsInfo, const _TStatementType StatementType, const bool ModifiedFieldsOnly, const int Index = 0xffffffff);
	virtual System::UnicodeString __fastcall GenerateTableSQL(const System::UnicodeString TableName, const System::UnicodeString OrderFields);
	virtual System::UnicodeString __fastcall GenerateRecCountSQL(bool UseBaseSQL = false);
	virtual System::UnicodeString __fastcall GenerateSelectValues(const System::UnicodeString ValuesList);
	virtual System::UnicodeString __fastcall GenerateEmptyTableSQL(const System::UnicodeString TableName);
	virtual System::UnicodeString __fastcall GenerateSmartFetchMetaInfoSQL();
	virtual System::UnicodeString __fastcall GenerateSmartFetchKeyOnlySQL(const Craccess::TFieldDescArray PrefetchedFields);
	virtual void __fastcall PrepareSmartFetchDataByKeySQL(const System::UnicodeString Sql, Craccess::TSmartFetchInfo* SmartFetchInfo);
	virtual System::UnicodeString __fastcall GenerateSmartFetchDataByKeySQL(Craccess::TSmartFetchInfo* SmartFetchInfo, Craccess::TDAParamsInfo* ParamsInfo, Memdata::PItemHeader FirstItem, int RowCount);
	__property Craccess::TCRRecordSet* IRecordSet = {write=SetIRecordSet};
	__property Craccess::TCRRecordSet* IUpdateRecordSet = {write=FIUpdateRecordSet};
	__property bool DesignMode = {write=FDesignMode, nodefault};
	__property bool DMLRefresh = {write=FDMLRefresh, nodefault};
	__property bool SubstituteParamName = {write=FSubstituteParamName, nodefault};
	__property System::UnicodeString KeySequence = {read=FKeySequence, write=FKeySequence};
	__property bool IsUsedIndexNameForFields = {read=FIsUsedIndexNameForFields, nodefault};
	__property System::UnicodeString RefreshSQLFields = {read=FRefreshSQLFields};
	__property TSQLGeneratorService* Service = {read=FService};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dasqlgenerator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASQLGENERATOR)
using namespace Dasqlgenerator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DasqlgeneratorHPP
