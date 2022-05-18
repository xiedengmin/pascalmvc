// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteClassesVirtual.pas' rev: 35.00 (Windows)

#ifndef LiteclassesvirtualHPP
#define LiteclassesvirtualHPP

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
#include <System.Variants.hpp>
#include <System.SyncObjs.hpp>
#include <Data.FmtBcd.hpp>
#include <Data.SqlTimSt.hpp>
#include <System.AnsiStrings.hpp>
#include <System.Generics.Collections.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRAccess.hpp>
#include <CRParser.hpp>
#include <CRTimeStamp.hpp>
#include <MemData.hpp>
#include <CRDataTypeMap.hpp>
#include <LiteFunctionVirtual.hpp>
#include <LiteCollationVirtual.hpp>
#include <LiteConstsVirtual.hpp>
#include <LiteCallVirtual.hpp>
#include <LiteErrorVirtual.hpp>
#include <LiteParserVirtual.hpp>

//-- user supplied -----------------------------------------------------------

namespace Liteclassesvirtual
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSQLiteConnection;
class DELPHICLASS TSQLiteTransaction;
class DELPHICLASS TSQLiteTypes;
class DELPHICLASS TSQLiteParamDesc;
class DELPHICLASS TSQLiteCommand;
class DELPHICLASS TSQLiteRecordSet;
class DELPHICLASS TSQLiteTableInfo;
struct TColumnInfo;
struct TIndexColumnInfo;
struct TIndexInfo;
struct TConstraintInfo;
struct TTableMembers;
class DELPHICLASS TSQLiteMetaData;
class DELPHICLASS TSQLiteInfo;
class DELPHICLASS TSQLiteLoader;
class DELPHICLASS TSQLiteBackup;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSQLiteConnection : public Craccess::TCRConnection
{
	typedef Craccess::TCRConnection inherited;
	
private:
	TSQLiteTypes* FLiteTypes;
	Litecollationvirtual::TSQLiteCollationManager* FCollationManager;
	Litefunctionvirtual::TSQLiteFunctionManager* FFunctionManager;
	System::UnicodeString FDatabase;
	System::UnicodeString FClientLibrary;
	bool FUseUnicode;
	bool FASCIIDataBase;
	int FCharLength;
	System::UnicodeString FEncryptionKey;
	System::UnicodeString FDateFormat;
	System::UnicodeString FTimeFormat;
	bool FNativeDate;
	bool FEnableSharedCache;
	bool FEnableLoadExtension;
	int FBusyTimeout;
	bool FReadUncommitted;
	bool FDefaultCollations;
	__int64 FLastInsertId;
	bool FForeignKeys;
	bool FStaticLibrary;
	bool FForceCreateDatabase;
	Liteconstsvirtual::TLiteEncryptionAlgorithm FEncryptionAlgorithm;
	Liteconstsvirtual::TConnectMode FConnectMode;
	Liteconstsvirtual::TLockingMode FLockingMode;
	Liteconstsvirtual::TSynchronous FSynchronous;
	Liteconstsvirtual::TJournalMode FJournalMode;
	Litecallvirtual::TSQLite3API* FAPI;
	System::UnicodeString FCipherLicense;
	void __fastcall SetASCIIDataBase(const bool Value);
	void __fastcall SetBusyTimeout(const int Value);
	void __fastcall SetReadUncommitted(const bool Value);
	void __fastcall SetDefaultCollations(const bool Value);
	void __fastcall SetForeignKeys(const bool Value);
	void __fastcall SetUseStaticLibrary(const bool Value);
	void __fastcall SetConnectMode(const Liteconstsvirtual::TConnectMode Value);
	void __fastcall SetLockingMode(const Liteconstsvirtual::TLockingMode Value);
	void __fastcall SetSynchronous(const Liteconstsvirtual::TSynchronous Value);
	void __fastcall SetJournalMode(const Liteconstsvirtual::TJournalMode Value);
	
protected:
	void __fastcall InternalSetBusyTimeout(const int Value);
	void __fastcall InternalSetReadUncommitted(const bool Value);
	void __fastcall InternalSetDefaultCollations(const bool Value);
	void __fastcall InternalSetForeignKeys(const bool Value);
	void __fastcall InternalSetLockingMode(const Liteconstsvirtual::TLockingMode Value);
	void __fastcall InternalSetSynchronous(const Liteconstsvirtual::TSynchronous Value);
	void __fastcall InternalSetJournalMode(const Liteconstsvirtual::TJournalMode Value);
	void __fastcall InternalCos(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalSin(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalTan(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalCot(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalAcos(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalAsin(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalAtan(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalAtan2(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalDegrees(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalRadians(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalTruncate(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalRound(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalCeiling(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalFloor(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalMod(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalPower(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalSqrt(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalSign(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalRand(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalExp(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalLog(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall InternalLog10(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);
	void __fastcall RegisterInternalFunctions();
	virtual Craccess::TCRCommandClass __fastcall GetInternalCommandClass();
	__property EnableBCD;
	__property EnableFMTBCD;
	
public:
	__fastcall virtual TSQLiteConnection();
	__fastcall virtual ~TSQLiteConnection();
	virtual Craccess::TCRCommandClass __fastcall GetCommandClass();
	virtual Craccess::TCRRecordSetClass __fastcall GetRecordSetClass();
	virtual Craccess::TCRTransactionClass __fastcall GetTransactionClass();
	virtual Craccess::TCRLoaderClass __fastcall GetLoaderClass();
	virtual Craccess::TCRMetaDataClass __fastcall GetMetaDataClass();
	virtual Litefunctionvirtual::TSQLiteFunctionManagerClass __fastcall GetFunctionManagerClass();
	__classmethod virtual Crdatatypemap::TCRMapRulesClass __fastcall GetMapRulesClass();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, /* out */ System::Variant &Value);
	void __fastcall Check(int ErrorCode);
	int __fastcall DetectError(int ErrorCode, /* out */ System::UnicodeString &ErrorMsg);
	virtual void __fastcall ProcessError(int ErrorCode, System::UnicodeString ErrorMsg, System::TObject* Component);
	virtual void __fastcall Connect(const System::UnicodeString ConnectString);
	virtual void __fastcall Disconnect();
	virtual void __fastcall Ping();
	virtual bool __fastcall CheckIsValid();
	void __fastcall BreakExec();
	virtual void __fastcall Assign(Craccess::TCRConnection* Source);
	virtual void __fastcall AssignConnect(Craccess::TCRConnection* Source);
	virtual System::UnicodeString __fastcall GetServerVersion();
	virtual System::UnicodeString __fastcall GetServerVersionFull();
	virtual System::UnicodeString __fastcall GetClientVersion();
	virtual bool __fastcall CanChangeDatabase();
	TSQLiteTypes* __fastcall GetTypes();
	Litecollationvirtual::TSQLiteCollationManager* __fastcall GetCollationManager();
	Litefunctionvirtual::TSQLiteFunctionManager* __fastcall GetFunctionManager();
	bool __fastcall IsUnicodeDataBase();
	System::AnsiString __fastcall EncodeString(System::AnsiString AString)/* overload */;
	System::AnsiString __fastcall EncodeString(System::AnsiString AString, bool UseUTF8)/* overload */;
	System::AnsiString __fastcall EncodeString(System::WideString WString)/* overload */;
	System::AnsiString __fastcall EncodeString(System::WideString WString, bool UseUTF8)/* overload */;
	System::UnicodeString __fastcall DecodeString(char * pUtf8String);
	System::AnsiString __fastcall DecodeStringA(char * pUtf8String, bool UseUTF8);
	System::WideString __fastcall DecodeStringW(char * pUtf8String, bool UseUTF8);
	void __fastcall EncryptDatabase(const System::UnicodeString NewKey);
	void __fastcall ReleaseDatabaseMemory();
	int __fastcall GetDatabaseReadOnly(const System::UnicodeString DatabaseName = System::UnicodeString());
	int __fastcall Limit(int Id, int NewVal);
	System::UnicodeString __fastcall NormalizeTableName(const System::UnicodeString TableName);
	__property bool ASCIIDataBase = {read=FASCIIDataBase, nodefault};
	__property bool UseUnicode = {read=FUseUnicode, nodefault};
	__property System::UnicodeString DateFormat = {read=FDateFormat};
	__property System::UnicodeString TimeFormat = {read=FTimeFormat};
	__property bool NativeDate = {read=FNativeDate, nodefault};
	__property bool ForeignKeys = {read=FForeignKeys, nodefault};
	__property bool UseStaticLibrary = {read=FStaticLibrary, nodefault};
	__property Liteconstsvirtual::TLiteEncryptionAlgorithm EncryptionAlgorithm = {read=FEncryptionAlgorithm, nodefault};
	__property Liteconstsvirtual::TConnectMode ConnectMode = {read=FConnectMode, nodefault};
	__property Litecallvirtual::TSQLite3API* API = {read=FAPI};
	__property System::UnicodeString CipherLicense = {read=FCipherLicense};
};


class PASCALIMPLEMENTATION TSQLiteTransaction : public Craccess::TCRTransaction
{
	typedef Craccess::TCRTransaction inherited;
	
public:
	virtual void __fastcall StartTransaction();
	virtual void __fastcall Commit();
	virtual void __fastcall Rollback();
	virtual void __fastcall Savepoint(const System::UnicodeString Name);
	virtual void __fastcall ReleaseSavepoint(const System::UnicodeString Name);
	virtual void __fastcall RollbackToSavepoint(const System::UnicodeString Name);
public:
	/* TCRTransaction.Create */ inline __fastcall virtual TSQLiteTransaction() : Craccess::TCRTransaction() { }
	/* TCRTransaction.Destroy */ inline __fastcall virtual ~TSQLiteTransaction() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteTypes : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSQLiteConnection* FConnection;
	
public:
	__fastcall TSQLiteTypes(TSQLiteConnection* Connection);
	__fastcall virtual ~TSQLiteTypes();
	System::Word __fastcall GetSQLiteType(System::Word DataType, bool &NeedConvertToText);
	System::Word __fastcall GetVarType(System::Word VarType);
	System::AnsiString __fastcall ConvertToText(System::Word DataType, System::Word Scale, const System::Variant &Value);
	System::DynamicArray<System::Byte> __fastcall ConvertBlob(const System::Variant &Value, void * &DestrType);
	System::AnsiString __fastcall ConvertMemo(const System::Variant &Value);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TSQLiteParamDesc : public Craccess::TParamDesc
{
	typedef Craccess::TParamDesc inherited;
	
private:
	System::UnicodeString FNormalizedName;
public:
	/* TParamDesc.Create */ inline __fastcall virtual TSQLiteParamDesc() : Craccess::TParamDesc() { }
	/* TParamDesc.Destroy */ inline __fastcall virtual ~TSQLiteParamDesc() { }
	
};


class PASCALIMPLEMENTATION TSQLiteCommand : public Craccess::TCRCommand
{
	typedef Craccess::TCRCommand inherited;
	
private:
	System::AnsiString FCommandSQL;
	void *FPCommandSQL;
	void *FCurrentCommand;
	void *FCommandTail;
	int FCommandLength;
	bool FIsCursorPragma;
	System::DynamicArray<System::Byte> FCachedBufA;
	System::DynamicArray<System::Byte> FCachedBufW;
	TSQLiteParamDesc* __fastcall FindParamByNormalizedName(const System::UnicodeString Name);
	
protected:
	void __fastcall AllocCachedBuffers();
	void __fastcall FreeCachedBuffers();
	void __fastcall BindParams(int Offset);
	void __fastcall BindBlob(int Index, const System::Variant &Value);
	void __fastcall BindMemo(int Index, const System::Variant &Value);
	void __fastcall BindAnsiText(int Index, char * pAStr, int Len);
	void __fastcall BindWideText(int Index, const System::WideString WStr);
	void __fastcall DetectCommandType();
	
private:
	TSQLiteConnection* FConnection;
	void *FStmt;
	Craccess::TCursorState FCursorState;
	int FExecResult;
	int FRowsAffected;
	bool FLockAfterExecute;
	System::WideChar FDateSeparator;
	System::UnicodeString FShortDateFormat;
	System::WideChar FTimeSeparator;
	System::UnicodeString FShortTimeFormat;
	bool FFormatSettingsInitialized;
	
protected:
	void __fastcall Check(int ErrorCode);
	virtual void __fastcall CheckPrepareError(int ErrorCode);
	void __fastcall InitFormatSettings();
	void __fastcall ResetFormatSettings();
	virtual void __fastcall InternalPrepare();
	virtual void __fastcall InternalExecute();
	virtual void __fastcall CreateBatchCommand();
	virtual bool __fastcall NeedBatchTransaction();
	virtual bool __fastcall NeedBatchSavepoint();
	virtual void __fastcall InternalExecuteBatch(int Iters, int Offset);
	
public:
	__fastcall virtual TSQLiteCommand();
	__fastcall virtual ~TSQLiteCommand();
	__classmethod virtual Craccess::TSQLInfoClass __fastcall GetSQLInfoClass();
	__classmethod virtual Craccess::TTableInfoClass __fastcall GetTableInfoClass();
	__classmethod virtual Crparser::TSQLParserClass __fastcall GetParserClass();
	__classmethod virtual Craccess::TParamDescClass __fastcall GetParamDescClass();
	__classmethod virtual Crdatatypemap::TCRMapRulesClass __fastcall GetMapRulesClass();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, /* out */ System::Variant &Value);
	virtual void __fastcall SetConnection(Craccess::TCRConnection* Value);
	virtual void __fastcall BreakExec();
	virtual void __fastcall Prepare();
	virtual void __fastcall Unprepare();
	virtual bool __fastcall GetPrepared();
	virtual void __fastcall Execute();
	virtual void __fastcall SetSQL(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall ParseSQL(const System::UnicodeString SQL, Craccess::TParamDescs* Params, const System::UnicodeString RenamePrefix = System::UnicodeString())/* overload */;
	virtual void __fastcall ParseSQLType()/* overload */;
	void __fastcall InitProcParams(const System::UnicodeString Name, int Overload);
	virtual System::UnicodeString __fastcall CreateProcCall(const System::UnicodeString Name, bool NeedDescribe, bool IsQuery);
	virtual Craccess::TCursorState __fastcall GetCursorState();
	virtual void __fastcall SetCursorState(Craccess::TCursorState Value);
	__property EnableBCD;
	__property EnableFMTBCD;
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  ParseSQL(){ Craccess::TCRCommand::ParseSQL(); }
	inline void __fastcall  ParseSQLType(const System::UnicodeString SQL){ Craccess::TCRCommand::ParseSQLType(SQL); }
	
};


class PASCALIMPLEMENTATION TSQLiteRecordSet : public Craccess::TCRRecordSet
{
	typedef Craccess::TCRRecordSet inherited;
	
	
private:
	typedef System::DynamicArray<int> _TSQLiteRecordSet__1;
	
	
private:
	TSQLiteCommand* FCommand;
	bool FDumpData;
	bool FUnknownAsString;
	bool FAdvancedTypeDetection;
	bool FHasUndefinedFields;
	bool FInExplicitInit;
	_TSQLiteRecordSet__1 FFieldTypes;
	void __fastcall DescribeFieldDesc(Craccess::TCRFieldDesc* Field, int Index);
	void __fastcall ReadFieldValues(void * RecBuf, int Row);
	void __fastcall ReadFieldValuesForDump(void * RecBuf);
	
protected:
	void __fastcall Check(int ErrorCode);
	virtual void __fastcall CreateCommand();
	virtual void __fastcall SetCommand(Craccess::TCRCommand* Value);
	virtual void __fastcall InternalPrepare();
	virtual bool __fastcall ExtFieldsInfoIsInternal();
	virtual void __fastcall CreateFieldDescs();
	virtual void __fastcall RequestFieldsInfo(Craccess::TSQLObjectsInfo Tables, Craccess::TCRColumnsInfo* Columns);
	virtual void __fastcall InternalClose();
	virtual bool __fastcall IdentityFieldIsData();
	virtual void __fastcall FetchBlock(Memdata::PBlockHeader Block, bool FetchBack, /* out */ int &RowsObtained);
	virtual bool __fastcall NeedUnPrepareAfterFetch();
	virtual System::Word __fastcall GetDBType(const System::UnicodeString SQLTypeName, int &Len, int &Scale)/* overload */;
	virtual System::Word __fastcall GetDBType(int SQLType)/* overload */;
	virtual System::Word __fastcall GetDBUnknown();
	virtual bool __fastcall IsFixedDBType(System::Word DBType);
	virtual System::Word __fastcall GetDataType(System::Word DBType);
	
public:
	__fastcall virtual TSQLiteRecordSet();
	__fastcall virtual ~TSQLiteRecordSet();
	void __fastcall InternalPrefetch();
	virtual void __fastcall ExplicitInitFields();
	virtual void __fastcall GetDataAsVariant(void * DataBuf, System::Word DataLen, System::Word DataType, System::Word SubDataType, bool HasParent, bool IsFixed, System::Variant &Value, bool UseRollback);
	virtual void __fastcall PutDataAsVariant(void * DataBuf, PWORD DataLenPtr, System::Word DataType, System::Word Len, System::Word Scale, bool HasParent, const System::Variant &Value, bool IsDatabaseValue);
	__classmethod virtual void __fastcall GetDateFromBuf(void * Buf, void * Date, bool HasParent, Memdata::TDateFormat Format);
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, /* out */ System::Variant &Value);
	virtual void __fastcall DetectIdentityField();
	void __fastcall DetectFieldType(const System::UnicodeString FieldName, System::Word DBType, int DBLength, short DBScale, /* out */ System::Word &DataType, /* out */ int &Len, /* out */ int &Scale, /* out */ bool &Fixed);
	virtual void __fastcall ExecCommand(int Iters = 0x1, int Offset = 0x0);
	virtual void __fastcall SetToEnd();
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteTableInfo : public Craccess::TCRTableInfo
{
	typedef Craccess::TCRTableInfo inherited;
	
protected:
	System::UnicodeString FTableNameFull;
	int FKeyCount;
	virtual System::UnicodeString __fastcall GetTableNameFull();
	virtual void __fastcall SetTableNameFull(const System::UnicodeString Value);
public:
	/* TCRTableInfo.Create */ inline __fastcall virtual TSQLiteTableInfo(Craccess::TCRTablesInfo* Owner) : Craccess::TCRTableInfo(Owner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSQLiteTableInfo() { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TColumnInfo
{
public:
	System::UnicodeString Name;
	System::UnicodeString DataType;
	bool IsAutoincrement;
	System::UnicodeString Default;
	int Length;
	int Scale;
	bool NotNull;
};


struct DECLSPEC_DRECORD TIndexColumnInfo
{
public:
	int ColumnIndex;
	bool IsDesc;
};


enum DECLSPEC_DENUM TIndexType : unsigned char { itPrimaryKey, itUnique, itNonUnique };

struct DECLSPEC_DRECORD TIndexInfo
{
	
private:
	typedef System::DynamicArray<TIndexColumnInfo> _TIndexInfo__1;
	
	
public:
	TIndexType IndexType;
	_TIndexInfo__1 ColumnInfo;
};


enum DECLSPEC_DENUM TConstraintType : unsigned char { ctPrimaryKey, ctUnique, ctCheck, ctForeignKey };

struct DECLSPEC_DRECORD TConstraintInfo
{
	
private:
	typedef System::DynamicArray<TIndexColumnInfo> _TConstraintInfo__1;
	
	
public:
	TConstraintType ConstraintType;
	System::UnicodeString Name;
	_TConstraintInfo__1 ColumnInfo;
};


struct DECLSPEC_DRECORD TTableMembers
{
	
private:
	typedef System::DynamicArray<TColumnInfo> _TTableMembers__1;
	
	typedef System::DynamicArray<TConstraintInfo> _TTableMembers__2;
	
	
public:
	_TTableMembers__1 Columns;
	_TTableMembers__2 Constraints;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteMetaData : public Craccess::TCRMetaData
{
	typedef Craccess::TCRMetaData inherited;
	
protected:
	TSQLiteConnection* __fastcall GetConnection();
	System::UnicodeString __fastcall GetTypesForSQL(const System::UnicodeString ObjectTypes, System::UnicodeString *AllTypes, const int AllTypes_High);
	virtual Craccess::TCRRecordSet* __fastcall CreateRecordSet();
	virtual void __fastcall InternalGetMetaDataKindsList(System::Classes::TStringList* List);
	virtual Memdata::TData* __fastcall GetTables(System::Classes::TStrings* Restrictions);
	virtual void __fastcall CreateColumnsFields();
	virtual Memdata::TData* __fastcall GetColumns(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetProcedures(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetProcedureParameters(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetIndexes(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetIndexColumns(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetConstraints(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetConstraintColumns(System::Classes::TStrings* Restrictions);
	
public:
	__fastcall virtual TSQLiteMetaData();
	__fastcall virtual ~TSQLiteMetaData();
	__classmethod void __fastcall ParseTableSQL(const System::UnicodeString SQL, System::UnicodeString &TableName, TTableMembers &Members);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteInfo : public Craccess::TSQLInfo
{
	typedef Craccess::TSQLInfo inherited;
	
public:
	virtual System::WideChar __fastcall LeftQuote();
	virtual System::WideChar __fastcall RightQuote();
	virtual Craccess::TIdentCase __fastcall IdentCase();
	virtual bool __fastcall ParamQuoteAllowed();
	virtual bool __fastcall IsQuoted(const System::UnicodeString Value);
	virtual void __fastcall SplitObjectName(const System::UnicodeString Name, /* out */ Craccess::TSQLObjectInfo &Info);
	virtual void __fastcall ParseTablesInfo(const System::UnicodeString SQL, Craccess::TCRTablesInfo* TablesInfo);
public:
	/* TSQLInfo.Create */ inline __fastcall virtual TSQLiteInfo(Crparser::TSQLParserClass ParserClass) : Craccess::TSQLInfo(ParserClass) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSQLiteInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteLoader : public Craccess::TCRSimpleLoader
{
	typedef Craccess::TCRSimpleLoader inherited;
	
private:
	bool FAutoCommit;
	bool FAutoCommitIsUsed;
	int FAutoCommitRowCount;
	
protected:
	virtual void __fastcall CreateCommand();
	virtual void __fastcall DoPrepare();
	virtual void __fastcall DoLoadRow();
	TSQLiteConnection* __fastcall UsedConnection();
	TSQLiteTransaction* __fastcall UsedTransaction();
	
public:
	__fastcall virtual TSQLiteLoader();
	__fastcall virtual ~TSQLiteLoader();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	virtual void __fastcall Finish();
};

#pragma pack(pop)

typedef void __fastcall (__closure *TSQLiteBackupProgressEvent)(int PagesTotal, int PagesRemaining);

class PASCALIMPLEMENTATION TSQLiteBackup : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSQLiteConnection* FSrcConnection;
	TSQLiteConnection* FDestConnection;
	void *FBackupHandle;
	System::UnicodeString FSourceDatabaseName;
	System::UnicodeString FDestinationDatabaseName;
	int FPagesPerStep;
	bool FWaitWhenLocked;
	int FWaitDelay;
	int FWaitTimeout;
	TSQLiteBackupProgressEvent FOnProgress;
	void __fastcall Init();
	bool __fastcall Step();
	void __fastcall Finish();
	int __fastcall GetPageCount();
	int __fastcall GetRemaining();
	
public:
	__fastcall TSQLiteBackup(TSQLiteConnection* SourceConnection, TSQLiteConnection* DestinationConnection);
	void __fastcall Backup();
	__property System::UnicodeString SourceDatabaseName = {read=FSourceDatabaseName, write=FSourceDatabaseName};
	__property System::UnicodeString DestinationDatabaseName = {read=FDestinationDatabaseName, write=FDestinationDatabaseName};
	__property int PagesPerStep = {read=FPagesPerStep, write=FPagesPerStep, default=-1};
	__property bool WaitWhenLocked = {read=FWaitWhenLocked, write=FWaitWhenLocked, default=0};
	__property int WaitDelay = {read=FWaitDelay, write=FWaitDelay, default=250};
	__property int WaitTimeout = {read=FWaitTimeout, write=FWaitTimeout, default=0};
	__property TSQLiteBackupProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSQLiteBackup() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TSQLiteInfo* SQLiteInfo;
extern DELPHI_PACKAGE System::TDateTime __fastcall ConvertStrToDateTime(System::Word DataType, System::UnicodeString Value, const System::WideChar ADateSeparator, const System::WideChar ATimeSeparator, const System::UnicodeString AShortDateFormat, const System::UnicodeString AShortTimeFormat, bool NativeDate, /* out */ System::UnicodeString &Fractions, /* out */ bool &Error);
extern DELPHI_PACKAGE Data::Sqltimst::TSQLTimeStampOffset __fastcall ConvertStrToTimestampOffset(System::UnicodeString Value, const System::WideChar ADateSeparator, const System::WideChar ATimeSeparator, const System::UnicodeString AShortDateFormat, const System::UnicodeString AShortTimeFormat);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FractionsToText(unsigned Fractions, System::Word Scale);
extern DELPHI_PACKAGE System::UnicodeString __fastcall TimezoneToText(short TimeZoneHour, short TimeZoneMinute, System::WideChar TimeSeparator);
}	/* namespace Liteclassesvirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITECLASSESVIRTUAL)
using namespace Liteclassesvirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LiteclassesvirtualHPP
