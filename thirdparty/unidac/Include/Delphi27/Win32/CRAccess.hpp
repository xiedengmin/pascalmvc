// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRAccess.pas' rev: 34.00 (Windows)

#ifndef CraccessHPP
#define CraccessHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <MTSCall.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.SyncObjs.hpp>
#include <System.Variants.hpp>
#include <Data.FmtBcd.hpp>
#include <Data.SqlTimSt.hpp>
#include <CRDataTypeMap.hpp>
#include <CREncryption.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRTimeStamp.hpp>
#include <CRProps.hpp>
#include <CRParser.hpp>
#include <CRVio.hpp>
#include <MemData.hpp>
#include <MemUtils.hpp>

//-- user supplied -----------------------------------------------------------
class TCRFieldDesc;
class TParamDesc;

namespace Craccess
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EFailOver;
struct TItemRefCount;
class DELPHICLASS ESmartFetchError;
struct TKeyAndDataFields;
class DELPHICLASS TSmartFetchInfo;
class DELPHICLASS TSQLGenerator;
class DELPHICLASS TCRConnector;
class DELPHICLASS TCRConnection;
class DELPHICLASS TCRCommand;
class DELPHICLASS TCRTableInfo;
class DELPHICLASS TCRTablesInfo;
class DELPHICLASS TCRColumnInfo;
class DELPHICLASS TCRColumnsInfo;
struct TSQLObjectInfo;
class DELPHICLASS TSQLInfo;
class DELPHICLASS TCRFieldDesc;
class DELPHICLASS TCRRecordSet;
class DELPHICLASS TParamDesc;
class DELPHICLASS TParamDescs;
class DELPHICLASS TDAParamInfo;
class DELPHICLASS TDAParamsInfo;
class DELPHICLASS TCRConnections;
class DELPHICLASS TCRTransaction;
class DELPHICLASS TMTSTransaction;
class DELPHICLASS TDataHelper;
class DELPHICLASS TCRMetaData;
class DELPHICLASS TCRCursor;
class DELPHICLASS TCRLoaderColumn;
class DELPHICLASS TCRLoaderColumns;
class DELPHICLASS TCRLoader;
class DELPHICLASS TCRSimpleLoader;
class DELPHICLASS TCRAlerter;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TCRConnectionClass;

typedef System::TMetaClass* TCRTransactionClass;

typedef System::TMetaClass* TCRCommandClass;

typedef System::TMetaClass* TCRRecordSetClass;

typedef System::TMetaClass* TParamDescClass;

typedef System::TMetaClass* TTableInfoClass;

typedef System::TMetaClass* TSQLInfoClass;

typedef System::TMetaClass* TCRMetaDataClass;

typedef System::TMetaClass* TCRLoaderColumnClass;

typedef System::TMetaClass* TCRLoaderClass;

typedef System::TMetaClass* TCRAlerterClass;

enum DECLSPEC_DENUM TCommandType : unsigned char { ctUnknown, ctStatement, ctCursor };

enum DECLSPEC_DENUM TParsedSQLType : unsigned char { qtUnparsed, qtUnknown, qtSelect, qtCursor, qtInsert, qtUpdate, qtDelete, qtInsertReturning, qtUpdateReturning, qtExecuteBlock, qtCreate, qtDrop, qtSelectProc, qtSelectInto };

enum DECLSPEC_DENUM TCursorState : unsigned char { csInactive, csOpen, csParsed, csPrepared, csBound, csExecuteFetchAll, csExecuting, csExecuted, csFetching, csFetchingAll, csFetched };

enum DECLSPEC_DENUM TCRIsolationLevel : unsigned char { ilReadCommitted, ilReadUnCommitted, ilRepeatableRead, ilIsolated, ilSnapshot, ilCustom };

typedef void __fastcall (__closure *TErrorProc)(System::Sysutils::Exception* E, bool &Fail, bool &Reconnect, bool &Reexecute, int ReconnectAttempt, Memdata::TConnLostCause &ConnLostCause);

typedef void __fastcall (__closure *TReconnectProc)(void);

typedef void __fastcall (__closure *TConnectProc)(void);

typedef void __fastcall (__closure *TBoolProc)(bool Value);

typedef void __fastcall (__closure *TBeforeFetchProc)(bool &Cancel);

typedef void __fastcall (__closure *TAfterFetchProc)(void);

typedef void __fastcall (__closure *TDataChangeProc)(void);

typedef void __fastcall (__closure *TReadParamsProc)(void);

typedef System::UnicodeString __fastcall (__closure *TGetSQLProc)(void);

typedef System::UnicodeString __fastcall (__closure *TGetDBKeyListProc)(const System::UnicodeString TableName, const System::UnicodeString IndexName);

typedef void __fastcall (__closure *TFillExtFieldsInfoProc)(void);

typedef TCRConnection* __fastcall (__closure *TGetPooledConnection)(void);

typedef void __fastcall (__closure *TInfoMessageProc)(System::Sysutils::Exception* E);

#pragma pack(push,4)
class PASCALIMPLEMENTATION EFailOver : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	Memdata::TConnLostCause FConnLostCause;
	__fastcall EFailOver(Memdata::TConnLostCause ConnLostCause);
public:
	/* Exception.CreateFmt */ inline __fastcall EFailOver(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EFailOver(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EFailOver(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EFailOver(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EFailOver(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EFailOver(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EFailOver(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EFailOver(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EFailOver(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EFailOver(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EFailOver(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EFailOver() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TSmartFetchState : unsigned char { sfNone, sfMetaInfo, sfKeyOnly, sfDataByKey };

enum DECLSPEC_DENUM TFetchedStatus : unsigned char { fsNotFetched, fsFetched, fsFree };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TItemRefCount
{
public:
	bool Used;
	System::Byte Count;
};
#pragma pack(pop)


typedef TItemRefCount *PItemRefCount;

typedef System::DynamicArray<TItemRefCount> TItemRefCountArr;

#pragma pack(push,4)
class PASCALIMPLEMENTATION ESmartFetchError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESmartFetchError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESmartFetchError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESmartFetchError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESmartFetchError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESmartFetchError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESmartFetchError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESmartFetchError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESmartFetchError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESmartFetchError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESmartFetchError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESmartFetchError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESmartFetchError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESmartFetchError() { }
	
};

#pragma pack(pop)

typedef System::DynamicArray<TCRFieldDesc*> TFieldDescArray;

struct DECLSPEC_DRECORD TKeyAndDataFields
{
public:
	TFieldDescArray KeyFieldDescs;
	TFieldDescArray DataFieldDescs;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSmartFetchInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSQLGenerator* FSQLGenerator;
	
protected:
	bool FGeneratedSmartFetchByKeySQL;
	TDAParamsInfo* FParamsInfo;
	
public:
	TFieldDescArray KeyFieldDescs;
	System::UnicodeString SqlBeforeWhere;
	System::UnicodeString SqlAfterWhere;
	__fastcall TSmartFetchInfo(TSQLGenerator* SQLGenerator);
	__fastcall virtual ~TSmartFetchInfo();
	__property TDAParamsInfo* ParamsInfo = {read=FParamsInfo};
	__property TSQLGenerator* SQLGenerator = {read=FSQLGenerator};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLGenerator : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	virtual TSQLGenerator* __fastcall CloneGenerator() = 0 ;
	virtual System::UnicodeString __fastcall GenerateTableSQL(const System::UnicodeString TableName, const System::UnicodeString OrderFields) = 0 ;
	virtual System::UnicodeString __fastcall GenerateRecCountSQL(bool UseBaseSQL = false) = 0 ;
	virtual System::UnicodeString __fastcall GenerateSelectValues(const System::UnicodeString ValuesList) = 0 ;
	virtual System::UnicodeString __fastcall GenerateEmptyTableSQL(const System::UnicodeString TableName) = 0 ;
	virtual System::UnicodeString __fastcall GenerateSmartFetchMetaInfoSQL() = 0 ;
	virtual System::UnicodeString __fastcall GenerateSmartFetchKeyOnlySQL(const TFieldDescArray PrefetchedFields) = 0 ;
	virtual System::UnicodeString __fastcall GenerateSmartFetchDataByKeySQL(TSmartFetchInfo* SmartFetchInfo, TDAParamsInfo* ParamsInfo, Memdata::PItemHeader FirstItem, int RowCount) = 0 ;
	virtual void __fastcall PrepareSmartFetchDataByKeySQL(const System::UnicodeString Sql, TSmartFetchInfo* SmartFetchInfo) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TSQLGenerator() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TSQLGenerator() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRConnector : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TCRConnection* FOwner;
	System::UnicodeString FServerName;
	System::UnicodeString FServerVersion;
	int FServerMajorVersion;
	int FServerMinorVersion;
	
public:
	__fastcall virtual TCRConnector(TCRConnection* Owner);
	__classmethod virtual System::Sysutils::Exception* __fastcall CloneException(System::Sysutils::Exception* E);
	virtual void __fastcall Connect() = 0 ;
	virtual void __fastcall Disconnect() = 0 ;
	virtual void __fastcall SetDatabase(const System::UnicodeString Value) = 0 ;
	virtual void __fastcall Enlist(TMTSTransaction* Transaction);
	virtual void __fastcall UnEnlist(TMTSTransaction* Transaction);
	virtual System::UnicodeString __fastcall GetClientVersionFull();
	virtual System::UnicodeString __fastcall GetClientVersion() = 0 ;
	virtual int __fastcall GetClientMajorVersion() = 0 ;
	__property TCRConnection* Connection = {read=FOwner};
	__property System::UnicodeString ServerName = {read=FServerName};
	__property System::UnicodeString ServerVersion = {read=FServerVersion};
	__property int ServerMajorVersion = {read=FServerMajorVersion, nodefault};
	__property int ServerMinorVersion = {read=FServerMinorVersion, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCRConnector() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRConnection : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TErrorProc FOnError;
	TInfoMessageProc FOnInfoMessage;
	TReconnectProc FOnReconnectError;
	TReconnectProc FOnReconnectSuccess;
	unsigned FConnectionTime;
	TSQLInfo* FSQLinfo;
	Crdatatypemap::TCRMapRules* FDataTypeMap;
	TGetPooledConnection FGetPooledConnection;
	bool FAdditional;
	TCRCommand* FCommand;
	TCRRecordSet* FRecordSet;
	System::Syncobjs::TCriticalSection* FCommandLock;
	TSQLInfo* __fastcall GetSQLInfo();
	
protected:
	TCRTransaction* FInternalTransaction;
	bool FConnected;
	bool FNativeConnection;
	System::UnicodeString FUsername;
	System::UnicodeString FPassword;
	System::UnicodeString FServer;
	System::UnicodeString FDatabase;
	bool FAutoCommit;
	bool FConvertEOL;
	bool FIsValid;
	System::TObject* FPool;
	int FPoolVersion;
	System::TObject* FComponent;
	bool FDisconnectedMode;
	TCRIsolationLevel FIsolationLevel;
	bool FEnableBCD;
	bool FEnableFMTBCD;
	bool FUuidWithBraces;
	bool FInProcessError;
	bool FReconnected;
	Memdata::TSortType FDefaultSortType;
	Crvio::TCRIOHandler* FIOHandler;
	Crvio::THttpOptions* FHttpOptions;
	Crvio::TProxyOptions* FProxyOptions;
	Crvio::TSSLOptions* FSSLOptions;
	virtual TSQLInfo* __fastcall CreateSQLInfo();
	virtual Crdatatypemap::TCRMapRules* __fastcall CreateDataTypeMap();
	virtual void __fastcall Enlist(TMTSTransaction* MTSTransaction);
	virtual void __fastcall UnEnlist(TMTSTransaction* MTSTransaction);
	virtual TCRCommandClass __fastcall GetInternalCommandClass();
	virtual bool __fastcall CheckCommand(TCRCommand* Command);
	TCRCommand* __fastcall CreateCommand();
	virtual void __fastcall InitCommandProp(TCRCommand* Command);
	virtual bool __fastcall CheckRecordSet(TCRRecordSet* RecordSet);
	TCRRecordSet* __fastcall CreateRecordSet();
	virtual void __fastcall InitRecordSetProp(TCRRecordSet* RecordSet);
	
public:
	__fastcall virtual TCRConnection();
	__fastcall virtual ~TCRConnection();
	virtual TCRCommandClass __fastcall GetCommandClass() = 0 ;
	virtual TCRRecordSetClass __fastcall GetRecordSetClass() = 0 ;
	virtual TCRTransactionClass __fastcall GetTransactionClass() = 0 ;
	virtual TCRLoaderClass __fastcall GetLoaderClass() = 0 ;
	virtual TCRMetaDataClass __fastcall GetMetaDataClass() = 0 ;
	__classmethod virtual Crdatatypemap::TCRMapRulesClass __fastcall GetMapRulesClass();
	virtual Memdata::TDBObject* __fastcall CreateObject(System::Word DataType, TCRTransaction* Transaction);
	virtual void __fastcall DoError(System::Sysutils::Exception* E, bool &Fail);
	virtual void __fastcall Connect(const System::UnicodeString ConnectString);
	virtual void __fastcall Disconnect() = 0 ;
	virtual void __fastcall Ping() = 0 ;
	virtual void __fastcall Assign(TCRConnection* Source);
	virtual void __fastcall AssignConnect(TCRConnection* Source) = 0 ;
	TCRCommand* __fastcall GetCommand();
	void __fastcall ReleaseCommand(TCRCommand* &Command);
	void __fastcall ExecuteSQL(const System::UnicodeString SQL);
	TCRRecordSet* __fastcall GetRecordSet();
	void __fastcall ReleaseRecordSet(TCRRecordSet* &RecordSet);
	TCRRecordSet* __fastcall OpenRecordSet(const System::UnicodeString SQL);
	bool __fastcall GetConnected();
	void __fastcall SetConnected(bool Value);
	virtual TCRTransaction* __fastcall GetInternalTransaction();
	virtual void __fastcall SetIsolationLevel(const TCRIsolationLevel Value);
	virtual void __fastcall SetUsername(const System::UnicodeString Value);
	virtual void __fastcall SetPassword(const System::UnicodeString Value);
	virtual void __fastcall SetServer(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetUsername();
	System::UnicodeString __fastcall GetPassword();
	System::UnicodeString __fastcall GetServer();
	virtual bool __fastcall CheckIsValid() = 0 ;
	virtual void __fastcall ReturnToPool();
	virtual System::UnicodeString __fastcall GetServerVersion() = 0 ;
	virtual System::UnicodeString __fastcall GetServerVersionFull() = 0 ;
	virtual System::UnicodeString __fastcall GetClientVersion() = 0 ;
	virtual TCRConnector* __fastcall GetConnector();
	virtual bool __fastcall CanChangeDatabase();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, /* out */ System::Variant &Value);
	__property TErrorProc OnError = {read=FOnError, write=FOnError};
	__property TInfoMessageProc OnInfoMessage = {read=FOnInfoMessage, write=FOnInfoMessage};
	__property TReconnectProc OnReconnectError = {read=FOnReconnectError, write=FOnReconnectError};
	__property TReconnectProc OnReconnectSuccess = {read=FOnReconnectSuccess, write=FOnReconnectSuccess};
	__property bool AutoCommit = {read=FAutoCommit, write=FAutoCommit, nodefault};
	__property bool EnableBCD = {read=FEnableBCD, write=FEnableBCD, nodefault};
	__property bool EnableFMTBCD = {read=FEnableFMTBCD, write=FEnableFMTBCD, nodefault};
	__property System::UnicodeString Database = {read=FDatabase, write=FDatabase};
	__property unsigned ConnectionTime = {read=FConnectionTime, nodefault};
	__property bool IsValid = {read=FIsValid, write=FIsValid, nodefault};
	__property System::TObject* Pool = {read=FPool, write=FPool};
	__property int PoolVersion = {read=FPoolVersion, write=FPoolVersion, nodefault};
	__property System::TObject* Component = {read=FComponent, write=FComponent};
	__property bool DisconnectedMode = {read=FDisconnectedMode, write=FDisconnectedMode, nodefault};
	__property bool NativeConnection = {read=FNativeConnection, nodefault};
	__property bool UuidWithBraces = {read=FUuidWithBraces, write=FUuidWithBraces, nodefault};
	__property Crvio::TCRIOHandler* IOHandler = {read=FIOHandler, write=FIOHandler};
	__property Crdatatypemap::TCRMapRules* DataTypeMap = {read=FDataTypeMap};
	__property TSQLInfo* SQLInfo = {read=GetSQLInfo};
	__property bool Additional = {read=FAdditional, write=FAdditional, nodefault};
	__property TGetPooledConnection GetPooledConnection = {read=FGetPooledConnection, write=FGetPooledConnection};
};


class PASCALIMPLEMENTATION TCRCommand : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSQLInfo* FSQLInfo;
	TCRCommand* FBatchOwner;
	TCRCommand* FBatchCommand;
	TSQLInfo* __fastcall GetSQLInfo();
	
protected:
	System::TObject* FComponent;
	TCRConnection* FConnection;
	TCommandType FCommandType;
	System::UnicodeString FSQL;
	System::UnicodeString FUserSQL;
	TParsedSQLType FParsedSQLType;
	TParamDescs* FParams;
	TDAParamsInfo* FParamsInfo;
	bool FAutoCommit;
	TBoolProc FAfterExecute;
	bool FScanParams;
	bool FDisableParamScan;
	bool FQuoteNames;
	bool FTrimFixedChar;
	Memdata::TCompressBlobMode FCompressBlob;
	bool FEnableBCD;
	bool FEnableFMTBCD;
	TReadParamsProc FReadParams;
	int FBatchIters;
	int FBatchOffset;
	int FLastBatchIters;
	int FParamsProcessed;
	bool FExecuting;
	virtual TSQLInfo* __fastcall CreateSQLInfo();
	void __fastcall ParseSQLParam(Clrclasses::WideStringBuilder* ParsedSQL, Crparser::TSQLParser* Parser, TParamDescs* Params, System::WideChar LeftQuote, System::WideChar RightQuote, const System::UnicodeString RenamePrefix, int PrevCode = 0xffffffff);
	virtual void __fastcall CheckSQLParamType(Clrclasses::WideStringBuilder* ParsedSQL, Crparser::TSQLParser* Parser, TParamDesc* Param);
	void __fastcall AddParamPosition(const System::UnicodeString ParamName, int StartPosition, int EndPosition, TParamDesc* ParamRef);
	virtual bool __fastcall IsLabelSyntax(int Code, int PrevCode);
	virtual void __fastcall CreateBatchCommand();
	TCRCommand* __fastcall GetBatchCommand();
	virtual System::UnicodeString __fastcall GetBatchSQL(int Iters, int Offset);
	virtual int __fastcall GetBatchIters(int Iters);
	virtual bool __fastcall NeedBatchTransaction();
	virtual bool __fastcall NeedBatchSavepoint();
	virtual void __fastcall InternalExecuteBatch(int Iters, int Offset);
	__property TCRCommand* BatchOwner = {read=FBatchOwner};
	__property bool EnableBCD = {read=FEnableBCD, write=FEnableBCD, nodefault};
	__property bool EnableFMTBCD = {read=FEnableFMTBCD, write=FEnableFMTBCD, nodefault};
	
public:
	__fastcall virtual TCRCommand();
	__fastcall virtual ~TCRCommand();
	__classmethod virtual TTableInfoClass __fastcall GetTableInfoClass();
	__classmethod virtual TSQLInfoClass __fastcall GetSQLInfoClass();
	__classmethod virtual Crparser::TSQLParserClass __fastcall GetParserClass();
	__classmethod virtual TParamDescClass __fastcall GetParamDescClass();
	__classmethod virtual Crdatatypemap::TCRMapRulesClass __fastcall GetMapRulesClass();
	virtual void __fastcall Prepare();
	virtual void __fastcall Unprepare();
	virtual bool __fastcall GetPrepared() = 0 ;
	virtual void __fastcall Execute() = 0 ;
	virtual void __fastcall ExecuteBatch(int Iters, int Offset);
	virtual void __fastcall Close();
	virtual void __fastcall SetConnection(TCRConnection* Value);
	TCRConnection* __fastcall GetConnection();
	virtual void __fastcall SetTransaction(TCRTransaction* Value);
	virtual TCRTransaction* __fastcall GetTransaction();
	virtual TCursorState __fastcall GetCursorState() = 0 ;
	virtual void __fastcall SetCursorState(TCursorState Value) = 0 ;
	virtual void __fastcall SetSQL(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall ParseSQL(const System::UnicodeString SQL, TParamDescs* Params, const System::UnicodeString RenamePrefix = System::UnicodeString())/* overload */;
	void __fastcall ParseSQL()/* overload */;
	virtual void __fastcall ParseSQLType()/* overload */;
	virtual void __fastcall ParseSQLType(const System::UnicodeString SQL)/* overload */;
	virtual bool __fastcall IsValidBatchSQL();
	virtual System::UnicodeString __fastcall CreateProcCall(const System::UnicodeString Name, bool NeedDescribe, bool IsQuery) = 0 ;
	virtual bool __fastcall ForceCreateSPParams();
	__classmethod virtual bool __fastcall IsAllowedArrayType(System::Word DataType);
	void __fastcall ClearParams();
	virtual TParamDesc* __fastcall AddParam();
	virtual TCRCursor* __fastcall GetCursor();
	virtual void __fastcall SetCursor(TCRCursor* Value);
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, /* out */ System::Variant &Value);
	virtual void __fastcall BreakExec();
	__property bool Executing = {read=FExecuting, write=FExecuting, nodefault};
	__property TCommandType CommandType = {read=FCommandType, write=FCommandType, nodefault};
	__property System::UnicodeString SQL = {read=FSQL, write=SetSQL};
	__property System::TObject* Component = {read=FComponent, write=FComponent};
	__property TBoolProc AfterExecute = {read=FAfterExecute, write=FAfterExecute};
	__property TReadParamsProc ReadParams = {read=FReadParams, write=FReadParams};
	__property TParamDescs* Params = {read=FParams};
	__property TDAParamsInfo* ParamsInfo = {read=FParamsInfo, write=FParamsInfo};
	__property TSQLInfo* SQLInfo = {read=GetSQLInfo};
	__property TParsedSQLType ParsedSQLType = {read=FParsedSQLType, nodefault};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRTableInfo : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TCRTablesInfo* FOwner;
	int FIndex;
	System::UnicodeString FTableName;
	System::UnicodeString FTableAlias;
	bool FIsView;
	void __fastcall SetTableName(const System::UnicodeString Value);
	void __fastcall SetTableAlias(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall GetTableNameFull();
	virtual void __fastcall SetTableNameFull(const System::UnicodeString Value);
	
public:
	__fastcall virtual TCRTableInfo(TCRTablesInfo* Owner);
	__property System::UnicodeString TableName = {read=FTableName, write=SetTableName};
	__property System::UnicodeString TableAlias = {read=FTableAlias, write=SetTableAlias};
	__property System::UnicodeString TableNameFull = {read=GetTableNameFull, write=SetTableNameFull};
	__property bool IsView = {read=FIsView, write=FIsView, nodefault};
	__property int Index = {read=FIndex, write=FIndex, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCRTableInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRTablesInfo : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<TCRTableInfo*> _TCRTablesInfo__1;
	
	
public:
	TCRTableInfo* operator[](int Index) { return this->Items[Index]; }
	
private:
	TCRTableInfo* __fastcall GetItem(int Index);
	void __fastcall SetItem(int Index, TCRTableInfo* const Value);
	
protected:
	bool FCaseSensitive;
	_TCRTablesInfo__1 FList;
	int FUpdateCount;
	TTableInfoClass FTableInfoClass;
	System::Classes::TStringList* FTableNameList;
	System::Classes::TStringList* FTableAliasList;
	void __fastcall InternalAdd(TCRTableInfo* TableInfo);
	void __fastcall Changed();
	void __fastcall TableNameChanged();
	void __fastcall TableAliasChanged();
	int __fastcall GetCount();
	void __fastcall SetCaseSensitive(bool Value);
	
public:
	__fastcall TCRTablesInfo(TTableInfoClass TableInfoClass);
	__fastcall virtual ~TCRTablesInfo();
	TCRTableInfo* __fastcall Add();
	void __fastcall Clear();
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	TCRTableInfo* __fastcall FindByName(const System::UnicodeString TableName);
	int __fastcall IndexOf(TCRTableInfo* TableInfo);
	int __fastcall IndexByName(const System::UnicodeString TableName);
	int __fastcall IndexByAlias(const System::UnicodeString TableAlias);
	TCRTableInfo* __fastcall FindByNameAndAlias(const System::UnicodeString TableName, const System::UnicodeString TableAlias);
	__property TCRTableInfo* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property int Count = {read=GetCount, nodefault};
	__property TTableInfoClass TableInfoClass = {read=FTableInfoClass};
	__property bool CaseSensitive = {read=FCaseSensitive, write=SetCaseSensitive, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRColumnInfo : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString Name;
	System::UnicodeString Table;
	int TableIndex;
	System::UnicodeString Expr;
	System::UnicodeString Alias;
	bool Used;
	bool Described;
	bool Required;
public:
	/* TObject.Create */ inline __fastcall TCRColumnInfo() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCRColumnInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRColumnsInfo : public Crtypes::TCRObjectList
{
	typedef Crtypes::TCRObjectList inherited;
	
public:
	TCRColumnInfo* operator[](int Index) { return this->Items[Index]; }
	
private:
	TCRColumnInfo* __fastcall GetItem(int Index);
	
public:
	__property TCRColumnInfo* Items[int Index] = {read=GetItem/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TCRColumnsInfo() { }
	
public:
	/* TObject.Create */ inline __fastcall TCRColumnsInfo() : Crtypes::TCRObjectList() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TIdentCase : unsigned char { icUpper, icLower, icMixed, icMixedCaseSensitive };

struct DECLSPEC_DRECORD TSQLObjectInfo
{
public:
	System::UnicodeString Name;
	System::UnicodeString Schema;
	System::UnicodeString Catalog;
	System::UnicodeString DBLink;
	System::UnicodeString Synonym;
	System::Byte Flag;
};


typedef System::DynamicArray<TSQLObjectInfo> TSQLObjectsInfo;

typedef void __fastcall (__closure *TRequestFieldsInfoProc)(TSQLObjectsInfo Tables, TCRColumnsInfo* Columns);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Crparser::TSQLParserClass FParserClass;
	
protected:
	virtual void __fastcall ParseExtColumnName(Crparser::TSQLParser* Parser, int &Code, System::UnicodeString &Str);
	virtual bool __fastcall FirstCharQuotesNeed(System::WideChar Ch, TIdentCase IdCase);
	virtual bool __fastcall NextCharQuotesNeed(System::WideChar Ch, TIdentCase IdCase);
	virtual bool __fastcall HasAsLexem();
	virtual bool __fastcall HasOnlyLexem();
	virtual void __fastcall ParseExtTableInfo(Crparser::TSQLParser* Parser, int &CodeLexem, System::UnicodeString &StLex, System::UnicodeString &Name);
	
public:
	__fastcall virtual TSQLInfo(Crparser::TSQLParserClass ParserClass);
	virtual System::WideChar __fastcall LeftQuote();
	virtual System::WideChar __fastcall RightQuote();
	virtual TIdentCase __fastcall IdentCase();
	virtual bool __fastcall ParamQuoteAllowed();
	virtual System::WideChar __fastcall ProcedureOverloadSeparator();
	virtual bool __fastcall IsCursorSQLType(int Code);
	virtual bool __fastcall DetectReturningSQLType();
	System::UnicodeString __fastcall Quote(const System::UnicodeString Value)/* overload */;
	virtual System::UnicodeString __fastcall Quote(const System::UnicodeString Value, const System::WideChar LeftQ, const System::WideChar RightQ)/* overload */;
	virtual System::UnicodeString __fastcall UnQuote(const System::UnicodeString Value);
	virtual bool __fastcall IsQuoted(const System::UnicodeString Value);
	virtual bool __fastcall QuotesNeeded(const System::UnicodeString Value);
	System::UnicodeString __fastcall QuoteIfNeed(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall NormalizeName(const System::UnicodeString Value, bool QuoteNames = false, bool UnQuoteNames = false)/* overload */;
	virtual System::UnicodeString __fastcall NormalizeName(const System::UnicodeString Value, const System::WideChar LeftQ, const System::WideChar RightQ, bool QuoteNames = false, bool UnQuoteNames = false)/* overload */;
	System::UnicodeString __fastcall ToStringConst(const System::UnicodeString Value);
	virtual void __fastcall SplitObjectName(const System::UnicodeString Name, /* out */ TSQLObjectInfo &Info);
	virtual void __fastcall ParseTablesInfo(const System::UnicodeString SQL, TCRTablesInfo* TablesInfo);
	virtual void __fastcall ParseColumnsInfo(const System::UnicodeString SQL, TCRColumnsInfo* ColumnsInfo);
	virtual System::UnicodeString __fastcall NamesFromList(System::Classes::TStrings* List, bool NormalizedName = true, System::UnicodeString Delimiter = L";");
	virtual void __fastcall NamesToList(System::UnicodeString Value, System::Classes::TStrings* List, bool NormalizedName = true);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSQLInfo() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRFieldDesc : public Memdata::TFieldDesc
{
	typedef Memdata::TFieldDesc inherited;
	
	
private:
	typedef System::StaticArray<System::UnicodeString, 2> _TCRFieldDesc__1;
	
	
private:
	System::Word FDBType;
	int FDBLength;
	int FDBScale;
	bool FIsImplicityPrefetched;
	bool FIsPrefetched;
	Crdatatypemap::TCRMapRule* FMapRule;
	int FMapLength;
	int FMapScale;
	bool FMapHasValueLen;
	Crdatatypemap::TOnDemandConverter* FOnDemandConverter;
	Crencryption::TEncryptionMethod FEncryptor;
	Crencryption::TDecryptionMethod FDecryptor;
	int FEncryptState;
	
protected:
	bool FIsNational;
	TCRTableInfo* FTableInfo;
	_TCRFieldDesc__1 FActualNameQuoted;
	System::UnicodeString FDefaultExpr;
	virtual int __fastcall GetMapLength();
	virtual System::Word __fastcall GetMapDataType();
	virtual void __fastcall SetFieldReadOnly(bool SetFieldsReadOnly, TCRTableInfo* UpdatingTableInfo);
	
public:
	__fastcall virtual TCRFieldDesc(Memdata::TRecordSetClass RecordSetClass);
	virtual void __fastcall Assign(Memdata::TFieldDesc* FieldDesc);
	virtual System::UnicodeString __fastcall ActualNameQuoted(TSQLInfo* SQLInfo, bool QuoteNames);
	__property bool IsNational = {read=FIsNational, nodefault};
	__property TCRTableInfo* TableInfo = {read=FTableInfo, write=FTableInfo};
	__property System::UnicodeString DefaultExpr = {read=FDefaultExpr, write=FDefaultExpr};
	__property System::Word DBType = {read=FDBType, write=FDBType, nodefault};
	__property int DBLength = {read=FDBLength, write=FDBLength, nodefault};
	__property int DBScale = {read=FDBScale, write=FDBScale, nodefault};
	__property bool IsPrefetched = {read=FIsPrefetched, write=FIsPrefetched, nodefault};
	__property Crdatatypemap::TCRMapRule* MapRule = {read=FMapRule, write=FMapRule};
	__property int MapLength = {read=GetMapLength, write=FMapLength, nodefault};
	__property int MapScale = {read=FMapScale, write=FMapScale, nodefault};
	__property bool MapHasValueLen = {read=FMapHasValueLen, write=FMapHasValueLen, nodefault};
	__property Crdatatypemap::TOnDemandConverter* OnDemandConverter = {read=FOnDemandConverter, write=FOnDemandConverter};
	__property Crencryption::TEncryptionMethod Encryptor = {read=FEncryptor, write=FEncryptor};
	__property Crencryption::TDecryptionMethod Decryptor = {read=FDecryptor, write=FDecryptor};
	__property int EncryptState = {read=FEncryptState, write=FEncryptState, nodefault};
public:
	/* TFieldDesc.Destroy */ inline __fastcall virtual ~TCRFieldDesc() { }
	
};


enum DECLSPEC_DENUM TFieldOrigins : unsigned char { foNone, foField, foTableAndField, foTableAliasAndField };

class PASCALIMPLEMENTATION TCRRecordSet : public Memdata::TMemData
{
	typedef Memdata::TMemData inherited;
	
	
private:
	typedef System::StaticArray<TFieldDescArray, 2> _TCRRecordSet__1;
	
	typedef System::StaticArray<TFieldDescArray, 2> _TCRRecordSet__2;
	
	
protected:
	TCRCommand* FCommand;
	TSQLGenerator* FSQLGenerator;
	Memdata::TFieldDescs* FTmpFields;
	bool FUniDirectional;
	int FFetchRows;
	bool FFetchAll;
	bool FLockFetchAll;
	TConnectProc FOnConnectRequest;
	TConnectProc FOnDisconnectRequest;
	bool FNoCountData;
	bool FLongStrings;
	bool FFlatBuffers;
	bool FExtendedFieldsInfo;
	bool FDefaultValues;
	TFieldOrigins FFieldOrigins;
	bool FReadOnly;
	bool FFullRefresh;
	bool FSetFieldsReadOnly;
	System::UnicodeString FKeyFields;
	System::UnicodeString FUpdatingTable;
	void *FFetchBuffer;
	int FFetchBufferSize;
	TBoolProc FAfterExecFetch;
	TBoolProc FAfterFetchAll;
	TBeforeFetchProc FOnBeforeFetch;
	TAfterFetchProc FOnAfterFetch;
	TDataChangeProc FOnDataChanged;
	TDataChangeProc FOnReopen;
	bool FWaitForFetchBreak;
	int FLastRowsObtained;
	bool FInsertAllSetFields;
	bool FForceInitFieldsOnFetch;
	int FChangedIndicatorOffset;
	bool FInFetching;
	int FEncryptStateOffset;
	int FFetchStateOffset;
	int FRefCountOffset;
	int FItemPtrOffset;
	Memdata::TConvertInfo FConvertInfo;
	Crdatatypemap::TCRMapRules* FDataTypeMap;
	Crencryption::TCREncryptor* FEncryptor;
	System::UnicodeString FEncryptedFields;
	System::UnicodeString FOriginalSQL;
	int FOriginalParamsCount;
	TSmartFetchState FSmartFetchState;
	TGetSQLProc FGetKeyValuesSQL;
	System::UnicodeString FPrefetchedFields;
	bool FNotFetchPrefetchedFields;
	bool FLiveBlockOnSmartFetch;
	bool FDataFieldsWasSynchronized;
	TSmartFetchInfo* FSmartFetchInfo;
	bool FIsFetchingDataByKey;
	Memdata::TBlockHeader *FSmartFetchBlock;
	TItemRefCountArr FItemRefCounts;
	int FLastItemRefInd;
	Memdata::TItemHeader *FFirstFetchedItem;
	TCRTablesInfo* FTablesInfo;
	bool FUpdTableIsArtificial;
	int FUpdatingTableInfoIdx;
	TCRFieldDesc* FIdentityField;
	bool FIdentityIsPartOfComplexPK;
	TCRFieldDesc* FKeyGeneratorField;
	TGetDBKeyListProc FGetDBKeyList;
	TFillExtFieldsInfoProc FFillExtFieldsInfo;
	bool FExtFieldsInfoInited;
	_TCRRecordSet__1 FCachedKeyFieldDescs;
	System::StaticArray<bool, 2> FKeyFieldDescsIsCached;
	_TCRRecordSet__2 FCachedDataFieldDescs;
	System::StaticArray<bool, 2> FDataFieldDescsIsCached;
	virtual void __fastcall CreateCommand() = 0 ;
	void __fastcall FreeCommand();
	virtual void __fastcall SetCommand(TCRCommand* Value);
	virtual void __fastcall SetTrimFixedChar(bool Value);
	virtual void __fastcall InitRecordSize();
	int __fastcall GetChangedIndicatorSize();
	Crdatatypemap::TCRMapRules* __fastcall CreateDataTypeMap();
	void __fastcall DecryptBuffer(Memdata::PItemHeader Item);
	bool __fastcall FieldIsEncrypted(const System::UnicodeString FieldName);
	System::UnicodeString __fastcall GetDataTypeName(TCRFieldDesc* Field);
	int __fastcall GetEncryptStateSize();
	virtual bool __fastcall ExtFieldsInfoIsInternal();
	void __fastcall ClearCachedKeyFieldDescs();
	virtual bool __fastcall CanUseAllKeyFields();
	virtual bool __fastcall IdentityFieldIsData();
	void __fastcall FillFieldDescs(/* out */ TFieldDescArray &FieldDescs, const System::UnicodeString FieldNames, bool CheckFields);
	virtual void __fastcall FillKeyFieldDescs(/* out */ TFieldDescArray &KeyFieldDescs, bool ForceUseAllKeyFields);
	virtual void __fastcall FillDataFieldDescs(/* out */ TFieldDescArray &DataFieldDescs, bool ForceUseAllKeyFields);
	virtual int __fastcall FindTableInfoBySimpleName(const System::UnicodeString Name);
	virtual void __fastcall ApplyColumnsInfo(TCRColumnsInfo* Columns, bool ReadFieldsFromServer, int DefaultTable, int AsteriskCount);
	virtual void __fastcall RequestFieldsInfo(TSQLObjectsInfo Tables, TCRColumnsInfo* Columns);
	virtual bool __fastcall NeedInitFieldsOnPrepare();
	virtual void __fastcall InternalPrepare();
	virtual void __fastcall InternalUnPrepare();
	virtual void __fastcall InternalOpen(bool DisableInitFields = false);
	virtual void __fastcall InternalClose();
	virtual void __fastcall AllocFetchBuffer();
	virtual void __fastcall FreeFetchBuffer();
	virtual void __fastcall FetchBlock(Memdata::PBlockHeader Block, bool FetchBack, /* out */ int &RowsObtained);
	virtual void __fastcall ProcessFetchedBlock(Memdata::PBlockHeader Block, bool FetchBack);
	virtual void __fastcall ProcessNoResult(bool FetchBack);
	virtual bool __fastcall ProcessFetchedException(System::Sysutils::Exception* E);
	virtual bool __fastcall FetchingAccessible(bool FetchBack);
	virtual bool __fastcall InternalFetch(bool FetchBack = false);
	virtual void __fastcall InitBlock(Memdata::PBlockHeader Block);
	void __fastcall ClearBlock(Memdata::PBlockHeader Block);
	void __fastcall CreateBlockStruct(Memdata::PBlockHeader Block, int RowsObtained, bool FetchBack = false, bool StandAloneBlock = false);
	virtual bool __fastcall CanFetchBack();
	virtual void __fastcall InitFetchedBlock(Memdata::PBlockHeader Block, int RowsObtained, bool FetchBack);
	virtual void __fastcall DoBeforeFetch(/* out */ bool &Cancel);
	virtual void __fastcall DoAfterFetch();
	virtual bool __fastcall NeedInitFieldsOnFetch();
	virtual bool __fastcall NeedUnPrepareAfterFetch();
	virtual bool __fastcall RequiredReInitFields();
	virtual void __fastcall InitItem(Memdata::PItemHeader Item);
	virtual void __fastcall DeleteItem(Memdata::PItemHeader Item);
	int __fastcall GetFetchStateSize();
	int __fastcall GetRefCountSize();
	int __fastcall GetItemPtrSize();
	bool __fastcall IsFieldPrefetched(Memdata::TFieldDesc* Field);
	virtual void __fastcall CheckFetched(void * RecBuf, Memdata::TFieldDesc* Field);
	void __fastcall CheckIfFetchedItem(Memdata::PItemHeader &CheckedItem, bool &AnyRecWasDeleted);
	virtual void __fastcall SyncKeyFields(Memdata::TFieldDescs* KeyFields, Memdata::TFieldDescs* OriginFields);
	virtual void __fastcall SyncDataFields(Memdata::TFieldDescs* DataFields, Memdata::TFieldDescs* OriginFields);
	void __fastcall ClearDataByKeyParams();
	void __fastcall SetSmartFetchSQL(Memdata::PItemHeader FirstFetchingItem = (Memdata::PItemHeader)(0x0), int RowCount = 0x0);
	void __fastcall FetchDataByKey(Memdata::PItemHeader FirstFetchingItem, int RowCount);
	void __fastcall SyncBlocks(Memdata::PItemHeader FirstFetchingItem, int RowCount);
	TFetchedStatus __fastcall GetFetchedStatus(void * RecBuf);
	void __fastcall SetFetchedStatus(void * RecBuf, TFetchedStatus Value);
	int __fastcall GetRefCountInd(void * RecBuf);
	void __fastcall WriteRefCountInd(void * RecBuf, int RefInd);
	int __fastcall GetNewRefCountInd();
	void __fastcall DisposeRefCountInd(int RefInd);
	Memdata::PItemHeader __fastcall GetItemFromRecBuf(void * RecBuf);
	void __fastcall WriteItemPtrIntoRecBuf(void * RecBuf, Memdata::PItemHeader Item);
	virtual void __fastcall FreeData();
	TTableInfoClass __fastcall GetTableInfoClass();
	TCRTableInfo* __fastcall GetUpdatingTableInfo();
	virtual void __fastcall CheckIndexFields();
	virtual void __fastcall SetSortDefaults(Memdata::TSortColumn* SortColumn);
	System::TObject* __fastcall GetComponent();
	void __fastcall SetComponent(System::TObject* Value);
	
public:
	__fastcall virtual TCRRecordSet();
	__fastcall virtual ~TCRRecordSet();
	virtual bool __fastcall IsFullReopen();
	virtual void __fastcall Reopen();
	virtual void __fastcall Prepare();
	virtual void __fastcall UnPrepare();
	virtual void __fastcall Disconnect();
	virtual void __fastcall ExecCommand(int Iters = 0x1, int Offset = 0x0);
	void __fastcall CloseCommand();
	virtual Memdata::TFieldDescClass __fastcall GetFieldDescType();
	void __fastcall CheckFieldDescs();
	void __fastcall InitExtFieldsInfo();
	void __fastcall GetExtFieldsInfo(TRequestFieldsInfoProc RequestFieldsInfo);
	virtual void __fastcall ClearFields();
	virtual void __fastcall InternalInitFieldDescs();
	bool __fastcall IsEqualDataTypes(Memdata::TFieldDesc* Field, Crdatatypemap::TCRMapRule* MapRule)/* overload */;
	virtual void __fastcall SetNull(Memdata::TFieldDesc* Field, void * RecBuf, bool Value);
	virtual bool __fastcall GetNull(Memdata::TFieldDesc* Field, void * RecBuf);
	Memdata::TSharedObject* __fastcall GetFieldObject(Memdata::TFieldDesc* Field, void * RecBuf);
	virtual void * __fastcall GetMappedDataBuf(Memdata::TFieldDesc* FieldDesc, void * DataBuf, System::Word &DataLen, System::Word &DataType, bool &HasParent, bool &IsFixed);
	void * __fastcall GetBufForDataMappingConverter(void * Source, System::Word DataType, bool HasParent, bool NeedConvert);
	void __fastcall PutBufAfterDataMappingConverter(void * Source, void * Dest, System::Word DataType, bool HasParent);
	virtual void __fastcall GetFieldData(Memdata::TFieldDesc* Field, void * DataBuf, System::Word &DataLen, void * Dest, bool NeedConvert);
	virtual void __fastcall PutFieldData(Memdata::TFieldDesc* Field, void * DataBuf, PWORD DataLenPtr, void * ValuePtr, System::Word ValueLen, bool NeedConvert, bool IsDatabaseValue = false);
	void __fastcall InitUpdatingTableIdx();
	void __fastcall ClearCachedFieldDescs();
	void __fastcall GetKeyFieldDescs(/* out */ TFieldDescArray &KeyFieldDescs, bool ForceUseAllFields = false);
	void __fastcall GetDataFieldDescs(/* out */ TFieldDescArray &DataFieldDescs, bool ForceUseAllFields = false);
	void __fastcall GetKeyAndDataFields(/* out */ TKeyAndDataFields &KeyAndDataFields, bool ForceUseAllFields);
	virtual void __fastcall DetectIdentityField();
	virtual void __fastcall DetectKeyGeneratorField();
	void __fastcall ClearKeyGeneratorField();
	void __fastcall SetFieldsReadOnly();
	void __fastcall SetIdentityField(TCRFieldDesc* Value);
	virtual void __fastcall GetNextRecord(void * RecBuf);
	virtual void __fastcall GetPriorRecord(void * RecBuf);
	virtual void __fastcall DeleteRecord();
	virtual bool __fastcall Fetch(bool FetchBack = false);
	virtual void __fastcall ExecFetch(bool DisableInitFields);
	virtual void __fastcall FetchAll();
	virtual void __fastcall BreakFetch();
	virtual void __fastcall WaitForFetch();
	virtual bool __fastcall CanDisconnect();
	virtual bool __fastcall RowsReturn();
	TCRCommand* __fastcall GetCommand();
	TCRConnection* __fastcall GetConnection();
	virtual void __fastcall SetConnection(TCRConnection* Value);
	virtual void __fastcall SetTransaction(TCRTransaction* Value);
	virtual void __fastcall SetSQL(const System::UnicodeString Value);
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, /* out */ System::Variant &Value);
	virtual bool __fastcall IsCaseSensitive();
	virtual void __fastcall FilterUpdated();
	virtual bool __fastcall GetChanged(Memdata::TFieldDesc* Field, void * RecBuf);
	virtual void __fastcall SetChanged(Memdata::TFieldDesc* Field, void * RecBuf, bool Value);
	void __fastcall ClearChangedIndicators(void * RecBuf);
	Crdatatypemap::TCRMapRule* __fastcall GetMapRule(TCRFieldDesc* Field)/* overload */;
	Crdatatypemap::TCRMapRule* __fastcall GetMapRule(const System::UnicodeString FieldName, System::Word DBType, int DBLength, int DBScale)/* overload */;
	Crdatatypemap::TFetchConverter* __fastcall GetMapFetchConverter(System::Word DBType, int DBLength, int DBScale)/* overload */;
	Crdatatypemap::TFetchConverter* __fastcall GetMapFetchConverter(const System::UnicodeString FieldName, System::Word DBType, int DBLength, int DBScale)/* overload */;
	virtual Crdatatypemap::TOnDemandConverter* __fastcall GetMapOnDemandConverter(TCRFieldDesc* Field, /* out */ Crdatatypemap::TCRMapRule* &MapRule)/* overload */;
	Crdatatypemap::TOnDemandConverter* __fastcall GetMapOnDemandConverter(System::Word DataType, System::Word DBType, int DBLength, int DBScale, /* out */ Crdatatypemap::TCRMapRule* &MapRule)/* overload */;
	virtual Crdatatypemap::TOnDemandConverter* __fastcall GetMapOnDemandConverter(const System::UnicodeString FieldName, System::Word DataType, System::Word DBType, int DBLength, int DBScale, /* out */ Crdatatypemap::TCRMapRule* &MapRule)/* overload */;
	bool __fastcall GetEncrypted(Memdata::TFieldDesc* Field, void * RecBuf);
	void __fastcall SetEncrypted(Memdata::TFieldDesc* Field, void * RecBuf, bool Value);
	virtual bool __fastcall IsEncryptableDataType(System::Word DataType);
	virtual System::Word __fastcall GetDecryptDataType(System::Word DataType);
	virtual void __fastcall InitRecord(void * RecBuf);
	virtual void __fastcall PutRecord(void * RecBuf);
	virtual void __fastcall AddRef(void * RecBuf);
	virtual void __fastcall ReleaseRef(void * RecBuf, bool IsResync, bool WithBlob);
	__property int FetchRows = {read=FFetchRows, nodefault};
	__property Crdatatypemap::TCRMapRules* DataTypeMap = {read=FDataTypeMap};
	__property Crencryption::TCREncryptor* Encryptor = {read=FEncryptor, write=FEncryptor};
	__property bool ForceInitFieldsOnFetch = {read=FForceInitFieldsOnFetch, write=FForceInitFieldsOnFetch, nodefault};
	__property TSmartFetchState SmartFetchState = {read=FSmartFetchState, write=FSmartFetchState, nodefault};
	__property System::UnicodeString OriginalSQL = {read=FOriginalSQL};
	__property TGetSQLProc GetKeyValuesSQL = {read=FGetKeyValuesSQL, write=FGetKeyValuesSQL};
	__property bool NotFetchPrefetchedFields = {read=FNotFetchPrefetchedFields, write=FNotFetchPrefetchedFields, nodefault};
	virtual bool __fastcall NeedConvertEOL();
	__property TBoolProc AfterExecFetch = {read=FAfterExecFetch, write=FAfterExecFetch};
	__property TBoolProc AfterFetchAll = {read=FAfterFetchAll, write=FAfterFetchAll};
	__property TBeforeFetchProc OnBeforeFetch = {read=FOnBeforeFetch, write=FOnBeforeFetch};
	__property TAfterFetchProc OnAfterFetch = {read=FOnAfterFetch, write=FOnAfterFetch};
	__property TDataChangeProc OnDataChanged = {read=FOnDataChanged, write=FOnDataChanged};
	__property TDataChangeProc OnReopen = {read=FOnReopen, write=FOnReopen};
	virtual void __fastcall SortItems();
	__property TCRTablesInfo* TablesInfo = {read=FTablesInfo};
	__property TCRFieldDesc* IdentityField = {read=FIdentityField};
	__property TCRFieldDesc* KeyGeneratorField = {read=FKeyGeneratorField};
	__property int UpdatingTableInfoIdx = {read=FUpdatingTableInfoIdx, nodefault};
	__property TCRTableInfo* UpdatingTableInfo = {read=GetUpdatingTableInfo};
	__property bool UpdTableIsArtificial = {read=FUpdTableIsArtificial, nodefault};
	__property TGetDBKeyListProc GetDBKeyList = {read=FGetDBKeyList, write=FGetDBKeyList};
	__property TFillExtFieldsInfoProc FillExtFieldsInfo = {read=FFillExtFieldsInfo, write=FFillExtFieldsInfo};
	__property TSQLGenerator* SQLGenerator = {read=FSQLGenerator, write=FSQLGenerator};
	__property TConnectProc OnConnectRequest = {read=FOnConnectRequest, write=FOnConnectRequest};
	__property TConnectProc OnDisconnectRequest = {read=FOnDisconnectRequest, write=FOnDisconnectRequest};
	__property System::TObject* Component = {read=GetComponent, write=SetComponent};
};


enum DECLSPEC_DENUM TParamDirection : unsigned char { pdUnknown, pdInput, pdOutput, pdInputOutput, pdResult };

class PASCALIMPLEMENTATION TParamDesc : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TCRCommand* FCommand;
	System::UnicodeString FName;
	System::Word FDataType;
	System::Word FSubDataType;
	TParamDirection FParamType;
	int FSize;
	int FPrecision;
	int FScale;
	System::Variant FData;
	bool FIsNull;
	bool FIsBound;
	bool FConvertEOL;
	bool FNational;
	Memdata::TSharedObject* FParamObject;
	int FArraySize;
	Crencryption::TEncryptionMethod FEncryptor;
	Memdata::TBlob* FEncryptedBlob;
	System::Variant __fastcall EncryptValue(System::Word DataType, const System::Variant &Value);
	Memdata::TSharedObject* __fastcall EncryptObject(System::Word DataType, Memdata::TSharedObject* const Value);
	virtual void __fastcall AllocBuffer();
	virtual void __fastcall FreeBuffer();
	void __fastcall CheckIndex(int Index);
	virtual bool __fastcall GetItemNull(int Index);
	virtual void __fastcall SetItemNull(int Index, bool Value);
	virtual Memdata::TSharedObject* __fastcall GetItemObject(int Index);
	virtual void __fastcall SetItemObject(int Index, Memdata::TSharedObject* Value);
	virtual System::Variant __fastcall GetItemValue(int Index);
	virtual void __fastcall SetItemValue(int Index, const System::Variant &Value);
	
public:
	__fastcall virtual TParamDesc();
	__fastcall virtual ~TParamDesc();
	virtual void __fastcall Assign(TParamDesc* Source);
	virtual void __fastcall Clear();
	virtual int __fastcall GetMinDefaultSize();
	virtual int __fastcall GetMaxStringSize(TCRConnection* Connection);
	System::UnicodeString __fastcall GetName();
	void __fastcall SetName(const System::UnicodeString Value);
	System::Word __fastcall GetDataType();
	virtual void __fastcall SetDataType(System::Word Value);
	System::Word __fastcall GetSubDataType();
	virtual void __fastcall SetSubDataType(System::Word Value);
	TParamDirection __fastcall GetParamType();
	virtual void __fastcall SetParamType(TParamDirection Value);
	int __fastcall GetArraySize();
	virtual void __fastcall SetArraySize(int Value);
	int __fastcall GetSize();
	virtual void __fastcall SetSize(int Value);
	int __fastcall GetPrecision();
	void __fastcall SetPrecision(int Value);
	int __fastcall GetScale();
	void __fastcall SetScale(int Value);
	virtual System::Variant __fastcall GetValue();
	virtual void __fastcall SetValue(const System::Variant &Value);
	virtual void __fastcall SetValueArr(Crtypes::PVariantArray PValueArr);
	virtual void __fastcall SetObjectArr(Crtypes::PVariantArray PValueArr);
	virtual System::PVariant __fastcall GetItemPtr(int Index);
	virtual bool __fastcall IsObjectValue();
	virtual Memdata::TSharedObject* __fastcall GetObject();
	virtual void __fastcall SetObject(Memdata::TSharedObject* Value);
	virtual bool __fastcall GetNull();
	virtual void __fastcall SetNull(const bool Value);
	bool __fastcall GetIsBound();
	virtual void __fastcall SetIsBound(bool Value);
	virtual bool __fastcall GetNational();
	virtual void __fastcall SetNational(bool Value);
	void __fastcall SetConvertEOL(const bool Value);
	void __fastcall SetEncryptor(const Crencryption::TEncryptionMethod Value);
	__classmethod virtual bool __fastcall NeedAssignScalarValues();
	__classmethod virtual bool __fastcall NeedAssignObjectValues();
	__property System::Variant Value = {read=FData, write=SetValue};
	__property bool ItemNull[int Index] = {read=GetItemNull, write=SetItemNull};
	__property Memdata::TSharedObject* ItemObject[int Index] = {read=GetItemObject, write=SetItemObject};
	__property System::Variant ItemValue[int Index] = {read=GetItemValue, write=SetItemValue};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TParamDescs : public Crtypes::TCRObjectList
{
	typedef Crtypes::TCRObjectList inherited;
	
public:
	TParamDesc* operator[](int Index) { return this->Items[Index]; }
	
private:
	TParamDesc* __fastcall GetItems(int Index);
	
public:
	TParamDesc* __fastcall FindParam(const System::UnicodeString Name);
	TParamDesc* __fastcall ParamByName(const System::UnicodeString Name);
	__property TParamDesc* Items[int Index] = {read=GetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TParamDescs() { }
	
public:
	/* TObject.Create */ inline __fastcall TParamDescs() : Crtypes::TCRObjectList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAParamInfo : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	TCRFieldDesc* FieldDesc;
	bool Old;
	System::UnicodeString ParamName;
	TParamDirection ParamType;
	TParamDesc* ParamRef;
	Clrclasses::WideStringBuilder* SB;
	int StartPosition;
	int EndPosition;
public:
	/* TCollectionItem.Create */ inline __fastcall virtual TDAParamInfo(System::Classes::TCollection* Collection) : System::Classes::TCollectionItem(Collection) { }
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TDAParamInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAParamsInfo : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TDAParamInfo* operator[](int Index) { return this->Items[Index]; }
	
protected:
	HIDESBASE TDAParamInfo* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TDAParamInfo* Value);
	
public:
	__property TDAParamInfo* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TDAParamsInfo(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TDAParamsInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRConnections : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TCRConnection* operator[](int Index) { return this->Items[Index]; }
	
private:
	TCRConnection* __fastcall GetItems(int Index);
	
public:
	__property TCRConnection* Items[int Index] = {read=GetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TCRConnections() { }
	
public:
	/* TObject.Create */ inline __fastcall TCRConnections() : System::Classes::TList() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TCRErrorProc)(System::Sysutils::Exception* E, bool &Fail);

enum DECLSPEC_DENUM TCRTransactionAction : unsigned char { taCommit, taRollback };

class PASCALIMPLEMENTATION TCRTransaction : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Syncobjs::TCriticalSection* FLock;
	
protected:
	bool FActive;
	System::TObject* FComponent;
	TCRErrorProc FOnError;
	TCRConnections* FConnections;
	TCRIsolationLevel FIsolationLevel;
	bool FReadOnly;
	bool FNativeTransaction;
	void __fastcall Lock();
	void __fastcall Unlock();
	
public:
	__fastcall virtual TCRTransaction();
	__fastcall virtual ~TCRTransaction();
	void __fastcall CheckInactive();
	void __fastcall CheckActive();
	virtual bool __fastcall AddConnection(TCRConnection* Connection);
	virtual bool __fastcall RemoveConnection(TCRConnection* Connection);
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	virtual bool __fastcall GetInTransaction();
	virtual bool __fastcall DetectInTransaction(bool CanActivate);
	virtual void __fastcall AssignConnect(TCRTransaction* Source);
	virtual void __fastcall StartTransaction();
	virtual void __fastcall Commit();
	virtual void __fastcall Rollback();
	virtual __int64 __fastcall GetMultiTransactionID();
	virtual void __fastcall CommitRetaining();
	virtual void __fastcall RollbackRetaining();
	virtual void __fastcall Savepoint(const System::UnicodeString Name);
	virtual void __fastcall ReleaseSavepoint(const System::UnicodeString Name);
	virtual void __fastcall RollbackToSavepoint(const System::UnicodeString Name);
	virtual void __fastcall Reset();
	virtual bool __fastcall CanRestoreAfterFailover();
	__property System::TObject* Component = {read=FComponent, write=FComponent};
	__property TCRErrorProc OnError = {read=FOnError, write=FOnError};
};


class PASCALIMPLEMENTATION TMTSTransaction : public TCRTransaction
{
	typedef TCRTransaction inherited;
	
private:
	Mtscall::_di_ICRTransactionDispenserSC FMTSGC;
	Mtscall::_di_ICRTransactionSC FMTSTrans;
	
protected:
	virtual void __fastcall StartMTSTransaction();
	virtual void __fastcall CompleteMTSTransaction(bool Commit);
	virtual void __fastcall EnlistLink(TCRConnection* Connection);
	virtual void __fastcall UnEnlistLink(TCRConnection* Connection);
	
public:
	__fastcall virtual ~TMTSTransaction();
	virtual bool __fastcall AddConnection(TCRConnection* Connection);
	virtual bool __fastcall RemoveConnection(TCRConnection* Connection);
	virtual void __fastcall StartTransaction();
	virtual void __fastcall Commit();
	virtual void __fastcall Rollback();
	virtual void __fastcall Savepoint(const System::UnicodeString Name);
	virtual void __fastcall ReleaseSavepoint(const System::UnicodeString Name);
	virtual void __fastcall RollbackToSavepoint(const System::UnicodeString Name);
	__property Mtscall::_di_ICRTransactionSC MTSTransaction = {read=FMTSTrans};
public:
	/* TCRTransaction.Create */ inline __fastcall virtual TMTSTransaction() : TCRTransaction() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataHelper : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Memdata::TData* FData;
	void *FRecBuf;
	void __fastcall FreeBuffer();
	System::Variant __fastcall GetFieldValue(int Index);
	void __fastcall SetFieldValue(int Index, const System::Variant &Value);
	
public:
	__fastcall TDataHelper(Memdata::TData* Data);
	__fastcall virtual ~TDataHelper();
	void __fastcall AllocBuffer();
	void __fastcall InitRecord();
	void __fastcall AppendRecord();
	bool __fastcall NextRecord();
	__property System::Variant FieldValues[int Index] = {read=GetFieldValue, write=SetFieldValue};
};

#pragma pack(pop)

typedef System::DynamicArray<bool> TBooleanArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRMetaData : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	Memdata::TMemData* FMemData;
	TDataHelper* FMemDataHelper;
	TCRRecordSet* FRecordSet;
	TDataHelper* FRecordSetHelper;
	System::UnicodeString FOperator;
	virtual TCRRecordSet* __fastcall CreateRecordSet() = 0 ;
	void __fastcall AddField(const System::UnicodeString AName, int AType, int ALength = 0xffffffff);
	void __fastcall CopyRecord(const int *SourceIndices, const int SourceIndices_High, const int *DestIndices, const int DestIndices_High);
	TBooleanArray __fastcall ParseTypes(const System::UnicodeString ObjectTypes, System::UnicodeString *AllTypes, const int AllTypes_High)/* overload */;
	void __fastcall ParseTypes(const System::UnicodeString ObjectTypes, System::Classes::TStringList* TypesList)/* overload */;
	void __fastcall AddWhere(System::UnicodeString &WhereClause, const System::UnicodeString Name, const System::UnicodeString Value, bool AddEmpty = false);
	virtual Memdata::TData* __fastcall InternalGetMetaData(const System::UnicodeString MetaDataKind, System::Classes::TStrings* Restrictions)/* overload */;
	virtual void __fastcall CreateMetaDataKindsFields();
	virtual void __fastcall InternalGetMetaDataKindsList(System::Classes::TStringList* List);
	virtual Memdata::TData* __fastcall GetMetaDataKinds();
	virtual void __fastcall CreateRestrictionsFields();
	virtual void __fastcall InternalGetRestrictionsList(System::Classes::TStringList* List, const System::UnicodeString MetaDataKind);
	virtual Memdata::TData* __fastcall GetRestrictions(System::Classes::TStrings* Restrictions);
	virtual void __fastcall CreateTablesFields();
	virtual Memdata::TData* __fastcall GetTables(System::Classes::TStrings* Restrictions) = 0 ;
	virtual void __fastcall CreateColumnsFields();
	virtual Memdata::TData* __fastcall GetColumns(System::Classes::TStrings* Restrictions) = 0 ;
	virtual void __fastcall CreateProceduresFields();
	virtual Memdata::TData* __fastcall GetProcedures(System::Classes::TStrings* Restrictions) = 0 ;
	virtual void __fastcall CreateProcedureParametersFields();
	virtual Memdata::TData* __fastcall GetProcedureParameters(System::Classes::TStrings* Restrictions) = 0 ;
	virtual void __fastcall CreateIndexesFields();
	virtual Memdata::TData* __fastcall GetIndexes(System::Classes::TStrings* Restrictions) = 0 ;
	virtual void __fastcall CreateIndexColumnsFields();
	virtual Memdata::TData* __fastcall GetIndexColumns(System::Classes::TStrings* Restrictions) = 0 ;
	virtual void __fastcall CreateConstraintsFields();
	virtual Memdata::TData* __fastcall GetConstraints(System::Classes::TStrings* Restrictions) = 0 ;
	virtual void __fastcall CreateConstraintColumnsFields();
	virtual Memdata::TData* __fastcall GetConstraintColumns(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetDatabases(System::Classes::TStrings* Restrictions);
	virtual void __fastcall CreateDatabasesFields();
	virtual Memdata::TData* __fastcall GetDataTypes(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetUsers(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetRoles(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetUDTs(System::Classes::TStrings* Restrictions);
	virtual Memdata::TData* __fastcall GetPackages(System::Classes::TStrings* Restrictions);
	
public:
	__fastcall virtual TCRMetaData();
	__fastcall virtual ~TCRMetaData();
	virtual Memdata::TData* __fastcall GetMetaData(TCRConnection* Connection, TCRTransaction* Transaction, const System::UnicodeString MetaDataKind, System::Classes::TStrings* Restrictions);
	void __fastcall GetMetaDataKindsList(System::Classes::TStrings* List);
	void __fastcall GetRestrictionsList(System::Classes::TStrings* List, const System::UnicodeString MetaDataKind);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRCursor : public Memdata::TSharedObject
{
	typedef Memdata::TSharedObject inherited;
	
public:
	virtual bool __fastcall CanFetch() = 0 ;
public:
	/* TSharedObject.Create */ inline __fastcall TCRCursor() : Memdata::TSharedObject() { }
	/* TSharedObject.Destroy */ inline __fastcall virtual ~TCRCursor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRLoaderColumn : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FActualFieldNo;
	System::UnicodeString FName;
	System::Word FDataType;
	System::Word FSubDataType;
	int FSize;
	int FDataSize;
	int FPrecision;
	int FScale;
	
public:
	__fastcall virtual TCRLoaderColumn();
	virtual void __fastcall UpdateDataType(System::Word Value);
	__property int ActualFieldNo = {read=FActualFieldNo, write=FActualFieldNo, nodefault};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::Word DataType = {read=FDataType, write=FDataType, nodefault};
	__property System::Word SubDataType = {read=FSubDataType, write=FSubDataType, nodefault};
	__property int Size = {read=FSize, write=FSize, nodefault};
	__property int DataSize = {read=FDataSize, write=FDataSize, nodefault};
	__property int Precision = {read=FPrecision, write=FPrecision, nodefault};
	__property int Scale = {read=FScale, write=FScale, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCRLoaderColumn() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRLoaderColumns : public Crtypes::TCRObjectList
{
	typedef Crtypes::TCRObjectList inherited;
	
public:
	TCRLoaderColumn* operator[](int Index) { return this->Items[Index]; }
	
private:
	TCRLoaderColumn* __fastcall GetColumn(int Index);
	void __fastcall SetColumn(int Index, TCRLoaderColumn* Value);
	
public:
	int __fastcall GetColumnIndexByName(const System::UnicodeString Name);
	TCRLoaderColumn* __fastcall FindColumnByName(const System::UnicodeString Name);
	TCRLoaderColumn* __fastcall GetColumnByName(const System::UnicodeString Name);
	__property TCRLoaderColumn* Items[int Index] = {read=GetColumn, write=SetColumn/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TCRLoaderColumns() { }
	
public:
	/* TObject.Create */ inline __fastcall TCRLoaderColumns() : Crtypes::TCRObjectList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRLoader : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TCRConnection* FConnection;
	TCRTransaction* FTransaction;
	System::UnicodeString FTableName;
	TCRLoaderColumns* FColumns;
	int FLastRow;
	int FLoadedRows;
	bool FSkipReadOnlyFieldDescs;
	bool FObjectReleaseNeeded;
	bool FQuoteNames;
	bool FUseBlankValues;
	virtual void __fastcall SetConnection(TCRConnection* Value);
	virtual void __fastcall SetTransaction(TCRTransaction* Value);
	void __fastcall CheckTableName();
	virtual void __fastcall FillColumn(TCRLoaderColumn* Column, Memdata::TFieldDesc* FieldDesc);
	virtual void __fastcall DoPrepare();
	virtual void __fastcall DoPutColumnData(int Col, int Row, const System::Variant &Value);
	void __fastcall DoAddObjectRef(const System::Variant &Value);
	void __fastcall DoReleaseObjectRef(const System::Variant &Value);
	virtual bool __fastcall IsPutColumnDataAllowed();
	
public:
	__fastcall virtual TCRLoader();
	__fastcall virtual ~TCRLoader();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	__classmethod virtual TCRLoaderColumnClass __fastcall GetColumnClass();
	virtual void __fastcall Prepare()/* overload */;
	virtual void __fastcall Prepare(int RecordCount)/* overload */;
	virtual void __fastcall Reset();
	virtual void __fastcall DoLoad();
	virtual void __fastcall Finish();
	virtual void __fastcall PutColumnData(int Col, int Row, const System::Variant &Value);
	void __fastcall CreateColumns();
	void __fastcall CheckColumnsInfo();
	void __fastcall DiscardRow();
	__property TCRConnection* Connection = {read=FConnection, write=SetConnection};
	__property TCRTransaction* Transaction = {read=FTransaction, write=SetTransaction};
	__property System::UnicodeString TableName = {read=FTableName, write=FTableName};
	__property TCRLoaderColumns* Columns = {read=FColumns};
	__property bool QuoteNames = {read=FQuoteNames, write=FQuoteNames, nodefault};
	__property bool UseBlankValues = {read=FUseBlankValues, write=FUseBlankValues, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRSimpleLoader : public TCRLoader
{
	typedef TCRLoader inherited;
	
protected:
	TCRCommand* FCommand;
	virtual void __fastcall CreateCommand() = 0 ;
	virtual void __fastcall DoLoadRow();
	virtual void __fastcall DoPrepare();
	virtual void __fastcall DoPutColumnData(int Col, int Row, const System::Variant &Value);
	void __fastcall DoAfterLoadRow();
	virtual bool __fastcall IsPutColumnDataAllowed();
	
public:
	__fastcall virtual TCRSimpleLoader();
	__fastcall virtual ~TCRSimpleLoader();
	virtual void __fastcall Reset();
	virtual void __fastcall PutColumnData(int Col, int Row, const System::Variant &Value);
	virtual void __fastcall DoLoad();
	virtual void __fastcall Finish();
};

#pragma pack(pop)

typedef void __fastcall (__closure *TCRAlerterEventCallback)(const System::UnicodeString EventName, const System::UnicodeString Message);

typedef void __fastcall (__closure *TCRAlerterErrorCallback)(System::Sysutils::Exception* E);

class PASCALIMPLEMENTATION TCRAlerter : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TCRConnection* FConnection;
	TCRTransaction* FTransaction;
	System::Classes::TStrings* FEventNames;
	bool FActive;
	TCRAlerterEventCallback FOnEvent;
	TCRAlerterErrorCallback FOnError;
	virtual void __fastcall SetConnection(TCRConnection* Value);
	virtual void __fastcall SetTransaction(TCRTransaction* Value);
	
public:
	__fastcall virtual TCRAlerter();
	__fastcall virtual ~TCRAlerter();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	virtual void __fastcall SendEvent(const System::UnicodeString EventName, const System::UnicodeString Message) = 0 ;
	virtual void __fastcall Start() = 0 ;
	virtual void __fastcall Stop() = 0 ;
	virtual bool __fastcall IsActive();
	__property TCRConnection* Connection = {read=FConnection, write=SetConnection};
	__property TCRTransaction* Transaction = {read=FTransaction, write=SetTransaction};
	__property System::Classes::TStrings* EventNames = {read=FEventNames};
	__property TCRAlerterEventCallback OnEvent = {read=FOnEvent, write=FOnEvent};
	__property TCRAlerterErrorCallback OnError = {read=FOnError, write=FOnError};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TSQLInfo* DefaultSQLInfo;
extern DELPHI_PACKAGE System::WideChar MacroChar;
extern DELPHI_PACKAGE System::UnicodeString __fastcall GenerateTableName(Memdata::TFieldDesc* const FieldDesc);
}	/* namespace Craccess */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRACCESS)
using namespace Craccess;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CraccessHPP
