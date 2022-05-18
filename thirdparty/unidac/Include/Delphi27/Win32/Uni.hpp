// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Uni.pas' rev: 34.00 (Windows)

#ifndef UniHPP
#define UniHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <Data.DB.hpp>
#include <System.StrUtils.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>
#include <CRVio.hpp>
#include <CRAccess.hpp>
#include <CRConnectionPool.hpp>
#include <CRConnectionString.hpp>
#include <CREncryption.hpp>
#include <System.Generics.Collections.hpp>
#include <MemData.hpp>
#include <MemDS.hpp>
#include <DBAccess.hpp>
#include <UniConsts.hpp>
#include <UniProvider.hpp>
#include <UniConnectionString.hpp>

//-- user supplied -----------------------------------------------------------
#ifdef SetPort
#undef SetPort
#endif
#ifdef GetObject
#undef GetObject
#endif

namespace Uni
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EUniError;
class DELPHICLASS TSpecificOptionsList;
class DELPHICLASS TSpecificOptionsHolder;
class DELPHICLASS TUniConnectionOptions;
class DELPHICLASS TUniConnectionSpecificOptions;
class DELPHICLASS TUniMacro;
class DELPHICLASS TUniMacros;
class DELPHICLASS TUniConnection;
class DELPHICLASS TUniTransactionSpecificOptions;
class DELPHICLASS TUniTransaction;
class DELPHICLASS TUniEncryptor;
class DELPHICLASS TUniSQL;
class DELPHICLASS TUniEncryption;
class DELPHICLASS TUniDataSetOptions;
class DELPHICLASS TUniDataSetSpecificOptions;
class DELPHICLASS TCustomUniDataSet;
class DELPHICLASS TUniQuery;
class DELPHICLASS TCustomUniTable;
class DELPHICLASS TUniTable;
class DELPHICLASS TUniStoredProc;
class DELPHICLASS TUniUpdateSQL;
class DELPHICLASS TUniDataSource;
class DELPHICLASS TUniMetaData;
class DELPHICLASS TUniBlob;
class DELPHICLASS TUniCursor;
class DELPHICLASS TUniParam;
class DELPHICLASS TUniParams;
class DELPHICLASS TUniUtils;
//-- type declarations -------------------------------------------------------
typedef Uniprovider::TUniProvider* __fastcall (__closure *TGetUniProviderFunc)(void);

#pragma pack(push,4)
class PASCALIMPLEMENTATION EUniError : public Dbaccess::EDAError
{
	typedef Dbaccess::EDAError inherited;
	
private:
	Dbaccess::EDAError* FInnerError;
	
public:
	__fastcall EUniError(Dbaccess::EDAError* AInnerError);
	__fastcall virtual ~EUniError();
	virtual bool __fastcall IsFatalError();
	virtual bool __fastcall IsKeyViolation();
	__property Dbaccess::EDAError* InnerError = {read=FInnerError};
public:
	/* Exception.CreateFmt */ inline __fastcall EUniError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Dbaccess::EDAError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EUniError(NativeUInt Ident)/* overload */ : Dbaccess::EDAError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EUniError(System::PResStringRec ResStringRec)/* overload */ : Dbaccess::EDAError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EUniError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Dbaccess::EDAError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EUniError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Dbaccess::EDAError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EUniError(const System::UnicodeString Msg, int AHelpContext) : Dbaccess::EDAError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EUniError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Dbaccess::EDAError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EUniError(NativeUInt Ident, int AHelpContext)/* overload */ : Dbaccess::EDAError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EUniError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Dbaccess::EDAError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EUniError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Dbaccess::EDAError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EUniError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Dbaccess::EDAError(Ident, Args, Args_High, AHelpContext) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TSpecificOptionsList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	System::Classes::TComponent* FOwner;
	TGetUniProviderFunc FGetProviderFunc;
	Uniprovider::TUniProvider* __fastcall GetProvider();
	HIDESBASE System::UnicodeString __fastcall GetValue(const System::UnicodeString Name);
	HIDESBASE void __fastcall SetValue(const System::UnicodeString Name, const System::UnicodeString Value);
	System::UnicodeString __fastcall ValidateOption(const System::UnicodeString FullName, const System::UnicodeString Value)/* overload */;
	System::UnicodeString __fastcall ValidateOption(const System::UnicodeString Prefix, const System::UnicodeString Name, const System::UnicodeString Value)/* overload */;
	System::UnicodeString __fastcall ValidateOption(const System::UnicodeString FullName)/* overload */;
	System::UnicodeString __fastcall ResolveDeprecated(const System::UnicodeString FullName)/* overload */;
	System::UnicodeString __fastcall ResolveDeprecated(const System::UnicodeString Prefix, const System::UnicodeString Name)/* overload */;
	void __fastcall SplitOptionName(const System::UnicodeString FullName, System::UnicodeString &Prefix, System::UnicodeString &Name);
	
protected:
	HIDESBASE void __fastcall ReadData(System::Classes::TReader* Reader);
	HIDESBASE void __fastcall WriteData(System::Classes::TWriter* Writer);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall Put(int Index, const System::UnicodeString S);
	virtual void __fastcall InsertItem(int Index, const System::UnicodeString S, System::TObject* AObject);
	
public:
	__fastcall TSpecificOptionsList(System::Classes::TComponent* AOwner, TGetUniProviderFunc GetUniProviderFunc);
	virtual int __fastcall IndexOfName(const System::UnicodeString Name);
	__property System::UnicodeString Values[const System::UnicodeString Name] = {read=GetValue, write=SetValue};
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TSpecificOptionsList() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpecificOptionsHolder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSpecificOptionsList* FValues;
	bool FIsModified;
	
protected:
	virtual void __fastcall ValuesChanging(System::TObject* Sender);
	virtual void __fastcall ValuesChanged(System::TObject* Sender);
	
public:
	__fastcall TSpecificOptionsHolder(System::Classes::TComponent* AOwner, TGetUniProviderFunc GetUniProviderFunc);
	__fastcall virtual ~TSpecificOptionsHolder();
	__property TSpecificOptionsList* Values = {read=FValues};
	__property bool IsModified = {read=FIsModified, write=FIsModified, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniConnectionOptions : public Dbaccess::TDAConnectionOptions
{
	typedef Dbaccess::TDAConnectionOptions inherited;
	
private:
	bool __fastcall GetConvertEOL();
	void __fastcall SetConvertEOL(bool Value);
	
__published:
	__property bool ConvertEOL = {read=GetConvertEOL, write=SetConvertEOL, default=0};
	__property DisconnectedMode = {default=0};
	__property KeepDesignConnected = {default=1};
	__property LocalFailover = {default=0};
	__property DefaultSortType = {default=0};
	__property EnableBCD = {default=0};
	__property EnableFMTBCD = {default=0};
public:
	/* TDAConnectionOptions.Create */ inline __fastcall TUniConnectionOptions(Dbaccess::TCustomDAConnection* Owner) : Dbaccess::TDAConnectionOptions(Owner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TUniConnectionOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniConnectionSpecificOptions : public TSpecificOptionsHolder
{
	typedef TSpecificOptionsHolder inherited;
	
protected:
	virtual void __fastcall ValuesChanging(System::TObject* Sender);
public:
	/* TSpecificOptionsHolder.Create */ inline __fastcall TUniConnectionSpecificOptions(System::Classes::TComponent* AOwner, TGetUniProviderFunc GetUniProviderFunc) : TSpecificOptionsHolder(AOwner, GetUniProviderFunc) { }
	/* TSpecificOptionsHolder.Destroy */ inline __fastcall virtual ~TUniConnectionSpecificOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniMacro : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FValue;
	System::UnicodeString FCondition;
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall SetValue(const System::UnicodeString Value);
	void __fastcall SetCondition(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property System::UnicodeString Value = {read=FValue, write=SetValue};
	__property System::UnicodeString Condition = {read=FCondition, write=SetCondition};
public:
	/* TCollectionItem.Create */ inline __fastcall virtual TUniMacro(System::Classes::TCollection* Collection) : System::Classes::TCollectionItem(Collection) { }
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TUniMacro() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniMacros : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TUniMacro* operator[](int Index) { return this->Items[Index]; }
	
private:
	HIDESBASE TUniMacro* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TUniMacro* Value);
	void __fastcall NotifyOwner();
	
protected:
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TUniMacros(System::Classes::TPersistent* Owner);
	HIDESBASE void __fastcall Add(const System::UnicodeString Name, const System::UnicodeString Value, const System::UnicodeString Condition = System::UnicodeString());
	TUniMacro* __fastcall FindMacro(const System::UnicodeString Name);
	TUniMacro* __fastcall MacroByName(const System::UnicodeString Name);
	__property TUniMacro* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TUniMacros() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TUniConnection : public Dbaccess::TCustomDAConnection
{
	typedef Dbaccess::TCustomDAConnection inherited;
	
private:
	Dbaccess::TDATransaction* FInternalTransaction;
	System::UnicodeString FProviderName;
	Uniprovider::TUniProvider* FProvider;
	int FPort;
	System::UnicodeString FDatabase;
	TUniConnectionSpecificOptions* FSpecificOptions;
	TUniMacros* FMacros;
	Uniprovider::TUniSqlFormatter* FSQLFormatter;
	System::Classes::TStringList* FMacroNames;
	System::Classes::TStringList* FMacroValues;
	bool FMacrosChanged;
	int FMacrosVersion;
	System::UnicodeString __fastcall GetProviderName();
	void __fastcall SetProviderName(System::UnicodeString Value);
	void __fastcall SetPort(int Value);
	void __fastcall SetDatabase(const System::UnicodeString Value);
	TSpecificOptionsList* __fastcall GetSpecificOptions();
	void __fastcall SetSpecificOptions(TSpecificOptionsList* Value);
	HIDESBASE TUniTransaction* __fastcall GetTransaction(int Index);
	TUniConnectionOptions* __fastcall GetOptions();
	HIDESBASE void __fastcall SetOptions(TUniConnectionOptions* Value);
	void __fastcall SetMacros(TUniMacros* Value);
	bool __fastcall IsMacrosStored();
	HIDESBASE TUniTransaction* __fastcall GetDefaultTransaction();
	HIDESBASE void __fastcall SetDefaultTransaction(TUniTransaction* const Value);
	TUniSQL* __fastcall GetSQL();
	
protected:
	void __fastcall CheckProvider();
	bool __fastcall CanGetProvider();
	Uniprovider::TUniProvider* __fastcall GetProvider();
	virtual Craccess::TCRConnectionClass __fastcall GetIConnectionClass();
	virtual Craccess::TCRCommandClass __fastcall GetICommandClass();
	virtual Craccess::TCRRecordSetClass __fastcall GetIRecordSetClass();
	virtual Craccess::TCRMetaDataClass __fastcall GetIMetaDataClass();
	virtual Craccess::TCRTransactionClass __fastcall GetITransactionClass();
	virtual void __fastcall SetConnectionParameters(Crconnectionpool::TCRConnectionParameters* ConnectionParameters);
	virtual void __fastcall SetBaseConnectionProps(Craccess::TCRConnection* Connection);
	virtual void __fastcall SetConnectionProps(Craccess::TCRConnection* Connection);
	virtual Crconnectionpool::TCRConnectionParametersClass __fastcall GetConnectionParametersClass();
	virtual Crconnectionpool::TCRConnectionPoolManagerClass __fastcall GetConnectionPoolingManagerClass();
	virtual void __fastcall CreateIConnection();
	virtual void __fastcall SetIConnection(Craccess::TCRConnection* Value);
	virtual void __fastcall DoConnect();
	virtual void __fastcall DoDisconnect();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual System::TClass __fastcall SQLMonitorClass();
	virtual Dbaccess::TConnectDialogClass __fastcall ConnectDialogClass();
	virtual Dbaccess::TDAConnectionOptions* __fastcall CreateOptions();
	virtual Crconnectionstring::TCRConnectionStringBuilder* __fastcall CreateConnectionStringBuilder();
	virtual bool __fastcall IsMultipleTransactionsSupported();
	virtual Dbaccess::TDATransaction* __fastcall UsedTransaction();
	virtual bool __fastcall GetInTransaction();
	TUniTransaction* __fastcall GetInternalDefTransaction();
	virtual void __fastcall DoError(System::Sysutils::Exception* E, bool &Fail, bool &Reconnect, bool &Reexecute, int ReconnectAttempt, Memdata::TConnLostCause &ConnLostCause);
	virtual void __fastcall AssignConnectOptions(Dbaccess::TCustomDAConnection* Source);
	void __fastcall CheckSqlFormatter();
	void __fastcall DetectActiveMacros();
	void __fastcall ExpandMacros(System::UnicodeString &SQL);
	virtual System::UnicodeString __fastcall DefaultTableSchema();
	System::UnicodeString __fastcall GetServerVersion();
	System::UnicodeString __fastcall GetServerVersionFull();
	System::UnicodeString __fastcall GetClientVersion();
	virtual System::Variant __fastcall GetConnectionStringParam(int ParamCode);
	virtual void __fastcall SetConnectionStringParam(int ParamCode, const System::Variant &ParamValue);
	
public:
	__fastcall virtual TUniConnection(System::Classes::TComponent* AOwner)/* overload */;
	__fastcall virtual ~TUniConnection();
	virtual System::Variant __fastcall ExecProc(const System::UnicodeString Name, const System::Variant *Params, const int Params_High);
	virtual System::Variant __fastcall ExecProcEx(const System::UnicodeString Name, const System::Variant *Params, const int Params_High);
	virtual Dbaccess::TCustomDADataSet* __fastcall CreateDataSet(System::Classes::TComponent* AOwner = (System::Classes::TComponent*)(0x0));
	virtual Dbaccess::TCustomDASQL* __fastcall CreateSQL();
	virtual Dbaccess::TDATransaction* __fastcall CreateTransaction();
	virtual Dbaccess::TDAMetaData* __fastcall CreateMetaData();
	HIDESBASE void __fastcall AssignConnect(TUniConnection* Source);
	HIDESBASE TUniParam* __fastcall ParamByName(const System::UnicodeString Name);
	TUniMacro* __fastcall MacroByName(const System::UnicodeString Name);
	System::Variant __fastcall ActiveMacroValueByName(const System::UnicodeString Name);
	HIDESBASE void __fastcall EncryptTable(const System::UnicodeString TableName, TUniEncryptor* Encryptor, const System::UnicodeString Fields);
	virtual void __fastcall StartTransaction()/* overload */;
	HIDESBASE void __fastcall StartTransaction(Craccess::TCRIsolationLevel IsolationLevel, bool ReadOnly = false)/* overload */;
	void __fastcall CommitRetaining();
	void __fastcall RollbackRetaining();
	void __fastcall Savepoint(const System::UnicodeString Name);
	void __fastcall RollbackToSavepoint(const System::UnicodeString Name);
	void __fastcall ReleaseSavepoint(const System::UnicodeString Name);
	__property TransactionCount;
	__property TUniTransaction* Transactions[int Index] = {read=GetTransaction};
	__property System::UnicodeString ServerVersion = {read=GetServerVersion};
	__property System::UnicodeString ServerVersionFull = {read=GetServerVersionFull};
	__property System::UnicodeString ClientVersion = {read=GetClientVersion};
	__property TUniSQL* SQL = {read=GetSQL};
	
__published:
	__property AutoCommit = {default=1};
	__property DataTypeMap;
	__property System::UnicodeString ProviderName = {read=GetProviderName, write=SetProviderName};
	__property int Port = {read=FPort, write=SetPort, default=0};
	__property System::UnicodeString Database = {read=FDatabase, write=SetDatabase};
	__property TSpecificOptionsList* SpecificOptions = {read=GetSpecificOptions, write=SetSpecificOptions};
	__property TUniConnectionOptions* Options = {read=GetOptions, write=SetOptions};
	__property TUniMacros* Macros = {read=FMacros, write=SetMacros, stored=IsMacrosStored};
	__property TUniTransaction* DefaultTransaction = {read=GetDefaultTransaction, write=SetDefaultTransaction};
	__property IOHandler;
	__property PoolingOptions;
	__property Pooling = {default=0};
	__property Debug = {default=0};
	__property Username = {default=0};
	__property Password = {default=0};
	__property Server = {default=0};
	__property Connected = {stored=IsConnectedStored, default=0};
	__property ConnectDialog;
	__property LoginPrompt = {default=1};
	__property ConnectString = {default=0};
	__property AfterConnect;
	__property BeforeConnect;
	__property AfterDisconnect;
	__property BeforeDisconnect;
	__property OnLogin;
	__property OnError;
	__property OnConnectionLost;
public:
	/* TCustomDAConnection.Create */ inline __fastcall TUniConnection(System::Classes::TComponent* Owner, const System::UnicodeString ConnectString)/* overload */ : Dbaccess::TCustomDAConnection(Owner, ConnectString) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniTransactionSpecificOptions : public TSpecificOptionsHolder
{
	typedef TSpecificOptionsHolder inherited;
	
protected:
	virtual void __fastcall ValuesChanged(System::TObject* Sender);
	virtual void __fastcall ValuesChanging(System::TObject* Sender);
public:
	/* TSpecificOptionsHolder.Create */ inline __fastcall TUniTransactionSpecificOptions(System::Classes::TComponent* AOwner, TGetUniProviderFunc GetUniProviderFunc) : TSpecificOptionsHolder(AOwner, GetUniProviderFunc) { }
	/* TSpecificOptionsHolder.Destroy */ inline __fastcall virtual ~TUniTransactionSpecificOptions() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TUniTransaction : public Dbaccess::TDATransaction
{
	typedef Dbaccess::TDATransaction inherited;
	
private:
	TUniTransactionSpecificOptions* FSpecificOptions;
	TUniConnection* __fastcall GetDefaultConnection();
	HIDESBASE void __fastcall SetDefaultConnection(TUniConnection* Value);
	HIDESBASE TUniConnection* __fastcall GetConnection(int Index);
	TSpecificOptionsList* __fastcall GetSpecificOptions();
	void __fastcall SetSpecificOptions(TSpecificOptionsList* const Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	Uniprovider::TUniProvider* __fastcall GetProvider();
	virtual Craccess::TCRTransactionClass __fastcall GetITransactionClass();
	virtual void __fastcall SetITransaction(Craccess::TCRTransaction* Value);
	virtual System::TClass __fastcall SQLMonitorClass();
	virtual void __fastcall CheckActive();
	virtual void __fastcall CheckInactive();
	virtual void __fastcall DoSavepoint(const System::UnicodeString Name);
	virtual void __fastcall DoReleaseSavepoint(const System::UnicodeString Name);
	virtual void __fastcall DoRollbackToSavepoint(const System::UnicodeString Name);
	
public:
	__fastcall virtual TUniTransaction(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TUniTransaction();
	virtual void __fastcall Commit();
	virtual void __fastcall Rollback();
	void __fastcall CommitRetaining();
	void __fastcall RollbackRetaining();
	void __fastcall Savepoint(System::UnicodeString Name);
	void __fastcall ReleaseSavePoint(System::UnicodeString Name);
	void __fastcall RollbackToSavepoint(System::UnicodeString Name);
	void __fastcall AddConnection(TUniConnection* Connection);
	void __fastcall RemoveConnection(TUniConnection* Connection);
	__property TUniConnection* Connections[int Index] = {read=GetConnection};
	__property ConnectionsCount;
	
__published:
	__property TUniConnection* DefaultConnection = {read=GetDefaultConnection, write=SetDefaultConnection, stored=IsInternalTrStored};
	__property TSpecificOptionsList* SpecificOptions = {read=GetSpecificOptions, write=SetSpecificOptions};
	__property TransactionType = {default=0};
	__property IsolationLevel = {default=0};
	__property ReadOnly = {default=0};
	__property DefaultCloseAction = {default=1};
	__property OnError;
	__property OnStart;
	__property OnCommit;
	__property OnRollback;
	__property OnCommitRetaining;
	__property OnRollbackRetaining;
};


class PASCALIMPLEMENTATION TUniEncryptor : public Crencryption::TCREncryptor
{
	typedef Crencryption::TCREncryptor inherited;
	
public:
	/* TCREncryptor.Create */ inline __fastcall virtual TUniEncryptor(System::Classes::TComponent* AOwner) : Crencryption::TCREncryptor(AOwner) { }
	/* TCREncryptor.Destroy */ inline __fastcall virtual ~TUniEncryptor() { }
	
};


class PASCALIMPLEMENTATION TUniSQL : public Dbaccess::TCustomDASQL
{
	typedef Dbaccess::TCustomDASQL inherited;
	
private:
	TSpecificOptionsHolder* FSpecificOptions;
	Dbaccess::TDATransaction* FFixedUsedTransaction;
	bool FWriteAllParams;
	System::Classes::TList* FParamRefs;
	bool FLockParamRefsReset;
	int FMacrosVersion;
	bool FEnableUniSQL;
	void __fastcall ResetParamRefs();
	TUniParams* __fastcall GetParams();
	HIDESBASE void __fastcall SetParams(TUniParams* Value);
	TUniConnection* __fastcall GetConnection();
	HIDESBASE void __fastcall SetConnection(TUniConnection* Value);
	TSpecificOptionsList* __fastcall GetSpecificOptions();
	void __fastcall SetSpecificOptions(TSpecificOptionsList* Value);
	TUniTransaction* __fastcall GetUniTransaction();
	void __fastcall SetUniTransaction(TUniTransaction* Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall SetICommand(Craccess::TCRCommand* Value);
	virtual Dbaccess::TDAFieldTypeMapClass __fastcall GetFieldTypeMapClass();
	Uniprovider::TUniProvider* __fastcall FindProvider();
	Uniprovider::TUniProvider* __fastcall GetProvider();
	virtual Dbaccess::TDAParams* __fastcall CreateParamsObject();
	virtual Dbaccess::TDATransaction* __fastcall UsedTransaction();
	virtual void __fastcall BeginConnection(bool NoConnectCheck = true);
	virtual void __fastcall EndConnection();
	virtual Dbaccess::TDATransaction* __fastcall GetTransaction();
	virtual void __fastcall InternalPrepare();
	virtual void __fastcall InternalUnPrepare();
	virtual void __fastcall InternalExecute(int Iters, int Offset);
	virtual System::UnicodeString __fastcall GetFinalSQL();
	void __fastcall CheckUniMacros();
	virtual bool __fastcall NeedRecreateProcCall();
	virtual bool __fastcall IsInOutParamSupported();
	virtual System::UnicodeString __fastcall ParseSQL(const System::UnicodeString SQL, Dbaccess::TDAParams* Params);
	virtual void __fastcall AssembleSQL();
	virtual void __fastcall CreateParams()/* overload */;
	virtual void __fastcall WriteParams(bool WriteValue = true);
	virtual void __fastcall ReadParams();
	virtual Dbaccess::TDAParam* __fastcall FindResultParam();
	virtual void __fastcall InternalCreateProcCall(const System::UnicodeString Name, bool NeedDescribe, bool IsQuery = false);
	virtual void __fastcall AssignParamDesc(Dbaccess::TDAParam* Param, Craccess::TParamDesc* ParamDesc);
	virtual void __fastcall AssignParamValue(Craccess::TParamDesc* ParamDesc, Dbaccess::TDAParam* Param);
	
public:
	__fastcall virtual TUniSQL(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TUniSQL();
	HIDESBASE TUniParam* __fastcall FindParam(const System::UnicodeString Value);
	HIDESBASE TUniParam* __fastcall ParamByName(const System::UnicodeString Value);
	void __fastcall CreateProcCall(const System::UnicodeString Name);
	__property __int64 LastInsertId = {read=FLastInsertId};
	
__published:
	__property TUniConnection* Connection = {read=GetConnection, write=SetConnection};
	__property TUniTransaction* Transaction = {read=GetUniTransaction, write=SetUniTransaction, stored=IsTransactionStored};
	__property TUniParams* Params = {read=GetParams, write=SetParams, stored=false};
	__property TSpecificOptionsList* SpecificOptions = {read=GetSpecificOptions, write=SetSpecificOptions};
	__property ParamCheck = {default=1};
	__property SQL;
	__property Macros;
	__property Debug = {default=0};
	__property BeforeExecute;
	__property AfterExecute;
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  CreateParams(Dbaccess::TDAParams* Params, Craccess::TParamDescs* ParamDescs){ Dbaccess::TCustomDASQL::CreateParams(Params, ParamDescs); }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniEncryption : public Dbaccess::TDAEncryption
{
	typedef Dbaccess::TDAEncryption inherited;
	
private:
	TUniEncryptor* __fastcall GetEncryptor();
	HIDESBASE void __fastcall SetEncryptor(TUniEncryptor* Value);
	
__published:
	__property TUniEncryptor* Encryptor = {read=GetEncryptor, write=SetEncryptor};
public:
	/* TDAEncryption.Create */ inline __fastcall TUniEncryption(Dbaccess::TCustomDADataSet* Owner) : Dbaccess::TDAEncryption(Owner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TUniEncryption() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniDataSetOptions : public Dbaccess::TDADataSetOptions
{
	typedef Dbaccess::TDADataSetOptions inherited;
	
public:
	__fastcall TUniDataSetOptions(Dbaccess::TCustomDADataSet* Owner);
	
__published:
	__property FullRefresh = {default=0};
	__property TrimFixedChar = {default=1};
	__property TrimVarChar = {default=0};
	__property SetEmptyStrToNull = {default=0};
	__property PrepareUpdateSQL = {default=0};
	__property SetFieldsReadOnly = {default=1};
	__property RequiredFields = {default=1};
	__property StrictUpdate = {default=1};
	__property NumberRange = {default=0};
	__property QueryRecCount = {default=0};
	__property AutoPrepare = {default=0};
	__property ReturnParams = {default=0};
	__property LongStrings = {default=1};
	__property FlatBuffers = {default=0};
	__property RemoveOnRefresh = {default=1};
	__property QuoteNames = {default=0};
	__property DetailDelay = {default=0};
	__property CompressBlobMode = {default=0};
	__property LocalMasterDetail = {default=0};
	__property CacheCalcFields = {default=0};
	__property FieldOrigins = {default=0};
	__property DefaultValues = {default=0};
	__property UpdateBatchSize = {default=1};
	__property UpdateAllFields = {default=0};
	__property EnableBCD = {default=0};
	__property EnableFMTBCD = {default=0};
	__property MasterFieldsNullable = {default=0};
	__property InsertAllSetFields = {default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TUniDataSetOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniDataSetSpecificOptions : public TSpecificOptionsHolder
{
	typedef TSpecificOptionsHolder inherited;
	
protected:
	virtual void __fastcall ValuesChanging(System::TObject* Sender);
public:
	/* TSpecificOptionsHolder.Create */ inline __fastcall TUniDataSetSpecificOptions(System::Classes::TComponent* AOwner, TGetUniProviderFunc GetUniProviderFunc) : TSpecificOptionsHolder(AOwner, GetUniProviderFunc) { }
	/* TSpecificOptionsHolder.Destroy */ inline __fastcall virtual ~TUniDataSetSpecificOptions() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCustomUniDataSet : public Dbaccess::TCustomDADataSet
{
	typedef Dbaccess::TCustomDADataSet inherited;
	
private:
	TUniDataSetSpecificOptions* FSpecificOptions;
	TUniTransaction* FFixedUsedTransaction;
	Craccess::TCRCursor* FCursor;
	bool FLockFetchAll;
	TUniConnection* __fastcall GetConnection();
	HIDESBASE void __fastcall SetConnection(TUniConnection* Value);
	TUniEncryption* __fastcall GetEncryption();
	HIDESBASE void __fastcall SetEncryption(TUniEncryption* Value);
	TUniDataSetOptions* __fastcall GetOptions();
	HIDESBASE void __fastcall SetOptions(TUniDataSetOptions* Value);
	TSpecificOptionsList* __fastcall GetSpecificOptions();
	void __fastcall SetSpecificOptions(TSpecificOptionsList* Value);
	TUniTransaction* __fastcall GetUniTransaction();
	void __fastcall SetUniTransaction(TUniTransaction* Value);
	TUniTransaction* __fastcall GetUpdateTransaction();
	HIDESBASE void __fastcall SetUpdateTransaction(TUniTransaction* Value);
	HIDESBASE TUniParams* __fastcall GetParams();
	HIDESBASE void __fastcall SetParams(TUniParams* Value);
	TUniUpdateSQL* __fastcall GetUpdateObject();
	HIDESBASE void __fastcall SetUpdateObject(TUniUpdateSQL* Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual Craccess::TCRCursor* __fastcall GetCRCursor();
	virtual void __fastcall SetCRCursor(Craccess::TCRCursor* Value);
	Uniprovider::TUniProvider* __fastcall FindProvider();
	Uniprovider::TUniProvider* __fastcall GetProvider();
	virtual void __fastcall CheckInactive();
	virtual void __fastcall SetIRecordSet(Memdata::TData* Value);
	virtual void __fastcall CreateCommand();
	virtual Memds::TDataSetServiceClass __fastcall GetDataSetServiceClass();
	virtual void __fastcall SetDataSetService(Memds::TDataSetService* Value);
	virtual Dbaccess::TDADataSetOptions* __fastcall CreateOptions();
	virtual Dbaccess::TDAEncryption* __fastcall CreateEncryption();
	virtual void __fastcall InternalExecute(int Iters, int Offset);
	virtual void __fastcall OpenCursor(bool InfoQuery);
	virtual void __fastcall CloseCursor();
	virtual bool __fastcall NeedComplexUpdateFieldDefList();
	virtual Dbaccess::TDATransaction* __fastcall GetTransaction();
	virtual void __fastcall SetTransaction(Dbaccess::TDATransaction* Value);
	virtual Dbaccess::TDATransaction* __fastcall UsedTransaction();
	virtual void __fastcall BeginConnection(bool NoConnectCheck = true);
	virtual void __fastcall EndConnection();
	Dbaccess::TDATransaction* __fastcall GetPSTransaction();
	virtual bool __fastcall PSInTransaction();
	virtual void __fastcall PSStartTransaction();
	virtual Memds::TFieldTypeMapClass __fastcall GetFieldTypeMapClass();
	virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldType FieldType)/* overload */;
	virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldDef* FieldDef)/* overload */;
	virtual Crparser::TSQLParserClass __fastcall GetParserClass();
	virtual System::UnicodeString __fastcall SQLAddWhere(const System::UnicodeString SQLText, const System::UnicodeString Condition);
	virtual System::UnicodeString __fastcall SQLSetOrderBy(const System::UnicodeString SQLText, const System::UnicodeString Fields);
	void __fastcall SetLockFetchAll(bool Value);
	virtual bool __fastcall GetFetchAll();
	virtual void __fastcall SetFetchAll(bool Value);
	virtual bool __fastcall GetNonBlocking();
	virtual void __fastcall QuickOpen(Dbaccess::TQuickOpenInfo &Info, bool Refresh = false);
	virtual void __fastcall Restore(const Dbaccess::TQuickOpenInfo &Info, bool RestoreActive = true);
	__property bool LockFetchAll = {read=FLockFetchAll, write=SetLockFetchAll, nodefault};
	
public:
	__fastcall virtual TCustomUniDataSet(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomUniDataSet();
	HIDESBASE TUniParam* __fastcall FindParam(const System::UnicodeString Value);
	HIDESBASE TUniParam* __fastcall ParamByName(const System::UnicodeString Value);
	Memdata::TDBObject* __fastcall GetObject(const System::UnicodeString FieldName);
	Memdata::TDBObject* __fastcall GetArray(const System::UnicodeString FieldName);
	virtual void __fastcall Prepare();
	virtual void __fastcall UnPrepare();
	void __fastcall CreateProcCall(const System::UnicodeString Name);
	bool __fastcall OpenNext();
	void __fastcall RefreshQuick(const bool CheckDeleted);
	__property TUniEncryption* Encryption = {read=GetEncryption, write=SetEncryption};
	__property SmartFetch;
	__property TUniConnection* Connection = {read=GetConnection, write=SetConnection};
	__property TUniTransaction* Transaction = {read=GetUniTransaction, write=SetUniTransaction, stored=IsTransactionStored};
	__property TUniTransaction* UpdateTransaction = {read=GetUpdateTransaction, write=SetUpdateTransaction};
	__property TUniParams* Params = {read=GetParams, write=SetParams, stored=false};
	__property TUniDataSetOptions* Options = {read=GetOptions, write=SetOptions};
	__property TSpecificOptionsList* SpecificOptions = {read=GetSpecificOptions, write=SetSpecificOptions};
	__property TUniUpdateSQL* UpdateObject = {read=GetUpdateObject, write=SetUpdateObject};
	__property __int64 LastInsertId = {read=FLastInsertId};
	__property Cursor;
	__property DMLRefresh = {default=0};
	__property KeyFields = {default=0};
	__property LockMode = {default=0};
	/* Hoisted overloads: */
	
protected:
	inline Data::Db::TFieldClass __fastcall  GetFieldClass(Data::Db::TFieldType FieldType, System::Word DataType){ return Memds::TMemDataSet::GetFieldClass(FieldType, DataType); }
	
};


class PASCALIMPLEMENTATION TUniQuery : public TCustomUniDataSet
{
	typedef TCustomUniDataSet inherited;
	
__published:
	__property UpdatingTable = {default=0};
	__property KeyFields = {default=0};
	__property SQLInsert;
	__property SQLDelete;
	__property SQLUpdate;
	__property SQLLock;
	__property SQLRefresh;
	__property SQLRecCount;
	__property LocalUpdate = {default=0};
	__property DataTypeMap;
	__property Encryption;
	__property SmartFetch;
	__property Connection;
	__property Transaction;
	__property UpdateTransaction;
	__property ParamCheck = {default=1};
	__property SQL;
	__property MasterSource;
	__property MasterFields = {default=0};
	__property DetailFields = {default=0};
	__property Debug = {default=0};
	__property Macros;
	__property Params;
	__property FetchRows = {default=25};
	__property ReadOnly = {default=0};
	__property UniDirectional = {default=0};
	__property CachedUpdates = {default=0};
	__property FilterSQL = {default=0};
	__property DMLRefresh = {default=0};
	__property LockMode = {default=0};
	__property RefreshOptions = {default=0};
	__property Options;
	__property SpecificOptions;
	__property BeforeExecute;
	__property AfterExecute;
	__property BeforeUpdateExecute;
	__property AfterUpdateExecute;
	__property OnUpdateError;
	__property OnUpdateRecord;
	__property BeforeFetch;
	__property AfterFetch;
	__property UpdateObject;
	__property Active = {default=0};
	__property AutoCalcFields = {default=1};
	__property Constraints = {stored=IsConstraintsStored};
	__property Filtered = {default=0};
	__property Filter = {default=0};
	__property FilterOptions = {default=0};
	__property IndexFieldNames = {default=0};
	__property ObjectView = {default=0};
	__property BeforeOpen;
	__property AfterOpen;
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
	__property CheckMode = {default=1};
	__property Fields;
public:
	/* TCustomUniDataSet.Create */ inline __fastcall virtual TUniQuery(System::Classes::TComponent* AOwner) : TCustomUniDataSet(AOwner) { }
	/* TCustomUniDataSet.Destroy */ inline __fastcall virtual ~TUniQuery() { }
	
};


class PASCALIMPLEMENTATION TCustomUniTable : public TCustomUniDataSet
{
	typedef TCustomUniDataSet inherited;
	
private:
	System::UnicodeString FTableName;
	System::UnicodeString FOrderFields;
	bool FSQLIsPrepared;
	void __fastcall SetTableName(const System::UnicodeString Value);
	void __fastcall SetOrderFields(const System::UnicodeString Value);
	
protected:
	virtual System::UnicodeString __fastcall PSGetTableName();
	virtual void __fastcall PSSetParams(Data::Db::TParams* AParams);
	virtual void __fastcall PSSetCommandText(const System::UnicodeString CommandText);
	virtual void __fastcall SetDataSetService(Memds::TDataSetService* Value);
	virtual void __fastcall OpenCursor(bool InfoQuery);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall CheckSQL();
	virtual void __fastcall SetFilterSQL(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall GetFinalSQL();
	
public:
	__fastcall virtual TCustomUniTable(System::Classes::TComponent* Owner);
	void __fastcall PrepareSQL();
	virtual void __fastcall Prepare();
	virtual void __fastcall Execute()/* overload */;
	HIDESBASE void __fastcall EmptyTable();
	__property System::UnicodeString TableName = {read=FTableName, write=SetTableName};
	__property System::UnicodeString OrderFields = {read=FOrderFields, write=SetOrderFields};
public:
	/* TCustomUniDataSet.Destroy */ inline __fastcall virtual ~TCustomUniTable() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  Execute(int Iters, int Offset = 0x0){ Dbaccess::TCustomDADataSet::Execute(Iters, Offset); }
	
};


class PASCALIMPLEMENTATION TUniTable : public TCustomUniTable
{
	typedef TCustomUniTable inherited;
	
__published:
	__property TableName = {default=0};
	__property OrderFields = {default=0};
	__property DataTypeMap;
	__property Encryption;
	__property SmartFetch;
	__property Connection;
	__property Transaction;
	__property UpdateTransaction;
	__property MasterSource;
	__property MasterFields = {default=0};
	__property DetailFields = {default=0};
	__property ReadOnly = {default=0};
	__property KeyFields = {default=0};
	__property FilterSQL = {default=0};
	__property DMLRefresh = {default=0};
	__property Debug = {default=0};
	__property Params;
	__property FetchRows = {default=25};
	__property UniDirectional = {default=0};
	__property CachedUpdates = {default=0};
	__property LockMode = {default=2};
	__property RefreshOptions = {default=0};
	__property OnUpdateError;
	__property OnUpdateRecord;
	__property UpdateObject;
	__property Active = {default=0};
	__property AutoCalcFields = {default=1};
	__property Constraints = {stored=IsConstraintsStored};
	__property Filtered = {default=0};
	__property Filter = {default=0};
	__property FilterOptions = {default=0};
	__property IndexFieldNames = {default=0};
	__property ObjectView = {default=0};
	__property BeforeOpen;
	__property AfterOpen;
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
	__property CheckMode = {default=1};
	__property Fields;
	__property Options;
	__property SpecificOptions;
public:
	/* TCustomUniTable.Create */ inline __fastcall virtual TUniTable(System::Classes::TComponent* Owner) : TCustomUniTable(Owner) { }
	
public:
	/* TCustomUniDataSet.Destroy */ inline __fastcall virtual ~TUniTable() { }
	
};


class PASCALIMPLEMENTATION TUniStoredProc : public TCustomUniDataSet
{
	typedef TCustomUniDataSet inherited;
	
private:
	System::UnicodeString FStoredProcName;
	bool FIsQuery;
	void __fastcall SetStoredProcName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall PSSetCommandText(const System::UnicodeString CommandText);
	virtual void __fastcall CreateCommand();
	virtual void __fastcall BeforeOpenCursor(bool InfoQuery);
	virtual void __fastcall DoBeforeExecute();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	virtual void __fastcall Prepare();
	void __fastcall PrepareSQL(bool IsQuery = false);
	void __fastcall ExecProc();
	
__published:
	__property System::UnicodeString StoredProcName = {read=FStoredProcName, write=SetStoredProcName};
	__property SQL;
	__property SQLInsert;
	__property SQLDelete;
	__property SQLUpdate;
	__property SQLLock;
	__property SQLRefresh;
	__property SQLRecCount;
	__property DataTypeMap;
	__property Encryption;
	__property SmartFetch;
	__property Connection;
	__property Transaction;
	__property UpdateTransaction;
	__property Debug = {default=0};
	__property Params;
	__property FetchRows = {default=25};
	__property ReadOnly = {default=0};
	__property UniDirectional = {default=0};
	__property CachedUpdates = {default=0};
	__property LockMode = {default=0};
	__property RefreshOptions = {default=0};
	__property Options;
	__property SpecificOptions;
	__property BeforeExecute;
	__property AfterExecute;
	__property BeforeUpdateExecute;
	__property AfterUpdateExecute;
	__property OnUpdateError;
	__property OnUpdateRecord;
	__property UpdateObject;
	__property Active = {default=0};
	__property AutoCalcFields = {default=1};
	__property Constraints = {stored=IsConstraintsStored};
	__property Filtered = {default=0};
	__property Filter = {default=0};
	__property FilterOptions = {default=0};
	__property IndexFieldNames = {default=0};
	__property ObjectView = {default=0};
	__property BeforeOpen;
	__property AfterOpen;
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
	__property Fields;
public:
	/* TCustomUniDataSet.Create */ inline __fastcall virtual TUniStoredProc(System::Classes::TComponent* AOwner) : TCustomUniDataSet(AOwner) { }
	/* TCustomUniDataSet.Destroy */ inline __fastcall virtual ~TUniStoredProc() { }
	
};


class PASCALIMPLEMENTATION TUniUpdateSQL : public Dbaccess::TCustomDAUpdateSQL
{
	typedef Dbaccess::TCustomDAUpdateSQL inherited;
	
protected:
	virtual Dbaccess::TCustomDADataSetClass __fastcall DataSetClass();
	virtual Dbaccess::TCustomDASQLClass __fastcall SQLClass();
public:
	/* TCustomDAUpdateSQL.Create */ inline __fastcall virtual TUniUpdateSQL(System::Classes::TComponent* Owner) : Dbaccess::TCustomDAUpdateSQL(Owner) { }
	/* TCustomDAUpdateSQL.Destroy */ inline __fastcall virtual ~TUniUpdateSQL() { }
	
};


class PASCALIMPLEMENTATION TUniDataSource : public Dbaccess::TCRDataSource
{
	typedef Dbaccess::TCRDataSource inherited;
	
public:
	/* TCRDataSource.Create */ inline __fastcall virtual TUniDataSource(System::Classes::TComponent* Owner) : Dbaccess::TCRDataSource(Owner) { }
	
public:
	/* TDataSource.Destroy */ inline __fastcall virtual ~TUniDataSource() { }
	
};


class PASCALIMPLEMENTATION TUniMetaData : public Dbaccess::TDAMetaData
{
	typedef Dbaccess::TDAMetaData inherited;
	
private:
	Dbaccess::TDATransaction* FFixedUsedTransaction;
	TUniConnection* __fastcall GetConnection();
	HIDESBASE void __fastcall SetConnection(TUniConnection* Value);
	TUniTransaction* __fastcall GetUniTransaction();
	void __fastcall SetUniTransaction(TUniTransaction* Value);
	
protected:
	virtual void __fastcall CloseCursor();
	virtual Dbaccess::TDATransaction* __fastcall UsedTransaction();
	virtual void __fastcall SetTransaction(Dbaccess::TDATransaction* Value);
	virtual void __fastcall BeginConnection();
	virtual void __fastcall EndConnection();
	
__published:
	__property Active = {default=0};
	__property Filtered = {default=0};
	__property Filter = {default=0};
	__property FilterOptions = {default=0};
	__property IndexFieldNames = {default=0};
	__property BeforeOpen;
	__property AfterOpen;
	__property BeforeClose;
	__property AfterClose;
	__property BeforeScroll;
	__property AfterScroll;
	__property OnFilterRecord;
	__property MetaDataKind = {default=0};
	__property Restrictions;
	__property TUniConnection* Connection = {read=GetConnection, write=SetConnection};
	__property TUniTransaction* Transaction = {read=GetUniTransaction, write=SetUniTransaction, stored=IsTransactionStored};
public:
	/* TDAMetaData.Create */ inline __fastcall virtual TUniMetaData(System::Classes::TComponent* AOwner) : Dbaccess::TDAMetaData(AOwner) { }
	/* TDAMetaData.Destroy */ inline __fastcall virtual ~TUniMetaData() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniBlob : public Memdata::TCompressedBlob
{
	typedef Memdata::TCompressedBlob inherited;
	
private:
	Memdata::TBlob* FNativeBlob;
	Memdata::TBlob* __fastcall GetNativeBlob();
	void __fastcall SetNativeBlob(Memdata::TBlob* Value);
	
public:
	__fastcall virtual ~TUniBlob();
	virtual void __fastcall Disconnect();
public:
	/* TBlob.Create */ inline __fastcall TUniBlob(bool IsUnicode) : Memdata::TCompressedBlob(IsUnicode) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniCursor : public Craccess::TCRCursor
{
	typedef Craccess::TCRCursor inherited;
	
private:
	Craccess::TCRCursor* FNativeCursor;
	
protected:
	Craccess::TCRCursor* __fastcall GetNativeCursor();
	void __fastcall SetNativeCursor(Craccess::TCRCursor* Value);
	
public:
	__fastcall virtual ~TUniCursor();
	virtual void __fastcall Disconnect();
	virtual bool __fastcall CanFetch();
public:
	/* TSharedObject.Create */ inline __fastcall TUniCursor() : Craccess::TCRCursor() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TUniParam : public Dbaccess::TDAParam
{
	typedef Dbaccess::TDAParam inherited;
	
private:
	HIDESBASE Memdata::TDBObject* __fastcall GetAsObject();
	HIDESBASE void __fastcall SetAsObject(Memdata::TDBObject* Value);
	Memdata::TDBObject* __fastcall GetAsArray();
	void __fastcall SetAsArray(Memdata::TDBObject* Value);
	
protected:
	Crtypes::TVariantArray FNativeDataArr;
	virtual Memdata::TSharedObject* __fastcall CreateObject();
	virtual Memdata::TSharedObject* __fastcall GetNativeParamObject(Memdata::TSharedObject* SourceObject)/* overload */;
	virtual bool __fastcall IsBlobDataType(Data::Db::TFieldType DataType);
	virtual bool __fastcall IsSharedObjectDataType(Data::Db::TFieldType DataType);
	bool __fastcall IsObjectDataType(Data::Db::TFieldType DataType);
	bool __fastcall IsObject();
	bool __fastcall IsArrayDataType(Data::Db::TFieldType DataType);
	bool __fastcall IsArray();
	virtual bool __fastcall GetIsNull();
	
public:
	TUniConnection* __fastcall GetConnection();
	virtual Memdata::TSharedObject* __fastcall GetNativeParamObject()/* overload */;
	__property AsCursor;
	__property Memdata::TDBObject* AsObject = {read=GetAsObject, write=SetAsObject};
	__property Memdata::TDBObject* AsArray = {read=GetAsArray, write=SetAsArray};
	
__published:
	__property National = {default=0};
public:
	/* TDAParam.Create */ inline __fastcall virtual TUniParam(System::Classes::TCollection* Collection)/* overload */ : Dbaccess::TDAParam(Collection) { }
	/* TDAParam.Destroy */ inline __fastcall virtual ~TUniParam() { }
	
public:
	/* TParam.Create */ inline __fastcall TUniParam(Data::Db::TParams* AParams, Data::Db::TParamType AParamType)/* overload */ : Dbaccess::TDAParam(AParams, AParamType) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniParams : public Dbaccess::TDAParams
{
	typedef Dbaccess::TDAParams inherited;
	
public:
	TUniParam* operator[](int Index) { return this->Items[Index]; }
	
private:
	HIDESBASE TUniParam* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TUniParam* Value);
	HIDESBASE System::Classes::TPersistent* __fastcall GetOwner();
	void __fastcall SetOwner(System::Classes::TPersistent* Owner);
	
protected:
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	__property Dbaccess::TParamsChangeType ParamsChangeType = {read=FParamsChangeType, write=FParamsChangeType, nodefault};
	
public:
	__fastcall TUniParams(System::Classes::TPersistent* Owner);
	HIDESBASE TUniParam* __fastcall FindParam(const System::UnicodeString Value);
	HIDESBASE TUniParam* __fastcall ParamByName(const System::UnicodeString Value);
	__property TUniParam* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TUniParams() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod bool __fastcall CanGetProvider(TUniConnection* Connection);
	__classmethod Uniprovider::TUniProvider* __fastcall GetProvider(TUniConnection* Connection);
	__classmethod Craccess::TCRConnection* __fastcall GetCRConnection(TUniConnection* Connection);
public:
	/* TObject.Create */ inline __fastcall TUniUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TUniUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define DACVersion L"11.4.2"
#define UniDACVersion L"8.4.2"
extern DELPHI_PACKAGE System::TClass __fastcall (*DefConnectDialogClassProc)(void);
extern DELPHI_PACKAGE bool EnableUniSQL;
extern DELPHI_PACKAGE bool OldTransactionBehaviour;
#define DACProductName L"UniDAC"
extern DELPHI_PACKAGE void __fastcall GetServerList(const System::UnicodeString ProviderName, System::Classes::TStrings* List, System::Classes::TStrings* SpecificOptions = (System::Classes::TStrings*)(0x0));
}	/* namespace Uni */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNI)
using namespace Uni;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniHPP
