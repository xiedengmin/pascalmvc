// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBAccess.pas' rev: 34.00 (Windows)

#ifndef DbaccessHPP
#define DbaccessHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <System.TypInfo.hpp>
#include <System.SyncObjs.hpp>
#include <System.Variants.hpp>
#include <Data.FmtBcd.hpp>
#include <Data.SqlTimSt.hpp>
#include <Data.DB.hpp>
#include <CLRClasses.hpp>
#include <CRXml.hpp>
#include <Data.DBCommon.hpp>
#include <Data.DBConsts.hpp>
#include <System.Generics.Collections.hpp>
#include <MemData.hpp>
#include <MemUtils.hpp>
#include <MemDS.hpp>
#include <CRTypes.hpp>
#include <CRAccess.hpp>
#include <CRParser.hpp>
#include <CRVio.hpp>
#include <CRDataTypeMap.hpp>
#include <CREncryption.hpp>
#include <CRConnectionString.hpp>
#include <CRServerEnumerator.hpp>
#include <CRConnectionPool.hpp>
#include <DASQLGenerator.hpp>
#include <DAConsts.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dbaccess
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EDAError;
class DELPHICLASS TFieldTypeInfo;
class DELPHICLASS TFieldTypeInfos;
class DELPHICLASS TDAFieldTypeMap;
struct TFailOverOperation;
class DELPHICLASS TDAConnectionOptions;
class DELPHICLASS TPoolingOptions;
class DELPHICLASS TDAConnectionSSLOptions;
class DELPHICLASS TDAMapRule;
class DELPHICLASS TDAMapRules;
class DELPHICLASS TDAConnectionMapRules;
class DELPHICLASS TDADataSetMapRules;
class DELPHICLASS TCustomDAConnection;
class DELPHICLASS TDAConnections;
class DELPHICLASS TDATransactions;
class DELPHICLASS TDATransaction;
class DELPHICLASS TDAParamValue;
class DELPHICLASS TDAParam;
class DELPHICLASS TDAParams;
class DELPHICLASS TDACursorField;
class DELPHICLASS TDADataSetUpdater;
class DELPHICLASS TDADataSetService;
class DELPHICLASS TDASQLGeneratorService;
struct TQuickOpenInfo;
class DELPHICLASS TDACondition;
class DELPHICLASS TDAConditions;
class DELPHICLASS TDAEncryption;
class DELPHICLASS TDADataSetOptions;
class DELPHICLASS TSmartFetchOptions;
class DELPHICLASS TCustomDADataSet;
class DELPHICLASS TCustomDASQL;
class DELPHICLASS TDAMetaData;
class DELPHICLASS TCustomDAUpdateSQL;
class DELPHICLASS TMacro;
class DELPHICLASS TMacros;
class DELPHICLASS TConnectDialogOption;
class DELPHICLASS TCustomConnectDialog;
struct TTableInfo;
class DELPHICLASS TCRDataSource;
class DELPHICLASS TDBAccessUtils;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCheckMode : unsigned char { cmNone, cmException, cmRefresh };

typedef System::TMetaClass* TDAFieldTypeMapClass;

typedef System::TMetaClass* TCustomDASQLClass;

typedef System::TMetaClass* TCustomDADataSetClass;

typedef System::TMetaClass* TDADataSetServiceClass;

typedef System::TMetaClass* TDADataSetUpdaterClass;

typedef System::TMetaClass* TConnectDialogClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EDAError : public Data::Db::EDatabaseError
{
	typedef Data::Db::EDatabaseError inherited;
	
protected:
	int FErrorCode;
	System::TObject* FComponent;
	
public:
	__fastcall EDAError(int ErrorCode, const System::UnicodeString Msg);
	virtual bool __fastcall IsFatalError();
	virtual bool __fastcall IsKeyViolation();
	__property int ErrorCode = {read=FErrorCode, nodefault};
	__property System::TObject* Component = {read=FComponent, write=FComponent};
public:
	/* Exception.CreateFmt */ inline __fastcall EDAError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Data::Db::EDatabaseError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EDAError(NativeUInt Ident)/* overload */ : Data::Db::EDatabaseError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EDAError(System::PResStringRec ResStringRec)/* overload */ : Data::Db::EDatabaseError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EDAError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Data::Db::EDatabaseError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EDAError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Data::Db::EDatabaseError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EDAError(const System::UnicodeString Msg, int AHelpContext) : Data::Db::EDatabaseError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EDAError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Data::Db::EDatabaseError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDAError(NativeUInt Ident, int AHelpContext)/* overload */ : Data::Db::EDatabaseError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDAError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Data::Db::EDatabaseError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDAError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Data::Db::EDatabaseError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDAError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Data::Db::EDatabaseError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EDAError() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFieldTypeInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Data::Db::TFieldType FFieldType;
	System::UnicodeString FName;
	bool FLength;
	bool FScale;
	
public:
	__fastcall TFieldTypeInfo(Data::Db::TFieldType FieldType, const System::UnicodeString Name, bool Length, bool Scale);
	__property Data::Db::TFieldType FieldType = {read=FFieldType, nodefault};
	__property System::UnicodeString Name = {read=FName};
	__property bool Length = {read=FLength, nodefault};
	__property bool Scale = {read=FScale, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFieldTypeInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFieldTypeInfos : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Crtypes::TCRObjectList* FTypeInfos;
	int __fastcall GetCount();
	TFieldTypeInfo* __fastcall GetTypeInfo(int Index);
	
public:
	__fastcall TFieldTypeInfos();
	__fastcall virtual ~TFieldTypeInfos();
	void __fastcall Add(Data::Db::TFieldType FieldType, const System::UnicodeString Name, bool Length, bool Scale);
	void __fastcall Delete(int Index);
	void __fastcall Clear();
	System::Sysutils::Exception* __fastcall Check(Data::Db::TFieldType FieldType, int Length, int Scale);
	TFieldTypeInfo* __fastcall FindTypeInfo(Data::Db::TFieldType FieldType)/* overload */;
	TFieldTypeInfo* __fastcall FindTypeInfo(const System::UnicodeString Name)/* overload */;
	__property TFieldTypeInfo* TypeInfos[int Index] = {read=GetTypeInfo};
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAFieldTypeMap : public Memds::TFieldTypeMap
{
	typedef Memds::TFieldTypeMap inherited;
	
public:
	__classmethod virtual TFieldTypeInfos* __fastcall GetFieldTypeInfos();
public:
	/* TObject.Create */ inline __fastcall TDAFieldTypeMap() : Memds::TFieldTypeMap() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDAFieldTypeMap() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TRetryMode : unsigned char { rmRaise, rmReconnect, rmReconnectExecute };

struct DECLSPEC_DRECORD TFailOverOperation
{
public:
	Memdata::TConnLostCause Operation;
	bool AllowFailOver;
};


typedef System::DynamicArray<TFailOverOperation> TOperationsStack;

typedef void __fastcall (__closure *TDAConnectionErrorEvent)(System::TObject* Sender, EDAError* E, bool &Fail);

typedef void __fastcall (__closure *TConnectionLostEvent)(System::TObject* Sender, System::Classes::TComponent* Component, Memdata::TConnLostCause ConnLostCause, TRetryMode &RetryMode);

typedef void __fastcall (__closure *TDAConnectionLoginEvent)(TCustomDAConnection* Connection, System::Classes::TStrings* LoginParams);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAConnectionOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FKeepDesignConnected;
	bool FDisconnectedMode;
	bool FLocalFailover;
	Memdata::TSortType FDefaultSortType;
	bool FEnableBCD;
	bool FEnableFMTBCD;
	bool FAllowImplicitConnect;
	bool FUuidWithBraces;
	void __fastcall SetDisconnectedMode(bool Value);
	void __fastcall SetDefaultSortType(Memdata::TSortType Value);
	void __fastcall SetEnableBCD(bool Value);
	void __fastcall SetEnableFMTBCD(bool Value);
	void __fastcall SetUuidWithBraces(const bool Value);
	
protected:
	TCustomDAConnection* FOwner;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	__property bool EnableBCD = {read=FEnableBCD, write=SetEnableBCD, default=0};
	__property bool EnableFMTBCD = {read=FEnableFMTBCD, write=SetEnableFMTBCD, default=0};
	__property bool UuidWithBraces = {read=FUuidWithBraces, write=SetUuidWithBraces, default=1};
	
public:
	__fastcall TDAConnectionOptions(TCustomDAConnection* Owner);
	__property bool DisconnectedMode = {read=FDisconnectedMode, write=SetDisconnectedMode, default=0};
	__property bool KeepDesignConnected = {read=FKeepDesignConnected, write=FKeepDesignConnected, default=1};
	__property bool LocalFailover = {read=FLocalFailover, write=FLocalFailover, default=0};
	__property Memdata::TSortType DefaultSortType = {read=FDefaultSortType, write=SetDefaultSortType, default=0};
	
__published:
	__property bool AllowImplicitConnect = {read=FAllowImplicitConnect, write=FAllowImplicitConnect, default=1};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDAConnectionOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TPoolingOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	TCustomDAConnection* FOwner;
	int FMaxPoolSize;
	int FMinPoolSize;
	int FConnectionLifetime;
	bool FValidate;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall virtual TPoolingOptions(TCustomDAConnection* Owner);
	
__published:
	__property int MaxPoolSize = {read=FMaxPoolSize, write=FMaxPoolSize, default=100};
	__property int MinPoolSize = {read=FMinPoolSize, write=FMinPoolSize, default=0};
	__property int ConnectionLifetime = {read=FConnectionLifetime, write=FConnectionLifetime, default=0};
	__property bool Validate = {read=FValidate, write=FValidate, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TPoolingOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAConnectionSSLOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TCustomDAConnection* FOwner;
	System::UnicodeString FCipherList;
	System::UnicodeString FCACert;
	System::UnicodeString FKey;
	System::UnicodeString FCert;
	void __fastcall SetCipherList(const System::UnicodeString Value);
	void __fastcall SetCACert(const System::UnicodeString Value);
	void __fastcall SetKey(const System::UnicodeString Value);
	void __fastcall SetCert(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadCipherList(System::Classes::TReader* Reader);
	__property TCustomDAConnection* Owner = {read=FOwner};
	
public:
	__fastcall virtual TDAConnectionSSLOptions(TCustomDAConnection* Owner);
	
__published:
	__property System::UnicodeString CipherList = {read=FCipherList, write=SetCipherList};
	__property System::UnicodeString CACert = {read=FCACert, write=SetCACert};
	__property System::UnicodeString Key = {read=FKey, write=SetKey};
	__property System::UnicodeString Cert = {read=FCert, write=SetCert};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDAConnectionSSLOptions() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDAConnectionSSLOptionsClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAMapRule : public Crdatatypemap::TMapRule
{
	typedef Crdatatypemap::TMapRule inherited;
	
private:
	Data::Db::TFieldType FFieldType;
	bool __fastcall IsFieldTypeStored();
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	HIDESBASE void __fastcall AssignToRule(TDAMapRule* Dest);
	void __fastcall DoRuleChanged();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadExtFieldType(System::Classes::TReader* Reader);
	void __fastcall WriteExtFieldType(System::Classes::TWriter* Writer);
	
public:
	__fastcall virtual TDAMapRule(System::Classes::TCollection* Collection);
	
__published:
	__property FieldName = {default=0};
	__property DBType = {default=0};
	__property DBLengthMin = {default=-1};
	__property DBLengthMax = {default=-1};
	__property DBScaleMin = {default=-1};
	__property DBScaleMax = {default=-1};
	__property Data::Db::TFieldType FieldType = {read=FFieldType, write=FFieldType, stored=IsFieldTypeStored, default=0};
	__property FieldLength = {default=-1};
	__property FieldScale = {default=-1};
	__property IgnoreErrors = {default=0};
	__property Format = {default=0};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TDAMapRule() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDAMapRuleClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAMapRules : public Crdatatypemap::TMapRules
{
	typedef Crdatatypemap::TMapRules inherited;
	
public:
	TDAMapRule* operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FIgnoreInvalidRules;
	HIDESBASE TDAMapRule* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TDAMapRule* Value);
	
protected:
	__classmethod virtual TDAMapRuleClass __fastcall GetMapRuleClass();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	void __fastcall AssignToRules(TDAMapRules* Dest);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	virtual System::Word __fastcall GetDataType(Data::Db::TFieldType FieldType) = 0 ;
	virtual TFieldTypeInfos* __fastcall GetFieldTypeInfos() = 0 ;
	virtual void __fastcall WriteTo(Crdatatypemap::TCRMapRules* Dest);
	Crparser::TParser* __fastcall CreateParser(const System::UnicodeString Rule);
	bool __fastcall ParseRule(Crparser::TParser* Parser, /* out */ System::UnicodeString &FieldName, /* out */ System::Word &DBType, /* out */ int &DBLengthMin, /* out */ int &DBLengthMax, /* out */ int &DBScaleMin, /* out */ int &DBScaleMax, /* out */ Data::Db::TFieldType &FieldType, /* out */ int &FieldLength, /* out */ int &FieldScale, /* out */ bool &IgnoreErrors, /* out */ System::UnicodeString &Format);
	virtual System::Sysutils::Exception* __fastcall CheckRule(const System::UnicodeString FieldName, System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, Data::Db::TFieldType FieldType, int FieldLength, int FieldScale);
	void __fastcall DoAddRule(const System::UnicodeString FieldName, System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, Data::Db::TFieldType FieldType, int FieldLength, int FieldScale, bool IgnoreErrors, const System::UnicodeString Format);
	virtual void __fastcall DoRulesChanged() = 0 ;
	
public:
	__fastcall virtual TDAMapRules();
	void __fastcall AddDBTypeRule(System::Word DBType, Data::Db::TFieldType FieldType, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddDBTypeRule(System::Word DBType, Data::Db::TFieldType FieldType, int FieldLength, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddDBTypeRule(System::Word DBType, Data::Db::TFieldType FieldType, int FieldLength, int FieldScale, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddDBTypeRule(System::Word DBType, int DBLengthMin, int DBLengthMax, Data::Db::TFieldType FieldType, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddDBTypeRule(System::Word DBType, int DBLengthMin, int DBLengthMax, Data::Db::TFieldType FieldType, int FieldLength, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddDBTypeRule(System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, Data::Db::TFieldType FieldType, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddDBTypeRule(System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, Data::Db::TFieldType FieldType, int FieldLength, int FieldScale, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddFieldNameRule(const System::UnicodeString FieldName, Data::Db::TFieldType FieldType, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddFieldNameRule(const System::UnicodeString FieldName, Data::Db::TFieldType FieldType, int FieldLength, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddFieldNameRule(const System::UnicodeString FieldName, Data::Db::TFieldType FieldType, int FieldLength, int FieldScale, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddRule(const System::UnicodeString FieldName, System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, Data::Db::TFieldType FieldType, int FieldLength, int FieldScale, bool IgnoreErrors = false, const System::UnicodeString Format = System::UnicodeString())/* overload */;
	void __fastcall AddRule(const System::UnicodeString Rule)/* overload */;
	void __fastcall AddRules(const System::UnicodeString Rules);
	__property TDAMapRule* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	
__published:
	__property bool IgnoreInvalidRules = {read=FIgnoreInvalidRules, write=FIgnoreInvalidRules, default=0};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TDAMapRules() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAConnectionMapRules : public TDAMapRules
{
	typedef TDAMapRules inherited;
	
private:
	TCustomDAConnection* FConnection;
	
protected:
	virtual System::Word __fastcall GetDataType(Data::Db::TFieldType FieldType);
	virtual TFieldTypeInfos* __fastcall GetFieldTypeInfos();
	virtual System::Sysutils::Exception* __fastcall CheckRule(const System::UnicodeString FieldName, System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, Data::Db::TFieldType FieldType, int FieldLength, int FieldScale);
	virtual void __fastcall DoRulesChanged();
	
public:
	__fastcall virtual TDAConnectionMapRules(TCustomDAConnection* Connection);
	__property TCustomDAConnection* Connection = {read=FConnection};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TDAConnectionMapRules() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDAConnectionMapRulesClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADataSetMapRules : public TDAMapRules
{
	typedef TDAMapRules inherited;
	
private:
	TCustomDADataSet* FDataSet;
	
protected:
	virtual System::Word __fastcall GetDataType(Data::Db::TFieldType FieldType);
	virtual TFieldTypeInfos* __fastcall GetFieldTypeInfos();
	virtual System::Sysutils::Exception* __fastcall CheckRule(const System::UnicodeString FieldName, System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, Data::Db::TFieldType FieldType, int FieldLength, int FieldScale);
	virtual void __fastcall DoRulesChanged();
	
public:
	__fastcall virtual TDADataSetMapRules(TCustomDADataSet* DataSet);
	__property TCustomDADataSet* DataSet = {read=FDataSet};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TDADataSetMapRules() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDADataSetMapRulesClass;

class PASCALIMPLEMENTATION TCustomDAConnection : public Data::Db::TCustomConnection
{
	typedef Data::Db::TCustomConnection inherited;
	
private:
	bool FInProcessDisconnecting;
	TDATransactions* FTransactions;
	System::UnicodeString FUsername;
	bool FAutoCommit;
	bool FInProcessError;
	TCustomConnectDialog* FConnectDialog;
	bool FDebug;
	TDAConnectionErrorEvent FOnError;
	TDAConnectionLoginEvent FOnLogin;
	bool FConvertEOL;
	TDAConnectionOptions* FOptions;
	TPoolingOptions* FPoolingOptions;
	bool FPooling;
	TConnectionLostEvent FOnConnectionLost;
	System::Syncobjs::TCriticalSection* hRegisterClient;
	TDAMapRules* FDataTypeMap;
	bool __fastcall IsMapRulesStored();
	void __fastcall SetDefaultTransaction(TDATransaction* Value);
	TDATransaction* __fastcall GetDefaultTransaction();
	TDATransaction* __fastcall GetTransaction(int Index);
	int __fastcall GetTransactionsCount();
	void __fastcall SetUsername(const System::UnicodeString Value);
	void __fastcall SetPassword(const System::UnicodeString Value);
	void __fastcall SetAutoCommit(bool Value);
	void __fastcall SetConnectDialog(TCustomConnectDialog* Value);
	void __fastcall SetPooling(bool Value);
	void __fastcall SetDebug(bool Value);
	void __fastcall DoAfterConnect();
	
protected:
	TDATransaction* FDefaultTransaction;
	TDATransaction* FInternalDefTransaction;
	int FConnectCount;
	System::Classes::TList* FSQLs;
	Craccess::TCRConnection* FIConnection;
	bool FStreamedConnected;
	System::UnicodeString FServer;
	System::UnicodeString FPassword;
	bool FShouldShowPrompt;
	TOperationsStack FOperationsStack;
	int FOperationsStackLen;
	TCustomDASQL* FCommand;
	bool FLockLoginPrompt;
	Crconnectionstring::TCRConnectionStringBuilder* FConnectionStringBuilder;
	Crvio::TCRIOHandler* FIOHandler;
	TDAConnectionSSLOptions* FSSLOptions;
	Crvio::THttpOptions* FHttpOptions;
	Crvio::TProxyOptions* FProxyOptions;
	virtual void __fastcall ClearRefs();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetIOHandler(Crvio::TCRIOHandler* Value);
	virtual TDAConnectionSSLOptionsClass __fastcall GetSSLOptionsClass();
	void __fastcall SetSSLOptions(TDAConnectionSSLOptions* Value);
	void __fastcall SetHttpOptions(Crvio::THttpOptions* Value);
	void __fastcall SetProxyOptions(Crvio::TProxyOptions* Value);
	virtual void __fastcall SetConnectionParameters(Crconnectionpool::TCRConnectionParameters* ConnectionParameters);
	virtual void __fastcall SetBaseConnectionProps(Craccess::TCRConnection* Connection);
	void __fastcall SetHttpConnectionProps(Craccess::TCRConnection* Connection);
	virtual void __fastcall SetConnectionProps(Craccess::TCRConnection* Connection);
	virtual Crconnectionpool::TCRConnectionParametersClass __fastcall GetConnectionParametersClass();
	virtual Crconnectionpool::TCRConnectionPoolManagerClass __fastcall GetConnectionPoolingManagerClass();
	Craccess::TCRConnection* __fastcall GetIConnection();
	virtual Craccess::TCRConnectionClass __fastcall GetIConnectionClass();
	virtual Craccess::TCRCommandClass __fastcall GetICommandClass();
	virtual Craccess::TCRRecordSetClass __fastcall GetIRecordSetClass();
	virtual Craccess::TCRMetaDataClass __fastcall GetIMetaDataClass();
	virtual Craccess::TCRTransactionClass __fastcall GetITransactionClass();
	virtual bool __fastcall IsMultipleTransactionsSupported();
	virtual bool __fastcall ApplyUpdatesInTransaction();
	virtual void __fastcall CreateIConnection();
	void __fastcall FreeIConnection();
	virtual void __fastcall SetIConnection(Craccess::TCRConnection* Value);
	Craccess::TCRCommand* __fastcall CreateICommand();
	Craccess::TCRRecordSet* __fastcall CreateIRecordSet();
	void __fastcall ClearTransactionRefs();
	virtual TDAFieldTypeMapClass __fastcall GetFieldTypeMapClass();
	virtual TDAConnectionMapRules* __fastcall CreateDataTypeMap();
	void __fastcall SetDataTypeMap(TDAMapRules* Value);
	virtual void __fastcall Loaded();
	virtual void __fastcall RegisterClient(System::TObject* Client, Data::Db::TConnectChangeEvent Event = 0x0);
	virtual void __fastcall UnRegisterClient(System::TObject* Client);
	virtual System::TClass __fastcall SQLMonitorClass();
	virtual TConnectDialogClass __fastcall ConnectDialogClass();
	virtual void __fastcall DoConnect();
	virtual void __fastcall DoDisconnect();
	virtual void __fastcall DisconnectTransaction();
	virtual void __fastcall InternalConnect();
	virtual void __fastcall InternalDisconnect();
	void __fastcall CheckInactive();
	virtual System::UnicodeString __fastcall InternalGetServer();
	virtual bool __fastcall IsConnectedStored();
	virtual bool __fastcall NeedPrompt();
	virtual int __fastcall PushOperation(Memdata::TConnLostCause Operation, bool AllowFailOver = true);
	virtual Memdata::TConnLostCause __fastcall PopOperation();
	virtual void __fastcall ResetOnFatalError();
	virtual void __fastcall RestoreAfterFailOver();
	virtual bool __fastcall IsFailOverAllowed();
	virtual Memdata::TConnLostCause __fastcall DetectConnLostCause(System::TObject* Component);
	virtual void __fastcall DoError(System::Sysutils::Exception* E, bool &Fail, bool &Reconnect, bool &Reexecute, int ReconnectAttempt, Memdata::TConnLostCause &ConnLostCause);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall GetLoginParams(System::Classes::TStrings* LoginParams)/* overload */;
	virtual void __fastcall SetLoginParams(System::Classes::TStrings* LoginParams)/* overload */;
	virtual bool __fastcall GetConnected();
	virtual void __fastcall SetConnected(bool Value);
	virtual System::UnicodeString __fastcall GetConnectionString();
	virtual void __fastcall SetConnectionString(const System::UnicodeString Value);
	virtual void __fastcall SetServer(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall DefaultTableSchema();
	void __fastcall SuppressAutoCommit();
	void __fastcall RestoreAutoCommit();
	virtual bool __fastcall DetectInTransaction(bool CanActivate = false);
	virtual bool __fastcall GetInTransaction();
	virtual TDATransaction* __fastcall UsedTransaction();
	void __fastcall SetConvertEOL(bool Value);
	virtual void __fastcall CheckCommand();
	virtual void __fastcall AssignConnectOptions(TCustomDAConnection* Source);
	virtual TDAConnectionOptions* __fastcall CreateOptions();
	void __fastcall SetOptions(TDAConnectionOptions* Value);
	virtual TPoolingOptions* __fastcall CreatePoolingOptions();
	void __fastcall SetPoolingOptions(TPoolingOptions* Value);
	virtual Crconnectionstring::TCRConnectionStringBuilder* __fastcall CreateConnectionStringBuilder();
	int __fastcall InternalAddTransaction(TDATransaction* TR);
	void __fastcall InternalRemoveTransaction(TDATransaction* TR);
	virtual int __fastcall DoAddTransaction(TDATransaction* TR);
	virtual void __fastcall DoRemoveTransaction(TDATransaction* TR);
	virtual void __fastcall DoCommitRetaining();
	virtual void __fastcall DoRollbackRetaining();
	virtual void __fastcall DoSavepoint(const System::UnicodeString Name);
	virtual void __fastcall DoRollbackToSavepoint(const System::UnicodeString Name);
	virtual void __fastcall DoReleaseSavepoint(const System::UnicodeString Name);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadEncryptedPassword(System::Classes::TReader* Reader);
	void __fastcall ReadPassword(System::Classes::TReader* Reader);
	void __fastcall WriteEncryptedPassword(System::Classes::TWriter* Writer);
	virtual System::DynamicArray<System::Byte> __fastcall Encrypt(const System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall Decrypt(const System::DynamicArray<System::Byte> Value);
	System::UnicodeString __fastcall EncryptToHex(const System::UnicodeString Value);
	System::UnicodeString __fastcall DecryptFromHex(const System::UnicodeString Value);
	TCustomDASQL* __fastcall InternalGetSQL();
	virtual System::Variant __fastcall GetConnectionStringParam(int Param);
	virtual void __fastcall SetConnectionStringParam(int Param, const System::Variant &Value);
	__property TDATransaction* DefaultTransaction = {read=GetDefaultTransaction, write=SetDefaultTransaction};
	__property int TransactionCount = {read=GetTransactionsCount, nodefault};
	__property TDATransaction* Transactions[int Index] = {read=GetTransaction};
	__property bool AutoCommit = {read=FAutoCommit, write=SetAutoCommit, default=1};
	__property Crvio::TCRIOHandler* IOHandler = {read=FIOHandler, write=SetIOHandler};
	__property Crvio::THttpOptions* HttpOptions = {read=FHttpOptions, write=SetHttpOptions};
	__property Crvio::TProxyOptions* ProxyOptions = {read=FProxyOptions, write=SetProxyOptions};
	
public:
	__fastcall virtual TCustomDAConnection(System::Classes::TComponent* Owner)/* overload */;
	__fastcall TCustomDAConnection(System::Classes::TComponent* Owner, const System::UnicodeString ConnectString)/* overload */;
	__fastcall virtual ~TCustomDAConnection();
	void __fastcall Connect()/* overload */;
	void __fastcall Connect(const System::UnicodeString ConnectString)/* overload */;
	void __fastcall Disconnect();
	void __fastcall PerformConnect(bool Retry = false);
	void __fastcall AssignConnect(TCustomDAConnection* Source);
	void __fastcall Ping();
	TDAParam* __fastcall ParamByName(const System::UnicodeString Name);
	System::Variant __fastcall ExecSQL(const System::UnicodeString Text)/* overload */;
	virtual System::Variant __fastcall ExecSQL(const System::UnicodeString Text, const System::Variant *Params, const int Params_High)/* overload */;
	virtual System::Variant __fastcall ExecSQLEx(const System::UnicodeString Text, const System::Variant *Params, const int Params_High);
	virtual System::Variant __fastcall ExecProc(const System::UnicodeString Name, const System::Variant *Params, const int Params_High);
	virtual System::Variant __fastcall ExecProcEx(const System::UnicodeString Name, const System::Variant *Params, const int Params_High);
	virtual void __fastcall GetTableNames(System::Classes::TStrings* List, bool AllTables = false, bool OnlyTables = false);
	virtual void __fastcall GetDatabaseNames(System::Classes::TStrings* List);
	virtual void __fastcall GetStoredProcNames(System::Classes::TStrings* List, bool AllProcs = false);
	virtual void __fastcall GetFieldNames(const System::UnicodeString TableName, System::Classes::TStrings* List);
	virtual void __fastcall GetKeyFieldNames(const System::UnicodeString TableName, System::Classes::TStrings* List);
	virtual void __fastcall StartTransaction();
	virtual void __fastcall Commit();
	virtual void __fastcall Rollback();
	virtual void __fastcall ApplyUpdates()/* overload */;
	virtual void __fastcall ApplyUpdates(TCustomDADataSet* const *DataSets, const int DataSets_High)/* overload */;
	virtual TDATransaction* __fastcall CreateTransaction();
	virtual TCustomDADataSet* __fastcall CreateDataSet(System::Classes::TComponent* AOwner = (System::Classes::TComponent*)(0x0));
	virtual TCustomDASQL* __fastcall CreateSQL();
	virtual TDAMetaData* __fastcall CreateMetaData();
	virtual void __fastcall EncryptTable(const System::UnicodeString TableName, Crencryption::TCREncryptor* Encryptor, const System::UnicodeString Fields);
	void __fastcall RemoveFromPool();
	void __fastcall MonitorMessage(const System::UnicodeString Msg);
	__property System::UnicodeString ConnectString = {read=GetConnectionString, write=SetConnectionString, stored=false};
	__property System::UnicodeString Username = {read=FUsername, write=SetUsername};
	__property System::UnicodeString Password = {read=FPassword, write=SetPassword, stored=false};
	__property System::UnicodeString Server = {read=FServer, write=SetServer};
	__property bool InTransaction = {read=GetInTransaction, nodefault};
	__property TCustomConnectDialog* ConnectDialog = {read=FConnectDialog, write=SetConnectDialog};
	__property TDAConnectionErrorEvent OnError = {read=FOnError, write=FOnError};
	__property TConnectionLostEvent OnConnectionLost = {read=FOnConnectionLost, write=FOnConnectionLost};
	__property TDAConnectionLoginEvent OnLogin = {read=FOnLogin, write=FOnLogin};
	__property LoginPrompt = {default=1};
	__property bool ConvertEOL = {read=FConvertEOL, write=SetConvertEOL, default=0};
	__property bool Debug = {read=FDebug, write=SetDebug, default=0};
	__property TDAConnectionOptions* Options = {read=FOptions, write=SetOptions};
	__property TPoolingOptions* PoolingOptions = {read=FPoolingOptions, write=SetPoolingOptions};
	__property bool Pooling = {read=FPooling, write=SetPooling, default=0};
	__property TDAMapRules* DataTypeMap = {read=FDataTypeMap, write=SetDataTypeMap, stored=IsMapRulesStored};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAConnections : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TCustomDAConnection* operator[](int Index) { return this->Items[Index]; }
	
private:
	TCustomDAConnection* __fastcall GetItems(int Index);
	
public:
	__property TCustomDAConnection* Items[int Index] = {read=GetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TDAConnections() { }
	
public:
	/* TObject.Create */ inline __fastcall TDAConnections() : System::Classes::TList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDATransactions : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TDATransaction* operator[](int Index) { return this->Items[Index]; }
	
private:
	TDATransaction* __fastcall GetItems(int Index);
	
public:
	__property TDATransaction* Items[int Index] = {read=GetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TDATransactions() { }
	
public:
	/* TObject.Create */ inline __fastcall TDATransactions() : System::Classes::TList() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TTransactionType : unsigned char { ttNative, ttMTS };

typedef void __fastcall (__closure *TDATransactionErrorEvent)(System::TObject* Sender, EDAError* E, bool &Fail);

class PASCALIMPLEMENTATION TDATransaction : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	void __fastcall SetDefaultConnection(TCustomDAConnection* Value);
	void __fastcall SetIsolationLevel(Craccess::TCRIsolationLevel Value);
	void __fastcall SetReadOnly(bool Value);
	void __fastcall SetTransactionType(TTransactionType Value);
	TCustomDAConnection* __fastcall GetConnection(int Index);
	int __fastcall GetConnectionsCount();
	bool __fastcall GetActive();
	
protected:
	bool FDesignCreate;
	TCustomDAConnection* FDefaultConnection;
	int FTrStartCount;
	int FUnCommitedStatementCount;
	bool FExplicitlyStarted;
	bool FDisconnectedMode;
	int FFailOverSatus;
	bool FPrepared;
	TTransactionType FTransactionType;
	Craccess::TCRTransactionAction FDefaultCloseAction;
	Craccess::TCRIsolationLevel FIsolationLevel;
	bool FReadOnly;
	bool FInProcessError;
	TDATransactionErrorEvent FOnError;
	Craccess::TCRTransaction* FITransaction;
	bool FShareTransaction;
	TDAConnections* FConnections;
	System::Classes::TNotifyEvent FOnStart;
	System::Classes::TNotifyEvent FOnCommit;
	System::Classes::TNotifyEvent FOnRollback;
	System::Classes::TNotifyEvent FOnCommitRetaining;
	System::Classes::TNotifyEvent FOnRollbackRetaining;
	bool __fastcall IsInternalTrStored();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual Craccess::TCRTransactionClass __fastcall GetITransactionClass();
	void __fastcall CheckITransaction();
	void __fastcall CreateITransaction();
	virtual void __fastcall SetITransaction(Craccess::TCRTransaction* Value);
	void __fastcall FreeITransaction();
	void __fastcall ClearRefs();
	virtual bool __fastcall DetectInTransaction(bool CanActivate = false);
	virtual void __fastcall CheckActive();
	virtual void __fastcall CheckInactive();
	void __fastcall Reset();
	void __fastcall Restore();
	void __fastcall CloseDataSets();
	virtual void __fastcall CloseTransaction(bool Force = false);
	virtual void __fastcall GainTransaction();
	virtual void __fastcall AutoCommitTransaction(bool NeedCommit);
	virtual void __fastcall ReleaseTransaction();
	virtual bool __fastcall CanAutoCommitExplicitTransaction();
	virtual System::TClass __fastcall SQLMonitorClass();
	virtual TCustomDAConnection* __fastcall UsedConnection();
	void __fastcall PrepareTransaction(bool CheckOnly = false);
	void __fastcall UnPrepareTransaction();
	virtual void __fastcall DoCommitRetaining();
	virtual void __fastcall DoRollbackRetaining();
	virtual void __fastcall DoSavepoint(const System::UnicodeString Name);
	virtual void __fastcall DoReleaseSavepoint(const System::UnicodeString Name);
	virtual void __fastcall DoRollbackToSavepoint(const System::UnicodeString Name);
	int __fastcall InternalAddConnection(TCustomDAConnection* Connection);
	void __fastcall InternalRemoveConnection(TCustomDAConnection* Connection);
	virtual int __fastcall DoAddConnection(TCustomDAConnection* Connection);
	virtual void __fastcall DoRemoveConnection(TCustomDAConnection* Connection);
	void __fastcall DoClearConnections();
	void __fastcall DoError(System::Sysutils::Exception* E, bool &Fail);
	__property TCustomDAConnection* Connections[int Index] = {read=GetConnection};
	__property int ConnectionsCount = {read=GetConnectionsCount, nodefault};
	__property TTransactionType TransactionType = {read=FTransactionType, write=SetTransactionType, default=0};
	__property Craccess::TCRIsolationLevel IsolationLevel = {read=FIsolationLevel, write=SetIsolationLevel, default=0};
	__property bool ReadOnly = {read=FReadOnly, write=SetReadOnly, default=0};
	
public:
	__fastcall virtual TDATransaction(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TDATransaction();
	__property bool Active = {read=GetActive, nodefault};
	virtual void __fastcall StartTransaction();
	virtual void __fastcall Commit();
	virtual void __fastcall Rollback();
	__property TCustomDAConnection* DefaultConnection = {read=FDefaultConnection, write=SetDefaultConnection};
	__property Craccess::TCRTransactionAction DefaultCloseAction = {read=FDefaultCloseAction, write=FDefaultCloseAction, default=1};
	__property TDATransactionErrorEvent OnError = {read=FOnError, write=FOnError};
	__property System::Classes::TNotifyEvent OnStart = {read=FOnStart, write=FOnStart};
	__property System::Classes::TNotifyEvent OnCommit = {read=FOnCommit, write=FOnCommit};
	__property System::Classes::TNotifyEvent OnRollback = {read=FOnRollback, write=FOnRollback};
	__property System::Classes::TNotifyEvent OnCommitRetaining = {read=FOnCommitRetaining, write=FOnCommitRetaining};
	__property System::Classes::TNotifyEvent OnRollbackRetaining = {read=FOnRollbackRetaining, write=FOnRollbackRetaining};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAParamValue : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TDAParam* FParam;
	int FIndex;
	
protected:
	virtual void __fastcall CheckDataType(Data::Db::TFieldType Value, const Data::Db::TFieldType *CompatibleTypes, const int CompatibleTypes_High);
	void __fastcall CheckBlobDataType(Data::Db::TFieldType Value);
	virtual void __fastcall SetAsSmallInt(int Value);
	virtual int __fastcall GetAsInteger();
	virtual void __fastcall SetAsInteger(int Value);
	virtual void __fastcall SetAsWord(int Value);
	virtual __int64 __fastcall GetAsLargeInt();
	virtual void __fastcall SetAsLargeInt(__int64 Value);
	virtual unsigned __fastcall GetAsLongWord();
	virtual void __fastcall SetAsLongWord(unsigned Value);
	virtual void __fastcall SetAsShortInt(int Value);
	virtual void __fastcall SetAsByte(int Value);
	virtual System::UnicodeString __fastcall GetAsString();
	virtual void __fastcall SetAsString(const System::UnicodeString Value);
	virtual System::AnsiString __fastcall GetAsAnsiString();
	virtual void __fastcall SetAsAnsiString(const System::AnsiString Value);
	virtual System::WideString __fastcall GetAsWideString();
	virtual void __fastcall SetAsWideString(const System::WideString Value);
	virtual System::Currency __fastcall GetAsBCD();
	virtual void __fastcall SetAsBCD(const System::Currency Value);
	virtual Data::Fmtbcd::TBcd __fastcall GetAsFMTBCD();
	virtual void __fastcall SetAsFMTBCD(const Data::Fmtbcd::TBcd &Value);
	virtual Data::Sqltimst::TSQLTimeStamp __fastcall GetAsSQLTimeStamp();
	virtual void __fastcall SetAsSQLTimeStamp(const Data::Sqltimst::TSQLTimeStamp &Value);
	virtual bool __fastcall GetAsBoolean();
	virtual void __fastcall SetAsBoolean(bool Value);
	virtual System::Currency __fastcall GetAsCurrency();
	virtual void __fastcall SetAsCurrency(const System::Currency Value);
	virtual void __fastcall SetAsDate(const System::TDateTime Value);
	virtual void __fastcall SetAsTime(const System::TDateTime Value);
	virtual System::TDateTime __fastcall GetAsDateTime();
	virtual void __fastcall SetAsDateTime(const System::TDateTime Value);
	virtual Data::Sqltimst::TSQLTimeStampOffset __fastcall GetAsSQLTimeStampOffset();
	virtual void __fastcall SetAsSQLTimeStampOffset(const Data::Sqltimst::TSQLTimeStampOffset &Value);
	virtual float __fastcall GetAsSingle();
	virtual void __fastcall SetAsSingle(const float Value);
	virtual double __fastcall GetAsFloat();
	virtual void __fastcall SetAsFloat(const double Value);
	virtual void __fastcall SetAsMemo(const System::UnicodeString Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetAsBytes();
	virtual void __fastcall SetAsBytes(const System::DynamicArray<System::Byte> Value);
	virtual void __fastcall SetAsBlob(const System::DynamicArray<System::Byte> Value);
	virtual Memdata::TBlob* __fastcall GetAsBlobRef();
	virtual void __fastcall SetAsBlobRef(Memdata::TBlob* const Value);
	virtual System::Variant __fastcall GetAsVariant();
	virtual void __fastcall SetAsVariant(const System::Variant &Value);
	virtual bool __fastcall GetIsNull();
	virtual void __fastcall SetIsNull(bool Value);
	virtual Memdata::TSharedObject* __fastcall CreateValueObject();
	virtual void __fastcall FreeValueObject();
	Memdata::TSharedObject* __fastcall AllocValueObject();
	virtual Memdata::TSharedObject* __fastcall GetValueObject();
	virtual void __fastcall SetValueObject(Memdata::TSharedObject* Value);
	__property TDAParam* Param = {read=FParam};
	
public:
	__fastcall TDAParamValue(TDAParam* Param);
	virtual void __fastcall Clear();
	__property int Index = {read=FIndex, nodefault};
	__property int AsSmallInt = {read=GetAsInteger, write=SetAsSmallInt, nodefault};
	__property int AsInteger = {read=GetAsInteger, write=SetAsInteger, nodefault};
	__property int AsWord = {read=GetAsInteger, write=SetAsWord, nodefault};
	__property __int64 AsLargeInt = {read=GetAsLargeInt, write=SetAsLargeInt};
	__property int AsByte = {read=GetAsInteger, write=SetAsByte, nodefault};
	__property int AsShortInt = {read=GetAsInteger, write=SetAsShortInt, nodefault};
	__property unsigned AsLongWord = {read=GetAsLongWord, write=SetAsLongWord, nodefault};
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	__property System::AnsiString AsAnsiString = {read=GetAsAnsiString, write=SetAsAnsiString};
	__property System::WideString AsWideString = {read=GetAsWideString, write=SetAsWideString};
	__property System::Currency AsBCD = {read=GetAsBCD, write=SetAsBCD};
	__property Data::Fmtbcd::TBcd AsFMTBCD = {read=GetAsFMTBCD, write=SetAsFMTBCD};
	__property Data::Sqltimst::TSQLTimeStamp AsSQLTimeStamp = {read=GetAsSQLTimeStamp, write=SetAsSQLTimeStamp};
	__property bool AsBoolean = {read=GetAsBoolean, write=SetAsBoolean, nodefault};
	__property System::Currency AsCurrency = {read=GetAsCurrency, write=SetAsCurrency};
	__property System::TDateTime AsDate = {read=GetAsDateTime, write=SetAsDate};
	__property System::TDateTime AsTime = {read=GetAsDateTime, write=SetAsTime};
	__property System::TDateTime AsDateTime = {read=GetAsDateTime, write=SetAsDateTime};
	__property Data::Sqltimst::TSQLTimeStampOffset AsSQLTimeStampOffset = {read=GetAsSQLTimeStampOffset, write=SetAsSQLTimeStampOffset};
	__property float AsSingle = {read=GetAsSingle, write=SetAsSingle};
	__property double AsFloat = {read=GetAsFloat, write=SetAsFloat};
	__property System::UnicodeString AsMemo = {read=GetAsString, write=SetAsMemo};
	__property System::DynamicArray<System::Byte> AsBytes = {read=GetAsBytes, write=SetAsBytes};
	__property System::DynamicArray<System::Byte> AsBlob = {read=GetAsBytes, write=SetAsBlob};
	__property Memdata::TBlob* AsBlobRef = {read=GetAsBlobRef, write=SetAsBlobRef};
	__property bool IsNull = {read=GetIsNull, write=SetIsNull, nodefault};
	__property System::Variant Value = {read=GetAsVariant, write=SetAsVariant};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDAParamValue() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDAParamValueClass;

class PASCALIMPLEMENTATION TDAParam : public Data::Db::TParam
{
	typedef Data::Db::TParam inherited;
	
public:
	TDAParamValue* operator[](int Index) { return this->Values[Index]; }
	
private:
	int FSize;
	System::Word FSubDataType;
	TDAParamValue* FParamValue;
	bool __fastcall IsDataTypeStored();
	bool __fastcall IsValueStored();
	void __fastcall CheckIndex(int Index);
	void __fastcall CheckGetValue();
	void __fastcall CheckSetValue();
	
protected:
	Memdata::TSharedObject* FParamObject;
	Crtypes::TVariantArray FDataArr;
	int FValueCount;
	bool FNational;
	Crencryption::TEncryptionMethod FEncryptor;
	virtual bool __fastcall NeedBlobUnicode();
	virtual Memdata::TSharedObject* __fastcall GetNativeParamObject()/* overload */;
	virtual Memdata::TSharedObject* __fastcall GetNativeParamObject(Memdata::TSharedObject* SourceObject)/* overload */;
	HIDESBASE Memdata::TSharedObject* __fastcall GetParamObject();
	void __fastcall SetParamObject(Memdata::TSharedObject* Value);
	virtual bool __fastcall IsSharedObjectDataType(Data::Db::TFieldType DataType);
	bool __fastcall IsSharedObject();
	virtual bool __fastcall IsBlobDataType(Data::Db::TFieldType DataType);
	bool __fastcall IsBlob();
	HIDESBASE virtual Data::Db::TFieldType __fastcall GetDataType();
	HIDESBASE virtual void __fastcall SetDataType(Data::Db::TFieldType Value);
	virtual int __fastcall GetSize();
	virtual void __fastcall SetSize(int Value);
	HIDESBASE virtual System::UnicodeString __fastcall GetAsString();
	HIDESBASE virtual void __fastcall SetAsString(const System::UnicodeString Value);
	HIDESBASE virtual System::AnsiString __fastcall GetAsAnsiString();
	HIDESBASE virtual void __fastcall SetAsAnsiString(const System::AnsiString Value);
	HIDESBASE virtual System::WideString __fastcall GetAsWideString();
	HIDESBASE virtual void __fastcall SetAsWideString(const System::WideString Value);
	HIDESBASE virtual System::DynamicArray<System::Byte> __fastcall GetAsBytes();
	HIDESBASE virtual void __fastcall SetAsBytes(const System::DynamicArray<System::Byte> Value);
	HIDESBASE virtual int __fastcall GetAsInteger();
	HIDESBASE virtual void __fastcall SetAsInteger(int Value);
	HIDESBASE virtual void __fastcall SetAsSmallInt(int Value);
	HIDESBASE virtual void __fastcall SetAsWord(int Value);
	HIDESBASE virtual double __fastcall GetAsFloat();
	HIDESBASE virtual void __fastcall SetAsFloat(double Value);
	HIDESBASE virtual __int64 __fastcall GetAsLargeInt();
	HIDESBASE virtual void __fastcall SetAsLargeInt(const __int64 Value);
	HIDESBASE virtual void __fastcall SetAsShortInt(int Value);
	HIDESBASE virtual void __fastcall SetAsByte(int Value);
	HIDESBASE virtual void __fastcall SetAsBlob(const System::DynamicArray<System::Byte> Value);
	HIDESBASE virtual void __fastcall SetAsMemo(const System::UnicodeString Value);
	virtual Memdata::TBlob* __fastcall GetAsBlobRef();
	virtual void __fastcall SetAsBlobRef(Memdata::TBlob* const Value);
	virtual Memdata::TBlob* __fastcall GetAsMemoRef();
	virtual void __fastcall SetAsMemoRef(Memdata::TBlob* const Value);
	HIDESBASE virtual System::Variant __fastcall GetAsVariant();
	HIDESBASE virtual void __fastcall SetAsVariant(const System::Variant &Value);
	HIDESBASE virtual Data::Sqltimst::TSQLTimeStamp __fastcall GetAsSQLTimeStamp();
	HIDESBASE virtual void __fastcall SetAsSQLTimeStamp(const Data::Sqltimst::TSQLTimeStamp &Value);
	Craccess::TCRCursor* __fastcall GetAsCursor();
	void __fastcall SetAsCursor(Craccess::TCRCursor* Value);
	HIDESBASE virtual void __fastcall SetText(const System::UnicodeString Value);
	HIDESBASE virtual bool __fastcall GetIsNull();
	void __fastcall SetIsNull(bool Value);
	virtual void __fastcall SetNational(bool Value);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadExtDataType(System::Classes::TReader* Reader);
	void __fastcall WriteExtDataType(System::Classes::TWriter* Writer);
	virtual Memdata::TSharedObject* __fastcall CreateObject();
	virtual void __fastcall FreeObject();
	HIDESBASE void __fastcall AssignParam(Data::Db::TParam* Param);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall CheckArrayType(Data::Db::TFieldType DataType);
	virtual void __fastcall FreeValues();
	virtual int __fastcall GetValueCount();
	virtual void __fastcall SetValueCount(int Value);
	virtual TDAParamValueClass __fastcall GetParamValueClass();
	TDAParamValue* __fastcall GetParamValue(int Index);
	__classmethod Data::Db::TFieldType __fastcall GetVarType(System::Word VarType);
	__property Memdata::TSharedObject* ParamObject = {read=GetParamObject, write=SetParamObject};
	__property System::Word SubDataType = {read=FSubDataType, write=FSubDataType, nodefault};
	__property bool National = {read=FNational, write=SetNational, nodefault};
	__property Craccess::TCRCursor* AsCursor = {read=GetAsCursor, write=SetAsCursor};
	
public:
	__fastcall virtual TDAParam(System::Classes::TCollection* Collection)/* overload */;
	__fastcall virtual ~TDAParam();
	HIDESBASE virtual void __fastcall Clear();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE void __fastcall AssignField(Data::Db::TField* Field);
	HIDESBASE virtual void __fastcall AssignFieldValue(Data::Db::TField* Field, const System::Variant &Value);
	HIDESBASE void __fastcall LoadFromFile(const System::UnicodeString FileName, Data::Db::TBlobType BlobType);
	HIDESBASE virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream, Data::Db::TBlobType BlobType);
	HIDESBASE void __fastcall SetBlobData(void * Buffer, int Size)/* overload */;
	HIDESBASE void __fastcall SetBlobData(System::DynamicArray<System::Byte> Buffer)/* overload */;
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	__property System::AnsiString AsAnsiString = {read=GetAsAnsiString, write=SetAsAnsiString};
	__property System::WideString AsWideString = {read=GetAsWideString, write=SetAsWideString};
	__property System::DynamicArray<System::Byte> AsBytes = {read=GetAsBytes, write=SetAsBytes};
	__property int AsInteger = {read=GetAsInteger, write=SetAsInteger, nodefault};
	__property int AsSmallInt = {read=GetAsInteger, write=SetAsSmallInt, nodefault};
	__property int AsWord = {read=GetAsInteger, write=SetAsWord, nodefault};
	__property double AsFloat = {read=GetAsFloat, write=SetAsFloat};
	__property __int64 AsLargeInt = {read=GetAsLargeInt, write=SetAsLargeInt};
	__property int AsShortInt = {read=GetAsInteger, write=SetAsShortInt, nodefault};
	__property int AsByte = {read=GetAsInteger, write=SetAsByte, nodefault};
	__property System::DynamicArray<System::Byte> AsBlob = {read=GetAsBytes, write=SetAsBlob};
	__property System::UnicodeString AsMemo = {read=GetAsString, write=SetAsMemo};
	__property Memdata::TBlob* AsBlobRef = {read=GetAsBlobRef, write=SetAsBlobRef};
	__property Memdata::TBlob* AsMemoRef = {read=GetAsMemoRef, write=SetAsMemoRef};
	__property Data::Sqltimst::TSQLTimeStamp AsSQLTimeStamp = {read=GetAsSQLTimeStamp, write=SetAsSQLTimeStamp};
	__property bool IsNull = {read=GetIsNull, nodefault};
	__property System::UnicodeString Text = {read=GetAsString, write=SetText};
	__property TDAParamValue* Values[int Index] = {read=GetParamValue/*, default*/};
	__property int ValueCount = {read=GetValueCount, write=SetValueCount, nodefault};
	
__published:
	__property Data::Db::TFieldType DataType = {read=GetDataType, write=SetDataType, stored=IsDataTypeStored, nodefault};
	__property ParamType = {default=0};
	__property int Size = {read=GetSize, write=SetSize, default=0};
	__property System::Variant Value = {read=GetAsVariant, write=SetAsVariant, stored=IsValueStored};
public:
	/* TParam.Create */ inline __fastcall TDAParam(Data::Db::TParams* AParams, Data::Db::TParamType AParamType)/* overload */ : Data::Db::TParam(AParams, AParamType) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  SetBlobData(System::DynamicArray<System::Byte> Buffer, int Size){ Data::Db::TParam::SetBlobData(Buffer, Size); }
	
};


enum DECLSPEC_DENUM TParamsChangeType : unsigned char { ctGenerated, ctUsers, ctUserChecked };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAParams : public Data::Db::TParams
{
	typedef Data::Db::TParams inherited;
	
public:
	TDAParam* operator[](int Index) { return this->Items[Index]; }
	
private:
	HIDESBASE TDAParam* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TDAParam* Value);
	int __fastcall GetValueCount();
	void __fastcall SetValueCount(int Value);
	
protected:
	System::Classes::TPersistent* FOwner;
	bool FNeedsUpdateItem;
	TParamsChangeType FParamsChangeType;
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual TCustomDAConnection* __fastcall GetConnection();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	void __fastcall Disconnect();
	
public:
	__fastcall TDAParams(System::Classes::TPersistent* Owner)/* overload */;
	HIDESBASE TDAParam* __fastcall ParamByName(const System::UnicodeString Value);
	HIDESBASE TDAParam* __fastcall FindParam(const System::UnicodeString Value);
	HIDESBASE TDAParam* __fastcall CreateParam(Data::Db::TFieldType FldType, const System::UnicodeString ParamName, Data::Db::TParamType ParamType);
	__property TDAParam* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property int ValueCount = {read=GetValueCount, write=SetValueCount, nodefault};
public:
	/* TParams.Create */ inline __fastcall TDAParams()/* overload */ : Data::Db::TParams() { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TDAParams() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TDACursorField : public Data::Db::TField
{
	typedef Data::Db::TField inherited;
	
private:
	Craccess::TCRCursor* __fastcall GetAsCursor();
	
protected:
	bool __fastcall GetValue(/* out */ Craccess::TCRCursor* &Value);
	
public:
	__fastcall virtual TDACursorField(System::Classes::TComponent* Owner);
	__property Craccess::TCRCursor* AsCursor = {read=GetAsCursor};
public:
	/* TField.Destroy */ inline __fastcall virtual ~TDACursorField() { }
	
};


enum DECLSPEC_DENUM TLockMode : unsigned char { lmNone, lmPessimistic, lmOptimistic };

typedef System::DynamicArray<Data::Db::TField*> TFieldArray;

enum DECLSPEC_DENUM TLockTrStarted : unsigned char { ltsNone, ltsOnLock, ltsOnLockCachedUpdates, ltsBeforeLockCachedUpdates };

enum DECLSPEC_DENUM TStatementType : unsigned char { stQuery, stInsert, stUpdate, stDelete, stLock, stRefresh, stCustom, stRefreshQuick, stRefreshCheckDeleted, stBatchUpdate, stRecCount };

typedef System::Set<TStatementType, TStatementType::stQuery, TStatementType::stRecCount> TStatementTypes;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADataSetUpdater : public Memds::TDataSetUpdater
{
	typedef Memds::TDataSetUpdater inherited;
	
private:
	System::Classes::TComponent* FUpdateQuery;
	bool FIsUsedIndexNameForFields;
	void __fastcall CopyRecBufToActiveRecord(Memdata::TData* SrcRecordSet, System::PByte SrcRecBuf, const TStatementTypes StatementTypes, /* out */ bool &RecordWasChanged);
	void __fastcall UpdateActiveRecordFromParams();
	void __fastcall UpdateActiveRecord(const TStatementTypes StatementTypes);
	void __fastcall GetUQFields(const Craccess::TFieldDescArray KeyFieldDescs, const TFieldArray KeyFields, /* out */ TFieldArray &KeyFieldsUQ);
	void __fastcall CheckDeletedRecords();
	void __fastcall RefreshQuickDataSet();
	
protected:
	TCustomDADataSet* FDataSet;
	TDADataSetService* FDataSetService;
	TDATransaction* FFixedTransaction;
	TLockTrStarted FLockTrStarted;
	System::StaticArray<System::Classes::TComponent*, 11> FUpdateComponents;
	Craccess::TDAParamsInfo* FParamsInfo;
	Clrclasses::WideStringBuilder* FBatchSQLs;
	TDAParams* FBatchParams;
	int FBatchStatements;
	bool FRefreshInUpdate;
	bool FOptionsForAutoGeneratedSQL;
	TCheckMode FCheckMode;
	virtual bool __fastcall UseParamType(TDAParam* Param);
	virtual void __fastcall SetUpdateQuery(System::Classes::TComponent* Value);
	virtual void __fastcall SetDefaultParamType(TDAParam* Param);
	virtual bool __fastcall NeedReturnParams();
	virtual bool __fastcall ReturnParamsAsFields();
	bool __fastcall UseSQLGeneratorParams(const TStatementTypes StatementTypes);
	virtual System::UnicodeString __fastcall GetSQLSeparator();
	virtual bool __fastcall RefreshAfterInsertAllowed();
	virtual bool __fastcall IsNeedInsertPreconnect();
	virtual bool __fastcall IsNeedEditPreconnect();
	virtual bool __fastcall IsPreconnected();
	virtual bool __fastcall RefreshByLockAllowed();
	bool __fastcall CanRefreshByLock();
	Craccess::TCRCommand* __fastcall GetICommand();
	Craccess::TCRRecordSet* __fastcall GetIRecordSet();
	void __fastcall CheckIRecordSet();
	TLockMode __fastcall GetLockMode();
	TCustomDAUpdateSQL* __fastcall GetUpdateObject();
	System::UnicodeString __fastcall GetUpdateSQL(TStatementType StatementType);
	TCustomDAConnection* __fastcall UsedConnection();
	TDATransaction* __fastcall UsedTransaction();
	TDATransaction* __fastcall UsedUpdateTransaction();
	void __fastcall SetRowsAffected(int Value);
	void __fastcall BeginConnection();
	void __fastcall EndConnection();
	void __fastcall SetIdentityFieldValue();
	virtual bool __fastcall GetIdentityFieldValue(System::Variant &Value);
	System::UnicodeString __fastcall GetSavepointName(bool CachedUpdates);
	System::UnicodeString __fastcall GetLockSavepointName();
	System::UnicodeString __fastcall GetLockCachedUpdatesSavepointName();
	virtual bool __fastcall SavepointAllowed();
	virtual void __fastcall SetSavepoint(System::UnicodeString SavepointName, bool CachedUpdates);
	void __fastcall SetLockSavepoint();
	void __fastcall SetLockCachedUpdatesSavepoint();
	virtual void __fastcall RollbackToSavepoint(System::UnicodeString SavepointName, bool CachedUpdates);
	void __fastcall RollbackToLockSavepoint();
	void __fastcall RollbackToLockCachedUpdatesSavepoint();
	void __fastcall ResetLockCachedUpdatesSavepoint();
	virtual Data::Db::TField* __fastcall FieldByParamName(System::UnicodeString &ParamName, /* out */ bool &Old, /* out */ int &AFieldNo, /* out */ bool &Master);
	virtual System::UnicodeString __fastcall GetUpdateStatement(const TStatementType StatementType);
	virtual void __fastcall CheckUpdateQuery(const TStatementType StatementType);
	virtual void __fastcall SetUpdateQueryOptions(const TStatementType StatementType, const bool IsAutoGeneratedSQL);
	virtual void __fastcall CheckUpdateSQL(const System::UnicodeString SQL, const TStatementTypes StatementTypes, bool UseGenerator = true);
	virtual void __fastcall UpdateExecute(const TStatementTypes StatementTypes);
	virtual bool __fastcall IsRefreshQuickField(Memdata::TFieldDesc* FieldDesc);
	virtual void __fastcall SaveMaxRefreshQuickValue(Memdata::TFieldDesc* FieldDesc, const System::Variant &Value);
	virtual void __fastcall PrepareAppend();
	virtual void __fastcall PrepareUpdate();
	virtual void __fastcall PrepareDelete();
	virtual void __fastcall UnPrepareAppendUpdateDelete();
	virtual void __fastcall PrepareCachedUpdate();
	virtual void __fastcall FinishCachedUpdate();
	virtual void __fastcall UnPrepareCachedUpdate();
	virtual void __fastcall UnLockCachedUpdate();
	virtual bool __fastcall PerformLock();
	virtual bool __fastcall PerformUnLock();
	void __fastcall EndUpdate(bool Success);
	virtual bool __fastcall PerformAppend();
	virtual bool __fastcall PerformUpdateDelete(const TStatementType StatementType);
	virtual bool __fastcall PerformDelete();
	virtual bool __fastcall PerformUpdate();
	virtual bool __fastcall PerformRefreshRecord();
	virtual bool __fastcall PerformRefreshRecordInUpdate();
	bool __fastcall PerformRefreshQuick(bool CheckDeleted);
	bool __fastcall PerformPSUpdateRecord(Data::Db::TUpdateKind UpdateKind, Data::Db::TDataSet* Delta);
	virtual bool __fastcall CacheChanged();
	virtual bool __fastcall CacheApplied();
	virtual bool __fastcall CacheCanceled();
	virtual bool __fastcall BatchUpdateAllowed();
	virtual bool __fastcall BatchUpdate();
	virtual bool __fastcall CanFlushBatch();
	void __fastcall ClearBatch();
	virtual void __fastcall FlushBatch();
	virtual System::UnicodeString __fastcall PrepareBatch(const System::UnicodeString SQL);
	void __fastcall UnprepareUpdateObjects();
	void __fastcall ReleaseParams(System::Classes::TComponent* UpdateQuery, bool AllParams);
	virtual bool __fastcall LockCompare(const System::Variant &Value1, const System::Variant &Value2);
	__property System::Classes::TComponent* UpdateQuery = {read=FUpdateQuery, write=SetUpdateQuery};
	
public:
	__fastcall virtual TDADataSetUpdater(Memds::TDataSetService* AOwner);
	__fastcall virtual ~TDADataSetUpdater();
	System::Variant __fastcall SelectDbValue(const System::UnicodeString OperationName, const System::UnicodeString SQL);
	virtual bool __fastcall GetDefaultExpressionValue(System::UnicodeString DefExpr, /* out */ System::Variant &Value);
	void __fastcall WriteUQParams(const TStatementTypes StatementTypes);
	virtual bool __fastcall PerformSQL(const System::UnicodeString SQL, const TStatementTypes StatementTypes);
	__property TCheckMode CheckMode = {read=FCheckMode, write=FCheckMode, nodefault};
	__property Craccess::TDAParamsInfo* ParamsInfo = {read=FParamsInfo};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADataSetService : public Memds::TDataSetService
{
	typedef Memds::TDataSetService inherited;
	
protected:
	TCustomDADataSet* FDataSet;
	TDADataSetUpdater* FUpdater;
	Dasqlgenerator::TDASQLGenerator* FSQLGenerator;
	bool FFieldsExtInited;
	bool FIsAnyFieldCanBeModified;
	virtual void __fastcall SetDataSetUpdater(Memds::TDataSetUpdater* Value);
	virtual void __fastcall CreateSQLGenerator();
	void __fastcall FreeSQLGenerator();
	virtual void __fastcall SetSQLGenerator(Dasqlgenerator::TDASQLGenerator* Value);
	virtual void __fastcall PreInitCursor();
	virtual void __fastcall InitCursor();
	virtual void __fastcall CloseCursor();
	void __fastcall InitUpdatingTable();
	virtual void __fastcall InitFieldsOptions();
	virtual void __fastcall UpdateFieldsOptions();
	virtual void __fastcall SetFieldOrigin(Data::Db::TField* Field, Craccess::TCRFieldDesc* FieldDesc);
	virtual void __fastcall FillFieldsDefaultValues();
	virtual void __fastcall SetFieldsReadOnly();
	void __fastcall SetFieldsReadOnlyOld();
	Craccess::TCRFieldDesc* __fastcall GetIdentityField();
	Craccess::TCRFieldDesc* __fastcall GetKeyGeneratorField();
	virtual TFieldArray __fastcall DetectHiddenFields();
	virtual bool __fastcall DetectCanModify();
	virtual int __fastcall GetRecCount();
	virtual void __fastcall BreakExec();
	virtual bool __fastcall Executing();
	virtual System::UnicodeString __fastcall GetCurrentSchema();
	virtual void __fastcall InitMasterParams(TDAParams* Params);
	virtual void __fastcall WriteFieldXMLAttributeType(Data::Db::TField* Field, Memdata::TFieldDesc* FieldDesc, const System::UnicodeString FieldAlias, Crxml::XmlTextWriter* XMLWriter);
	Craccess::TCRConnection* __fastcall GetIConnection();
	Craccess::TCRCommand* __fastcall GetICommand();
	Craccess::TCRRecordSet* __fastcall GetIRecordSet();
	void __fastcall CheckIRecordSet();
	TCustomDAConnection* __fastcall UsedConnection();
	bool __fastcall IsDMLRefresh();
	bool __fastcall IsAutoCommit();
	bool __fastcall IsFetchAll();
	void __fastcall SetAutoCommit(bool Value);
	void __fastcall SetNeedAddRef(bool Value);
	void __fastcall BeginConnection();
	void __fastcall EndConnection();
	
public:
	__fastcall virtual TDADataSetService(Memds::TMemDataSet* AOwner);
	__fastcall virtual ~TDADataSetService();
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	void __fastcall ClearFieldDescRefs();
	virtual void __fastcall ResetTableKeyFields();
	virtual System::UnicodeString __fastcall GetDBKeyList(const System::UnicodeString TableName, const System::UnicodeString IndexName);
	virtual bool __fastcall OpenNext();
	virtual bool __fastcall NeedParamValuesOnPrepare();
	void __fastcall SetKeyGeneratorValue(const System::Variant &Value);
	__property Craccess::TCRFieldDesc* IdentityField = {read=GetIdentityField};
	__property Craccess::TCRFieldDesc* KeyGeneratorField = {read=GetKeyGeneratorField};
	__property TDADataSetUpdater* Updater = {read=FUpdater};
	__property Dasqlgenerator::TDASQLGenerator* SQLGenerator = {read=FSQLGenerator};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDASQLGeneratorService : public Dasqlgenerator::TSQLGeneratorService
{
	typedef Dasqlgenerator::TSQLGeneratorService inherited;
	
protected:
	TCustomDADataSet* FDataSet;
	
public:
	virtual void __fastcall RaiseError(const System::UnicodeString Message);
	virtual void * __fastcall GetOldRecBuf();
	virtual void * __fastcall GetNewRecBuf();
	virtual bool __fastcall BlobFieldModified(Craccess::TCRFieldDesc* FieldDesc);
	virtual Memdata::TSharedObject* __fastcall GetFieldObject(Memdata::TFieldDesc* FieldDesc);
	virtual Craccess::TCRCommand* __fastcall GetUpdateCommand();
	virtual System::UnicodeString __fastcall GetDBKeyList(const System::UnicodeString TableName, const System::UnicodeString IndexName);
	virtual bool __fastcall ParamExists(const System::UnicodeString ParamName);
	virtual System::UnicodeString __fastcall BaseSQL();
	virtual System::UnicodeString __fastcall FinalSQL();
	virtual System::UnicodeString __fastcall FilterSQL();
	__property TCustomDADataSet* DataSet = {read=FDataSet};
public:
	/* TSQLGeneratorService.Create */ inline __fastcall virtual TDASQLGeneratorService() : Dasqlgenerator::TSQLGeneratorService() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDASQLGeneratorService() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TRefreshOption : unsigned char { roAfterInsert, roAfterUpdate, roBeforeEdit };

typedef System::Set<TRefreshOption, TRefreshOption::roAfterInsert, TRefreshOption::roBeforeEdit> TRefreshOptions;

typedef void __fastcall (__closure *TBeforeExecuteEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TAfterExecuteEvent)(System::TObject* Sender, bool Result);

typedef void __fastcall (__closure *TUpdateExecuteEvent)(Data::Db::TDataSet* Sender, TStatementTypes StatementTypes, TDAParams* Params);

typedef void __fastcall (__closure *TBeforeFetchEvent)(TCustomDADataSet* DataSet, bool &Cancel);

typedef void __fastcall (__closure *TAfterFetchEvent)(TCustomDADataSet* DataSet);

struct DECLSPEC_DRECORD TQuickOpenInfo
{
public:
	bool OldActive;
	bool OldDebug;
	bool OldFetchAll;
	int OldFetchRows;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDACondition : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FValue;
	bool FEnabled;
	void __fastcall SetValue(const System::UnicodeString Value);
	void __fastcall SetEnabled(bool Value);
	void __fastcall SetName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall virtual TDACondition(System::Classes::TCollection* Collection);
	void __fastcall Enable();
	void __fastcall Disable();
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property System::UnicodeString Value = {read=FValue, write=SetValue};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TDACondition() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAConditions : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TDACondition* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	System::UnicodeString FBaseSQL;
	bool FEnabled;
	System::UnicodeString FCurrentEnabledText;
	HIDESBASE TDACondition* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TDACondition* Value);
	System::UnicodeString __fastcall GetWhereSQL();
	void __fastcall RestoreBaseSQL();
	void __fastcall AssignParams(TDAParams* ParamsSrc, TDAParams* ParamsDst);
	void __fastcall SetText(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetText();
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TDAConditions(System::Classes::TPersistent* Owner);
	int __fastcall IndexOf(const System::UnicodeString Name);
	TDACondition* __fastcall Find(const System::UnicodeString Name);
	TDACondition* __fastcall Get(const System::UnicodeString Name);
	HIDESBASE TDACondition* __fastcall Add(const System::UnicodeString Name, const System::UnicodeString Value, bool Enabled = true)/* overload */;
	HIDESBASE TDACondition* __fastcall Add(const System::UnicodeString Value, bool Enabled = true)/* overload */;
	HIDESBASE void __fastcall Delete(int Index);
	void __fastcall Remove(const System::UnicodeString Name);
	HIDESBASE void __fastcall Clear();
	void __fastcall Enable();
	void __fastcall Disable();
	__property TDACondition* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property TDACondition* Condition[int Index] = {read=GetItem, write=SetItem};
	__property bool Enabled = {read=FEnabled, nodefault};
	__property System::UnicodeString WhereSQL = {read=GetWhereSQL};
	__property System::UnicodeString Text = {read=GetText, write=SetText};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TDAConditions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAEncryption : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Crencryption::TCREncryptor* FEncryptor;
	System::UnicodeString FFields;
	void __fastcall SetEncryptor(Crencryption::TCREncryptor* Value);
	void __fastcall SetFields(const System::UnicodeString Value);
	
protected:
	TCustomDADataSet* FOwner;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	bool __fastcall IsFieldEncrypted(Memdata::TFieldDesc* FieldDesc)/* overload */;
	
public:
	__fastcall TDAEncryption(TCustomDADataSet* Owner);
	bool __fastcall IsFieldEncrypted(const System::UnicodeString FieldName)/* overload */;
	bool __fastcall IsFieldEncrypted(int FieldNo)/* overload */;
	void __fastcall EncryptDataSet(bool AutoCommitExplicitTransaction = true);
	__property Crencryption::TCREncryptor* Encryptor = {read=FEncryptor, write=SetEncryptor};
	
__published:
	__property System::UnicodeString Fields = {read=FFields, write=SetFields};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDAEncryption() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADataSetOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FSetFieldsReadOnly;
	bool FRequiredFields;
	bool FStrictUpdate;
	bool FNumberRange;
	bool FQueryRecCount;
	bool FAutoPrepare;
	bool FReturnParams;
	bool FTrimFixedChar;
	bool FTrimVarChar;
	bool FSetEmptyStrToNull;
	bool FLongStrings;
	bool FRemoveOnRefresh;
	bool FFlatBuffers;
	bool FQuoteNames;
	Memdata::TCompressBlobMode FCompressBlobMode;
	bool FFullRefresh;
	bool FLocalMasterDetail;
	Craccess::TFieldOrigins FFieldOrigins;
	bool FDefaultValues;
	bool FExtendedFieldsInfo;
	int FUpdateBatchSize;
	bool FUpdateAllFields;
	bool FPrepareUpdateSQL;
	bool FEnableBCD;
	bool FEnableFMTBCD;
	bool FMasterFieldsNullable;
	bool FInsertAllSetFields;
	void __fastcall SetSetFieldsReadOnly(bool Value);
	void __fastcall SetFullRefresh(bool Value);
	void __fastcall SetRequiredFields(bool Value);
	void __fastcall SetNumberRange(bool Value);
	void __fastcall SetTrimFixedChar(bool Value);
	void __fastcall SetTrimVarChar(bool Value);
	void __fastcall SetSetEmptyStrToNull(bool Value);
	void __fastcall SetLongStrings(bool Value);
	void __fastcall SetAutoPrepare(bool Value);
	void __fastcall SetFlatBuffers(bool Value);
	int __fastcall GetDetailDelay();
	void __fastcall SetDetailDelay(int Value);
	void __fastcall SetCompressBlobMode(Memdata::TCompressBlobMode Value);
	void __fastcall SetLocalMasterDetail(bool Value);
	bool __fastcall GetCacheCalcFields();
	void __fastcall SetCacheCalcFields(bool Value);
	void __fastcall SetQuoteNames(bool Value);
	void __fastcall SetFieldOrigins(Craccess::TFieldOrigins Value);
	bool __fastcall GetFieldsOrigin _DEPRECATED_ATTRIBUTE0 ();
	void __fastcall SetFieldsOrigin _DEPRECATED_ATTRIBUTE0 (bool Value);
	void __fastcall SetDefaultValues(bool Value);
	void __fastcall SetExtendedFieldsInfo(bool Value);
	void __fastcall SetEnableBCD(bool Value);
	void __fastcall SetEnableFMTBCD(bool Value);
	void __fastcall SetMasterFieldsNullable(bool Value);
	void __fastcall SetInsertAllSetFields(bool Value);
	
protected:
	TCustomDADataSet* FOwner;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadFieldsOriginal(System::Classes::TReader* Reader);
	__property bool FullRefresh = {read=FFullRefresh, write=SetFullRefresh, default=0};
	__property bool TrimVarChar = {read=FTrimVarChar, write=SetTrimVarChar, default=0};
	__property bool SetEmptyStrToNull = {read=FSetEmptyStrToNull, write=SetSetEmptyStrToNull, default=0};
	__property bool ExtendedFieldsInfo = {read=FExtendedFieldsInfo, write=SetExtendedFieldsInfo, default=0};
	__property bool EnableBCD = {read=FEnableBCD, write=SetEnableBCD, default=0};
	__property bool EnableFMTBCD = {read=FEnableFMTBCD, write=SetEnableFMTBCD, default=0};
	
public:
	__fastcall TDADataSetOptions(TCustomDADataSet* Owner);
	__property bool SetFieldsReadOnly = {read=FSetFieldsReadOnly, write=SetSetFieldsReadOnly, default=1};
	__property bool RequiredFields = {read=FRequiredFields, write=SetRequiredFields, default=1};
	__property bool StrictUpdate = {read=FStrictUpdate, write=FStrictUpdate, default=1};
	__property bool PrepareUpdateSQL = {read=FPrepareUpdateSQL, write=FPrepareUpdateSQL, default=0};
	__property bool NumberRange = {read=FNumberRange, write=SetNumberRange, default=0};
	__property bool QueryRecCount = {read=FQueryRecCount, write=FQueryRecCount, default=0};
	__property bool AutoPrepare = {read=FAutoPrepare, write=SetAutoPrepare, default=0};
	__property bool ReturnParams = {read=FReturnParams, write=FReturnParams, default=0};
	__property bool TrimFixedChar = {read=FTrimFixedChar, write=SetTrimFixedChar, default=1};
	__property bool LongStrings = {read=FLongStrings, write=SetLongStrings, default=1};
	__property bool FlatBuffers = {read=FFlatBuffers, write=SetFlatBuffers, default=0};
	__property bool RemoveOnRefresh = {read=FRemoveOnRefresh, write=FRemoveOnRefresh, default=1};
	__property bool QuoteNames = {read=FQuoteNames, write=SetQuoteNames, default=0};
	__property int DetailDelay = {read=GetDetailDelay, write=SetDetailDelay, default=0};
	__property Memdata::TCompressBlobMode CompressBlobMode = {read=FCompressBlobMode, write=SetCompressBlobMode, default=0};
	__property bool LocalMasterDetail = {read=FLocalMasterDetail, write=SetLocalMasterDetail, default=0};
	__property bool CacheCalcFields = {read=GetCacheCalcFields, write=SetCacheCalcFields, default=0};
	__property bool FieldsOrigin = {read=GetFieldsOrigin, write=SetFieldsOrigin, nodefault};
	__property Craccess::TFieldOrigins FieldOrigins = {read=FFieldOrigins, write=SetFieldOrigins, default=0};
	__property bool DefaultValues = {read=FDefaultValues, write=SetDefaultValues, default=0};
	__property int UpdateBatchSize = {read=FUpdateBatchSize, write=FUpdateBatchSize, default=1};
	__property bool UpdateAllFields = {read=FUpdateAllFields, write=FUpdateAllFields, default=0};
	__property bool MasterFieldsNullable = {read=FMasterFieldsNullable, write=SetMasterFieldsNullable, default=0};
	__property bool InsertAllSetFields = {read=FInsertAllSetFields, write=SetInsertAllSetFields, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDADataSetOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSmartFetchOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	bool FLiveBlock;
	System::UnicodeString FPrefetchedFields;
	System::Classes::TStrings* FSQLGetKeyValues;
	System::Classes::TStrings* FSQLGetDataValues;
	void __fastcall SetEnabled(bool Value);
	void __fastcall SetLiveBlock(bool Value);
	void __fastcall SetPrefetchedFields(const System::UnicodeString Value);
	void __fastcall SetSQLGetKeyValues(System::Classes::TStrings* Value);
	
protected:
	TCustomDADataSet* FOwner;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TSmartFetchOptions(TCustomDADataSet* Owner);
	__fastcall virtual ~TSmartFetchOptions();
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=0};
	__property bool LiveBlock = {read=FLiveBlock, write=SetLiveBlock, default=1};
	__property System::UnicodeString PrefetchedFields = {read=FPrefetchedFields, write=SetPrefetchedFields};
	__property System::Classes::TStrings* SQLGetKeyValues = {read=FSQLGetKeyValues, write=SetSQLGetKeyValues};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCustomDADataSet : public Memds::TMemDataSet
{
	typedef Memds::TMemDataSet inherited;
	
private:
	TCustomDAConnection* FConnection;
	TDATransaction* FTransaction;
	TDATransaction* FUpdateTransaction;
	TDAParams* FParams;
	TMacros* FMacros;
	int FFetchRows;
	bool FDebug;
	bool FReadOnly;
	bool FUniDirectional;
	bool FAutoCommit;
	TCustomDAUpdateSQL* FUpdateObject;
	TRefreshOptions FRefreshOptions;
	TDADataSetOptions* FOptions;
	TSmartFetchOptions* FSmartFetchOptions;
	System::UnicodeString FBaseSQL;
	TLockMode FLockMode;
	System::UnicodeString FKeyFields;
	bool FDMLRefresh;
	Memdata::TLocateExOptions FFindKeyOptions;
	bool FDisconnected;
	TCheckMode FCheckMode;
	TDAMapRules* FDataTypeMap;
	TDAEncryption* FEncryption;
	TDAConditions* FWhereConditions;
	TBeforeExecuteEvent FBeforeExecute;
	TAfterExecuteEvent FAfterExecute;
	TBeforeFetchEvent FBeforeFetch;
	TAfterFetchEvent FAfterFetch;
	TUpdateExecuteEvent FBeforeUpdateExecute;
	TUpdateExecuteEvent FAfterUpdateExecute;
	bool __fastcall IsMapRulesStored();
	void __fastcall SetUpdateTransaction(TDATransaction* Value);
	System::Classes::TStrings* __fastcall GetSQL();
	void __fastcall SetSQL(System::Classes::TStrings* Value);
	void __fastcall SetFetchRows(int Value);
	TDAParams* __fastcall GetParams();
	void __fastcall SetParams(TDAParams* Value);
	System::Word __fastcall GetParamCount();
	bool __fastcall GetParamCheck();
	void __fastcall SetParamCheck(bool Value);
	TMacros* __fastcall GetMacros();
	void __fastcall SetMacros(TMacros* Value);
	System::Word __fastcall GetMacroCount();
	int __fastcall GetRowsAffected();
	int __fastcall GetParamsProcessed();
	HIDESBASE void __fastcall SetUniDirectional(bool Value);
	void __fastcall SetAutoCommit(bool Value);
	void __fastcall SetUpdateObject(TCustomDAUpdateSQL* Value);
	void __fastcall SetOptions(TDADataSetOptions* Value);
	void __fastcall SetSmartFetchOptions(TSmartFetchOptions* Value);
	void __fastcall SaveModifiedSQL(const System::UnicodeString NewSQL);
	System::UnicodeString __fastcall GetBaseSQL();
	void __fastcall SetEncryption(TDAEncryption* Value);
	void __fastcall SetCheckMode(TCheckMode Value);
	int __fastcall InternalPSExecuteStatement(const System::UnicodeString ASQL, Data::Db::TParams* AParams, TCustomDADataSet* Query);
	
protected:
	System::UnicodeString FOldKeyFields;
	System::UnicodeString FOldTableName;
	virtual bool __fastcall PSInTransaction();
	virtual void __fastcall PSStartTransaction();
	virtual void __fastcall PSEndTransaction(bool Commit);
	virtual void __fastcall PSExecute();
	virtual int __fastcall PSExecuteStatement(const System::UnicodeString ASQL, Data::Db::TParams* AParams)/* overload */;
	virtual int __fastcall PSExecuteStatement(const System::UnicodeString ASQL, Data::Db::TParams* AParams, Data::Db::TDataSet* &ResultSet)/* overload */;
	virtual int __fastcall PSExecuteStatement(const System::UnicodeString ASQL, Data::Db::TParams* AParams, void * ResultSet = (void *)(0x0))/* overload */;
	virtual Data::Db::TParams* __fastcall PSGetParams();
	virtual System::UnicodeString __fastcall PSGetQuoteChar();
	virtual System::UnicodeString __fastcall PSGetTableName();
	virtual bool __fastcall PSIsSQLBased();
	virtual bool __fastcall PSIsSQLSupported();
	virtual void __fastcall PSReset();
	virtual void __fastcall PSSetParams(Data::Db::TParams* AParams);
	virtual void __fastcall PSSetCommandText(const System::UnicodeString CommandText);
	virtual bool __fastcall PSUpdateRecord(Data::Db::TUpdateKind UpdateKind, Data::Db::TDataSet* Delta);
	virtual Data::Db::TIndexDef* __fastcall PSGetDefaultOrder();
	virtual System::UnicodeString __fastcall PSGetKeyFields();
	void __fastcall PSDetectKeyFields(Data::Db::TDataSet* DataSet);
	void __fastcall ReadConditions(System::Classes::TReader* Reader);
	void __fastcall WriteConditions(System::Classes::TWriter* Writer);
	Craccess::TCRRecordSet* FIRecordSet;
	Craccess::TCRCommand* FICommand;
	TCustomDASQL* FCommand;
	TDADataSetService* FDataSetService;
	System::UnicodeString FFilterSQL;
	System::UnicodeString FUpdatingTable;
	bool FDesignCreate;
	bool FNonBlocking;
	bool FLockDebug;
	System::StaticArray<System::Classes::TStrings*, 11> FUpdateSQL;
	int FRowsAffected;
	int FRecordCount;
	__int64 FLastInsertId;
	int FParamsProcessed;
	bool FFetchAll;
	bool FFetchCanceled;
	bool FStreamedOpen;
	Memdata::TSharedObject* __fastcall GetFieldObject(Data::Db::TField* Field)/* overload */;
	Memdata::TSharedObject* __fastcall GetFieldObject(Data::Db::TField* Field, System::PByte RecBuf)/* overload */;
	Memdata::TSharedObject* __fastcall GetFieldObject(Memdata::TFieldDesc* FieldDesc)/* overload */;
	Memdata::TSharedObject* __fastcall GetFieldObject(Memdata::TFieldDesc* FieldDesc, System::PByte RecBuf)/* overload */;
	virtual void __fastcall CheckActive();
	virtual void __fastcall CheckInactive();
	virtual void __fastcall CreateIRecordSet();
	HIDESBASE void __fastcall FreeIRecordSet();
	virtual void __fastcall SetIRecordSet(Memdata::TData* Value);
	void __fastcall CheckIRecordSet();
	virtual void __fastcall SetIndexFieldNames(const System::UnicodeString Value);
	virtual void __fastcall CreateCommand();
	void __fastcall FreeCommand();
	void __fastcall SetCommand(TCustomDASQL* Value);
	virtual TDADataSetMapRules* __fastcall CreateDataTypeMap();
	void __fastcall SetDataTypeMap(TDAMapRules* Value);
	virtual void __fastcall SetDataSetService(Memds::TDataSetService* Value);
	virtual TDADataSetOptions* __fastcall CreateOptions();
	virtual TDAEncryption* __fastcall CreateEncryption();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetConnection(TCustomDAConnection* Value);
	virtual TCustomDAConnection* __fastcall UsedConnection();
	virtual void __fastcall CheckConnection();
	virtual void __fastcall BeginConnection(bool NoConnectCheck = true);
	virtual void __fastcall EndConnection();
	void __fastcall ConnectRequest();
	void __fastcall DisconnectRequest();
	virtual void __fastcall Disconnect(bool NeedClose = true);
	virtual void __fastcall ConnectChange(System::TObject* Sender, bool Connecting);
	bool __fastcall IsTransactionStored();
	virtual TDATransaction* __fastcall GetTransaction();
	virtual void __fastcall SetTransaction(TDATransaction* Value);
	TDATransaction* __fastcall GetUsedTransaction();
	virtual TDATransaction* __fastcall UsedTransaction();
	virtual TDATransaction* __fastcall UsedUpdateTransaction();
	virtual void __fastcall SetKeyFields(const System::UnicodeString Value);
	virtual Memds::TFieldTypeMapClass __fastcall GetFieldTypeMapClass();
	virtual Data::Db::TFieldType __fastcall GetFieldType(Memdata::TFieldDesc* FieldDesc, /* out */ int &FieldSize, /* out */ int &FieldLength, /* out */ int &FieldScale)/* overload */;
	void __fastcall FillExtFieldsInfo();
	Craccess::TCRTablesInfo* __fastcall GetTablesInfo();
	Craccess::TSQLInfo* __fastcall GetSQLInfo();
	virtual void __fastcall SetUpdatingTable(const System::UnicodeString Value);
	System::UnicodeString __fastcall QuoteName(const System::UnicodeString AName);
	System::UnicodeString __fastcall GetDBKeyList(const System::UnicodeString TableName, const System::UnicodeString IndexName);
	virtual void __fastcall SetActive(bool Value);
	virtual void __fastcall BeforeOpenCursor(bool InfoQuery);
	virtual void __fastcall OpenCursor(bool InfoQuery);
	virtual void __fastcall AfterOpenCursor(bool InfoQuery);
	virtual void __fastcall CloseCursor();
	Craccess::TCRCursor* __fastcall GetCursor();
	virtual Craccess::TCRCursor* __fastcall GetCRCursor();
	virtual void __fastcall SetCRCursor(Craccess::TCRCursor* Value);
	void __fastcall GetCurrentKeys(/* out */ Craccess::TFieldDescArray &KeyFieldDescs, /* out */ TFieldArray &KeyFields);
	void __fastcall GetCurrentValues(const Craccess::TFieldDescArray KeyFieldDescs, /* out */ System::Variant &Values);
	void __fastcall GetCurrentKeysAndValues(/* out */ Craccess::TFieldDescArray &KeyFieldDescs, /* out */ TFieldArray &KeyFields, /* out */ System::Variant &Values);
	virtual void __fastcall DataReopen();
	virtual void __fastcall InternalRefresh();
	virtual void __fastcall InternalRefreshQuick(const bool CheckDeleted);
	virtual void __fastcall InternalExecute(int Iters, int Offset);
	virtual void __fastcall InternalClose();
	virtual void __fastcall DoAfterOpen();
	void __fastcall SetDMLRefresh(bool Value);
	virtual void __fastcall SetRefreshOptions(TRefreshOptions Value);
	virtual bool __fastcall GetFetchAll();
	virtual void __fastcall SetFetchAll(bool Value);
	virtual bool __fastcall GetNonBlocking();
	virtual void __fastcall SetNonBlocking(bool Value);
	virtual bool __fastcall SQLAutoGenerated();
	bool __fastcall DoOpenNext();
	virtual void __fastcall QuickOpen(TQuickOpenInfo &Info, bool Refresh = false);
	virtual void __fastcall Restore(const TQuickOpenInfo &Info, bool RestoreActive = true);
	virtual void __fastcall SetReadOnly(bool Value);
	virtual void __fastcall InternalEdit();
	virtual void __fastcall InternalDelete();
	virtual void __fastcall InternalInsert();
	virtual void __fastcall InternalCancel();
	virtual void __fastcall InternalPost();
	virtual void __fastcall InternalDeferredPost();
	virtual TStatementTypes __fastcall GetUpdateSQLStatementTypes();
	System::Classes::TStrings* __fastcall GetUpdateSQLIndex(int Index);
	void __fastcall SetUpdateSQLIndex(int Index, System::Classes::TStrings* Value);
	virtual void __fastcall SetFilterSQL(const System::UnicodeString Value);
	virtual void __fastcall SetFiltered(bool Value);
	virtual bool __fastcall LocateRecord(System::Classes::TList* KeyFields, const System::Variant &KeyValues, Memdata::TLocateExOptions Options, bool SavePos)/* overload */;
	virtual bool __fastcall GetCanModify();
	virtual void __fastcall SetStateFieldValue(Data::Db::TDataSetState State, Data::Db::TField* Field, const System::Variant &Value);
	virtual bool __fastcall CanRefreshField(Data::Db::TField* Field);
	virtual void __fastcall AssignFieldValue(TDAParam* Param, Data::Db::TField* Field, bool Old)/* overload */;
	virtual void __fastcall AssignFieldValue(TDAParam* Param, Memdata::TFieldDesc* FieldDesc, bool Old)/* overload */;
	virtual void __fastcall AssignFieldType(TDAParam* Param, Memdata::TFieldDesc* FieldDesc);
	virtual void __fastcall SetDefaultExpressionValues();
	virtual bool __fastcall UseLocalMasterDetailFilter();
	virtual bool __fastcall NeedDetailRefresh(TDAParam* Param, Memdata::TSharedObject* FieldValue);
	virtual bool __fastcall MDLinksRefreshed(Data::Db::TField* Field);
	virtual void __fastcall RefreshDetail(System::TObject* Sender);
	bool __fastcall SetMasterParams(TDAParams* AParams, Data::Db::TField* MasterField);
	virtual void __fastcall MDPropertiesChanged();
	virtual System::UnicodeString __fastcall SplitFieldName(const System::UnicodeString Fields, int &Pos);
	void __fastcall InitMasterParams();
	void __fastcall AssembleSQL();
	bool __fastcall GetForceSPInit();
	void __fastcall InternalCreateProcCall(const System::UnicodeString Name, bool NeedDescribe, bool IsQuery = false);
	virtual void __fastcall ScanMacros(System::TObject* Sender = (System::TObject*)(0x0));
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall DoBeforeExecute();
	virtual void __fastcall DoAfterExecute(bool Result);
	void __fastcall DoAfterExecFetch(bool Result);
	void __fastcall DoAfterFetchAll(bool Result);
	virtual void __fastcall DoAfterScroll();
	virtual void __fastcall DoOnBeforeFetch(bool &Cancel);
	virtual void __fastcall DoOnAfterFetch();
	void __fastcall DoOnReopen();
	void __fastcall DoOnFieldsChanged();
	System::UnicodeString __fastcall GetKeyValuesSQL();
	System::UnicodeString __fastcall GetDataValuesSQL();
	virtual int __fastcall GetRecordCount();
	virtual bool __fastcall GetIsQuery();
	virtual bool __fastcall AssignedBeforeUpdateExecute();
	virtual void __fastcall DoBeforeUpdateExecute(Data::Db::TDataSet* Sender, TStatementTypes StatementTypes, TDAParams* Params);
	virtual bool __fastcall AssignedAfterUpdateExecute();
	virtual void __fastcall DoAfterUpdateExecute(Data::Db::TDataSet* Sender, TStatementTypes StatementTypes, TDAParams* Params);
	virtual void __fastcall InternalOpen();
	virtual Crparser::TSQLParserClass __fastcall GetParserClass();
	virtual System::UnicodeString __fastcall SQLGetFrom(const System::UnicodeString SQLText);
	virtual System::UnicodeString __fastcall SQLGetWhere(const System::UnicodeString SQLText);
	virtual System::UnicodeString __fastcall SQLAddWhere(const System::UnicodeString SQLText, const System::UnicodeString Condition);
	virtual System::UnicodeString __fastcall SQLDeleteWhere(const System::UnicodeString SQLText);
	virtual System::UnicodeString __fastcall SQLGetOrderBy(const System::UnicodeString SQLText);
	virtual System::UnicodeString __fastcall SQLSetOrderBy(const System::UnicodeString SQLText, const System::UnicodeString Fields);
	virtual void __fastcall CheckSQL();
	virtual System::UnicodeString __fastcall GetFinalSQL();
	virtual void __fastcall DataEvent(Data::Db::TDataEvent Event, NativeInt Info);
	virtual bool __fastcall GetNextRecord();
	virtual bool __fastcall GetPriorRecord();
	HIDESBASE bool __fastcall LocateEx(Data::Db::TField* const *KeyFields, const int KeyFields_High, Craccess::TCRFieldDesc* const *KeyFieldDescs, const int KeyFieldDescs_High, const System::Variant &KeyValues, Memdata::TLocateExOptions Options)/* overload */;
	void __fastcall EmptyTable(const System::UnicodeString TableName);
	void __fastcall SetConditions(TDAConditions* Value);
	__property Craccess::TCRTablesInfo* TablesInfo = {read=GetTablesInfo};
	__property Craccess::TSQLInfo* SQLInfo = {read=GetSQLInfo};
	__property System::UnicodeString UpdatingTable = {read=FUpdatingTable, write=SetUpdatingTable};
	__property TDATransaction* Transaction = {read=GetTransaction, write=SetTransaction, stored=IsTransactionStored};
	__property TDATransaction* UpdateTransaction = {read=FUpdateTransaction, write=SetUpdateTransaction};
	__property bool AutoCommit = {read=FAutoCommit, write=SetAutoCommit, default=1};
	__property bool FetchAll = {read=GetFetchAll, write=SetFetchAll, default=0};
	__property bool NonBlocking = {read=GetNonBlocking, write=SetNonBlocking, default=0};
	__property TCustomDAUpdateSQL* UpdateObject = {read=FUpdateObject, write=SetUpdateObject};
	__property bool DMLRefresh = {read=FDMLRefresh, write=SetDMLRefresh, default=0};
	__property TLockMode LockMode = {read=FLockMode, write=FLockMode, default=0};
	__property Craccess::TCRCursor* Cursor = {read=GetCRCursor, write=SetCRCursor};
	__property TSmartFetchOptions* SmartFetch = {read=FSmartFetchOptions, write=SetSmartFetchOptions};
	__property TDAEncryption* Encryption = {read=FEncryption, write=SetEncryption};
	
public:
	__fastcall virtual TCustomDADataSet(System::Classes::TComponent* Owner);
	__fastcall virtual ~TCustomDADataSet();
	virtual void __fastcall Prepare();
	virtual void __fastcall UnPrepare();
	virtual void __fastcall Execute()/* overload */;
	virtual void __fastcall Execute(int Iters, int Offset = 0x0)/* overload */;
	void __fastcall ExecSQL();
	bool __fastcall Executing();
	bool __fastcall Fetching();
	bool __fastcall FetchingAll();
	virtual bool __fastcall Fetched();
	virtual void __fastcall BreakExec();
	HIDESBASE Memdata::TFieldDesc* __fastcall GetFieldDesc(const System::UnicodeString FieldName, const System::UnicodeString TableName)/* overload */;
	virtual void __fastcall GetDetailLinkFields(System::Generics::Collections::TList__1<Data::Db::TField*>* MasterFields, System::Generics::Collections::TList__1<Data::Db::TField*>* DetailFields)/* overload */;
	bool __fastcall FindKey(const System::TVarRec *KeyValues, const int KeyValues_High);
	void __fastcall FindNearest(const System::TVarRec *KeyValues, const int KeyValues_High);
	void __fastcall GotoCurrent(TCustomDADataSet* DataSet);
	virtual System::Classes::TStream* __fastcall CreateBlobStream(Data::Db::TField* Field, Data::Db::TBlobStreamMode Mode);
	virtual void __fastcall ApplyUpdates(const Memdata::TUpdateRecKinds UpdateRecKinds)/* overload */;
	__property System::Classes::TStrings* SQLInsert = {read=GetUpdateSQLIndex, write=SetUpdateSQLIndex, index=1};
	__property System::Classes::TStrings* SQLDelete = {read=GetUpdateSQLIndex, write=SetUpdateSQLIndex, index=3};
	__property System::Classes::TStrings* SQLUpdate = {read=GetUpdateSQLIndex, write=SetUpdateSQLIndex, index=2};
	__property System::Classes::TStrings* SQLRefresh = {read=GetUpdateSQLIndex, write=SetUpdateSQLIndex, index=5};
	__property System::Classes::TStrings* SQLLock = {read=GetUpdateSQLIndex, write=SetUpdateSQLIndex, index=4};
	__property System::Classes::TStrings* SQLRecCount = {read=GetUpdateSQLIndex, write=SetUpdateSQLIndex, index=10};
	void __fastcall RefreshRecord();
	virtual void __fastcall Lock();
	void __fastcall UnLock();
	TDAParam* __fastcall FindParam(const System::UnicodeString Value);
	TDAParam* __fastcall ParamByName(const System::UnicodeString Value);
	TMacro* __fastcall FindMacro(const System::UnicodeString Value);
	TMacro* __fastcall MacroByName(const System::UnicodeString Value);
	void __fastcall SaveSQL();
	void __fastcall RestoreSQL();
	bool __fastcall SQLSaved();
	void __fastcall AddWhere(const System::UnicodeString Condition);
	void __fastcall DeleteWhere();
	void __fastcall SetOrderBy(const System::UnicodeString Fields);
	System::UnicodeString __fastcall GetOrderBy();
	Data::Db::TField* __fastcall GetField(Memdata::TFieldDesc* FieldDesc);
	virtual int __fastcall GetDataType(const System::UnicodeString FieldName);
	int __fastcall GetFieldPrecision(const System::UnicodeString FieldName);
	int __fastcall GetFieldScale(const System::UnicodeString FieldName);
	Memdata::TSharedObject* __fastcall GetFieldObject(const System::UnicodeString FieldName)/* overload */;
	void __fastcall GetKeyFieldNames(System::Classes::TStrings* List);
	__property TCustomDAConnection* Connection = {read=FConnection, write=SetConnection};
	__property bool ParamCheck = {read=GetParamCheck, write=SetParamCheck, default=1};
	__property System::Classes::TStrings* SQL = {read=GetSQL, write=SetSQL};
	__property int FetchRows = {read=FFetchRows, write=SetFetchRows, default=25};
	__property bool Debug = {read=FDebug, write=FDebug, default=0};
	__property TDAParams* Params = {read=GetParams, write=SetParams, stored=false};
	__property System::Word ParamCount = {read=GetParamCount, nodefault};
	__property TMacros* Macros = {read=GetMacros, write=SetMacros, stored=false};
	__property System::Word MacroCount = {read=GetMacroCount, nodefault};
	__property bool UniDirectional = {read=FUniDirectional, write=SetUniDirectional, default=0};
	__property bool ReadOnly = {read=FReadOnly, write=SetReadOnly, default=0};
	__property int RowsAffected = {read=GetRowsAffected, nodefault};
	__property int ParamsProcessed = {read=GetParamsProcessed, nodefault};
	__property bool IsQuery = {read=GetIsQuery, nodefault};
	__property TRefreshOptions RefreshOptions = {read=FRefreshOptions, write=SetRefreshOptions, default=0};
	__property TDADataSetOptions* Options = {read=FOptions, write=SetOptions};
	__property System::UnicodeString BaseSQL = {read=GetBaseSQL};
	__property System::UnicodeString FinalSQL = {read=GetFinalSQL};
	__property System::UnicodeString FilterSQL = {read=FFilterSQL, write=SetFilterSQL};
	__property System::UnicodeString KeyFields = {read=FKeyFields, write=SetKeyFields};
	__property bool Disconnected = {read=FDisconnected, write=FDisconnected, nodefault};
	__property MasterSource;
	__property MasterFields = {default=0};
	__property DetailFields = {default=0};
	__property TDAMapRules* DataTypeMap = {read=FDataTypeMap, write=SetDataTypeMap, stored=IsMapRulesStored};
	__property TDAConditions* Conditions = {read=FWhereConditions, write=SetConditions, stored=false};
	__property TBeforeExecuteEvent BeforeExecute = {read=FBeforeExecute, write=FBeforeExecute};
	__property TAfterExecuteEvent AfterExecute = {read=FAfterExecute, write=FAfterExecute};
	__property TUpdateExecuteEvent BeforeUpdateExecute = {read=FBeforeUpdateExecute, write=FBeforeUpdateExecute};
	__property TUpdateExecuteEvent AfterUpdateExecute = {read=FAfterUpdateExecute, write=FAfterUpdateExecute};
	__property TBeforeFetchEvent BeforeFetch = {read=FBeforeFetch, write=FBeforeFetch};
	__property TAfterFetchEvent AfterFetch = {read=FAfterFetch, write=FAfterFetch};
	__property TCheckMode CheckMode = {read=FCheckMode, write=SetCheckMode, default=1};
	/* Hoisted overloads: */
	
protected:
	inline Data::Db::TFieldType __fastcall  GetFieldType(System::Word DataType){ return Memds::TMemDataSet::GetFieldType(DataType); }
	inline bool __fastcall  LocateRecord(const System::UnicodeString KeyFields, const System::Variant &KeyValues, Memdata::TLocateExOptions Options, bool SavePos){ return Memds::TMemDataSet::LocateRecord(KeyFields, KeyValues, Options, SavePos); }
	inline bool __fastcall  LocateRecord(Data::Db::TField* const *KeyFields, const int KeyFields_High, const System::Variant &KeyValues, Memdata::TLocateExOptions Options, bool SavePos){ return Memds::TMemDataSet::LocateRecord(KeyFields, KeyFields_High, KeyValues, Options, SavePos); }
	
public:
	inline bool __fastcall  LocateEx(const System::UnicodeString KeyFields, const System::Variant &KeyValues, Memdata::TLocateExOptions Options){ return Memds::TMemDataSet::LocateEx(KeyFields, KeyValues, Options); }
	inline bool __fastcall  LocateEx(Data::Db::TField* const *KeyFields, const int KeyFields_High, const System::Variant &KeyValues, Memdata::TLocateExOptions Options){ return Memds::TMemDataSet::LocateEx(KeyFields, KeyFields_High, KeyValues, Options); }
	inline Memdata::TFieldDesc* __fastcall  GetFieldDesc(Data::Db::TField* const Field){ return Memds::TMemDataSet::GetFieldDesc(Field); }
	inline Memdata::TFieldDesc* __fastcall  GetFieldDesc(const System::UnicodeString FieldName){ return Memds::TMemDataSet::GetFieldDesc(FieldName); }
	inline Memdata::TFieldDesc* __fastcall  GetFieldDesc(const int FieldNo){ return Memds::TMemDataSet::GetFieldDesc(FieldNo); }
	inline void __fastcall  GetDetailLinkFields _DEPRECATED_ATTRIBUTE1("Use overloaded method instead") (System::Classes::TList* MasterFields, System::Classes::TList* DetailFields){ Data::Db::TDataSet::GetDetailLinkFields(MasterFields, DetailFields); }
	inline void __fastcall  ApplyUpdates(){ Memds::TMemDataSet::ApplyUpdates(); }
	
};


class PASCALIMPLEMENTATION TCustomDASQL : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	System::Variant operator[](const System::UnicodeString ParamName) { return this->ParamValues[ParamName]; }
	
private:
	TCustomDAConnection* FConnection;
	TDATransaction* FTransaction;
	System::Classes::TStrings* FSQL;
	TDAParams* FParams;
	bool FParamCheck;
	TMacros* FMacros;
	bool FDebug;
	bool FChangeCursor;
	bool FSQLModified;
	int FBatchIters;
	int FBatchOffset;
	TBeforeExecuteEvent FBeforeExecute;
	TAfterExecuteEvent FAfterExecute;
	void __fastcall SetTransaction(TDATransaction* Value);
	void __fastcall SetSQL(System::Classes::TStrings* Value);
	bool __fastcall GetPrepared();
	void __fastcall SetPrepared(bool Value);
	void __fastcall SetParams(TDAParams* Value);
	System::Word __fastcall GetParamCount();
	void __fastcall SetParamCheck(bool Value);
	System::Variant __fastcall GetParamValues(const System::UnicodeString ParamName);
	void __fastcall SetParamValues(const System::UnicodeString ParamName, const System::Variant &Value);
	void __fastcall SetMacros(TMacros* Value);
	System::Word __fastcall GetMacroCount();
	int __fastcall GetRowsAffected();
	int __fastcall GetParamsProcessed();
	
protected:
	bool FAutoCommit;
	Craccess::TCRCommand* FICommand;
	TCustomDADataSet* FDataSet;
	bool FDesignCreate;
	bool FNonBlocking;
	bool FLockDebug;
	bool FLockAssembleSQL;
	bool FLockMacros;
	bool FLockScanParams;
	System::UnicodeString FStoredProcName;
	bool FStoredProcIsQuery;
	bool FIsSPInit;
	__int64 FLastInsertId;
	bool __fastcall IsTransactionStored();
	virtual void __fastcall CreateICommand();
	void __fastcall FreeICommand();
	virtual void __fastcall SetICommand(Craccess::TCRCommand* Value);
	void __fastcall CheckICommand();
	virtual TDAParams* __fastcall CreateParamsObject();
	virtual TDAFieldTypeMapClass __fastcall GetFieldTypeMapClass();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetAutoCommit(bool Value);
	virtual bool __fastcall GetNonBlocking();
	virtual void __fastcall SetNonBlocking(bool Value);
	void __fastcall SetConnection(TCustomDAConnection* Value);
	virtual TCustomDAConnection* __fastcall UsedConnection();
	virtual void __fastcall CheckConnection();
	virtual void __fastcall BeginConnection(bool NoConnectCheck = true);
	virtual void __fastcall EndConnection();
	virtual void __fastcall Disconnect(bool NeedClose = true);
	virtual void __fastcall ConnectChange(System::TObject* Sender, bool Connecting);
	virtual TDATransaction* __fastcall GetTransaction();
	virtual TDATransaction* __fastcall UsedTransaction();
	virtual void __fastcall InternalPrepare();
	virtual void __fastcall InternalUnPrepare();
	virtual void __fastcall InternalExecute(int Iters, int Offset);
	virtual void __fastcall InternalCreateProcCall(const System::UnicodeString Name, bool NeedDescribe, bool IsQuery = false);
	bool __fastcall GetForceSPInit();
	virtual void __fastcall DoBeforeExecute();
	virtual void __fastcall DoAfterExecute(bool Result);
	virtual System::UnicodeString __fastcall ParseSQL(const System::UnicodeString SQL, TDAParams* Params);
	void __fastcall SQLChanged(System::TObject* Sender);
	void __fastcall ProcessSQLChanged(bool LockMacros, bool SaveBaseSQL);
	virtual void __fastcall ScanMacros();
	virtual System::UnicodeString __fastcall GetFinalSQL();
	void __fastcall SetICommandSQL();
	virtual void __fastcall AssembleSQL();
	virtual bool __fastcall NeedRecreateProcCall();
	virtual void __fastcall CheckSQL(int Iters = 0x1);
	virtual bool __fastcall IsInOutParamSupported();
	virtual bool __fastcall NeedConvertEOLForBlob();
	virtual void __fastcall AssignParam(Craccess::TParamDesc* ParamDesc, TDAParam* Param);
	virtual void __fastcall AssignParamValue(Craccess::TParamDesc* ParamDesc, TDAParam* Param);
	virtual void __fastcall AssignParamDesc(TDAParam* Param, Craccess::TParamDesc* ParamDesc);
	virtual void __fastcall AssignParamDescValue(TDAParam* Param, Craccess::TParamDesc* ParamDesc);
	virtual void __fastcall CreateParams()/* overload */;
	virtual void __fastcall CreateParams(TDAParams* Params, Craccess::TParamDescs* ParamDescs)/* overload */;
	virtual void __fastcall WriteParams(bool WriteValue = true);
	virtual void __fastcall ReadParams();
	void __fastcall UpdateParams();
	virtual TDAParam* __fastcall FindResultParam();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadParamData(System::Classes::TReader* Reader);
	void __fastcall WriteParamData(System::Classes::TWriter* Writer);
	void __fastcall ReadMacroData(System::Classes::TReader* Reader);
	void __fastcall WriteMacroData(System::Classes::TWriter* Writer);
	void __fastcall ReadStoredProcName(System::Classes::TReader* Reader);
	void __fastcall WriteStoredProcName(System::Classes::TWriter* Writer);
	void __fastcall SetStoredProcName(const System::UnicodeString StoredProcName);
	void __fastcall ReadStoredProcIsQuery(System::Classes::TReader* Reader);
	void __fastcall WriteStoredProcIsQuery(System::Classes::TWriter* Writer);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	__property bool AutoCommit = {read=FAutoCommit, write=SetAutoCommit, default=0};
	__property bool NonBlocking = {read=GetNonBlocking, write=SetNonBlocking, default=0};
	__property bool StoredProcIsQuery = {read=FStoredProcIsQuery, write=FStoredProcIsQuery, nodefault};
	__property TDATransaction* Transaction = {read=GetTransaction, write=SetTransaction, stored=IsTransactionStored};
	
public:
	__fastcall virtual TCustomDASQL(System::Classes::TComponent* Owner);
	__fastcall virtual ~TCustomDASQL();
	virtual void __fastcall Prepare();
	virtual void __fastcall UnPrepare();
	virtual void __fastcall Execute()/* overload */;
	virtual void __fastcall Execute(int Iters, int Offset = 0x0)/* overload */;
	void __fastcall BreakExec();
	bool __fastcall Executing();
	bool __fastcall WaitExecuting(int TimeOut = 0x0);
	TDAParam* __fastcall FindParam(const System::UnicodeString Value);
	TDAParam* __fastcall ParamByName(const System::UnicodeString Value);
	TMacro* __fastcall FindMacro(const System::UnicodeString Value);
	TMacro* __fastcall MacroByName(const System::UnicodeString Value);
	__property TCustomDAConnection* Connection = {read=FConnection, write=SetConnection};
	__property bool ParamCheck = {read=FParamCheck, write=SetParamCheck, default=1};
	__property System::Classes::TStrings* SQL = {read=FSQL, write=SetSQL};
	__property bool Prepared = {read=GetPrepared, write=SetPrepared, nodefault};
	__property TDAParams* Params = {read=FParams, write=SetParams, stored=false};
	__property System::Word ParamCount = {read=GetParamCount, nodefault};
	__property System::Variant ParamValues[const System::UnicodeString ParamName] = {read=GetParamValues, write=SetParamValues/*, default*/};
	__property TMacros* Macros = {read=FMacros, write=SetMacros, stored=false};
	__property System::Word MacroCount = {read=GetMacroCount, nodefault};
	__property bool Debug = {read=FDebug, write=FDebug, default=0};
	__property bool ChangeCursor = {read=FChangeCursor, write=FChangeCursor, nodefault};
	__property int RowsAffected = {read=GetRowsAffected, nodefault};
	__property int ParamsProcessed = {read=GetParamsProcessed, nodefault};
	__property System::UnicodeString FinalSQL = {read=GetFinalSQL};
	__property TBeforeExecuteEvent BeforeExecute = {read=FBeforeExecute, write=FBeforeExecute};
	__property TAfterExecuteEvent AfterExecute = {read=FAfterExecute, write=FAfterExecute};
};


class PASCALIMPLEMENTATION TDAMetaData : public Memds::TMemDataSet
{
	typedef Memds::TMemDataSet inherited;
	
private:
	TCustomDAConnection* FConnection;
	TDATransaction* FTransaction;
	System::UnicodeString FMetaDataKind;
	System::Classes::TStrings* FRestrictions;
	Craccess::TCRMetaData* FIMetaData;
	bool FDesignCreate;
	void __fastcall SetConnection(TCustomDAConnection* Value);
	void __fastcall ConnectChange(System::TObject* Sender, bool Connecting);
	void __fastcall SetMetaDataKind(const System::UnicodeString Value);
	void __fastcall SetRestrictions(System::Classes::TStrings* Value);
	void __fastcall RestrictionsChanged(System::TObject* Sender);
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	TCustomDAConnection* __fastcall UsedConnection();
	virtual TDATransaction* __fastcall UsedTransaction();
	virtual TDATransaction* __fastcall GetTransaction();
	virtual void __fastcall SetTransaction(TDATransaction* Value);
	bool __fastcall IsTransactionStored();
	virtual void __fastcall BeginConnection();
	virtual void __fastcall EndConnection();
	void __fastcall CheckIMetaData();
	virtual void __fastcall OpenCursor(bool InfoQuery);
	virtual void __fastcall InternalOpen();
	virtual void __fastcall CloseCursor();
	__property TDATransaction* Transaction = {read=GetTransaction, write=SetTransaction, stored=IsTransactionStored};
	
public:
	__fastcall virtual TDAMetaData(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TDAMetaData();
	void __fastcall GetMetaDataKinds(System::Classes::TStrings* List);
	void __fastcall GetRestrictions(System::Classes::TStrings* List, const System::UnicodeString MetaDataKind);
	__property TCustomDAConnection* Connection = {read=FConnection, write=SetConnection};
	__property System::UnicodeString MetaDataKind = {read=FMetaDataKind, write=SetMetaDataKind};
	__property System::Classes::TStrings* Restrictions = {read=FRestrictions, write=SetRestrictions};
};


class PASCALIMPLEMENTATION TCustomDAUpdateSQL : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TCustomDADataSet* FDataSet;
	System::StaticArray<System::Classes::TStrings*, 11> FSQLText;
	System::StaticArray<System::Classes::TComponent*, 11> FUpdateObject;
	
protected:
	bool FDesignCreate;
	System::Classes::TStrings* __fastcall GetSQLIndex(int Index);
	void __fastcall SetSQLIndex(int Index, System::Classes::TStrings* Value);
	virtual System::Classes::TStrings* __fastcall GetSQL(Data::Db::TUpdateKind UpdateKind);
	void __fastcall SetSQL(Data::Db::TUpdateKind UpdateKind, System::Classes::TStrings* Value);
	System::Classes::TComponent* __fastcall GetObjectIndex(int Index);
	void __fastcall SetObjectIndex(int Index, System::Classes::TComponent* Value);
	virtual TCustomDADataSet* __fastcall GetDataSet();
	virtual void __fastcall SetDataSet(TCustomDADataSet* DataSet);
	virtual void __fastcall Loaded();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual TCustomDADataSetClass __fastcall DataSetClass();
	virtual TCustomDASQLClass __fastcall SQLClass();
	void __fastcall CheckUpdateComponent(System::Classes::TComponent* Component)/* overload */;
	void __fastcall CheckUpdateComponent(System::Classes::TComponent* Component, TCustomDADataSet* NewDataset)/* overload */;
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TCustomDAUpdateSQL(System::Classes::TComponent* Owner);
	__fastcall virtual ~TCustomDAUpdateSQL();
	virtual void __fastcall Apply(Data::Db::TUpdateKind UpdateKind);
	void __fastcall ExecSQL(Data::Db::TUpdateKind UpdateKind);
	__property TCustomDADataSet* DataSet = {read=GetDataSet, write=SetDataSet};
	__property System::Classes::TStrings* SQL[Data::Db::TUpdateKind UpdateKind] = {read=GetSQL, write=SetSQL};
	
__published:
	__property System::Classes::TStrings* InsertSQL = {read=GetSQLIndex, write=SetSQLIndex, index=1};
	__property System::Classes::TStrings* DeleteSQL = {read=GetSQLIndex, write=SetSQLIndex, index=3};
	__property System::Classes::TStrings* ModifySQL = {read=GetSQLIndex, write=SetSQLIndex, index=2};
	__property System::Classes::TStrings* RefreshSQL = {read=GetSQLIndex, write=SetSQLIndex, index=5};
	__property System::Classes::TStrings* LockSQL = {read=GetSQLIndex, write=SetSQLIndex, index=4};
	__property System::Classes::TComponent* InsertObject = {read=GetObjectIndex, write=SetObjectIndex, index=1};
	__property System::Classes::TComponent* DeleteObject = {read=GetObjectIndex, write=SetObjectIndex, index=3};
	__property System::Classes::TComponent* ModifyObject = {read=GetObjectIndex, write=SetObjectIndex, index=2};
	__property System::Classes::TComponent* RefreshObject = {read=GetObjectIndex, write=SetObjectIndex, index=5};
	__property System::Classes::TComponent* LockObject = {read=GetObjectIndex, write=SetObjectIndex, index=4};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TMacro : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FValue;
	bool FActive;
	void __fastcall SetValue(const System::UnicodeString Value);
	void __fastcall SetActive(bool Value);
	System::TDateTime __fastcall GetAsDateTime();
	void __fastcall SetAsDateTime(System::TDateTime Value);
	double __fastcall GetAsFloat();
	void __fastcall SetAsFloat(double Value);
	int __fastcall GetAsInteger();
	void __fastcall SetAsInteger(int Value);
	System::UnicodeString __fastcall GetAsString();
	void __fastcall SetAsString(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	bool __fastcall IsEqual(TMacro* Value);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TMacro(System::Classes::TCollection* Collection);
	void __fastcall Clear();
	__property System::TDateTime AsDateTime = {read=GetAsDateTime, write=SetAsDateTime};
	__property double AsFloat = {read=GetAsFloat, write=SetAsFloat};
	__property int AsInteger = {read=GetAsInteger, write=SetAsInteger, nodefault};
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString Value = {read=FValue, write=SetValue};
	__property bool Active = {read=FActive, write=SetActive, default=1};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TMacro() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMacros : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TMacro* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	void __fastcall ReadBinaryData(System::Classes::TStream* Stream);
	HIDESBASE TMacro* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TMacro* Value);
	void __fastcall NotifyOwner(TMacro* Item);
	
protected:
	Crparser::TSQLParserClass FParserClass;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	virtual System::UnicodeString __fastcall GetMacroValue(TMacro* Macro);
	
public:
	__fastcall TMacros(System::Classes::TPersistent* Owner);
	void __fastcall Scan(const System::UnicodeString SQL);
	void __fastcall AssignValues(TMacros* Value);
	bool __fastcall IsEqual(TMacros* Value);
	TMacro* __fastcall FindMacro(const System::UnicodeString Value);
	TMacro* __fastcall MacroByName(const System::UnicodeString Value);
	void __fastcall Expand(System::UnicodeString &SQL);
	void __fastcall SetParserClass(Crparser::TSQLParserClass Value);
	__property TMacro* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TMacros() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TLabelSet : unsigned char { lsCustom, lsEnglish, lsFrench, lsGerman, lsItalian, lsPolish, lsPortuguese, lsRussian, lsSpanish };

enum DECLSPEC_DENUM TConnectDialogOptionKind : unsigned char { okServer, okUserName, okPassword, okDatabase, okPort, okDirect, okAuthentication, okRole, okClientLibrary, okProtocol, okSchema, okHome };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TConnectDialogOption : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TCustomConnectDialog* FOwner;
	TConnectDialogOptionKind FKind;
	System::UnicodeString FCaption;
	bool FVisible;
	int FOrder;
	void __fastcall SetCaption(System::UnicodeString Value);
	void __fastcall SetVisible(bool Value);
	void __fastcall SetOrder(int Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TConnectDialogOption(TCustomConnectDialog* Owner, TConnectDialogOptionKind OptionKind, int Order, bool Visible);
	__property TConnectDialogOptionKind Kind = {read=FKind, nodefault};
	
__published:
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property bool Visible = {read=FVisible, write=SetVisible, nodefault};
	__property int Order = {read=FOrder, write=SetOrder, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TConnectDialogOption() { }
	
};

#pragma pack(pop)

typedef System::DynamicArray<TConnectDialogOption*> TConnectDialogOptionArray;

class PASCALIMPLEMENTATION TCustomConnectDialog : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TCustomDAConnection* FConnection;
	System::Word FRetries;
	System::UnicodeString FDialogClass;
	bool FSavePassword;
	bool FStoreLogInfo;
	bool FUseServerHistory;
	bool FNeedConnect;
	System::UnicodeString FCaption;
	System::UnicodeString FConnectButton;
	System::UnicodeString FCancelButton;
	TConnectDialogOption* FServerOption;
	TConnectDialogOption* FUserNameOption;
	TConnectDialogOption* FPasswordOption;
	void __fastcall SetCaption(System::UnicodeString Value);
	System::UnicodeString __fastcall GetUserNameLabel();
	void __fastcall SetUserNameLabel(System::UnicodeString Value);
	System::UnicodeString __fastcall GetPasswordLabel();
	void __fastcall SetPasswordLabel(System::UnicodeString Value);
	System::UnicodeString __fastcall GetServerLabel();
	void __fastcall SetServerLabel(System::UnicodeString Value);
	void __fastcall SetConnectButton(System::UnicodeString Value);
	void __fastcall SetCancelButton(System::UnicodeString Value);
	void __fastcall SetServerOption(TConnectDialogOption* Value);
	void __fastcall SetUserNameOption(TConnectDialogOption* Value);
	void __fastcall SetPasswordOption(TConnectDialogOption* Value);
	
protected:
	TLabelSet FLabelSet;
	bool InSetLabelSet;
	Crserverenumerator::TCRServerEnumerator* FServerEnumerator;
	virtual Crserverenumerator::TCRServerEnumeratorClass __fastcall GetServerEnumeratorClass();
	virtual void __fastcall SetServerEnumerator(Crserverenumerator::TCRServerEnumerator* Value);
	void __fastcall CreateServerEnumerator();
	void __fastcall FreeServerEnumerator();
	void __fastcall CheckServerEnumerator();
	System::UnicodeString __fastcall GetString(int Id);
	virtual void __fastcall SetLabelSet(TLabelSet Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual System::TClass __fastcall DefDialogClass();
	virtual System::UnicodeString __fastcall GetKeyPath();
	virtual System::UnicodeString __fastcall GetServerStoreName();
	System::UnicodeString __fastcall GetApplicationKeyPath();
	virtual System::UnicodeString __fastcall GetServerListKeyPath();
	virtual void __fastcall SaveServerListToRegistry();
	virtual void __fastcall LoadServerListFromRegistry(System::Classes::TStrings* List);
	virtual void __fastcall SaveInfoToRegistry(System::Win::Registry::TRegistry* Registry);
	virtual void __fastcall LoadInfoFromRegistry(System::Win::Registry::TRegistry* Registry);
	void __fastcall ReadServerCaptionProperty(System::Classes::TReader* Reader);
	void __fastcall ReadUserNameCaptionProperty(System::Classes::TReader* Reader);
	void __fastcall ReadPasswordCaptionProperty(System::Classes::TReader* Reader);
	void __fastcall ReadSavePasswordProperty(System::Classes::TReader* Reader);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	
public:
	__fastcall virtual TCustomConnectDialog(System::Classes::TComponent* Owner);
	__fastcall virtual ~TCustomConnectDialog();
	virtual bool __fastcall Execute();
	virtual void __fastcall GetServerList(System::Classes::TStrings* List);
	virtual void __fastcall GetOptions(TConnectDialogOptionArray &Options, bool Ordered = true);
	void __fastcall OptionChanged();
	__property TCustomDAConnection* Connection = {read=FConnection};
	__property System::Word Retries = {read=FRetries, write=FRetries, default=3};
	__property bool SavePassword = {read=FSavePassword, write=FSavePassword, default=0};
	__property bool StoreLogInfo = {read=FStoreLogInfo, write=FStoreLogInfo, default=1};
	__property System::UnicodeString DialogClass = {read=FDialogClass, write=FDialogClass};
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property System::UnicodeString UsernameLabel = {read=GetUserNameLabel, write=SetUserNameLabel};
	__property System::UnicodeString PasswordLabel = {read=GetPasswordLabel, write=SetPasswordLabel};
	__property System::UnicodeString ServerLabel = {read=GetServerLabel, write=SetServerLabel};
	__property System::UnicodeString ConnectButton = {read=FConnectButton, write=SetConnectButton};
	__property System::UnicodeString CancelButton = {read=FCancelButton, write=SetCancelButton};
	__property TConnectDialogOption* Server = {read=FServerOption, write=SetServerOption};
	__property TConnectDialogOption* UserName = {read=FUserNameOption, write=SetUserNameOption};
	__property TConnectDialogOption* Password = {read=FPasswordOption, write=SetPasswordOption};
	__property TLabelSet LabelSet = {read=FLabelSet, write=SetLabelSet, default=1};
	__property bool UseServerHistory = {read=FUseServerHistory, write=FUseServerHistory, default=1};
};


struct DECLSPEC_DRECORD TTableInfo
{
public:
	System::UnicodeString Name;
	System::UnicodeString Alias;
};


typedef System::DynamicArray<TTableInfo> TTablesInfo;

class PASCALIMPLEMENTATION TCRDataSource : public Data::Db::TDataSource
{
	typedef Data::Db::TDataSource inherited;
	
protected:
	bool FDesignCreate;
	virtual void __fastcall Loaded();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall virtual TCRDataSource(System::Classes::TComponent* Owner);
public:
	/* TDataSource.Destroy */ inline __fastcall virtual ~TCRDataSource() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBAccessUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod bool __fastcall IsSharedObjectDataType(TDAParam* Obj, Data::Db::TFieldType DataType);
	__classmethod bool __fastcall IsBlobDataType(TDAParam* Obj, Data::Db::TFieldType DataType);
	__classmethod bool __fastcall GetNational(TDAParam* Obj);
	__classmethod void __fastcall CheckConnection(TCustomDADataSet* Obj)/* overload */;
	__classmethod void __fastcall CheckConnection(TCustomDASQL* Obj)/* overload */;
	__classmethod TCustomDAConnection* __fastcall UsedConnection(TCustomDADataSet* Obj)/* overload */;
	__classmethod TCustomDAConnection* __fastcall UsedConnection(TCustomDASQL* Obj)/* overload */;
	__classmethod TCustomDAConnection* __fastcall UsedConnection(TDAMetaData* Obj)/* overload */;
	__classmethod TCustomDAConnection* __fastcall UsedConnection(System::Classes::TComponent* Obj)/* overload */;
	__classmethod void __fastcall SetAutoCommit(System::Classes::TComponent* Obj, bool Value);
	__classmethod bool __fastcall GetAutoCommit(TCustomDAConnection* Obj)/* overload */;
	__classmethod bool __fastcall GetAutoCommit(TCustomDADataSet* Obj)/* overload */;
	__classmethod bool __fastcall GetAutoCommit(TCustomDASQL* Obj)/* overload */;
	__classmethod void __fastcall SetDesignCreate(TDATransaction* Obj, bool Value)/* overload */;
	__classmethod bool __fastcall GetDesignCreate(TDATransaction* Obj)/* overload */;
	__classmethod void __fastcall SetDesignCreate(TCustomDADataSet* Obj, bool Value)/* overload */;
	__classmethod bool __fastcall GetDesignCreate(TCustomDADataSet* Obj)/* overload */;
	__classmethod void __fastcall SetDesignCreate(TCustomDASQL* Obj, bool Value)/* overload */;
	__classmethod bool __fastcall GetDesignCreate(TCustomDASQL* Obj)/* overload */;
	__classmethod void __fastcall SetDesignCreate(TCustomDAUpdateSQL* Obj, bool Value)/* overload */;
	__classmethod bool __fastcall GetDesignCreate(TCustomDAUpdateSQL* Obj)/* overload */;
	__classmethod void __fastcall SetDesignCreate(TDAMetaData* Obj, bool Value)/* overload */;
	__classmethod bool __fastcall GetDesignCreate(TDAMetaData* Obj)/* overload */;
	__classmethod void __fastcall SetDesignCreate(TCRDataSource* Obj, bool Value)/* overload */;
	__classmethod bool __fastcall GetDesignCreate(TCRDataSource* Obj)/* overload */;
	__classmethod void __fastcall SetLockLoginPrompt(TCustomDAConnection* Obj, bool Value);
	__classmethod Craccess::TCRConnection* __fastcall GetIConnection(TCustomDAConnection* Obj);
	__classmethod void __fastcall CreateIConnection(TCustomDAConnection* Obj);
	__classmethod System::Classes::TComponent* __fastcall GetUpdateQuery(TCustomDADataSet* Obj);
	__classmethod Craccess::TCRTablesInfo* __fastcall GetTablesInfo(TCustomDADataSet* Obj);
	__classmethod Craccess::TSQLInfo* __fastcall GetSQLInfo(TCustomDADataSet* Obj);
	__classmethod System::UnicodeString __fastcall GetUpdatingTable(TCustomDADataSet* Obj);
	__classmethod void __fastcall SetUpdatingTable(TCustomDADataSet* Obj, System::UnicodeString Value);
	__classmethod void __fastcall InternalConnect(TCustomDAConnection* Obj);
	__classmethod void __fastcall InternalDisconnect(TCustomDAConnection* Obj);
	__classmethod void __fastcall DisconnectTransaction(TCustomDAConnection* Obj);
	__classmethod void __fastcall SuppressAutoCommit(TCustomDAConnection* Obj);
	__classmethod void __fastcall RestoreAutoCommit(TCustomDAConnection* Obj);
	__classmethod bool __fastcall IsMultipleTransactionsSupported(TCustomDAConnection* Obj);
	__classmethod int __fastcall PushOperation(TCustomDAConnection* Obj, Memdata::TConnLostCause Operation, bool AllowFailOver = true);
	__classmethod Memdata::TConnLostCause __fastcall PopOperation(TCustomDAConnection* Obj);
	__classmethod void __fastcall RestoreAfterFailOver(TCustomDAConnection* Obj);
	__classmethod bool __fastcall IsFailOverAllowed(TCustomDAConnection* Obj);
	__classmethod TDATransaction* __fastcall UsedTransaction(TCustomDAConnection* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall UsedTransaction(TCustomDADataSet* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall UsedTransaction(TCustomDASQL* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall UsedTransaction(System::Classes::TComponent* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall GetTransaction(TCustomDADataSet* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall GetTransaction(TCustomDASQL* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall GetTransaction(TDAMetaData* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall GetDefaultTransaction(TCustomDAConnection* Obj);
	__classmethod void __fastcall SetTransaction(TCustomDADataSet* Obj, TDATransaction* Value)/* overload */;
	__classmethod void __fastcall SetTransaction(TCustomDASQL* Obj, TDATransaction* Value)/* overload */;
	__classmethod void __fastcall SetTransaction(TDAMetaData* Obj, TDATransaction* Value)/* overload */;
	__classmethod void __fastcall SetDefaultTransaction(TCustomDAConnection* Obj, TDATransaction* Value);
	__classmethod TDATransaction* __fastcall GetFTransaction(TCustomDADataSet* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall GetFTransaction(TCustomDASQL* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall GetFTransaction(TDAMetaData* Obj)/* overload */;
	__classmethod TDATransaction* __fastcall GetFDefaultTransaction(TCustomDAConnection* Obj);
	__classmethod Craccess::TCRTransaction* __fastcall GetITransaction(TDATransaction* Obj);
	__classmethod int __fastcall GetConnectionCount(TDATransaction* Obj);
	__classmethod TCustomDAConnection* __fastcall GetConnection(TDATransaction* Obj, int Index);
	__classmethod void __fastcall Savepoint(TDATransaction* Obj, const System::UnicodeString Name);
	__classmethod void __fastcall RollbackToSavepoint(TDATransaction* Obj, const System::UnicodeString Name);
	__classmethod void __fastcall ReleaseSavepoint(TDATransaction* Obj, const System::UnicodeString Name);
	__classmethod void __fastcall CommitRetaining(TDATransaction* Obj);
	__classmethod void __fastcall RollbackRetaining(TDATransaction* Obj);
	__classmethod void __fastcall GainTransaction(TDATransaction* Obj);
	__classmethod void __fastcall ReleaseTransaction(TDATransaction* Obj);
	__classmethod void __fastcall AutoCommitTransaction(TDATransaction* Obj, bool NeedCommit);
	__classmethod __int64 __fastcall GetMultiTransactionID(TDATransaction* Obj);
	__classmethod void __fastcall Disconnect(TCustomDASQL* Obj);
	__classmethod Dasqlgenerator::TDASQLGenerator* __fastcall SQLGenerator(TCustomDADataSet* Obj);
	__classmethod System::Classes::TList* __fastcall GetSQLs(TCustomDAConnection* Obj);
	__classmethod void __fastcall GetKeyAndDataFields(TCustomDADataSet* Obj, /* out */ Craccess::TKeyAndDataFields &KeyAndDataFields, const bool ForceUseAllKeyFields);
	__classmethod bool __fastcall GetLockDebug(System::Classes::TComponent* Obj);
	__classmethod void __fastcall SetLockDebug(System::Classes::TComponent* Obj, bool Value);
	__classmethod TCustomDAConnection* __fastcall FOwner(TDAConnectionOptions* Obj)/* overload */;
	__classmethod TCustomDADataSet* __fastcall FOwner(TDADataSetOptions* Obj)/* overload */;
	__classmethod System::TClass __fastcall SQLMonitorClass(TCustomDAConnection* Obj);
	__classmethod TConnectDialogClass __fastcall ConnectDialogClass(TCustomDAConnection* Obj);
	__classmethod System::UnicodeString __fastcall QuoteName(TCustomDADataSet* Obj, const System::UnicodeString AName);
	__classmethod void __fastcall RegisterClient(TCustomDAConnection* Obj, System::TObject* Client, Data::Db::TConnectChangeEvent Event = 0x0);
	__classmethod void __fastcall UnRegisterClient(TCustomDAConnection* Obj, System::TObject* Client);
	__classmethod Craccess::TCRFieldDesc* __fastcall GetIdentityField(TCustomDADataSet* Obj);
	__classmethod System::Classes::TStrings* __fastcall GetSQL(System::Classes::TComponent* Obj);
	__classmethod void __fastcall SetSQL(System::Classes::TComponent* Obj, System::Classes::TStrings* Value);
	__classmethod System::UnicodeString __fastcall GetSQLText(System::Classes::TComponent* Obj);
	__classmethod void __fastcall SetSQLText(System::Classes::TComponent* Obj, const System::UnicodeString SQLText, const bool LockScanParams, const bool LockMacros);
	__classmethod TDAParams* __fastcall GetParams(System::Classes::TComponent* Obj);
	__classmethod void __fastcall Execute(System::Classes::TComponent* Obj);
	__classmethod void __fastcall Open(System::Classes::TComponent* Obj);
	__classmethod int __fastcall GetRowsAffected(System::Classes::TComponent* Obj);
	__classmethod int __fastcall GetParamsProcessed(System::Classes::TComponent* Obj);
	__classmethod TStatementTypes __fastcall GetUpdateSQLStatementTypes(TCustomDADataSet* Obj);
	__classmethod System::Classes::TStrings* __fastcall GetUpdateSQLIndex(TCustomDADataSet* Obj, TStatementType StatementType);
	__classmethod System::UnicodeString __fastcall ParseSQL(TCustomDASQL* Obj, const System::UnicodeString SQL, TDAParams* Params);
	__classmethod TDAParams* __fastcall CreateParamsObject(TCustomDASQL* Obj);
	__classmethod void __fastcall SetDesigning(System::Classes::TComponent* Obj, bool Value, bool SetChildren = true);
	__classmethod Craccess::TCRRecordSet* __fastcall GetIRecordSet(TCustomDADataSet* Obj);
	__classmethod void __fastcall CheckIRecordSet(TCustomDADataSet* Obj);
	__classmethod Craccess::TCRCommand* __fastcall GetICommand(System::Classes::TComponent* Obj)/* overload */;
	__classmethod Craccess::TCRCommand* __fastcall GetICommand(TCustomDADataSet* Obj)/* overload */;
	__classmethod Craccess::TCRCommand* __fastcall GetICommand(TCustomDASQL* Obj)/* overload */;
	__classmethod TDADataSetUpdater* __fastcall GetUpdater(TCustomDADataSet* Obj);
	__classmethod TDADataSetService* __fastcall GetDataSetService(TCustomDADataSet* Obj);
	__classmethod TCustomDADataSetClass __fastcall GetDataSetClass(TCustomDAUpdateSQL* Obj);
	__classmethod TCustomDASQLClass __fastcall GetSQLClass(TCustomDAUpdateSQL* Obj);
	__classmethod TDAFieldTypeMapClass __fastcall GetFieldTypeMapClass(TCustomDAConnection* Obj)/* overload */;
	__classmethod TDAFieldTypeMapClass __fastcall GetFieldTypeMapClass(TCustomDADataSet* Obj)/* overload */;
	__classmethod Crparser::TSQLParserClass __fastcall GetParserClass(TMacros* Obj);
	__classmethod void __fastcall SaveServerListToRegistry(TCustomConnectDialog* Obj);
	__classmethod void __fastcall SetConnection(TCustomConnectDialog* Obj, TCustomDAConnection* Value);
	__classmethod void __fastcall SetUseServerHistory(TCustomConnectDialog* Obj, bool Value);
	__classmethod bool __fastcall GetNeedConnect(TCustomConnectDialog* Obj);
	__classmethod void __fastcall SetNeedConnect(TCustomConnectDialog* Obj, bool Value);
	__classmethod void __fastcall CreateProcCall(TCustomDASQL* Obj, const System::UnicodeString Name, bool NeedDescribe, bool IsQuery = false)/* overload */;
	__classmethod void __fastcall CreateProcCall(TCustomDADataSet* Obj, const System::UnicodeString Name, bool NeedDescribe, bool IsQuery = false)/* overload */;
	__classmethod TCustomDASQL* __fastcall GetCommand(TCustomDAConnection* Obj);
	__classmethod bool __fastcall GetStreamedConnected(TCustomDAConnection* Obj);
	__classmethod void __fastcall Loaded(TCustomDAConnection* Obj);
	__classmethod Craccess::TCRCursor* __fastcall GetAsCursor(TDAParam* Obj);
	__classmethod Craccess::TCRCursor* __fastcall GetCursor(TCustomDADataSet* Obj);
	__classmethod void __fastcall SetCursor(TCustomDADataSet* Obj, Craccess::TCRCursor* Value);
	__classmethod bool __fastcall GetFetchAll(TCustomDADataSet* Obj);
	__classmethod void __fastcall SetFetchAll(TCustomDADataSet* Obj, bool Value);
	__classmethod void __fastcall QuickOpen(TCustomDADataSet* Obj, TQuickOpenInfo &Info);
	__classmethod void __fastcall Restore(TCustomDADataSet* Obj, const TQuickOpenInfo &Info);
	__classmethod TLockMode __fastcall GetLockMode(TCustomDADataSet* Obj);
	__classmethod void __fastcall SetLockMode(TCustomDADataSet* Obj, const TLockMode Value);
	__classmethod __int64 __fastcall GetLastInsertId(TCustomDADataSet* Obj);
	__classmethod bool __fastcall GetFullRefresh(TCustomDADataSet* Obj);
public:
	/* TObject.Create */ inline __fastcall TDBAccessUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDBAccessUtils() { }
	
};

#pragma pack(pop)

typedef System::StaticArray<System::UnicodeString, 11> Dbaccess__83;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 OperationsStackDelta = System::Int8(0x32);
static const System::Int8 crSQLArrow = System::Int8(-30);
extern DELPHI_PACKAGE bool ChangeCursor;
extern DELPHI_PACKAGE void __fastcall (*SetCursorProc)(int Value);
extern DELPHI_PACKAGE bool __fastcall (*ShowConnectFormProc)(TCustomConnectDialog* ConnectDialog);
extern DELPHI_PACKAGE bool __fastcall (*ShowConnectFormProcFmx)(TCustomConnectDialog* ConnectDialog);
extern DELPHI_PACKAGE Dbaccess__83 StatementTypeNames;
#define BlobTypes (System::Set<Data::Db::TFieldType, Data::Db::TFieldType::ftUnknown, Data::Db::TFieldType::ftSingle>() << Data::Db::TFieldType::ftBlob << Data::Db::TFieldType::ftGraphic << Data::Db::TFieldType::ftOraBlob )
#define MemoTypes (System::Set<Data::Db::TFieldType, Data::Db::TFieldType::ftUnknown, Data::Db::TFieldType::ftSingle>() << Data::Db::TFieldType::ftMemo << Data::Db::TFieldType::ftFmtMemo << Data::Db::TFieldType::ftOraClob << Data::Db::TFieldType::ftWideMemo )
extern DELPHI_PACKAGE bool BaseSQLOldBehavior;
extern DELPHI_PACKAGE bool SQLGeneratorCompatibility;
extern DELPHI_PACKAGE bool ResyncBeforeFetch;
extern DELPHI_PACKAGE bool BoundParams;
extern DELPHI_PACKAGE bool OldFieldsReadOnly;
extern DELPHI_PACKAGE bool ParamStringAsAnsiString;
extern DELPHI_PACKAGE bool OldCachedUpdateLockMode;
extern DELPHI_PACKAGE bool PreventPSKeyFields;
extern DELPHI_PACKAGE void __fastcall RecreateParamsRef(Data::Db::TParams* Params);
extern DELPHI_PACKAGE void __fastcall SetCursor(int Value);
extern DELPHI_PACKAGE TStatementType __fastcall UpdateKindToStatementType(const Data::Db::TUpdateKind UpdateKind);
extern DELPHI_PACKAGE Data::Db::TUpdateKind __fastcall StatementTypeToUpdateKind(const TStatementType StatementType);
}	/* namespace Dbaccess */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DBACCESS)
using namespace Dbaccess;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DbaccessHPP
