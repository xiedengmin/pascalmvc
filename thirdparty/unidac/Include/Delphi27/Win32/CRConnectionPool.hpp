// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRConnectionPool.pas' rev: 34.00 (Windows)

#ifndef CrconnectionpoolHPP
#define CrconnectionpoolHPP

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
#include <System.SyncObjs.hpp>
#include <System.Types.hpp>
#include <System.Variants.hpp>
#include <CRTypes.hpp>
#include <CLRClasses.hpp>
#include <CRVio.hpp>
#include <CRAccess.hpp>
#include <MemUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crconnectionpool
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRConnectionParameters;
class DELPHICLASS TCRConnectionPool;
class DELPHICLASS TCRLocalConnectionPool;
class DELPHICLASS TValidateThread;
class DELPHICLASS TCRConnectionPoolManager;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TCRConnectionParametersClass;

typedef System::TMetaClass* TCRConnectionPoolManagerClass;

class PASCALIMPLEMENTATION TCRConnectionParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	Crvio::TSSLOptions* FSslOptions;
	Crvio::THttpOptions* FHttpOptions;
	Crvio::TProxyOptions* FProxyOptions;
	void __fastcall CreateSecureOptions();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual System::UnicodeString __fastcall ConnectParamsToString();
	virtual System::UnicodeString __fastcall PoolParamsToString();
	
public:
	int MinPoolSize;
	int MaxPoolSize;
	System::UnicodeString Username;
	System::UnicodeString Server;
	System::UnicodeString Password;
	int ConnectionLifeTime;
	bool Validate;
	Crvio::TCRIOHandler* IOHandler;
	Craccess::TErrorProc OnError;
	__fastcall virtual TCRConnectionParameters();
	__fastcall virtual ~TCRConnectionParameters();
	HIDESBASE virtual bool __fastcall Equals(TCRConnectionParameters* Parameters);
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual System::UnicodeString __fastcall AsString();
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRConnectionPool : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCRConnectionParameters* FConnectionParameters;
	TCRConnectionPoolManager* FManager;
	
protected:
	int FTakenConnectionsCount;
	virtual void __fastcall Validate();
	virtual void __fastcall Clear();
	virtual void __fastcall AsyncClear();
	virtual int __fastcall GetTotalConnectionsCount();
	virtual bool __fastcall InternalPutConnection(Craccess::TCRConnection* CRConnection) = 0 ;
	
public:
	__fastcall virtual TCRConnectionPool(TCRConnectionPoolManager* Manager, TCRConnectionParameters* ConnectionParameters);
	__fastcall virtual ~TCRConnectionPool();
	virtual Craccess::TCRConnection* __fastcall GetConnection() = 0 ;
	bool __fastcall PutConnection(Craccess::TCRConnection* CRConnection);
	virtual void __fastcall Invalidate();
	__property int TotalConnectionsCount = {read=GetTotalConnectionsCount, nodefault};
	__property TCRConnectionPoolManager* Manager = {read=FManager};
	__property TCRConnectionParameters* ConnectionParameters = {read=FConnectionParameters};
};

#pragma pack(pop)

typedef System::DynamicArray<Craccess::TCRConnection*> TCRConnectionsArray;

typedef System::DynamicArray<int> TIntegerArray;

typedef System::StaticArray<int, 8> TStatisticsArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRLocalConnectionPool : public TCRConnectionPool
{
	typedef TCRConnectionPool inherited;
	
private:
	TCRConnectionsArray FPooledConnections;
	int FPooledConnectionsCount;
	int FHead;
	int FTail;
	TIntegerArray FVersions;
	int FVersion;
	TStatisticsArray FStatistics;
	int FDoomedConnectionsCount;
	int FInvalidateVersion;
	int FClearVersion;
	System::Syncobjs::TEvent* hBusy;
	System::Syncobjs::TCriticalSection* FLockPooled;
	System::Syncobjs::TCriticalSection* FLockTaken;
	System::Syncobjs::TCriticalSection* FLockVersion;
	bool __fastcall IsLive(Craccess::TCRConnection* CRConnection);
	bool __fastcall CheckIsValid(Craccess::TCRConnection* Connection);
	void __fastcall ReserveConnection();
	bool __fastcall InternalGetConnection(/* out */ Craccess::TCRConnection* &Connection, /* out */ int &Version, bool Reserve = true);
	void __fastcall InternalFreeConnection(Craccess::TCRConnection* &Connection, bool Reserved = false);
	
protected:
	__classmethod virtual Craccess::TCRConnectionClass __fastcall GetConnectorClass();
	Craccess::TCRConnection* __fastcall CreateNewConnector();
	virtual void __fastcall OpenConnector(Craccess::TCRConnection* Connector);
	virtual void __fastcall InitConnectorParams(Craccess::TCRConnection* Connector);
	virtual void __fastcall InitConnectorSecureParams(Craccess::TCRConnection* Connector);
	virtual void __fastcall Validate();
	virtual void __fastcall Clear();
	virtual void __fastcall AsyncClear();
	virtual int __fastcall GetTotalConnectionsCount();
	virtual bool __fastcall InternalPutConnection(Craccess::TCRConnection* CRConnection);
	virtual void __fastcall InternalReturnConnection(Craccess::TCRConnection* Connection, int Version);
	
public:
	__fastcall virtual TCRLocalConnectionPool(TCRConnectionPoolManager* Manager, TCRConnectionParameters* ConnectionParameters);
	__fastcall virtual ~TCRLocalConnectionPool();
	virtual Craccess::TCRConnection* __fastcall GetConnection();
	virtual void __fastcall Invalidate();
	__property int PooledConnectionsCount = {read=FPooledConnectionsCount, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TValidateThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TCRConnectionPoolManager* FManager;
	System::Syncobjs::TEvent* FEvent;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TValidateThread(TCRConnectionPoolManager* Manager);
	__fastcall virtual ~TValidateThread();
	HIDESBASE void __fastcall Terminate();
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRConnectionPoolManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Crtypes::TCRObjectList* FPools;
	TValidateThread* FValidateThread;
	
protected:
	System::Syncobjs::TCriticalSection* FLockGet;
	System::Syncobjs::TCriticalSection* FLockList;
	System::TClass FSQLMonitorClass;
	__classmethod virtual int __fastcall GetPoolManagerIndex();
	__classmethod virtual void __fastcall SetPoolManagerIndex(int Value);
	virtual TCRConnectionPool* __fastcall CreateConnectionPool(TCRConnectionParameters* ConnectionParameters) = 0 ;
	TCRConnectionPool* __fastcall GetConnectionPool(TCRConnectionParameters* ConnectionParameters);
	void __fastcall InternalClear();
	void __fastcall InternalAsyncClear();
	virtual Craccess::TCRConnection* __fastcall InternalGetConnection(TCRConnectionParameters* ConnectionParameters);
	virtual Craccess::TCRConnection* __fastcall InternalCheckConnection(Craccess::TCRConnection* &Connection);
	
public:
	__fastcall TCRConnectionPoolManager();
	__fastcall virtual ~TCRConnectionPoolManager();
	__classmethod Craccess::TCRConnection* __fastcall GetConnection(TCRConnectionParameters* ConnectionParameters, System::TClass SQLMonitorClass);
	__classmethod void __fastcall Clear();
	__classmethod void __fastcall AsyncClear();
	__property System::TClass SQLMonitorClass = {read=FSQLMonitorClass, write=FSQLMonitorClass};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 StatisticsCount = System::Int8(0x8);
extern DELPHI_PACKAGE void __fastcall ClearPoolManager(void);
}	/* namespace Crconnectionpool */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRCONNECTIONPOOL)
using namespace Crconnectionpool;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrconnectionpoolHPP
