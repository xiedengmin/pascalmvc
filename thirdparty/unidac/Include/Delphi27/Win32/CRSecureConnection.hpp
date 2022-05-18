// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRSecureConnection.pas' rev: 34.00 (Windows)

#ifndef CrsecureconnectionHPP
#define CrsecureconnectionHPP

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
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRVio.hpp>
#include <CRVioTcp.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crsecureconnection
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TScVersion;
class DELPHICLASS TScNetworkCredential;
class DELPHICLASS TScWebProxy;
class DELPHICLASS TScConnectionPool;
class DELPHICLASS TScValidateThread;
class DELPHICLASS TScConnectionPoolManager;
class DELPHICLASS TScSecureConnectionParameters;
class DELPHICLASS TScSecureConnection;
class DELPHICLASS TScSecureConnectionStream;
class DELPHICLASS TScHttpParser;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TScVersion : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FMajor;
	int FMinor;
	int FBuild;
	int FRevision;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TScVersion()/* overload */;
	__fastcall TScVersion(int AMajor, int AMinor)/* overload */;
	__fastcall TScVersion(int AMajor, int AMinor, int ABuild)/* overload */;
	__fastcall TScVersion(int AMajor, int AMinor, int ABuild, int ARevision)/* overload */;
	bool __fastcall IsEqual(TScVersion* Obj);
	void __fastcall Parse(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall ToString();
	
__published:
	__property int Major = {read=FMajor, write=FMajor, default=0};
	__property int Minor = {read=FMinor, write=FMinor, default=0};
	__property int Build = {read=FBuild, write=FBuild, default=-1};
	__property int Revision = {read=FRevision, write=FRevision, default=-1};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TScVersion() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScNetworkCredential : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FDomain;
	System::UnicodeString FPassword;
	System::UnicodeString FUserName;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property System::UnicodeString Domain = {read=FDomain, write=FDomain};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString UserName = {read=FUserName, write=FUserName};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TScNetworkCredential() { }
	
public:
	/* TObject.Create */ inline __fastcall TScNetworkCredential() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScWebProxy : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAddress;
	int FPort;
	TScNetworkCredential* FCredentials;
	void __fastcall SetCredentials(TScNetworkCredential* Value);
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TScWebProxy();
	__fastcall virtual ~TScWebProxy();
	
__published:
	__property System::UnicodeString Address = {read=FAddress, write=FAddress};
	__property int Port = {read=FPort, write=FPort, default=8080};
	__property TScNetworkCredential* Credentials = {read=FCredentials, write=SetCredentials};
};

#pragma pack(pop)

typedef System::DynamicArray<TScSecureConnection*> TScConnectionsArray;

typedef System::DynamicArray<int> TIntegerArray;

typedef System::StaticArray<int, 8> TStatisticsArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScConnectionPool : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TScSecureConnectionParameters* FConnectionParameters;
	TScConnectionPoolManager* FManager;
	TScConnectionsArray FPooledConnections;
	int FPooledConnectionsCount;
	int FHead;
	int FTail;
	TIntegerArray FVersions;
	int FVersion;
	TStatisticsArray FStatistics;
	int FDoomedConnectionsCount;
	int FInvalidateVersion;
	System::Syncobjs::TEvent* hBusy;
	System::Syncobjs::TCriticalSection* FLockPooled;
	System::Syncobjs::TCriticalSection* FLockTaken;
	System::Syncobjs::TCriticalSection* FLockVersion;
	bool __fastcall IsLive(TScSecureConnection* Connection);
	bool __fastcall CheckIsConnected(TScSecureConnection* Connection);
	void __fastcall ReserveConnection();
	bool __fastcall InternalGetConnection(/* out */ TScSecureConnection* &Connection, /* out */ int &Version, bool Reserve = true);
	void __fastcall InternalFreeConnection(TScSecureConnection* &Connection, bool Reserved = false);
	
protected:
	int FTakenConnectionsCount;
	void __fastcall Validate();
	void __fastcall Clear();
	int __fastcall GetTotalConnectionsCount();
	bool __fastcall InternalPutConnection(TScSecureConnection* Connection);
	void __fastcall InternalReturnConnection(TScSecureConnection* Connection, int Version);
	TScSecureConnection* __fastcall CreateNewConnector();
	
public:
	__fastcall TScConnectionPool(TScConnectionPoolManager* Manager, TScSecureConnectionParameters* ConnectionParameters);
	__fastcall virtual ~TScConnectionPool();
	TScSecureConnection* __fastcall GetConnection();
	bool __fastcall PutConnection(TScSecureConnection* Connection);
	void __fastcall Invalidate();
	__property int TotalConnectionsCount = {read=GetTotalConnectionsCount, nodefault};
	__property TScConnectionPoolManager* Manager = {read=FManager};
	__property TScSecureConnectionParameters* ConnectionParameters = {read=FConnectionParameters};
	__property int PooledConnectionsCount = {read=FPooledConnectionsCount, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TScValidateThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TScConnectionPoolManager* FManager;
	System::Syncobjs::TEvent* FEvent;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TScValidateThread(TScConnectionPoolManager* Manager);
	__fastcall virtual ~TScValidateThread();
	HIDESBASE void __fastcall Terminate();
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TScConnectionPoolManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Crtypes::TCRObjectList* FPools;
	TScValidateThread* FValidateThread;
	
protected:
	System::Syncobjs::TCriticalSection* FLockGet;
	System::Syncobjs::TCriticalSection* FLockList;
	__classmethod int __fastcall GetPoolManagerIndex();
	__classmethod void __fastcall SetPoolManagerIndex(int Value);
	TScConnectionPool* __fastcall CreateConnectionPool(TScSecureConnectionParameters* ConnectionParameters);
	TScConnectionPool* __fastcall GetConnectionPool(TScSecureConnectionParameters* ConnectionParameters);
	void __fastcall InternalClear();
	TScSecureConnection* __fastcall InternalGetConnection(TScSecureConnectionParameters* ConnectionParameters);
	
public:
	__fastcall TScConnectionPoolManager();
	__fastcall virtual ~TScConnectionPoolManager();
	__classmethod TScSecureConnection* __fastcall GetConnection(TScSecureConnectionParameters* ConnectionParameters);
	__classmethod void __fastcall Clear();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScSecureConnectionParameters : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Crvio::TProxyOptions* FProxyOptions;
	Crvio::TSSLOptions* FSSLOptions;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	int MinPoolSize;
	int MaxPoolSize;
	int ConnectionLifeTime;
	System::UnicodeString ConnectionGroupName;
	System::UnicodeString ProviderName;
	System::UnicodeString BindAddress;
	System::UnicodeString Hostname;
	int Port;
	Crvio::TIPVersion IPVersion;
	int Timeout;
	bool IsSecure;
	Crvio::TCRIOHandler* IOHandler;
	__fastcall TScSecureConnectionParameters();
	__fastcall virtual ~TScSecureConnectionParameters();
	HIDESBASE bool __fastcall Equals(TScSecureConnectionParameters* ConnectionParameters);
	__property Crvio::TProxyOptions* ProxyOptions = {read=FProxyOptions};
	__property Crvio::TSSLOptions* SSLOptions = {read=FSSLOptions};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TScSecureConnection : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FRefCount;
	System::DynamicArray<System::Byte> FTmpBuffer;
	System::DynamicArray<System::Byte> FReadBuffer;
	int FReadPos;
	int FWritePos;
	System::Syncobjs::TCriticalSection* FLock;
	Crvio::TCRVio* FVio;
	bool FIsSecure;
	unsigned FConnectionTime;
	int FPoolVersion;
	TScConnectionPool* FPool;
	bool FIsConnected;
	bool FIsDisconnected;
	System::Classes::TNotifyEvent FOnAsyncReceive;
	void __fastcall SetOnAsyncReceive(System::Classes::TNotifyEvent Value);
	System::Classes::TNotifyEvent __fastcall GetAfterDisconnect();
	void __fastcall SetAfterDisconnect(System::Classes::TNotifyEvent Value);
	bool __fastcall GetIsSecure();
	void __fastcall SetIsSecure(bool Value);
	void __fastcall DefragBuffer();
	int __fastcall CopyData(const char * Buffer, int Offset, int Count);
	void __fastcall CreateSSLIOHandler(TScSecureConnectionParameters* ConnectionParameters);
	
public:
	__fastcall TScSecureConnection();
	__fastcall virtual ~TScSecureConnection();
	void __fastcall AddRef();
	void __fastcall Release();
	void __fastcall CreateVio(TScSecureConnectionParameters* ConnectionParameters);
	void __fastcall InitVio(TScSecureConnectionParameters* ConnectionParameters, Crviotcp::TCRVioTcp* AVio);
	void __fastcall Bind(const System::UnicodeString BindAddress);
	void __fastcall Connect();
	void __fastcall Abort();
	void __fastcall Disconnect();
	bool __fastcall CheckIsConnected();
	bool __fastcall IsValid();
	void __fastcall TryReturnToPool();
	int __fastcall Write(const char * Buffer, int Offset, int Count)/* overload */;
	void __fastcall Write(const System::DynamicArray<System::Byte> Buffer)/* overload */;
	void __fastcall WriteLine(const System::UnicodeString Str, Clrclasses::Encoding* AEncoding = (Clrclasses::Encoding*)(0x0));
	int __fastcall ReadNoWait(const char * Buffer, int Offset, int Count);
	int __fastcall Read(const char * Buffer, int Offset, int Count);
	bool __fastcall WaitForData(int MillisecondsTimeout);
	void __fastcall ClearBuffer();
	System::UnicodeString __fastcall ReadLine(Clrclasses::TScCancellationToken* CancellationToken)/* overload */;
	System::UnicodeString __fastcall ReadLine(Clrclasses::Encoding* AEncoding = (Clrclasses::Encoding*)(0x0), Clrclasses::TScCancellationToken* CancellationToken = (Clrclasses::TScCancellationToken*)(0x0))/* overload */;
	int __fastcall CheckForDataOnSource(int NeedCount, int TimeoutMSec = 0xffffffff);
	void __fastcall SetReadWriteTimeout(int Value);
	void __fastcall SetSocketOption(int OptionLevel, int OptionName, int OptionValue);
	void __fastcall RaiseLastError();
	void __fastcall Renegotiate();
	System::UnicodeString __fastcall GetLocalIP();
	int __fastcall GetLocalPort();
	System::UnicodeString __fastcall GetRemoteIP();
	int __fastcall GetRemotePort();
	__property bool IsConnected = {read=FIsConnected, nodefault};
	__property bool IsSecure = {read=GetIsSecure, write=SetIsSecure, nodefault};
	__property System::Classes::TNotifyEvent OnAsyncReceive = {read=FOnAsyncReceive, write=SetOnAsyncReceive};
	__property System::Classes::TNotifyEvent AfterDisconnect = {read=GetAfterDisconnect, write=SetAfterDisconnect};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TScSecureConnectionStream : public System::Classes::TStream
{
	typedef System::Classes::TStream inherited;
	
private:
	TScSecureConnection* FConnection;
	
protected:
	virtual __int64 __fastcall GetSize();
	
public:
	__fastcall TScSecureConnectionStream(TScSecureConnection* AOwner);
	virtual int __fastcall Read(void *Buffer, int Count)/* overload */;
	virtual int __fastcall Write(const void *Buffer, int Count)/* overload */;
	virtual __int64 __fastcall Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TScSecureConnectionStream() { }
	
	/* Hoisted overloads: */
	
public:
	inline int __fastcall  Read(System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline int __fastcall  Read(System::DynamicArray<System::Byte> &Buffer, int Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Count){ return System::Classes::TStream::Write(Buffer, Count); }
	inline int __fastcall  Seek(int Offset, System::Word Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScHttpParser : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__classmethod System::UnicodeString __fastcall ParseFragment(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall ParseNetworkLocation(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall ParseQuery(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall ParseParameters(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall ParseResource(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall ParsePath(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall ParsePassword(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall ParseUserPassword(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall ParsePort(System::UnicodeString &ParseString);
	__classmethod System::UnicodeString __fastcall GetToEnd(const System::WideChar FindChar, System::UnicodeString &ParseString, const bool KeepFirst);
	
public:
	__classmethod System::UnicodeString __fastcall ParseScheme(System::UnicodeString &ParseString);
	__classmethod void __fastcall ParseURL(const System::UnicodeString URL, /* out */ System::UnicodeString &Scheme, /* out */ System::UnicodeString &User, /* out */ System::UnicodeString &Password, /* out */ System::UnicodeString &NetworkLocation, /* out */ System::UnicodeString &Port, /* out */ System::UnicodeString &Path, /* out */ System::UnicodeString &Resource, /* out */ System::UnicodeString &Parameters, /* out */ System::UnicodeString &Query, /* out */ System::UnicodeString &Fragment);
	__classmethod System::UnicodeString __fastcall NthWord(const System::UnicodeString InputString, const System::WideChar Delimiter, int Number);
	__classmethod int __fastcall WordsCount(const System::UnicodeString InputString, const System::WideChar Delimiter);
	__classmethod void __fastcall ParseKeyValue(const System::UnicodeString Token, /* out */ System::UnicodeString &Key, /* out */ System::UnicodeString &Value);
	__classmethod System::DynamicArray<System::UnicodeString> __fastcall ParseTokens(const System::UnicodeString Tokens, const System::WideChar Delimiter);
	__classmethod int __fastcall FindToken(const System::UnicodeString Token, const System::UnicodeString Tokens, const System::WideChar Delimiter);
public:
	/* TObject.Create */ inline __fastcall TScHttpParser() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TScHttpParser() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 DefValMaxPoolSize = System::Int8(0x64);
static const System::Int8 StatisticsCount = System::Int8(0x8);
static const System::Word SEND_BLOCK_SIZE = System::Word(0x8000);
static const System::Word READ_BUFFER_SIZE = System::Word(0x8000);
static const System::Word UNBUFFERED_READ_MIN_SIZE = System::Word(0x800);
}	/* namespace Crsecureconnection */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRSECURECONNECTION)
using namespace Crsecureconnection;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrsecureconnectionHPP
