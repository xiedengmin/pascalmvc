// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DASQLMonitor.pas' rev: 34.00 (Windows)

#ifndef DasqlmonitorHPP
#define DasqlmonitorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Data.DB.hpp>
#include <System.Types.hpp>
#include <CRTypes.hpp>
#include <MemData.hpp>
#include <DBAccess.hpp>
#include <DBMonitorClient.hpp>
#include <DBMonitorMessages.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dasqlmonitor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDBMonitorOptions;
class DELPHICLASS TCustomDASQLMonitor;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDATraceFlag : unsigned char { tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect, tfTransact, tfBlob, tfService, tfMisc, tfParams, tfObjDestroy, tfPool };

typedef System::Set<TDATraceFlag, TDATraceFlag::tfQPrepare, TDATraceFlag::tfPool> TDATraceFlags;

enum DECLSPEC_DENUM TMonitorOption : unsigned char { moDialog, moSQLMonitor, moDBMonitor, moCustom, moHandled };

typedef System::Set<TMonitorOption, TMonitorOption::moDialog, TMonitorOption::moHandled> TMonitorOptions;

typedef void __fastcall (__closure *TOnSQLEvent)(System::TObject* Sender, System::UnicodeString Text, TDATraceFlag Flag);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBMonitorOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FHost;
	int FPort;
	int FReconnectTimeout;
	int FSendTimeout;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TDBMonitorOptions();
	
__published:
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, default=1000};
	__property int ReconnectTimeout = {read=FReconnectTimeout, write=FReconnectTimeout, default=5000};
	__property int SendTimeout = {read=FSendTimeout, write=FSendTimeout, default=1000};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDBMonitorOptions() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDASQLMonitorClass;

class PASCALIMPLEMENTATION TCustomDASQLMonitor : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FActive;
	TDATraceFlags FTraceFlags;
	TMonitorOptions FOptions;
	bool FStreamedActive;
	bool FRegistered;
	System::_di_IInterface FSMClient;
	Dbmonitorclient::TDBMonitor* FDBMonitor;
	TDBMonitorOptions* FDBMonitorOptions;
	void __fastcall SetActive(bool Value);
	void __fastcall SetOptions(TMonitorOptions Value);
	void __fastcall SetDBMonitorOptions(TDBMonitorOptions* Value);
	
protected:
	TOnSQLEvent FOnSQLEvent;
	virtual void __fastcall Loaded();
	void __fastcall CheckActive();
	virtual void __fastcall RegisterClient();
	virtual void __fastcall UnRegisterClient();
	void __fastcall AddStatement(System::UnicodeString St);
	void __fastcall SMClientSignal(System::TObject* Sender, int Data);
	virtual bool __fastcall NeedAutoActivate();
	void __fastcall StartDBMonitor();
	void __fastcall SendDBMonitorEvent(bool BeforeEvent, int EventType, const System::UnicodeString Description, System::TObject* Obj, const System::UnicodeString SQL, const Dbmonitormessages::TMsgSQLParams Params, bool Failed, const System::UnicodeString ErrorText, unsigned &EventID);
	System::UnicodeString __fastcall GetObjectHandle(System::TObject* Obj);
	virtual System::TObject* __fastcall GetParent(System::TObject* Obj);
	virtual int __fastcall GetObjectType(System::TObject* Obj);
	void __fastcall InternalSQLPrepare(System::TObject* Obj, const System::UnicodeString SQL, Dbaccess::TDAParams* Params, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalSQLUnprepare(System::TObject* Obj, const System::UnicodeString SQL, Dbaccess::TDAParams* Params, bool BeforeEvent, unsigned &MessageID);
	virtual void __fastcall InternalSQLExecute(System::TObject* Obj, const System::UnicodeString SQL, Dbaccess::TDAParams* Params, const System::UnicodeString Caption, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalDBConnect(Dbaccess::TCustomDAConnection* Connection, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalDBDisconnect(Dbaccess::TCustomDAConnection* Connection, bool BeforeEvent, unsigned &MessageID);
	virtual void __fastcall InternalTRStart(System::TObject* Obj, bool BeforeEvent, unsigned &MessageID);
	virtual void __fastcall InternalTRCommit(System::TObject* Obj, bool BeforeEvent, unsigned &MessageID);
	virtual void __fastcall InternalTRRollback(System::TObject* Obj, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalTRSavepoint(System::TObject* Obj, const System::UnicodeString Savepoint, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalTRRollbackToSavepoint(System::TObject* Obj, const System::UnicodeString Savepoint, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalTRReleaseSavepoint(System::TObject* Obj, const System::UnicodeString Savepoint, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalTRCommitRetaining(System::TObject* Obj, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalTRRollbackRetaining(System::TObject* Obj, bool BeforeEvent, unsigned &MessageID);
	void __fastcall InternalDBError(Dbaccess::EDAError* Exception);
	void __fastcall InternalCustomMessage(System::TObject* Obj, const System::UnicodeString Msg);
	void __fastcall InternalObjectDestroyed(System::TObject* Obj);
	void __fastcall InternalPoolMessage(System::TObject* Obj, const System::UnicodeString Msg, bool WithCount, bool WithParams);
	__classmethod virtual TCustomDASQLMonitor* __fastcall GetMonitor();
	virtual void __fastcall SetMonitor() = 0 ;
	virtual bool __fastcall IsTransactionIDSupported(System::TObject* Obj);
	
public:
	__classmethod void __fastcall SQLPrepare(System::TObject* Obj, const System::UnicodeString SQL, Dbaccess::TDAParams* Params, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall SQLUnprepare(System::TObject* Obj, const System::UnicodeString SQL, Dbaccess::TDAParams* Params, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall SQLExecute(System::TObject* Obj, const System::UnicodeString SQL, Dbaccess::TDAParams* Params, const System::UnicodeString Caption, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall DBConnect(Dbaccess::TCustomDAConnection* Connection, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall DBDisconnect(Dbaccess::TCustomDAConnection* Connection, unsigned &MessageID, bool BeforeEvent);
	__classmethod virtual void __fastcall TRStart(System::TObject* Obj, unsigned &MessageID, bool BeforeEvent);
	__classmethod virtual void __fastcall TRCommit(System::TObject* Obj, unsigned &MessageID, bool BeforeEvent);
	__classmethod virtual void __fastcall TRRollback(System::TObject* Obj, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall TRSavepoint(System::TObject* Obj, const System::UnicodeString Savepoint, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall TRRollbackToSavepoint(System::TObject* Obj, const System::UnicodeString Savepoint, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall TRReleaseSavepoint(System::TObject* Obj, const System::UnicodeString Savepoint, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall TRCommitRetaining(System::TObject* Obj, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall TRRollbackRetaining(System::TObject* Obj, unsigned &MessageID, bool BeforeEvent);
	__classmethod void __fastcall DBError(Dbaccess::EDAError* Exception);
	__classmethod void __fastcall CustomMessage(System::TObject* Obj, const System::UnicodeString Msg);
	__classmethod void __fastcall ObjectDestroyed(System::TObject* Obj);
	__classmethod void __fastcall PoolMessage(System::TObject* Obj, const System::UnicodeString Msg, bool WithCount, bool WithParams = false);
	__classmethod bool __fastcall HasMonitor();
	__classmethod virtual System::UnicodeString __fastcall GetParamDataType(Dbaccess::TDAParam* Param);
	__classmethod virtual System::UnicodeString __fastcall GetParamParamType(Dbaccess::TDAParam* Param);
	__classmethod virtual System::UnicodeString __fastcall GetParamValue(Dbaccess::TDAParam* Param);
	__classmethod System::UnicodeString __fastcall GetParam(Dbaccess::TDAParam* Param, Dbmonitormessages::TMsgSQLParam &SQLParam);
	__classmethod System::UnicodeString __fastcall GetParams(Dbaccess::TDAParams* Params, /* out */ Dbmonitormessages::TMsgSQLParams &SQLParams)/* overload */;
	__classmethod System::UnicodeString __fastcall GetParams(Dbaccess::TDAParams* Params)/* overload */;
	__classmethod virtual System::UnicodeString __fastcall GetCaption();
	__classmethod void __fastcall ShowDebug(System::TObject* Obj, const System::UnicodeString SQL, Dbaccess::TDAParams* Params, const System::UnicodeString Caption);
	__fastcall virtual TCustomDASQLMonitor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDASQLMonitor();
	__property bool Active = {read=FActive, write=SetActive, default=1};
	__property TDATraceFlags TraceFlags = {read=FTraceFlags, write=FTraceFlags, default=1643};
	__property TMonitorOptions Options = {read=FOptions, write=SetOptions, default=15};
	__property TDBMonitorOptions* DBMonitorOptions = {read=FDBMonitorOptions, write=SetDBMonitorOptions};
	__property TOnSQLEvent OnSQL = {read=FOnSQLEvent, write=FOnSQLEvent};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall (*ShowDebugFormProc)(TDASQLMonitorClass DASQLMonitorClass, System::Classes::TComponent* Component, System::UnicodeString SQL, Dbaccess::TDAParams* Params, System::UnicodeString Caption);
extern DELPHI_PACKAGE void __fastcall (*GetCallStackProc)(System::DynamicArray<System::WideString> &ACallStack);
extern DELPHI_PACKAGE int __fastcall GetObjectID(System::TObject* Obj);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetObjectName(System::TObject* Obj);
}	/* namespace Dasqlmonitor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASQLMONITOR)
using namespace Dasqlmonitor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DasqlmonitorHPP
