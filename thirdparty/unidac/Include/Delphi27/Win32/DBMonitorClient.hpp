// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBMonitorClient.pas' rev: 34.00 (Windows)

#ifndef DbmonitorclientHPP
#define DbmonitorclientHPP

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
#include <CRTypes.hpp>
#include <DBMonitorMessages.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dbmonitorclient
{
//-- forward type declarations -----------------------------------------------
struct TMonitorEvent;
class DELPHICLASS TDBMonitor;
class DELPHICLASS TEventSendThread;
//-- type declarations -------------------------------------------------------
typedef Dbmonitormessages::TMsgSQLParam TSQLParam;

typedef Dbmonitormessages::TMsgSQLParams TSQLParams;

struct DECLSPEC_DRECORD TMonitorEvent
{
public:
	unsigned EventID;
	int EventType;
	int ObjectID;
	System::WideString ObjectName;
	int ObjectType;
	System::WideString ObjectTypeName;
	int ParentID;
	System::WideString ParentName;
	int ParentType;
	System::WideString ParentTypeName;
	System::WideString Description;
	System::WideString SQL;
	Dbmonitormessages::TMsgSQLParams Params;
	bool Failed;
	System::WideString ErrorText;
	int RowsAffected;
	System::DynamicArray<System::WideString> CallStack;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBMonitor : public System::TObject
{
	typedef System::TObject inherited;
	
	
private:
	typedef System::DynamicArray<Dbmonitormessages::TEventMessage*> _TDBMonitor__1;
	
	
private:
	Dbmonitormessages::TSocketMessagePacker* FPacker;
	System::WideString FCaption;
	System::UnicodeString FHost;
	int FPort;
	unsigned FReconnectTimeout;
	unsigned FSendTimeout;
	int FLastEventID;
	_TDBMonitor__1 FEventCache;
	int FCacheHead;
	int FCacheTail;
	Dbmonitormessages::TStartupMessage* FStartupMessage;
	System::Syncobjs::TCriticalSection* FAddEventLock;
	System::Syncobjs::TEvent* FNewMesEvent;
	TEventSendThread* FSendThread;
	bool FIsDesigning;
	void __fastcall AddEvent(Dbmonitormessages::TEventMessage* Msg);
	Dbmonitormessages::TStartupMessage* __fastcall GetStartupMessage();
	
public:
	__fastcall TDBMonitor();
	__fastcall virtual ~TDBMonitor();
	void __fastcall Startup();
	void __fastcall Finish();
	bool __fastcall IsMonitorActive();
	void __fastcall SendEvent(TMonitorEvent &Event);
	void __fastcall SendEventStart(TMonitorEvent &Event);
	void __fastcall SendEventEnd(const TMonitorEvent &Event);
	__property System::WideString Caption = {read=FCaption, write=FCaption};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property unsigned ReconnectTimeout = {read=FReconnectTimeout, write=FReconnectTimeout, nodefault};
	__property unsigned SendTimeout = {read=FSendTimeout, write=FSendTimeout, nodefault};
	__property bool IsDesigning = {write=FIsDesigning, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TEventSendThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TDBMonitor* FDBMonitor;
	System::Syncobjs::TEvent* FConnectEvent;
	System::Syncobjs::TEvent* FEndEvent;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TEventSendThread(TDBMonitor* DBMonitor);
	__fastcall virtual ~TEventSendThread();
	HIDESBASE void __fastcall Terminate();
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 ET_APPSTARTED = System::Int8(0x0);
static const System::Int8 ET_APPFINISHED = System::Int8(0x1);
static const System::Int8 ET_CONNECT = System::Int8(0x2);
static const System::Int8 ET_DISCONNECT = System::Int8(0x3);
static const System::Int8 ET_BEGIN_TRANS = System::Int8(0x4);
static const System::Int8 ET_COMMIT = System::Int8(0x5);
static const System::Int8 ET_ROLLBACK = System::Int8(0x6);
static const System::Int8 ET_SAVEPOINT = System::Int8(0x7);
static const System::Int8 ET_PREPARE = System::Int8(0x8);
static const System::Int8 ET_UNPREPARE = System::Int8(0x9);
static const System::Int8 ET_EXECUTE = System::Int8(0xa);
static const System::Int8 ET_FETCH = System::Int8(0xb);
static const System::Int8 ET_BLOB = System::Int8(0xc);
static const System::Int8 ET_OBJ_CREATE = System::Int8(0xd);
static const System::Int8 ET_OBJ_DESTROY = System::Int8(0xe);
static const System::Int8 ET_CONN_POOL = System::Int8(0xf);
static const System::Int8 ET_MISC = System::Int8(0x10);
static const System::Int8 OT_UNKNOWN = System::Int8(0x0);
static const System::Int8 OT_CONNECTION = System::Int8(0x1);
static const System::Int8 OT_TRANSACTION = System::Int8(0x2);
static const System::Int8 OT_COMMAND = System::Int8(0x3);
static const System::Int8 OT_DATAREADER = System::Int8(0x4);
static const System::Int8 OT_CONNPOOL = System::Int8(0x5);
extern DELPHI_PACKAGE TDBMonitor* __fastcall GetDBMonitor(void);
extern DELPHI_PACKAGE void __fastcall FinishDBMonitor(void);
extern DELPHI_PACKAGE bool __fastcall HasMonitor(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WhereMonitor(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetDBMonitorVersion(void);
}	/* namespace Dbmonitorclient */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DBMONITORCLIENT)
using namespace Dbmonitorclient;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DbmonitorclientHPP
