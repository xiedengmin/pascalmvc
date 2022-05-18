// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRThread.pas' rev: 34.00 (Windows)

#ifndef CrthreadHPP
#define CrthreadHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.Types.hpp>
#include <Winapi.Windows.hpp>
#include <CRTypes.hpp>
#include <CLRClasses.hpp>
#include <CRFunctions.hpp>
#include <CRTimer.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crthread
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRThread;
class DELPHICLASS TCRThreadWrapper;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TCRThreadTerminateEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TCRThreadExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E, bool &Fail);

typedef void __fastcall (__closure *TCRThreadEvent)(System::TObject* Sender, void * Event);

class PASCALIMPLEMENTATION TCRThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
protected:
	TCRThreadWrapper* FOwner;
	System::Syncobjs::TEvent* FStartedEvent;
	System::Syncobjs::TEvent* FResumedEvent;
	virtual void __fastcall Execute();
	
public:
	__fastcall virtual TCRThread(TCRThreadWrapper* Owner);
	__fastcall virtual ~TCRThread();
};


enum DECLSPEC_DENUM TCRThreadState : unsigned char { tsSuspended, tsExecuting, tsTerminating, tsFinished };

class PASCALIMPLEMENTATION TCRThreadWrapper : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Crtimer::TCRIntervalProcessor* FIntervalProcessor;
	TCRThread* FThread;
	TCRThreadEvent FOnPostEvent;
	TCRThreadEvent FOnSendEvent;
	TCRThreadExceptionEvent FOnException;
	TCRThreadTerminateEvent FOnTerminate;
	System::Syncobjs::TCriticalSection* FLockState;
	System::Classes::TThreadList* FEvents;
	System::Sysutils::Exception* FException;
	TCRThreadState FThreadState;
	bool FDoTimerProcessing;
	bool FLockDestroy;
	bool FDestroyAfterTimer;
	void *FSendEvent;
	System::Syncobjs::TEvent* FSendEventProcessed;
	void __fastcall ProcessException();
	void __fastcall DoTerminate();
	void __fastcall SetTimer();
	void __fastcall KillTimer();
	void __fastcall DoTimer(System::TObject* Sender);
	
protected:
	virtual void __fastcall InternalExecute();
	virtual System::Sysutils::Exception* __fastcall CloneException(System::Sysutils::Exception* E);
	void __fastcall DoException(System::Sysutils::Exception* E);
	
public:
	__fastcall TCRThreadWrapper(bool ForceStartTimer);
	__fastcall virtual ~TCRThreadWrapper();
	void __fastcall PostEvent(void * Event);
	void __fastcall SendEvent(void * Event);
	void __fastcall WaitFor();
	void __fastcall Resume();
	void __fastcall TryTerminate();
	void __fastcall WaitForExit();
	bool __fastcall Terminated();
	bool __fastcall InThread();
	__property System::Sysutils::Exception* LastException = {read=FException};
	__property TCRThreadEvent OnPostEvent = {read=FOnPostEvent, write=FOnPostEvent};
	__property TCRThreadEvent OnSendEvent = {read=FOnSendEvent, write=FOnSendEvent};
	__property TCRThreadExceptionEvent OnException = {read=FOnException, write=FOnException};
	__property TCRThreadTerminateEvent OnTerminate = {read=FOnTerminate, write=FOnTerminate};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool UseAsyncEventProcessor;
extern DELPHI_PACKAGE void __fastcall StartAsyncEventProcessor(bool Force = false);
}	/* namespace Crthread */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRTHREAD)
using namespace Crthread;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrthreadHPP
