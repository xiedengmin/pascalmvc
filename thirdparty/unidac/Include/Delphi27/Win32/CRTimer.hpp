// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRTimer.pas' rev: 34.00 (Windows)

#ifndef CrtimerHPP
#define CrtimerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.SyncObjs.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <MemData.hpp>
#include <DAConsts.hpp>
#include <CRFunctions.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crtimer
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRTimer;
class DELPHICLASS TCRIntervalThread;
class DELPHICLASS TCRIntervalProcessor;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCRTimer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	unsigned FInterval;
	HWND FWindowHandle;
	System::Classes::TNotifyEvent FOnTimer;
	bool FEnabled;
	void __fastcall UpdateTimer();
	void __fastcall SetEnabled(bool Value);
	void __fastcall SetInterval(unsigned Value);
	void __fastcall SetOnTimer(System::Classes::TNotifyEvent Value);
	
protected:
	DYNAMIC void __fastcall Timer();
	
public:
	__fastcall virtual TCRTimer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCRTimer();
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property unsigned Interval = {read=FInterval, write=SetInterval, default=1000};
	__property System::Classes::TNotifyEvent OnTimer = {read=FOnTimer, write=SetOnTimer};
};


class PASCALIMPLEMENTATION TCRIntervalThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	System::Syncobjs::TEvent* FIntervalEvent;
	unsigned FInterval;
	System::Classes::TNotifyEvent FOnTimer;
	bool FEnabled;
	void __fastcall SetEnabled(bool Value);
	void __fastcall SetInterval(unsigned Value);
	void __fastcall SetOnTimer(System::Classes::TNotifyEvent Value);
	void __fastcall UpdateThread();
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TCRIntervalThread(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCRIntervalThread();
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property unsigned Interval = {read=FInterval, write=SetInterval, default=1000};
	__property System::Classes::TNotifyEvent OnTimer = {read=FOnTimer, write=SetOnTimer};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRIntervalProcessor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCRTimer* FTimer;
	TCRIntervalThread* FIntervalThread;
	bool __fastcall GetEnabled();
	void __fastcall SetEnabled(bool Value);
	unsigned __fastcall GetInterval();
	void __fastcall SetInterval(unsigned Value);
	System::Classes::TNotifyEvent __fastcall GetOnTimer();
	void __fastcall SetOnTimer(System::Classes::TNotifyEvent Value);
	
public:
	__fastcall TCRIntervalProcessor(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCRIntervalProcessor();
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, nodefault};
	__property unsigned Interval = {read=GetInterval, write=SetInterval, nodefault};
	__property System::Classes::TNotifyEvent OnTimer = {read=GetOnTimer, write=SetOnTimer};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crtimer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRTIMER)
using namespace Crtimer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrtimerHPP
