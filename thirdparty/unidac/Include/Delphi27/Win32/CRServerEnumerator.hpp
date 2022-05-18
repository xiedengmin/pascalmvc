// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRServerEnumerator.pas' rev: 34.00 (Windows)

#ifndef CrserverenumeratorHPP
#define CrserverenumeratorHPP

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

//-- user supplied -----------------------------------------------------------

namespace Crserverenumerator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRServerEnumerator;
struct TCRServiceInfo;
class DELPHICLASS TCRServicesThread;
class DELPHICLASS TCRServiceNamesThread;
class DELPHICLASS TCRNetManager;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRServerEnumerator : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__fastcall virtual TCRServerEnumerator();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	virtual void __fastcall GetServerList(System::Classes::TStrings* List);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCRServerEnumerator() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TCRServerEnumeratorClass;

enum DECLSPEC_DENUM TCRServiceStatus : unsigned char { ssStopped, ssStartPending, ssStopPending, ssRunning, ssContinuePending, ssPausePending, ssPaused };

struct DECLSPEC_DRECORD TCRServiceInfo
{
public:
	System::UnicodeString ServiceName;
	System::UnicodeString DisplayName;
	TCRServiceStatus Status;
};


typedef System::DynamicArray<TCRServiceInfo> TCRServicesInfo;

class PASCALIMPLEMENTATION TCRServicesThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	System::Classes::TStrings* FList;
	System::UnicodeString FKeywords;
	
protected:
	__property Terminated;
	virtual void __fastcall Execute();
	
public:
	__fastcall TCRServicesThread(System::Classes::TStrings* List, const System::UnicodeString Keywords);
	__fastcall virtual ~TCRServicesThread();
};


class PASCALIMPLEMENTATION TCRServiceNamesThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
protected:
	System::UnicodeString FKeywords;
	System::UnicodeString FServer;
	TCRServicesInfo FServiceNames;
	virtual void __fastcall Execute();
	
public:
	__fastcall TCRServiceNamesThread(const System::UnicodeString Server, const System::UnicodeString Keywords);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TCRServiceNamesThread() { }
	
};


typedef NativeUInt SC_HANDLE;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRNetManager : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::Syncobjs::TCriticalSection* FServicesCS;
	System::Classes::TStringList* FCachedServerList;
	unsigned FLastTickCount;
	__classmethod void __fastcall ServiceManagerOpen(const System::UnicodeString Server, const bool ReadOnly, /* out */ NativeUInt &sch);
	__classmethod void __fastcall ServiceManagerClose(const NativeUInt sch);
	__classmethod void __fastcall ServiceOpen(const System::UnicodeString Server, const System::UnicodeString ServiceName, const bool ReadOnly, /* out */ NativeUInt &sch, /* out */ NativeUInt &sh);
	__classmethod void __fastcall ServiceClose(const NativeUInt sch, const NativeUInt sh);
	void __fastcall ClearCachedServerList();
	void __fastcall AddToCachedServerList(const System::UnicodeString Keywords, const System::UnicodeString Server);
	
public:
	__fastcall TCRNetManager();
	__fastcall virtual ~TCRNetManager();
	__classmethod TCRServicesInfo __fastcall GetServiceNames(const System::UnicodeString Server);
	__classmethod TCRServiceStatus __fastcall GetServiceStatus(const System::UnicodeString Server, const System::UnicodeString ServiceName);
	__classmethod void __fastcall ServiceStart(const System::UnicodeString Server, const System::UnicodeString ServiceName, System::UnicodeString ParamStr = System::UnicodeString());
	__classmethod void __fastcall ServiceStop(const System::UnicodeString Server, const System::UnicodeString ServiceName);
	__classmethod void __fastcall GetServerList(System::Classes::TStrings* List)/* overload */;
	void __fastcall GetServerList(System::Classes::TStrings* List, const System::UnicodeString Keywords, const unsigned Timeout = (unsigned)(0x1), const unsigned CacheTimeout = (unsigned)(0x78))/* overload */;
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TCRNetManager* CRNetManager;
}	/* namespace Crserverenumerator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRSERVERENUMERATOR)
using namespace Crserverenumerator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrserverenumeratorHPP
