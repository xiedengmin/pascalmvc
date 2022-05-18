// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAAlerter.pas' rev: 34.00 (Windows)

#ifndef DaalerterHPP
#define DaalerterHPP

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
#include <System.Variants.hpp>
#include <CRTypes.hpp>
#include <CRAccess.hpp>
#include <DBAccess.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daalerter
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAAlerter;
class DELPHICLASS TDAAlerterUtils;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TAlerterEventEvent)(TDAAlerter* Sender, const System::UnicodeString EventName, const System::UnicodeString Message);

typedef void __fastcall (__closure *TAlerterErrorEvent)(TDAAlerter* Sender, System::Sysutils::Exception* E);

class PASCALIMPLEMENTATION TDAAlerter : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	void __fastcall SetConnection(Dbaccess::TCustomDAConnection* Value);
	void __fastcall SetActive(bool Value);
	void __fastcall SetAutoRegister(bool Value);
	System::UnicodeString __fastcall GetEvents();
	void __fastcall SetEvents(const System::UnicodeString Value);
	void __fastcall SetAutoCommit(bool Value);
	void __fastcall ConnectChange(System::TObject* Sender, bool Connecting);
	bool __fastcall CanRegisterEvents();
	
protected:
	bool FDesignCreate;
	Craccess::TCRAlerter* FIAlerter;
	Dbaccess::TCustomDAConnection* FConnection;
	Dbaccess::TDATransaction* FTransaction;
	bool FActive;
	bool FStreamedActive;
	bool FAutoRegister;
	System::Classes::TStrings* FEvents;
	bool FAutoCommit;
	TAlerterEventEvent FOnEvent;
	TAlerterErrorEvent FOnError;
	bool FInEndConnection;
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* Component, System::Classes::TOperation Operation);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual Craccess::TCRAlerterClass __fastcall GetInternalAlerterClass();
	virtual void __fastcall SetInternalAlerter(Craccess::TCRAlerter* Value);
	void __fastcall CreateInternalAlerter();
	void __fastcall FreeInternalAlerter();
	void __fastcall CheckInternalAlerter();
	bool __fastcall IsTransactionStored();
	virtual Dbaccess::TDATransaction* __fastcall GetTransaction();
	virtual void __fastcall SetTransaction(Dbaccess::TDATransaction* Value);
	virtual Dbaccess::TCustomDAConnection* __fastcall UsedConnection();
	virtual Dbaccess::TDATransaction* __fastcall UsedTransaction();
	virtual void __fastcall BeginConnection(bool WithTransaction = true);
	void __fastcall EndConnection(bool WithTransaction = true);
	void __fastcall CommitData();
	virtual void __fastcall DoOnEvent(const System::UnicodeString EventName, const System::UnicodeString Message);
	void __fastcall DoOnError(System::Sysutils::Exception* E);
	__property Dbaccess::TDATransaction* Transaction = {read=GetTransaction, write=SetTransaction, stored=IsTransactionStored};
	__property System::UnicodeString Events = {read=GetEvents, write=SetEvents};
	__property bool AutoCommit = {read=FAutoCommit, write=SetAutoCommit, default=1};
	__property TAlerterEventEvent OnEvent = {read=FOnEvent, write=FOnEvent};
	
public:
	__fastcall virtual TDAAlerter(System::Classes::TComponent* Owner);
	__fastcall virtual ~TDAAlerter();
	void __fastcall SendEvent(const System::UnicodeString EventName, const System::UnicodeString Message);
	void __fastcall Start();
	void __fastcall Stop();
	__property Dbaccess::TCustomDAConnection* Connection = {read=FConnection, write=SetConnection};
	__property bool Active = {read=FActive, write=SetActive, default=0};
	__property bool AutoRegister = {read=FAutoRegister, write=SetAutoRegister, default=0};
	__property TAlerterErrorEvent OnError = {read=FOnError, write=FOnError};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAAlerterUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall SetDesignCreate(TDAAlerter* Obj, bool Value);
	__classmethod bool __fastcall GetDesignCreate(TDAAlerter* Obj);
	__classmethod Dbaccess::TDATransaction* __fastcall GetTransaction(TDAAlerter* Obj);
	__classmethod void __fastcall SetTransaction(TDAAlerter* Obj, Dbaccess::TDATransaction* Value);
	__classmethod Dbaccess::TDATransaction* __fastcall GetFTransaction(TDAAlerter* Obj);
public:
	/* TObject.Create */ inline __fastcall TDAAlerterUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDAAlerterUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daalerter */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DAALERTER)
using namespace Daalerter;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaalerterHPP
