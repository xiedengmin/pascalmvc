// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniDacVcl.pas' rev: 35.00 (Windows)

#ifndef UnidacvclHPP
#define UnidacvclHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Data.DB.hpp>
#include <CRParser.hpp>
#include <CRAccess.hpp>
#include <CRServerEnumerator.hpp>
#include <DBAccess.hpp>
#include <DacVcl.hpp>
#include <UniProvider.hpp>
#include <Uni.hpp>
#include <UniConnectForm.hpp>

//-- user supplied -----------------------------------------------------------

namespace Unidacvcl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniConnectDialog;
class DELPHICLASS TUniConnectDialogUtils;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUniConnectDialog : public Dbaccess::TCustomConnectDialog
{
	typedef Dbaccess::TCustomConnectDialog inherited;
	
private:
	Uni::TUniConnection* __fastcall GetConnection();
	
protected:
	System::UnicodeString FDatabaseLabel;
	System::UnicodeString FPortLabel;
	System::UnicodeString FProviderLabel;
	Uniprovider::TConnectDialogService* FConnectDialogService;
	virtual Crserverenumerator::TCRServerEnumeratorClass __fastcall GetServerEnumeratorClass();
	virtual void __fastcall SetServerEnumerator(Crserverenumerator::TCRServerEnumerator* Value);
	void __fastcall CreateConnectDialogService();
	void __fastcall FreeConnectDialogService();
	void __fastcall CheckConnectDialogService();
	virtual void __fastcall SetLabelSet(Dbaccess::TLabelSet Value);
	virtual System::TClass __fastcall DefDialogClass();
	virtual System::UnicodeString __fastcall GetKeyPath();
	virtual System::UnicodeString __fastcall GetServerListKeyPath();
	virtual System::UnicodeString __fastcall GetServerStoreName();
	virtual void __fastcall SaveInfoToRegistry(System::Win::Registry::TRegistry* Registry);
	virtual void __fastcall LoadInfoFromRegistry(System::Win::Registry::TRegistry* Registry)/* overload */;
	HIDESBASE void __fastcall LoadInfoFromRegistry(System::Win::Registry::TRegistry* Registry, bool LoadProviderName)/* overload */;
	void __fastcall ReloadInfoFromRegistry();
	void __fastcall SaveDatabaseListToRegistry();
	void __fastcall LoadDatabaseListFromRegistry(System::Classes::TStrings* List);
	
public:
	__fastcall virtual TUniConnectDialog(System::Classes::TComponent* Owner);
	__fastcall virtual ~TUniConnectDialog();
	virtual bool __fastcall Execute();
	__property Uni::TUniConnection* Connection = {read=GetConnection};
	
__published:
	__property System::UnicodeString DatabaseLabel = {read=FDatabaseLabel, write=FDatabaseLabel};
	__property System::UnicodeString PortLabel = {read=FPortLabel, write=FPortLabel};
	__property System::UnicodeString ProviderLabel = {read=FProviderLabel, write=FProviderLabel};
	__property Retries = {default=3};
	__property SavePassword = {default=0};
	__property DialogClass = {default=0};
	__property Caption = {default=0};
	__property UsernameLabel = {default=0};
	__property PasswordLabel = {default=0};
	__property ServerLabel = {default=0};
	__property ConnectButton = {default=0};
	__property CancelButton = {default=0};
	__property LabelSet = {default=1};
	__property StoreLogInfo = {default=1};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniConnectDialogUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod Uniprovider::TConnectDialogService* __fastcall GetConnectDialogService(TUniConnectDialog* Obj);
	__classmethod void __fastcall ReloadInfoFromRegistry(TUniConnectDialog* Obj);
	__classmethod void __fastcall SaveDatabaseListToRegistry(TUniConnectDialog* Obj);
	__classmethod void __fastcall LoadDatabaseListFromRegistry(TUniConnectDialog* Obj, System::Classes::TStrings* List);
public:
	/* TObject.Create */ inline __fastcall TUniConnectDialogUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TUniConnectDialogUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::TClass __fastcall DefConnectDialogClass(void);
}	/* namespace Unidacvcl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIDACVCL)
using namespace Unidacvcl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UnidacvclHPP
