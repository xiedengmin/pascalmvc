// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniLoader.pas' rev: 34.00 (Windows)

#ifndef UniloaderHPP
#define UniloaderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <Data.DB.hpp>
#include <CRTypes.hpp>
#include <CRAccess.hpp>
#include <MemUtils.hpp>
#include <MemData.hpp>
#include <MemDS.hpp>
#include <DBAccess.hpp>
#include <DALoader.hpp>
#include <Uni.hpp>
#include <UniProvider.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniloader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniLoaderColumn;
class DELPHICLASS TUniLoader;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniLoaderColumn : public Daloader::TDAColumn
{
	typedef Daloader::TDAColumn inherited;
	
private:
	int FSize;
	int FPrecision;
	int FScale;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property int Size = {read=FSize, write=FSize, default=0};
	__property int Precision = {read=FPrecision, write=FPrecision, default=0};
	__property int Scale = {read=FScale, write=FScale, default=0};
public:
	/* TDAColumn.Create */ inline __fastcall virtual TUniLoaderColumn(System::Classes::TCollection* Collection) : Daloader::TDAColumn(Collection) { }
	
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TUniLoaderColumn() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TUniLoader : public Daloader::TDALoader
{
	typedef Daloader::TDALoader inherited;
	
private:
	Uni::TSpecificOptionsHolder* FSpecificOptions;
	bool FUseBlankValues;
	Uni::TUniConnection* __fastcall GetConnection();
	HIDESBASE void __fastcall SetConnection(Uni::TUniConnection* Value);
	Uni::TUniTransaction* __fastcall GetUniTransaction();
	void __fastcall SetUniTransaction(Uni::TUniTransaction* Value);
	Uni::TSpecificOptionsList* __fastcall GetSpecificOptions();
	void __fastcall SetSpecificOptions(Uni::TSpecificOptionsList* Value);
	void __fastcall SetUseBlankValues(const bool Value);
	
protected:
	bool __fastcall CanGetProvider();
	Uniprovider::TUniProvider* __fastcall GetProvider();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual Craccess::TCRLoaderClass __fastcall GetInternalLoaderClass();
	virtual void __fastcall SetInternalLoader(Craccess::TCRLoader* Value);
	__classmethod virtual Daloader::TDAColumnClass __fastcall GetColumnClass();
	virtual Memds::TFieldTypeMapClass __fastcall GetFieldTypeMapClass();
	virtual bool __fastcall NeedRecreateColumns();
	virtual void __fastcall ReadColumn(Daloader::TDAColumn* Column, Craccess::TCRLoaderColumn* CRColumn);
	virtual void __fastcall WriteColumn(Daloader::TDAColumn* Column, Craccess::TCRLoaderColumn* CRColumn);
	virtual void __fastcall BeginConnection();
	
public:
	__fastcall virtual TUniLoader(System::Classes::TComponent* Owner);
	__fastcall virtual ~TUniLoader();
	
__published:
	__property Uni::TUniConnection* Connection = {read=GetConnection, write=SetConnection};
	__property Uni::TUniTransaction* Transaction = {read=GetUniTransaction, write=SetUniTransaction, stored=IsTransactionStored};
	__property Uni::TSpecificOptionsList* SpecificOptions = {read=GetSpecificOptions, write=SetSpecificOptions};
	__property bool UseBlankValues = {read=FUseBlankValues, write=SetUseBlankValues, default=1};
	__property TableName = {default=0};
	__property Columns;
	__property OnPutData;
	__property OnGetColumnData;
	__property OnProgress;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Uniloader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNILOADER)
using namespace Uniloader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniloaderHPP
