// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DALoader.pas' rev: 34.00 (Windows)

#ifndef DaloaderHPP
#define DaloaderHPP

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
#include <System.Generics.Collections.hpp>
#include <MemData.hpp>
#include <MemDS.hpp>
#include <CRTypes.hpp>
#include <CRAccess.hpp>
#include <DBAccess.hpp>

//-- user supplied -----------------------------------------------------------

namespace Daloader
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAColumn;
class DELPHICLASS TDAColumns;
class DELPHICLASS TDALoaderOptions;
class DELPHICLASS TDALoader;
class DELPHICLASS TDALoaderUtils;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDAColumnDataType : unsigned char { ctString, ctDate, ctInteger, ctUInteger, ctFloat };

typedef System::TMetaClass* TDAColumnClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAColumn : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	Data::Db::TFieldType FFieldType;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual TDAColumnDataType __fastcall GetDataType();
	virtual void __fastcall SetDataType(TDAColumnDataType Value);
	virtual void __fastcall SetFieldType(Data::Db::TFieldType Value);
	virtual System::UnicodeString __fastcall GetDisplayName();
	__property TDAColumnDataType DataType = {read=GetDataType, write=SetDataType, nodefault};
	
public:
	__fastcall virtual TDAColumn(System::Classes::TCollection* Collection);
	
__published:
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property Data::Db::TFieldType FieldType = {read=FFieldType, write=SetFieldType, default=1};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TDAColumn() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAColumns : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TDAColumn* operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FInCreation;
	bool FChangedByUser;
	bool FCreatedFromFields;
	TDAColumn* __fastcall GetColumn(int Index);
	void __fastcall SetColumn(int Index, TDAColumn* Value);
	
protected:
	virtual void __fastcall Notify(System::Classes::TCollectionItem* Item, System::Generics::Collections::TCollectionNotification Action);
	__property bool ChangedByUser = {read=FChangedByUser, nodefault};
	
public:
	__property TDAColumn* Items[int Index] = {read=GetColumn, write=SetColumn/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TDAColumns(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TDAColumns() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TDAPutDataEvent)(TDALoader* Sender);

typedef void __fastcall (__closure *TGetColumnDataEvent)(System::TObject* Sender, TDAColumn* Column, int Row, System::Variant &Value, bool &IsEOF);

typedef void __fastcall (__closure *TLoaderProgressEvent)(System::TObject* Sender, int Percent);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDALoaderOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FQuoteNames;
	bool FUseBlankValues;
	void __fastcall SetQuoteNames(bool Value);
	void __fastcall SetUseBlankValues(bool Value);
	
protected:
	TDALoader* FOwner;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	Craccess::TCRLoader* __fastcall GetInternalLoader();
	
public:
	__fastcall virtual TDALoaderOptions(TDALoader* Owner);
	__property bool QuoteNames = {read=FQuoteNames, write=SetQuoteNames, default=0};
	__property bool UseBlankValues = {read=FUseBlankValues, write=SetUseBlankValues, default=1};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDALoaderOptions() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TDALoader : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FTableName;
	TDAPutDataEvent FOnPutData;
	TGetColumnDataEvent FOnGetColumnData;
	TDALoaderOptions* FOptions;
	void __fastcall SetConnection(Dbaccess::TCustomDAConnection* Value);
	void __fastcall SetColumns(TDAColumns* Value);
	bool __fastcall IsColumnsStored();
	void __fastcall CreateColumnsByFields(Data::Db::TFields* Fields);
	void __fastcall SetOptions(TDALoaderOptions* const Value);
	
protected:
	Craccess::TCRLoader* FILoader;
	TDAColumns* FColumns;
	Dbaccess::TCustomDAConnection* FConnection;
	Dbaccess::TDATransaction* FTransaction;
	bool FAutoCommit;
	bool FDesignCreate;
	TLoaderProgressEvent FOnLoaderProgress;
	virtual Craccess::TCRLoaderClass __fastcall GetInternalLoaderClass();
	virtual void __fastcall SetInternalLoader(Craccess::TCRLoader* Value);
	void __fastcall CreateInternalLoader();
	void __fastcall FreeInternalLoader();
	void __fastcall CheckInternalLoader();
	void __fastcall DoLoaderProgress(int Percent);
	virtual void __fastcall Loaded();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall Notification(System::Classes::TComponent* Component, System::Classes::TOperation Operation);
	virtual void __fastcall BeginConnection();
	virtual void __fastcall EndConnection();
	virtual void __fastcall CommitData();
	virtual void __fastcall InternalPutData();
	virtual void __fastcall PutData();
	void __fastcall SetTableName(const System::UnicodeString Value);
	__classmethod virtual TDAColumnClass __fastcall GetColumnClass();
	virtual void __fastcall CreateColumnByField(Data::Db::TField* Field, TDAColumn* Column);
	virtual Memds::TFieldTypeMapClass __fastcall GetFieldTypeMapClass();
	bool __fastcall IsTransactionStored();
	virtual Dbaccess::TDATransaction* __fastcall GetTransaction();
	virtual void __fastcall SetTransaction(Dbaccess::TDATransaction* Value);
	virtual Dbaccess::TCustomDAConnection* __fastcall UsedConnection();
	virtual Dbaccess::TDATransaction* __fastcall UsedTransaction();
	virtual bool __fastcall NeedRecreateColumns();
	virtual void __fastcall ReadColumn(TDAColumn* Column, Craccess::TCRLoaderColumn* CRColumn);
	virtual void __fastcall WriteColumn(TDAColumn* Column, Craccess::TCRLoaderColumn* CRColumn);
	void __fastcall ClearColumns();
	void __fastcall ReadColumns();
	void __fastcall WriteColumns();
	virtual TDALoaderOptions* __fastcall CreateOptions();
	__property Dbaccess::TDATransaction* Transaction = {read=GetTransaction, write=SetTransaction, stored=IsTransactionStored};
	__property bool AutoCommit = {read=FAutoCommit, write=FAutoCommit, default=1};
	
public:
	__fastcall virtual TDALoader(System::Classes::TComponent* Owner);
	__fastcall virtual ~TDALoader();
	virtual void __fastcall PutColumnData(int Col, int Row, const System::Variant &Value)/* overload */;
	void __fastcall PutColumnData(const System::UnicodeString ColName, int Row, const System::Variant &Value)/* overload */;
	virtual void __fastcall Load();
	void __fastcall CreateColumns();
	void __fastcall LoadFromDataSet(Data::Db::TDataSet* DataSet);
	__property Dbaccess::TCustomDAConnection* Connection = {read=FConnection, write=SetConnection};
	__property System::UnicodeString TableName = {read=FTableName, write=SetTableName};
	__property TDAColumns* Columns = {read=FColumns, write=SetColumns, stored=IsColumnsStored};
	__property TDAPutDataEvent OnPutData = {read=FOnPutData, write=FOnPutData};
	__property TGetColumnDataEvent OnGetColumnData = {read=FOnGetColumnData, write=FOnGetColumnData};
	__property TLoaderProgressEvent OnProgress = {read=FOnLoaderProgress, write=FOnLoaderProgress};
	__property TDALoaderOptions* Options = {read=FOptions, write=SetOptions};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDALoaderUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall SetDesignCreate(TDALoader* Obj, bool Value);
	__classmethod bool __fastcall GetDesignCreate(TDALoader* Obj);
	__classmethod Dbaccess::TCustomDAConnection* __fastcall UsedConnection(TDALoader* Obj);
	__classmethod Dbaccess::TDATransaction* __fastcall GetTransaction(TDALoader* Obj);
	__classmethod void __fastcall SetTransaction(TDALoader* Obj, Dbaccess::TDATransaction* Value);
	__classmethod Dbaccess::TDATransaction* __fastcall GetFTransaction(TDALoader* Obj);
	__classmethod void __fastcall SetAutoCommit(TDALoader* Obj, bool Value);
public:
	/* TObject.Create */ inline __fastcall TDALoaderUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDALoaderUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Daloader */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DALOADER)
using namespace Daloader;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DaloaderHPP
