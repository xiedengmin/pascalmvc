// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualDataSet.pas' rev: 34.00 (Windows)

#ifndef VirtualdatasetHPP
#define VirtualdatasetHPP

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
#include <MemData.hpp>
#include <MemDS.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualdataset
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomVirtualDataSetData;
class DELPHICLASS TVirtualDataSetData;
class DELPHICLASS TVirtualDataSetUpdater;
class DELPHICLASS TVirtualDataSetService;
class DELPHICLASS TCustomVirtualDataSet;
class DELPHICLASS TVirtualDataSet;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnGetRecordCountEvent)(System::TObject* Sender, /* out */ int &Count);

typedef void __fastcall (__closure *TOnGetFieldValueEvent)(System::TObject* Sender, Data::Db::TField* Field, int RecNo, /* out */ System::Variant &Value);

typedef void __fastcall (__closure *TOnModifyRecordEvent)(System::TObject* Sender, int &RecNo);

typedef void __fastcall (__closure *TOnDeleteRecordEvent)(System::TObject* Sender, int RecNo);

class PASCALIMPLEMENTATION TCustomVirtualDataSetData : public Memdata::TData
{
	typedef Memdata::TData inherited;
	
private:
	TCustomVirtualDataSet* FOwner;
	int FRecordNo;
	
protected:
	virtual void __fastcall InitData();
	virtual void __fastcall InternalOpen(bool DisableInitFields = false);
	virtual void __fastcall CreateFieldDescs();
	virtual int __fastcall GetItemCount();
	virtual int __fastcall GetItemNo();
	virtual void __fastcall SetItemNo(int Value);
	virtual int __fastcall GetRecordCount();
	virtual int __fastcall GetRecordNo();
	virtual void __fastcall SetRecordNo(int Value);
	void __fastcall GetRecordBufer(int Index, void * RecBuf);
	void __fastcall GetRecordField(int Index, void * RecBuf, Memdata::TFieldDesc* Field);
	
public:
	__fastcall TCustomVirtualDataSetData();
	virtual void __fastcall GetRecord(void * RecBuf);
	virtual void __fastcall GetNextRecord(void * RecBuf);
	virtual void __fastcall GetPriorRecord(void * RecBuf);
	virtual void __fastcall PutRecord(void * RecBuf);
	virtual void __fastcall SetToBegin();
	virtual void __fastcall SetToEnd();
	virtual void __fastcall AppendRecord(void * RecBuf);
	virtual void __fastcall InsertRecord(void * RecBuf);
	virtual void __fastcall UpdateRecord(void * RecBuf);
	virtual void __fastcall DeleteRecord();
	virtual void __fastcall EditRecord(void * RecBuf);
	virtual void __fastcall PostRecord(void * RecBuf);
	virtual void __fastcall CancelRecord(void * RecBuf);
	__property TCustomVirtualDataSet* Owner = {read=FOwner};
	__property int ItemNo = {read=GetItemNo, write=SetItemNo, nodefault};
	__property int RecordNo = {read=GetRecordNo, write=SetRecordNo, nodefault};
public:
	/* TData.Destroy */ inline __fastcall virtual ~TCustomVirtualDataSetData() { }
	
};


class PASCALIMPLEMENTATION TVirtualDataSetData : public TCustomVirtualDataSetData
{
	typedef TCustomVirtualDataSetData inherited;
	
private:
	int FItemNo;
	
protected:
	virtual void __fastcall InitData();
	virtual void __fastcall InternalClose();
	virtual int __fastcall GetItemNo();
	virtual void __fastcall SetItemNo(int Value);
	virtual int __fastcall GetRecordCount();
	virtual void __fastcall SetRecordNo(int Value);
	bool __fastcall IsFiltered();
	bool __fastcall OmitRecord(int ItemNo);
	
public:
	virtual void __fastcall GetNextRecord(void * RecBuf);
	virtual void __fastcall GetPriorRecord(void * RecBuf);
	virtual void __fastcall SetToBegin();
	virtual void __fastcall SetToEnd();
	virtual void __fastcall GetBookmark(Memdata::PRecBookmark Bookmark);
	virtual void __fastcall SetToBookmark(Memdata::PRecBookmark Bookmark);
	void __fastcall DataUpdated();
	virtual void __fastcall FilterUpdated();
public:
	/* TCustomVirtualDataSetData.Create */ inline __fastcall TVirtualDataSetData() : TCustomVirtualDataSetData() { }
	
public:
	/* TData.Destroy */ inline __fastcall virtual ~TVirtualDataSetData() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualDataSetUpdater : public Memds::TDataSetUpdater
{
	typedef Memds::TDataSetUpdater inherited;
	
protected:
	TCustomVirtualDataSet* __fastcall DataSet();
	virtual bool __fastcall PerformAppend();
	virtual bool __fastcall PerformUpdate();
	virtual bool __fastcall PerformDelete();
	bool __fastcall PerformCancel();
public:
	/* TDataSetUpdater.Create */ inline __fastcall virtual TVirtualDataSetUpdater(Memds::TDataSetService* AOwner) : Memds::TDataSetUpdater(AOwner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualDataSetUpdater() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualDataSetService : public Memds::TDataSetService
{
	typedef Memds::TDataSetService inherited;
	
protected:
	TVirtualDataSetUpdater* __fastcall Updater();
	virtual void __fastcall CreateDataSetUpdater();
public:
	/* TDataSetService.Create */ inline __fastcall virtual TVirtualDataSetService(Memds::TMemDataSet* AOwner) : Memds::TDataSetService(AOwner) { }
	/* TDataSetService.Destroy */ inline __fastcall virtual ~TVirtualDataSetService() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCustomVirtualDataSet : public Memds::TMemDataSet
{
	typedef Memds::TMemDataSet inherited;
	
private:
	bool FIsCursorOpen;
	bool FReadOnly;
	
protected:
	TVirtualDataSetService* __fastcall DataSetService();
	virtual System::PByte __fastcall AllocRecordBuffer();
	virtual void __fastcall FreeRecordBuffer(System::PByte &Buffer);
	virtual Memds::TDataSetServiceClass __fastcall GetDataSetServiceClass();
	virtual void __fastcall CreateIRecordSet();
	virtual void __fastcall SetIRecordSet(Memdata::TData* Value);
	virtual bool __fastcall GetCanModify();
	virtual void __fastcall InternalOpen();
	virtual void __fastcall InternalClose();
	virtual bool __fastcall IsCursorOpen();
	virtual void __fastcall InternalCancel();
	Data::Db::TField* __fastcall GetField(Memdata::TFieldDesc* FieldDesc);
	void __fastcall PerformInsertRecord();
	void __fastcall PerformUpdateRecord();
	void __fastcall PerformDeleteRecord();
	void __fastcall PerformCancelRecord();
	virtual void __fastcall DoGetRecordCount(int &Count) = 0 ;
	virtual void __fastcall DoGetFieldValue(Memdata::TFieldDesc* Field, int Index, System::Variant &Value) = 0 ;
	virtual void __fastcall DoInsertRecord(int &RecordNo) = 0 ;
	virtual void __fastcall DoUpdateRecord(int &RecordNo) = 0 ;
	virtual void __fastcall DoDeleteRecord(int RecordNo) = 0 ;
	virtual void __fastcall DoCancelRecord(int RecordNo) = 0 ;
	
public:
	__fastcall virtual TCustomVirtualDataSet(System::Classes::TComponent* AOwner);
	
__published:
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, default=0};
public:
	/* TMemDataSet.Destroy */ inline __fastcall virtual ~TCustomVirtualDataSet() { }
	
};


class PASCALIMPLEMENTATION TVirtualDataSet : public TCustomVirtualDataSet
{
	typedef TCustomVirtualDataSet inherited;
	
private:
	TOnGetRecordCountEvent FOnGetRecordCount;
	TOnGetFieldValueEvent FOnGetFieldValue;
	TOnModifyRecordEvent FOnInsertRecord;
	TOnModifyRecordEvent FOnModifyRecord;
	TOnDeleteRecordEvent FOnDeleteRecord;
	
protected:
	virtual void __fastcall CreateIRecordSet();
	virtual void __fastcall DoGetRecordCount(int &Count);
	virtual void __fastcall DoGetFieldValue(Memdata::TFieldDesc* Field, int RecNo, System::Variant &Value);
	virtual void __fastcall DoInsertRecord(int &RecordNo);
	virtual void __fastcall DoUpdateRecord(int &RecordNo);
	virtual void __fastcall DoDeleteRecord(int RecordNo);
	virtual void __fastcall DoCancelRecord(int RecordNo);
	
public:
	__fastcall virtual TVirtualDataSet(System::Classes::TComponent* AOwner);
	
__published:
	__property AutoCalcFields = {default=1};
	__property Constraints = {stored=IsConstraintsStored};
	__property Filtered = {default=0};
	__property Filter = {default=0};
	__property FilterOptions = {default=0};
	__property IndexFieldNames = {default=0};
	__property MasterSource;
	__property MasterFields = {default=0};
	__property DetailFields = {default=0};
	__property BeforeOpen;
	__property AfterOpen;
	__property BeforeClose;
	__property AfterClose;
	__property BeforeInsert;
	__property AfterInsert;
	__property BeforeEdit;
	__property AfterEdit;
	__property BeforePost;
	__property AfterPost;
	__property BeforeCancel;
	__property AfterCancel;
	__property BeforeDelete;
	__property AfterDelete;
	__property AfterRefresh;
	__property BeforeRefresh;
	__property BeforeScroll;
	__property AfterScroll;
	__property OnCalcFields;
	__property TOnGetRecordCountEvent OnGetRecordCount = {read=FOnGetRecordCount, write=FOnGetRecordCount};
	__property TOnGetFieldValueEvent OnGetFieldValue = {read=FOnGetFieldValue, write=FOnGetFieldValue};
	__property TOnModifyRecordEvent OnInsertRecord = {read=FOnInsertRecord, write=FOnInsertRecord};
	__property TOnModifyRecordEvent OnModifyRecord = {read=FOnModifyRecord, write=FOnModifyRecord};
	__property TOnDeleteRecordEvent OnDeleteRecord = {read=FOnDeleteRecord, write=FOnDeleteRecord};
public:
	/* TMemDataSet.Destroy */ inline __fastcall virtual ~TVirtualDataSet() { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define SNotSupportFieldType L"Field type is not supported by TVirtualDataSet. \rValid ty"\
	L"pes is String, WideString, Smallint, Integer, Word, Boolea"\
	L"n, Largeint, Float, ftSingle, ftExtended, Currency, Date, "\
	L"Time, DateTime, SQLTimeStamp, Blob, Memo, Guid, Bcd, FmtBc"\
	L"d, Bytes, VarBytes, Variant"
}	/* namespace Virtualdataset */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALDATASET)
using namespace Virtualdataset;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualdatasetHPP
