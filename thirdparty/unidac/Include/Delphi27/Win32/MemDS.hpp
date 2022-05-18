// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MemDS.pas' rev: 34.00 (Windows)

#ifndef MemdsHPP
#define MemdsHPP

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
#include <Data.SqlTimSt.hpp>
#include <Winapi.Windows.hpp>
#include <CRTimer.hpp>
#include <System.Generics.Collections.hpp>
#include <CLRClasses.hpp>
#include <CRXml.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRTimeStamp.hpp>
#include <MemData.hpp>
#include <MemUtils.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Memds
{
//-- forward type declarations -----------------------------------------------
struct TRecInfo;
struct TCalcFieldDescMapping;
struct TLocalMDLink;
class DELPHICLASS TFieldTypeMap;
class DELPHICLASS TDADetailDataLink;
class DELPHICLASS TDataSetUpdater;
class DELPHICLASS TDataSetService;
class DELPHICLASS TMemDataSet;
class DELPHICLASS TBlobStream;
class DELPHICLASS TMemDSUtils;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TDataSetServiceClass;

typedef System::PByte TRecordBuffer;

typedef TRecInfo *PRecInfo;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TRecInfo
{
public:
	int RecordNumber;
	Data::Db::TUpdateStatus UpdateStatus;
	Data::Db::TBookmarkFlag BookmarkFlag;
	bool RefComplexFields;
	bool KeyExclusive;
};
#pragma pack(pop)


struct DECLSPEC_DRECORD TCalcFieldDescMapping
{
public:
	Memdata::TFieldDesc* FieldDesc;
	Data::Db::TField* Field;
};


struct DECLSPEC_DRECORD TLocalMDLink
{
public:
	bool IsNull;
	void *DataBuf;
	System::Word DataLen;
	System::Word BufferType;
	bool NativeBuffer;
	int FieldNo;
	System::Variant MasterFieldValue;
};


typedef TLocalMDLink *PLocalMDLink;

typedef System::DynamicArray<TLocalMDLink> TLocalMDLinks;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFieldTypeMap : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod virtual Data::Db::TFieldType __fastcall GetFieldType(System::Word DataType);
	__classmethod virtual int __fastcall GetDataType(Data::Db::TFieldType FieldType, System::Word SubDataType = (System::Word)(0x0));
public:
	/* TObject.Create */ inline __fastcall TFieldTypeMap() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TFieldTypeMap() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TFieldTypeMapClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADetailDataLink : public Data::Db::TDetailDataLink
{
	typedef Data::Db::TDetailDataLink inherited;
	
private:
	TMemDataSet* FDataSet;
	
protected:
	virtual void __fastcall ActiveChanged();
	virtual void __fastcall RecordChanged(Data::Db::TField* Field);
	virtual void __fastcall CheckBrowseMode();
	virtual Data::Db::TDataSet* __fastcall GetDetailDataSet();
	
public:
	__fastcall TDADetailDataLink(TMemDataSet* DataSet);
public:
	/* TDataLink.Destroy */ inline __fastcall virtual ~TDADetailDataLink() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataSetUpdater : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TMemDataSet* FDataSet;
	TDataSetService* FDataSetService;
	virtual bool __fastcall PerformAppend();
	virtual bool __fastcall PerformDelete();
	virtual bool __fastcall PerformUpdate();
	virtual bool __fastcall CacheChanged();
	virtual bool __fastcall CacheApplied();
	virtual bool __fastcall CacheCanceled();
	void __fastcall DoPerformAppend();
	void __fastcall DoPerformDelete();
	void __fastcall DoPerformUpdate();
	void __fastcall DoApplyRecord(Memdata::TUpdateRecKind UpdateKind, Memdata::TUpdateRecAction &Action, bool LastItem);
	void __fastcall DoCacheChanged();
	void __fastcall DoCacheApplied();
	void __fastcall DoCacheCanceled();
	void __fastcall DoAfterApplyUpdates();
	virtual bool __fastcall BatchUpdate();
	virtual bool __fastcall CanFlushBatch();
	virtual void __fastcall FlushBatch();
	
public:
	__fastcall virtual TDataSetUpdater(TDataSetService* AOwner);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDataSetUpdater() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataSetService : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TMemDataSet* FDataSet;
	TDataSetUpdater* FUpdater;
	virtual void __fastcall CreateDataSetUpdater();
	virtual void __fastcall SetDataSetUpdater(TDataSetUpdater* Value);
	void __fastcall FreeDataSetUpdater();
	virtual void __fastcall SetNumberRange(Data::Db::TFieldDef* FieldDef);
	virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldType FieldType, System::Word DataType);
	virtual void __fastcall PreInitCursor();
	virtual void __fastcall WriteFieldXMLDataType(Data::Db::TField* Field, Memdata::TFieldDesc* FieldDesc, const System::UnicodeString FieldAlias, Crxml::XmlTextWriter* XMLWriter);
	virtual void __fastcall WriteFieldXMLAttributeType(Data::Db::TField* Field, Memdata::TFieldDesc* FieldDesc, const System::UnicodeString FieldAlias, Crxml::XmlTextWriter* XMLWriter);
	virtual System::WideString __fastcall GetFieldXMLValue(Data::Db::TField* Field, Memdata::TFieldDesc* FieldDesc);
	
public:
	__fastcall virtual TDataSetService(TMemDataSet* AOwner);
	__fastcall virtual ~TDataSetService();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	void __fastcall SaveToXML(System::Classes::TStream* Destination);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TMemDataSet : public Data::Db::TDataSet
{
	typedef Data::Db::TDataSet inherited;
	
	
private:
	typedef System::DynamicArray<TCalcFieldDescMapping> _TMemDataSet__1;
	
	
private:
	System::Byte *FOldRecBuf;
	System::Byte *FFilterBuffer;
	System::Byte *FRangeStartBuffer;
	System::Byte *FRangeEndBuffer;
	System::Byte *FRangeCurrentBuffer;
	bool FRanged;
	bool FCachedUpdates;
	bool FLocalUpdate;
	System::Byte *FNewCacheRecBuf;
	System::Byte *FOldCacheRecBuf;
	System::Byte *FOldDeferredPostBuf;
	bool FInInserting;
	bool FInEditing;
	System::UnicodeString FIndexFieldNames;
	_TMemDataSet__1 FCalcFieldsMapping;
	Data::Db::TUpdateRecordTypes FUpdateRecordTypes;
	Data::Db::TUpdateErrorEvent FOnUpdateError;
	Data::Db::TUpdateRecordEvent FOnUpdateRecord;
	TDADetailDataLink* FDataLink;
	TLocalMDLinks FLocalMDLinks;
	int FDetailDelay;
	Crtimer::TCRTimer* FDetailRefreshTimer;
	void __fastcall CheckRefreshDetailTimer();
	void __fastcall SetDetailDelay(int Value);
	void __fastcall SetMasterSource(Data::Db::TDataSource* Value);
	void __fastcall SetMasterFields(const System::UnicodeString Value);
	void __fastcall SetDetailFields(const System::UnicodeString Value);
	void __fastcall SetCachedUpdates(bool Value);
	void __fastcall SetLocalUpdate(bool Value);
	bool __fastcall GetUpdatesPending();
	bool __fastcall GetPrepared();
	void __fastcall SetPrepared(bool Value);
	Memdata::TItemTypes __fastcall ConvertUpdateRecordTypes(Data::Db::TUpdateRecordTypes Value);
	Data::Db::TUpdateRecordTypes __fastcall GetUpdateRecordTypes();
	void __fastcall SetUpdateRecordTypes(Data::Db::TUpdateRecordTypes Value);
	void __fastcall FreeDeferredBuf();
	void __fastcall CheckSetKeyMode();
	void __fastcall ClearRangeBuffer(System::PByte Buffer);
	void __fastcall InitRangeBuffer(System::PByte Buffer, bool Clear);
	void __fastcall SetRangeBuffer(System::PByte Buffer, const System::TVarRec *Values, const int Values_High);
	bool __fastcall GetKeyExclusive();
	void __fastcall SetKeyExclusive(bool Value);
	void __fastcall ResetRange();
	void __fastcall CreateCheckConstraints();
	void __fastcall CreateFieldConstrain();
	
protected:
	Memdata::TData* Data;
	TDataSetService* FDataSetService;
	int FBookmarkOfs;
	int FRecInfoOfs;
	bool FInDeferredPost;
	int FRecBufSize;
	TMemDataSet* FParentDataSet;
	int FLastParentPos;
	bool FLocalConstraints;
	bool FNumberRange;
	bool FNeedAddRef;
	bool FIsResync;
	bool FCacheCalcFields;
	bool FCreateCalcFieldDescs;
	bool FInSettingDefaultExpressionValues;
	bool FDataWasChanged;
	bool FDisableResync;
	System::UnicodeString FMasterFields;
	System::UnicodeString FDetailFields;
	bool __fastcall IsConstraintsStored();
	HIDESBASE void __fastcall SetModified(bool Value);
	HIDESBASE Data::Db::TDataSetState __fastcall SetTempState(const Data::Db::TDataSetState Value);
	HIDESBASE void __fastcall RestoreState(const Data::Db::TDataSetState Value);
	void __fastcall DoOnDataChanged();
	virtual void __fastcall CreateIRecordSet();
	void __fastcall FreeIRecordSet();
	virtual void __fastcall SetIRecordSet(Memdata::TData* Value);
	virtual void __fastcall SetIndexFieldNames(const System::UnicodeString Value);
	virtual TDataSetServiceClass __fastcall GetDataSetServiceClass();
	void __fastcall CreateDataSetService();
	void __fastcall FreeDataSetService();
	virtual void __fastcall SetDataSetService(TDataSetService* Value);
	void __fastcall CheckDataSetService();
	virtual void __fastcall OpenCursor(bool InfoQuery);
	virtual void __fastcall CloseCursor();
	virtual void __fastcall InternalOpen();
	virtual void __fastcall InternalClose();
	virtual bool __fastcall IsCursorOpen();
	virtual void __fastcall DataReopen();
	virtual void __fastcall InternalRefresh();
	virtual void __fastcall DoAfterOpen();
	virtual TFieldTypeMapClass __fastcall GetFieldTypeMapClass();
	virtual void __fastcall InternalInitFieldDefs();
	virtual bool __fastcall NeedCreateFieldDefs();
	virtual Data::Db::TFieldDef* __fastcall CreateFieldDef(Memdata::TFieldDesc* FieldDesc);
	int __fastcall CreateObjectFields(Memdata::TObjectType* ObjType, Data::Db::TFieldDef* Parent, int FieldNo);
	virtual void __fastcall CreateFieldDefs();
	void __fastcall UpdateFieldDefList();
	virtual bool __fastcall NeedComplexUpdateFieldDefList();
	virtual void __fastcall ClearCalcFields(NativeInt Buffer)/* overload */;
	virtual void __fastcall ClearCalcFields(System::PByte Buffer)/* overload */;
	virtual System::UnicodeString __fastcall GetObjectFieldDefName(Data::Db::TFieldDef* Parent, int Index, Memdata::TObjectType* ObjType);
	virtual int __fastcall GetFieldDefSize(Data::Db::TFieldType FieldType, int FieldLength);
	void __fastcall GetObjectTypeNames(Data::Db::TFields* Fields);
	virtual Data::Db::TFieldType __fastcall GetFieldType(System::Word DataType)/* overload */;
	virtual Data::Db::TFieldType __fastcall GetFieldType(Memdata::TFieldDesc* FieldDesc, /* out */ int &FieldSize, /* out */ int &FieldLength, /* out */ int &FieldScale)/* overload */;
	System::Word __fastcall PrepareValueBuffer(Data::Db::TField* Field, void * &Buffer);
	virtual void __fastcall InternalSetFieldData(Data::Db::TField* Field, void * Buffer, System::Word BufferLen);
	virtual bool __fastcall InternalDataConvert(Data::Db::TField* Field, void * Source, void * Dest, bool ToNative);
	virtual void __fastcall SetFieldData(Data::Db::TField* Field, System::DynamicArray<System::Byte> Buffer)/* overload */;
	virtual void __fastcall SetFieldData(Data::Db::TField* Field, System::DynamicArray<System::Byte> Buffer, bool NativeFormat)/* overload */;
	virtual void __fastcall DataConvert(Data::Db::TField* Field, System::DynamicArray<System::Byte> Source, System::DynamicArray<System::Byte> &Dest, bool ToNative)/* overload */;
	virtual void __fastcall SetFieldData(Data::Db::TField* Field, void * Buffer)/* overload */;
	virtual void __fastcall SetFieldData(Data::Db::TField* Field, void * Buffer, bool NativeFormat)/* overload */;
	virtual void __fastcall DataConvert(Data::Db::TField* Field, void * Source, void * Dest, bool ToNative)/* overload */;
	bool __fastcall GetSparseArrays();
	HIDESBASE void __fastcall SetSparseArrays(bool Value);
	virtual void __fastcall CheckFieldCompatibility(Data::Db::TField* Field, Data::Db::TFieldDef* FieldDef);
	virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldType FieldType)/* overload */;
	HIDESBASE virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldType FieldType, System::Word DataType)/* overload */;
	virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldDef* FieldDef)/* overload */;
	virtual System::PByte __fastcall AllocRecordBuffer();
	virtual void __fastcall FreeRecordBuffer(System::PByte &Buffer);
	virtual void __fastcall InitRecord(NativeInt Buffer)/* overload */;
	virtual void __fastcall InitRecord(System::PByte Buffer)/* overload */;
	virtual void __fastcall InternalInitRecord(NativeInt Buffer)/* overload */;
	virtual void __fastcall InternalInitRecord(System::PByte Buffer)/* overload */;
	System::PByte __fastcall GetOldRecord();
	System::PByte __fastcall GetOldRecBuf();
	System::PByte __fastcall GetNewRecBuf();
	bool __fastcall GetActiveRecBuf(/* out */ System::PByte &RecBuf);
	virtual Data::Db::TGetResult __fastcall GetRecord(NativeInt Buffer, Data::Db::TGetMode GetMode, bool DoCheck)/* overload */;
	virtual Data::Db::TGetResult __fastcall GetRecord(System::PByte Buffer, Data::Db::TGetMode GetMode, bool DoCheck)/* overload */;
	virtual void __fastcall BlockReadNext();
	virtual void __fastcall SetBlockReadSize(int Value);
	void __fastcall FreeRefBuffers();
	void __fastcall AddRefComplexFields(System::PByte Buffer);
	void __fastcall FreeRefComplexFields(System::PByte Buffer, bool WithBlob = true);
	virtual void __fastcall GetBookmarkData(NativeInt Buffer, System::DynamicArray<System::Byte> Bookmark)/* overload */;
	virtual void __fastcall SetBookmarkData(NativeInt Buffer, System::DynamicArray<System::Byte> Bookmark)/* overload */;
	virtual void __fastcall InternalGotoBookmark(System::DynamicArray<System::Byte> Bookmark)/* overload */;
	virtual Data::Db::TBookmarkFlag __fastcall GetBookmarkFlag(NativeInt Buffer)/* overload */;
	virtual void __fastcall SetBookmarkFlag(NativeInt Buffer, Data::Db::TBookmarkFlag Value)/* overload */;
	virtual void __fastcall GetBookmarkData(System::PByte Buffer, void * Bookmark)/* overload */;
	virtual void __fastcall SetBookmarkData(System::PByte Buffer, void * Bookmark)/* overload */;
	virtual void __fastcall InternalGotoBookmark(void * Bookmark)/* overload */;
	virtual Data::Db::TBookmarkFlag __fastcall GetBookmarkFlag(System::PByte Buffer)/* overload */;
	virtual void __fastcall SetBookmarkFlag(System::PByte Buffer, Data::Db::TBookmarkFlag Value)/* overload */;
	virtual void __fastcall InternalFirst();
	virtual void __fastcall InternalLast();
	virtual void __fastcall InternalSetToRecord(NativeInt Buffer)/* overload */;
	virtual void __fastcall InternalSetToRecord(System::PByte Buffer)/* overload */;
	virtual void __fastcall InternalAddRecord(NativeInt Buffer, bool Append)/* overload */;
	virtual void __fastcall InternalAddRecord(void * Buffer, bool Append)/* overload */;
	virtual void __fastcall InternalInsert();
	virtual void __fastcall InternalDelete();
	virtual void __fastcall InternalEdit();
	virtual void __fastcall InternalPost();
	virtual void __fastcall InternalCancel();
	virtual void __fastcall InternalDeferredPost();
	virtual void __fastcall SetDefaultExpressionValues();
	virtual void __fastcall DoOnNewRecord();
	void __fastcall DoPerformAppend();
	void __fastcall DoPerformDelete();
	void __fastcall DoPerformUpdate();
	void __fastcall DoApplyRecord(Memdata::TUpdateRecKind UpdateKind, Memdata::TUpdateRecAction &Action, bool LastItem);
	void __fastcall DoCacheChanged();
	void __fastcall DoCacheApplied();
	void __fastcall DoCacheCanceled();
	void __fastcall DoAfterApplyUpdates();
	void __fastcall DoGetCachedFields();
	void __fastcall DoGetCachedBuffer(void * Buffer, void * Source = (void *)(0x0));
	virtual void __fastcall ActivateFilters();
	virtual void __fastcall DeactivateFilters();
	bool __fastcall RecordFilter(void * RecBuf);
	void __fastcall SetFilterData(const System::UnicodeString Text, Data::Db::TFilterOptions Options);
	virtual void __fastcall SetFiltered(bool Value);
	virtual void __fastcall SetFilterOptions(Data::Db::TFilterOptions Value);
	virtual void __fastcall SetFilterText(const System::UnicodeString Value);
	virtual void __fastcall SetOnFilterRecord(const Data::Db::TFilterRecordEvent Value);
	virtual void __fastcall CopyFieldValue(const System::Variant &Value, /* out */ void * &ValuePtr, /* out */ System::Word &ValueLen, /* out */ System::Word &ValueType, System::Word FieldType, bool UseFieldType = true);
	virtual bool __fastcall LocateRecord(System::Classes::TList* KeyFields, const System::Variant &KeyValues, Memdata::TLocateExOptions Options, bool SavePos)/* overload */;
	bool __fastcall LocateRecord(const System::UnicodeString KeyFields, const System::Variant &KeyValues, Memdata::TLocateExOptions Options, bool SavePos)/* overload */;
	bool __fastcall LocateRecord(Data::Db::TField* const *KeyFields, const int KeyFields_High, const System::Variant &KeyValues, Memdata::TLocateExOptions Options, bool SavePos)/* overload */;
	virtual bool __fastcall FindRecord(bool Restart, bool GoForward);
	bool __fastcall RecordFilterRange(void * RecBuf);
	bool __fastcall AddFieldToList(const System::UnicodeString FieldName, Data::Db::TDataSet* DataSet, System::Generics::Collections::TList__1<Data::Db::TField*>* List);
	virtual Data::Db::TDataSource* __fastcall GetDataSource();
	bool __fastcall IsMasterDatasetActive();
	bool __fastcall IsConnectedToMaster();
	virtual bool __fastcall SetLocalMDLinks(Data::Db::TField* Field);
	virtual bool __fastcall MDLinksRefreshed(Data::Db::TField* Field);
	void __fastcall MasterRecordChanged(Data::Db::TField* Field);
	virtual void __fastcall RefreshDetail(System::TObject* Sender);
	bool __fastcall LocalDetailFilter(void * RecBuf);
	virtual bool __fastcall UseLocalMasterDetailFilter();
	void __fastcall SetLocalDetailFilter();
	virtual void __fastcall MDPropertiesChanged();
	virtual System::UnicodeString __fastcall SplitFieldName(const System::UnicodeString Fields, int &Pos);
	__property Data::Db::TDataSource* MasterSource = {read=GetDataSource, write=SetMasterSource};
	__property System::UnicodeString MasterFields = {read=FMasterFields, write=SetMasterFields};
	__property System::UnicodeString DetailFields = {read=FDetailFields, write=SetDetailFields};
	__property int DetailDelay = {read=FDetailDelay, write=SetDetailDelay, nodefault};
	Memdata::TUpdateRecAction __fastcall InternalGetUpdateResult();
	void __fastcall CheckCachedUpdateMode();
	Memdata::TBlob* __fastcall InternalGetBlob(Memdata::TFieldDesc* FieldDesc);
	bool __fastcall InternalSetBlob(Memdata::TFieldDesc* FieldDesc, Memdata::TBlob* Blob);
	bool __fastcall SetBlob(Data::Db::TField* Field, Memdata::TBlob* Blob);
	virtual void __fastcall CloseBlob(Data::Db::TField* Field);
	virtual int __fastcall GetRecordCount();
	virtual System::Word __fastcall GetRecordSize();
	virtual int __fastcall GetRecNo();
	virtual void __fastcall SetRecNo(int Value);
	virtual void __fastcall InternalHandleException();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual void __fastcall DataEvent(Data::Db::TDataEvent Event, NativeInt Info);
	virtual System::Variant __fastcall GetStateFieldValue(Data::Db::TDataSetState State, Data::Db::TField* Field);
	virtual int __fastcall GetMaxFieldCount();
	
public:
	__fastcall virtual TMemDataSet(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMemDataSet();
	virtual void __fastcall Prepare();
	virtual void __fastcall UnPrepare();
	void __fastcall CheckPrepared();
	virtual void __fastcall Resync(Data::Db::TResyncMode Mode);
	virtual void __fastcall GetDetailLinkFields(System::Generics::Collections::TList__1<Data::Db::TField*>* MasterFields, System::Generics::Collections::TList__1<Data::Db::TField*>* DetailFields)/* overload */;
	int __fastcall GetFieldDescNo(Data::Db::TField* Field);
	virtual Memdata::TFieldDesc* __fastcall GetFieldDesc(Data::Db::TField* const Field)/* overload */;
	Memdata::TFieldDesc* __fastcall GetFieldDesc(const System::UnicodeString FieldName)/* overload */;
	virtual Memdata::TFieldDesc* __fastcall GetFieldDesc(const int FieldNo)/* overload */;
	virtual bool __fastcall GetFieldData(int FieldNo, System::DynamicArray<System::Byte> &Buffer)/* overload */;
	virtual bool __fastcall GetFieldData(Data::Db::TField* Field, System::DynamicArray<System::Byte> &Buffer)/* overload */;
	virtual bool __fastcall GetFieldData(Data::Db::TField* Field, System::DynamicArray<System::Byte> &Buffer, bool NativeFormat)/* overload */;
	virtual bool __fastcall GetFieldData(int FieldNo, void * Buffer)/* overload */;
	virtual bool __fastcall GetFieldData(Data::Db::TField* Field, void * Buffer)/* overload */;
	virtual bool __fastcall GetFieldData(Data::Db::TField* Field, void * Buffer, bool NativeFormat)/* overload */;
	Memdata::TBlob* __fastcall GetBlob(const System::UnicodeString FieldName)/* overload */;
	Memdata::TBlob* __fastcall GetBlob(Data::Db::TField* Field)/* overload */;
	void __fastcall CreateConstraints();
	virtual void __fastcall Post();
	virtual void __fastcall Cancel();
	void __fastcall DeferredPost();
	virtual bool __fastcall BookmarkValid(System::DynamicArray<System::Byte> Bookmark);
	virtual int __fastcall CompareBookmarks(System::DynamicArray<System::Byte> Bookmark1, System::DynamicArray<System::Byte> Bookmark2);
	virtual System::Classes::TStream* __fastcall CreateBlobStream(Data::Db::TField* Field, Data::Db::TBlobStreamMode Mode);
	virtual bool __fastcall Locate(const System::UnicodeString KeyFields, const System::Variant &KeyValues, Data::Db::TLocateOptions Options)/* overload */;
	HIDESBASE bool __fastcall Locate(Data::Db::TField* const *KeyFields, const int KeyFields_High, const System::Variant &KeyValues, Data::Db::TLocateOptions Options)/* overload */;
	bool __fastcall LocateEx(const System::UnicodeString KeyFields, const System::Variant &KeyValues, Memdata::TLocateExOptions Options)/* overload */;
	bool __fastcall LocateEx(Data::Db::TField* const *KeyFields, const int KeyFields_High, const System::Variant &KeyValues, Memdata::TLocateExOptions Options)/* overload */;
	virtual System::Variant __fastcall Lookup(const System::UnicodeString KeyFields, const System::Variant &KeyValues, const System::UnicodeString ResultFields);
	virtual Data::Db::TUpdateStatus __fastcall UpdateStatus();
	Data::Db::TUpdateAction __fastcall UpdateResult();
	virtual void __fastcall ApplyUpdates()/* overload */;
	virtual void __fastcall ApplyUpdates(const Memdata::TUpdateRecKinds UpdateRecKinds)/* overload */;
	void __fastcall CommitUpdates();
	void __fastcall CancelUpdates();
	void __fastcall RestoreUpdates();
	void __fastcall RevertRecord();
	void __fastcall SaveToXML(System::Classes::TStream* Destination)/* overload */;
	void __fastcall SaveToXML(const System::UnicodeString FileName)/* overload */;
	void __fastcall SetRange(const System::TVarRec *StartValues, const int StartValues_High, const System::TVarRec *EndValues, const int EndValues_High, bool StartExlusive = false, bool EndExclusive = false);
	void __fastcall ApplyRange();
	void __fastcall CancelRange();
	void __fastcall SetRangeStart();
	void __fastcall SetRangeEnd();
	void __fastcall EditRangeStart();
	void __fastcall EditRangeEnd();
	virtual bool __fastcall IsSequenced();
	bool __fastcall InCacheProcessing();
	__property bool Prepared = {read=GetPrepared, write=SetPrepared, nodefault};
	__property bool CachedUpdates = {read=FCachedUpdates, write=SetCachedUpdates, default=0};
	__property bool UpdatesPending = {read=GetUpdatesPending, nodefault};
	__property bool LocalUpdate = {read=FLocalUpdate, write=SetLocalUpdate, default=0};
	__property Data::Db::TUpdateRecordTypes UpdateRecordTypes = {read=GetUpdateRecordTypes, write=SetUpdateRecordTypes, default=11};
	__property bool SparseArrays = {read=GetSparseArrays, write=SetSparseArrays, nodefault};
	__property bool Ranged = {read=FRanged, nodefault};
	__property bool KeyExclusive = {read=GetKeyExclusive, write=SetKeyExclusive, nodefault};
	__property bool LocalConstraints = {read=FLocalConstraints, write=FLocalConstraints, default=1};
	__property Data::Db::TUpdateErrorEvent OnUpdateError = {read=FOnUpdateError, write=FOnUpdateError};
	__property Data::Db::TUpdateRecordEvent OnUpdateRecord = {read=FOnUpdateRecord, write=FOnUpdateRecord};
	__property System::UnicodeString IndexFieldNames = {read=FIndexFieldNames, write=SetIndexFieldNames};
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  GetBookmarkData _DEPRECATED_ATTRIBUTE1("Use overloaded method instead") (System::PByte Buffer, System::DynamicArray<System::Byte> Data){ Data::Db::TDataSet::GetBookmarkData(Buffer, Data); }
	inline void __fastcall  SetBookmarkData _DEPRECATED_ATTRIBUTE1("Use overloaded method instead") (System::PByte Buffer, System::DynamicArray<System::Byte> Data){ Data::Db::TDataSet::SetBookmarkData(Buffer, Data); }
	inline void __fastcall  InternalAddRecord _DEPRECATED_ATTRIBUTE1("Use overloaded method instead") (System::PByte Buffer, bool Append){ Data::Db::TDataSet::InternalAddRecord(Buffer, Append); }
	
public:
	inline void __fastcall  GetDetailLinkFields _DEPRECATED_ATTRIBUTE1("Use overloaded method instead") (System::Classes::TList* MasterFields, System::Classes::TList* DetailFields){ Data::Db::TDataSet::GetDetailLinkFields(MasterFields, DetailFields); }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TBlobStream : public System::Classes::TStream
{
	typedef System::Classes::TStream inherited;
	
protected:
	Data::Db::TBlobField* FField;
	Memdata::TFieldDesc* FFieldDesc;
	TMemDataSet* FDataSet;
	System::Byte *FBuffer;
	Data::Db::TBlobStreamMode FMode;
	bool FOpened;
	bool FModified;
	int FPosition;
	int __fastcall GetBlobSize();
	virtual void __fastcall SetSize(int NewSize)/* overload */;
	
public:
	__fastcall TBlobStream(Data::Db::TBlobField* Field, Data::Db::TBlobStreamMode Mode);
	__fastcall virtual ~TBlobStream();
	virtual int __fastcall Read(void *Buffer, int Count)/* overload */;
	virtual int __fastcall Write(const void *Buffer, int Count)/* overload */;
	virtual int __fastcall Seek(int Offset, System::Word Origin)/* overload */;
	void __fastcall Truncate();
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  SetSize(const __int64 NewSize){ System::Classes::TStream::SetSize(NewSize); }
	
public:
	inline int __fastcall  Read(System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline int __fastcall  Read(System::DynamicArray<System::Byte> &Buffer, int Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Count){ return System::Classes::TStream::Write(Buffer, Count); }
	inline __int64 __fastcall  Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	
};

#pragma pack(pop)

typedef Data::Db::TSQLTimeStampField TDASQLTimeStampField;

typedef Data::Db::TSQLTimeStampOffsetField TDASQLTimeStampOffsetField;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMemDSUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod bool __fastcall SetBlob(TMemDataSet* Obj, Data::Db::TField* Field, Memdata::TBlob* Blob);
	__classmethod Memdata::TBlob* __fastcall GetBlob(TMemDataSet* Obj, Memdata::TFieldDesc* FieldDesc);
public:
	/* TObject.Create */ inline __fastcall TMemDSUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMemDSUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 uaDefault = System::Int8(0xa);
static const System::Int8 rlAny = System::Int8(-1);
static const System::Int8 ftDATimeStampOffset = System::Int8(0x63);
extern DELPHI_PACKAGE bool SendDataSetChangeEventAfterOpen;
extern DELPHI_PACKAGE bool DoNotRaiseExcetionOnUaFail;
extern DELPHI_PACKAGE bool LocateExOldBehavior;
extern DELPHI_PACKAGE bool RefreshParamsOnInsert;
extern DELPHI_PACKAGE void __fastcall ChangeDecimalSeparator(System::UnicodeString &Value, const System::WideChar OldSeparator, const System::WideChar NewSeparator);
}	/* namespace Memds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MEMDS)
using namespace Memds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MemdsHPP
