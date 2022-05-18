// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MemData.pas' rev: 34.00 (Windows)

#ifndef MemdataHPP
#define MemdataHPP

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
#include <System.Variants.hpp>
#include <Data.FmtBcd.hpp>
#include <Data.SqlTimSt.hpp>
#include <System.AnsiStrings.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRTimeStamp.hpp>
#include <CRParser.hpp>
#include <MemUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Memdata
{
//-- forward type declarations -----------------------------------------------
struct TBlockHeader;
struct TItemHeader;
class DELPHICLASS TBlockManager;
struct TBlock;
class DELPHICLASS TStringHeap;
struct TConvertInfo;
class DELPHICLASS TFieldDesc;
class DELPHICLASS TFieldDescs;
class DELPHICLASS TSortColumn;
class DELPHICLASS TSharedObject;
class DELPHICLASS TAttribute;
class DELPHICLASS TAttributeChain;
class DELPHICLASS TObjectType;
class DELPHICLASS TDBObject;
class DELPHICLASS TCacheItem;
struct TRecBookmark;
class DELPHICLASS TBoolParser;
class DELPHICLASS TExpressionNode;
class DELPHICLASS TCondition;
class DELPHICLASS TFilter;
class DELPHICLASS TConstraint;
class DELPHICLASS TFieldConstraint;
class DELPHICLASS TData;
class DELPHICLASS TMemData;
struct TPieceHeader;
class DELPHICLASS TCRBlobData;
class DELPHICLASS TBlob;
class DELPHICLASS TCompressedBlobData;
class DELPHICLASS TCompressedBlob;
class DELPHICLASS TVariantObject;
//-- type declarations -------------------------------------------------------
typedef System::Word TDataType;

enum DECLSPEC_DENUM TDANumericType : unsigned char { ntFloat, ntBCD, ntFmtBCD };

enum DECLSPEC_DENUM TConnLostCause : unsigned char { clUnknown, clExecute, clOpen, clRefresh, clApply, clServiceQuery, clTransStart, clConnectionApply, clConnect };

enum DECLSPEC_DENUM TLocateExOption : unsigned char { lxCaseInsensitive, lxPartialKey, lxNearest, lxNext, lxUp, lxPartialCompare };

typedef System::Set<TLocateExOption, TLocateExOption::lxCaseInsensitive, TLocateExOption::lxPartialCompare> TLocateExOptions;

enum DECLSPEC_DENUM TCompareOption : unsigned char { coCaseInsensitive, coPartialKey, coPartialCompare, coOrdinalCompare, coInvertNullOrder };

typedef System::Set<TCompareOption, TCompareOption::coCaseInsensitive, TCompareOption::coInvertNullOrder> TCompareOptions;

enum DECLSPEC_DENUM TReorderOption : unsigned char { roInsert, roDelete, roFull };

enum DECLSPEC_DENUM TSortType : unsigned char { stCaseSensitive, stCaseInsensitive, stBinary };

typedef TBlockHeader *PBlockHeader;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TBlockHeader
{
public:
	System::Word ItemCount;
	System::Word UsedItems;
	TBlockHeader *Prev;
	TBlockHeader *Next;
};
#pragma pack(pop)


enum DECLSPEC_DENUM TItemStatus : unsigned char { isUnmodified, isUpdated, isAppended, isDeleted };

typedef System::Set<TItemStatus, TItemStatus::isUnmodified, TItemStatus::isDeleted> TItemTypes;

enum DECLSPEC_DENUM TUpdateRecAction : unsigned char { urFail, urAbort, urSkip, urRetry, urApplied, urNone, urSuspended };

enum DECLSPEC_DENUM TItemFilterState : unsigned char { fsNotChecked, fsNotOmitted, fsOmitted };

typedef TItemHeader *PItemHeader;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TItemHeader
{
public:
	TBlockHeader *Block;
	TItemHeader *Prev;
	TItemHeader *Next;
	TItemHeader *Rollback;
	TItemStatus Status;
	TUpdateRecAction UpdateResult;
	int Order;
	int SavedOrder;
	System::Byte Flag;
	TItemFilterState FilterResult;
};
#pragma pack(pop)


#pragma pack(push,4)
class PASCALIMPLEMENTATION TBlockManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int RecordSize;
	void __fastcall AddFreeBlock();
	void __fastcall FreeAllBlocks();
	
public:
	TItemHeader *FirstFree;
	TBlockHeader *FirstBlock;
	System::Word DefaultItemCount;
	__fastcall TBlockManager();
	__fastcall virtual ~TBlockManager();
	void __fastcall AllocBlock(/* out */ PBlockHeader &Block, System::Word ItemCount, bool StandAloneBlock = false);
	void __fastcall ReAllocBlock(PBlockHeader &Block, System::Word ItemCount);
	void __fastcall FreeBlock(PBlockHeader Block, bool StandAloneBlock = false);
	void __fastcall AllocItem(/* out */ PItemHeader &Item);
	void __fastcall FreeItem(PItemHeader Item);
	void __fastcall PutRecord(PItemHeader Item, void * Rec);
	void __fastcall GetRecord(PItemHeader Item, void * Rec);
	void * __fastcall GetRecordPtr(PItemHeader Item);
	void __fastcall CopyRecord(PItemHeader ItemSrc, PItemHeader ItemDest);
};

#pragma pack(pop)

typedef TBlock *PBlock;

typedef System::StaticArray<System::Byte, 16380> TStrData;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TBlock
{
public:
	TBlock *Next;
	TStrData Data;
};
#pragma pack(pop)


typedef System::StaticArray<void *, 250> TSmallTab;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringHeap : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TSmallTab FSmallTab;
	int FFree;
	TBlock *FRoot;
	bool FEmpty;
	bool FSysGetMem;
	bool FUseSysMemSize;
	bool FThreadSafety;
	System::Syncobjs::TCriticalSection* FThreadSafetyCS;
	void __fastcall SetThreadSafety(const bool Value);
	bool __fastcall UseSmallTabs(int divSize);
	
public:
	__fastcall TStringHeap();
	__fastcall virtual ~TStringHeap();
	void * __fastcall NewBuf(int Size);
	void * __fastcall AllocStr(void * Str, int Len = 0xffffffff);
	void * __fastcall AllocTrimmedStr(void * Str, int &Len);
	void * __fastcall AllocWideStr(void * Str, int Len = 0xffffffff);
	void * __fastcall AllocTrimmedWideStr(void * Str, int &Len);
	void __fastcall DisposeBuf(void * Buf);
	void __fastcall AddRef(void * Buf);
	void __fastcall Clear();
	__property bool Empty = {read=FEmpty, nodefault};
	__property bool SysGetMem = {read=FSysGetMem, nodefault};
	__property bool ThreadSafety = {read=FThreadSafety, write=SetThreadSafety, nodefault};
};

#pragma pack(pop)

typedef System::Set<System::Byte, 0, 255> TFieldTypeSet;

enum DECLSPEC_DENUM TDateFormat : unsigned char { dfMSecs, dfDateTime, dfTime, dfDate };

enum DECLSPEC_DENUM TFieldDescKind : unsigned char { fdkData, fdkCached, fdkCalculated };

typedef System::TMetaClass* TRecordSetClass;

enum DECLSPEC_DENUM TConvertStatus : unsigned char { csSuccess, csBinaryTruncated, csStringTruncated, csDataTruncated, csFractionTruncated, csInvalidBinaryValue, csInvalidBlobValue, csInvalidDataMapping, csInvalidValueScale, csValueOverflow, csValueOutOfRange, csInvalidBooleanValue, csInvalidGUIDValue, csInvalidIntervalValue, csInvalidDateTimeValue, csInvalidSQLTimeStampValue, csInvalidIntegerValue, csInvalidNumericValue };

struct DECLSPEC_DRECORD TConvertInfo
{
public:
	TStringHeap* StringHeap;
	void *Source;
	int SourceOffset;
	int SourceLen;
	int SourceScale;
	void *Dest;
	int DestOffset;
	int DestLen;
	int DestScale;
	bool IgnoreConvertErrors;
	System::UnicodeString Format;
};


typedef TConvertStatus __fastcall (__closure *TConvertProcedure)(TConvertInfo &ConvertInfo);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFieldDesc : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TRecordSetClass FRecordSetClass;
	System::Word FDataType;
	System::Word FSubDataType;
	TObjectType* FObjectType;
	TFieldDesc* FParentField;
	bool FUpdateable;
	TFieldConstraint* FCustomConstraint;
	void __fastcall SetObjectType(TObjectType* Value);
	void __fastcall SetParentField(TFieldDesc* Value);
	
protected:
	System::UnicodeString FName;
	System::UnicodeString FActualName;
	System::Word FLength;
	System::Word FScale;
	System::Word FFieldNo;
	int FActualFieldNo;
	int FSize;
	int FOffset;
	int FDataOffset;
	bool FRequired;
	bool FReadOnly;
	bool FIsKey;
	bool FFixed;
	bool FHidden;
	bool FHiddenObject;
	void *FHandle;
	bool FReserved;
	TFieldDescKind FFieldDescKind;
	bool FIsAutoIncrement;
	bool FIsBlob;
	bool FIsObject;
	bool FIsSharedObject;
	bool FIsComplex;
	bool FHasValueLen;
	bool FHasParent;
	virtual int __fastcall GetMapLength();
	virtual System::Word __fastcall GetMapDataType();
	virtual void __fastcall SetDataType(System::Word Value);
	virtual void __fastcall SetSubDataType(System::Word Value);
	__property TRecordSetClass RecordSetClass = {read=FRecordSetClass};
	
public:
	__fastcall virtual TFieldDesc(TRecordSetClass RecordSetClass);
	__fastcall virtual ~TFieldDesc();
	virtual void __fastcall Assign(TFieldDesc* FieldDesc);
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString ActualName = {read=FActualName, write=FActualName};
	__property System::Word DataType = {read=FDataType, write=SetDataType, nodefault};
	__property System::Word SubDataType = {read=FSubDataType, write=SetSubDataType, nodefault};
	__property System::Word Length = {read=FLength, write=FLength, nodefault};
	__property System::Word Scale = {read=FScale, write=FScale, nodefault};
	__property System::Word FieldNo = {read=FFieldNo, write=FFieldNo, nodefault};
	__property int ActualFieldNo = {read=FActualFieldNo, write=FActualFieldNo, nodefault};
	__property int Size = {read=FSize, write=FSize, nodefault};
	__property int Offset = {read=FOffset, write=FOffset, nodefault};
	__property int DataOffset = {read=FDataOffset, write=FDataOffset, nodefault};
	__property bool Required = {read=FRequired, write=FRequired, nodefault};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, nodefault};
	__property bool IsKey = {read=FIsKey, write=FIsKey, nodefault};
	__property bool Fixed = {read=FFixed, write=FFixed, nodefault};
	__property bool Hidden = {read=FHidden, write=FHidden, nodefault};
	__property TObjectType* ObjectType = {read=FObjectType, write=SetObjectType};
	__property TFieldDesc* ParentField = {read=FParentField, write=SetParentField};
	__property bool HiddenObject = {read=FHiddenObject, write=FHiddenObject, nodefault};
	__property void * Handle = {read=FHandle, write=FHandle};
	__property TFieldDescKind FieldDescKind = {read=FFieldDescKind, write=FFieldDescKind, nodefault};
	__property bool IsAutoIncrement = {read=FIsAutoIncrement, write=FIsAutoIncrement, nodefault};
	__property bool Updateable = {read=FUpdateable, write=FUpdateable, nodefault};
	__property System::Word MapDataType = {read=GetMapDataType, nodefault};
	__property int MapLength = {read=GetMapLength, nodefault};
	__property bool IsBlob = {read=FIsBlob, nodefault};
	__property bool IsObject = {read=FIsObject, nodefault};
	__property bool IsSharedObject = {read=FIsSharedObject, nodefault};
	__property bool IsComplex = {read=FIsComplex, nodefault};
	__property bool HasValueLen = {read=FHasValueLen, nodefault};
	__property bool HasParent = {read=FHasParent, nodefault};
	__property TFieldConstraint* CustomConstraint = {read=FCustomConstraint, write=FCustomConstraint};
};

#pragma pack(pop)

typedef System::TMetaClass* TFieldDescClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFieldDescs : public Crtypes::TCRObjectList
{
	typedef Crtypes::TCRObjectList inherited;
	
public:
	TFieldDesc* operator[](int Index) { return this->Items[Index]; }
	
private:
	TFieldDesc* __fastcall GetItems(int Index);
	
public:
	TFieldDesc* __fastcall FindField(const System::UnicodeString Name);
	TFieldDesc* __fastcall FieldByName(const System::UnicodeString Name);
	TFieldDesc* __fastcall FieldByActualFieldNo(int ActualFieldNo);
	__property TFieldDesc* Items[int Index] = {read=GetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TFieldDescs() { }
	
public:
	/* TObject.Create */ inline __fastcall TFieldDescs() : Crtypes::TCRObjectList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSortColumn : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TFieldDesc* FieldDesc;
	bool DescendingOrder;
	TSortType SortType;
	bool UseForRangeStart;
	bool UseForRangeEnd;
public:
	/* TObject.Create */ inline __fastcall TSortColumn() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TSortColumn() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSharedObject : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	int FRefCount;
	void *FGCHandle;
	void * __fastcall GetGCHandle();
	
public:
	__fastcall TSharedObject();
	__fastcall virtual ~TSharedObject();
	HIDESBASE void __fastcall Free();
	System::Variant __fastcall ToVariant();
	__classmethod TSharedObject* __fastcall FromVariant(const System::Variant &Source);
	void __fastcall CheckValid();
	void __fastcall AddRef();
	void __fastcall Release();
	virtual void __fastcall Disconnect();
	__property int RefCount = {read=FRefCount, nodefault};
	__property void * GCHandle = {read=GetGCHandle};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAttribute : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FName;
	System::Word FDataType;
	System::Word FSubDataType;
	System::Word FLength;
	System::Word FScale;
	int FSize;
	int FDataSize;
	int FOffset;
	int FIndicatorOffset;
	System::Word FAttributeNo;
	TObjectType* FObjectType;
	TObjectType* FOwner;
	bool FFixed;
	void __fastcall SetObjectType(TObjectType* Value);
	
protected:
	virtual System::UnicodeString __fastcall GetActualName();
	virtual void __fastcall SetActualName(const System::UnicodeString Value);
	
public:
	__fastcall TAttribute();
	__fastcall virtual ~TAttribute();
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property System::UnicodeString ActualName = {read=GetActualName, write=SetActualName};
	__property System::Word DataType = {read=FDataType, write=FDataType, nodefault};
	__property System::Word SubDataType = {read=FSubDataType, write=FSubDataType, nodefault};
	__property bool Fixed = {read=FFixed, write=FFixed, nodefault};
	__property System::Word Length = {read=FLength, write=FLength, nodefault};
	__property System::Word Scale = {read=FScale, write=FScale, nodefault};
	__property int Size = {read=FSize, write=FSize, nodefault};
	__property int DataSize = {read=FDataSize, write=FDataSize, nodefault};
	__property int Offset = {read=FOffset, write=FOffset, nodefault};
	__property int IndicatorOffset = {read=FIndicatorOffset, write=FIndicatorOffset, nodefault};
	__property System::Word AttributeNo = {read=FAttributeNo, write=FAttributeNo, nodefault};
	__property TObjectType* ObjectType = {read=FObjectType, write=SetObjectType};
	__property TObjectType* Owner = {read=FOwner, write=FOwner};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAttributeChain : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TAttribute* FAttribute;
	int FIndex;
	TAttributeChain* FPrev;
	TAttributeChain* FNext;
	System::UnicodeString __fastcall GetAsString();
	
public:
	__fastcall TAttributeChain(TAttribute* Attribute, TAttributeChain* Prev, int Index);
	__fastcall virtual ~TAttributeChain();
	TAttributeChain* __fastcall First();
	TAttributeChain* __fastcall Last();
	__property TAttribute* Attribute = {read=FAttribute};
	__property int Index = {read=FIndex, nodefault};
	__property TAttributeChain* Prev = {read=FPrev};
	__property TAttributeChain* Next = {read=FNext};
	__property System::UnicodeString AsString = {read=GetAsString};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TObjectType : public TSharedObject
{
	typedef TSharedObject inherited;
	
private:
	TAttribute* __fastcall GetAttributes(int Index);
	int __fastcall GetAttributeCount();
	
protected:
	System::UnicodeString FName;
	System::Word FDataType;
	int FSize;
	Crtypes::TCRObjectList* FAttributes;
	void __fastcall ClearAttributes();
	TAttributeChain* __fastcall ParseAttribute(System::UnicodeString Name);
	
public:
	__fastcall TObjectType();
	__fastcall virtual ~TObjectType();
	virtual TAttribute* __fastcall FindAttribute(const System::UnicodeString Name);
	TAttribute* __fastcall GetAttribute(const System::UnicodeString Name);
	TAttributeChain* __fastcall GetAttributeChain(const System::UnicodeString Name);
	__property System::UnicodeString Name = {read=FName};
	__property System::Word DataType = {read=FDataType, nodefault};
	__property int Size = {read=FSize, nodefault};
	__property TAttribute* Attributes[int Index] = {read=GetAttributes};
	__property int AttributeCount = {read=GetAttributeCount, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBObject : public TSharedObject
{
	typedef TSharedObject inherited;
	
private:
	TObjectType* FObjectType;
	
protected:
	virtual void __fastcall SetObjectType(TObjectType* Value);
	virtual bool __fastcall GetIsNull() = 0 ;
	virtual void __fastcall GetAttributeValue(const System::UnicodeString Name, /* out */ void * &AttrBuf, /* out */ System::Word &AttrLen, /* out */ bool &IsBlank, /* out */ bool &NativeBuffer);
	virtual void __fastcall SetAttributeValue(const System::UnicodeString Name, void * ValuePtr, System::Word ValueLen);
	virtual bool __fastcall GetAttrIsNull(const System::UnicodeString Name);
	
public:
	__fastcall TDBObject();
	__property bool IsNull = {read=GetIsNull, nodefault};
	__property TObjectType* ObjectType = {read=FObjectType};
public:
	/* TSharedObject.Destroy */ inline __fastcall virtual ~TDBObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCacheItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TItemHeader *Item;
	TItemHeader *Restore;
	TCacheItem* Next;
public:
	/* TObject.Create */ inline __fastcall TCacheItem() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TCacheItem() { }
	
};

#pragma pack(pop)

typedef TRecBookmark *PRecBookmark;

struct DECLSPEC_DRECORD TRecBookmark
{
public:
	int RefreshIteration;
	TItemHeader *Item;
	int Order;
};


typedef bool __fastcall (__closure *TFilterFunc)(void * RecBuf);

class PASCALIMPLEMENTATION TBoolParser : public Crparser::TParser
{
	typedef Crparser::TParser inherited;
	
private:
	bool FOmitStringQuote;
	
protected:
	virtual bool __fastcall IsStringQuote(System::WideChar Ch);
	virtual void __fastcall ToRightQuote(System::WideChar LeftQuote);
	
public:
	__fastcall virtual TBoolParser(const System::UnicodeString Text)/* overload */;
	__property bool OmitStringQuote = {read=FOmitStringQuote, write=FOmitStringQuote, nodefault};
public:
	/* TParser.Create */ inline __fastcall TBoolParser(System::Classes::TStream* const Stream, Clrclasses::Encoding* AEncoding)/* overload */ : Crparser::TParser(Stream, AEncoding) { }
	/* TParser.Create */ inline __fastcall virtual TBoolParser(System::Classes::TStream* const Stream, __int64 ASize, Clrclasses::Encoding* AEncoding)/* overload */ : Crparser::TParser(Stream, ASize, AEncoding) { }
	/* TParser.Destroy */ inline __fastcall virtual ~TBoolParser() { }
	
};


enum DECLSPEC_DENUM TExpressionType : unsigned char { ntEqual, ntMore, ntLess, ntMoreEqual, ntLessEqual, ntNoEqual, ntAnd, ntOr, ntNot, ntField, ntValue, ntTrue, ntFalse, ntLike, ntNotLike, ntIn, ntNotIn, ntLower, ntUpper, ntBetween, ntNotBetween, ntMatch, ntGlob, ntRegExp };

class PASCALIMPLEMENTATION TExpressionNode : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TExpressionNode* NextAlloc;
	TExpressionType NodeType;
	TExpressionNode* LeftOperand;
	TExpressionNode* RightOperand;
	TExpressionNode* NextOperand;
	TFieldDesc* FieldDesc;
	System::Variant Value;
	bool UseCalculatedFields;
public:
	/* TObject.Create */ inline __fastcall TExpressionNode() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TExpressionNode() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCondition : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TData* FData;
	TExpressionNode* FExpression;
	TExpressionNode* FFirstAlloc;
	TBoolParser* FParser;
	int FCode;
	System::UnicodeString FStrLexem;
	System::UnicodeString FText;
	
public:
	__fastcall TCondition(TData* Data, const System::UnicodeString Text);
	__fastcall virtual ~TCondition();
	virtual void __fastcall ExpressionError() = 0 ;
	virtual TFieldDesc* __fastcall GetField(const System::UnicodeString FieldName);
	TExpressionNode* __fastcall AllocNode();
	TExpressionNode* __fastcall OrExpr();
	TExpressionNode* __fastcall AndExpr();
	TExpressionNode* __fastcall Condition();
	TExpressionNode* __fastcall Argument();
	virtual void __fastcall CreateExpression();
	virtual void __fastcall FreeExpression();
	__property TData* Data = {read=FData, write=FData};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property TExpressionNode* Expression = {read=FExpression};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFilter : public TCondition
{
	typedef TCondition inherited;
	
public:
	virtual void __fastcall ExpressionError();
public:
	/* TCondition.Create */ inline __fastcall TFilter(TData* Data, const System::UnicodeString Text) : TCondition(Data, Text) { }
	/* TCondition.Destroy */ inline __fastcall virtual ~TFilter() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TConstraint : public TCondition
{
	typedef TCondition inherited;
	
public:
	System::UnicodeString FErrorMessage;
	System::UnicodeString FAlias;
	bool FComplexConstraint;
	__fastcall TConstraint(TData* Data, const System::UnicodeString Text, const System::UnicodeString ConstraintErrorMessage, bool UseCreateExpression);
	virtual void __fastcall ExpressionError();
	void __fastcall ConstraintError();
	virtual void __fastcall CreateExpression();
	virtual void __fastcall FreeExpression();
	virtual void __fastcall EmptyConstraint();
	void __fastcall UpdateConstraint(const System::UnicodeString Text, const System::UnicodeString ConstraintErrorMessage);
	__property System::UnicodeString ErrorMessage = {read=FErrorMessage, write=FErrorMessage};
	__property bool ComplexConstraint = {read=FComplexConstraint, nodefault};
public:
	/* TCondition.Destroy */ inline __fastcall virtual ~TConstraint() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFieldConstraint : public TConstraint
{
	typedef TConstraint inherited;
	
private:
	TFieldDesc* FField;
	
public:
	__fastcall TFieldConstraint(TData* Data, const System::UnicodeString Text, const System::UnicodeString ConstraintErrorMessage, TFieldDesc* Field, bool UseCreateExpression);
	virtual TFieldDesc* __fastcall GetField(const System::UnicodeString FieldName);
	virtual void __fastcall EmptyConstraint();
	HIDESBASE void __fastcall UpdateConstraint(const System::UnicodeString Text, const System::UnicodeString ConstraintErrorMessage, TFieldDesc* Field);
	__property TFieldDesc* Field = {read=FField, write=FField};
public:
	/* TCondition.Destroy */ inline __fastcall virtual ~TFieldConstraint() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TUpdateRecKind : unsigned char { ukUpdate, ukInsert, ukDelete };

typedef System::Set<TUpdateRecKind, TUpdateRecKind::ukUpdate, TUpdateRecKind::ukDelete> TUpdateRecKinds;

typedef void __fastcall (__closure *TOnModifyRecord)(void);

typedef void __fastcall (__closure *TOnApplyRecord)(TUpdateRecKind UpdateKind, TUpdateRecAction &Action, bool LastItem);

typedef void __fastcall (__closure *TOnGetCachedFields)(void);

typedef void __fastcall (__closure *TOnGetCachedBuffer)(void * Buffer, void * Source/* = (void *)(0x0)*/);

typedef void __fastcall (__closure *TOnFieldsChanged)(void);

class PASCALIMPLEMENTATION TData : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FActive;
	bool FPrepared;
	bool FCachedUpdates;
	bool FLocalUpdate;
	bool FInCacheProcessing;
	void *FNewCacheRecBuf;
	void *FOldCacheRecBuf;
	TOnModifyRecord FOnAppend;
	TOnModifyRecord FOnDelete;
	TOnModifyRecord FOnUpdate;
	TOnApplyRecord FOnApplyRecord;
	TOnModifyRecord FOnCacheChanged;
	TOnModifyRecord FOnCacheApplied;
	TOnModifyRecord FOnCacheCanceled;
	TOnModifyRecord FAfterApplyUpdates;
	bool FAutoInitFields;
	bool FTrimFixedChar;
	bool FTrimVarChar;
	TOnFieldsChanged FOnFieldsChanged;
	TFilterFunc FFilterFunc;
	TFilterFunc FFilterMDFunc;
	TFilterFunc FFilterRangeFunc;
	bool FFilterCaseInsensitive;
	bool FFilterNoPartialCompare;
	TItemTypes FFilterItemTypes;
	bool FFilterUseRollBack;
	bool FRecordSearch;
	TBoolParser* FParser;
	int FCode;
	TCondition* FFilterCondition;
	Crtypes::TCRObjectList* FConstraints;
	Crtypes::TCRObjectList* FFieldConstraints;
	bool FHasBlobFields;
	bool FHasComplexFields;
	bool FSparseArrays;
	TOnGetCachedFields FOnGetCachedFields;
	TOnGetCachedBuffer FOnGetCachedBuffer;
	TExpressionNode* __fastcall GetFilterExpression();
	int __fastcall InternalCompareFields(void * DataBuf, System::Word DataLen, bool IsBlank, int DataType, void * RecBuf, TFieldDesc* FieldDesc, const TCompareOptions Options, bool Mapped);
	void __fastcall SetCachedUpdates(bool Value);
	
protected:
	int FDataSize;
	int FRecordSize;
	int FCalcDataSize;
	int FCalcRecordSize;
	void *FilterRecBuf;
	TStringHeap* FStringHeap;
	bool FSetEmptyStrToNull;
	bool FRequireEmptyStrToNull;
	int FRecordNoOffset;
	int FRecordCount;
	bool FBOF;
	bool FEOF;
	TFieldDescs* FFields;
	virtual void __fastcall SetTrimFixedChar(bool Value);
	virtual void __fastcall SetTrimVarChar(bool Value);
	virtual void __fastcall InternalPrepare();
	virtual void __fastcall InternalUnPrepare();
	virtual void __fastcall InternalOpen(bool DisableInitFields = false);
	virtual void __fastcall InternalClose();
	virtual void __fastcall InitData();
	virtual void __fastcall FreeData();
	virtual void __fastcall CheckFetched(void * RecBuf, TFieldDesc* Field);
	virtual void __fastcall InitRecordSize();
	void __fastcall InitCalcDataSize();
	virtual void __fastcall CreateFieldDescs();
	void __fastcall InitObjectFields(TObjectType* ObjectType, TFieldDesc* Parent);
	TSharedObject* __fastcall InternalGetObject(TFieldDesc* Field, void * RecBuf)/* overload */;
	TSharedObject* __fastcall InternalGetObject(void * DataBuf)/* overload */;
	void __fastcall InternalSetObject(void * DataBuf, TSharedObject* Obj);
	virtual System::UnicodeString __fastcall GetArrayFieldName(TObjectType* ObjectType, int ItemIndex);
	virtual int __fastcall InternalCompareFieldValue(void * ValuePtr, System::Word ValueLen, System::Word ValueType, void * DataBuf, System::Word DataLen, System::Word FieldType, bool HasParent, bool IsFixed, const TCompareOptions Options);
	virtual int __fastcall GetIndicatorItemSize();
	virtual int __fastcall GetIndicatorSize();
	void __fastcall GetChildFieldInfo(TFieldDesc* Field, /* out */ TFieldDesc* &RootField, /* out */ System::UnicodeString &AttrName);
	virtual void __fastcall GetChildField(TFieldDesc* Field, void * RecBuf, /* out */ void * &DataBuf, /* out */ System::Word &DataLen, /* out */ bool &IsBlank, /* out */ bool &NativeBuffer);
	void __fastcall PutChildField(TFieldDesc* Field, void * RecBuf, void * ValuePtr, System::Word ValueLen);
	virtual bool __fastcall GetChildFieldIsNull(TFieldDesc* Field, void * RecBuf);
	virtual bool __fastcall GetEOF();
	virtual bool __fastcall GetBOF();
	virtual int __fastcall GetRecordCount();
	virtual int __fastcall GetRecordNo();
	virtual void __fastcall SetRecordNo(int Value);
	virtual void __fastcall InternalAppend(void * RecBuf);
	virtual void __fastcall InternalDelete();
	virtual void __fastcall InternalUpdate(void * RecBuf);
	bool __fastcall Filtered();
	System::UnicodeString __fastcall GetFilterText();
	virtual void __fastcall SetFilterText(const System::UnicodeString Value);
	int __fastcall InternalAnsiStrComp(const void * Value1, const void * Value2, const TCompareOptions Options);
	int __fastcall InternalAnsiCompareText(const System::AnsiString Value1, const System::AnsiString Value2, const TCompareOptions Options);
	int __fastcall InternalWStrLComp(const System::WideString Value1, const System::WideString Value2, const TCompareOptions Options);
	int __fastcall InternalWStrComp(const System::WideString Value1, const System::WideString Value2, const TCompareOptions Options);
	int __fastcall CompareStrValues(const System::AnsiString Value, const System::AnsiString FieldValue, const TCompareOptions Options);
	int __fastcall CompareWideStrValues(const System::WideString Value, const System::WideString FieldValue, const TCompareOptions Options);
	int __fastcall CompareBinValues(const void * Value, const int ValueLen, const void * FieldValue, const int FieldValueLen, const TCompareOptions Options);
	virtual void __fastcall InternalCacheChanged();
	virtual void __fastcall InternalCacheApplied();
	virtual void __fastcall InternalCacheCanceled();
	virtual bool __fastcall GetUpdatesPending();
	virtual void __fastcall SetFilterItemTypes(const TItemTypes Value);
	
public:
	__fastcall TData();
	__fastcall virtual ~TData();
	bool __fastcall Eval(TExpressionNode* Node)/* overload */;
	bool __fastcall Eval(TExpressionNode* Node, TFieldDesc* ConstraintField, void * ValuePtr, System::Word ValueLen)/* overload */;
	void __fastcall Open();
	void __fastcall Close();
	virtual void __fastcall Prepare();
	virtual void __fastcall UnPrepare();
	virtual bool __fastcall IsFullReopen();
	virtual void __fastcall Reopen();
	virtual bool __fastcall NeedConvertEOL();
	__classmethod virtual int __fastcall GetBufferSize(System::Word DataType, int DataLen);
	virtual TFieldDescClass __fastcall GetFieldDescType();
	TFieldDesc* __fastcall CreateFieldDesc();
	virtual void __fastcall InitFields();
	virtual void __fastcall ExplicitInitFields();
	virtual void __fastcall ClearFields();
	virtual void __fastcall InternalInitFieldDescs();
	bool __fastcall IsEqualDataType(TFieldDesc* Field1, TFieldDesc* Field2);
	virtual void __fastcall GetField(TFieldDesc* Field, void * RecBuf, void * Dest, /* out */ System::Word &DestLen, bool NeedConvert, /* out */ bool &IsBlank);
	virtual void __fastcall GetFieldData(TFieldDesc* Field, void * DataBuf, System::Word &DataLen, void * Dest, bool NeedConvert);
	void * __fastcall GetDataBuf(void * RecBuf, TFieldDesc* FieldDesc, /* out */ PWORD &DataLenPtr);
	void * __fastcall GetFieldBuf(void * RecBuf, TFieldDesc* FieldDesc, /* out */ System::Word &DataLen, /* out */ bool &IsBlank, /* out */ bool &NativeBuffer);
	void * __fastcall GetMappedFieldBuf(void * RecBuf, TFieldDesc* FieldDesc, /* out */ System::Word &DataLen, /* out */ System::Word &DataType, /* out */ bool &HasParent, /* out */ bool &IsFixed, /* out */ bool &IsBlank, /* out */ bool &NativeBuffer);
	virtual void * __fastcall GetMappedDataBuf(TFieldDesc* FieldDesc, void * DataBuf, System::Word &DataLen, System::Word &DataType, bool &HasParent, bool &IsFixed);
	virtual void __fastcall PutField(TFieldDesc* Field, void * RecBuf, void * ValuePtr, System::Word ValueLen, bool NeedConvert, bool IsDatabaseValue = false);
	virtual void __fastcall PutFieldData(TFieldDesc* Field, void * DataBuf, PWORD DataLenPtr, void * ValuePtr, System::Word ValueLen, bool NeedConvert, bool IsDatabaseValue = false);
	virtual bool __fastcall GetNull(TFieldDesc* Field, void * RecBuf);
	virtual void __fastcall SetNull(TFieldDesc* Field, void * RecBuf, bool Value);
	bool __fastcall GetNullByBlob(TFieldDesc* Field, void * RecBuf);
	virtual bool __fastcall GetChanged(TFieldDesc* Field, void * RecBuf);
	virtual void __fastcall SetChanged(TFieldDesc* Field, void * RecBuf, bool Value);
	virtual void __fastcall GetFieldAsVariant(TFieldDesc* Field, void * RecBuf, /* out */ System::Variant &Value, bool UseRollback = false);
	virtual void __fastcall GetDataAsVariant(void * DataBuf, System::Word DataLen, System::Word DataType, System::Word SubDataType, bool HasParent, bool IsFixed, System::Variant &Value, bool UseRollback);
	virtual void __fastcall GetMappedFieldAsVariant(TFieldDesc* Field, void * RecBuf, /* out */ System::Variant &Value, bool UseRollback = false, bool FlatRecBuf = false);
	virtual void __fastcall GetMappedDataAsVariant(TFieldDesc* Field, void * DataBuf, System::Word DataLen, System::Variant &Value, bool UseRollback = false, bool FlatRecBuf = false);
	virtual void __fastcall PutFieldAsVariant(TFieldDesc* Field, void * RecBuf, const System::Variant &Value, bool IsDatabaseValue = false);
	virtual void __fastcall PutDataAsVariant(void * DataBuf, PWORD DataLenPtr, System::Word DataType, System::Word Len, System::Word Scale, bool HasParent, const System::Variant &Value, bool IsDatabaseValue);
	__classmethod virtual void __fastcall GetDateFromBuf(void * Buf, void * Date, bool HasParent, TDateFormat Format);
	__classmethod virtual void __fastcall PutDateToBuf(void * Buf, void * Date, bool HasParent, TDateFormat Format);
	TFieldDesc* __fastcall FindField(const System::UnicodeString Name);
	TFieldDesc* __fastcall FieldByName(const System::UnicodeString Name);
	__classmethod virtual bool __fastcall IsBlobDataType(System::Word DataType);
	__classmethod virtual bool __fastcall IsObjectDataType(System::Word DataType);
	__classmethod virtual bool __fastcall IsSharedObjectDataType(System::Word DataType);
	__classmethod virtual bool __fastcall IsComplexDataType(System::Word DataType);
	__classmethod virtual bool __fastcall HasValueLen(System::Word DataType);
	bool __fastcall HasFields(const TFieldTypeSet &FieldTypes);
	bool __fastcall CheckHasBlobFields();
	bool __fastcall CheckHasComplexFields();
	virtual bool __fastcall FieldListDependsOnParams();
	void __fastcall AllocRecBuf(/* out */ void * &RecBuf);
	void __fastcall FreeRecBuf(void * RecBuf);
	virtual void __fastcall InitRecord(void * RecBuf);
	virtual void __fastcall GetRecord(void * RecBuf) = 0 ;
	virtual void __fastcall GetNextRecord(void * RecBuf) = 0 ;
	virtual void __fastcall GetPriorRecord(void * RecBuf) = 0 ;
	virtual void __fastcall PutRecord(void * RecBuf) = 0 ;
	virtual void __fastcall AppendRecord(void * RecBuf) = 0 ;
	void __fastcall AppendBlankRecord();
	virtual void __fastcall InsertRecord(void * RecBuf) = 0 ;
	virtual void __fastcall UpdateRecord(void * RecBuf) = 0 ;
	virtual void __fastcall DeleteRecord() = 0 ;
	virtual void __fastcall EditRecord(void * RecBuf);
	virtual void __fastcall PostRecord(void * RecBuf);
	virtual void __fastcall CancelRecord(void * RecBuf);
	void __fastcall CreateComplexFields(void * RecBuf, bool WithBlob);
	virtual void __fastcall CreateComplexField(void * RecBuf, TFieldDesc* Field);
	void __fastcall AddRefComplexFields(void * RecBuf, bool CreateBlob = false);
	void __fastcall FreeComplexFields(void * RecBuf, bool WithBlob);
	virtual void __fastcall FreeComplexField(void * RecBuf, TFieldDesc* Field);
	void __fastcall CopyComplexFields(void * SourceRecBuf, void * DestRecBuf, bool WithBlob);
	virtual void __fastcall CopyComplexField(void * SourceRecBuf, void * DestRecBuf, TFieldDesc* Field);
	virtual void __fastcall AddRef(void * RecBuf);
	virtual void __fastcall ReleaseRef(void * RecBuf, bool IsResync, bool WithBlob);
	virtual void __fastcall SetToBegin();
	virtual void __fastcall SetToEnd();
	virtual void __fastcall GetBookmark(PRecBookmark Bookmark);
	virtual void __fastcall SetToBookmark(PRecBookmark Bookmark);
	virtual bool __fastcall BookmarkValid(PRecBookmark Bookmark);
	virtual int __fastcall CompareBookmarks(PRecBookmark Bookmark1, PRecBookmark Bookmark2);
	virtual bool __fastcall NeedGetRecordAfterGotoBookmark();
	virtual TItemStatus __fastcall GetUpdateStatus();
	virtual TUpdateRecAction __fastcall GetUpdateResult();
	virtual bool __fastcall HasUpdatedOrDeletedRecords();
	virtual void __fastcall SetCacheRecBuf(void * NewBuf, void * OldBuf);
	virtual void __fastcall ApplyUpdates(const TUpdateRecKinds UpdateRecKinds);
	virtual void __fastcall CommitUpdates();
	virtual void __fastcall CancelUpdates();
	virtual void __fastcall RestoreUpdates();
	virtual void __fastcall RevertRecord();
	virtual void __fastcall ApplyRecord(TUpdateRecKind UpdateKind, TUpdateRecAction &Action, bool LastItem);
	void __fastcall DoAfterApplyUpdates();
	virtual void __fastcall GetOldRecord(void * RecBuf);
	void __fastcall CheckConstraint(TFieldDesc* Field, void * RecBuf, void * ValuePtr, System::Word ValueLen, TFieldConstraint* FieldConstraint);
	void __fastcall CheckConstraints(void * RecBuf, Crtypes::TCRObjectList* Constraints);
	virtual void __fastcall FilterUpdated();
	int __fastcall CompareFieldValue(void * ValuePtr, System::Word ValueLen, System::Word ValueType, TFieldDesc* FieldDesc, void * RecBuf, const TCompareOptions Options, bool Mapped);
	virtual TCompareOptions __fastcall GetSortOptions(TSortColumn* SortColumn);
	int __fastcall CompareFields(void * RecBuf1, void * RecBuf2, TFieldDesc* FieldDesc, const TCompareOptions Options, bool Mapped);
	void __fastcall StartSearch();
	void __fastcall EndSearch();
	TBlob* __fastcall GetBlob(TFieldDesc* Field, void * RecBuf);
	void __fastcall SetBlob(TFieldDesc* Field, void * RecBuf, TBlob* Blob);
	unsigned __fastcall ReadBlob(TBlob* Blob, unsigned Position, unsigned Count, void * Dest, bool FromRollback = false, bool TrueUnicode = false)/* overload */;
	unsigned __fastcall ReadBlob(TFieldDesc* Field, void * RecBuf, unsigned Position, unsigned Count, void * Dest, bool FromRollback = false, bool TrueUnicode = false)/* overload */;
	void __fastcall WriteBlob(TBlob* Blob, unsigned Position, unsigned Count, void * Source, bool TrueUnicode = false)/* overload */;
	void __fastcall WriteBlob(TFieldDesc* Field, void * RecBuf, unsigned Position, unsigned Count, void * Source, bool TrueUnicode = false)/* overload */;
	int __fastcall TruncateBlob(TBlob* Blob, unsigned Size, bool TrueUnicode = false)/* overload */;
	int __fastcall TruncateBlob(TFieldDesc* Field, void * RecBuf, unsigned Size, bool TrueUnicode = false)/* overload */;
	unsigned __fastcall GetBlobSize(TBlob* Blob, bool FromRollback = false, bool TrueUnicode = false)/* overload */;
	unsigned __fastcall GetBlobSize(TFieldDesc* Field, void * RecBuf, bool FromRollback = false, bool TrueUnicode = false)/* overload */;
	void __fastcall SetBlobSize(TBlob* Blob, unsigned NewSize, bool FromRollback = false, bool TrueUnicode = false)/* overload */;
	void __fastcall SetBlobSize(TFieldDesc* Field, void * RecBuf, unsigned NewSize, bool FromRollback = false, bool TrueUnicode = false)/* overload */;
	__property bool Active = {read=FActive, write=FActive, nodefault};
	__property bool Prepared = {read=FPrepared, write=FPrepared, nodefault};
	__property TFieldDescs* Fields = {read=FFields};
	__property bool Bof = {read=GetBOF, nodefault};
	__property bool Eof = {read=GetEOF, write=FEOF, nodefault};
	__property int RecordSize = {read=FRecordSize, nodefault};
	__property int CalcRecordSize = {read=FCalcRecordSize, nodefault};
	__property int DataSize = {read=FDataSize, nodefault};
	__property TStringHeap* StringHeap = {read=FStringHeap};
	__property int RecordCount = {read=GetRecordCount, nodefault};
	__property int RecordNo = {read=GetRecordNo, write=SetRecordNo, nodefault};
	__property bool CachedUpdates = {read=FCachedUpdates, write=SetCachedUpdates, default=0};
	__property bool LocalUpdate = {read=FLocalUpdate, write=FLocalUpdate, default=0};
	__property bool InCacheProcessing = {read=FInCacheProcessing, write=FInCacheProcessing, nodefault};
	__property void * NewCacheRecBuf = {read=FNewCacheRecBuf, write=FNewCacheRecBuf};
	__property void * OldCacheRecBuf = {read=FOldCacheRecBuf, write=FOldCacheRecBuf};
	__property bool UpdatesPending = {read=GetUpdatesPending, nodefault};
	__property TFilterFunc FilterFunc = {read=FFilterFunc, write=FFilterFunc};
	__property TFilterFunc FilterMDFunc = {read=FFilterMDFunc, write=FFilterMDFunc};
	__property TFilterFunc FilterRangeFunc = {read=FFilterRangeFunc, write=FFilterRangeFunc};
	__property System::UnicodeString FilterText = {read=GetFilterText, write=SetFilterText};
	__property bool FilterCaseInsensitive = {read=FFilterCaseInsensitive, write=FFilterCaseInsensitive, nodefault};
	__property bool FilterNoPartialCompare = {read=FFilterNoPartialCompare, write=FFilterNoPartialCompare, nodefault};
	__property TItemTypes FilterItemTypes = {read=FFilterItemTypes, write=SetFilterItemTypes, nodefault};
	__property TExpressionNode* FilterExpression = {read=GetFilterExpression};
	__property bool AutoInitFields = {read=FAutoInitFields, write=FAutoInitFields, nodefault};
	__property bool TrimFixedChar = {read=FTrimFixedChar, write=SetTrimFixedChar, nodefault};
	__property bool TrimVarChar = {read=FTrimVarChar, write=SetTrimVarChar, nodefault};
	__property bool SetEmptyStrToNull = {read=FSetEmptyStrToNull, write=FSetEmptyStrToNull, nodefault};
	__property bool RequireEmptyStrToNull = {read=FRequireEmptyStrToNull, nodefault};
	__property Crtypes::TCRObjectList* Constraints = {read=FConstraints};
	__property Crtypes::TCRObjectList* FieldConstraints = {read=FFieldConstraints};
	__property bool SparseArrays = {read=FSparseArrays, write=FSparseArrays, nodefault};
	__property bool HasBlobFields = {read=FHasBlobFields, write=FHasBlobFields, nodefault};
	__property bool HasComplexFields = {read=FHasComplexFields, write=FHasComplexFields, nodefault};
	__property TOnModifyRecord OnAppend = {read=FOnAppend, write=FOnAppend};
	__property TOnModifyRecord OnDelete = {read=FOnDelete, write=FOnDelete};
	__property TOnModifyRecord OnUpdate = {read=FOnUpdate, write=FOnUpdate};
	__property TOnApplyRecord OnApplyRecord = {read=FOnApplyRecord, write=FOnApplyRecord};
	__property TOnModifyRecord OnCacheChanged = {read=FOnCacheChanged, write=FOnCacheChanged};
	__property TOnModifyRecord OnCacheApplied = {read=FOnCacheApplied, write=FOnCacheApplied};
	__property TOnModifyRecord OnCacheCanceled = {read=FOnCacheCanceled, write=FOnCacheCanceled};
	__property TOnModifyRecord AfterApplyUpdates = {read=FAfterApplyUpdates, write=FAfterApplyUpdates};
	__property TOnGetCachedFields OnGetCachedFields = {read=FOnGetCachedFields, write=FOnGetCachedFields};
	__property TOnGetCachedBuffer OnGetCachedBuffer = {read=FOnGetCachedBuffer, write=FOnGetCachedBuffer};
	__property TOnFieldsChanged OnFieldsChanged = {read=FOnFieldsChanged, write=FOnFieldsChanged};
};


typedef System::DynamicArray<PItemHeader> TRecordNoCache;

typedef int __fastcall (__closure *TItemsCompareFunction)(void * Item1, void * Item2);

class PASCALIMPLEMENTATION TMemData : public TData
{
	typedef TData inherited;
	
private:
	TCacheItem* Cache;
	TCacheItem* LastCacheItem;
	int FRefreshIteration;
	Crtypes::TCRObjectList* FIndexFields;
	void *FCalcRecBuf;
	void *FCalcRecBuf2;
	TRecordNoCache FRecordNoCache;
	TSortColumn* __fastcall GetIndexField(int Index);
	int __fastcall GetIndexFieldCount();
	void __fastcall ClearIndexFields();
	void __fastcall UpdateIndexFields();
	int __fastcall CompareRecords(void * RecBuf1, void * RecBuf2);
	int __fastcall CompareByRecBuf(void * Item1, void * Item2);
	int __fastcall CompareBySavedOrder(void * Item1, void * Item2);
	void __fastcall Exchange(PItemHeader I, PItemHeader J);
	void __fastcall MoveSortedRecord(int Dir);
	void __fastcall QuickSort(PItemHeader L, PItemHeader R, PItemHeader P, TItemsCompareFunction CompareFunction);
	void __fastcall RollbackItem(PItemHeader Item);
	
protected:
	System::UnicodeString FIndexFieldNames;
	int FRowsFetched;
	bool FOrderSaved;
	TItemHeader *FirstItem;
	TItemHeader *LastItem;
	TItemHeader *CurrentItem;
	TBlockManager* BlockMan;
	virtual void __fastcall InitItem(PItemHeader Item);
	PItemHeader __fastcall InsertItem();
	PItemHeader __fastcall AppendItem();
	virtual void __fastcall DeleteItem(PItemHeader Item);
	void __fastcall RevertItem(PItemHeader Item);
	virtual void __fastcall InitData();
	virtual void __fastcall FreeData();
	void __fastcall ReorderItems(PItemHeader Item, TReorderOption ReorderOption);
	virtual bool __fastcall GetEOF();
	virtual bool __fastcall GetBOF();
	virtual int __fastcall GetRecordNo();
	virtual void __fastcall SetRecordNo(int Value);
	void __fastcall InitFetchedItems(void * FetchedItem, bool NoCountData, bool FetchBack);
	virtual void __fastcall CheckIndexFields();
	virtual void __fastcall SetSortDefaults(TSortColumn* SortColumn);
	void __fastcall AddCacheItem(TCacheItem* CacheItem);
	void __fastcall RemoveItemFromCache(PItemHeader Item);
	void __fastcall FreeCachedItem(TCacheItem* CachedItem);
	virtual bool __fastcall GetUpdatesPending();
	virtual void __fastcall SetFilterItemTypes(const TItemTypes Value);
	virtual bool __fastcall IsSupportedDataType(System::Word DataType);
	virtual bool __fastcall IsSpecificType(TFieldDesc* const Field, System::Word &DataType, System::UnicodeString &DataTypeName, int &Len, int &Scale);
	
public:
	__fastcall TMemData();
	__fastcall virtual ~TMemData();
	virtual void __fastcall Reopen();
	virtual bool __fastcall Fetch(bool FetchBack = false);
	virtual void __fastcall InitFields();
	virtual void __fastcall ClearFields();
	virtual void __fastcall GetRecord(void * RecBuf);
	virtual void __fastcall GetNextRecord(void * RecBuf);
	virtual void __fastcall GetPriorRecord(void * RecBuf);
	virtual void __fastcall PutRecord(void * RecBuf);
	virtual void __fastcall AppendRecord(void * RecBuf);
	virtual void __fastcall InsertRecord(void * RecBuf);
	virtual void __fastcall UpdateRecord(void * RecBuf);
	virtual void __fastcall DeleteRecord();
	void __fastcall AddRecord(void * RecBuf);
	void __fastcall RemoveRecord();
	void __fastcall RefreshRecord(void * RecBuf);
	virtual void __fastcall PostRecord(void * RecBuf);
	bool __fastcall OmitRecord(PItemHeader Item);
	void __fastcall UpdateCachedBuffer(PItemHeader FItem, PItemHeader LItem);
	PItemHeader __fastcall GetFirstItem();
	PItemHeader __fastcall GetLastItem();
	PItemHeader __fastcall GetCurrentItem();
	virtual void __fastcall SetToBegin();
	virtual void __fastcall SetToEnd();
	bool __fastcall SetToItem(PItemHeader Item);
	void __fastcall PrepareRecNoCache(/* out */ int &Count);
	int __fastcall GetRefreshIteration();
	virtual void __fastcall GetBookmark(PRecBookmark Bookmark);
	virtual void __fastcall SetToBookmark(PRecBookmark Bookmark);
	virtual bool __fastcall BookmarkValid(PRecBookmark Bookmark);
	virtual int __fastcall CompareBookmarks(PRecBookmark Bookmark1, PRecBookmark Bookmark2);
	virtual TItemStatus __fastcall GetUpdateStatus();
	virtual TUpdateRecAction __fastcall GetUpdateResult();
	virtual bool __fastcall HasUpdatedOrDeletedRecords();
	virtual void __fastcall SetCacheRecBuf(void * NewBuf, void * OldBuf);
	virtual void __fastcall ApplyUpdates(const TUpdateRecKinds UpdateRecKinds);
	virtual void __fastcall CommitUpdates();
	virtual void __fastcall CancelUpdates();
	virtual void __fastcall RestoreUpdates();
	virtual void __fastcall RevertRecord();
	virtual void __fastcall GetOldRecord(void * RecBuf);
	virtual void __fastcall FilterUpdated();
	void __fastcall ClearItemsOmittedStatus();
	virtual void __fastcall SetIndexFieldNames(const System::UnicodeString Value);
	virtual void __fastcall SortItems();
	__property TSortColumn* IndexFields[int Index] = {read=GetIndexField};
	__property int IndexFieldCount = {read=GetIndexFieldCount, nodefault};
	__property int RowsFetched = {read=FRowsFetched, nodefault};
};


typedef TPieceHeader *PPieceHeader;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TPieceHeader
{
public:
	int Blob;
	int Size;
	int Used;
	TPieceHeader *Prev;
	TPieceHeader *Next;
	System::Word Test;
};
#pragma pack(pop)


#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRBlobData : public TSharedObject
{
	typedef TSharedObject inherited;
	
private:
	TPieceHeader *FFirstPiece;
	int FPieceSize;
	int FLargePieceSize;
	int __fastcall GetLargePieceSize();
	
protected:
	bool FIsNull;
	__property PPieceHeader FirstPiece = {read=FFirstPiece};
	__property int PieceSize = {read=FPieceSize, nodefault};
	__property int LargePieceSize = {read=GetLargePieceSize, write=FLargePieceSize, nodefault};
	
public:
	__fastcall TCRBlobData();
	__fastcall virtual ~TCRBlobData();
	__classmethod void __fastcall AllocPiece(/* out */ PPieceHeader &Piece, int Size);
	void __fastcall ReallocPiece(PPieceHeader &Piece, int Size);
	void __fastcall FreePiece(PPieceHeader Piece);
	void __fastcall AppendPiece(PPieceHeader Piece);
	void __fastcall DeletePiece(PPieceHeader Piece);
	void __fastcall CompressPiece(PPieceHeader &Piece);
	unsigned __fastcall GetDataSize();
	virtual unsigned __fastcall GetSize();
	virtual void __fastcall SetSize(unsigned Value);
	virtual void __fastcall Clear();
	virtual void __fastcall Truncate(unsigned NewSize);
	bool __fastcall IsEmpty();
	bool __fastcall IsNull();
	virtual unsigned __fastcall Read(unsigned Position, unsigned Count, void * Dest);
	virtual void __fastcall Write(unsigned Position, unsigned Count, void * Source);
	void __fastcall Compress();
	void __fastcall Defrag(int MinPieceSize = 0x0);
	void __fastcall CopyTo(TCRBlobData* Dest);
	void __fastcall AddCRUnicode();
	void __fastcall RemoveCRUnicode();
	void __fastcall AddCRString();
	void __fastcall RemoveCRString();
	unsigned __fastcall TranslatePositionToAnsi(unsigned Position);
	unsigned __fastcall TranslatePositionToUni(unsigned Position);
	unsigned __fastcall GetSizeAnsi();
	unsigned __fastcall GetSizeUni();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TBlob : public TSharedObject
{
	typedef TSharedObject inherited;
	
protected:
	TCRBlobData* FData;
	bool FIsUnicode;
	bool FNeedRollback;
	TCRBlobData* FRollback;
	TCRBlobData* FStoredData;
	System::UnicodeString __fastcall GetAsString();
	void __fastcall SetAsString(const System::UnicodeString Value);
	System::AnsiString __fastcall GetAsAnsiString();
	void __fastcall SetAsAnsiString(const System::AnsiString Value);
	System::WideString __fastcall GetAsWideString();
	void __fastcall SetAsWideString(const System::WideString Value);
	System::DynamicArray<System::Byte> __fastcall GetAsBytes();
	void __fastcall SetAsBytes(const System::DynamicArray<System::Byte> Value);
	int __fastcall GetPieceSize();
	void __fastcall SetPieceSize(int Value);
	int __fastcall GetLargePieceSize();
	void __fastcall SetLargePieceSize(int Value);
	bool __fastcall GetUseRollback();
	void __fastcall SetUseRollback(bool Value);
	virtual TBlob* __fastcall CreateClone();
	virtual TCRBlobData* __fastcall CreateBlobData();
	void __fastcall CloneBlobData(TBlob* SourceBlob);
	HIDESBASE void __fastcall CheckValid();
	void __fastcall CheckCached();
	virtual void __fastcall CheckValue();
	virtual void __fastcall SaveToRollback();
	virtual unsigned __fastcall GetSize();
	virtual void __fastcall SetSize(unsigned Value);
	virtual void __fastcall SetIsUnicode(bool Value);
	unsigned __fastcall TranslatePositionToAnsi(unsigned Position);
	unsigned __fastcall TranslatePositionToUni(unsigned Position);
	virtual unsigned __fastcall GetSizeAnsi();
	virtual unsigned __fastcall GetSizeUni();
	
public:
	System::Byte Test;
	__fastcall TBlob(bool IsUnicode);
	__fastcall virtual ~TBlob();
	TBlob* __fastcall Clone(bool FromRollback = false, bool CloneData = true);
	virtual void __fastcall FreeBlob();
	__classmethod void __fastcall AllocPiece(/* out */ PPieceHeader &Piece, int Size);
	void __fastcall ReallocPiece(PPieceHeader &Piece, int Size);
	void __fastcall FreePiece(PPieceHeader Piece);
	void __fastcall AppendPiece(PPieceHeader Piece);
	void __fastcall DeletePiece(PPieceHeader Piece);
	void __fastcall CompressPiece(PPieceHeader &Piece);
	PPieceHeader __fastcall FirstPiece();
	virtual unsigned __fastcall Read(unsigned Position, unsigned Count, void * Dest);
	virtual void __fastcall Write(unsigned Position, unsigned Count, void * Source);
	virtual void __fastcall Clear();
	virtual void __fastcall Truncate(unsigned NewSize);
	bool __fastcall IsEmpty();
	bool __fastcall IsNull();
	void __fastcall Compress();
	virtual void __fastcall Defrag(unsigned MinPieceSize = (unsigned)(0x0));
	void __fastcall AddCR();
	void __fastcall RemoveCR();
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	void __fastcall LoadFromFile(const System::UnicodeString FileName);
	void __fastcall SaveToFile(const System::UnicodeString FileName);
	void __fastcall Assign(TBlob* Source);
	TCRBlobData* __fastcall GetData();
	void __fastcall SetData(TCRBlobData* Value);
	void __fastcall EnableRollback();
	virtual void __fastcall Commit();
	virtual void __fastcall Cancel();
	bool __fastcall CanRollback();
	__property unsigned Size = {read=GetSize, write=SetSize, nodefault};
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	__property System::AnsiString AsAnsiString = {read=GetAsAnsiString, write=SetAsAnsiString};
	__property System::WideString AsWideString = {read=GetAsWideString, write=SetAsWideString};
	__property System::DynamicArray<System::Byte> AsBytes = {read=GetAsBytes, write=SetAsBytes};
	__property bool IsUnicode = {read=FIsUnicode, write=SetIsUnicode, nodefault};
	__property int PieceSize = {read=GetPieceSize, write=SetPieceSize, nodefault};
	__property int LargePieceSize = {read=GetLargePieceSize, write=SetLargePieceSize, nodefault};
	__property bool RollbackEnabled = {read=FNeedRollback, write=FNeedRollback, nodefault};
	__property bool UseRollback = {read=GetUseRollback, write=SetUseRollback, nodefault};
};

#pragma pack(pop)

enum DECLSPEC_DENUM TCompressBlobMode : unsigned char { cbNone, cbClient, cbServer, cbClientServer };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCompressedBlobData : public TCRBlobData
{
	typedef TCRBlobData inherited;
	
protected:
	bool __fastcall CompressFrom(void * source, const int sourceLen);
	void __fastcall UncompressTo(void * dest, int &destlen);
	
public:
	bool __fastcall IsCompressed();
	bool __fastcall SetCompressed(bool Value);
	unsigned __fastcall UnCompressedSize();
	virtual unsigned __fastcall GetSize();
	virtual void __fastcall SetSize(unsigned Value);
	unsigned __fastcall GetCompressedSize();
	virtual unsigned __fastcall Read(unsigned Position, unsigned Count, void * Dest);
	virtual void __fastcall Write(unsigned Position, unsigned Count, void * Source);
	virtual void __fastcall Truncate(unsigned NewSize);
public:
	/* TCRBlobData.Create */ inline __fastcall TCompressedBlobData() : TCRBlobData() { }
	/* TCRBlobData.Destroy */ inline __fastcall virtual ~TCompressedBlobData() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCompressedBlob : public TBlob
{
	typedef TBlob inherited;
	
protected:
	bool __fastcall GetCompressed();
	void __fastcall SetCompressed(bool Value);
	unsigned __fastcall GetCompressedSize();
	virtual TCRBlobData* __fastcall CreateBlobData();
	
public:
	__property bool Compressed = {read=GetCompressed, write=SetCompressed, nodefault};
	__property unsigned CompressedSize = {read=GetCompressedSize, nodefault};
public:
	/* TBlob.Create */ inline __fastcall TCompressedBlob(bool IsUnicode) : TBlob(IsUnicode) { }
	/* TBlob.Destroy */ inline __fastcall virtual ~TCompressedBlob() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TVariantObject : public TSharedObject
{
	typedef TSharedObject inherited;
	
private:
	System::Variant FValue;
	
public:
	__property System::Variant Value = {read=FValue, write=FValue};
public:
	/* TSharedObject.Create */ inline __fastcall TVariantObject() : TSharedObject() { }
	/* TSharedObject.Destroy */ inline __fastcall virtual ~TVariantObject() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte btSign = System::Byte(0xdd);
static const System::Byte flUsed = System::Byte(0xee);
static const System::Byte flFree = System::Byte(0xdd);
static const System::Int8 FlatBufferLimit = System::Int8(0x20);
static const System::Int8 dtUnknown = System::Int8(0x0);
static const System::Int8 dtString = System::Int8(0x1);
static const System::Int8 dtExtString = System::Int8(0x2);
static const System::Int8 dtWideString = System::Int8(0x3);
static const System::Int8 dtExtWideString = System::Int8(0x4);
static const System::Int8 dtFixedChar = System::Int8(0x5);
static const System::Int8 dtFixedWideChar = System::Int8(0x6);
static const System::Int8 dtInt8 = System::Int8(0x7);
static const System::Int8 dtInt16 = System::Int8(0x8);
static const System::Int8 dtSmallint = System::Int8(0x8);
static const System::Int8 dtInt32 = System::Int8(0x9);
static const System::Int8 dtInteger = System::Int8(0x9);
static const System::Int8 dtInt64 = System::Int8(0xa);
static const System::Int8 dtLargeint = System::Int8(0xa);
static const System::Int8 dtUInt8 = System::Int8(0xb);
static const System::Int8 dtByte = System::Int8(0xb);
static const System::Int8 dtUInt16 = System::Int8(0xc);
static const System::Int8 dtWord = System::Int8(0xc);
static const System::Int8 dtUInt32 = System::Int8(0xd);
static const System::Int8 dtLongWord = System::Int8(0xd);
static const System::Int8 dtUInt64 = System::Int8(0xe);
static const System::Int8 dtSingle = System::Int8(0xf);
static const System::Int8 dtFloat = System::Int8(0x10);
static const System::Int8 dtExtended = System::Int8(0x11);
static const System::Int8 dtCurrency = System::Int8(0x12);
static const System::Int8 dtBCD = System::Int8(0x13);
static const System::Int8 dtFMTBCD = System::Int8(0x14);
static const System::Int8 dtDate = System::Int8(0x15);
static const System::Int8 dtTime = System::Int8(0x16);
static const System::Int8 dtDateTime = System::Int8(0x17);
static const System::Int8 dtSQLTimeStamp = System::Int8(0x18);
static const System::Int8 dtSQLTimeStampOffset = System::Int8(0x19);
static const System::Int8 dtBoolean = System::Int8(0x1a);
static const System::Int8 dtBytes = System::Int8(0x1b);
static const System::Int8 dtVarBytes = System::Int8(0x1c);
static const System::Int8 dtExtBytes = System::Int8(0x1d);
static const System::Int8 dtExtVarBytes = System::Int8(0x1e);
static const System::Int8 dtBlob = System::Int8(0x1f);
static const System::Int8 dtMemo = System::Int8(0x20);
static const System::Int8 dtWideMemo = System::Int8(0x21);
static const System::Int8 dtVariant = System::Int8(0x22);
static const System::Int8 dtObject = System::Int8(0x23);
static const System::Int8 dtReference = System::Int8(0x24);
static const System::Int8 dtArray = System::Int8(0x25);
static const System::Int8 dtTable = System::Int8(0x26);
static const System::Int8 dtGuid = System::Int8(0x27);
static const System::Int8 dtCursor = System::Int8(0x28);
static const System::Int8 dtXML = System::Int8(0x29);
static const System::Word BlockSize = System::Word(0x4000);
static const System::Word SmallSize = System::Word(0x7d0);
static const System::Int8 Align = System::Int8(0x8);
static const System::Int8 RefNull = System::Int8(0x65);
static const int SizeOfTBcd = int(0x22);
extern DELPHI_PACKAGE int DefaultPieceSize;
extern DELPHI_PACKAGE int DefaultLargePieceSize;
static const System::Int8 CCompressBlobHeaderGuidSize = System::Int8(0x10);
static const System::Int8 CCompressBlobHeaderSize = System::Int8(0x14);
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 16> CCompressBlobHeaderGuid;
static const System::Int8 varDecimal = System::Int8(0xe);
static const System::Int8 varLongWord = System::Int8(0x13);
extern DELPHI_PACKAGE void __fastcall (*StartWaitProc)(void);
extern DELPHI_PACKAGE void __fastcall (*StopWaitProc)(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall (*ApplicationTitleProc)(void);
extern DELPHI_PACKAGE float SingleValueDelta;
extern DELPHI_PACKAGE double DoubleValueDelta;
extern DELPHI_PACKAGE double DateTimeValueDelta;
extern DELPHI_PACKAGE int MaxArrayItem;
extern DELPHI_PACKAGE System::UnicodeString UniqueFieldIndexSeparator;
extern DELPHI_PACKAGE bool DefaultExpressionOldBehavior;
extern DELPHI_PACKAGE void __fastcall DataError(System::UnicodeString Msg);
extern DELPHI_PACKAGE void __fastcall StartWait(void);
extern DELPHI_PACKAGE void __fastcall StopWait(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ApplicationTitle(void);
extern DELPHI_PACKAGE int __fastcall AddCRString(void * Source, int SourceLen, void * Dest, int DestLen);
extern DELPHI_PACKAGE int __fastcall RemoveCRString(void * Source, int SourceLen, void * Dest, int DestLen);
extern DELPHI_PACKAGE int __fastcall AddCRUnicode(void * Source, int SourceLen, void * Dest, int DestLen);
extern DELPHI_PACKAGE int __fastcall RemoveCRUnicode(void * Source, int SourceLen, void * Dest, int DestLen);
extern DELPHI_PACKAGE int __fastcall AddCRBigEndian(void * Source, int SourceLen, void * Dest, int DestLen);
extern DELPHI_PACKAGE int __fastcall RemoveCRBigEndian(void * Source, int SourceLen, void * Dest, int DestLen);
extern DELPHI_PACKAGE PPieceHeader __fastcall NextPiece(PPieceHeader Piece);
extern DELPHI_PACKAGE void * __fastcall PieceData(PPieceHeader Piece);
extern DELPHI_PACKAGE void * __fastcall PieceUsedPtr(PPieceHeader Piece);
}	/* namespace Memdata */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MEMDATA)
using namespace Memdata;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MemdataHPP
