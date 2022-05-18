// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRVirtualData.pas' rev: 35.00 (Windows)

#ifndef CrvirtualdataHPP
#define CrvirtualdataHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <Data.SqlTimSt.hpp>
#include <CRTypes.hpp>
#include <CLRClasses.hpp>
#include <CRFunctions.hpp>
#include <CRAccess.hpp>
#include <CRParser.hpp>
#include <CRTimeStamp.hpp>
#include <MemData.hpp>
#include <CRVirtualConsts.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crvirtualdata
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EVirtualQueryError;
struct TVirtualValue;
struct TVirtualConstraintItem;
struct TVirtualConstraint;
struct TVirtualFieldDesc;
class DELPHICLASS TSpecificTypeDesc;
class DELPHICLASS TVirtualLocalIndex;
class DELPHICLASS TFieldAccessor;
class DELPHICLASS TVirtualSQLInfo;
class DELPHICLASS TVirtualData;
class DELPHICLASS TMemDataFieldAccessor;
class DELPHICLASS TVirtualMemDataIndex;
class DELPHICLASS TVirtualMemData;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EVirtualQueryError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EVirtualQueryError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EVirtualQueryError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EVirtualQueryError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EVirtualQueryError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EVirtualQueryError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EVirtualQueryError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EVirtualQueryError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EVirtualQueryError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EVirtualQueryError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EVirtualQueryError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EVirtualQueryError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EVirtualQueryError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EVirtualQueryError() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TVirtualType : unsigned char { vrNull, vrInteger, vrFloat, vrString, vrAnsiString, vrWideString, vrBlob };

typedef TVirtualValue *PVirtualValue;

struct DECLSPEC_DRECORD TVirtualValue
{
public:
	TVirtualType ValueType;
	System::Variant Value;
	__int64 IntValue;
	double FloatValue;
	System::AnsiString AnsiStrValue;
	System::WideString WideStrValue;
};


typedef System::DynamicArray<TVirtualValue> TVirtualValues;

typedef TVirtualConstraintItem *PVirtualConstraintItem;

struct DECLSPEC_DRECORD TVirtualConstraintItem
{
public:
	int FieldIndex;
	int ArgIndex;
	Memdata::TExpressionType Operation;
	TVirtualValue Value;
	bool SimpleCompare;
	bool NativeCompare;
};


typedef TVirtualConstraint *PVirtualConstraint;

struct DECLSPEC_DRECORD TVirtualConstraint
{
	
private:
	typedef System::DynamicArray<TVirtualConstraintItem> _TVirtualConstraint__1;
	
	
public:
	int LocalIndex;
	int CurrentItem;
	int SortItemIndex;
	_TVirtualConstraint__1 Items;
};


typedef System::DynamicArray<TVirtualConstraint> TVirtualConstraints;

typedef __int64 TVirtualBookmark;

typedef TVirtualFieldDesc *PVirtualFieldDesc;

struct DECLSPEC_DRECORD TVirtualFieldDesc
{
public:
	System::UnicodeString Name;
	System::Word DBType;
	System::Word DataType;
	System::Word Length;
	System::Word Scale;
	bool IsKey;
	bool IsAutoIncrement;
	bool Required;
	bool ReadOnly;
	bool Hidden;
	System::Variant Default;
	System::TObject* FieldObject;
	int ActualIndex;
};


typedef System::DynamicArray<TVirtualFieldDesc> TVirtualFieldDescs;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpecificTypeDesc : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FDataTypeName;
	System::Word FDataType;
	int FLength;
	int FScale;
	
public:
	__fastcall TSpecificTypeDesc(System::UnicodeString ADataTypeName, System::Word ADataType, int ALength, int AScale);
	__property System::UnicodeString DataTypeName = {read=FDataTypeName};
	__property System::Word DataType = {read=FDataType, nodefault};
	__property int Length = {read=FLength, nodefault};
	__property int Scale = {read=FScale, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSpecificTypeDesc() { }
	
};

#pragma pack(pop)

typedef System::Classes::TStringList TSpecificTypes;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualLocalIndex : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	void * operator[](const int Index) { return this->Items[Index]; }
	
private:
	void * __fastcall GetItems(const int Index);
	
protected:
	TVirtualData* FData;
	System::Classes::TList* FItems;
	int FStartIndex;
	bool FNull1;
	bool FNull2;
	virtual void * __fastcall GetFieldPtr(const int FieldIndex) = 0 ;
	virtual void * __fastcall GetBuffer(const void * Item) = 0 ;
	virtual int __fastcall InternalCompare(const void * FieldPtr, void * Item1, void * Item2) = 0 ;
	virtual bool __fastcall IsIntegerField(const void * FieldPtr) = 0 ;
	virtual bool __fastcall IsDateTimeField(const void * FieldPtr) = 0 ;
	virtual bool __fastcall IsFloatField(const void * FieldPtr) = 0 ;
	virtual bool __fastcall IsAnsiStringField(const void * FieldPtr) = 0 ;
	virtual bool __fastcall IsWideStringField(const void * FieldPtr) = 0 ;
	
public:
	__fastcall TVirtualLocalIndex(TVirtualData* const Data, int Capacity);
	__fastcall virtual ~TVirtualLocalIndex();
	virtual void __fastcall Sort(const int FieldIndex);
	virtual int __fastcall GetItem(const PVirtualConstraint Constraint);
	int __fastcall GetNextItem(const int From, const PVirtualConstraint Constraint);
	void __fastcall DeleteItem(const void * Item);
	void __fastcall Add(const void * Item);
	__property void * Items[const int Index] = {read=GetItems/*, default*/};
};

#pragma pack(pop)

typedef System::TMetaClass* TVirtualLocalIndexClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFieldAccessor : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod virtual __int64 __fastcall AsInteger(const void * Field, const void * Buffer);
	__classmethod virtual double __fastcall AsFloat(const void * Field, const void * Buffer);
	__classmethod virtual System::AnsiString __fastcall AsAnsiString(const void * Field, const void * Buffer);
	__classmethod virtual System::WideString __fastcall AsWideString(const void * Field, const void * Buffer);
public:
	/* TObject.Create */ inline __fastcall TFieldAccessor() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TFieldAccessor() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TFieldAccessorClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualSQLInfo : public Craccess::TSQLInfo
{
	typedef Craccess::TSQLInfo inherited;
	
private:
	Crparser::TLexemList* FLexems;
	
public:
	__fastcall TVirtualSQLInfo(Crparser::TSQLParserClass ParserClass, Crparser::TLexemList* const Lexems);
	virtual System::WideChar __fastcall LeftQuote();
	virtual System::WideChar __fastcall RightQuote();
	virtual bool __fastcall IsQuoted(const System::UnicodeString Value);
	HIDESBASE System::UnicodeString __fastcall NormalizeName(const System::UnicodeString Name)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualSQLInfo() { }
	
	/* Hoisted overloads: */
	
public:
	inline System::UnicodeString __fastcall  NormalizeName(const System::UnicodeString Value, bool QuoteNames = false, bool UnQuoteNames = false){ return Craccess::TSQLInfo::NormalizeName(Value, QuoteNames, UnQuoteNames); }
	inline System::UnicodeString __fastcall  NormalizeName(const System::UnicodeString Value, const System::WideChar LeftQ, const System::WideChar RightQ, bool QuoteNames = false, bool UnQuoteNames = false){ return Craccess::TSQLInfo::NormalizeName(Value, LeftQ, RightQ, QuoteNames, UnQuoteNames); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualData : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool __fastcall CompareFieldValue(const PVirtualConstraintItem Constraint);
	
protected:
	System::Classes::TStringList* FLocalIndexes;
	TVirtualSQLInfo* FSQLInfo;
	bool FCanUseLocalindex;
	bool FPrepared;
	bool FAutoClose;
	bool FAutoUnwind;
	__classmethod virtual TVirtualLocalIndexClass __fastcall GetLocalIndexClass();
	virtual TFieldAccessorClass __fastcall GetFieldAccessorClass() = 0 ;
	virtual void __fastcall CreateSQLInfo();
	bool __fastcall ValueToVariant(const int FieldIndex, const TVirtualValue &VirtualValue, /* out */ System::Variant &Value);
	virtual bool __fastcall OmitRecord(const PVirtualConstraint Filter, const int ExcludeIndex = 0xffffffff);
	virtual bool __fastcall GetNextRecord(const PVirtualConstraint Filter);
	virtual void __fastcall InternalOpen();
	virtual void __fastcall InternalDescribeFields(System::Classes::TStringList* const SpecificTypes);
	virtual void __fastcall InternalGetCurrentRecord();
	virtual void __fastcall InternalNext() = 0 ;
	virtual bool __fastcall InternalEof(const PVirtualConstraint Filter) = 0 ;
	virtual bool __fastcall IsSupportedDataType(System::Word DataType);
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const System::Variant &Value) = 0 /* overload */;
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const __int64 Value) = 0 /* overload */;
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const double Value) = 0 /* overload */;
	virtual System::UnicodeString __fastcall GetFieldDataTypeName(const PVirtualFieldDesc Field);
	virtual System::UnicodeString __fastcall GetFieldDefinition(const PVirtualFieldDesc Field);
	
public:
	TVirtualFieldDescs FFields;
	__fastcall TVirtualData();
	__fastcall virtual ~TVirtualData();
	virtual bool __fastcall Prepared();
	virtual bool __fastcall Active() = 0 ;
	System::UnicodeString __fastcall GetCreateSQL();
	virtual void __fastcall Prepare();
	void __fastcall DescribeFields(System::Classes::TStringList* const SpecificTypes);
	virtual void __fastcall PrepareConstraints(const PVirtualConstraint Filter, int &Cost);
	virtual void __fastcall PrepareFilter(const PVirtualConstraint Filter);
	void __fastcall CheckActive();
	virtual __int64 __fastcall Open(const PVirtualConstraint Filter);
	virtual void __fastcall Close();
	virtual void __fastcall Reset();
	virtual __int64 __fastcall Next(const __int64 Bookmark, const PVirtualConstraint Filter);
	bool __fastcall Eof(const __int64 Bookmark);
	virtual __int64 __fastcall GetBookmark() = 0 ;
	virtual void __fastcall GotoBookmark(const __int64 Bookmark) = 0 ;
	virtual int __fastcall GetRecordCount() = 0 ;
	virtual System::Word __fastcall GetFieldType(int FieldIndex);
	virtual System::Word __fastcall GetFieldScale(int FieldIndex);
	virtual bool __fastcall GetFieldNull(int FieldIndex) = 0 ;
	virtual System::Variant __fastcall GetFieldValue(int FieldIndex) = 0 /* overload */;
	virtual System::Variant __fastcall GetFieldValue(int FieldIndex, bool &FieldNull)/* overload */;
	virtual void __fastcall FreeFieldValue(const System::Variant &Value);
	virtual void __fastcall DisableControls(bool SaveRecNo);
	virtual void __fastcall EnableControls();
	virtual void __fastcall EditRecord(const TVirtualValues Values) = 0 ;
	virtual void __fastcall InsertRecord(const TVirtualValues Values) = 0 ;
	virtual void __fastcall DeleteRecord() = 0 ;
	virtual bool __fastcall InTransaction() = 0 ;
	virtual void __fastcall StartTransaction() = 0 ;
	virtual void __fastcall Commit() = 0 ;
	virtual void __fastcall Rollback() = 0 ;
	virtual bool __fastcall IsSimpleFieldType(const int FieldIndex) = 0 ;
	virtual int __fastcall CompareInteger(const void * Field, const void * Buffer, const __int64 Value);
	virtual int __fastcall CompareFloat(const void * Field, const void * Buffer, const double Value);
	virtual int __fastcall CompareAnsiString(const void * Field, const void * Buffer, const System::AnsiString Value);
	virtual int __fastcall CompareWideString(const void * Field, const void * Buffer, const System::WideString Value);
	virtual int __fastcall CompareVariant(const void * Field, const void * Buffer, const System::Variant &Value);
	virtual int __fastcall GetLocalIndex(const PVirtualConstraint Constraint);
	virtual void __fastcall DropLocalIndex(const System::UnicodeString FieldName);
	virtual void __fastcall DeleteIndexItem(const __int64 Item);
	virtual void __fastcall ClearLocalIndexes();
	__property bool CanUseLocalIndex = {read=FCanUseLocalindex, nodefault};
	__property TVirtualFieldDescs Fields = {read=FFields, write=FFields};
	__property bool AutoClose = {read=FAutoClose, nodefault};
	__property bool AutoUnwind = {read=FAutoUnwind, nodefault};
	__property TVirtualSQLInfo* SQLInfo = {read=FSQLInfo};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMemDataFieldAccessor : public TFieldAccessor
{
	typedef TFieldAccessor inherited;
	
public:
	__classmethod virtual __int64 __fastcall AsInteger(const void * Field, const void * Buffer);
	__classmethod virtual double __fastcall AsFloat(const void * Field, const void * Buffer);
	__classmethod virtual System::AnsiString __fastcall AsAnsiString(const void * Field, const void * Buffer);
	__classmethod virtual System::WideString __fastcall AsWideString(const void * Field, const void * Buffer);
public:
	/* TObject.Create */ inline __fastcall TMemDataFieldAccessor() : TFieldAccessor() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMemDataFieldAccessor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualMemDataIndex : public TVirtualLocalIndex
{
	typedef TVirtualLocalIndex inherited;
	
protected:
	virtual void * __fastcall GetFieldPtr(const int FieldIndex);
	virtual void * __fastcall GetBuffer(const void * Item);
	virtual int __fastcall InternalCompare(const void * FieldPtr, void * Item1, void * Item2);
	virtual bool __fastcall IsIntegerField(const void * FieldPtr);
	virtual bool __fastcall IsDateTimeField(const void * FieldPtr);
	virtual bool __fastcall IsFloatField(const void * FieldPtr);
	virtual bool __fastcall IsAnsiStringField(const void * FieldPtr);
	virtual bool __fastcall IsWideStringField(const void * FieldPtr);
	
public:
	virtual void __fastcall Sort(const int FieldIndex);
public:
	/* TVirtualLocalIndex.Create */ inline __fastcall TVirtualMemDataIndex(TVirtualData* const Data, int Capacity) : TVirtualLocalIndex(Data, Capacity) { }
	/* TVirtualLocalIndex.Destroy */ inline __fastcall virtual ~TVirtualMemDataIndex() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualMemData : public TVirtualData
{
	typedef TVirtualData inherited;
	
private:
	Memdata::TRecBookmark FBookmark;
	Memdata::TRecBookmark FOpenBookmark;
	bool FExplicitTransaction;
	bool FUseFilter;
	Memdata::TExpressionNode* FFieldNode;
	Memdata::TExpressionNode* FOperationNode;
	Memdata::TExpressionNode* FValueNode;
	
protected:
	Memdata::TMemData* FMemData;
	void *FRecordBuffer;
	bool FBufferAllocated;
	__classmethod virtual TVirtualLocalIndexClass __fastcall GetLocalIndexClass();
	virtual TFieldAccessorClass __fastcall GetFieldAccessorClass();
	virtual bool __fastcall OmitRecord(const PVirtualConstraint Filter, const int ExcludeIndex = 0xffffffff);
	virtual bool __fastcall GetNextRecord(const PVirtualConstraint Filter);
	virtual void __fastcall InternalOpen();
	virtual __int64 __fastcall InternalOpenNoIndex(const PVirtualConstraint Filter);
	virtual void __fastcall InternalDescribeFields(System::Classes::TStringList* const SpecificTypes);
	virtual void __fastcall InternalGetCurrentRecord();
	virtual void __fastcall InternalNext();
	virtual bool __fastcall InternalEof(const PVirtualConstraint Filter);
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const System::Variant &Value)/* overload */;
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const __int64 Value)/* overload */;
	virtual bool __fastcall InternalCompareFieldValue(int FieldIndex, Memdata::TExpressionType Operation, const double Value)/* overload */;
	virtual bool __fastcall IsSupportedDataType(System::Word DataType);
	virtual System::UnicodeString __fastcall GetFieldDefinition(const PVirtualFieldDesc Field);
	virtual void __fastcall InternalAllocBuffer();
	virtual void __fastcall InternalFreeBuffer();
	void __fastcall InternalPutRecord(const TVirtualValues Values, void * Buffer);
	
public:
	__fastcall TVirtualMemData(Memdata::TMemData* const MemData)/* overload */;
	__fastcall virtual ~TVirtualMemData();
	virtual bool __fastcall Active();
	virtual void __fastcall Prepare();
	virtual __int64 __fastcall Open(const PVirtualConstraint Filter);
	virtual void __fastcall Close();
	virtual void __fastcall Reset();
	virtual __int64 __fastcall GetBookmark();
	virtual void __fastcall GotoBookmark(const __int64 Bookmark);
	virtual int __fastcall GetRecordCount();
	virtual System::Word __fastcall GetFieldType(int FieldIndex);
	virtual bool __fastcall GetFieldNull(int FieldIndex);
	virtual System::Variant __fastcall GetFieldValue(int FieldIndex)/* overload */;
	virtual void __fastcall EditRecord(const TVirtualValues Values);
	virtual void __fastcall InsertRecord(const TVirtualValues Values);
	virtual void __fastcall DeleteRecord();
	virtual bool __fastcall InTransaction();
	virtual void __fastcall StartTransaction();
	virtual void __fastcall Commit();
	virtual void __fastcall Rollback();
	virtual int __fastcall CompareVariant(const void * Field, const void * Buffer, const System::Variant &Value);
	virtual bool __fastcall IsSimpleFieldType(const int FieldIndex);
	virtual int __fastcall GetLocalIndex(const PVirtualConstraint Constraint);
	/* Hoisted overloads: */
	
public:
	inline System::Variant __fastcall  GetFieldValue(int FieldIndex, bool &FieldNull){ return TVirtualData::GetFieldValue(FieldIndex, FieldNull); }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::TDateTime __fastcall ConvertStringToDateTime(System::Word DataType, System::UnicodeString Value);
}	/* namespace Crvirtualdata */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRVIRTUALDATA)
using namespace Crvirtualdata;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrvirtualdataHPP
