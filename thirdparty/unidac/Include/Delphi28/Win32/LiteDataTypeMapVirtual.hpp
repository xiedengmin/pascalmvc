// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteDataTypeMapVirtual.pas' rev: 35.00 (Windows)

#ifndef LitedatatypemapvirtualHPP
#define LitedatatypemapvirtualHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <Data.SqlTimSt.hpp>
#include <CRTypes.hpp>
#include <CRDataTypeMap.hpp>
#include <CRFunctions.hpp>
#include <CRTimeStamp.hpp>
#include <MemData.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Litedatatypemapvirtual
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TLiteConverterManager;
class DELPHICLASS TLiteMapRules;
class DELPHICLASS TLiteDataConverters;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TLiteConverterManager : public Crdatatypemap::TConverterManager
{
	typedef Crdatatypemap::TConverterManager inherited;
	
public:
	__fastcall TLiteConverterManager();
	__classmethod virtual System::Word __fastcall GetDBProvider();
	__classmethod virtual System::Word __fastcall GetDBType(const System::UnicodeString SQLTypeName)/* overload */;
	__classmethod virtual System::Word __fastcall GetDBType(int SQLType)/* overload */;
public:
	/* TConverterManager.Destroy */ inline __fastcall virtual ~TLiteConverterManager() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLiteMapRules : public Crdatatypemap::TCRMapRules
{
	typedef Crdatatypemap::TCRMapRules inherited;
	
public:
	__classmethod virtual Crdatatypemap::TConverterManager* __fastcall GetConverterManager();
public:
	/* TCRMapRules.Create */ inline __fastcall virtual TLiteMapRules() : Crdatatypemap::TCRMapRules() { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TLiteMapRules() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLiteDataConverters : public Crdatatypemap::TDataConverters
{
	typedef Crdatatypemap::TDataConverters inherited;
	
public:
	__classmethod Memdata::TConvertStatus __fastcall IntToDateTime(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToDateTime(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall RealToDateTime(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToInt(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToInt64(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToReal(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToInt8(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToUInt8(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToInt16(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToUInt16(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToInt32(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToUInt32(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToInt64(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToSingle(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToFloat(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToExtended(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToDate(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToTime(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToDateTime(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToSQLTimeStamp(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToBoolean(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToMemo(Memdata::TConvertInfo &AConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BooleanToMemo(Memdata::TConvertInfo &AConvertInfo);
public:
	/* TObject.Create */ inline __fastcall TLiteDataConverters() : Crdatatypemap::TDataConverters() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TLiteDataConverters() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word liteBase = System::Word(0x258);
static const System::Word liteInteger = System::Word(0x259);
static const System::Word liteReal = System::Word(0x25a);
static const System::Word liteText = System::Word(0x25b);
static const System::Word liteBlob = System::Word(0x25c);
static const System::Word liteNull = System::Word(0x25d);
static const System::Word liteTinyInt = System::Word(0x25e);
static const System::Word liteInt2 = System::Word(0x25f);
static const System::Word liteSmallInt = System::Word(0x260);
static const System::Word liteInt = System::Word(0x261);
static const System::Word liteMediumInt = System::Word(0x262);
static const System::Word liteBigInt = System::Word(0x263);
static const System::Word liteUBigInt = System::Word(0x264);
static const System::Word liteInt8 = System::Word(0x265);
static const System::Word liteInt64 = System::Word(0x266);
static const System::Word liteChar = System::Word(0x267);
static const System::Word liteVarChar = System::Word(0x268);
static const System::Word liteClob = System::Word(0x269);
static const System::Word liteFloat = System::Word(0x26a);
static const System::Word liteDouble = System::Word(0x26b);
static const System::Word liteNumeric = System::Word(0x26c);
static const System::Word liteDecimal = System::Word(0x26d);
static const System::Word liteNumber = System::Word(0x26e);
static const System::Word liteMoney = System::Word(0x26f);
static const System::Word liteBool = System::Word(0x270);
static const System::Word liteBinary = System::Word(0x271);
static const System::Word liteVarBinary = System::Word(0x272);
static const System::Word liteDate = System::Word(0x273);
static const System::Word liteTime = System::Word(0x274);
static const System::Word liteDateTime = System::Word(0x275);
static const System::Word liteTimestamp = System::Word(0x276);
static const System::Word liteBit = System::Word(0x277);
static const System::Word liteTimestampTZ = System::Word(0x278);
extern DELPHI_PACKAGE bool __fastcall IsLiteUnsignedType(System::Word DBType);
extern DELPHI_PACKAGE void __fastcall InitLiteTypes(void);
}	/* namespace Litedatatypemapvirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITEDATATYPEMAPVIRTUAL)
using namespace Litedatatypemapvirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LitedatatypemapvirtualHPP
