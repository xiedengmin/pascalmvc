// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualDataTypeMap.pas' rev: 35.00 (Windows)

#ifndef VirtualdatatypemapHPP
#define VirtualdatatypemapHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <MemData.hpp>
#include <CRDataTypeMap.hpp>
#include <LiteDataTypeMapVirtual.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualdatatypemap
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVirtualMapRules;
class DELPHICLASS TVirtualConverterManager;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualMapRules : public Crdatatypemap::TCRMapRules
{
	typedef Crdatatypemap::TCRMapRules inherited;
	
public:
	__classmethod virtual Crdatatypemap::TConverterManager* __fastcall GetConverterManager();
public:
	/* TCRMapRules.Create */ inline __fastcall virtual TVirtualMapRules() : Crdatatypemap::TCRMapRules() { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TVirtualMapRules() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualConverterManager : public Litedatatypemapvirtual::TLiteConverterManager
{
	typedef Litedatatypemapvirtual::TLiteConverterManager inherited;
	
public:
	__fastcall TVirtualConverterManager();
	__classmethod virtual System::Word __fastcall GetDBProvider();
	__classmethod virtual System::Word __fastcall GetDBType(const System::UnicodeString SQLTypeName)/* overload */;
	__classmethod virtual System::Word __fastcall GetDBType(int SQLType)/* overload */;
	__classmethod System::Word __fastcall GetDataType(int SQLType);
	__classmethod System::Word __fastcall DataTypeToDBType(int DataType);
public:
	/* TConverterManager.Destroy */ inline __fastcall virtual ~TVirtualConverterManager() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Word vqBase = System::Word(0x1388);
static const System::Word vqUnknown = System::Word(0x1388);
static const System::Word vqString = System::Word(0x1389);
static const System::Word vqExtString = System::Word(0x138a);
static const System::Word vqWideString = System::Word(0x138b);
static const System::Word vqExtWideString = System::Word(0x138c);
static const System::Word vqFixedChar = System::Word(0x138d);
static const System::Word vqFixedWideChar = System::Word(0x138e);
static const System::Word vqInt8 = System::Word(0x138f);
static const System::Word vqByte = System::Word(0x1390);
static const System::Word vqSmallint = System::Word(0x1391);
static const System::Word vqInteger = System::Word(0x1392);
static const System::Word vqWord = System::Word(0x1393);
static const System::Word vqLongWord = System::Word(0x1394);
static const System::Word vqInt64 = System::Word(0x1395);
static const System::Word vqUInt64 = System::Word(0x1396);
static const System::Word vqSingle = System::Word(0x1397);
static const System::Word vqFloat = System::Word(0x1398);
static const System::Word vqExtended = System::Word(0x1399);
static const System::Word vqCurrency = System::Word(0x139a);
static const System::Word vqNumeric = System::Word(0x139b);
static const System::Word vqBCD = System::Word(0x139c);
static const System::Word vqFMTBCD = System::Word(0x139d);
static const System::Word vqDate = System::Word(0x139e);
static const System::Word vqTime = System::Word(0x139f);
static const System::Word vqDateTime = System::Word(0x13a0);
static const System::Word vqSQLTimeStamp = System::Word(0x13a1);
static const System::Word vqSQLTimeStampOffset = System::Word(0x13a2);
static const System::Word vqBoolean = System::Word(0x13a3);
static const System::Word vqBytes = System::Word(0x13a4);
static const System::Word vqVarBytes = System::Word(0x13a5);
static const System::Word vqExtVarBytes = System::Word(0x13a6);
static const System::Word vqBlob = System::Word(0x13a7);
static const System::Word vqMemo = System::Word(0x13a8);
static const System::Word vqWideMemo = System::Word(0x13a9);
static const System::Word vqObject = System::Word(0x13aa);
static const System::Word vqArray = System::Word(0x13ab);
static const System::Word vqTable = System::Word(0x13ac);
static const System::Word vqGuid = System::Word(0x13ad);
static const System::Word vqCursor = System::Word(0x13ae);
static const System::Word vqXML = System::Word(0x13af);
extern DELPHI_PACKAGE void __fastcall InitVirtualTypes(void);
}	/* namespace Virtualdatatypemap */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALDATATYPEMAP)
using namespace Virtualdatatypemap;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualdatatypemapHPP
