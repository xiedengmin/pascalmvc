// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRFunctions.pas' rev: 34.00 (Windows)

#ifndef CrfunctionsHPP
#define CrfunctionsHPP

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
#include <System.VarUtils.hpp>
#include <System.SyncObjs.hpp>
#include <Data.FmtBcd.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crfunctions
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS CRBitConverter;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION CRBitConverter : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod float __fastcall Int32BitsToSingle(int Value);
	__classmethod int __fastcall SingleToInt32Bits(float Value);
	__classmethod System::Extended __fastcall BytesToExtended(const System::DynamicArray<System::Byte> Value);
	__classmethod System::DynamicArray<System::Byte> __fastcall ExtendedToBytes(System::Extended Value);
public:
	/* TObject.Create */ inline __fastcall CRBitConverter() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~CRBitConverter() { }
	
};

#pragma pack(pop)

typedef bool __fastcall (*TIsClassFunction)(System::TObject* Obj, System::TClass AClass);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TIsClassFunction IsClass;
extern DELPHI_PACKAGE System::Syncobjs::TEvent* __fastcall CreateEvent(bool InitialState = false);
extern DELPHI_PACKAGE double __fastcall Exponent10(unsigned Exponent);
extern DELPHI_PACKAGE int __fastcall GetBcdPrecision(const Data::Fmtbcd::TBcd &Bcd);
extern DELPHI_PACKAGE int __fastcall GetBcdScale(const Data::Fmtbcd::TBcd &Bcd);
extern DELPHI_PACKAGE bool __fastcall IsBcdInt(const Data::Fmtbcd::TBcd &Bcd);
extern DELPHI_PACKAGE bool __fastcall IsBcdZero(const Data::Fmtbcd::TBcd &Bcd);
extern DELPHI_PACKAGE bool __fastcall NormalizeBcd(const Data::Fmtbcd::TBcd &InBCD, Data::Fmtbcd::TBcd &OutBcd, int Precision, int Places);
extern DELPHI_PACKAGE bool __fastcall TryStrToGUID(const System::UnicodeString S, /* out */ GUID &Value);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ConvertGuidToString(const GUID &Guid, const bool WithBraces);
extern DELPHI_PACKAGE void __fastcall AssignStrings(System::Classes::TStrings* Source, System::Classes::TStrings* Dest)/* overload */;
extern DELPHI_PACKAGE int __fastcall UnicodeToUtf8(char * Dest, System::WideChar * Source, int MaxBytes)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall UnicodeToUtf8(char * Dest, unsigned MaxDestBytes, System::WideChar * Source, unsigned SourceChars)/* overload */;
extern DELPHI_PACKAGE int __fastcall Utf8ToUnicode(System::WideChar * Dest, char * Source, int MaxChars)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall Utf8ToUnicode(System::WideChar * Dest, unsigned MaxDestChars, char * Source, unsigned SourceBytes)/* overload */;
extern DELPHI_PACKAGE System::RawByteString __fastcall Utf8Encode(const System::WideString WS);
extern DELPHI_PACKAGE System::WideString __fastcall Utf8Decode(const System::RawByteString S);
extern DELPHI_PACKAGE System::RawByteString __fastcall AnsiToUtf8(const System::AnsiString S);
extern DELPHI_PACKAGE System::AnsiString __fastcall Utf8ToAnsi(const System::RawByteString S);
extern DELPHI_PACKAGE unsigned __fastcall UnicodeToUtf8WoT(char * Dest, unsigned MaxDestBytes, System::WideChar * Source, unsigned SourceChars);
extern DELPHI_PACKAGE unsigned __fastcall Utf8ToUnicodeWoT(System::WideChar * Dest, unsigned MaxDestChars, char * Source, unsigned SourceBytes);
extern DELPHI_PACKAGE int __fastcall DetectUtf8LastChar(void * Buf, int Size);
extern DELPHI_PACKAGE int __fastcall DetectUtf8LastBrockenChar(void * Buf, int Size);
extern DELPHI_PACKAGE void __fastcall ConvertBigEndianBuffer(void * Buf, int Count)/* overload */;
extern DELPHI_PACKAGE void __fastcall ConvertBigEndianBuffer(void * SrcBuf, void * DestBuf, int Count)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall StrToCardinal(const System::UnicodeString S);
extern DELPHI_PACKAGE unsigned __int64 __fastcall StrToUInt64(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall TryStrToCardinal(const System::UnicodeString S, /* out */ unsigned &Value);
extern DELPHI_PACKAGE bool __fastcall TryStrToUInt64(const System::UnicodeString S, /* out */ unsigned __int64 &Value);
extern DELPHI_PACKAGE void * __fastcall PtrOffset(void * Value, int Offset)/* overload */;
extern DELPHI_PACKAGE void * __fastcall PtrOffset(void * Value, unsigned Offset)/* overload */;
extern DELPHI_PACKAGE void * __fastcall PtrOffset(void * Value, __int64 Offset)/* overload */;
extern DELPHI_PACKAGE int __fastcall PtrSubstract(void * Value1, void * Value2);
extern DELPHI_PACKAGE int __fastcall PtrCompare(void * Value1, void * Value2)/* overload */;
extern DELPHI_PACKAGE int __fastcall PtrCompare(void * Value1, int Offset1, void * Value2, int Offset2)/* overload */;
extern DELPHI_PACKAGE int __fastcall PtrCompare(void * Value1, __int64 Offset1, void * Value2, __int64 Offset2)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsMainThread(void);
extern DELPHI_PACKAGE bool __fastcall IsThread(unsigned ThreadID);
extern DELPHI_PACKAGE void __fastcall SynchronizeWithMainThread(System::Classes::_di_TThreadProcedure Proc)/* overload */;
extern DELPHI_PACKAGE void __fastcall SynchronizeWithMainThread(System::Classes::TThreadMethod Method)/* overload */;
extern DELPHI_PACKAGE bool __fastcall CompareMethods(const System::TMethod &Method1, const System::TMethod &Method2);
extern DELPHI_PACKAGE bool __fastcall GetIsClass(System::TObject* Obj, System::TClass AClass);
extern DELPHI_PACKAGE bool __fastcall GetIsClassByName(System::TObject* Obj, System::TClass AClass)/* overload */;
extern DELPHI_PACKAGE bool __fastcall GetIsClassByName(System::TObject* Obj, const System::UnicodeString AClassName)/* overload */;
extern DELPHI_PACKAGE int __fastcall LengthA(const System::AnsiString AStr);
extern DELPHI_PACKAGE void __fastcall SetLengthA(System::AnsiString &AStr, int NewLength);
extern DELPHI_PACKAGE unsigned __fastcall SwapCardinal(const unsigned Value);
extern DELPHI_PACKAGE unsigned __fastcall GetTickInterval(unsigned StartTickCount, unsigned FinishTickCount)/* overload */;
extern DELPHI_PACKAGE int __fastcall GetLocalTimeZoneOffset(void);
extern DELPHI_PACKAGE System::Byte __fastcall StrToDay(const System::UnicodeString Day);
extern DELPHI_PACKAGE int __fastcall StrToMonth(const System::UnicodeString Month);
extern DELPHI_PACKAGE System::TDateTime __fastcall InternetStrToDateTime(const System::UnicodeString Value);
extern DELPHI_PACKAGE System::TDateTime __fastcall GMTToLocalDateTime(const System::UnicodeString Value);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LocalDateTimeToGMT(const System::TDateTime Value, bool IncludeGMT = false);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DateTimeToHttpStr(const System::TDateTime Value);
extern DELPHI_PACKAGE bool __fastcall IsNumeric(const System::UnicodeString Str);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractFirstWord(System::UnicodeString &Str, const System::UnicodeString Separator);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetFirstWord(const System::UnicodeString Str, const System::UnicodeString Separator);
extern DELPHI_PACKAGE bool __fastcall ByteInSet(System::Byte AByte, const System::DynamicArray<System::Byte> ASet);
extern DELPHI_PACKAGE Crtypes::TMachineType __fastcall DetectLibraryMachineType(const System::UnicodeString LibraryName);
extern DELPHI_PACKAGE int __fastcall BobJenkinsHash(const void *Data, int Len, int InitData);
extern DELPHI_PACKAGE int __fastcall BobJenkinsHashStr(const System::UnicodeString Str, int InitData);
extern DELPHI_PACKAGE int __fastcall BobJenkinsHashAStr(const System::AnsiString AStr, int InitData);
extern DELPHI_PACKAGE int __fastcall BobJenkinsHashWStr(const System::WideString WStr, int InitData);
}	/* namespace Crfunctions */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRFUNCTIONS)
using namespace Crfunctions;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrfunctionsHPP
