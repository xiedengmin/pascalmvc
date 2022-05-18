// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MemUtils.pas' rev: 34.00 (Windows)

#ifndef MemutilsHPP
#define MemutilsHPP

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
#include <System.ZLib.hpp>
#include <System.ZLibConst.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Memutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef int __fastcall (*TCompressProc)(void * dest, void * destLen, const void * source, int sourceLen);

typedef int __fastcall (*TUncompressProc)(void * dest, void * destlen, void * source, int sourceLne);

typedef System::Zlib::EZCompressionError ECompressionError;

typedef System::Zlib::EZDecompressionError EDecompressionError;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MIN_COMPRESS_LENGTH = System::Int8(0x32);
extern DELPHI_PACKAGE TCompressProc CompressProc;
extern DELPHI_PACKAGE TUncompressProc UncompressProc;
extern DELPHI_PACKAGE bool IsWin9x;
extern DELPHI_PACKAGE bool __fastcall CompareGuid(const GUID &g1, const GUID &g2);
extern DELPHI_PACKAGE System::TDateTime __fastcall TimeStampToDateTime(const System::Sysutils::TTimeStamp &ATimeStamp);
extern DELPHI_PACKAGE bool __fastcall VarEqual(const System::Variant &Value1, const System::Variant &Value2);
extern DELPHI_PACKAGE void __fastcall CopyBuffer(void * Source, void * Dest, int Count);
extern DELPHI_PACKAGE void __fastcall CopyBufferAnsi(const System::AnsiString Source, void * Dest, int Count);
extern DELPHI_PACKAGE void __fastcall CopyBufferUni(const System::WideString Source, void * Dest, int Count);
extern DELPHI_PACKAGE void __fastcall FillChar(void * X, int Count, System::Byte Value);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall DynArrayCreate(const System::Byte *SourceArray, const int SourceArray_High)/* overload */;
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall DynArrayCreate(const char *SourceArray, const int SourceArray_High)/* overload */;
extern DELPHI_PACKAGE void __fastcall ArrayCopy(System::DynamicArray<System::Byte> SourceArray, int SourceIndex, System::DynamicArray<System::Byte> DestinationArray, int DestinationIndex, int Length);
extern DELPHI_PACKAGE void * __fastcall AllocGCHandle(void * Obj, bool Pinned = false)/* overload */;
extern DELPHI_PACKAGE System::TObject* __fastcall GetGCHandleTarget(void * Handle);
extern DELPHI_PACKAGE void * __fastcall GetAddrOfPinnedObject(void * Handle);
extern DELPHI_PACKAGE void __fastcall FreeGCHandle(void * Handle);
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall AllocValueBuffer(int Size);
extern DELPHI_PACKAGE void __fastcall FreeValueBuffer(System::DynamicArray<System::Byte> Buffer);
extern DELPHI_PACKAGE void __fastcall FreeString(void * P);
extern DELPHI_PACKAGE void * __fastcall AllocOrdinal(/* out */ System::Int8 &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall AllocOrdinal(/* out */ System::Byte &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall AllocOrdinal(/* out */ System::Word &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall AllocOrdinal(/* out */ int &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall AllocOrdinal(/* out */ unsigned &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall AllocOrdinal(/* out */ void * &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall OrdinalToPtr(/* out */ double &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall OrdinalToPtr(/* out */ System::Byte &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall OrdinalToPtr(/* out */ short &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall OrdinalToPtr(/* out */ int &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall OrdinalToPtr(/* out */ __int64 &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall OrdinalToPtr(/* out */ unsigned &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall OrdinalToPtr(/* out */ System::Word &Obj)/* overload */;
extern DELPHI_PACKAGE void * __fastcall OrdinalToPtr(/* out */ void * &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ System::Int8 &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ System::Byte &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ short &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ System::Word &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ int &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ __int64 &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ double &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ unsigned &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall PtrToOrdinal(void * P, /* out */ void * &Obj)/* overload */;
extern DELPHI_PACKAGE void __fastcall FreeOrdinal(void * P);
extern DELPHI_PACKAGE void * __fastcall StrCopyW(System::WideChar * Dest, const System::WideChar * Source);
extern DELPHI_PACKAGE void __fastcall StrLCopyW(System::WideChar * Dest, const System::WideChar * Source, int MaxLen);
extern DELPHI_PACKAGE int __fastcall StrLenW(const System::WideChar * Str);
extern DELPHI_PACKAGE int __fastcall StrTrim(const char * Str, int Len);
extern DELPHI_PACKAGE System::AnsiString __fastcall StrTrimmed(const char * Str, int Len = 0xffffffff);
extern DELPHI_PACKAGE int __fastcall StrTrimW(const System::WideChar * Str, int Len);
extern DELPHI_PACKAGE System::WideString __fastcall StrTrimmedW(const System::WideChar * Str, int Len = 0xffffffff);
extern DELPHI_PACKAGE int __fastcall AnsiStrLCompWS(const System::WideString S1, const System::WideString S2, int MaxLen);
extern DELPHI_PACKAGE int __fastcall AnsiStrLICompWS(const System::WideString S1, const System::WideString S2, int MaxLen);
extern DELPHI_PACKAGE int __fastcall AnsiStrCompWS(const System::WideString S1, const System::WideString S2);
extern DELPHI_PACKAGE int __fastcall AnsiStrICompWS(const System::WideString S1, const System::WideString S2);
extern DELPHI_PACKAGE int __fastcall AnsiStrCompS(char * S1, char * S2);
extern DELPHI_PACKAGE int __fastcall AnsiStrICompS(char * S1, char * S2);
extern DELPHI_PACKAGE int __fastcall AnsiCompareTextS(const System::AnsiString S1, const System::AnsiString S2);
extern DELPHI_PACKAGE int __fastcall AnsiCompareStrS(const System::AnsiString S1, const System::AnsiString S2);
extern DELPHI_PACKAGE void __fastcall BinToHexA(const System::DynamicArray<System::Byte> Buffer, char * Text, int BufSize);
extern DELPHI_PACKAGE void __fastcall BinToHexW(const System::DynamicArray<System::Byte> Buffer, System::WideChar * Text, int BufSize);
extern DELPHI_PACKAGE int __fastcall HexToBinA(char * Text, void * Buffer, int BufSize)/* overload */;
extern DELPHI_PACKAGE int __fastcall HexToBinW(System::WideChar * Text, void * Buffer, int BufSize)/* overload */;
extern DELPHI_PACKAGE void __fastcall OleVarClear(System::POleVariant pValue);
extern DELPHI_PACKAGE System::OleVariant __fastcall GetOleVariant(System::POleVariant pValue);
extern DELPHI_PACKAGE void __fastcall SetOleVariant(System::POleVariant pValue, const System::OleVariant &Value);
extern DELPHI_PACKAGE bool __fastcall TryEncodeDate(System::Word Year, System::Word Month, System::Word Day, /* out */ System::TDateTime &Date);
extern DELPHI_PACKAGE bool __fastcall TryEncodeTime(System::Word Hour, System::Word Min, System::Word Sec, System::Word MSec, /* out */ System::TDateTime &Time);
extern DELPHI_PACKAGE bool __fastcall TryEncodeDateTime(const System::Word AYear, const System::Word AMonth, const System::Word ADay, const System::Word AHour, const System::Word AMinute, const System::Word ASecond, const System::Word AMilliSecond, /* out */ System::TDateTime &AValue);
extern DELPHI_PACKAGE System::TDateTime __fastcall EncodeDateTime(const System::Word AYear, const System::Word AMonth, const System::Word ADay, const System::Word AHour, const System::Word AMinute, const System::Word ASecond, const System::Word AMilliSecond);
extern DELPHI_PACKAGE System::Word __fastcall Reverse2(System::Word Value);
extern DELPHI_PACKAGE unsigned __fastcall Reverse4(unsigned Value);
extern DELPHI_PACKAGE __int64 __fastcall Reverse8(__int64 Value);
extern DELPHI_PACKAGE void __fastcall CheckZLib(void);
extern DELPHI_PACKAGE void __fastcall DoCompress(void * dest, void * destLen, const void * source, int sourceLen);
extern DELPHI_PACKAGE void __fastcall DoUncompress(void * dest, void * destlen, void * source, int sourceLne);
}	/* namespace Memutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MEMUTILS)
using namespace Memutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MemutilsHPP
