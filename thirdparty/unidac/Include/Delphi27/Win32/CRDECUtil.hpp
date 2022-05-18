// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRDECUtil.pas' rev: 34.00 (Windows)

#ifndef CrdecutilHPP
#define CrdecutilHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crdecutil
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE unsigned __fastcall ROL(unsigned Value, int Shift);
extern DELPHI_PACKAGE unsigned __fastcall ReverseInt4(unsigned Value);
extern DELPHI_PACKAGE unsigned __fastcall SwapInteger(unsigned Value);
extern DELPHI_PACKAGE unsigned __int64 __fastcall SwapInt64(unsigned __int64 Value);
extern DELPHI_PACKAGE void __fastcall SwapIntegerBuffer(const Crtypes::TCardinalArray Source, const Crtypes::TCardinalArray Dest, int Count);
extern DELPHI_PACKAGE void __fastcall SwapInt64Buffer(const Crtypes::TUInt64Array Source, const Crtypes::TUInt64Array Dest, int Count);
extern DELPHI_PACKAGE void __fastcall XORBuffers(System::PCardinal Data, int Offset, const System::PCardinal XORBlock, int ByteSize);
extern DELPHI_PACKAGE int __fastcall RndXORBuffer(int Seed, const char * Buf, int Size);
extern DELPHI_PACKAGE int __fastcall RndXORBufferL(int Seed, const Crtypes::TCardinalArray Buf, int Size);
extern DELPHI_PACKAGE int __fastcall MemCompare(System::Sysutils::PByteArray P1, System::Sysutils::PByteArray P2, int Size);
extern DELPHI_PACKAGE int __fastcall CompareBuf(const System::DynamicArray<System::Byte> Buf1, const System::DynamicArray<System::Byte> Buf2, int Size);
extern DELPHI_PACKAGE void __fastcall ArrayReverse(const System::DynamicArray<System::Byte> Arr, const int Offset, const int Length);
extern DELPHI_PACKAGE unsigned __fastcall GetIntLE(const System::DynamicArray<System::Byte> Src, int Offset);
extern DELPHI_PACKAGE void __fastcall PutIntLE(const unsigned Value, System::DynamicArray<System::Byte> &Dest, int Offset);
extern DELPHI_PACKAGE unsigned __fastcall GetIntBE(const System::DynamicArray<System::Byte> Src, int Offset);
extern DELPHI_PACKAGE void __fastcall PutIntBE(const unsigned Value, char * Dest, int Offset);
extern DELPHI_PACKAGE __int64 __fastcall GetInt64BE(const System::DynamicArray<System::Byte> Src, int Offset);
extern DELPHI_PACKAGE void __fastcall PutInt64BE(const __int64 Value, char * Dest, int Offset);
}	/* namespace Crdecutil */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRDECUTIL)
using namespace Crdecutil;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrdecutilHPP
