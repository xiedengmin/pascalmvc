// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRBase64.pas' rev: 34.00 (Windows)

#ifndef Crbase64HPP
#define Crbase64HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crbase64
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TBase64;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBase64 : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall InternalEncode(const System::DynamicArray<System::Byte> Data, int Offset, int Count, const System::DynamicArray<System::Byte> OutBuf, int OutOffset, /* out */ int &OutCount);
	__classmethod System::DynamicArray<System::Byte> __fastcall Encode(const System::DynamicArray<System::Byte> Data)/* overload */;
	__classmethod System::DynamicArray<System::Byte> __fastcall Encode(const System::DynamicArray<System::Byte> Data, int Offset, int Count)/* overload */;
	__classmethod void __fastcall Encode(System::Classes::TStream* InStream, System::Classes::TStream* OutStream)/* overload */;
	__classmethod void __fastcall InternalDecode(const System::DynamicArray<System::Byte> InBuf, const int InOffset, const int InCount, const System::DynamicArray<System::Byte> OutBuf, int OutOffset, /* out */ int &OutCount, /* out */ int &UnReadCount, bool &IsFinised);
	__classmethod System::DynamicArray<System::Byte> __fastcall Decode(const System::DynamicArray<System::Byte> Data)/* overload */;
	__classmethod System::DynamicArray<System::Byte> __fastcall Decode(const System::DynamicArray<System::Byte> Data, int Offset, int Count)/* overload */;
	__classmethod void __fastcall Decode(System::Classes::TStream* InStream, System::Classes::TStream* OutStream)/* overload */;
public:
	/* TObject.Create */ inline __fastcall TBase64() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TBase64() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crbase64 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRBASE64)
using namespace Crbase64;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Crbase64HPP
