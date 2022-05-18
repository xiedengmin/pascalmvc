// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRCryptoTransformIntf.pas' rev: 34.00 (Windows)

#ifndef CrcryptotransformintfHPP
#define CrcryptotransformintfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crcryptotransformintf
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE ICryptoTransform;
typedef System::DelphiInterface<ICryptoTransform> _di_ICryptoTransform;
__interface DELPHIINTERFACE IHashTransform;
typedef System::DelphiInterface<IHashTransform> _di_IHashTransform;
//-- type declarations -------------------------------------------------------
__interface  INTERFACE_UUID("{AE5FFE08-1D09-4452-91C8-122B05513FEB}") ICryptoTransform  : public System::IInterface 
{
	virtual void __fastcall TransformBlock(const System::DynamicArray<System::Byte> Data, int Offset, int Count) = 0 /* overload */;
	virtual int __fastcall TransformBlock(const char * InputBuffer, int InputOffset, int InputCount, const char * OutputBuffer, int OutputOffset) = 0 /* overload */;
	virtual void __fastcall TransformFinalBlock(const System::DynamicArray<System::Byte> InputBuffer, int InputOffset, int InputCount) = 0 ;
	virtual void __fastcall SetIV(char * Value, int Offset, int Count) = 0 ;
	virtual int __fastcall Get_OutputBlockSize() = 0 ;
	__property int OutputBlockSize = {read=Get_OutputBlockSize};
};

__interface  INTERFACE_UUID("{2FA7F01B-FBAF-4357-A302-596DA966CD45}") IHashTransform  : public System::IInterface 
{
	virtual System::DynamicArray<System::Byte> __fastcall ComputeHash(const System::DynamicArray<System::Byte> Data) = 0 /* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall ComputeHash(const char * Data, int Offset, int Count) = 0 /* overload */;
	virtual int __fastcall Get_HashSize() = 0 ;
	__property int HashSize = {read=Get_HashSize};
};

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crcryptotransformintf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRCRYPTOTRANSFORMINTF)
using namespace Crcryptotransformintf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrcryptotransformintfHPP
