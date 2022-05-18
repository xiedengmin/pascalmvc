// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRHashAlgorithm.pas' rev: 34.00 (Windows)

#ifndef CrhashalgorithmHPP
#define CrhashalgorithmHPP

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
#include <CRDECUtil.hpp>
#include <CRCryptoTransformIntf.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crhashalgorithm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS THashAlgorithm;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* THashAlgorithmClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION THashAlgorithm : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	System::DynamicArray<System::Byte> FHashBlock;
	int FState;
	virtual void __fastcall HashCore(const char * Data, int Offset, int Count) = 0 ;
	virtual System::DynamicArray<System::Byte> __fastcall HashFinal() = 0 ;
	virtual int __fastcall Get_HashSize();
	
public:
	__fastcall virtual THashAlgorithm();
	virtual void __fastcall Initialize() = 0 ;
	void __fastcall TransformBlock(const System::DynamicArray<System::Byte> Data, int Offset, int Count)/* overload */;
	int __fastcall TransformBlock(const char * InputBuffer, int InputOffset, int InputCount, const char * OutputBuffer, int OutputOffset)/* overload */;
	void __fastcall TransformFinalBlock(const System::DynamicArray<System::Byte> InputBuffer, int InputOffset, int InputCount)/* overload */;
	void __fastcall TransformFinalBlock(const char * InputBuffer, int InputCount)/* overload */;
	System::DynamicArray<System::Byte> __fastcall ComputeHash(const char * Buffer, int Offset, int Count)/* overload */;
	System::DynamicArray<System::Byte> __fastcall ComputeHash(const System::DynamicArray<System::Byte> Buffer)/* overload */;
	__classmethod virtual int __fastcall GetHashSize();
	__property System::DynamicArray<System::Byte> Hash = {read=FHashBlock};
	__property int HashSize = {read=Get_HashSize, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THashAlgorithm() { }
	
private:
	void *__IHashTransform;	// Crcryptotransformintf::IHashTransform 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {2FA7F01B-FBAF-4357-A302-596DA966CD45}
	operator Crcryptotransformintf::_di_IHashTransform()
	{
		Crcryptotransformintf::_di_IHashTransform intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Crcryptotransformintf::IHashTransform*(void) { return (Crcryptotransformintf::IHashTransform*)&__IHashTransform; }
	#endif
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crhashalgorithm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRHASHALGORITHM)
using namespace Crhashalgorithm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrhashalgorithmHPP
