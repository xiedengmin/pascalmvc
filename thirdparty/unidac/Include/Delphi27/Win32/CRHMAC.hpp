// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRHMAC.pas' rev: 34.00 (Windows)

#ifndef CrhmacHPP
#define CrhmacHPP

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
#include <CRHashAlgorithm.hpp>
#include <CRCryptoTransformIntf.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crhmac
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TKeyedHashAlgorithm;
class DELPHICLASS THMAC;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TKeyedHashAlgorithm : public Crhashalgorithm::THashAlgorithm
{
	typedef Crhashalgorithm::THashAlgorithm inherited;
	
protected:
	System::DynamicArray<System::Byte> FKeyValue;
public:
	/* THashAlgorithm.Create */ inline __fastcall virtual TKeyedHashAlgorithm() : Crhashalgorithm::THashAlgorithm() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TKeyedHashAlgorithm() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THMAC : public TKeyedHashAlgorithm
{
	typedef TKeyedHashAlgorithm inherited;
	
private:
	Crhashalgorithm::THashAlgorithm* FHashAlgorithm;
	bool FIsHashing;
	System::DynamicArray<System::Byte> FPadded36;
	System::DynamicArray<System::Byte> FPadded5C;
	int FBlockSize;
	
protected:
	virtual int __fastcall Get_HashSize();
	virtual void __fastcall HashCore(const char * Data, int Offset, int Count);
	virtual System::DynamicArray<System::Byte> __fastcall HashFinal();
	
public:
	__fastcall THMAC(const Crhashalgorithm::THashAlgorithmClass HashAlgorithmClass, const System::DynamicArray<System::Byte> rgbKey);
	__fastcall virtual ~THMAC();
	virtual void __fastcall Initialize();
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crhmac */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRHMAC)
using namespace Crhmac;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrhmacHPP
