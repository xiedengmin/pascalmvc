// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRCipher.pas' rev: 34.00 (Windows)

#ifndef CrcipherHPP
#define CrcipherHPP

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
#include <DAConsts.hpp>
#include <CRDECUtil.hpp>
#include <CRSymmetricAlgorithm.hpp>
#include <CRCryptoTransformIntf.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crcipher
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCipher_Blowfish;
class DELPHICLASS TCipher_BlowfishLE;
class DELPHICLASS TCipher_Rijndael;
class DELPHICLASS TCipher_1DES;
class DELPHICLASS TCipher_3DES;
class DELPHICLASS TCipher_Cast128;
class DELPHICLASS TCipher_RC2;
class DELPHICLASS TCipher_RC4;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCipher_Blowfish : public Crsymmetricalgorithm::TSymmetricAlgorithm
{
	typedef Crsymmetricalgorithm::TSymmetricAlgorithm inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, Crsymmetricalgorithm::TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
public:
	/* TSymmetricAlgorithm.Create */ inline __fastcall virtual TCipher_Blowfish() : Crsymmetricalgorithm::TSymmetricAlgorithm() { }
	/* TSymmetricAlgorithm.Destroy */ inline __fastcall virtual ~TCipher_Blowfish() { }
	
};


class PASCALIMPLEMENTATION TCipher_BlowfishLE : public Crsymmetricalgorithm::TSymmetricAlgorithm
{
	typedef Crsymmetricalgorithm::TSymmetricAlgorithm inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, Crsymmetricalgorithm::TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
	
public:
	void __fastcall InitEx(const System::DynamicArray<System::Byte> Key)/* overload */;
	void __fastcall InitEx(const System::DynamicArray<System::Byte> Key, const System::DynamicArray<System::Byte> Salt)/* overload */;
public:
	/* TSymmetricAlgorithm.Create */ inline __fastcall virtual TCipher_BlowfishLE() : Crsymmetricalgorithm::TSymmetricAlgorithm() { }
	/* TSymmetricAlgorithm.Destroy */ inline __fastcall virtual ~TCipher_BlowfishLE() { }
	
};


class PASCALIMPLEMENTATION TCipher_Rijndael : public Crsymmetricalgorithm::TSymmetricAlgorithm
{
	typedef Crsymmetricalgorithm::TSymmetricAlgorithm inherited;
	
private:
	int FRounds;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, Crsymmetricalgorithm::TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
public:
	/* TSymmetricAlgorithm.Create */ inline __fastcall virtual TCipher_Rijndael() : Crsymmetricalgorithm::TSymmetricAlgorithm() { }
	/* TSymmetricAlgorithm.Destroy */ inline __fastcall virtual ~TCipher_Rijndael() { }
	
};


class PASCALIMPLEMENTATION TCipher_1DES : public Crsymmetricalgorithm::TSymmetricAlgorithm
{
	typedef Crsymmetricalgorithm::TSymmetricAlgorithm inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, Crsymmetricalgorithm::TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	void __fastcall MakeKey(const System::Byte *Data, const int Data_High, const int DataOffset, System::PCardinal Key, const int KeyOffset, bool Reverse);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
	__classmethod void __fastcall DES_Func(System::PCardinal Data, int DataOffset, System::PCardinal Key);
public:
	/* TSymmetricAlgorithm.Create */ inline __fastcall virtual TCipher_1DES() : Crsymmetricalgorithm::TSymmetricAlgorithm() { }
	/* TSymmetricAlgorithm.Destroy */ inline __fastcall virtual ~TCipher_1DES() { }
	
};


class PASCALIMPLEMENTATION TCipher_3DES : public TCipher_1DES
{
	typedef TCipher_1DES inherited;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, Crsymmetricalgorithm::TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
public:
	/* TSymmetricAlgorithm.Create */ inline __fastcall virtual TCipher_3DES() : TCipher_1DES() { }
	/* TSymmetricAlgorithm.Destroy */ inline __fastcall virtual ~TCipher_3DES() { }
	
};


class PASCALIMPLEMENTATION TCipher_Cast128 : public Crsymmetricalgorithm::TSymmetricAlgorithm
{
	typedef Crsymmetricalgorithm::TSymmetricAlgorithm inherited;
	
private:
	System::Byte FRounds;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, Crsymmetricalgorithm::TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
public:
	/* TSymmetricAlgorithm.Create */ inline __fastcall virtual TCipher_Cast128() : Crsymmetricalgorithm::TSymmetricAlgorithm() { }
	/* TSymmetricAlgorithm.Destroy */ inline __fastcall virtual ~TCipher_Cast128() { }
	
};


class PASCALIMPLEMENTATION TCipher_RC2 : public Crsymmetricalgorithm::TSymmetricAlgorithm
{
	typedef Crsymmetricalgorithm::TSymmetricAlgorithm inherited;
	
private:
	int FEffectiveKeySize;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, Crsymmetricalgorithm::TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
	
public:
	__property int EffectiveKeySize = {read=FEffectiveKeySize, write=FEffectiveKeySize, nodefault};
public:
	/* TSymmetricAlgorithm.Create */ inline __fastcall virtual TCipher_RC2() : Crsymmetricalgorithm::TSymmetricAlgorithm() { }
	/* TSymmetricAlgorithm.Destroy */ inline __fastcall virtual ~TCipher_RC2() { }
	
};


class PASCALIMPLEMENTATION TCipher_RC4 : public Crsymmetricalgorithm::TSymmetricAlgorithm
{
	typedef Crsymmetricalgorithm::TSymmetricAlgorithm inherited;
	
private:
	System::Byte FI;
	System::Byte FJ;
	System::Byte FSI;
	System::Byte FSJ;
	
protected:
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, Crsymmetricalgorithm::TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
	virtual void __fastcall Done();
	virtual int __fastcall Get_OutputBlockSize();
public:
	/* TSymmetricAlgorithm.Create */ inline __fastcall virtual TCipher_RC4() : Crsymmetricalgorithm::TSymmetricAlgorithm() { }
	/* TSymmetricAlgorithm.Destroy */ inline __fastcall virtual ~TCipher_RC4() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crcipher */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRCIPHER)
using namespace Crcipher;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrcipherHPP
