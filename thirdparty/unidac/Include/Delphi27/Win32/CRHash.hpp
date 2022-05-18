// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRHash.pas' rev: 34.00 (Windows)

#ifndef CrhashHPP
#define CrhashHPP

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
#include <CRDECUtil.hpp>
#include <CRHashAlgorithm.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crhash
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS THash_MD2;
class DELPHICLASS THash_MD4;
class DELPHICLASS THash_MD5;
class DELPHICLASS THash_SHA;
class DELPHICLASS THash_SHA1;
class DELPHICLASS THash_SHA2_256;
class DELPHICLASS THash_SHA2_224;
class DELPHICLASS THash_SHA2_512;
class DELPHICLASS THash_SHA2_384;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_MD2 : public Crhashalgorithm::THashAlgorithm
{
	typedef Crhashalgorithm::THashAlgorithm inherited;
	
private:
	int FCount;
	System::StaticArray<System::Byte, 16> FBuffer;
	System::StaticArray<System::Byte, 16> FCheckSum;
	System::StaticArray<System::Byte, 48> X;
	
protected:
	void __fastcall Transform(System::Sysutils::PByteArray Buffer);
	void __fastcall Done();
	virtual void __fastcall HashCore(const char * Data, int Offset, int Count);
	virtual System::DynamicArray<System::Byte> __fastcall HashFinal();
	
public:
	__fastcall virtual THash_MD2();
	virtual void __fastcall Initialize();
	__classmethod virtual int __fastcall GetHashSize();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_MD2() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_MD4 : public Crhashalgorithm::THashAlgorithm
{
	typedef Crhashalgorithm::THashAlgorithm inherited;
	
private:
	unsigned FCount;
	System::StaticArray<unsigned, 16> FBuffer;
	System::StaticArray<unsigned, 8> FDigest;
	
protected:
	virtual void __fastcall Transform(Crtypes::TCardinalArray Buffer);
	virtual void __fastcall Done();
	virtual void __fastcall HashCore(const char * Data, int Offset, int Count);
	virtual System::DynamicArray<System::Byte> __fastcall HashFinal();
	
public:
	__fastcall virtual THash_MD4();
	virtual void __fastcall Initialize();
	__classmethod virtual int __fastcall GetHashSize();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_MD4() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_MD5 : public THash_MD4
{
	typedef THash_MD4 inherited;
	
protected:
	virtual void __fastcall Transform(Crtypes::TCardinalArray Buffer);
public:
	/* THash_MD4.Create */ inline __fastcall virtual THash_MD5() : THash_MD4() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_MD5() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_SHA : public THash_MD4
{
	typedef THash_MD4 inherited;
	
protected:
	virtual void __fastcall Done();
public:
	/* THash_MD4.Create */ inline __fastcall virtual THash_SHA() : THash_MD4() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_SHA() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_SHA1 : public THash_SHA
{
	typedef THash_SHA inherited;
	
protected:
	bool FRotate;
	virtual void __fastcall Transform(Crtypes::TCardinalArray Buffer);
	
public:
	virtual void __fastcall Initialize();
	__classmethod virtual int __fastcall GetHashSize();
public:
	/* THash_MD4.Create */ inline __fastcall virtual THash_SHA1() : THash_SHA() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_SHA1() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_SHA2_256 : public THash_SHA
{
	typedef THash_SHA inherited;
	
protected:
	virtual void __fastcall Transform(Crtypes::TCardinalArray Buffer);
	
public:
	virtual void __fastcall Initialize();
	__classmethod virtual int __fastcall GetHashSize();
public:
	/* THash_MD4.Create */ inline __fastcall virtual THash_SHA2_256() : THash_SHA() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_SHA2_256() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THash_SHA2_224 : public THash_SHA2_256
{
	typedef THash_SHA2_256 inherited;
	
public:
	virtual void __fastcall Initialize();
	__classmethod virtual int __fastcall GetHashSize();
public:
	/* THash_MD4.Create */ inline __fastcall virtual THash_SHA2_224() : THash_SHA2_256() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_SHA2_224() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION THash_SHA2_512 : public Crhashalgorithm::THashAlgorithm
{
	typedef Crhashalgorithm::THashAlgorithm inherited;
	
private:
	__int64 FCount;
	System::StaticArray<unsigned __int64, 16> FBuffer;
	System::StaticArray<unsigned __int64, 8> FDigest;
	
protected:
	virtual void __fastcall Transform(Crtypes::TUInt64Array Buffer);
	virtual void __fastcall Done();
	virtual void __fastcall HashCore(const char * Data, int Offset, int Count);
	virtual System::DynamicArray<System::Byte> __fastcall HashFinal();
	
public:
	__fastcall virtual THash_SHA2_512();
	virtual void __fastcall Initialize();
	__classmethod virtual int __fastcall GetHashSize();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_SHA2_512() { }
	
};


class PASCALIMPLEMENTATION THash_SHA2_384 : public THash_SHA2_512
{
	typedef THash_SHA2_512 inherited;
	
public:
	virtual void __fastcall Initialize();
	__classmethod virtual int __fastcall GetHashSize();
public:
	/* THash_SHA2_512.Create */ inline __fastcall virtual THash_SHA2_384() : THash_SHA2_512() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~THash_SHA2_384() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crhash */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRHASH)
using namespace Crhash;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrhashHPP
