// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRRNG.pas' rev: 34.00 (Windows)

#ifndef CrrngHPP
#define CrrngHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SyncObjs.hpp>
#include <System.SysUtils.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRDECUtil.hpp>
#include <CRSymmetricAlgorithm.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crrng
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IScRandom;
typedef System::DelphiInterface<IScRandom> _di_IScRandom;
class DELPHICLASS TScRandom;
class DELPHICLASS TScRandomLFSR;
//-- type declarations -------------------------------------------------------
__interface IScRandom  : public System::IInterface 
{
	virtual void __fastcall Randomize(const System::DynamicArray<System::Byte> Seed) = 0 ;
	virtual void __fastcall Random(const System::DynamicArray<System::Byte> buf, const int Offset, const int Count) = 0 /* overload */;
	virtual void __fastcall Random(char * buf, int Count) = 0 /* overload */;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScRandom : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	int FRegister;
	Crsymmetricalgorithm::TSymmetricAlgorithm* FProtection;
	
protected:
	int FCount;
	int FSize;
	int FBasicSeed;
	System::Syncobjs::TCriticalSection* FLock;
	virtual void __fastcall SetSize(int Value);
	
public:
	__fastcall TScRandom();
	__fastcall virtual ~TScRandom();
	virtual void __fastcall Randomize(const System::DynamicArray<System::Byte> Seed, const int Offset, const int Count)/* overload */;
	void __fastcall Randomize(const System::DynamicArray<System::Byte> Seed)/* overload */;
	void __fastcall Randomize(System::Classes::TStream* Seed)/* overload */;
	void __fastcall Random(const System::DynamicArray<System::Byte> buf, const int Offset, const int Count)/* overload */;
	virtual void __fastcall Random(char * buf, int Count)/* overload */;
private:
	void *__IScRandom;	// IScRandom 
	
public:
	operator IScRandom*(void) { return (IScRandom*)&__IScRandom; }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScRandomLFSR : public TScRandom
{
	typedef TScRandom inherited;
	
private:
	int FPtr;
	int FLast;
	System::StaticArray<System::Word, 256> FTable;
	System::StaticArray<System::Byte, 256> FRegister;
	void __fastcall (*FFunc)(System::TObject* Self, char * Buffer, int Size);
	
protected:
	virtual void __fastcall SetSize(int Value);
	
public:
	virtual void __fastcall Randomize(const System::DynamicArray<System::Byte> Seed, const int Offset, const int Count)/* overload */;
	virtual void __fastcall Random(char * buf, int Count)/* overload */;
public:
	/* TScRandom.Create */ inline __fastcall TScRandomLFSR() : TScRandom() { }
	/* TScRandom.Destroy */ inline __fastcall virtual ~TScRandomLFSR() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  Randomize(const System::DynamicArray<System::Byte> Seed){ TScRandom::Randomize(Seed); }
	inline void __fastcall  Randomize(System::Classes::TStream* Seed){ TScRandom::Randomize(Seed); }
	inline void __fastcall  Random(const System::DynamicArray<System::Byte> buf, const int Offset, const int Count){ TScRandom::Random(buf, Offset, Count); }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crrng */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRRNG)
using namespace Crrng;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrrngHPP
