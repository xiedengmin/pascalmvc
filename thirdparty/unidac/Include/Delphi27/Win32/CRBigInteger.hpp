// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRBigInteger.pas' rev: 34.00 (Windows)

#ifndef CrbigintegerHPP
#define CrbigintegerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <Data.FmtBcd.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRRNG.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crbiginteger
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TBigInteger;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TBigInteger : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Crtypes::TLongWordArr FData;
	int FDataLength;
	System::Syncobjs::TCriticalSection* FLock;
	bool FIsPrepared;
	TBigInteger* FR1;
	TBigInteger* FR2;
	TBigInteger* FConstant;
	Crtypes::TIntArr FPolyArrray;
	void __fastcall ClearBarrettReduction();
	void __fastcall Truncate();
	__classmethod int __fastcall ShiftLeft(Crtypes::TLongWordArr &Buffer, int DataLength, int ShiftVal);
	__classmethod int __fastcall ShiftRight(Crtypes::TLongWordArr &Buffer, int ShiftVal);
	__classmethod void __fastcall MultiByteDivide(TBigInteger* bi1, TBigInteger* bi2, TBigInteger* outQuotient, TBigInteger* outRemainder);
	__classmethod void __fastcall SingleByteDivide(TBigInteger* bi1, TBigInteger* bi2, TBigInteger* outQuotient, TBigInteger* outRemainder);
	bool __fastcall RabinMillerTest(int Confidence);
	__classmethod void __fastcall Mul_1x1_GF2m(/* out */ unsigned &r1, /* out */ unsigned &r0, const unsigned a, const unsigned b);
	__classmethod void __fastcall Mul_2x2_GF2m(unsigned *R, const int R_High, unsigned a1, unsigned a0, unsigned b1, unsigned b0);
	
public:
	__fastcall TBigInteger()/* overload */;
	__fastcall TBigInteger(__int64 Value)/* overload */;
	__fastcall TBigInteger(__int64 Value, bool IsPositive)/* overload */;
	__fastcall TBigInteger(TBigInteger* Src)/* overload */;
	__fastcall TBigInteger(const System::UnicodeString Value, int Radix)/* overload */;
	__fastcall TBigInteger(const System::DynamicArray<System::Byte> Data)/* overload */;
	__fastcall TBigInteger(const System::DynamicArray<System::Byte> Data, int Offset, int Count)/* overload */;
	__fastcall TBigInteger(const unsigned *Data, const int Data_High)/* overload */;
	__fastcall TBigInteger(const Data::Fmtbcd::TBcd &Value)/* overload */;
	__fastcall virtual ~TBigInteger();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall SetToZero();
	void __fastcall PrepareForBarrettReduction();
	TBigInteger* __fastcall Add(TBigInteger* bi)/* overload */;
	TBigInteger* __fastcall Add(__int64 Value)/* overload */;
	TBigInteger* __fastcall Minus(TBigInteger* bi)/* overload */;
	TBigInteger* __fastcall Minus(__int64 Value)/* overload */;
	bool __fastcall IsNegative();
	bool __fastcall IsNegativeOrZero();
	TBigInteger* __fastcall Negate();
	TBigInteger* __fastcall ModMul(TBigInteger* bi, TBigInteger* modulus);
	TBigInteger* __fastcall BarrettReduction(TBigInteger* n);
	TBigInteger* __fastcall Mul(TBigInteger* bi)/* overload */;
	TBigInteger* __fastcall Mul(__int64 Value)/* overload */;
	TBigInteger* __fastcall Shl_(int ShiftVal);
	TBigInteger* __fastcall Shr_(int ShiftVal);
	void __fastcall Shr_1();
	void __fastcall SetBit(int BitNum);
	int __fastcall GetBit(int BitNum);
	bool __fastcall Equal(TBigInteger* bi);
	bool __fastcall NotEqual(TBigInteger* bi);
	bool __fastcall Greater(TBigInteger* bi);
	bool __fastcall Less(TBigInteger* bi);
	bool __fastcall GreaterOrEqual(TBigInteger* bi);
	bool __fastcall LessOrEqual(TBigInteger* bi);
	TBigInteger* __fastcall Div_(TBigInteger* bi);
	TBigInteger* __fastcall Mod_(TBigInteger* bi)/* overload */;
	TBigInteger* __fastcall Mod_(__int64 Value)/* overload */;
	TBigInteger* __fastcall ModInverse(TBigInteger* modulus);
	TBigInteger* __fastcall Max_(TBigInteger* bi);
	TBigInteger* __fastcall Min_(TBigInteger* bi);
	TBigInteger* __fastcall Or_(TBigInteger* bi);
	TBigInteger* __fastcall And_(TBigInteger* bi);
	TBigInteger* __fastcall Xor_(TBigInteger* bi);
	void __fastcall XorSelf(TBigInteger* bi);
	TBigInteger* __fastcall Abs();
	virtual System::UnicodeString __fastcall ToString()/* overload */;
	HIDESBASE System::UnicodeString __fastcall ToString(int Radix)/* overload */;
	TBigInteger* __fastcall ModPow(TBigInteger* exp, TBigInteger* n);
	TBigInteger* __fastcall gcd(TBigInteger* bi);
	void __fastcall GenRandomBits(int Bits, Crrng::_di_IScRandom Rand);
	int __fastcall BitCount();
	bool __fastcall IsOdd();
	unsigned __int64 __fastcall LongValue();
	unsigned __fastcall IntValue();
	System::DynamicArray<System::Byte> __fastcall GetBytes()/* overload */;
	System::DynamicArray<System::Byte> __fastcall GetBytes(int Count)/* overload */;
	System::DynamicArray<System::Byte> __fastcall GetBytesLE();
	Crtypes::TLongWordArr __fastcall GetData();
	__classmethod TBigInteger* __fastcall GenPseudoPrime(int Bits, int Confidence, Crrng::_di_IScRandom Rand);
	bool __fastcall IsProbablePrime(int Confidence);
	Crtypes::TIntArr __fastcall GetSetBitsArray();
	void __fastcall PrepareForGF2mCalc();
	TBigInteger* __fastcall ModSqr_GF2m(TBigInteger* modulus);
	TBigInteger* __fastcall ModMul_GF2m(TBigInteger* bi, TBigInteger* modulus);
	TBigInteger* __fastcall ModInv_GF2m(TBigInteger* modulus);
	TBigInteger* __fastcall ModDiv_GF2m(TBigInteger* bi, TBigInteger* modulus);
	TBigInteger* __fastcall Mod_GF2m(TBigInteger* modulus);
	__classmethod int __fastcall PutBigIntegerLE(TBigInteger* bi, System::DynamicArray<System::Byte> &Dest, int Offset);
	__classmethod System::DynamicArray<System::Byte> __fastcall GetBigIntegerLE(const System::DynamicArray<System::Byte> Src, int Offset, int Count)/* overload */;
	__classmethod System::DynamicArray<System::Byte> __fastcall GetBigIntegerLE(const System::DynamicArray<System::Byte> Src)/* overload */;
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crbiginteger */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRBIGINTEGER)
using namespace Crbiginteger;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrbigintegerHPP
