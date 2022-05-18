// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRSymmetricAlgorithm.pas' rev: 34.00 (Windows)

#ifndef CrsymmetricalgorithmHPP
#define CrsymmetricalgorithmHPP

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

namespace Crsymmetricalgorithm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TKeySizes;
class DELPHICLASS TSymmetricAlgorithm;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TKeySizes : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int MaxSize;
	int MinSize;
	int SkipSize;
public:
	/* TObject.Create */ inline __fastcall TKeySizes() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TKeySizes() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TCipherMode : unsigned char { cmECB, cmCBC, cmCTR, cmGCM };

enum DECLSPEC_DENUM TPaddingMode : unsigned char { pmNone, pmPKCS7, pmZeros };

typedef void __fastcall (__closure *TTransformBlockMethod)(char * Source, int SrcOffset, int DataSize, char * Dest, int DstOffset);

typedef System::TMetaClass* TSymmetricAlgorithmClass;

class PASCALIMPLEMENTATION TSymmetricAlgorithm : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TTransformBlockMethod FTransformBlockMethod;
	TCipherMode FMode;
	TPaddingMode FPadding;
	TKeySizes* FLegalKeySizes;
	int FIntSize;
	unsigned *FBuffer;
	unsigned *FFeedback;
	unsigned *FVector;
	unsigned *FTag;
	unsigned *FGHASH;
	System::DynamicArray<System::Byte> FAAD;
	System::DynamicArray<System::Byte> FReceivedTag;
	bool FInitialized;
	System::StaticArray<System::StaticArray<unsigned, 4>, 16> FGcmPreCalc;
	void __fastcall ProcessGcmAAD();
	void __fastcall ProcessGcmLengths(int DataSize);
	System::DynamicArray<System::Byte> __fastcall GetTag();
	
protected:
	Crtypes::TCardinalConstArray *FUser;
	int FUserSize;
	int FKeySize;
	int FBlockSize;
	void __fastcall SetMode(const TCipherMode Value);
	void __fastcall SetKey(const System::DynamicArray<System::Byte> Value);
	void __fastcall Set_IV(const System::DynamicArray<System::Byte> Value);
	void __fastcall IncVector();
	void __fastcall GcmMulH(char * Value);
	void __fastcall InitGCM();
	virtual void __fastcall Done();
	void __fastcall InitBegin(int KeySize);
	void __fastcall InitEnd(const System::PCardinal IVector);
	__classmethod virtual void __fastcall GetContext(int &BlockSize, int &UserSize, TKeySizes* KeySizes);
	virtual void __fastcall Encode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	virtual void __fastcall Decode(System::PCardinal Data, int Offset, int DataSize = 0x0);
	void __fastcall InternalEncodeBuffer(System::PCardinal Data, int DataSize);
	void __fastcall InternalDecodeBuffer(System::PCardinal Data, int DataSize);
	virtual void __fastcall Init(const System::DynamicArray<System::Byte> Key, const System::PCardinal IVector);
	virtual void __fastcall Protect();
	virtual int __fastcall Get_OutputBlockSize();
	__property Crtypes::TCardinalArray User = {read=FUser};
	__property int UserSize = {read=FUserSize, nodefault};
	
public:
	__fastcall virtual TSymmetricAlgorithm();
	__fastcall virtual ~TSymmetricAlgorithm();
	Crcryptotransformintf::_di_ICryptoTransform __fastcall CreateEncryptor();
	Crcryptotransformintf::_di_ICryptoTransform __fastcall CreateDecryptor();
	void __fastcall CodeBuffer(const char * Buffer, int BufferSize);
	virtual void __fastcall GenerateIV();
	virtual void __fastcall GenerateKey();
	void __fastcall ClearIV();
	void __fastcall SetIV(char * Value, int Offset, int Count);
	void __fastcall SetReceivedTag(char * Value, int Offset, int Count);
	void __fastcall EncodeBuffer(void * Source, void * Dest, int DataSize)/* overload */;
	void __fastcall EncodeBuffer(char * Source, int SrcOffset, int DataSize, char * Dest, int DstOffset)/* overload */;
	void __fastcall DecodeBuffer(void * Source, void * Dest, int DataSize)/* overload */;
	void __fastcall DecodeBuffer(char * Source, int SrcOffset, int DataSize, char * Dest, int DstOffset)/* overload */;
	bool __fastcall ValidKeySize(int BitLength);
	void __fastcall TransformBlock(const System::DynamicArray<System::Byte> Data, int Offset, int Count)/* overload */;
	int __fastcall TransformBlock(const char * InputBuffer, int InputOffset, int InputCount, const char * OutputBuffer, int OutputOffset)/* overload */;
	void __fastcall TransformFinalBlock(const System::DynamicArray<System::Byte> InputBuffer, int InputOffset, int InputCount);
	__property bool Initialized = {read=FInitialized, nodefault};
	__property TCipherMode Mode = {read=FMode, write=SetMode, nodefault};
	__property TPaddingMode Padding = {read=FPadding, write=FPadding, nodefault};
	__property int KeySize = {read=FKeySize, nodefault};
	__property TKeySizes* LegalKeySizes = {read=FLegalKeySizes};
	__property int BlockSize = {read=FBlockSize, nodefault};
	__property System::DynamicArray<System::Byte> Key = {write=SetKey};
	__property System::DynamicArray<System::Byte> IV = {write=Set_IV};
	__property System::DynamicArray<System::Byte> AAD = {read=FAAD, write=FAAD};
	__property System::DynamicArray<System::Byte> Tag = {read=GetTag};
private:
	void *__ICryptoTransform;	// Crcryptotransformintf::ICryptoTransform 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {AE5FFE08-1D09-4452-91C8-122B05513FEB}
	operator Crcryptotransformintf::_di_ICryptoTransform()
	{
		Crcryptotransformintf::_di_ICryptoTransform intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator Crcryptotransformintf::ICryptoTransform*(void) { return (Crcryptotransformintf::ICryptoTransform*)&__ICryptoTransform; }
	#endif
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crsymmetricalgorithm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRSYMMETRICALGORITHM)
using namespace Crsymmetricalgorithm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrsymmetricalgorithmHPP
