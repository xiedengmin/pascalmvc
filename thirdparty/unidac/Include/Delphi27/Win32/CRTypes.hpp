// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRTypes.pas' rev: 34.00 (Windows)

#ifndef CrtypesHPP
#define CrtypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.Variants.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crtypes
{
//-- forward type declarations -----------------------------------------------
__interface DELPHIINTERFACE IScAsyncResult;
typedef System::DelphiInterface<IScAsyncResult> _di_IScAsyncResult;
class DELPHICLASS TCRObjectList;
struct TIntValueItem;
class DELPHICLASS TIntValueStringList;
struct TStrValueItem;
class DELPHICLASS TStrValueStringList;
//-- type declarations -------------------------------------------------------
typedef int Int;

typedef short Int16;

typedef int Int32;

typedef System::Word UInt16;

typedef unsigned UInt32;

typedef System::Int8 SByte;

typedef void * IntPtr;

typedef void * *PIntPtr;

typedef void * MulticastDelegate;

typedef System::DynamicArray<System::Byte> TValueBuffer;

typedef char * TValueArr;

typedef char * PAChar;

typedef System::WideChar * PWChar;

typedef System::DynamicArray<System::UnicodeString> TStringArray;

typedef System::DynamicArray<System::Variant> TVariantArray;

typedef TVariantArray *PVariantArray;

typedef System::DynamicArray<System::Byte> TByteArr;

typedef System::DynamicArray<int> TIntArr;

typedef System::DynamicArray<System::Word> TWordArr;

typedef System::DynamicArray<unsigned> TLongWordArr;

typedef System::StaticArray<unsigned, 1044> TCardinalConstArray;

typedef TCardinalConstArray *TCardinalArray;

typedef System::StaticArray<unsigned __int64, 1044> TUInt64ConstArray;

typedef TUInt64ConstArray *TUInt64Array;

__interface  INTERFACE_UUID("{0426C822-A1FE-4760-AB89-60B6CB2FE1F3}") IScAsyncResult  : public System::IInterface 
{
	virtual System::TObject* __fastcall get_AsyncState() = 0 ;
	virtual System::Syncobjs::TEvent* __fastcall get_AsyncWaitHandle() = 0 ;
	virtual bool __fastcall get_IsCompleted() = 0 ;
	__property System::TObject* AsyncState = {read=get_AsyncState};
	__property System::Syncobjs::TEvent* AsyncWaitHandle = {read=get_AsyncWaitHandle};
	__property bool IsCompleted = {read=get_IsCompleted};
};

typedef void __fastcall (__closure *AsyncCallback)(_di_IScAsyncResult Result);

typedef System::Classes::TList TCRList;

typedef System::Classes::TThreadList TCRThreadList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRObjectList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
protected:
	virtual void __fastcall Notify(void * Instance, System::Classes::TListNotification Action);
public:
	/* TList.Destroy */ inline __fastcall virtual ~TCRObjectList() { }
	
public:
	/* TObject.Create */ inline __fastcall TCRObjectList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TMachineType : unsigned char { mtUnknown, mtAM33, mtAMD64, mtARM, mtEBC, mtI386, mtIA64, mtM32R, mtMIPS16, mtMIPSFPU, mtMIPSFPU16, mtPOWERPC, mtPOWERPCFP, mtR4000, mtSH3, mtSH3DSP, mtSH4, mtSH5, mtTHUMB, mtWCEMIPSV2 };

struct DECLSPEC_DRECORD TIntValueItem
{
public:
	System::UnicodeString Key;
	int Value;
};


typedef TIntValueItem *PIntValueItem;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TIntValueStringList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString operator[](int Index) { return this->Keys[Index]; }
	
private:
	System::Classes::TList* FCodeStrings;
	bool FSorted;
	System::UnicodeString __fastcall GetKey(int Index);
	int __fastcall GetValue(int Index);
	void __fastcall SetValue(int Index, const int Value);
	int __fastcall GetCount();
	
protected:
	virtual void __fastcall ListChanged();
	bool __fastcall Find(const System::UnicodeString Key, /* out */ int &Index);
	
public:
	__fastcall virtual TIntValueStringList();
	__fastcall virtual ~TIntValueStringList();
	void __fastcall Assign(TIntValueStringList* Source);
	int __fastcall Add(const System::UnicodeString S, int Code);
	void __fastcall Insert(int Index, const System::UnicodeString Key, int Value);
	void __fastcall Delete(int Index);
	void __fastcall Clear();
	int __fastcall IndexOf(const System::UnicodeString Key);
	bool __fastcall TryGetValue(const System::UnicodeString Key, /* out */ int &Value);
	void __fastcall Sort();
	__property System::UnicodeString Keys[int Index] = {read=GetKey/*, default*/};
	__property int Values[int Index] = {read=GetValue, write=SetValue};
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TStrValueItem
{
public:
	System::UnicodeString Key;
	System::UnicodeString Value;
};


typedef TStrValueItem *PStrValueItem;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStrValueStringList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString operator[](const System::UnicodeString Key) { return this->Names[Key]; }
	
private:
	System::Classes::TList* FStrValueStrings;
	bool FSorted;
	
protected:
	System::UnicodeString __fastcall GetKey(int Index);
	System::UnicodeString __fastcall GetValue(int Index);
	void __fastcall SetValue(int Index, const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall GetName(const System::UnicodeString Key);
	virtual void __fastcall SetName(const System::UnicodeString Key, const System::UnicodeString Value);
	int __fastcall GetCount();
	bool __fastcall Find(const System::UnicodeString Key, /* out */ int &Index);
	virtual void __fastcall CheckNewValue(const System::UnicodeString Key, const System::UnicodeString Value);
	
public:
	__fastcall TStrValueStringList();
	__fastcall virtual ~TStrValueStringList();
	void __fastcall Assign(TStrValueStringList* Source);
	void __fastcall AddStrings(System::Classes::TStrings* Strings)/* overload */;
	void __fastcall AddStrings(TStrValueStringList* Strings)/* overload */;
	int __fastcall Add(const System::UnicodeString Key, const System::UnicodeString Value);
	void __fastcall Insert(int Index, const System::UnicodeString Key, const System::UnicodeString Value);
	void __fastcall Delete(int Index);
	void __fastcall Clear();
	void __fastcall Remove(const System::UnicodeString Key);
	int __fastcall IndexOf(const System::UnicodeString Key);
	bool __fastcall TryGetValue(const System::UnicodeString Key, /* out */ System::UnicodeString &Value);
	void __fastcall Sort();
	__property System::UnicodeString Keys[int Index] = {read=GetKey};
	__property System::UnicodeString Values[int Index] = {read=GetValue, write=SetValue};
	__property System::UnicodeString Names[const System::UnicodeString Key] = {read=GetName, write=SetName/*, default*/};
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TScOnProgressEvent)(System::TObject* Sender, __int64 Total, __int64 Current, bool &Cancel);

//-- var, const, procedure ---------------------------------------------------
#define EMPTY_GUID L"{00000000-0000-0000-0000-000000000000}"
static const System::Int8 pidDevartWinPlatforms = System::Int8(0x3);
static const System::Word pidDevartAllPlatforms = System::Word(0x94df);
static const System::Word varSharedObject = System::Word(0x4000);
static const System::Word varValueArrayRef = System::Word(0x600c);
static const System::Word varObjectArrayRef = System::Word(0x6049);
}	/* namespace Crtypes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRTYPES)
using namespace Crtypes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrtypesHPP
