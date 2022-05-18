// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CLRClasses.pas' rev: 34.00 (Windows)

#ifndef ClrclassesHPP
#define ClrclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>

//-- user supplied -----------------------------------------------------------

namespace Clrclasses
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS BitConverter;
class DELPHICLASS Marshal;
class DELPHICLASS Encoding;
class DELPHICLASS ANSIEncoding;
class DELPHICLASS UnicodeEncoding;
class DELPHICLASS BigEndianUnicodeEncoding;
class DELPHICLASS UTF8Encoding;
class DELPHICLASS CodePageEncoding;
class DELPHICLASS AnsiStringBuilder;
class DELPHICLASS WideStringBuilder;
class DELPHICLASS Buffer;
class DELPHICLASS MemoryStream;
class DELPHICLASS TScCancellationToken;
class DELPHICLASS ArgumentException;
class DELPHICLASS NotSupportedException;
class DELPHICLASS AggregateException;
class DELPHICLASS InvalidDataException;
class DELPHICLASS InvalidOperationException;
class DELPHICLASS JSONException;
class DELPHICLASS OperationCanceledException;
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<char> TAnsiCharArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION BitConverter : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod System::DynamicArray<System::Byte> __fastcall GetBytes(System::Word value)/* overload */;
	__classmethod System::DynamicArray<System::Byte> __fastcall GetBytes(unsigned value)/* overload */;
	__classmethod System::DynamicArray<System::Byte> __fastcall GetBytes(__int64 value)/* overload */;
	__classmethod System::DynamicArray<System::Byte> __fastcall GetBytes(double value)/* overload */;
	__classmethod System::DynamicArray<System::Byte> __fastcall GetBytes(float value)/* overload */;
	__classmethod double __fastcall Int64BitsToDouble(__int64 value);
	__classmethod __int64 __fastcall DoubleToInt64Bits(double value);
	__classmethod double __fastcall ToDouble(const System::DynamicArray<System::Byte> value, int startIndex)/* overload */;
	__classmethod double __fastcall ToDouble(const char * value, int startIndex)/* overload */;
	__classmethod float __fastcall ToSingle(const System::DynamicArray<System::Byte> value, int startIndex);
	__classmethod short __fastcall ToInt16(const System::DynamicArray<System::Byte> value, int startIndex);
	__classmethod System::Word __fastcall ToUInt16(const System::DynamicArray<System::Byte> value, int startIndex)/* overload */;
	__classmethod System::Word __fastcall ToUInt16(const char * value, int startIndex)/* overload */;
	__classmethod int __fastcall ToInt32(const System::DynamicArray<System::Byte> value, int startIndex)/* overload */;
	__classmethod int __fastcall ToInt32(const char * value, int startIndex)/* overload */;
	__classmethod unsigned __fastcall ToUInt32(const System::DynamicArray<System::Byte> value, int startIndex);
	__classmethod __int64 __fastcall ToInt64(const System::DynamicArray<System::Byte> value, int startIndex)/* overload */;
	__classmethod __int64 __fastcall ToInt64(const char * value, int startIndex)/* overload */;
	__classmethod bool __fastcall IsLittleEndian();
public:
	/* TObject.Create */ inline __fastcall BitConverter() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~BitConverter() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION Marshal : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void * __fastcall AllocHGlobal(NativeInt cb);
	__classmethod void * __fastcall ReallocHGlobal(void * pv, NativeInt cb);
	__classmethod void __fastcall FreeHGlobal(void * hglobal);
	__classmethod void __fastcall FreeCoTaskMem(void * ptr);
	__classmethod System::Int8 __fastcall ReadInt8(void * ptr)/* overload */;
	__classmethod System::Int8 __fastcall ReadInt8(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteInt8(void * ptr, System::Int8 val)/* overload */;
	__classmethod void __fastcall WriteInt8(void * ptr, int ofs, System::Int8 val)/* overload */;
	__classmethod System::Byte __fastcall ReadByte(void * ptr)/* overload */;
	__classmethod System::Byte __fastcall ReadByte(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteByte(void * ptr, System::Byte val)/* overload */;
	__classmethod void __fastcall WriteByte(void * ptr, int ofs, System::Byte val)/* overload */;
	__classmethod short __fastcall ReadInt16(void * ptr)/* overload */;
	__classmethod short __fastcall ReadInt16(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteInt16(void * ptr, short val)/* overload */;
	__classmethod void __fastcall WriteInt16(void * ptr, int ofs, short val)/* overload */;
	__classmethod System::Word __fastcall ReadUInt16(void * ptr)/* overload */;
	__classmethod System::Word __fastcall ReadUInt16(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteUInt16(void * ptr, System::Word val)/* overload */;
	__classmethod void __fastcall WriteUInt16(void * ptr, int ofs, System::Word val)/* overload */;
	__classmethod int __fastcall ReadInt32(void * ptr)/* overload */;
	__classmethod int __fastcall ReadInt32(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteInt32(void * ptr, int val)/* overload */;
	__classmethod void __fastcall WriteInt32(void * ptr, int ofs, int val)/* overload */;
	__classmethod unsigned __fastcall ReadUInt32(void * ptr)/* overload */;
	__classmethod unsigned __fastcall ReadUInt32(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteUInt32(void * ptr, unsigned val)/* overload */;
	__classmethod void __fastcall WriteUInt32(void * ptr, int ofs, unsigned val)/* overload */;
	__classmethod __int64 __fastcall ReadInt64(void * ptr)/* overload */;
	__classmethod __int64 __fastcall ReadInt64(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteInt64(void * ptr, __int64 val)/* overload */;
	__classmethod void __fastcall WriteInt64(void * ptr, int ofs, __int64 val)/* overload */;
	__classmethod unsigned __int64 __fastcall ReadUInt64(void * ptr)/* overload */;
	__classmethod unsigned __int64 __fastcall ReadUInt64(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteUInt64(void * ptr, unsigned __int64 val)/* overload */;
	__classmethod void __fastcall WriteUInt64(void * ptr, int ofs, unsigned __int64 val)/* overload */;
	__classmethod NativeInt __fastcall ReadNativeInt(void * ptr)/* overload */;
	__classmethod NativeInt __fastcall ReadNativeInt(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteNativeInt(void * ptr, NativeInt val)/* overload */;
	__classmethod void __fastcall WriteNativeInt(void * ptr, int ofs, NativeInt val)/* overload */;
	__classmethod NativeUInt __fastcall ReadNativeUInt(void * ptr)/* overload */;
	__classmethod NativeUInt __fastcall ReadNativeUInt(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteNativeUInt(void * ptr, NativeUInt val)/* overload */;
	__classmethod void __fastcall WriteNativeUInt(void * ptr, int ofs, NativeUInt val)/* overload */;
	__classmethod double __fastcall ReadDouble(void * ptr)/* overload */;
	__classmethod double __fastcall ReadDouble(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteDouble(void * ptr, double val)/* overload */;
	__classmethod void __fastcall WriteDouble(void * ptr, int ofs, double val)/* overload */;
	__classmethod void * __fastcall ReadIntPtr(void * ptr)/* overload */;
	__classmethod void * __fastcall ReadIntPtr(void * ptr, int ofs)/* overload */;
	__classmethod void __fastcall WriteIntPtr(void * ptr, void * val)/* overload */;
	__classmethod void __fastcall WriteIntPtr(void * ptr, int ofs, void * val)/* overload */;
	__classmethod System::AnsiString __fastcall PtrToStringAnsi(void * ptr)/* overload */;
	__classmethod System::AnsiString __fastcall PtrToStringAnsi(void * ptr, int len)/* overload */;
	__classmethod System::WideString __fastcall PtrToStringUni(void * ptr)/* overload */;
	__classmethod System::WideString __fastcall PtrToStringUni(void * ptr, int len)/* overload */;
	__classmethod void * __fastcall StringToHGlobalAnsi(const System::AnsiString s);
	__classmethod void * __fastcall StringToHGlobalUni(const System::WideString s);
	__classmethod void __fastcall Copy(const System::DynamicArray<System::Byte> source, int startIndex, void * destination, int length)/* overload */;
	__classmethod void __fastcall Copy(void * source, const System::DynamicArray<System::Byte> destination, int startIndex, int length)/* overload */;
	__classmethod void * __fastcall GetIUnknownForObject(System::TInterfacedObject* o);
	__classmethod int __fastcall AddRef(void * pUnk);
	__classmethod int __fastcall Release(void * pUnk);
public:
	/* TObject.Create */ inline __fastcall Marshal() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~Marshal() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION Encoding : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FIsSingleByte;
	
public:
	__classmethod Encoding* __fastcall Default();
	__classmethod Encoding* __fastcall ASCII();
	__classmethod Encoding* __fastcall ANSI();
	__classmethod Encoding* __fastcall Unicode();
	__classmethod Encoding* __fastcall BigEndianUnicode();
	__classmethod Encoding* __fastcall UTF8();
	__classmethod Encoding* __fastcall GetEncoding(unsigned codepage);
	__classmethod System::DynamicArray<System::Byte> __fastcall Convert(Encoding* srcEncoding, Encoding* dstEncoding, const System::DynamicArray<System::Byte> bytes)/* overload */;
	__classmethod System::DynamicArray<System::Byte> __fastcall Convert(Encoding* srcEncoding, Encoding* dstEncoding, const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::AnsiString chars) = 0 /* overload */;
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex) = 0 /* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::WideString chars) = 0 /* overload */;
	virtual int __fastcall GetBytes(const System::WideString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex) = 0 /* overload */;
	System::UnicodeString __fastcall GetString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	System::UnicodeString __fastcall GetString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes) = 0 /* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes, int index, int count) = 0 /* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes) = 0 /* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes, int index, int count) = 0 /* overload */;
	virtual int __fastcall GetMaxByteCount(int charCount);
	virtual int __fastcall GetMaxCharCount(int byteCount);
	__property bool IsSingleByte = {read=FIsSingleByte, nodefault};
public:
	/* TObject.Create */ inline __fastcall Encoding() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~Encoding() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION ANSIEncoding : public Encoding
{
	typedef Encoding inherited;
	
public:
	__fastcall ANSIEncoding();
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::AnsiString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::WideString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::WideString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~ANSIEncoding() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION UnicodeEncoding : public Encoding
{
	typedef Encoding inherited;
	
public:
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::AnsiString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::WideString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::WideString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual int __fastcall GetMaxByteCount(int charCount);
	virtual int __fastcall GetMaxCharCount(int byteCount);
public:
	/* TObject.Create */ inline __fastcall UnicodeEncoding() : Encoding() { }
	/* TObject.Destroy */ inline __fastcall virtual ~UnicodeEncoding() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION BigEndianUnicodeEncoding : public UnicodeEncoding
{
	typedef UnicodeEncoding inherited;
	
public:
	virtual int __fastcall GetBytes(const System::WideString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
public:
	/* TObject.Create */ inline __fastcall BigEndianUnicodeEncoding() : UnicodeEncoding() { }
	/* TObject.Destroy */ inline __fastcall virtual ~BigEndianUnicodeEncoding() { }
	
	/* Hoisted overloads: */
	
public:
	inline System::DynamicArray<System::Byte> __fastcall  GetBytes(const System::AnsiString chars){ return UnicodeEncoding::GetBytes(chars); }
	inline int __fastcall  GetBytes(const System::AnsiString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex){ return UnicodeEncoding::GetBytes(chars, charIndex, charCount, bytes, byteIndex); }
	inline System::DynamicArray<System::Byte> __fastcall  GetBytes(const System::WideString chars){ return UnicodeEncoding::GetBytes(chars); }
	inline System::WideString __fastcall  GetWideString(const System::DynamicArray<System::Byte> bytes){ return UnicodeEncoding::GetWideString(bytes); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION UTF8Encoding : public Encoding
{
	typedef Encoding inherited;
	
public:
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::AnsiString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::WideString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::WideString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual int __fastcall GetMaxByteCount(int charCount);
public:
	/* TObject.Create */ inline __fastcall UTF8Encoding() : Encoding() { }
	/* TObject.Destroy */ inline __fastcall virtual ~UTF8Encoding() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION CodePageEncoding : public Encoding
{
	typedef Encoding inherited;
	
private:
	unsigned FCodePage;
	
public:
	__fastcall CodePageEncoding(unsigned CodePage);
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::AnsiString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::WideString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::WideString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	virtual System::AnsiString __fastcall GetAnsiString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~CodePageEncoding() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION AnsiStringBuilder : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	char operator[](int Index) { return this->Chars[Index]; }
	
private:
	char __fastcall GetChar(int Index);
	
protected:
	TAnsiCharArray FString;
	int FActualLength;
	void __fastcall SetActualLength(int Value);
	
public:
	__fastcall AnsiStringBuilder(int capacity)/* overload */;
	__fastcall AnsiStringBuilder(const System::AnsiString value, int capacity)/* overload */;
	void __fastcall Append(const System::AnsiString value)/* overload */;
	void __fastcall Append(const System::AnsiString value, const int startIndex, const int count)/* overload */;
	void __fastcall Append(const System::DynamicArray<System::Byte> value, const int startIndex, const int count)/* overload */;
	void __fastcall Append(char value)/* overload */;
	void __fastcall Append(char value, int repeatCount)/* overload */;
	void __fastcall Append(AnsiStringBuilder* value)/* overload */;
	void __fastcall Insert(int index, const System::AnsiString value)/* overload */;
	void __fastcall Replace(const System::AnsiString OldValue, const System::AnsiString NewValue);
	HIDESBASE System::AnsiString __fastcall ToString();
	__property int Length = {read=FActualLength, write=SetActualLength, nodefault};
	__property char Chars[int Index] = {read=GetChar/*, default*/};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~AnsiStringBuilder() { }
	
};

#pragma pack(pop)

typedef System::DynamicArray<System::WideChar> TWideCharArray;

typedef System::UnicodeString WString;

#pragma pack(push,4)
class PASCALIMPLEMENTATION WideStringBuilder : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::WideChar operator[](int Index) { return this->Chars[Index]; }
	
private:
	System::WideChar __fastcall GetChar(int Index);
	
protected:
	TWideCharArray FString;
	int FActualLength;
	void __fastcall SetActualLength(int Value);
	
public:
	__fastcall WideStringBuilder(int capacity)/* overload */;
	__fastcall WideStringBuilder(const System::UnicodeString value, int capacity)/* overload */;
	void __fastcall Append(const System::UnicodeString value)/* overload */;
	void __fastcall Append(const System::UnicodeString value, const int startIndex, const int count)/* overload */;
	void __fastcall Append(System::WideChar value)/* overload */;
	void __fastcall Append(System::WideChar value, int repeatCount)/* overload */;
	void __fastcall Append(WideStringBuilder* value)/* overload */;
	void __fastcall Insert(int index, const System::UnicodeString value)/* overload */;
	void __fastcall Replace(const System::UnicodeString OldValue, const System::UnicodeString NewValue);
	virtual System::UnicodeString __fastcall ToString();
	__property int Length = {read=FActualLength, write=SetActualLength, nodefault};
	__property System::WideChar Chars[int Index] = {read=GetChar/*, default*/};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~WideStringBuilder() { }
	
};

#pragma pack(pop)

typedef WideStringBuilder StringBuilder;

#pragma pack(push,4)
class PASCALIMPLEMENTATION Buffer : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall BlockCopy(const unsigned *src, const int src_High, int srcOffset, const unsigned *dst, const int dst_High, int dstOffset, int count)/* overload */;
	__classmethod void __fastcall BlockCopy(const unsigned *src, const int src_High, int srcOffset, const Crtypes::TCardinalArray dst, int dstOffset, int count)/* overload */;
	__classmethod void __fastcall BlockCopy(const unsigned *src, const int src_High, int srcOffset, const System::DynamicArray<System::Byte> dst, int dstOffset, int count)/* overload */;
	__classmethod void __fastcall BlockCopy(const Crtypes::TCardinalArray src, int srcOffset, const Crtypes::TCardinalArray dst, int dstOffset, int count)/* overload */;
	__classmethod void __fastcall BlockCopy(const Crtypes::TCardinalArray src, int srcOffset, const System::DynamicArray<System::Byte> dst, int dstOffset, int count)/* overload */;
	__classmethod void __fastcall BlockCopy(const System::DynamicArray<System::Byte> src, int srcOffset, const unsigned *dst, const int dst_High, int dstOffset, int count)/* overload */;
	__classmethod void __fastcall BlockCopy(const System::DynamicArray<System::Byte> src, int srcOffset, const Crtypes::TCardinalArray dst, int dstOffset, int count)/* overload */;
	__classmethod void __fastcall BlockCopy(const System::DynamicArray<System::Byte> src, int srcOffset, const System::DynamicArray<System::Byte> dst, int dstOffset, int count)/* overload */;
	__classmethod System::Byte __fastcall GetByte(const void * src, int Index);
	__classmethod void __fastcall SetByte(const void * src, int Index, System::Byte Value);
public:
	/* TObject.Create */ inline __fastcall Buffer() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~Buffer() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION MemoryStream : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::DynamicArray<System::Byte> FData;
	int FPosition;
	int FLength;
	
protected:
	void __fastcall SetPosition(const int Pos);
	
public:
	__fastcall MemoryStream(int Capacity);
	int __fastcall Seek(int Offset, System::Word Origin);
	int __fastcall Read(System::DynamicArray<System::Byte> &Buffer, int Offset, int Count)/* overload */;
	int __fastcall Read(char * Buffer, int Offset, int Count)/* overload */;
	void __fastcall Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count)/* overload */;
	void __fastcall Write(char * Buffer, int Offset, int Count)/* overload */;
	void __fastcall WriteByte(System::Byte Value);
	System::Byte __fastcall ReadByte();
	char * __fastcall GetBuffer();
	System::DynamicArray<System::Byte> __fastcall ToArray();
	void __fastcall Close();
	void __fastcall SetLength(int Value);
	__property int Length = {read=FLength, write=SetLength, nodefault};
	__property int Position = {read=FPosition, write=SetPosition, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~MemoryStream() { }
	
};

#pragma pack(pop)

typedef System::DynamicArray<System::TMethod> TMethodArray;

enum DECLSPEC_DENUM TScCancellationTokenState : unsigned char { ctsInited, ctsNotifying, ctsCanceled };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TScCancellationToken : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TScCancellationTokenState FState;
	System::Syncobjs::TEvent* FWaiter;
	System::Syncobjs::TCriticalSection* FLock;
	TMethodArray FOnCancelEventList;
	void __fastcall NotifyOnCancelEvents();
	
public:
	__fastcall TScCancellationToken();
	__fastcall virtual ~TScCancellationToken();
	void __fastcall ReInit();
	void __fastcall Cancel();
	bool __fastcall IsCancellationRequested();
	void __fastcall ThrowIfCancellationRequested();
	void __fastcall Delay(unsigned Timeout);
	bool __fastcall CanBeCanceled();
	void __fastcall Register(System::Classes::TThreadMethod Event);
	void __fastcall Unregister(System::Classes::TThreadMethod Event);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION ArgumentException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	__fastcall ArgumentException()/* overload */;
	__fastcall ArgumentException(const System::UnicodeString Msg)/* overload */;
public:
	/* Exception.CreateFmt */ inline __fastcall ArgumentException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ArgumentException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ArgumentException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ArgumentException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ArgumentException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ArgumentException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ArgumentException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ArgumentException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ArgumentException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ArgumentException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ArgumentException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ArgumentException() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION NotSupportedException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	__fastcall NotSupportedException()/* overload */;
	__fastcall NotSupportedException(const System::UnicodeString Msg)/* overload */;
public:
	/* Exception.CreateFmt */ inline __fastcall NotSupportedException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall NotSupportedException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall NotSupportedException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall NotSupportedException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall NotSupportedException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall NotSupportedException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall NotSupportedException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall NotSupportedException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall NotSupportedException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall NotSupportedException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall NotSupportedException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~NotSupportedException() { }
	
};

#pragma pack(pop)

typedef System::DynamicArray<System::Sysutils::Exception*> ExceptionArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION AggregateException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
private:
	ExceptionArray FExceptions;
	
public:
	__fastcall AggregateException(const ExceptionArray AExceptions)/* overload */;
	__fastcall AggregateException(const System::UnicodeString Msg)/* overload */;
	__fastcall virtual ~AggregateException();
	__property ExceptionArray Exceptions = {read=FExceptions};
public:
	/* Exception.CreateFmt */ inline __fastcall AggregateException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall AggregateException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall AggregateException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall AggregateException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall AggregateException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall AggregateException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall AggregateException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall AggregateException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall AggregateException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall AggregateException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall AggregateException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION InvalidDataException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall InvalidDataException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall InvalidDataException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall InvalidDataException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall InvalidDataException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall InvalidDataException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall InvalidDataException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall InvalidDataException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall InvalidDataException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall InvalidDataException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall InvalidDataException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall InvalidDataException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall InvalidDataException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~InvalidDataException() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION InvalidOperationException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall InvalidOperationException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall InvalidOperationException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall InvalidOperationException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall InvalidOperationException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall InvalidOperationException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall InvalidOperationException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall InvalidOperationException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall InvalidOperationException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall InvalidOperationException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall InvalidOperationException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall InvalidOperationException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall InvalidOperationException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~InvalidOperationException() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION JSONException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall JSONException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall JSONException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall JSONException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall JSONException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall JSONException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall JSONException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall JSONException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall JSONException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall JSONException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall JSONException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall JSONException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall JSONException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~JSONException() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION OperationCanceledException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall OperationCanceledException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall OperationCanceledException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall OperationCanceledException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall OperationCanceledException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall OperationCanceledException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall OperationCanceledException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall OperationCanceledException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall OperationCanceledException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall OperationCanceledException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall OperationCanceledException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall OperationCanceledException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall OperationCanceledException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~OperationCanceledException() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Clrclasses */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CLRCLASSES)
using namespace Clrclasses;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ClrclassesHPP
