// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRXml.pas' rev: 34.00 (Windows)

#ifndef CrxmlHPP
#define CrxmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.AnsiStrings.hpp>
#include <System.Contnrs.hpp>
#include <CRTypes.hpp>
#include <CLRClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crxml
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS StreamWriter;
class DELPHICLASS XmlException;
struct TAttribute;
class DELPHICLASS XmlTextReader;
class DELPHICLASS XmlTextWriter;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION StreamWriter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStream* FStream;
	bool FReleaseStream;
	Clrclasses::Encoding* FEncoding;
	
public:
	__fastcall StreamWriter(const System::UnicodeString path, bool Append)/* overload */;
	__fastcall StreamWriter(System::Classes::TStream* output, Clrclasses::Encoding* aEncoding)/* overload */;
	__fastcall virtual ~StreamWriter();
	void __fastcall Close();
	void __fastcall Flush();
	void __fastcall Write(const System::WideString value);
	void __fastcall WriteLine(const System::WideString value);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION XmlException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall XmlException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall XmlException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall XmlException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall XmlException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall XmlException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall XmlException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall XmlException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall XmlException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall XmlException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall XmlException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall XmlException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall XmlException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~XmlException() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM XmlNodeType : unsigned char { ntNone, ntStartElement, ntEndElement, ntClosedElement, ntAttribute, ntComment, ntDeclaration, ntDocumentType, ntText };

enum DECLSPEC_DENUM XmlReadState : unsigned char { Initial, Interactive, Error, EndOfFile, Closed };

typedef System::DynamicArray<System::DynamicArray<System::Byte> > TBytesArray;

struct DECLSPEC_DRECORD TAttribute
{
public:
	System::DynamicArray<System::Byte> Prefix;
	System::DynamicArray<System::Byte> Name;
	System::DynamicArray<System::Byte> Value;
};


typedef System::DynamicArray<TAttribute> TAttributeArray;

class PASCALIMPLEMENTATION XmlTextReader : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TBytesArray FBlocks;
	System::Classes::TStream* FStream;
	System::DynamicArray<System::Byte> FTmpBlock;
	__int64 FStreamPosition;
	int FBlockSize;
	__int64 FFullSize;
	int FMaxNumBlock;
	int FBlockCount;
	int FCurPosition;
	int FActualPosition;
	int FLastBlockSize;
	System::DynamicArray<System::Byte> FPrefix;
	System::DynamicArray<System::Byte> FValue;
	System::DynamicArray<System::Byte> FName;
	System::DynamicArray<System::Byte> FCurrElementName;
	XmlNodeType FNodeType;
	TAttributeArray FAttributes;
	int FAttributeCount;
	XmlReadState FState;
	bool FDecodeHTMLEntities;
	System::UnicodeString __fastcall GetName();
	System::UnicodeString __fastcall GetFullName();
	System::UnicodeString __fastcall GetPrefix();
	System::UnicodeString __fastcall GetValue();
	System::AnsiString __fastcall GetAnsiValue();
	System::WideString __fastcall GetWideValue();
	int __fastcall GetDepth();
	bool __fastcall GetHasAttributes();
	System::UnicodeString __fastcall GetAttributeName(int Index);
	System::UnicodeString __fastcall GetAttributePrefix(int Index);
	System::UnicodeString __fastcall GetAttributeValue(int Index);
	int __fastcall GetAttributeCount();
	HIDESBASE void __fastcall InitInstance();
	bool __fastcall GetEof();
	bool __fastcall LoadNextBlock(int Count);
	void __fastcall FreeLastBlocks(int Count);
	bool __fastcall ReadTo(const System::AnsiString SubStr, /* out */ System::DynamicArray<System::Byte> &ResultValue, const int AdvLenth = 0x0);
	bool __fastcall IsToken(const System::AnsiString SubStr);
	char __fastcall GetNextSymbol();
	bool __fastcall MoveTo(const System::AnsiString Lexem);
	
protected:
	int __fastcall GetAttributeIndex(const System::UnicodeString Name);
	void __fastcall ParseXMLNodeAttributes(const System::DynamicArray<System::Byte> Node);
	
public:
	__fastcall XmlTextReader(System::Classes::TStream* Stream, bool DecodeHTMLEntities)/* overload */;
	__fastcall XmlTextReader(const System::UnicodeString Str, bool DecodeHTMLEntities)/* overload */;
	__fastcall XmlTextReader(const System::DynamicArray<System::Byte> Binary, bool DecodeHTMLEntities)/* overload */;
	__fastcall virtual ~XmlTextReader();
	bool __fastcall Read();
	void __fastcall MoveToAttribute(int Index)/* overload */;
	bool __fastcall MoveToAttribute(const System::UnicodeString Name)/* overload */;
	virtual System::UnicodeString __fastcall Items(const int AttrIndex)/* overload */;
	System::UnicodeString __fastcall Items(const System::UnicodeString AttrName)/* overload */;
	__property System::UnicodeString Name = {read=GetName};
	__property System::UnicodeString Prefix = {read=GetPrefix};
	__property System::UnicodeString FullName = {read=GetFullName};
	__property System::UnicodeString Value = {read=GetValue};
	__property System::AnsiString AnsiValue = {read=GetAnsiValue};
	__property System::WideString WideValue = {read=GetWideValue};
	__property XmlNodeType NodeType = {read=FNodeType, nodefault};
	__property int Depth = {read=GetDepth, nodefault};
	__property XmlReadState ReadState = {read=FState, nodefault};
	__property bool Eof = {read=GetEof, nodefault};
	__property bool HasAttributes = {read=GetHasAttributes, nodefault};
	__property System::UnicodeString AttributeNames[int Index] = {read=GetAttributeName};
	__property System::UnicodeString AttributePrefixes[int Index] = {read=GetAttributePrefix};
	__property System::UnicodeString AttributeValues[int Index] = {read=GetAttributeValue};
	__property int AttributeCount = {read=GetAttributeCount, nodefault};
	__property TBytesArray Blocks = {read=FBlocks};
};


enum DECLSPEC_DENUM XmlFormatting : unsigned char { fmtNone, fmtIndented };

enum DECLSPEC_DENUM XmlWriteState : unsigned char { wsAttribute, wsClosed, wsContent, wsElement, wsStart };

#pragma pack(push,4)
class PASCALIMPLEMENTATION XmlTextWriter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Clrclasses::WideStringBuilder* FText;
	XmlFormatting FFormatting;
	int FIndentation;
	System::WideChar FIndentChar;
	System::WideChar FQuoteChar;
	XmlWriteState FWriteState;
	int FDepth;
	System::Contnrs::TStack* FPosStack;
	System::Classes::TStringList* FTagStack;
	StreamWriter* FWriter;
	System::WideString __fastcall IndentStr();
	System::UnicodeString __fastcall PopTagName();
	void __fastcall PushTagName(const System::UnicodeString TagName);
	void __fastcall InternalCloseStartTag();
	
protected:
	void __fastcall InternalWriteStartElement(const System::UnicodeString Prefix, const System::UnicodeString LocalName, const System::UnicodeString ns);
	void __fastcall InternalWriteElementString(const System::UnicodeString LocalName, const System::UnicodeString ns, const System::WideString Value);
	void __fastcall InternalWriteAttributeString(const System::UnicodeString Prefix, const System::UnicodeString LocalName, const System::UnicodeString ns, const System::WideString Value);
	void __fastcall InternalWriteEndElement();
	void __fastcall FlushData();
	
public:
	__fastcall XmlTextWriter(StreamWriter* w);
	__fastcall virtual ~XmlTextWriter();
	void __fastcall WriteStartElement(const System::UnicodeString LocalName)/* overload */;
	void __fastcall WriteStartElement(const System::UnicodeString Prefix, const System::UnicodeString LocalName, const System::UnicodeString ns)/* overload */;
	void __fastcall WriteStartElement(const System::UnicodeString LocalName, const System::UnicodeString ns)/* overload */;
	void __fastcall WriteEndElement();
	void __fastcall WriteFullEndElement();
	void __fastcall WriteString(const System::WideString Text)/* overload */;
	void __fastcall WriteElementString(const System::UnicodeString LocalName, const System::WideString Value)/* overload */;
	void __fastcall WriteElementString(const System::UnicodeString LocalName, const System::UnicodeString ns, const System::WideString Value)/* overload */;
	void __fastcall WriteAttributeString(const System::UnicodeString LocalName, const System::WideString Value)/* overload */;
	void __fastcall WriteAttributeString(const System::UnicodeString Prefix, const System::UnicodeString LocalName, const System::UnicodeString ns, const System::WideString Value)/* overload */;
	void __fastcall Close();
	__property XmlFormatting Formatting = {read=FFormatting, write=FFormatting, nodefault};
	__property int Indentation = {read=FIndentation, write=FIndentation, nodefault};
	__property System::WideChar IndentChar = {read=FIndentChar, write=FIndentChar, nodefault};
	__property System::WideChar QuoteChar = {read=FQuoteChar, write=FQuoteChar, nodefault};
	__property XmlWriteState WriteState = {read=FWriteState, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::DynamicArray<System::Byte> __fastcall XMLDecode(const System::DynamicArray<System::Byte> Value, bool DecodeHTMLEntities = true);
}	/* namespace Crxml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRXML)
using namespace Crxml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrxmlHPP
