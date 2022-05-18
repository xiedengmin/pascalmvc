// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteParserVirtual.pas' rev: 35.00 (Windows)

#ifndef LiteparservirtualHPP
#define LiteparservirtualHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>

//-- user supplied -----------------------------------------------------------

namespace Liteparservirtual
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TLiteParser;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TLiteParser : public Crparser::TSQLParser
{
	typedef Crparser::TSQLParser inherited;
	
protected:
	virtual void __fastcall ToRightQuote(System::WideChar RightQuote);
	virtual bool __fastcall IsIdentQuote(System::WideChar Ch);
	virtual void __fastcall InitParser();
public:
	/* TParser.Create */ inline __fastcall virtual TLiteParser(const System::UnicodeString Text)/* overload */ : Crparser::TSQLParser(Text) { }
	/* TParser.Create */ inline __fastcall TLiteParser(System::Classes::TStream* const Stream, Clrclasses::Encoding* AEncoding)/* overload */ : Crparser::TSQLParser(Stream, AEncoding) { }
	/* TParser.Create */ inline __fastcall virtual TLiteParser(System::Classes::TStream* const Stream, __int64 ASize, Clrclasses::Encoding* AEncoding)/* overload */ : Crparser::TSQLParser(Stream, ASize, AEncoding) { }
	/* TParser.Destroy */ inline __fastcall virtual ~TLiteParser() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word lxLiteFirst = System::Word(0x3e8);
static const System::Word lxALTER = System::Word(0x3e8);
static const System::Word lxCREATE = System::Word(0x3e9);
static const System::Word lxPRAGMA = System::Word(0x3ea);
static const System::Word lxTRIGGER = System::Word(0x3eb);
static const System::Word lxDROP = System::Word(0x3ec);
static const System::Word lxDEFAULT = System::Word(0x3ed);
static const System::Word lxINDEX = System::Word(0x3ee);
static const System::Word lxFOREIGN = System::Word(0x3ef);
static const System::Word lxTABLE = System::Word(0x3f0);
static const System::Word lxFIELD = System::Word(0x3f1);
static const System::Word lxADD = System::Word(0x3f2);
static const System::Word lxUNIQUE = System::Word(0x3f3);
static const System::Word lxDATE = System::Word(0x3f4);
static const System::Word lxTYPE = System::Word(0x3f5);
static const System::Word lxCHECK = System::Word(0x3f6);
static const System::Word lxUSER = System::Word(0x3f7);
extern DELPHI_PACKAGE Crparser::TLexemList* LiteKeywordLexems;
extern DELPHI_PACKAGE Crparser::TClauseList* LiteClauses;
}	/* namespace Liteparservirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITEPARSERVIRTUAL)
using namespace Liteparservirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LiteparservirtualHPP
