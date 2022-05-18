// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRParser.pas' rev: 34.00 (Windows)

#ifndef CrparserHPP
#define CrparserHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.SyncObjs.hpp>
#include <CRTypes.hpp>
#include <CLRClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crparser
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TLexemList;
class DELPHICLASS TClauseList;
class DELPHICLASS TParser;
class DELPHICLASS TSQLParser;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TParserClass;

typedef System::TMetaClass* TSQLParserClass;

typedef System::Set<char, _DELPHI_SET_CHAR(0), _DELPHI_SET_CHAR(255)> TCharSet;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLexemList : public Crtypes::TIntValueStringList
{
	typedef Crtypes::TIntValueStringList inherited;
	
private:
	Crtypes::TCRObjectList* FIndexes;
	int FMaxLength;
	System::Classes::TList* __fastcall GetIndex(int Len);
	
protected:
	System::Syncobjs::TCriticalSection* FLock;
	virtual void __fastcall ListChanged();
	void __fastcall CreateIndexes();
	void __fastcall FreeIndexes();
	
public:
	__fastcall virtual TLexemList();
	__fastcall virtual ~TLexemList();
	__property System::Classes::TList* Indexes[int Len] = {read=GetIndex};
	__property int MaxLength = {read=FMaxLength, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TClauseList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	int operator[](int Index) { return this->Items[Index]; }
	
protected:
	HIDESBASE int __fastcall Get(int Index);
	HIDESBASE void __fastcall Put(int Index, int Item);
	
public:
	HIDESBASE int __fastcall IndexOf(int Item);
	HIDESBASE int __fastcall Add(int Item);
	__property int Items[int Index] = {read=Get, write=Put/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TClauseList() { }
	
public:
	/* TObject.Create */ inline __fastcall TClauseList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TParser : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	bool FOmitBlank;
	bool FOmitComment;
	bool FOmitInlineComment;
	bool FUppered;
	bool FQuotedString;
	bool FAdvancedStringParsing;
	System::WideChar FDecSeparator;
	System::UnicodeString FCommentBegin;
	System::UnicodeString FCommentEnd;
	int OldPos;
	__int64 OldOldPos;
	__int64 FCurrLine;
	__int64 FPrevLine;
	int FCurrBegLine;
	int FPrevBegLine;
	int FSavedPos;
	int FLexemPos;
	int FLexemLength;
	int FToken;
	System::UnicodeString FLexem;
	Clrclasses::Encoding* FEncoding;
	System::Classes::TStream* FStream;
	__int64 FStartStreamPosition;
	__int64 FStreamSize;
	Crtypes::TIntValueStringList* FStoredBlocks;
	System::UnicodeString FCurrentBlock;
	int FBlockOffset;
	int FBlockSize;
	System::DynamicArray<System::Byte> FTmpBuffer;
	bool FAlternativeQuoting;
	bool FDollarQuoting;
	__int64 __fastcall GetCharSize(__int64 ByteSize);
	void __fastcall ReadNextBlock();
	System::WideChar __fastcall GetChar(int Index);
	System::WideChar __fastcall GetStoredChar(int Index);
	void __fastcall DecreaseBlockParameters();
	int TextLength;
	int Pos;
	__int64 StreamLength;
	__int64 Offset;
	TLexemList* FSymbolLexems;
	TLexemList* FKeywordLexems;
	bool FOmitKeywords;
	System::UnicodeString FDesiredLexem;
	bool FDesiredLexemFound;
	virtual void __fastcall InitParser();
	virtual bool __fastcall IsSymbol(System::WideChar Ch);
	virtual bool __fastcall IsAlpha(System::WideChar Ch);
	virtual bool __fastcall IsNumber(System::WideChar Ch);
	virtual bool __fastcall IsStringQuote(System::WideChar Ch);
	virtual void __fastcall ToRightQuoteP(System::WideChar RightQuote);
	virtual void __fastcall ToRightQuote(System::WideChar RightQuote);
	virtual bool __fastcall IsIdentQuote(System::WideChar Ch);
	virtual bool __fastcall IsInlineComment(System::WideChar Ch, int Pos);
	int __fastcall FindLexemIndex(const int LexemPos, const int LexemLength, TLexemList* Lexems);
	int __fastcall InternalGetNext();
	System::UnicodeString __fastcall CopyText(int Pos, int Count);
	void __fastcall AddToLexemArray(int Index, const int Len);
	__property System::WideChar Text[int Index] = {read=GetChar};
	__property bool AlternativeQuoting = {read=FAlternativeQuoting, write=FAlternativeQuoting, nodefault};
	__property bool DollarQuoting = {read=FDollarQuoting, write=FDollarQuoting, nodefault};
	
public:
	__fastcall virtual TParser(const System::UnicodeString Text)/* overload */;
	__fastcall TParser(System::Classes::TStream* const Stream, Clrclasses::Encoding* AEncoding)/* overload */;
	__fastcall virtual TParser(System::Classes::TStream* const Stream, __int64 ASize, Clrclasses::Encoding* AEncoding)/* overload */;
	__fastcall virtual ~TParser();
	System::Classes::TStream* __fastcall GetStream();
	void __fastcall SetText(const System::UnicodeString Text);
	virtual void __fastcall ToBegin();
	void __fastcall Back();
	virtual int __fastcall GetNext(/* out */ System::UnicodeString &Lexem);
	int __fastcall GetNextToken();
	int __fastcall ToLexem(const int Code, const bool SkipSubQueries = false)/* overload */;
	int __fastcall ToLexem(const int *Codes, const int Codes_High, const bool IsNestedQuery = false)/* overload */;
	bool __fastcall ToLexem(const System::UnicodeString Lexem)/* overload */;
	bool __fastcall IsSymbolCode(int Code);
	System::UnicodeString __fastcall GetSymbolByCode(int Code);
	__int64 __fastcall CurrPos();
	__int64 __fastcall PrevPos();
	__int64 __fastcall PrevPrevPos();
	__int64 __fastcall CurrLine();
	__int64 __fastcall PrevLine();
	__int64 __fastcall CurrCol();
	__int64 __fastcall PrevCol();
	__property __int64 StartStreamPosition = {read=FStartStreamPosition};
	__property int Token = {read=FToken, nodefault};
	__property System::UnicodeString Lexem = {read=FLexem};
	__property Clrclasses::Encoding* Encoding = {read=FEncoding};
	__property bool OmitBlank = {read=FOmitBlank, write=FOmitBlank, nodefault};
	__property bool OmitComment = {read=FOmitComment, write=FOmitComment, nodefault};
	__property bool OmitInlineComment = {read=FOmitInlineComment, write=FOmitInlineComment, nodefault};
	__property bool OmitKeywords = {read=FOmitKeywords, write=FOmitKeywords, nodefault};
	__property bool Uppered = {read=FUppered, write=FUppered, nodefault};
	__property bool QuotedString = {read=FQuotedString, write=FQuotedString, nodefault};
	__property bool AdvancedStringParsing = {read=FAdvancedStringParsing, write=FAdvancedStringParsing, nodefault};
	__property System::WideChar DecSeparator = {read=FDecSeparator, write=FDecSeparator, nodefault};
	__property System::UnicodeString CommentBegin = {read=FCommentBegin, write=FCommentBegin};
	__property System::UnicodeString CommentEnd = {read=FCommentEnd, write=FCommentEnd};
	__property TLexemList* SymbolLexems = {read=FSymbolLexems};
	__property TLexemList* KeywordLexems = {read=FKeywordLexems};
};


class PASCALIMPLEMENTATION TSQLParser : public TParser
{
	typedef TParser inherited;
	
protected:
	TClauseList* FClauses;
	virtual void __fastcall InitParser();
	virtual bool __fastcall IsAlpha(System::WideChar Ch);
	virtual bool __fastcall IsStringQuote(System::WideChar Ch);
	virtual bool __fastcall IsIdentQuote(System::WideChar Ch);
	virtual bool __fastcall IsInlineComment(System::WideChar Ch, int Pos);
	
public:
	bool __fastcall IsClauseLexem(int Code);
	int __fastcall CompareClauseLexems(const int Code1, const int Code2);
	virtual bool __fastcall IsMacroAllowed(int Code);
	virtual bool __fastcall IsSelectModifier(int Code);
	__classmethod virtual bool __fastcall IsNumericMacroNameAllowed();
	__classmethod virtual bool __fastcall IsFunctionOrConst(const System::UnicodeString UpperedName);
	__classmethod virtual bool __fastcall IsQuasiColumn(const System::UnicodeString UpperedName);
public:
	/* TParser.Create */ inline __fastcall virtual TSQLParser(const System::UnicodeString Text)/* overload */ : TParser(Text) { }
	/* TParser.Create */ inline __fastcall TSQLParser(System::Classes::TStream* const Stream, Clrclasses::Encoding* AEncoding)/* overload */ : TParser(Stream, AEncoding) { }
	/* TParser.Create */ inline __fastcall virtual TSQLParser(System::Classes::TStream* const Stream, __int64 ASize, Clrclasses::Encoding* AEncoding)/* overload */ : TParser(Stream, ASize, AEncoding) { }
	/* TParser.Destroy */ inline __fastcall virtual ~TSQLParser() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 lcEnd = System::Int8(0x0);
static const System::Int8 lcLexem = System::Int8(-100);
static const System::Int8 lcSymbol = System::Int8(-102);
static const System::Int8 lcIdent = System::Int8(-103);
static const System::Int8 lcNumber = System::Int8(-105);
static const System::Int8 lcString = System::Int8(-106);
static const System::Int8 lcBlank = System::Int8(-107);
static const System::Int8 lcComment = System::Int8(-108);
static const System::Int8 lxExclamation = System::Int8(0x1);
static const System::Int8 lxDoubleQuotes = System::Int8(0x2);
static const System::Int8 lxOctothorp = System::Int8(0x3);
static const System::Int8 lxDollar = System::Int8(0x4);
static const System::Int8 lxPercent = System::Int8(0x5);
static const System::Int8 lxAmp = System::Int8(0x6);
static const System::Int8 lxQuote = System::Int8(0x7);
static const System::Int8 lxLeftBracket = System::Int8(0x8);
static const System::Int8 lxRightBracket = System::Int8(0x9);
static const System::Int8 lxAsterisk = System::Int8(0xa);
static const System::Int8 lxPlus = System::Int8(0xb);
static const System::Int8 lxComma = System::Int8(0xc);
static const System::Int8 lxDash = System::Int8(0xd);
static const System::Int8 lxPoint = System::Int8(0xe);
static const System::Int8 lxSlash = System::Int8(0xf);
static const System::Int8 lxColon = System::Int8(0x10);
static const System::Int8 lxSemicolon = System::Int8(0x11);
static const System::Int8 lxLess = System::Int8(0x12);
static const System::Int8 lxEqual = System::Int8(0x13);
static const System::Int8 lxMore = System::Int8(0x14);
static const System::Int8 lxQuestion = System::Int8(0x15);
static const System::Int8 lxAt = System::Int8(0x16);
static const System::Int8 lxLeftSqBracket = System::Int8(0x17);
static const System::Int8 lxBackSlash = System::Int8(0x18);
static const System::Int8 lxRightSqBracket = System::Int8(0x19);
static const System::Int8 lxCircumflex = System::Int8(0x1a);
static const System::Int8 lxUnderline = System::Int8(0x1b);
static const System::Int8 lxGrave = System::Int8(0x1c);
static const System::Int8 lxMaxSymbolValue = System::Int8(0x1c);
static const System::Int8 lxSQLFirst = System::Int8(0x64);
static const System::Int8 lxALL = System::Int8(0x64);
static const System::Int8 lxAND = System::Int8(0x65);
static const System::Int8 lxAS = System::Int8(0x66);
static const System::Int8 lxBEGIN = System::Int8(0x67);
static const System::Int8 lxBY = System::Int8(0x68);
static const System::Int8 lxCASE = System::Int8(0x69);
static const System::Int8 lxCOMMIT = System::Int8(0x6a);
static const System::Int8 lxDECLARE = System::Int8(0x6b);
static const System::Int8 lxDELETE = System::Int8(0x6c);
static const System::Int8 lxDESC = System::Int8(0x6d);
static const System::Int8 lxDISTINCT = System::Int8(0x6e);
static const System::Int8 lxELSE = System::Int8(0x6f);
static const System::Int8 lxEND = System::Int8(0x70);
static const System::Int8 lxEXECUTE = System::Int8(0x71);
static const System::Int8 lxFETCH = System::Int8(0x72);
static const System::Int8 lxFOR = System::Int8(0x73);
static const System::Int8 lxFROM = System::Int8(0x74);
static const System::Int8 lxFULL = System::Int8(0x75);
static const System::Int8 lxGROUP = System::Int8(0x76);
static const System::Int8 lxHAVING = System::Int8(0x77);
static const System::Int8 lxINNER = System::Int8(0x78);
static const System::Int8 lxINSERT = System::Int8(0x79);
static const System::Int8 lxINTERSECT = System::Int8(0x7a);
static const System::Int8 lxINTO = System::Int8(0x7b);
static const System::Int8 lxIS = System::Int8(0x7c);
static const System::Int8 lxJOIN = System::Int8(0x7d);
static const System::Int8 lxLEFT = System::Int8(0x7e);
static const System::Int8 lxLIMIT = System::Int8(0x7f);
static const System::Byte lxLOCK = System::Byte(0x80);
static const System::Byte lxMINUS = System::Byte(0x81);
static const System::Byte lxNOT = System::Byte(0x82);
static const System::Byte lxOFFSET = System::Byte(0x83);
static const System::Byte lxON = System::Byte(0x84);
static const System::Byte lxONLY = System::Byte(0x85);
static const System::Byte lxOR = System::Byte(0x86);
static const System::Byte lxORDER = System::Byte(0x87);
static const System::Byte lxOUT = System::Byte(0x88);
static const System::Byte lxOUTER = System::Byte(0x89);
static const System::Byte lxOUTPUT = System::Byte(0x8d);
static const System::Byte lxRELEASE = System::Byte(0x8e);
static const System::Byte lxRETURNING = System::Byte(0x8f);
static const System::Byte lxRIGHT = System::Byte(0x90);
static const System::Byte lxROLLBACK = System::Byte(0x91);
static const System::Byte lxSAVEPOINT = System::Byte(0x92);
static const System::Byte lxSELECT = System::Byte(0x93);
static const System::Byte lxSET = System::Byte(0x94);
static const System::Byte lxTHEN = System::Byte(0x95);
static const System::Byte lxTO = System::Byte(0x96);
static const System::Byte lxTRANSACTION = System::Byte(0x97);
static const System::Byte lxUNION = System::Byte(0x98);
static const System::Byte lxUPDATE = System::Byte(0x99);
static const System::Byte lxWHEN = System::Byte(0x9a);
static const System::Byte lxWHERE = System::Byte(0x9b);
static const System::Byte lxWITH = System::Byte(0x9c);
static const System::Byte lxVALUES = System::Byte(0x9d);
static const System::Byte lxCALL = System::Byte(0x9e);
static const System::Byte lxCREATE = System::Byte(0x9f);
static const System::Byte lxEXEC = System::Byte(0xa0);
static const int BLOCK_SIZE = int(0x10000);
extern DELPHI_PACKAGE TLexemList* CommonSymbolLexems;
extern DELPHI_PACKAGE TLexemList* CommonKeywordLexems;
extern DELPHI_PACKAGE TLexemList* SQLSymbolLexems;
extern DELPHI_PACKAGE TLexemList* SQLKeywordLexems;
extern DELPHI_PACKAGE TClauseList* SQLClauses;
extern DELPHI_PACKAGE System::UnicodeString __fastcall _GetFrom(const System::UnicodeString SQL, TSQLParserClass ParserClass, bool OmitComment, const System::UnicodeString MacroChar);
extern DELPHI_PACKAGE System::UnicodeString __fastcall _GetWhere(const System::UnicodeString SQL, TSQLParserClass ParserClass, bool OmitComment, const System::UnicodeString MacroChar);
extern DELPHI_PACKAGE System::UnicodeString __fastcall _GetOrderBy(const System::UnicodeString SQL, TSQLParserClass ParserClass);
extern DELPHI_PACKAGE void __fastcall _FindWherePosition(const System::UnicodeString SQL, System::UnicodeString &Condition, TSQLParserClass ParserClass, bool OmitComment, const System::UnicodeString MacroChar, /* out */ int &StartPos, /* out */ int &EndPos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall _AddWhere(const System::UnicodeString SQL, System::UnicodeString Condition, TSQLParserClass ParserClass, bool OmitComment, const System::UnicodeString MacroChar);
extern DELPHI_PACKAGE System::UnicodeString __fastcall _SetWhere(const System::UnicodeString SQL, System::UnicodeString Condition, TSQLParserClass ParserClass, bool OmitComment);
extern DELPHI_PACKAGE System::UnicodeString __fastcall _SetOrderBy(const System::UnicodeString SQL, System::UnicodeString Fields, TSQLParserClass ParserClass);
}	/* namespace Crparser */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRPARSER)
using namespace Crparser;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrparserHPP
