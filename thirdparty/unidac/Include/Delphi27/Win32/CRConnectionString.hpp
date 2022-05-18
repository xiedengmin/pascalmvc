// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRConnectionString.pas' rev: 34.00 (Windows)

#ifndef CrconnectionstringHPP
#define CrconnectionstringHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <System.Variants.hpp>
#include <DAConsts.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crconnectionstring
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EConnectionStringError;
class DELPHICLASS TConnectionStringParam;
class DELPHICLASS TConnectionStringBuilder;
class DELPHICLASS TCRConnectionStringBuilder;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TConnectionStringBuilderClass;

typedef System::Variant __fastcall (__closure *TGetConnectionStringParamMethod)(int PropCode);

typedef void __fastcall (__closure *TSetConnectionStringParamMethod)(int PropCode, const System::Variant &PropValue);

#pragma pack(push,4)
class PASCALIMPLEMENTATION EConnectionStringError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EConnectionStringError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EConnectionStringError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EConnectionStringError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EConnectionStringError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EConnectionStringError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EConnectionStringError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EConnectionStringError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EConnectionStringError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EConnectionStringError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EConnectionStringError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EConnectionStringError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EConnectionStringError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EConnectionStringError() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TConnectionStringParamPriotity : unsigned char { ppLowest, ppLow, ppNormal, ppHigh, ppHighest };

class PASCALIMPLEMENTATION TConnectionStringParam : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TConnectionStringParamPriotity FPriority;
	System::UnicodeString FName;
	System::UnicodeString FNameUpper;
	System::DynamicArray<System::UnicodeString> FSupportedNames;
	System::DynamicArray<System::UnicodeString> FSupportedNamesUpper;
	int FCode;
	System::Word FDataType;
	System::Variant FDefaultValue;
	System::Variant FSkipValues;
	System::Typinfo::TTypeInfo *FTypeInfo;
	System::UnicodeString FEnumPrefix;
	
protected:
	int __fastcall GetEnumValue(System::Typinfo::PTypeInfo TypeInfo, const System::UnicodeString Name);
	System::UnicodeString __fastcall GetEnumName(System::Typinfo::PTypeInfo TypeInfo, int Value);
	
public:
	__fastcall TConnectionStringParam(TConnectionStringParamPriotity APriority, const System::UnicodeString AName, const System::UnicodeString *ASupportedNames, const int ASupportedNames_High, int ACode, System::Word ADataType, const System::Variant &ADefaultValue, const System::Variant &ASkipValues, System::Typinfo::PTypeInfo ATypeInfo, const System::UnicodeString AEnumPrefix);
	bool __fastcall CheckName(System::UnicodeString NameUpper);
	System::UnicodeString __fastcall GetAsString(const System::Variant &Value);
	System::Variant __fastcall GetAsVariant(System::UnicodeString Value);
	__property TConnectionStringParamPriotity Priority = {read=FPriority, nodefault};
	__property System::UnicodeString Name = {read=FName};
	__property System::UnicodeString NameUpper = {read=FNameUpper};
	__property System::DynamicArray<System::UnicodeString> SupportedNames = {read=FSupportedNames};
	__property System::DynamicArray<System::UnicodeString> SupportedNamesUpper = {read=FSupportedNamesUpper};
	__property int Code = {read=FCode, nodefault};
	__property System::Word DataType = {read=FDataType, nodefault};
	__property System::Variant DefaultValue = {read=FDefaultValue};
	__property System::Variant SkipValues = {read=FSkipValues};
	__property System::Typinfo::PTypeInfo TypeInfo = {read=FTypeInfo};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TConnectionStringParam() { }
	
};


class PASCALIMPLEMENTATION TConnectionStringBuilder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Crtypes::TCRObjectList* FParamList;
	TGetConnectionStringParamMethod FGetPropMethod;
	TSetConnectionStringParamMethod FSetPropMethod;
	TConnectionStringParam* __fastcall GetParam(int Index);
	int __fastcall GetParamCount();
	System::UnicodeString __fastcall GetConnectionString();
	void __fastcall SetConnectionString(const System::UnicodeString Value);
	void __fastcall SetExtStringBuilderClass(TConnectionStringBuilderClass Value);
	
protected:
	TConnectionStringBuilderClass FExtStringBuilderClass;
	TConnectionStringBuilder* FExtStringBuilder;
	virtual void __fastcall InitParams() = 0 ;
	void __fastcall AddParam(TConnectionStringParamPriotity Priority, const System::UnicodeString Name, const System::UnicodeString *SupportedNames, const int SupportedNames_High, int Code, System::Word DataType, const System::Variant &DefaultValue, System::Typinfo::PTypeInfo TypeInfo = (System::Typinfo::PTypeInfo)(0x0), const System::UnicodeString EnumPrefix = System::UnicodeString())/* overload */;
	void __fastcall AddParam(TConnectionStringParamPriotity Priority, const System::UnicodeString Name, const System::UnicodeString *SupportedNames, const int SupportedNames_High, int Code, System::Word DataType, const System::Variant &DefaultValue, const System::Variant *SkipValues, const int SkipValues_High, System::Typinfo::PTypeInfo TypeInfo = (System::Typinfo::PTypeInfo)(0x0), const System::UnicodeString EnumPrefix = System::UnicodeString())/* overload */;
	void __fastcall DeleteParam(int Code);
	virtual bool __fastcall IgnoreParam(int Code);
	int __fastcall GetParamIndex(TConnectionStringParamPriotity Priority, const System::UnicodeString Name);
	virtual System::Variant __fastcall GetParamValue(TConnectionStringParam* Param);
	virtual void __fastcall SetParamValue(TConnectionStringParam* Param, const System::Variant &Value);
	virtual System::UnicodeString __fastcall ConvertVarToStr(TConnectionStringParam* Param, const System::Variant &Value);
	virtual System::Variant __fastcall ConvertStrToVar(TConnectionStringParam* Param, const System::UnicodeString Value);
	bool __fastcall CheckParamName(const System::UnicodeString *Args, const int Args_High, const System::UnicodeString Name);
	virtual void __fastcall CheckParamValue(const System::UnicodeString Name, const System::UnicodeString Value, bool IsValueQuoted = false);
	void __fastcall AppendParam(System::UnicodeString &Result, const System::UnicodeString Name, const System::UnicodeString Value);
	void __fastcall AppendParamStr(System::UnicodeString &Result, const System::UnicodeString Str);
	virtual void __fastcall ResetParams();
	virtual void __fastcall ProcessParams(const System::UnicodeString Value);
	int __fastcall GetParamIndexByCode(int Code);
	int __fastcall GetParamIndexByName(const System::UnicodeString Name);
	virtual void __fastcall ReleaseExtStringBuilder();
	virtual void __fastcall InitExtStringBuilder();
	__property TGetConnectionStringParamMethod GetProp = {read=FGetPropMethod};
	__property TSetConnectionStringParamMethod SetProp = {read=FSetPropMethod};
	__property TConnectionStringBuilderClass ExtStringBuilderClass = {read=FExtStringBuilderClass, write=SetExtStringBuilderClass};
	__property TConnectionStringBuilder* ExtStringBuilder = {read=FExtStringBuilder};
	
public:
	__fastcall virtual TConnectionStringBuilder(TGetConnectionStringParamMethod GetPropMethod, TSetConnectionStringParamMethod SetPropMethod);
	__fastcall virtual ~TConnectionStringBuilder();
	Crtypes::TStrValueStringList* __fastcall Parse(const System::UnicodeString Value, bool AllowDuplicate = false);
	virtual void __fastcall ReadParam(TConnectionStringParam* Param, const System::UnicodeString Value);
	void __fastcall ReadParams(Crtypes::TStrValueStringList* ParamStrList);
	virtual bool __fastcall ReadUnknownParam(const System::UnicodeString Name, const System::UnicodeString Value);
	virtual void __fastcall ReadUnknownParams(Crtypes::TStrValueStringList* ParamStrList);
	virtual void __fastcall WriteParam(System::UnicodeString &ConnectionString, TConnectionStringParam* Param);
	void __fastcall WriteParams(System::UnicodeString &ConnectionString);
	virtual void __fastcall WriteUnknownParams(System::UnicodeString &ConnectionString);
	System::Classes::TList* __fastcall GetFullParamList();
	TConnectionStringParam* __fastcall FindParamByCode(int Code);
	TConnectionStringParam* __fastcall FindParamByName(const System::UnicodeString Name);
	__property TConnectionStringParam* Params[int Index] = {read=GetParam};
	__property int ParamCount = {read=GetParamCount, nodefault};
	__property System::UnicodeString ConnectionString = {read=GetConnectionString, write=SetConnectionString, stored=false};
};


class PASCALIMPLEMENTATION TCRConnectionStringBuilder : public TConnectionStringBuilder
{
	typedef TConnectionStringBuilder inherited;
	
protected:
	virtual void __fastcall InitParams();
	virtual bool __fastcall IgnoreParam(int Code);
public:
	/* TConnectionStringBuilder.Create */ inline __fastcall virtual TCRConnectionStringBuilder(TGetConnectionStringParamMethod GetPropMethod, TSetConnectionStringParamMethod SetPropMethod) : TConnectionStringBuilder(GetPropMethod, SetPropMethod) { }
	/* TConnectionStringBuilder.Destroy */ inline __fastcall virtual ~TCRConnectionStringBuilder() { }
	
};


typedef System::TMetaClass* TCRConnectionStringBuilderClass;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 cpLoginPrompt = System::Int8(-1);
static const System::Int8 cpPooling = System::Int8(-2);
static const System::Int8 cpConnectionLifetime = System::Int8(-3);
static const System::Int8 cpMaxPoolSize = System::Int8(-4);
static const System::Int8 cpMinPoolSize = System::Int8(-5);
static const System::Int8 cpValidateConnection = System::Int8(-6);
static const System::Int8 cpPersistSecurityInfo = System::Int8(-7);
static const System::Word varEnum = System::Word(0x1ff);
}	/* namespace Crconnectionstring */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRCONNECTIONSTRING)
using namespace Crconnectionstring;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrconnectionstringHPP
