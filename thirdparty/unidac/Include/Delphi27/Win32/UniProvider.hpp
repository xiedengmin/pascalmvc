// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UniProvider.pas' rev: 34.00 (Windows)

#ifndef UniproviderHPP
#define UniproviderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <System.TypInfo.hpp>
#include <Data.DB.hpp>
#include <MemDS.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>
#include <CRAccess.hpp>
#include <CRConnectionPool.hpp>
#include <CRDataTypeMap.hpp>
#include <CRConnectionString.hpp>
#include <CRServerEnumerator.hpp>
#include <MemData.hpp>
#include <DBAccess.hpp>
#include <DAScript.hpp>
#include <DADump.hpp>
#include <DALoader.hpp>
#include <DAAlerter.hpp>

//-- user supplied -----------------------------------------------------------

namespace Uniprovider
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUniProvider;
class DELPHICLASS TUniProviderDesc;
class DELPHICLASS TUniProviders;
class DELPHICLASS TConnectDialogService;
class DELPHICLASS TCRDummyAlerter;
class DELPHICLASS TOption;
class DELPHICLASS TIntegerOption;
class DELPHICLASS TStringOption;
class DELPHICLASS TBooleanOption;
class DELPHICLASS TEnumeratorOption;
class DELPHICLASS TOptionsList;
class DELPHICLASS TUniSqlFormatter;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TConnectDialogServiceClass;

typedef System::TMetaClass* TUniSqlFormatterClass;

class PASCALIMPLEMENTATION TUniProvider : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	TOptionsList* FConnectionOptions;
	TOptionsList* FSQLOptions;
	TOptionsList* FDataSetOptions;
	TOptionsList* FScriptOptions;
	TOptionsList* FLoaderOptions;
	TOptionsList* FDumpOptions;
	TOptionsList* FAlerterOptions;
	TOptionsList* FTransactionOptions;
	virtual void __fastcall CreateConnectionOptions() = 0 ;
	virtual void __fastcall CreateSQLOptions() = 0 ;
	virtual void __fastcall CreateDataSetOptions() = 0 ;
	virtual void __fastcall CreateScriptOptions();
	virtual void __fastcall CreateLoaderOptions();
	virtual void __fastcall CreateDumpOptions();
	virtual void __fastcall CreateAlerterOptions();
	virtual void __fastcall CreateTransactionOptions();
	
public:
	__fastcall virtual TUniProvider(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TUniProvider();
	__classmethod virtual System::UnicodeString __fastcall GetProviderName();
	__classmethod virtual Crconnectionstring::TCRConnectionStringBuilderClass __fastcall GetConnectionStringClass();
	virtual bool __fastcall IsServerSupported();
	virtual bool __fastcall IsDatabaseSupported();
	virtual bool __fastcall IsPortSupported();
	virtual bool __fastcall IsDataSetNeedTransaction();
	virtual bool __fastcall IsParamsSupported();
	virtual bool __fastcall IsInOutParamSupported();
	virtual bool __fastcall IsPoolingSupported();
	virtual bool __fastcall IsStoredProcSupported();
	virtual bool __fastcall IsUpdateSQLSupported();
	virtual bool __fastcall IsMacrosSupported();
	virtual bool __fastcall NeedRecreateProcCall();
	virtual bool __fastcall NeedComplexUpdateFieldDefList();
	virtual bool __fastcall NeedNativeParseSQL();
	virtual bool __fastcall NeedBlobUnicode(Dbaccess::TDAParam* Param);
	virtual Crconnectionpool::TCRConnectionParametersClass __fastcall GetConnectionParametersClass() = 0 ;
	virtual Crconnectionpool::TCRConnectionPoolManagerClass __fastcall GetConnectionPoolingManagerClass() = 0 ;
	virtual Craccess::TCRConnectionClass __fastcall GetConnectionClass() = 0 ;
	virtual Crserverenumerator::TCRServerEnumeratorClass __fastcall GetServerEnumeratorClass() = 0 ;
	virtual Crparser::TSQLParserClass __fastcall GetParserClass() = 0 ;
	virtual Dbaccess::TDADataSetServiceClass __fastcall GetDataSetServiceClass() = 0 ;
	virtual Dascript::TDAScriptProcessorClass __fastcall GetScriptProcessorClass() = 0 ;
	virtual Craccess::TCRAlerterClass __fastcall GetAlerterClass();
	virtual Dadump::TDADumpProcessorClass __fastcall GetDumpProcessorClass() = 0 ;
	virtual Dbaccess::TDAFieldTypeMapClass __fastcall GetFieldTypeMapClass() = 0 ;
	virtual TConnectDialogServiceClass __fastcall GetConnectDialogServiceClass() = 0 ;
	virtual TUniSqlFormatterClass __fastcall GetSqlFormatterClass() = 0 ;
	virtual Crdatatypemap::TConverterManagerClass __fastcall GetConverterManagerClass() = 0 ;
	virtual System::TClass __fastcall GetParamObjectClass(Dbaccess::TDAParam* Param);
	virtual Memdata::TSharedObject* __fastcall CreateParamObject(Dbaccess::TDAParam* Param, bool IsUnicode);
	virtual System::UnicodeString __fastcall CheckParamName(const System::UnicodeString ParamName);
	TOptionsList* __fastcall GetComponentOptionsList(System::Classes::TComponent* Component);
	virtual void __fastcall SetObjectProps(System::TObject* Obj, System::Classes::TStrings* Options);
	TOptionsList* __fastcall GetConnectionOptions();
	TOptionsList* __fastcall GetSQLOptions();
	TOptionsList* __fastcall GetDataSetOptions();
	TOptionsList* __fastcall GetScriptOptions();
	TOptionsList* __fastcall GetLoaderOptions();
	TOptionsList* __fastcall GetDumpOptions();
	TOptionsList* __fastcall GetAlerterOptions();
	virtual TOptionsList* __fastcall GetTransactionOptions();
	virtual System::UnicodeString __fastcall DefaultTableSchema();
	System::UnicodeString __fastcall ValidateOption(System::Classes::TComponent* Owner, const System::UnicodeString Prefix, const System::UnicodeString Name, const System::UnicodeString Value)/* overload */;
	System::UnicodeString __fastcall ValidateOption(System::Classes::TComponent* Owner, const System::UnicodeString Prefix, const System::UnicodeString Name)/* overload */;
};


typedef System::TMetaClass* TUniProviderClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniProviderDesc : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FProviderName;
	System::UnicodeString FProviderShortName;
	System::UnicodeString FPackageName;
	System::UnicodeString FAssemblyName;
	System::UnicodeString FSiblingProduct;
	TUniProvider* FProvider;
	System::UnicodeString __fastcall GetUnitName();
	System::UnicodeString __fastcall GetUnitNameCLR();
	System::UnicodeString __fastcall GetProviderComponentName();
	
public:
	__property System::UnicodeString ProviderName = {read=FProviderName};
	__property System::UnicodeString PackageName = {read=FPackageName};
	__property System::UnicodeString AssemblyName = {read=FAssemblyName};
	__property System::UnicodeString SiblingProduct = {read=FSiblingProduct};
	__property System::UnicodeString ProviderUnitName = {read=GetUnitName};
	__property System::UnicodeString ProviderUnitNameCLR = {read=GetUnitNameCLR};
	__property System::UnicodeString ProviderComponentName = {read=GetProviderComponentName};
	__property TUniProvider* Provider = {read=FProvider};
public:
	/* TObject.Create */ inline __fastcall TUniProviderDesc() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TUniProviderDesc() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniProviders : public System::Classes::TThreadList
{
	typedef System::Classes::TThreadList inherited;
	
private:
	TUniProviderDesc* __fastcall FindProviderDesc(System::UnicodeString ProviderName);
	void __fastcall RegisterProviderDesc(System::UnicodeString ProviderName, System::UnicodeString ProviderShortName, System::UnicodeString PackageName, System::UnicodeString AssemblyName, System::UnicodeString SiblingProduct);
	
public:
	__fastcall virtual ~TUniProviders();
	void __fastcall RegisterProvider(TUniProviderClass UniProviderClass);
	void __fastcall UnRegisterProvider(TUniProviderClass UniProviderClass);
	TUniProviderDesc* __fastcall GetProviderDesc(System::UnicodeString ProviderName);
	TUniProvider* __fastcall GetProvider(System::UnicodeString ProviderName);
	void __fastcall GetProviderNames(System::Classes::TStrings* Names);
public:
	/* TThreadList.Create */ inline __fastcall TUniProviders() : System::Classes::TThreadList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TConnectDialogService : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__fastcall virtual TConnectDialogService();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	virtual int __fastcall GetConnectMode();
	virtual bool __fastcall UseDatabaseHistory();
	virtual System::UnicodeString __fastcall GetDefaultDatabase();
	virtual bool __fastcall UsernameEnabled();
	virtual bool __fastcall PasswordEnabled();
	virtual bool __fastcall ServerEnabled();
	virtual bool __fastcall DatabaseEnabled();
	virtual bool __fastcall PortEnabled();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TConnectDialogService() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TCRDummyAlerter : public Craccess::TCRAlerter
{
	typedef Craccess::TCRAlerter inherited;
	
public:
	virtual void __fastcall SendEvent(const System::UnicodeString EventName, const System::UnicodeString Message);
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
public:
	/* TCRAlerter.Create */ inline __fastcall virtual TCRDummyAlerter() : Craccess::TCRAlerter() { }
	/* TCRAlerter.Destroy */ inline __fastcall virtual ~TCRDummyAlerter() { }
	
};


typedef void __fastcall (__closure *TOnAssignValue)(System::TObject* InternalObject, const System::Variant &Value);

typedef bool __fastcall (__closure *TSetPropFunc)(int Prop, const System::Variant &Value);

typedef void __fastcall (__closure *TOnGetValuesList)(System::Classes::TStrings* List);

typedef System::DynamicArray<System::TClass> TClassArray;

class PASCALIMPLEMENTATION TOption : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FOptionName;
	int FInternalIndex;
	TClassArray FInternalClasses;
	System::Variant FDefaultValue;
	TOnGetValuesList FOnGetValuesList;
	TOnAssignValue FOnAssignValue;
	
protected:
	void __fastcall ValidationError(const System::UnicodeString Value);
	virtual void __fastcall InternalGetValuesList(System::Classes::TStrings* List);
	
public:
	__fastcall TOption(const System::UnicodeString OptionName, int InternalIndex, System::TClass *InternalClasses, const int InternalClasses_High, const System::Variant &DefaultValue, TOnAssignValue OnAssign)/* overload */;
	System::Variant __fastcall GetDefaultValue();
	virtual System::UnicodeString __fastcall GetAsString(const System::Variant &Value);
	virtual System::Variant __fastcall GetAsNative(const System::UnicodeString Value);
	virtual bool __fastcall CheckValue(const System::UnicodeString Value);
	virtual void __fastcall Validate(const System::UnicodeString Value);
	virtual void __fastcall GetValuesList(System::Classes::TStrings* List);
	__property System::UnicodeString OptionName = {read=FOptionName};
	__property int InternalIndex = {read=FInternalIndex, nodefault};
	__property TClassArray InternalClasses = {read=FInternalClasses};
	__property TOnGetValuesList OnGetValuesList = {read=FOnGetValuesList, write=FOnGetValuesList};
	__property TOnAssignValue OnAssignValue = {read=FOnAssignValue, write=FOnAssignValue};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOption() { }
	
};


class PASCALIMPLEMENTATION TIntegerOption : public TOption
{
	typedef TOption inherited;
	
public:
	virtual System::UnicodeString __fastcall GetAsString(const System::Variant &Value);
	virtual System::Variant __fastcall GetAsNative(const System::UnicodeString Value);
	virtual bool __fastcall CheckValue(const System::UnicodeString Value);
public:
	/* TOption.Create */ inline __fastcall TIntegerOption(const System::UnicodeString OptionName, int InternalIndex, System::TClass *InternalClasses, const int InternalClasses_High, const System::Variant &DefaultValue, TOnAssignValue OnAssign)/* overload */ : TOption(OptionName, InternalIndex, InternalClasses, InternalClasses_High, DefaultValue, OnAssign) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TIntegerOption() { }
	
};


class PASCALIMPLEMENTATION TStringOption : public TOption
{
	typedef TOption inherited;
	
public:
	/* TOption.Create */ inline __fastcall TStringOption(const System::UnicodeString OptionName, int InternalIndex, System::TClass *InternalClasses, const int InternalClasses_High, const System::Variant &DefaultValue, TOnAssignValue OnAssign)/* overload */ : TOption(OptionName, InternalIndex, InternalClasses, InternalClasses_High, DefaultValue, OnAssign) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStringOption() { }
	
};


class PASCALIMPLEMENTATION TBooleanOption : public TOption
{
	typedef TOption inherited;
	
protected:
	virtual void __fastcall InternalGetValuesList(System::Classes::TStrings* List);
	
public:
	virtual System::UnicodeString __fastcall GetAsString(const System::Variant &Value);
	virtual System::Variant __fastcall GetAsNative(const System::UnicodeString Value);
	virtual bool __fastcall CheckValue(const System::UnicodeString Value);
public:
	/* TOption.Create */ inline __fastcall TBooleanOption(const System::UnicodeString OptionName, int InternalIndex, System::TClass *InternalClasses, const int InternalClasses_High, const System::Variant &DefaultValue, TOnAssignValue OnAssign)/* overload */ : TOption(OptionName, InternalIndex, InternalClasses, InternalClasses_High, DefaultValue, OnAssign) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TBooleanOption() { }
	
};


class PASCALIMPLEMENTATION TEnumeratorOption : public TOption
{
	typedef TOption inherited;
	
private:
	System::Typinfo::TTypeInfo *FTypeInfo;
	int FMinValue;
	int FMaxValue;
	bool FInternalType;
	
protected:
	virtual void __fastcall InternalGetValuesList(System::Classes::TStrings* List);
	
public:
	__fastcall TEnumeratorOption(const System::UnicodeString OptionName, int InternalIndex, System::TClass *InternalClasses, const int InternalClasses_High, const System::Variant &DefaultValue, System::Typinfo::PTypeInfo TypeInfo);
	virtual System::UnicodeString __fastcall GetAsString(const System::Variant &Value);
	virtual System::Variant __fastcall GetAsNative(const System::UnicodeString Value);
	virtual bool __fastcall CheckValue(const System::UnicodeString Value);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEnumeratorOption() { }
	
};


class PASCALIMPLEMENTATION TOptionsList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
public:
	TOption* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::UnicodeString FPrefix;
	System::Classes::TStringList* FDeprecatedOptions;
	TOption* __fastcall GetOption(int Index);
	void __fastcall SetOption(int Index, TOption* const Value);
	
public:
	__fastcall TOptionsList(const System::UnicodeString Prefix);
	__fastcall virtual ~TOptionsList();
	HIDESBASE void __fastcall Add(TOption* Value);
	void __fastcall AddDeprecated(const System::UnicodeString DeprecatedName, TOption* Value);
	void __fastcall Remove(const System::UnicodeString Name);
	TOption* __fastcall OptionByCode(int Code);
	TOption* __fastcall OptionByName(const System::UnicodeString Name);
	TOption* __fastcall OptionByDeprecatedName(const System::UnicodeString Name);
	void __fastcall ImportOptions(System::Classes::TStrings* Source, System::TObject* DestObject, TSetPropFunc SetPropFunc);
	void __fastcall ExportDefOptions(System::Classes::TStrings* Dest);
	System::Variant __fastcall GetValueByName(System::Classes::TStrings* Source, const System::UnicodeString Name);
	__property System::UnicodeString Prefix = {read=FPrefix};
	__property TOption* Items[int Index] = {read=GetOption, write=SetOption/*, default*/};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TUniSqlFormatter : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	Crtypes::TStrValueStringList* FFunctions;
	Crtypes::TStrValueStringList* FPredefinedMacros;
	System::Classes::TStringList* FUserMacroNames;
	System::Classes::TStringList* FUserMacroValues;
	Crparser::TSQLParserClass FParserClass;
	virtual bool __fastcall IsServerKeywordLexem(Crparser::TSQLParser* Parser, const System::UnicodeString Lexem);
	bool __fastcall IsDefinedMacro(const System::UnicodeString MacroName);
	System::UnicodeString __fastcall Parse(Crparser::TSQLParser* Parser, const System::UnicodeString EndChar = System::UnicodeString());
	System::UnicodeString __fastcall ProcessFunction(const System::UnicodeString Body);
	virtual System::UnicodeString __fastcall GetFunction(const System::UnicodeString FunctionName, const System::DynamicArray<System::UnicodeString> Params);
	System::UnicodeString __fastcall ProcessDate(const System::UnicodeString Body);
	System::UnicodeString __fastcall ProcessTime(const System::UnicodeString Body);
	System::UnicodeString __fastcall ProcessTimestamp(const System::UnicodeString Body);
	System::UnicodeString __fastcall ProcessMacro(const System::UnicodeString MacroName, const System::UnicodeString Body);
	virtual bool __fastcall NeedUnquote();
	
public:
	__fastcall virtual TUniSqlFormatter();
	__fastcall virtual ~TUniSqlFormatter();
	virtual System::WideChar __fastcall LeftQuote();
	virtual System::WideChar __fastcall RightQuote();
	void __fastcall SetUserMacros(System::Classes::TStringList* Names, System::Classes::TStringList* Values);
	bool __fastcall CheckIfCondition(const System::UnicodeString Body);
	virtual void __fastcall Expand(System::UnicodeString &SQL);
	void __fastcall SetParserClass(Crparser::TSQLParserClass Value);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TUniProviders* UniProviders;
extern DELPHI_PACKAGE void __fastcall FillOptionsList(const System::UnicodeString OptionPrefix, TOptionsList* OptionsList, System::Classes::TStrings* List);
extern DELPHI_PACKAGE void __fastcall GetOptionValuesList(const System::UnicodeString OptionName, TOptionsList* OptionsList, System::Classes::TStrings* List);
extern DELPHI_PACKAGE void __fastcall ExtractOption(const System::UnicodeString Str, System::UnicodeString &OptionPrefix, System::UnicodeString &OptionName, System::UnicodeString &OptionValue);
extern DELPHI_PACKAGE void __fastcall WriteOptions(TOptionsList* OptionsList, System::Classes::TStrings* List, System::TClass DestClass, TSetPropFunc SetPropFunc);
extern DELPHI_PACKAGE void __fastcall SetSpecificOption(Dbaccess::TCustomDAConnection* Connection, const System::UnicodeString Name, const System::UnicodeString Value);
extern DELPHI_PACKAGE void __fastcall CheckProviderName(const System::UnicodeString ProviderName);
}	/* namespace Uniprovider */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UNIPROVIDER)
using namespace Uniprovider;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UniproviderHPP
