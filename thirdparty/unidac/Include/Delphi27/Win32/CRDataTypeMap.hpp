// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRDataTypeMap.pas' rev: 34.00 (Windows)

#ifndef CrdatatypemapHPP
#define CrdatatypemapHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.DateUtils.hpp>
#include <System.Variants.hpp>
#include <Data.FmtBcd.hpp>
#include <Data.SqlTimSt.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRFunctions.hpp>
#include <CRTimeStamp.hpp>
#include <DAConsts.hpp>
#include <MemUtils.hpp>
#include <MemData.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crdatatypemap
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EDataTypeMappingError;
class DELPHICLASS EInvalidMapRuleExpression;
class DELPHICLASS EUnsupportedDataTypeMapping;
class DELPHICLASS EInvalidDBTypeMapping;
class DELPHICLASS EInvalidFieldTypeMapping;
class DELPHICLASS EDataMappingError;
class DELPHICLASS TDBTypeInfo;
class DELPHICLASS TDBTypeInfos;
class DELPHICLASS TMapRule;
class DELPHICLASS TMapRules;
class DELPHICLASS TCRMapRule;
class DELPHICLASS TCRMapRules;
class DELPHICLASS TFetchConverter;
class DELPHICLASS TOnDemandConverter;
struct TConverterDictionaryItem;
class DELPHICLASS TConverterDictionary;
class DELPHICLASS TFetchConverterDictionary;
class DELPHICLASS TOnDemandConverterDictionary;
class DELPHICLASS TConverterManager;
class DELPHICLASS TSizeConverters;
class DELPHICLASS TDataConverters;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TMapRuleClass;

typedef System::TMetaClass* TCRMapRuleClass;

typedef System::TMetaClass* TCRMapRulesClass;

typedef System::TMetaClass* TConverterManagerClass;

typedef int __fastcall (__closure *TCalcSizeFunc)(int Value);

#pragma pack(push,4)
class PASCALIMPLEMENTATION EDataTypeMappingError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	__fastcall EDataTypeMappingError()/* overload */;
public:
	/* Exception.CreateFmt */ inline __fastcall EDataTypeMappingError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EDataTypeMappingError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EDataTypeMappingError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EDataTypeMappingError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EDataTypeMappingError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EDataTypeMappingError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EDataTypeMappingError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDataTypeMappingError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDataTypeMappingError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDataTypeMappingError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDataTypeMappingError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EDataTypeMappingError() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidMapRuleExpression : public EDataTypeMappingError
{
	typedef EDataTypeMappingError inherited;
	
public:
	/* EDataTypeMappingError.Create */ inline __fastcall EInvalidMapRuleExpression()/* overload */ : EDataTypeMappingError() { }
	
public:
	/* Exception.CreateFmt */ inline __fastcall EInvalidMapRuleExpression(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : EDataTypeMappingError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidMapRuleExpression(NativeUInt Ident)/* overload */ : EDataTypeMappingError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidMapRuleExpression(System::PResStringRec ResStringRec)/* overload */ : EDataTypeMappingError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidMapRuleExpression(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidMapRuleExpression(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidMapRuleExpression(const System::UnicodeString Msg, int AHelpContext) : EDataTypeMappingError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidMapRuleExpression(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : EDataTypeMappingError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidMapRuleExpression(NativeUInt Ident, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidMapRuleExpression(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidMapRuleExpression(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidMapRuleExpression(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidMapRuleExpression() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EUnsupportedDataTypeMapping : public EDataTypeMappingError
{
	typedef EDataTypeMappingError inherited;
	
public:
	/* EDataTypeMappingError.Create */ inline __fastcall EUnsupportedDataTypeMapping()/* overload */ : EDataTypeMappingError() { }
	
public:
	/* Exception.CreateFmt */ inline __fastcall EUnsupportedDataTypeMapping(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : EDataTypeMappingError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EUnsupportedDataTypeMapping(NativeUInt Ident)/* overload */ : EDataTypeMappingError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EUnsupportedDataTypeMapping(System::PResStringRec ResStringRec)/* overload */ : EDataTypeMappingError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EUnsupportedDataTypeMapping(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EUnsupportedDataTypeMapping(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EUnsupportedDataTypeMapping(const System::UnicodeString Msg, int AHelpContext) : EDataTypeMappingError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EUnsupportedDataTypeMapping(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : EDataTypeMappingError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EUnsupportedDataTypeMapping(NativeUInt Ident, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EUnsupportedDataTypeMapping(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EUnsupportedDataTypeMapping(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EUnsupportedDataTypeMapping(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EUnsupportedDataTypeMapping() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidDBTypeMapping : public EDataTypeMappingError
{
	typedef EDataTypeMappingError inherited;
	
public:
	/* EDataTypeMappingError.Create */ inline __fastcall EInvalidDBTypeMapping()/* overload */ : EDataTypeMappingError() { }
	
public:
	/* Exception.CreateFmt */ inline __fastcall EInvalidDBTypeMapping(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : EDataTypeMappingError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidDBTypeMapping(NativeUInt Ident)/* overload */ : EDataTypeMappingError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidDBTypeMapping(System::PResStringRec ResStringRec)/* overload */ : EDataTypeMappingError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidDBTypeMapping(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidDBTypeMapping(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidDBTypeMapping(const System::UnicodeString Msg, int AHelpContext) : EDataTypeMappingError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidDBTypeMapping(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : EDataTypeMappingError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidDBTypeMapping(NativeUInt Ident, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidDBTypeMapping(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidDBTypeMapping(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidDBTypeMapping(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidDBTypeMapping() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EInvalidFieldTypeMapping : public EDataTypeMappingError
{
	typedef EDataTypeMappingError inherited;
	
public:
	/* EDataTypeMappingError.Create */ inline __fastcall EInvalidFieldTypeMapping()/* overload */ : EDataTypeMappingError() { }
	
public:
	/* Exception.CreateFmt */ inline __fastcall EInvalidFieldTypeMapping(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : EDataTypeMappingError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidFieldTypeMapping(NativeUInt Ident)/* overload */ : EDataTypeMappingError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EInvalidFieldTypeMapping(System::PResStringRec ResStringRec)/* overload */ : EDataTypeMappingError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidFieldTypeMapping(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EInvalidFieldTypeMapping(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EInvalidFieldTypeMapping(const System::UnicodeString Msg, int AHelpContext) : EDataTypeMappingError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EInvalidFieldTypeMapping(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : EDataTypeMappingError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidFieldTypeMapping(NativeUInt Ident, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EInvalidFieldTypeMapping(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidFieldTypeMapping(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EInvalidFieldTypeMapping(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EInvalidFieldTypeMapping() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EDataMappingError : public EDataTypeMappingError
{
	typedef EDataTypeMappingError inherited;
	
public:
	/* EDataTypeMappingError.Create */ inline __fastcall EDataMappingError()/* overload */ : EDataTypeMappingError() { }
	
public:
	/* Exception.CreateFmt */ inline __fastcall EDataMappingError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : EDataTypeMappingError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EDataMappingError(NativeUInt Ident)/* overload */ : EDataTypeMappingError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EDataMappingError(System::PResStringRec ResStringRec)/* overload */ : EDataTypeMappingError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EDataMappingError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EDataMappingError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EDataMappingError(const System::UnicodeString Msg, int AHelpContext) : EDataTypeMappingError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EDataMappingError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : EDataTypeMappingError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDataMappingError(NativeUInt Ident, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EDataMappingError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDataMappingError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EDataMappingError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : EDataTypeMappingError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EDataMappingError() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBTypeInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Word FDBType;
	System::UnicodeString FName;
	bool FLength;
	bool FScale;
	System::Word __fastcall GetDBProvider();
	
public:
	__fastcall TDBTypeInfo(System::Word DBType, const System::UnicodeString Name, bool Length, bool Scale);
	__property System::Word DBType = {read=FDBType, nodefault};
	__property System::Word DBProvider = {read=GetDBProvider, nodefault};
	__property System::UnicodeString Name = {read=FName};
	__property bool Length = {read=FLength, nodefault};
	__property bool Scale = {read=FScale, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TDBTypeInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDBTypeInfos : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TDBTypeInfo* operator[](int Index) { return this->TypeInfos[Index]; }
	
private:
	Crtypes::TCRObjectList* FTypeInfos;
	int __fastcall GetCount();
	TDBTypeInfo* __fastcall GetTypeInfo(int Index);
	
public:
	__fastcall TDBTypeInfos();
	__fastcall virtual ~TDBTypeInfos();
	void __fastcall Add(System::Word DBType, const System::UnicodeString Name, bool Length, bool Scale);
	void __fastcall Delete(int Index);
	void __fastcall Clear();
	System::Sysutils::Exception* __fastcall Check(System::Word DBType, int LengthMin, int LengthMax, int ScaleMin, int ScaleMax, bool IsDBTypeRequired);
	TDBTypeInfo* __fastcall FindTypeInfo(System::Word DBType)/* overload */;
	TDBTypeInfo* __fastcall FindTypeInfo(const System::UnicodeString Name, System::Word DBProvider)/* overload */;
	__property TDBTypeInfo* TypeInfos[int Index] = {read=GetTypeInfo/*, default*/};
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMapRule : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FFieldName;
	System::Word FDBType;
	int FDBLengthMin;
	int FDBLengthMax;
	int FDBScaleMin;
	int FDBScaleMax;
	int FFieldLength;
	int FFieldScale;
	System::UnicodeString FFormat;
	bool FIgnoreErrors;
	System::Word __fastcall GetDBProvider();
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	void __fastcall AssignToRule(TMapRule* Dest);
	
public:
	__fastcall virtual TMapRule(System::Classes::TCollection* Owner);
	__property System::UnicodeString FieldName = {read=FFieldName, write=FFieldName};
	__property System::Word DBType = {read=FDBType, write=FDBType, nodefault};
	__property System::Word DBProvider = {read=GetDBProvider, nodefault};
	__property int DBLengthMin = {read=FDBLengthMin, write=FDBLengthMin, nodefault};
	__property int DBLengthMax = {read=FDBLengthMax, write=FDBLengthMax, nodefault};
	__property int DBScaleMin = {read=FDBScaleMin, write=FDBScaleMin, nodefault};
	__property int DBScaleMax = {read=FDBScaleMax, write=FDBScaleMax, nodefault};
	__property int FieldLength = {read=FFieldLength, write=FFieldLength, nodefault};
	__property int FieldScale = {read=FFieldScale, write=FFieldScale, nodefault};
	__property System::UnicodeString Format = {read=FFormat, write=FFormat};
	__property bool IgnoreErrors = {read=FIgnoreErrors, write=FIgnoreErrors, nodefault};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TMapRule() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMapRules : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TMapRule* operator[](int Index) { return this->Items[Index]; }
	
private:
	HIDESBASE TMapRule* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TMapRule* Value);
	
public:
	__property TMapRule* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TMapRules(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TMapRules() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRMapRule : public TMapRule
{
	typedef TMapRule inherited;
	
private:
	System::Word FDataType;
	
protected:
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	HIDESBASE void __fastcall AssignToRule(TCRMapRule* Dest);
	
public:
	__fastcall virtual TCRMapRule(System::Classes::TCollection* Owner);
	__property System::Word DataType = {read=FDataType, write=FDataType, nodefault};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TCRMapRule() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCRMapRules : public TMapRules
{
	typedef TMapRules inherited;
	
public:
	TCRMapRule* operator[](int Index) { return this->Items[Index]; }
	
private:
	bool FEnabled;
	bool FIgnoreInvalidRules;
	HIDESBASE TCRMapRule* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TCRMapRule* Value);
	
protected:
	__classmethod virtual TCRMapRuleClass __fastcall GetMapRuleClass();
	bool __fastcall CheckSQLType(TMapRule* Rule, int DBType, int DBLength, int DBScale);
	System::UnicodeString __fastcall CheckFormat(TCRMapRule* Rule, const System::UnicodeString Format);
	
public:
	__fastcall virtual TCRMapRules();
	__classmethod virtual TConverterManager* __fastcall GetConverterManager();
	TCRMapRule* __fastcall AddRule(const System::UnicodeString FieldName, System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, System::Word DataType, int FieldLength, int FieldScale, bool IgnoreErrors, const System::UnicodeString Format);
	TCRMapRule* __fastcall DetectFieldNameMapRule(const System::UnicodeString FieldName, int DBType, int DBLength, int DBScale);
	TCRMapRule* __fastcall DetectDBTypeMapRule(int DBType, int DBLength, int DBScale);
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property bool IgnoreInvalidRules = {read=FIgnoreInvalidRules, write=FIgnoreInvalidRules, nodefault};
	__property TCRMapRule* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TCRMapRules() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TFetchConverter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Word FDBType;
	int FDBLengthMin;
	int FDBLengthMax;
	int FDBScaleMin;
	int FDBScaleMax;
	System::Word FDestDataType;
	System::Word FInternalDataType;
	TCalcSizeFunc FCalcLength;
	TCalcSizeFunc FCalcScale;
	
public:
	__fastcall TFetchConverter(System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, System::Word DestDataType, System::Word InternalDataType, TCalcSizeFunc CalcLength, TCalcSizeFunc CalcScale);
	__property System::Word DBType = {read=FDBType, nodefault};
	__property int DBLengthMin = {read=FDBLengthMin, nodefault};
	__property int DBLengthMax = {read=FDBLengthMax, nodefault};
	__property int DBScaleMin = {read=FDBScaleMin, nodefault};
	__property int DBScaleMax = {read=FDBScaleMax, nodefault};
	__property System::Word DestDataType = {read=FDestDataType, nodefault};
	__property System::Word InternalDataType = {read=FInternalDataType, nodefault};
	__property TCalcSizeFunc CalcLength = {read=FCalcLength};
	__property TCalcSizeFunc CalcScale = {read=FCalcScale};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFetchConverter() { }
	
};


class PASCALIMPLEMENTATION TOnDemandConverter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Word FSourceDataType;
	System::Word FDestDataType;
	TCalcSizeFunc FCalcLength;
	TCalcSizeFunc FCalcScale;
	Memdata::TConvertProcedure FGetDataConverter;
	Memdata::TConvertProcedure FPutDataConverter;
	
public:
	__fastcall TOnDemandConverter(System::Word SourceDataType, System::Word DestDataType, Memdata::TConvertProcedure GetDataConverter, Memdata::TConvertProcedure PutDataConverter, TCalcSizeFunc CalcLength, TCalcSizeFunc CalcScale);
	TOnDemandConverter* __fastcall Clone();
	__property System::Word SourceDataType = {read=FSourceDataType, write=FSourceDataType, nodefault};
	__property System::Word DestDataType = {read=FDestDataType, write=FDestDataType, nodefault};
	__property Memdata::TConvertProcedure GetDataConverter = {read=FGetDataConverter};
	__property Memdata::TConvertProcedure PutDataConverter = {read=FPutDataConverter};
	__property TCalcSizeFunc CalcLength = {read=FCalcLength};
	__property TCalcSizeFunc CalcScale = {read=FCalcScale};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOnDemandConverter() { }
	
};


struct DECLSPEC_DRECORD TConverterDictionaryItem
{
public:
	int HashCode;
	System::TObject* Value;
};


typedef System::DynamicArray<TConverterDictionaryItem> TConverterDictionaryItemArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TConverterDictionary : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TConverterDictionaryItemArray FItems;
	int FCount;
	int FGrowThreshold;
	int __fastcall GetRehashBucketIndex(const int HashCode);
	int __fastcall Hash(System::Word Value1, System::Word Value2);
	void __fastcall SetCapacity(const int Capacity);
	void __fastcall Grow();
	virtual void __fastcall Rehash(int NewCapacity);
	
public:
	__fastcall virtual TConverterDictionary()/* overload */;
	__fastcall virtual TConverterDictionary(int Capacity)/* overload */;
	__fastcall virtual ~TConverterDictionary();
	void __fastcall Clear();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFetchConverterDictionary : public TConverterDictionary
{
	typedef TConverterDictionary inherited;
	
private:
	int __fastcall GetBucketIndex(System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, System::Word DestDataType)/* overload */;
	int __fastcall GetBucketIndex(System::Word DBType, int DBLength, int DBScale, System::Word DestDataType)/* overload */;
	
public:
	void __fastcall Add(TFetchConverter* Value);
	TFetchConverter* __fastcall FindItem(System::Word DBType, int DBLength, int DBScale, System::Word DestDataType);
public:
	/* TConverterDictionary.Create */ inline __fastcall virtual TFetchConverterDictionary()/* overload */ : TConverterDictionary() { }
	/* TConverterDictionary.Create */ inline __fastcall virtual TFetchConverterDictionary(int Capacity)/* overload */ : TConverterDictionary(Capacity) { }
	/* TConverterDictionary.Destroy */ inline __fastcall virtual ~TFetchConverterDictionary() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TOnDemandConverterDictionary : public TConverterDictionary
{
	typedef TConverterDictionary inherited;
	
private:
	int __fastcall GetBucketIndex(System::Word SourceDataType, System::Word DestDataType);
	
public:
	void __fastcall Add(TOnDemandConverter* Value);
	void __fastcall CloneConverter(System::Word SourceDataType, System::Word DestDataType, System::Word NewDataType);
	void __fastcall CloneConverters(System::Word SourceDataType, System::Word NewDataType);
	TOnDemandConverter* __fastcall FindItem(System::Word SourceDataType, System::Word DestDataType);
public:
	/* TConverterDictionary.Create */ inline __fastcall virtual TOnDemandConverterDictionary()/* overload */ : TConverterDictionary() { }
	/* TConverterDictionary.Create */ inline __fastcall virtual TOnDemandConverterDictionary(int Capacity)/* overload */ : TConverterDictionary(Capacity) { }
	/* TConverterDictionary.Destroy */ inline __fastcall virtual ~TOnDemandConverterDictionary() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TConverterManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TFetchConverterDictionary* FFetchConverters;
	TOnDemandConverterDictionary* FOnDemandConverters;
	
protected:
	virtual System::UnicodeString __fastcall GetDateFormat();
	virtual System::UnicodeString __fastcall GetTimeFormat();
	virtual System::UnicodeString __fastcall GetDateTimeFormat();
	
public:
	__fastcall TConverterManager();
	__fastcall virtual ~TConverterManager();
	__classmethod virtual System::Word __fastcall GetDBProvider();
	void __fastcall AddFetchConverter(TFetchConverter* FetchConverter)/* overload */;
	void __fastcall AddFetchConverter(System::Word DBType, System::Word RequiredDataType)/* overload */;
	void __fastcall AddFetchConverter(System::Word DBType, System::Word RequiredDataType, System::Word DestDataType, TCalcSizeFunc CalcLength = 0x0, TCalcSizeFunc CalcScale = 0x0)/* overload */;
	void __fastcall AddFetchConverter(System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, System::Word RequiredDataType)/* overload */;
	void __fastcall AddFetchConverter(System::Word DBType, int DBLengthMin, int DBLengthMax, int DBScaleMin, int DBScaleMax, System::Word RequiredDataType, System::Word DestDataType, TCalcSizeFunc CalcLength = 0x0, TCalcSizeFunc CalcScale = 0x0)/* overload */;
	void __fastcall ClearFetchMappers();
	void __fastcall AddOnDemandConverter(TOnDemandConverter* OnDemandConverter)/* overload */;
	void __fastcall AddOnDemandConverter(System::Word SourceType, System::Word DestType, Memdata::TConvertProcedure GetDataConverter, Memdata::TConvertProcedure PutDataConverter, TCalcSizeFunc CalcLength = 0x0, TCalcSizeFunc CalcScale = 0x0)/* overload */;
	void __fastcall ClearOnDemandConverters();
	void __fastcall CloneOnDemandConverter(System::Word SourceDataType, System::Word DestDataType, System::Word NewDataType);
	void __fastcall CloneOnDemandConverters(System::Word SourceDataType, System::Word NewDataType);
	TFetchConverter* __fastcall DetectFetchConverter(System::Word DBType, int DBLength, int DBScale, System::Word DestDataType);
	TOnDemandConverter* __fastcall DetectOnDemandConverter(System::Word SourceDataType, System::Word DestDataType);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSizeConverters : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod int __fastcall CopySize(int Value);
	__classmethod int __fastcall SizeX2(int Value);
	__classmethod int __fastcall SizeDiv2(int Value);
	__classmethod int __fastcall Size0(int Value);
	__classmethod int __fastcall Size20(int Value);
	__classmethod int __fastcall Size32(int Value);
	__classmethod int __fastcall GuidSize(int Value);
public:
	/* TObject.Create */ inline __fastcall TSizeConverters() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TSizeConverters() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDataConverters : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	__classmethod Memdata::TConvertStatus __fastcall CheckNumericStr(const System::UnicodeString Num, int DestLen)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall CheckNumericStr(const System::UnicodeString Num, int Precision, int Scale)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall ExtendNumericStr(System::UnicodeString &Num, int SourceScale, int DestLen);
	__classmethod System::UnicodeString __fastcall CheckDateTimeFormat(const System::UnicodeString Format, int DestLength);
	__classmethod System::UnicodeString __fastcall InternalInt8ToStr(void * Source);
	__classmethod System::UnicodeString __fastcall InternalUInt8ToStr(void * Source);
	__classmethod System::UnicodeString __fastcall InternalInt16ToStr(void * Source);
	__classmethod System::UnicodeString __fastcall InternalUInt16ToStr(void * Source);
	__classmethod System::UnicodeString __fastcall InternalInt32ToStr(void * Source);
	__classmethod System::UnicodeString __fastcall InternalUInt32ToStr(void * Source);
	__classmethod System::UnicodeString __fastcall InternalInt64ToStr(void * Source);
	__classmethod System::UnicodeString __fastcall InternalUInt64ToStr(void * Source);
	__classmethod Memdata::TConvertStatus __fastcall InternalSingleToStr(void * Source, /* out */ System::UnicodeString &Str, int DestLen)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalSingleToStr(void * Source, /* out */ System::UnicodeString &Str, int Precision, int Scale)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalFloatToStr(void * Source, /* out */ System::UnicodeString &Str, int DestLen)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalFloatToStr(void * Source, /* out */ System::UnicodeString &Str, int Precision, int Scale)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalExtendedToStr(void * Source, /* out */ System::UnicodeString &Str, int DestLen)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalExtendedToStr(void * Source, /* out */ System::UnicodeString &Str, int Precision, int Scale)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalBCDToStr(void * Source, /* out */ System::UnicodeString &Str, int DestLen)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalBCDToStr(void * Source, /* out */ System::UnicodeString &Str, int Precision, int Scale)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalFMTBCDToStr(void * Source, /* out */ System::UnicodeString &Str, int DestLen)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalFMTBCDToStr(void * Source, /* out */ System::UnicodeString &Str, int Precision, int Scale)/* overload */;
	__classmethod System::UnicodeString __fastcall InternalBoolToStr(void * Source);
	__classmethod Memdata::TConvertStatus __fastcall InternalDateToStr(void * Source, /* out */ System::UnicodeString &Str, const System::UnicodeString Format);
	__classmethod Memdata::TConvertStatus __fastcall InternalTimeToStr(void * Source, int SourceScale, /* out */ System::UnicodeString &Str, const System::UnicodeString Format);
	__classmethod Memdata::TConvertStatus __fastcall InternalDateTimeToStr(void * Source, int SourceScale, /* out */ System::UnicodeString &Str, const System::UnicodeString Format);
	__classmethod Memdata::TConvertStatus __fastcall InternalSQLTimeStampToStr(void * Source, int SourceScale, /* out */ System::UnicodeString &Str, const System::UnicodeString Format);
	__classmethod Memdata::TConvertStatus __fastcall InternalSQLTimeStampOffsetToStr(void * Source, int SourceScale, /* out */ System::UnicodeString &Str, const System::UnicodeString Format);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToNumber(const System::UnicodeString Str, /* out */ __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToUNumber(const System::UnicodeString Str, /* out */ unsigned __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToInt8(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToUInt8(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToInt16(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToUInt16(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToInt32(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToUInt32(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToInt64(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToUInt64(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToSingle(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToFloat(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToExtended(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToBCD(const System::UnicodeString Str, void * Dest, int DestLen, int DestScale, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToFMTBCD(const System::UnicodeString Str, void * Dest, int DestLen, int DestScale, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToBool(const System::UnicodeString Str, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToDate(const System::UnicodeString Str, const System::UnicodeString Format, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToTime(const System::UnicodeString Str, const System::UnicodeString Format, void * Dest, bool IgnoreConvertErrors);
	__classmethod bool __fastcall ConvertStrToDateTime(const System::UnicodeString Str, const System::UnicodeString Format, /* out */ System::TDateTime &dt);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToDateTime(const System::UnicodeString Str, const System::UnicodeString Format, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToSQLTimeStamp(const System::UnicodeString Str, const System::UnicodeString Format, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToSQLTimeStampOffset(const System::UnicodeString Str, const System::UnicodeString Format, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalStrToGuid(const System::UnicodeString Str, /* out */ int &SourceLen, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalExactCopyToBlob(void * Source, int SourceLen, void * Dest, int DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyToBlob(void * Source, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalAStrToMemo(const System::AnsiString AStr, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalWStrToWideMemo(const System::WideString WStr, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalExactCopyFromBlob(void * Source, void * Dest, int DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyFromBlobToBytes(void * Source, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyFromBlobToVarBytes(void * Source, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyFromBlobToExtBytes(Memdata::TStringHeap* StringHeap, void * Source, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyFromBlobToExtVarBytes(Memdata::TStringHeap* StringHeap, void * Source, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod void __fastcall InternalMemoToAStr(void * Source, int &SourceOffset, /* out */ int &SourceLen, int DestOffset, int &DestLen, /* out */ System::AnsiString &AStr);
	__classmethod void __fastcall InternalWideMemoToWStr(void * Source, int &SourceOffset, /* out */ int &SourceLen, int DestOffset, int DestLen, /* out */ System::WideString &WStr);
	__classmethod void __fastcall InternalBytesToAStr(void * Source, int &SourceLen, /* out */ System::AnsiString &AStr);
	__classmethod void __fastcall InternalBytesToWStr(void * Source, int &SourceLen, /* out */ System::WideString &WStr);
	__classmethod void __fastcall InternalVarBytesToAStr(void * Source, /* out */ System::AnsiString &AStr, /* out */ int &SourceLen);
	__classmethod void __fastcall InternalVarBytesToWStr(void * Source, /* out */ System::WideString &WStr, /* out */ int &SourceLen);
	__classmethod Memdata::TConvertStatus __fastcall InternalBytesToGuid(void * Source, int &SourceLen, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalExactCopyToBytes(void * Source, int SourceLen, void * Dest, int DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyToBytes(void * Source, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalExactCopyToVarBytes(void * Source, int SourceLen, void * Dest, int DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyToVarBytes(void * Source, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalExactCopyToExtBytes(Memdata::TStringHeap* StringHeap, void * Source, int SourceLen, void * Dest, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyToExtBytes(Memdata::TStringHeap* StringHeap, void * Source, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalExactCopyToExtVarBytes(Memdata::TStringHeap* StringHeap, void * Source, int SourceLen, void * Dest, int DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalCopyToExtVarBytes(Memdata::TStringHeap* StringHeap, void * Source, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalExactCopyFromBytes(void * Source, int SourceLen, void * Dest, int DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalExactCopyFromVarBytes(void * Source, void * Dest, int DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteAStr(const System::AnsiString AStr, /* out */ int &SourceLen, void * Dest, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteAStr(const System::AnsiString AStr, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalWritePAChar(char * AStr, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteExtAStr(Memdata::TStringHeap* StringHeap, const System::AnsiString AStr, /* out */ int &SourceLen, void * Dest, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteExtAStr(Memdata::TStringHeap* StringHeap, const System::AnsiString AStr, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteExtPAChar(Memdata::TStringHeap* StringHeap, char * AStr, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteWStr(const System::WideString WStr, /* out */ int &SourceLen, void * Dest, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteWStr(const System::WideString WStr, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalWritePWChar(System::WideChar * WStr, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteExtWStr(Memdata::TStringHeap* StringHeap, const System::WideString WStr, /* out */ int &SourceLen, void * Dest, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteExtWStr(Memdata::TStringHeap* StringHeap, const System::WideString WStr, int SourceOffset, /* out */ int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall InternalWriteExtPWChar(Memdata::TStringHeap* StringHeap, System::WideChar * WStr, int SourceOffset, int &SourceLen, void * Dest, int DestOffset, int &DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt32ToInt8(int Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt32ToUInt8(int Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt32ToInt16(int Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt32ToUInt16(int Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt32ToUInt32(int Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt32ToUInt64(int Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt64ToInt8(__int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt64ToUInt8(__int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt64ToInt16(__int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt64ToUInt16(__int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt64ToInt32(__int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt64ToUInt32(__int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalInt64ToUInt64(__int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalUInt64ToInt8(unsigned __int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalUInt64ToUInt8(unsigned __int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalUInt64ToInt16(unsigned __int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalUInt64ToUInt16(unsigned __int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalUInt64ToInt32(unsigned __int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalUInt64ToUInt32(unsigned __int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalUInt64ToInt64(unsigned __int64 Value, void * Dest, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalSingleToInt64(void * Source, /* out */ __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalSingleToUInt64(void * Source, /* out */ unsigned __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalFloatToInt64(void * Source, /* out */ __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalFloatToUInt64(void * Source, /* out */ unsigned __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalExtendedToInt64(void * Source, /* out */ __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalExtendedToUInt64(void * Source, /* out */ unsigned __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalBCDToInt64(void * Source, /* out */ __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalBCDToUInt64(void * Source, /* out */ unsigned __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalExtendedToBCD(System::Extended e, void * Dest, int DestLen, int DestScale, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalCurrencyToBCD(System::Extended e, void * Dest, int DestLen, int DestScale, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall WriteInt64AsBCD(__int64 i64, void * Dest, int DestLen, int DestScale, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall WriteUInt64AsBCD(unsigned __int64 ui64, void * Dest, int DestLen, int DestScale, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalFMTBCDToInt64(void * Source, /* out */ __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalFMTBCDToUInt64(void * Source, /* out */ unsigned __int64 &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalFMTBCDToBCDAsInt64(const Data::Fmtbcd::TBcd &Bcd, void * Dest, int DestLen, int DestScale, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalBCDToFMTBCD(const Data::Fmtbcd::TBcd &Bcd, void * Dest, int DestLen, int DestScale, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalGuidToBytes(void * Source, int SourceLen, void * Dest, int DestLen, bool IgnoreConvertErrors);
	__classmethod Memdata::TConvertStatus __fastcall InternalVariantToBytes(const System::Variant &v, /* out */ void * &Buf, /* out */ int &Size);
	__classmethod void __fastcall InternalTimeStampToSQLTimeStamp(const System::Sysutils::TTimeStamp &TS, void * Dest);
	__classmethod void __fastcall InternalTimeStampToSQLTimeStampOffset(const System::Sysutils::TTimeStamp &TS, void * Dest);
	__classmethod void __fastcall InternalSQLTimeStampToSQLTimeStampOffset(Data::Sqltimst::PSQLTimeStamp Source, Data::Sqltimst::PSQLTimeStampOffset Dest);
	__classmethod void __fastcall InternalSQLTimeStampOffsetToSQLTimeStamp(Data::Sqltimst::PSQLTimeStampOffset Source, Data::Sqltimst::PSQLTimeStamp Dest);
	__classmethod Memdata::TConvertStatus __fastcall InternalSQLTimeStampToTimeStamp(void * Source, /* out */ System::Sysutils::TTimeStamp &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalSQLTimeStampOffsetToTimeStamp(void * Source, /* out */ System::Sysutils::TTimeStamp &Value);
	__classmethod Memdata::TConvertStatus __fastcall InternalSQLTimeStampToDate(void * Source, /* out */ int &Date);
	__classmethod Memdata::TConvertStatus __fastcall InternalSQLTimeStampToTime(void * Source, /* out */ int &Time);
	__classmethod Memdata::TConvertStatus __fastcall ValidateSQLTimeStamp(Data::Sqltimst::PSQLTimeStamp PValue);
	__classmethod Memdata::TConvertStatus __fastcall ValidateSQLTimeStampOffset(Data::Sqltimst::PSQLTimeStampOffset PValue);
	__classmethod Memdata::TConvertStatus __fastcall InternalReadTimeStamp(void * Source, /* out */ System::Sysutils::TTimeStamp &ts);
	__classmethod Memdata::TConvertStatus __fastcall InternalReadSQLTimeStamp(void * Source, /* out */ Data::Sqltimst::TSQLTimeStamp &SQLTimeStamp);
	__classmethod Memdata::TConvertStatus __fastcall InternalReadSQLTimeStampOffset(void * Source, /* out */ Data::Sqltimst::TSQLTimeStampOffset &SQLTimeStampOffset);
	
public:
	__classmethod void __fastcall ChangeDecimalSeparator(System::UnicodeString &Num)/* overload */;
	__classmethod void __fastcall ChangeDecimalSeparator(System::UnicodeString &Num, const System::WideChar NewDecimalSeparator)/* overload */;
	__classmethod Memdata::TConvertStatus __fastcall CopyByte(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall CopyInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall CopyInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall CopyInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall CopyPtr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int8ToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt8ToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int16ToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt16ToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int32ToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt32ToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall Int64ToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall UInt64ToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SingleToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FloatToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall CurrencyToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtendedToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BCDToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall FMTBCDToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BoolToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall TimeToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall DateTimeToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall SQLTimeStampOffsetToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToMemo(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToWideMemo(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall AStrToGuid(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToMemo(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToWideMemo(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WStrToGuid(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToMemo(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToWideMemo(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtAStrToGuid(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToMemo(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToWideMemo(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtWStrToGuid(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BlobToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall MemoToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WideMemoToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WideMemoToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WideMemoToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall WideMemoToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall BytesToGuid(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VarBytesToGuid(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtVarBytesToGuid(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToVariant(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall ExtBytesToGuid(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToUInt8(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToUInt16(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToUInt32(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToUInt64(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToSingle(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToFloat(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToExtended(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToFMTBCD(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToBool(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToDate(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToDateTime(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToSQLTimeStamp(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToSQLTimeStampOffset(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall VariantToBlob(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall GuidToAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall GuidToWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall GuidToExtAStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall GuidToExtWStr(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall GuidToBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall GuidToVarBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall GuidToExtBytes(Memdata::TConvertInfo &ConvertInfo);
	__classmethod Memdata::TConvertStatus __fastcall GuidToExtVarBytes(Memdata::TConvertInfo &ConvertInfo);
public:
	/* TObject.Create */ inline __fastcall TDataConverters() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDataConverters() { }
	
};

#pragma pack(pop)

typedef System::StaticArray<System::UnicodeString, 18> Crdatatypemap__12;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::StaticArray<unsigned, 4> DecValue;
extern DELPHI_PACKAGE Crdatatypemap__12 ConvertStatusErrors;
extern DELPHI_PACKAGE Data::Fmtbcd::TBcd Bcd_0;
extern DELPHI_PACKAGE TDBTypeInfos* DBTypeInfos;
extern DELPHI_PACKAGE Memdata::TConvertStatus __fastcall Max(Memdata::TConvertStatus V1, Memdata::TConvertStatus V2);
}	/* namespace Crdatatypemap */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRDATATYPEMAP)
using namespace Crdatatypemap;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrdatatypemapHPP
