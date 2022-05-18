// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteFunctionVirtual.pas' rev: 35.00 (Windows)

#ifndef LitefunctionvirtualHPP
#define LitefunctionvirtualHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRAccess.hpp>
#include <DAConsts.hpp>
#include <MemUtils.hpp>
#include <LiteCallVirtual.hpp>
#include <LiteErrorVirtual.hpp>

//-- user supplied -----------------------------------------------------------

namespace Litefunctionvirtual
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomLiteFunctionDesc;
class DELPHICLASS TLiteFunctionDesc;
class DELPHICLASS TSQLiteFunctionManager;
//-- type declarations -------------------------------------------------------
typedef System::Variant __fastcall (*TLiteFunction)(System::Variant *InValues, const int InValues_High);

typedef void __fastcall (*TLiteStepFunction)(System::Variant *InValues, const int InValues_High);

typedef System::Variant __fastcall (*TLiteFinalFunction)(void);

typedef void __fastcall (__closure *TLiteFunctionMethod)(System::Variant *InValues, const int InValues_High, System::Variant &ResultValue);

typedef void __fastcall (__closure *TLiteStepFunctionMethod)(System::Variant *InValues, const int InValues_High);

typedef void __fastcall (__closure *TLiteFinalFunctionMethod)(System::Variant &ResultValue);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomLiteFunctionDesc : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Craccess::TCRConnection* FConnection;
	System::UnicodeString FName;
	int FParamCount;
	int FTextRepresentation;
	virtual void __fastcall DoRegister(const char * pName, const void * pSelf) = 0 ;
	virtual void __fastcall DoRegister387(const char * pName, const void * pSelf) = 0 ;
	virtual void __fastcall DoRegister3250(const char * pName, const void * pSelf) = 0 ;
	
protected:
	void __fastcall RegisterFunction();
	void __fastcall UnregisterFunction();
	Crtypes::TVariantArray __fastcall GetInParams(void * Context, int ParamCount, void * pData);
	void __fastcall SetResult(void * Context, const System::Variant &Value);
	virtual void __fastcall DoFunction(void * Context, int ParamCount, void * pData) = 0 ;
	virtual void __fastcall DoStep(void * Context, int ParamCount, void * pData) = 0 ;
	virtual void __fastcall DoFinal(void * Context) = 0 ;
	
public:
	__fastcall TCustomLiteFunctionDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, int ParamCount);
	__fastcall virtual ~TCustomLiteFunctionDesc();
	__property System::UnicodeString Name = {read=FName};
	__property int ParamCount = {read=FParamCount, nodefault};
	__property int TextRepresentation = {read=FTextRepresentation, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TLiteFunctionDesc : public TCustomLiteFunctionDesc
{
	typedef TCustomLiteFunctionDesc inherited;
	
private:
	TLiteFunction FLiteFunction;
	TLiteStepFunction FStepFunction;
	TLiteFinalFunction FFinalFunction;
	TLiteFunctionMethod FLiteFunctionMethod;
	TLiteStepFunctionMethod FStepFunctionMethod;
	TLiteFinalFunctionMethod FFinalFunctionMethod;
	virtual void __fastcall DoRegister(const char * pName, const void * pSelf);
	virtual void __fastcall DoRegister387(const char * pName, const void * pSelf);
	virtual void __fastcall DoRegister3250(const char * pName, const void * pSelf);
	
protected:
	virtual void __fastcall DoFunction(void * Context, int ParamCount, void * pData);
	virtual void __fastcall DoStep(void * Context, int ParamCount, void * pData);
	virtual void __fastcall DoFinal(void * Context);
	
public:
	__fastcall TLiteFunctionDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, int ParamCount, TLiteFunction LiteFunction)/* overload */;
	__fastcall TLiteFunctionDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, int ParamCount, TLiteStepFunction StepFunction, TLiteFinalFunction FinalFunction)/* overload */;
	__fastcall TLiteFunctionDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, int ParamCount, TLiteFunctionMethod LiteFunctionMethod)/* overload */;
	__fastcall TLiteFunctionDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, int ParamCount, TLiteStepFunctionMethod StepFunctionMethod, TLiteFinalFunctionMethod FinalFunctionMethod)/* overload */;
	__property TLiteFunction LiteFunction = {read=FLiteFunction, write=FLiteFunction};
public:
	/* TCustomLiteFunctionDesc.Destroy */ inline __fastcall virtual ~TLiteFunctionDesc() { }
	
};


typedef System::TMetaClass* TLiteFunctionDescClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteFunctionManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Craccess::TCRConnection* FConnection;
	Crtypes::TCRObjectList* FFunctionList;
	
protected:
	virtual TLiteFunctionDescClass __fastcall GetFunctionDescClass();
	void __fastcall InternalAddFunction(TCustomLiteFunctionDesc* LiteFunctionDesc);
	void __fastcall InternalRemoveFunction(TCustomLiteFunctionDesc* LiteFunctionDesc);
	TCustomLiteFunctionDesc* __fastcall FindFunction(const System::UnicodeString Name, int ParamCount);
	
public:
	__fastcall TSQLiteFunctionManager(Craccess::TCRConnection* Connection);
	__fastcall virtual ~TSQLiteFunctionManager();
	void __fastcall RegisterFunction(const System::UnicodeString Name, int ParamCount, TLiteFunction LiteFunction)/* overload */;
	void __fastcall RegisterAggregateFunction(const System::UnicodeString Name, int ParamCount, TLiteStepFunction StepFunction, TLiteFinalFunction FinalFunction)/* overload */;
	void __fastcall RegisterFunction(const System::UnicodeString Name, int ParamCount, TLiteFunctionMethod LiteFunctionMethod)/* overload */;
	void __fastcall RegisterAggregateFunction(const System::UnicodeString Name, int ParamCount, TLiteStepFunctionMethod StepFunctionMethod, TLiteFinalFunctionMethod FinalFunctionMethod)/* overload */;
	void __fastcall UnRegisterFunction(const System::UnicodeString Name, int ParamCount);
	void __fastcall RegistrAllFunctions();
	void __fastcall UnRegistrAllFunctions();
};

#pragma pack(pop)

typedef System::TMetaClass* TSQLiteFunctionManagerClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Litefunctionvirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITEFUNCTIONVIRTUAL)
using namespace Litefunctionvirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LitefunctionvirtualHPP
