// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'LiteCollationVirtual.pas' rev: 35.00 (Windows)

#ifndef LitecollationvirtualHPP
#define LitecollationvirtualHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRAccess.hpp>
#include <MemUtils.hpp>
#include <LiteCallVirtual.hpp>
#include <LiteErrorVirtual.hpp>

//-- user supplied -----------------------------------------------------------

namespace Litecollationvirtual
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomLiteCollationDesc;
class DELPHICLASS TLiteCollationDesc;
class DELPHICLASS TLiteAnsiCollationDesc;
class DELPHICLASS TLiteWideCollationDesc;
class DELPHICLASS TSQLiteCollationManager;
//-- type declarations -------------------------------------------------------
typedef int __fastcall (*TLiteCollation)(const System::UnicodeString Str1, const System::UnicodeString Str2);

typedef int __fastcall (__closure *TLiteCollationMethod)(const System::UnicodeString Str1, const System::UnicodeString Str2);

typedef int __fastcall (*TLiteAnsiCollation)(const System::AnsiString Str1, const System::AnsiString Str2);

typedef int __fastcall (__closure *TLiteAnsiCollationMethod)(const System::AnsiString Str1, const System::AnsiString Str2);

typedef int __fastcall (*TLiteWideCollation)(const System::WideString Str1, const System::WideString Str2);

typedef int __fastcall (__closure *TLiteWideCollationMethod)(const System::WideString Str1, const System::WideString Str2);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomLiteCollationDesc : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Craccess::TCRConnection* FConnection;
	System::UnicodeString FName;
	int FTextRepresentation;
	
protected:
	void __fastcall RegisterCollation();
	void __fastcall UnregisterCollation();
	System::AnsiString __fastcall GetAnsiStr(int StrSize, const void * pStr);
	System::WideString __fastcall GetWideStr(int StrSize, const void * pStr);
	virtual int __fastcall DoCollate(int StrSize1, const void * pStr1, int StrSize2, const void * pStr2) = 0 ;
	
public:
	__fastcall TCustomLiteCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name)/* overload */;
	__fastcall virtual ~TCustomLiteCollationDesc();
	__property System::UnicodeString Name = {read=FName};
	__property int TextRepresentation = {read=FTextRepresentation, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TLiteCollationDesc : public TCustomLiteCollationDesc
{
	typedef TCustomLiteCollationDesc inherited;
	
private:
	TLiteCollation FLiteCollation;
	TLiteCollationMethod FLiteCollationMethod;
	
protected:
	virtual int __fastcall DoCollate(int StrSize1, const void * pStr1, int StrSize2, const void * pStr2);
	
public:
	__fastcall TLiteCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, TLiteCollation LiteCollation)/* overload */;
	__fastcall TLiteCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, TLiteCollationMethod LiteCollation)/* overload */;
	__property TLiteCollation LiteCollation = {read=FLiteCollation, write=FLiteCollation};
public:
	/* TCustomLiteCollationDesc.Create */ inline __fastcall TLiteCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name)/* overload */ : TCustomLiteCollationDesc(Connection, Name) { }
	/* TCustomLiteCollationDesc.Destroy */ inline __fastcall virtual ~TLiteCollationDesc() { }
	
};


class PASCALIMPLEMENTATION TLiteAnsiCollationDesc : public TCustomLiteCollationDesc
{
	typedef TCustomLiteCollationDesc inherited;
	
private:
	TLiteAnsiCollation FLiteAnsiCollation;
	TLiteAnsiCollationMethod FLiteAnsiCollationMethod;
	
protected:
	virtual int __fastcall DoCollate(int StrSize1, const void * pStr1, int StrSize2, const void * pStr2);
	
public:
	__fastcall TLiteAnsiCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, TLiteAnsiCollation LiteAnsiCollation)/* overload */;
	__fastcall TLiteAnsiCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, TLiteAnsiCollationMethod LiteAnsiCollation)/* overload */;
	__property TLiteAnsiCollation LiteAnsiCollation = {read=FLiteAnsiCollation, write=FLiteAnsiCollation};
public:
	/* TCustomLiteCollationDesc.Create */ inline __fastcall TLiteAnsiCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name)/* overload */ : TCustomLiteCollationDesc(Connection, Name) { }
	/* TCustomLiteCollationDesc.Destroy */ inline __fastcall virtual ~TLiteAnsiCollationDesc() { }
	
};


class PASCALIMPLEMENTATION TLiteWideCollationDesc : public TCustomLiteCollationDesc
{
	typedef TCustomLiteCollationDesc inherited;
	
private:
	TLiteWideCollation FLiteWideCollation;
	TLiteWideCollationMethod FLiteWideCollationMethod;
	
protected:
	virtual int __fastcall DoCollate(int StrSize1, const void * pStr1, int StrSize2, const void * pStr2);
	
public:
	__fastcall TLiteWideCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, TLiteWideCollation LiteWideCollation)/* overload */;
	__fastcall TLiteWideCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name, TLiteWideCollationMethod LiteWideCollation)/* overload */;
	__property TLiteWideCollation LiteWideCollation = {read=FLiteWideCollation, write=FLiteWideCollation};
public:
	/* TCustomLiteCollationDesc.Create */ inline __fastcall TLiteWideCollationDesc(Craccess::TCRConnection* Connection, const System::UnicodeString Name)/* overload */ : TCustomLiteCollationDesc(Connection, Name) { }
	/* TCustomLiteCollationDesc.Destroy */ inline __fastcall virtual ~TLiteWideCollationDesc() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TSQLiteCollationManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Craccess::TCRConnection* FConnection;
	Crtypes::TCRObjectList* FCollationList;
	
protected:
	void __fastcall InternalAddCollation(TCustomLiteCollationDesc* LiteCollationDesc);
	void __fastcall InternalRemoveCollation(TCustomLiteCollationDesc* LiteCollationDesc);
	TCustomLiteCollationDesc* __fastcall FindCollation(System::UnicodeString Name);
	
public:
	__fastcall TSQLiteCollationManager(Craccess::TCRConnection* Connection);
	__fastcall virtual ~TSQLiteCollationManager();
	void __fastcall RegisterCollation(const System::UnicodeString Name, TLiteCollation LiteCollation)/* overload */;
	void __fastcall RegisterCollation(const System::UnicodeString Name, TLiteCollationMethod LiteCollation)/* overload */;
	void __fastcall UnRegisterCollation(const System::UnicodeString Name);
	void __fastcall RegisterAnsiCollation(const System::UnicodeString Name, TLiteAnsiCollation LiteAnsiCollation)/* overload */;
	void __fastcall RegisterAnsiCollation(const System::UnicodeString Name, TLiteAnsiCollationMethod LiteAnsiCollation)/* overload */;
	void __fastcall UnRegisterAnsiCollation(const System::UnicodeString Name);
	void __fastcall RegisterWideCollation(const System::UnicodeString Name, TLiteWideCollation LiteWideCollation)/* overload */;
	void __fastcall RegisterWideCollation(const System::UnicodeString Name, TLiteWideCollationMethod LiteWideCollation)/* overload */;
	void __fastcall UnRegisterWideCollation(const System::UnicodeString Name);
	void __fastcall RegisterDefaultCollations();
	void __fastcall UnRegisterDefaultCollations();
	void __fastcall UnRegistrAllCollations();
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Litecollationvirtual */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_LITECOLLATIONVIRTUAL)
using namespace Litecollationvirtual;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// LitecollationvirtualHPP
