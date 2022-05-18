// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBToolsIntf.pas' rev: 34.00 (Windows)

#ifndef DbtoolsintfHPP
#define DbtoolsintfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dbtoolsintf
{
//-- forward type declarations -----------------------------------------------
struct ConnectionInfo;
struct CommandParameterInfo;
__interface DELPHIINTERFACE ISqlEditor;
typedef System::DelphiInterface<ISqlEditor> _di_ISqlEditor;
__interface DELPHIINTERFACE ISqlSource;
typedef System::DelphiInterface<ISqlSource> _di_ISqlSource;
__interface DELPHIINTERFACE ISqlSourceNotifier;
typedef System::DelphiInterface<ISqlSourceNotifier> _di_ISqlSourceNotifier;
__interface DELPHIINTERFACE IDbToolsService;
typedef System::DelphiInterface<IDbToolsService> _di_IDbToolsService;
__interface DELPHIINTERFACE IDbForgeServiceEvents;
typedef System::DelphiInterface<IDbForgeServiceEvents> _di_IDbForgeServiceEvents;
__interface DELPHIINTERFACE IDbForgeService;
typedef System::DelphiInterface<IDbForgeService> _di_IDbForgeService;
//-- type declarations -------------------------------------------------------
typedef Winapi::Activex::TOleEnum ParameterType;

#pragma pack(push,1)
struct DECLSPEC_DRECORD ConnectionInfo
{
public:
	System::WideString Name;
	System::WideString ConnectionString;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD CommandParameterInfo
{
public:
	System::WideChar *Name;
	Winapi::Activex::TOleEnum ParameterType;
	int DataType;
	unsigned _slack_space;
	System::OleVariant Value;
};
#pragma pack(pop)


typedef Winapi::Activex::PSafeArray TConnectionInfoArray;

__interface  INTERFACE_UUID("{471AE99C-AE21-491B-89E3-A2C717C81CA9}") ISqlEditor  : public System::IInterface 
{
	virtual int __stdcall get_Handle() = 0 ;
	virtual System::WideChar * __stdcall get_Text() = 0 ;
	virtual void __stdcall set_Text(const System::WideChar * Param1) = 0 ;
	virtual System::LongBool __stdcall get_Modified() = 0 ;
	virtual void __stdcall set_Modified(System::LongBool Param1) = 0 ;
	virtual System::LongBool __stdcall get_ReadOnly() = 0 ;
	virtual void __stdcall set_ReadOnly(System::LongBool Param1) = 0 ;
	virtual void __stdcall SetConnection(const System::WideString ConnectionString) = 0 ;
	__property System::LongBool Modified = {read=get_Modified, write=set_Modified};
	__property int Handle = {read=get_Handle};
	__property System::LongBool ReadOnly = {read=get_ReadOnly, write=set_ReadOnly};
	__property System::WideChar * Text = {read=get_Text, write=set_Text};
};

__interface  INTERFACE_UUID("{471AE99C-AE21-491B-89E3-A2C717C81CA8}") ISqlSource  : public System::IInterface 
{
	virtual System::WideChar * __stdcall Get_Name() = 0 ;
	virtual System::WideChar * __stdcall Get_ConnectionString() = 0 ;
	virtual System::WideChar * __stdcall Get_DesignerName() = 0 ;
	virtual int __stdcall Get_ParameterCount() = 0 ;
	virtual void __stdcall Set_ParameterCount(const int Param1) = 0 ;
	virtual System::WideChar * __stdcall Get_Sql() = 0 ;
	virtual void __stdcall Set_Sql(const System::WideChar * Param1) = 0 ;
	virtual void __stdcall Close() = 0 ;
	virtual void __stdcall GetParameter(const int index, /* out */ CommandParameterInfo &Info) = 0 ;
	virtual void __stdcall SetParameter(const int index, CommandParameterInfo Info) = 0 ;
	virtual System::WideChar * __stdcall Get_ID() = 0 ;
};

__interface  INTERFACE_UUID("{D4858B82-70FB-4E30-8BE5-30CF625A37E2}") ISqlSourceNotifier  : public System::IInterface 
{
	virtual void __stdcall OnSqlSourceChanged() = 0 ;
	virtual void __stdcall OnSqlSourceDeleted() = 0 ;
	virtual void __stdcall OnSqlSourceRenamed(const System::WideString prevName) = 0 ;
};

__interface  INTERFACE_UUID("{7FB8EF3F-AB68-48D1-9DD5-F85C05DF4A90}") IDbToolsService  : public System::IInterface 
{
	virtual void __stdcall CreateSqlEditor(/* out */ _di_ISqlEditor &editor) = 0 ;
	virtual void __stdcall DesignerClosing(const System::WideString DesignerName) = 0 ;
	virtual void __stdcall EditDatabaseObject(const System::WideString ConnectionString, const System::WideString objectType, const System::WideString fullName) = 0 ;
	virtual void __stdcall EditSql(const _di_ISqlSource sqlSource, System::LongBool asQuery, /* out */ _di_ISqlSourceNotifier &notifier) = 0 ;
	virtual void __stdcall ExecuteSql(const _di_ISqlSource sqlSource, System::LongBool debug) = 0 ;
	virtual void __stdcall FindInDatabaseExplorer(const System::WideString ConnectionString, const System::WideString objectType, const System::WideString fullName) = 0 ;
	virtual Winapi::Activex::PSafeArray __stdcall GetConnections() = 0 ;
	virtual void __stdcall RetrieveData(const _di_ISqlSource sqlSource, System::LongBool asDocument) = 0 ;
};

__interface  INTERFACE_UUID("{55CBF937-D3F9-486F-B34A-A435F4B9FCCC}") IDbForgeServiceEvents  : public System::IInterface 
{
	virtual void __stdcall OnDeactivate() = 0 ;
};

__interface  INTERFACE_UUID("{2A4633FC-8980-4F4F-BBE6-B300670748FB}") IDbForgeService  : public System::IInterface 
{
	virtual void __stdcall CreateSqlEditor(const _di_IDbForgeServiceEvents events, /* out */ _di_ISqlEditor &editor) = 0 ;
	virtual void __stdcall DesignerClosing(const System::WideString DesignerName) = 0 ;
	virtual void __stdcall EditDatabaseObject(const System::WideString ConnectionString, const System::WideString objectType, const System::WideString fullName) = 0 ;
	virtual void __stdcall EditSql(const _di_ISqlSource sqlSource, System::LongBool asQuery, /* out */ _di_ISqlSourceNotifier &notifier) = 0 ;
	virtual void __stdcall ExecuteSql(const _di_ISqlSource sqlSource, System::LongBool debug, /* out */ _di_ISqlSourceNotifier &notifier) = 0 ;
	virtual void __stdcall FindInDatabaseExplorer(const System::WideString ConnectionString, const System::WideString objectType, const System::WideString fullName) = 0 ;
	virtual Winapi::Activex::PSafeArray __stdcall GetConnections() = 0 ;
	virtual void __stdcall RetrieveData(const _di_ISqlSource sqlSource, System::LongBool asDocument) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
static const Winapi::Activex::TOleEnum ParameterType_Input = Winapi::Activex::TOleEnum(0x0);
static const Winapi::Activex::TOleEnum ParameterType_InputOutput = Winapi::Activex::TOleEnum(0x1);
static const Winapi::Activex::TOleEnum ParameterType_Output = Winapi::Activex::TOleEnum(0x2);
static const Winapi::Activex::TOleEnum ParameterType_ReturnValue = Winapi::Activex::TOleEnum(0x3);
static const System::Int8 Devart_DbTools_InterfacesMajorVersion = System::Int8(0x2);
static const System::Int8 Devart_DbTools_InterfacesMinorVersion = System::Int8(0x0);
}	/* namespace Dbtoolsintf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DBTOOLSINTF)
using namespace Dbtoolsintf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DbtoolsintfHPP
