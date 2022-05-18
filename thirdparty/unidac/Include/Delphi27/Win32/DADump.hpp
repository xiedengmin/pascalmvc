// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DADump.pas' rev: 34.00 (Windows)

#ifndef DadumpHPP
#define DadumpHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Variants.hpp>
#include <Data.DB.hpp>
#include <Data.SqlTimSt.hpp>
#include <CRTypes.hpp>
#include <CLRClasses.hpp>
#include <CRAccess.hpp>
#include <DBAccess.hpp>
#include <DAScript.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dadump
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDADumpOptions;
class DELPHICLASS TDADumpProcessor;
class DELPHICLASS TDADump;
class DELPHICLASS TDADumpUtils;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TDADumpProcessorClass;

typedef void __fastcall (__closure *TDABackupProgressEvent)(System::TObject* Sender, System::UnicodeString ObjectName, int ObjectNum, int ObjectCount, int Percent);

typedef void __fastcall (__closure *TDARestoreProgressEvent)(System::TObject* Sender, int Percent);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADumpOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	TDADump* FOwner;
	bool FGenerateHeader;
	bool FAddDrop;
	bool FQuoteNames;
	bool FCompleteInsert;
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
public:
	__fastcall TDADumpOptions(TDADump* Owner);
	
__published:
	__property bool GenerateHeader = {read=FGenerateHeader, write=FGenerateHeader, default=1};
	__property bool AddDrop = {read=FAddDrop, write=FAddDrop, default=1};
	__property bool QuoteNames = {read=FQuoteNames, write=FQuoteNames, default=0};
	__property bool CompleteInsert = {read=FCompleteInsert, write=FCompleteInsert, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDADumpOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADumpProcessor : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TDADump* FOwner;
	Dbaccess::TCustomDADataSet* FQuery;
	Dbaccess::TCustomDAConnection* __fastcall GetConnection();
	System::Classes::TStrings* __fastcall GetSQL();
	System::Classes::TStringList* __fastcall GetTables();
	System::Classes::TStringList* __fastcall GetTriggers();
	System::Classes::TStringList* __fastcall GetStoredProcs();
	System::Classes::TStream* __fastcall GetStream();
	virtual void __fastcall Add(const System::UnicodeString Line)/* overload */;
	void __fastcall Add(System::Classes::TStringList* const sl)/* overload */;
	void __fastcall AddLineToSQL(const System::UnicodeString Line)/* overload */;
	void __fastcall AddLineToSQL(const System::UnicodeString Line, const System::TVarRec *Args, const int Args_High)/* overload */;
	virtual void __fastcall Backup(System::UnicodeString Query);
	virtual void __fastcall CheckTables(const System::UnicodeString QueryText);
	void __fastcall AddHeader();
	virtual Dbaccess::TCustomDADataSet* __fastcall CreateQuery();
	virtual Dbaccess::TCustomDADataSet* __fastcall CreateDataQuery();
	void __fastcall CheckQuery();
	virtual void __fastcall AddSettings();
	virtual void __fastcall RestoreSettings();
	virtual void __fastcall BackupObjects(const System::UnicodeString QueryText);
	virtual void __fastcall BackupData(const System::UnicodeString TableName, const System::UnicodeString QueryText, int TableNum, int TableCount);
	virtual System::UnicodeString __fastcall GetFieldValueForDump(Data::Db::TField* Field);
	void __fastcall DoBackupProgress(const System::UnicodeString ObjectName, int ObjectNum, int ObjectCount, int Percent);
	virtual Craccess::TSQLInfo* __fastcall SQLInfo();
	virtual System::UnicodeString __fastcall QuoteName(const System::UnicodeString AName);
	
public:
	__fastcall virtual TDADumpProcessor(TDADump* Owner);
	__fastcall virtual ~TDADumpProcessor();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TDADump : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	TDADumpProcessor* FProcessor;
	Dbaccess::TCustomDAConnection* FConnection;
	System::Classes::TStrings* FSQL;
	System::Classes::TStream* FStream;
	TDADumpOptions* FOptions;
	bool FDebug;
	bool FDesignCreate;
	System::Classes::TStringList* FTables;
	System::Classes::TStringList* FTriggers;
	System::Classes::TStringList* FStoredProcs;
	TDABackupProgressEvent FOnBackupProgress;
	TDARestoreProgressEvent FOnRestoreProgress;
	Dascript::TOnErrorEvent FOnError;
	virtual TDADumpProcessorClass __fastcall GetProcessorClass();
	virtual void __fastcall SetProcessor(TDADumpProcessor* Value);
	void __fastcall CreateProcessor();
	void __fastcall FreeProcessor();
	void __fastcall CheckProcessor();
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual System::UnicodeString __fastcall GetTableNames();
	virtual void __fastcall SetTableNames(System::UnicodeString Value);
	virtual TDADumpOptions* __fastcall CreateOptions();
	virtual Dascript::TDAScript* __fastcall CreateScript();
	virtual void __fastcall Notification(System::Classes::TComponent* Component, System::Classes::TOperation Operation);
	void __fastcall SetConnection(Dbaccess::TCustomDAConnection* Value);
	virtual void __fastcall BeginConnection();
	void __fastcall EndConnection();
	void __fastcall SetSQL(System::Classes::TStrings* Value);
	void __fastcall SetOptions(TDADumpOptions* Value);
	virtual void __fastcall Loaded();
	void __fastcall InternalBackup(const System::UnicodeString Query);
	virtual System::UnicodeString __fastcall GenerateHeader() = 0 ;
	__property TDADumpProcessor* Processor = {read=FProcessor};
	
public:
	__fastcall virtual TDADump(System::Classes::TComponent* Owner);
	__fastcall virtual ~TDADump();
	void __fastcall Backup();
	void __fastcall BackupToStream(System::Classes::TStream* Stream, const System::UnicodeString Query = System::UnicodeString());
	void __fastcall BackupToFile(const System::UnicodeString FileName, const System::UnicodeString Query = System::UnicodeString());
	void __fastcall BackupQuery(const System::UnicodeString Query);
	void __fastcall Restore();
	void __fastcall RestoreFromStream(System::Classes::TStream* Stream);
	void __fastcall RestoreFromFile(const System::UnicodeString FileName);
	__property Dbaccess::TCustomDAConnection* Connection = {read=FConnection, write=SetConnection};
	__property TDADumpOptions* Options = {read=FOptions, write=SetOptions};
	
__published:
	__property System::UnicodeString TableNames = {read=GetTableNames, write=SetTableNames};
	__property System::Classes::TStrings* SQL = {read=FSQL, write=SetSQL};
	__property bool Debug = {read=FDebug, write=FDebug, default=0};
	__property TDABackupProgressEvent OnBackupProgress = {read=FOnBackupProgress, write=FOnBackupProgress};
	__property TDARestoreProgressEvent OnRestoreProgress = {read=FOnRestoreProgress, write=FOnRestoreProgress};
	__property Dascript::TOnErrorEvent OnError = {read=FOnError, write=FOnError};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDADumpUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall SetDesignCreate(TDADump* Obj, bool Value);
	__classmethod bool __fastcall GetDesignCreate(TDADump* Obj);
public:
	/* TObject.Create */ inline __fastcall TDADumpUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDADumpUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Dadump */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DADUMP)
using namespace Dadump;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DadumpHPP
