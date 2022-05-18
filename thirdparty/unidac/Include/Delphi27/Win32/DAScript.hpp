// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DAScript.pas' rev: 34.00 (Windows)

#ifndef DascriptHPP
#define DascriptHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.Variants.hpp>
#include <Data.DB.hpp>
#include <CLRClasses.hpp>
#include <CRTypes.hpp>
#include <CRParser.hpp>
#include <DBAccess.hpp>

//-- user supplied -----------------------------------------------------------

namespace Dascript
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TDAStatement;
class DELPHICLASS TDAStatements;
class DELPHICLASS TDAScriptProcessor;
class DELPHICLASS TDAScript;
class DELPHICLASS TDAScriptUtils;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAStatement : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
protected:
	bool FOmit;
	int FStatementType;
	int FStartPos;
	int FEndPos;
	int FStartLine;
	int FEndLine;
	int FStartOffset;
	int FEndOffset;
	Dbaccess::TDAParams* FParams;
	System::UnicodeString __fastcall GetSQL();
	TDAScript* __fastcall GetScript();
	Dbaccess::TDAParams* __fastcall GetParams();
	Dbaccess::TDAParams* __fastcall CreateParams();
	
public:
	__fastcall virtual ~TDAStatement();
	__property TDAScript* Script = {read=GetScript};
	__property System::UnicodeString SQL = {read=GetSQL};
	__property bool Omit = {read=FOmit, write=FOmit, nodefault};
	__property int StartPos = {read=FStartPos, nodefault};
	__property int EndPos = {read=FEndPos, nodefault};
	__property int StartLine = {read=FStartLine, nodefault};
	__property int EndLine = {read=FEndLine, nodefault};
	__property int StartOffset = {read=FStartOffset, nodefault};
	__property int EndOffset = {read=FEndOffset, nodefault};
	__property Dbaccess::TDAParams* Params = {read=GetParams};
	void __fastcall Execute();
public:
	/* TCollectionItem.Create */ inline __fastcall virtual TDAStatement(System::Classes::TCollection* Collection) : System::Classes::TCollectionItem(Collection) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDAStatementClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAStatements : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TDAStatement* operator[](int Index) { return this->Items[Index]; }
	
protected:
	TDAScript* FScript;
	HIDESBASE TDAStatement* __fastcall GetItem(int Index);
	TDAStatement* __fastcall CreateStatement(int StatementType, bool Omit, int StartPos, int EndPos, int StartLine, int EndLine, int StartOffset, int EndOffset);
	
public:
	__fastcall TDAStatements(System::Classes::TCollectionItemClass ItemClass, TDAScript* Script);
	__property TDAStatement* Items[int Index] = {read=GetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TDAStatements() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TDAStatementsClass;

enum DECLSPEC_DENUM TDelimiterState : unsigned char { dsNone, dsDelimiter, dsBlank, dsValue, dsSet };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAScriptProcessor : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TDAScript* FOwner;
	Crparser::TSQLParser* FParser;
	Crparser::TSQLParser* FSQLParser;
	System::UnicodeString FCurrDelimiter;
	int FCurrDelimiterLength;
	TDelimiterState FDelimiterState;
	System::UnicodeString FSt;
	int FCurrentStatementIdx;
	bool FStatementsPopulating;
	Clrclasses::WideStringBuilder* FSQL;
	Dbaccess::TCustomDAConnection* __fastcall UsedConnection();
	Dbaccess::TCustomDASQL* __fastcall GetCommand();
	virtual Crparser::TSQLParserClass __fastcall GetParserClass();
	Crparser::TSQLParser* __fastcall CreateParser(const System::UnicodeString Text)/* overload */;
	Crparser::TSQLParser* __fastcall CreateParser(System::Classes::TStream* Stream)/* overload */;
	Crparser::TSQLParser* __fastcall GetSQLParser(const System::UnicodeString Text);
	virtual bool __fastcall ExecuteNext();
	virtual void __fastcall ExecuteStatement(const System::UnicodeString SQL, int StatementType, bool &Omit, /* out */ bool &BreakExec, Dbaccess::TDAParams* Params = (Dbaccess::TDAParams*)(0x0));
	virtual void __fastcall CreateStatement(int StatementType, bool Omit, int StartPos, int EndPos, int StartLine, int EndLine, int StartOffset, int EndOffset);
	virtual void __fastcall BreakExec();
	virtual void __fastcall Reset();
	virtual void __fastcall CheckLexem(int Code, int &StatementType, bool &Omit);
	virtual bool __fastcall GetReady(int Code);
	virtual bool __fastcall IsSpecificSQL(int StatementType);
	virtual bool __fastcall CanOptimize(const System::UnicodeString SQL, const int StatementType);
	virtual bool __fastcall IsBlankEndsDelimeter();
	virtual bool __fastcall SlashIsDelimiter();
	virtual void __fastcall DoBeforeStatementExecute(System::UnicodeString &SQL, int StatementType, bool &Omit);
	virtual void __fastcall DoAfterStatementExecute(System::UnicodeString &SQL, int StatementType);
	
public:
	__fastcall virtual TDAScriptProcessor(TDAScript* Owner);
	__fastcall virtual ~TDAScriptProcessor();
	virtual bool __fastcall SetProp(int Prop, const System::Variant &Value);
	virtual bool __fastcall GetProp(int Prop, System::Variant &Value);
	void __fastcall Init(System::Classes::TStream* Stream);
	Clrclasses::Encoding* __fastcall GetEncoding();
};

#pragma pack(pop)

typedef System::TMetaClass* TDAScriptProcessorClass;

typedef void __fastcall (__closure *TBeforeStatementExecuteEvent)(System::TObject* Sender, System::UnicodeString &SQL, bool &Omit);

typedef void __fastcall (__closure *TAfterStatementExecuteEvent)(System::TObject* Sender, System::UnicodeString SQL);

enum DECLSPEC_DENUM TErrorAction : unsigned char { eaAbort, eaFail, eaException, eaContinue };

typedef void __fastcall (__closure *TOnErrorEvent)(System::TObject* Sender, System::Sysutils::Exception* E, System::UnicodeString SQL, TErrorAction &Action);

class PASCALIMPLEMENTATION TDAScript : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	TDAScriptProcessor* FProcessor;
	System::Classes::TStrings* FSQL;
	bool FSQLActual;
	System::Classes::TStream* FStream;
	Dbaccess::TCustomDASQL* FCommand;
	Dbaccess::TMacros* FMacros;
	__int64 FErrorOffset;
	__int64 FStartPos;
	__int64 FEndPos;
	__int64 FStartLine;
	__int64 FEndLine;
	__int64 FStartOffset;
	__int64 FEndOffset;
	Data::Db::TDataSource* FDataSource;
	TBeforeStatementExecuteEvent FBeforeExecute;
	TAfterStatementExecuteEvent FAfterExecute;
	TOnErrorEvent FOnError;
	__int64 FStmtOffset;
	bool FDesignCreate;
	System::UnicodeString FDelimiter;
	TDAStatements* FStatements;
	bool FScanParams;
	bool FUseOptimization;
	bool FAllowOptimization;
	Clrclasses::WideStringBuilder* FBuffer;
	bool FAutoCommit;
	bool FBreakExecution;
	System::Syncobjs::TCriticalSection* FcsBreakMultiThread;
	bool FNoPreconnect;
	int FRowsAffected;
	Dbaccess::TCustomDAConnection* __fastcall GetConnection();
	void __fastcall SetConnection(Dbaccess::TCustomDAConnection* Value);
	Dbaccess::TDATransaction* __fastcall GetTransaction();
	void __fastcall SetTransaction(Dbaccess::TDATransaction* Value);
	bool __fastcall IsTransactionStored();
	virtual TDAScriptProcessorClass __fastcall GetProcessorClass();
	virtual void __fastcall SetProcessor(TDAScriptProcessor* Value);
	void __fastcall CreateProcessor();
	void __fastcall FreeProcessor();
	virtual Dbaccess::TCustomDAConnection* __fastcall UsedConnection();
	Dbaccess::TDATransaction* __fastcall UsedTransaction();
	System::UnicodeString __fastcall GetSQLText(int StartLine, int EndLine, int StartOffset, int EndOffset, int Length);
	void __fastcall SetSQL(System::Classes::TStrings* Value);
	void __fastcall SQLChanged(System::TObject* Sender);
	bool __fastcall GetDebug();
	void __fastcall SetDebug(bool Value);
	void __fastcall SetMacros(Dbaccess::TMacros* Value);
	Dbaccess::TCustomDADataSet* __fastcall GetDataSet();
	void __fastcall SetDataSet(Dbaccess::TCustomDADataSet* Value);
	Dbaccess::TDAParams* __fastcall GetParams();
	void __fastcall SetAutoCommit(bool Value);
	void __fastcall SetDelimiter(const System::UnicodeString Value);
	bool __fastcall IsDelimiterStored();
	virtual void __fastcall Loaded();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall ReadMacroData(System::Classes::TReader* Reader);
	void __fastcall WriteMacroData(System::Classes::TWriter* Writer);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	virtual Dbaccess::TCustomDASQL* __fastcall CreateCommand();
	virtual void __fastcall CalculateErrorOffset(System::Sysutils::Exception* E);
	virtual TDAStatements* __fastcall CreateStatementsObject();
	TDAStatements* __fastcall GetStatements();
	void __fastcall Open(System::Classes::TStream* Stream);
	void __fastcall Close();
	void __fastcall InternalExecute(const System::UnicodeString SQL, /* out */ bool &BreakExec, Dbaccess::TDAParams* Params = (Dbaccess::TDAParams*)(0x0));
	void __fastcall Flush(/* out */ bool &BreakExec);
	__property Dbaccess::TDATransaction* Transaction = {read=GetTransaction, write=SetTransaction, stored=IsTransactionStored};
	__property bool ScanParams = {read=FScanParams, write=FScanParams, default=1};
	
public:
	__fastcall virtual TDAScript(System::Classes::TComponent* Owner);
	__fastcall virtual ~TDAScript();
	virtual void __fastcall Execute();
	void __fastcall ExecuteStream(System::Classes::TStream* Stream);
	void __fastcall ExecuteFile(const System::UnicodeString FileName);
	virtual bool __fastcall ExecuteNext();
	virtual void __fastcall BreakExec();
	Dbaccess::TMacro* __fastcall FindMacro(System::UnicodeString Name);
	Dbaccess::TMacro* __fastcall MacroByName(System::UnicodeString Name);
	__int64 __fastcall ErrorOffset();
	__property Dbaccess::TDAParams* Params = {read=GetParams};
	__property bool UseOptimization = {read=FUseOptimization, write=FUseOptimization, default=0};
	__property bool AutoCommit = {read=FAutoCommit, write=SetAutoCommit, default=0};
	__property int RowsAffected = {read=FRowsAffected, nodefault};
	__property Dbaccess::TCustomDAConnection* Connection = {read=GetConnection, write=SetConnection};
	__property Dbaccess::TCustomDADataSet* DataSet = {read=GetDataSet, write=SetDataSet};
	__property __int64 StartPos = {read=FStartPos};
	__property __int64 EndPos = {read=FEndPos};
	__property __int64 StartLine = {read=FStartLine};
	__property __int64 EndLine = {read=FEndLine};
	__property __int64 StartOffset = {read=FStartOffset};
	__property __int64 EndOffset = {read=FEndOffset};
	__property TDAStatements* Statements = {read=GetStatements};
	__property TDAScriptProcessor* Processor = {read=FProcessor};
	void __fastcall CheckProcessor();
	
__published:
	__property System::Classes::TStrings* SQL = {read=FSQL, write=SetSQL};
	__property bool Debug = {read=GetDebug, write=SetDebug, default=0};
	__property System::UnicodeString Delimiter = {read=FDelimiter, write=SetDelimiter, stored=IsDelimiterStored};
	__property Dbaccess::TMacros* Macros = {read=FMacros, write=SetMacros, stored=false};
	__property bool NoPreconnect = {read=FNoPreconnect, write=FNoPreconnect, default=0};
	__property TBeforeStatementExecuteEvent BeforeExecute = {read=FBeforeExecute, write=FBeforeExecute};
	__property TAfterStatementExecuteEvent AfterExecute = {read=FAfterExecute, write=FAfterExecute};
	__property TOnErrorEvent OnError = {read=FOnError, write=FOnError};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDAScriptUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall SetDesignCreate(TDAScript* Obj, bool Value);
	__classmethod bool __fastcall GetDesignCreate(TDAScript* Obj);
	__classmethod void __fastcall SetCommand(TDAScript* Obj, Dbaccess::TCustomDASQL* Command);
	__classmethod Dbaccess::TCustomDASQL* __fastcall GetCommand(TDAScript* Obj);
	__classmethod void __fastcall Open(TDAScript* Obj, System::Classes::TStream* Stream);
	__classmethod void __fastcall Close(TDAScript* Obj);
	__classmethod Dbaccess::TCustomDAConnection* __fastcall UsedConnection(TDAScript* Obj);
	__classmethod Dbaccess::TDATransaction* __fastcall UsedTransaction(TDAScript* Obj);
	__classmethod Dbaccess::TDATransaction* __fastcall GetTransaction(TDAScript* Obj);
	__classmethod void __fastcall SetTransaction(TDAScript* Obj, Dbaccess::TDATransaction* Value);
public:
	/* TObject.Create */ inline __fastcall TDAScriptUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDAScriptUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 ST_UNKNOWN = System::Int8(0x0);
static const System::Int8 ST_DELIMETER = System::Int8(0x1);
static const System::Int8 ST_COMMENT = System::Int8(0x2);
static const System::Int8 ST_STATEMENT = System::Int8(0x3);
static const System::Word ST_SPECIFIC_SQL = System::Word(0x8000);
}	/* namespace Dascript */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_DASCRIPT)
using namespace Dascript;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DascriptHPP
