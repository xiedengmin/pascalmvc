// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRBatchMove.pas' rev: 34.00 (Windows)

#ifndef CrbatchmoveHPP
#define CrbatchmoveHPP

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
#include <CRTypes.hpp>
#include <DBAccess.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crbatchmove
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCRBatchMove;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCRBatchMode : unsigned char { bmAppend, bmUpdate, bmAppendUpdate, bmDelete };

typedef void __fastcall (__closure *TCRBatchMoveProgressEvent)(System::TObject* Sender, int Percent);

typedef bool __fastcall (__closure *TDALocate)(void);

enum DECLSPEC_DENUM TCRFieldMappingMode : unsigned char { mmFieldIndex, mmFieldName };

class PASCALIMPLEMENTATION TCRBatchMove : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
	
private:
	typedef System::DynamicArray<Data::Db::TField*> _TCRBatchMove__1;
	
	typedef System::DynamicArray<bool> _TCRBatchMove__2;
	
	typedef System::DynamicArray<System::Variant> _TCRBatchMove__3;
	
	typedef System::DynamicArray<System::Word> _TCRBatchMove__4;
	
	
private:
	Dbaccess::TFieldArray FFldDestKeys;
	System::UnicodeString FStrDestKeys;
	_TCRBatchMove__1 FSrcKeyFields;
	_TCRBatchMove__2 FDestKeyFields;
	_TCRBatchMove__3 FKeyValues;
	_TCRBatchMove__4 FFieldMap;
	Data::Db::_di_IProviderSupport FPSDestination;
	TDALocate Locate;
	void __fastcall SetMappings(System::Classes::TStrings* Value);
	void __fastcall SetSource(Data::Db::TDataSet* Value);
	void __fastcall SetDestination(Data::Db::TDataSet* Value);
	Data::Db::_di_IProviderSupport __fastcall GetProviderSupport(Data::Db::TDataSet* DataSet);
	bool __fastcall LocateForCustomDaDataSet();
	bool __fastcall LocateForDataSet();
	
protected:
	Data::Db::TDataSet* FDestination;
	Data::Db::TDataSet* FSource;
	TCRBatchMode FMode;
	bool FAbortOnKeyViol;
	bool FAbortOnProblem;
	int FRecordCount;
	int FMovedCount;
	int FKeyViolCount;
	int FProblemCount;
	int FChangedCount;
	System::Classes::TStrings* FMappings;
	TCRFieldMappingMode FFieldMappingMode;
	int FCommitCount;
	TCRBatchMoveProgressEvent FOnBatchMoveProgress;
	bool FTransactionNeeds;
	int FAppliedCount;
	System::Word FDestCountKeys;
	void __fastcall DoBatchMoveProgress(int Percent);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	System::Variant __fastcall GetKeyValues();
	void __fastcall SetFieldsValues(bool SetKeyFields);
	void __fastcall Append();
	void __fastcall Update();
	void __fastcall AppendUpdate();
	void __fastcall Delete();
	
public:
	__fastcall virtual TCRBatchMove(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCRBatchMove();
	void __fastcall Execute();
	__property int ChangedCount = {read=FChangedCount, nodefault};
	__property int KeyViolCount = {read=FKeyViolCount, nodefault};
	__property int MovedCount = {read=FMovedCount, nodefault};
	__property int ProblemCount = {read=FProblemCount, nodefault};
	
__published:
	__property bool AbortOnKeyViol = {read=FAbortOnKeyViol, write=FAbortOnKeyViol, default=1};
	__property bool AbortOnProblem = {read=FAbortOnProblem, write=FAbortOnProblem, default=1};
	__property int CommitCount = {read=FCommitCount, write=FCommitCount, default=0};
	__property Data::Db::TDataSet* Destination = {read=FDestination, write=SetDestination};
	__property System::Classes::TStrings* Mappings = {read=FMappings, write=SetMappings};
	__property TCRFieldMappingMode FieldMappingMode = {read=FFieldMappingMode, write=FFieldMappingMode, default=0};
	__property TCRBatchMode Mode = {read=FMode, write=FMode, default=0};
	__property int RecordCount = {read=FRecordCount, write=FRecordCount, default=0};
	__property Data::Db::TDataSet* Source = {read=FSource, write=SetSource};
	__property TCRBatchMoveProgressEvent OnBatchMoveProgress = {read=FOnBatchMoveProgress, write=FOnBatchMoveProgress};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Crbatchmove */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CRBATCHMOVE)
using namespace Crbatchmove;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CrbatchmoveHPP
