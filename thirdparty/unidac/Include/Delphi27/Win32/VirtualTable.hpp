// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTable.pas' rev: 34.00 (Windows)

#ifndef VirtualtableHPP
#define VirtualtableHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Data.DB.hpp>
#include <MemData.hpp>
#include <MemDS.hpp>
#include <CRTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Virtualtable
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TVirtualAutoIncField;
class DELPHICLASS TVirtualTable;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TCRFileFormat : unsigned char { ffVTD, ffXML };

enum DECLSPEC_DENUM TVirtualTableOption : unsigned char { voPersistentData, voStored, voSetEmptyStrToNull, voSkipUnSupportedFieldTypes };

typedef System::Set<TVirtualTableOption, TVirtualTableOption::voPersistentData, TVirtualTableOption::voSkipUnSupportedFieldTypes> TVirtualTableOptions;

typedef void __fastcall (__closure *TVirtualTableProgressEvent)(System::TObject* Sender, int Percent);

class PASCALIMPLEMENTATION TVirtualAutoIncField : public Data::Db::TAutoIncField
{
	typedef Data::Db::TAutoIncField inherited;
	
private:
	int FCurrentValue;
	int FInitialValue;
	int FIncrement;
	void __fastcall SetInitialValue(const int Value);
	
public:
	__fastcall virtual TVirtualAutoIncField(System::Classes::TComponent* AOwner);
	
__published:
	__property AutoGenerateValue = {default=1};
	__property int InitialValue = {read=FInitialValue, write=SetInitialValue, default=-1};
	__property int Increment = {read=FIncrement, write=FIncrement, default=1};
public:
	/* TField.Destroy */ inline __fastcall virtual ~TVirtualAutoIncField() { }
	
};


class PASCALIMPLEMENTATION TVirtualTable : public Memds::TMemDataSet
{
	typedef Memds::TMemDataSet inherited;
	
private:
	TVirtualTableOptions FOptions;
	bool FStreamedActive;
	bool FAvoidRefreshData;
	int FAvoidReload;
	System::Classes::TMemoryStream* FRecordDataStream;
	bool FIsCursorOpen;
	TVirtualTableProgressEvent FOnVirtualTableProgress;
	bool FReadOnly;
	bool FLoadingData;
	Memdata::TSortType FDefaultSortType;
	void __fastcall ReadBinaryData(System::Classes::TStream* Stream);
	void __fastcall WriteBinaryData(System::Classes::TStream* Stream);
	bool __fastcall IsFieldDefsStored();
	Data::Db::TFieldDefs* __fastcall GetFieldDefs();
	HIDESBASE void __fastcall SetFieldDefs(Data::Db::TFieldDefs* Value);
	void __fastcall SetDefaultSortType(const Memdata::TSortType Value);
	void __fastcall SetOptions(TVirtualTableOptions Value);
	void __fastcall InternalSaveToStream(System::Classes::TStream* Stream, bool StoreFields, bool StoreAllData);
	void __fastcall InitAutoIncValues(void * RecBuf);
	
protected:
	bool FFieldDefsByField;
	virtual void __fastcall Loaded();
	virtual void __fastcall CreateIRecordSet();
	virtual void __fastcall OpenCursor(bool InfoQuery);
	virtual void __fastcall InternalOpen();
	virtual void __fastcall InternalClose();
	virtual bool __fastcall IsCursorOpen();
	virtual void __fastcall CreateFieldDefs();
	virtual void __fastcall DefChanged(System::TObject* Sender);
	void __fastcall Reload();
	virtual void __fastcall DataEvent(Data::Db::TDataEvent Event, NativeInt Info);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall AssignDataSet(Data::Db::TDataSet* Source);
	virtual void __fastcall SetActive(bool Value);
	void __fastcall DoVirtualTableProgress(int Percent);
	virtual bool __fastcall GetCanModify();
	virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldType FieldType)/* overload */;
	virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldType FieldType, System::Word DataType)/* overload */;
	virtual Data::Db::TFieldClass __fastcall GetFieldClass(Data::Db::TFieldDef* FieldDef)/* overload */;
	virtual void __fastcall InternalInsert();
	virtual void __fastcall SetFieldData(Data::Db::TField* Field, System::DynamicArray<System::Byte> Buffer)/* overload */;
	
public:
	__fastcall virtual TVirtualTable(System::Classes::TComponent* Owner);
	__fastcall virtual ~TVirtualTable();
	virtual bool __fastcall IsSequenced();
	void __fastcall AddField(const System::UnicodeString Name, Data::Db::TFieldType FieldType, int Size = 0x0, bool Required = false);
	void __fastcall DeleteField(const System::UnicodeString Name);
	void __fastcall DeleteFields();
	void __fastcall Clear();
	void __fastcall LoadFromStream(System::Classes::TStream* Stream, bool LoadFields = true, bool DecodeHTMLEntities = true);
	void __fastcall SaveToStream(System::Classes::TStream* Stream, bool StoreFields = true, bool StoreAllData = false);
	void __fastcall LoadFromFile(const System::UnicodeString FileName, bool LoadFields = true, bool DecodeHTMLEntities = true);
	void __fastcall SaveToFile(const System::UnicodeString FileName, bool StoreFields = true, bool StoreAllData = false);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	Memdata::TMemData* __fastcall GetData();
	
__published:
	__property Memdata::TSortType DefaultSortType = {read=FDefaultSortType, write=SetDefaultSortType, default=0};
	__property TVirtualTableOptions Options = {read=FOptions, write=SetOptions, default=11};
	__property TVirtualTableProgressEvent OnProgress = {read=FOnVirtualTableProgress, write=FOnVirtualTableProgress};
	__property Active = {default=0};
	__property AutoCalcFields = {default=1};
	__property Constraints = {stored=IsConstraintsStored};
	__property Filtered = {default=0};
	__property Filter = {default=0};
	__property FilterOptions = {default=0};
	__property IndexFieldNames = {default=0};
	__property MasterSource;
	__property MasterFields = {default=0};
	__property DetailFields = {default=0};
	__property Data::Db::TFieldDefs* FieldDefs = {read=GetFieldDefs, write=SetFieldDefs, stored=IsFieldDefsStored};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, default=0};
	__property BeforeOpen;
	__property AfterOpen;
	__property BeforeClose;
	__property AfterClose;
	__property BeforeInsert;
	__property AfterInsert;
	__property BeforeEdit;
	__property AfterEdit;
	__property BeforePost;
	__property AfterPost;
	__property BeforeCancel;
	__property AfterCancel;
	__property BeforeDelete;
	__property AfterDelete;
	__property BeforeScroll;
	__property AfterScroll;
	__property OnCalcFields;
	__property OnDeleteError;
	__property OnEditError;
	__property OnFilterRecord;
	__property OnNewRecord;
	__property OnPostError;
	__property OnUpdateError;
	__property OnUpdateRecord;
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  SetFieldData(Data::Db::TField* Field, System::DynamicArray<System::Byte> Buffer, bool NativeFormat){ Memds::TMemDataSet::SetFieldData(Field, Buffer, NativeFormat); }
	inline void __fastcall  SetFieldData(Data::Db::TField* Field, void * Buffer){ Memds::TMemDataSet::SetFieldData(Field, Buffer); }
	inline void __fastcall  SetFieldData(Data::Db::TField* Field, void * Buffer, bool NativeFormat){ Memds::TMemDataSet::SetFieldData(Field, Buffer, NativeFormat); }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word ftVirtualAutoInc = System::Word(0x22b);
extern DELPHI_PACKAGE bool VTOldBehavior;
}	/* namespace Virtualtable */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTABLE)
using namespace Virtualtable;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualtableHPP
