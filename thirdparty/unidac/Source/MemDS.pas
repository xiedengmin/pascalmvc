//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Memory Data Set
//  Created:            01.02.98
//////////////////////////////////////////////////

{$IFNDEF FPC}
{$I Dac.inc}
unit MemDS;
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, Variants, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFDEF BDE_SHARED}
  DBTables,
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$IFNDEF FPC}Windows,{$ENDIF}
  CRTimer,
{$ENDIF}
{$IFDEF VER17P}
  Generics.Collections,
{$ENDIF}
  CLRClasses, CRXml, CRTypes, CRFunctions, CRTimeStamp, MemData, MemUtils;

const
  uaDefault = 10; // TUpdateAction
  rlAny = -1; // Data Type Mapping Rule

{$IFNDEF FPC}
{$IFNDEF VER10P}
  ftFixedWideChar = 52;
{$ENDIF}
  ftDATimeStampOffset = 99;
{$ENDIF}

type
  TDataSetService = class;
  TDataSetServiceClass = class of TDataSetService;
  TMemDataSet = class;

{ TMemDataSet }

{$IFDEF FPC}
  TUpdateRecordTypes = set of (rtModified, rtInserted, rtDeleted, rtUnmodified);
  TUpdateErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
    UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction) of object;
  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
    var UpdateAction: TUpdateAction) of object;
{$ENDIF}

{$IFDEF VER12P}
  TRecordBuffer = PByte;
{$ELSE}
  TRecordBuffer = PAnsiChar;
{$ENDIF}
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    RecordNumber: integer;
    UpdateStatus: TUpdateStatus;
    BookmarkFlag: TBookmarkFlag;
    RefComplexFields: boolean;
    KeyExclusive: boolean;
  end;

  TCalcFieldDescMapping = record
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FieldDesc: TFieldDesc;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    Field: TField;
  end;

  TLocalMDLink = record
    IsNull: boolean;
    DataBuf: IntPtr;
    DataLen: Word;
    BufferType: Word;
    NativeBuffer: boolean;
    FieldNo: integer;
    MasterFieldValue: Variant;
  end;
  PLocalMDLink = ^TLocalMDLink;

  TLocalMDLinks = array of TLocalMDLink;

  TFieldTypeMap = class
  public
    class function GetFieldType(DataType: Word): TFieldType; virtual;
    class function GetDataType(FieldType: TFieldType; SubDataType: Word = 0): Integer; virtual;
  end;

  TFieldTypeMapClass = class of TFieldTypeMap;

{ TDADetailDataLink }

  TDADetailDataLink = class (TDetailDataLink)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TMemDataSet;

  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure CheckBrowseMode; override;
    function GetDetailDataSet: TDataSet; override;
  public
    constructor Create(DataSet: TMemDataSet);
  end;

  TDataSetUpdater = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TMemDataSet;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSetService: TDataSetService;

    function PerformAppend: boolean; virtual;
    function PerformDelete: boolean; virtual;
    function PerformUpdate: boolean; virtual;
    function CacheChanged: boolean; virtual;
    function CacheApplied: boolean; virtual;
    function CacheCanceled: boolean; virtual;

    procedure DoPerformAppend;
    procedure DoPerformDelete;
    procedure DoPerformUpdate;
    procedure DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
    procedure DoCacheChanged;
    procedure DoCacheApplied;
    procedure DoCacheCanceled;
    procedure DoAfterApplyUpdates;

    function BatchUpdate: boolean; virtual;
    function CanFlushBatch: boolean; virtual;
    procedure FlushBatch; virtual;
  public
    constructor Create(AOwner: TDataSetService); virtual;
  end;

  TDataSetService = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TMemDataSet;
    FUpdater: TDataSetUpdater;

    procedure CreateDataSetUpdater; virtual;
    procedure SetDataSetUpdater(Value: TDataSetUpdater); virtual;
    procedure FreeDataSetUpdater;

    procedure SetNumberRange(FieldDef: TFieldDef); virtual;
    function GetFieldClass(FieldType: TFieldType; DataType: Word): TFieldClass; virtual;

    procedure PreInitCursor; virtual;

  { XML }
    procedure WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: string;
      XMLWriter: XMLTextWriter); virtual;
    procedure WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: string;
      XMLWriter: XMLTextWriter); virtual;
    function GetFieldXMLValue(Field: TField; FieldDesc: TFieldDesc): WideString; virtual;

  public
    constructor Create(AOwner: TMemDataSet); virtual;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    procedure SaveToXML(Destination: TStream);
  end;

  TMemDataSet = class(TDataSet)
  private
    FOldRecBuf: TRecordBuffer;
    FFilterBuffer: TRecordBuffer;
    FRangeStartBuffer: TRecordBuffer;
    FRangeEndBuffer: TRecordBuffer;
    FRangeCurrentBuffer: TRecordBuffer;
    FRanged: Boolean;

    FCachedUpdates: boolean;
    FLocalUpdate: boolean;
    FNewCacheRecBuf: TRecordBuffer;
    FOldCacheRecBuf: TRecordBuffer;

    FOldDeferredPostBuf: TRecordBuffer;
    FInInserting: boolean;
    FInEditing: boolean;

    FIndexFieldNames: string;
    FCalcFieldsMapping: array of TCalcFieldDescMapping;

    FUpdateRecordTypes: TUpdateRecordTypes;
    FOnUpdateError: TUpdateErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;

  { Master/Detail }
    FDataLink: TDADetailDataLink;
    FLocalMDLinks: TLocalMDLinks;
    FDetailDelay: integer;
  {$IFDEF MSWINDOWS}
    FDetailRefreshTimer: TCRTimer;
    procedure CheckRefreshDetailTimer;
  {$ENDIF}
    procedure SetDetailDelay(Value: integer);
    procedure SetMasterSource(Value: TDataSource);
    procedure SetMasterFields(const Value: string);
    procedure SetDetailFields(const Value: string);

    {function GetBlobData(Field:TField; Buffer: PAnsiChar):TBlobData;
    procedure SetBlobData(Field:TField; Buffer: PAnsiChar; Value:TBlobData);
    procedure ClearBlobCache(Buffer: PAnsiChar);}

    procedure SetCachedUpdates(Value: boolean);
    procedure SetLocalUpdate(Value: boolean);
    function GetUpdatesPending: boolean;
    function GetPrepared: boolean;
    procedure SetPrepared(Value: boolean);
    function ConvertUpdateRecordTypes(Value: TUpdateRecordTypes): TItemTypes;
    function GetUpdateRecordTypes: TUpdateRecordTypes;
    procedure SetUpdateRecordTypes(Value: TUpdateRecordTypes);
    procedure FreeDeferredBuf;

  { Range }
    procedure CheckSetKeyMode;
    procedure ClearRangeBuffer(Buffer: TRecordBuffer);
    procedure InitRangeBuffer(Buffer: TRecordBuffer; Clear: Boolean);
    procedure SetRangeBuffer(Buffer: TRecordBuffer; const Values: array of const);
    function GetKeyExclusive: Boolean;
    procedure SetKeyExclusive(Value: Boolean);
    procedure ResetRange;

  { Constraints }
    procedure CreateCheckConstraints;
    procedure CreateFieldConstrain;

  protected
    Data: TData;  // FIRecordSet
    FDataSetService: TDataSetService;

    FBookmarkOfs: integer;
    FRecInfoOfs: integer;
    FInDeferredPost: boolean;
  {$IFNDEF VER10P}
    FWideStringOfs: integer;
  {$ENDIF}
    FRecBufSize: integer;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FParentDataSet: TMemDataSet;
    FLastParentPos: integer;
    FLocalConstraints: boolean;
    FNumberRange: boolean;
    FNeedAddRef: boolean;
    FIsResync: boolean;
    FCacheCalcFields: boolean;
    FCreateCalcFieldDescs: boolean;
    FInSettingDefaultExpressionValues: boolean;
    FDataWasChanged: boolean;
    FDisableResync: boolean;

    FMasterFields: string;
    FDetailFields: string;

    function IsConstraintsStored: boolean;

    procedure SetModified(Value: Boolean); reintroduce;
    function SetTempState(const Value: TDataSetState): TDataSetState; reintroduce;
    procedure RestoreState(const Value: TDataSetState); reintroduce;
    procedure DoOnDataChanged;

    procedure CreateIRecordSet; virtual;
    procedure FreeIRecordSet;
    procedure SetIRecordSet(Value: TData{TRecordSet}); virtual;
    procedure SetIndexFieldNames(const Value: string); virtual;

    function GetDataSetServiceClass: TDataSetServiceClass; virtual;
    procedure CreateDataSetService;
    procedure FreeDataSetService;
    procedure SetDataSetService(Value: TDataSetService); virtual;
    procedure CheckDataSetService;

  { Open/Close DataSet }
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure CloseCursor; override;

  {$IFDEF FPC}
    procedure CreateFields; override;
  {$ENDIF}
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: boolean; override;
    procedure DataReopen; virtual;
    procedure InternalRefresh; override;
    procedure DoAfterOpen; override;

  { Field Management }
    function GetFieldTypeMapClass: TFieldTypeMapClass; virtual;

    procedure InternalInitFieldDefs; override;
    function NeedCreateFieldDefs: boolean; virtual;
    function CreateFieldDef(FieldDesc: TFieldDesc): TFieldDef; virtual;
    function CreateObjectFields(ObjType: TObjectType; Parent: TFieldDef; FieldNo: Integer): Integer;
    procedure CreateFieldDefs; virtual;
  {$IFNDEF FPC}
    procedure UpdateFieldDefList;
  {$ENDIF}
    function  NeedComplexUpdateFieldDefList: boolean; virtual;
  {$IFDEF VER18P}
    procedure ClearCalcFields(Buffer: NativeInt); overload; override;
  {$ENDIF}
    procedure ClearCalcFields(Buffer: TRecordBuffer); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}

    function GetObjectFieldDefName(Parent: TFieldDef; Index: integer; ObjType: TObjectType):string; virtual;
    function GetFieldDefSize(FieldType: TFieldType; FieldLength: Integer): Integer; virtual;
  {$IFNDEF FPC}
    procedure GetObjectTypeNames(Fields: TFields);
  {$ENDIF}
    function GetFieldType(DataType: word): TFieldType; overload; virtual;
    function GetFieldType(FieldDesc: TFieldDesc; out FieldSize, FieldLength, FieldScale: Integer): TFieldType; overload; virtual;

    function PrepareValueBuffer(Field: TField; var Buffer: IntPtr): Word;
    procedure InternalSetFieldData(Field: TField; Buffer: IntPtr; BufferLen: Word); virtual;
    function InternalDataConvert(Field: TField; Source, Dest: IntPtr; ToNative: Boolean): boolean; virtual;
  {$IFDEF FPC}
  public
  {$ENDIF}
  {$IFDEF VER17P}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean); overload; override;
    procedure DataConvert(Field: TField; Source: TValueBuffer; {$IFDEF VER18P}var{$ENDIF} Dest: TValueBuffer; ToNative: Boolean); overload; override;
  {$ENDIF}
    procedure SetFieldData(Field: TField; Buffer: IntPtr); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    procedure SetFieldData(Field: TField; Buffer: IntPtr; NativeFormat: Boolean); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
  {$IFNDEF NEXTGEN}
    procedure DataConvert(Field: TField; Source, Dest: IntPtr; ToNative: Boolean); overload; override;
  {$ENDIF}

  protected
  {$IFDEF NEXTGEN}
    procedure DestroyFields; override;
  {$ENDIF}
    function GetSparseArrays: boolean;
    procedure SetSparseArrays(Value: boolean);
    procedure CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef); {$IFNDEF FPC}override{$ELSE}virtual{$ENDIF};
    function GetFieldClass(FieldType: TFieldType): TFieldClass; overload; override;
    function GetFieldClass(FieldType: TFieldType; DataType: Word): TFieldClass; reintroduce; overload; virtual;
  {$IFDEF VER12P}
    function GetFieldClass(FieldDef: TFieldDef): TFieldClass; overload; override;
  {$ENDIF}

  { Buffer/Record Management }
  {$IFDEF NEXTGEN}
    function AllocRecBuf: TRecBuf; override;
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
  {$ENDIF}
    function AllocRecordBuffer: TRecordBuffer; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}

  {$IFDEF VER18P}
    procedure InitRecord(Buffer: TRecBuf); overload; override;
  {$ENDIF}
    procedure InitRecord(Buffer: TRecordBuffer); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
  {$IFDEF VER18P}
    procedure InternalInitRecord(Buffer: TRecBuf); overload; override;
  {$ENDIF}
    procedure InternalInitRecord(Buffer: TRecordBuffer); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}

    function GetOldRecord: TRecordBuffer;
    function GetOldRecBuf: TRecordBuffer;
    function GetNewRecBuf: TRecordBuffer;
    function GetActiveRecBuf(out RecBuf: TRecordBuffer): boolean;
  {$IFDEF VER18P}
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; override;
  {$ENDIF}
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult; overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}

  {$IFNDEF FPC}
    procedure BlockReadNext; override;
    procedure SetBlockReadSize(Value: integer); override;
  {$ENDIF}
  {$IFDEF FPC}
    procedure SetBufListSize(Value: Integer); override;
  {$ENDIF}

    procedure FreeRefBuffers;
    procedure AddRefComplexFields(Buffer: TRecordBuffer);
    procedure FreeRefComplexFields(Buffer: TRecordBuffer; WithBlob: boolean = True);

  { Bookmarks }
  {$IFDEF VER17P}
    procedure GetBookmarkData(Buffer: {$IFDEF VER18P}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}; Bookmark: TBookmark); overload; override;
    procedure SetBookmarkData(Buffer: {$IFDEF VER18P}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}; Bookmark: TBookmark); overload; override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); overload; override;
  {$ENDIF}
  {$IFDEF VER18P}
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag); override;
  {$ENDIF}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Bookmark: Pointer); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    procedure SetBookmarkData(Buffer: TRecordBuffer; Bookmark: Pointer); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    procedure InternalGotoBookmark(Bookmark: Pointer); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}

  { Navigation }
    procedure InternalFirst; override;
    procedure InternalLast; override;

  {$IFDEF VER18P}
    procedure InternalSetToRecord(Buffer: TRecBuf); overload; override;
  {$ENDIF}
    procedure InternalSetToRecord(Buffer: TRecordBuffer); overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}

  { Editing }
  {$IFDEF VER17P}
    procedure InternalAddRecord(Buffer: {$IFDEF VER18P}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}; Append: boolean); overload; override;
  {$ENDIF}
    procedure InternalAddRecord(Buffer: IntPtr; Append: boolean); overload;{$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    procedure InternalInsert; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;

    procedure InternalPost; override;
    procedure InternalCancel; override;

    procedure InternalDeferredPost; virtual;

    procedure SetDefaultExpressionValues; virtual;
    procedure DoOnNewRecord; override;

    procedure DoPerformAppend;
    procedure DoPerformDelete;
    procedure DoPerformUpdate;
    procedure DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
    procedure DoCacheChanged;
    procedure DoCacheApplied;
    procedure DoCacheCanceled;
    procedure DoAfterApplyUpdates;

    procedure DoGetCachedFields;
    procedure DoGetCachedBuffer(Buffer: IntPtr; Source: IntPtr = nil);

  { Filter/Find/Locate }
    procedure ActivateFilters; virtual;
    procedure DeactivateFilters; virtual;
    function RecordFilter(RecBuf: IntPtr): boolean;
    procedure SetFilterData(const Text: string; Options: TFilterOptions);

    procedure SetFiltered(Value: boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;

    procedure CopyFieldValue(const Value: variant; out ValuePtr: IntPtr; out ValueLen: Word;
      out ValueType: Word; FieldType: Word; UseFieldType: boolean = True); virtual;

    function LocateRecord(KeyFields: TList; KeyValues: variant;
      Options: TLocateExOptions; SavePos: boolean): boolean; overload; virtual;
    function LocateRecord(const KeyFields: string; const KeyValues: variant;
      Options: TLocateExOptions; SavePos: boolean): boolean; overload;
    function LocateRecord(const KeyFields: array of TField; const KeyValues: variant;
      Options: TLocateExOptions; SavePos: boolean): boolean; overload;
    function FindRecord(Restart, GoForward: boolean): boolean; override;

    function RecordFilterRange(RecBuf: IntPtr): boolean;

  { Master/Detail }
  {$IFDEF VER17P}
    function AddFieldToList(const FieldName: string; DataSet: TDataSet; List: TList<TField>): boolean;
  {$ELSE}
    function AddFieldToList(const FieldName: string; DataSet: TDataSet; List: TList): boolean;
  {$ENDIF}
    function GetDataSource: TDataSource; override;
    function IsMasterDatasetActive: boolean;
    function IsConnectedToMaster: boolean;
    function SetLocalMDLinks(Field: TField): boolean; virtual;
    function MDLinksRefreshed(Field: TField): boolean; virtual;
    procedure MasterRecordChanged(Field: TField);
    procedure RefreshDetail(Sender: TObject); virtual;
    function LocalDetailFilter(RecBuf: IntPtr): boolean;
    function UseLocalMasterDetailFilter: boolean; virtual;
    procedure SetLocalDetailFilter;
    procedure MDPropertiesChanged; virtual;
    function SplitFieldName(const Fields: string; var Pos: Integer): string; virtual;

    property MasterSource: TDataSource read GetDataSource write SetMasterSource;
    property MasterFields: string read FMasterFields write SetMasterFields;
    property DetailFields: string read FDetailFields write SetDetailFields;
    property DetailDelay: integer read FDetailDelay write SetDetailDelay;

  { CachedUpdates }

    function InternalGetUpdateResult: TUpdateRecAction;
    procedure CheckCachedUpdateMode;

  { Blobs }
    //Renamed GetBlob (CBuilder5 bug - overloaded methods in different sections):
    function InternalGetBlob(FieldDesc: TFieldDesc): TBlob;
    function InternalSetBlob(FieldDesc: TFieldDesc; Blob: TBlob): boolean;
    function SetBlob(Field: TField; Blob: TBlob): boolean;
    procedure CloseBlob(Field: TField); override;

  { Misc }
    function GetRecordCount: integer; override;
    function GetRecordSize: word; override;

    function GetRecNo: integer; override;
    procedure SetRecNo(Value: integer); override;

    procedure InternalHandleException; override;

    procedure AssignTo(Dest: TPersistent); override;

  {$IFDEF FPC}
    procedure DataEvent(Event: TDataEvent; Info: PtrInt); override;
  {$ELSE}
  {$IFDEF VER16P}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
  {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF FPC}
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
  {$ENDIF}

    function GetMaxFieldCount: integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Prepare; virtual;
    procedure UnPrepare; virtual;
    procedure CheckPrepared;
    procedure Resync(Mode: TResyncMode); override;

  {$IFDEF VER17P}
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList<TField>); override;
  {$ELSE}
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); {$IFNDEF FPC}override;{$ELSE}virtual;{$ENDIF}
  {$ENDIF}

  { Fields }
    function GetFieldDescNo(Field: TField): integer;
    function GetFieldDesc(const Field: TField): TFieldDesc; overload; virtual;
    function GetFieldDesc(const FieldName: string): TFieldDesc; overload;
    function GetFieldDesc(const FieldNo: integer): TFieldDesc; overload; virtual;

    //function Translate(const Src: string; var Dest: string; ToOem: boolean): integer; override;

  {$IFDEF VER17P}
    function GetFieldData(FieldNo: integer; {$IFDEF VER18P}var{$ENDIF} Buffer: TValueBuffer): boolean; overload; override;
    function GetFieldData(Field: TField; {$IFDEF VER18P}var{$ENDIF} Buffer: TValueBuffer): boolean; overload; override;
    function GetFieldData(Field: TField; {$IFDEF VER18P}var{$ENDIF} Buffer: TValueBuffer; NativeFormat: Boolean): Boolean; overload; override;
  {$ENDIF}
    {$IFNDEF FPC}
    function GetFieldData(FieldNo: integer; Buffer: IntPtr): boolean; overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    {$ENDIF}
    function GetFieldData(Field: TField; Buffer: IntPtr): boolean; overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}
    function GetFieldData(Field: TField; Buffer: IntPtr; NativeFormat: Boolean): Boolean; overload; {$IFNDEF NEXTGEN}override;{$ELSE}virtual;{$ENDIF}

    function GetBlob(const FieldName: string): TBlob; overload;
    function GetBlob(Field: TField): TBlob; overload;

  { Constraints }
    procedure CreateConstraints;

  { Edit }
    procedure Post; override;
    procedure Cancel; override;
    procedure DeferredPost;

  { Bookmarks }
    function BookmarkValid(Bookmark: TBookmark): boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): integer; override;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    function Locate(const KeyFields: string; const KeyValues: variant;
      Options: TLocateOptions): boolean; overload; override;
    function Locate(const KeyFields: array of TField; const KeyValues: variant;
      Options: TLocateOptions): boolean; reintroduce; overload;
    function LocateEx(const KeyFields: string; const KeyValues: variant;
      Options: TLocateExOptions): boolean; overload;
    function LocateEx(const KeyFields: array of TField; const KeyValues: variant;
      Options: TLocateExOptions): boolean; overload;
    function Lookup(const KeyFields: string; const KeyValues: variant;
      const ResultFields: string): variant; override;
  {$IFDEF FPC}
    function FindFirst: boolean; override;
    function FindLast: boolean; override;
    function FindNext: boolean; override;
    function FindPrior: boolean; override;
  {$ENDIF}

  { CachedUpdates }
    function UpdateStatus: TUpdateStatus; override;
    function UpdateResult: TUpdateAction;
    procedure ApplyUpdates; overload; virtual;
    procedure ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds); overload; virtual;
    procedure CommitUpdates;
    procedure CancelUpdates;
    procedure RestoreUpdates;
    procedure RevertRecord;

  { XML }
    procedure SaveToXML(Destination: TStream); overload;
    procedure SaveToXML(const FileName: string); overload;

  { Range }
    procedure SetRange(const StartValues, EndValues: array of const; StartExlusive: Boolean = False; EndExclusive: Boolean = False);
    procedure ApplyRange;
    procedure CancelRange;
    procedure SetRangeStart;
    procedure SetRangeEnd;
    procedure EditRangeStart;
    procedure EditRangeEnd;

    function IsSequenced: boolean; override;
    function InCacheProcessing: boolean;

    property Prepared: boolean read GetPrepared write SetPrepared;
    property CachedUpdates: boolean read FCachedUpdates write SetCachedUpdates default False;
    property UpdatesPending: boolean read GetUpdatesPending;
    property LocalUpdate: boolean read FLocalUpdate write SetLocalUpdate default False;
    property UpdateRecordTypes: TUpdateRecordTypes read GetUpdateRecordTypes write SetUpdateRecordTypes default [rtModified, rtInserted, rtUnmodified];
    property SparseArrays: boolean read GetSparseArrays write SetSparseArrays;
    property Ranged: Boolean read FRanged;
    property KeyExclusive: Boolean read GetKeyExclusive write SetKeyExclusive;

  // obsolete
    property LocalConstraints: boolean read FLocalConstraints write FLocalConstraints default True;

    property OnUpdateError: TUpdateErrorEvent read FOnUpdateError write FOnUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
    property IndexFieldNames: string read FIndexFieldNames write SetIndexFieldNames;
  end;

{ TBlobStream }

  TBlobStream = class(TStream)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FField: TBlobField;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FFieldDesc: TFieldDesc;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDataSet: TMemDataSet;
    FBuffer: TRecordBuffer;
    FMode: TBlobStreamMode;
    FOpened: boolean;
    FModified: boolean;
    FPosition: longint;

    function GetBlobSize: Longint;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Truncate;
  end;

{$IFNDEF FPC}
{$IFNDEF VER14P}
  TDASQLTimeStampField = class(TSQLTimeStampField)
  private
    function GetValue(var Value: TSQLTimeStamp): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function GetAsDateTime: TDateTime; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TDASQLTimeStampOffsetField = class(TDASQLTimeStampField)
  private
    function GetValue(var Value: TSQLTimeStampOffset): Boolean;
  protected
    function GetAsDateTime: TDateTime; override;
    function GetAsVariant: Variant; override;
    function GetAsSQLTimeStampOffset: TSQLTimeStampOffset; {$IFDEF VER14P}override;{$ENDIF}
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); {$IFDEF VER14P}override;{$ENDIF}
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;

    property Value: TSQLTimeStampOffset read GetAsSQLTimeStampOffset write SetAsSQLTimeStampOffset;
  end;
{$ELSE}
  TDASQLTimeStampField = TSQLTimeStampField;
  TDASQLTimeStampOffsetField = TSQLTimeStampOffsetField;
{$ENDIF}
{$ENDIF}

  TMemDSUtils = class
  public
    class function SetBlob(Obj: TMemDataSet; Field: TField; Blob: TBlob): boolean;
    class function GetBlob(Obj: TMemDataSet; FieldDesc: TFieldDesc): TBlob;
  end;

  procedure ChangeDecimalSeparator(var Value: string; const OldSeparator, NewSeparator: Char);

var
  SendDataSetChangeEventAfterOpen: boolean = True;  ///CR-CRC23525  remove at 18.11.07
  DoNotRaiseExcetionOnUaFail: boolean = False;      ///CR-D23937  remove at 12.12.07
  LocateExOldBehavior: boolean = False;
  RefreshParamsOnInsert: boolean = False; // Old behavior

implementation

uses
  Math, StrUtils, DateUtils, FmtBcd,
{$IFNDEF FPC}
  DBConsts,
{$ELSE}
  DBConst,
{$ENDIF}
  DAConsts;

const
  DataTypeMap: array [TFieldType] of word = (
    // ftUnknown, ftString, ftSmallint, ftInteger, ftWord
    dtUnknown, dtString, dtInt16, dtInteger, dtUInt16,
    // ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    dtBoolean, dtFloat, dtCurrency, dtBCD, dtDate, dtTime, dtDateTime,
    // ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    dtBytes, dtVarBytes, dtInteger, dtBlob, dtMemo, dtBlob, dtMemo,
    // ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    0, 0, 0, dtCursor, dtString, dtWideString,
    // ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    dtInt64, dtObject, dtArray, dtReference, dtTable, 0, 0,
    // ftVariant, ftInterface, ftIDispatch, ftGuid
    dtVariant, 0, 0, dtGuid
    // ftTimeStamp, ftFMTBcd
    {$IFNDEF FPC}, dtSQLTimeStamp{$ELSE}, 0{$ENDIF}, dtFmtBCD
  {$IFDEF FPC}
    // ftFixedWideChar, ftWideMemo
    , dtWideString, dtWideMemo
  {$ENDIF}
  {$IFDEF VER10P}
    // ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval
    , dtWideString, dtWideMemo, 0, 0
  {$IFDEF VER12P}
    // ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream
    , dtUInt32, dtInt8, dtUInt8, dtExtended, 0, 0, 0
  {$IFDEF VER14P}
    // ftTimeStampOffset, ftObject, ftSingle
    , dtSQLTimeStampOffset, 0, dtSingle
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
    );

{ TFieldTypeMap }

class function TFieldTypeMap.GetFieldType(DataType: Word): TFieldType;
begin
  case DataType of
    dtUnknown:
      Result := ftUnknown;
    dtString:
      Result := ftString;
    dtWideString:
      Result := ftWideString;
    dtUInt8:
    {$IFDEF VER12P}
      Result := ftByte;
    {$ELSE}
      Result := ftSmallint;
    {$ENDIF}
    dtWord:
      Result := ftWord;
    dtUInt32:
    {$IFDEF VER12P}
      Result := ftLongWord;
    {$ELSE}
      Result := ftLargeInt;
    {$ENDIF}
    dtInt8:
    {$IFDEF VER12P}
      Result := ftShortint;
    {$ELSE}
      Result := ftSmallint;
    {$ENDIF}
    dtInt16:
      Result := ftSmallint;
    dtInteger:
      Result := ftInteger;
    dtInt64:
      Result := ftLargeInt;
    dtUInt64:
      Result := ftLargeInt;
    dtFloat:
      Result := ftFloat;
    dtSingle:
    {$IFDEF VER14P}
      Result := ftSingle;
    {$ELSE}
      Result := ftFloat;
    {$ENDIF}
  {$IFDEF VER14P}
    dtExtended:
      Result := ftExtended;
  {$ENDIF}
    dtDate:
      Result := ftDate;
    dtTime:
      Result := ftTime;
    dtDateTime:
      Result := ftDateTime;
    dtMemo:
      Result := ftMemo;
    dtWideMemo:
    {$IFDEF VER10P}
      Result := ftWideMemo;
    {$ELSE}{$IFDEF FPC}
      Result := ftWideMemo;
    {$ELSE}
      Result := ftMemo;
    {$ENDIF}{$ENDIF}
    dtBlob:
      Result := ftBlob;
    dtObject:
      Result := ftADT;
    dtReference:
      Result := ftReference;
    dtArray:
      Result := ftArray;
    dtTable:
      Result := ftDataSet;
    dtCursor:
      Result := ftCursor;
    dtBoolean:
      Result := ftBoolean;
    dtVariant:
      Result := ftVariant;
    dtExtString:
      Result := ftString;
    dtExtWideString:
      Result := ftWideString;
    dtBytes:
      Result := ftBytes;
    dtVarBytes:
      Result := ftVarBytes;
    dtExtVarBytes:
      Result := ftVarBytes;
    dtBCD:
      Result := ftBCD;
    dtFmtBCD:
      Result := ftFMTBcd;
  {$IFNDEF FPC}
    dtSQLTimeStamp:
      Result := ftTimeStamp;
    dtSQLTimeStampOffset:
      Result := TFieldType(ftDATimeStampOffset);
  {$ENDIF}
    dtGuid:
      Result := ftGuid;
    dtCurrency:
      Result := ftCurrency;
  else
    Assert(False, SUnknownDataType);
    Result := ftUnknown;
  end;
end;

class function TFieldTypeMap.GetDataType(FieldType: TFieldType; SubDataType: Word = 0): Integer;
begin
{$IFNDEF FPC}
{$IFNDEF VER10P}
  if Integer(FieldType) = ftFixedWideChar then
    Result := dtWideString
  else
{$ENDIF}
  if Integer(FieldType) = ftDATimeStampOffset then
    Result := dtSQLTimeStampOffset
  else
{$ENDIF}
    Result := DataTypeMap[FieldType];

  if (Result in [dtInt16, dtInt64, dtFloat]) and (SubDataType in [dtInt8, dtUInt8, dtUInt32, dtSingle]) then
    Result := SubDataType;
end;

{ TDADetailDataLink }

constructor TDADetailDataLink.Create(DataSet: TMemDataSet);
begin
  inherited Create;

  FDataSet := DataSet;
end;

procedure TDADetailDataLink.ActiveChanged;
begin
  if (FDataSet <> nil) and
      FDataSet.Active and
      not (csDestroying in FDataSet.ComponentState) and
      Active
  then
    FDataSet.MasterRecordChanged(nil);
end;

procedure TDADetailDataLink.RecordChanged(Field: TField);
begin
  if ((Field = nil) or (DataSet.Fields.IndexOf(Field) >= 0)) and
    FDataSet.Active and
    not ((Field <> nil) and ((FDataSet.State in [dsEdit, dsInsert]) or
    ((DataSet.State in [dsInsert]) and not RefreshParamsOnInsert) )) then
      FDataSet.MasterRecordChanged(Field);
end;

procedure TDADetailDataLink.CheckBrowseMode;
begin
  if FDataSet.Active and
    not((DataSet.State in [dsInsert]) and (FDataSet.State in [dsEdit,dsInsert]))
  then // Prevent post detail before post master
    FDataSet.CheckBrowseMode;
end;

function TDADetailDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataSet;
end;

{ TMemDataSet }

constructor TMemDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FUpdateRecordTypes := [rtModified, rtInserted, rtUnmodified];
  FLocalConstraints := True;
  FCreateCalcFieldDescs := True;

  FDataLink := TDADetailDataLink.Create(Self);
end;

destructor TMemDataSet.Destroy;
begin
{$IFDEF MSWINDOWS}
  FDetailRefreshTimer.Free;
{$ENDIF}

  FDataLink.Free;

  UnPrepare;

  FreeIRecordSet;
  SetIRecordSet(nil);
  SetDataSetService(nil);

  inherited;
end;

function  TMemDataSet.IsConstraintsStored: boolean;
begin
  Result := Constraints.Count > 0;
end;

procedure TMemDataSet.SetModified(Value: Boolean);
begin
  inherited;
end;

function TMemDataSet.SetTempState(const Value: TDataSetState): TDataSetState;
begin
  Result := inherited SetTempState(Value);
end;

procedure TMemDataSet.RestoreState(const Value: TDataSetState);
begin
  inherited;
end;

procedure TMemDataSet.CreateIRecordSet;
begin
  SetIRecordSet(TMemData.Create);
end;

procedure TMemDataSet.FreeIRecordSet;
begin
  Data.Free;
end;

procedure TMemDataSet.SetIRecordSet(Value: TData);
begin
  Data := Value;

  if Data <> nil then begin
    Data.CachedUpdates := FCachedUpdates;
    Data.LocalUpdate := FLocalUpdate;
    Data.SparseArrays := SparseArrays;
    Data.FilterItemTypes := ConvertUpdateRecordTypes(FUpdateRecordTypes);

    if Data is TMemData then
      TMemData(Data).SetIndexFieldNames(FIndexFieldNames);

    Data.OnAppend := DoPerformAppend;
    Data.OnDelete := DoPerformDelete;
    Data.OnUpdate := DoPerformUpdate;
    Data.OnApplyRecord := DoApplyRecord;
    Data.OnCacheChanged := DoCacheChanged;
    Data.OnCacheApplied := DoCacheApplied;
    Data.OnCacheCanceled := DoCacheCanceled;
    Data.AfterApplyUpdates := DoAfterApplyUpdates;
    if FCreateCalcFieldDescs then begin
      Data.OnGetCachedFields := DoGetCachedFields;
      Data.OnGetCachedBuffer := DoGetCachedBuffer;
    end;
    SetLocalDetailFilter;
  end;
end;

function TMemDataSet.GetDataSetServiceClass: TDataSetServiceClass;
begin
  Result := TDataSetService;
end;

procedure TMemDataSet.CreateDataSetService;
begin
  SetDataSetService(GetDataSetServiceClass.Create(Self));
end;

procedure TMemDataSet.FreeDataSetService;
begin
  FDataSetService.Free;
  FDataSetService := nil;
end;

procedure TMemDataSet.SetDataSetService(Value: TDataSetService);
begin
  FreeDataSetService;
  FDataSetService := Value;
end;

procedure TMemDataSet.CheckDataSetService;
begin
  if not (FDataSetService is GetDataSetServiceClass) then begin
    FreeDataSetService;
    CreateDataSetService;
  end;
end;

{ Open/Close DataSet }

procedure TMemDataSet.Prepare;
begin
  if (not Prepared) and (Data <> nil) then begin
    SetDefaultFields(FieldCount = 0);
    Data.Prepare;

    if FDataSetService <> nil then
      FDataSetService.PreInitCursor;
    CreateFieldDefs;
  end;
end;

procedure TMemDataSet.UnPrepare;
begin
  if Active then
    Close;

  if Data <> nil then
    Data.UnPrepare;

  if not (csDestroying in ComponentState) then // This line may be called after destroing FieldDefs. For details see TMemDataSet.Destroy
    FieldDefs.Updated := False;
end;

procedure TMemDataSet.CheckPrepared;
begin
  if not Prepared then
    DatabaseError(SDataSetIsNotPrepared);
end;

procedure TMemDataSet.Resync(Mode: TResyncMode);
begin
  if Active then begin // this need if Resync called for closed dataset (AV BUG !!!)
    FIsResync := True;
    try
      inherited;
    finally
      FIsResync := False;
    end;
  end;
end;

{$IFDEF FPC}

procedure TMemDataSet.CreateFields;
const
  ObjectFieldTypes = [ftADT, ftArray, ftReference, ftDataSet];
var
  i: integer;
  Field: TField;
  FieldDef: TFieldDef;
begin
  for i := 0 to FieldDefs.Count - 1 do begin
    FieldDef := FieldDefs[i];
    if (FieldDef.DataType <> ftUnknown) and
        not (FieldDef.DataType in ObjectFieldTypes) and
       (not (faHiddenCol in FieldDef.Attributes) or FieldDefs.HiddenFields)
    then
      Field := FieldDef.CreateField(Self);

    if ((faFixed in FieldDef.Attributes) or (FieldDef.DataType = ftFixedChar)) and (Field <> nil) and (Field is TStringField) then
      TStringField(Field).FixedChar := True;
  end;
end;

{$ENDIF}

procedure TMemDataSet.InternalOpen;
var
  Field: TField;
  i: integer;
  NeedFilter: boolean;
begin
  Assert(Data <> nil);
  Data.Open;

  // FDataSetService is not set in TVirtualTable and TMetaData
  if FDataSetService <> nil then
    FDataSetService.PreInitCursor;

  CreateFieldDefs;

  // Update FieldDefs once to avoid multiple Update calls when working with FieldDefsList
  // (Perfomance optimization)
//  FieldDefs.Updated := False;
//  FieldDefs.Update;

{$IFDEF VER20P}
  if not (lcPersistent in Fields.LifeCycles) then
{$ELSE}
  if DefaultFields then
{$ENDIF}
    CreateFields
  else // Setting actual size
    for i := 0 to FieldDefs.Count - 1 do
      if FieldDefs[i].DataType = ftString then begin
        Field := FindField(FieldDefs[i].Name);
        if (Field <> nil) and (Field.FieldKind = fkData) then begin
          CheckFieldCompatibility(Field, FieldDefs[i]);
          Field.Size := FieldDefs[i].Size;
        end;
      end;

  // Set number specific
  if FNumberRange and (FDataSetService <> nil) then
    for i := 0 to FieldDefs.Count - 1 do
      FDataSetService.SetNumberRange(FieldDefs[i]);

  BindFields(True);

  if (Data is TMemData) and (Data.Fields.Count > 0) and (Data.Fields[Data.Fields.Count - 1].FieldDescKind = fdkCached) then
    TMemData(Data).UpdateCachedBuffer(nil, nil);

{$IFNDEF FPC}
  if ObjectView then
    GetObjectTypeNames(Fields);
{$ENDIF}

  BookmarkSize := SizeOf(TRecBookmark);

  //FBlobCacheOfs := Data.RecordSize + CalcFieldsSize;
  if not FCreateCalcFieldDescs then
    FRecInfoOfs := Data.RecordSize + CalcFieldsSize
  else
    if FCacheCalcFields then
      FRecInfoOfs := Data.RecordSize
    else
      FRecInfoOfs := Data.RecordSize + Data.CalcRecordSize; //FBlobCacheOfs + BlobFieldCount * SizeOf(Pointer);

  FBookmarkOfs := FRecInfoOfs + SizeOf(TRecInfo);
  FRecBufSize := FBookmarkOfs + BookmarkSize;

{$IFNDEF VER10P}
  if not FCreateCalcFieldDescs then begin
    FWideStringOfs := FRecBufSize;

    if CalcFieldsSize > 0 then
      for i := 0 to Fields.Count - 1 do begin
        Field := Fields[i];
        if (Field.DataType = ftWideString) and (Field.FieldKind in [fkCalculated, fkLookUp]) then
          FRecBufSize := FRecBufSize + (Field.Size + 1) * sizeof(WideChar);
      end;
  end;
{$ENDIF}

  if (Data is TMemData) and (TMemData(Data).IndexFieldCount > 0) then
    TMemData(Data).SortItems;

  CreateConstraints;

  NeedFilter := False;
  if Filtered then begin
    ActivateFilters;
    NeedFilter := True;
  end;
  if IsMasterDatasetActive and UseLocalMasterDetailFilter then begin
    MDLinksRefreshed(nil);
    NeedFilter := True;
  end;
  if CachedUpdates then
    NeedFilter := True;

  if NeedFilter then
    Data.FilterUpdated;
end;

function TMemDataSet.IsCursorOpen: boolean;
begin
  if Data <> nil then
    Result := Data.Active
  else
    Result := False;
end;

procedure TMemDataSet.InternalClose;
var
  i: integer;
begin
  try
    if Data <> nil then begin
      BindFields(False);
    {$IFDEF VER20P}
      if not (lcPersistent in Fields.LifeCycles) then
    {$ELSE}
      if DefaultFields then
    {$ENDIF}
        DestroyFields;

      Data.Close;
      DeactivateFilters;
    end;
  finally
    for i := 0 to Length(FLocalMDLinks) - 1 do
      if not FLocalMDLinks[i].NativeBuffer then
        Marshal.FreeHGlobal(FLocalMDLinks[i].DataBuf);
    SetLength(FLocalMDLinks, 0);
  end;
end;

procedure TMemDataSet.OpenCursor(InfoQuery: boolean);
begin
  inherited;

  if not InfoQuery then begin
    if FOldRecBuf <> nil then
      FreeRecordBuffer(FOldRecBuf);
    FOldRecBuf := AllocRecordBuffer;

    if FRangeStartBuffer <> nil then
      FreeRecordBuffer(FRangeStartBuffer);
    FRangeStartBuffer := AllocRecordBuffer;

    if FRangeEndBuffer <> nil then
      FreeRecordBuffer(FRangeEndBuffer);
    FRangeEndBuffer := AllocRecordBuffer;
  end;
end;

procedure TMemDataSet.CloseCursor;
begin
{$IFDEF MSWINDOWS}
  if FDetailRefreshTimer <> nil then
    FDetailRefreshTimer.Enabled := False;
{$ENDIF}

  if Data <> nil then begin // Data can be nil if an exception occurs in OpenCursor
    Data.FilterRangeFunc := nil;

    // free complex fields if call Close in dsInsert or dsEdit mode
    // TDataSet.Close doesn't call Cancel
    if Data.HasComplexFields then begin
      if FInInserting or FInEditing then begin
        FreeRefComplexFields(IntPtr(ActiveBuffer), FInInserting); // on Editing Blobs isn't created, only on Inserting
        Data.FreeComplexFields(IntPtr(ActiveBuffer), FInInserting);
      end;

      FInInserting := False;
      FInEditing := False;
    end;

    if FOldRecBuf <> nil then
      FreeRecordBuffer(FOldRecBuf);
    if FRangeStartBuffer <> nil then
      FreeRecordBuffer(FRangeStartBuffer);
    if FRangeEndBuffer <> nil then
      FreeRecordBuffer(FRangeEndBuffer);
  end;

  FRanged := False;

  try
    inherited;
  finally
    FParentDataSet := nil;
  end;
end;

procedure TMemDataSet.DataReopen;
begin
  Assert(Data <> nil);
  Data.Reopen;
end;

procedure TMemDataSet.InternalRefresh;

  type
    TRangeColumn = record
      DataType: Word;
      Name: string;
      UseForRangeStart: boolean;
      UseForRangeEnd: boolean;
    end;
    PRangeColumn = ^TRangeColumn;

    TRangeColumns = array of TRangeColumn;

  function SaveRangeColumns: TRangeColumns;
  var
    i: Integer;
    SortColumn: TSortColumn;
  begin
    SetLength(Result, TMemData(Data).IndexFieldCount);
    for i := 0 to TMemData(Data).IndexFieldCount - 1 do begin
      SortColumn := TMemData(Data).IndexFields[i];
      if SortColumn.FieldDesc <> nil then begin
        Result[i].DataType := SortColumn.FieldDesc.DataType;
        Result[i].Name := SortColumn.FieldDesc.Name;
      end
      else begin
        Result[i].DataType := dtUnknown;
        Result[i].Name := '';
      end;
      Result[i].UseForRangeStart := SortColumn.UseForRangeStart;
      Result[i].UseForRangeEnd := SortColumn.UseForRangeEnd;
    end;
  end;

  procedure RestoreRangeColumns(const RangeColumns: TRangeColumns);
  var
    i: Integer;
    SortColumn: TSortColumn;
  begin
    for i := 0 to TMemData(Data).IndexFieldCount - 1 do begin
      if i >= Length(RangeColumns) then
        Exit;

      SortColumn := TMemData(Data).IndexFields[i];
      if (SortColumn.FieldDesc <> nil) and
         (SortColumn.FieldDesc.DataType = RangeColumns[i].DataType) and
         (SortColumn.FieldDesc.Name = RangeColumns[i].Name)
      then begin
        SortColumn.UseForRangeStart := RangeColumns[i].UseForRangeStart;
        SortColumn.UseForRangeEnd := RangeColumns[i].UseForRangeEnd;
      end;
    end;
  end;

var
  RangeColumns: TRangeColumns;
begin
  FreeRefBuffers;
  //ClearBuffers; /// CR11512

{$IFNDEF VER9P}
  SetLength(RangeColumns, 0); // anti-warning
{$ENDIF}
  if (Data is TMemData) and (TMemData(Data).IndexFieldCount > 0)  then begin
    if FRanged and Data.IsFullReopen then
      RangeColumns := SaveRangeColumns;

    DataReopen;

    if FRanged and Data.IsFullReopen then begin
      RestoreRangeColumns(RangeColumns);
      TMemData(Data).ClearItemsOmittedStatus;
    end;

    TMemData(Data).SortItems;
  end
  else
    DataReopen;

  CreateConstraints;
end;

procedure TMemDataSet.DoAfterOpen;
{$IFNDEF FPC}
var
  i: integer;
{$ENDIF}
begin
  inherited;

  if SendDataSetChangeEventAfterOpen then
    DataEvent(deDataSetChange, 0);

{$IFNDEF FPC}
  for i := 0 to NestedDataSets.Count - 1 do
    with TDataSet(NestedDataSets[i]) do
      if Active then
        DataEvent(deParentScroll, 0);
{$ENDIF}
end;

procedure TMemDataSet.FreeRefBuffers;
var
  i: integer;
begin
  if FNeedAddRef then begin
    for i := 0 to BufferCount do
      FreeRefComplexFields(IntPtr(Buffers[i]));
    FreeRefComplexFields(IntPtr(TempBuffer));
  end;
end;

procedure TMemDataSet.AddRefComplexFields(Buffer: TRecordBuffer);
var
  RecInfo: PRecInfo;
begin
  if FNeedAddRef then begin
    RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
    RecInfo.RefComplexFields := True;
    Data.AddRef(Buffer);
  end;
end;

procedure TMemDataSet.FreeRefComplexFields(Buffer: TRecordBuffer; WithBlob: boolean = True);
var
  RecInfo: PRecInfo;
begin
{$IFNDEF FPC}
  Assert(Data <> nil);
{$ELSE}
  if (Data = nil) or (Buffer = nil) then
    Exit;
{$ENDIF}

  RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
  if RecInfo.RefComplexFields then begin
    Data.ReleaseRef(Buffer, FIsResync, WithBlob);
    RecInfo.RefComplexFields := False;
  end;
end;

{ Field Management }

function TMemDataSet.GetFieldTypeMapClass: TFieldTypeMapClass;
begin
  Result := TFieldTypeMap;
end;

procedure TMemDataSet.InternalInitFieldDefs;
var
  CheckDefs: boolean;
  i: integer;
  OldFieldNames: array of string;
  OldFieldCount: integer;
begin
  // can't CreateFieldDefs if FieldDefs.Update(InitFieldDefs)
  Assert(Data <> nil);
  if not Data.Active then begin
    OldFieldCount := Data.Fields.Count;
    CheckDefs := (FieldDefs <> nil) and not FieldDefs.Updated;
    if CheckDefs then begin
      SetLength(OldFieldNames, OldFieldCount);
      for i := 0 to OldFieldCount - 1 do
        OldFieldNames[i] := Data.Fields[i].Name;
    end;

    Data.ExplicitInitFields;

    if OldFieldCount <> Data.Fields.Count then
      FieldDefs.Updated := False
    else
      if CheckDefs then
        for i := 0 to OldFieldCount - 1 do
          if OldFieldNames[i] <> Data.Fields[i].Name then begin
            FieldDefs.Updated := False;
            Break;
          end;

    if FDataSetService <> nil then
      FDataSetService.PreInitCursor;

    CreateFieldDefs;
  end;
end;

function TMemDataSet.GetFieldType(DataType: word): TFieldType;
begin
  Result := GetFieldTypeMapClass.GetFieldType(DataType);
end;

function TMemDataSet.GetFieldType(FieldDesc: TFieldDesc; out FieldSize, FieldLength, FieldScale: Integer): TFieldType;
begin
  Result := GetFieldType(FieldDesc.DataType);
  FieldSize := GetFieldDefSize(Result, FieldDesc.Length);
  FieldLength := FieldDesc.Length;
  FieldScale := FieldDesc.Scale;
end;

function TMemDataSet.GetObjectFieldDefName(Parent: TFieldDef; Index: integer; ObjType: TObjectType): string;
{$IFNDEF FPC}
var
  ParentDef: TFieldDef;
{$ENDIF}
begin
{$IFNDEF FPC}
  if NeedComplexUpdateFieldDefList and (ObjType.DataType = dtArray) then begin
    Result := Parent.Name;

    // if array is property of object
    ParentDef := Parent.ParentDef;
    while (ParentDef <> nil) and (ParentDef.DataType <> ftArray) do begin
      Result := ParentDef.Name + '.' + Result;
      ParentDef := ParentDef.ParentDef;
    end;

    Result := Result + '[' + IntToStr(Index) + ']';
    //To correct DB.pas ADT name handling
  end
  else
{$ENDIF}
    Result := IntToStr(Index);
end;

function TMemDataSet.GetFieldDefSize(FieldType: TFieldType; FieldLength: Integer): Integer;
begin
  Result := 0;
  case FieldType of
    ftString, ftWideString, ftGuid: begin
      Result := FieldLength;
      if Result = 0 then
        Result := 1;  // For SELECT NULL FROM ...
    end;
    ftBytes, ftVarBytes:
      Result := FieldLength;
  end;
end;

function TMemDataSet.NeedCreateFieldDefs: boolean;
begin
  Result := not FieldDefs.Updated or Data.FieldListDependsOnParams;
end;

function TMemDataSet.CreateFieldDef(FieldDesc: TFieldDesc): TFieldDef;
var
  FieldSize: Integer;
  FieldLength: Integer;
  FieldScale: Integer;
  FieldType: TFieldType;
begin
  Result := nil;
  FieldType := GetFieldType(FieldDesc, FieldSize, FieldLength, FieldScale);
  if FieldType <> ftUnknown then begin
    // FieldNo 1..
    Result := TFieldDef.Create(FieldDefs, FieldDesc.Name, FieldType, FieldSize,
      FieldDesc.Required and FLocalConstraints, FieldDesc.FieldNo);

    if FieldDesc.ReadOnly then
      Result.Attributes := Result.Attributes + [DB.faReadonly];

    if FieldType in [ftSmallint, ftWord, ftInteger, ftLargeint,
      {$IFDEF VER12P}ftByte, ftShortint, ftLongword,{$ENDIF}
      {$IFDEF VER14P}ftSingle,{$ENDIF} ftFloat]
    then
      Result.Precision := FieldLength
    else if FieldType in [ftBCD, ftFMTBCD] then begin
      Result.Precision := FieldLength;
      Result.Size := FieldScale;
    end
    else if FieldType in [ftADT, ftArray] then
      CreateObjectFields(FieldDesc.ObjectType, Result, FieldDesc.FieldNo + 1);

    if FieldDesc.Hidden then
      Result.Attributes := Result.Attributes + [DB.faHiddenCol];
    if FieldDesc.Fixed then
      Result.Attributes := Result.Attributes + [DB.faFixed];
  end
  else
    DatabaseError(SDataTypeNotSupported, Self);
end;

function TMemDataSet.CreateObjectFields(ObjType: TObjectType; Parent: TFieldDef; FieldNo: Integer): Integer;

  function GetObjectFieldName(const ParentFieldName, FieldName: string; Index: integer): string;
  var
    NewFieldName: string;
  begin
    if Index = 0 then
      NewFieldName := FieldName
    else
      NewFieldName := FieldName + '_' + IntToStr(Index);

    if (FieldDefs.IndexOf(ParentFieldName + '.' + NewFieldName) <> -1) then
      Result := GetObjectFieldName(ParentFieldName, FieldName, Index + 1)
    else
      Result := NewFieldName;
  end;

var
  i: integer;
  Attribute: TAttribute;
  Field: TFieldDesc;
  FieldDef: TFieldDef;
  FieldType: TFieldType;
  FieldSize: Integer;
  FieldLength: Integer;
  FieldScale: Integer;
  Item, CountItem: integer;
  FieldName: string;
begin
  if (ObjType.DataType = dtObject) or SparseArrays then
    CountItem := 1
  else begin
    CountItem := ObjType.Size;
    if CountItem > MaxArrayItem then // Restriction of array length
      CountItem := MaxArrayItem;
  end;

  for i := 0 to ObjType.AttributeCount - 1 do begin
    for Item := 0 to CountItem - 1 do begin
      Attribute := ObjType.Attributes[i];
      Field := Data.Fields[FieldNo - 1];

      FieldType := GetFieldType(Field, FieldSize, FieldLength, FieldScale);
      if ObjType.DataType = dtObject then
        FieldName := Attribute.Name
      else
        FieldName := GetObjectFieldDefName(Parent, Item, ObjType);
      FieldName := GetObjectFieldName(Parent.Name, FieldName, 0);

      FieldDef := TFieldDef.Create(Parent.{$IFNDEF FPC}ChildDefs{$ELSE}Collection as TFieldDefs{$ENDIF}, FieldName, FieldType, FieldSize,
        False, Parent.FieldNo + i * CountItem + Item + 1);

      if DB.faReadonly in Parent.Attributes then
        FieldDef.Attributes := FieldDef.Attributes + [DB.faReadonly];

      if (Field <> nil) and (FieldType in [ftSmallint, ftWord, ftInteger, ftLargeint,
        {$IFDEF VER12P}ftByte, ftShortint, ftLongword,{$ENDIF}
        {$IFDEF VER14P}ftSingle,{$ENDIF} ftFloat])
      then
        FieldDef.Precision := FieldLength
      else if FieldType in [ftBCD, ftFMTBCD] then begin
        FieldDef.Precision := FieldLength;
        FieldDef.Size := FieldScale;
      end;

      Inc(FieldNo);
      if FieldType in [ftADT, ftArray] then
        FieldNo := CreateObjectFields(Attribute.ObjectType, FieldDef, FieldNo);
    end;
  end;

  Result := FieldNo;
end;

procedure TMemDataSet.CreateFieldDefs;
var
  i: integer;
  FieldDesc: TFieldDesc;
begin
  Assert(Data <> nil);
  if not NeedCreateFieldDefs then
    Exit;

  FieldDefs.BeginUpdate;
  try
    FieldDefs.Clear;
    for i := 0 to Data.Fields.Count - 1 do begin
      FieldDesc := Data.Fields[i];
      if not FieldDesc.HiddenObject and
        (FieldDesc.FieldDescKind = fdkData) and
        (not FieldDesc.HasParent or FieldDesc.ParentField.HiddenObject)
      then
        CreateFieldDef(FieldDesc);

    end;
  finally
    FieldDefs.EndUpdate;
  end;
{$IFNDEF FPC}
  FieldDefs.Updated := False;
  UpdateFieldDefList;
{$ENDIF}
  FieldDefs.Updated := Data.Fields.Count <> 0; // bug with prepare method that does not get fields fixed
end;

{$IFNDEF FPC}
procedure TMemDataSet.UpdateFieldDefList;

  procedure BeforeUpdateFieldDefList(FieldDefs: TFieldDefs);
  var
    i: integer;
  begin
    // Update FieldDefs before FieldDefList updating
    for i := 0 to FieldDefs.Count - 1 do begin
      if FieldDefs[i].DataType = ftArray then begin
        FieldDefs[i].DataType := ftADT;
        FieldDefs[i].Size := 0;
        FieldDefs[i].Attributes := FieldDefs[i].Attributes + [faUnNamed];
      end;

      if FieldDefs[i].HasChildDefs then
        BeforeUpdateFieldDefList(FieldDefs[i].ChildDefs);
    end;
  end;

  procedure AfterUpdateFieldDefList(FieldDefs: TFieldDefs);
  var
    i: integer;
  begin
    // Update FieldDefs after FieldDefList updating
    for i := 0 to FieldDefs.Count - 1 do begin
      if (FieldDefs[i].DataType = ftADT) and (faUnNamed in FieldDefs[i].Attributes) then begin
        FieldDefs[i].DataType := ftArray;
        FieldDefs[i].Size := 0;
        FieldDefs[i].Attributes := FieldDefs[i].Attributes - [faUnNamed];
      end;

      if FieldDefs[i].HasChildDefs then
        AfterUpdateFieldDefList(FieldDefs[i].ChildDefs);
    end;
  end;

begin
  if NeedComplexUpdateFieldDefList then
    BeforeUpdateFieldDefList(FieldDefs);

  FieldDefList.Update;

  if NeedComplexUpdateFieldDefList then
    AfterUpdateFieldDefList(FieldDefs);
end;
{$ENDIF}

function TMemDataSet.NeedComplexUpdateFieldDefList;
begin
  Result := false;
end;

{$IFNDEF FPC}
procedure TMemDataSet.GetObjectTypeNames(Fields: TFields);
var
  i: integer;
  Field: TField;
  ObjectField: TObjectField;
begin
  for i := 0 to Fields.Count - 1 do begin
    Field := Fields[i];
    if Field is TObjectField then begin
      ObjectField := TObjectField(Field);

      ObjectField.ObjectType := GetFieldDesc(ObjectField).ObjectType.Name;

      with ObjectField do
        if DataType in [ftADT, ftArray] then begin
          if (DataType = ftArray) and SparseArrays and
             (Fields[0].DataType = ftADT)
          then
            GetObjectTypeNames(TObjectField(Fields[0]).Fields)
        else
          GetObjectTypeNames(Fields);
      end;
    end;
  end;
end;
{$ENDIF}

function TMemDataSet.GetFieldDescNo(Field: TField): integer;
var
  FieldDesc: TFieldDesc;
begin
  FieldDesc := GetFieldDesc(Field);
  if FieldDesc <> nil then
    Result := FieldDesc.FieldNo
  else
    raise Exception.Create(Format(SFieldNotFound, [Field.FieldName]));
end;

function TMemDataSet.GetFieldDesc(const Field: TField): TFieldDesc;
var
  FieldDesc: TFieldDesc;
  i: integer;
  Found: boolean;
begin
  Assert(Data <> nil, 'FIRecordSet must be setted to this time');
  Assert(Field <> nil, 'Field cannot be nil');
  Assert((Field.DataSet = Self) or (Field.DataSet = nil {CR 22356}), 'Wrong DataSet');
  {if Field.DataSet <> Self then
    for i := 0 to Data.FieldCount - 1 do
      if _LowerCase(Field.FieldName) = _LowerCase(Data.Fields[i].Name) then begin /// Field.FieldName must be equal with FieldDesc.Name
        Result := Data.Fields[i];
        Exit;
      end;}

  Result := nil;
  if Field.FieldNo > 0 then
    Result := TFieldDesc(Data.Fields[Field.FieldNo - 1])
  else
  begin
  {$IFDEF VER20P}
    if not (lcPersistent in Fields.LifeCycles) and
  {$ELSE}
    if DefaultFields and
  {$ENDIF}
       (Fields.Count = Data.Fields.Count {just in case; should be moved to Assert})
    then begin
      i := Fields.IndexOf(Field);
      Result := Data.Fields[i];
    end
    else
      for i := 0 to Data.Fields.Count - 1 do begin
        FieldDesc := Data.Fields[i];
        Found := SameText(Field.FieldName, FieldDesc.Name); /// Field.FieldName must be equal with FieldDesc.Name
        if (Field.FieldKind <> fkData) then begin
          if FieldDesc.FieldDescKind = fdkData then
            Found := False
          else
          if not Found then
            Found := SameText(Field.FieldName, FieldDesc.ActualName);
        end;
        if Found then begin
          Result := FieldDesc;
          Break;
        end;
      end;
  end;
  // Assert(_LowerCase(Field.FieldName) = _LowerCase(Result.Name), Format('Field.FieldName <> FieldDesc.Name' + DALineSeparator + '"%s" <> "%s"' + DALineSeparator, [Field.FieldName, Result.Name]));
end;

function TMemDataSet.GetFieldDesc(const FieldName: string): TFieldDesc;
begin
  if Data = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Result := Data.FieldByName(FieldName);
end;

function TMemDataSet.GetFieldDesc(const FieldNo: integer): TFieldDesc;
begin
  if (Data = nil) or (FieldNo <= 0) then {fkCalculated, fkLookup}
    Result := nil
  else
    Result := TFieldDesc(Data.Fields[FieldNo - 1])
end;

{$IFDEF VER17P}
function TMemDataSet.GetFieldData(FieldNo: integer; {$IFDEF VER18P}var{$ENDIF} Buffer: TValueBuffer): boolean;
begin
  Result := GetFieldData(FieldNo, IntPtr(Buffer));
end;

function TMemDataSet.GetFieldData(Field: TField; {$IFDEF VER18P}var{$ENDIF} Buffer: TValueBuffer): boolean;
begin
  Result := GetFieldData(Field, IntPtr(Buffer));
{$IFDEF VER25P}
  if Result and (Field.DataType in [ftBCD, ftSmallint, ftWord]) then
    Result := InternalDataConvert(Field, Buffer, Buffer, True);
{$ENDIF}
end;

function TMemDataSet.GetFieldData(Field: TField; {$IFDEF VER18P}var{$ENDIF} Buffer: TValueBuffer; NativeFormat: Boolean): Boolean;
begin
  if (Field.DataType = ftWideString) and (Buffer <> nil) then
    Result := inherited GetFieldData(Field, Buffer, True)
{$IFDEF VER25P}
  else if Field.DataType in [ftBCD, ftSmallint, ftWord] then
    Result := GetFieldData(Field, IntPtr(Buffer))
{$ENDIF}
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;
{$ENDIF}

{$IFNDEF FPC}
function TMemDataSet.GetFieldData(FieldNo: integer; Buffer: IntPtr): boolean;
var
  RecBuf: TRecordBuffer;
  Field: TField;
begin
//if BlockReadSize > 0 then

  Result := GetActiveRecBuf(RecBuf);
  if Result then begin
    Assert(Data <> nil);
    Field := FieldByNumber(FieldNo);
    if Field <> nil then begin
      Result := GetFieldData(Field, Buffer);
      if Result and (Field.DataType in [ftBCD, ftSmallint, ftWord]) then
        Result := InternalDataConvert(Field, Buffer, Buffer, True);
    end
    else
      Result := False;
  end;
end;
{$ENDIF}

function TMemDataSet.GetFieldData(Field: TField; Buffer: IntPtr): boolean;
var
  i: integer;
  BufferLen: Word;
  IsBlank: boolean;
  RecBuf: TRecordBuffer;
  FieldBuf: IntPtr;
  FieldDesc: TFieldDesc;
{$IFNDEF VER10P}
  DataOffset: Integer;
{$ENDIF}
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then
    Exit;

  Assert(Data <> nil);
  if Field.FieldNo > 0 then begin
    FieldDesc := GetFieldDesc(Field.FieldNo);
    Data.GetField(FieldDesc, RecBuf, Buffer, BufferLen, True, IsBlank);
    Result := not IsBlank;
  end
  else if State in  [dsBrowse, dsEdit, dsInsert, dsCalcFields, dsFilter, dsBlockRead] then
    if FCreateCalcFieldDescs then begin
      FieldDesc := nil;
      for i := 0 to Length(FCalcFieldsMapping) - 1 do
        if FCalcFieldsMapping[i].Field = Field then begin
          FieldDesc := FCalcFieldsMapping[i].FieldDesc;
          break;
        end;
      if FieldDesc <> nil then
        Data.GetField(FieldDesc, RecBuf, Buffer, BufferLen, True, IsBlank)
      else
        Data.GetField(GetFieldDesc(Field), RecBuf, Buffer, BufferLen, True, IsBlank);
      Result := not IsBlank;
    end
    else begin
      FieldBuf := PtrOffset(RecBuf, RecordSize + Field.Offset);
      Result := Boolean(Marshal.ReadByte(FieldBuf));
      if Result and (Buffer <> nil) then
      {$IFNDEF VER10P}
        if Field.DataType = ftWideString then begin
          DataOffset := Integer(PtrOffset(FieldBuf, 1)^);
          CopyBuffer(PtrOffset(RecBuf, FWideStringOfs + DataOffset), Buffer, (Field.Size + 1) * SizeOf(WideChar));
        end
        else
      {$ENDIF}
          CopyBuffer(PtrOffset(FieldBuf, 1), Buffer, Field.DataSize);
    end;
end;

function TMemDataSet.GetFieldData(Field: TField; Buffer: IntPtr; NativeFormat: Boolean): Boolean;
{$IFNDEF FPC}
{$IFNDEF VER10P}
var
  Temp: PWideChar;
{$ENDIF}
{$ENDIF}
begin
  if (Field.DataType = ftWideString) and (Buffer <> nil) then begin
  {$IFDEF VER10P}
    Result := inherited GetFieldData(Field, {$IFDEF NEXTGEN}TBytes{$ENDIF}(Buffer), True);
  {$ELSE}
  {$IFDEF FPC}
    Result := inherited GetFieldData(Field, Buffer, True);
  {$ELSE}
    { Cannot copy direct - may be conflict with Delphi string manager
    SetLength(WideString(Buffer^), Field.Size * sizeof(WideChar));
    Result := inherited GetFieldData(Field, PWideChar(WideString(Buffer^)), True);}
    GetMem(Temp, (Field.Size + 1 {#0} + 8) * sizeof(WideChar));//+ 8 for numbers
    try
      Result := inherited GetFieldData(Field, Temp, True);
      if Result then
        WideString(Buffer^) := Temp
      else
        WideString(Buffer^) := '';
    finally
      FreeMem(Temp);
    end;
  {$ENDIF}
  {$ENDIF}
  end
  else
    Result := inherited GetFieldData(Field, {$IFDEF NEXTGEN}TBytes{$ENDIF}(Buffer), NativeFormat);
end;

function TMemDataSet.PrepareValueBuffer(Field: TField; var Buffer: IntPtr): Word;
{$IFNDEF FPC}
{$IFNDEF VER17P}
var
  SafeArray: PVarArray;
{$ENDIF}
{$ENDIF}
begin
  case Field.DataType of
    ftString, ftGuid:
      if Buffer <> nil then
        Result := StrLen(PansiChar(Buffer))
      else
        Result := 0;
    ftWideString:
      if Buffer <> nil then begin
      {$IFNDEF FPC}{$IFNDEF VER10P}
        if IntPtr(Buffer^) <> nil then
          Buffer := IntPtr(Buffer^);
      {$ENDIF}{$ENDIF}
        Result := StrLenW(PWideChar(Buffer))
      end
      else
        Result := 0;
    ftVarBytes:
      if Buffer <> nil then
        Result := PWord(Buffer)^
      else
        Result := 0;
    ftBytes:
      if Buffer <> nil then begin
      {$IFDEF FPC}
        Result := Field.Size;
      {$ELSE}{$IFDEF VER17P}
        Result := Field.Size;
      {$ELSE}
        SafeArray := VarArrayAsPSafeArray(PVariant(Buffer)^);
        Result := SafeArray.Bounds[0].ElementCount - SafeArray.Bounds[0].LowBound;
        Buffer := SafeArray.Data;
      {$ENDIF}{$ENDIF}
      end
      else
        Result := 0;
    else
      Result := 0;
  end;
end;

procedure TMemDataSet.InternalSetFieldData(Field: TField; Buffer: IntPtr; BufferLen: Word);
var
  i: integer;
  RecBuf: TRecordBuffer;
  FieldBuf: IntPtr;
{$IFNDEF VER10P}
  DataOffset, WideStringSize: integer;
{$ENDIF}
  FieldDesc: TFieldDesc;
begin
  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing);

  Assert(Data <> nil);         // must be after state check if fields was created in DT, but SQL removed
  GetActiveRecBuf(RecBuf);

  if Field.FieldNo > 0 then begin
    if State = dsCalcFields then
      DatabaseError(SNotEditing);
    if Field.ReadOnly and not ((State in [dsSetKey, dsFilter]) or FInSettingDefaultExpressionValues) then
      DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
  {$IFDEF VER22P}
    Field.Validate(TValueBuffer(Buffer));
  {$ELSE}
    Field.Validate(Buffer);
  {$ENDIF}
    if Field.FieldKind <> fkInternalCalc then begin
      FieldDesc := GetFieldDesc(Field.FieldNo);
      Data.PutField(FieldDesc, RecBuf, Buffer, BufferLen, True);
    end;
  end
  else
    if FCreateCalcFieldDescs then begin
      FieldDesc := nil;
      for i := 0 to Length(FCalcFieldsMapping) - 1 do
        if FCalcFieldsMapping[i].Field = Field then begin
          FieldDesc := FCalcFieldsMapping[i].FieldDesc;
          break;
        end;
      if FieldDesc <> nil then
        Data.PutField(FieldDesc, RecBuf, Buffer, BufferLen, True)
      else begin
        FieldDesc := GetFieldDesc(Field.FieldNo);
        Data.PutField(FieldDesc, RecBuf, Buffer, BufferLen, True);
      end;
    end
    else begin
      FieldBuf := PtrOffset(RecBuf, RecordSize + Field.Offset);
      Marshal.WriteByte(FieldBuf, Byte(IntPtr(Buffer) <> nil));
      if IntPtr(Buffer) <> nil then
      {$IFNDEF VER10P}
        if Field.DataType = ftWideString then begin
          WideStringSize := BufferLen * 2;
          DataOffset := Integer(PtrOffset(FieldBuf, 1)^);

          CopyBuffer(Buffer, PtrOffset(RecBuf, FWideStringOfs + DataOffset), WideStringSize);
          Marshal.WriteInt16(PtrOffset(RecBuf, FWideStringOfs + DataOffset + WideStringSize), 0);
        end
        else
      {$ENDIF}
          CopyBuffer(Buffer, PtrOffset(FieldBuf, 1), Field.DataSize);
    end;

  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
  {$IFDEF FPC}
    DataEvent(deFieldChange, PtrInt(Field));
  {$ELSE}
  {$IFDEF VER16P}
    DataEvent(deFieldChange, NativeInt(Field));
  {$ELSE}
    DataEvent(deFieldChange, Longint(Field));
  {$ENDIF}
  {$ENDIF}
end;

function TMemDataSet.InternalDataConvert(Field: TField; Source, Dest: IntPtr; ToNative: Boolean): boolean;
var
  TimeStamp: TTimeStamp;
begin
  Result := True;

  case Field.DataType of
    ftDate, ftTime: begin
      Result := False;
      if GetFieldDesc(Field).MapDataType = dtDateTime then begin
    {$IFDEF FPC}
        TimeStamp := MSecsToTimeStamp(Trunc(Double(Source^)));
    {$ELSE}
        TimeStamp := MSecsToTimeStamp(Double(Source^));
    {$ENDIF}
        if Field.DataType = ftDate then
          Integer(Source^) := TimeStamp.Date
        else
          Integer(Source^) := TimeStamp.Time;
      end;
    end;
    ftBCD:
      if ToNative then begin
        if Source <> nil then
          Result := CurrToBCD(Currency(Source^), TBcd(Dest^), 32, Field.Size)
      end
      else
        Marshal.WriteInt64(Dest, Marshal.ReadInt64(Source));
    ftSmallint, ftWord:
      if ToNative then begin
        if (Source <> nil) and (GetFieldDesc(Field).Size = 1) then
          Marshal.WriteByte(Dest, 1, 0);
      end;
    else
  {$IFDEF FPC}
    if (Field.DataType <> ftDateTime) and
       (Field.DataType <> ftTimeStamp) and
       (Field.DataType <> ftFMTBCD) and
       (Field.DataType <> ftWideString)
    then
      CopyBuffer(Source, Dest, GetFieldDesc(Field).Size)
    else
  {$ENDIF}
      Result := False;
  end;
end;

{$IFDEF VER17P}
procedure TMemDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  SetFieldData(Field, IntPtr(Buffer));
end;

procedure TMemDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean);
begin
  case Field.DataType of
    ftBytes:
      InternalSetFieldData(Field, IntPtr(Buffer), Length(Buffer));
    ftBCD, ftFMTBcd:
      SetFieldData(Field, IntPtr(Buffer));
    else
      inherited SetFieldData(Field, Buffer, NativeFormat);
  end;
end;

procedure TMemDataSet.DataConvert(Field: TField; Source: TValueBuffer; {$IFDEF VER18P}var{$ENDIF} Dest: TValueBuffer; ToNative: Boolean);
begin
  if not InternalDataConvert(Field, Source, Dest, ToNative) then
    inherited DataConvert(Field, Source, Dest, ToNative);
end;
{$ENDIF}

procedure TMemDataSet.SetFieldData(Field: TField; Buffer: IntPtr);
var
  BufferLen: Word;
begin
  BufferLen := PrepareValueBuffer(Field, Buffer);
  InternalSetFieldData(Field, Buffer, BufferLen);
end;

procedure TMemDataSet.SetFieldData(Field: TField; Buffer: IntPtr; NativeFormat: Boolean);
begin
  case Field.DataType of
    ftWideString:
      inherited SetFieldData(Field, TValueBuffer(Buffer), {$IFDEF VER12P}NativeFormat{$ELSE}True{$ENDIF});
    ftBytes:
      SetFieldData(Field, Buffer);
    ftBCD, ftFMTBcd:
      SetFieldData(Field, Buffer);
  {$IFDEF VER17P}{$IFNDEF NEXTGEN}
    ftVariant:
      inherited SetFieldData(Field, Buffer, NativeFormat)
  {$ENDIF}{$ENDIF}
    else
      inherited SetFieldData(Field, TValueBuffer(Buffer), NativeFormat);
  end;
end;

{$IFNDEF NEXTGEN}
procedure TMemDataSet.DataConvert(Field: TField; Source, Dest: IntPtr; ToNative: Boolean);
begin
  if not InternalDataConvert(Field, Source, Dest, ToNative) then
    inherited DataConvert(Field, Source, Dest, ToNative);
end;
{$ENDIF}

{$IFDEF NEXTGEN}
procedure TMemDataSet.DestroyFields;
var
  i: integer;
begin
  // XE4 bug (rm-23646)
  for i := 0 to Fields.Count - 1 do
    RemoveComponent(Fields[i]);

  inherited;
end;
{$ENDIF}

function TMemDataSet.GetMaxFieldCount: integer;
begin
  Result := MaxInt;
end;

{$IFNDEF FPC}
function TMemDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
begin
  if ((Self.State = dsInsert) and (State = dsOldValue)) or (State = dsInsert) then
    Result := NULL
  else
    Result := inherited GetStateFieldValue(State, Field);
end;
{$ENDIF}

function TMemDataSet.GetSparseArrays: boolean;
begin
{$IFNDEF FPC}
  Result := inherited SparseArrays;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TMemDataSet.SetSparseArrays(Value: boolean);
begin
{$IFNDEF FPC}
  if SparseArrays <> Value then begin
    UnPrepare;

    inherited SparseArrays := Value;

    if Data <> nil then
      Data.SparseArrays := Value;
  end;
{$ENDIF}
end;

procedure TMemDataSet.CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef);
{$IFDEF FPC}
const
  BaseFieldTypes: array[TFieldType] of TFieldType = (
    ftUnknown, ftString, ftInteger, ftInteger, ftInteger, ftBoolean, ftFloat,
    ftFloat, ftBCD, ftDateTime, ftDateTime, ftDateTime, ftBytes, ftVarBytes,
    ftInteger, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftUnknown,
    ftString, ftUnknown, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet,
    ftBlob, ftBlob, ftVariant, ftInterface, ftInterface, ftString, ftTimeStamp, ftFMTBcd,
    ftFixedWideChar, ftWideMemo);

  CheckTypeSizes = [ftBytes, ftVarBytes, ftBCD, ftReference];
{$ENDIF}
var
  Compatible: Boolean;
begin
  case Field.DataType of
    ftFloat, ftCurrency:
      Compatible := FieldDef.DataType in [ftFloat, ftCurrency];
    ftBCD, ftFMTBcd{$IFDEF VER14P}, ftSingle{$ENDIF}:
      Compatible := FieldDef.DataType = Field.DataType;
  else
  {$IFNDEF FPC}
    if ((Field.DataType = TFieldType(ftDATimeStampOffset)){$IFDEF VER14P} or (Field.DataType = TFieldType(ftTimeStampOffset)){$ENDIF}) and
       (FieldDef.DataType = TFieldType(ftDATimeStampOffset))
    then
      Compatible := True
    else
  {$ENDIF}
      Compatible := False;
  end;
  if not Compatible then
{$IFDEF FPC}
  begin
    if (BaseFieldTypes[Field.DataType] <> BaseFieldTypes[FieldDef.DataType]) then
      DatabaseErrorFmt(SFieldTypeMismatch, [Field.DisplayName,
        FieldTypeNames[Field.DataType], FieldTypeNames[FieldDef.DataType]], Self);
    if (Field.DataType in CheckTypeSizes) and (Field.Size <> FieldDef.Size) then
        DatabaseErrorFmt(SFieldSizeMismatch, [Field.DisplayName, Field.Size,
          FieldDef.Size], Self);
  end;
{$ELSE}
  inherited;
{$ENDIF}
end;

function TMemDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := GetFieldClass(FieldType, dtUnknown);
end;

{$IFDEF VER12P}
function TMemDataSet.GetFieldClass(FieldDef: TFieldDef): TFieldClass;
var
  FieldDesc: TFieldDesc;
begin
  if Assigned(Data) and (Data.Fields.Count >= FieldDef.FieldNo) then begin
    FieldDesc := Data.Fields[FieldDef.FieldNo - 1];
    Result := GetFieldClass(FieldDef.DataType, FieldDesc.DataType);
  end
  else
    Result := GetFieldClass(FieldDef.DataType, dtUnknown);
end;
{$ENDIF}

function TMemDataSet.GetFieldClass(FieldType: TFieldType; DataType: Word): TFieldClass;
begin
  CheckDataSetService;
  Result := FDataSetService.GetFieldClass(FieldType, DataType);
end;

function TMemDataSet.InternalGetBlob(FieldDesc: TFieldDesc): TBlob;
var
  RecBuf: TRecordBuffer;
  DataBuf: IntPtr;
  DataLen: Word;
  IsBlank: boolean;
begin
  Assert(Data <> nil);

  if GetActiveRecBuf(RecBuf) then begin
    if not FieldDesc.IsBlob then
      DatabaseError(SNeedBlobType);

    DataBuf := nil;
    Data.GetField(FieldDesc, RecBuf, @DataBuf, DataLen, False, IsBlank);
    Result := TBlob(GetGCHandleTarget(DataBuf));
  end
  else
    Result := nil;
end;

function TMemDataSet.GetBlob(const FieldName: string): TBlob;
var
  FieldDesc: TFieldDesc;
begin
  Assert(Data <> nil);
  FieldDesc := Data.FieldByName(FieldName);
  Result := InternalGetBlob(FieldDesc);
end;

function TMemDataSet.GetBlob(Field: TField): TBlob;
var
  FieldDesc: TFieldDesc;
begin
  if (Field <> nil) and Active then begin
    FieldDesc := GetFieldDesc(Field);
    if FieldDesc <> nil then
      Result := InternalGetBlob(FieldDesc)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

{ Constraints }
procedure TMemDataSet.CreateConstraints;
begin
  if (Data = nil) or not Data.Active then
    Exit;

  CreateCheckConstraints;
  CreateFieldConstrain;
end;

function TMemDataSet.InternalSetBlob(FieldDesc: TFieldDesc; Blob: TBlob): boolean;
var
  OldBlob: TBlob;
  RecBuf: TRecordBuffer;
begin
  Assert(FieldDesc <> nil);
  Assert(Blob <> nil);
  OldBlob := InternalGetBlob(FieldDesc);
  Assert(OldBlob <> nil);
  Result := Blob.ClassType = OldBlob.ClassType;
  if GetActiveRecBuf(RecBuf) then begin
    if not FieldDesc.IsBlob then
      DatabaseError(SNeedBlobType);

    Blob.AddRef;
    OldBlob.Free;
    Marshal.WriteIntPtr(RecBuf, FieldDesc.DataOffset, Blob.GCHandle);
  end
  else
    Result := False;
end;

function TMemDataSet.SetBlob(Field: TField; Blob: TBlob): boolean;
var
  FieldDesc: TFieldDesc;
begin
  if Field <> nil then begin
    FieldDesc := GetFieldDesc(Field);
    if FieldDesc = nil then
      raise Exception.Create(Format(SFieldNotFound, [Field.FieldName]));
    Result := InternalSetBlob(FieldDesc, Blob);
  end
  else
    Result := False;
end;

{function TMemDataSet.Translate(const Src: string; var Dest: string; boolean): integer;
begin
  inherited Translate(Src, Dest, ToOem);
  Result := StrLen(Src);
{  if ToOem then
    AnsiToNativeBuf(Locale, Src, Dest, Result)
  else
    NativeToAnsiBuf(Locale, Src, Dest, Result);
  if Src <> Dest then
    Dest[Result] := #0;
end;}

{ Buffer/Record Management }

{$IFDEF NEXTGEN}
function TMemDataSet.AllocRecBuf: TRecBuf;
begin
  Result := NativeInt(AllocRecordBuffer);
end;

procedure TMemDataSet.FreeRecBuf(var Buffer: TRecBuf);
begin
  FreeRecordBuffer(TRecordBuffer(Buffer));
end;
{$ENDIF}

function TMemDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := Marshal.AllocHGlobal(FRecBufSize);
  PRecInfo(PtrOffset(Result, FRecInfoOfs)).RefComplexFields := False;
end;

procedure TMemDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeRefComplexFields(Buffer);
  Marshal.FreeHGlobal(Buffer);
  Buffer := nil;
end;

{$IFDEF VER18P}
function TMemDataSet.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := GetRecord(TRecordBuffer(Buffer), GetMode, DoCheck);
end;
{$ENDIF}

function TMemDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RecInfo: PRecInfo;
begin
  Assert(Data <> nil);
  FreeRefComplexFields(Buffer);
  case GetMode of
    gmCurrent:
      Data.GetRecord(Buffer);
    gmNext:
      Data.GetNextRecord(Buffer);
    gmPrior:
      Data.GetPriorRecord(Buffer);
  end;
  if Data.BOF then
    Result := grBOF
  else
    if Data.EOF then
      Result := grEOF
    else begin
      RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
      RecInfo.RecordNumber := Data.RecordNo;
      RecInfo.UpdateStatus := TUpdateStatus(Data.GetUpdateStatus);
      RecInfo.BookmarkFlag := bfCurrent;
      AddRefComplexFields(Buffer);

      //ClearBlobCache(Buffer);
      if not FCacheCalcFields then
        GetCalcFields({$IFDEF VER18P}NativeInt{$ENDIF}(Buffer));
      //SetBookmarkFlag(Buffer, bfCurrent);
      Data.GetBookmark(PRecBookmark(PtrOffset(Buffer, FBookmarkOfs)));

      Result := grOK;
    end;
end;

{$IFDEF VER18P}
procedure TMemDataSet.InternalInitRecord(Buffer: TRecBuf);
begin
  InternalInitRecord(TRecordBuffer(Buffer));
end;
{$ENDIF}

procedure TMemDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  Assert(Data <> nil);
  FreeRefComplexFields(Buffer);
  Data.InitRecord(Buffer);
  if Data.HasComplexFields then
    Data.CreateComplexFields(Buffer, True);
end;

{$IFDEF VER18P}
procedure TMemDataSet.InitRecord(Buffer: TRecBuf);
begin
  InitRecord(TRecordBuffer(Buffer));
end;
{$ENDIF}

procedure TMemDataSet.InitRecord(Buffer: TRecordBuffer);
var
  RecInfo: PRecInfo;
begin
  inherited InitRecord({$IFDEF NEXTGEN}NativeInt{$ENDIF}(Buffer));

  Assert(Data <> nil);
  //ClearBlobCache(Buffer);
  RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
  RecInfo.RecordNumber := -1;
  RecInfo.UpdateStatus := TUpdateStatus(usInserted);
  RecInfo.BookMarkFlag := bfInserted;
  Data.InitRecord(IntPtr(FOldRecBuf));  // clear OldRecBuf
end;

function TMemDataSet.GetOldRecBuf: TRecordBuffer;
begin
  if FInDeferredPost then
    Result := FOldDeferredPostBuf
  else
  if InCacheProcessing then
    Result := FOldCacheRecBuf
  else
    Result := GetOldRecord;
end;

function TMemDataSet.GetNewRecBuf: TRecordBuffer;
begin
  if InCacheProcessing then
    Result := FNewCacheRecBuf
  else
    Result := IntPtr(ActiveBuffer);
end;

function TMemDataSet.GetActiveRecBuf(out RecBuf: TRecordBuffer): boolean;
begin
  case State of
    dsBlockRead:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := IntPtr(ActiveBuffer);
    dsBrowse:
      if InCacheProcessing then
        RecBuf := FNewCacheRecBuf
      else
        if IsEmpty or not IsCursorOpen then
          RecBuf := nil
        else
          RecBuf := IntPtr(ActiveBuffer);
    dsEdit, dsInsert:
      RecBuf := IntPtr(ActiveBuffer);
    dsCalcFields:
      RecBuf := IntPtr(CalcBuffer);
    dsFilter:
      RecBuf := FFilterBuffer;
    dsNewValue:
      RecBuf := GetNewRecBuf;
    dsOldValue:
      RecBuf := GetOldRecBuf;
    dsSetKey:
      RecBuf := FRangeCurrentBuffer;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TMemDataSet.GetOldRecord: TRecordBuffer;
begin
  Assert(Data <> nil);
  UpdateCursorPos;
  Data.GetOldRecord(IntPtr(FOldRecBuf));
  Result := IntPtr(FOldRecBuf);
end;

{$IFDEF VER18P}
procedure TMemDataSet.ClearCalcFields(Buffer: NativeInt);
begin
  ClearCalcFields(TRecordBuffer(Buffer));
end;
{$ENDIF}

procedure TMemDataSet.ClearCalcFields(Buffer: TRecordBuffer);
var
  i: integer;
{$IFNDEF VER10P}
  DataOffset: integer;
  Field: TField;
{$ENDIF}
  FieldDesc: TFieldDesc;
begin
  if FCreateCalcFieldDescs then begin
    for i := 0 to Data.Fields.Count - 1 do begin
      FieldDesc := Data.Fields[i];
      if FieldDesc.FieldDescKind <> fdkData then
        Data.SetNull(FieldDesc, Buffer, True);
    end;
  end
  else begin
    FillChar(PtrOffset(Buffer, RecordSize), CalcFieldsSize, 0);

  {$IFNDEF VER10P}
    DataOffset := 0;
    for i := 0 to Fields.Count - 1 do begin
      Field := Fields[i];
      if (Field.DataType = ftWideString) and (Field.FieldKind in [fkCalculated, fkLookUp]) then begin
        Marshal.WriteInt32(PtrOffset(Buffer, RecordSize + Field.Offset + 1), DataOffset);
        DataOffset := DataOffset + (Field.Size + 1) * sizeof(WideChar);
      end;
    end;
    FillChar(PtrOffset(Buffer, FWideStringOfs), DataOffset, 0);
  {$ENDIF}
  end;
end;

// WAR don't support BlockRead

{$IFNDEF FPC}
procedure TMemDataSet.SetBlockReadSize(Value: Integer);
begin
  if Value <> BlockReadSize then begin
    if (Value > 0) or (Value < -1) then
      UpdateCursorPos;
    inherited;
  end;
end;

procedure TMemDataSet.BlockReadNext;
begin
  inherited;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TMemDataSet.SetBufListSize(Value: Integer);
const
  DefaultBufferCount = 10;
begin
  if (BufferCount = -1) and (Value = DefaultBufferCount) then
    Value := 1;

  inherited SetBufListSize(Value);
end;
{$ENDIF}

{ Bookmarks }

{$IFDEF VER17P}
procedure TMemDataSet.GetBookmarkData(Buffer: {$IFDEF VER18P}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}; Bookmark: TBookmark);
begin
  GetBookmarkData(TRecordBuffer(Buffer), IntPtr(Bookmark));
end;

procedure TMemDataSet.SetBookmarkData(Buffer: {$IFDEF VER18P}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}; Bookmark: TBookmark);
begin
  SetBookmarkData(TRecordBuffer(Buffer), IntPtr(Bookmark));
end;

procedure TMemDataSet.InternalGotoBookmark(Bookmark: TBookmark);
begin
  InternalGotoBookmark(IntPtr(Bookmark));
end;
{$ENDIF}

{$IFDEF VER18P}
function TMemDataSet.GetBookmarkFlag(Buffer:  TRecBuf): TBookmarkFlag;
begin
  Result := GetBookmarkFlag(TRecordBuffer(Buffer));
end;

procedure TMemDataSet.SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);
begin
  SetBookmarkFlag(TRecordBuffer(Buffer), Value);
end;
{$ENDIF}

procedure TMemDataSet.GetBookmarkData(Buffer: TRecordBuffer; Bookmark: Pointer);
begin
  CopyBuffer(PtrOffset(Buffer, FBookmarkOfs), Bookmark, BookmarkSize);
end;

procedure TMemDataSet.SetBookmarkData(Buffer: TRecordBuffer; Bookmark: Pointer);
begin
  CopyBuffer(Bookmark, PtrOffset(Buffer, FBookmarkOfs), BookmarkSize);
end;

procedure TMemDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  Assert(Data <> nil);
  Data.SetToBookMark(PRecBookmark(Bookmark));
end;

function TMemDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(PtrOffset(Buffer, FRecInfoOfs)).BookmarkFlag;
end;

procedure TMemDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(PtrOffset(Buffer, FRecInfoOfs)).BookmarkFlag := Value;
end;

procedure ChangeDecimalSeparator(var Value: string; const OldSeparator, NewSeparator: Char);
var
  i: integer;
begin
  if OldSeparator <> NewSeparator then
    for i := 1 to Length(Value) do
      if Value[i] = OldSeparator then begin
        Value[i] := NewSeparator;
        Break;
      end;
end;

procedure TMemDataSet.SaveToXML(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToXML(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TMemDataSet.SaveToXML(Destination: TStream);
begin
  CheckDataSetService;
  FDataSetService.SaveToXML(Destination);
end;

function TMemDataSet.BookmarkValid(Bookmark: TBookmark): boolean;
begin
  if Data <> nil then
    Result := Data.BookmarkValid(PRecBookmark(Bookmark))
  else
    Result := False;
end;

function TMemDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): integer;
begin
  if Data <> nil then
    Result := Data.CompareBookmarks(PRecBookmark(Bookmark1), PRecBookmark(Bookmark2))
  else
    Result := 0;
end;

{ Navigation }

{$IFDEF VER18P}
procedure TMemDataSet.InternalSetToRecord(Buffer: TRecBuf);
begin
  InternalSetToRecord(TRecordBuffer(Buffer));
end;
{$ENDIF}

procedure TMemDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  InternalGotoBookmark(PtrOffset(Buffer, FBookmarkOfs));

  if Data.NeedGetRecordAfterGotoBookmark and (GetBookmarkFlag(Buffer) <> bfInserted) then
    GetRecord(Buffer, gmCurrent, True);
end;

procedure TMemDataSet.SetIndexFieldNames(const Value: string);
begin
  if FIndexFieldNames <> Value then begin
    if Active then begin
      CheckBrowseMode;
      UpdateCursorPos;
    end;
    if (Data <> nil) and (Data is TMemData) then begin
      if FRanged then begin
        ResetRange;
        TMemData(Data).ClearItemsOmittedStatus;
      end;
      TMemData(Data).SetIndexFieldNames(Value);
      if Active and not UseLocalMasterDetailFilter then
        Resync([]);
    end;

    FIndexFieldNames := Value;
  end;
end;

procedure TMemDataSet.CheckSetKeyMode;
begin
  if State <> dsSetKey then
    DatabaseError(SNotEditing, Self);
end;

procedure TMemDataSet.ClearRangeBuffer(Buffer: TRecordBuffer);
var
  i: Integer;
begin
  FillChar(Buffer, FRecBufSize, 0);
  for i := 0 to Data.Fields.Count - 1 do
    Data.SetNull(Data.Fields[i], Buffer, True);
end;

procedure TMemDataSet.InitRangeBuffer(Buffer: TRecordBuffer; Clear: Boolean);
var
  i: integer;
  IndexField: TSortColumn;
begin
  if TMemData(Data).IndexFieldCount = 0 then
    DatabaseError(SNoFieldIndexes, Self);

  SetState(dsSetKey);
  FRangeCurrentBuffer := Buffer;

  if Clear then
    ClearRangeBuffer(FRangeCurrentBuffer);

  for i := 0 to TMemData(Data).IndexFieldCount - 1 do begin
    IndexField := TMemData(Data).IndexFields[i];
    if Buffer = FRangeStartBuffer then
      IndexField.UseForRangeStart := True
    else if Buffer = FRangeEndBuffer then
      IndexField.UseForRangeEnd := True;
  end;
end;

procedure TMemDataSet.SetRangeBuffer(Buffer: TRecordBuffer; const Values: array of const);
var
  i: integer;
  IndexField: TSortColumn;
begin
  InitRangeBuffer(Buffer, True);

  for i := 0 to TMemData(Data).IndexFieldCount - 1 do begin
    IndexField := TMemData(Data).IndexFields[i];
    if i <= High(Values) then
      FieldByNumber(IndexField.FieldDesc.FieldNo).AssignValue(Values[i])
    else if Buffer = FRangeStartBuffer then
      IndexField.UseForRangeStart := False
    else if Buffer = FRangeEndBuffer then
      IndexField.UseForRangeEnd := False;
  end;
end;

function TMemDataSet.GetKeyExclusive: Boolean;
begin
  CheckSetKeyMode;
  Result := PRecInfo(PtrOffset(FRangeCurrentBuffer, FRecInfoOfs)).KeyExclusive;
end;

procedure TMemDataSet.SetKeyExclusive(Value: Boolean);
begin
  CheckSetKeyMode;
  PRecInfo(PtrOffset(FRangeCurrentBuffer, FRecInfoOfs)).KeyExclusive := Value;
  SetModified(True);
end;

procedure TMemDataSet.ResetRange;
begin
  ClearRangeBuffer(FRangeStartBuffer);
  ClearRangeBuffer(FRangeEndBuffer);
  Data.FilterRangeFunc := nil;
  FRanged := False;
end;

{ Constraints }
procedure TMemDataSet.CreateCheckConstraints;
var
  Constraint: TConstraint;
  i: integer;
begin
  Data.Constraints.Clear;
  if Constraints.Count > 0 then
    for i := 0 to Constraints.Count - 1 do begin
      if Constraints.Items[i].CustomConstraint = '' then
        Continue;

      Constraint := TConstraint.Create(Data, Constraints.Items[i].CustomConstraint, Constraints.Items[i].ErrorMessage, True);
      Data.Constraints.Add(Constraint);
    end;
end;

procedure TMemDataSet.CreateFieldConstrain;
var
  FieldConstraint: TFieldConstraint;
  Field: TField; FieldDesc: TFieldDesc;
  i: integer;
begin
  Data.FieldConstraints.Clear;
  for i := 0 to Fields.Count - 1 do begin
    Field := Fields[i];
    if (Field.FieldNo <= Data.Fields.Count) and (Field.FieldNo > 0) then begin
      FieldDesc := GetFieldDesc(Field);

      FieldConstraint := FieldDesc.CustomConstraint;

      if (FieldConstraint = nil) and (Field.CustomConstraint = '') then
        Continue;

      if Field.CustomConstraint <> '' then begin
        if FieldConstraint = nil then begin
          FieldConstraint := TFieldConstraint.Create(Data, Field.CustomConstraint, Field.ConstraintErrorMessage, FieldDesc, True);
          FieldDesc.CustomConstraint := FieldConstraint;
          Data.FieldConstraints.Add(FieldDesc.CustomConstraint);
        end
        else
          FieldConstraint.UpdateConstraint(Field.CustomConstraint, Field.ConstraintErrorMessage, FieldDesc);
      end
      else
        if FieldConstraint <> nil then
          FieldConstraint.EmptyConstraint;
    end;
  end;
end;

procedure TMemDataSet.InternalFirst;
begin
  Assert(Data <> nil);
  Data.SetToBegin;
end;

procedure TMemDataSet.InternalLast;
begin
  Assert(Data <> nil);
  Data.SetToEnd;
end;

{ Editing }

{$IFDEF VER17P}
procedure TMemDataSet.InternalAddRecord(Buffer: {$IFDEF VER18P}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}; Append: boolean);
begin
  InternalAddRecord(IntPtr(Buffer), Append);
end;
{$ENDIF}

procedure TMemDataSet.InternalAddRecord(Buffer: IntPtr; Append: boolean);
begin
  Assert(Data <> nil);
  if Append then
    Data.AppendRecord(Buffer)
  else
    Data.InsertRecord(Buffer);
end;

procedure TMemDataSet.InternalInsert;
begin
  Assert(Data <> nil);
  FreeDeferredBuf;
  FInInserting := True;
end;

procedure TMemDataSet.InternalEdit;
begin
  Assert(Data <> nil);
  FreeDeferredBuf;
  FInEditing := True;

  FreeRefComplexFields(IntPtr(ActiveBuffer));
  Data.EditRecord(IntPtr(ActiveBuffer));
end;

procedure TMemDataSet.InternalDelete;
begin
  Assert(Data <> nil);
  if not CanModify then DatabaseError(SDataSetReadOnly, Self);
  FreeRefComplexFields(IntPtr(ActiveBuffer));
  Data.DeleteRecord;

  // CR M8107
  FInInserting := False;
  FInEditing := False;
end;

procedure TMemDataSet.InternalPost;
var
  i: integer;
  Blob: TBlob;
  Field: TField;
  FieldDesc: TFieldDesc;
begin
  // if Cancel was called in the BeforePost event
  if (State = dsBrowse) or (not FInInserting and not FInEditing) then
    exit;

  inherited;

  Assert(Data <> nil);

  Data.CheckConstraints(IntPtr(ActiveBuffer), Data.Constraints);
  Data.CheckConstraints(IntPtr(ActiveBuffer), Data.FieldConstraints);

  for i := 0 to FieldCount - 1 do begin
    Field := Fields[i];
    FieldDesc := GetFieldDesc(Field);
    if (FieldDesc <> nil) and FieldDesc.IsBlob then begin
      Blob := Data.GetBlob(FieldDesc, IntPtr(ActiveBuffer));
      if Blob.CanRollback and (Field is TBlobField) then
        TBlobField(Field).Modified := True;
    end;
  end;
  if State = dsEdit then
    Data.PostRecord(IntPtr(ActiveBuffer))
  else
    Data.InsertRecord(IntPtr(ActiveBuffer));

  FreeDeferredBuf;
  FInInserting := False;
  FInEditing := False;
end;

procedure TMemDataSet.Post;
begin
{$IFDEF FPC}
  if State <> dsSetKey then // Lazarus bug
{$ENDIF}
    inherited;

  if State = dsSetKey then begin
    DataEvent(deCheckBrowseMode, 0);
    SetState(dsBrowse);
    DataEvent(deDataSetChange, 0);
  end;
end;

procedure TMemDataSet.Cancel;
var
  CancelBuf: IntPtr;
  OldIteration: integer;
begin
  if Data = nil then
    Exit;

  OldIteration := -1;
  if (State in [dsEdit, dsInsert]) and Data.HasComplexFields then begin
    CancelBuf := AllocRecordBuffer;
    CopyBuffer(IntPtr(ActiveBuffer), CancelBuf, FRecBufSize);
    Data.AddRefComplexFields(CancelBuf);
    if Data is TMemData then
      OldIteration := TMemData(Data).GetRefreshIteration;
  end
  else
    CancelBuf := nil;

  try
    inherited;
  finally
    if CancelBuf <> nil then begin
      if not (Data is TMemData) or (TMemData(Data).GetRefreshIteration = OldIteration) then
        Data.FreeComplexFields(CancelBuf, True);
      Marshal.FreeHGlobal(CancelBuf);
    end;
  end;
end;

procedure TMemDataSet.InternalCancel;
begin
  Assert(Data <> nil);
  FreeDeferredBuf;
  FInInserting := False;
  FInEditing := False;

  if State = dsEdit then
    Data.CancelRecord(IntPtr(ActiveBuffer));
  if State = dsInsert then
    if Data.HasComplexFields then
      Data.FreeComplexFields(IntPtr(ActiveBuffer), True);
end;

procedure TMemDataSet.InternalDeferredPost;
begin
  if State = dsEdit then
    DoPerformUpdate
  else
    DoPerformAppend;
end;

procedure TMemDataSet.DeferredPost;
  procedure CheckRequiredFields;
  var
    I: Integer;
  begin
    for I := 0 to Fields.Count - 1 do
      with Fields[I] do
        if Required and not ReadOnly and (FieldKind = fkData) and IsNull then begin
          FocusControl;
          DatabaseErrorFmt(SFieldRequired, [DisplayName]);
        end;
  end;
begin
  Assert(Data <> nil);
  if not CachedUpdates then begin
    UpdateRecord;
    case State of
      dsEdit, dsInsert: begin
        DataEvent(deCheckBrowseMode, 0);
        CheckRequiredFields;
        UpdateCursorPos;

        InternalDeferredPost;

        if FOldDeferredPostBuf = nil then
          FOldDeferredPostBuf := AllocRecordBuffer;

        Data.FreeComplexFields(FOldDeferredPostBuf, False);
        CopyBuffer(IntPtr(ActiveBuffer), FOldDeferredPostBuf, FRecBufSize);
        Data.CreateComplexFields(FOldDeferredPostBuf, False);  //copy complex fields
        Data.CopyComplexFields(IntPtr(ActiveBuffer), FOldDeferredPostBuf, False);

        FInDeferredPost := True;
      end;
    end;
  end;
end;

procedure TMemDataSet.FreeDeferredBuf;
begin
  FInDeferredPost := False;
  if FOldDeferredPostBuf <> nil then begin
    Data.FreeComplexFields(FOldDeferredPostBuf, False); //own complex fields
    Marshal.FreeHGlobal(FOldDeferredPostBuf);
    FOldDeferredPostBuf := nil;
  end;
end;

procedure TMemDataSet.SetDefaultExpressionValues;
var
  i: integer;
  Val: string;
  Field: TField;
begin
  FInSettingDefaultExpressionValues := True;
  try
    for i := 0 to FieldCount - 1 do begin
      Field := Fields[i];
      if Field.DefaultExpression <> '' then begin
        Val := Field.DefaultExpression;
        if not DefaultExpressionOldBehavior then
          Val := AnsiDequotedStr(Val, '''');
        Field.AsString := Val;
      end;
    end;
  finally
    FInSettingDefaultExpressionValues := False;
  end;
end;

procedure TMemDataSet.DoOnNewRecord;
var
  DataSet: TDataSet;
  MasterField, DetailField: TField;
  MasterName, DetailName: string;
  MasterPos, DetailPos: integer;

  procedure LinkMDFields(const MasterName, DetailName: string);
  begin
    MasterField := DataSet.FindField(MasterName);
    if Assigned(MasterField) then begin
      DetailField := FindField(DetailName);
      if Assigned(DetailField) and not DetailField.ReadOnly then begin // CR 11917
        if DetailField is TLargeintField then
          TLargeintField(DetailField).AsLargeInt := MasterField.Value
        else
          DetailField.Assign(MasterField);
      end;
    end;
  end;

begin
  SetDefaultExpressionValues;

  try
    inherited;
  except
    InternalCancel;
    raise;
  end;

  if (DataSource <> nil) then begin
    DataSet := DataSource.DataSet;
    if (DataSet <> nil) and DataSet.Active then begin
      //MD link by MasteFields and DetailFields
      if (FMasterFields <> '') and (FDetailFields <> '') then begin
          MasterPos := 1;
          DetailPos := 1;
          while True do begin
            MasterName := SplitFieldName(FMasterFields, MasterPos);
            DetailName := SplitFieldName(FDetailFields, DetailPos);
            if (MasterName <> '') and (DetailName <> '') then
              LinkMDFields(MasterName, DetailName)
            else
              break;
          end;
      end;
      //We couldn't link MD fields in case of undefined FMasterFields or FDetailFields
      //cause there is could be field names mismatch
    end;
  end;
end;

procedure TMemDataSet.DoPerformAppend;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoPerformAppend;
end;

procedure TMemDataSet.DoPerformDelete;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoPerformDelete;
end;

procedure TMemDataSet.DoPerformUpdate;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoPerformUpdate;
end;

procedure TMemDataSet.DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoApplyRecord(UpdateKind, Action, LastItem);
end;

procedure TMemDataSet.DoCacheChanged;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoCacheChanged;
end;

procedure TMemDataSet.DoCacheApplied;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoCacheApplied;
end;

procedure TMemDataSet.DoCacheCanceled;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoCacheCanceled;
end;

procedure TMemDataSet.DoAfterApplyUpdates;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoAfterApplyUpdates;
end;

{ Filter / Locate / Find }
procedure TMemDataSet.DoGetCachedFields;
var
  i: Integer;
  Field: TField;
  FieldDesc: TFieldDesc;
  CalcFieldCount: integer;
begin
  if Data = nil then
    exit;

  CalcFieldCount := 0;
{$IFDEF VER20P}
  if lcPersistent in Fields.LifeCycles then begin
{$ELSE}
  if not DefaultFields then begin
{$ENDIF}
    for i := 0 to Fields.Count - 1 do begin
      Field := Fields[i];
      case Field.FieldKind of
        fkCalculated, fkLookup: begin
          FieldDesc := Data.CreateFieldDesc;
          try
            FieldDesc.ActualName := Field.{$IFNDEF FPC}FullName{$ELSE}FieldName{$ENDIF};
            FieldDesc.Name := Field.{$IFNDEF FPC}FullName{$ELSE}FieldName{$ENDIF};
            FieldDesc.FieldNo := Data.Fields.Count + 1;
            FieldDesc.DataType := GetFieldTypeMapClass.GetDataType(Field.DataType);
            FieldDesc.Size := Data.GetBufferSize(FieldDesc.DataType, Field.Size);
            FieldDesc.Length := Field.Size;
            if FCacheCalcFields then
              FieldDesc.FieldDescKind := fdkCached
            else
              FieldDesc.FieldDescKind := fdkCalculated;

            // FieldDescs with CachedField=True must be positioned after data FieldDescs
            Data.Fields.Add(FieldDesc);
            Inc(CalcFieldCount);
            SetLength(FCalcFieldsMapping, CalcFieldCount);
            FCalcFieldsMapping[CalcFieldCount - 1].Field := Field;
            FCalcFieldsMapping[CalcFieldCount - 1].FieldDesc := FieldDesc;
          except
            FieldDesc.Free;
            raise;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMemDataSet.DoGetCachedBuffer(Buffer: IntPtr; Source: IntPtr = nil);
var
  RecBuf: IntPtr;
begin
  if (CalcFieldsSize > 0) then begin
    Assert(Data <> nil);
    RecBuf := Marshal.AllocHGlobal(Data.RecordSize + Data.CalcRecordSize + SizeOf(TRecInfo) + BookmarkSize);
    try
      if Source = nil then
        CopyBuffer(Buffer, RecBuf, Data.RecordSize)
      else
        CopyBuffer(Source, RecBuf, Data.RecordSize);

      GetCalcFields({$IFDEF VER18P}NativeInt{$ENDIF}(RecBuf));
      CopyBuffer(RecBuf, Buffer, Data.RecordSize + Data.CalcRecordSize);
    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
  end;
end;

procedure TMemDataSet.ActivateFilters;
begin
  if Data = nil then
    Exit;

  DeactivateFilters;

  if Assigned(OnFilterRecord) then
    Data.FilterFunc := RecordFilter;
  if Trim(Filter) <> '' then
    Data.FilterText := Filter;

  Data.FilterCaseInsensitive := foCaseInsensitive in FilterOptions;
  Data.FilterNoPartialCompare := foNoPartialCompare in FilterOptions;
end;

procedure TMemDataSet.DeactivateFilters;
begin
  if Data = nil then
    Exit;
  Data.FilterFunc := nil;
  Data.FilterText := '';
end;

function TMemDataSet.RecordFilter(RecBuf: IntPtr): boolean;
var
  Accept: boolean;
  SaveState: TDataSetState;
begin
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := RecBuf;
  try
    Accept := True;
    OnFilterRecord(Self, Accept);
  except
    InternalHandleException;
  end;
  RestoreState(SaveState);
  Result := Accept;
end;

function TMemDataSet.RecordFilterRange(RecBuf: IntPtr): boolean;
var
  IndexField: TSortColumn;
  StartKeyExclusive, EndKeyExclusive: boolean;
  CompareResult1, CompareResult2: integer;
  i: integer;
begin
  Result := True;
  StartKeyExclusive := PRecInfo(PtrOffset(FRangeStartBuffer, FRecInfoOfs)).KeyExclusive;
  EndKeyExclusive := PRecInfo(PtrOffset(FRangeEndBuffer, FRecInfoOfs)).KeyExclusive;

  CompareResult1 := 0;
  CompareResult2 := 0;
  for i := 0 to TMemData(Data).IndexFieldCount - 1 do begin
    IndexField := TMemData(Data).IndexFields[i];
    if IndexField.UseForRangeStart or IndexField.UseForRangeEnd then begin
      if CompareResult1 = 0 then begin
        CompareResult1 := Data.CompareFields(FRangeStartBuffer, RecBuf, IndexField.FieldDesc, [], True);
        if (CompareResult1 > 0) or ((CompareResult1 = 0) and StartKeyExclusive) then begin
          Result := False;
          Exit;
        end;
      end;

      if CompareResult2 = 0 then begin
        CompareResult2 := Data.CompareFields(FRangeEndBuffer, RecBuf, IndexField.FieldDesc, [], True);
        if (CompareResult2 < 0) or ((CompareResult2 = 0) and EndKeyExclusive) then begin
          Result := False;
          Exit;
        end;
      end;
      if (CompareResult1 < 0) and (CompareResult2 > 0) then
        Exit;
    end;
  end;
end;

procedure TMemDataSet.SetFiltered(Value: boolean);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  if Value <> Filtered then begin
    if Value then
      ActivateFilters
    else
      DeactivateFilters;

    inherited SetFiltered(Value);

    if Active then begin
      Assert(Data <> nil);
      Data.FilterUpdated;
      Resync([]);
      First;
      // DoAfterScroll;
    end;
  end;
end;

procedure TMemDataSet.SetFilterData(const Text: string; Options: TFilterOptions);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  if (Text <> Filter) or (Options <> FilterOptions) then begin
    if Data <> nil then begin
      Data.FilterCaseInsensitive := foCaseInsensitive in Options;
      Data.FilterNoPartialCompare := foNoPartialCompare in Options;

      if Filtered and (Trim(Text) <> '') then
        Data.FilterText := Text
      else
        Data.FilterText := '';
    end;

    inherited SetFilterText(Text);
    inherited SetFilterOptions(Options);

    if Active and Filtered then begin
      Data.FilterUpdated;
      Resync([]);
      First;
      // DoAfterScroll;
    end;
  end;
end;

procedure TMemDataSet.SetFilterOptions(Value: TFilterOptions);
begin
  SetFilterData(Filter, Value);
end;

procedure TMemDataSet.SetFilterText(const Value: string);
begin
  SetFilterData(Value, FilterOptions);
end;

procedure TMemDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  inherited SetOnFilterRecord(Value);

  if Data = nil then
    Exit;

  if Filtered and Assigned(OnFilterRecord) then
    Data.FilterFunc := RecordFilter
  else
    Data.FilterFunc := nil;

  if Active then begin
    Data.FilterUpdated;
    Resync([]);
    First;
  end;
end;

function TMemDataSet.FindRecord(Restart, GoForward: boolean): boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  SetFound(False);
  UpdateCursorPos;
  CursorPosChanged;
  Assert(Data <> nil);

  if not Filtered then
    ActivateFilters;
  try
    Data.StartSearch;
    try
      if GoForward then begin
        if Restart then
          Data.SetToBegin;
        Data.GetNextRecord(nil);
      end
      else begin
        if Restart then
          Data.SetToEnd;
        Data.GetPriorRecord(nil);
      end;
    finally
      Data.EndSearch;
    end;
  finally
    if not Filtered then
      DeactivateFilters;
  end;

  if not Data.BOF and not Data.EOF then begin
    Resync([rmExact, rmCenter]);
    SetFound(True);
  end;
  Result := Found;
  if Result then
    DoAfterScroll;
end;

{$IFDEF FPC}
function TMemDataSet.FindFirst: boolean;
begin
  Result := FindRecord(True, True);
end;

function TMemDataSet.FindLast: boolean;
begin
  Result := FindRecord(True, False);
end;

function TMemDataSet.FindNext: boolean;
begin
  Result := FindRecord(False, True);
end;

function TMemDataSet.FindPrior: boolean;
begin
  Result := FindRecord(False, False);
end;
{$ENDIF}

{ Master/Detail }

{$IFDEF VER17P}
function TMemDataSet.AddFieldToList(const FieldName: string; DataSet: TDataSet; List: TList<TField>): boolean;
{$ELSE}
function TMemDataSet.AddFieldToList(const FieldName: string; DataSet: TDataSet; List: TList): boolean;
{$ENDIF}
var
  Field: TField;
begin
  Field := DataSet.FindField(FieldName);
  if (Field <> nil) then
    List.Add(Field);
  Result := Field <> nil;
end;

{$IFDEF VER17P}
procedure TMemDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList<TField>);
{$ELSE}
procedure TMemDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList);
{$ENDIF}
var
  DataSet: TDataSet;
  MasterName, DetailName: string;
  MasterPos, DetailPos: integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then begin
    if (Self.MasterFields <> '') and (Self.DetailFields <> '') then begin
      DataSet := DataSource.DataSet;
      if (DataSet <> nil) and DataSet.Active then begin
        MasterPos := 1;
        DetailPos := 1;
        while True do begin
          MasterName := ExtractFieldName(FMasterFields, MasterPos);
          DetailName := ExtractFieldName(FDetailFields, DetailPos);
          if (MasterName = '') or (DetailName = '') then
            Break;
          if AddFieldToList(MasterName, DataSource.DataSet, MasterFields) then
            AddFieldToList(DetailName, Self, DetailFields);
        end;
      end;
    end;
  end;
end;

function TMemDataSet.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TMemDataSet.IsMasterDatasetActive: boolean;
begin
  Result := (FDataLink.DataSource <> nil) and (FDataLink.DataSource.DataSet <> nil)
    and FDataLink.DataSource.DataSet.Active;
end;

function TMemDataSet.IsConnectedToMaster: boolean;
begin
  Result := (MasterSource <> nil) and (FMasterFields <> '') and (FDetailFields <> '');
end;

function TMemDataSet.SetLocalMDLinks(Field: TField): boolean;

  procedure SetBuffer(LocalMDLink: PLocalMDLink; CurrNativeBuffer: boolean; CurrDataBuf: IntPtr; CurrDataLen: Word; CurrBufferType: Word; CurrIsNull: boolean; Value: Variant);
  begin
    LocalMDLink.NativeBuffer := CurrNativeBuffer;
    if not LocalMDLink.NativeBuffer then
      Marshal.FreeHGlobal(LocalMDLink.DataBuf);
    LocalMDLink.DataBuf := CurrDataBuf;
    LocalMDLink.DataLen := CurrDataLen;
    LocalMDLink.BufferType := CurrBufferType;
    LocalMDLink.IsNull := CurrIsNull;
    LocalMDLink.MasterFieldValue := Value;
  end;

var
  MemDataSet: TMemDataSet;
  RecBuf: TRecordBuffer;

  MasterField: TField;
  MasterFieldDesc: TFieldDesc;
  DetailField: TField;
  DetailFieldDesc: TFieldDesc;
  MasterPos: integer;
  DetailPos: integer;
  MasterName: string;
  DetailName: string;
  LinksCount: integer;
  CurrDatBuf: IntPtr;
  CurrDataSize: Word;
  EmptyMDLinks, CurrIsNull, CurrNativeBuffer: boolean;
  LocalMDLink: PLocalMDLink;
  CurrBufferType: Word;
begin
  Result := False;

  if IsConnectedToMaster and IsMasterDatasetActive then begin
    if not (DataSource.DataSet is TMemDataSet) then
      raise Exception.Create(SMDIsNotMemDataSet);

    MemDataSet := TMemDataSet(DataSource.DataSet);
    LinksCount := 0;
    MasterPos := 1;
    DetailPos := 1;
    EmptyMDLinks := Length(FLocalMDLinks) = 0;
    while True do begin
      MasterName := ExtractFieldName(FMasterFields, MasterPos);
      DetailName := ExtractFieldName(FDetailFields, DetailPos);
      if (MasterName <> '') and (DetailName <> '') then begin
        MasterField := MemDataSet.FindField(MasterName);
        if Assigned(MasterField) then begin
          DetailField := FindField(DetailName);
          if Assigned(DetailField) then begin
            if EmptyMDLinks then
              SetLength(FLocalMDLinks, LinksCount + 1);
            LocalMDLink := @FLocalMDLinks[LinksCount];
            DetailFieldDesc := Data.FindField(DetailField.FieldName);
            MasterFieldDesc := MemDataSet.GetFieldDesc(MasterField);
            if DetailFieldDesc = nil then
              raise Exception.Create(Format(SFieldNotFound, [DetailField.FieldName]));
            if MasterFieldDesc = nil then
              raise Exception.Create(Format(SFieldNotFound, [MasterField.FieldName])); //TODO: Field : cannot be used for local master/detail link
            LocalMDLink.FieldNo := DetailFieldDesc.FieldNo;
            LocalMDLink.IsNull := MasterField.IsNull;
            if not LocalMDLink.IsNull then begin
              if MemDataSet.Data.IsEqualDataType(DetailFieldDesc, MasterFieldDesc) then begin
                if MemDataSet.GetActiveRecBuf(RecBuf) then begin
                  CurrBufferType := MasterFieldDesc.DataType;
                  CurrDatBuf := MemDataSet.Data.GetFieldBuf(RecBuf, MasterFieldDesc, CurrDataSize, CurrIsNull, CurrNativeBuffer);
                  if (LocalMDLink.DataBuf = nil) or Data.GetNull(MasterFieldDesc, RecBuf) or
                    not (Data.CompareFieldValue(LocalMDLink.DataBuf, LocalMDLink.DataLen, LocalMDLink.BufferType, MasterFieldDesc, RecBuf, [], True) = 0) or
                    (LocalMDLink.MasterFieldValue <> MasterField.Value)
                  then begin
                    SetBuffer(LocalMDLink, CurrNativeBuffer, CurrDatBuf, CurrDataSize, CurrBufferType, CurrIsNull, MasterField.Value);
                    Result := True;
                  end
                  else
                    SetBuffer(LocalMDLink, CurrNativeBuffer, LocalMDLink.DataBuf, LocalMDLink.DataLen, CurrBufferType, CurrIsNull, MasterField.Value);
                end;
              end
              else begin
                CopyFieldValue(MasterField.Value, LocalMDLink.DataBuf, LocalMDLink.DataLen, LocalMDLink.BufferType, DetailFieldDesc.MapDataType);
                Result := True;
              end;
              if Field = MasterField then
                Result := True;
            end
            else begin
              SetBuffer(LocalMDLink, LocalMDLink.NativeBuffer, nil, 0, LocalMDLink.BufferType, LocalMDLink.IsNull, MasterField.Value);
              Result := True;
            end;
            Inc(LinksCount);
          end;
        end;
      end
      else
        break;
    end;
  end;
end;

function TMemDataSet.MDLinksRefreshed(Field: TField): boolean;
begin
  Result := SetLocalMDLinks(Field);
end;

procedure TMemDataSet.MasterRecordChanged(Field: TField);
var
  DataSet: TDataSet;
begin
  FreeRefBuffers;

  if FDataLink.DataSource <> nil then begin
    DataSet := FDataLink.DataSource.DataSet;
    if DataSet <> nil then
      if DataSet.Active and (DataSet.State <> dsSetKey) then begin
        if MDLinksRefreshed(Field) then begin // need refresh
        {$IFDEF MSWINDOWS}
          if (FDetailRefreshTimer <> nil) and (FDetailRefreshTimer.Interval <> 0) then begin
            FDetailRefreshTimer.Enabled := False; //reset time period
            FDetailRefreshTimer.Enabled := True;
          end
          else
        {$ENDIF}
            RefreshDetail(nil);
        end;
      end;
  end;
end;

function TMemDataSet.UseLocalMasterDetailFilter: boolean;
begin
  Result := IsConnectedToMaster;
end;

procedure TMemDataSet.RefreshDetail(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if FDetailRefreshTimer <> nil then
    FDetailRefreshTimer.Enabled := False;
{$ENDIF}
  if not Active then
    Exit;

  if UseLocalMasterDetailFilter then begin
    Data.FilterUpdated;
    Resync([]);
    First;
  end;
end;

function TMemDataSet.LocalDetailFilter(RecBuf: IntPtr): boolean;
var
  i: integer;
  DataSet: TDataSet;
  FieldDesc: TFieldDesc;
begin
  Result := True;

  if FDataLink.DataSource <> nil then begin
    DataSet := FDataLink.DataSource.DataSet;
    if (DataSet <> nil) and DataSet.Active then begin
      // RefreshParamsOnInsert - returns old behavior
      Result := RefreshParamsOnInsert or (DataSet.RecordCount <> 0);
      if not Result then
        Exit;
    end;
  end;

  for i := 0 to Length(FLocalMDLinks) - 1 do
    with FLocalMDLinks[i] do begin
      if not Result then
        Break;
      FieldDesc := GetFieldDesc(FieldNo);
      if IsNull or Data.GetNull(FieldDesc, RecBuf) then
        Result := IsNull and Data.GetNull(FieldDesc, RecBuf)
      else
        Result := Data.CompareFieldValue(DataBuf, DataLen, BufferType, FieldDesc, RecBuf, [], False) = 0;
    end;
end;

procedure TMemDataSet.SetLocalDetailFilter;
begin
  if Data <> nil then
    if UseLocalMasterDetailFilter then
      Data.FilterMDFunc := LocalDetailFilter
    else
      Data.FilterMDFunc := nil;
end;

{$IFDEF MSWINDOWS}
procedure TMemDataSet.CheckRefreshDetailTimer;
begin
  if FDetailRefreshTimer = nil then begin
    FDetailRefreshTimer := TCRTimer.Create(Self);
    FDetailRefreshTimer.Enabled := False;
    FDetailRefreshTimer.OnTimer := RefreshDetail;
  end;
end;
{$ENDIF}

procedure TMemDataSet.SetDetailDelay(Value: integer);
begin
  if FDetailDelay <> Value then begin
    FDetailDelay := Value;
  {$IFDEF MSWINDOWS}
    CheckRefreshDetailTimer;
    FDetailRefreshTimer.Interval := Value;
  {$ENDIF}
  end;
end;

procedure TMemDataSet.MDPropertiesChanged;
begin
  if Active then begin
    MDLinksRefreshed(nil);
    Data.FilterUpdated;
    Resync([]);
  end;
end;

function TMemDataSet.SplitFieldName(const Fields: string; var Pos: Integer): string;
begin
  Result := ExtractFieldName(Fields, Pos);
end;

procedure TMemDataSet.SetMasterSource(Value: TDataSource);
var
  NeedRefresh: boolean;
begin
  if FDataLink.DataSource <> Value then begin
    if IsLinkedTo(Value) then
      DatabaseError(SCircularDataLink);
    NeedRefresh := IsConnectedToMaster;
    FDataLink.DataSource := Value;
    NeedRefresh := NeedRefresh or IsConnectedToMaster;
    SetLocalDetailFilter;

    if NeedRefresh then
      MDPropertiesChanged;
  end;
end;

procedure TMemDataSet.SetMasterFields(const Value: string);
var
  NeedRefresh: boolean;
begin
  if Value <> FMasterFields then begin
    NeedRefresh := IsConnectedToMaster;
    FMasterFields := Value;
    NeedRefresh := NeedRefresh or IsConnectedToMaster;
    SetLocalDetailFilter;

    if NeedRefresh then
      MDPropertiesChanged;
  end;
end;

procedure TMemDataSet.SetDetailFields(const Value: string);
var
  NeedRefresh: boolean;
begin
  if Value <> FDetailFields then begin
    NeedRefresh := IsConnectedToMaster;
    FDetailFields := Value;
    NeedRefresh := NeedRefresh or IsConnectedToMaster;
    SetLocalDetailFilter;

    if NeedRefresh then
      MDPropertiesChanged;
  end;
end;

// Allocate memory for Value (ValuePtr - must call FreeMem!) and copy Value to ValuePtr
procedure TMemDataSet.CopyFieldValue(const Value: variant;
  out ValuePtr: IntPtr; out ValueLen: Word; out ValueType: Word; FieldType: Word; UseFieldType: boolean = True);
var
  sa: AnsiString;
  ws: WideString;
  b: boolean;
  lw: Cardinal;
  i64: Int64;
  ASingle: single;
  c: currency;
  ui64: UInt64;
  bcd: TBcd;
  ValueSize: Integer;
  SafeArray: PVarArray;
begin
  ValueLen := 0;
  ValueType := FieldType;

  if VarType(Value) in [varEmpty, varNull] then begin
    ValuePtr := nil;
    Exit;
  end;

  if VarIsStr(Value) then begin
    if UseFieldType then begin
      case FieldType of
        dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32: begin
          CopyFieldValue(StrToInt(Value), ValuePtr, ValueLen, ValueType, FieldType);
          Exit;
        end;
        dtDate: begin
          CopyFieldValue(StrToDate(Value), ValuePtr, ValueLen, ValueType, FieldType);
          Exit;
        end;
        dtTime: begin
          CopyFieldValue(StrToTime(Value), ValuePtr, ValueLen, ValueType, FieldType);
          Exit;
        end;
        dtDateTime: begin
          CopyFieldValue(StrToDateTime(Value), ValuePtr, ValueLen, ValueType, FieldType);
          Exit;
        end;
      {$IFNDEF FPC}
        dtSQLTimeStamp: begin
          CopyFieldValue(VarSQLTimeStampCreate(StrToSQLTimeStamp(Value)), ValuePtr, ValueLen, ValueType, FieldType);
          Exit;
        end;
      {$ENDIF}
        dtFloat, dtBCD, dtFMTBCD, dtCurrency: begin
          CopyFieldValue(StrToFloat(Value), ValuePtr, ValueLen, ValueType, FieldType);
          Exit;
        end;
        dtSingle: begin
          ASingle := StrToFloat(Value);
          CopyFieldValue(ASingle, ValuePtr, ValueLen, ValueType, FieldType);
          Exit;
        end;
        dtInt64, dtUInt64, dtUInt32: begin
          CopyFieldValue(StrToInt64(Value), ValuePtr, ValueLen, ValueType, FieldType);
          Exit;
        end;
      end;
    end
    else
      if not (FieldType in [dtBoolean, dtBytes, dtVarBytes, dtExtVarBytes, dtBlob,
        {$IFDEF IS_UNICODE}dtString, dtExtString, dtMemo, dtGuid{$ELSE}dtWideString, dtExtWideString, dtWideMemo{$ENDIF}]) then
        ValueType := {$IFDEF IS_UNICODE}dtWideString{$ELSE}dtString{$ENDIF};
  end;

  case ValueType of
    dtBoolean: begin
      b := Value;
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Byte));
      Byte(ValuePtr^) := Byte(b);
    end;
    dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Integer));
      Integer(ValuePtr^) := Value;
    end;
    dtUInt32: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Cardinal));
      lw := Value;
      Cardinal(ValuePtr^) := lw;
    end;
    dtInt64: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Int64));
      i64 := Value;
      Int64(ValuePtr^) := i64;
    end;
    dtUInt64: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(UInt64));
      ui64 := Value;
      UInt64(ValuePtr^) := ui64;
    end;
    dtFloat, dtCurrency: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Double));
      Double(ValuePtr^) := Value;
    end;
    dtSingle: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Single));
      Single(ValuePtr^) := Value;
    end;
    dtExtended: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Extended));
      Extended(ValuePtr^) := Value;
    end;
    dtBCD: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Currency));
      c := Value;
      Currency(ValuePtr^) := c;
    end;
    dtFmtBCD: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOfTBcd);
      bcd := VarToBcd(Value);
      TBcd(ValuePtr^) := bcd;
    end;
    dtString, dtExtString: begin
      sa := AnsiString(Value);
      ValueLen := LengthA(sa);
      if Data.RequireEmptyStrToNull and (ValueLen = 0) then
        ValuePtr := nil
      else begin
        ValueSize := ValueLen + 1;
        ValuePtr := Marshal.AllocHGlobal(ValueSize);
        CopyBuffer(PAnsiChar(sa), ValuePtr, ValueSize);
        ValueType := dtString;
      end;
    end;
    dtBlob, dtMemo, dtGuid, dtVariant: begin
      sa := AnsiString(Value);
      ValueSize := LengthA(sa) + 1;
      ValuePtr := Marshal.AllocHGlobal(ValueSize);
      CopyBuffer(PAnsiChar(sa), ValuePtr, ValueSize);
      ValueType := dtString;
    end;
    dtWideString, dtExtWideString: begin
      ws := Value;
      ValueLen := Length(ws);
      if Data.RequireEmptyStrToNull and (ValueLen = 0) then
        ValuePtr := nil
      else begin
        ValueSize := (ValueLen + 1) * SizeOf(WideChar);
        ValuePtr := Marshal.AllocHGlobal(ValueSize);
        CopyBuffer(PWideChar(ws), ValuePtr, ValueSize);
        ValueType := dtWideString;
      end;
    end;
    dtWideMemo: begin
      ws := Value;
      ValueSize := Length(ws) * SizeOf(WideChar) + SizeOf(WideChar);
      ValuePtr := Marshal.AllocHGlobal(ValueSize);
      CopyBuffer(PWideChar(ws), ValuePtr, ValueSize);
      ValueType := dtWideString;
    end;
    dtDateTime, dtDate, dtTime: begin
      ValueType := dtFloat;
      ValuePtr := Marshal.AllocHGlobal(SizeOf(Int64));
      Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(TDateTime(Value)));
    end;
  {$IFNDEF FPC}
    dtSQLTimeStamp: begin
      ValuePtr := Marshal.AllocHGlobal(SizeOf(TSQLTimeStamp));
      TSQLTimeStamp(ValuePtr^) := VarToSQLTimeStamp(Value);
    end;
  {$ENDIF}
    dtBytes, dtVarBytes, dtExtVarBytes: begin
      ValueType := dtBytes;

      if VarIsStr(Value) then begin
        sa := AnsiString(Value);
        ValueLen := LengthA(sa);
        ValuePtr := Marshal.AllocHGlobal(ValueLen);
        CopyBuffer(PAnsiChar(sa), ValuePtr, ValueLen);
      end
      else begin
        Assert(VarType(Value) = varArray + varByte);
        SafeArray := VarArrayAsPSafeArray(Value);
        ValueLen := SafeArray.Bounds[0].ElementCount;
        ValuePtr := Marshal.AllocHGlobal(ValueLen);
        if ValueLen > 0 then
          Move(SafeArray.Data^, ValuePtr^, ValueLen);
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvertType);
  end;
end;

procedure TMemDataSet.DoOnDataChanged;
begin
  if not FDisableResync then begin
    FDataWasChanged := False;
    UpdateCursorPos;
    CursorPosChanged;
    Resync([]);
  end
  else
    FDataWasChanged := True;
end;

function TMemDataSet.LocateRecord(KeyFields: TList; KeyValues: variant;
  Options: TLocateExOptions; SavePos: boolean): boolean;
type
  TFiledValue = record
    DataPtr: IntPtr;
    DataLen: Word;
    DataType: Word;
  end;
var
  i: Integer;
  FieldCount: integer;
  FieldDesc: TFieldDesc;
  RecBuf: TRecordBuffer;
  Values: array of TFiledValue;
  ValuePtr: IntPtr;
  IndexedFieldCount: integer;
  Bookmark: PRecBookmark;
  Res: integer;
  CalcKeyFields: boolean;
  FirstRecNo, LastRecNo, RecCount: Integer;
  PartialSearchFirstEntry: Boolean;
  CompareOptions: TCompareOptions;
  CanUseGoldenSection: Boolean;
  FieldBlank: boolean;

  procedure SetKeyFields;
  var
    i, j: integer;
    Index: integer;
    Value: variant;
  begin
    CalcKeyFields := False;
    FieldCount := KeyFields.Count;
    SetLength(Values, FieldCount);
    for i := 0 to FieldCount - 1 do begin
      Values[i].DataPtr := nil; // Clear Values array to prevent AV in 'finally' section after Exception

      if TFieldDesc(KeyFields[i]).FieldDescKind = fdkCalculated then
        CalcKeyFields := True;
    end;

    IndexedFieldCount := 0;
    //lxPartialKey and lxPartialCompare incompatible with ordered locate
    //lxNearest can be partially supported in the next versions
    if  Data is TMemData then
      if not ((lxPartialKey in Options) or (lxPartialCompare in Options) or (lxNearest in Options)) then begin
        for i := 0 to TMemData(Data).IndexFieldCount - 1 do begin
          //Check ordered locate posibility for current Index field
          if (TMemData(Data).IndexFields[i].SortType = stCaseInsensitive) xor (lxCaseInsensitive in Options) then
            Break;

          //First IndexFields should be in KeyFields if not then we can't use ordered Locate
          Index := KeyFields.IndexOf(TMemData(Data).IndexFields[i].FieldDesc);
          if (Index > -1) then begin
            if IndexedFieldCount <> Index then begin
              FieldDesc := TFieldDesc(KeyFields[IndexedFieldCount]);
              KeyFields[IndexedFieldCount] := KeyFields[Index];
              KeyFields[Index] := FieldDesc;

              if not VarIsArray(KeyValues) then begin
                Value := KeyValues;
                KeyValues := VarArrayCreate([0, FieldCount - 1], varVariant);
                KeyValues[i] := Value;
                for j := 1 to FieldCount - 1 do
                  KeyValues[j] := Unassigned;
              end;

              Value := KeyValues[IndexedFieldCount];
              KeyValues[IndexedFieldCount] := KeyValues[Index];
              KeyValues[Index] := Value;
            end;
            Inc(IndexedFieldCount);
          end
          else
            break;
        end;
      end;

    for i := 0 to FieldCount - 1 do begin
      Value := Unassigned;
      if VarIsArray(KeyValues) and ((FieldCount > 1) or (VarArrayHighBound(KeyValues, 1) = 0)) then
        if i <= VarArrayHighBound(KeyValues, 1) then
          Value := KeyValues[i]
        else
          Value := Null
      else
        if i = 0 then
          Value := KeyValues
        else
          Value := Null;
      // string values should be converted to field native types only when they are used
      // in the golden section search (for compatibility)
      CopyFieldValue(Value, Values[i].DataPtr, Values[i].DataLen, Values[i].DataType, TFieldDesc(KeyFields[i]).MapDataType, i < IndexedFieldCount);
    end;
  end;

  function GetGoldenSectionDir(ReadRecBuf: boolean): integer;
  var
    i: integer;
    Dir: Integer;
    ValuePtr: IntPtr;
    Options: TCompareOptions;
  begin
    Result := 0;
    i := 0;
    if ReadRecBuf then
      Data.GetRecord(RecBuf);

    if CalcKeyFields then
      GetCalcFields({$IFDEF VER18P}NativeInt{$ENDIF}(RecBuf));

    while (Result = 0) and (i < IndexedFieldCount) do begin
      FieldDesc := TFieldDesc(KeyFields[i]);

      FieldBlank := Data.GetNull(FieldDesc, RecBuf);
      if Data is TMemData then
        Options := Data.GetSortOptions(TMemData(Data).IndexFields[i])
      else
        Options := [];

      if (Data is TMemData) and TMemData(Data).IndexFields[i].DescendingOrder then
        Dir := -1
      else
        Dir := 1;

      ValuePtr := Values[i].DataPtr;
      if FieldBlank and (ValuePtr = nil) then
        Result := 0
      else if FieldBlank and (ValuePtr <> nil) then
        if coInvertNullOrder in Options then
          Result := -1
        else
          Result := 1
      else
      if not FieldBlank and (ValuePtr = nil) then
        if coInvertNullOrder in Options then
          Result := 1
        else
          Result := -1
      else begin
        if (Data is TMemData) and (TMemData(Data).IndexFields[i].SortType = stBinary) then
          Include(CompareOptions, coOrdinalCompare)
        else
          Exclude(CompareOptions, coOrdinalCompare);
        Result := Data.CompareFieldValue(ValuePtr, Values[i].DataLen, Values[i].DataType, FieldDesc, RecBuf, CompareOptions, True);
      end;

      if Result <> 0 then
        Result := Result * Dir;
      Inc(i);
    end;
  end;

  function ExecGoldenSection: boolean;
  var
    First, Last, Current: Integer;
    Dir: Integer;

  begin
    Result := False;

    if FirstRecNo > LastRecNo then Exit; // cr - 23687

    First := FirstRecNo;
    Last  := LastRecNo;

    Data.RecordNo := First;
    Dir := GetGoldenSectionDir(True);
    if Dir <= 0 then begin
      Result := Dir = 0;
      Exit;
    end;

    Data.RecordNo := Last;
    Dir := GetGoldenSectionDir(True);
    if Dir >= 0 then begin
      Result := Dir = 0;
      Exit;
    end;

    repeat
      Current := (Last + First) shr 1;
      Data.RecordNo := Current;
      Dir := GetGoldenSectionDir(True);
      if Dir < 0 then
        Last := Current
      else
      if Dir > 0 then
        First := Current
      else begin
        Result := True;
        Break;
      end;
    until  Last - First <= 1;
  end;

begin
  Result := False;
  if KeyFields.Count = 0 then
    Exit;

  CheckBrowseMode;
  CursorPosChanged;
  UpdateCursorPos;

  CompareOptions := [];
  if lxCaseInsensitive in Options then
    Include(CompareOptions, coCaseInsensitive);
  if lxPartialKey in Options then
    Include(CompareOptions, coPartialKey);
  if lxPartialCompare in Options then
    Include(CompareOptions, coPartialCompare);

  Assert(Data <> nil);
  FieldCount := 0;
  Values := nil;

  RecBuf := TRecordBuffer(TempBuffer);
  FreeRefComplexFields(RecBuf);
  FDisableResync := True;

  Bookmark := Marshal.AllocHGlobal(sizeof(TRecBookmark));
  try
    CanUseGoldenSection := True;
    try
      SetKeyFields;
    except
      CanUseGoldenSection := False;
    end;

    Data.GetBookmark(Bookmark);

    //Set locate dimensions. This dimensions should be used for lxNearest ordered implementation
    FirstRecNo := 1;
    LastRecNo := Data.RecordCount;

    if (lxNext in Options) then begin//Search from current position
      if IndexedFieldCount > 0 then begin
        Data.GetNextRecord(RecBuf);  //Next RecNo in case of Ordered Search
        if Data.Eof then begin
          if not Data.Bof {Empty Data} then
            Data.SetToBookmark(Bookmark);
          exit;
        end;
      end;
      FirstRecNo := Data.RecordNo;
    end;

    if (lxUp in Options) then begin  //Search from current position downto first
      if IndexedFieldCount > 0 then
        Data.GetPriorRecord(RecBuf); //Prior RecNo in case of Ordered Search
      LastRecNo := Data.RecordNo;
    end;

    if (IndexedFieldCount > 0) and CanUseGoldenSection then begin
      if Data is TMemData then begin
        TMemData(Data).PrepareRecNoCache(RecCount);
        if RecCount < LastRecNo then
          LastRecNo := RecCount;
      end;
      Result := ExecGoldenSection;

      //Find the first occurence of located data
      if Result then
        repeat

          if (lxUp in Options) then begin
            if Data.RecordNo >= LastRecNo then //top limit of first occurrence search
              break
            else begin
              Data.GetNextRecord(RecBuf);
              if Data.Eof or (GetGoldenSectionDir(False) <> 0) then begin
                if IndexedFieldCount >= FieldCount then  //we shouldn't restore correct position in case of mixed locate
                  Data.GetPriorRecord(RecBuf);   //Restore correct position
                break;
              end;
            end;
          end
          else
            if Data.RecordNo <= FirstRecNo then //bottom limit of first occurrence search
              break
            else begin
              Data.GetPriorRecord(RecBuf);
              if (GetGoldenSectionDir(False) <> 0) then begin
                if IndexedFieldCount >= FieldCount then  //we shouldn't restore correct position in case of mixed locate
                  Data.GetNextRecord(RecBuf);     //Restore correct position
                break;
              end;
            end;

        until Data.Bof or Data.Eof;
    end;


    if (IndexedFieldCount = 0) or not CanUseGoldenSection or ((IndexedFieldCount < FieldCount) and Result) then begin

      if not((lxNext in Options) or (lxUp in Options)) and not (IndexedFieldCount > 0) then
        Data.SetToBegin;

      Exclude(CompareOptions, coOrdinalCompare);
      PartialSearchFirstEntry := (IndexedFieldCount < FieldCount) and Result;
      while True do begin
        if not PartialSearchFirstEntry then
          if lxUp in Options then
            Data.GetPriorRecord(RecBuf)
          else
            Data.GetNextRecord(RecBuf)
        else
          PartialSearchFirstEntry := False;

        if CalcKeyFields then
          GetCalcFields({$IFDEF VER18P}NativeInt{$ENDIF}(RecBuf));

        if not (Data.EOF or Data.BOF) then begin
          Result := True;
          i := 0;
          while Result and (i < FieldCount) do begin
            FieldDesc := TFieldDesc(KeyFields[i]);

            FieldBlank := Data.GetNull(FieldDesc, RecBuf);

            ValuePtr := Values[i].DataPtr;
            if (ValuePtr = nil) or FieldBlank then
              Result := (ValuePtr = nil) and FieldBlank
            else begin
              Res := Data.CompareFieldValue(ValuePtr, Values[i].DataLen, Values[i].DataType, FieldDesc, RecBuf, CompareOptions, True);
              Result := (Res = 0);
              if (Res < 0) and(lxNearest in Options) then begin
                Result := true;
                break;
              end;
            end;
            Inc(i);
          end;

          if Result then
            break;
        end
        else begin
          Result := lxNearest in Options; // goto last record for lxNearest
          break;
        end;
      end;
    end;

    if (SavePos or not Result)
      and not (Data.Eof and Data.Bof {Empty Data}) then
      Data.SetToBookmark(Bookmark);

  finally
    FDisableResync := False;

    Marshal.FreeHGlobal(Bookmark);
    for i := 0 to FieldCount - 1 do
      Marshal.FreeHGlobal(Values[i].DataPtr);
  end;

  if FDataWasChanged then
    DoOnDataChanged;
end;

function TMemDataSet.LocateRecord(const KeyFields: string; const KeyValues: variant;
  Options: TLocateExOptions; SavePos: boolean): boolean;
var
  Fields: TList;
  FieldDesc: TFieldDesc;

  procedure ParseKeyFields;
  var
    St: string;
    i: integer;
  begin
    i := 1;
    while True do begin
      St := ExtractFieldName(KeyFields, i);
    {$IFDEF FPC}
      St := Trim(St);
    {$ENDIF}
      if St <> '' then begin
        FieldDesc := Data.FieldByName(St);
        if FieldDesc <> nil then
          Fields.Add(FieldDesc);
      end
      else
        break;
    end;
  end;

begin
  CheckActive;

  Fields := TList.Create;
  try
    ParseKeyFields;
    Result := LocateRecord(Fields, KeyValues, Options, SavePos);
  finally
    Fields.Free;
  end;
end;

function TMemDataSet.LocateRecord(const KeyFields: array of TField; const KeyValues: variant;
  Options: TLocateExOptions; SavePos: boolean): boolean;
var
  Fields: TList;
  i: integer;
begin
  CheckActive;

  Fields := TList.Create;
  try
    for i := 0 to Length(KeyFields) - 1 do begin
      if KeyFields[i] <> nil then
        Fields.Add(GetFieldDesc(KeyFields[i]));
    end;

    Result := LocateRecord(Fields, KeyValues, Options, SavePos);
  finally
    Fields.Free;
  end;
end;

function LocateExOptions(Options: TLocateOptions): TLocateExOptions;
begin
  Result := [];
  if loCaseInsensitive in Options then
    Result := Result + [lxCaseInsensitive];

  if loPartialKey in Options then
    Result := Result + [lxPartialKey];
end;

function TMemDataSet.Locate(const KeyFields: array of TField; const KeyValues: variant;
  Options: TLocateOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, LocateExOptions(Options), False);

  if Result then begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemDataSet.Locate(const KeyFields: string;
  const KeyValues: variant; Options: TLocateOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, LocateExOptions(Options), False);

  if Result then begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemDataSet.LocateEx(const KeyFields: string;
  const KeyValues: variant; Options: TLocateExOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, Options, False);

  if Result then begin
    if LocateExOldBehavior or Data.Eof or Data.Bof then
      Resync([{rmExact, rmCenter}])
    else
      Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemDataSet.LocateEx(const KeyFields: array of TField;
  const KeyValues: variant; Options: TLocateExOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, Options, False);

  if Result then begin
    if LocateExOldBehavior or Data.Eof or Data.Bof then
      Resync([{rmExact, rmCenter}])
    else
      Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemDataSet.Lookup(const KeyFields: string; const KeyValues: variant;
  const ResultFields: string): variant;
var
  bm: TBookmark;
begin
  Result := Null;

  if Active and Filtered then begin
    bm := GetBookmark;
    try
      CheckBrowseMode;
      CursorPosChanged;
      UpdateCursorPos;

      Data.FilterUpdated;
      Resync([]);
      try
        GotoBookmark(bm);
      except
        First;
      end;
    finally
      if bm <> nil then
        FreeBookmark(bm);
    end;
  end;

  if LocateRecord(KeyFields, KeyValues, [], True) then begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

{ CachedUpdates }

function TMemDataSet.InternalGetUpdateResult: TUpdateRecAction;
begin
  UpdateCursorPos;

  Assert(Data <> nil);
  Result := Data.GetUpdateResult;
end;

procedure TMemDataSet.CheckCachedUpdateMode;
begin
  if not CachedUpdates then
    DatabaseError(SNotCachedUpdate);
end;

function TMemDataSet.UpdateStatus: TUpdateStatus;
var
  RecBuf: TRecordBuffer;
begin
  if CachedUpdates and not IsEmpty then begin
    if State = dsCalcFields then
      RecBuf := IntPtr(CalcBuffer)
    else
      RecBuf := IntPtr(ActiveBuffer);

    Result := PRecInfo(PtrOffset(RecBuf, FRecInfoOfs)).UpdateStatus;
  end
  else
    Result := usUnModified;
end;

function TMemDataSet.UpdateResult: TUpdateAction;
begin
  UpdateCursorPos;

  Assert(Data <> nil);
  if Data.GetUpdateResult = urNone then
    Result := uaApplied
  else
    Result := TUpdateAction(Data.GetUpdateResult);
end;

procedure TMemDataSet.ApplyUpdates;
begin
  ApplyUpdates([ukUpdate, ukInsert, ukDelete]);
end;

procedure TMemDataSet.ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds);
begin
  CheckActive;
  FreeRefBuffers;
  if State <> dsBrowse then
    Post;
  CheckCachedUpdateMode;
  UpdateCursorPos;

  Assert(Data <> nil);
  FNewCacheRecBuf := AllocRecordBuffer;
  FOldCacheRecBuf := AllocRecordBuffer;
  try
    Data.SetCacheRecBuf(FNewCacheRecBuf, FOldCacheRecBuf);
    Data.ApplyUpdates(UpdateRecKinds);
  finally
    FreeRecordBuffer(FNewCacheRecBuf);
    FreeRecordBuffer(FOldCacheRecBuf);
    Resync([]);
  end;
end;

procedure TMemDataSet.CommitUpdates;
begin
  CheckActive;
  CheckCachedUpdateMode;
  FreeRefBuffers;
  UpdateCursorPos;

  Assert(Data <> nil);
  FNewCacheRecBuf := AllocRecordBuffer;
  FOldCacheRecBuf := AllocRecordBuffer;
  try
    Data.SetCacheRecBuf(FNewCacheRecBuf, FOldCacheRecBuf);
    Data.CommitUpdates;
  finally
    FreeRecordBuffer(FNewCacheRecBuf);
    FreeRecordBuffer(FOldCacheRecBuf);
  end;
  Resync([]);
end;

procedure TMemDataSet.CancelUpdates;
begin
  CheckActive;
  FreeRefBuffers;
  Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Assert(Data <> nil);
  Data.CancelUpdates;
  Resync([]);
end;

procedure TMemDataSet.RestoreUpdates;
begin
  CheckActive;
  FreeRefBuffers;
  Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Assert(Data <> nil);
  Data.RestoreUpdates;
  Resync([]);
end;

procedure TMemDataSet.RevertRecord;
begin
  CheckActive;
  FreeRefComplexFields(IntPtr(ActiveBuffer));
  if State in dsEditModes then
    Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Assert(Data <> nil);
  Data.RevertRecord;
  Resync([]);
end;

{ BLOB Support }

{function TMemDataSet.GetBlobData(Field:TField; Buffer: PAnsiChar):TBlobData;
begin
  Result := PBlobDataArray(Buffer + FBlobCacheOfs)[Field.Offset];
end;

procedure TMemDataSet.SetBlobData(Field:TField; Buffer: PAnsiChar; Value:TBlobData);
begin
  if Buffer = ActiveBuffer then
    PBlobDataArray(Buffer + FBlobCacheOfs)[Field.Offset] := Value;
end;

procedure TMemDataSet.ClearBlobCache(Buffer: PAnsiChar);
var
  i: integer;
begin
  for i := 0 to BlobFieldCount - 1 do
    PBlobDataArray(Buffer + FBlobCacheOfs)[i] := '';
end;}

procedure TMemDataSet.CloseBlob(Field: TField);
begin
end;

function TMemDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TBlobStream.Create(Field as TBlobField, Mode);
end;

{ Informational }

function TMemDataSet.IsSequenced: boolean;
begin
  Result := True;
end;

function TMemDataSet.InCacheProcessing: boolean;
begin
  Assert(Data <> nil);
  Result := Data.InCacheProcessing;
end;

procedure TMemDataSet.SetRange(const StartValues, EndValues: array of const; StartExlusive: Boolean = False; EndExclusive: Boolean = False);
begin
  CheckBrowseMode;
  SetRangeBuffer(FRangeStartBuffer, StartValues);
  PRecInfo(PtrOffset(FRangeStartBuffer, FRecInfoOfs)).KeyExclusive := StartExlusive;
  SetRangeBuffer(FRangeEndBuffer, EndValues);
  PRecInfo(PtrOffset(FRangeEndBuffer, FRecInfoOfs)).KeyExclusive := EndExclusive;
  ApplyRange;
end;

procedure TMemDataSet.ApplyRange;
var
  IsModified: Boolean;
begin
  if (State = dsSetKey) then begin
    IsModified := Modified;
    CheckBrowseMode;
    if IsModified then begin
      Data.FilterRangeFunc := RecordFilterRange;
      Data.FilterUpdated;
      Resync([]);
      First;
    end;
    FRanged := True;
  end;
end;

procedure TMemDataSet.CancelRange;
begin
  if FRanged then begin
    Data.FilterRangeFunc := nil;
    Data.FilterUpdated;
    Resync([]);
    First;
    FRanged := False;
  end;
end;

procedure TMemDataSet.SetRangeStart;
begin
  InitRangeBuffer(FRangeStartBuffer, True);
end;

procedure TMemDataSet.SetRangeEnd;
begin
  InitRangeBuffer(FRangeEndBuffer, True);
end;

procedure TMemDataSet.EditRangeStart;
begin
  InitRangeBuffer(FRangeStartBuffer, False);
end;

procedure TMemDataSet.EditRangeEnd;
begin
  InitRangeBuffer(FRangeEndBuffer, False);
end;

function TMemDataSet.GetRecordSize: word;
begin
  Assert(Data <> nil);
  Result := word(Data.RecordSize);
end;

function TMemDataSet.GetRecordCount: integer;
begin
  if Active then
    Result := Data.RecordCount
  else
    Result := 0;
end;

function TMemDataSet.GetRecNo: integer;
var
  RecBuf: TRecordBuffer;
begin
  if GetActiveRecBuf(RecBuf) then
    Result := PRecInfo(PtrOffset(RecBuf, FRecInfoOfs)).RecordNumber
  else
    Result := 0;
end;

procedure TMemDataSet.SetRecNo(Value: integer);
begin
  CheckBrowseMode;
  DoBeforeScroll;
  Assert(Data <> nil);
  Data.RecordNo := Value;
  Resync([{rmCenter}]);
  DoAfterScroll;
end;

{ More }

procedure TMemDataSet.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(ExceptObject)
  else
    ShowException(ExceptObject, ExceptAddr)
end;

{$IFDEF FPC}
procedure TMemDataSet.DataEvent(Event: TDataEvent; Info: PtrInt);
{$ELSE}
{$IFDEF VER16P}
procedure TMemDataSet.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TMemDataSet.DataEvent(Event: TDataEvent; Info: Longint);
{$ENDIF}
{$ENDIF}
  procedure CheckIfParentScrolled;
  var
    ParentPosition, I: Integer;
  begin
    if FParentDataSet = nil then
      Exit;
    ParentPosition := 0;
    with FParentDataSet do
      if not IsEmpty then
        for I := 0 to BookmarkSize - 1 do
          ParentPosition := ParentPosition +
            Marshal.ReadByte(IntPtr(ActiveBuffer), FBookmarkOfs + I);
    if (FLastParentPos = 0) or (ParentPosition <> FLastParentPos) then
    begin
      First;
      FLastParentPos := ParentPosition;
    end else
    begin
      UpdateCursorPos;
      Resync([]);
    end;
  end;

begin
  if Event = deParentScroll then
    CheckIfParentScrolled;

  inherited DataEvent(Event, Info);
end;

procedure TMemDataSet.AssignTo(Dest: TPersistent);
begin
  if Dest is TMemDataSet then begin
    TMemDataSet(Dest).CachedUpdates := CachedUpdates;
    TMemDataSet(Dest).LocalConstraints := LocalConstraints;
    TMemDataSet(Dest).LocalUpdate := LocalUpdate;
    TMemDataSet(Dest).MasterSource := MasterSource;
    TMemDataSet(Dest).MasterFields := MasterFields;
    TMemDataSet(Dest).DetailFields := DetailFields;
    TMemDataSet(Dest).DetailDelay := DetailDelay;
  end
  else
    inherited;
end;

procedure TMemDataSet.SetCachedUpdates(Value: boolean);
begin
  if FCachedUpdates <> Value then begin
    CheckInactive;
    FCachedUpdates := Value;
    FUpdateRecordTypes := [rtModified, rtInserted, rtUnmodified];
    if Data <> nil then
      Data.CachedUpdates := FCachedUpdates;
  end;
end;

procedure TMemDataSet.SetLocalUpdate(Value: boolean);
begin
  if FLocalUpdate <> Value then begin
    FLocalUpdate := Value;
    if Data <> nil then
      Data.LocalUpdate := FLocalUpdate;
  end;
end;

function TMemDataSet.GetUpdatesPending: boolean;
begin
  if Data <> nil then
    Result := Data.UpdatesPending
  else
    Result := False;
end;

function TMemDataSet.GetPrepared: boolean;
begin
  if Data <> nil then
    Result := Data.Prepared
  else
    Result := False;
end;

procedure TMemDataSet.SetPrepared(Value: boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

function TMemDataSet.ConvertUpdateRecordTypes(Value: TUpdateRecordTypes): TItemTypes;
begin
  Result := [];
  if rtUnmodified in Value then
    Result := Result + [isUnmodified];
  if rtModified in Value then
    Result := Result + [isUpdated];
  if rtInserted in Value then
    Result := Result + [isAppended];
  if rtDeleted in Value then
    Result := Result + [isDeleted];
end;

function TMemDataSet.GetUpdateRecordTypes: TUpdateRecordTypes;
begin
  CheckCachedUpdateMode;

  Result := FUpdateRecordTypes;
end;

procedure TMemDataSet.SetUpdateRecordTypes(Value: TUpdateRecordTypes);
begin
  CheckCachedUpdateMode;

  //CheckBrowseMode;
  if Active then begin
    UpdateCursorPos;
    case State of // CR-S22656
      dsEdit, dsInsert: begin
        UpdateRecord;
        if Modified then Post else Cancel;
      end;
      dsSetKey:
        Post;
    end;
  end;

  FUpdateRecordTypes := Value;

  if Data <> nil then begin
    Data.FilterItemTypes := ConvertUpdateRecordTypes(Value);

    if Active then
      Resync([]);
  end;
end;

{ TBlobStream }

constructor TBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;

  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TMemDataSet;
  FFieldDesc := FDataSet.GetFieldDesc(FField.FieldNo);
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
{  if FDataSet.State = dsFilter then
    DatabaseErrorFmt('SNoFieldAccess', [FField.DisplayName]);}
  if not FField.Modified then begin
    if Mode = bmRead then begin
{      FCached := FDataSet.FCacheBlobs and (FBuffer = FDataSet.ActiveBuffer) and
        (FField.IsNull or (FDataSet.GetBlobData(FField, FBuffer) <> ''));}
    end
    else begin
//      FDataSet.SetBlobData(FField, FBuffer, '');
      if FField.ReadOnly then
        DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName]);
      if not (FDataSet.State in [dsNewValue, dsEdit, dsInsert]) then
        DatabaseError(SNotEditing);
    end;
  end;
  FOpened := True;
  if Mode = bmWrite then
    Truncate;
end;

destructor TBlobStream.Destroy;
begin
  if FOpened then begin
    if FModified then
      FField.Modified := True;
  end;
  if FModified then
    //try
    {$IFDEF FPC}
      FDataSet.DataEvent(deFieldChange, PtrInt(FField));
    {$ELSE}
    {$IFDEF VER16P}
      FDataSet.DataEvent(deFieldChange, NativeInt(FField));
    {$ELSE}
      FDataSet.DataEvent(deFieldChange, Longint(FField));
    {$ENDIF}
    {$ENDIF}
    {except
      Application.HandleException(Self);
    end;}

  inherited;
end;

function TBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if FOpened then begin
    if Count > Size - FPosition then
      Result := Size - FPosition
    else
      Result := Count;
    if Result > 0 then begin
      Result := FDataSet.Data.ReadBlob(FFieldDesc, FBuffer, FPosition, Count, @Buffer, (FDataSet.State = dsOldValue)
                                       {$IFDEF VER10P}, FField is TWideMemoField{$ENDIF}
                                       {$IFDEF FPC}, FField is TWideMemoField{$ENDIF}
                                      );
      Inc(FPosition, Result);
    end;
  end;
end;

function TBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if FOpened then begin
    FDataSet.Data.WriteBlob(FFieldDesc, FBuffer, FPosition, Count, @Buffer
                            {$IFDEF VER10P}, FField is TWideMemoField{$ENDIF}
                            {$IFDEF FPC}, FField is TWideMemoField{$ENDIF}
                           );
    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
{    FDataSet.SetBlobData(FField, FBuffer, '');}
  end;
end;

function TBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      FPosition := Offset;
    soFromCurrent:
      Inc(FPosition, Offset);
    soFromEnd:
      FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TBlobStream.Truncate;
begin
  if FOpened then begin
    FDataSet.Data.TruncateBlob(FFieldDesc, FBuffer, FPosition
                               {$IFDEF VER10P}, FField is TWideMemoField{$ENDIF}
                               {$IFDEF FPC}, FField is TWideMemoField{$ENDIF}
                              );
    FModified := True;
//    FDataSet.SetBlobData(FField, FBuffer, '');
  end;
end;

function TBlobStream.GetBlobSize: Longint;
begin
  Result := 0;
  if FOpened then
    Result := FDataSet.Data.GetBlobSize(FFieldDesc, FBuffer, (FDataSet.State = dsOldValue)
                                        {$IFDEF VER10P}, FField is TWideMemoField{$ENDIF}
                                        {$IFDEF FPC}, FField is TWideMemoField{$ENDIF}
                                       );
end;

procedure TBlobStream.SetSize(NewSize: Longint);
begin
  if FOpened then
    FDataSet.Data.SetBlobSize(FFieldDesc, FBuffer, NewSize, (FDataSet.State = dsOldValue)
                              {$IFDEF VER10P}, (FField is TWideMemoField){$ENDIF}
                              {$IFDEF FPC}, FField is TWideMemoField{$ENDIF}
                             );
end;

{ TMemDSUtils }

class function TMemDSUtils.SetBlob(Obj: TMemDataSet; Field: TField; Blob: TBlob): boolean;
begin
  Result := Obj.SetBlob(Field, Blob);
end;

class function TMemDSUtils.GetBlob(Obj: TMemDataSet; FieldDesc: TFieldDesc): TBlob;
begin
  Result := Obj.InternalGetBlob(FieldDesc);
end;

{ TDataSetUpdater }

constructor TDataSetUpdater.Create(AOwner: TDataSetService);
begin
  inherited Create;

  FDataSetService := AOwner;
  Assert(FDataSetService <> nil);
  FDataSet := FDataSetService.FDataset;
end;

function TDataSetUpdater.PerformAppend: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.PerformDelete: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.PerformUpdate: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.CacheChanged: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.CacheApplied: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.CacheCanceled: boolean;
begin
  Result := True;
end;

procedure TDataSetUpdater.DoPerformAppend;
var
  OldModified: boolean;
begin
  OldModified := FDataSet.Modified;
  try
    if not FDataSet.FLocalUpdate then
      if not FDataSet.FInDeferredPost then   // WAR supports defer posting
        PerformAppend
      else
        DoPerformUpdate;
  finally
    FDataSet.SetModified(OldModified);
  end;
end;

procedure TDataSetUpdater.DoPerformDelete;
var
  OldModified: boolean;
begin
  OldModified := FDataSet.Modified;
  try
    if not FDataSet.FLocalUpdate then
      PerformDelete
  finally
    FDataSet.SetModified(OldModified);
  end;
end;

procedure TDataSetUpdater.DoPerformUpdate;
var
  OldModified: boolean;
begin
  OldModified := FDataSet.Modified;
  try
    if not FDataSet.FLocalUpdate then
      PerformUpdate
  finally
    FDataSet.SetModified(OldModified);
  end;
end;

procedure TDataSetUpdater.DoCacheChanged;
begin
  CacheChanged;
end;

procedure TDataSetUpdater.DoCacheApplied;
begin
  CacheApplied;
end;

procedure TDataSetUpdater.DoCacheCanceled;
begin
  CacheCanceled;
end;

procedure TDataSetUpdater.DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
var
  OldModified: boolean;
  OldInCacheProcessing: boolean;
  UpdateAction: TUpdateAction;
begin
  OldModified := FDataSet.Modified;  // NewValue change Modified ??? or MemDS
  try
    UpdateAction := uaFail;
    try
      if Assigned(FDataSet.OnUpdateRecord) then begin
        FDataSet.OnUpdateRecord(FDataSet, TUpdateKind(UpdateKind), UpdateAction);
        case UpdateAction of
          uaFail:
            if not DoNotRaiseExcetionOnUaFail then
              raise EDatabaseError.Create(SCustomUpdateFailed);
          uaAbort:
            Abort;
        end;
      end;
      //else begin
      if not Assigned(FDataSet.OnUpdateRecord) or (UpdateAction = TUpdateAction(uaDefault)) then begin
        case UpdateKind of
          ukUpdate:
            PerformUpdate;
          ukInsert:
            PerformAppend;
          ukDelete:
            PerformDelete;
        end;

        if BatchUpdate then
          if CanFlushBatch or LastItem then
            // Should be flushed here because of Action parameter that should be returned
            // to TMemData.ApplyUpdates function
            FlushBatch
          else begin
            Action := urSuspended;
            Exit;
          end;

        UpdateAction := uaApplied;
      end;
    except
      on E: Exception do
        if IsClass(E, EDatabaseError) and Assigned(FDataSet.OnUpdateError) then begin
          OldInCacheProcessing := FDataSet.Data.InCacheProcessing;
          FDataSet.Data.InCacheProcessing := False;
          try
            FDataSet.OnUpdateError(FDataSet, EDatabaseError(E), TUpdateKind(UpdateKind), UpdateAction);
          finally
            FDataSet.Data.InCacheProcessing := OldInCacheProcessing;
          end;
          case UpdateAction of
            uaFail:
              raise;
            uaAbort:
              Abort;
          end;
        end
        else
          raise;
    end;
  finally
    FDataSet.SetModified(OldModified);
  end;
  Action := TUpdateRecAction(UpdateAction);
end;

procedure TDataSetUpdater.DoAfterApplyUpdates;
begin
  if BatchUpdate then
    FlushBatch;
end;

function TDataSetUpdater.BatchUpdate: boolean;
begin
  Result := False;
end;

function TDataSetUpdater.CanFlushBatch: boolean;
begin
  Result := False;
end;

procedure TDataSetUpdater.FlushBatch;
begin

end;

{ TDataSetService }

constructor TDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited Create;

  FDataSet := AOwner;
  CreateDataSetUpdater;
end;

destructor TDataSetService.Destroy;
begin
  FreeDataSetUpdater;

  inherited;
end;

procedure TDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TDataSetUpdater.Create(Self));
end;

procedure TDataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  FreeDataSetUpdater;
  FUpdater := Value;
end;

procedure TDataSetService.FreeDataSetUpdater;
begin
  FUpdater.Free;
  FUpdater := nil;
end;

function TDataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := False;
  Assert(False);
end;

function TDataSetService.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := False;
  Assert(False);
end;

procedure TDataSetService.SetNumberRange(FieldDef: TFieldDef);
var
  Field: TField;
  FieldDesc: TFieldDesc;
begin
  if FieldDef.DataType in [ftSmallint, ftInteger, ftLargeint, ftFloat] then begin
    Field := FDataSet.FindField(FieldDef.Name);
    if Field <> nil then begin
      FDataSet.CheckFieldCompatibility(Field, FieldDef);
      FieldDesc := FDataSet.Data.FindField(FieldDef.Name);
      if FieldDef.DataType in [ftSmallint, ftInteger] then begin
      {$IFNDEF FPC}
        Assert(Field is TIntegerField);
        TIntegerField(Field).MaxValue := Round(IntPower(10, FieldDesc.Length)) - 1;
        TIntegerField(Field).MinValue := -TIntegerField(Field).MaxValue;
      {$ELSE}
        Assert(Field is TLongintField);
        TLongintField(Field).MaxValue := Round(IntPower(10, FieldDesc.Length)) - 1;
        TLongintField(Field).MinValue := -TIntegerField(Field).MaxValue;
      {$ENDIF}
      end
      else
      if FieldDef.DataType = ftLargeint then begin
        Assert(Field is TLargeintField);
        TLargeintField(Field).MaxValue := Round(IntPower(10, FieldDesc.Length)) - 1;
        TLargeintField(Field).MinValue := -TIntegerField(Field).MaxValue;
      end
      else
        if (FieldDesc.Length > 0) and (FieldDesc.Length <= 15) then begin
          Assert(Field is TFloatField);
          TFloatField(Field).Precision := FieldDesc.Length;
          TFloatField(Field).MaxValue :=
            IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
            IntPower(10, - FieldDesc.Scale);
          TFloatField(Field).MinValue := - TFloatField(Field).MaxValue;
        end;
    end;
  end;
end;

function TDataSetService.GetFieldClass(FieldType: TFieldType; DataType: Word): TFieldClass;
begin
{$IFNDEF FPC}
  if Integer(FieldType) = ftDATimeStampOffset then
    Result := TDASQLTimeStampOffsetField
  else
{$ENDIF}
  Result := DefaultFieldClasses[FieldType];
end;

procedure TDataSetService.PreInitCursor;
var
  FieldDesc: TFieldDesc;
  i: integer;
begin
{$IFDEF VER20P}
  if lcPersistent in FDataSet.Fields.LifeCycles then
{$ELSE}
  if not FDataSet.DefaultFields then
{$ENDIF}
    // if Fielddesc is found in the dataset then it can be used in queries for data update
    for i := 0 to FDataSet.Data.Fields.Count - 1 do begin
      FieldDesc := FDataSet.Data.Fields[i];
      if FieldDesc.ParentField = nil then
        FieldDesc.Updateable := FDataSet.FindField(FieldDesc.Name) <> nil
      else
        FieldDesc.Updateable := FDataSet.FindField(FieldDesc.ActualName) <> nil;
    end;
end;

procedure TDataSetService.SaveToXML(Destination: TStream);
var
  FieldAliases: TStringList;

  function IsValidFieldName(const FldName: string): boolean;
  var
    i: integer;
    ch: char;
  begin
    for i := 1 to Length(FldName) do begin
      ch := FldName[i];
      if not ((ch = '_') or (ch >= 'A') and (ch <= 'Z') or (ch >= 'a') and (ch <= 'z') or
        (i > 1) and (ch >= '0') and (ch <= '9'))
      then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;

  function GetFieldAlias(const FldName: string): string;
  var
    i: integer;
    ActualName: string;
  begin
    for i := 0 to FieldAliases.Count - 1 do begin
      ActualName := FieldAliases[i];
      if ActualName = FldName then begin
        Result := 'c' + IntToStr(i + 1);
        exit;
      end;
    end;
    Result := '';
  end;

var
  OldActive: boolean;
  Bookmark: DB.TBookmark;
  DestWriter: StreamWriter;
  XMLWriter: XmlTextWriter;
  XML: WideString;
  FldName, FldAlias: string;
  i: integer;
  FieldDesc: TFieldDesc;
begin
  OldActive := FDataSet.Active;
  Bookmark := nil;
  FieldAliases := nil;
  DestWriter := nil;
  XmlWriter := nil;
  try
    FDataSet.DisableControls;
    FDataSet.Active := True;
    Bookmark := FDataSet.GetBookmark;
    FieldAliases := TStringList.Create;

    DestWriter := StreamWriter.Create(Destination, Encoding(Encoding.UTF8));
    XmlWriter := XmlTextWriter.Create(DestWriter);

    XmlWriter.QuoteChar := '''';
    XmlWriter.Formatting := fmtIndented;
    XmlWriter.Indentation := 2;
    XmlWriter.IndentChar := ' ';

    // Header
    XmlWriter.WriteStartElement('xml');

    XmlWriter.WriteAttributeString('xmlns:s', 'uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882');
    XmlWriter.WriteAttributeString('xmlns:dt', 'uuid:C2F41010-65B3-11d1-A29F-00AA00C14882');
    XmlWriter.WriteAttributeString('xmlns:rs', 'urn:schemas-microsoft-com:rowset');
    XmlWriter.WriteAttributeString('xmlns:z', '#RowsetSchema');

    // Fields
    XmlWriter.WriteStartElement('s:Schema');
    XmlWriter.WriteAttributeString('id', 'RowsetSchema');

    XmlWriter.WriteStartElement('s:ElementType');
    XmlWriter.WriteAttributeString('name', 'row');
    XmlWriter.WriteAttributeString('content', 'eltOnly');
    XmlWriter.WriteAttributeString('rs:updatable', 'true');

    for i := 0 to FDataSet.Fields.Count - 1 do
      if not (FDataSet.Fields[i].FieldKind in [fkCalculated, fkLookup]) then begin
        FieldDesc := FDataSet.GetFieldDesc(FDataSet.Fields[i]);
        if FieldDesc <> nil then begin
          FldName := FieldDesc.Name;
          if not IsValidFieldName(FldName) then begin
            FldAlias := 'c' + IntToStr(FieldAliases.Count + 1);
            FieldAliases.Add(FldName);
          end
          else
            FldAlias := '';
          XmlWriter.WriteStartElement('s:AttributeType');
          WriteFieldXMLAttributeType(FDataSet.Fields[i], FieldDesc, FldAlias, XmlWriter);
          XmlWriter.WriteStartElement('s:datatype');
          WriteFieldXMLDataType(FDataSet.Fields[i], FieldDesc, FldAlias, XmlWriter);
          XmlWriter.WriteEndElement; // s:datatype
          XmlWriter.WriteFullEndElement; // s:AttributeType
        end;
      end;

    XmlWriter.WriteStartElement('s:extends');
    XmlWriter.WriteAttributeString('type', 'rs:rowbase');
    XmlWriter.WriteEndElement; // s:extends
    XmlWriter.WriteFullEndElement; // s:ElementType
    XmlWriter.WriteFullEndElement; // s:Schema

    // Data
    XmlWriter.WriteStartElement('rs:data');

    FDataSet.First;
    while not FDataSet.EOF do begin
      XmlWriter.WriteStartElement('z:row');
      for i := 0 to FDataSet.Fields.Count - 1 do
        if not (FDataSet.Fields[i].FieldKind in [fkCalculated, fkLookup]) then begin
          if not FDataSet.Fields[i].IsNull then begin
            XML := GetFieldXMLValue(FDataSet.Fields[i], FDataSet.GetFieldDesc(FDataSet.Fields[i]));
            FldName := FDataSet.Fields[i].FieldName;
            if not IsValidFieldName(FldName) then
              FldName := GetFieldAlias(FldName);
            XmlWriter.WriteAttributeString(FldName, XML);
          end;
        end;
      XmlWriter.WriteEndElement;
      FDataSet.Next;
    end;

    XmlWriter.WriteFullEndElement; // rs:data
    XmlWriter.WriteFullEndElement; // xml

    XmlWriter.Close;
  finally
    FDataSet.Active := OldActive;
    if OldActive and (Bookmark <> nil) then
      FDataSet.GotoBookmark(Bookmark);
    if Bookmark <> nil then
      FDataSet.FreeBookmark(Bookmark);
    FieldAliases.Free;
    XmlWriter.Free;
    DestWriter.Free;

    FDataSet.EnableControls;
  end;
end;

procedure TDataSetService.WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc;
  const FieldAlias: string; XmlWriter: XMLTextWriter);
begin
  case FieldDesc.DataType of
    dtBlob, dtBytes, dtVarBytes, dtExtVarBytes: begin
      XmlWriter.WriteAttributeString('dt:type', 'bin.hex');
      if (FieldDesc.DataType = dtBlob) then
        XmlWriter.WriteAttributeString('dt:maxLength', '2147483647')
      else
        XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Length));
      if Field.IsBlob and not FieldDesc.Fixed then
        XmlWriter.WriteAttributeString('rs:long', 'true');
    end;
    dtBoolean: begin
      XmlWriter.WriteAttributeString('dt:type', 'boolean');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
    end;
    dtString, dtExtString, dtMemo, dtWideString, dtExtWideString, dtWideMemo: begin
      XmlWriter.WriteAttributeString('dt:type', 'string');
      if FieldDesc.DataType in [dtMemo, dtWideMemo] then
        XmlWriter.WriteAttributeString('dt:maxLength', '2147483647')
      else
        XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Length));
      if not (FieldDesc.DataType in [dtWideString, dtExtWideString, dtWideMemo]) then
        XmlWriter.WriteAttributeString('rs:dbtype', 'str');
      if Field.IsBlob and not FieldDesc.Fixed then
        XmlWriter.WriteAttributeString('rs:long', 'true');
    end;
    dtCurrency, dtBCD: begin
      XmlWriter.WriteAttributeString('dt:type', 'number');
      XmlWriter.WriteAttributeString('rs:dbtype', 'currency');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtDateTime: begin
      XmlWriter.WriteAttributeString('dt:type', 'datetime');
      XmlWriter.WriteAttributeString('rs:dbtype', 'variantdate');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtDate: begin
      XmlWriter.WriteAttributeString('dt:type', 'date');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtTime: begin
      XmlWriter.WriteAttributeString('dt:type', 'time');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
  {$IFNDEF FPC}
    dtSQLTimeStamp: begin
      XmlWriter.WriteAttributeString('dt:type', 'datetime');
      XmlWriter.WriteAttributeString('rs:dbtype', 'timestamp');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
  {$ENDIF}
    dtFloat, dtSingle, dtExtended: begin
      if (FieldDesc.Length <= 7) and (FieldDesc.Size <= 4) then begin
        XmlWriter.WriteAttributeString('dt:type', 'r4');
        XmlWriter.WriteAttributeString('dt:maxLength', '4');
      end
      else begin
        XmlWriter.WriteAttributeString('dt:type', 'float');
        XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      end;
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtGuid: begin
      XmlWriter.WriteAttributeString('dt:type', 'uuid');
      XmlWriter.WriteAttributeString('dt:maxLength', '38');
    end;
    dtInt32: begin
      XmlWriter.WriteAttributeString('dt:type', 'int');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', '10');
    end;
    dtFmtBCD: begin
      XmlWriter.WriteAttributeString('dt:type', 'number');
      XmlWriter.WriteAttributeString('rs:dbtype', 'numeric');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtInt16: begin
      XmlWriter.WriteAttributeString('dt:type', 'i2');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', '5');
    end;
    dtWord: begin
      XmlWriter.WriteAttributeString('dt:type', 'ui2');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtInt8: begin
      XmlWriter.WriteAttributeString('dt:type', 'i1');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtUInt8: begin
      XmlWriter.WriteAttributeString('dt:type', 'ui1');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtLongword: begin
      XmlWriter.WriteAttributeString('dt:type', 'ui4');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtInt64: begin
      XmlWriter.WriteAttributeString('dt:type', 'i8');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', '19');
    end;
    dtUInt64: begin
      XmlWriter.WriteAttributeString('dt:type', 'ui8');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', '19');
    end;
    dtUnknown, dtVariant:
      XmlWriter.WriteAttributeString('dt:type', 'string')
    else
      DatabaseError(SDataTypeNotSupported, FDataSet);
  end;

  if FieldDesc.Fixed and not (FieldDesc.DataType in [dtUnknown, dtVariant]) then
    XmlWriter.WriteAttributeString('rs:fixedlength', 'true');

  if Field.Required and not Field.ReadOnly then
    XmlWriter.WriteAttributeString('rs:maybenull', 'false');
end;

procedure TDataSetService.WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc;
  const FieldAlias: string; XmlWriter: XMLTextWriter);
begin
  if FieldAlias = '' then
    XmlWriter.WriteAttributeString('name', FieldDesc.Name)
  else begin
    XmlWriter.WriteAttributeString('name', FieldAlias);
    XmlWriter.WriteAttributeString('rs:name',  FieldDesc.Name);
  end;

  XmlWriter.WriteAttributeString('rs:number',  IntToStr(FieldDesc.FieldNo));
  if not Field.Required and not Field.ReadOnly then /// Can't use FieldDesc.Required, see "Required and FLocalConstraints" line in TMemDataSet.CreateFieldDefs for details
    XmlWriter.WriteAttributeString('rs:nullable', 'true');
  if not Field.ReadOnly then
    XmlWriter.WriteAttributeString('rs:writeunknown', 'true');
  // XmlWriter.WriteAttributeString('rs:basecatalog', '');

  if FieldDesc.ActualName <> '' then
    XmlWriter.WriteAttributeString('rs:basecolumn', FieldDesc.ActualName);
  if FieldDesc.IsKey then
    XmlWriter.WriteAttributeString('rs:keycolumn', 'true');
  if FDataSet.GetFieldDesc(Field).IsAutoIncrement then
    XmlWriter.WriteAttributeString('rs:autoincrement', 'true');
end;

function TDataSetService.GetFieldXMLValue(Field: TField; FieldDesc: TFieldDesc): WideString;
var
  Buffer: TBytes;
{$IFDEF FPC}
  sBuffer: string;
{$ENDIF}
  Blob: TBlob;
  Piece: PPieceHeader;
  sbOffset: integer;
  StrValue: string;
  StrBuffer: array[0..63] of Char;
  e: extended;

  function EncodeXMLDate(Value: TDateTime): string;
  var
    Year, Month, Day: Word;
  begin
    DecodeDate(Value, Year, Month, Day);
    Result := StringReplace(Format('%4d%s%2d%s%2d', [Year, '-', Month, '-', Day]),
      ' ', '0', [rfReplaceAll]);
  end;

  function EncodeXMLTime(Value: TDateTime): string;
  var
    Hour, Minute, Second, MilliSecond: Word;
  begin
    DecodeTime(Value, Hour, Minute, Second, MilliSecond);
    Result := StringReplace(Format('%2d%s%2d%s%2d', [Hour, ':', Minute, ':', Second]),
      ' ', '0', [rfReplaceAll]);
  end;

  function EncodeXMLDateTime(Value: TDateTime): string;
  begin
    Result := EncodeXMLDate(Value) + 'T' + EncodeXMLTime(Value);
  end;

begin
  Result := '';
  SetLength(Buffer, 0);
  case FieldDesc.DataType of
    dtBoolean:
      Result := WideString(BoolToStr(Field.AsBoolean, True));
    dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtInt64, dtUInt64:
      Result := WideString(Field.AsString);
    dtFloat: begin
      if FieldDesc.Scale = 0 then
        StrValue := FloatToStr(Field.AsFloat)
      else begin
        e := Field.AsFloat;
        SetString(StrValue, StrBuffer, FloatToText(StrBuffer, e,{$IFNDEF FPC} fvExtended,{$ENDIF} ffFixed, FieldDesc.Length, FieldDesc.Scale));
      end;
      ChangeDecimalSeparator(StrValue, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.');
      Result := WideString(StrValue);
    end;
    dtSingle: begin
      if (FieldDesc.Length = 0) and (FieldDesc.Scale = 0) then
        StrValue := FloatToStr(Field.{$IFDEF VER14P}AsSingle{$ELSE}AsFloat{$ENDIF})
      else begin
        e := Field.{$IFDEF VER14P}AsSingle{$ELSE}AsFloat{$ENDIF};
        SetString(StrValue, StrBuffer, FloatToText(StrBuffer, e,{$IFNDEF FPC} fvExtended,{$ENDIF} ffGeneral, FieldDesc.Length, FieldDesc.Scale));
      end;
      ChangeDecimalSeparator(StrValue, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.');
      Result := WideString(StrValue);
    end;
    dtExtended: begin
      if (FieldDesc.Length = 0) and (FieldDesc.Scale = 0) then
        StrValue := FloatToStr(Field.{$IFDEF VER12P}AsExtended{$ELSE}AsFloat{$ENDIF})
      else begin
        e := Field.{$IFDEF VER12P}AsExtended{$ELSE}AsFloat{$ENDIF};
        SetString(StrValue, StrBuffer, FloatToText(StrBuffer, e,{$IFNDEF FPC} fvExtended,{$ENDIF} ffGeneral, FieldDesc.Length, FieldDesc.Scale));
      end;
      ChangeDecimalSeparator(StrValue, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.');
      Result := WideString(StrValue);
    end;
    dtCurrency, dtBcd: begin
      StrValue := CurrToStr(Field.Value);
      ChangeDecimalSeparator(StrValue, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.');
      Result := WideString(StrValue);
    end;
    dtFmtBCD:
      Result := WideString(BcdToStr(Field.AsBCD));
    dtDateTime:
      Result := WideString(EncodeXMLDateTime(Field.AsDateTime));
    dtDate:
      Result := WideString(EncodeXMLDate(Field.AsDateTime));
    dtTime:
      Result := WideString(EncodeXMLTime(Field.AsDateTime));
  {$IFNDEF FPC}
    dtSQLTimeStamp:
      Result := EncodeXMLDateTime(Field.AsDateTime);
  {$ENDIF}
    dtBytes, dtVarBytes, dtExtVarBytes: begin
    {$IFNDEF FPC}
      Buffer := Field.Value;
      SetLength(StrValue, Length(Buffer) * 2);
      BinToHex(@Buffer[0], PChar(StrValue), Length(Buffer));
      Result := StrValue;
    {$ELSE}
      sBuffer := Field.Text;
      SetLength(StrValue, Length(sBuffer) * 2);
      BinToHex(PChar(sBuffer), PChar(StrValue), Length(Buffer));
      Result := WideString(StrValue);
    {$ENDIF}
    end;
    dtBlob: begin
      Blob := FDataSet.InternalGetBlob(FieldDesc);
      Piece := Blob.FirstPiece;
      SetLength(StrValue, Blob.Size * 2);
      sbOffset := 1;
      while Piece <> nil do begin
        BinToHex(PtrOffset(Piece, sizeof(TPieceHeader)), PChar(@StrValue[sbOffset]), Piece.Used);
        sbOffset := sbOffset + Piece.Used * 2;
        Piece := Piece.Next;
      end;
      Result := WideString(StrValue);
    end;
    else
      if Field is TWideStringField then
        Result := Field.Value
    {$IFDEF VER10P}
      else
      if Field is TWideMemoField then
        Result := Field.AsWideString
    {$ENDIF}
    {$IFDEF FPC}
      else
      if Field is TWideMemoField then
        Result := Field.AsWideString
    {$ENDIF}
      else
        Result := WideString(Field.AsString);
  end;
end;

{$IFNDEF FPC}
{$IFNDEF VER14P}

{ TDASQLTimeStampField }

const
  TicksPerMillisecond = 10000;

function TDASQLTimeStampField.GetValue(var Value: TSQLTimeStamp): Boolean;
var
  Data: TValueBuffer;
begin
{$IFDEF VER17P}
  SetLength(Data, SizeOf(TSQLTimeStamp));
{$ELSE}
  Data := Marshal.AllocHGlobal(SizeOf(TSQLTimeStamp));
{$ENDIF}

  Result := GetData(Data, True);
  if not Result then
    Value := NullSqlTimeStamp
  else
  {$IFDEF VER22P}
    Value := TDBBitConverter.UnsafeInto<TSQLTimeStamp>(Data);
  {$ELSE}
  {$IFDEF VER17P}
    Value := TBitConverter.ToSQLTimeStamp(Data);
  {$ELSE}
    Move(Data^, Value, SizeOf(TSqlTimeStamp));
  {$ENDIF}
  {$ENDIF}
end;

function TDASQLTimeStampField.GetAsDateTime: TDateTime;
var
  D: TSQLTimeStamp;
begin
  if GetValue(D) then begin
    D.Fractions := D.Fractions div TicksPerMillisecond;
    Result := SQLTimeStampToDateTime(D);
  end
  else
    Result := 0;
end;

procedure TDASQLTimeStampField.GetText(var Text: string; DisplayText: Boolean);
var
  F: string;
  D: TSQLTimeStamp;
begin
  if GetValue(D) then begin
    if DisplayText and (DisplayFormat <> '') then
      F := DisplayFormat
    else
      F := '';
    DateTimeToString(Text, F, D , FormatSettings);
  end
  else
    Text := '';
end;

{ TDASQLTimeStampOffsetField }

constructor TDASQLTimeStampOffsetField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(TFieldType(ftDATimeStampOffset));
end;

function TDASQLTimeStampOffsetField.GetValue(var Value: TSQLTimeStampOffset): Boolean;
var
  Data: TValueBuffer;
begin
{$IFDEF VER17P}
  SetLength(Data, SizeOf(TSQLTimeStampOffset));
{$ELSE}
  Data := Marshal.AllocHGlobal(SizeOf(TSQLTimeStampOffset));
{$ENDIF}

  Result := GetData(Data, True);
  if not Result then
    Value := NullSqlTimeStampOffset
  else
  {$IFDEF VER22P}
    Value := TDBBitConverter.UnsafeInto<TSQLTimeStampOffset>(Data);
  {$ELSE}
  {$IFDEF VER17P}
    Value := TBitConverter.ToSQLTimeStampOffset(Data);
  {$ELSE}
    Move(Data^, Value, SizeOf(TSQLTimeStampOffset));
  {$ENDIF}
  {$ENDIF}
end;

function TDASQLTimeStampOffsetField.GetAsDateTime: TDateTime;
var
  D: TSQLTimeStampOffset;
begin
  if GetValue(D) then begin
    D.Fractions := D.Fractions div TicksPerMillisecond;
    Result := SQLTimeStampOffsetToDateTime(D);
  end
  else
    Result := 0;
end;

function TDASQLTimeStampOffsetField.GetAsVariant: Variant;
var
  D: TSQLTimeStampOffset;
{$IFNDEF VER14P}
  TS: TSQLTimeStamp;
{$ENDIF}
begin
  if GetValue(D) then begin
  {$IFDEF VER14P}
    Result := VarSQLTimeStampOffsetCreate(D);
  {$ELSE}
    TS := ConvertFromUTC(D);
    Result := VarSQLTimeStampCreate(TS);
  {$ENDIF}
  end
  else
    Result := Null;
end;

function TDASQLTimeStampOffsetField.GetAsSQLTimeStampOffset: TSQLTimeStampOffset;
begin
  if not GetValue(Result) then Result := NullSQLTimeStampOffset;
end;

function TDASQLTimeStampOffsetField.GetDataSize: Integer;
begin
  Result := SizeOf(TSQLTimeStampOffset);
end;

procedure TDASQLTimeStampOffsetField.GetText(var Text: string;
  DisplayText: Boolean);
var
  F: string;
  D: TSQLTimeStampOffset;
  TS: TSQLTimeStamp;
  OffsetStr: string;
begin
  if GetValue(D) then begin
    if DisplayText and (DisplayFormat <> '') then
      F := DisplayFormat
    else
      F := '';

    TS := ConvertFromUTC(D);
    DateTimeToString(Text, F, TS, FormatSettings);

    //something to append the timezone time
    TS.Hour := Abs(D.TimeZoneHour);
    TS.Minute := D.TimeZoneMinute;
    TS.Second := 0;
    DateTimeToString(OffsetStr, 'hh:nn', TS, FormatSettings);
    if D.TimeZoneHour < 0 then
      Text := Text + ' -' + OffsetStr
    else
      Text := Text + ' +' + OffsetStr;
  end
  else
    Text := '';
end;

procedure TDASQLTimeStampOffsetField.SetAsDateTime(Value: TDateTime);
begin
  SetAsSQLTimeStampOffset(DateTimeToSQLTimeStampOffset(Value));
end;

procedure TDASQLTimeStampOffsetField.SetAsString(const Value: string);
begin
  if Value = '' then
    Clear
  else
    SetAsSQLTimeStampOffset(CRTimeStamp.StrToSQLTimeStampOffset(Value));
end;

procedure TDASQLTimeStampOffsetField.SetAsSQLTimeStampOffset(
  const Value: TSQLTimeStampOffset);
begin
  SetData(BytesOf(@Value, SizeOf(TSQLTimeStampOffset)), True);
end;

procedure TDASQLTimeStampOffsetField.SetVarValue(const Value: Variant);
var
  TSO: TSQLTimeStampOffset;
{$IFNDEF VER14P}
  TS: TSQLTimeStamp;
{$ENDIF}
begin
  if VarIsClear(Value) then
    DatabaseError(SUnassignedVar);
{$IFDEF VER14P}
  TSO := VarToSqlTimeStampOffset(Value);
{$ELSE}
  TS := VarToSqlTimeStamp(Value);
  Move(TS, TSO, SizeOf(TSqlTimeStamp));
  TSO.TimeZoneHour := 0;
  TSO.TimeZoneMinute := 0;
{$ENDIF}
  SetAsSQLTimeStampOffset(TSO);
end;
{$ENDIF}
{$ENDIF}

end.
