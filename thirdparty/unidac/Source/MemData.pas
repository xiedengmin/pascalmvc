
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Mem Data
//  Created:            20.02.98
//////////////////////////////////////////////////

{$I Dac.inc}
unit MemData;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
{$IFDEF NEXTGEN}
  Generics.Collections, Generics.Defaults,
{$ENDIF}
{$IFDEF POSIX}
  Posix.String_,
{$ENDIF}
  CLRClasses,
  CRTypes, CRFunctions, CRTimeStamp, CRParser, MemUtils;

const
  btSign = $DD;   // DEBUG
  flUsed = $EE;
  flFree = $DD;

  FlatBufferLimit = 32;

{ internal data types }

{ ! can't modify this consts }

  dtUnknown       = 0;
  dtString        = 1;
  dtExtString     = 2;
  dtWideString    = 3;
  dtExtWideString = 4;
  dtFixedChar     = 5;
  dtFixedWideChar = 6;
  dtInt8          = 7;
  dtInt16         = 8;
  dtSmallint      = dtInt16;
  dtInt32         = 9;
  dtInteger       = dtInt32;
  dtInt64         = 10;
  dtLargeint      = dtInt64;
  dtUInt8         = 11;
  dtByte          = dtUInt8;
  dtUInt16        = 12;
  dtWord          = dtUInt16;
  dtUInt32        = 13;
  dtLongWord      = dtUInt32;
  dtUInt64        = 14;
  dtSingle        = 15;
  dtFloat         = 16;
  dtExtended      = 17;
  dtCurrency      = 18;
  dtBCD           = 19;
  dtFMTBCD        = 20;
  dtDate          = 21; // Date only
  dtTime          = 22; // Time only
  dtDateTime      = 23; // Date and time
  dtSQLTimeStamp  = 24;
  dtSQLTimeStampOffset = 25;
  dtBoolean       = 26;
  dtBytes         = 27;
  dtVarBytes      = 28; /// Cannot be deleted because "Fixed" flag not avaible on component level (MSAccess) GetFieldType(DataType: word): TFieldType
  dtExtBytes      = 29; // for DataTypeMapping only !!!
  dtExtVarBytes   = 30;
  dtBlob          = 31;
  dtMemo          = 32;
  dtWideMemo      = 33; //This type corectly supported only in BDS 2006 and higher
  dtVariant       = 34;
  dtObject        = 35;
  dtReference     = 36;
  dtArray         = 37;
  dtTable         = 38;
  dtGuid          = 39;
  dtCursor        = 40;
  dtXML           = 41;

{ StringHeap const }

const
  BlockSize = 16384;
  SmallSize = 2000;
  Align = 8;
  RefNull = 101;

  SizeOfTBcd = SizeOf(TBcd);

type
  TDataType = Word;

  TDANumericType = (ntFloat, ntBCD, ntFmtBCD);

  //Note that TConnLostCause should be ordered by FailOver priority
  //e.g. there are multyple DataSet.ApplyUpdates during Connection.ApplyUpdates so Connection.ApplyUpdates is more
  //prioritized operation than DataSet.ApplyUpdates and should be reexecuted instead of DataSet.ApplyUpdates in case of
  //failover
  TConnLostCause = (clUnknown,  //Connection lost reason - unknown
    clExecute,                  //Connection Lost detected during SQL execution (Reconnect with exception possible)
    clOpen,                     //Connection Lost detected during query opening (Reconnect/Reexecute possible)
    clRefresh,                  //Connection Lost detected during query opening (Reconnect/Reexecute possible)
    clApply,                    //Connection Lost detected during DataSet.ApplyUpdates (Reconnect/Reexecute possible)
    clServiceQuery,             //Connection Lost detected during service information request (Reconnect/Reexecute possible)
    clTransStart,               //Connection Lost detected during transaction start (Reconnect/Reexecute possible)
                                //In IBDAC one connection could start several transactions during ApplyUpdates that's why
                                //clTransStart has less priority then clConnectionApply
    clConnectionApply,          //Connection Lost detected during Connection.ApplyUpdates (Reconnect/Reexecute possible)
    clConnect                   //Connection Lost detected during connection establishing (Reconnect possible)
    );

  TLocateExOption = (lxCaseInsensitive, lxPartialKey, lxNearest, lxNext, lxUp, lxPartialCompare{,lxCharCompare});
  TLocateExOptions = set of TLocateExOption;

  TCompareOption = (coCaseInsensitive, coPartialKey, coPartialCompare, coOrdinalCompare, coInvertNullOrder);
  TCompareOptions = set of TCompareOption;

  TReorderOption = (roInsert, roDelete, roFull);

  TSortType = (stCaseSensitive, stCaseInsensitive, stBinary);

{ TBlockManager }

  PBlockHeader = ^TBlockHeader;
  TBlockHeader = packed record
    ItemCount: Word;
    UsedItems: Word;
    Prev: PBlockHeader;
    Next: PBlockHeader;
  end;

  TItemStatus = (isUnmodified, isUpdated, isAppended, isDeleted);
  TItemTypes = set of TItemStatus;
  TUpdateRecAction = (urFail, urAbort, urSkip, urRetry, urApplied, urNone, urSuspended);
  TItemFilterState = (fsNotChecked, fsNotOmitted, fsOmitted);

  PItemHeader = ^TItemHeader;
  TItemHeader = packed record
    Block: PBlockHeader;
    Prev: PItemHeader;
    Next: PItemHeader;
    Rollback: PItemHeader;
    Status: TItemStatus;
    UpdateResult: TUpdateRecAction;
    Order,
    SavedOrder: integer;
    Flag: byte;
    FilterResult: TItemFilterState;
  {$IFNDEF FPC}
  {$IFOPT Z+}{$ELSE}
    AlignByte: byte; // for align struct size only: SizeOf(TItemHeader) mod 2 = 0
  {$ENDIF}
  {$ENDIF}
  end;

  TBlockManager = class
  private
    RecordSize: Integer;

    procedure AddFreeBlock;
    procedure FreeAllBlocks;

  public
    FirstFree: PItemHeader;
    FirstBlock: PBlockHeader;
    DefaultItemCount: Word;

    constructor Create;
    destructor Destroy; override;

    procedure AllocBlock(out Block: PBlockHeader; ItemCount: Word; StandAloneBlock: Boolean = False);
    procedure ReAllocBlock(var Block: PBlockHeader; ItemCount: Word);
    procedure FreeBlock(Block: PBlockHeader; StandAloneBlock: Boolean = False);

    procedure AllocItem(out Item: PItemHeader);
    procedure FreeItem(Item: PItemHeader);

    procedure PutRecord(Item: PItemHeader; Rec: IntPtr);
    procedure GetRecord(Item: PItemHeader; Rec: IntPtr);
    function GetRecordPtr(Item: PItemHeader): IntPtr;

    procedure CopyRecord(ItemSrc: PItemHeader; ItemDest: PItemHeader);
  end;

{ TStringHeap }

  PBlock = ^TBlock;
  TStrData = array [0..BlockSize - 5 {SizeOf(PBlock) - 1}] of byte;
  TBlock = packed record
    Next: PBlock;
    Data: TStrData;
  end;

  TSmallTab = array [1..SmallSize div Align] of IntPtr;

type
  TStringHeap = class
  private
    FSmallTab: TSmallTab;
    FFree: integer;
    FRoot: PBlock;
    FEmpty: boolean;
    FSysGetMem: boolean;
  {$IFDEF WIN32}
    FUseSysMemSize: boolean;
  {$ENDIF}
    FThreadSafety: boolean;
    FThreadSafetyCS: TCriticalSection;
    procedure SetThreadSafety(const Value: boolean);
    function UseSmallTabs(divSize: integer): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function NewBuf(Size: Integer): IntPtr;
    function AllocStr(Str: IntPtr; Len: integer = -1): IntPtr;
    function AllocTrimmedStr(Str: IntPtr; var Len: Integer): IntPtr;
    function AllocWideStr(Str: IntPtr; Len: integer = -1): IntPtr;
    function AllocTrimmedWideStr(Str: IntPtr; var Len: Integer): IntPtr;
    procedure DisposeBuf(Buf: IntPtr);
    procedure AddRef(Buf: IntPtr);
    procedure Clear;
    property Empty: boolean read FEmpty;
    property SysGetMem: boolean read FSysGetMem;
    property ThreadSafety: boolean read FThreadSafety write SetThreadSafety;
  end;

{ TFieldDesc }

  TFieldTypeSet = set of byte;

  TDateFormat = (dfMSecs, dfDateTime, dfTime, dfDate);

  TFieldDescKind = (fdkData, fdkCached, fdkCalculated);

  TObjectType = class;
  TFieldDesc = class;
  TData = class;
  TRecordSetClass = class of TData;

  // !!! Sync with CRDataTypeMap.ConvertStatusErrors and ODBC_Common.SQLStateOfConvertStatus
  TConvertStatus = (csSuccess,
    csBinaryTruncated{01004}, csStringTruncated{01004},
    csDataTruncated{01S07}, csFractionTruncated{01S07},
    csInvalidBinaryValue{07006}, csInvalidBlobValue{07006}, csInvalidDataMapping{07006},
    csInvalidValueScale{22003}, csValueOverflow{22003}, csValueOutOfRange{22003},
    csInvalidBooleanValue{22018}, csInvalidGUIDValue{22018}, csInvalidIntervalValue{22018},
    csInvalidDateTimeValue{22018}, csInvalidSQLTimeStampValue{22018},
    csInvalidIntegerValue{22018}, csInvalidNumericValue{22018}
  );

  TConvertInfo = record
    StringHeap: TStringHeap;
    Source: IntPtr;
    SourceOffset: Integer;
    SourceLen: Integer;
    SourceScale: Integer;
    Dest: IntPtr;
    DestOffset: Integer;
    DestLen: Integer;
    DestScale: Integer;
    IgnoreConvertErrors: Boolean;
    Format: string;
  end;

  TConvertProcedure = function (var ConvertInfo: TConvertInfo): TConvertStatus of object;

  TFieldConstraint = class;

  TFieldDesc = class
  private
    FRecordSetClass: TRecordSetClass;
    FDataType: Word;
    FSubDataType: Word;
    FObjectType: TObjectType;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FParentField: TFieldDesc;
    FUpdateable: boolean;
    FCustomConstraint: TFieldConstraint;

    procedure SetObjectType(Value: TObjectType);
    procedure SetParentField(Value: TFieldDesc);
  protected
    FName: string;       // unique name in TData
    FActualName: string; // original name from source
    FLength: Word;       // precision for number
    FScale: Word;
    FFieldNo: Word;
    FActualFieldNo: integer;
    FSize: Integer;     // size in rec buffer (changed word -> LongWord for Oracle varray(32767) of varchar2(32767) in the Unicode)
    FOffset: Integer;   // offset in rec buffer
    FDataOffset: Integer; // if HasValueLen then FDataOffset := FOffset + SizeOf(Word)
    FRequired: boolean;
    FReadOnly: boolean;
    FIsKey: boolean;
    FFixed: boolean;     // indicates that the string field has a fixed size
    FHidden: boolean;
    FHiddenObject: boolean;  // for hide Object field (child field is visible)
    FHandle: IntPtr;     // pointer to field specific data
    FReserved: boolean;  // reserved flag for perfomance optimization
    FFieldDescKind: TFieldDescKind;
    FIsAutoIncrement: boolean;
    FIsBlob: boolean;    // for performance tweaks
    FIsObject: boolean;  // for performance tweaks
    FIsSharedObject: boolean;  // for performance tweaks
    FIsComplex: boolean; // for performance tweaks
    FHasValueLen: boolean; // for store value length
    FHasParent: boolean; // for performance tweaks

    function GetMapLength: Integer; virtual;
    function GetMapDataType: Word; virtual;
    procedure SetDataType(Value: Word); virtual;
    procedure SetSubDataType(Value: Word); virtual;

    property RecordSetClass: TRecordSetClass read FRecordSetClass;
  public
    constructor Create(RecordSetClass: TRecordSetClass); virtual;
    destructor Destroy; override;

    procedure Assign(FieldDesc: TFieldDesc); virtual;

    property Name: string read FName write FName;
    property ActualName: string read FActualName write FActualName;
    property DataType: Word read FDataType write SetDataType;
    property SubDataType: Word read FSubDataType write SetSubDataType;
    property Length: Word read FLength write FLength;
    property Scale: Word read FScale write FScale;
    property FieldNo: Word read FFieldNo write FFieldNo;
    property ActualFieldNo: integer read FActualFieldNo write FActualFieldNo; // for define
    property Size: Integer read FSize write FSize;
    property Offset: Integer read FOffset write FOffset;
    property DataOffset: Integer read FDataOffset write FDataOffset;
    property Required: boolean read FRequired write FRequired;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property IsKey: boolean read FIsKey write FIsKey;
    property Fixed: boolean read FFixed write FFixed;
    property Hidden: boolean read FHidden write FHidden;
    property ObjectType: TObjectType read FObjectType write SetObjectType;
    property ParentField: TFieldDesc read FParentField write SetParentField;
    property HiddenObject: boolean read FHiddenObject write FHiddenObject; // IncludeObject
    property Handle: IntPtr read FHandle write FHandle;
    property FieldDescKind: TFieldDescKind read FFieldDescKind write FFieldDescKind;
    property IsAutoIncrement: boolean read FIsAutoIncrement write FIsAutoIncrement;
    property Updateable: boolean read FUpdateable write FUpdateable; // can be used in queries for data update
    property MapDataType: Word read GetMapDataType;
    property MapLength: Integer read GetMapLength;
    property IsBlob: boolean read FIsBlob;
    property IsObject: boolean read FIsObject;
    property IsSharedObject: boolean read FIsSharedObject;
    property IsComplex: boolean read FIsComplex;
    property HasValueLen: boolean read FHasValueLen;
    property HasParent: boolean read FHasParent;
    property CustomConstraint: TFieldConstraint read FCustomConstraint write FCustomConstraint;
  end;

  TFieldDescClass = class of TFieldDesc;

  TFieldDescs = class (TCRObjectList)
  private
    function GetItems(Index: Integer): TFieldDesc;
  public
    function FindField(const Name: string): TFieldDesc;
    function FieldByName(const Name: string): TFieldDesc;
    function FieldByActualFieldNo(ActualFieldNo: integer): TFieldDesc;

    property Items[Index: Integer]: TFieldDesc read GetItems; default;
  end;

{ TSortColumns }

  TSortColumn = class
  public
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FieldDesc: TFieldDesc;
    DescendingOrder: boolean;
    SortType: TSortType;
    UseForRangeStart: boolean;
    UseForRangeEnd: boolean;
  end;

{ TSharedObject }

  TSharedObject = class
  protected
    FRefCount: Integer;
    FGCHandle: IntPtr;

    function GetGCHandle: IntPtr;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Free;

    function ToVariant: Variant;
    class function FromVariant(const Source: Variant): TSharedObject;

    procedure CheckValid;

    procedure AddRef;
    procedure Release;
  {$IFNDEF VER12P}
  {$IFNDEF FPC}
    function GetHashCode: NativeInt;
  {$ENDIF}
  {$ENDIF}
    procedure Disconnect; virtual;

    property RefCount: Integer read FRefCount;
    property GCHandle: IntPtr read GetGCHandle;
  end;

{ TObjectType }

  TAttribute = class
  private
    FName: string;
    FDataType: Word;
    FSubDataType: Word;
    FLength: Word;
    FScale: Word;
    FSize: Integer;   // size of got data (changed word -> LongWord for Oracle varray(32767) of varchar2(32767) in the Unicode)
    FDataSize: Integer;   // size of stored data
    FOffset: Integer;     // stored offset
    FIndicatorOffset: Integer;  // indicator offset
    FAttributeNo: Word;
    FObjectType: TObjectType;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TObjectType;
    FFixed: boolean;

    procedure SetObjectType(Value: TObjectType);
  protected
    function GetActualName: string; virtual;
    procedure SetActualName(const Value: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property ActualName: string read GetActualName write SetActualName;
    property DataType: Word read FDataType write FDataType;
    property SubDataType: Word read FSubDataType write FSubDataType;
    property Fixed: boolean read FFixed write FFixed;
    property Length: Word read FLength write FLength;
    property Scale: Word read FScale write FScale;
    property Size: Integer read FSize write FSize;
    property DataSize: Integer read FDataSize write FDataSize;
    property Offset: Integer read FOffset write FOffset;
    property IndicatorOffset: Integer read FIndicatorOffset write FIndicatorOffset;
    property AttributeNo: Word read FAttributeNo write FAttributeNo;
    property ObjectType: TObjectType read FObjectType write SetObjectType;
    property Owner: TObjectType read FOwner write FOwner;
  end;

  TAttributeChain = class
  private
    FAttribute: TAttribute;
    FIndex: Integer;

    FPrev: TAttributeChain;
    FNext: TAttributeChain;

    function GetAsString: string;
  public
    constructor Create(Attribute: TAttribute; Prev: TAttributeChain; Index: Integer);
    destructor Destroy; override;

    function First: TAttributeChain;
    function Last: TAttributeChain;

    property Attribute: TAttribute read FAttribute;
    property Index: Integer read FIndex;
    property Prev: TAttributeChain read FPrev;
    property Next: TAttributeChain read FNext;

    property AsString: string read GetAsString;
  end;

  TObjectType = class (TSharedObject)
  private
    function GetAttributes(Index: integer): TAttribute;
    function GetAttributeCount: integer;
  protected
    FName: string;
    FDataType: Word;
    FSize: Integer;
    FAttributes: TCRObjectList;

    procedure ClearAttributes;
    function ParseAttribute(Name: string): TAttributeChain;
  public
    constructor Create;
    destructor Destroy; override;

    function FindAttribute(const Name: string): TAttribute; virtual;
    function GetAttribute(const Name: string): TAttribute;
    function GetAttributeChain(const Name: string): TAttributeChain;

    property Name: string read FName;
    property DataType: Word read FDataType;
    property Size: Integer read FSize;
    property Attributes[Index: integer]: TAttribute read GetAttributes;
    property AttributeCount: Integer read GetAttributeCount;
  end;

  TDBObject = class (TSharedObject)
  private
    FObjectType: TObjectType;

  protected
    procedure SetObjectType(Value: TObjectType); virtual;

    function GetIsNull: boolean; virtual; abstract;

    procedure GetAttributeValue(const Name: string; out AttrBuf: IntPtr; out AttrLen: Word; out IsBlank, NativeBuffer: boolean); virtual;
    procedure SetAttributeValue(const Name: string; ValuePtr: IntPtr; ValueLen: Word); virtual;
    function GetAttrIsNull(const Name: string): boolean; virtual;

  public
    constructor Create;

    property IsNull: boolean read GetIsNull;
    property ObjectType: TObjectType read FObjectType;
  end;

  TCacheItem = class
  private
    Item: PItemHeader;
    Restore: PItemHeader;
    Next: TCacheItem;
  end;

  PRecBookmark = ^TRecBookmark;
  TRecBookmark = record
    RefreshIteration: Integer;
    Item: PItemHeader;
    Order: Integer;
  end;

  TFilterFunc = function(RecBuf: IntPtr): boolean of object;

  TBoolParser = class (TParser)
  private
    FOmitStringQuote: boolean;
  protected
    function IsStringQuote(Ch: char): boolean; override;
    procedure ToRightQuote(LeftQuote: char); override;
  public
    constructor Create(const Text: string); override;

    property OmitStringQuote: boolean read FOmitStringQuote write FOmitStringQuote;
  end;

  TExpressionType = (ntEqual, ntMore, ntLess, ntMoreEqual, ntLessEqual, ntNoEqual,
    ntAnd, ntOr, ntNot, ntField, ntValue, ntTrue, ntFalse, ntLike, ntNotLike, ntIn, ntNotIn, ntLower, ntUpper, ntBetween, ntNotBetween,
    // for TVirtualQuery
    ntMatch, ntGlob, ntRegExp);

  TExpressionNode = class
  public
    NextAlloc: TExpressionNode;
    NodeType: TExpressionType;
    LeftOperand: TExpressionNode;
    RightOperand: TExpressionNode;
    NextOperand: TExpressionNode;
    FieldDesc: TFieldDesc; // used only when TExpressionType = ntField
    Value: variant;
    UseCalculatedFields: boolean; // speed optimization
  end;

  TCondition = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FData: TData;
    FExpression: TExpressionNode;
    FFirstAlloc: TExpressionNode;
    FParser: TBoolParser;
    FCode: Integer;
    FStrLexem: string;
    FText: string;

  public
    constructor Create(Data: TData; const Text: string);
    destructor Destroy; override;

    procedure ExpressionError; virtual; abstract;
    function GetField(const FieldName: string): TFieldDesc; virtual;
    function AllocNode: TExpressionNode;
    function OrExpr: TExpressionNode;
    function AndExpr: TExpressionNode;
    function Condition: TExpressionNode;
    function Argument: TExpressionNode;

    procedure CreateExpression; virtual;
    procedure FreeExpression; virtual;

    property Data: TData read FData write FData;
    property Text: string read FText write FText;
    property Expression: TExpressionNode read FExpression;
  end;

  TFilter = class (TCondition)
  public
    procedure ExpressionError; override;
  end;

  TConstraint = class (TCondition)
    FErrorMessage: string;
    FAlias: string;
    FComplexConstraint: Boolean;
  public
    constructor Create(Data: TData; const Text: string; const ConstraintErrorMessage: string; UseCreateExpression: boolean = False);

    procedure ExpressionError; override;
    procedure ConstraintError;

    procedure CreateExpression; override;
    procedure FreeExpression; override;

    procedure EmptyConstraint; virtual;
    procedure UpdateConstraint(const Text: string; const ConstraintErrorMessage: string);

    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property ComplexConstraint: Boolean read FComplexConstraint;
  end;

  TFieldConstraint = class (TConstraint)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FField: TFieldDesc;
  public
    constructor Create(Data: TData; const Text: string; const ConstraintErrorMessage: string; Field: TFieldDesc; UseCreateExpression: boolean = False);

    function GetField(const FieldName: string): TFieldDesc; override;
    procedure EmptyConstraint; override;
    procedure UpdateConstraint(const Text: string; const ConstraintErrorMessage: string; Field: TFieldDesc);

    property Field: TFieldDesc read FField write FField;
  end;

  TBlob = class;

{ TData }

  TUpdateRecKind = (ukUpdate, ukInsert, ukDelete);
  TUpdateRecKinds = set of TUpdateRecKind;
  TOnModifyRecord = procedure of object;
  TOnApplyRecord = procedure (UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean) of object;
  TOnGetCachedFields = procedure of object;
  TOnGetCachedBuffer = procedure(Buffer: IntPtr; Source: IntPtr = nil) of object;
  TOnFieldsChanged = procedure of object;

  TData = class
  private
    FActive: boolean;
    FPrepared: boolean;
    FCachedUpdates: boolean;
    FLocalUpdate: boolean;
    FInCacheProcessing: boolean;
    FNewCacheRecBuf: IntPtr;
    FOldCacheRecBuf: IntPtr;
    FOnAppend: TOnModifyRecord;
    FOnDelete: TOnModifyRecord;
    FOnUpdate: TOnModifyRecord;
    FOnApplyRecord: TOnApplyRecord;
    FOnCacheChanged: TOnModifyRecord;
    FOnCacheApplied: TOnModifyRecord;
    FOnCacheCanceled: TOnModifyRecord;
    FAfterApplyUpdates: TOnModifyRecord;
    FAutoInitFields: boolean;
    FTrimFixedChar: boolean;
    FTrimVarChar: boolean;
    FOnFieldsChanged: TOnFieldsChanged;

  { Filter }
    FFilterFunc: TFilterFunc;
    FFilterMDFunc: TFilterFunc;
    FFilterRangeFunc: TFilterFunc;
    FFilterCaseInsensitive: boolean;
    FFilterNoPartialCompare: boolean;
    FFilterItemTypes: TItemTypes;
    FFilterUseRollBack: boolean;
    FRecordSearch: Boolean;

    FParser: TBoolParser;
    FCode: Integer;
    FFilterCondition: TCondition;
    FConstraints: TCRObjectList;
    FFieldConstraints: TCRObjectList;

    FHasBlobFields: boolean;
    FHasComplexFields: boolean;
    FSparseArrays: boolean;

    FOnGetCachedFields: TOnGetCachedFields;
    FOnGetCachedBuffer: TOnGetCachedBuffer;

    function GetFilterExpression: TExpressionNode;

    function InternalCompareFields(DataBuf: IntPtr; DataLen: Word; IsBlank: boolean; DataType: integer;
      RecBuf: IntPtr; FieldDesc: TFieldDesc; const Options: TCompareOptions; Mapped: boolean): integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure SetCachedUpdates(Value: boolean);
  protected
    FDataSize: Integer; // size of data
    FRecordSize: Integer;
    FCalcDataSize: Integer;
    FCalcRecordSize: Integer;
    FilterRecBuf: IntPtr;

    FStringHeap: TStringHeap;

    FSetEmptyStrToNull: boolean;
    /// if True then PutField set Null for string fields with empty value ('')
    FRequireEmptyStrToNull: boolean;

    FRecordNoOffset: Integer;
    FRecordCount: Integer;
    FBOF: boolean;
    FEOF: boolean;

    FFields: TFieldDescs;

    procedure SetTrimFixedChar(Value: Boolean); virtual;
    procedure SetTrimVarChar(Value: Boolean); virtual;

  { Open/Close }
    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure InternalOpen(DisableInitFields: boolean = False); virtual;
    procedure InternalClose; virtual;

  { Data }
    procedure InitData; virtual;
    procedure FreeData; virtual;
    procedure CheckFetched(RecBuf: IntPtr; Field: TFieldDesc); virtual;

  { Fields }
    procedure InitRecordSize; virtual;
    procedure InitCalcDataSize;
    procedure CreateFieldDescs; virtual;
    procedure InitObjectFields(ObjectType: TObjectType; Parent: TFieldDesc);
    function InternalGetObject(Field: TFieldDesc; RecBuf: IntPtr): TSharedObject; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function InternalGetObject(DataBuf: IntPtr): TSharedObject; overload; {$IFDEF USE_INLINE}inline;{$ENDIF} // virtual;
    procedure InternalSetObject(DataBuf: IntPtr; Obj: TSharedObject); {$IFDEF USE_INLINE}inline;{$ENDIF} // virtual;
    function GetArrayFieldName(ObjectType: TObjectType; ItemIndex: integer): string; virtual;
    function InternalCompareFieldValue(ValuePtr: IntPtr; ValueLen: Word; ValueType: Word; DataBuf: IntPtr; DataLen: Word; FieldType: Word; HasParent: boolean; IsFixed: boolean; const Options: TCompareOptions): integer; virtual;

    function GetIndicatorItemSize: Integer; virtual;
    function GetIndicatorSize: Integer; virtual;

    procedure GetChildFieldInfo(Field: TFieldDesc; out RootField: TFieldDesc; out AttrName: string);
    procedure GetChildField(Field: TFieldDesc; RecBuf: IntPtr; out DataBuf: IntPtr; out DataLen: Word; out IsBlank, NativeBuffer: boolean); virtual;
    procedure PutChildField(Field: TFieldDesc; RecBuf: IntPtr; ValuePtr: IntPtr; ValueLen: Word);
    function GetChildFieldIsNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; virtual;

  { Navigation }
    function GetEOF: boolean; virtual;
    function GetBOF: boolean; virtual;

    function GetRecordCount: Integer; virtual;
    function GetRecordNo: Integer; virtual;
    procedure SetRecordNo(Value: Integer); virtual;

  { Edit }
    procedure InternalAppend(RecBuf: IntPtr); virtual;
    procedure InternalDelete; virtual;
    procedure InternalUpdate(RecBuf: IntPtr); virtual;

  { Filter }
    function Filtered: boolean;
    function GetFilterText: string;
    procedure SetFilterText(const Value: string); virtual;
    function InternalAnsiStrComp(const Value1, Value2: IntPtr;
      const Options: TCompareOptions): integer;
    function InternalAnsiCompareText(const Value1, Value2: AnsiString;
      const Options: TCompareOptions): integer;
    function InternalWStrLComp(const Value1, Value2: WideString;
      const Options: TCompareOptions): integer;
    function InternalWStrComp(const Value1, Value2: WideString;
      const Options: TCompareOptions): integer;
    function CompareStrValues(const Value: AnsiString; const FieldValue: AnsiString;
      const Options: TCompareOptions): integer;
    function CompareWideStrValues(const Value: WideString; const FieldValue: WideString;
      const Options: TCompareOptions): integer;
    function CompareBinValues(const Value: IntPtr; const ValueLen: Integer;
      const FieldValue: IntPtr; const FieldValueLen: Integer; const Options: TCompareOptions): integer;

  { CachedUpdates }
    procedure InternalCacheChanged; virtual;
    procedure InternalCacheApplied; virtual;
    procedure InternalCacheCanceled; virtual;
    function GetUpdatesPending: boolean; virtual;
    procedure SetFilterItemTypes(const Value: TItemTypes); virtual;

  public
    constructor Create;
    destructor Destroy; override;

    // for TVirtualQuery
    function Eval(Node: TExpressionNode): boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function Eval(Node: TExpressionNode; ConstraintField: TFieldDesc; ValuePtr: IntPtr; ValueLen: Word): boolean; overload;

  { Open/Close }
    procedure Open;
    procedure Close;

    procedure Prepare; virtual;
    procedure UnPrepare; virtual;

    function IsFullReopen: boolean; virtual;
    procedure Reopen; virtual;
    function NeedConvertEOL: boolean; virtual;

  { Fields }
    class function GetBufferSize(DataType: Word; DataLen: integer): integer; virtual;
    function GetFieldDescType: TFieldDescClass; virtual;
    function CreateFieldDesc: TFieldDesc;
    procedure InitFields; virtual;
    procedure ExplicitInitFields; virtual;
    procedure ClearFields; virtual;
    procedure InternalInitFieldDescs; virtual;
    function IsEqualDataType(Field1: TFieldDesc; Field2: TFieldDesc): boolean;
    procedure GetField(Field: TFieldDesc; RecBuf: IntPtr; Dest: IntPtr; out DestLen: Word; NeedConvert: boolean; out IsBlank: boolean); virtual;
    procedure GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean); virtual;
    function GetDataBuf(RecBuf: IntPtr; FieldDesc: TFieldDesc; out DataLenPtr: PWord): IntPtr; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetFieldBuf(RecBuf: IntPtr; FieldDesc: TFieldDesc; out DataLen: Word; out IsBlank, NativeBuffer: boolean): IntPtr; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetMappedFieldBuf(RecBuf: IntPtr; FieldDesc: TFieldDesc; out DataLen: Word; out DataType: Word; out HasParent: boolean; out IsFixed: boolean; out IsBlank, NativeBuffer: boolean): IntPtr;
    function GetMappedDataBuf(FieldDesc: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; var DataType: Word; var HasParent, IsFixed: boolean): IntPtr; virtual;
    procedure PutField(Field: TFieldDesc; RecBuf: IntPtr; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False); virtual;
    procedure PutFieldData(Field: TFieldDesc; DataBuf: IntPtr; DataLenPtr: PWord; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False); virtual;
    function GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; virtual;
    procedure SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean); virtual;
    function GetNullByBlob(Field: TFieldDesc; RecBuf: IntPtr): boolean;
    function GetChanged(Field: TFieldDesc; RecBuf: IntPtr): boolean; virtual;
    procedure SetChanged(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean); virtual;

    procedure GetFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: Variant; UseRollback: boolean = False); virtual;
    procedure GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType, SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean); virtual;
    procedure GetMappedFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: Variant; UseRollback: boolean = False; FlatRecBuf: boolean = False); virtual;
    procedure GetMappedDataAsVariant(Field: TFieldDesc; DataBuf: IntPtr; DataLen: Word; var Value: variant; UseRollback: boolean = False; FlatRecBuf: boolean = False); virtual;
    procedure PutFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; const Value: Variant; IsDatabaseValue: boolean = False); virtual;
    procedure PutDataAsVariant(DataBuf: IntPtr; DataLenPtr: PWord; DataType: Word; Len, Scale: Word; HasParent: boolean; const Value: Variant; IsDatabaseValue: boolean); virtual;

    class procedure GetDateFromBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat); virtual;
    class procedure PutDateToBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat); virtual;

    function FindField(const Name: string): TFieldDesc;
    function FieldByName(const Name: string): TFieldDesc;

    class function IsBlobDataType(DataType: word): boolean; virtual; // TBlob descendants - dtBlob, dtMemo etc
    class function IsObjectDataType(DataType: word): boolean; virtual;
    class function IsSharedObjectDataType(DataType: word): boolean; virtual;
    class function IsComplexDataType(DataType: word): boolean; virtual; // All supported complex field types (BlobFieldTypes, ExtFieldTypes and TSharedObject descendants (not BLOB))
    class function HasValueLen(DataType: word): boolean; virtual;

    function HasFields(FieldTypes: TFieldTypeSet): boolean;
    function CheckHasBlobFields: boolean;
    function CheckHasComplexFields: boolean;
    function FieldListDependsOnParams: boolean; virtual;

  { Records }
    procedure AllocRecBuf(out RecBuf: IntPtr);
    procedure FreeRecBuf(RecBuf: IntPtr);

    procedure InitRecord(RecBuf: IntPtr); virtual;
    //procedure FreeRecord(RecBuf: pointer);
    procedure GetRecord(RecBuf: IntPtr); virtual; abstract;
    procedure GetNextRecord(RecBuf: IntPtr); virtual; abstract;
    procedure GetPriorRecord(RecBuf: IntPtr); virtual; abstract;
    procedure PutRecord(RecBuf: IntPtr); virtual; abstract;
    procedure AppendRecord(RecBuf: IntPtr); virtual; abstract;
    procedure AppendBlankRecord;
    procedure InsertRecord(RecBuf: IntPtr); virtual; abstract;
    procedure UpdateRecord(RecBuf: IntPtr); virtual; abstract; // Modify
    procedure DeleteRecord; virtual; abstract;

    procedure EditRecord(RecBuf: IntPtr); virtual;
    procedure PostRecord(RecBuf: IntPtr); virtual;
    procedure CancelRecord(RecBuf: IntPtr); virtual;

    procedure CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean);
    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); virtual;
    procedure AddRefComplexFields(RecBuf: IntPtr; CreateBlob: boolean = False);
    procedure FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean);
    procedure FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc); virtual;
    procedure CopyComplexFields(SourceRecBuf, DestRecBuf: IntPtr; WithBlob: boolean); // copy content ComplexFields
    procedure CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc); virtual;
    procedure AddRef(RecBuf: IntPtr); virtual;
    procedure ReleaseRef(RecBuf: IntPtr; IsResync: boolean; WithBlob: boolean); virtual;

  { Navigation }
    procedure SetToBegin; virtual;
    procedure SetToEnd; virtual;

  { BookMarks }
    procedure GetBookmark(Bookmark: PRecBookmark); virtual;
    procedure SetToBookmark(Bookmark: PRecBookmark); virtual;
    function BookmarkValid(Bookmark: PRecBookmark): boolean; virtual;
    function CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer; virtual;
    function NeedGetRecordAfterGotoBookmark: boolean; virtual;

  { CachedUpdates }
    function GetUpdateStatus: TItemStatus; virtual;
    function GetUpdateResult: TUpdateRecAction; virtual;
    function HasUpdatedOrDeletedRecords: boolean; virtual;

    procedure SetCacheRecBuf(NewBuf: IntPtr; OldBuf: IntPtr); virtual;
    procedure ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds); virtual;
    procedure CommitUpdates; virtual;
    procedure CancelUpdates; virtual;
    procedure RestoreUpdates; virtual;
    procedure RevertRecord; virtual;

    procedure ApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean); virtual;
    procedure DoAfterApplyUpdates;

    procedure GetOldRecord(RecBuf: IntPtr); virtual; // get rollback data

  { Constraint }
    procedure CheckConstraint(Field: TFieldDesc; RecBuf: IntPtr; ValuePtr: IntPtr; ValueLen: Word; FieldConstraint: TFieldConstraint);
    procedure CheckConstraints(RecBuf: IntPtr; Constraints: TCRObjectList);

  { Filter }
    procedure FilterUpdated; virtual;
    function CompareFieldValue(ValuePtr: IntPtr; ValueLen: Word; ValueType: Word; FieldDesc: TFieldDesc; RecBuf: IntPtr; const Options: TCompareOptions; Mapped: boolean): integer;
    function GetSortOptions(SortColumn: TSortColumn): TCompareOptions; virtual;
    function CompareFields(RecBuf1: IntPtr; RecBuf2: IntPtr; FieldDesc: TFieldDesc; const Options: TCompareOptions; Mapped: boolean): integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure StartSearch;
    procedure EndSearch;

  { Blobs }
    function GetBlob(Field: TFieldDesc; RecBuf: IntPtr): TBlob;
    procedure SetBlob(Field: TFieldDesc; RecBuf: IntPtr; Blob: TBlob);
    function ReadBlob(Blob: TBlob; Position: Cardinal;
      Count: Cardinal; Dest: IntPtr; FromRollback: boolean = False; TrueUnicode: boolean = False): Cardinal; overload;
    function ReadBlob(Field: TFieldDesc; RecBuf: IntPtr; Position: Cardinal;
      Count: Cardinal; Dest: IntPtr; FromRollback: boolean = False; TrueUnicode: boolean = False): Cardinal; overload;
    procedure WriteBlob(Blob: TBlob; Position: Cardinal;
      Count: Cardinal; Source: IntPtr; TrueUnicode: boolean = False); overload;
    procedure WriteBlob(Field: TFieldDesc; RecBuf: IntPtr; Position: Cardinal;
      Count: Cardinal; Source: IntPtr; TrueUnicode: boolean = False); overload;
    function TruncateBlob(Blob: TBlob; Size: Cardinal;
      TrueUnicode: boolean = False): Integer; overload;
    function TruncateBlob(Field: TFieldDesc; RecBuf: IntPtr; Size: Cardinal;
      TrueUnicode: boolean = False): Integer; overload;
    function GetBlobSize(Blob: TBlob; FromRollback: boolean = False;
      TrueUnicode: boolean = False): Cardinal; overload;
    function GetBlobSize(Field: TFieldDesc; RecBuf: IntPtr; FromRollback: boolean = False;
      TrueUnicode: boolean = False): Cardinal; overload;
    procedure SetBlobSize(Blob: TBlob; NewSize: Cardinal; FromRollback: boolean = False;
      TrueUnicode: boolean = False); overload;
    procedure SetBlobSize(Field: TFieldDesc; RecBuf: IntPtr; NewSize: Cardinal; FromRollback: boolean = False;
      TrueUnicode: boolean = False); overload;

    property Active: boolean read FActive write FActive;
    property Prepared: boolean read FPrepared write FPrepared;
    property Fields: TFieldDescs read FFields;
    property Bof: boolean read GetBOF; // EOF: for CB case sensivity
    property Eof: boolean read GetEOF write FEOF;
    property RecordSize: Integer read FRecordSize;
    property CalcRecordSize: Integer read FCalcRecordSize;
    property DataSize: Integer read FDataSize;
    property StringHeap: TStringHeap read FStringHeap;
    property RecordCount: Integer read GetRecordCount;
    property RecordNo: Integer read GetRecordNo write SetRecordNo;
    property CachedUpdates: boolean read FCachedUpdates write SetCachedUpdates default False;
    property LocalUpdate: boolean read FLocalUpdate write FLocalUpdate default False;
    property InCacheProcessing: boolean read FInCacheProcessing write FInCacheProcessing;
    property NewCacheRecBuf: IntPtr read FNewCacheRecBuf write FNewCacheRecBuf;
    property OldCacheRecBuf: IntPtr read FOldCacheRecBuf write FOldCacheRecBuf;
    property UpdatesPending: boolean read GetUpdatesPending;
    property FilterFunc: TFilterFunc read FFilterFunc write FFilterFunc;
    property FilterMDFunc: TFilterFunc read FFilterMDFunc write FFilterMDFunc;
    property FilterRangeFunc: TFilterFunc read FFilterRangeFunc write FFilterRangeFunc;
    property FilterText: string read GetFilterText write SetFilterText;
    property FilterCaseInsensitive: boolean read FFilterCaseInsensitive write FFilterCaseInsensitive;
    property FilterNoPartialCompare: boolean read FFilterNoPartialCompare write FFilterNoPartialCompare;
    property FilterItemTypes: TItemTypes read FFilterItemTypes write SetFilterItemTypes;
    property FilterExpression: TExpressionNode read GetFilterExpression;
    property AutoInitFields: boolean read FAutoInitFields write FAutoInitFields;
    property TrimFixedChar: boolean read FTrimFixedChar write SetTrimFixedChar;
    property TrimVarChar: boolean read FTrimVarChar write SetTrimVarChar;
    /// if True then PutField set Null for string fields with empty value ('')
    property SetEmptyStrToNull: boolean read FSetEmptyStrToNull write FSetEmptyStrToNull;
    property RequireEmptyStrToNull: boolean read FRequireEmptyStrToNull;

    property Constraints: TCRObjectList read FConstraints;
    property FieldConstraints: TCRObjectList read FFieldConstraints;

    property SparseArrays: boolean read FSparseArrays write FSparseArrays;
    property HasBlobFields: boolean read FHasBlobFields write FHasBlobFields;
    property HasComplexFields: boolean read FHasComplexFields write FHasComplexFields;

    property OnAppend: TOnModifyRecord read FOnAppend write FOnAppend;
    property OnDelete: TOnModifyRecord read FOnDelete write FOnDelete;
    property OnUpdate: TOnModifyRecord read FOnUpdate write FOnUpdate;
    property OnApplyRecord: TOnApplyRecord read FOnApplyRecord write FOnApplyRecord;
    property OnCacheChanged: TOnModifyRecord read FOnCacheChanged write FOnCacheChanged;
    property OnCacheApplied: TOnModifyRecord read FOnCacheApplied write FOnCacheApplied;
    property OnCacheCanceled: TOnModifyRecord read FOnCacheCanceled write FOnCacheCanceled;
    property AfterApplyUpdates: TOnModifyRecord read FAfterApplyUpdates write FAfterApplyUpdates;
    property OnGetCachedFields: TOnGetCachedFields read FOnGetCachedFields write FOnGetCachedFields;
    property OnGetCachedBuffer: TOnGetCachedBuffer read FOnGetCachedBuffer write FOnGetCachedBuffer;
    property OnFieldsChanged: TOnFieldsChanged read FOnFieldsChanged write FOnFieldsChanged;
  end;

  TRecordNoCache = array of PItemHeader;

  TItemsCompareFunction = function(Item1, Item2: IntPtr): integer of object;

  TMemData = class (TData)
  private
    Cache: TCacheItem;
    LastCacheItem: TCacheItem;
    FRefreshIteration: Integer;
    FIndexFields: TCRObjectList;
    FCalcRecBuf, FCalcRecBuf2: IntPtr;

    FRecordNoCache: TRecordNoCache;

    function GetIndexField(Index: Integer): TSortColumn; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetIndexFieldCount: Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

  { Sorting }
    procedure ClearIndexFields;
    procedure UpdateIndexFields;
    function CompareRecords(RecBuf1, RecBuf2: IntPtr): integer;
    function CompareByRecBuf(Item1, Item2: IntPtr): integer;
    function CompareBySavedOrder(Item1, Item2: IntPtr): integer;
    procedure Exchange(I, J: PItemHeader);
    procedure MoveSortedRecord(Dir: integer);
    procedure QuickSort(L, R, P: PItemHeader; CompareFunction: TItemsCompareFunction);
    procedure RollbackItem(Item: PItemHeader);
  protected
    FIndexFieldNames: string;
    FRowsFetched: Integer;
    FOrderSaved: boolean;
    FirstItem: PItemHeader;
    LastItem: PItemHeader;
    CurrentItem: PItemHeader;

    BlockMan: TBlockManager;

  { Items/Data }
    procedure InitItem(Item: PItemHeader); virtual;
    function InsertItem: PItemHeader;
    function AppendItem: PItemHeader;
    procedure DeleteItem(Item: PItemHeader); virtual;
    procedure RevertItem(Item: PItemHeader);

    procedure InitData; override;
    procedure FreeData; override;

    procedure ReorderItems(Item: PItemHeader; ReorderOption: TReorderOption);

  { Navigation }
    function GetEOF: boolean; override;
    function GetBOF: boolean; override;

    function GetRecordNo: Integer; override;
    procedure SetRecordNo(Value: Integer); override;

  { Fetch }
    procedure InitFetchedItems(FetchedItem: IntPtr; NoCountData, FetchBack: boolean);

  { Sorting }
    procedure CheckIndexFields; virtual;
    procedure SetSortDefaults(SortColumn: TSortColumn); virtual;

  { Edit }
    procedure AddCacheItem(CacheItem: TCacheItem);
    procedure RemoveItemFromCache(Item: PItemHeader);
    procedure FreeCachedItem(CachedItem: TCacheItem);

  { CachedUpdates }
    function GetUpdatesPending: boolean; override;
    procedure SetFilterItemTypes(const Value: TItemTypes); override;

    function IsSupportedDataType(DataType: word): boolean; virtual;
    function IsSpecificType(const Field: TFieldDesc; var DataType: word; var DataTypeName: string; var Len, Scale: integer): boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;

  { Open/Close }
    procedure Reopen; override;

  { Fetch }
    function Fetch(FetchBack: boolean = False): boolean; virtual;

  { Fields }
    procedure InitFields; override;
    procedure ClearFields; override;

  { Records }
    procedure GetRecord(RecBuf: IntPtr); override;
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure GetPriorRecord(RecBuf: IntPtr); override;
    procedure PutRecord(RecBuf: IntPtr); override;
    procedure AppendRecord(RecBuf: IntPtr); override;
    procedure InsertRecord(RecBuf: IntPtr); override;
    procedure UpdateRecord(RecBuf: IntPtr); override;
    procedure DeleteRecord; override;
    procedure AddRecord(RecBuf: IntPtr);
    procedure RemoveRecord;  // remove record from memory
    procedure RefreshRecord(RecBuf: IntPtr);

    procedure PostRecord(RecBuf: IntPtr); override;

    function OmitRecord(Item: PItemHeader): boolean;
    procedure UpdateCachedBuffer(FItem, LItem: PItemHeader); // FItem and LItem can be nil. In this case FirstItem and LastItem used

  { Navigation }
    function GetFirstItem: PItemHeader;
    function GetLastItem: PItemHeader;
    function GetCurrentItem: PItemHeader;
    procedure SetToBegin; override;
    procedure SetToEnd; override;
    function SetToItem(Item: PItemHeader): Boolean;
    procedure PrepareRecNoCache(out Count: integer);

  { BookMarks }
    function GetRefreshIteration: integer;
    procedure GetBookmark(Bookmark: PRecBookmark); override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;
    function BookmarkValid(Bookmark: PRecBookmark): boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer; override;

  { CachedUpdates }
    function GetUpdateStatus: TItemStatus; override;
    function GetUpdateResult: TUpdateRecAction; override;
    function HasUpdatedOrDeletedRecords: boolean; override;

    procedure SetCacheRecBuf(NewBuf: IntPtr; OldBuf: IntPtr); override;
    procedure ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds); override;
    procedure CommitUpdates; override;
    procedure CancelUpdates; override;
    procedure RestoreUpdates; override;
    procedure RevertRecord; override;

    procedure GetOldRecord(RecBuf: IntPtr); override;

  { Filter }
    procedure FilterUpdated; override;
    procedure ClearItemsOmittedStatus;

  { Sorting }
    procedure SetIndexFieldNames(const Value: string); virtual;
    procedure SortItems; virtual;

    property IndexFields[Index: Integer]: TSortColumn read GetIndexField;
    property IndexFieldCount: Integer read GetIndexFieldCount;
    property RowsFetched: Integer read FRowsFetched;
  end;

{ TBlob }

  PPieceHeader = ^TPieceHeader;
  TPieceHeader = packed record
    Blob: {$IFDEF FPC}NativeInt{$ELSE}Integer{$ENDIF};
    Size: Integer;
    Used: Integer;  // offest 8 uses GetUsedPtr
    Prev: PPieceHeader;
    Next: PPieceHeader;
    Test: Word;       // DEBUG
  end;

  TCRBlobData = class (TSharedObject)
  private
    FFirstPiece: PPieceHeader;
    FPieceSize: Integer;
    FLargePieceSize: Integer;

    function GetLargePieceSize: Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    FIsNull: boolean;

    property FirstPiece: PPieceHeader read FFirstPiece;
    property PieceSize: Integer read FPieceSize;
    property LargePieceSize: Integer read GetLargePieceSize write FLargePieceSize;
  public
    constructor Create;
    destructor Destroy; override;

  { Pieces }
    class procedure AllocPiece(out Piece: PPieceHeader; Size: Integer);
    procedure ReallocPiece(var Piece: PPieceHeader; Size: Integer);
    procedure FreePiece(Piece: PPieceHeader);
    procedure AppendPiece(Piece: PPieceHeader);
    procedure DeletePiece(Piece: PPieceHeader);
    procedure CompressPiece(var Piece: PPieceHeader);

    function GetDataSize: Cardinal; // sum of pieces.used
    function GetSize: Cardinal; virtual; // if uncompressed then equal to GetDataSize else uncompressed size
    procedure SetSize(Value: Cardinal); virtual;
    procedure Clear; virtual;
    procedure Truncate(NewSize: Cardinal); virtual;
    function IsEmpty: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsNull: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}

    function Read(Position: Cardinal; Count: Cardinal; Dest: IntPtr): Cardinal; virtual;
    procedure Write(Position: Cardinal; Count: Cardinal; Source: IntPtr); virtual;

    procedure Compress;
    procedure Defrag(MinPieceSize: Integer = 0); // Move all data to first piece if PieceSize = 0
    procedure CopyTo(Dest: TCRBlobData); // used to fill rollback

    procedure AddCRUnicode;
    procedure RemoveCRUnicode;
    procedure AddCRString;
    procedure RemoveCRString;

  { Unicode to Ansi conversion methods }
    function TranslatePositionToAnsi(Position: Cardinal): Cardinal; // Ansi to Unicode
    function TranslatePositionToUni(Position: Cardinal): Cardinal; // Ansi to Unicode
    function GetSizeAnsi: Cardinal;
    function GetSizeUni: Cardinal;
  end;

  TBlob = class (TSharedObject)
  protected
    FData: TCRBlobData;
    FIsUnicode: boolean;

    FNeedRollback: boolean;
    FRollback: TCRBlobData;
    FStoredData: TCRBlobData;

    function GetAsString: string;
    procedure SetAsString(const Value: string);
  {$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString; //virtual;
    procedure SetAsAnsiString(const Value: AnsiString); //virtual;
  {$ENDIF}
    function GetAsWideString: WideString; //virtual;
    procedure SetAsWideString(const Value: WideString); //virtual;
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const Value: TBytes);

    function GetPieceSize: Integer;
    procedure SetPieceSize(Value: Integer);
    function GetLargePieceSize: Integer;
    procedure SetLargePieceSize(Value: Integer);
    function GetUseRollback: boolean;
    procedure SetUseRollback(Value: boolean);

    function CreateClone: TBlob; virtual;
    function CreateBlobData: TCRBlobData; virtual;
    procedure CloneBlobData(SourceBlob: TBlob);

    procedure CheckValid;   // DEBUG
    procedure CheckCached;

    procedure CheckValue; virtual;

    procedure SaveToRollback; virtual;

    function GetSize: Cardinal; virtual; // if uncompressed then equal to GetDataSize else uncompressed size
    procedure SetSize(Value: Cardinal); virtual;
    procedure SetIsUnicode(Value: boolean); virtual;

  { Unicode to Ansi and Ansi to Unicode conversion methods }
    function TranslatePositionToAnsi(Position: Cardinal): Cardinal; // Ansi to Unicode
    function TranslatePositionToUni(Position: Cardinal): Cardinal; // Unicode to Ansi
    function GetSizeAnsi: Cardinal; virtual;
    function GetSizeUni: Cardinal; virtual;
  public
    Test: byte;   // DEBUG

    constructor Create(IsUnicode: boolean = False);
    destructor Destroy; override;

    function Clone(FromRollback: boolean = False; CloneData: boolean = True): TBlob;
    procedure FreeBlob; virtual;

  { Pieces }
    class procedure AllocPiece(out Piece: PPieceHeader; Size: Integer);
    procedure ReallocPiece(var Piece: PPieceHeader; Size: Integer);
    procedure FreePiece(Piece: PPieceHeader);
    procedure AppendPiece(Piece: PPieceHeader);
    procedure DeletePiece(Piece: PPieceHeader);
    procedure CompressPiece(var Piece: PPieceHeader);

    function FirstPiece: PPieceHeader;

    function Read(Position: Cardinal; Count: Cardinal; Dest: IntPtr): Cardinal; virtual;
    procedure Write(Position: Cardinal; Count: Cardinal; Source: IntPtr); virtual;
    procedure Clear; virtual;
    procedure Truncate(NewSize: Cardinal); virtual;
    function IsEmpty: boolean;
    function IsNull: boolean;
    procedure Compress;
    procedure Defrag(MinPieceSize: Cardinal = 0); virtual; // Move all data to first piece if MinPieceSize = 0
    procedure AddCR;
    procedure RemoveCR;

  { Stream/File }

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure Assign(Source: TBlob);

    function GetData: TCRBlobData;
    procedure SetData(Value: TCRBlobData);

  { Rollback }
    procedure EnableRollback;
    procedure Commit; virtual;
    procedure Cancel; virtual;
    function CanRollback: boolean;

    property Size: Cardinal read GetSize write SetSize;
    property AsString: string read GetAsString write SetAsString;
  {$IFNDEF NEXTGEN}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  {$ENDIF}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property IsUnicode: boolean read FIsUnicode write SetIsUnicode;
    property PieceSize: Integer read GetPieceSize write SetPieceSize;
    property LargePieceSize: Integer read GetLargePieceSize write SetLargePieceSize;
    property RollbackEnabled: boolean read FNeedRollback write FNeedRollback;
    property UseRollback: boolean read GetUseRollback write SetUseRollback;
  end;

var
  DefaultPieceSize: Integer = 64*1024 - sizeof(TPieceHeader);
  DefaultLargePieceSize: Integer = 1024*1024 + 64*1024 - SizeOf(TPieceHeader);

{$IFDEF HAVE_COMPRESS}

{ TCompressedBlob }

const
  CCompressBlobHeaderGuidSize = 16;
  CCompressBlobHeaderSize = CCompressBlobHeaderGuidSize{guid} + SizeOf(Integer){uncompressed size};
  CCompressBlobHeaderGuid: array [0..CCompressBlobHeaderGuidSize - 1] of byte = ($39, $8C, $9D, $F1, $58, $55, $49, $38, $A6, $52, $87, $CE, $E0, $C6, $DA, $7E);

type
  TCompressBlobMode = (
    cbNone, // uncompressed (default)
    cbClient, // store compressed data on client. Save client memory. Other apps can read and write BLOBs on server
    cbServer, // store compressed data on server. Save server memory. Other apps can NOT read and write BLOBs on server
    cbClientServer // store compressed data on client and server.
  );

  TCompressedBlobData = class(TCRBlobData)
  protected
    function CompressFrom(source: IntPtr; const sourceLen: Integer): boolean;
    procedure UncompressTo(dest: IntPtr; var destlen: Integer);

  public
    function IsCompressed: boolean;
    function SetCompressed(Value: boolean): boolean;
    function UnCompressedSize: Cardinal;

    function GetSize: Cardinal; override;
    procedure SetSize(Value: Cardinal); override;
    function GetCompressedSize: Cardinal;

    function Read(Position: Cardinal; Count: Cardinal; Dest: IntPtr): Cardinal; override;
    procedure Write(Position: Cardinal; Count: Cardinal; Source: IntPtr); override;
    procedure Truncate(NewSize: Cardinal); override;
  end;

  TCompressedBlob = class (TBlob)
  protected
    function GetCompressed: boolean;
    procedure SetCompressed(Value: boolean);
    function GetCompressedSize: Cardinal;

    function CreateBlobData: TCRBlobData; override;

  public
    property Compressed: boolean read GetCompressed write SetCompressed;
    property CompressedSize: Cardinal read GetCompressedSize;
  end;
{$ELSE}
type
  TCompressedBlob = class(TBlob);
{$ENDIF}

{ TVariantObject }

  TVariantObject = class (TSharedObject)
  private
    FValue: Variant;
  public
    property Value: Variant read FValue write FValue;
  end;

  function NextPiece(Piece: PPieceHeader): PPieceHeader;
  function PieceData(Piece: PPieceHeader): IntPtr;
  function PieceUsedPtr(Piece: PPieceHeader): IntPtr;

  procedure DataError(Msg: string);

const
  varDecimal  = $000E;
  varLongWord = $0013;

var
  StartWaitProc: procedure;
  StopWaitProc: procedure;
  ApplicationTitleProc: function: string;

  SingleValueDelta: single = 0;
  DoubleValueDelta: double = 0;
  DateTimeValueDelta: double = 1.15e-8; // ~0,98 millisecond
  MaxArrayItem: integer = 100; // Max count of fields from array type
  UniqueFieldIndexSeparator: string = '_';
  DefaultExpressionOldBehavior: boolean = False; // moved to MemData 04.12.2015

{$IFDEF CRDEBUG}
var
  ShareObjectCnt: integer = 0;
  ShareObjectList: TList;

procedure ShowWarningMessage(const MessageStr: string; Count: integer = 0);
procedure ShowNotReleasedObjects;
{$ENDIF}

procedure StartWait;
procedure StopWait;
function ApplicationTitle: string;

function AddCRString(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
function RemoveCRString(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;

function AddCRUnicode(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
function RemoveCRUnicode(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;

function AddCRBigEndian(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
function RemoveCRBigEndian(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;

implementation

uses
  Types, Math,
  DAConsts;

const
  lxEqual           = 1;
  lxMore            = lxEqual + 1;
  lxLess            = lxMore + 1;
  lxMoreEqual       = lxLess + 1;
  lxLessEqual       = lxMoreEqual + 1;
  lxNoEqual         = lxLessEqual + 1;
  lxLeftBracket     = lxNoEqual + 1;
  lxRightBracket    = lxLeftBracket + 1;
  lxMinus           = lxRightBracket + 1;
  lxPlus            = lxMinus + 1;
  lxLeftSqBracket   = lxPlus + 1;
  lxRightSqBracket  = lxLeftSqBracket + 1;

  lxAND             = lxRightSqBracket + 1;
  lxFALSE           = lxAND + 1;
  lxIS              = lxFALSE + 1;
  lxLIKE            = lxIS + 1;
  lxNOT             = lxLIKE + 1;
  lxNULL            = lxNOT + 1;
  lxOR              = lxNULL + 1;
  lxTRUE            = lxOR + 1;
  lxIN              = lxTRUE + 1;
  lxLOWER           = lxIN + 1;
  lxUPPER           = lxLOWER + 1;
  lxBETWEEN         = lxUPPER + 1;

var
  BoolSymbolLexems, BoolKeywordLexems: TLexemList;
  RefreshIteration: Integer;
  SizeOf_TStrData: Integer;
  SizeOf_TBlock: Integer;

{ Functions }

procedure DataError(Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure StartWait;
begin
  if Assigned(StartWaitProc) then
    StartWaitProc;
end;

procedure StopWait;
begin
  if Assigned(StopWaitProc) then
    StopWaitProc;
end;

function ApplicationTitle: string;
begin
  if Assigned(ApplicationTitleProc) then
    Result := ApplicationTitleProc
  else
    Result := '';
end;

function CompareAlias(Field1, Field2: Pointer): integer;
begin
  if Field1 = Field2 then
    Result := 0
  else begin
    Result := AnsiCompareText(TFieldDesc(Field1).Name, TFieldDesc(Field2).Name);
    if Result = 0 then begin
      Result := TFieldDesc(Field1).FieldNo - TFieldDesc(Field2).FieldNo;
      TFieldDesc(Field1).FReserved := True;
      TFieldDesc(Field2).FReserved := True;
    end;
  end
end;

{ TFieldDesc }

constructor TFieldDesc.Create(RecordSetClass: TRecordSetClass);
begin
  inherited Create;

  FUpdateable := True;
  FRecordSetClass := RecordSetClass;
  FActualFieldNo := -1;
end;

destructor TFieldDesc.Destroy;
begin
  if FObjectType <> nil then
    FObjectType.Release;

  inherited;
end;

procedure TFieldDesc.SetObjectType(Value: TObjectType);
begin
  if Value <> FObjectType then begin
    if FObjectType <> nil then
      FObjectType.Release;

    FObjectType := Value;

    if FObjectType <> nil then
      FObjectType.AddRef;
  end;
end;

procedure TFieldDesc.SetParentField(Value: TFieldDesc);
begin
  FParentField := Value;
  FHasParent := Value <> nil;
end;

function TFieldDesc.GetMapLength: Integer;
begin
  Result := FLength;
end;

function TFieldDesc.GetMapDataType: Word;
begin
  Result := FDataType;
end;

procedure TFieldDesc.SetDataType(Value: Word);
begin
  FDataType := Value;

  // cache field attributes
  FIsBlob := FRecordSetClass.IsBlobDataType(Value);
  FIsObject := FRecordSetClass.IsObjectDataType(Value);
  FIsSharedObject := FRecordSetClass.IsSharedObjectDataType(Value);
  FIsComplex := FRecordSetClass.IsComplexDataType(Value);
  FHasValueLen := FRecordSetClass.HasValueLen(Value);
end;

procedure TFieldDesc.SetSubDataType(Value: Word);
begin
  FSubDataType := Value;
end;

procedure TFieldDesc.Assign(FieldDesc: TFieldDesc);
begin
  Name := FieldDesc.Name;
  ActualName := FieldDesc.ActualName;
  DataType := FieldDesc.DataType;
  SubDataType := FieldDesc.SubDataType;
  Length := FieldDesc.Length;
  Scale := FieldDesc.Scale;
  Size := FieldDesc.Size;
  Offset := FieldDesc.Offset;
  DataOffset := FieldDesc.DataOffset;
  Required := FieldDesc.Required;
  FieldNo := FieldDesc.FieldNo;

  FIsBlob := FieldDesc.IsBlob;
  FIsObject := FieldDesc.IsObject;
  FIsSharedObject := FieldDesc.IsSharedObject;
  FIsComplex := FieldDesc.IsComplex;
  FHasValueLen := FieldDesc.HasValueLen;
end;

{ TFieldDescs }

function TFieldDescs.FindField(const Name: string): TFieldDesc;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Items[i] <> nil) and not Items[i].HasParent then begin
      if AnsiCompareText(Items[i].Name, Name) = 0 then begin
        Result := Items[i];
        Exit;
      end;
    end;

  for i := 0 to Count - 1 do
    if Items[i] <> nil then begin
      if Items[i].HasParent then
        if AnsiCompareText(Items[i].Name, Name) = 0 then begin
          Result := Items[i];
          Exit;
        end;
      if AnsiCompareText(Items[i].ActualName, Name) = 0 then begin
        Result := Items[i];
        Exit;
      end;
    end;
end;

function TFieldDescs.FieldByName(const Name: string): TFieldDesc;
begin
  Result := FindField(Name);

  if Result = nil then
    raise Exception.Create(Format(SFieldNotFound, [Name]));
end;

function TFieldDescs.FieldByActualFieldNo(ActualFieldNo: integer): TFieldDesc;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Items[i] <> nil) and (Items[i].ActualFieldNo = ActualFieldNo) then begin
      Result := Items[i];
      Exit;
    end;
end;

function TFieldDescs.GetItems(Index: integer): TFieldDesc;
begin
  Result := TFieldDesc(inherited Items[Index]);
end;

{ TAttribute }

constructor TAttribute.Create;
begin
  inherited;
end;

destructor TAttribute.Destroy;
begin
  if FObjectType <> nil then
    FObjectType.Release;

  inherited;
end;

procedure TAttribute.SetObjectType(Value: TObjectType);
begin
  if FObjectType <> Value then begin
    if FObjectType <> nil then
      FObjectType.Release;

    FObjectType := Value;

    if FObjectType <> nil then
      FObjectType.AddRef;
  end;
end;

function TAttribute.GetActualName: string;
begin
  Result := FName;
end;

procedure TAttribute.SetActualName(const Value: string);
begin
  // do nothing
end;

{ TAttributeChain }

constructor TAttributeChain.Create(Attribute: TAttribute; Prev: TAttributeChain; Index: Integer);
begin
  inherited Create;

  FAttribute := Attribute;
  FIndex := Index;
  FNext := nil;
  FPrev := Prev;
  if FPrev <> nil then
    FPrev.FNext := Self;
end;

destructor TAttributeChain.Destroy;
begin
  if FPrev <> nil then begin
    FPrev.FNext := nil;
    FPrev.Free;
  end;

  if FNext <> nil then begin
    FNext.FPrev := nil;
    FNext.Free;
  end;

  inherited;
end;

function TAttributeChain.GetAsString: string;
var
  Chain: TAttributeChain;
begin
  Result := '';

  Chain := Self;
  repeat
    if Chain.Index >= 0 then
      Result := Result + '[' + IntToStr(Chain.Index) + ']'
    else if Result <> '' then
      Result := Result + '.' + Chain.Attribute.Name
    else
      Result := Chain.Attribute.Name;
    Chain := Chain.Next;
  until Chain = nil;
end;

function TAttributeChain.First: TAttributeChain;
begin
  Result := Self;
  while Result.Prev <> nil do
    Result := Result.Prev;
end;

function TAttributeChain.Last: TAttributeChain;
begin
  Result := Self;
  while Result.Next <> nil do
    Result := Result.Next;
end;

{ TObjectType }

constructor TObjectType.Create;
begin
  inherited;

  FAttributes := TCRObjectList.Create;
end;

destructor TObjectType.Destroy;
begin
  ClearAttributes;
  FAttributes.Free;

  inherited;
end;

procedure TObjectType.ClearAttributes;
begin
  FAttributes.Clear;
end;

function TObjectType.ParseAttribute(Name: string): TAttributeChain;
var
  i: integer;
  St: string;
  iPos, IndexPos: integer;
  Index: Integer;
  OType: TObjectType;
  Attribute: TAttribute;
begin
  Result := nil;

  Name := AnsiUpperCase(Name);
  OType := Self;

  repeat
    Name := TrimLeft(Name);

    Index := -1;
    iPos := Pos('.', Name);
    IndexPos := Pos('[', Name);
    if IndexPos = 1 then begin
      i := Pos(']', Name);
      if i = 0 then begin
        FreeAndNil(Result);
        Exit;
      end
      else if not TryStrToInt(Copy(Name, IndexPos + 1, i - IndexPos - 1), Index) then begin
        FreeAndNil(Result);
        Exit;
      end;

      if (i + 1 <= Length(Name)) and (Name[i + 1] = '.') then
        Inc(i);

      St := 'ELEMENT';
      Name := Copy(Name, i + 1, Length(Name));
    end
    else
      if (iPos > 0) and ((iPos < IndexPos) or (IndexPos = 0)) then begin
        St := Copy(Name, 1, iPos - 1);
        Name := Copy(Name, iPos + 1, Length(Name));
      end
      else
        if IndexPos > 0 then begin
          St := Copy(Name, 1, IndexPos - 1);
          Name := Copy(Name, IndexPos, Length(Name));
        end
        else
          St := Name;

    Attribute := nil;
    for i := 0 to OType.AttributeCount - 1 do
      if AnsiUpperCase(TAttribute(OType.Attributes[i]).Name) = St then begin
        Attribute := OType.Attributes[i];
        Result := TAttributeChain.Create(Attribute, Result, Index);
        break;
      end;

    if Attribute = nil then begin
      FreeAndNil(Result);
      Exit;
    end
    else if (iPos <> 0) and not (Attribute.DataType in [dtObject, dtArray, dtTable, dtReference]) then begin
      FreeAndNil(Result);
      Exit;
    end;

    OType := Attribute.ObjectType;
  until (iPos = 0) and ((IndexPos = 0) or (Name = ''));
end;

function TObjectType.FindAttribute(const Name: string): TAttribute;
var
  AttrChain: TAttributeChain;
begin
  AttrChain := ParseAttribute(Name);
  if AttrChain <> nil then
    Result := AttrChain.Attribute
  else
    Result := nil;
  AttrChain.Free;
end;

function TObjectType.GetAttribute(const Name: string): TAttribute;
begin
  Result := FindAttribute(Name);
  if Result = nil then
    raise Exception.Create(Format(SAttributeNotFount, [Name]));
end;

function TObjectType.GetAttributeChain(const Name: string): TAttributeChain;
begin
  Result := ParseAttribute(Name);

  if Result <> nil then
    Result := Result.First
  else
    raise Exception.Create(Format(SAttributeNotFount, [Name]));
end;

function TObjectType.GetAttributes(Index: integer): TAttribute;
begin
  Result := TAttribute(FAttributes[Index]);
end;

function TObjectType.GetAttributeCount: integer;
begin
  Result := FAttributes.Count;
end;

{ TDBObject }

constructor TDBObject.Create;
begin
  inherited;
end;

procedure TDBObject.SetObjectType(Value: TObjectType);
begin
  if Value <> FObjectType then begin
    if FObjectType <> nil then
      FObjectType.Release;

    FObjectType := Value;

    if FObjectType <> nil then
      FObjectType.AddRef;
  end;
end;

procedure TDBObject.GetAttributeValue(const Name: string; out AttrBuf: IntPtr; out AttrLen: Word; out IsBlank, NativeBuffer: boolean);
begin
  IsBlank := True;
  NativeBuffer := True;
  AttrBuf := nil;
end;

procedure TDBObject.SetAttributeValue(const Name: string; ValuePtr: IntPtr; ValueLen: Word);
begin
end;

function TDBObject.GetAttrIsNull(const Name: string): boolean;
begin
  Result := True;
end;

{ TBoolParser }

constructor TBoolParser.Create(const Text: string);
begin
  inherited Create(Text);

  FSymbolLexems := BoolSymbolLexems;
  FKeywordLexems := BoolKeywordLexems;

  FOmitStringQuote := False;
end;

function TBoolParser.IsStringQuote(Ch: char): boolean;
begin
  if OmitStringQuote then
    Result := False
  else
    Result := inherited IsStringQuote(Ch);
end;

procedure TBoolParser.ToRightQuote(LeftQuote: char);
begin
  while Pos <= TextLength do begin
    // Skip double quote
    if (Pos + 1 <= TextLength) and (Text[Pos] = '''') and (Text[Pos + 1] = '''') then
      Inc(Pos, 2)
    else
    // If found right quote then exit
    if Text[Pos] = LeftQuote then
      break
    else
      Inc(Pos)
  end;
end;

{ TCondition }

constructor TCondition.Create(Data: TData; const Text: string);
begin
  inherited Create;

  FData := Data;
  FText := Text;
end;

destructor TCondition.Destroy;
begin
  if FExpression <> nil then
    FreeExpression;

  inherited;
end;

function TCondition.GetField(const FieldName: string): TFieldDesc;
begin
  Result := FData.FindField(FieldName);
end;

function TCondition.AllocNode: TExpressionNode;
begin
  Result := TExpressionNode.Create;
  Result.NextAlloc := FFirstAlloc;
  FFirstAlloc := Result;
  Result.LeftOperand := nil;
  Result.RightOperand := nil;
  Result.NextOperand := nil;
end;

function TCondition.OrExpr: TExpressionNode;
var
  Node: TExpressionNode;
begin
  Result := AndExpr;
  while FCode = lxOR do begin
    FCode := FParser.GetNext(FStrLexem);
    Node := AllocNode;
    Node.NodeType := ntOr;
    Node.LeftOperand := Result;
    Node.RightOperand := AndExpr;
    Result := Node;
  end;
end;

function TCondition.AndExpr: TExpressionNode;
var
  Node: TExpressionNode;
begin
  Result := Condition;
  while FCode = lxAND do begin
    FCode := FParser.GetNext(FStrLexem);
    Node := AllocNode;
    Node.NodeType := ntAnd;
    Node.LeftOperand := Result;
    Node.RightOperand := Condition;
    Result := Node;
  end;
end;

function TCondition.Condition: TExpressionNode;
var
  OpCode: integer;
  PrevNode, NextNode: TExpressionNode;
  NodeType: TExpressionType;
begin
  Result := nil;
  if (FCode = lcIdent) or (FCode = lcNumber) or (FCode = lcString) or
     (FCode in [lxMinus, lxPlus, lxLeftSqBracket, lxRightSqBracket, lxLOWER, lxUPPER, lxBETWEEN])
  then begin
    Result := AllocNode;
    Result.LeftOperand := Argument;
    OpCode := FCode;
    case FCode of
      lxEqual, lxIS:
        Result.NodeType := ntEqual;
      lxMore:
        Result.NodeType := ntMore;
      lxLess:
        Result.NodeType := ntLess;
      lxMoreEqual:
        Result.NodeType := ntMoreEqual;
      lxLessEqual:
        Result.NodeType := ntLessEqual;
      lxNoEqual:
        Result.NodeType := ntNoEqual;
      lxLike:
        Result.NodeType := ntLike;
      lxNOT:
        Result.NodeType := ntNot;
      lxIN:
        Result.NodeType := ntIn;
      lxBETWEEN:
        Result.NodeType := ntBetween;
    else
      ExpressionError;
    end;
    FCode := FParser.GetNext(FStrLexem);
    if OpCode = lxIS then begin
      if FCode = lxNOT then begin
        FCode := FParser.GetNext(FStrLexem);
        if FCode <> lxNULL then
          ExpressionError;
        Result.NodeType := ntNoEqual;
      end
      else if FCode <> lxNULL then
        ExpressionError;
    end
    else
    if OpCode = lxNOT then begin
      if FCode = lxLike then begin
        FCode := FParser.GetNext(FStrLexem);
        Result.NodeType := ntNotLike;
      end
      else
      if FCode = lxIN then begin
        FCode := FParser.GetNext(FStrLexem);
        if FCode <> lxLeftBracket then
          ExpressionError;
        Result.NodeType := ntNotIn;
      end
      else
      if FCode = lxBETWEEN then begin
        FCode := FParser.GetNext(FStrLexem);
        if (FCode <> lcString) and (FCode <> lcNumber) then
          ExpressionError;
        Result.NodeType := ntNotBetween;
      end
      else
        ExpressionError;
    end
    else
    if OpCode = lxIN then begin
      if FCode <> lxLeftBracket then
        ExpressionError;
    end;
    if Result.NodeType in [ntBetween, ntNotBetween] then begin
      NodeType := Result.NodeType;
      Result.RightOperand := Argument;
      if FCode <> lxAND then
        ExpressionError;
      NextNode := AllocNode;
      NextNode.LeftOperand := Result.LeftOperand;
      if Result.NodeType = ntBetween then begin
        Result.NodeType := ntMoreEqual;
        NextNode.NodeType := ntLessEqual;
      end
      else begin
        Result.NodeType := ntLess;
        NextNode.NodeType := ntMore;
      end;
      FCode := FParser.GetNext(FStrLexem);
      if (FCode <> lcString) and (FCode <> lcNumber) then
        ExpressionError;
      NextNode.RightOperand := Argument;
      PrevNode := Result;
      Result := AllocNode;
      if NodeType = ntBetween then
        Result.NodeType := ntAnd
      else
        Result.NodeType := ntOr;
      Result.LeftOperand := PrevNode;
      Result.RightOperand := NextNode;
    end
    else
      Result.RightOperand := Argument;
  end
  else
    if FCode = lxNOT then begin
      FCode := FParser.GetNext(FStrLexem);
      Result := AllocNode;
      Result.NodeType := ntNot;
      Result.LeftOperand := Condition;
    end
    else
      if FCode = lxTRUE then begin
        FCode := FParser.GetNext(FStrLexem);
        Result := AllocNode;
        Result.NodeType := ntTrue;
      end
      else
        if FCode = lxFALSE then begin
          FCode := FParser.GetNext(FStrLexem);
          Result := AllocNode;
          Result.NodeType := ntFalse;
        end
        else
          if FCode = lxLeftBracket then begin
            FCode := FParser.GetNext(FStrLexem);
            Result := OrExpr;
            if FCode = lxRightBracket then
              FCode := FParser.GetNext(FStrLexem)
            else
              ExpressionError;
          end
          else
            ExpressionError;
end;

function TCondition.Argument: TExpressionNode;
var
  Field: TFieldDesc;
  FieldName, ASign, Buf, Prefix: string;
  InValuesStr: TIntValueStringList;
  InValues: Variant;
  OldDecSeparator: Char;
  i: integer;
  Node: TExpressionNode;
  ValCode: integer;
  ValValueInt64: Int64;

  function ParseFieldName(const FirstPart: string): string;
  begin
    Result := FirstPart;
    FCode := FParser.GetNext(FStrLexem);
    if FStrLexem = '.' then
      repeat
        FCode := FParser.GetNext(FStrLexem);
        if FCode = lcIdent then
          Result := Result + '.' + FStrLexem
        else
          break;
        FCode := FParser.GetNext(FStrLexem);
      until FStrLexem <> '.';
  end;

begin
  Result := AllocNode;
  case FCode of
    lcIdent: begin
      FieldName := ParseFieldName(FStrLexem);
      Field := GetField(FieldName);
      if Field = nil then
        raise Exception.Create(Format(SFieldNotFound, [FieldName]));
      Result.NodeType := ntField;
      Result.FieldDesc := Field;
      Result.Value := FStrLexem;
      Exit;
    end;
    lxLeftSqBracket: begin
      FieldName := '';
      FParser.OmitBlank := False;
      FParser.OmitStringQuote := True;
      FCode := FParser.GetNext(FStrLexem);
      while (FCode <> lxRightSqBracket) and (FCode <> lcEnd) do begin
        FieldName := FieldName + FStrLexem;
        FCode := FParser.GetNext(FStrLexem);
      end;
      FParser.OmitBlank := True;
      FParser.OmitStringQuote := False;
      Field := GetField(FieldName);
      if Field = nil then
        raise Exception.Create(Format(SFieldNotFound, [FieldName]));
      Result.NodeType := ntField;
      Result.FieldDesc := Field;
      Result.Value := FieldName;
    end;
    lcString: begin
      Result.NodeType := ntValue;
      if FStrLexem <> '' then
        Result.Value := AnsiDequotedStr('''' + FStrLexem + '''', '''') // TODO Optimize with StringBuilder
      else
        Result.Value := '';
    end;
    lcNumber: begin
      Result.NodeType := ntValue;
      Val(FStrLexem, ValValueInt64, ValCode);
      if ValCode = 0 then
        Result.Value := ValValueInt64
      else
        Result.Value := StrToFloat(FStrLexem);
    end;
    lxMinus, lxPlus: begin
      Result.NodeType := ntValue;
      ASign := FStrLexem;
      FCode := FParser.GetNext(FStrLexem);
      if FCode = lcNumber then
        Result.Value := StrToFloat(ASign + FStrLexem)
      else
        ExpressionError;
    end;
    lxNULL: begin
      Result.NodeType := ntValue;
      Result.Value := Null;
    end;
    lxTRUE: begin
      Result.NodeType := ntValue;
      Result.Value := True;
    end;
    lxFALSE: begin
      Result.NodeType := ntValue;
      Result.Value := False;
    end;
    lxLeftBracket: begin
      InValuesStr := TIntValueStringList.Create;
      OldDecSeparator := FParser.DecSeparator;
      try
        FParser.DecSeparator := '.';
        FCode := FParser.GetNext(FStrLexem);
        if (FCode = lxRightBracket) or (FCode = lcEnd) then
          ExpressionError;
        while (FCode <> lxRightBracket) and (FCode <> lcEnd) do begin
          if FStrLexem <> ',' then begin
            Prefix := '';
            if FCode in [lxMinus, lxPlus] then begin
              if FCode = lxMinus then
                Prefix := '-'
              else if FCode = lxPlus then
                Prefix := '+';
              FCode := FParser.GetNext(FStrLexem);
            end;
            if (FCode <> lcNumber) and (FCode <> lcString) then
              ExpressionError;
            InValuesStr.Insert(InValuesStr.Count, Prefix + FStrLexem, FCode);
          end;
          FCode := FParser.GetNext(FStrLexem);
        end;
        if FCode <> lxRightBracket then
          ExpressionError;
        InValues := VarArrayCreate([0, InValuesStr.Count - 1], varVariant);
        for i := 0 to InValuesStr.Count - 1 do
          if InValuesStr.Values[i] = lcNumber then begin
            Val(InValuesStr[i], ValValueInt64, ValCode);
            if ValCode = 0 then
              InValues[i] := ValValueInt64
            else begin
              Buf := StringReplace(InValuesStr[i], '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []);
              InValues[i] := StrToFloat(Buf);
            end;
          end
          else
            InValues[i] := InValuesStr[i];
        Result.NodeType := ntValue;
        Result.Value := InValues;
      finally
        FParser.DecSeparator := OldDecSeparator;
        InValuesStr.Free;
      end;
    end;
    lxLOWER, lxUPPER: begin
      case FCode of
        lxLOWER:
          Result.NodeType := ntLower;
        lxUPPER:
          Result.NodeType := ntUpper;
      end;
      FCode := FParser.GetNext(FStrLexem);
      if (FCode <> lxLeftBracket) then
        ExpressionError;
      FCode := FParser.GetNext(FStrLexem);
      case FCode of
        lcIdent: begin
          FieldName := ParseFieldName(FStrLexem);
          Field := GetField(FieldName);
          if Field = nil then
            raise Exception.Create(Format(SFieldNotFound, [FieldName]));
          Node := AllocNode;
          Node.NodeType := ntField;
          Node.FieldDesc := Field;
          if FCode = lxRightBracket then
            FCode := FParser.GetNext(FStrLexem)
          else
            ExpressionError;
          Node.Value := FStrLexem;
          Node.LeftOperand := Result;
          Result := Node;
          exit;
        end;
        lcString: begin
          Node := AllocNode;
          Node.NodeType := ntValue;
          if FStrLexem <> '' then
            Node.Value := AnsiDequotedStr('''' + FStrLexem + '''', '''') // TODO Optimize with StringBuilder
          else
            Node.Value := '';
          FCode := FParser.GetNext(FStrLexem);
          if FCode <> lxRightBracket then
            ExpressionError;
          Node.LeftOperand := Result;
          Result := Node;
        end;
      end;
    end;
  else
    ExpressionError;
  end;

  FCode := FParser.GetNext(FStrLexem);
end;

procedure TCondition.CreateExpression;
var
  Node: TExpressionNode;
begin
  FreeExpression;
  if Trim(Text) <> '' then begin
    FParser := TBoolParser.Create(Text);
    try
      try
        FParser.ToBegin();
        FCode := FParser.GetNext(FStrLexem);
        FExpression := OrExpr();

        Node := FFirstAlloc;
        while Node <> nil do begin
          if (Node.NodeType = ntField) and (Node.FieldDesc.FieldDescKind = fdkCalculated) then begin
            FExpression.UseCalculatedFields := True;
            break;
          end;
          Node := Node.NextAlloc;
        end;

        if (FCode <> lcEnd) then
          ExpressionError;
      except
        FreeExpression;
        raise;
      end;
    finally
      FParser.Free;
    end;
  end;
end;

procedure TCondition.FreeExpression;
var
  Node: TExpressionNode;
begin
  while FFirstAlloc <> nil do begin
    Node := FFirstAlloc;
    FFirstAlloc := FFirstAlloc.NextAlloc;
    Node.Free;
  end;
  FExpression := nil;
end;

{ TFilter }

procedure TFilter.ExpressionError;
begin
  raise Exception.Create(SIllegalFilter);
end;

{ TConstraint }

constructor TConstraint.Create(Data: TData; const Text: string; const ConstraintErrorMessage: string; UseCreateExpression: boolean = False);
begin
  inherited Create(Data, Text);

  FErrorMessage := ConstraintErrorMessage;
  FComplexConstraint := False;
  if UseCreateExpression then
    CreateExpression;
end;

procedure TConstraint.ExpressionError;
begin
  raise Exception.Create(SIllegalConstraint);
end;

procedure TConstraint.ConstraintError;
begin
  raise Exception.Create(FErrorMessage);
end;

procedure TConstraint.CreateExpression;
begin
  FComplexConstraint := False;

  inherited;
end;

procedure TConstraint.FreeExpression;
begin
  inherited;

  FComplexConstraint := False;
  FAlias := '';
end;

procedure TConstraint.EmptyConstraint;
begin
  FreeExpression;

  FErrorMessage := '';
  FText := '';
end;

procedure TConstraint.UpdateConstraint(const Text: string; const ConstraintErrorMessage: string);
begin
  FErrorMessage := ConstraintErrorMessage;
  FText := Text;
  CreateExpression;
end;

constructor TFieldConstraint.Create(Data: TData; const Text: string; const ConstraintErrorMessage: string; Field: TFieldDesc; UseCreateExpression: boolean = False);
begin
  FField := Field;

  inherited Create(Data, Text, ConstraintErrorMessage, UseCreateExpression);
end;

function TFieldConstraint.GetField(const FieldName: string): TFieldDesc;
begin
  Result := FData.FindField(FieldName);
  if Result = nil then begin
    if FAlias = '' then
      FAlias := FieldName;
    if AnsiCompareText(FieldName, FAlias) = 0 then
      Result := Field;
  end
  else
    FComplexConstraint := (AnsiCompareText(Field.FName, FieldName) <> 0) or FComplexConstraint;
end;

procedure TFieldConstraint.EmptyConstraint;
begin
  inherited;

  FField := nil;
end;

procedure TFieldConstraint.UpdateConstraint(const Text: string; const ConstraintErrorMessage: string; Field: TFieldDesc);
begin
  FField := Field;

  inherited UpdateConstraint(Text, ConstraintErrorMessage);
end;

{ TData }

{$IFDEF CRDEBUG}
var
  DataCnt: integer = 0;
{$ENDIF}

constructor TData.Create;
begin
  inherited;

  FEOF := True;
  FBOF := True;
  FFields := TFieldDescs.Create;
  FAutoInitFields := True;
  FRequireEmptyStrToNull := False;
  FSetEmptyStrToNull := False;
  FFilterUseRollBack := False;
  FConstraints := TCRObjectList.Create;
  FFieldConstraints := TCRObjectList.Create;
{$IFDEF CRDEBUG}
  Inc(DataCnt);
  ShareObjectList.Add(Self);
{$ENDIF}
  FStringHeap := TStringHeap.Create;
  FRecordSearch := False;
end;

destructor TData.Destroy;
begin
  Close;

  ClearFields;
  FFields.Free;
  FStringHeap.Free;
  FFilterCondition.Free;
  FConstraints.Free;
  FFieldConstraints.Free;

{$IFDEF CRDEBUG}
  Dec(DataCnt);
  ShareObjectList.Remove(Self);
{$ENDIF}

  inherited;
end;

{ Data }

procedure TData.InitData;
begin
  FBOF := True;
  FEOF := True;
  FRecordCount := 0;
  FRecordNoOffset := 0;
end;

procedure TData.FreeData;
begin
  InitData;
end;

procedure TData.CheckFetched(RecBuf: IntPtr; Field: TFieldDesc);
begin
end;

procedure TData.SetTrimFixedChar(Value: Boolean);
begin
  FTrimFixedChar := Value;
end;

procedure TData.SetTrimVarChar(Value: Boolean);
begin
  FTrimVarChar := Value;
end;

{ Open / Close }

procedure TData.InternalPrepare;
begin
end;

procedure TData.Prepare;
begin
  InternalPrepare;
  Prepared := True; // lost connection
end;

procedure TData.InternalUnPrepare;
begin
end;

procedure TData.UnPrepare;
begin
  if Prepared then begin
    Prepared := False;
    if FAutoInitFields then
      ClearFields;
    InternalUnPrepare;
  end;
end;

procedure TData.InternalOpen(DisableInitFields: boolean = False);
begin
end;

procedure TData.Open;
begin
  if not Active then begin
    InitData;
    try
      InternalOpen;

      try
        if (FFilterCondition <> nil) and (FFilterCondition.Text <> '') then
          FFilterCondition.CreateExpression;
      except
        InternalClose; // Bug with invalid filtertext on second openning

        raise;
      end;
    except
      FreeData;
      if FFilterCondition <> nil then
        FFilterCondition.FreeExpression;
      raise;
    end;
    Active := True;
  end;
end;

procedure TData.InternalClose;
begin
end;

procedure TData.Close;
begin
  try
    if Active then
      InternalClose;
  finally
    Active := False;
    FreeData;         // FreeData after for multithreads

    if FAutoInitFields and not Prepared then // After FreeData!
      ClearFields;

    if FFilterCondition <> nil then
      FFilterCondition.FreeExpression;
  end;
end;

function TData.IsFullReopen: boolean;
begin
  Result := True;
end;

procedure TData.Reopen;
begin
  Close;
  Open;
end;

{ Field }

class function TData.GetBufferSize(DataType: Word; DataLen: integer): integer;
begin
  case DataType of
    dtBoolean:
      Result := SizeOf(WordBool);
    dtInt8, dtUInt8:
      Result := SizeOf(word);
    dtInt16, dtUInt16:
      Result := SizeOf(smallint);
    dtInt32, dtUInt32:
      Result := SizeOf(integer);
    dtInt64, dtUInt64:
      Result := SizeOf(int64);

    dtCurrency:
      Result := SizeOf(double);
    dtSingle:
      Result := SizeOf(single);
    dtFloat:
      Result := SizeOf(double);
    dtExtended:
      Result := SizeOf(extended);
    dtBCD:
      Result := SizeOf(currency);
    dtFmtBCD:
      Result := SizeOfTBcd;

    dtDateTime, dtDate, dtTime:
      Result := SizeOf(TDateTime);
    dtSQLTimeStamp:
      Result := SizeOf(TSQLTimeStamp);
    dtSQLTimeStampOffset:
      Result := SizeOf(TSQLTimeStampOffset);


    dtString:
      Result := SizeOf(Word) {Size} + DataLen + 1;
    dtWideString:
      Result := SizeOf(Word) {Size} + (DataLen + 1) * SizeOf(WideChar);
    dtExtString, dtExtWideString:
      Result := SizeOf(Word) {Size} + SizeOf(IntPtr); {pointer to StringHeap}
    dtBytes, dtVarBytes:
      Result := SizeOf(Word) {Size} + DataLen {Data};
    dtExtVarBytes:
      Result := SizeOf(Word) {Size} + SizeOf(IntPtr); {pointer to StringHeap}
    dtGuid:
      Result := SizeOf(Word) {Size} + 38 + 1;

    dtBlob, dtMemo, dtWideMemo:
      Result := SizeOf(TBlob);
    dtVariant:
      Result := SizeOf(TVariantObject);
    dtArray:
      Result := SizeOf(IntPtr);
  else
    Result := SizeOf(IntPtr);
  end;
end;

function TData.GetFieldDescType: TFieldDescClass;
begin
  Result := TFieldDesc;
end;

function TData.CreateFieldDesc: TFieldDesc;
begin
  Result := GetFieldDescType.Create(TRecordSetClass(Self.ClassType));
end;

procedure TData.CreateFieldDescs;
begin
end;

function TData.GetArrayFieldName(ObjectType: TObjectType; ItemIndex: integer): string;
begin
  Result := '[' + IntToStr(ItemIndex) + ']';
end;

procedure TData.InitObjectFields(ObjectType: TObjectType; Parent: TFieldDesc);
var
  i: integer;
  Field: TFieldDesc;
  Item, CountItem: integer;
begin
  if (ObjectType.DataType in [dtObject, dtTable]) or FSparseArrays then
    CountItem := 1
  else begin
    CountItem := ObjectType.Size;
    if CountItem > MaxArrayItem then  // Restriction of array length
      CountItem := MaxArrayItem;
  end;

  for i := 0 to ObjectType.AttributeCount - 1 do begin
    for Item := 0 to CountItem - 1 do begin
      Field := CreateFieldDesc;
      Field.ParentField := Parent;
      if ObjectType.DataType in [dtObject, dtTable] then begin
        Field.Name := ObjectType.Attributes[i].Name;
        if Parent = nil then
          Field.ActualName := Field.Name
        else
          Field.ActualName := Parent.ActualName + '.' + Field.Name;
      end
      else begin
        Field.Name := GetArrayFieldName(ObjectType, Item);
        if Parent = nil then
          Field.ActualName := Field.Name
        else
          Field.ActualName := Parent.ActualName + Field.Name;
      end;

      Field.DataType := ObjectType.Attributes[i].DataType;
      Field.Size := ObjectType.Attributes[i].Size;
      Field.Fixed := ObjectType.Attributes[i].Fixed;
      Field.Length := ObjectType.Attributes[i].Length;
      Field.FieldNo := FFields.Count + 1;
      Field.ObjectType := ObjectType.Attributes[i].ObjectType;
      if Parent <> nil then
        Field.ReadOnly := Parent.ReadOnly;
      FFields.Add(Field);

      if Field.DataType in [dtObject, dtArray] then
        InitObjectFields(Field.ObjectType, Field);
    end;
  end;
end;

procedure TData.InitFields;
begin
  InternalInitFieldDescs;

  InitRecordSize;
  InitCalcDataSize;
  if Assigned(FOnFieldsChanged) then
    FOnFieldsChanged;
end;

procedure TData.InitRecordSize;
var
  i: integer;
  Align: integer;
  FieldDesc: TFieldDesc;
begin
  FDataSize := 0;
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := FFields[i];
    if (FieldDesc.FieldDescKind <> fdkCalculated) and not FieldDesc.HasParent then begin
      FieldDesc.Offset := FDataSize;

      if FieldDesc.DataType = dtWideString then begin
        Align := FieldDesc.Offset and 1;
        FieldDesc.Offset := FieldDesc.Offset + Align; // align 2
      end
      else
        Align := 0;

      if FieldDesc.HasValueLen then
        FieldDesc.DataOffset := FieldDesc.Offset + SizeOf(Word)
      else
        FieldDesc.DataOffset := FieldDesc.Offset;

      FDataSize := FDataSize + FieldDesc.Size + Align;
    end;
  end;

  FRecordSize := FDataSize + GetIndicatorSize;
  Align := FRecordSize and 1;
  FRecordSize := FRecordSize + Align; // align 2
end;

procedure TData.InitCalcDataSize;
var
  i: integer;
  Align: integer;
  FieldDesc: TFieldDesc;
  CalcFieldCount: integer;
begin
  FCalcDataSize := 0;
  CalcFieldCount := 0;
  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := FFields[i];
    if FieldDesc.FieldDescKind = fdkCalculated then begin
      FieldDesc.Offset := FRecordSize + FCalcDataSize;

      if FieldDesc.DataType = dtWideString then begin
        Align := FieldDesc.Offset and 1;
        FieldDesc.Offset := FieldDesc.Offset + Align; // align 2
      end
      else
        Align := 0;

      if FieldDesc.HasValueLen then
        FieldDesc.DataOffset := FieldDesc.Offset + SizeOf(Word)
      else
        FieldDesc.DataOffset := FieldDesc.Offset;

      FCalcDataSize := FCalcDataSize + FieldDesc.Size + Align;
      Inc(CalcFieldCount);
      FieldDesc.ActualFieldNo := CalcFieldCount;
    end;
  end;

  FCalcRecordSize := FCalcDataSize + CalcFieldCount * GetIndicatorItemSize;
  if FCalcRecordSize > 0 then begin
    Align := FCalcRecordSize and 1;
    FCalcRecordSize := FCalcRecordSize + Align; // align 2
  end;
end;

function TData.GetIndicatorItemSize: Integer;
begin
  Result := 1;
end;

function TData.GetIndicatorSize: Integer;
begin
  Result := FFields.Count * GetIndicatorItemSize;
end;

procedure TData.ExplicitInitFields;
begin
  InitFields;
end;

procedure TData.ClearFields;
begin
  FFields.Clear;

  if Assigned(FOnFieldsChanged) then
    FOnFieldsChanged;
end;

class procedure TData.GetDateFromBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat);
var
  DateTime: double;
begin
  case Format of
    dfMSecs: begin
      DateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Buf));
      DateTime := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
      Marshal.WriteInt64(Date, BitConverter.DoubleToInt64Bits(DateTime));
    end;
    dfDateTime:
      Marshal.WriteInt64(Date, Marshal.ReadInt64(Buf));
    dfDate: begin
      DateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Buf));
      Marshal.WriteInt32(Date, DateTimeToTimeStamp(DateTime).Date);
    end;
    dfTime: begin
      DateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Buf));
      Marshal.WriteInt32(Date, DateTimeToTimeStamp(DateTime).Time);
    end;
  end;
end;

class procedure TData.PutDateToBuf(Buf: IntPtr; Date: IntPtr; HasParent: boolean; Format: TDateFormat);
var
  Ts: TTimeStamp;
  DateTime: TDateTime;
begin
  case Format of
    dfMSecs: begin
    {$IFDEF FPC}
      DateTime := MemUtils.TimeStampToDateTime(MSecsToTimeStamp(Trunc(Double(Date^))));
    {$ELSE}
      DateTime := MemUtils.TimeStampToDateTime(MSecsToTimeStamp(TDateTime(Date^)));
    {$ENDIF}
    end;
    dfDateTime:
      DateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Date));
    dfDate: begin
      Ts.Date := Marshal.ReadInt32(Date);
      Ts.Time := 0;
      DateTime := MemUtils.TimeStampToDateTime(Ts);
    end;
    dfTime: begin
      Ts.Date := DateDelta;
      Ts.Time := Marshal.ReadInt32(Date);
      DateTime := MemUtils.TimeStampToDateTime(Ts);
    end;
    else
      DateTime := 0;
  end;
  Marshal.WriteInt64(Buf, BitConverter.DoubleToInt64Bits(DateTime));
end;

procedure TData.GetChildFieldInfo(Field: TFieldDesc; out RootField: TFieldDesc; out AttrName: string);
begin
  AttrName := '';
  repeat
    if AttrName = '' then
      AttrName := Field.Name
    else
      if Field.DataType = dtArray then
        AttrName := Field.Name + AttrName
      else
        AttrName := Field.Name + '.' + AttrName;
    Field := Field.ParentField;
  until not Field.HasParent;
  RootField := Field;
end;

procedure TData.GetChildField(Field: TFieldDesc; RecBuf: IntPtr; out DataBuf: IntPtr; out DataLen: Word; out IsBlank, NativeBuffer: boolean);
var
  DBObject: IntPtr;
  RootField: TFieldDesc;
  AttrName: string;
begin
  GetChildFieldInfo(Field, RootField, AttrName);
  DBObject := Marshal.ReadIntPtr(RecBuf, RootField.DataOffset);
  if DBObject <> nil then
    TDBObject(GetGCHandleTarget(DBObject)).GetAttributeValue(AttrName, DataBuf, DataLen, IsBlank, NativeBuffer)
  else begin
    IsBlank := True;
    NativeBuffer := True;
    DataBuf := nil;
    DataLen := 0;
  end;
end;

procedure TData.PutChildField(Field: TFieldDesc; RecBuf: IntPtr; ValuePtr: IntPtr; ValueLen: Word);
var
  DBObject: IntPtr;
  RootField: TFieldDesc;
  AttrName: string;
begin
  GetChildFieldInfo(Field, RootField, AttrName);
  DBObject := Marshal.ReadIntPtr(RecBuf, RootField.DataOffset);
  if DBObject <> nil then
    TDBObject(GetGCHandleTarget(DBObject)).SetAttributeValue(AttrName, ValuePtr, ValueLen);
end;

function TData.GetChildFieldIsNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
var
  DBObject: IntPtr;
  RootField: TFieldDesc;
  AttrName: string;
begin
  GetChildFieldInfo(Field, RootField, AttrName);
  DBObject := Marshal.ReadIntPtr(RecBuf, RootField.DataOffset);
  if DBObject <> nil then
    Result := TDBObject(GetGCHandleTarget(DBObject)).GetAttrIsNull(AttrName)
  else
    Result := True;
end;

const
  CRLF = $0A0D;
  LF   = $0A;
  CRLF_UTF16 = $000A000D;
  LF_UTF16   = $000A;
  CRLF_BE = $0A000D00;
  LF_BE   = $0A00;

function AddCRString(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
var
  SourceEnd, DestStart, DestEnd: IntPtr;
  w: Word;
  b: byte;
begin
  DestStart := Dest;
  SourceEnd := PtrOffset(Source, SourceLen);
  // Count does not include zero terminator
  DestEnd   := PtrOffset(Dest, DestLen);
  while (PtrCompare(Source, SourceEnd) < 0) and
        (PtrCompare(Dest, DestEnd) < 0)
  do begin
    w := Marshal.ReadInt16(Source);
    if w = CRLF then begin
      Marshal.WriteInt16(Dest, w);
      Source := PtrOffset(Source, 2);
      Dest   := PtrOffset(Dest, 2);
    end
    else begin
      b := Byte(w);
      if b = 0 then
        break
      else
      if b = LF then begin
        Marshal.WriteInt16(Dest, CRLF);
        Source := PtrOffset(Source, 1);
        Dest   := PtrOffset(Dest, 2);
      end
      else begin
        Marshal.WriteByte(Dest, b);
        Source := PtrOffset(Source, 1);
        Dest   := PtrOffset(Dest, 1);
      end;
    end;
  end;
  if PtrCompare(Dest, DestEnd) > 0 then // last char was CRLF
    Dest := DestEnd;
  Marshal.WriteByte(Dest, 0);
  Result := PtrSubstract(Dest, DestStart);
end;

function RemoveCRString(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
var
  SourceEnd, DestStart, DestEnd: IntPtr;
  w: Word;
begin
  DestStart := Dest;
  SourceEnd := PtrOffset(Source, SourceLen);
  DestEnd   := PtrOffset(Dest, DestLen);
  while (PtrCompare(Source, SourceEnd) < 0) and
        (PtrCompare(Dest, DestEnd) < 0)
  do begin
    w := Marshal.ReadInt16(Source);
    if w = CRLF then begin
      Marshal.WriteByte(Dest, LF);
      Source := PtrOffset(Source, 2);
      Dest   := PtrOffset(Dest, 1);
    end
    else
    begin
      Marshal.WriteByte(Dest, Byte(w));
      Source := PtrOffset(Source, 1);
      Dest   := PtrOffset(Dest, 1);
    end;
  end;
  Marshal.WriteByte(Dest, 0);
  Result := PtrSubstract(Dest, DestStart);
end;

function AddCRUnicode(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
var
  SourceEnd, DestStart, DestEnd: IntPtr;
  w: Cardinal;
  b: Word;
begin
  DestStart := Dest;
  SourceEnd := PtrOffset(Source, SourceLen * 2);
  // Count does not include zero terminator
  DestEnd := PtrOffset(Dest, DestLen * 2);
  while (PtrCompare(Source, SourceEnd) < 0) and
        (PtrCompare(Dest, DestEnd) < 0)
  do begin
    w := Marshal.ReadInt32(Source);
    if w = CRLF_UTF16 then begin
      Marshal.WriteInt32(Dest, w);
      Source := PtrOffset(Source, 4);
      Dest   := PtrOffset(Dest, 4);
    end
    else begin
      b := Word(w);
      if b = 0 then
        break
      else
      if b = LF_UTF16 then begin
        Marshal.WriteInt32(Dest, CRLF_UTF16);
        Source := PtrOffset(Source, 2);
        Dest   := PtrOffset(Dest, 4);
      end
      else begin
        Marshal.WriteInt16(Dest, b);
        Source := PtrOffset(Source, 2);
        Dest   := PtrOffset(Dest, 2);
      end;
    end;
  end;
  if PtrCompare(Dest, DestEnd) > 0 then // last char was CRLF_UTF16
    Dest := DestEnd;
  Marshal.WriteInt16(Dest, 0);
  Result := PtrSubstract(Dest, DestStart) shr 1;
end;

function RemoveCRUnicode(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
var
  SourceEnd, DestStart, DestEnd: IntPtr;
  w: Cardinal;
begin
  DestStart := Dest;
  SourceEnd := PtrOffset(Source, SourceLen * 2);
  DestEnd   := PtrOffset(Dest, DestLen * 2);
  while (PtrCompare(Source, SourceEnd) < 0) and
        (PtrCompare(Dest, DestEnd) < 0)
  do begin
    w := Marshal.ReadInt32(Source);
    if w = CRLF_UTF16 then begin
      Marshal.WriteInt16(Dest, LF_UTF16);
      Source := PtrOffset(Source, 4);
      Dest   := PtrOffset(Dest, 2);
    end
    else
    begin
      Marshal.WriteInt16(Dest, Word(w));
      Source := PtrOffset(Source, 2);
      Dest   := PtrOffset(Dest, 2);
    end;
  end;
  Marshal.WriteInt16(Dest, 0);
  Result := PtrSubstract(Dest, DestStart) shr 1;
end;

function AddCRBigEndian(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
var
  SourceEnd, DestStart, DestEnd: IntPtr;
  w: Cardinal;
  b: Word;
begin
  DestStart := Dest;
  SourceEnd := PtrOffset(Source, SourceLen * 2);
  // Count does not include zero terminator
  DestEnd := PtrOffset(Dest, DestLen * 2);
  while (PtrCompare(Source, SourceEnd) < 0) and
        (PtrCompare(Dest, DestEnd) < 0)
  do begin
    w := Marshal.ReadInt32(Source);
    if w = CRLF_BE then begin
      Marshal.WriteInt32(Dest, w);
      Source := PtrOffset(Source, 4);
      Dest   := PtrOffset(Dest, 4);
    end
    else begin
      b := Word(w);
      if b = 0 then
        break
      else
      if b = LF_UTF16 then begin
        Marshal.WriteInt32(Dest, CRLF_BE);
        Source := PtrOffset(Source, 2);
        Dest   := PtrOffset(Dest, 4);
      end
      else begin
        Marshal.WriteInt16(Dest, b);
        Source := PtrOffset(Source, 2);
        Dest   := PtrOffset(Dest, 2);
      end;
    end;
  end;
  if PtrCompare(Dest, DestEnd) > 0 then // last char was CRLF_UTF16
    Dest := DestEnd;
  Marshal.WriteInt16(Dest, 0);
  Result := PtrSubstract(Dest, DestStart) shr 1;
end;

function RemoveCRBigEndian(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: integer): integer;
var
  SourceEnd, DestStart, DestEnd: IntPtr;
  w: Cardinal;
begin
  DestStart := Dest;
  SourceEnd := PtrOffset(Source, SourceLen * 2);
  DestEnd   := PtrOffset(Dest, DestLen * 2);
  while (PtrCompare(Source, SourceEnd) < 0) and
        (PtrCompare(Dest, DestEnd) < 0)
  do begin
    w := Marshal.ReadInt32(Source);
    if w = CRLF_BE then begin
      Marshal.WriteInt16(Dest, LF_BE);
      Source := PtrOffset(Source, 4);
      Dest   := PtrOffset(Dest, 2);
    end
    else
    begin
      Marshal.WriteInt16(Dest, Word(w));
      Source := PtrOffset(Source, 2);
      Dest   := PtrOffset(Dest, 2);
    end;
  end;
  Marshal.WriteInt16(Dest, 0);
  Result := PtrSubstract(Dest, DestStart) shr 1;
end;

function TData.NeedConvertEOL: boolean;
begin
  Result := False;
end;

procedure TData.GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean);
var
{$IFNDEF VER14P}{$IFNDEF LITE}
  d: Double;
{$ENDIF}{$ENDIF}
  DataType: Word;
  DataSize: Integer;
begin
  DataType := Field.DataType;

  case DataType of
  {$IFNDEF VER14P}{$IFNDEF LITE}
    dtSingle: begin
      d := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(DataBuf));
      Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(d));
    end;
  {$ENDIF}{$ENDIF}
    dtUInt32:
    {$IFDEF VER12P}
      Marshal.WriteInt32(Dest, Marshal.ReadInt32(DataBuf));
    {$ELSE}
      Marshal.WriteInt64(Dest, Cardinal(Marshal.ReadInt32(DataBuf)));
    {$ENDIF}
    dtDateTime:
      GetDateFromBuf(DataBuf, Dest, Field.HasParent, dfMSecs);
    dtDate:
      GetDateFromBuf(DataBuf, Dest, Field.HasParent, dfDate);
    dtTime:
      GetDateFromBuf(DataBuf, Dest, Field.HasParent, dfTime);
    dtVariant: begin
    {$IFDEF VER21P}
      PVarData(Dest).VType := 0;
    {$ENDIF}
      Variant(Dest^) := TVariantObject(InternalGetObject(DataBuf)).Value;
    end;
    dtFmtBCD:
      CopyBuffer(DataBuf, Dest, SizeOfTBcd); // To avoid errors if Field.Size > SizeOfTBcd
    dtString, dtExtString: begin
      if DataType = dtExtString then
        DataBuf := PIntPtr(DataBuf)^;
      if NeedConvertEOL then
        DataLen := AddCRString(DataBuf, DataLen, Dest, Field.Length)
      else begin
        Move(DataBuf^, Dest^, DataLen);
        Marshal.WriteByte(Dest, DataLen, 0);
      end;
    end;
    dtWideString, dtExtWideString: begin
      if DataType = dtExtWideString then
        DataBuf := PIntPtr(DataBuf)^;
      if NeedConvertEOL then
        DataLen := AddCRUnicode(DataBuf, DataLen, Dest, Field.Length)
      else begin
        DataSize := DataLen * 2;
        Move(DataBuf^, Dest^, DataSize);
        Marshal.WriteInt16(Dest, DataSize, 0);
      end;
    end;
    dtBytes:
      Move(DataBuf^, Dest^, DataLen);
    dtVarBytes, dtExtVarBytes: begin
      PWord(Dest)^ := DataLen;
      Dest := PtrOffset(Dest, SizeOf(Word));
      if DataType = dtExtVarBytes then
        DataBuf := PIntPtr(DataBuf)^;
      Move(DataBuf^, Dest^, DataLen);
    end;
    dtGuid: begin
      Move(DataBuf^, Dest^, DataLen);
      Marshal.WriteByte(Dest, DataLen, 0);
    end
    else
      if Field.IsBlob then
        Move(DataBuf^, Dest^, sizeof(IntPtr))
      else
        Move(DataBuf^, Dest^, Field.Size);
  end;
end;

procedure TData.GetField(Field: TFieldDesc; RecBuf: IntPtr; Dest: IntPtr; out DestLen: Word; NeedConvert: boolean; out IsBlank: boolean);
var
  DataBuf: IntPtr;
  NativeBuffer: boolean;
  t: boolean;
begin
  DestLen := 0;

  IsBlank := GetNull(Field, RecBuf);

  if (Dest = nil) or IsBlank and
     (not Field.IsComplex or (Field.DataType in [dtExtString, dtExtWideString, dtExtVarBytes, dtVariant]))
  then
    Exit;

  if not Field.HasParent then begin
    DataBuf := PtrOffset(RecBuf, Field.DataOffset);
    if Field.HasValueLen then
      DestLen := Marshal.ReadUInt16(RecBuf, Field.Offset);
    GetFieldData(Field, DataBuf, DestLen, Dest, NeedConvert);
  end
  else begin
    GetChildField(Field, RecBuf, DataBuf, DestLen, IsBlank, NativeBuffer);
    if DataBuf <> nil then
      if NativeBuffer then
        GetFieldData(Field, DataBuf, DestLen, Dest, NeedConvert)
      else
        try
          GetFieldData(Field, DataBuf, DestLen, Dest, NeedConvert)
        finally
          Marshal.FreeHGlobal(DataBuf);
        end;
  end;

  if not IsBlank and (DestLen > 0) then
    case Field.MapDataType of
      dtString, dtExtString: begin
        if Field.Fixed then
          t := FTrimFixedChar
        else
          t := FTrimVarChar;
        if t then
          DestLen := StrTrim(Dest, DestLen);
      end;
      dtWideString, dtExtWideString: begin
        if Field.Fixed then
          t := FTrimFixedChar
        else
          t := FTrimVarChar;
        if t then
          DestLen := StrTrimW(Dest, DestLen);
      end;
      dtBytes:
        FillChar(PtrOffset(Dest, DestLen), Field.MapLength - DestLen, 0);
    end;
end;

procedure TData.InternalInitFieldDescs;
  {
    procedure ReplaceNextOriginalNames(StartName: string; StartInd: integer);
    var
      i, Res: integer;
      AliasNum: integer;
      S: string;
    begin
      AliasNum := 1;
      for i := StartInd to AFields.Count - 1 do begin
        S := TFieldDesc(AFields[i]).Name;
        Res := _CompareText(StartName, S);
        if (Res < 0) then
          break;
        if (Res = 0) then begin
          TFieldDesc(AFields[i]).Name := S + UniqueFieldIndexSeparator + IntToStr(AliasNum);
          Inc(AliasNum);
          ReplaceNextOriginalNames(TFieldDesc(AFields[i]).Name, i + 1);
        end;
      end;
    end;
  }

  // perfomance optimization for many fields set aliases
  procedure InitAliases;
  var
    i: integer;
    s: string;
    Field: TFieldDesc;
    FieldList: TList;
    AliasNum, AliasLen: integer;
  begin
    FieldList := TList.Create;
    try
      FieldList.Capacity := FFields.Capacity;
      for i := 0 to FFields.Count - 1 do begin
        Field := FFields[i];
        if (Field <> nil) and (Field.ParentField = nil) then
          FieldList.Add(Field);
      end;

      FieldList.Sort(CompareAlias);
      AliasNum := 0;
      for i := 0 to FieldList.Count - 1 do begin
        Field := TFieldDesc(FieldList[i]);
        if (Field.FReserved) or (Field.Name = '') then begin
          if AliasNum > 1 then begin
            s := TFieldDesc(FieldList[i - 1]).Name;
            AliasLen := Length(UniqueFieldIndexSeparator) + Length(IntToStr((AliasNum - 1)));
            SetLength(s, Length(s) - AliasLen);
            if not SameText(s, Field.Name) then
              AliasNum := 0;
          end;
          if (AliasNum <> 0) or (Field.Name = '') then begin
            s := Field.Name + UniqueFieldIndexSeparator + IntToStr(AliasNum);
            while FindField(s) <> nil do begin
              Inc(AliasNum);
              s := Field.Name + UniqueFieldIndexSeparator + IntToStr(AliasNum);
            end;
            Field.Name := s;
            // ReplaceNextOriginalNames(TFieldDesc(AFields[i]).Name, i + 1);
          end;
          Inc(AliasNum);
        end
        else
          AliasNum := 0;
      end;
    finally
      FieldList.Free;
    end;
  end;

var
  OldOnFieldsChanged: TOnFieldsChanged;
begin
  if FAutoInitFields then begin
    OldOnFieldsChanged := FOnFieldsChanged;
    FOnFieldsChanged := nil;
    try
      ClearFields;
    finally
      FOnFieldsChanged := OldOnFieldsChanged;
    end;
    CreateFieldDescs;
    if Assigned(FOnGetCachedFields) then
      FOnGetCachedFields();
    InitAliases;
  end;

  CheckHasBlobFields;
  CheckHasComplexFields;
end;

function TData.IsEqualDataType(Field1: TFieldDesc; Field2: TFieldDesc): boolean;
begin
  Result := (Field1.DataType = Field2.DataType) and
    ((Field1.DataType <> dtBytes) or (Field1.Length = Field2.Length)) and
    not Field1.IsBlob;
end;

function TData.GetDataBuf(RecBuf: IntPtr; FieldDesc: TFieldDesc; out DataLenPtr: PWord): IntPtr;
begin
  Result := PtrOffset(RecBuf, FieldDesc.DataOffset);
  if FieldDesc.HasValueLen then
    DataLenPtr := PtrOffset(RecBuf, FieldDesc.Offset)
  else
    DataLenPtr := nil;
end;

function TData.GetFieldBuf(RecBuf: IntPtr; FieldDesc: TFieldDesc; out DataLen: Word; out IsBlank, NativeBuffer: boolean): IntPtr;
begin
  if FieldDesc.ParentField = nil then begin
    IsBlank := GetNull(FieldDesc, RecBuf);
    Result := PtrOffset(RecBuf, FieldDesc.DataOffset);
    if FieldDesc.HasValueLen then
      DataLen := Marshal.ReadUInt16(RecBuf, FieldDesc.Offset)
    else
      DataLen := 0;
    NativeBuffer := True;
  end
  else
    GetChildField(FieldDesc, RecBuf, Result, DataLen, IsBlank, NativeBuffer);
end;

function TData.GetMappedFieldBuf(RecBuf: IntPtr; FieldDesc: TFieldDesc; out DataLen: Word; out DataType: Word; out HasParent: boolean; out IsFixed: boolean; out IsBlank, NativeBuffer: boolean): IntPtr;
var
  DataBuf: IntPtr;
begin
  DataType := FieldDesc.DataType;
  HasParent := FieldDesc.HasParent;
  IsFixed := FieldDesc.Fixed;

  if not FieldDesc.HasParent then begin
    DataLen := 0;
    NativeBuffer := True;

    IsBlank := GetNull(FieldDesc, RecBuf);
    if IsBlank and (not FieldDesc.IsComplex or (FieldDesc.DataType in [dtExtString, dtExtWideString, dtExtVarBytes])) then
      Result := nil
    else begin
      DataBuf := PtrOffset(RecBuf, FieldDesc.DataOffset);
      if FieldDesc.HasValueLen then
        DataLen := Marshal.ReadUInt16(RecBuf, FieldDesc.Offset);

      Result := GetMappedDataBuf(FieldDesc, DataBuf, DataLen, DataType, HasParent, IsFixed);
    end;
  end
  else
    GetChildField(FieldDesc, RecBuf, Result, DataLen, IsBlank, NativeBuffer);
end;

function TData.GetMappedDataBuf(FieldDesc: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; var DataType: Word; var HasParent, IsFixed: boolean): IntPtr;
begin
  Result := DataBuf;
end;

function SetScale(D: Double; Scale: Integer): Double;
type
  TWordArray = array[0..3] of Word;
  PWordArray = ^TWordArray;
var
  hhword: Word;
  exp2: Integer;
  exp10: Double;
begin
  if Scale > 0 then begin
    hhword := PWordArray(@D)[3];
    exp2 := (hhword shr 4) and $7FF;
    if (exp2 > 0) and (exp2 - $3FF < 50) then begin
      exp10 := Exponent10(Scale);
      Result := D * exp10;
      if System.Frac(Result) <> 0 then begin
        if hhword and $8000 = 0 then
          Result := Result + 0.5
        else
          Result := Result - 0.5;
        Result := System.Int(Result) / exp10;
      end
      else
        Result := D;
    end
    else
      Result := D;
  end
  else
    Result := D;
end;

procedure TData.PutFieldData(Field: TFieldDesc; DataBuf: IntPtr; DataLenPtr: PWord; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False);
var
  d: double;
{$IFNDEF VER14P}{$IFNDEF LITE}
  s: single;
{$ENDIF}{$ENDIF}
  DataType: Word;
  ValueSize: Integer;
  VarObj: TVariantObject;
begin
  DataType := Field.DataType;

  case DataType of
  {$IFNDEF VER14P}{$IFNDEF LITE}
    dtSingle: begin
      s := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
      Marshal.WriteInt32(DataBuf, CRBitConverter.SingleToInt32Bits(s));
    end;
  {$ENDIF}{$ENDIF}
    dtFloat: begin
      if not IsDatabaseValue then begin
        d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
        d := SetScale(d, Field.Scale);
        Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(d));
      end
      else
        Marshal.WriteInt64(DataBuf, Marshal.ReadInt64(ValuePtr));
    end;
    dtDateTime:
      PutDateToBuf(DataBuf, ValuePtr, Field.HasParent, dfMSecs);
    dtDate:
      PutDateToBuf(DataBuf, ValuePtr, Field.HasParent, dfDate);
    dtTime:
      PutDateToBuf(DataBuf, ValuePtr, Field.HasParent, dfTime);
    dtVariant: begin
      VarObj := TVariantObject(InternalGetObject(DataBuf));
      VarObj.Value := Variant(ValuePtr^);
    {$IFDEF VER22P}
      Variant(ValuePtr^) := Unassigned;
    {$ENDIF}
    end;
    dtString, dtExtString: begin
      if ValueLen > Field.Length then
        ValueLen := Field.Length;
      if DataType = dtExtString then begin
        FStringHeap.DisposeBuf(PIntPtr(DataBuf)^);
        PIntPtr(DataBuf)^ := FStringHeap.AllocStr(ValuePtr, ValueLen);
      end
      else begin
        Move(ValuePtr^, DataBuf^, ValueLen);
        Marshal.WriteByte(DataBuf, ValueLen, 0);
      end;
      DataLenPtr^ := ValueLen;
    end;
    dtWideString, dtExtWideString: begin
      if ValueLen > Field.Length then
        ValueLen := Field.Length;
      if DataType = dtExtWideString then begin
        FStringHeap.DisposeBuf(PIntPtr(DataBuf)^);
        PIntPtr(DataBuf)^ := FStringHeap.AllocWideStr(ValuePtr, ValueLen);
      end
      else begin
        ValueSize := ValueLen * 2;
        Move(ValuePtr^, DataBuf^, ValueSize);
        Marshal.WriteInt16(DataBuf, ValueSize, 0);
      end;
      DataLenPtr^ := ValueLen;
    end;
    dtBytes: begin
      if ValueLen > Field.Length then
        ValueLen := Field.Length;
      Move(ValuePtr^, DataBuf^, ValueLen);
      DataLenPtr^ := ValueLen;
    end;
    dtVarBytes, dtExtVarBytes: begin
      if ValuePtr <> nil then begin
        ValueLen := PWord(ValuePtr)^;
        if ValueLen > Field.Length then
          ValueLen := Field.Length;
        ValuePtr := PtrOffset(ValuePtr, SizeOf(Word));
        if DataType = dtExtVarBytes then begin
          FStringHeap.DisposeBuf(PIntPtr(DataBuf)^);
          PIntPtr(DataBuf)^ := FStringHeap.NewBuf(ValueLen);
          DataBuf := PIntPtr(DataBuf)^;
        end;
        Move(ValuePtr^, DataBuf^, ValueLen);
        DataLenPtr^ := ValueLen;
      end
      else begin
        Marshal.WriteIntPtr(DataBuf, nil);
        DataLenPtr^ := 0;
      end;
    end;
    dtGuid: begin
      if ValueLen > Field.Length then
        ValueLen := Field.Length;
      Move(ValuePtr^, DataBuf^, ValueLen);
      Marshal.WriteByte(DataBuf, ValueLen, 0);
      DataLenPtr^ := ValueLen;
    end
  else
    if Field.IsBlob then
      Move(ValuePtr^, DataBuf^, sizeof(IntPtr))
    else
      Move(ValuePtr^, DataBuf^, Field.Size);
  end;
end;

procedure TData.PutField(Field: TFieldDesc; RecBuf: IntPtr; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False);
var
  DataBuf: IntPtr;
  DataLenPtr: PWord;
  IsNullValue: boolean;
begin
  if Assigned(Field.CustomConstraint) and not Field.CustomConstraint.ComplexConstraint then
    CheckConstraint(Field, RecBuf, ValuePtr, ValueLen, Field.CustomConstraint);

  if ValuePtr = nil then begin
    SetNull(Field, RecBuf, True);
    SetChanged(Field, RecBuf, True);
    Exit;
  end;

  if not Field.HasParent then begin
    DataBuf := PtrOffset(RecBuf, Field.DataOffset);
    if Field.HasValueLen then begin
      DataLenPtr := PtrOffset(RecBuf, Field.Offset);
      PutFieldData(Field, DataBuf, DataLenPtr, ValuePtr, ValueLen, NeedConvert, IsDatabaseValue);

      if Field.DataType in [dtString, dtExtString, dtWideString, dtExtWideString] then
        if (DataLenPtr^ = 0) and (FSetEmptyStrToNull or FRequireEmptyStrToNull) then
          IsNullValue := True
        else
          IsNullValue := False
      else
        IsNullValue := False;
      SetNull(Field, RecBuf, IsNullValue);
    end
    else begin
      PutFieldData(Field, DataBuf, nil, ValuePtr, ValueLen, NeedConvert, IsDatabaseValue);
      SetNull(Field, RecBuf, False);
    end;

    SetChanged(Field, RecBuf, True);
  end
  else
    PutChildField(Field, RecBuf, ValuePtr, ValueLen);
end;

function TData.GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
begin
  if not Field.HasParent then begin
    if Field.FieldDescKind <> fdkCalculated then
      Result := Marshal.ReadByte(RecBuf, FDataSize + Field.FieldNo - 1) = 1
    else
      Result := Marshal.ReadByte(RecBuf, FRecordSize + FCalcDataSize + Field.ActualFieldNo - 1) = 1;
  end
  else
    Result := GetChildFieldIsNull(Field, RecBuf);
  if Result then
    Result := GetNullByBlob(Field, RecBuf);
end;

procedure TData.SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
var
  Flag: byte;
  Blob: TBlob;
begin
  if not Field.HasParent then begin
    if Value then
      Flag := 1
    else
      Flag := 0;

    if Field.FieldDescKind <> fdkCalculated then
      Marshal.WriteByte(RecBuf, FDataSize + Field.FieldNo - 1, Flag)
    else
      Marshal.WriteByte(RecBuf, FRecordSize + FCalcDataSize + Field.ActualFieldNo - 1, Flag);

    if Value and Field.IsBlob then begin // clear Blob value
      Blob := TBlob(InternalGetObject(PtrOffset(RecBuf, Field.DataOffset)));
      if Blob <> nil then
        Blob.Clear;
    end;
  end
  else
    PutChildField(Field, RecBuf, nil, 0);
end;

function TData.GetNullByBlob(Field: TFieldDesc; RecBuf: IntPtr): boolean;
var
  Blob: TBlob;
  DataBuf: IntPtr;
  DataLen: Word;
  IsBlank, NativeBuffer: boolean;
begin
  Result := True;
  if Field.IsBlob then begin
    if not Field.HasParent then begin
      DataBuf := PtrOffset(RecBuf, Field.DataOffset);
      NativeBuffer := True;
    end
    else
      GetChildField(Field, RecBuf, DataBuf, DataLen, IsBlank, NativeBuffer);

    try
      Blob := TBlob(InternalGetObject(DataBuf));
    finally
      if not NativeBuffer then
        Marshal.FreeHGlobal(DataBuf);
    end;

    if (Blob <> nil) and (Blob.Size <> 0) then begin
      Result := False;
      SetNull(Field, RecBuf, False);
    end;
  end;
end;

function TData.GetChanged(Field: TFieldDesc; RecBuf: IntPtr): boolean;
begin
  Result := False;
end;

procedure TData.SetChanged(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
begin

end;

procedure TData.GetFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: Variant; UseRollback: boolean = False);
var
  DataBuf: IntPtr;
  DataLen: Word;
  IsBlank, NativeBuffer: boolean;
begin
  if GetNull(Field, RecBuf) then begin
    Value := Null;
    Exit;
  end;

  Value := Unassigned; // Delphi bug

  if not Field.HasParent then begin
    DataBuf := PtrOffset(RecBuf, Field.DataOffset);
    if Field.HasValueLen then
      DataLen := Marshal.ReadUInt16(RecBuf, Field.Offset)
    else
      DataLen := 0;
    GetDataAsVariant(DataBuf, DataLen, Field.DataType, Field.SubDataType, Field.HasParent, Field.Fixed, Value, UseRollback);
  end
  else begin
    GetChildField(Field, RecBuf, DataBuf, DataLen, IsBlank, NativeBuffer);
    if NativeBuffer then
      GetDataAsVariant(DataBuf, DataLen, Field.DataType, Field.SubDataType, Field.HasParent, Field.Fixed, Value, UseRollback)
    else
      try
        GetDataAsVariant(DataBuf, DataLen, Field.DataType, Field.SubDataType, Field.HasParent, Field.Fixed, Value, UseRollback)
      finally
        Marshal.FreeHGlobal(DataBuf);
      end;
  end;
end;

procedure TData.GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType, SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: Variant; UseRollback: boolean);

  procedure BlobToVariant(Blob: TBlob; var V: Variant); {$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    BlobSize: integer;
  begin
    if Blob.IsNull then
      V := Null
    else begin
      BlobSize := Blob.Size;
      V := VarArrayCreate([0, BlobSize - 1], varByte);
      if BlobSize > 0 then
        Blob.Read(0, BlobSize, TVarData(V).VArray.Data);
    end;
  end;

var
  Date: TDateTime;
  Date32: Integer;
  Date64: Int64;
  PDate: IntPtr;
  t: boolean;
  Blob: TBlob;
{$IFDEF VER6}
  ui64: UInt64;
{$ENDIF}
  bcd: TBcd;
  ts: TSQLTimeStamp;
{$IFDEF VER14P}
  tso: TSQLTimeStampOffset;
{$ENDIF}
begin
  case DataType of
    dtString, dtExtString: begin
      if DataType = dtExtString then
        DataBuf := Marshal.ReadIntPtr(DataBuf);
      if IsFixed then
        t := FTrimFixedChar
      else
        t := FTrimVarChar;
      if t then
        // trim fixed char values
        Value := StrTrimmed(DataBuf, DataLen)
      else
        Value := Marshal.PtrToStringAnsi(DataBuf, DataLen);
    end;
    dtWideString, dtExtWideString: begin
      if DataType = dtExtWideString then
        DataBuf := Marshal.ReadIntPtr(DataBuf);
      if IsFixed then
        t := FTrimFixedChar
      else
        t := FTrimVarChar;
      if t then
        // trim fixed char values
        Value := StrTrimmedW(DataBuf, DataLen)
      else
        Value := Marshal.PtrToStringUni(DataBuf, DataLen);
    end;
    dtInt8:
      Value := ShortInt(DataBuf^);
    dtUInt8:
      Value := Byte(DataBuf^);
    dtInt16:
      Value := SmallInt(DataBuf^);
    dtUInt16:
      Value := Word(DataBuf^);
    dtInt32:
      Value := Integer(DataBuf^);
    dtUInt32:
      Value := Cardinal(DataBuf^);
    dtInt64:
      Value := Int64(DataBuf^);
    dtUInt64: begin
    {$IFDEF FPC}
      Value := UInt64(DataBuf^);
    {$ELSE}{$IFDEF VER7P}
      Value := UInt64(DataBuf^);
    {$ELSE}{$IFDEF VER6}
      // Delphi 6 conversion bug
      ui64 := UInt64(DataBuf^);
      Value := ui64;
    {$ENDIF}{$ENDIF}{$ENDIF}
    end;
    dtBoolean:
      Value := WordBool(DataBuf^);
    dtSingle:
      Value := Single(DataBuf^);
    dtFloat, dtCurrency:
      Value := Double(DataBuf^);
    dtExtended:
      Value := Extended(DataBuf^);
    dtDateTime: begin
      PDate := OrdinalToPtr(Date64);
      try
        GetDateFromBuf(DataBuf, PDate, HasParent, dfMSecs);
      finally
        PtrToOrdinal(PDate, Date64);
      end;
    {$IFDEF FPC}
      Date := MemUtils.TimeStampToDateTime(MSecsToTimeStamp(Trunc(BitConverter.Int64BitsToDouble(Date64))));
    {$ELSE}
      Date := MemUtils.TimeStampToDateTime(MSecsToTimeStamp(BitConverter.Int64BitsToDouble(Date64)));
    {$ENDIF}
      Value := Date;
    end;
    dtDate: begin
      PDate := OrdinalToPtr(Date32);
      try
        GetDateFromBuf(DataBuf, PDate, HasParent, dfDate);
      finally
        PtrToOrdinal(PDate, Date32);
      end;
      Date := Date32 - DateDelta;
      Value := Date;
    end;
    dtTime: begin
      PDate := OrdinalToPtr(Date32);
      try
        GetDateFromBuf(DataBuf, PDate, HasParent, dfTime);
      finally
        PtrToOrdinal(PDate, Date32);
      end;
      Date := Date32 / MSecsPerDay;
      Value := Date;
    end;
  {$IFNDEF NEXTGEN}
    dtMemo: begin
      Blob := TBlob(InternalGetObject(DataBuf));
      if UseRollback and not Blob.UseRollback and Blob.CanRollback then begin
        Blob.UseRollback := True;
        try
          if Blob.IsNull then
            Value := Null
          else
            Value := Blob.AsString;
        finally
          Blob.UseRollback := False;
        end;
      end
      else if Blob.IsNull then
        Value := Null
      else
        Value := Blob.AsString;
    end;
  {$ENDIF}
    dtWideMemo{$IFDEF NEXTGEN}, dtMemo{$ENDIF}: begin
      Blob := TBlob(InternalGetObject(DataBuf));
      if UseRollback and not Blob.UseRollback and Blob.CanRollback then begin
        Blob.UseRollback := True;
        try
          if Blob.IsNull then
            Value := Null
          else
            Value := Blob.AsWideString;
        finally
          Blob.UseRollback := False;
        end;
      end
      else if Blob.IsNull then
        Value := Null
      else
        Value := Blob.AsWideString;
    end;
    dtVariant:
      Value := TVariantObject(InternalGetObject(DataBuf)).Value;
    dtBytes: begin
      Value := VarArrayCreate([0, DataLen - 1], varByte);
      Move(DataBuf^, TVarData(Value).VArray.Data^, DataLen);
    end;
    dtVarBytes, dtExtVarBytes: begin
      Value :=  VarArrayCreate([0, DataLen - 1], varByte);
      if DataLen > 0 then begin
        if DataType = dtExtVarBytes then
          DataBuf := PIntPtr(DataBuf)^;
        Move(DataBuf^, TVarData(Value).VArray.Data^, DataLen);
      end;
    end;
    dtBlob: begin
      Blob := TBlob(InternalGetObject(DataBuf));
      if UseRollback and not Blob.UseRollback and Blob.CanRollback then begin
        Blob.UseRollback := True;
        try
          BlobToVariant(Blob, Value);
        finally
          Blob.UseRollback := False;
        end;
      end
      else
        BlobToVariant(Blob, Value);
    end;
    dtBCD:
      Value := PCurrency(DataBuf)^;
    dtFmtBCD: begin
      bcd := PBcd(DataBuf)^;
      Value := VarFMTBcdCreate(bcd);
    end;
    dtSQLTimeStamp: begin
      ts := PSQLTimeStamp(DataBuf)^;
      Value := VarSQLTimeStampCreate(ts);
    end;
    dtSQLTimeStampOffset: begin
    {$IFDEF VER14P}
      tso := PSQLTimeStampOffset(DataBuf)^;
      Value := VarSQLTimeStampOffsetCreate(tso);
    {$ELSE}
      ts := PSQLTimeStamp(DataBuf)^;
      Value := VarSQLTimeStampCreate(ts);
    {$ENDIF}
    end;
    dtGuid: begin
      Value := Marshal.PtrToStringAnsi(DataBuf, DataLen);
      if Value = '' then
        Value := EMPTY_GUID;
    end;
  else
    raise EConvertError.Create(SCannotConvertType + ' ' + IntToStr(Integer(DataType)));
  end;
end;

procedure TData.GetMappedFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: Variant; UseRollback: boolean = False; FlatRecBuf: boolean = False);
var
  DataBuf: IntPtr;
  DataLen: Word;
  DataType: Word;
  HasParent: boolean;
  IsFixed: boolean;
  IsBlank: boolean;
  NativeBuffer: boolean;
begin
  DataBuf := GetMappedFieldBuf(RecBuf, Field, DataLen, DataType, HasParent, IsFixed, IsBlank, NativeBuffer);

  if IsBlank then begin
    Value := Null;
    Exit;
  end;

  if FlatRecBuf then
    case DataType of
      dtExtString:
        DataType := dtString;
      dtExtWideString:
        DataType := dtWideString;
      dtExtVarBytes:
        DataType := dtVarBytes;
    end;

  if NativeBuffer then
    GetDataAsVariant(DataBuf, DataLen, DataType, Field.SubDataType, HasParent, IsFixed, Value, UseRollback)
  else
    try
      GetDataAsVariant(DataBuf, DataLen, DataType, Field.SubDataType, HasParent, IsFixed, Value, UseRollback)
    finally
      Marshal.FreeHGlobal(DataBuf);
    end;
end;

procedure TData.GetMappedDataAsVariant(Field: TFieldDesc; DataBuf: IntPtr; DataLen: Word; var Value: variant; UseRollback: boolean = False; FlatRecBuf: boolean = False);
var
  DataType: Word;
  HasParent: Boolean;
  IsFixed: Boolean;
begin
  DataType := Field.DataType;
  HasParent := Field.HasParent;
  IsFixed := Field.Fixed;

  DataBuf := GetMappedDataBuf(Field, DataBuf, DataLen, DataType, HasParent, IsFixed);

  if FlatRecBuf then
    case DataType of
      dtExtString:
        DataType := dtString;
      dtExtWideString:
        DataType := dtWideString;
      dtExtVarBytes:
        DataType := dtVarBytes;
    end;

  GetDataAsVariant(DataBuf, DataLen, DataType, Field.SubDataType, HasParent, IsFixed, Value, UseRollback);
end;

procedure TData.PutFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; const Value: Variant; IsDatabaseValue: boolean = False);
var
  DataBuf: IntPtr;
  DataLenPtr: PWord;
  DataLen: Word; // Dummy
  IsBlank, NativeBuffer: boolean;
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then begin
    SetNull(Field, RecBuf, True);
    SetChanged(Field, RecBuf, True);
    Exit;
  end;

  if not Field.HasParent then begin
    DataBuf := PtrOffset(RecBuf, Field.DataOffset);
    if Field.HasValueLen then begin
      DataLenPtr := PtrOffset(RecBuf, Field.Offset);
      PutDataAsVariant(DataBuf, DataLenPtr, Field.DataType, Field.Length, Field.Scale, Field.HasParent, Value, IsDatabaseValue);
    end
    else
      PutDataAsVariant(DataBuf, nil, Field.DataType, Field.Size, Field.Scale, Field.HasParent, Value, IsDatabaseValue);
  end
  else begin
    GetChildField(Field, RecBuf, DataBuf, DataLen, IsBlank, NativeBuffer);
    if NativeBuffer then
      PutDataAsVariant(DataBuf, @DataLen, Field.DataType, Field.Length, Field.Scale, Field.HasParent, Value, IsDatabaseValue)
    else
      try
        PutDataAsVariant(DataBuf, @DataLen, Field.DataType, Field.Length, Field.Scale, Field.HasParent, Value, IsDatabaseValue)
      finally
        Marshal.FreeHGlobal(DataBuf);
      end;
  end;

  SetNull(Field, RecBuf, False);
  SetChanged(Field, RecBuf, True);
end;

procedure TData.PutDataAsVariant(DataBuf: IntPtr; DataLenPtr: PWord; DataType: Word; Len, Scale: Word; HasParent: boolean; const Value: Variant; IsDatabaseValue: boolean);
var
  i32: integer;
  i64: Int64;
{$IFDEF VER12P}
  ui64: UInt64;
{$ENDIF}
  lw: Cardinal;
  p: IntPtr;
  ws: WideString;
  sa: AnsiString;
  DataLen: Integer;
  DataSize: Integer;
  DataPtr: IntPtr;
  SafeArray: PVarArray;
begin
////////////////////////////////////////////////////////////////////////////////
//    The problem is 442.65 <> SetScale(442.65, 2) , it means that number that
// loaded from database can be different from value after SetScale function
// In this case user can get error on attempt to edit record with LockMode = lmLockDelayed
// Error message: Record was changed by another users
//   So value IsDatabaseValue notify if value was loaded from database and
// scale checking can be skipped
////////////////////////////////////////////////////////////////////////////////

  case DataType of
    dtString, dtExtString: begin
      sa := AnsiString(Value);
      DataLen := LengthA(sa);
      if DataLen > Len then
        DataLen := Len;
      if DataType = dtExtString then begin
        FStringHeap.DisposeBuf(Marshal.ReadIntPtr(DataBuf));
        DataPtr := FStringHeap.AllocStr(PAnsiChar(sa), DataLen);
        Marshal.WriteIntPtr(DataBuf, DataPtr);
      end
      else begin
        Move(PAnsiChar(sa)^, DataBuf^, DataLen);
        Marshal.WriteByte(DataBuf, DataLen, 0);
      end;
      DataLenPtr^ := DataLen;
    end;
    dtWideString, dtExtWideString: begin
      ws := WideString(Value);
      DataLen := Length(ws);
      if DataLen > Len then
        DataLen := Len;
      if DataType = dtExtWideString then begin
        FStringHeap.DisposeBuf(Marshal.ReadIntPtr(DataBuf));
        DataPtr := FStringHeap.AllocWideStr(PWideChar(ws), DataLen);
        Marshal.WriteIntPtr(DataBuf, DataPtr);
      end
      else begin
        DataSize := DataLen * SizeOf(WideChar);
        Move(PWideChar(ws)^, DataBuf^, DataSize);
        Marshal.WriteInt16(DataBuf, DataSize, 0);
      end;
      DataLenPtr^ := DataLen;
    end;
    dtGuid: begin
      sa := AnsiString(VarToStr(Value));
      DataLen := LengthA(sa);
      if DataLen > Len then
        DataLen := Len;
      Move(PAnsiChar(sa)^, DataBuf^, DataLen);
      Marshal.WriteByte(DataBuf, DataLen, 0);
      DataLenPtr^ := DataLen;
    end;
    dtInt8, dtUInt8: begin
      i32 := Value;
      case Len of
        2:
          Marshal.WriteInt16(DataBuf, i32);
        1:
          Marshal.WriteByte(DataBuf, byte(i32));
        else
          Assert(False);
      end;
    end;
    dtInt16, dtUInt16:
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64,
        varSingle, varDouble, varCurrency
        {$IFDEF FPC}, varQWord{$ELSE}
        {$IFDEF VER12P}, varUInt64{$ENDIF}{$ENDIF}: begin
          i32 := Value;
          Marshal.WriteInt16(DataBuf, smallint(i32));
        end;
      else
        if VarIsStr(Value) then
          Marshal.WriteInt16(DataBuf, smallint(StrToInt(Value)))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtInt32:
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64,
        varSingle, varDouble, varCurrency
        {$IFDEF FPC}, varQWord{$ELSE}
        {$IFDEF VER12P}, varUInt64{$ENDIF}{$ENDIF}:
          Marshal.WriteInt32(DataBuf, Integer(Value));
      else
        if VarIsStr(Value) then
          Marshal.WriteInt32(DataBuf, StrToInt(Value))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtUInt32:
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varShortInt{$IFDEF FPC}, varQWord{$ENDIF}:
          Marshal.WriteInt32(DataBuf, Integer(Value));
        varLongWord, varInt64,
        varSingle, varDouble, varCurrency{$IFDEF VER12P}, varUInt64{$ENDIF}: begin
          // To prevent range-checking error on large values (for example, 4294967295)
          i64 := Value;
          lw := Cardinal(i64);
          Marshal.WriteInt32(DataBuf, Integer(lw));
        end;
      else
        if VarIsStr(Value) then
          Marshal.WriteInt32(DataBuf, Integer(StrToInt(Value)))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtInt64:
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64,
        varSingle, varDouble, varCurrency
        {$IFDEF FPC}, varQWord{$ELSE}
        {$IFDEF VER12P}, varUInt64{$ENDIF}{$ENDIF}: begin
          i64 := Value;
          Marshal.WriteInt64(DataBuf, i64);
        end;
      else
        if VarIsStr(Value) then
          Marshal.WriteInt64(DataBuf, StrToInt(Value))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtUInt64:
      case VarType(Value) of
      {$IFDEF VER12P}
        varUInt64: begin
          ui64 := Value;
          Marshal.WriteInt64(DataBuf, Int64(ui64));
        end;
      {$ELSE}
        varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64,
        varSingle, varDouble
        {$IFDEF FPC}, varQWord{$ELSE}
        {$IFDEF MSWINDOWS}, varCurrency{$ENDIF}{$ENDIF}: begin
          i64 := Value;
          Marshal.WriteInt64(DataBuf, i64);
        end;
      {$ENDIF}
      else
        if VarIsStr(Value) then
          Marshal.WriteInt64(DataBuf, StrToInt(Value))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtBoolean:
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64,
        varSingle, varDouble, varCurrency
        {$IFDEF FPC}, varQWord{$ELSE}
        {$IFDEF VER12P}, varUInt64{$ENDIF}{$ENDIF}: begin
          i32 := Value;
          Marshal.WriteInt16(DataBuf, smallint(i32));
        end;
        varBoolean:
          Marshal.WriteInt16(DataBuf, smallint(boolean(Value)));
      else
        if VarIsStr(Value) then
          Marshal.WriteInt16(DataBuf, smallint(StrToBool(Value)))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtFloat, dtCurrency:
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64
        {$IFDEF FPC}, varQWord{$ELSE}
        {$IFDEF VER12P}, varUInt64{$ENDIF}{$ENDIF}:
          Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(Value));
        varSingle, varDouble, varCurrency:
          if not IsDatabaseValue then
            Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(SetScale(Value, Scale)))
          else
            Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(Value));
      else
        if VarIsStr(Value) then
          if not IsDatabaseValue then
            Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(SetScale(StrToFloat(Value), Scale)))
          else
            Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(StrToFloat(Value)))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtSingle:
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64
        {$IFDEF FPC}, varQWord{$ELSE}
        {$IFDEF VER12P}, varUInt64{$ENDIF}{$ENDIF}:
          Marshal.WriteInt32(DataBuf, CRBitConverter.SingleToInt32Bits(Value));
        varSingle, varDouble, varCurrency:
          if not IsDatabaseValue then
            Marshal.WriteInt32(DataBuf, CRBitConverter.SingleToInt32Bits(SetScale(Value, Scale)))
          else
            Marshal.WriteInt32(DataBuf, CRBitConverter.SingleToInt32Bits(Value));
      else
        if VarIsStr(Value) then
          if not IsDatabaseValue then
            Marshal.WriteInt32(DataBuf, CRBitConverter.SingleToInt32Bits(SetScale(StrToFloat(Value), Scale)))
          else
            Marshal.WriteInt32(DataBuf, CRBitConverter.SingleToInt32Bits(StrToFloat(Value)))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtExtended:
      case VarType(Value) of
        varSmallint, varInteger, varByte, varWord, varLongWord, varShortInt, varInt64
        {$IFDEF FPC}, varQWord{$ELSE}
        {$IFDEF VER12P}, varUInt64{$ENDIF}{$ENDIF}:
          Marshal.Copy(CRBitConverter.ExtendedToBytes(Value), 0, DataBuf, SizeOf(Extended));
        varSingle, varDouble, varCurrency:
          if not IsDatabaseValue then
            Marshal.Copy(CRBitConverter.ExtendedToBytes(SetScale(Value, Scale)), 0, DataBuf, SizeOf(Extended))
          else
            Marshal.Copy(CRBitConverter.ExtendedToBytes(Value), 0, DataBuf, SizeOf(Extended));
      else
        if VarIsStr(Value) then
          if not IsDatabaseValue then
            Marshal.Copy(CRBitConverter.ExtendedToBytes(SetScale(Value, Scale)), 0, DataBuf, SizeOf(Extended))
          else
            Marshal.Copy(CRBitConverter.ExtendedToBytes(Value), 0, DataBuf, SizeOf(Extended))
        else
          raise EConvertError.Create(SCannotConvertType);
      end;
    dtDateTime, dtDate, dtTime: begin
      i64 := BitConverter.DoubleToInt64Bits(Value);
      p := OrdinalToPtr(i64);
      try
        PutDateToBuf(DataBuf, p, HasParent, dfDateTime);
      finally
        FreeOrdinal(p);
      end;
    end;
  {$IFNDEF NEXTGEN}
    dtMemo, dtBlob: // used by ODAC to refresh String as Memo
      TBlob(InternalGetObject(DataBuf)).AsAnsiString := AnsiString(VarToStr(Value));
  {$ENDIF}
    dtWideMemo{$IFDEF NEXTGEN}, dtMemo, dtBlob{$ENDIF}:
      TBlob(InternalGetObject(DataBuf)).AsWideString := VarToWideStr(Value);
    dtVariant:
      TVariantObject(InternalGetObject(DataBuf)).Value := Value;
    dtBytes: begin
      Assert(VarType(Value) = varArray + varByte, 'Invalid VType');
      SafeArray := VarArrayAsPSafeArray(Value);
      DataLen := SafeArray.Bounds[0].ElementCount - SafeArray.Bounds[0].LowBound;
      Assert(DataLen <= Len, 'Invalid data size');
      if DataLen > 0 then
        Move(SafeArray.Data^, DataBuf^, DataLen);
      DataLenPtr^ := DataLen;
    end;
    dtVarBytes: begin
      Assert(VarType(Value) = varArray + varByte, 'Invalid VType');
      SafeArray := VarArrayAsPSafeArray(Value);
      DataLen := SafeArray.Bounds[0].ElementCount - SafeArray.Bounds[0].LowBound;
      Assert(DataLen <= Len, 'Invalid data size');
      if DataLen > 0 then
        Move(SafeArray.Data^, DataBuf^, DataLen);
      DataLenPtr^ := DataLen;
    end;
    dtExtVarBytes: begin
      Assert(VarType(Value) = varArray + varByte, 'Invalid VType');
      SafeArray := VarArrayAsPSafeArray(Value);
      DataLen := SafeArray.Bounds[0].ElementCount - SafeArray.Bounds[0].LowBound;
      // Assert(VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1 <= Length, 'Invalid data size');
      FStringHeap.DisposeBuf(Marshal.ReadIntPtr(DataBuf));
      DataPtr := FStringHeap.NewBuf(DataLen);
      if DataLen > 0 then
        Move(SafeArray.Data^, DataPtr^, DataLen);
      Marshal.WriteIntPtr(DataBuf, DataPtr);
      DataLenPtr^ := DataLen;
    end;
    dtBCD:
      PCurrency(DataBuf)^ := Value;
    dtFmtBCD:
      PBcd(DataBuf)^ := StrToBcd(Value);
    dtSQLTimeStamp:
      PSQLTimeStamp(DataBuf)^ := VarToSQLTimeStamp(Value);
    dtSQLTimeStampOffset:
    {$IFDEF VER14P}
      PSQLTimeStampOffset(DataBuf)^ := VarToSQLTimeStampOffset(Value);
    {$ELSE}
      PSQLTimeStamp(DataBuf)^ := VarToSQLTimeStamp(Value);
    {$ENDIF}
  else
    raise EConvertError.Create(SCannotConvertType);
  end;
end;

function TData.FindField(const Name: string): TFieldDesc;
begin
  Result := FFields.FindField(Name);
end;

function TData.FieldByName(const Name: string): TFieldDesc;
begin
  Result := FFields.FieldByName(Name);
end;

class function TData.IsBlobDataType(DataType: word): boolean; // TBlob descendants - dtBlob, dtMemo etc
begin
  Result := DataType in [dtBlob, dtMemo, dtWideMemo];
end;

class function TData.IsObjectDataType(DataType: word): boolean;
begin
  Result := False;
end;

class function TData.IsSharedObjectDataType(DataType: word): boolean;
begin
  Result := IsBlobDataType(DataType) or IsObjectDataType(DataType);
end;

class function TData.IsComplexDataType(DataType: word): boolean; // All supported complex field types (BlobFieldTypes, ExtFieldTypes and TSharedObject descendants (not BLOB))
begin
  case DataType of
    dtExtString, dtExtWideString, dtExtVarBytes, dtVariant:
      Result := True;
  else
    Result := IsSharedObjectDataType(DataType);
  end;
end;

class function TData.HasValueLen(DataType: word): boolean;
begin
  case DataType of
    dtFixedChar, dtString, dtExtString,
    dtFixedWideChar, dtWideString, dtExtWideString,
    dtBytes, dtVarBytes, dtExtVarBytes,
    dtGuid:
      Result := True;
  else
    Result := False;
  end;
end;

function TData.HasFields(FieldTypes: TFieldTypeSet): boolean;
var
  i: integer;
begin
  i := 0;
  while (i < FFields.Count) and not (FFields[i].DataType in FieldTypes) do
    Inc(i);
  Result := i < FFields.Count;
end;

function TData.CheckHasBlobFields: boolean;
var
  i: integer;
begin
  i := 0;
  while (i < FFields.Count) and not FFields[i].IsBlob do
    Inc(i);
  Result := i < FFields.Count;

  FHasBlobFields := Result;
end;

function TData.CheckHasComplexFields: boolean;
var
  i: integer;
begin
  i := 0;
  while (i < FFields.Count) and not FFields[i].IsComplex do
    Inc(i);
  Result := i < FFields.Count;

  FHasComplexFields := Result;
end;

function TData.FieldListDependsOnParams: boolean;
begin
  Result := False;
end;

{ Records }

procedure TData.AllocRecBuf(out RecBuf: IntPtr);
begin
  RecBuf := Marshal.AllocHGlobal(RecordSize);
end;

procedure TData.FreeRecBuf(RecBuf: IntPtr);
begin
  Marshal.FreeHGlobal(RecBuf);
end;

procedure TData.CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Field: TFieldDesc;
begin
  if HasComplexFields then
    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if Field.IsComplex and
        (not Field.IsBlob or WithBlob) and
        (not Field.HasParent) and
        (Field.FieldDescKind <> fdkCalculated)
      then
        CreateComplexField(RecBuf, Field);
    end;
end;

procedure TData.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Obj: TSharedObject;
begin
  case Field.DataType of
    dtBlob, dtMemo, dtWideMemo: begin
      Obj := TCompressedBlob.Create; // UniDAC require TCompressedBlob
      if Field.DataType = dtWideMemo then
        TBlob(Obj).IsUnicode := True;
      // RollBack is always on for LOB fields. Otherwise modification
      // that cannot be canceled is possible.
      TBlob(Obj).EnableRollback;
      SetBlob(Field, RecBuf, TBlob(Obj));
    end;
    dtVariant: begin
      Obj := TVariantObject.Create;
      Marshal.WriteIntPtr(RecBuf, Field.DataOffset, Obj.GCHandle);
    end;
    dtExtString, dtExtWideString, dtExtVarBytes:
      Marshal.WriteIntPtr(RecBuf, Field.DataOffset, nil);
  end;
end;

procedure TData.AddRefComplexFields(RecBuf: IntPtr; CreateBlob: boolean = False);
var
  i: integer;
  so: TSharedObject;
  Field: TFieldDesc;
  Data: PIntPtr;
begin
  if HasComplexFields then
    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if not Field.HasParent and (Field.FieldDescKind <> fdkCalculated) then begin
        if Field.DataType in [dtExtString, dtExtWideString, dtExtVarBytes] then begin
          Data := PtrOffset(RecBuf, Field.DataOffset);
          FStringHeap.AddRef(Data^);
        end
        else
        if Field.IsComplex then begin
          Data := PtrOffset(RecBuf, Field.DataOffset);
          so := TSharedObject(InternalGetObject(Data));
          if (so = nil) and CreateBlob then
            CreateComplexField(RecBuf, Field)
          else
            // Assert(so <> nil, 'Shared object for ' + Field.Name + ' = nil');
            if so <> nil then
              so.AddRef;
        end;
      end;
    end;
end;

procedure TData.FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Field: TFieldDesc;
begin
  if HasComplexFields then
    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if Field.IsComplex and
        (not Field.IsBlob or WithBlob) and
        (not Field.HasParent) and
        (Field.FieldDescKind <> fdkCalculated)
      then
        FreeComplexField(RecBuf, Field);
    end;
end;

procedure TData.FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  DataBuf: PIntPtr;
  ValueBuf: IntPtr;
  so: TSharedObject;
  b: boolean;
begin
  case Field.DataType of
    dtBlob, dtMemo, dtWideMemo, dtVariant: begin
      DataBuf := PtrOffset(RecBuf, Field.DataOffset);
      ValueBuf := DataBuf^;
      if ValueBuf <> nil then begin
        so := TSharedObject(GetGCHandleTarget(ValueBuf));
        // see TSharedObject.Free for details
        b := (so <> nil) and (so.RefCount = 1);
        so.Free;
        if b then
          DataBuf^ := nil;
      end;
    end;
    dtExtString, dtExtWideString, dtExtVarBytes:
      if not FStringHeap.Empty then begin
        DataBuf := PtrOffset(RecBuf, Field.DataOffset);
        ValueBuf := DataBuf^;
        if ValueBuf <> nil then begin
          if Marshal.ReadInt16(ValueBuf, - SizeOf(Word)) = RefNull then begin
            FStringHeap.DisposeBuf(ValueBuf);
            DataBuf^ := nil;
          end
          else
            FStringHeap.DisposeBuf(ValueBuf);
        end;
      end;
  end;
end;

procedure TData.CopyComplexFields(SourceRecBuf, DestRecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Field: TFieldDesc;
begin
  if HasComplexFields then
    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if Field.IsComplex and
        (not Field.IsBlob or WithBlob) and
        (not Field.HasParent) and
        (Field.FieldDescKind <> fdkCalculated)
      then
        CopyComplexField(SourceRecBuf, DestRecBuf, Field);
    end;
end;

procedure TData.CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc);
var
  DataType: Word;
  Len: Word;
  SrcPtr: PIntPtr;
  DestPtr: PIntPtr;
  ValueSize: Integer;
begin
  DataType := Field.DataType;

  case DataType of
    dtExtString, dtExtWideString, dtExtVarBytes: begin
      Len := Marshal.ReadUInt16(SourceRecBuf, Field.Offset);
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, Field.DataOffset);
      if SrcPtr <> nil then begin
        case DataType of
          dtExtString:
            ValueSize := Len + 1; // add zero terminator
          dtExtWideString:
            ValueSize := (Len  + 1) * SizeOf(WideChar); // add zero terminator
          else
            ValueSize := Len;
        end;
        DestPtr := FStringHeap.NewBuf(ValueSize);
        Move(SrcPtr^, DestPtr^, ValueSize);
        Marshal.WriteUInt16(DestRecBuf, Field.Offset, Len);
        Marshal.WriteIntPtr(DestRecBuf, Field.DataOffset, DestPtr);
      end
      else begin
        Marshal.WriteUInt16(DestRecBuf, Field.Offset, 0);
        Marshal.WriteIntPtr(DestRecBuf, Field.DataOffset, nil);
      end;
    end;
    dtVariant: begin
      SrcPtr := PtrOffset(SourceRecBuf, Field.DataOffset);
      DestPtr := PtrOffset(DestRecBuf, Field.DataOffset);
      TVariantObject(InternalGetObject(DestPtr)).Value := TVariantObject(InternalGetObject(SrcPtr)).Value;
    end;
  end;
end;

procedure TData.AddRef(RecBuf: IntPtr);
begin
end;

procedure TData.ReleaseRef(RecBuf: IntPtr; IsResync: boolean; WithBlob: boolean);
begin
end;

procedure TData.InitRecord(RecBuf: IntPtr);
var
  i: integer;
  Field: TFieldDesc;
begin
// Complex fields need create later
  if HasComplexFields then  // clear pointer to complex field
    FillChar(RecBuf, RecordSize, 0);

  for i := 0 to FFields.Count - 1 do begin
    Field := FFields[i];
    if not Field.HasParent and (Field.FieldDescKind <> fdkCalculated) then
      SetNull(Field, RecBuf, True);
  end;
end;

procedure TData.AppendBlankRecord;
var
  RecBuf: IntPtr;
begin
  AllocRecBuf(RecBuf);
  try
    InitRecord(RecBuf);
    AppendRecord(RecBuf);
  finally
    FreeRecBuf(RecBuf);
  end;
end;

procedure TData.EditRecord(RecBuf: IntPtr);
var
  TempBuf: IntPtr;
begin
  AllocRecBuf(TempBuf);
  try
    CheckFetched(RecBuf, nil);
    GetRecord(TempBuf);
    CreateComplexFields(TempBuf, False);  // Blobs use internal cache
    CopyComplexFields(RecBuf, TempBuf, False);
    PutRecord(TempBuf);
  finally
    FreeRecBuf(TempBuf);
  end;
end;

procedure TData.PostRecord(RecBuf: IntPtr);
var
  i: integer;
  TempBuf: IntPtr;
  Blob: TBlob;
  Field: TFieldDesc;
begin
  AllocRecBuf(TempBuf);
  FFilterUseRollBack := True;
  try
    GetRecord(TempBuf);

    UpdateRecord(RecBuf);

    if HasBlobFields then
      for i := 0 to FFields.Count - 1 do begin
        Field := FFields[i];
        if Field.IsBlob then begin
          Blob := TBlob(InternalGetObject(Field, RecBuf));
          if Blob <> nil then
            Blob.Commit;
        end;
      end;

    FreeComplexFields(TempBuf, False);
  finally
    FFilterUseRollBack := False;
    FreeRecBuf(TempBuf);
  end;
end;

procedure TData.CancelRecord(RecBuf: IntPtr);
var
  i: integer;
  Blob: TBlob;
  Field: TFieldDesc;
begin
  if HasBlobFields then
    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if Field.IsBlob then begin
        Blob := TBlob(InternalGetObject(Field, RecBuf));
        if Blob <> nil then
          Blob.Cancel;
      end;
    end;

  FreeComplexFields(RecBuf, False);
end;

{ Edit }

procedure TData.InternalAppend(RecBuf: IntPtr);
begin
  if Assigned(FOnAppend) then
    FOnAppend;
end;

procedure TData.InternalDelete;
begin
  if Assigned(FOnDelete) then
    FOnDelete;
end;

procedure TData.InternalUpdate(RecBuf: IntPtr);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate;
end;

procedure TData.ApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
begin
  if Assigned(FOnApplyRecord) then
    FOnApplyRecord(UpdateKind, Action, LastItem);
end;

procedure TData.DoAfterApplyUpdates;
begin
  if Assigned(FAfterApplyUpdates) then
    FAfterApplyUpdates;
end;

{ Navigation }

function TData.GetEOF: boolean;
begin
  Result := FEOF;
end;

function TData.GetBOF: boolean;
begin
  Result := FBOF;
end;

procedure TData.SetToBegin;
begin
  FBOF := True;
  FEOF := False;
end;

procedure TData.SetToEnd;
begin
  FEOF := True;
  FBOF := False;
end;

function TData.GetRecordCount: Integer;
begin
  Result := FRecordCount;
end;

function TData.GetRecordNo: Integer;
begin
  Result := -1;
end;

procedure TData.SetRecordNo(Value: Integer);
begin
end;

{ BookMarks }

procedure TData.GetBookmark(Bookmark: PRecBookmark);
begin
  Bookmark.Order := RecordNo;
end;

procedure TData.SetToBookmark(Bookmark: PRecBookmark);
begin
  if (IntPtr(Bookmark) <> nil) and (Bookmark.Order <> -1) then
    SetRecordNo(Bookmark.Order);
end;

function TData.BookmarkValid(Bookmark: PRecBookmark): boolean;
begin
  if IntPtr(Bookmark) <> nil then
    Result := Bookmark.Order <> -1
  else
    Result := False;
end;

function TData.CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
  Result := RetCodes[IntPtr(Bookmark1) = nil, IntPtr(Bookmark2) = nil];
  if Result = 2 then begin
    if Bookmark1.Order >= Bookmark2.Order then
      if Bookmark1.Order = Bookmark2.Order then
        Result := 0
      else
        Result := 1
    else
      Result := -1
  end;
end;

function TData.NeedGetRecordAfterGotoBookmark: boolean;
begin
  Result := False;
end;

{ CachedUpdates }

function TData.GetUpdateStatus: TItemStatus;
begin
  Result := isUnmodified;
end;

function TData.GetUpdateResult: TUpdateRecAction;
begin
  Result := urNone;
end;

function TData.HasUpdatedOrDeletedRecords: boolean;
begin
  Result := False;
end;

procedure TData.SetCacheRecBuf(NewBuf: IntPtr; OldBuf: IntPtr);
begin
end;

procedure TData.ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds);
begin
end;

procedure TData.CommitUpdates;
begin
end;

procedure TData.CancelUpdates;
begin
end;

procedure TData.RestoreUpdates;
begin
end;

procedure TData.RevertRecord;
begin
end;

procedure TData.InternalCacheChanged;
begin
  if Assigned(FOnCacheChanged) then
    FOnCacheChanged;
end;

procedure TData.InternalCacheApplied;
begin
  if Assigned(FOnCacheApplied) then
    FOnCacheApplied;
end;

procedure TData.InternalCacheCanceled;
begin
  if Assigned(FOnCacheCanceled) then
    FOnCacheCanceled;
end;

function TData.GetUpdatesPending: boolean;
begin
  Result := False;
end;

procedure TData.GetOldRecord(RecBuf: IntPtr);
begin
end;

function TData.InternalCompareFieldValue(ValuePtr: IntPtr; ValueLen: Word; ValueType: Word; DataBuf: IntPtr; DataLen: Word; FieldType: Word; HasParent: boolean; IsFixed: boolean; const Options: TCompareOptions): integer;

  // performance optimization
  function CompareStrFieldValues(DataBuf: IntPtr; NeedTrim: Boolean): integer;
  var
    St: AnsiString;
    PDate: IntPtr;
    ADouble: double;
    c: Currency;
    Blob: TBlob;
    l: integer;
    bcd: TBcd;
    ts: TSQLTimeStamp;
    fmt: string;
    str: string;
    BlobValue: IntPtr;
    Value: AnsiString;
  begin
    case FieldType of
      dtString: begin
        if not NeedTrim and
           not (coPartialKey in Options) and
           not (coPartialCompare in Options)
        then begin
          Result := InternalAnsiStrComp(ValuePtr, DataBuf, Options);
          Exit;
        end;

        if NeedTrim then
          St := StrTrimmed(DataBuf, DataLen)
        else
          St := Marshal.PtrToStringAnsi(DataBuf, DataLen);
      end;
      dtWideString:
        if NeedTrim then
          St := AnsiString(StrTrimmedW(DataBuf, DataLen))
        else
          St := AnsiString(Marshal.PtrToStringUni(DataBuf, DataLen));
      dtExtString: begin
        if not NeedTrim and
           not (coPartialKey in Options) and
           not (coPartialCompare in Options)
        then begin
          Result := InternalAnsiStrComp(ValuePtr, Marshal.ReadIntPtr(DataBuf), Options);
          Exit;
        end;

        if NeedTrim then
          St := StrTrimmed(Marshal.ReadIntPtr(DataBuf), DataLen)
        else
          St := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(DataBuf), DataLen);
      end;
      dtExtWideString:
        if NeedTrim then
          St := AnsiString(StrTrimmedW(Marshal.ReadIntPtr(DataBuf), DataLen))
        else
          St := AnsiString(Marshal.PtrToStringUni(Marshal.ReadIntPtr(DataBuf), DataLen));
      dtVariant:
        St := AnsiString(TVariantObject(InternalGetObject(DataBuf)).Value);
      dtInt8:
        St := AnsiString(IntToStr(ShortInt(Marshal.ReadByte(DataBuf))));
      dtUInt8:
        St := AnsiString(IntToStr(Marshal.ReadByte(DataBuf)));
      dtInt16:
        St := AnsiString(IntToStr(Marshal.ReadInt16(DataBuf)));
      dtUInt16:
        St := AnsiString(IntToStr(Word(Marshal.ReadInt16(DataBuf))));
      dtInt32:
        St := AnsiString(IntToStr(Marshal.ReadInt32(DataBuf)));
      dtUInt32:
        St := AnsiString(IntToStr(Cardinal(Marshal.ReadInt32(DataBuf))));
      dtInt64, dtUInt64:
        St := AnsiString(IntToStr(Marshal.ReadInt64(DataBuf)));
      dtFloat, dtCurrency:
        St := AnsiString(FloatToStr(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(DataBuf))));
      dtSingle:
        St := AnsiString(FloatToStr(CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(DataBuf))));
      dtDate, dtTime, dtDateTime: begin
        PDate := OrdinalToPtr(ADouble);
        try
          GetDateFromBuf(DataBuf, PDate, HasParent, dfDateTime);
        finally
          PtrToOrdinal(PDate, ADouble);
        end;
        case FieldType of
          dtDate:
            St := AnsiString(DateToStr(ADouble));
          dtTime:
            St := AnsiString(TimeToStr(ADouble));
          dtDateTime:
            St := AnsiString(DateTimeToStr(ADouble));
        end;
      end;
      dtSQLTimeStamp: begin
        ts := PSQLTimeStamp(DataBuf)^;
        fmt := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat + ' ' + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
        if ts.Fractions <> 0 then
          fmt := fmt + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator + 'zzz';
        DateTimeToString(str, fmt, ts, FormatSettings);
        St := AnsiString(str);
      end;
      dtBCD: begin
        c := Marshal.ReadInt64(DataBuf) / 10000;
        St := AnsiString(CurrToStr(c));
        Result := CompareStrValues(Marshal.PtrToStringAnsi(ValuePtr), St, Options + [coCaseInsensitive]);
        Exit;
      end;
      dtFmtBCD: begin
        bcd := PBcd(DataBuf)^;
        St := AnsiString(BcdToStr(bcd));
        Result := CompareStrValues(Marshal.PtrToStringAnsi(ValuePtr), St, Options + [coCaseInsensitive]);
        Exit;
      end;
      dtGuid: begin
        Result := CompareStrValues(Marshal.PtrToStringAnsi(ValuePtr), Marshal.PtrToStringAnsi(DataBuf), Options + [coCaseInsensitive]);
        Exit;
      end;
    else
      if IsBlobDataType(FieldType) then begin
        Blob := TBlob(InternalGetObject(DataBuf));
        l := GetBlobSize(Blob);
        BlobValue := Marshal.AllocHGlobal(l + 1);
        try
          if l > 0 then
            ReadBlob(Blob, 0, l, BlobValue);
          Marshal.WriteByte(BlobValue, l, 0);
          St := Marshal.PtrToStringAnsi(BlobValue, l)
        finally
          Marshal.FreeHGlobal(BlobValue);
        end;
      end
      else
        raise EConvertError.Create(SCannotConvertType);
    end;

    if FieldType in [dtString, dtWideString, dtExtString, dtExtWideString] then
      if NeedTrim then
        Value := StrTrimmed(ValuePtr)
      else
        Value := Marshal.PtrToStringAnsi(ValuePtr)
    else
      Value := Marshal.PtrToStringAnsi(ValuePtr);

    Result := CompareStrValues(Value, St, Options);
  end;

  function CompareWideStrFieldValues(DataBuf: IntPtr; NeedTrim: Boolean): integer;
  var
    WSt: WideString;
    PDate: IntPtr;
    ADouble: double;
    c: Currency;
    BlobValue: IntPtr;
    WValue: WideString;
    Blob: TBlob;
    l: integer;
    bcd: TBcd;
    ts: TSQLTimeStamp;
    str: string;
    fmt: string;
  begin
    case FieldType of
      dtString:
        if NeedTrim then
          WSt := WideString(StrTrimmed(DataBuf))
        else
          WSt := WideString(Marshal.PtrToStringAnsi(DataBuf));
      dtExtString:
        if NeedTrim then
          WSt := WideString(StrTrimmed(Marshal.ReadIntPtr(DataBuf)))
        else
          WSt := WideString(Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(DataBuf)));
      dtWideString:
        if NeedTrim then
          WSt := StrTrimmedW(DataBuf)
        else
          WSt := Marshal.PtrToStringUni(DataBuf);
      dtExtWideString:
        if NeedTrim then
          WSt := StrTrimmedW(Marshal.ReadIntPtr(DataBuf))
        else
          WSt := Marshal.PtrToStringUni(Marshal.ReadIntPtr(DataBuf));
      dtVariant:
        WSt := TVariantObject(InternalGetObject(DataBuf)).Value;
      dtInt8:
        WSt := WideString(IntToStr(ShortInt(Marshal.ReadByte(DataBuf))));
      dtUInt8:
        WSt := WideString(IntToStr(Marshal.ReadByte(DataBuf)));
      dtInt16:
        WSt := WideString(IntToStr(Marshal.ReadInt16(DataBuf)));
      dtUInt16:
        WSt := WideString(IntToStr(Word(Marshal.ReadInt16(DataBuf))));
      dtInt32:
        WSt := WideString(IntToStr(Marshal.ReadInt32(DataBuf)));
      dtUInt32:
        WSt := WideString(IntToStr(Cardinal(Marshal.ReadInt32(DataBuf))));
      dtInt64, dtUInt64:
        WSt := WideString(IntToStr(Marshal.ReadInt64(DataBuf)));
      dtFloat, dtCurrency:
        WSt := WideString(FloatToStr(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(DataBuf))));
      dtSingle:
        WSt := WideString(FloatToStr(CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(DataBuf))));
      dtDate, dtTime, dtDateTime: begin
        PDate := OrdinalToPtr(ADouble);
        try
          GetDateFromBuf(DataBuf, PDate, HasParent, dfDateTime);
        finally
          PtrToOrdinal(PDate, ADouble);
        end;
        case FieldType of
          dtDate:
            WSt := WideString(DateToStr(ADouble));
          dtTime:
            WSt := WideString(TimeToStr(ADouble));
          dtDateTime:
            WSt := WideString(DateTimeToStr(ADouble));
        end;
      end;
      dtSQLTimeStamp: begin
        ts := PSQLTimeStamp(DataBuf)^;
        fmt := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat + ' ' + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
        if ts.Fractions <> 0 then
          fmt := fmt + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator + 'zzz';
        DateTimeToString(str, fmt, ts, FormatSettings);
        WSt := WideString(str);
      end;
      dtBCD: begin
        c := Marshal.ReadInt64(DataBuf) / 10000;
        WSt := WideString(CurrToStr(c));
        Result := CompareWideStrValues(Marshal.PtrToStringUni(ValuePtr), WSt, Options + [coCaseInsensitive]);
        Exit;
      end;
      dtFmtBCD: begin
        bcd := PBcd(DataBuf)^;
        WSt := WideString(BcdToStr(bcd));
        Result := CompareWideStrValues(Marshal.PtrToStringUni(ValuePtr), WSt, Options + [coCaseInsensitive]);
        Exit;
      end;
    else
      if IsBlobDataType(FieldType) then begin
        Blob := TBlob(InternalGetObject(DataBuf));
        l := GetBlobSize(Blob, False, True);
        BlobValue := Marshal.AllocHGlobal(l + 2);
        try
          if l > 0 then
            ReadBlob(Blob, 0, 0, BlobValue, False, True);
          Marshal.WriteInt16(BlobValue, l, 0);
          WSt := Marshal.PtrToStringUni(BlobValue);
        finally
          Marshal.FreeHGlobal(BlobValue);
        end;
      end
      else
        raise EConvertError.Create(SCannotConvertType);
    end;

    if FieldType in [dtString, dtWideString, dtExtString, dtExtWideString] then
      if NeedTrim then
        WValue := StrTrimmedW(ValuePtr)
      else
        WValue := Marshal.PtrToStringUni(ValuePtr)
    else
      WValue := Marshal.PtrToStringUni(ValuePtr);

    Result := CompareWideStrValues(WValue, WSt, Options);
  end;

  function CompareVariantFieldValues(DataBuf: IntPtr): integer;
  var
    v1, v2: variant;
    v1VType, v2VType: TVarType;
    v1VArray, v2VArray: PVarArray;
    v1VArrayData, v2VArrayData: IntPtr;
  begin
    v1 := TVariantObject(InternalGetObject(DataBuf)).Value;
    v2 := TVariantObject(InternalGetObject(ValuePtr)).Value;
    v1VType := VarType(v1);
    v2VType := VarType(v2);

    if (v1VType = varArray + varByte) or (v2VType = varArray + varByte) then begin
      if (v1VType = varNull) and (v2VType = varNull) then
        Result := 0
      else
      if v1VType = varNull then // (v1VType = varNull) and (v2VType = varArray + varByte)
        Result := 1
      else
      if v2VType = varNull then // (v2VType = varNull) and (v1VType = varArray + varByte)
        Result := -1
      else // (v1VType <> varNull) and (v2VType <> varNull)
      if v1VType <> v2VType then begin
        if v1VType < v2VType then
          Result := 1
        else
          Result := -1;
      end
      else begin
        Assert(v1VType = varArray + varByte, 'Invalid v1.VType');
        Assert(v2VType = varArray + varByte, 'Invalid v2.VType');

        v1VArray := TVarData(v1).VArray;
        v2VArray := TVarData(v2).VArray;
        if (v1VArray = nil) and (v2VArray = nil) then
          Result := 0
        else
        if (v1VArray = nil) and (v2VArray = nil) then
          Result := 0
        else
        if v1VArray = nil then // (v1VArray = nil) and (v2VArray <> nil)
          Result := 1
        else
        if v2VArray = nil then // (v2VArray = nil) and (v1VArray <> nil)
          Result := -1
        else // (v1VArray <> nil) and (v2VArray <> nil)
        if v1VArray.Bounds[0].ElementCount < v2VArray.Bounds[0].ElementCount then
          Result := 1
        else
        if v1VArray.Bounds[0].ElementCount > v2VArray.Bounds[0].ElementCount then
          Result := - 1
        else begin
          v1VArrayData := v1VArray.Data;
          v2VArrayData := v2VArray.Data;
          if (v1VArrayData = nil) and (v2VArrayData = nil) then
            Result := 0
          else
          if (v1VArrayData = nil) and (v2VArrayData = nil) then
            Result := 0
          else
          if v1VArrayData = nil then // (v1VArrayData = nil) and (v2VArrayData <> nil)
            Result := 1
          else
          if v2VArrayData = nil then // (v2VArrayData = nil) and (v1VArrayData <> nil)
            Result := -1
          else // (v1VArrayData <> nil) and (v2VArrayData <> nil)
            Result := CompareBinValues(v1VArrayData, v1VArray.Bounds[0].ElementCount, v2VArrayData, v2VArray.Bounds[0].ElementCount, Options);
        end;
      end
    end
    else
      if (v1VType = v2VType) or
         ((((v1VType >= varSmallint) and (v1VType <= varCurrency)) or ((v1VType >= varDecimal) and (v1VType <= varInt64))) and
          (((v2VType >= varSmallint) and (v2VType <= varCurrency)) or ((v2VType >= varDecimal) and (v2VType <= varInt64)))) then begin // Equal VarType or Numbers
        if v1 < v2 then
          Result := 1
        else
        if v1 > v2 then
          Result := -1
        else
          Result := 0;
      end
      else
      if VarIsStr(v1) or VarIsStr(v2) then begin
        Result := CompareStrValues(AnsiString(v2), AnsiString(v1), Options)
      end
      else // VarType is different
      if v1VType < v2VType then
        Result := 1
      else
        Result := -1;
  end;

  function BcdFromPBytes(const AValue: PByteArray): TBcd;
  begin
    Result.Precision := AValue[0];
    Result.SignSpecialPlaces := AValue[1];
    Move(AValue[2], Result.Fraction, 32);
  end;

{$IFDEF VER10P}
  function BcdCompare(Bcd1, Bcd2: TBcd): Integer;
  const
    OneBcdBytes: array [0..33] of Byte = (2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
    MinusOneBcdBytes: array [0..33] of Byte = (2,128,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  var
    OneBcd, MinusOneBcd: TBcd;
  begin
    OneBcd := BcdFromPBytes(@OneBcdBytes);
    MinusOneBcd := BcdFromPBytes(@MinusOneBcdBytes);
    if (FMTBcd.BcdCompare(OneBcd, Bcd1) > 0) and (FMTBcd.BcdCompare(MinusOneBcd, Bcd1) < 0) then
      if IsBcdNegative(Bcd1) then begin
        BcdSubtract(Bcd1, OneBcd, Bcd1);
        BcdSubtract(Bcd2, OneBcd, Bcd2);
      end
      else begin
        BcdAdd(Bcd1, OneBcd, Bcd1);
        BcdAdd(Bcd2, OneBcd, Bcd2);
      end;

    Result := FMTBcd.BcdCompare(Bcd1, Bcd2);
  end;
{$ENDIF}

  function CompareCardinalValues(Value1: Cardinal; Value2: Cardinal): Integer;
  begin
    if Value1 = Value2 then
      Result := 0
    else
    if Value1 < Value2 then
      Result := -1
    else
      Result := 1;
  end;

  procedure UpdateFixedBinaryLength;
  var
    ValueEndPtr: PByte;
    DataEndPtr: PByte;
  begin
    if ValueLen > DataLen then begin
      ValueEndPtr := PtrOffset(ValuePtr, ValueLen - 1);
      DataEndPtr := PtrOffset(DataBuf, DataLen - 1);
      while ValueEndPtr^ = 0 do begin
        Dec(ValueEndPtr);
        if NativeUInt(ValueEndPtr) = NativeUInt(DataEndPtr) then
          Break;
      end;
      ValueLen := PtrSubstract(ValueEndPtr, ValuePtr) + 1;
    end
    else if ValueLen < DataLen then begin
      DataEndPtr := PtrOffset(DataBuf, DataLen - 1);
      ValueEndPtr := PtrOffset(ValuePtr, ValueLen - 1);
      while DataEndPtr^ = 0 do begin
        Dec(DataEndPtr);
        if NativeUInt(DataEndPtr) = NativeUInt(ValueEndPtr) then
          Break;
      end;
      DataLen := PtrSubstract(DataEndPtr, DataBuf) + 1;
    end;
  end;

var
  PDate: IntPtr;
  c, cValue: currency;
  ADouble, ADoubleValue: double;
  ASingle, ASingleValue: single;
  ACompareResult: TValueRelationship;
  Blob: TBlob;
  Len: integer;
  bcd, bcdValue: TBcd;
  TimeSt, TimeStValue: TSQLTimeStamp;
  TimeStOff, TimeStOffValue: TSQLTimeStampOffset;
begin
  Result := 0;
  case ValueType of
    dtString, dtGuid, dtExtString: begin
      if ValueType = dtExtString then
        ValuePtr := PIntPtr(ValuePtr)^;

      case FieldType of
        dtString:
          if not (coPartialKey in Options) and not (coPartialCompare in Options)
             and not (IsFixed and TrimFixedChar)
             and not (not IsFixed and TrimVarChar)
          then begin
            Result := InternalAnsiStrComp(ValuePtr, DataBuf, Options);
            Exit;
          end;
        dtExtString:
          if not (coPartialKey in Options) and not (coPartialCompare in Options)
             and not (IsFixed and TrimFixedChar)
             and not (not IsFixed and TrimVarChar)
          then begin
            DataBuf := PIntPtr(DataBuf)^;
            Result := InternalAnsiStrComp(ValuePtr, DataBuf, Options);
            Exit;
          end;
      end;

      Result := CompareStrFieldValues(DataBuf, (IsFixed and TrimFixedChar) or (not IsFixed and TrimVarChar));
    end;
    dtWideString, dtExtWideString: begin
      if ValueType = dtExtWideString then
        ValuePtr := PIntPtr(ValuePtr)^;
      Result := CompareWideStrFieldValues(DataBuf, (IsFixed and TrimFixedChar) or (not IsFixed and TrimVarChar));
    end;
    dtInt8:
      if ShortInt(Marshal.ReadByte(DataBuf)) < ShortInt(Marshal.ReadByte(ValuePtr)) then
        Result := 1
      else
      if ShortInt(Marshal.ReadByte(DataBuf)) > ShortInt(Marshal.ReadByte(ValuePtr)) then
        Result := -1
      else
        Result := 0;
    dtUInt8:
      if Marshal.ReadByte(DataBuf) < Marshal.ReadByte(ValuePtr) then
        Result := 1
      else
      if Marshal.ReadByte(DataBuf) > Marshal.ReadByte(ValuePtr) then
        Result := -1
      else
        Result := 0;
    dtInt16:
      if Marshal.ReadInt16(DataBuf) < Marshal.ReadInt16(ValuePtr) then
        Result := 1
      else
      if Marshal.ReadInt16(DataBuf) > Marshal.ReadInt16(ValuePtr) then
        Result := -1
      else
        Result := 0;
    dtUInt16:
      if Word(Marshal.ReadInt16(DataBuf)) < Word(Marshal.ReadInt16(ValuePtr)) then
        Result := 1
      else
      if Word(Marshal.ReadInt16(DataBuf)) > Word(Marshal.ReadInt16(ValuePtr)) then
        Result := -1
      else
        Result := 0;
    dtInt32:
      if Marshal.ReadInt32(DataBuf) < Marshal.ReadInt32(ValuePtr) then
        Result := 1
      else
      if Marshal.ReadInt32(DataBuf) > Marshal.ReadInt32(ValuePtr) then
        Result := -1
      else
        Result := 0;
    dtUInt32:
      if Cardinal(Marshal.ReadInt32(DataBuf)) < Cardinal(Marshal.ReadInt32(ValuePtr)) then
        Result := 1
      else
      if Cardinal(Marshal.ReadInt32(DataBuf)) > Cardinal(Marshal.ReadInt32(ValuePtr)) then
        Result := -1
      else
        Result := 0;
    dtInt64{$IFNDEF VER7P}, dtUInt64{$ENDIF}:
      if Marshal.ReadInt64(DataBuf) < Marshal.ReadInt64(ValuePtr) then
        Result := 1
      else
      if Marshal.ReadInt64(DataBuf) > Marshal.ReadInt64(ValuePtr) then
        Result := -1
      else
        Result := 0;
  {$IFDEF VER7P}
    dtUInt64:
      if UInt64(Marshal.ReadInt64(DataBuf)) < UInt64(Marshal.ReadInt64(ValuePtr)) then
        Result := 1
      else
      if UInt64(Marshal.ReadInt64(DataBuf)) > UInt64(Marshal.ReadInt64(ValuePtr)) then
        Result := -1
      else
        Result := 0;
  {$ENDIF}
    dtBoolean:
      if (Marshal.ReadByte(DataBuf) = 0) = (Marshal.ReadByte(ValuePtr) = 0) then // Cannot use 'boolean(DataBuf^) = boolean(ValuePtr^)' because 'True' may have any value without 0
        Result := 0
      else
      if (Marshal.ReadByte(DataBuf) = 0) then
        Result := 1
      else
        Result := -1;
    dtFloat, dtCurrency: begin
      if FieldType in [dtDateTime, dtDate, dtTime] then begin
        PDate := OrdinalToPtr(ADouble);
        try
          GetDateFromBuf(DataBuf, PDate, HasParent, dfDateTime);
        finally
          PtrToOrdinal(PDate, ADouble);
        end;

        ADoubleValue := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
        ACompareResult := CompareValue(ADouble, ADoubleValue, DateTimeValueDelta);

        if ACompareResult = LessThanValue then
          Result := 1
        else
        if ACompareResult = GreaterThanValue then
          Result := -1
        else
          Result := 0;
      end
      else begin
        ADouble := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(DataBuf));

        ADoubleValue := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
        if DoubleValueDelta > 0 then begin
          ACompareResult := CompareValue(ADouble, ADoubleValue, DoubleValueDelta);

          if ACompareResult = LessThanValue then
            Result := 1
          else
          if ACompareResult = GreaterThanValue then
            Result := -1
          else
            Result := 0;
        end
        else begin
          if ADouble < ADoubleValue then
            Result := 1
          else
          if ADouble > ADoubleValue then
            Result := -1
          else
            Result := 0;
        end;
      end;
    end;
    dtSingle: begin
      ASingle := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(DataBuf));
      ASingleValue := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(ValuePtr));

      if SingleValueDelta > 0 then begin
        ACompareResult := CompareValue(ASingle, ASingleValue);

        if ACompareResult = LessThanValue then
          Result := 1
        else
        if ACompareResult = GreaterThanValue then
          Result := -1
        else
          Result := 0;
      end
      else begin
        if ASingle < ASingleValue then
          Result := 1
        else
        if ASingle > ASingleValue then
          Result := -1
        else
          Result := 0;
      end;
    end;
    dtDateTime, dtDate, dtTime: begin
      PDate := OrdinalToPtr(ADouble);
      try
        GetDateFromBuf(DataBuf, PDate, HasParent, dfDateTime);
      finally
        PtrToOrdinal(PDate, ADouble);
      end;

      PDate := OrdinalToPtr(ADoubleValue);
      try
        GetDateFromBuf(ValuePtr, PDate, HasParent, dfDateTime);
      finally
        PtrToOrdinal(PDate, ADoubleValue);
      end;

      ACompareResult := CompareValue(ADouble, ADoubleValue, DateTimeValueDelta);

      if ACompareResult = LessThanValue then
        Result := 1
      else
      if ACompareResult = GreaterThanValue then
        Result := -1
      else
        Result := 0;
    end;
    dtBytes, dtVarBytes, dtExtVarBytes: begin
      if ValueType = dtExtVarBytes then
        ValuePtr := PIntPtr(ValuePtr)^;
      if FieldType = dtExtVarBytes then
        DataBuf := PIntPtr(DataBuf)^;
      if ValueType = dtBytes then
        UpdateFixedBinaryLength;
      Result := CompareBinValues(ValuePtr, ValueLen, DataBuf, DataLen, Options);
    end;
    dtBCD: begin
      c := Marshal.ReadInt64(DataBuf) / 10000;
      cValue := Marshal.ReadInt64(ValuePtr) / 10000;
      if c < cValue then
        Result := 1
      else
      if c > cValue then
        Result := -1
      else
        Result := 0;
    end;
    dtVariant:
      Result := CompareVariantFieldValues(DataBuf);
    dtFmtBCD: begin
      bcd := PBcd(DataBuf)^;
      bcdValue := PBcd(ValuePtr)^;
      Result := BcdCompare(bcdValue, bcd);
    end;
    dtSQLTimeStamp: begin
      TimeSt := PSQLTimeStamp(DataBuf)^;
      TimeStValue := PSQLTimeStamp(ValuePtr)^;
      Result := SQLTimeStampCompare(TimeStValue, TimeSt);
    end;
    dtSQLTimeStampOffset: begin
      TimeStOff := PSQLTimeStampOffset(DataBuf)^;
      TimeStOffValue := PSQLTimeStampOffset(ValuePtr)^;
      Result := SQLTimeStampOffsetCompare(TimeStOffValue, TimeStOff);
    end;
  else
    if IsBlobDataType(ValueType) then begin
      Blob := TBlob(InternalGetObject(ValuePtr));
      Len := GetBlobSize(Blob);
      ValuePtr := Marshal.AllocHGlobal(Len + 1);
      try
        if Len > 0 then
          ReadBlob(Blob, 0, Len, ValuePtr);
        Marshal.WriteByte(ValuePtr, Len, 0);
        Result := CompareStrFieldValues(DataBuf, False);
      finally
        Marshal.FreeHGlobal(ValuePtr);
      end;
    end
    else
      Assert(False{, 'Unknown ValueType = ' + IntToStr(ValueType)}); // performance optimization
  end;
end;

function TData.CompareFieldValue(ValuePtr: IntPtr; ValueLen: Word; ValueType: Word; FieldDesc: TFieldDesc;
  RecBuf: IntPtr; const Options: TCompareOptions; Mapped: boolean): integer;
var
  DataBuf: IntPtr;
  DataLen: Word;
  DataType: Word;
  HasParent: boolean;
  IsFixed: boolean;
  IsBlank: boolean;
  NativeBuffer: boolean;
begin
  if FieldDesc.ParentField = nil then begin
    if Mapped then
      DataBuf := GetMappedFieldBuf(RecBuf, FieldDesc, DataLen, DataType, HasParent, IsFixed, IsBlank, NativeBuffer)
    else begin
      DataBuf := GetFieldBuf(RecBuf, FieldDesc, DataLen, IsBlank, NativeBuffer);
      DataType := FieldDesc.DataType;
      HasParent := FieldDesc.HasParent;
      IsFixed := FieldDesc.Fixed
    end;

    if NativeBuffer then
      Result := InternalCompareFieldValue(ValuePtr, ValueLen, ValueType, DataBuf, DataLen, DataType, HasParent, IsFixed, Options) // performance optimization
    else
      try
        Result := InternalCompareFieldValue(ValuePtr, ValueLen, ValueType, DataBuf, DataLen, DataType, HasParent, IsFixed, Options); // performance optimization
      finally
        Marshal.FreeHGlobal(DataBuf);
      end;
  end
  else begin
    GetChildField(FieldDesc, RecBuf, DataBuf, DataLen, IsBlank, NativeBuffer);
    if NativeBuffer then
      Result := InternalCompareFieldValue(ValuePtr, ValueLen, ValueType, DataBuf, DataLen, FieldDesc.DataType, FieldDesc.HasParent, FieldDesc.Fixed, Options)
    else
      try
        Result := InternalCompareFieldValue(ValuePtr, ValueLen, ValueType, DataBuf, DataLen, FieldDesc.DataType, FieldDesc.HasParent, FieldDesc.Fixed, Options);
      finally
        Marshal.FreeHGlobal(DataBuf);
      end;
  end;
end;

function TData.GetSortOptions(SortColumn: TSortColumn): TCompareOptions;
begin
  case SortColumn.SortType of
    stCaseInsensitive:
      Result := [coCaseInsensitive];
    stBinary:
      Result := [coOrdinalCompare];
  else
    Result := [];
  end;
end;

function TData.GetFilterExpression: TExpressionNode;
begin
  if FFilterCondition <> nil then
    Result := FFilterCondition.FExpression
  else
    Result := nil;
end;

function TData.InternalCompareFields(DataBuf: IntPtr; DataLen: Word; IsBlank: boolean; DataType: integer;
  RecBuf: IntPtr; FieldDesc: TFieldDesc; const Options: TCompareOptions; Mapped: boolean): integer;
var
  IsBlank2: boolean;
begin
  IsBlank2 := GetNull(FieldDesc, RecBuf);
  if IsBlank and not IsBlank2 then
    if coInvertNullOrder in Options then
      Result := 1
    else
      Result := -1
  else
  if not IsBlank and IsBlank2 then
    if coInvertNullOrder in Options then
      Result := -1
    else
      Result := 1
  else
  if IsBlank and IsBlank2 then
    Result := 0
  else
    Result := CompareFieldValue(DataBuf, DataLen, DataType, FieldDesc, RecBuf, Options, Mapped);
end;

function TData.CompareFields(RecBuf1: IntPtr; RecBuf2: IntPtr; FieldDesc: TFieldDesc; const Options: TCompareOptions; Mapped: boolean): integer;
var
  DataBuf: IntPtr;
  DataLen: Word;
  IsBlank: boolean;
  DataType: Word;
  HasParent: boolean;
  IsFixed: boolean;
  NativeBuffer: boolean;
begin
  if Mapped then
    DataBuf := GetMappedFieldBuf(RecBuf1, FieldDesc, DataLen, DataType, HasParent, IsFixed, IsBlank, NativeBuffer)
  else begin
    DataBuf := GetFieldBuf(RecBuf1, FieldDesc, DataLen, IsBlank, NativeBuffer);
    DataType := FieldDesc.DataType;
  end;

  if NativeBuffer then
    Result := InternalCompareFields(DataBuf, DataLen, IsBlank, DataType, RecBuf2, FieldDesc, Options, Mapped) // performance optimization
  else
    try
      Result := InternalCompareFields(DataBuf, DataLen, IsBlank, DataType, RecBuf2, FieldDesc, Options, Mapped);
    finally
      Marshal.FreeHGlobal(DataBuf);
    end;
end;

procedure TData.StartSearch;
begin
  FRecordSearch := True;
end;

procedure TData.EndSearch;
begin
  FRecordSearch := False;
end;

function TData.Eval(Node: TExpressionNode): boolean;
begin
  Result := Eval(Node, nil, nil, 0);
end;

function TData.Eval(Node: TExpressionNode; ConstraintField: TFieldDesc; ValuePtr: IntPtr; ValueLen: Word): boolean;
var
  V1, V2, V2i: variant;
  DateField1, DateField2, IsFloatField, IsBooleanField: boolean;
  FieldDesc: TFieldDesc;
  i, k: integer;

  function MatchesMask(St: string; Mask: string): boolean;
  const
    WildcardAst = '*';
    WildcardPct = '%';
    WildcardOne = '_';
  type
    TMatchesResult = (mrFalse, mrTrue, mrEnd);

    function SubMatchesMask(StIndex, MaskIndex: integer): TMatchesResult;
    begin
      while (MaskIndex <= Length(Mask)) and
        ((StIndex <= Length(St)) or
        ((Mask[MaskIndex] = WildcardAst) or (Mask[MaskIndex] = WildcardPct))) do begin
//http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/DB_TDataSet_FilterOptions.html
        if ((Mask[MaskIndex] = WildcardAst) and not (FilterNoPartialCompare and (MaskIndex = Length(Mask))))
          or (Mask[MaskIndex] = WildcardPct) then begin
          if MaskIndex = Length(Mask) then begin  //-
            Result := mrTrue;                     // Speed up
            Exit;                                 // with mask '*'
          end                                     //-
          else
            case SubMatchesMask(StIndex, MaskIndex + 1) of
              mrTrue: begin
                Result := mrTrue;
                Exit;
              end;
              mrFalse:
                if StIndex > Length(St) then begin
                  Result := mrEnd;
                  Exit;
                end
                else
                  Inc(StIndex);
              mrEnd: begin
                Result := mrEnd;
                Exit;
              end;
            end;
        end
        else begin
          if (Mask[MaskIndex] = '\') and (MaskIndex < Length(Mask)) and (Mask[MaskIndex + 1] = WildcardOne) then
            Inc(MaskIndex);
          if (St[StIndex] = Mask[MaskIndex]) or ((Mask[MaskIndex] = WildcardOne) and not ((MaskIndex > 1) and (Mask[MaskIndex - 1] = '\')))
          then begin
            Inc(StIndex);
            Inc(MaskIndex);
          end
          else begin
            Result := mrFalse;
            Exit;
          end;
        end;
      end;

      if StIndex > Length(St) then
        if MaskIndex > Length(Mask) then
          Result := mrTrue
        else
          Result := mrEnd
      else
        Result := mrFalse;
    end;
  begin
    Result := SubMatchesMask(1, 1) = mrTrue;
  end;

  procedure NormalizeDateField(var V: Variant);
  var
    d: TDateTime;
  begin
    if VarIsStr(V) then
      if TryStrToDateTime(VarToStr(V), d) then
        V := d
      else
        V := VarToDateTime(V);
  end;

  function GetNodeValue(ConstraintField: TFieldDesc; Field: TFieldDesc; ValuePtr: IntPtr; ValueLen: Word): Variant;
  begin
    if (ConstraintField = Field) and (ValuePtr <> nil) then
      GetMappedDataAsVariant(Field, ValuePtr, ValueLen, Result, FFilterUseRollBack, True)
    else
      GetMappedFieldAsVariant(Field, FilterRecBuf, Result, FFilterUseRollBack, False)
  end;

begin
  Assert(Node <> nil);

  Result := False;
  IsFloatField := False;
  IsBooleanField := False;

  if Node.NodeType in [ntEqual, ntMore, ntLess, ntMoreEqual, ntLessEqual,
    ntNoEqual, ntLike, ntNotLike, ntIn, ntNotIn]
  then begin
    Assert(Node.LeftOperand <> nil);
    Assert(Node.RightOperand <> nil);

    DateField1 := False;
    DateField2 := False;

    case Node.LeftOperand.NodeType of
      ntField: begin
        FieldDesc := Node.LeftOperand.FieldDesc;
        V1 := GetNodeValue(ConstraintField, FieldDesc, ValuePtr, ValueLen);
        DateField1 := FieldDesc.DataType in [dtDateTime, dtDate, dtTime];
        IsFloatField := FieldDesc.DataType in [dtFloat, dtCurrency, dtSingle];
        IsBooleanField := FieldDesc.DataType = dtBoolean;
      end;
      ntValue:
        V1 := Node.LeftOperand.Value;
    end;

    case Node.RightOperand.NodeType of
      ntField: begin
        FieldDesc := Node.RightOperand.FieldDesc;
        V2 := GetNodeValue(ConstraintField, FieldDesc, ValuePtr, ValueLen);
        DateField2 := FieldDesc.DataType in [dtDateTime, dtDate, dtTime];
        IsFloatField := IsFloatField or (FieldDesc.DataType in [dtFloat, dtCurrency, dtSingle]);
        IsBooleanField := IsBooleanField or (FieldDesc.DataType = dtBoolean);
      end;
      ntValue:
        V2 := Node.RightOperand.Value;
    end;

    if DateField1 then
      NormalizeDateField(V2); /// CR-D12823
    if DateField2 then
      NormalizeDateField(V1);

    if FilterCaseInsensitive then begin
      if VarIsStr(V1) then
        V1 := AnsiUpperCase(VarToStr(V1));
      if VarIsStr(V2) then
        V2 := AnsiUpperCase(VarToStr(V2));
      if VarIsArray(V2) then
        for i := 0 to VarArrayHighBound(V2, 1) do
          if VarIsStr(V2[i]) then
            V2[i] := AnsiUpperCase(VarToStr(V2[i]));
    end;

    if (Node.LeftOperand.NodeType in [ntField, ntValue]) and
       Assigned(Node.LeftOperand.LeftOperand) and
       not VarIsNull(V1)
    then
      case Node.LeftOperand.LeftOperand.NodeType of
        ntLower:
          V1 := AnsiLowerCase(V1);
        ntUpper:
          V1 := AnsiUpperCase(V1);
      end;

    if (Node.RightOperand.NodeType in [ntField, ntValue]) and
       Assigned(Node.RightOperand.LeftOperand) and
       not VarIsNull(V2)
    then
      case Node.RightOperand.LeftOperand.NodeType of
        ntLower:
          V2 := AnsiLowerCase(V2);
        ntUpper:
          V2 := AnsiUpperCase(V2);
      end;
  end;

  if (VarIsNull(V1) or VarIsNull(V2)) and (Node.NodeType in [ntMore, ntLess, ntMoreEqual, ntLessEqual]) then begin
    // To prevent exception on compare value with Null
    Result := False;
    Exit;
  end;

  case Node.NodeType of
    ntEqual, ntLike, ntIn, ntNoEqual, ntNotLike, ntNotIn: begin
      if Node.NodeType in [ntIn, ntNotIn] then
        k := VarArrayHighBound(V2, 1)
      else
        k := 0;
      for i := 0 to k do begin
        if VarIsArray(V2) then
          V2i := V2[i]
        else
          V2i := V2;
        if (FilterNoPartialCompare and not (Node.NodeType in [ntLike, ntNotLike])) or (not VarIsStr(V1) and (not VarIsStr(V2i) or (Node.NodeType = ntEqual))) then begin
          if IsFloatField and (DoubleValueDelta > 0) and
             VarIsNumeric(V1) and VarIsNumeric(V2i) and
             SameValue(V1, V2i, DoubleValueDelta)
          then
            Result := True
          else
          if IsBooleanField then begin
            if VarIsNull(V1) or VarIsNull(V2i) then
              Result := V1 = V2i
            else
              Result := Boolean(V1) = Boolean(V2i);
          end
          else
            Result := V1 = V2i
        end
        else
          Result := MatchesMask(VarToStr(V1), VarToStr(V2i));
        if Result then
          Break;
      end;
      if (Node.NodeType in [ntNoEqual, ntNotLike, ntNotIn]) then
        Result := not Result;
    end;
    ntMore:
      Result := V1 > V2;
    ntLess:
      Result := V1 < V2;
    ntMoreEqual:
      Result := V1 >= V2;
    ntLessEqual:
      Result := V1 <= V2;
    ntAnd:
      Result := Eval(Node.LeftOperand, ConstraintField, ValuePtr, ValueLen) and
                Eval(Node.RightOperand, ConstraintField, ValuePtr, ValueLen);
    ntOr:
      Result := Eval(Node.LeftOperand, ConstraintField, ValuePtr, ValueLen) or
                Eval(Node.RightOperand, ConstraintField, ValuePtr, ValueLen);
    ntNot:
      Result := not Eval(Node.LeftOperand, ConstraintField, ValuePtr, ValueLen);
    ntTrue:
      Result := True;
    ntFalse:
      Result := False;
  else
    Assert(False);
  end;
end;

procedure TData.CheckConstraint(Field: TFieldDesc; RecBuf: IntPtr; ValuePtr: IntPtr; ValueLen: Word; FieldConstraint: TFieldConstraint);
begin
  FilterRecBuf := RecBuf;
  if FieldConstraint <> nil then
    if FieldConstraint.Expression <> nil then
      if not Eval(FieldConstraint.Expression, Field, ValuePtr, ValueLen) then
        FieldConstraint.ConstraintError;
end;

procedure TData.CheckConstraints(RecBuf: IntPtr; Constraints: TCRObjectList);
var
  i: integer;
  Constraint: TConstraint;
begin
  if (Constraints = nil) or (Constraints.Count = 0) then
    Exit;

  for i := 0 to Constraints.Count - 1 do begin
    Constraint := TConstraint(Constraints.Items[i]);
    FilterRecBuf := RecBuf;
    if (Constraint <> nil) and (Constraint.Expression <> nil) then
      if not Eval(Constraint.Expression) then
        Constraint.ConstraintError;
  end;
end;

procedure TData.FilterUpdated;
begin
end;

function TData.Filtered: boolean;
begin
  Result := CachedUpdates or Assigned(FFilterFunc) or
    (Assigned(FFilterCondition) and Assigned(FFilterCondition.FExpression)) or
    Assigned(FFilterMDFunc);
end;

{ Blobs }

function TData.InternalGetObject(Field: TFieldDesc; RecBuf: IntPtr): TSharedObject;
var
  DataPtr: IntPtr;
  DataLen: Word;
  IsBlank: boolean;
begin
  GetField(Field, RecBuf, @DataPtr, DataLen, False, IsBlank);
  Result := InternalGetObject(@DataPtr);
end;

function TData.InternalGetObject(DataBuf: IntPtr): TSharedObject;
begin
  Result := TSharedObject(GetGCHandleTarget(PIntPtr(DataBuf)^));
end;

procedure TData.InternalSetObject(DataBuf: IntPtr; Obj: TSharedObject);
begin
  PIntPtr(DataBuf)^ := Obj.GCHandle;
end;

function TData.GetBlob(Field: TFieldDesc; RecBuf: IntPtr): TBlob;
begin
  if not Field.IsBlob then
    raise Exception.Create(SNeedBlobType);

  Result := InternalGetObject(Field, RecBuf) as TBlob;

  Assert(Result <> nil, 'Object for field ' + Field.Name + ' (' + IntToStr(Field.FieldNo - 1) + ') = nil');
end;

procedure TData.SetBlob(Field: TFieldDesc; RecBuf: IntPtr; Blob: TBlob);
begin
  if not Field.IsBlob then
    raise Exception.Create(SNeedBlobType);

  InternalSetObject(PtrOffset(RecBuf, Field.DataOffset), Blob);
end;

{$IFDEF VER6}
var
  DefaultUserCodePage: Integer;

type
  PStrRec = ^StrRec;
  StrRec = packed record
    refCnt: Integer;
    length: Integer;
  end;

function CharFromWCharD7(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer): Integer;
begin
  Result := WideCharToMultiByte(DefaultUserCodePage, 0, WCharSource, SrcChars,
      CharDest, DestBytes, nil, nil);
end;

procedure _LStrClr(var S);
var
  P: PStrRec;
begin
  if Pointer(S) <> nil then
  begin
    P := PtrOffset(Pointer(S), - SizeOf(StrRec));
    Pointer(S) := nil;
    if P.refCnt > 0 then
      if InterlockedDecrement(P.refCnt) = 0 then
        FreeMem(P);
  end;
end;

function _NewAnsiString(length: Integer): Pointer;
var
  P: PStrRec;
begin
  Result := nil;
  if length <= 0 then Exit;
  // Alloc an extra null for strings with even length.  This has no actual cost
  // since the allocator will round up the request to an even size anyway.
  // All widestring allocations have even length, and need a double null terminator.
  GetMem(P, length + sizeof(StrRec) + 1 + ((length + 1) and 1));
  Result := PtrOffset(P, sizeof(StrRec));
  P.length := length;
  P.refcnt := 1;
  PWideChar(Result)[length shr 1] := #0;  // length guaranteed >= 2
end;

procedure _LStrFromPCharLen(var Dest: AnsiString; Source: PAnsiChar; Length: Integer);
asm
  { ->    EAX     pointer to dest }
  {       EDX source              }
  {       ECX length              }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        { allocate new string }

        MOV     EAX,EDI

        CALL    _NewAnsiString
        MOV     ECX,EDI
        MOV     EDI,EAX

        TEST    ESI,ESI
        JE      @@noMove

        MOV     EDX,EAX
        MOV     EAX,ESI
        CALL    Move

        { assign the result to dest }

@@noMove:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure _LStrFromPWCharLenD7(var Dest: AnsiString; Source: PWideChar; Length: Integer);
var
  DestLen: Integer;
  Buffer: array[0..4095] of AnsiChar;
begin
  if Length <= 0 then
  begin
    _LStrClr(Dest);
    Exit;
  end;
  if Length+1 < (High(Buffer) div sizeof(WideChar)) then
  begin
    DestLen := CharFromWCharD7(Buffer, High(Buffer), Source, Length);
    if DestLen >= 0 then
    begin
      _LStrFromPCharLen(Dest, Buffer, DestLen);
      Exit;
    end;
  end;

  DestLen := (Length + 1) * sizeof(WideChar);
  SetLength(Dest, DestLen);  // overallocate, trim later
  DestLen := CharFromWCharD7(Pointer(Dest), DestLen, Source, Length);
  if DestLen < 0 then DestLen := 0;
  SetLength(Dest, DestLen);
end;

procedure _LStrFromWStrD7(var Dest: AnsiString; const Source: WideString);
asm
        { ->    EAX pointer to dest              }
        {       EDX pointer to WideString data   }

        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOV     ECX,[EDX-4]
        SHR     ECX,1
@@1:    JMP     _LStrFromPWCharLenD7
end;
{$ENDIF}

function TData.ReadBlob(Blob: TBlob; Position: Cardinal;
  Count: Cardinal; Dest: IntPtr; FromRollback: boolean = False;
  TrueUnicode: boolean = False): Cardinal;
var
  LenBytes, BlobPos: Cardinal;
  sBuf, Buf: IntPtr;
  ws: WideString;
  sa: AnsiString;
begin
  Blob.UseRollback := FromRollback;
  try
    if Blob.FIsUnicode = TrueUnicode then
      Result := Blob.Read(Position, Count, Dest)
    else
    begin
      if TrueUnicode then begin
        BlobPos := Blob.TranslatePositionToUni(Position);
        if Count = 0 then
          LenBytes := Blob.Size - BlobPos
        else
          LenBytes := Blob.TranslatePositionToUni(Count);
        sBuf := Marshal.AllocHGlobal(LenBytes);

        Buf := nil;
        try
          Result := Blob.Read(BlobPos, LenBytes, sBuf);
          ws := WideString(Marshal.PtrToStringAnsi(sBuf, Result));
          Result := Length(ws) * 2;
          Buf := Marshal.StringToHGlobalUni(ws);
          CopyBuffer(Buf, Dest, Result);
        finally
          Marshal.FreeHGlobal(sBuf);
          if Buf <> nil then
            Marshal.FreeCoTaskMem(Buf);
        end;
      end
      else begin
        BlobPos := Blob.TranslatePositionToAnsi(Position);
        if Count = 0 then
          LenBytes := Blob.Size - BlobPos
        else
          LenBytes := Blob.TranslatePositionToAnsi(Count);
        sBuf := Marshal.AllocHGlobal(LenBytes);

        Buf := nil;
        try
          Result := Blob.Read(BlobPos, LenBytes, sBuf);
        {$IFNDEF VER6}
          sa := AnsiString(Marshal.PtrToStringUni(sBuf, Result shr 1));
        {$ELSE}
          _LStrFromWStrD7(sa, Marshal.PtrToStringUni(sBuf, Result shr 1));
        {$ENDIF}
          Result := LengthA(sa);
          Buf := Marshal.StringToHGlobalAnsi(sa);
          CopyBuffer(Buf, Dest, Result);
        finally
          Marshal.FreeHGlobal(sBuf);
          if Buf <> nil then
            Marshal.FreeCoTaskMem(Buf);
        end;
      end;
    end;
  finally
    Blob.UseRollback := False;
  end;
end;

function TData.ReadBlob(Field: TFieldDesc; RecBuf: IntPtr; Position: Cardinal;
  Count: Cardinal; Dest: IntPtr; FromRollback: boolean = False;
  TrueUnicode: boolean = False): Cardinal;
var
  Blob: TBlob;
begin
  Blob := GetBlob(Field, RecBuf);
  Result := ReadBlob(Blob, Position, Count, Dest, FromRollback, TrueUnicode);
end;

procedure TData.WriteBlob(Blob: TBlob; Position: Cardinal;
  Count: Cardinal; Source: IntPtr; TrueUnicode: boolean = False);
var
  Buf: IntPtr;
  sa: AnsiString;
  ws: WideString;
begin
  Blob.EnableRollback;
  if Blob.FIsUnicode = TrueUnicode then
    Blob.Write(Position, Count, Source)
  else begin
    sa := Marshal.PtrToStringAnsi(Source, Count);
    ws := WideString(sa);
    Count := Length(ws) * 2; // for MBCS this differ from Count * 2
    if TrueUnicode then
      Position := Blob.TranslatePositionToUni(Position)
    else
      Position := Blob.TranslatePositionToAnsi(Position);
    Buf := Marshal.StringToHGlobalUni(ws);
    try
      Blob.Write(Position, Count, Buf); //Count length in bytes
    finally
      Marshal.FreeCoTaskMem(Buf);
    end;
  end;
end;

procedure TData.WriteBlob(Field: TFieldDesc; RecBuf: IntPtr; Position: Cardinal;
  Count: Cardinal; Source: IntPtr; TrueUnicode: boolean = False);
var
  Blob: TBlob;
begin
  Blob := GetBlob(Field, RecBuf);
  WriteBlob(Blob, Position, Count, Source, TrueUnicode);
  SetNull(Field, RecBuf, False);
end;

function TData.TruncateBlob(Blob: TBlob; Size: Cardinal;
  TrueUnicode: boolean = False): Integer;
begin
  Blob.EnableRollback;
  if Blob.FIsUnicode <> TrueUnicode then begin
    if TrueUnicode then
      Size := Blob.TranslatePositionToUni(Size)
    else
      Size := Blob.TranslatePositionToAnsi(Size);
  end;
  Blob.Truncate(Size);
  Result := Size;
end;

function TData.TruncateBlob(Field: TFieldDesc; RecBuf: IntPtr; Size: Cardinal;
  TrueUnicode: boolean = False): Integer;
var
  Blob: TBlob;
begin
  Blob := GetBlob(Field, RecBuf);
  Result := TruncateBlob(Blob, Size, TrueUnicode);

  if Result = 0 then
    SetNull(Field, RecBuf, True);
end;

function TData.GetBlobSize(Blob: TBlob; FromRollback: boolean = False;
  TrueUnicode: boolean = False): Cardinal;
begin
  Blob.UseRollback := FromRollback;
  try
    if Blob.FIsUnicode = TrueUnicode then
      Result := Blob.Size
    else if TrueUnicode then
      Result := Blob.GetSizeUni
    else
      Result := Blob.GetSizeAnsi;
  finally
    Blob.UseRollback := False;
  end;
end;

function TData.GetBlobSize(Field: TFieldDesc; RecBuf: IntPtr; FromRollback: boolean = False;
  TrueUnicode: boolean = False): Cardinal;
var
  Blob: TBlob;
begin
  if GetNull(Field, RecBuf) then begin
    Result := 0;
    Exit;
  end;

  Blob := GetBlob(Field, RecBuf);
  Result := GetBlobSize(Blob, FromRollback, TrueUnicode);
end;

procedure TData.SetBlobSize(Blob: TBlob; NewSize: Cardinal; FromRollback: boolean = False;
  TrueUnicode: boolean = False);
var
  OldSize: Cardinal;
begin
  Blob.UseRollback := FromRollback;
  try
    if Blob.FIsUnicode = TrueUnicode then
      Blob.Size := NewSize
    else if TrueUnicode then begin
      // Blob.Size is char count / 2
      OldSize := Blob.GetSizeUni;
      if NewSize > OldSize then
        Blob.Size := Blob.Size + (NewSize - OldSize) shr 1
      else
        Blob.Size := Blob.TranslatePositionToUni(NewSize);
    end
    else begin
      // Blob.Size is char count * 2
      OldSize := Blob.GetSizeAnsi;
      if NewSize > OldSize then
        Blob.Size := Blob.Size + (NewSize - OldSize) * 2
      else
        Blob.Size := Blob.TranslatePositionToAnsi(NewSize);
    end;
  finally
    Blob.UseRollback := False;
  end;
end;

procedure TData.SetBlobSize(Field: TFieldDesc; RecBuf: IntPtr; NewSize: Cardinal; FromRollback: boolean = False;
  TrueUnicode: boolean = False);
var
  Blob: TBlob;
begin
  SetNull(Field, RecBuf, False);

  Blob := GetBlob(Field, RecBuf);
  SetBlobSize(Blob, NewSize, FromRollback, TrueUnicode);
end;

procedure TData.SetCachedUpdates(Value: boolean);
begin
  if Value <> FCachedUpdates then begin
    if FCachedUpdates then
      CancelUpdates;

    FCachedUpdates := Value;
    FilterItemTypes := [isUnmodified, isUpdated, isAppended];
  end;
end;

function TData.GetFilterText: string;
begin
  if FFilterCondition <> nil then
    Result := FFilterCondition.FText
  else
    Result := '';
end;

procedure TData.SetFilterText(const Value: string);
begin
  if FFilterCondition = nil then
    FFilterCondition := TFilter.Create(Self, Value);

  FFilterCondition.Text := Value;
  if Active then
    FFilterCondition.CreateExpression;
end;

procedure TData.SetFilterItemTypes(const Value: TItemTypes);
begin
  FFilterItemTypes := Value;
end;

{ Filter }

function TData.InternalAnsiStrComp(const Value1, Value2: IntPtr;
  const Options: TCompareOptions): integer;
begin
  if coOrdinalCompare in Options then
    Result := {$IFDEF VER18P}{$IFNDEF NEXTGEN}AnsiStrings.{$ENDIF}{$ENDIF}
      StrComp(PAnsiChar(Value1), PAnsiChar(Value2))
  else begin
    if coCaseInsensitive in Options then
      Result := AnsiStrICompS(Value1, Value2)
    else
      Result := AnsiStrCompS(Value1, Value2);
  end;
end;

function TData.InternalAnsiCompareText(const Value1, Value2: AnsiString;
  const Options: TCompareOptions): integer;
begin
  if coOrdinalCompare in Options then
    Result := CompareStr(Value1, Value2)
  else begin
    if coCaseInsensitive in Options then
      Result := AnsiCompareTextS(Value1, Value2)
    else
      Result := AnsiCompareStrS(Value1, Value2);
  end;
end;

function TData.InternalWStrLComp(const Value1, Value2: WideString;
  const Options: TCompareOptions): integer;
begin
  if coOrdinalCompare in Options then
    Result := InternalWStrComp(Value1, Copy(Value2, 1, Length(Value1)), Options)
  else begin
    if coCaseInsensitive in Options then
      Result := AnsiStrLICompWS(Value1, Value2, Length(Value1))
    else
      Result := AnsiStrLCompWS(Value1, Value2, Length(Value1));
  end;
end;

function TData.InternalWStrComp(const Value1, Value2: WideString;
  const Options: TCompareOptions): integer;

  function CompareStrW(const S1, S2: WideString): integer;
  begin
    if S1 < S2 then
      Result := -1
    else
      if S1 > S2 then
        Result := 1
      else
        Result := 0;
  end;

begin
  if coOrdinalCompare in Options then
    Result := CompareStrW(Value1, Value2)
  else begin
    if coCaseInsensitive in Options then
      Result := AnsiStrICompWS(Value1, Value2)
    else
      Result := AnsiStrCompWS(Value1, Value2);
  end;
end;

// Used to compare field value and string KeyValue with matching options
function TData.CompareStrValues(const Value: AnsiString; const FieldValue: AnsiString;
  const Options: TCompareOptions): integer;
var
  Res: integer;
  ValueLen: integer;
begin
  if coPartialCompare in Options then begin
    if coCaseInsensitive in Options then
      Res := Pos(AnsiUpperCase(Value), AnsiUpperCase(FieldValue))
    else
      Res := Pos(Value, FieldValue);
    if Res = 0 then
      Res := 1
    else
      Res := 0;
  end
  else
  if coPartialKey in Options then begin
    ValueLen := LengthA(Value);
    if ValueLen = 0 then
      ValueLen := LengthA(FieldValue);
    if LengthA(FieldValue) >= ValueLen then
      Result := 0
    else
      Result := 1;
    if Result <> 0 then
      Exit // To avoid AV in case Len(Value) > Len(St)
    else
      Res := InternalAnsiCompareText(Value, Copy(FieldValue, 1, ValueLen), Options);
  end
  else
    Res := InternalAnsiCompareText(Value, FieldValue, Options);
  Result := Res;
end;

function TData.CompareWideStrValues(const Value: WideString; const FieldValue: WideString;
  const Options: TCompareOptions): integer;
var
  Res: integer;
  ValueLen: integer;

{$IFDEF MSWINDOWS}
  ValueS, FieldValueS: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if IsWin9x then begin
    ValueS := AnsiString(Value);
    FieldValueS := AnsiString(FieldValue);
    Result := CompareStrValues(ValueS, FieldValueS, Options);
    Exit;
  end;
{$ENDIF}

  if coPartialCompare in Options then begin
    if coCaseInsensitive in Options then
      Res := Pos(WideUpperCase(Value), WideUpperCase(FieldValue))
    else
      Res := Pos(Value, FieldValue);
    if Res = 0 then
      Res := 1
    else
      Res := 0;
  end
  else
  if coPartialKey in Options then begin
    ValueLen := Length(Value);
    if Length(FieldValue) >= ValueLen then
      Result := 0
    else
      Result := 1;
    if Result <> 0 then
      Exit // To avoid AV in case Len(Value) > Len(St)
    else
      Res := InternalWStrLComp(Value, FieldValue, Options);
  end
  else
    Res := InternalWStrComp(Value, FieldValue, Options);
  Result := Res;
end;

// Used to compare binary field value and binary KeyValue with matching options
function TData.CompareBinValues(const Value: IntPtr; const ValueLen: Integer;
  const FieldValue: IntPtr; const FieldValueLen: Integer;
  const Options: TCompareOptions): integer;

  function CompareMem(FieldValue, Value: IntPtr; FieldValueLen: integer): integer;
  var
    i: integer;
  begin
    for i := 0 to FieldValueLen - 1 do begin
      if Marshal.ReadByte(Value, i) > Marshal.ReadByte(FieldValue, i) then begin
        Result := 1;
        Exit;
      end
      else
      if Marshal.ReadByte(Value, i) < Marshal.ReadByte(FieldValue, i) then begin
        Result := -1;
        Exit;
      end
    end;
    Result := 0;
  end;

var
  i: Integer;

begin
  if coPartialCompare in Options then begin
    if FieldValueLen >= ValueLen then begin
      for i := 0 to FieldValueLen - ValueLen - 1 do begin
        Result := CompareMem(PtrOffset(FieldValue, i), Value, ValueLen);
        if Result = 0 then
          exit;
      end;
      Result := 1;
    end
    else
      Result := 1; // Field value is shorter when Value
  end
  else
  if coPartialKey in Options then begin
    if FieldValueLen >= ValueLen then
      Result := CompareMem(FieldValue, Value, ValueLen)
    else
      Result := 1; // Field value is shorter when Value
  end
  else
  begin
    Result := CompareMem(FieldValue, Value, Min(ValueLen, FieldValueLen));
    if Result = 0 then
      if ValueLen > FieldValueLen then
        Result := 1
      else
      if ValueLen < FieldValueLen then
        Result := -1;
  end;
end;

{ TMemData }

constructor TMemData.Create;
begin
  inherited;

  FIndexFields := TCRObjectList.Create;
  BlockMan := TBlockManager.Create;
  InitData;
end;

destructor TMemData.Destroy;
begin
  inherited;

  FIndexFields.Free;
  BlockMan.Free;
  SetLength(FRecordNoCache, 0);
end;

function TMemData.GetIndexField(Index: Integer): TSortColumn;
begin
  Result := TSortColumn(FIndexFields[Index]);
end;

function TMemData.GetIndexFieldCount: Integer;
begin
  Result := FIndexFields.Count;
end;

{ Items / Data }

procedure TMemData.ClearIndexFields;
begin
  if FCalcRecBuf <> nil then begin
    Marshal.FreeHGlobal(FCalcRecBuf);
    FCalcRecBuf := nil;
  end;
  if FCalcRecBuf2 <> nil then begin
    Marshal.FreeHGlobal(FCalcRecBuf2);
    FCalcRecBuf2 := nil;
  end;

  FIndexFields.Clear;
end;

procedure TMemData.UpdateIndexFields;
var
  S, S1: string;
  SLen: Integer;
  FldName: string;
  FieldDesc: TFieldDesc;
  SortColumn: TSortColumn;
  ProcessedCS, ProcessedDESC: boolean;

  procedure RaiseError;
  begin
    raise Exception.Create('Invalid IndexFieldNames format!');
  end;

begin
  ClearIndexFields;
  S := FIndexFieldNames;
  if Trim(S) <> '' then begin
    CheckIndexFields;

    FParser := TBoolParser.Create(S);
    FParser.QuotedString := True;

    try
      FParser.ToBegin();
      FCode := FParser.GetNext(S1);
      while FCode <> lcEnd do begin
        case FCode of
          lcIdent, lcNumber, lcString, lxLeftSqBracket: begin
            if FCode = lxLeftSqBracket then begin
              FParser.OmitBlank := False;
              FCode := FParser.GetNext(S1);
              FldName := '';
              while (FCode <> lxRightSqBracket) and (FCode <> lcEnd) do begin
                FldName := FldName + S1;
                FCode := FParser.GetNext(S1);
              end;
              FParser.OmitBlank := True;
              S1 := FldName;
            end;

            FieldDesc := FFields.FindField(S1);
            // try to unquote field name
            if FieldDesc = nil then begin
              SLen := Length(S1);
              if (SLen > 2) and
                 ((S1[1] = '''') and (S1[SLen] = '''')) or
                 ((S1[1] = '"') and (S1[SLen] = '"'))
               then begin
                 S1 := copy(S1, 2, SLen - 2);
                 FieldDesc := FFields.FindField(S1);
               end;
            end;
            if FieldDesc = nil then
              raise Exception.Create(Format(SFieldNotFound, [S1]));
            SortColumn := TSortColumn.Create;
            SortColumn.FieldDesc := FieldDesc;
            SetSortDefaults(SortColumn);
            FIndexFields.Add(SortColumn);

            FCode := FParser.GetNext(S1);
            ProcessedCS := False;
            ProcessedDESC := False;
            while not (((FCode = lcSymbol) and ((S1 = ';') or (S1 = ','))) or (FCode = lcEnd)) do begin
              if FCode = lcIdent then begin
                if not ProcessedDESC and ('DESC' = UpperCase(S1)) then begin
                  SortColumn.DescendingOrder := True;
                  ProcessedDESC := True;
                end
                else
                if not ProcessedDESC and ('ASC' = UpperCase(S1)) then begin
                  SortColumn.DescendingOrder := False;
                  ProcessedDESC := True;
                end
                else
                if not ProcessedCS and ('CIS' = UpperCase(S1)) then begin
                  SortColumn.SortType := stCaseInsensitive;
                  ProcessedCS := True;
                end
                else
                if not ProcessedCS and ('CS' = UpperCase(S1)) then begin
                  SortColumn.SortType := stCaseSensitive;
                  ProcessedCS := True;
                end
                else
                if not ProcessedCS and ('BIN' = UpperCase(S1)) then begin
                  SortColumn.SortType := stBinary;
                  ProcessedCS := True;
                end
                else
                  RaiseError;
                FCode := FParser.GetNext(S1);
              end
              else
                RaiseError;
            end;
          end;
          lcSymbol: begin
            if (S1 <> ';') and (S1 <> ',') then
              RaiseError;
            FCode := FParser.GetNext(S1);
          end
        else
          RaiseError;
        end;
      end;
    finally
      FParser.Free;
    end;
  end;
end;

procedure TMemData.CheckIndexFields;
begin
  // Dummy
end;

procedure TMemData.SetSortDefaults(SortColumn: TSortColumn);
begin
  SortColumn.DescendingOrder := False;
  SortColumn.SortType := stCaseSensitive;
end;

procedure TMemData.SetIndexFieldNames(const Value: string);
begin
  if FIndexFieldNames <> Value then begin
    FIndexFieldNames := Value;

    if Active then begin
      UpdateIndexFields;
      SortItems;
    end
    else
      if Prepared then
        UpdateIndexFields;
  end;
end;

function TMemData.CompareRecords(RecBuf1, RecBuf2: IntPtr): integer;
var
  SortColumn: TSortColumn;
  WasCalculated: boolean;
  i: integer;
  Dir: integer;
begin
  Result := 0;
  WasCalculated := False;
  for i := 0 to FIndexFields.Count - 1 do begin
    SortColumn := TSortColumn(FIndexFields[i]);
    if SortColumn.DescendingOrder then
      Dir := -1
    else
      Dir := 1;

    if SortColumn.FieldDesc.FieldDescKind = fdkCalculated then begin
      if FCalcRecBuf = nil then
        FCalcRecBuf := Marshal.AllocHGlobal(FRecordSize + FCalcRecordSize);
      if FCalcRecBuf2 = nil then
        FCalcRecBuf2 := Marshal.AllocHGlobal(FRecordSize + FCalcRecordSize);

      if not WasCalculated then begin
        if Assigned(FOnGetCachedBuffer) then
          FOnGetCachedBuffer(FCalcRecBuf, RecBuf1);
        if Assigned(FOnGetCachedBuffer) then
          FOnGetCachedBuffer(FCalcRecBuf2, RecBuf2);
        WasCalculated := True;
      end;
      Result := CompareFields(FCalcRecBuf, FCalcRecBuf2, SortColumn.FieldDesc, GetSortOptions(SortColumn), False) * Dir;
    end
    else
      Result := CompareFields(RecBuf1, RecBuf2, SortColumn.FieldDesc, GetSortOptions(SortColumn), False) * Dir;

    if Result <> 0 then
      break;
  end;
end;

function TMemData.CompareByRecBuf(Item1, Item2: IntPtr): integer;
begin
  Result := CompareRecords(PtrOffset(Item1, sizeof(TItemHeader)), PtrOffset(Item2, sizeof(TItemHeader)));
end;

function TMemData.CompareBySavedOrder(Item1, Item2: IntPtr): integer;
var
  Order1, Order2: integer;
begin
  Order1 := PItemHeader(Item1)^.SavedOrder;
  Order2 := PItemHeader(Item2)^.SavedOrder;

  if Order1 < Order2 then begin
    if Order1 <= 0 then
      Result := 1
    else
      Result := -1;
  end
  else if Order1 > Order2 then begin
    if Order2 <= 0 then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
end;

procedure TMemData.Exchange(I, J: PItemHeader);
var
  NextToI, PrevToJ: PItemHeader;
begin
  NextToI := I.Next;
  PrevToJ := J.Prev;
  if IntPtr(I.Prev) <> nil then
    I.Prev.Next := J;
  if IntPtr(J.Next) <> nil then
    J.Next.Prev := I;
  J.Prev := I.Prev;
  I.Next := J.Next;
  if NextToI = J then begin
    I.Prev := J;
    J.Next := I;
  end
  else begin
    I.Prev := PrevToJ;
    if IntPtr(PrevToJ) <> nil then
      PrevToJ.Next := I;
    J.Next := NextToI;
    if IntPtr(NextToI) <> nil then
      NextToI.Prev := J;
  end;

  if I = FirstItem then FirstItem := J;
  if J = LastItem then LastItem := I;
end;

procedure TMemData.MoveSortedRecord(Dir: integer);
begin
  if Dir = 0 then
    Exit;
  while True do begin
    if Dir > 0 then begin
      if (IntPtr(CurrentItem.Next) <> nil) and
        (CompareRecords(PtrOffset(CurrentItem, sizeof(TItemHeader)), PtrOffset(CurrentItem.Next, sizeof(TItemHeader))) > 0)
      then
        Exchange(CurrentItem, CurrentItem.Next)
      else
        break;
    end
    else begin
      if (IntPtr(CurrentItem.Prev) <> nil) and
        (CompareRecords(PtrOffset(CurrentItem, sizeof(TItemHeader)), PtrOffset(CurrentItem.Prev, sizeof(TItemHeader))) < 0)
      then
        Exchange(CurrentItem.Prev, CurrentItem)
      else
        break;
    end;
  end;
end;

procedure TMemData.QuickSort(L, R, P: PItemHeader; CompareFunction: TItemsCompareFunction);
var
  I, J, IP, JP, I1: PItemHeader;
  changeIP, changeJP: boolean;
begin
  repeat
    I := L;
    J := R;
    IP := I;
    JP := J;
    changeIP := False;
    changeJP := False;
    while True do begin
      while (IntPtr(I) <> IntPtr(P)) and (CompareFunction(I, P) < 0) do begin
        I := I.Next;
        if changeIP then
          IP := IP.Next;
        changeIP := not changeIP;
      end;
      while (IntPtr(J) <> IntPtr(P)) and (CompareFunction(J, P) > 0) do begin
        J := J.Prev;
        if changeJP then
          JP := JP.Prev;
        changeJP := not changeJP;
      end;
      if (IntPtr(J.Next) = IntPtr(I)) or
        (IntPtr(I) = IntPtr(J))
      then
        break;

      if CompareFunction(I, J) <> 0 then begin
        Exchange(I, J);
        I1 := I;
        I := J;
        J := I1;
        if L = I then
          L := J
        else
        if L = J then
          L := I;

        if JP = I then
          JP := J
        else
        if JP = J then
          JP := I;

        if IP = I then
          IP := J
        else
        if IP = J then
          IP := I;

        if R = I then
          R := J
        else
        if R = J then
          R := I;
      end;

      if IntPtr(I) <> IntPtr(P) then begin
        I := I.Next;
        if changeIP then
          IP := IP.Next;
        changeIP := not changeIP;
      end;
      if IntPtr(J) <> IntPtr(P) then begin
        J := J.Prev;
        if changeJP then
          JP := JP.Prev;
        changeJP := not changeJP;
      end;
    end;
    if IntPtr(L) <> IntPtr(J) then QuickSort(L, J, IP, CompareFunction);
    if (IntPtr(I) = IntPtr(J)) and (IntPtr(I) <> IntPtr(R)) then
      I := I.Next;
    L := I;
    P := JP;
  until I = R;
end;

procedure TMemData.SortItems;
var
  IndexFieldsCount: integer;
begin
  IndexFieldsCount := FIndexFields.Count;
  if (IndexFieldsCount = 0) and not FOrderSaved then
    Exit;

  if (IntPtr(FirstItem) <> nil) and (IntPtr(LastItem) <> nil) then begin
    if IndexFieldsCount > 0 then begin
      QuickSort(FirstItem, LastItem, FirstItem, CompareByRecBuf);
      ReorderItems(nil, roFull);
    end
    else begin
      QuickSort(FirstItem, LastItem, FirstItem, CompareBySavedOrder);
      FOrderSaved := True;
      ReorderItems(nil, roFull);
      FOrderSaved := False;
    end;
  end;
end;

procedure TMemData.InitItem(Item: PItemHeader);
begin
  Item.Rollback := nil;
  Item.Status := isUnmodified;
  Item.UpdateResult := urNone;
  Item.FilterResult := fsNotChecked;
  Item.Order := 0;
  Item.SavedOrder := 0;
end;

function TMemData.InsertItem: PItemHeader;
var
  Item: PItemHeader;
begin
  if EOF then begin
    Result := AppendItem;
    Exit;
  end;

  if BOF then
    CurrentItem := FirstItem;

  BlockMan.AllocItem(Item);
  InitItem(Item);

  Item.Next := CurrentItem;

  if IntPtr(CurrentItem) <> nil then begin
    Item.Prev := CurrentItem.Prev;
    if IntPtr(CurrentItem.Prev) <> nil then
      CurrentItem.Prev.Next := Item;
    CurrentItem.Prev := Item
  end
  else begin
    Item.Prev := nil;
  end;

  if FirstItem = CurrentItem then
    FirstItem := Item;

  if IntPtr(LastItem) = nil then
    LastItem := Item;

  Result := Item;
end;

function TMemData.AppendItem: PItemHeader;
var
  Item: PItemHeader;
begin
  BlockMan.AllocItem(Item);
  InitItem(Item);

  if IntPtr(FirstItem) = nil then begin
    FirstItem := Item;
    Item.Order := 1;
  end
  else begin
    LastItem.Next := Item;
    Item.Order := LastItem.Order + 1;
  end;

  Item.Prev := LastItem;
  Item.Next := nil;
  LastItem := Item;

  Result := Item;
end;

procedure TMemData.DeleteItem(Item: PItemHeader);
begin
  if IntPtr(Item) <> nil then begin
    if Item = FirstItem then
      if Item = LastItem then begin
        CurrentItem := nil;
        FirstItem := nil;
        LastItem := nil;
        FBOF := True;
        FEOF := True;
      end
      else begin
        FirstItem := Item.Next;
        FirstItem.Prev := nil;
        if Item = CurrentItem then
          CurrentItem := FirstItem;
      end
    else
      if Item = LastItem then begin
        LastItem := Item.Prev;
        LastItem.Next := nil;
        if Item = CurrentItem then
          CurrentItem := LastItem;
      end
      else begin
        if Item = CurrentItem then
          CurrentItem := Item.Next;

        if IntPtr(Item.Prev) <> nil then
          Item.Prev.Next := Item.Next;
        if IntPtr(Item.Next) <> nil then
          Item.Next.Prev := Item.Prev;
      end;

    FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True);
    BlockMan.FreeItem(Item);
  end;
end;

procedure TMemData.InitData;
begin
  FirstItem := nil;
  LastItem := nil;
  CurrentItem := nil;
  Cache := nil;
  LastCacheItem := nil;

  FBOF := True;
  FEOF := True;
  FRowsFetched := 0;
  FRecordCount := 0;
  FRecordNoOffset := 0;
  FOrderSaved := False;

  BlockMan.FirstFree := nil;
  Inc(RefreshIteration);
  FRefreshIteration := RefreshIteration;
  SetLength(FRecordNoCache, 0);
end;

procedure TMemData.FreeData;
var
  CacheItem: TCacheItem;
  Item: PItemHeader;
  NeedFreeComplex: boolean;

  function HasComplexFields(IncludeStrings: boolean): boolean;
  var
    i: integer;
    Field: TFieldDesc;
  begin
    Result := False;
    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if Field.IsComplex then
        case Field.DataType of
          dtExtString, dtExtWideString, dtExtVarBytes:
            Result := IncludeStrings;
          else begin
            Result := True;
            Exit;
          end;
        end;
    end;
  end;

begin
  if not FStringHeap.SysGetMem then begin
    NeedFreeComplex := HasComplexFields(False);
    FStringHeap.Clear;
  end
  else
    NeedFreeComplex := HasComplexFields(True);

  if NeedFreeComplex then begin
  // Free complex fields
    Item := FirstItem;
    while IntPtr(Item) <> nil do begin
      FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True);
      Item := Item.Next;
    end;
    CacheItem := Cache;
    while CacheItem <> nil do begin
      Item := CacheItem.Item.Rollback;
      if IntPtr(Item) <> nil then
        FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True);
      CacheItem := CacheItem.Next;
    end;
  end;

// Free cache
  while Cache <> nil do begin
    CacheItem := Cache;
    Cache := Cache.Next;
    FreeCachedItem(CacheItem);
  end;

  FStringHeap.Clear;
  BlockMan.FreeAllBlocks;

  InitData;
end;

procedure TMemData.ReorderItems(Item: PItemHeader; ReorderOption: TReorderOption);
var
  No: Integer;
  Item1: PItemHeader;
  NeedSaveOrder: boolean;
begin
  if Length(FRecordNoCache) > 0 then
    SetLength(FRecordNoCache, 0);

  if (IntPtr(Item) <> nil) or (ReorderOption = roFull) and (IntPtr(FirstItem) <> nil) then begin
    if ReorderOption = roFull then begin
      Item := FirstItem;
      No := 1;
    end
    else
      if IntPtr(Item.Next) <> nil then
        No := Item.Next.Order
      else
        if IntPtr(Item.Prev) <> nil then
          No := Item.Prev.Order
        else begin
          if not OmitRecord(Item) then
            Item.Order := 1
          else
            Item.Order := 0;
          FRecordNoOffset := 0;
          Exit;
        end;

    NeedSaveOrder := (ReorderOption = roFull) and not FOrderSaved;

    if (ReorderOption = roFull) or (No > (FRecordCount + FRecordNoOffset) shr 1) then begin
      Item1 := Item.Prev;
      while (IntPtr(Item1) <> nil) and OmitRecord(Item1) do
        Item1 := Item1.Prev;
      if IntPtr(Item1) <> nil then
        No := Item1.Order + 1
      else begin
        No := 1;
        FRecordNoOffset := 0;
      end;

      while IntPtr(Item) <> nil do begin
        if NeedSaveOrder then begin
          Item.SavedOrder := Item.Order;
          FOrderSaved := True;
        end;
        if not OmitRecord(Item) then begin
          Item.Order := No;
          Inc(No);
        end
        else
          Item.Order := 0;
        Item := Item.Next;
      end;
    end
    else begin
      Item1 := Item.Next;
      while (IntPtr(Item1) <> nil) and OmitRecord(Item1) do
        Item1 := Item1.Next;
      if IntPtr(Item1) <> nil then begin
        No := Item1.Order - 1;
        if ReorderOption = roInsert then begin
          if not OmitRecord(Item) then
            Inc(FRecordNoOffset);
        end
        else
          Dec(FRecordNoOffset);
      end
      else begin
        No := FRecordCount;
        FRecordNoOffset := 0;
      end;

      while IntPtr(Item) <> nil do begin
        if NeedSaveOrder then begin
          Item.SavedOrder := Item.Order;
          FOrderSaved := True;
        end;
        if not OmitRecord(Item) then begin
          Item.Order := No;
          Dec(No);
        end
        else
          Item.Order := 0;
        Item := Item.Prev;
      end;
    end;

    if ReorderOption = roFull then
      FRecordCount := No - 1;
  end;
end;

{ Fields }

procedure TMemData.Reopen;
begin
  inherited;

  if Length(FRecordNoCache) > 0 then
    SetLength(FRecordNoCache, 0);

  // M11255
  if FilterText <> '' then
    FilterUpdated;
  // M11254
  if FIndexFields.Count > 0 then
    SortItems;
end;

procedure TMemData.InitFields;
begin
  inherited;

  BlockMan.RecordSize := RecordSize;
  UpdateIndexFields;
end;

procedure TMemData.ClearFields;
begin
  ClearIndexFields;

  inherited;
end;


{ Records }

function TMemData.OmitRecord(Item: PItemHeader): boolean;
begin
  if IntPtr(Item) = nil then begin
    Result := True;
    Exit;
  end;

  if FRecordSearch then
    Item.FilterResult := fsNotChecked;

  if Item.FilterResult = fsNotChecked then begin
    Result := FCachedUpdates and not (Item.Status in FFilterItemTypes);

    if not Result then begin
      FilterRecBuf := BlockMan.GetRecordPtr(Item);
      if (FCalcRecordSize > 0) and
        (Assigned(FFilterMDFunc) or Assigned(FFilterFunc) or
        (Assigned(FFilterCondition) and Assigned(FFilterCondition.FExpression) and FFilterCondition.FExpression.UseCalculatedFields))
      then begin
        if FCalcRecBuf = nil then
          FCalcRecBuf := Marshal.AllocHGlobal(FRecordSize + FCalcRecordSize);

        if Assigned(FOnGetCachedBuffer) then
          FOnGetCachedBuffer(FCalcRecBuf, FilterRecBuf)
        else
          CopyBuffer(FilterRecBuf, FCalcRecBuf, FRecordSize);

        FilterRecBuf := FCalcRecBuf;
      end;

      Result := Assigned(FFilterFunc) and not FFilterFunc(FilterRecBuf) or
        Assigned(FFilterMDFunc) and not FFilterMDFunc(FilterRecBuf) or
        (Assigned(FFilterCondition) and Assigned(FFilterCondition.FExpression) and not Eval(FFilterCondition.FExpression)) or
        Assigned(FFilterRangeFunc) and not FFilterRangeFunc(FilterRecBuf);
    end;

    if not FRecordSearch then
      if Result then
        Item.FilterResult := fsOmitted
      else
        Item.FilterResult := fsNotOmitted;
  end
  else
    Result := Item.FilterResult = fsOmitted;
end;

procedure TMemData.GetRecord(RecBuf: IntPtr);
begin
  if not (EOF or BOF) then
    if IntPtr(CurrentItem) = nil then
      GetNextRecord(RecBuf)
    else
    if OmitRecord(CurrentItem) then
      GetNextRecord(RecBuf)
    else
      BlockMan.GetRecord(CurrentItem, RecBuf);
end;

procedure TMemData.GetNextRecord(RecBuf: IntPtr);

  procedure OmitRecords;
  begin
    while (IntPtr(CurrentItem) <> nil) and OmitRecord(CurrentItem) do
      CurrentItem := CurrentItem.Next;
  end;

begin
  if not EOF then begin
    if BOF then begin
      FBOF := False;
      CurrentItem := FirstItem;
    end
    else
      if IntPtr(CurrentItem) <> nil then
        CurrentItem := CurrentItem.Next
      else
        CurrentItem := FirstItem;

    OmitRecords;
    if IntPtr(CurrentItem) = nil then
      FEOF := True
    else
      if RecBuf <> nil then
        GetRecord(RecBuf);
  end;
end;

procedure TMemData.GetPriorRecord(RecBuf: IntPtr);

  procedure OmitRecords;
  begin
    while (IntPtr(CurrentItem) <> nil) and OmitRecord(CurrentItem) do
      CurrentItem := CurrentItem.Prev;
  end;

begin
  if not BOF then begin
    if EOF then begin
      FEOF := False;
      CurrentItem := LastItem;
    end
    else
      if IntPtr(CurrentItem) <> nil then
        CurrentItem := CurrentItem.Prev
      else
        CurrentItem := LastItem;

    OmitRecords;
    if IntPtr(CurrentItem) = nil then
      FBOF := True
    else
      if RecBuf <> nil then
        GetRecord(RecBuf);
  end;
end;

procedure TMemData.UpdateCachedBuffer(FItem, LItem: PItemHeader);
var
  Item: PItemHeader;
begin
  if not Assigned(FOnGetCachedBuffer) or (FCalcRecordSize > 0) then
    Exit;

  if IntPtr(FItem) = nil then
    FItem := FirstItem;
  if IntPtr(LItem) = nil then
    LItem := LastItem;

  Item := FItem;

  while IntPtr(Item) <> nil do begin
    FOnGetCachedBuffer(PtrOffset(Item, SizeOf(TItemHeader)));
    if Item = LItem then
      Break;
    Item := Item.Next;
  end;
end;

procedure TMemData.PutRecord(RecBuf: IntPtr);
begin
  Assert(IntPtr(CurrentItem) <> nil);
  if Length(FRecordNoCache) > 0 then
    SetLength(FRecordNoCache, 0);
  CurrentItem.FilterResult := fsNotChecked;
  BlockMan.PutRecord(CurrentItem, RecBuf);
end;

procedure TMemData.RefreshRecord(RecBuf: IntPtr);
begin
  Assert(IntPtr(CurrentItem) <> nil);
  RemoveItemFromCache(CurrentItem);
  CurrentItem.Status := isUnmodified;
  CurrentItem.UpdateResult := urNone;
  PutRecord(RecBuf);
end;

procedure TMemData.PostRecord(RecBuf: IntPtr);
var
  TempFilterFunc: TFilterFunc;
begin
  TempFilterFunc := FilterFunc;
  FilterFunc := nil;
  try
    inherited PostRecord(RecBuf);

    if Assigned(TempFilterFunc) then begin
      FilterFunc := TempFilterFunc;
      if CurrentItem <> nil then begin
        CurrentItem.FilterResult := fsNotChecked;
        if OmitRecord(CurrentItem) then
          Dec(FRecordCount);
      end;
    end;
  except
    FilterFunc := TempFilterFunc;
    raise;
  end;
end;

procedure TMemData.AddRecord(RecBuf: IntPtr);
var
  OldCurrentItem: PItemHeader;
  MoveDir, i: integer;
  Blob: TBlob;
  Field: TFieldDesc;
begin
  OldCurrentItem := CurrentItem;
  CurrentItem := InsertItem;

  PutRecord(RecBuf);

  if HasBlobFields then
    for i := 0 to FFields.Count - 1 do begin
      Field := FFields[i];
      if Field.IsBlob then begin
        Blob := TBlob(InternalGetObject(Field, RecBuf));
        if Blob <> nil then
          Blob.Commit;
      end;
    end;

  if FIndexFields.Count > 0 then begin
    if IntPtr(OldCurrentItem) = nil then
      MoveDir := -1
    else
      MoveDir := CompareRecords(RecBuf, PtrOffset(OldCurrentItem, sizeof(TItemHeader)));
    MoveSortedRecord(MoveDir);
  end;

  if FCachedUpdates and not LocalUpdate then begin
    CurrentItem.Status := isAppended;
    CurrentItem.UpdateResult := urNone;
  end;

  if not OmitRecord(CurrentItem) then
    Inc(FRecordCount);
  ReorderItems(CurrentItem, roInsert);
end;

procedure TMemData.InsertRecord(RecBuf: IntPtr);
var
  CacheItem: TCacheItem;
begin
  if not FCachedUpdates then
    InternalAppend(RecBuf);

  AddRecord(RecBuf);

  if FCachedUpdates and not LocalUpdate then begin
    CacheItem := TCacheItem.Create;
    CacheItem.Item := CurrentItem;
    AddCacheItem(CacheItem);
  end;
end;

procedure TMemData.AppendRecord(RecBuf: IntPtr);
begin
  SetToEnd;
  InsertRecord(RecBuf);
end;

procedure TMemData.UpdateRecord(RecBuf: IntPtr);
var
  i: integer;
  CacheItem: TCacheItem;
  Rollback: PItemHeader;
  Blob, RollbackBlob: TBlob;
  RollbackRecBuf: IntPtr;
  ItemRecBuf: IntPtr;
  MoveDir: integer;
  Field: TFieldDesc;
begin
  Assert(IntPtr(CurrentItem) <> nil);

  if not FCachedUpdates then
    InternalUpdate(RecBuf)
  else
  if not LocalUpdate then begin
    if CurrentItem.Status = isUnmodified then begin
    // add to cache
      CacheItem := TCacheItem.Create;
      CacheItem.Item := CurrentItem;
      AddCacheItem(CacheItem);
    end;

    if (CurrentItem.Status <> isAppended) or (CurrentItem.UpdateResult = urApplied)
    then begin
      CurrentItem.Status := isUpdated;

      if IntPtr(CurrentItem.Rollback) = nil then begin
      // create rollback record
        BlockMan.AllocItem(Rollback);
        InitItem(Rollback);
        CurrentItem.Rollback := Rollback;
        BlockMan.CopyRecord(CurrentItem, Rollback);
        AddRefComplexFields(PtrOffset(Rollback, sizeof(TItemHeader)));
      end;
      if HasBlobFields then begin
        RollbackRecBuf := PtrOffset(CurrentItem.Rollback, sizeof(TItemHeader));
        ItemRecBuf := PtrOffset(CurrentItem, sizeof(TItemHeader));
        for i := 0 to FFields.Count - 1 do begin
          Field := FFields[i];
          if Field.IsBlob then begin
            Blob := TBlob(InternalGetObject(PtrOffset(RollbackRecBuf, Field.DataOffset)));
            if Blob.CanRollback
              and (Blob = TBlob(InternalGetObject(PtrOffset(ItemRecBuf, Field.DataOffset))))
            then begin
              RollbackBlob := TBlob.Create(Blob.IsUnicode);
              RollbackBlob.SetData(Blob.FRollback);
              Marshal.WriteIntPtr(RollbackRecBuf, Field.DataOffset, RollbackBlob.GCHandle);
              Blob.Commit;
              Blob.Release;
            end;
          end;
        end;
      end;
    end;
    CurrentItem.UpdateResult := urNone;
  end;

  FFilterUseRollBack := False;

  if FIndexFields.Count > 0 then
    MoveDir := CompareRecords(RecBuf, PtrOffset(CurrentItem, sizeof(TItemHeader)))
  else
    MoveDir := 0;

  PutRecord(RecBuf);

  if MoveDir <> 0 then begin
    MoveSortedRecord(MoveDir);
    ReorderItems(nil, roFull);
  end
  else
    if OmitRecord(CurrentItem) then
      Dec(FRecordCount);
end;

procedure TMemData.RemoveRecord;
var
  PermitDelete: boolean;
begin
  if FCachedUpdates then begin
    PermitDelete := CurrentItem.Status <> isAppended;
    RevertRecord;
  end
  else
    PermitDelete := True;

  if PermitDelete then begin
    DeleteItem(CurrentItem);
    Dec(FRecordCount); // if PermitDelete = False RecordCount is decreased on RevertRecord
  end;

  ReorderItems(CurrentItem, roDelete);
end;

procedure TMemData.DeleteRecord;
var
  CacheItem: TCacheItem;
  NextToCurrentItem, PrevToCurrentItem: PItemHeader;
begin
  if IntPtr(CurrentItem.Next) = nil then
    Fetch;

  if not FCachedUpdates then begin
    CheckFetched(PtrOffset(CurrentItem, sizeof(TItemHeader)), nil);
    InternalDelete;
    RemoveRecord;
  end
  else
  if not LocalUpdate then begin
    if CurrentItem.Status = isDeleted then
      Exit;

    NextToCurrentItem := nil;
    PrevToCurrentItem := nil;
    if CurrentItem.Status = isUnmodified then begin
    // add to cache
      CacheItem := TCacheItem.Create;
      CacheItem.Item := CurrentItem;
      AddCacheItem(CacheItem);

      CurrentItem.Status := isDeleted;
      CurrentItem.UpdateResult := urNone;
    end
    else
    if (CurrentItem.Status = isAppended) and (CurrentItem.UpdateResult <> urApplied) then begin
      RemoveItemFromCache(CurrentItem);
      DeleteItem(CurrentItem);
    end
    else
    if (CurrentItem.Status = isAppended) or (CurrentItem.Status = isUpdated) then begin
      NextToCurrentItem := CurrentItem.Next;
      PrevToCurrentItem := CurrentItem.Prev;
      if CurrentItem.Status = isUpdated then
        RollbackItem(CurrentItem);
      CurrentItem.Status := isDeleted;
      CurrentItem.UpdateResult := urNone;
      CurrentItem.Order := 0;
    end;

    if IntPtr(CurrentItem) <> nil then
      CurrentItem.FilterResult := fsNotChecked;

    if IntPtr(NextToCurrentItem) <> nil then begin
      if IntPtr(NextToCurrentItem.Prev) <> nil then
        CurrentItem := NextToCurrentItem.Prev
      else
        CurrentItem := FirstItem;
    end
    else
    if IntPtr(PrevToCurrentItem) <> nil then begin
      if IntPtr(PrevToCurrentItem.Next) <> nil then
        CurrentItem := PrevToCurrentItem.Next
      else
        CurrentItem := LastItem;
    end;

    Dec(FRecordCount);
    ReorderItems(CurrentItem, roDelete);
  end
  else //LocalUpdate and CachedUpdates
    RemoveRecord;
end;

{ Edit }

{ Navigation }

function TMemData.GetFirstItem: PItemHeader;
begin
  Result := FirstItem;
end;

function TMemData.GetLastItem: PItemHeader;
begin
  Result := LastItem;
end;

function TMemData.GetCurrentItem: PItemHeader;
begin
  Result := CurrentItem;
end;

function TMemData.GetBOF: boolean;
begin
  Result := (IntPtr(CurrentItem) = nil) and FBOF; // WAR
end;

function TMemData.GetEOF: boolean;
begin
  Result := (IntPtr(CurrentItem) = nil) and FEOF; // WAR
end;

procedure TMemData.SetToBegin;
begin
  CurrentItem := nil; //FirstItem;
  FBOF := True;
  if IntPtr(LastItem) <> nil then
    FEOF := False;
end;

procedure TMemData.SetToEnd;
begin
  CurrentItem := nil; //LastItem;
  FEOF := True;
  if IntPtr(FirstItem) <> nil then
    FBOF := False;
end;

function TMemData.SetToItem(Item: PItemHeader): Boolean;
begin
  if Item.Flag = flUsed then begin
    CurrentItem := Item;
    FBOF := False;
    FEOF := False;
    Result := True;
  end
  else
    Result := False;
end;


procedure TMemData.PrepareRecNoCache(out Count: integer);
var
  Item: PItemHeader;
begin
  if Length(FRecordNoCache) > 0 then begin
    Count := Length(FRecordNoCache);
    Exit;
  end;

  Count := 0;
  Item := FirstItem;
  SetLength(FRecordNoCache, GetRecordCount);
  while IntPtr(Item) <> nil do begin
    if Item.FilterResult = fsNotOmitted then begin
      FRecordNoCache[Count] := Item;
      inc(Count);
    end;
    Item := Item.Next;
  end;
  SetLength(FRecordNoCache, Count);
end;

function TMemData.GetRecordNo: Integer;
begin
  if IntPtr(CurrentItem) <> nil then
    Result := CurrentItem.Order + FRecordNoOffset
  else
    Result := 0;
end;

procedure TMemData.SetRecordNo(Value: Integer);
var
  Item, CurrItem, LastOrderedItem: PItemHeader;
  ForwardDir: boolean;
begin
  if (IntPtr(FirstItem) <> nil) and (Value > 0) then begin
    if Length(FRecordNoCache) > 0 then begin
      if Value <= Length(FRecordNoCache) then
        CurrentItem := FRecordNoCache[Value - 1]
      else
        CurrentItem := nil;
      Exit;
    end;

    if IntPtr(CurrentItem) <> nil then
      CurrItem := CurrentItem
    else
      CurrItem := FirstItem;

    LastOrderedItem := LastItem;
    while OmitRecord(LastOrderedItem) do begin // if recordset is filtered
      LastOrderedItem := LastOrderedItem.Prev;
      if IntPtr(LastOrderedItem) = nil then
        Exit; // all records are rejected by filter
    end;

    if (Value < Abs(LastOrderedItem.Order + FRecordNoOffset - Value)) and
      (Value < Abs(CurrItem.Order + FRecordNoOffset - Value))
    then begin
    // from first
      Item := FirstItem;
      ForwardDir := True;
    end
    else
      if Abs(LastOrderedItem.Order + FRecordNoOffset - Value) <
        Abs(CurrItem.Order + FRecordNoOffset - Value)
      then begin
      // from
        Item := LastOrderedItem;
        ForwardDir := LastOrderedItem.Order + FRecordNoOffset < Value;
      end
      else begin
      // from current
        Item := CurrItem;
        ForwardDir := CurrItem.Order + FRecordNoOffset < Value;
      end;

    while (IntPtr(Item) <> nil) and ((Item.Order + FRecordNoOffset <> Value) or OmitRecord(Item)) do
      if ForwardDir then begin
        if IntPtr(Item.Next) = nil then
          Fetch;
        Item := Item.Next
      end
      else
        Item := Item.Prev;

    if IntPtr(Item) <> nil then
      CurrentItem := Item;
  end;
end;

{ Fetch }

function TMemData.Fetch(FetchBack: boolean = False): boolean;
begin
  Result := False;
end;

procedure TMemData.InitFetchedItems(FetchedItem: IntPtr; NoCountData, FetchBack: boolean);
var
  Item: PItemHeader;
  NewOrder: Integer;
begin
  Item := FetchedItem;
  if not FetchBack then begin
    NewOrder := 1;
    while (Item = FetchedItem) or ((IntPtr(Item) <> nil) and OmitRecord(Item)) do
      Item := Item.Prev;
    if IntPtr(Item) <> nil then
      NewOrder := Item.Order + 1;
  end
  else
    NewOrder := Item.Order;

  Item := FetchedItem;
  while IntPtr(Item) <> nil do begin
    if not OmitRecord(Item) then begin
      if not (NoCountData or FetchBack)  then
        Inc(FRecordCount);
      Item.Order := NewOrder;
      if FetchBack then
        Dec(NewOrder)
      else
        Inc(NewOrder);
    end;
    if FetchBack then
      Item := Item.Prev
    else
      Item := Item.Next;
  end;
end;

{ BookMarks }

function TMemData.GetRefreshIteration: integer;
begin
  Result := FRefreshIteration;
end;

procedure TMemData.GetBookmark(Bookmark: PRecBookmark);
begin
  Bookmark.RefreshIteration := FRefreshIteration;
  Bookmark.Item := CurrentItem;
  if IntPtr(CurrentItem) <> nil then
    Bookmark.Order := CurrentItem.Order + FRecordNoOffset
  else
    Bookmark.Order := -1;
end;

procedure TMemData.SetToBookmark(Bookmark: PRecBookmark);
begin
  if (IntPtr(Bookmark) <> nil) and
     (Bookmark.RefreshIteration = FRefreshIteration) and
     (IntPtr(Bookmark.Item) <> nil) and
     SetToItem(Bookmark.Item)
  then
    Exit;

  // Set by order
  inherited;
end;

function TMemData.BookmarkValid(Bookmark: PRecBookmark): boolean;
begin
  if IntPtr(Bookmark) <> nil then
    Result := ((Bookmark.Order <> -1) and (Bookmark.Order <= FRecordCount)) or
      ((Bookmark.RefreshIteration = FRefreshIteration) and (IntPtr(Bookmark.Item) <> nil) and
       (Bookmark.Item.Order <> -1) and (Bookmark.Item.Order <= FRecordCount))
  else
    Result := False;

  if Result and Filtered and (Bookmark.RefreshIteration = FRefreshIteration) then
    Result := not OmitRecord(Bookmark.Item);
end;

function TMemData.CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
  Result := RetCodes[IntPtr(Bookmark1) = nil, IntPtr(Bookmark2) = nil];
  if Result = 2 then
    if Bookmark1.RefreshIteration = Bookmark2.RefreshIteration then
      if Bookmark1.Item = Bookmark2.Item then begin
        Result := 0;
        Exit;
      end
      else
        if (IntPtr(Bookmark1.Item) <> nil) and (IntPtr(Bookmark2.Item) <> nil) then
          try // for freed item
            if Bookmark1.Item.Order >= Bookmark2.Item.Order then
              if Bookmark1.Item.Order = Bookmark2.Item.Order then
                Result := 0
              else
                Result := 1
            else
              Result := -1;

            Exit;
          except
          end;

// Compare by order
  Result := inherited CompareBookmarks(Bookmark1, Bookmark2);
end;

{ CachedUpdates }

function TMemData.GetUpdateStatus: TItemStatus;
begin
  if IntPtr(CurrentItem) <> nil then
    Result := CurrentItem.Status
  else
    Result := isUnmodified;
end;

function TMemData.GetUpdateResult: TUpdateRecAction;
begin
  if IntPtr(CurrentItem) <> nil then
    Result := CurrentItem.UpdateResult
  else
    Result := urNone;
end;

function TMemData.HasUpdatedOrDeletedRecords: boolean;
var
  CacheItem: TCacheItem;
begin
  Result := False;
  CacheItem := Cache;
  while (CacheItem <> nil) and not Result do begin
    Result := (CacheItem.Item.Status in [isUpdated, isDeleted]) and
              (CacheItem.Item.UpdateResult <> urApplied);
    CacheItem := CacheItem.Next;
  end;
end;

procedure TMemData.AddCacheItem(CacheItem: TCacheItem);
begin
// add to end cache
  CacheItem.Next := nil;
  if Cache = nil then
    Cache := CacheItem
  else
    LastCacheItem.Next := CacheItem;

  LastCacheItem := CacheItem;
end;

// remove record from cache
procedure TMemData.RemoveItemFromCache(Item: PItemHeader);
var
  CacheItem, OldCacheItem: TCacheItem;
begin
  if IntPtr(Item) <> nil then begin
    CacheItem := Cache;
    OldCacheItem := CacheItem;
    while CacheItem <> nil do begin
      if CacheItem.Item = Item then begin
        if CacheItem = LastCacheItem then
          if CacheItem = Cache then
            LastCacheItem := nil
          else
            LastCacheItem := OldCacheItem;

        if CacheItem = Cache then
          Cache := CacheItem.Next
        else
          OldCacheItem.Next := CacheItem.Next;

        FreeCachedItem(CacheItem);
        break;
      end;
      OldCacheItem := CacheItem;
      CacheItem := CacheItem.Next;
    end;
  end;
end;

procedure TMemData.FreeCachedItem(CachedItem: TCacheItem);
begin
  if IntPtr(CachedItem.Restore) <> nil then begin
    FreeComplexFields(PtrOffset(CachedItem.Restore, SizeOf(TItemHeader)), True);
    BlockMan.FreeItem(CachedItem.Restore);
  end;

  CachedItem.Free;
end;

procedure TMemData.SetCacheRecBuf(NewBuf: IntPtr; OldBuf: IntPtr);
begin
  NewCacheRecBuf := NewBuf;
  OldCacheRecBuf := OldBuf;
end;

procedure TMemData.ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds);
var
  CacheItem, NextCacheItem, PrevCacheItem: TCacheItem;
  Action: TUpdateRecAction;
  OldCurrentItem: PItemHeader;

  PacketCacheItem: TCacheItem;
  PrevPacketCacheItem: TCacheItem;

  function ValidateCacheItem: boolean;
    { On case of deleting current item from cache via RevertRecord in
      ApplyRecord call. Returns True if CacheItem was deleted. }
  begin
    Result := True;
    if PrevCacheItem <> nil then begin
      if PrevCacheItem.Next = NextCacheItem then begin
        CacheItem := NextCacheItem;
        Result := True;
      end;
    end
    else
      if NextCacheItem <> nil then begin
        if CacheItem.Next <> NextCacheItem then begin
          CacheItem := NextCacheItem;
          Result := False;
        end;
      end
      else
        if Cache = nil then begin
          CacheItem := nil;
          Result := False;
        end;
  end;

  procedure SetAction(Action: TUpdateRecAction);
  var
    Temp: TCacheItem;
  begin
    // Set action for batch of items
    if Action <> urSuspended then begin
      Temp := PacketCacheItem;
      while (Temp <> nil) and (Temp <> CacheItem) do begin
        Temp.Item.UpdateResult := Action;
        Temp := Temp.Next;
      end;
    end;
    CacheItem.Item.UpdateResult := Action;
  end;

var
  MoveDir: integer;
  IsRecordMoved: boolean;
begin
  IsRecordMoved := False;
  if FCachedUpdates then begin
    OldCurrentItem := CurrentItem;
    FInCacheProcessing := True;
    try
      PrevCacheItem := nil;
      PacketCacheItem := nil;
      PrevPacketCacheItem := nil;
      CacheItem := Cache;
      while CacheItem <> nil do
        if CacheItem.Item.UpdateResult <> urApplied then begin
          NextCacheItem := CacheItem.Next;
          try
            CurrentItem := CacheItem.Item; // for refresh on applied
            Action := urFail;
            try
              case CacheItem.Item.Status of
                isAppended: begin
                  if ukInsert in UpdateRecKinds then begin
                    BlockMan.GetRecord(CacheItem.Item, NewCacheRecBuf);
                    BlockMan.GetRecord(CacheItem.Item, OldCacheRecBuf);
                    ApplyRecord(ukInsert, Action, CacheItem.Next = nil);
                    if FIndexFields.Count > 0 then
                      MoveDir := CompareRecords(NewCacheRecBuf, PtrOffset(CacheItem.Item, sizeof(TItemHeader)))
                    else
                      MoveDir := 0;
                    BlockMan.PutRecord(CacheItem.Item, NewCacheRecBuf); // for ReturnParams
                    if MoveDir <> 0 then begin
                      IsRecordMoved := True;
                      MoveSortedRecord(MoveDir);
                    end;
                  end;
                end;
                isUpdated: begin
                  if ukUpdate in UpdateRecKinds then begin
                    BlockMan.GetRecord(CacheItem.Item, NewCacheRecBuf);
                    BlockMan.GetRecord(CacheItem.Item.Rollback, OldCacheRecBuf);
                    ApplyRecord(ukUpdate, Action, CacheItem.Next = nil);
                    if Action <> urSkip then begin
                      if FIndexFields.Count > 0 then
                        MoveDir := CompareRecords(NewCacheRecBuf, PtrOffset(CacheItem.Item, sizeof(TItemHeader)))
                      else
                        MoveDir := 0;
                      BlockMan.PutRecord(CacheItem.Item, NewCacheRecBuf); // for ReturnParams
                      if IntPtr(CacheItem.Restore) = nil then begin // Restore point
                        BlockMan.AllocItem(CacheItem.Restore);
                        InitItem(CacheItem.Restore);
                        BlockMan.CopyRecord(CacheItem.Item.Rollback, CacheItem.Restore);
                        AddRefComplexFields(PtrOffset(CacheItem.Restore, sizeof(TItemHeader)));
                      end;
                      FreeComplexFields(PtrOffset(CacheItem.Item.Rollback, SizeOf(TItemHeader)), True);
                      BlockMan.CopyRecord(CacheItem.Item, CacheItem.Item.Rollback);
                      AddRefComplexFields(PtrOffset(CacheItem.Item.Rollback, sizeof(TItemHeader)));
                      if MoveDir <> 0 then begin
                        IsRecordMoved := True;
                        MoveSortedRecord(MoveDir);
                      end;
                    end;
                  end;
                end;
                isDeleted: begin
                  if ukDelete in UpdateRecKinds then begin
                    BlockMan.GetRecord(CacheItem.Item, NewCacheRecBuf);
                    BlockMan.GetRecord(CacheItem.Item, OldCacheRecBuf);
                    ApplyRecord(ukDelete, Action, CacheItem.Next = nil);
                  end;
                end;
              else
                Assert(False);
              end;
            finally
              if Active and ValidateCacheItem then begin
                SetAction(Action);
                case Action of
                  urSuspended:
                    if PacketCacheItem = nil then begin
                      PacketCacheItem := CacheItem;
                      PrevPacketCacheItem := PrevCacheItem;
                    end;
                  urRetry:
                    if PacketCacheItem <> nil then begin
                      CacheItem := PacketCacheItem;
                      PrevCacheItem := PrevPacketCacheItem;
                    end;
                else
                  PacketCacheItem := nil;
                  PrevPacketCacheItem := nil;
                end;
                if Action <> urRetry then begin
                  PrevCacheItem := CacheItem;
                  CacheItem := NextCacheItem;
                end;
              end;
            end;
          except
            if (CacheItem <> nil) and (Cache <> nil) then
              OldCurrentItem := CacheItem.Item; // failed item is current
            raise;
          end;
        end
        else
          CacheItem := CacheItem.Next;

      DoAfterApplyUpdates;
    finally
      FInCacheProcessing := False;
      if (IntPtr(OldCurrentItem) = nil) or (OldCurrentItem.Flag = flUsed) then
        CurrentItem := OldCurrentItem;
    end;
    if IsRecordMoved then
      ReorderItems(nil, roFull);

    InternalCacheApplied;
  end;
end;

procedure TMemData.CommitUpdates;
var
  CacheItem, CacheItem1: TCacheItem;
begin
  if UpdatesPending then
    ApplyUpdates([ukUpdate, ukInsert, ukDelete]);

  FInCacheProcessing := True;
  try
    CacheItem := Cache;
    LastCacheItem := nil;
    while CacheItem <> nil do
      if CacheItem.Item.UpdateResult = urApplied then begin
        if IntPtr(CacheItem.Item.Rollback) <> nil then begin
          FreeComplexFields(PtrOffset(CacheItem.Item.Rollback, SizeOf(TItemHeader)), True);
          BlockMan.FreeItem(CacheItem.Item.Rollback);
          CacheItem.Item.Rollback := nil;
        end;

        if CacheItem.Item.Status = isDeleted then
          DeleteItem(CacheItem.Item)
        else begin
          CacheItem.Item.Status := isUnmodified;
          CacheItem.Item.UpdateResult := urNone;
        end;

        CacheItem1 := CacheItem;
        CacheItem := CacheItem.Next;

        if CacheItem1 = Cache then
          Cache := CacheItem;

        if (LastCacheItem <> nil) and (CacheItem1 = LastCacheItem.Next) then
          LastCacheItem.Next := CacheItem;
        FreeCachedItem(CacheItem1);
      end
      else begin
        LastCacheItem := CacheItem;
        CacheItem := CacheItem.Next;
      end;
  finally
    FInCacheProcessing := False;
  end;
end;

procedure TMemData.RollbackItem(Item: PItemHeader);
var
  i: integer;
  Blob, RollbackBlob: TBlob;
  RollbackRecBuf: IntPtr;
  ItemRecBuf: IntPtr;
  MoveDir: integer;
  Field: TFieldDesc;
begin
  // Rollback contains instances of TBlob class.
  // We need to keep blobs from ItemRecBuf (TOraLob, TIBCBlob) and set its Data from Rollback
  RollbackRecBuf := PtrOffset(Item.Rollback, sizeof(TItemHeader));
  ItemRecBuf := PtrOffset(Item, sizeof(TItemHeader));
  for i := 0 to FFields.Count - 1 do begin
    Field := FFields[i];
    if Field.IsBlob then begin
      Blob := TBlob(InternalGetObject(PtrOffset(ItemRecBuf, Field.DataOffset)));
      RollbackBlob := TBlob(InternalGetObject(PtrOffset(RollbackRecBuf, Field.DataOffset)));
      if Blob <> RollbackBlob then begin
        Blob.AddRef; // prevent deleting blob by FreeComplexFields
        Blob.SetData(RollbackBlob.FData);
        Marshal.WriteIntPtr(RollbackRecBuf, Field.DataOffset, Blob.GCHandle);
        RollbackBlob.Free;
      end;
    end;
  end;

  MoveDir := CompareRecords(PtrOffset(Item.Rollback, sizeof(TItemHeader)), PtrOffset(Item, sizeof(TItemHeader)));
  FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True);
  BlockMan.CopyRecord(Item.Rollback, Item);
  BlockMan.FreeItem(Item.Rollback);
  Item.Rollback := nil;

  if FIndexFields.Count > 0 then
    MoveSortedRecord(MoveDir);
end;

procedure TMemData.RevertItem(Item: PItemHeader);
begin
  case Item.Status of
    isAppended: begin
      DeleteItem(Item);
      Dec(FRecordCount);
    end;
    isUpdated: begin
      RollbackItem(Item);
      Item.Status := isUnmodified;
      Item.UpdateResult := urNone;
      Item.FilterResult := fsNotChecked;
    end;
    isDeleted: begin
      Item.Status := isUnmodified;
      Item.UpdateResult := urNone;
      Item.FilterResult := fsNotChecked;
      Inc(FRecordCount);
    end;
    isUnmodified: begin
      Item.UpdateResult := urNone;
      Item.FilterResult := fsNotChecked;
    end;
  end;

  InternalCacheChanged;
end;

procedure TMemData.RevertRecord;
var
  CacheItem: TCacheItem;
  OldCacheItem: TCacheItem;
begin
  if Cache <> nil then begin
    CacheItem := Cache;
    OldCacheItem := CacheItem;
    while (CacheItem <> nil) and not (CacheItem.Item = CurrentItem) do begin
      OldCacheItem := CacheItem;
      CacheItem := CacheItem.Next;
    end;
    if CacheItem <> nil then begin
      if OldCacheItem <> CacheItem then
        OldCacheItem.Next := CacheItem.Next
      else
        Cache := CacheItem.Next;

      RevertItem(CacheItem.Item);
      if CacheItem = LastCacheItem then
        LastCacheItem := OldCacheItem;
      FreeCachedItem(CacheItem);
    end;
  end;
end;

procedure TMemData.CancelUpdates;
var
  CacheItem: TCacheItem;
begin
  if Cache <> nil then begin
    InternalCacheCanceled;

    while Cache <> nil do begin
      RevertItem(Cache.Item);
      CacheItem := Cache;
      Cache := Cache.Next;
      FreeCachedItem(CacheItem);
    end;

    LastCacheItem := nil;

    ReorderItems(nil, roFull);
  end;
end;

procedure TMemData.RestoreUpdates;
var
  CacheItem: TCacheItem;
begin
  if FCachedUpdates then begin
    CacheItem := Cache;
    while CacheItem <> nil do begin
      if IntPtr(CacheItem.Restore) <> nil then begin
        FreeComplexFields(PtrOffset(CacheItem.Item.Rollback, SizeOf(TItemHeader)), True);
        BlockMan.CopyRecord(CacheItem.Restore, CacheItem.Item.Rollback);
        BlockMan.FreeItem(CacheItem.Restore);
        CacheItem.Restore := nil;
      end;
      CacheItem.Item.UpdateResult := urNone;
      CacheItem := CacheItem.Next;
    end;
  end;
end;

function TMemData.GetUpdatesPending: boolean;
var
  CacheItem: TCacheItem;
begin
  Result := False;
  CacheItem := Cache;
  while (CacheItem <> nil) and not Result do begin
    Result := CacheItem.Item.UpdateResult <> urApplied;
    CacheItem := CacheItem.Next;
  end;
end;

procedure TMemData.GetOldRecord(RecBuf: IntPtr);
begin
  if not(EOF or BOF or (IntPtr(CurrentItem) = nil)) then begin
    if OmitRecord(CurrentItem) then
      GetNextRecord(RecBuf);
    if IntPtr(CurrentItem) <> nil then
      if IntPtr(CurrentItem.Rollback) <> nil then
        BlockMan.GetRecord(CurrentItem.Rollback, RecBuf)
      else
        BlockMan.GetRecord(CurrentItem, RecBuf);
  end;
end;

{ Filter }

procedure TMemData.FilterUpdated;
begin
  ClearItemsOmittedStatus;
  ReorderItems(nil, roFull);
  FEOF := GetRecordCount = 0; // for correct navigation
end;

procedure TMemData.ClearItemsOmittedStatus;
var
  Item: PItemHeader;
begin
  Item := FirstItem;
  while IntPtr(Item) <> nil do begin
    Item.FilterResult := fsNotChecked;
    Item := Item.Next;
  end;
end;

procedure TMemData.SetFilterItemTypes(const Value: TItemTypes);
begin
  if Value <> FilterItemTypes then begin
    inherited;

    if Active then
      FilterUpdated;
  end;
end;

function TMemData.IsSupportedDataType(DataType: word): boolean;
begin
  Result := False;
end;

function TMemData.IsSpecificType(const Field: TFieldDesc; var DataType: word; var DataTypeName: string; var Len, Scale: integer): boolean;
begin
  DataType := dtUnknown;
  DataTypeName := '';
  Len := -1;
  Scale := -1;
  Result := False;
end;

{ TBlockManager }

constructor TBlockManager.Create;
begin
  Assert((SizeOf(TBlockHeader) and 1) = 0);
  Assert((SizeOf(TItemHeader) and 1) = 0);

  inherited;

  DefaultItemCount := 25;
end;

destructor TBlockManager.Destroy;
begin
  FreeAllBlocks;

  inherited;
end;

procedure TBlockManager.AllocBlock(out Block: PBlockHeader; ItemCount: Word; StandAloneBlock: Boolean = False);
var
  BlockSize: integer;
begin
  BlockSize := sizeof(TBlockHeader) + ItemCount * (sizeof(TItemHeader) + RecordSize);
  GetMem(Block, BlockSize);
{$IFDEF CRDEBUG}
  FillChar(Block, BlockSize, 255);
{$ENDIF}

  Block.ItemCount := ItemCount;
  Block.UsedItems := 0;

  if StandAloneBlock then begin
    Block.Next := nil;
    Block.Prev := nil;
  end
  else begin
    Block.Next := FirstBlock;
    Block.Prev := nil;
    if IntPtr(FirstBlock) <> nil then
      FirstBlock.Prev := Block;
    FirstBlock := Block;
  end;
end;

procedure TBlockManager.ReAllocBlock(var Block: PBlockHeader; ItemCount: Word);
var
  NewBlock: PBlockHeader;
  BlockSize: integer;
begin
  BlockSize := sizeof(TBlockHeader) + ItemCount * (sizeof(TItemHeader) + RecordSize);
  GetMem(NewBlock, BlockSize);
  Move(Block^, NewBlock^, sizeof(TBlockHeader));

  NewBlock.ItemCount := ItemCount;
  NewBlock.UsedItems := 0;
  NewBlock.Prev := Block.Prev;
  NewBlock.Next := Block.Next;

  if Block.Next <> nil then
    Block.Next.Prev := NewBlock;
  if Block.Prev <> nil then
    Block.Prev.Next := NewBlock;
  if FirstBlock = Block then
    FirstBlock := NewBlock;

  FreeMem(Block);
  Block := NewBlock;
end;

procedure TBlockManager.FreeBlock(Block: PBlockHeader; StandAloneBlock: Boolean = False);
begin
  if not StandAloneBlock then begin
    if Block = FirstBlock then begin
      FirstBlock := Block.Next;
      if IntPtr(FirstBlock) <> nil then
        FirstBlock.Prev := nil;
    end
    else begin
      Block.Prev.Next := Block.Next;
      if IntPtr(Block.Next) <> nil then
        Block.Next.Prev := Block.Prev;
    end;
  end;

  FreeMem(Block{, BlockSize}); // commented because of a bug with Lazarus with enabled Heaptrc under Linux
end;

procedure TBlockManager.FreeAllBlocks;
begin
  while IntPtr(FirstBlock) <> nil do
    FreeBlock(FirstBlock);

  FirstFree := nil;
end;

procedure TBlockManager.AddFreeBlock;
var
  i: Word;
  Block: PBlockHeader;
  Item: PItemHeader;
  ItemSize: Integer;
begin
  AllocBlock(Block, DefaultItemCount);

  Item := PtrOffset(Block, sizeof(TBlockHeader));
  ItemSize := sizeof(TItemHeader) + RecordSize;
  for i := 1 to DefaultItemCount do begin
    Item.Prev := nil;
    Item.Next := FirstFree;
    Item.Block := Block;
    Item.Flag := flFree;

    if IntPtr(FirstFree) <> nil then
      FirstFree.Prev := Item;
    FirstFree := Item;

    Item := PtrOffset(Item, ItemSize);
  end;
  Block.UsedItems := 0;
end;

procedure TBlockManager.AllocItem(out Item: PItemHeader);
begin
  if IntPtr(FirstFree) = nil then
    AddFreeBlock;

  Item := FirstFree;

  Assert(Item.Flag = flFree);
  Item.Flag := flUsed;

  FirstFree := FirstFree.Next;
  if IntPtr(FirstFree) <> nil then
    FirstFree.Prev := nil;

  Item.Block.UsedItems := Item.Block.UsedItems + 1;
end;

procedure TBlockManager.FreeItem(Item: PItemHeader);
var
  i: integer;
  Free: PItemHeader;
  ItemSize: integer;
begin
  Assert(Item.Flag = flUsed);
  Item.Flag := flFree;

  if Item.Block.UsedItems = 1 then begin
  // Procesing Free List
    Free := PtrOffset(Item.Block, sizeof(TBlockHeader));
    ItemSize := sizeof(TItemHeader) + RecordSize;
    for i := 1 to Item.Block.ItemCount do begin
      if not (Free = Item) then begin
        Assert(Free.Flag = flFree);

        if Free = FirstFree then begin
          FirstFree := Free.Next;
          if IntPtr(FirstFree) <> nil then
            FirstFree.Prev := nil;
        end
        else begin
          Free.Prev.Next := Free.Next;
          if IntPtr(Free.Next) <> nil then
            Free.Next.Prev := Free.Prev;
        end;
      end;
      Free := PtrOffset(Free, ItemSize);
    end;
    FreeBlock(Item.Block);
  end
  else begin
    Item.Prev := nil;
    Item.Next := FirstFree;
    if IntPtr(FirstFree) <> nil then
      FirstFree.Prev := Item;
    FirstFree := Item;
    Item.Block.UsedItems := Item.Block.UsedItems - 1;
  end;
end;

procedure TBlockManager.PutRecord(Item: PItemHeader; Rec: IntPtr);
begin
  CopyBuffer(Rec, PtrOffset(Item, sizeof(TItemHeader)), RecordSize);
end;

procedure TBlockManager.GetRecord(Item: PItemHeader; Rec: IntPtr);
begin
  CopyBuffer(PtrOffset(Item, sizeof(TItemHeader)), Rec, RecordSize);
end;

function TBlockManager.GetRecordPtr(Item: PItemHeader): IntPtr;
begin
  Result := PtrOffset(Item, sizeof(TItemHeader));
end;

procedure TBlockManager.CopyRecord(ItemSrc: PItemHeader; ItemDest: PItemHeader);
begin
  CopyBuffer(PtrOffset(ItemSrc, sizeof(TItemHeader)), PtrOffset(ItemDest, sizeof(TItemHeader)), RecordSize);
end;

{ TStringHeap }

constructor TStringHeap.Create;
begin
  inherited;

  FRoot := nil;
  FEmpty := True;
  FSysGetMem := False;

{$IFDEF WIN32}
  FUseSysMemSize := not IsMemoryManagerSet;
{$ENDIF}
  FThreadSafety := False;
  FThreadSafetyCS := nil;
end;

destructor TStringHeap.Destroy;
begin
  Clear;
  FThreadSafetyCS.Free;

  inherited;
end;

procedure TStringHeap.SetThreadSafety(const Value: boolean);
begin
  if Value <> FThreadSafety then begin
    FThreadSafety := Value;
    if Value then begin
      Assert(FThreadSafetyCS = nil);
      FThreadSafetyCS := TCriticalSection.Create;
    end
    else begin
      FThreadSafetyCS.Free;
      FThreadSafetyCS := nil;
    end;
  end;
end;

procedure TStringHeap.Clear;
var
  P, Temp: PBlock;
  i: integer;
begin
  if Empty then
    Exit;
  if FThreadSafetyCS <> nil then
    FThreadSafetyCS.Acquire;
  try
    P := FRoot;
    while IntPtr(P) <> nil do begin
      Temp := P;
      P := P.Next;
      Marshal.FreeHGlobal(Temp);
    end;
    FRoot := nil;
    FFree := SizeOf_TStrData;
    for i := Low(FSmallTab) to High(FSmallTab) do
      FSmallTab[i] := nil;
    FEmpty := True;
    FSysGetMem := False;
  finally
    if FThreadSafetyCS <> nil then
      FThreadSafetyCS.Release;
  end;
end;

function TStringHeap.AllocStr(Str: IntPtr; Len: integer = -1): IntPtr;
begin
  if Str = nil then
    Result := nil
  else begin
    if Len < 0 then
      Len := {$IFDEF VER18P}{$IFNDEF NEXTGEN}AnsiStrings.{$ENDIF}{$ENDIF}StrLen(PAChar(Str));
    Result := NewBuf(Integer(Len) + 1);
    Move(Str^, Result^, Len);
    Marshal.WriteByte(Result, Len, 0);
  end;
end;

function TStringHeap.AllocTrimmedStr(Str: IntPtr; var Len: Integer): IntPtr;
var
  EndPtr: PByte;
begin
  if Str = nil then
    Result := nil
  else begin
    EndPtr := PtrOffset(Str, Len - 1);
    // check for both $00 and ' '
    while (NativeUInt(EndPtr) >= NativeUInt(Str)) and (EndPtr^ in [$00, $20 {' '}]) do
      Dec(EndPtr);
    Len := PtrSubstract(EndPtr, Str) + 1;
    Result := NewBuf(Integer(Len) + 1);
    Move(Str^, Result^, Len);
    Marshal.WriteByte(Result, Len, 0);
  end;
end;

function TStringHeap.AllocWideStr(Str: IntPtr; Len: integer = -1): IntPtr;
var
  DataSize: Integer;
begin
  if Str = nil then
    Result := nil
  else begin
    if Len < 0 then
      Len := StrLenW(Str);
    DataSize := Len * SizeOf(WideChar);
    Result := NewBuf(DataSize + SizeOf(WideChar));
    Move(Str^, Result^, DataSize);
    Marshal.WriteInt16(Result, DataSize, 0);
  end;
end;

//  pwc: PWideChar;
//begin
//  pwc := PWideChar(Str) + Len - 1;
//
//  while pwc >= Str do
//    if pwc^ = #0 then
//      Dec(pwc)
//    else if pwc^ = ' ' then
//      Dec(pwc)
//    else
//      Break;

function TStringHeap.AllocTrimmedWideStr(Str: IntPtr; var Len: Integer): IntPtr;
var
  EndPtr: PWord;
  DataSize: Integer;
begin
  if Str = nil then
    Result := nil
  else begin
    EndPtr := PtrOffset(Str, (Len - 1) * SizeOf(WideChar));
    // check for both $0000 and ' '
    while (NativeUInt(EndPtr) >= NativeUInt(Str)) and (EndPtr^ in [$0000, $0020 {' '}]) do
      Dec(EndPtr);
    DataSize := PtrSubstract(EndPtr, Str) + 2;
    Len := DataSize shr 1;
    Result := NewBuf(DataSize + SizeOf(WideChar));
    if Len > 0 then
      Move(Str^, Result^, DataSize);
    Marshal.WriteInt16(Result, DataSize, 0);
  end;
end;

//function TStringHeap.ReAllocStr(Str: IntPtr; Trim: boolean = false): IntPtr;
//begin
//  if Trim then
//    Result := AllocTrimmedStr(Str)
//  else
//    Result := AllocStr(Str);
//  DisposeBuf(Str);
//end;
//
//function TStringHeap.ReAllocWideStr(Str: IntPtr; Trim: boolean = false): IntPtr;
//begin
//  Result := AllocStr(Str, Trim);
//  DisposeBuf(Str);
//end;

function TStringHeap.UseSmallTabs(divSize: integer): boolean;
begin
  Result := divSize <= SmallSize div Align;
  // This fix was added 04.04.2006 and rolled back 07.03.2007 because of bug
  // with allocation using memory manager and disposing using StringHeap block of memroy with size 2002 bytes
  // if (not Result) and ((Size - 1) div Align <= SmallSize div Align) then
  //   Result := True;
end;

function TStringHeap.NewBuf(Size: integer): IntPtr;
var
  P: IntPtr;
  Temp: PBlock;
  Idx: integer;
  divSize, sz: integer;
begin
  if Size <= 0 then begin
    Result := nil;
  end
  else begin
    if FThreadSafetyCS <> nil then
      FThreadSafetyCS.Acquire;
    try
      FEmpty := False;
      divSize := (Size + Align - 1) div Align;
      if UseSmallTabs(divSize) then begin
        Result := FSmallTab[divSize];
        if Result <> nil then begin
          FSmallTab[divSize] := Marshal.ReadIntPtr(Result);
          p := PtrOffset(Result, - SizeOf(Word));
          Marshal.WriteInt16(p, Marshal.ReadInt16(p) + 1);
          Exit;
        end;
        Size := divSize * Align;
        if IntPtr(FRoot) = nil then begin
          FRoot := Marshal.AllocHGlobal(SizeOf_TBlock);
          FRoot.Next := nil;
          FFree := SizeOf_TStrData;
        end
        else
        if FFree < Size + SizeOf(Integer) + SizeOf(Word) then begin
          P := PtrOffset(IntPtr(FRoot), SizeOf(PBlock) + SizeOf_TStrData - FFree);
          divSize := (FFree - SizeOf(Integer) - SizeOf(Word)) div Align;
          Marshal.WriteInt32(P, divSize * Align);
          P := PtrOffset(P, SizeOf(Integer));
          Marshal.WriteInt16(P, RefNull);
          P := PtrOffset(P, SizeOf(Word));
          Idx := divSize;
          Marshal.WriteIntPtr(P, FSmallTab[Idx]);
          FSmallTab[Idx] := P;
          Temp := FRoot;
          FRoot := Marshal.AllocHGlobal(SizeOf_TBlock);
          FRoot.Next := Temp;
          FFree := SizeOf_TStrData;
        end;
        Result := PtrOffset(IntPtr(FRoot), SizeOf(PBlock) + SizeOf_TStrData - FFree);
        Marshal.WriteInt32(Result, Size);
        Dec(FFree, Size + SizeOf(Integer) + SizeOf(Word));
        if FFree < SizeOf(Integer) + SizeOf(Word) + Align then begin
          sz := Marshal.ReadInt32(Result) + FFree and not (Align - 1);
          if sz <= SmallSize then
            Marshal.WriteInt32(Result, sz);
          Temp := FRoot;
          FRoot := Marshal.AllocHGlobal(SizeOf_TBlock);
          FRoot.Next := Temp;
          FFree := SizeOf_TStrData;
        end;
        Result := PtrOffset(Result, SizeOf(Integer));
      end
      else begin
      {$IFDEF WIN32}
        if FUseSysMemSize then
          Result := Marshal.AllocHGlobal(Size + SizeOf(Word))
        else begin
      {$ENDIF}
          Result := Marshal.AllocHGlobal(Size + SizeOf(Word) + SizeOf(Integer));
          Marshal.WriteInt32(Result, Size);
          Result := PtrOffset(Result, SizeOf(Integer));
      {$IFDEF WIN32}
        end;
      {$ENDIF}
        FSysGetMem := True;
      end;
      Marshal.WriteInt16(Result, RefNull);
      Result := PtrOffset(Result, SizeOf(Word));
    finally
      if FThreadSafetyCS <> nil then
        FThreadSafetyCS.Release;
    end;
  end;
end;

procedure TStringHeap.DisposeBuf(Buf: IntPtr);
var
  Size: integer;
  PRefCount: IntPtr;
  RefCount: Word;
  Idx: integer;
  divSize: integer;
begin
  if (Buf <> nil) then begin
    if FThreadSafetyCS <> nil then
      FThreadSafetyCS.Acquire;
    try
      PRefCount := PtrOffset(Buf, - SizeOf(Word));
      RefCount := Marshal.ReadInt16(PRefCount);
      Assert(RefCount >= RefNull, 'DisposeBuf failed');
      if RefCount = RefNull then begin
        Marshal.WriteInt16(PRefCount, RefCount - 1);
        Size := Marshal.ReadInt32(PRefCount, - SizeOf(Integer));
        divSize := (Size + Align - 1) div Align;
        Assert(divSize <> 0, 'SmallTab in DisposeBuf failed');
        if UseSmallTabs(divSize) then begin
          Idx := divSize;
          Marshal.WriteIntPtr(Buf, FSmallTab[Idx]);
          FSmallTab[Idx] := Buf;
        end
        else
        {$IFDEF WIN32}
          if FUseSysMemSize then
            Marshal.FreeHGlobal(PRefCount)
          else
        {$ENDIF}
            Marshal.FreeHGlobal(PtrOffset(PRefCount, - SizeOf(Integer)));
      end
      else
        Marshal.WriteInt16(PRefCount, RefCount - 1);
    finally
      if FThreadSafetyCS <> nil then
        FThreadSafetyCS.Release;
    end;
  end;
end;

procedure TStringHeap.AddRef(Buf: IntPtr);
var
  PRefCount: IntPtr;
  RefCount: Word;
begin
  if (Buf <> nil) then begin
    PRefCount := PtrOffset(Buf, - SizeOf(Word));
    RefCount := Marshal.ReadInt16(PRefCount);
    Assert(RefCount >= RefNull, 'AddRefStr failed');
    Marshal.WriteInt16(PRefCount, RefCount + 1);
  end;
end;

{ TSharedObject }

constructor TSharedObject.Create;
begin
  inherited;

  AddRef;
{$IFDEF CRDEBUG}
  InterlockedIncrement(ShareObjectCnt);
{$ENDIF}
end;

destructor TSharedObject.Destroy;
begin
{$IFDEF CRDEBUG}
  InterlockedDecrement(ShareObjectCnt);
{$ENDIF}
  FRefCount := 0;

  inherited;
end;

procedure TSharedObject.Free;
begin
  if Assigned(Self) then begin
    Assert(FRefCount > 0, ClassName + '.Free RefCount = ' + IntToStr(FRefCount));

    if FRefCount = 1 then begin
      if FGCHandle <> nil then
        FreeGCHandle(FGCHandle);
      inherited Free;
    end
    else
      Dec(FRefCount);

  {$IFDEF AUTOREFCOUNT}
    __ObjRelease;
  {$ENDIF}
  end;
end;

function TSharedObject.ToVariant: Variant;
begin
  Result := Unassigned;
  TVarData(Result).VType := varSharedObject;
  TVarData(Result).VPointer := Self;
end;

class function TSharedObject.FromVariant(const Source: Variant): TSharedObject;
begin
  if TVarData(Source).VType in [varEmpty, varNull] then
    Result := nil
  else begin
    Assert(TVarData(Source).VType = varSharedObject);
    Result := TObject(TVarData(Source).VPointer) as TSharedObject;
  end;
end;

procedure TSharedObject.CheckValid;
begin
  if FRefCount = 0 then
    raise Exception.Create(SInvalidSharedObject);
end;

procedure TSharedObject.AddRef;
begin
{$IFDEF AUTOREFCOUNT}
  __ObjAddRef;
{$ENDIF}

  Inc(FRefCount);
end;

procedure TSharedObject.Release;
begin
  Free;
end;

{$IFNDEF VER12P}
{$IFNDEF FPC}
function TSharedObject.GetHashCode: NativeInt;
begin
  Result := NativeInt(Self);
end;
{$ENDIF}
{$ENDIF}

function TSharedObject.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

procedure TSharedObject.Disconnect;
begin
end;

{ TPiece }

function NextPiece(Piece: PPieceHeader): PPieceHeader;
begin
  if IntPtr(Piece) <> nil then
    Result := Piece.Next
  else
    Result := nil;
end;

function PieceData(Piece: PPieceHeader): IntPtr;
begin
  if IntPtr(Piece) <> nil then
    Result := PtrOffset(Piece, sizeof(TPieceHeader))
  else
    Result := nil;
end;

function PieceUsedPtr(Piece: PPieceHeader): IntPtr;
begin
  if IntPtr(Piece) <> nil then
    Result := @Piece.Used
  else
    Result := nil;
end;

{ TCRBlobData }

constructor TCRBlobData.Create;
begin
  inherited Create;

  FIsNull := True;
  FPieceSize := DefaultPieceSize;
  FLargePieceSize := DefaultLargePieceSize;
end;

destructor TCRBlobData.Destroy;
begin
  Clear;

  inherited Destroy;
end;

function TCRBlobData.GetLargePieceSize: Integer;
begin
  if FLargePieceSize > FPieceSize then
    Result := FLargePieceSize
  else
    Result := FPieceSize;
end;

function TCRBlobData.GetDataSize: Cardinal; // sum of pieces.used
var
  Piece: PPieceHeader;
begin
  Result := 0;
  Piece := FFirstPiece;
  while Piece <> nil do begin
    Inc(Result, Piece.Used);
    Piece := Piece.Next;
  end;
end;

function TCRBlobData.GetSize: Cardinal;
begin
  Result := GetDataSize;
end;

procedure TCRBlobData.SetSize(Value: Cardinal);
var
  Piece: PPieceHeader;
  OldSize: Cardinal;
begin
  OldSize := GetSize;
  if OldSize > Value then
    Truncate(Value)
  else
    if OldSize < Value then begin
      AllocPiece(Piece, Value - OldSize);
      Piece.Used := Value - OldSize;
      FillChar(PtrOffset(Piece, Sizeof(TPieceHeader)), Value - OldSize, 0);
      AppendPiece(Piece);
    end;
end;

procedure TCRBlobData.Clear;
begin
  Truncate(0);
  FIsNull := True;
end;

procedure TCRBlobData.Truncate(NewSize: Cardinal);
var
  Piece: PPieceHeader;
  Size: Cardinal;
begin
  if NewSize > 0 then begin
    Size := 0;
    Piece := FFirstPiece;
    while Piece <> nil do begin
      if Size + Cardinal(Piece.Used) > NewSize then
        Piece.Used := NewSize - Size;
      Inc(Size, Piece.Used);
      Piece := Piece.Next;
    end;
  end
  else
    while FFirstPiece <> nil do begin
      Piece := FFirstPiece;
      FFirstPiece := FFirstPiece.Next;
      Marshal.FreeHGlobal(Piece);
    end;
end;

function TCRBlobData.IsEmpty: boolean;
begin
  Result := (FFirstPiece = nil) or (FFirstPiece.Used = 0);
end;

function TCRBlobData.IsNull: boolean;
begin
  Result := FIsNull and IsEmpty;
end;

function TCRBlobData.Read(Position: Cardinal; Count: Cardinal; Dest: IntPtr): Cardinal;
var
  Piece: PPieceHeader;
  Pos, { shift from Blob begin }
  Shift, { for read, in Piece }
  ReadCount, { all }
  MoveSize: Cardinal; { in Piece }
  Size: Cardinal;
begin
  Result := 0;
  Size := GetSize;

  if (FFirstPiece = nil) or (Position > Size) then
    Exit;

  if Count = 0 then
    Count := Size;

  if Position + Count > Size then
    Count := Size - Position;

  Piece := FFirstPiece;
  ReadCount := 0;
  Pos := 0;
  while (Piece <> nil) and (Pos < Position + Count) do begin
    if Pos + Cardinal(Piece.Used) > Position then begin
      if Position > Pos then
        Shift := Position - Pos
      else
        Shift := 0;

      if Pos + Cardinal(Piece.Used) > Position + Count then
        MoveSize := ({$IFDEF FPC}Int64{$ENDIF}(Position) + Count) - ({$IFDEF FPC}Int64{$ENDIF}(Pos) + Shift)
      else
        MoveSize := Cardinal(Piece.Used) - Shift;

      CopyBuffer(PtrOffset(Piece, sizeof(TPieceHeader) + Shift), PtrOffset(Dest, ReadCount), MoveSize);
      Inc(ReadCount, MoveSize);
    end;
    Inc(Pos, Piece.Used);
    Piece := Piece.Next;
  end;
  Result := ReadCount;
end;

{ similar to Read }

procedure TCRBlobData.Write(Position: Cardinal; Count: Cardinal; Source: IntPtr);
var
  Piece: PPieceHeader;
  CurPieceSize: Integer;
  Pos, { shift from Blob begin }
  Shift, { for write, in Piece }
  WriteCount, { all }
  MoveSize: Cardinal; { in Piece }
  Size: Cardinal;
begin
  FIsNull := False;

  Size := GetSize;
  if (Position > Size) then
    Position := Size;

  Piece := FFirstPiece;
  WriteCount := 0;
  Pos := 0;
  while (Pos < Position + Count) do begin
    if Piece = nil then begin
      CurPieceSize := LargePieceSize;
      if Count < Cardinal(CurPieceSize) then
        CurPieceSize := Count;
      AllocPiece(Piece, CurPieceSize);
      AppendPiece(Piece);
    end;

    if Pos + Cardinal(Piece.Size) > Position then begin
      if Position > Pos then
        Shift := Position - Pos
      else
        Shift := 0;

      if Pos + Cardinal(Piece.Size) > Position + Count then
        MoveSize := ({$IFDEF FPC}Int64{$ENDIF}(Position) + Count) - ({$IFDEF FPC}Int64{$ENDIF}(Pos) + Shift)
      else
        MoveSize := Cardinal(Piece.Size) - Shift;

      CopyBuffer(PtrOffset(Source, WriteCount), PtrOffset(Piece, sizeof(TPieceHeader) + Shift), MoveSize);
      Inc(WriteCount, MoveSize);

      Assert(Shift <= Cardinal(Piece.Used));
      if (Shift + MoveSize) > Cardinal(Piece.Used) then
        Piece.Used := Shift + MoveSize;
    end;
    Inc(Pos, Piece.Used);
    Piece := Piece.Next;
  end;
end;

procedure TCRBlobData.Compress;
var
  Piece: PPieceHeader;
  NextPiece: PPieceHeader;
begin
  Piece := FFirstPiece;
  while Piece <> nil do begin
    NextPiece := Piece.Next;
    CompressPiece(Piece);
    Piece := NextPiece;
  end;
end;

procedure TCRBlobData.Defrag(MinPieceSize: Integer = 0); // Move all data to first piece if PieceSize = 0
var
  Size, WriteCount: Integer;
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  SourcePos: Integer;
  DestPos: Integer;
begin
  if FFirstPiece = nil then
    Exit; // Is empty

  Size := GetDataSize;
  if MinPieceSize = 0 then
    MinPieceSize := Size
  else if (MinPieceSize = 0) or ((MinPieceSize > 0) and (MinPieceSize > Size)) then
    MinPieceSize := Size;

  DestPiece := FFirstPiece;
  while True do begin
    if DestPiece.Used = 0 then begin
      SourcePiece := DestPiece;
      DestPiece := DestPiece.Next;
      FreePiece(SourcePiece);
    end
    else if (DestPiece.Used >= MinPieceSize) or (DestPiece.Used = Size) then begin
      Size := Size - DestPiece.Used;
      DestPiece := DestPiece.Next;
    end
    else
      break;

    if DestPiece = nil then
      Exit; // defrag is not required
  end;

  ReallocPiece(DestPiece, MinPieceSize);
  DestPos := DestPiece.Used;

  SourcePiece := DestPiece.Next;
  SourcePos := 0;

  while true do begin
    if DestPiece.Size - DestPos >= SourcePiece.Used - SourcePos then
      WriteCount := SourcePiece.Used - SourcePos
    else
      WriteCount := DestPiece.Size - DestPos;

    Move(PtrOffset(SourcePiece, sizeof(TPieceHeader) + SourcePos)^, PtrOffset(DestPiece, sizeof(TPieceHeader) + DestPos)^, WriteCount);
    DestPos := DestPos + WriteCount;
    SourcePos := SourcePos + WriteCount;

    if SourcePos >= SourcePiece.Used then begin
      SourcePiece := SourcePiece.Next;
      while (SourcePiece <> nil) and (SourcePiece.Used = 0) do
        SourcePiece := SourcePiece.Next;
      if SourcePiece = nil then begin
        DestPiece.Used := DestPos;
        DestPiece := DestPiece.Next;
        break;
      end;
      SourcePos := 0;
    end;

    if DestPos >= DestPiece.Size then begin
      DestPiece.Used := DestPiece.Size;
      DestPiece := DestPiece.Next;
      Assert(DestPiece <> nil);
      if DestPiece.Size < MinPieceSize then
        if SourcePiece = DestPiece then begin
          ReallocPiece(DestPiece, MinPieceSize);
          SourcePiece := DestPiece;
        end
        else
          ReallocPiece(DestPiece, MinPieceSize);
      DestPos := 0;
    end;
  end;

  while DestPiece <> nil do begin
    SourcePiece := DestPiece;
    DestPiece := DestPiece.Next;
    FreePiece(SourcePiece);
  end;
end;

procedure TCRBlobData.CopyTo(Dest: TCRBlobData); // used to fill rollback
var
  Piece: PPieceHeader;
  CSize: Integer;
begin
  Dest.Truncate(0);
  Dest.FIsNull := FIsNull;
  if FFirstPiece <> nil then begin
    CSize := GetSize;
    AllocPiece(Piece, CSize);
    Piece.Used := CSize;
    Read(0, CSize, PtrOffset(Piece, sizeof(TPieceHeader)));

    Dest.FFirstPiece := FFirstPiece;
    FFirstPiece := nil;
    AppendPiece(Piece);
  end;
end;

procedure TCRBlobData.AddCRString;
var
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  LastPiece: PPieceHeader;
  FirstPiece: PPieceHeader;
  TempPiece: PPieceHeader;

  Source: IntPtr;
  SourceStart: IntPtr;
  Dest: IntPtr;
  DestEnd: IntPtr;
  SourceEnd: IntPtr;

  Shift: Integer;
  Used: Integer;
  w: Word;
  b: byte;
  c: byte;

  procedure AllocDestPiece;
  var
    AUsed, AUsed2: Integer;
  begin
    AUsed := Used + PtrSubstract(SourceStart, Source);
    if Dest <> nil then
      DestPiece.Used := DestPiece.Size - 1 + PtrSubstract(Dest, DestEnd);
    if AUsed < FPieceSize shr 1 then begin
      AUsed2 := AUsed * 2; //temporary for Update 7.1
      AllocPiece(DestPiece, AUsed2)
    end
    else
      AllocPiece(DestPiece, FPieceSize);
    Dest := PtrOffset(DestPiece, SizeOf(TPieceHeader));
    DestEnd := PtrOffset(Dest, DestPiece.Size - 1);
    DestPiece.Blob := Self.GetHashCode;
    DestPiece.Prev := LastPiece;
    if LastPiece <> nil then
      LastPiece.Next := DestPiece;
    LastPiece := DestPiece;
    if FirstPiece = nil then
      FirstPiece := DestPiece;
  end;

begin
  if (FFirstPiece = nil) then
    Exit;

  SourcePiece := FFirstPiece;
  FirstPiece := nil;
  LastPiece := nil;
  DestPiece := nil;
  Dest := nil;
  DestEnd := nil;
  Shift := 0;
  Used := GetSize;

  while SourcePiece <> nil do begin
    if SourcePiece.Used > Shift then begin
      SourceStart := PtrOffset(SourcePiece, SizeOf(TPieceHeader) + Shift);
      Source := SourceStart;
      SourceEnd := PtrOffset(Source, SourcePiece.Used - 1 - Shift);

      while PtrCompare(Source, SourceEnd) < 0 do begin
        if PtrCompare(Dest, DestEnd) >= 0 then
          AllocDestPiece;
        w := Marshal.ReadInt16(Source);
        if w = CRLF then begin
          Marshal.WriteInt16(Dest, w);
          Source := PtrOffset(Source, 2);
          Dest := PtrOffset(Dest, 2);
        end
        else begin
          b := Byte(w);
          if b = LF then begin
            Marshal.WriteInt16(Dest, CRLF);
            Source := PtrOffset(Source, 1);
            Dest := PtrOffset(Dest, 2);
          end
          else begin
            Marshal.WriteByte(Dest, b);
            Source := PtrOffset(Source, 1);
            Dest := PtrOffset(Dest, 1);
          end;
        end;
      end;

      if Source = SourceEnd then begin
        c := Marshal.ReadByte(Source);
        if PtrCompare(Dest, DestEnd) >= 0 then
          AllocDestPiece;
        Shift := Ord(
          (
           (c = 13) and
           (
            (
             (SourcePiece.Next <> nil) and
             (Marshal.ReadByte(SourcePiece.Next, SizeOf(TPieceHeader)) = 10)
            )
            or
            (SourcePiece.Next = nil)
            )
           )
          or
          (c = 10)
        );
        if (Shift = 1) then begin
          Marshal.WriteInt16(Dest, CRLF);
          Dest := PtrOffset(Dest, 2);
        end
        else begin
          Marshal.WriteByte(Dest, c);
          Dest := PtrOffset(Dest, 1);
        end;
      end
      else
        Shift := 0;
    end;
    Dec(Used, SourcePiece.Used);
    TempPiece := SourcePiece;
    SourcePiece := SourcePiece.Next;
    Marshal.FreeHGlobal(TempPiece);
  end;
  if Dest <> nil then
    DestPiece.Used := DestPiece.Size - 1 + PtrSubstract(Dest, DestEnd);
  FFirstPiece := FirstPiece;
end;

procedure TCRBlobData.RemoveCRString;
var
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  LastPiece: PPieceHeader;
  FirstPiece: PPieceHeader;
  TempPiece: PPieceHeader;

  SourceStart: IntPtr;
  Source: IntPtr;
  Dest: IntPtr;
  DestEnd: IntPtr;
  SourceEnd: IntPtr;

  Shift: Integer;
  Used: Integer;
  w: Word;
  c: byte;

  procedure AllocDestPiece;
  var
    AUsed: Integer;
  begin
    AUsed := Used + PtrSubstract(SourceStart, Source);
    if Dest <> nil then
      DestPiece.Used := DestPiece.Size + PtrSubstract(Dest, DestEnd);
    if AUsed < FPieceSize then
      AllocPiece(DestPiece, AUsed)
    else
      AllocPiece(DestPiece, FPieceSize);
    Dest := PtrOffset(DestPiece, SizeOf(TPieceHeader));
    DestEnd := PtrOffset(Dest, DestPiece.Size);
    DestPiece.Blob := Self.GetHashCode;
    DestPiece.Prev := LastPiece;
    if LastPiece <> nil then
      LastPiece.Next := DestPiece;
    LastPiece := DestPiece;
    if FirstPiece = nil then
      FirstPiece := DestPiece;
  end;

begin
  if FFirstPiece = nil then
    Exit;

  SourcePiece := FFirstPiece;
  FirstPiece := nil;
  LastPiece := nil;
  DestPiece := nil;
  Dest := nil;
  DestEnd := nil;
  Shift := 0;
  Used := GetSize;

  while SourcePiece <> nil do begin
    if SourcePiece.Used > Shift then begin
      SourceStart := PtrOffset(SourcePiece, SizeOf(TPieceHeader) + Shift);
      Source := SourceStart;
      SourceEnd := PtrOffset(Source, SourcePiece.Used - 1 - Shift);

      while PtrCompare(Source, SourceEnd) < 0 do begin
        if PtrCompare(Dest, DestEnd) >= 0 then
          AllocDestPiece;
        w := Marshal.ReadInt16(Source);
        if w = CRLF then begin
          Marshal.WriteByte(Dest, LF);
          Source := PtrOffset(Source, 2);
          Dest := PtrOffset(Dest, 1);
        end
        else
        begin
          Marshal.WriteByte(Dest, Byte(w));
          Source := PtrOffset(Source, 1);
          Dest := PtrOffset(Dest, 1);
        end;
      end;

      if Source = SourceEnd then begin
        c := Marshal.ReadByte(Source);
        if PtrCompare(Dest, DestEnd) >= 0 then
          AllocDestPiece;
        Shift := Ord((c = 13) and
                (SourcePiece.Next <> nil) and
                (Marshal.ReadByte(SourcePiece.Next, SizeOf(TPieceHeader)) = 10));
        if Shift = 1 then
          c := 10;
        Marshal.WriteByte(Dest, c);
        Dest := PtrOffset(Dest, 1);
      end
      else
        Shift := 0;
    end;
    Dec(Used, SourcePiece.Used);
    TempPiece := SourcePiece;
    SourcePiece := SourcePiece.Next;
    Marshal.FreeHGlobal(TempPiece);
  end;
  if Dest <> nil then
    DestPiece.Used := DestPiece.Size + PtrSubstract(Dest, DestEnd);
  FFirstPiece := FirstPiece;
end;

procedure TCRBlobData.AddCRUnicode;
var
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  LastPiece: PPieceHeader;
  FirstPiece: PPieceHeader;
  TempPiece: PPieceHeader;

  Source: IntPtr;
  SourceStart: IntPtr;
  Dest: IntPtr;
  DestEnd: IntPtr;
  SourceEnd: IntPtr;

  Shift: Integer; //bytes
  Used: Integer; //bytes
  w: Cardinal;
  b: Word;
  c: Word;

  procedure AllocDestPiece;
  var
    AUsed, AUsed2: Integer;
  begin
    AUsed := Used + PtrSubstract(SourceStart, Source);
    if Dest <> nil then
      DestPiece.Used := DestPiece.Size - sizeof(WideChar) + PtrSubstract(Dest, DestEnd);
    if AUsed < FPieceSize shr 1 then begin
      AUsed2 := AUsed * 2; //temporary for Update 7.1
      AllocPiece(DestPiece, AUsed2)
    end
    else
      AllocPiece(DestPiece, FPieceSize);
    Dest := PtrOffset(DestPiece, SizeOf(TPieceHeader));
    DestEnd := PtrOffset(Dest, DestPiece.Size - sizeof(WideChar));
    DestPiece.Blob := Self.GetHashCode;
    DestPiece.Prev := LastPiece;
    if LastPiece <> nil then
      LastPiece.Next := DestPiece;
    LastPiece := DestPiece;
    if FirstPiece = nil then
      FirstPiece := DestPiece;
  end;

begin
  if FFirstPiece = nil then
    Exit;

  SourcePiece := FFirstPiece;
  FirstPiece := nil;
  LastPiece := nil;
  DestPiece := nil;
  Dest := nil;
  DestEnd := nil;
  Shift := 0;
  Used := GetSize;

  while SourcePiece <> nil do begin
    if SourcePiece.Used > Shift then begin
      SourceStart := PtrOffset(SourcePiece, SizeOf(TPieceHeader) + Shift);
      Source := SourceStart;
      SourceEnd := PtrOffset(Source, SourcePiece.Used - sizeof(WideChar) - Shift);

      while PtrCompare(Source, SourceEnd) < 0 do begin
        if PtrCompare(Dest, DestEnd) >= 0 then
          AllocDestPiece;
        w := Marshal.ReadInt32(Source);
        if w = CRLF_UTF16 then begin
          Marshal.WriteInt32(Dest, w);
          Source := PtrOffset(Source, 4);
          Dest := PtrOffset(Dest, 4);
        end
        else begin
          b := Word(w);
          if b = LF_UTF16 then begin
            Marshal.WriteInt32(Dest, CRLF_UTF16);
            Source := PtrOffset(Source, 2);
            Dest := PtrOffset(Dest, 4);
          end
          else begin
            Marshal.WriteInt16(Dest, b);
            Source := PtrOffset(Source, 2);
            Dest := PtrOffset(Dest, 2);
          end;
        end;
      end;

      if Source = SourceEnd then begin
        c := Marshal.ReadInt16(Source);
        if PtrCompare(Dest, DestEnd) >= 0 then
          AllocDestPiece;
        Shift := Ord(
          (c = 13) and
          (SourcePiece.Next <> nil) and
          (Marshal.ReadInt16(SourcePiece.Next, SizeOf(TPieceHeader)) = 10)) * sizeof(WideChar);
        if Shift = sizeof(WideChar) then begin
          Marshal.WriteInt32(Dest, CRLF_UTF16);
          Dest := PtrOffset(Dest, 4);
        end
        else begin
          Marshal.WriteInt16(Dest, c);
          Dest := PtrOffset(Dest, 2);
        end;
      end
      else
        Shift := 0;
    end;
    Dec(Used, SourcePiece.Used);
    TempPiece := SourcePiece;
    SourcePiece := SourcePiece.Next;
    Marshal.FreeHGlobal(TempPiece);
  end;
  if Dest <> nil then
    DestPiece.Used := DestPiece.Size - sizeof(WideChar) + PtrSubstract(Dest, DestEnd);
  FFirstPiece := FirstPiece;
end;

procedure TCRBlobData.RemoveCRUnicode;
var
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  LastPiece: PPieceHeader;
  FirstPiece: PPieceHeader;
  TempPiece: PPieceHeader;

  SourceStart: IntPtr;
  Source: IntPtr;
  Dest: IntPtr;
  DestEnd: IntPtr;
  SourceEnd: IntPtr;

  Shift: Integer; //bytes
  Used: Integer;  //bytes
  w: Cardinal;
  c: Word;

  procedure AllocDestPiece;
  var
    AUsed: Integer;
  begin
    AUsed := Used + PtrSubstract(SourceStart, Source);
    if Dest <> nil then
      DestPiece.Used := DestPiece.Size + PtrSubstract(Dest, DestEnd);
    if AUsed < FPieceSize then
      AllocPiece(DestPiece, AUsed)
    else
      AllocPiece(DestPiece, FPieceSize);
    Dest := PtrOffset(DestPiece, SizeOf(TPieceHeader));
    DestEnd := PtrOffset(Dest, DestPiece.Size);
    DestPiece.Blob := Self.GetHashCode;
    DestPiece.Prev := LastPiece;
    if LastPiece <> nil then
      LastPiece.Next := DestPiece;
    LastPiece := DestPiece;
    if FirstPiece = nil then
      FirstPiece := DestPiece;
  end;

begin
  if FFirstPiece = nil then
    Exit;

  SourcePiece := FFirstPiece;
  FirstPiece := nil;
  LastPiece := nil;
  DestPiece := nil;
  Dest := nil;
  DestEnd := nil;
  Shift := 0;
  Used := GetSize;

  while SourcePiece <> nil do begin
    if SourcePiece.Used > Shift then begin
      SourceStart := PtrOffset(SourcePiece, SizeOf(TPieceHeader) + Shift);
      Source := SourceStart;
      SourceEnd := PtrOffset(Source, SourcePiece.Used - sizeof(WideChar) - Shift);

      while PtrCompare(Source, SourceEnd) < 0 do begin
        if PtrCompare(Dest, DestEnd) >= 0 then
          AllocDestPiece;
        w := marshal.ReadInt32(Source);
        if w = CRLF_UTF16 then begin
          Marshal.WriteInt16(Dest, LF_UTF16);
          Source := PtrOffset(Source, 4);
          Dest := PtrOffset(Dest, 2);
        end
        else
        begin
          Marshal.WriteInt16(Dest, Word(w));
          Source := PtrOffset(Source, 2);
          Dest := PtrOffset(Dest, 2);
        end;
      end;

      if Source = SourceEnd then begin
        c := Marshal.ReadInt16(Source);
        if PtrCompare(Dest, DestEnd) >= 0 then
          AllocDestPiece;
        Shift := Ord(
          (c = 13) and
          (SourcePiece.Next <> nil) and
          (Marshal.ReadInt16(SourcePiece.Next, SizeOf(TPieceHeader)) = 10)) * sizeof(WideChar);
        if Shift = sizeof(WideChar) then
          c := 10;
        Marshal.WriteInt16(Dest, c);
        Dest := PtrOffset(Dest, 2);
      end
      else
        Shift := 0;
    end;
    Dec(Used, SourcePiece.Used);
    TempPiece := SourcePiece;
    SourcePiece := SourcePiece.Next;
    Marshal.FreeHGlobal(TempPiece);
  end;
  if Dest <> nil then
    DestPiece.Used := DestPiece.Size + PtrSubstract(Dest, DestEnd);
  FFirstPiece := FirstPiece;
end;

function TCRBlobData.TranslatePositionToAnsi(Position: Cardinal): Cardinal; // Ansi to Unicode
var
  i: Integer;
  Piece: PPieceHeader;
  CurPosAnsi, CurPosUni: Cardinal;
  p: IntPtr;
  w: WideString;
  s: AnsiString;
begin
  if not SysLocale.FarEast or (Position = 0) then begin
    Result := Position * 2;
    Exit;
  end;

  CurPosAnsi := 0;
  CurPosUni := 0;
  Piece := FFirstPiece;
  while Piece <> nil do begin
    p := PtrOffset(Piece, Sizeof(TPieceHeader));
    w := Marshal.PtrToStringUni(p, Piece.Used shr 1);
    s := AnsiString(w);

    Inc(CurPosUni, Piece.Used);
    Inc(CurPosAnsi, LengthA(s));

    if CurPosAnsi = Position then begin
      Result := CurPosUni;
      Exit;
    end
    else if CurPosAnsi > Position then
      for i := (Piece.Used shr 1) - 1 downto 0 do begin
        w := Marshal.PtrToStringUni(PtrOffset(p, i * 2), 1);
        s := AnsiString(w);
        Dec(CurPosUni, 2);
        Dec(CurPosAnsi, LengthA(s));
        if CurPosAnsi = Position then begin
          Result := CurPosUni;
          Exit;
        end;
        if CurPosAnsi < Position then
          raise Exception.Create(SInvalidBlobPosition);
      end;

    Piece := Piece.Next;
  end;

  raise Exception.Create(SInvalidBlobPosition);
end;

function TCRBlobData.TranslatePositionToUni(Position: Cardinal): Cardinal; // Ansi to Unicode
var
  i: Integer;
  Piece: PPieceHeader;
  CurPosAnsi, CurPosUni: Cardinal;
  p: IntPtr;
  w: WideString;
  s: AnsiString;
begin
  if not SysLocale.FarEast or (Position = 0) then begin
    Result := Position shr 1;
    Exit;
  end;

  CurPosAnsi := 0;
  CurPosUni := 0;
  Piece := FFirstPiece;
  while Piece <> nil do begin
    p := PtrOffset(Piece, Sizeof(TPieceHeader));
    s := Marshal.PtrToStringAnsi(p, Piece.Used);
    w := WideString(s);

    Inc(CurPosUni, Length(w) * 2);
    Inc(CurPosAnsi, Piece.Used);

    if CurPosUni = Position then begin
      Result := CurPosAnsi;
      Exit;
    end
    else if CurPosUni > Position then
      for i := Length(w) downto 1 do begin
        s := AnsiString(w[i]);
        Dec(CurPosUni, 2);
        Dec(CurPosAnsi, LengthA(s));
        if CurPosUni = Position then begin
          Result := CurPosAnsi;
          Exit;
        end;
        if CurPosUni < Position then
          raise Exception.Create(SInvalidBlobPosition);
      end;

    Piece := Piece.Next;
  end;

  raise Exception.Create(SInvalidBlobPosition);
end;

function TCRBlobData.GetSizeAnsi: Cardinal;
var
  Piece: PPieceHeader;
  p: IntPtr;
  w: WideString;
  s: AnsiString;
begin
  if not SysLocale.FarEast and Encoding.Default.IsSingleByte then begin
    Result := GetSize shr 1;
    Exit;
  end;

  Result := 0;
  Piece := FFirstPiece;
  while Piece <> nil do begin
    p := PtrOffset(Piece, Sizeof(TPieceHeader));
    w := Marshal.PtrToStringUni(p, Piece.Used shr 1);
    s := AnsiString(w);
    Inc(Result, LengthA(s));
    Piece := Piece.Next;
  end;
end;

function TCRBlobData.GetSizeUni: Cardinal;
var
  Piece: PPieceHeader;
  p: IntPtr;
  w: WideString;
  s: AnsiString;
begin
  if not SysLocale.FarEast and Encoding.Default.IsSingleByte then begin
    Result := GetSize * 2;
    Exit;
  end;

  Result := 0;
  Piece := FFirstPiece;
  while Piece <> nil do begin
    p := PtrOffset(Piece, Sizeof(TPieceHeader));

    s := Marshal.PtrToStringAnsi(p, Piece.Used);
    w := WideString(s);
    Inc(Result, Length(w) * 2);
    Piece := Piece.Next;
  end;
end;

{ Pieces }

class procedure TCRBlobData.AllocPiece(out Piece: PPieceHeader; Size: Integer);
begin
  Assert(Size > 0);
  Piece := Marshal.AllocHGlobal(sizeof(TPieceHeader) + Size);
  Piece.Blob := 0;
  Piece.Size := Size;
  Piece.Used := 0;
  Piece.Prev := nil;
  Piece.Next := nil;
end;

procedure TCRBlobData.ReallocPiece(var Piece: PPieceHeader; Size: Integer);
var
  MemSize: Integer;
begin
  if Size = 0 then begin
    FreePiece(Piece);
    Piece := nil;
  end
  else if Size <> Piece.Size then begin
    MemSize := sizeof(TPieceHeader) + Size;
    Piece := Marshal.ReAllocHGlobal(Piece, MemSize);
    Piece.Size := Size;
    if Piece.Used > Size then
      Piece.Used := Size;
    if Piece.Blob <> 0 then begin
      if Piece.Prev <> nil then
        Piece.Prev.Next := Piece
      else
        FFirstPiece := Piece;

      if Piece.Next <> nil then
        Piece.Next.Prev := Piece;
    end;
  end;
end;

procedure TCRBlobData.FreePiece(Piece: PPieceHeader);
begin
  if Piece.Blob <> 0 then
    DeletePiece(Piece);

  Marshal.FreeHGlobal(Piece);
end;

procedure TCRBlobData.AppendPiece(Piece: PPieceHeader);
var
  Last: PPieceHeader;
begin
  Piece.Blob := Self.GetHashCode;
  Piece.Next := nil;
  if FFirstPiece = nil then begin
    Piece.Prev := nil;
    FFirstPiece := Piece;
  end
  else begin
    Last := FFirstPiece;
    while Last.Next <> nil do
      Last := Last.Next;
    Last.Next := Piece;
    Piece.Prev := Last;
  end;
end;

procedure TCRBlobData.DeletePiece(Piece: PPieceHeader);
begin
  Assert(Piece.Blob = Self.GetHashCode);

  if FFirstPiece = Piece then begin
    FFirstPiece := Piece.Next;
    if FFirstPiece <> nil then
      FFirstPiece.Prev := nil;
  end
  else
  begin
    Piece.Prev.Next := Piece.Next;
    if Piece.Next <> nil then
      Piece.Next.Prev := Piece.Prev;
  end;

  Piece.Blob := 0;
end;

procedure TCRBlobData.CompressPiece(var Piece: PPieceHeader);
begin
  if Piece.Used < Piece.Size then
    ReallocPiece(Piece, Piece.Used);
end;

{ TBlob }

constructor TBlob.Create(IsUnicode: boolean = False);
begin
  inherited Create;

  FIsUnicode := IsUnicode;
  Test := btSign;                    // DEBUG
  FData := CreateBlobData;
end;

destructor TBlob.Destroy;
begin
  CheckValid;   // DEBUG
  Test := 0;    // DEBUG

  FData.Free;
  FRollback.Free;

  inherited;
end;

function TBlob.Clone(FromRollback: boolean = False; CloneData: boolean = True): TBlob;
var
  OldRollback: boolean;
begin
  Result := CreateClone;

  if FromRollback then begin
    OldRollback := UseRollback;
    UseRollback := True;
    try
      Result.CloneBlobData(Self);
    finally
      UseRollback := OldRollback;
    end;
  end
  else
  if CloneData then
    Result.CloneBlobData(Self);
end;

procedure TBlob.FreeBlob;
begin
end;

function TBlob.CreateClone: TBlob;
begin
  Result := TBlob.Create(IsUnicode);
end;

function TBlob.CreateBlobData: TCRBlobData;
begin
  Result := TCRBlobData.Create;
end;

procedure TBlob.CloneBlobData(SourceBlob: TBlob);
begin
  SetData(SourceBlob.GetData);
end;

procedure TBlob.CheckValid;
begin
  if Test <> btSign then                    // DEBUG
    raise Exception.Create(SInvalidBlob);
end;

procedure TBlob.Clear;
begin
  // if we will perform SaveToRollback for already Empty Blob then
  // Blob will be marked as modified and we will get invalid update query
  if not IsNull then begin
    if FNeedRollback and (FRollback = nil) then
      SaveToRollback;

    FData.Clear;
  end
  // Blob can be Null, but FirstPiece <> nil
  else if FData.FirstPiece <> nil then
    FData.Clear;
end;

{ Pieces }

class procedure TBlob.AllocPiece(out Piece: PPieceHeader; Size: Integer);
begin
  TCRBlobData.AllocPiece(Piece, Size);
end;

procedure TBlob.ReallocPiece(var Piece: PPieceHeader; Size: Integer);
begin
  FData.ReallocPiece(Piece, Size);
end;

procedure TBlob.FreePiece(Piece: PPieceHeader);
begin
  FData.FreePiece(Piece);
end;

procedure TBlob.AppendPiece(Piece: PPieceHeader);
begin
  FData.AppendPiece(Piece);
end;

procedure TBlob.DeletePiece(Piece: PPieceHeader);
begin
  FData.DeletePiece(Piece);
end;

procedure TBlob.CompressPiece(var Piece: PPieceHeader);
begin
  FData.CompressPiece(Piece);
end;

function TBlob.FirstPiece: PPieceHeader;
begin
  Result := FData.FFirstPiece;
end;

procedure TBlob.CheckValue;
begin
end;

function TBlob.Read(Position: Cardinal; Count: Cardinal; Dest: IntPtr): Cardinal;
begin
  CheckValid;   // DEBUG
  CheckValue;

  Result := FData.Read(Position, Count, Dest);
end;

procedure TBlob.Write(Position: Cardinal; Count: Cardinal; Source: IntPtr);
begin
  CheckValid;   // DEBUG

  if FNeedRollback and (FRollback = nil) then
    SaveToRollback;

  FData.Write(Position, Count, Source);
end;

procedure TBlob.Truncate(NewSize: Cardinal);
begin
  if FNeedRollback and (FRollback = nil) then
    SaveToRollback;

  FData.Truncate(NewSize);
end;

function TBlob.IsEmpty: boolean;
begin
  CheckValue;

  Result := FData.IsEmpty;
end;

function TBlob.IsNull: boolean;
begin
  CheckValue;

  Result := FData.IsNull;
end;

procedure TBlob.Compress;
begin
  FData.Compress;
end;

procedure TBlob.Defrag(MinPieceSize: Cardinal = 0);
begin
  // Move all data to first piece if MinPieceSize = 0
  FData.Defrag(MinPieceSize);
end;

{ Stream/File }

procedure TBlob.LoadFromStream(Stream: TStream);
var
  Remainder, Position, Size, BufLen: Integer;
  Buffer: TBytes;
  BufferPtr: IntPtr;
begin
  Clear;

  Size := Stream.Size;
  if Size = 0 then
    exit;

  Stream.Seek(LongInt(0), soFromBeginning);

  Position := 0;
  Remainder := Size;
  SetLength(Buffer, PieceSize);
  BufferPtr := Buffer;
  while Remainder > 0 do begin
    if Remainder > PieceSize then
      BufLen := PieceSize
    else
      BufLen := Remainder;

    Stream.Read(Buffer[0], BufLen);
    Write(Position, BufLen, BufferPtr);

    Inc(Position, BufLen);
    Dec(Remainder, BufLen);
  end;
end;

procedure TBlob.SaveToStream(Stream: TStream);
var
  BufLen, Size, Position, Remainder: Integer;
  Buffer: TBytes;
  BufferPtr: IntPtr;
begin
  Stream.Size := 0;

  Size := GetSize;
  if Size = 0 then
    exit;

  Position := 0;
  Remainder := Size;
  SetLength(Buffer, PieceSize);
  BufferPtr := Buffer;
  while Remainder > 0 do begin
    if Remainder > PieceSize then
      BufLen := PieceSize
    else
      BufLen := Remainder;

    Read(Position, BufLen, BufferPtr);
    Stream.Write(Buffer[0], BufLen);

    Inc(Position, BufLen);
    Dec(Remainder, BufLen);
  end;
end;

procedure TBlob.LoadFromFile(const FileName: string);
var
  Stream:TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBlob.SaveToFile(const FileName: string);
var
  Stream:TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBlob.Assign(Source: TBlob);
const
  BufSize = 65536;
var
  Buf: IntPtr;
  Pos: Cardinal;
  Size: Cardinal;
begin
  Clear;
  IsUnicode := Source.IsUnicode;

  Pos := 0;
  Buf := Marshal.AllocHGlobal(BufSize);
  try
    repeat
      Size := Source.Read(Pos, BufSize, Buf);
      if Size > 0 then begin
        Write(Pos, Size, Buf);
        Inc(Pos, Size);
      end;
    until Size = 0;
  finally
    Marshal.FreeHGlobal(Buf);
  end;
end;

function TBlob.GetData: TCRBlobData;
begin
  Result := FData;
end;

procedure TBlob.SetData(Value: TCRBlobData);
begin
  if Value <> FData then begin
    Assert(not UseRollback);
    Assert(Value <> nil);
    FData.Free;
    FData := Value;
    Value.AddRef;
  end;
end;

procedure TBlob.CheckCached;
begin
  if not FNeedRollback then
    raise Exception.Create(SBlobMustBeCached);
end;

procedure TBlob.SaveToRollback;
begin
  CheckCached;

  FRollback := CreateBlobData;
  //Rollback.FIsUnicode := FIsUnicode;
  //Rollback.FModified := FModified;
  FData.CopyTo(FRollBack);
end;

procedure TBlob.EnableRollback;
begin
  {if FNeedRollback then
    raise Exception.Create(SCachedAlreadyEnabled);}

  FNeedRollback := True;
end;

procedure TBlob.Commit;
begin
  //CheckCached;

  if FRollback <> nil then begin
    FRollback.Free;
    FRollback := nil;
  end;
end;

procedure TBlob.Cancel;
begin
  //CheckCached;

  if FRollback <> nil then begin
    FData.Free;
    FData := FRollback;
    FRollback := nil;
  end;
end;

function TBlob.CanRollback: boolean;
begin
  Result := FRollback <> nil;
end;

function TBlob.GetSize: Cardinal;
begin
  Result := FData.GetSize;
end;

procedure TBlob.SetSize(Value: Cardinal);
begin
  FData.SetSize(Value);
end;

procedure TBlob.SetIsUnicode(Value: boolean);
begin
  if Value = IsUnicode then
    Exit;

  if Size > 0 then
    DataError(SCannotChangeIsUnicode);

  FIsUnicode := Value;
end;

function TBlob.TranslatePositionToAnsi(Position: Cardinal): Cardinal; // Ansi to Unicode
begin
  Assert(FIsUnicode);
  Result := FData.TranslatePositionToAnsi(Position);
end;

function TBlob.TranslatePositionToUni(Position: Cardinal): Cardinal; // Unicode to Ansi
begin
  Assert(not FIsUnicode);
  Result := FData.TranslatePositionToUni(Position);
end;

function TBlob.GetSizeAnsi: Cardinal;
begin
  Assert(FIsUnicode);
  Result := FData.GetSizeAnsi;
end;

function TBlob.GetSizeUni: Cardinal;
begin
  Assert(not FIsUnicode);
  Result := FData.GetSizeUni;
end;

function TBlob.GetAsString: string;
begin
{$IFDEF IS_UNICODE}
  Result := AsWideString;
{$ELSE}
  Result := AsAnsiString;
{$ENDIF}
end;

procedure TBlob.SetAsString(const Value: string);
begin
{$IFDEF IS_UNICODE}
  AsWideString := Value;
{$ELSE}
  AsAnsiString := Value;
{$ENDIF}
end;

{$IFNDEF NEXTGEN}

function TBlob.GetAsAnsiString: AnsiString;
begin
  if FIsUnicode then
    Result := AnsiString(GetAsWideString())
  else begin
    SetLength(Result, Size);
    Read(0, 0, IntPtr(Result));
  end;
end;

procedure TBlob.SetAsAnsiString(const Value: AnsiString);
var
  Size: Cardinal;
begin
  if FIsUnicode then
    SetAsWideString(WideString(Value))
{$IFDEF IS_UNICODE}{$IFDEF IS_UTF8}
  else if StringCodePage(Value) <> 65001 then
    SetAsWideString(WideString(Value))
{$ENDIF}{$ENDIF}
  else begin
    Clear;
    Size := Length(Value);
    Write(0, Size, PAChar(Value));
  end;
end;

{$ENDIF}

function TBlob.GetAsWideString: WideString;
begin
  if FIsUnicode then begin
    SetLength(Result, (Size + 1) shr 1);
    Read(0, 0, IntPtr(Result));
  end
  else
  {$IFDEF IS_UNICODE}
    {$IFDEF IS_UTF8_INCLUDE_MEMO}
    Result := Encoding.UTF8.GetString(GetAsBytes);
    {$ELSE}
    Result := Encoding.ANSI.GetString(GetAsBytes);
    {$ENDIF}
  {$ELSE}
    Result := WideString(GetAsAnsiString);
  {$ENDIF}
end;

procedure TBlob.SetAsWideString(const Value: WideString);
var
  Size: Cardinal;
begin
  if FIsUnicode then begin
    Clear;
    Size := Length(Value) * 2;
    Write(0, Size, PWChar(Value));
  end
  else
  {$IFDEF IS_UTF8_INCLUDE_MEMO}
    SetAsBytes(Encoding.UTF8.GetBytes(Value));
  {$ELSE}
    SetAsBytes(Encoding.ANSI.GetBytes(Value));
  {$ENDIF}
end;

function TBlob.GetAsBytes: TBytes;
var
  DataSize: Cardinal;
begin
  DataSize := Size;
  SetLength(Result, DataSize);
  Read(0, DataSize, IntPtr(Result));
end;

procedure TBlob.SetAsBytes(const Value: TBytes);
var
  Size: Cardinal;
begin
  Clear;
  Size := Length(Value);
  Write(0, Size, IntPtr(Value));
end;

function TBlob.GetPieceSize: Integer;
begin
  Result := FData.FPieceSize;
end;

procedure TBlob.SetPieceSize(Value: Integer);
begin
  FData.FPieceSize := Value;
end;

function TBlob.GetLargePieceSize: Integer;
begin
  Result := FData.GetLargePieceSize;
end;

procedure TBlob.SetLargePieceSize(Value: Integer);
begin
  FData.FLargePieceSize := Value;
end;

function TBlob.GetUseRollback: boolean;
begin
  Result := FStoredData <> nil;
end;

procedure TBlob.SetUseRollback(Value: boolean);
begin
  if Value then begin
    if (FStoredData = nil) and (FRollback <> nil) then begin
      FStoredData := FData;
      FData := FRollback;
    end;
  end
  else begin
    if FStoredData <> nil then begin
      FData := FStoredData;
      FStoredData := nil;
    end;
  end;
end;

procedure TBlob.AddCR;
begin
  CheckValid;   // DEBUG
  CheckValue;

  if FNeedRollback and (FRollback = nil) then
    SaveToRollback;

  if FIsUnicode then
    FData.AddCRUnicode
  else
    FData.AddCRString;
end;

procedure TBlob.RemoveCR;
begin
  CheckValid;   // DEBUG
  CheckValue;

  if FNeedRollback and (FRollback = nil) then
    SaveToRollback;

  if FIsUnicode then
    FData.RemoveCRUnicode
  else
    FData.RemoveCRString;
end;

{$IFDEF HAVE_COMPRESS}

{ TCompressedBlobData }

function TCompressedBlobData.CompressFrom(source: IntPtr; const sourceLen: Integer): boolean;
var
  CPiece: PPieceHeader;
  CSize: integer;
begin
  // see my_compress_alloc
  // *complen=  *len * 120 / 100 + 12;
  CheckZLib;
  CSize := CCompressBlobHeaderSize{header} + sourceLen + (sourceLen div 5) + 12;
  AllocPiece(CPiece, CSize);
  try
    DoCompress(PtrOffset(CPiece, sizeof(TPieceHeader) + CCompressBlobHeaderSize), @CSize, source, sourceLen);
    CPiece.Used := CCompressBlobHeaderSize + CSize;
    Result := Integer(CPiece.Used) < sourceLen; // Compression is successful
  except
    Result := False;
  end;
  if not Result then begin
    FreePiece(CPiece);
    Exit;
  end;

  // WriteHeader
  CopyBuffer(@CCompressBlobHeaderGuid[0], PtrOffset(CPiece, sizeof(TPieceHeader)), CCompressBlobHeaderGuidSize);
  Marshal.WriteInt32(CPiece, sizeof(TPieceHeader) + CCompressBlobHeaderSize - SizeOf(Int32), sourceLen);

  CompressPiece(CPiece);
  if FFirstPiece <> nil then
    FreePiece(FFirstPiece);
  AppendPiece(CPiece);
end;

procedure TCompressedBlobData.UncompressTo(dest: IntPtr; var destlen: integer);
var
  source: IntPtr;
begin
  Assert(FFirstPiece <> nil);

  Defrag;
  source := PtrOffset(FFirstPiece, sizeof(TPieceHeader));

  Assert(FFirstPiece.Next = nil);

  // Check header
  if FFirstPiece.Used <= CCompressBlobHeaderSize then
    DataError(SInvalidComprBlobSize);
  if not CompareMem(source, @CCompressBlobHeaderGuid[0], CCompressBlobHeaderGuidSize) then
    DataError(SInvalidComprBlobHeader);

  CheckZLib;
  try
    DoUncompress(dest, @destlen, PtrOffset(source, CCompressBlobHeaderSize), FFirstPiece.Used - CCompressBlobHeaderSize);
  except
    DataError(SInvalidComprBlobData);
  end;
end;

function TCompressedBlobData.GetSize: Cardinal;
begin
  if IsCompressed then begin
    Result := UnCompressedSize;
    Assert(Result > 0);
  end
  else
    Result := inherited GetSize;
end;

procedure TCompressedBlobData.SetSize(Value: Cardinal);
begin
  if IsCompressed then
    Assert((Value = 0) or (Value = GetSize));

  inherited;
end;

function TCompressedBlobData.GetCompressedSize: Cardinal;
begin
  if not IsCompressed then
    DataError(sBlobNotCompressed);
  Result := inherited GetSize;
end;

function TCompressedBlobData.IsCompressed: boolean;
begin
  Result :=
    (FFirstPiece <> nil) and
    // (FFirstPiece.Next = nil) and - false, if blob copied from another blob
    (FFirstPiece.Used > CCompressBlobHeaderSize) and
    CompareMem(PtrOffset(FFirstPiece, sizeof(TPieceHeader)), @CCompressBlobHeaderGuid[0], CCompressBlobHeaderGuidSize);
end;

function TCompressedBlobData.SetCompressed(Value: boolean): boolean;
var
  CPiece: PPieceHeader;
  Count, CSize: integer;
begin
  if (FFirstPiece = nil) or (IsCompressed = Value) then begin
    Result := False;
    Exit;
  end;

  Result := True;
  if Value then begin
    { pack
    (b) small blob without compression (Size < MIN_COMPRESS_LENGTH).
    (c) big blobs without compression (ZIP, JPG etc).
    (d) big blobs with compression (TXT etc).
    }
    Count := GetSize;
    // (b)
    if Count <= MIN_COMPRESS_LENGTH then
      Exit;

    Defrag;
    CompressFrom(PtrOffset(FFirstPiece, sizeof(TPieceHeader)), Count);
  end
  else
  begin
    Assert(FFirstPiece <> nil, 'FFirstPiece = nil');

    CSize := UnCompressedSize;
    AllocPiece(CPiece, CSize);
    try
      UncompressTo(PtrOffset(CPiece, sizeof(TPieceHeader)), CSize);
      CPiece.Used := CSize;
      if CPiece.Used <> CPiece.Size then
        DataError(SInvalidUnComprBlobSize); // DatabaseError(SInvalidUnComprBlobSize);
    except
      FreePiece(CPiece);
      raise;
    end;

    FreePiece(FFirstPiece);
    AppendPiece(CPiece);
  end;
end;

function TCompressedBlobData.UnCompressedSize: Cardinal;
begin
  Assert(IsCompressed);
  Result := Marshal.ReadInt32(FFirstPiece, sizeof(TPieceHeader) + CCompressBlobHeaderSize - SizeOf(Integer));
end;

procedure TCompressedBlobData.Truncate(NewSize: Cardinal);
begin
  if NewSize <> 0 then
    SetCompressed(False);

  inherited;
end;

function TCompressedBlobData.Read(Position, Count: Cardinal; Dest: IntPtr): Cardinal;
var
  CSize: Integer;
  ReadAll: boolean;
  Size: Cardinal;
begin
  Size := GetSize;
  // partial read or read all blob?
  ReadAll := (Position = 0) and ((Count = Size) or (Count = 0));
  if not ReadAll then
    SetCompressed(False);

  if ReadAll and IsCompressed then begin
    if (FFirstPiece = nil) or (Position > Size) then begin
      Result := 0;
      Exit;
    end;

    if Count = 0 then
      Count := Size;
    //-----------

    CSize := Count;
    UncompressTo(Dest, CSize);
    Assert(Cardinal(CSize) = Count);
    Result := CSize;
  end
  else
    Result := inherited Read(Position, Count, Dest);
end;

procedure TCompressedBlobData.Write(Position, Count: Cardinal; Source: IntPtr);
var
  Size: Cardinal;
begin
  if IsCompressed then begin
    Size := GetSize;
    if (Position <> 0) or ((Count <> Size) and (Size <> 0)) {full rewrite} then begin
      SetCompressed(False);
      inherited;
      Exit;
    end;
    Clear;

    { pack
    (b) small blob without compression (Size < MIN_COMPRESS_LENGTH).
    (c) big blobs without compression (ZIP, JPG etc).
    (d) big blobs with compression (TXT etc).
    }

    // (b)
    if (Count <= MIN_COMPRESS_LENGTH) or not CompressFrom(Source, Count) then
      inherited;
  end
  else
    inherited;
end;

{ TCompressedBlob }

function TCompressedBlob.CreateBlobData: TCRBlobData;
begin
  Result := TCompressedBlobData.Create;
end;

function TCompressedBlob.GetCompressed: boolean;
begin
  Result := TCompressedBlobData(FData).IsCompressed;
end;

procedure TCompressedBlob.SetCompressed(Value: boolean);
begin
  CheckValid;   // DEBUG
  CheckValue;

  TCompressedBlobData(FData).SetCompressed(Value);
end;

function TCompressedBlob.GetCompressedSize: Cardinal;
begin
  Result := TCompressedBlobData(FData).GetCompressedSize;
end;
{$ENDIF}

{$IFDEF VER6}
{$IFDEF MSWINDOWS}
function LCIDToCodePage(ALcid: Cardinal): Integer;
const
  CP_ACP = 0;                                // system default code page
  LOCALE_IDEFAULTANSICODEPAGE = $00001004;   // default ansi code page
var
  ResultCode: Integer;
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Val(Buffer, Result, ResultCode);
  if ResultCode <> 0 then
    Result := CP_ACP;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF CRDEBUG}
procedure ShowWarningMessage(const MessageStr: string; Count: integer = 0);
var
  str: string;
begin
  if Count = 0 then
    str := MessageStr
  else
    str := IntToStr(Count) + MessageStr;
{$IFDEF CONSOLE}
  WriteLn(str);
{$ELSE}{$IFDEF MSWINDOWS}
  MessageBox(0, PChar(str), 'DA warning', MB_OK);
{$ENDIF}{$ENDIF}
end;

procedure ShowNotReleasedObjects;
var
  i: Integer;
  cnt: Integer;
  str: string;
  ClassType: TClass;
begin
  if ShareObjectList.Count > 0 then begin
  {$IFDEF CONSOLE}
    WriteLn;
  {$ENDIF}
    str := '--- Not released objects ---' + #13#10;
    while ShareObjectList.Count > 0 do begin
      ClassType := TObject(ShareObjectList[0]).ClassType;
      cnt := 0;
      for i := ShareObjectList.Count - 1 downto 0 do
        if TObject(ShareObjectList[i]).ClassType = ClassType then begin
          Inc(cnt);
          ShareObjectList.Delete(i);
        end;
      str := str + '  ' + ClassType.ClassName + ': ' + IntToStr(cnt) + #13#10;
    end;

    ShowWarningMessage(str);
  end;
end;
{$ENDIF}

initialization
  StartWaitProc := nil;
  StopWaitProc := nil;
  ApplicationTitleProc := nil;

  SizeOf_TStrData := BlockSize - SizeOf(IntPtr);
  SizeOf_TBlock := SizeOf_TStrData + SizeOf(IntPtr);

{$IFDEF VER6}
{$IFDEF MSWINDOWS}
  // Code from Delphi7 system.pas
  // High bit is set for Win95/98/ME
  if not IsWin9x then begin
    if {Lo(GetVersion) > 4} Win32MajorVersion > 4 then
      DefaultUserCodePage := 3  // Use CP_THREAD_ACP with Win2K/XP
    else
      // Use thread's current locale with NT4
      DefaultUserCodePage := LCIDToCodePage(GetThreadLocale);
  end
  else
    // Convert thread's current locale with Win95/98/ME
    DefaultUserCodePage := LCIDToCodePage(GetThreadLocale);
{$ENDIF}
{$ENDIF}

  BoolSymbolLexems := TLexemList.Create;
  BoolKeywordLexems := TLexemList.Create;

  BoolSymbolLexems.Add('=', lxEqual);
  BoolSymbolLexems.Add('>', lxMore);
  BoolSymbolLexems.Add('<', lxLess);
  BoolSymbolLexems.Add('>=', lxMoreEqual);
  BoolSymbolLexems.Add('<=', lxLessEqual);
  BoolSymbolLexems.Add('<>', lxNoEqual);
  BoolSymbolLexems.Add('(', lxLeftBracket);
  BoolSymbolLexems.Add(')', lxRightBracket);
  BoolSymbolLexems.Add('-', lxMinus);
  BoolSymbolLexems.Add('+', lxPlus);
  BoolSymbolLexems.Add('[', lxLeftSqBracket);
  BoolSymbolLexems.Add(']', lxRightSqBracket);
  BoolSymbolLexems.Sort;

  BoolKeywordLexems.Add('AND', lxAND);
  BoolKeywordLexems.Add('FALSE', lxFALSE);
  BoolKeywordLexems.Add('IS', lxIS);
  BoolKeywordLexems.Add('NOT', lxNOT);
  BoolKeywordLexems.Add('NULL', lxNULL);
  BoolKeywordLexems.Add('OR', lxOR);
  BoolKeywordLexems.Add('TRUE', lxTRUE);
  BoolKeywordLexems.Add('LIKE', lxLIKE);
  BoolKeywordLexems.Add('IN', lxIN);
  BoolKeywordLexems.Add('LOWER', lxLOWER);
  BoolKeywordLexems.Add('UPPER', lxUPPER);
  BoolKeywordLexems.Add('BETWEEN', lxBETWEEN);
  BoolKeywordLexems.Sort;

  RefreshIteration := 1;

{$IFDEF CRDEBUG}
  ShareObjectList := TList.Create;
{$ENDIF}

finalization
  BoolSymbolLexems.Free;
  BoolKeywordLexems.Free;

{$IFDEF CRDEBUG}
  {$IFDEF MSWINDOWS}
  if DataCnt <> 0 then
    ShowWarningMessage(' Data(s) hasn''t been released', DataCnt);
  if ShareObjectCnt <> 0 then
    ShowWarningMessage(' ShareObject(s) hasn''t been released', ShareObjectCnt);
  {$ELSE}{$IFNDEF CONSOLE}
  ShowNotReleasedObjects;
  {$ENDIF}{$ENDIF}
  ShareObjectList.Free;
{$ENDIF}

end.
