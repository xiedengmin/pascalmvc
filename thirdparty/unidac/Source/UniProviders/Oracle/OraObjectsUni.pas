
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Oracle Objects Support
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraObjectsUni;

interface

uses
  Classes, SysUtils, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Types, Variants,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  OraCall, OraClasses,
{$ELSE}
  OraCallUni, OraClassesUni,
{$ENDIF}
  CLRClasses, CRTypes, MemUtils, MemData, CRAccess;

type

{ TOraType }

  TTypeState = (tsUnknown, tsDisconnected, tsInit);

  TOraType = class (TObjectType)
  private
    FOCISvcCtx: TOCISvcCtx;
    FOCI8: TOCI8API;
    FState: TTypeState;

    FDataSize: word; // instance size
    FIndicatorSize: word;
    FTypeCode: integer;
    FFinal: boolean;
    FAlignNeeded: boolean;

    procedure SetOCISvcCtx(Value: TOCISvcCtx);
  protected
    hTDO: pOCIType;

    procedure ClearAttributes;
    procedure Describe(OCISvcCtx: TOCISvcCtx; const Name: string);
    procedure DescribeAttribute(Attribute: TAttribute; TypeCode: Integer; National: Boolean; Len, Prec, Scale: Integer; const FullTypeName: string; var IndicatorSize: word);
    procedure DescribeHeader(OCISvcCtx: TOCISvcCtx);
    procedure Validate(OCISvcCtx: TOCISvcCtx);

    property OCISvcCtx: TOCISvcCtx read FOCISvcCtx;
    property OCI8: TOCI8API read FOCI8;
  public
    constructor Create(OCISvcCtx: TOCISvcCtx; const Name: string);
    destructor Destroy; override;

    procedure Init(const Name: string);

    property TDO: pOCIType read hTDO;
    property DataSize: word read FDataSize;
    property IndicatorSize: word read FIndicatorSize;
    property TypeCode: integer read FTypeCode;
    property Final: boolean read FFinal;
  end;

  TObjectTypes = class (TThreadList)
  public
    function GetTypeName(OCISvcCtx: TOCISvcCtx; TDO: pOCIType): string;
    function FindType(OCISvcCtx: TOCISvcCtx; const Name: string): TOraType;
    procedure ClearTypes(OCISvcCtx: TOCISvcCtx; DisconnectMode: boolean);
  end;

{ TOraObject }

  TOraRef = class;
  TOraArray = class;

  TOraObject = class (TDBObject)
  private
    FObjectType: TOraType;
    FOCISvcCtx: TOCISvcCtx;
    FOCI8: TOCI8API;
    FNativeInstance: boolean;
    FObjects: TList;            //This list contains Objects within indexes corresponding to
                                //AttributeNO, so some List[i] could be nil
    FAutoCreate: boolean;
    FTrimFixedChar: boolean;
    FOwner: TOraObject;
    FOwnerIndex: Integer;

    procedure SetOCISvcCtx(Value: TOCISvcCtx);
    procedure SetObjectType(Value: TOraType); reintroduce;
    procedure SetInstance(Value: IntPtr);
    function GetFInstance: IntPtr;
    procedure SetFInstance(Value: IntPtr);
    function GetFIndicator: IntPtr;
    procedure SetFIndicator(Value: IntPtr);
    function GetObjectData(OwnerType, InstanceType: TOraType; Instance: IntPtr): IntPtr;

    function GetAsOCIDate(const Name: string): OCIDate;
    procedure SetAsOCIDate(const Name: string; Value: OCIDate);
    function ReadAsOCIDate(Attr: TAttribute; Data: IntPtr): OCIDate;
    procedure WriteAsOCIDate(Attr: TAttribute; Data: IntPtr; Value: OCIDate);
    function GetAsOCINumber(const Name: string): OCINumber;
    procedure SetAsOCINumber(const Name: string; const Value: OCINumber);
    function ReadAsOCINumber(Attr: TAttribute; Data: IntPtr): OCINumber;
    procedure WriteAsOCINumber(Attr: TAttribute; Data: IntPtr; const Value: OCINumber);
    function GetAsOCIString(const Name: string): pOCIString;
    procedure SetAsOCIString(const Name: string; Value: pOCIString);
    function ReadAsOCIString(Attr: TAttribute; Data: IntPtr): pOCIString;
    procedure WriteAsOCIString(Attr: TAttribute; Data: IntPtr; Value: pOCIString);

    function GetAsDateTime(const Name: string): TDateTime;
    procedure SetAsDateTime(const Name: string; Value: TDateTime);
    function ReadAsDateTime(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): TDateTime;
    procedure WriteAsDateTime(Attr: TAttribute; Data: IntPtr; Value: TDateTime);
    function GetAsFloat(const Name: string): double;
    procedure SetAsFloat(const Name: string; Value: double);
    function ReadAsFloat(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): double;
    procedure WriteAsFloat(Attr: TAttribute; Data: IntPtr; Value: double);
    function GetAsInteger(const Name: string): integer;
    procedure SetAsInteger(const Name: string; Value: integer);
    function ReadAsInteger(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): integer;
    procedure WriteAsInteger(Attr: TAttribute; Data: IntPtr; Value: integer);
    function GetAsLargeInt(const Name: string): int64;
    procedure SetAsLargeInt(const Name: string; Value: int64);
    function ReadAsLargeInt(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): int64;
    procedure WriteAsLargeInt(Attr: TAttribute; Data: IntPtr; Value: int64);

    function GetAsString(const Name: string): string;
    procedure SetAsString(const Name: string; const Value: string);
    function ReadAsString(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): string;
    procedure WriteAsString(Attr: TAttribute; Data: IntPtr; const Value: string);
    function GetAsAnsiString(const Name: string): AnsiString;
    procedure SetAsAnsiString(const Name: string; const Value: AnsiString);
    function ReadAsAnsiString(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): AnsiString;
    procedure WriteAsAnsiString(Attr: TAttribute; Data: IntPtr; const Value: AnsiString); overload;
    procedure WriteAsAnsiString(Attr: TAttribute; Data: IntPtr; ValuePtr: IntPtr; ValueLen: Word); overload;
    function GetAsWideString(const Name: string): WideString;
    procedure SetAsWideString(const Name: string; const Value: WideString);
    function ReadAsWideString(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): WideString;
    procedure WriteAsWideString(Attr: TAttribute; Data: IntPtr; const Value: WideString); overload;
    procedure WriteAsWideString(Attr: TAttribute; Data: IntPtr; ValuePtr: IntPtr; ValueLen: Word); overload;

    function GetAsObject(const Name: string): TOraObject;
    function ReadAsObject(AttrChain: TAttributeChain): TOraObject;
    function GetAsRef(const Name: string): TOraRef;
    function ReadAsRef(AttrChain: TAttributeChain): TOraRef;
    function GetAsArray(const Name: string): TOraArray;
    function ReadAsArray(AttrChain: TAttributeChain): TOraArray;
    function GetAsLob(const Name: string): TOraLob;
    function ReadAsLob(AttrChain: TAttributeChain): TOraLob;
    function GetAsOraTimeStamp(const Name: string): TOraTimeStamp;
    function ReadAsOraTimeStamp(AttrChain: TAttributeChain): TOraTimeStamp;
    function GetAsOraInterval(const Name: string): TOraInterval;
    function ReadAsOraInterval(AttrChain: TAttributeChain): TOraInterval;
  protected
    FInstancePtr: IntPtr;
    FIndicatorPtr: IntPtr;
    FCached: boolean;

    procedure Check(Status: sword);

    procedure CheckType; virtual;
    procedure CheckSession;
    procedure CheckAlloc(RaiseException: boolean = True);
    procedure CheckNotCached;
    procedure AllocNewObject;

    procedure FreeObjects;

    function GetAttribute(const Name: string; out Data: IntPtr; out Ind: OCIInd;
      out Indicator: IntPtr; AutoAlloc: boolean = False; MakeNotNull: boolean = False): TAttribute; overload;
    function GetAttribute(AttrChain: TAttributeChain; out Data: IntPtr; out Ind: OCIInd;
      out Indicator: IntPtr; AutoAlloc: boolean = False; MakeNotNull: boolean = False): TAttribute; overload;
    procedure GetAttribute(Attr: TAttribute; Index: Integer; out Data: IntPtr; out Ind: OCIInd;
      out Indicator: IntPtr; AutoAlloc: boolean = False; MakeNotNull: boolean = False); overload;

    procedure GetAttributeData(AttrChain: TAttributeChain; Attr: TAttribute; var IsNotNull: boolean;
      var Data: IntPtr; out Ind: OCIInd; var Indicator: IntPtr; AutoAlloc: boolean; MakeNotNull: boolean);
    procedure GetAttributeItemData(Attr: TAttribute; Index: Integer; var Data: IntPtr; out Ind: OCIInd;
      var Indicator: IntPtr; AutoAlloc: boolean; MakeNotNull: boolean);
    procedure SetAttributeData(Attr: TAttribute; Data: IntPtr; Ind: OCIInd);
    procedure SetAttributeItemData(Attr: TAttribute; Index: Integer; Data: IntPtr; Ind: OCIInd);

    function GetAttrIsNull(const Name: string): boolean; override;
    procedure SetAttrIsNull(const Name: string; Value: boolean);
    function ReadAttrIsNull(Attr: TAttribute; Ind: OCIInd): boolean;
    procedure WriteAttrIsNull(Attr: TAttribute; IndPtr: IntPtr; Value: Boolean);

    procedure GetAttributeValue(const Name: string; out AttrBuf: IntPtr; out AttrSize: Word; out IsBlank, NativeBuffer: boolean); overload; override;
    procedure GetAttributeValue(AttrChain: TAttributeChain; out AttrBuf: IntPtr; out AttrLen: Word; out IsBlank, NativeBuffer: boolean); reintroduce; overload;
    procedure SetAttributeValue(const Name: string; ValuePtr: IntPtr; ValueLen: Word); overload; override;
    procedure SetAttributeValue(AttrChain: TAttributeChain; ValuePtr: IntPtr; ValueLen: Word); reintroduce; overload;

    function GetComplexAttribute(const Name: string): TSharedObject; overload;
    function GetComplexAttribute(AttrChain: TAttributeChain): TSharedObject; overload;
    function FindComplexAttribute(AttrChain: TAttributeChain): TSharedObject;

    function GetAttributeObject(Index: Integer): TSharedObject;
    procedure SetAttributeObject(Index: Integer; Obj: TSharedObject);

    function GetIsNull: boolean; override;
    procedure SetIsNull(Value: boolean); virtual;
    function ReadIndicator: OCIInd;
    procedure WriteIndicator(Value: OCIInd);

    function GetIndicatorPtr: IntPtr; virtual;
    procedure AssignInstance(Value: IntPtr); virtual;

    function GetChildObject(Attr: TAttribute; ArrayIndex: integer = -1): TSharedObject;
    function ReadChildObject(Parent: TOraObject; Attr: TAttribute; ArrayIndex: Integer; Value: IntPtr; Ind: OCIInd; IndPtr: IntPtr): TSharedObject;
    function GetChildIndex(Child: TOraObject): Integer;
    function GetOwnerIndex: Integer;
    procedure ChildAlloc(Child: TOraObject); virtual;

    function GetOCIRef: pOCIRef; virtual;

    property FInstance: IntPtr read GetFInstance write SetFInstance;
    property FIndicator: IntPtr read GetFIndicator write SetFIndicator;
    property OCI8: TOCI8API read FOCI8;
  public
    constructor Create(AObjectType: TOraType = nil);
    destructor Destroy; override;

    procedure AllocObject; overload; virtual;
    procedure AllocObject(OCISvcCtx: TOCISvcCtx); overload;
    procedure AllocObject(const TypeName: string); overload; virtual;
    procedure AllocObject(OCISvcCtx: TOCISvcCtx; const TypeName: string); overload; virtual;

    procedure FreeObject(FreeChild: boolean = True); virtual;
    procedure Disconnect; override;

    procedure CreateObject(OCISvcCtx: TOCISvcCtx; const TypeName: string); deprecated;

    procedure CacheObject;

    procedure Lock;
    procedure Flush;
    procedure Refresh;
    procedure MarkDelete;
    procedure MarkUpdate;
    procedure Unmark;
    procedure WriteLobs;
    procedure ReadLobs;
    procedure ResetInstance(NewInstance, NewIndicator: IntPtr);

    function Exists: boolean;
    function IsLocked: boolean;
    function IsDirty: boolean;

    procedure Assign(Source: TOraObject); virtual;
    procedure ObjectAssign(DataSrc, DataDest: IntPtr; OType: TOraType);
    procedure NestTableAssign(Source, Dest: IntPtr; OType: TOraType);

    property AttrIsNull[const Name: string]: boolean read GetAttrIsNull write SetAttrIsNull;

    property AttrAsOCIDate[const Name: string]: OCIDate read GetAsOCIDate write SetAsOCIDate;
    property AttrAsOCINumber[const Name: string]: OCINumber read GetAsOCINumber write SetAsOCINumber;
    property AttrAsOCIString[const Name: string]: pOCIString read GetAsOCIString write SetAsOCIString;

    property AttrAsDateTime[const Name: string]: TDateTime read GetAsDateTime write SetAsDateTime;
    property AttrAsFloat[const Name: string]: double read GetAsFloat write SetAsFloat;
    property AttrAsInteger[const Name: string]: integer read GetAsInteger write SetAsInteger;
    property AttrAsLargeInt[const Name: string]: int64 read GetAsLargeInt write SetAsLargeInt;
    property AttrAsString[const Name: string]: string read GetAsString write SetAsString;
{$IFDEF NEXTGEN}
  protected
{$ENDIF}
    property AttrAsAnsiString[const Name: string]: AnsiString read GetAsAnsiString write SetAsAnsiString;
  public
    property AttrAsWideString[const Name: string]: WideString read GetAsWideString write SetAsWideString;
    property AttrAsObject[const Name: string]: TOraObject read GetAsObject;
    property AttrAsRef[const Name: string]: TOraRef read GetAsRef;
    property AttrAsArray[const Name: string]: TOraArray read GetAsArray;
    property AttrAsLob[const Name: string]: TOraLob read GetAsLob;
    property AttrAsTimeStamp[const Name: string]: TOraTimeStamp read GetAsOraTimeStamp;
    property AttrAsInterval[const Name: string]: TOraInterval read GetAsOraInterval;

    property ObjectType: TOraType read FObjectType write SetObjectType;
    property OCISvcCtx: TOCISvcCtx read FOCISvcCtx write SetOCISvcCtx;
    property Instance: IntPtr read GetFInstance write SetInstance;
    property Indicator: IntPtr read GetFIndicator write SetFIndicator;
    property InstancePtr: IntPtr read FInstancePtr;
    property IndicatorPtr: IntPtr read GetIndicatorPtr;
    property IsNull: boolean read GetIsNull write SetIsNull;
    property AutoCreate: boolean read FAutoCreate write FAutoCreate;
    property NativeInstance: boolean read FNativeInstance write FNativeInstance;
  end;

{ TOraRef }

  TOraRef = class (TOraObject)
  private
    InvalidObject: boolean;

    function GethOCIRef: pOCIRef;
    procedure SethOCIRef(Value: pOCIRef);
    procedure SetOCIRef(Value: pOCIRef);
    function GetOCIRefPtr: ppOCIRef;

  protected
    FpOCIRef: ppOCIRef;

    function GetIsNull: boolean; override;
    procedure SetIsNull(Value: boolean); override;

    function GetAsHex: string;

    property FOCIRef: pOCIRef read GethOCIRef write SethOCIRef;
  public
    constructor Create(ObjectType: TOraType);
    destructor Destroy; override;

    procedure Pin;
    procedure Unpin;

    function RefIsNull: boolean;
    procedure Clear;

    procedure Assign(Source: TOraObject); override;

    property OCIRef: pOCIRef read GethOCIRef write SetOCIRef;
    property OCIRefPtr: ppOCIRef read GetOCIRefPtr;
    property AsHex: string read GetAsHex;
  end;

{ TOraXML }

  TOraXML = class (TOraObject)
  private
    phOCIDescriptor: pOCIDescriptor;
    CacheValue: TBytes;
    LocalIndicator: IntPtr;// for 10102
    LocalIndicatorPtr: IntPtr;

    procedure CreateXMLStream;
    procedure FreeXMLStream;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsWideString: WideString;
    procedure SetAsWideString(const Value: WideString);
    function ObjectIsNull: boolean;
    procedure StartRead;
    function Read(Count: cardinal; Dest: IntPtr): cardinal;

    function AccessBySQL: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure WriteXMLFromStringBySQL(Buf: IntPtr; BufSize: Integer);
    procedure WriteXMLFromClobBySQL(OraLob: TOraLob);
  protected
    procedure CheckType; override;
    function GetIsNull: boolean; override;
    procedure SetIsNull(Value: boolean); override;
    function GetIndicatorPtr: IntPtr; override;
    procedure AssignInstance(Value: IntPtr); override;
    procedure ReadLocalIndicator;
  public
    constructor Create(AOCISvcCtx: TOCISvcCtx; AObjectType: TOraType = nil);
    destructor Destroy; override;

    procedure AllocObject; overload; override;
    procedure AllocObject(const TypeName: string); overload; override;
    procedure AllocObject(AOCISvcCtx: TOCISvcCtx; const TypeName: string); overload; override;
    procedure AllocObject(AOCISvcCtx: TOCISvcCtx; AOraLob: TOraLob); overload;

    procedure FreeObject(FreeChild: boolean = True); override;

    procedure Assign(Source: TOraObject); override;

    procedure ReadXML;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure Extract(RetDoc: TOraXML; XPathExpr: string; NSmap: string = '');
    function Exists(const XPathExpr: string; const NSmap: string = ''): boolean;
    procedure Transform(XSLDoc: TOraXML; RetDoc: TOraXML);
    procedure GetSchema(SchemaDoc: TOraXML; var SchemaURL: string; var RootElem: string);
    function Validate(const SchemaURL: string): boolean;
    function IsSchemaBased: boolean;

    property AsString: string read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end;

{ TOraAnyData }

  TOraAnyData = class (TOraObject)
  private
    FValueAttribute: TAttribute;
    FValueTypeCode: OCITypeCode;
    FValueData: IntPtr;
    FValueObject: TSharedObject;
    FValueIndicator: IntPtr;
    FValueIndicatorPtr: IntPtr;
    hValueTDO: pOCIType;

    function GetValueType: Word;
    procedure SetValueType(Value: Word);

    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetAsFloat: double;
    procedure SetAsFloat(Value: double);
    function GetAsInteger: integer;
    procedure SetAsInteger(Value: integer);
    function GetAsLargeInt: int64;
    procedure SetAsLargeInt(Value: int64);

    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsWideString: WideString;
    procedure SetAsWideString(const Value: WideString);

    function GetAsObject: TOraObject;
    function GetAsRef: TOraRef;
    function GetAsArray: TOraArray;
    function GetAsLob: TOraLob;
    function GetAsOraTimeStamp: TOraTimeStamp;
    function GetAsOraInterval: TOraInterval;

    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
  protected
    procedure CheckType; override;
    function GetIsNull: boolean; override;
    procedure SetIsNull(Value: boolean); override;
    function GetIndicatorPtr: IntPtr; override;

    procedure ClearValue;

    function GetTypeCode(DataType: Word): OCITypeCode;
    procedure ReadAttribute;

    procedure AllocAttribute(DataType: Word; out TypeCode: OCITypeCode); overload;
    procedure AllocAttribute(DataType: Word; Len: Integer; out TypeCode: OCITypeCode); overload;
    procedure AllocAttribute(DataType: Word; Prec, Scale: Integer; out TypeCode: OCITypeCode); overload;
    procedure AllocAttribute(DataType: Word; const FullTypeName: string; out TypeCode: OCITypeCode); overload;
    procedure AllocAttribute(DataType: Word; Len, Prec, Scale: Integer; const FullTypeName: string; out TypeCode: OCITypeCode); overload;

    procedure WriteData(Attribute: TAttribute; TypeCode: OCITypeCode; Data: IntPtr); overload;
    procedure WriteData(Attribute: TAttribute; TypeCode: OCITypeCode; TDO: pOCIType; Data: IntPtr; IndPtr: IntPtr); overload;
  public
    constructor Create(AOCISvcCtx: TOCISvcCtx; AObjectType: TOraType = nil);
    destructor Destroy; override;

    procedure AllocObject; override;
    procedure Assign(Source: TOraObject); override;
    procedure WriteAnyData;

    property ValueType: Word read GetValueType write SetValueType;

    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsLargeInt: int64 read GetAsLargeInt write SetAsLargeInt;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;

    property AsObject: TOraObject read GetAsObject;
    property AsRef: TOraRef read GetAsRef;
    property AsArray: TOraArray read GetAsArray;
    property AsLob: TOraLob read GetAsLob;
    property AsTimeStamp: TOraTimeStamp read GetAsOraTimeStamp;
    property AsInterval: TOraInterval read GetAsOraInterval;

    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

{ TOraArray }

  TOraArray = class (TOraObject)
  private
    procedure SetSize(Value: integer);
    function GetMaxSize: integer;
    function GetItemType: word;
    function GetItemSubType: word;

    function GetItemExists(Index: integer): boolean;
    function GetItemIsNull(Index: integer): boolean;
    procedure SetItemIsNull(Index: integer; Value: boolean);

    function GetItemAsOCIString(Index: integer): pOCIString;
    procedure SetItemAsOCIString(Index: integer; Value: pOCIString);

    function GetItemAsDateTime(Index: integer): TDateTime;
    procedure SetItemAsDateTime(Index: integer; Value: TDateTime);
    function GetItemAsFloat(Index: integer): double;
    procedure SetItemAsFloat(Index: integer; Value: double);
    function GetItemAsInteger(Index: integer): integer;
    procedure SetItemAsInteger(Index: integer; Value: integer);
    function GetItemAsLargeInt(Index: integer): int64;
    procedure SetItemAsLargeInt(Index: integer; Value: int64);

    function GetItemAsString(Index: integer): string;
    procedure SetItemAsString(Index: integer; const Value: string);
    function GetItemAsAnsiString(Index: integer): AnsiString;
    procedure SetItemAsAnsiString(Index: integer; const Value: AnsiString);
    function GetItemAsWideString(Index: integer): WideString;
    procedure SetItemAsWideString(Index: integer; const Value: WideString);

    function GetItemAsObject(Index: integer): TOraObject;
    procedure SetItemAsObject(Index: integer; Value: TOraObject);
    function GetItemAsRef(Index: integer): TOraRef;
    procedure SetItemAsRef(Index: integer; Value: TOraRef);
    function GetItemAsLob(Index: integer): TOraLob;
    procedure SetItemAsLob(Index: integer; Value: TOraLob);

    function GetItemAsOraTimeStamp(Index: integer): TOraTimeStamp;
    procedure SetItemAsOraTimeStamp(Index: integer; Value: TOraTimeStamp);
    function GetItemAsOraInterval(Index: integer): TOraInterval;
    procedure SetItemAsOraInterval(Index: integer; Value: TOraInterval);

  protected
    procedure CheckType; override;

    procedure CheckIndex(Index: integer);

    procedure ChildAlloc(Child: TOraObject); override;

    function GetSize: integer; virtual;

  public
    procedure Clear;
    function AppendItem: integer;
    procedure InsertItem(Index: integer);

    procedure Assign(Source: TOraObject); override;

    property Size: integer read GetSize write SetSize;
    property MaxSize: integer read GetMaxSize;
    property ItemType: word read GetItemType;
    property ItemSubType: word read GetItemSubType;

    property ItemExists[Index: integer]: boolean read GetItemExists;
    property ItemIsNull[Index: integer]: boolean read GetItemIsNull write SetItemIsNull;

    property ItemAsOCIString[Index: integer]: pOCIString read GetItemAsOCIString write SetItemAsOCIString;

    property ItemAsDateTime[Index: integer]: TDateTime read GetItemAsDateTime write SetItemAsDateTime;
    property ItemAsFloat[Index: integer]: double read GetItemAsFloat write SetItemAsFloat;
    property ItemAsInteger[Index: integer]: integer read GetItemAsInteger write SetItemAsInteger;
    property ItemAsLargeInt[Index: integer]: int64 read GetItemAsLargeInt write SetItemAsLargeInt;
    property ItemAsString[Index: integer]: string read GetItemAsString write SetItemAsString;
    property ItemAsAnsiString[Index: integer]: AnsiString read GetItemAsAnsiString write SetItemAsAnsiString;
    property ItemAsWideString[Index: integer]: WideString read GetItemAsWideString write SetItemAsWideString;
    property ItemAsObject[Index: integer]: TOraObject read GetItemAsObject write SetItemAsObject;
    property ItemAsRef[Index: integer]: TOraRef read GetItemAsRef write SetItemAsRef;
    property ItemAsLob[Index: integer]: TOraLob read GetItemAsLob write SetItemAsLob;
    property ItemAsTimeStamp[Index: integer]: TOraTimeStamp read GetItemAsOraTimeStamp write SetItemAsOraTimeStamp;
    property ItemAsInterval[Index: integer]: TOraInterval read GetItemAsOraInterval write SetItemAsOraInterval;
  end;

{ TOraNestTable }

  TOraNestTable = class (TOraArray)
  private

  protected
    procedure CheckType; override;

    function GetSize: integer; override;

  public
    procedure Assign(Source: TOraObject); override;
    procedure DeleteItem(Index: integer);

    property Size: integer read GetSize;
  end;

{ TRefData }

  TRefData = class (TData)
  private
    FRef: TOraRef;
    FIncludeObjectField: boolean;

    procedure SetRef(Value: TOraRef);

  protected
    procedure InternalPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override; // PrepareData;

  { Fields }
    procedure CreateFieldDescs; override;

  { Edit }

  public
    constructor Create;
    destructor Destroy; override;

    procedure Reopen; override;

  { Records }
    procedure GetRecord(RecBuf: IntPtr); override;
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure GetPriorRecord(RecBuf: IntPtr); override;
    procedure PutRecord(RecBuf: IntPtr); override;

    procedure AppendRecord(RecBuf: IntPtr); override;
    procedure InsertRecord(RecBuf: IntPtr); override;
    procedure UpdateRecord(RecBuf: IntPtr); override;
    procedure DeleteRecord; override;

    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc); override;

  { Bookmarks }
    procedure GetBookmark(Bookmark: PRecBookmark); override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;

    property Ref: TOraRef read FRef write SetRef;
    property IncludeObjectField: boolean read FIncludeObjectField write FIncludeObjectField;
  end;

{ TTableData }

  TTableData = class (TMemData)
  private
    FTable: TOraNestTable;
    FIndexOfs: word;
    FIncludeObjectField: boolean;

    procedure SetTable(Value: TOraNestTable);

  protected
    LastIndex: integer;

    procedure Check(Status: sword);

    procedure InternalPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override; // PrepareData;

  { Fields }
    procedure CreateFieldDescs; override;
    //procedure GetObjectFields(RecBuf: IntPtr; Obj: TDBObject; var FieldNo: word);
    //procedure PutObjectFields(RecBuf: IntPtr; Obj: TDBObject; var FieldNo: word);

    function GetIndicatorSize: Integer; override;
  { Edit }
    procedure InternalAppend(RecBuf: IntPtr); override;
    procedure InternalDelete; override;
    procedure InternalUpdate(RecBuf: IntPtr); override;

    function GetRecordCount: Integer; override;

  public
    constructor Create;
    destructor Destroy; override;

    function Fetch(FetchBack: boolean = False): boolean; override;
    procedure Reopen; override;

  { Fields }
    procedure InitFields; override;
    class function IsBlobDataType(DataType: word): boolean; override;

  { Records }
    procedure GetRecord(RecBuf: IntPtr); override;
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure PutRecord(RecBuf: IntPtr); override;

    procedure CancelRecord(RecBuf: IntPtr); override;

    procedure FetchComplexFields(RecBuf: IntPtr; WithBlob: boolean);
    procedure FetchComplexField(RecBuf: IntPtr; Field: TFieldDesc);
    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc); override;

    property Table: TOraNestTable read FTable write SetTable;
    property IncludeObjectField: boolean read FIncludeObjectField write FIncludeObjectField;
  end;

  function GetObjectCacheMaxSize(OCISvcCtx: TOCISvcCtx): integer;
  procedure SetObjectCacheMaxSize(OCISvcCtx: TOCISvcCtx; Value: integer);
  function GetObjectCacheOptSize(OCISvcCtx: TOCISvcCtx): integer;
  procedure SetObjectCacheOptSize(OCISvcCtx: TOCISvcCtx; Value: integer);

var
  ObjectTypes: TObjectTypes;

implementation

uses
  CRFunctions, DAConsts,
{$IFNDEF UNIDACPRO}
  OraConsts, OraError;
{$ELSE}
  OraConstsUni, OraErrorUni;
{$ENDIF}

const
  SInvalidAttrName = 'Invalid attribute name';
  SCannotConvert = 'Cannot convert type to ';

var
  NotNullInd: IntPtr;
  NullInd: IntPtr;
  NullIndStruct: IntPtr;
  NullOCINumber: IntPtr;
  NullOCIDate: IntPtr;
  NullOCIString: IntPtr;

{ TOraType }

constructor TOraType.Create(OCISvcCtx: TOCISvcCtx; const Name: string);
begin
  inherited Create;

  SetOCISvcCtx(OCISvcCtx);

  Init(Name);
end;

destructor TOraType.Destroy;
begin
  ObjectTypes.Remove(Self);

  inherited;
end;

procedure TOraType.SetOCISvcCtx(Value: TOCISvcCtx);
begin
  if FOCISvcCtx <> Value then begin
    FOCISvcCtx := Value;
    if FOCISvcCtx <> nil then
      FOCI8 := FOCISvcCtx.OCI8
    else
      FOCI8 := nil;
  end;
end;

procedure TOraType.ClearAttributes;
begin
  inherited ClearAttributes;
end;

procedure TOraType.Describe(OCISvcCtx: TOCISvcCtx; const Name: string);

  procedure Check(Status: sword);
  begin
    if Status <> OCI_SUCCESS then
      OCI8.DoOraError(Status, OCISvcCtx);
  end;

  function CreateAttr(hAttr: pOCIParam; AllignIsRequired: boolean): boolean;
  var
    i: Integer;
    Attribute: TAttribute;
    Ptr, StrPtr: IntPtr;
    ValueInt: Integer;
    National: Boolean;
    DataSize: ub2;
    Len: integer;
    Scale: sb1;
    Prec: ub1;
    TypeCode: OCITypeCode;
    TypeName: string;
    SchemaName: string;
    Res: sword;
    IndicatorSize: word;
  begin
    Result := False;

    Attribute := TAttribute.Create;
    Attribute.Owner := Self;
    Attribute.Length := 0;
    Attribute.Scale := 0;
    Attribute.AttributeNo := FAttributes.Count + 1;

    FAttributes.Add(Attribute);

    Ptr := Marshal.AllocHGlobal(sizeof(integer));
    try
      StrPtr := nil;
      Res := OCI8.OCIAttrGet1(hAttr, OCI_DTYPE_PARAM, @StrPtr, Ptr, OCI_ATTR_NAME, OCISvcCtx.hOCIError);
      if (Res = OCI_SUCCESS) and (StrPtr <> nil) then begin
        Len := Marshal.ReadInt32(Ptr);
        if OCISvcCtx.Home.OCIVersion < 8100 then
          Attribute.Name := PtrToStringOCI(StrPtr, OCISvcCtx.UnicodeEnv) // with 0 terminator and Len += 1
        else
          Attribute.Name := PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv);
      end
      else
        Attribute.Name := 'ELEMENT';
      if Attribute.Name = '' then
        Attribute.Name := 'ELEMENT';

      Check(OCI8.OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_TYPECODE, OCISvcCtx.hOCIError));
      TypeCode := sb2(ValueInt);

      Check(OCI8.OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_SIZE, OCISvcCtx.hOCIError));
      DataSize := Word(ValueInt);

      National := False;
      Prec := 0;
      Scale := 0;
      SchemaName := '';
      TypeName := '';

      case TypeCode of
        OCI_TYPECODE_CHAR,
        OCI_TYPECODE_NCHAR,
        OCI_TYPECODE_VARCHAR,
        OCI_TYPECODE_VARCHAR2,
        OCI_TYPECODE_NVARCHAR2: begin
          if OCISvcCtx.Home.OCIVersion >= 9000 then begin
            Check(OCI8.OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHAR_SIZE, OCISvcCtx.hOCIError));
            if ValueInt > 0 then
              DataSize := Word(ValueInt);

            Check(OCI8.OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, OCISvcCtx.hOCIError));
            National := ub1(ValueInt) = SQLCS_NCHAR;
          end;
        end;
        OCI_TYPECODE_NUMBER,
        OCI_TYPECODE_DECIMAL: begin
          Check(OCI8.OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PRECISION, OCISvcCtx.hOCIError));
          Prec := ub1(ValueInt);
          Check(OCI8.OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_SCALE, OCISvcCtx.hOCIError));
          Scale := sb1(ValueInt);
        end;
        OCI_TYPECODE_OBJECT,
        OCI_TYPECODE_OPAQUE,
        OCI_TYPECODE_REF,
        OCI_TYPECODE_VARRAY,
        OCI_TYPECODE_TABLE,
        OCI_TYPECODE_NAMEDCOLLECTION: begin
          StrPtr := nil;
          Check(OCI8.OCIAttrGet1(hAttr, OCI_DTYPE_PARAM, @StrPtr, Ptr, OCI_ATTR_SCHEMA_NAME, OCISvcCtx.hOCIError));
          Len := Marshal.ReadInt32(Ptr);
          if OCISvcCtx.Home.OCIVersion < 8100 then
            SchemaName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, OCISvcCtx.UnicodeEnv)) // with 0 terminator and Len += 1
          else
            SchemaName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

          StrPtr := nil;
          Check(OCI8.OCIAttrGet1(hAttr, OCI_DTYPE_PARAM, @StrPtr, Ptr, OCI_ATTR_TYPE_NAME, OCISvcCtx.hOCIError));
          Len := Marshal.ReadInt32(Ptr);
          if OCISvcCtx.Home.OCIVersion < 8100 then
            TypeName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, OCISvcCtx.UnicodeEnv)) // with 0 terminator and Len += 1
          else
            TypeName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));
        end;
        OCI_TYPECODE_CLOB,
        OCI_TYPECODE_NCLOB: begin
          if OCISvcCtx.Home.OCIVersion >= 9000 then begin
            Check(OCI8.OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, OCISvcCtx.hOCIError));
            National := ub1(ValueInt) = SQLCS_NCHAR;
          end;
        end;
      end;

      IndicatorSize := SizeOf(OCIInd);

      DescribeAttribute(Attribute, TypeCode, National, DataSize, Prec, Scale, SchemaName + '.' + TypeName, IndicatorSize);

      if not OCISvcCtx.Home.Direct then // do not align for Direct mode
        // dword alignment              // can't dword align dtDateTime, dtObject !!!
        if AllignIsRequired or
           (Attribute.DataType in [dtString, dtWideString,
                                   {dtDateTime, dtObject, dtAnyData, dtXML}
                                   dtReference, dtArray, dtTable,
                                   dtOraBlob, dtOraClob, dtWideOraClob, dtBFILE, dtCFILE,
                                   dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
                                   dtIntervalYM, dtIntervalDS]) or
            // BINARY_FLOAT & BINARY_DOUBLE
           ((Attribute.DataType = dtFloat) and (Attribute.SubDataType in [dtBFloat, dtBDouble]))
        then begin
          // BINARY_DOUBLE align at 8 bytes always
          if (Attribute.DataType = dtFloat) and (Attribute.SubDataType = dtBDouble) then
            FDataSize := (FDataSize + 7) and not 7
          // Align to size of Pointer
          else
            FDataSize := (FDataSize + (sizeof(IntPtr) - 1)) and not (sizeof(IntPtr) - 1);
          FAlignNeeded := True;
          Result := True;
        end;

      if not OCISvcCtx.Home.Direct and (Attribute.DataType = dtObject) then // do not align for Direct mode
        if not TOraType(Attribute.ObjectType).Final then begin
          FDataSize := (FDataSize + 3) and not 3;
          FAlignNeeded := True;
        end
        else if TOraType(Attribute.ObjectType).FAlignNeeded then begin
        {$IFDEF CPU64}
          FDataSize := (FDataSize + 7) and not 7;
        {$ELSE}
          FDataSize := (FDataSize + 3) and not 3;
        {$ENDIF}
          for i := 0 to Attribute.ObjectType.AttributeCount - 1 do
            // ObjectType has BINARY_FLOAT & BINARY_DOUBLE attributes
            if (Attribute.ObjectType.Attributes[i].DataType = dtFloat) and
               (Attribute.ObjectType.Attributes[i].SubDataType = dtBDouble)
            then begin
              FDataSize := (FDataSize + 7) and not 7;
              break;
            end;
          FAlignNeeded := True;
        end;

      Attribute.Offset := FDataSize;

      Inc(FDataSize, Attribute.DataSize);

      Attribute.IndicatorOffset := FIndicatorSize;
      Inc(FIndicatorSize, IndicatorSize)
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end;

  function GetSuperTypeAlignment(hParentParam: pOCIParam; Level: integer = 0): TIntArr;
  var
    Ptr, StrPtr: IntPtr;
    ValueInt: Integer;
    Len: integer;
    pName: IntPtr;
    AName: string;
    SuperTypeName: string;
    SuperTypeSchemaName: string;
    hDescribe: pOCIDescribe;
    hParam: pOCIParam;
  begin
    if OCISvcCtx.Home.OCIVersion < 9000 then
      exit;

    Ptr := Marshal.AllocHGlobal(sizeof(integer));
    try
      StrPtr := nil;
      Check(OCI8.OCIAttrGet1(hParentParam, OCI_DTYPE_PARAM, @StrPtr, Ptr, OCI_ATTR_SUPERTYPE_NAME, OCISvcCtx.hOCIError));
      Len := Marshal.ReadInt32(Ptr);
      SuperTypeName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

      if SuperTypeName = '' then begin
        SetLength(Result, Level);
        exit;
      end;

      StrPtr := nil;
      Check(OCI8.OCIAttrGet1(hParentParam, OCI_DTYPE_PARAM, @StrPtr, Ptr, OCI_ATTR_SUPERTYPE_SCHEMA_NAME, OCISvcCtx.hOCIError));
      Len := Marshal.ReadInt32(Ptr);
      SuperTypeSchemaName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

      AName := SuperTypeSchemaName + '.' + SuperTypeName;

      Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
      try
        pName := StringToHGlobalOCI(AName, Len, OCISvcCtx.UnicodeEnv);
        try
          Check(OCI8.OCIDescribeAny(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, pName, Len,
            OCI_OTYPE_NAME, 0, OCI_PTYPE_TYPE, hDescribe));
        finally
          FreeStringOCI(pName, OCISvcCtx.UnicodeEnv);
        end;

        Check(OCI8.OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, @hParam, nil, OCI_ATTR_PARAM, OCISvcCtx.hOCIError));

        Result := GetSuperTypeAlignment(hParam, Level + 1);

        Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_NUM_TYPE_ATTRS, OCISvcCtx.hOCIError));
        Result[Level] := sb2(ValueInt);
      finally
        OCI8.OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE);
      end;
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end;

var
  hDescribe: pOCIDescribe;
  hParam: pOCIParam;
  hAttrList: pOCIParam;
  hAttr: pOCIParam;
  hCollElement: pOCIParam;
  i, j: integer;
  Count: integer;
  Res, SSize, TSize: integer;
  TypeName: string;
  SchemaName: string;
  TypeCode: OCITypeCode;
  ValueInt: Integer;
  pName: IntPtr;
  pS, pT: IntPtr;
  AlignArray: TIntArr;
  AllignIsRequired: boolean;
  AllignIsAllowed: boolean;
  ObjInfo: TSQLObjectInfo;
begin
  if OCISvcCtx = nil then
    RaiseError(SSvcCtxNotDefined);

  SetOCISvcCtx(OCISvcCtx);

  ClearAttributes;
  FDataSize := 0;
  FIndicatorSize := SizeOf(OCIInd);

  FName := OCISQLInfo.NormalizeName(Name);
  OCISQLInfo.SplitObjectName(FName, ObjInfo);
  if ObjInfo.Schema <> '' then begin
    SchemaName := OCISQLInfo.UnQuote(ObjInfo.Schema);
    TypeName := OCISQLInfo.UnQuote(ObjInfo.Name);
  end
  else begin
    SchemaName := '';
    TypeName := OCISQLInfo.UnQuote(ObjInfo.Name);
  end;

  pS := StringToHGlobalOCI(SchemaName, SSize, OCISvcCtx.UnicodeEnv);
  try
    pT := StringToHGlobalOCI(TypeName, TSize, OCISvcCtx.UnicodeEnv);
    try
      Res := OCI8.OCITypeByName(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, OCISvcCtx.hOCISvcCtx, pS, SSize, pT, TSize,
                           nil, 0, OCI_DURATION_SESSION, OCI_TYPEGET_HEADER, hTDO);
    finally
      FreeStringOCI(pT, OCISvcCtx.UnicodeEnv);
    end;
  finally
    FreeStringOCI(pS, OCISvcCtx.UnicodeEnv);
  end;
  Check(Res);

  Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
  try
    pName := StringToHGlobalOCI(FName, SSize, OCISvcCtx.UnicodeEnv);
    try
      Check(OCI8.OCIDescribeAny(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, pName, SSize,
        OCI_OTYPE_NAME, 0, OCI_PTYPE_TYPE, hDescribe));
    finally
      FreeStringOCI(pName, OCISvcCtx.UnicodeEnv);
    end;

    Check(OCI8.OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, @hParam, nil, OCI_ATTR_PARAM, OCISvcCtx.hOCIError));

    Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_TYPECODE, OCISvcCtx.hOCIError));
    TypeCode := sb2(ValueInt);

    if OCISvcCtx.Home.OCIVersion >= 9000 then begin
      Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_IS_FINAL_TYPE, OCISvcCtx.hOCIError));
      FFinal := boolean(Byte(ValueInt));
    end
    else
      FFinal := True;

    // Get attribute numbers after that alignment is required
    AlignArray := GetSuperTypeAlignment(hParam);

    FTypeCode := TypeCode;
    if (TypeCode = OCI_TYPECODE_OBJECT) or (TypeCode = OCI_TYPECODE_OPAQUE) then begin
      FDataType := dtObject;
      if OCISvcCtx.Home.OCIVersion >= 9200 then
        if (FName = 'SYS.XMLTYPE') or (FName = 'PUBLIC.XMLTYPE') then
          FDataType := dtXML
        else if (FName = 'SYS.ANYDATA') or (FName = 'PUBLIC.ANYDATA') then
          FDataType := dtAnyData;

      FSize := 0;

      Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_NUM_TYPE_ATTRS, OCISvcCtx.hOCIError));
      Count := sb2(ValueInt);

      Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, @hAttrList, nil, OCI_ATTR_LIST_TYPE_ATTRS, OCISvcCtx.hOCIError));

      AllignIsAllowed := False;
      for i := 1 to Count do begin
        Check(OCI8.OCIParamGet(hAttrList, OCI_DTYPE_PARAM, OCISvcCtx.hOCIError, hAttr, i));

        try
          // check if alignment is required
          AllignIsRequired := false;
          for j := 0 to Length(AlignArray) - 1 do
            // alignment is required for first property for each inherited type
            if i = AlignArray[j] + 1 then
            begin
              AllignIsRequired := true;
              break;
            end;

          AllignIsAllowed := CreateAttr(hAttr, AllignIsRequired and AllignIsAllowed) or AllignIsAllowed;
        finally
          // free memory after OCIParamGet
          OCI8.OCIDescriptorFree(hAttr, OCI_DTYPE_PARAM);
        end;
      end;
    end
    else
      if TypeCode = OCI_TYPECODE_NAMEDCOLLECTION then begin
        Check(OCI8.OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_COLLECTION_TYPECODE, OCISvcCtx.hOCIError));
        TypeCode := sb2(ValueInt);

        case TypeCode of
          OCI_TYPECODE_VARRAY:
            FDataType := dtArray;
          OCI_TYPECODE_TABLE:
            FDataType := dtTable;
        else
          Assert(False);
        end;

        Check(OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, @hCollElement, nil, OCI_ATTR_COLLECTION_ELEMENT, OCISvcCtx.hOCIError));
        OCI8.Check(OCI8.OCIAttrGet2(hCollElement, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_NUM_ELEMENTS, OCISvcCtx.hOCIError), OCISvcCtx);
        FSize := Integer(ValueInt);

        CreateAttr(hCollElement, False);
      end
      else
        Assert(False);
  finally
    OCI8.OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE);
  end;

  FState := tsInit;
end;

procedure TOraType.DescribeAttribute(Attribute: TAttribute; TypeCode: Integer; National: Boolean; Len, Prec, Scale: Integer; const FullTypeName: string; var IndicatorSize: word);
begin
  case TypeCode of
    OCI_TYPECODE_CHAR,
    OCI_TYPECODE_NCHAR,
    OCI_TYPECODE_VARCHAR,
    OCI_TYPECODE_VARCHAR2,
    OCI_TYPECODE_NVARCHAR2: begin
      if (TypeCode = OCI_TYPECODE_CHAR) or (TypeCode = OCI_TYPECODE_NCHAR) then
        Attribute.Fixed := True
      else
        Attribute.Fixed := False;

      if OCISvcCtx.UnicodeEnv then begin
        Attribute.DataType := dtWideString;
        Attribute.Size := (Len + 1) * 2;
        if National or
           (TypeCode = OCI_TYPECODE_NCHAR) or
           (TypeCode = OCI_TYPECODE_NVARCHAR2)
        then
          Attribute.SubDataType := dtNWideString
        else
          Attribute.SubDataType := dtWideString;
      end
      else begin
        Attribute.DataType := dtString;
        Attribute.Size := Len + 1;  // for null terminator
        if National or
           (TypeCode = OCI_TYPECODE_NCHAR) or
           (TypeCode = OCI_TYPECODE_NVARCHAR2)
        then
          Attribute.SubDataType := dtNString
        else
          Attribute.SubDataType := dtString;
      end;

      Attribute.Length := Len;
      Attribute.DataSize := SizeOf(pOCIString);
    end;
    OCI_TYPECODE_SIGNED8: begin
      Attribute.DataType := dtInt8;
      Attribute.DataSize := 1;
      Attribute.Size := sizeof(ShortInt);
    end;
    OCI_TYPECODE_SIGNED16: begin
      Attribute.DataType := dtInt16;
      Attribute.DataSize := 2;
      Attribute.Size := sizeof(SmallInt);
    end;
    OCI_TYPECODE_SIGNED32: begin
      Attribute.DataType := dtInt32;
      Attribute.DataSize := 4;
      Attribute.Size := sizeof(Integer);
    end;
    OCI_TYPECODE_UNSIGNED8: begin
      Attribute.DataType := dtUInt8;
      Attribute.DataSize := 1;
      Attribute.Size := sizeof(Byte);
    end;
    OCI_TYPECODE_UNSIGNED16: begin
      Attribute.DataType := dtUInt16;
      Attribute.DataSize := 2;
      Attribute.Size := sizeof(Word);
    end;
    OCI_TYPECODE_UNSIGNED32: begin
      Attribute.DataType := dtUInt32;
      Attribute.DataSize := 4;
      Attribute.Size := sizeof(Cardinal);
    end;
    OCI_TYPECODE_INTEGER,
    OCI_TYPECODE_NUMBER,
    OCI_TYPECODE_DECIMAL: begin
      Attribute.Length := Prec;
      Attribute.Scale := Abs(Scale);

      if (Prec <= IntegerPrecision) and (Attribute.Scale = 0) then begin
        Attribute.DataType := dtInteger;
        Attribute.Size := sizeof(Integer);
      end
      else
      if (Prec <= LargeIntPrecision) and (Attribute.Scale = 0) then begin
        Attribute.DataType := dtLargeint;
        Attribute.Size := sizeof(Int64);
      end
      else begin
        Attribute.DataType := dtFloat;
        Attribute.Size := SizeOf(Double);
      end;

      Attribute.DataSize := OCI_NUMBER_SIZE;
    end;
    OCI_TYPECODE_DATE: begin
      Attribute.DataType := dtDateTime;
      Attribute.Size := SizeOf(TDateTime);
      Attribute.DataSize := (SizeOf(OCIDate) + 3) and not 3; // dword align
    end;
    OCI_TYPECODE_OBJECT,
    OCI_TYPECODE_OPAQUE,
    OCI_TYPECODE_REF,
    OCI_TYPECODE_NAMEDCOLLECTION,
    OCI_TYPECODE_VARRAY,
    OCI_TYPECODE_TABLE: begin
      Attribute.Size := SizeOf(IntPtr);

      if FullTypeName <> '' then begin
        if ObjectTypes <> nil then
          Attribute.ObjectType := ObjectTypes.FindType(OCISvcCtx, FullTypeName);
        if Attribute.ObjectType = nil then begin
          Attribute.ObjectType := TOraType.Create(OCISvcCtx, FullTypeName);
          Attribute.ObjectType.Release;
        end;
      end
      else
        Attribute.ObjectType := nil; // for ANYDATA only

      case TypeCode of
        OCI_TYPECODE_OBJECT:
          Attribute.DataType := dtObject;
        OCI_TYPECODE_REF:
          Attribute.DataType := dtReference;
        OCI_TYPECODE_OPAQUE,
        OCI_TYPECODE_VARRAY,
        OCI_TYPECODE_TABLE,
        OCI_TYPECODE_NAMEDCOLLECTION:
          if Attribute.ObjectType <> nil then
            Attribute.DataType := Attribute.ObjectType.DataType
          else
            Assert(False, SUnknownDataType)
        else
          Assert(False, SUnknownDataType);
      end;

      if (Attribute.DataType = dtObject) and
         (Attribute.ObjectType <> nil) and
         TOraType(Attribute.ObjectType).FFinal
      then begin
        Attribute.DataSize := TOraType(Attribute.ObjectType).DataSize;
        if not OCISvcCtx.Home.Direct then // do not align for Direct mode
          if TOraType(Attribute.ObjectType).FAlignNeeded then
            Attribute.DataSize := (Attribute.DataSize + 3) and not 3;
        IndicatorSize := TOraType(Attribute.ObjectType).IndicatorSize;
      end
      else begin
        Attribute.DataSize := SizeOf(IntPtr);
        IndicatorSize := SizeOf(OCIInd);
      end;
    end;
    OCI_TYPECODE_BLOB: begin
      Attribute.DataType := dtOraBlob;
      Attribute.Size := SizeOf(IntPtr);
      Attribute.DataSize := SizeOf(pOCILobLocator);
    end;
    OCI_TYPECODE_CLOB,
    OCI_TYPECODE_NCLOB: begin
      if OCISvcCtx.UseUnicode then begin
        Attribute.DataType := dtWideOraClob;
        if National or (TypeCode = OCI_TYPECODE_NCLOB) then
          Attribute.SubDataType := dtNClob
        else
          Attribute.SubDataType := dtWideOraClob;
      end
      else begin
        Attribute.DataType := dtOraClob;
        if National or (TypeCode = OCI_TYPECODE_NCLOB) then
          Attribute.SubDataType := dtNClob
        else
          Attribute.SubDataType := dtOraClob;
      end;
      Attribute.Size := SizeOf(IntPtr);
      Attribute.DataSize := SizeOf(pOCILobLocator);
    end;
    OCI_TYPECODE_BFILE: begin
      Attribute.DataType := dtBFILE;
      Attribute.Size := sizeof(IntPtr);
      Attribute.DataSize := SizeOf(pOCILobLocator);
    end;
    OCI_TYPECODE_CFILE: begin
      Attribute.DataType := dtCFILE;
      Attribute.Size := sizeof(IntPtr);
      Attribute.DataSize := SizeOf(pOCILobLocator);
    end;
    OCI_TYPECODE_RAW: begin  // WAR  RAW as STRING
      Attribute.DataType := dtString;
      Attribute.Size := Len * 2 + 1;  // for terminator and heximal represent
      Attribute.Length := Len * 2;
      Attribute.DataSize := SizeOf(pOCIString);
    end;
    OCI_TYPECODE_TIMESTAMP,
    OCI_TYPECODE_TIMESTAMP_TZ,
    OCI_TYPECODE_TIMESTAMP_LTZ,
    OCI_TYPECODE_INTERVAL_YM,
    OCI_TYPECODE_INTERVAL_DS:
    begin
      if OCISvcCtx.Home.OCIVersion < 9000 then
        RaiseError(SDataTypeNotSupported);
      case TypeCode of
        OCI_TYPECODE_TIMESTAMP:
          Attribute.DataType := dtTimeStamp;
        OCI_TYPECODE_TIMESTAMP_TZ:
          Attribute.DataType := dtTimeStampTZ;
        OCI_TYPECODE_TIMESTAMP_LTZ:
          Attribute.DataType := dtTimeStampLTZ;
        OCI_TYPECODE_INTERVAL_YM:
          Attribute.DataType := dtIntervalYM;
        OCI_TYPECODE_INTERVAL_DS:
          Attribute.DataType := dtIntervalDS;
        else
          Assert(False, SUnknownDataType);
      end;
      Attribute.Size := SizeOf(IntPtr);
      Attribute.DataSize := SizeOf(IntPtr);
    end;
    OCI_TYPECODE_FLOAT,
    OCI_TYPECODE_DOUBLE: begin
      Attribute.DataType := dtFloat;
      Attribute.Size := SizeOf(Double);
      Attribute.DataSize := OCI_NUMBER_SIZE;
    end;
    OCI_TYPECODE_BFLOAT: begin
      Attribute.DataType := dtFloat;
      Attribute.SubDataType := dtBFloat;
      Attribute.Size := SizeOf(Double);
      Attribute.DataSize := SizeOf(Single);
      //Attribute.Length := 38;
      //Attribute.Scale := 127;
    end;
    OCI_TYPECODE_BDOUBLE: begin
      Attribute.DataType := dtFloat;
      Attribute.SubDataType := dtBDouble;
      Attribute.Size := SizeOf(Double);
      Attribute.DataSize := SizeOf(Double);
      //Attribute.Length := 38;
      //Attribute.Scale := 127;
    end;
  else
    Assert(False, SUnknownDataType);
  end;
end;

procedure TOraType.DescribeHeader(OCISvcCtx: TOCISvcCtx);
var
  TypeName: string;
  SchemaName: string;
  i, Res, SSize, TSize: integer;
  pS, pT: IntPtr;
begin
  if OCISvcCtx = nil then
    RaiseError(SSvcCtxNotDefined);

  SetOCISvcCtx(OCISvcCtx);

  i := Pos('.', Name);
  if i > 0 then begin
    TypeName := OCISQLInfo.UnQuote(Copy(Name, i + 1, Length(Name)));
    SchemaName := OCISQLInfo.UnQuote(Copy(Name, 1, i - 1));
  end
  else begin
    TypeName := OCISQLInfo.UnQuote(Name);
    SchemaName := '';
  end;

  pS := StringToHGlobalOCI(SchemaName, SSize, OCISvcCtx.UnicodeEnv);
  pT := StringToHGlobalOCI(TypeName, TSize, OCISvcCtx.UnicodeEnv);
  Res := OCI8.OCITypeByName(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, OCISvcCtx.hOCISvcCtx, pS, SSize, pT, TSize,
    nil, 0, OCI_DURATION_SESSION, OCI_TYPEGET_HEADER, hTDO);
  FreeStringOCI(pS, OCISvcCtx.UnicodeEnv);
  FreeStringOCI(pT, OCISvcCtx.UnicodeEnv);
  OCI8.Check(Res, OCISvcCtx);

  FState := tsInit;
end;

procedure TOraType.Validate(OCISvcCtx: TOCISvcCtx);
begin
  case FState of
    tsUnknown:
      Describe(OCISvcCtx, Name);
    tsDisconnected:
      DescribeHeader(OCISvcCtx);
  end;
end;

procedure TOraType.Init(const Name: string);
begin
  if ObjectTypes = nil then
    ObjectTypes := TObjectTypes.Create;

  ObjectTypes.Add(Self);

  Describe(FOCISvcCtx, Name);
end;

{ TObjectTypes }

function TObjectTypes.GetTypeName(OCISvcCtx: TOCISvcCtx; TDO: pOCIType): string;

  procedure Check(Status: sword);
  begin
    if Status <> OCI_SUCCESS then
      OCISvcCtx.OCI8.DoOraError(Status, OCISvcCtx);
  end;

var
  hDescribe: pOCIDescribe;
  hParam: pOCIParam;
  StrPtr: IntPtr;
  TypeName: string;
  SchemaName: string;
  Len: Cardinal;
begin
  Check(OCISvcCtx.OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
  try
    Check(OCISvcCtx.OCI8.OCIDescribeAny(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError,
      TDO, 0 {SizeOf(IntPtr)}, OCI_OTYPE_PTR, 0, OCI_PTYPE_TYPE, hDescribe));

    Check(OCISvcCtx.OCI8.OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, @hParam, nil, OCI_ATTR_PARAM, OCISvcCtx.hOCIError));

    StrPtr := nil;
    Check(OCISvcCtx.OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, @StrPtr, @Len, OCI_ATTR_NAME, OCISvcCtx.hOCIError));
    if OCISvcCtx.Home.OCIVersion < 8100 then
      TypeName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, OCISvcCtx.UnicodeEnv)) // with 0 terminator and Len += 1
    else
      TypeName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

    StrPtr := nil;
    Check(OCISvcCtx.OCI8.OCIAttrGet1(hParam, OCI_DTYPE_PARAM, @StrPtr, @Len, OCI_ATTR_SCHEMA_NAME, OCISvcCtx.hOCIError));
    if OCISvcCtx.Home.OCIVersion < 8100 then
      SchemaName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, OCISvcCtx.UnicodeEnv)) // with 0 terminator and Len += 1
    else
      SchemaName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, OCISvcCtx.UnicodeEnv));

    Result := SchemaName + '.' + TypeName
  finally
    OCISvcCtx.OCI8.OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE);
  end;
end;

function TObjectTypes.FindType(OCISvcCtx: TOCISvcCtx; const Name: string): TOraType;
var
  i: integer;
  Str: string;
  List: TList;
begin
  List := LockList;
  try
    Str := OCISQLInfo.NormalizeName(Name);
    for i := 0 to List.Count - 1 do begin
      Result := TOraType(List.Items[i]);
      if (Result.FOCISvcCtx = OCISvcCtx) and (Result.Name = Str) then
        Exit;
    end;

    Result := nil;
  finally
    UnlockList;
  end;
end;

procedure TObjectTypes.ClearTypes(OCISvcCtx: TOCISvcCtx; DisconnectMode: boolean);
var
  i: integer;
  List: TList;
  oraType: TOraType;
begin
  List := LockList;
  try
    if DisconnectMode then begin
      for i := 0 to List.Count - 1 do begin
        oraType := TOraType(List.Items[i]);
        if oraType.FOCISvcCtx = OCISvcCtx then begin
          oraType.FState := tsDisconnected;
          oraType.FOCISvcCtx := nil;
          oraType.hTDO := nil;
        end;
      end;
    end
    else begin
      // temporary AddRef to types
      for i := 0 to List.Count - 1 do begin
        oraType := TOraType(List.Items[i]);
        if oraType.FOCISvcCtx = OCISvcCtx then begin
          oraType.FState := tsUnknown;
          oraType.AddRef;
        end;
      end;

      // free circular links
      for i := 0 to List.Count - 1 do begin
        oraType := TOraType(List.Items[i]);
        if oraType.FOCISvcCtx = OCISvcCtx then
          oraType.ClearAttributes;
      end;

      // release Refs
      for i := List.Count - 1 downto 0 do begin
        oraType := TOraType(List.Items[i]);
        if oraType.FOCISvcCtx = OCISvcCtx then begin
          if oraType.RefCount > 1 then begin
            oraType.Release;
            oraType.FOCISvcCtx := nil;
            oraType.hTDO := nil;
          end
          else
            oraType.Release;
        end;
      end;
    end;
  finally
    UnlockList;
  end;
end;

{ TOraObject }

constructor TOraObject.Create(AObjectType: TOraType);
begin
  inherited Create;

  FOwnerIndex := -2;
  if AObjectType <> nil then
    SetOCISvcCtx(AObjectType.OCISvcCtx);
  FInstancePtr := Marshal.AllocHGlobal(Sizeof(IntPtr));
  Marshal.WriteIntPtr(FInstancePtr, nil);
  FIndicatorPtr := Marshal.AllocHGlobal(Sizeof(IntPtr));
  Marshal.WriteIntPtr(FIndicatorPtr, nil);
  SetObjectType(AObjectType);
  FAutoCreate := True;
  FTrimFixedChar := True;
end;

destructor TOraObject.Destroy;
begin
  FreeObject;
  FreeObjects;

  if FObjectType <> nil then
    FObjectType.Release;

  Marshal.FreeHGlobal(FInstancePtr);
  Marshal.FreeHGlobal(FIndicatorPtr);

  inherited;
end;

procedure TOraObject.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    OCI8.DoOraError(Status, OCISvcCtx);
end;

procedure TOraObject.CheckType;
var
  NewObjectType: TOraType;
begin
  if FObjectType = nil then
    RaiseError(SNoObjectType);

  if FObjectType.FOCISvcCtx = nil then begin
    CheckSession;
    NewObjectType := ObjectTypes.FindType(FOCISvcCtx, FObjectType.Name);
    if NewObjectType <> nil then begin
      FObjectType.Release;
      FObjectType := NewObjectType;
      FObjectType.AddRef;
    end
    else
      FObjectType.Validate(FOCISvcCtx);
  end;
end;

procedure TOraObject.CheckSession;
begin
  if FOCISvcCtx = nil then
    RaiseError(SNoOCISvcCtx);
end;

procedure TOraObject.CheckAlloc(RaiseException: boolean);
begin
  if (FInstance = nil) or (FIndicator = nil) then begin
    if FAutoCreate then
      AllocObject;
    if RaiseException and (FInstance = nil) then
      RaiseError(SObjectNotAllocated);
  end;
end;

procedure TOraObject.CheckNotCached;
begin
  if FCached then
    RaiseError(SObjectIsCached);
end;

procedure TOraObject.AllocNewObject;
var
  TypeCode: OCITypeCode;
  Instance: IntPtr;
begin
  if FObjectType.DataType = dtObject then
    TypeCode := FObjectType.FTypeCode // OCI_TYPECODE_OBJECT
  else if FObjectType.DataType in [dtXML, dtAnyData] then
    TypeCode := OCI_TYPECODE_OPAQUE
  else if FObjectType.DataType = dtReference then
    TypeCode := OCI_TYPECODE_REF
  else if FObjectType.DataType in [dtArray, dtTable] then
    TypeCode := OCI_TYPECODE_NAMEDCOLLECTION
  else
    TypeCode := 0;

  Check(OCI8.OCIObjectNew(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, OCISvcCtx.hOCISvcCtx, TypeCode, FObjectType.hTDO, nil,
    OCI_DURATION_SESSION, 1, Instance)); //If we need to pin Object then we should set Value to 0

  FInstance := Instance;
  FNativeInstance := True;
end;

procedure TOraObject.AllocObject;
var
  NewObject: boolean;
begin
  CheckType;
  CheckSession;

  if FOwner = nil then begin
    if FInstance = nil then begin
      AllocNewObject;
      NewObject := True;
    end
    else
      NewObject := False;
    if (FIndicator = nil) and (FInstance <> nil) then
      Check(OCI8.OCIObjectGetInd(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, FIndicatorPtr));
    // We cannot get NULL indicator on fetch for collections
    // So collection is NOT NULL until user explicitly set IsNull := True
    if (FObjectType.DataType = dtObject) and NewObject then
      Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
  end
  else
    FOwner.ChildAlloc(Self);
end;

procedure TOraObject.AllocObject(const TypeName: string);
var
  OraType: TOraType;
begin
  CheckSession;

  if TypeName <> '' then begin
    if ObjectTypes <> nil then
      OraType := ObjectTypes.FindType(OCISvcCtx, TypeName)
    else
      OraType := nil;

    if OraType <> nil then
      SetObjectType(OraType)
    else begin
      OraType := TOraType.Create(OCISvcCtx, TypeName);
      SetObjectType(OraType);
      OraType.Release;
    end;
  end;

  AllocObject;
end;

procedure TOraObject.AllocObject(OCISvcCtx: TOCISvcCtx; const TypeName: string);
begin
  SetOCISvcCtx(OCISvcCtx);

  AllocObject(TypeName);
end;

procedure TOraObject.AllocObject(OCISvcCtx: TOCISvcCtx);
begin
  SetOCISvcCtx(OCISvcCtx);

  AllocObject;
end;

procedure TOraObject.FreeObject(FreeChild: boolean = True);
begin
  if FNativeInstance and (FInstance <> nil) then begin
    if not FCached then
      if FObjectType.FState = tsInit then
        // Check() doen't need here because exception will prevent executing subsequent code
        if (FOwner = nil) or (FObjectType.FTypeCode <> OCI_TYPECODE_OBJECT) then
          OCI8.OCIObjectFree(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, OCI_OBJECTFREE_FORCE);
    FInstance := nil;
    FIndicator := nil;
    FNativeInstance := False;
  end
  else begin
    FInstance := nil;
    FIndicator := nil;
  end;

  if FreeChild then
    FreeObjects;
end;

procedure TOraObject.Disconnect;
begin
  FreeObject;
  SetOCISvcCtx(nil);
end;

procedure TOraObject.FreeObjects;
begin
  if FObjects <> nil then begin
    while FObjects.Count > 0 do begin
      TSharedObject(FObjects[0]).Free;
      FObjects.Delete(0);
    end;
    FObjects.Free;
    FObjects := nil;
  end;
end;

procedure TOraObject.CreateObject(OCISvcCtx: TOCISvcCtx; const TypeName: string);
begin
  AllocObject(OCISvcCtx, TypeName);
end;

procedure TOraObject.CacheObject;
begin
  FCached := True; //we should check this prop to deny OCI function execution in this object
end;

procedure TOraObject.Assign(Source: TOraObject);
var
  InstanceSrc: IntPtr;
  InstanceDest: IntPtr;
begin
  CheckSession;
  CheckAlloc;

  if Source.Instance <> nil then begin
    Check(OCI8.OCIObjectCopy(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, OCISvcCtx.hOCISvcCtx, Source.Instance, Source.Indicator,
      Instance, Indicator, FObjectType.hTDO, OCI_DURATION_SESSION, 0));
    if OCISvcCtx.Home.OCIVersion >= 9000 then begin
      if Source.FOwner = nil then
        InstanceSrc := GetObjectData(nil, Source.ObjectType, Source.Instance)
      else
        InstanceSrc := GetObjectData(Source.FOwner.ObjectType, Source.ObjectType, Source.Instance);
      if FOwner = nil then
        InstanceDest := GetObjectData(nil, ObjectType, Instance)
      else
        InstanceDest := GetObjectData(FOwner.ObjectType, ObjectType, Instance);

      ObjectAssign(InstanceSrc, InstanceDest, ObjectType);
    end;
  end;
end;

procedure TOraObject.ObjectAssign(DataSrc, DataDest: IntPtr; OType: TOraType);
var
  j: integer;
  Attr: TAttribute;
begin
  if (DataSrc = nil) or (DataDest = nil) then
    Exit;
  for j := 0 to OType.AttributeCount - 1 do begin
    Attr := OType.Attributes[j];
    if Attr.ObjectType <> nil then
      case Attr.DataType of
        dtObject:
          if TOraType(Attr.ObjectType).FFinal then
            ObjectAssign(PtrOffset(DataSrc, Attr.Offset), PtrOffset(DataDest, Attr.Offset),
              TOraType(Attr.ObjectType))
          else begin
            ObjectAssign(Marshal.ReadIntPtr(DataSrc, Attr.Offset),
              Marshal.ReadIntPtr(DataDest, Attr.Offset), TOraType(Attr.ObjectType));
          end;
        dtTable:
          NestTableAssign(Marshal.ReadIntPtr(DataSrc, Attr.Offset),
              Marshal.ReadIntPtr(DataDest, Attr.Offset), TOraType(Attr.ObjectType))
      end;
  end;
end;

procedure TOraObject.NestTableAssign(Source, Dest: IntPtr; OType: TOraType);
var
  i: integer;
  Exists: tbool;
  destelem, elem, elemind: IntPtr;
  ASize: integer;
  Attr: TAttribute;
begin
  if (Source = nil) or (Dest = nil) or not (TAttribute(OType.Attributes[0]).DataType in [dtObject, dtArray, dtTable]) then
    Exit;

  Check(OCI8.OCITableSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Source, ASize));
  for i := 1 to ASize do begin
    Exists := 1;
    Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Source, i - 1, Exists, elem, elemind));
    if Exists <> 1 then
      Exit;

    Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, i - 1, elem, elemind, Dest));

    Exists := 1;
    Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Dest, i - 1, Exists, destelem, elemind));
    if Exists <> 1 then
      Exit;

    Attr := OType.Attributes[0];
    if Attr.DataType = dtObject then begin
      elem := GetObjectData(OType, TOraType(Attr.ObjectType), elem);
      ObjectAssign(PtrOffset(elem, Attr.Offset), PtrOffset(destelem, Attr.Offset), TOraType(Attr.ObjectType));
    end;
  end;
end;

function TOraObject.GetChildIndex(Child: TOraObject): Integer;
var
  i: integer;
begin
  for i := 0 to FObjects.Count - 1 do
    if FObjects[i] = Child then begin
      Result := i;
      Exit;
    end;

  Result := -1;
end;

function TOraObject.GetOwnerIndex: Integer;
begin
  if FOwnerIndex < -1 then
    if (FOwner <> nil) and (FOwner.ObjectType.DataType = dtObject) then
      FOwnerIndex := FOwner.GetChildIndex(Self)
    else
      FOwnerIndex := -1;

  Result := FOwnerIndex;
end;

procedure TOraObject.ChildAlloc(Child: TOraObject);
var
  ChildIndex: Integer;
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  if FObjectType.DataType in [dtArray, dtTable] then
    ChildIndex := GetChildIndex(Child)
  else
    ChildIndex := Child.GetOwnerIndex;
  if ChildIndex < 0 then
    Exit;

  CheckType;

  if FObjectType.DataType in [dtArray, dtTable] then
    GetAttribute(ObjectType.Attributes[0], ChildIndex, Value, Ind, IndPtr, True)
  else
    GetAttribute(ObjectType.Attributes[ChildIndex], -1, Value, Ind, IndPtr, True);
  Child.Instance := Value;
  Child.Indicator := IndPtr;
end;

function TOraObject.GetOCIRef: pOCIRef;
begin
  Result := nil;

  CheckAlloc;
  Check(OCI8.OCIObjectNew(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, OCISvcCtx.hOCISvcCtx,
    OCI_TYPECODE_REF, FObjectType.hTDO, nil, OCI_DURATION_DEFAULT, 1, Result));
  Check(OCI8.OCIObjectGetObjectRef(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, Result));
end;

procedure TOraObject.Lock;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCI8.OCIObjectLock(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance));
end;

procedure TOraObject.Flush;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCI8.OCIObjectFlush(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance));
end;

procedure TOraObject.Refresh;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCI8.OCIObjectRefresh(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance));
end;

procedure TOraObject.MarkDelete;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCI8.OCIObjectMarkDelete(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance));
end;

procedure TOraObject.MarkUpdate;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCI8.OCIObjectMarkUpdate(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance));
end;

procedure TOraObject.Unmark;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCI8.OCIObjectUnmark(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance));
end;

procedure TOraObject.WriteLobs;
var
  i: integer;
  Obj: TObject;
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
  Attr: TAttribute;
  Index: Integer;
begin
  // Use SetAttribute for LOB attrubites.
  // Direct writing LOB locator to object causes AV in OCI.
  if (FObjects <> nil) and (OCISvcCtx.Home.OCIVersion >= 8100) then
    for i := 0 to FObjects.Count - 1 do begin
      Obj := TObject(FObjects[i]);
      if Obj <> nil then begin
        if FObjectType.DataType in [dtArray, dtTable] then begin
          Attr := FObjectType.Attributes[0];
          Index := i;
        end
        else begin
          Attr := FObjectType.Attributes[i];
          Index := -1;
        end;

        case Attr.DataType of
          dtOraBlob, dtOraClob, dtWideOraClob: begin
            GetAttribute(Attr, Index, Value, Ind, IndPtr);
            if Ind = OCI_IND_NOTNULL then begin
              if Attr.DataType = dtNClob then
                TOraLob(Obj).CreateTemporary(ltNClob)
              else if Attr.DataType in [dtOraClob, dtWideOraClob] then
                if Attr.SubDataType = dtNClob then
                  TOraLob(Obj).CreateTemporary(ltNClob)
                else
                  TOraLob(Obj).CreateTemporary(ltClob)
              else
                TOraLob(Obj).CreateTemporary(ltBlob);
              TOraLob(Obj).WriteLob;
              if FObjectType.DataType in [dtArray, dtTable] then
                SetAttributeItemData(Attr, Index, TOraLob(Obj).OCILobLocator, OCI_IND_NOTNULL)
              else
                SetAttributeData(Attr, TOraLob(Obj).OCILobLocator, OCI_IND_NOTNULL);
            end;
          end;
          dtTimeStamp,
          dtTimeStampTZ,
          dtTimeStampLTZ: begin
            Obj := GetAttributeObject(i);
            if Obj <> nil then
              Marshal.WriteInt16(Indicator, Attr.IndicatorOffset, Marshal.ReadInt16(TOraTimeStamp(Obj).IndicatorPtr));
          end;
          dtIntervalYM,
          dtIntervalDS: begin
            Obj := GetAttributeObject(i);
            if Obj <> nil then
              Marshal.WriteInt16(Indicator, Attr.IndicatorOffset, Marshal.ReadInt16(TOraInterval(Obj).IndicatorPtr));
          end;
          dtObject:
            TOraObject(Obj).WriteLobs;
        end;
      end
    end;
end;

procedure TOraObject.ReadLobs;
var
  i: integer;
  Obj: TObject;
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
  Attr: TAttribute;
  Index: Integer;
begin
  // Use SetAttribute for LOB attrubites.
  // Direct writing LOB locator to object causes AV in OCI.
  if (FObjects <> nil) and (OCISvcCtx.Home.OCIVersion >= 8100) then
    for i := 0 to FObjects.Count - 1 do begin
      Obj := TObject(FObjects[i]);
      if Obj <> nil then begin
        if FObjectType.DataType in [dtArray, dtTable] then begin
          Attr := FObjectType.Attributes[0];
          Index := i;
        end
        else begin
          Attr := FObjectType.Attributes[i];
          Index := -1;
        end;

        case Attr.DataType of
          dtOraBlob, dtOraClob, dtWideOraClob: begin
            GetAttribute(Attr, Index, Value, Ind, IndPtr);
            if Ind = OCI_IND_NOTNULL then begin
              Value := Marshal.ReadIntPtr(Value);
              if TOraLob(Obj).OCILobLocator <> Value then
                TOraLob(Obj).OCILobLocator := Value;
              TOraLob(Obj).ReadLob;
            end
            else begin
              TOraLob(Obj).FreeLob;
              TOraLob(Obj).Clear;
            end;
          end;
          dtObject:
            TOraObject(Obj).ReadLobs;
        end;
      end
    end;
end;

procedure TOraObject.ResetInstance(NewInstance, NewIndicator: IntPtr);
var
  i: integer;
  Obj: TObject;
  Value: IntPtr;
  Ind: OCIInd;
  Attr: TAttribute;
  Index: Integer;
  ObjIndicator: IntPtr;
begin
  if NewInstance <> Instance then begin
    FreeObject(False);
    FInstance := NewInstance;
    FIndicator := NewIndicator;
    if FObjects <> nil then begin
      for i := 0 to FObjects.Count - 1 do begin
        Obj := TObject(FObjects[i]);
        if Obj <> nil then begin
          if FObjectType.DataType in [dtArray, dtTable] then begin
            Attr := FObjectType.Attributes[0];
            Index := i;
          end
          else begin
            Attr := FObjectType.Attributes[i];
            Index := -1;
          end;

          GetAttribute(Attr, Index, Value, Ind, ObjIndicator);
          case Attr.DataType of
            dtOraBlob, dtOraClob, dtWideOraClob:
              TOraLob(Obj).OCILobLocator := Value;
            dtObject:
              TOraObject(Obj).ResetInstance(Value, ObjIndicator);
          end;
        end
      end;
    end;
  end;
end;

function TOraObject.Exists: boolean;
var
  Res: tbool;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCI8.OCIObjectExists(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, Res));
  Result := Res = 1;
end;

function TOraObject.IsLocked: boolean;
var
  Res: tbool;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCI8.OCIObjectIsLocked(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, Res));
  Result := Res = 1;
end;

function TOraObject.IsDirty: boolean;
var
  Res: tbool;
begin
  CheckAlloc;
  Check(OCI8.OCIObjectIsDirty(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, Res));
  Result := Res = 1;
end;

const
  MaxNames = 30;

function TOraObject.GetAttribute(const Name: string;
  out Data: IntPtr; out Ind: OCIInd; out Indicator: IntPtr;
  AutoAlloc: boolean = False; MakeNotNull: boolean = False): TAttribute;
var
  AttrChain: TAttributeChain;
begin
  AttrChain := ObjectType.GetAttributeChain(Name);
  try
    Result := GetAttribute(AttrChain, Data, Ind, Indicator, AutoAlloc, MakeNotNull);
  finally
    AttrChain.Free;
  end;
end;

function TOraObject.GetAttribute(AttrChain: TAttributeChain;
  out Data: IntPtr; out Ind: OCIInd; out Indicator: IntPtr;
  AutoAlloc: boolean = False; MakeNotNull: boolean = False): TAttribute;
var
  Attr: TAttribute;
  Index: integer;
  IsNotNull: boolean;
begin
  Result := AttrChain.Last.Attribute;

  CheckAlloc;

  if FOwner <> nil then
    Data := GetObjectData(FOwner.ObjectType, ObjectType, FInstance)
  else
    Data := GetObjectData(nil, ObjectType, FInstance);

  Indicator := FIndicator;
  if MakeNotNull then begin
    WriteIndicator(OCI_IND_NOTNULL);
    IsNotNull := True;
  end
  else
    IsNotNull := not IsNull;
  repeat
    Attr := AttrChain.Attribute;
    Index := AttrChain.Index;

    if Index >= 0 then
      GetAttributeItemData(Attr, Index, Data, Ind, Indicator, AutoAlloc, MakeNotNull)
    else
      GetAttributeData(AttrChain, Attr, IsNotNull, Data, Ind, Indicator, AutoAlloc, MakeNotNull);

    AttrChain := AttrChain.Next;
  until (AttrChain = nil) or (Data = nil);
end;

procedure TOraObject.GetAttribute(Attr: TAttribute; Index: Integer;
  out Data: IntPtr; out Ind: OCIInd; out Indicator: IntPtr;
  AutoAlloc: boolean = False; MakeNotNull: boolean = False);
var
  IsNotNull: boolean;
begin
  CheckAlloc;

  if FOwner <> nil then
    Data := GetObjectData(FOwner.ObjectType, ObjectType, FInstance)
  else
    Data := GetObjectData(nil, ObjectType, FInstance);

  Indicator := FIndicator;
  if MakeNotNull then begin
    WriteIndicator(OCI_IND_NOTNULL);
    IsNotNull := True;
  end
  else
    IsNotNull := not IsNull;

  if Index >= 0 then
    GetAttributeItemData(Attr, Index, Data, Ind, Indicator, AutoAlloc, MakeNotNull)
  else
    GetAttributeData(nil, Attr, IsNotNull, Data, Ind, Indicator, AutoAlloc, MakeNotNull);
end;

procedure TOraObject.GetAttributeData(AttrChain: TAttributeChain; Attr: TAttribute; var IsNotNull: boolean;
  var Data: IntPtr; out Ind: OCIInd; var Indicator: IntPtr; AutoAlloc: boolean; MakeNotNull: boolean);
var
  Obj: TSharedObject;
  OraObj: TOraObject;
  Instance: IntPtr;
  IndPtr: IntPtr;
  OldNull: boolean;
begin
  Data := PtrOffset(Data, Attr.Offset);
  Indicator := PtrOffset(Indicator, Attr.IndicatorOffset);

  if IsNotNull then begin
    case Attr.DataType of
      dtTimeStamp,
      dtTimeStampTZ,
      dtTimeStampLTZ: begin
        if AttrChain = nil then
          Obj := GetAttributeObject(Attr.AttributeNo - 1)
        else if AttrChain.Next = nil then
          Obj := FindComplexAttribute(AttrChain.First)
        else
          Obj := nil;
        if Obj <> nil then
          IndPtr := TOraTimeStamp(Obj).IndicatorPtr
        else
          IndPtr := Indicator;
      end;
      dtIntervalYM,
      dtIntervalDS: begin
        if AttrChain = nil then
          Obj := GetAttributeObject(Attr.AttributeNo - 1)
        else if AttrChain.Next = nil then
          Obj := FindComplexAttribute(AttrChain.First)
        else
          Obj := nil;
        if Obj <> nil then
          IndPtr := TOraTimeStamp(Obj).IndicatorPtr
        else
          IndPtr := Indicator;
      end;
      else
        IndPtr := Indicator;
    end;

    Ind := Marshal.ReadInt16(IndPtr);
    OldNull := (Ind <> OCI_IND_NOTNULL);
    if MakeNotNull and OldNull then begin
      Ind := OCI_IND_NOTNULL;
      Marshal.WriteInt16(IndPtr, Ind);
      IsNotNull := True;
    end
    else
      IsNotNull := not OldNull;
  end
  else begin
    Ind := OCI_IND_NULL;
    OldNull := False;
  end;

  if (Attr.ObjectType <> nil) and (Data <> nil) then
    if Attr.ObjectType.DataType in [dtArray, dtTable] then
      Data := Marshal.ReadIntPtr(Data)
    else if (Attr.ObjectType.DataType = dtObject) and not TOraType(Attr.ObjectType).FFinal then begin
      Instance := Marshal.ReadIntPtr(Data);
      OraObj := nil;

      if not OCISvcCtx.Home.Direct and OldNull then
        if AutoAlloc then begin
          // If attribute was NULL not we don't use Instance pointer from
          // this attribute. Probably it is a ramdom value.
          OraObj := TOraObject(GetChildObject(Attr));
          if OraObj.FInstance = nil then begin// not yet allocated
            OraObj.AllocNewObject;
            Instance := OraObj.FInstance;
            OraObj.FInstance := nil;
            Marshal.WriteIntPtr(Data, Instance);
            Check(OCI8.OCIObjectGetInd(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, OraObj.FIndicatorPtr));
          end;
        end
        else
          Instance := nil;

      if Instance <> nil then begin
        if OraObj <> nil then begin
          IndPtr := OraObj.FIndicatorPtr;
          Indicator := Marshal.ReadIntPtr(IndPtr);
        end
        else begin
          IndPtr := @Indicator;
          Check(OCI8.OCIObjectGetInd(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, IndPtr));
        end;

        if MakeNotNull then
          Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);

        if OCISvcCtx.Home.Direct then
          Instance := OCI8.OCIObjectPtr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance);
      end
      else
        Indicator := nil;

      Data := Instance;
    end;
end;

procedure TOraObject.GetAttributeItemData(Attr: TAttribute; Index: Integer;
  var Data: IntPtr; out Ind: OCIInd; var Indicator: IntPtr; AutoAlloc: boolean; MakeNotNull: boolean);
var
  i: Integer;
  ArrSize: integer;
  ItemExist: tbool;
  ItemInstance: IntPtr;
  ItemIndicator: IntPtr;
begin
  if AutoAlloc then begin
    Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Data, ArrSize));
    if Index >= ArrSize then
      for i := ArrSize to Index do begin
        case Attr.DataType of
          dtDateTime:
            ItemInstance := NullOCIDate;
          dtInteger, dtLargeInt, dtFloat:
            ItemInstance := NullOCINumber;
          dtString, dtWideString:
            ItemInstance := NullOCIString;
          dtObject:
            if OCISvcCtx.Home.Direct then
              ItemInstance := nil
            else
              ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
        else
          ItemInstance := nil;
          Assert(False, SUnknownDataType);
        end;

        // WAR need real Indicator for object
        try
          Check(OCI8.OCICollAppend(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ItemInstance, NullIndStruct, Data));
        finally
          if Attr.DataType = dtObject then
            Marshal.FreeHGlobal(ItemInstance);
        end;
      end;
  end;

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Data, Index, ItemExist,
    Data, ItemIndicator));

  if ItemExist = 1 then begin
    Data := GetObjectData(TOraType(Attr.Owner), TOraType(Attr.ObjectType), Data);
    Indicator := ItemIndicator;
    if MakeNotNull then begin
      Ind := OCI_IND_NOTNULL;
      Marshal.WriteInt16(ItemIndicator, Ind);
    end
    else
      Ind := Marshal.ReadInt16(ItemIndicator);
  end
  else begin
    Data := nil;
    Indicator := nil;
    Ind := OCI_IND_NULL;
    Exit;
  end;
end;

// this procedure cannot be used in Unicode environment because of OCI bug:
// OCIObjectSetAttr compares 1 character of name
//procedure TOraObject.SetAttribute(Name: string; Value: IntPtr; Ind: OCIInd);
//var
//  NamesArray: IntPtr;
//  Names: array [0..MaxNames] of string;
//  Lengths: IntPtr;
//  Types: array [1..MaxNames] of TOraType;
//  Attrs: array [0..MaxNames] of TAttribute;
//  NameCount: integer;
//  Instance, InstancePtr: IntPtr;
//  Indicator: IntPtr;
//  TDO: pOCIType;
//  TDOPtr: IntPtr;
//  i, Size: integer;
//  IndexPos: integer;
//  Index: integer;
//  Exists: tbool;
//  ArrSize: integer;
//  OType: TObjectType;
//  Attr: TAttribute;
//  ItemInstance, ItemInstancePtr: IntPtr;
//  ItemIndicator, ItemIndicatorPtr: IntPtr;
//  ItemInd: OCIInd;
//  ItemIndPtr: IntPtr;
//  Curr: integer;
//  NullBefore: boolean;
//  TempInd: IntPtr;
//begin
//  CheckAlloc;
//
//  Name := AnsiUpperCase(Name);
//
//  Instance := FInstance;
//  Indicator := FIndicator;
//  TDO := ObjectType.TDO;
//  OType := ObjectType;
//
//  Lengths := Marshal.AllocHGlobal(sizeof(integer) * (MaxNames + 1));
//  try
//    repeat
//      Name := TrimLeft(Name);
//
//      IndexPos := Pos('[', Name);
//      if IndexPos = 1 then begin
//        i := Pos(']', Name);
//        if i > 0 then
//          Index := StrToInt(Copy(Name, 2, i - 2))
//        else begin
//          RaiseError(SInvalidAttrName);
//          Index := -1;
//        end;
//
//        if (i + 1 <= Length(Name)) and (Name[i + 1] = '.') then
//          Inc(i);
//
//        Name := Copy(Name, i + 1, Length(Name));
//        Attr := OType.Attributes[0];
//        if Attr = nil then
//          RaiseError(SInvalidAttrName);
//
//        Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, ArrSize));
//        if Index >= ArrSize then
//          for i := ArrSize to Index do begin
//            case Attr.DataType of
//              dtDateTime:
//                ItemInstance := NullOCIDate;
//              dtInteger, dtLargeInt, dtFloat:
//                ItemInstance := NullOCINumber;
//              dtString, dtWideString:
//                ItemInstance := NullOCIString;
//              dtObject: begin
//                if OCISvcCtx.Home.Direct then
//                  ItemInstance := nil
//                else
//                  ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
//                // Check(OCIObjectNew());
//              end;
//            else
//              ItemInstance := nil;
//              Assert(False, SUnknownDataType);
//            end;
//
//            // WAR need real Indicator for object
//            try
//              Check(OCI8.OCICollAppend(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ItemInstance, NullIndStruct, Instance));
//            finally
//              if Attr.DataType = dtObject then
//                Marshal.FreeHGlobal(ItemInstance);
//            end;
//          end;
//
//        if Name = '' then begin
//          if Value = nil then begin
//            Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists, Value, TempInd));
//            Value := Marshal.ReadIntPtr(Value);  // WAR for dtString only
//          end;
//
//          TempInd := Marshal.AllocHGlobal(sizeof(Ind));
//          Marshal.WriteInt16(TempInd, Ind);
//          try
//            Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Value, TempInd, Instance));
//          finally
//            Marshal.FreeHGlobal(TempInd);
//          end;
//        end
//        else begin
//          if Ind = OCI_IND_NOTNULL then
//            Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object
//
//          Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
//            Instance, Indicator));
//
//          OType := OType.Attributes[0].ObjectType;
//          TDO := TOraType(OType).TDO; // WAR for array of objects
//        end;
//      end
//      else begin
//        NameCount := 0;
//        repeat
//          i := Pos('.', Name);
//          if (i > 0) and ((i < IndexPos) or (IndexPos = 0)) then begin
//            Names[NameCount] := Copy(Name, 1, i - 1);
//            Name := Copy(Name, i + 1, Length(Name));
//          end
//          else
//            if IndexPos > 0 then begin
//              Names[NameCount] := Copy(Name, 1, IndexPos - 1);
//              Name := Copy(Name, IndexPos, Length(Name));
//            end
//            else
//              Names[NameCount] := Name;
//
//          Attr := OType.FindAttribute(Names[NameCount]);
//          Attrs[NameCount] := Attr;
//          if Attr = nil then
//            RaiseError(SInvalidAttrName);
//          if Name <> '' then
//            OType := Attr.ObjectType;
//
//          Types[NameCount + 1] := TOraType(OType);
//          Inc(NameCount);
//        until (i = 0) or (i > IndexPos) and (IndexPos <> 0);
//
//        NamesArray := Marshal.AllocHGlobal((MaxNames + 1) * sizeof(IntPtr));
//        for i := 0 to NameCount - 1 do begin
//          Marshal.WriteIntPtr(NamesArray, i * sizeof(IntPtr), StringToHGlobalOCI(Names[i], Size, OCISvcCtx.UnicodeEnv));
//          Marshal.WriteInt32(Lengths, i * sizeof(integer), Size);
//        end;
//        try
//          if IndexPos > 0 then begin
//          // getting array
//            // set array to notnull OPTIM?
//            Check(OCI8.OCIObjectSetAttr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Indicator, TDO,
//              NamesArray, Lengths, NameCount, nil, 0, OCI_IND_NOTNULL, nil, nil));
//
//            InstancePtr := OrdinalToPtr(Instance);
//            TDOPtr := OrdinalToPtr(TDO);
//            try
//              Check(OCI8.OCIObjectGetAttr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Indicator, TDO,
//                NamesArray, Lengths, NameCount, nil, 0, nil, nil, InstancePtr, TDOPtr));
//            finally
//              PtrToOrdinal(InstancePtr, Instance);
//              PtrToOrdinal(TDOPtr, TDO);
//            end;
//
//            Instance := Marshal.ReadIntPtr(Instance);
//          end
//          else begin
//            Curr := 0;
//
//            if (NameCount > 1) and (Ind = OCI_IND_NOTNULL) then // for object attributes
//              for i := 1 to NameCount - 1 do begin
//                NullBefore := not Types[i].FFinal and
//                  (Marshal.ReadInt16(Indicator, Attrs[i-1].IndicatorOffset) = OCI_IND_NULL);
//                Check(OCI8.OCIObjectSetAttr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Indicator, TDO,
//                  PtrOffset(NamesArray, Curr), PtrOffset(Lengths, Curr), i - Curr, nil, 0, OCI_IND_NOTNULL, nil, nil));
//                if not Types[i].FFinal then begin
//                  ItemIndicatorPtr := OrdinalToPtr(ItemIndicator);
//                  ItemIndPtr := OrdinalToPtr(ItemInd);
//                  ItemInstancePtr := OrdinalToPtr(ItemInstance);
//                  try
//                    Check(OCI8.OCIObjectGetAttr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Indicator, TDO,
//                      PtrOffset(NamesArray, Curr), PtrOffset(Lengths, Curr), i - Curr, nil, 0, ItemIndPtr, ItemIndicatorPtr, ItemInstancePtr, nil));
//                  finally
//                    PtrToOrdinal(ItemInstancePtr, ItemInstance);
//                    PtrToOrdinal(ItemIndPtr, ItemInd);
//                    PtrToOrdinal(ItemIndicatorPtr, ItemIndicator);
//                  end;
//                  if (ItemInd = OCI_IND_NULL) or NullBefore then begin
//                    FillChar(ItemIndicator, Types[i].IndicatorSize, byte(OCI_IND_NULL));
//                    Marshal.WriteInt16(ItemIndicator, OCI_IND_NOTNULL);
//                  end;
//                  Instance := ItemInstance;
//                  Indicator := ItemIndicator;
//                  TDO := Types[i].TDO;
//                  Curr := i;
//                end;
//              end;
//
//            // setting attribute
//            Check(OCI8.OCIObjectSetAttr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Indicator, TDO,
//              PtrOffset(NamesArray, Curr * sizeof(IntPtr)), PtrOffset(Lengths, Curr * sizeof(IntPtr)), NameCount - Curr, nil, 0, Ind, nil, Value));
//          end;
//        finally
//          for i := 0 to NameCount - 1 do
//            FreeStringOCI(Marshal.ReadIntPtr(NamesArray, i * sizeof(IntPtr)), OCISvcCtx.UnicodeEnv);
//          Marshal.FreeHGlobal(NamesArray);
//        end;
//
//        {if Name <> '' then
//          OType := Attr.ObjectType;}
//      end;
//    until (IndexPos = 0) or (Name = '');
//  finally
//    Marshal.FreeHGlobal(Lengths);
//  end;
//
//  if (Ind = OCI_IND_NOTNULL) then
//    Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object
//end;

procedure TOraObject.SetAttributeData(Attr: TAttribute; Data: IntPtr; Ind: OCIInd);
var
  pName: IntPtr;
  Len: integer;
begin
  CheckAlloc;

  pName := StringToHGlobalOCI(Attr.Name, Len, OCISvcCtx.UnicodeEnv);
  try
    Check(OCI8.OCIObjectSetAttr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Indicator, ObjectType.TDO,
      @pName, @Len, 1, nil, 0, Ind, nil, Data));
  finally
    FreeStringOCI(pName, OCISvcCtx.UnicodeEnv);
  end;

  if (Ind = OCI_IND_NOTNULL) then
    Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object
end;

procedure TOraObject.SetAttributeItemData(Attr: TAttribute; Index: Integer; Data: IntPtr; Ind: OCIInd);
var
  i: integer;
  ArrSize: integer;
  ItemInstance: IntPtr;
  TempInd: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, ArrSize));
  if Index >= ArrSize then
    for i := ArrSize to Index do begin
      case Attr.DataType of
        dtDateTime:
          ItemInstance := NullOCIDate;
        dtInteger, dtLargeInt, dtFloat:
          ItemInstance := NullOCINumber;
        dtString, dtWideString:
          ItemInstance := NullOCIString;
        dtObject: begin
          if OCISvcCtx.Home.Direct then
            ItemInstance := nil
          else
            ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
          // Check(OCIObjectNew());
        end;
      else
        ItemInstance := nil;
        Assert(False, SUnknownDataType);
      end;

      // WAR need real Indicator for object
      try
        Check(OCI8.OCICollAppend(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ItemInstance, NullIndStruct, Instance));
      finally
        if Attr.DataType = dtObject then
          Marshal.FreeHGlobal(ItemInstance);
      end;
    end;

  if Data = nil then begin
    Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists, Data, TempInd));
    Data := Marshal.ReadIntPtr(Data);  // WAR for dtString only
  end;

  TempInd := Marshal.AllocHGlobal(sizeof(Ind));
  Marshal.WriteInt16(TempInd, Ind);
  try
    Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Data, TempInd, Instance));
  finally
    Marshal.FreeHGlobal(TempInd);
  end;

  if (Ind = OCI_IND_NOTNULL) then
    Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object
end;

procedure TOraObject.GetAttributeValue(const Name: string; out AttrBuf: IntPtr; out AttrSize: Word; out IsBlank, NativeBuffer: boolean);
var
  AttrChain: TAttributeChain;
begin
  AttrChain := ObjectType.GetAttributeChain(Name);
  try
    GetAttributeValue(AttrChain, AttrBuf, AttrSize, IsBlank, NativeBuffer);
  finally
    AttrChain.Free;
  end;
end;

procedure TOraObject.GetAttributeValue(AttrChain: TAttributeChain; out AttrBuf: IntPtr; out AttrLen: Word; out IsBlank, NativeBuffer: boolean);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
  Ptr: IntPtr;
  AttrSize: Integer;
  CurPtr: PWideChar;
  LastPtr: PWideChar;
  OCIStr: pOCIString;
begin
  Attr := GetAttribute(AttrChain, Data, Ind, IndPtr);
  IsBlank := ReadAttrIsNull(Attr, Ind);

  if IsBlank and
    not (Attr.DataType in [dtObject, dtReference, dtArray, dtTable,
                           dtOraBlob, dtOraClob, dtWideOraClob, dtBFILE, dtCFILE{,
                           dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS}])
  then begin
    NativeBuffer := True;
    AttrBuf := nil;
    Exit;
  end;

  AttrBuf := Marshal.AllocHGlobal(Attr.Size);
  NativeBuffer := False;
  try
    case Attr.DataType of
      dtString: begin
        OCIStr := ReadAsOCIString(Attr, Data);
        Ptr := OCI8.OCIStringPtr(OCISvcCtx.hOCIEnv, OCIStr);
        if Ptr <> nil then begin
          AttrLen := OCI8.OCIStringSize(OCISvcCtx.hOCIEnv, OCIStr);
          if AttrLen > Attr.Length then
            AttrLen := Attr.Length;

          Move(Ptr^, AttrBuf^, AttrLen);
          if Attr.Fixed and not FTrimFixedChar  then begin
            FillChar(PtrOffset(AttrBuf, AttrLen), Attr.Length - AttrLen, $20);
            AttrLen := Attr.Length;
          end;
          Marshal.WriteByte(AttrBuf, AttrLen, 0);
        end
        else
          Marshal.WriteByte(AttrBuf, 0);
      end;
      dtWideString: begin
        OCIStr := ReadAsOCIString(Attr, Data);
        Ptr := OCI8.OCIStringPtr(OCISvcCtx.hOCIEnv, OCIStr);
        if Ptr <> nil then begin
          AttrSize := OCI8.OCIStringSize(OCISvcCtx.hOCIEnv, OCIStr);
          AttrLen := AttrSize shr 1;
          if AttrLen > Attr.Length then begin
            AttrLen := Attr.Length;
            AttrSize := AttrLen * SizeOf(WideChar);
          end;

          Move(Ptr^, AttrBuf^, AttrSize);
          if Attr.Fixed and not FTrimFixedChar  then begin
            CurPtr := PtrOffset(AttrBuf, AttrSize);
            LastPtr := PtrOffset(AttrBuf, AttrSize);
            while CurPtr < LastPtr do begin
              CurPtr^ := ' ';
              Inc(CurPtr);
            end;
            AttrLen := Attr.Length;
            AttrSize := AttrLen * SizeOf(WideChar);
          end;
          Marshal.WriteInt16(AttrBuf, AttrSize, 0);
        end
        else
          Marshal.WriteInt16(AttrBuf, 0);
      end;
      dtInteger:
        Marshal.WriteInt32(AttrBuf, ReadAsInteger(Attr, Data, Ind));
      dtLargeInt:
        Marshal.WriteInt64(AttrBuf, ReadAsLargeInt(Attr, Data, Ind));
      dtFloat:
        Marshal.WriteInt64(AttrBuf, BitConverter.DoubleToInt64Bits(ReadAsFloat(Attr, Data, Ind)));
      dtDateTime:
        Marshal.WriteInt64(AttrBuf, BitConverter.DoubleToInt64Bits(ReadAsDateTime(Attr, Data, Ind)));
      dtObject,
      dtAnyData,
      dtXML:
        Marshal.WriteIntPtr(AttrBuf, ReadAsObject(AttrChain).GCHandle);
      dtReference:
        Marshal.WriteIntPtr(AttrBuf, ReadAsRef(AttrChain).GCHandle);
      dtArray,
      dtTable:
        Marshal.WriteIntPtr(AttrBuf, ReadAsArray(AttrChain).GCHandle);
      dtOraBlob,
      dtOraClob,
      dtWideOraClob,
      dtBFILE,
      dtCFILE:
        Marshal.WriteIntPtr(AttrBuf, ReadAsLob(AttrChain).GCHandle);
      dtTimeStamp,
      dtTimeStampTZ,
      dtTimeStampLTZ:
        Marshal.WriteIntPtr(AttrBuf, ReadAsOraTimeStamp(AttrChain).GCHandle);
      dtIntervalYM,
      dtIntervalDS:
        Marshal.WriteIntPtr(AttrBuf, ReadAsOraInterval(AttrChain).GCHandle);
    else
      Assert(False, SUnknownDataType);
    end;
  except
    Marshal.FreeHGlobal(AttrBuf);
    raise;
  end;
end;

procedure TOraObject.SetAttributeValue(const Name: string; ValuePtr: IntPtr; ValueLen: Word);
var
  AttrChain: TAttributeChain;
begin
  AttrChain := ObjectType.GetAttributeChain(Name);
  try
    SetAttributeValue(AttrChain, ValuePtr, ValueLen);
  finally
    AttrChain.Free;
  end;
end;

procedure TOraObject.SetAttributeValue(AttrChain: TAttributeChain; ValuePtr: IntPtr; ValueLen: Word);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
  BFile: TOraFile;
begin
  CheckType;

  Attr := GetAttribute(AttrChain, Data, Ind, IndPtr, True, True);

  if ValuePtr = nil then begin
    case Attr.DataType of
      dtOraBlob, dtOraClob, dtWideOraClob:
        WriteAttrIsNull(Attr, IndPtr, ReadAsLob(AttrChain).IsNull);
      dtBFILE, dtCFILE: begin
        BFile := TOraFile(ReadAsLob(AttrChain));
        WriteAttrIsNull(Attr, IndPtr, BFile.IsNull or ((BFile.FileDir = '') and (BFile.FileName = '')));
      end
    else
      WriteAttrIsNull(Attr, IndPtr, True);
    end;
    Exit;
  end;

  case Attr.DataType of
    dtString:
      WriteAsAnsiString(Attr, Data, ValuePtr, ValueLen);
    dtWideString:
      WriteAsWideString(Attr, Data, ValuePtr, ValueLen);
    dtInteger:
      WriteAsInteger(Attr, Data, Marshal.ReadInt32(ValuePtr));
    dtLargeInt:
      WriteAsLargeInt(Attr, Data, Marshal.ReadInt64(ValuePtr));
    dtFloat:
      WriteAsFloat(Attr, Data, BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr)));
    dtDateTime:
      WriteAsDateTime(Attr, Data, MemUtils.TimeStampToDateTime(MSecsToTimeStamp(Trunc(
        BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr))))));
    dtBFILE, dtCFILE:
      WriteAttrIsNull(Attr, IndPtr, ReadAsLob(AttrChain).Size = 0);
    dtOraBlob,
    dtOraClob,
    dtWideOraClob:
      ; // Empty
    dtTimeStamp,
    dtTimeStampTZ,
    dtTimeStampLTZ:
      ReadAsOraTimeStamp(AttrChain).Assign(TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr))));
    dtIntervalYM,
    dtIntervalDS:
      ReadAsOraInterval(AttrChain).Assign(TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr))));
  else
    RaiseError(SUnknownDataType);
  end;
end;

function TOraObject.GetComplexAttribute(const Name: string): TSharedObject;
var
  AttrChain: TAttributeChain;
begin
  AttrChain := ObjectType.GetAttributeChain(Name);
  try
    Result := GetComplexAttribute(AttrChain);
  finally
    AttrChain.Free;
  end;
end;

function TOraObject.GetComplexAttribute(AttrChain: TAttributeChain): TSharedObject;
begin
  Result := GetChildObject(AttrChain.Attribute, AttrChain.Index);

  if AttrChain.Next <> nil then
    if Result is TOraObject then
      Result := TOraObject(Result).GetComplexAttribute(AttrChain.Next)
    else
      RaiseError(SInvalidAttrName);
end;

function TOraObject.FindComplexAttribute(AttrChain: TAttributeChain): TSharedObject;
begin
  if AttrChain.Index < 0 then
    Result := GetAttributeObject(AttrChain.Attribute.AttributeNo - 1)
  else
    Result := GetAttributeObject(AttrChain.Index);

  if AttrChain.Next <> nil then
    if Result is TOraObject then
      Result := TOraObject(Result).FindComplexAttribute(AttrChain.Next)
    else
      Result := nil;
end;

function TOraObject.GetAttributeObject(Index: Integer): TSharedObject;
begin
  if FObjects = nil then
    Result := nil
  else if Index < FObjects.Count then
    Result := TSharedObject(FObjects[Index])
  else
    Result := nil;
end;

procedure TOraObject.SetAttributeObject(Index: Integer; Obj: TSharedObject);
begin
  if FObjects = nil then
    FObjects := TList.Create;

  while Index >= FObjects.Count do
    FObjects.Add(nil);

  FObjects[Index] := Obj;
end;

function TOraObject.GetIsNull: boolean;
var
  Ind: OCIInd;
begin
  if (ObjectType <> nil) and (ObjectType.FTypeCode = OCI_TYPECODE_OPAQUE) then begin
    Result := FInstance = nil;
    Exit;
  end;

  if (ObjectType <> nil) and ((FOwner <> nil) or (FInstance <> nil)) then
    CheckAlloc;

  Ind := ReadIndicator;
  Result := Ind <> OCI_IND_NOTNULL;
end;

procedure TOraObject.SetIsNull(Value: boolean);
begin
  if FOwner <> nil then
    CheckAlloc;

  if Value then
    WriteIndicator(OCI_IND_NULL)
  else
    WriteIndicator(OCI_IND_NOTNULL)
end;

function TOraObject.ReadIndicator: OCIInd;
var
  OnwerIndex: integer;
  Attr: TAttribute;
  IndPtr: IntPtr;
begin
  if (ObjectType <> nil) and
     (ObjectType.DataType = dtObject) and
     not TOraType(ObjectType).FFinal
  then begin
    OnwerIndex := GetOwnerIndex;
    if OnwerIndex > 0 then begin
      IndPtr := FOwner.FIndicator;
      if IndPtr <> nil then begin
        Attr := FOwner.ObjectType.Attributes[OnwerIndex];
        IndPtr := PtrOffset(IndPtr, Attr.IndicatorOffset);
        Result := Marshal.ReadInt16(IndPtr);
        Exit;
      end;
    end;
  end;

  IndPtr := FIndicator;
  if IndPtr <> nil then
    Result := Marshal.ReadInt16(IndPtr)
  else
    Result := OCI_IND_NULL;
end;

procedure TOraObject.WriteIndicator(Value: OCIInd);
var
  OnwerIndex: integer;
  Attr: TAttribute;
  IndPtr: IntPtr;
begin
  if (ObjectType <> nil) and
     (ObjectType.DataType = dtObject) and
     not TOraType(ObjectType).FFinal
  then begin
    OnwerIndex := GetOwnerIndex;
    if OnwerIndex > 0 then begin
      IndPtr := FOwner.FIndicator;
      if IndPtr <> nil then begin
        Attr := FOwner.ObjectType.Attributes[OnwerIndex];
        IndPtr := PtrOffset(IndPtr, Attr.IndicatorOffset);
        Marshal.WriteInt16(IndPtr, Value)
      end;
    end;
  end;

  IndPtr := FIndicator;
  if IndPtr <> nil then
    Marshal.WriteInt16(IndPtr, Value);
end;

function TOraObject.GetIndicatorPtr: IntPtr;
begin
  Result := FIndicatorPtr;
end;

procedure TOraObject.AssignInstance(Value: IntPtr);
begin
  FInstance := Value;
end;

function TOraObject.GetAttrIsNull(const Name: string): boolean;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAttrIsNull(Attr, Ind);
end;

procedure TOraObject.SetAttrIsNull(const Name: string; Value: boolean);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  if Value then begin
    Attr := GetAttribute(Name, Data, Ind, IndPtr);
    WriteAttrIsNull(Attr, IndPtr, Value);
  end
  else begin
    Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
    WriteAttrIsNull(Attr, IndPtr, Value);
  end;
end;

function TOraObject.ReadAttrIsNull(Attr: TAttribute; Ind: OCIInd): boolean;
begin
  Result := Ind <> OCI_IND_NOTNULL;
end;

procedure TOraObject.WriteAttrIsNull(Attr: TAttribute; IndPtr: IntPtr; Value: Boolean);
begin
  if IndPtr <> nil then
    if Value then
      Marshal.WriteInt16(IndPtr, OCI_IND_NULL)
    else
      Marshal.WriteInt16(IndPtr, OCI_IND_NOTNULL);
end;

function TOraObject.GetAsOCIDate(const Name: string): OCIDate;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsOCIDate(Attr, Data);
end;

procedure TOraObject.SetAsOCIDate(const Name: string; Value: OCIDate);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsOCIDate(Attr, Data, Value);
end;

function TOraObject.ReadAsOCIDate(Attr: TAttribute; Data: IntPtr): OCIDate;
begin
  case Attr.DataType of
    dtDateTime:
      Move(Data^, Result, sizeof(OCIDate));
  else
    raise EConvertError.Create(SCannotConvert + 'OCIDate');
    Result.OCIDateYYYY := 0; // anti warning
  end;
end;

procedure TOraObject.WriteAsOCIDate(Attr: TAttribute; Data: IntPtr; Value: OCIDate);
begin
  case Attr.DataType of
    dtDateTime: begin
      Assert(Data <> nil);
      Move(Value, Data^, sizeof(OCIDate));
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCIDate');
  end;
end;

function TOraObject.GetAsOCINumber(const Name: string): OCINumber;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsOCINumber(Attr, Data);
end;

procedure TOraObject.SetAsOCINumber(const Name: string; const Value: OCINumber);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsOCINumber(Attr, Data, Value);
end;

function TOraObject.ReadAsOCINumber(Attr: TAttribute; Data: IntPtr): OCINumber;
begin
  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat:
      Move(Data^, Result.OCINumberPart, OCI_NUMBER_SIZE);
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
    Result.OCINumberPart[0] := 0; // anti warning
  end;
end;

procedure TOraObject.WriteAsOCINumber(Attr: TAttribute; Data: IntPtr; const Value: OCINumber);
begin
  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      Assert(Data <> nil);
      Move(Value.OCINumberPart, Data^, OCI_NUMBER_SIZE);
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
  end;
end;

function TOraObject.GetAsOCIString(const Name: string): pOCIString;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsOCIString(Attr, Data);
end;

procedure TOraObject.SetAsOCIString(const Name: string; Value: pOCIString);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsOCIString(Attr, Data, Value);
end;

function TOraObject.ReadAsOCIString(Attr: TAttribute; Data: IntPtr): pOCIString;
begin
  case Attr.DataType of
    dtString, dtWideString: begin
      Result := Marshal.ReadIntPtr(Data)
      {if Ind >= OCI_IND_NOTNULL then
        Result := pOCIString(Value^)}
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCIString');
  end;
end;

procedure TOraObject.WriteAsOCIString(Attr: TAttribute; Data: IntPtr; Value: pOCIString);
begin
  case Attr.DataType of
    dtString, dtWideString:
      Check(OCI8.OCIStringAssign(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Value, Data));
  else
    raise EConvertError.Create(SCannotConvert + 'OCIString');
  end;
end;

function TOraObject.GetAsDateTime(const Name: string): TDateTime;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsDateTime(Attr, Data, Ind);
end;

procedure TOraObject.SetAsDateTime(const Name: string; Value: TDateTime);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsDateTime(Attr, Data, Value);
end;

function TOraObject.ReadAsDateTime(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): TDateTime;
var
  lOCIDate: OCIDate;
  Time: TDateTime;
begin
  if not ReadAttrIsNull(Attr, Ind) then begin
    lOCIDate := ReadAsOCIDate(Attr, Data);

    Result := EncodeDate(lOCIDate.OCIDateYYYY, lOCIDate.OCIDateMM, lOCIDate.OCIDateDD);
    Time := EncodeTime(lOCIDate.OCIDateTime.OCITimeHH, lOCIDate.OCIDateTime.OCITimeMI, lOCIDate.OCIDateTime.OCITimeSS, 0);
    if Result < 0 then
      Result := Result - Time
    else
      Result := Result + Time;
  end
  else
    Result := 0;
end;

procedure TOraObject.WriteAsDateTime(Attr: TAttribute; Data: IntPtr; Value: TDateTime);
var
  Year, Month, Day: word;
  Hour, Min, Sec, MSec: word;
  lOCIDate: OCIDate;
begin
  DecodeDate(Value, Year, Month, Day);
  DecodeTime(Value, Hour, Min, Sec, MSec);

  lOCIDate.OCIDateYYYY := Year;
  lOCIDate.OCIDateMM := Month;
  lOCIDate.OCIDateDD := Day;
  lOCIDate.OCIDateTime.OCITimeHH := Hour;
  lOCIDate.OCIDateTime.OCITimeMI := Min;
  lOCIDate.OCIDateTime.OCITimeSS := Sec;

  WriteAsOCIDate(Attr, Data, lOCIDate);
end;

function TOraObject.GetAsFloat(const Name: string): double;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsFloat(Attr, Data, Ind);
end;

procedure TOraObject.SetAsFloat(const Name: string; Value: double);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsFloat(Attr, Data, Value);
end;

function TOraObject.ReadAsFloat(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): double;
begin
  if not ReadAttrIsNull(Attr, Ind) then begin
    case Attr.DataType of
      dtInteger, dtLargeInt, dtFloat: begin
        case Attr.SubDataType of
          dtBDouble:
            Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Data));
          dtBFloat:
            Result := Single(Data^);
        else
          Check(OCI8.OCINumberToReal(OCISvcCtx.hOCIError, Data, SizeOf(Double), Result));
        end;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'OCINumber');
      Result := 0; // anti warning
    end;
  end
  else
    Result := 0;
end;

procedure TOraObject.WriteAsFloat(Attr: TAttribute; Data: IntPtr; Value: double);
begin
  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      Assert(Data <> nil);
      case Attr.SubDataType of
        dtBDouble:
          Marshal.WriteInt64(Data, BitConverter.DoubleToInt64Bits(Value));
        dtBFloat:
          Single(Data^) := Value;
      else
        Check(OCI8.OCINumberFromReal(OCISvcCtx.hOCIError, Value, SizeOf(Double), Data));
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
  end;
end;

function TOraObject.GetAsInteger(const Name: string): integer;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsInteger(Attr, Data, Ind);
end;

procedure TOraObject.SetAsInteger(const Name: string; Value: integer);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsInteger(Attr, Data, Value);
end;

function TOraObject.ReadAsInteger(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): integer;
var
  Val: int64;
begin
  if not ReadAttrIsNull(Attr, Ind) then begin
    case Attr.DataType of
      dtInteger, dtLargeInt, dtFloat: begin
        case Attr.SubDataType of
          dtBDouble:
            Result := Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Data)));
          dtBFloat:
            Result := Trunc(Single(Data^));
        else
          Check(OCI8.OCINumberToInt(OCISvcCtx.hOCIError, Data, SizeOf(Integer), OCI_NUMBER_SIGNED, Val));
          Result := Integer(Val);
        end;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'OCINumber');
    end;
  end
  else
    Result := 0;
end;

procedure TOraObject.WriteAsInteger(Attr: TAttribute; Data: IntPtr; Value: integer);
var
  Val: int64;
begin
  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      Assert(Data <> nil);
      case Attr.SubDataType of
        dtBDouble:
          Marshal.WriteInt64(Data, BitConverter.DoubleToInt64Bits(Value));
        dtBFloat:
          Single(Data^) := Value;
      else
        Val := Value;
        Check(OCI8.OCINumberFromInt(OCISvcCtx.hOCIError, Val, SizeOf(Integer), OCI_NUMBER_SIGNED, Data));
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
  end;
end;

function TOraObject.GetAsLargeInt(const Name: string): int64;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsLargeInt(Attr, Data, Ind);
end;

procedure TOraObject.SetAsLargeInt(const Name: string; Value: int64);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsLargeInt(Attr, Data, Value);
end;

function TOraObject.ReadAsLargeInt(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): int64;
var
  Val: int64;
begin
  if not ReadAttrIsNull(Attr, Ind) then begin
    case Attr.DataType of
      dtInteger, dtLargeInt, dtFloat: begin
        case Attr.SubDataType of
          dtBDouble:
            Result := Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Data)));
          dtBFloat:
            Result := Trunc(Single(Data^));
        else
          Check(OCI8.OCINumberToInt(OCISvcCtx.hOCIError, Data, SizeOf(Int64), OCI_NUMBER_SIGNED, Val));
          Result := Val;
        end;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'OCINumber');
    end;
  end
  else
    Result := 0;
end;

procedure TOraObject.WriteAsLargeInt(Attr: TAttribute; Data: IntPtr; Value: int64);
begin
  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      Assert(Data <> nil);
      case Attr.SubDataType of
        dtBDouble:
          Marshal.WriteInt64(Data, BitConverter.DoubleToInt64Bits(Value));
        dtBFloat:
          Single(Data^) := Value;
      else
        Check(OCI8.OCINumberFromInt(OCISvcCtx.hOCIError, Value, SizeOf(Int64), OCI_NUMBER_SIGNED, Data));
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
  end;
end;

function TOraObject.GetAsString(const Name: string): string;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsString(Attr, Data, Ind);
end;

procedure TOraObject.SetAsString(const Name: string; const Value: string);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsString(Attr, Data, Value);
end;

function TOraObject.ReadAsString(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): string;
begin
{$IFDEF VER12P}
  Result := ReadAsWideString(Attr, Data, Ind);
{$ELSE}
  Result := ReadAsAnsiString(Attr, Data, Ind);
{$ENDIF}
end;

procedure TOraObject.WriteAsString(Attr: TAttribute; Data: IntPtr; const Value: string);
begin
{$IFDEF VER12P}
  WriteAsWideString(Attr, Data, Value);
{$ELSE}
  WriteAsAnsiString(Attr, Data, Value);
{$ENDIF}
end;

function TOraObject.GetAsAnsiString(const Name: string): AnsiString;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsAnsiString(Attr, Data, Ind);
end;

procedure TOraObject.SetAsAnsiString(const Name: string; const Value: AnsiString);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsAnsiString(Attr, Data, Value);
end;

function TOraObject.ReadAsAnsiString(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): AnsiString;
var
  lOCIString: pOCIString;
  pOCIString: IntPtr;
  Len: Integer;
begin
  Result := '';

  if not ReadAttrIsNull(Attr, Ind) then begin
    case Attr.DataType of
      dtString: begin
        lOCIString := ReadAsOCIString(Attr, Data);
        if lOCIString <> nil then begin
          pOCIString := OCI8.OCIStringPtr(OCISvcCtx.hOCIEnv, lOCIString);
          if pOCIString <> nil then begin
            Len := OCI8.OCIStringSize(OCISvcCtx.hOCIEnv, lOCIString);
            if Len > Attr.Length then
              Len := Attr.Length;

            if Attr.Fixed and not FTrimFixedChar  then begin
              SetLengthA(Result, Attr.Length);
              Move(pOCIString^, PAnsiChar(Result)^, Len);
              FillChar(PtrOffset(PAnsiChar(Result), Len), Attr.Length - Len, $20);
            end
            else begin
              SetLengthA(Result, Len);
              Move(pOCIString^, PAnsiChar(Result)^, Len);
            end;
          end;
        end;
      end;
      dtWideString:
        Result := AnsiString(ReadAsWideString(Attr, Data, Ind));
      else
        raise EConvertError.Create(SCannotConvert + 'OCIString');
    end;
  end;
end;

procedure TOraObject.WriteAsAnsiString(Attr: TAttribute; Data: IntPtr; const Value: AnsiString);
begin
  WriteAsAnsiString(Attr, Data, Marshal.StringToHGlobalAnsi(Value), Length(Value));
end;

procedure TOraObject.WriteAsAnsiString(Attr: TAttribute; Data: IntPtr; ValuePtr: IntPtr; ValueLen: Word);
var
  Res: integer;
begin
  case Attr.DataType of
    dtString: begin
      Assert(Data <> nil);
      if not OCISvcCtx.Home.Direct then
        Res := OCI8.OCIStringResize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ValueLen + 1, Data) // bug in OCI 8.1
      else
        Res := 0;
      if Res = 0 then
        Res := OCI8.OCIStringAssignText(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ValuePtr, ValueLen, Data);
      FreeString(ValuePtr);
      Check(Res);
    end;
    dtWideString:
      WriteAsWideString(Attr, Data, WideString(Marshal.PtrToStringAnsi(ValuePtr, ValueLen)));
  else
    raise EConvertError.Create(SCannotConvert + 'OCIString');
  end;
end;

function TOraObject.GetAsWideString(const Name: string): WideString;
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr);
  Result := ReadAsWideString(Attr, Data, Ind);
end;

procedure TOraObject.SetAsWideString(const Name: string; const Value: WideString);
var
  Attr: TAttribute;
  Data: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  CheckType;
  Attr := GetAttribute(Name, Data, Ind, IndPtr, True, True);
  WriteAsWideString(Attr, Data, Value);
end;

function TOraObject.ReadAsWideString(Attr: TAttribute; Data: IntPtr; Ind: OCIInd): WideString;
var
  i: Integer;
  lOCIString: pOCIString;
  pOCIString: IntPtr;
  Len: Integer;
begin
  Result := '';

  if not ReadAttrIsNull(Attr, Ind) then begin
    case Attr.DataType of
      dtString:
        Result := WideString(ReadAsAnsiString(Attr, Data, Ind));
      dtWideString: begin
        lOCIString := ReadAsOCIString(Attr, Data);
        if lOCIString <> nil then begin
          pOCIString := OCI8.OCIStringPtr(OCISvcCtx.hOCIEnv, lOCIString);
          if pOCIString <> nil then begin
            Len := OCI8.OCIStringSize(OCISvcCtx.hOCIEnv, lOCIString) shr 1;

            if Len > Attr.Length then
              Len := Attr.Length;

            if Attr.Fixed and not FTrimFixedChar  then begin
              SetLength(Result, Attr.Length);
              Move(pOCIString^, Result[1], Len * 2);
              for i := Len + 1 to Attr.Length do
                Result[i] := ' ';
            end
            else begin
              SetLength(Result, Len);
              Move(pOCIString^, Result[1], Len * 2);
            end;
          end;
        end;
      end;
      else
        raise EConvertError.Create(SCannotConvert + 'OCIString');
    end;
  end;
end;

procedure TOraObject.WriteAsWideString(Attr: TAttribute; Data: IntPtr; const Value: WideString);
begin
  WriteAsWideString(Attr, Data, Marshal.StringToHGlobalUni(Value), Length(Value));
end;

procedure TOraObject.WriteAsWideString(Attr: TAttribute; Data: IntPtr; ValuePtr: IntPtr; ValueLen: Word);
var
  Res: integer;
begin
  case Attr.DataType of
    dtWideString: begin
      Res := OCI8.OCIStringAssignText(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ValuePtr, ValueLen * 2, Data);
      FreeString(ValuePtr);
      Check(Res);
    end;
    dtString:
      WriteAsAnsiString(Attr, Data, AnsiString(Marshal.PtrToStringUni(ValuePtr, ValueLen)));
  else
    raise EConvertError.Create(SCannotConvert + 'OCIString');
  end;
end;

function TOraObject.GetChildObject(Attr: TAttribute; ArrayIndex: integer = -1): TSharedObject;
var
  Index: integer;
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  if ArrayIndex = -1 then
    Index := Attr.AttributeNo - 1
  else
    Index := ArrayIndex;

  Result := GetAttributeObject(Index);
  if Result = nil then begin
    GetAttribute(Attr, ArrayIndex, Value, Ind, IndPtr);
    Result := ReadChildObject(Self, Attr, ArrayIndex, Value, Ind, IndPtr);
    if Result <> nil then
      SetAttributeObject(Index, Result);
  end;
end;

function TOraObject.ReadChildObject(Parent: TOraObject; Attr: TAttribute; ArrayIndex: Integer; Value: IntPtr; Ind: OCIInd; IndPtr: IntPtr): TSharedObject;
begin
  case Attr.DataType of
    dtObject, dtReference, dtArray, dtTable, dtAnyData, dtXML: begin
      case Attr.DataType of
        dtObject:
          Result := TOraObject.Create(TOraType(Attr.ObjectType));
        dtReference: begin
          Result := TOraRef.Create(TOraType(Attr.ObjectType));
          try
            if Value = nil then
              TOraRef(Result).OCIRef := nil
            else
              TOraRef(Result).OCIRef := Marshal.ReadIntPtr(Value);

            if not TOraRef(Result).IsNull then
              TOraRef(Result).Pin;  // ??? optim
          except
            Result.Free;
            raise;
          end;
        end;
        dtArray:
          Result := TOraArray.Create(TOraType(Attr.ObjectType));
        dtTable:
          Result := TOraNestTable.Create(TOraType(Attr.ObjectType));
        dtAnyData: begin
          Result := TOraAnyData.Create(FOCISvcCtx, TOraType(Attr.ObjectType));
          try
            TOraXML(Result).FInstance := Marshal.ReadIntPtr(Value);
            TOraXML(Result).FIndicator := IndPtr;
          except
            Result.Free;
            raise;
          end;
        end;
        dtXML: begin
          Result := TOraXML.Create(FOCISvcCtx, TOraType(Attr.ObjectType));
          try
            TOraXML(Result).FInstance := Marshal.ReadIntPtr(Value);
            TOraXML(Result).FIndicator := IndPtr;
            TOraXML(Result).ReadXML;
          except
            Result.Free;
            raise;
          end;
        end;
        else
          raise EConvertError.Create(SCannotConvert + 'Object');
      end;

      TOraObject(Result).OCISvcCtx := FOCISvcCtx;

      if Parent <> nil then begin
        TOraObject(Result).FOwner := Parent;
        if Attr.DataType = dtObject then
          WriteAttrIsNull(Attr, IndPtr, False);
      end
      else begin
        TOraObject(Result).FInstance := Marshal.ReadIntPtr(Value);
        TOraObject(Result).FIndicator := IndPtr;
      end;
    end;
    dtOraBlob, dtOraClob, dtWideOraClob: begin
      case Attr.DataType of
        dtBlob, dtOraBlob: begin
          Result := TOraLob.Create(FOCISvcCtx);
          TOraLob(Result).LobType := ltBlob;
        end;
        dtOraClob, dtWideOraClob: begin
          Result := TOraLob.Create(FOCISvcCtx);
          if Attr.SubDataType = dtNClob then
            TOraLob(Result).LobType := ltNClob
          else
            TOraLob(Result).LobType := ltClob;
          TOraLob(Result).IsUnicode := True;
        end;
        dtNClob: begin
          Result := TOraLob.Create(FOCISvcCtx);
          TOraLob(Result).LobType := ltNClob;
          TOraLob(Result).IsUnicode := True;
        end;
      else
        raise EConvertError.Create(SCannotConvert + 'Lob');
      end;

      if Ind = OCI_IND_NOTNULL then begin
        TOraLob(Result).OCILobLocator := Marshal.ReadIntPtr(Value);
        TOraLob(Result).ReadLob;
      end;
    end;
    dtBFILE, dtCFILE: begin
      Result := TOraFile.Create(FOCISvcCtx);
      try
        TOraFile(Result).OCILobLocator := Marshal.ReadIntPtr(Value);
        if Ind = OCI_IND_NOTNULL then begin
          TOraFile(Result).Open;
          TOraFile(Result).ReadLob;
          TOraFile(Result).Close;
        end;
      except
        Result.Free;
        raise;
      end;
    end;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
      Result := TOraTimeStamp.Create(FOCISvcCtx, Attr.DataType);
      if Attr.DataType = dtTimeStampLTZ then
        TOraTimeStamp(Result).DescriptorType := OCI_DTYPE_TIMESTAMP_LTZ;
      if Pointer(Value^) <> nil then
        TOraTimeStamp(Result).OCIDateTime := Pointer(Value^)
      else if OCISvcCtx.Home.OCIVersion >= 11000 then
        // may be nil for dtTimeStampLTZ
        // but in Oracle client lower that 11 version it can raise AV
        Pointer(Value^) := TOraTimeStamp(Result).OCIDateTime
      else
        TOraTimeStamp(Result).OCIDateTime := Pointer(Value^);
      TOraTimeStamp(Result).IsNull := Ind <> OCI_IND_NOTNULL;
    end;
    dtIntervalYM, dtIntervalDS: begin
      Result := TOraInterval.Create(FOCISvcCtx, Attr.DataType);
      TOraInterval(Result).OCIInterval := Marshal.ReadIntPtr(Value);
      TOraInterval(Result).IsNull := Ind <> OCI_IND_NOTNULL;
    end;
  else
    Result := nil;
  end;
end;

function TOraObject.GetAsObject(const Name: string): TOraObject;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraObject then
    Result := TOraObject(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Object');
end;

function TOraObject.ReadAsObject(AttrChain: TAttributeChain): TOraObject;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(AttrChain);
  if Res is TOraObject then
    Result := TOraObject(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Object');
end;

function TOraObject.GetAsRef(const Name: string): TOraRef;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraRef then
    Result := TOraRef(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Reference');
end;

function TOraObject.ReadAsRef(AttrChain: TAttributeChain): TOraRef;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(AttrChain);
  if Res is TOraRef then
    Result := TOraRef(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Reference');
end;

function TOraObject.GetAsArray(const Name: string): TOraArray;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraArray then
    Result := TOraArray(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Array');
end;

function TOraObject.ReadAsArray(AttrChain: TAttributeChain): TOraArray;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(AttrChain);
  if Res is TOraArray then
    Result := TOraArray(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Array');
end;

function TOraObject.GetAsLob(const Name: string): TOraLob;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraLob then
    Result := TOraLob(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Lob');
end;

function TOraObject.ReadAsLob(AttrChain: TAttributeChain): TOraLob;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(AttrChain);
  if Res is TOraLob then
    Result := TOraLob(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Lob');
end;

function TOraObject.GetAsOraTimeStamp(const Name: string): TOraTimeStamp;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraTimeStamp then
    Result := TOraTimeStamp(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'TimeStamp');
end;

function TOraObject.ReadAsOraTimeStamp(AttrChain: TAttributeChain): TOraTimeStamp;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(AttrChain);
  if Res is TOraTimeStamp then
    Result := TOraTimeStamp(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'TimeStamp');
end;

function TOraObject.GetAsOraInterval(const Name: string): TOraInterval;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraInterval then
    Result := TOraInterval(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Interval');
end;

function TOraObject.ReadAsOraInterval(AttrChain: TAttributeChain): TOraInterval;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(AttrChain);
  if Res is TOraInterval then
    Result := TOraInterval(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Interval');
end;

procedure TOraObject.SetInstance(Value: IntPtr);
begin
  if Value <> FInstance then begin
    FreeObject;
    AssignInstance(Value);
  end;
end;

function TOraObject.GetFInstance: IntPtr;
begin
  Result := Marshal.ReadIntPtr(FInstancePtr);
end;

procedure TOraObject.SetFInstance(Value: IntPtr);
begin
  Marshal.WriteIntPtr(FInstancePtr, Value);
end;

function TOraObject.GetFIndicator: IntPtr;
begin
  Result := Marshal.ReadIntPtr(FIndicatorPtr);
end;

procedure TOraObject.SetFIndicator(Value: IntPtr);
begin
  Marshal.WriteIntPtr(FIndicatorPtr, Value);
end;

function TOraObject.GetObjectData(OwnerType, InstanceType: TOraType; Instance: IntPtr): IntPtr;
begin
  if OCISvcCtx.Home.Direct and
     (InstanceType <> nil) and
     (InstanceType.FTypeCode = OCI_TYPECODE_OBJECT)
  then
    if OwnerType = nil then
      Result := OCI8.OCIObjectPtr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance)
    else if OwnerType.FTypeCode = OCI_TYPECODE_NAMEDCOLLECTION then
      Result := OCI8.OCIObjectPtr(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Marshal.ReadIntPtr(Instance))
    else
      Result := Instance
  else
    Result := Instance;
end;

procedure TOraObject.SetOCISvcCtx(Value: TOCISvcCtx);
begin
  if FOCISvcCtx <> Value then begin
    FOCISvcCtx := Value;
    if FOCISvcCtx <> nil then
      FOCI8 := FOCISvcCtx.OCI8
    else
      FOCI8 := nil;
  end;
end;

procedure TOraObject.SetObjectType(Value: TOraType);
begin
  if Value <> FObjectType then begin
    FreeObject;

    inherited SetObjectType(Value);

    FObjectType := Value;
    if Value <> nil then
      SetOCISvcCtx(Value.FOCISvcCtx);
  end;
end;

{ TOraRef }

constructor TOraRef.Create(ObjectType: TOraType);
begin
  inherited Create(ObjectType);

  FpOCIRef := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(FpOCIRef, nil);
end;

destructor TOraRef.Destroy;
begin
  if FOCISvcCtx.Home.Direct then
    if OCIRef <> nil then
      OCI8.OCIObjectFree(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, OCIRef, OCI_OBJECTFREE_FORCE);

  Marshal.FreeHGlobal(FpOCIRef);

  inherited;
end;

procedure TOraRef.Pin;
var
  Res: sword;
begin
  Assert(FInstance = nil);

  if IsNull{FOCIRef = nil} then
    RaiseError('OCIRef is not assigned');

  if FInstance = nil then begin
    if FOCISvcCtx.Home.Direct then
      Res := OCI8.OCIObjectPin2(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, FOCIRef, nil, OCI_PIN_ANY, OCI_DURATION_SESSION,
        OCI_LOCK_NONE, FInstancePtr)
    else
      Res := OCI8.OCIObjectPin(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FOCIRef, nil, OCI_PIN_ANY, OCI_DURATION_SESSION,
        OCI_LOCK_NONE, FInstancePtr);

    if Res <> OCI_SUCCESS then begin // WAR
      InvalidObject := True;
      Exit;
    end;
  end;

  if FIndicator = nil then
    Check(OCI8.OCIObjectGetInd(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, FIndicatorPtr));
end;

procedure TOraRef.Unpin;
begin
  inherited;

  FreeObject;
end;

procedure TOraRef.Assign(Source: TOraObject);
var
  Val: pOCIRef;
begin
   if Source is TOraRef then begin
     if TOraRef(Source).RefIsNull then
       Clear
     else begin
       Val := nil;
       Check(OCI8.OCIRefAssign(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, TOraRef(Source).OCIRef, Val));
       FOCIRef := Val;
     end;
   end
   else
     inherited;
end;

function TOraRef.RefIsNull: boolean;
begin
  Result := OCI8.OCIRefIsNull(OCISvcCtx.hOCIEnv, FOCIRef) = 1;
end;

procedure TOraRef.Clear;
begin
  if FOCIRef <> nil then begin
    Check(OCI8.OCIRefClear(OCISvcCtx.hOCIEnv, FOCIRef));
    FOCIRef := nil;
    InvalidObject := False;
  end;
end;

function TOraRef.GethOCIRef: pOCIRef;
begin
  Result := Marshal.ReadIntPtr(FpOCIRef);
end;

procedure TOraRef.SethOCIRef(Value: pOCIRef);
begin
  Marshal.WriteIntPtr(FpOCIRef, Value);
end;

procedure TOraRef.SetOCIRef(Value: pOCIRef);
begin
  if Value <> FOCIRef then begin
    FreeObject;
    FOCIRef := Value;
    InvalidObject := False;
  end;
end;

function TOraRef.GetOCIRefPtr: ppOCIRef;
begin
  Result := FpOCIRef;
end;

function TOraRef.GetIsNull: boolean;
begin
  if FOCIRef <> nil then begin
    Result := RefIsNull or InvalidObject;
  end
  else
    Result := True;
end;

procedure TOraRef.SetIsNull(Value: boolean);
begin
  if Value then
    Clear;
end;

function TOraRef.GetAsHex: string;
var
  Len: cardinal;
  Ptr: IntPtr;
begin
  if FOCIRef <> nil then begin
    Len := 1000;
    Ptr := Marshal.AllocHGlobal(Len);
    try
      Check(OCI8.OCIRefToHex(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FOCIRef, Ptr, Len));
      Result := string(Marshal.PtrToStringAnsi(Ptr, Len));
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end
  else
    Result := '';
end;

{ TOraXML }

constructor TOraXML.Create(AOCISvcCtx: TOCISvcCtx; AObjectType: TOraType = nil);
begin
  if (AObjectType <> nil) and (AObjectType.DataType <> dtXML) then
    RaiseError('Type of object must be XML');

  inherited Create(AObjectType);

  SetOCISvcCtx(AOCISvcCtx);

  phOCIDescriptor := nil;
  LocalIndicator := Marshal.AllocHGlobal(sizeof(Word));
  Marshal.WriteInt16(LocalIndicator, OCI_IND_NULL);
  LocalIndicatorPtr := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(LocalIndicatorPtr, LocalIndicator);
end;

destructor TOraXML.Destroy;
begin
  Marshal.FreeHGlobal(LocalIndicator);
  Marshal.FreeHGlobal(LocalIndicatorPtr);

  inherited;
end;

function TOraXML.ObjectIsNull: boolean;
begin
  Result := (Marshal.ReadInt16(LocalIndicator) <> OCI_IND_NOTNULL) and
            (inherited GetIsNull);
end;

procedure TOraXML.CheckType;
var
  ObjType: TOraType;
begin
  if FObjectType = nil then begin
    ObjType := nil;
    if ObjectTypes <> nil then begin
      ObjType := ObjectTypes.FindType(FOCISvcCtx, 'SYS.XMLTYPE');
      SetObjectType(ObjType);
    end;
    if ObjType = nil then begin
      ObjType := TOraType.Create(FOCISvcCtx, 'SYS.XMLTYPE');
      SetObjectType(ObjType);
      ObjType.Release;
    end;
  end
  else if FObjectType.DataType <> dtXML  then
    RaiseError('Type of object must be XML');
end;

function TOraXML.GetIsNull: boolean;
begin
  Result := (Marshal.ReadInt16(GetIndicatorPtr) = OCI_IND_NULL) or
            (Length(CacheValue) = 0);
end;

procedure TOraXML.SetIsNull(Value: boolean);
begin
  FreeObject;
  Marshal.WriteInt16(LocalIndicator, OCI_IND_NULL);
  SetLength(CacheValue, 0);
end;

function TOraXML.GetIndicatorPtr: IntPtr;
begin
  if Marshal.ReadInt16(LocalIndicator) = OCI_IND_NOTNULL then
    Result := LocalIndicatorPtr
  else
    Result := inherited GetIndicatorPtr;
end;

procedure TOraXML.AssignInstance(Value: IntPtr);
begin
  CheckType;
  inherited;
  ReadLocalIndicator;
end;

procedure TOraXML.ReadLocalIndicator;
begin
  if (FIndicator = nil) and (FInstance <> nil) then begin
    Check(OCI8.OCIObjectGetInd(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, FIndicatorPtr));
    if OCISvcCtx.Home.OCIVersion >= 10102 then
      Marshal.WriteInt16(LocalIndicator, OCI_IND_NOTNULL);
  end;
end;

procedure TOraXML.CreateXMLStream;
begin
  if phOCIDescriptor = nil then
  begin
    CheckAlloc;
    Check(OCI8.OCIDescriptorAlloc(OCISvcCtx.hOCIEnv, phOCIDescriptor, OCI_DTYPE_XML_STREAM, 0, nil));
    try
      Check(OCI8.OCIPStreamFromXMLType(OCISvcCtx.hOCIError, phOCIDescriptor, FInstance, 0));
    except
      Check(OCI8.OCIDescriptorFree(phOCIDescriptor, OCI_DTYPE_XML_STREAM));
      phOCIDescriptor := nil;
      raise;
    end;
  end;
end;

procedure TOraXML.FreeXMLStream;
begin
  if phOCIDescriptor <> nil then
  begin
    Check(OCI8.OCIPStreamClose(OCISvcCtx.hOCIError, phOCIDescriptor));
    Check(OCI8.OCIDescriptorFree(phOCIDescriptor, OCI_DTYPE_XML_STREAM));
    phOCIDescriptor := nil;
  end;
end;

function TOraXML.GetAsString: string;
begin
  if CacheValue <> nil then
    Result := Encoding.UTF8.GetString(CacheValue)
  else
    Result := '';
end;

procedure TOraXML.SetAsString(const Value: string);
begin
  CheckType;
  FreeObject;
  CacheValue := Encoding.UTF8.GetBytes(AnsiString(Value));
end;

function TOraXML.GetAsWideString: WideString;
begin
  if CacheValue <> nil then
    Result := Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(CacheValue)
  else
    Result := '';
end;

procedure TOraXML.SetAsWideString(const Value: WideString);
begin
  CheckType;
  FreeObject;
  CacheValue := Encoding.UTF8.GetBytes(Value);
end;

procedure TOraXML.AllocObject;
var
  OraLob: TOraLob;
  OCIStr: pOCIString;
  lOCIStr: ppOCIString;
  Instance: IntPtr;
  Buf: TBytes;
  Len: Integer;
begin
  CheckType;
  CheckSession;

  if FOwner = nil then begin
    if FInstance = nil then begin
      if not IsNull and (Length(CacheValue) > 0) then begin
      {$IFNDEF VER9P}
        SetLength(Buf, 0); // anti-warning
      {$ENDIF}
        if OCISvcCtx.Environment.UnicodeEnv then
          Buf := Encoding.Convert(Encoding.UTF8, Encoding.Unicode, CacheValue)
        else
          Buf := Encoding.Convert(Encoding.UTF8, Encoding.Default, CacheValue);
        Len := Length(Buf);
        if Len > 4000 then begin
          OraLob := TOraLob.Create(FOCISvcCtx);
          try
            OraLob.CreateTemporary(ltClob);
            OraLob.IsUnicode := OCISvcCtx.Environment.UnicodeEnv;
            OraLob.Write(0, Len, @Buf[0]);
            OraLob.WriteLob;

            if AccessBySQL then
              WriteXMLFromClobBySQL(OraLob)
            else begin
              Check(OCI8.OCIXMLTypeCreateFromSrc(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, OCI_DURATION_SESSION,
                OCI_XMLTYPE_CREATE_CLOB, OraLob.OCILobLocator, OCI_IND_NOTNULL, Instance));
              FInstance := Instance;
            end;
          finally
            OraLob.Free;
          end;
        end
        else begin
          if AccessBySQL then
            WriteXMLFromStringBySQL(@Buf[0], Len)
          else begin
            Check(OCI8.OCIObjectNew(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, OCISvcCtx.hOCISvcCtx,
              OCI_TYPECODE_VARCHAR, nil, nil, OCI_DURATION_SESSION, 1, OCIStr));
            try
              lOCIStr := @OCIStr;
              Check(OCI8.OCIStringAssignText(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, @Buf[0], Len, lOCIStr));
              Check(OCI8.OCIXMLTypeCreateFromSrc(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, OCI_DURATION_SESSION,
                OCI_XMLTYPE_CREATE_OCISTRING, Marshal.ReadIntPtr(lOCIStr), OCI_IND_NOTNULL, Instance));
              FInstance := Instance;
            finally
              Check(OCI8.OCIObjectFree(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, OCIStr, OCI_OBJECTFREE_FORCE));
            end;
          end;
        end;

        ReadLocalIndicator;
      end
      else begin
        Check(OCI8.OCIXMLTypeNew(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, OCI_DURATION_SESSION,
          '', 0, '', 0, Instance));
        FInstance := Instance;
        if (FIndicator = nil) and (FInstance <> nil) then
          Check(OCI8.OCIObjectGetInd(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, FInstance, FIndicatorPtr));
      end;

      FNativeInstance := True;
    end;
  end
  else
    FOwner.ChildAlloc(Self);
end;

procedure TOraXML.AllocObject(const TypeName: string);
begin
  if OCISQLInfo.NormalizeName(TypeName) <> 'SYS.XMLTYPE' then
    RaiseError('Type of object must be XML');

  inherited AllocObject(TypeName);
end;

procedure TOraXML.AllocObject(AOCISvcCtx: TOCISvcCtx; const TypeName: string);
begin
  if OCISQLInfo.NormalizeName(TypeName) <> 'SYS.XMLTYPE' then
    RaiseError('Type of object must be XML');

  inherited AllocObject(AOCISvcCtx, TypeName);
end;

procedure TOraXML.AllocObject(AOCISvcCtx: TOCISvcCtx; AOraLob: TOraLob);
var
  Instance: IntPtr;
begin
  FreeObject;

  SetOCISvcCtx(AOCISvcCtx);

  CheckType;
  CheckSession;

  if FOwner = nil then begin
    if AccessBySQL then
      WriteXMLFromClobBySQL(AOraLob)
    else begin
      Check(OCI8.OCIXMLTypeCreateFromSrc(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, OCI_DURATION_SESSION,
        OCI_XMLTYPE_CREATE_CLOB, AOraLob.OCILobLocator, OCI_IND_NOTNULL, Instance));
      FInstance := Instance;
    end;

    ReadLocalIndicator;

    FNativeInstance := True;

    // Read XML to CashedValue if OraXML is inited by OraLob
    ReadXML;
  end
  else
    FOwner.ChildAlloc(Self);
end;

procedure TOraXML.FreeObject(FreeChild: boolean = True);
begin
  FreeXMLStream;
  SetLength(CacheValue, 0);
  inherited FreeObject(FreeChild);
end;

procedure TOraXML.Assign(Source: TOraObject);
begin
  FreeObject;
  Self.AsString := TOraXML(Source).AsString;
end;

procedure TOraXML.StartRead;
begin
  FreeXMLStream;
  CreateXMLStream;
end;

function TOraXML.Read(Count: cardinal; Dest: IntPtr): cardinal;
var
  Len: int64;
begin
  Result := 0;

  if ObjectIsNull then // to avoid error on OCI function call
    Exit;

  CreateXMLStream;
  Len := Count;
  Check(OCI8.OCIPStreamRead(OCISvcCtx.hOCIError, phOCIDescriptor, Dest, Len, 0));
  Result := Len;
end;

function TOraXML.AccessBySQL: boolean;
begin
  Result := not OCISvcCtx.Environment.Home.Direct and
            OCISvcCtx.Environment.UnicodeEnv;
end;

procedure TOraXML.WriteXMLFromStringBySQL(Buf: IntPtr; BufSize: Integer);
const
  SQL = 'DECLARE VAL VARCHAR2(4000) := :1; BEGIN :2 := XMLTYPE(TO_CLOB(VAL)); END;';
var
  strPtr: IntPtr;
  strSize: Integer;
  OCIStmt: pOCIStmt;
  Bind: pOCIBind;
  CharsetId: Integer;
  Ind1, Ind2: SmallInt;
begin
  Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, OCIStmt, OCI_HTYPE_STMT, 0, nil));
  try
    strPtr := StringToHGlobalOCI(SQL, strSize, OCISvcCtx.UnicodeEnv);
    try
      Check(OCI8.OCIStmtPrepare(OCIStmt, OCISvcCtx.hOCIError, strPtr, strSize, OCI_NTV_SYNTAX, OCI_DEFAULT));
    finally
      FreeStringOCI(strPtr, OCISvcCtx.UnicodeEnv);
    end;

    strPtr := StringToHGlobalOCI('1', strSize, OCISvcCtx.UnicodeEnv);
    try
      Ind1 := OCI_IND_NOTNULL;
      Check(OCI8.OCIBindByName(OCIStmt, Bind, OCISvcCtx.hOCIError, strPtr, strSize,
        Buf, BufSize, SQLT_CHR, @Ind1, nil, nil, 0, nil, OCI_DEFAULT));
    finally
      FreeStringOCI(strPtr, OCISvcCtx.UnicodeEnv);
    end;

    if OCISvcCtx.UnicodeEnv then begin
      CharsetId := OCI_UTF16ID;
      Check(OCI8.OCIAttrSet2(Bind, OCI_HTYPE_BIND, CharsetId, 0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));
    end;

    strPtr := StringToHGlobalOCI('2', strSize, OCISvcCtx.UnicodeEnv);
    try
      Ind2 := OCI_IND_NOTNULL;
      Check(OCI8.OCIBindByName(OCIStmt, Bind, OCISvcCtx.hOCIError, strPtr, strSize,
        nil, 0, SQLT_NTY, @Ind2, nil, nil, 0, nil, OCI_DEFAULT));
    finally
      FreeStringOCI(strPtr, OCISvcCtx.UnicodeEnv);
    end;

    Check(OCI8.OCIBindObject(Bind, OCISvcCtx.hOCIError, Self.ObjectType.TDO, Self.InstancePtr, nil, Self.IndicatorPtr, nil));

    Check(OCI8.OCIStmtExecute(OCISvcCtx.hOCISvcCtx, OCIStmt, OCISvcCtx.hOCIError, 1, 0, nil, nil, OCI_DEFAULT));

  finally
    OCI8.OCIHandleFree(OCIStmt, OCI_HTYPE_STMT);
  end;
end;

procedure TOraXML.WriteXMLFromClobBySQL(OraLob: TOraLob);
const
  SQL = 'DECLARE VAL CLOB := :1; BEGIN :2 := XMLTYPE(VAL); END;';
var
  strPtr: IntPtr;
  strSize: Integer;
  OCIStmt: pOCIStmt;
  Bind: pOCIBind;
  CharsetId: Integer;
  Ind1, Ind2: SmallInt;
begin
  Check(OCI8.OCIHandleAlloc(OCISvcCtx.hOCIEnv, OCIStmt, OCI_HTYPE_STMT, 0, nil));
  try
    strPtr := StringToHGlobalOCI(SQL, strSize, OCISvcCtx.UnicodeEnv);
    try
      Check(OCI8.OCIStmtPrepare(OCIStmt, OCISvcCtx.hOCIError, strPtr, strSize, OCI_NTV_SYNTAX, OCI_DEFAULT));
    finally
      FreeStringOCI(strPtr, OCISvcCtx.UnicodeEnv);
    end;

    strPtr := StringToHGlobalOCI('1', strSize, OCISvcCtx.UnicodeEnv);
    try
      Ind1 := OCI_IND_NOTNULL;
      Check(OCI8.OCIBindByName(OCIStmt, Bind, OCISvcCtx.hOCIError, strPtr, strSize,
        OraLob.OCILobLocatorPtr, OraLob.Size, SQLT_CLOB, @Ind1, nil, nil, 0, nil, OCI_DEFAULT));
    finally
      FreeStringOCI(strPtr, OCISvcCtx.UnicodeEnv);
    end;

    if OCISvcCtx.UnicodeEnv then begin
      CharsetId := OCI_UTF16ID;
      Check(OCI8.OCIAttrSet2(Bind, OCI_HTYPE_BIND, CharsetId, 0, OCI_ATTR_CHARSET_ID, OCISvcCtx.hOCIError));
    end;

    strPtr := StringToHGlobalOCI('2', strSize, OCISvcCtx.UnicodeEnv);
    try
      Ind2 := OCI_IND_NOTNULL;
      Check(OCI8.OCIBindByName(OCIStmt, Bind, OCISvcCtx.hOCIError, strPtr, strSize,
        nil, 0, SQLT_NTY, @Ind2, nil, nil, 0, nil, OCI_DEFAULT));
    finally
      FreeStringOCI(strPtr, OCISvcCtx.UnicodeEnv);
    end;

    Check(OCI8.OCIBindObject(Bind, OCISvcCtx.hOCIError, Self.ObjectType.TDO, Self.InstancePtr, nil, Self.IndicatorPtr, nil));

    Check(OCI8.OCIStmtExecute(OCISvcCtx.hOCISvcCtx, OCIStmt, OCISvcCtx.hOCIError, 1, 0, nil, nil, OCI_DEFAULT));

  finally
    OCI8.OCIHandleFree(OCIStmt, OCI_HTYPE_STMT);
  end;
end;

procedure TOraXML.ReadXML;
var
  TotalReaded, Readed: integer;
  Buffer: TBytes;
begin
  SetLength(CacheValue, 0);

  if ObjectIsNull then
    Exit;

  StartRead;

  TotalReaded := 0;
  repeat
    SetLength(Buffer, TotalReaded + DefaultPieceSize);
    Readed := Read(DefaultPieceSize, @Buffer[TotalReaded]);
    Inc(TotalReaded, Readed);
  until Readed = 0;

  if not OCISvcCtx.Environment.UnicodeEnv then
    CacheValue := Encoding.Convert(Encoding.Default, Encoding.UTF8, Buffer, 0, TotalReaded)
  else if OCISvcCtx.Home.Direct then
    CacheValue := Encoding.Convert(Encoding.Unicode, Encoding.UTF8, Buffer, 0, TotalReaded)
  else begin
    if Length(Buffer) <> TotalReaded then
      SetLength(Buffer, TotalReaded);
    CacheValue := Buffer;
  end;

  FreeXMLStream;
end;

procedure TOraXML.LoadFromStream(Stream: TStream);
begin
  CheckType;
  FreeObject;

  if Stream.Size > 0 then begin
    SetLength(CacheValue, Stream.Size);
    Stream.Seek(LongInt(0), soFromBeginning);
    Stream.ReadBuffer(CacheValue[0], Stream.Size);
  end;
end;

procedure TOraXML.SaveToStream(Stream: TStream);
begin
  if Length(CacheValue) > 0 then
    Stream.WriteBuffer(CacheValue[0], Length(CacheValue));
end;

procedure TOraXML.Extract(RetDoc: TOraXML; XPathExpr: string; NSmap: string);
var
  Instance: IntPtr;
begin
  if RetDoc = nil then
    Exit;
  AllocObject;
  RetDoc.FreeObject;
  RetDoc.CheckType;
  if IsNull then begin
    RetDoc.SetIsNull(True);
    exit;
  end;
  Check(OCI8.OCIXMLTypeExtract(OCISvcCtx.hOCIError,  FInstance, OCI_DURATION_SESSION,
    PAnsiChar(AnsiString(XPathExpr)), Length(XPathExpr),
    PAnsiChar(AnsiString(NSmap)), length(NSmap),
    Instance));
  RetDoc.FInstance := Instance;
  if (RetDoc.FIndicator = nil) and (RetDoc.FInstance <> nil) then begin
    Check(OCI8.OCIObjectGetInd(RetDoc.OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, RetDoc.FInstance, RetDoc.FIndicatorPtr));
    if OCISvcCtx.Home.OCIVersion >= 10102 then
      Marshal.WriteInt16(RetDoc.LocalIndicator, OCI_IND_NOTNULL);
  end;
end;

procedure TOraXML.Transform(XSLDoc: TOraXML; RetDoc: TOraXML);
var
  Instance: IntPtr;
begin
  if RetDoc = nil then
    Exit;
  AllocObject;
  RetDoc.FreeObject;
  RetDoc.CheckType;
  if IsNull then begin
    RetDoc.SetIsNull(True);
    exit;
  end;
  XSLDoc.AllocObject;
  Check(OCI8.OCIXMLTypeTransform(OCISvcCtx.hOCIError, OCI_DURATION_SESSION, FInstance,
    XSLDoc.FInstance, Instance));
  RetDoc.FInstance := Instance;
  if (RetDoc.FIndicator = nil) and (RetDoc.FInstance <> nil) then begin
    Check(OCI8.OCIObjectGetInd(RetDoc.OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, RetDoc.FInstance, RetDoc.FIndicatorPtr));
    if OCISvcCtx.Home.OCIVersion >= 10102 then
      Marshal.WriteInt16(RetDoc.LocalIndicator, OCI_IND_NOTNULL);
  end;
end;

function TOraXML.Exists(const XPathExpr: string; const NSmap: string): boolean;
var
  Res: Cardinal;
begin
  if (OCISvcCtx.Home.OCIVersion = 9201) or (OCISvcCtx.Home.OCIVersion >= 10102) then
    RaiseError('Function not supported');
  AllocObject;
  Check(OCI8.OCIXMLTypeExists(OCISvcCtx.hOCIError, FInstance,
    PAnsiChar(AnsiString(XPathExpr)), Length(XPathExpr),
    PAnsiChar(AnsiString(NSmap)), length(NSmap),Res));
  Result := (Res <> 0);
end;

function TOraXML.Validate(const SchemaURL: string): boolean;
var
  Res: Cardinal;
begin
  AllocObject;
  Check(OCI8.OCIXMLTypeValidate(OCISvcCtx.hOCIError, FInstance, PAnsiChar(AnsiString(SchemaURL)),
    Length(SchemaURL), Res));
  Result := (Res <> 0);
end;

function TOraXML.IsSchemaBased: boolean;
var
  Res: Cardinal;
begin
  AllocObject;
  Check(OCI8.OCIXMLTypeIsSchemaBased(OCISvcCtx.hOCIError, FInstance, Res));
  Result := (Res <> 0);
end;

procedure TOraXML.GetSchema(SchemaDoc: TOraXML; var SchemaURL: string; var RootElem: string);
var
  pSchemaURL, pRootelem: IntPtr;
  SchemaURL_Len, Rootelem_Len: ub4;
  Instance: IntPtr;
begin
  AllocObject;
  SchemaDoc.FreeObject;
  SchemaDoc.CheckType;
  if IsNull then begin
    SchemaDoc.SetIsNull(True);
    exit;
  end;

  Check(OCI8.OCIXMLTypeGetSchema(OCISvcCtx.hOCIError, FInstance,
    Instance,
    pSchemaURL, schemaURL_Len,
    pRootelem, Rootelem_Len));
  SchemaDoc.FInstance := Instance;
  if (SchemaDoc.FIndicator = nil) and (SchemaDoc.FInstance <> nil) then begin
    Check(OCI8.OCIObjectGetInd(SchemaDoc.OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, SchemaDoc.FInstance, SchemaDoc.FIndicatorPtr));
    if OCISvcCtx.Home.OCIVersion >= 10102 then
      Marshal.WriteInt16(SchemaDoc.LocalIndicator, OCI_IND_NOTNULL);
  end;

  if schemaURL_Len > 0 then
    SchemaURL := string(Marshal.PtrToStringAnsi(pSchemaURL, schemaURL_Len))
  else
    SchemaURL := '';
  if Rootelem_Len > 0 then
    RootElem := string(Marshal.PtrToStringAnsi(pRootelem, Rootelem_Len))
  else
    RootElem := '';
end;

{ TOraAnyData }

constructor TOraAnyData.Create(AOCISvcCtx: TOCISvcCtx; AObjectType: TOraType = nil);
begin
  if (AObjectType <> nil) and (AObjectType.DataType <> dtAnyData) then
    RaiseError('Type of object must be AnyData');

  inherited Create(AObjectType);

  SetOCISvcCtx(AOCISvcCtx);

  FValueAttribute := nil;
  FValueData := nil;
  FValueIndicator := nil;
  FValueIndicatorPtr := @FValueIndicator;
end;

destructor TOraAnyData.Destroy;
begin
  ClearValue;

  inherited;
end;

function TOraAnyData.GetValueType: Word;
begin
  ReadAttribute;

  if IsNull then
    Result := dtUnknown
  else
    Result := FValueAttribute.DataType;
end;

procedure TOraAnyData.SetValueType(Value: Word);
begin
  if (FValueAttribute = nil) or (FValueAttribute.DataType <> value) then
    AllocAttribute(Value, FValueTypeCode);
end;

function TOraAnyData.GetAsDateTime: TDateTime;
begin
  ReadAttribute;

  Result := ReadAsDateTime(FValueAttribute, FValueData, OCIInd(FValueIndicatorPtr^));
end;

procedure TOraAnyData.SetAsDateTime(Value: TDateTime);
var
  TypeCode: OCITypeCode;
  Buf: IntPtr;
begin
  AllocAttribute(dtDateTime, TypeCode);
  try
    Buf := Marshal.AllocHGlobal(FValueAttribute.DataSize);
    try
      WriteAsDateTime(FValueAttribute, Buf, Value);
      WriteData(FValueAttribute, TypeCode, Buf);
    finally
      Marshal.FreeHGlobal(Buf);
    end;
  except
    FreeAndNil(FValueAttribute);
    raise;
  end;
end;

function TOraAnyData.GetAsFloat: double;
begin
  ReadAttribute;

  Result := ReadAsFloat(FValueAttribute, FValueData, OCIInd(FValueIndicatorPtr^));
end;

procedure TOraAnyData.SetAsFloat(Value: double);
var
  TypeCode: OCITypeCode;
  Buf: IntPtr;
begin
  AllocAttribute(dtFloat, 39, 39, TypeCode);
  try
    Buf := Marshal.AllocHGlobal(FValueAttribute.DataSize);
    try
      WriteAsFloat(FValueAttribute, Buf, Value);
      WriteData(FValueAttribute, TypeCode, Buf);
    finally
      Marshal.FreeHGlobal(Buf);
    end;
  except
    FreeAndNil(FValueAttribute);
    raise;
  end;
end;

function TOraAnyData.GetAsInteger: integer;
begin
  ReadAttribute;

  Result := ReadAsInteger(FValueAttribute, FValueData, OCIInd(FValueIndicatorPtr^));
end;

procedure TOraAnyData.SetAsInteger(Value: integer);
var
  TypeCode: OCITypeCode;
  Buf: IntPtr;
begin
  AllocAttribute(dtInteger, TypeCode);
  try
    Buf := Marshal.AllocHGlobal(FValueAttribute.DataSize);
    try
      WriteAsInteger(FValueAttribute, Buf, Value);
      WriteData(FValueAttribute, TypeCode, Buf);

    finally
      Marshal.FreeHGlobal(Buf);
    end;
  except
    FreeAndNil(FValueAttribute);
    raise;
  end;
end;

function TOraAnyData.GetAsLargeInt: int64;
begin
  ReadAttribute;

  Result := ReadAsLargeInt(FValueAttribute, FValueData, OCIInd(FValueIndicatorPtr^));
end;

procedure TOraAnyData.SetAsLargeInt(Value: int64);
var
  TypeCode: OCITypeCode;
  Buf: IntPtr;
begin
  AllocAttribute(dtLargeint, TypeCode);
  try
    Buf := Marshal.AllocHGlobal(FValueAttribute.DataSize);
    try
      WriteAsDateTime(FValueAttribute, Buf, Value);
      WriteData(FValueAttribute, TypeCode, Buf);
    finally
      Marshal.FreeHGlobal(Buf);
    end;
  except
    FreeAndNil(FValueAttribute);
    raise;
  end;
end;

function TOraAnyData.GetAsString: string;
begin
  ReadAttribute;

  Result := ReadAsString(FValueAttribute, @FValueData, OCIInd(FValueIndicatorPtr^));
end;

procedure TOraAnyData.SetAsString(const Value: string);
var
  TypeCode: OCITypeCode;
  Buf: IntPtr;
begin
  AllocAttribute(dtString, Length(Value), TypeCode);
  try
    Buf := Marshal.AllocHGlobal(FValueAttribute.DataSize);
    try
      FillChar(Buf, FValueAttribute.DataSize, 0);
      WriteAsString(FValueAttribute, Buf, Value);
      WriteData(FValueAttribute, TypeCode, Pointer(Buf^));
    finally
      Marshal.FreeHGlobal(Buf);
    end;
  except
    FreeAndNil(FValueAttribute);
    raise;
  end;
end;

function TOraAnyData.GetAsWideString: WideString;
begin
  ReadAttribute;

  Result := ReadAsWideString(FValueAttribute, @FValueData, OCIInd(FValueIndicatorPtr^));
end;

procedure TOraAnyData.SetAsWideString(const Value: WideString);
var
  TypeCode: OCITypeCode;
  Buf: IntPtr;
begin
  AllocAttribute(dtWideString, Length(Value), TypeCode);
  try
    Buf := Marshal.AllocHGlobal(FValueAttribute.DataSize);
    try
      WriteAsWideString(FValueAttribute, Buf, Value);
      WriteData(FValueAttribute, TypeCode, Pointer(Buf^));
    finally
      Marshal.FreeHGlobal(Buf);
    end;
  except
    FreeAndNil(FValueAttribute);
    raise;
  end;
end;

function TOraAnyData.GetAsObject: TOraObject;
begin
  ReadAttribute;

  if FValueObject is TOraObject then
    Result := TOraObject(FValueObject)
  else if (FValueObject = nil) and
          (FValueAttribute.DataType = dtObject)
  then begin
    if (Instance <> nil) and (FValueData <> nil) then
      FValueObject := ReadChildObject(nil, FValueAttribute, -1, @FValueData, OCIInd(FValueIndicator^), FValueIndicator)
    else begin
      FValueObject := TOraObject.Create(nil);
      TOraObject(FValueObject).SetOCISvcCtx(FOCISvcCtx);
    end;
    Result := FValueObject as TOraObject;
  end
  else
    raise EConvertError.Create(SCannotConvert + 'Object');
end;

function TOraAnyData.GetAsRef: TOraRef;
begin
  ReadAttribute;

  if FValueObject is TOraRef then
    Result := TOraRef(FValueObject)
  else if (FValueObject = nil) and
          (FValueAttribute.DataType = dtReference)
  then begin
    if (Instance <> nil) and (FValueData <> nil) then
      FValueObject := ReadChildObject(nil, FValueAttribute, -1, @FValueData, OCIInd(FValueIndicator^), FValueIndicator)
    else begin
      FValueObject := TOraRef.Create(nil);
      TOraRef(FValueObject).SetOCISvcCtx(FOCISvcCtx);
    end;
    Result := FValueObject as TOraRef;
  end
  else
    raise EConvertError.Create(SCannotConvert + 'Reference');
end;

function TOraAnyData.GetAsArray: TOraArray;
begin
  ReadAttribute;

  if FValueObject is TOraArray then
    Result := TOraArray(FValueObject)
  else if (FValueObject = nil) and
          (FValueAttribute.DataType = dtArray)
  then begin
    if (Instance <> nil) and (FValueData <> nil) then
      FValueObject := ReadChildObject(nil, FValueAttribute, -1, @FValueData, OCIInd(FValueIndicator^), FValueIndicator)
    else begin
      FValueObject := TOraArray.Create(nil);
      TOraArray(FValueObject).SetOCISvcCtx(FOCISvcCtx);
    end;
    Result := FValueObject as TOraArray;
  end
  else
    raise EConvertError.Create(SCannotConvert + 'Array');
end;

function TOraAnyData.GetAsLob: TOraLob;
begin
  ReadAttribute;

  if FValueObject is TOraLob then
    Result := TOraLob(FValueObject)
  else if (FValueObject = nil) and
          (FValueAttribute.DataType in [dtOraBlob, dtOraClob, dtNClob])
  then begin
    if (Instance <> nil) and (FValueData <> nil) then
      FValueObject := ReadChildObject(nil, FValueAttribute, -1, @FValueData, OCIInd(FValueIndicatorPtr^), FValueIndicatorPtr)
    else begin
      FValueObject := TOraLob.Create(FOCISvcCtx);
      if FValueAttribute.DataType = dtOraClob then
        TOraLob(FValueObject).LobType := ltClob
      else if FValueAttribute.DataType = dtNClob then
        TOraLob(FValueObject).LobType := ltNClob
      else
        TOraLob(FValueObject).LobType := ltBlob;
    end;
    Result := FValueObject as TOraLob;
  end
  else
    raise EConvertError.Create(SCannotConvert + 'Lob');
end;

function TOraAnyData.GetAsOraTimeStamp: TOraTimeStamp;
begin
  ReadAttribute;

  if FValueObject is TOraTimeStamp then
    Result := TOraTimeStamp(FValueObject)
  else if (FValueObject = nil) and
          (FValueAttribute.DataType in [dtSQLTimeStamp, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ])
  then begin
    if (Instance <> nil) and (FValueData <> nil) then
      FValueObject := ReadChildObject(nil, FValueAttribute, -1, @FValueData,  OCIInd(FValueIndicatorPtr^), FValueIndicatorPtr)
    else
      FValueObject := TOraTimeStamp.Create(FOCISvcCtx, FValueAttribute.DataType);
    Result := FValueObject as TOraTimeStamp;
  end
  else
    raise EConvertError.Create(SCannotConvert + 'TimeStamp');
end;

function TOraAnyData.GetAsOraInterval: TOraInterval;
begin
  ReadAttribute;

  if FValueObject is TOraInterval then
    Result := TOraInterval(FValueObject)
  else if (FValueObject = nil) and
          (FValueAttribute.DataType in [dtIntervalYM, dtIntervalDS])
  then begin
    if (Instance <> nil) and (FValueData <> nil) then
      FValueObject := ReadChildObject(nil, FValueAttribute, -1, @FValueData, OCIInd(FValueIndicatorPtr^), FValueIndicatorPtr)
    else
      FValueObject := TOraInterval.Create(FOCISvcCtx, FValueAttribute.DataType);
    Result := FValueObject as TOraInterval;
  end
  else
    raise EConvertError.Create(SCannotConvert + 'Interval');
end;

function TOraAnyData.GetAsVariant: Variant;
begin
  ReadAttribute;

  if IsNull then
    Result := null
  else
    case FValueAttribute.DataType of
      dtFixedChar,
      dtFixedNChar,
      dtString,
      dtExtString,
      dtNString:
        Result := GetAsString;
      dtFixedWideChar,
      dtFixedNWideChar,
      dtWideString,
      dtExtWideString,
      dtNWideString:
        Result := GetAsWideString;
      dtInt8,
      dtInt16,
      dtInt32:
        Result := GetAsInteger;
      dtInt64:
        Result := GetAsLargeInt;
      dtFloat:
        Result := GetAsFloat;
      dtDateTime:
        Result := GetAsDateTime;
      dtSQLTimeStamp,
      dtTimeStamp,
      dtTimeStampTZ,
      dtTimeStampLTZ:
        Result := GetAsOraTimeStamp.AsDateTime;
      else
        raise EConvertError.Create(SCannotConvert + 'Variant');
    end;
end;

procedure TOraAnyData.SetAsVariant(const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    SetIsNull(True)
  else
    case TVarData(Value).VType of
      varString:
        SetAsString(Value);
    {$IFDEF VER12P}
      varUString,
    {$ENDIF}
      varOleStr:
        SetAsWideString(Value);
      varShortInt,
      varSmallint,
      varInteger,
      varByte,
      varWord:
        SetAsInteger(Value);
      varLongWord,
    {$IFDEF VER12P}
      varUInt64,
    {$ENDIF}
      varInt64:
        SetAsLargeInt(Value);
      varSingle,
      varDouble,
      varCurrency:
        SetAsFloat(Value);
      varDate:
        SetAsDateTime(value);
      else
      {$IFNDEF FPC}
        if VarIsSQLTimeStamp(Value) then begin
          ValueType := dtTimeStamp;
          GetAsOraTimeStamp.AsTimeStamp := VarToSQLTimeStamp(Value);
        end
        else
      {$ENDIF}
          raise EConvertError.Create(SCannotConvertType);
    end;
end;

procedure TOraAnyData.CheckType;
var
  ObjType: TOraType;
begin
  if FObjectType = nil then begin
    ObjType := nil;
    if ObjectTypes <> nil then begin
      ObjType := ObjectTypes.FindType(FOCISvcCtx, 'SYS.ANYDATA');
      SetObjectType(ObjType);
    end;
    if ObjType = nil then begin
      ObjType := TOraType.Create(FOCISvcCtx, 'SYS.ANYDATA');
      SetObjectType(ObjType);
      ObjType.Release;
    end;
  end
  else if FObjectType.DataType <> dtAnyData  then
    RaiseError('Type of object must be ANYDATA');
end;

function TOraAnyData.GetIsNull: boolean;
begin
  Result := OCIInd(Pointer(GetIndicatorPtr^)^) = OCI_IND_NULL;
end;

procedure TOraAnyData.SetIsNull(Value: boolean);
begin
  ClearValue;
end;

function TOraAnyData.GetIndicatorPtr: IntPtr;
begin
  if FValueObject <> nil then
    case FValueAttribute.DataType of
      dtOraBlob,
      dtOraClob,
      dtWideOraClob:
        if TOraLob(FValueObject).IsNull then
          Result := @NullInd
        else
          Result := @NotNullInd;
      dtTimeStamp,
      dtTimeStampTZ,
      dtTimeStampLTZ:
        if TOraTimeStamp(FValueObject).IsNull then
          Result := @NullInd
        else
          Result := @NotNullInd;
      dtIntervalYM,
      dtIntervalDS:
        if TOraInterval(FValueObject).IsNull then
          Result := @NullInd
        else
          Result := @NotNullInd;
      dtObject,
      dtReference,
      dtArray,
      dtTable,
      dtAnyData,
      dtXML:
        if TOraObject(FValueObject).IsNull then
          Result := @NullInd
        else
          Result := @NotNullInd;
      else
        if Instance = nil then
          Result := @NullInd
        else
          Result := @NotNullInd;
    end
  else if Instance = nil then
    Result := @NullInd
  else
    Result := @NotNullInd;
end;

procedure TOraAnyData.ClearValue;
begin
  FValueData := nil;
  FValueTypeCode := 0;
  FValueIndicator := nil;
  FValueIndicatorPtr := @FValueIndicator;

  if FValueAttribute <> nil then
    FreeAndNil(FValueAttribute);

  if FValueObject <> nil then
    FreeAndNil(FValueObject);
end;

function TOraAnyData.GetTypeCode(DataType: Word): OCITypeCode;
begin
  case DataType of
    dtFixedChar,
    dtFixedWideChar:
      Result := OCI_TYPECODE_CHAR;
    dtFixedNChar,
    dtFixedNWideChar:
      Result := OCI_TYPECODE_NCHAR;
    dtString,
    dtExtString,
    dtWideString,
    dtExtWideString:
      Result := OCI_TYPECODE_VARCHAR2;
    dtNString,
    dtNWideString:
      Result := OCI_TYPECODE_NVARCHAR2;
    dtUInt8,
    dtUInt16,
    dtUInt32,
    dtUInt64,
    dtInt8,
    dtInt16,
    dtInt32,
    dtInt64:
      Result := OCI_TYPECODE_NUMBER;
    dtSingle,
    dtFloat,
    dtExtended,
    dtBCD,
    dtFMTBCD:
      Result := OCI_TYPECODE_NUMBER;
    dtBytes,
    dtVarBytes,
    dtExtVarBytes:
      Result := OCI_TYPECODE_RAW;
    dtDateTime:
      Result := OCI_TYPECODE_DATE;
    dtSQLTimeStamp,
    dtTimeStamp:
      Result := OCI_TYPECODE_TIMESTAMP;
    dtTimeStampTZ:
      Result := OCI_TYPECODE_TIMESTAMP_TZ;
    dtTimeStampLTZ:
      Result := OCI_TYPECODE_TIMESTAMP_LTZ;
    dtIntervalYM:
      Result := OCI_TYPECODE_INTERVAL_YM;
    dtIntervalDS:
      Result := OCI_TYPECODE_INTERVAL_DS;
    dtOraBlob:
      Result := OCI_TYPECODE_BLOB;
    dtOraClob:
      Result := OCI_TYPECODE_CLOB;
    dtNClob:
      Result := OCI_TYPECODE_NCLOB;
    dtObject:
      Result := OCI_TYPECODE_OBJECT;
    dtReference:
      Result := OCI_TYPECODE_REF;
    dtAnyData, dtXML:
      Result := OCI_TYPECODE_OPAQUE;
    dtArray:
      Result := OCI_TYPECODE_NAMEDCOLLECTION;
    else begin
      Assert(False, SUnknownDataType);
      Result := 0;
    end;
  end;
end;

procedure TOraAnyData.ReadAttribute;
var
  Len: Cardinal;
  FullTypeName: string;
  IndicatorSize: Word;
begin
  if FValueAttribute <> nil then
    Exit;

  if Instance <> nil then begin
    CheckAlloc;

    FValueAttribute := TAttribute.Create;
    try
      Check(OCI8.OCIAnyDataGetType(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, Instance, FValueTypeCode, hValueTDO));

      FValueIndicator := nil;
      Check(OCI8.OCIAnyDataAccess(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, Instance, FValueTypeCode, hValueTDO,
        FValueIndicatorPtr, FValueData, Len));

      if hValueTDO <> nil then
        FullTypeName := ObjectTypes.GetTypeName(OCISvcCtx, hValueTDO)
      else
        FullTypeName := '';

      ObjectType.DescribeAttribute(FValueAttribute, FValueTypeCode, False, Len, 39, 39, FullTypeName, IndicatorSize);
    except
      FreeAndNil(FValueAttribute);
      raise;
    end;
  end
  else
    FValueIndicator := nil;
end;

procedure TOraAnyData.AllocAttribute(DataType: Word; out TypeCode: OCITypeCode);
begin
  AllocAttribute(DataType, 0, 0, 0, '', TypeCode);
end;

procedure TOraAnyData.AllocAttribute(DataType: Word; Len: Integer; out TypeCode: OCITypeCode);
begin
  AllocAttribute(DataType, Len, 0, 0, '', TypeCode);
end;

procedure TOraAnyData.AllocAttribute(DataType: Word; Prec, Scale: Integer; out TypeCode: OCITypeCode);
begin
  AllocAttribute(DataType, 0, Prec, Scale, '', TypeCode);
end;

procedure TOraAnyData.AllocAttribute(DataType: Word; const FullTypeName: string; out TypeCode: OCITypeCode);
begin
  AllocAttribute(DataType, 0, 0, 0, FullTypeName, TypeCode);
end;

procedure TOraAnyData.AllocAttribute(DataType: Word; Len, Prec, Scale: Integer; const FullTypeName: string; out TypeCode: OCITypeCode);
var
  IndicatorSize: Word;
begin
  CheckType;
  ClearValue;

  TypeCode := GetTypeCode(DataType);

  FValueAttribute := TAttribute.Create;
  try
    ObjectType.DescribeAttribute(FValueAttribute, TypeCode, False, Len, Prec, Scale, FullTypeName, IndicatorSize);
  except
    FreeAndNil(FValueAttribute);
    raise;
  end;
end;

procedure TOraAnyData.WriteData(Attribute: TAttribute; TypeCode: OCITypeCode; Data: IntPtr);
begin
  WriteData(Attribute, TypeCode, nil, Data, NotNullInd);
end;

procedure TOraAnyData.WriteData(Attribute: TAttribute; TypeCode: OCITypeCode; TDO: pOCIType; Data: IntPtr; IndPtr: IntPtr);
var
  Len: Cardinal;
begin
  try
    Check(OCI8.OCIAnyDataConvert(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, TypeCode, TDO,
      OCI_DURATION_SESSION, IndPtr, Data, Attribute.DataSize, FInstancePtr));

    Check(OCI8.OCIAnyDataAccess(OCISvcCtx.hOCISvcCtx, OCISvcCtx.hOCIError, Instance, TypeCode, TDO,
      @FValueIndicator, FValueData, Len));

//    if FValueAttribute <> Attribute then begin
//      FreeAndNil(FValueAttribute);
//      FValueAttribute := Attribute;
//    end;
//
//    FValueTypeCode := TypeCode;
//    hValueTDO := TDO;
  except
    FreeAndNil(FValueAttribute);
    FValueTypeCode := 0;
    hValueTDO := nil;
    raise;
  end;
end;

procedure TOraAnyData.AllocObject;
begin
  CheckType;
  CheckSession;

  FNativeInstance := True;
end;

procedure TOraAnyData.Assign(Source: TOraObject);
begin
  FreeObject;
  AsVariant := TOraAnyData(Source).AsVariant;
end;

procedure TOraAnyData.WriteAnyData;
var
  OraLob: TOraLob;
  OraTimeStamp: TOraTimeStamp;
  OraInterval: TOraInterval;
begin
  if not IsNull and (FValueObject <> nil) then begin
     case FValueAttribute.DataType of
        dtOraBlob, dtOraClob, dtWideOraClob: begin
          OraLob := TOraLob(FValueObject);
          if FValueTypeCode = OCI_TYPECODE_BLOB then
            OraLob.CreateTemporary(ltNClob)
          else if FValueTypeCode = OCI_TYPECODE_CLOB then
            OraLob.CreateTemporary(ltClob)
          else if FValueTypeCode = OCI_TYPECODE_NCLOB then
            OraLob.CreateTemporary(ltNClob)
          else
            OraLob.CreateTemporary(ltBlob);
          OraLob.WriteLob;
          WriteData(FValueAttribute, FValueTypeCode, OraLob.OCILobLocator);
        end;
        dtTimeStamp,
        dtTimeStampTZ,
        dtTimeStampLTZ: begin
          OraTimeStamp := TOraTimeStamp(FValueObject);
          WriteData(FValueAttribute, FValueTypeCode, OraTimeStamp.OCIDateTime);
          ShareOraTimeStamp(OraTimeStamp);
        end;
        dtIntervalYM,
        dtIntervalDS: begin
          OraInterval := TOraInterval(FValueObject);
          WriteData(FValueAttribute, FValueTypeCode, OraInterval.OCIInterval);
          ShareOraInterval(OraInterval);
        end;
        dtObject: begin
          TOraObject(FValueObject).WriteLobs;
          WriteData(FValueAttribute, FValueTypeCode, TOraObject(FValueObject).ObjectType.TDO, TOraObject(FValueObject).Instance, TOraObject(FValueObject).Indicator);
          TOraObject(FValueObject).NativeInstance := False;
        end;
     end;
  end;
end;

{ TOraArray }

procedure TOraArray.CheckType;
begin
  inherited;

  if not (FObjectType.DataType in [dtArray,dtTable]) then
    RaiseError('Type of object must be Array');
end;

procedure TOraArray.CheckIndex(Index: integer);
begin
  if Index >= Size then
    Size := Index + 1;
end;

function TOraArray.AppendItem: integer;
var
  ItemInstance: IntPtr;
  Obj: TOraObject;
begin
  CheckAlloc;

  Obj := nil;
  case ItemType of
    dtDateTime:
      ItemInstance := NullOCIDate;
    dtInteger, dtLargeInt, dtFloat:
      ItemInstance := NullOCINumber;
    dtString, dtWideString:
      ItemInstance := NullOCIString;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
      ItemInstance := nil;
    dtIntervalYM, dtIntervalDS:
      ItemInstance := nil;
    dtBlob, dtOraBlob, dtOraClob, dtNClob, dtWideOraClob:
      ItemInstance := nil;
    dtObject: begin
      if OCISvcCtx.Home.Direct then
        ItemInstance := nil
      else begin
        Obj := TOraObject.Create(TOraType(ObjectType.Attributes[0].ObjectType));
        ItemInstance := Obj.Instance;
      end;
      // Check(OCIObjectNew());
    end;
  else
    ItemInstance := nil;
    Assert(False, SUnknownDataType);
  end;

  try
    Check(OCI8.OCICollAppend(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ItemInstance, NullIndStruct, Instance));
  finally
    if ItemType = dtObject then
      Obj.Free;
  end;

  Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Result));
  Dec(Result);
end;

procedure TOraArray.InsertItem(Index: integer);
var
  i, ArrSize: integer;
begin
  ArrSize := Size;
  for i := ArrSize to Index do
    AppendItem;
end;

procedure TOraArray.ChildAlloc(Child: TOraObject);
var
  ChildIndex: Integer;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ItemIndicator: IntPtr;
begin
  ChildIndex := GetChildIndex(Child);
  if ChildIndex < 0 then
    Exit;

  InsertItem(ChildIndex);

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, ChildIndex, Exists, ObjectPtr, ItemIndicator));
  if Exists = 1 then begin
    Child.Instance := ObjectPtr;
    Child.Indicator := ItemIndicator;
  end;
end;

function TOraArray.GetItemExists(Index: integer): boolean;
var
  ItemInstance: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    ItemInstance, ItemIndicator));
  Result := Exists = 1;
end;

function TOraArray.GetItemIsNull(Index: integer): boolean;
var
  ItemInstance: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    ItemInstance, ItemIndicator));
  if Exists = 1 then
    Result := Marshal.ReadInt16(ItemIndicator) <> OCI_IND_NOTNULL
  else
    Result := False;
end;

procedure TOraArray.SetItemIsNull(Index: integer; Value: boolean);
var
  ItemInstance: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;
  CheckIndex(Index);

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    ItemInstance, ItemIndicator));

  if Value then
    Marshal.WriteInt16(ItemIndicator, OCI_IND_NULL)
  else
    Marshal.WriteInt16(ItemIndicator, OCI_IND_NOTNULL);

  Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, ItemInstance, ItemIndicator,
    Instance));
end;

function TOraArray.GetItemAsOCIString(Index: integer): pOCIString;
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    lOCIString, ItemIndicator));

  if Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL then
    Result := Marshal.ReadIntPtr(lOCIString)
  else
    Result := nil;
end;
procedure TOraArray.SetItemAsOCIString(Index: integer; Value: pOCIString);
begin
  CheckAlloc;
  CheckIndex(Index);

  Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Value, NotNullInd,
    Instance));

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsDateTime(Index: integer): TDateTime;
var
  lOCIDate: pOCIDate;
  ItemIndicator: IntPtr;
  Exists: tbool;
  Time: TDateTime;
begin
  CheckAlloc;

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    lOCIDate, ItemIndicator));

  if Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL then
    case ItemType of
      dtDateTime: begin
        Result := EncodeDate(Marshal.ReadInt16(lOCIDate), Marshal.ReadByte(lOCIDate, 2), Marshal.ReadByte(lOCIDate, 3));
        Time := EncodeTime(Marshal.ReadByte(lOCIDate, 4), Marshal.ReadByte(lOCIDate, 5),
          Marshal.ReadByte(lOCIDate, 6), 0);
        if Result < 0 then
          Result := Result - Time
        else
          Result := Result + Time;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'DateTime');
    end
  else
    Result := 0;
end;

procedure TOraArray.SetItemAsDateTime(Index: integer; Value: TDateTime);
var
  lOCIDate: pOCIDate;
  Year, Month, Day: word;
  Hour, Min, Sec, MSec: word;
begin
  CheckAlloc;
  CheckIndex(Index);

  lOCIDate := Marshal.AllocHGlobal(sizeof(OCIDate));
  try
    case ItemType of
      dtDateTime: begin
        DecodeDate(Value, Year, Month, Day);
        DecodeTime(Value, Hour, Min, Sec, MSec);

        Marshal.WriteInt16(lOCIDate, Year);
        Marshal.WriteByte(lOCIDate, 2, Lo(Month));
        Marshal.WriteByte(lOCIDate, 3, Lo(Day));
        Marshal.WriteByte(lOCIDate, 4, Lo(Hour));
        Marshal.WriteByte(lOCIDate, 5, Lo(Min));
        Marshal.WriteByte(lOCIDate, 6, Lo(Sec));

        Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, lOCIDate, NotNullInd, Instance));
      end;
//      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
//        DecodeDate(Value, Year, Month, Day);
//        DecodeTime(Value, Hour, Min, Sec, MSec);
//
//        OraTimeStamp := TOraTimeStamp.Create(OCISvcCtx, ItemType);
//        try
//          OraTimeStamp.Construct(Year, Month, Day, Hour, Min, Sec, MSec, '');
//          Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, OraTimeStamp.OCIDateTime, NotNullInd, Instance));
//        finally
//          OraTimeStamp.Free;
//        end;
//      end
    else
      raise EConvertError.Create(SCannotConvert + 'DateTime');
    end;

  finally
    Marshal.FreeHGlobal(lOCIDate);
  end;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsFloat(Index: integer): double;
var
  Elem: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
        Elem, ItemIndicator));

      if (Exists = 1) and (Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL) then
        case ItemSubType of
          dtBDouble:
            Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Elem));
          dtBFloat:
            Result := Single(Elem^);
        else
          Check(OCI8.OCINumberToReal(OCISvcCtx.hOCIError, Elem, SizeOf(Double), Result));
        end
      else
        Result := 0;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Float');
  end;
end;

procedure TOraArray.SetItemAsFloat(Index: integer; Value: double);
var
  ValuePtr: IntPtr;
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      ValuePtr := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
      try
        case ItemSubType of
          dtBDouble:
            Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
          dtBFloat:
            Single(ValuePtr^) := Value;
        else
          Check(OCI8.OCINumberFromReal(OCISvcCtx.hOCIError, Value, SizeOf(Double), ValuePtr));
        end;

        Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, ValuePtr, NotNullInd,
          Instance));
        Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
      finally
        Marshal.FreeHGlobal(ValuePtr);
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Float');
  end;
end;

function TOraArray.GetItemAsInteger(Index: integer): integer;
var
  Elem: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
  Val: int64;
begin
  CheckAlloc;

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
        Elem, ItemIndicator));

      if (Exists = 1) and (Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL) then
        case ItemSubType of
          dtBDouble:
            Result := Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Elem)));
          dtBFloat:
            Result := Trunc(Single(Elem^));
        else
          Check(OCI8.OCINumberToInt(OCISvcCtx.hOCIError, Elem, SizeOf(Integer), OCI_NUMBER_SIGNED, Val));
          Result := Integer(Val);
        end
      else
        Result := 0;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Integer');
  end;
end;

procedure TOraArray.SetItemAsInteger(Index: integer; Value: integer);
var
  ValuePtr: IntPtr;
  Val: int64;
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      ValuePtr := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
      try
        case ItemSubType of
          dtBDouble:
            Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
          dtBFloat:
            Single(ValuePtr^) := Value;
        else
          Val := Value;
          Check(OCI8.OCINumberFromInt(OCISvcCtx.hOCIError, Val, SizeOf(Integer), OCI_NUMBER_SIGNED, ValuePtr));
        end;

        Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, ValuePtr, NotNullInd,
          Instance));
        Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
      finally
        Marshal.FreeHGlobal(ValuePtr);
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Integer');
  end;
end;

function TOraArray.GetItemAsLargeInt(Index: integer): int64;
var
  Elem: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
        Elem, ItemIndicator));

      if (Exists = 1) and (Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL) then
        case ItemSubType of
          dtBDouble:
            Result := Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Elem)));
          dtBFloat:
            Result := Trunc(Single(Elem^));
        else
          Check(OCI8.OCINumberToInt(OCISvcCtx.hOCIError, Elem, SizeOf(Int64), OCI_NUMBER_SIGNED, Result));
        end
      else
        Result := 0;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Int64');
  end;
end;

procedure TOraArray.SetItemAsLargeInt(Index: integer; Value: int64);
var
  ValuePtr: IntPtr;
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      ValuePtr := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
      try
        case ItemSubType of
          dtBDouble:
            Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
          dtBFloat:
            Single(ValuePtr^) := Value;
        else
          Check(OCI8.OCINumberFromInt(OCISvcCtx.hOCIError, Value, SizeOf(Int64), OCI_NUMBER_SIGNED, ValuePtr));
        end;

        Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, ValuePtr, NotNullInd,
          Instance));
        Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
      finally
        Marshal.FreeHGlobal(ValuePtr);
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Int64');
  end;
end;

function TOraArray.GetItemAsString(Index: integer): string;
begin
{$IFDEF IS_UNICODE}
  Result := ItemAsWideString[Index];
{$ELSE}
  Result := ItemAsAnsiString[Index];
{$ENDIF}
end;

procedure TOraArray.SetItemAsString(Index: integer; const Value: string);
begin
{$IFDEF IS_UNICODE}
  ItemAsWideString[Index] := Value;
{$ELSE}
  ItemAsAnsiString[Index] := Value;
{$ENDIF}
end;

function TOraArray.GetItemAsAnsiString(Index: integer): AnsiString;
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    IntPtr(lOCIString), ItemIndicator));

  if Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL then
    case ItemType of
      dtString:
        Result := Marshal.PtrToStringAnsi(OCI8.OCIStringPtr(OCISvcCtx.hOCIEnv, Marshal.ReadIntPtr(lOCIString)));
      dtWideString:
        Result := AnsiString(Marshal.PtrToStringUni(OCI8.OCIStringPtr(OCISvcCtx.hOCIEnv, Marshal.ReadIntPtr(lOCIString))));
    else
      raise EConvertError.Create(SCannotConvert + 'String');
      Result := '';
    end
  else
    Result := '';
end;

procedure TOraArray.SetItemAsAnsiString(Index: integer; const Value: AnsiString);
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
  ValuePtr: IntPtr;
  Res: integer;
  Val: AnsiString;
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtString: begin
      Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
        IntPtr(lOCIString), ItemIndicator));
      Assert(Exists = 1);
      Val := Value;
      if Val = '' then
        Val := #0;
      ValuePtr := Marshal.StringToHGlobalAnsi(Value);
      Res := OCI8.OCIStringAssignText(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ValuePtr, LengthA(Val), lOCIString);
      FreeString(ValuePtr);
      Check(Res);
    end;
    dtWideString: begin
      ItemAsWideString[Index] := WideString(Value);
      Exit;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'String');
  end;

  Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Marshal.ReadIntPtr(lOCIString), NotNullInd,
    Instance));

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsWideString(Index: integer): WideString;
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    IntPtr(lOCIString), ItemIndicator));

  if Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL then
    case ItemType of
      dtString:
        Result := WideString(Marshal.PtrToStringAnsi(OCI8.OCIStringPtr(OCISvcCtx.hOCIEnv, Marshal.ReadIntPtr(lOCIString))));
      dtWideString:
        Result := Marshal.PtrToStringUni(OCI8.OCIStringPtr(OCISvcCtx.hOCIEnv, Marshal.ReadIntPtr(lOCIString)));
    else
      raise EConvertError.Create(SCannotConvert + 'WideString');
      Result := '';
    end
  else
    Result := '';
end;

procedure TOraArray.SetItemAsWideString(Index: integer; const Value: WideString);
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
  ValuePtr: IntPtr;
  Res: integer;
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtWideString: begin
      Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
        IntPtr(lOCIString), ItemIndicator));
      Assert(Exists = 1);
      ValuePtr := Marshal.StringToHGlobalUni(Value);
      Res := OCI8.OCIStringAssignText(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ValuePtr, Length(Value) * 2, lOCIString);
      FreeString(ValuePtr);
      Check(Res);
    end;
    dtString: begin
      ItemAsAnsiString[Index] := AnsiString(Value);
      Exit;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'WideString');
  end;

  Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Marshal.ReadIntPtr(lOCIString), NotNullInd,
    Instance));

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsObject(Index: integer): TOraObject;
var
  ItemIndicator: IntPtr;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ArrSize, i: integer;
  Attr: TAttribute;
  ItemInstance: IntPtr;
begin
  CheckAlloc;

  if ItemType <> dtObject then
    raise EConvertError.Create(SCannotConvert + 'Object');

  Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, ArrSize));

  Attr := ObjectType.Attributes[0];
  if Attr = nil then
    RaiseError(SInvalidAttrName);
  if Index >= ArrSize then
    for i := ArrSize to Index do begin
      case Attr.DataType of
        dtObject: begin
          if OCISvcCtx.Home.Direct then
            ItemInstance := nil
          else
            ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
          // Check(OCIObjectNew());
        end;
      else
        raise EConvertError.Create(SCannotConvert + 'Object');
      end;

      // WAR need real Indicator for object
      try
        Check(OCI8.OCICollAppend(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ItemInstance, NullIndStruct, Instance));
      finally
        if Attr.DataType = dtObject then
          Marshal.FreeHGlobal(ItemInstance);
      end;
    end;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    ObjectPtr, ItemIndicator));
  if (FObjects <> nil) and (Index < FObjects.Count) then
    Result := TOraObject(FObjects[Index])
  else
    Result := nil;

  if Result = nil then begin
    if FObjects = nil then
      FObjects := TList.Create;

    case ItemType of
      dtObject:
        Result := TOraObject.Create;
    else
      raise EConvertError.Create(SCannotConvert + 'Object');
    end;
    Result.FOwner := Self;
    Result.ObjectType := TOraType(ObjectType.Attributes[0].ObjectType);
    Result.OCISvcCtx := FOCISvcCtx;

    while Index >= FObjects.Count do
      FObjects.Add(nil);
    FObjects[Index] := Result;
  end;

  if {(sb2(ItemIndicator^) = OCI_IND_NOTNULL) and} (Exists = 1) then begin
    Result.Instance := ObjectPtr;
    Result.Indicator := ItemIndicator;
          {case Attr.DataType of
            dtObject:
              Result := TOraObject.Create(TOraType(Attr.ObjectType));
            dtReference: begin
              Result := TOraRef.Create(TOraType(Attr.ObjectType));
              try
                GetAttribute(Name, Value, Ind);
                TOraRef(Result).OCIRef := IntPtr(Value^);
                if not TOraRef(Result).IsNull then
                  TOraRef(Result).Pin;  // ??? optim
              except
                Result.Free;
                raise;
              end;
            end;
            dtArray:
              Result := TOraArray.Create(TOraType(Attr.ObjectType));
            dtTable:
              Result := TOraNestTable.Create(TOraType(Attr.ObjectType));
          end;}
  end;
end;

procedure TOraArray.SetItemAsObject(Index: integer; Value: TOraObject);
begin
  CheckAlloc;
  CheckIndex(Index);

  // WAR need modify TObjects

  case ItemType of
    dtObject: begin
      Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Value.Instance, Value.Indicator,
        Instance));
      //Value.FNativeInstance := False;  // ???
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Object');
  end;

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsRef(Index: integer): TOraRef;
var
  ItemIndicator: IntPtr;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ArrSize, i: integer;
  Attr: TAttribute;
  ItemInstance: IntPtr;
begin
  CheckAlloc;

  if ItemType <> dtReference then
    raise EConvertError.Create(SCannotConvert + 'Reference');

  Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, ArrSize));

  Attr := ObjectType.Attributes[0];
  if Attr = nil then
    RaiseError(SInvalidAttrName);
  if Index >= ArrSize then
    for i := ArrSize to Index do begin
      case Attr.DataType of
        dtReference: begin
          if OCISvcCtx.Home.Direct then
            ItemInstance := nil
          else
            ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
          // Check(OCIObjectNew());
        end;
      else
        raise EConvertError.Create(SCannotConvert + 'Reference');
      end;

      // WAR need real Indicator for object
      try
        Check(OCI8.OCICollAppend(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ItemInstance, NullIndStruct, Instance));
      finally
        if Attr.DataType = dtReference then
          Marshal.FreeHGlobal(ItemInstance);
      end;
    end;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    ObjectPtr, ItemIndicator));
  if (FObjects <> nil) and (Index < FObjects.Count) then
    Result := TOraRef(FObjects[Index])
  else
    Result := nil;

  if Result = nil then begin
    if FObjects = nil then
      FObjects := TList.Create;

    case ItemType of
      dtReference:
        Result := TOraRef.Create(nil);
    else
      raise EConvertError.Create(SCannotConvert + 'Reference');
    end;
    Result.FOwner := Self;
    Result.ObjectType := TOraType(ObjectType.Attributes[0].ObjectType);
    Result.OCISvcCtx := FOCISvcCtx;

    while Index >= FObjects.Count do
      FObjects.Add(nil);
    FObjects[Index] := Result;
  end;

  if {(sb2(ItemIndicator^) = OCI_IND_NOTNULL) and} (Exists = 1) then begin
    Result.OCIRef := Marshal.ReadIntPtr(ObjectPtr);
  end;
end;

procedure TOraArray.SetItemAsRef(Index: integer; Value: TOraRef);
begin
  CheckAlloc;
  CheckIndex(Index);

  // WAR need modify TObjects

  case ItemType of
    dtReference: begin
      Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Value.OCIRef, NotNullInd,
        Instance));
      //Value.FNativeInstance := False;  // ???
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Reference');
  end;

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsLob(Index: integer): TOraLob;
var
  ItemIndicator: IntPtr;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ArrSize, i: integer;
  Attr: TAttribute;
  Ind: Integer;
begin
  CheckAlloc;

  if not ItemType in [dtBlob, dtOraBlob, dtOraClob, dtNClob, dtWideOraClob] then
    raise EConvertError.Create(SCannotConvert + 'Lob');

  Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, ArrSize));

  Attr := ObjectType.Attributes[0];
  if Attr = nil then
    RaiseError(SInvalidAttrName);
  if Index >= ArrSize then
    for i := ArrSize to Index do
      AppendItem;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    ObjectPtr, ItemIndicator));

  if (FObjects <> nil) and (Index < FObjects.Count) then
    Result := TOraLob(FObjects[Index])
  else
    Result := nil;

  if Result = nil then begin
    if FObjects = nil then
      FObjects := TList.Create;

    case ItemType of
      dtBlob, dtOraBlob: begin
        Result := TOraLob.Create(FOCISvcCtx);
        Result.LobType := ltBlob;
      end;
      dtOraClob, dtWideOraClob: begin
        Result := TOraLob.Create(FOCISvcCtx);
        if Attr.SubDataType = dtNClob then
          Result.LobType := ltNClob
        else
          Result.LobType := ltClob;
        TOraLob(Result).IsUnicode := True;
      end;
      dtNClob: begin
        Result := TOraLob.Create(OCISvcCtx);
        Result.LobType := ltNClob;
        TOraLob(Result).IsUnicode := True;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'Lob');
    end;

    Ind := Marshal.ReadInt16(ItemIndicator);
    if Ind = OCI_IND_NOTNULL then begin
      Result.OCILobLocator := Marshal.ReadIntPtr(ObjectPtr);
      Result.ReadLob;
    end;

    while Index >= FObjects.Count do
      FObjects.Add(nil);
    FObjects[Index] := Result;
  end;
end;

procedure TOraArray.SetItemAsLob(Index: integer; Value: TOraLob);
begin
  CheckAlloc;
  CheckIndex(Index);

  // WAR need modify TObjects

  case ItemType of
    dtBlob, dtOraBlob, dtOraClob, dtNClob, dtWideOraClob: begin
      Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Value.OCILobLocator, NotNullInd,
        Instance));
      //Value.FNativeInstance := False;  // ???
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Lob');
  end;

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsOraTimeStamp(Index: integer): TOraTimeStamp;
var
  ItemIndicator: IntPtr;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ArrSize, i: integer;
  Attr: TAttribute;
  Ind: Integer;
begin
  CheckAlloc;

  if not ItemType in [dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ] then
    raise EConvertError.Create(SCannotConvert + 'OraTimeStamp');

  Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, ArrSize));

  Attr := ObjectType.Attributes[0];
  if Attr = nil then
    RaiseError(SInvalidAttrName);
  if Index >= ArrSize then
    for i := ArrSize to Index do
      AppendItem;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    ObjectPtr, ItemIndicator));

  if (FObjects <> nil) and (Index < FObjects.Count) then
    Result := TOraTimeStamp(FObjects[Index])
  else
    Result := nil;

  if Result = nil then begin
    if FObjects = nil then
      FObjects := TList.Create;

    Result := TOraTimeStamp.Create(FOCISvcCtx, ItemType);

    Ind := Marshal.ReadInt16(ItemIndicator);
    if Ind = OCI_IND_NOTNULL then
      Result.OCIDateTime := Marshal.ReadIntPtr(ObjectPtr);

    while Index >= FObjects.Count do
      FObjects.Add(nil);
    FObjects[Index] := Result;
  end;
end;

procedure TOraArray.SetItemAsOraTimeStamp(Index: integer; Value: TOraTimeStamp);
begin
  CheckAlloc;
  CheckIndex(Index);

  // WAR need modify TObjects

  case ItemType of
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
      Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Value.OCIDateTime, NotNullInd,
        Instance));
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OraTimeStamp');
  end;

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsOraInterval(Index: integer): TOraInterval;
var
  ItemIndicator: IntPtr;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ArrSize, i: integer;
  Attr: TAttribute;
  Ind: Integer;
begin
  CheckAlloc;

  if not ItemType in [dtIntervalYM, dtIntervalDS] then
    raise EConvertError.Create(SCannotConvert + 'OraTimeStamp');

  Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, ArrSize));

  Attr := ObjectType.Attributes[0];
  if Attr = nil then
    RaiseError(SInvalidAttrName);
  if Index >= ArrSize then
    for i := ArrSize to Index do
      AppendItem;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

  Check(OCI8.OCICollGetElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Index, Exists,
    ObjectPtr, ItemIndicator));

  if (FObjects <> nil) and (Index < FObjects.Count) then
    Result := TOraInterval(FObjects[Index])
  else
    Result := nil;

  if Result = nil then begin
    if FObjects = nil then
      FObjects := TList.Create;

    Result := TOraInterval.Create(FOCISvcCtx, ItemType);

    Ind := Marshal.ReadInt16(ItemIndicator);
    if Ind = OCI_IND_NOTNULL then
      Result.OCIInterval := Marshal.ReadIntPtr(ObjectPtr);

    while Index >= FObjects.Count do
      FObjects.Add(nil);
    FObjects[Index] := Result;
  end;
end;

procedure TOraArray.SetItemAsOraInterval(Index: integer; Value: TOraInterval);
begin
  CheckAlloc;
  CheckIndex(Index);

  // WAR need modify TObjects

  case ItemType of
    dtIntervalYM, dtIntervalDS: begin
      Check(OCI8.OCICollAssignElem(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Value.OCIInterval, NotNullInd,
        Instance));
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OraInterval');
  end;

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

procedure TOraArray.Assign(Source: TOraObject);
begin
  if Source is TOraArray then begin
    CheckAlloc;

    Check(OCI8.OCICollAssign(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Source.Instance, Instance));
  end
  else
    inherited;
end;

function TOraArray.GetSize: integer;
begin
  CheckAlloc;

  Check(OCI8.OCICollSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Result))
end;

procedure TOraArray.SetSize(Value: integer);
var
  ASize: integer;
begin
  CheckAlloc;

  ASize := Size;
  if Value > ASize then
    InsertItem(Value - 1)
  else
    Check(OCI8.OCICollTrim(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, ASize - Value, Instance));
end;

function TOraArray.GetMaxSize: integer;
begin
  CheckAlloc;

  Result := OCI8.OCICollMax(OCISvcCtx.hOCIEnv, Instance);
end;

function TOraArray.GetItemType;
begin
  CheckType;

  Result := ObjectType.Attributes[0].DataType;
end;

function TOraArray.GetItemSubType;
begin
  CheckType;

  Result := ObjectType.Attributes[0].SubDataType;
end;

procedure TOraArray.Clear;
begin
  CheckAlloc;

  FreeObjects;
  Check(OCI8.OCICollTrim(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, GetSize, Instance));
end;

{ TOraNestTable }

procedure TOraNestTable.CheckType;
begin
  inherited;

  if not (FObjectType.DataType in [dtTable]) then
    RaiseError('Type of object must be Table');
end;

procedure TOraNestTable.Assign(Source: TOraObject);
begin
  inherited;

  if (Source is TOraNestTable) and (OCISvcCtx.Home.OCIVersion >= 9000) then
    NestTableAssign(Source.Instance, Instance, ObjectType);
end;

procedure TOraNestTable.DeleteItem(Index: integer);
begin
  Check(OCI8.OCITableDelete(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Index, Instance));
end;

function TOraNestTable.GetSize: integer;
begin
  CheckAlloc;

  Check(OCI8.OCITableSize(OCISvcCtx.hOCIEnv, OCISvcCtx.hOCIError, Instance, Result))
end;

{ TRefData }

constructor TRefData.Create;
begin
  inherited;

  FRequireEmptyStrToNull := True;
end;

destructor TRefData.Destroy;
begin
  inherited;

  if FRef <> nil then
    FRef.Release;
end;

procedure TRefData.InternalPrepare;
begin
  if FRef = nil then
    RaiseError('Reference is not defined');
end;

procedure TRefData.InternalOpen(DisableInitFields: boolean = False); // PrepareData;
begin
  inherited;

  FEOF := False;
  if not DisableInitFields then
    InitFields;
end;

procedure TRefData.Reopen;
begin
  FreeData;
  InitData;
  InternalOpen; // PrepareData;
end;

{ Fields }

procedure TRefData.CreateFieldDescs;
var
  ObjectType: TObjectType;
  Field: TFieldDesc;
begin
  ObjectType := FRef.ObjectType;

  Field := TFieldDesc.Create(TOCIRecordSet);
  Field.Name := 'REF';
  Field.DataType := ObjectType.DataType;
  Field.Size := SizeOf(IntPtr);
  Field.ObjectType := ObjectType;
  Field.HiddenObject := not FIncludeObjectField;
  Field.FieldNo := 1;

  FFields.Add(Field);

  InitObjectFields(ObjectType, Field);
end;

{ Records }

procedure TRefData.GetRecord(RecBuf: IntPtr);
begin
  if not FEOF and not FBOF and not FRef.IsNull then
    Marshal.WriteIntPtr(RecBuf, FRef.GCHandle);
end;

procedure TRefData.PutRecord(RecBuf: IntPtr);
begin
  if not FEOF and not FBOF and not FRef.IsNull then begin
    Assert(FRef = GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf)));
    FRef := TOraRef(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf)));
  end;
end;

procedure TRefData.GetNextRecord(RecBuf: IntPtr);
begin
  if not EOF then
    if BOF then
      if not FRef.IsNull then begin
        FBOF := False;
        GetRecord(RecBuf);
      end
      else
        FEOF := True
    else
      FEOF := True;
end;

procedure TRefData.GetPriorRecord(RecBuf: IntPtr);
begin
  if not BOF then
    if EOF then
      if not FRef.IsNull then begin
        FEOF := False;
        GetRecord(RecBuf);
      end
      else
        FBOF := True
    else
      FBOF := True;
end;

procedure TRefData.AppendRecord(RecBuf: IntPtr);
begin
end;

procedure TRefData.InsertRecord(RecBuf: IntPtr);
begin
  AppendRecord(RecBuf);
end;

procedure TRefData.UpdateRecord(RecBuf: IntPtr);
begin
  FRef.MarkDelete;//Update;
  FRef.Flush;
end;

procedure TRefData.DeleteRecord;
begin
end;

procedure TRefData.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Ptr: IntPtr;
  OraObject: TOraObject;
  OraRef: TOraRef;
begin
  Ptr := PtrOffset(RecBuf, Field.DataOffset);
  case Field.DataType of
    dtObject: begin
      OraObject := TOraObject.Create(TOraType(Field.ObjectType));
      try
        OraObject.AllocObject(FRef.OCISvcCtx);
      except
        OraObject.Free;
        raise;
      end;
      Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
    end;
    dtReference: begin
      OraRef := TOraRef.Create(TOraType(Field.ObjectType));
      Marshal.WriteIntPtr(Ptr, OraRef.GCHandle);
    end;
    dtAnyData: begin
      OraObject := TOraAnyData.Create(FRef.OCISvcCtx, TOraType(Field.ObjectType));
      try
        OraObject.AllocObject;
      except
        OraObject.Free;
        raise;
      end;
      Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
    end;
    dtXML: begin
      OraObject := TOraXML.Create(FRef.OCISvcCtx, TOraType(Field.ObjectType));
      try
        OraObject.AllocObject;
      except
        OraObject.Free;
        raise;
      end;
      Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
    end;
    else
      inherited;
  end;
end;

procedure TRefData.FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  OraObject: TOraObject;
begin
  case Field.DataType of
    dtObject, dtReference: begin
      OraObject := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
      OraObject.Release;
    end
    else
      inherited;
  end;
end;

procedure TRefData.CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc);
var
  ValueOffset: Integer;
  SrcPtr: IntPtr;
  DestPtr: IntPtr;
  OraObjectSrc, OraObjectDest: TOraObject;
begin
  ValueOffset := Field.DataOffset;

  case Field.DataType of
    dtObject, dtReference: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      OraObjectSrc := TOraObject(GetGCHandleTarget(SrcPtr));
      OraObjectDest := TOraObject(GetGCHandleTarget(DestPtr));
      OraObjectDest.Assign(OraObjectSrc);
    end;
    else
      inherited;
  end;
end;

{ Bookmarks }

procedure TRefData.GetBookmark(Bookmark: PRecBookmark);
begin
end;

procedure TRefData.SetToBookmark(Bookmark: PRecBookmark);
begin
  FEOF := False;
  FBOF := False;
end;

procedure TRefData.SetRef(Value: TOraRef);
begin
  if Value <> FRef then begin
    if FRef <> nil then
      FRef.Release;

    FRef := Value;

    if FRef <> nil then
      FRef.AddRef;
  end;
end;

{ TTableData }

constructor TTableData.Create;
begin
  inherited;

  FRequireEmptyStrToNull := True;
end;

destructor TTableData.Destroy;
begin
  inherited;

  if FTable <> nil then
    FTable.Release;
end;

procedure TTableData.Check(Status: sword);
begin
  FTable.Check(Status);
end;

procedure TTableData.InternalPrepare;
begin
  {if FTable = nil then
    RaiseError('Table is not defined');} //WAR
end;

procedure TTableData.InternalOpen(DisableInitFields: boolean = False); //PrepareData;
begin
  inherited;

  FEOF := False;
  LastIndex := -1;
  if not DisableInitFields then
    InitFields;
end;

procedure TTableData.Reopen;
begin
  FreeData;
  InitData;
  InternalOpen; // PrepareData;
end;

{ Fields }

procedure TTableData.CreateFieldDescs;
var
  ObjectType: TObjectType;
  Field: TFieldDesc;
begin
  if FTable <> nil then begin
    if FTable.ItemType = dtObject then begin
      ObjectType := FTable.ObjectType.Attributes[0].ObjectType;

      Field := TFieldDesc.Create(TOCIRecordSet);
      Field.Name := FTable.ObjectType.Attributes[0].Name;
      Field.DataType := ObjectType.DataType;
      Field.Size := SizeOf(IntPtr);
      Field.ObjectType := ObjectType;
      Field.HiddenObject := not FIncludeObjectField;
      Field.FieldNo := 1;

      FFields.Add(Field);
    end
    else begin
      ObjectType := FTable.ObjectType;
      Field := nil;
    end;

    InitObjectFields(ObjectType, Field);

    case FTable.ItemType of
      dtDateTime:
        Fields[0].Size := SizeOf(TDateTime);
      dtFloat:
        Fields[0].Size := SizeOf(Double);
      dtInteger:
        Fields[0].Size := SizeOf(Integer);
      dtLargeInt:
        Fields[0].Size := SizeOf(Int64);
      dtString:
        Fields[0].Size := Fields[0].Length + 1;
      dtWideString:
        Fields[0].Size := (Fields[0].Length + 1) * 2;
    end;
  end;
end;

function TTableData.GetIndicatorSize: Integer;
begin
  Result := inherited GetIndicatorSize + SizeOf(Integer); // for index
end;

procedure TTableData.InitFields;
begin
  inherited;

  FIndexOfs := RecordSize - SizeOf(Integer);
end;

class function TTableData.IsBlobDataType(DataType: word): boolean;
begin
  Result := TOCIRecordSet.IsBlobDataType(DataType);
end;

{ Records }

{procedure TTableData.GetObjectFields(RecBuf: IntPtr; Obj: TDBObject; var FieldNo: word);
var
  i: integer;
  ObjectType: TObjectType;
  IsBlank: boolean;
begin
  ObjectType := Obj.ObjectType;
  for i := 0 to ObjectType.AttributeCount - 1 do
    case ObjectType.Attributes[i].DataType of
      dtDateTime, dtFloat, dtInteger, dtLargeInt, dtString, dtWideString: begin
        TOraObject(Obj).GetAttributeValue(ObjectType.Attributes[i].Name,
          PtrOffset(RecBuf, Fields[FieldNo - 1].Offset), IsBlank);
        SetNull(FieldNo, RecBuf, IsBlank);
        Inc(FieldNo);
      end;
    else
      Assert(False, SUnknownDataType);
    end;
end;

procedure TTableData.PutObjectFields(RecBuf: IntPtr; Obj: TDBObject; var FieldNo: word);
var
  i: integer;
  ObjectType: TObjectType;
begin
  ObjectType := Obj.ObjectType;
  for i := 0 to ObjectType.AttributeCount - 1 do
    case ObjectType.Attributes[i].DataType of
      dtDateTime, dtFloat, dtInteger, dtLargeInt, dtString, dtWideString: begin
        if GetNull(FieldNo, RecBuf) then
          TOraObject(Obj).SetAttributeValue(ObjectType.Attributes[i].Name, nil)
        else
          TOraObject(Obj).SetAttributeValue(ObjectType.Attributes[i].Name,
            PtrOffset(RecBuf, Fields[FieldNo - 1].Offset));
        Inc(FieldNo);
      end;
    else
      Assert(False, SUnknownDataType);
    end;
end;}

procedure TTableData.GetRecord(RecBuf: IntPtr);
var
  Index, Len: integer;
  IsNull: boolean;
  //FieldNo: word;
  sa: AnsiString;
  ws: WideString;
begin
  inherited;

  if not(EOF or BOF or (IntPtr(CurrentItem) = nil)) then begin
    Index := Marshal.ReadInt32(CurrentItem, SizeOf(TItemHeader) + FIndexOfs);
    IsNull := FTable.ItemIsNull[Index];
    SetNull(Fields[0], RecBuf, IsNull);

    if not IsNull then
      case FTable.ItemType of
        dtDateTime:
          Marshal.WriteInt64(RecBuf, BitConverter.DoubleToInt64Bits(double(FTable.ItemAsDateTime[Index])));
        dtFloat:
          Marshal.WriteInt64(RecBuf, BitConverter.DoubleToInt64Bits(FTable.ItemAsFloat[Index]));
        dtInteger:
          Marshal.WriteInt32(RecBuf, FTable.ItemAsInteger[Index]);
        dtLargeInt:
          Marshal.WriteInt64(RecBuf, FTable.ItemAsLargeInt[Index]);
        dtString: begin
          sa := FTable.ItemAsAnsiString[Index];
          Len := LengthA(sa);
          if Len > FTable.ObjectType.Attributes[0].Length then
            Len := FTable.ObjectType.Attributes[0].Length;
          CopyBufferAnsi(sa, RecBuf, Len + 1);
        end;
        dtWideString: begin
          ws := FTable.ItemAsWideString[Index];
          Len := Length(ws);
          if Len > FTable.ObjectType.Attributes[0].Length then
            Len := FTable.ObjectType.Attributes[0].Length;
          CopyBufferUni(ws, RecBuf, (Len + 1) * 2);
        end;
        dtObject, dtReference: begin
          Marshal.WriteIntPtr(RecBuf, Marshal.ReadIntPtr(CurrentItem, SizeOf(TItemHeader)));
          {FieldNo := 1;
          GetObjectFields(RecBuf, TOraObject(IntPtr(PChar(CurrentItem) + SizeOf(TItemHeader))^), FieldNo);}
        end;
      else
        Assert(False, SUnknownDataType);
      end;
  end;
end;

procedure TTableData.PutRecord(RecBuf: IntPtr);
var
  Index: integer;
begin
  inherited;

  if not(EOF or BOF or (IntPtr(CurrentItem) = nil)) then begin
    Index := Marshal.ReadInt32(RecBuf, FIndexOfs);
    if not(FTable.ItemType in [dtObject]) and GetNull(Fields[0], RecBuf) then
      FTable.ItemIsNull[Index] := True
    else
      case FTable.ItemType of
        dtDateTime:
          FTable.ItemAsDateTime[Index] := TDateTime(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(RecBuf)));
        dtFloat:
          FTable.ItemAsFloat[Index] := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(RecBuf));
        dtInteger:
          FTable.ItemAsInteger[Index] := Marshal.ReadInt32(RecBuf);
        dtLargeInt:
          FTable.ItemAsLargeInt[Index] := Marshal.ReadInt64(RecBuf);
        dtString:
          FTable.ItemAsAnsiString[Index] := Marshal.PtrToStringAnsi(RecBuf);
        dtWideString:
          FTable.ItemAsWideString[Index] := Marshal.PtrToStringUni(RecBuf);
        dtObject: begin
          Marshal.WriteIntPtr(CurrentItem, SizeOf(TItemHeader), Marshal.ReadIntPtr(RecBuf));
          {FieldNo := 1;
          PutObjectFields(RecBuf, TOraObject(IntPtr(PChar(CurrentItem) + SizeOf(TItemHeader))^), FieldNo);}
        end;
      else
        Assert(False, SUnknownDataType);
      end;
  end;
end;

procedure TTableData.GetNextRecord(RecBuf: IntPtr);
var
  Item: PItemHeader;
  Res: boolean;

  procedure OmitRecords;
  begin
  end;
begin
  if not EOF then begin
    if BOF then begin
      if IntPtr(FirstItem) = nil then begin
        Res := Fetch;
        if Res then
          FBOF := False
        else
          FEOF := True;
      end
      else
        FBOF := False;

      CurrentItem := FirstItem;
      OmitRecords;
      if IntPtr(CurrentItem) = nil then
        FEOF := True
      else
        GetRecord(RecBuf);
    end
    else begin
      Item := CurrentItem;
      CurrentItem := CurrentItem.Next;
      OmitRecords;
      if IntPtr(CurrentItem) = nil then begin
        Res := Fetch;

        if Res then begin
          CurrentItem := Item.Next;
          GetRecord(RecBuf)
        end
        else
          FEOF := True
      end
      else
        GetRecord(RecBuf);
    end;
  end;
end;

procedure TTableData.FetchComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Field: TFieldDesc;
begin
  if HasComplexFields then
    for i := 0 to Fields.Count - 1 do begin
      Field := Fields[i];
      if Field.IsComplex and
        (not Field.IsBlob or WithBlob) and
        (not Field.HasParent) and
        (Field.FieldDescKind <> fdkCalculated)
      then
        FetchComplexField(RecBuf, Field);
    end;
end;

procedure TTableData.FetchComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Ptr: IntPtr;
  OraObject: TOraObject;
begin
  case Field.DataType of
    dtObject, dtAnyData, dtXML: begin
      Ptr := PtrOffset(RecBuf, Field.Offset);
      OraObject := FTable.ItemAsObject[LastIndex];
      OraObject.AddRef;
      Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
    end;
    dtReference: begin
      Ptr := PtrOffset(RecBuf, Field.Offset);
      OraObject := FTable.ItemAsRef[LastIndex];
      OraObject.AddRef;
      Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
    end;
    else
      inherited CreateComplexField(RecBuf, Field);
  end;
end;

procedure TTableData.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Ptr: IntPtr;
  OraObject: TOraObject;
  OraRef: TOraRef;
begin
  Ptr := PtrOffset(RecBuf, Field.Offset);
  case Field.DataType of
    dtObject: begin
      OraObject := TOraObject.Create(TOraType(Field.ObjectType));
      try
        OraObject.AllocObject(FTable.OCISvcCtx);
      except
        OraObject.Free;
        raise;
      end;
      Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
    end;
    dtReference: begin
      OraRef := TOraRef.Create(TOraType(Field.ObjectType));
      Marshal.WriteIntPtr(Ptr, OraRef.GCHandle);
    end;
    dtAnyData: begin
      OraObject := TOraAnyData.Create(FTable.OCISvcCtx, TOraType(Field.ObjectType));
      try
        OraObject.AllocObject;
      except
        OraObject.Free;
        raise;
      end;
      Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
    end;
    dtXML: begin
      OraObject := TOraXML.Create(FTable.OCISvcCtx, TOraType(Field.ObjectType));
      try
        OraObject.AllocObject;
      except
        OraObject.Free;
        raise;
      end;
      Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
    end;
    else
      inherited;
  end;
end;

procedure TTableData.FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  OraObject: TOraObject;
begin
  case Field.DataType of
    dtObject, dtReference, dtAnyData, dtXML: begin
      OraObject := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
      OraObject.Release;
    end
    else
      inherited;
  end;
end;

procedure TTableData.CopyComplexField(SourceRecBuf, DestRecBuf: IntPtr; Field: TFieldDesc);
var
  ValueOffset: Integer;
  SrcPtr: IntPtr;
  DestPtr: IntPtr;
  OraObjectSrc, OraObjectDest: TOraObject;
begin
  ValueOffset := Field.DataOffset;

  case Field.DataType of
    dtObject, dtReference: begin
      SrcPtr := Marshal.ReadIntPtr(SourceRecBuf, ValueOffset);
      DestPtr := Marshal.ReadIntPtr(DestRecBuf, ValueOffset);
      OraObjectSrc := TOraObject(GetGCHandleTarget(SrcPtr));
      OraObjectDest := TOraObject(GetGCHandleTarget(DestPtr));
      OraObjectDest.Assign(OraObjectSrc);
    end;
    else
      inherited;
  end;
end;

{ Fetching }

function TTableData.Fetch(FetchBack: boolean = False): boolean;
var
  Exists: tbool;
  Item: PItemHeader;
begin
  if FTable <> nil then begin
    FTable.CheckAlloc(False);
    if not FTable.IsNull then begin
      Check(FTable.OCI8.OCITableNext(FTable.OCISvcCtx.hOCIEnv, FTable.OCISvcCtx.hOCIError, LastIndex, FTable.Instance, LastIndex, Exists));
      Result := Exists = 1;
    end else
      Result := False;
  end
  else
    Result := False;

  if Result then begin
    BlockMan.AllocItem(Item);
    InitItem(Item);
    Marshal.WriteInt32(Item, SizeOf(TItemHeader) + FIndexOfs, LastIndex);

    if FTable.ItemType in [dtObject, dtReference] then
      FetchComplexFields(PtrOffset(Item, SizeOf(TItemHeader)), True);

    // Create Items
    if IntPtr(FirstItem) = nil then
      FirstItem := Item;

    Item.Prev := LastItem;
    Item.Next := nil;

    if IntPtr(LastItem) <> nil then begin
      LastItem.Next := Item;
      Item.Order := LastItem.Order + 1;
    end
    else
      Item.Order := 1;

    LastItem := Item;
  end;
end;

{ Edit }

procedure TTableData.InternalAppend(RecBuf: IntPtr);
var
  Exists: tbool;
  OraObject: TOraObject;
  Instance, Indicator: IntPtr;
begin
  Indicator := NullIndStruct;
  OraObject := nil;
  case FTable.ItemType of
    dtDateTime:
      Instance := NullOCIDate;
    dtInteger, dtLargeInt, dtFloat:
      Instance := NullOCINumber;
    dtString, dtWideString:
      Instance := NullOCIString;
    dtObject: begin
      OraObject := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf)));
      Instance := OraObject.Instance;
      Indicator := OraObject.Indicator;
    end;
  else
    Instance := nil;
    Assert(False, SUnknownDataType);
  end;

  Check(FTable.OCI8.OCICollAppend(FTable.OCISvcCtx.hOCIEnv, FTable.OCISvcCtx.hOCIError, Instance, Indicator, FTable.Instance));
  Check(FTable.OCI8.OCICollSize(FTable.OCISvcCtx.hOCIEnv, FTable.OCISvcCtx.hOCIError, FTable.Instance, LastIndex));
  Dec(LastIndex);
  if FTable.ItemType = dtObject then begin
    Exists := 1;
    FTable.OCI8.OCICollGetElem(FTable.OCISvcCtx.hOCIEnv, FTable.OCISvcCtx.hOCIError, FTable.Instance, LastIndex, Exists, Instance, Indicator);
    if OraObject.OCISvcCtx.Home.Direct then
      Instance := Marshal.ReadIntPtr(Instance);
    OraObject.ResetInstance(Instance, Indicator);
    with FTable do begin
      if FObjects = nil then
        FObjects := TList.Create;
      FObjects.Count := LastIndex + 1;
      FObjects[LastIndex] := OraObject;
      OraObject.AddRef;
    end;
  end;

  Marshal.WriteInt32(RecBuf, FIndexOfs, LastIndex);
end;

procedure TTableData.InternalDelete;
var
  Index: integer;
begin
  Index := Marshal.ReadInt32(CurrentItem, SizeOf(TItemHeader) + FIndexOfs);

  FTable.DeleteItem(Index);
end;

procedure TTableData.InternalUpdate;
begin
end;

procedure TTableData.CancelRecord(RecBuf: IntPtr);
var
  Index: integer;
  OraObject: TOraObject;
begin
  inherited;

  if FTable.ItemType = dtObject then begin
  // Updates table item to old value
    Index := Marshal.ReadInt32(CurrentItem, SizeOf(TItemHeader) + FIndexOfs);
    OraObject := TOraObject(GetGCHandleTarget(
      Marshal.ReadIntPtr(CurrentItem, SizeOf(TItemHeader))
    ));
    FTable.ItemAsObject[Index] := OraObject;
  end;
end;

procedure TTableData.SetTable(Value: TOraNestTable);
begin
  if Value <> FTable then begin
    if FTable <> nil then
      FTable.Release;

    FTable := Value;

    if FTable <> nil then
      FTable.AddRef;
  end;
end;

function GetObjectCacheMaxSize(OCISvcCtx: TOCISvcCtx): integer;
begin
  OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIAttrGet2(OCISvcCtx.hOCIEnv, OCI_HTYPE_ENV, Result, nil, OCI_ATTR_CACHE_MAX_SIZE, OCISvcCtx.hOCIError), OCISvcCtx);
end;

procedure SetObjectCacheMaxSize(OCISvcCtx: TOCISvcCtx; Value: integer);
begin
  OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIAttrSet2(OCISvcCtx.hOCIEnv, OCI_HTYPE_ENV, Value, 0, OCI_ATTR_CACHE_MAX_SIZE, OCISvcCtx.hOCIError), OCISvcCtx);
end;

function GetObjectCacheOptSize(OCISvcCtx: TOCISvcCtx): integer;
begin
  OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIAttrGet2(OCISvcCtx.hOCIEnv, OCI_HTYPE_ENV, Result, nil, OCI_ATTR_CACHE_OPT_SIZE, OCISvcCtx.hOCIError), OCISvcCtx);
end;

procedure SetObjectCacheOptSize(OCISvcCtx: TOCISvcCtx; Value: integer);
begin
  OCISvcCtx.OCI8.Check(OCISvcCtx.OCI8.OCIAttrSet2(OCISvcCtx.hOCIEnv, OCI_HTYPE_ENV, Value, 0, OCI_ATTR_CACHE_OPT_SIZE, OCISvcCtx.hOCIError), OCISvcCtx);
end;

function TTableData.GetRecordCount: Integer;
begin
  Result := LastIndex + 1;
end;

{$IFDEF CRDEBUG}
var
  i: integer;
{$ENDIF}

initialization
  ObjectTypes := nil;

  NotNullInd := Marshal.AllocHGlobal(sizeof(OCIInd));
  Marshal.WriteInt16(NotNullInd, OCI_IND_NOTNULL);
  NullInd := Marshal.AllocHGlobal(sizeof(OCIInd));
  Marshal.WriteInt16(NullInd, OCI_IND_NULL);
  NullIndStruct := Marshal.AllocHGlobal(100);
  FillChar(NullIndStruct, 100, $FF);
  NullOCINumber := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
  FillChar(NullOCINumber, OCI_NUMBER_SIZE, 0);
  NullOCIDate := Marshal.AllocHGlobal(SizeOf(OCIDate));
  FillChar(NullOCIDate, SizeOf(OCIDate), 0);
  NullOCIString := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(NullOCIString, nil);

finalization
{$IFDEF CRDEBUG}
  if ObjectTypes <> nil then
    with ObjectTypes.LockList do
    try
      i := Count;
    finally
      ObjectTypes.UnlockList;
    end
  else
    i := 0;
{$ENDIF}

  FreeAndNil(ObjectTypes);

  Marshal.FreeHGlobal(NotNullInd);
  Marshal.FreeHGlobal(NullInd);
  Marshal.FreeHGlobal(NullIndStruct);
  Marshal.FreeHGlobal(NullOCINumber);
  Marshal.FreeHGlobal(NullOCIDate);
  Marshal.FreeHGlobal(NullOCIString);

{$IFDEF CRDEBUG}
  Assert(i = 0, IntToStr(i) + ' ObjecType(s) hasn''t been released');
{$ENDIF}

end.

