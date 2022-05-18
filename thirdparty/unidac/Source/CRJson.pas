//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  CR JSON
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRJson;

interface

uses
  Classes, SysUtils,
  CRTypes, CRFunctions, CLRClasses, CRParser, CRBase64;

const
  lxExtJSONFirst  = 10000;
  lxExtJSONOID    = lxExtJSONFirst;

  cJSONOid        = '$oid';
  cJSONInt64      = '$numberLong';
  cJSONDate       = '$date';
  cJSONRegex      = '$regex';
  cJSONOptions    = '$options';
  cJSONTimestamp  = '$timestamp';
  cJSONBinary     = '$binary';
  cJSONType       = '$type';
  cJSONUndefined  = '$undefined';
  cJSONCode       = '$code';
  cJSONScope      = '$scope';
  cJSONMinKey     = '$minkey';
  cJSONMaxKey     = '$maxkey';
  cJSONRef        = '$ref';
  cJSONId         = '$id';
  cJSONDecimal128 = '$numberDecimal';
  cJSONTime       = 't';
  cJSONIncrement  = 'i';

  cAttrPattern   = 'regex';
  cAttrOptions   = 'options';
  cAttrCode      = 'code';
  cAttrScope     = 'scope';
  cAttrBinary    = 'binary';
  cAttrSubtype   = 'type';
  cAttrName      = 'ref';
  cAttrValue     = 'id';
  cAttrTime      = 't';
  cAttrIncrement = 'i';

  cTrue          = 'true';
  cFalse         = 'false';
  cNull          = 'null';

  cDateFormat    = 'YYYY-MM-DD';
  cTimeFormat    = 'hh:nn:ss';{TODO: }//.zzz';

  cInvalidObject     = 'Invalid Object';
  cInvalidArray      = 'Invalid Array';
  cInvalidString     = 'Invalid String';
  cInvalidNumber     = 'Invalid Number';
  cInvalidBoolean    = 'Invalid Boolean';
  cInvalidObjectId   = 'Invalid ObjectId';
  cInvalidUndefined  = 'Invalid Undefined';
  cInvalidInt32      = 'Invalid Integer';
  cInvalidInt64      = 'Invalid Int64';
  cInvalidDateTime   = 'Invalid DateTime';
  cInvalidTimeStamp  = 'Invalid TimeStamp';
  cInvalidKey        = 'Invalid Key';
  cInvalidDBPointer  = 'Invalid DBPointer';
  cInvalidRegex      = 'Invalid Regex';
  cInvalidJavaCode   = 'Invalid JavaScript';
  cInvalidBinary     = 'Invalid Binary';
  cInvalidDouble     = 'Invalid Double';
  cInvalidDecimal128 = 'Invalid Decimal128';
  cInvalidValue      = 'Invalid value';
  cInvalidValueSeparator = 'Invalid value separator';

  cDecimal128IsBinary = 'The value has the binary Decimal128 format';

  cJSONOidSize = 12;
  cJSONDecimal128Size = 16;
  cJSONDecimal128Length = 43;

type
  TJSONOid = array[0..cJSONOidSize - 1] of byte;
  PJSONOid = ^TJSONOid;

  TStringDecimal128 = string;

  TBinaryDecimal128 = record
    Low,
    High: Int64;
  end;
  PBinaryDecimal128 = ^TBinaryDecimal128;

  TDecimal128 = TStringDecimal128;

  TJSONTag = (jtNone, jtObject, jtObjectEnd, jtPair, jtArray, jtArrayEnd, jtString, jtNumber, jtBoolean, jtNull, jtComma, jtColon,
              jtObjectId, jtInt32, jtInt64, jtDateTime, jtJavaCode, jtUndefined, jtJavaScopeCode, jtRegex, jtTimestamp, jtBinary,
              jtDouble, jtBytes, jtMinKey, jtMaxKey, jtDBPointer, jtDecimal128);

  TJSONStringType = (jsAnsiString, jsWideString);

  TJSONBinaryType = (
    btDocumentEnd   = $0,
    btDouble        = $01,
    btString        = $02,
    btObject        = $03,
    btArray         = $04,
    btBinary        = $05,
    btUndefined     = $06,
    btObjectId      = $07,
    btBoolean       = $08,
    btDateTime      = $09,
    btNull          = $0A,
    btRegex         = $0B,
    btDBPointer     = $0C,
    btJavaCode      = $0D,
    btSymbol        = $0E,
    btJavaScopeCode = $0F,
    btInt32         = $10,
    btTimeStamp     = $11,
    btInt64         = $12,
    btDecimal128    = $13,
    btMaxKey        = $7F,
    btMinKey        = $FF
  );

  TJSONBinarySubtype = (
    bsGeneric     = $0,
    bsFunction    = $01,
    bsBinaryOld   = $02,
    bsUUIDOld     = $03,
    bsUUID        = $04,
    bsMD5         = $05,
    bsUserDefined = $80
  );

  TDeserializationMode = (dmJSON, dmCommand);

  TJSONValue = class
  private
    FTag: TJSONTag;
    FParent: TJSONValue;
    FSize: integer;
    FActualName: string;

    procedure SetSize(const AValue: integer); virtual;
    procedure SetActualName(const Value: string); virtual;
  protected
    FData: IntPtr;
    FStream: TStream;
    FStartPos: Int64;
    FEndPos: Int64;

    function GetAsString: string; virtual;
    function GetValuePtr: IntPtr; virtual;
  public
    constructor Create(const Tag: TJSONTag; const Parent: TJSONValue); overload;
    constructor Create(const Parent: TJSONValue); overload; virtual;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; virtual; abstract;
    procedure Copy(const Source: TJSONValue); virtual;
    function IsEqual(const AValue: TJSONValue): boolean; virtual;

    property Parent: TJSONValue read FParent write FParent;
    property Tag: TJSONTag read FTag;
    property Data: IntPtr read FData;

    property Size: integer read FSize write SetSize;
    property ActualName: string read FActualName write SetActualName;

    property AsString: string read GetAsString;
    property ValuePtr: IntPtr read GetValuePtr;
  end;

  TJSONNull = class(TJSONValue)
  protected
    function GetAsString: string; override;
    function GetValuePtr: IntPtr; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
  end;

  TJSONString = class(TJSONValue)
  private
    FStringType: TJSONStringType;

    function GetValue: string;
    procedure SetValue(const AValue: string);

    function GetAsAnsiString: AnsiString; virtual;
    procedure SetAsAnsiString(const AValue: AnsiString); virtual;
    function GetAsWideString: WideString; virtual;
    procedure SetAsWideString(const AValue: WideString); virtual;
  protected
    function GetAsString: string; override;
    procedure InitValue(const ASize: integer); virtual;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Value: string read GetValue write SetValue;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end;

  TJSONAnsiString = class(TJSONString)
  private
    FValue: AnsiString;

    function GetAsAnsiString: AnsiString; override;
    procedure SetAsAnsiString(const AValue: AnsiString); override;
    function GetAsWideString: WideString; override;
    procedure SetAsWideString(const AValue: WideString); override;
  protected
    function GetValuePtr: IntPtr; override;
    procedure InitValue(const ASize: integer); override;
  public
    constructor Create(const Parent: TJSONValue; const AValue: string); overload;
  end;

  TJSONWideString = class(TJSONString)
  private
    FValue: WideString;

    function GetAsAnsiString: AnsiString; override;
    procedure SetAsAnsiString(const AValue: AnsiString); override;
    function GetAsWideString: WideString; override;
    procedure SetAsWideString(const AValue: WideString); override;
  protected
    function GetValuePtr: IntPtr; override;
    procedure InitValue(const ASize: integer); override;
  public
    constructor Create(const Parent: TJSONValue; const AValue: string); overload;
  end;

  TJSONNumber = class(TJSONAnsiString)
  private
    FSubtype: TJSONTag;
  public
    constructor Create(const Parent: TJSONValue); override;
    constructor Create(const Parent: TJSONValue; const AValue: string; const ASubtype: TJSONTag); overload;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Subtype: TJSONTag read FSubtype;
  end;

  TJSONDecimal128 = class(TJSONAnsiString)
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;
  end;

  TJSONBoolean = class(TJSONValue)
  private
    FValue: boolean;
  protected
    function GetAsString: string; override;
    function GetValuePtr: IntPtr; override;
  public
    constructor Create(const Parent: TJSONValue); override;
    constructor Create(const Parent: TJSONValue; const AValue: boolean); overload;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Value: boolean read FValue write FValue;
  end;

  TJSONPair = class(TJSONValue)
  private
    FName: TJSONString;
    FValue: TJSONValue;

    procedure SetName(const AValue: TJSONString);
    procedure SetValue(const AValue: TJSONValue);
  public
    constructor Create(const Parent: TJSONValue); overload; override;
    constructor Create(const Parent: TJSONValue; const AName: string; const Unicode: boolean); overload;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;
    function IsEqual(const AValue: TJSONValue): boolean; override;

    property Name: TJSONString read FName write SetName;
    property Value: TJSONValue read FValue write SetValue;
  end;

  TJSONPairs = class({$IFDEF NEXTGEN}TCRObjectList{$ELSE}TList{$ENDIF})
  private
    function GetItem(Index: Integer): TJSONPair;
    procedure PutItem(Index: Integer; const Value: TJSONPair);
  public
  {$IFNDEF NEXTGEN}
    procedure Clear; override;
  {$ENDIF}

    property Items[Index: Integer]: TJSONPair read GetItem write PutItem; default;
  end;

  TJSONObject = class(TJSONValue)
  private
    FPairs: TJSONPairs;

    function GetValue(const Name: string): TJSONValue;
  protected
    procedure SetActualName(const Value: string); override;
  public
    constructor Create(const Parent: TJSONValue); override;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;
    function IsEqual(const AValue: TJSONValue): boolean; override;

    procedure AddPair(const Pair: TJSONPair);
    function GetPair(const Name: string): TJSONPair;
    procedure DeletePair(Pair: TJSONPair);

    property Pairs: TJSONPairs read FPairs;
    property ValueByName[const Name: string]: TJSONValue read GetValue; default;
  end;

  TJSONElements = class({$IFDEF NEXTGEN}TCRObjectList{$ELSE}TList{$ENDIF})
  private
    function GetItem(Index: Integer): TJSONValue;
    procedure PutItem(Index: Integer; const Value: TJSONValue);
  public
  {$IFNDEF NEXTGEN}
    procedure Clear; override;
  {$ENDIF}

    property Items[Index: Integer]: TJSONValue read GetItem write PutItem; default;
  end;

  TJSONArray = class(TJSONValue)
  private
    FElements: TJSONElements;
  protected
    procedure SetActualName(const Value: string); override;
  public
    constructor Create(const Parent: TJSONValue); override;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;
    function IsEqual(const AValue: TJSONValue): boolean; override;

    function AddElement(const Element: TJSONValue): integer;
    procedure DeleteElement(Element: TJSONValue);

    property Elements: TJSONElements read FElements;
  end;

  TJSONObjectId = class(TJSONValue)
  private
    FValue: TJSONOid;

  {$IFDEF VER12P}
    procedure SetValue(const AValue: TJSONOid);
  {$ENDIF}  
  protected
    function GetAsString: string; override;
    function GetValuePtr: IntPtr; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

  {$IFDEF VER12P}
    property Value: TJSONOid read FValue write SetValue;
  {$ELSE}
    function GetValue: TJSONOid;
    procedure SetValue(const AValue: TJSONOid);
  {$ENDIF}
  end;

  TJSONInt32 = class(TJSONValue)
  private
    FValue: integer;
  protected
    function GetAsString: string; override;
    function GetValuePtr: IntPtr; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Value: integer read FValue write FValue;
  end;

  TJSONInt64 = class(TJSONValue)
  private
    FValue: Int64;
  protected
    function GetAsString: string; override;
    function GetValuePtr: IntPtr; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Value: Int64 read FValue write FValue;
  end;

  TJSONDateTime = class(TJSONValue)
  private
    FValue: TDateTime;
  protected
    function GetAsString: string; override;
    function GetValuePtr: IntPtr; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Value: TDateTime read FValue write FValue;
  end;

  TJSONJavaCode = class(TJSONValue)
  private
    FCode: TJSONString;

    procedure SetCode(const AValue: TJSONString);
  protected
    procedure SetActualName(const Value: string); override;
  public
    constructor Create(const Parent: TJSONValue); override;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Code: TJSONString read FCode write SetCode;
  end;

  TJSONJavaScopeCode = class(TJSONJavaCode)
  private
    FScope: TJSONObject;
  protected
    procedure SetActualName(const Value: string); override;
  public
    constructor Create(const Parent: TJSONValue); override;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Scope: TJSONObject read FScope write FScope;
  end;

  TJSONUndefined = class(TJSONNull)
  protected
    function GetAsString: string; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
  end;

  TJSONMinKey = class(TJSONNull)
  protected
    function GetAsString: string; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
  end;

  TJSONMaxKey = class(TJSONNull)
  protected
    function GetAsString: string; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
  end;

  TJSONRegex = class(TJSONValue)
  private
    FPattern,
    FOptions: TJSONString;

    procedure SetPattern(const AValue: TJSONString);
    procedure SetOptions(const AValue: TJSONString);
    procedure SetActualName(const Value: string); override;
  public
    constructor Create(const Parent: TJSONValue); override;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Pattern: TJSONString read FPattern write SetPattern;
    property Options: TJSONString read FOptions write SetOptions;
  end;

  TJSONTimestamp = class(TJSONValue)
  private
    FTimestamp,
    FIncrement: TJSONInt32;

    procedure SetTimestamp(const AValue: TJSONInt32);
    procedure SetIncrement(const AValue: TJSONInt32);
    procedure SetActualName(const Value: string); override;
  public
    constructor Create(const Parent: TJSONValue); override;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Timestamp: TJSONInt32 read FTimestamp write SetTimestamp;
    property Increment: TJSONInt32 read FIncrement write SetIncrement;
  end;

  TJSONBytes = class(TJSONValue)
  private
    FValue: TBytes;

    procedure SetSize(const AValue: integer); override;
  {$IFDEF VER12P}
    procedure SetValue(const AValue: TBytes);
  {$ENDIF}  
  protected
    function GetAsString: string; override;
    function GetValuePtr: IntPtr; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

  {$IFDEF VER12P}
    property Value: TBytes read FValue write SetValue;
  {$ELSE}
    function GetValue: TBytes;
    procedure SetValue(const AValue: TBytes);
  {$ENDIF}
  end;

  TJSONBinary = class(TJSONValue)
  private
    FSubtype: TJSONInt32;
    FBinary: TJSONBytes;

    procedure SetSubtype(const AValue: TJSONInt32);
    procedure SetValue(const AValue: TJSONBytes);
    procedure SetActualName(const Value: string); override;
  protected
    function GetAsString: string; override;
  public
    constructor Create(const Parent: TJSONValue); override;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Subtype: TJSONInt32 read FSubtype write SetSubtype;
    property Binary: TJSONBytes read FBinary write SetValue;
  end;

  TJSONDouble = class(TJSONValue)
  private
    FValue: double;
  protected
    function GetAsString: string; override;
    function GetValuePtr: IntPtr; override;
  public
    constructor Create(const Parent: TJSONValue); override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Value: double read FValue write FValue;
  end;

  TJSONDBPointer = class(TJSONValue)
  private
    FName: TJSONString;
    FValue: TJSONObjectId;

    procedure SetName(const AValue: TJSONString);
  {$IFDEF VER12P}
    procedure SetValue(const AValue: TJSONObjectId);
  {$ENDIF}
    procedure SetActualName(const Value: string); override;
  public
    constructor Create(const Parent: TJSONValue); override;
    destructor Destroy; override;

    function Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue; override;
    procedure Copy(const Source: TJSONValue); override;

    property Name: TJSONString read FName write SetName;
  {$IFDEF VER12P}
    property Value: TJSONObjectId read FValue write SetValue;
  {$ELSE}
    function GetValue: TJSONObjectId;
    procedure SetValue(const AValue: TJSONObjectId);
  {$ENDIF}
  end;

  TCustomJSONWriter = class
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear; virtual; abstract;

    procedure WriteTag(const Value: TJSONValue); virtual;
    procedure WriteElementIndex(const Value: integer); virtual;

    procedure WriteObjectBegin(const Size: Cardinal = 0); virtual; abstract;
    procedure WriteObjectEnd; virtual; abstract;
    procedure WriteArrayBegin(const Size: Cardinal = 0); virtual; abstract;
    procedure WriteArrayEnd; virtual; abstract;
    procedure WriteJavaBegin(const Size: Cardinal = 0); virtual; abstract;
    procedure WriteJavaCodeBegin; virtual; abstract;
    procedure WriteJavaCodeEnd; virtual; abstract;
    procedure WriteJavaScopeBegin; virtual; abstract;
    procedure WriteJavaScopeEnd; virtual; abstract;
    procedure WriteRegexPatternBegin; virtual; abstract;
    procedure WriteRegexPatternEnd; virtual; abstract;
    procedure WriteRegexOptionsBegin; virtual; abstract;
    procedure WriteRegexOptionsEnd; virtual; abstract;
    procedure WriteDBPointerBegin; virtual; abstract;
    procedure WriteDBPointerIdBegin; virtual; abstract;
    procedure WriteOid(const Value: TJSONOid); virtual; abstract;
    procedure WriteDBPointerEnd; virtual; abstract;
    procedure WriteElementSeparator; virtual; abstract;
    procedure WriteValueSeparator; virtual; abstract;
    procedure WriteQuote; virtual; abstract;

    procedure WriteNumber(const Value: string; const Subtype: TJSONTag); virtual; abstract;
    procedure WriteString(const Value: string; WriteSize: boolean = False; Escape: boolean = False);
    procedure WriteAnsiString(const Value: AnsiString; const WriteSize, Escape: boolean); virtual; abstract;
    procedure WriteWideString(const Value: WideString; const WriteSize, Escape: boolean); virtual; abstract;
    procedure WriteBoolean(const Value: boolean); virtual; abstract;
    procedure WriteNull; virtual; abstract;

    procedure WriteObjectId(const Value: TJSONOid); virtual; abstract;
    procedure WriteInt32(const Value: integer); virtual; abstract;
    procedure WriteInt64(const Value: Int64; ExtFormat: boolean = True); virtual; abstract;
    procedure WriteDateTime(const Value: TDateTime); virtual; abstract;
    procedure WriteUndefined; virtual; abstract;
    procedure WriteRegex(const Pattern, Options: string); virtual; abstract;
    procedure WriteTimestamp(const Timestamp: TJSONTimestamp); virtual; abstract;
    procedure WriteBinary(const Value: TBytes; const Subtype: TJSONBinarySubtype); virtual; abstract;
    procedure WriteDouble(const Value: double); virtual; abstract;
    procedure WriteMinKey; virtual; abstract;
    procedure WriteMaxKey; virtual; abstract;
    procedure WriteDecimal128(const Value: TDecimal128); virtual; abstract;
  end;

  TJSONWriterClass = class of TCustomJSONWriter;

  TJSONStreamWriter = class(TCustomJSONWriter)
  private
    FStream: TStream;
    FStreamOwner: boolean;

    procedure SetStream(Value: TStream);

  public
    constructor Create; overload; override;
    constructor Create(AStream: TStream); reintroduce; overload;
    destructor Destroy; override;

    procedure Clear; override;

    property Stream: TStream read FStream write SetStream;
  end;

  TJSONTextWriter = class(TJSONStreamWriter)
  protected
    FTokenType: TJSONTag;
    procedure InternalWriteAsBytes(const Value: TBytes; const Escape: boolean);
    procedure InternalWriteAsString(const Value: string; const Escape: boolean); virtual;
    procedure InternalWriteQuotedString(const Value: string; const Escape: boolean);
  public
    constructor Create; override;

    procedure Clear; override;

    procedure WriteObjectBegin(const Size: Cardinal = 0); override;
    procedure WriteObjectEnd; override;
    procedure WriteArrayBegin(const Size: Cardinal = 0); override;
    procedure WriteArrayEnd; override;
    procedure WriteJavaBegin(const Size: Cardinal = 0); override;
    procedure WriteJavaCodeBegin; override;
    procedure WriteJavaCodeEnd; override;
    procedure WriteJavaScopeBegin; override;
    procedure WriteJavaScopeEnd; override;
    procedure WriteRegexPatternBegin; override;
    procedure WriteRegexPatternEnd; override;
    procedure WriteRegexOptionsBegin; override;
    procedure WriteRegexOptionsEnd; override;
    procedure WriteDBPointerBegin; override;
    procedure WriteDBPointerIdBegin; override;
    procedure WriteOid(const Value: TJSONOid); override;
    procedure WriteDBPointerEnd; override;
    procedure WriteElementSeparator; override;
    procedure WriteValueSeparator; override;
    procedure WriteQuote; override;

    procedure WriteNull; override;
    procedure WriteUndefined; override;
    procedure WriteNumber(const Value: string; const Subtype: TJSONTag); override;
    procedure WriteAnsiString(const Value: AnsiString; const WriteSize, Escape: boolean); override;
    procedure WriteWideString(const Value: WideString; const WriteSize, Escape: boolean); override;
    procedure WriteBoolean(const Value: boolean); override;
    procedure WriteInt32(const Value: integer); override;
    procedure WriteInt64(const Value: Int64; ExtFormat: boolean = True); override;
    procedure WriteDateTime(const Value: TDateTime); override;
    procedure WriteRegex(const Pattern, Options: string); override;
    procedure WriteTimestamp(const Timestamp: TJSONTimestamp); override;
    procedure WriteBinary(const Value: TBytes; const Subtype: TJSONBinarySubtype); override;
    procedure WriteDouble(const Value: double); override;
    procedure WriteMinKey; override;
    procedure WriteMaxKey; override;
    procedure WriteDecimal128(const Value: TDecimal128); override;
    procedure WriteObjectId(const Value: TJSONOid); override;

    class function CalcSize(const Value: TJSONValue; const FieldMap: TStringList): integer;

    function AsBytes: TBytes;
    function AsText: string;
  end;

  TJSONBinaryWriter = class(TJSONStreamWriter)
  private
    procedure InternalWriteByte(const Value: byte);
    procedure InternalWriteBytes(const Value: TBytes);
    procedure InternalWriteInt32(const Value: integer);
    procedure InternalWriteInt64(const Value: Int64);
  public
    procedure WriteTag(const Value: TJSONValue); override;
    procedure WriteElementIndex(const Value: integer); override;

    procedure WriteObjectBegin(const Size: Cardinal = 0); override;
    procedure WriteObjectEnd; override;
    procedure WriteArrayBegin(const Size: Cardinal = 0); override;
    procedure WriteArrayEnd; override;
    procedure WriteJavaBegin(const Size: Cardinal = 0); override;
    procedure WriteJavaCodeBegin; override;
    procedure WriteJavaCodeEnd; override;
    procedure WriteJavaScopeBegin; override;
    procedure WriteJavaScopeEnd; override;
    procedure WriteRegexPatternBegin; override;
    procedure WriteRegexPatternEnd; override;
    procedure WriteRegexOptionsBegin; override;
    procedure WriteRegexOptionsEnd; override;
    procedure WriteDBPointerBegin; override;
    procedure WriteDBPointerIdBegin; override;
    procedure WriteOid(const Value: TJSONOid); override;
    procedure WriteDBPointerEnd; override;
    procedure WriteQuote; override;

    procedure WriteNumber(const Value: string; const Subtype: TJSONTag); override;
    procedure WriteAnsiString(const Value: AnsiString; const WriteSize, Escape: boolean); override;
    procedure WriteWideString(const Value: WideString; const WriteSize, Escape: boolean); override;
    procedure WriteBoolean(const Value: boolean); override;
    procedure WriteNull; override;

    procedure WriteObjectId(const Value: TJSONOid); override;
    procedure WriteInt32(const Value: integer); override;
    procedure WriteInt64(const Value: Int64; ExtFormat: boolean = True); override;
    procedure WriteDateTime(const Value: TDateTime); override;
    procedure WriteUndefined; override;
    procedure WriteRegex(const Pattern, Options: string); override;
    procedure WriteTimestamp(const Timestamp: TJSONTimestamp); override;
    procedure WriteBinary(const Value: TBytes; const Subtype: TJSONBinarySubtype); override;
    procedure WriteDouble(const Value: double); override;
    procedure WriteMinKey; override;
    procedure WriteMaxKey; override;
    procedure WriteDecimal128(const Value: TDecimal128); override;

    procedure WriteElementSeparator; override;
    procedure WriteValueSeparator; override;

    class function CalcSize(const Value: TJSONValue): integer;
  end;

  TJSONSerializer = class
  private
    FWriter: TCustomJSONWriter;

    procedure InitWriter(const WriterClass: TJSONWriterClass);
    procedure FreeWriter;
  protected
    class function GetTextWriterClass: TJSONWriterClass; virtual;

    procedure ProcessValue(const Value: TJSONValue);
    procedure ProcessObject(const Value: TJSONObject);
    procedure ProcessArray(const Value: TJSONArray);
    procedure ProcessString(const Value: TJSONString; const WriteSize, Escape: boolean);
    procedure ProcessNumber(const Value: TJSONNumber);
    procedure ProcessBoolean(const Value: TJSONBoolean);
    procedure ProcessNull(const Value: TJSONNull);
    procedure ProcessObjectId(const Value: TJSONObjectId);
    procedure ProcessInt32(const Value: TJSONInt32);
    procedure ProcessInt64(const Value: TJSONInt64);
    procedure ProcessDateTime(const Value: TJSONDateTime);
    procedure ProcessJavaCode(const Value: TJSONJavaCode);
    procedure ProcessUndefined(const Value: TJSONUndefined);
    procedure ProcessJavaScopeCode(const Value: TJSONJavaScopeCode);
    procedure ProcessRegex(const Value: TJSONRegex);
    procedure ProcessTimestamp(const Value: TJSONTimestamp);
    procedure ProcessBinary(const Value: TJSONBinary);
    procedure ProcessDouble(const Value: TJSONDouble);
    procedure ProcessMinKey(const Value: TJSONMinKey);
    procedure ProcessMaxKey(const Value: TJSONMaxKey);
    procedure ProcessDBPointer(const Value: TJSONDBPointer);
    procedure ProcessDecimal128(const Value: TJSONDecimal128);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ToBinary(const Value: TJSONValue; const Stream: TStream);
    procedure ToText(const Value: TJSONValue; out Text: string); overload;
    procedure ToText(const Value: TJSONValue; const Stream: TStream); overload;
  end;

  TCustomJSONReader = class
  private
    FTokenType: TJSONTag;
    FTagPos: Int64;
    FAllowedArrayTerminators,
    FRestrictedArrayTerminators: set of TJSONTag;
    procedure Check(Condition: boolean; const Msg: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Initialize; virtual; abstract;
    procedure Finalize; virtual; abstract;

    function ReadTag: TJSONTag; virtual; abstract;
    function ReadExtTag(const Tag: TJSONTag): TJSONTag; virtual; abstract;
    function ReadObjectSize: integer; virtual; abstract;
    function ReadArraySize: integer; virtual; abstract;
    function ReadElementName: string; virtual; abstract;
    function ReadString: string; overload;
    function ReadString(const ZeroTerminated, Unescape: boolean; out Size: integer): string; overload;
    function ReadAnsiString(const ZeroTerminated, Unescape: boolean; out Size: integer): AnsiString; virtual; abstract;
    function ReadWideString(const ZeroTerminated, Unescape: boolean; out Size: integer): WideString; virtual; abstract;
    function ReadValueSeparator: boolean; virtual; abstract;
    function ReadElementSeparator: boolean; virtual; abstract;
    function ReadNumber(out Subtype: TJSONTag): string; virtual; abstract;
    function ReadBoolean: boolean; virtual; abstract;
    function ReadOid: TJSONOid; virtual; abstract;
    procedure ReadUndefined; virtual; abstract;
    function ReadInt32: Integer; virtual; abstract;
    function ReadInt64: Int64; virtual; abstract;
    function ReadDateTime: TDateTime; virtual; abstract;
    function ReadTimestampTime: Cardinal; virtual; abstract;
    function ReadTimestampIncrement: Cardinal; virtual; abstract;
    procedure ReadMinMaxKey; virtual; abstract;
    function ReadDBPointerId: TJSONOid; virtual; abstract;
    procedure ReadRegexOptionsBegin; virtual; abstract;
    procedure ReadRegexOptionsEnd; virtual; abstract;
    procedure ReadJavaCodeBegin; virtual; abstract;
    function ReadJavaCode(out HasScope: boolean; out Size: integer): string; virtual; abstract;
    procedure ReadJavaScopeEnd; virtual; abstract;
    function ReadBinary(var Data: TBytes; out Subtype: integer): integer; virtual; abstract;
    function ReadDouble: double; virtual; abstract;
    procedure ReadDecimal128(var Value: TDecimal128); virtual; abstract;

    function GetTagPos: Int64; virtual;
    function GetCurPos: Int64; virtual; abstract;

    property TokenType: TJSONTag read FTokenType;
  end;

  TJSONParser = class(TParser)
  protected
    procedure InitParser; override;
    procedure ToRightQuoteP(RightQuote: char); override;
  end;

  TJSONReaderClass = class of TJSONStreamReader;

  TJSONStreamReader = class(TCustomJSONReader)
  private
    FStream: TStream;
    FStartStreamPos: Int64;
    FStreamOwner: boolean;
    FStreamSize: Int64;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetStream(const Stream: TStream; Size: Int64 = -1);
  end;

  TJSONTextReader = class(TJSONStreamReader)
  protected
    FParser: TJSONParser;

    function InternalUnescape(const Value: string): string;
    function InternalReadString(const Unescape: boolean): string; virtual;
    procedure InternalReadLexem; virtual;
  public
    Code: integer;
    Lexem: string;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure Finalize; override;

    function GetCurPos: Int64; override;
    procedure Back;

    function ReadTag: TJSONTag; override;
    function ReadExtTag(const Tag: TJSONTag): TJSONTag; override;
    function ReadObjectSize: integer; override;
    function ReadArraySize: integer; override;
    function ReadElementName: string; override;
    function ReadAnsiString(const ZeroTerminated, Unescape: boolean; out Size: integer): AnsiString; override;
    function ReadWideString(const ZeroTerminated, Unescape: boolean; out Size: integer): WideString; override;
    function ReadValueSeparator: boolean; override;
    function ReadElementSeparator: boolean; override;
    function ReadNumber(out Subtype: TJSONTag): string; override;
    function ReadBoolean: boolean; override;
    function ReadOid: TJSONOid; override;
    procedure ReadUndefined; override;
    function ReadInt32: integer; override;
    function ReadInt64: Int64; override;
    function ReadDateTime: TDateTime; override;
    function ReadTimestampTime: Cardinal; override;
    function ReadTimestampIncrement: Cardinal; override;
    procedure ReadMinMaxKey; override;
    function ReadDBPointerId: TJSONOid; override;
    procedure ReadRegexOptionsBegin; override;
    procedure ReadRegexOptionsEnd; override;
    procedure ReadJavaCodeBegin; override;
    function ReadJavaCode(out HasScope: boolean; out Size: integer): string; override;
    procedure ReadJavaScopeEnd; override;
    function ReadBinary(var Data: TBytes; out Subtype: integer): integer; override;
    function ReadDouble: double; override;
    procedure ReadDecimal128(var Value: TDecimal128); override;
    procedure Skip;

    procedure SetText(const Text: string);
    procedure SetBytes(const Binary: TBytes);
  end;

  TJSONBinaryReader = class(TJSONStreamReader)
  private
    FInProcess: boolean;

    function InternalReadByte: byte;
    function InternalReadInt32: integer;
    function InternalReadInt64: Int64;
    procedure InternalReadBytes(var Value: TBytes; const ZeroTerminated: boolean);
    procedure InternalReadBuffer(var Buffer; const Size: NativeInt);
  public
    procedure Initialize; override;
    procedure Finalize; override;

    function GetCurPos: Int64; override;

    function ReadTag: TJSONTag; override;
    function ReadExtTag(const Tag: TJSONTag): TJSONTag; override;
    function ReadObjectSize: integer; override;
    function ReadArraySize: integer; override;
    function ReadElementName: string; override;
    function ReadAnsiString(const ZeroTerminated, Unescape: boolean; out Size: integer): AnsiString; override;
    function ReadWideString(const ZeroTerminated, Unescape: boolean; out Size: integer): WideString; override;
    function ReadValueSeparator: boolean; override;
    function ReadElementSeparator: boolean; override;
    function ReadBoolean: boolean; override;
    function ReadOid: TJSONOid; override;
    procedure ReadUndefined; override;
    function ReadInt32: integer; override;
    function ReadInt64: Int64; override;
    function ReadDateTime: TDateTime; override;
    function ReadTimestampTime: Cardinal; override;
    function ReadTimestampIncrement: Cardinal; override;
    procedure ReadMinMaxKey; override;
    function ReadDBPointerId: TJSONOid; override;
    procedure ReadRegexOptionsBegin; override;
    procedure ReadRegexOptionsEnd; override;
    procedure ReadJavaCodeBegin; override;
    function ReadJavaCode(out HasScope: boolean; out Size: integer): string; override;
    function ReadBinary(var Data: TBytes; out Subtype: integer): integer; override;
    function ReadDouble: double; override;
    procedure ReadDecimal128(var Value: TDecimal128); override;
  end;

  TJSONDeserializer = class
  private
    FData: IntPtr;

    procedure InitReader(const ReaderClass: TJSONReaderClass);
    procedure FreeReader;

    function CreateString(const Parent: TJSONValue): TJSONString;

  protected
    FReader: TJSONStreamReader;
    FReaderOwner: boolean;
    FUseUnicode: boolean;

    class function GetTextReaderClass: TJSONReaderClass; virtual;

    function Process: TJSONValue;
    function ProcessTag(const Tag: TJSONTag; const Parent: TJSONValue): TJSONValue;
    function ProcessObject(const Parent: TJSONValue): TJSONObject;
    function ProcessArray(const Parent: TJSONValue): TJSONArray;
    function ProcessNumber(const Parent: TJSONValue): TJSONNumber;
    function ProcessString(const Parent: TJSONValue; const Unescape: boolean): TJSONString;
    function ProcessBoolean(const Parent: TJSONValue): TJSONBoolean;
    function ProcessNull(const Parent: TJSONValue): TJSONNull;
    function ProcessUndefined(const Parent: TJSONValue): TJSONUndefined;
    function ProcessObjectId(const Parent: TJSONValue): TJSONObjectId;
    function ProcessInt32(const Parent: TJSONValue): TJSONInt32;
    function ProcessInt64(const Parent: TJSONValue): TJSONInt64;
    function ProcessDateTime(const Parent: TJSONValue): TJSONDateTime;
    function ProcessTimestamp(const Parent: TJSONValue): TJSONTimestamp;
    function ProcessMinKey(const Parent: TJSONValue): TJSONMinKey;
    function ProcessMaxKey(const Parent: TJSONValue): TJSONMaxKey;
    function ProcessDBPointer(const Parent: TJSONValue): TJSONDBPointer;
    function ProcessJavaCode(const Parent: TJSONValue): TJSONJavaCode;
    function ProcessJavaScopeCode(const Parent: TJSONValue): TJSONJavaScopeCode;
    function ProcessBinary(const Parent: TJSONValue): TJSONBinary;
    function ProcessDouble(const Parent: TJSONValue): TJSONDouble;
    function ProcessDecimal128(const Parent: TJSONValue): TJSONDecimal128;
    function ProcessCString(const Parent: TJSONValue; const Unescape: boolean): TJSONString;
    function ProcessPair(const Parent: TJSONObject; const Tag: TJSONTag): TJSONPair; virtual;
    function ProcessRegex(const Parent: TJSONValue): TJSONRegex; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function FromText(const Text: string; Data: IntPtr = nil; const UseUnicode: boolean = True): TJSONValue; overload;
    function FromText(Stream: TStream; Data: IntPtr = nil; const UseUnicode: boolean = True): TJSONValue; overload;
    function FromBinary(Stream: TStream; Data: IntPtr = nil; const UseUnicode: boolean = True): TJSONValue;
    function FromJSONReader(JSONReader: TJSONStreamReader; Data: IntPtr = nil; const UseUnicode: boolean = True): TJSONValue;
  end;

const
  JSONComplexTags = [jtObject, jtArray, jtRegex, jtJavaCode, jtJavaScopeCode, jtBinary, jtDBPointer, jtTimestamp, jtDecimal128];
  JSONConstantTags = [jtNull, jtUndefined, jtMinKey, jtMaxKey];

function StringToOid(const Value: string): TJSONOid;

function Decimal128ToString(const Value: PBinaryDecimal128): string;
function StringToDecimal128(const Value: TStringDecimal128; const Decimal: PBinaryDecimal128): boolean;

function IsQuoted(const Value: string): boolean;
function Quote(const Value: string): string;
function UnQuote(const Value: string): string;

implementation

uses
  Math, DateUtils;

const
  BufferSize = 1024;

{$IFDEF USE_TFORMATSETTINGS}
var
  DateTimeFormat: TFormatSettings;
{$ENDIF}

function IsQuoted(const Value: string): boolean;
var
  l: integer;
begin
  l := Length(Value);
  if (l <= 1) then
    Result := False
  else
    Result := (Value[1] = '"') and (Value[l] = '"');
end;

function Quote(const Value: string): string;
begin
  if not IsQuoted(Value) then
    Result := '"' + Value + '"'
  else
    Result := Value;
end;

function UnQuote(const Value: string): string;
begin
  if IsQuoted(Value) then
    Result := Copy(Value, 2, length(Value) - 2)
  else
    Result := Value;
end;

function BinToHex(Buffer: IntPtr; BufferSize: integer): string;
const
  Convert: array[0..15] of char = '0123456789ABCDEF';
var
  i, n: integer;
  b: byte;
begin
  SetLength(Result, BufferSize * 2);

  n := 1;
  for i := 0 to BufferSize - 1 do begin
    b := Byte(PtrOffset(Buffer, i)^);
    Result[n] := Convert[b shr 4];
    Result[n + 1] := Convert[b and $F];
    Inc(n, 2);
  end;
end;

function StringToOid(const Value: string): TJSONOid;
var
  i, c, Index: integer;
  p: IntPtr;
  b1, b2: byte;
begin
  if Length(Value) <> 24 then
    raise JSONException.Create(cInvalidObjectId);

  Index := 0;
  p := IntPtr((@Value)^);
  for i := 0 to 11 do begin
    c := Ord(char(p^));
    if (c>= $30) and (c <= $39) {0..9} then
      b1 := byte(c - $30)
    else if (c >= $41) and (c <= $46) {A..F} then
      b1 := byte(c - $37)
    else if (c >= $61) and (c <= $66) {a..f} then
      b1 := byte(c - $57)
    else
      raise JSONException.Create(cInvalidObjectId);
    p := PtrOffset(p, SizeOf(char));

    c := Ord(char(p^));
    if (c>= $30) and (c <= $39) {0..9} then
      b2 := byte(c - $30)
    else if (c >= $41) and (c <= $46) {A..F} then
      b2 := byte(c - $37)
    else if (c >= $61) and (c <= $66) {a..f} then
      b2 := byte(c - $57)
    else
      raise JSONException.Create(cInvalidObjectId);
    p := PtrOffset(p, SizeOf(char));

    Result[Index] := byte(b1 * 16 + b2);
    Inc(Index);
  end;
end;

type
  bson_uint128_t = array[0..3] of Cardinal;
  pbson_uint128_t = ^bson_uint128_t;
  bson_uint128_6464_t = array[0..1] of UInt64;

{$IFDEF VER7}
{$R-}
{$ENDIF}
function Decimal128ToString(const Value: PBinaryDecimal128): string;
const
   BSON_DECIMAL128_INF = 'Infinity';
   BSON_DECIMAL128_NAN = 'NaN';
   COMBINATION_MASK: Cardinal = $1F;
   EXPONENT_MASK: Cardinal = $3FFF;
   COMBINATION_INFINITY: Cardinal = 30;
   COMBINATION_NAN: Cardinal = 31;
   EXPONENT_BIAS: Cardinal = 6176;

var
  significand_str: array[0..34] of byte;
  high: Cardinal;
  midh: Cardinal;
  midl: Cardinal;
  low: Cardinal;
  combination: Cardinal;
  biased_exponent: Cardinal;
  significand_msb: byte;
  exponent: integer;
  significand128: bson_uint128_t;
  is_zero: boolean;
  significand: array[0..35] of Cardinal;
  least_digits: Cardinal;
  significand_digits: Cardinal;
  scientific_exponent: integer;
  significand_read: integer;
  radix_position: integer;
  i, j, k: integer;

  procedure bson_uint128_divide1B(value: bson_uint128_t; out quotient: bson_uint128_t; out rem: Cardinal);
  const
    DIVISOR: Cardinal = 1000 * 1000 * 1000;
  var
    _rem: UInt64;
    i: integer;
  begin
    _rem := 0;

    if (value[0] = 0) and (value[1] = 0) and (value[2] = 0) and (value[3] = 0) then begin
      quotient := value;
      rem := 0;
      Exit;
    end;

    for i := 0 to 3 do begin
      _rem := _rem shl 32;
      _rem := _rem + value[i];
      value[i] := _rem div DIVISOR;
      _rem := _rem mod DIVISOR;
    end;

    quotient := value;
    rem := Cardinal(_rem);
  end;

begin
  Result := '';
  is_zero := False;
  FillChar(significand_str, 35, 0);
  FillChar(significand, 36 * SizeOf(Cardinal), 0);

  if Int64(Value^.high) < 0 then
    Result := '-';

  low := Cardinal((@Value^.Low)^);
  midl := Value^.Low shr 32;
  midh := Cardinal((@Value^.High)^);
  high := Value^.High shr 32;

  combination := (high shr 26) and COMBINATION_MASK;

  if (combination shr 3) = 3 then begin
    if combination = COMBINATION_INFINITY then begin
      Result := BSON_DECIMAL128_INF;
      Exit;
    end
    else if combination = COMBINATION_NAN then begin
      Result := BSON_DECIMAL128_NAN;
      Exit;
    end
    else begin
      biased_exponent := (high shr 15) and EXPONENT_MASK;
      significand_msb := $8 + ((high shr 14) and $1);
    end;
  end
  else begin
    significand_msb := (high shr 14) and $7;
    biased_exponent := (high shr 17) and EXPONENT_MASK;
  end;

  exponent := biased_exponent - EXPONENT_BIAS;

  significand128[0] := (high and EXPONENT_MASK) + ((significand_msb and $F) shl 14);
  significand128[1] := midh;
  significand128[2] := midl;
  significand128[3] := low;

  if ((significand128[0] = 0) and (significand128[1] = 0) and (significand128[2] = 0) and (significand128[3] = 0)) then
    is_zero := true
  else if (significand128[0] >= (1 shl 17)) then
    is_zero := true
  else
  for k := 3 downto 0 do begin
    least_digits := 0;
    bson_uint128_divide1B(significand128, significand128, least_digits);

    if least_digits = 0 then
      Continue;

    for j := 8 downto 0 do begin
      significand[k * 9 + j] := least_digits mod 10;
      least_digits := least_digits div 10;
    end;
  end;

  if is_zero then begin
    significand_digits := 1;
    significand_read := 0;
    significand[0] := 0;
  end
  else begin
    significand_digits := 36;
    significand_read := 0;

    while (significand_read <= 35) and (significand[significand_read] = 0) do begin
      Dec(significand_digits);
      Inc(significand_read);
    end;
  end;

  scientific_exponent := integer(significand_digits) - 1 + exponent;

  if (scientific_exponent < -6) or (exponent > 0) then begin
    Result := Result + Chr(significand[significand_read] + ord('0'));
    Dec(significand_digits);
    Inc(significand_read);

    if significand_digits > 0 then
      Result := Result + '.';

    while significand_read <= 35 do begin
      Result := Result + Chr(significand[significand_read] + ord('0'));
      Inc(significand_read);
    end;

    Result := Result + 'E';
    Result := Result + IntToStr(scientific_exponent);
  end
  else begin
    if exponent >= 0 then begin
      for i := 0 to (35 - significand_digits) do
        Result := Result + Chr(significand[significand_read + i] + ord('0'));
    end
    else begin
      radix_position := integer(significand_digits) + exponent;

      if radix_position > 0 then begin
        for i := 0 to radix_position - 1 do
          Result := Result + Chr(significand[significand_read + i] + ord('0'));
      end
      else
        Result := '0';

      Result := Result + '.';

      while (radix_position < 0) do begin
        Result := Result + '0';
        Inc(radix_position);
      end;

      significand_read := significand_read + radix_position;

      while significand_read <= 35 do begin
        Result := Result + Chr(significand[significand_read] + ord('0'));
        Inc(significand_read);
      end;
    end;
  end;
end;

function StringToDecimal128(const Value: TStringDecimal128; const Decimal: PBinaryDecimal128): boolean;
const
  CDigits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
  BSON_DECIMAL128_NAN: UInt64 = $7C00000000000000;
  BSON_DECIMAL128_INF: UInt64 = $7800000000000000;
{$IFNDEF FPC}
{$IFDEF VER11P}
  BSON_DECIMAL128_INF_NEG: UInt64 = $8000000000000000;
{$ENDIF}
{$ENDIF}
  BSON_DECIMAL128_MAX_DIGITS = 34;
  BSON_DECIMAL128_EXPONENT_MAX = 6111;
  BSON_DECIMAL128_EXPONENT_MIN = -6176;
  BSON_DECIMAL128_EXPONENT_BIAS = 6176;
var
  significand: bson_uint128_6464_t;
  str_max: integer;
  str_read: integer;
  is_negative: boolean;
  saw_radix: boolean;
  includes_sign: boolean;
  found_nonzero: boolean;
  significant_digits: Cardinal;
  ndigits_read: Cardinal;
  ndigits: Cardinal;
  radix_position: Cardinal;
  first_nonzero: Cardinal;
  digits: array[0..BSON_DECIMAL128_MAX_DIGITS - 1] of Word;
  ndigits_stored: Word;
  digits_insert: Word;
  first_digit: Cardinal;
  last_digit: Cardinal;
  exponent: integer;
  significand_high: UInt64;
  significand_low: UInt64;
  biased_exponent: UInt64;  
  s: string;
  d_idx: integer;

  procedure mul_64x64(left, right: UInt64; out product: bson_uint128_6464_t);
  var
    left_high,
    left_low,
    right_high,
    right_low,
    product_high,
    product_mid,
    product_mid2,
    product_low: UInt64;
  begin
    product[0] := 0;
    product[1] := 0;

    if (left = 0) and (right = 0) then
      Exit;

    left_high := left shr 32;
    left_low := Cardinal((@left)^);
    right_high := right shr 32;
    right_low := Cardinal((@right)^);

    product_high := left_high * right_high;
    product_mid := left_high * right_low;
    product_mid2 := left_low * right_high;
    product_low := left_low * right_low;

    product_high := product_high + product_mid shr 32;
    product_mid := Cardinal((@product_mid)^) + product_mid2 + (product_low shr 32);

    product_high := product_high + (product_mid shr 32);
    product_low := (product_mid shl 32) + Cardinal((@product_low)^);

    product[0] := product_high;
    product[1] := product_low;
  end;

begin
  Result := False;

  Decimal^.Low := 0;
  Decimal^.High := 0;

  FillChar(digits, BSON_DECIMAL128_MAX_DIGITS * SizeOf(Word), 0);

  significand[0] := 0;
  significand[1] := 0;
  ndigits_stored := 0;
  ndigits_read := 0;
  first_nonzero := 0;
  digits_insert := 0;
  ndigits := 0;
  radix_position := 0;
  exponent := 0;
  saw_radix := False;
  is_negative := False;
  found_nonzero := False;
  includes_sign := False;
  str_read := 1;
  str_max := Min(cJSONDecimal128Length, Length(Value));

  if (Value[str_read] = '+') or (Value[str_read] = '-') then begin
    is_negative := Value[str_read] = '-';
    includes_sign := True;
    Inc(str_read);
  end;

  if not (CharInSet(Value[str_read], CDigits) or (Value[str_read] = '.')) then begin
    if SameText(Copy(string(Value), str_read, 3), 'inf') or (SameText(Copy(string(Value), str_read, 8), 'infinity')) then begin
      Decimal^.Low := 0;
      Decimal^.High := BSON_DECIMAL128_INF;

    {$IFNDEF FPC}
    {$IFDEF VER11P}
      if is_negative then
        Decimal^.High := Decimal^.High or BSON_DECIMAL128_INF_NEG;
    {$ENDIF}
    {$ENDIF}
      Result := True;
    end
    else if SameText(Copy(string(Value), str_read, 3), 'nan') then begin
      Decimal^.Low := 0;
      Decimal^.High := BSON_DECIMAL128_NAN;
      Result := True;
    end
    else begin
      Decimal^.Low := 0;
      Decimal^.High := BSON_DECIMAL128_NAN;
    end;

    Exit;
  end;

  while (str_read <= str_max) and (CharInSet(Value[str_read], CDigits) or (Value[str_read] = '.')) do begin
    if Value[str_read] = '.' then begin
      if saw_radix then begin
        Decimal^.Low := 0;
        Decimal^.High := BSON_DECIMAL128_NAN;
        Exit;
      end;

      saw_radix := True;
      Inc(str_read);

      Continue;
    end;

    if ndigits_stored < 34 then
      if (Value[str_read] <> '0') or found_nonzero then begin
        if not found_nonzero then
          first_nonzero := ndigits_read;

        found_nonzero := True;
        digits[digits_insert] := Ord(Value[str_read]) - Ord('0');
        Inc(digits_insert);
        Inc(ndigits_stored);
      end;

    if found_nonzero then
      Inc(ndigits);

    if saw_radix then
      Inc(radix_position);

    Inc(ndigits_read);
    Inc(str_read);
  end;

  if saw_radix and (ndigits_read = 0) then begin
    Decimal^.Low := 0;
    Decimal^.High := BSON_DECIMAL128_NAN;
    Exit;
  end;

  if (str_read <= str_max) and ((Value[str_read] = 'e') or (Value[str_read] = 'E')) then begin
    Inc(str_read);
    s := Copy(string(Value), str_read, str_max);

    if not TryStrToInt(s, exponent) then begin
      Decimal^.Low := 0;
      Decimal^.High := BSON_DECIMAL128_NAN;
      Exit;
    end;
  end;

  first_digit := 0;

  if ndigits_stored = 0 then begin
    first_digit := 0;
    last_digit := 0;
    digits[0] := 0;
    ndigits := 1;
    ndigits_stored := 1;
    significant_digits := 0;
  end
  else begin
    last_digit := ndigits_stored - 1;
    significant_digits := ndigits;
    while (Value[first_nonzero + significant_digits + Cardinal(includes_sign) + Cardinal(saw_radix)] = '0') do
      Dec(significant_digits);
  end;

  if (exponent <= integer(radix_position)) and ((integer(radix_position) - exponent) > (1 shl 14)) then
    exponent := BSON_DECIMAL128_EXPONENT_MIN
  else
    exponent := exponent - integer(radix_position);

  while exponent > BSON_DECIMAL128_EXPONENT_MAX do begin
    Inc(last_digit);

    if (last_digit - first_digit) > BSON_DECIMAL128_MAX_DIGITS then begin
      if significant_digits = 0 then begin
        exponent := BSON_DECIMAL128_EXPONENT_MAX;
        Break;
      end;

      Decimal^.Low := 0;
      Decimal^.High := BSON_DECIMAL128_NAN;
      Exit;
    end;

    Dec(exponent);
  end;

  while (exponent < BSON_DECIMAL128_EXPONENT_MIN) or (ndigits_stored < ndigits) do begin
    if last_digit = 0 then begin
      if significant_digits = 0 then begin
        exponent := BSON_DECIMAL128_EXPONENT_MIN;
        Break;
      end;

      Decimal^.Low := 0;
      Decimal^.High := BSON_DECIMAL128_NAN;
      Exit;
    end;

    if ndigits_stored < ndigits then begin
      if (Ord(Value[ndigits + Cardinal(includes_sign) + Cardinal(saw_radix)]) - Ord('0') <> 0) and (significant_digits <> 0) then begin
        Decimal^.Low := 0;
        Decimal^.High := BSON_DECIMAL128_NAN;
        Exit;
      end;

      Dec(ndigits);
    end
    else begin
      if digits[last_digit] <> 0 then begin
        Decimal^.Low := 0;
        Decimal^.High := BSON_DECIMAL128_NAN;
        Exit;
      end;


      Dec(last_digit);
    end;

    if exponent < BSON_DECIMAL128_EXPONENT_MAX then
      Inc(exponent)
    else begin
      Decimal^.Low := 0;
      Decimal^.High := BSON_DECIMAL128_NAN;
      Exit;
    end;
  end;

  if (last_digit - first_digit + 1) < significant_digits then begin
//      uint8_t round_digit;
//
//      /* There are non-zero digits after last_digit that need rounding. */
//      /* We round to nearest, ties to even */
//      round_digit =
//         string[first_nonzero + last_digit + includes_sign + saw_radix + 1] -
//         '0';
//
//      if (round_digit != 0) {
//         /* Inexact (non-zero) rounding is not allowed */
//         BSON_DECIMAL128_SET_NAN (*dec);
//         return false;
//      }
  end;

  significand_high := 0;

  if significant_digits = 0 then begin
    significand_high := 0;
    significand_low := 0;
  end
  else if (last_digit - first_digit) < 17 then begin
    d_idx := first_digit;
    significand_low := digits[d_idx];
    Inc(d_idx);

    while d_idx <= integer(last_digit) do begin
      significand_low := significand_low * 10;
      significand_low := significand_low + digits[d_idx];
      significand_high := 0;
      Inc(d_idx);
    end;
  end
  else begin
    d_idx := first_digit;
    significand_high := digits[d_idx];
    Inc(d_idx);

    while d_idx <= (integer(last_digit) - 17) do begin
      significand_high := significand_high * 10;
      significand_high := significand_high + digits[d_idx];
      Inc(d_idx);
    end;

    significand_low := digits[d_idx];
    Inc(d_idx);

    while d_idx <= integer(last_digit) do begin
      significand_low := significand_low * 10;
      significand_low := significand_low + digits[d_idx];
      Inc(d_idx);
    end;
  end;

  mul_64x64 (significand_high, 100000000000000000, significand);
  significand[1] := significand[1] + significand_low;

  if (significand[1] < significand_low) then
    significand[0] := significand[0] + 1;

  biased_exponent := exponent + BSON_DECIMAL128_EXPONENT_BIAS;

  if ((significand[0] shr 49) and 1) > 0 then begin
    Decimal^.High := Decimal^.High or (UInt64($3) shl 61);
    Decimal^.High := Decimal^.High or ((biased_exponent and UInt64($3FFF)) shl 47);
    Decimal^.High := Decimal^.High or (significand[0] and UInt64($7FFFFFFFFFFF));
  end
  else begin
    Decimal^.High := Decimal^.High or ((biased_exponent and UInt64($3FFF)) shl 49);
    Decimal^.High := Decimal^.High or (significand[0] and UInt64($1FFFFFFFFFFFF));
  end;

  Decimal^.Low := significand[1];

  if is_negative then
    Decimal^.High := Decimal^.High or {$IFNDEF FPC}UInt64{$ENDIF}($8000000000000000);

  Result := True;
end;

{ TJSONValue }

constructor TJSONValue.Create(const Tag: TJSONTag; const Parent: TJSONValue);
begin
  inherited Create;

  FTag := Tag;
  FParent := Parent;
  FActualName := '';
  FSize := 0;
  if Parent = nil then
    FData := nil
  else
    FData := Parent.FData;
end;

constructor TJSONValue.Create(const Parent: TJSONValue);
begin
  Create(jtNone, Parent);
end;

procedure TJSONValue.Copy(const Source: TJSONValue);
begin
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

function TJSONValue.IsEqual(const AValue: TJSONValue): boolean;
begin
  Result := Tag = AValue.Tag;
end;

procedure TJSONValue.SetSize(const AValue: integer);
begin
  FSize := AValue;
end;

procedure TJSONValue.SetActualName(const Value: string);
begin
  FActualName := Value;
end;

function TJSONValue.GetAsString: string;
var
  Buf: TBytes;
begin
  if (FStream <> nil) and (FEndPos > FStartPos) then begin
    SetLength(Buf, FEndPos - FStartPos);
    FStream.Position := FStartPos;
    FStream.Read(Buf[0], Length(Buf));
    Result := Encoding.UTF8.GetString(Buf);
  end
  else
    Result := '';
end;

function TJSONValue.GetValuePtr: IntPtr;
begin
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
  raise Exception.Create('Cannot access ValuePtr');
end;

{ TJSONNull }

constructor TJSONNull.Create(const Parent: TJSONValue);
begin
  inherited Create(jtNull, Parent);
end;

function TJSONNull.GetAsString: string;
begin
  Result := cNull;
end;

function TJSONNull.GetValuePtr: IntPtr;
begin
  Result := nil;
end;

function TJSONNull.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONNull.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

{ TJSONString }

constructor TJSONString.Create(const Parent: TJSONValue);
begin
  inherited Create(jtString, Parent);

  SetValue('');
end;

function TJSONString.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  if FStringType = jsAnsiString then
    Result := TJSONAnsiString.Create(AParent)
  else
    Result := TJSONWideString.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONString.Copy(const Source: TJSONValue);
begin
  inherited;

  Value := (Source as TJSONString).Value;
end;

function TJSONString.GetValue: string;
begin
  if FStringType = jsAnsiString then
    Result := string(AsAnsiString)
  else
    Result := string(AsWideString);
end;

procedure TJSONString.SetValue(const AValue: string);
begin
  if FStringType = jsAnsiString then
    AsAnsiString := AnsiString(AValue)
  else
    AsWideString := WideString(AValue);
end;

function TJSONString.GetAsAnsiString: AnsiString;
begin
  Result := '';
  Assert(False);
end;

procedure TJSONString.SetAsAnsiString(const AValue: AnsiString);
begin
  Assert(False);
end;

function TJSONString.GetAsWideString: WideString;
begin
  Result := '';
  Assert(False);
end;

procedure TJSONString.SetAsWideString(const AValue: WideString);
begin
  Assert(False);
end;

function TJSONString.GetAsString: string;
begin
  Result := GetValue;
end;

procedure TJSONString.InitValue(const ASize: integer);
begin
  FSize := ASize;
end;

{ TJSONAnsiString }

constructor TJSONAnsiString.Create(const Parent: TJSONValue; const AValue: string);
begin
  inherited Create(Parent);

  FStringType := jsAnsiString;

  SetValue(AValue);
end;

function TJSONAnsiString.GetAsAnsiString: AnsiString;
begin
  Result := FValue;
end;

procedure TJSONAnsiString.SetAsAnsiString(const AValue: AnsiString);
begin
  FValue := AValue;
  FSize := Length(FValue) + 1;
end;

function TJSONAnsiString.GetAsWideString: WideString;
begin
  Result := WideString(FValue);
end;

procedure TJSONAnsiString.SetAsWideString(const AValue: WideString);
begin
  FValue := AnsiString(AValue);
  FSize := Length(FValue) + 1;
end;

function TJSONAnsiString.GetValuePtr: IntPtr;
begin
  if Length(FValue) > 0 then
    Result := {$IFNDEF NEXTGEN}@FValue[1]{$ELSE}FValue.Ptr{$ENDIF}
  else
    Result := nil;
end;

procedure TJSONAnsiString.InitValue(const ASize: integer);
begin
  inherited;

  SetLengthA(FValue, FSize);
end;

{ TJSONWideString }

constructor TJSONWideString.Create(const Parent: TJSONValue; const AValue: string);
begin
  inherited Create(Parent);

  FStringType := jsWideString;

  SetValue(AValue);
end;

function TJSONWideString.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(FValue);
end;

procedure TJSONWideString.SetAsAnsiString(const AValue: AnsiString);
begin
  FValue := WideString(AValue);
  FSize := Length(FValue) * 2 + 2;
end;

function TJSONWideString.GetAsWideString: WideString;
begin
  Result := FValue;
end;

procedure TJSONWideString.SetAsWideString(const AValue: WideString);
begin
  FValue := AValue;
  FSize := Length(FValue) * 2 + 2;
end;

function TJSONWideString.GetValuePtr: IntPtr;
begin
  if Length(FValue) > 0 then
    Result := @FValue[1]
  else
    Result := nil;
end;

procedure TJSONWideString.InitValue(const ASize: integer);
begin
  inherited;

  SetLength(FValue, FSize);
end;

{ TJSONNumber }

constructor TJSONNumber.Create(const Parent: TJSONValue);
begin
  inherited Create(jtNumber, Parent);

  SetValue('0');
  FSubtype := jtInt32;
end;

constructor TJSONNumber.Create(const Parent: TJSONValue; const AValue: string; const ASubtype: TJSONTag);
begin
  Create(Parent);

  SetValue(AValue);
  FSubtype := ASubtype;
end;

function TJSONNumber.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONNumber.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONNumber.Copy(const Source: TJSONValue);
begin
  inherited;

  FValue := (Source as TJSONNumber).FValue;
  FSubtype := (Source as TJSONNumber).FSubtype;
end;

{ TJSONDecimal128 }

constructor TJSONDecimal128.Create(const Parent: TJSONValue);
begin
  Create(jtDecimal128, Parent);

  SetValue('0');
end;

function TJSONDecimal128.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONDecimal128.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONDecimal128.Copy(const Source: TJSONValue);
begin
  inherited;

  FValue := (Source as TJSONDecimal128).FValue;
end;

{ TJSONBoolean }

constructor TJSONBoolean.Create(const Parent: TJSONValue);
begin
  inherited Create(jtBoolean, Parent);

  FValue := False;
  FSize := 1;
end;

constructor TJSONBoolean.Create(const Parent: TJSONValue; const AValue: boolean);
begin
  Create(Parent);

  FValue := AValue;
end;

function TJSONBoolean.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONBoolean.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONBoolean.Copy(const Source: TJSONValue);
begin
  inherited;

  FValue := (Source as TJSONBoolean).FValue;
end;

function TJSONBoolean.GetAsString: string;
begin
  Result := AnsiLowerCase(BoolToStr(FValue, True));
end;

function TJSONBoolean.GetValuePtr: IntPtr;
begin
  Result := @FValue;
end;

{ TJSONPair }

constructor TJSONPair.Create(const Parent: TJSONValue);
begin
  inherited Create(jtPair, Parent);

  FName := nil;
  FValue := nil;
end;

constructor TJSONPair.Create(const Parent: TJSONValue; const AName: string; const Unicode: boolean);
begin
  Create(Parent);

  if Unicode then
    FName := TJSONWideString.Create(Self, AName)
  else
    FName := TJSONAnsiString.Create(Self, AName);
end;

destructor TJSONPair.Destroy;
begin
  FName.Free;
  FValue.Free;

  inherited;
end;

procedure TJSONPair.SetName(const AValue: TJSONString);
begin
  FName := AValue;

  if FName <> nil then
    FName.FParent := Self;
end;

procedure TJSONPair.SetValue(const AValue: TJSONValue);
begin
  FValue := AValue;

  if FValue <> nil then
    FValue.FParent := Self;
end;

function TJSONPair.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONPair.Create(AParent);
  TJSONPair(Result).Name := FName.Clone(Result, AData) as TJSONString;
  TJSONPair(Result).Value := FValue.Clone(Result, AData);
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONPair.Copy(const Source: TJSONValue);
begin
  inherited;

  FName.Copy((Source as TJSONPair).FName);
  FValue.Copy((Source as TJSONPair).FValue);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

function TJSONPair.IsEqual(const AValue: TJSONValue): boolean;
begin
  Result := inherited IsEqual(AValue);

  if Result then begin
    if FName.FStringType = jsWideString then
      Result := FName.AsWideString = TJSONPair(AValue).Name.AsWideString
    else
      Result := FName.AsAnsiString = TJSONPair(AValue).Name.AsAnsiString;
  end;
  if Result then
    Result := FValue.IsEqual(TJSONPair(AValue).Value);
end;

{ TJSONPairs }

function TJSONPairs.GetItem(Index: Integer): TJSONPair;
begin
{$IFNDEF NEXTGEN}
  Result := TJSONPair(Get(Index));
{$ELSE}
  Result := TJSONPair(inherited Items[Index]);
{$ENDIF}
end;

procedure TJSONPairs.PutItem(Index: Integer; const Value: TJSONPair);
begin
{$IFNDEF NEXTGEN}
  Put(Index, Pointer(Value));
{$ELSE}
  inherited Items[Index] := Value;
{$ENDIF}
end;

{$IFNDEF NEXTGEN}
procedure TJSONPairs.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TJSONValue(Items[i]).Free;

  inherited;
end;
{$ENDIF}

{ TJSONObject }

constructor TJSONObject.Create(const Parent: TJSONValue);
begin
  inherited Create(jtObject, Parent);

  FPairs := TJSONPairs.Create;
end;

destructor TJSONObject.Destroy;
begin
  FPairs.Clear;
  FPairs.Free;

  inherited;
end;

function TJSONObject.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
var
  i: integer;
begin
  Result := TJSONObject.Create(AParent);
  for i := 0 to FPairs.Count - 1 do
    TJSONObject(Result).AddPair(FPairs[i].Clone(Result, AData) as TJSONPair);
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONObject.Copy(const Source: TJSONValue);
var
  i: integer;
  SrcPair,
  DestPair: TJSONPair;
begin
  inherited;

  for i := 0 to FPairs.Count - 1 do begin
    DestPair := FPairs[i];
    SrcPair := (Source as TJSONObject).GetPair(DestPair.Name.AsString);
    if SrcPair <> nil then
      DestPair.Copy(SrcPair);
  end;
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

function TJSONObject.IsEqual(const AValue: TJSONValue): boolean;
var
  i: integer;
begin
  Result := inherited IsEqual(AValue);

  if Result then
    Result := FPairs.Count = TJSONObject(AValue).Pairs.Count;
  if Result then
    for i := 0 to FPairs.Count - 1 do
      if not FPairs[i].IsEqual(TJSONObject(AValue).Pairs[i]) then begin
        Result := False;
        Break;
      end;
end;

function TJSONObject.GetValue(const Name: string): TJSONValue;

  function InternalGetValue(const Value: TJSONValue; const Name: string): TJSONValue;
  var
    i: integer;
  begin
    Result := nil;

    if Value = nil then
      Exit
    else
    case Value.FTag of
      jtObject: begin
        for i := 0 to TJSONObject(Value).Pairs.Count - 1 do begin
          Result := InternalGetValue(TJSONObject(Value).Pairs[i], Name);
          if Result <> nil then
            Break;
        end;
      end;
      jtArray: begin
        for i := 0 to TJSONArray(Value).Elements.Count - 1 do begin
          if TJSONArray(Value).Elements[i] = nil then
            Continue;

          if SameText(TJSONValue(TJSONArray(Value).Elements[i]).ActualName, Name) then
            Result := TJSONValue(TJSONArray(Value).Elements[i])
          else
            Result := InternalGetValue(TJSONArray(Value).Elements[i], Name);
          if Result <> nil then
            Break;
        end;
      end;
      jtPair:
        if SameText(TJSONPair(Value).Value.ActualName, Name) or SameText(TJSONPair(Value).Name.AsString, Name) then
          Result := TJSONPair(Value).Value
        else
          Result := InternalGetValue(TJSONPair(Value).Value, Name);
      jtRegex:
        if SameText(TJSONRegex(Value).Pattern.ActualName, Name) then
          Result := TJSONRegex(Value).Pattern
        else if SameText(TJSONRegex(Value).Options.ActualName, Name) then
          Result := TJSONRegex(Value).Options;
      jtJavaCode:
        if SameText(TJSONJavaCode(Value).Code.ActualName, Name) then
          Result := TJSONJavaCode(Value).Code;
      jtJavaScopeCode:
        if SameText(TJSONJavaScopeCode(Value).Code.ActualName, Name) then
          Result := TJSONJavaScopeCode(Value).Code
        else if SameText(TJSONJavaScopeCode(Value).Scope.ActualName, Name) then
          Result := TJSONJavaScopeCode(Value).Scope
        else
          Result := InternalGetValue(TJSONJavaScopeCode(Value).Scope, Name);
      jtBinary:
        if SameText(TJSONBinary(Value).Binary.ActualName, Name) then
          Result := TJSONBinary(Value).Binary
        else if SameText(TJSONBinary(Value).Subtype.ActualName, Name) then
          Result := TJSONBinary(Value).Subtype;
      jtDBPointer:
        if SameText(TJSONDBPointer(Value).Name.ActualName, Name) then
          Result := TJSONDBPointer(Value).Name
        else if SameText(TJSONDBPointer(Value).FValue.ActualName, Name) then
          Result := TJSONDBPointer(Value).FValue;
      jtTimestamp:
        if SameText(TJSONTimestamp(Value).Timestamp.ActualName, Name) then
          Result := TJSONTimestamp(Value).Timestamp
        else if SameText(TJSONTimestamp(Value).Increment.ActualName, Name) then
          Result := TJSONTimestamp(Value).Increment;
    end;
  end;

begin
  Result := InternalGetValue(Self, Name);
end;

procedure TJSONObject.SetActualName(const Value: string);
var
  i: Integer;
begin
  if FActualName <> Value then begin
    inherited;

    for i := 0 to FPairs.Count - 1 do
      if FActualName <> '' then
        FPairs[i].Value.ActualName := FActualName + '.' + FPairs[i].Name.Value
      else
        FPairs[i].Value.ActualName := FPairs[i].Name.Value;
  end;
end;

procedure TJSONObject.AddPair(const Pair: TJSONPair);
begin
  Pair.FParent := Self;

  if FActualName <> '' then
    Pair.Value.ActualName := FActualName + '.' + Pair.Name.Value
  else
    Pair.Value.ActualName := Pair.Name.Value;

  FPairs.Add(Pair);
end;

function TJSONObject.GetPair(const Name: string): TJSONPair;
var
  i: integer;
  TmpPair: TJSONPair;
begin
  Result := nil;

  for i := 0 to FPairs.Count - 1 do begin
    TmpPair := FPairs[i];
    if SameText(TmpPair.Name.AsString, Name) then begin
      Result := TmpPair;
      Break;
    end;
  end;
end;

procedure TJSONObject.DeletePair(Pair: TJSONPair);
var
  i: integer;
begin
  i := FPairs.IndexOf(Pair);

  if i >= 0 then begin
    FPairs.Delete(i);
    Pair.Free;
  end;
end;

{ TJSONElements }

function TJSONElements.GetItem(Index: Integer): TJSONValue;
begin
{$IFNDEF NEXTGEN}
  Result := TJSONValue(Get(Index));
{$ELSE}
  Result := TJSONValue(inherited Items[Index]);
{$ENDIF}
end;

procedure TJSONElements.PutItem(Index: Integer; const Value: TJSONValue);
begin
{$IFNDEF NEXTGEN}
  Put(Index, Pointer(Value));
{$ELSE}
  inherited Items[Index] := Value;
{$ENDIF}
end;

{$IFNDEF NEXTGEN}
procedure TJSONElements.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TJSONValue(Items[i]).Free;

  inherited;
end;
{$ENDIF}

{ TJSONArray }

constructor TJSONArray.Create(const Parent: TJSONValue);
begin
  inherited Create(jtArray, Parent);

  FElements := TJSONElements.Create;
end;

destructor TJSONArray.Destroy;
begin
  FElements.Clear;
  FElements.Free;

  inherited;
end;

procedure TJSONArray.SetActualName(const Value: string);
var
  i: integer;
begin
  if FActualName <> Value then begin
    inherited;

    for i := 0 to FElements.Count - 1 do
      if FActualName <> '' then
        FElements[i].ActualName := FActualName + '.' + IntToStr(i)
      else
        FElements[i].ActualName := IntToStr(i);
  end;
end;

function TJSONArray.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
var
  i: integer;
begin
  Result := TJSONArray.Create(AParent);
  for i := 0 to FElements.Count - 1 do
    TJSONArray(Result).AddElement(FElements[i].Clone(Result, AData));
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONArray.Copy(const Source: TJSONValue);
var
  i: integer;
begin
  inherited;

  for i := 0 to FElements.Count - 1 do
    if i < (Source as TJSONArray).FElements.Count then
      FElements[i].Copy((Source as TJSONArray).FElements[i]);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

function TJSONArray.IsEqual(const AValue: TJSONValue): boolean;
var
  i: integer;
begin
  Result := inherited IsEqual(AValue);

  if Result then
    Result := FElements.Count = TJSONArray(AValue).Elements.Count;
  if Result then
    for i := 0 to FElements.Count - 1 do
      if not TJSONValue(FElements[i]).IsEqual(TJSONArray(AValue).Elements[i]) then begin
        Result := False;
        Break;
      end;
end;

function TJSONArray.AddElement(const Element: TJSONValue): integer;
var
  i: integer;
begin
  if Element = nil then
    raise JSONException.Create(cInvalidValue);

  i := FElements.Count;
  if FActualName <> '' then
    Element.ActualName := FActualName + '.' + IntToStr(i)
  else
    Element.ActualName := IntToStr(i);

  Result := FElements.Add(Element);
end;

procedure TJSONArray.DeleteElement(Element: TJSONValue);
var
  i, n: integer;
begin
  n := FElements.IndexOf(Element);

  if n >= 0 then begin
    FElements.Delete(n);
    Element.Free;

    for i := n to FElements.Count - 1 do
      if FActualName = '' then
        FElements[i].ActualName := IntToStr(i)
      else
        FElements[i].ActualName := FActualName + '.' + IntToStr(i);
  end;
end;

{ TBSONObjectId }

constructor TJSONObjectId.Create(const Parent: TJSONValue);
begin
  Create(jtObjectId, Parent);

  FillChar(FValue, cJSONOidSize, 0);
end;

{$IFNDEF VER12P}
function TJSONObjectId.GetValue: TJSONOid;
begin
  Result := FValue;
end;
{$ENDIF}

procedure TJSONObjectId.SetValue(const AValue: TJSONOid);
begin
  FValue := AValue;
  FSize := cJSONOidSize;
end;

function TJSONObjectId.GetAsString: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to cJSONOidSize - 1 do
    Result := Result + IntToHex(FValue[i], 2);
end;

function TJSONObjectId.GetValuePtr: IntPtr;
begin
  Result := @FValue[0];
end;

function TJSONObjectId.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONObjectId.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONObjectId.Copy(const Source: TJSONValue);
begin
  inherited;

  FValue := (Source as TJSONObjectId).FValue;
end;

{ TBSONInt32 }

constructor TJSONInt32.Create(const Parent: TJSONValue);
begin
  Create(jtInt32, Parent);

  FValue := 0;
  FSize := 4;
end;

function TJSONInt32.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TJSONInt32.GetValuePtr: IntPtr;
begin
  Result := @FValue;
end;

function TJSONInt32.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONInt32.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONInt32.Copy(const Source: TJSONValue);
begin
  inherited;

  FValue := (Source as TJSONInt32).FValue;
end;

{ TBSONInt64 }

constructor TJSONInt64.Create(const Parent: TJSONValue);
begin
  Create(jtInt64, Parent);

  FValue := 0;
  FSize := 8;
end;

function TJSONInt64.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

function TJSONInt64.GetValuePtr: IntPtr;
begin
  Result := @FValue;
end;

function TJSONInt64.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONInt64.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONInt64.Copy(const Source: TJSONValue);
begin
  inherited;

  FValue := (Source as TJSONInt64).FValue;
end;

{ TBSONDateTime }

constructor TJSONDateTime.Create(const Parent: TJSONValue);
begin
  Create(jtDateTime, Parent);

  FValue := 0;
  FSize := 8;
end;

function TJSONDateTime.GetAsString: string;
begin
  Result := FormatDateTime('YYYY-MM-DD hh:nn:ss.zzz', FValue);
end;

function TJSONDateTime.GetValuePtr: IntPtr;
begin
  Result := @FValue;
end;

function TJSONDateTime.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONDateTime.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONDateTime.Copy(const Source: TJSONValue);
begin
  inherited;

  FValue := (Source as TJSONDateTime).FValue;
end;

{ TJSONJavaCode }

constructor TJSONJavaCode.Create(const Parent: TJSONValue);
begin
  inherited Create(jtJavaCode, Parent);

  FCode := nil;
end;

destructor TJSONJavaCode.Destroy;
begin
  FCode.Free;

  inherited;
end;

procedure TJSONJavaCode.SetCode(const AValue: TJSONString);
begin
  FCode := AValue;

  if FCode <> nil then
    FCode.FParent := Self;
end;

procedure TJSONJavaCode.SetActualName(const Value: string);
begin
  inherited;

  if FActualName <> '' then
    FCode.ActualName := FActualName + '.' + cAttrCode
  else
    FCode.ActualName := cAttrCode;
end;

function TJSONJavaCode.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONJavaCode.Create(AParent);
  TJSONJavaCode(Result).Code := FCode.Clone(Result, AData) as TJSONString;
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONJavaCode.Copy(const Source: TJSONValue);
begin
  inherited;

  FCode.Copy((Source as TJSONJavaCode).FCode);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

{ TJSONJavaScopeCode }

constructor TJSONJavaScopeCode.Create(const Parent: TJSONValue);
begin
  inherited Create(jtJavaScopeCode, Parent);

  FCode := nil;
  FScope := nil;
end;

destructor TJSONJavaScopeCode.Destroy;
begin
  FScope.Free;

  inherited;
end;

procedure TJSONJavaScopeCode.SetActualName(const Value: string);
begin
  inherited;

  if FActualName <> '' then
    FScope.ActualName := FActualName + '.' + cAttrScope
  else
    FScope.ActualName := cAttrScope;
end;

function TJSONJavaScopeCode.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONJavaScopeCode.Create(AParent);
  TJSONJavaScopeCode(Result).Code := FCode.Clone(Result, AData) as TJSONString;
  TJSONJavaScopeCode(Result).Scope := FScope.Clone(Result, AData) as TJSONObject;
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONJavaScopeCode.Copy(const Source: TJSONValue);
begin
  inherited;

  FCode.Copy((Source as TJSONJavaScopeCode).FCode);
  FScope.Copy((Source as TJSONJavaScopeCode).FScope);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

{ TJSONUndefined }

constructor TJSONUndefined.Create(const Parent: TJSONValue);
begin
  inherited Create(jtUndefined, Parent);

  FSize := 4;
end;

function TJSONUndefined.GetAsString: string;
begin
  Result := cTrue;
end;

function TJSONUndefined.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONUndefined.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

{ TJSONMinKey }

constructor TJSONMinKey.Create(const Parent: TJSONValue);
begin
  inherited Create(jtMinKey, Parent);

  FSize := 1;
end;

function TJSONMinKey.GetAsString: string;
begin
  Result := '1';
end;

function TJSONMinKey.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONMinKey.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

{ TJSONMaxKey }

constructor TJSONMaxKey.Create(const Parent: TJSONValue);
begin
  inherited Create(jtMaxKey, Parent);

  FSize := 1;
end;

function TJSONMaxKey.GetAsString: string;
begin
  Result := '1';
end;

function TJSONMaxKey.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONMaxKey.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

{ TBSONRegex }

constructor TJSONRegex.Create(const Parent: TJSONValue);
begin
  inherited Create(jtRegex, Parent);

  FPattern := nil;
  FOptions := nil;
end;

destructor TJSONRegex.Destroy;
begin
  FPattern.Free;
  FOptions.Free;

  inherited;
end;

procedure TJSONRegex.SetPattern(const AValue: TJSONString);
begin
  FPattern := AValue;

  if FPattern <> nil then
    FPattern.FParent := Self;
end;

procedure TJSONRegex.SetOptions(const AValue: TJSONString);
begin
  FOptions := AValue;

  if FOptions <> nil then
    FOptions.FParent := Self;
end;

procedure TJSONRegex.SetActualName(const Value: string);
begin
  inherited;

  if FActualName <> '' then begin
    FPattern.ActualName := FActualName + '.' + cAttrPattern;
    FOptions.ActualName := FActualName + '.' + cAttrOptions;
  end
  else begin
    FPattern.ActualName := cAttrPattern;
    FOptions.ActualName := cAttrOptions;
  end;
end;

function TJSONRegex.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONRegex.Create(AParent);
  TJSONRegex(Result).Pattern := FPattern.Clone(Result, AData) as TJSONString;
  TJSONRegex(Result).Options := FOptions.Clone(Result, AData) as TJSONString;
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONRegex.Copy(const Source: TJSONValue);
begin
  inherited;

  FPattern.Copy((Source as TJSONRegex).FPattern);
  FOptions.Copy((Source as TJSONRegex).FOptions);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

{ TJSONTimeStamp }

constructor TJSONTimestamp.Create(const Parent: TJSONValue);
begin
  inherited Create(jtTimestamp, Parent);

  FTimestamp := nil;
  FIncrement := nil;
end;

destructor TJSONTimestamp.Destroy;
begin
  FTimestamp.Free;
  FIncrement.Free;

  inherited;
end;

procedure TJSONTimestamp.SetTimestamp(const AValue: TJSONInt32);
begin
  FTimestamp := AValue;

  if FTimestamp <> nil then
    FTimestamp.FParent := Self;
end;

procedure TJSONTimestamp.SetIncrement(const AValue: TJSONInt32);
begin
  FIncrement := AValue;

  if FIncrement <> nil then
    FIncrement.FParent := Self;
end;

procedure TJSONTimestamp.SetActualName(const Value: string);
begin
  inherited;

  if FActualName <> '' then begin
    FTimestamp.ActualName := FActualName + '.' + cAttrTime;
    FIncrement.ActualName := FActualName + '.' + cAttrIncrement;
  end
  else begin
    FTimestamp.ActualName := cAttrTime;
    FIncrement.ActualName := cAttrIncrement;
  end;
end;

function TJSONTimestamp.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONTimestamp.Create(AParent);
  TJSONTimestamp(Result).Timestamp := FTimestamp.Clone(Result, AData) as TJSONInt32;
  TJSONTimestamp(Result).Increment := FIncrement.Clone(Result, AData) as TJSONInt32;
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONTimestamp.Copy(const Source: TJSONValue);
begin
  inherited;

  FTimestamp.Copy((Source as TJSONTimestamp).FTimestamp);
  FIncrement.Copy((Source as TJSONTimestamp).FIncrement);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

{ TJSONBytes }

constructor TJSONBytes.Create(const Parent: TJSONValue);
begin
  inherited Create(jtBytes, Parent);
end;

procedure TJSONBytes.SetSize(const AValue: integer);
begin
  inherited;

  SetLength(FValue, AValue);
end;

procedure TJSONBytes.SetValue(const AValue: TBytes);
begin
  FValue := AValue;
  FSize := Length(AValue);
end;

function TJSONBytes.GetAsString: string;
begin
  Result := Encoding.ASCII.GetString(TBase64.Encode(FValue));
end;

function TJSONBytes.GetValuePtr: IntPtr;
begin
  if FSize > 0 then
    Result := @FValue[0]
  else
    Result := nil;
end;

function TJSONBytes.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONBytes.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONBytes.Copy(const Source: TJSONValue);
begin
  inherited;

  SetValue((Source as TJSONBytes).FValue);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

{$IFNDEF VER12P}
function TJSONBytes.GetValue: TBytes;
begin
  Result := FValue;
end;
{$ENDIF}

{ TBSONBinary }

constructor TJSONBinary.Create(const Parent: TJSONValue);
begin
  inherited Create(jtBinary, Parent);

  FBinary := nil;
  FSubtype := nil;
end;

destructor TJSONBinary.Destroy;
begin
  FBinary.Free;
  FSubtype.Free;

  inherited;
end;

procedure TJSONBinary.SetSubtype(const AValue: TJSONInt32);
begin
  FSubtype := AValue;

  if FSubtype <> nil then
    FSubtype.FParent := Self;
end;

procedure TJSONBinary.SetValue(const AValue: TJSONBytes);
begin
  FBinary := AValue;

  if FBinary <> nil then
    FBinary.FParent := Self;
end;

procedure TJSONBinary.SetActualName(const Value: string);
begin
  inherited;

  if FActualName <> '' then begin
    FBinary.ActualName := FActualName + '.' + cAttrBinary;
    FSubtype.ActualName := FActualName + '.' + cAttrSubtype;
  end
  else begin
    FBinary.ActualName := cAttrBinary;
    FSubtype.ActualName := cAttrSubtype;
  end;
end;

function TJSONBinary.GetAsString: string;
begin
  Result := FBinary.AsString;
end;

function TJSONBinary.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONBinary.Create(AParent);
  TJSONBinary(Result).Subtype := FSubtype.Clone(Result, AData) as TJSONInt32;
  TJSONBinary(Result).Binary := FBinary.Clone(Result, AData) as TJSONBytes;
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONBinary.Copy(const Source: TJSONValue);
begin
  inherited;

  FSubtype.Copy((Source as TJSONBinary).FSubtype);
  FBinary.Copy((Source as TJSONBinary).FBinary);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

{ TBSONDouble }

constructor TJSONDouble.Create(const Parent: TJSONValue);
begin
  Create(jtDouble, Parent);

  FValue := 0;
end;

function TJSONDouble.GetAsString: string;
begin
  Result := StringReplace(FloatToStr(FValue), ',', '.', [rfReplaceAll]);
end;

function TJSONDouble.GetValuePtr: IntPtr;
begin
  Result := @FValue;
end;

function TJSONDouble.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONDouble.Create(AParent);
  Result.Copy(Self);
  Result.FData := AData;
end;

procedure TJSONDouble.Copy(const Source: TJSONValue);
begin
  inherited;

  FValue := (Source as TJSONDouble).FValue;
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

{ TJSONDBPointer }

constructor TJSONDBPointer.Create(const Parent: TJSONValue);
begin
  inherited Create(jtDBPointer, Parent);

  FName := nil;
  FValue := nil;
end;

destructor TJSONDBPointer.Destroy;
begin
  FName.Free;
  FValue.Free;

  inherited;
end;

procedure TJSONDBPointer.SetName(const AValue: TJSONString);
begin
  FName := AValue;

  if FName <> nil then
    FName.FParent := Self;
end;

procedure TJSONDBPointer.SetValue(const AValue: TJSONObjectId);
begin
  FValue := AValue;

  if FValue <> nil then
    FValue.FParent := Self;
end;

procedure TJSONDBPointer.SetActualName(const Value: string);
begin
  inherited;

  if FActualName <> '' then begin
    FName.ActualName := FActualName + '.' + cAttrName;
    FValue.ActualName := FActualName + '.' + cAttrValue;
  end
  else begin
    FName.ActualName := cAttrName;
    FValue.ActualName := cAttrValue;
  end;
end;

function TJSONDBPointer.Clone(const AParent: TJSONValue; const AData: IntPtr): TJSONValue;
begin
  Result := TJSONDBPointer.Create(AParent);
  TJSONDBPointer(Result).Name := FName.Clone(Result, AData) as TJSONString;
  TJSONDBPointer(Result).SetValue(FValue.Clone(Result, AData) as TJSONObjectId);
  Result.ActualName := FActualName;
  Result.FSize := FSize;
  Result.FData := AData;
end;

procedure TJSONDBPointer.Copy(const Source: TJSONValue);
begin
  inherited;

  FName.Copy((Source as TJSONDBPointer).FName);
  FValue.Copy((Source as TJSONDBPointer).FValue);
  ActualName := Source.FActualName;
  FSize := Source.FSize;
end;

{$IFNDEF VER12P}
function TJSONDBPointer.GetValue: TJSONObjectId;
begin
  Result := FValue;
end;
{$ENDIF}

{ TCustomJSONWriter }

constructor TCustomJSONWriter.Create;
begin
  inherited;
end;

destructor TCustomJSONWriter.Destroy;
begin
  inherited;
end;

procedure TCustomJSONWriter.WriteTag(const Value: TJSONValue);
begin
  // do nothing
end;

procedure TCustomJSONWriter.WriteElementIndex(const Value: integer);
begin
  // do nothing
end;

procedure TCustomJSONWriter.WriteString(const Value: string; WriteSize: boolean = False; Escape: boolean = False);
begin
{$IFDEF IS_UNICODE}
  WriteWideString(Value, WriteSize, Escape);
{$ELSE}
  WriteAnsiString(Value, WriteSize, Escape);
{$ENDIF}
end;

{ TJSONTextWriter }

constructor TJSONTextWriter.Create;
begin
  inherited Create;

  FStream := TMemoryStream.Create;
  FStreamOwner := True;
end;

procedure TJSONTextWriter.InternalWriteAsBytes(const Value: TBytes; const Escape: boolean);
var
  i: integer;
  b: Byte;
begin
  if not Escape then begin
    i := Length(Value);
    if i > 0 then
      FStream.WriteBuffer(Value[0], i);
  end
  else
    for i := 0 to Length(Value) - 1 do begin
      b := Value[i];
      case b of
        $08: begin
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
          b := Byte('b');
          FStream.WriteBuffer(b, 1);
        end;
        $09: begin
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
          b := Byte('t');
          FStream.WriteBuffer(b, 1);
        end;
        $0A: begin
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
          b := Byte('n');
          FStream.WriteBuffer(b, 1);
        end;
        $0C: begin
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
          b := Byte('f');
          FStream.WriteBuffer(b, 1);
        end;
        $0D: begin
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
          b := Byte('r');
          FStream.WriteBuffer(b, 1);
        end;
        $22: begin
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
          b := Byte('"');
          FStream.WriteBuffer(b, 1);
        end;
        $2F: begin
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
          b := Byte('/');
          FStream.WriteBuffer(b, 1);
        end;
        $5C: begin
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
          b := Byte('\');
          FStream.WriteBuffer(b, 1);
        end;
      {TODO: hex?}
      else
        FStream.WriteBuffer(b, 1);
      end;
    end;

  FTokenType := jtString;
end;

procedure TJSONTextWriter.InternalWriteAsString(const Value: string; const Escape: boolean);
var
  Data: TBytes;
begin
  Data := Encoding.UTF8.GetBytes(Value);
  InternalWriteAsBytes(Data, Escape);
end;

procedure TJSONTextWriter.InternalWriteQuotedString(const Value: string; const Escape: boolean);
begin
  WriteQuote;
  InternalWriteAsString(Value, Escape);
  WriteQuote;
end;

procedure TJSONTextWriter.Clear;
begin
  FStream.Size := 0;
  FTokenType := jtNone;
end;

procedure TJSONTextWriter.WriteObjectBegin(const Size: Cardinal);
var
  b: Byte;
begin
  b := Byte('{');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtObject;
end;

procedure TJSONTextWriter.WriteObjectEnd;
var
  b: Byte;
begin
  if FTokenType = jtComma then
    FStream.Position := FStream.Position - 1;

  b := Byte('}');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtObjectEnd;
end;

procedure TJSONTextWriter.WriteArrayBegin(const Size: Cardinal = 0);
var
  b: Byte;
begin
  b := Byte('[');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtArray;
end;

procedure TJSONTextWriter.WriteArrayEnd;
var
  b: Byte;
begin
  b := Byte(']');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtArrayEnd;
end;

procedure TJSONTextWriter.WriteJavaBegin(const Size: Cardinal = 0);
begin
  // do nothing
end;

procedure TJSONTextWriter.WriteJavaCodeBegin;
var
  b: Byte;
begin
  b := Byte('{');
  FStream.WriteBuffer(b, 1);
  InternalWriteQuotedString(cJSONCode, False);
  WriteValueSeparator;
  FTokenType := jtJavaCode;
end;

procedure TJSONTextWriter.WriteJavaCodeEnd;
var
  b: Byte;
begin
  b := Byte('}');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtJavaCode;
end;

procedure TJSONTextWriter.WriteJavaScopeBegin;
begin
  InternalWriteQuotedString(cJSONScope, False);
  WriteValueSeparator;
  FTokenType := jtJavaScopeCode;
end;

procedure TJSONTextWriter.WriteJavaScopeEnd;
begin
  // do nothing
  FTokenType := jtJavaScopeCode;
end;

procedure TJSONTextWriter.WriteRegexPatternBegin;
var
  b: Byte;
begin
  b := Byte('{');
  FStream.WriteBuffer(b, 1);
  InternalWriteQuotedString(cJSONRegex, False);
  WriteValueSeparator;
  FTokenType := jtRegex;
end;

procedure TJSONTextWriter.WriteRegexPatternEnd;
begin
  // do nothing
  FTokenType := jtRegex;
end;

procedure TJSONTextWriter.WriteRegexOptionsBegin;
begin
  InternalWriteQuotedString(cJSONOptions, False);
  WriteValueSeparator;
  FTokenType := jtRegex;
end;

procedure TJSONTextWriter.WriteRegexOptionsEnd;
var
  b: Byte;
begin
  b := Byte('}');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtRegex;
end;

procedure TJSONTextWriter.WriteDBPointerBegin;
var
  b: Byte;
begin
  b := Byte('{');
  FStream.WriteBuffer(b, 1);
  InternalWriteQuotedString(cJSONRef, False);
  WriteValueSeparator;
  FTokenType := jtDBPointer;
end;

procedure TJSONTextWriter.WriteDBPointerIdBegin;
begin
  InternalWriteQuotedString(cJSONId, False);
  WriteValueSeparator;
  FTokenType := jtDBPointer;
end;

procedure TJSONTextWriter.WriteDBPointerEnd;
var
  b: Byte;
begin
  b := Byte('}');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtDBPointer;
end;

procedure TJSONTextWriter.WriteOid(const Value: TJSONOid);
begin
  InternalWriteQuotedString(BinToHex(@Value[0], cJSONOidSize), False);
  FTokenType := jtObjectId;
end;

procedure TJSONTextWriter.WriteElementSeparator;
var
  b: Byte;
begin
  if not (FTokenType in [jtComma, jtObject, jtArray]) then begin
    b := Byte(',');
    FStream.WriteBuffer(b, 1);
    FTokenType := jtComma;
  end;
end;

procedure TJSONTextWriter.WriteValueSeparator;
var
  b: Byte;
begin
  b := Byte(':');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtColon;
end;

procedure TJSONTextWriter.WriteQuote;
var
  b: Byte;
begin
  b := Byte('"');
  FStream.WriteBuffer(b, 1);
  FTokenType := jtString;
end;

procedure TJSONTextWriter.WriteNull;
begin
  InternalWriteAsString(cNull, False);
end;

procedure TJSONTextWriter.WriteUndefined;
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONUndefined, False);
  WriteValueSeparator;
  InternalWriteAsString(cTrue, False);

  WriteObjectEnd;
end;

procedure TJSONTextWriter.WriteNumber(const Value: string; const Subtype: TJSONTag);
begin
  InternalWriteAsString(Value, False);
end;

procedure TJSONTextWriter.WriteAnsiString(const Value: AnsiString; const WriteSize, Escape: boolean);
begin
  WriteQuote;
  InternalWriteAsBytes(Encoding.UTF8.GetBytes(Value), Escape);
  WriteQuote;
end;

procedure TJSONTextWriter.WriteWideString(const Value: WideString; const WriteSize, Escape: boolean);
begin
  WriteQuote;
  InternalWriteAsBytes(Encoding.UTF8.GetBytes(Value), Escape);
  WriteQuote;
end;

procedure TJSONTextWriter.WriteBoolean(const Value: boolean);
begin
  if Value then
    InternalWriteAsString(cTrue, False)
  else
    InternalWriteAsString(cFalse, False);
end;

procedure TJSONTextWriter.WriteInt32(const Value: integer);
begin
  InternalWriteAsString(IntToStr(Value), False);
end;

procedure TJSONTextWriter.WriteInt64(const Value: Int64; ExtFormat: boolean = True);
begin
  if ExtFormat then begin
    WriteObjectBegin;

    InternalWriteQuotedString(cJSONInt64, False);
    WriteValueSeparator;
    InternalWriteQuotedString(IntToStr(Value), False);

    WriteObjectEnd;
  end
  else
    InternalWriteAsString(IntToStr(Value), False);
end;

procedure TJSONTextWriter.WriteDateTime(const Value: TDateTime);
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONDate, False);
  WriteValueSeparator;
  InternalWriteQuotedString(FormatDateTime(cDateFormat + ' ' + cTimeFormat, Value), False);

  WriteObjectEnd;
end;

procedure TJSONTextWriter.WriteRegex(const Pattern, Options: string);
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONRegex, False);
  WriteValueSeparator;
  InternalWriteQuotedString(Pattern, True);
  WriteElementSeparator;
  InternalWriteQuotedString(cJSONOptions, False);
  WriteValueSeparator;
  InternalWriteQuotedString(Options, True);

  WriteObjectEnd;
end;

procedure TJSONTextWriter.WriteTimestamp(const Timestamp: TJSONTimestamp);
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONTimestamp, False);
  WriteValueSeparator;

  WriteObjectBegin;
  InternalWriteQuotedString(cJSONTime, False);
  WriteValueSeparator;
  InternalWriteAsString(IntToStr(Timestamp.Timestamp.Value), False);
  WriteElementSeparator;
  InternalWriteQuotedString(cJSONIncrement, False);
  WriteValueSeparator;
  InternalWriteAsString(IntToStr(Timestamp.Increment.Value), False);
  WriteObjectEnd;

  WriteObjectEnd;
end;

procedure TJSONTextWriter.WriteBinary(const Value: TBytes; const Subtype: TJSONBinarySubtype);
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONBinary, False);
  WriteValueSeparator;
  InternalWriteQuotedString(Encoding.ASCII.GetString(TBase64.Encode(Value)), False);
  WriteElementSeparator;
  InternalWriteQuotedString(cJSONType, False);
  WriteValueSeparator;
  InternalWriteQuotedString(IntToStr(Integer(Subtype)), False);

  WriteObjectEnd;
end;

procedure TJSONTextWriter.WriteDouble(const Value: double);
begin
  InternalWriteAsString(StringReplace(FloatToStr(Value), ',', '.', [rfReplaceAll]), False);
end;

procedure TJSONTextWriter.WriteMinKey;
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONMinKey, False);
  WriteValueSeparator;
  InternalWriteAsString('1', False);

  WriteObjectEnd;
end;

procedure TJSONTextWriter.WriteMaxKey;
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONMaxKey, False);
  WriteValueSeparator;
  InternalWriteAsString('1', False);

  WriteObjectEnd;
end;

procedure TJSONTextWriter.WriteDecimal128(const Value: TDecimal128);
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONDecimal128, False);
  WriteValueSeparator;
  InternalWriteQuotedString(string(Value), False);

  WriteObjectEnd;
end;

procedure TJSONTextWriter.WriteObjectId(const Value: TJSONOid);
begin
  WriteObjectBegin;

  InternalWriteQuotedString(cJSONOid, False);
  WriteValueSeparator;
  WriteOid(Value);

  WriteObjectEnd;
end;

class function TJSONTextWriter.CalcSize(const Value: TJSONValue; const FieldMap: TStringList): integer;
var
  i, n: integer;
begin
  Result := 0;

  case Value.Tag of
    jtObject: begin
      // 2 chars brackets + pair length + 1 char comma
      Result := 2;
      n := TJSONObject(Value).Pairs.Count;
      for i := 0 to n - 1 do begin
        Result := Result + CalcSize(TJSONObject(Value).Pairs[i], FieldMap);
        if i < n then
          Inc(Result);
      end;
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtPair: begin
      // pair name length + 1 char colon
      Result := 1;
      Result := Result + CalcSize(TJSONPair(Value).Name, FieldMap);
      Result := Result + CalcSize(TJSONPair(Value).Value, FieldMap);
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtArray: begin
      // 2 chars brackets + pair length + 1 char comma
      Result := 2;
      n := TJSONArray(Value).Elements.Count;
      for i := 0 to n - 1 do begin
        Result := Result + CalcSize(TJSONArray(Value).Elements[i], FieldMap);
        if i < n then
          Inc(Result);
      end;
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtString: begin
      // 2 chars quotes + value length
      Result := 2;
      Result := Result + Length(Value.AsString);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtUndefined: begin
      // 2 chars brackets + 2chars quotes + length of cJSONUndefined + colon + value length
      Result := 5 + Length(cJSONUndefined);
      Result := Result + 4 {length of 'null'};
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtNumber,
    jtInt32,
    jtDouble: begin
      Value.Size := Length(Value.AsString);
      Result := Value.Size;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtInt64: begin
      // 2 chars brackets + 2chars quotes + length of cJSONInt64 + colon + 2chars quotes + value length
      Result := 7 + Length(cJSONInt64);
      Result := Result + Length(Value.AsString);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtDateTime: begin
      // 2 chars brackets + 2chars quotes + length of cJSONDate + colon + 2chars quotes + value length
      Result := 7 + Length(cJSONDate);
      Result := Result + Length(Value.AsString);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtTimestamp: begin
      // 2 chars brackets + 2 chars quotes + length of cJSONTimestamp + colon + 2 chars brackets
      // 2 chars quotes + length of cJSONTime + colon + timestamp length + comma
      // 2 chars quotes + length of cJSONIncrement + colon + increment length
      Result := 14 + Length(cJSONTimestamp) + Length(cJSONTime) + Length(cJSONIncrement);
      Result := Result + CalcSize(TJSONTimestamp(Value).Timestamp, FieldMap);
      Result := Result + CalcSize(TJSONTimestamp(Value).Increment, FieldMap);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtNull: begin
      Value.Size := 4;
      Result := Value.Size;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtObjectId: begin
      Value.Size := 24;
      Result := Value.Size;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtBoolean: begin
      Value.Size := 5;
      Result := Value.Size;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtJavaCode: begin
      // 2 chars brackets + 2 chars quotes + length of cJSONCode + colon + code length
      Result := 5 + Length(cJSONCode);
      Result := Result + CalcSize(TJSONJavaCode(Value).Code, FieldMap);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtJavaScopeCode: begin
      // 2 chars brackets + 2 chars quotes + length of cJSONCode + colon + code length + comma
      // 2 chars quotes + length of cJSONScope + colon + scope length
      Result := 9 + Length(cJSONCode) + Length(cJSONScope);
      Result := Result + CalcSize(TJSONJavaScopeCode(Value).Code, FieldMap);
      Result := Result + CalcSize(TJSONJavaScopeCode(Value).Scope, FieldMap);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtRegex: begin
      // 2 chars brackets + 2 chars quotes + length of cJSONRegex + colon + regex length + comma
      // 2 chars quotes + length of cJSONOptions + colon + options length
      Result := 9 + Length(cJSONRegex) + Length(cJSONOptions);
      Result := Result + CalcSize(TJSONRegex(Value).Pattern, FieldMap);
      Result := Result + CalcSize(TJSONRegex(Value).Options, FieldMap);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    // 2 chars brackets + 2 chars quotes + length of cJSONBinary + colon + 2 chars quotes + data length + comma
    // 2 chars quotes + length of cJSONType + colon + 2 chars quotes + 1 byte type
    jtBinary: begin
      Result := 14 + Length(cJSONBinary) + Length(cJSONType);
      Result := Result + Length(Value.AsString);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtMinKey: begin
      // 2 chars brackets + 2chars quotes + length of cJSONMinKey + colon + value length
      Result := 5 + Length(cJSONMinKey);
      Result := Result + 1 {length of '1'};
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtMaxKey: begin
      // 2 chars brackets + 2chars quotes + length of cJSONMaxKey + colon + value length
      Result := 5 + Length(cJSONMaxKey);
      Result := Result + 1 {length of '1'};
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtDBPointer: begin
      // 2 chars brackets + 2chars quotes + length of cJSONRef + colon + name length + comma
      // 2 chars quotes + length of cJSONId + colon + 2 chars quotes + value length
      Result := 11 + Length(cJSONRef) + Length(cJSONId);
      Result := Result + CalcSize(TJSONDBPointer(Value).Name, FieldMap);
      Result := Result + CalcSize(TJSONDBPointer(Value).FValue, FieldMap);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
    jtDecimal128: begin
      // 2 chars brackets + 2chars quotes + length of cJSONDecimal128 + colon + 2chars quotes + value length
      Result := 7 + Length(cJSONDecimal128);
      Result := Result + Length(Value.AsString);
      Value.Size := Result;
      if Value.ActualName <> '' then
        FieldMap.AddObject(Value.ActualName, Value);
    end;
  else
    Assert(False);
  end;
end;

function TJSONTextWriter.AsBytes: TBytes;
begin
  FStream.Seek(0, soBeginning);
  SetLength(Result, FStream.Size);
  FStream.ReadBuffer(Result[0], FStream.Size);
end;

function TJSONTextWriter.AsText: string;
begin
  Result := Encoding.UTF8.GetString(AsBytes);
end;

{ TJSONStreamWriter }

constructor TJSONStreamWriter.Create;
begin
  inherited Create;
end;

constructor TJSONStreamWriter.Create(AStream: TStream);
begin
  inherited Create;

  FStream := AStream;
  FStreamOwner := False;
end;

destructor TJSONStreamWriter.Destroy;
begin
  if FStreamOwner then
    FStream.Free;

  inherited;
end;

procedure TJSONStreamWriter.SetStream(Value: TStream);
begin
  if FStream <> Value then begin
    if FStreamOwner then
      FreeAndNil(FStream);

    FStream := Value;
    FStreamOwner := False;
  end;
end;

procedure TJSONStreamWriter.Clear;
begin
  Assert(FStream <> nil);

  FStream.Size := 0;
end;

{ TJSONBinaryWriter }

procedure TJSONBinaryWriter.InternalWriteByte(const Value: byte);
begin
  FStream.Write(Value, 1);
end;

procedure TJSONBinaryWriter.InternalWriteBytes(const Value: TBytes);
var
  n: integer;
begin
  n := Length(Value);
  if n > 0 then
    FStream.Write(Value[0], n);
end;

procedure TJSONBinaryWriter.InternalWriteInt32(const Value: integer);
begin
  FStream.Write(Value, 4);
end;

procedure TJSONBinaryWriter.InternalWriteInt64(const Value: Int64);
begin
  FStream.Write(Value, 8);
end;

procedure TJSONBinaryWriter.WriteTag(const Value: TJSONValue);
begin
  case Value.Tag of
    jtObject: InternalWriteByte(byte(btObject));
    jtArray: InternalWriteByte(byte(btArray));
    jtString: InternalWriteByte(byte(btString));
    jtNumber:
      if TJSONNumber(Value).Subtype = jtDouble then
        InternalWriteByte(byte(btDouble))
      else if TJSONNumber(Value).Subtype = jtInt32 then
        InternalWriteByte(byte(btInt32))
      else
        InternalWriteByte(byte(btInt64));
    jtBoolean: InternalWriteByte(byte(btBoolean));
    jtNull: InternalWriteByte(byte(btNull));
    jtObjectId: InternalWriteByte(byte(btObjectId));
    jtInt32: InternalWriteByte(byte(btInt32));
    jtInt64: InternalWriteByte(byte(btInt64));
    jtDateTime: InternalWriteByte(byte(btDateTime));
    jtJavaCode: InternalWriteByte(byte(btJavaCode));
    jtUndefined: InternalWriteByte(byte(btUndefined));
    jtJavaScopeCode: InternalWriteByte(byte(btJavaScopeCode));
    jtRegex: InternalWriteByte(byte(btRegex));
    jtTimestamp: InternalWriteByte(byte(btTimeStamp));
    jtBinary: InternalWriteByte(byte(btBinary));
    jtDouble: InternalWriteByte(byte(btDouble));
    jtDBPointer: InternalWriteByte(byte(btDBPointer));
    jtMinKey: InternalWriteByte(byte(btMinKey));
    jtMaxKey: InternalWriteByte(byte(btMaxKey));
    jtDecimal128: InternalWriteByte(byte(btDecimal128));
  else
    Assert(False);
  end;
end;

procedure TJSONBinaryWriter.WriteElementIndex(const Value: integer);
begin
  WriteAnsiString(AnsiString(IntToStr(Value)), False, False);
end;

procedure TJSONBinaryWriter.WriteObjectBegin(const Size: Cardinal = 0);
begin
  InternalWriteInt32(Size);
end;

procedure TJSONBinaryWriter.WriteObjectEnd;
begin
  InternalWriteByte(0);
end;

procedure TJSONBinaryWriter.WriteArrayBegin(const Size: Cardinal = 0);
begin
  InternalWriteInt32(Size);
end;

procedure TJSONBinaryWriter.WriteArrayEnd;
begin
  InternalWriteByte(0);
end;

procedure TJSONBinaryWriter.WriteJavaBegin(const Size: Cardinal = 0);
begin
  InternalWriteInt32(Size);
end;

procedure TJSONBinaryWriter.WriteJavaCodeBegin;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteJavaCodeEnd;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteJavaScopeBegin;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteJavaScopeEnd;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteRegexPatternBegin;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteRegexPatternEnd;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteRegexOptionsBegin;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteRegexOptionsEnd;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteDBPointerBegin;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteDBPointerIdBegin;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteOid(const Value: TJSONOid);
var
  i: integer;
begin
  for i := 0 to 11 do
    InternalWriteByte(Value[i]);
end;

procedure TJSONBinaryWriter.WriteDBPointerEnd;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteQuote;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteNumber(const Value: string; const Subtype: TJSONTag);
var
  StrValue: string;
  i64Value: Int64;
  DoubleValue: double;
begin
  if Subtype = jtDouble then begin
    StrValue := StringReplace(Value, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, [rfReplaceAll]);
    StrValue := StringReplace(StrValue, ',', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, [rfReplaceAll]);
    DoubleValue := StrToFloat(StrValue);
    WriteDouble(DoubleValue);
  end
  else if Subtype = jtInt32 then
    WriteInt32(StrToInt(Value))
  else begin
    i64Value := StrToInt64(Value);
    WriteInt64(i64Value);
  end;
end;

procedure TJSONBinaryWriter.WriteAnsiString(const Value: AnsiString; const WriteSize, Escape: boolean);
var
  Buffer: TBytes;
begin
  Buffer := Encoding.UTF8.GetBytes(Value);
  if WriteSize then
    InternalWriteInt32(Length(Buffer) + 1);
  InternalWriteBytes(Buffer);
  InternalWriteByte(0);
end;

procedure TJSONBinaryWriter.WriteWideString(const Value: WideString; const WriteSize, Escape: boolean);
var
  Buffer: TBytes;
begin
  Buffer := Encoding.UTF8.GetBytes(Value);
  if WriteSize then
    InternalWriteInt32(Length(Buffer) + 1);
  InternalWriteBytes(Buffer);
  InternalWriteByte(0);
end;

procedure TJSONBinaryWriter.WriteBoolean(const Value: boolean);
begin
  if Value then
    InternalWriteByte(1)
  else
    InternalWriteByte(0);
end;

procedure TJSONBinaryWriter.WriteNull;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteObjectId(const Value: TJSONOid);
begin
  WriteOid(Value);
end;

procedure TJSONBinaryWriter.WriteInt32(const Value: integer);
begin
  InternalWriteInt32(Value);
end;

procedure TJSONBinaryWriter.WriteInt64(const Value: Int64; ExtFormat: boolean = True);
begin
  InternalWriteInt64(Value);
end;

procedure TJSONBinaryWriter.WriteDateTime(const Value: TDateTime);
var
  i64: Int64;
begin
  i64 := DateTimeToUnix(Value);
  InternalWriteInt64(i64 * MSecsPerSec);
end;

procedure TJSONBinaryWriter.WriteUndefined;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteRegex(const Pattern, Options: string);
begin
  WriteWideString(WideString(Pattern), False, False);
  WriteWideString(WideString(Options), False, False);
end;

procedure TJSONBinaryWriter.WriteTimestamp(const Timestamp: TJSONTimestamp);
var
  i64: Int64;
begin
  Integer((@i64)^) := Timestamp.Timestamp.Value;
  Integer(PtrOffset(@i64, 4)^) := Timestamp.Increment.Value;
  InternalWriteInt64(i64);
end;

procedure TJSONBinaryWriter.WriteBinary(const Value: TBytes; const Subtype: TJSONBinarySubtype);
begin
  InternalWriteInt32(length(Value));
  InternalWriteByte(byte(Subtype));
  InternalWriteBytes(Value);
end;

procedure TJSONBinaryWriter.WriteDouble(const Value: double);
begin
  InternalWriteInt64(Int64((@Value)^));
end;

procedure TJSONBinaryWriter.WriteMinKey;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteMaxKey;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteDecimal128(const Value: TDecimal128);
var
  d: TBinaryDecimal128;
begin
  if StringToDecimal128(TStringDecimal128(Value), @d) then begin
    InternalWriteInt64(Int64((@d.Low)^));
    InternalWriteInt64(Int64((@d.High)^));
  end;
end;

procedure TJSONBinaryWriter.WriteElementSeparator;
begin
  // do nothing
end;

procedure TJSONBinaryWriter.WriteValueSeparator;
begin
  // do nothing
end;

class function TJSONBinaryWriter.CalcSize(const Value: TJSONValue): integer;
var
  i: integer;
begin
  Result := 0;

  case Value.Tag of
    jtObject: begin
      // 4 bytes size + 1 byte end marker
      Result := 5;
      for i := 0 to TJSONObject(Value).Pairs.Count - 1 do
        Result := Result + CalcSize(TJSONObject(Value).Pairs[i]);
      TJSONObject(Value).Size := Result;
    end;
    jtPair: begin
      // 1 byte tag + pair name length + zero char
      Result := 1;
      if TJSONPair(Value).Name.FStringType = jsWideString then
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONPair(Value).Name.AsWideString)) + 1
      else
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONPair(Value).Name.AsAnsiString)) + 1;
      Result := Result + CalcSize(TJSONPair(Value).Value);
    end;
    jtArray: begin
      // 4 bytes size + 1 byte end marker
      Result := 5;
      for i := 0 to TJSONArray(Value).Elements.Count - 1 do begin
        // 1 byte tag + element index length + zero char
        Result := Result + 1 + Length(IntToStr(i)) + 1;
        Result := Result + CalcSize(TJSONArray(Value).Elements[i]);
      end;
      TJSONArray(Value).Size := Result;
    end;
    jtString: begin
      // 4 byte size + value length + zero char
      Result := 4;
      if TJSONString(Value).FStringType = jsWideString then
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONWideString(Value).AsWideString)) + 1
      else
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONAnsiString(Value).AsAnsiString)) + 1;
    end;
    jtNumber:
      if TJSONNumber(Value).Subtype = jtInt32 then
        Result := 4
      else
        Result := 8;
    jtInt64,
    jtDouble,
    jtTimestamp,
    jtDateTime: Result := 8;
    jtBoolean: Result := 1;
    jtObjectId: Result := 12;
    jtInt32: Result := 4;
    jtDecimal128: Result := 16;
    jtJavaCode: begin
      // 4 byte code length + zero char
      Result := 4;
      if TJSONString(Value).FStringType = jsWideString then
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONWideString(TJSONJavaCode(Value).Code).AsWideString)) + 1
      else
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONAnsiString(TJSONJavaCode(Value).Code).AsAnsiString)) + 1;
    end;
    jtJavaScopeCode: begin
      // 4 byte total size + 4 byte code size + code length + zero char + code length
      Result := 8;
      if TJSONString(TJSONJavaScopeCode(Value).Code).FStringType = jsWideString then
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONWideString(TJSONJavaScopeCode(Value).Code).AsWideString)) + 1
      else
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONAnsiString(TJSONJavaScopeCode(Value).Code).AsAnsiString)) + 1;
      Result := Result + CalcSize(TJSONJavaScopeCode(Value).Scope);
      TJSONJavaScopeCode(Value).Size := Result;
    end;
    // pattern length + zero char + options length + zero char
    jtRegex:
      if TJSONString(TJSONRegex(Value).Pattern).FStringType = jsWideString then
        Result := Length(Encoding.UTF8.GetBytes(TJSONWideString(TJSONRegex(Value).Pattern).AsWideString)) +
                  Length(Encoding.UTF8.GetBytes(TJSONWideString(TJSONRegex(Value).Options).AsWideString)) + 2
      else
        Result := Length(Encoding.UTF8.GetBytes(TJSONAnsiString(TJSONRegex(Value).Pattern).AsAnsiString)) +
                  Length(Encoding.UTF8.GetBytes(TJSONAnsiString(TJSONRegex(Value).Options).AsAnsiString)) + 2;
    // 4 byte size + 1 byte subtype + value size
    jtBinary: Result := 5 + length(TJSONBinary(Value).Binary.FValue);
    jtDBPointer: begin
      // 4 byte size + name length + zero char + 12 bytes id
      Result := 16;
      if TJSONString(TJSONDBPointer(Value).Name).FStringType = jsWideString then
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONDBPointer(Value).Name.AsWideString)) + 1
      else
        Result := Result + Length(Encoding.UTF8.GetBytes(TJSONDBPointer(Value).Name.AsAnsiString)) + 1;
    end;
    jtNull,
    jtUndefined,
    jtMinKey,
    jtMaxKey: Result := 0;
  else
    Assert(False);
  end;
end;

{ TJSONSerializer }

constructor TJSONSerializer.Create;
begin
  inherited Create;

  FWriter := nil;
end;

destructor TJSONSerializer.Destroy;
begin
  FreeWriter;

  inherited;
end;

procedure TJSONSerializer.InitWriter(const WriterClass: TJSONWriterClass);
begin
  if (FWriter = nil) or (FWriter.ClassType <> WriterClass) then begin
    FreeWriter;
    FWriter := WriterClass.Create;
  end;
end;

procedure TJSONSerializer.FreeWriter;
begin
  FreeAndNil(FWriter);
end;

class function TJSONSerializer.GetTextWriterClass: TJSONWriterClass;
begin
  Result := TJSONTextWriter;
end;

procedure TJSONSerializer.ProcessValue(const Value: TJSONValue);
begin
  if Value = nil then
    Exit;

  case Value.FTag of
    jtObject: ProcessObject(TJSONObject(Value));
    jtArray: ProcessArray(TJSONArray(Value));
    jtString: ProcessString(TJSONString(Value), True, True);
    jtNumber: ProcessNumber(TJSONNumber(Value));
    jtBoolean: ProcessBoolean(TJSONBoolean(Value));
    jtNull: ProcessNull(TJSONNull(Value));
    jtObjectId: ProcessObjectId(TJSONObjectId(Value));
    jtInt32: ProcessInt32(TJSONInt32(Value));
    jtInt64: ProcessInt64(TJSONInt64(Value));
    jtDateTime: ProcessDateTime(TJSONDateTime(Value));
    jtJavaCode: ProcessJavaCode(TJSONJavaCode(Value));
    jtUndefined: ProcessUndefined(TJSONUndefined(Value));
    jtJavaScopeCode: ProcessJavaScopeCode(TJSONJavaScopeCode(Value));
    jtRegex: ProcessRegex(TJSONRegex(Value));
    jtTimestamp: ProcessTimestamp(TJSONTimestamp(Value));
    jtBinary: ProcessBinary(TJSONBinary(Value));
    jtDouble: ProcessDouble(TJSONDouble(Value));
    jtMinKey: ProcessMinKey(TJSONMinKey(Value));
    jtMaxKey: ProcessMaxKey(TJSONMaxKey(Value));
    jtDBPointer: ProcessDBPointer(TJSONDBPointer(Value));
    jtDecimal128: ProcessDecimal128(TJSONDecimal128(Value));
  end;
end;

procedure TJSONSerializer.ProcessObject(const Value: TJSONObject);
var
  i: integer;
  Pair: TJSONPair;
begin
  FWriter.WriteObjectBegin(Value.Size);

  for i := 0 to Value.FPairs.Count - 1 do begin
    if i > 0 then
      FWriter.WriteElementSeparator;

    Pair := TJSONPair(Value.FPairs[i]);

    FWriter.WriteTag(Pair.Value);
    ProcessString(Pair.Name, False, False);
    FWriter.WriteValueSeparator;
    ProcessValue(Pair.Value);
  end;

  FWriter.WriteObjectEnd;
end;

procedure TJSONSerializer.ProcessArray(const Value: TJSONArray);
var
  i: integer;
begin
  FWriter.WriteArrayBegin(Value.Size);

  for i := 0 to Value.FElements.Count - 1 do begin
    if i > 0 then
      FWriter.WriteElementSeparator;

    FWriter.WriteTag(TJSONValue(Value.FElements[i]));
    FWriter.WriteElementIndex(i);
    ProcessValue(TJSONValue(Value.FElements[i]));
  end;

  FWriter.WriteArrayEnd;
end;

procedure TJSONSerializer.ProcessString(const Value: TJSONString; const WriteSize, Escape: boolean);
begin
  if Value.FStringType = jsAnsiString then
    FWriter.WriteAnsiString(TJSONAnsiString(Value).AsAnsiString, WriteSize, Escape)
  else
    FWriter.WriteWideString(TJSONWideString(Value).AsWideString, WriteSize, Escape);
end;

procedure TJSONSerializer.ProcessNumber(const Value: TJSONNumber);
begin
  FWriter.WriteNumber(Value.Value, Value.Subtype);
end;

procedure TJSONSerializer.ProcessBoolean(const Value: TJSONBoolean);
begin
  FWriter.WriteBoolean(Value.Value);
end;

procedure TJSONSerializer.ProcessNull(const Value: TJSONNull);
begin
  FWriter.WriteNull;
end;

procedure TJSONSerializer.ProcessObjectId(const Value: TJSONObjectId);
begin
  FWriter.WriteObjectId(Value.FValue);
end;

procedure TJSONSerializer.ProcessInt32(const Value: TJSONInt32);
begin
  FWriter.WriteInt32(Value.Value);
end;

procedure TJSONSerializer.ProcessInt64(const Value: TJSONInt64);
begin
  FWriter.WriteInt64(Value.Value);
end;

procedure TJSONSerializer.ProcessDateTime(const Value: TJSONDateTime);
begin
  FWriter.WriteDateTime(Value.Value);
end;

procedure TJSONSerializer.ProcessJavaCode(const Value: TJSONJavaCode);
begin
  FWriter.WriteJavaCodeBegin;
  ProcessString(Value.FCode, True, True);
  FWriter.WriteJavaCodeEnd;
end;

procedure TJSONSerializer.ProcessUndefined(const Value: TJSONUndefined);
begin
  FWriter.WriteUndefined;
end;

procedure TJSONSerializer.ProcessJavaScopeCode(const Value: TJSONJavaScopeCode);
begin
  FWriter.WriteJavaBegin(Value.Size);
  FWriter.WriteJavaCodeBegin;
  ProcessString(Value.FCode, True, True);
  FWriter.WriteElementSeparator;
  FWriter.WriteJavaScopeBegin;
  ProcessObject(Value.FScope);
  FWriter.WriteJavaScopeEnd;
  FWriter.WriteJavaCodeEnd;
end;

procedure TJSONSerializer.ProcessRegex(const Value: TJSONRegex);
begin
  FWriter.WriteRegexPatternBegin;
  ProcessString(Value.FPattern, False, True);
  FWriter.WriteRegexPatternEnd;
  FWriter.WriteElementSeparator;
  FWriter.WriteRegexOptionsBegin;
  ProcessString(Value.FOptions, False, True);
  FWriter.WriteRegexOptionsEnd;
end;

procedure TJSONSerializer.ProcessTimestamp(const Value: TJSONTimestamp);
begin
  FWriter.WriteTimestamp(Value);
end;

procedure TJSONSerializer.ProcessBinary(const Value: TJSONBinary);
begin
  FWriter.WriteBinary(Value.Binary.FValue, TJSONBinarySubtype(Value.Subtype.Value));
end;

procedure TJSONSerializer.ProcessDouble(const Value: TJSONDouble);
begin
  FWriter.WriteDouble(Value.Value);
end;

procedure TJSONSerializer.ProcessMinKey(const Value: TJSONMinKey);
begin
  FWriter.WriteMinKey;
end;

procedure TJSONSerializer.ProcessMaxKey(const Value: TJSONMaxKey);
begin
  FWriter.WriteMaxKey;
end;

procedure TJSONSerializer.ProcessDBPointer(const Value: TJSONDBPointer);
begin
  FWriter.WriteDBPointerBegin;
  ProcessString(Value.FName, True, False);
  FWriter.WriteElementSeparator;
  FWriter.WriteDBPointerIdBegin;
  FWriter.WriteOid(Value.FValue.FValue);
  FWriter.WriteDBPointerEnd;
end;

procedure TJSONSerializer.ProcessDecimal128(const Value: TJSONDecimal128);
begin
  FWriter.WriteDecimal128(TDecimal128(Value.Value));
end;

procedure TJSONSerializer.ToBinary(const Value: TJSONValue; const Stream: TStream);
begin
  InitWriter(TJSONBinaryWriter);
  TJSONBinaryWriter(FWriter).Stream := Stream;
  TJSONBinaryWriter(FWriter).Clear;

  TJSONBinaryWriter(FWriter).CalcSize(Value);
  ProcessValue(Value);
end;

procedure TJSONSerializer.ToText(const Value: TJSONValue; out Text: string);
begin
  InitWriter(GetTextWriterClass);
  TJSONTextWriter(FWriter).Clear;

  ProcessValue(Value);

  Text := TJSONTextWriter(FWriter).AsText;
end;

procedure TJSONSerializer.ToText(const Value: TJSONValue; const Stream: TStream);
begin
  InitWriter(TJSONStreamWriter);
  TJSONStreamWriter(FWriter).Stream := Stream;
  TJSONBinaryWriter(FWriter).Clear;

  ProcessValue(Value);
end;

{ TCustomJSONReader }

constructor TCustomJSONReader.Create;
begin
  inherited;
end;

destructor TCustomJSONReader.Destroy;
begin
  inherited;
end;

function TCustomJSONReader.GetTagPos: Int64;
begin
  Result := FTagPos;
end;

procedure TCustomJSONReader.Check(Condition: boolean; const Msg: string);
begin
  if not Condition then
    raise JSONException.Create(Msg);
end;

function TCustomJSONReader.ReadString: string;
var
  Size: integer;
begin
  Result := ReadString(False, False, Size);
end;

function TCustomJSONReader.ReadString(const ZeroTerminated, Unescape: boolean; out Size: integer): string;
begin
{$IFDEF IS_UNICODE}
  Result := ReadWideString(ZeroTerminated, Unescape, Size);
{$ELSE}
  Result := ReadAnsiString(ZeroTerminated, Unescape, Size);
{$ENDIF}
end;

{ TJSONParser }

procedure TJSONParser.InitParser;
begin
  inherited;
end;

procedure TJSONParser.ToRightQuoteP(RightQuote: char);
var
  c: char;
  i, n: integer;
begin
  inherited;

  while True do begin
    c := Text[Pos];

    if (c = RightQuote) and (Pos > 1) then begin
      if Text[Pos - 1] <> '\' then
        Break;

      i := Pos - 2;
      n := 0;
      while (i > 0) and (Text[i] = '\') do begin
        Inc(n);
        Dec(i);
      end;

      if (n and 1) = 0 then begin
        Inc(Pos);
        inherited;
      end
      else
        Break;
    end
    else
      Break;
  end;
end;

{ TJSONTextReader }

destructor TJSONTextReader.Destroy;
begin
  FParser.Free;

  inherited;
end;

function TJSONTextReader.InternalUnescape(const Value: string): string;
var
  Builder: StringBuilder;
  l, i: integer;
  c, c1: char;
  cc, cc1: SmallInt;
begin
  Result := Value;

  i := 1;
  l := Length(Result);
  Builder := StringBuilder.Create(l);
  try
    while i <= l do begin
      c := Result[i];
      cc := Ord(c);
      if (cc = $5C) and (i < l) then begin
        c1 := Result[i + 1];
        cc1 := Ord(c1);
        case cc1 of
          {TODO: uppercase?}
          Ord('b'): begin
            Builder.Append(#$08);
            Inc(i);
          end;
          Ord('t'): begin
            Builder.Append(#$09);
            Inc(i);
          end;
          Ord('n'): begin
            Builder.Append(#$0A);
            Inc(i);
          end;
          Ord('f'): begin
            Builder.Append(#$0C);
            Inc(i);
          end;
          Ord('r'): begin
            Builder.Append(#$0D);
            Inc(i);
          end;
          Ord('"'),
          Ord('/'),
          Ord('\'): begin
            Builder.Append(c1);
            Inc(i);
          end
        else
          Builder.Append(c);
        end;
      end
      else
        Builder.Append(c);
      Inc(i);
    end;

    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TJSONTextReader.InternalReadString(const Unescape: boolean): string;
begin
  Result := '';
  Code := FParser.GetNext(Lexem);
  if Code = lcEnd then
    Exit;

  Result := UnQuote(Lexem);

  if Unescape then
    Result := InternalUnescape(Result);
end;

procedure TJSONTextReader.InternalReadLexem;
begin
  Code := FParser.GetNext(Lexem);
end;

procedure TJSONTextReader.Initialize;
begin
  inherited;

  if FStream = nil then
    raise ArgumentException.Create;

  FreeAndNil(FParser);

  FTokenType := jtNone;
  FParser := TJSONParser.Create(FStream, FStreamSize, Encoding.UTF8);
  FParser.QuotedString := True;
  FParser.DecSeparator := '.';

  FAllowedArrayTerminators := [jtArrayEnd];
  FRestrictedArrayTerminators := [jtNone, jtObjectEnd];
end;

procedure TJSONTextReader.Finalize;
begin
  FreeAndNil(FParser);

  inherited;
end;

function TJSONTextReader.GetCurPos: Int64;
begin
  Result := FStartStreamPos + FParser.CurrPos;
end;

procedure TJSONTextReader.Back;
begin
  FParser.Back;
end;

function TJSONTextReader.ReadTag: TJSONTag;
begin
  FTagPos := GetCurPos;

  InternalReadLexem;

  if Lexem = '{' then begin
    FTokenType := jtObject;

    // try to detect extended data types
    InternalReadLexem;
    Lexem := UnQuote(Lexem);
    if Code <> lcEnd then begin
      if SameText(Lexem, cJSONOid) then
        FTokenType := jtObjectId
      else if SameText(Lexem, cJSONInt64) then
        FTokenType := jtInt64
      else if SameText(Lexem, cJSONDate) then
        FTokenType := jtDateTime
      else if SameText(Lexem, cJSONRegex) then
        FTokenType := jtRegex
      else if SameText(Lexem, cJSONTimestamp) then
        FTokenType := jtTimestamp
      else if SameText(Lexem, cJSONBinary) then
        FTokenType := jtBinary
      else if SameText(Lexem, cJSONUndefined) then
        FTokenType := jtUndefined
      else if SameText(Lexem, cJSONCode) then
        FTokenType := jtJavaCode
      else if SameText(Lexem, cJSONScope) then
        FTokenType := jtJavaScopeCode
      else if SameText(Lexem, cJSONMinKey) then
        FTokenType := jtMinKey
      else if SameText(Lexem, cJSONMaxKey) then
        FTokenType := jtMaxKey
      else if SameText(Lexem, cJSONRef) then
        FTokenType := jtDBPointer
      else if SameText(Lexem, cJSONRegex) then
        FTokenType := jtRegex
      else if SameText(Lexem, cJSONBinary) then
        FTokenType := jtBinary
      else if SameText(Lexem, cJSONDecimal128) then
        FTokenType := jtDecimal128
      else
        FParser.Back;
    end;
  end
  else if Lexem = '}' then
    FTokenType := jtObjectEnd
  else if Code = lxComma then
    FTokenType := jtComma
  else if Code = lxColon then
    FTokenType := jtColon
  else if Code = lxLeftSqBracket then
    FTokenType := jtArray
  else if Code = lxRightSqBracket then
    FTokenType := jtArrayEnd
  else if (Code = lcString) and IsQuoted(Lexem) then begin
    FTokenType := jtString;
    FParser.Back;
  end
  else if (Code = lcNumber) or (Code = lxDash) or (Code = lxPlus) then begin
    FTokenType := jtNumber;
    FParser.Back;
  end
  else if Code = lcIdent then begin
    if SameText(Lexem, cFalse) or SameText(Lexem, cTrue) then begin
      FTokenType := jtBoolean;
      FParser.Back;
    end
    else if SameText(Lexem, cNull) then
      FTokenType := jtNull
    else begin
      FTokenType := jtString;
      FParser.Back;
    end;
  end
  else if (Code = lcString) and (Length(Lexem) > 0) and (Lexem[1] = '$') then begin
    FTokenType := jtString;
    FParser.Back;
  end
  else
    FTokenType := jtNone;

  Result := FTokenType;
end;

function TJSONTextReader.ReadExtTag(const Tag: TJSONTag): TJSONTag;
begin
  Result := ReadTag;
end;

function TJSONTextReader.ReadObjectSize: integer;
begin
  Result := 0;
end;

function TJSONTextReader.ReadArraySize: integer;
begin
  Result := 0;
end;

function TJSONTextReader.ReadElementName: string;
begin
  Result := '';
end;

function TJSONTextReader.ReadAnsiString(const ZeroTerminated, Unescape: boolean; out Size: integer): AnsiString;
var
  TmpStr: string;
begin
  TmpStr := InternalReadString(Unescape);
  Result := AnsiString(TmpStr);
  Size := Length(Result);
end;

function TJSONTextReader.ReadWideString(const ZeroTerminated, Unescape: boolean; out Size: integer): WideString;
begin
  Result := WideString(InternalReadString(Unescape));
  Size := Length(Result);
end;

function TJSONTextReader.ReadValueSeparator: boolean;
begin
  Code := FParser.GetNext(Lexem);
  Result := Code = lxColon;
  if not Result then
    FParser.Back;
end;

function TJSONTextReader.ReadElementSeparator: boolean;
begin
  Code := FParser.GetNext(Lexem);
  Result := Code = lxComma;
  if not Result then
    FParser.Back;
end;

function TJSONTextReader.ReadNumber(out Subtype: TJSONTag): string;
begin
  Result := '';
  Subtype := jtInt32;

  Code := FParser.GetNext(Lexem);
  Check(Code <> lcEnd, cInvalidNumber);

  FParser.OmitBlank := False;
  try
    while (Code <> lcEnd) and (Code <> lcBlank) and (Code <> lxComma) and (Code <> lxRightSqBracket) and (Lexem <> '}') do begin
      if (Pos('.', Lexem) > 0) or SameText(Lexem, 'e') then
        Subtype := jtDouble;
      Result := Result + Lexem;
      Code := FParser.GetNext(Lexem);
    end;

    if Code <> lcEnd then
      FParser.Back;
  finally
    FParser.OmitBlank := True;
  end;
end;

function TJSONTextReader.ReadBoolean: boolean;
begin
  Code := FParser.GetNext(Lexem);
  Check(Code <> lcEnd, cInvalidBoolean);

  Result := SameText(Lexem, cTrue);
  Check(Result or SameText(Lexem, cFalse), cInvalidBoolean);
end;

function TJSONTextReader.ReadOid: TJSONOid;
begin
  Code := FParser.GetNext(Lexem);
  Check(Code = lcString, cInvalidObjectId);

  Result := StringToOid(UnQuote(Lexem));

  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidObjectId);
end;

procedure TJSONTextReader.ReadUndefined;
begin
  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and SameText(Lexem, cTrue), cInvalidUndefined);

  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidUndefined);
end;

function TJSONTextReader.ReadInt32: integer;
begin
  Result := 0;
  Assert(False);
end;

function TJSONTextReader.ReadInt64: Int64;
begin
  Code := FParser.GetNext(Lexem);
  Check(Code = lcString, cInvalidInt64);

  if not TryStrToInt64(UnQuote(Lexem), Result) then
    raise JSONException.Create(cInvalidInt64);

  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidInt64);
end;

function TJSONTextReader.ReadDateTime: TDateTime;
{$IFNDEF USE_TFORMATSETTINGS}
var
  OldDateSeparator, OldTimeSeparator: char;
  OldDateFormat, OldTimeFormat: string;
{$ENDIF}

  procedure SetSeparators;
  begin
  {$IFNDEF USE_TFORMATSETTINGS}
    OldDateSeparator := DateSeparator;
    OldTimeSeparator := TimeSeparator;
    OldDateFormat := ShortDateFormat;
    OldTimeFormat := LongTimeFormat;
    DateSeparator := '-';
    TimeSeparator := ':';
    ShortDateFormat := 'yyyy-mm-dd';
    ShortTimeFormat := 'h:mm:ss.zzz';
  {$ENDIF}
  end;

{$IFNDEF USE_TFORMATSETTINGS}
  procedure RestoreSeparators;
  begin
    DateSeparator := OldDateSeparator;
    TimeSeparator := OldTimeSeparator;
    ShortDateFormat := OldDateFormat;
    LongTimeFormat := OldTimeFormat;
  end;
{$ENDIF}

begin
  Code := FParser.GetNext(Lexem);
  Check(Code = lcString, cInvalidDateTime);

{$IFDEF USE_TFORMATSETTINGS}
  if not TryStrToDateTime(UnQuote(Lexem), Result, DateTimeFormat) then
    raise JSONException.Create(cInvalidDateTime);
{$ELSE}
  SetSeparators;
  try
    if not TryStrToDateTime(UnQuote(Lexem), Result) then
      raise JSONException.Create(cInvalidDateTime);
  finally
    RestoreSeparators;
  end;
{$ENDIF}

  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidDateTime);
end;

function TJSONTextReader.ReadTimestampTime: Cardinal;
begin
  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '{'), cInvalidTimeStamp);
  Code := FParser.GetNext(Lexem);
  Check((Code = lcString) and SameText(UnQuote(Lexem), cJSONTime), cInvalidTimeStamp);
  Check(ReadValueSeparator, cInvalidTimeStamp);
  Code := FParser.GetNext(Lexem);
  Check(Code = lcNumber, cInvalidTimeStamp);

  if not TryStrToInt(UnQuote(Lexem), integer(Result)) then
    raise JSONException.Create(cInvalidTimeStamp);
end;

function TJSONTextReader.ReadTimestampIncrement: Cardinal;
begin
  Code := FParser.GetNext(Lexem);
  Check((Code = lcString) and SameText(UnQuote(Lexem), cJSONIncrement), cInvalidTimeStamp);
  Check(ReadValueSeparator, cInvalidTimeStamp);
  Code := FParser.GetNext(Lexem);
  Check(Code = lcNumber, cInvalidTimeStamp);

  if not TryStrToInt(UnQuote(Lexem), integer(Result)) then
    raise JSONException.Create(cInvalidTimeStamp);

  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidTimeStamp);
  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidTimeStamp);
end;

procedure TJSONTextReader.ReadMinMaxKey;
begin
  Code := FParser.GetNext(Lexem);
  Check((Code = lcNumber) and (Lexem = '1'), cInvalidKey);
  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidKey);
end;

function TJSONTextReader.ReadDBPointerId: TJSONOid;
begin
  Code := FParser.GetNext(Lexem);
  Check((Code = lcString) and SameText(UnQuote(Lexem), cJSONId), cInvalidDBPointer);
  Check(ReadValueSeparator, cInvalidDBPointer);

  Result := ReadOid;
end;

procedure TJSONTextReader.ReadRegexOptionsBegin;
begin
  InternalReadLexem;
  Check((Code = lcString) and SameText(UnQuote(Lexem), cJSONOptions), cInvalidRegex);
  Check(ReadValueSeparator, cInvalidRegex);
end;

procedure TJSONTextReader.ReadRegexOptionsEnd;
begin
  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidRegex);
end;

procedure TJSONTextReader.ReadJavaCodeBegin;
begin
  // do nothing
end;

function TJSONTextReader.ReadJavaCode(out HasScope: boolean; out Size: integer): string;
var
  Tag: TJSONTag;
begin
  HasScope := False;
  Check(ReadValueSeparator, cInvalidJavaCode);

  Result := InternalReadString(True);
  Size := Length(Result);

  Tag := ReadTag;
  if Tag = jtComma then begin
    Code := FParser.GetNext(Lexem);
    if (Code = lcString) and SameText(UnQuote(Lexem), cJSONScope) then begin
      HasScope := True;
      Check(ReadValueSeparator, cInvalidJavaCode);
    end
    else
      raise JSONException.Create(cInvalidJavaCode);
  end
  else
    Check(Tag = jtObjectEnd, cInvalidJavaCode);
end;

procedure TJSONTextReader.ReadJavaScopeEnd;
begin
  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidJavaCode);
end;

function TJSONTextReader.ReadBinary(var Data: TBytes; out Subtype: integer): integer;
begin
  Code := FParser.GetNext(Lexem);
  Check(Code = lcString, cInvalidBinary);

  Data := TBase64.Decode(Encoding.ASCII.GetBytes(AnsiString(Lexem)));
  Result := Length(Data);

  Check(ReadElementSeparator, cInvalidBinary);
  Code := FParser.GetNext(Lexem);
  Check((Code = lcString) and SameText(UnQuote(Lexem), cJSONType), cInvalidBinary);
  Check(ReadValueSeparator, cInvalidBinary);
  Code := FParser.GetNext(Lexem);
  Check(Code = lcString, cInvalidBinary);

  if not TryStrToInt(UnQuote(Lexem), integer(Subtype)) then
    raise JSONException.Create(cInvalidBinary);

  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidBinary);
end;

function TJSONTextReader.ReadDouble: double;
begin
  Result := 0;
  Assert(False);
end;

procedure TJSONTextReader.ReadDecimal128(var Value: TDecimal128);
begin
  Value := TStringDecimal128(Copy(InternalReadString(True), 1, cJSONDecimal128Length));

  Code := FParser.GetNext(Lexem);
  Check((Code <> lcEnd) and (Lexem = '}'), cInvalidDecimal128);
end;

procedure TJSONTextReader.Skip;
var
  NestedObjectCount: integer;
begin
  NestedObjectCount := 0;
  if FTokenType in [jtObject, jtArray] then
    Inc(NestedObjectCount);

  while Code <> lcEnd do begin
    Code := FParser.GetNext(Lexem);
    if Lexem = '{' then
      Inc(NestedObjectCount)
    else
    if Code = lxLeftSqBracket then
      Inc(NestedObjectCount)
    else
    if Lexem = '}' then begin
      FTokenType := jtObjectEnd;
      Dec(NestedObjectCount);
      if NestedObjectCount <= 0 then begin
        FParser.Back;
        Exit;
      end;
    end
    else
    if Code = lxRightSqBracket then begin
      FTokenType := jtArrayEnd;
      Dec(NestedObjectCount);
      if NestedObjectCount <= 0 then
        Exit;
    end
    else
    if Code = lxComma then begin
      FTokenType := jtComma;
      if NestedObjectCount <= 0 then begin
        FParser.Back;
        Exit;
      end;
    end;
  end;
end;

procedure TJSONTextReader.SetText(const Text: string);
begin
  SetBytes(Encoding.UTF8.GetBytes(Text));
end;

procedure TJSONTextReader.SetBytes(const Binary: TBytes);
begin
  if FStreamOwner then
    FreeAndNil(FStream);

  FStream := TMemoryStream.Create;
  FStreamOwner := True;
  FStreamSize := -1;

  if Length(Binary) > 0 then begin
    FStream.Write(Binary[0], Length(Binary));
    FStream.Position := 0;
  end;
end;

{ TJSONStreamReader }

constructor TJSONStreamReader.Create;
begin
  inherited Create;

  FStream := nil;
  FStreamOwner := True;
  FStreamSize := -1;
end;

destructor TJSONStreamReader.Destroy;
begin
  if FStreamOwner then
    FStream.Free;

  inherited;
end;

procedure TJSONStreamReader.SetStream(const Stream: TStream; Size: Int64 = -1);
begin
  if FStream <> Stream then
    if FStreamOwner then
      FreeAndNil(FStream);

  FStream := Stream;
  FStartStreamPos := FStream.Position;
  FStreamOwner := False;
  FStreamSize := Size;
end;

{ TJSONBinaryReader }

function TJSONBinaryReader.InternalReadByte: byte;
begin
  FStream.Read(Result, 1);
end;

function TJSONBinaryReader.InternalReadInt32: integer;
begin
  FStream.Read(Result, 4);
end;

function TJSONBinaryReader.InternalReadInt64: Int64;
begin
  FStream.Read(Result, 8);
end;

procedure TJSONBinaryReader.InternalReadBytes(var Value: TBytes; const ZeroTerminated: boolean);
var
  BufferLen,
  BytesTotal,
  BytesRead: integer;
begin
  BytesTotal := 0;

  if not ZeroTerminated then
    BufferLen := InternalReadInt32
  else
    BufferLen := BufferSize;

  SetLength(Value, BufferLen);

  while True do begin
    if not ZeroTerminated then
      BytesRead := FStream.Read(Value[0], BufferLen)
    else
      BytesRead := FStream.Read(Value[BytesTotal], 1);

    if (BytesRead = 0) or (Value[BytesTotal] = 0) then
      Break;

    BytesTotal := BytesTotal + BytesRead;
    if ZeroTerminated then begin
      if BytesTotal = BufferLen then begin
        Inc(BufferLen, BufferSize);
        SetLength(Value, BufferLen);
      end;
    end
    else
      Break;
  end;

  if ZeroTerminated then
    SetLength(Value, BytesTotal);
end;

procedure TJSONBinaryReader.InternalReadBuffer(var Buffer; const Size: NativeInt);
begin
  FStream.Read(Buffer, Size);
end;

procedure TJSONBinaryReader.Initialize;
begin
  inherited;

  FTokenType := jtNone;
  FInProcess := False;

  FAllowedArrayTerminators := [jtArrayEnd, jtObjectEnd];
  FRestrictedArrayTerminators := [jtNone];
end;

procedure TJSONBinaryReader.Finalize;
begin
  // do nothing
end;

function TJSONBinaryReader.GetCurPos: Int64;
begin
  Result := FStream.Position;
end;

function TJSONBinaryReader.ReadTag: TJSONTag;
var
  Tag: TJSONBinaryType;
begin
  if not FInProcess then begin
    FTokenType := jtObject;
    Result := FTokenType;
    FInProcess := True;
    Exit;
  end;

  FTagPos := GetCurPos;

  FTokenType := jtNone;
  Tag := TJSONBinaryType(InternalReadByte);

  case Tag of
    btDocumentEnd: FTokenType := jtObjectEnd;
    btDouble: FTokenType := jtDouble;
    btString: FTokenType := jtString;
    btObject: FTokenType := jtObject;
    btArray: FTokenType := jtArray;
    btBinary: FTokenType := jtBinary;
    btUndefined: FTokenType := jtUndefined;
    btObjectId: FTokenType := jtObjectId;
    btBoolean: FTokenType := jtBoolean;
    btDateTime: FTokenType := jtDateTime;
    btNull: FTokenType := jtNull;
    btRegex: FTokenType := jtRegex;
    btJavaCode: FTokenType := jtJavaCode;
    btJavaScopeCode: FTokenType := jtJavaScopeCode;
    btInt32: FTokenType := jtInt32;
    btTimeStamp: FTokenType := jtTimestamp;
    btInt64: FTokenType := jtInt64;
    btMinKey: FTokenType := jtMinKey;
    btMaxKey: FTokenType := jtMaxKey;
    btDBPointer: FTokenType := jtDBPointer;
    btDecimal128: FTokenType := jtDecimal128;
    btSymbol: ;
  else
    Assert(False);
  end;

  Result := FTokenType;
end;

function TJSONBinaryReader.ReadExtTag(const Tag: TJSONTag): TJSONTag;
begin
  Result := Tag;
end;

function TJSONBinaryReader.ReadObjectSize: integer;
begin
  Result := InternalReadInt32;
end;

function TJSONBinaryReader.ReadArraySize: integer;
begin
  Result := InternalReadInt32;
end;

function TJSONBinaryReader.ReadElementName: string;
var
  Buffer: TBytes;
begin
  Result := '';
  InternalReadBytes(Buffer, True);
end;

function TJSONBinaryReader.ReadAnsiString(const ZeroTerminated, Unescape: boolean; out Size: integer): AnsiString;
var
  Buffer: TBytes;
begin
  InternalReadBytes(Buffer, ZeroTerminated);
  Size := Length(Buffer);
  if not ZeroTerminated then
    Dec(Size);

  if Size > 0 then
    Result := {$IFNDEF NEXTGEN}Encoding.UTF8.GetAnsiString(Buffer, 0, Size){$ELSE}AnsiString(Encoding.UTF8.GetString(Buffer, 0, Size)){$ENDIF}
  else
    Result := '';
end;

function TJSONBinaryReader.ReadWideString(const ZeroTerminated, Unescape: boolean; out Size: integer): WideString;
var
  Buffer: TBytes;
begin
  InternalReadBytes(Buffer, ZeroTerminated);
  Size := Length(Buffer);
  if not ZeroTerminated then
    Dec(Size);

  if Size > 0 then
    Result := {$IFNDEF NEXTGEN}Encoding.UTF8.GetWideString(Buffer, 0, Size){$ELSE}WideString(Encoding.UTF8.GetString(Buffer, 0, Size)){$ENDIF}
  else
    Result := '';
end;

function TJSONBinaryReader.ReadValueSeparator: boolean;
begin
  Result := True;
end;

function TJSONBinaryReader.ReadElementSeparator: boolean;
begin
  Result := True;
end;

function TJSONBinaryReader.ReadBoolean: boolean;
begin
  Result := InternalReadByte = 1;
end;

function TJSONBinaryReader.ReadOid: TJSONOid;
begin
  InternalReadBuffer(Result[0], 12);
end;

procedure TJSONBinaryReader.ReadUndefined;
begin
  // do nothing
end;

function TJSONBinaryReader.ReadInt32: integer;
begin
  Result := InternalReadInt32;
end;

function TJSONBinaryReader.ReadInt64: Int64;
begin
  Result := InternalReadInt64;
end;

function TJSONBinaryReader.ReadDateTime: TDateTime;
var
  i64: Int64;
begin
  i64 := InternalReadInt64;
  Result := UnixToDateTime(trunc(i64/MSecsPerSec));
end;

function TJSONBinaryReader.ReadTimestampTime: Cardinal;
begin
  Result := Cardinal(InternalReadInt32);
end;

function TJSONBinaryReader.ReadTimestampIncrement: Cardinal;
begin
  Result := Cardinal(InternalReadInt32);
end;

procedure TJSONBinaryReader.ReadMinMaxKey;
begin
  // do nothing
end;

function TJSONBinaryReader.ReadDBPointerId: TJSONOid;
begin
  InternalReadBuffer(Result[0], cJSONOidSize);
end;

procedure TJSONBinaryReader.ReadRegexOptionsBegin;
begin
  // do nothing
end;

procedure TJSONBinaryReader.ReadRegexOptionsEnd;
begin
  // do nothing
end;

procedure TJSONBinaryReader.ReadJavaCodeBegin;
begin
  InternalReadInt32;
end;

function TJSONBinaryReader.ReadJavaCode(out HasScope: boolean; out Size: integer): string;
begin
  HasScope := False;
{$IFDEF VER12P}
  Result := ReadWideString(False, False, Size);
{$ELSE}
  Result := ReadAnsiString(False, False, Size);
{$ENDIF}
end;

function TJSONBinaryReader.ReadBinary(var Data: TBytes; out Subtype: integer): integer;
begin
  Result := InternalReadInt32;
  Subtype := integer(InternalReadByte);

  SetLength(Data, Result);
  InternalReadBuffer(Data[0], Result);
end;

function TJSONBinaryReader.ReadDouble: double;
var
  i64: Int64;
begin
  i64 := InternalReadInt64;
  Result := Double((@i64)^);
end;

procedure TJSONBinaryReader.ReadDecimal128(var Value: TDecimal128);
var
  b: TBinaryDecimal128;
begin
  InternalReadBuffer((@b)^, cJSONDecimal128Size);
  Value := TStringDecimal128(Decimal128ToString(@b));
end;

{ TJSONDeserializer }

constructor TJSONDeserializer.Create;
begin
  inherited;

  FReader := nil;
end;

destructor TJSONDeserializer.Destroy;
begin
  FreeReader;

  inherited;
end;

procedure TJSONDeserializer.InitReader(const ReaderClass: TJSONReaderClass);
begin
  if (FReader = nil) or not IsClass(FReader, ReaderClass) then begin
    FreeReader;
    FReader := ReaderClass.Create;
    FReaderOwner := True;
  end;
end;

procedure TJSONDeserializer.FreeReader;
begin
  if FReaderOwner then
    FreeAndNil(FReader);
end;

function TJSONDeserializer.CreateString(const Parent: TJSONValue): TJSONString;
begin
  if FUseUnicode then
    Result := TJSONWideString.Create(Parent)
  else
    Result := TJSONAnsiString.Create(Parent);
end;

class function TJSONDeserializer.GetTextReaderClass: TJSONReaderClass;
begin
  Result := TJSONTextReader;
end;

function TJSONDeserializer.Process: TJSONValue;
var
  Tag: TJSONTag;
begin
  Result := nil;

  if FReaderOwner then
    FReader.Initialize;
  try
    Tag := FReader.ReadTag;
    if Tag <> jtNone then
      Result := ProcessTag(Tag, nil);
  finally
    if FReaderOwner then
      FReader.Finalize;
  end;
end;

function TJSONDeserializer.ProcessTag(const Tag: TJSONTag; const Parent: TJSONValue): TJSONValue;
begin
  Result := nil;

  case Tag of
    jtObject: Result := ProcessObject(Parent);
    jtArray: Result := ProcessArray(Parent);
    jtString: Result := ProcessString(Parent, True);
    jtNumber: Result := ProcessNumber(Parent);
    jtBoolean: Result := ProcessBoolean(Parent);
    jtNull: Result := ProcessNull(Parent);
    jtObjectId: Result := ProcessObjectId(Parent);
    jtInt32: Result := ProcessInt32(Parent);
    jtInt64: Result := ProcessInt64(Parent);
    jtDateTime: Result := ProcessDateTime(Parent);
    jtJavaCode: Result := ProcessJavaCode(Parent);
    jtJavaScopeCode: Result := ProcessJavaScopeCode(Parent);
    jtUndefined: Result := ProcessUndefined(Parent);
    jtRegex: Result := ProcessRegex(Parent);
    jtTimestamp: Result := ProcessTimestamp(Parent);
    jtBinary: Result := ProcessBinary(Parent);
    jtDouble: Result := ProcessDouble(Parent);
    jtMinKey: Result := ProcessMinKey(Parent);
    jtMaxKey: Result := ProcessMaxKey(Parent);
    jtDBPointer: Result := ProcessDBPointer(Parent);
    jtDecimal128: Result := ProcessDecimal128(Parent);
  end;
end;

function TJSONDeserializer.ProcessObject(const Parent: TJSONValue): TJSONObject;
var
  Pair: TJSONPair;
  Tag: TJSONTag;
begin
  Result := TJSONObject.Create(Parent);
  try
    Result.FStream := FReader.FStream;
    Result.FStartPos := FReader.GetTagPos;

    Result.FData := FData;
    Result.Size := FReader.ReadObjectSize;

    while True do begin
      Tag := FReader.ReadTag;

      if Tag in [jtNone, jtArrayEnd] then
        raise JSONException.Create(cInvalidObject);
      if Tag = jtObjectEnd then
        Break;
      if Tag = jtComma then
        Continue;

      Pair := ProcessPair(Result, Tag);
      if Pair <> nil then begin
        if Pair.Value <> nil then
          Result.AddPair(Pair)
        else
          raise JSONException.Create(cInvalidValue);
      end
      else
        Break;
    end;

    Result.FEndPos := FReader.GetCurPos;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessArray(const Parent: TJSONValue): TJSONArray;
var
  Tag: TJSONTag;
begin
  Result := TJSONArray.Create(Parent);
  try
    Result.FStream := FReader.FStream;
    Result.FStartPos := FReader.GetTagPos;

    Result.Size := FReader.ReadArraySize;

    while True do begin
      Tag := FReader.ReadTag;

      if Tag in FReader.FRestrictedArrayTerminators then
        raise JSONException.Create(cInvalidArray);
      if Tag in FReader.FAllowedArrayTerminators then
        Break;
      if Tag = jtComma then
        Continue;

      FReader.ReadElementName;
      Result.AddElement(ProcessTag(Tag, Result));
    end;

    Result.FEndPos := FReader.GetCurPos;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessNumber(const Parent: TJSONValue): TJSONNumber;
var
  Value: string;
  Subtype: TJSONTag;
begin
  Result := nil;

  Value := FReader.ReadNumber(Subtype);
  if Value = '' then
    Exit;

  Result := TJSONNumber.Create(Parent, Value, Subtype);
end;

function TJSONDeserializer.ProcessString(const Parent: TJSONValue; const Unescape: boolean): TJSONString;
var
  Size: integer;
begin
  Result := CreateString(Parent);
  try
    if FUseUnicode then
      Result.AsWideString := FReader.ReadWideString(False, Unescape, Size)
    else
      Result.AsAnsiString := FReader.ReadAnsiString(False, Unescape, Size);
    Result.Size := Size;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessBoolean(const Parent: TJSONValue): TJSONBoolean;
begin
  Result := TJSONBoolean.Create(Parent);
  try
    Result.Value := FReader.ReadBoolean;
    Result.Size := 1;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessNull(const Parent: TJSONValue): TJSONNull;
begin
  Result := TJSONNull.Create(Parent);
end;

function TJSONDeserializer.ProcessUndefined(const Parent: TJSONValue): TJSONUndefined;
begin
  Result := TJSONUndefined.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidUndefined);

    FReader.ReadUndefined;
    Result.Size := 0;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessObjectId(const Parent: TJSONValue): TJSONObjectId;
begin
  Result := TJSONObjectId.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidObjectId);

    Result.SetValue(FReader.ReadOid);
    Result.Size := 24;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessInt32(const Parent: TJSONValue): TJSONInt32;
begin
  Result := TJSONInt32.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidInt32);

    Result.Value := FReader.ReadInt32;
    Result.Size := 4;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessInt64(const Parent: TJSONValue): TJSONInt64;
begin
  Result := TJSONInt64.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidInt64);

    Result.Value := FReader.ReadInt64;
    Result.Size := 8;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessDateTime(const Parent: TJSONValue): TJSONDateTime;
begin
  Result := TJSONDateTime.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidDateTime);

    Result.Value := FReader.ReadDateTime;
    Result.Size := 8;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessTimestamp(const Parent: TJSONValue): TJSONTimestamp;
begin
  Result := TJSONTimestamp.Create(Parent);
  try
    Result.FStream := FReader.FStream;
    Result.FStartPos := FReader.GetTagPos;

    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidTimeStamp);

    Result.Timestamp := TJSONInt32.Create(Result);
    Result.Timestamp.Value := FReader.ReadTimestampTime;

    if not FReader.ReadElementSeparator then
      raise JSONException.Create(cInvalidTimeStamp);

    Result.Increment := TJSONInt32.Create(Result);
    Result.Increment.Value := FReader.ReadTimestampIncrement;
    Result.Size := 8;

    Result.FEndPos := FReader.GetCurPos;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessMinKey(const Parent: TJSONValue): TJSONMinKey;
begin
  Result := TJSONMinKey.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidKey);

    FReader.ReadMinMaxKey;
    Result.Size := 0;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessMaxKey(const Parent: TJSONValue): TJSONMaxKey;
begin
  Result := TJSONMaxKey.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidKey);

    FReader.ReadMinMaxKey;
    Result.Size := 0;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessDBPointer(const Parent: TJSONValue): TJSONDBPointer;
begin
  Result := TJSONDBPointer.Create(Parent);
  try
    Result.FStream := FReader.FStream;
    Result.FStartPos := FReader.GetTagPos;

    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidDBPointer);

    Result.Name := ProcessString(Result, False);

    if not FReader.ReadElementSeparator then
      raise JSONException.Create(cInvalidDBPointer);

    Result.SetValue(TJSONObjectId.Create(Result));
    Result.FValue.FValue := FReader.ReadDBPointerId;
    Result.FValue.Size := 24;

    Result.FEndPos := FReader.GetCurPos;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessJavaCode(const Parent: TJSONValue): TJSONJavaCode;
var
  Code: string;
  HasScope: boolean;
  Size: integer;
begin
  Code := FReader.ReadJavaCode(HasScope, Size);

  if not HasScope then begin
    Result := TJSONJavaCode.Create(Parent);
    try
      Result.Code := CreateString(Result);
      Result.Code.Value := Code;
      Result.Code.Size := Size;
    except
      Result.Free;
      raise;
    end;
  end
  else begin
    // for ExtJSON only
    Result := TJSONJavaScopeCode.Create(Parent);
    try
      Result.Code := CreateString(Result);
      Result.Code.Value := Code;
      Result.Code.Size := Size;
      TJSONJavaScopeCode(Result).Scope := ProcessObject(Result);
      FReader.ReadJavaScopeEnd;
    except
      Result.Free;
      raise;
    end;
  end;
end;

function TJSONDeserializer.ProcessJavaScopeCode(const Parent: TJSONValue): TJSONJavaScopeCode;
var
  Code: string;
  HasScope: boolean;
  Size: integer;
begin
  // for BSON only
  Result := TJSONJavaScopeCode.Create(Parent);
  try
    FReader.ReadJavaCodeBegin;
    Code := FReader.ReadJavaCode(HasScope, Size);
    Result.Code := CreateString(Result);
    Result.Code.Value := Code;
    Result.Code.Size := Size;
    TJSONJavaScopeCode(Result).Scope := ProcessObject(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessBinary(const Parent: TJSONValue): TJSONBinary;
begin
  Result := TJSONBinary.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidBinary);

    Result.Subtype := TJSONInt32.Create(Result);
    Result.Binary := TJSONBytes.Create(Result);
    Result.Binary.Size := FReader.ReadBinary(Result.Binary.FValue, Result.Subtype.FValue);
    Result.Subtype.Size := 4;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessDouble(const Parent: TJSONValue): TJSONDouble;
begin
  Result := TJSONDouble.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidDouble);

    Result.Value := FReader.ReadDouble;
    Result.Size := 8;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessDecimal128(const Parent: TJSONValue): TJSONDecimal128;
var
  d: TDecimal128;
begin
  Result := TJSONDecimal128.Create(Parent);
  try
    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidDecimal128);

    FReader.ReadDecimal128(d);
    Result.SetValue(string(d));
    Result.Size := cJSONDecimal128Size;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessCString(const Parent: TJSONValue; const Unescape: boolean): TJSONString;
var
  Size: integer;
begin
  Result := CreateString(Parent);
  try
    if FUseUnicode then
      Result.AsWideString := FReader.ReadWideString(True, Unescape, Size)
    else
      Result.AsAnsiString := FReader.ReadAnsiString(True, Unescape, Size);
    Result.Size := Size;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessPair(const Parent: TJSONObject; const Tag: TJSONTag): TJSONPair;
var
  ValueTag: TJSONTag;
begin
  Result := TJSONPair.Create(Parent);
  try
    Result.FStream := FReader.FStream;
    Result.FStartPos := FReader.GetTagPos;

    Result.Name := ProcessCString(Parent, False);

    if Result.Name.AsString = '' then
      raise JSONException.Create(cInvalidValue);

    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidObject);

    ValueTag := FReader.ReadExtTag(Tag);
    if ValueTag = jtNone then
      Exit;

    Result.Value := ProcessTag(ValueTag, Result);

    if Result.Value = nil then
      raise JSONException.Create(cInvalidValue);

    Result.FEndPos := FReader.GetCurPos;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.ProcessRegex(const Parent: TJSONValue): TJSONRegex;
begin
  Result := TJSONRegex.Create(Parent);
  try
    Result.FStream := FReader.FStream;
    Result.FStartPos := FReader.GetTagPos;

    if not FReader.ReadValueSeparator then
      raise JSONException.Create(cInvalidRegex);

    Result.Pattern := ProcessCString(Result, True);

    if not FReader.ReadElementSeparator then
      raise JSONException.Create(cInvalidRegex);

    FReader.ReadRegexOptionsBegin;
    Result.Options := ProcessCString(Result, True);
    FReader.ReadRegexOptionsEnd;

    Result.FEndPos := FReader.GetCurPos;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONDeserializer.FromText(const Text: string; Data: IntPtr = nil; const UseUnicode: boolean = True): TJSONValue;
begin
  FData := Data;
  FUseUnicode := UseUnicode;

  InitReader(GetTextReaderClass);
  TJSONTextReader(FReader).SetText(Text);

  Result := Process;
end;

function TJSONDeserializer.FromText(Stream: TStream; Data: IntPtr = nil; const UseUnicode: boolean = True): TJSONValue;
begin
  FData := Data;
  FUseUnicode := UseUnicode;

  InitReader(GetTextReaderClass);
  TJSONTextReader(FReader).SetStream(Stream);

  Result := Process;
end;

function TJSONDeserializer.FromBinary(Stream: TStream; Data: IntPtr = nil; const UseUnicode: boolean = True): TJSONValue;
begin
  FData := Data;
  FUseUnicode := UseUnicode;

  InitReader(TJSONBinaryReader);
  TJSONBinaryReader(FReader).SetStream(Stream);

  Result := Process;
end;

function TJSONDeserializer.FromJSONReader(JSONReader: TJSONStreamReader; Data: IntPtr = nil;
  const UseUnicode: boolean = True): TJSONValue;
begin
  FData := Data;
  FUseUnicode := UseUnicode;

  FReader := JSONReader;
  FReaderOwner := False;

  Result := Process;
end;

{$IFDEF USE_TFORMATSETTINGS}

initialization
  DateTimeFormat := FormatSettings;
  DateTimeFormat.DateSeparator := '-';
  DateTimeFormat.TimeSeparator := ':';
  DateTimeFormat.ShortDateFormat := 'yyyy-mm-dd';
  DateTimeFormat.LongTimeFormat := {$IFNDEF FPC}'h:mm:ss.zzz'{$ELSE}'h:mm:ss'{$ENDIF};

{$ENDIF}
  
end.
