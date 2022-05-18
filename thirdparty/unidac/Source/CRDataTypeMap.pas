
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Core Data Type Mapping
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRDataTypeMap;

{$IFDEF LITE}
  Error // Data Type Mapping is not supported in dbExpress drivers
{$ENDIF}

{$IFDEF VER12P}
  {$DEFINE USE_UINT64}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE USE_UINT64}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRTimeStamp, DAConsts, MemUtils, MemData;

const
  MaxSingle = 3.4e+38;
  {$EXTERNALSYM MaxSingle}
  MaxDouble = 1.7e+308;
  {$EXTERNALSYM MaxDouble}
  DecValue: array[0..3] of Cardinal = (10000, 1000, 100, 10);

type
  TMapRules = class;
  TCRMapRules = class;
  TConverterManager = class;

  TMapRuleClass = class of TMapRule;
  TCRMapRuleClass = class of TCRMapRule;
  TCRMapRulesClass = class of TCRMapRules;
  TConverterManagerClass = class of TConverterManager;

  TCalcSizeFunc = function(Value: Integer): Integer of object;

  EDataTypeMappingError = class(Exception)
  public
    constructor Create; overload;
  end;

  EInvalidMapRuleExpression = class (EDataTypeMappingError)
  end;

  EUnsupportedDataTypeMapping = class (EDataTypeMappingError)
  end;

  EInvalidDBTypeMapping = class (EDataTypeMappingError)
  end;

  EInvalidFieldTypeMapping = class (EDataTypeMappingError)
  end;

  EDataMappingError = class (EDataTypeMappingError)
  end;

  TDBTypeInfo = class
  private
    FDBType: Word;
    FName: string;
    FLength: boolean;
    FScale: boolean;
    function GetDBProvider: Word;
  protected
  public
    constructor Create(DBType: Word; const Name: string; Length, Scale: boolean);

    property DBType: Word read FDBType;
    property DBProvider: Word read GetDBProvider;
    property Name: string read FName;
    property Length: boolean read FLength;
    property Scale: boolean read FScale;
  end;

  TDBTypeInfos = class
  private
    FTypeInfos: TCRObjectList;

    function GetCount: Integer;
    function GetTypeInfo(Index: integer): TDBTypeInfo;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(DBType: Word; const Name: string; Length, Scale: boolean);
    procedure Delete(Index: Integer);
    procedure Clear;

    function Check(DBType: Word; LengthMin, LengthMax, ScaleMin, ScaleMax: Integer; IsDBTypeRequired: boolean): Exception;

    function FindTypeInfo(DBType: Word): TDBTypeInfo; overload;
    function FindTypeInfo(const Name: string; DBProvider: Word): TDBTypeInfo; overload;

    property TypeInfos[Index: Integer]: TDBTypeInfo read GetTypeInfo; default;
    property Count: Integer read GetCount;
  end;

  TMapRule = class (TCollectionItem)
  private
    // Field Name
    FFieldName: string;

    // Source Database Type
    FDBType: Word;
    FDBLengthMin: Integer;
    FDBLengthMax: Integer;
    FDBScaleMin: Integer;
    FDBScaleMax: Integer;

    // Field Data Type
    FFieldLength: Integer;
    FFieldScale: Integer;

    // String format of date, time, float, etc.
    FFormat: string;

    // Ignore conversion errors
    FIgnoreErrors: Boolean;

    function GetDBProvider: Word;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToRule(Dest: TMapRule);
  public
    constructor Create(Owner: TCollection); override;

    property FieldName: string read FFieldName write FFieldName;

    property DBType: Word read FDBType write FDBType;
    property DBProvider: Word read GetDBProvider;
    property DBLengthMin: Integer read FDBLengthMin write FDBLengthMin;
    property DBLengthMax: Integer read FDBLengthMax write FDBLengthMax;
    property DBScaleMin: Integer read FDBScaleMin write FDBScaleMin;
    property DBScaleMax: Integer read FDBScaleMax write FDBScaleMax;

    property FieldLength: Integer read FFieldLength write FFieldLength;
    property FieldScale: Integer read FFieldScale write FFieldScale;

    property Format: string read FFormat write FFormat;
    property IgnoreErrors: Boolean read FIgnoreErrors write FIgnoreErrors;
  end;

  TMapRules = class (TCollection)
  private
    function GetItem(Index: Integer): TMapRule;
    procedure SetItem(Index: Integer; Value: TMapRule);
  protected
  public
    property Items[Index: Integer]: TMapRule read GetItem write SetItem; default;
  end;

  TCRMapRule = class (TMapRule)
  private
    FDataType: Word;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToRule(Dest: TCRMapRule);
  public
    constructor Create(Owner: TCollection); override;

    property DataType: Word read FDataType write FDataType;
  end;

  TCRMapRules = class (TMapRules)
  private
    FEnabled: boolean;
    FIgnoreInvalidRules: boolean;

    function GetItem(Index: Integer): TCRMapRule;
    procedure SetItem(Index: Integer; Value: TCRMapRule);
  protected
    class function GetMapRuleClass: TCRMapRuleClass; virtual;

    function CheckSQLType(Rule: TMapRule; DBType, DBLength, DBScale: integer): boolean;
    function CheckFormat(Rule: TCRMapRule; const Format: string): string;
  public
    constructor Create; virtual;

    class function GetConverterManager: TConverterManager; virtual;

    function AddRule(const FieldName: string;
      DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      DataType: Word; FieldLength, FieldScale: Integer;
      IgnoreErrors: boolean; const Format: string): TCRMapRule;
    function DetectFieldNameMapRule(const FieldName: string; DBType, DBLength, DBScale: integer): TCRMapRule;
    function DetectDBTypeMapRule(DBType, DBLength, DBScale: integer): TCRMapRule;

    property Enabled: boolean read FEnabled write FEnabled;
    property IgnoreInvalidRules: boolean read FIgnoreInvalidRules write FIgnoreInvalidRules;
    property Items[Index: Integer]: TCRMapRule read GetItem write SetItem; default;
  end;

  TFetchConverter = class
  private
    FDBType: Word;
    FDBLengthMin: Integer;
    FDBLengthMax: Integer;
    FDBScaleMin: Integer;
    FDBScaleMax: Integer;

    FDestDataType: Word;
    FInternalDataType: Word;

    FCalcLength: TCalcSizeFunc;
    FCalcScale: TCalcSizeFunc;
  public
    constructor Create(DBType: Word;
      DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
      DestDataType, InternalDataType: Word;
      CalcLength: TCalcSizeFunc; CalcScale: TCalcSizeFunc);

    property DBType: Word read FDBType;
    property DBLengthMin: Integer read FDBLengthMin;
    property DBLengthMax: Integer read FDBLengthMax;
    property DBScaleMin: Integer read FDBScaleMin;
    property DBScaleMax: Integer read FDBScaleMax;

    property DestDataType: Word read FDestDataType;
    property InternalDataType: Word read FInternalDataType;

    property CalcLength: TCalcSizeFunc read FCalcLength;
    property CalcScale: TCalcSizeFunc read FCalcScale;
  end;

  TOnDemandConverter = class
  private
    FSourceDataType: Word;
    FDestDataType: Word;

    FCalcLength: TCalcSizeFunc;
    FCalcScale: TCalcSizeFunc;

    FGetDataConverter: TConvertProcedure;
    FPutDataConverter: TConvertProcedure;
  public
    constructor Create(SourceDataType, DestDataType: Word;
      GetDataConverter, PutDataConverter: TConvertProcedure;
      CalcLength: TCalcSizeFunc; CalcScale: TCalcSizeFunc);

    function Clone: TOnDemandConverter;

    property SourceDataType: Word read FSourceDataType write FSourceDataType;
    property DestDataType: Word read FDestDataType write FDestDataType;

    property GetDataConverter: TConvertProcedure read FGetDataConverter;
    property PutDataConverter: TConvertProcedure read FPutDataConverter;

    property CalcLength: TCalcSizeFunc read FCalcLength;
    property CalcScale: TCalcSizeFunc read FCalcScale;
  end;

  TConverterDictionaryItem = record
    HashCode: Integer;
    Value: TObject;
  end;

  TConverterDictionaryItemArray = array of TConverterDictionaryItem;

  TConverterDictionary = class
  private
    FItems: TConverterDictionaryItemArray;
    FCount: Integer;
    FGrowThreshold: Integer;

    function GetRehashBucketIndex(const HashCode: integer): integer;
    function Hash(Value1, Value2: Word): integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetCapacity(const Capacity: integer);
    procedure Grow;
    procedure Rehash(NewCapacity: integer); virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(Capacity: integer); overload; virtual;
    destructor Destroy; override;

    procedure Clear;
  end;

  TFetchConverterDictionary = class (TConverterDictionary)
  private
    function GetBucketIndex(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer; DestDataType: Word): Integer; overload;
    function GetBucketIndex(DBType: Word; DBLength, DBScale: Integer; DestDataType: Word): Integer; overload;
  public
    procedure Add(Value: TFetchConverter);
    function FindItem(DBType: Word; DBLength, DBScale: Integer; DestDataType: Word): TFetchConverter;
  end;

  TOnDemandConverterDictionary = class (TConverterDictionary)
  private
    function GetBucketIndex(SourceDataType, DestDataType: Word): integer;
  public
    procedure Add(Value: TOnDemandConverter);
    procedure CloneConverter(SourceDataType, DestDataType, NewDataType: Word);
    procedure CloneConverters(SourceDataType, NewDataType: Word);
    function FindItem(SourceDataType, DestDataType: Word): TOnDemandConverter;
  end;

  TConverterManager = class
  private
    FFetchConverters: TFetchConverterDictionary;
    FOnDemandConverters: TOnDemandConverterDictionary;
  protected
    function GetDateFormat: string; virtual;
    function GetTimeFormat: string; virtual;
    function GetDateTimeFormat: string; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    class function GetDBProvider: Word; virtual;

    procedure AddFetchConverter(FetchConverter: TFetchConverter); overload;
    procedure AddFetchConverter(DBType, RequiredDataType: Word); overload;
    procedure AddFetchConverter(DBType, RequiredDataType, DestDataType: Word; CalcLength: TCalcSizeFunc = nil; CalcScale: TCalcSizeFunc = nil); overload;
    procedure AddFetchConverter(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: integer; RequiredDataType: Word); overload;
    procedure AddFetchConverter(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: integer; RequiredDataType, DestDataType: Word; CalcLength: TCalcSizeFunc = nil; CalcScale: TCalcSizeFunc = nil); overload;
    procedure ClearFetchMappers;

    procedure AddOnDemandConverter(OnDemandConverter: TOnDemandConverter); overload;
    procedure AddOnDemandConverter(SourceType, DestType: Word; GetDataConverter, PutDataConverter: TConvertProcedure; CalcLength: TCalcSizeFunc = nil; CalcScale: TCalcSizeFunc = nil); overload;
    procedure ClearOnDemandConverters;
    procedure CloneOnDemandConverter(SourceDataType, DestDataType, NewDataType: Word);
    procedure CloneOnDemandConverters(SourceDataType, NewDataType: Word);

    function DetectFetchConverter(DBType: Word; DBLength, DBScale: integer; DestDataType: Word): TFetchConverter;
    function DetectOnDemandConverter(SourceDataType, DestDataType: Word): TOnDemandConverter;
  end;

  TSizeConverters = class
  public
    class function CopySize(Value: Integer): Integer;
    class function SizeX2(Value: Integer): Integer;
    class function SizeDiv2(Value: Integer): Integer;
    class function Size0(Value: Integer): Integer;
    class function Size20(Value: Integer): Integer;
    class function Size32(Value: Integer): Integer;
    class function GuidSize(Value: Integer): Integer;
  end;

  TDataConverters = class
  protected
    class function CheckNumericStr(const Num: string; DestLen: Integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CheckNumericStr(const Num: string; Precision, Scale: Integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ExtendNumericStr(var Num: string; SourceScale, DestLen: Integer): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function CheckDateTimeFormat(const Format: string; DestLength: Integer): string; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function InternalInt8ToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt8ToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt16ToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt16ToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt32ToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt32ToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt64ToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalUInt64ToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
    class function InternalSingleToStr(Source: IntPtr; out Str: String; DestLen: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalSingleToStr(Source: IntPtr; out Str: String; Precision, Scale: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalFloatToStr(Source: IntPtr; out Str: String; DestLen: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalFloatToStr(Source: IntPtr; out Str: String; Precision, Scale: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalExtendedToStr(Source: IntPtr; out Str: String; DestLen: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalExtendedToStr(Source: IntPtr; out Str: String; Precision, Scale: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalBCDToStr(Source: IntPtr;  out Str: String; DestLen: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalBCDToStr(Source: IntPtr; out Str: String; Precision, Scale: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalFMTBCDToStr(Source: IntPtr;  out Str: String; DestLen: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalFMTBCDToStr(Source: IntPtr; out Str: String; Precision, Scale: integer): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalBoolToStr(Source: IntPtr): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalDateToStr(Source: IntPtr; out Str: String; const Format: string): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalTimeToStr(Source: IntPtr; SourceScale: Integer; out Str: String; const Format: string): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalDateTimeToStr(Source: IntPtr; SourceScale: Integer; out Str: String; const Format: string): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalSQLTimeStampToStr(Source: IntPtr; SourceScale: Integer; out Str: String; const Format: string): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalSQLTimeStampOffsetToStr(Source: IntPtr; SourceScale: Integer; out Str: String; const Format: string): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function InternalStrToNumber(const Str: String; out Value: Int64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalStrToUNumber(const Str: String; out Value: UInt64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
    class function InternalStrToInt8(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToUInt8(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToInt16(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToUInt16(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToInt32(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToUInt32(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToInt64(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalStrToUInt64(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
    class function InternalStrToSingle(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToFloat(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToExtended(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToBCD(const Str: String; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToFMTBCD(const Str: String; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToBool(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToDate(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
    class function InternalStrToTime(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
    class function ConvertStrToDateTime(const Str: String; const Format: String; out dt: TDateTime): boolean;
    class function InternalStrToDateTime(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToSQLTimeStamp(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToSQLTimeStampOffset(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToGuid(const Str: String; out SourceLen: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function InternalExactCopyToBlob(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyToBlob(Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalAStrToMemo(const AStr: AnsiString; SourceOffset: Integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWStrToWideMemo(const WStr: WideString; SourceOffset: Integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalExactCopyFromBlob(Source: IntPtr; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyFromBlobToBytes(Source: IntPtr; SourceOffset: integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyFromBlobToVarBytes(Source: IntPtr; SourceOffset: integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyFromBlobToExtBytes(StringHeap: TStringHeap; Source: IntPtr; SourceOffset: integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyFromBlobToExtVarBytes(StringHeap: TStringHeap; Source: IntPtr; SourceOffset: integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class procedure InternalMemoToAStr(Source: IntPtr; var SourceOffset: Integer; out SourceLen: Integer; DestOffset: Integer; var DestLen: Integer; out AStr: AnsiString); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure InternalWideMemoToWStr(Source: IntPtr; var SourceOffset: Integer; out SourceLen: Integer; DestOffset: Integer; DestLen: Integer; out WStr: WideString); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure InternalBytesToAStr(Source: IntPtr; var SourceLen: Integer; out AStr: AnsiString); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure InternalBytesToWStr(Source: IntPtr; var SourceLen: Integer; out WStr: WideString); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure InternalVarBytesToAStr(Source: IntPtr; out AStr: AnsiString; out SourceLen: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure InternalVarBytesToWStr(Source: IntPtr; out WStr: WideString; out SourceLen: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalBytesToGuid(Source: IntPtr; var SourceLen: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function InternalExactCopyToBytes(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyToBytes(Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalExactCopyToVarBytes(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyToVarBytes(Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalExactCopyToExtBytes(StringHeap: TStringHeap; Source: IntPtr; SourceLen: Integer; Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyToExtBytes(StringHeap: TStringHeap; Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalExactCopyToExtVarBytes(StringHeap: TStringHeap; Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCopyToExtVarBytes(StringHeap: TStringHeap; Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalExactCopyFromBytes(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalExactCopyFromVarBytes(Source: IntPtr; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function InternalWriteAStr(const AStr: AnsiString; out SourceLen: Integer; Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteAStr(const AStr: AnsiString; SourceOffset: Integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWritePAChar(AStr: PAChar; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteExtAStr(StringHeap: TStringHeap; const AStr: AnsiString; out SourceLen: Integer; Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteExtAStr(StringHeap: TStringHeap; const AStr: AnsiString; SourceOffset: Integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteExtPAChar(StringHeap: TStringHeap; AStr: PAChar; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteWStr(const WStr: WideString; out SourceLen: Integer; Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteWStr(const WStr: WideString; SourceOffset: Integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWritePWChar(WStr: PWChar; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteExtWStr(StringHeap: TStringHeap; const WStr: WideString; out SourceLen: Integer; Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteExtWStr(StringHeap: TStringHeap; const WStr: WideString; SourceOffset: Integer; out SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalWriteExtPWChar(StringHeap: TStringHeap; WStr: PWChar; SourceOffset: Integer; var SourceLen: Integer; Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function InternalInt32ToInt8(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt32ToUInt8(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt32ToInt16(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt32ToUInt16(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt32ToUInt32(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt32ToUInt64(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function InternalInt64ToInt8(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt64ToUInt8(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt64ToInt16(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt64ToUInt16(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt64ToInt32(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalInt64ToUInt32(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalInt64ToUInt64(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}

  {$IFDEF USE_UINT64}
    class function InternalUInt64ToInt8(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt64ToUInt8(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt64ToInt16(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt64ToUInt16(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt64ToInt32(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt64ToUInt32(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalUInt64ToInt64(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}

    class function InternalSingleToInt64(Source: IntPtr; out Value: Int64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalSingleToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
    class function InternalFloatToInt64(Source: IntPtr; out Value: Int64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalFloatToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
    class function InternalExtendedToInt64(Source: IntPtr; out Value: Int64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalExtendedToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
    class function InternalBCDToInt64(Source: IntPtr; out Value: Int64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalBCDToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
    class function InternalExtendedToBCD(e: Extended; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalCurrencyToBCD(e: Extended; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function WriteInt64AsBCD(i64: Int64; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function WriteUInt64AsBCD(ui64: UInt64; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}

    class function InternalFMTBCDToInt64(Source: IntPtr; out Value: Int64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$IFDEF USE_UINT64}
    class function InternalFMTBCDToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
    class function InternalFMTBCDToBCDAsInt64(const Bcd: TBcd; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalBCDToFMTBCD(Bcd: TBcd; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalGuidToBytes(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
    class function InternalVariantToBytes(const v: Variant; out Buf: Pointer; out Size: Integer): TConvertStatus;

    class procedure InternalTimeStampToSQLTimeStamp(const TS: TTimeStamp; Dest: IntPtr); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure InternalTimeStampToSQLTimeStampOffset(const TS: TTimeStamp; Dest: IntPtr); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure InternalSQLTimeStampToSQLTimeStampOffset(Source: PSQLTimeStamp; Dest: PSQLTimeStampOffset); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure InternalSQLTimeStampOffsetToSQLTimeStamp(Source: PSQLTimeStampOffset; Dest: PSQLTimeStamp); {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalSQLTimeStampToTimeStamp(Source: IntPtr; out Value: TTimeStamp): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalSQLTimeStampOffsetToTimeStamp(Source: IntPtr; out Value: TTimeStamp): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalSQLTimeStampToDate(Source: IntPtr; out Date: Integer): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalSQLTimeStampToTime(Source: IntPtr; out Time: Integer): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ValidateSQLTimeStamp(PValue: PSQLTimeStamp): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ValidateSQLTimeStampOffset(PValue: PSQLTimeStampOffset): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalReadTimeStamp(Source: IntPtr; out ts: TTimeStamp): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalReadSQLTimeStamp(Source: IntPtr; out SQLTimeStamp: TSQLTimeStamp): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalReadSQLTimeStampOffset(Source: IntPtr; out SQLTimeStampOffset: TSQLTimeStampOffset): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

  public
    class procedure ChangeDecimalSeparator(var Num: string); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure ChangeDecimalSeparator(var Num: string; const NewDecimalSeparator: Char); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function CopyByte(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function CopyInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function CopyInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function CopyInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function CopyPtr(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function Int8ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function Int8ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function Int8ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int8ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function UInt8ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function UInt8ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function UInt8ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function Int16ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function Int16ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function Int16ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function UInt16ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function UInt16ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function UInt16ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function Int32ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function Int32ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function Int32ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function UInt32ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function UInt32ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function UInt32ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function Int64ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function Int64ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function Int64ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

  {$IFDEF USE_UINT64}
    class function UInt64ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt64ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}

    class function SingleToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function SingleToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function SingleToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function FloatToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function FloatToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function FloatToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function CurrencyToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function ExtendedToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function ExtendedToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function ExtendedToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function BCDToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function BCDToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function BCDToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function FMTBCDToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function FMTBCDToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function FMTBCDToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function BoolToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function BoolToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function BoolToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BoolToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function DateToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function TimeToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function DateTimeToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function SQLTimeStampToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFNDEF FPC}
    class function SQLTimeStampToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function SQLTimeStampToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function SQLTimeStampOffsetToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampOffsetToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFNDEF FPC}
    class function SQLTimeStampOffsetToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function SQLTimeStampOffsetToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function AStrToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function AStrToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function AStrToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToWideMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function WStrToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function WStrToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function WStrToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToWideMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function ExtAStrToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function ExtAStrToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function ExtAStrToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToWideMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function ExtWStrToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function ExtWStrToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function ExtWStrToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToWideMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function BlobToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function BlobToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function BlobToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BlobToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function MemoToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function WideMemoToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WideMemoToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WideMemoToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function WideMemoToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function BytesToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function BytesToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function BytesToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function BytesToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function VarBytesToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function VarBytesToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function VarBytesToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VarBytesToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function ExtVarBytesToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function ExtVarBytesToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function ExtVarBytesToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtVarBytesToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function ExtBytesToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function ExtBytesToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function ExtBytesToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtBytesToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function VariantToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFDEF USE_UINT64}
    class function VariantToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function VariantToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$IFNDEF FPC}
    class function VariantToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
  {$ENDIF}
    class function VariantToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function VariantToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;

    class function GuidToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function GuidToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function GuidToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function GuidToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function GuidToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function GuidToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function GuidToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
    class function GuidToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
  end;

const
  ConvertStatusErrors: array [TConvertStatus] of string = ('',
    SBinaryTooLong, SStringTooLong,
    SDataTruncated, SFractionTruncated,
    SInvalidBinaryValue, SInvalidBlobValue, SInvalidDataMapping,
    SInvalidValueScale, SValueTooLong, SValueOutOfRange,
    SInvalidBooleanValue, SInvalidGUIDValue, SInvalidIntervalValue,
    SInvalidDateTimeValue, SInvalidSQLTimeStampValue,
    SInvalidIntegerValue, SInvalidNumericValue
  );

const
  Bcd_0: TBcd = (Precision: 8; SignSpecialPlaces: 2; Fraction: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

var
  DBTypeInfos: TDBTypeInfos;

function Max(V1, V2: TConvertStatus): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}

implementation

const
  SDuplicateItem = 'List does not allow duplicates';
  SListCapacityError = 'List capacity out of bounds';
  SIndexOutOfRange = 'Index out of range';

{$IFDEF USE_TFORMATSETTINGS}
var
  InternalFormatSettings: TFormatSettings;
{$ENDIF}

{ EDataTypeMappingError }

constructor EDataTypeMappingError.Create;
begin
  inherited Create('Unexpected data type mapping error');
end;

{ TDBTypeInfo }

constructor TDBTypeInfo.Create(DBType: Word; const Name: string; Length, Scale: boolean);
begin
  inherited Create;

  FDBType := DBType;
  FName := Name;
  FLength := Length;
  FScale := Scale;
end;

function TDBTypeInfo.GetDBProvider: Word;
begin
  Result := (FDBType div 100) * 100;
end;

{ TDBTypeInfos }

constructor TDBTypeInfos.Create;
begin
  inherited;

  FTypeInfos := TCRObjectList.Create;
end;

destructor TDBTypeInfos.Destroy;
begin
  Clear;
  FTypeInfos.Free;
  inherited;
end;

function TDBTypeInfos.GetCount: Integer;
begin
  Result := FTypeInfos.Count;
end;

function TDBTypeInfos.GetTypeInfo(Index: integer): TDBTypeInfo;
begin
  Result := TDBTypeInfo(FTypeInfos[Index]);
end;

procedure TDBTypeInfos.Add(DBType: Word; const Name: string; Length, Scale: boolean);
var
  DBTypeInfo: TDBTypeInfo;
begin
  // DB types can be duplicated if installed any DAC product and UniDAC
  if FindTypeInfo(DBType) = nil then begin
    DBTypeInfo := TDBTypeInfo.Create(DBType, Name, Length, Scale);
    FTypeInfos.Add(DBTypeInfo);
  end;
end;

procedure TDBTypeInfos.Delete(Index: Integer);
begin
  FTypeInfos.Delete(Index);
end;

procedure TDBTypeInfos.Clear;
begin
  FTypeInfos.Clear;
end;

function TDBTypeInfos.Check(DBType: Word; LengthMin, LengthMax, ScaleMin, ScaleMax: Integer; IsDBTypeRequired: boolean): Exception;
var
  DBTypeInfo: TDBTypeinfo;
begin
  if DBType = 0 then begin
    if IsDBTypeRequired then
      Result := EInvalidDBTypeMapping.Create(SNotDefinedDBType)
    else
      Result := nil
  end
  else begin
    DBTypeInfo := FindTypeInfo(DBType);
    if DBTypeInfo = nil then
      Result := EInvalidDBTypeMapping.CreateFmt(SUnsupportedDBType, [IntToStr(DBType)])
    else if (DBTypeInfo.Length = False) and ((LengthMin <> -1) or (LengthMax <> -1)) then
      Result := EInvalidDBTypeMapping.CreateFmt(SInvalidDBLength, [DBTypeInfo.Name])
    else if (DBTypeInfo.Scale = False) and ((ScaleMin <> -1) or (ScaleMax <> -1)) then
      Result := EInvalidDBTypeMapping.CreateFmt(SInvalidDBScale, [DBTypeInfo.Name])
    else
      Result := nil;
  end;
end;

function TDBTypeInfos.FindTypeInfo(DBType: Word): TDBTypeInfo;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if TypeInfos[i].DBType = DBType then begin
      Result := TypeInfos[i];
      Exit;
    end;
end;

function TDBTypeInfos.FindTypeInfo(const Name: string; DBProvider: Word): TDBTypeInfo;
var
  UName: string;
  i: integer;
begin
  Result := nil;

  UName := AnsiUpperCase(Trim(Name));
  for i := 0 to Count - 1 do
    if (DBProvider = 0) or (DBProvider = TypeInfos[i].DBProvider) then
      if AnsiUpperCase(TypeInfos[i].Name) = UName then begin
        Result := TypeInfos[i];
        Exit;
      end;
end;

{ TMapRule }

constructor TMapRule.Create(Owner: TCollection);
begin
  inherited Create(Owner);

  FFieldName := '';

  FDBType := 0;
  FDBLengthMin := -1;
  FDBLengthMax := -1;
  FDBScaleMin := -1;
  FDBScaleMax := -1;

  FFieldLength := -1;
  FFieldScale := -1;

  FFormat := '';
  FIgnoreErrors := False;
end;

function TMapRule.GetDBProvider: Word;
begin
  Result := (FDBType div 100) * 100;
end;

procedure TMapRule.AssignTo(Dest: TPersistent);
begin
  if Dest is TMapRule then
    AssignToRule(TMapRule(Dest));
end;

procedure TMapRule.AssignToRule(Dest: TMapRule);
begin
  Dest.FFieldName    := FFieldName;

  Dest.FDBType       := FDBType;

  Dest.FDBLengthMin  := FDBLengthMin;
  Dest.FDBLengthMax  := FDBLengthMax;
  Dest.FDBScaleMin   := FDBScaleMin;
  Dest.FDBScaleMax   := FDBScaleMax;

  Dest.FFieldLength  := FFieldLength;
  Dest.FFieldScale   := FFieldScale;

  Dest.FFormat := FFormat;
  Dest.FIgnoreErrors := FIgnoreErrors;
end;

{ TMapRules }

function TMapRules.GetItem(Index: Integer): TMapRule;
begin
  Result := TMapRule(inherited GetItem(Index));
end;

procedure TMapRules.SetItem(Index: Integer; Value: TMapRule);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TCRMapRule }

constructor TCRMapRule.Create(Owner: TCollection);
begin
  inherited Create(Owner);

  FDataType := 0;
end;

procedure TCRMapRule.AssignTo(Dest: TPersistent);
begin
  if Dest is TCRMapRule then
    AssignToRule(TCRMapRule(Dest))
  else
    inherited;
end;

procedure TCRMapRule.AssignToRule(Dest: TCRMapRule);
begin
  inherited AssignToRule(Dest);

  Dest.FDataType := FDataType;
end;

{ TCRMapRules }

constructor TCRMapRules.Create;
begin
  inherited Create(GetMapRuleClass);
  FEnabled := True;
end;

function TCRMapRules.GetItem(Index: Integer): TCRMapRule;
begin
  Result := TCRMapRule(inherited GetItem(Index));
end;

procedure TCRMapRules.SetItem(Index: Integer; Value: TCRMapRule);
begin
  inherited SetItem(Index, TMapRule(Value));
end;

class function TCRMapRules.GetMapRuleClass: TCRMapRuleClass;
begin
  Result := TCRMapRule;
end;

class function TCRMapRules.GetConverterManager: TConverterManager;
begin
  Result := nil;
  Assert(False); // Must be overriden
end;

function TCRMapRules.AddRule(const FieldName: string;
  DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer;
  DataType: Word; FieldLength, FieldScale: Integer;
  IgnoreErrors: boolean; const Format: string): TCRMapRule;
var
  Error: Exception;
begin
  Error := DBTypeInfos.Check(DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, FieldName = '');
  Result := nil;

  if Error = nil then begin
    Result := GetMapRuleClass.Create(Self);

    Result.FieldName := FieldName;

    Result.DBType := DBType;
    Result.DBLengthMin := DBLengthMin;
    Result.DBLengthMax := DBLengthMax;
    Result.DBScaleMin := DBScaleMin;
    Result.DBScaleMax := DBScaleMax;

    Result.DataType := DataType;
    Result.FieldLength := FieldLength;
    Result.FieldScale := FieldScale;

    Result.IgnoreErrors := IgnoreErrors;
    Result.Format := CheckFormat(Result, Format);
  end
  else if FIgnoreInvalidRules then
    Error.Free
  else
    raise Error;
end;

function TCRMapRules.CheckSQLType(Rule: TMapRule; DBType, DBLength, DBScale: integer): boolean;
begin
  Result := (Rule.DBType = DBType) and
   ((DBLength = -1) or ((Rule.DBLengthMin = -1) or (DBLength >= Rule.DBLengthMin)) and
                       ((Rule.DBLengthMax = -1) or (DBLength <= Rule.DBLengthMax))) and
   ((DBScale  = -1) or ((Rule.DBScaleMin  = -1) or (DBScale  >= Rule.DBScaleMin)) and
                       ((Rule.DBScaleMax  = -1) or (DBScale  <= Rule.DBScaleMax)));
end;

function TCRMapRules.DetectFieldNameMapRule(const FieldName: string; DBType, DBLength, DBScale: integer): TCRMapRule;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if (Result.FieldName <> '') and
       (AnsiUpperCase(Result.FieldName) = AnsiUpperCase(FieldName)) and
       ((Result.DBType = 0) or CheckSQLType(Result, DBType, DBLength, DBScale))
    then
      Exit;
  end;

  Result := nil;
end;

function TCRMapRules.DetectDBTypeMapRule(DBType, DBLength, DBScale: integer): TCRMapRule;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if (Result.DBType <> 0) and CheckSQLType(Result, DBType, DBLength, DBScale) then
      Exit;
  end;

  Result := nil;
end;

function TCRMapRules.CheckFormat(Rule: TCRMapRule; const Format: string): string;
begin
  Result := Format;
  if Format = '' then
    case Rule.DataType of
      dtDateTime,
      dtSQLTimeStamp,
      dtSQLTimeStampOffset:
        Result := GetConverterManager.GetDateTimeFormat;
      dtDate:
        Result := GetConverterManager.GetDateFormat;
      dtTime:
        Result := GetConverterManager.GetTimeFormat;
    end;
end;

{ TFetchConverter }

constructor TFetchConverter.Create(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: integer;
  DestDataType, InternalDataType: Word; CalcLength: TCalcSizeFunc; CalcScale: TCalcSizeFunc);
begin
  inherited Create;

  FDBType := DBType;
  FDBLengthMin := DBLengthMin;
  FDBLengthMax := DBLengthMax;
  FDBScaleMin := DBScaleMin;
  FDBScaleMax := DBScaleMax;

  FDestDataType := DestDataType;
  FInternalDataType := InternalDataType;

  FCalcLength := CalcLength;
  FCalcScale := CalcScale;
end;

{ TOnDemandConverter }

constructor TOnDemandConverter.Create(SourceDataType, DestDataType: Word;
  GetDataConverter, PutDataConverter: TConvertProcedure;
  CalcLength: TCalcSizeFunc; CalcScale: TCalcSizeFunc);
begin
  inherited Create;

  FSourceDataType := SourceDataType;
  FDestDataType := DestDataType;

  FGetDataConverter := GetDataConverter;
  FPutDataConverter := PutDataConverter;

  FCalcLength := CalcLength;
  FCalcScale := CalcScale;
end;

function TOnDemandConverter.Clone: TOnDemandConverter;
begin
  Result := TOnDemandConverter.Create(FSourceDataType, FDestDataType, FGetDataConverter, FPutDataConverter, FCalcLength, FCalcScale);
end;

{ TConverterDictionary }

constructor TConverterDictionary.Create;
begin
  Create(0);
end;

constructor TConverterDictionary.Create(Capacity: integer);
begin
  inherited Create;

  if Capacity < 0 then
    raise EDataTypeMappingError.Create(SListCapacityError);

  FGrowThreshold := 0;

  SetCapacity(Capacity);
end;

destructor TConverterDictionary.Destroy;
begin
  Clear;

  inherited;
end;

function TConverterDictionary.GetRehashBucketIndex(const HashCode: integer): integer;
var
  Code: integer;
begin
  if Length(FItems) = 0 then begin
    Result := not High(Integer);
    Exit;
  end;

  Result := HashCode and (Length(FItems) - 1);
  while True do begin
    Code := FItems[Result].HashCode;

    if Code = 0 then begin
      Result := not Result;
      Exit;
    end;

    Inc(Result);
    if Result >= Length(FItems) then
      Result := 0;
  end;
end;

function TConverterDictionary.Hash(Value1, Value2: Word): integer;
const
  PositiveMask = not Integer($80000000);
begin
  if (Value1 = 0) and (Value2 = 0) then
    Result := 42
  else
    Result := Value2 xor Value1 xor (Value2 shl 3) xor (Value1 shl 6) xor (Value2 shl 9) xor (Value1 shl 12);
  Result := PositiveMask and ((PositiveMask and Result) + 1);
end;

procedure TConverterDictionary.SetCapacity(const Capacity: integer);
var
  n: integer;
begin
  if Capacity = Length(FItems) then
    Exit
  else if Capacity < FCount then
    raise EDataTypeMappingError.Create(SListCapacityError);

  if Capacity = 0 then
    Clear
  else begin
    n := 4;
    while n < Capacity do
      n := n shl 1;
    if n shr 1 + n shr 2 < Capacity then
      n := n shl 1;
    Rehash(n);
  end
end;

procedure TConverterDictionary.Grow;
var
  NewCapacity: integer;
begin
  NewCapacity := Length(FItems) * 2;
  if NewCapacity = 0 then
    NewCapacity := 4;
  Rehash(NewCapacity);
end;

procedure TConverterDictionary.Rehash(NewCapacity: integer);
var
  i: integer;
  Index: Integer;
  OldItems: TConverterDictionaryItemArray;
  NewItems: TConverterDictionaryItemArray;
begin
  OldItems := FItems;
  SetLength(NewItems, NewCapacity);
  FItems := NewItems;
  FGrowThreshold := NewCapacity shr 1;

  for i := 0 to Length(OldItems) - 1 do
    if OldItems[i].HashCode <> 0 then begin
      Index := not GetRehashBucketIndex(OldItems[i].HashCode);
      FItems[Index] := OldItems[i];
    end;
end;

procedure TConverterDictionary.Clear;
var
  i: integer;
begin
  for i := 0 to Length(FItems) - 1 do
    if FItems[i].HashCode <> 0 then
      FItems[i].Value.Free;

  FCount := 0;
  SetLength(FItems, 0);
  FGrowThreshold := 0;
end;

{ TFetchConverterDictionary }

function TFetchConverterDictionary.GetBucketIndex(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: Integer; DestDataType: Word): Integer;
var
  HashCode: Integer;
  ItemHashCode: integer;
  FetchConverter: TFetchConverter;
begin
  if Length(FItems) = 0 then begin
    Result := not High(Integer);
    Exit;
  end;

  HashCode := Hash(DBType, DestDataType);

  Result := HashCode and (Length(FItems) - 1);
  while True do begin
    ItemHashCode := FItems[Result].HashCode;

    if ItemHashCode = 0 then begin
      Result := not Result;
      Exit;
    end;

    if ItemHashCode = HashCode then begin
      FetchConverter := TFetchConverter(FItems[Result].Value);
      if (DBType = FetchConverter.DBType) and
         (DestDataType = FetchConverter.DestDataType) and
         (DBLengthMin = FetchConverter.FDBLengthMin) and
         (DBLengthMax = FetchConverter.FDBLengthMax) and
         (DBScaleMin = FetchConverter.FDBScaleMin) and
         (DBScaleMax = FetchConverter.FDBScaleMax)
      then
        Exit;
    end;

    Inc(Result);
    if Result >= Length(FItems) then
      Result := 0;
  end;
end;

function TFetchConverterDictionary.GetBucketIndex(DBType: Word; DBLength, DBScale: Integer; DestDataType: Word): Integer;
var
  HashCode: Integer;
  ItemHashCode: integer;
  FetchConverter: TFetchConverter;
begin
  if Length(FItems) = 0 then begin
    Result := not High(Integer);
    Exit;
  end;

  HashCode := Hash(DBType, DestDataType);

  Result := HashCode and (Length(FItems) - 1);
  while True do begin
    ItemHashCode := FItems[Result].HashCode;

    if ItemHashCode = 0 then begin
      Result := not Result;
      Exit;
    end;

    if ItemHashCode = HashCode then begin
      FetchConverter := TFetchConverter(FItems[Result].Value);
      if (DBType = FetchConverter.DBType) and
         (DestDataType = FetchConverter.DestDataType) and
         ((DBLength = -1) or (((FetchConverter.FDBLengthMin = -1) or (DBLength >= FetchConverter.FDBLengthMin)) and
                             ((FetchConverter.FDBLengthMax  = -1) or (DBLength <= FetchConverter.FDBLengthMax)))) and
         ((DBScale = -1)  or (((FetchConverter.FDBScaleMin  = -1)  or (DBScale >= FetchConverter.FDBScaleMin)) and
                             ((FetchConverter.FDBScaleMax   = -1)  or (DBScale <= FetchConverter.FDBScaleMax))))
      then
        Exit;
    end;

    Inc(Result);
    if Result >= Length(FItems) then
      Result := 0;
  end;
end;

procedure TFetchConverterDictionary.Add(Value: TFetchConverter);
var
  Index: Integer;
begin
  if FCount >= FGrowThreshold then
    Grow;

  Index := GetBucketIndex(Value.DBType, Value.DBLengthMin, Value.DBLengthMax, Value.DBScaleMin, Value.DBScaleMax, Value.DestDataType);
  if Index >= 0 then
    raise EDataTypeMappingError.Create(SDuplicateItem);

  Index := not Index;

  FItems[Index].HashCode := Hash(Value.DBType, Value.DestDataType);
  FItems[Index].Value := Value;

  Inc(FCount);
end;

function TFetchConverterDictionary.FindItem(DBType: Word; DBLength, DBScale: Integer; DestDataType: Word): TFetchConverter;
var
  Index: Integer;
begin
  Index := GetBucketIndex(DBType, DBLength, DBScale, DestDataType);
  if Index >= 0 then
    Result := TFetchConverter(FItems[Index].Value)
  else
    Result := nil;
end;

{ TOnDemandConverterDictionary }

function TOnDemandConverterDictionary.GetBucketIndex(SourceDataType, DestDataType: Word): Integer;
var
  HashCode: Integer;
  ItemHashCode: integer;
  OnDemandConverter: TOnDemandConverter;
begin
  if Length(FItems) = 0 then begin
    Result := not High(Integer);
    Exit;
  end;

  HashCode := Hash(SourceDataType, DestDataType);

  Result := HashCode and (Length(FItems) - 1);
  while True do begin
    ItemHashCode := FItems[Result].HashCode;

    if ItemHashCode = 0 then begin
      Result := not Result;
      Exit;
    end;

    if ItemHashCode = HashCode then begin
      OnDemandConverter := TOnDemandConverter(FItems[Result].Value);
      if (SourceDataType = OnDemandConverter.SourceDataType) and
         (DestDataType = OnDemandConverter.DestDataType)
      then
        Exit;
    end;

    Inc(Result);
    if Result >= Length(FItems) then
      Result := 0;
  end;
end;

procedure TOnDemandConverterDictionary.Add(Value: TOnDemandConverter);
var
  Index: Integer;
begin
  if FCount >= FGrowThreshold then
    Grow;

  Index := GetBucketIndex(Value.SourceDataType, Value.DestDataType);
  if Index >= 0 then
    raise EDataTypeMappingError.Create(SDuplicateItem);

  Index := not Index;

  FItems[Index].HashCode := Hash(Value.SourceDataType, Value.DestDataType);
  FItems[Index].Value := Value;

  Inc(FCount);
end;

procedure TOnDemandConverterDictionary.CloneConverter(SourceDataType, DestDataType, NewDataType: Word);
var
  i: Integer;
  List: TList;
  OnDemandConverter: TOnDemandConverter;
begin
  List := TList.Create;
  try
    for i := 0 to Length(FItems) - 1 do
      if FItems[i].HashCode <> 0 then begin
        OnDemandConverter := TOnDemandConverter(FItems[i].Value);
        if (OnDemandConverter.SourceDataType = SourceDataType) and
           (OnDemandConverter.DestDataType = DestDataType)
        then
          List.Add(OnDemandConverter);
      end;

    for i := 0 to List.Count - 1 do begin
      OnDemandConverter := List[i];
      OnDemandConverter := OnDemandConverter.Clone;
      OnDemandConverter.SourceDataType := NewDataType;
      Add(OnDemandConverter);
    end;
  finally
    List.Free;
  end;
end;

procedure TOnDemandConverterDictionary.CloneConverters(SourceDataType, NewDataType: Word);
var
  i: Integer;
  List: TList;
  OnDemandConverter: TOnDemandConverter;
begin
  List := TList.Create;
  try
    for i := 0 to Length(FItems) - 1 do
      if FItems[i].HashCode <> 0 then begin
        OnDemandConverter := TOnDemandConverter(FItems[i].Value);
        if OnDemandConverter.SourceDataType = SourceDataType then
          List.Add(OnDemandConverter);
      end;

    for i := 0 to List.Count - 1 do begin
      OnDemandConverter := List[i];
      OnDemandConverter := OnDemandConverter.Clone;
      OnDemandConverter.SourceDataType := NewDataType;
      Add(OnDemandConverter);
    end;
  finally
    List.Free;
  end;
end;

function TOnDemandConverterDictionary.FindItem(SourceDataType, DestDataType: Word): TOnDemandConverter;
var
  Index: Integer;
begin
  Index := GetBucketIndex(SourceDataType, DestDataType);
  if Index >= 0 then
    Result := TOnDemandConverter(FItems[Index].Value)
  else
    Result := nil;
end;

{ TConverterManager }

constructor TConverterManager.Create;
begin
  inherited Create;

  FFetchConverters := TFetchConverterDictionary.Create(256);
  FOnDemandConverters := TOnDemandConverterDictionary.Create(1024);

  AddOnDemandConverter(dtBoolean, dtInt8, TDataConverters.BoolToInt8, TDataConverters.Int8ToBool);
  AddOnDemandConverter(dtBoolean, dtUInt8, TDataConverters.BoolToUInt8, TDataConverters.UInt8ToBool);
  AddOnDemandConverter(dtBoolean, dtSmallint, TDataConverters.BoolToInt16, TDataConverters.Int16ToBool);
  AddOnDemandConverter(dtBoolean, dtWord, TDataConverters.BoolToUInt16, TDataConverters.UInt16ToBool);
  AddOnDemandConverter(dtBoolean, dtInteger, TDataConverters.BoolToInt32, TDataConverters.Int32ToBool);
  AddOnDemandConverter(dtBoolean, dtUInt32, TDataConverters.BoolToUInt32, TDataConverters.UInt32ToBool);
  AddOnDemandConverter(dtBoolean, dtInt64, TDataConverters.BoolToInt64, TDataConverters.Int64ToBool);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtBoolean, dtUInt64, TDataConverters.BoolToUInt64, TDataConverters.UInt64ToBool);
{$ENDIF}
  AddOnDemandConverter(dtBoolean, dtSingle, TDataConverters.BoolToSingle, TDataConverters.SingleToBool);
  AddOnDemandConverter(dtBoolean, dtFloat, TDataConverters.BoolToFloat, TDataConverters.FloatToBool);
  AddOnDemandConverter(dtBoolean, dtCurrency, TDataConverters.BoolToFloat, TDataConverters.FloatToBool);
  AddOnDemandConverter(dtBoolean, dtExtended, TDataConverters.BoolToExtended, TDataConverters.ExtendedToBool);
  AddOnDemandConverter(dtBoolean, dtBCD, TDataConverters.BoolToBCD, TDataConverters.BCDToBool, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtBoolean, dtFMTBCD, TDataConverters.BoolToFMTBCD, TDataConverters.FMTBCDToBool, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtBoolean, dtString, TDataConverters.BoolToAStr, TDataConverters.AStrToBool);
  AddOnDemandConverter(dtBoolean, dtExtString, TDataConverters.BoolToExtAStr, TDataConverters.ExtAStrToBool);
  AddOnDemandConverter(dtBoolean, dtWideString, TDataConverters.BoolToWStr, TDataConverters.WStrToBool);
  AddOnDemandConverter(dtBoolean, dtExtWideString, TDataConverters.BoolToExtWStr, TDataConverters.ExtWStrToBool);
  AddOnDemandConverter(dtBoolean, dtBytes, TDataConverters.BoolToBytes, TDataConverters.BytesToBool);
  AddOnDemandConverter(dtBoolean, dtVarBytes, TDataConverters.BoolToVarBytes, TDataConverters.VarBytesToBool);
  AddOnDemandConverter(dtBoolean, dtExtVarBytes, TDataConverters.BoolToExtVarBytes, TDataConverters.ExtVarBytesToBool);
  AddOnDemandConverter(dtBoolean, dtVariant, TDataConverters.BoolToVariant, TDataConverters.VariantToBool);

  AddOnDemandConverter(dtInt8, dtBoolean, TDataConverters.Int8ToBool, TDataConverters.BoolToInt8);
  AddOnDemandConverter(dtInt8, dtUInt8, TDataConverters.Int8ToUInt8, TDataConverters.UInt8ToInt8);
  AddOnDemandConverter(dtInt8, dtSmallint, TDataConverters.Int8ToInt16, TDataConverters.Int16ToInt8);
  AddOnDemandConverter(dtInt8, dtWord, TDataConverters.Int8ToUInt16, TDataConverters.UInt16ToInt8);
  AddOnDemandConverter(dtInt8, dtInteger, TDataConverters.Int8ToInt32, TDataConverters.Int32ToInt8);
  AddOnDemandConverter(dtInt8, dtUInt32, TDataConverters.Int8ToUInt32, TDataConverters.UInt32ToInt8);
  AddOnDemandConverter(dtInt8, dtInt64, TDataConverters.Int8ToInt64, TDataConverters.Int64ToInt8);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtInt8, dtUInt64, TDataConverters.Int8ToUInt64, TDataConverters.UInt64ToInt8);
{$ENDIF}
  AddOnDemandConverter(dtInt8, dtSingle, TDataConverters.Int8ToSingle, TDataConverters.SingleToInt8);
  AddOnDemandConverter(dtInt8, dtFloat, TDataConverters.Int8ToFloat, TDataConverters.FloatToInt8);
  AddOnDemandConverter(dtInt8, dtCurrency, TDataConverters.Int8ToFloat, TDataConverters.FloatToInt8);
  AddOnDemandConverter(dtInt8, dtExtended, TDataConverters.Int8ToExtended, TDataConverters.ExtendedToInt8);
  AddOnDemandConverter(dtInt8, dtBCD, TDataConverters.Int8ToBCD, TDataConverters.BCDToInt8, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtInt8, dtFMTBCD, TDataConverters.Int8ToFMTBCD, TDataConverters.FMTBCDToInt8, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtInt8, dtString, TDataConverters.Int8ToAStr, TDataConverters.AStrToInt8);
  AddOnDemandConverter(dtInt8, dtExtString, TDataConverters.Int8ToExtAStr, TDataConverters.ExtAStrToInt8);
  AddOnDemandConverter(dtInt8, dtWideString, TDataConverters.Int8ToWStr, TDataConverters.WStrToInt8);
  AddOnDemandConverter(dtInt8, dtExtWideString, TDataConverters.Int8ToExtWStr, TDataConverters.ExtWStrToInt8);
  AddOnDemandConverter(dtInt8, dtBytes, TDataConverters.Int8ToBytes, TDataConverters.BytesToInt8);
  AddOnDemandConverter(dtInt8, dtVarBytes, TDataConverters.Int8ToVarBytes, TDataConverters.VarBytesToInt8);
  AddOnDemandConverter(dtInt8, dtExtVarBytes, TDataConverters.Int8ToExtVarBytes, TDataConverters.ExtVarBytesToInt8);
  AddOnDemandConverter(dtInt8, dtVariant, TDataConverters.Int8ToVariant, TDataConverters.VariantToInt8);

  AddOnDemandConverter(dtUInt8, dtBoolean, TDataConverters.UInt8ToBool, TDataConverters.BoolToUInt8);
  AddOnDemandConverter(dtUInt8, dtInt8, TDataConverters.UInt8ToInt8, TDataConverters.Int8ToUInt8);
  AddOnDemandConverter(dtUInt8, dtSmallint, TDataConverters.UInt8ToInt16, TDataConverters.Int16ToUInt8);
  AddOnDemandConverter(dtUInt8, dtWord, TDataConverters.UInt8ToUInt16, TDataConverters.UInt16ToUInt8);
  AddOnDemandConverter(dtUInt8, dtInteger, TDataConverters.UInt8ToInt32, TDataConverters.Int32ToUInt8);
  AddOnDemandConverter(dtUInt8, dtUInt32, TDataConverters.UInt8ToUInt32, TDataConverters.UInt32ToUInt8);
  AddOnDemandConverter(dtUInt8, dtInt64, TDataConverters.UInt8ToInt64, TDataConverters.Int64ToUInt8);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtUInt8, dtUInt64, TDataConverters.UInt8ToUInt64, TDataConverters.UInt64ToUInt8);
{$ENDIF}
  AddOnDemandConverter(dtUInt8, dtSingle, TDataConverters.UInt8ToSingle, TDataConverters.SingleToUInt8);
  AddOnDemandConverter(dtUInt8, dtFloat, TDataConverters.UInt8ToFloat, TDataConverters.FloatToUInt8);
  AddOnDemandConverter(dtUInt8, dtCurrency, TDataConverters.UInt8ToFloat, TDataConverters.FloatToUInt8);
  AddOnDemandConverter(dtUInt8, dtExtended, TDataConverters.UInt8ToExtended, TDataConverters.ExtendedToUInt8);
  AddOnDemandConverter(dtUInt8, dtBCD, TDataConverters.UInt8ToBCD, TDataConverters.BCDToUInt8, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtUInt8, dtFMTBCD, TDataConverters.UInt8ToFMTBCD, TDataConverters.FMTBCDToUInt8, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtUInt8, dtString, TDataConverters.UInt8ToAStr, TDataConverters.AStrToUInt8);
  AddOnDemandConverter(dtUInt8, dtExtString, TDataConverters.UInt8ToExtAStr, TDataConverters.ExtAStrToUInt8);
  AddOnDemandConverter(dtUInt8, dtWideString, TDataConverters.UInt8ToWStr, TDataConverters.WStrToUInt8);
  AddOnDemandConverter(dtUInt8, dtExtWideString, TDataConverters.UInt8ToExtWStr, TDataConverters.ExtWStrToUInt8);
  AddOnDemandConverter(dtUInt8, dtBytes, TDataConverters.UInt8ToBytes, TDataConverters.BytesToUInt8);
  AddOnDemandConverter(dtUInt8, dtVarBytes, TDataConverters.UInt8ToVarBytes, TDataConverters.VarBytesToUInt8);
  AddOnDemandConverter(dtUInt8, dtExtVarBytes, TDataConverters.UInt8ToExtVarBytes, TDataConverters.ExtVarBytesToUInt8);
  AddOnDemandConverter(dtUInt8, dtVariant, TDataConverters.UInt8ToVariant, TDataConverters.VariantToUInt8);

  AddOnDemandConverter(dtSmallint, dtBoolean, TDataConverters.Int16ToBool, TDataConverters.BoolToInt16);
  AddOnDemandConverter(dtSmallint, dtInt8, TDataConverters.Int16ToInt8, TDataConverters.Int8ToInt16);
  AddOnDemandConverter(dtSmallint, dtUInt8, TDataConverters.Int16ToUInt8, TDataConverters.UInt8ToInt16);
  AddOnDemandConverter(dtSmallint, dtInteger, TDataConverters.Int16ToInt32, TDataConverters.Int32ToInt16);
  AddOnDemandConverter(dtSmallint, dtWord, TDataConverters.Int16ToUInt16, TDataConverters.UInt16ToInt16);
  AddOnDemandConverter(dtSmallint, dtUInt32, TDataConverters.Int16ToUInt32, TDataConverters.UInt32ToInt16);
  AddOnDemandConverter(dtSmallint, dtInt64, TDataConverters.Int16ToInt64, TDataConverters.Int64ToInt16);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtSmallint, dtUInt64, TDataConverters.Int16ToUInt64, TDataConverters.UInt64ToInt16);
{$ENDIF}
  AddOnDemandConverter(dtSmallint, dtSingle, TDataConverters.Int16ToSingle, TDataConverters.SingleToInt16);
  AddOnDemandConverter(dtSmallint, dtFloat, TDataConverters.Int16ToFloat, TDataConverters.FloatToInt16);
  AddOnDemandConverter(dtSmallint, dtCurrency, TDataConverters.Int16ToFloat, TDataConverters.FloatToInt16);
  AddOnDemandConverter(dtSmallint, dtExtended, TDataConverters.Int16ToExtended, TDataConverters.ExtendedToInt16);
  AddOnDemandConverter(dtSmallint, dtBCD, TDataConverters.Int16ToBCD, TDataConverters.BCDToInt16, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtSmallint, dtFMTBCD, TDataConverters.Int16ToFMTBCD, TDataConverters.FMTBCDToInt16, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtSmallint, dtString, TDataConverters.Int16ToAStr, TDataConverters.AStrToInt16);
  AddOnDemandConverter(dtSmallint, dtExtString, TDataConverters.Int16ToExtAStr, TDataConverters.ExtAStrToInt16);
  AddOnDemandConverter(dtSmallint, dtWideString, TDataConverters.Int16ToWStr, TDataConverters.WStrToInt16);
  AddOnDemandConverter(dtSmallint, dtExtWideString, TDataConverters.Int16ToExtWStr, TDataConverters.ExtWStrToInt16);
  AddOnDemandConverter(dtSmallint, dtBytes, TDataConverters.Int16ToBytes, TDataConverters.BytesToInt16);
  AddOnDemandConverter(dtSmallint, dtVarBytes, TDataConverters.Int16ToVarBytes, TDataConverters.VarBytesToInt16);
  AddOnDemandConverter(dtSmallint, dtExtVarBytes, TDataConverters.Int16ToExtVarBytes, TDataConverters.ExtVarBytesToInt16);
  AddOnDemandConverter(dtSmallint, dtVariant, TDataConverters.Int16ToVariant, TDataConverters.VariantToInt16);

  AddOnDemandConverter(dtWord, dtBoolean, TDataConverters.UInt16ToBool, TDataConverters.BoolToUInt16);
  AddOnDemandConverter(dtWord, dtInt8, TDataConverters.UInt16ToInt8, TDataConverters.Int8ToUInt16);
  AddOnDemandConverter(dtWord, dtUInt8, TDataConverters.UInt16ToUInt8, TDataConverters.UInt8ToUInt16);
  AddOnDemandConverter(dtWord, dtSmallint, TDataConverters.UInt16ToInt16, TDataConverters.Int16ToUInt16);
  AddOnDemandConverter(dtWord, dtInteger, TDataConverters.UInt16ToInt32, TDataConverters.Int32ToUInt16);
  AddOnDemandConverter(dtWord, dtUInt32, TDataConverters.UInt16ToUInt32, TDataConverters.UInt32ToUInt16);
  AddOnDemandConverter(dtWord, dtInt64, TDataConverters.UInt16ToInt64, TDataConverters.Int64ToUInt16);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtWord, dtUInt64, TDataConverters.UInt16ToUInt64, TDataConverters.UInt64ToUInt16);
{$ENDIF}
  AddOnDemandConverter(dtWord, dtSingle, TDataConverters.UInt16ToSingle, TDataConverters.SingleToUInt16);
  AddOnDemandConverter(dtWord, dtFloat, TDataConverters.UInt16ToFloat, TDataConverters.FloatToUInt16);
  AddOnDemandConverter(dtWord, dtCurrency, TDataConverters.UInt16ToFloat, TDataConverters.FloatToUInt16);
  AddOnDemandConverter(dtWord, dtExtended, TDataConverters.UInt16ToExtended, TDataConverters.ExtendedToUInt16);
  AddOnDemandConverter(dtWord, dtBCD, TDataConverters.UInt16ToBCD, TDataConverters.BCDToUInt16, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtWord, dtFMTBCD, TDataConverters.UInt16ToFMTBCD, TDataConverters.FMTBCDToUInt16, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtWord, dtString, TDataConverters.UInt16ToAStr, TDataConverters.AStrToUInt16);
  AddOnDemandConverter(dtWord, dtExtString, TDataConverters.UInt16ToExtAStr, TDataConverters.ExtAStrToUInt16);
  AddOnDemandConverter(dtWord, dtWideString, TDataConverters.UInt16ToWStr, TDataConverters.WStrToUInt16);
  AddOnDemandConverter(dtWord, dtExtWideString, TDataConverters.UInt16ToExtWStr, TDataConverters.ExtWStrToUInt16);
  AddOnDemandConverter(dtWord, dtBytes, TDataConverters.UInt16ToBytes, TDataConverters.BytesToUInt16);
  AddOnDemandConverter(dtWord, dtVarBytes, TDataConverters.UInt16ToVarBytes, TDataConverters.VarBytesToUInt16);
  AddOnDemandConverter(dtWord, dtExtVarBytes, TDataConverters.UInt16ToExtVarBytes, TDataConverters.ExtVarBytesToUInt16);
  AddOnDemandConverter(dtWord, dtVariant, TDataConverters.UInt16ToVariant, TDataConverters.VariantToUInt16);

  AddOnDemandConverter(dtInteger, dtBoolean, TDataConverters.Int32ToBool, TDataConverters.BoolToInt32);
  AddOnDemandConverter(dtInteger, dtInt8, TDataConverters.Int32ToInt8, TDataConverters.Int8ToInt32);
  AddOnDemandConverter(dtInteger, dtUInt8, TDataConverters.Int32ToUInt8, TDataConverters.UInt8ToInt32);
  AddOnDemandConverter(dtInteger, dtSmallint, TDataConverters.Int32ToInt16, TDataConverters.Int16ToInt32);
  AddOnDemandConverter(dtInteger, dtWord, TDataConverters.Int32ToUInt16, TDataConverters.UInt16ToInt32);
  AddOnDemandConverter(dtInteger, dtUInt32, TDataConverters.Int32ToUInt32, TDataConverters.UInt32ToInt32);
  AddOnDemandConverter(dtInteger, dtInt64, TDataConverters.Int32ToInt64, TDataConverters.Int64ToInt32);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtInteger, dtUInt64, TDataConverters.Int32ToUInt64, TDataConverters.UInt64ToInt32);
{$ENDIF}
  AddOnDemandConverter(dtInteger, dtSingle, TDataConverters.Int32ToSingle, TDataConverters.SingleToInt32);
  AddOnDemandConverter(dtInteger, dtFloat, TDataConverters.Int32ToFloat, TDataConverters.FloatToInt32);
  AddOnDemandConverter(dtInteger, dtCurrency, TDataConverters.Int32ToFloat, TDataConverters.FloatToInt32);
  AddOnDemandConverter(dtInteger, dtExtended, TDataConverters.Int32ToExtended, TDataConverters.ExtendedToInt32);
  AddOnDemandConverter(dtInteger, dtBCD, TDataConverters.Int32ToBCD, TDataConverters.BCDToInt32, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtInteger, dtFMTBCD, TDataConverters.Int32ToFMTBCD, TDataConverters.FMTBCDToInt32, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtInteger, dtString, TDataConverters.Int32ToAStr, TDataConverters.AStrToInt32);
  AddOnDemandConverter(dtInteger, dtExtString, TDataConverters.Int32ToExtAStr, TDataConverters.ExtAStrToInt32);
  AddOnDemandConverter(dtInteger, dtWideString, TDataConverters.Int32ToWStr, TDataConverters.WStrToInt32);
  AddOnDemandConverter(dtInteger, dtExtWideString, TDataConverters.Int32ToExtWStr, TDataConverters.ExtWStrToInt32);
  AddOnDemandConverter(dtInteger, dtBytes, TDataConverters.Int32ToBytes, TDataConverters.BytesToInt32);
  AddOnDemandConverter(dtInteger, dtVarBytes, TDataConverters.Int32ToVarBytes, TDataConverters.VarBytesToInt32);
  AddOnDemandConverter(dtInteger, dtExtVarBytes, TDataConverters.Int32ToExtVarBytes, TDataConverters.ExtVarBytesToInt32);
  AddOnDemandConverter(dtInteger, dtVariant, TDataConverters.Int32ToVariant, TDataConverters.VariantToInt32);

  AddOnDemandConverter(dtUInt32, dtBoolean, TDataConverters.UInt32ToBool, TDataConverters.BoolToUInt32);
  AddOnDemandConverter(dtUInt32, dtInt8, TDataConverters.UInt32ToInt8, TDataConverters.Int8ToUInt32);
  AddOnDemandConverter(dtUInt32, dtUInt8, TDataConverters.UInt32ToUInt8, TDataConverters.UInt8ToUInt32);
  AddOnDemandConverter(dtUInt32, dtSmallint, TDataConverters.UInt32ToInt16, TDataConverters.Int16ToUInt32);
  AddOnDemandConverter(dtUInt32, dtWord, TDataConverters.UInt32ToUInt16, TDataConverters.UInt16ToUInt32);
  AddOnDemandConverter(dtUInt32, dtInteger, TDataConverters.UInt32ToInt32, TDataConverters.Int32ToUInt32);
  AddOnDemandConverter(dtUInt32, dtInt64, TDataConverters.UInt32ToInt64, TDataConverters.Int64ToUInt32);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtUInt32, dtUInt64, TDataConverters.UInt32ToUInt64, TDataConverters.UInt64ToUInt32);
{$ENDIF}
  AddOnDemandConverter(dtUInt32, dtSingle, TDataConverters.UInt32ToSingle, TDataConverters.SingleToUInt32);
  AddOnDemandConverter(dtUInt32, dtFloat, TDataConverters.UInt32ToFloat, TDataConverters.FloatToUInt32);
  AddOnDemandConverter(dtUInt32, dtCurrency, TDataConverters.UInt32ToFloat, TDataConverters.FloatToUInt32);
  AddOnDemandConverter(dtUInt32, dtExtended, TDataConverters.UInt32ToExtended, TDataConverters.ExtendedToUInt32);
  AddOnDemandConverter(dtUInt32, dtBCD, TDataConverters.UInt32ToBCD, TDataConverters.BCDToUInt32, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtUInt32, dtFMTBCD, TDataConverters.UInt32ToFMTBCD, TDataConverters.FMTBCDToUInt32, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtUInt32, dtString, TDataConverters.UInt32ToAStr, TDataConverters.AStrToUInt32);
  AddOnDemandConverter(dtUInt32, dtExtString, TDataConverters.UInt32ToExtAStr, TDataConverters.ExtAStrToUInt32);
  AddOnDemandConverter(dtUInt32, dtWideString, TDataConverters.UInt32ToWStr, TDataConverters.WStrToUInt32);
  AddOnDemandConverter(dtUInt32, dtExtWideString, TDataConverters.UInt32ToExtWStr, TDataConverters.ExtWStrToUInt32);
  AddOnDemandConverter(dtUInt32, dtBytes, TDataConverters.UInt32ToBytes, TDataConverters.BytesToUInt32);
  AddOnDemandConverter(dtUInt32, dtVarBytes, TDataConverters.UInt32ToVarBytes, TDataConverters.VarBytesToUInt32);
  AddOnDemandConverter(dtUInt32, dtExtVarBytes, TDataConverters.UInt32ToExtVarBytes, TDataConverters.ExtVarBytesToUInt32);
  AddOnDemandConverter(dtUInt32, dtVariant, TDataConverters.UInt32ToVariant, TDataConverters.VariantToUInt32);

  AddOnDemandConverter(dtInt64, dtBoolean, TDataConverters.Int64ToBool, TDataConverters.BoolToInt64);
  AddOnDemandConverter(dtInt64, dtInt8, TDataConverters.Int64ToInt8, TDataConverters.Int8ToInt64);
  AddOnDemandConverter(dtInt64, dtUInt8, TDataConverters.Int64ToUInt8, TDataConverters.UInt8ToInt64);
  AddOnDemandConverter(dtInt64, dtSmallint, TDataConverters.Int64ToInt16, TDataConverters.Int16ToInt64);
  AddOnDemandConverter(dtInt64, dtWord, TDataConverters.Int64ToUInt16, TDataConverters.UInt16ToInt64);
  AddOnDemandConverter(dtInt64, dtInteger, TDataConverters.Int64ToInt32, TDataConverters.Int32ToInt64);
  AddOnDemandConverter(dtInt64, dtUInt32, TDataConverters.Int64ToUInt32, TDataConverters.UInt32ToInt64);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtInt64, dtUInt64, TDataConverters.Int64ToUInt64, TDataConverters.UInt64ToInt64);
{$ENDIF}
  AddOnDemandConverter(dtInt64, dtSingle, TDataConverters.Int64ToSingle, TDataConverters.SingleToInt64);
  AddOnDemandConverter(dtInt64, dtFloat, TDataConverters.Int64ToFloat, TDataConverters.FloatToInt64);
  AddOnDemandConverter(dtInt64, dtCurrency, TDataConverters.Int64ToFloat, TDataConverters.FloatToInt64);
  AddOnDemandConverter(dtInt64, dtExtended, TDataConverters.Int64ToExtended, TDataConverters.ExtendedToInt64);
  AddOnDemandConverter(dtInt64, dtBCD, TDataConverters.Int64ToBCD, TDataConverters.BCDToInt64, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtInt64, dtFMTBCD, TDataConverters.Int64ToFMTBCD, TDataConverters.FMTBCDToInt64, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtInt64, dtString, TDataConverters.Int64ToAStr, TDataConverters.AStrToInt64);
  AddOnDemandConverter(dtInt64, dtExtString, TDataConverters.Int64ToExtAStr, TDataConverters.ExtAStrToInt64);
  AddOnDemandConverter(dtInt64, dtWideString, TDataConverters.Int64ToWStr, TDataConverters.WStrToInt64);
  AddOnDemandConverter(dtInt64, dtExtWideString, TDataConverters.Int64ToExtWStr, TDataConverters.ExtWStrToInt64);
  AddOnDemandConverter(dtInt64, dtBytes, TDataConverters.Int64ToBytes, TDataConverters.BytesToInt64);
  AddOnDemandConverter(dtInt64, dtVarBytes, TDataConverters.Int64ToVarBytes, TDataConverters.VarBytesToInt64);
  AddOnDemandConverter(dtInt64, dtExtVarBytes, TDataConverters.Int64ToExtVarBytes, TDataConverters.ExtVarBytesToInt64);
  AddOnDemandConverter(dtInt64, dtVariant, TDataConverters.Int64ToVariant, TDataConverters.VariantToInt64);

{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtUInt64, dtBoolean, TDataConverters.UInt64ToBool, TDataConverters.BoolToUInt64);
  AddOnDemandConverter(dtUInt64, dtInt8, TDataConverters.UInt64ToInt8, TDataConverters.Int8ToUInt64);
  AddOnDemandConverter(dtUInt64, dtUInt8, TDataConverters.UInt64ToUInt8, TDataConverters.UInt8ToUInt64);
  AddOnDemandConverter(dtUInt64, dtSmallint, TDataConverters.UInt64ToInt16, TDataConverters.Int16ToUInt64);
  AddOnDemandConverter(dtUInt64, dtWord, TDataConverters.UInt64ToUInt16, TDataConverters.UInt16ToUInt64);
  AddOnDemandConverter(dtUInt64, dtInteger, TDataConverters.UInt64ToInt32, TDataConverters.Int32ToUInt64);
  AddOnDemandConverter(dtUInt64, dtUInt32, TDataConverters.UInt64ToUInt32, TDataConverters.UInt32ToUInt64);
  AddOnDemandConverter(dtUInt64, dtInt64, TDataConverters.UInt64ToInt64, TDataConverters.Int64ToUInt64);
  AddOnDemandConverter(dtUInt64, dtSingle, TDataConverters.UInt64ToSingle, TDataConverters.SingleToUInt64);
  AddOnDemandConverter(dtUInt64, dtFloat, TDataConverters.UInt64ToFloat, TDataConverters.FloatToUInt64);
  AddOnDemandConverter(dtUInt64, dtCurrency, TDataConverters.UInt64ToFloat, TDataConverters.FloatToUInt64);
  AddOnDemandConverter(dtUInt64, dtExtended, TDataConverters.UInt64ToExtended, TDataConverters.ExtendedToUInt64);
  AddOnDemandConverter(dtUInt64, dtBCD, TDataConverters.UInt64ToBCD, TDataConverters.BCDToUInt64, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtUInt64, dtFMTBCD, TDataConverters.UInt64ToFMTBCD, TDataConverters.FMTBCDToUInt64, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtUInt64, dtString, TDataConverters.UInt64ToAStr, TDataConverters.AStrToUInt64);
  AddOnDemandConverter(dtUInt64, dtExtString, TDataConverters.UInt64ToExtAStr, TDataConverters.ExtAStrToUInt64);
  AddOnDemandConverter(dtUInt64, dtWideString, TDataConverters.UInt64ToWStr, TDataConverters.WStrToUInt64);
  AddOnDemandConverter(dtUInt64, dtExtWideString, TDataConverters.UInt64ToExtWStr, TDataConverters.ExtWStrToUInt64);
  AddOnDemandConverter(dtUInt64, dtBytes, TDataConverters.UInt64ToBytes, TDataConverters.BytesToUInt64);
  AddOnDemandConverter(dtUInt64, dtVarBytes, TDataConverters.UInt64ToVarBytes, TDataConverters.VarBytesToUInt64);
  AddOnDemandConverter(dtUInt64, dtExtVarBytes, TDataConverters.UInt64ToExtVarBytes, TDataConverters.ExtVarBytesToUInt64);
  AddOnDemandConverter(dtUInt64, dtVariant, TDataConverters.UInt64ToVariant, TDataConverters.VariantToUInt64);
{$ENDIF}

  AddOnDemandConverter(dtSingle, dtBoolean, TDataConverters.SingleToBool, TDataConverters.BoolToSingle);
  AddOnDemandConverter(dtSingle, dtInt8, TDataConverters.SingleToInt8, TDataConverters.Int8ToSingle);
  AddOnDemandConverter(dtSingle, dtUInt8, TDataConverters.SingleToUInt8, TDataConverters.UInt8ToSingle);
  AddOnDemandConverter(dtSingle, dtSmallint, TDataConverters.SingleToInt16, TDataConverters.Int16ToSingle);
  AddOnDemandConverter(dtSingle, dtWord, TDataConverters.SingleToUInt16, TDataConverters.UInt16ToSingle);
  AddOnDemandConverter(dtSingle, dtInteger, TDataConverters.SingleToInt32, TDataConverters.Int32ToSingle);
  AddOnDemandConverter(dtSingle, dtUInt32, TDataConverters.SingleToUInt32, TDataConverters.UInt32ToSingle);
  AddOnDemandConverter(dtSingle, dtInt64, TDataConverters.SingleToInt64, TDataConverters.Int64ToSingle);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtSingle, dtUInt64, TDataConverters.SingleToUInt64, TDataConverters.UInt64ToSingle);
{$ENDIF}
  AddOnDemandConverter(dtSingle, dtFloat, TDataConverters.SingleToFloat, TDataConverters.FloatToSingle);
  AddOnDemandConverter(dtSingle, dtCurrency, TDataConverters.SingleToFloat, TDataConverters.FloatToSingle);
  AddOnDemandConverter(dtSingle, dtExtended, TDataConverters.SingleToExtended, TDataConverters.ExtendedToSingle);
  AddOnDemandConverter(dtSingle, dtBCD, TDataConverters.SingleToBCD, TDataConverters.BCDToSingle, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtSingle, dtFMTBCD, TDataConverters.SingleToFMTBCD, TDataConverters.FMTBCDToSingle, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtSingle, dtString, TDataConverters.SingleToAStr, TDataConverters.AStrToSingle);
  AddOnDemandConverter(dtSingle, dtExtString, TDataConverters.SingleToExtAStr, TDataConverters.ExtAStrToSingle);
  AddOnDemandConverter(dtSingle, dtWideString, TDataConverters.SingleToWStr, TDataConverters.WStrToSingle);
  AddOnDemandConverter(dtSingle, dtExtWideString, TDataConverters.SingleToExtWStr, TDataConverters.ExtWStrToSingle);
  AddOnDemandConverter(dtSingle, dtBytes, TDataConverters.SingleToBytes, TDataConverters.BytesToSingle);
  AddOnDemandConverter(dtSingle, dtVarBytes, TDataConverters.SingleToVarBytes, TDataConverters.VarBytesToSingle);
  AddOnDemandConverter(dtSingle, dtExtVarBytes, TDataConverters.SingleToExtVarBytes, TDataConverters.ExtVarBytesToSingle);
  AddOnDemandConverter(dtSingle, dtVariant, TDataConverters.SingleToVariant, TDataConverters.VariantToSingle);

  AddOnDemandConverter(dtFloat, dtBoolean, TDataConverters.FloatToBool, TDataConverters.BoolToFloat);
  AddOnDemandConverter(dtFloat, dtInt8, TDataConverters.FloatToInt8, TDataConverters.Int8ToFloat);
  AddOnDemandConverter(dtFloat, dtUInt8, TDataConverters.FloatToUInt8, TDataConverters.UInt8ToFloat);
  AddOnDemandConverter(dtFloat, dtSmallint, TDataConverters.FloatToInt16, TDataConverters.Int16ToFloat);
  AddOnDemandConverter(dtFloat, dtWord, TDataConverters.FloatToUInt16, TDataConverters.UInt16ToFloat);
  AddOnDemandConverter(dtFloat, dtInteger, TDataConverters.FloatToInt32, TDataConverters.Int32ToFloat);
  AddOnDemandConverter(dtFloat, dtUInt32, TDataConverters.FloatToUInt32, TDataConverters.UInt32ToFloat);
  AddOnDemandConverter(dtFloat, dtInt64, TDataConverters.FloatToInt64, TDataConverters.Int64ToFloat);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtFloat, dtUInt64, TDataConverters.FloatToUInt64, TDataConverters.UInt64ToFloat);
{$ENDIF}
  AddOnDemandConverter(dtFloat, dtSingle, TDataConverters.FloatToSingle, TDataConverters.SingleToFloat);
  AddOnDemandConverter(dtFloat, dtCurrency, TDataConverters.CopyInt64, TDataConverters.CopyInt64);
  AddOnDemandConverter(dtFloat, dtExtended, TDataConverters.FloatToExtended, TDataConverters.ExtendedToFloat);
  AddOnDemandConverter(dtFloat, dtBCD, TDataConverters.FloatToBCD, TDataConverters.BCDToFloat, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtFloat, dtFMTBCD, TDataConverters.FloatToFMTBCD, TDataConverters.FMTBCDToFloat, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtFloat, dtString, TDataConverters.FloatToAStr, TDataConverters.AStrToFloat);
  AddOnDemandConverter(dtFloat, dtExtString, TDataConverters.FloatToExtAStr, TDataConverters.ExtAStrToFloat);
  AddOnDemandConverter(dtFloat, dtWideString, TDataConverters.FloatToWStr, TDataConverters.WStrToFloat);
  AddOnDemandConverter(dtFloat, dtExtWideString, TDataConverters.FloatToExtWStr, TDataConverters.ExtWStrToFloat);
  AddOnDemandConverter(dtFloat, dtBytes, TDataConverters.FloatToBytes, TDataConverters.BytesToFloat);
  AddOnDemandConverter(dtFloat, dtVarBytes, TDataConverters.FloatToVarBytes, TDataConverters.VarBytesToFloat);
  AddOnDemandConverter(dtFloat, dtExtVarBytes, TDataConverters.FloatToExtVarBytes, TDataConverters.ExtVarBytesToFloat);
  AddOnDemandConverter(dtFloat, dtVariant, TDataConverters.FloatToVariant, TDataConverters.VariantToFloat);

  AddOnDemandConverter(dtCurrency, dtBoolean, TDataConverters.FloatToBool, TDataConverters.BoolToFloat);
  AddOnDemandConverter(dtCurrency, dtInt8, TDataConverters.FloatToInt8, TDataConverters.Int8ToFloat);
  AddOnDemandConverter(dtCurrency, dtUInt8, TDataConverters.FloatToUInt8, TDataConverters.UInt8ToFloat);
  AddOnDemandConverter(dtCurrency, dtSmallint, TDataConverters.FloatToInt16, TDataConverters.Int16ToFloat);
  AddOnDemandConverter(dtCurrency, dtWord, TDataConverters.FloatToUInt16, TDataConverters.UInt16ToFloat);
  AddOnDemandConverter(dtCurrency, dtInteger, TDataConverters.FloatToInt32, TDataConverters.Int32ToFloat);
  AddOnDemandConverter(dtCurrency, dtUInt32, TDataConverters.FloatToUInt32, TDataConverters.UInt32ToFloat);
  AddOnDemandConverter(dtCurrency, dtInt64, TDataConverters.FloatToInt64, TDataConverters.Int64ToFloat);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtCurrency, dtUInt64, TDataConverters.FloatToUInt64, TDataConverters.UInt64ToFloat);
{$ENDIF}
  AddOnDemandConverter(dtCurrency, dtSingle, TDataConverters.FloatToSingle, TDataConverters.SingleToFloat);
  AddOnDemandConverter(dtCurrency, dtFloat, TDataConverters.CopyInt64, TDataConverters.CopyInt64);
  AddOnDemandConverter(dtCurrency, dtExtended, TDataConverters.FloatToExtended, TDataConverters.ExtendedToFloat);
  AddOnDemandConverter(dtCurrency, dtBCD, TDataConverters.CurrencyToBCD, TDataConverters.BCDToFloat, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtCurrency, dtFMTBCD, TDataConverters.FloatToFMTBCD, TDataConverters.FMTBCDToFloat, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtCurrency, dtString, TDataConverters.FloatToAStr, TDataConverters.AStrToFloat);
  AddOnDemandConverter(dtCurrency, dtExtString, TDataConverters.FloatToExtAStr, TDataConverters.ExtAStrToFloat);
  AddOnDemandConverter(dtCurrency, dtWideString, TDataConverters.FloatToWStr, TDataConverters.WStrToFloat);
  AddOnDemandConverter(dtCurrency, dtExtWideString, TDataConverters.FloatToExtWStr, TDataConverters.ExtWStrToFloat);
  AddOnDemandConverter(dtCurrency, dtBytes, TDataConverters.FloatToBytes, TDataConverters.BytesToFloat);
  AddOnDemandConverter(dtCurrency, dtVarBytes, TDataConverters.FloatToVarBytes, TDataConverters.VarBytesToFloat);
  AddOnDemandConverter(dtCurrency, dtExtVarBytes, TDataConverters.FloatToExtVarBytes, TDataConverters.ExtVarBytesToFloat);
  AddOnDemandConverter(dtCurrency, dtVariant, TDataConverters.FloatToVariant, TDataConverters.VariantToFloat);

  AddOnDemandConverter(dtExtended, dtBoolean, TDataConverters.ExtendedToBool, TDataConverters.BoolToExtended);
  AddOnDemandConverter(dtExtended, dtInt8, TDataConverters.ExtendedToInt8, TDataConverters.Int8ToExtended);
  AddOnDemandConverter(dtExtended, dtUInt8, TDataConverters.ExtendedToUInt8, TDataConverters.UInt8ToExtended);
  AddOnDemandConverter(dtExtended, dtSmallint, TDataConverters.ExtendedToInt16, TDataConverters.Int16ToExtended);
  AddOnDemandConverter(dtExtended, dtWord, TDataConverters.ExtendedToUInt16, TDataConverters.UInt16ToExtended);
  AddOnDemandConverter(dtExtended, dtInteger, TDataConverters.ExtendedToInt32, TDataConverters.Int32ToExtended);
  AddOnDemandConverter(dtExtended, dtUInt32, TDataConverters.ExtendedToUInt32, TDataConverters.UInt32ToExtended);
  AddOnDemandConverter(dtExtended, dtInt64, TDataConverters.ExtendedToInt64, TDataConverters.Int64ToExtended);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtExtended, dtUInt64, TDataConverters.ExtendedToUInt64, TDataConverters.UInt64ToExtended);
{$ENDIF}
  AddOnDemandConverter(dtExtended, dtSingle, TDataConverters.ExtendedToSingle, TDataConverters.SingleToExtended);
  AddOnDemandConverter(dtExtended, dtFloat, TDataConverters.ExtendedToFloat, TDataConverters.FloatToExtended);
  AddOnDemandConverter(dtExtended, dtCurrency, TDataConverters.ExtendedToFloat, TDataConverters.FloatToExtended);
  AddOnDemandConverter(dtExtended, dtBCD, TDataConverters.ExtendedToBCD, TDataConverters.BCDToExtended, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtended, dtFMTBCD, TDataConverters.ExtendedToFMTBCD, TDataConverters.FMTBCDToExtended, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtended, dtString, TDataConverters.ExtendedToAStr, TDataConverters.AStrToExtended);
  AddOnDemandConverter(dtExtended, dtExtString, TDataConverters.ExtendedToExtAStr, TDataConverters.ExtAStrToExtended);
  AddOnDemandConverter(dtExtended, dtWideString, TDataConverters.ExtendedToWStr, TDataConverters.WStrToExtended);
  AddOnDemandConverter(dtExtended, dtExtWideString, TDataConverters.ExtendedToExtWStr, TDataConverters.ExtWStrToExtended);
  AddOnDemandConverter(dtExtended, dtBytes, TDataConverters.ExtendedToBytes, TDataConverters.BytesToExtended);
  AddOnDemandConverter(dtExtended, dtVarBytes, TDataConverters.ExtendedToVarBytes, TDataConverters.VarBytesToExtended);
  AddOnDemandConverter(dtExtended, dtExtVarBytes, TDataConverters.ExtendedToExtVarBytes, TDataConverters.ExtVarBytesToExtended);
  AddOnDemandConverter(dtExtended, dtVariant, TDataConverters.ExtendedToVariant, TDataConverters.VariantToExtended);

  AddOnDemandConverter(dtBCD, dtBoolean, TDataConverters.BCDToBool, TDataConverters.BoolToBCD);
  AddOnDemandConverter(dtBCD, dtInt8, TDataConverters.BCDToInt8, TDataConverters.Int8ToBCD);
  AddOnDemandConverter(dtBCD, dtUInt8, TDataConverters.BCDToUInt8, TDataConverters.UInt8ToBCD);
  AddOnDemandConverter(dtBCD, dtSmallint, TDataConverters.BCDToInt16, TDataConverters.Int16ToBCD);
  AddOnDemandConverter(dtBCD, dtWord, TDataConverters.BCDToUInt16, TDataConverters.UInt16ToBCD);
  AddOnDemandConverter(dtBCD, dtInteger, TDataConverters.BCDToInt32, TDataConverters.Int32ToBCD);
  AddOnDemandConverter(dtBCD, dtUInt32, TDataConverters.BCDToUInt32, TDataConverters.UInt32ToBCD);
  AddOnDemandConverter(dtBCD, dtInt64, TDataConverters.BCDToInt64, TDataConverters.Int64ToBCD);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtBCD, dtUInt64, TDataConverters.BCDToUInt64, TDataConverters.UInt64ToBCD);
{$ENDIF}
  AddOnDemandConverter(dtBCD, dtSingle, TDataConverters.BCDToSingle, TDataConverters.SingleToBCD);
  AddOnDemandConverter(dtBCD, dtFloat, TDataConverters.BCDToFloat, TDataConverters.FloatToBCD);
  AddOnDemandConverter(dtBCD, dtCurrency, TDataConverters.BCDToFloat, TDataConverters.FloatToBCD);
  AddOnDemandConverter(dtBCD, dtExtended, TDataConverters.BCDToExtended, TDataConverters.ExtendedToBCD);
  AddOnDemandConverter(dtBCD, dtBCD, TDataConverters.BCDToBCD, TDataConverters.BCDToBCD, TSizeConverters.CopySize, TSizeConverters.CopySize);
  AddOnDemandConverter(dtBCD, dtFMTBCD, TDataConverters.BCDToFMTBCD, TDataConverters.FMTBCDToBCD, TSizeConverters.CopySize, TSizeConverters.CopySize);
  AddOnDemandConverter(dtBCD, dtString, TDataConverters.BCDToAStr, TDataConverters.AStrToBCD);
  AddOnDemandConverter(dtBCD, dtExtString, TDataConverters.BCDToExtAStr, TDataConverters.ExtAStrToBCD);
  AddOnDemandConverter(dtBCD, dtWideString, TDataConverters.BCDToWStr, TDataConverters.WStrToBCD);
  AddOnDemandConverter(dtBCD, dtExtWideString, TDataConverters.BCDToExtWStr, TDataConverters.ExtWStrToBCD);
  AddOnDemandConverter(dtBCD, dtBytes, TDataConverters.BCDToBytes, TDataConverters.BytesToBCD);
  AddOnDemandConverter(dtBCD, dtVarBytes, TDataConverters.BCDToVarBytes, TDataConverters.VarBytesToBCD);
  AddOnDemandConverter(dtBCD, dtExtVarBytes, TDataConverters.BCDToExtVarBytes, TDataConverters.ExtVarBytesToBCD);
  AddOnDemandConverter(dtBCD, dtVariant, TDataConverters.BCDToVariant, TDataConverters.VariantToBCD);

  AddOnDemandConverter(dtFMTBCD, dtBoolean, TDataConverters.FMTBCDToBool, TDataConverters.BoolToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtInt8, TDataConverters.FMTBCDToInt8, TDataConverters.Int8ToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtUInt8, TDataConverters.FMTBCDToUInt8, TDataConverters.UInt8ToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtSmallint, TDataConverters.FMTBCDToInt16, TDataConverters.Int16ToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtWord, TDataConverters.FMTBCDToUInt16, TDataConverters.UInt16ToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtInteger, TDataConverters.FMTBCDToInt32, TDataConverters.Int32ToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtUInt32, TDataConverters.FMTBCDToUInt32, TDataConverters.UInt32ToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtInt64, TDataConverters.FMTBCDToInt64, TDataConverters.Int64ToFMTBCD);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtFMTBCD, dtUInt64, TDataConverters.FMTBCDToUInt64, TDataConverters.UInt64ToFMTBCD);
{$ENDIF}
  AddOnDemandConverter(dtFMTBCD, dtSingle, TDataConverters.FMTBCDToSingle, TDataConverters.SingleToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtFloat, TDataConverters.FMTBCDToFloat, TDataConverters.FloatToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtCurrency, TDataConverters.FMTBCDToFloat, TDataConverters.FloatToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtExtended, TDataConverters.FMTBCDToExtended, TDataConverters.ExtendedToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtBCD, TDataConverters.FMTBCDToBCD, TDataConverters.BCDToFMTBCD, TSizeConverters.CopySize, TSizeConverters.CopySize);
  AddOnDemandConverter(dtFMTBCD, dtFMTBCD, TDataConverters.FMTBCDToFMTBCD, TDataConverters.FMTBCDToFMTBCD, TSizeConverters.CopySize, TSizeConverters.CopySize);
  AddOnDemandConverter(dtFMTBCD, dtString, TDataConverters.FMTBCDToAStr, TDataConverters.AStrToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtExtString, TDataConverters.FMTBCDToExtAStr, TDataConverters.ExtAStrToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtWideString, TDataConverters.FMTBCDToWStr, TDataConverters.WStrToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtExtWideString, TDataConverters.FMTBCDToExtWStr, TDataConverters.ExtWStrToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtBytes, TDataConverters.FMTBCDToBytes, TDataConverters.BytesToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtVarBytes, TDataConverters.FMTBCDToVarBytes, TDataConverters.VarBytesToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtExtVarBytes, TDataConverters.FMTBCDToExtVarBytes, TDataConverters.ExtVarBytesToFMTBCD);
  AddOnDemandConverter(dtFMTBCD, dtVariant, TDataConverters.FMTBCDToVariant, TDataConverters.VariantToFMTBCD);

  AddOnDemandConverter(dtDate, dtDateTime, TDataConverters.DateToDateTime, TDataConverters.DateTimeToDate);
  AddOnDemandConverter(dtDate, dtSQLTimeStamp, TDataConverters.DateToSQLTimeStamp, TDataConverters.SQLTimeStampToDate);
  AddOnDemandConverter(dtDate, dtSQLTimeStampOffset, TDataConverters.DateToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToDate);
  AddOnDemandConverter(dtDate, dtString, TDataConverters.DateToAStr, TDataConverters.AStrToDate);
  AddOnDemandConverter(dtDate, dtExtString, TDataConverters.DateToExtAStr, TDataConverters.ExtAStrToDate);
  AddOnDemandConverter(dtDate, dtWideString, TDataConverters.DateToWStr, TDataConverters.WStrToDate);
  AddOnDemandConverter(dtDate, dtExtWideString, TDataConverters.DateToExtWStr, TDataConverters.ExtWStrToDate);
  AddOnDemandConverter(dtDate, dtBytes, TDataConverters.DateToBytes, TDataConverters.BytesToDate);
  AddOnDemandConverter(dtDate, dtVarBytes, TDataConverters.DateToVarBytes, TDataConverters.VarBytesToDate);
  AddOnDemandConverter(dtDate, dtExtVarBytes, TDataConverters.DateToExtVarBytes, TDataConverters.ExtVarBytesToDate);
  AddOnDemandConverter(dtDate, dtVariant, TDataConverters.DateToVariant, TDataConverters.VariantToDate);

  AddOnDemandConverter(dtTime, dtDateTime, TDataConverters.TimeToDateTime, TDataConverters.DateTimeToTime);
  AddOnDemandConverter(dtTime, dtSQLTimeStamp, TDataConverters.TimeToSQLTimeStamp, TDataConverters.SQLTimeStampToTime);
  AddOnDemandConverter(dtTime, dtSQLTimeStampOffset, TDataConverters.TimeToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToTime);
  AddOnDemandConverter(dtTime, dtString, TDataConverters.TimeToAStr, TDataConverters.AStrToTime);
  AddOnDemandConverter(dtTime, dtExtString, TDataConverters.TimeToExtAStr, TDataConverters.ExtAStrToTime);
  AddOnDemandConverter(dtTime, dtWideString, TDataConverters.TimeToWStr, TDataConverters.WStrToTime);
  AddOnDemandConverter(dtTime, dtExtWideString, TDataConverters.TimeToExtWStr, TDataConverters.ExtWStrToTime);
  AddOnDemandConverter(dtTime, dtBytes, TDataConverters.TimeToBytes, TDataConverters.BytesToTime);
  AddOnDemandConverter(dtTime, dtVarBytes, TDataConverters.TimeToVarBytes, TDataConverters.VarBytesToTime);
  AddOnDemandConverter(dtTime, dtExtVarBytes, TDataConverters.TimeToExtVarBytes, TDataConverters.ExtVarBytesToTime);
  AddOnDemandConverter(dtTime, dtVariant, TDataConverters.TimeToVariant, TDataConverters.VariantToTime);

  AddOnDemandConverter(dtDateTime, dtDate, TDataConverters.DateTimeToDate, TDataConverters.DateToDateTime);
  AddOnDemandConverter(dtDateTime, dtTime, TDataConverters.DateTimeToTime, TDataConverters.TimeToDateTime);
  AddOnDemandConverter(dtDateTime, dtSQLTimeStamp, TDataConverters.DateTimeToSQLTimeStamp, TDataConverters.SQLTimeStampToDateTime);
  AddOnDemandConverter(dtDateTime, dtSQLTimeStampOffset, TDataConverters.DateTimeToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToDateTime);
  AddOnDemandConverter(dtDateTime, dtString, TDataConverters.DateTimeToAStr, TDataConverters.AStrToDateTime);
  AddOnDemandConverter(dtDateTime, dtExtString, TDataConverters.DateTimeToExtAStr, TDataConverters.ExtAStrToDateTime);
  AddOnDemandConverter(dtDateTime, dtWideString, TDataConverters.DateTimeToWStr, TDataConverters.WStrToDateTime);
  AddOnDemandConverter(dtDateTime, dtExtWideString, TDataConverters.DateTimeToExtWStr, TDataConverters.ExtWStrToDateTime);
  AddOnDemandConverter(dtDateTime, dtBytes, TDataConverters.DateTimeToBytes, TDataConverters.BytesToDateTime);
  AddOnDemandConverter(dtDateTime, dtVarBytes, TDataConverters.DateTimeToVarBytes, TDataConverters.VarBytesToDateTime);
  AddOnDemandConverter(dtDateTime, dtExtVarBytes, TDataConverters.DateTimeToExtVarBytes, TDataConverters.ExtVarBytesToDateTime);
  AddOnDemandConverter(dtDateTime, dtVariant, TDataConverters.DateTimeToVariant, TDataConverters.VariantToDateTime);

  AddOnDemandConverter(dtSQLTimeStamp, dtDate, TDataConverters.SQLTimeStampToDate, TDataConverters.DateToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtTime, TDataConverters.SQLTimeStampToTime, TDataConverters.TimeToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtDateTime, TDataConverters.SQLTimeStampToDateTime, TDataConverters.DateTimeToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtSQLTimeStampOffset, TDataConverters.SQLTimeStampToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtString, TDataConverters.SQLTimeStampToAStr, TDataConverters.AStrToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtExtString, TDataConverters.SQLTimeStampToExtAStr, TDataConverters.ExtAStrToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtWideString, TDataConverters.SQLTimeStampToWStr, TDataConverters.WStrToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtExtWideString, TDataConverters.SQLTimeStampToExtWStr, TDataConverters.ExtWStrToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtBytes, TDataConverters.SQLTimeStampToBytes, TDataConverters.BytesToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtVarBytes, TDataConverters.SQLTimeStampToVarBytes, TDataConverters.VarBytesToSQLTimeStamp);
  AddOnDemandConverter(dtSQLTimeStamp, dtExtVarBytes, TDataConverters.SQLTimeStampToExtVarBytes, TDataConverters.ExtVarBytesToSQLTimeStamp);
{$IFNDEF FPC}
  AddOnDemandConverter(dtSQLTimeStamp, dtVariant, TDataConverters.SQLTimeStampToVariant, TDataConverters.VariantToSQLTimeStamp);
{$ENDIF}

  AddOnDemandConverter(dtSQLTimeStampOffset, dtDate, TDataConverters.SQLTimeStampOffsetToDate, TDataConverters.DateToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtTime, TDataConverters.SQLTimeStampOffsetToTime, TDataConverters.TimeToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtDateTime, TDataConverters.SQLTimeStampOffsetToDateTime, TDataConverters.DateTimeToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtSQLTimeStamp, TDataConverters.SQLTimeStampOffsetToSQLTimeStamp, TDataConverters.SQLTimeStampToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtString, TDataConverters.SQLTimeStampOffsetToAStr, TDataConverters.AStrToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtExtString, TDataConverters.SQLTimeStampOffsetToExtAStr, TDataConverters.ExtAStrToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtWideString, TDataConverters.SQLTimeStampOffsetToWStr, TDataConverters.WStrToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtExtWideString, TDataConverters.SQLTimeStampOffsetToExtWStr, TDataConverters.ExtWStrToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtBytes, TDataConverters.SQLTimeStampOffsetToBytes, TDataConverters.BytesToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtVarBytes, TDataConverters.SQLTimeStampOffsetToVarBytes, TDataConverters.VarBytesToSQLTimeStampOffset);
  AddOnDemandConverter(dtSQLTimeStampOffset, dtExtVarBytes, TDataConverters.SQLTimeStampOffsetToExtVarBytes, TDataConverters.ExtVarBytesToSQLTimeStampOffset);
{$IFNDEF FPC}
  AddOnDemandConverter(dtSQLTimeStampOffset, dtVariant, TDataConverters.SQLTimeStampOffsetToVariant, TDataConverters.VariantToSQLTimeStampOffset);
{$ENDIF}

  AddOnDemandConverter(dtString, dtBoolean, TDataConverters.AStrToBool, TDataConverters.BoolToAStr);
  AddOnDemandConverter(dtString, dtInt8, TDataConverters.AStrToInt8, TDataConverters.Int8ToAStr);
  AddOnDemandConverter(dtString, dtUInt8, TDataConverters.AStrToUInt8, TDataConverters.UInt8ToAStr);
  AddOnDemandConverter(dtString, dtSmallint, TDataConverters.AStrToInt16, TDataConverters.Int16ToAStr);
  AddOnDemandConverter(dtString, dtWord, TDataConverters.AStrToUInt16, TDataConverters.UInt16ToAStr);
  AddOnDemandConverter(dtString, dtInteger, TDataConverters.AStrToInt32, TDataConverters.Int32ToAStr);
  AddOnDemandConverter(dtString, dtUInt32, TDataConverters.AStrToUInt32, TDataConverters.UInt32ToAStr);
  AddOnDemandConverter(dtString, dtInt64, TDataConverters.AStrToInt64, TDataConverters.Int64ToAStr);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtString, dtUInt64, TDataConverters.AStrToUInt64, TDataConverters.UInt64ToAStr);
{$ENDIF}
  AddOnDemandConverter(dtString, dtSingle, TDataConverters.AStrToSingle, TDataConverters.SingleToAStr);
  AddOnDemandConverter(dtString, dtFloat, TDataConverters.AStrToFloat, TDataConverters.FloatToAStr);
  AddOnDemandConverter(dtString, dtCurrency, TDataConverters.AStrToFloat, TDataConverters.FloatToAStr);
  AddOnDemandConverter(dtString, dtExtended, TDataConverters.AStrToExtended, TDataConverters.ExtendedToAStr);
  AddOnDemandConverter(dtString, dtBCD, TDataConverters.AStrToBCD, TDataConverters.BCDToAStr, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtString, dtFMTBCD, TDataConverters.AStrToFMTBCD, TDataConverters.FMTBCDToAStr, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtString, dtDate, TDataConverters.AStrToDate, TDataConverters.DateToAStr);
  AddOnDemandConverter(dtString, dtTime, TDataConverters.AStrToTime, TDataConverters.TimeToAStr);
  AddOnDemandConverter(dtString, dtDateTime, TDataConverters.AStrToDateTime, TDataConverters.DateTimeToAStr);
  AddOnDemandConverter(dtString, dtSQLTimeStamp, TDataConverters.AStrToSQLTimeStamp, TDataConverters.SQLTimeStampToAStr);
  AddOnDemandConverter(dtString, dtSQLTimeStampOffset, TDataConverters.AStrToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToAStr);
  AddOnDemandConverter(dtString, dtString, TDataConverters.AStrToAStr, TDataConverters.AStrToAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtString, dtExtString, TDataConverters.AStrToExtAStr, TDataConverters.ExtAStrToAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtString, dtWideString, TDataConverters.AStrToWStr, TDataConverters.WStrToAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtString, dtExtWideString, TDataConverters.AStrToExtWStr, TDataConverters.ExtWStrToAStr, TSizeConverters.CopySize);
//  AddOnDemandConverter(dtString, dtMemo, TDataConverters.AStrToMemo, TDataConverters.MemoToAStr);
//  AddOnDemandConverter(dtString, dtWideMemo, TDataConverters.AStrToMemo, TDataConverters.MemoToAStr);
  AddOnDemandConverter(dtString, dtBytes, TDataConverters.AStrToBytes, TDataConverters.BytesToAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtString, dtVarBytes, TDataConverters.AStrToVarBytes, TDataConverters.VarBytesToAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtString, dtExtVarBytes, TDataConverters.AStrToExtVarBytes, TDataConverters.ExtVarBytesToAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtString, dtVariant, TDataConverters.AStrToVariant, TDataConverters.VariantToAStr);
  AddOnDemandConverter(dtString, dtGuid, TDataConverters.AStrToGuid, TDataConverters.GuidToAStr, TSizeConverters.GuidSize);

  AddOnDemandConverter(dtWideString, dtBoolean, TDataConverters.WStrToBool, TDataConverters.BoolToWStr);
  AddOnDemandConverter(dtWideString, dtInt8, TDataConverters.WStrToInt8, TDataConverters.Int8ToWStr);
  AddOnDemandConverter(dtWideString, dtUInt8, TDataConverters.WStrToUInt8, TDataConverters.UInt8ToWStr);
  AddOnDemandConverter(dtWideString, dtSmallint, TDataConverters.WStrToInt16, TDataConverters.Int16ToWStr);
  AddOnDemandConverter(dtWideString, dtWord, TDataConverters.WStrToUInt16, TDataConverters.UInt16ToWStr);
  AddOnDemandConverter(dtWideString, dtInteger, TDataConverters.WStrToInt32, TDataConverters.Int32ToWStr);
  AddOnDemandConverter(dtWideString, dtUInt32, TDataConverters.WStrToUInt32, TDataConverters.UInt32ToWStr);
  AddOnDemandConverter(dtWideString, dtInt64, TDataConverters.WStrToInt64, TDataConverters.Int64ToWStr);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtWideString, dtUInt64, TDataConverters.WStrToUInt64, TDataConverters.UInt64ToWStr);
{$ENDIF}
  AddOnDemandConverter(dtWideString, dtSingle, TDataConverters.WStrToSingle, TDataConverters.SingleToWStr);
  AddOnDemandConverter(dtWideString, dtFloat, TDataConverters.WStrToFloat, TDataConverters.FloatToWStr);
  AddOnDemandConverter(dtWideString, dtCurrency, TDataConverters.WStrToFloat, TDataConverters.FloatToWStr);
  AddOnDemandConverter(dtWideString, dtExtended, TDataConverters.WStrToExtended, TDataConverters.ExtendedToWStr);
  AddOnDemandConverter(dtWideString, dtBCD, TDataConverters.WStrToBCD, TDataConverters.BCDToWStr, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtWideString, dtFMTBCD, TDataConverters.WStrToFMTBCD, TDataConverters.FMTBCDToWStr, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtWideString, dtDate, TDataConverters.WStrToDate, TDataConverters.DateToWStr);
  AddOnDemandConverter(dtWideString, dtTime, TDataConverters.WStrToTime, TDataConverters.TimeToWStr);
  AddOnDemandConverter(dtWideString, dtDateTime, TDataConverters.WStrToDateTime, TDataConverters.DateTimeToWStr);
  AddOnDemandConverter(dtWideString, dtSQLTimeStamp, TDataConverters.WStrToSQLTimeStamp, TDataConverters.SQLTimeStampToWStr);
  AddOnDemandConverter(dtWideString, dtSQLTimeStampOffset, TDataConverters.WStrToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToWStr);
  AddOnDemandConverter(dtWideString, dtString, TDataConverters.WStrToAStr, TDataConverters.AStrToWStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtWideString, dtExtString, TDataConverters.WStrToExtAStr, TDataConverters.ExtAStrToWStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtWideString, dtWideString, TDataConverters.WStrToWStr, TDataConverters.WStrToWStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtWideString, dtExtWideString, TDataConverters.WStrToExtWStr, TDataConverters.ExtWStrToWStr, TSizeConverters.CopySize);
//  AddOnDemandConverter(dtWideString, dtMemo, TDataConverters.WStrToMemo, TDataConverters.MemoToWStr);
//  AddOnDemandConverter(dtWideString, dtWideMemo, TDataConverters.WStrToWideMemo, TDataConverters.WideMemoToWStr);
  AddOnDemandConverter(dtWideString, dtBytes, TDataConverters.WStrToBytes, TDataConverters.BytesToWStr, TSizeConverters.SizeX2);
  AddOnDemandConverter(dtWideString, dtVarBytes, TDataConverters.WStrToVarBytes, TDataConverters.VarBytesToWStr, TSizeConverters.SizeX2);
  AddOnDemandConverter(dtWideString, dtExtVarBytes, TDataConverters.WStrToExtVarBytes, TDataConverters.ExtVarBytesToWStr, TSizeConverters.SizeX2);
  AddOnDemandConverter(dtWideString, dtVariant, TDataConverters.WStrToVariant, TDataConverters.VariantToWStr);
  AddOnDemandConverter(dtWideString, dtGuid, TDataConverters.WStrToGuid, TDataConverters.GuidToWStr, TSizeConverters.GuidSize);

  AddOnDemandConverter(dtExtString, dtBoolean, TDataConverters.ExtAStrToBool, TDataConverters.BoolToExtAStr);
  AddOnDemandConverter(dtExtString, dtInt8, TDataConverters.ExtAStrToInt8, TDataConverters.Int8ToExtAStr);
  AddOnDemandConverter(dtExtString, dtUInt8, TDataConverters.ExtAStrToUInt8, TDataConverters.UInt8ToExtAStr);
  AddOnDemandConverter(dtExtString, dtSmallint, TDataConverters.ExtAStrToInt16, TDataConverters.Int16ToExtAStr);
  AddOnDemandConverter(dtExtString, dtWord, TDataConverters.ExtAStrToUInt16, TDataConverters.UInt16ToExtAStr);
  AddOnDemandConverter(dtExtString, dtInteger, TDataConverters.ExtAStrToInt32, TDataConverters.Int32ToExtAStr);
  AddOnDemandConverter(dtExtString, dtUInt32, TDataConverters.ExtAStrToUInt32, TDataConverters.UInt32ToExtAStr);
  AddOnDemandConverter(dtExtString, dtInt64, TDataConverters.ExtAStrToInt64, TDataConverters.Int64ToExtAStr);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtExtString, dtUInt64, TDataConverters.ExtAStrToUInt64, TDataConverters.UInt64ToExtAStr);
{$ENDIF}
  AddOnDemandConverter(dtExtString, dtSingle, TDataConverters.ExtAStrToSingle, TDataConverters.SingleToExtAStr);
  AddOnDemandConverter(dtExtString, dtFloat, TDataConverters.ExtAStrToFloat, TDataConverters.FloatToExtAStr);
  AddOnDemandConverter(dtExtString, dtCurrency, TDataConverters.ExtAStrToFloat, TDataConverters.FloatToExtAStr);
  AddOnDemandConverter(dtExtString, dtExtended, TDataConverters.ExtAStrToExtended, TDataConverters.ExtendedToExtAStr);
  AddOnDemandConverter(dtExtString, dtBCD, TDataConverters.ExtAStrToBCD, TDataConverters.BCDToExtAStr, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtString, dtFMTBCD, TDataConverters.ExtAStrToFMTBCD, TDataConverters.FMTBCDToExtAStr, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtString, dtDate, TDataConverters.ExtAStrToDate, TDataConverters.DateToExtAStr);
  AddOnDemandConverter(dtExtString, dtTime, TDataConverters.ExtAStrToTime, TDataConverters.TimeToExtAStr);
  AddOnDemandConverter(dtExtString, dtDateTime, TDataConverters.ExtAStrToDateTime, TDataConverters.DateTimeToExtAStr);
  AddOnDemandConverter(dtExtString, dtSQLTimeStamp, TDataConverters.ExtAStrToSQLTimeStamp, TDataConverters.SQLTimeStampToExtAStr);
  AddOnDemandConverter(dtExtString, dtSQLTimeStampOffset, TDataConverters.ExtAStrToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToExtAStr);
  AddOnDemandConverter(dtExtString, dtString, TDataConverters.ExtAStrToAStr, TDataConverters.AStrToExtAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtString, dtExtString, TDataConverters.ExtAStrToExtAStr, TDataConverters.ExtAStrToExtAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtString, dtWideString, TDataConverters.ExtAStrToWStr, TDataConverters.WStrToExtAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtString, dtExtWideString, TDataConverters.ExtAStrToExtWStr, TDataConverters.ExtWStrToExtAStr, TSizeConverters.CopySize);
//  AddOnDemandConverter(dtExtString, dtMemo, TDataConverters.ExtAStrToMemo, TDataConverters.MemoToExtAStr);
//  AddOnDemandConverter(dtExtString, dtWideMemo, TDataConverters.ExtAStrToWideMemo, TDataConverters.WideMemoToExtAStr);
  AddOnDemandConverter(dtExtString, dtBytes, TDataConverters.ExtAStrToBytes, TDataConverters.BytesToExtAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtString, dtVarBytes, TDataConverters.ExtAStrToVarBytes, TDataConverters.VarBytesToExtAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtString, dtExtVarBytes, TDataConverters.ExtAStrToExtVarBytes, TDataConverters.ExtVarBytesToExtAStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtString, dtVariant, TDataConverters.ExtAStrToVariant, TDataConverters.VariantToExtAStr);
  AddOnDemandConverter(dtExtString, dtGuid, TDataConverters.ExtAStrToGuid, TDataConverters.GuidToExtAStr, TSizeConverters.GuidSize);

  AddOnDemandConverter(dtExtWideString, dtBoolean, TDataConverters.ExtWStrToBool, TDataConverters.BoolToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtInt8, TDataConverters.ExtWStrToInt8, TDataConverters.Int8ToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtUInt8, TDataConverters.ExtWStrToUInt8, TDataConverters.UInt8ToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtSmallint, TDataConverters.ExtWStrToInt16, TDataConverters.Int16ToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtWord, TDataConverters.ExtWStrToUInt16, TDataConverters.UInt16ToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtInteger, TDataConverters.ExtWStrToInt32, TDataConverters.Int32ToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtUInt32, TDataConverters.ExtWStrToUInt32, TDataConverters.UInt32ToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtInt64, TDataConverters.ExtWStrToInt64, TDataConverters.Int64ToExtWStr);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtExtWideString, dtUInt64, TDataConverters.ExtWStrToUInt64, TDataConverters.UInt64ToExtWStr);
{$ENDIF}
  AddOnDemandConverter(dtExtWideString, dtSingle, TDataConverters.ExtWStrToSingle, TDataConverters.SingleToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtFloat, TDataConverters.ExtWStrToFloat, TDataConverters.FloatToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtCurrency, TDataConverters.ExtWStrToFloat, TDataConverters.FloatToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtExtended, TDataConverters.ExtWStrToExtended, TDataConverters.ExtendedToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtBCD, TDataConverters.ExtWStrToBCD, TDataConverters.BCDToExtWStr, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtWideString, dtFMTBCD, TDataConverters.ExtWStrToFMTBCD, TDataConverters.FMTBCDToExtWStr, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtWideString, dtDate, TDataConverters.ExtWStrToDate, TDataConverters.DateToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtTime, TDataConverters.ExtWStrToTime, TDataConverters.TimeToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtDateTime, TDataConverters.ExtWStrToDateTime, TDataConverters.DateTimeToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtSQLTimeStamp, TDataConverters.ExtWStrToSQLTimeStamp, TDataConverters.SQLTimeStampToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtSQLTimeStampOffset, TDataConverters.ExtWStrToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtString, TDataConverters.ExtWStrToAStr, TDataConverters.AStrToExtWStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtWideString, dtExtString, TDataConverters.ExtWStrToExtAStr, TDataConverters.ExtAStrToExtWStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtWideString, dtWideString, TDataConverters.ExtWStrToWStr, TDataConverters.WStrToExtWStr, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtWideString, dtExtWideString, TDataConverters.ExtWStrToExtWStr, TDataConverters.ExtWStrToExtWStr, TSizeConverters.CopySize);
//  AddOnDemandConverter(dtExtWideString, dtMemo, TDataConverters.ExtWStrToMemo, TDataConverters.MemoToExtWStr);
//  AddOnDemandConverter(dtExtWideString, dtWideMemo, TDataConverters.ExtWStrToWideMemo, TDataConverters.WideMemoToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtBytes, TDataConverters.ExtWStrToBytes, TDataConverters.BytesToExtWStr, TSizeConverters.SizeX2);
  AddOnDemandConverter(dtExtWideString, dtVarBytes, TDataConverters.ExtWStrToVarBytes, TDataConverters.VarBytesToExtWStr, TSizeConverters.SizeX2);
  AddOnDemandConverter(dtExtWideString, dtExtVarBytes, TDataConverters.ExtWStrToExtVarBytes, TDataConverters.ExtVarBytesToExtWStr, TSizeConverters.SizeX2);
  AddOnDemandConverter(dtExtWideString, dtVariant, TDataConverters.ExtWStrToVariant, TDataConverters.VariantToExtWStr);
  AddOnDemandConverter(dtExtWideString, dtGuid, TDataConverters.ExtWStrToGuid, TDataConverters.GuidToExtWStr, TSizeConverters.GuidSize);

  AddOnDemandConverter(dtMemo, dtString, TDataConverters.MemoToAStr, TDataConverters.AStrToMemo);
  AddOnDemandConverter(dtMemo, dtWideString, TDataConverters.MemoToWStr, TDataConverters.WStrToMemo);
  AddOnDemandConverter(dtMemo, dtExtString, TDataConverters.MemoToExtAStr, TDataConverters.ExtAStrToMemo);
  AddOnDemandConverter(dtMemo, dtExtWideString, TDataConverters.MemoToExtWStr, TDataConverters.ExtWStrToMemo);
  AddOnDemandConverter(dtMemo, dtWideMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtMemo, dtBytes, TDataConverters.BlobToBytes, TDataConverters.BytesToBlob);

  AddOnDemandConverter(dtWideMemo, dtString, TDataConverters.WideMemoToAStr, TDataConverters.AStrToWideMemo);
  AddOnDemandConverter(dtWideMemo, dtWideString, TDataConverters.WideMemoToWStr, TDataConverters.WStrToWideMemo);
  AddOnDemandConverter(dtWideMemo, dtExtString, TDataConverters.WideMemoToExtAStr, TDataConverters.ExtAStrToWideMemo);
  AddOnDemandConverter(dtWideMemo, dtExtWideString, TDataConverters.WideMemoToExtWStr, TDataConverters.ExtWStrToWideMemo);
  AddOnDemandConverter(dtWideMemo, dtMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtWideMemo, dtBytes, TDataConverters.BlobToBytes, TDataConverters.BytesToBlob);

  AddOnDemandConverter(dtBlob, dtBoolean, TDataConverters.BlobToBool, TDataConverters.BoolToBlob);
  AddOnDemandConverter(dtBlob, dtInt8, TDataConverters.BlobToInt8, TDataConverters.Int8ToBlob);
  AddOnDemandConverter(dtBlob, dtUInt8, TDataConverters.BlobToUInt8, TDataConverters.UInt8ToBlob);
  AddOnDemandConverter(dtBlob, dtInt16, TDataConverters.BlobToInt16, TDataConverters.Int16ToBlob);
  AddOnDemandConverter(dtBlob, dtUInt16, TDataConverters.BlobToUInt16, TDataConverters.UInt16ToBlob);
  AddOnDemandConverter(dtBlob, dtInt32, TDataConverters.BlobToInt32, TDataConverters.Int32ToBlob);
  AddOnDemandConverter(dtBlob, dtUInt32, TDataConverters.BlobToUInt32, TDataConverters.UInt32ToBlob);
  AddOnDemandConverter(dtBlob, dtInt64, TDataConverters.BlobToInt64, TDataConverters.Int64ToBlob);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtBlob, dtUInt64, TDataConverters.BlobToUInt64, TDataConverters.UInt64ToBlob);
{$ENDIF}
  AddOnDemandConverter(dtBlob, dtSingle, TDataConverters.BlobToSingle, TDataConverters.SingleToBlob);
  AddOnDemandConverter(dtBlob, dtFloat, TDataConverters.BlobToFloat, TDataConverters.FloatToBlob);
  AddOnDemandConverter(dtBlob, dtCurrency, TDataConverters.BlobToFloat, TDataConverters.FloatToBlob);
  AddOnDemandConverter(dtBlob, dtExtended, TDataConverters.BlobToExtended, TDataConverters.ExtendedToBlob);
  AddOnDemandConverter(dtBlob, dtBCD, TDataConverters.BlobToBCD, TDataConverters.BCDToBlob);
  AddOnDemandConverter(dtBlob, dtFMTBCD, TDataConverters.BlobToFMTBCD, TDataConverters.FMTBCDToBlob);
  AddOnDemandConverter(dtBlob, dtDate, TDataConverters.BlobToDate, TDataConverters.DateToBlob);
  AddOnDemandConverter(dtBlob, dtTime, TDataConverters.BlobToTime, TDataConverters.TimeToBlob);
  AddOnDemandConverter(dtBlob, dtDateTime, TDataConverters.BlobToDateTime, TDataConverters.DateTimeToBlob);
  AddOnDemandConverter(dtBlob, dtSQLTimeStamp, TDataConverters.BlobToSQLTimeStamp, TDataConverters.SQLTimeStampToBlob);
  AddOnDemandConverter(dtBlob, dtSQLTimeStampOffset, TDataConverters.BlobToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToBlob);
  AddOnDemandConverter(dtBlob, dtString, TDataConverters.MemoToAStr, TDataConverters.AStrToMemo);
  AddOnDemandConverter(dtBlob, dtExtString, TDataConverters.MemoToExtAStr, TDataConverters.ExtAStrToMemo);
  AddOnDemandConverter(dtBlob, dtWideString, TDataConverters.MemoToWStr, TDataConverters.WStrToMemo);
  AddOnDemandConverter(dtBlob, dtExtWideString, TDataConverters.MemoToExtWStr, TDataConverters.ExtWStrToMemo);
  AddOnDemandConverter(dtBlob, dtMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtBlob, dtWideMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtBlob, dtBytes, TDataConverters.BlobToBytes, TDataConverters.BytesToBlob);
  AddOnDemandConverter(dtBlob, dtVarBytes, TDataConverters.BlobToVarBytes, TDataConverters.VarBytesToBlob);
  AddOnDemandConverter(dtBlob, dtExtVarBytes, TDataConverters.BlobToExtVarBytes, TDataConverters.ExtVarBytesToBlob);
  AddOnDemandConverter(dtBlob, dtVariant, TDataConverters.BlobToVariant, TDataConverters.VariantToBlob);

  AddOnDemandConverter(dtBytes, dtBoolean, TDataConverters.BytesToBool, TDataConverters.BoolToBytes);
  AddOnDemandConverter(dtBytes, dtInt8, TDataConverters.BytesToInt8, TDataConverters.Int8ToBytes);
  AddOnDemandConverter(dtBytes, dtUInt8, TDataConverters.BytesToUInt8, TDataConverters.UInt8ToBytes);
  AddOnDemandConverter(dtBytes, dtSmallint, TDataConverters.BytesToInt16, TDataConverters.Int16ToBytes);
  AddOnDemandConverter(dtBytes, dtWord, TDataConverters.BytesToUInt16, TDataConverters.UInt16ToBytes);
  AddOnDemandConverter(dtBytes, dtInteger, TDataConverters.BytesToInt32, TDataConverters.Int32ToBytes);
  AddOnDemandConverter(dtBytes, dtUInt32, TDataConverters.BytesToUInt32, TDataConverters.UInt32ToBytes);
  AddOnDemandConverter(dtBytes, dtInt64, TDataConverters.BytesToInt64, TDataConverters.Int64ToBytes);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtBytes, dtUInt64, TDataConverters.BytesToUInt64, TDataConverters.UInt64ToBytes);
{$ENDIF}
  AddOnDemandConverter(dtBytes, dtSingle, TDataConverters.BytesToSingle, TDataConverters.SingleToBytes);
  AddOnDemandConverter(dtBytes, dtFloat, TDataConverters.BytesToFloat, TDataConverters.FloatToBytes);
  AddOnDemandConverter(dtBytes, dtCurrency, TDataConverters.BytesToFloat, TDataConverters.FloatToBytes);
  AddOnDemandConverter(dtBytes, dtExtended, TDataConverters.BytesToExtended, TDataConverters.ExtendedToBytes);
  AddOnDemandConverter(dtBytes, dtBCD, TDataConverters.BytesToBCD, TDataConverters.BCDToBytes, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtBytes, dtFMTBCD, TDataConverters.BytesToFMTBCD, TDataConverters.FMTBCDToBytes, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtBytes, dtDate, TDataConverters.BytesToDate, TDataConverters.DateToBytes);
  AddOnDemandConverter(dtBytes, dtTime, TDataConverters.BytesToTime, TDataConverters.TimeToBytes);
  AddOnDemandConverter(dtBytes, dtDateTime, TDataConverters.BytesToDateTime, TDataConverters.DateTimeToBytes);
  AddOnDemandConverter(dtBytes, dtSQLTimeStamp, TDataConverters.BytesToSQLTimeStamp, TDataConverters.SQLTimeStampToBytes);
  AddOnDemandConverter(dtBytes, dtSQLTimeStampOffset, TDataConverters.BytesToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToBytes);
  AddOnDemandConverter(dtBytes, dtString, TDataConverters.BytesToAStr, TDataConverters.AStrToBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtBytes, dtExtString, TDataConverters.BytesToExtAStr, TDataConverters.ExtAStrToBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtBytes, dtWideString, TDataConverters.BytesToWStr, TDataConverters.WStrToBytes, TSizeConverters.SizeDiv2);
  AddOnDemandConverter(dtBytes, dtExtWideString, TDataConverters.BytesToExtWStr, TDataConverters.ExtWStrToBytes, TSizeConverters.SizeDiv2);
  AddOnDemandConverter(dtBytes, dtBytes, TDataConverters.BytesToBytes, TDataConverters.BytesToBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtBytes, dtVarBytes, TDataConverters.BytesToVarBytes, TDataConverters.VarBytesToBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtBytes, dtExtBytes, TDataConverters.BytesToExtBytes, TDataConverters.ExtBytesToBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtBytes, dtExtVarBytes, TDataConverters.BytesToExtVarBytes, TDataConverters.ExtVarBytesToBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtBytes, dtVariant, TDataConverters.BytesToVariant, TDataConverters.VariantToBytes);
  AddOnDemandConverter(dtBytes, dtBlob, TDataConverters.BytesToBlob, TDataConverters.BlobToBytes);
  AddOnDemandConverter(dtBytes, dtGuid, TDataConverters.BytesToGuid, TDataConverters.GuidToBytes, TSizeConverters.GuidSize);

  AddOnDemandConverter(dtVarBytes, dtBoolean, TDataConverters.VarBytesToBool, TDataConverters.BoolToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtInt8, TDataConverters.VarBytesToInt8, TDataConverters.Int8ToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtUInt8, TDataConverters.VarBytesToUInt8, TDataConverters.UInt8ToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtSmallint, TDataConverters.VarBytesToInt16, TDataConverters.Int16ToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtWord, TDataConverters.VarBytesToUInt16, TDataConverters.UInt16ToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtInteger, TDataConverters.VarBytesToInt32, TDataConverters.Int32ToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtUInt32, TDataConverters.VarBytesToUInt32, TDataConverters.UInt32ToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtInt64, TDataConverters.VarBytesToInt64, TDataConverters.Int64ToVarBytes);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtVarBytes, dtUInt64, TDataConverters.VarBytesToUInt64, TDataConverters.UInt64ToVarBytes);
{$ENDIF}
  AddOnDemandConverter(dtVarBytes, dtSingle, TDataConverters.VarBytesToSingle, TDataConverters.SingleToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtFloat, TDataConverters.VarBytesToFloat, TDataConverters.FloatToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtCurrency, TDataConverters.VarBytesToFloat, TDataConverters.FloatToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtExtended, TDataConverters.VarBytesToExtended, TDataConverters.ExtendedToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtBCD, TDataConverters.VarBytesToBCD, TDataConverters.BCDToVarBytes, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtVarBytes, dtFMTBCD, TDataConverters.VarBytesToFMTBCD, TDataConverters.FMTBCDToVarBytes, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtVarBytes, dtDate, TDataConverters.VarBytesToDate, TDataConverters.DateToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtTime, TDataConverters.VarBytesToTime, TDataConverters.TimeToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtDateTime, TDataConverters.VarBytesToDateTime, TDataConverters.DateTimeToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtSQLTimeStamp, TDataConverters.VarBytesToSQLTimeStamp, TDataConverters.SQLTimeStampToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtSQLTimeStampOffset, TDataConverters.VarBytesToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtString, TDataConverters.VarBytesToAStr, TDataConverters.AStrToVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtVarBytes, dtExtString, TDataConverters.VarBytesToExtAStr, TDataConverters.ExtAStrToVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtVarBytes, dtWideString, TDataConverters.VarBytesToWStr, TDataConverters.WStrToVarBytes, TSizeConverters.SizeDiv2);
  AddOnDemandConverter(dtVarBytes, dtExtWideString, TDataConverters.VarBytesToExtWStr, TDataConverters.ExtWStrToVarBytes, TSizeConverters.SizeDiv2);
  AddOnDemandConverter(dtVarBytes, dtBytes, TDataConverters.VarBytesToBytes, TDataConverters.BytesToVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtVarBytes, dtVarBytes, TDataConverters.VarBytesToVarBytes, TDataConverters.VarBytesToVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtVarBytes, dtExtBytes, TDataConverters.VarBytesToExtBytes, TDataConverters.ExtBytesToVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtVarBytes, dtExtVarBytes, TDataConverters.VarBytesToExtVarBytes, TDataConverters.ExtVarBytesToVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtVarBytes, dtVariant, TDataConverters.VarBytesToVariant, TDataConverters.VariantToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtBlob, TDataConverters.VarBytesToBlob, TDataConverters.BlobToVarBytes);
  AddOnDemandConverter(dtVarBytes, dtGuid, TDataConverters.VarBytesToGuid, TDataConverters.GuidToVarBytes, TSizeConverters.GuidSize);

  AddOnDemandConverter(dtExtBytes, dtBoolean, TDataConverters.ExtBytesToBool, TDataConverters.BoolToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtInt8, TDataConverters.ExtBytesToInt8, TDataConverters.Int8ToVarBytes);
  AddOnDemandConverter(dtExtBytes, dtUInt8, TDataConverters.ExtBytesToUInt8, TDataConverters.UInt8ToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtSmallint, TDataConverters.ExtBytesToInt16, TDataConverters.Int16ToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtWord, TDataConverters.ExtBytesToUInt16, TDataConverters.UInt16ToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtInteger, TDataConverters.ExtBytesToInt32, TDataConverters.Int32ToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtUInt32, TDataConverters.ExtBytesToUInt32, TDataConverters.UInt32ToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtInt64, TDataConverters.ExtBytesToInt64, TDataConverters.Int64ToExtBytes);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtExtBytes, dtUInt64, TDataConverters.ExtBytesToUInt64, TDataConverters.UInt64ToExtBytes);
{$ENDIF}
  AddOnDemandConverter(dtExtBytes, dtSingle, TDataConverters.ExtBytesToSingle, TDataConverters.SingleToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtFloat, TDataConverters.ExtBytesToFloat, TDataConverters.FloatToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtCurrency, TDataConverters.ExtBytesToFloat, TDataConverters.FloatToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtExtended, TDataConverters.ExtBytesToExtended, TDataConverters.ExtendedToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtBCD, TDataConverters.ExtBytesToBCD, TDataConverters.BCDToExtBytes, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtBytes, dtFMTBCD, TDataConverters.ExtBytesToFMTBCD, TDataConverters.FMTBCDToExtBytes, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtBytes, dtDate, TDataConverters.ExtBytesToDate, TDataConverters.DateToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtTime, TDataConverters.ExtBytesToTime, TDataConverters.TimeToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtDateTime, TDataConverters.ExtBytesToDateTime, TDataConverters.DateTimeToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtSQLTimeStamp, TDataConverters.ExtBytesToSQLTimeStamp, TDataConverters.SQLTimeStampToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtSQLTimeStampOffset, TDataConverters.ExtBytesToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtString, TDataConverters.ExtBytesToAStr, TDataConverters.AStrToExtBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtBytes, dtExtString, TDataConverters.ExtBytesToExtAStr, TDataConverters.ExtAStrToExtBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtBytes, dtWideString, TDataConverters.ExtBytesToWStr, TDataConverters.WStrToExtBytes, TSizeConverters.SizeDiv2);
  AddOnDemandConverter(dtExtBytes, dtExtWideString, TDataConverters.ExtBytesToExtWStr, TDataConverters.ExtWStrToExtBytes, TSizeConverters.SizeDiv2);
  AddOnDemandConverter(dtExtBytes, dtBytes, TDataConverters.ExtBytesToBytes, TDataConverters.BytesToExtBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtBytes, dtVarBytes, TDataConverters.ExtBytesToVarBytes, TDataConverters.VarBytesToExtBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtBytes, dtExtBytes, TDataConverters.ExtBytesToExtBytes, TDataConverters.ExtBytesToExtBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtBytes, dtExtVarBytes, TDataConverters.ExtBytesToExtVarBytes, TDataConverters.ExtVarBytesToExtBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtBytes, dtVariant, TDataConverters.ExtBytesToVariant, TDataConverters.VariantToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtBlob, TDataConverters.ExtBytesToBlob, TDataConverters.BlobToExtBytes);
  AddOnDemandConverter(dtExtBytes, dtGuid, TDataConverters.ExtBytesToGuid, TDataConverters.GuidToExtBytes, TSizeConverters.GuidSize);

  AddOnDemandConverter(dtExtVarBytes, dtBoolean, TDataConverters.ExtVarBytesToBool, TDataConverters.BoolToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtInt8, TDataConverters.ExtVarBytesToInt8, TDataConverters.Int8ToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtUInt8, TDataConverters.ExtVarBytesToUInt8, TDataConverters.UInt8ToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtSmallint, TDataConverters.ExtVarBytesToInt16, TDataConverters.Int16ToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtWord, TDataConverters.ExtVarBytesToUInt16, TDataConverters.UInt16ToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtInteger, TDataConverters.ExtVarBytesToInt32, TDataConverters.Int32ToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtUInt32, TDataConverters.ExtVarBytesToUInt32, TDataConverters.UInt32ToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtInt64, TDataConverters.ExtVarBytesToInt64, TDataConverters.Int64ToExtVarBytes);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtExtVarBytes, dtUInt64, TDataConverters.ExtVarBytesToUInt64, TDataConverters.UInt64ToExtVarBytes);
{$ENDIF}
  AddOnDemandConverter(dtExtVarBytes, dtSingle, TDataConverters.ExtVarBytesToSingle, TDataConverters.SingleToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtFloat, TDataConverters.ExtVarBytesToFloat, TDataConverters.FloatToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtCurrency, TDataConverters.ExtVarBytesToFloat, TDataConverters.FloatToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtExtended, TDataConverters.ExtVarBytesToExtended, TDataConverters.ExtendedToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtBCD, TDataConverters.ExtVarBytesToBCD, TDataConverters.BCDToExtVarBytes, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtVarBytes, dtFMTBCD, TDataConverters.ExtVarBytesToFMTBCD, TDataConverters.FMTBCDToExtVarBytes, TSizeConverters.Size0, TSizeConverters.Size0);
  AddOnDemandConverter(dtExtVarBytes, dtDate, TDataConverters.ExtVarBytesToDate, TDataConverters.DateToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtTime, TDataConverters.ExtVarBytesToTime, TDataConverters.TimeToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtDateTime, TDataConverters.ExtVarBytesToDateTime, TDataConverters.DateTimeToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtSQLTimeStamp, TDataConverters.ExtVarBytesToSQLTimeStamp, TDataConverters.SQLTimeStampToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtSQLTimeStampOffset, TDataConverters.ExtVarBytesToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtString, TDataConverters.ExtVarBytesToAStr, TDataConverters.AStrToExtVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtVarBytes, dtExtString, TDataConverters.ExtVarBytesToExtAStr, TDataConverters.ExtAStrToExtVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtVarBytes, dtWideString, TDataConverters.ExtVarBytesToWStr, TDataConverters.WStrToExtVarBytes, TSizeConverters.SizeDiv2);
  AddOnDemandConverter(dtExtVarBytes, dtExtWideString, TDataConverters.ExtVarBytesToExtWStr, TDataConverters.ExtWStrToExtVarBytes, TSizeConverters.SizeDiv2);
  AddOnDemandConverter(dtExtVarBytes, dtBytes, TDataConverters.ExtVarBytesToBytes, TDataConverters.BytesToExtVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtVarBytes, dtVarBytes, TDataConverters.ExtVarBytesToVarBytes, TDataConverters.VarBytesToExtVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtVarBytes, dtExtBytes, TDataConverters.ExtVarBytesToExtBytes, TDataConverters.ExtBytesToExtVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtVarBytes, dtExtVarBytes, TDataConverters.ExtVarBytesToExtVarBytes, TDataConverters.ExtVarBytesToExtVarBytes, TSizeConverters.CopySize);
  AddOnDemandConverter(dtExtVarBytes, dtVariant, TDataConverters.ExtVarBytesToVariant, TDataConverters.VariantToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtBlob, TDataConverters.ExtVarBytesToBlob, TDataConverters.BlobToExtVarBytes);
  AddOnDemandConverter(dtExtVarBytes, dtGuid, TDataConverters.ExtVarBytesToGuid, TDataConverters.GuidToExtVarBytes, TSizeConverters.GuidSize);

  AddOnDemandConverter(dtVariant, dtBoolean, TDataConverters.VariantToBool, TDataConverters.BoolToVariant);
  AddOnDemandConverter(dtVariant, dtInt8, TDataConverters.VariantToInt8, TDataConverters.Int8ToVariant);
  AddOnDemandConverter(dtVariant, dtUInt8, TDataConverters.VariantToUInt8, TDataConverters.UInt8ToVariant);
  AddOnDemandConverter(dtVariant, dtSmallint, TDataConverters.VariantToInt16, TDataConverters.Int16ToVariant);
  AddOnDemandConverter(dtVariant, dtWord, TDataConverters.VariantToUInt16, TDataConverters.UInt16ToVariant);
  AddOnDemandConverter(dtVariant, dtInteger, TDataConverters.VariantToInt32, TDataConverters.Int32ToVariant);
  AddOnDemandConverter(dtVariant, dtUInt32, TDataConverters.VariantToUInt32, TDataConverters.UInt32ToVariant);
  AddOnDemandConverter(dtVariant, dtInt64, TDataConverters.VariantToInt64, TDataConverters.Int64ToVariant);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtVariant, dtUInt64, TDataConverters.VariantToUInt64, TDataConverters.UInt64ToVariant);
{$ENDIF}
  AddOnDemandConverter(dtVariant, dtSingle, TDataConverters.VariantToSingle, TDataConverters.SingleToVariant);
  AddOnDemandConverter(dtVariant, dtFloat, TDataConverters.VariantToFloat, TDataConverters.FloatToVariant);
  AddOnDemandConverter(dtVariant, dtCurrency, TDataConverters.VariantToFloat, TDataConverters.FloatToVariant);
  AddOnDemandConverter(dtVariant, dtExtended, TDataConverters.VariantToExtended, TDataConverters.ExtendedToVariant);
  AddOnDemandConverter(dtVariant, dtBCD, TDataConverters.VariantToBCD, TDataConverters.BCDToVariant);
  AddOnDemandConverter(dtVariant, dtFMTBCD, TDataConverters.VariantToFMTBCD, TDataConverters.FMTBCDToVariant);
  AddOnDemandConverter(dtVariant, dtDate, TDataConverters.VariantToDate, TDataConverters.DateToVariant);
  AddOnDemandConverter(dtVariant, dtTime, TDataConverters.VariantToTime, TDataConverters.TimeToVariant);
  AddOnDemandConverter(dtVariant, dtDateTime, TDataConverters.VariantToDateTime, TDataConverters.DateTimeToVariant);
{$IFNDEF FPC}
  AddOnDemandConverter(dtVariant, dtSQLTimeStamp, TDataConverters.VariantToSQLTimeStamp, TDataConverters.SQLTimeStampToVariant);
  AddOnDemandConverter(dtVariant, dtSQLTimeStampOffset, TDataConverters.VariantToSQLTimeStampOffset, TDataConverters.SQLTimeStampOffsetToVariant);
{$ENDIF}
  AddOnDemandConverter(dtVariant, dtString, TDataConverters.VariantToAStr, TDataConverters.AStrToVariant);
  AddOnDemandConverter(dtVariant, dtExtString, TDataConverters.VariantToExtAStr, TDataConverters.ExtAStrToVariant);
  AddOnDemandConverter(dtVariant, dtWideString, TDataConverters.VariantToWStr, TDataConverters.WStrToVariant);
  AddOnDemandConverter(dtVariant, dtExtWideString, TDataConverters.VariantToExtWStr, TDataConverters.ExtWStrToVariant);
  AddOnDemandConverter(dtVariant, dtBytes, TDataConverters.VariantToBytes, TDataConverters.BytesToVariant);
  AddOnDemandConverter(dtVariant, dtVarBytes, TDataConverters.VariantToVarBytes, TDataConverters.VarBytesToVariant);
  AddOnDemandConverter(dtVariant, dtExtVarBytes, TDataConverters.VariantToExtVarBytes, TDataConverters.ExtVarBytesToVariant);

  AddOnDemandConverter(dtGuid, dtString, TDataConverters.GuidToAStr, TDataConverters.AStrToGuid, TSizeConverters.GuidSize);
  AddOnDemandConverter(dtGuid, dtExtString, TDataConverters.GuidToExtAStr, TDataConverters.ExtAStrToGuid, TSizeConverters.GuidSize);
  AddOnDemandConverter(dtGuid, dtWideString, TDataConverters.GuidToWStr, TDataConverters.WStrToGuid, TSizeConverters.GuidSize);
  AddOnDemandConverter(dtGuid, dtExtWideString, TDataConverters.GuidToExtWStr, TDataConverters.ExtWStrToGuid, TSizeConverters.GuidSize);
  AddOnDemandConverter(dtGuid, dtBytes, TDataConverters.GuidToBytes, TDataConverters.BytesToGuid, TSizeConverters.GuidSize);
  AddOnDemandConverter(dtGuid, dtVarBytes, TDataConverters.GuidToVarBytes, TDataConverters.VarBytesToGuid, TSizeConverters.GuidSize);
  AddOnDemandConverter(dtGuid, dtExtVarBytes, TDataConverters.GuidToExtVarBytes, TDataConverters.ExtVarBytesToGuid, TSizeConverters.GuidSize);
end;

destructor TConverterManager.Destroy;
begin
  ClearFetchMappers;
  FFetchConverters.Free;

  ClearOnDemandConverters;
  FOnDemandConverters.Free;

  inherited;
end;

class function TConverterManager.GetDBProvider: Word;
begin
  Result := 0;
end;

procedure TConverterManager.AddFetchConverter(FetchConverter: TFetchConverter);
begin
  FFetchConverters.Add(FetchConverter);
end;

procedure TConverterManager.AddFetchConverter(DBType, RequiredDataType: Word);
var
  FetchConverter: TFetchConverter;
begin
  FetchConverter := TFetchConverter.Create(DBType, -1, -1, -1, -1, RequiredDataType, RequiredDataType, nil, nil);
  AddFetchConverter(FetchConverter);
end;

procedure TConverterManager.AddFetchConverter(DBType, RequiredDataType, DestDataType: Word; CalcLength: TCalcSizeFunc = nil; CalcScale: TCalcSizeFunc = nil);
var
  FetchConverter: TFetchConverter;
begin
  FetchConverter := TFetchConverter.Create(DBType, -1, -1, -1, -1, RequiredDataType, DestDataType, CalcLength, CalcScale);
  AddFetchConverter(FetchConverter);
end;

procedure TConverterManager.AddFetchConverter(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: integer; RequiredDataType: Word);
var
  FetchConverter: TFetchConverter;
begin
  FetchConverter := TFetchConverter.Create(DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, RequiredDataType, RequiredDataType, nil, nil);
  AddFetchConverter(FetchConverter)
end;

procedure TConverterManager.AddFetchConverter(DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax: integer; RequiredDataType, DestDataType: Word; CalcLength: TCalcSizeFunc = nil; CalcScale: TCalcSizeFunc = nil);
var
  FetchConverter: TFetchConverter;
begin
  FetchConverter := TFetchConverter.Create(DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, RequiredDataType, DestDataType, CalcLength, CalcScale);
  AddFetchConverter(FetchConverter)
end;

procedure TConverterManager.ClearFetchMappers;
begin
  FFetchConverters.Clear;
end;

procedure TConverterManager.AddOnDemandConverter(OnDemandConverter: TOnDemandConverter);
begin
  FOnDemandConverters.Add(OnDemandConverter);
end;

procedure TConverterManager.AddOnDemandConverter(SourceType, DestType: Word; GetDataConverter, PutDataConverter: TConvertProcedure; CalcLength: TCalcSizeFunc = nil; CalcScale: TCalcSizeFunc = nil);
var
  OnDemandConverter: TOnDemandConverter;
begin
  OnDemandConverter := TOnDemandConverter.Create(SourceType, DestType, GetDataConverter, PutDataConverter, CalcLength, CalcScale);
  AddOnDemandConverter(OnDemandConverter);
end;

procedure TConverterManager.ClearOnDemandConverters;
begin
  FOnDemandConverters.Clear;
end;

procedure TConverterManager.CloneOnDemandConverter(SourceDataType, DestDataType, NewDataType: Word);
begin
  FOnDemandConverters.CloneConverter(SourceDataType, DestDataType, NewDataType);
end;

procedure TConverterManager.CloneOnDemandConverters(SourceDataType, NewDataType: Word);
begin
  FOnDemandConverters.CloneConverters(SourceDataType, NewDataType);
end;

function TConverterManager.DetectFetchConverter(DBType: Word; DBLength, DBScale: integer; DestDataType: Word): TFetchConverter;
begin
  Result := FFetchConverters.FindItem(DBType, DBLength, DBScale, DestDataType);
end;

function TConverterManager.DetectOnDemandConverter(SourceDataType, DestDataType: Word): TOnDemandConverter;
begin
  Result := FOnDemandConverters.FindItem(SourceDataType, DestDataType);
end;

function TConverterManager.GetDateFormat: string;
begin
  Result := '';//{$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
end;

function TConverterManager.GetTimeFormat: string;
begin
  Result := '';//{$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
end;

function TConverterManager.GetDateTimeFormat: string;
begin
  Result := '';//{$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat + ' ' + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
end;

{ TSizeConverters }

class function TSizeConverters.CopySize(Value: Integer): Integer;
begin
  Result := Value;
end;

class function TSizeConverters.SizeX2(Value: Integer): Integer;
begin
  Result := Value * 2;
end;

class function TSizeConverters.SizeDiv2(Value: Integer): Integer;
begin
  Result := Value shr 1;
end;

class function TSizeConverters.Size0(Value: Integer): Integer;
begin
  Result := 0;
end;

class function TSizeConverters.Size20(Value: Integer): Integer;
begin
  Result := 20;
end;

class function TSizeConverters.Size32(Value: Integer): Integer;
begin
  Result := 32;
end;

class function TSizeConverters.GuidSize(Value: Integer): Integer;
begin
  Result := 38;
end;

{ TDataConverters }

function Max(V1, V2: TConvertStatus): TConvertStatus;
begin
  if V1 > V2 then
    Result := V1
  else
    Result := V2;
end;

function BcdAsInt64ToStr(Value: Int64): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
var
  Frac: Integer;
  LastPos: Integer;
  Sign: string;
begin
  Frac := Value mod 10000;
  if Frac = 0 then
    Result := IntToStr(Value div 10000)
  else begin
    if Frac > 0 then
      Frac := 10000 + Frac
    else
      Frac := 10000 - Frac;
    Result := IntToStr(Frac);
    LastPos := 5;

    if (Value < 0) then
      Sign := '-'
    else
      Sign := '';

    while Result[LastPos] = '0' do
      Dec(LastPos);

    Result := Sign + IntToStr(Abs(Value) div 10000) +
              {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator +
              Copy(Result, 2, LastPos - 1);
  end;
end;

class function TDataConverters.CheckNumericStr(const Num: string; DestLen: Integer): TConvertStatus;
var
  DecPos: integer;
begin
  if Length(Num) > DestLen then begin
    DecPos := Pos({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, Num);
    if (DecPos > 0) and (DecPos - 1 <= DestLen) then
      Result := csFractionTruncated
    else
      Result := csValueOverflow;
  end
  else
    Result := csSuccess;
end;

class function TDataConverters.CheckNumericStr(const Num: string; Precision, Scale: Integer): TConvertStatus;
var
  DecPos, Len, SignLen: integer;
  IsNegative: boolean;
begin
  Result := csSuccess;

  Len := Length(Num);
  if Len > Precision then begin
    IsNegative := Num[1] = '-';
    SignLen := 0;
    if IsNegative then
      Inc(SignLen);
    if Num[1 + Ord(IsNegative)] = '0' then
      Inc(SignLen);
    Dec(Len, SignLen);

    if Len > Precision then begin
      DecPos := Pos({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, Num);
      if DecPos > 0 then begin
        Dec(DecPos, SignLen);

        if DecPos - 1 <= (Precision - Scale) then begin
          if Len - DecPos > Scale then
            Result := csFractionTruncated;
        end
        else
          Result := csValueOverflow;
      end
      else
        Result := csValueOverflow;
    end;
  end;
end;

class function TDataConverters.ExtendNumericStr(var Num: string; SourceScale, DestLen: Integer): TConvertStatus;
var
  i: Integer;
  StrLen: integer;
  DecPos: integer;
begin
  if SourceScale > 0 then begin
    StrLen := Length(Num);
    DecPos := Pos({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, Num);
    if DecPos < 1 then begin
      SetLength(Num, StrLen + SourceScale + 1);
      Num[StrLen + 1] := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
      for i := StrLen + 2 to Length(Num) do
        Num[i] := '0';
    end
    else if StrLen - DecPos < SourceScale  then begin
      SetLength(Num, DecPos + SourceScale);
      for i := StrLen  + 1 to Length(Num) do
        Num[i] := '0';
    end;
  end;

  Result := CheckNumericStr(Num, DestLen);
end;

class function TDataConverters.CheckDateTimeFormat(const Format: string; DestLength: Integer): string;
var
  FormatLen: integer;
begin
  FormatLen := Length(Format);
  if FormatLen > DestLength then begin
    if (FormatLen > 5) and (Copy(Format, FormatLen - 4, 5) = 'AM/PM') then
      Dec(FormatLen, 5)
    else
    if (FormatLen > 4) and (Copy(Format, FormatLen - 3, 4) = 'AMPM') then
      Dec(FormatLen, 4)
    else
    if (FormatLen > 3) and (Copy(Format, FormatLen - 2, 3) = 'A/P') then
      Dec(FormatLen, 3)
    else begin
      while (FormatLen > DestLength) and (Format[FormatLen] = 'z') do // millisec
        Dec(FormatLen);
      if Format[FormatLen] = '.' then
        Dec(FormatLen);
    end;

    Result := Trim(Copy(Format, 1, FormatLen));
  end
  else
    Result := Format;
end;

class function TDataConverters.InternalInt8ToStr(Source: IntPtr): string;
begin
  Result := IntToStr(ShortInt(Marshal.ReadByte(Source)));
end;

class function TDataConverters.InternalUInt8ToStr(Source: IntPtr): string;
begin
  Result := IntToStr(Marshal.ReadByte(Source));
end;

class function TDataConverters.InternalInt16ToStr(Source: IntPtr): string;
begin
  Result := IntToStr(Marshal.ReadInt16(Source));
end;

class function TDataConverters.InternalUInt16ToStr(Source: IntPtr): string;
begin
  Result := IntToStr(Word(Marshal.ReadInt16(Source)));
end;

class function TDataConverters.InternalInt32ToStr(Source: IntPtr): string;
begin
  Result := IntToStr(Marshal.ReadInt32(Source));
end;

class function TDataConverters.InternalUInt32ToStr(Source: IntPtr): string;
begin
  Result := IntToStr(Cardinal(Marshal.ReadInt32(Source)));
end;

class function TDataConverters.InternalInt64ToStr(Source: IntPtr): string;
begin
  Result := IntToStr(Marshal.ReadInt64(Source));
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalUInt64ToStr(Source: IntPtr): string;
var
  ui64: UInt64;
begin
  ui64 := UInt64(Marshal.ReadInt64(Source));
  Result := {$IFDEF FPC}IntToStr{$ELSE}UIntToStr{$ENDIF}(ui64);
end;

{$ENDIF}

class function TDataConverters.InternalSingleToStr(Source: IntPtr; out Str: String; DestLen: Integer): TConvertStatus;
var
  s: Single;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(Source));
  Str := FloatToStr(s);
  Result := CheckNumericStr(Str, DestLen);
end;

class function TDataConverters.InternalSingleToStr(Source: IntPtr; out Str: String; Precision, Scale: Integer): TConvertStatus;
var
  s: Single;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(Source));
  Str := FloatToStr(s);
  Result := CheckNumericStr(Str, Precision, Scale);
end;

class function TDataConverters.InternalFloatToStr(Source: IntPtr; out Str: String; DestLen: Integer): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source));
  Str := FloatToStr(d);
  Result := CheckNumericStr(Str, DestLen);
end;

class function TDataConverters.InternalFloatToStr(Source: IntPtr; out Str: String; Precision, Scale: Integer): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source));
  Str := FloatToStr(d);
  Result := CheckNumericStr(Str, Precision, Scale);
end;

class function TDataConverters.InternalExtendedToStr(Source: IntPtr; out Str: String; DestLen: Integer): TConvertStatus;
var
  e: Extended;
begin
  e := Extended(Source^);
  Str := FloatToStr(e);
  Result := CheckNumericStr(Str, DestLen);
end;

class function TDataConverters.InternalExtendedToStr(Source: IntPtr; out Str: String; Precision, Scale: Integer): TConvertStatus;
var
  e: Extended;
begin
  e := Extended(Source^);
  Str := FloatToStr(e);
  Result := CheckNumericStr(Str, Precision, Scale);
end;

class function TDataConverters.InternalBCDToStr(Source: IntPtr;  out Str: String; DestLen: Integer): TConvertStatus;
begin
  Str := BcdAsInt64ToStr(Marshal.ReadInt64(Source));
  Result := CheckNumericStr(Str, DestLen);
end;

class function TDataConverters.InternalBCDToStr(Source: IntPtr; out Str: String; Precision, Scale: Integer): TConvertStatus;
begin
  Str := BcdAsInt64ToStr(Marshal.ReadInt64(Source));
  Result := CheckNumericStr(Str, Precision, Scale);
end;

class function TDataConverters.InternalFMTBCDToStr(Source: IntPtr;  out Str: String; DestLen: Integer): TConvertStatus;
begin
  Str := BcdToStr(PBcd(Source)^);
  Result := CheckNumericStr(Str, DestLen);
end;

class function TDataConverters.InternalFMTBCDToStr(Source: IntPtr; out Str: String; Precision, Scale: Integer): TConvertStatus;
begin
  Str := BcdToStr(PBcd(Source)^);
  Result := CheckNumericStr(Str, Precision, Scale);
end;

class function TDataConverters.InternalBoolToStr(Source: IntPtr): string;
var
  wb: WordBool;
begin
  wb := WordBool(Marshal.ReadInt16(Source));
  if wb then
    Result := 'True'
  else
    Result := 'False';
end;

class function TDataConverters.InternalDateToStr(Source: IntPtr; out Str: String; const Format: string): TConvertStatus;
var
  ts: TTimeStamp;
begin
  ts.Date := Marshal.ReadInt32(Source);
  ts.Time := 0;

  if ts.Date <= 0 then begin
    Result := csInvalidDateTimeValue;
    Str := '';
  end
  else begin
    if Format = '' then
      Str := DateToStr(TimeStampToDateTime(ts))
    else
      Str := FormatDateTime(Format, TimeStampToDateTime(ts));
    Result := csSuccess;
  end;
end;

class function TDataConverters.InternalTimeToStr(Source: IntPtr; SourceScale: Integer; out Str: String; const Format: string): TConvertStatus;
var
  ts: TTimeStamp;
begin
  ts.Date := DateDelta;
  ts.Time := Marshal.ReadInt32(Source);

  if ts.Time < 0 then begin
    Result := csInvalidDateTimeValue;
    Str := '';
  end
  else begin
    if Format = '' then
      Str := TimeToStr(TimeStampToDateTime(ts))
    else
      Str := FormatDateTime(Format, TimeStampToDateTime(ts));


    Result := csSuccess;
  end;
end;

class function TDataConverters.InternalDateTimeToStr(Source: IntPtr; SourceScale: Integer; out Str: String; const Format: string): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalReadTimeStamp(Source, ts);

  if Result = csSuccess then begin
    if Format = '' then
      Str := DateTimeToStr(TimeStampToDateTime(ts))
    else
      Str := FormatDateTime(Format, TimeStampToDateTime(ts));

  end
  else
    Str := '';
end;

class function TDataConverters.InternalSQLTimeStampToStr(Source: IntPtr; SourceScale: Integer; out Str: String; const Format: string): TConvertStatus;
begin
  CRTimeStamp.DateTimeToString(Str, Format, PSQLTimeStamp(Source)^, FormatSettings);


  Result := csSuccess;
end;

class function TDataConverters.InternalSQLTimeStampOffsetToStr(Source: IntPtr; SourceScale: Integer; out Str: String; const Format: string): TConvertStatus;
var
  TZHourStr: string;
  TZMinuteStr: string;
begin
  Result := InternalSQLTimeStampToStr(Source, SourceScale, Str, Format );
  if Result <> csSuccess then
    Exit;

  if PSQLTimeStampOffset(Source).TimeZoneHour >= 0 then begin
    TZHourStr := IntToStr(PSQLTimeStampOffset(Source).TimeZoneHour);
    if Length(TZHourStr) < 2 then
      TZHourStr := '+0' + TZHourStr
    else
      TZHourStr := '+' + TZHourStr;
  end
  else begin
    TZHourStr := IntToStr(-PSQLTimeStampOffset(Source).TimeZoneHour);
    if Length(TZHourStr) < 2 then
      TZHourStr := '-0' + TZHourStr
    else
      TZHourStr := '-' + TZHourStr;
  end;

  TZMinuteStr := IntToStr(PSQLTimeStampOffset(Source).TimeZoneMinute);
  if Length(TZMinuteStr) < 2 then
    TZMinuteStr := '0' + TZMinuteStr;

  Str := Str + ' ' + TZHourStr + ':' + TZMinuteStr;

  Result := csSuccess;
end;

class function TDataConverters.InternalStrToNumber(const Str: String; out Value: Int64): TConvertStatus;
var
  e: Extended;
begin
  if TryStrToInt64(Str, Value) then begin
    Result := csSuccess;
    Exit;
  end;

  if not TryStrToFloat(Trim(Str), e) then begin
    Result := csInvalidIntegerValue;
    Value := 0;
  end
  else
    if (e > Int64($7FFFFFFFFFFFFFFF)) or (e < Int64($8000000000000000)) then begin
      Result := csValueOutOfRange;
      Value := 0;
    end
    else begin
      if System.Frac(e) <> 0 then
        Result := csFractionTruncated
      else
        Result := csSuccess;

      Value := Trunc(e);
    end;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalStrToUNumber(const Str: String; out Value: UInt64): TConvertStatus;
var
  e: Extended;
  ui64: UInt64;
begin
  if {$IFDEF FPC}TryStrToQWord{$ELSE}TryStrToUInt64{$ENDIF}(Str, Value) then begin
    Result := csSuccess;
    Exit;
  end;

  if not TryStrToFloat(Trim(Str), e) then begin
    Result := csInvalidIntegerValue;
    Value := 0;
  end
  else begin
    ui64 := UInt64($FFFFFFFFFFFFFFFF);
    if (e > ui64) or (e < 0) then begin
      Result := csValueOutOfRange;
      Value := 0;
    end
    else begin
      if System.Frac(e) <> 0 then
        Result := csFractionTruncated
      else
        Result := csSuccess;

      Value := Trunc(e);
    end;
  end;
end;

{$ENDIF}

class function TDataConverters.InternalStrToInt8(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  if not TryStrToInt64(Trim(Str), i64) then begin
    Result := InternalStrToNumber(Str, i64);
    if (Result <> csSuccess) and not IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  r := InternalInt64ToInt8(i64, Dest, IgnoreConvertErrors);
  Result := Max(Result, r);
end;

class function TDataConverters.InternalStrToUInt8(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  if not TryStrToInt64(Trim(Str), i64) then begin
    Result := InternalStrToNumber(Str, i64);
    if (Result <> csSuccess) and not IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  r := InternalInt64ToUInt8(i64, Dest, IgnoreConvertErrors);
  Result := Max(Result, r);
end;

class function TDataConverters.InternalStrToInt16(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  if not TryStrToInt64(Trim(Str), i64) then begin
    Result := InternalStrToNumber(Str, i64);
    if (Result <> csSuccess) and not IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  r := InternalInt64ToInt16(i64, Dest, IgnoreConvertErrors);
  Result := Max(Result, r);
end;

class function TDataConverters.InternalStrToUInt16(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  if not TryStrToInt64(Trim(Str), i64) then begin
    Result := InternalStrToNumber(Str, i64);
    if (Result <> csSuccess) and not IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  r := InternalInt64ToUInt16(i64, Dest, IgnoreConvertErrors);
  Result := Max(Result, r);
end;

class function TDataConverters.InternalStrToInt32(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  if not TryStrToInt64(Trim(Str), i64) then begin
    Result := InternalStrToNumber(Str, i64);
    if (Result <> csSuccess) and not IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  r := InternalInt64ToInt32(i64, Dest, IgnoreConvertErrors);
  Result := Max(Result, r);
end;

class function TDataConverters.InternalStrToUInt32(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  if not TryStrToInt64(Trim(Str), i64) then begin
    Result := InternalStrToNumber(Str, i64);
    if (Result <> csSuccess) and not IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  r := InternalInt64ToUInt32(i64, Dest, IgnoreConvertErrors);
  Result := Max(Result, r);
end;

class function TDataConverters.InternalStrToInt64(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  i64: Int64;
begin
  if not TryStrToInt64(Trim(Str), i64) then begin
    Result := InternalStrToNumber(Str, i64);
    if (Result <> csSuccess) and not IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt64(Dest, i64);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalStrToUInt64(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  ui64: UInt64;
begin
  if not {$IFDEF FPC}TryStrToQWord{$ELSE}TryStrToUInt64{$ENDIF}(Trim(Str), ui64) then begin
    Result := InternalStrToUNumber(Str, ui64);
    if (Result <> csSuccess) and not IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt64(Dest, Int64(ui64));
end;

{$ENDIF}

class function TDataConverters.InternalStrToSingle(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  e: Extended;
begin

  if not TryStrToFloat(Trim(Str), e) then begin
    Result := csInvalidNumericValue;
    if not IgnoreConvertErrors then
      Exit
    else
      e := 0;
  end
  else
    if e < -MaxSingle then begin
      Result := csValueOutOfRange;
      if not IgnoreConvertErrors then
        Exit
      else
        e := -MaxSingle;
    end
    else
    if e > MaxSingle then begin
      Result := csValueOutOfRange;
      if not IgnoreConvertErrors then
        Exit
      else
        e := MaxSingle;
    end
    else
      Result := csSuccess;

  Marshal.WriteInt32(Dest, CRBitConverter.SingleToInt32Bits(e));
end;

class function TDataConverters.InternalStrToFloat(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  e: Extended;
begin

  if not TryStrToFloat(Trim(Str), e) then begin
    Result := csInvalidNumericValue;
    if not IgnoreConvertErrors then
      Exit
    else
      e := 0;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(e));
end;

class function TDataConverters.InternalStrToExtended(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  e: Extended;
begin

  if not TryStrToFloat(Trim(Str), e) then begin
    Result := csInvalidNumericValue;
    if not IgnoreConvertErrors then
      Exit
    else
      e := 0;
  end
  else
    Result := csSuccess;

  Extended(Dest^) := e;
end;

class function TDataConverters.InternalStrToBCD(const Str: String; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  bcd: TBcd;
begin

  if not TryStrToBcd(Trim(Str), bcd) then begin
    Result := csInvalidNumericValue;
    if not IgnoreConvertErrors then
      Exit
    else
      bcd := Bcd_0;
  end
  else
    Result := InternalFMTBCDToBCDAsInt64(bcd, Dest, DestLen, DestScale, IgnoreConvertErrors);
end;

class function TDataConverters.InternalStrToFMTBCD(const Str: String; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  bcd: TBcd;
  r: TConvertStatus;
begin

  if not TryStrToBcd(Trim(Str), bcd) then begin
    Result := csInvalidNumericValue;
    if not IgnoreConvertErrors then
      Exit
    else
      bcd := Bcd_0;
  end
  else
    Result := csSuccess;

  r := InternalBCDToFMTBCD(bcd, Dest, DestLen, DestScale, IgnoreConvertErrors);
  Result := Max(Result, r);
end;

class function TDataConverters.InternalStrToBool(const Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  b: boolean;
begin
  if not TryStrToBool(Trim(Str), b) then begin
    Result := csInvalidBooleanValue;
    if not IgnoreConvertErrors then
      Exit
    else
      b := False;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt16(Dest, SmallInt(WordBool(b)));
end;

class function TDataConverters.InternalStrToDate(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  dt: TDateTime;
  IsDate: boolean;
begin
{$IFDEF USE_TFORMATSETTINGS}
  if Format <> '' then begin
    InternalFormatSettings.ShortDateFormat := Format;
    if Pos('-', Format) > 0 then
      InternalFormatSettings.DateSeparator := '-'
    else
      InternalFormatSettings.DateSeparator := '.';
    InternalFormatSettings.DecimalSeparator := '.';
    IsDate := TryStrToDateTime(Trim(Str), dt, InternalFormatSettings);
  end
  else
{$ENDIF}
    IsDate := TryStrToDateTime(Trim(Str), dt);

  if not IsDate then begin
    Result := csInvalidDateTimeValue;
    if not IgnoreConvertErrors then
      Exit
    else
      dt := 0;
  end
  else
    if System.Frac(dt) <> 0 then
      Result := csDataTruncated
    else
      Result := csSuccess;

  Marshal.WriteInt32(Dest, DateTimeToTimeStamp(dt).Date);
end;

class function TDataConverters.InternalStrToTime(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  dt: TDateTime;
  IsDate: boolean;
begin
{$IFDEF USE_TFORMATSETTINGS}
  if Format <> '' then begin
    InternalFormatSettings.LongTimeFormat := Format;
    InternalFormatSettings.DateSeparator := '-';
    if Pos('-', Format) > 0 then
      InternalFormatSettings.DateSeparator := '-'
    else
      InternalFormatSettings.DateSeparator := '.';
    InternalFormatSettings.DecimalSeparator := '.';
    IsDate := TryStrToDateTime(Trim(Str), dt, InternalFormatSettings);
  end
  else
{$ENDIF}
    IsDate := TryStrToDateTime(Trim(Str), dt);

  if not IsDate then begin
    Result := csInvalidDateTimeValue;
    if not IgnoreConvertErrors then
      Exit
    else
      dt := 0;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt32(Dest, DateTimeToTimeStamp(dt).Time);
end;

class function TDataConverters.ConvertStrToDateTime(const Str: String; const Format: String; out dt: TDateTime): boolean;
{$IFDEF USE_TFORMATSETTINGS}
var
  p: Integer;
{$ENDIF}
begin
{$IFDEF USE_TFORMATSETTINGS}
  if Format <> '' then begin
    p := Pos(' ', Format);
    if p > 0 then begin
      InternalFormatSettings.ShortDateFormat := Copy(Format, 1, p - 1);
      InternalFormatSettings.LongTimeFormat := Copy(Format, p + 1, Length(Format));
    end
    else begin
      InternalFormatSettings.ShortDateFormat := Format;
      InternalFormatSettings.LongTimeFormat := '';
    end;
    if Pos('-', Format) > 0 then
      InternalFormatSettings.DateSeparator := '-'
    else
      InternalFormatSettings.DateSeparator := '.';
    InternalFormatSettings.DecimalSeparator := '.';
    Result := TryStrToDateTime(Trim(Str), dt, InternalFormatSettings);
  end
  else
{$ENDIF}
    Result := TryStrToDateTime(Trim(Str), dt);
end;

class function TDataConverters.InternalStrToDateTime(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  dt: TDateTime;
begin
  if not ConvertStrToDateTime(Str, Format, dt) then begin
    Result := csInvalidDateTimeValue;
    if not IgnoreConvertErrors then
      Exit
    else
      dt := 0;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(DateTimeToTimeStamp(dt))));
end;

class function TDataConverters.InternalStrToSQLTimeStamp(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
  dt: TDateTime;
begin
  if not ConvertStrToDateTime(Str, Format, dt) then begin
    Result := csInvalidDateTimeValue;
    if not IgnoreConvertErrors then
      Exit
    else
      SQLTimeStamp := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.DateTimeToSQLTimeStamp(0);
  end
  else begin
    SQLTimeStamp := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.DateTimeToSQLTimeStamp(dt);
    Result := csSuccess;
  end;

  PSQLTimeStamp(Dest)^ := SQLTimeStamp;
end;

class function TDataConverters.InternalStrToSQLTimeStampOffset(const Str: String; const Format: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  SQLTimeStampOffset: TSQLTimeStampOffset;
  dt: TDateTime;
begin
  if not ConvertStrToDateTime(Str, Format, dt) then begin
    Result := csInvalidDateTimeValue;
    if not IgnoreConvertErrors then
      Exit
    else
      SQLTimeStampOffset := {$IFDEF VER14P}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.DateTimeToSQLTimeStampOffset(0, 0, 0);
  end
  else begin
    SQLTimeStampOffset := {$IFDEF VER14P}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.DateTimeToSQLTimeStampOffset(dt);
    Result := csSuccess;
  end;

  PSQLTimeStampOffset(Dest)^ := SQLTimeStampOffset;
end;

class function TDataConverters.InternalStrToGuid(const Str: String; out SourceLen: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  GuidStr: string;
  Guid: TGUID;
  r: TConvertStatus;
  DestLen: integer;
begin
  GuidStr := Trim(Str);
  if Length(GuidStr) > 1 then begin
    if GuidStr[1] <> '{' then
      GuidStr := '{' + GuidStr;
    if string(GuidStr[Length(GuidStr)]) <> '}' then // Range check error
      GuidStr := GuidStr + '}';
  end;

  if not TryStrToGUID(GuidStr, Guid) then begin
    Result := csInvalidGUIDValue;
    if not IgnoreConvertErrors then
      Exit
    else
      GuidStr := EMPTY_GUID;
  end
  else
    Result := csSuccess;

  DestLen := 38;
  r := InternalWriteAStr(AnsiString(GuidStr), SourceLen, Dest, DestLen, IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.InternalExactCopyToBlob(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  Result := InternalCopyToBlob(Source, 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalCopyToBlob(Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Blob: TBlob;
begin
  Dec(SourceLen, SourceOffset);
  if DestLen > 0 then
    Dec(DestLen, DestOffset);

  if (DestLen > 0) and (SourceLen > DestLen) then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));

  Assert(Blob <> nil);

  Blob.EnableRollback;
  if DestOffset = 0 then
    Blob.Clear;
  Blob.Write(DestOffset, DestLen, PtrOffset(Source, SourceOffset));
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalAStrToMemo(const AStr: AnsiString; SourceOffset: Integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Blob: TBlob;
begin
  SourceLen := LengthA(AStr) - SourceOffset;
  if DestLen > 0 then
    Dec(DestLen, DestOffset);

  if (DestLen > 0) and (SourceLen > DestLen) then begin
    Result := csStringTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));

  Assert(Blob <> nil);
  if Blob.Size = 0 then
    Blob.IsUnicode := False
  else
    Assert(not Blob.IsUnicode);

  Blob.EnableRollback;
  if DestOffset = 0 then
    Blob.Clear;
  Blob.Write(DestOffset, DestLen, PtrOffset(PAChar(AStr), SourceOffset));
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalWStrToWideMemo(const WStr: WideString; SourceOffset: Integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Blob: TBlob;
begin
  SourceLen := Length(WStr) - SourceOffset;
  if DestLen > 0 then
    Dec(DestLen, DestOffset);

  if (DestLen > 0) and (SourceLen > DestLen) then begin
    Result := csStringTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));

  Assert(Blob <> nil);
  if Blob.Size = 0 then
    Blob.IsUnicode := True
  else
    Assert(Blob.IsUnicode);

  Blob.EnableRollback;
  if DestOffset = 0 then
    Blob.Clear;
  Blob.Write(DestOffset shl 1, DestLen shl 1, PtrOffset(@WStr[1], SourceOffset shl 1));
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalExactCopyFromBlob(Source: IntPtr; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Blob: TBlob;
  BlobSize: Cardinal;
begin
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Source)));
  Assert(Blob <> nil);
  BlobSize := Blob.Size;
  if BlobSize = 0 then begin
    Result := csSuccess;
    if Dest <> nil then
      FillChar(Dest, DestLen, 0);
    Exit;
  end
  else
  if BlobSize <> Cardinal(DestLen) then begin
    Result := csInvalidBlobValue;
    if not IgnoreConvertErrors then
      Exit
    else
    if BlobSize > Cardinal(DestLen) then
      BlobSize := DestLen;
  end
  else
    Result := csSuccess;
  Blob.Read(0, BlobSize, Dest);
end;

class function TDataConverters.InternalCopyFromBlobToBytes(Source: IntPtr; SourceOffset: integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Blob: TBlob;
  BlobSize: Cardinal;
  CopyLen: integer;
begin
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Source)));
  Assert(Blob <> nil);
  BlobSize := Blob.Size - Cardinal(SourceOffset);
  SourceLen := BlobSize;
  Dec(DestLen, DestOffset);

  if BlobSize > Cardinal(DestLen) then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit
    else
      CopyLen := DestLen;
  end
  else begin
    CopyLen := BlobSize;
    Result := csSuccess;
  end;

  if CopyLen > 0 then
    Blob.Read(SourceOffset, CopyLen, PtrOffset(Dest, DestOffset));
//  FillChar(PtrOffset(Dest, DestOffset + CopyLen), DestLen - CopyLen, 0);
  DestLen := DestOffset + CopyLen;
end;

class function TDataConverters.InternalCopyFromBlobToVarBytes(Source: IntPtr; SourceOffset: integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Blob: TBlob;
  BlobSize: Cardinal;
begin
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Source)));
  Assert(Blob <> nil);
  BlobSize := Blob.Size - Cardinal(SourceOffset);
  SourceLen := BlobSize;
  Dec(DestLen, DestOffset);

  if BlobSize > Cardinal(DestLen) then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := BlobSize;
    Result := csSuccess;
  end;

  Marshal.WriteInt16(Dest, DestLen + DestOffset);
  if DestLen > 0 then
    Blob.Read(SourceOffset, DestLen, PtrOffset(Dest, SizeOf(Word) + DestOffset));
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalCopyFromBlobToExtBytes(StringHeap: TStringHeap; Source: IntPtr; SourceOffset: integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Blob: TBlob;
  BlobSize: Cardinal;
  OldBuf, NewBuf: IntPtr;
begin
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Source)));
  Assert(Blob <> nil);
  BlobSize := Blob.Size - Cardinal(SourceOffset);
  SourceLen := BlobSize;
  Dec(DestLen, DestOffset);

  if BlobSize > Cardinal(DestLen) then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := BlobSize;
    Result := csSuccess;
  end;

  NewBuf := StringHeap.NewBuf(DestLen + DestOffset);
  OldBuf := Marshal.ReadIntPtr(Dest);
  if OldBuf <> nil then begin
    if DestOffset > 0 then
      Move(OldBuf^, NewBuf^, DestOffset);
    StringHeap.DisposeBuf(OldBuf);
  end;

  if DestLen > 0 then
    Blob.Read(SourceOffset, DestLen, PtrOffset(NewBuf, DestOffset));
  Marshal.WriteIntPtr(Dest, NewBuf);
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalCopyFromBlobToExtVarBytes(StringHeap: TStringHeap; Source: IntPtr; SourceOffset: integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Blob: TBlob;
  BlobSize: Cardinal;
  OldBuf, NewBuf: IntPtr;
begin
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Source)));
  Assert(Blob <> nil);
  BlobSize := Blob.Size - Cardinal(SourceOffset);
  SourceLen := BlobSize;
  Dec(DestLen, DestOffset);

  if BlobSize > Cardinal(DestLen) then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := BlobSize;
    Result := csSuccess;
  end;

  NewBuf := StringHeap.NewBuf(DestLen + DestOffset + SizeOf(Word));
  OldBuf := Marshal.ReadIntPtr(Dest);
  if OldBuf <> nil then begin
    if DestOffset > 0 then
      Move(PtrOffset(OldBuf, SizeOf(Word))^, PtrOffset(NewBuf, SizeOf(Word))^, DestOffset);
    StringHeap.DisposeBuf(OldBuf);
  end;

  Marshal.WriteInt16(NewBuf, DestLen + DestOffset);
  if DestLen > 0 then
    Blob.Read(SourceOffset, DestLen, PtrOffset(NewBuf, SizeOf(Word) + DestOffset));
  Marshal.WriteIntPtr(Dest, NewBuf);
  Inc(DestLen, DestOffset);
end;

class procedure TDataConverters.InternalMemoToAStr(Source: IntPtr; var SourceOffset: Integer; out SourceLen: Integer; DestOffset: Integer; var DestLen: Integer; out AStr: AnsiString);
var
  Blob: TBlob;
  Size: Integer;
begin
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Source)));
  Assert((Blob <> nil) and not Blob.IsUnicode);

  SourceLen := Integer(Blob.Size);
  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);
  if DestLen < SourceLen then
    Size := DestLen
  else
    Size := SourceLen;

  SetLengthA(AStr, Size);
  if Size > 0 then
    Blob.Read(SourceOffset, Size, PAChar(AStr));
  SourceOffset := 0;
end;

class procedure TDataConverters.InternalWideMemoToWStr(Source: IntPtr; var SourceOffset: Integer; out SourceLen: Integer; DestOffset: Integer; DestLen: Integer; out WStr: WideString);
var
  Blob: TBlob;
  Size: Integer;
begin
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Source)));
  Assert((Blob <> nil) and Blob.IsUnicode);

  SourceLen := (Integer(Blob.Size) shr 1);
  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);
  if DestLen < SourceLen then
    Size := DestLen
  else
    Size := SourceLen;

  SetLength(WStr, Size);
  if Size > 0 then
    Blob.Read(SourceOffset shl 1, Size shl 1, @WStr[1]);
  SourceOffset := 0;
end;

class procedure TDataConverters.InternalBytesToAStr(Source: IntPtr; var SourceLen: Integer; out AStr: AnsiString);
var
  EndPtr: PByte;
begin
  EndPtr := PtrOffset(Source, SourceLen - 1);
  while (NativeUInt(EndPtr) >= NativeUInt(Source)) and (EndPtr^ = 0) do
    Dec(EndPtr);
  SourceLen := PtrSubstract(EndPtr, Source) + 1;

  SetLengthA(AStr, SourceLen);
  if SourceLen > 0 then
    Move(Source^, PAnsiChar(AStr)^, SourceLen);
end;

class procedure TDataConverters.InternalBytesToWStr(Source: IntPtr; var SourceLen: Integer; out WStr: WideString);
var
  Len: Integer;
  EndPtr: PWord;
begin
  if (SourceLen and $00000001 = 1) and (Marshal.ReadByte(Source, SourceLen - 1) > 0) then begin
    Len := (SourceLen + 1) shr 1;
    SetLength(WStr, Len);
    Move(Source^, WStr[1], SourceLen);
    Marshal.WriteByte(@WStr[1], SourceLen, 0);
  end
  else begin
    EndPtr := PtrOffset(Source, (SourceLen and $FFFFFFFE) - 2);
    while (NativeUInt(EndPtr) >= NativeUInt(Source)) and (EndPtr^ = 0) do
      Dec(EndPtr);
    SourceLen := PtrSubstract(EndPtr, Source) + 2;

    Len := SourceLen shr 1;
    SetLength(WStr, Len);
    if Len > 0 then
      Move(Source^, WStr[1], SourceLen);
  end;

  SourceLen := Len;
end;

class procedure TDataConverters.InternalVarBytesToAStr(Source: IntPtr; out AStr: AnsiString; out SourceLen: Integer);
begin
  SourceLen := Marshal.ReadInt16(Source);
  SetLengthA(AStr, SourceLen);
  Move(PtrOffset(Source, SizeOf(Word))^, PAnsiChar(AStr)^, SourceLen);
end;

class procedure TDataConverters.InternalVarBytesToWStr(Source: IntPtr; out WStr: WideString; out SourceLen: Integer);
var
  Len: Integer;
begin
  SourceLen := Marshal.ReadInt16(Source);
  Len := (SourceLen  + 1) shr 1;
  SetLength(WStr, Len);
  Move(PtrOffset(Source, SizeOf(Word))^, WStr[1], SourceLen);
  if SourceLen and $00000001 = 1 then
    Marshal.WriteByte(@WStr[1], SourceLen, 0);
  SourceLen := Len;
end;

class function TDataConverters.InternalBytesToGuid(Source: IntPtr; var SourceLen: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Str: string;
  Buf: TBytes;
begin
  if SourceLen <> 16 then begin
    if IgnoreConvertErrors then begin
      Str := EMPTY_GUID;
      Result := InternalStrToGUID(Str, SourceLen, Dest, IgnoreConvertErrors);
    end
    else
      Result := csInvalidGUIDValue;
  end
  else begin
    SetLength(Buf, 16);
    Marshal.Copy(Source, Buf, 0, 16);

    SetLength(Str, 32);
  {$IFDEF IS_UNICODE}
    BinToHexW(Buf, PWideChar(Str), 16);
  {$ELSE}
    BinToHexA(Buf, PAnsiChar(Str), 16);
  {$ENDIF}
    Str :='{' + Copy(Str, 1, 8) + '-' + Copy(Str, 9, 4) + '-' + Copy(Str, 13, 4) + '-' + Copy(Str, 17, 4) + '-' + Copy(Str, 21, 32) + '}';

    Result := InternalStrToGUID(Str, SourceLen, Dest, IgnoreConvertErrors);
  end;
end;

class function TDataConverters.InternalExactCopyToBytes(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  Result := InternalCopyToBytes(Source, 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalCopyToBytes(Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  CopyLen: Integer;
begin
  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);

  if SourceLen > DestLen then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit
    else
      CopyLen := DestLen;
  end
  else begin
    CopyLen := SourceLen;
    Result := csSuccess;
  end;

  Move(PtrOffset(Source, SourceOffset)^, PtrOffset(Dest, DestOffset)^, CopyLen);
//  FillChar(PtrOffset(Dest, DestOffset + CopyLen), DestLen - CopyLen, 0);
  DestLen := DestOffset + CopyLen;
end;

class function TDataConverters.InternalExactCopyToVarBytes(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  Result := InternalCopyToVarBytes(Source, 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalCopyToVarBytes(Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);

  if SourceLen > DestLen then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  Marshal.WriteInt16(Dest, DestLen + DestOffset);
  Move(PtrOffset(Source, SourceOffset)^, PtrOffset(Dest, SizeOf(Word) + DestOffset)^, DestLen);
  Inc(DestLen, DestOffset);
end;


class function TDataConverters.InternalExactCopyToExtBytes(StringHeap: TStringHeap; Source: IntPtr; SourceLen: Integer; Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  Result := InternalCopyToExtBytes(StringHeap, Source, 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalCopyToExtBytes(StringHeap: TStringHeap; Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  OldBuf, NewBuf: IntPtr;
begin
  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);

  if SourceLen > DestLen then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  NewBuf := StringHeap.NewBuf(DestLen + DestOffset);
  OldBuf := Marshal.ReadIntPtr(Dest);
  if OldBuf <> nil then begin
    if DestOffset > 0 then
      Move(OldBuf^, NewBuf^, DestOffset);
    StringHeap.DisposeBuf(OldBuf);
  end;

  Move(PtrOffset(Source, SourceOffset)^, PtrOffset(NewBuf, DestOffset)^, DestLen);
  Marshal.WriteIntPtr(Dest, NewBuf);
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalExactCopyToExtVarBytes(StringHeap: TStringHeap; Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  Result := InternalCopyToExtVarBytes(StringHeap, Source, 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalCopyToExtVarBytes(StringHeap: TStringHeap; Source: IntPtr; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  OldBuf, NewBuf: IntPtr;
begin
  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);

  if SourceLen > DestLen then begin
    Result := csBinaryTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  NewBuf := StringHeap.NewBuf(DestLen + DestOffset + SizeOf(Word));
  OldBuf := Marshal.ReadIntPtr(Dest);
  if OldBuf <> nil then begin
    if DestOffset > 0 then
      Move(PtrOffset(OldBuf, SizeOf(Word))^, PtrOffset(NewBuf, SizeOf(Word))^, DestOffset);
    StringHeap.DisposeBuf(OldBuf);
  end;

  Marshal.WriteInt16(NewBuf, DestLen + DestOffset);
  Move(PtrOffset(Source, SourceOffset)^, PtrOffset(NewBuf, SizeOf(Word) + DestOffset)^, DestLen);
  Marshal.WriteIntPtr(Dest, NewBuf);
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalExactCopyFromBytes(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if SourceLen <> DestLen then begin
    Result := csInvalidBinaryValue;
    if not IgnoreConvertErrors then
      Exit
    else
    if SourceLen > DestLen then
      SourceLen := DestLen;
  end
  else
    Result := csSuccess;

  Move(Source^, Dest^, SourceLen);
end;

class function TDataConverters.InternalExactCopyFromVarBytes(Source: IntPtr; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  SourceLen: Integer;
begin
  SourceLen := Marshal.ReadInt16(Source);

  if SourceLen <> DestLen then begin
    Result := csInvalidBinaryValue;
    if not IgnoreConvertErrors then
      Exit
    else
    if SourceLen > DestLen then
      SourceLen := DestLen;
  end
  else
    Result := csSuccess;

  Move(PtrOffset(Source, SizeOf(Word))^, Dest^, SourceLen);
end;

class function TDataConverters.InternalWriteAStr(const AStr: AnsiString; out SourceLen: Integer;
  Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  SourceLen := -1;
  Result := InternalWritePAChar(PAChar(AStr), 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalWriteAStr(const AStr: AnsiString; SourceOffset: Integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  SourceLen := -1;
  Result := InternalWritePAChar(PAChar(AStr), SourceOffset, SourceLen, Dest, DestOffset, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalWritePAChar(AStr: PAChar; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if SourceLen < 0 then
    SourceLen := StrLen(AStr);

  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);

  if SourceLen > DestLen then begin
    Result := csStringTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  if DestLen >= 0 then begin
    Move(PtrOffset(AStr, SourceOffset)^, PtrOffset(Dest, DestOffset)^, DestLen);
    Marshal.WriteByte(Dest, DestOffset + DestLen, 0);
  end;
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalWriteExtAStr(StringHeap: TStringHeap; const AStr: AnsiString; out SourceLen: Integer;
  Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  SourceLen := -1;
  Result := InternalWriteExtPAChar(StringHeap, PAChar(AStr), 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalWriteExtAStr(StringHeap: TStringHeap; const AStr: AnsiString; SourceOffset: Integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  SourceLen := -1;
  Result := InternalWriteExtPAChar(StringHeap, PAChar(AStr), SourceOffset, SourceLen, Dest, DestOffset, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalWriteExtPAChar(StringHeap: TStringHeap; AStr: PAChar; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  OldBuf, NewBuf: IntPtr;
begin
  if SourceLen < 0 then
    SourceLen := StrLen(AStr);

  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);

  if SourceLen > DestLen then begin
    Result := csStringTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  if DestLen >= 0 then begin
    NewBuf := StringHeap.NewBuf(DestLen + DestOffset + 1);
    OldBuf := Marshal.ReadIntPtr(Dest);
    if OldBuf <> nil then begin
      if DestOffset > 0 then
        Move(OldBuf^, NewBuf^, DestOffset);
      StringHeap.DisposeBuf(OldBuf);
    end;

    Move(PtrOffset(AStr, SourceOffset)^, PtrOffset(NewBuf, DestOffset)^, DestLen);
    Marshal.WriteByte(NewBuf, DestLen + DestOffset, 0);
    Marshal.WriteIntPtr(Dest, NewBuf);
  end;
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalWriteWStr(const WStr: WideString; out SourceLen: Integer; Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  SourceLen := -1;
  Result := InternalWritePWChar(PWChar(WStr), 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalWriteWStr(const WStr: WideString; SourceOffset: Integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  SourceLen := -1;
  Result := InternalWritePWChar(PWChar(WStr), SourceOffset, SourceLen, Dest, DestOffset, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalWritePWChar(WStr: PWChar; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if SourceLen < 0 then
    SourceLen := StrLenW(WStr);

  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);

  if SourceLen > DestLen then begin
    Result := csStringTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  if DestLen >= 0 then begin
    Move(PtrOffset(WStr, SourceOffset * SizeOf(WideChar))^, PtrOffset(Dest, DestOffset * SizeOf(WideChar))^, DestLen * SizeOf(WideChar));
    Marshal.WriteInt16(Dest, (DestOffset + DestLen) * SizeOf(WideChar), 0);
  end;
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalWriteExtWStr(StringHeap: TStringHeap; const WStr: WideString; out SourceLen: Integer;
  Dest: IntPtr; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  SourceLen := -1;
  Result := InternalWriteExtPWChar(StringHeap, PWChar(WStr), 0, SourceLen, Dest, 0, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalWriteExtWStr(StringHeap: TStringHeap; const WStr: WideString; SourceOffset: Integer; out SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  SourceLen := -1;
  Result := InternalWriteExtPWChar(StringHeap, PWChar(WStr), SourceOffset, SourceLen, Dest, DestOffset, DestLen, IgnoreConvertErrors);
end;

class function TDataConverters.InternalWriteExtPWChar(StringHeap: TStringHeap; WStr: PWChar; SourceOffset: Integer; var SourceLen: Integer;
  Dest: IntPtr; DestOffset: Integer; var DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  OldBuf, NewBuf: IntPtr;
begin
  if SourceLen < 0 then
    SourceLen := StrLenW(WStr);

  Dec(SourceLen, SourceOffset);
  Dec(DestLen, DestOffset);

  if SourceLen > DestLen then begin
    Result := csStringTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end
  else begin
    DestLen := SourceLen;
    Result := csSuccess;
  end;

  if DestLen >= 0 then begin
    NewBuf := StringHeap.NewBuf((DestLen + DestOffset + 1) * sizeof(WideChar));
    OldBuf := Marshal.ReadIntPtr(Dest);
    if OldBuf <> nil then begin
      if DestOffset > 0 then
        Move(OldBuf^, NewBuf^, DestOffset * sizeof(WideChar));
      StringHeap.DisposeBuf(OldBuf);
    end;

    Move(PtrOffset(WStr, SourceOffset * sizeof(WideChar))^, PtrOffset(NewBuf, DestOffset * sizeof(WideChar))^, DestLen * sizeof(WideChar));
    Marshal.WriteInt16(NewBuf, (DestLen + DestOffset) * sizeof(WideChar), 0);
    Marshal.WriteIntPtr(Dest, NewBuf);
  end;
  Inc(DestLen, DestOffset);
end;

class function TDataConverters.InternalInt32ToInt8(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  // Range check
  if (Cardinal(Value) and $FFFFFF80 <> $00000000) and (Cardinal(Value) and $FFFFFF80 <> $FFFFFF80) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Cardinal(Value) and $80000000 = 0 then
      Marshal.WriteByte(Dest, Byte($7F))
    else
      Marshal.WriteByte(Dest, Byte($80));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(Dest, Byte(Value));
  end;
end;

class function TDataConverters.InternalInt32ToUInt8(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  // Range check
  if Cardinal(Value) and $FFFFFF00 <> $00000000 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Cardinal(Value) and $80000000 = 0 then
      Marshal.WriteByte(Dest, Byte($FF))
    else
      Marshal.WriteByte(Dest, Byte($00));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(Dest, Byte(Value));
  end;
end;

class function TDataConverters.InternalInt32ToInt16(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  // Range check
  if (Cardinal(Value) and $FFFF8000 <> $00000000) and (Cardinal(Value) and $FFFF8000 <> $FFFF8000) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Cardinal(Value) and $80000000 = 0 then
      Marshal.WriteInt16(Dest, SmallInt($7FFF))
    else
      Marshal.WriteInt16(Dest, SmallInt($8000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(Dest, SmallInt(Value));
  end;
end;

class function TDataConverters.InternalInt32ToUInt16(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  // Range check
  if Cardinal(Value) and $FFFF0000 <> $00000000 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Cardinal(Value) and $80000000 = 0 then
      Marshal.WriteInt16(Dest, SmallInt($FFFF))
    else
      Marshal.WriteInt16(Dest, SmallInt($0000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(Dest, SmallInt(Value));
  end;
end;

class function TDataConverters.InternalInt32ToUInt32(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  // Range check
  if Cardinal(Value) and $80000000 <> 0 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt32(Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt32(Dest, Value);
  end;
end;

class function TDataConverters.InternalInt32ToUInt64(Value: Integer; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  // Range check
  if Cardinal(Value) and $80000000 <> 0 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt64(Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt64(Dest, Value);
  end;
end;

class function TDataConverters.InternalInt64ToInt8(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if (Value and $FFFFFFFFFFFFFF80 <> 0) and (Value and $FFFFFFFFFFFFFF80 <> $FFFFFFFFFFFFFF80) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteByte(Dest, Byte($7F))
    else
      Marshal.WriteByte(Dest, Byte($80));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(Dest, Byte(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalInt64ToUInt8(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if (Int64Rec(Value).Hi <> $00000000) or (Int64Rec(Value).Lo and $FFFFFF00 <> $00000000) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteByte(Dest, Byte($FF))
    else
      Marshal.WriteByte(Dest, Byte($00));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(Dest, Byte(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalInt64ToInt16(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if (Value and $FFFFFFFFFFFF8000 <> 0) and (Value and $FFFFFFFFFFFF8000 <> $FFFFFFFFFFFF8000) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteInt16(Dest, SmallInt($7FFF))
    else
      Marshal.WriteInt16(Dest, SmallInt($8000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(Dest, SmallInt(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalInt64ToUInt16(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if (Int64Rec(Value).Hi <> $00000000) or (Int64Rec(Value).Lo and $FFFF0000 <> $00000000) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteInt16(Dest, SmallInt($FFFF))
    else
      Marshal.WriteInt16(Dest, SmallInt($0000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(Dest, SmallInt(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalInt64ToInt32(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if (Value and $FFFFFFFF80000000 <> 0) and (Value and $FFFFFFFF80000000 <> $FFFFFFFF80000000) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteInt32(Dest, Integer($7FFFFFFF))
    else
      Marshal.WriteInt32(Dest, Integer($80000000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt32(Dest, Integer(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalInt64ToUInt32(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if Int64Rec(Value).Hi <> $00000000 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteInt32(Dest, Integer($FFFFFFFF))
    else
      Marshal.WriteInt32(Dest, Integer($00000000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt32(Dest, Integer(Int64Rec(Value).Lo));
  end;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalInt64ToUInt64(Value: Int64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if Int64Rec(Value).Hi and $80000000 <> 0 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit;
    Marshal.WriteInt64(Dest, 0);
  end
  else begin
    Marshal.WriteInt64(Dest, Value);
    Result := csSuccess;
  end;
end;

class function TDataConverters.InternalUInt64ToInt8(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if Value and UInt64($FFFFFFFFFFFFFF80) <> 0 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteByte(Dest, Byte($7F))
    else
      Marshal.WriteByte(Dest, Byte($00));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(Dest, Byte(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalUInt64ToUInt8(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if (Int64Rec(Value).Hi <> $00000000) or (Int64Rec(Value).Lo and $FFFFFF00 <> $00000000) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteByte(Dest, Byte($FF))
    else
      Marshal.WriteByte(Dest, Byte($00));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(Dest, Byte(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalUInt64ToInt16(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if Value and UInt64($FFFFFFFFFFFF8000) <> 0 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteInt16(Dest, SmallInt($7FFF))
    else
      Marshal.WriteInt16(Dest, SmallInt($0000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(Dest, SmallInt(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalUInt64ToUInt16(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if (Int64Rec(Value).Hi <> $00000000) or (Int64Rec(Value).Lo and $FFFF0000 <> $00000000) then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteInt16(Dest, SmallInt($FFFF))
    else
      Marshal.WriteInt16(Dest, SmallInt($0000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(Dest, SmallInt(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalUInt64ToInt32(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if Value and UInt64($FFFFFFFF80000000) <> 0 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteInt32(Dest, Integer($7FFFFFFF))
    else
      Marshal.WriteInt32(Dest, Integer($00000000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt32(Dest, Integer(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalUInt64ToUInt32(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if Int64Rec(Value).Hi <> $00000000 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else if Int64Rec(Value).Hi and $80000000 = 0 then
      Marshal.WriteInt32(Dest, Integer($FFFFFFFF))
    else
      Marshal.WriteInt32(Dest, Integer($00000000));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt32(Dest, Integer(Int64Rec(Value).Lo));
  end;
end;

class function TDataConverters.InternalUInt64ToInt64(Value: UInt64; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  if Int64Rec(Value).Hi and $80000000 <> 0 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit;
    Marshal.WriteInt64(Dest, 0);
  end
  else begin
    Marshal.WriteInt64(Dest, Int64(Value));
    Result := csSuccess;
  end;
end;

{$ENDIF}

class function TDataConverters.InternalSingleToInt64(Source: IntPtr; out Value: Int64): TConvertStatus;
var
  s: Single;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(Source));

  if (s > Int64($7FFFFFFFFFFFFFFF)) or (s < Int64($8000000000000000)) then begin
    Result := csValueOutOfRange;
    Value := 0;
  end
  else begin
    if System.Frac(s) <> 0 then
      Result := csFractionTruncated
    else
      Result := csSuccess;

    Value := Trunc(s);
  end;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalSingleToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus;
var
  s: Single;
  ui64: UInt64;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(Source));

  ui64 := UInt64($FFFFFFFFFFFFFFFF);
  if (s > ui64) or (s < 0) then begin
    Result := csValueOutOfRange;
    Value := 0;
  end
  else begin
    if System.Frac(s) <> 0 then
      Result := csFractionTruncated
    else
      Result := csSuccess;

    Value := Trunc(s);
  end;
end;

{$ENDIF}

class function TDataConverters.InternalFloatToInt64(Source: IntPtr; out Value: Int64): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source));

  if (d > Int64($7FFFFFFFFFFFFFFF)) or (d < Int64($8000000000000000)) then begin
    Result := csValueOutOfRange;
    Value := 0;
  end
  else begin
    if System.Frac(d) <> 0 then
      Result := csFractionTruncated
    else
      Result := csSuccess;

    Value := Trunc(d);
  end;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalFloatToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus;
var
  d: Double;
  ui64: UInt64;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source));

  ui64 := UInt64($FFFFFFFFFFFFFFFF);
  if (d > ui64) or (d < 0) then begin
    Result := csValueOutOfRange;
    Value := 0;
  end
  else begin
    if System.Frac(d) <> 0 then
      Result := csFractionTruncated
    else
      Result := csSuccess;

    Value := Trunc(d);
  end;
end;

{$ENDIF}

class function TDataConverters.InternalExtendedToInt64(Source: IntPtr; out Value: Int64): TConvertStatus;
var
  e: Extended;
begin
  e := Extended(Source^);

  if (e > Int64($7FFFFFFFFFFFFFFF)) or (e < Int64($8000000000000000)) then begin
    Result := csValueOutOfRange;
    Value := 0;
  end
  else begin
    if System.Frac(e) <> 0 then
      Result := csFractionTruncated
    else
      Result := csSuccess;

    Value := Trunc(e);
  end;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalExtendedToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus;
var
  e: Extended;
  ui64: Uint64;
begin
  e := Extended(Source^);

  ui64 := UInt64($FFFFFFFFFFFFFFFF);
  if (e > ui64) or (e < 0) then begin
    Result := csValueOutOfRange;
    Value := 0;
  end
  else begin
    if System.Frac(e) <> 0 then
      Result := csFractionTruncated
    else
      Result := csSuccess;

    Value := Trunc(e);
  end;
end;

{$ENDIF}

class function TDataConverters.InternalBCDToInt64(Source: IntPtr; out Value: Int64): TConvertStatus;
begin
  Value := Marshal.ReadInt64(Source);

  // Cast check
  if Value mod 10000 <> 0 then
    Result := csFractionTruncated
  else
    Result := csSuccess;

  Value := Value div 10000;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalBCDToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus;
begin
  Value := UInt64(Marshal.ReadInt64(Source));

  // Cast check
  if Value mod 10000 <> 0 then
    Result := csFractionTruncated
  else
    Result := csSuccess;

  Value := Value div 10000;
end;

{$ENDIF}

class function TDataConverters.InternalExtendedToBCD(e: Extended; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  c_frac: Currency;
  i64: Int64;
  r: TConvertStatus;
begin
  if e < -922337203685477 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else
      e := -922337203685477;
  end
  else if e > 922337203685477 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else
      e := 922337203685477;
  end
  else
    Result := csSuccess;

  c_frac := System.Frac(e) * 10000;
  if System.Frac(c_frac) <> 0 then begin
    Result := csFractionTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end;
  {$Q-} // Bug in Delphi on Overflow checking when i64 <= -922337203685477
  i64 := Trunc(e) * Int64(10000) + Trunc(c_frac);

  r := WriteInt64AsBCD(i64, Dest, DestLen, DestScale, IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.InternalCurrencyToBCD(e: Extended; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  c_frac: Currency;
  i64: Int64;
  r: TConvertStatus;
begin
  if e < -922337203685477 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else
      e := -922337203685477;
  end
  else if e > 922337203685477 then begin
    Result := csValueOutOfRange;
    if not IgnoreConvertErrors then
      Exit
    else
      e := 922337203685477;
  end
  else
    Result := csSuccess;

  c_frac := System.Frac(e) * 10000;
  if Abs(System.Frac(c_frac)) > 0.09 then begin
    Result := csFractionTruncated;
    if not IgnoreConvertErrors then
      Exit;
  end;
  {$Q-} // Bug in Delphi on Overflow checking when i64 <= -922337203685477
  i64 := Trunc(e) * Int64(10000) + Trunc(c_frac);

  r := WriteInt64AsBCD(i64, Dest, DestLen, DestScale, IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.WriteInt64AsBCD(i64: Int64; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  Result := csSuccess;
  if (DestLen > 0) or (DestScale > 0) then
    if (DestScale >= 0) and (DestScale < 4) and (i64 mod DecValue[DestScale] > 0) then begin
      Result := csInvalidValueScale;
      if not IgnoreConvertErrors then
        Exit
      else
        i64 := (i64 div DecValue[DestScale]) * DecValue[DestScale];
    end;

  Marshal.WriteInt64(Dest, i64);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.WriteUInt64AsBCD(ui64: UInt64; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
begin
  Result := csSuccess;
  if (DestLen > 0) or (DestScale > 0) then
    if (DestScale >= 0) and (DestScale < 4) and (ui64 mod DecValue[DestScale] > 0) then begin
      Result := csInvalidValueScale;
      if not IgnoreConvertErrors then
        Exit
      else
        ui64 := (ui64 div DecValue[DestScale]) * DecValue[DestScale];
    end;

  Marshal.WriteInt64(Dest, Int64(ui64));
end;

{$ENDIF}

class function TDataConverters.InternalFMTBCDToInt64(Source: IntPtr; out Value: Int64): TConvertStatus;
var
  bcd, ibcd: TBcd;
begin
  bcd := PBcd(Source)^;

  // Cast check
  if not IsBcdInt(bcd) then
    Result := csFractionTruncated
  else
    Result := csSuccess;

  CRFunctions.NormalizeBcd(bcd, ibcd, bcd.Precision, 0);
  Value := StrToInt64(BcdToStr(ibcd));
end;

{$IFDEF USE_UINT64}

class function TDataConverters.InternalFMTBCDToUInt64(Source: IntPtr; out Value: UInt64): TConvertStatus;
var
  bcd, ibcd: TBcd;
begin
  bcd := PBcd(Source)^;

  // Cast check
  if not IsBcdInt(bcd) then
    Result := csFractionTruncated
  else
    Result := csSuccess;

  CRFunctions.NormalizeBcd(bcd, ibcd, bcd.Precision, 0);
  Value := {$IFDEF FPC}StrToQWord{$ELSE}StrToUInt64{$ENDIF}(BcdToStr(ibcd));
end;

{$ENDIF}

class function TDataConverters.InternalFMTBCDToBCDAsInt64(const Bcd: TBcd; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  abcd: TBcd;
  i64: Int64;
  Scale: Integer;
  SpecialPlaces: Integer;
  r: TConvertStatus;
begin
  // Cast check
  Scale := GetBcdScale(bcd);
  if Scale > 4 then begin
    Result := csFractionTruncated;
    if not IgnoreConvertErrors then
      Exit
    else
      CRFunctions.NormalizeBcd(Bcd, abcd, Bcd.Precision, 4);
    Scale := GetBcdScale(abcd);
  end
  else begin
    Result := csSuccess;
    abcd := Bcd;
  end;

  if Scale and $01 = 0 then begin
    SpecialPlaces := abcd.SignSpecialPlaces and $3E;
    abcd.SignSpecialPlaces := abcd.SignSpecialPlaces and $C1;
  end
  else begin
    SpecialPlaces := abcd.SignSpecialPlaces and $3F;
    abcd.SignSpecialPlaces := abcd.SignSpecialPlaces and $C0;
  end;

  if SpecialPlaces < 4 then
    abcd.Precision := abcd.Precision + 4 - SpecialPlaces;

  if not TryStrToInt64(BcdToStr(abcd), i64) then begin
    Result := csValueOutOfRange;
    if abcd.SignSpecialPlaces and $80 = 0 then
      i64 := $7FFFFFFFFFFFFFFF
    else
      i64 := $8000000000000000;
  end;

  r := WriteInt64AsBCD(i64, Dest, DestLen, DestScale, IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.InternalBCDToFMTBCD(Bcd: TBcd; Dest: IntPtr; DestLen, DestScale: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  aBcd: TBcd;
  Precision, Scale: Integer;
  Str: string;
begin
  Result := csSuccess;
  if (DestLen > 0) or (DestScale > 0) then begin
    Precision := GetBcdPrecision(Bcd);
    Scale := GetBcdScale(Bcd);

    if (DestScale >= 0) and (Scale > DestScale) then begin
      Result := csInvalidValueScale;
      if not IgnoreConvertErrors then
        Exit
      else begin
        CRFunctions.NormalizeBcd(Bcd, aBcd, Precision - Scale + DestScale, DestScale);
        Precision := GetBcdPrecision(aBcd);
        Bcd := aBcd;
      end;
    end;

    if (DestLen >= 0) and (Precision > DestLen) then begin
      Result := csValueOutOfRange;
      if not IgnoreConvertErrors then
        Exit
      else if DestLen = 0 then
        Bcd := Bcd_0
      else begin
        if DestLen - DestScale > 0 then
          Str := StringOfChar('9', DestLen - DestScale)
        else
          Str := '0';

        if DestScale > 0 then
          Str := Str + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator + StringOfChar('9', DestScale);

        if IsBcdNegative(Bcd)then
          Str := '-' + Str;

        Bcd := StrToBcd(Str);
      end;
    end;
  end;

  PBcd(Dest)^ := Bcd;
end;

class function TDataConverters.InternalGuidToBytes(Source: IntPtr; SourceLen: Integer; Dest: IntPtr; DestLen: Integer; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Str: String;
  Buf: TBytes;
begin
  if DestLen < 16 then begin
    if IgnoreConvertErrors then begin
      FillChar(Dest, DestLen, 0);
      Result := csSuccess;
    end
    else
      Result := csBinaryTruncated
  end
  else begin
    Str := string(Marshal.PtrToStringAnsi(Source, SourceLen));
    if Str[1] = '{' then
      Str := Copy(Str, 2, 36);
    Str := StringReplace(Str, '-', '', [rfReplaceAll]);

    SetLength(Buf, 16);
  {$IFDEF IS_UNICODE}
    HexToBinW(PWideChar(Str), Buf, 32);
  {$ELSE}
    HexToBinA(PAnsiChar(Str), Buf, 32);
  {$ENDIF}

    Marshal.Copy(Buf, 0, Dest, 16);
    Result := csSuccess;
  end;
end;

class function TDataConverters.InternalVariantToBytes(const v: Variant; out Buf: Pointer; out Size: Integer): TConvertStatus;
begin
  Result := csSuccess;

  if VarIsEmpty(v) or VarIsNull(v) then begin
    Buf := nil;
    Size := 0;
  end
  else
    case TVarData(v).VType of
      varBoolean, varShortInt, varByte: begin
        Buf := @TVarData(v).VPointer;
        Size := 1;
      end;
      varSmallInt, varWord: begin
        Buf := @TVarData(v).VPointer;
        Size := 2;
      end;
      varInteger, varLongWord: begin
        Buf := @TVarData(v).VPointer;
        Size := 4;
      end;
      varInt64{$IFDEF USE_UINT64}, varUInt64{$ENDIF}: begin
        Buf := @TVarData(v).VPointer;
        Size := 8;
      end;
      varSingle: begin
        Buf := @TVarData(v).VPointer;
        Size := 4;
      end;
      varDouble, varCurrency: begin
        Buf := @TVarData(v).VPointer;
        Size := 8;
      end;
      varDate: begin
        Buf := @TVarData(v).VPointer;
        Size := 8;
      end;
      varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: begin
        Buf := TVarData(v).VPointer;
        Size := (Length(VarToStr(v)) + 1{0}) * sizeof(WideChar);
      end;
      varString: begin
        Buf := TVarData(v).VPointer;
        Size := Length(VarToStr(v)) + 1{0};
      end;
    else
      if (TVarData(V).VType and varArray) <> 0 then begin
        Buf := TVarData(v).VArray.Data;
        Size := TVarData(v).VArray.Bounds[0].ElementCount;
      end
      else begin
        Result := csInvalidDataMapping;
        Buf := nil;
        Size := 0;
      end;
    end;
end;

class procedure TDataConverters.InternalTimeStampToSQLTimeStamp(const TS: TTimeStamp; Dest: IntPtr);
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  SQLTimeStamp := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.DateTimeToSQLTimeStamp(TimeStampToDateTime(TS));
  PSQLTimeStamp(Dest)^ := SQLTimeStamp;
end;

class procedure TDataConverters.InternalTimeStampToSQLTimeStampOffset(const TS: TTimeStamp; Dest: IntPtr);
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  InternalTimeStampToSQLTimeStamp(TS, @SQLTimeStamp);
  InternalSQLTimeStampToSQLTimeStampOffset(@SQLTimeStamp, Dest);
end;

class procedure TDataConverters.InternalSQLTimeStampToSQLTimeStampOffset(Source: PSQLTimeStamp; Dest: PSQLTimeStampOffset);
var
  tz: Integer;
begin
  PSQLTimeStamp(Dest)^ := Source^;
  tz := GetLocalTimeZoneOffset;
  Dest.TimeZoneHour := tz div 60;
  if tz >= 0 then
    Dest.TimeZoneMinute := tz mod 60
  else
    Dest.TimeZoneMinute := -tz mod 60;
end;

class procedure TDataConverters.InternalSQLTimeStampOffsetToSQLTimeStamp(Source: PSQLTimeStampOffset; Dest: PSQLTimeStamp);
var
  SQLTimeStamp: TSQLTimeStamp;
  dt: TDateTime;
  tz: Integer;
begin
  SQLTimeStamp := PSQLTimeStamp(Source)^;
  dt := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.SQLTimeStampToDateTime(SQLTimeStamp);
  tz := GetLocalTimeZoneOffset;
  if Source.TimeZoneHour >= 0 then
    tz := tz - Source.TimeZoneHour * 60 - Source.TimeZoneMinute
  else
    tz := tz - Source.TimeZoneHour * 60 + Source.TimeZoneMinute;
  dt := dt + tz / 1440;
  Dest^ := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.DateTimeToSQLTimeStamp(dt);
  Dest.Fractions := Source.Fractions;
end;

class function TDataConverters.InternalSQLTimeStampToTimeStamp(Source: IntPtr; out Value: TTimeStamp): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
  dt: TDateTime;
begin
  Result := InternalReadSqlTimeStamp(Source, SQLTimeStamp);
  if Result <> csSuccess then begin
    Value.Date := DateDelta;
    Value.Time := 0;
  end
  else begin
    dt := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.SQLTimeStampToDateTime(SQLTimeStamp);
    Value := DateTimeToTimeStamp(dt);
  end;
end;

class function TDataConverters.InternalSQLTimeStampOffsetToTimeStamp(Source: IntPtr; out Value: TTimeStamp): TConvertStatus;
var
  SQLTimeStampOffset: TSQLTimeStampOffset;
  dt: TDateTime;
  tz: Integer;
begin
  Result := InternalReadSqlTimeStampOffset(Source, SQLTimeStampOffset);
  if Result <> csSuccess then begin
    Value.Date := DateDelta;
    Value.Time := 0;
  end
  else begin
    dt := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.SQLTimeStampToDateTime(PSQLTimeStamp(@SQLTimeStampOffset)^);
    tz := GetLocalTimeZoneOffset;
    if SQLTimeStampOffset.TimeZoneHour >= 0 then
      tz := tz - SQLTimeStampOffset.TimeZoneHour * 60 - SQLTimeStampOffset.TimeZoneMinute
    else
      tz := tz - SQLTimeStampOffset.TimeZoneHour * 60 + SQLTimeStampOffset.TimeZoneMinute;
    dt := dt + tz / 1440;
    Value := DateTimeToTimeStamp(dt);
  end;
end;

class function TDataConverters.InternalSQLTimeStampToDate(Source: IntPtr; out Date: Integer): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalSQLTimeStampToTimeStamp(Source, ts);
  Date := ts.Date;

  // Cast check
  if (Result = csSuccess) and (ts.Time <> 0)then
    Result := csDataTruncated;
end;

class function TDataConverters.InternalSQLTimeStampToTime(Source: IntPtr; out Time: Integer): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalSQLTimeStampToTimeStamp(Source, ts);
  Time := ts.Time;

  // Cast check
  if (Result = csSuccess) and (ts.Date <> 0) and (ts.Date <> DateDelta) then
    Result := csDataTruncated;
end;

class function TDataConverters.ValidateSQLTimeStamp(PValue: PSQLTimeStamp): TConvertStatus;
begin
  if PValue.Year + PValue.Month + PValue.day +
     PValue.Hour + PValue.Minute + PValue.Second > 0 then begin
    if PValue.Year + PValue.Month + PValue.Day > 0 then
      if (PValue.Year = 0) or (PValue.Month = 0) or
       (PValue.Day = 0) or (PValue.Month > 31) or (PValue.Day > DaysInAMonth(PValue.Year, PValue.Month)) then begin
         PValue^ := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.DateTimeToSQLTimeStamp(0);
         Result := csInvalidSQLTimeStampValue;
         Exit;
      end;
    if PValue.Hour + PValue.Minute + PValue.Second > 0 then
       if (PValue.Hour > 23) or (PValue.Second > 59) or (PValue.Minute > 59) then begin
         PValue^ := {$IFNDEF FPC}SqlTimSt{$ELSE}CRTimeStamp{$ENDIF}.DateTimeToSQLTimeStamp(0);
         Result := csInvalidSQLTimeStampValue;
         Exit;
       end;
  end;
  Result := csSuccess;
end;

class function TDataConverters.ValidateSQLTimeStampOffset(PValue: PSQLTimeStampOffset): TConvertStatus;
begin
  Result := ValidateSQLTimeStamp(PSQLTimeStamp(PValue));
  if Result <> csSuccess then
    Exit;

  if (PValue.TimeZoneMinute < 0) or (PValue.TimeZoneMinute > 59) then begin
    PValue.TimeZoneMinute := 0;
    Result := csInvalidSQLTimeStampValue;
  end;
end;

class function TDataConverters.InternalReadTimeStamp(Source: IntPtr; out ts: TTimeStamp): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source));
{$IFDEF FPC}
  ts := MSecsToTimeStamp(Trunc(d));
{$ELSE}
  ts := MSecsToTimeStamp(d);
{$ENDIF}

  Result := csSuccess;
  if ts.Date < 0 then begin
    ts.Date := DateDelta;
    Result := csInvalidDateTimeValue;
  end;

  if ts.Time < 0 then begin
    ts.Time := 0;
    Result := csInvalidDateTimeValue;
  end;
end;

class function TDataConverters.InternalReadSQLTimeStamp(Source: IntPtr; out SQLTimeStamp: TSQLTimeStamp): TConvertStatus;
begin
  SQLTimeStamp := PSQLTimeStamp(Source)^;
//{$IFDEF ODBC_DRIVER}
//  SQLTimeStamp.Fractions := (SQLTimeStamp.Fractions + 500000) div 1000000;
//{$ENDIF}
  Result := ValidateSQLTimeStamp(@SQLTimeStamp);
end;

class function TDataConverters.InternalReadSQLTimeStampOffset(Source: IntPtr; out SQLTimeStampOffset: TSQLTimeStampOffset): TConvertStatus;
begin
  SQLTimeStampOffset := PSQLTimeStampOffset(Source)^;
  Result := ValidateSQLTimeStampOffset(@SQLTimeStampOffset);
end;

class procedure TDataConverters.ChangeDecimalSeparator(var Num: string);
begin
  ChangeDecimalSeparator(Num, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
end;

class procedure TDataConverters.ChangeDecimalSeparator(var Num: string; const NewDecimalSeparator: Char);
var
  Len: Integer;
  PNum, PEnd: PChar;
begin
  Len := Length(Num);
  if Len > 0 then begin
    PNum := @Num[1];
    PEnd := @Num[Len];
    while PNum <= PEnd do begin
      if (PNum^ = #$2C) or (PNum^ = #$2E) then begin
        PNum^ := NewDecimalSeparator;
        break;
      end;
      Inc(PNum);
    end;
  end;
end;

class function TDataConverters.CopyByte(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteByte(ConvertInfo.Dest, Marshal.ReadByte(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.CopyInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt16(ConvertInfo.Dest, Marshal.ReadInt16(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.CopyInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, Marshal.ReadInt32(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.CopyInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Marshal.ReadInt64(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.CopyPtr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteIntPtr(ConvertInfo.Dest, Marshal.ReadIntPtr(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.Int8ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  b: byte;
begin
  b := Marshal.ReadByte(ConvertInfo.Source);

  if b and $80 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteByte(ConvertInfo.Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(ConvertInfo.Dest, b);
  end;
end;

class function TDataConverters.Int8ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt16(ConvertInfo.Dest, ShortInt(Marshal.ReadByte(ConvertInfo.Source)));
  Result := csSuccess;
end;

class function TDataConverters.Int8ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  b: byte;
begin
  b := Marshal.ReadByte(ConvertInfo.Source);

  if b and $80 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt16(ConvertInfo.Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(b));
  end;
end;

class function TDataConverters.Int8ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, ShortInt(Marshal.ReadByte(ConvertInfo.Source)));
  Result := csSuccess;
end;

class function TDataConverters.Int8ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  b: byte;
begin
  b := Marshal.ReadByte(ConvertInfo.Source);

  if b and $80 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt32(ConvertInfo.Dest, Integer(b));
  end;
end;

class function TDataConverters.Int8ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, ShortInt(Marshal.ReadByte(ConvertInfo.Source)));
  Result := csSuccess;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.Int8ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  b: byte;
begin
  b := Marshal.ReadByte(ConvertInfo.Source);

  if b and $80 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt64(ConvertInfo.Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt64(ConvertInfo.Dest, Int64(b));
  end;
end;

{$ENDIF}

class function TDataConverters.Int8ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := ShortInt(Marshal.ReadByte(ConvertInfo.Source));
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.Int8ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := ShortInt(Marshal.ReadByte(ConvertInfo.Source));
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.Int8ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := ShortInt(Marshal.ReadByte(ConvertInfo.Source));
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.Int8ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
begin
  i := ShortInt(Marshal.ReadByte(ConvertInfo.Source)) * 10000;
  Result := WriteInt64AsBCD(i, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int8ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
  bcd: TBcd;
begin
  i := ShortInt(Marshal.ReadByte(ConvertInfo.Source));
  bcd := StrToBcd(IntToStr(i));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int8ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  if Marshal.ReadByte(ConvertInfo.Source) = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.Int8ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt8ToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int8ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt8ToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int8ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt8ToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int8ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt8ToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int8ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Shortint);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int8ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Shortint);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int8ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Shortint);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int8ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Shortint);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int8ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := ShortInt(Marshal.ReadByte(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.Int8ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Shortint);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt8ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  b: byte;
begin
  b := Marshal.ReadByte(ConvertInfo.Source);

  if b and $80 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteByte(ConvertInfo.Dest, Byte($7F));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(ConvertInfo.Dest, b);
  end;
end;

class function TDataConverters.UInt8ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(Marshal.ReadByte(ConvertInfo.Source)));
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(Marshal.ReadByte(ConvertInfo.Source)));
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, Marshal.ReadByte(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, Marshal.ReadByte(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Marshal.ReadByte(ConvertInfo.Source));
  Result := csSuccess;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.UInt8ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Marshal.ReadByte(ConvertInfo.Source));
  Result := csSuccess;
end;

{$ENDIF}

class function TDataConverters.UInt8ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := Marshal.ReadByte(ConvertInfo.Source);
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := Marshal.ReadByte(ConvertInfo.Source);
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Marshal.ReadByte(ConvertInfo.Source);
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  lw: Cardinal;
begin
  lw := Cardinal(Marshal.ReadByte(ConvertInfo.Source)) * 10000;
  Result := WriteInt64AsBCD(lw, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt8ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
  bcd: TBcd;
begin
  i := Marshal.ReadByte(ConvertInfo.Source);
  bcd := StrToBcd(IntToStr(i));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt8ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  if Marshal.ReadByte(ConvertInfo.Source) = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt8ToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt8ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt8ToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt8ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt8ToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt8ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt8ToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt8ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Byte);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt8ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Byte);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt8ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Byte);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt8ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Byte);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt8ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := Marshal.ReadByte(ConvertInfo.Source);
  Result := csSuccess;
end;

class function TDataConverters.UInt8ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Byte);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int16ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  w := Word(Marshal.ReadInt16(ConvertInfo.Source));

  if (w and $FF80 <> $0000) and (w and $FF80 <> $FF80) then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else if w and $8000 = 0 then
      Marshal.WriteByte(ConvertInfo.Dest, Byte($7F))
    else
      Marshal.WriteByte(ConvertInfo.Dest, Byte($80));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(ConvertInfo.Dest, Byte(w));
  end;
end;

class function TDataConverters.Int16ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  w := Word(Marshal.ReadInt16(ConvertInfo.Source));

  if w and $FF00 <> $0000 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else if w and $8000 = 0 then
      Marshal.WriteByte(ConvertInfo.Dest, Byte($FF))
    else
      Marshal.WriteByte(ConvertInfo.Dest, Byte($00));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(ConvertInfo.Dest, Byte(w));
  end;
end;

class function TDataConverters.Int16ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  w := Word(Marshal.ReadInt16(ConvertInfo.Source));

  if w and $8000 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt16(ConvertInfo.Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(w));
  end;
end;

class function TDataConverters.Int16ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, Marshal.ReadInt16(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.Int16ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  w := Word(Marshal.ReadInt16(ConvertInfo.Source));

  if w and $8000 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt32(ConvertInfo.Dest, w);
  end;
end;

class function TDataConverters.Int16ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Marshal.ReadInt16(ConvertInfo.Source));
  Result := csSuccess;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.Int16ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  w := Word(Marshal.ReadInt16(ConvertInfo.Source));

  if w and $8000 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt64(ConvertInfo.Dest, 0);
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt64(ConvertInfo.Dest, w);
  end;
end;

{$ENDIF}

class function TDataConverters.Int16ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := Marshal.ReadInt16(ConvertInfo.Source);
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.Int16ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := Marshal.ReadInt16(ConvertInfo.Source);
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.Int16ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Marshal.ReadInt16(ConvertInfo.Source);
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.Int16ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i: Integer;
begin
  i := Integer(Marshal.ReadInt16(ConvertInfo.Source)) * 10000;
  Result := WriteInt64AsBCD(i, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int16ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i: Integer;
  bcd: TBcd;
begin
  i := Marshal.ReadInt16(ConvertInfo.Source);
  bcd := StrToBcd(IntToStr(i));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int16ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  if Marshal.ReadInt16(ConvertInfo.Source) = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.Int16ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt16ToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int16ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt16ToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int16ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt16ToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int16ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt16ToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int16ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Smallint);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int16ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Smallint);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int16ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Smallint);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int16ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Smallint);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int16ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := Marshal.ReadInt16(ConvertInfo.Source);
  Result := csSuccess;
end;

class function TDataConverters.Int16ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Smallint);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt16ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  w := Word(Marshal.ReadInt16(ConvertInfo.Source));

  if w and $FF80 <> $0000 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteByte(ConvertInfo.Dest, Byte($7F));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(ConvertInfo.Dest, Byte(w));
  end;
end;

class function TDataConverters.UInt16ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  w := Word(Marshal.ReadInt16(ConvertInfo.Source));

  if w and $FF00 <> $0000 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteByte(ConvertInfo.Dest, Byte($FF));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(ConvertInfo.Dest, Byte(w));
  end;
end;

class function TDataConverters.UInt16ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  w := Word(Marshal.ReadInt16(ConvertInfo.Source));

  if w and $8000 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt16(ConvertInfo.Dest, SmallInt($7FFF));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(w));
  end;
end;

class function TDataConverters.UInt16ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, Word(Marshal.ReadInt16(ConvertInfo.Source)));
  Result := csSuccess;
end;

class function TDataConverters.UInt16ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, Word(Marshal.ReadInt16(ConvertInfo.Source)));
  Result := csSuccess;
end;

class function TDataConverters.UInt16ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Word(Marshal.ReadInt16(ConvertInfo.Source)));
  Result := csSuccess;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.UInt16ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Word(Marshal.ReadInt16(ConvertInfo.Source)));
  Result := csSuccess;
end;

{$ENDIF}

class function TDataConverters.UInt16ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := Word(Marshal.ReadInt16(ConvertInfo.Source));
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.UInt16ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := Word(Marshal.ReadInt16(ConvertInfo.Source));
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.UInt16ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Word(Marshal.ReadInt16(ConvertInfo.Source));
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.UInt16ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  lw: Cardinal;
begin
  lw := Cardinal(Word(Marshal.ReadInt16(ConvertInfo.Source))) * 10000;
  Result := WriteInt64AsBCD(lw, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt16ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i: Integer;
  bcd: TBcd;
begin
  i := Word(Marshal.ReadInt16(ConvertInfo.Source));
  bcd := StrToBcd(IntToStr(i));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt16ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  if Marshal.ReadInt16(ConvertInfo.Source) = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.UInt16ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt16ToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt16ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt16ToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt16ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt16ToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt16ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt16ToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt16ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Word);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt16ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Word);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt16ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Word);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt16ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Word);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt16ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := Word(Marshal.ReadInt16(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.UInt16ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Word);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalInt32ToInt8(Marshal.ReadInt32(ConvertInfo.Source), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalInt32ToUInt8(Marshal.ReadInt32(ConvertInfo.Source), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalInt32ToInt16(Marshal.ReadInt32(ConvertInfo.Source), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalInt32ToUInt16(Marshal.ReadInt32(ConvertInfo.Source), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalInt32ToUInt32(Marshal.ReadInt32(ConvertInfo.Source), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Marshal.ReadInt32(ConvertInfo.Source));
  Result := csSuccess;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.Int32ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalInt32ToUInt64(Marshal.ReadInt32(ConvertInfo.Source), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.Int32ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := Marshal.ReadInt32(ConvertInfo.Source);
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.Int32ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := Marshal.ReadInt32(ConvertInfo.Source);
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.Int32ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Marshal.ReadInt32(ConvertInfo.Source);
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.Int32ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Int64(Marshal.ReadInt32(ConvertInfo.Source)) * 10000;
  Result := WriteInt64AsBCD(i64, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i: Integer;
  bcd: TBcd;
begin
  i := Marshal.ReadInt32(ConvertInfo.Source);
  bcd := StrToBcd(IntToStr(i));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  if Marshal.ReadInt32(ConvertInfo.Source) = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.Int32ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt32ToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int32ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt32ToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int32ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt32ToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int32ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt32ToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int32ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int32ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := Marshal.ReadInt32(ConvertInfo.Source);
  Result := csSuccess;
end;

class function TDataConverters.Int32ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt32ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  lw: Cardinal;
begin
  lw := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));

  if lw and $FFFFFF80 <> $00000000 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteByte(ConvertInfo.Dest, Byte($7F));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(ConvertInfo.Dest, Byte(lw));
  end;
end;

class function TDataConverters.UInt32ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  lw: Cardinal;
begin
  lw := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));

  if lw and $FFFFFF00 <> $00000000 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteByte(ConvertInfo.Dest, Byte($FF));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteByte(ConvertInfo.Dest, Byte(lw));
  end;
end;

class function TDataConverters.UInt32ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  lw: Cardinal;
begin
  lw := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));

  if lw and $FFFF8000 <> $00000000 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt16(ConvertInfo.Dest, SmallInt($7FFF));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(Word(lw)));
  end;
end;

class function TDataConverters.UInt32ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  lw: Cardinal;
begin
  lw := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));

  if lw and $FFFF0000 <> $00000000 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt16(ConvertInfo.Dest, SmallInt($FFFF));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(Word(lw)));
  end;
end;

class function TDataConverters.UInt32ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  lw: Cardinal;
begin
  lw := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));

  if lw and $80000000 <> 0 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      Marshal.WriteInt32(ConvertInfo.Dest, Integer($7FFFFFFF));
  end
  else begin
    Result := csSuccess;
    Marshal.WriteInt32(ConvertInfo.Dest, Integer(lw));
  end;
end;

class function TDataConverters.UInt32ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Cardinal(Marshal.ReadInt32(ConvertInfo.Source)));
  Result := csSuccess;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.UInt32ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Cardinal(Marshal.ReadInt64(ConvertInfo.Source)));
  Result := csSuccess;
end;

{$ENDIF}

class function TDataConverters.UInt32ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.UInt32ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.UInt32ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.UInt32ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Int64(Cardinal(Marshal.ReadInt32(ConvertInfo.Source))) * 10000;
  Result := WriteInt64AsBCD(i64, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt32ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  bcd: TBcd;
begin
  i64 := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));
  bcd := StrToBcd(IntToStr(i64));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt32ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  if Marshal.ReadInt32(ConvertInfo.Source) = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.UInt32ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt32ToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt32ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt32ToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt32ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt32ToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt32ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt32ToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt32ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Cardinal);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt32ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Cardinal);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt32ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Cardinal);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt32ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Cardinal);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt32ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := Cardinal(Marshal.ReadInt32(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.UInt32ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Cardinal);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  Result := InternalInt64ToInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  Result := InternalInt64ToUInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  Result := InternalInt64ToInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  Result := InternalInt64ToUInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  Result := InternalInt64ToInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  Result := InternalInt64ToUInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.Int64ToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  Result := InternalInt64ToUInt64(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.Int64ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := Marshal.ReadInt64(ConvertInfo.Source);
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.Int64ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := Marshal.ReadInt64(ConvertInfo.Source);
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.Int64ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Marshal.ReadInt64(ConvertInfo.Source);
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.Int64ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);

  // Cast check
  if i64 < -922337203685477 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      i64 := -9223372036854770000;
  end
  else if i64 > 922337203685477 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      i64 := 9223372036854770000;
  end
  else begin
    Result := csSuccess;
    {$Q-} // Bug in Delphi on Overflow checking when i64 <= -922337203685477
    i64 := i64 * 10000;
  end;

  r := WriteInt64AsBCD(i64, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.Int64ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  bcd: TBcd;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  bcd := StrToBcd(IntToStr(i64));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  if Marshal.ReadInt64(ConvertInfo.Source) = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.Int64ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt64ToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int64ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt64ToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int64ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt64ToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int64ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalInt64ToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.Int64ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.Int64ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := Marshal.ReadInt64(ConvertInfo.Source);
  Result := csSuccess;
end;

class function TDataConverters.Int64ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.UInt64ToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalUInt64ToInt8(ui64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalUInt64ToUInt8(ui64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalUInt64ToInt16(ui64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalUInt64ToUInt16(ui64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalUInt64ToInt32(ui64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalUInt64ToUInt32(ui64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalUInt64ToInt64(ui64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.UInt64ToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.UInt64ToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.UInt64ToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
  r: TConvertStatus;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));

  // Cast check
  if ui64 > 922337203685477 then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      ui64 := 9223372036854770000;
  end
  else begin
    Result := csSuccess;
    {$Q-} // Bug in Delphi on Overflow checking when i64 <= -922337203685477
    ui64 := ui64 * 10000;
  end;

  r := WriteUInt64AsBCD(ui64, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.UInt64ToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
  bcd: TBcd;
begin
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  bcd := StrToBcd({$IFDEF FPC}IntToStr{$ELSE}UIntToStr{$ENDIF}(ui64));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  if Marshal.ReadInt64(ConvertInfo.Source) = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.UInt64ToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt64ToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt64ToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt64ToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt64ToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt64ToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt64ToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalUInt64ToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.UInt64ToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(UInt64);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(UInt64);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(UInt64);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(UInt64);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.UInt64ToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  ui64: UInt64;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  ui64 := UInt64(Marshal.ReadInt64(ConvertInfo.Source));
  VariantObj.Value := ui64;
  Result := csSuccess;
end;

class function TDataConverters.UInt64ToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(UInt64);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.SingleToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalSingleToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalSingleToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalSingleToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalSingleToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalSingleToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalSingleToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  Result := InternalSingleToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, i64);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.SingleToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  Result := InternalSingleToUInt64(ConvertInfo.Source, ui64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, Int64(ui64));
end;

{$ENDIF}

class function TDataConverters.SingleToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(ConvertInfo.Source));
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.SingleToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(ConvertInfo.Source));
  Extended(ConvertInfo.Dest^) := s;
  Result := csSuccess;
end;

class function TDataConverters.SingleToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(ConvertInfo.Source));
  Result := InternalExtendedToBCD(s, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SingleToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
  bcd: TBcd;
  r: TConvertStatus;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(ConvertInfo.Source));

  if not TryStrToBcd(FloatToStr(s), bcd) then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      bcd := Bcd_0;
  end
  else
    Result := csSuccess;

  r := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(ConvertInfo.Source));
  if s = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.SingleToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalSingleToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalSingleToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalSingleToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalSingleToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.SingleToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Single);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SingleToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Single);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SingleToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Single);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SingleToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Single);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SingleToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.SingleToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Single);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FloatToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFloatToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFloatToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFloatToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFloatToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFloatToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFloatToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  Result := InternalFloatToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, i64);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.FloatToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  Result := InternalFloatToUInt64(ConvertInfo.Source, ui64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, Int64(ui64));
end;

{$ENDIF}

class function TDataConverters.FloatToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ConvertInfo.Source));

  if d < -MaxSingle then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      d := -MaxSingle;
  end
  else
  if d > MaxSingle then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      d := MaxSingle;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(d));
end;

class function TDataConverters.FloatToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ConvertInfo.Source));
  Extended(ConvertInfo.Dest^) := d;
  Result := csSuccess;
end;

class function TDataConverters.FloatToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalExtendedToBCD(d, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.CurrencyToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ConvertInfo.Source));
  Result := InternalCurrencyToBCD(d, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FloatToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
  bcd: TBcd;
  r: TConvertStatus;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ConvertInfo.Source));

  if not TryStrToBcd(FloatToStr(d), bcd) then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      bcd := Bcd_0;
  end
  else
    Result := csSuccess;

  r := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ConvertInfo.Source));
  if d = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.FloatToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalFloatToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalFloatToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalFloatToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalFloatToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FloatToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Double);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FloatToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Double);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FloatToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Double);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FloatToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Double);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FloatToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ConvertInfo.Source));
  Result := csSuccess;
end;

class function TDataConverters.FloatToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Double);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtendedToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalExtendedToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalExtendedToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalExtendedToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalExtendedToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalExtendedToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalExtendedToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  Result := InternalExtendedToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, i64);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.ExtendedToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  Result := InternalExtendedToUInt64(ConvertInfo.Source, ui64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, Int64(ui64));
end;

{$ENDIF}

class function TDataConverters.ExtendedToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Extended(ConvertInfo.Source^);

  if e < -MaxSingle then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      e := -MaxSingle;
  end
  else
  if e > MaxSingle then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      e := MaxSingle;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(e));
end;

class function TDataConverters.ExtendedToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Extended(ConvertInfo.Source^);
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(e));
  Result := csSuccess;
end;

class function TDataConverters.ExtendedToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Extended(ConvertInfo.Source^);
  Result := InternalExtendedToBCD(e, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtendedToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
  bcd: TBcd;
  r: TConvertStatus;
begin
  e := Extended(ConvertInfo.Source^);

  if not TryStrToBcd(FloatToStr(e), bcd) then begin
    Result := csValueOutOfRange;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      bcd := Bcd_0;
  end
  else
    Result := csSuccess;

  r := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Extended(ConvertInfo.Source^);
  if e = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
  Result := csSuccess;
end;

class function TDataConverters.ExtendedToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalExtendedToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalExtendedToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalExtendedToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalExtendedToStr(ConvertInfo.Source, Str, ConvertInfo.DestLen);
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.ExtendedToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Extended);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtendedToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Extended);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtendedToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Extended);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtendedToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Extended);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtendedToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := Extended(ConvertInfo.Source^);
  Result := csSuccess;
end;

class function TDataConverters.ExtendedToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Extended);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BCDToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  Result := InternalBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, i64);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.BCDToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  Result := InternalBCDToUInt64(ConvertInfo.Source, ui64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, Int64(ui64));
end;

{$ENDIF}

class function TDataConverters.BCDToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := Marshal.ReadInt64(ConvertInfo.Source) / 10000;
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.BCDToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := Marshal.ReadInt64(ConvertInfo.Source) / 10000;
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.BCDToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Marshal.ReadInt64(ConvertInfo.Source) / 10000;
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.BCDToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  Result := WriteInt64AsBCD(i64, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BCDToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  bcd: TBcd;
begin
  i64 := Marshal.ReadInt64(ConvertInfo.Source);
  bcd := StrToBcd(IntToStr(i64));
  bcd.SignSpecialPlaces := bcd.SignSpecialPlaces or $04;
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BCDToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  Result := InternalBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  if i64 = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
end;

class function TDataConverters.BCDToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalBCDToStr(ConvertInfo.Source,  Str, ConvertInfo.DestLen);
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalBCDToStr(ConvertInfo.Source,  Str, ConvertInfo.DestLen);
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalBCDToStr(ConvertInfo.Source,  Str, ConvertInfo.DestLen);
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalBCDToStr(ConvertInfo.Source,  Str, ConvertInfo.DestLen);
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.BCDToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BCDToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BCDToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BCDToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BCDToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  d: Double;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  d := Marshal.ReadInt64(ConvertInfo.Source) / 10000;
  VariantObj.Value := d;
  Result := csSuccess;
end;

class function TDataConverters.BCDToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FMTBCDToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt8(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt16(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  r := InternalInt64ToUInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  Result := InternalFMTBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, i64);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.FMTBCDToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ui64: UInt64;
begin
  Result := InternalFMTBCDToUInt64(ConvertInfo.Source, ui64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  Marshal.WriteInt64(ConvertInfo.Dest, Int64(ui64));
end;

{$ENDIF}

class function TDataConverters.FMTBCDToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := BcdToDouble(PBcd(ConvertInfo.Source)^);
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.FMTBCDToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := BcdToDouble(PBcd(ConvertInfo.Source)^);
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.FMTBCDToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := BcdToDouble(PBcd(ConvertInfo.Source)^);
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.FMTBCDToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  bcd: TBcd;
begin
  bcd := PBcd(ConvertInfo.Source)^;
  Result := InternalFMTBCDToBCDAsInt64(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FMTBCDToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  bcd: TBcd;
begin
  bcd := PBcd(ConvertInfo.Source)^;
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FMTBCDToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  Result := InternalFMTBCDToInt64(ConvertInfo.Source, i64);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  if i64 = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
end;

class function TDataConverters.FMTBCDToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToStr(ConvertInfo.Source,  Str, ConvertInfo.DestLen);
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToStr(ConvertInfo.Source,  Str, ConvertInfo.DestLen);
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToStr(ConvertInfo.Source,  Str, ConvertInfo.DestLen);
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalFMTBCDToStr(ConvertInfo.Source,  Str, ConvertInfo.DestLen);
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TDataConverters.FMTBCDToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TBcd);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FMTBCDToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TBcd);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FMTBCDToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TBcd);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FMTBCDToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TBcd);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.FMTBCDToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  d: Double;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  d := BcdToDouble(PBcd(ConvertInfo.Source)^);
  VariantObj.Value := d;
  Result := csSuccess;
end;

class function TDataConverters.FMTBCDToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TBcd);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BoolToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteByte(ConvertInfo.Dest, Byte(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))));
  Result := csSuccess;
end;

class function TDataConverters.BoolToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteByte(ConvertInfo.Dest, Byte(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))));
  Result := csSuccess;
end;

class function TDataConverters.BoolToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))));
  Result := csSuccess;
end;

class function TDataConverters.BoolToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))));
  Result := csSuccess;
end;

class function TDataConverters.BoolToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, Integer(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))));
  Result := csSuccess;
end;

class function TDataConverters.BoolToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt32(ConvertInfo.Dest, Integer(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))));
  Result := csSuccess;
end;

class function TDataConverters.BoolToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Int64(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))));
  Result := csSuccess;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.BoolToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(ConvertInfo.Dest, Int64(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))));
  Result := csSuccess;
end;

{$ENDIF}

class function TDataConverters.BoolToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  s: Single;
begin
  s := Integer(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  Marshal.WriteInt32(ConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(s));
  Result := csSuccess;
end;

class function TDataConverters.BoolToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
begin
  d := Integer(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(d));
  Result := csSuccess;
end;

class function TDataConverters.BoolToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  e: Extended;
begin
  e := Integer(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.BoolToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Integer;
begin
  i64 := Integer(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)))) * 10000;
  Result := WriteInt64AsBCD(i64, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BoolToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
  bcd: TBcd;
begin
  i := Integer(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  bcd := StrToBcd(IntToStr(i));
  Result := InternalBCDToFMTBCD(bcd, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BoolToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalBoolToStr(ConvertInfo.Source);
  Result := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.BoolToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalBoolToStr(ConvertInfo.Source);
  Result := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.BoolToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalBoolToStr(ConvertInfo.Source);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.BoolToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  Str := InternalBoolToStr(ConvertInfo.Source);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.BoolToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  ConvertInfo.SourceLen := SizeOf(w);
  w := Word(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  Result := InternalExactCopyToBytes(@w, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BoolToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  ConvertInfo.SourceLen := SizeOf(w);
  w := Word(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  Result := InternalExactCopyToVarBytes(@w, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BoolToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  ConvertInfo.SourceLen := SizeOf(w);
  w := Word(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, @w, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BoolToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  ConvertInfo.SourceLen := SizeOf(w);
  w := Word(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, @w, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BoolToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source)));
  Result := csSuccess;
end;

class function TDataConverters.BoolToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  ConvertInfo.SourceLen := SizeOf(WordBool);
  w := Word(Boolean(WordBool(Marshal.ReadInt16(ConvertInfo.Source))));
  Result := InternalExactCopyToBlob(@w, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  ts.Date := Marshal.ReadInt32(ConvertInfo.Source);
  ts.Time := 0;
  if ts.Date <= 0 then begin
    Result := csInvalidDateTimeValue;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      ts.Date := DateDelta;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(ts)));
end;

class function TDataConverters.DateToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  ts.Date := Marshal.ReadInt32(ConvertInfo.Source);
  ts.Time := 0;
  if ts.Date <= 0 then begin
    Result := csInvalidDateTimeValue;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      ts.Date := DateDelta;
  end
  else
    Result := csSuccess;

  InternalTimeStampToSQLTimeStamp(ts, ConvertInfo.Dest);
end;

class function TDataConverters.DateToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  ts.Date := Marshal.ReadInt32(ConvertInfo.Source);
  ts.Time := 0;
  if ts.Date <= 0 then begin
    Result := csInvalidDateTimeValue;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      ts.Date := DateDelta;
  end
  else
    Result := csSuccess;

  InternalTimeStampToSQLTimeStampOffset(ts, ConvertInfo.Dest);
end;

class function TDataConverters.DateToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalDateToStr(ConvertInfo.Source, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.DateToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalDateToStr(ConvertInfo.Source, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.DateToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalDateToStr(ConvertInfo.Source, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.DateToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalDateToStr(ConvertInfo.Source, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.DateToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  ts: TTimeStamp;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  ts.Date := Marshal.ReadInt32(ConvertInfo.Source);
  ts.Time := 0;
  VariantObj.Value := TimeStampToDateTime(ts);
  Result := csSuccess;
end;

class function TDataConverters.DateToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.TimeToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  ts.Date := DateDelta;
  ts.Time := Marshal.ReadInt32(ConvertInfo.Source);
  if ts.Time < 0 then begin
    Result := csInvalidDateTimeValue;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      ts.Time := 0;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(ts)));
end;

class function TDataConverters.TimeToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  ts.Date := DateDelta;
  ts.Time := Marshal.ReadInt32(ConvertInfo.Source);
  if ts.Time < 0 then begin
    Result := csInvalidDateTimeValue;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      ts.Time := 0;
  end
  else
    Result := csSuccess;

  InternalTimeStampToSQLTimeStamp(ts, ConvertInfo.Dest);
end;

class function TDataConverters.TimeToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  ts.Date := DateDelta;
  ts.Time := Marshal.ReadInt32(ConvertInfo.Source);
  if ts.Time < 0 then begin
    Result := csInvalidDateTimeValue;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit
    else
      ts.Time := 0;
  end
  else
    Result := csSuccess;

  InternalTimeStampToSQLTimeStampOffset(ts, ConvertInfo.Dest);
end;

class function TDataConverters.TimeToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalTimeToStr(ConvertInfo.Source, 0, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.TimeToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalTimeToStr(ConvertInfo.Source, 0, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.TimeToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalTimeToStr(ConvertInfo.Source, 0, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.TimeToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalTimeToStr(ConvertInfo.Source, 0, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.TimeToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.TimeToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.TimeToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.TimeToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.TimeToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  ts: TTimeStamp;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  ts.Date := DateDelta;
  ts.Time := Marshal.ReadInt32(ConvertInfo.Source);
  VariantObj.Value := TimeStampToDateTime(ts);
  Result := csSuccess;
end;

class function TDataConverters.TimeToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Integer);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateTimeToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalReadTimeStamp(ConvertInfo.Source, ts);

  if Result = csSuccess then begin
    // Cast check
    if ts.Time <> 0 then begin
      Result := csDataTruncated;
      if not ConvertInfo.IgnoreConvertErrors then
        Exit;
    end
  end
  else
    if not ConvertInfo.IgnoreConvertErrors then
      Exit;

  Marshal.WriteInt32(ConvertInfo.Dest, ts.Date);
end;

class function TDataConverters.DateTimeToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalReadTimeStamp(ConvertInfo.Source, ts);

  if Result = csSuccess then begin
    // Cast check
    if (ts.Date <> 0) and (ts.Date <> DateDelta) then begin
      Result := csDataTruncated;
      if not ConvertInfo.IgnoreConvertErrors then
        Exit;
    end;
  end
  else
    if not ConvertInfo.IgnoreConvertErrors then
      Exit;

  Marshal.WriteInt32(ConvertInfo.Dest, ts.Time);
end;

class function TDataConverters.DateTimeToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalReadTimeStamp(ConvertInfo.Source, ts);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  InternalTimeStampToSQLTimeStamp(ts, ConvertInfo.Dest);
end;

class function TDataConverters.DateTimeToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalReadTimeStamp(ConvertInfo.Source, ts);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  InternalTimeStampToSQLTimeStampOffset(ts, ConvertInfo.Dest);
end;

class function TDataConverters.DateTimeToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalDateTimeToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.DateTimeToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalDateTimeToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.DateTimeToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalDateTimeToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.DateTimeToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalDateTimeToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.DateTimeToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateTimeToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateTimeToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateTimeToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.DateTimeToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  ts: TTimeStamp;
begin
  Result := InternalReadTimeStamp(ConvertInfo.Source, ts);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := TimeStampToDateTime(ts);
end;

class function TDataConverters.DateTimeToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(Int64);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SQLTimeStampToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Date: Integer;
begin
  Result := InternalSQLTimeStampToDate(ConvertInfo.Source, Date);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  Marshal.WriteInt32(ConvertInfo.Dest, Date);
end;

class function TDataConverters.SQLTimeStampToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Time: Integer;
begin
  Result := InternalSQLTimeStampToTime(ConvertInfo.Source, Time);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  Marshal.WriteInt32(ConvertInfo.Dest, Time);
end;

class function TDataConverters.SQLTimeStampToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalSQLTimeStampToTimeStamp(ConvertInfo.Source, ts);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(ts)));
end;

class function TDataConverters.SQLTimeStampToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  Result := InternalReadSQLTimeStamp(ConvertInfo.Source, SQLTimeStamp);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  InternalSQLTimeStampToSQLTimeStampOffset(@SQLTimeStamp, ConvertInfo.Dest);
end;

class function TDataConverters.SQLTimeStampToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalSQLTimeStampToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.SQLTimeStampToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalSQLTimeStampToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.SQLTimeStampToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalSQLTimeStampToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.SQLTimeStampToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalSQLTimeStampToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.SQLTimeStampToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStamp);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SQLTimeStampToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStamp);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SQLTimeStampToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStamp);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SQLTimeStampToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStamp);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

{$IFNDEF FPC}

class function TDataConverters.SQLTimeStampToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  ts: TTimeStamp;
begin
  Result := InternalSQLTimeStampToTimeStamp(ConvertInfo.Source, ts);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := TimeStampToDateTime(ts);
end;

{$ENDIF}

class function TDataConverters.SQLTimeStampToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStamp);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SQLTimeStampOffsetToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
  Date: Integer;
begin
  InternalSQLTimeStampOffsetToSQLTimeStamp(ConvertInfo.Source, @SQLTimeStamp);
  Result := InternalSQLTimeStampToDate(@SQLTimeStamp, Date);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  Marshal.WriteInt32(ConvertInfo.Dest, Date);
end;

class function TDataConverters.SQLTimeStampOffsetToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
  Time: Integer;
begin
  InternalSQLTimeStampOffsetToSQLTimeStamp(ConvertInfo.Source, @SQLTimeStamp);
  Result := InternalSQLTimeStampToDate(@SQLTimeStamp, Time);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  Marshal.WriteInt32(ConvertInfo.Dest, Time);
end;

class function TDataConverters.SQLTimeStampOffsetToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
  ts: TTimeStamp;
begin
  InternalSQLTimeStampOffsetToSQLTimeStamp(ConvertInfo.Source, @SQLTimeStamp);
  Result := InternalSQLTimeStampToTimeStamp(@SQLTimeStamp, ts);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(ts)));
end;

class function TDataConverters.SQLTimeStampOffsetToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStampOffset: TSQLTimeStampOffset;
begin
  Result := InternalReadSQLTimeStampOffset(ConvertInfo.Source, SQLTimeStampOffset);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;
  InternalSQLTimeStampOffsetToSQLTimeStamp(@SQLTimeStampOffset, ConvertInfo.Dest);
end;

class function TDataConverters.SQLTimeStampOffsetToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalSQLTimeStampOffsetToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteAStr(AnsiString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.SQLTimeStampOffsetToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalSQLTimeStampOffsetToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteWStr(WideString(Str), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.SQLTimeStampOffsetToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalSQLTimeStampOffsetToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.SQLTimeStampOffsetToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
  r: TConvertStatus;
begin
  Result := InternalSQLTimeStampOffsetToStr(ConvertInfo.Source, ConvertInfo.SourceScale, Str, CheckDateTimeFormat(ConvertInfo.Format, ConvertInfo.DestLen));
  r := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(Str), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TDataConverters.SQLTimeStampOffsetToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStampOffset);
  Result := InternalExactCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SQLTimeStampOffsetToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStampOffset);
  Result := InternalExactCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SQLTimeStampOffsetToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStampOffset);
  Result := InternalExactCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.SQLTimeStampOffsetToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStampOffset);
  Result := InternalExactCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

{$IFNDEF FPC}

class function TDataConverters.SQLTimeStampOffsetToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  ts: TTimeStamp;
begin
  Result := InternalSQLTimeStampOffsetToTimeStamp(ConvertInfo.Source, ts);
  if (Result <> csSuccess) and not ConvertInfo.IgnoreConvertErrors then
    Exit;

  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := TimeStampToDateTime(ts);
end;

{$ENDIF}

class function TDataConverters.SQLTimeStampOffsetToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := SizeOf(TSQLTimeStampOffset);
  Result := InternalExactCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToInt8(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToUInt8(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToInt16(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToUInt16(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToInt32(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToUInt32(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToInt64(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.AStrToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToUInt64(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.AStrToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToSingle(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToFloat(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToExtended(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToBCD(Str, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToFMTBCD(Str, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToBool(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToDate(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToTime(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToDateTime(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToSQLTimeStamp(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToSQLTimeStampOffset(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalWritePAChar(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalWriteWStr(WideString(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalAStrToMemo(AStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToWideMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := WideString(Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalWStrToWideMemo(WStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := InternalCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := InternalCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  AStr: AnsiString;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  VariantObj.Value := AStr;
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := csSuccess;
end;

class function TDataConverters.AStrToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.AStrToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToGUID(string(AStr), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToInt8(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToUInt8(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToInt16(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToUInt16(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToInt32(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToUInt32(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToInt64(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.WStrToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToUInt64(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.WStrToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToSingle(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToFloat(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToExtended(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToBCD(Str, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalStrToFMTBCD(Str, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToBool(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToDate(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToTime(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToDateTime(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToSQLTimeStamp(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToSQLTimeStampOffset(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalWriteAStr(AnsiString(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalWritePWChar(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalWriteExtPWChar(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen));
  Result := InternalAStrToMemo(AStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToWideMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalWStrToWideMemo(WStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := InternalCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := InternalCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WStrToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  WStr: WideString;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  VariantObj.Value := WStr;
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := csSuccess;
end;

class function TDataConverters.WStrToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalStrToGUID(string(WStr), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToInt8(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToUInt8(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToInt16(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToUInt16(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToInt32(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToUInt32(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToInt64(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.ExtAStrToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToUInt64(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.ExtAStrToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToSingle(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToFloat(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToExtended(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToBCD(Str, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToFMTBCD(Str, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToBool(string(AStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToDate(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToTime(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToDateTime(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToSQLTimeStamp(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToSQLTimeStampOffset(string(AStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalWritePAChar(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  AStr := Marshal.PtrToStringAnsi(SourcePtr, ConvertInfo.SourceLen);
  Result := InternalWriteWStr(WideString(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  AStr := Marshal.PtrToStringAnsi(SourcePtr, ConvertInfo.SourceLen);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalAStrToMemo(AStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToWideMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := WideString(Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalWStrToWideMemo(WStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  AStr: AnsiString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  AStr := Marshal.PtrToStringAnsi(SourcePtr, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := InternalCopyToBytes(SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  AStr: AnsiString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  AStr := Marshal.PtrToStringAnsi(SourcePtr, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := InternalCopyToVarBytes(SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  AStr: AnsiString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  AStr := Marshal.PtrToStringAnsi(SourcePtr, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  AStr: AnsiString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  AStr := Marshal.PtrToStringAnsi(SourcePtr, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtAStrToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  AStr: AnsiString;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  VariantObj.Value := AStr;
  ConvertInfo.SourceLen := LengthA(AStr);
  Result := csSuccess;
end;

class function TDataConverters.ExtAStrToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToGUID(string(AStr), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToInt8(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToUInt8(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToInt16(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToUInt16(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToInt32(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToUInt32(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToInt64(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.ExtWStrToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToUInt64(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.ExtWStrToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToSingle(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToFloat(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToExtended(Str, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToBCD(Str, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalStrToFMTBCD(Str, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToBool(string(WStr), ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToDate(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToTime(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToDateTime(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToSQLTimeStamp(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToSQLTimeStampOffset(string(WStr),
    ConvertInfo.Format,
    ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  WStr: WideString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  WStr := Marshal.PtrToStringUni(SourcePtr, ConvertInfo.SourceLen);
  Result := InternalWriteAStr(AnsiString(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  WStr: WideString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  WStr := Marshal.PtrToStringUni(SourcePtr, ConvertInfo.SourceLen);
  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AnsiString(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := InternalWritePWChar(SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := InternalWriteExtPWChar(ConvertInfo.StringHeap, SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen));
  Result := InternalAStrToMemo(AStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToWideMemo(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalWStrToWideMemo(WStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestOffset, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  WStr: WideString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  WStr := Marshal.PtrToStringUni(SourcePtr, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := InternalCopyToBytes(SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  WStr: WideString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  WStr := Marshal.PtrToStringUni(SourcePtr, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := InternalCopyToVarBytes(SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  WStr: WideString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  WStr := Marshal.PtrToStringUni(SourcePtr, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
  WStr: WideString;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  WStr := Marshal.PtrToStringUni(SourcePtr, ConvertInfo.SourceLen);
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtWStrToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  WStr: WideString;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  VariantObj.Value := WStr;
  ConvertInfo.SourceLen := Length(WStr) * 2;
  Result := csSuccess;
end;

class function TDataConverters.ExtWStrToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  WStr := Marshal.PtrToStringUni(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen);
  Result := InternalStrToGUID(string(WStr), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Shortint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Byte), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Smallint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Word), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Cardinal), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.BlobToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(UInt64), ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.BlobToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Single), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Double), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Extended), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(TBcd), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, @w, SizeOf(w), ConvertInfo.IgnoreConvertErrors);
  if w = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
end;

class function TDataConverters.BlobToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
  if ((Result = csSuccess) or ConvertInfo.IgnoreConvertErrors) and (Marshal.ReadInt32(ConvertInfo.Dest) < 0) then begin
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, DateDelta);
  end;
end;

class function TDataConverters.BlobToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
  if ((Result = csSuccess) or ConvertInfo.IgnoreConvertErrors) and (Marshal.ReadInt32(ConvertInfo.Dest) < 0) then begin
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
  end;
end;

class function TDataConverters.BlobToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := InternalReadTimeStamp(ConvertInfo.Dest, ts);
  if (Result <> csSuccess) and ConvertInfo.IgnoreConvertErrors then
    Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(DateTimeToTimeStamp(0))));
end;

class function TDataConverters.BlobToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
  r: TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, @SQLTimeStamp, SizeOf(TSQLTimeStamp), ConvertInfo.IgnoreConvertErrors);
  if (Result = csSuccess) or ConvertInfo.IgnoreConvertErrors then begin
    r := ValidateSQLTimeStamp(@SQLTimeStamp);
    if (r = csSuccess) or ConvertInfo.IgnoreConvertErrors then
      PSQLTimeStamp(ConvertInfo.Dest)^ := SQLTimeStamp;
    if Result = csSuccess then
      Result := r;
  end;
end;

class function TDataConverters.BlobToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStampOffset: TSQLTimeStampOffset;
  r: TConvertStatus;
begin
  Result := InternalExactCopyFromBlob(ConvertInfo.Source, @SQLTimeStampOffset, SizeOf(TSQLTimeStampOffset), ConvertInfo.IgnoreConvertErrors);
  if (Result = csSuccess) or ConvertInfo.IgnoreConvertErrors then begin
    r := ValidateSQLTimeStampOffset(@SQLTimeStampOffset);
    if (r = csSuccess) or ConvertInfo.IgnoreConvertErrors then
      PSQLTimeStampOffset(ConvertInfo.Dest)^ := SQLTimeStampOffset;
    if Result = csSuccess then
      Result := r;
  end;
end;

class function TDataConverters.BlobToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyFromBlobToBytes(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyFromBlobToVarBytes(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyFromBlobToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyFromBlobToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BlobToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  Blob: TBlob;
  BlobSize: Cardinal;
begin
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  Assert(Blob <> nil);
  BlobSize := Blob.Size - Cardinal(ConvertInfo.SourceOffset);

  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := VarArrayCreate([0, BlobSize - 1], varByte);

  if BlobSize > 0 then
    Blob.Read(ConvertInfo.SourceOffset, BlobSize, TVarData(VariantObj.Value).VArray.Data);
  ConvertInfo.SourceLen := BlobSize;
  Result := csSuccess;
end;

class function TDataConverters.MemoToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalMemoToAStr(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen, ConvertInfo.DestOffset, ConvertInfo.DestLen, AStr);
  Result := InternalWritePAChar(PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.MemoToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalMemoToAStr(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen, ConvertInfo.DestOffset, ConvertInfo.DestLen, AStr);
  Result := InternalWritePWChar(PWChar(WideString(AStr)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.MemoToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalMemoToAStr(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen, ConvertInfo.DestOffset, ConvertInfo.DestLen, AStr);
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.MemoToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalMemoToAStr(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen, ConvertInfo.DestOffset, ConvertInfo.DestLen, AStr);
  Result := InternalWriteExtPWChar(ConvertInfo.StringHeap, PWChar(WideString(AStr)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WideMemoToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalWideMemoToWStr(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen, ConvertInfo.DestOffset, ConvertInfo.DestLen, WStr);
  Result := InternalWritePAChar(PAChar(AnsiString(WStr)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WideMemoToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalWideMemoToWStr(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen, ConvertInfo.DestOffset, ConvertInfo.DestLen, WStr);
  Result := InternalWritePWChar(PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WideMemoToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalWideMemoToWStr(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen, ConvertInfo.DestOffset, ConvertInfo.DestLen, WStr);
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, PAChar(AnsiString(WStr)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.WideMemoToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalWideMemoToWStr(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen, ConvertInfo.DestOffset, ConvertInfo.DestLen, WStr);
  Result := InternalWriteExtPWChar(ConvertInfo.StringHeap, PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Shortint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Byte), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Smallint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Word), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Cardinal), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.BytesToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(UInt64), ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.BytesToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Single), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Double), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Extended), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(TBcd), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, @w, SizeOf(w), ConvertInfo.IgnoreConvertErrors);
  if w = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
end;

class function TDataConverters.BytesToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
  if ((Result = csSuccess) or ConvertInfo.IgnoreConvertErrors) and (Marshal.ReadInt32(ConvertInfo.Dest) < 0) then begin
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, DateDelta);
  end;
end;

class function TDataConverters.BytesToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
  if ((Result = csSuccess) or ConvertInfo.IgnoreConvertErrors) and (Marshal.ReadInt32(ConvertInfo.Dest) < 0) then begin
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
  end;
end;

class function TDataConverters.BytesToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := InternalReadTimeStamp(ConvertInfo.Dest, ts);
  if (Result <> csSuccess) and ConvertInfo.IgnoreConvertErrors then
    Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(DateTimeToTimeStamp(0))));
end;

class function TDataConverters.BytesToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
  r: TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, @SQLTimeStamp, SizeOf(TSQLTimeStamp), ConvertInfo.IgnoreConvertErrors);
  if (Result = csSuccess) or ConvertInfo.IgnoreConvertErrors then begin
    r := ValidateSQLTimeStamp(@SQLTimeStamp);
    if (r = csSuccess) or ConvertInfo.IgnoreConvertErrors then
      PSQLTimeStamp(ConvertInfo.Dest)^ := SQLTimeStamp;
    if Result = csSuccess then
      Result := r;
  end;
end;

class function TDataConverters.BytesToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStampOffset: TSQLTimeStampOffset;
  r: TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(ConvertInfo.Source, ConvertInfo.SourceLen, @SQLTimeStampOffset, SizeOf(TSQLTimeStampOffset), ConvertInfo.IgnoreConvertErrors);
  if (Result = csSuccess) or ConvertInfo.IgnoreConvertErrors then begin
    r := ValidateSQLTimeStampOffset(@SQLTimeStampOffset);
    if (r = csSuccess) or ConvertInfo.IgnoreConvertErrors then
      PSQLTimeStampOffset(ConvertInfo.Dest)^ := SQLTimeStampOffset;
    if Result = csSuccess then
      Result := r;
  end;
end;

class function TDataConverters.BytesToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalBytesToAStr(ConvertInfo.Source, ConvertInfo.SourceLen, AStr);
  Result := InternalWritePAChar(PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalBytesToAStr(ConvertInfo.Source, ConvertInfo.SourceLen, AStr);
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalBytesToWStr(ConvertInfo.Source, ConvertInfo.SourceLen, WStr);
  Result := InternalWritePWChar(PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalBytesToWStr(ConvertInfo.Source, ConvertInfo.SourceLen, WStr);
  Result := InternalWriteExtPWChar(ConvertInfo.StringHeap, PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyToBytes(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyToVarBytes(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  pArray: PVarArray;
  Size: integer;
begin
  Size := ConvertInfo.SourceLen - ConvertInfo.SourceOffset;
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := VarArrayCreate([0, Size - 1], varByte);
  if Size > 0 then begin
    pArray := TVarData(VariantObj.Value).VArray;
    Move(PtrOffset(ConvertInfo.Source, ConvertInfo.SourceOffset)^, pArray.Data^, Size);
  end;
  ConvertInfo.SourceLen := Size;
  Result := csSuccess;
end;

class function TDataConverters.BytesToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalCopyToBlob(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.BytesToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalBytesToGUID(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Shortint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Byte), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Smallint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Word), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Cardinal), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.VarBytesToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(UInt64), ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.VarBytesToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Single), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Double), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Extended), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(TBcd), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, @w, SizeOf(w), ConvertInfo.IgnoreConvertErrors);
  if w = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
end;

class function TDataConverters.VarBytesToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
  if ((Result = csSuccess) or ConvertInfo.IgnoreConvertErrors) and (Marshal.ReadInt32(ConvertInfo.Dest) < 0) then begin
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, DateDelta);
  end;
end;

class function TDataConverters.VarBytesToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
  if ((Result = csSuccess) or ConvertInfo.IgnoreConvertErrors) and (Marshal.ReadInt32(ConvertInfo.Dest) < 0) then begin
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
  end;
end;

class function TDataConverters.VarBytesToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  ts: TTimeStamp;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := InternalReadTimeStamp(ConvertInfo.Dest, ts);
  if (Result <> csSuccess) and ConvertInfo.IgnoreConvertErrors then
    Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(DateTimeToTimeStamp(0))));
end;

class function TDataConverters.VarBytesToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStamp: TSQLTimeStamp;
  r: TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(TSQLTimeStamp), ConvertInfo.IgnoreConvertErrors);
  if (Result = csSuccess) or ConvertInfo.IgnoreConvertErrors then begin
    r := ValidateSQLTimeStamp(@SQLTimeStamp);
    if (r = csSuccess) or ConvertInfo.IgnoreConvertErrors then
      PSQLTimeStamp(ConvertInfo.Dest)^ := SQLTimeStamp;
    if Result = csSuccess then
      Result := r;
  end;
end;

class function TDataConverters.VarBytesToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SQLTimeStampOffset: TSQLTimeStampOffset;
  r: TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(ConvertInfo.Source, ConvertInfo.Dest, SizeOf(TSQLTimeStampOffset), ConvertInfo.IgnoreConvertErrors);
  if (Result = csSuccess) or ConvertInfo.IgnoreConvertErrors then begin
    r := ValidateSQLTimeStampOffset(@SQLTimeStampOffset);
    if (r = csSuccess) or ConvertInfo.IgnoreConvertErrors then
      PSQLTimeStampOffset(ConvertInfo.Dest)^ := SQLTimeStampOffset;
    if Result = csSuccess then
      Result := r;
  end;
end;

class function TDataConverters.VarBytesToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalVarBytesToAStr(ConvertInfo.Source, AStr, ConvertInfo.SourceLen);
  Result := InternalWritePAChar(PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalVarBytesToAStr(ConvertInfo.Source, AStr, ConvertInfo.SourceLen);
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalVarBytesToWStr(ConvertInfo.Source, WStr, ConvertInfo.SourceLen);
  Result := InternalWritePWChar(PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalVarBytesToWStr(ConvertInfo.Source, WStr, ConvertInfo.SourceLen);
  Result := InternalWriteExtPWChar(ConvertInfo.StringHeap, PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := Marshal.ReadInt16(ConvertInfo.Source);
  Result := InternalCopyToBytes(PtrOffset(ConvertInfo.Source, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := Marshal.ReadInt16(ConvertInfo.Source);
  Result := InternalCopyToVarBytes(PtrOffset(ConvertInfo.Source, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := Marshal.ReadInt16(ConvertInfo.Source);
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, PtrOffset(ConvertInfo.Source, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := Marshal.ReadInt16(ConvertInfo.Source);
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, PtrOffset(ConvertInfo.Source, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  pArray: PVarArray;
  Size: integer;
begin
  Size := Marshal.ReadInt16(ConvertInfo.Source) - ConvertInfo.SourceOffset;
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := VarArrayCreate([0, Size - 1], varByte);
  if Size > 0 then begin
    pArray := TVarData(VariantObj.Value).VArray;
    Move(PtrOffset(ConvertInfo.Source, SizeOf(Word) + ConvertInfo.SourceOffset)^, pArray.Data^, Size);
  end;
  ConvertInfo.SourceLen := Size;
  Result := csSuccess;
end;

class function TDataConverters.VarBytesToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := Marshal.ReadInt16(ConvertInfo.Source);
  Result := InternalCopyToBlob(PtrOffset(ConvertInfo.Source, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VarBytesToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.SourceLen := Word(Marshal.ReadInt16(ConvertInfo.Source));
  Result := InternalBytesToGUID(PtrOffset(ConvertInfo.Source, SizeOf(Word)), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Shortint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Byte), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Smallint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Word), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Cardinal), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.ExtVarBytesToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(UInt64), ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.ExtVarBytesToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Single), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Double), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Extended), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.Dest, SizeOf(TBcd), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  Result := InternalExactCopyFromVarBytes(Marshal.ReadIntPtr(ConvertInfo.Source), @w, SizeOf(w), ConvertInfo.IgnoreConvertErrors);
  if w = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
end;

class function TDataConverters.ExtVarBytesToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := VarBytesToDate(ConvertInfo);
end;

class function TDataConverters.ExtVarBytesToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := VarBytesToTime(ConvertInfo);
end;

class function TDataConverters.ExtVarBytesToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := VarBytesToDateTime(ConvertInfo);
end;

class function TDataConverters.ExtVarBytesToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := VarBytesToSQLTimeStamp(ConvertInfo);
end;

class function TDataConverters.ExtVarBytesToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := VarBytesToSQLTimeStampOffset(ConvertInfo);
end;

class function TDataConverters.ExtVarBytesToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalVarBytesToAStr(Marshal.ReadIntPtr(ConvertInfo.Source), AStr, ConvertInfo.SourceLen);
  Result := InternalWritePAChar(PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalVarBytesToAStr(Marshal.ReadIntPtr(ConvertInfo.Source), AStr, ConvertInfo.SourceLen);
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalVarBytesToWStr(Marshal.ReadIntPtr(ConvertInfo.Source), WStr, ConvertInfo.SourceLen);
  Result := InternalWritePWChar(PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalVarBytesToWStr(Marshal.ReadIntPtr(ConvertInfo.Source), WStr, ConvertInfo.SourceLen);
  Result := InternalWriteExtPWChar(ConvertInfo.StringHeap, PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  ConvertInfo.SourceLen := Marshal.ReadInt16(SourcePtr);
  Result := InternalCopyToBytes(PtrOffset(SourcePtr, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  ConvertInfo.SourceLen := Marshal.ReadInt16(SourcePtr);
  Result := InternalCopyToVarBytes(PtrOffset(SourcePtr, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  ConvertInfo.SourceLen := Marshal.ReadInt16(SourcePtr);
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, PtrOffset(SourcePtr, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  ConvertInfo.SourceLen := Marshal.ReadInt16(SourcePtr);
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, PtrOffset(SourcePtr, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  pArray: PVarArray;
  SourcePtr: IntPtr;
  Size: integer;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Size := Marshal.ReadInt16(SourcePtr) - ConvertInfo.SourceOffset;
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := VarArrayCreate([0, Size - 1], varByte);
  if Size > 0 then begin
    pArray := TVarData(VariantObj.Value).VArray;
    Move(PtrOffset(SourcePtr, SizeOf(Word) + ConvertInfo.SourceOffset)^, pArray.Data^, Size);
  end;
  ConvertInfo.SourceLen := Size;
  Result := csSuccess;
end;

class function TDataConverters.ExtVarBytesToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  ConvertInfo.SourceLen := Marshal.ReadInt16(SourcePtr);
  Result := InternalCopyToBlob(PtrOffset(SourcePtr, SizeOf(Word)), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtVarBytesToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  ConvertInfo.SourceLen := Word(Marshal.ReadInt16(SourcePtr));
  Result := InternalBytesToGUID(PtrOffset(SourcePtr, SizeOf(Word)), ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Shortint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Byte), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Smallint), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Word), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Integer), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Cardinal), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}

class function TDataConverters.ExtBytesToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(UInt64), ConvertInfo.IgnoreConvertErrors);
end;

{$ENDIF}

class function TDataConverters.ExtBytesToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Single), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Double), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Extended), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(Int64), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, ConvertInfo.Dest, SizeOf(TBcd), ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  w: Word;
begin
  Result := InternalExactCopyFromBytes(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, @w, SizeOf(w), ConvertInfo.IgnoreConvertErrors);
  if w = 0 then
    Marshal.WriteInt16(ConvertInfo.Dest, 0)
  else
    Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(True)));
end;

class function TDataConverters.ExtBytesToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := BytesToDate(ConvertInfo);
end;

class function TDataConverters.ExtBytesToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := BytesToTime(ConvertInfo);
end;

class function TDataConverters.ExtBytesToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := BytesToDateTime(ConvertInfo);
end;

class function TDataConverters.ExtBytesToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := BytesToSQLTimeStamp(ConvertInfo);
end;

class function TDataConverters.ExtBytesToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  ConvertInfo.Source := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := BytesToSQLTimeStampOffset(ConvertInfo);
end;

class function TDataConverters.ExtBytesToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalBytesToAStr(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, AStr);
  Result := InternalWritePAChar(PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  InternalBytesToAStr(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, AStr);
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, PAChar(AStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalBytesToWStr(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, WStr);
  Result := InternalWritePWChar(PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  WStr: WideString;
begin
  InternalBytesToWStr(Marshal.ReadIntPtr(ConvertInfo.Source), ConvertInfo.SourceLen, WStr);
  Result := InternalWriteExtPWChar(ConvertInfo.StringHeap, PWChar(WStr), ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := InternalCopyToBytes(SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := InternalCopyToVarBytes(SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToVariant(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  pArray: PVarArray;
  SourcePtr: IntPtr;
  Size: integer;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Size := ConvertInfo.SourceLen - ConvertInfo.SourceOffset;
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Dest)));
  VariantObj.Value := VarArrayCreate([0, Size - 1], varByte);
  if Size > 0 then begin
    pArray := TVarData(VariantObj.Value).VArray;
    Move(PtrOffset(SourcePtr, ConvertInfo.SourceOffset)^, pArray.Data^, Size);
  end;
  ConvertInfo.SourceLen := Size;
  Result := csSuccess;
end;

class function TDataConverters.ExtBytesToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := InternalCopyToBlob(SourcePtr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.ExtBytesToGuid(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  SourcePtr: IntPtr;
begin
  SourcePtr := Marshal.ReadIntPtr(ConvertInfo.Source);
  Result := InternalBytesToGUID(SourcePtr, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  i32: Integer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    i32 := VariantObj.Value;
  except
    Result := csInvalidIntegerValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteByte(ConvertInfo.Dest, 0);
    Exit;
  end;

  Result := InternalInt32ToInt8(i32, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToUInt8(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  i32: Integer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    i32 := VariantObj.Value;
  except
    Result := csInvalidIntegerValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteByte(ConvertInfo.Dest, 0);
    Exit;
  end;

  Result := InternalInt32ToUInt8(i32, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  i32: Integer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    i32 := VariantObj.Value;
  except
    Result := csInvalidIntegerValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt16(ConvertInfo.Dest, 0);
    Exit;
  end;

  Result := InternalInt32ToInt16(i32, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToUInt16(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  i32: Integer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    i32 := VariantObj.Value;
  except
    Result := csInvalidIntegerValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt16(ConvertInfo.Dest, 0);
    Exit;
  end;

  Result := InternalInt32ToUInt16(i32, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  i32: Integer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    i32 := VariantObj.Value;
  except
    Result := csInvalidIntegerValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
    Exit;
  end;

  Marshal.WriteInt32(ConvertInfo.Dest, i32);
  Result := csSuccess;
end;

class function TDataConverters.VariantToUInt32(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  i64: Int64;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    i64 := VariantObj.Value;
  except
    Result := csInvalidIntegerValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
    Exit;
  end;

  Result := InternalInt64ToUInt32(i64, ConvertInfo.Dest, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  i64: Int64;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    i64 := VariantObj.Value;
  except
    Result := csInvalidIntegerValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt64(ConvertInfo.Dest, 0);
    Exit;
  end;

  Marshal.WriteInt64(ConvertInfo.Dest, i64);
  Result := csSuccess;
end;

{$IFDEF USE_UINT64}

class function TDataConverters.VariantToUInt64(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  ui64: UInt64;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    ui64 := VariantObj.Value;
  except
    Result := csInvalidIntegerValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt64(ConvertInfo.Dest, 0);
    Exit;
  end;

  Marshal.WriteInt64(ConvertInfo.Dest, Int64(ui64));
  Result := csSuccess;
end;

{$ENDIF}

class function TDataConverters.VariantToSingle(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  s: Single;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    s := VariantObj.Value;
  except
    Result := csInvalidNumericValue;
    if ConvertInfo.IgnoreConvertErrors then
      Single(ConvertInfo.Dest^) := 0;
    Exit;
  end;

  Single(ConvertInfo.Dest^) := s;
  Result := csSuccess;
end;

class function TDataConverters.VariantToFloat(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  d: Double;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    d := VariantObj.Value;
  except
    Result := csInvalidNumericValue;
    if ConvertInfo.IgnoreConvertErrors then
      Double(ConvertInfo.Dest^) := 0;
    Exit;
  end;

  Double(ConvertInfo.Dest^) := d;
  Result := csSuccess;
end;

class function TDataConverters.VariantToExtended(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  e: Extended;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    e := VariantObj.Value;
  except
    Result := csInvalidNumericValue;
    if ConvertInfo.IgnoreConvertErrors then
      Extended(ConvertInfo.Dest^) := 0;
    Exit;
  end;

  Extended(ConvertInfo.Dest^) := e;
  Result := csSuccess;
end;

class function TDataConverters.VariantToBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  e: Extended;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    e := VariantObj.Value;
  except
    Result := csInvalidNumericValue;
    if ConvertInfo.IgnoreConvertErrors then
      InternalExtendedToBCD(0, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
    Exit;
  end;

  Result := InternalExtendedToBCD(e, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToFMTBCD(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  e: Extended;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    e := VariantObj.Value;
  except
    Result := csInvalidNumericValue;
    if ConvertInfo.IgnoreConvertErrors then
      InternalExtendedToBCD(0, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
    Exit;
  end;

  Result := InternalExtendedToBCD(e, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.DestScale, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToBool(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  b: Boolean;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    b := VariantObj.Value;
  except
    Result := csInvalidBooleanValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt16(ConvertInfo.Dest, 0);
    Exit;
  end;

  Marshal.WriteInt16(ConvertInfo.Dest, SmallInt(WordBool(b)));
  Result := csSuccess;
end;

class function TDataConverters.VariantToDate(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  dt: TDateTime;
  ts: TTimeStamp;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    dt := VariantObj.Value;
  except
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
    Exit;
  end;

  ts := DateTimeToTimeStamp(dt);
  if ts.Time <> 0 then begin
    Result := csDataTruncated;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt32(ConvertInfo.Dest, ts.Date);
end;

class function TDataConverters.VariantToTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  dt: TDateTime;
  ts: TTimeStamp;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    dt := VariantObj.Value;
  except
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt32(ConvertInfo.Dest, 0);
    Exit;
  end;

  ts := DateTimeToTimeStamp(dt);
  if (ts.Date <> 0) and (ts.Date <> DateDelta) then begin
    Result := csDataTruncated;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit;
  end
  else
    Result := csSuccess;

  Marshal.WriteInt32(ConvertInfo.Dest, ts.Time);
end;

class function TDataConverters.VariantToDateTime(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  dt: TDateTime;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    dt := VariantObj.Value;
  except
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      Marshal.WriteInt64(ConvertInfo.Dest, 0);
    Exit;
  end;

  Result := csSuccess;
  Marshal.WriteInt64(ConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(DateTimeToTimeStamp(dt))));
end;

{$IFNDEF FPC}

class function TDataConverters.VariantToSQLTimeStamp(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  dt: TDateTime;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    dt := VariantObj.Value;
  except
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      InternalTimeStampToSQLTimeStamp(DateTimeToTimeStamp(0), ConvertInfo.Dest);
    Exit;
  end;

  Result := csSuccess;
  InternalTimeStampToSQLTimeStamp(DateTimeToTimeStamp(dt), ConvertInfo.Dest);
end;

class function TDataConverters.VariantToSQLTimeStampOffset(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  dt: TDateTime;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    dt := VariantObj.Value;
  except
    Result := csInvalidDateTimeValue;
    if ConvertInfo.IgnoreConvertErrors then
      InternalTimeStampToSQLTimeStampOffset(DateTimeToTimeStamp(0), ConvertInfo.Dest);
    Exit;
  end;

  Result := csSuccess;
  InternalTimeStampToSQLTimeStampOffset(DateTimeToTimeStamp(dt), ConvertInfo.Dest);
end;

{$ENDIF}

class function TDataConverters.VariantToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  AStr: AnsiString;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    AStr := AnsiString(VarToStr(VariantObj.Value));
  except
    Result := csInvalidDataMapping;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit;
    AStr := '';
  end;

  Result := InternalWriteAStr(AStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  AStr: AnsiString;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    AStr := AnsiString(VarToStr(VariantObj.Value));
  except
    Result := csInvalidDataMapping;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit;
    AStr := '';
  end;

  Result := InternalWriteExtAStr(ConvertInfo.StringHeap, AStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  WStr: WideString;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    WStr := VarToWideStr(VariantObj.Value);
  except
    Result := csInvalidDataMapping;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit;
    WStr := '';
  end;

  Result := InternalWriteWStr(WStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  WStr: WideString;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  try
    WStr := VarToWideStr(VariantObj.Value);
  except
    Result := csInvalidDataMapping;
    if not ConvertInfo.IgnoreConvertErrors then
      Exit;
    WStr := '';
  end;

  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WStr, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  Buf: Pointer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  Result := InternalVariantToBytes(VariantObj.Value, Buf, ConvertInfo.SourceLen);
  if ConvertInfo.SourceLen = 0 then begin
    ConvertInfo.DestLen := ConvertInfo.DestOffset;
    Exit;
  end;

  Result := InternalCopyToBytes(Buf, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  Buf: Pointer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  Result := InternalVariantToBytes(VariantObj.Value, Buf, ConvertInfo.SourceLen);
  if ConvertInfo.SourceLen = 0 then begin
    ConvertInfo.DestLen := ConvertInfo.DestOffset;
    Exit;
  end;

  Result := InternalCopyToVarBytes(Buf, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  Buf: Pointer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  Result := InternalVariantToBytes(VariantObj.Value, Buf, ConvertInfo.SourceLen);
  if ConvertInfo.SourceLen = 0 then begin
    ConvertInfo.DestLen := ConvertInfo.DestOffset;
    Exit;
  end;

  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, Buf, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  Buf: Pointer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  Result := InternalVariantToBytes(VariantObj.Value, Buf, ConvertInfo.SourceLen);
  if ConvertInfo.SourceLen = 0 then begin
    ConvertInfo.DestLen := ConvertInfo.DestOffset;
    Exit;
  end;

  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, Buf, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.VariantToBlob(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  VariantObj: TVariantObject;
  Buf: Pointer;
begin
  VariantObj := TVariantObject(GetGCHandleTarget(Marshal.ReadIntPtr(ConvertInfo.Source)));
  Result := InternalVariantToBytes(VariantObj.Value, Buf, ConvertInfo.SourceLen);
  if ConvertInfo.SourceLen = 0 then begin
    ConvertInfo.DestLen := ConvertInfo.DestOffset;
    Exit;
  end;

  Result := InternalCopyToBlob(Buf, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.GuidToAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalWritePAChar(ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.GuidToWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalWriteWStr(WideString(AStr), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.GuidToExtAStr(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalWriteExtPAChar(ConvertInfo.StringHeap, ConvertInfo.Source, ConvertInfo.SourceOffset, ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestOffset, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.GuidToExtWStr(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
begin
  AStr := Marshal.PtrToStringAnsi(ConvertInfo.Source, ConvertInfo.SourceLen);
  Result := InternalWriteExtWStr(ConvertInfo.StringHeap, WideString(AStr), ConvertInfo.SourceLen,
    ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.GuidToBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalGuidToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, ConvertInfo.Dest, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  ConvertInfo.SourceLen := 16;
end;

class function TDataConverters.GuidToVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalGuidToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, PtrOffset(ConvertInfo.Dest, SizeOf(Word)), ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    ConvertInfo.SourceLen := 16
  else
    ConvertInfo.SourceLen := 0;
  Marshal.WriteInt16(ConvertInfo.Dest, ConvertInfo.SourceLen);
end;

class function TDataConverters.GuidToExtBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Buf: TBytes;
begin
  SetLength(Buf, 16);
  Result := InternalGuidToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, @Buf[0], Length(Buf), ConvertInfo.IgnoreConvertErrors);
  if Result <> csSuccess then
    Exit;

  ConvertInfo.SourceLen := Length(Buf);
  Result := InternalCopyToExtBytes(ConvertInfo.StringHeap, @Buf[0], 0, ConvertInfo.SourceLen,
    ConvertInfo.Dest, 0, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

class function TDataConverters.GuidToExtVarBytes(var ConvertInfo: TConvertInfo): TConvertStatus;
var
  Buf: TBytes;
begin
  SetLength(Buf, 16);
  Result := InternalGuidToBytes(ConvertInfo.Source, ConvertInfo.SourceLen, @Buf[0], Length(Buf), ConvertInfo.IgnoreConvertErrors);
  if Result <> csSuccess then
    Exit;

  ConvertInfo.SourceLen := Length(Buf);
  Result := InternalCopyToExtVarBytes(ConvertInfo.StringHeap, @Buf[0], 0, ConvertInfo.SourceLen,
    ConvertInfo.Dest, 0, ConvertInfo.DestLen, ConvertInfo.IgnoreConvertErrors);
end;

initialization
  DBTypeInfos := TDBTypeInfos.Create;
{$IFDEF USE_TFORMATSETTINGS}
  InternalFormatSettings.ShortDateFormat := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
  InternalFormatSettings.LongTimeFormat := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat;
  InternalFormatSettings.DateSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator;
  InternalFormatSettings.TimeSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator;
  InternalFormatSettings.DecimalSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  InternalFormatSettings.TimeAMString := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeAMString;
  InternalFormatSettings.TimePMString := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}TimePMString;
{$ENDIF}

finalization
  DBTypeInfos.Free;

end.
