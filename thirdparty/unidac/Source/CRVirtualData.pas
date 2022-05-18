
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit CRVirtualData;

interface

uses
  SysUtils, Classes, Variants,
{$IFNDEF FPC}
  SqlTimSt,
{$ENDIF}
  CRTypes, CLRClasses, CRFunctions, CRAccess, CRParser, CRTimeStamp,
  MemData,
  CRVirtualConsts;

type
  EVirtualQueryError = class(Exception);

  TVirtualType = (vrNull, vrInteger, vrFloat, vrString, vrAnsiString, vrWideString, vrBlob);

  PVirtualValue = ^TVirtualValue;
  TVirtualValue = record
    ValueType: TVirtualType;
    Value: Variant;
    IntValue: Int64;
    FloatValue: double;
    AnsiStrValue: AnsiString;
    WideStrValue: WideString;
  end;
  TVirtualValues = array of TVirtualValue;

  PVirtualConstraintItem = ^TVirtualConstraintItem;
  TVirtualConstraintItem = record
    FieldIndex: integer;
    ArgIndex: integer;
    Operation: TExpressionType;
    Value: TVirtualValue;
    SimpleCompare,
    NativeCompare: boolean;
  end;

  PVirtualConstraint = ^TVirtualConstraint;
  TVirtualConstraint = record
    LocalIndex: integer;
    CurrentItem: integer;
    SortItemIndex: integer;
    Items: array of TVirtualConstraintItem;
  end;
  TVirtualConstraints = array of TVirtualConstraint;

  TVirtualBookmark = Int64;

  PVirtualFieldDesc = ^TVirtualFieldDesc;
  TVirtualFieldDesc = record
    Name: string;
    DBType,
    DataType,
    Length,
    Scale: word;
    IsKey,
    IsAutoIncrement,
    Required,
    ReadOnly,
    Hidden: boolean;
    Default: Variant;
  {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FieldObject: TObject;
    ActualIndex: integer;
  end;
  TVirtualFieldDescs = array of TVirtualFieldDesc;

  TSpecificTypeDesc = class
  private
    FDataTypeName: string;
    FDataType: word;
    FLength,
    FScale: integer;
  public
    constructor Create(ADataTypeName: string; ADataType: word; ALength, AScale: integer);

    property DataTypeName: string read FDataTypeName;
    property DataType: word read FDataType;
    property Length: integer read FLength;
    property Scale: integer read FScale;
  end;

  TSpecificTypes = TStringList;

  TVirtualData = class;

  TVirtualLocalIndex = class
  private
    function GetItems(const Index: integer): pointer;
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FData: TVirtualData;
    FItems: TList;
    FStartIndex: integer;
    FNull1, FNull2: boolean;

    function GetFieldPtr(const FieldIndex: integer): IntPtr; virtual; abstract;
    function GetBuffer(const Item: pointer): IntPtr; virtual; abstract;
    function InternalCompare(const FieldPtr: IntPtr; Item1, Item2: pointer): integer; virtual; abstract;
    function IsIntegerField(const FieldPtr: IntPtr): boolean; virtual; abstract;
    function IsDateTimeField(const FieldPtr: IntPtr): boolean; virtual; abstract;
    function IsFloatField(const FieldPtr: IntPtr): boolean; virtual; abstract;
    function IsAnsiStringField(const FieldPtr: IntPtr): boolean; virtual; abstract;
    function IsWideStringField(const FieldPtr: IntPtr): boolean; virtual; abstract;
  public
    constructor Create(const Data: TVirtualData; Capacity: integer);
    destructor Destroy; override;

    procedure Sort(const FieldIndex: integer); virtual;
    function GetItem(const Constraint: PVirtualConstraint): integer; virtual;
    function GetNextItem(const From: integer; const Constraint: PVirtualConstraint): integer;
    procedure DeleteItem(const Item: IntPtr);

    procedure Add(const Item: pointer);
    property Items[const Index: integer]: pointer read GetItems; default;
  end;

  TVirtualLocalIndexClass = class of TVirtualLocalIndex;

  TFieldAccessor = class
    class function AsInteger(const Field, Buffer: IntPtr): Int64; virtual;
    class function AsFloat(const Field, Buffer: IntPtr): double; virtual;
    class function AsAnsiString(const Field, Buffer: IntPtr): AnsiString; virtual;
    class function AsWideString(const Field, Buffer: IntPtr): WideString; virtual;
  end;

  TFieldAccessorClass = class of TFieldAccessor;

  TVirtualSQLInfo = class(TSQLInfo)
  private
    FLexems: TLexemList;
  public
    constructor Create(ParserClass: TSQLParserClass; const Lexems: TLexemList); reintroduce;

    function LeftQuote: Char; override;
    function RightQuote: Char; override;
    function IsQuoted(const Value: string): boolean; override;

    function NormalizeName(const Name: string): string; overload;
  end;

  TVirtualData = class
  private
    function CompareFieldValue(const Constraint: PVirtualConstraintItem): boolean;
  protected
    FLocalIndexes: TStringList;
    FSQLInfo: TVirtualSQLInfo;
    FCanUseLocalindex,
    FPrepared,
    FAutoClose,
    FAutoUnwind: boolean;

    class function GetLocalIndexClass: TVirtualLocalIndexClass; virtual;
    function GetFieldAccessorClass: TFieldAccessorClass; virtual; abstract;
    procedure CreateSQLInfo; virtual;

    function ValueToVariant(const FieldIndex: integer; const VirtualValue: TVirtualValue; out Value: variant): boolean;

    function OmitRecord(const Filter: PVirtualConstraint; const ExcludeIndex: integer = -1): boolean; virtual;
    function GetNextRecord(const Filter: PVirtualConstraint): boolean; virtual;

    procedure InternalOpen; virtual;
    procedure InternalDescribeFields(const SpecificTypes: TSpecificTypes); virtual;
    procedure InternalGetCurrentRecord; virtual;
    procedure InternalNext; virtual; abstract;
    function InternalEof(const Filter: PVirtualConstraint): boolean; virtual; abstract;

    function IsSupportedDataType(DataType: word): boolean; virtual;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: variant): boolean; overload; virtual; abstract;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: Int64): boolean; overload; virtual; abstract;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: double): boolean; overload; virtual; abstract;

    function GetFieldDataTypeName(const Field: PVirtualFieldDesc): string; virtual;
    function GetFieldDefinition(const Field: PVirtualFieldDesc): string; virtual;
  public
    FFields: TVirtualFieldDescs;

    constructor Create;
    destructor Destroy; override;

    function Prepared: boolean; virtual;
    function Active: boolean; virtual; abstract;
    function GetCreateSQL: string;

    procedure Prepare; virtual;
    procedure DescribeFields(const SpecificTypes: TSpecificTypes);
    procedure PrepareConstraints(const Filter: PVirtualConstraint; var Cost: integer); virtual;
    procedure PrepareFilter(const Filter: PVirtualConstraint); virtual;

    procedure CheckActive;
    function Open(const Filter: PVirtualConstraint): TVirtualBookmark; virtual;
    procedure Close; virtual;
    procedure Reset; virtual;
    function Next(const Bookmark: TVirtualBookmark; const Filter: PVirtualConstraint): TVirtualBookmark; virtual;
    function Eof(const Bookmark: TVirtualBookmark): boolean;

    function GetBookmark: TVirtualBookmark; virtual; abstract;
    procedure GotoBookmark(const Bookmark: TVirtualBookmark); virtual; abstract;

    function GetRecordCount: integer; virtual; abstract;
    function GetFieldType(FieldIndex: integer): word; virtual;
    function GetFieldScale(FieldIndex: integer): word; virtual;
    function GetFieldNull(FieldIndex: integer): boolean; virtual; abstract;
    function GetFieldValue(FieldIndex: integer): variant; overload; virtual; abstract;
    function GetFieldValue(FieldIndex: integer; var FieldNull: boolean): variant; overload; virtual;
    procedure FreeFieldValue(Value: variant); virtual;

    procedure DisableControls(SaveRecNo: boolean); virtual;
    procedure EnableControls; virtual;

    procedure EditRecord(const Values: TVirtualValues); virtual; abstract;
    procedure InsertRecord(const Values: TVirtualValues); virtual; abstract;
    procedure DeleteRecord; virtual; abstract;

    function InTransaction: boolean; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;

    function IsSimpleFieldType(const FieldIndex: Integer): boolean; virtual; abstract;
    function CompareInteger(const Field, Buffer: IntPtr; const Value: Int64): integer; virtual;
    function CompareFloat(const Field, Buffer: IntPtr; const Value: double): integer; virtual;
    function CompareAnsiString(const Field, Buffer: IntPtr; const Value: AnsiString): integer; virtual;
    function CompareWideString(const Field, Buffer: IntPtr; const Value: WideString): integer; virtual;
    function CompareVariant(const Field, Buffer: IntPtr; const Value: Variant): integer; virtual;

    function GetLocalIndex(const Constraint: PVirtualConstraint): integer; virtual;
    procedure DropLocalIndex(const FieldName: string); virtual;
    procedure DeleteIndexItem(const Item: TVirtualBookmark); virtual;
    procedure ClearLocalIndexes; virtual;

    property CanUseLocalIndex: boolean read FCanUseLocalindex;
    property Fields: TVirtualFieldDescs read FFields write FFields;
    property AutoClose: boolean read FAutoClose;
    property AutoUnwind: boolean read FAutoUnwind;
    property SQLInfo: TVirtualSQLInfo read FSQLInfo;
  end;

  TMemDataFieldAccessor = class(TFieldAccessor)
    class function AsInteger(const Field, Buffer: IntPtr): Int64; override;
    class function AsFloat(const Field, Buffer: IntPtr): double; override;
    class function AsAnsiString(const Field, Buffer: IntPtr): AnsiString; override;
    class function AsWideString(const Field, Buffer: IntPtr): WideString; override;
  end;

  TVirtualMemDataIndex = class(TVirtualLocalIndex)
  protected
    function GetFieldPtr(const FieldIndex: integer): IntPtr; override;
    function GetBuffer(const Item: pointer): IntPtr; override;
    function InternalCompare(const FieldPtr: IntPtr; Item1, Item2: pointer): integer; override;
    function IsIntegerField(const FieldPtr: IntPtr): boolean; override;
    function IsDateTimeField(const FieldPtr: IntPtr): boolean; override;
    function IsFloatField(const FieldPtr: IntPtr): boolean; override;
    function IsAnsiStringField(const FieldPtr: IntPtr): boolean; override;
    function IsWideStringField(const FieldPtr: IntPtr): boolean; override;
  public
    procedure Sort(const FieldIndex: integer); override;
  end;

  TVirtualMemData = class(TVirtualData)
  private
    FBookmark,
    FOpenBookmark: TRecBookmark;
    FExplicitTransaction,
    FUseFilter: boolean;
    FFieldNode,
    FOperationNode,
    FValueNode: TExpressionNode;
  protected
    FMemData: TMemData;
    FRecordBuffer: IntPtr;
    FBufferAllocated: boolean;

    class function GetLocalIndexClass: TVirtualLocalIndexClass; override;
    function GetFieldAccessorClass: TFieldAccessorClass; override;

    function OmitRecord(const Filter: PVirtualConstraint; const ExcludeIndex: integer = -1): boolean; override;
    function GetNextRecord(const Filter: PVirtualConstraint): boolean; override;

    procedure InternalOpen; override;
    function InternalOpenNoIndex(const Filter: PVirtualConstraint): TVirtualBookmark; virtual;
    procedure InternalDescribeFields(const SpecificTypes: TSpecificTypes); override;
    procedure InternalGetCurrentRecord; override;
    procedure InternalNext; override;
    function InternalEof(const Filter: PVirtualConstraint): boolean; override;

    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: variant): boolean; overload; override;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: Int64): boolean; overload; override;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: double): boolean; overload; override;
    function IsSupportedDataType(DataType: word): boolean; override;

    function GetFieldDefinition(const Field: PVirtualFieldDesc): string; override;

    procedure InternalAllocBuffer; virtual;
    procedure InternalFreeBuffer; virtual;
    procedure InternalPutRecord(const Values: TVirtualValues; Buffer: IntPtr);
  public
    constructor Create(const MemData: TMemData); overload;
    destructor Destroy; override;

    function Active: boolean; override;

    procedure Prepare; override;

    function Open(const Filter: PVirtualConstraint): TVirtualBookmark; override;
    procedure Close; override;
    procedure Reset; override;

    function GetBookmark: TVirtualBookmark; override;
    procedure GotoBookmark(const Bookmark: TVirtualBookmark); override;

    function GetRecordCount: integer; override;
    function GetFieldType(FieldIndex: integer): word; override;
    function GetFieldNull(FieldIndex: integer): boolean; override;
    function GetFieldValue(FieldIndex: integer): variant; override;

    procedure EditRecord(const Values: TVirtualValues); override;
    procedure InsertRecord(const Values: TVirtualValues); override;
    procedure DeleteRecord; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    function CompareVariant(const Field, Buffer: IntPtr; const Value: Variant): integer; override;

    function IsSimpleFieldType(const FieldIndex: Integer): boolean; override;
    function GetLocalIndex(const Constraint: PVirtualConstraint): integer; override;
  end;

function ConvertStringToDateTime(DataType: word; Value: string): TDateTime;

implementation

uses
{$IFDEF OFS}
{$IFNDEF LOG}
  Debug,
{$ELSE}
  Unit4,
{$ENDIF}
{$ENDIF}
  Math,
  CRDataTypeMap, MemUtils,
  LiteClassesVirtual, LiteParserVirtual;

const
  FuzzFactor = 1000;
  DoubleResolution   = 1E-15 * FuzzFactor;

type
  TMemDataHelper = class(TMemData);

function ConvertStringToDateTime(DataType: word; Value: string): TDateTime;
var
  s: string;
  err: boolean;
begin
  Result := ConvertStrToDateTime(DataType, Value, '-', ':', 'yyyy-mm-dd', 'hh:nn:ss', False, s, err);

  if err then begin
    Result := ConvertStrToDateTime(DataType, Value,
      {$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator,
      {$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator,
      {$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat,
      {$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}LongTimeFormat,
      False, s, err);
  end;
end;

{ TSpecificTypeDesc }

constructor TSpecificTypeDesc.Create(ADataTypeName: string; ADataType: word; ALength, AScale: integer);
begin
  inherited Create;

  FDataTypeName := ADataTypeName;
  FDataType := ADataType;
  FLength := ALength;
  FScale := AScale;
end;

{ TVirtualLocalIndex }

constructor TVirtualLocalIndex.Create(const Data: TVirtualData; Capacity: integer);
begin
  inherited Create;

  FData := Data;

  FItems := TList.Create;
  FItems.Capacity := Capacity;

  FStartIndex := 0;
end;

destructor TVirtualLocalIndex.Destroy;
begin
  FItems.Free;

  inherited;
end;

function TVirtualLocalIndex.GetItems(const Index: integer): pointer;
begin
  Result := FItems[Index];
end;

procedure TVirtualLocalIndex.Add(const Item: pointer);
begin
  FItems.Add(Item);
end;

procedure TVirtualLocalIndex.Sort(const FieldIndex: integer);
var
  FieldPtr: IntPtr;

  procedure QuickSort(L, R: integer);
  var
    I, J : integer;
    P, Q : pointer;
  begin
    repeat
      I := L;
      J := R;
      P := FItems[(L + R) div 2];
      repeat
        while InternalCompare(FieldPtr, FItems[I], P) < 0 do
          Inc(I);
        while InternalCompare(FieldPtr, FItems[J], P) > 0 do
          Dec(J);
        if I <= J then begin
          if I < J then begin
            Q := FItems[I];
            FItems[I] := FItems[J];
            FItems[J] := Q;
          end;

          if FNull2 and (I > FStartIndex) then
            FStartIndex := I;

          Inc(I);
          Dec(J);
        end;
      until I > J;

      if J - L < R - I then begin
        if L < J then
          QuickSort(L, J);
        L := I;
      end
      else begin
        if I < R then
          QuickSort(I, R);
        R := J;
      end;
    until L >= R;
  end;

begin
  if FItems.Count > 0 then begin
    FieldPtr := GetFieldPtr(FieldIndex);
    QuickSort(0, FItems.Count - 1);
  end;
end;

function TVirtualLocalIndex.GetItem(const Constraint: PVirtualConstraint): integer;
var
  ConstraintItem: PVirtualConstraintItem;
  FieldPtr: IntPtr;
  Item: integer;
  Buffer: IntPtr;
  i, k, n, PrevIndex: integer;
  Value: variant;

  function BinarySearch(const From, UpTo: integer; out Ind: integer): integer;
  var
    L, H: Integer;
    cmp: Integer;
    Ptr: IntPtr;
  begin
    Result := -1;

    L := From;
    H := UpTo;

    if H = -1 then
      Exit;

    while L <= H do begin
      Ind := L + (H - L) shr 1;
      Ptr := FItems[Ind];
      Buffer := GetBuffer(Ptr);

      if ConstraintItem^.NativeCompare then begin
        if IsIntegerField(FieldPtr) then
          cmp := FData.CompareInteger(FieldPtr, Buffer, ConstraintItem^.Value.IntValue)
        else if IsFloatField(FieldPtr) or IsDateTimeField(FieldPtr) then
          cmp := FData.CompareFloat(FieldPtr, Buffer, ConstraintItem^.Value.FloatValue)
        else if IsAnsiStringField(FieldPtr) then
          cmp := FData.CompareAnsiString(FieldPtr, Buffer, AnsiString(ConstraintItem^.Value.Value))
        else if IsWideStringField(FieldPtr) then
          cmp := FData.CompareWideString(FieldPtr, Buffer, WideString(ConstraintItem^.Value.Value))
        else
          cmp := FData.CompareVariant(FieldPtr, Buffer, ConstraintItem^.Value.Value);
      end
      else begin
        if not FData.ValueToVariant(ConstraintItem^.FieldIndex, ConstraintItem^.Value, Value) then
          Exit;
        cmp := FData.CompareVariant(FieldPtr, Buffer, Value);
      end;

      if cmp < 0 then begin
        case ConstraintItem^.Operation of
          ntLess,
          ntLessEqual: begin
            Result := Ind;
            Exit;
          end;
          ntEqual,
          ntMore,
          ntMoreEqual:
            L := Ind + 1;
        end;
      end
      else if cmp > 0 then begin
        case ConstraintItem^.Operation of
          ntMore,
          ntMoreEqual: begin
            Result := Ind;
            Exit;
          end;
          ntEqual,
          ntLess,
          ntLessEqual:
            H := Ind - 1;
        end;
      end
      else begin
        case ConstraintItem^.Operation of
          ntEqual,
          ntLessEqual,
          ntMoreEqual: begin
            Result := Ind;
            Exit;
          end;
          ntMore:
            L := Ind + 1;
          ntLess:
            H := Ind - 1;
        end;
      end;
    end;
  end;

begin
  Result := -1;
  ConstraintItem := @Constraint^.Items[Constraint^.SortItemIndex];
  if ConstraintItem^.Value.ValueType = vrNull then
    Exit;

  FieldPtr := GetFieldPtr(FData.FFields[ConstraintItem^.FieldIndex].ActualIndex);

  PrevIndex := -1;
  k := FStartIndex;
  n := FItems.Count - 1;
  i := -1;
  repeat
    Item := BinarySearch(k, n, i);

    if Item = -1 then
      if PrevIndex >= 0 then begin
        Result := PrevIndex;
        Break;
      end
      else begin
        Constraint^.CurrentItem := -1;
        Break;
      end;

    PrevIndex := i;
    Constraint^.CurrentItem := i;
    n := i - 1;
    k := trunc(n/2);
    if k < FStartIndex then
      k := FStartIndex;
  until False;
end;

function TVirtualLocalIndex.GetNextItem(const From: integer; const Constraint: PVirtualConstraint): integer;
var
  ConstraintItem: PVirtualConstraintItem;
  cmp: integer;
  FieldPtr, Ptr, Buffer: IntPtr;
begin
  Result := -1;
  Constraint^.CurrentItem := -1;

  if From >= FItems.Count then
    Exit;

  ConstraintItem := @Constraint^.Items[Constraint^.SortItemIndex];
  if ConstraintItem^.Value.ValueType = vrNull then
    Exit;

  FieldPtr := GetFieldPtr(FData.FFields[ConstraintItem^.FieldIndex].ActualIndex);
  Ptr := FItems[From];
  Buffer := GetBuffer(Ptr);

  if ConstraintItem^.NativeCompare then begin
    if IsIntegerField(FieldPtr) then
      cmp := FData.CompareInteger(FieldPtr, Buffer, ConstraintItem^.Value.IntValue)
    else if IsFloatField(FieldPtr) or IsDateTimeField(FieldPtr) then
      cmp := FData.CompareFloat(FieldPtr, Buffer, ConstraintItem^.Value.FloatValue)
    else if IsAnsiStringField(FieldPtr) then
      cmp := FData.CompareAnsiString(FieldPtr, Buffer, AnsiString(ConstraintItem^.Value.Value))
    else if IsWideStringField(FieldPtr) then
      cmp := FData.CompareWideString(FieldPtr, Buffer, WideString(ConstraintItem^.Value.Value))
    else
      Exit;
  end
  else
    cmp := FData.CompareVariant(FieldPtr, Buffer, ConstraintItem^.Value.Value);

  case ConstraintItem^.Operation of
    ntEqual:
      if cmp = 0 then
        Result := From;
    ntLessEqual:
      if cmp <= 0 then
        Result := From;
    ntMoreEqual:
      if cmp >= 0 then
        Result := From;
    ntMore:
      if cmp > 0 then
        Result := From;
    ntLess:
      if cmp < 0 then
        Result := From;
  end;

  if Result >= -1 then
    Constraint^.CurrentItem := From;
end;

procedure TVirtualLocalIndex.DeleteItem(const Item: IntPtr);
var
  Idx: integer;
begin
  Idx := FItems.IndexOf(Item);
  if Idx >= 0 then
    FItems.Delete(Idx);
end;

{ TVirtualData }

constructor TVirtualData.Create;
begin
  inherited;

  FCanUseLocalindex := False;
  FPrepared := False;

  FLocalIndexes := TStringList.Create;

  CreateSQLInfo;
end;

destructor TVirtualData.Destroy;
begin
  ClearLocalIndexes;
  FLocalIndexes.Free;

  FSQLInfo.Free;

  inherited;
end;

{ TFieldAccessor }

class function TFieldAccessor.AsInteger(const Field, Buffer: IntPtr): Int64;
begin
  Result := 0;
end;

class function TFieldAccessor.AsFloat(const Field, Buffer: IntPtr): double;
begin
  Result := 0;
end;

class function TFieldAccessor.AsAnsiString(const Field, Buffer: IntPtr): AnsiString;
begin
  Result := '';
end;

class function TFieldAccessor.AsWideString(const Field, Buffer: IntPtr): WideString;
begin
  Result := '';
end;

{ TVirtualSQLInfo }

constructor TVirtualSQLInfo.Create(ParserClass: TSQLParserClass; const Lexems: TLexemList);
begin
  inherited Create(ParserClass);

  FLexems := Lexems;
end;

function TVirtualSQLInfo.LeftQuote: Char;
begin
  Result := '[';
end;

function TVirtualSQLInfo.RightQuote: Char;
begin
  Result := ']';
end;

function TVirtualSQLInfo.IsQuoted(const Value: string): boolean;
var
  l: integer;
begin
  l := Length(Value);
  if (l <= 1) then
    Result := False
  else
    Result := CharInSet(Value[1], ['"', '[', '`', '''']) and CharInSet(Value[l], ['"', '[', '`', '''']);
end;

function TVirtualSQLInfo.NormalizeName(const Name: string): string;
var
  TmpName: string;
begin
  TmpName := UnQuote(Name);

  if FLexems.IndexOf(UpperCase(TmpName)) >= 0 then
    Result := Quote(TmpName)
  else
    Result := QuoteIfNeed(Name);
end;

{ TVirtualData }

function TVirtualData.CompareFieldValue(const Constraint: PVirtualConstraintItem): boolean;
var
  Value: Variant;
  Rowid,
  ValInt64: Int64;
begin
  if Constraint^.FieldIndex = -1 then begin
    Rowid := Int64(GetBookmark);
    ValInt64 := Constraint^.Value.Value;

    case Constraint^.Operation of
      ntEqual: Result := Rowid = ValInt64;
      ntMore: Result := Rowid > ValInt64;
      ntLess: Result := Rowid < ValInt64;
      ntMoreEqual: Result := Rowid >= ValInt64;
      ntLessEqual: Result := Rowid <= ValInt64;
      ntNoEqual: Result := Rowid <> ValInt64;
    else
      Result := False;
    end;
  end
  else if Constraint^.Value.ValueType <> vrNull then begin
    if Constraint^.SimpleCompare then begin
      if Constraint^.NativeCompare then begin
        case Constraint^.Value.ValueType of
          vrInteger:
            Result := InternalCompareFieldValue(Constraint^.FieldIndex, Constraint^.Operation, Constraint^.Value.IntValue);
          vrFloat:
            Result := InternalCompareFieldValue(Constraint^.FieldIndex, Constraint^.Operation, Constraint^.Value.FloatValue);
        else
          Result := InternalCompareFieldValue(Constraint^.FieldIndex, Constraint^.Operation, Constraint^.Value.Value);
        end;
      end
      else
        Result := InternalCompareFieldValue(Constraint^.FieldIndex, Constraint^.Operation, Constraint^.Value.Value);
    end
    else begin
      ValueToVariant(Constraint^.FieldIndex, Constraint^.Value, Value);
      Result := InternalCompareFieldValue(Constraint^.FieldIndex, Constraint^.Operation, Value);
    end;
  end
  else
    Result := False;
end;

class function TVirtualData.GetLocalIndexClass: TVirtualLocalIndexClass;
begin
  Result := TVirtualLocalIndex;
end;

procedure TVirtualData.CreateSQLInfo;
begin
  FSQLInfo := TVirtualSQLInfo.Create(TLiteParser, LiteKeywordLexems);
end;

function TVirtualData.ValueToVariant(const FieldIndex: integer; const VirtualValue: TVirtualValue; out Value: variant): boolean;
var
  ValInt64: Int64;
  ValFloat: Extended;
  ValCurr: Currency;
  ValBool: boolean;
  ValTs: TSQLTimeStampOffset;
begin
  Result := True;

  case FFields[FieldIndex].DataType of
    dtString,
    dtExtString,
    dtFixedChar: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: Value := string(VirtualValue.Value);
        vrInteger: Value := IntToStr(VirtualValue.Value);
        vrFloat: Value := FloatToStr(VirtualValue.Value);
        vrBlob: Value := Encoding.UTF8.GetString(TBytes(VirtualValue.Value));
      else
        VarClear(Value);
      end;
    end;
    dtWideString,
    dtExtWideString,
    dtFixedWideChar: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: Value := string(VirtualValue.Value);
        vrInteger: Value := IntToStr(VirtualValue.Value);
        vrFloat: Value := FloatToStr(VirtualValue.Value);
      {$IFDEF NEXTGEN}
        vrBlob: Value := Encoding.UTF8.GetString(TBytes(VirtualValue.Value));
      {$ELSE}
        vrBlob: Value := Encoding.UTF8.GetWideString(TBytes(VirtualValue.Value));
      {$ENDIF}
      else
        VarClear(Value);
      end;
    end;
    dtByte,
    dtInt8,
    dtWord,
    dtSmallint,
    dtLongWord,
    dtInteger,
    dtLargeint,
    dtUInt64: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: begin
          Result := TryStrToInt64(VirtualValue.Value, ValInt64);
          if Result then
            Value := ValInt64;
        end;
        vrInteger: begin
          ValInt64 := VirtualValue.Value;
          Value := ValInt64;
        end;  
        vrFloat: Value := double(VirtualValue.Value);
      else
        VarClear(Value);
      end;
    end;
    dtSingle,
    dtFloat,
    dtExtended: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: begin
          Result := TryStrToFloat(VirtualValue.Value, ValFloat);
          if Result then
            Value := ValFloat;
        end;
        vrInteger: begin
          ValInt64 := VirtualValue.Value;
          Value := ValInt64;
        end;  
        vrFloat: Value := Extended(VirtualValue.Value);
      else
        VarClear(Value);
      end;
    end;
    dtCurrency: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: begin
          Result := TryStrToCurr(VirtualValue.Value, ValCurr);
          if Result then
            Value := ValCurr;
        end;
        vrInteger: begin
          ValInt64 := VirtualValue.Value;
          Value := ValInt64;
        end;
        vrFloat: Value := Currency(VirtualValue.Value);
      else
        VarClear(Value);
      end;
    end;
    dtDate: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: Value := ConvertStringToDateTime(dtDate, string(VirtualValue.Value));
        vrInteger,
        vrFloat: Value := TDateTime(double(VirtualValue.Value));
      else
        VarClear(Value);
      end;
    end;
    dtTime: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: Value := ConvertStringToDateTime(dtTime, string(VirtualValue.Value));
        vrInteger,
        vrFloat: Value := TDateTime(double(VirtualValue.Value));
      else
        VarClear(Value);
      end;
    end;
    dtDateTime: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: Value := ConvertStringToDateTime(dtDateTime, string(VirtualValue.Value));
        vrInteger,
        vrFloat: Value := TDateTime(double(VirtualValue.Value));
      else
        VarClear(Value);
      end;
    end;
    dtSQLTimeStampOffset: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: begin
          ValTs := ConvertStrToTimestampOffset(string(VirtualValue.Value), '-', ':', 'yyyy-mm-dd', 'hh:nn:ss');
          Value := VarSQLTimeStampOffsetCreate(ValTs);
        end;
      else
        VarClear(Value);
      end;
    end;
    dtBoolean: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: begin
          Result := TryStrToBool(VirtualValue.Value, ValBool);
          if Result then
            Value := ValBool;
        end;
        vrInteger: Value := boolean(VirtualValue.Value);
        vrFloat: Value := boolean(VirtualValue.Value);
      else
        VarClear(Value);
      end;
    end;
    dtBCD,
    dtFMTBCD:
      Value := double(VirtualValue.Value);
    dtMemo: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: Value := string(VirtualValue.Value);
        vrInteger: Value := IntToStr(VirtualValue.Value);
        vrFloat: Value := FloatToStr(VirtualValue.Value);
        vrBlob: Value := Encoding.UTF8.GetString(TBytes(VirtualValue.Value));
      else
        VarClear(Value);
      end;
    end;
    dtBytes,
    dtBlob: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: Value := Encoding.UTF8.GetBytes(string(VirtualValue.Value));
        vrBlob: Value := TBytes(VirtualValue.Value);
      else
        VarClear(Value);
      end;
    end;
    dtWideMemo: begin
      case VirtualValue.ValueType of
        vrString,
        vrAnsiString,
        vrWideString: Value := string(VirtualValue.Value);
        vrInteger: Value := IntToStr(VirtualValue.Value);
        vrFloat: Value := FloatToStr(VirtualValue.Value);
      {$IFDEF NEXTGEN}
        vrBlob: Value := Encoding.UTF8.GetString(TBytes(VirtualValue.Value));
      {$ELSE}
        vrBlob: Value := Encoding.UTF8.GetWideString(TBytes(VirtualValue.Value));
      {$ENDIF}
      else
        VarClear(Value);
      end;
    end;
  else
    VarClear(Value);
  end;
end;

function TVirtualData.OmitRecord(const Filter: PVirtualConstraint; const ExcludeIndex: integer = -1): boolean;
var
  i, n: integer;
begin
  Result := False;

  n := Length(Filter^.Items);
  if n = 0 then
    Exit;

  for i := 0 to n - 1 do
    if (ExcludeIndex = -1) or (ExcludeIndex <> i) then
      if not CompareFieldValue(@Filter^.Items[i]) then begin
        Result := True;
        Break;
      end;
end;

function TVirtualData.GetNextRecord(const Filter: PVirtualConstraint): boolean;
begin
  repeat
    Result := not InternalEof(Filter);
    if not Result then
      Break;

    InternalNext;

    Result := not InternalEof(Filter);
    if not Result then
      Break;

    Result := not OmitRecord(Filter);
    if Result then
      Break;
  until False;

  if Result then
    InternalGetCurrentRecord;
end;

procedure TVirtualData.InternalOpen;
begin
  // do nothing
end;

procedure TVirtualData.InternalDescribeFields(const SpecificTypes: TSpecificTypes);
begin
  // do nothing
end;

procedure TVirtualData.InternalGetCurrentRecord;
begin
  // do nothing
end;

function TVirtualData.IsSupportedDataType(DataType: word): boolean;
begin
  Result := DataType in [dtString .. dtWideMemo];
end;

function TVirtualData.GetFieldDataTypeName(const Field: PVirtualFieldDesc): string;
begin
  case Field^.DataType of
    dtObject: Result := 'OBJECT';
    dtArray: Result := 'ARRAY';
    dtTable: Result := 'ATABLE';
    dtGuid: Result := 'GUID';
    dtCursor: Result := 'CURSOR';
    dtXML: Result := 'XML';
    dtBlob: Result := 'BLOB';
    dtString: Result := 'VARCHAR';
    dtExtString: Result := 'EXTVARCHAR';
    dtWideString: Result := 'WVARCHAR';
    dtExtWideString: Result := 'EXTWVARCHAR';
    dtFixedChar: Result := 'CHAR';
    dtFixedWideChar: Result := 'WCHAR';
    dtByte: Result := 'BYTE';
    dtInt8: Result := 'TINYINT';
    dtWord: Result := 'WORD';
    dtSmallint: Result := 'SMALLINT';
    dtBoolean: Result := 'BOOLEAN';
    dtLongWord: Result := 'LONGWORD';
    dtInteger: Result := 'INTEGER';
    dtLargeint: Result := 'BIGINT';
    dtUInt64: Result := 'UBIGINT';
    dtSingle: Result := 'REAL';
    dtFloat:
      if Field.Scale = 0 then
        Result := 'FLOAT'
      else
        Result := 'NUMERIC';
    dtExtended: Result := 'EXTENDED';
    dtCurrency: Result := 'MONEY';
    dtBCD: Result := 'BCD';
    dtFMTBCD: Result := 'FMTBCD';
    dtDate: Result := 'DATE';
    dtTime: Result := 'TIME';
    dtDateTime: Result := 'DATETIME';
    dtSQLTimeStamp: Result := 'TIMESTAMP';
    dtSQLTimeStampOffset: Result := 'TIMESTAMPOFFSET';
    dtBytes: Result := 'BYTES';
    dtVarBytes: Result := 'VARBYTES';
    dtExtVarBytes: Result := 'EXTVARBYTES';
    dtMemo: Result := 'TEXT';
    dtWideMemo: Result := 'WTEXT';
  else
    Result := 'BLOB';
  end;
end;

function TVirtualData.GetFieldDefinition(const Field: PVirtualFieldDesc): string;
begin
  case Field^.DataType of
    dtGuid,
    dtString,
    dtExtString,
    dtWideString,
    dtExtWideString,
    dtFixedChar,
    dtFixedWideChar,
    dtBytes,
    dtVarBytes,
    dtExtVarBytes: Result := GetFieldDataTypeName(Field) + '(' + IntToStr(Field^.Length) + ')';
    dtBCD,
    dtFMTBCD: Result := GetFieldDataTypeName(Field) + '(' + IntToStr(Field^.Length) + ', ' + IntToStr(Field^.Scale) + ')';
    dtFloat: begin
      Result := GetFieldDataTypeName(Field);
      Result := Result + '(' + IntToStr(Field^.Length) + ', ' + IntToStr(Field^.Scale) + ')';
    end;
    dtTime,
    dtDateTime,
    dtSQLTimeStamp: begin
      Result := GetFieldDataTypeName(Field);
      if Field^.Scale > 0 then
        Result := Result + '(' + IntToStr(Field^.Scale) + ')';
    end;
  else
    Result := GetFieldDataTypeName(Field);
  end;
end;

function TVirtualData.CompareInteger(const Field, Buffer: IntPtr; const Value: Int64): integer;
var
  FieldValue: Int64;
begin
  FieldValue := GetFieldAccessorClass.AsInteger(Field, Buffer);

  if FieldValue = Value then
    Result := 0
  else if FieldValue > Value then
    Result := 1
  else
    Result := -1;
end;

function TVirtualData.CompareFloat(const Field, Buffer: IntPtr; const Value: double): integer;
var
  FieldValue: double;
  Epsilon: double;
begin
  FieldValue := GetFieldAccessorClass.AsFloat(Field, Buffer);

  if DoubleValueDelta = 0 then
    Epsilon := Math.Max(Min(Abs(FieldValue), Abs(Value)) * DoubleResolution, DoubleResolution)
  else
    Epsilon := DoubleValueDelta;

  if FieldValue > Value then begin
    if (FieldValue - Value) <= Epsilon then
      Result := 0
    else
      Result := 1;
  end
  else begin
    if (Value - FieldValue) <= Epsilon then
      Result := 0
    else
      Result := -1;
  end;
end;

function TVirtualData.CompareAnsiString(const Field, Buffer: IntPtr; const Value: AnsiString): integer;
var
  FieldValue: AnsiString;
begin
  FieldValue := GetFieldAccessorClass.AsAnsiString(Field, Buffer);

  Result := CompareStr(string(FieldValue), string(Value));
end;

function TVirtualData.CompareWideString(const Field, Buffer: IntPtr; const Value: WideString): integer;
var
  FieldValue: WideString;
begin
  FieldValue := GetFieldAccessorClass.AsWideString(Field, Buffer);

  Result := CompareStr(string(FieldValue), string(Value));
end;

function TVirtualData.CompareVariant(const Field, Buffer: IntPtr; const Value: Variant): integer;
begin
  Result := -2;
end;

function TVirtualData.GetLocalIndex(const Constraint: PVirtualConstraint): integer;
begin
  Result := -1;
end;

procedure TVirtualData.DropLocalIndex(const FieldName: string);
var
  Idx: integer;
begin
  Idx := FLocalIndexes.IndexOf(FieldName);
  if Idx >= 0 then begin
    TVirtualLocalIndex(FLocalIndexes.Objects[Idx]){$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
    FLocalIndexes.Delete(Idx);
  end;
end;

procedure TVirtualData.DeleteIndexItem(const Item: TVirtualBookmark);
var
  i{$IFDEF FPC}, n{$ENDIF}: integer;
begin
{$IFDEF FPC}
  n := Item;
{$ENDIF}
  for i := 0 to FLocalIndexes.Count - 1 do
    TVirtualLocalIndex(FLocalIndexes.Objects[i]).DeleteItem(IntPtr({$IFNDEF FPC}Item{$ELSE}(@n)^{$ENDIF}));
end;

procedure TVirtualData.ClearLocalIndexes;
var
  i: integer;
begin
  for i := 0 to FLocalIndexes.Count - 1 do
    TVirtualLocalIndex(FLocalIndexes.Objects[i]){$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
  FLocalIndexes.Clear;
end;

function TVirtualData.Prepared: boolean;
begin
  Result := FPrepared;
end;

function TVirtualData.GetCreateSQL: string;
const
  Separator: array[boolean] of string = (', ', '');
var
  i, n: integer;
  Builder: StringBuilder;
  KeyFields: TStrings;
begin
  Builder := StringBuilder.Create(1024);
  KeyFields := TStringList.Create;

  try
    n := Length(FFields) - 1;

    if n >= 0 then begin
      for i := 0 to n do
        if FFields[i].IsKey and (KeyFields.IndexOf(FFields[i].Name) < 0) then
          KeyFields.Add(FFields[i].Name);

      Builder.Append('CREATE TABLE VT(');

      n := Length(FFields) - 1;
      for i := 0 to n do begin
        Builder.Append(FSQLInfo.NormalizeName(FFields[i].Name));
        Builder.Append(' ');
        if FFields[i].Hidden then
          Builder.Append('HIDDEN ');
        Builder.Append(GetFieldDefinition(@FFields[i]));
        if FFields[i].IsKey and (KeyFields.Count = 1) then
          Builder.Append(' PRIMARY KEY');
        if FFields[i].Required and
           (not FFields[i].IsKey or (KeyFields.Count > 1) or (FFields[i].DataType <> dtInteger))
        then
          Builder.Append(' NOT NULL');
        Builder.Append(Separator[i = n]);
      end;

      if KeyFields.Count > 1 then begin
        Builder.Append(', PRIMARY KEY(');
        for i := 0 to KeyFields.Count - 1 do begin
          Builder.Append(FSQLInfo.NormalizeName(KeyFields[i]));
          Builder.Append(Separator[i = (KeyFields.Count - 1)]);
        end;
        Builder.Append(')');
      end;

      Builder.Append(')');

      Result := Builder.ToString;
    end
    else
      Result := '';
  {$IFDEF OFS}
    OFS(Result);
  {$ENDIF}
  finally
    Builder.Free;
    KeyFields.Free;
  end;
end;

procedure TVirtualData.Prepare;
begin
  FPrepared := True;
end;

procedure TVirtualData.DescribeFields(const SpecificTypes: TSpecificTypes);
begin
  if not Prepared then
    Prepare;

  InternalDescribeFields(SpecificTypes);
end;

procedure TVirtualData.PrepareConstraints(const Filter: PVirtualConstraint; var Cost: integer);
var
  i: integer;
begin
  Cost := 0;

  for i := 0 to Length(Filter^.Items) - 1 do
    if Filter^.Items[i].FieldIndex <> -100 then begin
      if Filter^.Items[i].FieldIndex = -1 then
        Cost := Cost + 1
      else if (FFields[Filter^.Items[i].FieldIndex].IsKey) or (FFields[Filter^.Items[i].FieldIndex].IsAutoIncrement) then
        Cost := Cost + 5
      else
        Cost := Cost + 10;
    end;

  if Cost = 0 then
    Cost := Length(FFields) * 10;
end;

procedure TVirtualData.PrepareFilter(const Filter: PVirtualConstraint);
var
  Index: TVirtualMemDataIndex;
  i: integer;
  pItem: PVirtualConstraintItem;
  FieldPtr: IntPtr;
  i64: Int64;
  dt: TDateTime;
  d: double;
begin
  if Filter^.LocalIndex >= FLocalIndexes.Count then begin
    Filter^.LocalIndex := -1;
    Exit;
  end;

  Index := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex]);

  for i := 0 to Length(Filter^.Items) - 1 do begin
    pItem := @Filter^.Items[i];
    FieldPtr := Index.GetFieldPtr(Index.FData.FFields[pItem^.FieldIndex].ActualIndex);

    if Index.IsIntegerField(FieldPtr) then begin
      pItem^.NativeCompare := pItem^.Value.ValueType = vrInteger;

      if not pItem^.NativeCompare then
        case pItem^.Value.ValueType of
          vrAnsiString:
            if TryStrToInt64(string(pItem^.Value.AnsiStrValue), i64) then begin
              pItem^.Value.ValueType := vrInteger;
              pItem^.Value.IntValue := i64;
              pItem^.Value.Value := pItem^.Value.IntValue;
              pItem^.NativeCompare := True;
            end;
          vrWideString:
            if TryStrToInt64(string(pItem^.Value.WideStrValue), i64) then begin
              pItem^.Value.ValueType := vrInteger;
              pItem^.Value.IntValue := i64;
              pItem^.Value.Value := pItem^.Value.IntValue;
              pItem^.NativeCompare := True;
            end;
        end;
    end
    else if Index.IsFloatField(FieldPtr) or Index.IsDateTimeField(FieldPtr) then begin
      pItem^.NativeCompare := pItem^.Value.ValueType = vrFloat;

      if not pItem^.NativeCompare then
        case pItem^.Value.ValueType of
          vrInteger: begin
            pItem^.Value.ValueType := vrFloat;
            pItem^.Value.FloatValue := pItem^.Value.IntValue;
            pItem^.Value.Value := pItem^.Value.FloatValue;
            pItem^.NativeCompare := True;
          end;
          vrAnsiString:
            if Index.IsDateTimeField(FieldPtr) then begin
              dt := ConvertStringToDateTime(FFields[pItem^.FieldIndex].DataType, string(pItem^.Value.AnsiStrValue));
              pItem^.Value.ValueType := vrFloat;
              pItem^.Value.FloatValue := dt;
              pItem^.Value.Value := pItem^.Value.FloatValue;
              pItem^.NativeCompare := True;
            end
            else if TryStrToFloat(string(pItem^.Value.AnsiStrValue), d) then begin
              pItem^.Value.ValueType := vrFloat;
              pItem^.Value.FloatValue := d;
              pItem^.Value.Value := pItem^.Value.FloatValue;
              pItem^.NativeCompare := True;
            end;
          vrWideString:
            if Index.IsDateTimeField(FieldPtr) then begin
              dt := ConvertStringToDateTime(FFields[pItem^.FieldIndex].DataType, string(pItem^.Value.WideStrValue));
              pItem^.Value.ValueType := vrFloat;
              pItem^.Value.FloatValue := dt;
              pItem^.Value.Value := pItem^.Value.FloatValue;
              pItem^.NativeCompare := True;
            end
            else if TryStrToFloat(string(pItem^.Value.WideStrValue), d) then begin
              pItem^.Value.ValueType := vrFloat;
              pItem^.Value.FloatValue := d;
              pItem^.Value.Value := pItem^.Value.FloatValue;
              pItem^.NativeCompare := True;
            end;
        end;
    end
    else
      if Index.IsAnsiStringField(FieldPtr) or Index.IsWideStringField(FieldPtr) then begin
        pItem^.NativeCompare := pItem^.Value.ValueType in [vrAnsiString, vrWideString];
        {TODO: convert value to string}
      end;
  end;
end;

procedure TVirtualData.CheckActive;
begin
  if FAutoClose and not Active then
    InternalOpen;
end;

function TVirtualData.Open(const Filter: PVirtualConstraint): TVirtualBookmark;
var
  ValInt64: Int64;
begin
  if (Filter <> nil) then begin
    // performance optimization on record editing
    if (Length(Filter^.Items) = 1) and (Filter^.Items[0].FieldIndex = -1) and (Filter^.Items[0].Operation = ntEqual) then begin
      ValInt64 := Filter^.Items[0].Value.Value;
      Result := TVirtualBookmark(ValInt64);
      GotoBookmark(Result);
      InternalGetCurrentRecord;
    end
    else if GetNextRecord(Filter) then
      Result := GetBookmark
    else
      Result := -1;
  end
  else
    Result := -1;
end;

procedure TVirtualData.Close;
begin
  // do nothing
end;

procedure TVirtualData.Reset;
begin
  // do nothing
end;

function TVirtualData.Next(const Bookmark: TVirtualBookmark; const Filter: PVirtualConstraint): TVirtualBookmark;
begin
  if Bookmark <> -1 then begin
    GotoBookmark(Bookmark);

    if GetNextRecord(Filter) then
      Result := GetBookmark
    else
      Result := -1;
  end
  else
    Result := -1;
end;

function TVirtualData.Eof(const Bookmark: TVirtualBookmark): boolean;
begin
  Result := Bookmark = -1;
end;

function TVirtualData.GetFieldType(FieldIndex: integer): word;
begin
  Result := FFields[FieldIndex].DataType;
end;

function TVirtualData.GetFieldScale(FieldIndex: integer): word;
begin
  Result := FFields[FieldIndex].Scale;
end;

function TVirtualData.GetFieldValue(FieldIndex: integer; var FieldNull: boolean): variant;
begin
  Result := Unassigned;
  FieldNull := GetFieldNull(FieldIndex);
  if not FieldNull then
    Result := GetFieldValue(FieldIndex);
end;

procedure TVirtualData.FreeFieldValue(Value: variant);
begin
  // do nothing
end;

procedure TVirtualData.DisableControls(SaveRecNo: boolean);
begin
  // do nothing
end;

procedure TVirtualData.EnableControls;
begin
  // do nothing
end;

{ TMemDataFieldAccessor }

class function TMemDataFieldAccessor.AsInteger(const Field, Buffer: IntPtr): Int64;
var
  FieldDesc: TFieldDesc;
begin
  FieldDesc := TFieldDesc(Field);

  case FieldDesc.DataType of
    dtInt32, dtUInt32:
      Result := Marshal.ReadInt32(PtrOffset(Buffer, FieldDesc.DataOffset));
    dtInt64, dtUInt64:
      Result := Marshal.ReadInt64(PtrOffset(Buffer, FieldDesc.DataOffset));
    dtInt8, dtUInt8:
      Result := Marshal.ReadByte(PtrOffset(Buffer, FieldDesc.DataOffset));
    dtInt16, dtUInt16:
      Result := Marshal.ReadInt16(PtrOffset(Buffer, FieldDesc.DataOffset));
  else
    Result := 0;
  end;
end;

class function TMemDataFieldAccessor.AsFloat(const Field, Buffer: IntPtr): double;
var
  FieldDesc: TFieldDesc;
begin
  FieldDesc := TFieldDesc(Field);

  case FieldDesc.DataType of
    dtFloat, dtCurrency, dtDate, dtTime, dtDateTime:
      Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(PtrOffset(Buffer, FieldDesc.DataOffset)));
    dtSingle:
      Result := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(PtrOffset(Buffer, FieldDesc.DataOffset)));
    dtInt8, dtUInt8:
      Result := Marshal.ReadByte(PtrOffset(Buffer, FieldDesc.DataOffset));
    dtInt16, dtUInt16:
      Result := Marshal.ReadInt16(PtrOffset(Buffer, FieldDesc.DataOffset));
    dtInt32, dtUInt32:
      Result := Marshal.ReadInt32(PtrOffset(Buffer, FieldDesc.DataOffset));
    dtInt64, dtUInt64:
      Result := Marshal.ReadInt64(PtrOffset(Buffer, FieldDesc.DataOffset));
  else
    Result := 0;
  end;
end;

class function TMemDataFieldAccessor.AsAnsiString(const Field, Buffer: IntPtr): AnsiString;
var
  FieldDesc: TFieldDesc;
  LenPtr: PWord;
  DataBuf: IntPtr;
begin
  FieldDesc := TFieldDesc(Field);
  LenPtr := PtrOffset(Buffer, FieldDesc.Offset);
  DataBuf := PtrOffset(Buffer, FieldDesc.DataOffset);
  if FieldDesc.DataType = dtExtString then
    DataBuf := Marshal.ReadIntPtr(DataBuf);

  Result := Marshal.PtrToStringAnsi(DataBuf, LenPtr^);
end;

class function TMemDataFieldAccessor.AsWideString(const Field, Buffer: IntPtr): WideString;
var
  FieldDesc: TFieldDesc;
  LenPtr: PWord;
  DataBuf: IntPtr;
begin
  FieldDesc := TFieldDesc(Field);
  LenPtr := PtrOffset(Buffer, FieldDesc.Offset);
  DataBuf := PtrOffset(Buffer, FieldDesc.DataOffset);
  if FieldDesc.DataType = dtExtWideString then
    DataBuf := Marshal.ReadIntPtr(DataBuf);

  Result := Marshal.PtrToStringUni(DataBuf, LenPtr^);
end;

{ TVirtualMemDataIndex }

function TVirtualMemDataIndex.GetFieldPtr(const FieldIndex: integer): IntPtr;
begin
  Result := TVirtualMemData(FData).FMemData.Fields[FieldIndex];
end;

function TVirtualMemDataIndex.GetBuffer(const Item: pointer): IntPtr;
begin
  Result := PtrOffset(Item, SizeOf(TItemHeader));
end;

function TVirtualMemDataIndex.InternalCompare(const FieldPtr: IntPtr; Item1, Item2: pointer): integer;
var
  Buffer1, Buffer2: IntPtr;
  IntValue: Int64;
  FloatValue: Double;
  AStrValue: AnsiString;
  WStrValue: WideString;
begin
  Buffer1 := GetBuffer(Item1);
  Buffer2 := GetBuffer(Item2);

  FNull1 := TVirtualMemData(FData).FMemData.GetNull(TFieldDesc(FieldPtr), Buffer1);
  FNull2 := TVirtualMemData(FData).FMemData.GetNull(TFieldDesc(FieldPtr), Buffer2);

  if FNull1 then begin
    if FNull2 then begin
      Result := 0;
      Exit;
    end
    else begin
      Result := -1;
      Exit;
    end;
  end
  else if FNull2 then begin
    Result := 1;
    Exit;
  end;

  case TFieldDesc(FieldPtr).DataType of
    dtInt8, dtUInt8,
    dtInt16, dtUInt16,
    dtInt32, dtUInt32,
    dtInt64, dtUInt64: begin
      IntValue := FData.GetFieldAccessorClass.AsInteger(FieldPtr, Buffer2);
      Result := FData.CompareInteger(FieldPtr, Buffer1, IntValue);
    end;
    dtSingle, dtFloat,
    dtDate, dtTime, dtDateTime: begin
      FloatValue := FData.GetFieldAccessorClass.AsFloat(FieldPtr, Buffer2);
      Result := FData.CompareFloat(FieldPtr, Buffer1, FloatValue);
    end;
    dtString, dtExtString, dtFixedChar: begin
      AStrValue := FData.GetFieldAccessorClass.AsAnsiString(FieldPtr, Buffer2);
      Result := FData.CompareAnsiString(FieldPtr, Buffer1, AStrValue);
    end;
    dtWideString, dtExtWideString, dtFixedWideChar: begin
      WStrValue := FData.GetFieldAccessorClass.AsWideString(FieldPtr, Buffer2);
      Result := FData.CompareWideString(FieldPtr, Buffer1, WStrValue);
    end;
  else
    Result := 0;
  end;
end;

function TVirtualMemDataIndex.IsIntegerField(const FieldPtr: IntPtr): boolean;
begin
  Result := TFieldDesc(FieldPtr).DataType in [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtInt64, dtUInt64];
end;

function TVirtualMemDataIndex.IsDateTimeField(const FieldPtr: IntPtr): boolean;
begin
  Result := TFieldDesc(FieldPtr).DataType in [dtDate, dtTime, dtDateTime];
end;

function TVirtualMemDataIndex.IsFloatField(const FieldPtr: IntPtr): boolean;
begin
  Result := IsDateTimeField(FieldPtr) or (TFieldDesc(FieldPtr).DataType in [dtSingle, dtFloat]);
end;

function TVirtualMemDataIndex.IsAnsiStringField(const FieldPtr: IntPtr): boolean;
begin
  Result := TFieldDesc(FieldPtr).DataType in [dtString, dtExtString, dtFixedChar, dtMemo];
end;

function TVirtualMemDataIndex.IsWideStringField(const FieldPtr: IntPtr): boolean;
begin
  Result := TFieldDesc(FieldPtr).DataType in [dtWideString, dtExtWideString, dtFixedWideChar, dtWideMemo];
end;

procedure TVirtualMemDataIndex.Sort(const FieldIndex: integer);
var
  Cnt: integer;
  FieldPtr: IntPtr;
begin
  inherited;

  Cnt := FItems.Count;
  FieldPtr := GetFieldPtr(FieldIndex);
  while (FStartIndex < Cnt) and (TVirtualMemData(FData).FMemData.GetNull(TFieldDesc(FieldPtr), PtrOffset(FItems[FStartIndex], sizeof(TItemHeader)))) do
    Inc(FStartIndex);
end;

{ TVirtualMemData }

constructor TVirtualMemData.Create(const MemData: TMemData);
begin
  inherited Create;

  FMemData := MemData;
  FUseFilter := False;
  FCanUseLocalindex := True;

  FFieldNode := TExpressionNode.Create;
  FFieldNode.NodeType := ntField;
  FValueNode := TExpressionNode.Create;
  FValueNode.NodeType := ntValue;
  FOperationNode := TExpressionNode.Create;
  FOperationNode.LeftOperand := FFieldNode;
  FOperationNode.RightOperand := FValueNode;
end;

destructor TVirtualMemData.Destroy;
begin
  FFieldNode.Free;
  FOperationNode.Free;
  FValueNode.Free;

  inherited;
end;

class function TVirtualMemData.GetLocalIndexClass: TVirtualLocalIndexClass;
begin
  Result := TVirtualMemDataIndex;
end;

function TVirtualMemData.GetFieldAccessorClass: TFieldAccessorClass;
begin
  Result := TMemDataFieldAccessor;
end;

function TVirtualMemData.OmitRecord(const Filter: PVirtualConstraint; const ExcludeIndex: integer = -1): boolean;
begin
  Result := False;

  if not FUseFilter and (Length(Filter^.Items) > 0) then begin
    TMemDataHelper(FMemData).FilterRecBuf := TMemDataHelper(FMemData).BlockMan.GetRecordPtr(TMemDataHelper(FMemData).CurrentItem);
    Result := inherited OmitRecord(Filter, ExcludeIndex);
  end;
end;

function TVirtualMemData.GetNextRecord(const Filter: PVirtualConstraint): boolean;
var
  Index: integer;
begin
  if Filter^.LocalIndex < 0 then
    Result := inherited GetNextRecord(Filter)
  else begin
    Index := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter);

    if (Index >= 0) and (Length(Filter^.Items) > 1) then
      repeat
        FMemData.GetBookmark(@FBookmark);
        FBookmark.Item := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index];
        FMemData.SetToBookmark(@FBookmark);

        if OmitRecord(Filter, Filter^.SortItemIndex) then
          Index := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter)
        else
          Break;
      until Index = -1;

    if Index >= 0 then begin
      if Length(Filter^.Items) <= 1 then begin
        FMemData.GetBookmark(@FBookmark);
        FBookmark.Item := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index];
        FMemData.SetToBookmark(@FBookmark);
      end;

      InternalGetCurrentRecord;
      Result := True;
    end
    else
      Result := False;
  end;
end;

procedure TVirtualMemData.InternalOpen;
begin
  FMemData.Open;
end;

function TVirtualMemData.InternalOpenNoIndex(const Filter: PVirtualConstraint): TVirtualBookmark;
begin
  FMemData.GetBookmark(@FOpenBookmark);
  FMemData.SetToBegin;
  Result := inherited Open(Filter);
end;

procedure TVirtualMemData.InternalDescribeFields(const SpecificTypes: TSpecificTypes);
var
  i, j, n, Len, Scale: integer;
  DataTypeName: string;
  DataType: word;
  FieldDesc: TFieldDesc;
begin
  inherited;

  if not (FMemData.Active or FMemData.Prepared) then
    raise EVirtualQueryError.Create(SDataSetNotOpened);

  n := 0;
  for i := 0 to FMemData.Fields.Count - 1 do
    if not (FMemData.Fields[i].DataType in [dtObject, dtArray]) and not ((FMemData.Fields[i] is TCRFieldDesc) and TCRFieldDesc(FMemData.Fields[i]).Hidden) then
      Inc(n);

  SetLength(FFields, n);

  j := 0;
  for i := 0 to FMemData.Fields.Count - 1 do begin
    FieldDesc := FMemData.Fields[i];
    if not (FieldDesc.DataType in [dtObject, dtArray]) and not ((FieldDesc is TCRFieldDesc) and TCRFieldDesc(FieldDesc).Hidden) then begin

      if (FieldDesc.ActualName <> '') and (FieldDesc.ParentField <> nil) then
        FFields[j].Name := FieldDesc.ActualName
      else
        FFields[j].Name := FieldDesc.Name;
      FFields[j].DataType := FieldDesc.DataType;
      FFields[j].Length := FieldDesc.Length;
      FFields[j].Scale := FieldDesc.Scale;
      FFields[j].IsKey := FieldDesc.IsKey;
      FFields[j].IsAutoIncrement := FieldDesc.IsAutoIncrement;
      FFields[j].Required := FieldDesc.Required;
      FFields[j].ReadOnly := FieldDesc.ReadOnly;
      FFields[j].ActualIndex := i;

      if TMemDataHelper(FMemData).IsSpecificType(FieldDesc, DataType, DataTypeName, Len, Scale) then begin
        n := SpecificTypes.IndexOf(DataTypeName);
        if n = -1 then
          n := SpecificTypes.AddObject(LowerCase(DataTypeName), TSpecificTypeDesc.Create(DataTypeName, DataType, Len, Scale));
        FFields[j].FieldObject := SpecificTypes.Objects[n];
      end;

      Inc(j);
    end;
  end;
end;

procedure TVirtualMemData.InternalGetCurrentRecord;
begin
  FMemData.GetRecord(FRecordBuffer);
end;

procedure TVirtualMemData.InternalNext;
begin
  FMemData.GetNextRecord(nil);
end;

function TVirtualMemData.InternalEof(const Filter: PVirtualConstraint): boolean;
begin
  if Filter^.LocalIndex < 0 then
    Result := FMemData.Eof
  else
    Result := Filter^.CurrentItem = -1;
end;

function TVirtualMemData.InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: variant): boolean;
var
  FieldDesc: TFieldDesc;
  OldConverter: TOnDemandConverter;
  V: Variant;
begin
  FieldDesc := FMemData.Fields[FFields[FieldIndex].ActualIndex];

  OldConverter := nil;
  try
    if FieldDesc is TCRFieldDesc then begin
      OldConverter := TCRFieldDesc(FieldDesc).OnDemandConverter;
      TCRFieldDesc(FieldDesc).OnDemandConverter := nil;
    end;

    FFieldNode.FieldDesc := FieldDesc;
    FValueNode.Value := Value;
    FOperationNode.NodeType := Operation;

    if FieldDesc.DataType = dtBytes then begin
      FMemData.GetMappedFieldAsVariant(FieldDesc, TMemDataHelper(FMemData).FilterRecBuf, V);

      case Operation of
        ntEqual: Result := Encoding.UTF8.GetString(TBytes(Value)) = Encoding.UTF8.GetString(TBytes(V));
        ntMore: Result := Encoding.UTF8.GetString(TBytes(Value)) > Encoding.UTF8.GetString(TBytes(V));
        ntLess: Result := Encoding.UTF8.GetString(TBytes(Value)) < Encoding.UTF8.GetString(TBytes(V));
        ntMoreEqual: Result := Encoding.UTF8.GetString(TBytes(Value)) >= Encoding.UTF8.GetString(TBytes(V));
        ntLessEqual: Result := Encoding.UTF8.GetString(TBytes(Value)) <= Encoding.UTF8.GetString(TBytes(V));
        ntNoEqual: Result := Encoding.UTF8.GetString(TBytes(Value)) <> Encoding.UTF8.GetString(TBytes(V));
      else
        Result := False;
      end;
    end
    else
      Result := FMemData.Eval(FOperationNode);
  finally
    if FieldDesc is TCRFieldDesc then
      TCRFieldDesc(FieldDesc).OnDemandConverter := OldConverter;
  end;
end;

function TVirtualMemData.InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: Int64): boolean;
var
  FieldDesc: TFieldDesc;
  Comp: integer;
begin
  Result := False;

  FieldDesc := FMemData.Fields[FFields[FieldIndex].ActualIndex];
  if FMemData.GetNull(FieldDesc, TMemDataHelper(FMemData).FilterRecBuf) then
    Exit;

  if FieldDesc.DataType in [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtInt64, dtUInt64] then
    Comp := CompareInteger(FieldDesc, TMemDataHelper(FMemData).FilterRecBuf, Value)
  else
    Exit;
  if Comp = -2 then
    Exit;

  case Operation of
    ntEqual: Result := Comp = 0;
    ntMore: Result := Comp = 1;
    ntLess: Result := Comp = -1;
    ntMoreEqual: Result := Comp >= 0;
    ntLessEqual: Result := Comp <= 0;
    ntNoEqual: Result := Comp <> 0;
  else
    Result := False;
  end;
end;

function TVirtualMemData.InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: double): boolean;
var
  FieldDesc: TFieldDesc;
  Comp: integer;
begin
  Result := False;

  FieldDesc := FMemData.Fields[FFields[FieldIndex].ActualIndex];
  if FMemData.GetNull(FieldDesc, TMemDataHelper(FMemData).FilterRecBuf) then
    Exit;

  if FieldDesc.DataType in [dtSingle, dtFloat, dtDate, dtTime, dtDateTime] then
    Comp := CompareFloat(FieldDesc, TMemDataHelper(FMemData).FilterRecBuf, Value)
  else
    Exit;
  if Comp = -2 then
    Exit;

  case Operation of
    ntEqual: Result := Comp = 0;
    ntMore: Result := Comp = 1;
    ntLess: Result := Comp = -1;
    ntMoreEqual: Result := Comp >= 0;
    ntLessEqual: Result := Comp <= 0;
    ntNoEqual: Result := Comp <> 0;
  else
    Result := False;
  end;
end;

function TVirtualMemData.IsSupportedDataType(DataType: word): boolean;
begin
  Result := TMemDataHelper(FMemData).IsSupportedDataType(DataType);
  if not Result then
    Result := inherited IsSupportedDataType(DataType);
end;

function TVirtualMemData.GetFieldDefinition(const Field: PVirtualFieldDesc): string;
var
  TypeDesc: TSpecificTypeDesc;
begin
  if Field^.FieldObject <> nil then begin
    TypeDesc := Field^.FieldObject as TSpecificTypeDesc;

    Result := TypeDesc.DataTypeName;
    if (TypeDesc.Length <> -1) or (TypeDesc.Scale <> -1) then begin
      Result := Result + '(';
      if TypeDesc.Length <> -1 then
        Result := Result + IntToStr(TypeDesc.Length);
      if TypeDesc.Scale <> -1 then begin
        if TypeDesc.Length <> -1 then
          Result := Result + ', ';
        Result := Result + IntToStr(TypeDesc.Scale);
      end;
      Result := Result + ')';
    end;
  end
  else
    Result := inherited GetFieldDefinition(Field);
end;

function TVirtualMemData.CompareVariant(const Field, Buffer: IntPtr; const Value: Variant): integer;
var
  FieldDesc: TFieldDesc;
  FieldValue: Variant;
begin
  FieldDesc := TFieldDesc(Field);
  FMemData.GetMappedFieldAsVariant(FieldDesc, Buffer, FieldValue);

  if FieldValue = Value then
    Result := 0
  else if FieldValue > Value then
    Result := 1
  else
    Result := -1;
end;

function TVirtualMemData.IsSimpleFieldType(const FieldIndex: Integer): boolean;
begin
  Result := FMemData.Fields[FieldIndex].DataType in [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtInt64, dtUInt64,
                                                     dtSingle, dtFloat, dtDate, dtTime, dtDateTime,
                                                     dtString, dtExtString, dtFixedChar,
                                                     dtWideString, dtExtWideString, dtFixedWideChar];
end;

function TVirtualMemData.GetLocalIndex(const Constraint: PVirtualConstraint): integer;
var
  i: integer;
  IndexName: string;
  Bookmark: TRecBookmark;
  Index: TVirtualMemDataIndex;
begin
  IndexName := '';
  for i := 0 to Length(Constraint^.Items) - 1 do
    if Constraint^.Items[i].FieldIndex <> -100 then
      IndexName := IndexName + FFields[Constraint^.Items[i].FieldIndex].Name + ';';
  Result := FLocalIndexes.IndexOf(IndexName);

  if Result = -1 then begin
    Index := GetLocalIndexClass.Create(Self, FMemData.RecordCount) as TVirtualMemDataIndex;
    Result := FLocalIndexes.AddObject(IndexName, Index);

    FMemData.GetBookmark(@Bookmark);
    try
      FMemData.SetToBegin;
      while True do begin
        FMemData.GetNextRecord(nil);

        if not FMemData.Eof then
          Index.FItems.Add(FMemData.GetCurrentItem)
        else
          Break;
      end;

      Index.Sort(FFields[Constraint^.Items[Constraint^.SortItemIndex].FieldIndex].ActualIndex);
    finally
      FMemData.SetToBookmark(@Bookmark);
    end;
  end;
end;

procedure TVirtualMemData.InternalAllocBuffer;
begin
  FMemData.AllocRecBuf(FRecordBuffer);
  FBufferAllocated := True;
end;

procedure TVirtualMemData.InternalFreeBuffer;
begin
  FMemData.FreeRecBuf(FRecordBuffer);
  FBufferAllocated := False;
end;

procedure TVirtualMemData.InternalPutRecord(const Values: TVirtualValues; Buffer: IntPtr);
var
  i: integer;
  Value: Variant;
  Blob: TBlob;
  Field: TFieldDesc;
begin
  for i := 0 to Length(Values) - 1 do begin
    Field := FMemData.Fields[FFields[i].ActualIndex];
    if not Field.ReadOnly then begin
      ValueToVariant(i, Values[i], Value);
      case Field.DataType of
        dtBlob: if VarType(Value) = varArray + varByte then begin
          if VarIsNull(Value) or VarIsEmpty(Value) then begin
            FMemData.SetNull(Field, Buffer, True);
            FMemData.SetChanged(Field, Buffer, True);
            Exit;
          end;

          Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(PtrOffset(Buffer, Field.DataOffset))));
          Blob.AsBytes := Value;

          FMemData.SetNull(Field, Buffer, False);
          FMemData.SetChanged(Field, Buffer, True);
        end
        else
          FMemData.PutFieldAsVariant(Field, Buffer, Value);
      else
        FMemData.PutFieldAsVariant(Field, Buffer, Value);
      end;
    end;
  end;
end;

function TVirtualMemData.Active: boolean;
begin
  Result := FMemData.Active;
end;

procedure TVirtualMemData.Prepare;
begin
  inherited;

  if FMemData.Active or FMemData.Prepared then
    Exit;
  if not FMemData.Active then
    FMemData.Prepare;
end;

function TVirtualMemData.Open(const Filter: PVirtualConstraint): TVirtualBookmark;
var
  i, Index: integer;
  FilterStr, ValueStr: string;
begin
  CheckActive;
  FMemData.GetBookmark(@FOpenBookmark);

  if not FBufferAllocated then
    InternalAllocBuffer;

  if FUseFilter then begin
    FilterStr := '';

    for i := 0 to Length(Filter^.Items) - 1 do begin
      if FilterStr <> '' then
        FilterStr := FilterStr + ' AND ';
      FilterStr := FilterStr + FFields[Filter^.Items[i].FieldIndex].Name + ' ';

      case Filter^.Items[i].Operation of
        ntEqual: FilterStr := FilterStr + '=';
        ntMore: FilterStr := FilterStr + '>';
        ntLess: FilterStr := FilterStr + '<';
        ntMoreEqual: FilterStr := FilterStr + '>=';
        ntLessEqual: FilterStr := FilterStr + '<=';
        ntNoEqual: FilterStr := FilterStr + '<>';
      end;
      FilterStr := FilterStr + ' ';

      ValueStr := VarToStr(Filter^.Items[i].Value.Value);

      if FFields[Filter^.Items[i].FieldIndex].DataType in [dtString, dtWideString, dtDate, dtTime, dtDateTime] then
        ValueStr := QuotedStr(ValueStr);

      FilterStr := FilterStr + ValueStr;
    end;

    if FilterStr <> FMemData.FilterText then begin
      FMemData.FilterText := FilterStr;
      FMemData.FilterUpdated;
    end;
  end;

  if (Filter <> nil) and (Filter^.LocalIndex >= 0) then begin
    Index := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetItem(Filter);

    if (Index >= 0) and (Length(Filter^.Items) > 1) then
      repeat
        FMemData.GetBookmark(@FBookmark);
        FBookmark.Item := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index];
        FMemData.SetToBookmark(@FBookmark);

        if OmitRecord(Filter, Filter^.SortItemIndex) then
          Index := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter)
        else
          Break;
      until Index = -1;

    if Index >= 0 then begin
      if Length(Filter^.Items) <= 1 then begin
        FMemData.GetBookmark(@FBookmark);
        FBookmark.Item := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index];
        FMemData.SetToBookmark(@FBookmark);
      end;

      InternalGetCurrentRecord;
      Result := TVirtualBookmark(NativeUInt((@FBookmark.Item)^));
    end
    else begin
      Result := -1;
      Exit;
    end;
  end
  else
    Result := InternalOpenNoIndex(Filter);
end;

procedure TVirtualMemData.Close;
begin
  ClearLocalIndexes;

  if FBufferAllocated then
    InternalFreeBuffer;
  FExplicitTransaction := False;

  if FAutoClose then
    FMemData.Close;

  inherited;
end;

procedure TVirtualMemData.Reset;
begin
  FMemData.SetToBookmark(@FOpenBookmark);
end;

function TVirtualMemData.GetBookmark: TVirtualBookmark;
begin
  FMemData.GetBookmark(@FBookmark);
  Result := TVirtualBookmark(NativeUInt((@FBookmark.Item)^));
end;

procedure TVirtualMemData.GotoBookmark(const Bookmark: TVirtualBookmark);
var
  p: NativeUInt;
begin
  p := NativeUInt(Bookmark);
  FBookmark.Item := PItemHeader((@p)^);
  FMemData.SetToBookmark(@FBookmark);
end;

function TVirtualMemData.GetRecordCount: integer;
begin
  Result := FMemData.RecordCount;
end;

function TVirtualMemData.GetFieldType(FieldIndex: integer): word;
begin
  if FFields[FieldIndex].FieldObject <> nil then
    Result := (FFields[FieldIndex].FieldObject as TSpecificTypeDesc).DataType
  else
    Result := inherited GetFieldType(FieldIndex);
end;

function TVirtualMemData.GetFieldNull(FieldIndex: integer): boolean;
begin
  Result := FMemData.GetNull(FMemData.Fields[FFields[FieldIndex].ActualIndex], FRecordBuffer);
end;

function TVirtualMemData.GetFieldValue(FieldIndex: integer): variant;
begin
  if IsSupportedDataType(FMemData.Fields[FFields[FieldIndex].ActualIndex].DataType) then
    FMemData.GetMappedFieldAsVariant(FMemData.Fields[FFields[FieldIndex].ActualIndex], FRecordBuffer, Result)
  else
    VarClear(Result);
end;

procedure TVirtualMemData.EditRecord(const Values: TVirtualValues);
begin
  FMemData.GetRecord(FRecordBuffer);
  InternalPutRecord(Values, FRecordBuffer);
  FMemData.UpdateRecord(FRecordBuffer);
end;

procedure TVirtualMemData.InsertRecord(const Values: TVirtualValues);
begin
  InternalPutRecord(Values, FRecordBuffer);
  FMemData.InsertRecord(FRecordBuffer);
end;

procedure TVirtualMemData.DeleteRecord;
begin
  FMemData.DeleteRecord;
end;

function TVirtualMemData.InTransaction: boolean;
var
  InternalTransaction: TCRTransaction;
begin
  if FMemData is TCRRecordSet then begin
    InternalTransaction := TCRRecordSet(FMemData).GetConnection.GetInternalTransaction;
    Result := InternalTransaction.GetInTransaction;
  end
  else
    Result := False;
end;

procedure TVirtualMemData.StartTransaction;
var
  InternalTransaction: TCRTransaction;
begin
  if (FMemData is TCRRecordSet) and not InTransaction then begin
    InternalTransaction := TCRRecordSet(FMemData).GetConnection.GetInternalTransaction;
    InternalTransaction.StartTransaction;
    FExplicitTransaction := True;
  end;
end;

procedure TVirtualMemData.Commit;
var
  InternalTransaction: TCRTransaction;
begin
  if (FMemData is TCRRecordSet) and InTransaction then begin
    InternalTransaction := TCRRecordSet(FMemData).GetConnection.GetInternalTransaction;
    if FExplicitTransaction then begin
      InternalTransaction.Commit;
      FExplicitTransaction := False;
    end
    else
      InternalTransaction.CommitRetaining;
  end;
end;

procedure TVirtualMemData.Rollback;
var
  InternalTransaction: TCRTransaction;
begin
  if (FMemData is TCRRecordSet) and InTransaction then begin
    InternalTransaction := TCRRecordSet(FMemData).GetConnection.GetInternalTransaction;
    if FExplicitTransaction then begin
      InternalTransaction.Rollback;
      FExplicitTransaction := False;
    end
    else
      InternalTransaction.RollbackRetaining;
  end;
end;

end.
