
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFFoxProUni;

interface

{$IFDEF DBFENGINE}

uses
  SysUtils, Classes, Variants, DateUtils, StrUtils, Types, TypInfo,
{$IFDEF LOG_PACKETS}
  LogHandler,
{$ENDIF}
  FMTBcd,
  CRTypes, CRFunctions, CRAccess, MemData,
  CRVirtualData, CLRClasses,
  LiteClassesVirtual,
{$IFNDEF UNIDACPRO}
  DBFConsts, DBFStructs, DBFUtils, DBFIndexes, DBFMemos, DBFDBase, DBFParser,
  {$IFDEF ODBC_PROVIDER}ODBCDataTypeMap,{$ENDIF}
  DBFDataTypeMap;
{$ELSE}
  DBFConstsUni, DBFStructsUni, DBFUtilsUni, DBFIndexesUni, DBFMemosUni, DBFDBaseUni, DBFParserUni,
  {$IFDEF ODBC_PROVIDER}ODBCDataTypeMapUni,{$ENDIF}
  DBFDataTypeMapUni;
{$ENDIF}

type
  TDBFFoxPro = class(TDBFDBase)
  protected
    procedure SetFieldConstraint(FieldPtr: PDBFField; ConstraintInfo: PConstraintInfo); override;
    procedure SetHasMemo(Value: boolean); override;
    procedure InternalOpenIndexFile; override;
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); override;

    class function GetMemoClass: TDBFMemoClass; override;
    class function GetIndexClass: TDBFIndexClass; override;
    class function GetMemoExt: string; override;
    class function GetIndexExt: string; override;

    function GetIndexKeyData(const Value: TVirtualValue; FieldNo, IdxNo: integer): TBytes; override;
  end;

  TDBFVisualFoxPro = class(TDBFFoxPro)
  private
    function GetFieldNullMaskIsSet(FieldNo: integer): boolean;
    procedure SetFieldNullMask(FieldNo: integer; IsNull: boolean);
    function GetFieldHasLenMaskIsSet(FieldNo: integer): boolean;
    procedure SetFieldHasLenMask(FieldNo: integer; HasLen: boolean);
  protected
    function IsAllowedFieldType(FType: AnsiChar): boolean; override;
    function IsNullableFieldType(FieldType: AnsiChar; Flags: byte): boolean; override;
    function IsAllowedIndexType(FType: AnsiChar): boolean; override;
    function IsNullMaskField(FieldNo: integer): boolean; override;
    procedure SetFieldDef(FieldPtr: PDBFField; ColumnInfo: PColumnInfo; var Offset: integer); override;
    procedure WriteMemoIndex(FieldNo, IdxOffset, Len: integer); override;
    function SetMemoValue(FieldNo: integer; IsInline, IsBinary, WriteLength: boolean; const Value: PVirtualValue): integer; override;
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); override;

    procedure Prepare; override;

    function GetFieldNullable(FieldNo: integer): boolean; override;
    function GetFieldTypeIsMemo(FieldType: AnsiChar): boolean; override;
    function GetFieldFlags(FieldNo: integer): Byte; override;
    function GetNullableFieldCount: integer; override;
    function GetNullFlagsLength(NullableFieldCount: integer): integer; override;
    function GetFieldNull(FieldNo: integer): boolean; override;
    function GetFieldValue(FieldNo: integer): variant; override;
    function GetFieldAsFloat(FieldNo: integer; ForceIgnoreErrors: boolean = False): double; override;

    procedure SetFieldValue(FieldNo: integer; var Value: TVirtualValue; GenerateAutoInc: boolean); override;

    procedure DescribeField(FieldNo: integer; var Idx: integer; var FieldDesc: TVirtualFieldDesc); override;
  end;

  TDBFCodebase = class(TDBFFoxPro)
  protected
    procedure WriteMemoIndex(FieldNo, IdxOffset, Len: integer); override;
  public
    constructor Create(const Database, TableName: string; HeaderFormat: TDBFFormat); override;

    function GetFieldNull(FieldNo: integer): boolean; override;
  end;

{$ENDIF}

implementation

{$IFDEF DBFENGINE}

uses
  Math;

{ TDBFFoxPro }

constructor TDBFFoxPro.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited;

  FDBFFormat := dfFoxPro2;
end;

class function TDBFFoxPro.GetMemoClass: TDBFMemoClass;
begin
  Result := TDBFFoxProMemo;
end;

class function TDBFFoxPro.GetIndexClass: TDBFIndexClass;
begin
  Result := TCDXFile;
end;

class function TDBFFoxPro.GetMemoExt: string;
begin
  Result := FPT_EXT;
end;

class function TDBFFoxPro.GetIndexExt: string;
begin
  Result := CDX_EXT;
end;

procedure TDBFFoxPro.SetFieldConstraint(FieldPtr: PDBFField; ConstraintInfo: PConstraintInfo);
begin
  // do nothing, flag DB3FlagMDX is empty in FoxPro
end;

procedure TDBFFoxPro.SetHasMemo(Value: boolean);
begin
  if Value then
    PDBFHeader(@FHeader).flags := PDBFHeader(@FHeader).flags or DBF_FLAG_HAS_MEMO
  else
    PDBFHeader(@FHeader).flags := PDBFHeader(@FHeader).flags and not DBF_FLAG_HAS_MEMO;
end;

procedure TDBFFoxPro.InternalOpenIndexFile;
var
  i, j: integer;
  IndexNamesList: TStringList;
begin
  if (FHeader.DB3.flags and DBF_FLAG_HAS_MDX) <> 0 then begin
    FIndexFileType := itCDX;
    FIndexFile := GetIndexClass.Create(Self, FindFileByName(ChangeFileExt(FFileName, GetIndexExt)));
    FIndexFile.Open;
    IndexNamesList := TStringList.Create;
    try
      FIndexFile.GetIndexFields(IndexNamesList);
      for i := 0 to IndexNamesList.Count - 1 do
        for j := 0 to FFieldCount - 1 do
          FKeyFields[j] := FKeyFields[j] or (UpperCase(IndexNamesList[i]) = UpperCase(GetFieldName(j)));
    finally
      IndexNamesList.Free;
    end;
  end;
end;

function TDBFFoxPro.GetIndexKeyData(const Value: TVirtualValue; FieldNo, IdxNo: integer): TBytes;
var
  nullable: boolean;

  procedure SetAsBlob(Blob: TBlob);
  var
    BlobData: TCRBlobData;
    bufPos, bufLen, count: integer;
  begin
    bufPos := 0;
    BlobData := Blob.GetData;
    count := Blob.Size;
    if count > FLengths[FieldNo] then
      count := FLengths[FieldNo];
    while bufPos < count do begin
      if nullable then
        bufLen := BlobData.Read(bufPos, count, @Result[1 + bufPos])
      else
        bufLen := BlobData.Read(bufPos, count, @Result[bufPos]);
      Inc(bufPos, bufLen);
    end;
  end;

var
  fType: AnsiChar;
  Expression: TDBFExpression;
  i, keyLen, len: integer;
  ExprValue: variant;
  str: string;
  aStr: AnsiString;
  b: Byte;
  dt: TDateTime;
  d: Double;
  c: Currency;
  Blob: TBlob;
  v_type: Word;
begin
  keyLen := FIndexFile.GetIndexKeyLength(IdxNo);
  Assert(keyLen > 0);
  nullable := FIndexFile.GetIndexNullable(IdxNo);
  SetLength(Result, keyLen);
  FillChar(Result[0], Length(Result), 0); // Result reusable, several filters can use same Result

  fType := FIndexFile.GetIndexKeyType(IdxNo);

  Expression := FIndexFile.GetIndexExpression(IdxNo);
  ExprValue := FCalculator.CalculateExpression(Self, Expression, Value, FieldNo, GetIndexFieldValue).Result;

  if fType = {$IFNDEF NEXTGEN}#0{$ELSE}0{$ENDIF} then begin
    fType := Expression.ResultType;
    FIndexFile.SetIndexKeyType(IdxNo, fType);
  end;

  case fType of
    DBF_TYPE_AUTOINC, DBF_TYPE_INTEGER:
      if not VarIsNull(ExprValue) then begin
        i := IntegerToVfpData(integer(ExprValue));
        if nullable then begin
          Result[0] := DBF_CHARNULL;
          PInteger(@Result[1])^ := i;
        end
        else
          PInteger(@Result[0])^ := i;
      end;
    DBF_TYPE_CHAR, DBF_TYPE_VARCHAR: begin
      if fType = DBF_TYPE_CHAR then
        FillChar(Result[0], Length(Result), $20);
      if not VarIsNull(ExprValue) then begin
        aStr := AnsiString(VarToStr(ExprValue));
        if nullable then
          Result[0] := DBF_CHARNULL;
        len := Min(LengthA(aStr), keyLen);
        if len > 0 then
          if nullable then
            Move(PAnsiChar(aStr)^, Result[1], len)
          else
            Move(PAnsiChar(aStr)^, Result[0], len);
      end;
    end;
    DBF_TYPE_LOGICAL: begin
      if VarIsNull(ExprValue) then
        b := ANSI_SPACE
      else if boolean(ExprValue) then
        b := Byte(CHAR_TRUE)
      else
        b := Byte(CHAR_FALSE);

      if nullable then begin
        Result[0] := DBF_CHARNULL;
        Result[1] := b;
      end
      else
        Result[0] := b;
    end;
    DBF_TYPE_CURRENCY:
      if not VarIsNull(ExprValue) then begin
        c := CurrencyToDBase7Data(currency(ExprValue));
        if nullable then begin
          Result[0] := DBF_CHARNULL;
          PCurrency(@Result[1])^ := c;
        end
        else
          PCurrency(@Result[0])^ := c;
      end;
    DBF_VFP_TYPE_DOUBLE, DBF_TYPE_FLOAT, DBF_TYPE_NUMERIC:
      if not VarIsNull(ExprValue) then begin
        d := DoubleToDBase7Data(double(ExprValue));
        if nullable then begin
          Result[0] := DBF_CHARNULL;
          PDouble(@Result[1])^ := d;
        end
        else
          PDouble(@Result[0])^ := d;
      end;
    DBF_TYPE_DATE: // string YYYY-MM-DD to string YYYYMMDD like 20021201
      if not VarIsNull(ExprValue) then begin
        if Value.ValueType = vrFloat then
          dt := double(ExprValue)
        else begin
          str := StringReplace(VarToStr(ExprValue), '-', '', [rfReplaceAll]);
          dt := EncodeDate(StrToInt(Copy(str, 1, 4)), StrToInt(Copy(str, 5, 2)), StrToInt(Copy(str, 7, 2)));
        end;
        d := DateTimeToJulianDate(dt) + 0.5; // 12:00:00 of day must have!
        d := DoubleToDBase7Data(d);
        if nullable then begin
          Result[0] := DBF_CHARNULL;
          PDouble(@Result[1])^ := d;
        end
        else
          PDouble(@Result[0])^ := d;
      end;
    DBF_TYPE_TIME:
      if not VarIsNull(ExprValue) then begin
        dt := InternalStrToDateTime(VarToStr(ExprValue));
        d := DoubleToDBase7Data(DateTimeToJulianDate(dt) + 0.5);
        if nullable then begin
          Result[0] := DBF_CHARNULL;
          PDouble(@Result[1])^ := d;
        end
        else
          PDouble(@Result[0])^ := d;
      end;
    DBF_TYPE_VARBINARY:
      if not VarIsNull(ExprValue) then begin
        if nullable then
          Result[0] := DBF_CHARNULL;

        case Value.ValueType of
          vrString: begin
            aStr := AnsiString(VarToStr(ExprValue));
            len := Min(LengthA(aStr), keyLen);
            if len > 0 then
              if nullable then
                Move(PAnsiChar(aStr)^, Result[1], len)
              else
                Move(PAnsiChar(aStr)^, Result[0], len);
          end;
          vrBlob: begin
            v_type := TVarData(ExprValue).VType;
            if (v_type and varByRef) <> 0 then begin
              Blob := TVarData(ExprValue).VPointer;
              SetAsBlob(Blob);
              // todo move
            end
            else if v_type = varArray + varByte then begin
              len := Min(TVarData(ExprValue).VArray.Bounds[0].ElementCount, keyLen);
              if len > 0 then
                if nullable then
                  Move(TVarData(ExprValue).VArray.Data^, Result[1], len)
                else
                  Move(TVarData(ExprValue).VArray.Data^, Result[0], len);
            end
            else
              raise Exception.CreateFmt('Unknown v_type %d', [v_type]);
          end;
        else
          raise Exception.CreateFmt('Unknown ValueType %d', [integer(Value.ValueType)]);
        end;
      end;
  else
    raise Exception.CreateFmt('Unknown fType %s', [string(AnsiString(fType))]);
  end;
end;

{ TDBFVisualFoxPro }

constructor TDBFVisualFoxPro.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited;

  FDBFFormat := dfVisualFoxPro;
end;

function TDBFVisualFoxPro.GetFieldTypeIsMemo(FieldType: AnsiChar): boolean;
begin
  Result := FieldType in [DBF_TYPE_MEMO, DBF_TYPE_GENERAL, DBF_TYPE_BLOB];
end;

function TDBFVisualFoxPro.IsAllowedFieldType(FType: AnsiChar): boolean;
begin
  Result := inherited IsAllowedFieldType(FType) or (FType in [DBF_TYPE_CURRENCY, DBF_TYPE_FLOAT, DBF_TYPE_TIME, DBF_VFP_TYPE_DOUBLE, DBF_TYPE_INTEGER,
    DBF_TYPE_GENERAL, DBF_TYPE_PICTURE, DBF_TYPE_BLOB, DBF_TYPE_NULLFLAGS, DBF_TYPE_VARBINARY, DBF_TYPE_VARCHAR]);
end;

function TDBFVisualFoxPro.IsNullableFieldType(FieldType: AnsiChar; Flags: byte): boolean;
begin
  Result := (flags and DBF_FIELDFLAG_NULLABLE) <> 0;
end;

function TDBFVisualFoxPro.IsAllowedIndexType(FType: AnsiChar): boolean;
begin
  Result := inherited IsAllowedIndexType(FType) or (FType in [DBF_TYPE_CURRENCY, DBF_TYPE_FLOAT, DBF_TYPE_TIME, DBF_VFP_TYPE_DOUBLE, DBF_TYPE_INTEGER,
    DBF_TYPE_VARBINARY, DBF_TYPE_VARCHAR]);
end;

function TDBFVisualFoxPro.GetFieldFlags(FieldNo: integer): Byte;
var
  fieldPtr: PDBFField;
begin
  fieldPtr := GetField(FieldNo);
  Result := fieldPtr.FPFlags;
end;

function TDBFVisualFoxPro.GetNullableFieldCount: integer;
var
  i: integer;
  fieldPtr: PDBFField;
begin
  Result := 0;
  for i := 0 to FFieldCount - 1 do begin
    fieldPtr := GetField(i);
    if fieldPtr.DB3FType in [DBF_TYPE_VARBINARY, DBF_TYPE_VARCHAR] then
      Inc(Result);
    if (fieldPtr.FPFlags and DBF_FIELDFLAG_NULLABLE) <> 0 then
      Inc(Result);
  end;
end;

function TDBFVisualFoxPro.GetNullFlagsLength(NullableFieldCount: integer): integer;
begin
  Result := 0;
  if NullableFieldCount > 0 then begin
    Result := NullableFieldCount shr 3;
    if (NullableFieldCount and 7) > 0 then
      Inc(Result);
  end;
end;

function TDBFVisualFoxPro.GetFieldNullable(FieldNo: integer): boolean;
var
  fieldPtr: PDBFField;
begin
  fieldPtr := GetField(FieldNo);
  Result := (fieldPtr.FPFlags and DBF_FIELDFLAG_NULLABLE) <> 0;
end;

function TDBFVisualFoxPro.GetFieldNullMaskIsSet(FieldNo: integer): boolean;
var
  v: variant;
  MaskIdx: Byte;
begin
  Result := False;
  // bits applied only to fields with nullable flag, adjust
  if FNullMaskFieldIndex > 0 then begin
    v := GetFieldValue(FNullMaskFieldIndex);
    MaskIdx := FFieldNoToFieldMaskIndex[FieldNo];
    if not (VarIsNull(v) or VarIsEmpty(v)) and (MaskIdx > 0) then
      Result := (PByteArray(TVarData(v).VArray.Data)[(MaskIdx - 1) div 8] and (1 shl ((MaskIdx - 1) mod 8))) <> 0;
  end;
end;

procedure TDBFVisualFoxPro.SetFieldNullMask(FieldNo: integer; IsNull: boolean);
var
  v: TVirtualValue;
  mask, MaskIdx: Byte;
begin
  if (FNullMaskFieldIndex > 0) and (FNullMaskFieldIndex <> FieldNo) then begin
    v.Value := GetFieldValue(FNullMaskFieldIndex);
    v.ValueType := vrBlob;
    MaskIdx := FFieldNoToFieldMaskIndex[FieldNo];
    if not (VarIsNull(v.Value) or VarIsEmpty(v.Value)) and (MaskIdx > 0) then begin
      mask := PByteArray(TVarData(v.Value).VArray.Data)[(MaskIdx - 1) div 8];
      if IsNull then
        mask := mask or (1 shl ((MaskIdx - 1) mod 8))
      else
        mask := mask and not(1 shl ((MaskIdx - 1) mod 8));
      PByteArray(TVarData(v.Value).VArray.Data)[(MaskIdx - 1) div 8] := mask;
      SetFieldValue(FNullMaskFieldIndex, v, False);
    end;
  end;
end;

function TDBFVisualFoxPro.GetFieldHasLenMaskIsSet(FieldNo: integer): boolean;
var
  v: variant;
  MaskIdx: Byte;
begin
  Result := False;
  // bits applied only to variable size fields
  if FNullMaskFieldIndex > 0 then begin
    v := GetFieldValue(FNullMaskFieldIndex);
    MaskIdx := FFieldNoToFieldHasLenIndex[FieldNo];
    if not (VarIsNull(v) or VarIsEmpty(v)) and (MaskIdx > 0) then
      Result := (PByteArray(TVarData(v).VArray.Data)[(MaskIdx - 1) div 8] and (1 shl ((MaskIdx - 1) mod 8))) <> 0;
  end;
end;

procedure TDBFVisualFoxPro.SetFieldHasLenMask(FieldNo: integer; HasLen: boolean);
var
  v: TVirtualValue;
  mask, MaskIdx: Byte;
begin
  if (FNullMaskFieldIndex > 0) and (FNullMaskFieldIndex <> FieldNo) then begin
    v.Value := GetFieldValue(FNullMaskFieldIndex);
    v.ValueType := vrBlob;
    MaskIdx := FFieldNoToFieldHasLenIndex[FieldNo];
    if not (VarIsNull(v.Value) or VarIsEmpty(v.Value)) and (MaskIdx > 0) then begin
      mask := PByteArray(TVarData(v.Value).VArray.Data)[(MaskIdx - 1) div 8];
      if HasLen then
        mask := mask or (1 shl ((MaskIdx - 1) mod 8))
      else
        mask := mask and not(1 shl ((MaskIdx - 1) mod 8));
      PByteArray(TVarData(v.Value).VArray.Data)[(MaskIdx - 1) div 8] := mask;
      SetFieldValue(FNullMaskFieldIndex, v, False);
    end;
  end;
end;

function TDBFVisualFoxPro.IsNullMaskField(FieldNo: integer): boolean;
begin
  Result := (FNullMaskFieldIndex > 0) and (FieldNo = FNullMaskFieldIndex);
end;

procedure TDBFVisualFoxPro.SetFieldDef(FieldPtr: PDBFField; ColumnInfo: PColumnInfo; var Offset: integer);
var
  fType: AnsiChar;
  ftIdx: integer;
  aStr: AnsiString;
begin
  // get DBF field type
  ftIdx := SqlTypeToDbfType.IndexOf(UpperCase(ColumnInfo.DataType));
  if ftIdx < 0 then
    raise Exception.CreateFmt('Unknown field type %s', [ColumnInfo.DataType]);

  FieldPtr.FPFlags := 0;
  fType := AnsiChar(SqlTypeToDbfType.Values[ftIdx]);
  case fType of
    DBF_TYPE_OLEBLOB:
      fType := DBF_VFP_TYPE_DOUBLE;
    DBF_TYPE_AUTOINC: begin
      fType := DBF_TYPE_INTEGER;
      FieldPtr.FPFlags := FieldPtr.FPFlags or DBF_FIELDFLAG_AUTOINC;
      ColumnInfo.NotNull := True;
      ColumnInfo.IsAutoincrement := True;
    end;
    DBF_TYPE_NULLFLAGS: begin
      // todo: check already exists?
      ColumnInfo.Name := VFP_NULLFLAGS_NAME;
      ColumnInfo.Length := GetNullFlagsLength(GetNullableFieldCount);
      ColumnInfo.NotNull := True;
    end;
  end;
  if not IsAllowedFieldType(fType) then
    raise Exception.CreateFmt('Field type %s not allowed for table type %s', [string(AnsiString(fType)), GetEnumName(TypeInfo(TDBFFormat), Ord(FDBFFormat))]);
  FieldPtr.DB3FType := fType;

  if fType <> DBF_TYPE_NULLFLAGS then
    ColumnInfo.Name := UpperCase(ColumnInfo.Name);
  aStr := AnsiString(ColumnInfo.Name);
  if Length(aStr) > Length(FieldPtr.DB3Name) then
    SetLengthA(aStr, Length(FieldPtr.DB3Name));
  if LengthA(aStr) > 0 then
    Move(PAnsiChar(aStr)^, FieldPtr.DB3Name[0], LengthA(aStr));

  if ColumnInfo.Length > 0 then
    FieldPtr.DB3Len := Byte(ColumnInfo.Length)
  else if DbfFieldTypeToFieldLen[Byte(fType)] > 0 then begin
    if GetFieldTypeIsMemo(fType) then
      FieldPtr.DB3Len := 4
    else if fType = DBF_VFP_TYPE_DOUBLE then
      FieldPtr.DB3Len := 8
    else
      FieldPtr.DB3Len := DbfFieldTypeToFieldLen[Byte(fType)];
  end
  else
    raise Exception.CreateFmt('Unknown field length for field %s type %s', [ColumnInfo.Name, ColumnInfo.DataType]);

  FieldPtr.DB3Offset := Offset;
  Inc(Offset, FieldPtr.DB3Len);

  if ColumnInfo.Scale < 0 then
    FieldPtr.DB3NumDecimal := 0
  else
    FieldPtr.DB3NumDecimal := ColumnInfo.Scale;

  if fType = DBF_TYPE_NULLFLAGS then
    FieldPtr.FPFlags := FieldPtr.FPFlags or DBF_FIELDFLAG_SYSTEM or DBF_FIELDFLAG_BINARY;
  if not ColumnInfo.NotNull then
    FieldPtr.FPFlags := FieldPtr.FPFlags or DBF_FIELDFLAG_NULLABLE;

  if ColumnInfo.IsAutoincrement then begin
    FieldPtr.FPAutoincNextValue := 1;
    FieldPtr.FPAutoincStepValue := 1;
  end
  else begin
    FieldPtr.FPAutoincNextValue := 0;
    FieldPtr.FPAutoincStepValue := 0;
  end;

  if fType in [DBF_TYPE_CURRENCY, DBF_TYPE_INTEGER, DBF_VFP_TYPE_DOUBLE, DBF_TYPE_TIME] then
    FieldPtr.FPFlags := FieldPtr.FPFlags or DBF_FIELDFLAG_BINARY;
end;

procedure TDBFVisualFoxPro.DescribeField(FieldNo: integer; var Idx: integer; var FieldDesc: TVirtualFieldDesc);
var
  flags: Byte;
  fType: AnsiChar;
begin
  inherited;

  flags := GetFieldFlags(FieldNo);
  fType := GetFieldType(FieldNo);

  //if FieldDesc.Hidden and (FieldDesc.Name = VFP_NULLFLAGS_NAME) then
  //  FNullMaskFieldIndex := FieldNo;

  if (fType = DBF_VFP_TYPE_DOUBLE) and (FieldDesc.Length = 0) and (FieldDesc.Scale > 0) then
    FieldDesc.Length := GetFieldLength(FieldNo) + FieldDesc.Scale;

  if (fType = DBF_TYPE_VARBINARY) or (fType = DBF_TYPE_VARCHAR) then begin
    // has length at the end bit
    FFieldNoToFieldHasLenIndex[FieldNo] := Idx;
    Inc(Idx);
  end;
  if (flags and DBF_FIELDFLAG_NULLABLE) <> 0 then begin
    // is nullable bit
    FFieldNoToFieldMaskIndex[FieldNo] := Idx;
    Inc(Idx);
  end;
end;

function TDBFVisualFoxPro.GetFieldNull(FieldNo: integer): boolean;
var
  fType: AnsiChar;
begin
  Result := GetFieldNullMaskIsSet(FieldNo);

  fType := GetFieldType(FieldNo);

  case fType of
    DBF_TYPE_VARBINARY, DBF_TYPE_VARCHAR:
      Result := Result and (FCurrentRecordBuffer[FOffsets[FieldNo] + FLengths[FieldNo] - 1] = 0);
    DBF_TYPE_NULLFLAGS:
      Result := False;
    DBF_TYPE_MEMO, DBF_TYPE_GENERAL, DBF_TYPE_BLOB: begin
      if FLengths[FieldNo] = 4 then
        Result := PInteger(@FCurrentRecordBuffer[FOffsets[FieldNo]])^ <= 0
      else if FLengths[FieldNo] = 10 then
        Result := inherited GetFieldNull(FieldNo);
    end;
  end;
end;

procedure TDBFVisualFoxPro.Prepare;
begin
  inherited;

  SetLength(FFieldNoToFieldMaskIndex, FFieldCount);
  FillChar(FFieldNoToFieldMaskIndex[0], Length(FFieldNoToFieldMaskIndex), 0);
  // variant size fields has extra bit
  SetLength(FFieldNoToFieldHasLenIndex, FFieldCount);
  FillChar(FFieldNoToFieldHasLenIndex[0], Length(FFieldNoToFieldHasLenIndex), 0);
end;

procedure TDBFVisualFoxPro.WriteMemoIndex(FieldNo, IdxOffset, Len: integer);
begin
  if FLengths[FieldNo] = 4 then
    PInteger(@FCurrentRecordBuffer[FOffsets[FieldNo]])^ := IdxOffset
  else
    inherited WriteMemoIndex(FieldNo, IdxOffset, Len);
end;

function TDBFVisualFoxPro.SetMemoValue(FieldNo: integer; IsInline, IsBinary, WriteLength: boolean; const Value: PVirtualValue): integer;
begin
  Result := inherited SetMemoValue(FieldNo, IsInline, IsBinary, WriteLength, Value);

  SetFieldHasLenMask(FieldNo, IsInline and (Result < FLengths[FieldNo]));
end;

function TDBFVisualFoxPro.GetFieldValue(FieldNo: integer): variant;
var
  fType: AnsiChar;
  MemoIdx, len: integer;
  d: Double;
  tsRec: TVFPTimeStamp;
  dt: TDateTime;
  hasFieldLen: boolean;
begin
  Result := Unassigned;

  fType := GetFieldType(FieldNo);

  case fType of
    DBF_TYPE_CURRENCY: // little-endian
      Result := PCurrency(@FCurrentRecordBuffer[FOffsets[FieldNo]])^;
    DBF_TYPE_TIME: begin
      Move(FCurrentRecordBuffer[FOffsets[FieldNo]], tsRec, SizeOf(TVFPTimeStamp));
      // Visual FoxPro always rounds to the nearest second
      d := tsRec.Date;
      if d <> 0 then begin
        dt := JulianDateToDateTime(d - 0.5);
        dt := IncSecond(dt, Round(tsRec.Time / 1000));
        Result := dt;
      end
      else
        Result := IncSecond(0, Round(tsRec.Time / 1000));
    end;
    DBF_TYPE_INTEGER:
      Result := PInteger(@FCurrentRecordBuffer[FOffsets[FieldNo]])^;
    DBF_VFP_TYPE_DOUBLE: begin
      Assert(FLengths[FieldNo] = 8);
      Result := PDouble(@FCurrentRecordBuffer[FOffsets[FieldNo]])^;
    end;
    DBF_TYPE_MEMO, DBF_TYPE_GENERAL, DBF_TYPE_BLOB: begin
      if FLengths[FieldNo] = 4 then begin
        MemoIdx := PInteger(@FCurrentRecordBuffer[FOffsets[FieldNo]])^;
        Result := GetMemoValue(FieldNo, MemoIdx, -1, False, fType = DBF_TYPE_BLOB);
      end
      else if FLengths[FieldNo] = 10 then
        Result := inherited GetFieldValue(FieldNo)
      else
        Result := GetMemoValue(FieldNo, -1, FLengths[FieldNo], True, True);
    end;
    DBF_TYPE_VARBINARY: begin
      hasFieldLen := GetFieldHasLenMaskIsSet(FieldNo);
      if hasFieldLen then
        // filled by spaces to the end, last byte is length if null bit is set
        len := FCurrentRecordBuffer[FOffsets[FieldNo] + FLengths[FieldNo] - 1]
      else
        len := FLengths[FieldNo];
      Result := GetMemoValue(FieldNo, -1, len, True, True);
    end;
    DBF_TYPE_VARCHAR: begin
      hasFieldLen := GetFieldHasLenMaskIsSet(FieldNo);
      if hasFieldLen then
        // filled by spaces to the end, last byte is length if null bit is set
        len := FCurrentRecordBuffer[FOffsets[FieldNo] + FLengths[FieldNo] - 1]
      else
        len := FLengths[FieldNo];
      Result := GetFieldAsString(FieldNo, len);
    end;
    DBF_TYPE_NULLFLAGS: begin
      Result := VarArrayCreate([0, FLengths[FieldNo] - 1], varByte);
      Move(FCurrentRecordBuffer[FOffsets[FieldNo]], TVarData(Result).VArray.Data^, FLengths[FieldNo]);
    end;
    DBF_TYPE_PICTURE:
      raise Exception.CreateFmt('Type %s not implemented yet', [fType]);
  else
    Result := inherited GetFieldValue(FieldNo);
  end;
end;

function TDBFVisualFoxPro.GetFieldAsFloat(FieldNo: integer; ForceIgnoreErrors: boolean = False): double;
begin
  case GetFieldType(FieldNo) of
    DBF_VFP_TYPE_DOUBLE: begin
      Assert(FLengths[FieldNo] = 8);
      Result := PDouble(@FCurrentRecordBuffer[FOffsets[FieldNo]])^;
    end;
  else
    Result := inherited GetFieldAsFloat(FieldNo, ForceIgnoreErrors);
  end;
end;

procedure TDBFVisualFoxPro.SetFieldValue(FieldNo: integer; var Value: TVirtualValue; GenerateAutoInc: boolean);

  procedure SetAsBlob(Blob: TBlob);
  var
    BlobData: TCRBlobData;
    bufPos, bufLen, count: integer;
  begin
    bufPos := 0;
    BlobData := Blob.GetData;
    count := Blob.Size;
    if count > FLengths[FieldNo] then
      count := FLengths[FieldNo];
    while bufPos < count do begin
      bufLen := BlobData.Read(bufPos, count, @FCurrentRecordBuffer[FOffsets[FieldNo] + bufPos]);
      Inc(bufPos, bufLen);
    end;
  end;

var
  fieldPtr: PDBFField;
  i: integer;
  fType: AnsiChar;
  d: Double;
  c: Currency;
  tsRec: TVFPTimeStamp;
  dt: TDateTime;
begin
  SetFieldNullMask(FieldNo, Value.ValueType = vrNull);

  fieldPtr := GetField(FieldNo);
  fType := GetFieldType(FieldNo);

{$IFDEF LOG_PACKETS}
  AddToLog(Format('VisualFoxPro.SetFieldValue: %d, %s', [FieldNo, string(Value.Value)]));
{$ENDIF}

  case fType of
    DBF_TYPE_CURRENCY: begin
      // little-endian
      if VirtualContainsValue(Value) then
        c := Value.Value
      else
        c := 0;
      Move(c, FCurrentRecordBuffer[FOffsets[FieldNo]], SizeOf(Currency));
    end;
    DBF_TYPE_TIME: begin
      if VirtualContainsValue(Value) then begin
        dt := InternalStrToDateTime(Value.Value);
        d := DateTimeToJulianDate(dt) + 0.5;
        tsRec.Date := Trunc(d);
        tsRec.Time := MillisecondOfTheDay(d);
      end
      else
        FillChar(tsRec, SizeOf(tsRec), 0);
      Move(tsRec, FCurrentRecordBuffer[FOffsets[FieldNo]], SizeOf(TVFPTimeStamp));
    end;
    DBF_TYPE_INTEGER: begin
      if (fieldPtr.FPFlags and DBF_FIELDFLAG_AUTOINC) <> 0 then begin
        if GenerateAutoInc then begin
          i := fieldPtr.FPAutoincNextValue;
          fieldPtr.FPAutoincNextValue := i + fieldPtr.FPAutoincStepValue;
          Move(i, FCurrentRecordBuffer[FOffsets[FieldNo]], SizeOf(Integer));
          Value.Value := i;
          Value.ValueType := vrInteger;
          WriteField(FieldNo);
        end;
      end
      else begin
        if VirtualContainsValue(Value) then
          i := Value.Value
        else
          i := 0;
        Move(i, FCurrentRecordBuffer[FOffsets[FieldNo]], SizeOf(Integer));
      end;
    end;
    DBF_VFP_TYPE_DOUBLE: begin
      Assert(FLengths[FieldNo] = 8);
      if VirtualContainsValue(Value) then
        d := Value.Value
      else
        d := 0;
      PDouble(@FCurrentRecordBuffer[FOffsets[FieldNo]])^ := d;
    end;
    DBF_TYPE_MEMO, DBF_TYPE_GENERAL, DBF_TYPE_BLOB: begin
      if not VirtualContainsValue(Value) and ((FLengths[FieldNo] = 4) or (FLengths[FieldNo] = 10)) then
        WriteMemoIndex(FieldNo, 0, 0)
      else if (FLengths[FieldNo] = 4) or (FLengths[FieldNo] = 10) then
        SetMemoValue(FieldNo, False, fType = DBF_TYPE_BLOB, False, @Value)
      else
        SetMemoValue(FieldNo, True, fType = DBF_TYPE_BLOB, False, @Value);
    end;
    DBF_TYPE_VARBINARY:
      SetMemoValue(FieldNo, True, True, True, @Value);
    DBF_TYPE_VARCHAR:
      SetMemoValue(FieldNo, True, False, True, @Value);
    DBF_TYPE_NULLFLAGS:
      if VirtualContainsValue(Value) then begin
        Assert(TVarData(Value.Value).VType = varArray + varByte);
        Assert(TVarData(Value.Value).VArray.Bounds[0].ElementCount = FLengths[FieldNo]);
        Move(TVarData(Value.Value).VArray.Data^, FCurrentRecordBuffer[FOffsets[FieldNo]], FLengths[FieldNo]);
      end;
    DBF_TYPE_PICTURE:
      raise Exception.CreateFmt('Type %s not implemented yet', [fType]);
  else
    inherited;
  end;
end;

{ TDBFCodebase }

constructor TDBFCodebase.Create(const Database, TableName: string; HeaderFormat: TDBFFormat);
begin
  inherited;

  FHasDeletedFlag := False;
end;

procedure TDBFCodebase.WriteMemoIndex(FieldNo, IdxOffset, Len: integer);
var
  aStr: AnsiString;
begin
  FillChar(FCurrentRecordBuffer[FOffsets[FieldNo]], FLengths[FieldNo], ANSI_SPACE);
  aStr := AnsiString(IntToStr(IdxOffset));
  Move(PAnsiChar(aStr)^, FCurrentRecordBuffer[FOffsets[FieldNo] + FLengths[FieldNo] - Length(aStr)], Length(aStr));
end;

function TDBFCodebase.GetFieldNull(FieldNo: integer): boolean;
var
  fType: AnsiChar;
  aStr: AnsiString;
begin
  fType := GetFieldType(FieldNo);

  case fType of
    DBF_TYPE_MEMO: begin
      SetLengthA(aStr, FLengths[FieldNo]);
      Move(FCurrentRecordBuffer[FOffsets[FieldNo]], PAnsiChar(aStr)^, FLengths[FieldNo]);
      Result := Trim(string(aStr)) = '0';
    end;
  else
    Result := inherited GetFieldNull(FieldNo);
  end;
end;

{$ENDIF}

end.
